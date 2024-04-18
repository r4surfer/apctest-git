         rem*************************************************************~
            *                                                           *~
            *  pppp    aaa   y   y  iiiii  n   n  pppp   eeeee  zzzzz   *~
            *  p   p  a   a  y   y    i    nn  n  p   p  e         z    *~
            *  pppp   aaaaa   yyy     i    n n n  pppp   eee      z     *~
            *  p      a   a    y      i    n  nn  p      e       z      *~
            *  p      a   a    y    iiiii  n   n  p      eeeee  zzzzz   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * payinpez - abbreviated a/p invoice entry, allowing only   *~
            *            non-stocked items.  useful for entering phone  *~
            *            bills and the like, since line item distibution*~
            *            is much faster than in the 'normal' invoice    *~
            *            input routine, payinput.                       *~
            *-----------------------------------------------------------*~
            *                  m o d i f i c a t i o n s                *~
            *---when---+------------------what--------------------+-who-*~
            * 01/29/87 ! original                                 ! hes *~
            * 05/18/87 ! paybuf2, paylines & hnymastr record      ! jim *~
            *          !  length modifications for standard cost  !     *~
            * 05/26/87 ! project/job matching #s logic.           ! jim *~
            * 03/23/89 ! corrected disabling of pf11 on line items! rjm *~
            *          !  screen & fixed bug that turned off pf13 !     *~
            * 09/14/93 ! prr 12483 - issues askuser when entering ! mlj *~
            *          !  'new invoice' with pmts already on file.!     *~
            *          !  also issues askuser when delleting lines!     *~
            *          !  of an invoice which has pmts on file.   !     *~
            * 02/22/94 ! prr 13116 - now writes hex(00)s to #10,  !     *~
            *          !  pos 179-182 instead of hex(20)'s.       !     *~
            * 04/01/94 ! condition for background posting         ! kab *~
            * 04/21/94 ! prr 10407,10877,11973. added soft enables! jdh *~
            * 08/15/96 ! Changes for the year 2000.               ! DXL *~
            * 07/07/97 ! Pass unformated date to PIPATCDZ         ! DXL *~
            *************************************************************

        dim                                                              ~
            acct$(100)16,                /* ACCOUNT NUMBERS            */~
            acctdescr$32,                /* ACCOUNT DESCRIPTION        */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cost(12), cost$96,           /* Inventory costs            */~
            cursor%(2),                  /* CURSOR LOACATIONS FOR EDIT */~
            date$8,                      /* SCREEN DATE STRING STUFF   */~
            date1$8,                     /* DATE FOR PAY DATE COMPUTING*/~
            date2$8,                     /* ANOTHER DATE...            */~
            datetime$7,                  /* DATE TIME STAMP            */~
            defaultacct$16,              /* DEFAULT ACCOUNT FOR INPUT  */~
            defstr$3,                    /* DEFAULT STORE NUMBR INPUT  */~
            disc_amt$10,                 /* DISCOUNT AMOUNT            */~
            disc_pct$10,                 /* DISCOUNT PERCENT           */~
            discdate$8,                  /* PAY TAKING DISCOUNT DATE   */~
            distcode$6,                  /* Auto Distribution Code     */~
            distamt$10,                  /* Auto Distribution Code     */~
            errormsg$79,                 /* ERROR MESSAGE TEXT LINE    */~
            ext$(100)10,                 /* EXTENSIONS AND STUFF       */~
            freetext$20,                 /* FREE TEXT INFORMATION      */~
            header$(2)79,                /* Screen Title               */~
            hold$1,                      /* Invoice On Hold Flag       */~
            i$(24)80,                    /* JUNK SCREEN IMAGE (NOT USED*/~
            inc(2),                      /* For The Elusive "PLOWCODE" */~
            inc$(2)28,                   /* For The Elusive "PLOWCODE" */~
            invdate$8,                   /* SCREEN INVOICE DATE        */~
            invnet$10,                   /* INVOICE TOTAL LESS DISCOUNT*/~
            invoicenr$16,                /* INVOICE NUMBER             */~
            invtype$,                    /* INVOICE TYPE (INTERNAL)    */~
            invtotal$10,                 /* INVOICE TOTAL              */~
            job$(100)8, xob$(100)8,      /* JOB NUMBERS FOR LINES      */~
            lastinvoice$16,              /* LAST INVOICE NUMBER INPUT  */~
            lastvendor$9,                /* LAST VENDOR PROCESSED      */~
            lfac$(20)1,                  /* LINEAR INPUT FAC'S         */~
            mfac$(13,4)1,                /* LINEAR INPUT FAC'S         */~
            manual$8,                    /* For call to 'MANUAL'       */~
            message$79,                  /* INPUT MESSAGE              */~
            nondisc$10,                  /* NON-DISCOUNTABLE AMOUNT    */~
            oldseq$(100)3,               /* OLD SEQUENCE NUMBER        */~
            origdate$6,                  /* DATE OF ORIGINAL INVOICE   */~
            origuserid$3,                /* USERID OF ORIGINAL INVOICE */~
            part$(100)25,                /* PART NUMBERS FOR LINES     */~
            payacct$16,                  /* PAY ACCOUNT NUMBER         */~
            payacctdescr$32,             /* PAYABLES ACCOUNT DESCRIPTN */~
            payaccttype$1,               /* PAYABLES ACCOUNT TYPE      */~
            paydate$8,                   /* PAYABLES DATE INFORMATION  */~
            pfdescr$(3)79,               /* FUNCTION KEYS ENABLED LISTS*/~
            pfkeys$32,                   /* FUNCTION KEYS ENABLED LISTS*/~
            po$16,                       /* PO Number For Auto Generate*/~
            poline$,                     /* PO Line For Line!          */~
            puracct$16,                  /* PURCHASES ACCOUNT NUMBER   */~
            puracctdescr$32,             /* PURCHASES ACCT DESCRIPTION */~
            rcv$,                        /* Reveiver Number For Line   */~
            readkey$90,                  /* KEY FOR PLOW ROUTINES      */~
            receiver$16,                 /* DEFAULT REVEIVER NUMBER    */~
            regulardate$8,               /* REGULAR DATE INFORMATION   */~
            seq$(100)4,                  /* SEQUENCE NUMBERS FOR SCREEN*/~
            seqnr$3,                     /* SEQUENCE NUMBER FLAG       */~
            seqnr2$4,                    /* SERIAL NUMBER FIELD        */~
            set%(255), xref%(3,8),       /* Soft Enables / Screen Refs */~
            sysacct$(5)9,                /* SYSTEM DEFAULT ACCOUNTS    */~
            tdate$8,                     /* Temporary Date             */~
            ten99$4,                     /* 1099 CATEGORY              */~
            ten99descr$32,               /* 1099 CATEGORY DESCRIPTION  */~
            text$4,                      /* Document Text Id. Number   */~
            textmsg$79,                  /* Text Rtn Message           */~
            text$(113,1)70,              /* Free Text Array            */~
            tfac$(20)1,                  /* SUMMARY SCREEN FAC'S       */~
            topline$79,                  /* Floating Program Title     */~
            userid$3,                    /* USERID THIS USER           */~
            venaddr$(6)30,               /* ADDRESS THIS VENDOR        */~
            vencode$9,                   /* VENDOR CODE THIS INVOICE   */~
            vendescr$32                  /* VENDOR DESCRIPTION         */

        dim f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            f2%(64),                     /* FILE STATUS FLAGS          */~
            fs%(64),                     /* FILE STATUS FLAGS          */~
            rslt$(64)20                  /* RETURNED FROM OPENFILE     */

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
            * # 2 ! GLMAIN   ! GENERAL LEDGER.  SALES ACCT VERIFICATION *~
            * # 3 ! VENDOR   ! LOAD VENDOR MASTER INFORMATION HERE      *~
            * # 4 ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * # 5 ! PAYMASTR ! PAYABLES MAIN HEADER FILE.               *~
            * # 6 ! PAYLINES ! PAYABLES LINE ITEMS FILE                 *~
            * # 7 ! SYSFILE2 ! SYSTEM INFO (DEFAULT PAY DATES)          *~
            * # 9 ! PAYBUFFR ! PAYABLES INVOICE HEADER BUFFER           *~
            * #10 ! PAYBUF2  ! PAYABLES INVOICE LINE ITEM BUFFER.       *~
            * #11 ! JOBMASTR ! WIP/JC MASTER FILE                       *~
            * #12 ! GLDTABLE ! G/L Account Distribution Tables          *~
            * #13 ! JBMASTR2 ! Production job master file               *~
            * #20 ! GENCODES ! General Purpose Code File                *~
            * #25 ! TXTFILE  ! System Text File                         *~
            * #27 ! CSHLINES ! Cash Disbursements File                  *~
            *************************************************************

            select # 1, "USERINFO",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos = 1, keylen = 3

            select # 2, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select #3,  "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos=1, keylen=9,                              ~
                        alt key 1, keypos = 10, keylen = 30, dup

            select #4,  "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 900,                                   ~
                        keypos = 1, keylen = 25

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
                        alternate key 1, keypos = 1, keylen = 63,        ~
                                  key 2, keypos = 17, keylen = 47

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
                        alternate key 1, keypos = 1, keylen = 63,        ~
                                  key 2, keypos = 17, keylen = 47

            select #11, "JOBMASTR",      /* JOB MASTER FILE            */~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 700,                                   ~
                        keypos = 1, keylen = 8

            select #12, "GLDTABLE",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos =   1, keylen = 15

            select #13, "JBMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1300,                                 ~
                        keypos =    1, keylen =   8                      ~

            select #20, "GENCODES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 128,                                   ~
                        keypos = 1, keylen = 24

            select #25, "TXTFILE",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos  = 1, keylen = 11

            select #27, "CSHLINES",                                      ~
                        varc, indexed, recsize = 100,                    ~
                        keypos = 1, keylen = 20,                         ~
                        alt key 1, keypos = 21, keylen = 16, dup

            call "SHOSTAT"  ("Opening Files, One Moment Please.")

            call "OPENCHCK" (#1, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#2, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#3, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#4, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#5, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#6, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#7, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#9, 0%, 0%, 100%, " ")
            call "OPENCHCK" (#10, 0%, 0%, 200%, " ")
            call "OPENCHCK" (#11, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#12, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#13, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#20, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                 I N I T I A L I Z A T I O N               *~
            *                                                           *~
            * SETS USER ID, DATES, CONTROL INFO  ETC...                 *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            manual$ = "PAYINPEZ"
            date$ = date
            call "DATEFMT" (date$)
            call "EXTRACT" addr("ID", userid$)
            lines_allowed% = dim(part$(),1)

            REM Get Users Posting Date...
            call "READ100" (#1, userid$, f1%(1))
                if f1%(1) = 1 then L09240
                call "ASKUSER" (keyhit%, "Sorry",                        ~
                  "You're Not Listed As A Valid User In This Data Base", ~
                                          " ", "Press (RETURN) To Exit.")
                goto L65000

L09240:     get #1, using L09250, paydate$, defstr$
L09250:         FMT XX(9), CH(6), XX(48), CH(3)

            REM Validate Users Posting Date...
                call "WHICHMON" (#7, paydate$, this%)
                  if this% <> 0 then L09350
                call "ASKUSER" (keyhit%, "Sorry",                        ~
                      "Your Posting Date Is Outside The Posting Window", ~
                                          " ", "Press (RETURN) To Exit.")
                goto L65000

L09350:         call "DATEFMT" (paydate$)

*        See it is ok to mess with buffers
            call "PAYBKCTL" (#9, ret%)
               if ret% <> 0% then end 0%

            REM Get A/P System Defaults...
                call "READ100" (#7, "MODULE.DEFAULTS.AP  ", f1%(7))
                     if f1%(7) = 0 then L09430
                get #7, using L09410,sysbillsdue%, sysdiscsdue%, sysacct$()
L09410:         FMT XX(20), 2*BI(4), XX(8), 5*CH(9)

L09430:     for i% = 1 to lines_allowed%
                convert i% to seq$(i%), pic(###)
                str(seq$(i%),4) = ")"
            next i%

            topline$ = "Manage Vendor Invoices              Post Date: XX~
        ~XXXXXX  Today's Date: XXXXXXXX"
            str(topline$,48,8) = paydate$
            str(topline$,72,8) = date$

            gosub init_enables

*        See if this User is a module administrator
            call "CMSMACHK" ("VBK", lfac$(1%), lfac$(2%))
                if lfac$(1%) = "Y" or lfac$(2%) = "Y" then admin% = 1%

        REM *************************************************************~
            *     I N P U T   I N V O I C E   H E A D E R   I N F O     *~
            *                                                           *~
            * GETS INVOICE HEADER INFORMATION AND THAT SORT OF THING.   *~
            *************************************************************

        inputmode
            gosub init_data

            for fieldnr% = 1 to 8
                gosub'161(fieldnr%, 1%)
                      if enabled% = 0 then L10210
L10120:         gosub'201(fieldnr%)
L10130:               if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then L10190
                         fieldnr% = max(1%, fieldnr%-1%)
                         gosub'161(fieldnr%, 1%)
                         if enabled% = 0 then L10130
                         goto L10120
L10190:               if keyhit%  = 16 and fieldnr% < 3 then L65000
                      if keyhit% <>  0 then       L10120
L10210:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
                next fieldnr%

            for fieldnr% = 1 to 6
                gosub'162(fieldnr%, 1%)
                      if enabled% = 0 then L10360
L10280:         gosub'202(fieldnr%)
L10290:               if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then L10350
                         fieldnr% = max(1%, fieldnr%-1%)
                         gosub'162(fieldnr%, 1%)
                         if enabled% = 0 then L10290
                         goto L10280
L10350:               if keyhit% <>  0 then       L10280
L10360:         gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10280
                next fieldnr%

        REM Enter Line Items...
            base%, screenline%, c% = 0%

L10430:     screenline% = screenline%+1
            if screenline% < 14 then L10470
                base% = base% + 1
                screenline% = 13
L10470:     c% = base% + screenline%
            maxlines% = maxlines% + 1
            if maxlines% > lines_allowed% then L10520
            gosub inputlines
            if keyhit% <> 16 then L10430
L10520:     maxlines% = maxlines% - 1
            gosub columnone
            goto editmode


        inputlines
            linemode% = 0
            for fieldnr% = 1 to 4
                gosub'163(fieldnr%, 1%) /* Set enables, input msg */
                     if enabled% = 0 then L10810
L10620:         gosub'103(screenline%,fieldnr%)
                      if keyhit%  = 16 then return
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  <> 2 then L10680
                             gosub columnone
                             goto  inputlines
L10680:               if keyhit% <>  6 then L10710
                         gosub prevline
                         goto L10780
L10710:               if keyhit% <>  4 then L10770
L10720:                  fieldnr% = max(1%, fieldnr% - 1%)
                         if fieldnr% = 1% then inputlines
                         gosub'163(fieldnr%, 1%)
                         if enabled% <> 0 then L10620
                         goto L10720
L10770:               if keyhit% <>  0 then       L10620
L10780:         gosub'153(fieldnr%)
                     if str(errormsg$,,1) = hex(84) then L10810
                     if errormsg$ <> " " then L10620
L10810:         next fieldnr%
            gosub total_up_invoice
        return

        prevline
            if c% = 1% then return
                on fieldnr% gosub L10930,           /* PART NUMBER      */~
                                  L10940,           /* EXTENSION        */~
                                  L10950,           /* ACCOUNT NUMBER   */~
                                  L10960            /* JOB NUMBER       */
            return

L10930:         part$(c%) = part$(c%-1) : return
L10940:         ext$ (c%) = ext$ (c%-1) : return
L10950:         acct$(c%) = acct$(c%-1) : return
L10960:         job$ (c%) = job$ (c%-1) : return

        REM *************************************************************~
            *           E D I T   I N V O I C E   H E A D E R           *~
            *                                                           *~
            * EDITS INVOICE HEADERS, PERMITTING ALL OF THE FIELDS TO BE *~
            * MODIFIED.                                                 *~
            *************************************************************

        editmode
            base% = 0 : editmode% = 1

        editpg1
            message$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Field And Press RETURN."
            gosub'211(0%)
                 errormsg$ = " "
                 if keyhit%  =  1% then gosub startover
                 if keyhit%  =  2% then       edit_lines
                 if keyhit%  =  5% then       editpg2
                 if keyhit%  = 16% then       datasave
                 if keyhit%  = 25% then gosub edit_text
                 if keyhit%  = 29% then L11210
                 if keyhit%  <> 0% then editpg1
L11210:     oldfieldnr% = 0%
L11220:     fieldnr% = cursor%(1%) - 12%
            if fieldnr% < 3% or fieldnr% > 7% then editpg1
            if fieldnr% > 6% then fieldnr% = fieldnr% + 1%
            if fieldnr% = 6% and cursor%(2%) > 35% then fieldnr% = 7%
            if fieldnr% = oldfieldnr% then editpg1
            oldfieldnr% = fieldnr%
            if keyhit% <> 29% then L11290
                gosub'049(1%, fieldnr%)
                goto editpg1

L11290:     gosub'161(fieldnr%, 2%)
                 if enabled% = 0% then editpg1
L11310:     gosub'211(fieldnr%)
                 if keyhit%  =  1% then gosub startover
                 if keyhit% <>  0% then       L11310
            gosub'151(fieldnr%)
                 if errormsg$ <> " " then L11310
            goto L11220

        editpg2
            message$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Field And Press RETURN."
            gosub'212(0%)
                 errormsg$ = " "
                 if keyhit%  =  1% then gosub startover
                 if keyhit%  =  2% then       edit_lines
                 if keyhit%  =  4% then       editpg1
                 if keyhit%  = 16% then       datasave
                 if keyhit%  = 25% then gosub edit_text
                 if keyhit%  = 29% then L11490
                 if keyhit%  <> 0% then editpg2
L11490:     oldfieldnr% = 0%
L11500:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 6% then editpg2
            if fieldnr% = oldfieldnr% then editpg2
            oldfieldnr% = fieldnr%
            if keyhit% <> 29% then L11550
                gosub'049(2%, fieldnr%)
                goto editpg2

L11550:     gosub'162(fieldnr%, 2%)
                 if enabled% = 0%  then editpg2
L11570:     gosub'212(fieldnr%)
                 if keyhit%  =  1% then gosub startover
                 if keyhit% <>  0% then       L11570
            gosub'152(fieldnr%)
                 if errormsg$ <> " " then L11570
            goto L11500

        REM *************************************************************~
            *           E D I T   I N V O I C E   L I N E S             *~
            *                                                           *~
            * EDITS INVOICE LINES ITEMS.                                *~
            *************************************************************

        edit_lines
            base% = 0% : editmode% = 1%
        resume_edit
            c%, screenline% = 0%
            lastfieldnr% = -1%
            base% = max(0%,min(base%,maxlines%-13%))
            linemode% = 1%
            message$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Field And Press RETURN."

L12160:     gosub'113(0%,0%)
                  errormsg$ = " "
                  if keyhit% =  1% then gosub startover
                  if keyhit% =  2% then base% = 0%
                  if keyhit% =  3% then base% = lines_allowed%
                  if keyhit% =  4% then base% = base% - 9%
                  if keyhit% =  5% then base% = base% + 9%
                  if keyhit% =  6% then base% = base% - 1%
                  if keyhit% =  7% then base% = base% + 1
                  base% = max(0%,min(base%,maxlines%-13))
                  if keyhit% =  9% then editpg1
                  if keyhit% = 11% then insertline
                  if keyhit% = 25% then gosub edit_text
                  if keyhit% = 28% then gosub delete_all
                  if keyhit% = 29% then L12330
                  if keyhit% = 16% then datasave
                  if keyhit% <> 0% and keyhit% <> 12% then L12160

L12330:     screenline% = cursor%(1%) - 6%
            if screenline% < 1% or screenline% > 13% then resume_edit
            c% = screenline%+base%
            if c% > maxlines% then resume_edit
            if keyhit% = 12% then deleteline
            fieldnr% = 0%
            if cursor%(2%) >  6% then fieldnr% = 1%
            if cursor%(2%) > 32% then fieldnr% = 2%
            if cursor%(2%) > 44% then fieldnr% = 3%
            if cursor%(2%) > 71% then fieldnr% = 4%
            if lastfieldnr%=fieldnr%+(screenline%*1000)then resume_edit
            ltemp%, ltemp1% = fieldnr%
            if keyhit% <> 29% then L12450
                gosub'049(3%, fieldnr%)
                goto edit_lines
L12450:     if fieldnr% <> 0 then L12480
                ltemp% = 1% : ltemp1% = 4%

L12480:     for fieldnr% = ltemp% to ltemp1%
            gosub'163(fieldnr%, 2%) /* Set input message, enables  */
                if enabled% = 0% then L12540
L12510:     gosub'113(screenline%,fieldnr%)
                if keyhit% =  1% then gosub startover
                if keyhit% <> 0% then L12510
L12540:     gosub'153(fieldnr%)
            if str(errormsg$,,1%) = hex(84) then L12570
            if errormsg$<>" " then L12510
L12570:     next fieldnr%
            lastfieldnr% = fieldnr% + (screenline% * 1000)
            gosub total_up_invoice
            goto L12330

        insertline
            if maxlines% > lines_allowed%-1 then resume_edit
            screenline% = min(13, max(cursor%(1)-5,1))
            if screenline%<1 or screenline%>13 then resume_edit
L12660:     c%=min(screenline%+base%,maxlines%+1)
            if screenline% > 12 then base% = max(0, c% - 12)
            if screenline% > 12 then screenline% = min(12,c%)
            if c% <= maxlines% then L12740
            screenline% = c% - base%
            if c% > lines_allowed%-1 then resume_edit
            goto L12820

L12740:     for i% = maxlines% to c% step -1%
                part$   (i%+1) = part$   (i%)
                ext$    (i%+1) = ext$    (i%)
                acct$   (i%+1) = acct$   (i%)
                job$    (i%+1) = job$    (i%)
                oldseq$ (i%+1) = oldseq$ (i%)
            next i%

L12820:     gosub columnone
            maxlines% = maxlines% + 1
            if maxlines% > lines_allowed% then L12930
            gosub inputlines
            if keyhit% = 16 then L12930
            screenline% = screenline% + 1
            if screenline% < 14 then L12660
                base% = base% + 1
                screenline% = 13
            goto L12660

L12930:     maxlines% = maxlines% - 1
L12940:     if c% > maxlines% then L13020
            for i% = c% to maxlines%
                part$   (i%) = part$   (i%+1)
                ext$    (i%) = ext$    (i%+1)
                acct$   (i%) = acct$   (i%+1)
                job$    (i%) = job$    (i%+1)
                oldseq$ (i%) = oldseq$ (i%+1)
            next i%
L13020:         c% = maxlines% + 1
                gosub columnone
                gosub total_up_invoice
                goto resume_edit

        deleteline
            gosub'103(screenline%,5%)
                if keyhit% <> 0 then resume_edit
*        Check if pmts on file for invoice/vendor before deleting...
            if fs%(27%) = 0% then                                        ~
                call "OPENCHCK" (#27, fs%(27%), f2%(27%), 0%, rslt$(27%))
            readkey$ = str(invoicenr$) & hex(00)
            call "REDALT0" (#27, readkey$, 1%, f1%(27%))
                goto L13067
L13066:     call "READNEXT" (#27, f1%(27%))
L13067:         if f1%(27%) = 0% then L13090          /* No Pmts On File */
            get #27 using L13070, temp1$, temp2$
L13070:         FMT CH(9), XX(11), CH(16)
            if temp2$ <> invoicenr$ then L13090   /* No Pmts On File     */
            if temp1$ <> vencode$   then L13066   /* Not for this Vendor */
L13073:     u3% = 0%
            call "ASKUSER" (u3%, "**** PAYMENTS ON FILE ****",           ~
                 "Invoice " & invoicenr$ & " currently has payments on "&~
                 "file.", "Press RETURN to continue Deletion process -O"&~
                 "R-"," Press PF(16) to return without Deleting.")
            if u3% = 0% then L13090
                if u3% <> 16% then L13073
                goto resume_edit
L13090:     maxlines% = maxlines% - 1%
*          GOSUB TOTAL_UP_INVOICE
            goto L12940

        delete_all
*        First check if any payments on file...
            if fs%(27%) = 0% then                                        ~
                call "OPENCHCK" (#27, fs%(27%), f2%(27%), 0%, rslt$(27%))
            readkey$ = str(invoicenr$) & hex(00)
            call "REDALT0" (#27, readkey$, 1%, f1%(27%))
                goto L13138
L13137:     call "READNEXT" (#27, f1%(27%))
L13138:         if f1%(27%) = 0% then L13161          /* No Pmts On File */
            get #27 using L13140, temp1$, temp2$
L13140:         FMT CH(9), XX(11), CH(16)
            if temp2$ <> invoicenr$ then L13161
            if temp1$ <> vencode$   then L13137

L13150:     u3% = 0%
            call "ASKUSER" (u3%, "**** PAYMENTS ON FILE ****",           ~
                 "Invoice " & invoicenr$ & " currently has payments on "&~
                 "file.", "Press RETURN to continue Deletion process -O"&~
                 "R-"," Press PF(16) to return without Deleting.")
            if u3% = 0% then L13161
                if u3% <> 16% then L13150
                    return
L13161:         hitkey% = 2%
                call "ASKUSER" (hitkey%, "Delete it ?", hex(8c) &        ~
                    "Press RETURN to clear all lines for re-entry or dele~
        ~tion" & hex(84), hex(8c) & "Note that NOTHING is permanently chan~
        ~ged until DATA SAVED" & hex(84),                                 ~
                                    "Press Any PF Key To Return To Edit")
                if hitkey% <> 0 then return
                part$(), ext$(), job$(), xob$(), acct$(), oldseq$() = " "
                maxlines%, base% = 0
                gosub total_up_invoice
        return

        columnone
            acct$(c%), part$(c%), ext$(c%), job$(c%), errormsg$,         ~
            oldseq$(c%), acctdescr$ = " "
            return

        REM *************************************************************~
            *      M I S C E L L A N E O U S   S U B R O U T I N E S    *~
            *                                                           *~
            *************************************************************

        total_up_invoice
            invamt, disc_amt = 0
            if maxlines% = 0% then L14330
            for i% = 1% to maxlines%
                convert ext$(i%) to temp, data goto L14310
                invamt = round(invamt + temp,2)
L14310:     next i%
            if invamt < 0 then L14370
L14330:     convert disc_amt$ to disc_amt, data goto L14340
L14340:     convert disc_pct$ to disc_pct, data goto L14350
L14350:     if disc_pct <> 0 then disc_amt =                             ~
                       round((invamt-min(invamt,nondisc))*disc_pct/100,2)
L14370:     call "CONVERT" (disc_amt, -2.2, disc_amt$)
            call "CONVERT" (invamt, -2.2, invtotal$)
            call "CONVERT" (invamt-disc_amt, -2.2, invnet$)
        return

        edit_text
            textmsg$ = "Vendor: " & vencode$ & ", Invoice: " & invoicenr$
            call "TXTINSUB" (#25, 0%, "011", textmsg$, text$, text$())
        return

        deffn'049(s%, f%)               /*  SCREEN AND FIELD NUMBER     */
            if admin% <> 1% then return           /*  NOT AUTHORIZED    */
            call "ENABLSUB" ("MODIFY", "PAYINPEZ", xref%(), set%(),      ~
                            s%, f%, 0%, 0%)
            return

        REM *************************************************************~
            *          W R I T E   D A T A   T O   B U F F E R          *~
            *                                                           *~
            * THIS ROUTINE DELETES THE OLD INVOICE FROM THE BUFFER IF   *~
            * THERE WAS ONE.  THEN IT GOES AND CALLS THE ROUTINE THAT   *~
            * WRITES THE NEW ONE OUT THERE.                             *~
            *************************************************************

        datasave
            REM First Try To Delete The Old Invoice From Buffer...
                gosub total_up_invoice /* Precaution */
                readkey$ = vencode$
                str(readkey$,10) = invoicenr$
                call "REDALT1" (#9, readkey$, 1%, f1%(9))
                      if f1%(9) <> 0 then delete #9
                call "DELETE"   (#10, readkey$, 25%)

            REM Set Last Document Input Information And Write Invoice.
                gosub L31000
                lastvendor$ = vencode$
                lastinvoice$ = invoicenr$
                goto inputmode

        REM *************************************************************~
            *  S E T   D E F A U L T S   F O R   H E A D E R   I N F O  *~
            *                                                           *~
            * SETS DEFAULTS FOR THE FIRST PAGE OF THE HEADER--ADDRESS,  *~
            * INVOICE DATE, DATE TO PAY WITH AND WITHOUT DISCOUNTS, AND *~
            * THAT SORT OF THING.                                       *~
            *************************************************************

            deffn'161(fieldnr%, mode%)
                  call "ENABLSUB" ("SET", "PAYINPEZ", xref%(), set%(),   ~
                                  1%, fieldnr%, mode%, enabled%)
                  if mode% = 2% then return

                  message$ = " "
                  on fieldnr% gosub L20260,         /* VENDOR CODE      */~
                                    L20300,         /* INVOICE NUMBER   */~
                                    L20340,         /* DATE OF INVOICE  */~
                                    L20400,         /* REGULAR DATE     */~
                                    L20460,         /* DISCOUNT DATE    */~
                                    L20530,         /* DISC_PCT         */~
                                    L20600,         /* DISC_AMT         */~
                                    L20650          /* NON-DISCOUNTABLE */
                return
L20260:     REM ENABLE STUFF FOR VENDOR CODE
                message$ = "Enter Vendor Number.  Leave Blank And Press (~
        ~RETURN) To Find An Existing Vendor."
                return
L20300:     REM INPUT ENABLE FOR INVOICE NUMBER
                message$ = "Enter Vendor's Invoice Number."
                return
L20340:     REM INPUT ENABLE FOR INVOICE DATE
                message$ = "Enter Date Of Invoice."
                if invdate$ = " " or invdate$ = blankdate$ ~
                                then invdate$ = paydate$
                return
L20400:     REM INPUT ENABLE FOR REGULAR DISBURSEMENT DATE
                message$ = "Enter Date This Invoice Should Be Paid By."
                if regulardate$ = " " or regulardate$ = blankdate$ ~
                                                 then gosub L56310
                return
L20460:     REM INPUT ENABLE FOR DISCOUNT DISBURSEMENT DATE
                if discdate$ = " " or discdate$ = blankdate$ then gosub L56650
                message$ = "Enter Date This Invoice Would Have To Be Paid~
        ~ By To Receive a Discount."
                return
L20530:     REM ENABLE DISCOUNT PERCENT
                message$ = "Enter Discount Percentage, If Any (Eg. Enter ~
        ~'2' If a 2% Discount Is Possible)."
                if disc_pct = 0 then return
                if disc_pct$ <> " " then return
                call "CONVERT" (disc_pct, -2.4, disc_pct$)
                return
L20600:     REM ENABLE DISCOUNT AMOUNT
                message$ = "Enter The Total Discount For This Invoice."
                return
L20650:     REM INPUT ENABLE FOR NON-DISCOUNTABLE AMOUNT
                message$ = "Enter Portion Of Total Invoice That Is Non Di~
        ~scountable."
                return

        REM *************************************************************~
            *  S E T   D E F A U L T S   F O R   H E A D E R   I N F O  *~
            *                                                           *~
            * SETS DEFAULTS FOR THE SECOND PAGE OF THE HEADER.          *~
            *************************************************************

            deffn'162(fieldnr%, mode%)
                  call "ENABLSUB" ("SET", "PAYINPEZ", xref%(), set%(),   ~
                                  2%, fieldnr%, mode%, enabled%)
                  if mode% = 2% then return

                  message$ = " "
                  on fieldnr% gosub L21230,         /* FREE TEXT FIELD  */~
                                    L21270,         /* 1099 CATEGORY    */~
                                    L21370,         /* PAYABLES ACCOUNT */~
                                    L21530,         /* HOLD STATUS      */~
                                    L21600,         /* PURCHASES ACCT   */~
                                    L21740          /* Dist Code & Amt  */
                return

L21230:     REM INPUT ENABLE FOR FREE TEXT FIELD.
                message$ = "You Can Put Anything You Would Like Here."
                return

L21270:     REM INPUT ENABLE FOR 1099 CATEGORY
                message$ = "Enter The 1099 Category Of This Invoice."
                if ten99$ <> " " then return
                get #3, using L21310, ten99$
L21310:             FMT XX(514), CH(4)
*              IF TEN99$ = " " THEN ENABLED% = 0
                readkey$ = "1099 CATS" & ten99$
                call "DESCRIBE" (#20, readkey$, ten99descr$, 0%, f1%(20))
                return

L21370:     REM INPUT ENABLE FOR PAYABLES ACCOUNT NUMBER
                message$ = "Enter The G/L Accounts Payable Account Number~
        ~.  Blank To Search For Account."
                if payacct$ <> " " then return
                get #3, using L21420, payacct$
L21420:                 FMT XX(258), CH(9)
                if payacct$ = " " then payacct$ = sysacct$(3)
                call "DESCRIBE" (#2, payacct$, payacctdescr$, 1%, f1%(2))
                   if f1%(2) <> 0 then L21480
                      payacct$, payacctdescr$ = " "
                      return
L21480:         get #2, using L21490, payaccttype$
L21490:             FMT XX(39), CH(1)
                call "GLFMT" (payacct$)
                return

L21530:     REM INPUT ENABLE FOR HOLD STATUS
                message$ = "Enter 'Y' to Post This Invoice To Payables, B~
        ~ut Disallow Payment."
                return

L21600:     REM INPUT ENABLE FOR DEFAULT PURCHASES ACCOUNT NUMBER
                message$ = "Enter The Default Purchases Account Number Fo~
        ~r Line On This Invoice."
                if puracct$ <> " " then return
                get #3, using L21650, puracct$
L21650:                 FMT XX(249), CH(9)
                if puracct$ = " " then puracct$ = sysacct$(1)
                call "DESCRIBE" (#2, puracct$, puracctdescr$, 1%, f1%(2))
                   if f1%(2) <> 0% then L21710
                      puracct$, puracctdescr$ = " "
                      return
L21710:         call "GLFMT" (puracct$)
                return

L21740:     REM INPUT ENABLE FOR DEFAULT AUTO DISTRIBUTE CODE AND AMOUNT
                message$ = "For Automatic Line Creation, Enter Distributi~
        ~on Code And Amount To Distribute."
                return

        REM *************************************************************~
            *  S E T   D E F A U L T S   F O R   T A B L E   I N P U T  *~
            *                                                           *~
            * SETS DEFAULTS FOR THE VARIOUS LINE ITEMS. FOR PARTS ON    *~
            * FILE, SETS THE EXTENSION = TO THE MATERIAL COST * THE     *~
            * QUANTITY, LEAVES IT BLANK IF ZERO.  FOR DEBIT ACCOUNT,    *~
            * SUPPLY THE DEFAULT PURCHASES ACCOUNT                      *~
            *************************************************************

            deffn'163(fieldnr%, mode%)
                  call "ENABLSUB" ("SET", "PAYINPEZ", xref%(), set%(),   ~
                                  3%, fieldnr%, mode%, enabled%)
                  if mode% = 2% then return

                  message$ = " "
                  on fieldnr% gosub L22180,         /* INVENTORY PART # */~
                                    L22220,         /* EXTENSION        */~
                                    L22280,         /* DEBIT ACCOUNT    */~
                                    L22330          /* JOB NUMBER       */
                     return

L22180:     REM SET DEFAULT INVENTORY PART DEFAULT VALUE
                message$ = "Enter Text To Describe Nature Of Charge."
                return

L22220:     REM SET DEFAULT EXTENSION VALUE
                call "SPCSMASH" (ext$(c%))
                message$ = "Enter The Non Discounted Total For This Line ~
        ~Of The Invoice."
                return

L22280:     REM SET DEFAULT VALUE FOR DEBIT ACCOUNT
                if acct$(c%) = " " then acct$(c%) = defaultacct$
                message$ = "Enter The G/L Purchases Account Number."
                return

L22330:     REM SET DEFAULT JOB NUMBER VALUE.
                if job$(c%) <> " " then return
                message$ = "Enter Job Or Project Number.  Enter Partial V~
        ~alue To Search Master Files."
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

        init_data
            vencode$, invoicenr$, vendescr$, venaddr$(), errormsg$ = " "

        init_data_special
            init (" ")                                                   ~
            invdate$, puracct$, origdate$, po$, ext$(), origuserid$,     ~
            puracctdescr$, payacct$, payacctdescr$, discdate$, invtype$, ~
            regulardate$, nondisc$, acct$(), part$(), disc_amt$, poline$,~
            job$(), receiver$, oldseq$(), freetext$, hold$, ten99descr$, ~
            ten99$, disc_pct$, rcv$, text$, distcode$, distamt$, xob$()

            maxlines%, editmode%, disc_pct, disc_amt, invamt = 0
            call "TXTFUTIL" (#25, 0%, "INTL", text$)
        return

        init_enables
*        Define Screen, Field Cross Ref and Field Enable Settings.
            mat set% = con   : mat set% = (99%) * set%
            mat xref% = zer
            xref%(1%, 1%) = 1%  : set%(1%)  = 13%
            xref%(1%, 2%) = 2%  : set%(2%)  = 13%
            xref%(1%, 3%) = 3%  : set%(3%)  =  2%
            xref%(1%, 4%) = 4%  : set%(4%)  =  2%
            xref%(1%, 5%) = 5%  : set%(5%)  =  2%
            xref%(1%, 6%) = 6%  : set%(6%)  =  2%
            xref%(1%, 7%) = 7%  : set%(7%)  =  2%
            xref%(1%, 8%) = 8%  : set%(8%)  =  2%

            xref%(2%, 1%) = 9%  : set%(9%)  =  2%
            xref%(2%, 2%) = 10% : set%(10%) =  2%
            xref%(2%, 3%) = 11% : set%(11%) =  2%
            xref%(2%, 4%) = 12% : set%(12%) =  2%
            xref%(2%, 5%) = 13% : set%(13%) =  2%
            xref%(2%, 6%) = 14% : set%(14%) =  2%

            xref%(3%, 1%) = 15% : set%(15%) = 13%
            xref%(3%, 2%) = 16% : set%(16%) =  2%
            xref%(3%, 3%) = 17% : set%(17%) =  2%
            xref%(3%, 4%) = 18% : set%(18%) =  2%
*           Next available ^^% number is 19.

            call "ENABLSUB" ("INIT", "PAYINPEZ", xref%(), set%(),        ~
                             0%, 0%, 0%, 0%)
            return

L30000: REM *************************************************************~
            *     L O A D   O L D   I N V O I C E   O F F   F I L E     *~
            *                                                           *~
            *    PLOWS THROUGH BUFFER AND THEN THROUGH PAYABLES MASTER  *~
            * FILE LOOKING FOR THE GIVEN INVOICE.  IF ON FILE, LOAD AND *~
            * EDIT, OTHERWISE, JUST MAKE NEW INVOICE.                   *~
            *************************************************************

            REM Prepare To Search Data Files...
                oldinvoiceonfile%, maxlines% = 0
                readkey$ = str(vencode$) & str(invoicenr$)

*        First, try to read HEADER record from BUFFER...
             file% = 10%
             call "REDALT0" (#9, readkey$, 1%, f1%(9%))
                 if f1%(9%) = 0% then L30102
             get #9, using L30085, str(lot$(),,200%), str(lot$(),201%)
L30085:          FMT POS(11), CH(200), CH(140)
             call "SHOSTAT" ("Loading Invoice from Buffer File")
             goto L30140

L30102
*        Not in BUFFER, so try MASTER file...
             file% = 6%
             call "READ100" (#5, readkey$, f1%(5%))
                 if f1%(5%) = 0% then L30116           /* Check CSHLINES */
             get #5, str(lot$(),,340%)
             call "SHOSTAT" ("Loading Invoice From Master File")
             goto L30140

L30116
*        Not In Master, Try CSHLINES...
             if fs%(27%) = 0% then                                       ~
                 call "OPENCHCK" (#27, fs%(27%), f2%(27%), 0%, rslt$(27%))
             readkey$ = str(invoicenr$) & hex(00)
             call "REDALT0" (#27, readkey$, 1%, f1%(27%))
                 goto L30123
L30122:      call "READNEXT" (#27, f1%(27%))
L30123:          if f1%(27%) = 0% then L30425                  /* Return */
             get #27 using L30125, temp1$, temp2$
L30125:          FMT CH(9), XX(11), CH(16)
             if temp2$ <> invoicenr$ then L30425
             if temp1$ <> vencode$   then L30122
L30128:      u3% = 0%
             call "ASKUSER" (u3%, "**** PAYMENTS ON FILE ****",          ~
                 "Invoice " & invoicenr$ & " currently has payments on "&~
                 "file.", "Press RETURN to continue with this number -O"&~
                 "R-", "PF(16) to re-enter the invoice number.")
             if u3% = 0% then L30425
                 if u3% <> 16% then L30128
                     errormsg$ = "Invoice Has Payment(s) On File"
                      return
L30140:     REM Actually get And Format Data Off Header...
                get str(lot$(),,340), using L30440, receiver$, invdate$,  ~
                    puracct$, payacct$, payaccttype$, regulardate$,      ~
                    discdate$, nondisc, origdate$, origuserid$,          ~
                    freetext$, disc_pct, disc_amt, ten99$, hold$,        ~
                    invtype$, text$
                call "TXTFUTIL" (#25, 0%, "LOAD", text$)
                oldinvoiceonfile% = 1
                lot$() = " "

                call "DESCRIBE" (#2, puracct$, puracctdescr$, 1%, f1%(2))
                     call "GLFMT" (puracct$)
                     defaultacct$ = puracct$
                call "DESCRIBE" (#2, payacct$, payacctdescr$, 1%, f1%(2))
                     call "GLFMT" (payacct$)
                call "DATEFMT"  (invdate$)
                call "DATEFMT"  (regulardate$)
                if discdate$ = blankdate$ then discdate$ = "00/00/00" else ~
                      call "DATEFMT"  (discdate$)
                call "CONVERT" (nondisc, -2.4, nondisc$)
               if disc_pct<>0 then call"CONVERT"(disc_pct,-2.4,disc_pct$)
                call "CONVERT"(disc_amt, -2.2, disc_amt$)

                if ten99$ = " " then L30270
                     readkey$ = "1099 CATS" & ten99$
                     call "DESCRIBE"(#20,readkey$,ten99descr$,0%,f1%(20))

L30270:     REM Load Up And Format Line Item Details...
                readkey$ = vencode$
                str(readkey$, 10) = invoicenr$
L30285:         call "PLOWNEXT" (#file%, readkey$, 25%, f1%(file%))
                     if f1%(file%) = 0 then L30380  /* Return */

                c%, maxlines% = maxlines% + 1
                get #file%, using L30550, rcv$, po$, poline$, seqnr$,     ~
                         acct$(c%), part$(c%), ext, job$(c%), oldseq$(c%)

                if rcv$ = " " and po$ = " " and poline$ = " " then L30355
                       errormsg$ = "Invoice Linked To a Receiver"
                       goto L30380
                call "DESCRIBE" (#4, part$(c%), " ", 0%, f1%(4))
                     if f1%(4) = 0 then L30355
                       errormsg$ = "Invoice Contains Stocked Parts"
                       goto L30380
L30355:         if file% = 6 then oldseq$(c%) = seqnr$
                call "CONVERT" (ext, 2.2, ext$(c%))
                call "GLFMT" (acct$(c%))
                goto L30285

L30380:     REM Check if edit allowed, total invoice, & return...
                REM Note that ERRORMSG may already be set from above code
                if invtype$="A" then errormsg$="Adjustment Invoice"
                if invtype$="R" then errormsg$="Recurring Master Invoice"
                if invtype$="O" then errormsg$="Offline Invoice"
                if errormsg$ <> " " then gosub init_data_special
                if errormsg$ <> " " then errormsg$ = errormsg$ &         ~
                                                ", Can't Edit From Here."
L30425:         gosub total_up_invoice
                return

L30440:     FMT XX(9),                   /* VENDOR CODE                */~
                XX(16),                  /* INVOICE NUMBER             */~
                CH(16),                  /* RECEIVER NUMBER            */~
                CH(6),                   /* INVOICE DATE               */~
                CH(9),                   /* PURCHASES ACCOUNT          */~
                CH(9),                   /* PAYABLES ACCOUNT           */~
                CH(1),                   /* PAYABLES ACCOUNT TYPE (A,E)*/~
                CH(6),                   /* PAY W/O DISCOUNT DATE      */~
                CH(6),                   /* PAY W/DISCOUNT DATE        */~
                PD(14,4),                /* NON-DISCOUNTABLE AMOUNT    */~
                XX(6),                   /* LAST POSTING DATE          */~
                CH(6),                   /* ORIGINAL INPUT DATE        */~
                CH(3),                   /* ORIGINAL INPUT BY          */~
                XX(25),                  /* AUDIT INFO AND TOTALS      */~
                CH(20),                  /* FREE TEXT FIELD            */~
                PD(14,4),                /* DISCOUNT PERCENT           */~
                PD(14,4),                /* DISCOUNT AMOUNT            */~
                CH(4),                   /* 1099 CATEGORY              */~
                CH(1),                   /* HOLD STATUS                */~
                CH(1),                   /* INVOICE TYPE (INTERNAL)    */~
                CH(4)                    /* TEXT ID NUMBER             */

L30550:     FMT CH(16),                  /* RECEIVER NUMBER            */~
                CH(16),                  /* PO NUMBER                  */~
                CH(03),                  /* PO LINE NUMBER             */~
                XX(9),                   /* VENDOR CODE                */~
                XX(16),                  /* INVOICE NUMBER             */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                CH(9),                   /* EXPENSE ACCOUNT NUMBER     */~
                CH(25),                  /* PART NUMBER                */~
                XX(8),                   /* QUANTITY                   */~
                PD(14,4),                /* EXTENSION                  */~
                CH(8),                   /* JOB NUMBER                 */~
                XX(3),                   /* STORE NUMBER               */~
                XX(6),                   /* LOT NUMBER                 */~
                XX(8),                   /* UNIT PRICE                 */~
                CH(3)                    /* OLD SEQUENCE NUMBER        */

L31000: REM *************************************************************~
            *       W R I T E   I N V O I C E   T O   B U F F E R       *~
            *                                                           *~
            * TOSSES THE CURRENT INVOICE TO THE BUFFER.  THE DATASAVE   *~
            * ROUTINE DELETED THE OLD INVOICE, SO NOW WE WRITE THE NEW  *~
            * ONE IN ITS PLACE.                                         *~
            *************************************************************

            REM WRITE LINE ITEM INFORMATION TO FILE.
                call "SHOSTAT" ("Writing Invoice To Buffer Files")

                invamt = 0
                seqnr2$ = all(hex(00))
                if maxlines% = 0 then L31360
                for c% = 1 to maxlines%
                    convert c% to seqnr$, pic(###)
                    ext = 0
                    convert ext$(c%) to ext, data goto L31240
L31240:             invamt = round(invamt + ext,2)
                    call "GLUNFMT" (acct$(c%))
                     mat cost = zer : cost(1) = ext
                     call "PACKZERO" (cost(), cost$)

                    write #10%, using L32180, " ", " ", " ", vencode$,    ~
                               invoicenr$, seqnr$, acct$(c%), part$(c%), ~
                               1, ext, job$(c%), " ", " ", ext,          ~
                               oldseq$(c%), 1, " ", " ", seqnr2$, ext,   ~
                               cost$, " ", " "
                next c%
                goto L31530

L31360:         REM If Deleted Invoice Isn't In Master, Save Nothing...
                call "TXTFUTIL" (#25, 0%, "XOUT", text$)
                readkey$ = str(vencode$) & str(invoicenr$)
                call "READ100" (#5, readkey$, f1%(5))
                     if f1%(5) = 0 then L31820
                REM Is in master, so allow cancelling of changes...
L31440:         ask% = 0%
                call "ASKUSER" (ask%, "*** DELETE OPTION ***",           ~
                "Press RETURN to cancel invoice changes this session",   ~
                "---- OR ----",                                          ~
                "Press PF-16 to mark the vendor's invoice for DELETION")
                if ask% = 0% then L31820
                if ask% = 16% then L31530
                goto L31440

L31530:     REM WRITE HEADER INFORMATION TO FILE
                call "DATUNFMT" (invdate$)
                call "DATUNFMT" (regulardate$)
                call "DATUNFMT" (discdate$)
                call "GLUNFMT" (puracct$)
                call "GLUNFMT" (payacct$)
                if origdate$ = " " or origdate$ = blankdate$ ~
                                 then origdate$ = date
                if origuserid$ = " " then origuserid$ = userid$
                convert nondisc$ to nondisc
                if invtype$ = " " then invtype$ = "N"

                put str(lot$(),,340), using L31920, vencode$, invoicenr$, ~
                            receiver$, invdate$, puracct$, payacct$,     ~
                            payaccttype$, regulardate$, discdate$,       ~
                            nondisc, " ", origdate$, origuserid$, date,  ~
                            userid$, invamt, 0, freetext$, disc_pct,     ~
                            disc_amt, ten99$, hold$, invtype$, text$, " "

                REM Write Out Invoice For Posting...
L31760:         call "GETDTTM" addr (datetime$)
                write #9, using L31790, userid$, datetime$, str(lot$()),  ~
                                      str(lot$(),201,140), eod goto L31760
L31790:         FMT CH(3), CH(7), CH(200), CH(140)
L31820:         call "TXTFUTIL" (#25, 0%, "SAVE", text$)
                return

L31920:     FMT CH(9),                   /* VENDOR CODE                */~
                CH(16),                  /* INVOICE NUMBER             */~
                CH(16),                  /* RECEIVER NUMBER            */~
                CH(6),                   /* INVOICE DATE               */~
                CH(9),                   /* PURCHASES ACCOUNT          */~
                CH(9),                   /* PAYABLES ACCOUNT           */~
                CH(1),                   /* PAYABLES ACCOUNT TYPE      */~
                CH(6),                   /* PAY W/O DISCOUNT DATE      */~
                CH(6),                   /* PAY W/DISCOUNT DATE        */~
                PD(14,4),                /* NON-DISCOUNTABLE AMOUNT    */~
                CH(6),                   /* LAST POSTING DATE          */~
                CH(6),                   /* ORIGINAL INPUT DATE        */~
                CH(3),                   /* ORIGINAL INPUT BY          */~
                CH(6),                   /* LAST MOD DATE              */~
                CH(3),                   /* LAST MOD BY                */~
                PD(14,4),                /* INVOICE AMOUNT             */~
                PD(14,4),                /* INVOICE OPEN AMOUNT        */~
                CH(20),                  /* FREE TEXT FIELD            */~
                PD(14,4),                /* DISC PERCENT               */~
                PD(14,4),                /* DISCOUNT AMOUNT            */~
                CH(4),                   /* 1099 CATEGORY              */~
                CH(1),                   /* HOLD STATUS                */~
                CH(1),                   /* INVOICE TYPE (INTERNAL)    */~
                CH(4),                   /* TEXT ID NUMBER             */~
                CH(168)                  /* FILLER                     */

L32180:     FMT CH(16),                  /* RECEIVER NUMBER            */~
                CH(16),                  /* PO NUMBER                  */~
                CH(03),                  /* PO LINE NUMBER             */~
                CH(9),                   /* VENDOR CODE                */~
                CH(16),                  /* INVOICE NUMBER             */~
                CH(3),                   /* SEQUENCE NUMBER            */~
                CH(9),                   /* EXPENSE ACCOUNT NUMBER     */~
                CH(25),                  /* PART NUMBER                */~
                PD(14,4),                /* QUANTITY                   */~
                PD(14,4),                /* EXTENSION                  */~
                CH(8),                   /* JOB NUMBER                 */~
                CH(03),                  /* STORE NUMBER               */~
                CH(06),                  /* LOT NUMBER                 */~
                PD(14,7),                /* UNIT PRICE                 */~
                CH(3),                   /* OLD SEQUENCE NUMBER        */~
                PD(14,4),                /* Quantity Per Vendor Unit   */~
                CH(4),                   /* Unit Of Measure            */~
                CH(25),                  /* Vendors Part Number        */~
                CH(04),                  /* FILLER                     */~
                PD(14,4),                /* Total Inventory cost       */~
                CH(96),                  /* Inventory cost array       */~
                CH(9),                   /* Price cost variance acct # */~
                CH(246)                  /* FILLER                     */

        auto_distribute
            u3% = maxlines% /* Save incoming MAXLINES% */
            call "GLDSTSUB" (#12, #2, #11, distcode$, damt, "D",         ~
                      maxlines%, acct$(), part$(), ext$(), job$(), hits%)
            errormsg$ = "No Lines Were Created"
            distamt$ = " "
            if hits% < 1 then return
            gosub total_up_invoice
            errormsg$ = " "
            if hits% < 1000% then L35110
                errormsg$="Warning: Invalid Accts On Table Were Skipped"
                return
L35110:     counter = hits% : init (" ") xob$()
            for u1% = u3% + 1% to maxlines% /* Lines added by GLDSTSUB */
                if job$(u1%) = " " then goto L35250
                for u2% = 1% to 100%  /* Don't ASKUSER same job twice */
                     if xob$(u2%) = " " then goto L35170
                     if xob$(u2%) = job$(u1%) then goto L35240
                next u2%
L35170:         call "READ100" (#13, job$(u1%), f1%(13))
                if f1%(13) = 0% then goto L35250
L35181:         u4% = 2%
                call "ASKUSER" (u4%,                                     ~
                     "AUTO DISTRIBUTION: PROJECT/JOB CONFILCT",          ~
                     "Project # " & job$(u1%) & " is also a Job Number", ~
                     "Project # " & job$(u1%) & " will be set = spaces", ~
                     "Press (RETURN) to acknowledge & continue")
                if u4% <> 0% then goto L35181
                xob$(u2%) = job$(u1%)
L35240:         job$(u1%) = " "
L35250:     next u1%
            call "CONVERT" (counter, -0.001, str(errormsg$,,3))
            errormsg$ = errormsg$ & " Lines" & hex(8c) & "Were Created"
        return

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
                header$(1) = " "
                if fieldnr% > 1% then L40270
                str(pfdescr$(1),19,19) = " "    /* Shut Off Prev Field */
                str(pfkeys$,3,2) = hex(ffff)
L40270:         if fieldnr% > 2% then L40310
                if lastvendor$ <> " " then header$(1) = "Last Vendor: " &~
                     lastvendor$ & "  " & "Last Invoice: " & lastinvoice$
                goto L40550
L40310:         pfdescr$(3) = " "               /* Shut Off Exit       */
                str(pfkeys$,6,1),str(pfkeys$,8,1) = hex(ff)
                goto L40550

            deffn'211(fieldnr%)
                REM Editmode logic...
                pfdescr$(1) = "(1)Start Over                             ~
        ~                     (13)Instructions"
                pfdescr$(2) = "(2)Line Items      (5)Next Page           ~
        ~                     (15)Print Screen"
                pfdescr$(3) = "(25)Free Text                             ~
        ~                     (16)Save Data"
                pfkeys$ = hex(000102050d0f10191d)
                header$(1) = "Invoice Total: " & invtotal$
                str(header$(1),27) = "Disc: " & disc_amt$
                str(header$(1),44) = "Net: " & invnet$
                init(hex(8c)) lfac$()
                init(hex(86)) str(lfac$(),3)
                if fieldnr% = 0% then L40550
                     str(pfdescr$(2),,63), pfdescr$(3) = " "
                     str(pfkeys$,3,2), str(pfkeys$,7,3) = hex(ffffff)
                     init(hex(8c)) lfac$()

L40550:           str(pfdescr$(3),63,1) = hex(84)
                  str(header$(1),62) = str(manual$) & ": " & cms2v$
                  on fieldnr% gosub L40700,         /* Vendor Code      */~
                                    L40700,         /* Invoice Number   */~
                                    L40700,         /* Invoice Date     */~
                                    L40700,         /* Regular Date     */~
                                    L40700,         /* Discount Date    */~
                                    L40700,         /* Discount Percent */~
                                    L40700,         /* Discount Amount  */~
                                    L40700          /* Non Disc Amount  */
                     goto L40770

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40700:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40770:     accept                                                       ~
               at (01,02), fac(hex(8c)),   topline$             , ch(79),~
               at (02,02), fac(hex(ac)),   header$(1)           , ch(79),~
               at (04,02), fac(hex(94)),   errormsg$            , ch(79),~
                                                                         ~
               at (06,02), "Vendor Code",                                ~
               at (06,30), fac(lfac$(1)), vencode$              , ch(09),~
               at (06,49), fac(hex(8c)),  vendescr$             , ch(32),~
               at (07,02), "Invoice Number",                             ~
               at (07,30), fac(lfac$(2)), invoicenr$            , ch(16),~
               at (09,02), "Vendor Name",                                ~
               at (09,30), fac(hex(8c)),  venaddr$(1)           , ch(30),~
               at (10,02), "   Address (Line 1)",                        ~
               at (10,30), fac(hex(8c)),  venaddr$(2)           , ch(30),~
               at (11,02), "           (Line 2)",                        ~
               at (11,30), fac(hex(8c)),  venaddr$(3)           , ch(30),~
               at (12,02), "           (Line 3)",                        ~
               at (12,30), fac(hex(8c)),  venaddr$(4)           , ch(30),~
               at (13,02), "           (Line 4)",                        ~
               at (13,30), fac(hex(8c)),  venaddr$(5)           , ch(30),~
               at (14,02), "   City, State, Zip",                        ~
               at (14,30), fac(hex(8c)),  venaddr$(6)           , ch(30),~
               at (15,02), "Invoice Date",                               ~
               at (15,30), fac(lfac$(3)),  invdate$             , ch(08),~
               at (16,02), "Regular Disbursement Date",                  ~
               at (16,30), fac(lfac$(4)),  regulardate$         , ch(08),~
               at (17,02), "Discount Disbursement Date",                 ~
               at (17,30), fac(lfac$(5)),  discdate$            , ch(08),~
               at (18,02), "Discount Percent",                           ~
               at (18,30), fac(lfac$(6)),  disc_pct$            , ch(10),~
               at (18,41), "And Amount",                                 ~
               at (18,52), fac(lfac$(7)),  disc_amt$            , ch(10),~
               at (19,02), "Non-Discountable Amount",                    ~
               at (19,30), fac(lfac$(8)),  nondisc$             , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfdescr$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L41380
                  call "MANUAL" (manual$)
                  goto L40770

L41380:        if keyhit% <> 15 then L41420
                  call "PRNTSCRN"
                  goto L40770

L41420:        if editmode% = 0 then return
               close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   2      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'202(fieldnr%)
                init(hex(8c)) lfac$()
                header$(1) = " "
                pfdescr$(1) = "(1)Start Over     (4)Previous Field       ~
        ~                     (13)Instructions"
                pfdescr$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfdescr$(3) = "                                          ~
        ~                                     "
                pfkeys$ = hex(0001040d0f)

                REM Turn off appropriate fields...
                if fieldnr% > 1% then L42190
                str(pfdescr$(1),19,19) = " "
                str(pfkeys$,3,2) = hex(ffff)
                goto L42190

            deffn'212(fieldnr%)
                REM Editmode logic...
                pfdescr$(1) = "(1)Start Over                             ~
        ~                     (13)Instructions"
                pfdescr$(2) = "(2)Line Items      (4)Prev Page           ~
        ~                     (15)Print Screen"
                pfdescr$(3) = "(25)Free Text                             ~
        ~                        (16)Save Data"
                pfkeys$ = hex(000102040d0f10191d)
                init(hex(86)) lfac$()
                if fieldnr% = 0% then L42190
                     str(pfdescr$(2),,63), pfdescr$(3) = " "
                     str(pfkeys$,3,2), str(pfkeys$,7,3) = hex(ffffff)
                     init(hex(8c)) lfac$()

L42190:           str(pfdescr$(3),63,1) = hex(84)
                  header$(1) = "Vendor: " & vencode$ & ", Invoice: " &   ~
                                                               invoicenr$
                  str(header$(1),62) = str(manual$) & ": " & cms2v$
                  on fieldnr% gosub L42265,         /* Free Text Field  */~
                                    L42265,         /* 1099 Category    */~
                                    L42265,         /* Payables Account */~
                                    L42265,         /* Hold Status      */~
                                    L42265,         /* Purch Account    */~
                                    L42265          /* Dist Code & Amt  */
                     goto L42300

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L42265:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L42300:     accept                                                       ~
               at (01,02), fac(hex(8c)),   topline$             , ch(79),~
               at (02,02), fac(hex(ac)),   header$(1)           , ch(79),~
               at (04,02), fac(hex(94)),   errormsg$            , ch(79),~
                                                                         ~
               at (06,02), "Free Text Field",                            ~
               at (06,30), fac(lfac$( 1)), freetext$            , ch(20),~
               at (07,02), "1099 Category Code",                         ~
               at (07,30), fac(lfac$( 2)), ten99$               , ch(04),~
               at (07,49), fac(hex(8c)),     ten99descr$        , ch(30),~
               at (08,02), "Payables Account",                           ~
               at (08,30), fac(lfac$( 3)), payacct$             , ch(12),~
               at (08,49), fac(hex(8c)),     payacctdescr$      , ch(32),~
               at (09,02), "Invoice On Hold? (Y/N)",                     ~
               at (09,30), fac(lfac$( 4)), hold$                , ch(01),~
               at (10,02), "Default Purchases Account",                  ~
               at (10,30), fac(lfac$( 5)), puracct$             , ch(12),~
               at (10,49), fac(hex(8c)),     puracctdescr$      , ch(32),~
               at (11,02), "Auto Distribute Code & Amt",                 ~
               at (11,30), fac(lfac$( 6)), distcode$            , ch(06),~
               at (11,40), fac(lfac$( 6)), distamt$             , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfdescr$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L42470
                  call "MANUAL" (manual$)
                  goto L42300

L42470:        if keyhit% <> 15 then L42490
                  call "PRNTSCRN"
                  goto L42300

L42490:        if editmode% = 0 then return
               close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   3      *~
            * --------------------------------------------------------- *~
            * Input/Edit of First Line Item Screen.                     *~
            *************************************************************

            deffn'103(screenline%,fieldnr%)  /* Input Mode, Lines */
                pfdescr$(1) = "(1)Start Invoice Over                   "&~
                              "                       (13)Instructions"
                pfdescr$(2) = "(2)Restart Line     (4)Prev. Field      "&~
                              "                       (15)Print Screen"
                pfdescr$(3) = "                    (6)Same as Prev Line"&~
                              "                       (16)Edit Mode"
                pfkeys$ = hex(00010204060d0f10)
                if fieldnr% > 1% then L43470
                  str(pfdescr$(2),,37) = " "  /* Shut Off Prev Field */
                  str(pfkeys$,3,2) = hex(ffff)
                  goto L43470

            deffn'113(screenline%,fieldnr%) /* Edit Mode Lines */
                pfdescr$(1) = "(1)Start Over    (4)Prev Lines  (6)Down On~
        ~e  (11)Insert Line   (13)Instructions"
                pfdescr$(2) = "(2)First Lines   (5)Next Lines  (7)Up One ~
        ~   (12)Delete Line   (15)Print Screen"
                pfdescr$(3) = "(3)Last Lines    (25)Free Text  (9)Header ~
        ~   (28)Delete All    (16)Save Invoice"
                pfkeys$ = hex(00010203040506070a0b0c0d0f1019091c1d)

                REM Turn Off Appropriate Fields. Are we editing a field?
                if fieldnr% = 0% then L43340  /* no */
                     str(pfdescr$(1),14,49), str(pfdescr$(2),,63),       ~
                                                        pfdescr$(3) = " "
                     init(hex(ff)) str(pfkeys$,3,9), str(pfkeys$,14)
                     goto L43470
L43340:         REM Display Mode...
                if base% > 0 then L43390        /* Shut Off Prev Stuff */
                  str(pfdescr$(1),14,30), str(pfdescr$(2),,14) = " "
                  str(pfkeys$,3,1), str(pfkeys$,5,1), str(pfkeys$,7,1) = ~
                                                                  hex(ff)
L43390:         if base%+13 < maxlines% then L43430 /* Shut Off Next */
                  str(pfdescr$(2),14,30), str(pfdescr$(3),,14) = " "
                  str(pfkeys$,4,1), str(pfkeys$,6,1), str(pfkeys$,8,1) = ~
                                                                  hex(ff)
L43430:         if maxlines% > 0 then L43470    /* Shut Off Deletes */
                  str(pfdescr$(2),46,15), str(pfdescr$(3),46,15) = " "
                  str(pfkeys$,11,1), str(pfkeys$,17,1) = hex(ff)

L43470:         REM Set Up header Portion Of Screen...
                init(hex(8c)) mfac$(), tfac$()
                if fieldnr% = 0% then init(hex(8e)) tfac$()
                if fieldnr% = 0% then init(hex(86)) mfac$()
                header$(2) = "Line Description                   Amount  ~
        ~Exp. Account              Job/Proj"
                header$(1) = "Vendor:" & hex(84) & vencode$ & hex(8c)
                header$(1) = header$(1) & vendescr$
                str(header$(1),62) = str(manual$) & ": " & cms2v$
                str(tfac$(), min(20, (maxlines%-base%)+1)) = all(hex(9c))
                str(pfdescr$(3),63,1) = hex(84)

                on fieldnr% gosub L43690,         /* Part Number        */~
                                  L43690,         /* Extension          */~
                                  L43690,         /* Account Number     */~
                                  L43690,         /* Job Number         */~
                                  L43750          /* Delete Line Item   */
                     goto L43820

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      mfac$(screenline%, fieldnr%) = hex(80)
                      return
L43690:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      mfac$(screenline%, fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      mfac$(screenline%, fieldnr%) = hex(82)
                      return
L43750:           REM Set Blinking FAC For Delete Of Line...
                  init (hex(94)) str(mfac$(),4*screenline%-3%,4)
                  str(pfdescr$(1),,62) = "(1)Cancel Delete Request"
                  str(pfdescr$(2),,62) = " "
                  pfdescr$(3) = "(RETURN)Delete Item"
                  return

L43820:     accept                                                       ~
               at (01,02), "Manage Invoice Line Items",                  ~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(8c)), header$(1)             , ch(79),~
               at (03,02), "Invoice:",                                   ~
               at (03,11), fac(hex(84)), invoicenr$             , ch(16),~
               at (03,31), "Total:",                                     ~
               at (03,38), fac(hex(84)), invtotal$              , ch(10),~
               at (03,49), "Disc:",                                      ~
               at (03,55), fac(hex(84)), disc_amt$              , ch(10),~
               at (03,66), "Net:",                                       ~
               at (03,71), fac(hex(84)), invnet$                , ch(10),~
               at (05,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), fac(hex(ac)), header$(2)             , ch(04),~
               at (06,07), fac(hex(ac)), str(header$(2),6)      , ch(74),~
                                                                         ~
               at (07,02), fac(tfac$( 1)), seq$   (base%+ 1%)   , ch(04),~
               at (08,02), fac(tfac$( 2)), seq$   (base%+ 2%)   , ch(04),~
               at (09,02), fac(tfac$( 3)), seq$   (base%+ 3%)   , ch(04),~
               at (10,02), fac(tfac$( 4)), seq$   (base%+ 4%)   , ch(04),~
               at (11,02), fac(tfac$( 5)), seq$   (base%+ 5%)   , ch(04),~
               at (12,02), fac(tfac$( 6)), seq$   (base%+ 6%)   , ch(04),~
               at (13,02), fac(tfac$( 7)), seq$   (base%+ 7%)   , ch(04),~
               at (14,02), fac(tfac$( 8)), seq$   (base%+ 8%)   , ch(04),~
               at (15,02), fac(tfac$( 9)), seq$   (base%+ 9%)   , ch(04),~
               at (16,02), fac(tfac$(10)), seq$   (base%+10%)   , ch(04),~
               at (17,02), fac(tfac$(11)), seq$   (base%+11%)   , ch(04),~
               at (18,02), fac(tfac$(12)), seq$   (base%+12%)   , ch(04),~
               at (19,02), fac(tfac$(13)), seq$   (base%+13%)   , ch(04),~
                                                                         ~
               at (07,07), fac(mfac$( 1,1)), part$(base%+ 1%)   , ch(25),~
               at (08,07), fac(mfac$( 2,1)), part$(base%+ 2%)   , ch(25),~
               at (09,07), fac(mfac$( 3,1)), part$(base%+ 3%)   , ch(25),~
               at (10,07), fac(mfac$( 4,1)), part$(base%+ 4%)   , ch(25),~
               at (11,07), fac(mfac$( 5,1)), part$(base%+ 5%)   , ch(25),~
               at (12,07), fac(mfac$( 6,1)), part$(base%+ 6%)   , ch(25),~
               at (13,07), fac(mfac$( 7,1)), part$(base%+ 7%)   , ch(25),~
               at (14,07), fac(mfac$( 8,1)), part$(base%+ 8%)   , ch(25),~
               at (15,07), fac(mfac$( 9,1)), part$(base%+ 9%)   , ch(25),~
               at (16,07), fac(mfac$(10,1)), part$(base%+10%)   , ch(25),~
               at (17,07), fac(mfac$(11,1)), part$(base%+11%)   , ch(25),~
               at (18,07), fac(mfac$(12,1)), part$(base%+12%)   , ch(25),~
               at (19,07), fac(mfac$(13,1)), part$(base%+13%)   , ch(25),~
                                                                         ~
               at (07,33), fac(mfac$( 1,2)), ext$ (base%+ 1%)   , ch(10),~
               at (08,33), fac(mfac$( 2,2)), ext$ (base%+ 2%)   , ch(10),~
               at (09,33), fac(mfac$( 3,2)), ext$ (base%+ 3%)   , ch(10),~
               at (10,33), fac(mfac$( 4,2)), ext$ (base%+ 4%)   , ch(10),~
               at (11,33), fac(mfac$( 5,2)), ext$ (base%+ 5%)   , ch(10),~
               at (12,33), fac(mfac$( 6,2)), ext$ (base%+ 6%)   , ch(10),~
               at (13,33), fac(mfac$( 7,2)), ext$ (base%+ 7%)   , ch(10),~
               at (14,33), fac(mfac$( 8,2)), ext$ (base%+ 8%)   , ch(10),~
               at (15,33), fac(mfac$( 9,2)), ext$ (base%+ 9%)   , ch(10),~
               at (16,33), fac(mfac$(10,2)), ext$ (base%+10%)   , ch(10),~
               at (17,33), fac(mfac$(11,2)), ext$ (base%+11%)   , ch(10),~
               at (18,33), fac(mfac$(12,2)), ext$ (base%+12%)   , ch(10),~
               at (19,33), fac(mfac$(13,2)), ext$ (base%+13%)   , ch(10),~
                                                                         ~
               at (07,45), fac(mfac$( 1,3)), acct$(base%+ 1%)   , ch(12),~
               at (08,45), fac(mfac$( 2,3)), acct$(base%+ 2%)   , ch(12),~
               at (09,45), fac(mfac$( 3,3)), acct$(base%+ 3%)   , ch(12),~
               at (10,45), fac(mfac$( 4,3)), acct$(base%+ 4%)   , ch(12),~
               at (11,45), fac(mfac$( 5,3)), acct$(base%+ 5%)   , ch(12),~
               at (12,45), fac(mfac$( 6,3)), acct$(base%+ 6%)   , ch(12),~
               at (13,45), fac(mfac$( 7,3)), acct$(base%+ 7%)   , ch(12),~
               at (14,45), fac(mfac$( 8,3)), acct$(base%+ 8%)   , ch(12),~
               at (15,45), fac(mfac$( 9,3)), acct$(base%+ 9%)   , ch(12),~
               at (16,45), fac(mfac$(10,3)), acct$(base%+10%)   , ch(12),~
               at (17,45), fac(mfac$(11,3)), acct$(base%+11%)   , ch(12),~
               at (18,45), fac(mfac$(12,3)), acct$(base%+12%)   , ch(12),~
               at (19,45), fac(mfac$(13,3)), acct$(base%+13%)   , ch(12),~
                                                                         ~
               at (07,72), fac(mfac$( 1,4)), job$ (base%+ 1%)   , ch(08),~
               at (08,72), fac(mfac$( 2,4)), job$ (base%+ 2%)   , ch(08),~
               at (09,72), fac(mfac$( 3,4)), job$ (base%+ 3%)   , ch(08),~
               at (10,72), fac(mfac$( 4,4)), job$ (base%+ 4%)   , ch(08),~
               at (11,72), fac(mfac$( 5,4)), job$ (base%+ 5%)   , ch(08),~
               at (12,72), fac(mfac$( 6,4)), job$ (base%+ 6%)   , ch(08),~
               at (13,72), fac(mfac$( 7,4)), job$ (base%+ 7%)   , ch(08),~
               at (14,72), fac(mfac$( 8,4)), job$ (base%+ 8%)   , ch(08),~
               at (15,72), fac(mfac$( 9,4)), job$ (base%+ 9%)   , ch(08),~
               at (16,72), fac(mfac$(10,4)), job$ (base%+10%)   , ch(08),~
               at (17,72), fac(mfac$(11,4)), job$ (base%+11%)   , ch(08),~
               at (18,72), fac(mfac$(12,4)), job$ (base%+12%)   , ch(08),~
               at (19,72), fac(mfac$(13,4)), job$ (base%+13%)   , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfdescr$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(3)          , ch(79),~
                                                                         ~
                    keys(pfkeys$),                                       ~
                    key(keyhit%)

               if keyhit% <> 13 then L44980
                  call "MANUAL" (manual$)
                  goto L43820

L44980:        if keyhit% <> 15 then L45020
                  call "PRNTSCRN"
                  goto L43820

L45020:        if linemode% = 0 then return
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
                  on fieldnr% gosub L50180,         /* VENDOR CODE      */~
                                    L50310,         /* INVOICE NUMBER   */~
                                    L50560,         /* DATE OF INVOICE  */~
                                    L50650,         /* PAY W/O DISC DATE*/~
                                    L50700,         /* PAY W/DISC DATE  */~
                                    L50760,         /* DISC PCT         */~
                                    L50840,         /* DISC AMOUNT      */~
                                    L50920          /* NON-DISCOUNTABLE */
                  return

L50180:     REM TEST FOR VENDOR CODE ON FILE.
                vendescr$ = hex(0684) & "Select Vendor"
                call "GETCODE" (#3, vencode$, vendescr$, 1%, 1.3, f1%(3))
                     if f1%(3) = 0 then L50290
                REM GET ADDRESS & STUFF, JUST FOR FUN.
                    get #3, using L50240, venaddr$()
L50240:             FMT XX(39), 6*CH(30)
                    get #3, using L50260, billsdue%, discsdue%, discpercent
L50260:                     FMT XX(285), 3*PD(14,4)
                     disc_pct = discpercent /* DEFAULT VALUE           */
                    return
L50290:         errormsg$ = "Vendor Not On File: " & vencode$
                return
L50310:     REM TEST FOR VENDOR CODE/INVOICE ON FILE
                if invoicenr$ <> " " then L50460
                readkey$ = vencode$
                errormsg$ = hex(06) & "Select Invoice For Edit?"
                header$(), inc$() = " "
                mat inc = zer
                inc(2) =  168.01 : inc$(2) = "N"
                call "PLOWCODE" (#5, readkey$, errormsg$, 5009%,         ~
                               0, f1%(5), header$(), 0, 0, inc(), inc$())
                     if f1%(5) <> 0 then L50450
                     errormsg$ = hex(00)
                     return
L50450:         invoicenr$ = str(readkey$,10)
L50460:         errormsg$ = " "
                if str(invoicenr$,,1) <> "#" then L50510
                   if str(invoicenr$,6) <> " " then L50510
                     errormsg$ = "Sorry, Reserved Invoice Number"
                     return
L50510:         gosub L30000
                if errormsg$ <> " " then return
                if oldinvoiceonfile% = 0 then return
                   return clear all
                   goto editmode
L50560:     REM TEST FOR VALID INVOICE DATE
                if invdate$ <> " " and invdate$ <> blankdate$ then L50600
                   invdate$ = paydate$
L50600:         call "DATEOK" (invdate$, temp%, errormsg$)
                     if errormsg$ <> " " then return
                        gosub L56000                /* REGULAR DATE JUNK*/
                        gosub L56650                /* DISCOUNT DATE    */
                     return
L50650:     REM TEST FOR VALID "DISBURSEMENT WITHOUT DISCOUNT" DATE
                if regulardate$ = " " or regulardate$ = blankdate$ then ~
                   gosub L56310
                call "DATEOK" (regulardate$, temp%, errormsg$)
                     return
L50700:     REM TEST FOR VALID DISBURSEMENT WITH DISCOUNT DATE
                if discdate$ = " " or discdate$ = blankdate$ then gosub L56650
                if discdate$ = "00/00/00" then return
                call "DATEOK" (discdate$, temp%, errormsg$)
                     return
L50760:     REM TEST FOR VALID DISC PERCENT
                disc_pct  = 0
                if disc_pct$ = " " then L50820
                call"NUMTEST"(disc_pct$,-9e7,1000,errormsg$,2.4,disc_pct)
                disc_amt = round((invamt-nondisc)*disc_pct/100,2)
                call "CONVERT" (disc_amt, -2.2, disc_amt$)
L50820:         gosub total_up_invoice
                return
L50840:     REM TEST FOR VALID DISC AMOUNT
                call "NUMTEST"(disc_amt$,-9e7,9e7,errormsg$,2.2,disc_amt)
                     if errormsg$ <> " " then return
                if disc_amt = round((invamt-min(invamt,nondisc)) *       ~
                                               disc_pct/100,2) then L50900
                     disc_pct = 0 : disc_pct$ = " "
L50900:         gosub total_up_invoice
                return
L50920:     REM TEST FOR VALID NON-DISCOUNTABLE AMOUNT
                call "NUMTEST" (nondisc$, 0,9e7, errormsg$, 2.2, nondisc)
                gosub total_up_invoice
                return

        REM *************************************************************~
            *         T E S T   D A T A   F R O M   H E A D E R         *~
            *                                                           *~
            * TESTS ALL THE DATA ON THE PAYABLES INVOICE HEADER.        *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51160,         /* FREE TEXT FIELD  */~
                                    L51200,         /* 1099 CATEGORY    */~
                                    L51320,         /* PAYABLES         */~
                                    L51420,         /* HOLD STATUS      */~
                                    L51480,         /* DEFAULT PURCHASES*/~
                                    L51570          /* DIST CODE & AMOUN*/
                  return

L51160:     REM TEST FREE TEXT FIELD
                REM ACTUALLY, THERE'S NOTHING TO TEST...
                return

L51200:     REM TEST FOR 1099 CATEGORY
                ten99descr$ = " "
                if ten99$ = " " then return
                readkey$ = "1099 CATS" & ten99$
                ten99descr$ = hex(06) & "Select 1099 Category"
                call "PLOWCODE" (#20,readkey$,ten99descr$,9%,0,f1%(20))
                     if f1%(20) = 1% then L51290
                     errormsg$ = "Invalid 1099 Category Code"
                     return
L51290:         ten99$ = str(readkey$,10)
                return

L51320:     REM TEST FOR VALID PAYABLES ACCOUNT
                payacctdescr$ = hex(0694) & "Select The Payables Account"
                call "GETCODE" (#2, payacct$, payacctdescr$, 1%,0,f1%(2))
                     if f1%(2) = 0 then L51390
                        get #2, using L51370, payaccttype$
L51370:                         FMT XX(39), CH(1)
                        return
L51390:         errormsg$ = "Payables Account Not On File: " & payacct$
                return

L51420:     REM TEST FOR HOLD STATUS
                if hold$ = " " then hold$ = "N"
                if pos("NY" = hold$) <> 0 then return
                     errormsg$ = "Please Enter Y or N: " & hold$
                     return

L51480:     REM TEST FOR VALID PURCHASES ACCOUNT DEFAULT
                puracctdescr$ = hex(0684) & "Select Purchase Account"
                call "GETCODE" (#2, puracct$, puracctdescr$,1%,0,f1%(2))
                if f1%(2) <> 0 then L51540
                   errormsg$="Purchases Account Not on File: " & puracct$
                   return
L51540:         defaultacct$ = puracct$
                return

L51570:     REM TEST FOR AUTO DISTRIBUTE CODE AND AMOUNT
                if distcode$ = " " and distamt$ = " " then return
                call "PLOWCODE" (#12, distcode$, " ", -6%, -.30, f1%(12))
                if f1%(12) <> 0 then L51630
                  errormsg$="Distribution Table Not on File: "&distcode$
                  return
L51630:         call "NUMTEST" (distamt$, 0, 9e7, errormsg$, 2.2, damt)
                     if errormsg$ <> " " then return
                if damt > 0 then L51680
                  errormsg$="Amount Must Be Greater Then Zero: "&distamt$
                  return
L51680:         gosub auto_distribute
                return clear all
                goto edit_lines

        REM *************************************************************~
            *            T E S T   L I N E   I T E M   D A T A          *~
            *                                                           *~
            * VERIFY ALL LINE ITEM DATA.                                *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L53000,         /* INVENTORY PART   */~
                                    L53090,         /* EXTENSION        */~
                                    L53140,         /* DEBIT ACCOUNT    */~
                                    L53220          /* JOB NUMBER       */
                  return

L53000:     REM TEST DATA FOR PART NUMBER
                if part$(c%) <> " " then L53040
                     errormsg$ = "Description Cannot Be Blank"
                     return
L53040:         call "DESCRIBE" (#4, part$(c%), " ", 0%, f1%(4))
                     if f1%(4) = 0 then return
                     errormsg$ = "Sorry, Can't Be An Inventory Item"
                     return

L53090:     REM TEST DATA FOR EXTENSION
                call "NUMTEST" (ext$(c%), -9e8, 9e8, errormsg$,-2.2,temp)
                     if errormsg$ <> " " then return
                return

L53140:     REM TEST DATA FOR VALID DEBIT ACCOUNT
                call "GETCODE" (#2, acct$(c%), acctdescr$, 1%, 0, f1%(2))
                if f1%(2) <> 0 then L53190
                   errormsg$ = "Account Not On File: " & acct$(c%)
                   return
L53190:         errormsg$ = hex(84) & acctdescr$
                return

L53220:     REM TEST DATA FOR JOB NUMBER
                jobdescr$ = " "
                if job$(c%)=" " then return
                call "DESCRIBE" (#13,job$(c%),jobdescr$,1%,f1%(13))
                      if f1%(13) = 1 then return
                call "DESCRIBE" (#11,job$(c%),jobdescr$,1%,f1%(11))
                      if f1%(11) = 1 then return
                call "GETCODE" (#13,job$(c%),jobdescr$,1%,0,f1%(13))
                      if f1%(13) = 1 then return
                call "GETCODE" (#11,job$(c%),jobdescr$,1%,0,f1%(11))
                      if f1%(11) = 1 then return
                errormsg$ = "Job/Project Not On File"
                return

L56000: REM *************************************************************~
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

L56310:     REM ROUTINE THAT COMPUTES REGULARDATE$
                date1$ = invdate$
                call "DATUNFMT" (date1$)
                date2$ = date1$

                on sgn (billsdue%)+2 gosub L56510, L56410, L56470
                   regulardate$ = date2$
                   call "DATEFMT" (regulardate$)
                   return

L56410:         REM CASE 1--IF BILLS DUE = 0 THEN USE SYSTEM DEFAULT
                    if sysbillsdue% = 0% then return
                       billsdue% = sysbillsdue%
                       on sgn (billsdue%)+2 goto L56510,,L56470
                       return

L56470:         REM CASE 2--IF BILLS DUE > 0 THEN USE INV DATE + DAYS
                    call "DATE" addr("G+",date1$,billsdue%,date2$,err%)
                    return

L56510:         REM CASE 3--IF BILLS DUE < 0 THEN SEE EXPLANATION ABOVE
                    tdate$ = date1$
                    call "DATEFMT" (tdate$, 0%, date1$)
                    convert str(date1$,5%,2%) to month%
                    convert str(date1$,1%,4%) to year%
                    month% = month% + 1%
                    if month% < 13% then L56580      /* CARRY YEAR       */
                       year% = year% + 1%
                       month% = 1%
L56580:             day% = abs(billsdue%)
                    REM CONVERT DATES BACK TO REGULAR DATE FORMAT.
                        convert year%  to str(date2$,1%,4%), pic(0000)
                        convert month% to str(date2$,5%,2%), pic(00)
                        convert day%   to str(date2$,7%,2%), pic(00)
                        call "DATECONV" (date1$)
                        call "DATECONV" (date2$)
                           return

L56650:     REM ROUTINE THAT COMPUTES DISCDATE$
                date1$ = invdate$
                call "DATUNFMT" (date1$)
                date2$ = "00/00/00"
                if discpercent = 0 then L56720

                   on sgn (discsdue%)+2 gosub L56860, L56760, L56820
L56720:               discdate$ = date2$
                      call "DATEFMT" (discdate$)
                         return

L56760:         REM CASE 1--IF BILLS DUE = 0 THEN USE SYSTEM DEFAULT
                    if sysdiscsdue% = 0% then return
                       discsdue% = sysdiscsdue%
                       on sgn (discsdue%)+2 goto L56860,,L56820
                       return

L56820:         REM CASE 2--IF DISCOUNTS DUE > 0 THEN USE INV DATE + DAYS
                    call "DATE" addr("G+",date1$,discsdue%,date2$,err%)
                    return

L56860:         REM CASE 3--IF DISCOUNTS DUE < 0 THEN SEE EXPLANATION
                    tdate$ = date1$
                    call "DATEFMT" (tdate$, 0%, date1$)
                    convert str(date1$,5%,2%) to month%
                    convert str(date1$,1%,4%) to year%
                    month% = month% + 1%
                    if month% < 13% then L56930      /* CARRY YEAR       */
                       year% = year% + 1%
                       month% = 1%
L56930:             day% = abs(discsdue%)
                    REM CONVERT DATES BACK TO REGULAR DATE FORMAT.
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
