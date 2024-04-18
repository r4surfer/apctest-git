        rem**************************************************************~
            *                                                           *~
            *   ggg   l      dcdd   iiiii   sss   ttttt  iiiii  n   n   *~
            *  g      l      d   d    i    s        t      i    nn  n   *~
            *  g ggg  l      d   d    i     sss     t      i    n n n   *~
            *  g   g  l      d   d    i        s    t      i    n  nn   *~
            *   ggg   lllll  dddd   iiiii   sss     t    iiiii  n   n   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * gldistin - enter default g/l distribution tables          *~
            *----------------------------------------------------------q*~
            *                  m o d i f i c a t i o n s                *~
            *---when---+----------------what----------------------+-who-*~
            * 02/02/87 ! original                                 ! hes *~
            * 06/08/87 ! added proj/job validation via jbmastr2.  ! jim *~
            *************************************************************

        dim                                                              ~
            acct$(100)12,                /* G/L Accounts               */~
            code$6,                      /* Distribution Code          */~
            codedescr$30,                /* Distribution Code Descrip  */~
            cursor%(2),                  /* CURSOR LOCATIONS FOR EDIT  */~
            date$8,                      /* TODAY'S CLOCK DATE         */~
            descr$(100)27,               /* Account Descriptions       */~
            dfac$(20)1,                  /* SUMMARY SCREEN FAC'S       */~
            dcflag$(100)1,               /* Debit/Credit Flag          */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            famt$(100)9,                 /* Flat Amount For Distributon*/~
            header$79,                   /* Screen Title               */~
            header1$79,                  /* Screen Subtitle            */~
            i$(24)80,                    /* SCREEN IMAGE (NOT USED)    */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            message$79,                  /* INPUT MESSAGE TEXT         */~
            pct$(100)7,                  /* Percent to distribute      */~
            pfktext$(3)79,               /* FUNCTION KEYS ENABLED LISTS*/~
            pfkeys$32,                   /* FUNCTION KEYS ENABLED LISTS*/~
            proj$(100)8,                 /* Project Codes              */~
            qfac$(20,6)1,                /* FIELD ATTRIBUTE CHARACTERS */~
            readkey$70,                  /* KEY TO PLOW FILE WITH      */~
            readkey1$70,                 /* KEY TO PLOW FILE WITH      */~
            savecode$6,                  /* Distribution Code For Copy */~
            savedescr$30,                /* Distribution Descr For Copy*/~
            seq$(999)5,                  /* SEQUENCE NUMBERS FOR SCREEN*/~
            tfac$(37)1,                  /* SUMMARY SCREEN FAC'S       */~
            total$(2,2)9                 /* Totals For Screen          */

        dim f2%(24),                     /* FILE STATUS FLAGS FOR      */~
            f1%(24)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.00.00 09/08/87 Standard costs to 12            "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! GLDTABLE ! G/L Account Distribution Tables          *~
            * # 2 ! GLMAIN   ! GENERAL LEDGER MAIN FILE                 *~
            * # 3 ! JOBMASTR ! WIP/JC MASTER FILE                       *~
            * # 4 ! JBMASTR2 ! Production job master file               *~
            *************************************************************

            select  #1,  "GLDTABLE",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 100,                                  ~
                         keypos =   1, keylen = 15

            select  #2, "GLMAIN",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 9

            select  #3, "JOBMASTR",      /* JOB MASTER FILE            */~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 700,                                   ~
                        keypos = 1, keylen = 8

            select #4 , "JBMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1300,                                 ~
                        keypos =    1, keylen =   8                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, 0%, f2%( 1), 100%, " ")
            call "OPENCHCK" (#2, 0%, f2%( 2), 0%, " ")
            call "OPENCHCK" (#3, 0%, f2%( 3), 0%, " ")
            call "OPENCHCK" (#4, 0%, f2%( 4), 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            lines_allowed% = dim(famt$(),1)

            for i% = 1 to 999
                convert i% to seq$(i%), pic(####)
                str(seq$(i%),5) = ")"
            next i%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            codedescr$, code$, pct$(), errormsg$, famt$(), acct$(),      ~
            descr$(), proj$(), dcflag$(), total$() = " "
            editmode%, maxlines%, line% = 0

L10110:  for fieldnr% = 1 to 2
             gosub'161(fieldnr%)
                   if enabled% = 0 then L10250
L10140:      gosub'201(fieldnr%)
               if keyhit%  =  1 then gosub startover
               if keyhit%  =  3 then gosub copy_table
               if keyhit% <>  4 then L10230
L10180:           fieldnr% = fieldnr% - 1%
                  if fieldnr% < 1% then L10110
                     gosub'161(fieldnr%)
                     if enabled% <> 0 then L10140
                     goto L10180
L10230:        if keyhit%  = 16 and fieldnr% = 1 then L65000
               if keyhit% <>  0 then       L10140
L10250:      gosub'151(fieldnr%)
                   if errormsg$ <> " " then L10140
             next fieldnr%
             goto L11000

        copy_table
            readkey$, errormsg$ = " "
            readkey1$ = hex(06) & "Select Table To Copy..."
            call "PLOWCODE"(#1, readkey$, readkey1$, -6%, -.30, f1%(1))
                if f1%(1) <> 0 then L10370
                   errormsg$ = hex(84) & "Copy Cancelled" : return
L10370:     REM Now Try And Load Distibution Table...
            savecode$ = code$
            savedescr$ = codedescr$
            code$ = readkey$
            gosub L30000
            code$ = savecode$
            if savedescr$ <> " " then codedescr$ = savedescr$
            return clear all
            goto line_summary

L11000: REM *************************************************************~
            *              I N P U T   L I N E   I T E M S              *~
            *                                                           *~
            * INPUTS LINE ITEMS AND TESTS FOR VALIDITY.                 *~
            *************************************************************

L11060:     if maxlines% = lines_allowed% then L11110
                c% = maxlines% + 1%
                line% = max(0, maxlines% - 12)
                gosub inputlines
                if keyhit% <> 16% then L11130
L11110:              gosub columnone
                     goto line_summary
L11130:         maxlines% = maxlines% + 1%
                gosub total_up_details
                goto L11060

        inputlines
            linemode% = 0
            gosub columnone
            if maxlines% = 0 and editmode% = 0 then                      ~
                errormsg$ = hex(84) & "Enter The Program List..."
L11220:     for fieldnr% = 1 to 5
                gosub'163(fieldnr%)
                      if enabled% = 0 then L11350
L11250:         gosub'203(fieldnr%)
                     if keyhit% <>  4 then L11320
L11270:                 fieldnr% = fieldnr% - 1%
                        if fieldnr% < 1% then L11220
                        gosub'163(fieldnr%)
                        if enabled% <> 0 then L11250
                        goto L11270
L11320:               if keyhit%  = 16 and fieldnr% = 1 then return
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  0 then       L11250
L11350:         gosub'153(fieldnr%)
                      if errormsg$ <> " " then L11250
                next fieldnr%
                return

        REM *************************************************************~
            *               E D I T   L I N E   I T E M S               *~
            *                                                           *~
            * EDITS LINE ITEMS, GOES AND SAVES DATA TO THE FILE, ETC.   *~
            *************************************************************

        line_summary              /* Summary Screen */
        line_cont
            editmode% = 1
            line% = max(0%,min(line%,maxlines%-12%))
            errormsg$=" "
L12110:     message$ =   "To Modify a Line Item, Position Cursor and" &  ~
                         " press RETURN."
L12130:     gosub'115(0%)
              errormsg$ = " "
              if keyhit% =   1 then gosub startover
              if keyhit% =   2 then line% = 0%
              if keyhit% =   3 then line% = maxlines%
              if keyhit% =   4 then line% = line% - 11
              if keyhit% =   5 then line% = line% + 11
              if keyhit% =   6 then line% = line% - 1
              if keyhit% =   7 then line% = line% + 1
              line% = max(0%,min(line%,maxlines%-12%))
              if keyhit%  = 16 then       datasave
              if keyhit% <> 28 then L12270
                 fieldnr% = 0
                 goto deletemode
L12270:     fieldnr% = cursor%(1) - 7
            if fieldnr% <> -2 then L12310
                fieldnr% = fieldnr% + 4
                goto L12600
L12310:     if keyhit% <> 11 or cursor%(1) > 1 then L12340
                c% = maxlines%
                goto insertmode
L12340:     errormsg$ = hex(84) & "Please Position Cursor First..."
            if keyhit% = 0 then errormsg$ = " "
            if fieldnr% < 0 or fieldnr% > 12 then L12130
            if fieldnr% = 0 and keyhit% = 0 then fieldnr% = 1
            if fieldnr% = 0 and keyhit% <> 11 then L12130
                c% = min(line% + fieldnr%, maxlines%)
                if c% = 0 and keyhit% <> 11 then L12130
                fieldnr% = c% - line%
            if keyhit% = 12 then deletemode
            if keyhit% = 11 then insertmode
            if keyhit% <> 0 then line_cont
            linemode%, fieldnr% = 1
            if cursor%(2) > 48 then fieldnr% = 2
            if cursor%(2) > 52 then fieldnr% = 3
            if cursor%(2) > 63 then fieldnr% = 4
            if cursor%(2) > 71 then fieldnr% = 5

            gosub'163(fieldnr%)
                if enabled% = 0% then line_cont
L12530:     gosub'113(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12530
            gosub'153(fieldnr%)
                  if errormsg$ <> " " then L12530
            goto line_summary

L12600:     gosub'161(fieldnr%)
                if enabled% = 0% then line_cont
L12620:     gosub'101(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12620
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L12620
            goto line_cont

        total_up_details
            tf1, tp1, tf2, tp2 = 0
            if maxlines% < 1 then L12850
            for i% = 1 to maxlines%
                temp, temp1 = 0
                if dcflag$(i%) = "C" then L12800
                convert famt$(i%) to temp, data goto L12760
L12760:         convert pct$(i%) to temp1, data goto L12770
L12770:         tf1 = tf1 + temp
                tp1 = tp1 + temp1
                goto L12840
L12800:         convert famt$(i%) to temp, data goto L12810
L12810:         convert pct$(i%) to temp1, data goto L12820
L12820:         tf2 = tf2 + temp
                tp2 = tp2 + temp1
L12840:     next i%
L12850:     call "CONVERT" (tf1, 2.2, total$(1,1))
            call "CONVERT" (tf2, 2.2, total$(2,1))
            call "CONVERT" (tp1, 0.2, str(total$(1,2),,8))
            call "CONVERT" (tp2, 0.2, str(total$(2,2),,8))
            str(total$(1,2),9) = "%"
            str(total$(2,2),9) = "%"
        return

        REM *************************************************************~
            *       I N S E R T   &   D E L E T E   L O G I C           *~
            *                                                           *~
            * INSERT & DELETE CODE RESIDES HERE.                        *~
            *************************************************************

        insertmode
            if maxlines% = lines_allowed% then line_summary

            REM Copy all Elements Up One...
            maxlines%=maxlines%+1%
            c% = c% + 1
            if c%=maxlines% then L13180
                roll% = -1
                for temp% = maxlines% to c% step -1
                     gosub roll_lines
                next temp%

L13180:     REM Get Line Item Data...
            if c% > line% + 12 then line% = max(0, c% - 12)
            gosub inputlines
            if keyhit% = 16% then delete_line
            gosub total_up_details
        goto insertmode

        deletemode
            if maxlines% = 0 then line_cont
            message$ = "To DELETE Flashing Data, press RETURN, To Return ~
        ~without Delete, press PF1."
            gosub'125(fieldnr%)
                  if keyhit% = 1 then line_cont
                  if keyhit%<>0 then deletemode
            if fieldnr% <> 0 then delete_line
            for c% = 1 to maxlines%  /* Kill'em All */
                gosub columnone
            next c%
            maxlines% = 0
            goto L13410

        delete_line
            gosub delete_it
L13410:     gosub total_up_details
            goto line_summary

        delete_it
            if c%=maxlines% then L13510
                roll% = 1
                for temp% = c% to maxlines%-1%
                     gosub roll_lines
                next temp%
                c%=maxlines%
L13510:     gosub columnone
            maxlines%=maxlines%-1%
        return

        roll_lines
                famt$     (temp%) = famt$     (temp%+roll%)
                acct$     (temp%) = acct$     (temp%+roll%)
                descr$    (temp%) = descr$    (temp%+roll%)
                dcflag$   (temp%) = dcflag$   (temp%+roll%)
                pct$      (temp%) = pct$      (temp%+roll%)
                proj$     (temp%) = proj$     (temp%+roll%)
        return

        columnone
            famt$(c%), acct$(c%), descr$(c%), dcflag$(c%), errormsg$,    ~
            pct$(c%), proj$(c%) = " "
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub total_up_details  /* Just to be sure... */
            if total$(1,2) = "     100%" then L19140
            if total$(1,2) = "       0%" then L19140
            if total$(2,2) = "     100%" then L19140
            if total$(2,2) = "       0%" then L19140
                errormsg$="Percentage Totals Must Be Either Zero or 100"
                goto L12110
L19140:     readkey$ = code$
            call "DELETE" (#1, readkey$, 6%)
            gosub L31000                  /* WRITE RECORDS              */
            lastcode$ = code$
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'161(fieldnr%)
                  message$ = " "
                  enabled% = 1
                  on fieldnr% gosub L20120,         /* Distribution Code*/~
                                    L20160          /* Distribution Desc*/
                  return
L20120:     REM DEFAULT/ENABLE FOR Distribution Code
                message$ = "Enter Distribution Code To Input or Modify, B~
        ~lank To Search File."
                return
L20160:     REM DEFAULT/ENABLE FOR Distribution Description
                message$ = "Enter Distribution Code Description."
                return

        REM *************************************************************~
            *      D E F A U L T / E N A B L E   F O R   T A B L E      *~
            *                                                           *~
            * DEFAULTS/ENABLES FOR TABULAR INPUT. EVERY FIELD ENABLED,  *~
            * FIELD DESCRIPTIONS SET HERE...                            *~
            *************************************************************

            deffn'163(fieldnr%)
                  enabled% = 1
                  message$ = " "
                  on fieldnr% gosub L21150,         /* Account Number   */~
                                    L21180,         /* Debit/Credit Flag*/~
                                    L21230,         /* Flat Amount      */~
                                    L21280,         /* Percentage       */~
                                    L21340          /* Project Code     */
                     return
L21150:     REM DEFAULT/ENABLE FOR Account Number
                message$ = "Enter Account To Distribute Amount To."
                return
L21180:     REM DEFAULT/ENABLE FOR Debit/Credit Flag
                message$ = "Enter 'D' To Debit This Account, 'C' To Credi~
        ~t It."
                if c%>1 and dcflag$(c%)=" "then dcflag$(c%)=dcflag$(c%-1)
                return
L21230:     REM DEFAULT/ENABLE FOR Flat Amount
                message$ = "Enter Base Amount To Apply To This Account, I~
        ~f Any."
                call "STRING" addr("LJ", famt$(c%), 9%)
                return
L21280:     REM DEFAULT/ENABLE FOR Percentage
                message$ = "Enter Percentage Of Remainder To Apply To Thi~
        ~s Account, If Any."
                call "STRING" addr("LJ", pct$(c%), 7%)
                return

L21340:     REM DEFAULT/ENABLE FOR Percentage
                message$ = "Enter Project Code To Be Posted."
                if c% > 1 and proj$(c%) = " "then proj$(c%) = proj$(c%-1)
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

L30000: REM *************************************************************~
            *        L O A D   O L D   A C C O U N T   L I S T          *~
            *                                                           *~
            * READS IN TABLE OFF OF FILE.                               *~
            *************************************************************

            maxlines%, c% = 0%
            readkey$ = code$
            str(readkey$,7) = all(hex(00))
L30090:     call "PLOWNEXT" (#1, readkey$, 6%, f1%(1))
                 if f1%(1) = 0 then L30230
            if c% = 0 then print at(3,1,80);hex(84); "Recalling Table..."
            c%, maxlines% = c% + 1
            get #1, using L31200, code$, acct$(c%), codedescr$,           ~
                                 dcflag$(c%), temp, temp1, proj$(c%)
            FMT CH(8)
            descr$(c%) = hex(94) & "Not On File"
            call "DESCRIBE" (#2, acct$(c%), descr$(c%), 0%, f1%(2))
            call "GLFMT" (acct$(c%))
            call "CONVERT" (temp, 2.2, famt$(c%))
            call "CONVERT" (temp1, 0.2, pct$(c%))
            goto L30090

L30230:     gosub total_up_details
            return

L31000: REM *************************************************************~
            *            W R I T E   A C C O U N T   L I S T            *~
            *                                                           *~
            * DELETES ALL THE OLD ACCOUNT ENTRIES FOR THIS TABLE        *~
            * AND WRITES NEW ONES TO THE FILE.                          *~
            *************************************************************

            if maxlines% = 0 then return
            call "SHOSTAT"("Saving Distribution Table")

            for c% = 1 to maxlines%
                temp, temp1 = 0
                convert famt$(c%) to temp, data goto L31130
L31130:         convert pct$(c%) to temp1, data goto L31140
L31140:         call "GLUNFMT" (acct$(c%))
                write #1, using L31200, code$, acct$(c%), codedescr$,     ~
                          dcflag$(c%), temp, temp1, proj$(c%), " "
            next c%
            return

L31200:     FMT CH(06),                  /* Table Name                 */~
                CH(09),                  /* Account Number             */~
                CH(30),                  /* Table Description          */~
                CH(01),                  /* Debit/Credit Flag          */~
                PD(14,4),                /* Flat Amount                */~
                PD(14,4),                /* Percentage                 */~
                CH(08),                  /* Project Code               */~
                CH(30)                   /* Filler                     */

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'201(fieldnr%)
                init(hex(84)) lfac$()
                header$ = " "
                pfktext$(1) = "(1)Start Over     (4)Previous Field       ~
        ~                     (13)Instructions"
                pfktext$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfktext$(3) = "                                          ~
        ~                     (16)Exit Program"
                pfkeys$ = hex(000104ff0d0f10)

                if fieldnr% > 1% then L40240
                  str(pfktext$(1),,62) = " "    /* Shut Off Start Over */
                  str(pfktext$(3),,62) = " "    /* Shut Off More       */
                  str(pfkeys$,2,3) = hex(ffffff)
                  header$ = "Last Code Managed: " & lastcode$
                  if lastcode$ = " " then header$ = " "
                  goto L40300
L40240:         if fieldnr% > 2% then L40270
                  str(pfktext$(2),18,20)=hex(84)&"(3)Copy Table"&hex(8c)
                  pfkeys$ = pfkeys$ & hex(03)
L40270:        str(pfktext$(3),64) = " "        /* Shut Off Exit       */
               str(pfkeys$,7,1) = hex(ff)
               header$ = " "
L40300:        init(hex(9c)) qfac$(), tfac$(), dfac$()
               goto L40430

            deffn'101(fieldnr%)   /* Edit Header */
                init(hex(84)) lfac$()
                header$ = " "
                pfktext$(1) = "(1)Start Over                             ~
        ~                     (13)Instructions"
                pfktext$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfktext$(3), header$ = " "
                pfkeys$ = hex(00010d0f10)

L40430:           on fieldnr% gosub L40500,         /* Distribution Code*/~
                                    L40470          /* Distribution Desc*/
                     goto L41320

L40470:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40500:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return

            deffn'203(fieldnr%)          /* Input Mode Lines */
                init(hex(8c)) qfac$(), tfac$(), dfac$()
                init(hex(84)) lfac$()
                pfktext$(1) = "(1)Start Over     (4)Previous Field     "&~
                              "                       (13)Instructions"
                pfktext$(2) = "                                        "&~
                              "                       (15)Print Screen"
                pfktext$(3) = "                                        "&~
                              "                       (16)Edit Mode"
                pfkeys$ = hex(0001040d0f10)
                if fieldnr% > 1% then L40670
                     str(pfktext$(1),19,20) = " "
                     str(pfkeys$,3,1) = hex(ff)
L40670:         if c% > line%+13 then line% = c%-13
                goto L40800

            deffn'113(fieldnr%)          /* Edit Mode Lines */
                init(hex(8c)) qfac$(), tfac$(), dfac$()
                init(hex(84)) lfac$()
                pfktext$(1) = "(1)Start Over                           "&~
                              "                       (13)Instructions"
                pfktext$(2) = "                                        "&~
                              "                       (15)Print Screen"
                pfktext$(3) = " "
                pfkeys$ = hex(00010d0f)

L40800:         REM Set Modifiable FAC...
                header$ = " "
                on fieldnr% gosub L40890,         /* Account Number     */~
                                  L40890,         /* Debit/Credit Flag  */~
                                  L40890,         /* Flat Amount        */~
                                  L40890,         /* Percentage         */~
                                  L40890          /* Project Code       */
                     goto L41320

L40890:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      qfac$(c%-line%, fieldnr%) = hex(81)
                      return

            deffn'115(fieldnr%)
                init (hex(8c)) tfac$() : init (hex(86)) qfac$()
                init (hex(8c)) dfac$()
                init (hex(84)) lfac$()
                pfktext$(1) = "(1)Start Over                           (1~
        ~1)Insert             (13)Instructions"
                pfktext$(2) = "(2)First Line  (4)Prev  (6)Down One     (1~
        ~2)Delete Line        (15)Print Screen"
                pfktext$(3) = "(3)Last Lines  (5)Next  (7)Up One       (2~
        ~8)Delete All         (16)Save Data"
                pfkeys$ = hex(0001020406030507ffff0b0c0d0f10ff1c)
                header$ = " "

                REM Turn Off Appropriate Fields
                if maxlines% > 0 then L41110
                  str(pfktext$(2),41,18) = " " /* Shut Off Delete */
                  str(pfktext$(3),41,22) = " " /* Shut Off Delete All */
                  str(pfkeys$,12,1), str(pfkeys$,17,1) = hex(ff)
L41110:         if line% > 0 then L41140
                  str(pfktext$(2),,36)   = " "  /* Shut Off Prev Stuff */
                  str(pfkeys$,3,3) = hex(ffffff)
L41140:         if line%+12 < maxlines% then L41320
                  str(pfktext$(3),,36)   = " "  /* Shut Off Prev Stuff */
                  str(pfkeys$,6,3) = hex(ffffff)
                  goto L41320

            deffn'125(d%)  /* Delete Lines From Memory */
                init (hex(8c)) qfac$(), tfac$(), dfac$()
                pfktext$(1) = "(1)Cancel Delete Request                  ~
        ~                     (13)Instructions"
                pfktext$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfktext$(3) = "(ENTER) Delete Line"
                pfkeys$ = hex(00010d0f10)
                if d% = 0% then init(hex(94)) qfac$(), tfac$(), dfac$()  ~
                    else qfac$(d%,1),qfac$(d%,2),qfac$(d%,3),qfac$(d%,4),~
                         qfac$(d%,5), tfac$(d%), dfac$(d%) = hex(94)
                header$ = " "

L41320:         header1$ = "Seq.  Account      Description               ~
        ~DB/CR  Flat Amt  Percent Project"
                str(header$,62) = "GLDISTIN: " & cms2v$
                str(tfac$(), min(20, (maxlines%-line%)+1)) = all(hex(9c))
                str(pfktext$(3),63,1) = hex(84)

L41380:     accept                                                       ~
               at (01,02), "Manage G/L Auto Distribution Tables",        ~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Distribution Code",                          ~
               at (04,20), fac(lfac$( 1)), code$                , ch(06),~
               at (05,02), "Description",                                ~
               at (05,14), fac(lfac$( 2)), codedescr$           , ch(30),~
                                                                         ~
               at (04,51), "DB Totals:",                                 ~
               at (04,62), fac(hex(84)), total$(1,1)            , ch(09),~
               at (04,72), fac(hex(84)), total$(1,2)            , ch(09),~
               at (05,51), "CR Totals:",                                 ~
               at (05,62), fac(hex(84)), total$(2,1)            , ch(09),~
               at (05,72), fac(hex(84)), total$(2,2)            , ch(09),~
                                                                         ~
               at (07,02), fac(hex(ac)), header1$               , ch(04),~
               at (07,08), fac(hex(ac)), str(header1$,7)        , ch(73),~
                                                                         ~
               at (08,02), fac(tfac$( 1)), seq$   (line%+ 1%)   , ch(05),~
               at (09,02), fac(tfac$( 2)), seq$   (line%+ 2%)   , ch(05),~
               at (10,02), fac(tfac$( 3)), seq$   (line%+ 3%)   , ch(05),~
               at (11,02), fac(tfac$( 4)), seq$   (line%+ 4%)   , ch(05),~
               at (12,02), fac(tfac$( 5)), seq$   (line%+ 5%)   , ch(05),~
               at (13,02), fac(tfac$( 6)), seq$   (line%+ 6%)   , ch(05),~
               at (14,02), fac(tfac$( 7)), seq$   (line%+ 7%)   , ch(05),~
               at (15,02), fac(tfac$( 8)), seq$   (line%+ 8%)   , ch(05),~
               at (16,02), fac(tfac$( 9)), seq$   (line%+ 9%)   , ch(05),~
               at (17,02), fac(tfac$(10)), seq$   (line%+10%)   , ch(05),~
               at (18,02), fac(tfac$(11)), seq$   (line%+11%)   , ch(05),~
               at (19,02), fac(tfac$(12)), seq$   (line%+12%)   , ch(05),~
                                                                         ~
               at (08,08), fac(qfac$( 1,1)), acct$ (line%+ 1%)  , ch(12),~
               at (09,08), fac(qfac$( 2,1)), acct$ (line%+ 2%)  , ch(12),~
               at (10,08), fac(qfac$( 3,1)), acct$ (line%+ 3%)  , ch(12),~
               at (11,08), fac(qfac$( 4,1)), acct$ (line%+ 4%)  , ch(12),~
               at (12,08), fac(qfac$( 5,1)), acct$ (line%+ 5%)  , ch(12),~
               at (13,08), fac(qfac$( 6,1)), acct$ (line%+ 6%)  , ch(12),~
               at (14,08), fac(qfac$( 7,1)), acct$ (line%+ 7%)  , ch(12),~
               at (15,08), fac(qfac$( 8,1)), acct$ (line%+ 8%)  , ch(12),~
               at (16,08), fac(qfac$( 9,1)), acct$ (line%+ 9%)  , ch(12),~
               at (17,08), fac(qfac$(10,1)), acct$ (line%+10%)  , ch(12),~
               at (18,08), fac(qfac$(11,1)), acct$ (line%+11%)  , ch(12),~
               at (19,08), fac(qfac$(12,1)), acct$ (line%+12%)  , ch(12),~
                                                                         ~
               at (08,21), fac(dfac$( 1)), descr$ (line%+ 1%)   , ch(27),~
               at (09,21), fac(dfac$( 2)), descr$ (line%+ 2%)   , ch(27),~
               at (10,21), fac(dfac$( 3)), descr$ (line%+ 3%)   , ch(27),~
               at (11,21), fac(dfac$( 4)), descr$ (line%+ 4%)   , ch(27),~
               at (12,21), fac(dfac$( 5)), descr$ (line%+ 5%)   , ch(27),~
               at (13,21), fac(dfac$( 6)), descr$ (line%+ 6%)   , ch(27),~
               at (14,21), fac(dfac$( 7)), descr$ (line%+ 7%)   , ch(27),~
               at (15,21), fac(dfac$( 8)), descr$ (line%+ 8%)   , ch(27),~
               at (16,21), fac(dfac$( 9)), descr$ (line%+ 9%)   , ch(27),~
               at (17,21), fac(dfac$(10)), descr$ (line%+10%)   , ch(27),~
               at (18,21), fac(dfac$(11)), descr$ (line%+11%)   , ch(27),~
               at (19,21), fac(dfac$(12)), descr$ (line%+12%)   , ch(27),~
                                                                         ~
               at (08,49), fac(qfac$( 1,2)), dcflag$(line%+ 1)  , ch(01),~
               at (09,49), fac(qfac$( 2,2)), dcflag$(line%+ 2)  , ch(01),~
               at (10,49), fac(qfac$( 3,2)), dcflag$(line%+ 3)  , ch(01),~
               at (11,49), fac(qfac$( 4,2)), dcflag$(line%+ 4)  , ch(01),~
               at (12,49), fac(qfac$( 5,2)), dcflag$(line%+ 5)  , ch(01),~
               at (13,49), fac(qfac$( 6,2)), dcflag$(line%+ 6)  , ch(01),~
               at (14,49), fac(qfac$( 7,2)), dcflag$(line%+ 7)  , ch(01),~
               at (15,49), fac(qfac$( 8,2)), dcflag$(line%+ 8)  , ch(01),~
               at (16,49), fac(qfac$( 9,2)), dcflag$(line%+ 9)  , ch(01),~
               at (17,49), fac(qfac$(10,2)), dcflag$(line%+10)  , ch(01),~
               at (18,49), fac(qfac$(11,2)), dcflag$(line%+11)  , ch(01),~
               at (19,49), fac(qfac$(12,2)), dcflag$(line%+12)  , ch(01),~
                                                                         ~
               at (08,53), fac(qfac$( 1,3)), famt$  (line%+ 1%) , ch(09),~
               at (09,53), fac(qfac$( 2,3)), famt$  (line%+ 2%) , ch(09),~
               at (10,53), fac(qfac$( 3,3)), famt$  (line%+ 3%) , ch(09),~
               at (11,53), fac(qfac$( 4,3)), famt$  (line%+ 4%) , ch(09),~
               at (12,53), fac(qfac$( 5,3)), famt$  (line%+ 5%) , ch(09),~
               at (13,53), fac(qfac$( 6,3)), famt$  (line%+ 6%) , ch(09),~
               at (14,53), fac(qfac$( 7,3)), famt$  (line%+ 7%) , ch(09),~
               at (15,53), fac(qfac$( 8,3)), famt$  (line%+ 8%) , ch(09),~
               at (16,53), fac(qfac$( 9,3)), famt$  (line%+ 9%) , ch(09),~
               at (17,53), fac(qfac$(10,3)), famt$  (line%+10%) , ch(09),~
               at (18,53), fac(qfac$(11,3)), famt$  (line%+11%) , ch(09),~
               at (19,53), fac(qfac$(12,3)), famt$  (line%+12%) , ch(09),~
                                                                         ~
               at (08,64), fac(qfac$( 1,4)), pct$(line%+ 1)     , ch(07),~
               at (09,64), fac(qfac$( 2,4)), pct$(line%+ 2)     , ch(07),~
               at (10,64), fac(qfac$( 3,4)), pct$(line%+ 3)     , ch(07),~
               at (11,64), fac(qfac$( 4,4)), pct$(line%+ 4)     , ch(07),~
               at (12,64), fac(qfac$( 5,4)), pct$(line%+ 5)     , ch(07),~
               at (13,64), fac(qfac$( 6,4)), pct$(line%+ 6)     , ch(07),~
               at (14,64), fac(qfac$( 7,4)), pct$(line%+ 7)     , ch(07),~
               at (15,64), fac(qfac$( 8,4)), pct$(line%+ 8)     , ch(07),~
               at (16,64), fac(qfac$( 9,4)), pct$(line%+ 9)     , ch(07),~
               at (17,64), fac(qfac$(10,4)), pct$(line%+10)     , ch(07),~
               at (18,64), fac(qfac$(11,4)), pct$(line%+11)     , ch(07),~
               at (19,64), fac(qfac$(12,4)), pct$(line%+12)     , ch(07),~
                                                                         ~
               at (08,72), fac(qfac$( 1,5)), proj$(line%+ 1)    , ch(08),~
               at (09,72), fac(qfac$( 2,5)), proj$(line%+ 2)    , ch(08),~
               at (10,72), fac(qfac$( 3,5)), proj$(line%+ 3)    , ch(08),~
               at (11,72), fac(qfac$( 4,5)), proj$(line%+ 4)    , ch(08),~
               at (12,72), fac(qfac$( 5,5)), proj$(line%+ 5)    , ch(08),~
               at (13,72), fac(qfac$( 6,5)), proj$(line%+ 6)    , ch(08),~
               at (14,72), fac(qfac$( 7,5)), proj$(line%+ 7)    , ch(08),~
               at (15,72), fac(qfac$( 8,5)), proj$(line%+ 8)    , ch(08),~
               at (16,72), fac(qfac$( 9,5)), proj$(line%+ 9)    , ch(08),~
               at (17,72), fac(qfac$(10,5)), proj$(line%+10)    , ch(08),~
               at (18,72), fac(qfac$(11,5)), proj$(line%+11)    , ch(08),~
               at (19,72), fac(qfac$(12,5)), proj$(line%+12)    , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfktext$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfktext$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfktext$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key(keyhit%)

               if keyhit% <> 13 then L42630
                  call "MANUAL" ("GLDISTIN")
                  goto L41380

L42630:        if keyhit% <> 15 then L42670
                  call "PRNTSCRN"
                  goto L41380

L42670:        close ws
               call "SCREEN" addr ("C", 3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50110,         /* Distribution Code*/~
                                    L50220          /* Distribution Desc*/
                     return
L50110:     REM TEST DATA FOR Distribution Code
                if code$ <> " " then L50160
                call "PLOWCODE"(#1,code$,codedescr$,-6%,-.30,f1%(1))
                     if f1%(1) <> 0 then L50160
                     errormsg$ = hex(00) : return
L50160:         REM Now Try And Load Distibution Table...
                gosub L30000
                if maxlines% = 0 then return
                return clear all
                goto line_summary

L50220:     REM TEST DATA FOR Distribution Description
                if codedescr$ = " " then                                 ~
                                 errormsg$ = "Description Can't Be Blank"
                return

        REM *************************************************************~
            *      T E S T   D A T A   F O R   L I N E   I T E M S      *~
            *                                                           *~
            * TESTS DATA FOR THE LINE ITEM INFORMATION, INCLUDING       *~
            * PART NUMBER VALID, AND POSITIVE QUANTITY, SIZE (X USED)   *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51160,         /* Account Number   */~
                                    L51240,         /* Debit/Credit Flag*/~
                                    L51310,         /* Flat Amount      */~
                                    L51370,         /* Percentage       */~
                                    L51430          /* Project Code     */
                     return

L51160:     REM Test Data For Account Number
                call "PLOWCODE" (#2, acct$(c%), descr$(c%),0%,.30,f1%(2))
                     if f1%(2) <> 0 then L51210
                        errormsg$ = "Account Not On File: " & acct$(c%)
                        return
L51210:         gosub test_for_dup
                return

L51240:     REM Test Data For Debit/Credit Flag
                if pos("DC" = dcflag$(c%)) > 0 then L51280
                     errormsg$ = "Please Enter 'D' or 'C'"
                     return
L51280:         if  linemode% <> 0 then gosub total_up_details
                return

L51310:     REM Test Data For Flat Amount
                call "NUMTEST" (famt$(c%), 0, 9e7, errormsg$, -2.2, 0)
                     if errormsg$ <> " " then return
                if  linemode% <> 0 then gosub total_up_details
                return

L51370:     REM Test Data For Percentage
                call "NUMTEST" (pct$(c%), 0, 100, errormsg$, -0.2, 0)
                     if errormsg$ <> " " then return
                if  linemode% <> 0 then gosub total_up_details
                return

L51430:     REM Test Data For Project Code
                if proj$(c%) = " " then return
                call "PLOWCODE" (#3, proj$(c%), " ", 0%, .30, f1%(3))
            REM May not also be a Job number
                call "READ100" (#4, proj$(c%), f1%(4))
                if f1%(4) = 0% then return
                     errormsg$ = "Project code may not also be a JOB co"&~
                          "de.  Try again."
                     return

        test_for_dup
                mat cursor% = zer
                search acct$() = str(acct$(c%)) to cursor%() step 12
                     if cursor%(2) = 0 then return
                     errormsg$ = "Account Is Already On This List"
                     return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end
