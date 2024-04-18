        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L       CCC   H   H  K  K   RRRR    GGG    *~
            *  P   P  R   R  L      C   C  H   H  K K    R   R  G   G   *~
            *  PPPP   RRRR   L      C      HHHHH  KK     RRRR   G       *~
            *  P      R   R  L      C   C  H   H  K  K   R   R  G  GG   *~
            *  P      R   R  LLLLL   CCC   H   H  K   K  R   R   GGG    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLCHKRG - REPRINTS THE PAYROLL JOURNAL FOR A RANGE OF    *~
            *            DATES.                                         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/16/81 ! ORIGINAL                                 ! BCW *~
            * 12/12/85 ! Print Earnings Register by Check or emply! RAC *~
            * 06/23/86 ! CORRECTED MISC BUG                       ! SGA *~
            * 08/13/86 ! CORRECTED MORE MISC BUGS & STANDARDS     ! KAB *~
            * 10/13/86 ! Compatable With Direct Deposit Slips     ! HES *~
            * 04/01/91 ! Corrected Search Bug (PPR # 11779)       ! RJB *~
            * 06/27/91 !QC-FIXES Corrected cursor/fieldnr setting ! RJB *~
            *          !   in 'EDIT' section.                     !     *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 01/12/93 ! Page 0 Facs fix                          ! RJH *~
            * 04/19/93 !MISC - Removed detail page heading from   ! MLJ *~
            *          ! page 0.  Dim'd some variables, fixed     !     *~
            *          ! some implied integers.                   !     *~
            *          !PRR 8078 & 12635 - Corrected deduction    !     *~
            *          ! distribution on employee/employer when   !     *~
            *          ! both have same description.              !     *~
            * 08/26/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            check$8,                     /* CHECK NUMBER               */~
            company$60,                  /* COMPANY NAME               */~
            dedcat$6,                    /* DEDUCTION CATEGORY INFO    */~
            dedempflag$1,                /* DEDUCTION EMPLOYEE PAYS?   */~
            deduction$18,                /* DEDUCTION DESCRIPTION      */~
            dedxnstak$(100)21,           /* DEDUCTION DESCRIPTION STACK*/~
            dedxnstak(100,3),            /* DEDUCTION AMOUNT STACK     */~
            dtotstak$(100)21,            /* DEDUCTION DESCRIPTION STACK*/~
            dtotstak(100,3),             /* DEDUCTION AMOUNT STACK     */~
            desc1$32,                    /* PRINT DATE RANGE FIELD     */~
            desc2$44,                    /* PRINT EMPLOYEE RANGE FIELD */~
            dept$4,                      /* EARNINGS DEPARTMENT CODE   */~
            department$4,                /* EMP. MASTER DEPARTMENT CODE*/~
            dpt$4,                       /* DEPARTMENT CODE            */~
            dstack$(999)20,              /* DEDUCTION STACK            */~
            dstak(999),                  /* DITTO WITH NUMBERS         */~
            earn(2),                     /* AMOUNTS THIS EARNINGS REC  */~
            earnstak$(100)12,            /* EARNINGS TYPE STACK        */~
            earnstak(100,2),             /* EARNINGS UNITS STACK       */~
            etotstak$(100)12,            /* EARNINGS TYPE STACK        */~
            etotstak(100,2),             /* EARNINGS UNITS STACK       */~
            empcode$12,                  /* EMPLOYEE CODE INFO         */~
            emprstack$(999)20,           /* EMPLOYERS SHARE STACK      */~
            emprstak(999),               /* DITTO WITH NUMBERS         */~
            estack$(999)20,              /* EARNINGS STACK             */~
            estak1(999),                 /* DITTO WITH NUMBERS         */~
            estak2(999),                 /* DITTO     DITTO            */~
            etype$12,                    /* EARNINGS TYPE DESCRIPTION  */~
            firstpaydate$8,              /* FIRST DATE PAY PERIOD      */~
            lastpaydate$8,               /* LAST DATE THIS PAY PERIOD  */~
            line2$79,                    /* Line two for screen        */~
            linenumber%(6),              /* LINE POINTERS FOR PRINT    */~
            loc$2,                       /* LOCATOR ARRAY FOR MATSEARCH*/~
            location$2,                  /* LOCATOR ARRAY FOR MATSEARCH*/~
            name$(3)20,                  /* EMPLOYEE'S 3 NAMES         */~
            mainkey$50,                  /* READ KEY FOR FILE INFO     */~
            pointer%(2),                 /* POINTERS FOR STACK PRINTS  */~
            postdate$8,                  /* DATE CHECK POSTED          */~
            print$(11)20,                /* PRINT COLUMN ENTRIES       */~
            readkey$50,                  /* READ KEY FOR FILE INFO     */~
            rptdate$8,                   /* REPORT DATE                */~
            rptttle$60,                  /* REPORT TITLE               */~
            rpttime$8,                   /* REPORT TIME                */~
            scrnfrom$12,                 /* SCREEN FROM PROMPT         */~
            scrnto$12,                   /* SCREEN TO PROMPT           */~
            ssn$11,                      /* SOCIAL SECURITY NUMBER     */~
            string$50,                   /* SEARCH STRING IN STACK     */~
            temp$21,                     /* TEMPORARY WORK AREA        */~
            type$12                      /* EARNINGS TYPE DESCRIPTION  */

        dim                                                              ~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            firstdate$10,                /* First Date                 */~
            firstemployee$12,            /* First Employee             */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            lastdate$10,                 /* last Date                  */~
            lastemployee$12,             /* Last Employee              */~
            lfac$(20)1                   /* FIELD ATTRIBUTE CHARACTERS */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            fs%(64),                                                     ~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20                  /* RETURN CODE FROM "FILEOPEN"*/~

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
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! SYSTEM CONTROL FILE (CHECK # AND DATE    *~
            * # 3 ! EMPMASTR ! EMPLOYEE FILE MASTER RECORDS.            *~
            * # 6 ! PRLCHK   ! PAYROLL CHECK FILE                       *~
            * # 7 ! PRLCHK2  ! PAYROLL CHECK LINE ITEM FILE             *~
            * #14 ! PERMASTR ! PERSONNEL MASTER FILE                    *~
            *************************************************************

            select # 1, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select # 3, "EMPMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 136,                                  ~
                         keypos = 1, keylen = 12,                        ~
                         alt key  1, keypos = 70, keylen =  1, dup

            select # 6, "PRLCHK",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 120,                                   ~
                        keypos =   1, keylen = 23,                       ~
                        alt key  1, keypos =  13, keylen =  11,          ~
                            key  2, keypos =  42, keylen =   9, dup

            select # 7, "PRLCHK2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 120,                                   ~
                        keypos =   1, keylen = 25

            select #14, "PERMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 950,                                   ~
                        keypos = 39, keylen = 12,                        ~
                        alt key  1, keypos =  28, keylen = 23,           ~
                            key  2, keypos =   2, keylen = 49,           ~
                            key  3, keypos =   1, keylen = 50


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6), 0%, rslt$( 6))
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7), 0%, rslt$( 7))
            call "OPENCHCK" (#14, fs%(14), f2%(14), 0%, rslt$(14))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * INITIALIZES NECESSARY VARIABLES.  ALSO DOES GETPARM FOR   *~
            * FIGURING OUT WHETHER THIS IS A PROOF PAYROLL JOURNAL OR   *~
            * THE "REAL THING."                                         *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Press RETURN for F~
        ~ull Screen Edit."

            str(line2$,61,9) = "PRLCHKRG:"
            str(line2$,71,8) = str(cms2v$,1,8)

            scrnfrom$ = "From:"
            scrnto$   = "To:"

            call "COMPNAME" (12%, company$, f1%(1))

        REM *************************************************************~
            *                  M A I N   P R O G R A M                  *~
            *-----------------------------------------------------------*~
            * GET RANGE TO PRINT.                                       *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, firstemployee$, firstdate$,~
                      lastemployee$, lastdate$, level$
            pf16$ = "(16)EXIT PROGRAM"
            for fieldnr% = 1 to  3
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10210
L10150:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10150
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10150
L10210:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

L10281:     inpmessage$ = edtmessage$
            pf16$ = "(16)PRINT REPORT"
L10290:     gosub'101(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       print_report
                  if keyhit% <>  0 then       L10290
            fieldnr% = cursor%(1) - 5
            if fieldnr% > 3 then L10281
            if fieldnr% < 1 then fieldnr% = 4%
            gosub'051(fieldnr%)
              if enabled% = 0% then L10281
L10360:     gosub'101(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L10360
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L10360
            goto L10281

        REM *************************************************************~
            *                  P R I N T   R E P O R T                  *~
            *                                                           *~
            * PLOWS DOWN EMPLOYEE LIST IN EMPLOYEE CODE ORDER AND GOSUBS*~
            * TO THE PROCESSING ROUTINE FOR THOSE WITH THE EARNINGS FLAG*~
            * SET.                                                      *~
            *************************************************************

        print_report
            call "SHOSTAT" ("Printing Report")
                   if firstdate$ = "ALL" then desc1$ = "DATE RANGE: ALL" ~
                     else desc1$ = "DATE RANGE: " & firstdate$ & " TO " &~
                     lastdate$
                   if firstemployee$ = "ALL" then desc2$ = "EMPLOYEE RANG~
        ~E : ALL" else desc2$ = "EMPLOYEE RANGE: " & firstemployee$ & " TO~
        ~ " & lastemployee$
                call "STRING" addr("RJ", desc2$, 44%)
                if firstdate$ <> "ALL" then L11200
                firstdate$ = "19000101"
                lastdate$  = "20991231"
                call "DATECONV" (firstdate$,firstdate%)
                call "DATECONV" (lastdate$, lastdate%)
L11200:         if firstemployee$ <> "ALL" then L11260
                init(hex(00)) firstemployee$
                init(hex(ff)) lastemployee$
                goto L11270
L11260:         firstemployee$ = add(hex(ff))
L11270:         line% = 1000
                page% = 0
                rpttime$, rptdate$ = " "
                rptdate$ = date : call "DATEFMT" (rptdate$)
                call "TIME" (rpttime$)
                init(hex(00)) empcode$
                str(empcode$,1,12) = str(firstemployee$)

            REM Initialize stacks
                init(hex(ff)) emprstack$(), estack$(), dstack$()
                mat estak1 = zer
                mat estak2 = zer
                mat dstak  = zer
                eptr%, dptr%, emprptr% = 0

                select printer(134)
                call "SETPRNT" ("PRL010", " ", 0%, 0%)

L11450:     call "PLOWNEXT" (#3, empcode$, 0%, f1%(3))
                 if f1%(3) = 0 then L19000
                 if empcode$ > lastemployee$ then L19000
            mainkey$ = empcode$
            totflag% = 0%
                init(hex(ff)) etotstak$(), dtotstak$()
                mat etotstak = zer
                mat dtotstak = zer
                etotptr%, dtotptr% = 0

L11550:     call "PLOWNEXT" (#6, mainkey$, 12%, f1%(6))
                 if f1%(6) = 0 then L12010
            str(mainkey$,21,3) = all(hex(ff))
            call "READ100" (#14, str(mainkey$,,12), f1%(14))
            gosub L30000                  /* EMPLOYEE MASTER RECORDS    */
            call "DATECONV" (postdate$,postdate%)
            if postdate% < firstdate% then L11550
            if postdate% > lastdate% then L11550
            REM If this is a deposit stub & check also exists, skip since~
                check was already printed (in theory)
            if str(check$,,1) = "D" and other$ <> " " then L11550
            totflag% = totflag% + 1%
               init(hex(ff)) dedxnstak$(), earnstack$()
               mat earnstak = zer
               mat dedxnstak = zer
               earnptr%, dedxnptr% = 0

               REM Now load nonzero earnings records into stack.
                   readkey$ = key(#6)
                   str(readkey$,24) = "P" & hex(00)
                   gross = 0
L11730:            call "PLOWNEXT" (#7, readkey$, 24%, f1%(7))
                        if f1%(7) = 0 then L11830
                   gosub L31000           /* LOAD EARNINGS RECORD       */
                   if earn(2) = 0 then L11730
                      gross = gross + earn(2)
                      gosub'162(type$, earn(1), earn(2))
                      gosub'182(type$, earn(1), earn(2))
                      gosub'172(department$, type$, earn(1), earn(2))
                      goto L11730

L11830:        REM Now load nonzero deductions records into stack.
                   readkey$ = key(#6)
                   str(readkey$,24) = "W" & hex(00)
L11860:            call "PLOWNEXT" (#7, readkey$, 24%, f1%(7))
                        if f1%(7) = 0 then L11970
                   gosub L32000           /* LOAD DEDUCTIONS RECORD     */
                   if dedamt = 0 then L11860
                      if dedempflag$ <> "Y" then                         ~
                         gosub'174(department$, deduction$, dedamt)
                      gosub'163(deduction$, dedamt)
                      gosub'183(deduction$, dedamt)
                      if dedempflag$ <> "Y" then L11860
                         gosub'173(department$, deduction$, dedamt)
                         goto L11860
L11970:        REM Now print the journal entries
                   if level$ = "1" then gosub L20000
                   goto L11550

L12010:     REM Now print the employee totals
            if totflag% = 0% then L11450
            earnptr% = etotptr%
            dedxnptr% = dtotptr%
            mat earnstak$ = etotstak$
            mat earnstak = etotstak
            mat dedxnstak$ = dtotstak$
            mat dedxnstak = dtotstak
            prtflag% = 1%
            gosub L20000
            prtflag% = 0%
            goto L11450
L19000: REM *************************************************************~
            *    P R I N T   D E P A R T M E N T   R E C A P            *~
            *************************************************************

            line% = 1000
            page% = 0
            gosub L44500
            if dptr% + eptr% + emprptr% = 0 then L19292

            if eptr% > 0 then                                            ~
            call "SORT" addr(str(estack$(), 1), eptr%, 20%,              ~
                             str(estack$(), 1), 1%, 16%, "A", "S")

            if dptr% > 0 then                                            ~
            call "SORT" addr(str(dstack$(), 1), dptr%, 20%,              ~
                             str(dstack$(), 1), 1%, 16%, "A", "S")

            if emprptr% > 0 then                                         ~
            call "SORT" addr(str(emprstack$(), 1), emprptr%, 20%,        ~
                             str(emprstack$(), 1), 1%, 16%, "A", "S")
            gosub L40000
            print$(1) = "****"
            print$(2) = "***TOTALS***"

            tot1 = round(tot1, 2%)
            tot2 = round(tot2, 2%)
            tot3 = round(tot3, 2%)
            tot4 = round(tot4, 2%)
            print using L45270, print$(1), print$(2), tot1, tot2,         ~
                               print$(1), print$(2), tot3,               ~
                               print$(1), print$(2), tot4
            print using L45140

L19292:     print skip(2)
            print "                                         ********** E ~
        ~N D   O F   R E P O R T ********** "
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            go to inputmode

L20000: REM *************************************************************~
            *    R E P O R T   P R I N T I N G   S U B R O U T I N E    *~
            *                                                           *~
            * PRINTS THE PAYROLL JOURNAL FOR A SINGLE EMPLOYEE.  NOTE   *~
            * THAT THIS IS THE STANDARD FIVE-COLUMN PRINT ROUTINE.      *~
            *************************************************************

            colsdone% = 0
            mat linenumber% = con
            if dedxnptr% > 0 then                                        ~
            call "SORT" addr(str(dedxnstak$(), 1), dedxnptr%, 21%,       ~
                             str(dedxnstak$(), 1), 1%, 3%, "A", "S")

            REM Loop through computing and printing lines until done.
L20140:         for column% = 1 to 6
                    on column% gosub L21000, L22000, L23000, L24000, L25000,  ~
                                     L26000
                    next column%
                if colsdone% < 6 then L20260
                   REM EXIT ROUTINE FOR FINISHED.
                       if tagprinted% <> 0 then return
                          gosub L28000
                          if tagprinted% <> 0 then return
                             if prtflag% = 0 then print using L28430      ~
                                else print using L28452
                             tagprinted% = 1
                             return
L20260:         gosub L28000              /* Page heading, if neccessary*/
                print using L28460, print$(1), print$(2), print$(3),      ~
                                   print$(4), print$(5), print$(6),      ~
                                   print$(7), print$(8), print$(9),      ~
                                   print$(10), print$(11)
                tagprinted% = 0
                goto L20140

L21000:     REM First  column--employee code, etc.
                on linenumber%(1) gosub L21100, L21200, L21300, L21400,      ~
                                        L21500, L21600,        L21800
                   return
L21100:         REM Handles first  case--print employee information
                    print$(1) = empcode$
                    linenumber%(1) = 2
                    return
L21200:         REM Handles second case--department code
                    print$(1) = "DEPT: " & department$
                    print$(10) = " "
                    if takehome < 0 then L21250
                    linenumber%(1) = 7
                    return
L21250:             print$(10) = "*********"
                    linenumber%(1) = 3
                    return
L21300:         REM Handles third case--negative check message
                    print$(1) = " "
                    print$(10) = "*WILL   *"
                    linenumber%(1) = 4
                    return
L21400:         REM Handles fourth case--negative check
                    print$(10) = "*NOT    *"
                    linenumber%(1) = 5
                    return
L21500:         REM Handles fifth case--negative check
                    print$(10) = "*PROCESS*"
                    linenumber%(1) = 6
                    return
L21600:         REM Handles sixth case--negative check
                    print$(10) = "*********"
                    linenumber%(1) = 7
                    return
L21800:         REM Handles seventh case--zap variables
                    print$(1), print$(10) = " "
                    linenumber%(1) = 8
                    colsdone% = colsdone% + 1
                    return
L22000:     REM Second column--employee name
                on linenumber%(2) gosub L22100, L22200, L22300, L22400
                   return
L22100:         REM Handles first  case--last name if nonblank
                    if name$(3) = " " then L22200
                       print$(2) = name$(3)
                       linenumber%(2) = 2
                       return
L22200:         REM Handles second case--first name (maybe initial, too)
                    if name$(1) = " " then L22300
                       print$(2) = name$(1)
                       if name$(2) <> " " and len(name$(2)) = 1          ~
                          then str(print$(2), len(print$(2)) + 2) =      ~
                                         name$(2)
                       linenumber%(2) = 3
                       return
L22300:         REM Handles third  case--middle name, if len > 1
                    if name$(2) = " " then L22400
                       if len(name$(2)) <= 1 then L22400      /* INITIAL*/
                          print$(2) = name$(2)
                          linenumber%(2) = 4
                          return
L22400:         REM Handles fourth case--zap variables
                    print$(2) = " "
                    linenumber%(2) = 5
                    colsdone% = colsdone% + 1
                    return

L23000:     REM Third  column--earnings information
                on linenumber%(3) gosub L23030, L23050
                   return
L23030:         REM Handles first  case--initialization for processing
                    pointer%(1) = 0
L23050:         REM Handles second case--print an earnings line
                    pointer%(1) = pointer%(1) + 1
                    if pointer%(1) > earnptr% then L23150
                       print$(3) = earnstak$(pointer%(1))
                       call "CONVERT" (earnstak(pointer%(1),1), 0.2,     ~
                                                       str(print$(4),,6))
                       call "CONVERT" (earnstak(pointer%(1),2), 2.2,     ~
                                                       str(print$(5),,9))
                       linenumber%(3) = 2
                       return
L23150:         REM Handles third  case--zap variables
                    print$(3), print$(4), print$(5) = " "
                    linenumber%(3) = 3
                    colsdone% = colsdone% + 1
                    return

L24000:     REM Fourth column--deductions information
                on linenumber%(4) gosub L24030, L24050
                   return
L24030:         REM Handles first  case--initialization for processing
                    pointer%(2) = 0
L24050:         REM Handles second case--print a deductions line
                    pointer%(2) = pointer%(2) + 1
                    if pointer%(2) > dedxnptr% then L24220
                       print$(6) = str(dedxnstak$(pointer%(2)),4,12)
                       print$(7), print$(8), print$(9) = " "
                       temp% = val(str(dedxnstak$(pointer%(2)),2,2),2)
                       temp = dedxnstak(temp%,2)
                       if print$(6) = "DIRECT DEPOS" then L24140
                            call "CONVERT" (temp, 2.2, str(print$(7),,9))
L24140:                temp = dedxnstak(temp%,3)
                       print$(8), print$(9) = " "
                       if str(dedxnstak$(pointer%(2)),,1)<>"Y" then L24190
                          call "CONVERT" (temp, 2.2, str(print$(8),,9))
                          goto L24200
L24190:                call "CONVERT" (temp, 2.2, str(print$(9),,9))
L24200:             linenumber%(4) = 2
                    return
L24220:         REM Handles fourth case--zap variables
                    print$(6), print$(7), print$(8), print$(9) = " "
                    linenumber%(4) = 3
                    colsdone% = colsdone% + 1
                    return

L25000:     REM Fifth  column--"other" information
                on linenumber%(5) gosub L25025, L25100, L25200,             ~
                                        L25300, L25400, L25500, L25550
                   return
L25025:         REM Handles first case--social security number
                    if ssn$ = " " then L25100
                       print$(11) = "SS#:" & ssn$
                       if prtflag% = 1% then linenumber%(5) = 7          ~
                          else linenumber%(5) = 2
                       return
L25100:         REM Handles second case--check number if extant
                    if takehome < 0 then print$(11) = "RVRSL: "& check$  ~
                       else print$(11) = "CHECK: "& check$
                    if takehome >= 0 and str(check$,,1) = "D" then       ~
                            print$(11) = "DSLIP: "& check$
                    linenumber%(5) = 3
                    return
L25200:         REM Handles second case a -- check date
                    print$(11) = "DATED: " & checkdate$
                    linenumber%(5) = 4
                    return
L25300:         REM Handles third  case--first date pay period
                    if firstpaydate$ = " " or firstdate$ = blankdate$ ~
                                 then L25400
                       print$(11) = "FROM:          "
                       str(print$(11), 7) = firstpaydate$
                       linenumber%(5) = 5
                       return
L25400:         REM Handles fourth case--last date pay period
                    if lastpaydate$ = " " or lastpaydate$ = blankdate$ ~
                                 then L25500
                       print$(11) = "TO:            "
                       str(print$(11), 7) = lastpaydate$
                       linenumber%(5) = 6
                       return
L25500:         REM Handles fifth  case--zap variables
                    if postdate$ = " " or postdate$ = blankdate$ ~
                                 then L25550
                       print$(11) = "POSTED:        "
                       str(print$(11), 8) = postdate$
                       call "DATEFMT" (str(print$(11), 8))
                       linenumber%(5) = 7
                       return
L25550:         REM Handles sixth  case--zap variables
                    print$(11) = " "
                    linenumber%(5) = 8
                    colsdone% = colsdone% + 1
                    return

L26000:     REM Fifth  column--totals line
                on linenumber%(6) gosub L26030, L26340
                   return
L26030:         REM Handles first case
                   if colsdone% < 5 then return
                REM Handles second case print totals
                    REM Total gross pay
                        gunits, gdollars, deductions, employer = 0
                        if earnptr% = 0 then L26160
                           for temp% = 1 to earnptr%
                               gdollars = gdollars + earnstak(temp%,2)
                               gunits = gunits + earnstak(temp%,1)
                           next temp%
                        print$(3) = "GROSS"
                        call "CONVERT" (gunits, 0.2, str(print$(4),,6))
                        call "CONVERT" (gdollars, 2.2, str(print$(5),,9))
L26160:             REM Total net pay
                        if dedxnptr% = 0 then L26290
                        for temp% = 1 to dedxnptr%
                           ptr% = val(str(dedxnstak$(temp%),2,2),2)
                           temp = dedxnstak(ptr%,3)
                           if str(dedxnstak$(temp%),,1)<>"Y" then L26240
                              deductions = deductions + temp
                              goto L26250
L26240:                    employer = employer + temp
L26250:                 next temp%
                    print$(6) = "DEDUCTIONS"
                    call "CONVERT" (deductions, 2.2, str(print$(8),,9))
                    call "CONVERT" (employer, 2.2, str(print$(9),,9))
L26290:             takehome = gdollars - deductions
                    call "CONVERT" (takehome, 2.2, str(print$(10),,9))
                    if prtflag% = 1 then print$(2) = "EMPLOYEE TOTALS"   ~
                       else print$(2) = "CHECK TOTAL"
                    linenumber%(6) = 2
                    return
L26340:         REM Handles third case--zap variables
                    linenumber%(6) = 3
                    print$() = " "
                    colsdone% = colsdone% + 1
                    return

L28000:  REM Page control subroutine

                line% = line% + 1
                if line% < 60 then return
                   if page% > 0 then L28060
                      gosub L28081
                      gosub screen_dump
                      goto L28080
L28060:            if tagprinted% <> 0 then L28080
                      print using L28430

L28080:            page% = page% + 1
L28081:            rptttle$ = "EARNINGS REGISTER"
                   call "FMTTITLE" (rptttle$, " ", 12%)

                   print page
                   print using L28230, rptdate$, rpttime$, company$
                   print using L28270, rptttle$, page%
                   print
                       if page% = 0% then return
                   print using L28310
                   print using L28340
                   print using L28370, ":"
                   print using L28400
                   print using L28430
                   line% = 8              /* Set starting line on page  */
                   tagprinted% = 1
                   return

L28230: %RUN: ######## ########        ##################################~
        ~###########################                          PRLCHKRG:PRL~
        ~010

L28270: %                              ##################################~
        ~###########################                                PAGE:#~
        ~###

L28310: %+------------+-----------------+--------------------------------~
        ~--------+----------------------+---------+---------+-------------~
        ~--+
L28340: %!  EMPLOYEE  !        LAST     !  E A R N I N G S   I N F O   ! ~
        ~      D E D U C T I O N S      ! EMPLOYER! NET PAY !   O T H E R ~
        ~  !
L28370: %!            !  NAME# FIRST    +------------+-------+---------+-~
        ~-----------+-------------------+   PAID  !         !             ~
        ~  !
L28400: %!    CODE    !        MIDDLE   !    TYPE    ! UNITS !  AMOUNT !D~
        ~ESCRIPTIONS! SUBJECT !  AMOUNT !  AMOUNT !         !             ~
        ~  !
L28430: %+------------+-----------------+------------+-------+---------+-~
        ~-----------+---------+---------+---------+---------+-------------~
        ~--+
L28452: %+============+=================+============+=======+=========+=~
        ~===========+=========+=========+=========+=========+=============~
        ~==+
L28460: %!############!#################!############!#######!#########!#~
        ~###########!#########!#########!#########!#########!#############~
        ~##!

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%:inpmessage$ = " "
                  on fieldnr% gosub L28630,         /* Dates            */~
                                    L28690,         /* Employee         */~
                                    L28750          /* Report Level     */

                     return
L28630:     REM DEFAULT/ENABLE FOR First Date
                if firstdate$ = " " or firstdate$ = blankdate$ ~
                                 then firstdate$ = "ALL"
                inpmessage$ = "Enter Posting Date Range"
                return
L28690:     REM DEFAULT/ENABLE FOR First Employee
                if firstemployee$ = " " then firstemployee$ = "ALL"
                inpmessage$ = "Enter Employee Range"
                return
L28750:     REM DEFAULT/ENABLE FOR Report Level
                inpmessage$ = "Select Report Level"
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

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *   L O A D   E M P L O Y E E   M A S T E R   R E C O R D   *~
            *                                                           *~
            * LOADS THE EMPLOYEE MASTER RECORD FROM THE EMPLOYEE MASTER *~
            * FILE.                                                     *~
            *************************************************************

            get   #6, using L30110, empcode$, check$, checkdate$,         ~
                      firstpaydate$, lastpaydate$, postdate$,department$,~
                      other$

L30110:     FMT CH(12),                  /* EMPLOYEE CODE              */~
                CH(8),                   /* CHECK NUMBER               */~
                XX(3),                   /* REVERSE SEQUENCE NUMBER    */~
                CH(6),                   /* DATE OF CHECK              */~
                CH(6),                   /* FIRST DATE THIS PAY PERIOD */~
                CH(6),                   /* LAST DATE THIS PAY PERIOD  */~
                XX(9),                   /* CASH IN BANK ACCOUNT       */~
                XX(8),                   /* GROSS PAY AMOUNT           */~
                XX(8),                   /* NET PAY AMOUNT             */~
                XX(7),                   /* RECONCILIATION INFO        */~
                CH(6),                   /* POSTING DATE               */~
                CH(4),                   /* EMPLOYEE DEPARTMENT CODE   */~
                CH(8)                    /* Deposit Slip # or Check #  */

            call "DATEFMT" (checkdate$)
            call "DATEFMT" (firstpaydate$)
            call "DATEFMT" (lastpaydate$)

            ssn$, name$() = " "
            name$(3) = "NOT IN PERSONNEL"
            name$(1)  = "FILE"

         if f1%(14) = 1 then get #14, using L30410,   /* FILE: PERMASTR */~
            name$(3),            /* Last name of person - part of pers */~
            name$(1),            /* First name of person               */~
            name$(2),            /* Middle name of person              */~
            ssn$

            return

L30410: FMT                      /* FILE: PERMASTR                     */~
            XX(1),               /* General purpose status indicator   */~
            CH(15),              /* Last name of person - part of pers */~
            CH(10),              /* First name of person               */~
            CH(1),               /* Middle name of person              */~
            CH(11),              /* Social security number             */~
            XX(12),              /* employee code                      */~
            XX(10),              /* Telephone number                   */~
            XX(30),              /* Street address line 1              */~
            XX(30),              /* Street address line 2              */~
            XX(20),              /* City in address                    */~
            XX(20),              /* County in address                  */~
            XX(2),               /* State in address                   */~
            XX(9),               /* Zip code in address                */~
            XX(30),              /* emergency contact                  */~
            XX(10),              /* emergency contact's phone number   */~
            XX(16),              /* Emergency contacts relationship to */~
            XX(1),               /* Gender of a person                 */~
            XX(6),               /* birth date                         */~
            XX(3),               /* minority (eeoc compliance) code    */~
            XX(1),               /* Marital status - personnel system  */~
            XX(2),               /* Number of dependants - personnel s */~
            XX(16),              /* A persons physical status - handyc */~
            XX(16),              /* A persons military status - vet, r */~
            XX(16),              /* Citizenship status - personnel sys */~
            XX(16),              /* Passport status - personnel system */~
            XX(16),              /* Union status - personnel system    */~
            XX(16),              /* Bonding status - personnel system  */~
            XX(16),              /* Current job title - personnel syst */~
            XX(16),              /* EEO class of current job - personn */~
            XX(4),               /* Current department                 */~
            XX(1),               /* Current shift worked               */~
            XX(16),              /* Current supervisor                 */~
            XX(6),               /* Original date hired                */~
            XX(6),               /* Seniority date                     */~
            XX(6)                /* Last date rehired                  */

L31000: REM *************************************************************~
            *          L O A D   E A R N I N G S   R E C O R D          *~
            *                                                           *~
            * LOADS AN EARNINGS RECORD SO THAT WE CAN PUSH IT ONTO THE  *~
            * EARNINGS STACK.  THE REASON WE HAVE AN EARNINGS STACK IS  *~
            * SO THAT WE CAN A.) COMPUTE THE GROSS PAY IN ADVANCE OF    *~
            * PRINTING IT, AND B.) ACCUMULATE ALL OF THE EARNINGS TYPES *~
            * REGARDLESS OF DEPARTMENT.                                 *~
            *************************************************************

            get   #7, using L31140, dept$, type$, earn(1), earn(2)
            return

L31140:     FMT XX(12),                  /* EMPLOYEE CODE              */~
                XX(8),                   /* CHECK NUMBER               */~
                XX(3),                   /* REVERSE SEQUENCE NUMBER    */~
                XX(1),                   /* Record Type (P=PAY, W=WHLD)*/~
                XX(1),                   /* SEQUENCE NUMBER            */~
                XX(1),                   /* Employee Paid? DED only    */~
                CH(6),                   /* CATEGORY                   */~
                CH(18),                  /* EARN/DEDUCTION TYPE        */~
                PD(14,4),                /* UNITS / UNITS SUBJECT      */~
                XX(8),                   /* RATE / DOLLARS SUBJECT     */~
                PD(14,4),                /* AMOUNT                     */~
                CH(18),                  /* Accounts                   */~
                CH(28)                   /* EARN. REGISTER FREE SPACE  */

L32000: REM *************************************************************~
            *         L O A D   D E D U C T I O N   R E C O R D         *~
            *                                                           *~
            * LOADS A DEDUCTION RECORD SO THAT WE CAN PUT IT IN THE     *~
            * STACK.  NOTE THAT WE HAVE TO DO THAT INSTEAD OF PRINTING  *~
            * DIRECTLY SINCE WE NEED TO KNOW THE TOTAL DEDUCTIONS       *~
            * FOR PRINTING THE "NET PAY" FIELD.                         *~
            *************************************************************

            get   #7, using L32130, dedempflag$, dedcat$, deduction$,     ~
                                   unitbase, dollarbase, dedamt
            return

L32130:     FMT XX(12),                  /* EMPLOYEE CODE              */~
                XX(8),                   /* CHECK NUMBER               */~
                XX(3),                   /* REVERSE SEQUENCE NUMBER    */~
                XX(1),                   /* Record Type (P=PAY, W=WHLD)*/~
                XX(1),                   /* SEQUENCE NUMBER            */~
                CH(1),                   /* Employee Paid? DED only    */~
                CH(6),                   /* CATEGORY                   */~
                CH(18),                  /* EARN/DEDUCTION TYPE        */~
                PD(14,4),                /* UNITS / UNITS SUBJECT      */~
                PD(14,4),                /* RATE / DOLLARS SUBJECT     */~
                PD(14,4),                /* AMOUNT                     */~
                CH(18),                  /* Accounts                   */~
                CH(28)                   /* EARN. REGISTER FREE SPACE  */

L40000: REM *************************************************************~
            *    R E P O R T   P R I N T I N G   S U B R O U T I N E    *~
            *                                                           *~
            * PRINTS THE EARNINGS/DEDUCTION RECAP BY DEPARTMENT.        *~
            *************************************************************

            colsdone% = 0
            mat linenumber% = con
            go1%, go2%, go3%, e1, e2, d1, d2, tot1, tot2, tot3, tot4 = 0

            REM Loop through computing and printing lines until done.
L40110:         for column% = 1 to 3
                    on column% gosub L41000, L42000, L43000
                    next column%
                if colsdone% < 3 then L40220
                   REM Exit routine for finished.
                       return

L40220:         gosub L44500              /* Page heading, if neccessary*/
                print using L45230, print$(1), print$(2), print$(3),      ~
                                   print$(4), print$(5), print$(6),      ~
                                   print$(7), print$(8), print$(9),      ~
                                   print$(10)
                goto L40110

L41000:     REM First  column--earnings stack
                on linenumber%(1) gosub L41100, L41300, L41400, L41500
                   return
L41100:         REM Handles first  case--print an entry
                    go1% = go1% + 1
                    if go1% > eptr% then L41300
                    if go1% = 1 then prevdept1$ = str(estack$(go1%), 1, 4)
                    if prevdept1$ <> str(estack$(go1%), 1, 4) then L41300
                    print$(1) = str(estack$(go1%), 1, 4)
                    print$(2) = str(estack$(go1%), 5, 12)
                    convert str(estack$(go1%), 17, 4) to t%
                    print$(3), print$(4) = " "
                    call "CONVERT" (estak1(t%), 2.2, str(print$(3),4,9))
                    call "CONVERT" (estak2(t%), 2.2, str(print$(4),4,9))
                    e1 = e1 + estak1(t%)
                    e2 = e2 + estak2(t%)
                    linenumber%(1) = 1
                    return
L41300:         REM Handles second case--department total
                    print$(1) = prevdept1$
                    print$(2) = "***TOTAL***"
                    print$(3), print$(4) = " "
                    call "CONVERT" (e1, 2.4, str(print$(3),4,9))
                    call "CONVERT" (e2, 2.2, str(print$(4),4,9))
                    prevdept1$ = str(estack$(go1%), 1, 4)
                    tot1 = tot1 + e1
                    tot2 = tot2 + e2
                    e1, e2 = 0
                    linenumber%(1) = 3
                    return
L41400:         REM Handles third case--tag line
                    print$(1) = "----"
                    print$(2) = "------------"
                    print$(3) = "-----------------"
                    print$(4) = "----------------"
                    if go1% > eptr% then linenumber%(1) = 4              ~
                                    else linenumber%(1) = 1
                    go1% = go1% - 1
                    return
L41500:         REM Handles fourth case--zap variables
                    print$(1), print$(2), print$(3), print$(4) = " "
                    colsdone% = colsdone% + 1
                    linenumber%(1) = 5
                    return

L42000:     REM Second column--deduction stack
                on linenumber%(2) gosub L42100, L42300, L42400, L42500
                   return
L42100:         REM Handles first  case--print an entry
                    go2% = go2% + 1
                    if go2% > dptr% then L42300
                    if go2% = 1 then prevdept2$ = str(dstack$(go2%), 1, 4)
                    if prevdept2$ <> str(dstack$(go2%), 1, 4) then L42300
                    print$(5) = str(dstack$(go2%), 1, 4)
                    print$(6) = str(dstack$(go2%), 5, 12)
                    convert str(dstack$(go2%), 17, 4) to t%
                    print$(7) = " "
                    call "CONVERT" (dstak(t%), 2.2, str(print$(7),4,9))
                    d1 = d1 + dstak(t%)
                    linenumber%(2) = 1
                    return
L42300:         REM Handles second case--department total
                    print$(5) = prevdept2$
                    print$(6) = "***TOTAL***"
                    print$(7) = " "
                    call "CONVERT" (d1, 2.2, str(print$(7),4,9))
                    prevdept2$ = str(dstack$(go2%), 1, 4)
                    tot3 = tot3 + d1
                    d1 = 0
                    linenumber%(2) = 3
                    return
L42400:         REM Handles third case--tag line
                    print$(5) = "----"
                    print$(6) = "------------"
                    print$(7) = "----------------"
                    if go2% > dptr% then linenumber%(2) = 4              ~
                                    else linenumber%(2) = 1
                    go2% = go2% - 1
                    return
L42500:         REM Handles fourth case--zap variables
                    print$(5), print$(6), print$(7) = " "
                    colsdone% = colsdone% + 1
                    linenumber%(2) = 5
                    return

L43000:     REM Second column--deduction stack
                on linenumber%(3) gosub L43030, L43160, L43260, L43340
                   return
L43030:         REM Handles first  case--print an entry
                    go3% = go3% + 1
                    if emprptr% = 0 then L43260
                    if go3% > emprptr% then L43160
                    if go3% = 1 then prevdept3$ = str(emprstack$(go3%),,4)
                    if prevdept3$ <> str(emprstack$(go3%),,4) then L43160
                    print$(8) = str(emprstack$(go3%), 1, 4)
                    print$(9) = str(emprstack$(go3%), 5, 12)
                    convert str(emprstack$(go3%), 17, 4) to t%
                    print$(10) = " "
                    call "CONVERT" (emprstak(t%),2.2, str(print$(10),4,9))
                    d2 = d2 + emprstak(t%)
                    linenumber%(3) = 1
                    return
L43160:         REM Handles second case--department total
                    print$(8) = prevdept3$
                    print$(9) = "***TOTAL***"
                    print$(10) = " "
                    call "CONVERT" (d2, 2.2, str(print$(10),4,9))
                    prevdept3$ = str(emprstack$(go3%), 1, 4)
                    tot4 = tot4 + d2
                    d2 = 0
                    linenumber%(3) = 3
                    return
L43260:         REM Handles third case--tag line
                    print$(8) = "----"
                    print$(9) = "------------"
                    print$(10) = "----------------"
                    if go3% > emprptr% then linenumber%(3) = 4           ~
                                    else linenumber%(3) = 1
                    go3% = go3% - 1
                    return
L43340:         REM Handles fourth case--zap variables
                    print$(8), print$(9), print$(10) = " "
                    colsdone% = colsdone% + 1
                    linenumber%(3) = 5
                    return

L44500:     REM Page control subroutine

                line% = line% + 1
                if line% < 60 then return
                   if page% > 0 then L44570
                     if dptr% + eptr% + emprptr% > 0 then L44580
                     gosub L44582
                     gosub screen_dump
                     goto  L44580
L44570:            print using L45140

L44580:            page% = page% + 1
L44582:            rptttle$ = "DEPARTMENT RECAP"
                   call "FMTTITLE" (rptttle$, " ", 12%)

                   print page
                   print using L44730, rptdate$, rpttime$, company$
                   print using L44770, rptttle$, page%
                   print
                       if page% = 0% then return
                   print using L45080
                   print using L45110
                   print using L45140
                   print using L45170
                   print using L45140
                   line% = 8              /* Set starting line on page  */
                   return

L44730: %RUN: ######## ########        ##################################~
        ~###########################                          PRLCHKRG:PRL~
        ~010

L44770: %                              ##################################~
        ~###########################                                PAGE:#~
        ~###

L45080: %+----------------------------------------------------+  +-------~
        ~---------------------------+  +----------------------------------~
        ~+

L45110: %!                  E A R N I N G S                   !  !       ~
        ~D E D U C T I O N S        !  !  E M P L O Y E R S    S H A R E  ~
        ~!

L45140: %!----+------------+-----------------+----------------!  !----+--~
        ~----------+----------------!  !----+------------+----------------~
        ~!

L45170: %!DEPT! TYPE       !      UNITS      !     DOLLARS    !  !DEPT!  ~
        ~TYPE      !     DOLLARS    !  !DEPT!  TYPE      !     DOLLARS    ~
        ~!

L45230: %!####!############!#################!################!  !####!##~
        ~##########!################!  !####!############!################~
        ~!

L45270: %!####!############! -###,###,###.## ! -###,###,###.##!  !####!##~
        ~##########! -###,###,###.##!  !####!############! -###,###,###.##~
        ~!

        REM *************************************************************~
            * PRINT SELECTION INFORMATION FROM SCREEN                   *~
            *************************************************************

        screen_dump
L45545:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L45565
                str(i$(), i%, 1%) = hex(20)
                goto L45545
L45565:     print skip(3)
            print tab(37);
            print "--------------------- Report Selection Parameters ----~
        ~---------"
            for x% = 5% to 10%: print tab(37); i$(x%) : next x%
            print tab(37);
            print "------------------------------------------------------~
        ~---------"
            return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(86)) lfac$()
                  on fieldnr% gosub L46170,         /* Dates            */~
                                    L46170,         /* Employees        */~
                                    L46200,         /* Report Level     */~
                                    L46221          /* Full Screen      */

                     goto L46240

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      init(hex(84)) lfac$()
                      lfac$(fieldnr%) = hex(80)
                      return
L46170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      init(hex(84)) lfac$()
                      lfac$(fieldnr%) = hex(81)
                      return
L46200:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      init(hex(84)) lfac$()
                      lfac$(fieldnr%) = hex(82)
                      return
L46221:           REM SET FAC'S FOR FULL SCREEN EDIT
                      init(hex(84)) lfac$()
                      init(hex(81)) str(lfac$(),,3)
                      return

L46240:     accept                                                       ~
               at (01,02),                                               ~
                  "Earnings Register Report",                            ~
               at (01,66),                                               ~
                  "DATE:",                                               ~
               at (01,72), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,30), fac(hex(a4)), scrnfrom$              , ch(12),~
               at (05,45), fac(hex(a4)), scrnto$                , ch(12),~
                                                                         ~
               at (06,02),                                               ~
                  "Posting Date Range:",                                 ~
               at (06,30), fac(lfac$( 1)), firstdate$           , ch(10),~
               at (06,45), fac(lfac$( 1)), lastdate$            , ch(10),~
               at (07,02),                                               ~
                  "Employee Code Range:",                                ~
               at (07,30), fac(lfac$( 2)), firstemployee$,        ch(12),~
               at (07,45), fac(lfac$( 2)), lastemployee$,         ch(12),~
               at (08,02),                                               ~
                  "Report Level:",                                       ~
               at (08,30), fac(lfac$( 3)), level$               , ch(01),~
               at (09,12),                                               ~
                  "1. Detail",                                           ~
               at (10,12),                                               ~
                  "2. Summary",                                          ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65), fac(hex(84)), pf16$,                          ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L46730
                  call "MANUAL" ("PRLCHKRG")
                  goto L46240

L46730:        if keyhit% <> 15 then L47550
                  call "PRNTSCRN"
                  goto L46240

L47550:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  if fieldnr% < 4% then L47770
                              gosub L47820          /* FIRST DATE       */
                              if errormsg$ <> " " then return
                              gosub L47940          /* FIRST EMPLOYEE   */
                              if errormsg$ <> " " then return
                              gosub L48030          /* Report Level     */
                     return

L47770:           on fieldnr% gosub L47820,         /* First Date       */~
                                    L47940,         /* First Employee   */~
                                    L48030          /* Report Level     */
                     return

L47820:     REM TEST DATA FOR First Date
                if firstdate$ <> "ALL" then L47840
                   lastdate$ = " "
                   return
L47840:         call "DATEOKC" (firstdate$, firstdate%, errormsg$)
                   if errormsg$ <> " " then return
                if lastdate$ = " " or lastdate$ = blankdate$ ~
                                 then lastdate$ = firstdate$
                call "DATEOKC" (lastdate$, lastdate%, errormsg$)
                   if errormsg$ <> " " then return
                if lastdate% < firstdate% then                           ~
                                        errormsg$ = "Invalid Date Range"
                return
L47940:     REM TEST DATA FOR First Employee
                if firstemployee$ <> "ALL" then L47980
                   lastemployee$ = " "
                   return
L47980:         if lastemployee$ = " " then lastemployee$ = firstemployee$
                if lastemployee$ < firstemployee$ then                   ~
                                     errormsg$ = "Invalid Employee Range"
                return

L48030:     REM TEST DATA FOR Report Level
                if level$ = "1" or level$ = "2" then return
                errormsg$ = "Invalid Report Level: " & level$
                return

        REM *************************************************************~
            *        S T A C K   P U S H I N G   R O U T I N E S        *~
            *                                                           *~
            * PUSHES THE INDICATED INFORMATION ONTO THE DESIRED STACK   *~
            * FOR LATER PROCESSING.                                     *~
            *************************************************************

            deffn'162(etype$, units, amount)
                  search str(earnstack$(),1) = str(etype$) to location$  ~
                                                              step 12
                  if location$ = hex(0000) then L60140
                     temp% = int(val(location$,2)/12)+1
                     earnstak(temp%,1) = earnstak(temp%,1) + units
                     earnstak(temp%,2) = earnstak(temp%,2) + amount
                     return
L60140:           REM Push new item onto stack.
                      earnptr% = earnptr% + 1
                      earnstak$(earnptr%) = etype$
                      earnstak(earnptr%,1) = units
                      earnstak(earnptr%,2) = amount
                      return

            deffn'163(deduction$, amount)
                  if dedempflag$ <> "Y" then dedempflag$ = "N"
                  temp$ = dedempflag$
                  str(temp$,4) = str(deduction$)
                  REM Push new entry onto sales account stack.
                      dedxnptr% = dedxnptr% + 1
                      dedxnstak$(dedxnptr%) = temp$
                      str(dedxnstak$(dedxnptr%),2,2) = bin(dedxnptr%,2)
                      dedxnstak(dedxnptr%,1) = unitbase
                      dedxnstak(dedxnptr%,2) = dollarbase
                      dedxnstak(dedxnptr%,3) = amount
                      return

            deffn'172(dpt$, etype$, units, amount)
                  string$ = dpt$
                  str(string$, 5) = etype$
                  search str(estack$(),1) = str(string$, 1, 16)          ~
                                         to location$ step 20
                  if location$ = hex(0000) then L60500
                     temp% = int(val(location$,2)/20)+1
                     estak1(temp%) = estak1(temp%) + units
                     estak2(temp%) = estak2(temp%) + amount
                     return
L60500:           REM Push new item onto stack.
                      eptr% = eptr% + 1
                      estack$(eptr%) = string$
                      convert eptr% to                                   ~
                               str(estack$(eptr%), 17, 4), pic(####)
                      estak1(eptr%) = units
                      estak2(eptr%) = amount
                      return

            deffn'173(dpt$, deduction$, amount)
                  string$ = dpt$
                  str(string$, 5) = deduction$
                  search str(dstack$(),1) = str(string$, 1, 16)          ~
                               to location$ step 20
                  if location$ = hex(0000) then L60680
                     temp% = int(val(location$,2)/20)+1
                     dstak(temp%) = dstak(temp%) + amount
                     return
L60680:           REM Push new entry onto stack.
                      dptr% = dptr% + 1
                      dstack$(dptr%) = string$
                      convert dptr% to                                   ~
                               str(dstack$(dptr%), 17, 4), pic(####)
                      dstak (dptr%) = amount
                      return

            deffn'174(dpt$, deduction$, amount)
                  string$ = dpt$
                  str(string$, 5) = deduction$
                  search str(emprstack$(),1) = str(string$, 1, 16)       ~
                               to location$ step 20
                  if location$ = hex(0000) then L60850
                     temp% = int(val(location$,2)/20)+1
                     emprstak(temp%) = emprstak(temp%) + amount
                     return
L60850:           REM Push new entry onto stack.
                      emprptr% = emprptr% + 1
                      emprstack$(emprptr%) = string$
                      convert emprptr% to                                ~
                            str(emprstack$(emprptr%), 17, 4), pic(####)
                      emprstak (emprptr%) = amount
                      return

            deffn'182(etype$, units, amount)
                  search str(etotstak$(),1) = str(etype$) to location$   ~
                                                                  step 12
                  if location$ = hex(0000) then L61000
                     temp% = int(val(location$,2)/12)+1
                     etotstak(temp%,1) = etotstak(temp%,1) + units
                     etotstak(temp%,2) = etotstak(temp%,2) + amount
                     return
L61000:           REM Push new item onto stack.
                      etotptr% = etotptr% + 1
                      etotstak$(etotptr%) = etype$
                      etotstak(etotptr%,1) = units
                      etotstak(etotptr%,2) = amount
                      return

            deffn'183(deduction$, amount)
                  if dedempflag$ <> "Y" then dedempflag$ = "N"
                  temp$ = dedempflag$
                  str(temp$,4) = str(deduction$)
                  search str(dtotstak$(),4%) = str(deduction$,1%,18%) to ~
                                                             loc$ step 21%
                  if loc$ = hex(0000) then L61180
                     temp% = int(val(loc$,2%)/21%)+1%
                     dtotstak(temp%,1) = unitbase + dtotstak(temp%,1)
                     dtotstak(temp%,2) = dollarbase + dtotstak(temp%,2)
                     dtotstak(temp%,3) = amount + dtotstak(temp%,3)
                     return
L61180:           REM Push new entry onto deduction stack.
                      dtotptr% = dtotptr% + 1
                      dtotstak$(dtotptr%) = temp$
                      str(dtotstak$(dtotptr%),2,2) = bin(dtotptr%,2)
                      dtotstak(dtotptr%,1) = unitbase
                      dtotstak(dtotptr%,2) = dollarbase
                      dtotstak(dtotptr%,3) = amount
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
