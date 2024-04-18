        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L      EEEEE  RRRR   N   N  RRRR    GGG    *~
            *  P   P  R   R  L      E      R   R  NN  N  R   R  G       *~
            *  PPPP   RRRR   L      EEEE   RRRR   N N N  RRRR   G GGG   *~
            *  P      R   R  L      E      R   R  N  NN  R   R  G   G   *~
            *  P      R   R  LLLLL  EEEEE  R   R  N   N  R   R   GGG    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLERNRG - PRINTS ALL OR PART OF THE EARNINGS REGISTER BY *~
            *            DEPARTMENT FOR A RANGE OF EMPLOYEES IN ANY     *~
            *            GIVEN DEPARTMENT.                              *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/10/81 ! ORIGINAL                                 ! TEM *~
            * 08/04/81 ! MONTH END PROCESSING                     ! TEM *~
            * 03/26/86 ! EXTENDED PRLDEPTF RECORD LENGTH          ! HES *~
            * 06/24/86 ! MODIFIED TESTS AT LINES 10314 & 10340    ! SGA *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            *************************************************************

        com                                                              ~
            extlen%(15),                 /* External field lengths     */~
            field%(15),                  /* Selectable field list      */~
            format$(15)1,                /* Data type format codes     */~
            from$(15)25,                 /* Low range data test items  */~
            fromnr(15),                  /* Low range numeric test item*/~
            length%(15),                 /* Internal field lengths     */~
            position%(15),               /* Position in rec (from 1)   */~
            prompt$(15)25,               /* Field name prompt          */~
            record$(12)250,              /* 3 Records * 1000 chars ea. */~
            record%(15),                 /* Which of 3 records it's in */~
            to$(15)25,                   /* Hi value range data test   */~
            tonr(15)                     /* Hi range numeric range test*/~

        dim                                                              ~
            d(4),                        /* Curr,mtd,qtd,ytd deductions*/~
            date$8,                      /* Screen formatted date      */~
            deduc(4),                    /* Totals for decutions       */~
            depart$4,                    /* Employee's deduction code  */~
            dtype$12,                    /* Deduction description      */~
            e(4),                        /* Earnings                   */~
            earn(4),                     /* Earnings totals            */~
            edtmessage$79,               /* Edit message text          */~
            empcode$12,                  /* Employee's code            */~
            errormsg$79,                 /* Error message text         */~
            etype$12,                    /* Earnings description       */~
            first$20,                    /* First name                 */~
            inpmessage$79,               /* Input message text         */~
            last$20,                     /* Last name                  */~
            linenumber%(3),              /* Print control variable     */~
            middle$20,                   /* Middle name                */~
            olddept$4,                   /* Header department code     */~
            olddeptdescr$32,             /* Department description     */~
            paid$1,                      /* Paid in cash? flag         */~
            passpgm$18,                  /* Pgm & rel# for SLCRCSRN    */~
            pays$1,                      /* Employee pays? flag        */~
            prtdate$45,                  /* Date for printing          */~
            prtvar$(11)20,               /* Printing variable          */~
            readkey$50,                  /* File read key              */~
            readkey1$50,                 /* Earnings plow key          */~
            readkey2$50,                 /* Deductions plow key        */~
            ssnum$10,                    /* Social security number     */~
            thiscode$12,                 /* Consec work file read key  */~
            work$100                     /* Work variable              */~

        dim f2%(64),                     /* File status flags for      */~
            f1%(64)                      /* Record-on-file flags       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other          "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLE  F2%()            SHOULD NOT BE   */
                     /* MODIFIED.     IT  IS AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 4 ! EMPMASTR ! EMPLOYEE MASTER FILE                     *~
            * # 5 ! EMPDEDXN ! EMPLOYEE DEDUCTION LINE ITEM FILE        *~
            * # 6 ! EMPEARN1 ! EMPLOYEE EARNINGS RECORDS FILE           *~
            * # 7 ! PRLDEPTF ! PAYROLL DEPARTMENT CODE FILE             *~
            * # 9 ! SORTWORK ! WORK FILE FOR SORT ROUTINE               *~
            * #14 ! PERMASTR ! PERSONNEL EMPLOYEE MASTER FILE           *~
            *************************************************************

             select #4, "EMPMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 136,                                  ~
                         keypos = 1, keylen = 12,                        ~
                         alt key  1, keypos = 70, keylen =  1, dup

            select #5, "EMPDEDXN",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 300,                                    ~
                       keypos = 1, keylen = 15,                          ~
                       alt key 1, keypos = 16, keylen = 18, dup

            select #6, "EMPEARN1",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 200,                                    ~
                       keypos = 1, keylen = 15,                          ~
                       alt key 1, keypos = 16, keylen = 28

            select #7, "PRLDEPTF",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 300,                                    ~
                       keypos = 1, keylen = 4

            select #9, "SORTWORK",                                       ~
                       consec,                                           ~
                       recsize = 100

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

            call"SHOSTAT"("Opening Files To Print The Earnings Register")

            call "OPENCHCK" (#4,  0%, f2%( 4),   0%, " ")
            call "OPENCHCK" (#5,  0%, f2%( 5),   0%, " ")
            call "OPENCHCK" (#6,  0%, f2%( 6),   0%, " ")
            call "OPENCHCK" (#7,  0%, f2%( 7),   0%, " ")
            call "OPENCHCK" (#14, 0%, f2%(14),   0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *                                                           *~
            * HERE IS HOW THE PARAMETERS FOR THE SELECT (ALSO SORT) KEYS*~
            * WORK.  THE FIRST OF EACH DATA ITEM IS THE NAME OF THE KEY.*~
            * IT IS THE PROMPT THAT APPEARS ON THE SCREEN.              *~
            *      THE FIRST ITEM TO THE RIGHT OF THE PROMPT IS THE     *~
            * DATA TYPE.  THE DATA TYPE IS ONE OF THE FOLLOWING.        *~
            *                                                           *~
            *      U = UPPER CASE ALPHANUMERIC.                         *~
            *      L = UPPER/LOWER CASE ALPHANUMERIC.                   *~
            *      D = DATE-FORMATTED UPPER CASE FIELD.  MUST BE YYMMDD *~
            *      N = NUMERIC 8-BYTE FLOATING PT. ALL OTHER NUMERICS   *~
            *          SHOULD BE COMPARED ASCII, OR MODIFY THE ROUTINE. *~
            *                                                           *~
            * THE REMAINING NUMBERS IDENTIFY RESPECTIVELY THE LENGTH OF *~
            * THE FIELD ON THE DISK, THE NUMBER OF POSITIONS IT FILLS   *~
            * ON THE PAPER, WHICH OF THE UP-TO-3 RECORDS IN CORE THIS   *~
            * FIELD LIES IN, AND THE POSITION OF THE FIELD WITHIN THE   *~
            * RECORD.                                                   *~
            *                                                           *~
            * THE FIRST BLANK PROMPT NAME SIGNIFIES THE END OF THE      *~
            * LIST OF PROMPTS ON THE SYSTEM.                            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press RETURN."

            pageline% = 1000

            REM SET UP DATA FOR SELECT INTERPRETATION
                for temp% = 1 to 15
                    read prompt$(temp%), format$(temp%), length%(temp%), ~
                         extlen%(temp%), record%(temp%), position%(temp%)
                    next temp%

                data "DEPARTMENT CODE          ", "U",  4,  4, 1, 150,   ~
                     "EMPLOYEE NAME            ", "L", 15, 15, 1, 002,   ~
                     "EMPLOYEE CODE            ", "U", 12, 12, 1, 039,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001,   ~
                     "                         ", " ", 16, 16, 1, 001

L10000: REM *************************************************************~
            *    S U P E R   S E L E C T O R   R O U T I N E            *~
            *                                                           *~
            * USES ***SUPER SELECTOR*** ROUTINES TO                     *~
            * SELECT AND SORT THE DESIRED RECORDS.                      *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, from$(), to$()
            mat fromnr = zer: mat tonr = zer: mat field% = zer
            inpmessage$ = "'ALL', 'FIRST', and 'LAST' are valid entries"

            for temp% = 1 to 15
                if prompt$(temp%) <> " " then from$(temp%) = "ALL"
            next temp%

            passpgm$ = "PRLERNRG: " & str(cms2v$,1,8)
L10150:     call "SLCTSCRN" ("PRINT EARNINGS REGISTER", errormsg$,       ~
                           inpmessage$, keyhit%, passpgm$)
                  if keyhit%  =  1 then       L10000
                  if keyhit%  = 16 then       L65000
                  if keyhit% <>  0 then       L10150
            call "SLCTTEST" (errormsg$, maxfields%)
                  if errormsg$ <> " " then L10150

            REM NOW SELECT THE DESIRED EMPLOYEES!!!
                readkey$ = " "
                flag% = 0
                call "SHOSTAT" ("Selecting Employees...One Moment Please")
                call "WORKOPEN" (#9, "OUTPT", 500%, f2%(9))

            REM PLOW THROUGH EMPLOYEE FILE.  DISREGARD TERMINATED ONES
L10300:         call "PLOWNEXT" (#4, readkey$, 0%, f1%(4))
                     if f1%(4) = 0 then L10460
                get #4, using L10309, str(record$(),150,4)
L10309:                 FMT XX(91), CH(4)
                call "READ100" (#14, str(readkey$,,12), f1%(14))
                     if f1%(14) = 0 then L10300
                get #14, using L10330, str(record$(), 1, 100)
L10330:                 FMT CH(100)
        REM     IF STR(RECORD$(),, 1) <> "C" THEN 10300
                call "SLCTPASS" (maxfields%, select%)
                     if select% = 0 then L10300
                     write #9, using L10420, str(record$(), 150,  4),     ~
                                            str(record$(),   2, 15),     ~
                                            str(record$(),  17, 11),     ~
                                            str(record$(),  39, 12),     ~
                                            " "
L10420:              FMT CH(04), CH(15), CH(11), CH(12), CH(58)
                     flag% = 1
                     go to L10300

L10460:     REM NOW SORT THE WORK FILE WITH THE DESIRED RECORDS
                if flag% = 0 then L65000
                call "SHOSTAT" ("Sorting Selected Employees...")
                call "SLCTSORT" (#9, 64%)

                call "SHOSTAT" ("Printing Earnings Register...")
                select printer (134)
L10530:         read #9, using L10540, work$, thiscode$, eod goto L65000
L10540:                  FMT CH(30), CH(12)
                gosub'200(thiscode$)
                go to L10530

        REM *************************************************************~
            *  P R I N T   E M P L O Y E E   E A R N I N G S   R E G .  *~
            *                                                           *~
            * LOADS PERTINENT INFO FROM EMPLOYEE'S MASTER FILE.         *~
            *************************************************************

            deffn'200(empcode$)

            REM READ EMPLOYEE FILE
                call "READ100" (#14, empcode$, f1%(14))
                     if f1%(14) = 0 then return    /* UNLIKELY...      */
                get #14, using L12170,    last$,    /* FIRST NAME       */~
                                         first$,   /* MIDDLE NAME      */~
                                         middle$,  /* LAST NAME        */~
                                         ssnum$    /* SOCIAL SECURITY  */
                depart$ = str(work$,,4)

L12170:         FMT  XX(1), CH(15), CH(10), CH(1), CH(11)

            REM CHECK DEPARTMENT
                if depart$ = olddept$ then L20000
                   olddept$ = depart$
                   call "DESCRIBE" (#7,olddept$,olddeptdescr$,1%,f1%(7))
                   pageline% = 1000

L20000: REM *************************************************************~
            * P R I N T   E A R N I N G S   R E G I S T E R   E N T R Y *~
            *                                                           *~
            * PRINTS THE EARNINGS REGISTER FOR ONE EMPLOYEE.  IT DOES   *~
            * A PLOWNEXT ON THE EARNINGS AND DEDUCTION TYPES.           *~
            *************************************************************

            colsdone% = 0
            mat linenumber% = con
            init(" ") prtvar$()
            readkey1$, readkey2$ = empcode$
            mat earn = zer: mat deduc = zer

            REM LOOP THROUGH PRINTING LINES UNTIL DONE
L20150:         for column% = 1 to 3
                    on column% gosub L21000, L22000, L23000
                    next column%
                if colsdone% < 3 then L20220
                             pageline% = pageline% + 1
                             print using L50140
                             return
L20220:         gosub L60000
                print using L50180, prtvar$(1), prtvar$(2), prtvar$(3),   ~
                                   prtvar$( 4),                          ~
                                   prtvar$( 5), prtvar$( 6), prtvar$( 7),~
                                   prtvar$( 8), prtvar$( 9), prtvar$(10),~
                                   prtvar$(11)
                go to L20150

           REM **********************************************************
L21000:     REM *** HANDLES FIRST  COLUMN--DEPARTMENT CODE, NAME, SS#, ETC
                on linenumber%(1) gosub L21100, L21200, L21300, L21400,      ~
                                        L21500, L21600
                   return

L21100:         REM HANDLES FIRST  CASE--PRINT DEPARTMENT/EMPLOYEE CODE
                    prtvar$(1) = depart$ & " (" & empcode$ & ")"
                    linenumber%(1) = 2
                    return
L21200:         REM HANDLES SECOND CASE--PRINT FIRST NAME
                    prtvar$(1) = first$
                    linenumber%(1) = 3
                    return
L21300:         REM HANDLES THIRD CASE--PRINT MIDDLE NAME
                    prtvar$(1) = middle$
                    linenumber%(1) = 4
                    return
L21400:         REM HANDLES FOURTH CASE--PRINT LAST NAME
                    prtvar$(1) = last$
                    linenumber%(1) = 5
                    return
L21500:         REM HANDLES FIFTH CASE--SOCIAL SECURITY NUMBER
                    prtvar$(1) = ssnum$
                    linenumber%(1) = 6
                    return
L21600:         REM HANDLES SIXTH CASE--ZAP VARIABLES
                    prtvar$(1) = " "
                    linenumber%(1) = 7             /* NULL CASE        */
                    colsdone% = colsdone% + 1
                    return

           REM **********************************************************
L22000:     REM *** HANDLES SECOND COLUMN--EARNINGS TYPES
                on linenumber%(2) gosub L22100, L22200, L22300
                   return

L22100:         REM HANDLES FIRST  CASE--PLOW DOWN EARNINGS
L22110:             call "PLOWNEXT" (#6, readkey1$, 12%, f1%(6))
                         if f1%(6) = 0 then L22200
                         gosub L30000
                         if paid$ <> "Y" then L22110
                    prtvar$(2) = etype$
                    for t% = 1 to 4
                        call "CONVERT" (e(t%),2.2,str(prtvar$(t%+2),1,10))
                        earn(t%) = earn(t%) + e(t%)
                    next t%
                    linenumber%(2) = 1
                    return
L22200:         REM HANDLES SECOND CASE--PRINT TOTALS
                    prtvar$(2) = "TOTALS"
                    for t% = 1 to 4
                        call "CONVERT" (earn(t%),2.2,                    ~
                                         str(prtvar$(t%+2),1,10))
                    next t%
                    linenumber%(2) = 3
                    return
L22300:         REM HANDLES THIRD  CASE--ZAP VARIABLES
                    for t% = 2 to 6
                        prtvar$(t%) = " "
                    next t%
                    linenumber%(2) = 4
                    colsdone% = colsdone% + 1
                    return

        REM *************************************************************
L23000:     REM *** HANDLES THIRD  COLUMN--DEDUCTION TYPES
                on linenumber%(3) gosub L23100, L23200, L23300
                   return

L23100:         REM HANDLES FIRST  CASE--PLOW DOWN DEDUCTIONS
L23110:             call "PLOWNEXT" (#5, readkey2$, 12%, f1%(5))
                         if f1%(5) = 0 then L23200
                         gosub L31000
                         if pays$ <> "Y" then L23110
                    prtvar$(7) = dtype$
                    for t% = 1 to 4
                        call "CONVERT" (d(t%),2.2,str(prtvar$(t%+7),1,10))
                        deduc(t%) = deduc(t%) + d(t%)
                    next t%
                    linenumber%(3) = 1
                    return
L23200:         REM HANDLES SECOND CASE--PRINT TOTALS
                    prtvar$(7) = "TOTALS"
                    for t% = 1 to 4
                        call "CONVERT"  (deduc(t%), 2.2,                 ~
                                         str(prtvar$(t%+7),1,10))
                    next t%
                    linenumber%(3) = 3
                    return
L23300:         REM HANDLES THIRD  CASE--ZAP VARIABLES
                    for t% = 7 to 11
                        prtvar$(t%) = " "
                    next t%
                    linenumber%(3) = 4
                    colsdone% = colsdone% + 1
                    return

L30000: REM *************************************************************~
            *  G E T   E A R N I N G S   R E C O R D                    *~
            *                                                           *~
            * RETRIEVE PERTINENT INFO FROM AN EARNINGS RECORD           *~
            *************************************************************

            get #6, using L30070, etype$, paid$, e(1), e(2), e(3), e(4)
L30070:     FMT XX(27), CH(12), XX(4), CH(1), XX(64), PD(14,4),          ~
                                               XX(8), PD(14,4),          ~
                                               XX(8), PD(14,4),          ~
                                               XX(8), PD(14,4)
            return

L31000: REM *************************************************************~
            *  G E T   D E D U C T I O N   R E C O R D                  *~
            *                                                           *~
            * RETRIEVE PERTINENT INFO FROM A DEDUCTION RECORD           *~
            *************************************************************

            get #5, using L31070, dtype$, pays$, d(1), d(2), d(3), d(4)
L31070:     FMT XX(39),CH(12),CH(1),XX(64),PD(14,4),PD(14,4),2*PD(14,4)
            return


        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *                                                           *~
            * HANDLES ALL THE PRINT FORMATTING.                         *~
            *************************************************************

L50070: %PAGE ######         P A Y R O L L   E A R N I N G S   R E G I S ~
        ~T E R                  ##########################################~
        ~###

L50100: %                FOR DEPARTMENT #### ############################~
        ~######


L50140: %+---------------+-----------------------------------------------~
        ~---------+-------------------------------------------------------~
        ~-+

L50180: %!###############!############!##########!##########!##########!#~
        ~#########!############!##########!##########!##########!#########~
        ~#!

L50220: %!DEPT (EMPCODE) !          E A R N I N G S   T Y P E            ~
        ~         !             D E D U C T I O N S                       ~
        ~ !

L50260: %!EMPLOYEE  NAME !---------------------------------------------!-~
        ~---------!-------------------------------------------------------~
        ~-!

L50300: %!SOCIAL SEC. NO.!   TYPE     ! CURRENT  !   MTD    !   QTD    ! ~
        ~  YTD    !    TYPE    ! CURRENT  !   MTD    !   QTD    !   YTD   ~
        ~ !

L50340: %+---------------!------------!----------!----------!----------!-~
        ~---------!------------!----------!----------!----------!---------~
        ~-!

L60000: REM *************************************************************~
            *  P A G E   H E A D I N G / C O N T R O L   R O U T I N E  *~
            *                                                           *~
            * TRACKS WHICH LINE OF THE PAGE WE ARE ON, SKIPS TO NEW PAGE*~
            * AND PRINTS HEADINGS IF WE'RE NOT ABLE TO FIT IT ALL ON ONE*~
            *************************************************************

            pageline% = pageline% + 1
            if pageline% < 60 then return
               print page
               call "DATE" addr ("HD", prtdate$)
               pagenumber% = pagenumber% + 1
               print using L50070, pagenumber%, prtdate$
               print using L50100, olddept$, olddeptdescr$
               print
               print using L50140
               print using L50220
               print using L50260
               print using L50300
               print using L50340
               pageline% = 7
               return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("Closing Files, One Moment Please")
            call "FILEBGON" (#9)
            end
