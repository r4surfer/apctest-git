        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L      W   W  H   H   OOO   L      DDDD    *~
            *  P   P  R   R  L      W   W  H   H  O   O  L      D   D   *~
            *  PPPP   RRRR   L      W   W  HHHHH  O   O  L      D   D   *~
            *  P      R   R  L      W W W  H   H  O   O  L      D   D   *~
            *  P      R   R  LLLLL   W W   H   H   OOO   LLLLL  DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLWHOLD - PRINTS A WITHHOLDING REPORT SHOWING ALL THE    *~
            *            DETAIL BY EMPLOYEE.                            *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/08/81 ! ORIGINAL                                 ! TEM *~
            * 10/21/81 ! SORT BY EMPLOYEE NAME                    ! TEM *~
            * 06/24/86 ! CHANGED SELECTION FROM DED. DESCR. TO DED!     *~
            *          ! METHOD AND REPORT ONLY ON EMPLOYEE DED.  ! SGA *~
            * 06/13/91 ! Eliminated unused function FNX.          ! JDH *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            *************************************************************

        com                                                              ~
            extlen%(15),                 /* EXTERNAL FIELD LENGTHS     */~
            field%(15),                  /* SELECTABLE FIELD LIST      */~
            format$(15)1,                /* DATA TYPE FORMAT CODES     */~
            from$(15)25,                 /* LOW RANGE DATA TEST ITEMS  */~
            fromnr(15),                  /* LOW RANGE NUMERIC TEST ITEM*/~
            length%(15),                 /* INTERNAL FIELD LENGTHS     */~
            position%(15),               /* POSITION IN REC (FROM 1)   */~
            prompt$(15)25,               /* FIELD NAME PROMPT          */~
            record$(12)250,              /* 3 RECORDS * 1000 CHARS EA. */~
            record%(15),                 /* WHICH OF 3 RECORDS IT'S IN */~
            to$(15)25,                   /* HI VALUE RANGE DATA TEST   */~
            tonr(15)                     /* HI RANGE NUMERIC RANGE TEST*/~

        dim                                                              ~
            d(4),                        /* CURR,MTD,QTD,YTD DEDUCTIONS*/~
            dt(4),                       /* TOTAL PER DEPARTMENT       */~
            date$8,                      /* SCREEN FORMATTED DATE      */~
            ddr$12,                      /* DEDUCTION DESCRIPTION      */~
            ddr2$12,                     /* DEDUCTION DESCRIPTION 2    */~
            ddkey$15,                    /* DEDUCTION KEY              */~
            dept$(2)4,                   /* DEPARTMENT CODES FOR PRINT */~
            dept$4,                      /* DEPARTMENT CODE            */~
            dept2$4,                     /* DEPARTMENT CODE 2          */~
            descri$100,                  /* EMPLOYEE DESCRIPTOR        */~
            e(4),                        /* DEDUCT TOTALS PER DEDUC    */~
            edtmessage$79,               /* EDIT MESSAGE TEXT          */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            inpmessage$79,               /* INPUT MESSAGE TEXT         */~
            linenumber%(3),              /* PRINT CONTROL VARIABLE     */~
            name$(3)20,                  /* LAST, FIRST, MIDDLE NAMES  */~
            passpgm$18,                  /* Pgm & Rel# for SLCTSCRN    */~
            prtdate$45,                  /* DATE FOR PRINTING          */~
            prtvar$(11)20,               /* PRINTING VARIABLE          */~
            prtdesc$100,                 /* PRINTING VARIABLE          */~
            readkey$50,                  /* FILE READ KEY              */~
            readkey1$50                  /* Plow key for Deduction File*/~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

            dim cms2v$50
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other          "
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
            * # 4 ! EMPMASTR ! EMPLOYEE MASTER FILE                     *~
            * # 5 ! EMPDEDXN ! EMPLOYEE DEDUCTION LINE ITEM FILE        *~
            * # 9 ! SORTWORK ! WORK FILE FOR SORT ROUTINE               *~
            * #14 ! PERMASTR ! PERSONNEL MASTER FILE                    *~
            *************************************************************

            select #4, "EMPMASTR",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 136,                                    ~
                       keypos = 1, keylen = 12,                          ~
                       alt key 1, keypos = 70, keylen = 1, dup

            select #5, "EMPDEDXN",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 300,                                    ~
                       keypos = 1, keylen = 15,                          ~
                       alt key 1, keypos = 16, keylen = 18, dup

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

            call "SHOSTAT" ("Linking To Data Base To Print Employee Deduc~
        ~tion Accruals")

            call "OPENCHCK" (# 4, 0%, f2%( 4),   0%, " ")
            call "OPENCHCK" (# 5, 0%, f2%( 4),   0%, " ")
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

                data "DEDUCTION METHOD         ", "L", 06, 06, 2, 034,   ~
                     "EMPLOYEE DEPARTMENT      ", "U", 04, 04, 1, 150,   ~
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
            inpmessage$ = "'ALL', 'FIRST', and 'LAST' are valid Parameter~
        ~s"

            for temp% = 1 to 15
                if prompt$(temp%) <> " " then from$(temp%) = "ALL"
            next temp%

            passpgm$ = "PRLWHOLD: " & str(cms2v$,1,8)
L10160:     call "SLCTSCRN" ("PRINT WITHHOLDING REPORT",       errormsg$,~
                           inpmessage$, keyhit%, passpgm$)
                  if keyhit%  =  1 then       L10000
                  if keyhit%  = 16 then       L65000
                  if keyhit% <>  0 then       L10160
            call "SLCTTEST" (errormsg$, maxfields%)
                  if errormsg$ <> " " then L10160

            REM NOW SELECT THE DESIRED DEDUCTION RECORDS
                readkey$ = " "
                flag% = 0
                call "SHOSTAT" ("Selecting Deductions...")
                call "WORKOPEN" (#9, "OUTPT", 4000%, f2%(9))

            REM PLOW THROUGH EMPLOYEE FILE.  REGARD ONLY ACTIVE ONES
L10310:         call "PLOWNEXT" (#4, readkey$, 0%, f1%(4))
                     if f1%(4) = 0 then L10640
                get #4, using L10340, str(record$(), 150, 4)
L10340:                 FMT XX(91), CH(4)
                call "READ100" (#14, str(readkey$,,12), f1%(14))
                     if f1%(14) = 0 then L10310
                get #14, using L10380, str(record$(), 1, 136)
L10380:                 FMT CH(136)

                 REM PLOW DOWN DEDUCTION RECORDS
                     readkey1$ = str(record$(), 39, 12)
L10420:              call "PLOWNEXT" (#5, readkey1$, 12%, f1%(5))
                           if f1%(5) = 0 then L10310
                     get #5, using L10450, str(record$(), 1001, 200)
L10450:                      FMT CH(200)
                     if str(record$(), 1052, 1) = "N" then L10420
                     temp% = 0
                     get #5, using L10470, d(1), d(2), d(3), d(4)
L10470:                      FMT XX(116), 4*PD(14,4)
                     for i% = 1 to 4
                          if abs(d(i%)) > .0001 then temp% = 1
                     next i%
                     if temp% = 0 then L10420  /* NONE TAKEN ALL YEAR */

                call "SLCTPASS" (maxfields%, select%)
                     if select% = 0 then L10420
                     write #9, using L10600, str(record$(), 1034, 06),    ~
                                            " ",                         ~
                                            str(record$(),    2, 26),    ~
                                            str(record$(), 1001, 15),    ~
                                            " "
L10600:              FMT CH(06), CH(04), CH(26), CH(15), CH(43)
                     flag% = 1
                     go to L10420

L10640:     REM NOW SORT THE WORK FILE WITH THE DESIRED RECORDS
                if flag% = 0 then L65000
                call "SHOSTAT" ("Sorting Selected Deductions...")
                call "SLCTSORT" (#9, 76%)

                call "SHOSTAT" ("Printing Deduction Report...")
                dept$(1) = from$(2) : dept$(2) = to$(2)
                if dept$(2) = " " then dept$(2) = dept$(1)
                select printer (134)

            REM PRIME BY READING FIRST RECORD
                read #9, using L10750, ddr$,dept$, name$(1), name$(2),    ~
                      name$(3),  ddkey$,   eod go to L65000
L10750:                 FMT CH(06), CH(04), CH(15), CH(10), CH(1), CH(15)
                descri$ = str(ddkey$,1,12) & " "  & name$(1) &           ~
                                  ", " & name$(2) & " " & name$(3) & "."
                mat dt = zer
                mat e = zer
L10800:         gosub L20000    /* PRINT A DESCRIPTION        */
                if finis% = 99 then L65000
                ddr$ = ddr2$
                dept$ = dept2$
                mat e = zer
                mat dt = zer
                go to L10800

L20000: REM *************************************************************~
            * P R I N T   D E D U C T I O N S                           *~
            *                                                           *~
            * PRINTS THE DEDUCTION REPORT FOR ONE DEDUCTION DESCRIPTION.*~
            * CALL A PLOW ON THE CONSEC FILE.                           *~
            *************************************************************

            colsdone% = 0
            mat linenumber% = con
            init(" ") prtvar$()

            REM LOOP THROUGH PRINTING LINES UNTIL DONE
L20150:         for column% = 1 to 2
                    on column% gosub L21000, L22000
                    next column%
                if colsdone% < 2 then L20220
                             pageline% = pageline% + 1
                             print using L50110
                             return
L20220:         gosub L60000
                print using L50150, prtvar$(1), prtdesc$,                 ~
                                               prtvar$(3),               ~
                                   prtvar$(4), prtvar$(5), prtvar$(6)
                go to L20150

        REM **********************************************************
L21000:     REM *** HANDLES FIRST  COLUMN--DEDUCTION DESCRIPTION
                on linenumber%(1) gosub L21100, L21200
                   return

L21100:         REM HANDLES FIRST  CASE--DEDUCTION DESCRIPTION
                    prtvar$(1) = ddr$
                    linenumber%(1) = 2
                    return
L21200:         REM HANDLES SECOND CASE--ZAP VARIABLES
                    prtvar$(1) = " "
                    linenumber%(1) = 3
                    colsdone% = colsdone% + 1
                    return

        REM **********************************************************
L22000:     REM *** HANDLES SECOND COLUMN--DEDUCTION CATEGORIES
                on linenumber%(2) gosub L22040, L22100, L22200, L22300, L22400
                   return

L22040:     REM HANDLES CASE ZERO--PRINT EMPLOYEE DEDUCTIONS
                prtvar$(2) = dept$
                prtdesc$ = descri$
                mat d = zer
                call "READ100" (#5, ddkey$, f1%(5))
                  if f1%(5) = 0 then L22075           /* WHAT NOW */
                  get #5, using L22065, d(1), d(2), d(3), d(4)
L22065:            FMT XX(116), 4*PD(14,4)
L22075:           for t% = 1 to 4
                     call "CONVERT" (d(t%), 2.2, str(prtvar$(t%+2),1,10))
                     dt(t%) = round(dt(t%) + d(t%), 2)
                  next t%
                  gosub L40000
                  return
L22100:     REM HANDLES FIRST  CASE--PRINT DEPARTMENT TOTALS
                prtdesc$ = "      DEPARTMENT TOTAL"
                str(prtdesc$, 47) = "* SUBTOTAL * ==>"
                prtvar$(2) = dept$
                for t% = 1 to 4
                    call "CONVERT" (dt(t%), 2.2, str(prtvar$(t%+2),1,10))
                    e(t%) = round(e(t%) + dt(t%), 2)
                next t%
                linenumber%(2) = 3
            REM RETURN
L22200:     REM HANDLES SECOND CASE--CHECK IF TOTALS NEEDED
                if finis% = 99 then L22300
                if ddr2$ <> ddr$ then L22300
                dept$ = dept2$
                mat dt = zer
                    go to L22040
L22300:         REM HANDLES THIRD CASE---TOTALS
                    prtdesc$ = "            TOTAL FOR DEDUCTION " & ddr$
                    str(prtdesc$, 60) = "==>"
                    prtvar$(2) = " "
                    for t% = 1 to 4
                      call "CONVERT" (e(t%), 2.2, str(prtvar$(t%+2),1,10))
                    next t%
                    linenumber%(2) = 5
                    return
L22400:         REM HANDLES FOURTH CASE--ZAP VARIABLES
                    for t% = 2 to 6
                        prtvar$(t%) = " "
                    next t%
                    linenumber%(2) = 6
                    colsdone% = colsdone% + 1
                    return

L40000: REM *************************************************************~
            *       P L O W   N E X T   F O R   W O R K   F I L E       *~
            *                                                           *~
            * PLOWS DOWN WORK FILE RETRIEVING DEDUCTION TOTALS FOR A    *~
            * SINGLE DEPARTMENT WITHIN A DEDUCTION DESCRIPTION.         *~
            * ASSUMES THAT WE HAVE THE DEDUCTION DESCRIPTION, AND THE   *~
            * DEPARTMENT CODE, AND THAT WE HAVE ALREADY READ THE FIRST  *~
            * DEDUCTION KEY.                                            *~
            *************************************************************

            linenumber%(2) = 1

            REM READ NEXT RECORD
               read #9, using L40222, ddr2$,dept2$, name$(1), name$(2),   ~
                                   name$(3), ddkey$, eod go to L40270
L40222:              FMT CH(06), CH(04), CH(15), CH(10), CH(1), CH(15)
                descri$ = str(ddkey$,,12) & " " & name$(1) &  ", " &     ~
                                          name$(2) & " " & name$(3) & "."
                if ddr2$ = ddr$ and dept2$ = dept$ then return
                linenumber%(2) = 2
                return

L40270:     REM NO MORE RECORDS IN WORK FILE
                linenumber%(2) = 2
                finis% = 99
                return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *                                                           *~
            * HANDLES ALL THE PRINT FORMATTING.                         *~
            *************************************************************

L50070: %PAGE ######         P A Y R O L L   W I T H H O L D I N G   R E ~
        ~P O R T                ##########################################~
        ~###

L50110: % +--------------+-----------------------------------------------~
        ~-----------------+----------+----------+----------+----------+


L50150: % ! ############ ! ##############################################~
        ~################ !##########!##########!##########!##########!


L50190: % !   METHOD     ! E M P L O Y E E   C O D E   A N D   N A M E   ~
        ~                 ! CURRENT  !   MTD    !   QTD    !   YTD    !


L50220: % !--------------!-----------------------------------------------~
        ~-----------------!----------!----------!----------!----------!


L60000: REM *************************************************************~
            *  P A G E   H E A D I N G / C O N T R O L   R O U T I N E  *~
            *                                                           *~
            * TRACKS WHICH LINE OF THE PAGE WE ARE ON, SKIPS TO NEW PAGE*~
            * AND PRINTS HEADINGS IF WE'RE NOT ABLE TO FIT IT ALL ON ONE*~
            *************************************************************

            pageline% = pageline% + 1
            if pageline% < 60 then return
            if pagenumber% = 0 then L60110
               print using L50110
L60110:        print page
               call "DATE" addr ("HD", prtdate$)
               pagenumber% = pagenumber% + 1
               print using L50070, pagenumber%, prtdate$
               print using L60220, dept$(1), dept$(2)
               print using L50110
               print using L50190
               print using L50220
               pageline% = 7
               return

L60220: %  DEPARTMENTS #### THRU ####
L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "FILEBGON" (#9)
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
