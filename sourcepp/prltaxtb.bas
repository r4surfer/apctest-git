        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP   RRRR   L      TTTTT   AAA   X   X  TTTTT  BBBB    *~
            *  P   P  R   R  L        T    A   A   X X     T    B   B   *~
            *  PPPP   RRRR   L        T    AAAAA    X      T    BBBB    *~
            *  P      R   R  L        T    A   A   X X     T    B   B   *~
            *  P      R   R  LLLLL    T    A   A  X   X    T    BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLTAXTB - OPERATOR INPUT/MODIFICATION OF STANDARD        *~
            *            BRACKET-TYPE TAX TABLES ALA U.S. GOVT          *~
            *            CIRCULAR E PERCENTAGE WITHHOLDING TABLES       *~
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
            * 06/01/83 ! ORIGINAL                                 ! KEN *~
            * 07/06/88 ! ADDED SPECIAL EDIT FOR NC-IT ROUTINE     ! MDE *~
            * 09/12/88 ! Minor bug fixes for NC-IT ROUTINE        ! JDH *~
            * 12/01/88 ! Changed Percent fields to 4 dec places   ! KAB *~
            * 12/13/91 ! Lots of clean up. Added Report option.   ! KAB *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 02/10/93 ! Minor mods for UNIX. PLOWCODE Dummy Chan.! JDH *~
            * 07/15/93 ! PRR 12981. Corrected PLOWCODE In/Ex Param! JDH *~
            *          ! PRR 12334. Hide PF3 prompt when not avail!     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim                                                              ~
            base(12),                    /* BASE                       */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            descr$30,                    /* DEDUCTION DESCRIPTION      */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            fac$(20,4)1,                 /* TABLE FACS                 */~
            header$79,                   /* HEADER FOR TABLE           */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            pf16$20,                     /* WHAT PF 16 DOES            */~
            pf2$20,                      /* WHAT PF 2  DOES            */~
            pf3$20,                      /* WHAT PF 3  DOES            */~
            infomessage$79,              /* TABLE MESSAGES             */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            line2$79,                    /* SCREEN LINE                */~
            lastt$60,                    /* LAST TABLE EDITED          */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            maximum(12),                 /* MAXIMUM                    */~
            minimum(12),                 /* MINIMUM                    */~
            percent(12),                 /* PERCENTS                   */~
            taxcode$9,                   /* TAXCODE                    */~
            taxcodedescr$32,             /* TABLE DESCRITION           */~
                                                                         ~
            incl_excl(1),                /* PLOWCODE VARIABLE          */~
            incl_excl$(1)1,              /* PLOWCODE VARIABLE          */~
            descr_map(4),                /* PLOWCODE VARIABLE          */~
            columnhdr$(3)80              /* PLOWCODE VARIABLE          */~

        dim                              /* REPORT VARIABLES           */~
            pmethod$6,                                                   ~
            pdescr$12,                                                   ~
            pempflag$3,                                                  ~
            papplies$6,                                                  ~
            pdescr$(5)15,                                                ~
            pamount(5),                                                  ~
            proutine$8,                                                  ~
            pexempt$3,                                                   ~
            pcdescr$(6)30,                                               ~
            pcamount(6),                                                 ~
            pxdescr$30,                                                  ~
            ptable$3,                                                    ~
            plowfirst$6, firstcode$6,                                    ~
            plowlast$6, lastcode$6,                                      ~
            firstdescr$30, lastdescr$30,                                 ~
            prtline$132,                                                 ~
            ptime$8,                                                     ~
            pname$36,                                                    ~
                                                                         ~
            ptaxcode$7,                                                  ~
            ptaxcodedescr$30,                                            ~
            pminimum(12),                                                ~
            pbase(12),                                                   ~
            ppercent(12),                                                ~
            pmaximum(12),                                                ~
            ptablekey$(500)8,                                            ~
            psrch%(10)

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
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
            * # 1 ! PRLTABLS ! PAYROLL TABLES FILE                      *~
            * # 2 ! PRLDDT   ! SYSTEM DEDUCTION CONTROL FILE            *~
            *************************************************************

            select  #1, "PRLTABLS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 7

            select  #2, "PRLDDT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 400,                                   ~
                        keypos = 1, keylen = 6


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Opening Files To Manage Tax Tables, One Mome~
        ~nt Please")
            call "OPENCHCK" (# 1, 0%, f2%( 1), 100%, " ")
            call "OPENCHCK" (# 2, 0%, f2%( 2),   0%, " ")
            if f2%(2) <> 0 then L65000

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press RETURN."

               header$=                                                  ~
          "                     At Least   But Less Than          Amount ~
        ~   Plus Percent"

            str(line2$,61%) = "PRLTAXTB: " & str(cms2v$,,8%)

            ptablekey% = dim(ptablekey$(), 1)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode1
            init(" ") errormsg$, inpmessage$, infomessage$, taxcode$,    ~
                      taxcodedescr$, pf2$, str(taxcode$,1,1), descr$
            bracket% = 1
            depallow = 0
            mat minimum = zer
            mat maximum = zer
            mat base    = zer
            mat percent = zer
            edit%=0
            call "ALLFREE"

            for fieldnr% = 1 to  4
                gosub'052(fieldnr%)
                      if enabled% =  0 then L10390
L10340:         gosub'102(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  3 and fieldnr% = 1 then printmode
                      if keyhit%  = 16 then L65000
                      if keyhit% <>  0 then       L10340
                gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10340
L10390:         next fieldnr%
        inputlines
            for line% = 1 to bracket%
L10411:     for fieldnr% = 1 to 4
                gosub'053 (line%,fieldnr%)
                     if enabled% =   0 then L10490
L10440:         gosub'103 (line%,fieldnr%)
                     if keyhit%  =  1 then gosub startover
                     if keyhit%  =  2 then L10411
                     if keyhit%  = 16 then L65000
                     if keyhit% <>   0 then       L10440
                gosub'153 (line%,fieldnr%)
                     if errormsg$ <> " " then L10440
L10490:         next fieldnr%
                next line%

L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR LINEAR SCREENS.        *~
            *************************************************************
            edit%=1
L11230:     gosub'112(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit%  =  8 then       deletemode
                  if keyhit%  =  2 then       redo_table
                  if keyhit% <>  0 then       L11230
            fieldnr% = cursor%(1) - 3
            if fieldnr% < 1 or fieldnr% =  2 or fieldnr% = 4 then L11230
            if fieldnr% > 4 then L11400
            if cursor%(2) >= 40 then fieldnr% = fieldnr% + 1

L11310:     gosub'112(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11310
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L11310
            goto L11230

L11400:     fieldnr% = fieldnr% - 4%
            if fieldnr% > bracket% then L11230
            line% = fieldnr%
            fieldnr% = int(cursor%(2)/16)
            if fieldnr% = 0 then L11600
L11450:     gosub'113(line%,fieldnr%)
                if keyhit%  =   1 then gosub startover
                if keyhit%  =   2 then L11600
                if keyhit% <>   0 then L11450
            gosub'153(line%,fieldnr%)
                if errormsg$ <> " " then L11450
            goto L11230

L11600:     for fieldnr% = 1 to 4
L11610:     gosub'113(line%,fieldnr%)
                if keyhit%  =   1 then gosub startover
                if keyhit%  =   2 then L11600
                if keyhit% <>   0 then L11610
            gosub'153(line%,fieldnr%)
                if errormsg$ <> " " then L11610
            next fieldnr%
            goto L11230

        deletemode
            call "READ101" (#1, taxcode$, f1%(1))
            if f1%(1) <> 0 then delete #1
            goto inputmode1

        redo_table
            edit% = 0%
            mat minimum = zer
            mat maximum = zer
            mat base    = zer
            mat percent = zer
            goto inputlines

        REM *************************************************************~
            *       P R I N T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * GETS RANGE OF DEDUCTIONS TO PRINT.                        *~
            *************************************************************

        printmode
            init(" ") errormsg$, lastcode$, firstdescr$, lastdescr$
            firstcode$ = "ALL"
            page% = 0% : prtline% = 1000%
            ptime$ = " " : call "TIME" (ptime$)
            call "COMPNAME" (2%, pname$, f1%(2))

L13120:     mode% = 1%
L13130:     gosub L43000
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       inputmode1
            gosub L53000
                  if errormsg$ <> " " then L13130

            mode% = 0%
            gosub L43000
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <> 16 then       L13120

        REM PLOW ROUTINE FOR PRINTING DDT LISTING.

                call "SHOSTAT" ("Printing Deduction Definition Table")
                gosub L35000              /* LOAD TABLE KEYS            */
L13250:         call "PLOWNEXT" (#2, plowfirst$, 0%, f1%(1))
                     if f1%(1) = 0 then L13320
                     if plowfirst$ > plowlast$ then L13320
                gosub L33000              /* GET DDT CODE               */
                gosub L60000              /* AND PRINT IT.              */
                goto L13250

L13320: REM RETURN FROM ROUTINE

                    if page% > 0% then gosub end_report

                    close printer
                    goto inputmode1

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 2 OF INPUT. *~
            *************************************************************

            deffn'052(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L21100,         /* CODE             */~
                                    L21150,         /* DESCRIPTION      */~
                                    L21250,         /* DEPENDENT ALLOW  */~
                                    L21200          /* BRACKETS         */
                     return

L21100:     REM DEFAULT/ENABLE FOR CODES
                return
L21150:     REM DEFAULT/ENABLE FOR DESCRIPTION
                return
L21200:     REM DEFAULT/ENABLE FOR BRACKETS
                bracket% = 1%
                return
L21250:     REM DEFAULT/ENABLE FOR DEPENDENT ALLOW
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *                TABLE SECTION OF SCREEN                    *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 2 OF INPUT. *~
            *************************************************************

            deffn'053(line%, fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L22130,         /* AT LEAST         */~
                                    L22150,         /* BUT LESS THAN    */~
                                    L22170,         /* AMOUNT           */~
                                    L22190          /* PLUS PERCENT     */
                     return
L22130:     REM DEFAULT/ENABLE FOR AT LEAST
                if line% > 1% then minimum(line%) = maximum(line%-1)
                return
L22150:     REM DEFAULT/ENABLE FOR BUT LESS THAN
                if line% = bracket% then maximum(line%) = 9999999.99
                return
L22170:     REM DEFAULT/ENABLE FOR AMOUNT
                if line%>1 then                                          ~
                    base(line%) = round(base(line%-1) +                  ~
                                  ((maximum(line%-1) - minimum(line%-1)) ~
                                    * percent(line%-1)/100), 2)
                return
L22190:     REM DEFAULT/ENABLE FOR PLUS PERCENT
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
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode1

L30000: REM *************************************************************~
            *                                                           *~
            *              LOAD EXISTING RECORD                         *~
            *                                                           *~
            *************************************************************

            get #1, using L30100, taxcodedescr$, depallow,                ~
                              bracket%, minimum(), base(), percent(),    ~
                              maximum()

            if bracket% = 12 then L30090

                for i% = bracket% + 1% to 12%
                minimum(i%), base(i%), percent(i%), maximum(i%) = 0
                next i%

L30090:     goto L11000

L30100:     FMT      XX(07),             /* CODE                       */~
                     CH(30),             /* DESCRIPTION                */~
                     PD(14,4),           /* DEPENDENT ALLOWANCE        */~
                     BI(4),              /* BRACKETS                   */~
                     12*PD(14,4),        /* MINIMUMS                   */~
                     12*PD(14,4),        /* BASE AMOUNTS               */~
                     12*PD(14,4),        /* PERCENTS                   */~
                     12*PD(14,4),        /* MAXIMUMS                   */~
                     XX(67)              /* FILLER                     */

        REM *************************************************************~
            *                  SAVE DATA ON DISK                        *~
            *                                                           *~
            *************************************************************
        datasave
            call "READ101" (#1, taxcode$, f1%(1))
            put #1, using L31110, taxcode$, taxcodedescr$,                ~
                                 depallow, bracket%,                     ~
                                 minimum(), base(), percent(),           ~
                                 maximum(), " "

            if f1%(1) = 1% then rewrite #1 else write #1
            str(lastt$,1,60) = "Last Table:"
            str(lastt$,14,8) = str(taxcode$,2,6) & ":" &  str(taxcode$,,1)
            str(lastt$,24)   = descr$
            goto inputmode1

L31110:     FMT      CH(07),             /* 'PRLTAXTABLE' & TAXCODE    */~
                     CH(30),             /* DESCRIPTION                */~
                     PD(14,4),           /* DEPENDENT ALLOWANCE        */~
                     BI(4),              /* BRACKETS                   */~
                     12*PD(14,4),        /* MINIMUMS                   */~
                     12*PD(14,4),        /* BASE AMOUNTS               */~
                     12*PD(14,4),        /* PERCENTS                   */~
                     12*PD(14,4),        /* MAXIMUMS                   */~
                     CH(67)              /* FILLER                     */

L33000: REM *************************************************************~
            *                                                           *~
            *  GET PRLDDT RECORD FOR PRINT OUT                          *~
            *                                                           *~
            *************************************************************

            get #2, using L33200,                                         ~
                    pmethod$, pdescr$, pempflag$, papplies$,             ~
                    pdescr$(1), pdescr$(2), pdescr$(3), pdescr$(4),      ~
                    pamount(), proutine$, pexempt$, pcdescr$(),          ~
                    pcamount(), pxdescr$, ptable$

            pdescr$(5) = " "
            if pamount(5) <> 0 then pdescr$(5) = "GOAL"
            if ptable$ = "Y" then ptable$ = "YES" else ptable$ = "NO"
            if pempflag$ = "Y" then pempflag$ = "YES"                    ~
                               else pexempt$ = "NO"
            if pexempt$ = "Y" then pexempt$ = "YES" else pexempt$ = "NO"
            return

L33200:     FMT CH(6),                   /* METHOD OF DEDUCTION        */~
                CH(12),                  /* DEDUCTION DESCRIPTION      */~
                CH(1),                   /* EMPLOYEE PAYS FLAG         */~
                XX(18),                  /* CREDIT, DEBIT ACCOUNT      */~
                CH(6),                   /* APPLIES                    */~
                4*CH(15),                /* DESCRIPTION 1 - 4          */~
                5*PD(14,4),              /* AMOUNT 1 - 4 & GOAL        */~
                CH(8),                   /* ROUTINE NAME               */~
                CH(1),                   /* EXEMPT FLAG                */~
                6*CH(25),                /* CONSTANT DESCRIPTIONS      */~
                6*PD(14,4),              /* CONSTANT AMOUNTS           */~
                CH(30),                  /* extended description       */~
                CH(1)                    /* TAX TABLE REQUIRED?        */~
        /*      XX(19)                   /* FREE SPACE IN DDT RECORD   */~

L34000: REM *************************************************************~
            * READ TAX TABLE FOR PRINT                                  *~
            *                                                           *~
            *************************************************************

            call "READ100" (#1, ptaxcode$, found%)
               if found% = 0% then return
            get #1, using L34150, ptaxcode$, ptaxcodedescr$,              ~
                                 pdepallow, pbracket%,                   ~
                                 pminimum(), pbase(), ppercent(),        ~
                                 pmaximum()

            return

L34150:     FMT      CH(07),             /* 'PRLTAXTABLE' & TAXCODE    */~
                     CH(30),             /* DESCRIPTION                */~
                     PD(14,4),           /* DEPENDENT ALLOWANCE        */~
                     BI(4),              /* BRACKETS                   */~
                     12*PD(14,4),        /* MINIMUMS                   */~
                     12*PD(14,4),        /* BASE AMOUNTS               */~
                     12*PD(14,4),        /* PERCENTS                   */~
                     12*PD(14,4),        /* MAXIMUMS                   */~
                     CH(67)              /* FILLER                     */

L35000: REM *************************************************************~
            * LOAD ARRAY WITH TABLE KEYS                                *~
            *************************************************************

            init (hex(ff)) ptablekey$() : init (hex(00)) ptaxcode$
            pmaxtable% = 0%

            call "PLOWNEXT" (#1, ptaxcode$, 0%, f1%(1))
L35060:         if f1%(1) = 0% then return
                if str(key(#1), 2, 6) < plowfirst$ then L35090
                if str(key(#1), 2, 6) > plowlast$  then L35090
            pmaxtable% = pmaxtable% + 1%
            ptablekey$(pmaxtable%) = key (#1)
              if pmaxtable% >= ptablekey% then return
L35090:     call "READNEXT" (#1, f1%(1))
            goto L35060

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'102(fieldnr%)
                  screen% = 0
                  pf16$ = "(16)Exit Program"
                  pf2$ = " "
                  pf3$ = " "
                  if fieldnr% = 1% then pf3$ = "(3)Print Report"
                  if fieldnr% = 1% then str(line2$,,60) = lastt$         ~
                                   else str(line2$,,60) = " "
                  init(hex(84)) lfac$()
                  init(hex(9c)) fac$()
                  on fieldnr% gosub L41310,         /* CODE             */~
                                    L41280,         /* DESCRIPTION      */~
                                    L41340,         /* BRACKETS         */~
                                    L41340          /* DEP ALLOWANCE    */
                     goto L41700

            deffn'112(fieldnr%)
                  screen% = 1
                  if errormsg$ <> " " then L41220
                  if fieldnr% = 0% then init(hex(86)) lfac$(),fac$()     ~
                                    else init(hex(84)) lfac$(),fac$()
                  init(hex(9c)) str(fac$(),4*min(bracket%,12%)+1%)
                  if fieldnr% = 0% then pf16$ = "(16)SAVE TABLE"         ~
                                     else pf16$ = " "
                  pf2$, pf3$ = " "
                  if fieldnr% = 0% then pf2$ = "(2)Re-enter Table"
                  str(line2$,,60) = " "
L41220:           on fieldnr% gosub L41310,         /* CODE             */~
                                    L41280,         /* DESCRIPTION      */~
                                    L41340,         /* BRACKETS         */~
                                    L41340          /* DEP ALLOWANCE    */
                     goto L41700

L41280:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41310:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L41340:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

            deffn'103(line%, fieldnr%)
                  screen% = 0
                  pf2$ = "(2)Restart Line"
                  pf3$ = " "
                  init(hex(84)) lfac$(), fac$()
                  init (hex(9c)) str(fac$(),4*line%+1)
                  on fieldnr% gosub L41660,         /* AT LEAST         */~
                                    L41660,         /* BUT LESS THAN    */~
                                    L41660,         /* AMOUNT           */~
                                    L41660          /* PLUS PERCENT     */
                     goto L41700

            deffn'113(line%, fieldnr%)
                  screen% = 0
                  init(hex(84)) lfac$(), fac$()
                  init (hex(9c)) str(fac$(),4*min(bracket%,12%)+1%)
                  pf16$ = " "
                  pf2$ = "(2)Restart Line"
                  pf3$ = " "
                  on fieldnr% gosub L41660,         /* AT LEAST         */~
                                    L41660,         /* BUT LESS THAN    */~
                                    L41660,         /* AMOUNT           */~
                                    L41660          /* PLUS PERCENT     */
                     goto L41700

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$(line%,fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(line%,fieldnr%) = hex(81)
                      return
L41660:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      fac$(line%,fieldnr%) = hex(82)
                      return

L41700: accept                                                           ~
               at (01,02),                                               ~
                  "Payroll Tax Tables For:",                             ~
               at (01,26), fac(hex(84)), descr$,                         ~
               at (01,66), "Date:",                                      ~
               at (01,72), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02),                                               ~
                  "Method",                                              ~
               at (04,10), fac(lfac$(1)), str(taxcode$,2)       , ch(06),~
               at (04,18),                                               ~
                  "Status (Table ID)",                                   ~
               at (04,37), fac(lfac$(1)), str(taxcode$,,1)      , ch(01),~
               at (04,40), fac(lfac$(2)), taxcodedescr$         , ch(30),~
               at (05,02), fac(hex(94)) , errormsg$             , ch(79),~
               at (06,40),                                               ~
                  "Number of Brackets",                                  ~
               at (06,65), fac(lfac$(4)), bracket%             , pic(##),~
               at (06,02), "Dependent Allowance",                        ~
               at (06,25), fac(lfac$(3)), depallow     ,pic(-#######.##),~
               at (07,02), fac(hex(ac)) , header$               , ch(79),~
                                                                         ~
               at (08,02), "Bracket  1",                                 ~
               at (09,02), "Bracket  2",                                 ~
               at (10,02), "Bracket  3",                                 ~
               at (11,02), "Bracket  4",                                 ~
               at (12,02), "Bracket  5",                                 ~
               at (13,02), "Bracket  6",                                 ~
               at (14,02), "Bracket  7",                                 ~
               at (15,02), "Bracket  8",                                 ~
               at (16,02), "Bracket  9",                                 ~
               at (17,02), "Bracket 10",                                 ~
               at (18,02), "Bracket 11",                                 ~
               at (19,02), "Bracket 12",                                 ~
                                                                         ~
               at (08,20), fac(fac$( 1,1)), minimum( 1),pic(-#######.##),~
               at (09,20), fac(fac$( 2,1)), minimum( 2),pic(-#######.##),~
               at (10,20), fac(fac$( 3,1)), minimum( 3),pic(-#######.##),~
               at (11,20), fac(fac$( 4,1)), minimum( 4),pic(-#######.##),~
               at (12,20), fac(fac$( 5,1)), minimum( 5),pic(-#######.##),~
               at (13,20), fac(fac$( 6,1)), minimum( 6),pic(-#######.##),~
               at (14,20), fac(fac$( 7,1)), minimum( 7),pic(-#######.##),~
               at (15,20), fac(fac$( 8,1)), minimum( 8),pic(-#######.##),~
               at (16,20), fac(fac$( 9,1)), minimum( 9),pic(-#######.##),~
               at (17,20), fac(fac$(10,1)), minimum(10),pic(-#######.##),~
               at (18,20), fac(fac$(11,1)), minimum(11),pic(-#######.##),~
               at (19,20), fac(fac$(12,1)), minimum(12),pic(-#######.##),~
                                                                         ~
               at (08,52), fac(fac$( 1,3)), base   ( 1),pic(-#######.##),~
               at (09,52), fac(fac$( 2,3)), base   ( 2),pic(-#######.##),~
               at (10,52), fac(fac$( 3,3)), base   ( 3),pic(-#######.##),~
               at (11,52), fac(fac$( 4,3)), base   ( 4),pic(-#######.##),~
               at (12,52), fac(fac$( 5,3)), base   ( 5),pic(-#######.##),~
               at (13,52), fac(fac$( 6,3)), base   ( 6),pic(-#######.##),~
               at (14,52), fac(fac$( 7,3)), base   ( 7),pic(-#######.##),~
               at (15,52), fac(fac$( 8,3)), base   ( 8),pic(-#######.##),~
               at (16,52), fac(fac$( 9,3)), base   ( 9),pic(-#######.##),~
               at (17,52), fac(fac$(10,3)), base   (10),pic(-#######.##),~
               at (18,52), fac(fac$(11,3)), base   (11),pic(-#######.##),~
               at (19,52), fac(fac$(12,3)), base   (12),pic(-#######.##),~
                                                                         ~
               at (08,68), fac(fac$( 1,4)), percent( 1),pic(-#####.####),~
               at (09,68), fac(fac$( 2,4)), percent( 2),pic(-#####.####),~
               at (10,68), fac(fac$( 3,4)), percent( 3),pic(-#####.####),~
               at (11,68), fac(fac$( 4,4)), percent( 4),pic(-#####.####),~
               at (12,68), fac(fac$( 5,4)), percent( 5),pic(-#####.####),~
               at (13,68), fac(fac$( 6,4)), percent( 6),pic(-#####.####),~
               at (14,68), fac(fac$( 7,4)), percent( 7),pic(-#####.####),~
               at (15,68), fac(fac$( 8,4)), percent( 8),pic(-#####.####),~
               at (16,68), fac(fac$( 9,4)), percent( 9),pic(-#####.####),~
               at (17,68), fac(fac$(10,4)), percent(10),pic(-#####.####),~
               at (18,68), fac(fac$(11,4)), percent(11),pic(-#####.####),~
               at (19,68), fac(fac$(12,4)), percent(12),pic(-#####.####),~
                                                                         ~
               at (08,36), fac(fac$( 1,2)), maximum( 1),pic(-#######.##),~
               at (09,36), fac(fac$( 2,2)), maximum( 2),pic(-#######.##),~
               at (10,36), fac(fac$( 3,2)), maximum( 3),pic(-#######.##),~
               at (11,36), fac(fac$( 4,2)), maximum( 4),pic(-#######.##),~
               at (12,36), fac(fac$( 5,2)), maximum( 5),pic(-#######.##),~
               at (13,36), fac(fac$( 6,2)), maximum( 6),pic(-#######.##),~
               at (14,36), fac(fac$( 7,2)), maximum( 7),pic(-#######.##),~
               at (15,36), fac(fac$( 8,2)), maximum( 8),pic(-#######.##),~
               at (16,36), fac(fac$( 9,2)), maximum( 9),pic(-#######.##),~
               at (17,36), fac(fac$(10,2)), maximum(10),pic(-#######.##),~
               at (18,36), fac(fac$(11,2)), maximum(11),pic(-#######.##),~
               at (19,36), fac(fac$(12,2)), maximum(12),pic(-#######.##),~
                                                                         ~
               at (21,02), fac(hex(a4)) ,  infomessage$         , ch(79),~
               at (22,02),                                               ~
           "(1)Start Over                    (8)Delete Record            ~
        ~  (13)Instructions",                                             ~
               at (23,02), fac(hex(8c)), pf2$                   , ch(20),~
               at (23,65), "(15)Print Screen",                           ~
               at (24,02), fac(hex(8c)), pf3$                   , ch(16),~
               at (24,65), fac(hex(84)), pf16$                  , ch(16),~
                                                                         ~
               keys(hex(00010203080d0f10)), key (keyhit%)

               if keyhit% <> 13 then L42670
                  call "MANUAL" ("PRLTAXTB")
                  goto L41700

L42670:        if keyhit% <> 15 then L42710
                  call "PRNTSCRN"
                  goto L41700

L42710:        if screen% = 0 then return
                  close ws
                  call "SCREEN" addr("C", 0%, "I", i$(), cursor%())
                  return

L43000: REM *************************************************************~
            *         S C R E E N   F O R   P R I N T   M O D E         *~
            *                                                           *~
            * SCREEN FOR PRINT MODE -- GETS RANGE THEY WANT PRINTED.    *~
            *************************************************************

            if mode% <> 0% then L43110
               lfac$( 1) = hex(86)
               pf16$ = "(16)Print Report"
               goto L43140

L43110:        lfac$( 1) = hex(81)
               pf16$ = "(16)Rtn to Input"

L43140:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Deduction Methods List with Tables",            ~
               at (01,66),                                               ~
                  "Date:",                                               ~
               at (01,72), fac(hex(8c)),   date$                , ch(08),~
               at (02,02), fac(hex(ac)),   line2$               , ch(79),~
               at (04,02), fac(hex(94)),   errormsg$            , ch(79),~
                                                                         ~
               at (06,02),                                               ~
                  "First Method of Deduction",                           ~
               at (06,30), fac(lfac$( 1)), firstcode$           , ch(06),~
               at (06,40), fac(hex(8c)),   firstdescr$          , ch(30),~
               at (07,02),                                               ~
                  "Last Method of Deduction",                            ~
               at (07,30), fac(lfac$( 1)), lastcode$            , ch(06),~
               at (07,40), fac(hex(8c)),   lastdescr$           , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   infomessage$         , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  fac(hex(8c)), pf16$                           , ch(16),~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

            if keyhit% <> 13 then L43450
                call "MANUAL" ("PRLTAXTB")
                goto L43140

L43450:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

L50000: REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

        REM TEST DATA FOR SELECTION
*         DESCR$ =  HEX(06) & "Use PF(16) to cancel and Exit"
           errormsg$, descr$ =  " "
           f1%(2) = 0%
           incl_excl(1) = 381.01  : incl_excl$(1) = "Y"
           descr_map(1) = 351.30  : descr_map(2)  =  1
           descr_map(3) =   7.12  : descr_map(4)  = 33
           columnhdr$(1) = "  Method     Description                     ~
        ~Check Description"
           columnhdr$(2) = " "
           str(columnhdr$(3), 1%) = "DATE: " & date$
           str(columnhdr$(3),20%) = "Select Method for Table Maintenance"
           str(columnhdr$(3),63%) = "PRLTAXTB: " & str(cms2v$,,8%)

           call "PLOWCODE" (#2,          /* PRLDDT                     */~
                 str(taxcode$,2),                                        ~
                 descr$,                                                 ~
                 9000%,                                                  ~
                 0.30,                                                   ~
                 f1%(2),                                                 ~
                 columnhdr$(),                                           ~
                 0,                                                      ~
                 351,                                                    ~
                 incl_excl(),                                            ~
                 incl_excl$(),                                           ~
                 " ",                                                    ~
                 "N",                                                    ~
                 #2,                                                     ~
                 descr_map())

            if f1%(2) = 0% then L50450
            return

L50450:     errormsg$ = "Select Method for Tax Table Maintenance"
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2.                       *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51200,         /* CODE             */~
                                    L51300,         /* DESCRIPTION      */~
                                    L51400,         /* DEP ALLOWANCE    */~
                                    L51500          /* BRACKETS         */
                  return

L51200: REM TEST DATA FOR CODE
           /*   IF POS("MSHAJ" = STR(TAXCODE$,,1)) <> 0 THEN 51225
                ERRORMSG$="First Character Must be 'S'ingle, 'M'arried, '~
        H'ead of house, 'A'll or 'J'oint :" & STR(TAXCODE$,,1)
                     RETURN    */
                gosub L50000
                if errormsg$ <> " " then return
                if str(taxcode$,1,1) <> " " then L51240
                   errormsg$ = "Status Code cannot be BLANK"
                   return
L51240:         if edit% = 1% then return
                call "READ100" (#1, taxcode$, f1%(1))
                if f1%(1) = 0% then return
                     return clear
                     return clear
                     goto L30000

L51300: REM TEST DATA FOR DESCRIPTION
                return

L51400: REM TEST DATA FOR DEP ALLOWANCE
                depallow = round(depallow, 2)
                if depallow >= 0 then return
                     errormsg$ = "Must be at least zero"
                     return

L51500: REM TEST DATA FOR BRACKETS
                if bracket% > 0% and bracket% < 13% then L51540
                     errormsg$ = "Must have 1 to 12 brackets"
                     return
L51540:         if bracket% > 11% then return
                for i% = bracket% + 1% to 12%
                    minimum(i%), base(i%), percent(i%),maximum(i%) = 0
                next i%
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * TABULAR SECTION OF SCREEN                                 *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2.                       *~
            *************************************************************

            deffn'153(line%,fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L52200,         /* AT LEAST         */~
                                    L52300,         /* BUT LESS THAN    */~
                                    L52400,         /* AMOUNT           */~
                                    L52500          /* PLUS PERCENT     */
                     return

L52200: REM TEST MINIMUM
            minimum(line%) = round(minimum(line%), 2)
            if minimum(line%) >= 0 then return
                errormsg$ = "Must be greater than or equal to zero"
                return

L52300: REM TEST MAXIMUM
            if line% = bracket% then maximum(line%) = 9999999.99
               maximum(line%) = round(maximum(line%), 2)
            if maximum(line%) >= minimum(line%) then return
                errormsg$ = "Must be greater than or equal to minimum"
                return

L52400: REM TEST BASE
            base(line%) = round(base(line%), 2)
            if base(line%) >= 0 then return
                errormsg$ = "Must be greater than or equal to zero"
                return

L52500: REM TEST PERCENT
            percent(line%) = round(percent(line%), 4)
            if percent(line%) >= 0 then return
                errormsg$ = "Must be greater than or equal to zero"
                return

L53000: REM *************************************************************~
            *       V A L I D A T E   R A N G E   T O   P R I N T       *~
            *                                                           *~
            * VALIDATES THE RANGE TO BE PRINTED.                        *~
            *************************************************************

             firstdescr$, lastdescr$, errormsg$ = " "
        REM Handles Case For "All" Deduction Methods
            if firstcode$ <> "?"   then L53160
               firstdescr$ =  hex(0684) & "Select Code for First Method"
               call "GETCODE" (#2, firstcode$, firstdescr$, 0%, 0, f1%(2))
                   if f1%(2) <> 0% then L53160
                errormsg$ = hex(00)
                return

L53160:     if firstcode$ <> "ALL" then L53210
               init(hex(00)) plowfirst$
               init(hex(ff)) plowlast$
               firstdescr$, lastdescr$, lastcode$ = " "
               return
L53210: REM Handles Case For Single Code
            if lastcode$ <> "?"   then L53300
               lastdescr$ =  hex(0684) & "Select Code for Last Method"
               call "GETCODE" (#2, lastcode$, lastdescr$, 0%, 0, f1%(2))
                   if f1%(2) <> 0% then L53160
                errormsg$ = hex(00)
                return

L53300:     if lastcode$ <> " " then L53330
               lastcode$ = firstcode$

L53330: REM Handles Case For A Range Of Codes
            if lastcode$ < firstcode$ then L53380
               plowfirst$ = firstcode$ addc all(hex(ff))
               plowlast$  = lastcode$
               firstdescr$, lastdescr$ = " "
               call "GETCODE" (#2, firstcode$, firstdescr$, 0%, 99, f%)
               call "GETCODE" (#2, lastcode$ , lastdescr$ , 0%, 99, f%)
               return
L53380: REM Handles Error Message -- Last < First.
            errormsg$ = "ILLEGAL RANGE!  Please Respecify."
            return

L60000: REM *************************************************************~
            *                                                           *~
            * PRINT REPORT                                              *~
            *                                                           *~
            *************************************************************

            most% = 6%
L60070:     if pcdescr$(most%) <> " " then L60130
            if most% = 6% then L60100
            if pdescr$(most%)  <> " " then L60130
L60100:        most% = most% - 1%
               goto L60070

L60130:     if prtline% + 4% + most% + (4% * sgn(most%)) < 60% then L60160
               gosub print_header

L60160:     print using L64000, pmethod$, pxdescr$, pdescr$
            print skip(1%)
            print using L64010, pempflag$, pexempt$, papplies$
            print using L64030, proutine$, ptable$
            prtline% = prtline% + 4%
            if most% = 0% then L60470

            print skip(1%)
            print using L64040
            print using L64050
            print using L64055
            prtline% = prtline% + 4%

            empc% = 0% : sysc% = 0%

L60310:     init (" ") prtline$
            if empc% >= 5% then L60370
               empc% = empc% + 1%
               if pdescr$(empc%) = " " then L60370
                  put str(prtline$,1,40) using L64060, pdescr$(empc%),    ~
                                                      pamount(empc%)
L60370:     if sysc% >= 6% then L60420
               sysc% = sysc% + 1%
               if pcdescr$(sysc%) = " " then L60420
                  put str(prtline$,36) using L64070, pcdescr$(sysc%),     ~
                                                    pcamount(sysc%)
L60420:     if prtline$ = " " then L60470
            print prtline$
            prtline% = prtline% + 1%
            goto L60310

L60470:     search str(ptablekey$(),2) = str(pmethod$) to psrch%() step 8
            if psrch%(1%) > 0% then L60530
L60490:        print skip(2%)
               prtline% = prtline% + 2%
               return

L60530:     for i% = 1% to 10%
                if psrch%(i%) <> 0% then L60570
                   i% = 11%
                   goto L60610
L60570:         ptaxcode$ = str(ptablekey$(), psrch%(i%), 7%)
                gosub L34000
                   if found% = 0% then L60610
                gosub L60640
L60610:         next i%
            goto L60490

L60640:     if prtline% + 7% + pbracket% < 60% then L60690
               gosub print_header
               print using L64000, pmethod$, pxdescr$, pdescr$
               prtline% = prtline% + 1%

L60690:     print skip(2%)
            print using L64500, str(ptaxcode$,1,1), ptaxcodedescr$
            print using L64510, pdepallow, pbracket%
            print skip(1%)
            print using L64520
            print using L64530
                for j% = 1% to pbracket%
                    print using L64540, j%, pminimum(j%), pmaximum(j%),   ~
                                           pbase(j%), ppercent(j%)
                next j%
            prtline% = prtline% + 7% + pbracket%
            return

        end_report
            print using L64800, date$, ptime$
            return

        print_header
            select printer(134)
            page% = page% + 1%
            print page
            prtline% = 0%

            print using L63000, date$, pname$, str(cms2v$,,8%)
            print using L63010, ptime$, page%
            print skip(2%)
            prtline% = prtline% + 4%
            return

L63000: %DATE:  ########      ####################################  PRLTA~
        ~XTB:  ########
L63010: %TIME:  ########      PAYROLL TAX METHOD AND TABLE LISTING      P~
        ~AGE:  ########

L64000: %METHOD:  ######              ##############################     ~
        ~(############)
L64010: %   EMPLOYEE PAID:  ###       EXEMPT EARNINGS ALLOWED:  ###    AP~
        ~PLIES:  ######
L64030: %   ROUTINE:        ########  TABLE REQUIRED:           ###
L64040: %   EMPLOYEE CONSTANTS              SYSTEM CONSTANTS
L64050: %   DESCRIPTION           AMOUNT    DESCRIPTION                  ~
        ~        AMOUNT
L64055: %   --------------- ------------    -----------------------------~
        ~- ------------
L64060: %   ############### #######.####
L64070: %############################## #######.####

L64500: %                        TABLE TYPE: #   ########################~
        ~######
L64510: %                              DEPENDENT ALLOWANCE:  ########.## ~
        ~  BRACKETS: ##
L64520: %                        AT LEAST    BUT LESS THAN   BASE AMOUNT ~
        ~ PLUS PERCENT
L64530: %                 --  -----------    -------------   ----------- ~
        ~ ------------
L64540: %                 ##  ########.##      ########.##   ########.## ~
        ~   ######.####

L64800: %DATE:  ########     * * * * * * END OF REPORT * * * * * *      T~
        ~IME:  ########

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

            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
