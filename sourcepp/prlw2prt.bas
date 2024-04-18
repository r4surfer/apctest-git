        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   RRRR   L      W   W   222   PPPP   RRRR   TTTTT   *~
            *  P   P  R   R  L      W   W      2  P   P  R   R    T     *~
            *  PPPP   RRRR   L      W   W   222   PPPP   RRRR     T     *~
            *  P      R   R  L      W W W  2      P      R   R    T     *~
            *  P      R   R  LLLLL   W W   22222  P      R   R    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLW2PRT - PRINTS THE W-2S.  PRINTING PARAMETERS MUST HAVE*~
            *            BEEN DEFINED IN 'PRLW2INP'.                    *~
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
            * 12/10/84 ! ORIGINAL                                 ! HEN *~
            * 04/07/89 ! No print if no earnings or deductions    ! RJM *~
            * 01/03/90 ! Changed to Handle all 6 boxes in Box 5   ! MJB *~
            * 01/12/90 ! Added Employers State ID # and changed   ! MJB *~
            *          !  PRLWYYYY record length to 330 Ch.       !     *~
            * 11/26/90 ! 1990 W-2 Format & Fields                 ! KAB *~
            * 12/03/91 ! 1991 W-2 Format & Fields (Medicare Tax   ! JBK *~
            *          !  and base wages).                        !     *~
            * 12/20/91 ! Medicare tax print bug corrected         ! JBK *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            *          !  PRR - 12512 Close #7 to keep Unix Happy.!     *~
            * 10/22/92 ! PRR - 12650 Return to ASKUSER when       ! JBK *~
            *          !  invalid response given.                 !     *~
            * 11/20/92 ! PRLW2MAP replaces SYSFILE2 record.       ! MLJ *~
            * 12/01/93 ! Reformatted W-2 printing to conform with ! JBK *~
            *          !  1993 requirements.                      !     *~
            * 03/24/94 ! SES Correction - Changed channel #7 from ! MLJ *~
            *          !  PRINT to PRINTER.                       !     *~
            * 01/04/95 ! Modified 1st read of work file of        ! JBK *~
            *          !  Supplemental Forms to keep UNIX happy.  !     *~
            * 08/27/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            address1$30,                 /* ADDRESS LINE ONE           */~
            address2$30,                 /* ADDRESS LINE TWO           */~
            aeic$10,                     /* Advance EIC                */~
            box$(8)1,                    /* Boxes on W-2 Form          */~
            box13$(4)1,                  /* Box 13 Identifiers         */~
            box13p$(4)1,                 /* Box 13 Identifiers (Prt)   */~
            box13pa$(4)10,               /* Box 13 Amounts     (Prt)   */~
            box13(4),                    /* Box 13 Amounts             */~
            box14$(2)12,                 /* Box 14 Identifiers         */~
            box14pa$(2)10,               /* Box 14 Amounts     (Prt)   */~
            box14(2),                    /* Box 14 Amounts             */~
            city$30,                     /* CITY (FOR EMPLOYER)        */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            dcass$10,                    /* Dep. Comp. Assistance      */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            empaddr$(3)30,               /* EMPLOYEE ADDRESS           */~
            empname$30,                  /* EMPLOYEES NAME FOR PRINT   */~
            empzip$9,                    /* ZIP (FOR EMPLOYEE ADDRESS) */~
            empzipf$10,                  /* FORMATTED ZIP CODE         */~
            errormsg$35,                 /* FOR SCREEN                 */~
            fedid$10,                    /* FEDERAL EMPLOYER TAX ID NO.*/~
            ficatax$10,                  /* FICA TAX WITHHELD          */~
            ficawages$10,                /* FICA TAX BASE WAGES        */~
            form$2,                      /* WORK VARIABLE              */~
            fringeben$10,                /* FRINGE BENEFITS            */~
            lc$8,      lc2$8,            /* LOCAL ID CODE              */~
            line2$79,                    /* SCREEN LINE 2              */~
            ltax$8,    ltax2$8,          /* LOCAL TAX WITHHELD         */~
            lwages$8, lwages2$8,         /* LOCAL TAX BASE WAGES       */~
            medicaretax$10,              /* Medicare Tax Withheld      */~
            medicarewages$10,            /* Medicare Tax Base Wages    */~
            msg_text$(6)70,              /* TEXT FOR GETPARM SCREEN    */~
            msg$60,                      /* MESSAGE TEXT               */~
            name$30,                     /* EMPLOYER NAME              */~
            nqplan$10,                   /* Non Qualified Plan         */~
            p$67,                        /* WORK VARIABLE              */~
            pfkey$1,                     /* PFKEY                      */~
            pfmask$4,                    /* PFKEY MASK                 */~
            pline$65,                    /* WORK VARIABLE              */~
            prname$8,                    /* W2 EXTRACT FILE NAME       */~
            readkey$100,                 /* WORK VARIABLE              */~
            sid$10, sid2$10,             /* Employers State ID #       */~
            ssn$11,                      /* EMP SOCIAL SECURITY NUMBER */~
            st$2,      st2$2,            /* STATE ID CODE              */~
            state$15,                    /* STATE (FOR ADDRESS)        */~
            stax$8,    stax2$8,          /* STATE TAX WITHHELD         */~
            sort$1,                      /* W-2 sort order             */~
            suppinline$1,                /* Print Supp with Feds?      */~
            swages$9, swages2$9,         /* STATE TAX BASE WAGES       */~
            tax$10,                      /* FEDRL TAX WITHHELD         */~
            tips$10,                     /* TIPS                       */~
            tipsalloc$10,                /* Allocaed Tips              */~
            ttax$10,                     /* FEDRL TAX WITHHELD TOTAL   */~
            twages$10,                   /* FEDRL TAX BASE WAGES TOTAL */~
            tficatax$10,                 /* FICA TAX WITHHELD TOTAL    */~
            tficawages$10,               /* FICA TAX BASE WAGES TOTAL  */~
            ttipsalloc$10,               /* Allocaed Tips              */~
            tmedicaretax$10,             /* Medicare Tax Withheld Total*/~
            tmedicarewages$10,           /* Medicare Tax Base Wages Tot*/~
            taeic$10,                    /* ADVANCE EIC                */~
            ttips$10,                    /* TIPS TOTAL BUCKET          */~
            tnqplan$10,                  /* NON QUALIFIED PLANS        */~
            tdcass$10,                   /* DEP CARE ASSISTANCE        */~
            ttax(2),                     /* FEDRL TAX WITHHELD TOTAL   */~
            twages(2),                   /* FEDRL TAX BASE WAGES TOTAL */~
            tficatax(2),                 /* FICA TAX WITHHELD TOTAL    */~
            tficawages(2),               /* FICA TAX BASE WAGES TOTAL  */~
            ttipsalloc(2),               /* Allocaed Tips              */~
            taeic(2),                    /* ADVANCE EIC                */~
            ttips(2),                    /* TIPS TOTAL BUCKET          */~
            tmedicaretax(2),             /* Medicare Tax Withheld Total*/~
            tmedicarewages(2),           /* Medicare Tax Base Wages Tot*/~
            tnq457(2),                   /* NON QUALIFIED PLANS        */~
            tnqn457(2),                  /* NON QUALIFIED PLANS        */~
            tdcass(2),                   /* DEP CARE ASSISTANCE        */~
            tfringeben(2),               /* FRINGE BENEFITS            */~
            tbox13(14,2),                /* BOX 13 TOTALS              */~
            tot_box13$10,                /* Total for Box13 Pension $  */~
            w2year$4,                    /* YEAR SELECTED              */~
            wages$10,                    /* FEDRL TAX BASE WAGES       */~
            w$(4)32,                     /* WORK VARIABLE              */~
            zip$10                       /* ZIP CODE                   */~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            fs%(16)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * #1  ! PRLW2MAP ! Payroll W-2 Wage, Tax & Deduction Map    *~
            * #3  ! PRLWYYYY ! W-2 Data Extract File                    *~
            * #7  ! PRINTER  ! Print File For Federal W-2               *~
            * #8  ! PRINT1   ! Print File For Left over state & locals  *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "PRLW2MAP",                                      ~
                        varc,  indexed,  recsize = 1300,                 ~
                        keypos =  1,   keylen =   11

            select #3,  "PRLWYYYY",                                      ~
                        varc, indexed, recsize = 500,                    ~
                        keypos = 1, keylen = 13,                         ~
                        alt key  1, keypos = 40, keylen = 12,            ~
                            key  2, keypos = 14, keylen = 38

            select #7,  "PRINTER",                                       ~
                        printer,  recsize = 67

            select #8,  "WORK",                                          ~
                        consec,   recsize = 67


        REM Check to See if Payroll/Personnel is Active...
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 0%, rslt$(1%))

L02720:     pfmask$ = hex(00010000)  :  pfkey$ = "@"
            msg_text$(1) = hex(84) & "Enter the Reporting" &             ~
                           " Year of the Extract file to be used"
            msg_text$(2) = " "
            msg_text$(3) = hex(8420202020)
            msg_text$(4) = hex(842020202020)
            msg_text$(5) = " "
            msg_text$(6) = "Press 'RETURN' to Continue, or PF-16 to EXIT"
            str(msg_text$(1),69) = hex(0d)
            str(msg_text$(2),69) = hex(0d)
            str(msg_text$(3),69) = hex(0d)
            str(msg_text$(4),69) = hex(0d)
            str(msg_text$(5),69) = hex(0d)

            call "DATEFMT" ( date, 0%, str( w2year$, 1%, 4%) )

L02900:     call "GETPARM" addr("I ", "R", "  Year  ", pfkey$,           ~
                           "W203", "W2PRT ", msg_text$(), 420%,          ~
                            "K", "W2YEAR  ", w2year$, 4%,                ~
                            "A", 14%, "A", 45%, "A",                     ~
                            "T", "W2 Reporting Year ", 17%,              ~
                            "A", 14%, "A", 20%, "P", pfmask$, "E")

            if pfkey$ = "P" then L65000
            if pfkey$ <> "@" then L02900

            prname$ = "PRLW" & str(w2year$)
            call "PUTPRNAM" addr(#3, prname$)

            call "OPENCHCK" (#3, fs%(3), f2%(3),    0%, rslt$(3))
                if fs%(3) = 1% then L09000
L03050:     ask% = 2%
            call "ASKUSER" (ask%, "***** FILE NOT FOUND *****",          ~
                 "The W2 Extract File for " & w2year$ &                  ~
                 " Does Not Exist!",                                     ~
                 "Press PF-16 to EXIT this program or PF-27 to re-enter",~
                 " the Extract Year")

            if ask% = 16% then L65000
            if ask% = 27% then L02720
            goto L03050

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            str(line2$,62) = "PRLW2PRT: " & str(cms2v$,,8)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

            call "READNEXT" (#1, f1%(1%))
                if f1%(1%) <> 0% then L09170
            msg$ = "W-2 Mapping Parameters are undefined."
            goto no_file

L09170:     get #1, using L09200, fedid$, name$, address1$, address2$,     ~
                                city$, state$, zip$
            city$ = city$ & ", " & state$
L09200:     FMT CH(10), XX(1), 3*CH(30), 2*CH(15), CH(10)

            count%, batch%, printed%, f% = 1
            suppinline$ = "N"            /* Print Supp with Feds?      */
            goto L10000

        no_file
            u3% = 0%
            call "ASKUSER" (u3%, "***** FILE ERROR *****", msg$, " ",    ~
                 "Press PF-16 to EXIT this program")
                if u3% <> 16% then no_file
                goto L65000

L10000: REM *************************************************************~
            *                 T E S T   L I N E  U P                    *~
            *                                                           *~
            * Lets user print so test W-2 for form line up.  Also       *~
            * find out the sort order they want... SSN or Name.         *~
            *************************************************************

        accept                                                           ~
               at (01,02), "Payroll W-2 Print Program",                  ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (07,05), fac(hex(94)), errormsg$,                      ~
               at (08,05),                                               ~
        "Do you want the W-2 in last name order or Social Security Number~
        ~ order?",                                                        ~
               at (09,23),                                               ~
        " Type 'N' for Name or 'S' for SSN:",                            ~
               at (09,58), fac(hex(81)), sort$                  , ch(01),~
               at (11,08),                                               ~
        "Type 'Y' To Print Supplemental W-2s With Federal:",             ~
               at (11,58), fac(hex(81)), suppinline$            , ch(01),~
               at (13,23),                                               ~
        " Press PF(1) To print 2 (1 page) test W-2s.",                   ~
               at (14,11),                                               ~
        "When form is lined up OK, Press PF(8), and print will begin",   ~
               at (16,38),                                               ~
        "Or...",                                                         ~
               at (18,16),                                               ~
        "PRESS PF(16) TO EXIT PROGRAM WITHOUT PRINTING W-2s",            ~
            keys(hex(01080d0f10)),                                       ~
            key(keyhit%)

            if keyhit% = 16 then L65000

            if keyhit% <> 1 then L10290
                gosub test_form
                errormsg$ = hex(84) & "Test printed, please check it"
                goto L10000

L10290:     errormsg$ = " "
            if sort$ = "S" or sort$ = "N" then L10340
               errormsg$ = "Please Type 'N' Or 'S' For Print Order"
               goto L10000

L10340:     if suppinline$ = "Y" or suppinline$ = "N" then L10390
L10350:        errormsg$ = "Please Type 'N' Or 'Y' For Supplemental Forms"
               goto L10000

L10390:     if keyhit% <> 8 then L10430
            if sort$ <> "S" then L10410
               sort% = 1% : break% = 11% : goto L11000
L10410:     if sort$ <> "N" then L10350
               sort% = 2% : break% = 37% : goto L11000

L10430:        if keyhit% <> 13 then L10470
                  call "MANUAL" ("PRLW2PRT")
                  goto L10000

L10470:        if keyhit% <> 15 then L10000
                  call "PRNTSCRN"
                  goto L10000

L11000: REM *************************************************************~
            *              M A I N   P R O C E S S I N G                *~
            *                                                           *~
            * Print the W-2s...                                         *~
            *************************************************************

            call "SHOSTAT" ("Printing Payroll W-2 Forms")

            readkey$ = all(hex(00))
L11045:
L11050
*        Read Using Via Chosen Path
            call "PLOWALTS" (#3, readkey$, sort%, 0%, f1%(3))
                if f1%(3) <> 0% then L11205

            if printed% = 1 then L11095
            if count% > 1 then gosub subtotal
            f% = 2
            gosub subtotal

L11095:         REM Move Second 'report' to print file...
                if print_open2% = 0 then L65000
                if print_open1% = 1 then L11122
                call"OPENPRT"(#7,2000%,"        ","        ","      ",x%)
                print_open1% = 1
                gosub test_form    /* Add on extra W-2s */
L11122:         read #8, record = 1, p$, eod goto L65000
                     goto L11130
L11125:         read #8, p$, eod goto L65000
L11130:             write #7, p$
                    goto L11125

*        Get Employee Information
L11205:     get #3, using L11240, ssn$, seqno, empname$, empaddr$(),      ~
                                 empzip$, tax, wages, ficatax, ficawages,~
                                 tips, fringeben, swages, stax, st$,     ~
                                 lwages, ltax, lc$, box$(), sid$,        ~
                                 box13$(), box14$(), medicaretax,        ~
                                 medicarewages, tipsalloc, aeic,         ~
                                 dcass, nq457, nqn457, box14(), box13(), ~
                                 swages2, stax2, st2$, sid2$,            ~
                                 lwages2, ltax2, lc2$

L11240:         FMT XX(12),              /* Employee Code              */~
                    XX(1),               /* Sequence No.               */~
                    XX(26),              /* Last Name                  */~
                    CH(11),              /* SSAN                       */~
                    BI(1),               /* Sequence No.               */~
                    CH(30),              /* Employee Name              */~
                    3*CH(30),            /* Employee Address           */~
                    CH(9),               /* Employee Zip Code          */~
                    6*PD(14,4),          /* Amounts - FIT, Wages,      */~
                                         /*         - FICA, FICA Wages,*/~
                                         /*         - Tips, Fringes    */~
                    2*PD(14,4),          /* State Amounts - Wages, Tax */~
                    CH(2),               /* State Abbreviation         */~
                    2*PD(14,4),          /* Local Amounts - Wages, Tax */~
                    CH(8),               /* Locality                   */~
                    XX(12),              /* Old Box 16 Label  (filler) */~
                    XX(8),               /* Old Deferred Comp (filler) */~
                    8*CH(1),             /* W-2 Check Off Boxes        */~
                    CH(10),              /* State Id Number            */~
                    4*CH(1),             /* Box 17 Identifiers         */~
                    2*CH(12),            /* Box 14 Labels              */~
                    2*PD(14,4),          /* Medicare Amounts-Tax, Wages*/~
                    XX( 2),              /* Filler (character)         */~
                    PD(14,4),            /* Allocated Tips             */~
                    PD(14,4),            /* A.E.I.C.                   */~
                    PD(14,4),            /* Dep Care Assistance        */~
                    PD(14,4),            /* Non Qualified 457          */~
                    PD(14,4),            /* Non Qualified non 457      */~
                    2*PD(14,4),          /* Box 14 Amounts             */~
                    4*PD(14,4),          /* Box 13 Amounts             */~
                    2*PD(14,4),          /* State Amounts - Wages, Tax */~
                    CH(2),               /* State Abbreviation         */~
                    CH(10),              /* State Id Number            */~
                    2*PD(14,4),          /* Local Amounts - Wages, Tax */~
                    CH(8),               /* Locality                   */~
                    XX(6)                /* Filler                     */

        /* Got all the data, let's format and so forth . . . */

            if str(empzip$,6) <> " " then L11510
               str(empzipf$) = empzip$  :  goto L11531
L11510:     str(empzipf$) = str(empzip$,,5)
            str(empzipf$) = str(empzip$,,5)
            str(empzipf$,6) = "-"
            str(empzipf$,7) = str(empzip$,6,4)

L11531:     tax$, wages$, ficatax$, ficawages$, medicaretax$,            ~
                  medicarewages$ = " "

            if tax            <> 0 then                                  ~
               call "CONVERT" (tax,           2.2, tax$)
            if wages          <> 0 then                                  ~
               call "CONVERT" (wages,         2.2, wages$)
            if ficatax        <> 0 then                                  ~
               call "CONVERT" (ficatax,       2.2, ficatax$)
            if ficawages      <> 0 then                                  ~
               call "CONVERT" (ficawages,     2.2, ficawages$)
            if medicaretax    <> 0 then                                  ~
               call "CONVERT" (medicaretax,   2.2, medicaretax$)
            if medicarewages  <> 0 then                                  ~
               call "CONVERT" (medicarewages, 2.2, medicarewages$)

            tipsalloc$, aeic$, tips$, dcass$, fringeben$, nqplan$ = " "

            if tipsalloc > 0 then                                        ~
               call "CONVERT" (tipsalloc, 2.2, tipsalloc$)
            if aeic      > 0 then                                        ~
               call "CONVERT" (aeic     , 2.2, aeic$)
            if tips      > 0 then                                        ~
               call "CONVERT" (tips,      2.2, tips$)
            if dcass     > 0 then                                        ~
               call "CONVERT" (dcass    , 2.2, dcass$)
            if fringeben > 0 then                                        ~
               call "CONVERT" (fringeben, 2.2, fringeben$)

            nq457 = round(nq457, 2) : nqn457 = round(nqn457, 2)
            if (nq457 + nqn457) > 0 then                                 ~
               call "CONVERT" (nq457 + nqn457, 2.2, nqplan$)

            init (" ") box13p$(), box13pa$() : j% = 1%
            if str(box13$()) = " " then L11735
            for i% = 1% to 4%
                if box13$(i%) = " " then L11725
                   box13p$(j%) = box13$(i%)
                   call "CONVERT" (box13(i%), -2.2, box13pa$(j%))
                   j% = j% + 1%
L11725:     next i%

L11735:     init (" ") box14pa$()

            if box14$(1%) <> " " then                                    ~
               call "CONVERT" (box14(1%), 2.2, box14pa$(1%))
            if box14$(2%) <> " " then                                    ~
               call "CONVERT" (box14(2%), 2.2, box14pa$(2%))

            swages$, swages2$, stax$, stax2$ = " "
            lwages$, lwages2$, ltax$, ltax2$ = " "

            if swages > 0 then                                           ~
               call "CONVERT" (swages, 2.2, swages$)
            if swages2 > 0 then                                          ~
               call "CONVERT" (swages2, 2.2, swages2$)
            if stax > 0 then                                             ~
               call "CONVERT" (stax, 2.2, stax$)
            if stax2 > 0 then                                            ~
               call "CONVERT" (stax2, 2.2, stax2$)
            if lwages > 0 then                                           ~
               call "CONVERT" (lwages, 2.2, lwages$)
            if lwages2 > 0 then                                          ~
               call "CONVERT" (lwages2, 2.2, lwages2$)
            if ltax > 0 then                                             ~
               call "CONVERT" (ltax, 2.2, ltax$)
            if ltax2 > 0 then                                            ~
               call "CONVERT" (ltax2, 2.2, ltax2$)

            if suppinline$ = "Y" then L11895
            if seqno > 1 then goto supplemental_w2
L11895:     if seqno <= 1 then L12000
*             INIT (" ") TAX$, FICATAX$, WAGES$, FICAWAGES$,            ~
*                        MEDICARETAX$, MEDICAREWAGES$
*                        TAX, FICATAX, WAGES, FICAWAGES = 0
*                        MEDICARETAX, MEDICAREWAGES = 0

L12000
*        Print Logic For Federal Destined W2's
            if print_open1% = 1 then L12050
            call "OPENPRT"(#7,5000%,"        ","        ","      ",x%)
            print_open1% = 1

L12050:     convert batch% to temp$, pic(00)
            convert count% to str(temp$,4%,3%), pic(000)
            convert printed% to str(temp$,8%,4%), pic(0000)
            str(temp$,3%,1%), str(temp$,7%,1%) = "-"

            put p$, using L17010, temp$, " "
                gosub'181(7%, 1%, p$)
            put p$, using L17020, fedid$, wages$, tax$
                gosub'181(7%, 2%, p$)
            put p$, using L17030, name$, ficawages$, ficatax$
                gosub'181(7%, 2%, p$)
            put p$, using L17040, address1$
                gosub'181(7%, 1%, p$)
            put p$, using L17050, address2$, medicarewages$, medicaretax$
                gosub'181(7%, 1%, p$)
            put p$, using L17060, city$
                gosub'181(7%, 1%, p$)
            put p$, using L17070, zip$, tips$, tipsalloc$
                gosub'181(7%, 1%, p$)
            put p$, using L17080, ssn$, aeic$, dcass$
                gosub'181(7%, 2%, p$)
            put p$, using L17090, empname$, nqplan$, fringeben$
                gosub'181(7%, 2%, p$)
            put p$, using L17100, empaddr$(1%)
                gosub'181(7%, 1%, p$)
            put p$, using L17110, empaddr$(2%), box13p$(1%), box13pa$(1%),~
                                              box14$  (1%)
                gosub'181(7%, 1%, p$)
            put p$, using L17120, empaddr$(3%), box13p$(2%), box13pa$(2%),~
                                              box14pa$(1%)
                gosub'181(7%, 1%, p$)
            put p$, using L17130, empzipf$,    box13p$(3%), box13pa$(3%), ~
                                              box14$  (2%)
                gosub'181(7%, 1%, p$)
            put p$, using L17135,              box13p$(4%), box13pa$(4%), ~
                                              box14pa$(2%)
                gosub'181(7%, 1%, p$)
            put p$, using L17137, box$(1%), box$(2%), box$(3%), box$(4%), ~
                                 box$(5%), " ", box$(7%)
                gosub'181(7%, 2%, p$)
            put p$, using L17140, st$, sid$, swages$, stax$,              ~
                                 lc$,       lwages$, ltax$
                gosub'181(7%, 2%, p$)
            put p$, using L17150, st2$, sid2$, swages2$, stax2$,          ~
                                 lc2$,      lwages2$, ltax2$
                gosub'181(7%, 2%, p$)
            put p$, using L17160, " "
                gosub'181(7%, 9%, p$)

        /* Now to accumulate for the next level of subtotals . . .  */
            ttax          (1%) = ttax          (1%) + tax
            twages        (1%) = twages        (1%) + wages
            tficatax      (1%) = tficatax      (1%) + ficatax
            tficawages    (1%) = tficawages    (1%) + ficawages
            ttipsalloc    (1%) = ttipsalloc    (1%) + tipsalloc
            taeic         (1%) = taeic         (1%) + aeic
            ttips         (1%) = ttips         (1%) + tips
            tnq457        (1%) = tnq457        (1%) + nq457
            tnqn457       (1%) = tnqn457       (1%) + nqn457
            tdcass        (1%) = tdcass        (1%) + dcass
            tfringeben    (1%) = tfringeben    (1%) + fringeben
            tmedicaretax  (1%) = tmedicaretax  (1%) + medicaretax
            tmedicarewages(1%) = tmedicarewages(1%) + medicarewages

            for i% = 1% to 4%
                if box13$(i%) = " " then L13165
                if box13 (i%) <=  0 then L13165
                   j% = val(box13$(i%),1) - 64%
                   if j% < 1% or j% > 14% then L13165
                      tbox13(j%,1%) = tbox13(j%,1%) + box13(i%)
L13165:     next i%

            count% = count% + 1
            printed% = printed% + 1
            if count% = 42 then gosub subtotal

            goto L11050

        supplemental_w2
            if print_open2% = 1 then L14050
            call "WORKOPEN" (#8, "IO", 2000%, f2%(8))
            print_open2% = 1

L14050:     put p$, using L17010, " ", "X"
                gosub'181(8%, 1%, p$)
            put p$, using L17020, fedid$, wages$, tax$
                gosub'181(8%, 2%, p$)
            put p$, using L17030, name$, ficawages$, ficatax$
                gosub'181(8%, 2%, p$)
            put p$, using L17040, address1$
                gosub'181(8%, 1%, p$)
            put p$, using L17050, address2$, medicarewages$, medicaretax$
                gosub'181(8%, 1%, p$)
            put p$, using L17060, city$
                gosub'181(8%, 1%, p$)
            put p$, using L17070, zip$, tips$, tipsalloc$
                gosub'181(8%, 1%, p$)
            put p$, using L17080, ssn$, aeic$, dcass$
                gosub'181(8%, 2%, p$)
            put p$, using L17090, empname$, nqplan$, fringeben$
                gosub'181(8%, 2%, p$)
            put p$, using L17100, empaddr$(1%)
                gosub'181(8%, 1%, p$)
            put p$, using L17110, empaddr$(2%), box13p$(1%), box13pa$(1%),~
                                              box14$  (1%)
                gosub'181(8%, 1%, p$)
            put p$, using L17120, empaddr$(3%), box13p$(2%), box13pa$(2%),~
                                              box14pa$(1%)
                gosub'181(8%, 1%, p$)
            put p$, using L17130, empzipf$,    box13p$(3%), box13pa$(3%), ~
                                              box14$  (2%)
                gosub'181(8%, 1%, p$)
            put p$, using L17135,              box13p$(4%), box13pa$(4%), ~
                                              box14pa$(2%)
                gosub'181(8%, 1%, p$)
            put p$, using L17137, box$(1%), box$(2%), box$(3%), box$(4%), ~
                                 box$(5%), " ", box$(7%)
                gosub'181(8%, 2%, p$)
            put p$, using L17140, st$, sid$, swages$, stax$,              ~
                                 lc$,       lwages$, ltax$
                gosub'181(8%, 2%, p$)
            put p$, using L17150, st2$, sid2$, swages2$, stax2$,          ~
                                 lc2$,      lwages2$, ltax2$
                gosub'181(8%, 2%, p$)
            put p$, using L17160, " "
                gosub'181(8%, 9%, p$)

            goto L11045

        REM **************************************************************
        REM                       PRINT SUB TOTALS
        REM **************************************************************

        subtotal
            p$ = " "
            box$() = " "
            y% = mod(count%,2%)
            if f% = 2 then L15130

            if printed% < 42% then subtotal2
            if y% = 1% then gosub'181(7%, 33%, p$) /* To Bottom of Form */
            empname$ = "T O T A L"
            box$(6) = "X"
            goto L15170

L15130:     if y% = 0% then gosub'181(7%, 33%, p$) /* To Top of Form */
            empname$ = "G R A N D    T O T A L"
            box$(8) = "X"

L15170:     call "CONVERT" (ttax      (f%), 2.2, ttax$)
            call "CONVERT" (twages    (f%), 2.2, twages$)
            call "CONVERT" (tficatax  (f%), 2.2, tficatax$)
            call "CONVERT" (tficawages(f%), 2.2, tficawages$)
            call "CONVERT" (ttipsalloc(f%), 2.2, ttipsalloc$)
            call "CONVERT" (taeic     (f%), 2.2, taeic$)
            call "CONVERT" (ttips     (f%), 2.2, ttips$)
            call "CONVERT" (tdcass    (f%), 2.2, tdcass$)
            tnqplan = tnq457(f%) + tnqn457(f%)
            call "CONVERT" (tnqplan, 2.2, tnqplan$)
            call "CONVERT" (tmedicarewages(f%), 2.2, tmedicarewages$)
            call "CONVERT" (tmedicaretax(f%), 2.2, tmedicaretax$)

            init(" ") box13p$(), box13pa$() : j% = 1%
            init(" ") tot_box13$  :  tot_box13 = 0
            for i% = 4% to 8%
*              IF I% = 7% THEN 15242
                if tbox13(i%, f%) <= 0% then L15242
                   tot_box13 = tot_box13 + tbox13(i%,f%)
*                 BOX13P$(J%) = BIN(64% + I%, 1)
*                 CALL "CONVERT" (TBOX13(I%,F%), -2.2, BOX13PA$(J%))
*                 J% = J% + 1%
                   call "CONVERT" (tot_box13, -2.2, tot_box13$)
L15242:     next i%

            convert batch% to temp$, pic(00)
            convert count% to str(temp$,4%,3%), pic(000)
            str(temp$,3%,1%), str(temp$,7%,5%) = "-0000"

            put p$, using L17010, temp$, box$(8%)
                gosub'181(7%, 1%, p$)
            put p$, using L17020, fedid$, twages$, ttax$
                gosub'181(7%, 2%, p$)
            put p$, using L17030, name$, tficawages$, tficatax$
                gosub'181(7%, 2%, p$)
            put p$, using L17040, address1$
                gosub'181(7%, 1%, p$)
            put p$, using L17050, address2$, tmedicarewages$, tmedicaretax$
                gosub'181(7%, 1%, p$)
            put p$, using L17060, city$
                gosub'181(7%, 1%, p$)
            put p$, using L17070, zip$, ttips$, ttipsalloc$
                gosub'181(7%, 1%, p$)
            put p$, using L17080, " ", taeic$, tdcass$
                gosub'181(7%, 2%, p$)
            put p$, using L17090, empname$, tnqplan$, " "
                gosub'181(7%, 2%, p$)
            put p$, using L17100, " "
                gosub'181(7%, 1%, p$)
            put p$, using L17110, " "         , " ", tot_box13$, " "
                gosub'181(7%, 1%, p$)
            put p$, using L17120, " "         , " ", " ", " "
                gosub'181(7%, 1%, p$)
            put p$, using L17130, " "         , " ", " ", " "
                gosub'181(7%, 1%, p$)
            put p$, using L17135,               " ", " ", " "
                gosub'181(7%, 1%, p$)
            put p$, using L17137, " ", " ", " ", " ", " ", box$(6%), " "
                gosub'181(7%, 2%, p$)
            put p$, using L17160, " "
                gosub'181(7%,13%, p$)

            count% = 1
        subtotal2
            batch% = batch% + 1
            box$() = " "
            if f% = 1% then L15640
               gosub'181(7%, 33%, p$)  /* F% = 2%, Advance form */
               return

L15640:     ttax          (2%) = ttax          (2%) + ttax      (1%)
            twages        (2%) = twages        (2%) + twages    (1%)
            tficatax      (2%) = tficatax      (2%) + tficatax  (1%)
            tficawages    (2%) = tficawages    (2%) + tficawages(1%)
            ttipsalloc    (2%) = ttipsalloc    (2%) + ttipsalloc(1%)
            taeic         (2%) = taeic         (2%) + taeic     (1%)
            ttips         (2%) = ttips         (2%) + ttips     (1%)
            tdcass        (2%) = tdcass        (2%) + tdcass    (1%)
            tnq457        (2%) = tnq457        (2%) + tnq457    (1%)
            tnqn457       (2%) = tnqn457       (2%) + tnqn457   (1%)
            tfringeben    (2%) = tfringeben    (2%) + tfringeben(1%)
            tmedicaretax  (2%) = tmedicaretax  (2%) + tmedicaretax(1%)
            tmedicarewages(2%) = tmedicarewages(2%) + tmedicarewages(1%)

            ttax          (1%) = 0
            twages        (1%) = 0
            tficatax      (1%) = 0
            tficawages    (1%) = 0
            ttipsalloc    (1%) = 0
            taeic         (1%) = 0
            ttips         (1%) = 0
            tdcass        (1%) = 0
            tnq457        (1%) = 0
            tnqn457       (1%) = 0
            tfringeben    (1%) = 0
            tmedicaretax  (1%) = 0
            tmedicarewages(1%) = 0

            for i% = 1% to 14%
                tbox13(i%,2%) = tbox13(i%,2%) + tbox13(i%,1%)
                tbox13(i%,1%) = 0
            next i%

            return

        REM **************************************************************
        REM                       PRINT TEST FOR LINE UP
        REM **************************************************************

        test_form

            if print_open1% = 1 then L16090
            call "OPENPRT"(#7,100%,"        ","        ","      ",x%)

L16090:     w$() = all ("X")
            for i% = 1 to 2
            put p$, using L17010, w$(1%), "X"
                gosub'181(7%, 1%, p$)                     /* Control # */
            put p$, using L17020, w$(1%), w$(2%), w$(3%)
                gosub'181(7%, 2%, p$)                     /* Empr ID   */
            put p$, using L17030, w$(1%), w$(2%), w$(3%)
                gosub'181(7%, 2%, p$)                     /* Empr Name */
            put p$, using L17040, w$(1%)
                gosub'181(7%, 1%, p$)                     /* Empr Add 1*/
            put p$, using L17050, w$(1%), w$(2%), w$(3%)
                gosub'181(7%, 1%, p$)                     /* Empr Add 2*/
            put p$, using L17060, w$(1%)
                gosub'181(7%, 1%, p$)                     /* Empr Add 3*/
            put p$, using L17070, w$(1%), w$(2%), w$(3%)
                gosub'181(7%, 1%, p$)                     /* Empr Zip  */
            put p$, using L17080, w$(1%), w$(2%), w$(3%)
                gosub'181(7%, 2%, p$)                     /* Empe SS # */
            put p$, using L17090, w$(1%), w$(2%), w$(3%)
                gosub'181(7%, 2%, p$)                     /* Empe Name */
            put p$, using L17100, w$(1%)
                gosub'181(7%, 1%, p$)                     /* Empe Add 1*/
            put p$, using L17110, w$(1%), w$(2%), w$(3%), w$(4%)
                gosub'181(7%, 1%, p$)                     /* Empe Add 2*/
            put p$, using L17120, w$(1%), w$(2%), w$(3%), w$(4%)
                gosub'181(7%, 1%, p$)                     /* Empe Add 3*/
            put p$, using L17130, w$(1%), w$(2%), w$(3%), w$(4%)
                gosub'181(7%, 1%, p$)                     /* Empe Zip  */
            put p$, using L17135, w$(1%), w$(2%), w$(3%)
                gosub'181(7%, 1%, p$)                     /* Last 13,14*/
            put p$, using L17137, "X", "X", "X", "X", "X", "X", "X"
                gosub'181(7%, 2%, p$)                     /* Box 15    */
            put p$, using L17140, w$(1%), w$(2%), w$(3%), w$(1%), w$(2%), ~
                                                         w$(3%), w$(4%)
                gosub'181(7%, 2%, p$)                     /* St & Loc 1*/
            put p$, using L17150, w$(1%), w$(2%), w$(3%), w$(1%), w$(2%), ~
                                                         w$(3%), w$(4%)
                gosub'181(7%, 2%, p$)                     /* St & Loc 2*/
            put p$, using L17160, " "
                gosub'181(7%, 9%, p$)
            next i%
            if print_open1% = 0 then close #7
        return

        %....!....1....!....2....!....3....!....4....!....5....!....6....
L17010: % ###########         #
L17020: % ##########                            ##########     ##########
L17030: % ###############################       ##########     ##########
L17040: % ###############################
L17050: % ###############################       ##########     ##########
L17060: % ###############################
L17070: %                      ##########       ##########     ##########
L17080: % ###########                           ##########     ##########
L17090: % ###############################       ##########     ##########
L17100: % ###############################
L17110: % ###############################     # ##########   ############
L17120: % ###############################     # ##########     ##########
L17130: %                      ##########     # ##########   ############
L17135: %                                     # ##########     ##########
L17137: %                                   #   #   #   #   #   #   #
L17140: % ##  ##########    ######### ######## ######## ######## ########
L17150: % ##  ##########    ######### ######## ######## ######## ########
L17160: %  #

        REM *************************************************************~
            *                F O R M   C O N T R O L                    *~
            *                                                           *~
            * WRITE FORM ADVANCE, PRINT DATA...                         *~
            *************************************************************

        deffn'181(x%, skip%, pline$) /*If Negitive, Skip AFTER Printing*/
            str(form$,,1) = hex(00)
            if skip% < 0% then str(form$,,1) = hex(40)
            str(form$,2) = bin(skip%,1)

            write #x% using L18120, form$, pline$
L18120:     FMT CH(2), CH(65)
            p$ = " "
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
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("Closing Files, One Moment Please")

            if print_open1% = 1% then close #7


            end
