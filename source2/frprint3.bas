        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  FFFFF  RRRR   PPPP   RRRR   IIIII  N   N  TTTTT   333    *~
            *  F      R   R  P   P  R   R    I    NN  N    T        3   *~
            *  FFF    RRRR   PPPP   RRRR     I    N N N    T     333    *~
            *  F      R  R   P      R  R     I    N  NN    T        3   *~
            *  F      R   R  P      R   R  IIIII  N   N    T     333    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FRPRINT3 - Prints a Profit & Loss Type Report, using user *~
            *            selected data as defined in the report format  *~
            *            specification files. Optional comparisons can  *~
            *            be made to previous year, previous period, or  *~
            *            budget. Percentages are printed as defined in  *~
            *            the format specification files.                *~
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
            * 02/25/85 ! ORIGINAL (Re-write)                      ! HES *~
            * 03/04/86 ! Changes due to Fiscal Dates reformat     ! ERN *~
            * 04/17/86 ! Added option to select any period        ! HES *~
            * 10/13/87 ! W.R. Grace accountancy (dual books) mods.! JIM *~
            * 08/15/88 ! Dual Books depends on a SYSFILE2 flag.   ! JIM *~
            * 08/09/89 ! Fixed "Print Line When Zero" option to   ! MLJ *~
            *          !  print correctly when "NO" is specified. !     *~
            * 09/26/91 ! PRR 11232 TEXT$ expanded to 45 characters! JIM *~
            * 10/28/91 ! Added Prefious/Next Year Budget Exposure ! MLJ *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 05/04/92 ! Chg'd #12130 to FILEBGON, REM'd #30320.  ! MLJ *~
            * 03/01/93 ! Modified to support period range         ! MLJ *~
            *          !   selection, standardized STARTOVER,     !     *~
            *          !   fixed implied integers, subtitles ("H")!     *~
            *          !   now always print in column 1.          !     *~
            * 11/04/93 ! Corrected period range description. Now  ! MLJ *~
            *          !   specifies correct date regardless of   !     *~
            *          !   System Date format used (A, S or E).   !     *~
            * 02/24/94 ! Added ASKUSER to determine starting used ! MLJ *~
            *          !  used for YTD calcs when reporting period!     *~
            *          !  >=14.                                   !     *~
	    * 06/14/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**~

        dim                                                              ~
            account$9,                   /* WORK VARIABLE              */~
            acct1$(320)9,                /* FROM ACCOUNT               */~
            acct2$(320)9,                /* TO ACCOUNT                 */~
            base$(320)3,                 /* FROM ACCOUNT               */~
            col$(320)1,                  /* COLUMN INDICATOR           */~
            comp$(320)3,                 /* COMPANY (DBASE POINTER)    */~
            compare$2,                   /* COMPARE FLAG               */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            descr$45,                    /* REPORT DESCRIPTION         */~
            dual_books$1,                /* Dual books in effect?      */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            filler$12,                   /* FILLER AT END OF RECORD    */~
            fixed$(320)3,                /* RUN TIME OVERRIDE FLAG     */~
            group$(320)6,                /* ACCOUNT GROUPING CODE      */~
            header$79,                   /* HEADER FOR SCREEN          */~
            header1$79,                  /* HEADER FOR SCREEN          */~
            hfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            ieflag$(4)1,                 /* INCLUDE/EXCLUDE FLAGS      */~
            iemesg$(4)7,                 /* INCLUDE/EXCLUDE MEMO       */~
            inst$(320)2,                 /* INSTRUCTION CODE           */~
            inst_codes$30,               /* VALID INSTRUCTIONS         */~
            line$132,                    /* PRINT LINE TO WRITE TO WRKF*/~
            line1$132,                   /* WORK VARIABLE              */~
            message$79,                  /* INPUT MESSAGE              */~
            memo$(320)70,                /* JUNK TEXT                  */~
            month1$2,                    /* STARTING G/L PERIOD        */~
            month2$2,                    /* ENDING G/L PERIOD          */~
            months$64,                   /* MONTHS LIST AVAILABLE      */~
            mdate$32,                    /* DATES OF AVAILABLE MONTHS  */~
            mdescr$32,                   /* DATES OF AVAILABLE MONTHS  */~
            mfac$(3)1,                   /* FIELD ATTRIBUTE CHARACTERS */~
            p(4),                        /* WORK VARIABLE              */~
            p$(4)15,                     /* WORK VARIABLE              */~
            p1(4),                       /* WORK VARIABLE              */~
            pdescr$32,                   /* PERIOD RANGE DESCRIPTION   */~
            pp$(4)8,                     /* WORK VARIABLE              */~
            paren$3,                     /* DEFAUT PARENTHESIS OPTION  */~
            pfdescr$(3)79,               /* DESCRIPTION OF PFKEYS      */~
            print_if$(320)1,             /* PRINT/TOTAL CONSTRAINT FLAG*/~
            print$(320)3,                /* PRINT CODE                 */~
            readkey$60,                  /* WORK VARIABLE              */~
            record$(33)18,               /* DATA RETRIEVED FROM SUB    */~
            report$3,                    /* REPORT ID.                 */~
            reportheader$50,             /* FLOATING REPORT HEADER     */~
            reverse$(320)3,              /* REVERSE SIGN INDICATOR     */~
            round$3,                     /* ROUND OPTION (DEFUALT)     */~
            rptid$6,                                                     ~
            set$1, setdescr$30, sethdr$60,/* Set of books to use       */~
            setmsg$11, set2$1,           /* Screen message for SET     */~
            temp1$8,                     /* Temporary date area        */~
            temp2$8,                     /* Temporary date area        */~
            tmpbal(4),                   /* NUMBERS TO PRINT           */~
            tmpbal$(4)15,                /* NUMBERS TO PRINT, FORMATTED*/~
            text$(320)45,                /* TEXT FOR LINE ITEMS        */~
            tfac$1,                      /* FIELD ATTRIBUTE CHARACTERS */~
            title$50,                    /* DEFAULT REPORT TITLE       */~
            total$(320)10,               /* TOTALING SPEC FOR LINE     */~
            totals(10,33),               /* REPORT TOTALS ACCUMULATOR  */~
            zero$3,                      /* PRINT IF ZERO? (DEFUALT)   */~
            z$(4)35,                     /* ZONE DESCR FOR PRINT       */~
            zone%(4,2),                  /* START, LENGTH IN ACCOUNT   */~
            zone$(4,2)2,                 /* START, LENGTH IN ACCOUNT   */~
            zonename$(4)20,              /* LABLE FOR ZONE IN ACCOUNT  */~
            zonespec1$(16,1)1,           /* USERS SELECTION FOR ZONE 1 */~
            zonespec2$(16,1)1,           /* USERS SELECTION FOR ZONE 2 */~
            zonespec3$(16,1)1,           /* USERS SELECTION FOR ZONE 3 */~
            zonespec4$(16,1)1            /* USERS SELECTION FOR ZONE 4 */

        dim f1%(64)                      /* = 1 IF READ WAS SUCCESSFUL */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! FRNAMES  ! Financial Statement Header File          *~
            * #02 ! FRFORMAT ! Financial Statement Format File          *~
            * #03 ! SYSFILE2 ! SYSTEM INFORMATION (WHICH MONTHS OPEN)   *~
            * #08 ! WORKFILE ! WORKFILE FOR PRELIMINARY OUTPUT          *~
            * #09 ! WORKFILE ! WORKFILE FOR FRACCUM RETURNED DATA       *~
            *************************************************************

            select #01,  "FRNAMES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos = 1, keylen = 3

            select #02,  "FRFORMAT",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos = 1, keylen = 8

            select  #03, "SYSFILE2",                                     ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select  #08, "GILBERT",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 139,                                   ~
                        keypos = 1, keylen = 2,                          ~
                        alt key 1, keypos = 3, keylen = 2, dup

            select  #09, "MARY",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 680,                                   ~
                        keypos = 1, keylen = 16

            call "SHOSTAT" ("Opening files, one moment please.")

            call "OPENCHCK" (#01, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#02, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#03, 0%, 0%, 0%, " ")
            call "WORKOPEN" (#09, "IO   ", 100%, 0%)

            dual_books$ = "N"                        /* Default to 'no' */
            call "READ100" (#03, "SWITCHS.GL", f1%(3%))
                if f1%(3%) = 0% then goto L09000
            get #03 using L02550, dual_books$
L02550:         FMT POS(21), CH(1)

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            if dual_books$ = "Y" then setmsg$ = "G/L System:"
            date$ = date
            call "DATEFMT" (date$)
            f28% = 1

        REM List of valid instructions codes...
            inst_codes$ = "H P D DCU DUPACBPOYOYAT "
            rptid$ = "FRP003"

        REM Format Dates For Period Display Generation...
            months$ = "Jan.Feb.Mar.Apr.May Jun.Jul.Aug.Sep.Oct.Nov.Dec."&~
                      "Dec.Jan.Feb.Mar."

                call "READ100" (#03, "FISCAL DATES", f1%(3%))
                     if f1%(3%) <> 0% then L09210
                        u3% = 0%
                        call "ASKUSER" (u3%, "*** FISCAL DATE ERROR ***",~
                        "Fiscal dates not found in file SYSFILE2", " ",  ~
                        "Press (RETURN) to cancel program")
                       goto L65000
L09210:      get #03, using L09220, periods%
L09220:      FMT XX(20), BI(2)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            gosub L29000  /* Clear Variables For Input */
            maxlines% = 0%

            for fieldnr% = 1% to 10%
                gosub'051(fieldnr%)
                      if enabled% = 0% then L10240
L10130:         gosub'101(1%, fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10210
L10160:                   if fieldnr% < 2%  then L10130
                          fieldnr% = fieldnr% - 1%
                          gosub'061(fieldnr%)
                          if enabled% <> 0% then L10130
                          goto L10160
L10210:               if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit%  = 32% and fieldnr% = 1% then L65000
                      if keyhit% <>  0% then       L10130
L10240:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10130
                next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        editmode
            message$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Value And Press (ENTER)."
L11090:     gosub'101(2%, 0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       print_it
                  if keyhit% <>  0% then       L11090
            fieldnr% = cursor%(1%) - 7%
            if fieldnr% <  2% or fieldnr% > 12% then L11090
            if fieldnr% =  8% or fieldnr% =  9% then L11090
            if fieldnr% >  9% and header1$ = " " then L11090
            if fieldnr% >  9% then fieldnr% = fieldnr% - 2%

            gosub'061(fieldnr%)
L11230:     gosub'101(3%, fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11230
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11230
            goto editmode

        REM *************************************************************~
            *             G E N E R A T E   R E P O R T                 *~
            *                                                           *~
            * SCANS FORMAT FILE, RESPONDING TO LINES ACCORDINGLY.       *~
            *************************************************************

        print_it
            pageline% = 1000%
            pagenumber%, seq% = 0%
            p1$ = "REPORT:" : p2$ = report$
            call "SHOSTAT" ("Setting Up P&L And % Analysis for period " &~
                                            "ending " & mdate$)
            call "STRING" addr ("CT", title$, 50%)
            call "FILEBGON" (#08)
            call "WORKOPEN" (#08, "IO   ", 500%, f28%)

        REM Set Up Zone Print Info....
            if zonename$(1%) = " " then L12220
                z$(1%) = zonename$(1%) & ": " & zonespec1$()
                if zonespec1$()= " " then z$(1%)= zonename$(1%) & ": All"
                if ieflag$(1%) = "X" then z$(1%) = zonename$(1%) &       ~
                                              ": All But " & zonespec1$()
L12220:     if zonename$(2%) = " " then L12270
                z$(2%) = zonename$(2%) & ": " & zonespec2$()
                if zonespec2$()= " " then z$(2%)= zonename$(2%) & ": All"
                if ieflag$(2%) = "X" then z$(2%) = zonename$(2%) &       ~
                                              ": All But " & zonespec2$()
L12270:     if zonename$(3%) = " " then L12330
                z$(3%) = zonename$(3%) & ": " & zonespec3$()
                if zonespec3$()= " " then z$(3%)= zonename$(3%) & ": All"
                if ieflag$(3%) = "X" then z$(3%) = zonename$(3%) &       ~
                                              ": All But " & zonespec3$()

L12330: REM Loop Through Format Lines...
            for c% = 1% to maxlines%

        REM Action Taken Depends On The Instruction Code...
                descr$ = " "
                if str(inst$(c%),,1%) = "T" then tlev$ = str(inst$(c%),2%)
                if str(inst$(c%),,1%) = "T" then str(inst$(c%),2%) = " "
                search inst_codes$ = inst$(c%) to cursor%() step 2%
                if cursor%(1%) > 0% then L12440
                    print "INVALID INSTRUCTION ENCOUNTERED: "; inst$(c%)
                    goto L12580
L12440:         inst% = (cursor%(1%)+1%) / 2%
                on inst% gosub insth,        /* SET HEADER LINE 2      */~
                               instp,        /* FORCE NEW PAGE         */~
                               instd,        /* PRINT DESCRIPTION ONLY */~
                               instdc,       /* CENTERED DESCRIPTION   */~
                               instu,        /* PRINT UNDERLINE        */~
                               instdu,       /* PRINT DOUBLE UNDERLINE */~
                               get_balances, /* PRINT AMOUNT(S)        */~
                               get_balances, /* PRINT AMOUNT(S)        */~
                               get_balances, /* PRINT AMOUNT(S)        */~
                               get_balances, /* PRINT AMOUNT(S)        */~
                               get_balances, /* PRINT AMOUNT(S)        */~
                               get_balances  /* TOTALS                 */

L12580:     next c%
            gosub L16000
            lastreport$ = report$
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            goto inputmode

        REM *************************************************************~
            * G E T   B A L A N C E   I N S T R U C T I O N   C O D E S *~
            *                                                           *~
            * Retrieves account balance(s) & assoc data for report lines*~
            *************************************************************

        get_balances
            if inst$(c%) = "T" then str(inst$(c%),2%) = tlev$
            call "FRACCUM"  (period1%, period2%, comp$(c%), inst$(c%),   ~
                                group$(c%),                              ~
                                acct1$(c%), acct2$(c%), zone%(),         ~
                                str(zonespec1$()), str(zonespec2$()),    ~
                                str(zonespec3$()), str(zonespec4$()),    ~
                                fixed$(c%), ieflag$(), print_if$(c%),    ~
                                reverse$(c%), print$(c%), paren$, round$,~
                                0%, total$(c%), totals(), text$(c%), 1%, ~
                                periods%, #09, set, dual_books$,u3%, bal%)

            if u3% = 1%  then return  /* Nothing Accumulated */
            if u3% = 99% then L65000  /* Crash */

            readkey$ = all(hex(00))
L13210:     call "PLOWNEXT" (#09, readkey$, 0%, f1%(9%))
                if f1%(9%) = 0% then return

        REM Load Data Returned...
            get #09, using L13305, account$, descr$, record$()

            for i% = 1% to 33%
                if str(record$(i%),,3%) <> " " then record$(i%) = all("#")
            next i%

L13305:     FMT CH(16),     /* Account Number                          */~
                CH(45),     /* Text Or Account Description             */~
                CH(18),     /* Current Year - Opening Balance          */~
                CH(18),     /* Current Period - Opening Balance        */~
                CH(18),     /* Current Period - Budget Opening Balance */~
                CH(18),     /* Current Period  - Activity              */~
                CH(18),     /* Current Period  - Budget Activity       */~
                CH(18),     /* Current Period  - Balance               */~
                CH(18),     /* Current Period  - YTD Budget            */~
                CH(18),     /* Current Year - YTD Activity             */~
                CH(18),     /* Prior year - Opening Balance            */~
                CH(18),     /* Prior Year - Period Opening Balance     */~
                CH(18),     /* Prior Year - Period Budget Opening Bal  */~
                CH(18),     /* Prior Year - Current Period Activity    */~
                CH(18),     /* Prior Year - Current Period Budget Act  */~
                CH(18),     /* Prior Year - Current Period Balance     */~
                CH(18),     /* Prior Year - Current Period YTD Budget  */~
                CH(18),     /* Prior Year - YTD Activity               */~
                CH(18),     /* Current Year - Previous Period Activity */~
                CH(18),     /* Variance - Current Period From Last Per */~
                CH(18),     /* Variance - Current Balance From Last Yr */~
                CH(18),     /* Variance - Current Balance From Budget  */~
                CH(18),     /* Variance - Period Activity From Last Yr */~
                CH(18),     /* Variance - Period Activity From Budget  */~
                CH(18),     /* Next Year - Period Budget Activity      */~
                CH(18),     /* Next Year - Budget Current Balance      */~
                CH(18),     /* Next Year - Period Budget Opening Bal   */~
                CH(18),     /* Current Year - Last Period Budget Act   */~
                CH(18),     /* Current Year - Last Period Budget YTD   */~
                CH(18),     /* Variance - Curr Per Bgt Act From Last Yr*/~
                CH(18),     /* Variance - Curr Per Bgt Act From Next Yr*/~
                CH(18),     /* Variance - Curr Per Bgt Act From Last Pr*/~
                CH(18),     /* Variance - Curr Bgt Bal From Last Year  */~
                CH(18),     /* Variance - Curr Bgt Bal From Next Year  */~
                CH(18)      /* Variance - Curr Bgt Bal From Last Per   */

        REM Set variables For Print...
            p$(), pp$() = " "

        REM Period Activity (PA) ...
            if str(compare$,1%,1%) = "N" then L13520 else L13600
L13520:         if compare$ = "NN" then L13522 else L13530
L13522:             p$(2%) = str(record$(23%),4%)
                    p$(3%) = str(record$(24%),4%)
                    goto L13720
L13530:         if compare$ = "NP" then L13532 else L13540
L13532:             p$(2%) = str(record$(13%),4%)
                    p$(3%) = str(record$(15%),4%)
                    goto L13720
L13540:         if compare$ = "NC" then L13542 else L13600
L13542:             p$(2%) = str(record$(5%),4%)
                    p$(3%) = str(record$(7%),4%)
                    goto L13720
L13600:     p$(1%) = str(record$(4%),4%)
            if compare$ = " " then L13660
            if compare$ = "B " or compare$ = " B" then L13614 else L13620
L13614:         p$(2%) = str(record$(5%),4%)
                goto L13660
L13620:     if compare$ = "P " or compare$ = " P" then L13622 else L13630
L13622:         p$(2%) = str(record$(12%),4%)
                goto L13660
L13630:     if compare$ = "L " or compare$ = " L" then L13632 else L13635
L13632:         p$(2%) = str(record$(17%),4%)
                goto L13660
L13635:     p$(1%) = str(record$(5%),4%)
            if compare$ = "BP" then p$(2%) = str(record$(13%),4%)
            if compare$ = "BN" then p$(2%) = str(record$(23%),4%)
            if compare$ = "BL" then p$(2%) = str(record$(26%),4%)

L13660: REM Current Balance (CB) ...
            p$(3%) = str(record$(6%),4%)
            if compare$ = " " then L13720
            if compare$ = "P " or compare$ = " P" then L13684 else L13688
L13684:         p$(4%) = str(record$(14%),4%)
                goto L13720
L13688:     if compare$ = "B " or compare$ = " B" then L13690 else L13694
L13690:         p$(4%) = str(record$(7%),4%)
                goto L13720
L13694:     if compare$ = "L " or compare$ = " L" then L13696 else L13710
L13696:         p$(4%) = str(record$(14%),4%)
                goto L13720
L13710:     p$(3%) = str(record$(7%),4%)
            if compare$ = "BP" then p$(4%) = str(record$(15%),4%)
            if compare$ = "BN" then p$(4%) = str(record$(24%),4%)
            if compare$ = "BL" then p$(4%) = str(record$(27%),4%)

L13720:         if zero$ = "YES" then L13840
                if print$(c%) = "NO" then L13840
                if str(inst$(c%),,1%) = "T" then L13840 /* Totals Always */
                for i% = 1% to 4%                          /*  Print  */
                    temp$ = p$(i%)
                    tran(temp$, " ( ) ,")replacing
                    call "SPCSMASH" (temp$)
                    temp = 0: convert temp$ to temp, data goto L13800
L13800:             if abs(temp) > .001 then L13840
                next i%
                goto L13210

L13840:         if str(print$(c%),,1%)<> "N" then gosub form_control
                if compare$ = " " then p$(2%), p$(4%) = " "
                put line$, using L15600, descr$, p$(1%), " ", p$(2%), " ",~
                                                p$(3%), " ", p$(4%)
                seq% = seq% + 1%
                convert base$(c%) to base%, data goto L13900 : goto L13910
L13900:         base% = 0%
L13910:         write #08, using L14390, seq%, c%, print$(c%), line$, base%
                line$ = " "
                goto L13210

        REM *************************************************************~
            *            O T H E R   I N S T R U C T I O N S            *~
            *                                                           *~
            * Page Control & Cosmetic Routine Are Here.                 *~
            *************************************************************

        instd : REM Print Text Only...
                p$(),pp$() = " "
                descr$ = text$(c%)
                gosub write_misc
                return

        instdc : REM Print Description Only, Center On Page...
                 gosub form_control
                 call "STRING" addr ("CT", text$(c%), 45%)
                 put line$, using L15570, text$(c%)
                 call "STRING" addr ("LJ", text$(c%), 45%)
                 gosub just_write
                 return

        instu : REM Print Under Lines...
                init("-") p$(), pp$()
                goto L14160

        instdu : REM Print Double Under Lines...
                 init("=") p$(), pp$()
L14160:          for i% = 1% to 4%
                     str(p$(i%),15%) = " "
                     str(pp$(i%),8%) = " "
                 next i%
                 gosub write_misc
                 return

        instp : REM Force New Page...
                pageline% = 1000%
                return

        insth : REM Set Report Subtitle...
                if text$(c%) =  " " then reportheader$ = text$(1%)       ~
                                    else reportheader$ = text$(c%)
                call "STRING" addr ("CT", reportheader$, 50%)
                return

        write_misc : REM Writes Non Numeric Lines...
                gosub form_control
                if str(compare$,1%,1%) = "N" then L14304 else L14310
L14304:             put line$, using L15480, descr$, p$(1%), p$(2%), p$(3%)
                    goto just_write
L14310:         if compare$ = " " then p$(2%),pp$(2%),p$(4%),pp$(4%)=" "
                put line$, using L15600, descr$, p$(1%), pp$(1%), p$(2%), ~
                    pp$(2%), p$(3%), pp$(3%), p$(4%), pp$(4%)

        just_write:  seq% = seq% + 1%
                write #08, using L14390, seq%, 0%, "Y", line$, 0%
                line$ = " "
                return

L14390:         FMT BI(2), BI(2), CH(1), CH(132), BI(2)

        REM *************************************************************~
            *        P A G E   C O N T R O L   R O U T I N E            *~
            *                                                           *~
            * CONTROLS THE PAGING                                       *~
            *************************************************************

        form_control
            pageline% = pageline% + 1%
               if pageline% < 59% then return
               line$ = "^PAGE^TOF^" : gosub just_write
               pagenumber% = pagenumber% + 1%
               put line$, using L15320, date$, rptid$
                  gosub just_write
               put line$, using L15330, z$(1%), reportheader$, pagenumber%
                  gosub just_write
               put line$, using L15340, z$(2%), title$
                  gosub just_write
               put line$, using L15350, sethdr$
                  gosub just_write
               put line$, using L15360, z$(3%), mdate$
                  gosub just_write
               p1$, p2$, z$() = " "
               gosub just_write
               gosub just_write
               pageline% = 10%
               if compare$ = "  " then put line$, using L15370, " "
               if compare$ = "P " or compare$ = " P" then                ~
                   put line$, using L15380, " "
               if compare$ = "B " or compare$ = " B" then                ~
                   put line$, using L15390, " "
               if compare$ = "L " or compare$ = " L" then                ~
                   put line$, using L15400, " "
               if compare$ = "BP" then put line$, using L15410, " "
               if compare$ = "BN" then put line$, using L15420, " "
               if compare$ = "BL" then put line$, using L15430, " "
               if compare$ = "NN" then put line$, using L15440, " "
               if compare$ = "NP" then put line$, using L15450, " "
               if compare$ = "NC" then put line$, using L15460, " "
               gosub just_write
               gosub just_write
               pageline% = 10%
               return

L15320: %RUN DATE: ########                                              ~
        ~                                                FRPRINT3: ######

L15330: %###################################   ##########################~
        ~########################                            PAGE: ###

L15340: %###################################   ##########################~
        ~########################

L15350: %                                  ##############################~
        ~##############################

L15360: %###################################          ###################~
        ~#############

L15370: %                                  PERIOD ACTIVITY      %        ~
        ~                      YTD BALANCE      %                        #

L15380: %                              PERIOD ACTIVITY      %         LAS~
        ~T YEAR      %       YTD BALANCE      %         LAST YEAR      % #

L15390: %                              PERIOD ACTIVITY      %           B~
        ~UDGET       %       YTD BALANCE      %            BUDGET      % #

L15400: %                              PERIOD ACTIVITY      %      LAST P~
        ~ERIOD       %       YTD BALANCE      %         LAST YEAR      % #

L15410: %                              PERIOD ACTIVITY      %    PREV YR ~
        ~BUDGET      %       YTD BALANCE      %    PREV YR BUDGET      % #

L15420: %                              PERIOD ACTIVITY      %    NEXT YR ~
        ~BUDGET      %       YTD BALANCE      %    NEXT YR BUDGET      % #

L15430: %                              PERIOD ACTIVITY      %    PREV PER~
        ~IOD BGT     %       YTD BALANCE      %    PREV PERIOD BGT     % #

L15440: %                                                       PERIOD BU~
        ~DGET-NEXT   %    YTD BUDGET-NEXT     %                          #

L15450: %                                                       PERIOD BU~
        ~DGET-PREV   %    YTD BUDGET-PREV     %                          #

L15460: %                                                       PERIOD BU~
        ~DGET-CURR   %    YTD BUDGET-CURR     %                          #

L15480: %############################## ###############          ########~
        ~#######          ###############

L15570: %                                            ####################~
        ~#########################

L15600: %############################## ############### ######## ########~
        ~####### ######## ############### ######## ############### #######~
        ~#
L16000: REM *************************************************************~
            *            P R I N T   T H E   R E P O R T                *~
            *-----------------------------------------------------------*~
            * TAKES PRINT IMAGE FROM WORK FILE, CALCS THE DEFFINED      *~
            * PERCENTAGES, AND SENDS IT ON TO THE PRINTER.              *~
            *************************************************************

            call "SHOSTAT" ("Printing" & hex(84) &                       ~
                                           str(title$, pos(title$<>" ")))
                select printer (132)
                call "SETPRNT" (rptid$, " ", 0%, 0%)
                readkey$ = all(hex(00))

        REM Load Next Line To Be Printed...
L16130:         call "PLOWALTS" (#08, readkey$, 0%, 0%, f1%(8%))
                     if f1%(8%) = 0% then return
                get #08, using L16160, print$, line$, base%
L16160:         FMT XX(4), CH(1), CH(132), BI(2)
                if print$ = "N" then L16130
                if base% <> 0 then L16230  /* Line To Show Percents? */
L16190:            if line$="^PAGE^TOF^" then print page else print line$
                   goto L16130

        REM Load Up Line Were Supposed To Base Percentage On...
L16230:         temp$ = bin(base%,2%)
                call "REDALT0" (#08, str(temp$,,2%), 1%, f1%(8%))
                     if f1%(8%) <> 0% then L16350
                          close printer
                          errormsg$ = "Base Line (From Format) : ###"
                          convert base% to str(errormsg$,27%,3%), pic(###)
                          call "ASKUSER" (0%, "Please Note",             ~
                                      errormsg$, "Is Pointed To, But " & ~
                                      "Does Not Exist.",                 ~
                                      "Press (RETURN) To Exit")
                          goto L65000

L16350:         get #08, using L16360, line1$
L16360:         FMT XX(5), CH(132)

        REM Retrieve Numbers From Base Line...
                get line1$, using L16870, tmpbal$(1%), tmpbal$(2%),       ~
                    tmpbal$(3%), tmpbal$(4%)
                tran(str(tmpbal$()), " )-( ,")replacing
                for i% = 1% to 4%
                     call "SPCSMASH" (tmpbal$(i%))
                     convert tmpbal$(i%) to tmpbal(i%), data goto L16450
L16450:         next i%

        REM Retrieve Numbers From Line Where Gonna Print...
                get line$, using L16870, p$(1%), p$(2%), p$(3%), p$(4%)
                tran(str(p$()), " )-( ,")replacing
                for i% = 1% to 4%
                     call "SPCSMASH" (p$(i%))
                     convert p$(i%) to p(i%), data goto L16530
L16530:         next i%

        REM Calculate Percentages...
                mat p1 = zer
                pp$() = " "
                for i% = 1% to 4%
                     if p$(i%) = " " then L16720
                     if tmpbal(i%) = 0 then L16720
                     p1(i%) = round((p(i%)/tmpbal(i%))*100%,2%)
        REM          IF ABS(P1(I%)) < .005 THEN 16680
                     convert p1(i%) to pp$(i%), pic(-###.##)
                     if abs(p1(i%)) >= 1000 then convert                 ~
                                   p1(i%) to pp$(i%), pic(-####.#)

        REM Put Parenthesis Around Percentages If Needed...
                     if paren$ <> "YES" then L16720
                     if pos(pp$(i%) = "-") = 0 then L16720
                          str(pp$(i%),8%) = ")"
                          tran(pp$(i%), "(-")replacing
L16720:         next i%

        REM Put Percentages Back Into Print Line...
                put line$, using L16800, pp$(1%)
                  put line$, using L16810, pp$(2%)
                    put line$, using L16820, pp$(3%)
                      put line$, using L16830, pp$(4%)

L16800:         FMT POS(48), CH(8)
L16810:           FMT POS(73), CH(8)
L16820:             FMT POS(98), CH(8)
L16830:               FMT POS(123), CH(8)

                goto L16190           /* PRINT THE LINE */

L16870: %                               ###############          ########~
        ~#######          ###############          ###############

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1% : message$=" "
                  on fieldnr% gosub L20350,         /* REPORT NUMBER    */~
                                    L20390,         /* DESCRIPTION      */~
                                    L20440,         /* DEFAULT TITLE    */~
                                    L20490,         /* DEFAULT PAREN    */~
                                    L20550,         /* ROUND?           */~
                                    L20610,         /* PRINT IF ZERO?   */~
                                    L20670,         /* COMPARE TO       */~
                                    L20720,         /* ZONE ONE         */~
                                    L20720,         /* ZONE TWO         */~
                                    L20720          /* ZONE THREE       */
                  return

            deffn'061(fieldnr%)
                  message$=" "
                  enabled% = 1%
                  on fieldnr% gosub L20360,         /* REPORT NUMBER    */~
                                    L20410,         /* PERIOD           */~
                                    L20470,         /* TITLE            */~
                                    L20520,         /* PAREN            */~
                                    L20580,         /* ROUND?           */~
                                    L20640,         /* PRINT IF ZERO?   */~
                                    L20690,         /* COMPARE TO       */~
                                    L20720,         /* ZONE ONE         */~
                                    L20720,         /* ZONE TWO         */~
                                    L20720          /* ZONE THREE       */
                     return

L20350: REM DEFAULT/ENABLE FOR REPORT NUMBER...
L20360:         message$ = "Enter A Blank Report Number To Search For Des~
        ~ired Report."
                return

L20390: REM DEFAULT/ENABLE FOR PERIOD TO REPORT...
            month1$, month2$ = " "
L20410:     message$ = "Enter Range Of Periods To Be Reported."
            return

L20440: REM DEFAULT/ENABLE FOR TITLE...
                get #01, using L20460, title$
L20460:         FMT XX(33), CH(50)
L20470:         message$= "Enter Title To Print On Report Header."
                return

L20490: REM DEFAULT/ENABLE FOR PARENTHESIS OPTION...
                get #01, using L20510, paren$
L20510:         FMT XX(83), CH(3)
L20520:         message$ = "Enter 'YES' If You Want Parenthesis Rather Th~
        ~an Minus Signs."
                return

L20550: REM DEFAULT/ENABLE FOR ROUNDING OPTION...
                get #01, using L20570, round$
L20570:         FMT XX(89), CH(3)
L20580:         message$ = "Enter 'YES' If You Don't Want 'Cents' Printed~
        ~ On Dollar Amounts."
                return

L20610: REM DEFAULT/ENABLE FOR PRINT IF ZERO OPTION...
                get #01, using L20630, zero$
L20630:         FMT XX(86), CH(3)
L20640:         message$ = "Enter 'NO' If You Don't Want To Print Line Wh~
        ~en amount(s) On Line Are All 0."
                return

L20670: REM DEFAULT/ENABLE FOR COMPARISON FLAG...
                compare$ = defcompare$
L20690:         message$ = "Leave Blank or enter 'B', 'P', 'L', 'BP', 'BN~
        ~', 'BL', 'NN', 'NP' or 'NC'."
                return

L20720: REM DEFAULT/ENABLE FOR ZONE SPECIFICATIONS...
                if zonename$(fieldnr%-7%) = " " then enabled% = 0% else  ~
                         header1$ = "     'O' For Only, 'X' For All But"
                message$ = "Leave Blank For All, Or Enter " &            ~
                      zonename$(fieldnr%-7%) & " To Be Printed/Excluded."
                return

L29000: REM *************************************************************~
            * INITIALIZATION BLOCK (NEATER THAN CRAMMING AT 10000)      *~
            *************************************************************

            set$, setdescr$, sethdr$, set2$,/* G/L set of books to use */~
            acct1$(),                    /* FROM ACCOUNT               */~
            acct2$(),                    /* TO ACCOUNT                 */~
            base$(),                     /* FROM ACCOUNT               */~
            col$(),                      /* COLUMN INDICATOR           */~
            comp$(),                     /* COMPANY (DBASE POINTER)    */~
            compare$,                    /* COMPARE FLAG               */~
            descr$,                      /* REPORT DESCRIPTION         */~
            errormsg$,                   /* ERROR MESSAGE              */~
            filler$,                     /* FILLER AT END OF RECORD    */~
            fixed$(),                    /* RUN TIME OVERRIDE FLAG     */~
            group$(),                    /* ACCOUNT GROUPING CODE      */~
            header1$,                    /* SCREEN HEADER              */~
            reportheader$,               /* REPORT HEADER              */~
            ieflag$(),                   /* INCLUDE/EXCLUDE FLAGS      */~
            iemesg$(),                   /* INCLUDE/EXCLUDE FLAGS      */~
            inst$(),                     /* INSTRUCTION CODE           */~
            line$,                       /* PRINT LINE TO WRITE TO WRKF*/~
            mdescr$,                     /* DATES OF AVAILABLE MONTHS  */~
            message$,                    /* INPUT MESSAGE              */~
            memo$(),                     /* JUNK TEXT                  */~
            month1$,                     /* STARTING G/L PERIOD        */~
            month2$,                     /* ENDING G/L PERIOD          */~
            paren$,                      /* DEFAUT PARENTHESIS OPTION  */~
            pdescr$,                     /* PERIOD RANGE DESCRIPTION   */~
            print$(),                    /* PRINT CODE                 */~
            print_if$(),                 /* PRINT/TOTAL CONSTRAINT FLAG*/~
            report$,                     /* REPORT ID.                 */~
            reverse$(),                  /* REVERSE SIGN INDICATOR     */~
            round$,                      /* ROUND OPTION (DEFUALT)     */~
            text$(),                     /* TEXT FOR LINE ITEMS        */~
            title$,                      /* DEFAULT REPORT TITLE       */~
            total$(),                    /* TOTALING SPEC FOR LINE     */~
            z$(),                        /* ZONE DESCR FOR PRINT       */~
            zero$,                       /* PRINT IF ZERO? (DEFUALT)   */~
            zone$(),                     /* START, LENGTH IN ACCOUNT   */~
            zonename$(),                 /* LABLE FOR ZONE IN ACCOUNT  */~
            zonespec1$(),                /* USERS SELECTION FOR ZONE 1 */~
            zonespec2$(),                /* USERS SELECTION FOR ZONE 2 */~
            zonespec3$(),                /* USERS SELECTION FOR ZONE 3 */~
            zonespec4$() = " "           /* USERS SELECTION FOR ZONE 4 */

            mat totals = zer             /* REPORT TOTALS ACCUMULATOR  */
        return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

L29916: startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
                if u3% <> 0% then L29916
            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *              L O A D   O L D   R E P O R T                *~
            *                                                           *~
            * LOADS OLD REPORT FROM DISK FILES.                         *~
            *************************************************************

            onfile% = 0%

        REM Try And Load Report...
            call "READ100" (#01, report$, f1%(1%))
                if f1%(1%) = 0% then return

        REM Load Zone Data From Header First...
            onfile% = 1%
            get #01, using L30150, zone$(), zonename$(), set$
L30150:         FMT XX(92), 8*CH(2), 4*CH(20), CH(1)

            if set$ <> "2" or dual_books$ <> "Y" then set$ = "1"
            if dual_books$ = "Y" then set2$ = set$
            convert set$ to set, data goto L30170
L30170:     mat zone% = zer
            for i% = 1% to 4%
                for j% = 1% to 2%
                     if zone$(i%,j%) <> " " then convert zone$(i%,j%) to ~
                                                             zone%(i%,j%)
                next j%
            next i%

            for i% = 1% to 4%
                if zonename$(i%) = " " then L30330
                if zone%(i%,1%) = 0% then L30330
                on i% goto L30280, L30290, L30300, L30310
L30280:         mat redim zonespec1$(zone%(i%,2%),1%)1% : goto L30330
L30290:         mat redim zonespec2$(zone%(i%,2%),1%)1% : goto L30330
L30300:         mat redim zonespec3$(zone%(i%,2%),1%)1% : goto L30330
L30310:         mat redim zonespec4$(zone%(i%,2%),1%)1% : goto L30330
*              ZONENAME$(I%) = " "
L30330:     next i%

        REM Set up the descriptives for the G/L set of books to use...
            if dual_books$ <> "Y" then goto L30350
                setdescr$ = "Statutory"
                if set = 2 then setdescr$ = "Local Authority"
                sethdr$ = setdescr$
                call "FMTTITLE" (sethdr$, "G/L SYSTEM", 12%)
                call "PUTPAREN" (setdescr$)

L30350: REM Load The Line Items...
            maxlines% = 0%
            readkey$ = str(report$) & hex(0000)
L30380:     call "PLOWNEXT" (#02, readkey$, 3%, f1%(2%))
                     if f1%(2%) = 0% then return

            c%, maxlines% = maxlines% + 1%
            get #02, using L30580, report$, c%, inst$(c%), comp$(c%),     ~
                          group$(c%), acct1$(c%), acct2$(c%),            ~
                          fixed$(c%), print$(c%), print_if$(c%),         ~
                          col$(c%), reverse$(c%), text$(c%), base$(c%),  ~
                          total$(c%), memo$(c%)

            if fixed$(c%) = "Y" then fixed$(c%) = "YES"
            if fixed$(c%) = "N" then fixed$(c%) = "NO "
            if reverse$(c%) = "Y" then reverse$(c%) = "YES"
            if reverse$(c%) = "N" then reverse$(c%) = "NO "
            if print$(c%) = "Y" then print$(c%) = "YES"
            if print$(c%) = "N" then print$(c%) = "NO "
            if print$(c%) = "A" then print$(c%) = "ALL"
            if print$(c%) = "S" then print$(c%) = "SUM"
            goto L30380

L30580:         FMT CH(03),              /* REPORT NUMBER              */~
                    BI(2),               /* SEQUENCE NUMBER            */~
                    CH(02),              /* INSTRUCTION CODE           */~
                    CH(03),              /* COMPANY CODE               */~
                    CH(06),              /* ACCOUNT GROUP CODE         */~
                    CH(16),              /* START ACCOUNT NUMBER       */~
                    CH(16),              /* END ACCOUNT NUMBER         */~
                    CH(01),              /* IGNORE ZONING FLAG         */~
                    CH(01),              /* PRINT FLAG                 */~
                    CH(01),              /* PRINT RESTRICTION          */~
                    CH(01),              /* COLUMN INDICATOR           */~
                    CH(01),              /* REVERSE SIGNS?             */~
                    CH(45),              /* DESCRIPTION                */~
                    CH(03),              /* LINE NUMBER TO BASE %S ON  */~
                    CH(10),              /* TOTALING SPECS             */~
                    CH(70),              /* MEMO SPACE                 */~
                    CH(19)               /* FILLER                     */

        REM *************************************************************~
            *      I N P U T   /   E D I T   M O D E   P A G E   1      *~
            *                                                           *~
            * SERVES INPUT LOOP AND EDIT MODE FOR PAGE ONE OF DOCUMENT. *~
            *************************************************************

            deffn'101(screen%, fieldnr%)
                  gosub set_keys
                  init(hex(84)) hfac$()
                  init(hex(8c)) mfac$(), tfac$
                  if fieldnr% = 0% then init(hex(86)) str(hfac$(),2%)
                  if header1$ <> " " then tfac$ = hex(ac)
                  header$  = "Last Report Printed: " & lastreport$
                  if lastreport$ = " " then header$ = " "
                  str(header$,62%) = "FRPRINT3: " & cms2v$

                  on fieldnr% gosub L40320,         /* REPORT NUMBER    */~
                                    L40290,         /* PERIOD           */~
                                    L40290,         /* TITLE            */~
                                    L40320,         /* PAREN            */~
                                    L40320,         /* ROUND?           */~
                                    L40320,         /* PRINT IF ZERO?   */~
                                    L40320,         /* COMPARE TO       */~
                                    L40320,         /* ZONE ONE         */~
                                    L40320,         /* ZONE TWO         */~
                                    L40320          /* ZONE THREE       */
                     goto L40390

L40290:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      hfac$(fieldnr%) = hex(80)
                      return
L40320:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      hfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      hfac$(fieldnr%) = hex(82)
                      return

L40390: accept                                                           ~
               at (01,02), "Financial Statements Report Generation",     ~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$                   ,ch(08),~
               at (02,02), fac(hex(ac)), header$                 ,ch(79),~
               at (04,27), "** Percentage Analysis **",                  ~
               at (05,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Report Number:",                             ~
               at (06,17), fac(hfac$(1%)), report$               ,ch(03),~
               at (06,37), fac(hex(8c)),  descr$                 ,ch(32),~
                                                                         ~
               at (07,02), fac(hex(8c)),  setmsg$                ,ch(11),~
               at (07,19), fac(hex(8c)),  set2$                  ,ch(01),~
               at (07,37), fac(hex(8c)),  setdescr$              ,ch(30),~
                                                                         ~
               at (09,02), "Select Period Range To Report",              ~
               at (09,33), fac(hfac$(2%)), month1$              , ch(02),~
               at (09,37), "Thru",                                       ~
               at (09,43), fac(hfac$(2%)), month2$              , ch(02),~
               at (09,47), fac(hex(8c)),  pdescr$               , ch(32),~
                                                                         ~
               at (10,02), "Report Title:",                              ~
               at (10,17), fac(hfac$(3%)), title$               , ch(50),~
                                                                         ~
               at (11,02), "Parenthesis To Show Negatives?",             ~
               at (11,36), fac(hfac$(4%)), paren$                ,ch(03),~
                                                                         ~
               at (12,02), "Round Amounts To Nearest Dollar?",           ~
               at (12,36), fac(hfac$(5%)), round$                ,ch(03),~
                                                                         ~
               at (13,02), "Print Line When All Zeroes?",                ~
               at (13,36), fac(hfac$(6%)), zero$                 ,ch(03),~
                                                                         ~
               at (14,02), "Compare With (Blank, B, P, L, BP, BN, BL, NN,~
        ~ NP or NC):",                                                    ~
               at (14,60), fac(hfac$(7%)), compare$              ,ch(02),~
                                                                         ~
               at (16,28), fac(tfac$),    header1$               ,ch(39),~
               at (17,03), fac(hex(8c)),  zonename$(1%)          ,ch(20),~
               at (17,23), fac(hfac$(8%)), str(zonespec1$())            ,~
               at (17,46), fac(hfac$(8%)), ieflag$(1%)           ,ch(01),~
               at (17,49), fac(hex(84)),  iemesg$(1%)            ,ch(07),~
                                                                         ~
               at (18,03), fac(hex(8c)),  zonename$(2%)          ,ch(20),~
               at (18,23), fac(hfac$(9%)), str(zonespec2$())            ,~
               at (18,46), fac(hfac$(9%)), ieflag$(2%)           ,ch(01),~
               at (18,49), fac(hex(84)),  iemesg$(2%)            ,ch(07),~
                                                                         ~
               at (19,03), fac(hex(8c)),  zonename$(3%)          ,ch(20),~
               at (19,23), fac(hfac$(10%)), str(zonespec3$())           ,~
               at (19,46), fac(hfac$(10%)), ieflag$(3%)          ,ch(01),~
               at (19,49), fac(hex(84)),  iemesg$(3%)            ,ch(07),~
                                                                         ~
               at (21,02), fac(hex(a4)), message$                ,ch(79),~
               at (22,02), fac(hex(8c)), pfdescr$(1%)            ,ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(2%)            ,ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(3%)            ,ch(79),~
                                                                         ~
               keys(pfkeys$), key (keyhit%)

               if keyhit% <> 13% then L41020
                  call "MANUAL" ("FRPRINT3")
                  goto L40390

L41020:        if keyhit% <> 15% then L41060
                  call "PRNTSCRN"
                  goto L40390

L41060:        if fieldnr% <> 0% then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                    S E T   K E Y S                        *~
            *                                                           *~
            * Sets PF Keys & Descriptions Based On SCREEN%...           *~
            *************************************************************

        set_keys                 /* Cleansliness Is Next To Godliness? */
            on screen% goto L49110,         /* Header, Input Mode       */~
                            L49280,         /* Header, Edit Mode        */~
                            L49370          /* All Scrns, Edit Field    */

L49110: REM  =-=-=-=-=-=-=-=-=-= Header, Input mode =-=-=-=-=-=-=-=-=-=-=
            pfdescr$(1%)= "(1)Start Over    (4)Previous Field            ~
        ~                 (13)Instructions"
            pfdescr$(2%)= "                                              ~
        ~                 (15)Print Screen"
            pfdescr$(3%)= "                                              ~
        ~                 (16)Exit Program"
            pfkeys$ = hex(0001040d0f10ffffffffffffffff)
            str(pfdescr$(3%),63%,1%) = hex(84)

*        Flip Off Appropriate Fields
            if fieldnr% = 1% then L49240
                str(pfdescr$(3%),63%)  = " "    /* Shut Off Exit Optn  */
                str(pfkeys$,6%,1%) = hex(ff)
                goto L49260
L49240:     str(pfdescr$(1%),,35%) = " "        /* Shut Off Prev Field */
            str(pfkeys$,2%,2%) = hex(ffff)
L49260: return

L49280: REM =-=-=-=-=-=-=-=-=-= Header, Edit mode =-=-=-=-=-=-=-=-=-=-=-=
            pfdescr$(1%)= "(1)Start Over                                 ~
        ~                 (13)Instructions"
            pfdescr$(2%)= "                                              ~
        ~                 (15)Print Screen"
            pfdescr$(3%)= "                                              ~
        ~                 (16)PRINT REPORT"
            pfkeys$ = hex(00010d0f10ffffffffffffffffffff)
            str(pfdescr$(3%),63%,1%) = hex(84)
        return

L49370: REM =-=-=-=-=-=-=-= All Screens, Field Edit =-=-=-=-=-=-=-=-=-=
            pfdescr$(1%)= "(1)Start Over                                 ~
        ~                 (13)Instructions"
            pfdescr$(2%)= "(ENTER) Validate Modification(s)              ~
        ~                 (15)Print Screen"
            pfdescr$(3%)= " "
            pfkeys$ = hex(00010d0e0fffffffffffffffffffffff)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50200,         /* REPORT NUMBER    */~
                                    L50290,         /* PERIOD           */~
                                    L50480,         /* TITLE            */~
                                    L50500,         /* PAREN            */~
                                    L50590,         /* ROUND?           */~
                                    L50680,         /* PRINT IF ZERO?   */~
                                    L50770,         /* COMPARE FLAG     */~
                                    L50830,         /* ZONE ONE         */~
                                    L50830,         /* ZONE TWO         */~
                                    L50830          /* ZONE THREE       */
                     return

L50200: REM TEST DATA FOR REPORT NUMBER...
                call "GETCODE" (#01, report$, descr$, 1%, 0, f1%(1%))
                if f1%(1%) <> 0% then L50250
                    errormsg$ = hex(00)
                    return
L50250:         gosub L30000
                if onfile% = 0 then errormsg$ = "Report Not On File"
                return

L50290: REM TEST DATA FOR PERIOD RANGE...
            if month1$ = "?" then month1$ = " "
                period1%, period2% = 0%
            convert month1$ to period1%, data goto L50310
L50310:     mdescr$ = hex(06) & "Select Starting G/L Period"
            call "GLPERIOD" (#03, period1%, mdescr$, 1%, 0%, u3%)
                if u3% = 1% then L50335
L50325:              errormsg$ = "Invalid Choice For Period"
                     return
L50335:     convert period1% to month1$, pic(##)
                pdescr$ = str(mdescr$)
            if month2$ <> " " then L50360
                month2$ = month1$ : period2% = period1%
                goto L50415
L50360:     if month2$ = "?" then month2$ = " "
            convert month2$ to period2%, data goto L50370
L50370:     mdescr$ = hex(06) & "Select Ending G/L Period"
            call "GLPERIOD" (#3, period2%, mdescr$, 1%, 0%, u3%)
                if u3% = 1% then L50385 else L50325
L50385:     convert period2% to month2$, pic(##)
                str(pdescr$,16%,9%) = str(mdescr$,16,9%)
            if period2% >= period1% then L50415
                errormsg$ = "Starting Period MUST be LESS than or EQUAL"&~
                            " To Ending Period"
                return
L50415:     mdate$ = " "
            temp2$ = str(pdescr$,2%,8%)
            call "DATUNFMT" (temp2$,temp%,temp1$)
            convert str(temp1$,5%,2%) to month%
                m% = month% * 4% - 3%
                mdate$ = str(months$,m%,4%) & " " & str(temp1$,7%,2%) &  ~
                         ", " & str(temp1$,1%,4%) & " Thru "
            temp2$ = str(pdescr$,16%,8%)
            call "DATUNFMT" (temp2$,temp%,temp1$)
            convert str(temp1$,5%,2%) to month%
                m% = month% * 4% - 3%
                str(mdate$,20%) = str(months$,m%,4%) & " "              &~
                        str(temp1$,7%,2%) & ", "& str(temp1$,1%,4%)
            bal% = 1%                                   /* Set default  */
            if period1% < 14% then return               /* Not Needed   */
L50460:         bal% = 0%
                call "ASKUSER" (bal%, "*** YTD CALCULATION ***",         ~
                     "Do you want YTD calculations to begin with Period"&~
                     " 1 or Periods 14?", "Press PF(1) to begin with Pe"&~
                     "riod 1    -OR-", "Press PF(14) to begin with Peri"&~
                     "od 14.")
                if bal% = 1% or bal% = 14% then return
                    goto L50460

L50480: REM TEST DATA FOR REPORT TITLE...
             if title$ = " " then errormsg$ = "Title Can't Be Blank"
             return

L50500: REM TEST DATA FOR PARENTHESIS OPTION...
                if paren$ = " " then paren$ = "NO "
                if str(paren$,,1%) = "Y" then paren$ = "YES"
                if str(paren$,,1%) = "N" then paren$ = "NO "
                if paren$ = "YES" then return
                if paren$ = "NO " then return
                errormsg$ = "Please Enter 'YES' Or 'NO': " & paren$
                return

L50590: REM TEST DATA FOR ROUND OPTION...
                if round$ = " " then round$ = "NO "
                if str(round$,,1%) = "Y" then round$ = "YES"
                if str(round$,,1%) = "N" then round$ = "NO "
                if round$ = "YES" then return
                if round$ = "NO " then return
                errormsg$ = "Please Enter 'YES' Or 'NO': " & round$
                return

L50680: REM TEST DATA FOR ZERO OPTION...
                if zero$ = " " then zero$ = "YES"
                if str(zero$,,1%) = "Y" then zero$ = "YES"
                if str(zero$,,1%) = "N" then zero$ = "NO "
                if zero$ = "YES" then return
                if zero$ = "NO " then return
                errormsg$ = "Please Enter 'YES' Or 'NO': " & zero$
                return

L50770: REM TEST DATA FOR COMPARISON FLAG...
            defcompare$ = compare$
            if compare$ = "  " or compare$ = " B" or compare$ = "B " or  ~
                                  compare$ = " P" or compare$ = "P " or  ~
                                  compare$ = " L" or compare$ = "L " or  ~
                                  compare$ = "BP" or compare$ = "BN" or  ~
                                  compare$ = "BL" or compare$ = "NN" or  ~
                                  compare$ = "NP" or compare$ = "NC"     ~
                   then return
            errormsg$ = "Please Enter Blank, B, L, P, BN, BP, BL, NN, " &~
                        "NP, or NC: " & compare$
                return

L50830: REM TEST DATA FOR ZONE ONE SPECS...
                k% = fieldnr% - 7%
                on k% goto L50860, L50870, L50880
L50860:         if zonespec1$() <> " " then L50930 : goto L50890
L50870:         if zonespec2$() <> " " then L50930 : goto L50890
L50880:         if zonespec3$() <> " " then L50930 : goto L50890
L50890:         ieflag$(k%) = " "
                if zonename$(k%) = " " then return
                iemesg$(k%) = "ALL"
                return
L50930:         if pos("OX" = ieflag$(k%)) = 0 then L50970
                     iemesg$(k%) = "Only"
                     if ieflag$(k%) = "X" then iemesg$(k%) = "All But"
                     return
L50970:         errormsg$= "Please Indicate Desired Only/Exclude Option."
                if ieflag$(k%) <> " " then errormsg$ = "Enter 'O' Or 'X'"
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

            call "SHOSTAT" ("Releasing Files, One Moment Please")
            call "FILEBGON" (#08)
            call "FILEBGON" (#09)
            end
