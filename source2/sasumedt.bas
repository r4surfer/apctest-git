        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS    AAA    SSS   U   U  M   M  EEEEE  DDDD   TTTTT   *~
            *  S      A   A  S      U   U  M M M  E      D   D    T     *~
            *   SSS   AAAAA   SSS   U   U  M   M  EEE    D   D    T     *~
            *      S  A   A      S  U   U  M   M  E      D   D    T     *~
            *   SSS   A   A   SSS    UUU   M   M  EEEEE  DDDD     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SASUMEDT - Allows input and edit of target / actual       *~
            *            fields in the Sales Analysis Summary files.    *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/09/92 ! Cloned from SATGTINP                     ! KAB *~
            * 05/27/94 ! PRR 13173 - Column Headers now display   ! RJH *~
            *          !  for Part and non-Part Summaries.        !     *~
            * 08/24/94 ! Allow Non-stocked Parts if flag is set.  ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            book_qty(13),                /* Booking Units              */~
            book_qty$(13)8,              /* Booking Units              */~
            book_val$(13)10,             /* Booking Value              */~
            book_val(13),                /* Booking Value              */~
            butotal$10,                  /* Booking Units Total        */~
            bvtotal$10,                  /* Booking Value Total        */~
            code1$(1)25,                 /* 1st Grp Code - Input/Edit  */~
            code2$(1)25,                 /* 2nd Grp Code - Input/Edit  */~
            codefile%(8), codelen%(8),   /* Group Code Len, File#      */~
            codes%(10,2),                /* Summary File Group Codes   */~
            codes$(10,2)14,              /* Group Code Descriptors     */~
            codex$25,                    /* Group code work area       */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descrs$(10)30,               /* Summary File Descriptions  */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            gr1_desc$34,                 /* Group code 1 description   */~
            gr2_desc$34,                 /* Group code 2 description   */~
            group1$14, group2$14,        /* Group Code Descriptors     */~
            hi$80,                       /* Line 1 for ASKUSER         */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            keydate$8,                   /* Date key for SASUMRYn      */~
            keytab$17,                   /* PF keys enabled            */~
            last$8,                      /* Last SA Year               */~
            l7qfac$1,                    /* Fac for line 7 Quantities  */~
            l7vfac$1,                    /* Fac for line 7 Values      */~
            lo$80,                       /* Line 3 for ASKUSER         */~
            l7_period$12,                /* Line 7 header              */~
            l7_bkgqty$9,                 /* Line 7 header              */~
            l7_bkgval$9,                 /* Line 7 header              */~
            l7_shpqty$9,                 /* Line 7 header              */~
            l7_shpval$9,                 /* Line 7 header              */~
            l7_prices$14,                /* Line 7 header              */~
            l7_prices$(2)14,             /* Line 7 header              */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            linenr$(13)3,                /* Edited line numbers        */~
            mid$80,                      /* Line 2 for ASKUSER         */~
            msg$79,                      /* Miscellaneous messages     */~
            next$8,                      /* Next SA Year               */~
            nonstock$1,                  /* Update Non-Stock parts?    */~
            period$(13)8,                /* Period dates               */~
            pdfac$(13)1,                 /* Fac for periods            */~
            pfac$(13)1, qfac$(14)1,      /* Fac for prices             */~
            ppfac$(13)1, pqfac$(14)1,    /* Fac for prices             */~
            pflit$(16)20, pflit2$(2)20,  /* PF Key Literals            */~
            pffac$(16)1,                 /* PF Key Facs                */~
            pcode$(13)2,                 /* Price code ID              */~
            price$(13)10, pprice$(13)10, /* Price code                 */~
            price(16),                   /* Price code                 */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            prnames$(10)8,               /* Summary File PR Names      */~
            readkey$56,                  /* SASUMRYn read key          */~
            ship_qty$(13)8,              /* Shipping Units             */~
            ship_qty(13),                /* Shipping Units             */~
            ship_val$(13)10,             /* Shipping Value             */~
            ship_val(13),                /* Shipping Value             */~
            sctotal$10,                  /* Shipping Costs Total       */~
            sutotal$10,                  /* Shipping Units Total       */~
            svtotal$10,                  /* Shipping Value Total       */~
            summ_desc$60,                /* Summary file header descr  */~
            summ_file$1,                 /* Summary file               */~
            this$8,                      /* This SA Year               */~
            totlit$8,                    /* Total Literal              */~
            type_header$30,              /* Screen Type of Maint.      */~
            type_header$(2)30,           /* Screen Type of Maint.      */~
            unit_val$10,                 /* Per Unit Value             */~
            utfac$1,                     /* Unit total FAC             */~
            userid$3,                    /* Current User Id            */~
            valfac$1,                    /* Per Unit Value FAC         */~
            vtfac$1,                     /* Value total FAC            */~
            vpc(2), upc(2), cpc(2),      /* Precision controls - CONV  */~
            vpn(2), upn(2), cpn(2),      /* Precision controls - NUMT  */~
            vps%(2), ups%(2), cps%(2),   /* Precision controls - SPRD  */~
            xfac$(13,4)1,                /* Field Attribute Characters */~
            zarray(65)                   /* Zero Array                 */

        dim                              /* Numeric Arrays             */~
            sa_b_units(13),              /* Booked Units               */~
            sa_b_value(13),              /* Booked Value               */~
            sa_s_units(13),              /* Shipped Units              */~
            sa_s_value(13),              /* Shipped Value              */~
            sa_s_costs(13),              /* Shipped Costs              */~
            st_b_units(13),              /* Booked Units (Target)      */~
            st_b_value(13),              /* Booked Value (Target)      */~
            st_s_units(13),              /* Shipped Units (Target)     */~
            st_s_value(13)               /* Shipped Value (Target)     */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SASUMRY# ! Sales Anaylsis Summary File              *~
            * #2  ! SYSFILE2 ! Caelus Management System Information     *~
            * #3  ! CUSTOMER ! Customer Master File                     *~
            * #4  ! HNYMASTR ! Inventory Master File                    *~
            * #5  ! CATEGORY ! Inventory Category Codes File            *~
            * #6  ! GENCODES ! System General Codes file.               *~
            * #7  ! SLMMASTR ! Salesman master file                     *~
            * #8  ! STORNAME ! Store Information File                   *~
            * #9  ! CPRPRICE ! Selling Prices File                      *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "SASUMRY#",                                      ~
                        varc,     indexed,  recsize =  1048,             ~
                        keypos = 1,    keylen = 56,                      ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup

            select #2,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #3,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #4,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #5,  "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   4

            select #6,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #7,  "SLMMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   4

            select #8,  "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3

            select #9,  "CPRPRICE",                                      ~
                        varc, indexed,                                   ~
                        recsize =  700,                                  ~
                        keypos =     1, keylen =  47

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%, rslt$(4 ))
            call "OPENCHCK" (#5,  fs%(5 ), f2%(5 ), 0%, rslt$(5 ))
            call "OPENCHCK" (#6,  fs%(6 ), f2%(6 ), 0%, rslt$(6 ))
            call "OPENCHCK" (#7,  fs%(7 ), f2%(7 ), 0%, rslt$(7 ))
            call "OPENCHCK" (#8,  fs%(8 ), f2%(8 ), 0%, rslt$(8 ))
            call "OPENCHCK" (#9,  fs%(8 ), f2%(8 ), 0%, rslt$(8 ))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "SHOSTAT" ("Initializing ...")
*        Load S/A Summary File Descriptions
            plowkey$ = "SA.FILES.SASUMRY"
            str(plowkey$, len(plowkey$)+1) = all(hex(00))
L09050:     call "PLOWNEXT" (#2, plowkey$, 16%, f1%(2))
            if f1%(2) = 1% then L09085
                if file% <> 0% then L09110
                call "ASKUSER" (0%, "*** NO S/A FILES ***",              ~
                                "There are no S/A Summary Files Defined",~
                                " ", "Press RETURN to exit program...")
                goto exit_program
L09085:     convert str(plowkey$,17,1) to file% : file% = file% + 1%
            get #2 using L09100, prnames$(file%), descrs$(file%),          ~
                               codes%(file%, 1), codes%(file%, 2)
L09100:         FMT XX(9), CH(8), XX(3), CH(30), XX(2), 2*BI(1)
            goto L09050
L09110
*        Now a friendly warning . . . .
                ask% = 2%
                call "ASKUSER" (ask%, "*** WARNING ***",                 ~
                             "This Program allows maintanence of" &      ~
                             " both Actual and Target Data.  If ",       ~
                             "UPDATE tasks (Sales Order or Invoi" &      ~
                             "cing) are in process, DATA MAY BE LOST.",  ~
                             "Press PF16 to EXIT PROGRAM at this" &      ~
                             " time, or press RETURN to Continue.")
            if ask% = 16% then exit_program
*        First set up a few constants!
            codes$(1,1) = "  Part Number:" : codes$(1,2) = "Part Number"
            codes$(2,1) = "Part Category:" : codes$(2,2) = "Part Category"
            codes$(3,1) = "      Account:" : codes$(3,2) = "Account"
            codes$(4,1) = "      Ship-to:" : codes$(4,2) = "Ship-to"
            codes$(5,1) = "Customer Type:" : codes$(5,2) = "Customer Type"
            codes$(6,1) = "        Store:" : codes$(6,2) = "Store"
            codes$(7,1) = " Sales Region:" : codes$(7,2) = "Sales Region"
            codes$(8,1) = "     Salesman:" : codes$(8,2) = "Salesman"

            codefile%(1) = 4%   :   codelen%(1) = 25%
            codefile%(2) = 5%   :   codelen%(2) =  4%
            codefile%(3) = 3%   :   codelen%(3) =  9%
            codefile%(4) = 3%   :   codelen%(4) =  9%
            codefile%(5) = 6%   :   codelen%(5) =  2%
            codefile%(6) = 8%   :   codelen%(6) =  3%
            codefile%(7) = 6%   :   codelen%(7) =  4%
            codefile%(8) = 7%   :   codelen%(8) =  4%

L09210
*        Now get which Summary File to display data for
            code1$(1), gr1_desc$ = " "
L09220:     plowkey$ = "SA.FILES.SASUMRY"
            if f2%(1) = 0% then close #1
            call "SASUMINP" ("SASUMEDT",                                 ~
                "Select the SA Summary file for Data Management",        ~
                "NY", descrs$(), ret%)

            if ret% = 116% then L65000                /* Why Here??? */
            convert ret% to summ_file$, pic (#)
            sumry% = ret% + 1%
            call "PUTPRNAM" addr (#1, prnames$(sumry%))
            call "OPENCHCK" (#1,  fs%(1), f2%(1), 0%, rslt$(1))
            if f2%(1) <> 0% then L09220
            summ_desc$ = "Summary File #" & summ_file$ & ": " &          ~
                                              descrs$(sumry%)
            group1% = codes%(sumry%, 1%)
            file1%  = codefile%(group1%)
            mat redim code1$(1)codelen%(group1%)
            group1$ = codes$(group1%, 1)

            group2% = codes%(sumry%, 2%)  : file2% = 0%
            group2$ = " "
            part% = 0%
            if group1% = 1% or group2% = 1% then part% = 1%
            if group2% = 0% then L09360
                 file2%  = codefile%(group2%)
                 mat redim code2$(1)codelen%(group2%)
                 group2$ = codes$(group2%, 1)

L09360
*        Now get which year they want, Last, This or Next

            readkey$ = "SWITCHS.SA          "
            call "READ100" (#2, readkey$, f1%(2))
            get #2 using L09382, nonstock$, last$, this$, next$
L09382:         FMT POS(24), CH(1), POS(74), CH(6), POS(152), CH(6),     ~
                    POS(230), CH(6)
            call "DATEFMT" (last$)
            call "DATEFMT" (this$)
            call "DATEFMT" (next$)
L09405:     ask% = 2%
            hi$  = "Press PF1 to Select 'Last Year', Starting on " & last$
            mid$ = "Press PF2 to Select 'This Year', Starting on " & this$
            lo$  = "Press PF3 to Select 'Next Year', Starting on " & next$
            call "ASKUSER" (ask%, "Select SA 'YEAR'", hi$, mid$, lo$)
            if ask% < 1% or ask% > 3% then L09405
            maxper% = 0%
            if ask% = 1% then get #2 using L09440, period$()
L09440:         FMT POS( 74), 13*CH(6)
            if ask% = 2% then get #2 using L09450, period$()
L09450:         FMT POS(152), 13*CH(6)
            if ask% = 3% then get #2 using L09460, period$()
L09460:         FMT POS(230), 13*CH(6)
            for i% = 1% to 13%
                if period$(i%) = " " then L09490
                call "DATEFMT" (period$(i%))
                maxper% = i%
            next i%
L09490:     call "EXTRACT" addr("ID", userid$)
            date$ = date : call "DATEFMT" (date$)
            if maxper% > 0% then L09525
L09502:         ask% = 0%
                call "ASKUSER" (ask%, "*** NO S/A PERIODS ***",          ~
                                "There are no S/A Periods Defined",      ~
                                "Press PF 1 to Reselect Options, or",    ~
                                "Press PF 16 to Exit Program...")
                if ask% = 16% then exit_program
                if ask% <> 1% then L09502
                goto L09405
L09525:     edtmessage$  = " "
            l7_period$ = "Pd.   Date  " : l7_bkgqty$ = "Bkg Units"
            l7_bkgval$ = "Bkg Value"    : l7_shpqty$ = "Shp Units"
            l7_shpval$ = "Shp Value"    : l7_prices$ = "Selling Prices"
            l7_prices$(1) = "Selling Prices"
            l7_prices$(2) = "         Costs"

            init (hex(ff)) keytab$
            str(keytab$,  1, 1) = hex(01)
            str(keytab$, 13, 1) = hex(0d) : str(keytab$, 15, 1) = hex(0f)
            str(keytab$, 16, 1) = hex(10) : str(keytab$, 17, 1) = hex(00)

            pflit2$(1) = "(2)Chg to Actual"
            pflit2$(2) = "(2)Chg to Target"
            pflit$( 6) = "(6)Set Unit Val"
            pflit$( 7) = "(7)Qty Tot Spread"
            pflit$( 8) = "(8)Val Tot Spread"
            pflit$( 9) = "(9)Cst Tot Spread"
            pflit$(10) = "(10)Modify Qty's "
            pflit$(11) = "(11)Modify Val's "
            pflit$(12) = "(12)Delete Recrd "
            totlit$    = "Totals  "

            pcode$( 1) = "A)"  :  pcode$( 2) = "B)"
            pcode$( 3) = "C)"  :  pcode$( 4) = "D)"
            pcode$( 5) = "E)"  :  pcode$( 6) = "F)"
            pcode$( 7) = "G)"  :  pcode$( 8) = "H)"
            pcode$( 9) = "I)"  :  pcode$(10) = "J)"
            pcode$(11) = "K)"  :  pcode$(12) = "L)"
            pcode$(13) = "M)"

            type_header$(1) = "** Manage Target Values **"
            type_header$(2) = "** Manage Actual Values **"

            vpc(1) = 0 : vpc(2) = 0.2
            upc(1) = 0 : upc(2) = 0.2
            cpc(1) = 0 : cpc(2) = 0.4

            vpn(1) = -0.001 : vpn(2) = -0.2
            upn(1) = -0.001 : upn(2) = -0.2
            cpn(1) = -0.001 : cpn(2) = -0.4

            vps%(1) = 0% : vps%(2) = 2%
            ups%(1) = 0% : ups%(2) = 2%
            cps%(1) = 0% : cps%(2) = 4%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode

            type_header$ = " "
            pflit$( 2) = "(2)Slct New File"
            pflit$( 3) = "(3)Slct New Year"
            pflit$(16) = "(16)Exit Program"
            init(" ") errormsg$, inpmessage$, summ_file$, gr2_desc$,     ~
                 code2$(1), linenr$(), gr1_desc$, price$(), unit_val$,   ~
                 book_val$(), ship_val$(), book_qty$(), ship_qty$(),     ~
                 butotal$, sutotal$, bvtotal$, sctotal$, svtotal$

            str(keytab$,  2, 1) = hex(02) : pffac$( 2) = hex(8c)
            str(keytab$,  3, 1) = hex(03) : pffac$( 3) = hex(8c)
            str(keytab$,  6, 1) = hex(ff) : pffac$( 6) = hex(9c)
            str(keytab$,  7, 1) = hex(ff) : pffac$( 7) = hex(9c)
            str(keytab$,  8, 1) = hex(ff) : pffac$( 8) = hex(9c)
            str(keytab$,  9, 1) = hex(ff) : pffac$( 9) = hex(9c)
            str(keytab$, 10, 1) = hex(ff) : pffac$(10) = hex(9c)
            str(keytab$, 11, 1) = hex(ff) : pffac$(11) = hex(9c)
            str(keytab$, 12, 1) = hex(ff) : pffac$(12) = hex(9c)

            utfac$, vtfac$, l7vfac$, l7qfac$, valfac$ = hex(9c)
            init (hex(9c)) pdfac$(), xfac$(), pfac$(), qfac$(),          ~
                           ppfac$(), pqfac$()
            mat book_qty  = zer : mat book_val  = zer : mat price  = zer
            mat ship_qty  = zer : mat ship_val  = zer : mat zarray = zer
            init (" ") linenr$()
            butotal, sutotal, bvtotal, svtotal, unit_val = 0

            mode% = 0%
            mat sa_b_units = zer
            mat sa_b_value = zer
            mat sa_s_units = zer
            mat sa_s_value = zer
            mat sa_s_costs = zer
            mat st_b_units = zer
            mat st_b_value = zer
            mat st_s_units = zer
            mat st_s_value = zer

            for fieldnr% = 1% to 2%
                gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                    if enabled% = 0% then goto L10360
L10300:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                    if keyhit%  =  1% then gosub startover
                    if keyhit%  =  2% then       L09210
                    if keyhit%  =  3% then       L09360
                    if keyhit%  = 16% then goto exit_program
                    if keyhit% <>  0% then goto L10300
L10360:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                    if errormsg$ <> " " then L10300
            next fieldnr%

            for i% = 1% to maxper%
                convert i% to linenr$(i%), pic (##)
                str(linenr$(i%), 3, 1) = "."
            next i%

            gosub dataload   /* Now load data if the record is there */

*        Now get selling prices if part involved
            if part% = 0% then L10620
            if group1% = 1% then plowkey$ = "C" & code1$(1)
            if group2% = 1% then plowkey$ = "C" & code2$(1)
            call "CPRUPDSB" (#9, 0%, "00", plowkey$, 0%, f1%(9))
            if f1%(9) = 0% then L10620
                get #9 using L10540, price()
L10540:              FMT XX(56), 16*PD(14,4)
            init(hex(9c)) ppfac$(), pqfac$()
            for i% = 1% to 13%
                if price(i%) < 0 then L10600
                  call "CONVERT" (price(i%), -2.2, pprice$(i%))
                  ppfac$(i%) = hex(8c): pqfac$(i%) = hex(8c)
L10600:     next i%

L10620:     gosub change_mode

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            inpmessage$ = edtmessage$
            if part% = 0% then L11095
                valfac$ = hex(ac)
L11095:     l7qfac$, l7vfac$ = hex(ac)
            utfac$, vtfac$ = hex(a4)

            for i% = 1% to maxper%
              pdfac$(i%) = hex(8c)
              xfac$(i%,2), xfac$(i%,4) = hex(8c)
              if part% = 0% then L11170
              xfac$(i%,1), xfac$(i%,3) = hex(8c)
L11170:     next i%

            pflit$(16) = "(16)Save Data"
            pflit$( 3) = pflit$( 6)
            str(keytab$,  3, 1) = hex(ff) : pffac$( 3) = hex(9c)
            str(keytab$,  8, 1) = hex(08) : pffac$( 8) = hex(8c)
            str(keytab$,  9, 1) = hex(09) : pffac$( 9) = hex(8c)
            str(keytab$, 11, 1) = hex(0b) : pffac$(11) = hex(8c)
            str(keytab$, 12, 1) = hex(0c) : pffac$(12) = hex(8c)
            if part% = 0% then L11280
            str(keytab$,  6, 1) = hex(06) : pffac$( 3) = hex(8c)
            str(keytab$,  7, 1) = hex(07) : pffac$( 7) = hex(8c)
            str(keytab$, 10, 1) = hex(0a) : pffac$(10) = hex(8c)

L11280:     gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then gosub change_mode
                  if keyhit%  =  6% then gosub per_unit_change
                  if keyhit%  =  7% then gosub qty_spread
                  if keyhit%  =  8% then gosub val_spread
                  if keyhit%  =  9% then gosub cost_spread
                  if keyhit%  = 10% then gosub edit_qty
                  if keyhit%  = 11% then gosub edit_val
                  if keyhit%  = 12% then gosub delete_summary_record
                  if keyhit%  = 16% then goto datasave
                  if keyhit% <>  0% then goto edtpg1
            goto L11280

        REM *************************************************************~
            *     M I S C E L L A N E O U S   S U B R O U T I N E S     *~
            *************************************************************

        capture_codes
*        Get Group Code                        CODEX$
            msg$ = hex(06) & "Select " & codes$(g%,2)
            if file% = 6% then L12150

*         Get code using GETCODE
            call "GETCODE" (#file%, codex$, msg$, 0%, 0, onfile%)
            if onfile% = 1% then return
                if file% = 4% and nonstock$ = "Y" and codex$ <> " " then ~
                                                                    L12130
L12120:         errormsg$ = codes$(g%,2) & " not found on file."
L12130:         return

L12150
*         Get code using PLOWCODE
            if g% = 5% then readkey$ = "CUS TYPES" & codex$
            if g% = 7% then readkey$ = "REGIONS  " & codex$
            call "PLOWCODE" (#file%, readkey$, msg$, 9%, 0.30, onfile%)

            if onfile% = 0% then L12120
                codex$ = str(readkey$,10)
                return

        edit_qty
            for i% = 1% to maxper%
                xfac$(i%,1), xfac$(i%,3) = hex(82)
            next i%
L13040:     gosub'101(3%)
            gosub'151(6%)
            if errormsg$ <> " " then L13040
            for i% = 1% to maxper%
                xfac$(i%,1), xfac$(i%,3) = hex(8c)
                if unit_val = 0 then L13140
                book_val(i%) = round((book_qty(i%) * unit_val),0%)
                ship_val(i%) = round((ship_qty(i%) * unit_val),0%)
                call "CONVERT" (book_val(i%), vpc, book_val$(i%))
                call "CONVERT" (ship_val(i%), vpc, ship_val$(i%))
L13140:     next i%
            goto reset_total_values

        edit_val
            for i% = 1% to maxper%
                xfac$(i%,2), xfac$(i%,4) = hex(82)
                if mode% = 2% then qfac$(i%) = hex(82)
            next i%
L13550:     gosub'101(3%)
            gosub'151(7%)
            if errormsg$ <> " " then L13550
            goto reset_total_values

        qty_spread
            utfac$ = hex(a2)  :  vtfac$ = hex(ac)
L14020:     gosub'101(4%)
            gosub'151(3%)
            if errormsg$ <> " " then L14020
            hi$  = "Press PF4 to Spread the Booking Quantity"
            mid$ = "Press PF5 to Spread the Shipping Quantity"
            lo$  = "Press PF6 to Spread BOTH of the Entered Quantities"
            ask% = 1%
            call"ASKUSER"(ask%,"***** SPREAD WHAT? *****",hi$,mid$,lo$)
            if ask% < 4% or ask% > 6% then clean_up_spread
            if ask% = 5% then L14400

*       *  Spread Booking Quantity
            bqtytemp = round(butotal, ups%)
            for i% = 1% to maxper%
                book_qty(i%) = round((butotal / maxper%),ups%)
                bqtytemp = round(bqtytemp - book_qty(i%), ups%)
                if unit_val = 0 then L14270
                book_val(i%) = round((book_qty(i%) * unit_val), vps%)
L14270:         call "CONVERT" (book_val(i%), vpc, book_val$(i%))
                call "CONVERT" (book_qty(i%), upc, book_qty$(i%))
            next i%

            book_qty(maxper%) = round(book_qty(maxper%) + bqtytemp, ups%)
            if unit_val = 0 then L14340
            book_val(maxper%) = round((book_qty(maxper%) * unit_val),vps%)
L14340:     call "CONVERT" (book_val(maxper%), vpc, book_val$(maxper%))
            call "CONVERT" (book_qty(maxper%), upc, book_qty$(maxper%))
            if ask% = 4% then clean_up_spread    /* Spread Booking Only */

L14400
*       *  Spread Shipping Quantity
            sqtytemp = round(sutotal, ups%)
            for i% = 1% to maxper%
                ship_qty(i%) = round((sutotal / maxper%), ups%)
                sqtytemp = round(sqtytemp - ship_qty(i%), ups%)
                if unit_val = 0 then L14470
                ship_val(i%) = round((ship_qty(i%) * unit_val), vps%)
L14470:         call "CONVERT" (ship_val(i%), vpc, ship_val$(i%))
                call "CONVERT" (ship_qty(i%), upc, ship_qty$(i%))
            next i%

            ship_qty(maxper%) = round(ship_qty(maxper%) + sqtytemp, ups%)
            if unit_val = 0 then L14540
            ship_val(maxper%) = round((ship_qty(maxper%) * unit_val),vps%)
L14540:     call "CONVERT" (ship_val(maxper%), vpc, ship_val$(maxper%))
            call "CONVERT" (ship_qty(maxper%), upc, ship_qty$(maxper%))
            goto clean_up_spread

        val_spread
            vtfac$ = hex(a2)
            if part% = 1% then utfac$ = hex(ac)
L15030:     if part% = 0% then L15050
            inpmessage$ = "Will NOT Recalculate the Per Unit Value!"
L15050:     gosub'101(4%)
            inpmessage$ = " "
            gosub'151(4%)
                if errormsg$ <> " " then L15030
            hi$  = "Press PF7 to Spread the Booking Dollars"
            mid$ = "Press PF8 to Spread the Shipping Dollars"
            lo$  = "Press PF9 to Spread BOTH of the Entered Dollars"
            ask% = 1%
            call"ASKUSER"(ask%,"***** SPREAD WHAT? *****",hi$,mid$,lo$)
            if ask% < 7% or ask% > 9% then clean_up_spread
            if ask% = 8% then L15410

*       *  Spread Booking Dollars
            bvaltemp = round(bvtotal, vps%)
            for i% = 1% to maxper%
                book_val(i%) = round((bvtotal / maxper%), vps%)
                call "CONVERT" (book_val(i%), vpc, book_val$(i%))
                bvaltemp = round(bvaltemp - book_val(i%), vps%)
            next i%
            book_val(maxper%) = round(book_val(maxper%) + bvaltemp, vps%)
            call "CONVERT" (book_val(maxper%), vpc, book_val$(maxper%))
            if ask% = 7% then clean_up_spread /* Spread booking $$ only */

*       * Spread Shipping Dollars
L15410:     svaltemp = round(svtotal, vps%)
            for i% = 1% to maxper%
                ship_val(i%) = round((svtotal / maxper%), vps%)
                call "CONVERT" (ship_val(i%), vpc, ship_val$(i%))
                svaltemp = round(svaltemp - ship_val(i%), vps%)
            next i%
            ship_val(maxper%) = round(ship_val(maxper%) + svaltemp, vps%)
            call "CONVERT" (ship_val(maxper%), vpc, ship_val$(maxper%))
            goto clean_up_spread

        cost_spread
            qfac$(14) = hex(a2)
            if part% = 1% then utfac$ = hex(ac)
L16050:     gosub'101(4%)
            gosub'151(8%)
                if errormsg$ <> " " then L16050
            hi$  = "Press PF10 to Spread the Cost Dollars"
            mid$ = " "
            lo$  = "Press RETURN to EXIT without spread"
            ask% = 1%
            call"ASKUSER"(ask%,"***** COST SPREAD? *****",hi$,mid$,lo$)
            if ask% <> 10% then clean_up_spread

*       *  Spread Cost Dollars
            sctemp = round(sctotal, cps%)
            for i% = 1% to maxper%
                price(i%) = round((sctotal / maxper%), cps%)
                call "CONVERT" (price(i%), cpc, price$(i%))
                sctemp = round(sctemp - price(i%), cps%)
            next i%
            price(maxper%) = round(price(maxper%) + sctemp, cps%)
            call "CONVERT" (price(maxper%), cpc, price$(maxper%))
            goto clean_up_spread

        clean_up_spread
            utfac$ = hex(a4)  :  vtfac$ = hex(a4)
            if mode% = 2% then qfac$(14) = hex(a4)
            goto reset_total_values

        per_unit_change
            valfac$ = hex(a2)
L17020:     gosub'101(4%)
            gosub'151(5%)
            if errormsg$ <> " " then L17020
            for i% = 1% to maxper%
                book_val(i%) = book_qty(i%) * unit_val
                call "CONVERT" (book_val(i%), vpc, book_val$(i%))
                ship_val(i%) = ship_qty(i%) * unit_val
                call "CONVERT" (ship_val(i%), vpc, ship_val$(i%))
            next i%
            valfac$ = hex(a4)
            goto reset_total_values

        reset_total_values
            butotal, sutotal, bvtotal, sctotal, svtotal = 0
            for i% = 1% to maxper%
                xfac$(i%,2), xfac$(i%,4) = hex(8c)
                bvtotal = bvtotal + book_val(i%)
                svtotal = svtotal + ship_val(i%)
                butotal = butotal + book_qty(i%)
                sutotal = sutotal + ship_qty(i%)
                if mode% = 1% then L17610
                   qfac$(i%) = hex(8c)
                   sctotal = sctotal + price(i%)
L17610:     next i%
                call "CONVERT" (bvtotal, vpc, bvtotal$)
                call "CONVERT" (svtotal, vpc, svtotal$)
                call "CONVERT" (butotal, upc, butotal$)
                call "CONVERT" (sutotal, upc, sutotal$)
            if mode% = 1% then return
            call "CONVERT" (sctotal, cpc, sctotal$)
            return

        REM Delete Targrt Records
        delete_summary_record
L18020:     ask% = 2%
            call "ASKUSER" (ask%, "*** DELETE Confirmation ***",         ~
                 "Press PF-28 To Delete this Sales Analysis Record",     ~
                 "- or -",                                               ~
                 "Press RETURN to Return to the Edit Screen")
            if ask% = 0 then return
            if ask% <> 28% then L18020

            call "READ101" (#1, readkey$, f1%(1))
            if f1%(1) = 0 then return
            delete #1
            return clear all
            goto inputmode

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20120,         /* Group Code 1     */~
                                    L20160          /* Group Code 2     */
                  return

L20120: REM Group Code 1                          CODE1$
            inpmessage$ = "Enter a value for " & codes$(group1%, 2%)
            return

L20160: REM Group Code 2                          CODE2$
            if group2% = 0% then enabled% = 0%
            inpmessage$ = "Enter a value for " & codes$(group2%, 2%)
            return


        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode


        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload
            keydate$ = period$(1)  :  call "DATUNFMT" (keydate$)
            readkey$ = keydate$
            str(readkey$,  7, 25) = code1$(1)
            str(readkey$, 32, 25) = code2$(1)
            call "READ100" (#1, readkey$, f1%(1))
            if f1%(1) = 0% then return
            get #1 using L30190, sa_b_units(), sa_b_value(),              ~
                                sa_s_units(), sa_s_value(),              ~
                                sa_s_costs(),                            ~
                                st_b_units(), st_b_value(),              ~
                                st_s_units(), st_s_value()

L30190:     FMT POS(57), 13*PD(14,4), 13*PD(14,4), 13*PD(14,4),          ~
                13*PD(14,4), 13*PD(14,4), 13*PD(14,4), 13*PD(14,4),      ~
                13*PD(14,4), 13*PD(14,4)

            return

        REM *************************************************************~
            * Swap Variables in and out as needed                       *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        change_mode
            if mode% = 0% then L30625
            if mode% = 1% then L30700

            mat sa_b_units = book_qty
            mat sa_b_value = book_val
            mat sa_s_units = ship_qty
            mat sa_s_value = ship_val
            mat sa_s_costs = price

L30625:     mat book_qty = st_b_units
            mat book_val = st_b_value
            mat ship_qty = st_s_units
            mat ship_val = st_s_value

            mat pfac$ = ppfac$
            mat qfac$ = pqfac$
            mat price$ = pprice$
            l7_prices$ = l7_prices$(1)
            vpc = vpc(1) : upc = upc(1) : cpc = cpc(1)
            vpn = vpn(1) : upn = upn(1) : cpn = cpn(1)
            vps% = vps%(1) : ups% = ups%(1) : cps% = cps%(1)
            mode% = 1%
            goto L30800

L30700:     mat st_b_units = book_qty
            mat st_b_value = book_val
            mat st_s_units = ship_qty
            mat st_s_value = ship_val

            mat book_qty = sa_b_units
            mat book_val = sa_b_value
            mat ship_qty = sa_s_units
            mat ship_val = sa_s_value
            mat price    = sa_s_costs

            vpc = vpc(2) : upc = upc(2) : cpc = cpc(2)
            vpn = vpn(2) : upn = upn(2) : cpn = cpn(2)
            vps% = vps%(2) : ups% = ups%(2) : cps% = cps%(2)
            init (hex(9c)) pfac$():init (hex(8c)) qfac$()
            sctotal = 0 : qfac$(14) = hex(a4)
            for i% = 1% to maxper%
                call "CONVERT" (price(i%), cpc, price$(i%))
                sctotal = sctotal + price(i%)
            next i%
            call "CONVERT" (sctotal, cpc, sctotal$)
            l7_prices$ = l7_prices$(2)
            mode% = 2%

L30800:     butotal, sutotal, bvtotal, svtotal = 0
            for i% = 1% to maxper%
                call "CONVERT" (book_qty(i%), upc, book_qty$(i%))
                call "CONVERT" (book_val(i%), vpc, book_val$(i%))
                call "CONVERT" (ship_qty(i%), upc, ship_qty$(i%))
                call "CONVERT" (ship_val(i%), vpc, ship_val$(i%))
                butotal = butotal + book_qty(i%)
                bvtotal = bvtotal + book_val(i%)
                sutotal = sutotal + ship_qty(i%)
                svtotal = svtotal + ship_val(i%)
            next i%
            call "CONVERT" (butotal, upc, butotal$)
            call "CONVERT" (sutotal, upc, sutotal$)
            call "CONVERT" (svtotal, vpc, svtotal$)
            call "CONVERT" (bvtotal, vpc, bvtotal$)

            pflit$( 2) = pflit2$(mode%)
            type_header$ = type_header$(mode%)

            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        dataput
            gosub change_mode
            call "READ101" (#1, readkey$, f1%(1))
            if f1%(1) = 0 then L31210
            put #1 using L30190, sa_b_units(), sa_b_value(),              ~
                                sa_s_units(), sa_s_value(),              ~
                                sa_s_costs(),                            ~
                                st_b_units(), st_b_value(),              ~
                                st_s_units(), st_s_value()

            rewrite #1
            return

L31210:     put #1 using L31290, keydate$, code1$(1), code2$(1),          ~
                                sa_b_units(), sa_b_value(),              ~
                                sa_s_units(), sa_s_value(),              ~
                                sa_s_costs(),                            ~
                                st_b_units(), st_b_value(),              ~
                                st_s_units(), st_s_value(),              ~
                    code2$(1), keydate$, code1$(1)

            write #1
            return


L31290:     FMT /*  Record Layout in file SASUMRYn                     */~
                CH(  6),                /* Year Key                 */   ~
                CH( 25),                /* Code 1                   */   ~
                CH( 25),                /* Code 2                   */   ~
                13*PD(14,4),            /* Actual Booking Units     */   ~
                13*PD(14,4),            /* Actual Booking Value     */   ~
                13*PD(14,4),            /* Actual Shipping Units    */   ~
                13*PD(14,4),            /* Actual Shipping Value    */   ~
                13*PD(14,4),            /* Actual Shipping Costs    */   ~
                13*PD(14,4),            /* Target Booking Units     */   ~
                13*PD(14,4),            /* Target Booking Value     */   ~
                13*PD(14,4),            /* Target Shipping Units    */   ~
                13*PD(14,4),            /* Target Shipping Value    */   ~
                CH( 25),                /* Code 2                   */   ~
                CH(  6),                /* Year Key                 */   ~
                CH( 25)                 /* Code 1                   */


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                line2$ = summ_desc$
                str(line2$,62%) = "SASUMEDT: " & str(cms2v$,,8%)
                if mode% <> 2% then L40087
                   str(keytab$,  9, 1) = hex(09) : pffac$( 9) = hex(8c)
                      goto L40090
L40087:            str(keytab$,  9, 1) = hex(ff) : pffac$( 9) = hex(9c)
L40090:         if fieldnr% = 3% then L40200
                if fieldnr% > 3% then L40220
                if fieldnr% > 0% then init(hex(8c)) lfac$()              ~
                                 else init(hex(84)) lfac$()
                on fieldnr% gosub L40180,           /* Group Code 1     */~
                                  L40180            /* Group Code 2     */
                goto L40220

                lfac$(fieldnr%) = hex(80)  :  return  /* Up - Low    */
L40180:         lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only  */

L40200:     init (hex(84)) lfac$()

L40220:     accept                                                       ~
               at (01,02),                                               ~
                  "Sales Analysis Summary Management",                   ~
               at (01,37), fac(hex(84)), type_header$           , ch(30),~
               at (01,66), "Today:"                             ,        ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), fac(hex(8c))  , group1$              , ch(14),~
               at (04,17), fac(lfac$( 1)), code1$(1)            ,        ~
               at (04,43), fac(hex(8c))  , gr1_desc$            , ch(34),~
                                                                         ~
               at (05,02), fac(hex(8c))  , group2$              , ch(14),~
               at (05,17), fac(lfac$( 2)), code2$(1)            ,        ~
               at (05,43), fac(hex(8c))  , gr2_desc$            , ch(34),~
                                                                         ~
               at (06,02), fac(l7vfac$)  , l7_period$           , ch(12),~
               at (06,18), fac(l7qfac$)  , l7_bkgqty$           , ch( 9),~
               at (06,30), fac(l7vfac$)  , l7_bkgval$           , ch( 9),~
               at (06,42), fac(l7qfac$)  , l7_shpqty$           , ch( 9),~
               at (06,54), fac(l7vfac$)  , l7_shpval$           , ch( 9),~
               at (06,66), fac(l7qfac$)  , l7_prices$           , ch(14),~
                                                                         ~
               at (07,02), fac(pdfac$( 1))  , linenr$( 1)       , ch(03),~
               at (07,06), fac(pdfac$( 1))  , period$( 1)       , ch(08),~
               at (07,19), fac(xfac$( 1,1)), book_qty$(1),        ch( 8),~
               at (07,29), fac(xfac$( 1,2)), book_val$(1),        ch(10),~
               at (07,43), fac(xfac$( 1,3)), ship_qty$(1),        ch( 8),~
               at (07,53), fac(xfac$( 1,4)), ship_val$(1),        ch(10),~
               at (07,66), fac(pfac$( 1))  , pcode$(1)          , ch( 2),~
               at (07,70), fac(qfac$( 1))  , price$(1)          , ch(10),~
                                                                         ~
               at (08,02), fac(pdfac$( 2))  , linenr$( 2)       , ch(03),~
               at (08,06), fac(pdfac$( 2))  , period$( 2)       , ch(08),~
               at (08,19), fac(xfac$( 2,1)), book_qty$(2),        ch( 8),~
               at (08,29), fac(xfac$( 2,2)), book_val$(2),        ch(10),~
               at (08,43), fac(xfac$( 2,3)), ship_qty$(2),        ch( 8),~
               at (08,53), fac(xfac$( 2,4)), ship_val$(2),        ch(10),~
               at (08,66), fac(pfac$( 2))  , pcode$(2)          , ch( 2),~
               at (08,70), fac(qfac$( 2))  , price$(2)          , ch(10),~
                                                                         ~
               at (09,02), fac(pdfac$( 3))  , linenr$( 3)       , ch(03),~
               at (09,06), fac(pdfac$( 3))  , period$( 3)       , ch(08),~
               at (09,19), fac(xfac$( 3,1)), book_qty$(3),        ch( 8),~
               at (09,29), fac(xfac$( 3,2)), book_val$(3),        ch(10),~
               at (09,43), fac(xfac$( 3,3)), ship_qty$(3),        ch( 8),~
               at (09,53), fac(xfac$( 3,4)), ship_val$(3),        ch(10),~
               at (09,66), fac(pfac$( 3))  , pcode$(3)          , ch( 2),~
               at (09,70), fac(qfac$( 3))  , price$(3)          , ch(10),~
                                                                         ~
               at (10,02), fac(pdfac$( 4))  , linenr$( 4)       , ch(03),~
               at (10,06), fac(pdfac$( 4))  , period$( 4)       , ch(08),~
               at (10,19), fac(xfac$( 4,1)), book_qty$(4),        ch( 8),~
               at (10,29), fac(xfac$( 4,2)), book_val$(4),        ch(10),~
               at (10,43), fac(xfac$( 4,3)), ship_qty$(4),        ch( 8),~
               at (10,53), fac(xfac$( 4,4)), ship_val$(4),        ch(10),~
               at (10,66), fac(pfac$( 4))  , pcode$(4)          , ch( 2),~
               at (10,70), fac(qfac$( 4))  , price$(4)          , ch(10),~
                                                                         ~
               at (11,02), fac(pdfac$( 5))  , linenr$( 5)       , ch(03),~
               at (11,06), fac(pdfac$( 5))  , period$( 5)       , ch(08),~
               at (11,19), fac(xfac$( 5,1)), book_qty$(5),        ch( 8),~
               at (11,29), fac(xfac$( 5,2)), book_val$(5),        ch(10),~
               at (11,43), fac(xfac$( 5,3)), ship_qty$(5),        ch( 8),~
               at (11,53), fac(xfac$( 5,4)), ship_val$(5),        ch(10),~
               at (11,66), fac(pfac$( 5))  , pcode$(5)          , ch( 2),~
               at (11,70), fac(qfac$( 5))  , price$(5)          , ch(10),~
                                                                         ~
               at (12,02), fac(pdfac$( 6))  , linenr$( 6)       , ch(03),~
               at (12,06), fac(pdfac$( 6))  , period$( 6)       , ch(08),~
               at (12,19), fac(xfac$( 6,1)), book_qty$(6),        ch( 8),~
               at (12,29), fac(xfac$( 6,2)), book_val$(6),        ch(10),~
               at (12,43), fac(xfac$( 6,3)), ship_qty$(6),        ch( 8),~
               at (12,53), fac(xfac$( 6,4)), ship_val$(6),        ch(10),~
               at (12,66), fac(pfac$( 6))  , pcode$(6)          , ch( 2),~
               at (12,70), fac(qfac$( 6))  , price$(6)          , ch(10),~
                                                                         ~
               at (13,02), fac(pdfac$( 7))  , linenr$( 7)       , ch(03),~
               at (13,06), fac(pdfac$( 7))  , period$( 7)       , ch(08),~
               at (13,19), fac(xfac$( 7,1)), book_qty$(7),        ch( 8),~
               at (13,29), fac(xfac$( 7,2)), book_val$(7),        ch(10),~
               at (13,43), fac(xfac$( 7,3)), ship_qty$(7),        ch( 8),~
               at (13,53), fac(xfac$( 7,4)), ship_val$(7),        ch(10),~
               at (13,66), fac(pfac$( 7))  , pcode$(7)          , ch( 2),~
               at (13,70), fac(qfac$( 7))  , price$(7)          , ch(10),~
                                                                         ~
               at (14,02), fac(pdfac$( 8))  , linenr$( 8)       , ch(03),~
               at (14,06), fac(pdfac$( 8))  , period$( 8)       , ch(08),~
               at (14,19), fac(xfac$( 8,1)), book_qty$(8),        ch( 8),~
               at (14,29), fac(xfac$( 8,2)), book_val$(8),        ch(10),~
               at (14,43), fac(xfac$( 8,3)), ship_qty$(8),        ch( 8),~
               at (14,53), fac(xfac$( 8,4)), ship_val$(8),        ch(10),~
               at (14,66), fac(pfac$( 8))  , pcode$(8)          , ch( 2),~
               at (14,70), fac(qfac$( 8))  , price$(8)          , ch(10),~
                                                                         ~
               at (15,02), fac(pdfac$( 9))  , linenr$( 9)       , ch(03),~
               at (15,06), fac(pdfac$( 9))  , period$( 9)       , ch(08),~
               at (15,19), fac(xfac$( 9,1)), book_qty$(9),        ch( 8),~
               at (15,29), fac(xfac$( 9,2)), book_val$(9),        ch(10),~
               at (15,43), fac(xfac$( 9,3)), ship_qty$(9),        ch( 8),~
               at (15,53), fac(xfac$( 9,4)), ship_val$(9),        ch(10),~
               at (15,66), fac(pfac$( 9))  , pcode$(9)          , ch( 2),~
               at (15,70), fac(qfac$( 9))  , price$(9)          , ch(10),~
                                                                         ~
               at (16,02), fac(pdfac$(10))  , linenr$(10)       , ch(03),~
               at (16,06), fac(pdfac$(10))  , period$(10)       , ch(08),~
               at (16,19), fac(xfac$(10,1)), book_qty$(10),       ch( 8),~
               at (16,29), fac(xfac$(10,2)), book_val$(10),       ch(10),~
               at (16,43), fac(xfac$(10,3)), ship_qty$(10),       ch( 8),~
               at (16,53), fac(xfac$(10,4)), ship_val$(10),       ch(10),~
               at (16,66), fac(pfac$(10))  , pcode$(10)         , ch( 2),~
               at (16,70), fac(qfac$(10))  , price$(10)         , ch(10),~
                                                                         ~
               at (17,02), fac(pdfac$(11))  , linenr$(11)       , ch(03),~
               at (17,06), fac(pdfac$(11))  , period$(11)       , ch(08),~
               at (17,19), fac(xfac$(11,1)), book_qty$(11),       ch( 8),~
               at (17,29), fac(xfac$(11,2)), book_val$(11),       ch(10),~
               at (17,43), fac(xfac$(11,3)), ship_qty$(11),       ch( 8),~
               at (17,53), fac(xfac$(11,4)), ship_val$(11),       ch(10),~
               at (17,66), fac(pfac$(11))  , pcode$(11)         , ch( 2),~
               at (17,70), fac(qfac$(11))  , price$(11)         , ch(10),~
                                                                         ~
               at (18,02), fac(pdfac$(12))  , linenr$(12)       , ch(03),~
               at (18,06), fac(pdfac$(12))  , period$(12)       , ch(08),~
               at (18,19), fac(xfac$(12,1)), book_qty$(12),       ch( 8),~
               at (18,29), fac(xfac$(12,2)), book_val$(12),       ch(10),~
               at (18,43), fac(xfac$(12,3)), ship_qty$(12),       ch( 8),~
               at (18,53), fac(xfac$(12,4)), ship_val$(12),       ch(10),~
               at (18,66), fac(pfac$(12))  , pcode$(12)         , ch( 2),~
               at (18,70), fac(qfac$(12))  , price$(12)         , ch(10),~
                                                                         ~
               at (19,02), fac(pdfac$(13))  , linenr$(13)       , ch(03),~
               at (19,06), fac(pdfac$(13))  , period$(13)       , ch(08),~
               at (19,19), fac(xfac$(13,1)), book_qty$(13)      , ch( 8),~
               at (19,29), fac(xfac$(13,2)), book_val$(13)      , ch(10),~
               at (19,43), fac(xfac$(13,3)), ship_qty$(13)      , ch( 8),~
               at (19,53), fac(xfac$(13,4)), ship_val$(13)      , ch(10),~
               at (19,66), fac(pfac$(13))  , pcode$(13)         , ch( 2),~
               at (19,70), fac(qfac$(13))  , price$(13)         , ch(10),~
                                                                         ~
                                                                         ~
               at (20,08), fac(l7vfac$)    , totlit$,             ch( 8),~
               at (20,17), fac(utfac$)     , butotal$,            ch(10),~
               at (20,29), fac(vtfac$)     , bvtotal$,            ch(10),~
               at (20,41), fac(utfac$)     , sutotal$,            ch(10),~
               at (20,53), fac(vtfac$)     , svtotal$,            ch(10),~
               at (20,70), fac(qfac$(14))  , sctotal$,            ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
               at (22,02), "(1)Start Over",                              ~
               at (22,30), fac(pffac$( 7)), pflit$( 7),                  ~
               at (22,48), fac(pffac$(10)), pflit$(10),                  ~
               at (22,65), "(13)Instructions",                           ~
                                                                         ~
               at (23,02), fac(pffac$( 2)), pflit$( 2),                  ~
               at (23,30), fac(pffac$( 8)), pflit$( 8),                  ~
               at (23,48), fac(pffac$(11)), pflit$(11),                  ~
               at (23,65), "(15)Print Screen",                           ~
                                                                         ~
               at (24,02), fac(pffac$( 3)), pflit$( 3),                  ~
               at (24,19), fac(valfac$)   , unit_val$,            ch(10),~
               at (24,30), fac(pffac$( 9)), pflit$( 9),                  ~
               at (24,48), fac(pffac$(12)), pflit$(12),                  ~
               at (24,65), fac(hex(8c)),    pflit$(16),           ch(16),~
                                                                         ~
               keys(keytab$), key (keyhit%)

               if keyhit% <> 13 then L41920
                  call "MANUAL" ("SASUMEDT")
                  goto L40220

L41920:        if keyhit% <> 15 then L41960
                  call "PRNTSCRN"
                  goto L40220

L41960:        if fieldnr% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50200,         /* Group Code 1     */~
                                    L50400,         /* Group Code 2     */~
                                    L50600,         /* Quantity Spread  */~
                                    L50800,         /* Value Spread     */~
                                    L51000,         /* Per Unit Value   */~
                                    L51200,         /* Line Quantities  */~
                                    L51400,         /* Line Values      */~
                                    L51600          /* Cost Spread      */
                  return

L50200: REM Group Code 1                          CODE1$
            g% = group1%
            codex$ = code1$(1) : file% = file1%
            gosub capture_codes
            code1$(1) = codex$
            gr1_desc$ = msg$
            return

L50400: REM Group Code 2                          CODE2$
            if enabled% = 0% then return
            g% = group2%
            codex$ = code2$(1) : file% = file2%
            gosub capture_codes
            code2$(1) = codex$
            gr2_desc$ = msg$
            return

L50600: REM Test Total Quantity for Spread
            call"NUMTEST"(butotal$,-1.3e9,1.3e9,errormsg$, upn, butotal)
                if errormsg$ <> " " then return
            call"NUMTEST"(sutotal$,-1.3e9,1.3e9,errormsg$, upn, sutotal)
            return

L50800: REM Test Total Value for Spread
            call"NUMTEST"(bvtotal$,-1.3e9,1.3e9,errormsg$, vpn, bvtotal)
                if errormsg$ <> " " then return
            call"NUMTEST"(svtotal$,-1.3e9,1.3e9,errormsg$, vpn, svtotal)
            return

L51000: REM Test Per Unit Value
            call"NUMTEST"(unit_val$, 0, 9e7, errormsg$, 0.2, unit_val)
            return

L51200: REM Test Line Quantity Entry
            for j% = 1% to maxper%
                call "NUMTEST" (book_qty$(j%), -9.9e7, 9.9e7,            ~
                                errormsg$, upn, book_qty(j%))
                if errormsg$ <> " " then return
                call "NUMTEST" (ship_qty$(j%), -9.9e7, 9.9e7,            ~
                                errormsg$, upn, ship_qty(j%))
                if errormsg$ <> " " then return
            next j%
            return

L51400: REM Test Line Value Entry
            for j% = 1% to maxper%
                call "NUMTEST" (book_val$(j%), -9.9e7, 9.9e7,            ~
                                errormsg$, vpn, book_val(j%))
                if errormsg$ <> " " then return
                call "NUMTEST" (ship_val$(j%), -9.9e7, 9.9e7,            ~
                                errormsg$, vpn, ship_val(j%))
                if errormsg$ <> " " then return
            if mode% = 1% then L51460
                call "NUMTEST" (price$(j%), -9.9e7, 9.9e7,               ~
                                errormsg$, cpn, price(j%))
                if errormsg$ <> " " then return
L51460:     next j%
            return

L51600: REM Test Total Cost for Spread
            call"NUMTEST"(sctotal$,-1.3e9,1.3e9,errormsg$, cpn, sctotal)
            return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
