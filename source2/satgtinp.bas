        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS    AAA   TTTTT   GGG   TTTTT  IIIII  N   N  PPPP    *~
            *  S      A   A    T    G        T      I    NN  N  P   P   *~
            *   SSS   AAAAA    T    G GGG    T      I    N N N  PPPP    *~
            *      S  A   A    T    G   G    T      I    N  NN  P       *~
            *   SSS   A   A    T     GGG     T    IIIII  N   N  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SATGTINP - Allows input and edit of target fields in the  *~
            *            Sales Analysis Summary files.                  *~
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
            * 01/13/87 ! Original                                 ! MJB *~
            * 05/14/87 ! Standard Costing Changes                 ! ERN *~
            * 08/24/88 ! Fixed setting of MAXPER% if 13 periods.  ! RJM *~
            * 09/16/88 ! 1-Added Delete Target Record Function.   ! MJB *~
            *          ! 2-Added close on summary file when re-   !     *~
            *          !   selecting to access different file.    !     *~
            * 09/20/88 ! Fixed Askuser Restart if Periods Blank.  ! RJM *~
            * 02/02/89 ! Proj 7880714 new price set implementation! JIM *~
            * 05/08/92 ! Clarify Delete Message (THE WHOLE RECORD)! KAB *~
            *          !   To Remove Delete capability from this  !     *~
            *          !   program, alternate lines at 11201,     !     *~
            *          !   40087/89 are present but *'d out.      !     *~
            *          !   This would be even safer.              !     *~
            * 08/21/96 ! Century date conversion                  ! DER *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankdate$8,                 /* blank unfmt date           */~
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
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            linenr$(13)3,                /* Edited line numbers        */~
            mid$80,                      /* Line 2 for ASKUSER         */~
            msg$79,                      /* Miscellaneous messages     */~
            next$8,                      /* Next SA Year               */~
            period$(13)8,                /* Period dates               */~
            pdfac$(13)1,                 /* Fac for periods            */~
            pfac$(13)1,                  /* Fac for prices             */~
            pflit$(15)19,                /* PF Key Literals            */~
            pffac$(15)1,                 /* PF Key Facs                */~
            pf16$16,                     /* PF16 Literal               */~
            pcode$(13)2,                 /* Price code ID              */~
            price$(13)10,                /* Price code                 */~
            price(16),                   /* Price code                 */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            prnames$(10)8,               /* Summary File PR Names      */~
            readkey$56,                  /* SASUMRYn read key          */~
            ship_qty$(13)8,              /* Shipping Units             */~
            ship_qty(13),                /* Shipping Units             */~
            ship_val$(13)10,             /* Shipping Value             */~
            ship_val(13),                /* Shipping Value             */~
            sutotal$10,                  /* Shipping Units Total       */~
            svtotal$10,                  /* Shipping Value Total       */~
            summ_desc$60,                /* Summary file header descr  */~
            summ_file$1,                 /* Summary file               */~
            this$8,                      /* This SA Year               */~
            totlit$8,                    /* Total Literal              */~
            unit_val$10,                 /* Per Unit Value             */~
            utfac$1,                     /* Unit total FAC             */~
            userid$3,                    /* Current User Id            */~
            valfac$1,                    /* Per Unit Value FAC         */~
            vtfac$1,                     /* Value total FAC            */~
            xfac$(13,4)1,                /* Field Attribute Characters */~
            zarray(65)                   /* Zero Array                 */

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
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
*        Load S/A Summary File Descriptions
            plowkey$ = "SA.FILES.SASUMRY"
            str(plowkey$, len(plowkey$)+1) = all(hex(00))
L09050:     call "PLOWNEXT" (#2, plowkey$, 16%, f1%(2))
            if f1%(2) = 1% then L09085
                if file% <> 0% then L09115
                call "ASKUSER" (0%, "*** NO S/A FILES ***",              ~
                                "There are no S/A Summary Files Defined",~
                                " ", "Press RETURN to exit program...")
                goto exit_program
L09085:     convert str(plowkey$,17,1) to file% : file% = file% + 1%
            get #2 using L09100, prnames$(file%), descrs$(file%),          ~
                               codes%(file%, 1), codes%(file%, 2)
L09100:         FMT XX(9), CH(8), XX(3), CH(30), XX(2), 2*BI(1)
            goto L09050

L09115
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
            call "SASUMINP" ("SATGTINP",                                 ~
                "Select the SA Summary file for Target Input",           ~
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
            get #2 using L09385, last$, this$, next$
L09385:         FMT POS(74), CH(6), POS(152), CH(6), POS(230), CH(6)
            call "DATEFMT" (last$)
            call "DATEFMT" (this$)
            call "DATEFMT" (next$)
L09405:     ask% = 2%
            hi$ = "Press PF1 to Select 'Last Year', Starting on " & last$
            mid$ = "Press PF2 to Select 'This Year', Starting on " & this$
            lo$ = "Press PF3 to Select 'Next Year', Starting on " & next$
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
                call "DATEFMT" (period$(i%))
            next i%
            for i% = 1% to 13%
                if period$(i%) = " " or period$(i%) = blankdate$ then L09490
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
            init (hex(ff)) keytab$
            str(keytab$,  1, 1) = hex(01) : str(keytab$,  4, 1) = hex(04)
            str(keytab$, 13, 1) = hex(0d) : str(keytab$, 15, 1) = hex(0f)
            str(keytab$, 16, 1) = hex(10) : str(keytab$, 17, 1) = hex(00)

            pflit$( 2) = "(2)Select New File "
            pflit$( 3) = "(3)Select New Year "
            pflit$( 7) = "(7)Qty Total Spread"
            pflit$( 8) = "(8)Val Total Spread"
            pflit$( 9) = "(9)Set Unit Value  "
            pflit$(10) = "(10)Modify Qty's   "
            pflit$(11) = "(11)Modify Val's   "
            totlit$    = "Totals  "

            pcode$( 1) = "A)"  :  pcode$( 2) = "B)"
            pcode$( 3) = "C)"  :  pcode$( 4) = "D)"
            pcode$( 5) = "E)"  :  pcode$( 6) = "F)"
            pcode$( 7) = "G)"  :  pcode$( 8) = "H)"
            pcode$( 9) = "I)"  :  pcode$(10) = "J)"
            pcode$(11) = "K)"  :  pcode$(12) = "L)"
            pcode$(13) = "M)"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode

            pf16$ = "(16)Exit Program"
            init(" ") errormsg$, inpmessage$, summ_file$, gr2_desc$,     ~
                 code2$(1), linenr$(), gr1_desc$, price$(), unit_val$,   ~
                 book_val$(), ship_val$(), book_qty$(), ship_qty$(),     ~
                 butotal$, sutotal$, bvtotal$, svtotal$

            str(keytab$,  2, 1) = hex(02) : pffac$( 2) = hex(8c)
            str(keytab$,  3, 1) = hex(03) : pffac$( 3) = hex(8c)
            str(keytab$,  7, 1) = hex(ff) : pffac$( 7) = hex(9c)
            str(keytab$,  8, 1) = hex(ff) : pffac$( 8) = hex(9c)
            str(keytab$,  9, 1) = hex(ff) : pffac$( 9) = hex(9c)
            str(keytab$, 10, 1) = hex(ff) : pffac$(10) = hex(9c)
            str(keytab$, 11, 1) = hex(ff) : pffac$(11) = hex(9c)

            utfac$, vtfac$, l7vfac$, l7qfac$, valfac$ = hex(9c)
            init (hex(9c)) pdfac$(), xfac$(), pfac$()
            mat book_qty  = zer : mat book_val  = zer : mat price  = zer
            mat ship_qty  = zer : mat ship_val  = zer : mat zarray = zer
            init (" ") linenr$()
            butotal, sutotal, bvtotal, svtotal, unit_val = 0

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
            if part% = 0% then L11000
            if group1% = 1% then plowkey$ = "C" & code1$(1)
            if group2% = 1% then plowkey$ = "C" & code2$(1)
            call "CPRUPDSB" (#9, 0%, "00", plowkey$, 0%, f1%(9))
            if f1%(9) = 0% then L11000
                get #9 using L10540, price()
L10540:              FMT XX(56), 16*PD(14,4)
            init(hex(9c)) pfac$()
            for i% = 1% to 13%
                if price(i%) < 0 then L10600
                  call "CONVERT" (price(i%), -2.2, price$(i%))
                  pfac$(i%) = hex(8c)
L10600:     next i%


L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            inpmessage$ = edtmessage$
            if part% = 0% then L11100
            valfac$, l7qfac$ = hex(ac)  :  utfac$ = hex(a4)
L11100:     vtfac$ = hex(a4)
            l7vfac$ = hex(ac)
            for i% = 1% to maxper%
              pdfac$(i%) = hex(8c)
              xfac$(i%,2), xfac$(i%,4) = hex(8c)
              if part% = 0% then L11170
              xfac$(i%,1), xfac$(i%,3) = hex(8c)
L11170:     next i%
            pf16$ = "(16)Save Data"
            str(keytab$,  2, 1) = hex(ff) : pffac$( 2) = hex(9c)
            str(keytab$,  3, 1) = hex(0c) : pffac$( 3) = hex(8c)
*          STR(KEYTAB$,  3, 1) = HEX(FF) : PFFAC$( 3) = HEX(94)
            str(keytab$,  8, 1) = hex(08) : pffac$( 8) = hex(8c)
            str(keytab$, 11, 1) = hex(0b) : pffac$(11) = hex(8c)
            if part% = 0% then L11280
            str(keytab$,  7, 1) = hex(07) : pffac$( 7) = hex(8c)
            str(keytab$,  9, 1) = hex(09) : pffac$( 9) = hex(8c)
            str(keytab$, 10, 1) = hex(0a) : pffac$(10) = hex(8c)

L11280:     gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  7% then gosub qty_spread
                  if keyhit%  =  8% then gosub val_spread
                  if keyhit%  =  9% then gosub per_unit_change
                  if keyhit%  = 10% then gosub edit_qty
                  if keyhit%  = 11% then gosub edit_val
                  if keyhit%  = 12% then gosub delete_target
                  if keyhit%  = 16% then goto datasave
                  if keyhit% <>  0% then goto edtpg1
            goto L11280

        REM *************************************************************~
            *     M I S C E L L A N E O U S   S U B R O U T I N E S     *~
            *************************************************************

        capture_codes
*        Get Group Code                        CODEX$
            msg$ = hex(06) & "Select " & codes$(g%,2)
            if file% = 6% then L15150

*         Get code using GETCODE
            call "GETCODE" (#file%, codex$, msg$, 0%, 0, onfile%)
            if onfile% = 1% then return
L15120:         errormsg$ = codes$(g%,2) & " not found on file."
                return

L15150
*         Get code using PLOWCODE
            if g% = 5% then readkey$ = "CUS TYPES" & codex$
            if g% = 7% then readkey$ = "REGIONS  " & codex$
            call "PLOWCODE" (#file%, readkey$, msg$, 9%, 0.30, onfile%)

            if onfile% = 0% then L15120
                codex$ = str(readkey$,10)
                return

        edit_qty
            for i% = 1% to maxper%
                xfac$(i%,1), xfac$(i%,3) = hex(82)
            next i%
L15280:     gosub'101(3%)
            gosub'151(6%)
            if errormsg$ <> " " then L15280
            butotal, sutotal = 0
            for i% = 1% to maxper%
                xfac$(i%,1), xfac$(i%,3) = hex(8c)
                butotal = butotal + book_qty(i%)
                sutotal = sutotal + ship_qty(i%)
                if unit_val = 0 then L15440
                bvtotal, svtotal = 0
                book_val(i%) = round((book_qty(i%) * unit_val),0%)
                ship_val(i%) = round((ship_qty(i%) * unit_val),0%)
                call "CONVERT" (book_val(i%), 0.0, book_val$(i%))
                call "CONVERT" (ship_val(i%), 0.0, ship_val$(i%))
                bvtotal = bvtotal + book_val(i%)
                svtotal = svtotal + ship_val(i%)
L15440:     next i%
                call "CONVERT" (butotal, 0.0, butotal$)
                call "CONVERT" (sutotal, 0.0, sutotal$)
                call "CONVERT" (svtotal, 0.0, svtotal$)
                call "CONVERT" (bvtotal, 0.0, bvtotal$)
            return

        edit_val
            for i% = 1% to maxper%
                xfac$(i%,2), xfac$(i%,4) = hex(82)
            next i%
L15550:     gosub'101(3%)
            gosub'151(7%)
            if errormsg$ <> " " then L15550
            bvtotal, svtotal = 0
            for i% = 1% to maxper%
                xfac$(i%,2), xfac$(i%,4) = hex(8c)
                bvtotal = bvtotal + book_val(i%)
                svtotal = svtotal + ship_val(i%)
            next i%
                call "CONVERT" (svtotal, 0.0, svtotal$)
                call "CONVERT" (bvtotal, 0.0, bvtotal$)
            return


        qty_spread
            utfac$ = hex(a2)  :  vtfac$ = hex(ac)
L15710:     gosub'101(4%)
            gosub'151(3%)
            if errormsg$ <> " " then L15710
            hi$  = "Press PF4 to Spread the Target Booking Quantity"
            mid$ = "Press PF5 to Spread the Target Shipping Quantity"
            lo$  = "Press PF6 to Spread BOTH of the Entered Quantities"
L15770:     ask% = 1%
            call"ASKUSER"(ask%,"***** SPREAD WHAT? *****",hi$,mid$,lo$)
            if ask% < 4% or ask% > 6% then L15770
            if ask% = 5% then L16060

*       *  Spread Booking Quantity
            bqtytemp% = butotal
            bvtotal = 0
            for i% = 1% to maxper%
                book_qty(i%) = round((butotal / maxper%),0)
                bqtytemp% = bqtytemp% - book_qty(i%)
                if unit_val = 0 then L15900
                book_val(i%) = round((book_qty(i%) * unit_val),0)
L15900:         call "CONVERT" (book_val(i%), 0.0, book_val$(i%))
                call "CONVERT" (book_qty(i%), 0.0, book_qty$(i%))
                if i% = maxper% then L15940
                bvtotal = bvtotal + book_val(i%)
L15940:     next i%
            book_qty(maxper%) = book_qty(maxper%) + bqtytemp%
            if unit_val = 0 then L15980
            book_val(maxper%) = round((book_qty(maxper%) * unit_val),0)
L15980:     call "CONVERT" (book_val(maxper%), 0.0, book_val$(maxper%))
            call "CONVERT" (book_qty(maxper%), 0.0, book_qty$(maxper%))
            bvtotal = bvtotal + book_val(maxper%)
            call "CONVERT" (butotal, 0.0, butotal$)
            call "CONVERT" (bvtotal, 0.0, bvtotal$)
            utfac$ = hex(a4)  :  vtfac$ = hex(a4)
            if ask% = 4% then return         /* Spread Booking Only */

L16060
*       *  Spread Shipping Quantity
            sqtytemp% = sutotal
            svtotal = 0
            for i% = 1% to maxper%
                ship_qty(i%) = round((sutotal / maxper%),0)
                sqtytemp% = sqtytemp% - ship_qty(i%)
                if unit_val = 0 then L16140
                ship_val(i%) = round((ship_qty(i%) * unit_val),0)
L16140:         call "CONVERT" (ship_val(i%), 0.0, ship_val$(i%))
                call "CONVERT" (ship_qty(i%), 0.0, ship_qty$(i%))
                if i% = maxper% then L16180
                svtotal = svtotal + ship_val(i%)
L16180:     next i%
            ship_qty(maxper%) = ship_qty(maxper%) + sqtytemp%
            if unit_val = 0 then L16220
            ship_val(maxper%) = round((ship_qty(maxper%) * unit_val),0)
L16220:     call "CONVERT" (ship_val(maxper%), 0.0, ship_val$(maxper%))
            call "CONVERT" (ship_qty(maxper%), 0.0, ship_qty$(maxper%))
            svtotal = svtotal + ship_val(maxper%)
            call "CONVERT" (sutotal, 0.0, sutotal$)
            call "CONVERT" (svtotal, 0.0, svtotal$)
            utfac$ = hex(a4)  :  vtfac$ = hex(a4)
            return

        val_spread
            vtfac$ = hex(a2)
            if part% = 1% then utfac$ = hex(ac)
L16330:     if part% = 0% then L16350
            inpmessage$ = "Will NOT Recalculate the Per Unit Value!"
L16350:     gosub'101(4%)
            inpmessage$ = " "
            gosub'151(4)
                if errormsg$ <> " " then L16330
            hi$  = "Press PF7 to Spread the Target Booking Dollars"
            mid$ = "Press PF8 to Spread the Target Shipping Dollars"
            lo$  = "Press PF9 to Spread BOTH of the Entered Dollars"
L16420:     ask% = 1%
            call"ASKUSER"(ask%,"***** SPREAD WHAT? *****",hi$,mid$,lo$)
            if ask% < 7% or ask% > 9% then L16420
            if ask% = 8% then L16610

*       *  Spread Booking Dollars
            bvaltemp% = bvtotal
            for i% = 1% to maxper%
                book_val(i%) = round((bvtotal / maxper%),0)
                call "CONVERT" (book_val(i%), 0.0, book_val$(i%))
                bvaltemp% = bvaltemp% - book_val(i%)
            next i%
            book_val(maxper%) = book_val(maxper%) + bvaltemp%
            call "CONVERT" (book_val(maxper%), 0.0, book_val$(maxper%))
            vtfac$ = hex(a4)
            if part% = 1% then utfac$ = hex(a4)
            if ask% = 7% then return       /* Spread booking $$ only */

*       * Spread Shipping Dollars
L16610:     svaltemp% = svtotal
            for i% = 1% to maxper%
                ship_val(i%) = round((svtotal / maxper%),0)
                call "CONVERT" (ship_val(i%), 0.0, ship_val$(i%))
                svaltemp% = svaltemp% - ship_val(i%)
            next i%
            ship_val(maxper%) = ship_val(maxper%) + svaltemp%
            call "CONVERT" (ship_val(maxper%), 0.0, ship_val$(maxper%))
            vtfac$ = hex(a4)
            if part% = 1% then utfac$ = hex(a4)
            return


        per_unit_change
            valfac$ = hex(a2)
L16760:     gosub'101(4%)
            gosub'151(5%)
            if errormsg$ <> " " then L16760
            bvtotal, svtotal = 0
            for i% = 1% to maxper%
                book_val(i%) = book_qty(i%) * unit_val
                call "CONVERT" (book_val(i%), 0.0, book_val$(i%))
                ship_val(i%) = ship_qty(i%) * unit_val
                call "CONVERT" (ship_val(i%), 0.0, ship_val$(i%))
                bvtotal = bvtotal + book_val(i%)
                svtotal = svtotal + ship_val(i%)
            next i%
            call "CONVERT" (svtotal, 0.0, svtotal$)
            call "CONVERT" (bvtotal, 0.0, bvtotal$)
            valfac$ = hex(a4)
            return

        REM Delete Targrt Records
        delete_target
L17010:     ask% = 2%
            call "ASKUSER" (ask%, "*** DELETE Confirmation ***",         ~
                 "Press PF-28 To Delete this Sales Analysis Record",     ~
                 "* * * (Both Targets and Actuals) * * *",               ~
                 "- or - Press RETURN to Return to the Edit Screen")
            if ask% = 0 then return
            if ask% <> 28% then L17010

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
            get #1 using L30280, book_qty(), book_val(),                  ~
                                ship_qty(), ship_val()
            butotal, sutotal, bvtotal, svtotal = 0
            for i% = 1% to maxper%
                call "CONVERT" (book_qty(i%), 0.0, book_qty$(i%))
                call "CONVERT" (book_val(i%), 0.0, book_val$(i%))
                call "CONVERT" (ship_qty(i%), 0.0, ship_qty$(i%))
                call "CONVERT" (ship_val(i%), 0.0, ship_val$(i%))
                butotal = butotal + book_qty(i%)
                bvtotal = bvtotal + book_val(i%)
                sutotal = sutotal + ship_qty(i%)
                svtotal = svtotal + ship_val(i%)
            next i%
            call "CONVERT" (butotal, 0.0, butotal$)
            call "CONVERT" (sutotal, 0.0, sutotal$)
            call "CONVERT" (svtotal, 0.0, svtotal$)
            call "CONVERT" (bvtotal, 0.0, bvtotal$)
            return

L30280:     FMT    /*  Record Layout for SASUMRYn for GET              */~
                POS( 577), 13*PD(14, 4), /* Target Booking Units       */~
                POS( 681), 13*PD(14, 4), /* Target Booking Value       */~
                POS( 785), 13*PD(14, 4), /* Target Shipping Units      */~
                POS( 889), 13*PD(14, 4)  /* Target Shipping Value      */

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        dataput
            call "READ101" (#1, readkey$, f1%(1))
            if f1%(1) = 0 then L31210
            put #1 using L30280, book_qty(), book_val(),                  ~
                                ship_qty(), ship_val()
            rewrite #1
            return

L31210:     put #1 using L31290, keydate$, code1$(1), code2$(1), zarray(),~
                    book_qty(), book_val(), ship_qty(), ship_val(),      ~
                    code2$(1), keydate$, code1$(1)

            write #1
            return


L31290:     FMT /*  Record Layout in file SASUMRYn                     */~
                CH(  6),                /* Year Key                 */   ~
                CH( 25),                /* Code 1                   */   ~
                CH( 25),                /* Code 2                   */   ~
                65*PD(14,4),            /* Actuals                  */   ~
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
                str(line2$,62%) = "SATGTINP: " & str(cms2v$,,8%)
                if fieldnr% > 0% then pflit$(3) = "(3)Select New Year"   ~
                                 else pflit$(3) = "(12)Delete Record"
*              IF FIELDNR% > 0% THEN PFLIT$(3) = "(3)Select New Year"   ~
*                               ELSE PFLIT$(3) = " "
                if fieldnr% = 3% then L40200
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
                  "Sales Analysis Target Management",                    ~
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
               at (07,70), fac(pfac$( 1))  , price$(1)          , ch(10),~
                                                                         ~
               at (08,02), fac(pdfac$( 2))  , linenr$( 2)       , ch(03),~
               at (08,06), fac(pdfac$( 2))  , period$( 2)       , ch(08),~
               at (08,19), fac(xfac$( 2,1)), book_qty$(2),        ch( 8),~
               at (08,29), fac(xfac$( 2,2)), book_val$(2),        ch(10),~
               at (08,43), fac(xfac$( 2,3)), ship_qty$(2),        ch( 8),~
               at (08,53), fac(xfac$( 2,4)), ship_val$(2),        ch(10),~
               at (08,66), fac(pfac$( 2))  , pcode$(2)          , ch( 2),~
               at (08,70), fac(pfac$( 2))  , price$(2)          , ch(10),~
                                                                         ~
               at (09,02), fac(pdfac$( 3))  , linenr$( 3)       , ch(03),~
               at (09,06), fac(pdfac$( 3))  , period$( 3)       , ch(08),~
               at (09,19), fac(xfac$( 3,1)), book_qty$(3),        ch( 8),~
               at (09,29), fac(xfac$( 3,2)), book_val$(3),        ch(10),~
               at (09,43), fac(xfac$( 3,3)), ship_qty$(3),        ch( 8),~
               at (09,53), fac(xfac$( 3,4)), ship_val$(3),        ch(10),~
               at (09,66), fac(pfac$( 3))  , pcode$(3)          , ch( 2),~
               at (09,70), fac(pfac$( 3))  , price$(3)          , ch(10),~
                                                                         ~
               at (10,02), fac(pdfac$( 4))  , linenr$( 4)       , ch(03),~
               at (10,06), fac(pdfac$( 4))  , period$( 4)       , ch(08),~
               at (10,19), fac(xfac$( 4,1)), book_qty$(4),        ch( 8),~
               at (10,29), fac(xfac$( 4,2)), book_val$(4),        ch(10),~
               at (10,43), fac(xfac$( 4,3)), ship_qty$(4),        ch( 8),~
               at (10,53), fac(xfac$( 4,4)), ship_val$(4),        ch(10),~
               at (10,66), fac(pfac$( 4))  , pcode$(4)          , ch( 2),~
               at (10,70), fac(pfac$( 4))  , price$(4)          , ch(10),~
                                                                         ~
               at (11,02), fac(pdfac$( 5))  , linenr$( 5)       , ch(03),~
               at (11,06), fac(pdfac$( 5))  , period$( 5)       , ch(08),~
               at (11,19), fac(xfac$( 5,1)), book_qty$(5),        ch( 8),~
               at (11,29), fac(xfac$( 5,2)), book_val$(5),        ch(10),~
               at (11,43), fac(xfac$( 5,3)), ship_qty$(5),        ch( 8),~
               at (11,53), fac(xfac$( 5,4)), ship_val$(5),        ch(10),~
               at (11,66), fac(pfac$( 5))  , pcode$(5)          , ch( 2),~
               at (11,70), fac(pfac$( 5))  , price$(5)          , ch(10),~
                                                                         ~
               at (12,02), fac(pdfac$( 6))  , linenr$( 6)       , ch(03),~
               at (12,06), fac(pdfac$( 6))  , period$( 6)       , ch(08),~
               at (12,19), fac(xfac$( 6,1)), book_qty$(6),        ch( 8),~
               at (12,29), fac(xfac$( 6,2)), book_val$(6),        ch(10),~
               at (12,43), fac(xfac$( 6,3)), ship_qty$(6),        ch( 8),~
               at (12,53), fac(xfac$( 6,4)), ship_val$(6),        ch(10),~
               at (12,66), fac(pfac$( 6))  , pcode$(6)          , ch( 2),~
               at (12,70), fac(pfac$( 6))  , price$(6)          , ch(10),~
                                                                         ~
               at (13,02), fac(pdfac$( 7))  , linenr$( 7)       , ch(03),~
               at (13,06), fac(pdfac$( 7))  , period$( 7)       , ch(08),~
               at (13,19), fac(xfac$( 7,1)), book_qty$(7),        ch( 8),~
               at (13,29), fac(xfac$( 7,2)), book_val$(7),        ch(10),~
               at (13,43), fac(xfac$( 7,3)), ship_qty$(7),        ch( 8),~
               at (13,53), fac(xfac$( 7,4)), ship_val$(7),        ch(10),~
               at (13,66), fac(pfac$( 7))  , pcode$(7)          , ch( 2),~
               at (13,70), fac(pfac$( 7))  , price$(7)          , ch(10),~
                                                                         ~
               at (14,02), fac(pdfac$( 8))  , linenr$( 8)       , ch(03),~
               at (14,06), fac(pdfac$( 8))  , period$( 8)       , ch(08),~
               at (14,19), fac(xfac$( 8,1)), book_qty$(8),        ch( 8),~
               at (14,29), fac(xfac$( 8,2)), book_val$(8),        ch(10),~
               at (14,43), fac(xfac$( 8,3)), ship_qty$(8),        ch( 8),~
               at (14,53), fac(xfac$( 8,4)), ship_val$(8),        ch(10),~
               at (14,66), fac(pfac$( 8))  , pcode$(8)          , ch( 2),~
               at (14,70), fac(pfac$( 8))  , price$(8)          , ch(10),~
                                                                         ~
               at (15,02), fac(pdfac$( 9))  , linenr$( 9)       , ch(03),~
               at (15,06), fac(pdfac$( 9))  , period$( 9)       , ch(08),~
               at (15,19), fac(xfac$( 9,1)), book_qty$(9),        ch( 8),~
               at (15,29), fac(xfac$( 9,2)), book_val$(9),        ch(10),~
               at (15,43), fac(xfac$( 9,3)), ship_qty$(9),        ch( 8),~
               at (15,53), fac(xfac$( 9,4)), ship_val$(9),        ch(10),~
               at (15,66), fac(pfac$( 9))  , pcode$(9)          , ch( 2),~
               at (15,70), fac(pfac$( 9))  , price$(9)          , ch(10),~
                                                                         ~
               at (16,02), fac(pdfac$(10))  , linenr$(10)       , ch(03),~
               at (16,06), fac(pdfac$(10))  , period$(10)       , ch(08),~
               at (16,19), fac(xfac$(10,1)), book_qty$(10),       ch( 8),~
               at (16,29), fac(xfac$(10,2)), book_val$(10),       ch(10),~
               at (16,43), fac(xfac$(10,3)), ship_qty$(10),       ch( 8),~
               at (16,53), fac(xfac$(10,4)), ship_val$(10),       ch(10),~
               at (16,66), fac(pfac$(10))  , pcode$(10)         , ch( 2),~
               at (16,70), fac(pfac$(10))  , price$(10)         , ch(10),~
                                                                         ~
               at (17,02), fac(pdfac$(11))  , linenr$(11)       , ch(03),~
               at (17,06), fac(pdfac$(11))  , period$(11)       , ch(08),~
               at (17,19), fac(xfac$(11,1)), book_qty$(11),       ch( 8),~
               at (17,29), fac(xfac$(11,2)), book_val$(11),       ch(10),~
               at (17,43), fac(xfac$(11,3)), ship_qty$(11),       ch( 8),~
               at (17,53), fac(xfac$(11,4)), ship_val$(11),       ch(10),~
               at (17,66), fac(pfac$(11))  , pcode$(11)         , ch( 2),~
               at (17,70), fac(pfac$(11))  , price$(11)         , ch(10),~
                                                                         ~
               at (18,02), fac(pdfac$(12))  , linenr$(12)       , ch(03),~
               at (18,06), fac(pdfac$(12))  , period$(12)       , ch(08),~
               at (18,19), fac(xfac$(12,1)), book_qty$(12),       ch( 8),~
               at (18,29), fac(xfac$(12,2)), book_val$(12),       ch(10),~
               at (18,43), fac(xfac$(12,3)), ship_qty$(12),       ch( 8),~
               at (18,53), fac(xfac$(12,4)), ship_val$(12),       ch(10),~
               at (18,66), fac(pfac$(12))  , pcode$(12)         , ch( 2),~
               at (18,70), fac(pfac$(12))  , price$(12)         , ch(10),~
                                                                         ~
               at (19,02), fac(pdfac$(13))  , linenr$(13)       , ch(03),~
               at (19,06), fac(pdfac$(13))  , period$(13)       , ch(08),~
               at (19,19), fac(xfac$(13,1)), book_qty$(13)      , ch( 8),~
               at (19,29), fac(xfac$(13,2)), book_val$(13)      , ch(10),~
               at (19,43), fac(xfac$(13,3)), ship_qty$(13)      , ch( 8),~
               at (19,53), fac(xfac$(13,4)), ship_val$(13)      , ch(10),~
               at (19,66), fac(pfac$(13))  , pcode$(13)         , ch( 2),~
               at (19,70), fac(pfac$(13))  , price$(13)         , ch(10),~
                                                                         ~
                                                                         ~
               at (20,08), fac(l7vfac$)    , totlit$,             ch( 8),~
               at (20,17), fac(utfac$)     , butotal$,            ch(10),~
               at (20,29), fac(vtfac$)     , bvtotal$,            ch(10),~
               at (20,41), fac(utfac$)     , sutotal$,            ch(10),~
               at (20,53), fac(vtfac$)     , svtotal$,            ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
               at (22,02), "(1)Start Over",                              ~
               at (22,21), fac(pffac$( 7)), pflit$( 7),                  ~
               at (22,42), fac(pffac$(10)), pflit$(10),                  ~
               at (22,65), "(13)Instructions",                           ~
                                                                         ~
               at (23,02), fac(pffac$( 2)), pflit$( 2),                  ~
               at (23,21), fac(pffac$( 8)), pflit$( 8),                  ~
               at (23,42), fac(pffac$(11)), pflit$(11),                  ~
               at (23,65), "(15)Print Screen",                           ~
                                                                         ~
               at (24,02), fac(pffac$( 3)), pflit$( 3),                  ~
               at (24,21), fac(pffac$( 9)), pflit$( 9),                  ~
               at (24,39), fac(valfac$)   , unit_val$,            ch(10),~
               at (24,65), fac(hex(8c)), pf16$,                          ~
                                                                         ~
               keys(keytab$), key (keyhit%)

               if keyhit% <> 13 then L41920
                  call "MANUAL" ("SATGTINP")
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
                                    L51400          /* Line Values      */
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
            call"NUMTEST"(butotal$,-1.3e9,1.3e9,errormsg$, -0.001,butotal)
                if errormsg$ <> " " then return
            call"NUMTEST"(sutotal$,-1.3e9,1.3e9,errormsg$, -0.001,sutotal)
            return

L50800: REM Test Total Value for Spread
            call"NUMTEST"(bvtotal$,-1.3e9,1.3e9,errormsg$, -0.001,bvtotal)
                if errormsg$ <> " " then return
            call"NUMTEST"(svtotal$,-1.3e9,1.3e9,errormsg$, -0.001,svtotal)
            return

L51000: REM Test Per Unit Value
            call"NUMTEST"(unit_val$, 0, 9e7, errormsg$, 0.2, unit_val)
            return
L51200: REM Test Line Quantity Entry
            for j% = 1% to maxper%
                call "NUMTEST" (book_qty$(j%), -9.9e7, 9.9e7,            ~
                                errormsg$, -0.001, book_qty(j%))
                if errormsg$ <> " " then return
                call "NUMTEST" (ship_qty$(j%), -9.9e7, 9.9e7,            ~
                                errormsg$, -0.001, ship_qty(j%))
                if errormsg$ <> " " then return
            next j%
            return

L51400: REM Test Line Value Entry
            for j% = 1% to maxper%
                call "NUMTEST" (book_val$(j%), -9.9e7, 9.9e7,            ~
                                errormsg$, -0.001, book_val(j%))
                if errormsg$ <> " " then return
                call "NUMTEST" (ship_val$(j%), -9.9e7, 9.9e7,            ~
                                errormsg$, -0.001, ship_val(j%))
                if errormsg$ <> " " then return
            next j%
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
