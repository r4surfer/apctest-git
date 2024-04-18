        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS    AAA   RRRR   PPPP   TTTTT  IIIII  N   N  PPPP    *~
            *  S      A   A  R   R  P   P    T      I    NN  N  P   P   *~
            *   SSS   AAAAA  RRRR   PPPP     T      I    N N N  PPPP    *~
            *      S  A   A  R   R  P        T      I    N  NN  P       *~
            *   SSS   A   A  R   R  P        T    IIIII  N   N  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SARPTINP - Input/edit of Sales Analysis Report Format     *~
            *            Definitions.  Report columns are defined as to *~
            *            content.  Report definitions are stored in     *~
            *            SYSFILE2.                                      *~
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
            * 12/11/86 ! Original                                 ! JIM *~
            * 05/14/87 ! Standard Costing Changes                 ! ERN *~
            * 02/18/92 ! Added support for Including summary codes! JDH *~
            *          !   with no activity in specified years.   !     *~
            * 06/25/92 ! PRR 12498 Fixed overflow problem of DESC.! JDH *~
            * 04/08/93 ! PRRs 10886, 11079 Non-Stock Parts OK if  ! JIM *~
            *          !   SYSFILE2 SWITCHS.SA record says so.    !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim    /* Renumber the 1000 - 2000 series by 10   */             ~
            asnd_dsnd$1,                 /* Ascend or Descend          */~
            cdescr$(7)45,                /* Report Column description  */~
            cfac$(7)1,                   /* Column header FACs         */~
            code1$(2)25,                 /* 1st Grp Codes- Input/Edit  */~
            code2$(2)25,                 /* 2nd Grp Codes- Input/Edit  */~
            codefile%(8), codelen%(8),   /* Group Code Len, File#      */~
            codes%(10,2),                /* Summary File Group Codes   */~
            codes$(10,2)14,              /* Group Code Descriptors     */~
            codex$25,                    /* Group code work area       */~
            col$3,                       /* Column header              */~
            col$(7)1,                    /* Column designators A-G     */~
            colahdr$(3)10,               /* Column A report header     */~
            colbhdr$(3)10,               /* Column B report header     */~
            colchdr$(3)10,               /* Column C report header     */~
            coldhdr$(3)10,               /* Column D report header     */~
            colehdr$(3)10,               /* Column E report header     */~
            colfhdr$(3)10,               /* Column F report header     */~
            colghdr$(3)10,               /* Column G report header     */~
            coldesc$18,                  /* Column header              */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descrs$(10)30,               /* Summary File Descriptions  */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fc$3,                        /* Column header              */~
            fc1$(7)1,                    /* Field or Column 1          */~
            fc2$(7)1,                    /* Field or Column 2          */~
            ffmt$3,                      /* Field format (validation)  */~
            ffmt$(7)3,                   /* Field Format               */~
            fmt$3,                       /* Column header              */~
            grcd1$(2)25,                 /* 1st Grp Codes- SYSFILE2    */~
            grcd2$(2)25,                 /* 2nd Grp Codes- SYSFILE2    */~
            group1$14, group2$14,        /* Group Code Descriptors     */~
            grp1_desc$2,                 /* Group Code 1 Desc length   */~
            grp2_desc$2,                 /* Group Code 2 Desc length   */~
            i$(24)80,                    /* Screen Image               */~
            incl$1,                      /* Include groups outside yrs */~
            inpmessage$79,               /* Informational Message      */~
            junk$25,                     /* Misc alpha data            */~
            keytab$33,                   /* PF keys enabled at ACCEPT  */~
            lfac$(23)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            line16$79,                   /* Screen line 16             */~
            line17$79,                   /* Screen line 17             */~
            line18$79,                   /* Screen line 18             */~
            line19$79,                   /* Screen line 19             */~
            line20$79,                   /* Screen line 20             */~
            lname$(15)21,                /* Field/column names (long)  */~
            max_print$3,                 /* Max to print               */~
            msg$79,                      /* Message                    */~
            nonstock$1,                  /* Non-Stock parts OK?        */~
            op$4,                        /* Column header              */~
            oper$(7)1,                   /* Operation                  */~
            page_brk$1,                  /* Page break code            */~
            pf1$16, pf1fac$1,            /* PF 1 Screen Literal  & FAC */~
            pf2$16, pf2fac$1,            /* PF 2 Screen Literal  & FAC */~
            pf3$16, pf3fac$1,            /* PF 3 Screen Literal  & FAC */~
            pf4$18, pf4fac$1,            /* PF 4 Screen Literal  & FAC */~
            pf8$18, pf8fac$1,            /* PF 8 Screen Literal  & FAC */~
            pf12$18, pf12fac$1,          /* PF 12 Screen Literal & FAC */~
            pf16$18, pf16fac$1,          /* PF 16 Screen Literal & FAC */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            posn_rem$4,                  /* Edited positions remaining */~
            prnames$(10)8,               /* Summary File PR Names      */~
            rank_colm$1,                 /* Rank on Column             */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rept_array$(12)10,           /* Reports by Report Group    */~
            rept_desc$30,                /* Report Description         */~
            rept_desx$30,                /* Report Description save    */~
            rept_grp$10,                 /* Report Group name          */~
            rept_key$20,                 /* SYSFILE2 key               */~
            rept_name$10,                /* Report Name                */~
            rept_save$10,                /* Report Name duplicate      */~
            rept_type$1,                 /* Report Type                */~
            sname$(15)10,                /* Field/column names (short) */~
            summ_desc$60,                /* Summary file header descr  */~
            summ_detl$1,                 /* Summary or Detail          */~
            summ_file$1,                 /* Summary File #             */~
            userid$3,                    /* Current User Id            */~
            ytd$4,                       /* Column header              */~
            yr$2,                        /* Column header              */~
            yr1$(7)2,                    /* Data Year 1                */~
            yr2$(7)2,                    /* Data Year 2                */~
            ytd1$(7)1,                   /* Year to Date 1             */~
            ytd2$(7)1                    /* Year to Date 2             */

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
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
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
            * #3  ! CUSTOMER ! CUSTOMER MASTER FILE                     *~
            * #4  ! HNYMASTR ! Inventory Master File                    *~
            * #5  ! CATEGORY ! INVENTORY CATEGORY CODES FILE            *~
            * #6  ! GENCODES ! System General Codes file.               *~
            * #7  ! SLMMASTR ! Salesman master file                     *~
            * #8  ! STORNAME ! STORE INFORMATION FILE                   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "SASUMRY#",                                      ~
                        varc,     indexed,  recsize =  1048,             ~
                        keypos =  1,   keylen =  56,                     ~
                        alt key 1, keypos =   993, keylen = 56,          ~
                            key 2, keypos =  1024, keylen = 25, dup

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

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%, rslt$(4 ))
            call "OPENCHCK" (#5,  fs%(5 ), f2%(5 ), 0%, rslt$(5 ))
            call "OPENCHCK" (#6,  fs%(6 ), f2%(6 ), 0%, rslt$(6 ))
            call "OPENCHCK" (#7,  fs%(7 ), f2%(7 ), 0%, rslt$(7 ))
            call "OPENCHCK" (#8,  fs%(8 ), f2%(8 ), 0%, rslt$(8 ))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

*        Get the Non-stocked part OK? flag from SYSFILE2.
            nonstock$ = "N"
            call "READ100" (#2, "SWITCHS.SA", f1%(2%))
            if f1%(2%) <> 0% then get #2 using L09065, nonstock$
L09065:         FMT POS(24), CH(1)

            call "SHOSTAT" ("Initializing ...")
*        Load S/A Summary File Descriptions
            plowkey$ = "SA.FILES.SASUMRY"
            str(plowkey$, len(plowkey$)+1) = all(hex(00))
L09110:     call "PLOWNEXT" (#2, plowkey$, 16%, f1%(2))
            if f1%(2) = 1% then L09180
                if file% <> 0% then L09240
                call "ASKUSER" (0%, "*** NO S/A FILES ***",              ~
                                "There are no S/A Summary Files Defined",~
                                " ", "Press RETURN to exit program...")
                goto exit_program
L09180:     convert str(plowkey$,17,1) to file% : file% = file% + 1%
            get #2 using L09210, prnames$(file%), descrs$(file%),          ~
                               codes%(file%, 1), codes%(file%, 2)
L09210:         FMT XX(9), CH(8), XX(3), CH(30), XX(2), 2*BI(1)
            goto L09110

L09240:     codes$(1,1) = "  Part Number:" : codes$(1,2) = "Part Number"
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

            call "EXTRACT" addr("ID", userid$)
            date$ = date : call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            col$ = "Col" : yr$ = "Yr" : ytd$ = "YTD?" : fc$ = "F/C"
            op$ = "Oper" : fmt$ = "Fmt" : coldesc$ = "Column Description"
            line16$ = "Years          ! Operators ! Field Codes"
            line17$ = "-n:n years ago ! +:Total   ! 1:Actl Bkng Qty  4:Ac~
        ~tl Ship Val  7:Trgt Bkng Val"
            line18$ = " 0:Current     ! -:Diff    ! 2:Actl Bkng Val  5:Ac~
        ~tl Ship Cst  8:Trgt Ship Qty"
            line19$ = "+n:Future yrs  ! @:Average ! 3:Actl Ship Qty  6:Tr~
        ~gt Bkng Qty  9:Trgt Ship Val"
            line20$ = " (n = 1-3)     ! %:Percent !     or, you may use C~
        ~olumn Designators A-F"
            pf1$ = "(1)Start Over"         : pf1fac$ = hex(8c)
            pf2$ = "(2)Line Over"
            pf3$ = "(3)Copy a Format"
            pf4$ = "(4)Previous Field"
            pf12$ = "(12)Delete Format"
            keytab$ = all(hex(ff))
            str(keytab$,33, 1) = hex(00) : str(keytab$, 1, 1) = hex(01)
            str(keytab$,13, 1) = hex(0d) : str(keytab$,15, 1) = hex(0f)
            rept_key$ = "SA.RPTDEF."
            col$(1) = "A" : col$(2) = "B" : col$(3) = "C" : col$(4) = "D"
            col$(5) = "E" : col$(6) = "F" : col$(7) = "G"
            sname$( 1) = "A.Bkng Qty" : sname$( 2) = "A.Bkng Val"
            sname$( 3) = "A.Ship Qty" : sname$( 4) = "A.Ship Val"
            sname$( 5) = "A.Ship Cst" : sname$( 6) = "T.Bkng Qty"
            sname$( 7) = "T.Bkng Val" : sname$( 8) = "T.Ship Qty"
            sname$( 9) = "T.Ship Val" : sname$(10) = "Column A  "
            sname$(11) = "Column B  " : sname$(12) = "Column C  "
            sname$(13) = "Column D  " : sname$(14) = "Column E  "
            sname$(15) = "Column F  "
            lname$( 1) = "Actual Booking Units "
            lname$( 2) = "Actual Booking Value "
            lname$( 3) = "Actual Shipping Units"
            lname$( 4) = "Actual Shipping Value"
            lname$( 5) = "Actual Shipping Cost "
            lname$( 6) = "Target Booking Units "
            lname$( 7) = "Target Booking Value "
            lname$( 8) = "Target Shipping Units"
            lname$( 9) = "Target Shipping Value"
            lname$(10) = "Column A             "
            lname$(11) = "Column B             "
            lname$(12) = "Column C             "
            lname$(13) = "Column D             "
            lname$(14) = "Column E             "
            lname$(15) = "Column F             "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            mat redim code1$(2)25, code2$(2)25
            pf8$ = "(8)Edit Col Header"
            pf16$ = "(16)Exit Program"
            str(keytab$,16, 1) = hex(10) : pf16fac$ = hex(84)
            str(keytab$,12, 1) = hex(ff) : pf12fac$ = hex(9c)
            init(" ") errormsg$, inpmessage$, posn_rem$, rept_name$,     ~
                rept_desc$, summ_file$, code1$(), code2$(), grp1_desc$,  ~
                grp2_desc$, rept_type$, summ_detl$, incl$,               ~
                rank_colm$, asnd_dsnd$, max_print$, page_brk$, yr1$(),   ~
                ytd1$(), fc1$(), oper$(), yr2$(), ytd2$(), fc2$(),       ~
                ffmt$(), summ_desc$, grcd1$(), grcd2$(), group1$,        ~
                group2$, cdescr$(), colahdr$(), colbhdr$(), colchdr$(),  ~
                coldhdr$(), colehdr$(), colfhdr$(), colghdr$()
            colahdr$(1) = "Column A" : colbhdr$(1) = "Column B"
            colchdr$(1) = "Column C" : coldhdr$(1) = "Column D"
            colehdr$(1) = "Column E" : colfhdr$(1) = "Column F"
            colghdr$(1) = "Column G"
            group1%, group2%, grp2_desc%, grp2_max%, sum%, sumry%,       ~
                file%, file1%, file2% = 0%
            call "ALLFREE"

            for fieldnr% = 1% to 23%
L10290:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0% then goto L10670
L10310:         if fieldnr% > 1% then goto L10350
                     str(keytab$, 4, 1) = hex(ff) : pf4fac$ = hex(9c)
                     str(keytab$, 8, 1) = hex(ff) : pf8fac$ = hex(9c)
                     goto L10380
L10350:         str(keytab$, 4, 1) = hex(04) : pf4fac$ = hex(8c)
                str(keytab$, 8, 1) = hex(08) : pf8fac$ = hex(8c)
L10380:         if fieldnr% = 3% then goto L10410
                     str(keytab$, 3, 1) = hex(ff) : pf3fac$ = hex(9c)
                     goto L10420
L10410:         str(keytab$, 3, 1) = hex(03) : pf3fac$ = hex(8c)
L10420:         if fieldnr% > 16% then goto L10450
                     str(keytab$, 2, 1) = hex(ff) : pf2fac$ = hex(9c)
                     goto L10460
L10450:         str(keytab$, 2, 1) = hex(02) : pf2fac$ = hex(8c)
L10460:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  2% then goto L10460
                      if keyhit% <>  3% then goto L10520
                          gosub replicate_a_format
                          goto L10290
L10520:               if keyhit% <>  4% then goto L10600
                         gosub line_killer
L10540:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then goto L10310
                         if fieldnr% = 1% then goto L10290
                         goto L10540
L10600:               if keyhit%  =  8% then gosub edit_column_headers
                      if keyhit%  = 16% then goto exit_program
                      if keyhit% <>  0% then goto L10290
                gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then goto L10310
                if fieldnr% = 1% and f1%(2) = 1% then goto edtpg1
L10670:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            inpmessage$ = edtmessage$ : errormsg$ = " "
            pf16$ = "(16)Save Format"
            str(keytab$, 2, 1) = hex(ff) : pf2fac$ = hex(9c)
            str(keytab$, 3, 1) = hex(ff) : pf3fac$ = hex(9c)
            str(keytab$, 4, 1) = hex(ff) : pf4fac$ = hex(9c)
            str(keytab$, 8, 1) = hex(08) : pf8fac$ = hex(8c)
            str(keytab$,12, 1) = hex(0c) : pf12fac$ = hex(8c)
            gosub test_pf16_on
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  8% then gosub edit_column_headers
                  if keyhit%  = 12% then gosub deletefmt
                  if keyhit%  = 16% then goto datasave
                  if keyhit% <>  0% then goto edtpg1
            str(keytab$, 2, 1) = hex(ff) : pf2fac$ = hex(9c)
            on cursor%(1) goto edtpg1, edtpg1, edtpg1, L11280, L11290,     ~
                L11300, L11310, edtpg1, L11320, L11320, L11320, L11320,        ~
                L11320, L11320, L11320
                goto edtpg1
L11280:     lo% =  2% : hi% =  3% : goto L11340
L11290:     lo% =  4% : hi% =  6% : goto L11340
L11300:     lo% =  7% : hi% =  9% : goto L11340
L11310:     lo% = 10% : hi% = 16% : goto L11340
L11320:     lo% = cursor%(1) + 8% : hi% = lo%
            str(keytab$, 2, 1) = hex(02) : pf2fac$ = hex(8c)
L11340:     for fieldnr% = lo% to hi%
L11350:         if fieldnr% < 1% or fieldnr% > 23% then goto edtpg1
                if lo% = hi% then goto L11420
L11370:         if fieldnr% = lo% then goto L11400
                    str(keytab$, 4, 1) = hex(04) : pf4fac$ = hex(8c)
                    goto L11420
L11400:         str(keytab$, 4, 1) = hex(ff) : pf4fac$ = hex(9c)
L11420:         if fieldnr% > 16% then c% = fieldnr% - 16%
                gosub test_pf16_on
L11430:         gosub'051(fieldnr%)   /* Check Enables, Set Defaults */
                      if enabled% = 0% then goto edtpg1
L11450:         gosub'101(fieldnr%)     /* Display & Accept Screen     */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  2% then goto L11350
                      if keyhit% <>  4% then goto L11560
                         gosub line_killer
L11500:                  fieldnr% = max(lo%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then goto L11370
                         if fieldnr% = lo% then goto L11340
                         goto L11500
L11560:               if keyhit%  =  8% then gosub edit_column_headers
                      if keyhit%  = 12% then gosub deletefmt
                      if keyhit%  = 16% then goto datasave
                      if keyhit% <>  0% then goto L11430
                gosub'151(fieldnr%)     /* Edit Field for Valid Entry  */
                      if errormsg$ <> " " then goto L11450
            next fieldnr%
            goto edtpg1

        test_pf16_on
            pf16% = 0%
            for n% = 1% to 7%
                if fc1$(n%) <> " " then pf16% = 1%
            next n%
            if pf16% <> 0% then goto L11720
                str(keytab$,16, 1) = hex(ff) : pf16fac$ = hex(9c)
                return
L11720:     str(keytab$,16, 1) = hex(10) : pf16fac$ = hex(84)
            return

        REM *************************************************************~
            *  Operator may edit the column headers that will print     *~
            *  above the report columns.                                *~
            *************************************************************

        edit_column_headers
            inpmessage$ = "Enter the report column heading titles you "& ~
                "desire"
            init (hex(8c)) cfac$()
            for resp% = 1% to 7%
                if fc1$(resp%) <> " " then cfac$(resp%) = hex(80)
            next resp%
L12130:     gosub'102
            if resp% =  1% then gosub startover
            if resp% =  8% then return
            if resp% <> 0% then goto L12130
            return

        REM *************************************************************~
            *  Operator may enter the name of a previously-defined for- *~
            *  mat.  That format is copied into the fields for edit     *~
            *  under the name just entered.                             *~
            *************************************************************

        replicate_a_format
            rept_save$ = rept_name$ : rept_desx$ = rept_desc$
            plowkey$ = str(rept_key$,,10) & hex(00)
            msg$ = hex(06) & "Select a report definition to copy"
            call "PLOWCODE" (#2, plowkey$, msg$, 10%, 0.30, f1%(2))
            if f1%(2) = 0% then goto L12350
                gosub dataload
                rept_name$ = rept_save$ : rept_desc$ = rept_desx$
                return clear all
                goto edtpg1
L12350:     str(rept_key$,11) = rept_name$
            return

        REM *************************************************************~
            *     M I S C E L L A N E O U S   S U B R O U T I N E S     *~
            *************************************************************

        line_killer
            if fieldnr% < 17% then return
                if fc1$(c%) <> " " then return
                     init (" ") yr1$(c%), ytd1$(c%), oper$(c%),          ~
                          fc2$(c%), yr2$(c%), ytd2$(c%), ffmt$(c%)
            return

        select_summary_file
*        Get which Summary File to display data for
            if f2%(1) = 0% then close #1
            f2%(1) = 1%
            sum% = 0%
            convert summ_file$ to sum%, data gosub sa_files_subroutine
            if sum% = 116% then return
            sumry% = sum% + 1%
            convert sum% to summ_file$, pic (#)
            call "PUTPRNAM" addr (#1, prnames$(sumry%))
            call "OPENCHCK" (#1,  fs%(1), f2%(1), 0%, rslt$(1))
            if f2%(1) <> 0% then return
            summ_desc$ = "Summary " & summ_file$ & ": " & descrs$(sumry%)

            group1% = codes%(sumry%, 1%)
            file1%  = codefile%(group1%)
            grcd1$(1) = code1$(1) : grcd1$(2) = code1$(2)
            mat redim code1$(2)max(5%, codelen%(group1%))
            code1$(1) = grcd1$(1) : code1$(2) = grcd1$(2) :
            group1$ = codes$(group1%, 1)
            if group1% = 1% then grp1_max% = 32% else grp1_max% = 30%
            grp1_desc% = min(grp1_max%, grp1_desc% + posn_rem%)
            convert grp1_desc% to grp1_desc$, pic (##)
            call "STRING" addr ("LJ", grp1_desc$, 2%)

            group2% = codes%(sumry%, 2%) : file2% = 0% : group2$ = " "
            if group2% = 0% then return
                file2%  = codefile%(group2%)
                grcd2$(1) = code2$(1) : grcd2$(2) = code2$(2)
                mat redim code2$(2)max(5%, codelen%(group2%))
                code2$(1) = grcd2$(1) : code2$(2) = grcd2$(2)
                group2$ = codes$(group2%, 1)
                if group2% = 1% then grp2_max% = 32% else grp2_max% = 30%
                grp2_desc% = min(grp2_max%, grp2_desc% + posn_rem%)
                convert grp2_desc% to grp2_desc$, pic (##)
                call "STRING" addr ("LJ", grp2_desc$, 2%)
            return

        sa_files_subroutine
            plowkey$ = "SA.FILES.SASUMRY"
            call "SASUMINP" ("SARPTINP",                                 ~
                             "Sales Analysis: Define Report Definition", ~
                             "NY", descrs$(), sum%)
            return

        REM Compute number of positions remaining on a print line *******
        compute_posn_rem
            posn_rem% = 132% : grp1%, grp2% = 0%
            if group2% = 0% then goto L15630
                grp2% = codelen%(group2%) + 1%
                if grp2_desc% <> 0% then grp2% = grp2% + grp2_desc% + 1%
L15630:     if group1% = 0% then goto compute_posn_rem_lines
                grp1% = codelen%(group1%) + 1%
                if grp1_desc% <> 0% then grp1% = grp1% + grp1_desc% + 1%
                if group2% = 0% then goto compute_posn_rem_summary
                if rept_type$ = "R" then goto compute_posn_rem_summary
                if summ_detl$ = "S" then goto compute_posn_rem_summary
                     posn_rem% = posn_rem% - max(grp1%, grp2%)
                     goto compute_posn_rem_lines
        compute_posn_rem_summary
            posn_rem% = posn_rem% - grp1%
        compute_posn_rem_lines
            for n% = 1% to 7%
                if ffmt$(n%) = " " then goto L15820
                posn%, fdig%, fdec% = 0%
                convert str(ffmt$(n%),1,2) to fdig%, data goto L15780
L15780:         convert str(ffmt$(n%),3,1) to fdec%, data goto L15790
L15790:         posn% = posn% + fdig% + 1%
                if fdec% <> 0% then posn% = posn% + fdec% + 1%
                posn_rem% = posn_rem% - max(11%, posn%)
L15820:     next n%
            convert posn_rem% to posn_rem$, pic (-###)
            call "STRING" addr ("LJ", posn_rem$, 4%)
            return

        capture_codes
*        Get Group Code                        CODEX$
            msg$ = hex(06) & "Select " & codes$(g%,2)
            if file% = 6% then L15980

*         Get code using GETCODE
            call "GETCODE" (#file%, codex$, msg$, 0%, 0, onfile%)
            if onfile% = 1% then return
                if nonstock$ = "Y" and file% = 4% then return
L15950:              errormsg$ = codes$(g%,2) & " not found on file."
                     return

L15980
*         Get code using PLOWCODE
            if g% = 5% then readkey$ = "CUS TYPES" & codex$
            if g% = 7% then readkey$ = "REGIONS  " & codex$
            call "PLOWCODE" (#file%, readkey$, msg$, 9%, 0.30, onfile%)

            if onfile% = 0% then L15950
                codex$ = str(readkey$,10)
                return

        validate_a_column
            if fc1$(c%) <> " " then goto L16120
                yr1$(c%), ytd1$(c%), oper$(c%), fc2$(c%), yr2$(c%),      ~
                     ytd2$(c%), ffmt$(c%), cdescr$(c%) = " "
                return
L16120:     y$ = fc1$(c%)
            if pos("123456789ABCDEF" = fc1$(c%)) <> 0% then goto L16170
L16140:         errormsg$ = "Field/Column entry '" & y$ & "' is invalid"&~
                     ".  Must be Field Code 1-9  -OR-  Column A-F"
                return
L16170:     if pos("123456789ABCDEF" = fc1$(c%)) < 10% then goto v_year1
                if pos(col$() = fc1$(c%)) < c% then goto L16220
L16190:              errormsg$ = "Sorry, report column '" & fc1$(c%) &   ~
                          "' can't be used before it's been defined"
                     return
L16220:         yr1$(c%), ytd1$(c%) = " "
                goto v_oper
        v_year1
            y$ = yr1$(c%)
            convert yr1$(c%) to yr%, data goto L16290
            if yr% < -3% then goto L16290
            if yr% < 4% then goto L16320
L16290:         errormsg$ = "Year '" & y$ & "' has an invalid format.  "&~
                    "Must be between -3 and +3"
                return
L16320:     if yr% > 0% then convert yr% to yr1$(c%), pic (+#)
            if yr% = 0% then yr1$(c%) = " 0"
            call "STRING" addr ("RJ", yr1$(c%), 2%)
            if ytd1$(c%) = "Y" then goto v_oper
            if ytd1$(c%) = "N" then goto v_oper
L16360:         errormsg$ = "Year to date field must contain either " &  ~
                     "'Y' or 'N'"
                return
        v_oper
            on pos("+-%@ " = oper$(c%)) goto L16510, L16510, L16510, L16450, ~
                L16490
                errormsg$ = "Operation '" & oper$(c%) & "' is invalid. "&~
                     "Valid values are shown under 'Operators'"
                return
L16450:     if pos("123456789ABCDEF" = fc1$(c%)) < 10% then goto L16480
                errormsg$ = "You may not Average a report column"
                return
L16480:     ytd1$(c%) = "Y"
L16490:     yr2$(c%), ytd2$(c%), fc2$(c%) = " "
            goto get_column_description
L16510:     y$ = fc2$(c%)
            if fc2$(c%) <> " " then goto L16560
                errormsg$ = "Operation codes '+', '-', and '%' require "&~
                     "that a second field be defined"
                return
L16560:     if pos("123456789ABCDEF" = fc2$(c%)) = 0% then L16140
            if pos("123456789ABCDEF" = fc2$(c%)) < 10% then L16610
                if pos(col$() = fc2$(c%)) >= c% then goto L16190
                yr2$(c%), ytd2$(c%) = " "
                goto get_column_description
L16610:     y$ = yr2$(c%)
            convert yr2$(c%) to yr%, data goto L16290
            if yr% < -3% then goto L16290
            if yr% > 3% then goto L16290
            if yr% > 0% then convert yr% to yr2$(c%), pic (+#)
            if yr% = 0% then yr2$(c%) = " 0"
            call "STRING" addr ("RJ", yr2$(c%), 2%)
            if ytd2$(c%) = "Y" then goto get_column_description
            if ytd2$(c%) <> "N" then goto L16360
        get_column_description
            gosub compute_column_description
            ffmt$ = ffmt$(c%)
            call "STRING" addr ("RJ", ffmt$, 3%)
            if str(ffmt$,1,1) = " " then str(ffmt$,1,1) = "0"
            ffmt$(c%) = ffmt$
            convert str(ffmt$,1,2) to fdig%, data goto L16880
            convert str(ffmt$,3,1) to fdec%, data goto L16880
            convert fdig% to str(ffmt$,1,2), pic (00)
            convert fdec% to str(ffmt$,3,1), pic (0)
            ffmt$(c%) = ffmt$
            if fdig% + fdec% > 18% then goto L16880
            if fdig% < 1% or fdig% > 14% then goto L16880
            if fdec% < 0% or fdec% > 4% then goto L16880
            gosub compute_posn_rem
            if posn_rem% >= 0% then return
            errormsg$ = "Format of field is too large for the number of"&~
                " print positions remaining"
            return
L16880:     errormsg$ = "Format is nnd: 'nn'/digits must be 1-14; " &    ~
                "'d'/decimals must be 0-4"
            return

        compute_column_description
            cdescr$(c%) = " "
            if fc1$(c%) = " " then return
            on pos("+-%@ " = oper$(c%)) goto L16980, L16980, L16980, L17160, ~
                L17170
            cdescr$(c%) = "*** UNKNOWN VALUES FOR FIELDS" : return
L16980:     cdescr$(c%) = sname$(pos("123456789ABCDEF" = fc1$(c%)))
            gosub compute_lh_years
            str(cdescr$(c%),len(cdescr$(c%))+2) = oper$(c%)
            str(cdescr$(c%),len(cdescr$(c%))+2) =                        ~
                sname$(pos("123456789ABCDEF" = fc2$(c%)))
            if yr2$(c%) = " " then goto L17130
            if yr2$(c%) <> " 0" then goto L17070
                str(cdescr$(c%),len(cdescr$(c%))+1) = ",Curr"
                goto L17130
L17070:     if str(yr2$(c%),1,1) <> "-" then goto L17110
                str(cdescr$(c%),len(cdescr$(c%))+1) = "," &              ~
                     str(yr2$(c%),2,1) & "Ago"
                goto L17130
L17110:     str(cdescr$(c%),len(cdescr$(c%))+1) = "," &                  ~
                str(yr2$(c%),2) & "Fut"
L17130:     if ytd2$(c%) <> "Y" then return
                str(cdescr$(c%),len(cdescr$(c%))+1) = ",YTD"
            return
L17160:     cdescr$(c%) = "Average"
L17170:     str(cdescr$(c%),len(cdescr$(c%))+2) =                        ~
                lname$(pos("123456789ABCDEF" = fc1$(c%)))
            gosub compute_lh_years
            call "STRING" addr ("LJ", cdescr$(c%), 45%)
            return

        compute_lh_years
            if yr1$(c%) = " " then goto L17340
            if yr1$(c%) <> " 0" then goto L17280
                str(cdescr$(c%),len(cdescr$(c%))+1) = ",Curr"
                goto L17340
L17280:     if str(yr1$(c%),1,1) <> "-" then goto L17320
                str(cdescr$(c%),len(cdescr$(c%))+1) = "," &              ~
                     str(yr1$(c%),2,1) & "Ago"
                goto L17340
L17320:     str(cdescr$(c%),len(cdescr$(c%))+1) = "," &                  ~
                str(yr1$(c%),2) & "Fut"
L17340:     if ytd1$(c%) <> "Y" then return
                str(cdescr$(c%),len(cdescr$(c%))+1) = ",YTD"
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            str(rept_key$,11) = rept_name$
            call "READ101" (#2, rept_key$, f1%(2))
                if f1%(2) <> 0% then delete #2
            mat grcd1$ = code1$ : mat grcd2$ = code2$
            write #2 using L60070, rept_key$, rept_desc$, summ_file$,     ~
                grcd1$(), grcd2$(), grp1_desc$, grp2_desc$, rept_type$,  ~
                summ_detl$, rank_colm$, asnd_dsnd$, max_print$,          ~
                page_brk$, yr1$(), ytd1$(), fc1$(), oper$(), yr2$(),     ~
                ytd2$(), fc2$(), ffmt$(), colahdr$(), colbhdr$(),        ~
                colchdr$(), coldhdr$(), colehdr$(), colfhdr$(),          ~
                colghdr$(), incl$, " "
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                if fieldnr% > 16% then c% = fieldnr% - 16%
                  enabled% = 1%
                  on fieldnr% gosub L20340,         /* Report Name      */~
                                    L20390,         /* Report Descriptio*/~
                                    L20440,         /* Summary File #   */~
                                    L20490,         /* Group Code 1 from*/~
                                    L20540,         /* Group Code 1 to  */~
                                    L20590,         /* Group Code 1 desc*/~
                                    L20670,         /* Group Code 2 from*/~
                                    L20750,         /* Group Code 2 to  */~
                                    L20830,         /* Group Code 2 Desc*/~
                                    L20940,         /* Report Type      */~
                                    L20990,         /* Summary or Detail*/~
                                    L21040,         /* Rank on Column   */~
                                    L21120,         /* Ascend or Descend*/~
                                    L21200,         /* Max to print     */~
                                    L21280,         /* Page break code  */~
                                    L21430,         /* Include groups   */~
                                    L21350,         /* Report Column A  */~
                                    L21350,         /* Report Column B  */~
                                    L21350,         /* Report Column C  */~
                                    L21350,         /* Report Column D  */~
                                    L21350,         /* Report Column E  */~
                                    L21350,         /* Report Column F  */~
                                    L21350          /* Report Column G  */
                     return

L20340: REM Report Name                           REPT_NAME$
            inpmessage$ = "Enter the name of the Report Format Definiti"&~
                "on you wish to add or modify"
            return

L20390: REM Report Description                    REPT_DESC$
            inpmessage$ = "Type a 30-character description of the Repor"&~
                "t Format Definition"
            return

L20440: REM Summary File #                        SUMM_FILE$
            inpmessage$ = "Enter an S/A Summary file number (0 thru 9)" &~
                ".  Blank/RETURN to see choices."
            return

L20490: REM Group Code 1 'FROM'
            inpmessage$ = "Enter 'from' range value for " &              ~
                codes$(group1%, 2%) & ", 'FIRST', or 'ALL'"
            return

L20540: REM Group Code 1 'TO'
            inpmessage$ = "Enter 'to' range value for " &                ~
                codes$(group1%, 2%) & ", or 'LAST'"
            return

L20590: REM Group Code 1 Desc                     GRP1_DESC$
            n% = min(grp1_max%, grp1_desc% + posn_rem%)
            convert n% to str(junk$,,2), pic (##)
            call "STRING" addr ("LJ", str(junk$,,2), 2%)
            inpmessage$ = "Enter the length of the description for " &   ~
                codes$(group1%, 2%) & ".  Maximum: " & str(junk$,,2)
            return

L20670: REM Group Code 2 'FROM'
            if group2% <> 0 then goto L20710
                init (" ") grcd2$(), code2$(), grp2_desc$
                enabled% = 0% : return
L20710:     inpmessage$ = "Enter 'from' range value for " &              ~
                codes$(group2%, 2%) & ", 'FIRST', or 'ALL'"
            return

L20750: REM Group Code 2 'TO'
            if group2% <> 0 then goto L20790
                init (" ") grcd2$(), code2$(), grp2_desc$
                enabled% = 0% : return
L20790:     inpmessage$ = "Enter 'to' range value for " &                ~
                codes$(group2%, 2%) & ", or 'LAST'"
            return

L20830: REM Group Code 2 Desc                     GRP2_DESC$
            if group2% <> 0 then goto L20870
                init (" ") grcd2$(), code2$(), grp2_desc$
                enabled% = 0% : return
L20870:     n% = min(grp2_max%, grp2_desc% + posn_rem%)
            convert n% to str(junk$,,2), pic (##)
            call "STRING" addr ("LJ", str(junk$,,2), 2%)
            inpmessage$ = "Enter the length of the description for " &   ~
                codes$(group2%, 2%) & ".  Maximum: " & str(junk$,,2)
            return

L20940: REM Report Type                           REPT_TYPE$
            inpmessage$ = "Enter the type of report: 'H' (History), 'R'"&~
                " (Ranking) or 'B' (Both)"
            return

L20990: REM Summary or Detail                     SUMM_DETL$
            inpmessage$ = "Enter 'S' for Summary report; 'D' for a Deta"&~
                "il report"
            return

L21040: REM Rank on Column                        RANK_COLM$
            if rept_type$ <> "H" then goto L21070
                enabled% = 0% : return
L21070:     if rank_colm$ = " " then rank_colm$ = "A"
            inpmessage$ = "Enter the column to rank (sort) the report o"&~
                "n (A thru G)"
            return

L21120: REM Ascend or Descend                     ASND_DSND$
            if rept_type$ <> "H" then goto L21150
                enabled% = 0% : return
L21150:     if asnd_dsnd$ = " " then asnd_dsnd$ = "A"
            inpmessage$ = "Enter 'A' to rank in ascending sequence; 'D'"&~
                " for descending"
            return

L21200: REM Max to print                          MAX_PRINT$
            if rept_type$ <> "H" then goto L21230
                enabled% = 0% : return
L21230:     if max_print$ = " " then max_print$ = "ALL"
            inpmessage$ = "Enter the maximum number of items to print o"&~
                "r 'ALL'"
            return

L21280: REM Page Break code                       PAGE_BRK$
            if summ_detl$ <> "S" then goto L21310
                enabled% = 0% : return
L21310:     inpmessage$ = "Enter 'Y' to force page breaks at " &         ~
                codes$(group1%, 2) & " totals; 'N' to omit breaks"
            return

L21350: REM Report Columns A - G
            if ffmt$(c%) <> " " then goto L21390
                if posn_rem% < 11% then return
                ffmt$(c%) = "072"
L21390:     inpmessage$ = "Define report column '" & col$(c%) &          ~
                "' contents.  Refer to keys, above for valid entries"
                return

L21430: REM Include groups outside specified years
            if incl$ = " " then incl$ = "N"
            inpmessage$ = "Should Report Include Summary Groups that " & ~
                          "have no Activity in Years Specified?"
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
            get #2 using L60070, rept_key$, rept_desc$, summ_file$,       ~
                grcd1$(), grcd2$(), grp1_desc$, grp2_desc$, rept_type$,  ~
                summ_detl$, rank_colm$, asnd_dsnd$, max_print$,          ~
                page_brk$, yr1$(), ytd1$(), fc1$(), oper$(), yr2$(),     ~
                ytd2$(), fc2$(), ffmt$(), colahdr$(), colbhdr$(),        ~
                colchdr$(), coldhdr$(), colehdr$(), colfhdr$(),          ~
                colghdr$(), incl$
            rept_name$ = str(rept_key$,11)
            if incl$ = " " then incl$ = "N"
            mat code1$ = grcd1$ : mat code2$ = grcd2$
            grp1_desc% = 0%
            convert grp1_desc$ to grp1_desc%, data goto L30180
L30180:     grp2_desc% = 0%
            convert grp2_desc$ to grp2_desc%, data goto L30200
L30200:     gosub select_summary_file
            for c% = 1% to 7%
                gosub compute_column_description
            next c%
            return

        REM *************************************************************~
            *         D E L E T E   R E P O R T   F O R M A T           *~
            *-----------------------------------------------------------*~
            * Asks operator confirmation; deletes report format defs.   *~
            *************************************************************
        deletefmt
            plowkey$ = "SA.RPTGRP." & hex(00)
            msg$ = " "
L30340:     call "PLOWNEXT" (#2, plowkey$, 10%, f1%(2))
            if f1%(2) = 0% then goto L30490
                get #2 using L30370, rept_grp$, rept_array$()
L30370:              FMT  POS(11), CH(10), POS(51), 12*CH(10)
                for ret% = 1% to 12%
                     if rept_array$(ret%) = " " then goto L30340
                     if rept_array$(ret%) = rept_name$ then goto L30430
                next ret%
                goto L30340
L30430:     ret% = 0%
            call "ASKUSER" (ret%, "*** GROUP REFERENCE ***", "All Group"&~
                " references to this report must be removed first", " ", ~
                "Press (RETURN) to continue")
            if ret% <> 0% then goto L30430
            return

L30490:     str(rept_key$,11) = rept_name$
L30500:     ret% = 2%
            call "ASKUSER" (ret%, "*** CONFIRM DELETION ***",            ~
                "Press PF(12) to DELETE Report Format '" & rept_name$ &  ~
                "'", "-- OR --", "Press (RETURN) to abort deletion")
            if ret% = 0% then return
            if ret% <> 12% then goto L30500
            call "READ101" (#2, rept_key$, f1%(2))
            if f1%(2) <> 0% then delete #2
            return clear all
            goto inputmode

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                line2$ = summ_desc$
                  str(line2$,62%) = "SARPTINP: " & str(cms2v$,,8%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40400,         /* Report Name      */~
                                    L40370,         /* Report Descriptio*/~
                                    L40430,         /* Summary File #   */~
                                    L40400,         /* Group Code 1 from*/~
                                    L40400,         /* Group Code 1 to  */~
                                    L40430,         /* Group Code 1 Desc*/~
                                    L40400,         /* Group Code 2 from*/~
                                    L40400,         /* Group Code 2 to  */~
                                    L40430,         /* Group Code 2 Desc*/~
                                    L40400,         /* Report Type      */~
                                    L40400,         /* Summary or Detail*/~
                                    L40400,         /* Rank on Column   */~
                                    L40400,         /* Ascend or Descend*/~
                                    L40400,         /* Max to print     */~
                                    L40400,         /* Page break code  */~
                                    L40400,         /* Include groups   */~
                                    L40400,         /* Report Column A  */~
                                    L40400,         /* Report Column B  */~
                                    L40400,         /* Report Column C  */~
                                    L40400,         /* Report Column D  */~
                                    L40400,         /* Report Column E  */~
                                    L40400,         /* Report Column F  */~
                                    L40400          /* Report Column G  */

                  goto fac_setting_done

L40370:           REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40400:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L40430:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

        fac_setting_done
            if fieldnr% <> 1% then L40480
                posn_rem% = 0%  /* To ensure that Desc Length from file */
                goto L40490      /* does not get reset.                  */
L40480:     gosub compute_posn_rem
L40490:     accept                                                       ~
               at (01,02),                                               ~
                  "Sales Analysis: Define Report Definitions",           ~
               at (01,66), "Today:"                             ,        ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,04), "Report Name:",                               ~
               at (04,17), fac(lfac$( 1)), rept_name$           , ch(10),~
               at (04,30), fac(lfac$( 2)), rept_desc$           , ch(30),~
                                                                         ~
               at (04,63), "Summary File:",                              ~
               at (04,77), fac(lfac$( 3)), summ_file$           , ch(01),~
                                                                         ~
               at (05,02), fac(hex(8c))  , group1$              , ch(14),~
               at (05,17), fac(lfac$( 4)), code1$(1)            ,        ~
               at (05,43),                 "-"                  ,        ~
               at (05,45), fac(lfac$( 5)), code1$(2)            ,        ~
               at (05,71),                 "Desc:"              ,        ~
               at (05,77), fac(lfac$( 6)), grp1_desc$           , ch(02),~
                                                                         ~
               at (06,02), fac(hex(8c))  , group2$              , ch(14),~
               at (06,17), fac(lfac$( 7)), code2$(1)            ,        ~
               at (06,43),                 "-"                  ,        ~
               at (06,45), fac(lfac$( 8)), code2$(2)            ,        ~
               at (06,71),                 "Desc:"              ,        ~
               at (06,77), fac(lfac$( 9)), grp2_desc$           , ch(02),~
                                                                         ~
               at (07,02), "Type:",                                      ~
               at (07,08), fac(lfac$(10)), rept_type$           , ch(01),~
               at (07,11), "S/D:",                                       ~
               at (07,16), fac(lfac$(11)), summ_detl$           , ch(01),~
               at (07,19), "Rank on:",                                   ~
               at (07,28), fac(lfac$(12)), rank_colm$           , ch(01),~
               at (07,31), "A/D:",                                       ~
               at (07,36), fac(lfac$(13)), asnd_dsnd$           , ch(01),~
               at (07,39), "# to print:",                                ~
               at (07,51), fac(lfac$(14)), max_print$           , ch(03),~
               at (07,56), "Brk?",                                       ~
               at (07,61), fac(lfac$(15)), page_brk$            , ch(01),~
               at (07,64), "Inc?",                                       ~
               at (07,69), fac(lfac$(16)), incl$                , ch(01),~
               at (07,72), "Pos:",                                       ~
               at (07,77), fac(hex(8c)),   posn_rem$            , ch(04),~
                                                                         ~
               at (08,02), fac(hex(ac)), col$                   , ch(03),~
               at (08,06), fac(hex(ac)), fc$                    , ch(03),~
               at (08,10), fac(hex(ac)), yr$                    , ch(02),~
               at (08,13), fac(hex(ac)), ytd$                   , ch(04),~
               at (08,18), fac(hex(ac)), op$                    , ch(04),~
               at (08,23), fac(hex(ac)), fc$                    , ch(03),~
               at (08,27), fac(hex(ac)), yr$                    , ch(02),~
               at (08,30), fac(hex(ac)), ytd$                   , ch(04),~
               at (08,35), fac(hex(ac)), fmt$                   , ch(03),~
               at (08,39), fac(hex(ac)), coldesc$               , ch(42),~
                                                                         ~
               at (09,03), "A"                                  ,        ~
               at (09,07), fac(lfac$(17)), fc1$(1)              , ch(01),~
               at (09,10), fac(lfac$(17)), yr1$(1)              , ch(02),~
               at (09,14), fac(lfac$(17)), ytd1$(1)             , ch(01),~
               at (09,19), fac(lfac$(17)), oper$(1)             , ch(01),~
               at (09,24), fac(lfac$(17)), fc2$(1)              , ch(01),~
               at (09,27), fac(lfac$(17)), yr2$(1)              , ch(02),~
               at (09,31), fac(lfac$(17)), ytd2$(1)             , ch(01),~
               at (09,35), fac(lfac$(17)), ffmt$(1)             , ch(03),~
               at (09,39), fac(hex(8c))  , cdescr$(1)           , ch(42),~
                                                                         ~
               at (10,03), "B"                                  ,        ~
               at (10,07), fac(lfac$(18)), fc1$(2)              , ch(01),~
               at (10,10), fac(lfac$(18)), yr1$(2)              , ch(02),~
               at (10,14), fac(lfac$(18)), ytd1$(2)             , ch(01),~
               at (10,19), fac(lfac$(18)), oper$(2)             , ch(01),~
               at (10,24), fac(lfac$(18)), fc2$(2)              , ch(01),~
               at (10,27), fac(lfac$(18)), yr2$(2)              , ch(02),~
               at (10,31), fac(lfac$(18)), ytd2$(2)             , ch(01),~
               at (10,35), fac(lfac$(18)), ffmt$(2)             , ch(03),~
               at (10,39), fac(hex(8c))  , cdescr$(2)           , ch(42),~
                                                                         ~
               at (11,03), "C"                                  ,        ~
               at (11,07), fac(lfac$(19)), fc1$(3)              , ch(01),~
               at (11,10), fac(lfac$(19)), yr1$(3)              , ch(02),~
               at (11,14), fac(lfac$(19)), ytd1$(3)             , ch(01),~
               at (11,19), fac(lfac$(19)), oper$(3)             , ch(01),~
               at (11,24), fac(lfac$(19)), fc2$(3)              , ch(01),~
               at (11,27), fac(lfac$(19)), yr2$(3)              , ch(02),~
               at (11,31), fac(lfac$(19)), ytd2$(3)             , ch(01),~
               at (11,35), fac(lfac$(19)), ffmt$(3)             , ch(03),~
               at (11,39), fac(hex(8c))  , cdescr$(3)           , ch(42),~
                                                                         ~
               at (12,03), "D"                                  ,        ~
               at (12,07), fac(lfac$(20)), fc1$(4)              , ch(01),~
               at (12,10), fac(lfac$(20)), yr1$(4)              , ch(02),~
               at (12,14), fac(lfac$(20)), ytd1$(4)             , ch(01),~
               at (12,19), fac(lfac$(20)), oper$(4)             , ch(01),~
               at (12,24), fac(lfac$(20)), fc2$(4)              , ch(01),~
               at (12,27), fac(lfac$(20)), yr2$(4)              , ch(02),~
               at (12,31), fac(lfac$(20)), ytd2$(4)             , ch(01),~
               at (12,35), fac(lfac$(20)), ffmt$(4)             , ch(03),~
               at (12,39), fac(hex(8c))  , cdescr$(4)           , ch(42),~
                                                                         ~
               at (13,03), "E"                                  ,        ~
               at (13,07), fac(lfac$(21)), fc1$(5)              , ch(01),~
               at (13,10), fac(lfac$(21)), yr1$(5)              , ch(02),~
               at (13,14), fac(lfac$(21)), ytd1$(5)             , ch(01),~
               at (13,19), fac(lfac$(21)), oper$(5)             , ch(01),~
               at (13,24), fac(lfac$(21)), fc2$(5)              , ch(01),~
               at (13,27), fac(lfac$(21)), yr2$(5)              , ch(02),~
               at (13,31), fac(lfac$(21)), ytd2$(5)             , ch(01),~
               at (13,35), fac(lfac$(21)), ffmt$(5)             , ch(03),~
               at (13,39), fac(hex(8c))  , cdescr$(5)           , ch(42),~
                                                                         ~
               at (14,03), "F"                                  ,        ~
               at (14,07), fac(lfac$(22)), fc1$(6)              , ch(01),~
               at (14,10), fac(lfac$(22)), yr1$(6)              , ch(02),~
               at (14,14), fac(lfac$(22)), ytd1$(6)             , ch(01),~
               at (14,19), fac(lfac$(22)), oper$(6)             , ch(01),~
               at (14,24), fac(lfac$(22)), fc2$(6)              , ch(01),~
               at (14,27), fac(lfac$(22)), yr2$(6)              , ch(02),~
               at (14,31), fac(lfac$(22)), ytd2$(6)             , ch(01),~
               at (14,35), fac(lfac$(22)), ffmt$(6)             , ch(03),~
               at (14,39), fac(hex(8c))  , cdescr$(6)           , ch(42),~
                                                                         ~
               at (15,03), "G"                                  ,        ~
               at (15,07), fac(lfac$(23)), fc1$(7)              , ch(01),~
               at (15,10), fac(lfac$(23)), yr1$(7)              , ch(02),~
               at (15,14), fac(lfac$(23)), ytd1$(7)             , ch(01),~
               at (15,19), fac(lfac$(23)), oper$(7)             , ch(01),~
               at (15,24), fac(lfac$(23)), fc2$(7)              , ch(01),~
               at (15,27), fac(lfac$(23)), yr2$(7)              , ch(02),~
               at (15,31), fac(lfac$(23)), ytd2$(7)             , ch(01),~
               at (15,35), fac(lfac$(23)), ffmt$(7)             , ch(03),~
               at (15,39), fac(hex(8c))  , cdescr$(7)           , ch(42),~
                                                                         ~
               at (16,02), fac(hex(ac)), line16$                , ch(79),~
               at (17,02), fac(hex(8c)), line17$                , ch(79),~
               at (18,02), fac(hex(8c)), line18$                , ch(79),~
               at (19,02), fac(hex(8c)), line19$                , ch(79),~
               at (20,02), fac(hex(8c)), line20$                , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
               at (22,02), fac(pf1fac$), pf1$                   ,        ~
               at (22,21), fac(pf4fac$), pf4$                   ,        ~
               at (22,42), fac(pf12fac$), pf12$                 ,        ~
               at (22,63), "(13)Instructions"                   ,        ~
                                                                         ~
               at (23,02), fac(pf2fac$), pf2$                   ,        ~
               at (23,63), "(15)Print Screen"                   ,        ~
                                                                         ~
               at (24,02), fac(pf3fac$), pf3$                   ,        ~
               at (24,21), fac(pf8fac$), pf8$                   ,        ~
               at (24,63), fac(pf16fac$), pf16$                 ,        ~
                                                                         ~
               keys(keytab$), key (keyhit%)

               if keyhit% <> 13 then L42080
                  call "MANUAL" ("SARPTINP")
                  goto L40490

L42080:        if keyhit% <> 15 then L42120
                  call "PRNTSCRN"
                  goto L40490

L42120:        if fieldnr% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Operator edits the report column headers.                 *~
            *************************************************************

            deffn'102
L45080:     accept                                                       ~
               at (01,02),                                               ~
                  "Sales Analysis: Edit Report Column Headers",          ~
               at (01,66), "Today:"                             ,        ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,04), "Report Name:",                               ~
               at (04,17), fac(hex(8c)),   rept_name$           , ch(10),~
               at (04,30), fac(hex(8c)),   rept_desc$           , ch(30),~
                                                                         ~
               at (04,63), "Summary File:",                              ~
               at (04,77), fac(hex(8c)),   summ_file$           , ch(01),~
                                                                         ~
               at (05,02), fac(hex(8c)),   group1$              , ch(14),~
               at (05,17), fac(hex(8c)),   code1$(1)            ,        ~
               at (05,43),                 "-"                  ,        ~
               at (05,45), fac(hex(8c)),   code1$(2)            ,        ~
               at (05,71),                 "Desc:"              ,        ~
               at (05,77), fac(hex(8c)),   grp1_desc$           , ch(02),~
                                                                         ~
               at (06,02), fac(hex(8c)),   group2$              , ch(14),~
               at (06,17), fac(hex(8c)),   code2$(1)            ,        ~
               at (06,43),                 "-"                  ,        ~
               at (06,45), fac(hex(8c)),   code2$(2)            ,        ~
               at (06,71),                 "Desc:"              ,        ~
               at (06,77), fac(hex(8c)),   grp2_desc$           , ch(02),~
                                                                         ~
               at (07,02), "Type:",                                      ~
               at (07,08), fac(hex(8c)),   rept_type$           , ch(01),~
               at (07,11), "S/D:",                                       ~
               at (07,16), fac(hex(8c)),   summ_detl$           , ch(01),~
               at (07,19), "Rank on:",                                   ~
               at (07,28), fac(hex(8c)),   rank_colm$           , ch(01),~
               at (07,31), "A/D:",                                       ~
               at (07,36), fac(hex(8c)),   asnd_dsnd$           , ch(01),~
               at (07,39), "# to print:",                                ~
               at (07,51), fac(hex(8c)),   max_print$           , ch(03),~
               at (07,56), "Brk?",                                       ~
               at (07,61), fac(hex(8c)),   page_brk$            , ch(01),~
               at (07,64), "Inc?",                                       ~
               at (07,69), fac(hex(8c)),   incl$                , ch(01),~
               at (07,72), "Pos:",                                       ~
               at (07,77), fac(hex(8c)),   posn_rem$            , ch(04),~
                                                                         ~
               at (08,02), fac(hex(ac)), col$                   , ch(03),~
               at (08,06), fac(hex(ac)), fc$                    , ch(03),~
               at (08,10), fac(hex(ac)), yr$                    , ch(02),~
               at (08,13), fac(hex(ac)), ytd$                   , ch(04),~
               at (08,18), fac(hex(ac)), op$                    , ch(04),~
               at (08,23), fac(hex(ac)), fc$                    , ch(03),~
               at (08,27), fac(hex(ac)), yr$                    , ch(02),~
               at (08,30), fac(hex(ac)), ytd$                   , ch(04),~
               at (08,35), fac(hex(ac)), fmt$                   , ch(03),~
               at (08,39), fac(hex(ac)), coldesc$               , ch(42),~
                                                                         ~
               at (09,03), "A"                                  ,        ~
               at (09,07), fac(hex(8c)),   fc1$(1)              , ch(01),~
               at (09,10), fac(hex(8c)),   yr1$(1)              , ch(02),~
               at (09,14), fac(hex(8c)),   ytd1$(1)             , ch(01),~
               at (09,19), fac(hex(8c)),   oper$(1)             , ch(01),~
               at (09,24), fac(hex(8c)),   fc2$(1)              , ch(01),~
               at (09,27), fac(hex(8c)),   yr2$(1)              , ch(02),~
               at (09,31), fac(hex(8c)),   ytd2$(1)             , ch(01),~
               at (09,35), fac(hex(8c)),   ffmt$(1)             , ch(03),~
               at (09,39), fac(hex(8c)),   cdescr$(1)           , ch(42),~
                                                                         ~
               at (10,03), "B"                                  ,        ~
               at (10,07), fac(hex(8c)),   fc1$(2)              , ch(01),~
               at (10,10), fac(hex(8c)),   yr1$(2)              , ch(02),~
               at (10,14), fac(hex(8c)),   ytd1$(2)             , ch(01),~
               at (10,19), fac(hex(8c)),   oper$(2)             , ch(01),~
               at (10,24), fac(hex(8c)),   fc2$(2)              , ch(01),~
               at (10,27), fac(hex(8c)),   yr2$(2)              , ch(02),~
               at (10,31), fac(hex(8c)),   ytd2$(2)             , ch(01),~
               at (10,35), fac(hex(8c)),   ffmt$(2)             , ch(03),~
               at (10,39), fac(hex(8c)),   cdescr$(2)           , ch(42),~
                                                                         ~
               at (11,03), "C"                                  ,        ~
               at (11,07), fac(hex(8c)),   fc1$(3)              , ch(01),~
               at (11,10), fac(hex(8c)),   yr1$(3)              , ch(02),~
               at (11,14), fac(hex(8c)),   ytd1$(3)             , ch(01),~
               at (11,19), fac(hex(8c)),   oper$(3)             , ch(01),~
               at (11,24), fac(hex(8c)),   fc2$(3)              , ch(01),~
               at (11,27), fac(hex(8c)),   yr2$(3)              , ch(02),~
               at (11,31), fac(hex(8c)),   ytd2$(3)             , ch(01),~
               at (11,35), fac(hex(8c)),   ffmt$(3)             , ch(03),~
               at (11,39), fac(hex(8c)),   cdescr$(3)           , ch(42),~
                                                                         ~
               at (12,03), "D"                                  ,        ~
               at (12,07), fac(hex(8c)),   fc1$(4)              , ch(01),~
               at (12,10), fac(hex(8c)),   yr1$(4)              , ch(02),~
               at (12,14), fac(hex(8c)),   ytd1$(4)             , ch(01),~
               at (12,19), fac(hex(8c)),   oper$(4)             , ch(01),~
               at (12,24), fac(hex(8c)),   fc2$(4)              , ch(01),~
               at (12,27), fac(hex(8c)),   yr2$(4)              , ch(02),~
               at (12,31), fac(hex(8c)),   ytd2$(4)             , ch(01),~
               at (12,35), fac(hex(8c)),   ffmt$(4)             , ch(03),~
               at (12,39), fac(hex(8c)),   cdescr$(4)           , ch(42),~
                                                                         ~
               at (13,03), "E"                                  ,        ~
               at (13,07), fac(hex(8c)),   fc1$(5)              , ch(01),~
               at (13,10), fac(hex(8c)),   yr1$(5)              , ch(02),~
               at (13,14), fac(hex(8c)),   ytd1$(5)             , ch(01),~
               at (13,19), fac(hex(8c)),   oper$(5)             , ch(01),~
               at (13,24), fac(hex(8c)),   fc2$(5)              , ch(01),~
               at (13,27), fac(hex(8c)),   yr2$(5)              , ch(02),~
               at (13,31), fac(hex(8c)),   ytd2$(5)             , ch(01),~
               at (13,35), fac(hex(8c)),   ffmt$(5)             , ch(03),~
               at (13,39), fac(hex(8c)),   cdescr$(5)           , ch(42),~
                                                                         ~
               at (14,03), "F"                                  ,        ~
               at (14,07), fac(hex(8c)),   fc1$(6)              , ch(01),~
               at (14,10), fac(hex(8c)),   yr1$(6)              , ch(02),~
               at (14,14), fac(hex(8c)),   ytd1$(6)             , ch(01),~
               at (14,19), fac(hex(8c)),   oper$(6)             , ch(01),~
               at (14,24), fac(hex(8c)),   fc2$(6)              , ch(01),~
               at (14,27), fac(hex(8c)),   yr2$(6)              , ch(02),~
               at (14,31), fac(hex(8c)),   ytd2$(6)             , ch(01),~
               at (14,35), fac(hex(8c)),   ffmt$(6)             , ch(03),~
               at (14,39), fac(hex(8c)),   cdescr$(6)           , ch(42),~
                                                                         ~
               at (15,03), "G"                                  ,        ~
               at (15,07), fac(hex(8c)),   fc1$(7)              , ch(01),~
               at (15,10), fac(hex(8c)),   yr1$(7)              , ch(02),~
               at (15,14), fac(hex(8c)),   ytd1$(7)             , ch(01),~
               at (15,19), fac(hex(8c)),   oper$(7)             , ch(01),~
               at (15,24), fac(hex(8c)),   fc2$(7)              , ch(01),~
               at (15,27), fac(hex(8c)),   yr2$(7)              , ch(02),~
               at (15,31), fac(hex(8c)),   ytd2$(7)             , ch(01),~
               at (15,35), fac(hex(8c)),   ffmt$(7)             , ch(03),~
               at (15,39), fac(hex(8c)),   cdescr$(7)           , ch(42),~
                                                                         ~
                at (17,05), "- A -"                             ,        ~
                at (17,16), "- B -"                             ,        ~
                at (17,27), "- C -"                             ,        ~
                at (17,38), "- D -"                             ,        ~
                at (17,49), "- E -"                             ,        ~
                at (17,60), "- F -"                             ,        ~
                at (17,71), "- G -"                             ,        ~
                                                                         ~
                at (18,03), fac(cfac$(1)),colahdr$(1)           , ch(10),~
                at (18,14), fac(cfac$(2)),colbhdr$(1)           , ch(10),~
                at (18,25), fac(cfac$(3)),colchdr$(1)           , ch(10),~
                at (18,36), fac(cfac$(4)),coldhdr$(1)           , ch(10),~
                at (18,47), fac(cfac$(5)),colehdr$(1)           , ch(10),~
                at (18,58), fac(cfac$(6)),colfhdr$(1)           , ch(10),~
                at (18,69), fac(cfac$(7)),colghdr$(1)           , ch(10),~
                                                                         ~
                at (19,03), fac(cfac$(1)),colahdr$(2)           , ch(10),~
                at (19,14), fac(cfac$(2)),colbhdr$(2)           , ch(10),~
                at (19,25), fac(cfac$(3)),colchdr$(2)           , ch(10),~
                at (19,36), fac(cfac$(4)),coldhdr$(2)           , ch(10),~
                at (19,47), fac(cfac$(5)),colehdr$(2)           , ch(10),~
                at (19,58), fac(cfac$(6)),colfhdr$(2)           , ch(10),~
                at (19,69), fac(cfac$(7)),colghdr$(2)           , ch(10),~
                                                                         ~
                at (20,03), fac(cfac$(1)),colahdr$(3)           , ch(10),~
                at (20,14), fac(cfac$(2)),colbhdr$(3)           , ch(10),~
                at (20,25), fac(cfac$(3)),colchdr$(3)           , ch(10),~
                at (20,36), fac(cfac$(4)),coldhdr$(3)           , ch(10),~
                at (20,47), fac(cfac$(5)),colehdr$(3)           , ch(10),~
                at (20,58), fac(cfac$(6)),colfhdr$(3)           , ch(10),~
                at (20,69), fac(cfac$(7)),colghdr$(3)           , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
               at (22,02), fac(hex(8c)),  pf1$                  ,        ~
               at (22,63), "(13)Instructions"                   ,        ~
                                                                         ~
               at (23,63), "(15)Print Screen"                   ,        ~
                                                                         ~
               at (24,21), "(8)Return"                          ,        ~
                                                                         ~
               keys(hex(01080d0f00)), key (resp%)

               if resp% <> 13 then L46860
                  call "MANUAL" ("SARPTINP")
                  goto L45080

L46860:        if resp% <> 15 then return
                  call "PRNTSCRN"
                  goto L45080

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50330,         /* Report Name      */~
                                    L50440,         /* Report Descriptio*/~
                                    L50470,         /* Summary File #   */~
                                    L50540,         /* Group Code 1 from*/~
                                    L50720,         /* Group Code 1 to  */~
                                    L50850,         /* Group Code 1 Desc*/~
                                    L50950,         /* Group Code 2 from*/~
                                    L51100,         /* Group Code 2 to  */~
                                    L51240,         /* Group Code 2 Desc*/~
                                    L51350,         /* Report Type      */~
                                    L51520,         /* Summary or Detail*/~
                                    L51630,         /* Rank on Column   */~
                                    L51700,         /* Ascend or Descend*/~
                                    L51770,         /* Max to print     */~
                                    L51910,         /* Page break code  */~
                                    L52020,         /* Include Groups   */~
                                    L51980,         /* Report Column A  */~
                                    L51980,         /* Report Column B  */~
                                    L51980,         /* Report Column C  */~
                                    L51980,         /* Report Column D  */~
                                    L51980,         /* Report Column E  */~
                                    L51980,         /* Report Column F  */~
                                    L51980          /* Report Column G  */
                  return

L50330: REM Report Name                           REPT_NAME$
            plowkey$ = str(rept_key$,,10) & rept_name$
            msg$ = hex(06) & "Select a report definition: (" &           ~
                rept_name$ & ")"
            call "PLOWCODE" (#2, plowkey$, msg$, 10%, 0.30, f1%(2))
            if f1%(2) <> 0% then gosub dataload
            if rept_name$ = " " then errormsg$ = "The Report Definition"&~
                " name may not be blank.  Try again."
            str(rept_key$,11) = rept_name$
            return

L50440: REM Report Description                    REPT_DESC$
            return

L50470: REM Summary File #                        SUMM_FILE$
            gosub select_summary_file
            if f2%(1) = 0% then return
                errormsg$ = "The file you have selected doesn't exist."& ~
                     "  Try again."
            return

L50540: REM Group Code 1 FROM
            if code1$(1) = " " then return
            if code1$(1) <> "ALL" then goto L50600
                code1$(2) = " "
                fieldnr% = fieldnr% + 1%
                return
L50600:     if code1$(1) = "FIRST" then return
            g% = group1%
            if len(code1$(1)) > codelen%(group1%) then goto L50670
            codex$ = code1$(1) : file% = file1%
            gosub capture_codes
            code1$(1) = codex$
            return
L50670:     convert codelen%(g%) to junk$, pic (##)
            errormsg$ = "Length of entry is too long. " & str(junk$,,2) &~
                " is the maximum length."
            return

L50720: REM Group Code 1 TO
            if code1$(2) = " " then return
            if code1$(1) = "ALL" then goto L50810
            if code1$(2) = "LAST" then goto L50810
            g% = group1%
            if len(code1$(2)) > codelen%(group1%) then goto L50670
            codex$ = code1$(2) : file% = file1%
            gosub capture_codes
            code1$(2) = codex$
L50810:     call "TESTRNGE" (code1$(1), code1$(2), junk$, junk$,         ~
                errormsg$)
            return

L50850: REM Group Code 1 Desc                     GRP1_DESC$
            n% = grp1_desc%
            convert grp1_desc$ to grp1_desc%, data goto L50910
            if grp1_desc% <= min(grp1_max%, n% + posn_rem%) then return
            errormsg$ = "Description length exceeds allowable maximum"
            return
L50910:     errormsg$ = "Description length format error: " &            ~
                grp1_desc$
            return

L50950: REM Group Code 2 FROM
            if group2% = 0% then return
            if code2$(1) = " " then return
            if code2$(1) <> "ALL" then goto L51020
                code2$(2) = " "
                fieldnr% = fieldnr% + 1%
                return
L51020:     if code2$(1) = "FIRST" then return
            g% = group2%
            if len(code2$(1)) > codelen%(group2%) then goto L50670
            codex$ = code2$(1) : file% = file2%
            gosub capture_codes
            code2$(1) = codex$
            return

L51100: REM Group Code 2 TO
            if group2% = 0% then return
            if code2$(1) = " " then return
            if code2$(1) = "ALL" then goto L51200
            if code2$(2) = "LAST" then goto L51200
            g% = group2%
            if len(code2$(2)) > codelen%(group2%) then goto L50670
            codex$ = code2$(2) : file% = file2%
            gosub capture_codes
            code2$(2) = codex$
L51200:     call "TESTRNGE" (code2$(1), code2$(2), junk$, junk$,         ~
                errormsg$)
            return

L51240: REM Group Code 2 Desc                     GRP2_DESC$
            if group2% = 0% then return
            n% = grp2_desc%
            convert grp2_desc$ to grp2_desc%, data goto L51310
            if grp2_desc% <= min(grp2_max%, n% + posn_rem%) then return
            errormsg$ = "Description length exceeds allowable maximum"
            return
L51310:     errormsg$ = "Description length format error: " &            ~
                grp2_desc$
            return

L51350: REM Report Type                           REPT_TYPE$
            if rept_type$ <> "H" then goto L51390
                rank_colm$, asnd_dsnd$, max_print$ = " "
                goto L51440
L51390:     if rank_colm$ = " " then rank_colm$ = "A"
            if asnd_dsnd$ = " " then asnd_dsnd$ = "A"
            if max_print$ = " " then max_print$ = "ALL"
            if rept_type$ = "R" then goto L51450
            if rept_type$ <> "B" then goto L51480
L51440:     if group2% <> 0% then return
L51450:         summ_detl$ = "S" : page_brk$ = " "
                fieldnr% = fieldnr% + 1%
                return
L51480:     errormsg$ = "Type must be 'H' (History), 'R' (Ranking), or "&~
                "'B' (Both)"
            return

L51520: REM Summary or Detail                     SUMM_DETL$
            if summ_detl$ <> "S" then goto L51550
                page_brk$ = " " : goto L51560
L51550:     if summ_detl$ <> "D" then goto L51590
L51560:     if rept_type$ <> "H" then return
                fieldnr% = fieldnr% + 3%
                return
L51590:     errormsg$ = "You must enter 'S' for a Summary report; 'D'"&  ~
                " for a Detail report"
            return

L51630: REM Rank on Column                        RANK_COLM$
            if rank_colm$ < "A" then goto L51660
            if rank_colm$ < "H" then return
L51660:     errormsg$ = "Ranking must be on one of the report columns ("&~
                "A thru G), below"
            return

L51700: REM Ascend or Descend                     ASND_DSND$
            if asnd_dsnd$ = "A" then return
            if asnd_dsnd$ = "D" then return
            errormsg$ = "You must enter 'A' for Ascending sequence; 'D'"&~
                " for Descending sequence"
            return

L51770: REM Max to print                          MAX_PRINT$
            if max_print$ = "ALL" then return
            convert max_print$ to max_print%, data goto L51870
            if max_print% < 1% then goto L51870
            if max_print% > 0% then goto L51840
            errormsg$ = "Must be greater than zero"
            return
L51840:     convert max_print% to max_print$, pic (###)
            call "STRING" addr ("LJ", max_print$, 3%)
            return
L51870:     errormsg$ = "Number to Print has an invalid format: " &      ~
                max_print$
            return

L51910: REM Page break code                       PAGE_BRK$
            if page_brk$ = "Y" then return
            if page_brk$ = "N" then return
            errormsg$ = "You must enter 'Y' for page breaks; 'N' to " &  ~
                "omit them"
            return

L51980: REM Report Columns A - G
            gosub validate_a_column
            return

L52020: REM Include groups outside years          INCL$
            if incl$ = "Y" or incl$ = "N" then return
            errormsg$ = "You must enter 'Y' or 'N'."
            return

        REM *************************************************************~
            *             I M A G E   S T A T E M E N T S               *~
            *-----------------------------------------------------------*~
            * Record layouts.                                           *~
            *************************************************************

L60070:     FMT /* SA.RPTDEF.xxxxxxxxxx record layout in file SYSFILE2 */~
                CH(20),                  /* Report Format Def name     */~
                CH(30),                  /* Report Format Def descriptn*/~
                CH(1),                   /* S/A Summary file #         */~
                2*CH(25),                /* Group 1 range values       */~
                2*CH(25),                /* Group 2 range values       */~
                CH(2),                   /* Group 1 description length */~
                CH(2),                   /* Group 2 description length */~
                CH(1),                   /* Type of report (H, R, B)   */~
                CH(1),                   /* Summary or Detail (S, D)   */~
                CH(1),                   /* Rank on Column (A - G)     */~
                CH(1),                   /* Ascending or Descending    */~
                CH(3),                   /* Max # to print (ALL, n)    */~
                CH(1),                   /* Page break code            */~
                7*CH(2),                 /* 1st fld Year rel to current*/~
                7*CH(1),                 /* 1st fld YTD indicator      */~
                7*CH(1),                 /* 1st fld code/column entry  */~
                7*CH(1),                 /* Column operator            */~
                7*CH(2),                 /* 2nd fld Year rel to current*/~
                7*CH(1),                 /* 2nd fld YTD indicator      */~
                7*CH(1),                 /* 2nd fld code/column entry  */~
                7*CH(3),                 /* Column format- 'nnd'       */~
                3*CH(10),                /* Column A report headers    */~
                3*CH(10),                /* Column B report headers    */~
                3*CH(10),                /* Column C report headers    */~
                3*CH(10),                /* Column D report headers    */~
                3*CH(10),                /* Column E report headers    */~
                3*CH(10),                /* Column F report headers    */~
                3*CH(10),                /* Column G report headers    */~
                CH(1),                   /* Include groups outside yrs */~
                CH(42)                   /* Filler                     */

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
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
