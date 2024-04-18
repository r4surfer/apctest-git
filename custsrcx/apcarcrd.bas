        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCARCRD                             *~
            *  Creation Date     - 05/11/95                             *~
            *  Last Modified Date- 11/05/97                             *~
            *  Description       - This Program Scans the 'ARIMASTR'    *~
            *                      File and Totals the Invoices or      *~
            *                      Credit Memos Associated with Specific*~
            *                      Invoice Reason Codes or 'ALL' Codes  *~
            *                                                           *~
            *  Code Tables Used  - (INVREASON) - Code Table             *~
            *                                                           *~
            *  Special Comments  - A BLANK CODE WILL GO TO BUCKET (28)  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/11/95 ! New Program for (APC) - Last Mod Date    ! RHH *~
            *          !                                          !     *~
            * 11/05/97 ! Changed Version To Reflect 60403         ! DJD *~
            *          !                                          !     *~
            * 11/11/97 ! Added fourth key to ARIMASTR (60403)     ! DJD *~
            *          !                                          !     *~
            * 04/01/98 ! Y2K modification                         ! ERN *~
            *************************************************************

        dim                                                              ~
            aa$(100%)4, tab_hdr$(3%)40,  /* Inv Reason Codes           */~
            bb$(100%)30, cnt$8,          /* INV Reason Descriptions    */~
            cc$(100%)5,                  /* Inv Count Assoc. Code      */~
            dd$(100%)14,                 /* Total Amount For Code      */~
            readkey$24, descr$30,        /* GENCODES Key               */~
            inv_key$8,                   /* ARIMASTR Primary Key       */~
            inv_code$9, inv_code_desc$30,/* Invoice Reason Code        */~
            invdate$6, postdate$6,       /* Invoice Date, Posting Date */~
            inv_typ$1, inv_typ_desc$30,  /* Invoice Type Code          */~
            chk_cd$9,                    /* Input Reason Code          */~
            beg_dte$6, beg_date$10, x$10,/* Beginning Date             */~
            end_dte$6, end_date$10,      /* Ending Date                */~
            inv_buf(100%,6%),            /* Total Buffer               */~
            date$10,                     /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            cursor%(2%),                 /* Cursor location for edit   */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            company$40, line2$79,        /* Comapny Name and Title     */~
            userid$3                     /* Current User Id            */~

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 11/05/97 A/R Reason Code Scan Utility   "
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
            * #1  ! ARIMASTR ! Invoice Master File                      *~
            * #2  ! ARILINES ! Invoice Line Items File                  *~
            * #3  ! GENCODES ! GENERAL SYSTEM CODES FILE                *~
            * #4  ! CUSTOMER ! CUSTOMER MASTER FILE                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  17,                     ~
                        alt key  1, keypos =   10, keylen =   8, dup,    ~
                            key  2, keypos =   18, keylen =  16, dup,    ~
                            key  3, keypos =   34, keylen =  16, dup,    ~
                            key  4, keypos = 1783, keylen =  26, dup

            select #2,  "ARILINES",                                      ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =    1, keylen =  20

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =   24

            select #4,   "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 0%, rslt$(1%))
        REM CALL "OPENCHCK" (#2, FS%(2%), F2%(2%), 0%, RSLT$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%), 0%, rslt$(3%))
        REM CALL "OPENCHCK" (#4, FS%(4%), F2%(4%), 0%, RSLT$(4%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATFMTC" (date$) : ret% = 0%
            call "COMPNAME" (12%, company$, ret%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62%) = "APCARCRD: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  4%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10210:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles EDIT MODE for range selection screen.             *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then gosub process_data
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% >  4% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *           E X T R A C T   R E P O R T   D A T A           *~
            *-----------------------------------------------------------*~
            * Data Extraction section for report.                       *~
            *************************************************************

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
        return
        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter the Beginning Posting Date?                            ",~
         "Enter the Ending Posting Date?                               ",~
         "Enter a Valid Invoice Reason Code or 'AA' for (ALL)?         ",~
         "Enter a Valid Invoice Type Code,(O,I,A,C,R,F) or X = 'ALL'?  "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, beg_dte$, beg_date$,       ~
                      end_dte$, end_date$, inv_typ$, inv_code$, invdate$,~
                      postdate$, chk_cd$, inv_code_desc$,                ~
                      readkey$, descr$, inv_typ_desc$, aa$(), bb$(),     ~
                      cc$(), dd$()
            mat inv_buf = zer
        return
        REM *************************************************************~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40180,                /* Beginning Date*/~
                                L40180,                /* Ending Date   */~
                                L40180,                /* Inv Reason Cd */~
                                L40180                 /* Inv Type Code */
              goto L40210

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40210:     accept                                                       ~
               at (01,02),                                               ~
                  "APC A/R Invoice Reason Code Utility Screen",          ~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (05,02), "Begining Posting Date:",                     ~
               at (05,25), fac(lfac$(1%)), beg_date$            , ch(10),~
                                                                         ~
               at (06,02), "Ending Posting Date  :",                     ~
               at (06,25), fac(lfac$(2%)), end_date$            , ch(10),~
                                                                         ~
               at (07,02), "Invoice Reason Code  :",                     ~
               at (07,25), fac(lfac$(3%)), chk_cd$              , ch(09),~
               at (07,40), fac(hex(84)), inv_code_desc$         , ch(30),~
                                                                         ~
               at (08,02), "Invoice Type Code    :",                     ~
               at (08,25), fac(lfac$(4%)), inv_typ$             , ch(01),~
               at (08,40), fac(hex(84)), inv_typ_desc$          , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 9% then L40540
                  gosub display_codes

L40540:        if keyhit% <> 15 then L40570
                  call "PRNTSCRN" : goto L40210

L40570:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40760     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                 (9)Display Codes       " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffff09ffffffffff0f1000)
            if fieldnr% = 1% then L40730
                str(pf$(3%),64%)  = " " : str(pfkeys$,16%,1%) = hex(ff)
            if fieldnr% > 1% then L40740
L40730:         str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40740:     return

L40760: if fieldnr% > 0% then L40850  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                 (9)Display Codes       " &       ~
                      "                       (16)Process Data"
            pfkeys$ = hex(01ffffffffffffff09ffffff0dff0f1000)
            return
L40850:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50140,                 /* Beg Post Date*/  ~
                              L50230,                 /* End Post Date*/  ~
                              L50350,                 /* Reason Code  */  ~
                              L50540                  /* INV Type Code*/
            return

L50140: REM Beginning Posting Date                BEG_DTE$, BEG_DATE$
            date% = 0%
            call "DATEOKC" (beg_date$, date%, errormsg$)
            if date% = 0% then return
            x$ = beg_date$
            call "DATUFMTC"(x$)
            beg_dte$ = str(x$,1%,6%)
        return

L50230: REM Ending Posting Date                   END_DTE$, END_DATE$
            if end_date$ <> " " then goto L50270
               end_date$ = beg_date$

L50270:     date% = 0%
            call "DATEOKC" (end_date$, date%, errormsg$)
            if date% = 0% then return
            x$ = end_date$
            call "DATUFMTC"(x$)
            end_dte$ = str(x$,1%,6%)
        return

L50350: REM Invoice Reason Code                   INV_CODE$
            if chk_cd$ <> " " then goto L50420
L50370:        chk_cd$ = "AA"
               inv_code_desc$ = "(ALL) Invoice Reason Codes"
               chk_cd% = 0%
               return

L50420:     if str(chk_cd$,1%,1%) = "A" then goto L50370
               gosub lookup_reason
               if inv_code_desc$ <> " " then goto L50470
                  goto L50500

L50470:        convert chk_cd$ to chk_cd%, data goto L50480
L50480:
        return
L50500:     errormsg$ = "(Error) - Invalid Invoice Reason Code?"
            chk_cd$, inv_code_desc$ = " "
        return

L50540: REM Invoice Type Code                     INV_TYP$
            inv_typ_desc$ = " "
            if inv_typ$ <> " " then goto L50590
               inv_typ$ = "X"

L50590:     if inv_typ$ = "O" then inv_typ_desc$ = "Sales Order"
            if inv_typ$ = "I" then inv_typ_desc$ = "Direct     "
            if inv_typ$ = "A" then inv_typ_desc$ = "Adjustment "
            if inv_typ$ = "C" then inv_typ_desc$ = "Credit Memo"
            if inv_typ$ = "R" then inv_typ_desc$ = "Recurring  "
            if inv_typ$ = "F" then inv_typ_desc$ = "Finance Chg"
            if inv_typ$ = "X" then inv_typ_desc$ = "(All) Codes"
            sav_typ$ = inv_typ$
            if inv_typ_desc$ <> " " then return
               goto L50690
        return
L50690:     errormsg$ = "(Error) - Invalid Invoice Type Code?"
            init(" ") inv_typ$, inv_typ_desc$, sav_typ$
        return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

        lookup_reason
            init(" ") readkey$, inv_code_desc$
            str(readkey$,1%,9%)   = "INVREASON"
            str(readkey$,10%,15%) = chk_cd$
            read #3,key = readkey$,using L60120, inv_code_desc$,          ~
                                                eod goto L60130
L60120:        FMT POS(25), CH(30)
L60130: return

        display_codes
           readkey$ = " "
           str(readkey$,1%,9%) = "INVREASON"
           descr$ =hex(06) & "Invoice Reason Codes"
           call "PLOWCODE" (#3, readkey$, descr$, 9%, .30, f1%(3))
        return

        process_data
           call "SHOSTAT" ("Scanning A/R Reason Code Data")
           mat inv_buf = zer
           cnt% = 0%
           init(" ") inv_key$
        arimastr_loop
            read #1,key 1% > inv_key$,using L60320, inv_key$,postdate$,   ~
                             inv_typ$, inv_code$, eod goto process_done
L60320:        FMT POS(10),CH(8), POS(533), CH(6),POS(891),CH(1),CH(9)
            cnt% = cnt% + 1%
            if mod(cnt%,100) <> 0 then goto L60380
               convert cnt% to cnt$, pic(00000000)
               call "SHOSTAT" ("Invoices Scanned .... "& cnt$ &")")

L60380:     if postdate$ < beg_dte$ or postdate$ > end_dte$ then         ~
                                                       goto arimastr_loop
            if sav_typ$ = "X" then goto L60430
               if sav_typ$ <> inv_typ$ then goto arimastr_loop

L60430:     inv_code% = 0%
            convert inv_code$ to inv_code%, data goto L60450
L60450:
            if inv_code% = 0% then inv_code% = 28%   /* MISC */
            if chk_cd$ = "AA" then goto L60490
               if chk_cd% <> inv_code% then goto arimastr_loop
L60490:     get #1,using L60510,invdate$, grsinv, discamt, freight,       ~
                                                          slstax, netinv
L60510:       FMT POS(521), CH(6), POS(793), PD(14,4),XX(8),4*PD(14,4)
                                  /* BUILLD REASON CODE BUCKETS TOTALS */
            i% = inv_code%
            inv_buf(i%,1%) = round(inv_buf(i%,1%) + 1.0    ,2)
            inv_buf(i%,2%) = round(inv_buf(i%,2%) + grsinv ,2)
            inv_buf(i%,3%) = round(inv_buf(i%,3%) + discamt,2)
            inv_buf(i%,4%) = round(inv_buf(i%,4%) + freight,2)
            inv_buf(i%,5%) = round(inv_buf(i%,5%) + slstax ,2)
            inv_buf(i%,6%) = round(inv_buf(i%,6%) + netinv ,2)
                                  /* BUILD GRAND TOTALS OF EACH COLUMN */
            inv_buf(100%,1%) = round(inv_buf(100%,1%) + 1.0    ,2)
            inv_buf(100%,2%) = round(inv_buf(100%,2%) + grsinv ,2)
            inv_buf(100%,3%) = round(inv_buf(100%,3%) + discamt,2)
            inv_buf(100%,4%) = round(inv_buf(100%,4%) + freight,2)
            inv_buf(100%,5%) = round(inv_buf(100%,5%) + slstax ,2)
            inv_buf(100%,6%) = round(inv_buf(100%,6%) + netinv ,2)
            goto arimastr_loop
        process_done
            gosub format_data
            gosub display_data
        return clear all
        goto inputmode

        display_data
            i% = 1%
L60750:     gosub set_keys
            accept                                                       ~
               at (01,02), "To:",                                        ~
               at (02,02), "Fr:",                                        ~
               at (01,06), fac(hex(84)), beg_date$              , ch(10),~
               at (02,06), fac(hex(84)), end_date$              , ch(10),~
                                                                         ~
               at (01,21), fac(hex(84)), tab_hdr$(1%)           , ch(40),~
               at (02,21), fac(hex(84)), tab_hdr$(2%)           , ch(40),~
               at (03,21), fac(hex(84)), tab_hdr$(3%)           , ch(40),~
                                                                         ~
               at (04,10), "Code  <--------- Description ------>",       ~
               at (05,10), "----  ------------------------------",       ~
                                                                         ~
               at (04,50), "Count    Total Amount ",                     ~
               at (05,50), "-----   --------------",                     ~
                                                                         ~
               at (06,10), fac(hex(84))  , aa$(i% )             , ch(04),~
               at (06,16), fac(hex(84))  , bb$(i% )             , ch(30),~
               at (06,50), fac(hex(84))  , cc$(i% )             , ch(05),~
               at (06,58), fac(hex(84))  , dd$(i% )             , ch(14),~
                                                                         ~
               at (07,10), fac(hex(84))  , aa$(i%+1% )          , ch(04),~
               at (07,16), fac(hex(84))  , bb$(i%+1% )          , ch(30),~
               at (07,50), fac(hex(84))  , cc$(i%+1% )          , ch(05),~
               at (07,58), fac(hex(84))  , dd$(i%+1% )          , ch(14),~
                                                                         ~
               at (08,10), fac(hex(84))  , aa$(i%+2% )          , ch(04),~
               at (08,16), fac(hex(84))  , bb$(i%+2% )          , ch(30),~
               at (08,50), fac(hex(84))  , cc$(i%+2% )          , ch(05),~
               at (08,58), fac(hex(84))  , dd$(i%+2% )          , ch(14),~
                                                                         ~
               at (09,10), fac(hex(84))  , aa$(i%+3% )          , ch(04),~
               at (09,16), fac(hex(84))  , bb$(i%+3% )          , ch(30),~
               at (09,50), fac(hex(84))  , cc$(i%+3% )          , ch(05),~
               at (09,58), fac(hex(84))  , dd$(i%+3% )          , ch(14),~
                                                                         ~
               at (10,10), fac(hex(84))  , aa$(i%+4% )          , ch(04),~
               at (10,16), fac(hex(84))  , bb$(i%+4% )          , ch(30),~
               at (10,50), fac(hex(84))  , cc$(i%+4% )          , ch(05),~
               at (10,58), fac(hex(84))  , dd$(i%+4% )          , ch(14),~
                                                                         ~
               at (11,10), fac(hex(84))  , aa$(i%+5% )          , ch(04),~
               at (11,16), fac(hex(84))  , bb$(i%+5% )          , ch(30),~
               at (11,50), fac(hex(84))  , cc$(i%+5% )          , ch(05),~
               at (11,58), fac(hex(84))  , dd$(i%+5% )          , ch(14),~
                                                                         ~
               at (12,10), fac(hex(84))  , aa$(i%+6% )          , ch(04),~
               at (12,16), fac(hex(84))  , bb$(i%+6% )          , ch(30),~
               at (12,50), fac(hex(84))  , cc$(i%+6% )          , ch(05),~
               at (12,58), fac(hex(84))  , dd$(i%+6% )          , ch(14),~
                                                                         ~
               at (13,10), fac(hex(84))  , aa$(i%+7% )          , ch(04),~
               at (13,16), fac(hex(84))  , bb$(i%+7% )          , ch(30),~
               at (13,50), fac(hex(84))  , cc$(i%+7% )          , ch(05),~
               at (13,58), fac(hex(84))  , dd$(i%+7% )          , ch(14),~
                                                                         ~
               at (14,10), fac(hex(84))  , aa$(i%+8% )          , ch(04),~
               at (14,16), fac(hex(84))  , bb$(i%+8% )          , ch(30),~
               at (14,50), fac(hex(84))  , cc$(i%+8% )          , ch(05),~
               at (14,58), fac(hex(84))  , dd$(i%+8% )          , ch(14),~
                                                                         ~
               at (15,10), fac(hex(84))  , aa$(i%+9% )          , ch(04),~
               at (15,16), fac(hex(84))  , bb$(i%+9% )          , ch(30),~
               at (15,50), fac(hex(84))  , cc$(i%+9% )          , ch(05),~
               at (15,58), fac(hex(84))  , dd$(i%+9% )          , ch(14),~
                                                                         ~
               at (16,10), fac(hex(84))  , aa$(i%+10%)          , ch(04),~
               at (16,16), fac(hex(84))  , bb$(i%+10%)          , ch(30),~
               at (16,50), fac(hex(84))  , cc$(i%+10%)          , ch(05),~
               at (16,58), fac(hex(84))  , dd$(i%+10%)          , ch(14),~
                                                                         ~
               at (17,10), fac(hex(84))  , aa$(i%+11%)          , ch(04),~
               at (17,16), fac(hex(84))  , bb$(i%+11%)          , ch(30),~
               at (17,50), fac(hex(84))  , cc$(i%+11%)          , ch(05),~
               at (17,58), fac(hex(84))  , dd$(i%+11%)          , ch(14),~
                                                                         ~
               at (18,10), fac(hex(84))  , aa$(i%+12%)          , ch(04),~
               at (18,16), fac(hex(84))  , bb$(i%+12%)          , ch(30),~
               at (18,50), fac(hex(84))  , cc$(i%+12%)          , ch(05),~
               at (18,58), fac(hex(84))  , dd$(i%+12%)          , ch(14),~
                                                                         ~
               at (19,10), fac(hex(84))  , aa$(i%+13%)          , ch(04),~
               at (19,16), fac(hex(84))  , bb$(i%+13%)          , ch(30),~
               at (19,50), fac(hex(84))  , cc$(i%+13%)          , ch(05),~
               at (19,58), fac(hex(84))  , dd$(i%+13%)          , ch(14),~
                                                                         ~
               at (20,10), fac(hex(84))  , aa$(100%)            , ch(04),~
               at (20,16), fac(hex(84))  , bb$(100%)            , ch(30),~
               at (20,50), fac(hex(84))  , cc$(100%)            , ch(05),~
               at (20,58), fac(hex(84))  , dd$(100%)            , ch(14),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L61770
                  i% = 1%
                  goto L60750

L61770:        if keyhit% <> 3% then goto L61810
                  i% = 15%
                  goto L60750

L61810:        if keyhit% <> 4% then goto L61850
                  i% = 1%
                  goto L60750

L61850:        if keyhit% <> 5% then goto L61890
                  i% = 15
                  goto L60750

L61890:        if keyhit% <> 15 then goto L61930
                  call "PRNTSCRN"
                  goto L60750

L61930:        close ws
        call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return
        set_keys
            tab_hdr$(1%) = "***************************************"
            tab_hdr$(2%) = "**  Totals for Invoice Reason Codes  **"
            tab_hdr$(3%) = "***************************************"
            pf$(1%) = "(1)Start Over     (4)Previous Page      " &       ~
                      "                                       "
            pf$(2%) = "(2)First Page     (5)Next Page          " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(3)Last Page                            " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(0102030405ffffffffffffffffff0f1000)
            if i% <> 1% then goto L62110
               str(pf$(2%),1%,14%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
               str(pf$(1%),18%,18%) = " " : str(pfkeys$,4%,1%) = hex(ff)
               return
L62110:      str(pf$(2%),18%,14%) = " "  : str(pfkeys$,5%,1%) = hex(ff)
             str(pf$(3%),1%,18%)  = " "  : str(pfkeys$,3%,1%) = hex(ff)
        return

        format_data
            for k% = 1% to  28%
                if k% > 9% then goto L62210
                   convert k% to aa$(k%),pic(#)
                   goto L62230

L62210:         convert k% to aa$(k%),pic(##)

L62230:         chk_cd$ = aa$(k%)
                gosub lookup_reason
                bb$(k%) = inv_code_desc$
                convert inv_buf(k%,1%) to cc$(k%), pic(#####)

                convert inv_buf(k%,6%) to dd$(k%),pic($#,###,###.##-)

            next k%
            aa$(100%) = "  "
            bb$(100%) = "Total of all Reason Codes"
            convert inv_buf(100%,1%) to cc$(100%), pic(#####)

            convert inv_buf(100%,6%) to dd$(100%),pic($#,###,###.##-)

        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            *************************************************************

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
