        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  FFFFF   AAA   RRRR   EEEEE  PPPP    OOO   RRRR   TTTTT   *~
            *  F      A   A  R   R  E      P   P  O   O  R   R    T     *~
            *  FFFF   AAAAA  RRRR   EEEE   PPPP   O   O  RRRR     T     *~
            *  F      A   A  R   R  E      P      O   O  R   R    T     *~
            *  F      A   A  R   R  EEEEE  P       OOO   R   R    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FAREPORT - THIS PROGRAM ALLOWS USER TO SELECT A FIELD TO  *~
            *            SORT BY AND THE VALID RANGE OF FIELD VALUES    *~
            *            TO PRINT ON A STANDARD LISTING FORMAT.         *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/13/88 ! Original                                 ! RJM *~
            * 08/28/89 ! Set totals accumulators to zero.         ! MJB *~
            * 08/07/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            columnttl$53,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            beg$(12)30,                  /* BEGINING RANGE             */~
            end$(12)30,                  /* ENDING RANGE               */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lorange$30,                  /* BEGINING RANGE             */~
            hirange$30,                  /* ENDING RANGE               */~
            order$(12)35,                /* Sort Order Description     */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            record$200,                                                  ~
            rpttitle$60,                 /* Report Title               */~
            tdate$10,                    /* TEMP DATE FOR TESTING      */~
            time$8,                      /* System Time                */~
            userid$3                     /* Current User Id            */~

        dim                                                              ~
            accum_depr(3),               /* ACCUMULATED DEPRECIATION   */~
            accum_depr$12,               /* ACCUMULATED DEPRECIATION   */~
            asset_acct$16,               /* ASSET G/L ACCOUNT #        */~
            asset_code$10,               /* ASSET CODE                 */~
            bonus_depr(3),               /* BONUS DEPRECIATION TAKEN   */~
            bonus_depr$11,               /* BONUS DEPRECIATION TAKEN   */~
            cnv_descr$(4)14,             /* PRORATION CONVENTION DESCR */~
            convention$(3)1,             /* PRORATION CONVENTION       */~
            current_depr(3),             /* CURRENT YEAR'S DEPR        */~
            current_depr$11,             /* CURRENT DEPRECIATION       */~
            depr_acct$16,                /* ACCUM DEPR G/L ACCOUNT #   */~
            depr_category$1,                                             ~
            depr_method$(3)2,            /* DEPRECIATION METHOD        */~
            descr_1$30,                  /* ASSET DESCRIPTION #1       */~
            descr_2$30,                  /* ASSET DESCRIPTION #2       */~
            disposal_date$10,            /* DISPOSAL DATE FORMATTED    */~
            disposal_descr$30,           /* DISPOSAL DESCRIPTION       */~
            exp_acct$16,                 /* EXPENSE G/L ACCOUNT #      */~
            exp_deduct(3),               /* EXPENSE DEDUCTION TAKEN    */~
            exp_deduct$11,               /* EXPENSE DEDUCTION          */~
            group$(3)10,                 /* GROUP TABLE NAME           */~
            id_code$15,                  /* IDENTIFICATION CODE        */~
            in_service$(3)2,             /* PERIOD IN PUT IN SERVICE   */~
            invoice_number$16,           /* INVOICE NUMBER             */~
            itc_reduct(3),               /* ITC REDUCTION              */~
            itc_reduct$11,               /* ITC BASIS REDUCTION        */~
            life$(3)5,                   /* LIFE IN YEARS              */~
            location$30,                 /* LOCATION                   */~
            method_descr$(9)56,          /* DEPRECIATION METHOD DESCR  */~
            method$56,                                                   ~
            orig_basis(3),               /* ORIGINAL BASIS             */~
            orig_basis$12,               /* ORIGINAL BASIS             */~
            other_descr$(3)50,           /* OTHER BASIS REDUCT DESCR   */~
            other_reduct(3),             /* OTHER REDUCTION            */~
            other_reduct$11,             /* OTHER BASIS REDUCTION      */~
            percentage(3),               /* PERCENT FOR MANUAL OR DB   */~
            percentage$6,                /* PERCENTAGE                 */~
            property_type$1,             /* PROPERTY TYPE              */~
            prop$(7)18,                  /* PROPERTY TYPE DESCRIPTION  */~
            property_descr$18,           /* PROPERTY TYPE DESCRIPTION  */~
            purch_date$10,               /* PURCHASE DATE FORMATTED    */~
            salvage_value(3),            /* SALVAGE VALUE              */~
            salvage_value$11,            /* SALVAGE VALUE              */~
            seq$3,                       /* TEXT SEQUENCE NUMBER       */~
            seqnr$3,                     /* TEXT SEQUENCE NUMBER       */~
            service_date$(3)10,          /* DATE 1ST PLACED IN SERVICE */~
            switch_year$(3)4,            /* YEAR OF SWITCH             */~
            txdate$8,                    /* FREE TEXT DATE             */~
            tdescr$55,                   /* FREE TEXT                  */~
            text$(100)75,                /* DESCRIPTIVE TEXT           */~
            text_flag$1,                 /* PRINT TEXT Y OR N          */~
            text_heading$(4)76,          /* HEADING ON DESCRIPTIVE TEXT*/~
            textkey$14,                  /* KEY TO TEXT FILE           */~
            text_type$1,                 /* TEXT TYPE                  */~
            tvendor$9,                   /* FREE TEXT VENDOR           */~
            type_code$1,                 /* TYPE CODE                  */~
            vendor_code$9                /* VENDOR CODE                */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

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
            * #01 ! FAMASTER ! Fixed Assets Master File                 *~
            * #02 ! SYSFILE2 ! Caelus Management System Information     *~
            * #03 ! FATEXT   ! Fixed Assets Descriptive Text File       *~
            * #16 ! WORKFILE ! Workfile to Sort Report                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "FAMASTER",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  120, keylen =  10,                     ~
                        alt key  1, keypos =   58, keylen =   1, dup     ~

            select #02, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select #03, "FATEXT",                                        ~
                        varc,     indexed,  recsize = 125,               ~
                        keypos =  1,    keylen = 14

            select #16, "WORKFILE",                                      ~
                        varc,     indexed,  recsize = 40,                ~
                        keypos =  1,    keylen = 40

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))

            recs% = 500%
            get str(rslt$(1),17, 4) using L02350, recs%, data goto L09000
L02350:         FMT BI(4)

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, err%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "SORTED FIXED ASSETS LISTING  -"

            str(columnttl$, 1) = "Beginning Code"
            str(columnttl$,32) = "Ending Code"

            str(line2$,62) = "FAREPORT: " & str(cms2v$,,8)

        REM READ "SYSFILE2" FOR FISCAL YEAR END DATE
            call "READ100" (#02, "SWITCHS.FA", f1%(2))
                 if f1%(2) = 0% then L65000
            get #02, using L09240, fiscal_begin$, fiscal_end$
L09240:         FMT XX(20), CH(8), CH(8)
            call "DATFMTC" (fiscal_begin$)       /* MM/DD/YYYY */
            call "DATFMTC" (fiscal_end$)         /* MM/DD/YYYY */

            method_descr$(1%) = "STRAIGHT LINE"
            method_descr$(2%) = "STRAIGHT LINE AFTER SWITCH FROM DECLININ~
        ~G BALANCE"
            method_descr$(3%) = "DECLINING BALANCE"
            method_descr$(4%) = "DECLINING BALANCE WITH AUTOMATIC SWITCH ~
        ~TO STRAIGHT LINE"
            method_descr$(5%) = "SUM OF THE YEARS DIGITS"
            method_descr$(6%) = "TABLE METHOD # 1"
            method_descr$(7%) = "TABLE METHOD # 2"
            method_descr$(8%) = "PERCENT"
            method_descr$(9%) = "MANUAL"

            prop$(1%) = "PERSONAL PROPERTY"
            prop$(2%) = "REAL ESTATE"
            prop$(3%) = "LOW INCOME HOUSING"
            prop$(4%) = "LEASED PROPERTY"
            prop$(5%) = "AMORTIZED PROPERTY"
            prop$(6%) = "RESIDENTIAL RENTAL"
            prop$(7%) = "OTHER"

            text_heading$(1%) =      "REPAIRS & MAINTENANCE:  SEQ   DATE ~
        ~     AMOUNT    DESCRIPTION"
            text_heading$(2%) =      "PURCHASE HISTORY:       SEQ   DATE ~
        ~     AMOUNT     VENDOR   DESCRIPTION"
            text_heading$(3%) =      "MARKET VALUE HISTORY:   SEQ   DATE ~
        ~     AMOUNT    DESCRIPTION"
            text_heading$(4%) =      "FREE DESCRIPTIVE TEXT:  SEQ FREE TE~
        ~XT"

            cnv_descr$(1) = "HALF - YEAR"
            cnv_descr$(2) = "MID - QUARTER"
            cnv_descr$(3) = "MID - MONTH"
            cnv_descr$(4) = " "

           order$( 1) = "ASSET CODE"
           order$( 2) = "DESCRIPTION"
           order$( 3) = "IDENTIFICATION CODE"
           order$( 4) = "PURCHASE DATE"
           order$( 5) = "PROPERTY TYPE"
           order$( 6) = "LOCATION"
           order$( 7) = "DISPOSAL DATE"
           order$( 8) = "ASSET G/L ACCOUNT CODE"
           order$( 9) = "ACCUMULATED DEPR G/L ACCOUNT CODE"
           order$(10) = "EXPENSE G/L ACCOUNT CODE"
           order$(11) = "DATE PLACED IN SERVICE"
           order$(12) = "DEPRECIATION METHOD"


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

L10100:     for fieldnr% = 1% to 2%
                gosub'052(fieldnr%)
                      if enabled% = 0% then L10250
L10130:         gosub'100(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         if fieldnr% = 1% then L10100
                         goto L10160
L10210:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
                gosub'150(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
L10250:     next fieldnr%

        REM *************************************************************~
            *       I N P U T   M O D E   SORT ORDER SELECTION SCREEN   *~
            *************************************************************

            init(" ") beg$()
            inpmessage$ = "Place a Non-Blank character next One of the Av~
        ~ailable Sorting Sequences."
L11100:     gosub'101
               if keyhit%  =  1% then gosub startover
               if keyhit% <>  0% then L11100
            errormsg$ = " "
            rsort% = pos(str(beg$()) <> " ")
            if rsort% = 0% then errormsg$ = "One Sorting Selection MUST b~
        ~e made!"
            if errormsg$ <> " " then L11100
            rsort% = int(rsort%/30 + 1%)
            init(" ") beg$(), end$()


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles EDIT MODE for range selection screen.             *~
            *************************************************************

        editpg1
            fieldnr% = rsort%
            gosub'051(fieldnr%)         /* CHECK ENABLES, SET DEFAULTS */
L13170:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L13170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13170
L13220:     gosub'102(0%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       extract_data
                  if keyhit%  =  0% then       editpg1
            goto L13220


        REM *************************************************************~
            *           E X T R A C T   R E P O R T   D A T A           *~
            *-----------------------------------------------------------*~
            * Data Extraction section for report.                       *~
            *************************************************************
        extract_data
            call "SHOSTAT" ("Sorting & Printing Report")
            if rsort% = 1% then generate_report
            plowkey$ = all(hex(00))
            call "PLOWNEXT" (#01, plowkey$, 0%, f1%(1))
                 if f1%(1) = 0% then inputmode     /* ERROR */
            call "WORKOPEN" (#16, "IO   ", recs%, f2%(16))
                 if f2%(16) = 1% then inputmode    /* ERROR */
            goto L15140
L15120:     call "READNEXT" (#01, f1%(1))
               if f1%(1) = 0% then load_done
L15140:     get #01, using L15150, record$
L15150:        FMT CH(190)
            asset_code$ = str(record$, 120, 10)
            junk$ = str(record$, offset%, flen%)
            if junk$ < lorange$ or junk$ > hirange$ then L15120
            if str(record$,190,1) <> "3" then L15120
            write #16, using L15210, junk$, asset_code$
L15210:        FMT CH(30), CH(10)
            goto L15120

        load_done:
            call "WORKOPN2" (#16, "INPUT", 0%, f2%(16%))
               if f2%(16) = 1% then inputmode      /* ERROR */
            goto generate_report

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
           enabled% = 1%
           if beg$(fieldnr%) <> " " and beg$(fieldnr%) <> blankdate$ then return
           beg$(fieldnr%) = "ALL"  : end$(fieldnr%) = " "
           return



        REM *************************************************************~
            *     D E F A U L T / E N A B L E S                         *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES                                 *~
            *************************************************************

        deffn'052(fieldnr%)
              enabled% = 1%
              on fieldnr% gosub L21100,                                   ~
                                L21200
              return

L21100:     REM DEFAULT/ENABLE FOR DEPR CATEGORY
            depr_category$ = "B"
            inpmessage$ = "Valid Categories: 'B'-Book Depr, 'F'-Federal T~
        ~Ax Depr, 'S'-State/Local Depr"
            return

L21200:     REM DEFAULT/ENABLE FOR DESCRIPTIVE TEXT?
            text_flag$ = "N"
            inpmessage$ = "Enter 'Y' to Print Descriptive Text"
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
         "Enter Asset Code Range                                       ",~
         "Enter Description #1 Range                                   ",~
         "Enter Id Code Range                                          ",~
         "Enter Purchase Date Range                                    ",~
         "Enter Property Type Range                                    ",~
         "Enter Location Range                                         ",~
         "Enter Disposal Date Range                                    ",~
         "Enter Asset G/L Account Range                                ",~
         "Enter Accum Depr Account Range                               ",~
         "Enter Expense G/L Account Range                              ",~
         "Enter Service Date Range                                     ",~
         "Enter Depreciation Method Range                              "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      beg$(), end$(), lorange$, hirange$
            no_of_assets% = 0%
            basis_ttl1, salvage_ttl1, itc_ttl1, bonus_ttl1, exp_ttl1,    ~
                        other_ttl1, accum_ttl1, current_ttl1 = 0

            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
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
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************
        generate_report
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("F/A006", " ", 0%, 0%)
            on cat% gosub L31900, L31920, L31940
            pcntr% = 0% : lcntr% = 99% /* Page & Line Counters */
            if lcntr% > 56% then gosub page_head

            if rsort% > 1% then L30135
            plowkey$ = str(lorange$) & hex(00)
L30115:        call "PLOWNEXT" (#01, plowkey$, 0%, f1%(1))
                    if f1%(1) = 0% then end_report
               if str(plowkey$,,10) > str(hirange$,,10) then end_report
               goto L30240

L30135:     plowkey$ = all(hex(00))
            call "PLOWNEXT" (#16, plowkey$, 0%, f1%(16))
                 if f1%(16) = 0% then end_report
            goto L30190
L30170:     call "READNEXT" (#16, f1%(16))
                 if f1%(16) = 0% then end_report
L30190:     get #16, using L30200, plowkey$
L30200:              FMT XX(30), CH(10)
            call "READ100" (#01, plowkey$, f1%(1))
                 if f1%(1) = 0% then L30170

L30240:     get #01, using L35030,                                        ~
                     depr_method$(),                                     ~
                     service_date$(),                                    ~
                     exp_acct$,                                          ~
                     depr_acct$,                                         ~
                     asset_acct$,                                        ~
                     property_type$,                                     ~
                     disposal_date$,                                     ~
                     purch_date$,                                        ~
                     location$,                                          ~
                     id_code$,                                           ~
                     asset_code$,                                        ~
                     descr_1$,                                           ~
                     descr_2$,                                           ~
                     type_code$,                                         ~
                     disposal_descr$,                                    ~
                     vendor_code$,                                       ~
                     invoice_number$,                                    ~
                     group$(),                                           ~
                     life$(),                                            ~
                     convention$(),                                      ~
                     percentage(),                                       ~
                     switch_year$(),                                     ~
                     orig_basis(),                                       ~
                     salvage_value(),                                    ~
                     itc_reduct(),                                       ~
                     bonus_depr(),                                       ~
                     exp_deduct(),                                       ~
                     other_reduct(),                                     ~
                     other_descr$(),                                     ~
                     accum_depr(),                                       ~
                     current_depr(),                                     ~
                     in_service$()

            if type_code$ <> "3" then L30760
            call "CONVERT" (percentage(cat%),    2.2, percentage$)
            call "CONVERT" (orig_basis(cat%),    2.2, orig_basis$)
            call "CONVERT" (salvage_value(cat%), 2.2, salvage_value$)
            call "CONVERT" (itc_reduct(cat%),    2.2, itc_reduct$)
            call "CONVERT" (bonus_depr(cat%),    2.2, bonus_depr$)
            call "CONVERT" (exp_deduct(cat%),    2.2, exp_deduct$)
            call "CONVERT" (other_reduct(cat%),  2.2, other_reduct$)
            call "CONVERT" (accum_depr(cat%),    2.2, accum_depr$)
            call "CONVERT" (current_depr(cat%),  2.2, current_depr$)

            p% = pos("PRLAEXO"=property_type$)
            property_descr$ = prop$(p%)
            gosub L36000
            if text_flag$ = "Y" then gosub L37000

L30760:     if rsort% = 1% then L30115  else L30170


L31900:     str(rpttitle$, 34) = "BOOK DEPRECIATION"
            return
L31920:     str(rpttitle$, 34) = "FEDERAL DEPRECIATION"
            return
L31940:     str(rpttitle$, 34) = "STATE/LOCAL DEPRECIATION"
            return


        end_report                /* Report Ending Routine */
            if no_of_assets% = 0% then L33915
            print using L60530
            if line% > 57% then gosub page_head
            print using L60750,                                           ~
                               basis_ttl1, salvage_ttl1, itc_ttl1,       ~
                               bonus_ttl1, exp_ttl1, other_ttl1,         ~
                               accum_ttl1, current_ttl1

            print using L60560
            print skip(1)
            print using L60600, no_of_assets%
L33915:     print skip(2)
            print using L64990     /* End of report line */
            close printer
            call "SETPRNT" ("F/A006", " ", 0%, 1%)
            if rsort% > 1% then call "FILEBGON" (#16)
            goto inputmode

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "FAREPORT"
            print using L60110, userid$, rpttitle$, pcntr%
            print
            print using L60210, fiscal_end$
            print using L60220, order$(rsort%)
            print using L60230, beg$(rsort%), end$(rsort%)
            print
            print using L60250
            print using L60280
            print using L60320
            line% = 9%
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: FAMASTER                          */~
            3*CH(2),        /* Depreciation Method                     */~
            3*CH(8),        /* Date Placed In Service                  */~
            CH(9),          /* Depreciation Expense G/L Account Code   */~
            CH(9),          /* Accumulated Depreciation G/L Account Cod*/~
            CH(9),          /* Asset G/L Account Code                  */~
            CH(1),          /* Property Type                           */~
            CH(8),          /* Disposal Date                           */~
            CH(8),          /* Purchase Date                           */~
            CH(30),         /* Location of fixed asset                 */~
            CH(15),         /* Identification Code                     */~
            CH(10),         /* Asset Code                              */~
            CH(30),         /* Fixed Asset Description Line #1         */~
            CH(30),         /* Fixed Asset Description Line #2         */~
            CH(1),          /* Fixed Asset Record Type Code            */~
            XX(8),          /* Purchase Price                          */~
            XX(8),          /* Investment Tax Credit Taken             */~
            XX(8),          /* Disposal Price                          */~
            CH(30),         /* Disposal Description                    */~
            CH(9),          /* Vendor Code                             */~
            CH(16),         /* Invoice Number                          */~
            3*CH(10),       /* General Group Name for categorizing tabl*/~
            3*CH(5),        /* Asset Life (Years)                      */~
            3*CH(1),        /* PRORATION CONVENTION                    */~
            3*PD(14,4),     /* Percent for Declining Balance or Percent*/~
            3*CH(4),        /* Year depr method was switch to straight */~
            3*PD(14,4),     /* Original Basis                          */~
            3*PD(14,4),     /* Salvage Value                           */~
            3*PD(14,4),     /* ITC Basis Reduction                     */~
            3*PD(14,4),     /* Bonus Depreciation Taken                */~
            3*PD(14,4),     /* Expense Deduction Taken                 */~
            3*PD(14,4),     /* Other Basis Reduction                   */~
            3*CH(50),       /* Other Basis Reduction Description       */~
            3*PD(14,4),     /* Accumulated Depreciation                */~
            3*PD(14,4),     /* Current Depreciation                    */~
            3*CH(2)         /* In service period.                      */


        FMT                 /* FILE: FATEXT                            */~
            CH(10),         /* Asset Code                              */~
            CH(1),          /* Fixed Assets Descriptive Text Type      */~
            CH(3),          /* Descriptive Text Sequence Number        */~
            CH(75),         /* Descriptive Text String                 */~
            CH(36)          /* Unused Space                            */~



L36000: REM *************************************************************~
            *        PRINT THE ASSET AND ACCUMULATED TOTALS             *~
            *                                                           *~
            *************************************************************

            call "DATFMTC" (service_date$(cat%))
            call "DATFMTC" (disposal_date$)
            call "DATFMTC" (purch_date$)

            method$ = " "
            convert depr_method$(cat%) to method%, data goto L36200
            if method% > 0% and method% < 10% then                       ~
                 method$ = method_descr$(method%)
            c% = 4%
            if convention$(cat%) = " " then L36170
            convert convention$(cat%) to c%, data goto L36170

L36170
*          DISPOSAL_DESC$ = " "
*          IF DISPOSAL_DATE$ <> " " THEN DISPOSAL_DESC$ =               ~
*              "**** RETIRED ****"
L36200:     if line% + 8% > 57% then gosub page_head

            print using L60360, asset_code$, service_date$(cat%),         ~
                               life$(cat%), orig_basis$, salvage_value$, ~
                               itc_reduct$, bonus_depr$, exp_deduct$,    ~
                               other_reduct$, accum_depr$, current_depr$

            print using L60420, descr_1$
            print using L60390, descr_2$, other_descr$(cat%)
            call "GLFMT" (asset_acct$)
            call "GLFMT" (depr_acct$)
            call "GLFMT" (exp_acct$)
            print using L60440, property_type$, property_descr$,          ~
                               purch_date$, asset_acct$
            print using L60460, depr_method$(cat%), method$,              ~
                               depr_acct$
            print using L60490, percentage(cat%), id_code$, exp_acct$
            if method% < 6% or method% > 7% then L36450
               print using L60620, group$(cat%), life$(cat%),             ~
                                  convention$(cat%), cnv_descr$(c%),     ~
                                  in_service$(cat%)
               line% = line% + 1%

L36450:     print using L60510, disposal_date$, disposal_descr$, location$

            print skip(2)
            line% = line% + 9%
            if line% > 57% then gosub page_head

            basis_ttl1   = basis_ttl1   + orig_basis(cat%)
            salvage_ttl1 = salvage_ttl1 + salvage_value(cat%)
            itc_ttl1     = itc_ttl1     + itc_reduct(cat%)
            bonus_ttl1   = bonus_ttl1   + bonus_depr(cat%)
            exp_ttl1     = exp_ttl1     + exp_deduct(cat%)
            other_ttl1   = other_ttl1   + other_reduct(cat%)
            accum_ttl1   = accum_ttl1   + accum_depr(cat%)
            current_ttl1 = current_ttl1 + current_depr(cat%)

            no_of_assets% = no_of_assets% + 1%
            return

L37000: REM *************************************************************~
            *                PRINT THE DESCRIPTIVE TEXT                 *~
            *************************************************************

            init(" ") text$()
            textkey$ = str(asset_code$,1,10) & hex(00)
            call "READ104" (#03, textkey$, f1%(7))
            if f1%(7) = 0 then return
            get #03, using L37090, asset$, old_type$
L37090:     FMT CH(10), CH(1)
            if asset$ <> asset_code$ then return

L37120:     call "PLOWNEXT" (#03, textkey$, 10%, f1%(7))
            if f1%(7) <> 0 then L37170
            if temp% = 0% then return else gosub L37260
            return

L37170:     get #03, using L37180, text_type$, seqnr$
L37180:     FMT XX(10), CH(1), CH(3), XX(75)

            if text_type$ <> old_type$ then gosub L37260
            convert seqnr$ to temp%, data goto L37120
            get #03, using L37230, text$(temp%)
L37230:     FMT XX(10), XX(1), XX(3), CH(75)
            goto L37120

L37260:     convert old_type$ to type%, data goto L37330
            if line% > 54% then gosub page_head
            print using L60790, text_heading$(type%)
            line% = line% + 1%

            on type% gosub L37350, L37490, L37640, L37780
            old_type$ = text_type$
L37330:     return

L37350:     for t% = 1% to temp%             /* REPAIRS AND MAINTENANCE*/
                convert t% to seq$, pic(###)
                if line% > 55% then gosub page_head
                txdate$  = str(text$(t%),1,8)
                amount$ = str(text$(t%),9,12)
                tdescr$ = str(text$(t%),21,55)
                print using L60820, seq$, txdate$, amount$, tdescr$
                line% = line% + 1%
            next t%
            print
            line% = line% + 1%
            init(" ") text$()
            return

L37490:     for t% = 1% to temp%             /* PURCHASE HISTORY */
                convert t% to seq$, pic(###)
                if line% > 55% then gosub page_head
                txdate$   = str(text$(t%),1,8)
                amount$  = str(text$(t%),9,12)
                tvendor$ = str(text$(t%),21,9)
                tdescr$  = str(text$(t%),30,46)
                print using L60850, seq$,txdate$,amount$,tvendor$,tdescr$
                line% = line% + 1%
            next t%
            print
            line% = line% + 1%
            init(" ") text$()
            return

L37640:     for t% = 1% to temp%              /* MARKET VALUE HISTORY */
                convert t% to seq$, pic(###)
                if line% > 55% then gosub page_head
                txdate$  = str(text$(t%),1,8)
                amount$ = str(text$(t%),9,12)
                tdescr$ = str(text$(t%),21,55)
                print using L60880, seq$, txdate$, amount$, tdescr$
                line% = line% + 1%
            next t%
            print
            line% = line% + 1%
            init(" ") text$()
            return

L37780:     for t% = 1% to temp%                   /* FREE TEXT */
                convert t% to seq$, pic(###)
                if line% > 55% then gosub page_head
                print using L60910, seq$, text$(t%)
                line% = line% + 1%
            next t%
            print
            line% = line% + 1%
            init(" ") text$()
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input Selection                                  *~
            *************************************************************

        deffn'101

L40145:     accept                                                       ~
               at (01,02),                                               ~
                  "Report Selection",                                    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "PUT IN ORDER BY:",                           ~
                                                                         ~
               at (07,02), "Asset Code",                                 ~
               at (07,20), fac(hex(81)), beg$(1)                , ch(01),~
                                                                         ~
               at (08,02), "Description #1",                             ~
               at (08,20), fac(hex(81)), beg$(2)                , ch(01),~
                                                                         ~
               at (09,02), "ID Code",                                    ~
               at (09,20), fac(hex(81)), beg$(3)                , ch(01),~
                                                                         ~
               at (10,02), "Purchase Date",                              ~
               at (10,20), fac(hex(81)), beg$(4)                , ch(01),~
                                                                         ~
               at (11,02), "Property Type",                              ~
               at (11,20), fac(hex(81)), beg$(5)                , ch(01),~
                                                                         ~
               at (12,02), "Location",                                   ~
               at (12,20), fac(hex(81)), beg$(6)                , ch(01),~
                                                                         ~
               at (13,02), "Disposal Date",                              ~
               at (13,20), fac(hex(81)), beg$(7)                , ch(01),~
                                                                         ~
               at (14,02), "Asset G/L Acct",                             ~
               at (14,20), fac(hex(81)), beg$(8)                , ch(01),~
                                                                         ~
               at (15,02), "Accum Depr Acct",                            ~
               at (15,20), fac(hex(81)), beg$(9)                , ch(01),~
                                                                         ~
               at (16,02), "Expense G/L Acct",                           ~
               at (16,20), fac(hex(81)), beg$(10)               , ch(01),~
                                                                         ~
               at (17,02), "Service Date",                               ~
               at (17,20), fac(hex(81)), beg$(11)               , ch(01),~
                                                                         ~
               at (18,02), "Depr Method",                                ~
               at (18,20), fac(hex(81)), beg$(12)               , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40490
                  call "MANUAL" ("FAREPORT") : goto L40145

L40490:        if keyhit% <> 15 then L40505
                  call "PRNTSCRN" : goto L40145

L40505:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'050(fieldnr%)
              gosub set_pf1
              init(hex(8c)) lfac$()
              lfac$(rsort%) = hex(86)
              on fieldnr% gosub L41260,         /* ASSET CODE        */   ~
                                L41260,         /* DESCRIPTION       */   ~
                                L41260,         /* ID CODE           */   ~
                                L41260,         /* PURCHASE DATE     */   ~
                                L41260,         /* PROPERTY TYPE     */   ~
                                L41260,         /* LOCATION          */   ~
                                L41260,         /* DISPOSAL DATE     */   ~
                                L41260,         /* ASSET G/L ACCOUNT */   ~
                                L41260,         /* ACCUM DEPR ACCOUN */   ~
                                L41260,         /* EXPENSE G/L ACCOU */   ~
                                L41260,         /* SERVICE DATE      */   ~
                                L41260          /* DEPRECIATION METH */
              goto L41290

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L41260:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L41290:     accept                                                       ~
               at (01,02),                                               ~
                  "Input Report Selection Criteria",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,20), fac(hex(ac)),   columnttl$           , ch(53),~
                                                                         ~
               at (07,02), "ASSET CODE",                                 ~
               at (07,20), fac(lfac$( 1)), beg$(1)              , ch(10),~
               at (07,51), fac(lfac$( 1)), end$(1)              , ch(10),~
                                                                         ~
               at (08,02), "DESCRIPTION #1",                             ~
               at (08,20), fac(lfac$( 2)), beg$(2)              , ch(25),~
               at (08,51), fac(lfac$( 2)), end$(2)              , ch(25),~
                                                                         ~
               at (09,02), "ID CODE",                                    ~
               at (09,20), fac(lfac$( 3)), beg$(3)              , ch(15),~
               at (09,51), fac(lfac$( 3)), end$(3)              , ch(15),~
                                                                         ~
               at (10,02), "PURCHASE DATE",                              ~
               at (10,20), fac(lfac$( 4)), beg$(4)              , ch(10),~
               at (10,51), fac(lfac$( 4)), end$(4)              , ch(10),~
                                                                         ~
               at (11,02), "PROPERTY TYPE",                              ~
               at (11,20), fac(lfac$( 5)), beg$(5)              , ch(04),~
               at (11,51), fac(lfac$( 5)), end$(5)              , ch(04),~
                                                                         ~
               at (12,02), "LOCATION",                                   ~
               at (12,20), fac(lfac$( 6)), beg$(6)              , ch(25),~
               at (12,51), fac(lfac$( 6)), end$(6)              , ch(25),~
                                                                         ~
               at (13,02), "DISPOSAL DATE",                              ~
               at (13,20), fac(lfac$( 7)), beg$(7)              , ch(10),~
               at (13,51), fac(lfac$( 7)), end$(7)              , ch(10),~
                                                                         ~
               at (14,02), "ASSET G/L ACCT",                             ~
               at (14,20), fac(lfac$( 8)), beg$(8)              , ch(12),~
               at (14,51), fac(lfac$( 8)), end$(8)              , ch(12),~
                                                                         ~
               at (15,02), "ACCUM DEPR ACCT",                            ~
               at (15,20), fac(lfac$( 9)), beg$(9)              , ch(12),~
               at (15,51), fac(lfac$( 9)), end$(9)              , ch(12),~
                                                                         ~
               at (16,02), "EXPENSE G/L ACCT",                           ~
               at (16,20), fac(lfac$(10)), beg$(10)             , ch(12),~
               at (16,51), fac(lfac$(10)), end$(10)             , ch(12),~
                                                                         ~
               at (17,02), "SERVICE DATE",                               ~
               at (17,20), fac(lfac$(11)), beg$(11)             , ch(10),~
               at (17,51), fac(lfac$(11)), end$(11)             , ch(10),~
                                                                         ~
               at (18,02), "DEPR METHOD",                                ~
               at (18,20), fac(lfac$(12)), beg$(12)             , ch(04),~
               at (18,51), fac(lfac$(12)), end$(12)             , ch(04),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L41970
                  call "MANUAL" ("FAREPORT") : goto L41290

L41970:        if keyhit% <> 15 then L42000
                  call "PRNTSCRN" : goto L41290

L42000:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L42190     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L42160
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% > 1% then L42170
L42160:         str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L42170:     return

L42190: if fieldnr% > 0% then L42280  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L42280:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input Selection                                  *~
            *************************************************************

        deffn'100(fieldnr%, edit%)

*            GOSUB'050(FIELDNR%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L43180,         /* Depr Category     */   ~
                                L43180          /* Include Text ?    */
              goto L43300

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L43180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L43300:     accept                                                       ~
               at (01,02),                                               ~
                  "Report Category Selection",                           ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Depreciation Category",                      ~
               at (06,30), fac(lfac$( 1)), depr_category$       , ch(01),~
                                                                         ~
               at (07,02), "Print Descriptive Text?",                    ~
               at (07,30), fac(lfac$( 2)), text_flag$           , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L43540
                  call "MANUAL" ("FAREPORT") : goto L43300

L43540:        if keyhit% <> 15 then L43570
                  call "PRNTSCRN" : goto L43300

L43570:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* ASSET CODE             */~
                              L50150,         /* DESCRIPTION            */~
                              L50200,         /* ID CODE                */~
                              L50250,         /* PURCHASE DATE          */~
                              L50300,         /* PROPERTY TYPE          */~
                              L50350,         /* LOCATION               */~
                              L50400,         /* DISPOSAL DATE          */~
                              L50450,         /* ASSET G/L ACCOUNT      */~
                              L50500,         /* ACCUM DEPR ACCOUN      */~
                              L50550,         /* EXPENSE G/L ACCOU      */~
                              L50600,         /* SERVICE DATE           */~
                              L50650          /* DEPRECIATION METH      */
            return
L50100: REM Test for ASSET CODE
            call "TESTRNGE" (beg$(1), end$(1), lorange$, hirange$,       ~
                             errormsg$)
            return

L50150: REM Test for DESCRIPTION #1
            call "TESTRNGE" (beg$(2), end$(2), lorange$, hirange$,       ~
                             errormsg$)
            offset% = 130% :  flen% = 30%
            return

L50200: REM Test for ID CODE
            call "TESTRNGE" (beg$(3), end$(3), lorange$, hirange$,       ~
                             errormsg$)
            offset% = 105% :  flen% = 15%
            return

L50250: REM Test for PURCHASE DATE
            offset% =  67% :  flen% =  8%
            gosub L52000
            return

L50300: REM Test for PROPERTY TYPE
            call "TESTRNGE" (beg$(5), end$(5), lorange$, hirange$,       ~
                             errormsg$)
            offset% =  58% :  flen% =  1%
            return

L50350: REM Test for LOCATION
            call "TESTRNGE" (beg$(6), end$(6), lorange$, hirange$,       ~
                             errormsg$)
            offset% =  75% :  flen% = 30%
            return

L50400: REM Test for DISPOSAL DATE
            offset% =  59% :  flen% =  8%
            gosub L52000
            return

L50450: REM Test for ASSET G/L ACCOUNT
            call "TESTRNGE" (beg$(8), end$(8), lorange$, hirange$,       ~
                             errormsg$)
            offset% =  49% :  flen% =  9%
            return

L50500: REM Test for ACCUM DEPR ACCOUNT
            call "TESTRNGE" (beg$(9), end$(9), lorange$, hirange$,       ~
                             errormsg$)
            offset% =  40% :  flen% =  9%
            return

L50550: REM Test for EXPENSE G/L ACCOUNT
            call "TESTRNGE" (beg$(10), end$(10), lorange$, hirange$,     ~
                             errormsg$)
            offset% =  31% :  flen% =  9%
            return

L50600: REM Test for SERVICE DATE
            offset% =  7% + (cat%-1%)*8% :  flen% =  8%
            gosub L52000
            return

L50650: REM Test for DEPRECIATION METHOD
            call "TESTRNGE" (beg$(12), end$(12), lorange$, hirange$,     ~
                             errormsg$)
            offset% =  1% + (cat%-1%)*2% :  flen% =  2%
            return

        REM *************************************************************~
            *                                                           *~
            *************************************************************~

        deffn'150(fieldnr%)
            errormsg$ = " "
                               on fieldnr% goto L51100, L51200
            return


L51100:     cat% = pos("BFS"=depr_category$)
            if cat% <> 0% then L51150
            errormsg$ = "VALID CATEGORIES: 'B'-BOOK DEPR, 'F'-FEDERAL TAX~
        ~ DEPR, 'S'-STATE/LOCAL DEPR"
            return
L51150:     if cat% = 1% then category_descr$ = "BOOK DEPRECIATION"
                    if cat% = 2% then category_descr$ = "FEDERAL TAX DEPR~
        ~EPRECIATION"
                    if cat% = 3% then category_descr$ = "STATE/LOCAL TAX ~
        ~DEPRECIATION"
L51200:
            return


L52000: REM     TEST DATE RANGE
            if beg$(fieldnr%) = "ALL" then L52170
               tdate$ = beg$(fieldnr%)
               call "DATEOKC" (tdate$, err%, errormsg$)
                     if errormsg$ <> " " then return
               beg$(fieldnr%) = tdate$
               call "DATUFMTC" (tdate$)
               lorange$ = tdate$
            if end$(fieldnr%) = " " or end$(fieldnr%) = blankdate$ ~
                                    or end$(fieldnr%) = "ALL" then L52190
               tdate$ = end$(fieldnr%)
               call "DATEOKC" (tdate$, err%, errormsg$)
                     if errormsg$ <> " " then return
               end$(fieldnr%) = tdate$
               call "DATUFMTC" (tdate$)
               hirange$ = tdate$
            if lorange$ > hirange$ then errormsg$ = "Invalid Date Range"
            return
L52170:     beg$(fieldnr%) = "ALL"
            lorange$ = all(hex(00))
L52190:     end$(fieldnr%) = " "
            hirange$ = all(hex(ff))
            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ######## @ ########              ###########################~
        ~#################################                 ########: F/A00~
        ~6
*       * Header Line 2
L60110: %BY: ###                              ###########################~
        ~#################################                     PAGE:   ###~
        ~#
L60210: %FISCAL YEAR ENDING ##########
L60220: %IN ORDER BY: ###################################
L60230: %RANGE: ############################## TO #######################~
        ~#######
L60250: %ASSET CODE/ DATE PLACED             ORIGINAL      SALVAGE   ITC ~
        ~BASIS  BONUS DEPR   EXPENSE   OTHER BASIS    ACCUMULATED   CURREN~
        ~T
L60280: %DESCRIPTION IN SERVICE  LIFE         BASIS         VALUE    REDU~
        ~CTION    TAKEN      DEDUCTION   REDUCTION       DEPR     YEAR'S D~
        ~EPR

L60320: %----------  ---------- -------- ------------  ----------- ------~
        ~----- ----------- ----------- -----------   ------------ --------~
        ~---

L60360: %##########  ########## #####Yrs ############  ########### ######~
        ~##### ########### ########### ###########   ############ ########~
        ~###
L60390: %     ##############################            OTHER BASIS REDUC~
        ~TION DESCRIPTION: ###############################################~
        ~##
L60420: %     ##############################

L60440: %     PROPERTY TYPE: #   ##################      PURCHASE DATE: #~
        ~#########                       ASSET G/L ACCOUNT  : ############
L60460: %     DEPR METHOD  : ##  ########################################~
        ~################                ACCUM DEPR ACCOUNT : ############

L60490: %     PERCENTAGE   : ###.##                IDENTIFICATION CODE: #~
        ~##############                  EXPENSE G/L ACCOUNT: ############
L60510: %     DISPOSAL DATE: ##########  #################    LOCATION: #~
        ~#############################
L60530: %                                ------------ ------------ ------~
        ~----- ----------- ----------- ----------- -------------- --------~
        ~---
L60560: %                               ============= ============ ======~
        ~===== =========== =========== =========== ============== ========~
        ~===

L60600: %NUMBER OF ASSETS PRINTED:  ######

L60620: %DEPRECIATION TABLE: GROUP: ##########                RECOVERY: #~
        ~####   CONVENTION: # (##############)        PERIOD: ##


L60750: %     GRAND TOTAL:              ##########.## #########.## ######~
        ~##.## ########.## ########.## ########.## ###########.## ########~
        ~.##

L60790: %     ###########################################################~
        ~#################

L60820: %                             ### ######## ############ #########~
        ~##############################################

L60850: %                             ### ######## ############  ########~
        ~# ##############################################

L60880: %                             ### ######## ############  ########~
        ~##############################################

L60910: %                             ### ###############################~
        ~############################################


        %** Report Title for page 0
        %############################################################

L64990: %                                  * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   * * * * * * * * * *

L65000: REM THISPROGRAMWASGENERATEDBYGENRPPGMAPROPRIETRYPRODUCTOFCAELUS**~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
