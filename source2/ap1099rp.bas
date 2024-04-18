        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   AAA   PPPP     1      0     999    999   RRRR   PPPP    *~
            *  A   A  P   P   11     0 0   9   9  9   9  R   R  P   P   *~
            *  AAAAA  PPPP     1    0   0   9999   9999  RRRR   PPPP    *~
            *  A   A  P        1     0 0       9      9  R   R  P       *~
            *  A   A  P      11111    0     999    999   R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * AP1099RP - Print 1099 data sorted either by vendor or by  *~
            *            1099 category code.  Shows details of each     *~
            *            check.                                         *~
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
            * 11/04/88 ! Original                                 ! WPH *~
            * 12/23/92 ! Page 0 Facs, Header, & End Report Time.  ! RJH *~
            * 01/10/96 ! Lengthened key of workfile.              ! JDH *~
            * 09/05/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            address$(5)30,               /* Vendor Address lines       */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cat$4,                       /* 1099 category code         */~
            check$8,                     /* Check Number               */~
            checkline$3,                 /* Check Line item number     */~
            columnttl$51,                /* Column titles line         */~
            checkdate$8,                 /* Date printed on Check      */~
            ten99cat$4,                  /* 1099 category code         */~
            cgross$10,                   /* Gross Amount category total*/~
            cdisc$10,                    /* Disc. Amount category total*/~
            cnet$10,                     /* Net   Amount category total*/~
            company$60,                  /* Company or Division Name   */~
            cursor%(2),                  /* Cursor location for edit   */~
            d$8,                         /* Checkdate for range testing*/~
            currentvend$9,               /*                            */~
            currentcat$4,                /*                            */~
            currentyr$4,                 /*                            */~
            date$8,                      /* Date for screen display    */~
            disc$10,                     /* Discount from Gross        */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            file$8,                      /* Workfile name for READFDR  */~
            fmcategory$4,                /* Range of 1099 Categories   */~
            fmday$10,                    /* Range of Dates             */~
            fmvendor$9,                  /* Range of Vendors           */~
            gross$10,                    /* Gross Check Amount         */~
            ggross$10,                   /* Gross Amount grand    total*/~
            gdisc$10,                    /* Disc. Amount grand    total*/~
            gnet$10,                     /* Net   Amount grand    total*/~
            hicategory$4,                /* Range of 1099 Categories   */~
            hiday$10,                    /* Range of Dates             */~
            hivendor$9,                  /* Range of Vendors           */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            invnumber$16,                /* Vendor Invoice Number      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            lib$8,                       /* workfile lib for READFDR   */~
            line2$79,                    /* Screen Line #2             */~
            locategory$4,                /* Range of 1099 Categories   */~
            loday$10,                    /* Range of Dates             */~
            lovendor$9,                  /* Range of Vendors           */~
            name$30,                     /* Vendor name                */~
            net$10,                      /* Net Check amount           */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            pcat$4,                      /* cat code to print          */~
            pvend$9,                     /* vendor code to print       */~
            pname$30,                    /* vendor name to print       */~
            ptax$12,                     /* vendor tax no. to print    */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            postdate$8,                  /* Check Posting Date         */~
            rpttitle$60,                 /* Report Title               */~
            sort$1,                      /* Sort Option                */~
            sortname$30,                 /* Vendor Sort Name           */~
            taxnmbr$12,                  /* Vendor Tax ID Number       */~
            tdate$10,                    /* Temporary Date Variable    */~
            time$8,                      /* System Time                */~
            timeend$8,                   /* System Time @ End Report   */~
            tocategory$4,                /* Range of 1099 Categories   */~
            today$10,                    /* Range of Dates             */~
            ts$7,                        /* Binary time stamp from file*/~
            tovendor$9,                  /* Range of Vendors           */~
            udate$8,                     /* Unformated Std Temp Date   */~
            userid$3,                    /* Current User Id            */~
            vgross$10,                   /* Gross Amount vendor   total*/~
            vdisc$10,                    /* Disc. Amount vendor   total*/~
            vnet$10,                     /* Net   Amount vendor   total*/~
            vol$6,                       /* workfile vol for READFDR   */~
            vencode$9,                   /* Vendor Code                */~
            year$4,                      /* Year to Report (whole year)*/~
            yr$4                         /* year of check date         */

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
            * #01 ! CSH1099  ! 1099 Report Detail file                  *~
            * #02 ! VENDOR   ! VENDOR MASTER RECORD                     *~
            * #03 ! WORKFILE ! Temporary System Workfile                *~
            * #04 ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "CSH1099",                                       ~
                        varc,     indexed,  recsize = 120,               ~
                        keypos = 7,    keylen = 20,                      ~
                    alternate key 1, keypos =  1 , keylen = 26,          ~
                              key 2, keypos = 27 , keylen = 32

            select #02, "VENDOR",                                        ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   9,                     ~
                    alternate key 1, keypos = 10 ,  keylen = 30

            select #03, "WORKFILE",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos = 1,    keylen = 42

            select #04, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
              /* we open the workfile when & if we need it */
            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            ret% = ret%
            call "COMPNAME" (12%, company$, ret%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."


            str(columnttl$, 1) = "From          "
            str(columnttl$,27) = "To         "

            str(line2$,62) = "AP1099RP: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables
            e% = 0%
            for fieldnr% = 1% to  5%
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
            e% = 1%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       extract_data
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  5% then editpg1
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
        extract_data

           if year$ =  " " then L13090
           loday$, hiday$ = all(hex(00))
           loday$ = str(year$, 1%, 4%) & "0101" /* they want whole year so */
           call "DATECONV" (loday$)
           hiday$ = str(year$, 1%, 4%) & "1231" /* we given em the whole yr*/
           call "DATECONV" (hiday$)

L13090:    if sort$ = "C" then generate_report

*       * We only need to build workfile if sort is by vendor
            r% = 0%
            call "GETNAMES" addr(#1, file$, lib$, vol$)
            call "READFDR" addr(file$, lib$, vol$, 0%, "RC", rec%, ret%)
            call "WORKOPEN" (#3, "IO", rec%, f2%(3)) /* set back IO*/

            tdate$ = loday$: call "DATEFMT" (tdate$, 0%, udate$)
            convert str(udate$, 1%, 4%) to loday%, data goto plow_1099
plow_1099:  plowkey$ = all(hex(00))
            str(plowkey$,1%,2%) = bin(loday%,2%)
            str(plowkey$,3%,4%) = str(locategory$,,)

L13170:     call "PLOWALTS" (#1, plowkey$, 1%, 0%, f1%(1))
               if f1%(1) = 0% then generate_report

            get #1, using L13230, ten99cat$, vencode$, check$, checkline$,~
                invnumber$, ts$,  checkdate$, postdate$, amount, discount

L13230:       FMT XX(2), CH(4), CH(9), CH(8), CH(3), XX(9),              ~
                    CH(16), CH(7),  CH(6), CH(6), PD(14,4), PD(14,4)

            tdate$ = hiday$: call "DATEFMT" (tdate$, 0%, udate$)
            if str(yr$,1%,4%) > str(udate$,1%,4%) then generate_report
            if ten99cat$ > hicategory$ then generate_report

            x% = 0%
            gosub'099 (ten99cat$, vencode$, checkdate$, x%)
            if x% <> 0% then L13170

            write #03 using L14020, vencode$, ten99cat$, checkdate$, ts$, ~
              invnumber$, check$, checkline$, postdate$, amount, discount

L14020:        FMT CH(9), CH(4), CH(6), CH(7), CH(16), CH(8), CH(3),     ~
                 CH(6),    PD(14,4), PD(14,4)

            r% = r% +1%

            goto L13170

        deffn'099(cat$, ven$, d$, x%) /* The filter                     */
                                      /* leave X% = 0% if passes        */

            if d$ >= loday$  then L19070
               x% = 1%
               return

L19070:     if d$ <= hiday$ then L19080
               x% = 2%
               return

L19080:     if cat$ >= locategory$ and cat$ <= hicategory$ then L19110
               x% = 1%
               return

L19110:     if ven$ >= lovendor$ and ven$ <= hivendor$ then return
               x% = 1%
               return


        load_vendor

            call "READ100" (#2, vencode$, f1%(2))
              if f1%(2) = 0% then return
            get #2, using L19600, sortname$, name$, address$(), taxnmbr$
L19600:       FMT XX(9), CH(30), CH(30), 5*CH(30), POS(503), CH(12)

        return


        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            if e% = 1% then return
            on fieldnr% gosub L20100,         /* Year to Report         */~
                              L20200,         /* Range of Dates         */~
                              L20300,         /* Range of Vendors       */~
                              L20400,         /* Range of 1099 Cat      */~
                              L20500          /* Sort Option            */
            return
L20100: REM Def/Enable Year to Report (whole year)   YEAR$
            tdate$ = date: call "DATEFMT" (tdate$, 0%, udate$)
            if year$ = " " then year$  = str(udate$,1%,4%)

            return

L20200: REM Def/Enable Range of Dates
            if year$ = " " then L20205
              enabled% = 0%
              return
L20205:     if fmday$ <> " " and fmday$ <> blankdate$ and ~
               today$ <> " " and today$ <> blankdate$ then return
            tdate$ = date: call "DATEFMT" (tdate$, 0%, udate$)
            fmday$  = str(udate$,1%,4%) & "0101"
            today$  = str(udate$,1%,4%) & "1231"
            call "DATFMTC" (fmday$)
            call "DATFMTC" (today$)
            return

L20300: REM Def/Enable Range of Vendors            FMVENDOR$
            if fmvendor$ = " " then fmvendor$  = "ALL"

            return

L20400: REM Def/Enable Range of 1099 Categories    FMCATEGORY$
            if fmcategory$ = " " then fmcategory$ = "ALL"

            return

L20500: REM Def/Enable Sort Option                   SORT$
            if sort$ = " " then sort$ = "C"

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
         "Enter all 4 digits for year, or blank to select date range",   ~
         "Enter the range of Check Dates to Include on Report or 'ALL' ",~
         "Enter the range of Vendors to include on the report or 'ALL' ",~
         "Enter the range of 1099 Category codes to include or 'ALL'   ",~
         "Enter a 'V' to sort by Vendor or 'C' to sort by Category Code"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      fmcategory$, fmday$,   sort$, fmvendor$,   year$,  ~
                      hicategory$, hiday$,          hivendor$,           ~
                      locategory$, loday$,          lovendor$,           ~
                      tocategory$, today$,          tovendor$

            rpttitle$ = "1099 Report"
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
            if f2%(3) <> 0% then L29638
            call "FILEBGON" (#03)
            f2%(3) = 1%
L29638:     return clear all
            goto inputmode

        REM *************************************************************~
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************
        generate_report
            call "SHOSTAT" ("Generating 1099 Report")
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("A/P12", " ", 0%, 0%)
            if sort$ = "V" then  L30084
               rpttitle$ = rpttitle$ & " Sorted by 1099 Category Code"
               goto L30086
L30084:        rpttitle$ = rpttitle$ & " Sorted by 1099 Vendor Code"

L30086:     call "STRING" addr( "CT", rpttitle$, 60%, rpttitle$)

            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
            pcat$, pname$, ptax$, pvend$ = " "
            vchecks%, cchecks%, gchecks% = 0%
            vgross, cgross, ggross = 0
            vdisc, cdisc, gdisc = 0
            vnet, cnet, gnet = 0

            currentcat$, currentvend$ = " "
            hits%, printed% = 0%

            if sort$ = "V" then read_workfile

*       * Sort by Category can pull em off 1099 file by 1st alt key

            init(hex(00)) plowkey$

            tdate$ = loday$
            call "DATEFMT" (tdate$, 0%, udate$)
            convert str(udate$, 1%, 4%) to loday%, data goto set_plow_key
set_plow_key:
            str(plowkey$,1%,2%) = bin(loday%,2%)
            str(plowkey$,3%,4%) = str(locategory$,,)

L30180:     call "PLOWALTS" (#1, plowkey$, 1%, 0%, f1%(1))
               if f1%(1) <> 0% then L30200
               if printed% = 0% then exit_gracefully
               goto end_report

L30200:     get #1, using L30210, yr%, ten99cat$, vencode$, check$,       ~
                     checkline$,  invnumber$, checkdate$, postdate$,     ~
                    gross, discount
            convert yr% to yr$, pic(0000)
L30210:       FMT BI(2), CH(4), CH(9), CH(8), CH(3), XX(9),              ~
                    CH(16), XX(7),  CH(6), CH(6),                        ~
                    PD(14,4), PD(14,4)

            tdate$ = hiday$
            call "DATEFMT" (tdate$, 0%, udate$)
            if str(yr$,1%,4%) > str(udate$,1%,4%) then end_report

            if ten99cat$ > hicategory$ then end_report
            x% = 0%
            gosub'099 (ten99cat$, vencode$, checkdate$, x%) /* filter  */
              if x% =  0% then L30250       /* X% = 0% = passes filter  */
              if x% <> 0% then L30180       /* X% = 1% = outside range  */


            if currentvend$ = vencode$ then L30252
L30250:     gosub load_vendor

L30252:     net = gross - discount

            pcat$ = ten99cat$  /* set 'em here, we suppress 'em after   */
            pvend$ = vencode$  /* first time we print them to clean up  */
            pname$ = name$     /* report.  then refresh on each new page*/
            ptax$ = taxnmbr$

            call "DATEFMT" (checkdate$)
            call "CONVERT" (gross,    2.2, gross$)
            call "CONVERT" (discount, 2.2, disc$ )
            call "CONVERT" (net,      2.2, net$  )


*       * Print Control Section for sort by category code
            if hits% <> 0% then L30300
                 gosub page_head
                 goto L30420

L30300:     if lcntr% > 50% then gosub page_head

            if currentvend$ = vencode$ then L30390
L30330:        gosub print_vend_totals

            if currentcat$ = ten99cat$ then L30420
               gosub print_cat_totals
               if lcntr% > 50% then gosub page_head
               if lcntr% > 3%  then gosub print_column_header
               goto L30420

L30390:     if currentcat$ <> ten99cat$ then L30330  /* catch rare case */
            pcat$, pvend$, pname$, ptax$ = " "

L30420:     gosub accumulate_vendor_totals
            gosub accumulate_category_totals
            gosub accumulate_grand_totals

            gosub print_line
            hits% = hits% + 1%

            goto L30180

         accumulate_vendor_totals

            vchecks% = vchecks% + 1%
            vgross = vgross + gross
            vdisc = vdisc + discount
            vnet = vnet + net

            return


         accumulate_category_totals

            cchecks% = cchecks% + 1%
            cgross = cgross + gross
            cdisc = cdisc + discount
            cnet = cnet + net

            return

         accumulate_grand_totals

            gchecks% = gchecks% + 1%
            ggross = ggross + gross
            gdisc = gdisc + discount
            gnet = gnet + net
            return

         print_line
            if sort$ = "V" then L31032
            print using L60170, pcat$,     pvend$ , pname$, ptax$,        ~
                               check$, checkline$,  checkdate$,          ~
                               invnumber$,   gross$, disc$, net$
            goto L31040
L31032:     print using L60194, pvend$, pcat$,      pname$, ptax$,        ~
                               check$, checkline$,  checkdate$,          ~
                               invnumber$,   gross$, disc$, net$

L31040:     printed% = 1%
            currentvend$ = vencode$
            currentcat$ = ten99cat$
            currentyr$ = yr$
            lcntr% = lcntr% + 1%
            return

         print_column_header

            print
            if sort$ = "V" then L31243
            print using L60140
            print using L60150
            goto L31250
L31243:     print using L60155
            print using L60159

L31250:     lcntr% = lcntr% + 3%
            return

         print_vend_totals
            call "CONVERT" (vgross, 2.2, vgross$)
            call "CONVERT" (vdisc , 2.2, vdisc$ )
            call "CONVERT" (vnet  , 2.2, vnet$  )

            print using  L60200      /* underline   */
            if sort$ = "V" then L31390
            print using  L60210, currentvend$, currentcat$,  vchecks%,    ~
                          vgross$,    vdisc$, vnet$
            print
            if straddle% = 0% then L31396
              if currentyr$ = yr$ then L31396
              print using L60350, currentyr$
              print
              lcntr% = lcntr% + 2%
            goto L31396

L31390:     print using  L60251, currentvend$,              vchecks%,     ~
                          vgross$,    vdisc$, vnet$
            print
L31396:     lcntr% = lcntr% + 3%
            vchecks% = 0%
            vgross, vdisc, vnet = 0
         return

         print_cat_totals
            call "CONVERT" (cgross, 2.2, cgross$)
            call "CONVERT" (cdisc , 2.2, cdisc$ )
            call "CONVERT" (cnet  , 2.2, cnet$  )

            print using  L60232      /* underline   */
            if sort$ = "V" then L31545
            print using L60240, currentcat$, cchecks%, cgross$, cdisc$,   ~
                                cnet$
            print
            if straddle% = 0% then L31550
              if currentyr$ = yr$ then L31550
              print using L60350, currentyr$
              print
              lcntr% = lcntr% + 2%
            goto L31550

L31545:     print using L60210, currentvend$, currentcat$, cchecks%,      ~
                                cgross$,   cdisc$, cnet$
            print
L31550:     lcntr% = lcntr% + 3%
            cchecks% = 0%
            cgross, cdisc, cnet = 0

         return

         print_grand_totals
            call "CONVERT" (ggross, 2.2, ggross$)
            call "CONVERT" (gdisc , 2.2, gdisc$ )
            call "CONVERT" (gnet  , 2.2, gnet$  )

            print using  L60272      /* underline   */
            print using  L60280,           gchecks%, ggross$, gdisc$, gnet$
            lcntr% = lcntr% + 2%
            gchecks% = 0%
            ggross, gdisc, gnet = 0

         return


        read_workfile  /* sort by vendor  */
            init(hex(00)) plowkey$

L31960:     call "PLOWNEXT" (#3, plowkey$, 0%, f1%(3))
               if f1%(3) <> 0% then L32040
               if printed% = 0% then exit_gracefully
               goto end_report
L32040:     get   #03 using L32100, vencode$, ten99cat$, checkdate$, ts$, ~
              invnumber$, check$, checkline$, postdate$, gross , discount

L32100:        FMT CH(9), CH(4), CH(6), CH(7), CH(16), CH(8), CH(3),     ~
                 CH(6),    PD(14,4), PD(14,4)

            if currentvend$ = vencode$ then L32220
            gosub load_vendor

L32220:     net = gross - discount

            pcat$ = ten99cat$  /* set 'em here, we suppress 'em after   */
            pvend$ = vencode$  /* first time we print them to clean up  */
            pname$ = name$     /* report.  then refresh on each new page*/
            ptax$ = taxnmbr$

            call "DATEFMT" (checkdate$)
            call "CONVERT" (gross,    2.2, gross$)
            call "CONVERT" (discount, 2.2, disc$ )
            call "CONVERT" (net,      2.2, net$  )


*       * Print Control Section for sort by vendor
            if hits% <> 0% then L32580
                 gosub page_head
                 goto L32840

L32580:     if lcntr% > 50% then gosub page_head

            if currentcat$ = ten99cat$ then L32780
L32640:        gosub print_cat_totals

            if currentvend$ = vencode$ then L32840
               gosub print_vend_totals
               if lcntr% > 50% then gosub page_head
               if lcntr% > 3% then gosub print_column_header

               goto L32840

L32780:     if currentvend$ <> vencode$ then L32640  /* catch rare case */
            pcat$, pvend$, pname$, ptax$ = " "

L32840:     gosub accumulate_vendor_totals
            gosub accumulate_category_totals
            gosub accumulate_grand_totals

            gosub print_line
            hits% = hits% + 1%

            goto L31960



        exit_gracefully

           close printer
           if sort$ = "V" then L33190
           call "ASKUSER" ( kh%, "* * * NOTHING TO REPORT * * *",        ~
                            "No records found in the CSH1099 file, or ", ~
                            "the file was not found.",                   ~
                            "Press any key to acknowledge.")

           call "SETPRNT" (" ", " ", 0%, 1%)
           goto inputmode

L33190:    call "ASKUSER" ( kh%, "* * * NOTHING TO REPORT * * *",        ~
                    "No records found which meet selection criteria",    ~
                    "Press any key to return to Edit Mode to change",    ~
                    "the selection criteria or exit via 'Startover'")

           if f2%(3) <> 0% then L33220
             call "FILEBGON" (#3)

L33220:    call "SETPRNT" (" ", " ", 0%, 1%)
           goto editpg1

        end_report                /* Report Ending Routine */
            if printed% = 0% then L33480
               if lcntr% > 50% then gosub page_head
               if sort$ = "V" then L33370

               gosub print_vend_totals
               gosub print_cat_totals
               goto L33380

L33370:        gosub print_cat_totals
               gosub print_vend_totals

L33380:     gosub print_grand_totals
            print skip(2)
            timeend$ = " "  :  call "TIME" (timeend$)
            print using L64990, timeend$    /* End of report line */
            goto L33560

L33480:        print skip(2)
               print using L60320    /* nothing found to report */

L33560:     close printer
            if f2%(3) <> 0% then L33580
               call "FILEBGON" (#3)

L33580:     call "SETPRNT" (" ", " ", 0%, 1%)
            goto inputmode

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            if pcntr% = 0% then gosub print_params
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "AP1099RP"
            print using L60110, rpttitle$, pcntr%
            print
            lcntr% = 3%
            gosub print_column_header
            pcat$ = ten99cat$  /* refresh em here                       */
            pvend$ = vencode$
            pname$ = name$
            ptax$ = taxnmbr$
            return

        print_params           /* Print Page Zero */
            print page
L33965:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L33985
                str(i$(), i%, 1%) = hex(20)
                goto L33965
L33985:     print using L60070, date$, time$, company$, "AP1099RP"
            print using L60110, rpttitle$, pcntr%
            print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            pcntr% = pcntr% + 1%
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

        FMT                 /* FILE: CSH1099                           */~
            CH(2),          /* year of check date                      */~
            CH(4),          /* 1099 Category Code                      */~
            CH(9),          /* Vendor code                             */~
            CH(8),          /* check-no for use as key on buffers      */~
            CH(3),          /* Binary Test                             */~
            CH(9),          /* Vendor code                             */~
            CH(16),         /* Invoice number                          */~
            BI(3),          /* system (clock) date from the computer   */~
            BI(4),          /* Time from the system clock              */~
            CH(6),          /* check date                              */~
            CH(6),          /* Post Date                               */~
            PD(14,4),       /* Quantity of Something                   */~
            PD(14,4),       /* discount amount                         */~
            CH(9),          /* cash in bank account                    */~
            CH(9),          /* Discounts Account                       */~
            CH(9),          /* Expenses Account Number.                */~
            CH(1),          /* Asset G/L Account Code                  */~
            CH(06)          /* Unused Space                            */~

        FMT                 /* FILE: VENDOR                            */~
            CH(9),          /* Vendor Code                             */~
            CH(30),         /* Vendor Description (aka Sort Name)      */~
            CH(30),         /* vendor name                             */~
            5*CH(30),       /* vendor billing address                  */~
            CH(20),         /* contact's name                          */~
            CH(10),         /* vendor phone number                     */~
            CH(9),          /* Purchases Account Number                */~
            CH(9),          /* Payables Account                        */~
            CH(9),          /* cash in bank account                    */~
            CH(9),          /* discounts taken account                 */~
            PD(14,4),       /* bills due (days) (for prox, this < 0)   */~
            PD(14,4),       /* discounts due (days) (for prox. this is */~
            PD(14,4),       /* vendor discount percent                 */~
            PD(14,4),       /* current outstanding balance             */~
            CH(9),          /* Price - Cost Variance G/L Account Code  */~
            CH(87),         /* Unused Space                            */~
            CH(1),          /* P.O. Conformation Flag (Y/N)            */~
            CH(1),          /* Freight Terms                           */~
            CH(30),         /* F.O.B.                                  */~
            CH(1),          /* Taxable Purchase (Y/N)                  */~
            CH(30),         /* ship via free text field                */~
            CH(4),          /* Vendor Type                             */~
            CH(9),          /* Interim Liabilities Account Code        */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(9),          /* Main vendor number.                     */~
            CH(12),         /* Federal Tax Identification Number       */~
            CH(4),          /* 1099 Category Code                      */~
            CH(9),          /* freight account                         */~
            CH(73)          /* Unused Space                            */~


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
              on fieldnr% gosub L40100,         /* Year to Report    */   ~
                                L40095,         /* Range of Dates    */   ~
                                L40095,         /* Range of Vendors  */   ~
                                L40095,         /* Range of 1099 Cat */   ~
                                L40095          /* Sort Option       */
              goto L40110

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40095:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40100:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40110:     accept                                                       ~
               at (01,02),                                               ~
                  "Print 1099 Report",                                   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),   columnttl$           , ch(51),~
                                                                         ~
               at (07,02), "Year to Report (CCYY)      ",                ~
               at (07,30), fac(lfac$( 1)),   year$              , ch(04),~
                                                                         ~
               at (08,02), "Range of Check Dates",                       ~
               at (08,30), fac(lfac$( 2)), fmday$               , ch(10),~
               at (08,56), fac(lfac$( 2)), today$               , ch(10),~
                                                                         ~
               at (09,02), "Range of Vendors",                           ~
               at (09,30), fac(lfac$( 3)), fmvendor$            , ch(09),~
               at (09,56), fac(lfac$( 3)), tovendor$            , ch(09),~
                                                                         ~
               at (10,02), "Range of 1099 Categories",                   ~
               at (10,30), fac(lfac$( 4)), fmcategory$          , ch(04),~
               at (10,56), fac(lfac$( 4)), tocategory$          , ch(04),~
                                                                         ~
               at (11,02), "Sort Option  (V/C)",                         ~
               at (11,30), fac(lfac$( 5)),   sort$              , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40315
                  call "MANUAL" ("AP1099RP") : goto L40110

L40315:        if keyhit% <> 15 then L40330
                  call "PRNTSCRN" : goto L40110

L40330:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40425     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40410
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% > 1% then L40415
L40410:         str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40415:     return

L40425: if fieldnr% > 0% then L40470  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40470:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
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
            on fieldnr% gosub L50300,         /* Year to Report         */~
                              L50720,         /* Range of Dates         */~
                              L51320,         /* Range of Vendors       */~
                              L51580,         /* Range of 1099 Cat      */~
                              L51840          /* Sort Option            */
            return
L50300: REM Test for Year to Report (whole year)  YEAR$
            year% = year%
            if year$ <> " " then L50420
              if e% = 0% then return
              if fmday$ = " " or fmday$ = blankdate$ then L50600
              return
L50420:     if e% = 1% and fmday$ <> " " and fmday$ <> blankdate$ then L50660
            convert year$ to year% , data goto  L50540
            if year% < 1900% then L50540
            loday$ = str(year$,1%,4%) & "0101"
            call "DATECONV" (loday$)
            hiday$ = str(year$,1%,4%) & "1231"
            call "DATECONV" (hiday$)
            return

L50540:     errormsg$ = "Enter all four digits for a year or blank"
            return

L50600:     errormsg$ = "The year and date range cannot both be blank"
            return

L50660:     errormsg$ = "Enter either a year or date range, not both"
            return

L50720: REM Test for Range of Dates               FMDAY$
            straddle% = 0%
               if e% = 1%       and year$  <> " " and ~
                  fmday$ <> " " and fmday$ <> blankdate$ then L50660

            if year$ =  " " then L50790
               fmday$, today$ = " " : return

L50790:     if (fmday$ = " " or fmday$ = blankdate$) and ~
               (today$ = " " or today$ = blankdate$) then L51284
            if fmday$ = "ALL" then  L51220
            if today$ = " " or today$ = blankdate$ then today$ = fmday$
            call "DATEOKC" (fmday$, from%, errormsg$)
               if errormsg$ <> " " then return
            call "DATEOKC" (today$, to%, errormsg$)
               if errormsg$ <> " " then return

            if  from% <= to%   then  L51000
              errormsg$ = "Ending Date Cannot be prior to Starting Date"
              return
L51000:     loday$ = fmday$: call "DATUFMTC" (loday$)
            hiday$ = today$: call "DATUFMTC" (hiday$)

           if str(loday$,1%,3%) = str(hiday$,1%,3%) then return

           call "ASKUSER" (kh%, "   *** ARE YOU SURE?? ***   ",          ~
                          "Your date range straddles two calendar years",~
                          "Press any key to acknowledge", " ")
            straddle% = 1%
            return

L51220:     loday$ = "19010101" : call "DATECONV" (loday$)
            hiday$ = "20991231" : call "DATECONV" (hiday$)
            today$ = " "
            return

L51284:     errormsg$ = "Enter Starting and Ending dates"
            return

L51320: REM Test for Range of Vendors             FMVENDOR$
            if fmvendor$ = "ALL"  then  L51480
            call "TESTRNGE"                                              ~
                  (fmvendor$           , tovendor$           ,           ~
                   lovendor$           , hivendor$           ,           ~
                   errormsg$)
            return

L51480:     tovendor$ = " "
            lovendor$ = all(hex(00))
            hivendor$ = all(hex(ff))
            return

L51580: REM Test for Range of 1099 Categories     FMCATEGORY$
            if fmcategory$ = "ALL"  then  L51740
            call "TESTRNGE"                                              ~
                  (fmcategory$         , tocategory$         ,           ~
                   locategory$         , hicategory$         ,           ~
                   errormsg$)
            return

L51740:     tocategory$ = " "
            locategory$ = all(hex(00))
            hicategory$ = all(hex(ff))
            return

L51840: REM Test for Sort Option                    SORT$
            if sort$ = "V" then    return
            if sort$ = "C" then    return
            errormsg$ = "Enter either a 'V' or a 'C' to indicate the sort~
        ~ sequence for the report"
            return


        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:A/P12

*       * Header Line 2
L60110: %                                     ###########################~
        ~#################################                     PAGE: ####

*       * Header for Sort by Category
L60140: % Cat  Vendor    Vendor Name                    Tax ID No.   Chec~
        ~K     Ln Ck Date  Vendor Invoice    Gross Amt   Discount    Net A~
        ~mt

L60150: % ---- --------- ------------------------------ ------------ ----~
        ~---- --- -------- ---------------- ---------- ---------- --------~
        ~--

*       * Header for Sort by Vendor
L60155: % Vendor    Cat  Vendor Name                    Tax ID No.   Chec~
        ~K     Ln Ck Date  Vendor Invoice    Gross Amt   Discount    Net A~
        ~mt

L60159: % --------- ---- ------------------------------ ------------ ----~
        ~---- --- -------- ---------------- ---------- ---------- --------~
        ~--
*       * Detail Line for sort by Category
L60170: % #### ######### ############################## ############ ####~
        ~#### ### ######## ################ ########## ########## ########~
        ~##

*       * Detail Line for sort by Vendor
L60194: % ######### #### ############################## ############ ####~
        ~#### ### ######## ################ ########## ########## ########~
        ~##
*       * Vendor Totals Line
L60200: %                                               -----------------~
        ~---------------------------------- ---------- ---------- --------~
        ~--

L60210: %                                             * Total Vendor/Cat:~
        ~ #########/####  (checks: #######) ########## ########## ########~
        ~##

*       * Category Totals Line
L60232: %                                               -----------------~
        ~---------------------------------- ---------- ---------- --------~
        ~--

L60240: %                                            ** Total for Categor~
        ~y Code:    ####  (checks: #######) ########## ########## ########~
        ~##

*       * Vendor Totals Line
L60251: %                                            ** Total for Vendor ~
        ~Code: #########  (checks: #######) ########## ########## ########~
        ~##

*       * Grand Totals Line
L60272: %                                               -----------------~
        ~---------------------------------- ---------- ---------- --------~
        ~--

L60280: %                                           *** Grand Totals for ~
        ~Report Range     (checks:########) ########## ########## ########~
        ~##

*       * Nothing to Report Line
L60320: %  *  *  *  * No Data Found meeting Selection criteria *  *  *  *


L60350: %                                 * * * * * * * * * *  End of Li~
        ~sting for year: ####  * * * * * * * * * *
        %** Report Title for page 0
        %############################################################

L64990:       %                            * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   @   ########   * * * * * * * * * *

        REM THISPROGRAMWASGENERATEDBYGENRPPGMAPROPRIETRYPRODUCTOFCAELUS**~
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
            if f2%(3) <> 0% then L65210
            call "FILEBGON" (#3)
L65210:     end
