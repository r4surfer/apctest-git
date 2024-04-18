        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  V   V  EEEEE  N   N  L       AAA   BBBB   EEEEE  L       *~
            *  V   V  E      NN  N  L      A   A  B   B  E      L       *~
            *  V   V  EEEE   N N N  L      AAAAA  BBBB   EEEE   L       *~
            *   V V   E      N  NN  L      A   A  B   B  E      L       *~
            *    V    EEEEE  N   N  LLLLL  A   A  BBBB   EEEEE  LLLLL   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VENLABEL - Prints vendor labels based on vendor code,     *~
            *            vendor type, and zip code.  Sorts on any of    *~
            *            these fields.  Various label sizes can be      *~
            *            specified.                                     *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/06/94 ! Original                                 ! JDH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            address$(6)31,               /* Vendor Name & Address      */~
            bf_code$6,                   /* Buy From Code              */~
            buy_from$1,                  /* Include Buy Froms?         */~
            columnttl$51,                /* Column titles line         */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fmvencode$9,                 /* Vendor Code                */~
            fmventype$4,                 /* Vendor Type                */~
            fmzip$9,                     /* Zip Code                   */~
            hivencode$9,                 /* Vendor Code                */~
            hiventype$4,                 /* Vendor Type                */~
            hizip$9,                     /* Zip Code                   */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            line_up_characters$35,       /* For Line Up Test           */~
            lovencode$9,                 /* Vendor Code                */~
            loventype$4,                 /* Vendor Type                */~
            lozip$9,                     /* Zip Code                   */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            print_a$(6,3)31,             /* Print array                */~
            rptid$6,                     /* Report to Print            */~
            rpttitle$30,                 /* Report Title               */~
            size$1,                      /* Label Size (1, 2, or 3)    */~
            sort_by$1,                   /* Sort By (C, T, or Z)       */~
            temp$4,                      /* Temporary Zip Code Variable*/~
            tovencode$9,                 /* Vendor Code                */~
            toventype$4,                 /* Vendor Type                */~
            tozip$9,                     /* Zip Code                   */~
            type$4,                      /* Vendor Type                */~
            userid$3,                    /* Current User Id            */~
            vencode$9,                   /* Vendor Code                */~
            ventype$4,                   /* Vendor Type                */~
            zip$10                       /* Zip Code Formatted         */

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
            * #01 ! VENDOR   ! VENDOR MASTER RECORD                     *~
            * #02 ! WORKFILE ! Temporary System Workfile                *~
            * #03 ! VENDORBF ! Vendor Buy From File                     *~
            * #05 ! GENCODES ! General Codes File                       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "VENDOR",                                        ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup

            select #02, "WORKFILE",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos = 1,    keylen = 15,                      ~
                        alt key  1, keypos =   16, keylen =  30, dup,    ~
                            key  2, keypos =  226, keylen =  34, dup,    ~
                            key  3, keypos =  187, keylen =  39, dup

            select #03, "VENDORBF",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                               keypos =    1, keylen =  15

            select #05, "GENCODES", varc, indexed, recsize = 128,        ~
                        keypos = 1, keylen = 24

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(1%), f2%(1%), 0%, rslt$(1%))
                recs% = val(str(rslt$(1%),17%,4%),4%) / 2%
            call "OPENCHCK" (#03, fs%(3%), f2%(3%), 0%, rslt$(3%))
            call "OPENCHCK" (#05, fs%(5%), f2%(5%), 0%, rslt$(5%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "Vendor Label Printing"
            rptid$    = "VEN004"
            line_up_characters$ = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
            str(columnttl$, 1%) = "Beginning Code"
            str(columnttl$,27%) = "Ending Code"

            str(line2$,62%) = "VENLABEL: " & str(cms2v$,,8%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  6%
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
                  if keyhit%  = 14% then       line_up_test
                  if keyhit%  = 16% then       extract_data
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  6% then editpg1
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
            call "SHOSTAT" ("Gathering Data...")
            count% = 0%
            f2%(2%) = 1%  /* Workfile Not Open */
            plowkey$ = lovencode$
            call "READ102" (#01, plowkey$, f1%(1%))
                if f1%(1%) = 0% then nothing_to_print
            call "WORKOPEN" (#02, "IO   ",  recs%, f2%(2%))
            goto L13140
        plow_loop
            call "READNEXT" (#01, f1%(1%))
                if f1%(1%) = 0% then end_of_data
L13140:     get #01 using L13150, vencode$, address$(), ventype$
L13150:         FMT CH(9), POS(40), 6*CH(30), POS(477), CH(4)
            if vencode$ > hivencode$ then end_of_data
            if ventype$ <= loventype$ or ventype$ > hiventype$ then      ~
                                                                plow_loop
            zip$ = str(address$(6%),22%,9%)
            if zip$ <= lozip$ or zip$ > hizip$ then zip_exit
            bf_code$ = " "
            gosub write_workfile
            if buy_from$ = "Y" then gosub buy_froms
            goto plow_loop

        zip_exit
*        Just because the vendor's zip isn't in range doesn't mean that
*        one of its Buy Froms isn't.  So, here we go.
            gosub buy_froms
            goto plow_loop

        buy_froms
            plowkey$ = vencode$
        bf_loop
            call "PLOWNEXT" (#03, plowkey$, 9%, f1%(3%))
                if f1%(3%) = 0% then return
            get #03 using L13550, bf_code$, address$()
L13550:         FMT POS(10), CH(6), POS(46), 6*CH(30)
            zip$ = str(address$(6%),22%,9%)
            if zip$ <= lozip$ or zip$ > hizip$ then bf_loop
            gosub write_workfile
            goto bf_loop

        write_workfile
            put #02 using L14030, vencode$, bf_code$, address$(),         ~
                                 address$(1%), ventype$, address$(1%)
L14030:         FMT CH(9), CH(6), 7*CH(30), CH(4), CH(30)
            write #02
            count% = count% + 1%
            return

        end_of_data
            if count% = 0% then nothing_to_print
            goto generate_report

        nothing_to_print
            call "ASKUSER" (2%, "NOTHING TO PRINT",                      ~
                          "No Vendors were found within the criteria",   ~
                          "specified.  Press any PF Key to Continue...", ~
                          " ")
            if f2%(2%) = 0% then call "FILEBGON" (#02)
            goto editpg1

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Vendor Code            */~
                              L20200,         /* Vendor Type            */~
                              L20300,         /* Zip Code               */~
                              L20400,         /* Sort By                */~
                              L20500,         /* Label Size             */~
                              L20600          /* Include Buy Froms      */
            return

L20100: REM Def/Enable Vendor Code                 FMVENCODE$
            if fmvencode$          = " " then                            ~
               fmvencode$          = "ALL"
            return

L20200: REM Def/Enable Vendor Type                 FMVENTYPE$
            if fmventype$          = " " then                            ~
               fmventype$          = "ALL"
            return

L20300: REM Def/Enable Zip Code                    FMZIP$
            if fmzip$              = " " then                            ~
               fmzip$              = "ALL"
            return

L20400: REM Def/Enable Sort By (C, T, or Z)        SORT_BY$
            if sort_by$ = " " then sort_by$ = "C"
            return

L20500: REM Def/Enable Label Size (1, 2, or 3)     SIZE$
            if size$ = " " then size$ = "1"
            return

L20600: REM Def/Enable Include Vendor Buy Froms    BUY_FROM$
            if buy_from$ = " " then buy_from$ = "Y"
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
         "Enter Vendor Code Range                                      ",~
         "Enter Vendor Type Range                                      ",~
         "Enter Zip Code Range                                         ",~
         "Enter 'C'ustomer, 'T'ype, or 'Z'ip to Sort by                ",~
         "Size:  1 = 3 x 4 (1 by), 2 = 1 x 3-1/2 (2 by), 3 = 1 x 3 (3 by)~
        ~.",                                                              ~
         "Enter 'Y'es to Include Vendor Buy From(s); else 'N'o."

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, buy_from$, print_a$(),     ~
                      size$, sort_by$, fmvencode$, fmventype$, fmzip$,   ~
                      hivencode$, hiventype$, hizip$, lovencode$,        ~
                      loventype$, lozip$, tovencode$, toventype$, tozip$
            call "FILEBGON" (#02)
            temp% = 0%
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
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
            call "SHOSTAT" ("Printing Labels...")
            gosub set_up_printer
            gosub page_head

            put_in_array% = 0% : print_a$() = " "
            plowkey$ = hex(00)
            akey% = pos("CTZ" = sort_by$)
            call "REDALT2" (#02, plowkey$, akey%, f1%(2%))
            goto L30122
        workfile_loop
            call "READNEXT" (#02, f1%(2%))
L30122:         if f1%(2%) <> 0% then L30140
                     if put_in_array% = 0% then L30136
                          if size% = 2% then gosub print_2_by
                          if size% = 3% then gosub print_3_by
L30136:              gosub end_report
                     goto inputmode
L30140:     get #02 using L30150, address$(), type$
L30150:         FMT POS(16), 6*CH(30), POS(226), CH(4)
*        Line Smash plus count the lines
            non_blank_lines% = 0%
            for i% = 1% to 6%
                if address$(i%) = " " then L30210
                     non_blank_lines% = non_blank_lines% + 1%
                     address$(non_blank_lines%) = address$(i%)
                     if non_blank_lines% <> i% then address$(i%) = " "
L30210:         next i%
*        Format Zip & truncate city if printing to only 30 characters
            zip$ = str(address$(non_blank_lines%),22%,9%)
            if len(zip$) <> 9% then L30310       /* Probably is U.S. zip */
                convert zip$ to temp%, data goto L30310 /* Assume US zip */
                temp$ = str(zip$,6%)
                str(address$(non_blank_lines%),27%,1%) = "-"
                str(address$(non_blank_lines%),28%,4%) = temp$
                if size% <> 3% then L30310
                     str(address$(non_blank_lines%),17%) =               ~
                                     str(address$(non_blank_lines%),18%)
L30310
*        Get rid of one line if 1 inch labels and all six are non-blank
*        Enie, meanie, minie, moe!  Catch a coder by the halo...
*          This eliminates the shortest of lines 2 through 5.
*          The (meager) assumption is that the line with the least info
*            is the least important.
*          If you don't like this, change it!
*            It could be a)eliminate line 4 (this emulates CUSLABEL)
*                        b)eliminate line 3
*                        c)eliminate line 2 or
*                        d)something else
            if non_blank_lines% < 6% then L30440
                temp% = 30%
                for i% = 2% to 5% /* Blow away last shortest line */
                     len% = len(address$(i%))
                     if len% > temp% then L30400
                          temp% = len%
                          rid% = i%
L30400:              next i%
                address$(rid%) = " "
                call "LINSMASH" (address$())
                non_blank_lines% = 5%
L30440
*        What kind of labels are we going to print?
            on size% goto L30500, L30600, L30600

L30500
*        3 x 4 (1 across)
            print using L60150, address$(1%)
            print using L60150, address$(2%)
            print using L60150, address$(3%)
            print using L60150, address$(4%)
            print using L60150, address$(5%)
            print using L60150, address$(6%)
            print
            print using L60150, type$
            print skip(10)
            goto workfile_loop

L30600
*        1 x 3 (3 across) and 1 x 3-1/2 (2 across)
            put_in_array% = put_in_array% + 1%
            for i% = 1% to 5%
                print_a$(i%, put_in_array%) = address$(i%)
                next i%
            if put_in_array% < size% then goto workfile_loop

            if size% = 2% then gosub print_2_by else gosub print_3_by
            put_in_array% = 0% : print_a$() = " "
            goto workfile_loop

        print_2_by
            print using L60170, print_a$(1%, 1%), print_a$(1%, 2%)
            print using L60170, print_a$(2%, 1%), print_a$(2%, 2%)
            print using L60170, print_a$(3%, 1%), print_a$(3%, 2%)
            print using L60170, print_a$(4%, 1%), print_a$(4%, 2%)
            print using L60170, print_a$(5%, 1%), print_a$(5%, 2%)
            print
            return

        print_3_by
            print using L60200, print_a$(1%, 1%), print_a$(1%, 2%),       ~
                               print_a$(1%, 3%)
            print using L60200, print_a$(2%, 1%), print_a$(2%, 2%),       ~
                               print_a$(2%, 3%)
            print using L60200, print_a$(3%, 1%), print_a$(3%, 2%),       ~
                               print_a$(3%, 3%)
            print using L60200, print_a$(4%, 1%), print_a$(4%, 2%),       ~
                               print_a$(4%, 3%)
            print using L60200, print_a$(5%, 1%), print_a$(5%, 2%),       ~
                               print_a$(5%, 3%)
            print
            return


        end_report                /* Report Ending Routine */
            print skip(2)
            print using L64990     /* End of report line */
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            return

        page_head              /* Page Heading Print Routine */
            print page        /* Top of Form */
            print using L60070, "VENLABEL", rptid$, date$
            print using L60080, fmvencode$, tovencode$
            print using L60090, fmventype$, toventype$
            print using L60095, fmzip$, tozip$
            print using L60100, sort_by$, size$
            print
            if size$ = "1" then print skip(12)
            return


        set_up_printer
            select printer(134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            return

        line_up_test
            call "SHOSTAT" ("Printing Test Labels for Alignment.")
            gosub set_up_printer
            gosub page_head
            for j% = 1% to 3%
                on size% gosub L34770, L34800, L34840
            next j%
            gosub end_report
            goto editpg1
L34770:         for i% = 1% to 8%
                     print using L60150, line_up_characters$
                next i% : print skip(10) : return
L34800:         for i% = 1% to 5%
                     print using L60170, line_up_characters$,             ~
                                        line_up_characters$
                next i% : print : return
L34840:         for i% = 1% to 5%
                     print using L60200, line_up_characters$,             ~
                                        line_up_characters$,             ~
                                        line_up_characters$
                next i% : print : return

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
              on fieldnr% gosub L40095,         /* Vendor Code       */   ~
                                L40095,         /* Vendor Type       */   ~
                                L40095,         /* Zip Code          */   ~
                                L40095,         /* Sort By           */   ~
                                L40095,         /* Label Size        */   ~
                                L40095          /* Include Buy Froms */
              goto L40110

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40095:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40110:     accept                                                       ~
               at (01,02),                                               ~
                  "Vendor Label Printing",                               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),   columnttl$           , ch(51),~
                                                                         ~
               at (07,02), "Vendor Code",                                ~
               at (07,30), fac(lfac$(1%)), fmvencode$           , ch(09),~
               at (07,56), fac(lfac$(1%)), tovencode$           , ch(09),~
                                                                         ~
               at (08,02), "Vendor Type",                                ~
               at (08,30), fac(lfac$(2%)), fmventype$           , ch(04),~
               at (08,56), fac(lfac$(2%)), toventype$           , ch(04),~
                                                                         ~
               at (09,02), "Zip Code",                                   ~
               at (09,30), fac(lfac$(3%)), fmzip$               , ch(09),~
               at (09,56), fac(lfac$(3%)), tozip$               , ch(09),~
                                                                         ~
               at (10,02), "Sort By (C, T, or Z)",                       ~
               at (10,30), fac(lfac$(4%)), sort_by$             , ch(01),~
                                                                         ~
               at (11,02), "Label Size (1, 2, or 3)",                    ~
               at (11,30), fac(lfac$(5%)), size$                , ch(01),~
                                                                         ~
               at (12,02), "Include Buy From(s)? (Y/N)",                 ~
               at (12,30), fac(lfac$(6%)), buy_from$            , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40315
                  call "MANUAL" ("VENLABEL") : goto L40110

L40315:        if keyhit% <> 15% then L40330
                  call "PRNTSCRN" : goto L40110

L40330:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40425     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40410
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
            if fieldnr% > 1% then L40415
L40410:         str(pf$(2%),18%,26%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L40415:     return

L40425: if fieldnr% > 0% then L40470  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                    (14)" &        ~
                     "Label Line Up Test     (16)Print Labels"
            pfkeys$ = hex(01ffffffffffffffffffffff0d0e0f1000)
            return
L40470:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
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
            on fieldnr% gosub L50100,         /* Vendor Code            */~
                              L50200,         /* Vendor Type            */~
                              L50300,         /* Zip Code               */~
                              L50400,         /* Sort By                */~
                              L50500,         /* Label Size             */~
                              L50600          /* Include Buy Froms?     */
            return
L50100: REM Test for Vendor Code                  FMVENCODE$
            call "TESTRNGE"                                              ~
                  (fmvencode$          , tovencode$          ,           ~
                   lovencode$          , hivencode$          ,           ~
                   errormsg$, #01)
            return

L50200: REM Test for Vendor Type                  FMVENTYPE$
            call "TESTRNGE"                                              ~
                  (fmventype$          , toventype$          ,           ~
                   loventype$          , hiventype$          ,           ~
                   errormsg$, #05, "VEN TYPES")
            return

L50300: REM Test for Zip Code                     FMZIP$
            call "TESTRNGE"                                              ~
                  (fmzip$              , tozip$              ,           ~
                   lozip$              , hizip$              ,           ~
                   errormsg$)
            return

L50400: REM Test for Sort By (C, T, or Z)         SORT_BY$
            if pos("CTZ" = sort_by$) <> 0% then L50490
                errormsg$ = "Invalid entry; Must be 'C', 'T', or 'Z'."
L50490:         return

L50500: REM Test for Label Size (1, 2, or 3)      SIZE$
            if pos("123" = size$) <> 0% then L50540
                errormsg$ = "Invalid entry; Must be '1', '2', or '3'."
                return
L50540:     convert size$ to size%
            return

L50600: REM Test for Include Vendor Buy Froms     BUY_FROM$
            if pos("YN" = buy_from$) <> 0% then L50630
                errormsg$ = "Invalid entry; Must be 'Y' or 'N'."
L50630:         return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Headers (Offset 5 and max 30 so all can use)
L60070: %     ########:######  ########
L60080: %     Ven- #########:##########
L60090: %     Type-     ####:####
L60095: %     Zip- #########:##########
L60100: %     Sort- #  Size- #
*       * 3 x 4 (1 across) 31 #s
L60150: %     ###############################
*       * 1 x 3-1/2  (2 across) 31 #s
L60170: %###############################     ############################~
        ~###
*       * 1 x 3  (3 across) 30 #s
L60200: %############################## ############################## ##~
        ~############################

*       * End of Report
L64990: %     ** END OF LIST **


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
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
