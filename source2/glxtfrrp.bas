        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   GGG   L      X   X  TTTTT  FFFFF  RRRR   RRRR   PPP     *~
            *  G      L       X X     T    F      R   R  R   R  P  P    *~
            *  G GGG  L        X      T    FFFF   RRRR   RRRR   PPP     *~
            *  G   G  L       X X     T    F      R   R  R   R  P       *~
            *   GGG   LLLLL  X   X    T    F      R   R  R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLXTFRRP - THIS PROGRAM PRINTS THE MANAGEMENT TRANSFER    *~
            *            VALUES CONTAINED IN THE FILE MGTFCTR2.         *~
            *            RANGES AND SORTS ARE USER SELECTED.            *~
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
            * 11/13/90 ! ORIGINAL                                 ! JDH *~
            * 10/20/92 ! Removed FACs from page zero.             ! JIM *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            cat$4,                       /* Part Category              */~
            cod$4,                       /* 'Code'                     */~
            code1$25, code2$25,          /* Codes                      */~
            company$60,                  /* Company Name               */~
            cursor%(2),                  /* Cursor location for edit   */~
            cus$9,                       /* Customer Code              */~
            date$8,                      /* Date for screen display    */~
            date_changed$8,              /* Date data changed          */~
            desc$79,                     /* GET/PLOWCODE Description   */~
            descr1$32, descr2$32,        /* Code Descriptions          */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            factor$6,                    /* Management Transfer Factor */~
            fr_cust_code$(2)9,           /* Scr 'From' Customer Code   */~
            fr_cust_type$(2)5,           /* Scr 'From' Customer Type   */~
            fr_effc_date$(2)8,           /* Scr 'From' Effective Date  */~
            fr_part_code$(2)25,          /* Scr 'From' Part Number     */~
            fr_part_ctgy$(2)5,           /* Scr 'From' Part Category   */~
            from$8,                      /* From Date                  */~
            hdr1$13, hdr2$13,            /* Code Headings              */~
            hivalue$25,                  /* All hex FF's               */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            p%(3),                       /* SEARCH catcher             */~
            part$25,                     /* Part Code                  */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey$50,                  /* Misc. Read Key             */~
            sortkey$(3)2,                /* Sort key codes             */~
            temp$20,                     /* OPENCHCK work area         */~
            time$8,                      /* Run Time                   */~
            to$8,                        /* To Effective Date          */~
            to_cust_code$(2)9,           /* Scr 'To' Customer Code     */~
            to_cust_type$(2)4,           /* Scr 'To' Customer Type     */~
            to_effc_date$(2)8,           /* Scr 'To' Effective Date    */~
            to_part_code$(2)25,          /* Scr 'To' Part Number       */~
            to_part_ctgy$(2)4,           /* Scr 'To' Part Category     */~
            type$2,                      /* Customer Type Code         */~
            userid$3,                    /* Current User Id            */~
            who_changed$3,               /* User Id last changed  data */~
            writkey$40                   /* WORKFILE key               */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! MGTFCTR2 ! Management Transfer Values File          *~
            * #02 ! GENCODES ! General Codes File                       *~
            * #03 ! CUSTOMER ! Customer Master File                     *~
            * #04 ! CATEGORY ! Inventory category codes file            *~
            * #05 ! HNYMASTR ! Inventory Master File                    *~
            * #09 ! WORKFILE ! Temporary System Workfile (Sort)         *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "MGTFCTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  100,                                  ~
                        keypos =    1, keylen =  46

            select #02, "GENCODES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  128,                                  ~
                        keypos =    1, keylen =  24

            select #03, "CUSTOMER",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1200,                                  ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #04, "CATEGORY",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  200,                                  ~
                        keypos =    1, keylen =   4

            select #05, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #09, "WORKFILE", varc, indexed, recsize = 86,         ~
                        keypos =    1, keylen =  86

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#01, 0%, f2%(1), 0%, temp$)
            get temp$ using L02580, sort%
L02580:         FMT POS(17), BI(4)
            sort% = max(100%, sort% / 2%)
            call "OPENCHCK" (#02, 0%, f2%(2), 0%, " ")
            call "OPENCHCK" (#03, 0%, f2%(3), 0%, " ")
            call "OPENCHCK" (#04, 0%, f2%(4), 0%, " ")
            call "OPENCHCK" (#05, 0%, f2%(5), 0%, " ")


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date : call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, u3%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            str(line2$,62) = "GLXTFRRP: " & str(cms2v$,,8)
            hivalue$ = all(hex(ff)) /* XOR for descending keys */
            cod$ = "Code"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            call "FILEBGON" (#09) /* Bye-bye, WORKFILE */
            gosub initialize_variables

            for fieldnr% = 1% to  8%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10205
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10205:               if keyhit% =  8% then print_all
                      if keyhit% = 16% and fieldnr% = 1% then L65000
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then goto L15000
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 7%
            if fieldnr% = 6% then editpg1
            if fieldnr% > 6% then fieldnr% = fieldnr% - 1%
            if fieldnr% < 1% or fieldnr% >  8% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11190:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11190
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11190
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *                M I S C   R O U T I N E S                  *~
            *************************************************************

        print_all
            readkey$ = hex(00)
            call "PLOWNEXT" (#01, readkey$, 0%, f1%(1))
            if f1%(1) = 1% then L14090
                call "ASKUSER" (0%, "NO RECORDS",                        ~
                                "There are no records to report on.",    ~
                                "Press any PF key to continue", " ")
                goto L65000

L14090:     u3% = 2%
            call "ASKUSER" (u3%, "MANAGEMENT TRANSFER VALUES REPORT",    ~
                            "Press RETURN to Continue with report",      ~
                            "-OR-",                                      ~
                            "Press any other PF Key to return to menu")
            if u3% <> 0% then L65000

            str(i$(08),20,5)  = "ALL  "
            str(i$(08),51,4)  = "    "
            str(i$(09),20,9)  = "ALL      "
            str(i$(10),20,5) = "ALL   "
            str(i$(11),20,25) = "ALL                      "
            str(i$(12),20,8)  = "ALL     "

            goto L15760

L15000: REM *************************************************************~
            *                P R I N T   R E P O R T                    *~
            * --------------------------------------------------------- *~
            * Do record selection and, if indicated, print a report.    *~
            *************************************************************

            call "SHOSTAT"                                               ~
                ("Management Transfer Values selection in process")
            plowkey$ = hex(00)   /* Plow the records */
            sw% = 0%

        plow_for_selection
            call "PLOWNEXT" (#01, plowkey$, 0%, f1%(1))
                if f1%(1) = 0% then goto L15680
            get #01, using L15150, cus$, type$, part$, cat$, from$
L15150:         FMT CH(9), CH(2), CH(25), CH(4), CH(6)

*        Try to eliminate the record based on user's criteria
            if str(fr_cust_type$(2),,2)<>" " or to_cust_type$(2)<>" "    ~
                then L15215                                               ~
            else goto L15260
L15215:     if type$ = " " then plow_for_selection
                if fr_cust_type$(1)= "ALL" then goto L15260
                if type$ < str(fr_cust_type$(2),,2)                      ~
                    then plow_for_selection
                if type$ > to_cust_type$(2) then plow_for_selection

L15260:     if fr_cust_code$(2)<>" " or to_cust_code$(2)<>" " then L15285 ~
            else goto L15320
L15285:     if cus$ = " " then plow_for_selection
                if fr_cust_code$(1)= "ALL" then goto L15320
                if cus$ < fr_cust_code$(2) then goto plow_for_selection
                if cus$ > to_cust_code$(2) then goto plow_for_selection

L15320:     if str(fr_part_ctgy$(2),,4)<>" " or to_part_ctgy$(2)<>" "    ~
                then L15355                                               ~
            else goto L15400
L15355:     if cat$ = " " then plow_for_selection
                if fr_part_ctgy$(1)= "ALL" then goto L15400
                if cat$ < str(fr_part_ctgy$(2),,4)                       ~
                    then plow_for_selection
                if cat$ > to_part_ctgy$(2) then plow_for_selection

L15400:     if fr_part_code$(2)<>" " or to_part_code$(2)<>" " then L15425 ~
            else goto L15460
L15425:     if part$ = " " then plow_for_selection
                if fr_part_code$(1)= "ALL" then goto L15460
                if part$ < fr_part_code$(2) then plow_for_selection
                if part$ > to_part_code$(2) then plow_for_selection

L15460:     if fr_effc_date$(1) = "ALL" then goto L15500
                if from$ < fr_effc_date$(2) then plow_for_selection
                if from$ > to_effc_date$(2) then plow_for_selection

L15500
*        Record is to be included -- fashion a sort key and write it to
*        the WORKFILE.
            writkey$ = " " : lth% = 0%
            for x% = 1% to 3%
                on pos("12345" = str(sortkey$(x%),1,1)) gosub            ~
                     key_cust_type, key_cust_code, key_category,         ~
                     key_part_code, key_from_date
            next x%

*        Key has been formatted -- write the WORKFILE record, thus
*        sorting the selected records.
            if sw% = 0% then call "WORKOPEN" (#09, "IO   ", sort%, f2%(9))
            write #09 using L15640, writkey$, cus$, type$, part$, cat$,   ~
                                                                    from$
L15640:         FMT CH(40), CH(9), CH(2), CH(25), CH(4), CH(6)
            sw% = 1% /* Indicate records output to the WORKFILE */
            goto plow_for_selection

L15680
*        MGTFCTR2 is at EOF -- see if we are to continue (any records?)
            if sw% <> 0% then goto L15760
                u3% = 0%
                call "ASKUSER" (u3%, "*** NULL SET SELECTED ***",        ~
                     "No records were selected based on your criteria.", ~
                     " ", "Press RETURN to continue.")
                goto inputmode

L15760
*        OK, we're going to print a report.
            call "SHOSTAT" ("Printing MANAGEMENT TRANSFER VALUES Report")
            line% = 857% : page% = 0%
            time$ = " " : call "TIME" (time$)
            select printer (134)
            call "SETPRNT"  ("MGTG02", " ", 0%, 0%)

        REM Print 'Page Zero' -- the selection screen *******************
            pagesw% = 1%
            gosub page_0_heading
            print skip (4)
L15862:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then goto L15870
                str(i$(), i%, 1%) = " "
                goto L15862
L15870:     print using L60220, "    ---- SELECTION CRITERIA ----"
            print
            for n% = 6% to 19%
                print using L60220, i$(n%)
            next n%
            pagesw% = 0%

            plowkey$ = xor plowkey$
            call "READ102" (#09, plowkey$, f1%(9)) /* Prime WORKFILE */
            if keyhit% = 8% then L16050 else L16000

        report_loop
            if keyhit% <> 8% then L15990
                 read #01, eod goto end_of_report
                 goto L16050
L15990:     call "READNEXT" (#09, f1%(9))
L16000:         if f1%(9) = 0% then goto end_of_report
            get #09 using L16020, readkey$
L16020:         FMT POS(41), CH(46) /* The key to the MGTFCTR2 file */
            call "READ100" (#01, readkey$, f1%(1))
                if f1%(1) = 0% then goto report_loop /* Ouch!!! */
L16050:     get  #01 using L16070, cus$, type$, part$, cat$, from$, to$,  ~
                                  who_changed$, date_changed$, factor
L16070:         FMT CH(9), CH(2), CH(25), CH(4), 2*CH(6), CH(3), CH(6),  ~
                    PD(14,4)
            call "DATEFMT" (to$  )
            call "DATEFMT" (from$)
            call "DATEFMT" (date_changed$)
            call "CONVERT" (factor, 2.2, factor$)

*        Describe the Code fields
            if cus$ = " " then L16190
                hdr1$    = "Customer Code"
                code1$   = cus$
                call "DESCRIBE" (#03, cus$, descr1$, 0%, f1%(3))
                goto L16240

L16190:         hdr1$    = "Customer Type"
                code1$   = type$
                readkey$ = "CUS TYPES" & type$
                call "DESCRIBE" (#02, readkey$, descr1$, 0%, f1%(2))

L16240:     if part$ = " " then L16300
                hdr2$    = "Part Number  "
                code2$   = part$
                call "DESCRIBE" (#05, part$, descr2$, 0%, f1%(5))
                goto L16340

L16300:         hdr2$    = "Part Category"
                code2$   = cat$
                call "DESCRIBE" (#04, cat$, descr2$, 0%, f1%(4))

L16340
*        Get the Header portion out of the way
            if line% > 54% then gosub page_heading
            print using L60100, hdr1$, code1$, descr1$, from$, to$, factor$
            print using L60130, hdr2$, code2$, descr2$, date_changed$,    ~
                               who_changed$
            print
            line% = line% + 3%

            goto report_loop


        page_heading
            page% = page% + 1%
            line% = 5%
        page_0_heading
            print page
            print using L60040, date$, time$, company$
            print using L60070, page%
            if pagesw% <> 0% then return
            print : print
            return


        end_of_report
            print
            print "END OF REPORT"
            close printer
            call "SETPRNT"  ("MGTG02", " ", 0%, 1%)
            goto inputmode


        REM *************************************************************~
            *   M I S C E L L A N E O U S   S U B R O U T I N E S       *~
            *************************************************************

        key_cust_code
            if str(sortkey$(x%),2,1) = "A"                               ~
                then str(writkey$,lth%+1%,9%) = cus$                     ~
                else str(writkey$,lth%+1%,9%) = cus$ xor hivalue$
            lth% = lth% + 9%
            return

        key_cust_type
            if str(sortkey$(x%),2,1) = "A"                               ~
                then str(writkey$,lth%+1%,2%) = type$                    ~
                else str(writkey$,lth%+1%,2%) = type$ xor hivalue$
            lth% = lth% + 2%
            return

        key_part_code
            if str(sortkey$(x%),2,1) = "A"                               ~
                then str(writkey$,lth%+1%,25%) = part$                   ~
                else str(writkey$,lth%+1%,25%) = part$ xor hivalue$
            lth% = lth% + 25%
            return

        key_category
            if str(sortkey$(x%),2,1) = "A"                               ~
                then str(writkey$,lth%+1%,4%) = cat$                     ~
                else str(writkey$,lth%+1%,4%) = cat$ xor hivalue$
            lth% = lth% + 4%
            return

        key_from_date
            if str(sortkey$(x%),2,1) = "A"                               ~
                then str(writkey$,lth%+1%,6%) = from$                    ~
                else str(writkey$,lth%+1%,6%) = from$ xor hivalue$
            lth% = lth% + 6%
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20170,         /* Customer Type          */~
                              L20260,         /* Customer Code          */~
                              L20350,         /* Part Category          */~
                              L20440,         /* Part Code              */~
                              L20530,         /* Eff Date From          */~
                              L20580,         /* 1st Sort key code      */~
                              L20610,         /* 2nd Sort key code      */~
                              L20640          /* 3rd Sort key code      */
            return
L20170: REM Def/Enable Customer Type               FR_CUST_TYPE$
            if fr_cust_code$(1) = " " and to_cust_code$(1) = " "         ~
                then goto L20220
                enabled% = 0%
                return
L20220:     if fr_cust_type$(1) = " " and to_cust_type$(1) = " "         ~
                then fr_cust_type$(1) = "ALL"
            return

L20260: REM Def/Enable Customer code               FR_CUST_CODE$
            if fr_cust_type$(1) = " " and to_cust_type$(1) = " "         ~
                then goto L20310
                enabled% = 0%
                return
L20310:     if fr_cust_code$(1) = " " and to_cust_code$(1) = " "         ~
                then fr_cust_code$(1) = "ALL"
            return

L20350: REM Def/Enable Part Category               FR_PART_CTGY$
            if fr_part_code$(1) = " " and to_part_code$(1) = " "         ~
                then goto L20400
                enabled% = 0%
                return
L20400:     if fr_part_ctgy$(1) = " " and to_part_ctgy$(1) = " "         ~
                then fr_part_ctgy$(1) = "ALL"
            return

L20440: REM Def/Enable Part Code                   FR_PART_CODE$
            if fr_part_ctgy$(1) = " " and to_part_ctgy$(1) = " "         ~
                then goto L20490
                enabled% = 0%
                return
L20490:     if fr_part_code$(1) = " " and to_part_code$(1) = " "         ~
                then fr_part_code$(1) = "ALL"
            return

L20530: REM Def/Enable Eff Date From               FR_EFFC_DATE$
            if fr_effc_date$(1) = " " and to_effc_date$(1) = " "         ~
                then fr_effc_date$(1) = "ALL"
            return

L20580: REM Def/Enable 1st Sort key code           SORTKEY$(1)
            return

L20610: REM Def/Enable 2nd Sort key code           SORTKEY$(2)
            return

L20640: REM Def/Enable 3rd Sort key code           SORTKEY$(3)
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Customer Type range, 'FIRST', 'LAST', 'ALL', or Blank. '?~
        ~' to see list.",                                                 ~
         "Enter Customer Code range, 'FIRST', 'LAST', or 'ALL'. '?' to se~
        ~e list.",                                                        ~
         "Enter Part Category range, 'FIRST', 'LAST', 'ALL', or Blank. '?~
        ~' to see list.",                                                 ~
         "Enter Part Code range, 'FIRST', 'LAST', or 'ALL'. '?' to see li~
        ~st.",                                                            ~
         "Enter Effective Date range, 'FIRST', 'LAST', or 'ALL'.       ",~
         "Enter 1st Sort key code as 'na' (n=1-5 and a='A' or 'D').    ",~
         "Enter 2nd Sort key code as 'na' (n=1-5 and a='A' or 'D').    ",~
         "Enter 3rd Sort key code as 'na' (n=1-5 and a='A' or 'D').    "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      fr_cust_code$(), fr_cust_type$(),                  ~
                      fr_effc_date$(), fr_part_code$(), fr_part_ctgy$(), ~
                      to_cust_code$(), to_cust_type$(),                  ~
                      to_effc_date$(), to_part_code$(), to_part_ctgy$(), ~
                      sortkey$()
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1989  an unpublished work by CAELUS,       *~
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
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40220,         /* Customer Type     */   ~
                                L40220,         /* Customer Code     */   ~
                                L40220,         /* Part Category     */   ~
                                L40220,         /* Part Code         */   ~
                                L40220,         /* Eff Date From     */   ~
                                L40220,         /* 1st Sort key code */   ~
                                L40220,         /* 2nd Sort key code */   ~
                                L40220          /* 3rd Sort key code */
              goto L40250

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40220:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40250:     accept                                                       ~
               at (01,02), "Management Transfer Values Report  ",        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), fac(hex(ac)), cod$                   , ch(04),~
                                                                         ~
               at (08,02), " 1.",                                        ~
               at (08,06), "Customer Type",                              ~
               at (08,20), fac(lfac$( 1)), fr_cust_type$(1)     , ch(05),~
               at (08,46), "thru",                                       ~
               at (08,51), fac(lfac$( 1)), to_cust_type$(1)     , ch(04),~
                                                                         ~
               at (09,02), " 2.",                                        ~
               at (09,10), "-or- Code",                                  ~
               at (09,20), fac(lfac$( 2)), fr_cust_code$(1)     , ch(09),~
               at (09,46), "thru",                                       ~
               at (09,51), fac(lfac$( 2)), to_cust_code$(1)     , ch(09),~
                                                                         ~
               at (10,02), " 3.",                                        ~
               at (10,06), "Part Category",                              ~
               at (10,20), fac(lfac$( 3)), fr_part_ctgy$(1)     , ch(05),~
               at (10,46), "thru",                                       ~
               at (10,51), fac(lfac$( 3)), to_part_ctgy$(1)     , ch(04),~
                                                                         ~
               at (11,02), " 4.",                                        ~
               at (11,10), "-or- Code",                                  ~
               at (11,20), fac(lfac$( 4)), fr_part_code$(1)     , ch(25),~
               at (11,46), "thru",                                       ~
               at (11,51), fac(lfac$( 4)), to_part_code$(1)     , ch(25),~
                                                                         ~
               at (12,02), " 5.",                                        ~
               at (12,06), "Eff Date From",                              ~
               at (12,20), fac(lfac$( 5)), fr_effc_date$(1)     , ch(08),~
               at (12,46), "thru",                                       ~
               at (12,51), fac(lfac$( 5)), to_effc_date$(1)     , ch(08),~
                                                                         ~
               at (14,02), "1st Sort key code",                          ~
               at (14,20), fac(lfac$( 6)), sortkey$(1)          , ch(02),~
                                                                         ~
               at (15,02), "2nd Sort key code",                          ~
               at (15,20), fac(lfac$( 7)), sortkey$(2)          , ch(02),~
                                                                         ~
               at (16,02), "3rd Sort key code",                          ~
               at (16,20), fac(lfac$( 8)), sortkey$(3)          , ch(02),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40830
                  call "MANUAL" ("GLXTFRRP") : goto L40250

L40830:        if keyhit% <> 15 then L40860
                  call "PRNTSCRN" : goto L40250

L40860:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41050     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (8)Print All           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffff08ffffffff0dff0f1000)
            if fieldnr% = 1% then L41010
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
                str(pf$(1),18,12)  = " " :  str(pfkeys$, 8,1) = hex(ff)
L41010:     if fieldnr% > 2% then L41030
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41030:     return

L41050: if fieldnr% > 0% then L41140  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffff0dff0f1000)
            return
L41140:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50170,         /* Customer Type          */~
                              L50450,         /* Customer Code          */~
                              L50690,         /* Part Category          */~
                              L50930,         /* Part Code              */~
                              L51170,         /* Eff Date From          */~
                              L51440,         /* 1st Sort key code      */~
                              L51440,         /* 2nd Sort key code      */~
                              L51440          /* 3rd Sort key code      */
            return
L50170: REM Test for Customer Type                FR_CUST_TYPE$
            if enabled% <> 0% then goto L50210
                init (" ") fr_cust_type$(), to_cust_type$()
                return
L50210:     fr_cust_type$(2), to_cust_type$(2) = " "
            if fr_cust_type$(1)=" " and to_cust_type$(1)=" " then return
            if fr_cust_type$(1)="ALL" then goto L50400
            if fr_cust_type$(1)="FIRST" then goto L50330
            if fr_cust_type$(1)="?" then fr_cust_type$(1)=" "
            desc$ = hex(06) & "Select the starting Customer Type code."
            plowkey$ = "CUS TYPES" & fr_cust_type$(1)
            call "PLOWCODE" (#02, plowkey$, desc$, 9%, .3, f1%(2))
            if f1%(2) = 0% then goto L50330
*              ERRORMSG$ = "Customer type is invalid. Try again."
*              RETURN
            fr_cust_type$(1) = str(plowkey$,10,2)
L50330:     if to_cust_type$(1)="LAST" then goto L50400
            if to_cust_type$(1)="?" then to_cust_type$(1)=" "
            desc$ = hex(06) & "Select the 'thru' Customer Type code."
            plowkey$ = "CUS TYPES" & to_cust_type$(1)
            call "PLOWCODE" (#02, plowkey$, desc$, 9%, .3, f1%(2))
            if f1%(2) = 0% then goto L50400
            to_cust_type$(1) = str(plowkey$,10,2)
L50400:     call "TESTRNGE" (fr_cust_type$(1), to_cust_type$(1),         ~
                fr_cust_type$(2), to_cust_type$(2), errormsg$)
            init (" ") fr_cust_code$(), to_cust_code$()
            return

L50450: REM Test for Customer code                FR_CUST_CODE$
            if enabled% <> 0% then goto L50490
                init (" ") fr_cust_code$(), to_cust_code$()
                return
L50490:     fr_cust_code$(2), to_cust_code$(2) = " "
            if fr_cust_code$(1)=" " and to_cust_code$(1)=" " then return
            if fr_cust_code$(1)="ALL" then goto L50640
            if fr_cust_code$(1)="FIRST" then goto L50590
            if fr_cust_code$(1)="?" then fr_cust_code$(1)=" "
            desc$ = hex(0684) & "Select the starting Customer Code."
            call "GETCODE" (#03, fr_cust_code$(1), desc$, 0%, 0, f1%(3))
*          IF F1%(3) <> 0% THEN GOTO 50590
*              ERRORMSG$ = "Customer code is invalid. Try again."
*              RETURN
L50590:     if to_cust_code$(1)="LAST" then goto L50640
            if to_cust_code$(1)="?" then to_cust_code$(1)=" "
            desc$ = hex(0684) & "Select the 'thru' Customer Code."
            call "GETCODE" (#03, to_cust_code$(1), desc$, 0%, 0, f1%(3))
*          IF F1%(3) = 0% THEN GOTO 50570
L50640:     call "TESTRNGE" (fr_cust_code$(1), to_cust_code$(1),         ~
                fr_cust_code$(2), to_cust_code$(2), errormsg$)
            init (" ") fr_cust_type$(), to_cust_type$()
            return

L50690: REM Test for Part Category                FR_PART_CTGY$
            if enabled% <> 0% then goto L50730
                init (" ") fr_part_ctgy$(), to_part_ctgy$()
                return
L50730:     fr_part_ctgy$(2), to_part_ctgy$(2) = " "
            if fr_part_ctgy$(1)=" " and to_part_ctgy$(1)=" " then return
            if fr_part_ctgy$(1)="ALL" then goto L50880
            if fr_part_ctgy$(1)="FIRST" then goto L50830
            if fr_part_ctgy$(1)="?" then fr_part_ctgy$(1)=" "
            desc$ = hex(0684) & "Select the starting Part Category."
            call "GETCODE" (#04, fr_part_ctgy$(1), desc$, 0%, 0, f1%(4))
*          IF F1%(4) <> 0% THEN GOTO 50830
*              ERRORMSG$ = "Part Category is invalid. Try again."
*              RETURN
L50830:     if to_part_ctgy$(1)="LAST" then goto L50880
            if to_part_ctgy$(1)="?" then to_part_ctgy$(1)=" "
            desc$ = hex(0684) & "Select the 'thru' Part Category."
            call "GETCODE" (#04, to_part_ctgy$(1), desc$, 0%, 0, f1%(4))
*          IF F1%(4) = 0% THEN GOTO 50810
L50880:     call "TESTRNGE" (fr_part_ctgy$(1), to_part_ctgy$(1),         ~
                fr_part_ctgy$(2), to_part_ctgy$(2), errormsg$)
            init (" ") fr_part_code$(), to_part_code$()
            return

L50930: REM Test for Part code                    FR_PART_CODE$
            if enabled% <> 0% then goto L50970
                init (" ") fr_part_code$(), to_part_code$()
                return
L50970:     fr_part_code$(2), to_part_code$(2) = " "
            if fr_part_code$(1)=" " and to_part_code$(1)=" " then return
            if fr_part_code$(1)="ALL" then goto L51120
            if fr_part_code$(1)="FIRST" then goto L51070
            if fr_part_code$(1)="?" then fr_part_code$(1)=" "
            desc$ = hex(0684) & "Select the starting Part Code."
            call "GETCODE" (#05, fr_part_code$(1), desc$, 0%, 0, f1%(5))
*          IF F1%(5) <> 0% THEN GOTO 51070
*              ERRORMSG$ = "Part Code is invalid. Try again."
*              RETURN
L51070:     if to_part_code$(1)="LAST" then goto L51120
            if to_part_code$(1)="?" then to_part_code$(1)=" "
            desc$ = hex(0684) & "Select the 'thru' Part Code."
            call "GETCODE" (#05, to_part_code$(1), desc$, 0%, 0, f1%(5))
*          IF F1%(5) = 0% THEN GOTO 51050
L51120:     call "TESTRNGE" (fr_part_code$(1), to_part_code$(1),         ~
                fr_part_code$(2), to_part_code$(2), errormsg$)
            init (" ") fr_part_ctgy$(), to_part_ctgy$()
            return

L51170: REM Test for Eff Date From                FR_EFFC_DATE$
            if fr_effc_date$(1)<>" " or to_effc_date$(1)<>" " then L51200
                fr_effc_date$(1) = "ALL" : to_effc_date$(1) = " "
L51200:     if fr_effc_date$(1) <> "ALL" and to_effc_date$(1) <> "ALL"   ~
                then goto L51270
                fr_effc_date$(1) = "ALL"
                to_effc_date$(1) = " "
                fr_effc_date$(2) = all(hex(00))
                to_effc_date$(2) = all(hex(ff))
                return
L51270:     if fr_effc_date$(1) <> "FIRST" then goto L51300
                fr_effc_date$(2) = all(hex(00))
                goto L51330
L51300:     call "DATEOK" (fr_effc_date$(1), temp%, errormsg$)
                if errormsg$ <> " " then return
            fr_effc_date$(2) = fr_effc_date$(1)
            call "DATUNFMT" (fr_effc_date$(2))
L51330:     if to_effc_date$(1) <> "LAST" then goto L51360
                to_effc_date$(2) = all(hex(ff))
                return
L51360:     call "DATEOK" (to_effc_date$(1), temp%, errormsg$)
                if errormsg$ <> " " then return
            to_effc_date$(2) = to_effc_date$(1)
            call "DATUNFMT" (to_effc_date$(2))
             if fr_effc_date$(2) > to_effc_date$(2) then                  ~
                errormsg$ = "Starting date must be earlier than the 'th"&~
                "ru' date."
            return

L51440: REM Test for Sort key codes               SORTKEY$(x)
            if str(sortkey$()) <> " " then goto L51480
                errormsg$ = "You must specify at least one sort key."
                return
L51480:     if sortkey$(fieldnr%-5%) = " " then return
            if pos("12345" = sortkey$(fieldnr%-5%)) <> 0% then L51520
                errormsg$ = "Specify a field code ('n') of 1 thru 5."
                return
L51520:     if pos("AD" = str(sortkey$(fieldnr%-5%),2,1)) <> 0% then L51560
                errormsg$ = "Specify a = 'A' for ascending or 'D' for d"&~
                    "escending sequence."
                return
L51560:     mat p% = zer
            search str(sortkey$()) = str(sortkey$(fieldnr%-5%),,1)       ~
                to p%() step 2%
            if p%(2) = 0% then goto L51630
                errormsg$ = "Field codes ('n') (1 - 5) may not be dupli"&~
                    "cated."
                return
L51630:     on pos("1234" = sortkey$(fieldnr%-5%)) gosub                 ~
                check_cust_type, check_cust_code, check_part_ctgy,       ~
                check_part_code
            return

        check_cust_type
            if fr_cust_type$(1) = " " and to_cust_type$(1) = " " then    ~
                errormsg$ = "Customer Type field was not selected."
                return

        check_cust_code
            if fr_cust_code$(1) = " " and to_cust_code$(1) = " " then    ~
                errormsg$ = "Customer Code field was not selected."
                return

        check_part_ctgy
            if fr_part_ctgy$(1) = " " and to_part_ctgy$(1) = " " then    ~
                errormsg$ = "Part Category field was not selected."
                return

        check_part_code
            if fr_part_code$(1) = " " and to_part_code$(1) = " " then    ~
                errormsg$ = "Part Code field was not selected."
                return

        REM *************************************************************~
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************

L60040: %RUN DATE: ######## ########            #########################~
        ~###################################               GLXTFRRP-MGTG02

L60070: %                                                    MANAGEMENT T~
        ~RANSFER VALUES REPORT                                 PAGE: ###

L60100: %#############: ######################### #######################~
        ~#########    Effective from ######## to ########   FACTOR: ######

L60130: %#############: ######################### #######################~
        ~#########    Last Changed:  ######## by ###

L60220: %                          ######################################~
        ~##########################################

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * Closes all the files currently open, and also displays    *~
            * a message (ONLY if in foreground) while linking to the    *~
            * next program.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One Moment Please")
            call "FILEBGON" (#09)
            end
