        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   SSS    AAA   DDDD   EEEEE  DDDD   IIIII  TTTTT   SSS    *~
            *  S      A   A  D   D  E      D   D    I      T    S       *~
            *   SSS   AAAAA  D   D  EEE    D   D    I      T     SSS    *~
            *      S  A   A  D   D  E      D   D    I      T        S   *~
            *   SSS   A   A  DDDD   EEEEE  DDD    IIIII    T     SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SADEDITS - Allows the user to twittle with SA records.    *~
            *            Does NOT update summaries.  Care should be     *~
            *            taken.  Unlike SAPOSTSB, this writes a user id *~
            *            to the record to identify the reprobate.       *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/07/93 ! Original                                 ! JDH *~
            * 08/21/96 ! Modified for Century Dates               ! DER *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            account$12,                  /* Sales Account              */~
            account_xref$9,              /* Account X-Ref              */~
            adj_reason$9,                /* Adjustment Reason          */~
            blankdate$8,                 /* Test for blank date        */~
            bol$3,                       /* BOL Number                 */~
            cat$4,                       /* Category                   */~
            curr$4,                      /* Currency Code              */~
            curr_date$8,                 /* Date Effective             */~
            curr_type$1,                 /* Currency Table             */~
            currkey$99,                  /* Currency Key               */~
            cursor%(2),                  /* Cursor location for edit   */~
            cus_code$9,                  /* Customer                   */~
            cus_type$2,                  /* Customer Type              */~
            cut_off_date$8,              /* Cut Off for Deletions      */~
            date_time$7,                 /* Date for record uniqueness */~
            date$8,                      /* Date for screen display    */~
            descr$79,                    /* Description for PLOWCODE   */~
            descr$(5)79,                 /* Descriptions for PLOWCODE  */~
            descr_map(14),               /* Mapping for PLOWCODE       */~
            edtmessage$79,               /* Edit screen message        */~
            equiv$15,                    /* Equivalency Factor         */~
            errormsg$79,                 /* Error message              */~
            fiscal_start$8,              /* Fiscal Year Start          */~
            gross$10,                    /* Gross Value                */~
            hdr$(2)79,                   /* Headers for PLOWCODE       */~
            i$(24)80,                    /* Screen Image               */~
            incl_excl(5),                /* Include/Exclude PLOWCODE   */~
            incl_excl$(5)16,             /* Include/Exclude PLOWCODE   */~
            inpmessage$79,               /* Informational Message      */~
            invoice$8,                   /* Invoice                    */~
            inv_cost$10,                 /* Inventory Cost             */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line$3,                      /* Line Number                */~
            line2$79,                    /* Screen Line #2             */~
            mc_on$1,                     /* Is Multi-Currency Active?  */~
            mdmc$10,                     /* MDMC Value                 */~
            mgtrpt_on$1,                 /* Is Management Values Active*/~
            net$10,                      /* Net Value                  */~
            part$25,                     /* Part Number                */~
            period$2,                    /* Fiscal Period              */~
            per_stat$15,                 /* Units/Statutory            */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            post_date$8,                 /* Posted Date                */~
            price_price$10,              /* Price @ Pricing UOM        */~
            price_stock$10,              /* Price @ Stocking UOM       */~
            project$8,                   /* Project Number             */~
            qty$10,                      /* Quantity                   */~
            rec_type$1,                  /* Record Type                */~
            region$4,                    /* Region                     */~
            rev_date$8,                  /* Reverse Date for Currency  */~
            salesperson$4,               /* Salesperson                */~
            ship_date$8,                 /* Ship Date                  */~
            so$16,                       /* Sales Order                */~
            startdate$8,                 /* Start date for deletion    */~
            stat$4,                      /* Statutory Currency & Flag  */~
            std_cost$10,                 /* Standard Cost              */~
            stocked_flag$1,              /* Stocked Part (Y/N)         */~
            str$3,                       /* Store                      */~
            userid$3                     /* Current User Id            */~

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
            * #01 ! SADETAIL ! Sales Analysis Detail Transactions       *~
            * #02 ! SYSFILE2 ! Caelus Management System Information     *~
            * #03 ! GENCODES ! System General Codes file.               *~
            * #04 ! CATEGORY ! INVENTORY CATEGORY CODES FILE            *~
            * #05 ! CUSTOMER ! Customer Master File                     *~
            * #06 ! STORNAME ! STORE INFORMATION FILE                   *~
            * #07 ! SLMMASTR ! Salesman master file                     *~
            * #08 ! GLMAIN   ! General Ledger CHart Of Accounts File.   *~
            * #09 ! HNYMASTR ! Inventory Master File                    *~
            * #40 ! CURMASTR ! Currency Master file                     *~
            * #42 ! CURCONVR ! Multi-Currency Conversion Tables         *~
            * #50 ! WORKFILE ! Record Types Workfile                    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "SADETAIL",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   8

            select #02, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #03, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #04, "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   4

            select #05, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  5, keypos = 1049, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  1, keypos =   10, keylen =  30, dup

            select #06, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3

            select #07, "SLMMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   4

            select #08, "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            select #09, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos = 1, keylen = 25,                         ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90, keylen = 4, dup,  ~
                                   key 3, keypos = 26, keylen = 32, dup

            select #40, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =   4

            select #42, "CURCONVR",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  11

            select #50, "RECTYPE",                                       ~
                        varc,     indexed,  recsize =  50,               ~
                        keypos = 1, keylen = 1

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 0%, rslt$(01%))
            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 0%, rslt$(02%))
            call "OPENCHCK" (#03, fs%(03%), f2%(03%), 0%, rslt$(03%))
            call "OPENCHCK" (#04, fs%(04%), f2%(04%), 0%, rslt$(04%))
            call "OPENCHCK" (#05, fs%(05%), f2%(05%), 0%, rslt$(05%))
            call "OPENCHCK" (#06, fs%(06%), f2%(06%), 0%, rslt$(06%))
            call "OPENCHCK" (#07, fs%(07%), f2%(07%), 0%, rslt$(07%))
            call "OPENCHCK" (#08, fs%(08%), f2%(08%), 0%, rslt$(08%))
            call "OPENCHCK" (#09, fs%(09%), f2%(09%), 0%, rslt$(09%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62%) = "SADEDITS: " & str(cms2v$,,8%)

*        See if operator is an administrator or not
            call "CMSMACHK" ("SA ", lfac$(1%), lfac$(2%))
            if lfac$(1%) = "Y" or lfac$(2%) = "Y" then L09240
                u3% = 0%
                call "ASKUSER" (u3%, "RESTRICTED ACCESS",                ~
                     "You must be a Sales Analysis Module Administrator",~
                     "or a Database Administrator to run this program.", ~
                     "Press RETURN to acknowledge and exit.")
                goto exit_program

L09240:         u3% = 0%
                call "ASKUSER" (u3%, " WARNING!!! ",                     ~
                     "Non-system edits of Sales Analysis records have " &~
                     "NO supporting detail!", "No updating of summary " &~
                     "records will be performed!",                       ~
                     "Press RETURN to Exit -or- PF16 to Continue.")

                if u3%  =  0% then exit_program
                if u3% <> 16% then L09240

*        Is MC on?
            stat$, mc_on$ = " "
            plowkey$ = "SWITCHS.CUR         "
            call "READ100" (#02, plowkey$, f1%(2%))
            if f1%(2%) = 0% then L09370
                get #02 using L09330, mc_on$, stat$, curr_type$
L09330:             FMT POS(21), CH(1), CH(4), POS(27), CH(1)
                if mc_on$ <> "Y" then stat$ = " "
                if mc_on$ <> "Y" then L09370
                call "OPENCHCK" (#40, fs%(40%), f2%(40%), 0%, rslt$(40%))
                call "OPENCHCK" (#42, fs%(42%), f2%(42%), 0%, rslt$(42%))

L09370
*        See if G/L Management Reporting is on
            mgtrpt_on$ = "N"
            plowkey$ = "SWITCHS.GL          "
            call "READ100" (#02, plowkey$, f1%(2%))
            if f1%(2%) = 1% then get #02 using L09420, mgtrpt_on$
L09420:         FMT POS(59), CH(1)

*        Are there Adjustment or Cancellation Reasons on file?
            plowkey$ = "SO REASON" & hex(00)
            call "PLOWNEXT" (#03, plowkey$, 9%, adj_reason_on_file%)
            plowkey$ = "CANREASON" & hex(00)
            call "PLOWNEXT" (#03, plowkey$, 9%, can_reason_on_file%)

*        Are there Customer Types on file?
            plowkey$ = "CUS TYPES" & hex(00)
            call "PLOWNEXT" (#03, plowkey$, 9%, cus_type_on_file%)

*        Are there Regions on file?
            plowkey$ = "REGIONS  " & hex(00)
            call "PLOWNEXT" (#03, plowkey$, 9%, region_on_file%)

*        Descriptions for PLOWCODE Header
            descr$(1%) = "Select from Original SO Booking Records"
            descr$(2%) = "Select from Adjusted SO Booking Records"
            descr$(3%) = "Select from Invoiced Shipping Records"
            descr$(4%) = "Select from Invoiced Bookings Records"
            descr$(5%) = "Select from Cancelled SO Bookings Records"

*        Load up workfile for record types
            call "WORKOPEN" (#50, "IO   ", 20%, 1%)
            write #50, using L09800, " ", "Blank denotes all record types."
L09800:         FMT CH(1), CH(49)
            write #50, using L09800, "1", "Original SO Bookings Record."
            write #50, using L09800, "2", "Adjusted SO Bookings Record."
            write #50, using L09800, "3", "Invoiced Shipping Record."
            write #50, using L09800, "4", "Invoiced Bookings Record."
            write #50, using L09800, "5", "Cancelled SO Record."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 12%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  8% then       find_record
                      if keyhit%  = 12% then       delete_many
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

            for fieldnr% = 1% to 14%
L10280:         gosub'052(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10400
L10300:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10380
L10330:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L10300
                         if fieldnr% = 1% then L10280
                         goto L10330
L10380:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10300
L10400:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10300
            next fieldnr%

            if mc_on$ <> "Y" then L11000

            for fieldnr% = 1% to  4%
L10450:         gosub'053(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10570
L10470:         gosub'103(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10550
L10500:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'053(fieldnr%)
                         if enabled% = 1% then L10470
                         if fieldnr% = 1% then L10450
                         goto L10500
L10550:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10470
L10570:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10470
            next fieldnr%

L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  5% then       editpg2
                  if keyhit%  = 12% then       delete_record
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11130:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 12% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11180:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11180
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11180
                  lastfieldnr% = fieldnr%
            goto L11130

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% then       editpg1
                  if keyhit%  =  5% then       editpg3
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg2
L11340:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 14% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L11390:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11390
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11390
                  lastfieldnr% = fieldnr%
            goto L11340

        editpg3
            lastfieldnr% = 0%
            gosub'103(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% then       editpg2
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg3
L11540:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% > 2% then L11544
                fieldnr% = 1% : goto L11550
L11544:     if fieldnr% > 4% then L11546
                fieldnr% = 2% : goto L11550
L11546:     fieldnr% = fieldnr% - 2%
L11550:     if fieldnr% < 1% or fieldnr% >  4% then editpg3
            if fieldnr% = lastfieldnr% then    editpg3
            gosub'053(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg3
L11590:     gosub'103(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11590
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11590
                  lastfieldnr% = fieldnr%
            goto L11540

        find_record
*        Get find parameters from screen
            new_record% = 0%
L11930:     gosub find_parameters_screen
                if keyhit%  =  1% then gosub startover
                if keyhit%  = 16% then       inputmode
                if keyhit% <>  0% then       find_record
            for fieldnr% = 1% to 6%
                gosub'151(fieldnr%)
                    if errormsg$ <> " " then L11930
                next fieldnr%

*        Set up description and break
            plowkey$ = rec_type$
            descr$ = hex(06) & "Select from all Sales Analysis Records"
            break% = 9000%
            if rec_type$ = " " then L12020
                convert rec_type$ to temp%
                descr$ = hex(06) & descr$(temp%)
                break% = 9001%

L12020
*        Set up include/exclude
            mat incl_excl = zer
            init (" ") incl_excl$()
            if post_date$ = " " or post_date$ = blankdate$ then L12100
                incl_excl(1%)  = 9.06
                temp$ = post_date$
                call "DATUNFMT" (temp$)
                incl_excl$(1%) = str(temp$,1%,6%)
L12100:     if so$ = " " then L12130
                incl_excl(2%)  = 15.16
                incl_excl$(2%) = so$
L12130:     if bol$ = " " then L12160
                incl_excl(3%)  = 31.03
                incl_excl$(3%) = bol$
L12160:     if invoice$ = " " then L12190
                incl_excl(4%)  = 34.08
                incl_excl$(4%) = invoice$
L12190:     if line$ = " " then L12300
                incl_excl(5%)  = 42.03
                incl_excl$(5%) = line$

L12300
*        Set up screen mapping
            init (" ") hdr$()
            hdr$(1%) = " Type Post Date Sales Order    BOL   Invoice " & ~
                       "Line Part"

            descr_map( 1%) =  1.01  : descr_map( 2%) =  3.0
            descr_map( 3%) =  9.061 : descr_map( 4%) =  6.0
            descr_map( 5%) = 15.16  : descr_map( 6%) = 15.0
            descr_map( 7%) = 31.03  : descr_map( 8%) = 32.0
            descr_map( 9%) = 34.08  : descr_map(10%) = 36.0
            descr_map(11%) = 42.03  : descr_map(12%) = 45.0
            descr_map(13%) = 93.25  : descr_map(14%) = 49.0

*        And finally the PLOW
            call "PLOWCODE" (#01, plowkey$, descr$, break%, .99, f1%(1%),~
                             hdr$(), 0.0, 0.0, incl_excl(), incl_excl$(),~
                             "D", " ", #01, descr_map())
            if f1%(1%) = 0% then inputmode
                gosub dataload
                goto editpg1

        delete_record
*        First the Confirmation Hearings
L13020:     u3% = 2%
            call "ASKUSER" (u3%, "*** CONFIRM DELETION ***",             ~
                            "Press PF16 to Delete Record", "-- or --",   ~
                            "Press PF1 to Abort Deletion")
            if u3% =   1% then editpg1
            if u3% <> 16% then L13020

*        I hear and obey....
            plowkey$ = str(rec_type$) & date_time$
            call "READ101" (#01, plowkey$, f1%(1%))
            if f1%(1%) <> 1% then editpg1 /* Opps! How can this be? */
                delete #01
                goto inputmode

        delete_many
*        Get Cut-Off Date from User
            u3% = 0% : d% = 0%
            startdate$ = "19000101"
            call "DATECONV" (startdate$)
            call "ASKDATE" (u3%, "CUT OFF DATE FOR DELETIONS",           ~
                            "Enter the Posting Date through which you " &~
                            "want Deletions to happen.", startdate$,       ~
                            date, cut_off_date$, d%)
            if u3% <> 0% then inputmode

*        Confirm it
            call "DATEFMT" (cut_off_date$)
L13610:     u3% = 2%
            call "ASKUSER" (u3%, "*** CONFIRM DELETIONS ***",            ~
                            "Press PF16 to Delete Records with a " &     ~
                            "Posting Date on or before " &               ~
                            cut_off_date$ & ".",                         ~
                            "-- or --", "Press PF1 to Abort Deletions")
            if u3% =   1% then inputmode
            if u3% <> 16% then L13610

*        Do it
            call "DATUNFMT" (cut_off_date$)
            del_cntr% = 0%
            plowkey$ = all(hex(00))
        del_loop
            call "PLOWNXT1" (#01, plowkey$, 0%, f1%(1%))
            if f1%(1%) = 0% then del_results
                get #01 using L13770, post_date$
L13770:              FMT POS(9), CH(6)
                if post_date$ > cut_off_date$ then del_loop
                     delete #01
                     del_cntr% = del_cntr% + 1%
                     goto del_loop
        del_results
            convert del_cntr% to errormsg$, pic(#######)
            errormsg$ = errormsg$ & " Sales Analysis Records Deleted."
            u3% = 0%
            call "ASKUSER" (u3%, "*** RESULTS OF DELETIONS ***",         ~
                            errormsg$, " ", "Press RETURN to Acknowledge")
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
            on fieldnr% gosub L20100,         /* Record Type            */~
                              L20150,         /* Posted Date            */~
                              L20200,         /* Sales Order            */~
                              L20250,         /* BOL Number             */~
                              L20300,         /* Invoice                */~
                              L20350,         /* Line Number            */~
                              L20400,         /* Ship Date              */~
                              L20450,         /* Adjustment Reason      */~
                              L20500,         /* Quantity               */~
                              L20550,         /* Gross Value            */~
                              L20600,         /* Net Value              */~
                              L20650          /* Standard Cost          */
            return
L20100: REM Def/Enable Record Type                 REC_TYPE$
            if new_record% = 0% then enabled% = 0%
            return

L20150: REM Def/Enable Posted Date                 POST_DATE$
            return

L20200: REM Def/Enable Sales Order                 SO$
            return

L20250: REM Def/Enable BOL Number                  BOL$
            return

L20300: REM Def/Enable Invoice                     INVOICE$
            return

L20350: REM Def/Enable Line Number                 LINE$
            return

L20400: REM Def/Enable Ship Date                   SHIP_DATE$
            return

L20450: REM Def/Enable Adjustment Reason           ADJ_REASON$
            return

L20500: REM Def/Enable Quantity                    QTY$
            return

L20550: REM Def/Enable Gross Value                 GROSS$
            return

L20600: REM Def/Enable Net Value                   NET$
            return

L20650: REM Def/Enable Standard Cost               STD_COST$
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L21100,         /* Stocked Part           */~
                              L21150,         /* Part Number            */~
                              L21200,         /* Category               */~
                              L21300,         /* Customer               */~
                              L21250,         /* Account X-Ref          */~
                              L21350,         /* Customer Type          */~
                              L21400,         /* Store                  */~
                              L21450,         /* Region                 */~
                              L21500,         /* Salesperson            */~
                              L21550,         /* Inventory Cost         */~
                              L21600,         /* Project Number         */~
                              L21650,         /* Sales Account          */~
                              L21700,         /* Fiscal Period/Start    */~
                              L21750          /* MDMC                   */
            return
L21100: REM Def/Enable Stocked Part (Y/N)          STOCKED_FLAG$
            return

L21150: REM Def/Enable Part Number                 PART$
            return

L21200: REM Def/Enable Category                    CAT$
            return

L21250: REM Def/Enable Account X-Ref               ACCOUNT_XREF$
            return

L21300: REM Def/Enable Customer                    CUS_CODE$
            return

L21350: REM Def/Enable Customer Type               CUS_TYPE$
            return

L21400: REM Def/Enable Store                       STR$
            return

L21450: REM Def/Enable Region                      REGION$
            return

L21500: REM Def/Enable Salesperson                 SALESPERSON$
            return

L21550: REM Def/Enable Inventory Cost              INV_COST$
            return

L21600: REM Def/Enable Project Number              PROJECT$
            return

L21650: REM Def/Enable Sales Account               ACCOUNT$
            return

L21700: REM Def/Enable Fiscal Period/Start         PERIOD$/FISCAL_START$
            return

L21750: REM Def/Enable MDMC                        MDMC$
            mdmc$ = " " : mdmc = 0
            if mgtrpt_on$ <> "Y" then enabled% = 0%
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  3  of Input. *~
            *************************************************************

        deffn'053(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L22100,         /* Currency Code/Eff Date */~
                              L22300,         /* Equiv/Units Factor     */~
                              L22500,         /* Price @ Stocking       */~
                              L22600          /* Price @ Pricing        */
            return
L22100: REM Def/Enable Currency Code/Effec Date    CURR$/CURR_DATE$
            return

L22300: REM Def/Enable Equiv/Units Factor          EQUIV$/PER_STAT$
            if curr$ = stat$ then enabled% = 0%
            return

L22500: REM Def/Enable Price @ Stocking UOM        PRICE_STOCK$
            if curr$ = stat$ then enabled% = 0%
            return

L22600: REM Def/Enable Price @ Pricing UOM         PRICE_PRICE$
            if curr$ = stat$ then enabled% = 0%
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            if scrnr% = 2% then restore line = scrn2_msg, fieldnr%
            if scrnr% = 3% then restore line = scrn3_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Record Type or '?' for selection list                  ",~
         "Enter Posted Date                                            ",~
         "Enter Sales Order                                            ",~
         "Enter BOL Number                                             ",~
         "Enter Invoice                                                ",~
         "Enter Line Item Sequence Number                              ",~
         "Enter Pre-Invoiced Ship Date; Enter blank if not pre-invoiced",~
         "Enter Adjustment Reason or '?' for selection list            ",~
         "Enter Quantity                                               ",~
         "Enter Gross Value                                            ",~
         "Enter Net Value                                              ",~
         "Enter Standard Cost                                          "

        scrn2_msg  :  data                                               ~
         "Enter Stocked Part (Y/N)                                     ",~
         "Enter Part Number or '?' for selection list                  ",~
         "Enter Category or '?' for selection list                     ",~
         "Enter Customer or '?' for selection list                     ",~
         "Enter Account Cross Reference or '?' for selection list      ",~
         "Enter Customer Type or '?' for selection list                ",~
         "Enter Store or '?' for selection list                        ",~
         "Enter Region or '?' for selection list                       ",~
         "Enter Salesperson or '?' for selection list                  ",~
         "Enter Inventory Cost                                         ",~
         "Enter Project Number                                         ",~
         "Enter Sales Account or '?' for selection list                ",~
         "Enter Fiscal Period and Fiscal Year Start Date               ",~
         "Enter Management Direct Manufacturing Cost                   "

        scrn3_msg  :  data                                               ~
         "Enter Currency Code or '?' and Exchange Rate Effectivity Date",~
         "Enter Rates; 1 Blank Calcs Other; Both Blank for System Rates",~
         "Enter Transaction Price @ Stocking UOM                       ",~
         "Enter Transaction Price @ Pricing UOM                        "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, cut_off_date$,             ~
                      account$, account_xref$, adj_reason$, bol$, cat$,  ~
                      curr$, curr_date$, cus_code$, cus_type$, equiv$,   ~
                      fiscal_start$, gross$, invoice$, inv_cost$,        ~
                      line$, net$, part$, period$, per_stat$, mdmc$,     ~
                      post_date$, price_price$, price_stock$, project$,  ~
                      qty$, rec_type$, region$, salesperson$,            ~
                      ship_date$, so$, std_cost$, stocked_flag$, str$
            new_record% = 1%
            qty, gross, net, std_cost, inv_cost, price_stock,            ~
            price_price, mdmc = 0
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            get #01 using L35030, rec_type$, post_date$, so$, bol$,       ~
                                 invoice$, line$, ship_date$,            ~
                                 adj_reason$, qty, gross, net, std_cost, ~
                                 stocked_flag$, part$, cat$,             ~
                                 account_xref$, cus_code$, cus_type$,    ~
                                 str$, region$, salesperson$, inv_cost,  ~
                                 curr$, curr_date%, conveqv, convunt,    ~
                                 price_stock, price_price, project$,     ~
                                 account$, period$, fiscal_start$, mdmc, ~
                                 temp$, temp$
            get #01 using L30172, date_time$
L30172:         FMT POS(2), CH(7)
            call "CONVERT" (qty,      2.2, qty$)
            call "CONVERT" (gross,    2.2, gross$)
            call "CONVERT" (net,      2.2, net$)
            call "CONVERT" (std_cost, 2.2, std_cost$)
            call "CONVERT" (inv_cost, 2.2, inv_cost$)
            equiv$, per_stat$, curr_date$ = " "
            if mc_on$ <> "Y" then L30250
                call "CONVERT" (conveqv , 7.7, equiv$)
                call "CONVERT" (convunt , 7.7, per_stat$)
                convert curr_date% to curr_date$, pic(00000000)
                call "DATECONV" (curr_date$)
                call "CONVERT" (price_stock, 4.4, price_stock$)
                call "CONVERT" (price_price, 4.4, price_price$)
L30250:     mdmc$ = " "
            if mdmc > 2e10 then mdmc = 0
            if mgtrpt_on$ <> "Y" then L30270
                call "CONVERT" (mdmc    , 2.2, mdmc$)
L30270:     call "GLFMT" (account$)
            call "DATEFMT" (curr_date$)
            call "DATEFMT" (post_date$)
            call "DATEFMT" (ship_date$)
            call "DATEFMT" (fiscal_start$)
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "GLUNFMT" (account$)
            call "DATUNFMT" (post_date$)
            call "DATUNFMT" (ship_date$)
            call "DATUNFMT" (fiscal_start$)
            f1%(1%) = 0%

            if new_record% = 1% then L31100
                plowkey$ = str(rec_type$) & date_time$
                call "READ101" (#01, plowkey$, f1%(1%))

L31100:     put #01 using L35030, rec_type$, post_date$, so$, bol$,       ~
                                 invoice$, line$, ship_date$,            ~
                                 adj_reason$, qty, gross, net, std_cost, ~
                                 stocked_flag$, part$, cat$,             ~
                                 account_xref$, cus_code$, cus_type$,    ~
                                 str$, region$, salesperson$, inv_cost,  ~
                                 curr$, curr_date%, conveqv, convunt,    ~
                                 price_stock, price_price, project$,     ~
                                 account$, period$, fiscal_start$, mdmc, ~
                                 userid$, " "

L31210:     if new_record% = 1% then call "GETDTTM" addr(date_time$)
            put #01 using L31230, date_time$
L31230:         FMT POS(2), CH(7)
            if new_record% = 0% or f1%(1%) = 1% then L31280
                write #01, eod goto L31210
                goto L31990
L31280:     rewrite #01
L31990:     return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: SADETAIL                          */~
            CH(1),          /* Record Type Identifier for Multi-Record */~
            XX(7),          /* Date and time stored as binary for recor*/~
            CH(6),          /* Date Posted                             */~
            CH(16),         /* sales order number                      */~
            CH(3),          /* Bill of Lading Number                   */~
            CH(8),          /* Invoice Number                          */~
            CH(3),          /* Sequence Number                         */~
            CH(6),          /* Pre-Invoicing Ship Date.                */~
            CH(9),          /* Adjustment Reason Code                  */~
            PD(14,4),       /* VF Unit of Measure                      */~
            PD(14,4),       /* Value                                   */~
            PD(14,4),       /* Value                                   */~
            PD(14,4),       /* Cost field                              */~
            CH(1),          /* Stocked Item Flag (Y/N)                 */~
            CH(25),         /* Part code                               */~
            CH(4),          /* category code                           */~
            CH(9),          /* Acccount Cross Reference.               */~
            CH(9),          /* Customer Code                           */~
            CH(2),          /* customer type                           */~
            CH(3),          /* Warehouse or Store                      */~
            CH(4),          /* region code                             */~
            CH(4),          /* Salesman Id                             */~
            PD(14,4),       /* Cost field                              */~
            CH(4),          /* Currency code.                          */~
            BI(4),          /* Factor Effective Date                   */~
            PD(14,7),       /* Conversion Factor                       */~
            PD(14,7),       /* Currency Units Per Statutory Units      */~
            PD(14,4),       /* Unit price                              */~
            PD(14,4),       /* Unit price                              */~
            CH(8),          /* Project Number                          */~
            CH(9),          /* G/L account number                      */~
            CH(2),          /* Fiscal Period Number                    */~
            CH(6),          /* User input Start Date Range             */~
            PD(14,4),       /* Management Direct Manufacturing Cost    */~
            CH(3),          /* User who changed something              */~
            CH(64)          /* Unused Space                            */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              str(line2$,,22%) = " "
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40130,         /* Record Type       */   ~
                                L40130,         /* Posted Date       */   ~
                                L40130,         /* Sales Order       */   ~
                                L40130,         /* BOL Number        */   ~
                                L40130,         /* Invoice           */   ~
                                L40130,         /* Line Number       */   ~
                                L40130,         /* Ship Date         */   ~
                                L40130,         /* Adjustment Reason */   ~
                                L40135,         /* Quantity          */   ~
                                L40135,         /* Gross Value       */   ~
                                L40135,         /* Net Value         */   ~
                                L40135          /* Standard Cost     */
              goto L40145

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40130:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40135:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40145:     accept                                                       ~
               at (01,02),                                               ~
                  "Sales Analysis Detail (Only) Editor - Page 1",        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Record Type",                                ~
               at (06,30), fac(lfac$( 1%)), rec_type$           , ch(01),~
                                                                         ~
               at (07,02), "Posted Date",                                ~
               at (07,30), fac(lfac$( 2%)), post_date$          , ch(08),~
                                                                         ~
               at (08,02), "Sales Order",                                ~
               at (08,30), fac(lfac$( 3%)), so$                 , ch(16),~
                                                                         ~
               at (09,02), "BOL Number",                                 ~
               at (09,30), fac(lfac$( 4%)), bol$                , ch(03),~
                                                                         ~
               at (10,02), "Invoice",                                    ~
               at (10,30), fac(lfac$( 5%)), invoice$            , ch(08),~
                                                                         ~
               at (11,02), "Line Seq. Number",                           ~
               at (11,30), fac(lfac$( 6%)), line$               , ch(03),~
                                                                         ~
               at (12,02), "Ship Date",                                  ~
               at (12,30), fac(lfac$( 7%)), ship_date$          , ch(08),~
                                                                         ~
               at (13,02), "Adjustment Reason",                          ~
               at (13,30), fac(lfac$( 8%)), adj_reason$         , ch(09),~
                                                                         ~
               at (14,02), "Quantity",                                   ~
               at (14,30), fac(lfac$( 9%)), qty$                , ch(10),~
                                                                         ~
               at (15,02), "Gross Value",                                ~
               at (15,30), fac(lfac$(10%)), gross$              , ch(10),~
                                                                         ~
               at (16,02), "Net Value",                                  ~
               at (16,30), fac(lfac$(11%)), net$                , ch(10),~
                                                                         ~
               at (17,02), "Standard Cost",                              ~
               at (17,30), fac(lfac$(12%)), std_cost$           , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40420
                  call "MANUAL" ("SADEDITS") : goto L40145

L40420:        if keyhit% <> 15% then L40435
                  call "PRNTSCRN" : goto L40145

L40435:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40530     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                 (8)Find Record         " &        ~
                     "(12)Delete Many        (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffff08ffffff0c0dff0f1000)
            if fieldnr% = 1% then L40510
                str(pf$(3%),41%,15%) = " " : str(pfkeys$,12%,1%) = hex(ff)
                str(pf$(3%),64%)     = " " : str(pfkeys$,16%,1%) = hex(ff)
                str(pf$(3%),18%,14%) = " " : str(pfkeys$, 8%,1%) = hex(ff)
L40510:     if fieldnr% > 1% then L40520
                str(pf$(2%),18%,26%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L40520:     return

L40530: if fieldnr% > 0% then L40575  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                 (5)Next Screen         " &        ~
                     "(12)Delete Record      (16)Save Data   "
            pfkeys$ = hex(01ffffff05ffffffffffff0c0dff0f1000)
            return
L40575:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'050(2%, fieldnr%)
              gosub set_pf2
              str(line2$,,22%) = " "
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L41140,         /* Stocked Part      */   ~
                                L41140,         /* Part Number       */   ~
                                L41140,         /* Category          */   ~
                                L41140,         /* Customer          */   ~
                                L41140,         /* Account X-Ref     */   ~
                                L41140,         /* Customer Type     */   ~
                                L41140,         /* Store             */   ~
                                L41140,         /* Region            */   ~
                                L41140,         /* Salesperson       */   ~
                                L41145,         /* Inventory Cost    */   ~
                                L41140,         /* Project Number    */   ~
                                L41140,         /* Sales Account     */   ~
                                L41140,         /* Fiscal Period/Strt*/   ~
                                L41145          /* MDMC              */
              goto L41155

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L41140:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L41145:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L41155:     accept                                                       ~
               at (01,02),                                               ~
                  "Sales Analysis Detail (Only) Editor - Page 2",        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Stocked Part (Y/N)",                         ~
               at (06,30), fac(lfac$( 1%)), stocked_flag$       , ch(01),~
                                                                         ~
               at (07,02), "Part Number",                                ~
               at (07,30), fac(lfac$( 2%)), part$               , ch(25),~
                                                                         ~
               at (08,02), "Category",                                   ~
               at (08,30), fac(lfac$( 3%)), cat$                , ch(04),~
                                                                         ~
               at (09,02), "Customer     ",                              ~
               at (09,30), fac(lfac$( 4%)), cus_code$           , ch(09),~
                                                                         ~
               at (10,02), "Account X-Ref",                              ~
               at (10,30), fac(lfac$( 5%)), account_xref$       , ch(09),~
                                                                         ~
               at (11,02), "Customer Type",                              ~
               at (11,30), fac(lfac$( 6%)), cus_type$           , ch(02),~
                                                                         ~
               at (12,02), "Store",                                      ~
               at (12,30), fac(lfac$( 7%)), str$                , ch(03),~
                                                                         ~
               at (13,02), "Region",                                     ~
               at (13,30), fac(lfac$( 8%)), region$             , ch(04),~
                                                                         ~
               at (14,02), "Salesperson",                                ~
               at (14,30), fac(lfac$( 9%)), salesperson$        , ch(04),~
                                                                         ~
               at (15,02), "Inventory Cost",                             ~
               at (15,30), fac(lfac$(10%)), inv_cost$           , ch(10),~
                                                                         ~
               at (16,02), "Project Number",                             ~
               at (16,30), fac(lfac$(11%)), project$            , ch(08),~
                                                                         ~
               at (17,02), "Sales Account",                              ~
               at (17,30), fac(lfac$(12%)), account$            , ch(12),~
                                                                         ~
               at (18,02), "Fiscal Period / Start Date",                 ~
               at (18,30), fac(lfac$(13%)), period$             , ch(02),~
               at (18,34), fac(lfac$(13%)), fiscal_start$       , ch(08),~
                                                                         ~
               at (19,02), "Management Direct Mfg Cost",                 ~
               at (19,30), fac(lfac$(14%)), mdmc$               , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L41460
                  call "MANUAL" ("SADEDITS") : goto L41155

L41460:        if keyhit% <> 15% then L41475
                  call "PRNTSCRN" : goto L41155

L41475:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L41570     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L41550
                str(pf$(3%),64%)     = " " : str(pfkeys$,16%,1%) = hex(ff)
L41550:     if fieldnr% > 1% then L41560
                str(pf$(2%),18%,26%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L41560:     return

L41570: if fieldnr% > 0% then L41615  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                 (4)Previous Screen     " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                 (5)Next Screen         " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffff0405ffffffffffffff0dff0f1000)
            if mc_on$ = "Y" then L41610
                str(pf$(3%),18%,14%) = " " : str(pfkeys$, 5%,1%) = hex(ff)
L41610:     return
L41615:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
              gosub'050(3%, fieldnr%)
              gosub set_pf3
              str(line2$,,22%) = "Currency Specific Data"
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42100,         /* Currency Code/Date*/   ~
                                L42105,         /* Equiv/Units Factor*/   ~
                                L42105,         /* Price @ Stocking  */   ~
                                L42105          /* Price @ Pricing   */
              goto L42115

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42100:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L42105:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42115:     accept                                                       ~
               at (01,02),                                               ~
                  "Sales Analysis Detail (Only) Editor - Page 3",        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Currency Code",                              ~
               at (06,30), fac(lfac$( 1%)), curr$               , ch(04),~
                                                                         ~
               at (07,02), "Date Effective",                             ~
               at (07,30), fac(lfac$( 1%)), curr_date$          , ch(08),~
                                                                         ~
               at (08,02), "Equivalency Factor",                         ~
               at (08,30), fac(lfac$( 2%)), equiv$              , ch(15),~
                                                                         ~
               at (09,02), "Units/Statutory",                            ~
               at (09,30), fac(lfac$( 2%)), per_stat$           , ch(15),~
                                                                         ~
               at (10,02), "Price @ Stocking UOM",                       ~
               at (10,30), fac(lfac$( 3%)), price_stock$        , ch(10),~
                                                                         ~
               at (11,02), "Price @ Pricing UOM",                        ~
               at (11,30), fac(lfac$( 4%)), price_price$        , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L42300
                  call "MANUAL" ("SADEDITS") : goto L42115

L42300:        if keyhit% <> 15% then L42315
                  call "PRNTSCRN" : goto L42115

L42315:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf3
        if edit% = 2% then L42410     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L42390
                str(pf$(3%),64%)     = " " : str(pfkeys$,16%,1%) = hex(ff)
L42390:     if fieldnr% > 1% then L42400
                str(pf$(2%),18%,26%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L42400:     return

L42410: if fieldnr% > 0% then L42455  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                 (4)Previous Screen     " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            return
L42455:                              /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   E X T R A           *~
            *-----------------------------------------------------------*~
            * Input Parameters to Select Existing SA Record.            *~
            *************************************************************

        find_parameters_screen
              inpmessage$ = "Enter Parameters for Search.  Blanks are " &~
                            "valid; '?' for List."
              gosub set_pf4
              str(line2$,,22%) = " "
              init(hex(81)) lfac$()

L43290:     accept                                                       ~
               at (01,02),                                               ~
                  "Sales Analysis Detail (Only) Editor - Search Parameter~
        ~s",                                                              ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Record Type",                                ~
               at (06,30), fac(lfac$( 1%)), rec_type$           , ch(01),~
                                                                         ~
               at (07,02), "Posted Date",                                ~
               at (07,30), fac(lfac$( 2%)), post_date$          , ch(08),~
                                                                         ~
               at (08,02), "Sales Order",                                ~
               at (08,30), fac(lfac$( 3%)), so$                 , ch(16),~
                                                                         ~
               at (09,02), "BOL Number",                                 ~
               at (09,30), fac(lfac$( 4%)), bol$                , ch(03),~
                                                                         ~
               at (10,02), "Invoice",                                    ~
               at (10,30), fac(lfac$( 5%)), invoice$            , ch(08),~
                                                                         ~
               at (11,02), "Line Number",                                ~
               at (11,30), fac(lfac$( 6%)), line$               , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L43830
                  call "MANUAL" ("SADEDITS") : goto L43290

L43830:        if keyhit% <> 15% then L43860
                  call "PRNTSCRN" : goto L43290

L43860:        return

        set_pf4
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Return      "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* Record Type            */~
                              L50150,         /* Posted Date            */~
                              L50200,         /* Sales Order            */~
                              L50250,         /* BOL Number             */~
                              L50300,         /* Invoice                */~
                              L50350,         /* Line Number            */~
                              L50400,         /* Ship Date              */~
                              L50450,         /* Adjustment Reason      */~
                              L50500,         /* Quantity               */~
                              L50550,         /* Gross Value            */~
                              L50600,         /* Net Value              */~
                              L50650          /* Standard Cost          */
            return
L50100: REM Test for Record Type                  REC_TYPE$
            if new_record% = 0% and rec_type$ = " " then return
                if new_record% = 1% and rec_type$ = " " then rec_type$ = ~
                                                                  hex(00)
                if rec_type$ = "?" then rec_type$ = hex(00)
                mat incl_excl = zer : init (" ") incl_excl$()
                if new_record% = 0% then L50125
                    incl_excl(1%) = -1.01 : incl_excl$(1%) = " "
                    call "DATUNFMT" (incl_excl$(1%))
L50125:         descr$ = hex(06) & "Select Record Type."
                call "PLOWCODE" (#50, rec_type$, descr$, 5000%, .5,      ~
                      f1%(50%), " ", 0.0, 0.0, incl_excl(), incl_excl$())
                if f1%(50%) = 1% then L50145
                    errormsg$ = "Invalid Record Type.  Select again."
L50145:         return

L50150: REM Test for Posted Date                  POST_DATE$
            if new_record% = 0% and ~
              (post_date$ = " " or post_date$ = blankdate$) then return
                call "DATEOK" (post_date$, temp%, errormsg$)
            return

L50200: REM Test for Sales Order                  SO$
            return

L50250: REM Test for BOL Number                   BOL$
            if bol$ = " " then return
                convert bol$ to temp%, data goto L50280
                call "STRING" addr ("RJ", bol$, 3%, bol$)
                if so$ <> " " then return
                     errormsg$ = "A non-blank BOL Requires a non-blank "&~
                                 "Sales Order." : return
L50280:              errormsg$ = "Invalid BOL Number"
            return

L50300: REM Test for Invoice                      INVOICE$
            return

L50350: REM Test for Line Number                  LINE$
            if line$ = " " then L50382
                convert line$ to temp%, data goto L50380
                call "STRING" addr ("RJ", line$, 3%, line$)
                if invoice$ <> " " or so$ <> " " then return
                     errormsg$ = "A non-blank Line Requires a non-blank"&~
                                 " SO or Invoice." : return
L50380:              errormsg$ = "Invalid Line Number" : return
L50382:     if invoice$ = " " and so$ = " " then return
                errormsg$ = "Line Number Must be Specified"
            return

L50400: REM Test for Ship Date                    SHIP_DATE$
            if ship_date$ = " " or ship_date$ = blankdate$ then return
            call "DATEOK" (ship_date$, temp%, errormsg$)
            return

L50450: REM Test for Adjustment Reason            ADJ_REASON$
            if adj_reason_on_file% + can_reason_on_file% = 0% then return
            if adj_reason$ = " " then return
                descr$ = hex(06) & "Select Adjust Reason Code or " &     ~
                                   "PF16 for Cancellation Reasons."
                plowkey$ = "SO REASON" & adj_reason$
                call "PLOWCODE" (#03, plowkey$, descr$, 9%, 0.3, f1%(3%))
                if f1%(3%) = 1% then adj_reason$ = str(plowkey$,10)      ~
                                else L50480
                    return     /* Valid Adj Reason so return */
L50480:         descr$ = hex(06) & "Select Cancellation Reason Code"
                plowkey$ = "CANREASON" & adj_reason$
                call "PLOWCODE" (#03, plowkey$, descr$, 9%, 0.3, f1%(3%))
                if f1%(3%) = 1% then adj_reason$ = str(plowkey$,10)
            return

L50500: REM Test for Quantity                     QTY$
            if qty$ = " " then qty$ = "0"
            convert qty$ to qty, data goto L50515 : goto L50520
L50515:         errormsg$ = "Invalid Quantity." : return
L50520:     call "CONVERT" (qty, 2.2, qty$)
            return

L50550: REM Test for Gross Value                  GROSS$
            if gross$ = " " then gross$ = "0"
            convert gross$ to gross, data goto L50580 : goto L50590
L50580:         errormsg$ = "Invalid Gross Amount." : return
L50590:     call "CONVERT" (gross, 2.2, gross$)
            return

L50600: REM Test for Net Value                    NET$
            if net$ = " " then net$ = "0"
            convert net$ to net, data goto L50630 : goto L50640
L50630:         errormsg$ = "Invalid Net Amount." : return
L50640:     call "CONVERT" (net, 2.2, net$)
            return

L50650: REM Test for Standard Cost                STD_COST$
            if std_cost$ = " " then std_cost$ = "0"
            convert std_cost$ to std_cost, data goto L50680
            if std_cost >= 0 then L50690
                errormsg$ = "Invalid Standard Cost; Can't be Negative."
                return
L50680:         errormsg$ = "Invalid Standard Cost." : return
L50690:     call "CONVERT" (std_cost, 2.2, std_cost$)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51100,         /* Stocked Part           */~
                              L51150,         /* Part Number            */~
                              L51200,         /* Category               */~
                              L51300,         /* Customer               */~
                              L51250,         /* Account X-Ref          */~
                              L51350,         /* Customer Type          */~
                              L51400,         /* Store                  */~
                              L51450,         /* Region                 */~
                              L51500,         /* Salesperson            */~
                              L51550,         /* Inventory Cost         */~
                              L51600,         /* Project Number         */~
                              L51650,         /* Sales Account          */~
                              L51700,         /* Fiscal Period/Start    */~
                              L51750          /* MDMC                   */
            return
L51100: REM Test for Stocked Part (Y/N)           STOCKED_FLAG$
            if stocked_flag$ = "Y" or stocked_flag$ = "N" then return
                errormsg$ = "Invalid Stocked Part Flag; Enter 'Y' or 'N'."
            return

L51150: REM Test for Part Number                  PART$
            if stocked_flag$ = "N" then return
                descr$ = hex(06) & "Select Part Number"
                call "GETCODE" (#09, part$, descr$, 0%, 0, f1%(9%))
                if f1%(9%) = 1% then L51180
                    errormsg$ = "Part Number Not on File." : return
L51180:         get #09 using L51185, cat$
L51185:              FMT POS(90), CH(4)
            return

L51200: REM Test for Category                     CAT$
            if cat$ = " " then return
                descr$ = hex(06) & "Select Part Category"
                call "GETCODE" (#04, cat$, descr$, 0%, 0, f1%(4%))
                if f1%(4%) = 1% then return
                    errormsg$ = "Category Not on File."
            return

L51250: REM Test for Account X-Ref                ACCOUNT_XREF$
            descr$ = hex(06) & "Select Customer Account Cross Reference"
            call "GETCODE" (#05, account_xref$, descr$, 0%, 1, f1%(5%))
            if f1%(5%) = 1% then return
                errormsg$ = "Account Cross Reference Not on File."
            return

L51300: REM Test for Customer                     CUS_CODE$
            descr$ = hex(06) & "Select Customer Code"
            call "GETCODE" (#05, cus_code$, descr$, 0%, 1, f1%(5%))
            if f1%(5%) = 1% then L51325
                errormsg$ = "Customer Not on File." : return
L51325:     get #05 using L51330, account_xref$, cus_type$, curr$
L51330:         FMT POS(771), CH(9), POS(1023), CH(2), POS(1045), CH(4)
            return

L51350: REM Test for Customer Type                CUS_TYPE$
            if cus_type$ = " " then return
            if cus_type_on_file% = 0% then return
                descr$ = hex(06) & "Select Customer Type"
                plowkey$ = "CUS TYPES" & cus_type$
                call "PLOWCODE" (#03, plowkey$, descr$, 9%, .30, f1%(3%))
                if f1%(3%) = 1% then L51390
                    errormsg$ = "Customer Type Code not on file." : return
L51390:         cus_type$ = str(plowkey$,10)
            return

L51400: REM Test for Store                        STR$
            descr$ = hex(06) & "Select Store Code"
            call "GETCODE" (#06, str$, descr$, 0%, 0, f1%(6%))
            if f1%(6%) = 1% then return
                errormsg$ = "Store Code Not on File."
            return

L51450: REM Test for Region                       REGION$
            if region$ = " " then return
            if region_on_file% = 0% then return
                descr$ = hex(06) & "Select Sales Region"
                plowkey$ = "REGIONS  " & region$
                call "PLOWCODE" (#03, plowkey$, descr$, 9%, .3, f1%(3%))
                if f1%(3%) = 1% then L51485
                     errormsg$ = "Sales Region Code Not on File." : return
L51485:         region$ = str(plowkey$,10)
            return

L51500: REM Test for Salesperson                  SALESPERSON$
            if salesperson$ = " " then return
                descr$ = hex(06) & "Select Salesperson"
                call "GETCODE" (#07, salesperson$, descr$, 0%, 0, f1%(7%))
                if f1%(7) = 1% then return
                    errormsg$ = "Salesperson Not on File."
            return

L51550: REM Test for Inventory Cost               INV_COST$
            if inv_cost$ = " " then inv_cost$ = "0"
            convert inv_cost$ to inv_cost, data goto L51580
            call "CONVERT" (inv_cost, 2.2, inv_cost$)
            return
L51580:         errormsg$ = "Invalid Inventory Cost."
            return

L51600: REM Test for Project Number               PROJECT$
            return

L51650: REM Test for Sales Account                ACCOUNT$
            if account$ = " " then return
                descr$ = hex(06) & "Select Sales Account."
                call "GETCODE" (#08, account$, descr$, 0%, 0, f1%(8%))
                if f1%(8%) = 1% then return
                    errormsg$ = "Invalid Sales Account."
            return

L51700: REM Test for Fiscal Period/Start          PERIOD$/FISCAL_START$
            if period$ = " " and ~
              (fiscal_start$ = " " or fiscal_start$ = blankdate$) then return
                convert period$ to period%, data goto L51710 : goto L51715
L51710:             errormsg$ = "Invalid Fiscal Period; Enter '01' " &   ~
                                "to '13'"
                    return
L51715:         if period% < 1% or period% > 13% then L51710
                convert period% to period$, pic(00)
                call "DATEOK" (fiscal_start$, temp%, errormsg$)
            return

L51750: REM Test for MDMC                         MDMC$
            if enabled% = 0% then return
                if mdmc$ = " " then mdmc$ = "0"
                convert mdmc$ to mdmc, data goto L51780
                if mdmc >= 0 then L51785
                    errormsg$ = "Invalid MDMC Cost; Can't be Negative."
                    return
L51780:             errormsg$ = "Invalid MDMC Cost." : return
L51785:         call "CONVERT" (mdmc, 2.2, mdmc$)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 3.                      *~
            *************************************************************

        deffn'153(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L52100,         /* Currency Code/Eff Date */~
                              L52300,         /* Equiv/Units Factr      */~
                              L52500,         /* Price @ Stocking       */~
                              L52600          /* Price @ Pricing        */
            return
L52100: REM Test for Currency Code                CURR$
            if curr$ = " " then curr$ = stat$
            if curr$ <> stat$ then L52120
                equiv$, per_stat$ = "      1.0000000"
                conveqv, convunt = 1
                curr_date$ = " " : curr_date% = 0%
                price_stock, price_price = 0
                price_stock$, price_price$ = "0.00"
                return
L52120:     descr$ = hex(06) & "Select Currency Code"
            call "GETCODE" (#40, curr$, descr$, 0%, 0, f1%(40%))
            if f1%(40%) <> 0% then L52160
                errormsg$ = "Invalid Currency Code." : return

L52160: REM Test for Date Effective               CURR_DATE$
            if curr_date$ = " " or curr_date$ = blankdate$ then ~
               curr_date$ = post_date$
            call "DATEOK" (curr_date$, curr_date%, errormsg$)
            if errormsg$ <> " " then return
            if equiv$ = " " and per_stat$ = " " then gosub calc_curr
            return

        calc_curr
*        Capture Effectivity Date and Exchange Rates (possibly)
/*            rev_date$ = curr_date$                                */
            conveqv, convunt = 1 : curr_date% = 0%

            call "DATREVRS" (curr_date$, rev_date$, errormsg$)

/*            call "DATEOK" (rev_date$, rev%, errormsg$)            */
/*            convert (999999% - rev%) to rev_date$, pic(000000)    */

            currkey$ = str(curr_type$) & str(curr$) & str(rev_date$,1%,6%)
            call "PLOWNEXT" (#42, currkey$, 5%, f1%(42%))
            if f1%(42%) = 0% then L52250
                get #42 using L52245, curr_date$, conveqv, convunt
L52245:             FMT POS(12), CH(6), 2*PD(14,7)

                call "DATEOK" (curr_date$, curr_date%, errormsg$)
L52250:     call "CONVERT" (conveqv, 7.7, equiv$)
            call "CONVERT" (convunt, 7.7, per_stat$)
            return

L52300: REM Test for Equiv/Units Factor           EQUIV$/PER_STAT$
            if enabled% = 0% then return
                if equiv$ = " " and per_stat$ = " " then gosub calc_curr
                if equiv$ <> " " then L52370
                     conveqv = round(1/convunt, 7)
                     call "CONVERT" (conveqv, 7.7, equiv$)
L52370:         convert equiv$ to conveqv, data goto L52470
                if conveqv > 0 then L52400
L52390:             errormsg$ = "Invalid Factor; Can't be Neg. or Zero."
                    return
L52400:         call "CONVERT" (conveqv, 7.7, equiv$)
                if per_stat$ <> " " then L52430
                     convunt = round(1/conveqv, 7)
                     call "CONVERT" (convunt, 7.7, per_stat$)
L52430:         convert per_stat$ to convunt, data goto L52470
                if convunt > 0 then L52480
                    goto L52390
L52470:             errormsg$ = "Invalid Factor." : return
L52480:         call "CONVERT" (convunt, 7.7, per_stat$)
                if round(conveqv * convunt, 6) = 1.0 then return
                     errormsg$ = "Conversion Rate Factors must be the "& ~
                                 "Inverse of one another."
            return

L52500: REM Test for Price @ Stocking UOM         PRICE_STOCK$
            if enabled% = 0% then return
                if price_stock$ = " " then price_stock$ = "0"
                convert price_stock$ to price_stock, data goto L52570
                if price_stock >= 0 then L52580
                    errormsg$ = "Invalid Price; Can't be Negative."
                    return
L52570:             errormsg$ = "Invalid Price." : return
L52580:         call "CONVERT" (price_stock, 2.4, price_stock$)
            return

L52600: REM Test for Price @ Pricing UOM          PRICE_PRICE$
            if enabled% = 0% then return
                if price_price$ = " " then price_price$ = "0"
                convert price_price$ to price_price, data goto L52670
                if price_price >= 0 then L52680
                    errormsg$ = "Invalid Price; Can't be Negative."
                    return
L52670:             errormsg$ = "Invalid Price." : return
L52680:         call "CONVERT" (price_price, 2.4, price_price$)
            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
