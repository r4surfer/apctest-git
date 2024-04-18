        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y  L       OOO    CCC   PPPP   TTTTT   *~
            *  H   H  NN  N  Y   Y  L      O   O  C   C  P   P    T     *~
            *  HHHHH  N N N   YYY   L      O   O  C      PPPP     T     *~
            *  H   H  N  NN    Y    L      O   O  C   C  P        T     *~
            *  H   H  N   N    Y    LLLLL   OOO    CCC   P        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYLOCPT - Prints Warehouse (Store) Map in Location order.*~
            *            Operator has Option to Print all Stores or     *~
            *            range and all locations or a range of          *~
            *            locations.  Part data at a location can be     *~
            *            included or excluded as well as HNYQUAN data.  *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/30/90 ! Original                                 ! JEF *~
            * 02/28/90 ! Fixed Misc. Bugs.                        ! SID *~
            * 08/26/91 ! Fixed Store and Location Printing Range  ! SID *~
            * 09/04/91 ! Added a Report ID (HNY041)               ! SID *~
            * 01/12/93 ! Page 0 Facs, Header, & End Report Time.  ! RJH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            beg_location$(2)8,           /* Beginning location         */~
            beg_store$(2)3,              /* Beginning store            */~
            col_head1$132,               /* Report Column Headings     */~
            col_head2$132,               /* Report Column Headings     */~
            columnttl$39,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            comment$(2)30,               /* Comment fields - LOCATION  */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            end_location$(2)8,           /* Ending Location            */~
            end_store$(2)3,              /* Ending Store               */~
            errormsg$79,                 /* Error message              */~
            hyphen$60,                   /* Hyphens for underlining    */~
            inc_qty$1,                   /* Include Inventory Qty data */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            linesave$132,                /* Save Detail Print Line     */~
            location$8,                  /* Current Location Code      */~
            loc_descr$30,                /* Location Description       */~
            lot$6,                       /* Lot Code                   */~
            old_location$8,              /* Last Location printed      */~
            old_part$25,                 /* Last Part Number printed   */~
            old_store$3,                 /* Last Store printed         */~
            partdata$1,                  /* Include part data          */~
            part$25,                     /* Part number                */~
            part_descr$32,               /* Part description           */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            printline$132,               /* Line to be Printed         */~
            rpttitle$60,                 /* Report Title               */~
            store$3,                     /* Store Name                 */~
            storename$30,                /* Store Name                 */~
            temp_location$8,             /* Dummy Location for printing*/~
            temp_store$3,                /* Dummy Store for printing   */~
            time$8,                      /* System Time                */~
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
            cms2v$ = "R6.02.03 02/16/93 Customer Credit & Core Trackng  "
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
            * #01 ! HNYLOCNS ! Stock location master file               *~
            * #02 ! LOCATION ! Store/ location master file              *~
            * #03 ! STORNAME ! STORE INFORMATION FILE                   *~
            * #04 ! HNYMASTR ! Inventory Master File                    *~
            * #06 ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYLOCNS",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =  443, keylen =  42,         ~
                            key  2, keypos =  485, keylen =  42,         ~
                            key  3, keypos =  527, keylen =  42,         ~
                            key  4, keypos =  590, keylen =  42          ~

            select #02, "LOCATION",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    1, keylen =  11,                     ~
                        alt key  1, keypos =    4, keylen =  11          ~

            select #03, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3                      ~

            select #04, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup     ~

            select #06, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))
            call "OPENCHCK" (#06, fs%(06), f2%(06), 0%, rslt$(06))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, ret%)
            ret% = 0
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "WAREHOUSE / LOCATION MAP REPORT"
            call "STRING" addr("CT", company$, 60%, company$)
            call "STRING" addr("CT", rpttitle$, 60%, rpttitle$)

            str(columnttl$, 1) = "Beginning Code"
            str(columnttl$,20) = "Ending Code"

            str(line2$,62) = "HNYLOCPT: " & str(cms2v$,,8)

            init ("-")  hyphen$

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
                  if keyhit%  = 16% then       extract_data
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
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
        extract_data

*        Set-up column headings lines for report printing

            col_head1$ = "STORE  LOCATION  LOCATION DESCRIPTION"
            col_head2$ = "-----  --------  --------------------" &       ~
                         "----------"
            if partdata$ = "Y" then str(col_head1$,50) = "PART NUMBER"
            if partdata$ = "Y" then str(col_head2$,50) = str(hyphen$,,25)
            if partdata$ = "Y" then str(col_head1$,77) = "DESCRIPTION"
            if partdata$ = "Y" then str(col_head2$,77) = str(hyphen$,,32)
            if partdata$ = "Y" then str(col_head1$,111) = "LOT"
            if partdata$ = "Y" then str(col_head2$,111) = "------"
            if inc_qty$  = "Y" then str(col_head1$,118) = "LOCATION QTY"
            if inc_qty$  = "Y" then str(col_head2$,118) = "------------"
            if partdata$ = "N" then str(col_head1$,50)  = "COMMENTS"
            if partdata$ = "N" then str(col_head2$,50)  = hyphen$

            goto generate_report

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Store Number           */~
                              L20200,         /* Location               */~
                              L20300,         /* Include Part Info      */~
                              L20400          /* Include Inventory      */
            return
L20100: REM Def/Enable Store Number                BEG_STORE$
            if beg_store$(1)    = " " then beg_store$(1)    = "ALL"
            return

L20200: REM Def/Enable Location                    BEG_LOCATION$
            if beg_location$(1) = " " then beg_location$(1) = "ALL"
            return

L20300: REM Def/Enable Include Part Information    PARTDATA$
            if partdata$        = " " then partdata$        = "Y"
            return

L20400: REM Def/Enable Include Inventory Qty data  INC_QTY$
            if partdata$        = "N" then inc_qty$         = "N"
            if partdata$        = "N" then enabled%         = 0%
            if inc_qty$         = " " then inc_qty$         = "Y"

            return

L22530:
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
         "Enter Store Number Range to be Included or 'ALL'.            ",~
         "Enter Location Range to be Included or 'ALL'.                ",~
         "Enter 'Y' to Include Part/Lot Data or 'N' to Exclude It.     ",~
         "Enter 'Y' to Include Inventory Qty Data or 'N' to Exclude it."

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      inc_qty$, beg_location$(), end_location$(),        ~
                      partdata$, beg_store$(), end_store$(),             ~
                      col_head1$, col_head2$,                            ~
                      old_store$, old_location$, old_part$
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
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
            call "SHOSTAT" ("Report Generation in Progress")
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("HNY041", " ", 0%, 0%)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */

*       * Report Generation Logic goes here
        REM GET HNYLOCNS RECORD
            plowkey$ = str(beg_store$(2)) & str(beg_location$(2)) &      ~
                                            hex(00)
        read_loop
            call "PLOWNEXT" (#1, plowkey$, 0, f1%(1))
            if f1%(1) <> 0 then L30180
                printline$ = "* * * END OF STORE " & old_store$ & " * * *"
                call "STRING" addr ("CT", printline$, 132%)
                print skip(2) : lcntr% = lcntr% + 2%
                gosub print_line
                goto end_report

L30180:     get #1, using L30190, store$, location$, part$, lot$, quantity
L30190:         FMT CH(3), CH(8), CH(25), CH(6), POS(573), PD(14,4)

            if store$ < beg_store$(2) then read_loop
            if store$ > end_store$(2) then end_report
            if store$ <> old_store$ then gosub store_break

            if (location$ < beg_location$(2)) or                         ~
               (location$ > end_location$(2)) then read_loop

            temp_store$ = store$ : temp_location$ = location$

            if partdata$ = "N" and location$ = old_location$ then        ~
                               read_loop
            if location$ <> old_location$ then gosub location_break

            if part$ <> old_part$ then gosub part_break
            if lcntr% > 56% then gosub page_head
            if store$ <> old_store$ then printline$ = store$
            if location$ <> old_location$ then str(printline$,8)         ~
                         = str(location$) & "  " & str(loc_descr$)
            if partdata$ = "Y" then L30380
                str(printline$,50) = str(comment$(1)) & " " &            ~
                                                  str(comment$(2))
                goto L30430
L30380:     if part$ <> old_part$ then str(printline$,50) = str(part$) & ~
                                       "  " & str(part_descr$)
            str(printline$,111,6) = lot$
            if inc_qty$ <> "Y" then L30430
            call "CONVERT" (quantity, 0.4, str(printline$,118,12))
L30430:     gosub print_line
            str_count% = str_count% + 1 : loc_count% = loc_count% + 1
            part_count% = part_count% + 1
            old_store$ = store$
            old_location$ = location$
            old_part$ = part$

            goto read_loop


        store_break                      /* New Store                  */
            if old_store$ = " " then L33180     /* Check for prior store*/
            printline$ = "* * * END OF STORE " & old_store$ & " * * *"
            call "STRING" addr ("CT", printline$, 132%)
            print skip(2) : lcntr% = lcntr% + 2%
            gosub print_line


        REM SET-UP FOR NEW STORE
            storename$ = " "
            if store$ > end_store$(2) then end_report
L33180:     call "READ100" (#3, store$, f1%(3))
            if f1%(3) = 0% then L33220
            get #3 using L33210, storename$
L33210:         FMT POS(4), CH(30)
L33220:     lcntr% = 99%
            old_store$, old_location$, old_part$ = " "
            return

        location_break                             /* New Location     */
            loc_descr$ = " "
            init (" ")  comment$()
            call "READ100" (#2, store$ & location$, f1%(2))
            if f1%(2) = 0% then L33360
            get #2 using L33350, loc_descr$, comment$()
L33350:         FMT POS(15), CH(30), 2*CH(30)
L33360:     old_location$, old_part$ = " "
            return

        part_break                                 /* New Part         */
            part_descr$ = " "
            call "READ100" (#4, part$, f1%(4))
            if f1%(4) = 0% then L22530
            get #4 using L33550, part_descr$
L33550:         FMT POS(26), CH(32)
            old_part$ = " "
            return

        end_report                /* Report Ending Routine */
            if lcntr% = 99% then gosub page_head
            print skip(2)
            time$ = " "   :   call "TIME" (time$)
            print using L64990, time$    /* End of report line */
            close printer
            call "SETPRNT" ("HNY041", " ", 0%, 1%)
            goto inputmode

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            if pcntr% = 0% then gosub print_params
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "HNYLOCPT"
            print using L60110, rpttitle$, pcntr%
            print
            lcntr% = 3%
            linesave$ = str(printline$,1,132)
            printline$ = str(col_head1$)  :  gosub print_line
            printline$ = str(col_head2$)  :  gosub print_line
            printline$ = str(linesave$,1,132)
            str(printline$,1,3) = temp_store$
            str(printline$,8)   = temp_location$
            init(" ") temp_store$, temp_location$
            return

        print_params           /* Print Page Zero */
            print page
L34515:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L34535
                str(i$(), i%, 1%) = hex(20)
                goto L34515
L34535:     print using L60070, date$, time$, company$, "FAAMTRPT"
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

        print_line                                 /* Print Line       */
            if lcntr% > 56% then gosub page_head
            print using L60140, str(printline$,1,132)
            lcntr% = lcntr% + 1
            init (" ") printline$
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

        FMT                 /* FILE: HNYLOCNS                          */~
            CH(3),          /* Warehouse or Store                      */~
            CH(08),         /* Stock location                          */~
            CH(25),         /* Part code for restarting, if necessary  */~
            CH(6),          /* Which lot in inventory - always used wit*/~
            8*CH(50),       /* Text or Descriptive data - no edits, fre*/~
            CH(3),          /* Warehouse or Store                      */~
            CH(25),         /* Part code for restarting, if necessary  */~
            CH(6),          /* Which lot in inventory - always used wit*/~
            CH(8),          /* Stock location                          */~
            CH(3),          /* Warehouse or Store                      */~
            CH(25),         /* Part code for restarting, if necessary  */~
            CH(8),          /* Stock location                          */~
            CH(6),          /* Which lot in inventory - always used wit*/~
            CH(25),         /* Part code for restarting, if necessary  */~
            CH(3),          /* Warehouse or Store                      */~
            CH(8),          /* Stock location                          */~
            CH(6),          /* Which lot in inventory - always used wit*/~
            CH(4),          /* Stock location skid or pallet           */~
            PD(14,4),       /* Quantity                                */~
            CH(3),          /* Last modified by User ID                */~
            CH(6),          /* Date Record was last modified           */~
            CH(3),          /* Warehouse or Store                      */~
            CH(6),          /* Which lot in inventory - always used wit*/~
            CH(25),         /* Part code for restarting, if necessary  */~
            CH(8),          /* Stock location                          */~
            CH(69)          /* Unused Space                            */~

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
              on fieldnr% gosub L40090,         /* Store Number      */   ~
                                L40090,         /* Location          */   ~
                                L40090,         /* Include Part Info */   ~
                                L40090          /* Include Inventory */
              goto L40105

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40090:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40105:     accept                                                       ~
               at (01,02),                                               ~
                  "Input Report Selection Criteria",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,40), fac(hex(ac)),   columnttl$           , ch(39),~
                                                                         ~
               at (07,02), "Store Number",                               ~
               at (07,40), fac(lfac$( 1)), beg_store$(1)        , ch(03),~
               at (07,60), fac(lfac$( 1)), end_store$(1)        , ch(03),~
                                                                         ~
               at (08,02), "Location Code",                              ~
               at (08,40), fac(lfac$( 2)), beg_location$(1)     , ch(08),~
               at (08,60), fac(lfac$( 2)), end_location$(1)     , ch(08),~
                                                                         ~
               at (09,02), "Include Part Information? (Y,N)",            ~
               at (09,40), fac(lfac$( 3)), partdata$            , ch(01),~
                                                                         ~
               at (10,02), "Include Inventory Qty Data? (Y,N)",          ~
               at (10,40), fac(lfac$( 4)), inc_qty$             , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40290
                  call "MANUAL" ("HNYLOCPT") : goto L40105

L40290:        if keyhit% <> 15 then L40305
                  call "PRNTSCRN" : goto L40105

L40305:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40400     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40385
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% > 1% then L40390
L40385:         str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40390:     return

L40400: if fieldnr% > 0% then L40445  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40445:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100,         /* Store Number           */~
                              L50200,         /* Location               */~
                              L50300,         /* Include Part Info      */~
                              L50400          /* Include Inventory Qty  */
            return
L50100: REM Test for Store Number                 BEG_STORE$
            call "TESTRNGE"    (beg_store$(1), end_store$(1),            ~
                                beg_store$(2), end_store$(2),            ~
                                errormsg$)
            if beg_store$(1) <> "ALL" then beg_store$(2) = beg_store$(1)
            return

L50200: REM Test for Location                     BEG_LOCATION$
            call "TESTRNGE"    (beg_location$(1), end_location$(1),      ~
                                beg_location$(2), end_location$(2),      ~
                                errormsg$)
            return

L50300: REM Test for Include Part Information     PARTDATA$
            if partdata$ =  "Y" then return
            if partdata$ <> "N" then L50345
            inc_qty$     =  "N"  : return
L50345:     errormsg$ = "Enter a 'Y' or 'N'."
            return

L50400: REM Test for Include Inventory Qty Data   INC_QTY$
            if inc_qty$ = "Y" or inc_qty$ = "N" then return
            errormsg$ = "Enter a 'Y' or 'N'."
            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ########   ########              ###########################~
        ~#################################                ########: HNY041

*       * Header Line 2
L60110: %                                     ###########################~
        ~#################################                    PAGE:   ####

L60140: %################################################################~
        ~#################################################################~
        ~###

        %** Report Title for page 0
        %############################################################

L64990:        %                           * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T  @  ########   * * * * * * * * * *

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
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
