        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y  L       CCC    AAA   RRRR   PPPP    *~
            *  H   H  NN  N  Y   Y  L      C   C  A   A  R   R  P   P   *~
            *  HHHHH  N N N   YYY   L      C      AAAAA  RRRR   PPPP    *~
            *  H   H  N  NN    Y    L      C   C  A   A  R   R  P       *~
            *  H   H  N   N    Y    LLLLL   CCC   A   A  R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYLCARP - This program provides the means to report on   *~
            *            the Location Audit File and to Purge those     *~
            *            records report upon.                           *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/13/92 ! Original                                 ! RJ1 *~
            * 06/21/93 ! PRR 12914.  Module or DB admin for Purge.! JDH *~
            * 11/24/93 ! Modified to support LOCAUDIT file change.! MLJ *~
            *          ! Removed Page 0 FACs.                     !     *~
            * 06/24/96 ! DATEFMTC on fminvdate, toinvdate         ! DER *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            adate$8,                     /* Date for screen display    */~
            audit_userid$3,              /* Audit File User ID Stamp   */~
            columnttl$51,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr$32,                    /* Part Description           */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            filler1$7,                   /* Filler                     */~
            fminvdate$10,                /* Date                       */~
            fmloc$8,                     /* Location                   */~
            fmpart$25,                   /* Part Number                */~
            fmstore$3,                   /* Store                      */~
            hiinvdate$10,                /* Date                       */~
            hiloc$8,                     /* Location                   */~
            hipart$25,                   /* Part Number                */~
            histore$3,                   /* Store                      */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            loc$8,                       /* Location                   */~
            locold$8,                    /* Location                   */~
            loinvdate$10,                /* Date                       */~
            loloc$8,                     /* Location                   */~
            lopart$25,                   /* Part Number                */~
            lostore$3,                   /* Store                      */~
            lot$6,                       /* Lot                        */~
            lotold$6,                    /* Lot                        */~
            mtf$1,                       /* Multiple Transaction Flag  */~
            part$25,                     /* Part Number                */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            purgeflag$1,                 /* Purge Records Flag         */~
            qtydif$10,                   /* Quantity Difference        */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            report_type$1,               /* Report Sort Type           */~
            rpttitle$60,                 /* Report Title               */~
            store$3,                     /* Store                      */~
            storeold$3,                  /* Store                      */~
            subitem$25,                  /* Subtotal Item              */~
            temp$8,                      /* Work Variable              */~
            time$8,                      /* System Time                */~
            transtype$1,                 /* Audit Transaction Type     */~
            toinvdate$10,                /* Date                       */~
            toloc$8,                     /* Location                   */~
            topart$25,                   /* Part Number                */~
            tostore$3,                   /* Store                      */~
            transdescr$4,                /* Transaction Type Descriptn */~
            ts$8,                        /* Time Stamp                 */~
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
            * #01 ! HNYMASTR ! Inventory Master File                    *~
            * #03 ! STORNAME ! STORE INFORMATION FILE                   *~
            * #04 ! LOCATION ! Stock location master file               *~
            * #05 ! LOCAUDIT ! Location Audit File                      *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  3, keypos =   26, keylen =  32, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  1, keypos =  102, keylen =   9, dup     ~

            select #03, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3                      ~

            select #04, "LOCATION",                                      ~
                        varc, indexed,  recsize =  400,                  ~
                        keypos = 1, keylen = 11,                         ~
                         alternate key 1, keypos =  4, keylen = 11

            select #05, "LOCAUDIT",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos =    1, keylen =  50,                     ~
                        alt key  1, keypos =   95, keylen =  42, dup     ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 0%, rslt$(01%))
            call "OPENCHCK" (#03, fs%(03%), f2%(03%), 0%, rslt$(03%))
            call "OPENCHCK" (#04, fs%(04%), f2%(04%), 0%, rslt$(04%))
            call "OPENCHCK" (#05, fs%(05%), f2%(05%), 0%, rslt$(05%))

            if f2%(5%) = 0% then L09000
            u3% = 2%
            call "ASKUSER" (u3%, "**** MISSING AUDIT FILE ****",         ~
                 " LOCAUDIT Does Not Exist or is Not Available!", " ",   ~
                 "Press Any PF Key to acknowledge and exit program.")
            goto exit_program

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$) : adm% = 0%
*        See if this User is a Data Base or Module Administrator
            call "CMSMACHK" ("HNY", lfac$(1%), lfac$(2%))
            if lfac$(1%) = "Y" or lfac$(2%) = "Y" then adm% = 1%

            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, ret%)
                ret% = ret%
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "        Location Audit File Report and"  &      ~
                        " Purge Program                "

            str(columnttl$, 1%) = "Beginning Range"
            str(columnttl$,27%) = "Ending Range"

            str(line2$,62%) = "HNYLCARP: " & str(cms2v$,,8%)

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
                  if keyhit%  = 16% then       extract_data
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% = 7% then fieldnr% = 6%
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

            call "SHOSTAT" ("Printing Report")

            gosub generate_report
            gosub end_report
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Part Number            */~
                              L20200,         /* Store                  */~
                              L20300,         /* Location               */~
                              L20400,         /* Date                   */~
                              L20500,         /* Report Order           */~
                              L20700          /* Purge Records          */
            return
L20100: REM Def/Enable Part Number                 FMPART$
            if fmpart$  = " " then   fmpart$  = "ALL"
            return

L20200: REM Def/Enable Store                       FMSTORE$
            if fmstore$  = " " then   fmstore$  = "ALL"
            return

L20300: REM Def/Enable Location                    FMLOC$
            if fmloc$    = " " then   fmloc$    = "ALL"
            return

L20400: REM Def/Enable Date                        FMINVDATE$
            if fminvdate$  = " " then  fminvdate$  = "ALL"
            return

L20500: REM Def/Enable Report Order                REPORT_TYPE$
            if report_type$  = " " then report_type$  = "P"
            return

L20700: REM Def/Enable Purge Records               PURGEFLAG$
            if purgeflag$ = " " then  purgeflag$  = "N"
            if adm% = 1% then return
                purgeflag$  = "N"
                enabled% = 0%
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
         "Enter Part Number Range, 'ALL' or '?'.                       ",~
         "Enter Store Range, 'ALL' or '?'.                             ",~
         "Enter Location Range or 'ALL'.                               ",~
         "Enter Date Range or 'ALL'.                                   ",~
         "Enter Report Sequence Code.                                  ",~
         "Enter 'Y' to Purge Records after Printing Report.            "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, fminvdate$, fmloc$,        ~
                      fmpart$, purgeflag$, report_type$, fmstore$,       ~
                      hiinvdate$, hiloc$, hipart$, histore$, loinvdate$, ~
                      loloc$, lopart$, lostore$, toinvdate$, toloc$,     ~
                      loinvdate$, loloc$, lopart$, topart$, tostore$

            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
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
            subitem$ = " "
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("RPTID", " ", 0%, 0%)
            pcntr% =  0% : lcntr% = 99% /* Page & Line Counters */
            itemcntr% = 0%
            if lcntr% < 57% then L30250
                gosub page_head
                gosub column_head

*       * Start Main Report Section Now **

L30250:     lot$ = all(hex(00))
            if report_type$ = "P" then L30310
                akey% = 0%
                readkey$ = str(lostore$) & str(loloc$) & str(lopart$) &  ~
                       str(lot$) & hex(00)
                goto L30340
L30310:     akey% = 1%
            readkey$ = str(lopart$) & str(lostore$) & str(lot$) &        ~
                       str(loloc$)
L30340:     call "REDALT5" (#5, readkey$, akey%, f1%(5%))
                if f1%(5%) = 1% then L30470
L30360:     u3% = 2%
            call "ASKUSER" (u3%, "**** NO DATA ****",                    ~
                 "No Data Exists that Matches your Selected Critera!",   ~
                 "Press Any PF Key to acknowledge and return.")

            if u3% <> 0% then L30360
                gosub end_report
                goto inputmode

        loop_file
            call "READNXT1" (#5, f1%(5%))
                if f1%(5%) = 0% then return      /* Time to finish up */
L30470:     get #5 using L35040, store$, loc$, part$, lot$, adate%, atime, ~
                       filler1$, transtype$, mtf$, qty, qtyold, locold$, ~
                       storeold$,lotold$, audit_userid$

        REM Is Store/Part/Location within range...
            if store$ < lostore$ or store$ > histore$ then loop_file
            if part$  < lopart$  or part$  > hipart$  then loop_file
            if loc$   < loloc$   or loc$   > hiloc$   then loop_file
            adate% = 19000000% + adate%
            convert adate% to adate$, pic(########)
            call "DATECONV" (adate$)
            if adate$ < loinvdate$  or adate$ > hiinvdate$ then loop_file
            itemcntr%  =  itemcntr%  +  1%

        REM Should we delete it ??
            if purgeflag$ = "Y" then delete #05

        REM Found it so let's print it...
            gosub set_transaction_descr
            qtydif = qty - qtyold
            if transtype% <> 5% then qtydif = abs(qtydif)
            call "CONVERT" (qtydif, 2.2, qtydif$)
            call "DESCRIBE" (#01, part$, descr$, 0%, f1%(1%))
            call "DATEFMT" (adate$)
            convert atime to temp$, pic(########)
            ts$ = str(temp$,1%,2%) & ":" & str(temp$,3%,2%) & ":" &      ~
                  str(temp$,5%,2%)
            lcntr% = lcntr% + 1%
            if lcntr% < 57% then L30705
                gosub page_head
                gosub  column_head

L30705:     if transtype% = 2% then L30710
                locold$, storeold$ = " "
L30710:     if report_type$ = "S" then gosub print_line_by_store         ~
                                  else gosub print_line_by_part
            goto loop_file
*       *  End Print Loop  **

*        **** Begin Misc. Print Subs **** *

        print_line_by_store
            if subitem$ = str(store$) & loc$ then L30840 /* Check for   */
                subitem$ = str(store$) & loc$          /* Subtot break */
                if itemcntr% < 2% then L30850
                    print  :   lcntr% = lcntr% + 1%
                goto L30850
L30840:     store$ = " "
L30850:     print using L60380 ,store$, loc$, part$, str(descr$),lot$,    ~
                                transdescr$, qtydif$, locold$,           ~
                                storeold$, audit_userid$, adate$, ts$

            return

        print_line_by_part
            if subitem$ = part$ then L30970              /* Check for   */
                subitem$ = part$                       /* Subtot break */
                if itemcntr% < 2% then L30980
                    print  :   lcntr% = lcntr% + 1%
                goto L30980
L30970:     part$, descr$  = " "
L30980:     print using L60283 ,part$, str(descr$), store$, lot$, loc$,   ~
                                transdescr$, qtydif$, locold$,           ~
                                storeold$, audit_userid$, adate$, ts$

            return

        set_transaction_descr
            convert transtype$ to transtype%
            on transtype% goto L31080, L31090, L31100, L31110, L31120, L31130, ~
                               L31140, L31150
L31080:         transdescr$ = "SELC"   :  return         /* Select P/L */
L31090:         transdescr$ = "TRAN"   :  return         /* Transfer   */
L31100:         transdescr$ = "ADD "   :  return         /* Addition   */
L31110:         transdescr$ = "WDRL"   :  return         /* Withdrawl  */
L31120:         transdescr$ = "CHNG"   :  return         /* Change     */
L31130:         transdescr$ = "NO/A"   :  return         /* No Action  */
L31140:         transdescr$ = "NEW "   :  return         /* Create/Add */
L31150:         transdescr$ = "PI  "   :  return         /* PI Count   */


        end_report                /* Report Ending Routine */
            time$ = " "  :  call "TIME" (time$)
            if itemcntr% > 0% then L33920
                print skip(2)  :  print using L60460
L33920:     print skip(2)
            print using L64990, time$    /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            return

        column_head              /* Column Heading Print Routine */
            print
            if report_type$ = "S" then  L34040
                print using L60210
                print using L60260
                goto L34070
L34040:     print using L60305
            print using L60340
L34070:     lcntr% = lcntr% + 3%
            return

        page_head              /* Page Heading Print Routine */
            print page         /* Top of Form */
            print using L60070, date$, time$, company$, "HNYLCARP"
            print using L60110, rpttitle$, pcntr%
            print
            lcntr% = 3%
            if pcntr% = 0% then gosub print_params                       ~
                           else pcntr% = pcntr% + 1%
            return

        print_params           /* Print Page Zero */
L34520:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L34550
                str(i$(), i%, 1%) = hex(20)
                goto L34520
L34550:     print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            pcntr% = pcntr% + 1%
*          PRINT PAGE
            gosub page_head
            return

        REM *************************************************************~
            *               F I L E   F O R M A T                       *~
            *************************************************************

L35040: FMT                                         /* File LOCAUDIT   */~
            CH(3),                /* Warehouse or Stores               */~
            CH(8),                /* Stock location                    */~
            CH(25),               /* Part code                         */~
            CH(6),                /* Which lot in inventory            */~
            POS(44), BI(3),       /* Date Stamp                        */~
            BI(4),                /* Time Stamp                        */~
            CH(6),                /* Filler                            */~
            CH(1),                /* Audit File Transaction Code       */~
            CH(1),                /* Multiple Transaction Flag         */~
            PD(14,4),             /* New Quantity                      */~
            PD(14,4),             /* Original Quantity                 */~
            CH(8),                /* Stock location        (Original)  */~
            CH(3),                /* Warehouse or Stores   (Original)  */~
            CH(6),                /* Which lot in inventory(Original)  */~
            CH(3)                 /* User Identificaltion              */


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
              if adm% = 0% then lfac$(6%) = hex(8c) /* Dim */
              on fieldnr% gosub L40105,         /* Part Number       */   ~
                                L40105,         /* Store             */   ~
                                L40105,         /* Location          */   ~
                                L40105,         /* Date              */   ~
                                L40105,         /* Report Order      */   ~
                                L40105          /* Purge Records     */
              goto L40120

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40105:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40120:     accept                                                       ~
               at (01,02),                                               ~
                  "Location Audit Report with Purge Option",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,22), fac(hex(ac)),   columnttl$           , ch(51),~
                                                                         ~
               at (07,02), "Part Number Range",                          ~
               at (07,22), fac(lfac$( 1)), fmpart$              , ch(25),~
               at (07,48), fac(lfac$( 1)), topart$              , ch(25),~
                                                                         ~
               at (08,02), "Store Range",                                ~
               at (08,22), fac(lfac$( 2)), fmstore$             , ch(03),~
               at (08,48), fac(lfac$( 2)), tostore$             , ch(03),~
                                                                         ~
               at (09,02), "Location Range",                             ~
               at (09,22), fac(lfac$( 3)), fmloc$               , ch(08),~
               at (09,48), fac(lfac$( 3)), toloc$               , ch(08),~
                                                                         ~
               at (10,02), "Date Range",                                 ~
               at (10,22), fac(lfac$( 4)), fminvdate$           , ch(10),~
               at (10,48), fac(lfac$( 4)), toinvdate$           , ch(10),~
                                                                         ~
               at (11,02), "Report Sequence",                            ~
               at (11,22), fac(lfac$( 5)), report_type$         , ch(01),~
               at (11,30), "(P) - Part/Store/Lot/Location ",             ~
               at (12,30), "(S) - Store/Location/Part/Lot ",             ~
                                                                         ~
               at (13,02), "Purge Records",                              ~
               at (13,22), fac(lfac$( 6)), purgeflag$           , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40560
                  call "MANUAL" ("HNYLCARP") : goto L40120

L40560:        if keyhit% <> 15% then L40590
                  call "PRNTSCRN" : goto L40120

L40590:        if edit% = 1% then return
                   close ws
                   call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                   return

        set_pf1
        if edit% = 2% then L40780     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                 (4)Previous Field      " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40750
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
            if fieldnr% > 1% then L40760
L40750:         str(pf$(2%),18%,26%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L40760:     return

L40780: if fieldnr% > 0% then L40870  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40870:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
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
            on fieldnr% gosub L50100,         /* Part Number            */~
                              L50200,         /* Store                  */~
                              L50300,         /* Location               */~
                              L50400,         /* Date                   */~
                              L50600,         /* Report Order           */~
                              L50700          /* Purge Records          */
            return
L50100: REM Test for Part Number                  FMPART$
            call "TESTRNGE"                                              ~
                  (fmpart$             , topart$             ,           ~
                   lopart$             , hipart$             ,           ~
                   errormsg$, #01)
            return

L50200: REM Test for Store                        FMSTORE$
            call "TESTRNGE" (fmstore$, tostore$, lostore$, histore$,     ~
                             errormsg$, #03)
                if errormsg$ <> " " then return
            if fmstore$ <> "ALL" then L50240
                lostore$ = all(hex(00))
                histore$ = all(hex(ff))
                return
L50240:     str(lostore$) = str(fmstore$)
            if tostore$ = " " then str(histore$) = str(fmstore$)         ~
                else str(histore$) = str(tostore$)
            return

L50300: REM Test for Location                     FMLOC$
            call "TESTRNGE"                                              ~
                  (fmloc$              , toloc$              ,           ~
                   loloc$              , hiloc$              ,           ~
                   errormsg$)
            return

L50400: REM Test for Date                         FMDATE$

            if fminvdate$ <> "ALL"    then L50430
                loinvdate$, fminvdate$  =  "19000101"
                hiinvdate$, toinvdate$  =  "20991231"
            	 call "DATFMTC" (loinvdate$)
            	 call "DATFMTC" (hiinvdate$)
                goto L50470
L50430:     call "DATEOKC" (fminvdate$, fminvdate%, errormsg$)
            if errormsg$ <> " " then  return
            if toinvdate$ = " " then toinvdate$ = fminvdate$
            call "DATEOKC" (toinvdate$, toinvdate%, errormsg$)
            if errormsg$ <> " " then return

            loinvdate$ = fminvdate$
            hiinvdate$ = toinvdate$
L50470:     call "DATUFMTC"  (loinvdate$)
            call "DATUFMTC"  (hiinvdate$)
            if loinvdate$ > hiinvdate$  then                             ~
                errormsg$ = "TO MUST BE EQUAL TO OR GREATER THAN FROM"   ~
                else  errormsg$ = " "

            return

            toinvdate%, fminvdate% = 0%  /* Do Nothing line */

L50600: REM Test for Report Order                 REPORT_TYPE$
            p% = pos("SP" = report_type$)
            if p% = 0% then errormsg$ = "'P' or 'S' Please."

            return

L50700: REM Test for Purge Records                PURGEFLAG$
            p% = pos("YN" = purgeflag$)
            if p% = 0% then errormsg$ = "'Y' or 'N' Please."

            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:HNY053

*       * Header Line 2
L60110: %                                     ###########################~
        ~#################################                     PAGE: ####

*       * Column Header By PART
L60210: %PART NUMBER               DESCRIPTION                      STR L~
        ~OT    LOCATION TRAN   TRAN QTY FRM LOC FRM STR ID  DATE     TIME


*       * Column Underline By PART
L60260: %------------------------- -------------------------------- --- -~
        ~----- -------- ---- ---------- ------- ------- --- -------- -----~
        ~---

        %** Report By PART
L60283: %######################### ################################ ### #~
        ~##### ######## #### ########## #######    ###  ### ######## #####~
        ~###

*       * Column Header By STORE
L60305: %STR LOCATION PART NUMBER               DESCRIPTION              ~
        ~        LOT    TRAN   TRAN QTY FRM LOC FRM STR ID  DATE     TIME

*       * Column Underline By STORE
L60340: %--- -------- ------------------------- -------------------------~
        ~------- ------ ---- ---------- ------- ------- --- -------- -----~
        ~---

        %** Report By STORE
L60380: %### ######## ######################### #########################~
        ~####### ###### #### ########## #######    ###  ### ######## #####~
        ~###




        %** Null Printing Message
L60460: %                                       * * * No Records Printed ~
        ~* * *

        %** Report Title for page 0
        %############################################################

L64990:         %                          * * * * * * * * * *   E N D   ~
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
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
