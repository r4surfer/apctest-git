        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  V   V  BBBB   K   K  EEEEE  X   X  PPPP   EEEEE  DDDD    *~
            *  V   V  B   B  K  K   E       X X   P   P  E      D   D   *~
            *  V   V  BBBB   KKK    EEEE     X    PPPP   EEEE   D   D   *~
            *   V V   B   B  K  K   E       X X   P      E      D   D   *~
            *    V    BBBB   K   K  EEEEE  X   X  P      EEEEE  DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VBKEXPED - Report to allow users to specify where to      *~
            *            attack back-ordered PO lines.  Report gives    *~
            *            PO line detail.  User specifies window for     *~
            *            past due PO lines and/or window for PO lines   *~
            *            due in the future (useful if you want to       *~
            *            expidite them).                                *~
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
            * 03/26/90 ! Original                                 ! RAN *~
            * 12/06/90 ! Corrected to allow edit of fields 4 & 5. ! WPH *~
            *          ! Changed to call DATE subroutine to       !     *~
            *          ! determine age of PO line.  Removed unused!     *~
            *          ! file channels and code.  Now starts plow !     *~
            *          ! with LOVENRANGE$ to include alpha vendors!     *~
            * 12/18/91 ! Corrected PO & Qty Open field lengths,   ! JDH *~
            *          !   page breaks, ranges, & variable inits. !     *~
            * 12/07/92 ! PRR 12536 - Now prints value total at end! MLJ *~
            *          !  of report.  Also added time at end and  !     *~
            *          !  fixed implied integers.                 !     *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**


        dim                                                              ~
            columnttl$51,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            expday$3,                    /* EXPEDITE DAY               */~
            fmbuyer$3,                   /* Buyer Range                */~
            fmdayrange$3,                /* Days Range                 */~
            fmexpday$3,                  /* Expedite days              */~
            fmvenrange$9,                /* Vendor Range               */~
            fmtype$4,                    /* Vendor type code range     */~
            hibuyer$3,                   /* Buyer Range                */~
            hidayrange$3,                /* Days Range                 */~
            hivenrange$9,                /* Vendor Range               */~
            hitype$4,                    /* Vendor type code range     */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lobuyer$3,                   /* Buyer Range                */~
            lodayrange$3,                /* Days Range                 */~
            lovenrange$9,                /* Vendor Range               */~
            lotype$4,                    /* Vendor type code range     */~
            pastday$3,                   /* PAST DUE DAYS              */~
            part$25,                     /* Part number                */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            plowven$99,                  /* Miscellaneous Read/Plow Key*/~
            po$22,                       /* Key to VBKLINES            */~
            rpttitle$60,                 /* Report Title               */~
            sort$116,                    /* SORTCALL variable          */~
            time$8,                      /* System Time                */~
            tobuyer$3,                   /* Buyer Range                */~
            todayrange$3,                /* Days Range                 */~
            today$6,                     /* Todays date in YYMMDD      */~
            toexpday$3,                  /* Expedite days              */~
            tovenrange$9,                /* Vendor Range               */~
            totype$4,                    /* Vendor Type range          */~
            vtype$4,                     /* Vendor type code           */~
            vendesc$30,                  /* Vendor Description         */~
            userid$3                     /* Current User Id            */

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
            * #01 ! CALMASTR ! PLANNING CALENDAR FILE                   *~
            * #02 ! VBKLINES ! Purchase Order Line Items File           *~
            * #03 ! VENDOR   ! VENDOR MASTER RECORD                     *~
            * #05 ! WORKFILE ! Work area for report                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #02, "VBKLINES",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  28                      ~

            select #03, "VENDOR",                                        ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key 1,  keypos = 10,   keylen = 30

            select #05, "WORKFILE",                                      ~
                        varc, consec, recsize = 93

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            today$, date$ = date
            call "DATEFMT" (date$)
            ret% = ret%
            call "COMPNAME" (12%, company$, ret%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "Purchase Order Expedite Report" &               ~
                        "                              "

            str(columnttl$, 1%) = "Beginning Code"
            str(columnttl$,27%) = "Ending Code"

            str(line2$,62%) = "VBKEXPED: " & str(cms2v$,,8%)


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  5%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
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
L10380:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  5% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L10430:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L10430
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L10430
                  lastfieldnr% = fieldnr%
            goto L10380


        REM *************************************************************~
            *           E X T R A C T   R E P O R T   D A T A           *~
            *-----------------------------------------------------------*~
            * Data Extraction section for report.                       *~
            *************************************************************
        extract_data
            call "SHOSTAT"("Report Generation in Progress")
            call "WORKOPN2"(#5, "OUTPT", 10000%, f1%(5%))

            plowven$ = lovenrange$
        vendor_plow
            call "PLOWNEXT"(#3, plowven$, 0%, f1%(3%))
                if f1%(3%) = 0% then generate_report
                if str(plowven$,,9%) > hivenrange$ then generate_report
                if fmtype$ = "ALL" then start_vbkline_loop
            get #3 using L19106, vtype$
L19106:          FMT POS(477), CH(4)
            if vtype$ <  fmtype$ or vtype$ > totype$ then vendor_plow

        start_vbkline_loop
            init (hex(00)) plowkey$
            str(plowkey$,,9) = plowven$
        vbkline_loop
               call "PLOWNEXT"(#2, plowkey$, 9%, f1%(2%))
                  if f1%(2%) = 0% then vendor_plow
               get #2 using L19200, vendor$, po$, part$, openqty, price,  ~
                                   duedate$, buyer$
L19200:           FMT CH(9),CH(22),CH(25),POS(109),PD(14,4),PD(14,7),    ~
                     POS(142),CH(6),POS(370),CH(3)
               if openqty=0 then vbkline_loop
               if fmbuyer$="ALL" then L19280
                  if buyer$<>fmbuyer$ then vbkline_loop

L19280:        call "DATE" addr("G-", today$, duedate$, expday%, ret%)
               abs% = abs(expday%)
               if expday% >= 0% then  L19340
                  if abs% < fmpast or abs% > topast then vbkline_loop
                  gosub write_record
                  goto vbkline_loop

L19340:        if abs% < fmexp or abs% > toexp then vbkline_loop
                  gosub write_record
                  goto vbkline_loop

        write_record
            write #5 using L19440, vendor$, po$, part$, openqty,          ~
                               openqty*price, duedate$, buyer$, expday%
L19440:     FMT CH(9), CH(22), CH(25), PD(14,4), PD(14,4), CH(6), CH(3), ~
                PD(14,4)
            writecount%=1%
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20140,         /* Vendor Range           */~
                              L20190,         /* Vendor Type code       */~
                              L20240,         /* Buyer Range            */~
                              L20290,         /* Past Due days          */~
                              L20320          /* Future Due Days        */
            return
L20140: REM Def/Enable Vendor Range              FMVENRANGE$, TOVENRANGE$
            if fmvenrange$         = " " then                            ~
               fmvenrange$         = "ALL"
            return

L20190: REM Def/Enable Vendor Type Code          FMVENTYPE$, TOVENTYPE$
            if fmtype$             = " " then                            ~
               fmtype$             = "ALL"
            return

L20240: REM Def/Enable Buyer Range                 FMBUYER$
            if fmbuyer$            = " " then                            ~
               fmbuyer$            = "ALL"
            return

L20290: REM Def/Enable Past Due Days Range         FMPASTDAY$, TOPASTDAY$
            if fmpastday$ <> " " then return
               fmpastday$ = "ALL" :  fmpast = 1  :  topast = 490
            return

L20320: REM Def/Enable Future Due Days Range       FMEXPDAY$, TOEXPDAY$
            if fmexpday$ <> " " then return
               fmexpday$  = "ALL" :  fmexp = 0  :  toexp = 490
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(fieldnr%)
            if fieldnr% <> 0% then L20460
                inpmessage$ = edtmessage$
                return

L20460
*        Define the Input Message for the Screen/Field Indicated
            restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Vendor Range to Scan.                                  ",~
         "Enter the Range of Vendor Type Codes to Scan.                ",~
         "Enter a Single Buyer Code, or 'ALL'.                         ",~
         "Enter Past Due Days Range (1-490), 'ALL' or Leave Blank.     ",~
         "Enter Future Days Due Range (0-490), 'ALL' or Leave Blank.   "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      fmbuyer$,    fmdayrange$, fmvenrange$,             ~
                      hibuyer$,    hidayrange$, hivenrange$,             ~
                      lobuyer$,    lodayrange$, lovenrange$,             ~
                      tobuyer$,    todayrange$, tovenrange$,             ~
                      totype$, fmtype$, lotype$, hitype$,                ~
                      fmexpday$, toexpday$, fmpastday$, topastday$
            writecount% = 0%
            fmpast, topast, fmexp, toexp = 0
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
            gosub sort_workfile
            call "SHOSTAT" ("Printing Report...")
            call "WORKOPN2"(#5,"INPUT",0%,f2%(5))
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("VBK009", " ", 0%, 0%)
            pcntr% = 0% : lcntr% = 99% /* Page & Line Counters */
            gosub print_params

            totalprice, oldexpday  = 0
L30200:     call "READNEXT"(#5, f1%(5%))
               if f1%(5%) = 0% then goto end_report
            get #5 using L19440, vendor$, po$, part$, openqty, price,     ~
               duedate$, buyer$, expday
            expday$, pastday$=" "
            if oldven$ = vendor$ then L30310
            vendesc$ = "****  NOT DEFINED  ****"
            call "READ100"(#3, vendor$, f1%(3%))
               if f1%(3%) = 0% then L30310
            get #3 using L30300, vendesc$, vtype$
L30300:        FMT POS(10), CH(30), POS(477), CH(4)
L30310:     call "CONVERT"(expday,0.0,expday$)
            if expday>=0 and oldexpday<0 then gosub exped_breakpoint
            oldexpday = expday : oldven$ = vendor$
               if expday>=0 then L30365
            expday=abs(expday) : call "CONVERT"(expday,0.0,pastday$)
            expday$=" "
L30365:     if lcntr% > 54% then gosub page_head
            if expday$<>" " then                                         ~
            print using L55220, pastday$, expday$, str(po$,,16%),         ~
               str(po$,20%,3%),buyer$, vendor$, vendesc$, vtype$, part$, ~
               openqty, price                                            ~
               else                                                      ~
            print using L55220, pastday$, expday$, str(po$,,16%),         ~
               str(po$,20%,3%),buyer$, vendor$, vendesc$, vtype$, part$, ~
               openqty, price
            lcntr% = lcntr%+1% : totalprice = totalprice + price
               if mod(lcntr%,5%) <> 0% then L30200
                  print : lcntr% = lcntr% + 1%
                  goto L30200

        exped_breakpoint
           print using L55382
           print using L55360, totalprice
           gosub page_head
           return

        end_report                /* Report Ending Routine */
*          IF FMEXPDAY$ <> " " THEN 30780
               print using L55382
               print using L55330, totalprice
            print skip(2)
               time$ = " "
               call "TIME" (time$)
            print using L55420, time$
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            close #5
            call "FILEBGON" (#5)
            goto inputmode

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            gosub report_title
            print using L55140
            print using L55160
            print using L55190
            lcntr% = 6%
            return

        report_title          /* Page Heading Print Routine */
            print page        /* Top of Form */
            print using L55070, date$, time$, company$, "VBKEXPED"
            print using L55110, rpttitle$, pcntr%

            print
            return

        print_params           /* Print Page Zero */
L30980:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L31000
                str(i$(), i%, 1%) = hex(20)
                goto L30980
L31000:     gosub report_title
            print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            return

        sort_workfile
            if writecount% <> 0%  then goto L31210
            keyhit% = 2%
            call "ASKUSER"(keyhit%, "*** No Records Found ***",          ~
               "There were no records found that met your criteria.",    ~
               "Press any PF Key to return.", " ")
            goto inputmode

L31210:     call "SHOSTAT"("Sorting Report Records")
            call "GETNAMES" addr(#5, file$, lib$, vol$)
            close #5

            sort$=file$
            str(sort$, 9%, 8%)=lib$
            str(sort$,17%, 6%)=vol$
            str(sort$,23%,22%)=str(sort$,,22%)
            str(sort$,45%, 9%)="0082008PA"
            str(sort$,54%, 9%)="0010022CA"
            str(sort$,63%, 9%)="0032025CA"

            call "SORTCALL" addr(sort$, err%)
               if err%=0% then return

            keyhit% = 2%
            call "ASKUSER"(keyhit%, "***  SORT FAILURE  ***",            ~
               "SORT Routine failed ...",                                ~
               "Press any PF Key to acknowledge and exit.")
            goto L65000


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
              on fieldnr% gosub L40190,         /* Vendor Range      */   ~
                                L40190,         /* Vendor Type code  */   ~
                                L40190,         /* Buyer Range       */   ~
                                L40190,         /* Past Due          */   ~
                                L40190          /* Expedite          */
              goto L40220

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40190:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40220:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Purchase Order Expedite Report",                ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),   columnttl$           , ch(51),~
                                                                         ~
               at (07,02), "Vendor Range",                               ~
               at (07,30), fac(lfac$( 1%)), fmvenrange$         , ch(09),~
               at (07,56), fac(lfac$( 1%)), tovenrange$         , ch(09),~
                                                                         ~
               at (08,02), "Vendor Type Code Range",                     ~
               at (08,30), fac(lfac$( 2%)), fmtype$             , ch(04),~
               at (08,56), fac(lfac$( 2%)), totype$             , ch(04),~
                                                                         ~
               at (09,02), "Buyer Code",                                 ~
               at (09,30), fac(lfac$( 3%)), fmbuyer$            , ch(03),~
                                                                         ~
               at (10,02), "Past Due Days Range",                        ~
               at (10,30), fac(lfac$( 4%)), fmpastday$          , ch(03),~
               at (10,56), fac(lfac$( 4%)), topastday$          , ch(03),~
                                                                         ~
               at (11,02), "Future Due Days Range",                      ~
               at (11,30), fac(lfac$( 5%)), fmexpday$           , ch(03),~
               at (11,56), fac(lfac$( 5%)), toexpday$           , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40610
                  call "MANUAL" ("VBKEXPED") : goto L40220

L40610:        if keyhit% <> 15% then L40640
                  call "PRNTSCRN" : goto L40220

L40640:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40830     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40800
                str(pf$(3%),64%)  = " "  :  str(pfkeys$,16%,1%) = hex(ff)
            if fieldnr% > 1% then L40810
L40800:         str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40810:     return

L40830: if fieldnr% > 0% then L40920  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40920:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50140,         /* Vendor Range           */~
                              L50230,         /* Vendor Type code       */~
                              L50300,         /* Buyer Range            */~
                              L50330,         /* Past Due Days Range    */~
                              L50430          /* Future Dure Days Range */
            return
L50140: REM Test for Vendor Range
            call "TESTRNGE"                                              ~
                  (fmvenrange$         , tovenrange$         ,           ~
                   lovenrange$         , hivenrange$         ,           ~
                   errormsg$)
            if fmvenrange$<>"ALL" then return
            tovenrange$=" "
            return

L50230: REM Test for Vendor Type Code Range
            call "TESTRNGE"                                              ~
                  (fmtype$             , totype$             ,           ~
                   lotype$             , hitype$             ,           ~
                   errormsg$)
            return

L50300: REM Test for Buyer Code                   FMBUYER$
            return

L50330: REM Test for Past Due days                FMDAYRANGE$
            if fmpastday$ = " " or fmpastday$ = "ALL" then return
            call "NUMTEST"(fmpastday$,1,490,errormsg$,0.0,fmpast)
               if errormsg$<>" " then return
            call "NUMTEST"(topastday$,1,490,errormsg$,0.0,topast)
               if errormsg$<>" " then return
            if fmpast>topast then errormsg$="Beginning Past Due Days MU"&~
               "ST be Less than or Equal Ending Past Due Days"
            return

L50430: REM Test for Expedite days                FMDAYRANGE$
            if fmexpday$=" " and fmpastday$<>" " then return
                if fmexpday$<>" " then L50475
               errormsg$="You MUST Enter Either Past Due Days Range OR "&~
                         "Future Due Days Range"
               return
L50475:     if fmexpday$ = "ALL" then return
            call "NUMTEST"(fmexpday$,0,490,errormsg$,0.0,fmexp)
               if errormsg$<>" " then return
            call "NUMTEST"(toexpday$,0,490,errormsg$,0.0,toexp)
               if errormsg$<>" " then return
            if fmexp>toexp then errormsg$="Beginning Future Due Days MU"&~
               "ST be Less than or Equal Ending Future Due Days"
            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L55070: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:VBK009

*       * Header Line 2
L55110: %                                                  ##############~
        ~#################################                     PAGE: ####

L55140: %Pst Day PO               PO                                     ~
        ~          Vend                                  Qty             O~
        ~pen
L55160: %Due Exp Number           Lin Buy Vendor    Vendor Name          ~
        ~          Type  Part                           Open            Va~
        ~lue
L55190: %--- --- ---------------- --- --- --------- ---------------------~
        ~--------- ---- ------------------------- ---------- -------------~
        ~---
L55220: %### ### ################ ### ### ######### #####################~
        ~######### #### ######################### #######.## #,###,###,###~
        ~.##
        %                                                 REQUIREMENTS   ~
        ~      Date Out   Qty Req'd
        %                                                 ---------------~
        ~--------------------------
        %                                                 ###############~
        ~####  ########  #######.##
        %                                                 ***  END OF REQ~
        ~UIREMENTS  ***
L55330: %                                                                ~
        ~         *** Report Total Value ***                ##,###,###,###~
        ~.##
L55360: %                                                                ~
        ~        *** Past Due Total Value ***               ##,###,###,###~
        ~.##

L55382: %                                                                ~
        ~                                                    -------------~
        ~---

        %** Report Title for page 0
        %############################################################

L55420: %* * * * * * * * * *   E N D   O F   R E P O R T   @  ########   ~
        ~* * * * * * * * * *



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
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
