        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSSS   AAA   FFFFF  L       AAA    GGG    SSS           *~
            *  S      A   A  F      L      A   A  G      S              *~
            *   SSS   AAAAA  FFFF   L      AAAAA  G GGG   SSS           *~
            *      S  A   A  F      L      A   A  G   G      S          *~
            *  SSSS   A   A  F      LLLLL  A   A   GGG    SSS           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SAFLAGS - (1) Set switches for Sales Analysis Module.     *~
            *           (2) Define SA on-line summary files.            *~
            *           (3) Define SA periods.                          *~
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
            * 05/30/86 ! ORIGINAL                                 ! ERN *~
            * 01/02/87 ! Fixed year end bug                       ! KAB *~
            * 11/22/89 ! Increased size of the SADETAIL file.     ! JEF *~
            * 02/12/93 ! PRRs 10886, 11079 Non-Stock Part option. ! JIM *~
            * 02/12/93 ! ALLFREE & selected implied integer convs.! JIM *~
            * 05/30/96 ! Changes for the year 2000                ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                              /* NORMAL OLD VARIABLES       */~
            blankdate$8,                 /* blank unfmt date           */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(40)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second screen line         */~
            p%(1),                       /* Receiver for SEARCH        */~
            pf$(3)79,                    /* PF Key Prompts             */~
            pfk$12,                      /* PF Keys Available          */~
            readkey$75,                  /* Read Key Variable          */~
            tdate$10,                    /* Temp date string           */~
            udate$10                     /* Unformatted date string    */

        dim                              /* IN ORDER OF APPEARANCE     */~
            sales$1,                     /* Post Gross or Net Sales?   */~
            sadtl$1,                     /* Maintain SADETAIL file?    */~
            bookinv$1,                   /* Book non-SO Invoices?      */~
            nonstock$1,                  /* Update Non-Stock parts?    */~
                                                                         ~
            periods$(39)10,              /* SA Period Definitions      */~
            year$6, years$60, used$3,    /* Years Used Work Variables  */~
                                                                         ~
            files$(10)2,                 /* Summary File Definitions   */~
            filedescrs$(10)35,           /* File Descriptions          */~
            groups$(10)14,               /* Grouping Descriptions      */~
                                                                         ~
            hdr2$(4)35,                  /* Second Screen Headings     */~
            hdr3$(4)10,                  /* Third  Screen Headings     */~
            temp$(13)10, last$6, work$10,/* Temporary Array & Variable */~
            prname$8,                    /* Summary file name          */~
            files%(10)                   /* Summary files created flags*/

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
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SYSFILE2 ! Caelus Management System Information     *~
            * #2  ! SASUMRY# ! Sales analysis summary files             *~
            * #3  ! SADETAIL ! Sales analysis detail file               *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "SYSFILE2",                                      ~
                        varc, indexed,                                   ~
                        recsize =  500,                                  ~
                        keypos =     1, keylen =  20

            select #2,  "SASUMRY#",                                      ~
                        varc, indexed,                                   ~
                        recsize = 1048,                                  ~
                        keypos =      1, keylen =  56,                   ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup

            select #3, "SADETAIL",                                       ~
                        varc, indexed,                                   ~
                        recsize =  300,                                  ~
                        keypos =     1, keylen =  8

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, 0%, f2%(1),   0%, " ")
            if f2%(1) <> 0% then L65000


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            date$ = date  :  call "DATEFMT" (date$)

*        See if this User is a Data Base or Module Administrator
            call "CMSMACHK" ("SA", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then L09160
            pf$(1) = "You must be a Data Base or A/R Module Administrato"~
                     & "r to run this program."
            call "ASKUSER" (0%, "SECURITY CHECK", " ", pf$(1), " ")
            goto L65000

L09160
*        Set some variables
            str(line2$,62) = " SAFLAGS: " & cms2v$

            groups$( 1) = "Part Number"
            groups$( 2) = "Part Category"
            groups$( 3) = "Account"
            groups$( 4) = "Ship-to"
            groups$( 5) = "Customer Type"
            groups$( 6) = "Store"
            groups$( 7) = "Sales Region"
            groups$( 8) = "Salesman"

            hdr2$(1) = "FILE"
            hdr2$(2) = "GROUPS"
            hdr2$(3) = "FILE DESCRIPTION"
            hdr2$(4) = "    G R O U P S"

            hdr3$(1) = "PERIOD"
            hdr3$(2) = "LAST YEAR"
            hdr3$(3) = "THIS YEAR"
            hdr3$(4) = "NEXT YEAR"


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * No real input mode - just load defaults from disk.        *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$
            call "ALLFREE"
            gosub L30000   /* Load and then format data        */

            if onfile% = 0% or alreadybeenherebefore% = 1% then L11000
                                                           goto L11000
                keyhit1% = 2%
                call "ASKUSER" (keyhit1%, "W A R N I N G",               ~
        "Changing of existing Parameters may corrupt the SA Data Base.", ~
        "Be sure you understand what you are doing before you continue!",~
            "Enter PF-16 to continue -or- any PF Key to abort." )
            if keyhit1% <> 16% then L65000
            alreadybeenherebefore% = 1%

L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * Handles EDIT of switches and branching to other screens.  *~
            *************************************************************
        switches
L11055:     gosub'051(0%)
L11060:     gosub'111(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then gosub L30100
                  if keyhit%  =  8% then       safiles
                  if keyhit%  =  9% then       mod_admin
                  if keyhit%  = 10% then       saperiods
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 32% then       L65000
                  if keyhit% <>  0% then       L11060
L11110:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 4% then L11060

            gosub'051(fieldnr%)
                  if enabled% = 0% then L11190
L11160:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11160
L11190:     gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11160
                  if fieldnr% = cursor%(1) - 5% then L11055 else L11110


        mod_admin    /* Allow maintenance of ARM Module Administrators */
            call "CMSMAINP" ("SA", "Sales Analysis")
            goto L11000


        REM *************************************************************~
            *        D E F I N E   O N - L I N E   F I L E S            *~
            * --------------------------------------------------------- *~
            * Define Sales Analysis on-line.                            *~
            *************************************************************
        safiles
L12070:     gosub'112(-1%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       switches
                  if keyhit% <>  0 then       L12070

            fieldnr% = 0%
L12210:     gosub'112(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12210
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L12210
                  goto L12070


        REM *************************************************************~
            *        D E F I N E   S A   P E R I O D S                  *~
            * --------------------------------------------------------- *~
            * Define posting periods for Sales Analysis.                *~
            *************************************************************
        saperiods
L14060:     gosub'113(-1%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       switches
                  if keyhit%  = 26 then gosub close_year
                  if keyhit% <>  0 then       L14060

L14120:     fieldnr% = 0%  :  errormsg$ = " "
L14130:     gosub'113(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  8 then gosub fiscal_cal
                  if keyhit%  =  9 then gosub normal_cal
                  if keyhit% <>  0 then L14120
            gosub'153(fieldnr%)
                  if errormsg$ <> " " then L14130
                  goto L14060

        normal_cal  /* Set periods based on Gregorian Calendar         */
            init (" ") periods$()
	    tdate$ = date
	    call "DATEFMT" (tdate$, tdate%, udate$)
            convert str(udate$,3,2) to year%
            for f% = 1% to 12%
                convert year% - 1% to str(periods$(f%    ),1,2), pic(00)
                convert year%      to str(periods$(f%+13%),1,2), pic(00)
                convert year% + 1% to str(periods$(f%+26%),1,2), pic(00)
                convert f%         to str(periods$(f%    ),3,2), pic(00)
                convert f%         to str(periods$(f%+13%),3,2), pic(00)
                convert f%         to str(periods$(f%+26%),3,2), pic(00)
                str(periods$(f%    ),5,2) = "01"
                str(periods$(f%+13%),5,2) = "01"
                str(periods$(f%+26%),5,2) = "01"
                call "DATEFMT" (periods$(f%      ))
                call "DATEFMT" (periods$(f% + 13%))
                call "DATEFMT" (periods$(f% + 26%))
            next f%
            return



        fiscal_cal   /* Set using current fiscal calendar              */
            call "READ100" (#1, "FISCAL DATES        ", f1%(1))
            if f1%(1) = 1% then L14480
                errormsg$ = "Fiscal Calendar has not been defined."
                return
L14480:     init (" ") periods$()
            get #1 using L14510, periods$(14%), periods$(15%), /* This Year */ ~
                 periods$(16%), periods$(17%), periods$(18%), periods$(19%),   ~
                 periods$(20%), periods$(21%), periods$(22%), periods$(23%),   ~
                 periods$(24%), periods$(25%), periods$(26%)
L14510:         FMT XX(22), 13*CH(8)
            for f% = 1% to 13%     /* Subtract a year to get last year */
                if periods$(f%+13%) = blankdate$ then periods$(f%+13%) = " "
                periods$(f%) = periods$(13%+f%)
                if periods$(f%) = " " or periods$(f%) = blankdate$ then L14590
		tdate$ = periods$(f%)
		call "DATEFMT" ( tdate$, tdate%, periods$(f%) )
                convert str(periods$(f%),,4) to year%
                convert year% - 1% to str(periods$(f%),,4), pic(0000)
                if str(periods$(f%),5,4) = "0229" then                   ~
                     str(periods$(f%),5,4)="0228"
                call "DATECONV" (periods$(f%))
L14590:     next f%
            for f% = 1% to 39%           /* Format Dates     */
                call "DATEFMT" (periods$(f%))
            next f%
            return


        close_year
*        Allows user to close 1st Year on file IF that year is after
*        today.
            if (periods$(14) <> " " and periods$(14) <> blankdate$) or ~
               (periods$(27) <> " " and periods$(27) <> blankdate$) then L14720
                errormsg$ = "A 'future' year must be defined."
                return
L14720:     work$ = periods$(14)
            if work$ = " " or work$ = blankdate$ then work$ = periods$(27)
            call "DATUFMTC" (work$)
            if work$ < date then L14780
                errormsg$ = "Last Year has not yet expired."
                return
L14780:     u3% = 2%
            call "ASKUSER" (u3%, "CLOSE YEAR",                           ~
                            "Press PF-32 to Close Last Year", "-OR-",    ~
                            "RETURN to return to edit of dates.")
            if u3% <> 32% then return
                for x% = 1% to 39%
                    if x% < 27% then ~
                       periods$(x%) = periods$(13%+x%) else ~
                       periods$(x%) = " "
                next x%
                str(used$,1,3)  = str(used$,2,2) & " "
                keyhit% = 0%   /* Drop into Edit Mode */
                return


        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            * --------------------------------------------------------- *~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            goto L31000



        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * --------------------------------------------------------- *~
            * Sets DEFAULTS and ENABLES fields for the Page 1 of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
            if fieldnr% <> 0% then L20035
                inpmessage$ = "Place cursor at field to modify and" &    ~
                " Press (RETURN); PF-32 to EXIT program."
                return
L20035:     enabled% = 1%

                  on fieldnr% gosub L20100,         /* Gross or Net     */~
                                    L20200,         /* SADETAIL file    */~
                                    L20300,         /* INV Bookings     */~
                                    L20350          /* Update Non-Stock?*/
*                                  20400,         /*                  */~
*                                  20500,         /*                  */~
*                                  20600,         /*                  */~
*                                  20700,         /*                  */~
*                                  20800,         /*                  */~
*                                  20900          /*                  */
                     return

L20100
*        Default/Enable for GROSS OR NET SALES
            inpmessage$ = "Enter 'G' to post Sales History with Gross" & ~
                          " Sales Dollars, 'N' to post with Net."
            return

L20200
*        Default/Enable for MAINTAIN SADETAIL FILE
            inpmessage$ =                                                ~
              "O= Orders Only, S= Sales Only, B= Both, N= None."
            return

L20300
*        Default/Enable for BOOK INVOICES
            inpmessage$ =                                                ~
              "Enter 'Y' to have Non Sales Order Invoices post bookings."
            return

L20350
*        Default/Enable for Update Non-Stock Parts?
            if nonstock$ = " " then nonstock$ = "N"
            inpmessage$ =                                                ~
              "Enter 'Y' to update Non-Stock Parts to the S/A Summary f"&~
                "iles. Otherwise 'N'."
            return

*        Default/Enable for
            inpmessage$ = "                                             "
            return

*        Default/Enable for
            inpmessage$ = "                                             "
            return

*        Default/Enable for
            inpmessage$ = "                                             "
            return

*        Default/Enable for
            inpmessage$ = "                                             "
            return

*        Default/Enable for
            inpmessage$ = "                                             "
            return

*        Default/Enable
            inpmessage$ = "                                          " & ~
                          "                  "
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            * --------------------------------------------------------- *~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%  /* PUT MSG AREA AT BOTTOM OF SCREEN  */
            call "STARTOVR" (keyhit1%)
                if keyhit1% = 1% then return

                return clear all
                goto inputmode

L30000: REM *************************************************************~
            *                  L O A D   D A T A                        *~
            * --------------------------------------------------------- *~
            * Pull data from switch's string and format.                *~
            *************************************************************
        call "SHOSTAT" ("Loading Data and SA Data Base Configuration")
        call "READ100" (#1, "SWITCHS.SA ", onfile%)
        if onfile% = 1% then L30210

*        No record on file or User wants to reset to system defaults
L30100:  e1% = 0%  : if keyhit% = 2% then e1% = -1% /* Return Dflts */
         e%  = e1% : call "BCKSWTCH" ("SA ", "SALES   ", sales$   , e, e%)
         e%  = e1% : call "BCKSWTCH" ("SA ", "SADTL   ", sadtl$   , e, e%)
         e%  = e1% : call "BCKSWTCH" ("SA ", "BOOKINV ", bookinv$ , e, e%)
         e%  = e1% : call "BCKSWTCH" ("SA ", "NONSTOCK", nonstock$, e, e%)
         if keyhit% = 2% then return     /* Reset switches w/ defaults */

         init (" ") filedescrs$(), periods$(), files$()
         mat files% = con
         return


L30210
*        Record is on file.  Get Settings and Describe History Environ.
            get #1 using L30240, sales$, sadtl$, bookinv$, nonstock$,     ~
                periods$(), files$()

            for x% = 1% to 39%
                if periods$(x%) = blankdate$ then periods$(x%) = " "
            next x%

L30240:         FMT XX(20), 4*CH(1), XX(49), 39*CH(6), 10*CH(2)

*        Find out which summary files are already defined
*        Also determine which 'Years' are on file
            mat files% = con
            years$ = "YYMMDD"
            for f% = 1% to 10%
                prname$ = "SASUMRY" & "#"
                convert f% - 1% to str(prname$,8,1), pic(0)
                call "PUTPRNAM" addr (#2, prname$)
                call "OPENCHCK" (#2, 0%, files%(f%), 0%, " ")
                if files%(f%) =  0% then close #2
                if files%(f%) <> 0% then files$(f%) = " "
            next f%
            gosub describe_files

*        Determine which Open Years have History
            years$ = "YYMMDD"
            readkey$ = "SA.YEARS.USED." & all(hex(00))
L30430:     call "PLOWNEXT" (#1, readkey$, 14%, f1%(1))
            if f1%(1) = 0% then L30490
                year$ = str(readkey$,15,6)
                search years$ = year$ to p%() step 6
                if p%(1) = 0% then years$ = years$ & year$
                goto L30430
L30490:     used$ = " "
            for i% = 1% to 3%
                i1% = i% * 13% - 12%
                if periods$(i1%) = " " or ~
                   periods$(i1%) = blankdate$ then L30550
                search years$ = periods$(i1%) to p%() step 6
                if p%(1) <> 0% then str(used$,i%,1) = "Y"
L30550:     next i%

            for i% = 1% to 39%
                call "DATFMTC" (periods$(i%))
            next i%

            return


        describe_files
            for i% = 1% to 10%
                filedescrs$(i%) = " "
                if files$  (i%) = " " then L30730
                     convert str(files$(i%),1,1) to i1%
                     filedescrs$(i%) = groups$(i1%)
                     convert str(files$(i%),2,1) to i1%, data goto L30730
                     filedescrs$(i%) = filedescrs$(i%) & ", "            ~
                                                           & groups$(i1%)
L30730:     next i%
            return


L31000: REM *************************************************************~
            *                  S A V E   D A T A                        *~
            * --------------------------------------------------------- *~
            * Write switches back to file                               *~
            *************************************************************

        call "SHOSTAT" ("Saving Data and Creating SA Data Base")
            for i% = 1% to 39%
                call "DATUFMTC" (periods$(i%))
            next i%

            call "READ101" (#1, "SWITCHS.SA ", f1%(1%))
            put #1 using L31140, "SWITCHS.SA ", sales$, sadtl$, bookinv$, ~
                nonstock$, " ", periods$(), files$(), " "
L31140:         FMT CH(20), 4*CH(1), CH(49), 39*CH(6), 10*CH(2), CH(173)

            if f1%(1%) = 0% then write #1 else rewrite #1

*        Now create SA files & Save PLOWCODE descriptors file
            mat files% = con
            readkey$ = "SA.FILES.SASUMRY"
            call "DELETE" (#1, readkey$, 16%)
            for f% = 1% to 10%
                if files$(f%) = " " then L31390
                     prname$ = "SASUMRY" & "#"
                     convert f% - 1% to str(prname$,8,1), pic(0)
                     call "PUTPRNAM" addr (#2, prname$)
                     call "OPENCHCK" (#2, 0%, files%(f%), 500%, " ")
                     if files%(f%) = 0% then close #2
                     file1%, file2% = 0%
                     convert str(files$(f%),1,1) to file1%,              ~
                                                          data goto L31320
L31320:              convert str(files$(f%),2,1) to file2%,              ~
                                                          data goto L31340
L31340:              write #1 using L31370, "SA.FILES.", prname$,         ~
                                           filedescrs$(f%), files$(f%),  ~
                                           file1%, file2%, " ", " "
L31370:                   FMT CH(9), CH(11), CH(30), CH(2), 2*BI(1),     ~
                              CH(246), CH(200)
L31390:     next f%

            if sadtl$ = "N" then L65000
                call "OPENCHCK" (#3, 0%, f2%(3), 1000%, " ")
                goto L65000


        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            * --------------------------------------------------------- *~
            * Screen for Editing Page 1 (Switch settings).              *~
            *************************************************************

        deffn'111(fieldnr%)
            if fieldnr% = 0% then init(hex(86)) lfac$()  else            ~
                                  init(hex(8c)) lfac$()
            gosub set_pf_keys

                  on fieldnr% gosub L40190,         /* Sales (Gross/Net)*/~
                                    L40190,         /* SADETAIL         */~
                                    L40190,         /* Book Invoices    */~
                                    L40190          /* Non-Stock Parts? */
                     goto L40260

                  REM Set FAC'S for UPPER/LOWER CASE input
                      lfac$(fieldnr%) = hex(80)
                      return
L40190:           REM Set FAC'S for UPPER CASE ONLY input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC'S for NUMERIC ONLY input
                      lfac$(fieldnr%) = hex(82)
                      return

L40260:   accept                                                         ~
            at (01,02), "Manage Sales Analysis Behavior Switches",       ~
            at (01,66), "Today:",                                        ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (04,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (06,02),                                                  ~
             "Post Gross Sales (G) or Net Sales (N)                    ",~
            at (06,70), fac(lfac$(1%)), sales$                  , ch(01),~
            at (07,02),                                                  ~
             "What Sales Analysis Detail should be kept? (O/S/B/N)     ",~
            at (07,70), fac(lfac$(2%)), sadtl$                  , ch(01),~
            at (08,02),                                                  ~
             "Update Booking History w/ Non Sales Order Invoices? (Y/N)",~
            at (08,70), fac(lfac$(3%)), bookinv$                , ch(01),~
            at (09,02),                                                  ~
             "Update Non-Stock Parts to S/A Summary files? (Y/N)       ",~
            at (09,70), fac(lfac$(4%)), nonstock$               , ch(01),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)), pf$(1)                     ,ch(79),~
            at (23,02), fac(hex(8c)), pf$(2)                     ,ch(79),~
            at (24,02), fac(hex(8c)), pf$(3)                     ,ch(79),~
                                                                         ~
                keys(pfk$),                                              ~
                key (keyhit%)

                if keyhit% <> 13 then L40550
                     call "MANUAL" ("SAFLAGS")
                     goto L40260

L40550:         if keyhit% <> 15 then L40590
                     call "PRNTSCRN"
                     goto L40260

L40590:         close ws
                call "SCREEN" addr("C", n%, "I", i$(), cursor%())
                                        n% = n%
                return

        set_pf_keys
          if fieldnr% <> 0% then L40760
            pf$(1) = "(1)Start Over            (8)Manage SA Su" &        ~
                     "mmary File Definitions (13)Instructions"
            pf$(2) = "                         (9)Maintain SA " &        ~
                     "Module Administrators  (15)Print Screen"
            pf$(3) = "(2)Reset using Defaults (10)Manage SA Pe" &        ~
                     "riod Definitions       (16)SAVE DATA   "
            str(pf$(3),63,1) = hex(84)
            pfk$ = hex(00010208090a0d0f1020ff)
            return

L40760:     pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfk$ = hex(00010d0fffffffffffff)
            return


        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   2       *~
            * --------------------------------------------------------- *~
            * Screen for Editing Page 1 (File Definitions).             *~
            *************************************************************

        deffn'112(fieldnr%)
            init(hex(8c)) lfac$()
            if fieldnr% = -1% then init(hex(84)) lfac$()
            if fieldnr% =  0% then init(hex(82)) lfac$()
            if fieldnr% >  0% then init(hex(82)) lfac$(fieldnr%)
            for f% = 1% to 10%      /* Protect files already defined   */
                if files%(f%) = 0% then lfac$(f%) = hex(8c)
            next f%
            gosub set_pf_keys2


L41140:   accept                                                         ~
            at (01,02), "Manage SA Summary File Definitions ",           ~
            at (01,67), "Date:",                                         ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (04,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (06,02), fac(hex(ac)), str(hdr2$(1),, 4),                 ~
            at (06,07), fac(hex(ac)), str(hdr2$(2),, 6),                 ~
            at (06,14), fac(hex(ac)), str(hdr2$(3),,35),                 ~
            at (06,55), "!",                                             ~
            at (06,58), fac(hex(ac)), str(hdr2$(4),,22),                 ~
            at (07,02), " 0", at (08,02), " 1", at (09,02), " 2",        ~
            at (10,02), " 3", at (11,02), " 4", at (12,02), " 5",        ~
            at (13,02), " 6", at (14,02), " 7", at (15,02), " 8",        ~
            at (16,02), " 9",                                            ~
            at (07,55), "!", at (08,55), "!",  at (09,55), "!",          ~
            at (10,55), "!", at (11,55), "!",  at (12,55), "!",          ~
            at (13,55), "!", at (14,55), "!",  at (15,55), "!",          ~
            at (16,55), "!",                                             ~
            at (07,58), " 1", at (08,58), " 2", at (09,58), " 3",        ~
            at (10,58), " 4", at (11,58), " 5", at (12,58), " 6",        ~
            at (13,58), " 7", at (14,58), " 8",                          ~
            at (07,62), fac(hex(8c)), groups$(1),                        ~
            at (08,62), fac(hex(8c)), groups$(2),                        ~
            at (09,62), fac(hex(8c)), groups$(3),                        ~
            at (10,62), fac(hex(8c)), groups$(4),                        ~
            at (11,62), fac(hex(8c)), groups$(5),                        ~
            at (12,62), fac(hex(8c)), groups$(6),                        ~
            at (13,62), fac(hex(8c)), groups$(7),                        ~
            at (14,62), fac(hex(8c)), groups$(8),                        ~
                                                                         ~
            at (07,08), fac(lfac$( 1)), str(files$(1),1,1)      , ch(01),~
            at (07,11), fac(lfac$( 1)), str(files$(1),2,1)      , ch(01),~
            at (08,08), fac(lfac$( 2)), str(files$(2),1,1)      , ch(01),~
            at (08,11), fac(lfac$( 2)), str(files$(2),2,1)      , ch(01),~
            at (09,08), fac(lfac$( 3)), str(files$(3),1,1)      , ch(01),~
            at (09,11), fac(lfac$( 3)), str(files$(3),2,1)      , ch(01),~
            at (10,08), fac(lfac$( 4)), str(files$(4),1,1)      , ch(01),~
            at (10,11), fac(lfac$( 4)), str(files$(4),2,1)      , ch(01),~
            at (11,08), fac(lfac$( 5)), str(files$(5),1,1)      , ch(01),~
            at (11,11), fac(lfac$( 5)), str(files$(5),2,1)      , ch(01),~
            at (12,08), fac(lfac$( 6)), str(files$(6),1,1)      , ch(01),~
            at (12,11), fac(lfac$( 6)), str(files$(6),2,1)      , ch(01),~
            at (13,08), fac(lfac$( 7)), str(files$(7),1,1)      , ch(01),~
            at (13,11), fac(lfac$( 7)), str(files$(7),2,1)      , ch(01),~
            at (14,08), fac(lfac$( 8)), str(files$(8),1,1)      , ch(01),~
            at (14,11), fac(lfac$( 8)), str(files$(8),2,1)      , ch(01),~
            at (15,08), fac(lfac$( 9)), str(files$(9),1,1)      , ch(01),~
            at (15,11), fac(lfac$( 9)), str(files$(9),2,1)      , ch(01),~
            at (16,08), fac(lfac$(10)), str(files$(10),1,1)     , ch(01),~
            at (16,11), fac(lfac$(10)), str(files$(10),2,1)     , ch(01),~
                                                                         ~
            at (07,14), fac(hex(8c)), filedescrs$( 1)           , ch(30),~
            at (08,14), fac(hex(8c)), filedescrs$( 2)           , ch(30),~
            at (09,14), fac(hex(8c)), filedescrs$( 3)           , ch(30),~
            at (10,14), fac(hex(8c)), filedescrs$( 4)           , ch(30),~
            at (11,14), fac(hex(8c)), filedescrs$( 5)           , ch(30),~
            at (12,14), fac(hex(8c)), filedescrs$( 6)           , ch(30),~
            at (13,14), fac(hex(8c)), filedescrs$( 7)           , ch(30),~
            at (14,14), fac(hex(8c)), filedescrs$( 8)           , ch(30),~
            at (15,14), fac(hex(8c)), filedescrs$( 9)           , ch(30),~
            at (16,14), fac(hex(8c)), filedescrs$(10)           , ch(30),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)), pf$(1)                     ,ch(79),~
            at (23,02), fac(hex(8c)), pf$(2)                     ,ch(79),~
            at (24,02), fac(hex(8c)), pf$(3)                     ,ch(79),~
                                                                         ~
                keys(pfk$),                                              ~
                key (keyhit%)

                if keyhit% <> 13 then L41900
                     call "MANUAL" ("SAFLAGS")
                     goto L41140

L41900:         if keyhit% <> 15 then L41940
                     call "PRNTSCRN"
                     goto L41140

L41940:         close ws
                call "SCREEN" addr("C", n%, "I", i$(), cursor%())
                                        n% = n%
                return

        set_pf_keys2
          if fieldnr% <> -1% then L42120
            inpmessage$ = "Press (RETURN) to Edit Fields."
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)RETURN      "
            str(pf$(3),63,1) = hex(84)
            pfk$ = hex(0001ffffffff0d0f10ffff)
            return

L42120:     inpmessage$ = "Edit fields and then Press (RETURN)."
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfk$ = hex(00010d0fffffffffffff)
            return


        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   3       *~
            * --------------------------------------------------------- *~
            * Screen for Editing Page 1 (Period Definitions).           *~
            *************************************************************

        deffn'113(fieldnr%)
            init(hex(8c)) lfac$()
            if fieldnr% < 100% then L44120
                init(hex(89)) lfac$()
                lfac$(fieldnr% - 100%) = hex(81)
                goto L44150
L44120:     if fieldnr% = -1% then init(hex(84)) lfac$()
            if fieldnr% =  0% then init(hex(81)) lfac$()
            if fieldnr% >  0% then init(hex(81)) lfac$(fieldnr%)
L44150:     for i% = 1% to 3%
                if str(used$,i%,1) = " " then L44190
                     i1% = i% * 13% - 12%
                     init (hex(8c)) str(lfac$(),i1%,13)
L44190:     next i%
            gosub set_pf_keys3


L44230:   accept                                                         ~
            at (01,02), "Manage SA Period Definitions ",                 ~
            at (01,67), "Date:",                                         ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (04,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (06,15), fac(hex(ac)), str(hdr3$(1),, 6),                 ~
            at (06,25), fac(hex(ac)), str(hdr3$(2),, 9),                 ~
            at (06,40), fac(hex(ac)), str(hdr3$(3),, 9),                 ~
            at (06,55), fac(hex(ac)), str(hdr3$(4),, 9),                 ~
            at (07,17), " 1", at (08,17), " 2", at (09,17), " 3",        ~
            at (10,17), " 4", at (11,17), " 5", at (12,17), " 6",        ~
            at (13,17), " 7", at (14,17), " 8", at (15,17), " 9",        ~
            at (16,17), "10", at (17,17), "11", at (18,17), "12",        ~
            at (19,17), "13",                                            ~
                                                                         ~
            at (07,25), fac(lfac$( 1)), periods$( 1)            , ch(10),~
            at (08,25), fac(lfac$( 2)), periods$( 2)            , ch(10),~
            at (09,25), fac(lfac$( 3)), periods$( 3)            , ch(10),~
            at (10,25), fac(lfac$( 4)), periods$( 4)            , ch(10),~
            at (11,25), fac(lfac$( 5)), periods$( 5)            , ch(10),~
            at (12,25), fac(lfac$( 6)), periods$( 6)            , ch(10),~
            at (13,25), fac(lfac$( 7)), periods$( 7)            , ch(10),~
            at (14,25), fac(lfac$( 8)), periods$( 8)            , ch(10),~
            at (15,25), fac(lfac$( 9)), periods$( 9)            , ch(10),~
            at (16,25), fac(lfac$(10)), periods$(10)            , ch(10),~
            at (17,25), fac(lfac$(11)), periods$(11)            , ch(10),~
            at (18,25), fac(lfac$(12)), periods$(12)            , ch(10),~
            at (19,25), fac(lfac$(13)), periods$(13)            , ch(10),~
                                                                         ~
            at (07,40), fac(lfac$(14)), periods$(14)            , ch(10),~
            at (08,40), fac(lfac$(15)), periods$(15)            , ch(10),~
            at (09,40), fac(lfac$(16)), periods$(16)            , ch(10),~
            at (10,40), fac(lfac$(17)), periods$(17)            , ch(10),~
            at (11,40), fac(lfac$(18)), periods$(18)            , ch(10),~
            at (12,40), fac(lfac$(19)), periods$(19)            , ch(10),~
            at (13,40), fac(lfac$(20)), periods$(20)            , ch(10),~
            at (14,40), fac(lfac$(21)), periods$(21)            , ch(10),~
            at (15,40), fac(lfac$(22)), periods$(22)            , ch(10),~
            at (16,40), fac(lfac$(23)), periods$(23)            , ch(10),~
            at (17,40), fac(lfac$(24)), periods$(24)            , ch(10),~
            at (18,40), fac(lfac$(25)), periods$(25)            , ch(10),~
            at (19,40), fac(lfac$(26)), periods$(26)            , ch(10),~
                                                                         ~
            at (07,55), fac(lfac$(27)), periods$(27)            , ch(10),~
            at (08,55), fac(lfac$(28)), periods$(28)            , ch(10),~
            at (09,55), fac(lfac$(29)), periods$(29)            , ch(10),~
            at (10,55), fac(lfac$(30)), periods$(30)            , ch(10),~
            at (11,55), fac(lfac$(31)), periods$(31)            , ch(10),~
            at (12,55), fac(lfac$(32)), periods$(32)            , ch(10),~
            at (13,55), fac(lfac$(33)), periods$(33)            , ch(10),~
            at (14,55), fac(lfac$(34)), periods$(34)            , ch(10),~
            at (15,55), fac(lfac$(35)), periods$(35)            , ch(10),~
            at (16,55), fac(lfac$(36)), periods$(36)            , ch(10),~
            at (17,55), fac(lfac$(37)), periods$(37)            , ch(10),~
            at (18,55), fac(lfac$(38)), periods$(38)            , ch(10),~
            at (19,55), fac(lfac$(39)), periods$(39)            , ch(10),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)), pf$(1)                     ,ch(79),~
            at (23,02), fac(hex(8c)), pf$(2)                     ,ch(79),~
            at (24,02), fac(hex(8c)), pf$(3)                     ,ch(79),~
                                                                         ~
                keys(pfk$),                                              ~
                key (keyhit%)

                if keyhit% <> 13 then L44940
                     call "MANUAL" ("SAFLAGS")
                     goto L44230

L44940:         if keyhit% <> 15 then L44980
                     call "PRNTSCRN"
                     goto L44230

L44980:         close ws
                call "SCREEN" addr("C", n%, "I", i$(), cursor%())
                return

        set_pf_keys3
          if fieldnr% <> -1% then L45170
            inpmessage$ = "Press (RETURN) to Edit fields."
            if used$ <> " " then inpmessage$ = inpmessage$ &             ~
                          " Dim Years have data and may not be changed."
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                (26)Close Last Year     " &        ~
                     "                       (16)RETURN      "
            str(pf$(3),63,1) = hex(84)
            pfk$ = hex(0001ffffffff0d0f101aff)
            return

L45170:     inpmessage$ = "Enter Period Start Dates and press (RETURN)."
            pf$(1) = "(1)Start Over   ( 8)Set Using Fiscal Cal" &        ~
                     "endar                  (13)Instructions"
            pf$(2) = "                ( 9)Set Using Gregorian " &        ~
                     "Calendar               (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfk$ = hex(00010809ff0d0fffffff)
            if used$ = " " then return
                str(pf$(1),17,32) = " "  : str(pfk$,3,1) = hex(ff)
                str(pf$(2),17,32) = " "  : str(pfk$,4,1) = hex(ff)
                return



        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the items on Page 1 (Switches).             *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* Gross/Net Sales  */~
                                    L50200,         /* SADETAIL         */~
                                    L50300,         /* Book Invoices    */~
                                    L50350          /* Update Non-Stock?*/
*                                  50400,         /*                  */~
*                                  50500,         /*                  */~
*                                  50600,         /*                  */~
*                                  50700,         /*                  */~
*                                  50800,         /*                  */~
*                                  50900          /*                  */
                     return

L50100
*        Test Data for POST GROSS OR NET SALES
            if sales$ = "G" or sales$ = "N" then return
                errormsg$ = "Enter 'G' for Gross, 'N' for Net."
                return

L50200
*        Test Data for SADETAIL
            if sadtl$ = "O" or sadtl$ = "S" or sadtl$ = "B"              ~
                                            or sadtl$ = "N" then return
                errormsg$ = "Enter 'O', 'S', 'B', or 'N'."
                return

L50300
*        Test Data for BOOK INVOICES
            if bookinv$ = "Y" or bookinv$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L50350
*        Test Data for Update Non-Stock Parts?
            if nonstock$ = "Y" or nonstock$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N'."
                return



                return

*        Test Data for



                return

*        Test Data for



                return

*        Test Data for



            return

*        Test Data for



                return

*        Test Data for



                return

*        Test Data for



                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the items on Page 2 (File Definitions).     *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "

            for fieldnr% = 1% to 10%
                f% = fieldnr%
                if files$(f%) = " " then L51250
                     if str(files$(f%),,1) = " " then                    ~
                                        files$(f%) = str(files$(f%),2,1)
                     if str(files$(f%),1,1) < "1" or                     ~
                        str(files$(f%),1,1) > "8" then L51300
                     if str(files$(f%),2,1) = " " then L51210
                     if str(files$(f%),2,1) < "1" or                     ~
                        str(files$(f%),2,1) > "8" then L51300
                     if str(files$(f%),1,1) = str(files$(f%),2,1)        ~
                                                             then L51310
L51210:              if f% = 1% then L51250
                          for f1% = 1% to f% - 1%
                               if files$(f1%) = files$(f%) then L51320
                          next f1%
L51250:     next fieldnr%
            gosub describe_files
            return


L51300:     errormsg$ = "Entries must be 1 - 8"     : return
L51310:     errormsg$ = "Entries must be different" : return
L51320:     errormsg$ = "File already defined!"     : return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the items on Page 3 (Period Definitions).   *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "

*        FIRST We'll smash out any blank spots
          for u3% = 0% to 2%
            for x% = 1% to 13%: temp$(x%) = periods$(u3%*13%+x%): next x%
            call "LINSMASH" (temp$())
            for x% = 1% to 13%: periods$(u3%*13%+x%) = temp$(x%): next x%
          next u3%

*        NEXT We Check that the dates are A-ok
            last$ = all(hex(00))
            count% = 0%
            for f% = 1% to 39%
                if periods$(f%) = " " or ~
                   periods$(f%) = blankdate$ then L52300

                count% = count% + 1%
                call "DATEOKC" (periods$(f%), n%, errormsg$)
                if errormsg$ <> " " then fieldnr% = f%
                if errormsg$ <> " " then return
                     call "DATUFMTC" (periods$(f%))
                     if periods$(f%) <= last$ then                       ~
                          errormsg$ = "Dates must be in ascending order."
                     last$ = periods$(f%)
                     call "DATFMTC" (periods$(f%))
                     if errormsg$ <> " " then fieldnr% = 100% + f%
                     if errormsg$ <> " " then return
L52300:     next f%

            if count% > 1% then return
                errormsg$ = "You must enter at least TWO Dates."
                fieldnr%  = 0%
                return

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

            end
