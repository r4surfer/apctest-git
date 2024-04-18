        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  FFFFF   AAA   TTTTT  BBBB   L      PPPP   RRRR   TTTTT   *~
            *  F      A   A    T    B   B  L      P   P  R   R    T     *~
            *  FFFF   AAAAA    T    BBBB   L      PPPP   RRRR     T     *~
            *  F      A   A    T    B   B  L      P      R   R    T     *~
            *  F      A   A    T    BBBB   LLLLL  P      R   R    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * FATBLPRT - Prints the tables entered from FADEPRTB        *~
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
            * 09/15/88 ! Original                                 ! TJ2 *~
            * 12/21/93 ! Minor mods for UNIX printers.            ! JDH *~
            * 08/07/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            begdate$(12)8,               /* Table Beginning Date       */~
            cnv$1,                       /* Convention                 */~
            convdescr$20,                /* Proration Convention Descr.*/~
            columnttl$51,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            enddate$(12)8,               /* Table END Date             */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fmconvention$1,              /* Proration Convention       */~
            fmgroup$10,                  /* Group                      */~
            fmperiod$2,                  /* Inservice Period           */~
            fmrecovery$5,                /* Recovery in Yrs            */~
            fmsort$1,                    /* Sort By                    */~
            getper$(58)100,              /* To read in %s from workfile*/~
            group$10,                    /* Group                      */~
            group1$10,                   /* Group                      */~
            hiconvention$1,              /* Proration Convention       */~
            higroup$10,                  /* Group                      */~
            hiperiod$2,                  /* Inservice Period           */~
            hirecovery$5,                /* Recovery in Yrs            */~
            hdr$(12)5,                   /* Header Array for Report    */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            key$18,                      /* Workfile key, dep on SORT$ */~
            life$5,                      /* Life (Recovery)            */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            loconvention$1,              /* Proration Convention       */~
            logroup$10,                  /* Group                      */~
            loperiod$2,                  /* Inservice Period           */~
            lorecovery$5,                /* Recovery in Yrs            */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            prec1$16,                    /* DATA TRANSFER              */~
            per$(60)8,                   /* Percents                   */~
            per(60),                     /* Percents                   */~
            pper$(12)8,                  /* Percents                   */~
            period$2,                    /* Period put in service      */~
            rpttitle$60,                 /* Report Title               */~
            time$8,                      /* System Time                */~
            toconvention$1,              /* Proration Convention       */~
            togroup$10,                  /* Group                      */~
            toperiod$2,                  /* Inservice Period           */~
            torecovery$5,                /* Recovery in Yrs            */~
            types$7,                     /* Types                      */~
            types$(12)7,                 /* Types                      */~
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
            * #01 ! FATABLE  ! Fixed Assets ACRS Depreciation Tables Fi *~
            * #02 ! WORKFILE ! Workfile for sorting report              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "FATABLE",                                       ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    1, keylen =  18

            select #02, "WORKFILE",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =  18

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, u3%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "Fixed Assets Table Report     " &               ~
                        "                              "
            call "STRING" addr("CT", rpttitle$, 60%)

            str(columnttl$, 1) = "Beginning Code"
            str(columnttl$,27) = "Ending Code"

            str(line2$,62) = "FATBLPRT: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

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
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       generate_report
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
        extract_data:
            call "WORKOPEN" (#02, "IO   ", 1000%, f2%(2))
                if f2%(2) = 1% then inputmode      /* ERROR */
            init(hex(00)) plowkey$
            str(plowkey$,1,10) = str(logroup$,1,10)
            get_next:
              call "PLOWNEXT" (#01, plowkey$, 0%, f1%(1))
              if f1%(1) = 0% then extract_done

              group$  = str(plowkey$,1 ,10)
              life$   = str(plowkey$,11, 5)
              cnv$    = str(plowkey$,16, 1)
              period$ = str(plowkey$,17, 2)

              /* Test record */
              if str(group$) >  str(higroup$) then extract_done
              if str(group$) <= str(logroup$) then get_next
              if str(life$) <= str(lorecovery$)  or                      ~
                             str(life$) >  str(hirecovery$) then get_next
              if str(cnv$) <= str(loconvention$) or                      ~
                            str(cnv$) >  str(hiconvention$) then get_next
              if str(period$) <= str(loperiod$)  or                      ~
                             str(period$) >  str(hiperiod$) then get_next

              get #01, using L13300, prec1$, per(), types$
L13300:         FMT POS(19), CH(16), 60*PD(7,4), CH(7)
              if fmsort$ = "1" then key$ = str(group$) & str(cnv$) &     ~
                                                str(period$) & str(life$)~
              else key$ = str(group$) & str(cnv$) &                      ~
                                                str(life$) & str(period$)
              convert life$ to recovery
              recovery% = min(round(recovery + 1.4,0), 60)
              init(" ") per$()
              for i% = 1% to recovery%
                 convert per(i%) to per$(i%), pic(###.####)
              next i%
              write #02, using L13420,   key$, prec1$, per$(), types$,    ~
                                        recovery%, period$
L13420:            FMT                  CH(18), CH(16), 60*CH(8), CH(7), ~
                                        BI(2), CH(2)
              extracted% = 1%
            goto get_next

        extract_done:
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Group                  */~
                              L20200,         /* Recovery in Yrs        */~
                              L20300,         /* Convention             */~
                              L20400,         /* Inservice Period       */~
                              L20500          /* Sort By                */
            return

L20100: REM Def/Enable Group                       FMGROUP$
            if fmgroup$            = " " then                            ~
               fmgroup$            = "ALL"
            return

L20200: REM Def/Enable Recovery in Yrs             FMRECOVERY$
            if fmrecovery$         = " " then                            ~
               fmrecovery$         = "ALL"
            return

L20300: REM Def/Enable Proration Convention        FMCONVENTION$
            if fmconvention$ <> " " then L20390
               fmconvention$ = " " : toconvention$ = "2"
L20390:     return

L20400: REM Def/Enable Inservice Period            FMPERIOD$
            if fmperiod$ <> " " then L20490
               fmperiod$ = " " : toperiod$ = "12"
L20490:     return

L20500: REM Def/Enable Sort By                     FMSORT$
            if fmsort$ <> " " then L20590
            if fmconvention$ = "3" or toconvention$ = "3" then           ~
                                         fmsort$ = "2" else fmsort$ = "1"
L20590:     return

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
         "Enter Group Range.                                           ",~
         "Enter Recovery time in Years Range, 1 - 60.                  ",~
         "Proration Convention, blank)NONE, 1)Half-Year, 2)Mid-QTR, 3)Mid~
        ~-MTH",                                                           ~
         "Enter Inservice Period Range, Blank or 1 - 12.               ",~
         "Report Columns: 1-Recovery Period, 2-Inservice Period.  "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      fmconvention$, fmgroup$, fmperiod$, fmrecovery$,   ~
                      fmsort$, hiconvention$, higroup$, hiperiod$,       ~
                      hirecovery$, loconvention$, logroup$,              ~
                      loperiod$, lorecovery$, toconvention$,             ~
                      togroup$, toperiod$, torecovery$
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
            call "SHOSTAT" ("Printing Depreciation Tables")
            extracted% = 0%
            gosub extract_data
            if extracted% = 1% then L30210
              call "FILEBGON" (#02)
L30110:       u3% = 2%
              call "ASKUSER" ( u3%, " ", "Report Not Printed.",          ~
                       "The specified criteria was not met.",            ~
                       "Press RETURN to EDIT  -OR-  PF(1) to STARTOVER.")
              if u3% <> 1% then L30190
                gosub L30170
L30170:         return clear all
                goto inputmode
L30190:       if u3% = 0% then editpg1
              goto L30110
L30210:     select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("F/A007", " ", 0%, 0%)
            pcntr% = -1% : lcntr% =  4% /* Page & Line Counters */

            if fmsort$ = "1" then break% = 13% else break% = 16%
            init(hex(00)) plowkey$
            next_page:
               i% = 1%
               init(" ") getper$(),hdr$(),begdate$(),enddate$(),types$()
               call "PLOWNEXT" (#02, plowkey$, 0%, f1%(2))
               if f1%(2) = 0% then end_report
                 get #02 using L30350, key$, begdate$(i%), enddate$(i%),  ~
                     str(getper$(),(i%-1)*60*8+1,480),types$(i%),recovery%
L30350:              FMT CH(18), CH(8), CH(8), CH(480), CH(7),BI(2)
                 gosub fmt_for_headings
                 gosub page_head
               next_rec:
                 i% = i% + 1%
                 if i% > 12% then L30560
                 call "PLOWNEXT" (#02, plowkey$, break%, f1%(2))
                 if f1%(2) = 0% then L30560
                 get #02 using L30470, key$, begdate$(i%), enddate$(i%),  ~
                     str(getper$(),(i%-1)*60*8+1,480),types$(i%),recovery%
L30470:              FMT CH(18), CH(8), CH(8), CH(480), CH(7),BI(2)
               gosub fmt_for_headings
               goto next_rec
L30560:        gosub print_lines
            goto next_page

        print_lines:
            gosub print_heading
            for j% = 1% to recovery%
             if lcntr% < 56% then L30650
               gosub page_head
               gosub print_heading
L30650:      for i% = 1% to 12%
               pper$(i%) = str(getper$(), (i%-1)*60*8+((j%-1)*8+1), 8)
             next i%
             convert j% to j$, pic(##)
             print using L31210,j$,pper$(1), pper$(2), pper$(3), pper$(4),~
                                pper$(5), pper$(6), pper$(7), pper$(8),  ~
                                pper$(9), pper$(10), pper$(11), pper$(12)
            lcntr% = lcntr% + 1%
            next j%
            return

        fmt_for_headings:
            if fmsort$ <> "1" then L30765
              hdr$(i%) = str(key$,14,5)
              hdr$ = "RECOVERY"
              goto L30770
L30765:     hdr$(i%) = str(key$,17,2)
            hdr$ = "IN SERV."
L30770:     call "STRING" addr("CT", hdr$(i%), 5%)
            call "SPCSMASH" (types$(i%))
            call "STRING" addr("CT", types$(i%), 7%)
            call "DATEFMT"  (begdate$(i%))
            call "DATEFMT"  (enddate$(i%))
            return

        print_heading:
            print using L31120, types$(1),types$(2),types$(3),types$(4),  ~
                               types$(5),types$(6),types$(7),types$(8),  ~
                               types$(9),types$(10),types$(11),types$(12)

            print using L31150, begdate$(1), begdate$(2), begdate$(3),    ~
                               begdate$(4), begdate$(5), begdate$(6),    ~
                               begdate$(7), begdate$(8), begdate$(9),    ~
                               begdate$(10), begdate$(11), begdate$(12)

            print using L31180, enddate$(1), enddate$(2), enddate$(3),    ~
                               enddate$(4), enddate$(5), enddate$(6),    ~
                               enddate$(7), enddate$(8), enddate$(9),    ~
                               enddate$(10), enddate$(11), enddate$(12)

            print
            print using L31090, hdr$, hdr$(1), hdr$(2), hdr$(3), hdr$(4), ~
                               hdr$(5), hdr$(6), hdr$(7), hdr$(8),       ~
                               hdr$(9), hdr$(10), hdr$(11), hdr$(12)
            print
            lcntr% = lcntr% + 6%
            return

L31090: %########    #####     #####     #####     #####     #####     ##~
        ~####    #####     #####     #####     #####     #####     #####

L31120: %TYPES      #######   #######   #######   #######   #######   ###~
        ~####   #######   #######   #######   #######   #######   #######

L31150: %BEG DATE  ########  ########  ########  ########  ########  ####~
        ~####  ########  ########  ########  ########  ########  ########

L31180: %END DATE  ########  ########  ########  ########  ########  ####~
        ~####  ########  ########  ########  ########  ########  ########

L31210: %      ##  ########  ########  ########  ########  ########  ####~
        ~####  ########  ########  ########  ########  ########  ########

        get_convention_descr:
            if cnv$ = "1" then convdescr$ = "Half-Year"
            if cnv$ = "2" then convdescr$ = "Mid-Quarter"
            if cnv$ = "3" then convdescr$ = "Mid-Month"
            if cnv$ = " " then convdescr$ = "No Convention"
            return

        end_report                /* Report Ending Routine */
            print skip(2)
            print using L64990     /* End of report line */
            call "FILEBGON" (#02)
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            goto inputmode

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            if pcntr% = 0% then gosub print_params
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "FATBLPRT"
            print using L60110, rpttitle$, pcntr%
            print
            cnv$ = str(key$,11,1)
            gosub get_convention_descr
            group1$ = str(key$,,10)
            call "STRING" addr("CT", group1$, 10%)
            if cnv$ <> " " or fmsort$ = "2" then L34140
                print using L60181, group1$, convdescr$
                goto L34180
L34140:     print using L60150, group1$
            if fmsort$ = "1"                                             ~
                         then print using L60200,convdescr$,str(key$,12,2)~
                         else print using L60240,convdescr$,str(key$,12,5)
L34180:     print
            lcntr% = 5%
            return

        print_params           /* Print Page Zero */
            print page
L34232:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L34250
                str(i$(), i%, 1%) = hex(20)
                goto L34232
L34250:     print using L64980, rpttitle$
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
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40095,         /* Group             */   ~
                                L40095,         /* Recovery in Yrs   */   ~
                                L40095,         /* Convention        */   ~
                                L40095,         /* Inservice Period  */   ~
                                L40100          /* Sort By           */
              goto L40110

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40095:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40100:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40110:     accept                                                       ~
               at (01,02),                                               ~
                  "Input Report Selection Criteria",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),   columnttl$           , ch(51),~
                                                                         ~
               at (07,02), "Group",                                      ~
               at (07,30), fac(lfac$( 1)), fmgroup$             , ch(10),~
               at (07,56), fac(lfac$( 1)), togroup$             , ch(10),~
                                                                         ~
               at (08,02), "Recovery in Yrs",                            ~
               at (08,30), fac(lfac$( 2)), fmrecovery$          , ch(05),~
               at (08,56), fac(lfac$( 2)), torecovery$          , ch(05),~
                                                                         ~
               at (09,02), "Proration Convention",                       ~
               at (09,30), fac(lfac$( 3)), fmconvention$        , ch(01),~
               at (09,56), fac(lfac$( 3)), toconvention$        , ch(01),~
                                                                         ~
               at (10,02), "Inservice Period",                           ~
               at (10,30), fac(lfac$( 4)), fmperiod$            , ch(02),~
               at (10,56), fac(lfac$( 4)), toperiod$            , ch(02),~
                                                                         ~
               at (11,02), "Report Columns",                             ~
               at (11,30), fac(lfac$( 5)), fmsort$              , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40315
                  call "MANUAL" ("FATBLPRT") : goto L40110

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
            on fieldnr% gosub L50140,         /* Group                  */~
                              L50230,         /* Recovery in Yrs        */~
                              L50400,         /* Convention             */~
                              L50610,         /* Inservice Period       */~
                              L50820          /* Sort By                */
            return
L50140: REM Test for Group                        FMGROUP$
            call "STRING" addr("LJ", fmgroup$, 10%)
            call "STRING" addr("LJ", togroup$, 10%)
            call "TESTRNGE"                                              ~
                  (fmgroup$            , togroup$            ,           ~
                   logroup$            , higroup$            ,           ~
                   errormsg$)
            return

L50230: REM Test for Recovery in Yrs              FMRECOVERY$
            if str(fmrecovery$,,3)="ALL" or                              ~
                                  str(fmrecovery$,,5)="FIRST" then L50290
            call "NUMTEST" (fmrecovery$, 1, 60, errormsg$, 2.2, recovery)
            if errormsg$ <> " " then return
            call "STRING" addr("RJ", fmrecovery$, 5%)
L50290:     if str(torecovery$,,5)="     " or str(torecovery$,,4)="LAST" ~
                                                              then L50340
            call "NUMTEST" (torecovery$, 1, 60, errormsg$, 2.2, recovery)
            if errormsg$ <> " " then return
            call "STRING" addr("RJ", torecovery$, 5%)
L50340:     call "TESTRNGE"                                              ~
                  (fmrecovery$         , torecovery$         ,           ~
                   lorecovery$         , hirecovery$         ,           ~
                   errormsg$)
            return

L50400: REM Test for Proration Convention         FMCONVENTION$
            temp%, t2% = 0%
            if fmconvention$ <> " " then L50490
              fmconvention$ = addc all(hex(ff))
              temp% = 1%
              if toconvention$ <> " " then L50501
                toconvention$ = addc hex(01)
                t2% = 1%
              goto L50530
L50490:     call "NUMTEST" (fmconvention$, 1, 3, errormsg$, 0.0, t)
            if errormsg$ <> " " then errormsg$ = "Enter Blank, 1, 2 or 3."
L50501:     if toconvention$ <> " " then                                 ~
                    call "NUMTEST" (toconvention$,1,3, errormsg$, 0.0, t)
            if errormsg$ <> " " then errormsg$ = "Enter Blank, 1, 2 or 3."
L50530:     call "TESTRNGE"                                              ~
                  (fmconvention$       , toconvention$       ,           ~
                   loconvention$       , hiconvention$       ,           ~
                   errormsg$)
            if temp% = 1% then fmconvention$ = " "
            if t2% = 1% then toconvention$ = " "
            return

L50610: REM Test for Inservice Period             FMPERIOD$
            temp%, t2%  = 0%
            if fmperiod$ <> "  " then L50720
              fmperiod$ = addc all(hex(ff))
              temp% = 1%
              if toperiod$ = "  " then t2% = 1%
              goto L50740
            call "NUMTEST" (fmperiod$, 1, 12, errormsg$, 0.0, t)
            if errormsg$ <> " " then errormsg$ = "Enter Blank, or 1 - 12."
            if toperiod$ <> " " then                                     ~
                      call "NUMTEST" (toperiod$,1, 12, errormsg$, 0.0, t)
            if errormsg$ <> " " then errormsg$ = "Enter Blank, or 1 - 12."
L50720:     call "STRING" addr("RJ", fmperiod$, 2%)
            call "STRING" addr("RJ", toperiod$, 2%)
L50740:     call "TESTRNGE"                                              ~
                  (fmperiod$           , toperiod$           ,           ~
                   loperiod$           , hiperiod$           ,           ~
                   errormsg$)
            if temp% = 1% then fmperiod$ = " "
            if t2% = 1% then toperiod$ = " "
            return

L50820: REM Test for Sort By                      FMSORT$
            if fmsort$ <> "1" and fmsort$ <> "2" then errormsg$ =        ~
              "Enter SORT METHOD 1 or 2."
            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ######## @ ########              ###########################~
        ~#################################                ########:F/A007

*       * Header Line 2
L60110: %                                     ###########################~
        ~#################################                     PAGE: ####

*       * Header Line 3
L60150: %                                                        GROUP: #~
        ~#########

*       * OR---
L60181: %                                           GROUP: ##########    ~
        ~ CONVENTION: ####################

*       * Header Line 4
L60200: %                               CONVENTION: #################### ~
        ~            PERIOD PUT IN SERVICE: ##

*       * OR---
L60240: %                               CONVENTION: #################### ~
        ~            RECOVERY IN YEARS: #####

        %** Report Title for page 0
L64980: %############################################################

L64990: %                                  * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   * * * * * * * * * *

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

            end
