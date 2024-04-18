        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   SSS   EEEEE  RRRR    AAA   RRRR    CCC   H   H  V   V   *~
            *  S      E      R   R  A   A  R   R  C   C  H   H  V   V   *~
            *   SSS   EEEE   RRRR   AAAAA  RRRR   C      HHHHH  V   V   *~
            *      S  E      R   R  A   A  R   R  C   C  H   H   V V    *~
            *   SSS   EEEEE  R   R  A   A  R   R   CCC   H   H    V     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SERARCHV - Archiving of Serial Number Data.  This program *~
            *            will archive files from SERMASTR to SNMYYYY and*~
            *            from SERDETAL to SNDYYYY based on users picked *~
            *            ranges. (i.e. YYYY denotes year)               *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/20/91 ! Original                                 ! SID *~
            * 08/21/96 ! Century date conversion                  ! DER *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            archyear$4,                  /* Archive File Year          */~
            assypart$45,                 /* Parent Part and Serianl Nbr*/~
            blankdate$8,                 /* blank unfmt date           */~
            blankline$79,                /* Line for input screen      */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dateline$79,                 /* Date line on screen        */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            eligibles$38,                /* # of records eligibles Mtr */~
            eligiblesd$10,               /* # of records eligibles Dtl */~
            fmarchdate$10,               /* Archive Date Range         */~
            fminv$8,                     /* Invoice Numbers Range      */~
            fmsernbr$20,                 /* Serial Numbers Range       */~
            getback$9,                   /* (1)Return on Safeguard Scr.*/~
            header$42, header2$23,       /* Heading for Input Screen   */~
            hiinv$8,                     /* Invoice Numbers            */~
            hisernbr$20,                 /* Serial Numbers             */~
            hits$38,                     /* # of records hits Master   */~
            hitsd$10,                    /* # of records hits Detail   */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            loinv$8,                     /* Invoice Numbers            */~
            losernbr$20,                 /* Serial Numbers             */~
            mode$5,                      /* HNYQUAN Open mode          */~
            miscflag$1,                  /* Include Misc. Usage (Y/N)? */~
            part$(50)45,                 /* Parent Part and Serianl Nbr*/~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            qty(3),                      /* Qty fields in SERDETAL     */~
            readkey$93,                  /*                            */~
            recd$(2)100,                 /* SERDETAL Record            */~
            recm$(3)100,                 /* SERMASTR Record            */~
            sernbrlen$2,                 /* Serial Numbers Max Length  */~
            safeguard$3,                 /* Proceed to Archive (Y/N)?  */~
            toarchdate$10,               /* Archive Date Range         */~
            toinv$8,                     /* Invoice Numbers Range      */~
            tosernbr$20,                 /* Serial Numbers Range       */~
            unffmarchdate$10,            /* unfmt from arch date       */~
            unftoarchdate$10,            /* unfmt to arch date         */~
            userid$3,                    /* Current User Id            */~
            wildcard$20,                 /* Wild Card Serial Numbers   */~
            writtens$38,                 /* # of records writtens Mtr. */~
            writtensd$10                 /* # of records writtens Dtl. */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20,                 /* Text from file opening     */~
            axd$4                        /* AXD Block for OPENFILE     */

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
            * #01 ! SERMASTR ! Serial Number Tracking Master File       *~
            * #02 ! SERDETAL ! Serial Number Tracking Parent (Component)*~
            * #03 ! SERMYYYY ! Serial Number Master Dummp File          *~
            * #04 ! SERDYYYY ! Serial Number Detail Dummp File          *~
            * #05 ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  2, keypos =    1, keylen =  76,         ~
                            key  1, keypos =   32, keylen =  45          ~

            select #02, "SERDETAL",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =   46, keylen =  48,                     ~
                        alt key  1, keypos =    1, keylen =  93

            select #03, "SERMYYYY",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  2, keypos =    1, keylen =  76,         ~
                            key  1, keypos =   32, keylen =  45          ~

            select #04, "SERDYYYY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =   46, keylen =  48,                     ~
                        alt key  1, keypos =    1, keylen =  93

            select #05, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =     1, keylen =  20

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#05, fs%(05), f2%(05), 0%, rslt$(05))

L03000:     keyhit% = 2% /* Window in the Bottom */
            call "ASKUSER" (keyhit%, "*** EXCLUSIVE FILES OPTION ***",   ~
                "Press PF1 to Archive Files in an EXCLUSIVE MODE ",      ~
                "Press PF2 to Archive Files in a SHARED MODE ",          ~
                "Press PF16 to Abort")
            if keyhit% =  1% then mode$ = "IO   "
            if keyhit% =  2% then mode$ = "SHARE"
            if keyhit% = 16% then exit_program
            if keyhit% = 1% or keyhit% = 2% then L08000 else L03000

L08000: REM Check to see if SERMASTR/SERDETAL exist !!!!
            sermrec%, serdrec% = 200%
            rslt$(1), rslt$(2) = "REQUIRED"
            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            if f2%(1) <> 0% or f2%(2) <> 0% then no_file
               get rslt$(1) using L08080, sermrec%    /* Get Record Size */
               get rslt$(2) using L08080, serdrec%
L08080:                     FMT POS(17), BI(4)

               close #01 : close #02
               call "OPENFILE" (#01, mode$, f2%(01), rslt$(01), axd$)
               call "OPENFILE" (#02, mode$, f2%(02), rslt$(02), axd$)

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

            str(line2$,62) = "SERARCHV: " & str(cms2v$,,8)
            str(header$, 1,20) = "From               "
            str(header$,21,20) = "To                 "
            header2$ = "Master File Detail File"
            sernbrlen% = 20%
            call "READ100" (#5, "SWITCHS.HNY", f1%(5))
                 if f1%(5) = 0% then L10000
                 get #5 using L09170, sernbrlen$
L09170:                       FMT POS(42), CH(2)
                 convert sernbrlen$ to sernbrlen%, data goto L10000


L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            gosub initialize_results

            for fieldnr% = 1% to 6%
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
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <> 16% then L11110
                     if end% = 1% then exit_program
                     gosub initialize_results
                     goto archive_data
L11110:           if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  6% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
                  end% = 0%
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        archive_data

*        Get "DO YOU WANT TO PROCEED (Y/N) INFO"
            safeguard$ = " "
L19059:     gosub L42000
                if keyhit%  =  1 then editpg1
                if keyhit% <>  0 then L19059
            if safeguard$ <> "YES" then editpg1

            gosub create_records
L19070:     if miscflag$ = "Y" then L19080
               readkey$ = "4" & hex(00)
L19072:        call "PLOWALTS" (#1, readkey$, 2%, 1%, f1%(1))
                if f1%(1) = 0% then records_total
                get #1 using L19120, str(recm$())
                goto L19240

L19080:     readkey$ = part$(1) & hex(00)
            call "READ102" (#1, readkey$, f1%(1))
L19100:      if f1%(1) = 0% then records_total
             get #1 using L19120, str(recm$())
L19120:                   FMT    CH(300)
             if str(recm$(),1,1) = "4" then L19180
             if miscflag$ = "Y" and str(recm$(), 1, 1) = "9" then L19180
             goto L19275  /* Get next record */


L19180: REM  Record found is either an Invoiced or Misc.  Let's do it !!
             if fmarchdate$ = "ALL" then L19210
             if str(recm$(),177, 6) < unffmarchdate$ or                  ~
                str(recm$(),177, 6) > unftoarchdate$ then L19274

L19210: REM  If Misc (ie '9') No Invoice Numbers or Serial Numbers used

             if fminv$ = "ALL" then L19231
             if str(recm$(), 11,  8) < loinv$      or                    ~
                str(recm$(), 11,  8) > hiinv$      then L19274

L19231:      if fmsernbr$ = "ALL" then L19240
             if str(recm$(), 32, 20) < losernbr$   or                    ~
                str(recm$(), 32, 20) > hisernbr$   then L19274

L19240: REM  Check for Any Wild Card Used !!
             if str(wildcard$, 1, sernbrlen%) = " " then L19280
             k% = 0%
             for i% = 1% to sernbrlen%
                 if str(wildcard$,i%,1%) = "*" then L19254
                 if str(wildcard$,i%,1%) = str(recm$(),32%+(i%-1%), 1%)  ~
                                   then L19254 else k% = k% + 1%
L19254:      next i%
             if k% = 0% then L19280

L19274:      if miscflag$ = "N" then L19072
L19275:      call "READNEXT" (#1, f1%(1))
             goto L19100

L19280: REM  Passed all ranges tests.  Let's do the actual archiving !!
             gosub do_archive
             init (" ")  str(recm$())
             goto L19070

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Archive Date Range     */~
                              L20200,         /* Invoice Numbers Range  */~
                              L20300,         /* Serial Numbers Range   */~
                              L20400,         /* Archive File Year      */~
                              L20500,         /* Include Misc. Usage    */~
                              L20608          /* WildCard Serial Number */
            return

L20100: REM Def/Enable Archive Date Range          FMARCHDATE$
            if fmarchdate$ = " " or ~
               fmarchdate$ = blankdate$ then fmarchdate$ = "ALL"
            return

L20200: REM Def/Enable Invoice Numbers Range       FMINV$
            if fminv$ = " " then fminv$ = "ALL"
            return

L20300: REM Def/Enable Serial Numbers Range        FMSERNBR$
            if fmsernbr$ = " " then fmsernbr$ = "ALL"
            return

L20400: REM Def/Enable Archive File Year           ARCHYEAR$
            return

L20500: REM Def/Enable Include Misc. Usage (Y/N)?  MISCFLAG$
            if miscflag$ = " " then miscflag$ = "N"
            return

L20608: REM Def/Enable Serial Numbers WildCardN    WILDCARD$
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
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Archive Date Range                                     ",~
         "Enter Invoice Numbers Range                                  ",~
         "Enter Serial Numbers Range                                   ",~
         "Enter Archive File Year Name (i.e. 1988, 1989, etc...)       ",~
         "Enter Include Misc. Usage (Y/N)?                             ",~
         "Use '*' as Wild Card                                         "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            call "ALLFREE"
            init(" ") errormsg$, inpmessage$, wildcard$,                 ~
                      fmarchdate$, toarchdate$, archyear$,               ~
                      fminv$, toinv$, fmsernbr$, tosernbr$, miscflag$
            init(hex(00)) part$()
        return


        initialize_results
            hits%, eligibles%, writtens%, end% = 0%
            hitsd%, eligiblesd%, writtensd% = 0%
            hits$      = "# of Records Hit      =     " & "         0"
            eligibles$ = "# of Records Eligible =     " & "         0"
            writtens$  = "# of Records Written  =     " & "         0"
            hitsd$      = "         0"
            eligiblesd$ = "         0"
            writtensd$  = "         0"
        return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
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
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
                 if f2%(03) = 0% then close #03
            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))
                 if f2%(04) = 0% then close #04
            return clear all
            goto inputmode

        REM *************************************************************~
            *           D O   A R C H I V E                             *~
            *-----------------------------------------------------------*~
            * Perform Archiving (SERMASTR->SNMYYYY, SERDETAL->SNDYYYY)  *~
            *************************************************************

        do_archive
            l% = 1%
            call "SHOSTAT" ("Transfering Data to Archive Files...")

            part$(l%) = str(recm$(), 52,45)
            gosub'7(part$(l%))
            return

            deffn'7(assypart$)
            call "READ100" (#2, assypart$, f1%(2))
              if f1%(2) = 0% then L30180
                 l% = l% + 1%
                 get #2 using L30141, part$(l%)
L30141:                 FMT   CH(45)
L30150:          gosub'7(part$(l%))
            return

L30180:     call "READ100" (#1, part$(l%), f1%(1))
              if f1%(1) = 0% then L30224
                 hits%, eligibles%, writtens% = writtens% + 1%
                 get #1 using L30201, str(recm$())
L30201:                 FMT   CH(300)
                 get #1 using L30206, lastmod, timeenter
L30206:                 FMT   POS(202), BI(3), BI(4)
               call "READ101" (#3, part$(l%), f1%(3))
                 if f1%(3) = 0% then goto L30219
               put #3 using L30216, str(recm$(),,201), lastmod,           ~
                      timeenter, str(recm$(),209,92), data goto L30222
L30216:                   FMT CH(201), BI(3), BI(4), CH(92)
               rewrite #3
               goto L30222
L30219:        write #3 using L30216, str(recm$(),,201), lastmod,         ~
                      timeenter, str(recm$(),209,92), data goto L30222

L30222:        call "READ101" (#1, part$(l%), f1%(1))
               delete #1
L30224:    if l% = 1% then return
           readkey$ = str(part$(l%)) & str(part$(l% - 1%)) & hex(00)
           call "REDALT4" (#2, readkey$, 1%, f1%(2))
             if f1%(2) = 0% then L30280
               hitsd%, eligiblesd%, writtensd% = writtensd% + 1%
               get #2 using L30252, str(recd$(), 1,90), seqnbr,           ~
                          str(recd$(),94,27), qty(), str(recd$(),145,56)
L30252:               FMT CH(90), BI(3), CH(27), 3*PD(14,4), CH(56)
           readkey$ = str(part$(l%)) & str(part$(l% - 1%)) & hex(00)
           call "PLOWAL1" (#4, readkey$, 1%, 45%, f1%(4))
             if f1%(4) = 0% then L30275
               put #4 using L30252, str(recd$(), 1,90), seqnbr,           ~
                 str(recd$(),94,27), qty(), str(recd$(),145,56),         ~
                 data goto L30280
               rewrite #4
               goto L30278
L30275:        write #4 using L30252, str(recd$(), 1,90), seqnbr,         ~
                         str(recd$(),94,27), qty(), str(recd$(),145,56)

L30278:        call "REDALT5" (#2, readkey$, 1%, f1%(2))
               delete #2
L30280:    if l% = 1% then return
           l% = l% - 1%
           goto L30150


        records_total
            end% = 1%
            convert hits%      to str(hits$     ,29,10), pic(##########)
            convert eligibles% to str(eligibles$,29,10), pic(##########)
            convert writtens%  to str(writtens$ ,29,10), pic(##########)

            convert hitsd%      to hitsd$      , pic(##########)
            convert eligiblesd% to eligiblesd$ , pic(##########)
            convert writtensd%  to writtensd$  , pic(##########)
        goto editpg1

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              if fieldnr% > 0% then end% = 0%
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              if end% = 1% and fieldnr% = 0%  then                       ~
                                  str(pf$(3),64,16) = "(16)Exit Program"

              on fieldnr% gosub L40090,         /* Archive Date Range   */~
                                L40095,         /* Invoice Numbers Range*/~
                                L40095,         /* Serial Numbers Range */~
                                L40095,         /* Archive File Year    */~
                                L40095,         /* Include Misc. Usage  */~
                                L40095          /* WildCard Serial Nbr. */
              goto L40110

L40090:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40095:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40110:     accept                                                       ~
               at (01,02),                                               ~
                  "Archiving selection criteria",                        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),str(header$, 1, 20)     , ch(20),~
               at (06,52), fac(hex(ac)),str(header$,21, 20)     , ch(20),~
                                                                         ~
               at (07,02), "Archive Date Range",                         ~
               at (07,30), fac(lfac$( 1)), fmarchdate$          , ch(10),~
               at (07,52), fac(lfac$( 1)), toarchdate$          , ch(10),~
                                                                         ~
               at (08,02), "Invoice Numbers Range",                      ~
               at (08,30), fac(lfac$( 2)), fminv$               , ch(08),~
               at (08,52), fac(lfac$( 2)), toinv$               , ch(08),~
                                                                         ~
               at (09,02), "Serial Numbers Range",                       ~
               at (09,30), fac(lfac$( 3)), str(fmsernbr$,1,sernbrlen%),  ~
               at (09,52), fac(lfac$( 3)), str(tosernbr$,1,sernbrlen%),  ~
                                                                         ~
               at (10,02), "Archive File Year",                          ~
               at (10,30), fac(lfac$( 4)), archyear$            , ch(04),~
                                                                         ~
               at (11,02), "Include Misc. Usage (Y/N)?",                 ~
               at (11,30), fac(lfac$( 5)), miscflag$            , ch(01),~
                                                                         ~
               at (12,02), "Wild Card Serial Number"            ,        ~
               at (12,30), fac(lfac$( 6)), str(wildcard$,1,sernbrlen%),  ~
                                                                         ~
               at (14,29), fac(hex(a4)),   str(header2$, 1,11)  ,        ~
               at (14,44), fac(hex(a4)),   str(header2$,13,11)  ,        ~
               at (15,02), fac(hex(84)),   hits$                , ch(38),~
               at (16,02), fac(hex(84)),   eligibles$           , ch(38),~
               at (17,02), fac(hex(84)),   writtens$            , ch(38),~
               at (15,45), fac(hex(84)),   hitsd$               , ch(10),~
               at (16,45), fac(hex(84)),   eligiblesd$          , ch(10),~
               at (17,45), fac(hex(84)),   writtensd$           , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,65), fac(hex(84)),   str(pf$(3),64,)      ,        ~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40285
                  call "MANUAL" ("SERARCHV") : goto L40110

L40285:        if keyhit% <> 15 then L40300
                  call "PRNTSCRN" : goto L40110

L40300:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if end% = 1% and fieldnr% = 0% then return
        if edit% = 2% then L40395     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40375
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40375:     if fieldnr% >= 2% then L40385
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40385:     return

L40395: if fieldnr% > 0% then L40440  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Archive     "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40440:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

L42000: REM *************************************************************~
            *            S A F E G U A R D   S C R E E N   1            *~
            * --------------------------------------------------------- *~
            * Safeguards the accidental purging of data by asking if he *~
            * really wants to purge it.                                 *~
            *************************************************************
            getback$ = "(1)Return"
            str(dateline$,,60) = "Enter Safeguard Word"

L42080:     accept                                                       ~
               at (01,02),                                               ~
                  "Archiving Invoiced/Misc. Items"              ,        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (08,23), "**************************************",     ~
               at (09,23), "* ARCHIVING of data is about to      *",     ~
               at (10,23), "* begin. Do you wish to PROCEED?     *",     ~
               at (11,23), "*                                    *",     ~
               at (12,23), "*                                    *",     ~
               at (13,23), "*                                    *",     ~
               at (14,23), "*     Responses other than 'YES'     *",     ~
               at (15,23), "*    Will return you to Edit Mode    *",     ~
               at (16,23), "**************************************",     ~
                                                                         ~
               at (12,40), fac(hex(81)), safeguard$             , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)), blankline$             , ch(79),~
                                                                         ~
               at (22,02), fac(hex(84)), getback$               ,        ~
               at (22,65), "(13)Instructions"                   ,        ~
               at (23,65), "(15)Print Screen"                   ,        ~
                                                                         ~
               keys(hex(00010d0f)),                                      ~
               key (keyhit%)

               if keyhit% <> 13 then L42400
                  call "MANUAL" ("HNYARCHV")
                  goto L42080

L42400:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L42080

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* Archive Date Range     */~
                              L50200,         /* Invoice Numbers Range  */~
                              L50300,         /* Serial Numbers Range   */~
                              L50400,         /* Archive File Year      */~
                              L50500,         /* Include Misc. Usage    */~
                              L50700          /* WildCard Serial Number */
            return
L50100: REM Test for Archive Date Range         FMARCHDATE$
            fmarchdate%, toarchdate% = 0%
            if fmarchdate$ = "ALL" then return
            if ( fmarchdate$ <> " " and fmarchdate$ <> blankdate$ ) and ~
                 toarchdate$ = " " then toarchdate$ = fmarchdate$
            call "DATEOKC" (fmarchdate$, fmarchdate%, errormsg$)
                 if errormsg$ <> " " then return
            call "DATEOKC" (toarchdate$, toarchdate%, errormsg$)
                 if errormsg$ <> " " then return
            unffmarchdate$ = fmarchdate$
            unftoarchdate$ = toarchdate$
            call "DATUFMTC" (unffmarchdate$)
            call "DATUFMTC" (unftoarchdate$)
            if (unffmarchdate$ >  unftoarchdate$)                        ~
                then errormsg$ = "From may not be greater then To."
            return

L50200: REM Test for Invoice Numbers Range        FMINV$
            call "TESTRNGE" (fminv$, toinv$, loinv$, hiinv$, errormsg$)
            return

L50300: REM Test for Serial Numbers Range         FMSERNBR$
            call "TESTRNGE" (fmsernbr$, tosernbr$, losernbr$, hisernbr$, ~
                            errormsg$)
            return

L50400: REM Test for Archive File Year            ARCHYEAR$
            if archyear$ = " " then                                      ~
               errormsg$ = "Archive File Year Name is Require."
            if len(archyear$) < 4 then                                   ~
               errormsg$ = "Length may not be less than 4."
            return

L50500: REM Test for Include Misc. Usage (Y/N)?   MISCFLAG$
            if pos("YN" = miscflag$) = 0 then                            ~
               errormsg$ = "Miscellaneous Usage must be 'Y' or 'N'."
            return

L50700: REM Test for WildCard Serial Numbers      WILDCARD$
            return

        no_file
            ask% = 0%
            call "ASKUSER" (ask%, "***** FILE NOT OPENED *****",         ~
                 "The required file was NOT OPENED!",                    ~
                 "Either it doesn't exist or it could not be opened "  & ~
                 "EXCLUSIVELY",                                          ~
                 "Press PF-16 to end this program")
            if ask% <> 16% then no_file
            goto exit_program

        create_records
            call "SHOSTAT" ("Creating or Opening Archive Files")
            sermprname$="SERM" & archyear$
            call "PUTPRNAM" addr(#3, sermprname$)
            serdprname$="SERD" & archyear$
            call "PUTPRNAM" addr(#4, serdprname$)
            sermrec% = max(1%, sermrec%/2%)
            serdrec% = max(1%, serdrec%/2%)
            call "OPENCHCK"(#3, fs%(3), f2%(3), 0%, rslt$(3))
                 if f2%(3) <> 0% then L60160 else L60161
L60160:     call "OPENCHCK"(#3, fs%(3), f2%(3), sermrec%, rslt$(3))
L60161:     call "OPENCHCK"(#4, fs%(4), f2%(4), 0%, rslt$(4))
                 if f2%(4) <> 0% then L60170 else return
L60170:     call "OPENCHCK"(#4, fs%(4), f2%(4), serdrec%, rslt$(4))

            close #03 : close #04
            call "OPENFILE" (#03, mode$, f2%(03), rslt$(03), axd$)
            call "OPENFILE" (#04, mode$, f2%(04), rslt$(04), axd$)
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
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
