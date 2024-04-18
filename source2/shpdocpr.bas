        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   H   H  PPPP   DDDD    OOO    CCC   PPPP   RRRR    *~
            *  S      H   H  P   P  D   D  O   O  C   C  P   P  R   R   *~
            *   SSS   HHHHH  PPPP   D   D  O   O  C      PPPP   RRRR    *~
            *      S  H   H  P      D   D  O   O  C   C  P      R   R   *~
            *   SSS   H   H  P      DDDD    OOO    CCC   P      R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SHPDOCPR - PERMITS ENTRY OF CRITERIA -- DOCUMENT(S) TO    *~
            *            PRINT, STORE RANGE, AND SCHEDULED SHIP DATE    *~
            *            RANGE.  THEN PRINT BILLS OF LADING AND/OR PICK *~
            *            LISTS BASED ON THESE CRITERIA.  PRINTING IS    *~
            *            DRIVEN BY THE ENTRIES IN BCKPRIDX.             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/23/86 ! Original                                 ! JIM *~
            * 05/14/87 ! Std Cost Changes                         ! ERN *~
            * 08/11/87 ! HES's SO, PF8, PF9 Ploymer customizations! JIM *~
            * 08/23/88 ! Removed call to Plowcode after SO entry  ! RJM *~
            * 09/22/88 ! Added PF8 to View SO Documents on file   ! RJM *~
            * 10/18/88 ! Moved variables from COM to DIM.         ! JDH *~
            * 03/01/90 ! modified COM statement.  Changed SHIP_TO$! LAB *~
            *          ! from 30 to 31                            ! LAB *~
            * 02/13/91 ! Made Full Screen Entry/Edit.             ! JDH *~
            * 06/14/91 ! Added Shipping Label function (SHPLABSB).! JIM *~
            * 01/06/92 ! Added default of 'N' for label printing. ! JDH *~
            * 02/21/92 ! PRR 11690  Optional print of price on PL.! JDH *~
            * 10/22/92 ! Changed COMs to DIMs & added Arguments.  ! JDH *~
            * 03/10/93 ! Added PF17 Set Defualts.                 ! JDH *~
            * 09/18/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        REM *************************************************************~
            *THIS PROGRAM CALLS TWO SUBROUTINES BASED ON OPERATOR INPUT.*~
            *THE SUBROUTINE NAMES ARE 'SHPBOLS' - PRINT BILLS OF LADING,*~
            *AND 'SHPPICKS' - PRINT PICK LISTS.                         *~
            *************************************************************

        dim        /* OLD COM VARIABLES                                */~
            date$8,                      /* Date for screen display    */~
            from_stor$3,                 /* Low store for compare      */~
            hdr$40,                      /* ASKUSER constant           */~
            high_date$10,                /* High date for capture      */~
            i$(24)80,                    /* Screen Image               */~
            low_date$10,                 /* Low date for capture       */~
            msg$(3)79,                   /* ASKUSER constant           */~
            print_labels$1,              /* Print Shipping Labels?     */~
            print_price$1,               /* Print Price on Pick List?  */~
            so$(2,2)16,                  /* Sales Order # Range For Pri*/~
            to_stor$3,                   /* High store for compare     */~
            userid$3,                    /* Current User Id            */~
                   /* COMMON DATA FILE ARRAYS                          */~
            f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        dim        /* LOCAL VARIABLES                                  */~
            code$1,                      /* 1= BOLs, 2=Picks, 3= both  */~
            codedescr$24,                /* Description of CODE$       */~
            descr$40,                                                    ~
            descr_map(8),                                                ~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            high_stor$3,                 /* High store for capture     */~
            incl(2),                                                     ~
            incl$(2)6,                                                   ~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            low_stor$3,                  /* Low store for capture      */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf17$17, pf17k$1             /* PF17 Screen Literal & Key  */



        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            mat f2% = con : mat f1% = zer

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #02 ! SYSFILE2 ! Caelus Management System General Informa *~
            * #3  ! BCKPRIDX ! SO Document Print Index File             *~
            * #4  ! BCKMASTR ! BACKLOG MASTER FILE (GET STORE NUMBER)   *~
            * #5  ! BCKLINES ! BACK LOG LINE ITEM FILE                  *~
            * #6  ! SHPHDRS  ! Shipment Scheduling / Pre-Invoicing- Hea *~
            * #7  ! SHPLINES ! Shipment Scheduling / Pre-Invoicing- Lin *~
            * #64 !          ! DUMMY SELECT FOR PLOWCODE                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #02,  "SYSFILE2",                                     ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #3,  "BCKPRIDX",                                      ~
                        varc,     indexed,  recsize =   45,              ~
                        keypos =   11, keylen =  29,                     ~
                        alt key  1, keypos =    1, keylen =  39          ~

            select #4,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup     ~

            select #5,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19                      ~

            select #6,  "SHPHDRS",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  28                      ~

            select #7,  "SHPLINES",                                      ~
                        varc,     indexed,  recsize = 600,               ~
                        keypos =   10, keylen =  22                      ~

            select #64, "DUMMY",                                         ~
                        varc,     consec,  recsize = 64


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#02, fs%(2%), f2%(2%), 0%, rslt$(2%))
            call "OPENCHCK" (#3,  fs%(3%), f2%(3%), 0%, rslt$(3%))
            if fs%(3) > 0% then get rslt$(3 ) using L09110, nbr_recs%
L09110:         FMT POS(17), BI(4)
            call "OPENCHCK" (#4,  fs%(4%), f2%(4%), 0%, rslt$(4%))
            call "OPENCHCK" (#5,  fs%(5%), f2%(5%), 0%, rslt$(5%))
            call "OPENCHCK" (#6,  fs%(6%), f2%(6%), 0%, rslt$(6%))
            call "OPENCHCK" (#7,  fs%(7%), f2%(7%), 0%, rslt$(7%))

            if nbr_recs% > 0% then L09310
L09240:         comp% = 2%
                hdr$ = "*** CANCEL REQUEST ***"
                msg$(1) = "There is no Print Index activity to analyze"
                msg$(3) = "Press RETURN to cancel program"
                call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
                if comp% <> 0% then L09240
                goto exit_program /* NOTHING TO DO; DON'T DO ANYTHING */
L09310:     call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

*        See if operator is an administrator or not
            call "CMSMACHK" ("BCK", lfac$(1%), lfac$(2%))
            if lfac$(1%) = "Y" or lfac$(2%) = "Y" then admin% = 1%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf16$ = "(16)Exit Program" : pf17$ = " " : pf17k$ = hex(ff)
            init(" ") errormsg$, inpmessage$, code$, high_stor$,         ~
                high_date$, codedescr$, print_labels$
            low_stor$, low_date$, so$() = "ALL"
            print_labels$ = "N"   /* Default value */
            print_price$  = "N"   /* Default value */
            editing_defaults% = 0%
            gosub get_defaults
            if admin% <> 1% then L10120
                pf17$ = "(17)Edit Defaults"
                pf17k$ = hex(11)

L10120:     for fieldnr% = 1% to 1%
                gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0% then L10250
L10150:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 17% then goto  edit_defaults
                      if keyhit%  = 16% then       exit_program
                      if keyhit% <>  0% then       L10150
L10250:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10150
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            inpmessage$ = edtmessage$
            pf17$ = " " : pf17k$ = hex(ff)
            pf16$ = "(16)Print Data"
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       edtpg1
            fieldnr% = 1%
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
                  pf16$ = " "
L11200:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11200
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11200
            goto edtpg1

        edit_defaults
            editing_defaults% = 1%
            gosub read_defaults
            inpmessage$ = "Enter Defaults.  Date fields should be 'ALL'"&~
                          " or Offsets."
            pf16$ = "(16)Save Default"
L11560:     gosub'102(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       write_defaults
                  if keyhit% <>  0% then       L11560
            fieldnr% = 1%
L11640:     gosub'102(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11640
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11640
            goto L11560

        get_defaults
            call "READ100" (#02, "SHPDOCPR.DEFAULTS   ", f1%(2%))
            if f1%(2%) = 1% then read_defaults
                high_date$ = " " : low_date$ = "ALL" : code$ = "1"
                goto write_defaults

        read_defaults
            get #02 using L12100, code$, low_stor$, high_stor$, low_date$,~
                          high_date$, print_labels$, print_price$
L12100:         FMT POS(21), CH(1), 4*CH(3), 2*CH(1)
            if editing_defaults% = 1% then return
            if low_date$ = "ALL" then return
                if low_date$ = " " then L12170
                     convert low_date$ to offset%
                     call "DATE" addr ("G+", date, offset%, low_date$,   ~
                                       u3%)
                     call "DATFMTC" (low_date$)
L12170:         if high_date$ = " " then L12220
                     convert high_date$ to offset%
                     call "DATE" addr ("G+", date, offset%, high_date$,  ~
                                       u3%)
                     call "DATFMTC" (high_date$)
L12220:         if high_date$ = " " and low_date$ = " " then             ~
                                                        low_date$ = "ALL"
                return

        write_defaults
            call "READ101" (#02, "SHPDOCPR.DEFAULTS   ", f1%(2%))
            put #02 using L12320, "SHPDOCPR.DEFAULTS   ", code$,          ~
                                 low_stor$, high_stor$, low_date$,       ~
                                 high_date$, print_labels$, print_price$,~
                                 " "
L12320:         FMT CH(20), CH(1), 4*CH(3), 2*CH(1), CH(465)
            if f1%(2%) = 1% then rewrite #02 else write #02
            goto inputmode

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if low_date$ <> " " and low_date$ <> "ALL" and low_date$ <> "LAST" ~
               then call "DATUFMTC" ( low_date$ )
            if high_date$ <> "LAST" then call "DATUFMTC" ( high_date$ )
            date$ = date

            select printer
            if code$ = "1" then call "SHPBOLS" (date$, from_date%,       ~
                from_stor$, i$(), low_date$, print_labels$, so$(),       ~
                to_date%, to_stor$, #3, #4, #5, #6, #7)

            if code$ = "2" then call "SHPPICKS" (date$, from_date%,      ~
                high_date$, from_stor$, i$(), low_date$, print_labels$,  ~
                print_price$, so$(), to_date%, to_stor$, #3, #4, #5, #6, ~
                #7)

            if code$ = "3" then call "SHPBOLS" (date$, from_date%,       ~
                from_stor$, i$(), low_date$, print_labels$, so$(),       ~
                to_date%, to_stor$, #3, #4, #5, #6, #7)

            if code$ = "3" then call "SHPPICKS" (date$, from_date%,      ~
                high_date$, from_stor$, i$(), low_date$, print_labels$,  ~
                print_price$, so$(), to_date%, to_stor$, #3, #4, #5, #6, ~
                #7)

            if low_date$ <> " " and low_date$ <> "ALL" and low_date$ <> "LAST" ~
               then call "DATFMTC" ( low_date$ )
            if high_date$ <> "LAST" then call "DATFMTC" ( high_date$ )
            call "DATEFMT" (date$)

            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%

            REM DOCUMENT(S) TO PRINT                  CODE$
            inpmessage$ = "Enter CODE for document(s) (1=BOLs, 2=Pick L"&~
                "ists, 3=Both), Ranges & Options"
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
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

            deffn'101(fieldnr%)
                  str(line2$,62%) = "SHPDOCPR: " & str(cms2v$,,8%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()

                  if fieldnr% = 0% then L40270
                      init(hex(81)) lfac$()
                      lfac$(1) = hex(82)

L40270:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Bills of Lading and/or Pick Lists",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Document(s) to Print",                       ~
               at (06,28), fac(lfac$( 1)), code$                , ch(01),~
               at (06,31), fac(hex(8c)), codedescr$             , ch(24),~
                                                                         ~
               at (07,02), "Range of Stores",                            ~
               at (07,28), fac(lfac$( 2)), low_stor$            , ch(03),~
               at (07,50), fac(lfac$( 2)), high_stor$           , ch(03),~
                                                                         ~
               at (08,02), "Sched. Ship Date range",                     ~
               at (08,28), fac(lfac$( 3)), low_date$            , ch(10),~
               at (08,50), fac(lfac$( 3)), high_date$           , ch(10),~
                                                                         ~
               at (09,02), "Sales Order # range",                        ~
               at (09,28), fac(lfac$( 4)), so$(1,1)             , ch(16),~
               at (09,50), fac(lfac$( 4)), so$(1,2)             , ch(16),~
                                                                         ~
               at (10,02), "Print Shipping Labels?",                     ~
               at (10,28), fac(lfac$( 5)), print_labels$        , ch(01),~
                                                                         ~
               at (11,02), "Print Price on Pick List?",                  ~
               at (11,28), fac(lfac$( 6)), print_price$         , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
               at (22,02), "(1)Start Over",                              ~
               at (23,02), fac(hex(8c)), pf17$                          ,~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,20), "(8)View SO Documents on File",               ~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(0001080d0f10) & pf17k$), key (keyhit%)

               if keyhit% <> 13% then L40840
                  call "MANUAL" ("SHPDOCPR")
                  goto L40270

L40840:        if keyhit% <> 15% then L40872
                  call "PRNTSCRN"
                  goto L40270

L40872:        if keyhit% <> 8% then L40880
                  gosub so_plowcode
                  goto L40270

L40880:        if fieldnr% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen for Defaults.              *~
            *************************************************************

            deffn'102(fieldnr%)
                  str(line2$,62%) = "SHPDOCPR: " & str(cms2v$,,8%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()

                  if fieldnr% = 0% then L41150
                      init(hex(81)) lfac$()
                      lfac$(1) = hex(82)

L41150:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Bills of Lading and/or Pick Lists",             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,28), "** DEFAULTS **",                             ~
               at (06,02), "Document(s) to Print",                       ~
               at (06,28), fac(lfac$( 1)), code$                , ch(01),~
               at (06,31), fac(hex(8c)), codedescr$             , ch(24),~
                                                                         ~
               at (07,02), "Range of Stores",                            ~
               at (07,28), fac(lfac$( 2)), low_stor$            , ch(03),~
               at (07,34), fac(lfac$( 2)), high_stor$           , ch(03),~
                                                                         ~
               at (08,02), "Sched. Ship Date range",                     ~
               at (08,28), fac(lfac$( 3)), low_date$            , ch(03),~
               at (08,34), fac(lfac$( 3)), high_date$           , ch(03),~
                                                                         ~
               at (09,02), "Print Shipping Labels?",                     ~
               at (09,28), fac(lfac$( 4)), print_labels$        , ch(01),~
                                                                         ~
               at (10,02), "Print Price on Pick List?",                  ~
               at (10,28), fac(lfac$( 5)), print_price$         , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(00010d0f10)), key (keyhit%)

               if keyhit% <> 13% then L41550
                  call "MANUAL" ("SHPDOCPR")
                  goto L41150

L41550:        if keyhit% <> 15% then L41590
                  call "PRNTSCRN"
                  goto L41150

L41590:        if fieldnr% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "

        REM DOCUMENT(S) TO PRINT                      CODE$
            if code$ = " " and keyhit% = 0% then code$ = "1" /*DEFAULT*/
            if code$ <> "1" then L50170
                codedescr$ = "(Bills of Lading)" : goto L50240
L50170:     if code$ <> "2" then L50190
                codedescr$ = "(Pick Lists)" : goto L50240
L50190:     if code$ <> "3" then L50210
                codedescr$ = "(Both Documents)" : goto L50240
L50210:     errormsg$ = "You must enter either '1', '2' or '3'" &        ~
                        " for Document(s) to Print Code."
                return

L50240: REM RANGE OF STORES                           FROM_STOR$
            call "TESTRNGE" (low_stor$, high_stor$, from_stor$, to_stor$,~
                errormsg$)
            if errormsg$ = " " then L50270
                errormsg$ = errormsg$ & " (Store Range)"
                return
L50270:     from_stor$ = addc (hex(01))

        REM SCHEDULED SHIP DATE RANGE                 FROM_DATE$
            from_date% = 0% :  to_date% = 999999%
            if low_date$ = " " and high_date$ = " " then goto L50440
            if low_date$ = " " then goto L50400
            if low_date$ = "ALL" then goto L50440
            if low_date$ = "FIRST" then goto L50400
                call "DATEOKC" (low_date$, from_date%, errormsg$)
                if errormsg$ <> " " then return
                if high_date$ = "LAST" then goto L50440
                if high_date$ <> " " then goto L50420
                     high_date$ = low_date$ : to_date% = from_date%
                     goto L50440
L50400:     if high_date$ = " " then high_date$ = "LAST"
            if high_date$ = "LAST" then goto L50440
L50420:     call "DATEOKC" (high_date$, to_date%, errormsg$)
            if errormsg$ <> " " then return
L50440:     if from_date% > to_date% then errormsg$ =                    ~
                "The FROM Date must be EARLIER than the TO Date"
            if errormsg$ <> " " then return

        REM SO NUMBER RANGE                       SO$()
            if so$(1,1) <> "ALL" then goto L50520
                so$(1,2) = " "
                goto L50620
L50520: REM Test the beginning Sales Order number
*          IF SO$(1,1) = "FIRST" THEN GOTO 50570
*          ERRORMSG$ = HEX(06) & "Enter the beginning S.O. number"
*          X% = 1%
*          GOSUB SO_PLOWCODE
        REM Test the ending Sales Order number
*          IF SO$(1,2) = "LAST" THEN GOTO 50620
*          ERRORMSG$ = HEX(06) & "Enter the ending S.O. number"
*          X% = 2%
*          GOSUB SO_PLOWCODE
L50620: REM Test for a valid range of Sales Order numbers
            call "TESTRNGE" (so$(1,1), so$(1,2), so$(2,1), so$(2,2),     ~
                errormsg$)
            if errormsg$ = " " then goto L50661
                errormsg$ = errormsg$ & " (SO# Range)"
                return

L50661: REM Validate Print Shipping Labels code       PRINT_LABELS$
            if print_labels$ = "Y" or print_labels$ = "N" then L50670
            errormsg$ = "Print Shipping Labels should be 'Y' or 'N'."
            return

L50670: REM Validate Print Price on Pick List         PRINT_PRICE$
            if print_price$ = "Y" or print_price$ = "N" then return
            errormsg$ = "Print Price on Pick List should be 'Y' or 'N'."
            return

        so_plowcode
            plowkey$ = xor plowkey$
            mat incl = zer
            incl$() = " "
            descr_map(1) = 11.01  :  descr_map(2) = 1
            descr_map(3) = 12.09  :  descr_map(4) = 6
            descr_map(5) = 21.16  :  descr_map(6) = 16
            descr_map(7) = 37.03  :  descr_map(8) = 33
            msg$(1) = " Type  Customer  Sales Order No.  BOL"
            msg$(2) = " "
            msg$(3) = "Sales Order Documents On File."
            descr$ = hex(06) & "Press PF(16) to End Display."

            if code$ = "2"                                               ~
                then str(plowkey$,,1) = "P"                              ~
                else str(plowkey$,,1) = "B"
            incl(1) = -1.01   :   incl$(1) = "A"
            if code$ <> "1" then L50740
               incl(2) = -1.01  : incl$(2) = "P"

L50740:     call "PLOWCODE" (#3, plowkey$, descr$, 9000%, 0.39, f1%(3),  ~
                             msg$(), 0, 0, incl(), incl$(), "d", "Y",    ~
                             #64, descr_map())

*          IF F1%(3) <> 0% THEN SO$(1, X%) = STR(PLOWKEY$,11,16)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "

        REM DOCUMENT(S) TO PRINT                      CODE$
            if code$ <> "1" then L51130
                codedescr$ = "(Bills of Lading)" : goto L51210
L51130:     if code$ <> "2" then L51150
                codedescr$ = "(Pick Lists)" : goto L51210
L51150:     if code$ <> "3" then L51170
                codedescr$ = "(Both Documents)" : goto L51210
L51170:     errormsg$ = "You must enter either '1', '2' or '3'" &        ~
                        " for Document(s) to Print Code."
                return

L51210: REM RANGE OF STORES                           LOW_STOR$
            call "TESTRNGE" (low_stor$, high_stor$, from_stor$, to_stor$,~
                errormsg$)
            if errormsg$ = " " then L51290
                errormsg$ = errormsg$ & " (Store Range)"
                return

L51290: REM SCHEDULED SHIP DATE RANGE                 LOW_DATE$
            if low_date$ = "ALL" then L51350
                if low_date$ = " " then L51315
                    convert low_date$  to temp1, data goto L51370
L51315:         if high_date$ = " " then L51360
                    convert high_date$ to temp2, data goto L51370
                if low_date$ = " " then temp1 = -9e7
                if temp1 > temp2 then L51390
                goto L51700
L51350:     high_date$ = " "
L51360:     if low_date$ = " " and high_date$ = " " then low_date$ = "ALL"
            goto L51700
L51370:         errormsg$ = "Invalid Date Range.  Enter blank, 'ALL'" &  ~
                            " or Offset Numeric."
                return
L51390:         errormsg$ = "Invalid Date Range Offset.  'From' Offset "&~
                            "cannot be greater than 'To' Offset."
                return

L51700: REM Validate Print Shipping Labels code       PRINT_LABELS$
            if print_labels$ = "Y" or print_labels$ = "N" then L51750
            errormsg$ = "Print Shipping Labels should be 'Y' or 'N'."
            return

L51750: REM Validate Print Price on Pick List         PRINT_PRICE$
            if print_price$ = "Y" or print_price$ = "N" then return
            errormsg$ = "Print Price on Pick List should be 'Y' or 'N'."
            return


        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
