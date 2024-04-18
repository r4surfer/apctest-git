        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  PPPP   IIIII   GGG   EEEEE  N   N   *~
            *  H   H  NN  N  Y   Y  P   P    I    G      E      NN  N   *~
            *  HHHHH  N N N   YYY   PPPP     I    G GGG  EEEE   N N N   *~
            *  H   H  N  NN    Y    P        I    G   G  E      N  NN   *~
            *  H   H  N   N    Y    P      IIIII   GGG   EEEEE  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYPIGEN - This program begins the Physical Inventory     *~
            *            process by generating an electronic image of   *~
            *            the inventory tickets which will be used to    *~
            *            count the inventory items or parts.  Items to  *~
            *            be counted are determined via user selected    *~
            *            parameters and options.                        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/10/85 ! ORIGINAL                                 ! LDJ *~
            * 06/09/86 ! Corrected bug in Ticket Length due to    ! LDJ *~
            *          !   use of function LGT, use LOG instead   !     *~
            * 03/18/87 ! Enhancement change; Now keeps snapshot   ! LDJ *~
            *          !   of HNYQUAN in separate file, destinct  !     *~
            *          !   from Tickets.  Significant file layout !     *~
            *          !   changes for this reason and in prep for!     *~
            *          !   next major release (12 cost buckets!). !     *~
            * 04/29/87 ! Bug Fix - Ticket length generated not    ! LDJ *~
            *          !   long enuff if user starts with a       !     *~
            *          !   high ticket number.                    !     *~
            * 05/21/87 ! Std Costs Enhancements                   ! MJB *~
            * 08/08/89 ! Data validation enhancements/ PRR 11044. ! MLJ *~
            * 02/13/90 ! Added 4th Alt. Key for HNYLOCNS file.    ! JEF *~
            * 10/30/91 ! CMS/DEC 'MASK' Project/Added 'ALLFREE'   ! SID *~
            * 03/17/92 ! Cycle Counting Project Integration.      ! SID *~
            * 03/30/92 ! Removed STR(KEY(#4),3,12) = at ln 33811  ! MJB *~
            *          !  and ticket # added to PUT statement.    !     *~
            * 07/07/92 ! Added the ability to exclude the snap    ! SID *~
            *          !  shot of qty. & cost.                    !     *~
            *          ! Moved the print via lot or location      !     *~
            *          !  option from 2nd screen to the 1st screen!     *~
            * 09/21/92 ! Changed ABC$ from CH(4) to CH(5) at the  ! SID *~
            *          !  format statement.                       !     *~
            * 09/25/92 ! Add Location 'Snapshot' File             ! RJH *~
            * 09/29/92 ! PRR 12610  Include last range sel. item. ! RJH *~
            * 10/13/92 ! Stop '9' extra tickets from adding an    ! RJH *~
            *          !  extra '0' to ticket name.               !     *~
            *          ! Present Different SHOSTAT Msg. per       !     *~
            *          !  PIC_FLAG$.                              !     *~
            * 03/01/93 ! PRR 12815 - 2nd & 3rd Part Range fix.    ! RJH *~
            *          ! Change Category range selection to allow !     *~
            *          !  the 1st array to be frm " " to " ".     !     *~
            * 05/04/93 ! Changed breakpoint at #31700 from 34% to ! MLJ *~
            *          !  28% in order to pickup all lots & locs  !     *~
            *          !  for a given part/store when using 'L'.  !     *~
            *          ! Now prints LOT$ when using 'L'.          !     *~
            *          ! Corrected HNYCCMST Select clause.        !     *~
            * 05/19/93 ! Changed Looping logic to reset Cycle     ! RJH *~
            *          !  Activity Flags when deleting a PI Sessn.!     *~
            * 09/02/93 ! For CC Parts, insure HNYQUAN Record.     ! RJH *~
            * 10/01/93 ! PRR 13026 - For All Parts, insure HNYQUAN! RJH *~
            *          !  Record (that is reverse 05/04/93).      !     *~
            *          !  Allow blank tickets when Cycle Counting !     *~
            *          !   but only for additional Locations.     !     *~
            * 11/11/93 ! Increase OPEN Array variable size to 64  ! RJH *~
            * 06/27/94 ! HNYPICST opened even if HNYQUAN < 4 recs.! JDH *~
            * 10/14/94 ! PRR 13302 - Correct Sort parameters.     ! RJH *~
            * 11/01/94 ! Fixed short key readnext problem.        ! JDH *~
            * 08/29/95 ! PRR 13499 - Branch at line 51350 now goes! MLJ *~
            *          !   to 51362 instead of 51365.             !     *~
            * 08/08/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            abc$5,                       /* ABC Classes to Count       */~
            abc_code$1,                  /* Part ABC class code        */~
            askmsg$(2)79,                /* Askuser Message            */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            blank_only$3,                /* Create Blank Tickets Only? */~
            cat$4,                       /* Part Category              */~
            cat$(9,2)4,                  /* Categories to Count (Range)*/~
            cc_key$57,                   /* Key for Reading HNYCCDTL   */~
            check_digit$1,               /* Calc & Append Check Digit? */~
            cost(12),                    /* HNYQUAN Cost Fields        */~
            cost$96,                     /* for PACKZERO               */~
            count_date$8,                /* Exclude parts counted after*/~
            countdate$6,                 /* Last Count Date            */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            descr_m(6),                  /* Descr Map For PLOWCODE     */~
            description$30,              /* Count Session Notes        */~
            descr$42,                    /* Dummy Arg for PLOWCODE     */~
            edtmessage$79,               /* Edit screen message        */~
            endpart$25,                  /* Ending Part to process     */~
            errormsg$79,                 /* Error message              */~
            error$79,                    /* Error message              */~
            extra$6,                     /* Number of Extra Tickets    */~
            file$8,                      /* Work File Name             */~
            filekey$100,                 /* Miscell Read / Plow Key    */~
            filler$(2)256,               /* filler                     */~
            firstpart$44,                /* First Part to plow from    */~
            glvar$1,                     /* Update G/L variances flag  */~
            header$(3)79,                /* For PLOWCODE Call          */~
            hnyvar$1,                    /* Update HNY variances Flag  */~
            i$(24)80,                    /* Screen image               */~
            identity(1,13),              /* Identity matrix for Totalng*/~
            inc(2),                      /* Plowcode Arguments         */~
            inc$(2)16,                   /* Plowcode Arguments         */~
            inpmessage$79,               /* Input message              */~
            lastpart$25,                 /* Last Part Number Processed */~
            lastticket$6,                /* Last Ticket Nbr Processed  */~
            lfac$(20)1,                  /* Field attribute characters */~
            lib$8,                       /* Work File Library          */~
            line2$79,                    /* Second Header line on scrns*/~
            loc$8,                       /* Part Location Code         */~
            lockey$50,                   /* Misc Location Read Key     */~
            lot$16,                      /* Lot Code                   */~
            lot_or_loc$1,                /* Tickets by Lot or Loc Flag */~
            neg_only$1,                  /* Count Neg On-Hand Only ?   */~
            option$1,                    /* 'C' Cycle Count 'P' PI     */~
            part$25,                     /* Part Code                  */~
            part$(3,2)25,                /* Parts to Count (Ranges)    */~
            part_req$1,                  /* Part Numbers required ?    */~
            pfkeys$18,                   /* PF Keys                    */~
            pfs1$(2)79,                  /* Screen 1 PF Key Literals   */~
            pfs2$(2)79,                  /* Screen 2 PF Key Literals   */~
            pfs3$(2)79,                  /* Screen 3 PF Key Literals   */~
            pf13$19,                     /* PF-13 Literal              */~
            pic_flag$1,                  /* Snapshot Files Flag (Y/N)  */~
            plowkey$100,                 /* Misc Read / Plow Key       */~
            prefix$3,                    /* Ticket Number Prefix (Opt) */~
            print$1,                     /* Print tickets or sheets    */~
            readkey$100,                 /* Misc Read / Plow Key       */~
            record$(2)256,               /*  Work File Record          */~
            reject$1,                    /* Elegibility Test Flag      */~
            seq$1,                       /* Ticket Number Sequence     */~
            sessiondescr$30,             /* Cycle Count Session Descr. */~
            session_cc$12,               /* Cycle Count Session Nbr    */~
            session_date$8,              /* Planned Count Date         */~
            session_nbr$2,               /* Session Number Assigned    */~
            sort$116,                    /* Sort parameters to SORTCALL*/~
            sourceflag$1,                /* C-Cycle Count P-Physical   */~
            start_ticket$6,              /* Starting Ticket Number     */~
            store$3,                     /* Store / Warehouse Code     */~
            temp_ssnbr$2,                /* Session Number TEST        */~
            testkey$13,                  /* CC_session test            */~
            ticket$13,                   /* Formatted Ticket Number    */~
            vol$6,                       /* Work File Volume           */~
            whse$(9,2)3                  /* Warehouses to Count (Range)*/~

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if read was successful */~
            rslt$(64)20,                 /* Text from file opening     */~
            axd$(64)4                    /* Alt key pointer from Open  */

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
            * #1  ! HNYMASTR ! Inventory Master File                    *~
            * #2  ! HNYQUAN  ! Inventory Store Quantity File            *~
            * #3  ! HNYPISYS ! Physical Inventory System Session Contro *~
            * #4  ! HNYPITKT ! Physical Inventory Ticket File           *~
            * #5  ! STORNAME ! Store Names and Addresses                *~
            * #6  ! CATEGORY ! Inventory Category Descriptions          *~
            * #7  ! HNYLOCNS ! Part Locations Master File               *~
            * #8  ! HNYPILOC ! Physical Inventory Location Snapshot File*~
            * #9  ! HNYPICST ! Physical Inventory Costs Snapshot File   *~
            * #10 ! HNYCCMST ! Cycle Count Master File                  *~
            * #11 ! HNYCCDTL ! Cycle Count Session Detail File          *~
            * #12 ! HNYCCSYS ! Cycle Count System Control File          *~
            * #50 ! WORKFILE ! Work file for Sorting Tickets            *~
            * #55 ! DUMMY    ! For PLOWCODE Extended Arguments          *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #2,  "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  650,                                  ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #3,  "HNYPISYS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 512,                                   ~
                        keypos =    7, keylen =   2,                     ~
                        alt key  1, keypos =    1, keylen =   8

            select #4,  "HNYPITKT",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 512,                                   ~
                        keypos =    1, keylen =  15,                     ~
                        alt key  1, keypos =   16, keylen =  52, dup,    ~
                            key  2, keypos =  313, keylen =  16

            select #5,  "STORNAME",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 3

            select #6,  "CATEGORY",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 200,                                  ~
                         keypos = 1, keylen = 4

            select #7,  "HNYLOCNS",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 700,                                  ~
                         keypos = 1, keylen = 42,                        ~
                        alt key  1, keypos =  443, keylen =  42,         ~
                            key  2, keypos =  485, keylen =  42,         ~
                            key  3, keypos =  527, keylen =  42,         ~
                            key  4, keypos =  590, keylen =  42          ~

            select #8,  "HNYPILOC",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos =    1, keylen =  44

            select #9,  "HNYPICST",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 256,                                   ~
                        keypos =    1, keylen =  46

            select #10, "HNYCCMST",                                      ~
                        varc,     indexed,  recsize =  796,              ~
                        keypos =    1, keylen =  44,                     ~
                        alt key  1, keypos =   45, keylen =   6, dup,    ~
                            key  2, keypos =   81, keylen =   7, dup,    ~
                            key  3, keypos =   73, keylen =  15, dup

            select #11, "HNYCCDTL",                                      ~
                        varc,     indexed,  recsize =  436,              ~
                        keypos =    1,  keylen = 57,                     ~
                        alt key  1, keypos =   13, keylen =  45, dup,    ~
                        key  2, keypos =   14, keylen =  44, dup

            select #12, "HNYCCSYS",                                      ~
                        varc,     indexed,  recsize =  249,              ~
                        keypos =   1,  keylen =  12,                     ~
                        alt key  1, keypos =   43, keylen =  13, dup,    ~
                            key  2, keypos =   56, keylen =  10

            select #50, "WORKFILE",                                      ~
                         varc,                                           ~
                         consec,                                         ~
                         recsize = 512

            select #55, "DUMMY",                                         ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  42

            call "SHOSTAT" ("Opening Files, One Moment Please...")

            rslt$(1) = "REQUIRED"
            rslt$(5) = "REQUIRED"
            rslt$(6) = "REQUIRED"

            call "OPENFILE" (#1,  "SHARE", f2%(1%), rslt$(1%), axd$(1%))
            call "OPENFILE" (#2,  "SHARE", f2%(2%), rslt$(2%), axd$(2%))
            call "OPENCHCK" (#3,  0%, f2%(3%), 100%, rslt$(3%))
            call "OPENFILE" (#4,  "SHARE", f2%(4%), rslt$(4%), axd$(4%))
            call "OPENFILE" (#5,  "SHARE", f2%(5%), rslt$(5%), axd$(5%))
            call "OPENFILE" (#6,  "SHARE", f2%(6%), rslt$(6%), axd$(6%))
            call "OPENFILE" (#7,  "SHARE", f2%(7%), rslt$(7%), axd$(7%))
            call "OPENFILE" (#10, "SHARE", f2%(10%),rslt$(10%), axd$(10%))
            call "OPENFILE" (#11, "SHARE", f2%(11%),rslt$(11%), axd$(11%))
            call "OPENFILE" (#12, "SHARE", f2%(12%),rslt$(12%), axd$(12%))

           if f2%(1%) + f2%(2%) + f2%(5%) + f2%(6%) > 0% then exit_program
            records% = val(str(rslt$(2%),17%,4%),4)
            records% = records% / 4%
            records% = max(100%, records%)
            call "OPENCHCK" (#9,  0%, f2%(9%), records%, rslt$(9%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            mat identity = con
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press RETURN."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, abc$, hnyvar$, cat$(),     ~
                      check_digit$, part_req$, glvar$, part$(),filler$(),~
                      print$,   neg_only$, prefix$, seq$, session_date$, ~
                      session_nbr$, start_ticket$, whse$(), description$,~
                      lot_or_loc$, extra$, blank_only$, count_date$,     ~
                      session_cc$, sessiondescr$
            call "ALLFREE"
L10115:     option$, sourceflag$ = "P" /*  Physical Inventory */
L10120:     pfs1$(2) =                                                   ~
        "                                                               (~
        ~16)Exit Program"
            pfs1$(1) =                                                   ~
        "(1)Start Over                          (7)See PI Session       (~
        ~15)Print Screen"
            pfs2$(1) =                                                   ~
        "(1)Start Over       (4)Previous Field                          (~
        ~15)Print Screen"
            pfs2$(2) = " "
            pfs3$(1) =                                                   ~
        "(1)Start Over       (4)Previous Field                          (~
        ~15)Print Screen"
            pfs3$(2) = " "
            pf13$ = "    (13)Instructions"

            pfkeys$ = hex(0001ffff0405ff07ffffffffff0dff0f1020)

L10310:     REM *** Screen 1 ***
            for fieldnr% = 1% to 6%
                if fieldnr% = 1% then L10324
                   str(pfs1$(1),21,36) = "(4)Previous Field"
                   str(pfkeys$,8,1) = hex(ff) /* Disabled PF7 */
L10324:         if fieldnr% <> 2% then L10328
L10325:            str(pfs1$(2),40,20) = "(8)Select CC Session"
                   str(pfkeys$,9,1) = hex(08) /* Enabled PF8 */
                   goto L10330
L10328:            str(pfs1$(2),40,20) = " "
                   str(pfkeys$,9,1) = hex(ff) /* Disabled PF8 */
L10330:         gosub'051(fieldnr%)
                      if enabled% = 0 then        L10460
L10350:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10414
L10380:                  fieldnr% = max(1%, fieldnr%-1%)
                         if fieldnr% = 1% then L10115
                         if fieldnr% = 2% then L10325
                         gosub'051(fieldnr%)
                         if enabled% = 0 then     L10380
                         goto L10350
L10414:               if keyhit%  =  7 then gosub see_pi_session
                      if keyhit%  =  8 then gosub select_cc_session
                      if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10350
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then    L10350
L10460:     next fieldnr%

        REM Skip Screen 2 if doing Cycle Counting
           if option$ = "C" then L10660  /* Go to Screen 3 */

L10480:    REM *** Screen 2 ***
            for fieldnr% = 1% to 6%
                gosub'052(fieldnr%)
                      if enabled% = 0 then L10640
L10520:         gosub'102(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  4 and fieldnr% = 1% then L10310
                      if keyhit% <>  4 then       L10610
L10560:                  fieldnr% = max(1%, fieldnr%-1%)
                         gosub'052(fieldnr%)
                         if enabled% = 0 then     L10560
                         goto L10520
L10610:               if keyhit% <>  0 then       L10520
                gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10520
L10640:     next fieldnr%

L10660:     REM *** Screen 3 ***
            for fieldnr% = 1% to 5%
                gosub'053(fieldnr%)
                      if enabled% = 0 then L10770
L10700:         gosub'103(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  4 and fieldnr% = 1% and             ~
                         option$ = "C" then L10120
                      if keyhit%  =  4 and fieldnr% = 1% then L10480
                      if keyhit% <>  4 then       L10740
L10722:                  fieldnr% = max(1%, fieldnr%-1%)
                         gosub'053(fieldnr%)
                         if enabled% = 0 then     L10722
                         goto L10700
L10740:               if keyhit% <>  0 then       L10700
                gosub'153(fieldnr%)
                      if errormsg$ <> " " then L10700
L10770:     next fieldnr%

            REM *** CALL TO HNYPIDEF ***
L10800:     call "DATUNFMT" (session_date$)
                call "HNYPIDEF" (                                        ~
                        #3,              /* HNYPISYS File              */~
                        session_nbr$,    /* Count Session Number       */~
                        session_date$,   /* Planned count date         */~
                        description$,    /* Session notes/name         */~
                        part_req$,       /* Part numbers required flag */~
                        print$,          /* Print sheets or tickets ?  */~
                        hnyvar$,         /* Update QTY-ON-HAND Var Flag*/~
                        glvar$,          /* Update G/L Variance Flag   */~
                        keyhit%)         /* PF Key passed back;        */~
                                         /*    0 = save data           */~
                                         /*    1 = start over          */~
                                         /*    4 = prior screen        */~
                                         /*   16 = exit, no save       */
            call "DATEFMT" (session_date$)

                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  4 then       L10660
                      if keyhit%  = 32 then       exit_program
                      if keyhit% <>  0 then       L10800

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of edit mode for data entry screens     *~
            *************************************************************

        edtpg1
            pfs1$(1) =                                                   ~
        "(1)Start Over           (5)Next Screen                     (15)P~
        ~rint Screen"
            pfs1$(2) =                                                   ~
        "(32)QUIT & EXIT                                            (16)G~
        ~ENERATE TICKETS"
            inpmessage$ = "Press PF-16 to Generate Session  -OR-  Press P~
        ~F-32 to Exit & Return"

            pf13$ = "(13)Instructions"

L11130:     gosub'101(0%)
                  if keyhit%  =  1 then gosub startover
*                IF KEYHIT%  =  5 THEN       EDTPG2
                  if keyhit%  = 32 then       exit_program
*                IF KEYHIT% <>  0 AND KEYHIT% <> 16 THEN 11130
            gosub'151(0%)
                  if errormsg$ <> " " then L11130
*                IF KEYHIT% = 0% THEN 11130
                  if keyhit% =   5 then       edtpg2
                  if keyhit% =  16 then       datasave
            goto L11130
*          GOTO DATASAVE

        edtpg2
            if option$ = "C" then edtpg3
            pfs2$(1) =                                                   ~
        "(RETURN)Edit Selections       (4)Previous Screen               (~
        ~15)Print Screen"
            pfs2$(2) =                                                   ~
        "(1)Start Over                 (5)Next Screen                   (~
        ~32)QUIT"
            gosub'052(0%)
            if enabled% = 0% then edtpg3
L11290:     gosub'102(0%)
                  if keyhit%  =  1 then gosub startover
*                IF KEYHIT%  =  4 THEN       EDTPG1
*                IF KEYHIT%  =  5 THEN       EDTPG3
                  if keyhit%  = 32 then       exit_program
*                IF KEYHIT% <>  0 THEN       11290
            gosub'152(0%)
            if errormsg$ <> " " then L11290
                  if keyhit%  =  4 then       edtpg1
                  if keyhit%  =  5 then       edtpg3
            goto L11290

        edtpg3
            pfs3$(1) =                                                   ~
        "(RETURN)Edit Selections       (4)Previous Screen               (~
        ~15)Print Screen"
            pfs3$(2) =                                                   ~
        "(1)Start Over                 (5)Next Screen                   (~
        ~32)QUIT"
            gosub'053(0%)
L11460:     gosub'103(0%)
                  if keyhit%  =  1 then gosub startover
*                IF KEYHIT%  =  4 AND BLANK_ONLY$ = "YES" THEN EDTPG1
*                IF KEYHIT%  =  4 THEN       EDTPG2
*                IF KEYHIT%  =  5 THEN       EDTPG4
                  if keyhit%  = 32 then       exit_program
*                IF KEYHIT% <>  0 THEN       11460
            gosub'153(0%)
            if errormsg$ <> " " then L11460
                  if keyhit%  =  4 and blank_only$ = "YES" then edtpg1
                  if keyhit%  =  4 and option$ = "C" then edtpg1
                  if keyhit%  =  4 then       edtpg2
                  if keyhit%  =  5 then       edtpg4
            goto L11460

        edtpg4
            call "DATUNFMT" (session_date$)
                call "HNYPIDEF" (                                        ~
                        #3,              /* HNYPISYS File              */~
                        session_nbr$,    /* Count Session Number       */~
                        session_date$,   /* Planned count date         */~
                        description$,    /* Session notes/name         */~
                        part_req$,       /* Part numbers required flag */~
                        print$,          /* Print sheets or tickets ?  */~
                        hnyvar$,         /* Update QTY-ON-HAND Var Flag*/~
                        glvar$,          /* Update G/L Variance Flag   */~
                        keyhit%)         /* PF Key passed back;        */
            call "DATEFMT" (session_date$)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  4 then       edtpg3
                  if keyhit%  =  5 then       edtpg1
                  if keyhit%  = 32 then       exit_program

                  goto edtpg4

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after input/editing.                   *~
            *************************************************************

        datasave

            counter% = 0%
            records% = records% + extra
            call "WORKOPN2" (#50, "OUTPT", records% / 4%, f2%(50%))
            gosub generate_session_record

         /* Check with User for doing Snapshot Now */
            askmsg$(1%) = "Press PF 1 to Include 'SnapShots"
            askmsg$(2%) = "Press PF 2 to Exclude 'SnapShots"
            if lot_or_loc$ = "L" then L19180
                askmsg$(1%) = str(askmsg$(1%),,31%)
                askmsg$(2%) = str(askmsg$(2%),,31%)
L19180:     askmsg$(1%) = askmsg$(1%) & "' of Quantities & Costs"
            askmsg$(2%) = askmsg$(2%) & "' of Quantities & Costs"
L19200:     k% = 2%
            call "ASKUSER" (k%, "***** PROCESSING OPTIONS *****",        ~
                  askmsg$(1%), askmsg$(2%),                              ~
                  "-or- Press PF 16 to Return to Edit Mode")
            if k% = 1% or k% = 2% or k% = 16% then L19250 else L19200
L19250:     if k% = 16% then edtpg1
            if k% = 2% then L19380                      /* No Snapshots */

          /* Snapshots to be done */
            pic_flag$ = "Y"
            if extra$ = "0" or option$ = "C" then L19410 /* Partial Snaps*/
            call "SHOSTAT" ("Taking Initial 'SnapShot' of " &            ~
                               "Quantities & Costs")
            if lot_or_loc$ <> "L" then L19360
                call "OPENCHCK" (#8, 0%, f2%(8%), records% /4%, rslt$(8%))
                gosub generate_full_location_snapshot
L19360:     goto L19410

L19380:     pic_flag$ = "N"                 /* No Snapshots to be done */

          /* Lets Select the parts to use */
L19410:     call "SHOSTAT" ("Now Selecting Items to Count")
            if lot_or_loc$ <> "L" or f2%(8%) = 0%  then L19420
                call "OPENCHCK" (#8, 0%, f2%(8%), records% /4%, rslt$(8%))
L19420:     gosub selection_process
             if option$ <> "C" then L19520
              if counter% = 0% then L19520   /* Update CC Session Flag */

               readkey$ = str(session_cc$)
               call "READ101" (#12, readkey$, f1%(12)) /* HNYCCSYS */
                put #12 using L19490, "A", session_nbr$
L19490:         FMT POS(43), CH(1), POS(56), CH(2)
                rewrite #12

L19520:     close #50
            gosub construct_ticket_format
            if l% <= 6% then L19600
                call "ASKUSER" (k%, "INVALID TICKET NUMBERS",            ~
                  "Sorry, the number of tickets to be generated causes", ~
                  "the maximum allowable ticket number to be exceeded.", ~
                  "Press RETURN to change the parameters or exit.")
            goto L19660
L19600:     if counter% > 0% then L19710
              if extra > 0 then L19730
               call "ASKUSER" (k%, "INVALID SELECTION PARAMETERS",       ~
                 "Sorry, no Part Quantity records eligible for counting",~
                 "were found using the given selection parameters.",     ~
                 "Press RETURN to change the parameters or exit.")
L19660:        call "SHOSTAT" ("Now Deleting Partial Session Information")
               call "DELETE" (#3, session_nbr$, 2%)  /* PI System File */
               call "DELETE" (#8, session_nbr$, 2%)  /*Location Snapshot*/
               call "DELETE" (#9, session_nbr$, 2%)  /*Cost Snapshot*/
               goto edtpg3
L19710:     call "SHOSTAT" ("Now Sorting Items Selected")
            gosub sort_file
L19730:     call "SHOSTAT" ("Now Generating Ticket Records")
            gosub generate_tickets_file
            if extra < 1 then L19780
               call "SHOSTAT" ("Now Generating Extra Ticket Records")
               gosub generate_extra_tickets
L19780:     gosub update_session_record
            goto exit_program

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields for the page 1 of input. *~
            *************************************************************

            deffn'051(fieldnr%)
                enabled% = 1%
                inpmessage$ = " "
                on fieldnr% gosub L20200,           /* Session Number   */~
                                  L20231,           /* CC Session Nbr   */~
                                  L20240,           /* Session Date     */~
                                  L20280,           /* Session Notes    */~
                                  L20320,           /* Blank Tickets ?  */~
                                  L20410            /* Gen Tickets By.. */
                return
L20200:     REM Session Number Field
                inpmessage$ = "Enter the Session Number (01 - 99) to crea~
        ~te or delete"
                return
L20231:     REM Cycle Count Session Number
                inpmessage$ = "Enter Cycle Count Session to load parts," ~
                               & " or leave Blank."
                return
L20240:     REM Session Date
                if option$ <> "C" then L20255
                   enabled% = 0%
                   call "READ100" (#12, session_cc$, f1%(12))
                   if f1%(12) = 1% then L20249 : enabled% = 1% : goto L20255
L20249:              get #12 using L20250, session_date$
L20250:                      FMT POS(66), CH(6)
                     call "DATEFMT" (session_date$)
L20255:         inpmessage$ = "Enter the 'Planned' Count Date for this Se~
        ~ssion                         "
                return
L20280:     REM Session Name / Notes
                inpmessage$ = "Enter something to help you identify this ~
        ~session (if using multiple sessions)"
                description$ =  sessiondescr$
                return
L20320:     REM Blank Only ?
                if option$ = "C" then enabled% = 0
                inpmessage$ = "Enter YES to only generate Blank/Extra "  ~
                            & "Tickets or NO"
                if blank_only$ = " " then blank_only$ = "NO"
                return

L20410:     REM Tickets by Lot or Location
                if lot_or_loc$ = " " then lot_or_loc$ = "L"
                inpmessage$ = "Enter L to Allow/Create 1 Ticket Per "    ~
                            & "Location or P for 1 Per Store (& Lot)"
                if blank_only$ = "YES" then enabled% = 0%
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields for the page 2 of input. *~
            *************************************************************

            deffn'052(fieldnr%)
                enabled% = 1%
                if blank_only$ = "YES" then enabled% = 0%
                inpmessage$ = "Enter the Selection Criteria governing wha~
        ~t you plan to count"
                on fieldnr% gosub        L21180,    /* ABC Classes      */~
                                         L21250,    /* Warehouses       */~
                                         L21320,    /* Categories       */~
                                         L21370,    /* Parts            */~
                                         L21410,    /* Neg Only ?       */~
                                         L21510     /* Exclude Count dat*/
                return

L21180:     REM ABC Classes
                if abc$ = " " then abc$ = " ABCD"
                inpmessage$ = "Enter the ABCD Classes to include in this ~
        ~Session"
                return
L21250:     REM Warehouses
                if str(whse$(),1) = " " then whse$(1,1) = "ALL"
                inpmessage$ = "Select the Warehouses (Stores) to include ~
        ~in this Session"
                return
L21320:     REM Categories
                if str(cat$(),1)  = " " then cat$(1,1)  = "ALL"
                inpmessage$ = "Select the Categories ('blank' to 'blank' ~
        ~allowed in 1st selection)"
                return
L21370:     REM Parts
                if str(part$(),1)  = " " then part$(1,1)  = "ALL"
                inpmessage$="Select the Parts to include in this Session"
                return
L21410:     REM Negative Quantities ?
                if neg_only$ = " " then neg_only$ = "A"
                inpmessage$="Enter (A) for All Items, (N) for Non-zero It~
        ~ems, or (X) for Neg Qty Items."
                return
L21510:     REM Exclude Parts Counted On or After ...
                inpmessage$="Any part counted on/after this date will be"~
                          & " ignored REGARDLESS of Store Number!"
                if count_date$ = " " or count_date$ = blankdate$ ~
                                   then count_date$ = session_date$
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields for the page 3 of input. *~
            *************************************************************

            deffn'053(fieldnr%)
                enabled% = 1%
                inpmessage$ = "Please enter the requested information bel~
        ~ow"
                  on fieldnr% gosub L22100,         /* Prefix           */~
                                    L22140,         /* Starting Ticket  */~
                                    L22190,         /* Check Digit ?    */~
                                    L22231,         /* Extra Tickets    */~
                                    L22305          /* Ticket Sequence  */

                return

L22100:     REM Prefix
                inpmessage$ = "You may optionally enter a prefix to be at~
        ~ the start of each ticket #"
                return
L22140:     REM Starting Ticket
                inpmessage$ = "Enter the starting ticket number to use. E~
        ~nter leading zeroes to control length"
                if start_ticket$ = " " then start_ticket$ = "1"
                return
L22190:     REM Check Digit
                inpmessage$ = "Enter Y to append a check digit to the end~
        ~ of each ticket number or N"
                if check_digit$ = " " then check_digit$ = "N"
                return

L22231:     REM Extra Tickets
                if option$ <> "C" then L22270
                if lot_or_loc$ = "P" then L22255
                inpmessage$ = "Enter the desired number of extra tickets ~
        ~(Location Only) to generate"
                return

L22255:         call "NUMTEST" (extra$,0,99999,error$,0,extra)
                enabled% = 0% : return

L22270:         inpmessage$ = "Enter the desired number of extra or suppl~
        ~emental tickets to generate"
                return

L22305:     REM Ticket Sequence
                inpmessage$ = "Decide what sequence your tickets will be ~
        ~in (see above)"
                if seq$ > " " then return
                if lot_or_loc$ = "P" then seq$ = "2" else seq$ = "1"
                if blank_only$ = "YES" then enabled% = 0%
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the user the ability to start over when he wants to *~
            * or will return user back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            k% = 2%
            call "STARTOVR" (k%)
            if k% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *       D A T A   P R O C E S S I N G   S E C T I O N       *~
            *-----------------------------------------------------------*~
            *   Selects, Sorts, and Writes to Ticket file(s).           *~
            *************************************************************

        REM *************************************************************~
            *             S E L E C T I O N   P R O C E S S             *~
            *-----------------------------------------------------------*~
            *  Plows HNYQUAN and HNYLOCNS (optional) files to construct *~
            *  ticket records.  Each record is examined to see if it    *~
            *  meets the user defined selection criteria.               *~
            *************************************************************
        selection_process
            if option$ = "C" then do_cycle_count_parts
            gosub set_first_last_parts
            call "READ104" (#2, firstpart$, f1%(2%))
            goto L31180

        hnyquan_loop
            call "READNEXT" (#2, f1%(2)) /* HNYQUAN */
L31180:     if f1%(2) = 0% then return   /* Back to DATASAVE Area */
            plowkey$ = key(#2, 0%)
            if str(plowkey$,,25) > endpart$ then return
                                               /* Back to DATASAVE Area*/
            get #2 using L35030, part$, store$, lot$, loc$, qoh,          ~
                   tot_cost, cost()
            call "PACKZERO" (cost(), cost$)

            if pic_flag$ = "N" or extra$ = "0" or option$ = "C" then L31380
                 gosub generate_cost_record /*Gen Cst Rec for all Prts*/

L31380:     if blank_only$ = "YES" then hnyquan_loop
            if neg_only$ = "X" and qoh >= 0 then hnyquan_loop
            if neg_only$ = "N" and qoh  = 0 then hnyquan_loop
            reject$ = "Y"
            gosub check_part
            if reject$ = "Y" then hnyquan_loop
            reject$ = "Y"
            gosub check_warehouse
            if reject$ = "Y" then hnyquan_loop
            reject$ = "Y"
            if part$ = lastpart$ then L31510
               call "READ100" (#1, part$, f1%(1))
               if f1%(1) = 0% then hnyquan_loop
               get #1 using L31610, cat$, abc_code$, countdate$
L31510:     gosub check_category
            if reject$ = "Y" then hnyquan_loop
            reject$ = "Y"
            gosub check_abc_class
            if reject$ = "Y" then hnyquan_loop
            gosub check_count_date
            if reject$ = "Y" then hnyquan_loop
            gosub check_for_dupe
            if reject$ = "Y" then hnyquan_loop
            gosub hnylocns_loop
            if pic_flag$ = "Y" and  (extra$ = "0" or option$ = "C")      ~
                                        then gosub generate_cost_record
            goto hnyquan_loop

L31610:     FMT POS(90), CH(4), POS(111), CH(1), CH(6)

        hnylocns_loop
            if lot_or_loc$ = "P" then generate_sort_record/*ends L loop*/
            readkey$ = str(store$) & str(part$) & str(lot$,,6%) & hex(00)
            call "PLOWALTS" (#7, readkey$, 1%, 34%, f1%(7))
            if f1%(7) = 0% then generate_sort_record /* ends locns loop */
L31675:     lot$ = str(readkey$,29%,6%)
            loc$ = str(readkey$,35%,8%)
            if pic_flag$ = "Y" and  (extra$ = "0" or option$ = "C")      ~
                 then  gosub generate_location_record
            gosub generate_sort_record
            call "PLOWALTS" (#7, readkey$, 1%, 34%, f1%(7%))
            if f1%(7) = 0% then return
            goto L31675

        generate_sort_record
            put #50 using L35570, /* WORKFIL2 (ie HNYPITKT Sort File)   */~
            session_nbr$,   /* Number corresponding to a Inventory     */~
                            /* Count Session                           */~
            " ",            /* Ticket Number to a Physical Inventory   */~
                            /* Count Item                              */~
            99%,            /* A number decremented for each recount   */~
                            /* (99=original)                           */~
            part$,          /* Part Number                             */~
            store$,         /* Store or Warehouse Code                 */~
                            /* Warehouse code                          */~
            lot$,           /* Lot Number                              */~
            loc$,           /* bin location                            */~
                            /* Actual Bin Location (from HNYQUAN file  */~
                            /* or HNYLOCNSfile)                        */~
            " ",            /* Extra (supplemental) Ticket Flag: "X" = */~
                            /* extra, blank = not an extra             */~
            " ",            /* Flag or switch which controls posting   */~
                            /* to G/L                                  */~
            " ",            /* Flag or switch which controls posting   */~
                            /* to Inventory                            */~
            "N",            /* Override flag / indicator               */~
                            /* = "Y" if cost(s) are manually           */~
                            /* overridden                              */~
            " ",            /* Name of person who counted something    */~
            " ",            /* User Id of Data Entry Operator - 1 for  */~
                            /* each count                              */~
            " ",            /* Date something was counted              */~
            " ",            /* Date a transaction was entered          */~
            " ",            /* The System Time when a transaction was  */~
                            /* entered                                 */~
            0,              /* Actual Quantity On Hand.                */~
            0,0,0,0,0,      /* Quantity Counted for the current UOM    */~
                            /* (CASE)                                  */~
                            /* Case quantities table (upto 5)          */~
            0,0,0,0,0,      /* # of Stocking (base) Units which go     */~
                            /* into this Unit                          */~
            0,              /* Quantity From HNYLOCNS file (if         */~
                            /* applicable)                             */~
            0,0,0,0,0,0,    /* 12 cost                                 */~
            0,0,0,0,0,0,    /*         buckets                         */~
            0,              /* Total Standard Cost                     */~
            session_nbr$,   /* Number corresponding to a Inventory     */~
            "N",            /* Ticket/Count Sheets Print Flag          */~
            " "             /* Filler (Internal, unused space)         */

            write #50
            counter% = counter% + 1%
            return

        check_part
            if part$(1,1) <> "ALL" then L32260
               reject$ = "N"
               return
L32260:     for x% = 1% to 3%
                if part$ >= part$(x%,1%) and part$ <= part$(x%,2%) then  ~
                   reject$ = "N"
            next x%
            return

        check_warehouse
            if whse$(1,1) <> "ALL" then L32360
               reject$ = "N"
               return
L32360:     if store$ = " " then return
            for x% = 1% to 9%
                if store$ >= whse$(x%,1%) and store$ <= whse$(x%,2%) then~
                   reject$ = "N"
            next x%
            return

        check_category
            if cat$(1,1) <> "ALL" then L32462
               reject$ = "N"
               return
L32462:     if cat$ < cat$(1%,1%) or cat$ > cat$(1%,2%) then L32470
                   reject$ = "N"
                   return         /* Meets Range criteria */
L32470:     for x% = 2% to 9%
                if cat$(x%,1%) = " " and cat$(x%,2%) = " "               ~
                   then L32500   /* Don't allow " "'s after 1st element */
                if cat$ >= cat$(x%,1%) and cat$ <= cat$(x%,2%) then      ~
                   reject$ = "N"
L32500:     next x%
            return

        check_abc_class
            if abc$ = "ALL" then reject$ = "N"
            if pos(str(abc$,,len(abc$)) = abc_code$) > 0% then           ~
                                                            reject$ = "N"
            return

        check_count_date
            call "DATUNFMT" (count_date$)
            if countdate$ >= count_date$ then reject$ = "Y"
            call "DATEFMT" (count_date$)
            return

        check_for_dupe
            filekey$ = str(part$) & str(store$) & hex(00)
            call "PLOWALTS" (#4, filekey$, 1%, 28%, f1%(4))
            if f1%(4) = 1% then reject$ = "Y"
            return

        sort_file
            call "GETNAMES" addr(#50, file$, lib$, vol$)
            sort$ = file$
            str(sort$,9%,8%) = lib$
            str(sort$,17%,6%) = vol$
            str(sort$,23%,22%) = str(sort$,,22%)
            convert seq$ to seq%
            on seq% gosub by_store_loc_part,                             ~
                          by_store_part_lot,                             ~
                          by_store_part_loc,                             ~
                          by_part_store_loc,                             ~
                          by_part_loc_lot,                               ~
                          by_part_lot_loc
            call "SORTCALL" addr(sort$, ret%)
            if ret% = 0% then L32840
               call "ASKUSER" (k%, "*** SORT FAILURE ***",               ~
                "Either unable to link to SORT (not found or protected)",~
                "or SORT failed for some other reason (not enuff space)."~
                ,"Press RETURN to acknowlege & exit (still restartable).")
               goto exit_program
L32840:     call "WORKOPN2" (#50, "INPUT", 0%, f2%(50%))
            if f2%(4) = 0% then return
               records% = max(counter%, records%)
               call "OPENCHCK" (#4, 0%, f2%(4), records% / 2%, rslt$(4))
               close #4
               call "OPENFILE" (#4, "IO   ", f2%(4), rslt$(4), axd$(4))
               return

        by_store_loc_part
            str(sort$,45%,9%) = "0041003CA"        /* Store            */
            str(sort$,54%,9%) = "0060008CA"        /* Location         */
            str(sort$,63%,9%) = "0016025CA"        /* Part             */
            str(sort$,72%,9%) = "0044016CA"        /* Lot              */
            return

        by_store_part_lot
            str(sort$,45%,9%) = "0041003CA"        /* Store            */
            str(sort$,54%,9%) = "0016025CA"        /* Part             */
            str(sort$,63%,9%) = "0044016CA"        /* Lot              */
            str(sort$,72%,9%) = "0060008CA"        /* Location         */
            return

        by_store_part_loc
            str(sort$,45%,9%) = "0041003CA"        /* Store            */
            str(sort$,54%,9%) = "0016025CA"        /* Part             */
            str(sort$,63%,9%) = "0060008CA"        /* Location         */
            str(sort$,72%,9%) = "0044016CA"        /* Lot              */
            return

        by_part_store_loc
            str(sort$,45%,9%) = "0016025CA"        /* Part             */
            str(sort$,54%,9%) = "0041003CA"        /* Store            */
            str(sort$,63%,9%) = "0060008CA"        /* Location         */
            str(sort$,72%,9%) = "0044016CA"        /* Lot              */
            return

        by_part_loc_lot
            str(sort$,45%,9%) = "0016025CA"        /* Part             */
            str(sort$,54%,9%) = "0060008CA"        /* Location         */
            str(sort$,63%,9%) = "0044016CA"        /* Lot              */
            str(sort$,72%,9%) = "0041003CA"        /* Store            */
            return

        by_part_lot_loc
            str(sort$,45%,9%) = "0016025CA"        /* Part             */
            str(sort$,54%,9%) = "0044016CA"        /* Lot              */
            str(sort$,63%,9%) = "0060008CA"        /* Location         */
            str(sort$,72%,9%) = "0041003CA"        /* Store            */
            return

        generate_session_record
            call "DATUNFMT" (session_date$)

            put #3 using L35345,  /* File - HNYPISYS */                   ~
            session_date$,  /* Date count session planned              */~
            session_nbr$,   /* Number corresponding to a Inventory Coun*/~
            description$,   /* Generic for general code descriptions   */~
            " ",            /* Filler                                  */~
            whse$(),        /* Warehouse or Stores  (Ranges)           */~
            cat$(),         /* Category codes (Ranges)                 */~
            part$(),        /* Part codes (ranges)                     */~
            neg_only$,      /* Count Negative items only ?             */~
            lot_or_loc$,    /* 1 Ticket per Lot or 1 per Location flag */~
            prefix$,        /* 1 to 3 character prefix to the P.I. Tick*/~
            start_ticket$,  /* Ticket Number to a Physical Inventory Co*/~
            lastticket$,    /* Last Ticket Number used                 */~
            check_digit$,   /* Calculate & append check digit ?        */~
            extra$,         /* Number of extra Physical Inventory Ticke*/~
            seq$,           /* Flag controlling what sequence tickets w*/~
            part_req$,      /* Part Numbers required in Count Entry ?  */~
            print$,         /* Print tickets or Count sheets  Indicator*/~
            hnyvar$,        /* Update inventory variances     Indicator*/~
            glvar$,         /* Update G/L       variances     Indicator*/~
            date,           /* Date a session generated                */~
            date,           /* Date costs/quantities captured          */~
            time,           /* Time costs/quantities captured          */~
            " ",            /* All Parts Accounted for Flag            */~
            abc$,           /* ABC category                            */~
            sourceflag$,    /* C - Cycle Count  P - Physical Inventory */~
            filler$(1)      /* Filler For Rest of Record               */~

            write #3
            call "DATEFMT" (session_date$)
            return

        construct_ticket_format
            ticket$ = prefix$
            convert start_ticket$ to ticket%
            p% = pos(ticket$ = " ")
            if  counter% + extra + ticket% - 1% = 0 then  l% = 1%   else ~
                l% = log(counter% + extra + ticket% - 1%) / log(10) + 1%
            if len(start_ticket$) > l% then l% = len(start_ticket$)
            return

        generate_tickets_file
            call "READNEXT" (#50, f1%(50%))
            if f1%(50%) = 0% then return
            get #50, using L33842, record$()
            put #4, using L33842, record$()
            if option$ = "C" then  gosub  reset_cc_flags
            on l% gosub L33860, L33880, L33900, L33920, L33940, L33960
            if check_digit$ = "Y" then gosub compute_check_digit
            put #4, using L33812, ticket$, ticket$, 99%
L33812:         FMT POS(3), CH(12), POS(316), CH(12), BI(1)
            write #4
            ticket% = ticket% + 1%
            goto generate_tickets_file

L33842:     FMT 2*CH(256)

L33860:     convert ticket% to str(ticket$,p%), pic(0)
            return
L33880:     convert ticket% to str(ticket$,p%), pic(00)
            return
L33900:     convert ticket% to str(ticket$,p%), pic(000)
            return
L33920:     convert ticket% to str(ticket$,p%), pic(0000)
            return
L33940:     convert ticket% to str(ticket$,p%), pic(00000)
            return
L33960:     convert ticket% to str(ticket$,p%), pic(000000)
            return
*          CONVERT TICKET% TO STR(TICKET$,P%), PIC(0000000)
*          RETURN

        compute_check_digit
            sum% = 0%
            for i% = 1% to l%
                convert str(ticket$,p%-1%+i%,1%) to y%
                sum% = sum% + (l%+2%-i%) * y%
            next i%
            check_digit% = 11% - mod(sum%,11%)
            convert check_digit% to str(ticket$,len(ticket$)+1%,1%),pic(0)
            return

        generate_extra_tickets
            call "OPENCHCK" (#4, 0%, f2%(4), records% / 2%, rslt$(4))
            mat cost = zer
            for x% = 1% to extra
               on l% gosub L33860, L33880, L33900, L33920, L33940, L33960
               if check_digit$ = "Y" then gosub compute_check_digit
               put #4 using L35570,  /* FILE: HNYPITKT                  */~
               session_nbr$,/* Number corresponding to a Inventory     */~
                            /* Count Session                           */~
               ticket$,     /* Ticket Number to a Physical Inventory   */~
                            /* Count Item                              */~
               99%,         /* A number decremented for each recount   */~
                            /* (99=original)                           */~
               " ",         /* Part Number                             */~
               " ",         /* Store or Warehouse Code                 */~
                            /* Warehouse code                          */~
               " ",         /* Lot Number                              */~
               " ",         /* bin location                            */~
                            /* Actual Bin Location (from HNYQUAN file  */~
                            /* or HNYLOCNSfile)                        */~
               "X",         /* Extra (supplemental) Ticket Flag: "X" = */~
                            /* extra, blank = not an extra             */~
               " ",         /* Flag or switch which controls posting   */~
                            /* to G/L                                  */~
               " ",         /* Flag or switch which controls posting   */~
                            /* to Inventory                            */~
               "N",         /* Override flag / indicator               */~
                            /* = "Y" if cost(s) are manually           */~
                            /* overridden                              */~
               " ",         /* Name of person who counted something    */~
               " ",         /* User Id of Data Entry Operator - 1 for  */~
                            /* each count                              */~
               " ",         /* Date something was counted              */~
               " ",         /* Date a transaction was entered          */~
               " ",         /* The System Time when a transaction was  */~
                            /* entered                                 */~
               0,           /* Actual Quantity On Hand according to    */~
                            /* the count. If less than 0 then ticket   */~
                            /* is a VOIDED ticket.                     */~
               0,0,0,0,0,   /* Quantity Counted for the current UOM    */~
                            /* (CASE)                                  */~
                            /* Case quantities table (upto 5)          */~
               0,0,0,0,0,   /* # of Stocking (base) Units which go     */~
                            /* into this Unit                          */~
               0,           /* Quantity From HNYLOCNS file (if         */~
                            /* applicable)                             */~
               cost(),      /* 12 cost buckets                         */~
               0,           /* Total cost filed                        */~
               session_nbr$,/* Number corresponding to a Inventory     */~
               "N",         /* Ticket/Count Sheets for Print Flag      */~
               ticket$,     /* Ticket Number to a Physical Inventory   */~
               99%,         /* A number decremented for each recount   */~
                            /* (99% = Original)                        */~
               " "          /* Filler (Internal, unused space)         */

               write #4
               ticket% = ticket% + 1%
            next x%
            return

        update_session_record
            lastticket$ = str(ticket$,p%,l%)
            if option$ = "C" then sourceflag$ = "C"
            call "DATUNFMT" (session_date$)
            call "READ101" (#3, key(#3), f1%(3))
            put #3 using L35345,                                          ~
            session_date$,  /* Date count session planned              */~
            session_nbr$,   /* Number corresponding to a Inventory Coun*/~
            description$,   /* Generic for general code descriptions   */~
            " ",            /* Filler                                  */~
            whse$(),        /* Warehouse or Stores  (Ranges)           */~
            cat$(),         /* Category codes (Ranges)                 */~
            part$(),        /* Part codes (ranges)                     */~
            neg_only$,      /* Count Negative items only ?             */~
            lot_or_loc$,    /* 1 Ticket per Lot or 1 per Location flag */~
            prefix$,        /* 1 to 3 character prefix to the P.I. Tick*/~
            start_ticket$,  /* Ticket Number to a Physical Inventory Co*/~
            lastticket$,    /* Last Ticket Number used                 */~
            check_digit$,   /* Calculate & append check digit ?        */~
            extra$,         /* Number of extra Physical Inventory Ticke*/~
            seq$,           /* Flag controlling what sequence tickets w*/~
            part_req$,      /* Part Numbers required in Count Entry ?  */~
            print$,         /* Print tickets or Count sheets  Indicator*/~
            hnyvar$,        /* Update inventory variances     Indicator*/~
            glvar$,         /* Update G/L       variances     Indicator*/~
            date,           /* Date a session generated                */~
            date,           /* Date costs/quantities captured          */~
            time,           /* Time costs/quantities captured          */~
            " ",            /* All Parts Accounted for Flag            */~
            abc$,           /* ABC category                            */~
            sourceflag$,    /* C - Cycle Count      P - Physical Inv   */~
            filler$(1)      /* Filler For Rest of Record               */

            rewrite #3
            call "DATEFMT" (session_date$)
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * Format statements for data files.                         *~
            *************************************************************

L35030: FMT POS(17),        /* FILE: HNYQUAN                           */~
            CH(25),         /* Part Number                             */~
            CH(3),          /* store number                            */~
            CH(16),         /* lot number                              */~
            POS(61),                                                     ~
            CH(8),          /* bin location                            */~
            PD(14,4),       /* quantity on-hand                        */~
            POS(117),                                                    ~
            13*PD(14,4)     /* costs                                   */

L35345: FMT                 /* FILE: HNYPISYS                          */~
            CH(6),          /* Date something was counted              */~
            CH(2),          /* Number corresponding to a Inventory Coun*/~
            CH(30),         /* Generic for general code descriptions   */~
            CH(3),          /* Filler                                  */~
            18*CH(3),       /* Warehouse or Stores (ranges)            */~
            18*CH(4),       /* category code (ranges)                  */~
            6*CH(25),       /* Part Codes (ranges)                     */~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(3),          /* 1 to 3 character prefix to the P.I. Tick*/~
            CH(06),         /* Ticket Number to a Physical Inventory Co*/~
            CH(06),         /* Last Ticket Number used                 */~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(6),          /* Number of extra Physical Inventory Ticke*/~
            CH(1),          /* Flag controlling what sequence tickets w*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(1),          /* General Purpose Flag or Switch Indicator*/~
            CH(6),          /* Date a transaction was entered          */~
            CH(6),          /* System Date                             */~
            CH(6),          /* The System Time when a transaction was e*/~
            CH(1),          /* All Parts Accounted for Flag            */~
            CH(5),          /* ABC category                            */~
            CH(1),          /* C-Cycle Count, P-Physical Count         */~
            CH(141)         /* Filler For Rest of Record or Internal Sp*/~

L35570: FMT                 /* FILE: HNYPITKT                          */~
            CH(2),          /* Number corresponding to a Inventory     */~
                            /* Count Session                           */~
            CH(12),         /* Ticket Number to a Physical Inventory   */~
                            /* Count Item                              */~
            BI(1),          /* A number decremented for each recount   */~
                            /* (99=original                            */~
            CH(25),         /* Part Number                             */~
            CH(3),          /* Store or Warehouse Code                 */~
                            /* Warehouse code                          */~
            CH(16),         /* Lot Number                              */~
            CH(8),          /* bin location                            */~
                            /* Actual Bin Location (from HNYQUAN file  */~
                            /* or HNYLOCNSfile)                        */~
            CH(1),          /* General Purpose Flag or Switch          */~
                            /* Indicator                               */~
                            /* Extra (supplemental) Ticket Flag: "X" = */~
                            /* extra, blank = not an extra             */~
            CH(1),          /* Flag or switch which controls posting   */~
                            /* to G/L                                  */~
            CH(1),          /* Flag or switch which controls posting   */~
                            /* to Inventory                            */~
            CH(1),          /* Override flag / indicator               */~
                            /* = "Y" if cost(s) are manually           */~
                            /* overridden                              */~
            CH(20),         /* Name of person who counted something    */~
            CH(3),          /* user-id of specific user                */~
                            /* User Id of Data Entry Operator - 1 for  */~
                            /* each count                              */~
            CH(6),          /* Date something was counted              */~
            CH(6),          /* Date a transaction was entered          */~
                            /* Date entered into computer              */~
            CH(6),          /* The System Time when a transaction was  */~
                            /* entered                                 */~
            PD(14,4),       /* Quantity counted of something           */~
                            /* Actual Quantity On Hand according to    */~
                            /* the count. If less than 0 then ticket   */~
                            /* is a VOIDED ticket.                     */~
            5*PD(14,4),     /* Quantity Counted for the current UOM    */~
                            /* (CASE)                                  */~
                            /* Case quantities table (upto 5)          */~
            5*PD(14,4),     /* # of Stocking (base) Units which go     */~
                            /* into this Unit                          */~
            PD(14,4),       /* Quantity of Something                   */~
                            /* Quantity From HNYLOCNS file (if         */~
                            /* applicable)                             */~
            12*PD(14,4),    /* 12 cost buckets                         */~
            PD(14,4),       /* Total Standard Cost                     */~
            CH(2),          /* Session Number                          */~
            CH(1),          /* Ticket/Count Sheets Print Flag          */~
            CH(12),         /* Ticket Number to a Physical Inventory   */~
            BI(1),          /* A number decremented for each recount   */~
                            /* (99% = Origianl)                        */~
            CH(164)         /* Filler (Internal, unused space)         */

L36390: FMT                 /* FILE: HNYPICST                          */~
            CH(2),          /* Number corresponding to a Inventory     */~
                            /* Count Session                           */~
            CH(25),         /* Part Number                             */~
            CH(3),          /* Store or Warehouse Code                 */~
                            /* Warehouse code                          */~
            CH(16),         /* Lot Number                              */~
            PD(14,4),       /* quantity on-hand                        */~
                            /* Frozen qty-on-hand from HNYQUAN record. */~
                            /* This is the 'snapshot' qty-on-hand.     */~
            CH(96),         /* 12 Cost buckets                         */~
            PD(14,4),       /* Total Standard Cost                     */~
                            /*         These are the frozen or         */~
                            /* 'snapshot' costs                        */~
            CH(98)          /* Filler (Internal, unused space)         */~

        generate_cost_record
          /* Generate Cost Record for that Part */
            write #9 using L36390, /* HNYPICST File */                    ~
            session_nbr$,   /* Number corresponding to a Inventory     */~
                            /* Count Session                           */~
            part$,          /* Part Number                             */~
            store$,         /* Store or Warehouse Code                 */~
                            /* Warehouse code                          */~
            lot$,           /* Lot Number                              */~
            qoh,            /* quantity on-hand                        */~
                            /* Frozen qty-on-hand from HNYQUAN record. */~
                            /* This is the 'snapshot' qty-on-hand.     */~
            cost$,          /* 12 Cost Buckets                         */~
            tot_cost,       /* Total Standard Cost                     */~
                            /*         These are the frozen or         */~
                            /* 'snapshot' costs                        */~
            " "             /* Filler (Internal, unused space)         */
        return

        generate_location_record
*          READKEY$ = STR(STORE$) & STR(LOC$) & STR(PART$) &            ~
*                     STR(LOT$,,6%)
*          CALL "READ100" (#7, READKEY$, F1%(7%))
*              IF F1%(7%) = 0% THEN RETURN      /* No Location Data */
            get #7 using L38310, locquan
            lockey$ = str(session_nbr$) & str(part$) & str(store$) &     ~
                       str(lot$,,6%) & loc$
            call "READ101" (#8, lockey$, f1%(8%))
            put #8  using L38320, lockey$, locquan, " "
                if f1%(8%) = 1% then rewrite #8   else   write #8
            return

L38310: FMT POS(573), PD(14,4)             /* HNYLOCNS */
L38320: FMT CH(44), PD(14,4), CH(48)       /* HNYPILOC Snapshot File */

        generate_full_location_snapshot
            readkey$ = all(hex(00))
            call "READ104" (#7, readkey$, f1%(7%))
L38370:         if f1%(7%) = 0% then return
            get #7 using L38430, store$, loc$, part$, lot$
            gosub generate_location_record
            call "READNEXT" (#7, f1%(7%))
            goto L38370

L38430: FMT CH(3), CH(8), CH(25), CH(6)

        do_cycle_count_parts
            cc_key$ = str(session_cc$) & "P" & hex(00)
            testkey$ = str(cc_key$,,13%)
            call "READ104" (#11, cc_key$, f1%(11%))
            goto L39080

        loop_cc
            call "READNEXT" (#11, f1%(11%))         /* HNYCCDTL */
L39080:     if f1%(11%) = 0% then return   /* Back to DATASAVE Area */
            cc_key$ = key(#11, 0%)
            if str(cc_key$,,13%)  >  testkey$ then return
            part$  = str(cc_key$,14%,25%)
            store$ = str(cc_key$,39%, 3%)
            lot$   = str(cc_key$,42%, 6%)
            call "READ100" (#2, str(cc_key$,14%, 34%),f1%(2%))
            if f1%(2%) = 0% then loop_cc /* no Hnyquan so get another CC*/
            get #2 using L35030, part$, store$, lot$, loc$, qoh,          ~
                   tot_cost, cost()
            call "PACKZERO" (cost(), cost$)

            gosub hnylocns_loop    /* Loop Locations & Write Tickets */
            if pic_flag$ = "Y" then gosub generate_cost_record

            goto loop_cc           /* Get another CC Part */

        reset_cc_flags
            cc_key$ = str(record$(1%),16%,34%)
            call "READ101" (#10, cc_key$, f1%(10%))      /* HNYCCDTL */
            if f1%(10%) = 0% then L39250  /* Should not happen */
            put #10 using L39220, "A"
L39220:     FMT POS(81), CH(1)
            rewrite #10

L39250:     cc_key$ = str(session_cc$) & "P" & cc_key$
            call "READ100" (#11, cc_key$, f1%(11%))
            if f1%(11%) = 0% then return
            call "DELETE" (#11, cc_key$,57%)
            put #11 using L39300, "A"
L39300:     FMT POS(13), CH(1)
            write #11

            return

        set_first_last_parts
            firstpart$ = all(hex(00))
            endpart$   = all(hex(ff))
            if blank_only$ = "YES" or (extra$ > "0" and option$ = "P")   ~
                                                               then return
            if part$(1,1) = "ALL" then return
            firstpart$ = part$(1%,1%)  :  endpart$ =  part$(1%,2%)
            for x% = 1% to 3%
              if part$(x%,1%) = " "  then L39440
              if firstpart$ > part$(x%,1%) then firstpart$ = part$(x%,1%)
              if endpart$   < part$(x%,2%) then endpart$   = part$(x%,2%)
L39440:     next x%

            return

        REM *************************************************************~
            *                  S C R E E N   P A G E   1                *~
            *                                                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  if fieldnr% = 0% then init(hex(81)) lfac$()            ~
                                   else init(hex(8c)) lfac$()
                  if option$ = "P" then L40065
                     if fieldnr% <> 0% then L40070
                        init(hex(8c)) lfac$(3), lfac$(5)
L40065:           if option$ = "P" and fieldnr% = 0% then                ~
                        init(hex(8c)) lfac$(2)
L40070:           line2$ = "Generate Physical Inventory or Cycle Count " ~
                         & "Tickets"
                  str(line2$,62%) = "HNYPIGEN: " & str(cms2v$,,8%)

                  if session_date$ < " " then call "DATEFMT" (session_date$)

                  on fieldnr% gosub L40180,         /* PI Session Nbr   */~
                                    L40180,         /* CC Session Nbr   */~
                                    L40180,         /* Count Date       */~
                                    L40180,         /* Description      */~
                                    L40180,         /* Blank Only       */~
                                    L40180          /* Gen By ?         */

                  if errormsg$ > " " and fieldnr% > 0% then              ~
                     lfac$(fieldnr%) = or(hex(10))
                  goto L40220

L40180:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return

L40220: accept                                                           ~
               at (01,02),"Start Physical Inventory/Cycle Count Session",~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02),                                               ~
        "  Please enter the PI Session Number/CC Session Name, Planned Co~
        ~unt Date",                                                       ~
               at (06,02),                                               ~
        "and Session Description for this PI/CC Session.",               ~
               at (07,02),                                               ~
        "  If you enter an existing PI Session #, you will be warned but ~
        ~allowed",                                                        ~
               at (08,02),                                               ~
        "to continue.  If you do you will wipe out the previously planned~
        ~ session.",                                                      ~
               at (09,02),                                               ~
        "  The Session Number must be a unique number (from 01 - 99) whic~
        ~h the system",                                                   ~
               at (10,02),                                               ~
        "will use to identify all activity associated with this count ses~
        ~sion.",                                                          ~
               at (13,02), "Count Session Number       :"       ,        ~
               at (13,32), fac(lfac$( 1)), session_nbr$         , ch(02),~
               at (14,02), "Cycle Count Session        :"       ,        ~
               at (14,32), fac(lfac$( 2)), session_cc$          , ch(12),~
               at (14,46), fac(hex(8c)), sessiondescr$          , ch(30),~
               at (15,02), "Planned Count Date         :"       ,        ~
               at (15,32), fac(lfac$( 3)), session_date$        , ch(08),~
               at (16,02), "Session Description        :"       ,        ~
               at (16,32), fac(lfac$( 4)), description$         , ch(30),~
               at (17,02), "Create Blank Tickets Only? :"       ,        ~
               at (17,32), fac(lfac$( 5)), blank_only$          , ch(03),~
               at (18,02), "Generate 1 Ticket Per      :"       ,        ~
               at (18,32), fac(lfac$( 6)), lot_or_loc$          , ch(01),~
               at (18,37), "(L) Part/Store/Lot/Loc"             ,        ~
               at (19,37), "(P) Part/Store/Lot"                 ,        ~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,61), fac(hex(8c)), pf13$                  , ch(19),~
               at (23,02), fac(hex(8c)), pfs1$(1)               , ch(79),~
               at (24,02), fac(hex(8c)), pfs1$(2)               , ch(79),~
               keys(pfkeys$)                                    ,        ~
               key (keyhit%)

               if keyhit% <> 13 then L40670
                  call "MANUAL" ("HNYPIGEN")
                  goto L40220

L40670:        if keyhit% <> 15 then L40710
                  call "PRNTSCRN"
                  goto L40220

L40710:        if fieldnr% <> 0% then return
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                 S C R E E N   P A G E   2                 *~
            *                                                           *~
            *************************************************************

            deffn'102(fieldnr%)
                  line2$ = "Select Items To Be Counted For Count Date "  ~
                           & session_date$
                  str(line2$,62%) = "HNYPIGEN: " & str(cms2v$,,8%)

                  if fieldnr% = 0% then init(hex(81)) lfac$()            ~
                                   else init(hex(8c)) lfac$()

                  on fieldnr% gosub L41110,         /* ABCD Class       */~
                                    L41110,         /* Warehouses       */~
                                    L41110,         /* Categories       */~
                                    L41110,         /* Parts            */~
                                    L41110,         /* Count Negative?  */~
                                    L41110          /* Exclude Count dat*/

                  goto L41130

L41110:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return

L41130: accept                                                           ~
               at (01,02),                                               ~
        "Start Physical Inventory/Cycle Count Session",                  ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02),                                               ~
        "'ABCD' Class(s) to Count     :",                                ~
               at (05,33), fac(lfac$(1)),  abc$                 , ch(05),~
               at (05,41),                                               ~
        "(e.g. A, AB, ABCD...)"                                 ,        ~
               at (06,02),                                               ~
        "Warehouses to Count (Range) :",                                 ~
               at (06,33), fac(lfac$(02)),        whse$(1,1)    , ch(03),~
               at (06,38),                                               ~
        "to",                                                            ~
               at (06,41), fac(lfac$(02)),        whse$(1,2)    , ch(03),~
               at (06,48), fac(lfac$(02)),        whse$(2,1)    , ch(03),~
               at (06,53),                                               ~
        "to",                                                            ~
               at (06,56), fac(lfac$(02)),        whse$(2,2)    , ch(03),~
               at (06,63), fac(lfac$(02)),        whse$(3,1)    , ch(03),~
               at (06,68),                                               ~
        "to",                                                            ~
               at (06,71), fac(lfac$(02)),        whse$(3,2)    , ch(03),~
               at (07,33), fac(lfac$(02)),        whse$(4,1)    , ch(03),~
               at (07,38),                                               ~
        "to",                                                            ~
               at (07,41), fac(lfac$(02)),        whse$(4,2)    , ch(03),~
               at (07,48), fac(lfac$(02)),        whse$(5,1)    , ch(03),~
               at (07,53),                                               ~
        "to",                                                            ~
               at (07,56), fac(lfac$(02)),        whse$(5,2)    , ch(03),~
               at (07,63), fac(lfac$(02)),        whse$(6,1)    , ch(03),~
               at (07,68),                                               ~
        "to",                                                            ~
               at (07,71), fac(lfac$(02)),        whse$(6,2)    , ch(03),~
               at (08,33), fac(lfac$(02)),        whse$(7,1)    , ch(03),~
               at (08,38),                                               ~
        "to",                                                            ~
               at (08,41), fac(lfac$(02)),        whse$(7,2)    , ch(03),~
               at (08,48), fac(lfac$(02)),        whse$(8,1)    , ch(03),~
               at (08,53),                                               ~
        "to",                                                            ~
               at (08,56), fac(lfac$(02)),        whse$(8,2)    , ch(03),~
               at (08,63), fac(lfac$(02)),        whse$(9,1)    , ch(03),~
               at (08,68),                                               ~
        "to",                                                            ~
               at (08,71), fac(lfac$(02)),        whse$(9,2)    , ch(03),~
               at (09,02),                                               ~
        "Categories to Count (Range) :",                                 ~
               at (09,33), fac(lfac$(03)),        cat$(1,1)     , ch(04),~
               at (09,38),                                               ~
        "to",                                                            ~
               at (09,41), fac(lfac$(03)),        cat$(1,2)     , ch(04),~
               at (09,48), fac(lfac$(03)),        cat$(2,1)     , ch(04),~
               at (09,53),                                               ~
        "to",                                                            ~
               at (09,56), fac(lfac$(03)),        cat$(2,2)     , ch(04),~
               at (09,63), fac(lfac$(03)),        cat$(3,1)     , ch(04),~
               at (09,68),                                               ~
        "to",                                                            ~
               at (09,71), fac(lfac$(03)),        cat$(3,2)     , ch(04),~
               at (10,33), fac(lfac$(03)),        cat$(4,1)     , ch(04),~
               at (10,38),                                               ~
        "to",                                                            ~
               at (10,41), fac(lfac$(03)),        cat$(4,2)     , ch(04),~
               at (10,48), fac(lfac$(03)),        cat$(5,1)     , ch(04),~
               at (10,53),                                               ~
        "to",                                                            ~
               at (10,56), fac(lfac$(03)),        cat$(5,2)     , ch(04),~
               at (10,63), fac(lfac$(03)),        cat$(6,1)     , ch(04),~
               at (10,68),                                               ~
        "to",                                                            ~
               at (10,71), fac(lfac$(03)),        cat$(6,2)     , ch(04),~
               at (11,33), fac(lfac$(03)),        cat$(7,1)     , ch(04),~
               at (11,38),                                               ~
        "to",                                                            ~
               at (11,41), fac(lfac$(03)),        cat$(7,2)     , ch(04),~
               at (11,48), fac(lfac$(03)),        cat$(8,1)     , ch(04),~
               at (11,53),                                               ~
        "to",                                                            ~
               at (11,56), fac(lfac$(03)),        cat$(8,2)     , ch(04),~
               at (11,63), fac(lfac$(03)),        cat$(9,1)     , ch(04),~
               at (11,68),                                               ~
        "to",                                                            ~
               at (11,71), fac(lfac$(03)),        cat$(9,2)     , ch(04),~
               at (12,02),                                               ~
        "Parts to Count (Ranges) from:",                                 ~
               at (12,33), fac(lfac$(04)),        part$(1,1)    , ch(25),~
               at (13,28), "to:",                                        ~
               at (13,33), fac(lfac$(04)),        part$(1,2)    , ch(25),~
               at (14,26), "from:",                                      ~
               at (14,33), fac(lfac$(04)),        part$(2,1)    , ch(25),~
               at (15,28), "to:",                                        ~
               at (15,33), fac(lfac$(04)),        part$(2,2)    , ch(25),~
               at (16,26), "from:",                                      ~
               at (16,33), fac(lfac$(04)),        part$(3,1)    , ch(25),~
               at (17,28), "to:",                                        ~
               at (17,33), fac(lfac$(04)),        part$(3,2)    , ch(25),~
               at (18,02),                                               ~
        "(A) - Any Onhand, (N) - Non-zero Only, (X) - Neg Qty Only",     ~
               at (18,61), fac(lfac$(05)),        neg_only$     , ch(01),~
               at (19,02),                                               ~
        "Exclude Parts Counted On/After:",                               ~
               at (19,34), fac(lfac$(06)),        count_date$   , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), fac(hex(8c)), pfs2$(1)               , ch(79),~
               at (24,02), fac(hex(8c)), pfs2$(2)               , ch(79),~
               keys(hex(000104050d0f1020)),                              ~
               key (keyhit%)

               if keyhit% <> 13 then L41735
                  call "MANUAL" ("HNYPIGEN")
                  goto L41130

L41735:        if keyhit% <> 15 then L41755
                  call "PRNTSCRN"
                  goto L41130

L41755:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                  S C R E E N   P A G E   3                *~
            *                                                           *~
            *************************************************************

            deffn'103(fieldnr%)
                  line2$ = "Define Ticket Number Format For Count Date " ~
                           & session_date$
                  str(line2$,62%) = "HNYPIGEN: " & str(cms2v$,,8%)
                  if fieldnr% = 0% then init(hex(81)) lfac$()            ~
                                   else init(hex(8c)) lfac$()

                  on fieldnr% gosub L42206,         /* Prefix           */~
                                    L42228,         /* Starting Ticket  */~
                                    L42206,         /* Check Digit ?    */~
                                    L42228,         /* Extra Tickets    */~
                                    L42228          /* Ticket Sequence  */

                  if option$ = "C" and lot_or_loc$ = "P"                 ~
                                             then init(hex(8c)) lfac$(4)
                  goto L42250

L42206:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L42228:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L42250:     accept                                                       ~
               at (01,02),                                               ~
        "Start Physical Inventory/Cycle Count Session",                  ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02),                                               ~
        "Ticket Number Prefix       :",                                  ~
               at (05,32), fac(lfac$(01)),        prefix$       , ch(03),~
               at (05,39),                                               ~
        "(Example - warehouse to count)",                                ~
               at (07,02),                                               ~
        "Starting Ticket Number     :",                                  ~
               at (07,32), fac(lfac$(02)),        start_ticket$ , ch(06),~
               at (07,39),                                               ~
        "(Leading Zeroes Optional)",                                     ~
               at (09,02),                                               ~
        "Append Data Entry Check # ?:",                                  ~
               at (09,32), fac(lfac$(03)),        check_digit$  , ch(01),~
               at (09,39),                                               ~
        "(Y or N. If Y extra check digit appended)",                     ~
               at (11,02),                                               ~
        "Number of 'EXTRA' Tickets  :",                                  ~
               at (11,32), fac(lfac$(04)),        extra$        , ch(05),~
               at (11,39),                                               ~
        "(The number of blank tickets to generate)",                     ~
               at (13,02),                                               ~
        "Ticket Number Sequence     :",                                  ~
               at (13,32), fac(lfac$(05)),        seq$          , ch(01),~
               at (13,39),                                               ~
        "(See below)",                                                   ~
               at (15,02),                                               ~
        "Ticket Numbers may be generated/printed in one of the following ~
        ~sequences:",                                                     ~
               at (17,08), "1 = By Store/Loc/Part/Lot",                  ~
               at (18,08), "2 = By Store/Part/Lot/Loc",                  ~
               at (19,08), "3 = By Store/Part/Loc/Lot",                  ~
               at (17,46), "4 = By Part/Store/Loc/Lot",                  ~
               at (18,46), "5 = By Part/Loc/Lot/Store",                  ~
               at (19,46), "6 = By Part/Lot/Loc/Store",                  ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), fac(hex(8c)), pfs3$(1)               , ch(79),~
               at (24,02), fac(hex(8c)), pfs3$(2)               , ch(79),~
               keys(hex(000104050d0f1020)),                              ~
               key (keyhit%)

               if keyhit% <> 13 then L42690
                  call "MANUAL" ("HNYPIGEN")
                  goto L42250

L42690:        if keyhit% <> 15 then L42730
                  call "PRNTSCRN"
                  goto L42250

L42730:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        /* PF7)  From Screen 1 */
        see_pi_session
            init(" ") str(plowkey$)
            descr$ = hex(06) & "PI Sessions Currently on File"
            call "PLOWCODE" (#3, plowkey$, descr$, 0%, .30, f1%(3))
            return

        /* PF8)  From Screen 1 */
        select_cc_session
            init(" ") str(plowkey$,,99)
            str(plowkey$) = str(session_cc$)

            mat descr_m = zer : mat inc = zer : init(" ") inc$()
            header$(3) = hex(80) & "Select Cycle Count Session Name"
            header$(1) = "  Session Name     Description                "~
                       & "      Planned Count Date"
            descr_m(01) =     1.12  : descr_m(02) = 0001.0
            descr_m(03) =    13.30  : descr_m(04) = 0018.0
            descr_m(05) =    66.061 : descr_m(06) = 0051.0

            inc(1) = 43.01 : inc$(1) = "P"

            call "PLOWCODE" (#12, plowkey$, descr$, 9000%, 0.42, f1%(12),~
                             header$(), 0, 0, inc(), inc$(), "D", " ",   ~
                             #55, descr_m())
            if f1%(12) = 1% then L47015
                errormsg$ = "Session Not on File"
                return
L47015:     session_cc$ = str(plowkey$, 1,12)
            call "DESCRIBE" (#12, session_cc$, sessiondescr$, 0%, f1%(12))
            init(" ") errormsg$, cat$()
            option$ = "C"  /* Cycle Count */
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Tests data for the items on page 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
            errormsg$ = " "
            if fieldnr% > 0% then L50160
                        gosub L50230                /* PI Session Nbr   */
                        if errormsg$ > " " then return
                        gosub L50720                /* CC Session Nbr   */
                        if errormsg$ > " " then return
                        gosub L50810                /* Plan Count Date  */
                        if errormsg$ > " " then return
                        gosub L50980                /* Session Name     */
                        if errormsg$ > " " then return
                        gosub L50990                /* Blank Only ?     */
                        if errormsg$ > " " then return
                        gosub L50975                /* Gen By ?         */
                        if errormsg$ > " " then return
                        return

L50160:     on fieldnr% gosub L50230,               /* Session Number   */~
                              L50720,               /* CC Session Nbr   */~
                              L50810,               /* Plan Count Date  */~
                              L50920,               /* Session Name     */~
                              L50945,               /* Blank Only ?     */~
                              L50975                /* Gen By ?         */

            return

L50230:     REM TEST DATA FOR Session Number Assigned
                convert session_nbr$ to s%, data goto L50260
                convert s% to session_nbr$, pic(00)
L50260:         call "NUMTEST" (session_nbr$, 1,99,errormsg$,0,test)
                if errormsg$ > " " then errormsg$ = "Session Number Must ~
        ~be between 01 & 99 !"
                if errormsg$ > " " then return
                convert test to session_nbr$, pic(00)
                call "READ100" (#3, session_nbr$, f1%(3))
                if f1%(3) = 0% then return
                k% = 0%
L50340:         call "ASKUSER" (k%, "SESSION ALREADY IN USE!",           ~
                         "Press RETURN to Respecify Session Number",     ~
                         "- OR -",                                       ~
                         "Press PF1 to DELETE the Existing Session")
                if k% > 1% then L50340
                on k% goto L50430
L50400:            errormsg$ = "Respecify the Session Number please (alre~
        ~ady in use)"
                   return
L50430:         call "ASKUSER" (k%, "ARE YOU SURE ?",                    ~
        "Press RETURN to DELETE the Existing Session & All It's Tickets",~
                         "- OR -",                                       ~
                         "Press PF1 to Reconsider your Decision")
                if k% > 1% then L50430
                on k% goto L50400
                k% = 2%
L50500:         call "ASKUSER" (k%, "ARE YOU REALLY SURE ?",             ~
        "Press RETURN to DELETE the Existing Session & All It's Tickets",~
                         "- OR -",                                       ~
                         "Press PF1 to Reconsider your Decision")
                if k% > 1% then L50500
                on k% goto L50400
                REM *** DELETE A SESSION ***
                call "SHOSTAT" ("Now Deleting Old Session Tickets")
                call "DELETE" (#4,session_nbr$,2%) /*Delete all tickets*/
                call "READ100" (#3, session_nbr$, f1%(3))  /* HNYPISYS */
                   if f1%(3) = 0% then return
                get #3 using L50586, sourceflag$
L50586:                FMT POS(371), CH(1)
                call "DELETE" (#3,session_nbr$,2%) /*Delete session hdr*/
                call "DELETE" (#9,session_nbr$,2%) /*Delete Costs      */
                if f2%(08%) = 1% then                                    ~
                    call "OPENCHCK" (#8,0%,f2%(8%),records% /4%,rslt$(8%))
                call "DELETE" (#8,session_nbr$,2%)  /*Location Snapshot*/
                if sourceflag$ <> "C" then return
                str(cc_key$) = "A"& hex(00)  /* Search for Active Sesns */
L50602:         call "PLOWAL1" (#12, cc_key$, 1%, 1%, f1%(12))/*HNYCCSYS*/
                  if f1%(12%) = 0% then return  /* Cant find CC_SES */
                get #12 using L50607, session_cc$, temp_ssnbr$
                if session_nbr$ <> temp_ssnbr$  then L50602 /* Get Next */
                put #12 using L50608, "P", " "
L50607:            FMT POS(44), CH(12), CH(2)
L50608:            FMT POS(43), CH(1), POS(56), CH(2)
                rewrite #12
                str(cc_key$) = str(session_cc$) & "A" & hex(00)

L50630:         call "PLOWNEXT" (#11, cc_key$, 13%, f1%(11)) /*HNYCCDTL*/
                   if f1%(11) = 0% then goto L50715
                   call "DELETE" (#11, cc_key$, 57%) /*Delete HNYCCDTL */
                   put #11 using L50651, "P"
L50651:            FMT POS(13), CH(1)
                   write #11

               call "READ100" (#10, str(cc_key$,14,44), f1%(10))
                 if f1%(10) = 0% then goto L50715
                 call "DELETE" (#10, str(cc_key$,14,44), 44%) /*HNYCCMST*/
                 put #10 using L50705, "P"
L50705:          FMT POS(81), CH(1)
                 write #10
               goto L50630  /* Get next HNYCCDTL */

L50715:        session_cc$ = " "
               return

L50720:     REM TEST DATA FOR Cycle Count Session Number
               if session_cc$ = " " then return
               if session_cc$ <> " " then L50770
L50770:        gosub select_cc_session
               return

L50810:     REM TEST DATA FOR Planned Count Date
                if option$ = "C" then return /* Doing Cycle Count ? */
                call "DATEOK" (session_date$, k%, error$)
                if error$ > " " then errormsg$ = error$
                if errormsg$ > " " then return
                call "DATUNFMT" (session_date$)
                if date > session_date$ then errormsg$ = "Planned count d~
        ~ate must greater than today's date."
                call "DATEFMT" (session_date$)
                return

L50920:     REM TEST DATA FOR Session Name
                return

L50945:     REM TEST DATA FOR Blank Only
                if option$ = "C" then return /* Doing Cycle Count ? */
                if blank_only$ = "NO" or blank_only$ = "YES" then return
                errormsg$ = "Must be YES or NO"
                return

L50975:     REM TEST DATA FOR By Lot or Location ?
L50980:         if lot_or_loc$ = "L" or lot_or_loc$ = "P" then return
                errormsg$ = "By Lot or Location Selection must be L or P"
L50990:         return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Tests data for the items on page 2.                       *~
            *************************************************************

            deffn'152(fieldnr%)
            errormsg$ = " "
            if fieldnr% > 0% then L51090
            for fieldnr% = 1% to 6%
                  on fieldnr% gosub L51135,         /* ABC Classes      */~
                                    L51185,         /* Warehouses       */~
                                    L51340,         /* Categories       */~
                                    L51495,         /* Parts            */~
                                    L51650,         /* Count Neg Only ? */~
                                    L51705          /* Exclude Count Dat*/
            if errormsg$ <> " " then fieldnr% = 6%
            next fieldnr%
            return

L51090:     on fieldnr% gosub L51135,               /* ABC Classes      */~
                              L51185,               /* Warehouses       */~
                              L51340,               /* Categories       */~
                              L51495,               /* Parts            */~
                              L51650,               /* Count Neg Only ? */~
                              L51705                /* Exclude Count Dat*/

            return

L51135:     REM TEST DATA FOR ABC Classes to Count
                if abc$ = " " then return
                if abc$ = "ALL" then return
                if pos(" ABCDX" = str(abc$,1%,1%)) = 0% then L51170
                if pos(" ABCDX" = str(abc$,2%,1%)) = 0% then L51170
                if pos(" ABCDX" = str(abc$,3%,1%)) = 0% then L51170
                if pos(" ABCDX" = str(abc$,4%,1%)) = 0% then L51170
                if pos(" ABCDX" = str(abc$,5%,1%)) = 0% then L51170
                return
L51170:        errormsg$ = "ABCD Classes must be ALL, ' ', 'A', 'B', 'C',~
        ~ 'D', 'X' or a combination thereof"
                return
L51185:     REM TEST DATA FOR Warehouses to Count (Range)
                if  str(whse$(),1) = " " then whse$(1,1) = "ALL"
                if whse$(1,1) <> "ALL" then L51210
                   str(whse$(),4%) = "  "
                   return
L51210:         for x% = 1% to 9%
                    if whse$(x%,1%)=" " and whse$(x%,2%)=" " then L51280
                    row% = -((x% - 1%) / 3% + 6%)
                    f1%(5) = row%
                    descr$ = hex(06) & "SELECT WAREHOUSE (" &            ~
                             whse$(x%,1%) & ")"
                    call "PLOWCODE" (#5,whse$(x%,1%),descr$,0%,.3,f1%(5))
                    if f1%(5) = 0% then L51290
                    f1%(5) = row%
                    descr$ = hex(06) & "SELECT WAREHOUSE (" &            ~
                             whse$(x%,2%) & ")"
                    call "PLOWCODE" (#5,whse$(x%,2%),descr$,0%,.3,f1%(5))
                    if f1%(5) = 0% then L51305
                    if whse$(x%,1%) > whse$(x%,2%) then L51320
L51280:         next x%
                return
L51290:         errormsg$ = "Warehouse " & whse$(x%,1%) &                ~
                            " not found! Please Reenter"
                return
L51305:         errormsg$ = "Warehouse " & whse$(x%,2%) &                ~
                            " not found! Please Reenter"
                return
L51320:         errormsg$ = "FROM Warehouse " & whse$(x%,2%) &           ~
                            " Cannot be greater than TO Warehouse " &    ~
                            whse$(x%,1%)
                return
L51340:     REM TEST DATA FOR Categories to Count (Range)
*              IF  STR(CAT$(),1) = " " THEN CAT$(1,1) = "ALL"
                if  cat$(1,1) <> "ALL" then L51362
                   str( cat$(),5%) = "  "
                   return
L51362:         if cat$(1%,1%) = " " and cat$(1%,2%) = " "               ~
                     then start% = 2% else start% = 1% /*Allow 1st Blank*/
                for x% = start% to 9%
                    if  cat$(x%,1%)=" " and  cat$(x%,2%)=" " then L51435
                    row% = -((x% - 1%) / 3% + 9%)
                    f1%(6) = row%
                    descr$ = hex(06) & "SELECT CATEGORY (" &             ~
                              cat$(x%,1%) & ")"
                    call "PLOWCODE" (#6, cat$(x%,1%),descr$,0%,.3,f1%(6))
                    if f1%(6) = 0% then L51445
                    f1%(6) = row%
                    descr$ = hex(06) & "SELECT CATEGORY (" &             ~
                              cat$(x%,2%) & ")"
                    call "PLOWCODE" (#6, cat$(x%,2%),descr$,0%,.3,f1%(6))
                    if f1%(6) = 0% then L51460
                    if  cat$(x%,1%) >  cat$(x%,2%) then L51475
L51435:         next x%
                return
L51445:         errormsg$ = "Category " &  cat$(x%,1%) &                 ~
                            " not found! Please Reenter"
                return
L51460:         errormsg$ = "Category " &  cat$(x%,2%) &                 ~
                            " not found! Please Reenter"
                return
L51475:         errormsg$ = "FROM Category " &  cat$(x%,2%) &            ~
                            " Cannot be greater than TO Category " &     ~
                             cat$(x%,1%)
                return
L51495:     REM TEST DATA FOR Parts to Count (Range)
                if  str(part$(),1) = " " then part$(1,1) = "ALL"
                if part$(1,1) <> "ALL" then L51520
                   str(part$(),26%) = "  "
                   return
L51520:         for x% = 1% to 3%
                    if part$(x%,1%)=" " and part$(x%,2%)=" " then L51590
                    row% = -((x% - 1%) / 3% + 12%)
                    f1%(1) = row%
                    descr$ = hex(06) & "SELECT PART (" &                 ~
                             part$(x%,1%) & ")"
                    call "PLOWCODE" (#1,part$(x%,1%),descr$,0%,.32,f1%(1))
                    if f1%(1) = 0% then L51600
                    f1%(1) = row%
                    descr$ = hex(06) & "SELECT PART (" &                 ~
                             part$(x%,2%) & ")"
                    call "PLOWCODE" (#1,part$(x%,2%),descr$,0%,.32,f1%(1))
                    if f1%(1) = 0% then L51615
                    if part$(x%,1%) > part$(x%,2%) then L51630
L51590:         next x%
                return
L51600:         errormsg$ = "Part Code " & part$(x%,1%) &                ~
                            " not found! Please Reenter"
                return
L51615:         errormsg$ = "Part Code " & part$(x%,2%) &                ~
                            " not found! Please Reenter"
                return
L51630:         errormsg$ = "FROM Part " & part$(x%,2%) &                ~
                            " Cannot be greater than TO Part " &         ~
                            part$(x%,1%)
                return
L51650:     REM TEST DATA FOR Count Neg On-Hand Only ?
                if neg_only$ = "A" or neg_only$ = "N" then return
                if neg_only$ = "X" then return
                errormsg$ = "On hand status selection must be A, X or N"
                return
L51705:     REM TEST DATA FOR Exclude Parts Counted On/After ...
                if errormsg$ > " " then return
                call "DATEOK" (count_date$, u3%, errormsg$)
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Tests data for the items on page 3.                       *~
            *************************************************************

            deffn'153(fieldnr%)
            errormsg$ = " "
            if fieldnr% > 0% then L52180

            for fieldnr% = 1% to 5%
                  on fieldnr% gosub L52230,         /* Ticket Nbr Prefix*/~
                                    L52250,         /* Start Ticket Nbr */~
                                    L52300,         /* Check Digit ?    */~
                                    L52340,         /* Extra Tickets    */~
                                    L52380          /* Ticket Nbr Seq   */
                  if errormsg$ > " " then fieldnr% = 5%
            next fieldnr%
            return

L52180:     on fieldnr% gosub L52230,               /* Ticket Nbr Prefix*/~
                              L52250,               /* Start Ticket Nbr */~
                              L52300,               /* Check Digit ?    */~
                              L52340,               /* Extra Tickets    */~
                              L52380                /* Ticket Nbr Seq   */
            return

L52230:     REM TEST DATA FOR Ticket Number Prefix (Opt)
                return
L52250:     REM TEST DATA FOR Starting Ticket Number
                convert start_ticket$ to test, data goto L52280
                if test < 0 then L52280
                return
L52280:         errormsg$ = "Starting Ticket Number must be numeric!"
                return
L52300:     REM TEST DATA FOR Calc & Append Check Digit?
                if check_digit$ = "Y" or check_digit$ = "N" then return
                errormsg$ = "Invalid Entry, must be Y or N"
                return
L52340:     REM TEST DATA FOR Number of Extra Tickets
                call "NUMTEST" (extra$,0,99999,error$,0,extra)
                if error$ > " " then errormsg$ = error$
                return
L52380:     REM TEST DATA FOR Ticket Number Sequence
                if seq$ > "0" and seq$ < "7" then return
                errormsg$ = "Ticket sequence must be 1,2,3,4,5, or 6"
                return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1985, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

        exit_program
            call "SHOSTAT" ("Closing Files & Returning to Menu")
            call "FILEBGON" (#50)
            end
