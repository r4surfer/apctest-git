        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP   IIIII  PPPP    OOO     AA    SSSSS  U   U  BBBBB  *~
            *  P   P    I    P   P  O   O   A  A   S      U   U  B   B  *~
            *  PPPP     I    PPPP   O   O  A AA A  SSSSS  U   U  AAAAB  *~
            *  P        I    P      O   O  A    A      S  U   U  B   B  *~
            *  P      IIIII  P       OOO   A    A  SSSSS  UUUUU  BBBBB  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPOASUB - ALLOWS USER TO REPORT ON PIP OUT'S USING A     *~
            *            RANGE OF CRITERIA.  PIPOUT'S INCLUDE SALES TO  *~
            *            BE SHIPPED, PARTS TO BE KITTED, PURCHASES FOR  *~
            *            SPECIFIC JOBS/PROJECTS.                        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1984, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/03/84 ! ORIGINAL                                 ! BLT *~
            * 12/26/85 ! File changes- VBKMASTR, VBKLINES,        ! ERN *~
            *          !  VBKBUFFR, VBKBUF2                       !     *~
            *          !  Also to V2PMASTR/LINES                  !     *~
            * 11/05/86 ! File changes- BCKMASTR, BCKLINES         ! ERN *~
            * 07/29/87 ! Std Costing Enhancements, Misc clean-up. ! ERN *~
            * 03/10/88 ! Made A Special Subroutine For Receiving  ! MDE *~
            * 03/10/88 ! Fixed Bug - Endless Loop                 ! MDE *~
            * 03/28/89 ! Corrected FMT ln 12750 for 7 dec unit prc! MJB *~
            * 09/20/89 ! Added default of ALL for part range.     ! JDH *~
            * 04/23/91 !(PRR 11792) Added clearing of all numeric ! RJB *~
            *          !     and Alpha Display fields immediately !     *~
            *          !     after their use. Also added call to  !     *~
            *          !     "ALLFREE" as per new standards.      !     *~
            *          !QC-FIXES Added a clearing of fields that  ! RJB *~
            *          ! was missed first time, removed 'ALLFREE' !     *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 11/05/92 ! PRR 12014 - Stoped display of PF(2,11,12)! RJH *~
            *          !  in edit mode.                           !     *~
            *          ! PRR 12192 - Fixed use of date range for  !     *~
            *          !  PF(2,12) display and report.            !     *~
            *          ! PRR 12246 - Fixed return to INPUT without!     *~
            *          !  looking at all the PIP in PF(2) display.!     *~
            *          ! Added END OF REPORT Msg. w/ time stamp.  !     *~
            * 01/05/93 ! Minor Screen Mods                        ! RJH *~
            * 10/26/93 ! Purchase Job Project - Added Support for ! JBK *~
            *          !   'BW' and 'RW' PIPOUT Records.  Moved   !     *~
            *          !   coding @12400 thru 13140 to 18000 and  !     *~
            *          !   re-organized and re-wrote routine.     !     *~
            * 09/18/97 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "PIPOASUB" (p1$,#1,#8,#11,#12)

        dim                                                              ~
            begdate$8,                   /* BEGINNING DATE             */~
            bdate%(1),                                                   ~
            tdate%(1),                                                   ~
            beginpart$25,                /* BEGINNING PART NUMBER      */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            blankline$79,                                                ~
            bkorder$(1000)10,                                            ~
            bw$1,                        /* Need by Buy Work Advice    */~
            col_header$51,               /* Screen Column Header       */~
            cq$1,                        /* Display Variable           */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            custcode$9,                                                  ~
            descrmsg$(1000)27,                                           ~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            dd$1,                        /* Display Variable           */~
            duedate$(1000)8,                                             ~
            edate%(1),                                                   ~
            hdrdate$45,                                                  ~
            include$(7)1,                /* Types of items to include  */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            fq$1,                        /* Display Variable           */~
            jobno$8,                     /* Job Number                 */~
            line2$79,                    /* SCREEN LINE 2              */~
            mm$1,                        /* Display Variable           */~
            partdescr$34,                                                ~
            partdescre$34,                                               ~
            part$25,                                                     ~
            pf2edt$40,                   /* Edit Mode PF2 Message      */~
            pf11edt$40,                  /* Edit Mode PF11 Message     */~
            pf12edt$40,                  /* Edit Mode PF12 Message     */~
            pf16edt$17,                  /* Edit Mode PF12 Message     */~
            pfkeys$33,                   /* PF Key Hex Values          */~
            tdate$8,                     /* Todays date                */~
            enddate$8,                   /* ENDING DATE                */~
            udate$8,                     /* Temporary Date             */~
            unitvalue$10,                                                ~
            extension$10,                                                ~
            ww$1,                        /* Display Variable           */~
            rptfac$(5)1,                                                 ~
            rw$1,                        /* Need Buy Work Directives   */~
            selectcode$8,                                                ~
            endpart$25,                  /* ENDING PART NUMBER         */~
            dateout$3,                                                   ~
            time$10,                     /* System Time                */~
            totextension$12,                                             ~
            totonorder$12,                                               ~
            errormsg$79,                 /* ERROR MESSAGE              */~
            header$79,                                                   ~
            header1$79,                                                  ~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            jo$1,                        /* NEED BY JOB ORDER          */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            original$(1000)10,                                           ~
            pipouttg$(1000)19,                                           ~
            setptrhdr$12,                                                ~
            p1$25,                       /* Passed Part      */          ~
            po$1,                        /* NEED BY PURCHASE ORDER     */~
            plowkey$100,                                                 ~
            readkey$100,                                                 ~
            userid$3,                                                    ~
            yymmdd$(490)6,                                               ~
            shipped$(1)10,                                               ~
            so$1,                        /* NEED BY SALES ORDER        */~
            onorder$(1)10,                                               ~
            total$(1)10,                                                 ~
            wo$1,                        /* NEED BY WORK ORDER         */~
            yy%(490),                                                    ~
            mm%(490),                                                    ~
            dd%(490),                                                    ~
            mwoy%(490),                                                  ~
            cqoy%(490),                                                  ~
            fqoy%(490)                                                   ~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
           mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! USERINFO ! User default information file            *~
            * #2  ! PIPOUT   ! planned inventory position demand file   *~
            * #3  ! JOBMASTR ! PROJECT  master file                     *~
            * #5  ! BCKLINES ! sales line item file                     *~
            * #6  ! CALMASTR ! calendar master                          *~
            * #7  ! JBMASTR2 ! production job master file               *~
            * #8  ! HNYMASTR ! inventory master                         *~
            * #11 ! VBKMASTR ! PO MASTER FILE                           *~
            * #12 ! VBKLINES ! PO LINE ITEM FILE                        *~
            * #17 ! WORKFILE ! SYSTEM WORK FILE                         *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *


            select #2,  "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   64,                                  ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select #3,  "JOBMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  700,                                  ~
                        keypos =    1, keylen =   8                      ~

            select #5,  "BCKLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 10,   keylen =  19

            select #6,  "CALMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1962,                                  ~
                        keypos =    1, keylen =   2                      ~

            select #7,  "JBMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1300,                                 ~
                        keypos =    1, keylen =   8                      ~



            select #17, "WORKFILE",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  132,                                  ~
                        keypos =    1, keylen =  55

        REM CALL "SHOSTAT" ("Opening Files, One Moment Please")
            if beenherbefore% <> 0% then L09000

            call "OPENFILE" (#2,  "SHARE", f2%(2 ), rslt$(2 ), axd$(2 ))
            call "OPENFILE" (#3,  "SHARE", f2%(3 ), rslt$(3 ), axd$(3 ))
            call "OPENFILE" (#5,  "SHARE", f2%(5 ), rslt$(5 ), axd$(5 ))
            call "OPENFILE" (#6,  "SHARE", f2%(6 ), rslt$(6 ), axd$(6 ))
            call "OPENFILE" (#7,  "SHARE", f2%(7 ), rslt$(7 ), axd$(7 ))
            beenherbefore% = 9999%

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."
            str(line2$,62) = "PIPOASUB: " & str(cms2v$,,8)

            header$ = "PIP OUT ANALYSIS BY INDIVIDUAL PART"
            header1$="  DATE     TO BE USED FOR                          ~
        ~         ORIGINAL     LEFT"
            col_header$ = "From                      To"

            call "READ100" (#6,"10",f1%(6))
            if f1%(6)<>1 then L65000
            get #6,using L09180,str(yymmdd$(),1,1470)
L09180:        FMT XX(2),CH(1470)
            call "READ100" (#6,"11",f1%(6))
            get #6,using L09180, str(yymmdd$(),1471,1470)
L09206:        FMT XX(2),490*BI(4)
            call "READ100" (#6,"30",f1%(6))
            get #6,using L09206, mm%()
            call "READ100" (#6,"40",f1%(6))
            get #6,using L09206, dd%()
            call "READ100" (#6,"60",f1%(6))
            get #6, using L09206, mwoy%()
            call "READ100" (#6,"70",f1%(6))
            get #6, using L09206, cqoy%()
            call "READ100" (#6,"71",f1%(6))
            get #6, using L09206, fqoy%()
            call "READ100" (#6,"20",f1%(6))
            get #6, using L09206, yy%()
            call "EXTRACT" addr("ID",userid$)
            call "READ100" (#1, userid$, f1%(1))
            if f1%(1)=0 then L65000

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                  P A G E  1                               *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, begdate$, beginpart$,      ~
                      enddate$, endpart$, jo$, po$, so$, wo$, duedate$(),~
                      pipouttg$(), descrmsg$(), original$(), shipped$(), ~
                      partdescr$, partdescre$, include$(), bw$, rw$,     ~
                      dd$, mm$, ww$, fq$, cq$, bkorder$(), custcode$
            begdate$ = "ALL"
            so$, jo$, po$, wo$, bw$, rw$ = "Y"
            dd$ = "X"
            pagenumber% = 0%
            beginpart$ = p1$

            for fieldnr% = 1 to 3
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10230
L10170:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then L65000
                      if keyhit% <>  0 then       L10170
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10170
L10230:         next fieldnr%

L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

L11051:     inpmessage$ = edtmessage$
            pf2edt$ =  "( 2)Display One Part at a time"
            pf11edt$ = "(11)Print for Range Shown by Parts"
            pf12edt$ = "(12)Print for Range Shown by Dates"
            pf16edt$ = "(16)Exit Program"
            gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then L12000
                  if keyhit%  = 11 then L13410
                  if keyhit%  = 12 then L13930
                  if keyhit%  = 16 then L65000
                  if keyhit% <>  0 then L11051
            fieldnr% = 0%
            if cursor%(1%) =  6% then fieldnr% = 1%
            if cursor%(1%) =  7% then fieldnr% = 2%
            if cursor%(1%) > 9% and cursor%(1%) < 16% then fieldnr% = 3%
            if fieldnr% < 1% or fieldnr% > 3% then L11051

            gosub'051(fieldnr%)
            pf2edt$, pf11edt$, pf12edt$, pf16edt$ = " "
L11190:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11190
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11190
            goto L11051

L12000: REM *************************************************************~
            *             S H 0 W   D A T A   O N   F I L E             *~
            *                                                           *~
            * CONTROLS DISPLAY OF RECORDS FROM DATA FILES.              *~
            *************************************************************

            init (hex(00)) readkey$, plowkey$
            if beginpart$ = "ALL" then endpart$   = all(hex(ff))
            if beginpart$ = "ALL" then beginpart$ = " "
            if beginpart$ = " "   then L12110
               str(readkey$,1,25)= str(beginpart$,1,25) addc all(hex(ff))
               str(plowkey$,1,25)= str(beginpart$,1,25) addc all(hex(ff))
L12110:     totonorder, tottotal = 0
            init (" ") duedate$(), pipouttg$(), descrmsg$(), original$(),~
                       shipped$(), bkorder$(), total$(), part$,          ~
                       partdescr$

            max% = 0%
            call "PLOWALTS" (#2, readkey$, 1%, 0%, f1%(2))
                if f1%(2) = 0% then L13150
            get #2 using L35060, pipouttg$(max%+1), part$, datein%,onorder
            call "DESCRIBE" (#8, part$, partdescr$, 1%, f1%(8))
            goto L12260

L12230:     call "PLOWALTS" (#2, readkey$, 1%, 25%, f1%(2))
                if f1%(2) = 0% then L13190
            get #2 using L35060, pipouttg$(max%+1), part$, datein%,onorder
L12260:     if datein%   >  edate%(1) then L12230
*          IF BDATE%(1) <= TDATE%(1) THEN 12290
            if datein%   <  bdate%(1) then L12230
            if part$     >  endpart$  then inputmode
            if part$ = " " and beginpart$ = endpart$ then inputmode
            gosub what_is_it_and_do_we_want_it
            if ok% = 0% then L12230

            duedate$(max%) = yymmdd$(datein%)
            call "DATEFMT" (duedate$(max%))
            onorder    = round(onorder, 2)
            totonorder = totonorder + onorder
            call "CONVERT" (onorder, 2.2, bkorder$(max%))
            goto L12230

L13150:     max% = 2%
            pipouttg$(max%) = "END OF FILE"
            goto L13270

L13190:     if max% = 0% and part$ > endpart$ then inputmode
            if max% = 0% then L12110     /* Back for another part to try */
            pipouttg$(max%+1) = " "
            max% = max% + 2%
            pipouttg$(max%) = "TOTAL"
            call "CONVERT" (tottotal  , 2.2, original$(max%))
            call "CONVERT" (totonorder, 2.2, bkorder$(max%))
            original$(max%) = " "

L13270:     line% = 0%
L13280:     gosub L42000
                if keyhit%  =  0  then       L12110
                if keyhit%  =  1  then gosub startover
                if keyhit%  =  2  then line%=0
                if keyhit%  =  3  then line%=max(0,max%-5)
                if keyhit%  =  4  then line%=max(0,line%-15)
                if keyhit%  =  5  then line%=min(line%+15,max(0,max%-5))
                if keyhit%  =  6  then line%=max(0,line%-1)
                if keyhit%  =  7  then line%=min(line%+1,max(0,max%-5))
                if keyhit%  = 16  then goto inputmode
            goto L13280


L13410: REM *************************************************************~
            *          P R I N T  O U T  P I P O U T  I T E M S         *~
            *************************************************************
            call "SETPRNT" ("PLN001", " ", 0%, 0%)
            whichreport% = 1%
            call "SHOSTAT" ("Now printing, one moment please")

            init (hex(00)) plowkey$
            if beginpart$ = "ALL" then endpart$   = all(hex(ff))
            if beginpart$ = "ALL" then beginpart$ = " "
            if beginpart$ = "ALL" or beginpart$ = " " then L13510
                str(plowkey$,,25) = str(beginpart$,,25) addc all(hex(ff))
L13510:     ky%    = 1%
            break% = 25%
L13530:     line%  = 1000%
            call "PLOWALTS" (#2, plowkey$, ky%, 0%, f1%(2))
            if f1%(2) = 0% then exit_print else L13590

L13570:     call "PLOWALTS" (#2, plowkey$, ky%, break%, f1%(2))
                if f1%(2) <> 1% then break_print_routine
L13590:     get #2, using L35060, pipouttg$(1), part$, datein%, onorder
            if part$   > endpart$  then exit_print
            if datein% < bdate%(1) then L13570
            if datein% > edate%(1) then L13570
            ok%, max% = 0%
            gosub what_is_it_and_do_we_want_it
            if ok% = 0% then L13570
            if line%> 55% then gosub new_page
            duedate$(1)= yymmdd$(datein%) : call "DATEFMT" (duedate$(1))
            onorder   = round(onorder  , 2)
            unitprice = round(unitprice, 2)
            extension = round(onorder*unitprice, 2)
            call "CONVERT" (onorder,   2.2, onorder$(1))
            call "CONVERT" (unitprice, 2.2, unitvalue$)
            call "CONVERT" (extension, 2.2, extension$)
            totonorder   = totonorder + onorder
            totextension = totextension + extension
            onorder, unitprice, extension = 0
            print using L15420, duedate$(1),part$, pipouttg$(1),          ~
                     descrmsg$(1), original$(1), onorder$(1), unitvalue$,~
                     extension$
            line% = line% + 1%
            init(" ") duedate$(), pipouttg$(), descrmsg$(), original$(), ~
                      onorder$(), unitvalue$, extension$
            goto L13570

        break_print_routine
            if line% > 500% then L13530
            print : print
            call "CONVERT" (totonorder  , 2.2, totonorder$)
            call "CONVERT" (totextension, 2.2, totextension$)
            print using L15550, totonorder$, totextension$
            totonorder, totextension = 0
            goto L13530

L13930:     init(hex(80)) rptfac$()
            init(hex(8c)) lfac$()
            init(hex(00)) plowkey$
            inpmessage$ = "Select one (and only one) grouping format "  &~
                "with a non-blank character"
L13954:     gosub L41280
            if keyhit% <> 1% then goto L13962
                gosub startover : goto L13954
L13962:     if keyhit% = 16% then goto L65000
            if keyhit% <> 0% then goto L13954
            if beginpart$ = "ALL" then endpart$   = all(hex(ff))
            if beginpart$ = "ALL" then beginpart$ = " "
            call "SETPRNT" ("PLN001", " ", 0%, 0%)
            whichreport% = 2%
            call "SHOSTAT" ("Now printing, one moment please")
            call "WORKOPEN" (#17,"IO   ", 5000%, f2%(17))
            if f2%(17) = 0% then L14030
                errormsg$ = "Work File cannot be opened at this time"
                goto L11000
L14030:     call "PLOWALTS" (#2, plowkey$, 1%, 0%, f1%(2))
            if f1%(2) = 0% then L14530
            get #2 using L35060, pipouttg$(1), part$, datein%, onorder
            if part$   > endpart$   then L14530
            if datein% < bdate%(1)  then L14030
            if datein% > edate%(1)  then L14030
            if part$   < beginpart$ then L14030
            ok%, max% = 0%
            gosub what_is_it_and_do_we_want_it
            if ok% = 0% then L14030
            convert datein% to dateout$, pic(000)
            if fq$ = " " then L14170
                setptr% = fqoy%(datein%)
                setptrhdr$ = "FISCAL QTR  "
                goto L14320
L14170:     if cq$ = " " then L14210
                setptr% = cqoy%(datein%)
                setptrhdr$ = "CALENDAR QTR"
                goto L14320
L14210:     if mm$ = " " then L14250
                setptr% = mm%(datein%)
                setptrhdr$ = "MONTH       "
                goto L14320
L14250:     if ww$=" " then L14290
                setptr% = mwoy%(datein%)
                setptrhdr$ = "WEEK        "
                goto L14320
L14290:     setptr% = dd%(datein%)

            convert mm%(datein%) to str(selectcode$,5%,2%), pic(00)
            setptrhdr$ = "DAY         "
L14320
            convert setptr% to str(selectcode$,7%,2%), pic(00)
            convert yy%(datein%) to str(selectcode$,1%,4%), pic (0000)
            str(readkey$, 1, 8) = str(selectcode$,,8)
            str(readkey$, 9, 3) = str(dateout$,,3)
            str(readkey$,12,25) = str(part$,,25)
            str(readkey$,37,19) = str(pipouttg$(1),,19)
            call "READ101" (#17, str(readkey$,,55), f1%(17))
                if f1%(17) = 0% then L14450
            get #17 using L14490, selectcode$, dateout$, part$,           ~
                                 pipouttg$(1), orderamt, original,       ~
                                 shipped, bckorder, unitprice,           ~
                                 descrmsg$(1), original$(1)
            onorder = round(onorder + orderamt, 2)
L14450:     put #17 using L14490, selectcode$, dateout$, part$,           ~
                                 pipouttg$(1), onorder, original,        ~
                                 shipped, bckorder, unitprice,           ~
                                 descrmsg$(1), original$(1)
L14490:      FMT CH(8), CH(3), CH(25), CH(19), 5*PD(14,4), CH(27), CH(10)
            if f1%(17) = 0% then write #17 else rewrite #17
            init(" ") selectcode$, dateout$, part$, pipouttg$( ),        ~
                      descrmsg$( ), original$( )
            onorder, original, shipped, bckorder, unitprice = 0
            goto L14030

L14530:     init(hex(00)) plowkey$
L14540:     line% = 1000%
            call "PLOWALTS" (#17, plowkey$, 0%, 0%, f1%(17))
                if f1%(17)=0% then exit_print
                goto L14610

L14590:     call "PLOWALTS" (#17, plowkey$, 0%, 8%, f1%(17))
                if f1%(17) = 0% then break_date_print
L14610:     if line% > 55% then gosub new_date_page
            get #17 using L14490, selectcode$, dateout$, part$,           ~
                                 pipouttg$(1), onorder, original,        ~
                                 shipped, bckorder, unitprice,           ~
                                 descrmsg$(1), original$(1)
            onorder   = round(onorder, 2)
            unitprice = round(unitprice, 2)
            extension = round(onorder*unitprice, 2)
            call "CONVERT" (onorder  , 2.2, onorder$(1))
            call "CONVERT" (unitprice, 2.2, unitvalue$)
            call "CONVERT" (extension, 2.2, extension$)
            datein% = 490% : convert dateout$ to datein%, data goto L14720
L14720:     duedate$(1)  = yymmdd$(datein%) : call "DATEFMT" (duedate$(1))
            totonorder   = totonorder + onorder
            totextension = totextension + extension
            onorder, unitprice, extension = 0
            print using L15420, duedate$(1), part$, pipouttg$(1),         ~
                               descrmsg$(1), original$(1), onorder$(1),  ~
                               unitvalue$, extension$
            line% = line%+1%
            init(" ") duedate$(), pipouttg$(), descrmsg$(), original$(), ~
                      onorder$(), unitvalue$, extension$
            goto L14590

        break_date_print
            print : print
            call "CONVERT" (totonorder,   2.2, totonorder$)
            call "CONVERT" (totextension, 2.2, totextension$)
            print using L15550, totonorder$, totextension$
            totonorder, totextension = 0
            goto L14540

        exit_print
            time$ = " "  :  call "TIME" (time$)
            print
            print using L15580, time$
            close printer
            call "FILEBGON" (#17)
            goto inputmode

        new_page
            select printer (134)
            if pagenumber% = 0% then gosub print_params
            print page
            line% = 14%
            pagenumber% = pagenumber% + 1%
            call "DESCRIBE" (#8, str(plowkey$,,25), partdescr$,0%,f1%(8))
            call "DATE" addr ("HD", hdrdate$)
            print using L15320, pagenumber%, hdrdate$
            print using L15350
            if enddate$ <> "ALL" and enddate$ <> " " and enddate$ <> blankdate$~
                then print using L15360, begdate$, enddate$               ~
                else print using L15370
            print
            print using L15400, str(plowkey$,,25), partdescr$
            print
            print using L15450
            print using L15480
            print using L15510
            print
            return

        new_date_page
            select printer (134)
            if pagenumber% = 0% then gosub print_params
            print page
            line% = 14%
            pagenumber% = pagenumber% + 1%
            call "DATE" addr ("HD", hdrdate$)
            print using L15320, pagenumber%, hdrdate$
            print using L15380, setptrhdr$
            if enddate$ <> "ALL" and enddate$ <> " " and enddate$ <> blankdate$~
                then  print using L15360, begdate$, enddate$              ~
                else  print using L15370
            print
            print using L15450
            print using L15480
            print using L15510
            print
            return

L15320: %PAGE #####   PLN001     ANALYSIS OF SCHEDULED PART USAGE & SHIPM~
        ~ENTS                   ##########################################~
        ~###
L15350: %                                 BY INVENTORY PART NUMBER
L15360: %                        FOR DATES FROM ######## TO ######## INCL~
        ~USIVE
L15370: %                                 WITH ALL DATES INCLUDED
L15380: %                        PART USAGE ITEMS ARE GROUPED BY ########~
        ~####
L15400: %PART NUMBER:  #########################     ####################~
        ~############
L15420: % ######### #########################  ###################  #####~
        ~######################  ########## ########## ########## ########~
        ~##
L15450: %================================================================~
        ~=================================================================~
        ~===
L15480: %!   DATE  !PART NUMBER               !TO BE USED FOR      ! DESC~
        ~RIPTION                ! ORIGINAL !QUAN LEFT !UNIT VALUE! EXTENSI~
        ~ON!
L15510: %!---------+--------------------------+--------------------+-----~
        ~-----------------------+----------+----------+----------+--------~
        ~--!

L15550: %                                      TOTAL                     ~
        ~                                 ############          ##########~
        ~##
L15580: %                      * * * * *  E N D   OF  R E P O T   @   ###~
        ~#######  * * * * *

        print_params
L15710:     ii% = pos(str(i$()) > hex(7f))
            if ii% = 0% then L15760
                str(i$(), ii%, 1%) = hex(20)
                goto L15710

L15760:     print page
            call "DATE" addr ("HD", hdrdate$)
            print using L15320, pagenumber%, hdrdate$
            if whichreport% = 1% then print using L15350
            if whichreport% = 2% then print using L15380, setptrhdr$
            if enddate$ <> "ALL" and enddate$ <> " " and enddate$ <> blankdate$~
                then print using L15360, begdate$, enddate$               ~
                else print using L15370

            print skip(3)
            print tab(25);
            print "--------------------- Report Selection Parameters ----~
        ~---------"
            for x% = 5% to 19%: print tab(25); i$(x%) : next x%
            print tab(25);
            print "------------------------------------------------------~
        ~---------"

            return

        REM *************************************************************~
            *  Common Subroutine to determine what kind of PIPOUT we    *~
            *  are dealing with and decide whether we want to see or    *~
            *  print it.                                                *~
            *************************************************************
        what_is_it_and_do_we_want_it
            ok% = 0%

            search "JOWOBWRWPOQC" = str(pipouttg$(max%+1%),,2%) to       ~
                                    cursor%() step 2%
            index% = int((cursor%(1%)+1%)/2%) + 1%
            if index% < 1% or index% > 7% then return
            if include$(index%) <> "Y" then return
            ok%, max% = max% + 1%

            on index% goto L18170, L18350, L18650, L18750, L18850, L18950, L18950
            return

L18170
*        Must have Sales Order Tag, By Default
            call"READ100" (#5, str(pipouttg$(max%),1%,19), f1%(5%))
                if f1%(5%) = 0% then return
            get #5 using L18230, custcode$, original, shipped, bckorder,  ~
                                unitprice
L18230:         FMT CH(9), POS(93), 3*PD(14,4), POS(141), PD(14,4)
            str(descrmsg$(max%),1,14) = "SHIP TO CUST: "
            str(descrmsg$(max%),15,9) = custcode$
            goto L19200

L18350
*        JO Tag, Job Order (Production Job) or Project
            if str(pipouttg$(max%),,9%) = "JOB(PROJ)" then L18410
            if str(pipouttg$(max%),,9%) = "JOB ORDER" then L18490
                /* Not true JO tag, so maybe a sales order  */
                if include$(1%) = "Y" then L18170
                     ok% = 0%  :  max% = max% - 1%
                     return

L18410
*        Project or 'JOB(PROJ)'
            call "READ100" (#3, str(pipouttg$(max%),12%,8%), f1%(3%))
                if f1%(3%) = 0% then return
            get #3 using L18450, descrmsg$(max%), original
L18450:         FMT XX(8), CH(26), XX(50), PD(14,4)
            shipped = 0
            goto L18540

L18490
*        Production Job
            call "READ100" (#7, str(pipouttg$(max%),12,8), f1%(7%))
                if f1%(7%) = 0% then return
            get #7 using L18530, descrmsg$(max%), original, shipped
L18530:         FMT XX(8), CH(26), XX(48), PD(14,4), PD(14,4)
L18540:     shipped  = round(shipped , 2)
            goto L19200

L18650
*        'WO' Tag, Work Order
            descrmsg$(max%) = "PLANNED  - NOT RELEASED TO"
            original$(max%) = " THE JOB  "
            return

L18750
*        'BW' Tag, Buy Work (Purchase Job) Advice
            descrmsg$(max%) = "PLANNED PURCHASE JOB ADVICE"
            original$(max%) = " - NOT REL"
            return

L18850
*        'RW' Tag, Buy Work (Purchase Job) Directive
            descrmsg$(max%) = "PLNED PURCHASE JOB DIRECTVE"
            original$(max%) = " - NOT REL"
            return

L18950
*        'PO' or 'QC' Tag, Purchase Order or Quality Control
            call "REDALT0" (#11, str(pipouttg$(max%),3%,14%) & "  ",     ~
                                                            1%, f1%(11%))
                if f1%(11%) = 0% then return             /* VBKMASTR */
            get #11 using L19000, custcode$
L19000:         FMT CH(9)
            call "READ100" (#12,                                         ~
                            str(custcode$,1%,9%) &                       ~
                            str(pipouttg$(max%),3%,14%) &                ~
                            "  " & str(pipouttg$(max%),17%,3%), f1%(12%))
                if f1%(12%) = 0% then return
            get #12 using L19070, original, shipped, bckorder, unitprice, ~
                                 jobno$        /* VBKLINES or VBKBUF2 */
L19070:         FMT XX(92), 3*PD(14,4), PD(14,7), XX(41), CH(8)
            if jobno$ = " " then L19200
            if str(pipouttg$(max%),,2%) = "PO" then                      ~
                          descrmsg$(max%) = "ON ORDER FOR JOB " & jobno$
            if str(pipouttg$(max%),,2%) = "QC" then                      ~
                          descrmsg$(max%) = "NOW IN QC FOR JOB " & jobno$
            goto L19200

L19200
*        Common Routine for ORIGINAL
            original = round(original,2)
            tottotal = tottotal + original
            call "CONVERT" (original, 2.2, original$(max%))
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1
                  on fieldnr% gosub L20160,         /* Part Range       */~
                                    L20280,         /* Date Range       */~
                                    L20210          /* Select By        */
                     return

L20160
*        Default/enable for PART NUMBER RANGE
            inpmessage$ = "Enter the beginning and ending Part #s, or "& ~
                "'ALL'"
            if beginpart$ = " " then beginpart$ = "ALL"
            return

L20210
*        Default/enable for Selections
            inpmessage$ = "Enter a 'Y' at all items to be included ('N'"&~
                " to exclude)"
            return

L20280
*        Default/enable for DATE RANGE
            inpmessage$ = "Enter the beginning and ending dates, or 'ALL'"
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
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%  /* PUT MSG AREA AT BOTTOM OF SCREEN  */
            call "STARTOVR" (keyhit1%)
                if keyhit1% = 1% then return

                return clear all
                goto inputmode

        REM *************************************************************~
            *                                                           *~
            *           F O R M A T    S T A T E M E N T S              *~
            *                                                           *~
            *************************************************************

L35060: FMT                 /* FILE: PIPOUT                            */~
            CH(19),         /* TAG NUMBER IN LEVEL 2 PLANNING          */~
            CH(25),         /* PART NUMBER                             */~
            BI(4),          /* DATE IN SUBSCRIPT FOR PIP               */~
            XX(8),                                                       ~
            PD(14,4)        /* QUANTITY OF SOMETHING IN PACKED DECIMAL */

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  init(hex(8c)) rptfac$()
                  on fieldnr% gosub L40170,         /* Part Number Range*/~
                                    L40170,         /* Date Range       */~
                                    L40170          /* Select For       */
                     goto L40240

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40240:     accept                                                       ~
               at (01,02),                                               ~
                  "ANALYSIS OF SCHEDULED PART USAGE & SHIPMENTS",        ~
               at (01,66), "Today: ",                                    ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), "RANGE:",                                     ~
               at (05,28), fac(hex(ac)), col_header$            , ch(51),~
               at (06,05), "Part Number Range",                          ~
               at (06,28), fac(lfac$( 1)), beginpart$           , ch(25),~
               at (06,54), fac(lfac$( 1)), endpart$             , ch(25),~
               at (07,05), "Date Range",                                 ~
               at (07,28), fac(lfac$( 2)), begdate$             , ch(08),~
               at (07,54), fac(lfac$( 2)), enddate$             , ch(08),~
               at (09,02), "INCLUDE:",                                   ~
               at (10,05), "Shipments to be made",                       ~
               at (10,68), fac(lfac$( 3)), so$                  , ch(01),~
               at (11,05),                                               ~
                  "Parts committed to jobs/proj but not yet kitted",     ~
               at (11,68), fac(lfac$( 3)), jo$                  , ch(01),~
               at (12,05),                                               ~
                  "Parts ordered for jobs/proj but not yet received",    ~
               at (12,68), fac(lfac$( 3)), po$                  , ch(01),~
               at (13,05),                                               ~
                  "Parts to be kitted for jobs planned but not yet releas~
        ~ed",                                                             ~
               at (13,68), fac(lfac$( 3)), wo$                  , ch(01),~
               at (14,05),                                               ~
                  "Parts to be kitted for Purchase Jobs planned but not r~
        ~eleased",                                                        ~
               at (14,68), fac(lfac$( 3)), bw$                  , ch(01),~
               at (15,05),                                               ~
                  "Parts to be kitted for Purchase Jobs when PO created",~
               at (15,68), fac(lfac$( 3)), rw$                  , ch(01),~
               at (17,02), "If organizing by date, how do you want to gro~
        ~up?",                                                            ~
               at (18,02), fac(rptfac$( 1)), dd$                , ch(01),~
               at (18,04), "By Day",                                     ~
               at (18,30), fac(rptfac$( 2)), ww$                , ch(01),~
               at (18,32), "By Week",                                    ~
               at (18,60), fac(rptfac$( 3)), mm$                , ch(01),~
               at (18,62), "By Month",                                   ~
               at (19,02), fac(rptfac$( 4)), cq$                , ch(01),~
               at (19,04), "By Calendar Quarter",                        ~
               at (19,42), fac(rptfac$( 5)), fq$                , ch(01),~
               at (19,44), "By Fiscal Quarter",                          ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                     keys(hex(00010d0f10)), key(keyhit%)

               if keyhit% <> 13 then L40830
                  call "MANUAL" ("PIPOASUB")
                  goto L40240

L40830:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40240

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  gosub set_pfkey
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41210,         /* Part Range       */~
                                    L41210,         /* Date Range       */~
                                    L41210          /* Types Selection  */
                  goto L41280

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41210:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41280:     accept                                                       ~
               at (01,02),                                               ~
                  "ANALYSIS OF SCHEDULED PART USAGE & SHIPMENTS",        ~
               at (01,66), "Today: ",                                    ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,28), fac(hex(ac)), col_header$            , ch(51),~
               at (06,05), "Part Number Range",                          ~
               at (06,28), fac(lfac$( 1)), beginpart$           , ch(25),~
               at (06,54), fac(lfac$( 1)), endpart$             , ch(25),~
               at (07,05), "Date Range",                                 ~
               at (07,28), fac(lfac$( 2)), begdate$             , ch(08),~
               at (07,54), fac(lfac$( 2)), enddate$             , ch(08),~
               at (09,02), "INCLUDE:",                                   ~
               at (10,05), "Shipments to be made",                       ~
               at (10,68), fac(lfac$( 3)), so$                  , ch(01),~
               at (11,05),                                               ~
                  "Parts committed to jobs/proj but not yet kitted",     ~
               at (11,68), fac(lfac$( 3)), jo$                  , ch(01),~
               at (12,05),                                               ~
                  "Parts ordered for jobs/proj but not yet received",    ~
               at (12,68), fac(lfac$( 3)), po$                  , ch(01),~
               at (13,05),                                               ~
                  "Parts to be kitted for jobs planned but not yet releas~
        ~ed",                                                             ~
               at (13,68), fac(lfac$( 3)), wo$                  , ch(01),~
               at (14,05),                                               ~
                  "Parts to be kitted for Purchase Jobs planned but not r~
        ~eleased",                                                        ~
               at (14,68), fac(lfac$( 3)), bw$                  , ch(01),~
               at (15,05),                                               ~
                  "Parts to be kitted for Purchase Jobs when PO created",~
               at (15,68), fac(lfac$( 3)), rw$                  , ch(01),~
               at (17,02), "If organizing by date, how do you want to gro~
        ~up?",                                                            ~
               at (17,02), "If organizing by date, how do you want to gro~
        ~up?",                                                            ~
               at (18,02), fac(rptfac$( 1)), dd$                , ch(01),~
               at (18,04), "By Day",                                     ~
               at (18,30), fac(rptfac$( 2)), ww$                , ch(01),~
               at (18,32), "By Week",                                    ~
               at (18,60), fac(rptfac$( 3)), mm$                , ch(01),~
               at (18,62), "BY Month",                                   ~
               at (19,02), fac(rptfac$( 4)), cq$                , ch(01),~
               at (19,04), "By Calendar Quarter",                        ~
               at (19,42), fac(rptfac$( 5)), fq$                , ch(01),~
               at (19,44), "By Fiscal Quarter",                          ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,23), fac(hex(8c)),   pf2edt$              , ch(40),~
               at (23,23), fac(hex(8c)),   pf11edt$             , ch(40),~
               at (24,23), fac(hex(8c)),   pf12edt$             , ch(40),~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)),   pf16edt$             , ch(16),~
                     keys(pfkeys$), key (keyhit%)

               if keyhit% <> 13 then L41900
                  call "MANUAL" ("PIPOASUB")
                  goto L41280

L41900:        if keyhit% <> 15 then L41940
                  call "PRNTSCRN"
                  goto L41280

L41940:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                     u3% = u3%
               return

        set_pfkey
            pfkeys$ = hex(0001020b0c0d0f10)
            if pf2edt$ = " " then  pfkeys$ = hex(0001ffffff0d0fff)
            return

L42000: REM *************************************************************~
            *                                                           *~
            *    DISPLAY  SCREEN  FOR  PARTS  SELECTED                  *~
            *                                                           *~
            *************************************************************
            accept   at(01,02), fac(hex(8c)), header$           , ch(79),~
               at (01,66), "Today: ",                                    ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
                at (03,02), "Part: ",                                    ~
                at (03,10), fac(hex(84)), part$, ch(25),                 ~
                at (03,38), fac(hex(84)), partdescr$, ch(34),            ~
                at (04,02), fac(hex(ac)), header1$, ch(79),              ~
                at (05,02), fac(hex(84)), duedate$ (line%+ 1)   , ch(08),~
                at (06,02), fac(hex(84)), duedate$ (line%+ 2)   , ch(08),~
                at (07,02), fac(hex(84)), duedate$ (line%+ 3)   , ch(08),~
                at (08,02), fac(hex(84)), duedate$ (line%+ 4)   , ch(08),~
                at (09,02), fac(hex(84)), duedate$ (line%+ 5)   , ch(08),~
                at (10,02), fac(hex(84)), duedate$ (line%+ 6)   , ch(08),~
                at (11,02), fac(hex(84)), duedate$ (line%+ 7)   , ch(08),~
                at (12,02), fac(hex(84)), duedate$ (line%+ 8)   , ch(08),~
                at (13,02), fac(hex(84)), duedate$ (line%+ 9)   , ch(08),~
                at (14,02), fac(hex(84)), duedate$ (line%+10)   , ch(08),~
                at (15,02), fac(hex(84)), duedate$ (line%+11)   , ch(08),~
                at (16,02), fac(hex(84)), duedate$ (line%+12)   , ch(08),~
                at (17,02), fac(hex(84)), duedate$ (line%+13)   , ch(08),~
                at (18,02), fac(hex(84)), duedate$ (line%+14)   , ch(08),~
                at (19,02), fac(hex(84)), duedate$ (line%+15)   , ch(08),~
                                                                         ~
                at (05,13), fac(hex(84)), pipouttg$(line%+ 1)   , ch(19),~
                at (06,13), fac(hex(84)), pipouttg$(line%+ 2)   , ch(19),~
                at (07,13), fac(hex(84)), pipouttg$(line%+ 3)   , ch(19),~
                at (08,13), fac(hex(84)), pipouttg$(line%+ 4)   , ch(19),~
                at (09,13), fac(hex(84)), pipouttg$(line%+ 5)   , ch(19),~
                at (10,13), fac(hex(84)), pipouttg$(line%+ 6)   , ch(19),~
                at (11,13), fac(hex(84)), pipouttg$(line%+ 7)   , ch(19),~
                at (12,13), fac(hex(84)), pipouttg$(line%+ 8)   , ch(19),~
                at (13,13), fac(hex(84)), pipouttg$(line%+ 9)   , ch(19),~
                at (14,13), fac(hex(84)), pipouttg$(line%+10)   , ch(19),~
                at (15,13), fac(hex(84)), pipouttg$(line%+11)   , ch(19),~
                at (16,13), fac(hex(84)), pipouttg$(line%+12)   , ch(19),~
                at (17,13), fac(hex(84)), pipouttg$(line%+13)   , ch(19),~
                at (18,13), fac(hex(84)), pipouttg$(line%+14)   , ch(19),~
                at (19,13), fac(hex(84)), pipouttg$(line%+15)   , ch(19),~
                                                                         ~
                at (05,33), fac(hex(84)), descrmsg$(line%+ 1)   , ch(27),~
                at (06,33), fac(hex(84)), descrmsg$(line%+ 2)   , ch(27),~
                at (07,33), fac(hex(84)), descrmsg$(line%+ 3)   , ch(27),~
                at (08,33), fac(hex(84)), descrmsg$(line%+ 4)   , ch(27),~
                at (09,33), fac(hex(84)), descrmsg$(line%+ 5)   , ch(27),~
                at (10,33), fac(hex(84)), descrmsg$(line%+ 6)   , ch(27),~
                at (11,33), fac(hex(84)), descrmsg$(line%+ 7)   , ch(27),~
                at (12,33), fac(hex(84)), descrmsg$(line%+ 8)   , ch(27),~
                at (13,33), fac(hex(84)), descrmsg$(line%+ 9)   , ch(27),~
                at (14,33), fac(hex(84)), descrmsg$(line%+10)   , ch(27),~
                at (15,33), fac(hex(84)), descrmsg$(line%+11)   , ch(27),~
                at (16,33), fac(hex(84)), descrmsg$(line%+12)   , ch(27),~
                at (17,33), fac(hex(84)), descrmsg$(line%+13)   , ch(27),~
                at (18,33), fac(hex(84)), descrmsg$(line%+14)   , ch(27),~
                at (19,33), fac(hex(84)), descrmsg$(line%+15)   , ch(27),~
                                                                         ~
                at (05,60), fac(hex(84)), original$(line%+ 1)   , ch(10),~
                at (06,60), fac(hex(84)), original$(line%+ 2)   , ch(10),~
                at (07,60), fac(hex(84)), original$(line%+ 3)   , ch(10),~
                at (08,60), fac(hex(84)), original$(line%+ 4)   , ch(10),~
                at (09,60), fac(hex(84)), original$(line%+ 5)   , ch(10),~
                at (10,60), fac(hex(84)), original$(line%+ 6)   , ch(10),~
                at (11,60), fac(hex(84)), original$(line%+ 7)   , ch(10),~
                at (12,60), fac(hex(84)), original$(line%+ 8)   , ch(10),~
                at (13,60), fac(hex(84)), original$(line%+ 9)   , ch(10),~
                at (14,60), fac(hex(84)), original$(line%+10)   , ch(10),~
                at (15,60), fac(hex(84)), original$(line%+11)   , ch(10),~
                at (16,60), fac(hex(84)), original$(line%+12)   , ch(10),~
                at (17,60), fac(hex(84)), original$(line%+13)   , ch(10),~
                at (18,60), fac(hex(84)), original$(line%+14)   , ch(10),~
                at (19,60), fac(hex(84)), original$(line%+15)   , ch(10),~
                                                                         ~
                at (05,71), fac(hex(84)), bkorder$ (line%+ 1)   , ch(10),~
                at (06,71), fac(hex(84)), bkorder$ (line%+ 2)   , ch(10),~
                at (07,71), fac(hex(84)), bkorder$ (line%+ 3)   , ch(10),~
                at (08,71), fac(hex(84)), bkorder$ (line%+ 4)   , ch(10),~
                at (09,71), fac(hex(84)), bkorder$ (line%+ 5)   , ch(10),~
                at (10,71), fac(hex(84)), bkorder$ (line%+ 6)   , ch(10),~
                at (11,71), fac(hex(84)), bkorder$ (line%+ 7)   , ch(10),~
                at (12,71), fac(hex(84)), bkorder$ (line%+ 8)   , ch(10),~
                at (13,71), fac(hex(84)), bkorder$ (line%+ 9)   , ch(10),~
                at (14,71), fac(hex(84)), bkorder$ (line%+10)   , ch(10),~
                at (15,71), fac(hex(84)), bkorder$ (line%+11)   , ch(10),~
                at (16,71), fac(hex(84)), bkorder$ (line%+12)   , ch(10),~
                at (17,71), fac(hex(84)), bkorder$ (line%+13)   , ch(10),~
                at (18,71), fac(hex(84)), bkorder$ (line%+14)   , ch(10),~
                at (19,71), fac(hex(84)), bkorder$ (line%+15)   , ch(10),~
                                                                         ~
                at (21,02), fac(hex(ac)), blankline$            , ch(79),~
                at(22,02), "(RETURN)Next Part                            ~
        ~                (13)Instructions",                               ~
                at(23,02), "(1)Start Over  (2)First   (4)Prev  (6)Down   ~
        ~                (15)Print Screen",                               ~
                at(24,02), "               (3)Last    (5)Next  (7)Up     ~
        ~                (16)Return",                                     ~
                     keys(hex(00010203040506070d0f10)), key(keyhit%)

               if keyhit% <> 13 then L43070
                  call "MANUAL" ("PIPOASUB")
                  goto L42000

L43070:     if keyhit%<>15 then return
                call "PRNTSCRN"
                goto L42000
        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50130,         /* Part Range       */~
                                    L50400,         /* Date Range       */~
                                    L50270          /* Selections       */
                  return

L50130
*        Test data for Part Number Range
            if beginpart$ <> "ALL" then L50170
                partdescr$, endpart$, partdescre$ = " "
                return
L50170:     call "DESCRIBE" (#8, beginpart$, partdescr$, 1%, f1%(8))

            if endpart$ = " " then endpart$ = beginpart$
            if endpart$ >= beginpart$ then L50240
                errormsg$ = "Ending Part must be equal or greater than" &~
                            " Beginning Part"
                return
L50240:     call "DESCRIBE" (#8, endpart$, partdescre$, 1%, f1%(8))
            return

L50270
*        Test data for WHICH PIPS TO SELECT
            if so$ <> "Y" and so$ <> "N" then L50370
            if jo$ <> "Y" and jo$ <> "N" then L50370
            if po$ <> "Y" and po$ <> "N" then L50370
            if wo$ <> "Y" and wo$ <> "N" then L50370
            if bw$ <> "Y" and bw$ <> "N" then L50370
            if rw$ <> "Y" and rw$ <> "N" then L50370

            include$(1%) = so$
            include$(2%) = jo$
            include$(3%) = wo$
            include$(4%) = bw$
            include$(5%) = rw$
            include$(6%) = po$
            include$(7%) = po$

            if pos((so$ & jo$ & po$ & wo$ & bw$ & rw$) = "Y") > 0% then  ~
                                                                   return
                errormsg$ = "Please Select at least one..."
                return

L50370:         errormsg$ = "Enter either 'Y' -or- 'N'."
                return

L50400
*        Test data for DATE RANGE
            tdate$ = date$
            call "DATUNFMT" (tdate$)
            search str(yymmdd$()) = str(tdate$,,6) to tdate%() step 6
            if tdate%(1) = 0% then L50470 else                            ~
                                        tdate%(1) = 1% +int(tdate%(1)/6%)

L50470:     if begdate$ <> "ALL" then L50490
                begdate$ = yymmdd$(1) : enddate$ = yymmdd$(490)
L50490:     if begdate$  = " " or begdate$ = blankdate$ then ~
                begdate$ = yymmdd$(1)
            call "DATEOK" (begdate$, bdate%(1), errormsg$)
            if errormsg$ <> " " then return
            bdate%(1) = 0%
            temp$ = begdate$ : call "DATUNFMT" (temp$)
            search str(yymmdd$()) = str(temp$,,6) to bdate%() step 6
            if bdate%(1) = 0% then bdate%(1) = 1%                        ~
                              else bdate%(1) = 1% + int(bdate%(1)/6%)

            if enddate$ = " " or enddate$ = "LAST" or ~
               enddate$ = blankdate$ then enddate$ = yymmdd$(490)
L50600:     call "DATEOK" (enddate$, edate%(1), errormsg$)
            if errormsg$ <> " " then return
            edate%(1) = 0%
            temp$ = enddate$  :  call "DATUNFMT" (temp$)
            search str(yymmdd$()) = str(temp$,,6) to edate%() step 6
            if edate%(1) > 0% then L50680
                enddate$ = yymmdd$(490)
                goto L50600
L50680:     edate%(1) = 1% + int(edate%(1)/6%)
            if edate%(1) < bdate%(1) then                                ~
                errormsg$ = "Ending Date must be on or after Beginning" &~
                            " Date."
            return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN







            call "SETPRNT" ("PLN001", " ", 0%, 1%)

            end
