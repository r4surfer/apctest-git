        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  JJJJJ  BBBB    CCC   TTTTT  L      RRRR   PPPP   TTTTT   *~
            *    J    B   B  C   C    T    L      R   R  P   P    T     *~
            *    J    BBBB   C        T    L      RRRR   PPPP     T     *~
            *  J J    B   B  C   C    T    L      R   R  P        T     *~
            *   J     BBBB    CCC     T    LLLLL  R   R  P        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBCTLRPT - This report will list all Jobs,                *~
            *            PO lines, and Inventory lots linked by a       *~
            *            particular control number and will show the    *~
            *            actual and standard costs for each.            *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1989  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/14/89 ! Original                                 ! SID *~
            * 02/11/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 11/10/94 ! Bug fix.  Arg missing in call to describe! JDH *~
            *          !   and problem with blank control number. !     *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            cntrlnum$19,         /* Control Number From User Input     */~
            company$80,          /* Company Name                       */~
            controlnbr$19,       /* Control Number From JBMASTR2       */~
            cursor%(2),          /* Cursor location for edit           */~
            date$8,              /* Date for screen display            */~
            descr_m(8),          /* Descr Map For PlowCode             */~
            edtmessage$79,       /* Edit screen message                */~
            errormsg$79,         /* Error message                      */~
            header1$132,         /*                                    */~
            header2$132,         /*                                    */~
            header$(3)79,        /* For PLOWCODE Call                  */~
            i$(24)80,            /* Screen Image                       */~
            inc(2),              /* Plowcode Arguments of some         */~
            inc$(2)8,            /* Plowcode Arguments of some         */~
            inpmessage$79,       /* Informational Message              */~
            invheader1$132,      /* Page Headers for Inventory Section */~
            jbheader1$132,       /* Page Headers for Job Section       */~
            jbheader2$132,       /* Page Headers for Job Section       */~
            jobnum$9,            /* Job number                         */~
            jobdescr$32,         /* Job Description                    */~
            lfac$(20)1,          /* Field Attribute Characters         */~
            line2$79,            /* Screen Line #2                     */~
            line$3,              /* Line Item Number                   */~
            orderdate$8,         /* Actual order date                  */~
            partdescr$32,        /* Part description                   */~
            partnum$25,          /* Part number                        */~
            pfkeys$32,           /* PF Key Hex Values                  */~
            plowkey$99,          /* Miscellaneous Read/Plow Key        */~
            poheader1$132,       /* Page Headers for PO Section        */~
            poheader2$132,       /* Page Headers for PO Section        */~
            ponum$16,            /* PO number                          */~
            prnt1$(300)135,      /* Array to Sort                      */~
            prnt2$(300)132,      /* Array to Sort Detail               */~
            prnt_id$16,          /* Report Program ID                  */~
            println1$132,        /*                                    */~
            println2$132,        /*                                    */~
            requisitioner$20,    /* Requisitioner Field From VBKLINES  */~
            rpttitle$41,         /* Report Title                       */~
            sortseq$1,           /* Sorting sequence option            */~
            startdate$8,         /* Actual start date                  */~
            store$3,             /* Store number                       */~
            userid$3             /* Current User Id                    */

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
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
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
            * #01 ! JBMASTR2 ! Production job master file               *~
            * #02 ! VBKLINES ! Purchase Order Line Items File           *~
            * #03 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * #04 ! VBKMASTR ! Purchase Order Master File               *~
            * #05 ! HNYMASTR ! Inventory Part Master File               *~
            * #06 ! SYSFILE2 ! Caelus Management System Information     *~
            * #55 ! DUMMY    ! Dummy Channel for PLOWCODE               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "JBMASTR2",                                      ~
                        varc,     indexed,  recsize = 1300,              ~
                        keypos =    1, keylen =   8,                     ~
                        alt key  1, keypos = 1120, keylen =  19, dup,    ~
                            key  2, keypos =   58, keylen =  25, dup     ~

            select #02, "VBKLINES",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  28

            select #03, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #04, "VBKMASTR",                                      ~
                        varc,     indexed,  recsize =  1030,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos = 10, keylen = 16             ~


            select #05, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen = 25,                      ~
                        alt key 1, keypos = 102, keylen = 9,             ~
                            key 2, keypos = 90,  keylen = 4,             ~
                            key 3, keypos = 26,  keylen = 32

            select #06, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =    1, keylen = 20

            select #55, "DUMMY",                                         ~
                        varc,     indexed,  recsize  =  5,               ~
                        keypos =  1,  keylen =  5

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))
            call "OPENCHCK" (#05, fs%(05), f2%(05), 0%, rslt$(05))
            call "OPENCHCK" (#06, fs%(06), f2%(06), 0%, rslt$(06))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            ret% = 0%
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, ret%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            pf4$ = "(4)Previous Field" : pf16$ = "(16)Exit"
            prnt_id$ = "JBCTLRPT: JB0009"
            str(line2$,62) = "JBCTLRPT: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            for fieldnr% = 1% to  2%
                if fieldnr% = 1% then pf4$ = " " else pf4$ = "(4)Previous"
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
                         errormsg$ = " " : pf4$ = " "
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
            pf4$ = " " : pf16$ = "(16)Print Report"
            lastfieldnr% = 0%
            inpmessage$ = edtmessage$
            gosub'101(0%)               /* Display Screen - No Entry  */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       generate_report
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  2% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Control number         */~
                              L20200          /* Sort sequence          */
            return

L20100: REM Def/Enable Control number    CNTRLNUM$
            inpmessage$ = "Enter a Control Number or ? to select an "    ~
                        & "existing number."
            return

L20200: REM Def/Enable Part number       SORTSEQ$
            inpmessage$ = "Enter a report sorting sequence "
            return

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      cntrlnum$, sortseq$
            pf16$ = "(16)Exit"
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1989  an unpublished work by CAELUS,       *~
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

        generate_report
            select printer(134)
            call "SHOSTAT" ("Report Generation In Progress")
            call "SETPRNT" ("JB0009", " ", 0%, 0%)
            time$ = " " : call "TIME" (time$)
            pagecntr% = -1%
            gosub set_headers
            gosub print_params

        REM Start Processing at Job Level    ** JBMASTR2 **
            init(" ") prnt1$(), prnt2$()
            section% = 1% : linecntr% = 99% : k% = 0%
            rpttitle$ = "JOBS FOR CONTROL # " & cntrlnum$
            call "STRING" addr("CT", rpttitle$, 41%, rpttitle$)
            if linecntr% > 55% then gosub print_heading
            if sortseq$ = "1" then keylength% =  9%
            if sortseq$ = "2" then keylength% = 25%
            if sortseq$ = "3" then keylength% =  6%
            plowkey$ = str(cntrlnum$, 1, 19) & hex(00)
            str(plowkey$, 1,19) = addc all(hex(ff))
            call "PLOWALTS" (#01, plowkey$, 1%, 0%, f1%(01))
L29780:       if f1%(01) = 1% then L29790
                 if k% <> 0% then gosub sort_arrary
                 print skip(1)
                 print "                                                 ~
        ~      ***** END OF JOBS REPORT *****"
                 goto L30320
L29790:          get #01 using L29820, jobnum$, jobdescr$, partnum$,      ~
                         qtytobld, qtycomp, startdate$, qtyscr, qtyrwk,  ~
                         totalactcst, controlnbr$, perccomp
L29820:             FMT  CH(8), CH(30), POS(58), CH(25), PD(14,4),       ~
                         PD(14,4), POS(147), CH(6), POS(212), PD(14,4),  ~
                         PD(14,4), POS(232), PD(14,4), POS(1120), CH(19),~
                         PD(14,4)

              if cntrlnum$ <> controlnbr$ then L30292

              k% = k% + 1%

              call "STCCOSTS" (partnum$, " ", #06, 1%, unitstdcst)
              call "DATEFMT" (startdate$)

              qtyrminjob     =  qtytobld  - (qtycomp  + qtyscr  + qtyrwk)
              extstdcst      =  unitstdcst * qtytobld
              variance       =  extstdcst - totalactcst
              if qtytobld <> 0 then unitactcst = totalactcst / qtytobld
              if perccomp = 0 then L29980
                 estcstcomp = (unitactcst / (perccomp / 100))
                 estvarcomp =  estcstcomp - unitstdcst
                 if extstdcst  <> 0  then                                ~
                   estvarperccomp = (1 - (estcstcomp / unitstdcst)) * 100
                 estvarperccomp = estvarperccomp * (-1)

L29980:    if sortseq$ <> "1" then L30020
            str(prnt1$(k%),  1,  9) = jobnum$
            str(prnt1$(k%), 13, 32) = jobdescr$
            str(prnt1$(k%), 45, 25) = partnum$
            str(prnt1$(k%), 74,  8) = startdate$ : goto L30110

L30020:    if sortseq$ <> "2" then L30060
            str(prnt1$(k%),  1, 25) = partnum$
            str(prnt1$(k%), 27,  9) = jobnum$
            str(prnt1$(k%), 39, 32) = jobdescr$
            str(prnt1$(k%), 74,  8) = startdate$ : goto L30110

        REM When SORTSEQ$ = "3"
L30060:     str(prnt1$(k%),  1,  8) = startdate$
            str(prnt1$(k%), 13,  9) = jobnum$
            str(prnt1$(k%), 31, 32) = jobdescr$
            str(prnt1$(k%), 65, 25) = partnum$

L30110:     convert qtytobld   to str(prnt1$(k%), 86,10), pic(#######.##)
            convert qtycomp    to str(prnt1$(k%), 99,10), pic(#######.##)
            convert qtyrminjob to str(prnt1$(k%),120,10), pic(#######.##)

            convert unitstdcst  to str(prnt2$(k%), 7,10), pic(#######.##)
            convert extstdcst   to str(prnt2$(k%),23,10), pic(#######.##)
            convert unitactcst  to str(prnt2$(k%),43,10), pic(#######.##)

            convert variance    to str(prnt2$(k%),57,10), pic(-######.##)
            convert perccomp    to str(prnt2$(k%),67, 8), pic(#####.##)
            convert estcstcomp  to str(prnt2$(k%),85,10), pic(-######.##)
            convert estvarcomp to str(prnt2$(k%),103,10), pic(-######.##)
            convert estvarperccomp to str(prnt2$(k%),123,10),            ~
                                                         pic(-######.##)
            convert k% to str(prnt1$(k%),133,3), pic(###) /* Pointer */

            qtytobld, qtycomp, qtyrminjob, unitstdcst, extstdcst,        ~
            unitactcst, variance, perccomp, estcstcomp, estvarcomp,      ~
            estvarperccomp, totalactcst = 0

            init(" ") jobnum$, jobdescr$, partnum$, startdate$,          ~
                      controlnbr$
L30292:     call "READNEXT" (#01, f1%(01))
            goto L29780

L30320: REM Start Processing at PO Level    ** VBKLINES **
            init(" ") prnt1$(), prnt2$()
            section% = 2% : linecntr% = 99% : pagecntr%, k% = 0%
            rpttitle$ = "PURCHASE ORDERS FOR CONTROL # " & cntrlnum$
            call "STRING" addr("CT", rpttitle$, 41%, rpttitle$)
            if linecntr% > 55% then gosub print_heading
            if sortseq$ = "1" then keylength% = 16%
            if sortseq$ = "2" then keylength% = 25%
            plowkey$ = str(cntrlnum$, 1, 19) addc all(hex(ff))
            str(plowkey$, 1, 19) = addc all(hex(ff))
            call "PLOWALTS" (#02, plowkey$, 1%, 0%, f1%(02))
L30350:       if f1%(02) = 1% then L30360
              if k% <> 0% then gosub sort_arrary
              print skip(1)
              print "                                                    ~
        ~   ***** END OF P.O. REPORT *****"
              goto L30750
L30360:       get #02 using L30380, ponum$, line$, partnum$, partdescr$,  ~
                                  orderqty, qtyrecv, qtyopen, poprice,   ~
                                  requisitioner$
L30380:                  FMT    POS(10), CH(16), CH(3), POS(32), CH(25), ~
                                CH(32), POS(93), PD(14,4), PD(14,4),     ~
                                PD(14,4), PD(14,4), POS(333), CH(20)

              if str(cntrlnum$, 1, 19) <> str(requisitioner$, 1, 19)     ~
                               then L30731

              k% = k% + 1%

              call "STCCOSTS" (partnum$, " ", #06, 1%, unitstdcst)

              plowpo$ = ponum$
              call "REDALT0" (#04, plowpo$, 1%, f1%(04))
              if f1%(04) = 0% then L30490
              get #04 using L30470, orderdate$
L30470:                    FMT  POS(451), CH(6)

L30490:       pounitprice = poprice / 1000           /* PD(14,7) */
              variance =  unitstdcst - pounitprice
              call "DATEFMT" (orderdate$)

            if sortseq$ = "1" or sortseq$ = "3" then L30540 else L30580
L30540:       str(prnt1$(k%),  1, 16) = ponum$
              str(prnt1$(k%), 23,  3) = line$
              str(prnt1$(k%), 30, 25) = partnum$
              str(prnt1$(k%), 59, 32) = partdescr$
              str(prnt1$(k%), 99, 32) = orderdate$ : goto L30660

L30580: REM Sorting Sequence is by Part Number
              str(prnt1$(k%),  1, 25) = partnum$
              str(prnt1$(k%), 27, 32) = partdescr$
              str(prnt1$(k%), 53, 16) = ponum$
              str(prnt1$(k%), 76,  3) = line$
              str(prnt1$(k%), 99, 32) = orderdate$

L30660:    convert orderqty    to str(prnt2$(k%),11, 10), pic(#######.##)
           convert qtyrecv     to str(prnt2$(k%),36, 10), pic(#######.##)
           convert qtyopen     to str(prnt2$(k%),57, 10), pic(#######.##)
           convert unitstdcst  to str(prnt2$(k%),82, 10), pic(#######.##)
           convert pounitprice to str(prnt2$(k%),104,10), pic(#######.##)
           convert variance    to str(prnt2$(k%),123,10), pic(-######.##)

           convert k% to str(prnt1$(k%),133,3), pic(###) /* Pointer */

           orderqty, qtyrecv, qtyopen, unitstdcst, pounitprice,          ~
           variance = 0

           init(" ") ponum$, line$, partnum$, partdescr$, orderdate$

L30731:    call "READNEXT" (#02, f1%(02))
           goto L30350     /* Get Next Record */

L30750: REM Start Processing at Inventory Level  ** HNYQUAN **
            init(" ") prnt1$(), prnt2$()
            section% = 3% : linecntr% = 99% : pagecntr%, k% = 0%
            rpttitle$ = "INVENTORY FOR CONTROL # " & cntrlnum$
            call "STRING" addr("CT", rpttitle$, 41%, rpttitle$)
            if linecntr% > 55% then gosub print_heading
            plowkey$ = str(cntrlnum$, 1, 6) & hex(00)
            str(plowkey$, 1, 6) = addc all(hex(ff))
            call "PLOWALTS" (#03, plowkey$, 1%, 0%, f1%(03))
L30780:       if f1%(03) = 1% then L30790
                 if k% <> 0% then gosub print_line
                 print skip (1)
                 print "                                                 ~
        ~      ***** END OF INVENTORY REPORT *****"
                 print "                                                 ~
        ~           ***** END OF REPORT *****"
                 close printer
                 goto inputmode
L30790:       get #3 using L30800, partnum$, store$, lotnbr$, qtyonhand,  ~
                                  unitactcst
L30800:                FMT POS(17), CH(25), CH(3), CH(16), POS(69),      ~
                           PD(14,4), POS(117), PD(14,4)

              if str(cntrlnum$, 1, 6) <>  str(lotnbr$, 1, 6) then L30971

              k% = k% + 1%

              call "STCCOSTS" (partnum$, " ", #06, 1%, unitstdcst)

              unitvar  = unitstdcst - unitactcst
              totalvar = unitvar * qtyonhand

           str(prnt1$(k%),  1,  25) = partnum$
           str(prnt1$(k%), 31,  16) = lotnbr$
           str(prnt1$(k%), 54,   3) = store$
           convert qtyonhand  to str(prnt1$(k%), 63, 10), pic(#######.##)
           convert unitstdcst to str(prnt1$(k%), 80, 10), pic(#######.##)
           convert unitactcst to str(prnt1$(k%), 95, 10), pic(#######.##)
           convert unitvar    to str(prnt1$(k%),108, 10), pic(-######.##)
           convert totalvar   to str(prnt1$(k%),123, 10), pic(-######.##)

           call "DESCRIBE" (#05, partnum$, partdescr$, 0%, f1%(5%))
           str(prnt2$(k%),  6,  32) = partdescr$

           qtyonhand, unitstdcst, unitactcst, unitvar, totalvar = 0
           init(" ") partnum$, partdescr$, store$, lotnbr$

L30971:    call "READNEXT" (#03, f1%(03))
           goto L30780

        set_headers
            jbheader2$ = "   UNIT STD COST    EXTENDED STD    UNIT ACTU" ~
              & "AL COST      VARIANCE  % COMP    EST COST AT COMP   "   ~
              & "EST VAR AT COMP   EST VAR % AT COMP"
            poheader2$ = "      ORDER QUANTITY        QUANTITY RECEIVED" ~
              & "    QUANTITY ON ORDER       UNIT STANDARD COST      "   ~
              & "      UNIT PRICE           VARIANCE"
            invheader1$ = "PART/DESCRIPTION              LOT NUMBER   "  ~
              & "        STORE     QTY ON-HAND     UNIT STD CST  UNIT "  ~
              & "ACT COST     UNIT VAR      TOTAL VAR"

            jbheader1$ = "JOB NUMBER  JOB DESCRIPTION                 "  ~
                       & "PART NUMBER                START DATE    QTY"  ~
                       & " TO BLD     QTY COMP    QTY REMAIN TO BLD"
            poheader1$ = "PO NUMBER            LINE    PART NUMBER    "  ~
                       & "              PART DESCRIPTION              "  ~
                       & "        ORDER DATE"
            if sortseq$ <> "2" then L36206
            str(jbheader1$,1,66) = "PART NUMBER               JOB NUMB"  ~
                                 & "ER  JOB DESCRIPTION              "
            str(poheader1$,1,79) = "PART NUMBER               DESCRIPT"  ~
                                 & "ION               PO NUMBER       "  ~
                                 & "      LINE"
            return
L36206:     if sortseq$ <> "3" then return
            str(jbheader1$,1,81) = "START DATE  JOB NUMBER        JOB D" ~
                     &  "ESCRIPTION                   PART NUMBER      "
            return

        print_line
          for i% = 1% to k%
               if linecntr% > 55% then gosub print_heading
               str(println1$, 1, 132) = str(prnt1$(i%), 1, 132)
               print str(println1$, 1, 132)
               if section% <> 3% then L36340
                  p% = i% : goto L36350
L36340:        convert str(prnt1$(i%), 133, 3) to p%
L36350:        str(println2$, 1, 132) = str(prnt2$(p%), 1, 132)
               print str(println2$, 1, 132)
               init(" ") println1$, println2$
               linecntr% = linecntr% + 2%
          next i%
        return

        print_heading
            print page : pagecntr% = pagecntr% + 1%
            print using L65213, date$, time$, company$, prnt_id$
            print using L65223, rpttitle$, pagecntr%
            if section% <> 1% then L36620
               str(header1$, 1, 132) = jbheader1$
               str(header2$, 1, 132) = jbheader2$
               goto L36630
L36620:     if section% <> 2% then L36624
               str(header1$, 1, 132) = poheader1$
               str(header2$, 1, 132) = poheader2$
               goto L36630
L36624:     str(header1$, 1, 132) = invheader1$
            init(" ") str(header2$)
L36630:      print
             print using L65230, header1$
             if section% = 3% then L36660
             print using L65230, header2$
L36660:      print using L65270
             if section% = 3% then linecntr% = 5% else linecntr% = 6%
        return

        print_params
            print page
            pagecntr% = pagecntr% + 1%
            rpttitle$ = "Pegging Linkage Report"
            call "STRING" addr("CT", company$,  80%, company$)
            call "STRING" addr("CT", rpttitle$, 41%, rpttitle$)
            print using L65213, date$, time$, company$, prnt_id$
            print using L65223, rpttitle$, pagecntr%
            print skip (3)
            print using L65490  : print
            print skip (2)
            print using L65510, "CONTROL NUMBER", cntrlnum$
            print using L65521, "SORTING OPTIONS", sortseq$
            print using L65530, "1) JOB/PO NUMBER"
            print using L65532, "2) PART NUMBER "
            print using L65534, "3) START DATE "
            print skip (2)
            print using L65501
        return

        sort_arrary
          call "SORT" addr(prnt1$(), k%, 135%, prnt1$(), 1%, keylength%, ~
                           "A")
          gosub print_line
          return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              if fieldnr% = 1% then pfkeys$ = (hex(00010d0f10))          ~
                               else pfkeys$ = (hex(0001040d0f10))
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40080,         /* Control Number    */   ~
                                L40095          /* Sorting Sequence  */
              goto L40100

L40080:           lfac$(fieldnr%) = hex(81)  :  return  /* Numeric    */
L40095:           lfac$(fieldnr%) = hex(80)  :  return  /* Numeric    */

L40100: accept                                                           ~
               at (01,02),                                               ~
                  "Input Report Selection Criteria",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (07,02), "Control Number"                     ,        ~
               at (07,22), fac(lfac$( 1)), cntrlnum$            , ch(19),~
               at (08,02), "Sorting Options"                    ,        ~
               at (08,22), fac(lfac$( 2)), sortseq$             , ch(01),~
               at (09,22), "1) Job/PO Number  "                 ,        ~
               at (10,22), "2) Part Number "                    ,        ~
               at (11,22), "3) Start Date  "                    ,        ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), "(1)Start Over"                      ,        ~
               at (22,64), "(13)Instructions"                   ,        ~
               at (23,20), fac(hex(8c)), pf4$                   ,        ~
               at (23,64), "(15)Print Screen"                   ,        ~
               at (24,64), fac(hex(84)), pf16$                  ,        ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40245
                  call "MANUAL" ("JBCTLRPT") : goto L40100

L40245:        if keyhit% <> 15 then L40260
                  call "PRNTSCRN" : goto L40100

L40260:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50070,         /* Control Number         */~
                              L50210          /* Sorting Sequence       */
            return

L50070: REM Test for Control Number
            plowkey$ = str(cntrlnum$) & hex(00)
            if cntrlnum$ = " " then plowkey$ = "!" /* Start after blank */
            if cntrlnum$ = "?" then plowkey$ = "!" /* Start after blank */
            header$(1%) = "  Control Number"
            header$(3%) = hex(84) & "Select a Control Number"
            descr_m(1%) =  1120.19  : descr_m(2%) = 0001.0
            mat inc = zer    : init(" ") inc$()
            call "PLOWCODE" (#01, plowkey$, " ", -9019%, -1.00,          ~
                 f1%(1%), header$(), 0, 0, inc(), inc$(), "D", " ",      ~
                 #55, descr_m())
            if f1%(1%) <> 0 then L50180
               errormsg$="Nothing Found For Control Number: " & cntrlnum$
               return
L50180:     if plowkey$ <> " " and plowkey$ <> "!" then L50188
               errormsg$ = "Control Number Cannot be Blank"
               return
L50188:     cntrlnum$ = str(plowkey$,,19%)
            return

L50210: REM Test for Sorting Sequence
            if pos("123" = sortseq$) <> 0% then return
               errormsg$ = "Sorting Option Must be '1', '2' or '3'."
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
            *  Copyright (c) 1989  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end


L65213: %RUN ########   ########        #################################~
        ~##############################################      #############~
        ~###

L65223: %                                                  ##############~
        ~###########################                             PAGE:   #~
        ~###

L65230: %################################################################~
        ~#################################################################~
        ~###

L65270: %----------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~---

L65490: %                                -------------------------- Repor~
        ~t Selection Parameters --------------------------
L65501: %                                --------------------------------~
        ~-------------------------------------------------
L65510: %                                               #################~
        ~      ############
L65521: %                                               #################~
        ~      ############
L65530: %                                                                ~
        ~      ###########################
L65532: %                                                                ~
        ~      ###########################
L65534: %                                                                ~
        ~      ###########################
