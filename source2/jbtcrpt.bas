        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB   TTTTT   CCC   RRRR   PPPP   TTTTT          *~
            *    J    B   B    T    C   C  R   R  P   P    T            *~
            *    J    BBBB     T    C      RRRR   PPPP     T            *~
            *  J J    B   B    T    C   C  R   R  P        T            *~
            *   J     BBBB     T     CCC   R   R  P        T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBTCRPT  - PRINT TIME CARDS.  BY EMPLOYEE OR BY DAY.      *~
            *            DETAIL OR SUMMARY.                             *~
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
            * 03/27/86 ! ORIGINAL                                 ! KAB *~
            * 07/02/87 ! Removed SYSFILE2.                        ! JIM *~
            * 11/11/88 ! File format changes                      ! ERN *~
            * 08/14/96 ! Changes for the year 2000.               ! DXL *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~

        dim                                                              ~
            askhdr$40,                   /* Ask User header            */~
            askpf1$80,                   /* Ask User first line        */~
            askmid$80,                   /* Ask User second line       */~
            askpf2$80,                   /* Ask User third line        */~
            base$10,                                                     ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            company$60,                                                  ~
            cpgm$8,                      /* CURRENT PROGRAM NAME       */~
            crptid$6,                    /* CURRENT REPORT ID          */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            detdescr$30,                                                 ~
            edtmessage$79,               /* Edit screen message        */~
            emp$12,                      /* CURRENT EMPLOYEE           */~
            errormsg$79,                 /* Error message              */~
            etype$12,                    /* CURRENT EARNINGS TYPE      */~
            fromdate$10,                 /* Time Card Date Range       */~
            fromemp$12,                  /* Employee Range             */~
            gross$10,                                                    ~
            header$132,                  /* G. P. REPORT HEADER LINE   */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            job$8,                       /* CURRENT JOB                */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            ovhd$10,                     /* OVERHEAD                   */~
            printshift$3,                                                ~
            printshift1$1,                                               ~
            plowkey$100,                 /* G. P. READ KEY             */~
            prtdescr$(4)16,                                              ~
            prtamount$(6)10,                                             ~
            rate$10,                                                     ~
            readkey$100,                 /* G. P. Read Key             */~
            shift$1,                                                     ~
            task$,                       /* Time Card Task Code        */~
            tcdate$8,                    /* TIME CARD DATE             */~
            temp$50,                                                     ~
            time$8,                                                      ~
            tlevel$1,                                                    ~
            todate$10,                   /* High Date Cuttoff          */~
            toemp$12,                    /* High Employee Cuttoff      */~
            units$10,                                                    ~
            userid$3,                    /* Current User Id            */~
            wc$4                         /* CURRENT WORK CENTER        */~

        dim                                                              ~
            bgross(5,5),                 /* BREAK GROSS                */~
            bovhd (5,5),                 /* BREAK OVERHEAD             */~
            bunits(5,5),                 /* BREAK HOURS                */~
            dgross(5,5),                 /* DAILY GROSS                */~
            dovhd (5,5),                 /* DAILY OVERHEAD             */~
            dunits(5,5),                 /* DAILY HOURS                */~
            ggross(5,5),                 /* GRAND GROSS                */~
            govhd (5,5),                 /* GRAND OVERHEAD             */~
            gunits(5,5),                 /* GRAND HOURS                */~
            pgross(5,5),                 /* PRINT GROSS                */~
            povhd (5,5),                 /* PRINT OVERHEAD             */~
            punits(5,5)                  /* PRINT HOURS                */~

        dim f2%(50),                     /* = 0 if the file is open    */~
            f1%(50),                     /* = 1 if READ was successful */~
            fs%(50),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(50)20                  /* Text from file opening     */

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
            * # 2 ! JBTCBUFF ! Time Card Buffer (Daily Header)          *~
            * # 3 ! JBTCBUF2 ! Time Card Buffer (Daily Detail)          *~
            * # 4 ! JBTCMSTR ! Time Card Master (Daily Header)          *~
            * # 5 ! JBTCLINE ! Time Card Master (Daily Detail)          *~
            * # 6 ! USERINFO ! Valid Users for this Data Base           *~
            * #50 ! WORKFILE ! Self-evident                             *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 2, "JBTCBUFF",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    8, keylen =  18,                     ~
                        alt key  1, keypos =    2, keylen =  18,         ~
                            key  2, keypos =    1, keylen =  19          ~

            select # 3, "JBTCBUF2",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =  25                      ~

            select # 4, "JBTCMSTR",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    8, keylen =  18,                     ~
                        alt key  1, keypos =    2, keylen =  18,         ~
                            key  2, keypos =    1, keylen =  19          ~

            select # 5, "JBTCLINE",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =  25                      ~

            select # 6, "USERINFO",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  3                       ~

            select #50, "WORKFILE",                                      ~
                        varc,     indexed,  recsize =  74,               ~
                        keypos =    1, keylen =  74                      ~

            call "SHOSTAT" ("Opening files.  One moment please.")

            rslt$(6) = "REQUIRED"
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6),   0%, rslt$( 6))
                if fs%(6) < 0% then exit_program

            call "OPENCHCK" (# 2, fs%( 2), f2%( 2),   0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3),   0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4),   0%, rslt$( 4))
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5),   0%, rslt$( 5))

            if fs%(2) + fs%(4) < 0% then exit_files

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)

            call "READ100" (#6, userid$, f1%(6))
                if f1%(6) = 0% then exit_user

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (ENTER)."

            call "COMPNAME" (12%, company$, f1%(1))

            prtdescr$(1) = "REGULAR"
            prtdescr$(2) = "REGULAR OVERTIME"
            prtdescr$(3) = "HOLIDAY"
            prtdescr$(4) = "HOLIDAY OVERTIME"

            call "EXTRACT" addr("CF", cpgm$)
            crptid$ = " "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode

            init(" ") fromemp$, fromdate$, toemp$, todate$, sort$, file$,~
                  printshift$, detail$, tlevel$, errormsg$, inpmessage$
            pf16$ = "(16)EXIT PROGRAM"

            for fieldnr% = 1 to  7
                gosub'051(fieldnr%)
                      if enabled% = 0 then L10280
L10180:         gosub'111(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10180
L10280:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10180
            next fieldnr%
            goto editmode

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editmode

            pf16$ = "(16)PRINT REPORT"

L11060:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11060
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  7 then L11060
            gosub'051(fieldnr%)
                  if enabled% = 0% then       L11060
L11140:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11140
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11140
            goto L11060

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave

            fromdate1$ = fromdate$:todate1$ = todate$
            fromemp1$  = fromemp$ :toemp1$  = toemp$

            if file$ = "M" then file2% = 4% else file2% = 2%
               file1% = file2% + 1%

            detdescr$ = " "
            if detail$ = "F" then detdescr$ = "COMPLETE LISTING"
            if detail$ = "J" then detdescr$ = "SORTED BY EARNING TYPE/JOB"
            if detail$ = "E" then detdescr$ = "SORTED BY EARNING TYPE"

            if sort$ = "1" then key% = 0% else key% = 1%
            if key% = 0% then break% = 12% else break% = 6%
            page% = 0%
            line% = 1000%
            headerd% = 0%

            time$ = " "
            call "TIME" (time$)

            if fromdate$ <> "ALL" then L19170
               fromdate$ = all(hex(00))
               todate$   = all(hex(ff))
                 goto L19200

L19170:        call "DATUFMTC" (fromdate$)
               call "DATUFMTC" (todate$)

L19200:     if fromemp$ <> "ALL" then L19350
                fromemp$ = all(hex(00))
                toemp$   = all(hex(ff))

L19350:     call "SHOSTAT" ("Processing Time Card Files.")

            gosub create_workfile

            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100,         /* Card Date Range  */~
                                    L20200,         /* Employee Range   */~
                                    L20250,         /* PRINT SHIFT      */~
                                    L20300,         /* File Selection   */~
                                    L20400,         /* Detail Level     */~
                                    L20450,         /* TOTAL LEVEL      */~
                                    L20500          /* Sort Order       */
                     return
L20100:     REM Default/Enable for Time Card Date Range
                if fromdate$ = " " or fromdate$ = blankdate$ ~
                                 then fromdate$ = "ALL"
                inpmessage$ = "Enter DATE range for time card selection."
                return
L20200:     REM Default/Enable for Employee Range
                if fromemp$ = " " then fromemp$ = "ALL"
                inpmessage$ = "Enter EMPLOYEE range for time card selecti~
        ~on."
                return

L20250:     REM Default/Enable for Shift to Print
                if printshift$ = " " then printshift$ = "A"
                inpmessage$ = "Enter SHIFT to print. A = All or 1 - 4."
                return
L20300:     REM Default/Enable for File Selection
                if file$ = " " then file$ = "M"
                inpmessage$ = "Enter 'M' for Master File or 'H' for Holdi~
        ~ng File."
                return
L20400:     REM Default/Enable for Detail Level
                if detail$ = " " then detail$ = "S"
                inpmessage$ = "'S' = Summary 'E' = Earnings Type 'J' = Jo~
        ~b 'F' = Full."
                return
L20450:     REM DEFAULT/ENABLE FOR TOTAL LEVEL
                if tlevel$ = " " then tlevel$ = "B"
                inpmessage$ = "'D' = Daily by Employee 'B' = Sort break p~
        ~oint 'R' = Report 'N' = None."
                return
L20500:     REM Default/Enable for Sort Order
                if sort$ = " " then sort$ = "1"
                inpmessage$ = "Enter '1' or '2' to indicate sort order."
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
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
            *  READ TIMECARD INFORMATION, HERE WE GO.                   *~
            *************************************************************

        create_workfile
            recnbr% = val(str(rslt$(file1%),17,4),4)
            recnbr% = min(recnbr%, 1000%)
            call "WORKOPEN" (#50, "IO   ", (recnbr% / 2%), f2%(50))

            init (hex(00)) readkey$

            call "PLOWALTS" (#file2%, readkey$, key%, 0%, f1%(2))
               goto L30140
        plow_file
            call "READNEXT" (#file2%, f1%(2))
L30140:        if f1%(2) = 0% then print_report

            if key% = 0% then L30250
               get #file2%, using L30180, readkey$
L30180:            FMT POS(2), CH(18)
               if str(readkey$,,6)   >  str(todate$,,6) then print_report
               if str(readkey$,,6)   <  str(fromdate$,,6) then plow_file
               if str(readkey$,7,12) <  str(fromemp$,,12) then plow_file
               if str(readkey$,7,12) <= str(toemp$,,12) then process_card
                  init (hex(ff)) str(readkey$,7)   /* NEXT DATE */
                  goto plow_file

L30250:        get #file2%, using L30260, readkey$
L30260:            FMT POS(8), CH(18)
               if str(readkey$,,12)  >  str(toemp$,,12) then print_report
               if str(readkey$,,12)  <  str(fromemp$,,12) then plow_file
               if str(readkey$,13,6) <  str(fromdate$,,6) then plow_file
               if str(readkey$,13,6) <= str(todate$,,6) then process_card
                  init (hex(ff)) str(readkey$,13)  /* NEXT EMPLOYEE */
                  goto plow_file

        process_card

            get #file2%, using L30351, plowkey$, shift$, holiday$
L30351:         FMT POS(8), CH(18), POS(100), 2*CH(1)
            if printshift1$ = hex(00) then L30380
               if printshift1$ <> shift$ then plow_file

L30380:     init (hex(00)) str(plowkey$,19)

L30400:     call "PLOWNEXT" (#file1%, plowkey$, 18%, f1%(3))
                if f1%(3) = 0% then plow_file

            get #file1% using L30450, job$, task$, lclass$, etype$, wc$,  ~
                                     ovtm$
            if job$ = " " then job$ = ".." & task$
L30450:         FMT POS(26), CH(8), CH(6), POS(50), CH(4), CH(12),       ~
                    POS(98), CH(4), POS(116), CH(1)

            if detail$ = "F" then L30550
            if detail$ = "J" then L30550
               job$, lclass$, wc$ = " "
            if detail$ = "E" then L30550
               etype$ = " "

L30550:     write #50 using L30570, str(readkey$,1,18), shift$, holiday$, ~
                                   ovtm$, etype$, job$, wc$, lclass$,    ~
                                   str(plowkey$,1,25)
L30570:     FMT CH(18), 3*CH(1), CH(12), CH(8), CH(4), CH(4), CH(25)

            goto L30400

        REM *************************************************************~
            * NOW TO PRINT WHAT WE FOUND                                *~
            *************************************************************

        print_report

            select printer (134)
            call "SETPRNT" (crptid$, " ", 0%, 0%)
            init (hex(00)) plowkey$

        plow0

            call "PLOWNEXT" (#50, plowkey$, 0%, f1%(50))
                if f1%(50) = 0% then grand_totals
                   goto get_data

        plowbreak

            call "PLOWNEXT" (#50, plowkey$, break%, f1%(50))
                if f1%(50) = 0% then break_totals
                   goto get_data

        plow18

            call "PLOWNEXT" (#50, plowkey$, 18%, f1%(50))
                if f1%(50) = 0% then daily_totals
                   goto get_data

        plow49

            call "PLOWNEXT" (#50, plowkey$, 49%, f1%(50))
                if f1%(50) = 0% then etype_totals
                   goto get_data

        get_data

            readkey$ = str(plowkey$,50,25)
            call "READ100" (#file1%, readkey$, f1%(3))
                if f1%(3) = 0% then plow49

            get #file1% using L35060, emp$, tcdate$, job$, task$, lclass$,~
                             etype$, rate, units, gross, ovhd, wc$, base
                     if job$ = " " then job$ = ".." & task$
            get str(plowkey$,19,3) using L31420, shift%, holiday$, ovtm$
L31420:         FMT BI(1), CH(1), CH(1)

        REM NOW ACCUMULATE ETYPE TOTALS AND DAILY TOTALS

            lunits = lunits + units
            lgross = lgross + gross
            lovhd  = lovhd  + ovhd

            col% = max(1%, min(shift%, 4%))
            row% = 1%
            if ovtm$ = "Y" then row% = row% + 1%
            if holiday$ = "Y" then row% = row% + 2%

            dunits(row%, col%) = dunits(row%, col%) + units
            dgross(row%, col%) = dgross(row%, col%) + gross
            dovhd (row%, col%) = dovhd (row%, col%) + ovhd

            if detail$ <> "F" then plow49

            call "DATEFMT" (tcdate$)

            gosub header_detail

            call "CONVERT" (base, 4.4, base$)
            call "CONVERT" (rate, 4.4, rate$)
            call "CONVERT" (units, 2.2, units$)
            call "CONVERT" (gross, 2.2, gross$)
            call "CONVERT" (ovhd, 2.2, ovhd$)

            print using L33290, shift%, holiday$, ovtm$, etype$, job$,    ~
                  wc$, lclass$, base$, rate$, units$, gross$, ovhd$
            line% = line% + 1%
            goto plow49

        etype_totals

            if detail$ = "F" then L31830
            if detail$ = "S" then L31830

            get str(plowkey$,19) using L31740, shift%, holiday$, ovtm$,   ~
                                              etype$, job$, wc$, lclass$

L31740:     FMT BI(1), CH(1), CH(1), CH(12), CH(8), CH(4), CH(4)

            call "DATEFMT" (tcdate$)
            gosub header_detail

            call "CONVERT" (lunits, 2.2, units$)
            call "CONVERT" (lgross, 2.2, gross$)
            call "CONVERT" (lovhd, 2.2, ovhd$)

            print using L33290, shift%, holiday$, ovtm$, etype$, job$,    ~
                  wc$, lclass$, " ", " ", units$, gross$, ovhd$
            line% = line% + 1%

L31830:     lunits, lgross, lovhd = 0

            goto plow18

        daily_totals

            mat bgross = bgross + dgross
            mat bovhd  = bovhd  + dovhd
            mat bunits = bunits + dunits

            mat pgross =  dgross
            mat povhd  =  dovhd
            mat punits =  dunits

            call "DATEOK" (tcdate$, f1%(1), errormsg$)

            if headerd% = 0% then L31966
               print using L33230
               line% = line% + 1%
               headerd% = 0%

L31966:     if tlevel$ <> "D" then L32020

            header$ = "DAILY TOTAL FOR EMPLOYEE " & emp$ & " ON " &      ~
                                                                  tcdate$

            gosub print_arrays

L32020:     mat dgross = zer
            mat dovhd  = zer
            mat dunits = zer

            goto plowbreak

        break_totals

            mat ggross = ggross + bgross
            mat govhd  = govhd  + bovhd
            mat gunits = gunits + bunits

            mat pgross =  bgross
            mat povhd  =  bovhd
            mat punits =  bunits

            if tlevel$ = "D" then L32180
            if tlevel$ <> "B" then L32230

L32180:     header$ = "TOTAL FOR"
            if sort$ = "1" then header$ = header$ & " EMPLOYEE " & emp$
            if sort$ = "2" then header$ = header$ & " " & tcdate$

            gosub print_arrays

L32230:     mat bgross = zer
            mat bovhd  = zer
            mat bunits = zer

            line% = 1000%
            goto plow0

        grand_totals

            mat pgross =  ggross
            mat povhd  =  govhd
            mat punits =  gunits

            if tlevel$ = "N" then L32440

            line% = 1000%
            header$ = "REPORT TOTALS"

            gosub print_arrays

L32440:     mat ggross = zer
            mat govhd  = zer
            mat gunits = zer

            close printer
            call "SETPRNT" (" ", " ", 0%, 0%)
            call "FILEBGON" (#50)
            return

        print_arrays

            header$ = header$ & " - SHIFT = " & printshift$

            for i% = 1% to 2%

            if line% + 13% > 55% then line% = 1000%
            gosub header_page

            print skip(1)
            print using L32700
            print using L32730, header$
            print using L32760
            print using L32790, 2%*i% - 1%, 2%*i%
            print using L32850
            print using L32870
            print using L32850

            for j% = 1% to 4%

            call "CONVERT" (punits(j%,2%*i%-1%), 2.2, prtamount$(1))
            call "CONVERT" (pgross(j%,2%*i%-1%), 2.2, prtamount$(2))
            call "CONVERT" (povhd (j%,2%*i%-1%), 2.2, prtamount$(3))
            call "CONVERT" (punits(j%,2%*i%), 2.2, prtamount$(4))
            call "CONVERT" (pgross(j%,2%*i%), 2.2, prtamount$(5))
            call "CONVERT" (povhd (j%,2%*i%), 2.2, prtamount$(6))

            print using L32890, prtdescr$(j%), prtamount$(1),             ~
                  prtamount$(2), prtamount$(3), prtamount$(4),           ~
                  prtamount$(5), prtamount$(6)

            next j%

            print using L32850
            line% = line% + 13%
            next i%

            return
L32700: %+---------------------------------------------------------------~
        ~----------------------------------+
L32730: %! ##############################################################~
        ~##########                        !
L32760: %+------------------+--------------------------------------++----~
        ~----------------------------------!
L32790: %!                  !               SHIFT #                !!    ~
        ~           SHIFT #                !
L32850: %+------------------+------------+------------+------------++----~
        ~--------+------------+------------!
L32870: %! TYPE OF LABOR    !    UNITS   !    GROSS   !  OVERHEAD  !!    ~
        ~UNITS   !    GROSS   !  OVERHEAD  !
L32890: %! ################ ! ########## ! ########## ! ########## !! ###~
        ~####### ! ########## ! ########## !

        REM *************************************************************~
            * HEADING CONTROLS                                          *~
            *************************************************************

        header_detail

            if headerd% <> 0% then L33050
            if line% + 6% > 55% then line% = 1000%

L33050:     gosub header_page
            if headerd% <> 0% then return

            print skip(1)
            print using L33170
            print using L33200, emp$, tcdate$, detdescr$
            print using L33230
            print using L33260
            print using L33230

            line% = line% + 6%
            headerd% = 1%
            return

L33170:    %+------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~-----+
L33200: %!  TIME CARD DETAILS FOR EMPLOYEE NO. ############ AS OF #######~
        ~#   ##############################                               ~
        ~  !
L33230:    %+-------+-----+-----+--------------+----------+--------+-----~
        ~-----+------------+------------+------------+------------+-------~
        ~-----+
L33260:    %! SHIFT ! HOL ! O/T ! EARNING TYPE ! JOB/TASK ! W/CNTR ! LABO~
        ~R CL !    BASE    !    RATE    !    UNITS   !    GROSS   !  OVERH~
        ~EAD  !
L33290:    %!  ##   !  #  !  #  ! ############ ! ######## !  ####  !   ##~
        ~##   ! ########## ! ########## ! ########## ! ########## ! ######~
        ~#### !

        header_page

            if line% < 55% then return

            if headerd% <> 0% then print using L33230

            if page% = 0% then gosub disclaimer
            page% = page% + 1%
            print page

            print using L33660, date$, time$, company$, page%
            print using L33690, cpgm$, crptid$
            print skip (1)

            line% = 3%
            headerd% = 0%

            return

L33660: %######## ########                   ############################~
        ~################################                         PAGE: ##~
        ~###
L33690: %########                                    D A I L Y   T I M E ~
        ~  C A R D   R E P O R T                                  RPT: ###~
        ~###

        disclaimer

            print page

            print using L33660, date$, time$, company$, page%
            print using L33690, cpgm$, crptid$
            print skip (2)

            print using L34170
            print skip (2)

            print using L34190, fromdate1$, todate1$
            print using L34202, fromemp1$, toemp1$
            print using L34210, printshift$

            if file$ = "M" then temp$ = "MASTER" else temp$ = "HOLDING"
               print using L34230, temp$
            temp$ = "NONE, SUMMARY ONLY"
            if detail$ = "E" then temp$ = "BY EARNINGS TYPE"
            if detail$ = "J" then temp$ = "BY EARNINGS TYPE/JOB/WC"
            if detail$ = "F" then temp$ = "FULL DETAIL"
               print using L34250, temp$
            temp$ = "NOT TO PRINT"
            if tlevel$ = "D" then temp$ = "DAILY BY EMPLOYEE"
            if tlevel$ <> "B" then  L34090
               if sort$ = "1" then temp$ = "BY EMPLOYEE"
               if sort$ = "2" then temp$ = "BY DATE"
L34090:     if tlevel$ = "R" then temp$ = temp$ & ", REPORT TOTALS ONLY"
               print using L34270, temp$
            temp$ = "EMPLOYEE"
            if sort$ = "2" then temp$ = "DATE"
               print using L34290, temp$

            return

L34170: %REPORT PARAMETERS:

L34190: %     TIME CARD DATE RANGE:  FROM ##########   TO ##########

L34202: %     EMPLOYEE CODE RANGE:   FROM ############ TO ############

L34210: %     SHIFT SELECTED WAS ###

L34230: %     THE REPORT WAS PRINTED FROM THE ####### FILE

L34250: %     DETAIL LEVEL REQUESTED WAS #################################

L34270: %     SUBTOTALS WERE REQUESTED ###################################

L34290: %     THE FILE WAS SORTED BY ##############

        REM *************************************************************~
            * FORMAT STATEMENTS                                         *~
            *************************************************************

        REM PRLTCRDL & PRLTBUF2

L35060:         FMT CH(12),              /* EMP NUMBER                 */~
                    CH(6),               /* TIME CARD DATE             */~
                    POS(26),             /*                            */~
                    CH(8),               /* JOB NUMBER                 */~
                    CH(6),               /* Task                       */~
                    POS(50),             /*                            */~
                    CH(04),              /* LABOR CLASS                */~
                    CH(12),              /* EARN TYPE                  */~
                    PD(14,4),            /* EARNINGS RATE              */~
                    PD(14,4),            /* NET HOURS                  */~
                    PD(14,4),            /* GROSS PAY                  */~
                    PD(14,4),            /* OVERHEAD AMOUNT            */~
                    CH(4),               /* WORK CENTER                */~
                    POS(107),            /*                            */~
                    PD(14,4)             /* Base Rate                  */

        REM *************************************************************~
            *                S C R E E N   P A G E   1                  *~
            *-----------------------------------------------------------*~
            * Document Screen Number 1.                                 *~
            *************************************************************

            deffn'111(fieldnr%)

                  str(line2$,62) = " JBTCRPT: " & str(cms2v$,,8)
                  if fieldnr% > 0% then L40140
                     init(hex(86)) lfac$()
                     inpmessage$ = edtmessage$
                     goto L40370

L40140:           init(hex(84)) lfac$()
                  on fieldnr% gosub L40300,         /* Card Date Range  */~
                                    L40300,         /* Employee Range   */~
                                    L40300,         /* Shift to Print   */~
                                    L40300,         /* File Selection   */~
                                    L40300,         /* Detail Level     */~
                                    L40300,         /* TOTAL LEVEL      */~
                                    L40300          /* Sort Order       */

                     goto L40370


                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40300:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40370:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Time Cards",                                    ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Time Card Date Range       From:              To:",   ~
               at (06,35), fac(lfac$( 1)), fromdate$            , ch(10),~
               at (06,52), fac(lfac$( 1)), todate$              , ch(10),~
               at (07,02),                                               ~
                  "Employee Range             From:              To:",   ~
               at (07,35), fac(lfac$( 2)), fromemp$             , ch(12),~
               at (07,52), fac(lfac$( 2)), toemp$               , ch(12),~
               at (08,02),                                               ~
                  "Shift to Print",                                      ~
               at (08,35), fac(lfac$( 3)), printshift$          , ch(01),~
               at (09,02),                                               ~
                  "Master or Holding File (M/H)",                        ~
               at (09,35), fac(lfac$( 4)), file$                , ch(01),~
               at (10,02),                                               ~
                  "Detail Level (S, E, J or F)",                         ~
               at (10,35), fac(lfac$( 5)), detail$              , ch(01),~
               at (11,02),                                               ~
                  "Total Level (D, B, R or N)",                          ~
               at (11,35), fac(lfac$( 6)), tlevel$              , ch(01),~
               at (12,02),                                               ~
                  "Sort Order - (1)Employee/Date",                       ~
               at (12,35), fac(lfac$( 7)), sort$                , ch(01),~
               at (13,02),                                               ~
                  "             (2)Date/Employee",                       ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                  , ch(16),~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40840
                  call "MANUAL" (cpgm$)
                  goto L40370

L40840:        if keyhit% <> 15 then L40880
                  call "PRNTSCRN"
                  goto L40370

L40880:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* Card Date Range  */~
                                    L50200,         /* Employee Range   */~
                                    L50250,         /* Shift to Print   */~
                                    L50300,         /* File Selection   */~
                                    L50400,         /* Detail Level     */~
                                    L50450,         /* TOTAL LEVEL      */~
                                    L50500          /* Sort Order       */
                  return
L50100:     REM Test Data for Time Card Date Range
                if fromdate$ <> "ALL" then L50140
                   todate$ = " "
                   return
L50140:         call "DATEOKC" (fromdate$, d1%, errormsg$)
                   if errormsg$ <> " " then return
                if todate$ = " " or todate$ = blankdate$ ~
                               then todate$ = fromdate$
                call "DATEOKC" (todate$, d2%, errormsg$)
                   if errormsg$ <> " " then return
                if d1% <= d2% then return
                errormsg$ = "Invalid date range selection.  Please re-ent~
        ~er."
                return
L50200:     REM Test Data for Employee Range
                if fromemp$ <> "ALL" then L50220
                   toemp$ = " "
                   return
L50220:         if toemp$ = " " then toemp$ = fromemp$
                if fromemp$ <= toemp$ then return
                errormsg$ = "Invalid employee range selection.  Please re~
        ~-enter."
                return
L50250:     REM Test Data for Shift to Print
                printshift1$ = hex(00)
                if str(printshift$,1,1) <> "A" then L50272
                   printshift$ = "ALL"
                   return
L50272:         if str(printshift$,1,1) < "1" then L50282
                if str(printshift$,1,1) > "4" then L50282
                   printshift1$ = printshift$
                   printshift1$ = printshift1$ and hex(07)
                   init (" ") str(printshift$,2,2)
                   return
L50282:         errormsg$ = "Enter 'A' or '1' - '4', please."
                return
L50300:     REM Test Data for File Selection
                if file$ = "M" then return
                if file$ = "H" then return
                errormsg$ = "Enter 'M' or 'H', Please."
                return
L50400:     REM Test Data for Detail Level
                if detail$ = "F" then return
                if detail$ = "J" then return
                if detail$ = "E" then return
                if detail$ = "S" then L50442
                errormsg$ = "Enter 'S', 'E', 'J' or 'F', Please."
                return
L50442:         if tlevel$ <> "N" then return
                goto L50484
L50450:     REM TEST DATA FOR TOTAL  LEVEL
                if tlevel$ = "D" then return
                if tlevel$ = "B" then return
                if tlevel$ = "R" then return
                if tlevel$ = "N" then L50482
                errormsg$ = "Enter 'D', 'B', 'R' or 'N', Please."
                  return
L50482:         if detail$ <> "S" then return
L50484:         errormsg$ = "No Report will be generated."
                return
L50500:     REM Test Data for Sort Order
                if sort$ = "1" then return
                if sort$ = "2" then return
                errormsg$ = "Enter '1' or '2', Please."
                return


        REM *************************************************************~
            *  HERES WHERE WE BAIL OUT BECAUSE OF WHATEVER.             *~
            *************************************************************

        exit_user

            askpf1$ = "YOU ARE NOT LISTED AS A VALID USER IN THIS DATA BA~
        ~SE."
            goto exit_error

        exit_files

            askpf1$ = "THE TIME CARD FILES ARE NOT CURRENTLY AVAILABLE."
            goto exit_error

        exit_error

            askhdr$ = "* * * S O R R Y * * *"
            askmid$ = " "
            askpf2$ = "PRESS RETURN TO EXIT PROGRAM"

            u3% = 0%

            call "ASKUSER" (u3%, askhdr$, askpf1$, askmid$, askpf2$)

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
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program

            call "SHOSTAT" ("One moment please.")
            end
