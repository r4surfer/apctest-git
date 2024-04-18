        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  RRRR    OOO   PPPP    CCC    AAA   L       CCC           *~
            *  R   R  O   O  P   P  C   C  A   A  L      C   C          *~
            *  RRRR   O   O  PPPP   C      AAAAA  L      C              *~
            *  R   R  O   O  P      C   C  A   A  L      C   C          *~
            *  R   R   OOO   P       CCC   A   A  LLLLL   CCC           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ROPCALC  - This program recalculates the ROP for all      *~
            *            parts.                                         *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/05/87 ! Original                                 ! LKM *~
            * 05/10/89 ! Merge CMS2/CMSI                          ! MJB *~
            *          !  - Added file ROPCLASS                   !     *~
            *          !  - Changed to use ROPHNY as driver file  !     *~
            *          !  - Changed to dissallow ROP calculation  !     *~
            *          !     for periods beyond current.          !     *~
            *          !  - General Clean-up.                     !     *~
            * 08/09/89 ! Branched around MODulus if PANSIZE = 0.  ! JDH *~
            *          !  Problem was if PANSIZE = 0 then EOQ = 0.!     *~
            * 06/15/90 ! Get calculation dates right.             ! JDH *~
            * 11/08/90 ! Removed duplicate CMS2V$ Section         ! MJB *~
            * 08/28/96 ! Changes for the year 2000.               ! DXL *~
            * 10/07/97 ! Tried to clean up date problems.         ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankline$79,                /* Blank Line For Input Screen*/~
            calcflag$1,                  /* From ROP Class File        */~
            xcessflag$1,                 /* From ROP Class File        */~
            class$4,                     /* Part Class Code            */~
            company$50,                  /* Company Name               */~
            date$8,                      /* Date for screen display    */~
            dates$(17)8,                 /* Fiscal Calendar            */~
            dates$8,                     /* Fiscal Calendar            */~
            devratio$6,                  /* For error log              */~
            eoq$10,                      /* Economic Order Qty         */~
            err$4,                       /* Total Errors               */~
            errormsg$79,                 /* Error Message              */~
            ess$10,                      /* Economic Safety Stock      */~
            interest$6,                  /* Interest Rate              */~
            leadtime$10,                 /* Leadtime                   */~
            line2$79,                    /* Second Line of Screen      */~
            logflag$,                    /* Y or N to print error log  */~
            oldeoq$10,                   /* ROPHNY Economic Order Qty  */~
            oldess$10,                   /* HNYMASTR Safety Stock      */~
            oldrop$10,                   /* ROPHNY Re-order Point      */~
            oldstddev$10,                /* ROPHNY Standard Deviation  */~
            oldusage$10,                 /* ROPHNY Average Usage       */~
            part$25,                     /* Part Number                */~
            plowkey$25,                  /* Miscellaneous Read/Plow Key*/~
            prompt$28,                   /* For Accept Statement       */~
            prompt2$28,                  /* For Accept Statement       */~
            purch$10,                    /* Fixed Purchasing Cost      */~
            readkey$50,                  /* Miscellaneous Read Key     */~
            rop$10,                      /* Re-order Point             */~
            ropdate$6,                   /* Date ROP last changed      */~
            runtime$8,                   /* For Audit Report           */~
            shomsg$60,                   /* For SHOSTAT                */~
            smco$9,                      /* Smoothing Coefficient      */~
            stdcost$10,                  /* Standard Cost              */~
            stddev$10,                   /* Standard Deviation         */~
            svyymm$6,                    /* Date of Last Calculation   */~
            svpart$25,                   /* Part being calculated      */~
            temp$6,                      /* For error log              */~
            totpart$6,                   /* Total Parts Updated        */~
            type$3,                      /* Part Type Code             */~
            usage$10,                    /* Average Usage              */~
            used$10,                     /* Used This Month            */~
            used(13),                    /* Usage from HNYHSTRF        */~
            used2(12),                   /* Usage from HNYHSTRY        */~
            wrkcntr$10,                  /* Fixed Workcenter Cost      */~
            yearmnth$6,                  /* From history record        */~
            yearmnthday$8,               /* Temporary Date Variable    */~
            yymm$(1)6                    /* For Accept statement       */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            axd$(32)4,                   /* For OPENFILE               */~
            rslt$(32)20                  /* Text from file opening     */

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
            * # 1 ! HNYHSTRY ! Caelus Management System Information     *~
            * # 2 ! HNYMASTR ! Inventory Master File                    *~
            * # 3 ! ROPHNY   ! File containing part specific ROP data   *~
            * # 4 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 5 ! WORKFILE ! Workfile                                 *~
            * # 6 ! HNYHSTRF ! Inventory History Reference File         *~
            * # 7 ! ROPCLASS ! ROP Class Data File                      *~
            * # 8 ! PIPMASTR ! Planned Inventory Position Master File   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "HNYHSTRY",                                      ~
                        varc,     indexed,  recsize =  450,              ~
                        keypos =    1, keylen =  30                      ~

            select # 2, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select # 3, "ROPHNY",                                        ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  104, keylen = 4, dup

            select # 4, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            select # 5, "WORKFILE",                                      ~
                         varc,    consec,   recsize =  104

            select # 6, "HNYHSTRF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  600,                                  ~
                        keypos =    1, keylen =  32                      ~

            select # 7, "ROPCLASS",                                      ~
                        varc,     indexed,  recsize =  50,               ~
                        keypos = 1,    keylen =  4

            select # 8, "PIPMASTR",                                      ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26          ~

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6), 0%, rslt$( 6))
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7), 0%, rslt$( 7))
            call "OPENCHCK" (# 8, fs%( 8), f2%( 8), 0%, rslt$( 8))

            if fs%(1) < 0 or fs%(6) < 0 then L02431
            if min(fs%()) < 0% then exit_program

L02431:     rslt$(3) = "REQUIRED"
            f2%(3) = 1
            call "OPENFILE" (# 3, "IO   ", f2%( 3), rslt$( 3), axd$(3))
            if f2%(3) <> 0 then exit_program

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            date$ = date
            call "DATEFMT" (date$)
            u3% = 0
            printed, err, work = 0
            readkey$ = "ROP PARAM"
            call "READ100" (#4, readkey$, f1%(4))
            if f1%(4) = 1 then L09190
L09120:        keyhit% = 2
               call "ASKUSER" (keyhit%, "Record Not Found", "System ROP P~
        ~arameters Not Found in SYSFILE2", "This record must be set up thr~
        ~ough program ROPSYSIN prior to running ROPCALC", "Press PF16 to E~
        ~xit")
               if keyhit% = 16 then exit_program else L09120

L09190:     get #4, using L09220 , smco, purch, wrkcntr, interest,devratio,~
                                 pcnt, svpart$, svyymm%, svuflag%

L09220:         FMT POS(21), PD(14,7), 5*PD(14,4), CH(25), BI(4), BI(1)
            convert svyymm% to svyymm$, pic(######)
            call "SPCSMASH" (svyymm$)
            if len(svyymm$) = 1% then svyymm$ = "0" & svyymm$ /* Single digit period */

            call "READ100" (#4, "SWITCHS.HNY", f1%(4))
            if f1%(4) <> 1 then L09281
               get #4 using L09270, cal$
L09270:        FMT POS(95), CH(1)

            if cal$ <> " " then L09290
L09281:           kh% = 2
                  call "ASKUSER" (kh%,"HISTORY RETENTION FLAG NOT VALID",~
                  "The History Retention Flag Must be set to 'F', 'G' or ~
        ~'B' in HNYFLAGS.", "Press any PF key to acknowledge.", " ")
                  goto L65000
L09290:     if cal$ = "G" then L09320
               if fs%(6) > 0 then L09320
                  kh% = 2
                  call "ASKUSER" (kh%, "HNYHSTRF FILE NOT FOUND", "The Hi~
        ~story Retention Flag is set to 'F' or 'B'", "The corresponding hi~
        ~story file does not exist.", "Press any key to acknowledge")
                  goto L65000

L09320:        call "READ100" (#4, "FISCAL DATES", f1%(4))
               get #4, using L09340, periods%, dates$(), currper%
L09340:        FMT POS(21), BI(2), 17*CH(08), BI(2)

            if cal$ <> "G" then L09390
               if fs%(1) > 0 then L09390
                  kh% = 2
                  call "ASKUSER" (kh%, "HNYHSTRY FILE NOT FOUND", "The Hi~
        ~story Retention Flag is set to 'G' but the corresponding history ~
        ~file does not exist.", "Press any key to acknowledge", " ")
                  goto L65000

L09390:        if svpart$ = " " then input_screen
L09400:           mode% = 2
                  call "ASKUSER" (mode%, "Continue Previous Run?", "Press~
        ~ PF1 to Restart Aborted Run", "- or -", "Press PF16 to Exit.")
                  if mode% = 16 then exit_program
                  if mode% <> 1 then L09400
               plowkey$ = svpart$ addc all(hex(ff))
               shomsg$ = "Restarting Calculations with Part: " & svpart$
               call "SHOSTAT" (shomsg$)
               goto plow_next_part

        input_screen
            prompt2$ = "Print Error Log (Y or N)?"
            logflag$ = "N"
            lastcalc$ = svyymm$
            convert currper% to currper$, pic (00)
            if cal$ = "G" then L10420

        REM Fiscal Calendar
               prompt$ = "Fiscal Period to Calculate?"
               mat redim yymm$(1)2
               if str(svyymm$,3,2) = " " then L10090
                   kh% = 2%
                   call "ASKUSER" (kh%, "LAST CALCULATED DATE CONFLICT", ~
        "The last calculated date in SYSFILE2 not in Fiscal format.",    ~
        "ROP is now set to use Fiscal Usage.", "Press RETURN to acknowled~
        ~ge.")
                   goto L65000
L10090:        convert str(svyymm$,,2) to last%
               mo% = min(last% + 1, currper%)
               m% = 1
               if mo% < periods% then L10160
                  m% = 14
                  if mo% = 13% then mo% = 14%
                  if mo% > 17% then mo% = 17%
L10160:        dates$ = (dates$(mo%))
               convert mo% to yymm$(1), pic (00)
               yymm2$ = yymm$(1)
               call "DATEFMT" (dates$)
L10200:        gosub L40000
               if keyhit% = 16 then exit_program
               if keyhit% <> 0 then L10200
               if yymm$(1) = " " then L10200
               errormsg$ = " "
               convert yymm$(1) to mo%
               if mo% > 13% then mo% = mo% - 13%
               if yymm$(1) = str(svyymm$,,2) then L10330
               if yymm$(1) = yymm2$ then L10300
               if yymm$(1) < str(svyymm$,,2) then                        ~
                  errormsg$ = "Can't be earlier than last calculated."   ~
                                      else                               ~
                  errormsg$ = "Can't calculate future ROP."
                  goto L10090

L10300:        svyymm$ = str(yymm$(1)) & "  "
               goto L10750

L10330:        kh% = 2%
               call "ASKUSER" (kh%, "Recalculate This Period?", "Press PF~
        ~1 to Recalculate This Period", "- or -", "Press PF16 to Return to~
        ~ Input Screen.")
               if kh% = 16 then input_screen
               if kh% <> 1 then L10330
               goto L10720

        REM Gregorian Calendar
L10420:     prompt$ = "Month to Calculate? (YYYYMM)"
            dates$ = " "
            if str(svyymm$,5%,2%) <> " " then L10440
                kh% = 2%
                call "ASKUSER" (kh%, "LAST CALCULATED DATE CONFLICT",    ~
        "The last calculated date in SYSFILE2 not in Gregorian format.", ~
        "ROP is now set to use Gregorian Usage.", "Press RETURN to acknow~
        ~ledge.")
                goto L65000
L10440:     convert str(svyymm$,5%,2%) to last%
            convert str(svyymm$,1%,4%) to yy%
            mm% = min(last% + 1, currper%)
            if mm% < 13 then L10500
               mm% = 1
               yy% = yy% + 1
L10500:     convert mm% to str(yymm$(1%),5%,2%), pic(00)
            convert yy% to str(yymm$(1%),1%,4%), pic(0000)
            yymm2$ = yymm$(1)
L10520:     gosub L40000
            if keyhit% = 16 then exit_program
            if keyhit% <> 0 then L10520
            if yymm$(1) = " " then L10520
            errormsg$ = " "
            if yymm$(1) = svyymm$ then L10640
            if yymm$(1) = yymm2$ then L10610
               if yymm$(1) < svyymm$ then                                ~
                  errormsg$ = "Can't be earlier than last calculated."   ~
                                      else                               ~
                  errormsg$ = "Can't calculate future ROP."
               goto L10440

L10610:     svyymm$ = yymm$(1)
            goto L10750

L10640:     kh% = 2%
            call "ASKUSER" (kh%, "Recalculate This Month?", "Press PF1 to~
        ~ Recalculate This Month", "- or -", "Press PF16 to Return to Inpu~
        ~t Screen")
            if kh% = 16 then input_screen
            if kh% <> 1 then L10640

        REM Both Calendars meet here
L10720:     svuflag% = 1

        REM Check for printing log
L10750:     if logflag$ = "N" or logflag$ = "Y" then L10780
               errormsg$ = "Print Error Log must be 'Y' or 'N'"
               if cal$ = "G" then L10520 else L10200
L10780:     if svuflag% = 1 then L20600

        REM *************************************************************~
            *                  M A I N   L O G I C                      *~
            *************************************************************

L20200:     kh% = 2
            call "ASKUSER" (kh%, "OK to Proceed?", "Press PF1 to proceed ~
        ~with ROP calculations", "- or -", "Press PF16 to Return to Input ~
        ~Screen")
            if kh% = 16 then L09000
            if kh% <> 1 then L20200
               init (hex(00)) plowkey$

L20600:     call "READ101" (#4, "ROP PARAM", f1%(4))
                if f1%(4) <> 1 then L09120
		convert svyymm$ to svyymm%
                put #4 using L20750, svyymm%, svuflag%, str(date)
L20750:             FMT POS(94), BI(4), BI(1), CH(6)
            rewrite #4
            call "SHOSTAT" ("ROP Calculation in Progress...")

        plow_next_part  /* Plow ROPHNY for next ROP Part */
            call "PLOWNEXT" (#3, plowkey$, 0%, f1%(3))
            if f1%(3) <> 1% then end_processing
            get #3 using L21350, part$, oldrop, oldeoq, oldusage,         ~
                                oldstddev, svusage, svrop, sveoq, svess, ~
                                svstddev, ropdate$, class$
L21350:         FMT CH(25), 3*PD(14,4), PD(14,7), 4*PD(14,4), PD(14,7),  ~
                              CH(6), CH(04)
            call "READ100" (#7, class$, f1%(7))
            get #7 using L21510, xcessflag$, calcflag$
L21510:         FMT POS(35), CH(1), CH(1)
            if calcflag$ = "Y" then L21600
                errormsg$ = "Class is '" & class$ & "' and ROP "  &      ~
                            "Calculation Flag is NO for this Class"
                gosub error_rtn
                goto plow_next_part

L21600:     call "READ100" (#2, part$, f1%(2))
            get #2, using L21700, part$, leadtime$, type$, oldess, pansize
L21700:         FMT CH(25), POS(170), CH(10), CH(3), POS(318), 2*PD(14,4)
            if type$ < "200" then plow_next_part
            readkey$ = "ROP PARAM"
            call "READ101" (#4, readkey$, f1%(4))
                put #4 using L21950, part$
L21950:             FMT POS(69), CH(25)
            rewrite #4

            if svuflag% <> 1 then L23500
                oldusage = svusage
                oldrop = svrop
                oldeoq = sveoq
                oldstddev = svstddev
                oldess = svess
                goto L23800

L23500:     svusage = oldusage
            svrop = oldrop
            sveoq = oldeoq
            svstddev = oldstddev
            svess = oldess

L23800: REM Get total standard cost
            call "STCCOSTS" (part$, " ", #4, 1%, stdcost)
            if stdcost <> 0 then L24150
               errormsg$ = "Part Standard Cost in Current Cost Set "  &  ~
                           "is Zero."
               gosub error_rtn
               goto plow_next_part

L24150:     readkey$ = str(part$) & hex(00)
            used = 0

        REM Plow HNYHSTRF (Fiscal)
            if cal$ = "G" then L24850
L24400:        call "PLOWNEXT" (#6, readkey$, 25%, f1%(6))
               if f1%(6) <> 1 then do_calculations
               get #6 using L24550, yearmnth%, used()
L24550:        FMT POS(29), BI(4), POS(137), 13*PD(14,4)
               convert yearmnth% to yearmnth$, pic(000000)
               yearmnthday$ = yearmnth$ & "01"
               call "DATECONV" (yearmnthday$)
               if str(yearmnthday$,1%,4%) <> str(dates$(m%),,4%) then L24400
               used = used + used(mo%)
               goto L24400

        REM Plow HNYHSTRY (Gregorian)
L24850:     call "PLOWNEXT" (#1, readkey$, 25%, f1%(1))
            if f1%(1) <> 1 then do_calculations
            get #1 using L25000, year%, used2()
L25000:     FMT POS(29), BI(2), POS(127), 12*PD(14,4)
            convert year% to year$, pic(0000)
            if year$ <> str(yymm$(1%),1%,4%) then L24850
               convert str(yymm$(1%),5%,2%) to mo%
               used = used + used2(mo%)
               goto L24850

        do_calculations
            if type$ < "500" then fixed = purch else fixed = wrkcntr
            convert leadtime$ to leadtime
            leadtime = leadtime / 30
            usage  = (1-smco)*oldusage + smco * used
            stddev = (1-smco)*oldstddev+1.25*smco*abs(used-oldusage)
            if interest <> 0 and stdcost <> 0 then L25750
               eoq = 0
               goto L25890
L25750:     eoq = sqr((fixed * usage * 24) / (interest * stdcost))
            if eoq <= usage * 3 then L25890
               eoq = usage * 3
L25890:     if pansize = 0 then L26050
            r = mod(eoq,pansize)
            if r = 0 then L26050
               eoq = eoq - r + pansize
L26050:     ess = usage * sqr(leadtime)
            rop = usage * (leadtime + sqr(leadtime))
            if rop > oldrop - oldrop * pcnt and rop < oldrop + oldrop *  ~
               pcnt then rop = oldrop else ropdate$ = str(date)
            gosub update_record
            goto plow_next_part

        end_processing
            readkey$ = "ROP PARAM"
            if rop = oldrop then L26550
L26550:     call "READ101" (#4, readkey$, f1%(4))
                put #4 using L26650, " ", 0%
L26650:             FMT POS(69), CH(25), POS(98), BI(1)
            rewrite #4
            if printed = 0 then gosub audit_report
            print
            call "CONVERT" (totpart, 0.0, totpart$)
            print using L30790, totpart$
            print
            print "***** END REPORT *****"
            call "SETPRNT" ("ROP001", " ", 0%, 1%)
            close printer
            if work = 1 then goto error_log else goto exit_program

        update_record
            call "READ101" (#3, part$, f1%(3))
            put #3 using L27400, rop, eoq, usage, stddev, svusage, svrop, ~
                                sveoq, svess, svstddev, ropdate$, class$

L27400:         FMT POS(26), 3*PD(14,4), PD(14,7), 4*PD(14,4),           ~
                    PD(14,7), CH(6), CH(4)
            rewrite #3
            call "READ101" (#2, part$, f1%(2))
                put #2 using L27650, ess
L27650:             FMT POS(318), PD(14,4)
            rewrite #2
            call "READ101" (#8, part$, f1%(8))   /* Update PIPMASTR too */
              if f1%(8) = 0% then L27750
                put #8 using L27730, ess
L27730:             FMT POS(1995), PD(14,4)
            rewrite #8
L27750:     if eoq <> 0 then L28000
               errormsg$ = "Economic Order Quantity ( EOQ ) is Zero."
               gosub error_rtn
               return

L28000:     if xcessflag$ <> "Y" then L28450
            temp = stddev / usage
            if temp <= devratio then L28450
               call "CONVERT" (temp, 2.2, temp$)
               call "CONVERT" (devratio, -2.2, devratio$)
               errormsg$ = "WARNING - Deviation Ratio Exceeds System Limi~
        ~t: " & temp$ & " : " & devratio$
               gosub error_rtn

L28450:     if printed = 1 then L28550
               gosub audit_report
L28550:        gosub print_rec
               return

        error_rtn
            if logflag$ = "N" then return
            if err > 0 then gosub write_error_rec else                   ~
               gosub open_work_file
            return

        audit_report
            call "TIME" (runtime$)
            call "COMPNAME" (12%, company$, u3%)
            page% = 0%
            select printer(134)
            call "SETPRNT" ("ROP001", " ", 0%, 0%)
            call "CONVERT" (smco, -1.7, smco$)
*          X% = POS(SMCO$ = "0")
*          IF X% > 0 THEN STR(SMCO$,X%,1) = " "
            call "CONVERT" (purch, -4.4, purch$)
            call "CONVERT" (wrkcntr, -4.4, wrkcntr$)
            call "CONVERT" (interest, -1.4, interest$)
*          X% = POS(INTEREST$ = "0")
*          IF X% > 0 THEN STR(INTEREST$,X%,1) = " "
            gosub print_header
            printed = 1
            totpart = 0
            return

        print_rec
            if line% > 55% then gosub print_header
            call "CONVERT" (used, 2.2, used$)
            call "CONVERT" (oldusage, 2.2, oldusage$)
            call "CONVERT" (oldstddev, 2.2, oldstddev$)
            call "CONVERT" (oldeoq, 2.2, oldeoq$)
            call "CONVERT" (oldess, 2.2, oldess$)
            call "CONVERT" (oldrop, 2.2, oldrop$)
            call "CONVERT" (usage, 2.2, usage$)
            call "CONVERT" (stddev, 2.2, stddev$)
            call "CONVERT" (eoq, 2.2, eoq$)
            call "CONVERT" (ess, 2.2, ess$)
            call "CONVERT" (rop, 2.2, rop$)
            convert leadtime$ to leadtime
            call "CONVERT" (leadtime, 2.2, leadtime$)
            call "CONVERT" (stdcost, 4.4, stdcost$)
            oldnew$ = "Old:"
            print using L30730, part$, type$, class$, leadtime$, stdcost$,~
                               used$, oldnew$, oldusage$, oldstddev$,    ~
                               oldeoq$, oldess$, oldrop$
            oldnew$ = "New:"
            print using L30760, oldnew$, usage$, stddev$, eoq$, ess$, rop$
            print
            line% = line% + 3%
            totpart = totpart + 1
            return

        print_header
            page% = page% + 1%  :  line% = 8%
            print page
            print using L30600, date$, runtime$, company$
            print using L30620, page%
            print
            print using L30640, str(smco$,2,8), purch$, wrkcntr$,         ~
                               str(interest$,2,5)
            print
            print using L30670
            print using L30700
            print
            return

L30600: %Run Date: ######## ########             ########################~
        ~####################################               ROPCALC:ROP001
L30620: %                                                 RE-ORDER POINT ~
        ~CALCULATION REPORT                                    PAGE: ##
L30640: %Smoothing Coeffiecient = ########   Fixed Purchasing Cost = ####~
        ~######   Fixed Workcenter Cost = ##########   Interest Rate = ###~
        ~##
L30670: %Part Number               Type Class   Leadtime   Std Cost      ~
        ~ Used         Avg Usage    Std Dev        EOQ        ESS        R~
        ~OP
L30700: %------------------------- ---- ----- ---------- ---------- -----~
        ~-----        ---------- ---------- ---------- ---------- --------~
        ~--
L30730: %#########################  ###  #### ########## ########## #####~
        ~#####   #### ########## ########## ########## ########## ########~
        ~##
L30760: %                                                                ~
        ~        #### ########## ########## ########## ########## ########~
        ~##
L30790: %Total Parts Updated is ######

        open_work_file
            call "WORKOPEN" (#5, "OUTPT", 250%, f1%(5))
            if f1%(5) <> 0 then L30860
               work = 1
               gosub write_error_rec
               return

L30860:     keyhit% = 2
            call "ASKUSER" (keyhit%, " ", "Unable To Open Work File.",   ~
                 "An Error Has Occurred But Cannot Be Recorded In The Err~
        ~or Log.", "Press RETURN To Exit.")
            if keyhit% <> 0 then L30860
            goto exit_program

        write_error_rec
            write #5 using L30950, part$, errormsg$
L30950:     FMT CH(25), CH(79)
            err = err + 1
            return

        error_log
            close #5
            call "WORKOPN2" (#5, "INPUT", 0%, f1%(5))
            if f1%(5) <> 0 then exit_program
            call "SHOSTAT" ("Printing Error Log...")
            select printer(134)
            call "SETPRNT" ("ROP002", " ", 0%, 0%)
            errpage% = 0

        plow_next_error
            call "READNEXT" (#5, f1%(5))
            if f1%(5) <> 1 then end_error_log
               get #5 using L31120, part$, errormsg$
L31120:        FMT CH(25), CH(79)
               if errpage% > 0 then print_error_rec
                  gosub error_hdr
                  goto print_error_rec

        error_hdr
             errpage% = errpage% + 1
             print page
             print using L31460, date$, runtime$, company$
             print using L31480, errpage%
             print
             print using L31500
             print using L31510
             print
             errline% = 6
             return

        print_error_rec
             if errline% > 55% then gosub error_hdr
             print using L31530, part$, errormsg$
             errline% = errline% + 1
             goto plow_next_error

        end_error_log
            print
            call "CONVERT" (err, 0.0, err$)
            print using L31550, err$
            print
            print "** END REPORT **"
            call "SETPRNT" ("ROP002", " ", 0%, 1%)
            close printer
            call "FILEBGON" (#5)
            goto exit_program

L31460: %Run Date: ######## ########             ########################~
        ~####################################               ROPCALC:ROP002
L31480: %                                                   RE-ORDER POIN~
        ~T EXCEPTION LOG                                       Page: ##
L31500: %Part Number                 Exception Description
L31510: %-------------------------   ------------------------------------~
        ~-------------------------------------------
L31530: %#########################   ####################################~
        ~###########################################
L31550: %TOTAL ERRORS = ####

L40000: REM Allow override of date
            str(line2$,62) = "ROPCALC: " & str(cms2v$,,8)

            accept                                                       ~
               at (01,02), "Re-order Point Calculation",                 ~
               at (01,65), "Today: ",                                    ~
               at (01,72), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,05), "Last Period Calculated was:",                ~
               at (05,35), fac(hex(84)), lastcalc$,               ch(06),~
               at (07,05), "     The Current Period is:",                ~
               at (07,35), fac(hex(84)), currper$,                ch(04),~
                                                                         ~
               at (10,05), fac(hex(8c)), prompt$,                 ch(28),~
               at (10,35), fac(hex(82)), yymm$(1),                       ~
               at (10,42), fac(hex(8c)), dates$,                  ch(08),~
                                                                         ~
               at (12,05), fac(hex(8c)), prompt2$,                ch(28),~
               at (12,35), fac(hex(81)), logflag$,                ch(01),~
                                                                         ~
               at (21,02), fac(hex(ac)), blankline$,              ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                      keys(hex(000d0f10)), key (keyhit%)

               if keyhit% <> 13 then L40320
                  call "MANUAL" ("ROPCALC")
                  goto L40000

L40320:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40000

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
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

L65000: exit_program
            call "SHOSTAT" ("One Moment Please")

            end
