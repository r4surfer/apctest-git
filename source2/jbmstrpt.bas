        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB   M   M   SSS   TTTTT  RRRR   PPPP   TTTTT   *~
            *    J    B   B  MM MM  S        T    R   R  P   P    T     *~
            *    J    BBBB   M M M   SSS     T    RRRR   PPPP     T     *~
            *  J J    B   B  M   M      S    T    R  R   P        T     *~
            *   J     BBBB   M   M   SSS     T    R   R  P        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBMSTRPT - Production Job Status Report(s)                *~
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
            * 12/16/85 ! ORIGINAL                                 ! MJB *~
            * 04/30/87 ! Page Breaks & Totals                     ! MJB *~
            * 07/01/87 ! JBMASTR2 file format change for Std cost ! JIM *~
            * 09/22/87 ! Correct Job 'Closed' tests               ! HES *~
            * 04/04/88 ! Corrected 'ALL' option,don't check WIPTOT! HES *~
            * 05/05/88 ! Error Message, Print Cost Breakdown, bug ! TLJ *~
            * 03/28/89 ! Modified to allow report combinations    ! MLJ *~
            * 10/10/89 ! Corrected all Range Selecting Logic      ! MJB *~
            * 01/12/93 ! Page 0 Facs fix, End of Report Time Stamp! RJH *~
            * 06/18/93 ! Added Core Processing, Added some totals ! JBK *~
            * 08/12/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            actl_bom(12), actl_rte(12),  /* Actual Job Costs           */~
                          actl_misc(12), /*                            */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            buckets$(12)10,              /* Cost Bucket IDs            */~
            coremsc(12),                 /* Core Misc or Shadow Values */~
            coredb(12),                  /* Core Debits                */~
            corecr(12),                  /* Core Credits               */~
            corecls(12),                 /* Core Closing Values        */~
            corefg(12),                  /* Core Finished Goods Values */~
            closing(12),                 /* Job Closing/Adjustments    */~
            cr_bom(12), cr_tl(12),       /* Credited Job Costs         */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            hidate$10,                   /* Unformatted End Date       */~
            lodate$10,                   /* Unformatted Start Date     */~
            pdateas$8,                   /* Actual Start Date Print    */~
            pdateae$8,                   /* Actual End   Date Print    */~
            pdateps$8,                   /* Planned Start Date Print   */~
            pdatepe$8,                   /* Planned End   Date Print   */~
            print$(6)10,                 /* Work Variable              */~
            print1$(3)11,                /* Work Variable              */~
            wip$16,                      /* WIP GL Account             */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            enddate$10,                  /* Ending Date                */~
            endjob$8, hijob$8,           /* Ending Job Number          */~
            endpart$25, hipart$25,       /* Ending Part Number         */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            hdrsel$60,                   /* Header Selection           */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            jobcode$8,                   /* Job Number                 */~
            part$25,                     /* Part Number for job        */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line2$79,                    /* Screen line #2             */~
            ocflag1$1,                   /* Include Open Incomplete    */~
            ocflag2$1,                   /* Include Open Completed     */~
            ocflag3$1,                   /* Include Closed Jobs        */~
            pftext$17,                   /* Previous Field Key Label   */~
            pfkeys$20,                   /* PFkey Values               */~
            proc$16,                     /* Print O/C/A literal        */~
            prsel$30,                    /* Print Report Type Literal  */~
            range$75,                    /* Literal for range print    */~
            readkey1$8,                  /* Read key for report #1     */~
            readkey2$39,                 /* Read key for report #2 & 3 */~
            rpthead$60,                  /* Header for Comp Name       */~
            rptid$6,                     /* Report ID                  */~
            rpttime$8,                   /* Report Time                */~
            rpttype$1,                   /* Report Type                */~
            set$8, set_id$4,             /* Current Cost Set           */~
            startdate$10,                /* Beginning Date             */~
            startjob$8, lojob$8,         /* Beginning Job Number       */~
            startpart$25, lopart$25,     /* Beginning Part Number      */~
            time$8,                      /* Time Stamp                 */~
            totala(12),                  /* Work Arrary For Totaling   */~
            totalacore(12),              /* Work Arrary For Totaling   */~
            totalc(12),                  /* Work Arrary For Totaling   */~
            totalccore(12),              /* Work Arrary For Totaling   */~
            userid$3                     /* Man At The Helm            */

        dim f2%(20),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(20),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(20)20                  /* TEXT FROM FILE OPENING     */

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
            * #1  ! JBMASTR2 ! Production job master file               *~
            * #2  ! SYSFILE2 ! System Information File                  *~
            * #3  ! WORKFILE ! File used for reports 2 & 3              *~
            * #4  ! JBMASTRC ! Job Master Core Appendix                 *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "JBMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1300,                                 ~
                        keypos =    1, keylen =   8

            select  #2, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20

            select #3,  "WORKFILE",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 47,                                    ~
                        keypos  =   1, keylen = 39

           select #4,  "JBMASTRC",                                       ~
                        varc, indexed, recsize = 600,                    ~
                        keypos =  1, keylen =  8

            call "OPENCHCK" (#2, 0%, f2%(2),   0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date : call "DATEFMT" (date$)
            u3% = 0%     : call "EXTRACT" addr("ID", userid$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press RETURN."

            str(line2$,62) = "JBMSTRPT: " & str(cms2v$,1,8)
            call "COMPNAME" (12%, rpthead$, u3%)
            call "STCSETID" (2%, #2, set$, set_id$, buckets$())

*        Check for Core
            readkey2$ = "SWITCHS.COR"
            call "READ100" (#2, readkey2$, corebank%)     /* SYSFILE2 */
               if corebank% = 0% then  L09250
            get #2 using L09200, rpttype$
L09200:         FMT POS(135), CH(1)
            if rpttype$ <> "Y" then corebank% = 0%
            if corebank% <> 0% then                                      ~
                            call "OPENCHCK" (#4, 0%, f2%(4%),   0%, " ")

L09250:     sumflag$ = "N"
            rpttype$ = "1"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, startjob$, startpart$,     ~
                      startdate$, endjob$, endpart$, enddate$, ocflag1$, ~
                      ocflag2$, ocflag3$, range$, lojob$, hijob$,        ~
                      lopart$, hipart$
            init(hex(00)) lodate$
            init(hex(ff)) hidate$

            enabled% = 1%
            inpmessage$ = "Please enter Y or N to Print Cost Breakdown an~
        ~d 1, 2, or 3 for Report Type"

L10170:     gosub'101
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then L65000
                  if keyhit% <>  0 then L10170
            errormsg$ = " "
            if rpttype$ < "1" or rpttype$ > "3" then errormsg$ =         ~
                                          "Report Type Must be 1, 2 or 3"
            if pos("YN" = sumflag$) = 0 then errormsg$ =                 ~
                               "Print Cost Breakdown Must Be 'Y' or 'N'"
            if errormsg$ <> " " then L10170

            convert rpttype$ to rptsel
            on rptsel goto L10320 , L10430 , L10540

L10320:     for fieldnr% = 1 to 2
                gosub'052(fieldnr%)
                      if enabled% = 0 then L10400
L10350:         gosub'102(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then L10361
L10353:                   fieldnr% = max(1, fieldnr% - 1%)
                          if fieldnr% = 1% then L10320
                              gosub'052(fieldnr%)
                          if enabled% <> 0% then L10350
                          goto L10353
L10361:               if keyhit%  = 16 then L65000
                      if keyhit% <>  0 then L10350
                gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10350
L10400:     next fieldnr%
            goto L11000

L10430:     for fieldnr% = 1 to 2
                gosub'053(fieldnr%)
                      if enabled% = 0 then L10510
L10460:         gosub'103(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then L10471
L10463:                   fieldnr% = max(1, fieldnr% - 1%)
                          if fieldnr% = 1% then L10430
                              gosub'053(fieldnr%)
                          if enabled% <> 0% then L10460
                          goto L10463
L10471:               if keyhit%  = 16 then L65000
                      if keyhit% <>  0 then L10460
                gosub'153(fieldnr%)
                      if errormsg$ <> " " then L10460
L10510:     next fieldnr%
            goto L11000

L10540:     for fieldnr% = 1 to 2
                gosub'054(fieldnr%)
                      if enabled% = 0 then L10620
L10570:         gosub'104(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then L10581
L10573:                   fieldnr% = max(1, fieldnr% - 1%)
                          if fieldnr% = 1% then L10540
                              gosub'104(fieldnr%)
                          if enabled% <> 0% then L10570
                          goto L10573
L10581:               if keyhit%  = 16 then L65000
                      if keyhit% <>  0 then L10570
                gosub'154(fieldnr%)
                      if errormsg$ <> " " then L10570
L10620:     next fieldnr%

L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *************************************************************

            on rptsel goto edtpg21, edtpg22, edtpg23

        edtpg21
L11070:     gosub'112(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then gen_report
                  if keyhit% <>  0 then L11070
            fieldnr% = cursor%(1) - 9
            if fieldnr% > 2 then fieldnr% = 2 else fieldnr% = 1

L11140:     gosub'112(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11140
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L11140
            goto L11070

        edtpg22
L11220:     gosub'113(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then gen_report
                  if keyhit% <>  0 then L11220
            fieldnr% = cursor%(1) - 9
            if fieldnr% > 2 then fieldnr% = 2 else fieldnr% = 1

L11290:     gosub'113(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11290
            gosub'153(fieldnr%)
                  if errormsg$ <> " " then L11290
            goto L11220

        edtpg23
L11370:     gosub'114(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then gen_report
                  if keyhit% <>  0 then L11370
            fieldnr% = cursor%(1) - 9
            if fieldnr% > 2 then fieldnr% = 2 else fieldnr% = 1

L11440:     gosub'114(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11440
            gosub'154(fieldnr%)
                  if errormsg$ <> " " then L11440
            goto L11370

        REM *************************************************************~
            * G E N E R A T E   T H E   S E L E C T E D   R E P O R T S *~
            *************************************************************

        gen_report
            rpttime$ = " " : call "TIME" (rpttime$)
            select printer(134)
            call "OPENCHCK" (#1, 0%, f2%(1), 0%, rslt$(1))
                if f2%(1) <> 0 then L19055
            recnbr% = val(str(rslt$(1),17,4),4)
            if recnbr% <> 0 then L19070
L19055:         errormsg$ = "Sorry, the Job Master File is Empty. This Re~
        ~port can not be produced."
                goto inputmode
L19070:     lcntr% = 99% : pcntr% = -1% : itemcnt% = 0%
            mat totala = zer     : mat totalc = zer
            mat totalacore = zer : mat totalccore = zer
            totalsa, totalsc, totalsacore, totalsccore = 0
            range$ = " "
            inc% = 5%
            if ocflag3$ = " " then inc% = 4%
            if ocflag1$ = " " and ocflag2$ = " " then inc% = 3%
            if ocflag1$ = " " and ocflag3$ = " " then inc% = 2%
            if ocflag2$ = " " and ocflag3$ = " " then inc% = 1%
            if inc% = 5% then proc$ = "ALL JOBS"
            if inc% = 4% then proc$ = "ALL OPEN JOBS"
            if inc% = 3% then proc$ = "CLOSED JOBS"
            if inc% = 2% then proc$ = "OPEN COMP. JOBS"
            if inc% = 1% then proc$ = "OPEN INC. JOBS"
            on rptsel goto report1, report2, report3

        report1
            call "SHOSTAT" ("Generating Production Job Status Report by J~
        ~OB NUMBER")
            rptid$ = "JB0001"
            call "SETPRNT" ("JB0001", " ", 1%, 0%)
            prsel$ = "REPORTED BY JOB NUMBER"
            hdrsel$ = proc$ & " " & prsel$
            call "FMTTITLE" (hdrsel$, " ", 2%)
            if startjob$ <> "ALL" then range$ = "Range Selected From " & ~
                             "Job " & startjob$ & " To Job " & endjob$
            if lcntr% > 56% then gosub page_head
            readkey1$ = lojob$
            call "READ104" (#1, readkey1$, f1%(1))
                if f1%(1) = 0 then end_report
            goto L19255

        read_loop1
            qtymake, qtyleft, qtycomp, totcr, totactl = 0
            pdateae$ = " "
            call "READNEXT" (#1, f1%(1))
                if f1%(1) = 0 then end_report
L19255:     get #1 using L19260, jobcode$,qtymake,qtycomp, pdateae$
L19260:     FMT CH(8),POS(83), PD(14,4),PD(14,4), POS(153), CH(6)
            qtyleft = qtymake - qtycomp
            qtyleft = round(qtyleft,1)
            if jobcode$ > hijob$ then end_report
            if inc% = 5% then L19325
            if inc% = 3% and (pdateae$ = " " or  pdateae$ =  blankdate$) ~
                                                        then read_loop1
            if inc% = 4% and pdateae$ <> " " and pdateae$ <> blankdate$  ~
                                                        then read_loop1
            if inc% = 1% and pdateae$ <> " " and pdateae$ <> blankdate$  ~
                                                        then read_loop1
            if inc% = 1% and qtyleft = 0.0 then read_loop1
            if inc% = 2% and pdateae$ <> " " and pdateae$ <> blankdate$  ~
                                                        then read_loop1
            if inc% = 2% and qtyleft <> 0.0 then read_loop1
L19325:     gosub print_data
            goto read_loop1

        report2
            call "SHOSTAT" ("Generating Production Job Status Report by P~
        ~ART NUMBER")
            rptid$ = "JB0002"
            call "SETPRNT" ("JB0002", " ", 1%, 0%)
            prsel$ = "REPORTED BY PART NUMBER"
            hdrsel$ = proc$ & " " & prsel$
            call "FMTTITLE" (hdrsel$, " ", 2%)
            if startpart$ <> "ALL" then range$ = "Range Selected From " &~
                         "Part " & startpart$ & " To Part " & endpart$
            if lcntr% > 56% then gosub page_head
            if startpart$ = "ALL" then endpart$   = all(hex(ff))
            if startpart$ = "ALL" then startpart$ = all(hex(00))
            call "WORKOPEN" (#3, "IO   ", (recnbr% / 2%), f2%(3))

        read_loop2
            qtymake, qtyleft, qtycomp, totcr, totactl = 0
            call "READNEXT" (#1, f1%(1))
                if f1%(1) = 0 then print_loop23
            get #1 using L19445, jobcode$, part$,qtymake,qtycomp, pdateae$
L19445:     FMT CH(8), POS(58), CH(25), PD(14,4),PD(14,4),POS(153), CH(6)
            if part$ < startpart$ or part$ > endpart$ then read_loop2
            qtyleft = qtymake - qtycomp
            qtyleft = round(qtyleft,1)
            if inc% = 5% then L19510
            if inc% = 3% and (pdateae$ = " " or  pdateae$ =  blankdate$) ~
                                                        then read_loop2
            if inc% = 4% and pdateae$ <> " " and pdateae$ <> blankdate$ ~
                                                        then read_loop2
            if inc% = 1% and pdateae$ <> " " and pdateae$ <> blankdate$ ~
                                                        then read_loop2
            if inc% = 1% and qtyleft = 0.0 then read_loop2
            if inc% = 2% and pdateae$ <> " " and pdateae$ <> blankdate$ ~
                                                        then read_loop2
            if inc% = 2% and qtyleft <> 0.0 then read_loop2
L19510:     write #3 using L35240, part$, jobcode$, pdateps$, jobcode$
            goto read_loop2

        report3
            call "SHOSTAT" ("Generating Production Job Status Report by P~
        ~LANNED START DATES")
            rptid$ = "JB0003"
            call "SETPRNT" ("JB0003", " ", 1%, 0%)
            prsel$ = "REPORTED BY PLANNED START DATE"
            hdrsel$ = proc$ & " " & prsel$
            call "FMTTITLE" (hdrsel$, " ", 2%)
            if startdate$ <> "ALL" then range$ = "Range Selected From " &~
                              startdate$ & " To " &  enddate$
            if lcntr% > 56% then gosub page_head
            call "WORKOPEN" (#3, "IO   ", (recnbr% / 2%), f2%(3))


        read_loop3
            qtymake, qtyleft, qtycomp, totcr, totactl = 0
            call "READNEXT" (#1, f1%(1))
                if f1%(1) = 0 then print_loop23
            get #1 using L19625, jobcode$, part$, qtymake,qtycomp,        ~
            pdateae$,pdateps$
L19625:     FMT CH(8),POS(58),CH(25),PD(14,4),PD(14,4),POS(153),CH(6),   ~
            POS(168),CH(6)
            qtyleft = qtymake - qtycomp
            qtyleft = round(qtyleft,1)
            if pdateps$ < lodate$ or pdateps$ > hidate$ then read_loop3
            if inc% = 5% then L19695
            if inc% = 3% and (pdateae$ = " " or pdateae$ =  blankdate$) ~
                                                        then read_loop3
            if inc% = 4% and pdateae$ <> " " and pdateae$ <> blankdate$ ~
                                                        then read_loop3
            if inc% = 1% and pdateae$ <> " " and pdateae$ <> blankdate$ ~
                                                        then read_loop3
            if inc% = 1% and qtyleft = 0.0 then read_loop3
            if inc% = 2% and pdateae$ <> " " and pdateae$ <> blankdate$ ~
                                                        then read_loop3
            if inc% = 2% and qtyleft <> 0.0 then read_loop3
L19695:     write #3 using L35300, pdateps$, jobcode$, part$, jobcode$
            goto read_loop3

        print_loop23
            init(hex(00)) readkey2$
            call "READ104" (#3, readkey2$, f1%(3))
            goto L19740

L19735:     call "READNEXT" (#3, f1%(3))
L19740:         if f1%(3) = 0% then end_report23
            get #3 using L19750, jobcode$
L19750:     FMT POS(40), CH(8)
            call "READ100" (#1, jobcode$, f1%(1))
                if f1%(1) = 0 then L19735 /* ?? */
            gosub print_data
            goto L19735

        print_data
            get #1 using L35060, jobcode$, part$, qtymake ,qtycomp,       ~
                pdateas$,pdateae$,wip$,pdateps$, pdatepe$,totactl,       ~
                actl_bom(),actl_rte(),actl_misc(),totcr,cr_bom(),cr_tl(),~
                closing, closing()
*        Test for Core Data for the job
            coremastrc% = 0%  :  coredb, corecr, corecls, corefg = 0
            mat coredb = zer : mat corecr = zer : mat corefg = zer
            mat coremsc = zer
            if corebank% = 0% then L19960

            call "READ100" (#4, jobcode$, coremastrc%)
                if coremastrc% = 0% then L19960
            get #4 using L19930, coredb,  coredb(), coremsc(), corecr,    ~
                                corecr(), corecls, corecls(), corefg,    ~
                                corefg()
L19930:         FMT POS(9), 64*PD(14,4)


L19960:     mat actl_bom = actl_bom + actl_rte
            mat actl_bom = actl_bom + actl_misc
            if corebank% = 0% then L20020
                if coremastrc% = 0% then L20020
                     mat actl_bom = actl_bom + corefg
                     totactl = totactl + corefg
L20020:     mat cr_bom = cr_bom + cr_tl
            mat cr_bom = cr_bom + closing
            totcr = totcr + closing
            if corebank% = 0% then L20130
                if coremastrc% = 0% then L20130
                     mat coredb = coredb + coremsc
                     mat corecr = corecr + corecls
                     mat corecr = corecr + corefg
                     corecr = corecr + corecls + corefg
                     corewiptot = coredb - corecr

L20130:     call "DATEFMT" (pdateae$)
            call "DATEFMT" (pdateas$)
            call "DATEFMT" (pdateps$)
            call "DATEFMT" (pdatepe$)
            qtyleft = qtymake - qtycomp
            wiptot = totactl - totcr
            print$() = " "
            call "CONVERT" (qtymake, 0.2, str(print$(1),,7))
            call "CONVERT" (qtyleft, 0.2, str(print$(2),,7))
            if sumflag$ = "N" then L20600
            for i% = 1% to 12%
                if actl_bom(i%) = 0 and cr_bom(i%) = 0 then L20320
                if buckets$(i%) = " " then buckets$(i%) = "??"
                print$(3) = buckets$(i%)
                call "CONVERT" (actl_bom(i%), 2.2, str(print$(4),,9))
                call "CONVERT" (cr_bom(i%), 2.2, str(print$(5),,9))
                temp = actl_bom(i%) - cr_bom(i%)
                call "CONVERT" (temp, 2.2, str(print$(6),,9))
                gosub print_line
L20320:     next i%
            print$(3) = "    Totals"
            gosub print_job_totals
            gosub print_blank_line

            if corebank% = 0% then return
                if coremastrc% = 0% then return
                     print$(1%) = "   Core"  :  print$(2%) = "Value"
                     for i% = 1% to 12%
                     if coredb(i%) = 0 and corecr(i%) = 0 then L20500
                     if buckets$(i%) = " " then buckets$(i%) = "??"
                     print$(3) = buckets$(i%)
                     call "CONVERT" (coredb(i%), 2.2, str(print$(4%),,9%))
                     call "CONVERT" (corecr(i%), 2.2, str(print$(5%),,9%))
                     temp = coredb(i%) - corecr(i%)
                     call "CONVERT" (temp, 2.2, str(print$(6%),,9%))
                     gosub print_line
L20500:              next i%
                     print$(3) = "Core Total"
                     gosub print_core_totals
                     gosub total_core
                     gosub print_blank_line
                     mat coredb = coredb + actl_bom
                     mat corecr = corecr + cr_bom
                     coredb = coredb + totactl
                     corecr = corecr + totcr
                     corewiptot = corewiptot + wiptot
                     print$(1%) = "    Job"  :  print$(2%) = "Totals"
                     for i% = 1% to 12%
                     if coredb(i%) = 0 and corecr(i%) = 0 then L20570
                     if buckets$(i%) = " " then buckets$(i%) = "??"
                     print$(3) = buckets$(i%)
                     call "CONVERT" (coredb(i%), 2.2, str(print$(4%),,9%))
                     call "CONVERT" (corecr(i%), 2.2, str(print$(5%),,9%))
                     temp = coredb(i%) - corecr(i%)
                     call "CONVERT" (temp, 2.2, str(print$(6%),,9%))
                     gosub print_line
L20570:              next i%
                     print$(3) = " Job Total"
                     gosub print_core_totals
                     gosub print_blank_line
                     return

L20600
*        Summary Lines Only
            gosub print_job_totals
            if corebank% = 0% then return
                if coremastrc% <> 0% then L20635
                if lcntr% > 56% then return
                     gosub print_blank_line
                     return
L20635:              print$(3%) = "Core Total"
                     gosub print_core_totals
                     gosub total_core
                     coredb = coredb + totactl
                     corecr = corecr + totcr
                     corewiptot = corewiptot + wiptot
                     print$(3%) = " "
                     str(print$(4%),,9%) = "---------"
                     str(print$(5%),,9%) = "---------"
                     str(print$(6%),,9%) = "---------"
                     gosub print_line
                     print$(3%) = " Job Total"
                     gosub print_core_totals
                     if lcntr% > 56% then return
                          gosub print_blank_line
            return

        print_job_totals
            REM Note totals from JBMASTR2 are used rather then re-adding.~
                This is intentional, on the outside chance they don't    ~
                agree, somebody could see that...
            call "CONVERT" (totactl, 2.2, str(print$(4),,9))
            call "CONVERT" (totcr, 2.2, str(print$(5),,9))
            call "CONVERT" (wiptot, 2.2, str(print$(6),,9))
            mat totala = totala + actl_bom
            mat totalc = totalc + cr_bom
            totalsa = totalsa + totactl
            totalsc = totalsc + totcr
            itemcnt% = itemcnt% + 1%
            gosub print_line
            return

        print_core_totals
            REM Note totals from JBMASTRC are used rather then re-adding.~
                This is intentional, on the outside chance they don't    ~
                agree, somebody could see that...
            call "CONVERT" (coredb, 2.2, str(print$(4),,9))
            call "CONVERT" (corecr, 2.2, str(print$(5),,9))
            call "CONVERT" (corewiptot, 2.2, str(print$(6),,9))
            gosub print_line
            return

        total_core
            mat totalacore = totalacore + coredb
            mat totalccore = totalccore + corecr
            totalsacore = totalsacore + coredb
            totalsccore = totalsccore + corecr
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2,1   *~
            *************************************************************

            deffn'052(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L21120,         /* Job Range        */~
                                    L21200          /* Include Flag     */
                  return
L21120:     REM DEFAULT/ENABLE FOR Beginning Job Number
                if startjob$ = " " then startjob$ = "ALL"
                inpmessage$ = "Enter Starting & Ending JOB NUMBERS"  &   ~
                              " or 'ALL'"
                return

L21200:     REM DEFAULT/ENABLE FOR Include Flag
                inpmessage$ = " 'X' Indicates PRINT, blank to OMIT."
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2,2   *~
            *************************************************************

            deffn'053(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L22120,         /* Beginning Part   */~
                                    L22200          /* Include Flag     */
                  return
L22120:     REM DEFAULT/ENABLE FOR Beginning Part Number
                if startpart$ = " " then startpart$  = "ALL"
                inpmessage$ = "Enter Starting and Ending PART "    &     ~
                              "NUMBERS or 'ALL'"
                return

L22200:     REM DEFAULT/ENABLE FOR Include Flag
                inpmessage$ = " 'X' Indicates PRINT, blank to OMIT."
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2,3   *~
            *************************************************************

            deffn'054(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L23120,         /* Beginning Date   */~
                                    L23200          /* Include Flag     */
                  return
L23120:     REM DEFAULT/ENABLE FOR Beginning Date
                if startdate$ = " " or startdate$ = blankdate$ ~
                                  then startdate$ = "ALL"
                inpmessage$ = "Enter Starting and Ending DATES "   &     ~
                              "or 'ALL'"
                return

L23200:     REM DEFAULT/ENABLE FOR Include Flag
                inpmessage$ = " 'X' Indicates PRINT, blank to OMIT."
                return

        REM *************************************************************~
            *              PAGE HEADING SUBROUTINE                      *~
            *************************************************************
        page_head
            print page
            pcntr% = pcntr% + 1%
            print using L55040, date$, rpttime$, rpthead$, rptid$
            print using L55080, userid$, pcntr%
            print using L55120, hdrsel$
            print using L55310, range$
            if pcntr% <> 0% then L24130
            gosub print_params
            goto page_head
L24130:     print
            print using L55150
            print using L55190
            print using L55230
            lcntr% = 8%
            return

        end_report23
            call "FILEBGON" (#3)

        end_report
            if lcntr% > 56% then gosub page_head
            print
            print
            call "CONVERT" (totalsa, 2.2, print1$(1%))
            call "CONVERT" (totalsc, 2.2, print1$(2%))
            temp = totalsa - totalsc
            call "CONVERT" (temp, 2.2, print1$(3%))
            print using L55340, itemcnt%, print1$(1%), print1$(2%),       ~
                               print1$(3%)
            lcntr% = lcntr% + 3%
            if corebank% = 0% then L25290
                call "CONVERT" (totalsacore, 2.2, print1$(1%))
                call "CONVERT" (totalsccore, 2.2, print1$(2%))
                temp = totalsacore - totalsccore
                call "CONVERT" (temp, 2.2, print1$(3%))
                print1$(4%) = "Core Total"
                gosub print_total_line
                print1$(1%) = "-----------"
                print1$(2%) = "-----------"
                print1$(3%) = "-----------"
                gosub print_total_line
                call "CONVERT" (totalsa + totalsacore, 2.2, print1$(1%))
                call "CONVERT" (totalsc + totalsccore, 2.2, print1$(2%))
                temp = (totalsa + totalsacore) - (totalsc + totalsccore)
                call "CONVERT" (temp, 2.2, print1$(3%))
                print1$(4%) = " Job Total"
                gosub print_total_line

L25290:     if sumflag$ = "N" then L25780
            gosub print_blank_line

            errormsg$ = "********** BUCKET SUMMARY *********"
            for i% = 1% to 12%
                if totala(i%) = 0 and totalc(i%) = 0 then L25430
                if buckets$(i%) = " " then buckets$(i%) = "??"
                print1$(4%) = buckets$(i%)
                call "CONVERT" (totala(i%), 2.2, print1$(1%))
                call "CONVERT" (totalc(i%), 2.2, print1$(2%))
                temp = totala(i%) - totalc(i%)
                call "CONVERT" (temp, 2.2, print1$(3%))
                gosub print_total_line
L25430:     next i%

            print1$(4%) = "     Total"
            call "CONVERT" (totalsa, 2.2, print1$(1%))
            call "CONVERT" (totalsc, 2.2, print1$(2%))
            temp = totalsa - totalsc
            call "CONVERT" (temp, 2.2, print1$(3%))
            gosub print_total_line

            if corebank% = 0% then L25780
            gosub print_blank_line

            errormsg$ = "         ******* CORE TOTAL *******"
            for i% = 1% to 12%
                if totalacore(i%) = 0 and totalccore(i%) = 0 then L25600
                if buckets$(i%) = " " then buckets$(i%) = "??"
                print1$(4%) = buckets$(i%)
                call "CONVERT" (totalacore(i%), 2.2, print1$(1%))
                call "CONVERT" (totalccore(i%), 2.2, print1$(2%))
                temp = totalacore(i%) - totalccore(i%)
                call "CONVERT" (temp, 2.2, print1$(3%))
                gosub print_total_line
L25600:     next i%

            call "CONVERT" (totalsacore, 2.2, print1$(1%))
            call "CONVERT" (totalsccore, 2.2, print1$(2%))
            temp = totalsacore - totalsccore
            call "CONVERT" (temp, 2.2, print1$(3%))
            print1$(4%) = "Core Total"
            gosub print_total_line

            gosub print_blank_line

            errormsg$ = "         ******* JOB TOTALS *******"
            mat totalacore = totalacore + totala
            mat totalccore = totalccore + totalc
            for i% = 1% to 12%
                if totalacore(i%) = 0 and totalccore(i%) = 0 then L25760
                if buckets$(i%) = " " then buckets$(i%) = "??"
                print1$(4%) = buckets$(i%)
                call "CONVERT" (totalacore(i%), 2.2, print1$(1%))
                call "CONVERT" (totalccore(i%), 2.2, print1$(2%))
                temp = totalacore(i%) - totalccore(i%)
                call "CONVERT" (temp, 2.2, print1$(3%))
                gosub print_total_line
L25760:     next i%

            call "CONVERT" (totalsa + totalsacore, 2.2, print1$(1%))
            call "CONVERT" (totalsc + totalsccore, 2.2, print1$(2%))
            temp = (totalsa + totalsacore) - (totalsc + totalsccore)
            call "CONVERT" (temp, 2.2, print1$(3%))
            print1$(4%) = "Job Totals"
            gosub print_total_line


L25780:     time$ = " "  :  call "TIME" (time$)
            print "*****  END OF REPORT @ " & time$ & "  *****"
            close #1  :  close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            goto inputmode

        print_line
            if lcntr% > 56% then gosub page_head
            print using L55270, jobcode$, part$, pdateps$, pdateas$,      ~
                               pdatepe$, pdateae$, print$(1), print$(2), ~
                               print$(3), print$(4), print$(5), print$(6)
            jobcode$, part$, pdateps$, pdateas$, pdatepe$, pdateae$,     ~
            print$(1), print$(2) = " "
            lcntr% = lcntr% + 1%
        return

        print_total_line
            if lcntr% > 56% then gosub page_head
            print using L55380, errormsg$, print1$(4%), print1$(1%),      ~
                                          print1$(2%), print1$(3%)
            lcntr% = lcntr% + 1%
            print1$(4%), errormsg$ = " "
            return

        print_blank_line
            if lcntr% > 56% then return
                print
                lcntr% = lcntr% + 1%
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

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

L35060: FMT                 /* File #1- JBMASTR2                       */~
            CH(8),          /* Production job code                     */~
            POS(58),CH(25), /* Part code                               */~
            PD(14,4),       /* QTYMAKE                                 */~
            PD(14,4),       /* QTYCOMP                                 */~
            POS(147),CH(6), /* Date production job actually started    */~
            CH(6),          /* Date production job actually ended      */~
            CH(9),          /* Work in process GL account code         */~
            CH(6),          /* Date production job planned to start    */~
            CH(6),          /* Date production job planned to be comple*/~
            POS(232),PD(14,4), /* Total Actual Job Costs               */~
            12*PD(14,4),    /* Actual (aka inventory) Costs (BOM)      */~
            12*PD(14,4),    /* Actual (aka inventory) Costs (RTE)      */~
            12*PD(14,4),    /* Actual (aka inventory) Costs (MISC)     */~
            PD(14,4),       /* Total Credited From Job                 */~
            12*PD(14,4),    /* Total Credits from Job. (BOM)           */~
            12*PD(14,4),    /* Total Credits from Job. (RTE + MISC)    */~
            POS(1147),PD(14,4), /* Total Adj/Closing Costs             */~
            12*PD(14,4)     /* Total Adjustments/Closing from Job      */

L35240: FMT                 /* Format for WORKFILE report #2           */~
            CH(25),         /* Part code                               */~
            CH(8),          /* Production job code                     */~
            CH(6),          /* Date production job planned to start    */~
            CH(8)         /* Production job code                     */

L35300: FMT                 /* Format for WORKFILE report #3           */~
            CH(6),          /* Date production job planned to start    */~
            CH(8),          /* Production job code                     */~
            CH(25),         /* Part code                               */~
            CH(8)            /* Production job code                     */

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101
               init(hex(84)) lfac$()
               lfac$(1) = hex(81)
               str(line2$,1,60) = "Select Summary Level And Report Type"

L40110:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Production Job Status Report",                  ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02),                                               ~
                  "Print Cost Breakdown?",                               ~
               at (05,30), fac(lfac$( 1)), sumflag$             , ch(01),~
               at (06,02),                                               ~
                  "Report Type:",                                        ~
               at (06,30), fac(lfac$( 1)), rpttype$             , ch(01),~
               at (06,40), "1 = Report by JOB NUMBER",                   ~
               at (07,40), "2 = Report by PART NUMBER",                  ~
               at (08,40), "3 = Report by PLANNED START DATE",           ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40450
                  call "MANUAL" ("JBMSTRPT")
                  goto L40110

L40450:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40110

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   2,1    *~
            *************************************************************

            deffn'102(fieldnr%)
               gosub setpf4
               init(hex(84)) lfac$()
                  on fieldnr% gosub L41150,         /* Job Range        */~
                                    L41150          /* Include Flag     */
                     goto L41220

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41150:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41220:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Production Job Status Report",                  ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02),                                               ~
                  "Print Cost Breakdown?",                               ~
               at (05,30), fac(hex(84)),   sumflag$             , ch(01),~
               at (06,02),                                               ~
                  "Report Type",                                         ~
               at (06,30), fac(hex(84)),   rpttype$             , ch(01),~
               at (06,40), "1 = Report by JOB NUMBER",                   ~
               at (07,40), "2 = Report by PART NUMBER",                  ~
               at (08,40), "3 = Report by PLANNED START DATE",           ~
                                                                         ~
               at (10,02),                                               ~
                  "Beginning Job Number",                                ~
               at (10,30), fac(lfac$( 1)), startjob$            , ch(08),~
               at (11,02),                                               ~
                  "Ending Job Number",                                   ~
               at (11,30), fac(lfac$( 1)), endjob$              , ch(08),~
               at (12,02),                                               ~
                  "Include Options",                                     ~
               at (12,30), fac(lfac$( 2)), ocflag1$             , ch(01),~
               at (12,40), "Open Incomplete Jobs",                       ~
               at (13,30), fac(lfac$( 2)), ocflag2$             , ch(01),~
               at (13,40), "Open Completed Jobs",                        ~
               at (14,30), fac(lfac$( 2)), ocflag3$             , ch(01),~
               at (14,40), "Closed Jobs",                                ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,28), fac(hex(8c)),   pftext$              ,        ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                        keys(str(pfkeys$)), key(keyhit%)

               if keyhit% <> 13 then L41630
                  call "MANUAL" ("JBMSTRPT")
                  goto L41220

L41630:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L41220

            setpf4
                  if fieldnr% = 1% then L41720
                      pftext$ = "(4)Previous Field"
                      pfkeys$ = hex(0001ffff04ffffffffffffffff0dff0f10)
                      return
L41720:           pftext$ = " "
                  pfkeys$ = hex(0001ffffffffffffffffffffff0dff0f10)
                  return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   2,2    *~
            *************************************************************

            deffn'103(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L42150,         /* Part Range       */~
                                    L42150          /* Include Flag     */
                     goto L42220

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L42150:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L42220:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Production Job Status Report",                  ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02),                                               ~
                  "Print Cost Breakdown?",                               ~
               at (05,30), fac(hex(84)),   sumflag$             , ch(01),~
               at (06,02),                                               ~
                  "Report Type",                                         ~
               at (06,30), fac(hex(84)),   rpttype$             , ch(01),~
               at (06,40), "1 = Report by JOB NUMBER",                   ~
               at (07,40), "2 = Report by PART NUMBER",                  ~
               at (08,40), "3 = Report by PLANNED START DATE",           ~
                                                                         ~
               at (10,02),                                               ~
                  "Beginning Part Number",                               ~
               at (10,30), fac(lfac$( 1)), startpart$           , ch(25),~
               at (11,02),                                               ~
                  "Ending Part Number",                                  ~
               at (11,30), fac(lfac$( 1)), endpart$             , ch(25),~
               at (12,02),                                               ~
                  "Include Options",                                     ~
               at (12,30), fac(lfac$( 2)), ocflag1$             , ch(01),~
               at (12,40), "Open Incomplete Jobs",                       ~
               at (13,30), fac(lfac$( 2)), ocflag2$             , ch(01),~
               at (13,40), "Open Completed Jobs",                        ~
               at (14,30), fac(lfac$( 2)), ocflag3$             , ch(01),~
               at (14,40), "Closed Jobs",                                ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,28),                                               ~
                  "(4)Previous Field",                                   ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(0001040d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L42630
                  call "MANUAL" ("JBMSTRPT")
                  goto L42220

L42630:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L42220

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   2,3    *~
            *************************************************************

            deffn'104(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L43150,         /* Date Range       */~
                                    L43150          /* Include Flag     */
                     goto L43220

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L43150:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L43220:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Production Job Status Report",                  ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02),                                               ~
                  "Print Cost Breakdown?",                               ~
               at (05,30), fac(hex(84)),   sumflag$             , ch(01),~
               at (06,02),                                               ~
                  "Report Type",                                         ~
               at (06,30), fac(hex(84)),   rpttype$             , ch(01),~
               at (06,40), "1 = Report by JOB NUMBER",                   ~
               at (07,40), "2 = Report by PART NUMBER",                  ~
               at (08,40), "3 = Report by PLANNED START DATE",           ~
                                                                         ~
               at (10,02),                                               ~
                  "Beginning Date",                                      ~
               at (10,30), fac(lfac$( 1)), startdate$           , ch(10),~
               at (11,02),                                               ~
                  "Ending Date",                                         ~
               at (11,30), fac(lfac$( 1)), enddate$             , ch(10),~
               at (12,02),                                               ~
                  "Include Options",                                     ~
               at (12,30), fac(lfac$( 2)), ocflag1$             , ch(01),~
               at (12,40), "Open Incomplete Jobs",                       ~
               at (13,30), fac(lfac$( 2)), ocflag2$             , ch(01),~
               at (13,40), "Open Completed Jobs",                        ~
               at (14,30), fac(lfac$( 2)), ocflag3$             , ch(01),~
               at (14,40), "Closed Jobs",                                ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,28),                                               ~
                  "(4)Previous Field",                                   ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(0001040d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L43630
                  call "MANUAL" ("JBMSTRPT")
                  goto L43220

L43630:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L43220

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   2,1     *~
            *************************************************************

            deffn'112(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L44150,         /* Job Range        */~
                                    L44150          /* Include Flag     */
                     goto L44220

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L44150:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L44220:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Production Job Status Report",                  ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02),                                               ~
                  "Print Cost Breakdown?",                               ~
               at (05,30), fac(hex(84)),   sumflag$             , ch(01),~
               at (06,02),                                               ~
                  "Report Type",                                         ~
               at (06,30), fac(hex(84)),   rpttype$             , ch(01),~
               at (06,40), "1 = Report by JOB NUMBER",                   ~
               at (07,40), "2 = Report by PART NUMBER",                  ~
               at (08,40), "3 = Report by PLANNED START DATE",           ~
                                                                         ~
               at (10,02),                                               ~
                  "Beginning Job Number",                                ~
               at (10,30), fac(lfac$( 1)), startjob$            , ch(08),~
               at (11,02),                                               ~
                  "Ending Job Number",                                   ~
               at (11,30), fac(lfac$( 1)), endjob$              , ch(08),~
               at (12,02),                                               ~
                  "Include Options",                                     ~
               at (12,30), fac(lfac$( 2)), ocflag1$             , ch(01),~
               at (12,40), "Open Incomplete Jobs",                       ~
               at (13,30), fac(lfac$( 2)), ocflag2$             , ch(01),~
               at (13,40), "Open Completed Jobs",                        ~
               at (14,30), fac(lfac$( 2)), ocflag3$             , ch(01),~
               at (14,40), "Closed Jobs",                                ~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)PRINT REPORT",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L44630
                  call "MANUAL" ("JBMSTRPT")
                  goto L44220

L44630:        if keyhit% <> 15 then L44670
                  call "PRNTSCRN"
                  goto L44220

L44670:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   2,2     *~
            *************************************************************

            deffn'113(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L45150,         /* Part Range       */~
                                    L45150          /* Include Flag     */
                     goto L45220

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L45150:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L45220:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Production Job Status Report",                  ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02),                                               ~
                  "Print Cost Breakdown?",                               ~
               at (05,30), fac(hex(84)),   sumflag$             , ch(01),~
               at (06,02),                                               ~
                  "Report Type",                                         ~
               at (06,30), fac(hex(84)),   rpttype$             , ch(01),~
               at (06,40), "1 = Report by JOB NUMBER",                   ~
               at (07,40), "2 = Report by PART NUMBER",                  ~
               at (08,40), "3 = Report by PLANNED START DATE",           ~
                                                                         ~
               at (10,02),                                               ~
                  "Beginning Part Number",                               ~
               at (10,30), fac(lfac$( 1)), startpart$           , ch(25),~
               at (11,02),                                               ~
                  "Ending Part Number",                                  ~
               at (11,30), fac(lfac$( 1)), endpart$             , ch(25),~
               at (12,02),                                               ~
                  "Include Options",                                     ~
               at (12,30), fac(lfac$( 2)), ocflag1$             , ch(01),~
               at (12,40), "Open Incomplete Jobs",                       ~
               at (13,30), fac(lfac$( 2)), ocflag2$             , ch(01),~
               at (13,40), "Open Completed Jobs",                        ~
               at (14,30), fac(lfac$( 2)), ocflag3$             , ch(01),~
               at (14,40), "Closed Jobs",                                ~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)PRINT REPORT",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L45630
                  call "MANUAL" ("JBMSTRPT")
                  goto L45220

L45630:        if keyhit% <> 15 then L45670
                  call "PRNTSCRN"
                  goto L45220

L45670:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   2,3     *~
            *************************************************************

            deffn'114(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L46150,         /* Date Range       */~
                                    L46150          /* Include Flag     */
                     goto L46220

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L46150:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L46220:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Production Job Status Report",                  ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02),                                               ~
                  "Print Cost Breakdown?",                               ~
               at (05,30), fac(hex(84)),   sumflag$             , ch(01),~
               at (06,02),                                               ~
                  "Report Type",                                         ~
               at (06,30), fac(hex(84)),   rpttype$             , ch(01),~
               at (06,40), "1 = Report by JOB NUMBER",                   ~
               at (07,40), "2 = Report by PART NUMBER",                  ~
               at (08,40), "3 = Report by PLANNED START DATE",           ~
                                                                         ~
               at (10,02),                                               ~
                  "Beginning Date",                                      ~
               at (10,30), fac(lfac$( 1)), startdate$           , ch(10),~
               at (11,02),                                               ~
                  "Ending Date",                                         ~
               at (11,30), fac(lfac$( 1)), enddate$             , ch(10),~
               at (12,02),                                               ~
                  "Include Options",                                     ~
               at (12,30), fac(lfac$( 2)), ocflag1$             , ch(01),~
               at (12,40), "Open Incomplete Jobs",                       ~
               at (13,30), fac(lfac$( 2)), ocflag2$             , ch(01),~
               at (13,40), "Open Completed Jobs",                        ~
               at (14,30), fac(lfac$( 2)), ocflag3$             , ch(01),~
               at (14,40), "Closed Jobs",                                ~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)PRINT REPORT",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L46630
                  call "MANUAL" ("JBMSTRPT")
                  goto L46220

L46630:        if keyhit% <> 15 then L46670
                  call "PRNTSCRN"
                  goto L46220

L46670:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            * PRINT SELECTION INFORMATION FROM SCREEN                   *~
            *************************************************************

        print_params
L47045:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L47065
                str(i$(), i%, 1%) = hex(20)
                goto L47045
L47065:     print skip(3)
            print tab(37);
            print "--------------------- Report Selection Parameters ----~
        ~---------"
            print
            for x% = 5% to 15%: print tab(37); i$(x%) : next x%
            print tab(37);
            print "------------------------------------------------------~
        ~---------"
            return

        REM *************************************************************~
            *                     T E S T   D A T A       PAGE 2,1      *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* Job Range        */~
                                    L50190          /* Option Flag      */
                     return
L50100:     REM TEST DATA FOR Beginning Job Number
                if startjob$ = " " and endjob$ <> " "                    ~
                     then startjob$ = "FIRST"
                if startjob$ = "FIRST" and endjob$ = " " then L50155
                call "TESTRNGE" (startjob$, endjob$,                     ~
                                  lojob$,   hijob$,  errormsg$)
                return

L50155:         errormsg$ = "Please Enter Ending Job Number"
                return


L50190:     REM TEST DATA FOR Option Selection Flag
                if ocflag1$ = " " and ocflag2$ = " " and ocflag3$ = " "  ~
                    then L50210
                return
L50210:         errormsg$ = "You must select at least one option to conti~
        ~nue"
                return

        REM *************************************************************~
            *                     T E S T   D A T A       PAGE 2,2      *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51100,         /* Beginning Part   */~
                                    L51190          /* Option Flag      */
                     return
L51100:     REM TEST DATA FOR Beginning Part Number
                if startpart$ = " " and endpart$ <> " "                  ~
                   then startpart$ = "FIRST"
                if startpart$ = "FIRST" and endpart$ = " " then L51165
                call "TESTRNGE"(startpart$, endpart$,                    ~
                                lopart$,    hipart$,  errormsg$)
                return

L51165:         errormsg$ = "Please Enter Ending Part Number"
                return


L51190:     REM TEST DATA FOR Option Selection Flag
                if ocflag1$ = " " and ocflag2$ = " " and ocflag3$ = " "  ~
                    then L51212
                return
L51212:         errormsg$ = "You must select at least one option to conti~
        ~nue"
                return

        REM *************************************************************~
            *                     T E S T   D A T A       PAGE 2,3      *~
            *************************************************************

            deffn'154(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L52100,         /* Date Range       */~
                                    L52280          /* Option Flag      */
                     return

L52100: REM TEST DATA FOR Date Range
            if startdate$ <> " " and startdate$ <> blankdate$ then L52140
                errormsg$ = "Must enter starting Date or 'ALL'"
                return
L52140:     if startdate$ <> "ALL" then L52170
                lodate$ = all(hex(00))
                hidate$ = all(hex(ff))
                return
L52170:     call "DATEOKC" (startdate$, lodate%, errormsg$)
                if errormsg$ <> " " then return
            lodate$ = startdate$
            call "DATUFMTC" (lodate$)
            if enddate$ = " " or enddate$ = blankdate$ ~
                            then enddate$ = startdate$
            call "DATEOKC" (enddate$, hidate%, errormsg$)
                if errormsg$ <> " " then return
            hidate$ = enddate$
            call "DATUFMTC" (hidate$)
            if hidate% >= lodate% then return
                errormsg$ = "Ending Date MUST be Equal to or Later Than S~
        ~tarting Date"
                return

L52280:     REM TEST DATA FOR Option Selection Flag
                if ocflag1$ = " " and ocflag2$ = " " and ocflag3$ = " "  ~
                    then L52302
                return
L52302:         errormsg$ = "You must select at least one option to conti~
        ~nue"
                return

        REM *************************************************************~
            *        AND NOW...    THE IMAGE STATEMENTS!                *~
            *************************************************************

L55040: %DATE: ######## ########            #############################~
        ~###############################                      JBMSTRPT:###~
        ~###

L55080: %USER: ###                                          PRODUCTION JO~
        ~B STATUS REPORT                                          PAGE: ##~
        ~##

L55120: %                                   #############################~
        ~###############################

L55150: %                                   ---START DATES--- --FINISH DA~
        ~TES---     QTY     QTY             TOTAL JOB    CREDIT &         ~
        ~WIP

L55190: %JOB NBR  PART NUMBER               PLANNED    ACTUAL PLANNED    ~
        ~ACTUAL TO MAKE    LEFT BUCKET ID.      VALUE   ADJ VALUE       VA~
        ~LUE

L55230: %-------- ------------------------- ----------------- -----------~
        ~------ ------- ------- ----------  ---------   ---------   ------~
        ~---

L55270: %######## ######################### ######## ######## ######## ##~
        ~###### ####### ####### ##########  #########   #########   ######~
        ~###

L55310: %################################################################~
        ~###########

L55340: %                   **************** REPORT TOTALS **************~
        ~**   ###,### ITEMS LISTED...     ########### ########### ########~
        ~###

L55380: %                               #################################~
        ~###               ##########     ########### ########### ########~
        ~###

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

            call "SHOSTAT" ("One moment please")
            end
