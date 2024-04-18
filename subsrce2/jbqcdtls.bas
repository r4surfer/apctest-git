        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BBBB    QQQ    CCC   DDDD   TTTTT  L       SSS    *~
            *    J    B   B  Q   Q  C   C  D   D    T    L      S       *~
            *    J    BBBB   Q   Q  C      D   D    T    L       SSS    *~
            *  J J    B   B  Q Q Q  C   C  D   D    T    L          S   *~
            *   J     BBBB    QQQ    CCC   DDDD     T    LLLLL   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBQCDTLS - Prints detailed analysis of the job passed by  *~
            *            the caller. Prints Summary, Material, Labor,   *~
            *            Work Center, and Miscellaneous costs.          *~
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
            * 07/08/87 ! Original                                 ! JIM *~
            * 04/04/88 ! Screen Defaults and Edits not working    ! BPN *~
            *          ! correctly.                               ! BPN *~
            * 05/18/88 ! Corrected Page Break Logic               ! HES *~
            * 06/22/88 ! Changed branch location rdsub for closed ! MDE *~
            *          !  jobs - prevented subsequent tests on    !     *~
            *          !  printing options.                       !     *~
            * 10/12/88 ! Remove repetative shostat calls          ! HES *~
            * 10/12/88 ! Clear Totals, add additional include opt ! HES *~
            * 10/27/88 ! A Little Clean-up, Head Pg 0, Numbering, ! KAB *~
            *          ! Handling of Job in store/lot (Thanx, HES)! KAB *~
            * 03/21/89 ! Changed "Amoont" to "Amount" on Headings,! MLJ *~
            *          ! Qty to print 2 decimal places            ! MLJ *~
            * 03/23/89 ! Fx'd to handle selection by JOB STATUS   ! MLJ *~
            * 09/26/89 ! Fixed so page zero prints on range of    ! JEF *~
            *          ! jobs, minor total printing alignment.    ! JEF *~
            * 05/03/91 !(PRR 11551) Added the Qty Reverse field to! RJB *~
            *          !     the GET statement of JBCREDIT. Added !     *~
            *          !     Using the qty reversed in total calc.!     *~
            * 05/28/91 ! PRR 11954 Restore Actual Unit Cost.      ! JIM *~
            * 05/28/91 ! Modified PF(16) descriptive.             ! JIM *~
            * 04/01/93 ! Added JBMASTRC channel and enhanced to   ! WPH *~
            *          ! print core value data on report          !     *~
            * 05/15/93 ! Core Project                             ! KAB *~
            *          !    Major Restructuring - clean up        !     *~
            *          !    Support for core ledgers, Closing Adj.!     *~
            * 06/02/94 ! PRR 13170 - Show details of return from  ! RJH *~
            *          !   Inventory on Credit Transaction Report !     *~
            * 05/15/93 ! Access to core ledgers/Closing Adj       ! KAB *~
            * 02/28/95 ! PRR 13268 - JBVALUE2 Records now corrctly! RJH *~
            *          !  have Posting Date and transaction date. !     *~
            *          !  Report uses Posting date,except material!     *~
            *          !  details which uses sysdate.             !     *~
            * 07/17/96 ! Changes for the year 2000.               ! DXL *~
            * 04/02/97 ! Added return code to PUTPARM call for NT ! LDJ *~
            * 09/05/97 ! Changed Plow on JBTIF to alt 1.          ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "JBQCDTLS"                   /* Job Analysis Print S/R     */~
                (jobnbr$,                /* Job Number passed fr caller*/~
                #01,                     /* SYSFILE2 UFB               */~
                #02,                     /* JBMASTR2 UFB               */~
                #03,                     /* JBMATER2 UFB               */~
                #04,                     /* JBVALUE2 UFB               */~
                #05,                     /* HNYMASTR UFB               */~
                #08)                     /* JBMASTRC UFB               */

        dim                                                              ~
            activity$4,                  /* Workcenter activity code   */~
            bcktid$(12)10, bcktnm$(12)20,/* Standard cost bucket stuff */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cost(12), tcost(12),         /* Cost Arrays                */~
            bucket$10,                   /* BUCKET ID                  */~
            col1$(12)10,                 /*                            */~
            col2$(12)10,                 /*                            */~
            col3$(12)10,                 /*                            */~
            col4$(12)10,                 /*                            */~
            col5$(12)10,                 /*                            */~
            col6$(12)10,                 /*                            */~
            col7$(12)10,                 /*                            */~
            col8$(12)10,                 /*                            */~
            col8(12),                    /*                            */~
            col1$10,                     /*                            */~
            col2$10,                     /*                            */~
            col3$10,                     /*                            */~
            col4$10,                     /*                            */~
            col5$10,                     /*                            */~
            col6$10,                     /*                            */~
            col7$10,                     /*                            */~
            col8$10,                     /*                            */~
            coname$60,                   /* Company name               */~
            core$4,                      /* Core Record Flag           */~
            subcst$10,                   /* Another subtotal for print */~
            cset$8, csetnm$32, csetcd$4, /* Cost set ID, description   */~
            date$8,                      /* Date for screen display    */~
            text$40,                     /* Credit Posting Text        */~
            err%(3),                     /* Error Indicator for Msgs   */~
            earntype$12,                 /* Earnings code              */~
            earnrate$12,                 /* Earning Rate               */~
            employee$12,                 /* Employee code              */~
            jobnbr$8, jobdesc$32,        /* Job Number passed fr caller*/~
            jobnme$8,                    /* JOB                        */~
            lot$6,                       /* Lot number                 */~
            cstflag$1,                   /* Cost Credit Flag "B" or "P"*/~
            part$25, partdesc$32,        /* Part Number, description   */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            postdate$8,                  /* Date job cost posted       */~
            posttext$40,                 /* Posting comment            */~
            rptid$6,                     /* Report ID                  */~
            secthdr$100,                 /* Section Header             */~
            store$3, prtstore$10,        /* Store number               */~
            subhdr$95,                   /* Job, description, cost set */~
            sysdate$8,                   /* Date job cost recorded     */~
            findte$6,                    /* Date job was completed     */~
            time$8,                      /* Time of day                */~
            user$3,                      /* User who recorded transact.*/~
            userid$3,                    /* Current user               */~
            beg_job$(2)8,                /* Starting Customer Number   */~
            end_job$(2)8,                /* Ending Customer Number     */~
            cursor%(2),                  /* Cursor position holder     */~
            ghr$10,                      /* TOTAL HOURS                */~
            chk_job$9,                   /* Job to Check in JBTIF      */~
            errormsg$79,                 /* Error message              */~
            header$(2)15,                /* Variable Screen Title      */~
            pflag$(7)1, pflag$1,         /* PAGE OPTION FLAGS          */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            gthrs$10,                    /* Grand Total Hours          */~
            gtqty$10,                    /* Grand Total Quantity       */~
            gtlft$10,                    /* QTY LEFY                   */~
            line1$79,                    /* Second Line of Screen Headr*/~
            gqt$10,                      /* Total for Summary          */~
            gtcst$12,                    /* Grand Total Material       */~
            gtact$10,                    /* Actual Unit Cost           */~
            gtcrd$10,                    /* TOTAL                      */~
            gtttl$10,                    /* ANOTHER TOTAL              */~
            qtyrever$10,                 /* Reversed Qty frm Inventory */~
            qtymake$10,                  /* Qty Made & sentto Inventory*/~
            pf$(3)79, pfkey$32,          /* PF Prompts and Keys        */~
            workcenter$4,                /* Work center code           */~
            pd$1, pm$1,                  /* Print File Info            */~
            pf$8, pl$8, pv$6             /* Print File Info            */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
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
            *                       F I L E S                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #02 ! JBMASTR2 ! Production job master file               *~
            * #03 ! JBMATER2 ! Production job material used detail file *~
            * #04 ! JBVALUE2 ! Production job value added detail file   *~
            * #05 ! HNYMASTR ! Inventory Master File                    *~
            * #06 ! JBCREDIT ! Production Credit Transaction file       *~
            * #07 ! JBTIF    ! Job Transaction Image File               *~
            * #08 ! JBMASTRC ! Job Master Core Appendix                 *~
            *************************************************************
        REM OPEN JBCREDIT AND JBTIF FILE
            select #06, "JBCREDIT",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 22,                        ~
                         alt key 1, keypos = 23, keylen = 48

            select #07, "JBTIF",                                         ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 350,                                  ~
                         keypos =    9, keylen =      18,                ~
                         alt key 1, keypos = 1, keylen = 26

            call "OPENCHCK" (#01, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (#02, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (#03, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (#04, fs%( 4), f2%( 4), 0%, rslt$( 4))
            call "OPENCHCK" (#05, fs%( 5), f2%( 5), 0%, rslt$( 5))
            call "OPENCHCK" (#06, fs%( 6), f2%( 6), 0%, rslt$( 6))
            call "OPENCHCK" (#07, fs%( 7), f2%( 7), 0%, rslt$( 7))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            * IF JOBNBR$ = " " DISPLAY Screen To Select Job Numbers     *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date : call "DATEFMT" (date$)
            rptid$ = "JB0005"
            beg_job$(1) = jobnbr$

            header$(1) = "Beginning Job"  :  header$(2) = "Ending Job"

        REM *************************************************************~
            *       I N P U T   M O D E   -  H E A D E R S              *~
            *-----------------------------------------------------------*~
            * Handles normal input for header screens.                  *~
            *************************************************************

        inputmode
L10070:     errormsg$, inpmessage$ = " "
            init(" ") pflag$(), pd$ : senabled% = 0%
            select ws

            for fieldnr% = 1% to 2%
                gosub'051(fieldnr%)
                      if enabled% = 0% then L10260
L10140:         gosub'101(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
                         if fieldnr% < 3% then L10070
                         fieldnr% = fieldnr% - 1%
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10140
L10210:            if keyhit%  = 16% and fieldnr% = 1% then gosub endit2
                   if keyhit% <> 0% then goto L10140
                gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10140
                      fieldnr% = max(1%,fieldnr%)
L10260:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editmode
            fieldnr% = 0%
            inpmessage$ = "To Modify a Displayed Value, Position Cursor"&~
                          " To Field And Press RETURN"
            gosub'101(0%, 2%)           /* Display Screen - No Entry */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       goto rpt
                  if keyhit% <>  0% then       editmode
            fieldnr% = cursor%(1%) - 6%
                if fieldnr% <   1% or  fieldnr%  > 11% then editmode
                if fieldnr% > 1% then fieldnr% = 2%
            gosub'051(fieldnr%)
                  if enabled% = 0% then       editmode
L11180:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11180
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then  L11180
                          goto editmode

        REM *************************************************************~
            *            D E F A U L T S   F O R   P A G E   1          *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS for Screen 1 of Input.                      *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20180,         /* Job # RANGE      */~
                                    L20250,         /* Summary Option   */~
                                    L20320,         /* Material         */~
                                    L20390,         /* Labor Page       */~
                                    L20460,         /* Wrk Center       */~
                                    L20530,         /* Dir & Misc.      */~
                                    L20600,         /* Credit           */~
                                    L20670,         /* Job Status       */~
                                    L20790          /* Print or Disp    */
                     return

L20180:     REM Default/Enable for Vendor Range
                if beg_job$(1) = " " then beg_job$(1) = "ALL"
                inpmessage$ = "Enter the JOB # Range To be Processed " & ~
                              "or 'ALL' for all Job #'s ."
                return

L20250:     REM Default/Enable for PO Range
                if pflag$(1) = " " then pflag$(1) = "D"

L20320:     REM Default/Enable for Material Cost Page
                if pflag$(2) = " " then pflag$(2) = "S"

L20390:     REM Default/Enable for Labor Cost Page
              if pflag$(3) = " " then pflag$(3) = "S"

L20460:     REM Default/Enable for Work Center Page
              if pflag$(4) = " " then pflag$(4) = "S"

L20530:     REM Default/Enable for Direct & Miscellaneous Costs Page
                if pflag$(5) = " " then pflag$(5) = "S"

L20600:     REM Default/Enable for Credit Page
              enabled% = 1%
              if pflag$(6) = " " then pflag$(6) = "S"

L20670: REM  DEFAULT/ENABLE FOR JOB STATUS FLAG
              senabled% = 1%
              if pflag$(7) = " " then pflag$(7) = "A"
              if beg_job$(1) <> end_job$(1) then L20790
                 pflag$(7) = "A"
                 senabled% = 0%

L20790: REM  DEFAULT/ENABLE FOR PRINT OR DISPLAY
            if pd$ = " " then pd$ = "P"

            inpmessage$ = "Enter the Cost Options, Job Status, and " &   ~
                          "Print/Display Selections."
            return

        REM *************************************************************~
            * S T A R T O V E R   S E C T I O N                         *~
            *-----------------------------------------------------------*~
            * Allows User to Start Over with Last Chance.               *~
            *************************************************************

        startover
L29070:     u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
                if u3% <> 0% then L29070

            return clear all
            beg_job$(), end_job$(), pflag$() = " "
            goto inputmode

        REM *************************************************************~
            * P R I N T  R E P O R T                                    *~
            *-----------------------------------------------------------*~
            * Here We Go!!                                              *~
            *************************************************************

        rpt
            if pd$ <> "D" then                                           ~
            call "SHOSTAT" ("Printing Job Cost Detail Report")           ~
                          else                                           ~
            call "SHOSTAT" ("Generating Job Cost Detail Report")

            select printer (134)
            call "SETPRNT" (rptid$, " ",0%,0%)
            if pd$ <> "D" then L30110
               call "EXTRACT" addr("PM", pm$)
               call "SET" addr("PM", "K")

L30110: REM General initialization
            call "EXTRACT" addr ("ID", userid$)
            u3% = 2%
            call "COMPNAME" (12%, coname$, u3%)
            time$ = " "  : call "TIME" (time$)
            n% = 12% /* STCSETID rqst; number of buckets comes back here*/
            cset$ = " " /* Get 'current' cost set; name comes back here */
            call "STCSETID" (n%, #01, cset$, csetcd$, bcktid$(),         ~
                bcktnm$(), csetnm$)
            call "PUTPAREN" (csetnm$)
            need_page0% = 1%    /* Turned off after first header       */
            found%      = 0%    /* Count of Jobs meeting criteria      */
            printed%    = 0%    /* Count of Jobs actually printed      */
            max_lines%  = 50%

        REM Initialize Beginning Job Number
            plowkey$ = all(hex(00))
            plowkey$ = str(beg_job$(2%)) & hex(00)
        REM READ LOOP FOR REPORT
            call "PLOWALTS"(#02, plowkey$, 0%, 0%, f1%(2))
            goto L30350

        nxt
            call "READNEXT" (#02,f1%(2))
L30350:        if f1%(2) = 0% then goto endit
            gosub rdsub
            goto nxt

        rdsub

            get #02 using L30440, jobnme$, jobdesc$, part$, qtymake,      ~
                          qtycomp, findte$, totcost, cost(), totcrdt,    ~
                          col8
L30440:         FMT  /* File #02- JBMASTR2                             */~
                   CH(8), CH(30), POS(58), CH(25), 2*PD(14,4),POS(153),  ~
                   CH(6), POS(232), 13*PD(14,4), POS(528), PD(14,4),     ~
                   POS(1147), PD(14,4)
            if str(jobnme$,,8) <= str(end_job$(2),,8) then L30520
               return clear all
               goto endit

L30520:     qtyleft = qtymake - qtycomp

*       ** CHECK REQUIREMENTS FOR JOB STATUS FLAG SELECTION
            if pflag$(7%)  = "A" then L30640
            if pflag$(7%) <> "C" then L30590
               if findte$  = " " or findte$ = blankdate$ then return
                  goto L30640
L30590:     if findte$    <> " " and findte$ <> blankdate$ then return
            if pflag$(7%)  = "D" and qtyleft > 0 then return
            if pflag$(7%)  = "I" and qtyleft = 0 then return
*       ** IF PFLAG$(7%)  = "O" is all open, drop through...

L30640:     found% = found% + 1%

            call "PUTPAREN" (jobdesc$)

            page_nbr% = 0%
            prntd%    = 0%     /* Job had something printed            */
            nitems%   = 0%     /* Section Active                       */
            new_page% = 1%     /* New Report for each Job              */

            subhdr$ = "Job: " & jobnme$ & " " & jobdesc$ & " / Cost set"&~
                ": " & cset$ & " " & csetnm$
            call "STRING" addr ("CT", subhdr$, len(str(subhdr$)))

        REM Print Job cost summary data (JBMASTR2). Start by editing the ~
            data gotten at initialization time.
            call "DESCRIBE" (#05, part$, partdesc$, 0%, f1%(5))

        REM Check JBTIF File To See If Any Transactions Pending or error
            gosub err_chk

            if need_page0% = 1% then L31000
            time$ = " "  : call "TIME" (time$)
              /* New strart time for each job except first, where it   */
              /* is reset after printing page 0.                       */

L31000: REM *************************************************************~
            * Job Summary Section                                       *~
            *************************************************************

            if pflag$(1%) = "N" then goto L32000  /* Summary Flag */

        REM Et, voila! ... the print-out begins
            do_cores% = 0%

            call "CONVERT" (qtymake, 2.0, gqt$)
            call "CONVERT" (qtycomp, 2.0, ghr$)
            call "CONVERT" (qtyleft, 2.0, gtlft$)
            call "CONVERT" (totcost, 2.2, gtttl$)
            call "CONVERT" (totcrdt, 2.2, gtcrd$)
            call "CONVERT" (col8   , 2.2, col8$)
            gtact$ = "      0.00"
            if qtymake <> 0 then                                         ~
               call "CONVERT" (round(totcost/qtymake,4), 2.4, gtact$)

            m% = 5%

            call "READ100" (#8, jobnme$, do_cores%)
            if do_cores% <> 0% then m% = 7%

            secthdr$ = "SUMMARY OF JOB: " & jobnme$ & " " & jobdesc$

            if new_page% = 1% then gosub page_heading
            if nbr_lines% + m% > max_lines% then gosub page_heading

            print using L62210, secthdr$
            print skip(1)
            print using L62310 , "#"            /* Summary column header */
            print using L62325                            /* Underscore */
            print using L62340, part$, partdesc$, gqt$, ghr$, gtlft$,     ~
                gtact$, gtttl$, gtcrd$, col8$
            if do_cores% = 0% then L31255

            get #8 using L31185, col1, col2, col3
L31185:         FMT POS(9), PD(14,4), POS(209), PD(14,4),                ~
                    POS(313), PD(14,4)
            call "CONVERT" (col1, 2.2, gtttl$)
            call "CONVERT" (col2, 2.2, gtcrd$)
            call "CONVERT" (col3, 2.2, col3$)
            gtact$ = "      0.00"
            if qtymake <> 0 then                                         ~
               call "CONVERT" (round(col1/qtymake,4), 2.4, gtact$)

            print using L62355, gtact$, gtttl$, gtcrd$, col3$

            col4 = (totcost + col1) - (totcrdt + col2 + col8 + col3)
            call "CONVERT" (col4, 2.2, gtcrd$)

            print using L62370, gtcrd$

L31255:     print skip(2)
            nbr_lines% = nbr_lines% + m% + 2%
            prntd% = 1%
            if pflag$(1%) = "S" then goto L32000  /* SUMMARY FLAG */

*       ** Now for the Job Summary

            col1 = 0
            col2 = 0
            col3 = 0
            col4 = 0
            col5 = 0
            col6 = 0
            col7 = 0
            col8 = 0

            init(" ") col8$(), col8$

*       ** format & total BOM costs
            mat tcost = cost
            for i% = 1% to n%
               col1 = col1 + cost(i%)
               call "CONVERT" (cost(i%), 2.2, col1$(i%))
            next i%

*       ** GET, format & total RTE & Labor costs
            get #02 using L31364, cost()
L31364:         FMT POS(336), 12*PD(14,4)   /* File #02- JBMASTR2 */
            mat tcost = tcost + cost
            for i% = 1% to n%
               col2 = col2 + cost(i%)
               call "CONVERT" (cost(i%), 2.2, col2$(i%))
            next i%

*       ** GET, format & total Direct & Misc costs
            get #02 using L31384, cost()
L31384:         FMT POS(432), 12*PD(14,4)   /* File #02- JBMASTR2 */
            mat tcost = tcost + cost
            for i% = 1% to n%
               col3  = col3  + cost(i%)
               call "CONVERT" (cost(i%), 2.2, col3$(i%))
            next i%

*       ** Format Job Debits
            mat col8 = tcost
            for i% = 1% to n%  /* TOTAL COSTS CUMULATIVE */
               col4 = col4 + tcost(i%)
               call "CONVERT" (tcost(i%), 2.2, col4$(i%))
            next i%

*       ** GET, SUM and format unfolded credits
            get #02 using L31424, cost(), tcost()
L31424:         FMT POS(536), 12*PD(14,4), 12*PD(14,4)
            mat tcost = tcost + cost
            mat cost  = tcost
            for i% = 1% to n%
               col5 = col5 + cost(i%)
               call "CONVERT" (cost(i%), 2.2, col5$(i%))
            next i%

*       ** GET, SUM and format adjustments
            get #02 using L31444, cost()
L31444:         FMT POS(1155), 12*PD(14,4)
            mat tcost = tcost + cost
            for i% = 1% to n%
               col6 = col6 + cost(i%)
               call "CONVERT" (cost(i%), 2.2, col6$(i%))
            next i%
            if do_cores% = 0% then L31535

*       ** GET and format Core to Finished Goods
            get #08 using L31464, cost()
L31464:         FMT POS(425), 12*PD(14,4)
            mat tcost = tcost - cost
            mat cost = (-1) * cost
            for i% = 1% to n%
               col7 = col7 + cost(i%)
               call "CONVERT" (cost(i%), 2.2, col7$(i%))
            next i%

*       ** Now calculate & format Job Balance
            mat col8 = col8 - tcost
            mat cost = col8
            for i% = 1% to n%
               col8 = col8 + cost(i%)
               call "CONVERT" (cost(i%), 2.2, col8$(i%))
            next i%
            goto L31570

L31535
*       ** Now calculate & format Job Balance
            mat col8 = col8 - tcost
            mat cost = col8
            for i% = 1% to n%
               col7 = col7 + cost(i%)
               call "CONVERT" (cost(i%), 2.2, col7$(i%))
            next i%

L31570
*       ** Last, but not least, print and clean up
            gosub cost_breakdown


*       *** Now check for and print Core Value Summary, if any

            if do_cores% = 0% then L32000

*       ** Got something, lets print it

            col1 = 0
            col2 = 0
            col3 = 0
            col4 = 0
            col5 = 0
            col6 = 0
            col7 = 0
            col8 = 0
            do_cores% = do_cores% + 1%

*       ** GET, format & total Core Material costs
            get #08 using L31675, tcost()
L31675:         FMT POS(17), 12*PD(14,4)
            mat cost = tcost
            for i% = 1% to n%
               col1 = col1 + cost(i%)
               call "CONVERT" (cost(i%), 2.2, col1$(i%))
            next i%

*       ** GET, format & total Core Shadow costs
            get #08 using L31715, cost()
L31715:         FMT POS(113), 12*PD(14,4)
            mat tcost = tcost + cost
            for i% = 1% to n%
               col2 = col2 + cost(i%)
               call "CONVERT" (cost(i%), 2.2, col2$(i%))
            next i%

*       ** Total and format Core Debits
            mat cost = tcost
            for i% = 1% to n%
               col3  = col3  + cost(i%)
               call "CONVERT"(cost(i%), 2.2, col3$(i%))
            next i%

*       ** GET and format Core credits
            get #08 using L31790, cost()
L31790:         FMT POS(217), 12*PD(14,4)
            mat tcost = tcost - cost
            for i% = 1% to n%
               col4 = col4 + cost(i%)
               call "CONVERT" (cost(i%), 2.2, col4$(i%))
            next i%

*       ** GET and format Core to Finished Goods
            get #08 using L31834, cost()
L31834:         FMT POS(425), 12*PD(14,4)
            mat tcost = tcost - cost
            for i% = 1% to n%
               col5 = col5 + cost(i%)
               call "CONVERT" (cost(i%), 2.2, col5$(i%))
            next i%

*       ** GET and format Core Adjustments
            get #08 using L31852, cost()
L31852:         FMT POS(321), 12*PD(14,4)
            mat tcost = tcost - cost
            for i% = 1% to n%
               col6 = col6 + cost(i%)
               call "CONVERT" (cost(i%), 2.2, col6$(i%))
            next i%

*       ** SUM and format Core Balance
            mat cost = tcost
            for i% = 1% to n%
               col7 = col7 + cost(i%)
               call "CONVERT" (cost(i%), 2.2, col7$(i%))
            next i%

*       ** Now SUM and format the Job Balance
            mat col8 = col8 + tcost
            for i% = 1% to n%
               col8 = col8 + col8(i%)
               call "CONVERT" (col8(i%), 2.2, col8$(i%))
            next i%

*       ** Last, but not least, print and clean up
            gosub cost_breakdown

L32000: REM *************************************************************~
            * Print Material cost details (JBMATER2)                    *~
            *************************************************************

            if pflag$(2%) = "N" then goto L34000 /* Material Costs Flag */

            plowkey$ = all(hex(00))
            str(plowkey$,,8) = str(jobnme$) & hex(00)

            subt        = 0
            totcst      = 0
            totqty      = 0
            do_cores%   = 0%
            core_found% = 0%

            secthdr$ = "MATERIAL COST DETAILS FOR JOB: " & jobnme$ &     ~
                        " " & jobdesc$

        read_jbmater2
            call "PLOWNEXT" (#03, plowkey$, 8%, f1%(3))
               if f1%(3) = 0% then goto L32600

            get #03 using L32240, sysdate$, part$, store$, lot$, qtymove, ~
                totcost, cost(), posttext$, user$, core$
L32240:         FMT  /* File #03- JBMATER2                             */~
                     POS(9), CH(6), POS(23), CH(25), CH(3), CH(6),       ~
                     POS(71), 14*PD(14,4), POS(287), CH(40), CH(3),      ~
                     POS(338), CH(4)

            if core$ = " " then L32330                   /* Not a Core */
            if do_cores% <> 0% then L32360            /* Printing them */
                core_found% = 1%  /* flags that we have core material */
                goto read_jbmater2  /* and skip this record for later */
L32330:     if do_cores% = 0% then L32360
               goto read_jbmater2

L32360:     call "DATEFMT" (sysdate$)
            call "DESCRIBE" (#05, part$, partdesc$, 0%, f1%(5))
            totcst = totcst + totcost
            totqty = totqty + qtymove
            call "CONVERT" (qtymove,2.2,gtqty$)
            call "CONVERT" (totcost,2.2,gtcst$)
            gosub store_lot

            pflag$ = pflag$(2%) : gosub section_control
            if ret% = 0% then L32510

            print using L62510
            print using L62525
            nbr_lines% = nbr_lines% + 2%

L32510:     print using L62540 , sysdate$, part$, partdesc$, gtcst$,      ~
                gtqty$, prtstore$, user$,bucket$,subcst$
            nbr_lines% = nbr_lines% + 1%
            if pflag$(2%) = "D" then gosub print_cost_breakdown
            nitems% = nitems% + 1%
            goto read_jbmater2

L32600
*       *** Check to see if we need section totals
            if nitems% = 0% then L32800

            nitems% = 0%
            call "CONVERT" (totcst, 2.2, gtcst$)
            call "CONVERT" (totqty, 2.2, gtqty$)
            call "CONVERT" (subt  , 2.2, subcst$)
            print using L62525
            print using L62555, gtcst$, gtqty$, subcst$
            print skip(2)
            nbr_lines% = nbr_lines% + 4%

L32800
*       *** Check to see if we can print cores
             if core_found% = 0% then L33000
                if do_cores% = 1% then L33000
                   do_cores% = 1%
                   init (hex(00)) str(plowkey$,9%)
                   subt        = 0
                   totcst      = 0
                   totqty      = 0
                   secthdr$ = "CORE MATERIAL DETAILS FOR JOB: " &        ~
                               jobnme$ & " " & jobdesc$
                   goto read_jbmater2

L33000: REM *************************************************************~
            * Print Core Shadow Details (JBVALUE2 type X records)       *~
            *************************************************************

            plowkey$ = all(hex(00))
            str(plowkey$,,9) = str(jobnme$) & "X"
            subt   = 0
            totcst = 0

            secthdr$ = "INDIRECT CORE VALUE DETAILS FOR JOB: " & jobnme$ ~
                        & " " & jobdesc$

        read_jbvalue2_x
            call "PLOWNEXT" (#04, plowkey$, 9%, f1%(4))
               if f1%(4) = 0% then goto L33400
            get #04 using L33180, postdate$, user$, totcost, cost(),      ~
                posttext$

L33180:         FMT  /* File #04- JBVALUE2- Direct/Misc record type 'X'*/~
                     POS(10), CH(6), POS(30), CH(3), 13*PD(14,4), CH(40)

            call "DATEFMT" (postdate$)
            totcst = totcst + totcost
            call "CONVERT" (totcost,2.2,gtcst$)

            pflag$ = pflag$(2%) : gosub section_control
            if ret% = 0% then L33320

            print using L62610            /* Direct & Misc column header */
            print using L62625                             /* Underscore */
            nbr_lines% = nbr_lines% + 2%

L33320:     print using L62640 ,postdate$, posttext$, gtcst$, user$,      ~
                              bucket$,subcst$
            nbr_lines% = nbr_lines% + 1%
            if pflag$(2%) = "D" then gosub print_cost_breakdown

            nitems% = nitems% + 1%
            goto read_jbvalue2_x

L33400
*       *** Now to Subtotal and move on ....
            if nitems% = 0% then L34000

            nitems% = 0%
            call "CONVERT" (totcst, 2.2, gtcst$)
            call "CONVERT" (subt  , 2.2, subcst$)
            print using L62625
            print using L62655, gtcst$, subcst$
            print skip(2)
            nbr_lines% = nbr_lines% + 4%

L34000: REM *************************************************************~
            * Print Labor cost details (JBVALUE2 'L'-type records)      *~
            *************************************************************

            if pflag$(3%) = "N" then goto L35000 /* Labor Costs Flag */

            plowkey$ = all(hex(00))
            str(plowkey$,,9) = str(jobnme$) & "L"

            subt      = 0
            tothrs    = 0
            totcst    = 0
            totearn   = 0

            secthdr$ = "LABOR COST DETAILS FOR JOB: " & jobnme$ & " " &  ~
                        jobdesc$

        read_jbvalue2_labor
            call "PLOWNEXT" (#04, plowkey$, 9%, f1%(4))
               if f1%(4) = 0% then goto L34600

            get #04 using L34250,postdate$, user$, totcost, cost(),       ~
                posttext$, workcenter$, employee$, earntype$, earnrate,  ~
                hours

L34250:         FMT  /* File #04- JBVALUE2- Labor record type 'L'      */~
                     POS(10), CH(6), POS(30), CH(3), 13*PD(14,4), CH(40),~
                     CH(4), 2*CH(12), POS(219), 2*PD(14,4)

            call "DATEFMT" (postdate$)
            tothrs = tothrs + hours
            totcst = totcst + totcost
            totearn = totearn + earnrate
            call "CONVERT" (hours,2.2,gthrs$)
            call "CONVERT" (totcost,2.2,gtcst$)
            call "CONVERT" (earnrate,2.2,earnrate$)

            pflag$ = pflag$(3%) : gosub section_control
            if ret% = 0% then L34440

            print using L62710                    /* Labor column header */
            print using L62725                             /* Underscore */
            nbr_lines% = nbr_lines% + 2%

L34440:     print using L62740 ,postdate$, employee$, workcenter$,        ~
                earntype$, earnrate$ , gtcst$, gthrs$, user$, bucket$,   ~
                subcst$
            nbr_lines% = nbr_lines% + 1%
            if pflag$(3%) = "D" then gosub print_cost_breakdown
            nitems% = nitems% + 1%
            goto read_jbvalue2_labor

L34600
*       *** Now to Subtotal and move on ....
            if nitems% = 0% then L35000

            nitems% = 0%
            call "CONVERT" (totcst , 2.2, gtcst$)
            call "CONVERT" (tothrs , 2.2, gthrs$)
            call "CONVERT" (totearn, 2.2, earnrate$)
            call "CONVERT" (subt   , 2.2, subcst$)
            print using L62725
            print using L62755, earnrate$, gtcst$, gthrs$, subcst$
            print skip(2)
            nbr_lines% = nbr_lines% + 4%

L35000: REM *************************************************************~
            * Print Work Center cost details (JBVALUE2 'W'-type records)*~
            *************************************************************

            if pflag$(4%) = "N" then goto L36000  /* Work Center Costs */

            plowkey$ = all(hex(00))
            str(plowkey$,,9) = str(jobnme$) & "W"

            subt   = 0
            totcst = 0
            tothrs = 0

            secthdr$ = "WORKCENTER COST DETAILS FOR JOB: " & jobnme$ &   ~
                        " " & jobdesc$

        read_jbvalue2_workcenter
            call "PLOWNEXT" (#04, plowkey$, 9%, f1%(4))
               if f1%(4) = 0% then goto L35500

            get #04 using L35230,postdate$, user$, totcost, cost(),       ~
                posttext$, workcenter$, activity$, earnrate, hours

L35230:         FMT  /* File #04- JBVALUE2- W/C record type 'W'        */~
                     POS(10), CH(6), POS(30), CH(3), 13*PD(14,4), CH(40),~
                     CH(4), POS(215), CH(4), 2*PD(14,4)

            call "DATEFMT" (postdate$)
            totcst = totcst + totcost
            tothrs = tothrs + hours
            call "CONVERT" (totcost,2.2,gtcst$)
            call "CONVERT" (hours,2.2,gthrs$)
            call "CONVERT" (earnrate,2.2,earnrate$)

            pflag$ = pflag$(4%) : gosub section_control
            if ret% = 0% then L35410

            print using L62810             /* Work Center column header */
            print using L62825                            /* Underscore */
            nbr_lines% = nbr_lines% + 2%

L35410:     print using L62840 ,postdate$, workcenter$, activity$,        ~
               earnrate$, gtcst$, gthrs$, user$,bucket$,subcst$
            nbr_lines% = nbr_lines% + 1%
            if pflag$(4%) = "D" then gosub print_cost_breakdown
            nitems% = nitems% + 1%
            goto read_jbvalue2_workcenter

L35500
*       *** Now to Subtotal and move on ....
            if nitems% = 0%  then L36000

            nitems% = 0%
            call "CONVERT" (totcst , 2.2, gtcst$)
            call "CONVERT" (tothrs , 2.2, gthrs$)
            call "CONVERT" (totearn, 2.2, earnrate$)
            call "CONVERT" (subt   , 2.2, subcst$)

            print using L62825
            print using L62855, earnrate$, gtcst$, gthrs$, subcst$
            print skip(2)
            nbr_lines% = nbr_lines% + 4%

L36000: REM *************************************************************~
            * Print Direct & Misc cost dtls (JBVALUE2 'M'-type records) *~
            *************************************************************

            if pflag$(5%) = "N" then goto L37000  /* Direct & Misc. Cost */

            plowkey$ = all(hex(00))
            str(plowkey$,,9) = str(jobnme$) & "M"

            subt   = 0
            totcst = 0

            secthdr$ = "DIRECT & MISC COST DETAILS FOR JOB: " & jobnme$  ~
                        & " " & jobdesc$

        read_jbvalue2_direct
            call "PLOWNEXT" (#04, plowkey$, 9%, f1%(4))
               if f1%(4) = 0% then goto L36500
            get #04 using L36210,postdate$, user$, totcost, cost(),       ~
                posttext$

L36210:         FMT  /* File #04- JBVALUE2- Direct/Misc record type 'M'*/~
                     POS(10), CH(6), POS(30), CH(3), 13*PD(14,4), CH(40)

            call "DATEFMT" (postdate$)
            totcst = totcst + totcost
            call "CONVERT" (totcost,2.2,gtcst$)

            pflag$ = pflag$(5%) : gosub section_control
            if ret% = 0% then L36350

            print using L62910            /* Direct & Misc column header */
            print using L62925                             /* Underscore */
            nbr_lines% = nbr_lines% + 2%

L36350:     print using L62940,postdate$, posttext$, gtcst$, user$,       ~
                               bucket$, subcst$
            nbr_lines% = nbr_lines% + 1%
            if pflag$(5%) = "D" then gosub print_cost_breakdown
            nitems% = nitems% + 1%
            goto read_jbvalue2_direct

L36500
*       *** Now to Subtotal and move on ....
            if nitems% = 0% then L37000

            nitems% = 0%
            call "CONVERT" (totcst, 2.2, gtcst$)
            call "CONVERT" (subt  , 2.2, subcst$)
            print using L62925
            print using L62955, gtcst$, subcst$
            print skip(2)
            nbr_lines% = nbr_lines% + 2%

L37000: REM *************************************************************~
            * REM JBCREDIT File Lookup for JBNBR$ and get credit        *~
            * Transactions for this job                                 *~
            *************************************************************

            if pflag$(6%) = "N" then goto L39000 /* Credit Cost Flag */

            subt    = 0
            totcst  = 0
            totqty  = 0

            plowkey$ = all(hex(00))
            str(plowkey$,,8) = str(jobnme$) & hex(00)

            secthdr$ = "COST CREDIT TRANSACTIONS FOR JOB : " & jobnme$ & ~
                        " " & jobdesc$

        read_jbcredit
            call "PLOWNEXT"(#6, plowkey$, 8%, f1%(6))
               if f1%(6) = 0% then goto L37300

            get #6 using L37125,postdate$, part$, store$, lot$, qtymake,  ~
                                totcost, cost(), tcost(), qtyrever,      ~
                                text$, user$, cstflag$

L37125:     FMT /* JBCREDIT FILE - Credit Transactions File   */         ~
            POS(9), CH(6), POS(23), CH(25), CH(3), CH(6), POS(71),       ~
            PD(14,4), PD(14,4),12*PD(14,4), 12*PD(14,4), POS(375),       ~
            PD(14,4), POS(391), CH(40), CH(3), CH(1)

            mat cost = cost + tcost
            call "DATEFMT" (postdate$)
            temp = qtymake
            if qtyrever > 0 then temp = temp - qtyrever                  ~
                            else temp = temp + qtyrever
            totqty = totqty + temp
            totcst = totcst + totcost
            call "CONVERT" (temp, 2.2, gtqty$)
            call "CONVERT" (totcost, 2.2, gtcst$)
            gosub store_lot

            pflag$ = pflag$(6%) : gosub section_control
            if ret% = 0% then L37235

            print using L63010
            print using L63025
            nbr_lines% = nbr_lines% + 2%

L37235:     if qtyrever = 0 then L37240
                gosub print_reversed_detail
                goto L37255
L37240:     print using L63040,postdate$, part$, text$, gtcst$, gtqty$,   ~
                               prtstore$, user$,bucket$,subcst$
            nbr_lines% = nbr_lines% + 1%
L37255:     if pflag$(6%) = "D" then gosub print_cost_breakdown
            nitems% = nitems% + 1%
            goto read_jbcredit

L37300
*       *** Now to Subtotal and move on ....
            if nitems% = 0% then L37500

            nitems% = 0%
            call "CONVERT" (totcst, 2.2, gtcst$)
            call "CONVERT" (totqty, 2.2, gtqty$)
            call "CONVERT" (subt  , 2.2, subcst$)

            print using L63025
            print using L63055, gtcst$, gtqty$, subcst$
            print skip(2)
            nbr_lines% = nbr_lines% + 4%

L37500: REM *************************************************************~
            * Print Core Credit Details     (JBVALUE2 'Y'-type records) *~
            *************************************************************

            plowkey$ = all(hex(00))
            str(plowkey$,,9) = str(jobnme$) & "Y"

            subt   = 0
            totcst = 0

            secthdr$ = "CORE CREDIT DETAILS FOR JOB: " & jobnme$ & " " & ~
                        jobdesc$

        read_jbvalue2_y
            call "PLOWNEXT" (#04, plowkey$, 9%, f1%(4))
               if f1%(4) = 0% then goto L37700
            get #04 using L37595,postdate$, user$, totcost, cost(),       ~
                posttext$

L37595:         FMT  /* File #04- JBVALUE2- Direct/Misc record type 'M'*/~
                     POS(24), CH(6), POS(30), CH(3), 13*PD(14,4), CH(40)

            call "DATEFMT" (postdate$)
            totcst = totcst + totcost
            call "CONVERT" (totcost,2.2,gtcst$)

            pflag$ = pflag$(6%) : gosub section_control
            if ret% = 0% then L37665

            print using L63110            /* Direct & Misc column header */
            print using L63125                             /* Underscore */
            nbr_lines% = nbr_lines% + 2%

L37665:     print using L63140,postdate$, posttext$, gtcst$, user$,       ~
                               bucket$, subcst$
            nbr_lines% = nbr_lines% + 1%
            if pflag$(6%) = "D" then gosub print_cost_breakdown
            nitems% = nitems% + 1%
            goto read_jbvalue2_y

L37700
*       *** Now to Subtotal and move on ....
            if nitems% = 0% then L38000

            nitems% = 0%
            call "CONVERT" (totcst, 2.2, gtcst$)
            call "CONVERT" (subt  , 2.2, subcst$)
            print using L63125
            print using L63155, gtcst$, subcst$
            print skip(2)
            nbr_lines% = nbr_lines% + 2%

L38000: REM *************************************************************~
            * Print Core Credit Memo Details        (JBVALUE2 'F'-type) *~
            *************************************************************

            plowkey$ = all(hex(00))
            str(plowkey$,,9) = str(jobnme$) & "F"

            subt   = 0
            totcst = 0

            secthdr$ = "CORE CREDIT MEMO DETAILS FOR JOB: " & jobnme$ &  ~
                       " " &  jobdesc$

        read_jbvalue2_f
            call "PLOWNEXT" (#04, plowkey$, 9%, f1%(4))
               if f1%(4) = 0% then goto L38200
            get #04 using L38095,postdate$, user$, totcost, cost(),       ~
                posttext$

L38095:         FMT  /* File #04- JBVALUE2- Direct/Misc record type 'M'*/~
                     POS(10), CH(6), POS(30), CH(3), 13*PD(14,4), CH(40)

            call "DATEFMT" (postdate$)
            totcst = totcst + totcost
            call "CONVERT" (totcost,2.2,gtcst$)

            pflag$ = pflag$(6%) : gosub section_control
            if ret% = 0% then L38165

            print using L63210            /* Direct & Misc column header */
            print using L63225                             /* Underscore */
            nbr_lines% = nbr_lines% + 2%

L38165:     print using L63240,postdate$, posttext$, gtcst$, user$,       ~
                               bucket$, subcst$
            nbr_lines% = nbr_lines% + 1%
            if pflag$(6%) = "D" then gosub print_cost_breakdown
            nitems% = nitems% + 1%
            goto read_jbvalue2_f

L38200
*       *** Now to Subtotal and move on ....
            if nitems% = 0% then L38300

            nitems% = 0%
            call "CONVERT" (totcst, 2.2, gtcst$)
            call "CONVERT" (subt  , 2.2, subcst$)
            print using L63225
            print using L63255, gtcst$, subcst$
            print skip(2)
            nbr_lines% = nbr_lines% + 2%

L38300: REM *************************************************************~
            * Print Closing Adjustments     (JBVALUE2 'C'-type records) *~
            *************************************************************

            plowkey$ = all(hex(00))
            str(plowkey$,,9) = str(jobnme$) & "C"

            subt   = 0
            totcst = 0

            secthdr$ = "CLOSING ADJUSTMENT DETAILS FOR JOB: " & jobnme$ &~
                       " " & jobdesc$

        read_jbvalue2_c
            call "PLOWNEXT" (#04, plowkey$, 9%, f1%(4))
               if f1%(4) = 0% then goto L38500
            get #04 using L38395,postdate$, user$, totcost, cost(),       ~
                posttext$

L38395:         FMT  /* File #04- JBVALUE2- Direct/Misc record type 'M'*/~
                     POS(10), CH(6), POS(30), CH(3), 13*PD(14,4), CH(40)

            call "DATEFMT" (postdate$)
            totcst = totcst + totcost
            call "CONVERT" (totcost,2.2,gtcst$)

            pflag$ = pflag$(6%) : gosub section_control
            if ret% = 0% then L38465

            print using L63310            /* Direct & Misc column header */
            print using L63325                             /* Underscore */
            nbr_lines% = nbr_lines% + 2%

L38465:     print using L63340,postdate$, posttext$, gtcst$, user$,       ~
                               bucket$, subcst$
            nbr_lines% = nbr_lines% + 1%
            if pflag$(6%) = "D" then gosub print_cost_breakdown
            nitems% = nitems% + 1%
            goto read_jbvalue2_c

L38500
*       *** Now to Subtotal and move on ....
            if nitems% = 0% then L38600

            nitems% = 0%
            call "CONVERT" (totcst, 2.2, gtcst$)
            call "CONVERT" (subt  , 2.2, subcst$)
            print using L63325
            print using L63355, gtcst$, subcst$
            print skip(2)
            nbr_lines% = nbr_lines% + 2%

L38600: REM *************************************************************~
            * Print Core Closing Adj. Det.  (JBVALUE2 'Z'-type records) *~
            *************************************************************

            plowkey$ = all(hex(00))
            str(plowkey$,,9) = str(jobnme$) & "Z"

            subt   = 0
            totcst = 0

            secthdr$ = "CORE CLOSING ADJUSTMENT DETAILS FOR JOB: " &     ~
                        jobnme$ & " " & jobdesc$

        read_jbvalue2_z
            call "PLOWNEXT" (#04, plowkey$, 9%, f1%(4))
               if f1%(4) = 0% then goto L38800
            get #04 using L38695,postdate$, user$, totcost, cost(),       ~
                posttext$

L38695:         FMT  /* File #04- JBVALUE2- Direct/Misc record type 'M'*/~
                     POS(10), CH(6), POS(30), CH(3), 13*PD(14,4), CH(40)

            call "DATEFMT" (postdate$)
            totcst = totcst + totcost
            call "CONVERT" (totcost,2.2,gtcst$)

            pflag$ = pflag$(6%) : gosub section_control
            if ret% = 0% then L38765

            print using L63410            /* Direct & Misc column header */
            print using L63425                             /* Underscore */
            nbr_lines% = nbr_lines% + 2%

L38765:     print using L63440,postdate$, posttext$, gtcst$, user$,       ~
                               bucket$, subcst$
            nbr_lines% = nbr_lines% + 1%
            if pflag$(6%) = "D" then gosub print_cost_breakdown
            nitems% = nitems% + 1%
            goto read_jbvalue2_z

L38800
*       *** Now to Subtotal and move on ....
            if nitems% = 0% then L39000

            nitems% = 0%
            call "CONVERT" (totcst, 2.2, gtcst$)
            call "CONVERT" (subt  , 2.2, subcst$)
            print using L63425
            print using L63455, gtcst$, subcst$
            print skip(2)
            nbr_lines% = nbr_lines% + 2%

L39000: REM *************************************************************~
            * End of report ...                                         *~
            *************************************************************

            m% = 0%
            for i% = 1% to 3%
                if err%(i%) <> 0% then m% = m% + 1%
            next i%
            if m% = 0% then L39190

            if new_page% = 1% then gosub page_heading
            if nbr_lines% + m% + 1% > max_lines% then gosub page_heading

            if err%(1%) = 1% then print using L64720, jobnme$
            if err%(2%) = 1% then print using L64740, jobnme$
            if err%(3%) = 1% then print using L64740, jobnme$
            print skip(1)
            prntd% = 1%

L39190:     if prntd% = 0% then return
            time$ = " "  : call "TIME" (time$)
            print using L64620, jobnme$, time$   /* End of REPORT */
            printed% = printed% + 1%
            return

        REM *************************************************************~
            * Misc Processing Subroutines                               *~
            *************************************************************

        section_control
            ret% = 0%
            totcost = cost(1%)
            m% = 1%
            for i% = 2% to n%
                totcost = totcost + cost(i%)
                if cost(i%) <> 0 then m% = m% + 1%
            next i%
            subt = subt + totcost

            if pflag$ = "D" then L39505
               call "CONVERT" (totcost, 2.2, subcst$)
               bucket$ = "SUMMARY"
               call "RJUSTIFY" (bucket$)
               m% = 1%
               goto L39535

L39505:     subcst$, bucket$ = " "
            if cost(1%) = 0 then L39535
               call "CONVERT" (cost(1%), 2.2, subcst$)
               bucket$ = str(bcktid$(1%),,10)
               call "RJUSTIFY" (bucket$)

L39535:     if new_page% = 1% then gosub page_heading
            if nitems% > 0% then L39590

            if nbr_lines% + m% + 4% > max_lines% then gosub page_heading

            print using L62210, secthdr$
            print skip (1)
            nbr_lines% = nbr_lines% + 2%
            prntd% = 1%
            goto L39610

L39590:     if nbr_lines% + m% <= max_lines% then L39615

            gosub page_heading

L39610:     ret% = 1%
L39615:     return

        store_lot
            prtstore$ = " "
            if str(store$,,1) = "*"                                      ~
               then prtstore$ = str(store$,,3) & lot$                    ~
               else prtstore$ = str(store$,,3) & " " & lot$
            return

        REM SUBROUTINE RETURNING STATUS OF JBTIF FILE LOOKUP
        err_chk

            mat err% = zer

            str(chk_job$,,9) = jobnme$
            call "PLOWALTS" (#07, chk_job$, 1%, 9%, result%)
               if result% > 0% then err%(1) = 1%

            str(chk_job$,,9) = str(jobnme$) & "B"
            call "PLOWALTS" (#07, chk_job$, 1%, 9%, result%)
               if result% > 0% then err%(2) = 1%

            str(chk_job$,,9) = str(jobnme$) & "X"
            call "PLOWALTS" (#07, chk_job$, 1%, 9%, result%)
               if result% > 0% then err%(3) = 1%

            return /* non-zero status = transactions pending/or errors */

        print_reversed_detail
            if qtyrever > 0 then qtyrever = - qtyrever
            call "CONVERT" (qtyrever, -2.2, qtyrever$)
            call "CONVERT" (qtymake , -2.2, qtymake$ )
            temp$ = qtymake$  :  str(qtymake$,1%,1%) = " "
            str(qtymake$,2%,9%) = temp$
            print using L63040,postdate$, part$, text$, " ", qtymake$ ,   ~
                               " ", " ", " ", " "
            print using L63040, " "     , " "  , " "  , " ", qtyrever$,   ~
                               " ", " ", " ", " "
            print using L63040, " "     , " "  , " "  , gtcst$, gtqty$,   ~
                               prtstore$, user$,bucket$,subcst$

            nbr_lines% = nbr_lines% + 3%
            return

        REM *************************************************************~
            *        I N P U T / E D I T   F O R   P A G E   1          *~
            *-----------------------------------------------------------*~
            * Input/ Edit for Screen 1 of Input.                        *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
            if edit% = 2% then L40220
            init(hex(84)) lfac$()

           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                 (4)Previous Field   (14)View JOB "&~
                    "NUMBERS      (15)Print Screen"
           pf$(3), line1$ = " "
           pfkey$ = hex(0001040d0e0f)
           if fieldnr% > 1% then L40450
                str(pf$(2),18,18) = " "
                str(pf$(3),64) = "(16)Return      "
                str(pfkey$,3,1) = hex(10)
                goto L40450

L40220:  if fieldnr% > 0% then L40390     /* Edit Mode- Select Field    */
           init(hex(86)) lfac$()
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                     (14)View JOB "&~
                    "NUMBERS      (15)Print Screen"
           if pd$ <> "D" then                                            ~
           pf$(3) = "                                                  "&~
                    "             (16)Print Report"                      ~
                         else                                            ~
           pf$(3) = "                                                  "&~
                    "             (16)Display Rpt "
           pfkey$ = hex(0001040d0e0f10)
           if pos(str(pflag$(),,6) <> "N") <> 0% then L40450
           errormsg$ = "At Least One Print Section Must Be Selected"
           goto L40410

L40390:     REM Edit Mode, Field Enabled
                init(hex(8c)) lfac$()
L40410:         pf$(3) = " "
                pfkey$ = hex(0001040d0e0f10)


L40450:     str(pf$(3),63,1) = hex(84)
*          IF FIELDNR%>0% AND FIELDNR%<10% THEN LFAC$(FIELDNR%)=HEX(81)
            if fieldnr% = 1%  then str(lfac$(),1%,1%) = hex(81)
            if fieldnr% = 2%  then str(lfac$(),2%,8%) = hex(81)
            if senabled% = 0% then str(lfac$(),8%,1%) = hex(8c)

            str(line1$,62) = "JBQCDTLS: " & str(cms2v$)
L40520:     accept                                                       ~
               at (01,02), "Print Detailed / Summary Cost Information",  ~
               at (01,66), "Today: ",                                    ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line1$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
              at (06,32), fac(hex(ac)), header$(1%)             , ch(15),~
              at (06,50), fac(hex(ac)), header$(2%)             , ch(15),~
                                                                         ~
              at (07,02), "Job Number Range",                            ~
              at (07,32), fac(lfac$( 1)), beg_job$(1%)          , ch(08),~
              at (07,50), fac(lfac$( 1)), end_job$(1%)          , ch(08),~
                                                                         ~
              at (08,02), "Summary Page",                                ~
              at (08,32), fac(lfac$( 2)), pflag$(1%)            , ch(01),~
                                                                         ~
              at (09,02), "Material Costs",                              ~
              at (09,32), fac(lfac$( 3)), pflag$(2%)            , ch(01),~
                                                                         ~
              at (10,02), "Labor Costs",                                 ~
              at (10,32), fac(lfac$( 4)), pflag$(3%)            , ch(01),~
                                                                         ~
              at (11,02), "Work Center Costs",                           ~
              at (11,32), fac(lfac$( 5)), pflag$(4%)            , ch(01),~
                                                                         ~
              at (12,02), "Direct & Misc. Costs",                        ~
              at (12,32), fac(lfac$( 6)), pflag$(5%)            , ch(01),~
                                                                         ~
              at (13,02), "Credited Costs/Closing Adj.",                 ~
              at (13,32), fac(lfac$( 7)), pflag$(6%)            , ch(01),~
                                                                         ~
              at (14,02), "Job Status",                                  ~
              at (14,32), fac(lfac$( 8)), pflag$(7%)            , ch(01),~
                                                                         ~
              at (17,02), "Print or Display",                            ~
              at (17,32), fac(lfac$( 9)), pd$                   , ch(01),~
                                                                         ~
              at (08,40), "Cost Option Selections:",                     ~
              at (09,40), " S = Summary, D = Detail, N = No Print.",     ~
                                                                         ~
              at (14,40), "A = All, O = All Open, C = Closed,",          ~
              at (15,40), "I = Open Incomplete, D = Open Complete.",     ~
                                                                         ~
              at (17,40), "P = Print, D = Display",                      ~
                                                                         ~
              at (21,02), fac(hex(a4)),   inpmessage$          , ch(79), ~
              at (22,02), fac(hex(8c)),   pf$(1%)               , ch(79),~
              at (23,02), fac(hex(8c)),   pf$(2%)               , ch(79),~
              at (24,02), fac(hex(8c)),   pf$(3%)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

               if keyhit% = 13% then call "MANUAL" ("JBQCDTLS")
               if keyhit% = 15% then call "PRNTSCRN"
               if keyhit% = 13% or keyhit% = 15% then L40520
               if keyhit% <> 14% then L41070
                  chk_job$ = " "
                  call "GETCODE" (#02, chk_job$, jobdesc$, 0%, 0, f1%(4))
                  if chk_job$ <> " " then beg_job$(1) = chk_job$
                     goto L40520

L41070:         if edit% <> 2% then return
                close ws
                call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
                return


        REM *************************************************************~
            *               E D I T S   F O R   P A G E   1             *~
            *-----------------------------------------------------------*~
            * Checks Entries for Screen 1 of Input.                     *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50180,         /* Job Number Range */~
                                    L50230,         /* Summary Page     */~
                                    L50280,         /* Material         */~
                                    L50330,         /* Labor Page       */~
                                    L50380,         /* WRK Center       */~
                                    L50430,         /* Direct & Misc.   */~
                                    L50480,         /* Credit           */~
                                    L50530,         /* Job Status Flag  */~
                                    L50600          /* Print or Display */
                  return

L50180:     REM VALIDATE INPUT FOR JOB # RANGE
                call "TESTRNGE"(beg_job$(1%), end_job$(1%), beg_job$(2%),~
                                   end_job$(2%), errormsg$)
                if beg_job$(1) <> end_job$(1) then L50203
                   pflag$(7%) = "A" : senabled% = 0% : return
L50203:         senabled% = 1%
                return

L50230:     REM Must be 'blank' 'S/ummary' or 'D'etail for Summary page
            if pos("YNSD"=pflag$(1%)) <> 0% then L50280
               errormsg$ = "Entry Must Be 'N' 'S' or 'D'"

L50280:     REM Must be 'N/o, 'S/ummary' or 'D/etail' for Material Costs
            if pos("SDN"=pflag$(2%)) <> 0% then L50330
               errormsg$ = "Entry Must Be 'N' 'S' or 'D'"

L50330:     REM TEST DATA FOR Labor Costs Page Option
            if pos("SDN"=pflag$(3%)) <> 0% then L50380
               errormsg$ = "Entry Must Be 'N' 'S' or 'D'"

L50380:     REM TEST DATA FOR Work Center Costs
            if pos("SDN"=pflag$(4%)) <> 0% then L50430
               errormsg$ = "Entry Must Be 'N' 'S' or 'D'"

L50430:     REM TEST DATA FOR Direct and Misc. Costs
            if pos("SDN"=pflag$(5%)) <> 0% then L50470
               errormsg$ = "Entry Must Be 'N' 'S' or 'D'"
L50470:
L50480:     REM TEST DATA FOR Credit Page
            if pos("SDN"=pflag$(6%)) <> 0% then L50530
               errormsg$ = "Entry Must Be 'N' 'S' or 'D'"

L50530:     REM TEST DATA FOR Job Status Flag
            if pos("AOICD"=pflag$(7%)) <> 0% then L50600
            if errormsg$ = " " then                                      ~
               errormsg$ = "Entry Must Be 'A', 'O', 'I', 'D', or 'C'"

L50600:     REM TEST DATA FOR Print or Display
            if pos("PD"=pd$) <> 0% then return
            if errormsg$ = " " then                                      ~
               errormsg$ = "Entry Must Be 'P' or 'D'"
               return

        REM *************************************************************~
            *           C O M M O N   S U B R O U T I N E S             *~
            *************************************************************

        page_heading

        REM DECIDE IF PAGE SUBTOTALS SHOULD BE PRINTED
            if need_page0% = 1% then gosub opt_list

            page_nbr% = page_nbr% + 1% : nbr_lines% = 0% : new_page% = 0%

            print page
            print using L62110, date$, time$, coname$, "-" & rptid$
            print using L62125, userid$, page_nbr%
            print using L62140, subhdr$
            print skip (2)
            return

        print_cost_breakdown
            for i% = 2% to n%
                if cost(i%) = 0 then goto L60260
                bucket$ = bcktid$(i%)
                call "RJUSTIFY" (bucket$)
                call "CONVERT" (cost(i%), 2.2, subcst$)
                print using L64520, bucket$, subcst$
                nbr_lines% = nbr_lines% + 1%
L60260:     next i%
            return

        cost_breakdown
            if nbr_lines% + n% + 4% > max_lines% then gosub page_heading
            if do_cores% <> 0% then L60320
               print using L62410
               print using L62420
               goto L60340
L60320:     if do_cores% = 1% then print using L62440                     ~
                              else print using L62445
               print using L62460
L60340:     nbr_lines% = nbr_lines% + 2%

            for i% = 1% to n%
            bucket$ = bcktid$(i%)
            print using L62430, bucket$, col1$(i%), col2$(i%), col3$(i%), ~
                  col4$(i%), col5$(i%), col6$(i%), col7$(i%), col8$(i%)
            nbr_lines% = nbr_lines% + 1%
            next i%

            call "CONVERT" (col1, 2.2, col1$)
            call "CONVERT" (col2, 2.2, col2$)
            call "CONVERT" (col3, 2.2, col3$)
            call "CONVERT" (col4, 2.2, col4$)
            call "CONVERT" (col5, 2.2, col5$)
            call "CONVERT" (col6, 2.2, col6$)
            call "CONVERT" (col7, 2.2, col7$)
            if do_cores% <> 0% then call "CONVERT" (col8, 2.2, col8$)

            if do_cores% = 0% then print using L62420                     ~
                              else print using L62460
            print using L62430, "***** TOTALS", col1$, col2$, col3$,      ~
                               col4$, col5$, col6$, col7$, col8$

            print skip(2)
            nbr_lines% = nbr_lines% + 4%
            return

        opt_list
            print page
            print using L62110, date$, time$, coname$, "-" & rptid$
            print using L62125, userid$, page_nbr%
            print
            print using L64820
            print using L64830
            print using L64840, beg_job$(1), end_job$(1)
            print using L64850, pflag$(1)
            print using L64860, pflag$(2)
            print using L64870, pflag$(3)
            print using L64880, pflag$(4)
            print using L64890, pflag$(5)
            print using L64900, pflag$(6)
            print using L64910, pflag$(7)
            print using L64920
            print
            print using L64930, pd$
            print
            print using L64940

            need_page0% = 0%
            time$ = " "  : call "TIME" (time$)
            return

        REM *************************************************************~
            *         P R I N T   L I N E   F O R M A T S               *~
            *************************************************************

*       **** PAGE_HEADING Images *****

L62110: %Run Date: ######## @ ########      #############################~
        ~###############################                      JBQCDTLS####~
        ~###
L62125: %Run By: ###                                D E T A I L   J O B  ~
        ~ C O S T   R E P O R T                                    Page: #~
        ~###
L62140: %                  ##############################################~
        ~#################################################

*       **** General Section Header Image *****

L62210: % ***** #########################################################~
        ~######################################

*       **** Summary Heading Images *****

L62310: %Part Number to Build      Description                    # To Ma~
        ~ke  Qty Compl   Qty Left  Unit Cost Total Cost     Credit Adjustm~
        ~ent
L62325: %------------------------- ----------------------------- --------~
        ~-- ---------- ---------- ---------- ---------- ---------- -------~
        ~---
L62340: %######################### ############################# ########~
        ~## ########## ########## ########## ########## ########## #######~
        ~###
L62355: %                                                                ~
        ~       Core Information: ########## ########## ########## #######~
        ~###
L62370: %                                                                ~
        ~            Job Balance:                                  #######~
        ~###

*       **** COST_BREAKDOWN Images *****

L62410:     % BKT ID          MATERIAL    LABOR/WC  DIR & MISC   TOTAL CO~
        ~ST      CREDIT  ADJUSTMENT     BALANCE
L62420:     % ------------  ----------  ----------  ----------  ---------~
        ~--  ----------  ----------  ----------
L62430:     % ############  ##########  ##########  ##########   ########~
        ~##  ##########  ##########  ##########   ##########
L62440:     % BKT ID          MATERIAL    LABOR/WC  DIR & MISC   TOTAL CO~
        ~ST      CREDIT  ADJUSTMENT  WIP / CORE  WIP BALANCE
L62445:     % BKT ID         CORE MATL  CORE M/VAL   CORE COST  CORE CRED~
        ~IT  CORE / WIP    CORE ADJ    CORE WIP  JOB BALANCE

L62460:     % ------------  ----------  ----------  ----------  ---------~
        ~--  ----------  ----------  ----------  -----------


*       **** Material Ledger Images *****

L62510: %   Date     Part Number               Description               ~
        ~         Total Cost   Qty Moved Str Lot    By   Bucket ID     Amo~
        ~unt
L62525: %   -------- ------------------------- --------------------------~
        ~------ ------------  ---------- --- ------ --- ---------- -------~
        ~---
L62540: %   ######## ######################### ##########################~
        ~###### ############  ########## ########## ### ########## #######~
        ~###
L62555: %                                                                ~
        ~       ############  ##########                           #######~
        ~###

*       **** Indirect Core Detail images *****

L62610: %   Date     Description                                         ~
        ~         Total Cost                  By         Bucket ID     Amo~
        ~unt
L62625: %   -------- ----------------------------------------            ~
        ~        -----------                  ---       ---------- -------~
        ~---
L62640: %   ######## ########################################            ~
        ~       ############                  ###       ########## #######~
        ~###
L62655: %                                                                ~
        ~       ############                                       #######~
        ~###

*       **** Labor Detail Images *****

L62710: %   Date     Employee     W/C  Earn Type                        R~
        ~ate      Total Cost        Hours     By         Bucket ID     Amo~
        ~unt
L62725: %   -------- ------------ ---- ------------               -------~
        ~---    ------------   ----------    ---       ---------- --------~
        ~---
L62740: %   ######## ############ #### ############             #########~
        ~###    ############   ##########     ###       ########## #######~
        ~###
L62755: %                                                       #########~
        ~###    ############   ##########                          #######~
        ~###

*       **** W/C Detail Images *****

L62810: %   Date     W/C  Acty                                          R~
        ~ate      Total Cost        Hours     By         Bucket ID     Amo~
        ~unt
L62825: %   -------- ---- ----                                    -------~
        ~---    ------------   ----------     ---       ---------- -------~
        ~---
L62840: %   ######## #### ####                                  #########~
        ~###    ############   ##########     ###       ########## #######~
        ~###
L62855: %                                                       #########~
        ~###    ############   ##########                          #######~
        ~###

*       **** Direct & Misc. Detail images *****

L62910: %   Date     Description                                         ~
        ~         Total Cost                  By         Bucket ID     Amo~
        ~unt
L62925: %   -------- ----------------------------------------            ~
        ~        -----------                  ---       ---------- -------~
        ~---
L62940: %   ######## ########################################            ~
        ~       ############                  ###       ########## #######~
        ~###
L62955: %                                                                ~
        ~       ############                                       #######~
        ~###

*       **** Credit Detail Images *****

L63010: %   Date     Part Number               Description               ~
        ~       Total Credit   Qty Moved Str Lot    By   Bucket ID     Amo~
        ~unt
L63025: %   -------- ------------------------- --------------------------~
        ~------ ------------  ---------- --- ------ --- ---------- -------~
        ~---
L63040: %   ######## ######################### ##########################~
        ~###### ############  ########## ########## ### ########## #######~
        ~###
L63055: %                                                                ~
        ~       ############  ##########                           #######~
        ~###

*       **** Core Credit Detail images *****

L63110: %   Date     Description                                         ~
        ~       Total Credit                  By         Bucket ID     Amo~
        ~unt
L63125: %   -------- ----------------------------------------            ~
        ~        -----------                  ---       ---------- -------~
        ~---
L63140: %   ######## ########################################            ~
        ~       ############                  ###       ########## #######~
        ~###
L63155: %                                                                ~
        ~       ############                                       #######~
        ~###

*       **** Core Credit Memo Detail images *****

L63210: %   Date     Description                                         ~
        ~       Total Credit                  By         Bucket ID     Amo~
        ~unt
L63225: %   -------- ----------------------------------------            ~
        ~        -----------                  ---       ---------- -------~
        ~---
L63240: %   ######## ########################################            ~
        ~       ############                  ###       ########## #######~
        ~###
L63255: %                                                                ~
        ~       ############                                       #######~
        ~###

*       **** Closing Adj. Detail images *****

L63310: %   Date     Description                                         ~
        ~       Total Adjustment              By         Bucket ID     Amo~
        ~unt
L63325: %   -------- ----------------------------------------            ~
        ~        -----------                  ---       ---------- -------~
        ~---
L63340: %   ######## ########################################            ~
        ~       ############                  ###       ########## #######~
        ~###
L63355: %                                                                ~
        ~       ############                                       #######~
        ~###

*       **** Core Closing Adj. Detail images *****

L63410: %   Date     Description                                         ~
        ~       Total Adjustment              By         Bucket ID     Amo~
        ~unt
L63425: %   -------- ----------------------------------------            ~
        ~        -----------                  ---       ---------- -------~
        ~---
L63440: %   ######## ########################################            ~
        ~       ############                  ###       ########## #######~
        ~###
L63455: %                                                                ~
        ~       ############                                       #######~
        ~###

*       **** PRINT_COST_BREAKDOWN Images *****

L64520: %                                                                ~
        ~                                               ########## #######~
        ~###

*       **** End of Job Images *****

L64620: %                                   ***** END OF REPORT FOR JOB (~
        ~########) @ ######## *****

L64650: %                                     ***** END OF JOB COST DETAI~
        ~L REPORT @ ######## *****

*       **** Transaction Warning Images *****

L64720: %    THERE ARE TRANSACTIONS PENDING FOR JOB# ( ######## )        ~
        ~RE-RUN THIS PROCEDURE FOR CURRENT COST INFORMATION
L64740: %    THERE ARE UNRESOLVED JOB TRANSACTIONS FOR JOB# ( ########)  ~
        ~RUN SHOP FLOOR ADMINISTRATIVE FUNCTIONS

*       *** OUTPUT SELECTION IMAGES *****

L64820: %                     OUTPUT SELECTION LIST
L64830: % ----------------------------------------------------------------
L64840: %   Job Range                    From: ########   To: ########
L64850: %   Summary page option                #
L64860: %   Material cost option               #
L64870: %   Labor cost option                  #
L64880: %   Work center cost option            #
L64890: %   Direct & misc. cost option         #
L64900: %   Credit cost / Closing Adj. option  #
L64910: %   Job status option                  #
L64920: %    (A = All, O = Open, I = Incomplete, D = Complete, C = Closed)
L64930: %   Print or Display option            #
L64940: % ----------------------------------------------------------------


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

        endit
            if printed% <> 0% then L65214
L65170:     u3% = 2%
            call "ASKUSER" (u3%, "  NO REPORT PRINTED  ",                ~
               "Sorry, no report was generated using the input criteria",~
                 " ", "Press PF-16 to acknowledge")
            if u3% <> 16% then L65170
               goto L65220

L65214:     time$ = " "  : call "TIME" (time$)
            print using L64650, time$

L65220:     call "GETPRTNM" addr(pf$, pl$, pv$)
            close printer
            if pd$ <> "D" then L65230
               call "SET" addr("PM", pm$)
L65230:     call "SETPRNT" (rptid$, " ", 0%, 1%)
            init(" ") beg_job$(), end_job$()
            if pd$ <> "D" then inputmode
            if printed% = 0% then inputmode
            close ws
            call "PUTPARM" addr("E", "INPUT   ",4%,                      ~
                                               "FILE    ", pf$    ,  8%, ~
                                               "LIBRARY ", pl$    ,  8%, ~
                                               "VOLUME  ", pv$    ,  6%, ~
                                               "ACCESS  ", "PRINT ", 6%, ~
                                               "@", ret%)
            call "LINK" addr("DISPLAY ","S"," "," ",0%," "," ",0%,"N",   ~
                             u3%, ret%)
            call "SCRATCH" addr("F", pf$, pl$, pv$, " ", " ", ret%)
            goto inputmode

        endit2
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            end
