        REM *************************************************************~
            *                                                           *~
            *  IIIII  N   N  V   V  DDDD   DDDD   IIIII   SSS   PPPP    *~
            *    I    NN  N  V   V  D   D  D   D    I    S      P   P   *~
            *    I    N N N  V   V  D   D  D   D    I     SSS   PPPP    *~
            *    I    N  NN   V V   D   D  D   D    I        S  P       *~
            *  IIIII  N   N    V    DDDD   DDDD   IIIII   SSS   P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * INVDDISP - Display HNYDETAL Info for Part Specified.      *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/29/86 ! Original                                 ! ERN *~
            * 04/08/86 ! Standard Cost Project Modifications      ! ERN *~
            * 05/15/87 ! Added Specific Detail Display Option     ! HES *~
            * 05/26/88 ! Added Restart logic to reset to 'ALL'    ! MJB *~
            * 01/06/89 ! Added Previous screen and selection by   ! ERN *~
            *          !  transaction type.  Redid 5/26/88 changes!     *~
            *          !  so that PF-1 resets to incomming args.  !     *~
            * 12/04/91 ! Added the ability to display infomations ! SID *~
            *          !  from the archives files (PICKYEAR)      !     *~
            * 03/20/92 ! PRR 11361.  Added GETCODE for Trans Code.! JDH *~
            *          ! PRR 12019.  Fields for Rpt Totals bigger.!     *~
            *          ! PRR 12151.  2nd line Rpt Params for date.!     *~
            *          ! PRR 11298.  Added descr. for Trans "JR". !     *~
            * 09/17/93 ! Added support for trans code 'IZ'        ! WPH *~
            * 06/27/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************~
            * 03/03/06 ! (PAR000) CR347 Mod for New part Number   ! RHH *~
            *          !                                          !     *~
            *************************************************************

        sub "INVDDISP"   (part_key$,     /* Part Number to Display     */~
                          request$,      /* Display or Report          */~
                          dat1$, dat2$,  /* From/to Dates (unfmtd)     */~
                          str1$, str2$,  /* From/To Stores             */~
                          lot1$, lot2$,  /* From/To Lots               */~
                          tcc1$,         /* Selected Trans Code        */~
                          #1,            /* INVMASTR Channel   (PAR000)*/~
                          #2 )           /* INVDETAL Channel   (PAR000)*/

*        PART Number must be on file for Display or the subroutine whips
*          right back to the caller.  The Caller must open the files.
*
*        REQUEST- 'D' for Display and Part Level Reporting
*                 'R' for Report Only (Part may be passed blank).
*
*        Ranges passed in set up the initial display defaults.
*          Leave blank to have them set to "ALL".  If invalid they will
*          be set to "ALL".  Also Valid are 'FIRST', 'LAST', and
*          values.



        dim                                                              ~
            arcyear$7,                   /* Archived Year Litteral     */~
            blankdate$8,                 /* Blank date for comparison  */~
            choice$4,                    /* PICKYEAR return choice     */~
            company$60,                  /* Company Name for Report    */~
            ctlpart$45,                  /* Control Break for Report   */~
            cursor%(2%),                 /* Cursor location for edit   */~
            dat$(4%,2%)10,               /* Date Range Info            */~
            dat1$10,dat2$10,             /* Date Range per Caller      */~
            date$8,                      /* Date for screen display    */~
            dfac$(15%)1,                 /* Display Facs               */~
            dsply$(16%)79,               /* Display Strings            */~
            descr$32, descr1$79,         /* Part Description           */~
            errormsg$79,                 /* Error message              */~
            fileid$4,                    /* File ID for PICKYEAR       */~
            hdr$79,                      /* Screen Column Headings     */~
            hnydate$8,                   /* Post Date                  */~
            hnydprname$8,                /* Current HNYDETAL File Name */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            keys$(16%)62,                /* Keys to HNYDETAIL for recal*/~
            lastpart$45,                 /* Last Part Described        */~
            lfac$1,                      /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            lot$6, lot$(4%,2%)6,         /* Lot Info                   */~
            lot1$6, lot2$6,              /* Lot Range per Caller       */~
            n$(7%)10,                    /* Print some Numbers         */~
            part$45, part$(2%,2%)45,     /* Part Number, Ranges        */~
            part1$45,                    /* Part Number for Reports    */~
            partin$25,                   /* Part Number from Caller    */~
            partin_sub$20,               /* New Part Number    (PAR000)*/~
            part_key$45,                 /* New Part Number    (PAR000)*/~
            pf$(3%)79, pfkeys$20,        /* PF Keys                    */~
            pf4$(500%)17,                /* PF-4 Stack                 */~
            plowkey$99, plowkey1$99,     /* Misc Use Plow keys         */~
            request$1,                   /* What to Do (D/R)           */~
            rptdescr$128,                /* Report Select Criteria     */~
            rptdescr2$128,               /* Report Select Criteria #2  */~
            runtime$8,                   /* Report Run Time            */~
            str$3, str$(4%,2%)5,         /* Store Info                 */~
            str1$3, str2$3,              /* Store Range per Caller     */~
            t$(3%)12,                    /* Print some Totals          */~
            tc$2,                        /* Transaction Code           */~
            tcc$(2%)3, tcc1$3,           /* Transaction Code Criteria  */~
            tcdescr$32,                  /* Transaction Code Descriptio*/~
            temp$4,                      /* Temp Variable              */~
            text$40,                     /* Hny Text                   */~
            tdate$8,                     /* Transaction System Date    */~
            ttime$5,                     /* Transaction System Time    */~
            ttemp$8,                     /* Transaction System Time    */~
            user$3                       /* User ID                    */

        dim f1%(32%),                    /* = 1 if READ was successful */~
            f2%(32%),                    /* = 0 if the file is open    */~
            fs%(32%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "REV:01.00 03/03/06 New Finished Goods Part Number"
                                                           /* (PAR000) */
            partin$     = str(part_key$,1%,25%)
            partin_sub$ = str(part_key$,26%,20%)

                                                           /* (PAR000) */
        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! INVMASTR ! Inventory Master File            (PAR000)*~
            * #2  ! INVDETAL ! Inventory Detail File            (PAR000)*~
            * #6  ! WORKAREA ! Work File for Transaction Codes          *~
            *************************************************************~

            select #6,  "WORKAREA",                                      ~
                        varc,     indexed,  recsize =  50,               ~
                        keypos = 1, keylen = 3

*        Open workfile and fill with Transaction Codes & Descriptions
            call "WORKOPEN" (#6, "IO   ", 20%, 1%)
            write #6, using L02230, "ALL", "Indicates all Codes."
L02230:         FMT CH(3), CH(47)
            write #6, using L02230, "IA ", "Inventory Additions"
            write #6, using L02230, "IS ", "Inter-store Movements"
            write #6, using L02230, "IC ", "Cycle Count Adjustments"
            write #6, using L02230, "IP ", "Physical Inventory Adjustments"
            write #6, using L02230, "IW ", "Inventory Withdrawals"
            write #6, using L02230, "JB ", "Job By-products"
            write #6, using L02230, "JC ", "Job Completions"
            write #6, using L02230, "JK ", "Job Kitting"
            write #6, using L02230, "JT ", "Job Scrap"
            write #6, using L02230, "PR ", "PO/QC to Rework"
            write #6, using L02230, "PO ", "Purchase Order Receiving"
            write #6, using L02230, "PQ ", "QC to On-hand"
            write #6, using L02230, "RT ", "A/R Transactions"
            write #6, using L02230, "VT ", "A/P Transactions"
            write #6, using L02230, "IQ ", "Inventory to Projects"
            write #6, using L02230, "JR ", "Movement To/From Rework Jobs"
            write #6, using L02230, "IZ ", "Part to Part Conversion"

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            part$ = part_key$
            if arcyear$ = " " then arcyear$ = "Current"
            date$ = date
            call "DATEFMT" (date$)
            str(line2$,62) = "INVDDISP: " & str(cms2v$,,8)

            hdr$ = "Str" & hex(ac) & "Lot No" & hex(ac) & " Posted " &   ~
                   hex(ac) & " Quantity " & hex(ac) & "Inv. Value"   &   ~
                   hex(ac) & "       Descriptive Text              " &   ~
                   hex(8c)

            call "COMPNAME" (12%, company$, u3%)

*        Set up and test Ranges per caller's input
        reset_to_original
            oneshot% = 1%

            if len(dat1$) <> 6% then dat1$ = " "
	    if dat1$ = blankdate$ then dat1$ = " "
            if dat1$ = " " then dat$(1,1) = "ALL" else dat$(1,1) = dat1$
                dat$(1,2) = dat2$
L09260:         errormsg$ = " "
                gosub test_dates
                if errormsg$ = " " then L09300
                     dat$(1,1) = "ALL" : goto L09260
L09300:     if str1$ = " " then str$(1,1) = "ALL" else str$(1,1) = str1$
                str$(1,2) = str2$
L09320:         errormsg$ = " "
                gosub test_stores
                if errormsg$ = " " then L09360
                     str$(1,1) = "ALL" : goto L09320
L09360:     if lot1$ = " " then lot$(1,1) = "ALL" else lot$(1,1) = lot1$
                lot$(1,2) = lot2$
L09380:         errormsg$ = " "
                gosub test_lots
                if errormsg$ = " " then L09420
                     lot$(1,1) = "ALL" : goto L09380
L09420:     tcc$(1)  = tcc1$ : if tcc$(1) = " " then tcc$(1) = "ALL"
            oneshot% = 0%

L09450
*        If we are doing a Report then let's branch for it now.
            if request$ <> "R" then L09540
                part$(1%,1%) = part$
                if part$(1%,1%) = " " then part$(1%,1%) = "ALL"
                if part$(1%,1%) = "ALL" then part$(1%,2%) = " " else         ~
                                           part$(1%,2%) = part$(1%,1%)
                goto report_request

*        The rest of this is for Display Mode.
                                                       /* (PAR000)   */
L09540:     call "READ100" (#1, part_key$, f1%(1%))
            if f1%(1) = 0% then exit_program
                get #1 using L09570, part_key$, descr$
L09570:              FMT CH(45), CH(32)

                part$       = part_key$
                partin$     = str(part_key$,1%,25%)
                partin_sub$ = str(part_key$,26%,20%)

                
                str(line2$,,61%) = "Part: " & partin$ & " " & partin_sub$
                                                       /* (PAR000)     */  
            plowkey$ = part_key$
            call "PLOWNEXT" (#2, plowkey$, 45%, f1%(2%))
            if f1%(2%) = 1% then L10000

            if arcyear$ <> "Current" and f1%(2%) <> 1% then L09640 else L09900
L09640:         u3% = 2%
                call "ASKUSER" (u3%, "INVDETAL DISPLAY",                 ~
                   "There are no Movement Details for this Part",        ~
                   "for " & arcyear$ & " Archived Year",                 ~
                   "Press RETURN to return to the Current HNYDETAL file")
                if u3% <> 0% then L09640
                goto exit_program

L09900:         u3% = 2%
                call "ASKUSER" (u3%, "INVDETAL DISPLAY",                 ~
                          "There are no Movement Details for this Part", ~
                          "Press PF 1 to Select From Archive Files",     ~
                          "Or RETURN to continue...")
                if u3% = 1% then select_archive_year
                goto exit_program

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
                                                           /* (PAR000)  */
            init(" ") errormsg$, inpmessage$, dsply$(), pf4$() : pf4% = 0%
            plowkey$ = str(part_key$,,45%) & str(str$(2%,1%)) & lot$(2%,1%)
            gosub load_screen
                                                           /* (PAR000)  */
*        Main Screen: Get Display Options
        main_screen
            gosub set_pf_main_screen
            gosub'101               /* Display & Accept Screen    */
                if keyhit%  =  0% then more_detail
                if keyhit%  =  1% then reset_to_original
                if keyhit%  =  2% then first_screen
                if keyhit%  =  3% then select_archive_year
                if keyhit%  =  4% then prev_screen
                if keyhit%  =  5% then next_screen
                if keyhit%  =  8% then calc_totals
                if keyhit%  = 10% then modify_criteria
                if keyhit%  = 14% then print_report
                if keyhit%  = 16% then exit_program
                goto main_screen


        more_detail
            u3% = cursor%(1) - 4%
            if u3% < 1% or u3% > 15% then main_screen
            call "READ100" (#2, keys$(u3%), f1%(2%))
L10300:         if f1%(2) = 0 then main_screen
                if str(key(#2,0),,45%) <> part$ then main_screen
                get #2 using L10330, str$, lot$, hnydate$, tc$
L10330:              FMT XX(45), CH(3), CH(6), POS(63), CH(6), CH(2)
                if str$ >  str$(2,2) then main_screen   /* DONE   */
                if lot$ <= lot$(2,1) or lot$ > lot$(2,2) then L10430
                if hnydate$<= dat$(2,1) or hnydate$> dat$(2,2) then L10430
                if tcc$(1) = "ALL" then L10390
                    if str(tc$,,len(tcc$(1))) <> tcc$(1) then L10430
L10390:     gosub load_detail
            gosub format_detail
            gosub'103
            if keyhit% <> 7% then main_screen
L10430:     call "READNEXT" (#2, f1%(2))
            goto L10300
                                                            /* (PAR000) */ 
        first_screen
            plowkey$ = str(part_key$,,45%) & str(str$(2%,1%)) & lot$(2%,1%)
            init (" ") pf4$() : pf4% = 0%
            gosub load_screen
            goto  main_screen
                                                            /* (PAR000) */
        next_screen:
            if dsply$(1%) = " " or str(dsply$(1%),,6%) = "** END" then L10570
                pf4%       = pf4% + 1%
                pf4$(pf4%) = str(keys$(1%),26%)
                pf4$(pf4%) = addc all(hex(ff))
L10570:     gosub load_screen
            goto  main_screen
                                                       /* (PAR000) */
        prev_screen
            if pf4% = 0% then main_screen
                plowkey$ = str(part_key$,,45%) & pf4$(pf4%)
                pf4%     = pf4% - 1%
                gosub load_screen
                goto  main_screen
                                                       /* (PAR000) */

        calc_totals: /* Display totals for criteria specified          */
            inpmessage$ = "Enter Selection Criteria for Totals."
            opt% = 1%
            gosub save_display
            gosub get_criteria
                if keyhit%  =  1% then restore_display
            gosub load_totals
            gosub set_pf_totals
L10760:     gosub'101
                if keyhit%  =  0% then restore_display
                if keyhit%  = 16% then exit_program
                goto L10760


        modify_criteria:  /* Change Display Parameters                 */
            inpmessage$ = "Enter new Display Criteria."
            opt% = 2%
            gosub save_display
            gosub get_criteria
                if keyhit%  =  1% then restore_display
                goto first_screen

        print_report:
            inpmessage$ = "Enter Selection Criteria for Report."
            opt% = 3%
            gosub save_display
            gosub get_criteria
                if keyhit%  =  1% then restore_display
            oneshot% = 1%
            part$(1%,1%), part$(1%,2%) = part$ : gosub test_part_ranges
            oneshot% = 0%
            gosub report_printing
            goto  restore_display


        get_criteria  /* Get Selection Criteria and Return   */
L11040:     gosub set_pf_criteria
            gosub'101
                if keyhit%  =  1% then return
                if keyhit%  = 16% then return clear all
                if keyhit%  = 16% then exit_program
                if keyhit% <>  0% then L11040
            gosub test_display_ranges
                if errormsg$ <> " " then L11040
            return


        save_display:  /* Save Display and Criteria for start over     */
            dsply$(16) = dsply$(1)
            dat$(3,1) = dat$(1,1) : dat$(3,2) = dat$(1,2)
            dat$(4,1) = dat$(2,1) : dat$(4,2) = dat$(2,2)
            str$(3,1) = str$(1,1) : str$(3,2) = str$(1,2)
            str$(4,1) = str$(2,1) : str$(4,2) = str$(2,2)
            lot$(3,1) = lot$(1,1) : lot$(3,2) = lot$(1,2)
            lot$(4,1) = lot$(2,1) : lot$(4,2) = lot$(2,2)
            tcc$(2)   = tcc$(1)
            return

        restore_display:  /* Restore last display                      */
            dsply$(1) = dsply$(16)
            dat$(1,1) = dat$(3,1) : dat$(1,2) = dat$(3,2)
            dat$(2,1) = dat$(4,1) : dat$(2,2) = dat$(4,2)
            str$(1,1) = str$(3,1) : str$(1,2) = str$(3,2)
            str$(2,1) = str$(4,1) : str$(2,2) = str$(4,2)
            lot$(1,1) = lot$(3,1) : lot$(1,2) = lot$(3,2)
            lot$(2,1) = lot$(4,1) : lot$(2,2) = lot$(4,2)
            tcc$(1)   = tcc$(2)
            goto main_screen


        report_printing:  /* Produce Report per Criteria specified     */
            page% = 0% : line% = 857%
            p1, p2, p3, t1, t2, t3 = 0
            call "SHOSTAT" ("Printing Movements Report")
            call "SETPRNT" ("HNY031", " ", 0%, 0%)
            select printer (134)
            runtime$ = " "
            call "TIME" (runtime$)
            plowkey1$ = str(part$(2%,1%),,45%) & str(str$(2%,1%))
            if part$(1%,1%)  = "ALL" then rptdescr$ = "FOR ALL PARTS"
            if part$(1%,1%) <> "ALL" and part$(1%,1%) = part$(1%,2%) then      ~
                rptdescr$ = "FOR PART " & part$(1%,1%)
            if part$(1,1) <> "ALL" and part$(1%,1%) <> part$(1%,2%) then     ~
                rptdescr$ = "FOR PARTS " & part$(1%,1%) & " TO " &         ~
                            part$(1%,2%)
            if str$(1%,1%) = "ALL" then                                    ~
                rptdescr$ = rptdescr$ & ";  FOR ALL STORES"
            if str$(1,1) <> "ALL" and str$(1,1) = str$(1,2) then         ~
                rptdescr$ = rptdescr$ & ";  FOR STORE " & str$(1,1)
            if str$(1,1) <> "ALL" and str$(1,1) <> str$(1,2) then        ~
                rptdescr$ = rptdescr$ & ";  FOR STORES " & str$(1,1) &   ~
                            " TO " & str$(1,2)
            if lot$(1,1) = "ALL" then                                    ~
                rptdescr$ = rptdescr$ & ";  FOR ALL LOTS"
            if lot$(1,1) <> "ALL" and lot$(1,1) = lot$(1,2) then         ~
                rptdescr$ = rptdescr$ & ";  FOR LOT " & lot$(1,1)
            if lot$(1,1) <> "ALL" and lot$(1,1) <> lot$(1,2) then        ~
                rptdescr$ = rptdescr$ & ";  FOR LOTS " & lot$(1,1) &     ~
                            " TO " & lot$(1,2)
            if dat$(1,1) = "ALL" then                                    ~
                rptdescr2$ = "FOR ALL DATES"
            if dat$(1,1) <> "ALL" and dat$(1,1) = dat$(1,2) then         ~
                rptdescr2$ = "FOR DATE " & dat$(1,1)
            if dat$(1,1) <> "ALL" and dat$(1,1) <> dat$(1,2) then        ~
                rptdescr2$ = "FOR DATES " & dat$(1,1) & " TO " & dat$(1,2)
            rptdescr2$ = rptdescr2$ & ";  TRANSACTION CODE: " & tcc$(1)
            call "STRING" addr("CT", rptdescr$, 128%)
            call "STRING" addr("CT", rptdescr2$, 128%)
            first%, subhdr% = 1%

        report_loop:
            call "PLOWNEXT" (#2, plowkey1$, 0%, f1%(2))
            if f1%(2%) = 0% then end_report

            gosub load_detail
            if part1$ > part$(2%,2%) then end_report
            if str$ <= str$(2%,1%) or str$ > str$(2%,2%) then report_loop
            if lot$ <= lot$(2%,1%) or lot$ > lot$(2%,2%) then report_loop
            if hnydate$ <= dat$(2%,1%) or hnydate$ > dat$(2%,2%) then        ~
                                                             report_loop
            if tcc$(1) = "ALL" then L11820
                if str(tc$,,len(tcc$(1%))) <> tcc$(1%) then report_loop

L11820:     if first% <> 1% then L11860
                ctlpart$ = part1$
                first%   = 0%

L11860:     if part1$ <> ctlpart$ then gosub part_totals

            gosub format_detail
            if line%    > 53% then gosub page_heading
            if subhdr%  > 0%  then gosub sub_heading
            print using L13160, str$, lot$, n$(1), n$(2), n$(3), n$(4),   ~
                               n$(5), hnydate$, tc$, user$, text$
            p1 = p1 + qty       :  t1 = t1 + qty
            p2 = p2 + costext   :  t2 = t2 + costext
            p3 = p3 + priceext  :  t3 = t3 + priceext
            line%   = line% + 1%
            subhdr% = 0%
            goto report_loop

        load_detail:
            get #2 using L12040, part1$, str$, lot$, temp$, hnydate$, tc$,~
                                qty, cost, price, priceext, text$, user$,~
                                tdate$, stdcost
L12040:         FMT CH(45), CH(3), CH(6), XX(4), CH(4), CH(6), CH(2),    ~
                    4*PD(14,4), CH(40), CH(3), CH(6), PD(14,4)
           str(temp$,,4) = bool5 str(temp$,,4)
           tmp = val(str(temp$,,4), 4)
        return

        format_detail:
            priceext = round(qty * price, 2)
            costext  = round(qty * cost , 2)
            stdext   = round(qty * stdcost , 2)
            call "CONVERT" (qty     , 2.2, n$(1))
            call "CONVERT" (cost    , 4.4, n$(2))
            call "CONVERT" (costext , 2.2, n$(3))
            call "CONVERT" (price   , 2.2, n$(4))
            call "CONVERT" (priceext, 2.2, n$(5))
            call "CONVERT" (stdcost , 2.4, n$(6))
            call "CONVERT" (stdext  , 2.2, n$(7))
            call "CONVERT" (tmp     , 0.0, ttemp$)
            if price = 0 then n$(4), n$(5) = "       n/a"
            call "DATEFMT" (hnydate$)
            call "DATEFMT" (tdate$)
            ttime$ = str(ttemp$,,2) & ":" & str(ttemp$,3,2)
            tcdescr$ = "(Unknown Transaction Type)"
            if tc$ = "IA" then tcdescr$ = "Direct Inventory Addition"
            if tc$ = "IS" then tcdescr$ = "Inter-store Movement"
            if tc$ = "IC" then tcdescr$ = "Cycle Count Adjustment"
            if tc$ = "IP" then tcdescr$ = "Physical Adjustment"
            if tc$ = "IQ" then tcdescr$ = "Inventory to Project"
            if tc$ = "IW" then tcdescr$ = "Direct Inventory Withdrawal"
            if tc$ = "JB" then tcdescr$ = "Produced as a Job Byproduct"
            if tc$ = "JC" then tcdescr$ = "Produced by a Job"
            if tc$ = "JK" then tcdescr$ = "Kitted (issued) To a Job"
            if tc$ = "JR" then tcdescr$ = "To/From a Rework Job"
            if tc$ = "JT" then tcdescr$ = "Scrapped From A Job"
            if tc$ = "PR" then tcdescr$ = "Reworked From QC"
            if tc$ = "PO" then tcdescr$ = "Receipt From P.O."
            if tc$ = "PQ" then tcdescr$ = "Moved To On-Hand From QC"
            if tc$ = "RT" then tcdescr$ = "Invoice Through AR (sold)"
            if tc$ = "VT" then tcdescr$ = "Invoice Through AP w/no PO"
            if tc$ = "IZ" then tcdescr$ = "Part to Part Conversion"
        return

        end_report:
            if line% > 55% then gosub page_heading
            if part$(1%,1%) <> part$(1%,2%) then gosub part_totals
            call "CONVERT" (t1, 2.2, t$(1%))
            call "CONVERT" (t2, 2.2, t$(2%))
            call "CONVERT" (t3, 2.2, t$(3%))
            print using L13190
            print using L13240, "*TOTALS*", t$(1%), t$(2%), t$(3%)
            print
            print "** END OF REPORT **"
            close printer
            call "SETPRNT" ("HNY031", " ", 0%, 1%)
            return

        part_totals
            if first% = 1% then return
            call "CONVERT" (p1, 2.2, n$(1%))
            call "CONVERT" (p2, 2.2, n$(2%))
            call "CONVERT" (p3, 2.2, n$(3%))
            print using L13190
            print using L13220, "        ", n$(1%), n$(2%), n$(3%)
            print
            line%    = line% + 3%
            subhdr%  = 1%
            ctlpart$ = part1$  :  p1, p2, p3 = 0
            return

        page_heading
            page%    = page% + 1%
            line%    = 9%
            subhdr%  = 1%
            print page
            print using L12960, date$, runtime$, company$
            print using L12990, arcyear$, page%
            print using L13020, rptdescr$
            print using L13020, rptdescr2$
            print
            print using L13050
            print using L13080
            print using L13110
            return

        sub_heading
            if line% > 52% then gosub page_heading
            if lastpart$ = ctlpart$ then L12910
                call "GETCODE" (#1, ctlpart$, descr1$, 1%, 99, f1%(1))
                lastpart$ = ctlpart$
                descr1$ = ctlpart$ & " " & descr1$
L12910:     print using L13140, descr1$
            line% = line% + 1%
            return


L12960: %RUN DATE: ######## ########            #########################~
        ~###################################               INVDDISP-HNY031

L12990: %  ARCHIVE YEAR: #######                                INVENTORY~
        ~ MOVEMENTS LISTING                                    PAGE: ###

L13020: %    ############################################################~
        ~#################################################################

L13050: %        LOT              ----- V A L U E ----- ----- S A L E S -~
        ~----               USER

L13080: %   STR NUMBER  QUANTITY    PER UNIT  EXTENSION   PER UNIT  EXTEN~
        ~SION  POSTED   TC   ID   DESCRIPTIVE TEXT

L13110: %   --- ------ ---------- ---------- ---------- ---------- ------~
        ~---- --------  --  ----  ----------------------------------------

L13140: %################################################################~
        ~################

L13160: %   ### ###### ########## ########## ########## ########## ######~
        ~#### ########  ##   ###  ########################################

L13190: %              ----------            ----------            ------~
        ~----

L13220: % ##########   ##########            ##########            ######~
        ~####
L13240: % ########   ############          ############          ########~
        ~####

        REM *************************************************************~
            *        S T A N D A L O N E   R E P O R T                  *~
            *-----------------------------------------------------------*~
            * Get the parameters for producing a Report Request.        *~
            * Initial report parameters have already been set in the    *~
            * 9000s area.                                               *~
            *************************************************************
        report_request
            goto L16160

L16100
*        The 'first' screen is the 'Display Mode'.
L16110:     gosub'102
                if keyhit% = 16% then print_request
                if keyhit% = 32% then exit_program
                if keyhit% <> 0% then L16110

L16160
*        Get Changes to Report Parameters
L16170:     gosub'112
                if keyhit%  = 14% then select_archive_year
                if keyhit%  = 16% then exit_program
                if keyhit% <>  0% then L16170
            gosub test_report_ranges
                if errormsg$ <> " " then L16170 else L16100


        print_request
            gosub report_printing
            goto report_request


        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads the Next Screen's Worth of Data.                    *~
            *************************************************************
        load_screen:
            dsply$(), keys$() = " "
            l% = 0%
                                                           /* (PAR000) */
L30090:     call "PLOWNEXT" (#2, plowkey$, 45%, f1%(2%))
            if f1%(2%) = 1% then L30140
L30110:         l% = l% + 1%
                dsply$(l%) = "** END OF DATA **"
                return
L30140:     get #2 using L30150, str$, lot$, hnydate$, tc$,qty, amt, text$
L30150:         FMT XX(45), CH(3), CH(6), POS(63), CH(6), CH(2),         ~
                    2*PD(14,4), POS(103), CH(40)
                                                            /* (PAR000) */
            if str$ >  str$(2%,2%) then L30110         /* DONE   */
            if lot$     <= lot$(2%,1%) or lot$     > lot$(2%,2%) then L30090
            if hnydate$ <= dat$(2%,1%) or hnydate$ > dat$(2%,2%) then L30090
            if tcc$(1%) = "ALL" then L30220
                if str(tc$,,len(tcc$(1%))) <> tcc$(1%) then L30090

L30220:         l% = l% + 1%
                call "DATEFMT" (hnydate$)
                dsply$(l%) = str(str$) & " " & str(lot$) & " " & hnydate$
                call "CONVERT" (qty, 2.2, str(dsply$(l%),21%,10%))
                amt = round(qty * amt, 2) /* Total Cost */
                call "CONVERT" (amt, 2.2, str(dsply$(l%),32%,10%))
                str(dsply$(l%),43%) = text$
                keys$(l%) = key(#2, 0)
                if l% = 15% then return else goto L30090


        load_totals
            init (" ") dsply$(1)
            t1, t2 = 0 : c% = 0%
            plowkey1$ = str(part_key$,,45%) & str(str$(2%,1%)) & hex(00)

L30380:     call "PLOWNEXT" (#2, plowkey1$, 45%, f1%(2%))
            if f1%(2%) = 0% then L30530

            get #2 using L30150, str$, lot$, hnydate$, tc$, qty, amt, text$

            if str$ >  str$(2%,2%) then L30530         /* DONE   */
              if lot$     <= lot$(2%,1%) or lot$     > lot$(2%,2%) then L30380
              if hnydate$ <= dat$(2%,1%) or hnydate$ > dat$(2%,2%) then L30380
              if tcc$(1%) = "ALL" then L30480
                  if str(tc$,,len(tcc$(1%))) <> tcc$(1%) then L30380
L30480:              t1 = t1 + qty
                     t2 = t2 + round(qty * amt, 2)
                     c% = c% + 1%
                     goto L30380

L30530:     dsply$(1%) = "*TOTALS*"
            call "CONVERT" (t1, 2.2, str(dsply$(1%),21%,10%))
            call "CONVERT" (t2, 2.2, str(dsply$(1%),32%,10%))
            str(dsply$(1%),43%) = "Records Counted: #####"
            convert c% to str(dsply$(1%),60%,5%), pic(####0)
            return



        select_archive_year
            fileid$ = "HNYD"
            call "PICKYEAR" (fileid$, choice$)
              if f2%(2) = 0 then close #2 /* Close Curr. Detail File */
              if choice$ = "CURR" or choice$ = " " then L30670 else L30680
L30670:       hnydprname$ = "HNYDETAL" : goto L30690
L30680:       hnydprname$ = "HNYD" & choice$
L30690:       call "PUTPRNAM" addr(#2, hnydprname$)
              call "OPENCHCK" (#2, fs%(2), f2%(2), 0%, rslt$(2))
              if choice$ = "CURR" or choice$ = " "                       ~
               then arcyear$ = "Current" else arcyear$ = choice$ & "   "
            goto L09450  /* Start from the beginning */


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101                                  /* Display Screen   */

L40040:        if dat$(1%,1%) = " " then l40045
               call "DATUFMTC" (dat$(1%,1%))
               call "DATEOK" (dat$(1%,1%), 0%, " ")

L40045:        if dat$(1%,2%) = " " then l40050
               call "DATUFMTC" (dat$(1%,2%))
               call "DATEOK" (dat$(1%,2%), 0%, " ")

L40050:     accept                                                       ~
               at (01,02), "Display Part Movement Information",          ~
               at (01,40), "Archive Year:",fac(hex(8c)),arcyear$, ch(07),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), fac(hex(ac)), hdr$                   , ch(79),~
                                                                         ~
               at (05,02), fac(dfac$( 1)), dsply$( 1%)          , ch(79),~
               at (06,02), fac(dfac$( 2)), dsply$( 2%)          , ch(79),~
               at (07,02), fac(dfac$( 3)), dsply$( 3%)          , ch(79),~
               at (08,02), fac(dfac$( 4)), dsply$( 4%)          , ch(79),~
               at (09,02), fac(dfac$( 5)), dsply$( 5%)          , ch(79),~
               at (10,02), fac(dfac$( 6)), dsply$( 6%)          , ch(79),~
               at (11,02), fac(dfac$( 7)), dsply$( 7%)          , ch(79),~
               at (12,02), fac(dfac$( 8)), dsply$( 8%)          , ch(79),~
               at (13,02), fac(dfac$( 9)), dsply$( 9%)          , ch(79),~
               at (14,02), fac(dfac$(10)), dsply$(10%)          , ch(79),~
               at (15,02), fac(dfac$(11)), dsply$(11%)          , ch(79),~
               at (16,02), fac(dfac$(12)), dsply$(12%)          , ch(79),~
               at (17,02), fac(dfac$(13)), dsply$(13%)          , ch(79),~
               at (18,02), fac(dfac$(14)), dsply$(14%)          , ch(79),~
               at (19,02), fac(dfac$(15)), dsply$(15%)          , ch(79),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               at (22,44), fac(lfac$    ), dat$(1%,1%)          , ch(08),~
               at (22,55), fac(lfac$    ), dat$(1%,2%)          , ch(08),~
               at (22,77), fac(lfac$    ), tcc$(1%)             , ch(03),~
               at (23,44), fac(lfac$    ), str$(1%,1%)          , ch(05),~
               at (23,55), fac(lfac$    ), str$(1%,2%)          , ch(05),~
               at (24,44), fac(lfac$    ), lot$(1%,1%)          , ch(06),~
               at (24,55), fac(lfac$    ), lot$(1%,2%)          , ch(06),~
                   keys(pfkeys$),  key (keyhit%)

               call "DATEOKC" (dat$(1%,1%), 0%, " ")
               call "DATEOKC" (dat$(1%,2%), 0%, " ")

               if keyhit% <> 13 then L40250
                  call "MANUAL" ("HNYDDISP")
                  goto L40040

L40250:        if keyhit% <> 15 then L40270
                  call "PRNTSCRN"
                  goto L40040

L40270:        if keyhit% <> 0% then return
                   close ws
                   call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

        set_pf_main_screen:
           inpmessage$ = "Tab to Line & Press (RETURN) for DETAIL; PF-1" ~
                       & " Restart OR PF-3 to Select Year."
           init (hex(86)) dfac$()
           init (hex(84)) lfac$
           pf$(1) = "(2)First     !( 8)Display Totals   Dates: XXXXXXXX"&~
                    "- XXXXXXXX  Trans Code: XX   "
           pf$(2) = "(4)Previous  !(10)Change Display  Stores: XXXxx   "&~
                    " - XXXxx    !(15)Print Screen"
           pf$(3) = "(5)Next      !(14)Print Report      Lots: XXXXXX  "&~
                    " - XXXXXX   !(16)Exit Display"
           pfkeys$ = hex(0102030405ffff08ff0affff0d0e0f10ffffff00)
           if pf4% > 0% then L40360
               str(pf$(2%),,12%) = " "  :  str(pfkeys$,4%,1%) = hex(ff)
L40360:    for xl% = 1% to 15%
               if str(dsply$(xl%),,6%) = "** END" then L40380
           next xl%
           goto L40385
L40380:        str(pf$(3%),,12%) = " "  :  str(pfkeys$,5%,1%) = hex(ff)
L40385:    return

        set_pf_criteria:
           init (hex(8c)) dfac$()
           init (hex(81)) lfac$
           pf$(1) = "(1)Restart   !    Display Totals   Dates: XXXXXXXX"&~
                    "- XXXXXXXX  Trans Code: XX   "
           pf$(2) = "             !    Change Display  Stores: XXXxx   "&~
                    " - XXXxx    !(15)Print Screen"
           pf$(3) = "             !    Print Report      Lots: XXXXXX  "&~
                    " - XXXXXX   !(16)Exit Display"
           pfkeys$ = hex(01ffffffffffffffffffffff0dfe0f10ffffff00)
           str(pf$(opt%),18%,1%) = hex(84)
           str(pf$(opt%),34%,1%) = hex(8c)
           return

        set_pf_totals:
           inpmessage$ = "Press (RETURN) to Continue with Display."
           init (hex(9c)) dfac$()  :  dfac$(1) = hex(84)
           init (hex(84)) lfac$
           pf$(1) = "             !    Display Totals   Dates: XXXXXXXX"&~
                    "- XXXXXXXX  Trans Code: XX   "
           pf$(2) = "             !    Change Display  Stores: XXXxx   "&~
                    "- XXXxx     !(15)Print Screen"
           pf$(3) = "             !    Print Report      Lots: XXXXXX  "&~
                    "- XXXXXX    !(16)Exit Display"
           pfkeys$ = hex(01ffffffffffffffffffffff0dfe0f10ffffff00)
           str(pf$(1%),18%,1%) = hex(84)
           str(pf$(1%),34%,1%) = hex(8c)
           return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Displays transaction detail.                              *~
            *************************************************************

        deffn'103                                  /* Display Screen   */
            gosub set_pf_103

L41090:     accept                                                       ~
               at (01,02), "Display Part Movement Information",          ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (05,03), "Store Number",                               ~
               at (05,21), fac(hex(84)), str$                   , ch(03),~
                                                                         ~
               at (06,03), "Lot Number",                                 ~
               at (06,21), fac(hex(84)), lot$                   , ch(06),~
                                                                         ~
               at (07,03), "Transaction Date             Time",          ~
               at (07,21), fac(hex(84)), tdate$                 , ch(08),~
               at (07,37), fac(hex(84)), ttime$                 , ch(05),~
               at (07,46), "User",                                       ~
               at (07,51), fac(hex(84)), user$                  , ch(03),~
                                                                         ~
               at (09,03), "Posting Date",                               ~
               at (09,21), fac(hex(84)), hnydate$               , ch(08),~
                                                                         ~
               at (10,03), "Transaction Type",                           ~
               at (10,21), fac(hex(84)), tc$                    , ch(02),~
               at (10,25), fac(hex(84)), tcdescr$               , ch(32),~
                                                                         ~
               at (11,03), "Text/Description",                           ~
               at (11,21), fac(hex(84)), text$                  , ch(40),~
                                                                         ~
               at (13,03), "Quantity Information:",                      ~
               at (14,10), "Quantity",                                   ~
               at (14,21), fac(hex(84)), n$(1%)                 , ch(10),~
               at (15,10), "Cost Each",                                  ~
               at (15,21), fac(hex(84)), n$(2%)                 , ch(10),~
               at (15,36), "Extension",                                  ~
               at (15,46), fac(hex(84)), n$(3%)                 , ch(10),~
               at (16,10), "Price Each",                                 ~
               at (16,21), fac(hex(84)), n$(4%)                 , ch(10),~
               at (16,36), "Extension",                                  ~
               at (16,46), fac(hex(84)), n$(5%)                 , ch(10),~
               at (17,10), "Stnd Cost",                                  ~
               at (17,21), fac(hex(84)), n$(6%)                 , ch(10),~
               at (17,36), "Extension",                                  ~
               at (17,46), fac(hex(84)), n$(7%)                 , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
                   keys(pfkeys$),  key (keyhit%)

               if keyhit% <> 13 then L41640
                  call "MANUAL" ("HNYDDISP")
                  goto L41090

L41640:        if keyhit% <> 15 then L41680
                  call "PRNTSCRN"
                  goto L41090

L41680: set_pf_103
           inpmessage$ = "Press RETURN or PF(16) To Return To Summary" & ~
                         " Screen."
           init (hex(84)) lfac$
           pf$(1) = "(ENTER)Return               (7)Next Transaction   "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "             (16)Return      "
           pfkeys$ = hex(00070d0f10)
           return

        REM *************************************************************~
            *           R E P O R T   R E Q U E S T   S C R E E N       *~
            *-----------------------------------------------------------*~
            * Get Parameters for produced Standalone Report.            *~
            *************************************************************

        deffn'102                                  /* Display          */
            gosub set_pf_102
            goto L42130

        deffn'112                                  /* Edit             */
            gosub set_pf_112

L42130:     accept                                                       ~
               at (01,02), "Part Movement Detail Report",                ~
               at (01,40), "Archive Year:",fac(hex(8c)),arcyear$, ch(07),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Begin Part",                                 ~
               at (06,14), fac(lfac$    ), str(part$(1%,1%),1%,25%), ch(25),~
               at (06,40), fac(lfac$    ), str(part$(1%,1%),26%,20%),ch(20),~
                                                                         ~
               at (07,02), "End   Part",                                 ~
               at (07,14), fac(lfac$    ), str(part$(1%,2%),1%,25%), ch(25),~
               at (07,40), fac(lfac$    ), str(part$(1%,2%),26%,20%),ch(20),~
               at (08,02), "Dates",                                      ~
               at (08,14), fac(lfac$    ), dat$ (1%,1%)         , ch(10),~
               at (08,41), fac(lfac$    ), dat$ (1%,2%)         , ch(10),~
                                                                         ~
               at (09,02), "Stores",                                     ~
               at (09,14), fac(lfac$    ), str$ (1%,1%)         , ch(05),~
               at (09,41), fac(lfac$    ), str$ (1%,2%)         , ch(05),~
                                                                         ~
               at (10,02), "Lots",                                       ~
               at (10,14), fac(lfac$    ), lot$ (1%,1%)         , ch(08),~
               at (10,41), fac(lfac$    ), lot$ (1%,2%)         , ch(08),~
                                                                         ~
               at (11,02), "Trans Codes",                                ~
               at (11,14), fac(lfac$    ), tcc$ (1%)            , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                   keys(pfkeys$),  key (keyhit%)

               if keyhit% <> 13 then L42470
                  call "MANUAL" ("HNYDDISP")
                  goto L42130

L42470:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L42130

        set_pf_102
           inpmessage$ = "Press RETURN to Change Parameters, PF-16 to" & ~
                         " Print Report."
           init (hex(84)) lfac$
           pf$(1) = "                                                  "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                 (32)Exit Report F"&~
                    "unction      (16)Print Report"
           pfkeys$ = hex(ffffffffffffffffffffffff0dff0f1020ffff00)
           return

        set_pf_112
           inpmessage$ = "Enter Report Parameters then Press RETURN."
           init (hex(81)) lfac$
           pf$(1) = "                                                  "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                       (14)Select "&~
                    "Archive Year (16)Exit Report "
           pfkeys$ = hex(ffffffffffffffffffffffff0d0e0f10ffffff00)
           return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test Ranges Entered for Display and Report.               *~
            *************************************************************

        test_report_ranges
            errormsg$ = " "

        test_part_ranges
            call "TESTRNGE" (part$(1%,1%), part$(1%,2%),                     ~
                             part$(2%,1%), part$(2%,2%), errormsg$)
            if errormsg$ = " " then L50150
                errormsg$ = "Parts: " & errormsg$
                return
L50150:     if oneshot% = 1% then return

        test_display_ranges
            errormsg$ = " "

*        Test Dates (1st section for screen entries only)
            if len(dat$(1%,1%)) < 6% then L50250
                call "DATEOKC" (dat$(1%,1%), u3%, errormsg$)
                if errormsg$ <> " " then return
                call "DATUFMTC" (dat$(1%,1%))
L50250:     if len(dat$(1%,2%)) < 6% then test_dates
                call "DATEOKC" (dat$(1%,2%), u3%, errormsg$)
                if errormsg$ <> " " then return
                call "DATUFMTC" (dat$(1%,2%))
        test_dates
            call "TESTRNGE" (dat$(1%,1%), dat$(1%,2%), dat$(2%,1%), dat$(2%,2%), ~
                             errormsg$)
            if errormsg$ = " " then L50350
                errormsg$ = "Date: " & errormsg$
                return
L50350:     if len(dat$(1%,1%)) = 6% then call "DATFMTC" (dat$(1%,1%))
            if len(dat$(1%,2%)) = 6% then call "DATFMTC" (dat$(1%,2%))
            if oneshot% = 1% then return

        test_stores
            call "TESTRNGE" (str$(1%,1%), str$(1%,2%), str$(2%,1%), str$(2%,2%), ~
                             errormsg$)
            if errormsg$ = " " then L50450
                errormsg$ = "Stores: " & errormsg$
                return
L50450:     if oneshot% = 1% then return

        test_lots
            if lot$(1%,1%) <> " " or lot$(1%,2%) <> " " then L50520
                lot$(2%,1%) = hex(20202020201f) /* Lot blank is a lot--  */
                lot$(2%,2%) = " "               /* of what I don't know! */
                return
L50520:     call "TESTRNGE" (lot$(1%,1%), lot$(1%,2%), lot$(2%,1%), lot$(2%,2%), ~
                             errormsg$)
            if errormsg$ = " " then test_trans_codes
                errormsg$ = "Lots: " & errormsg$
                return

        test_trans_codes
            if tcc$(1%) = " " then tcc$(1%) = "ALL"
            tcdescr$ = hex(0684) & "Select Transaction Code."
            call "GETCODE" (#6, tcc$(1%), tcdescr$, 1%, .47, f1%(6%))
            if f1%(6%) = 0% then errormsg$ = "Please Enter Valid Code"
            return

        exit_program
            if arcyear$ = "Current" then L65210
                hnydprname$ = "HNYDETAL" : close #2
                call "PUTPRNAM" addr(#2, hnydprname$)
                call "OPENCHCK" (#2, fs%(2%), f2%(2%), 0%, rslt$(2%))
                arcyear$ = "Current"
L65210:     call "FILEBGON" (#6)
            end
