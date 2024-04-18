        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   TTTTT   CCC    CCC    OOO   M   M  PPPP   RRRR    *~
            *  S        T    C   C  C   C  O   O  MM MM  P   P  R   R   *~
            *   SSS     T    C      C      O   O  M M M  PPPP   RRRR    *~
            *      S    T    C   C  C   C  O   O  M   M  P      R   R   *~
            *   SSS     T     CCC    CCC    OOO   M   M  P      R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCCOMPR - This program will compare the total standard   *~
            *            cost between two cost sets and print a report  *~
            *            showing the variance.                          *~
            *----------------------------------------------------------Q*~
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
            * 04/17/87 ! Original                                 ! LKM *~
            * 10/30/87 ! Fixed call to HNYTOTSB sub-routine.      !     *~
            *          ! Added totals to Report.                  ! DAW *~
            * 03/30/88 ! Split into 2 reports and added options   ! MJB *~
            * 03/10/93 ! Added Core Value Coding for Reman Parts. ! JBK *~
            *          !  PRR 10065 - Page Number Expanded.       !     *~
            *          !  PRR 11217 - Added Subtotals by Category.!     *~
            *          !  PRR 11886 - Fixed Totaling Problems with!     *~
            *          !              Uneven Cost Sets.           !     *~
            *          !  PRR 11887 - Added Ability to include    !     *~
            *          !              Obsolete Items, Print range !     *~
            *          !              of Catagory Codes, Subtotals!     *~
            *          !              by Category.                !     *~
            *          !  Added Page 0 to the report.  $ variance !     *~
            *          !  values were truncating, now round.  If  !     *~
            *          !  no percent variance can be calculated as!     *~
            *          !  Cost1 is zero, then no variance % will  !     *~
            *          !  show.  Time added to end of report.     !     *~
            * 06/16/94 ! Added compare to HNYQUAN value. Added    ! JDH *~
            *          !  exception reporting % limit.            !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            avail%(2),                   /* Records Available Flags    */~
            begcat$4,                    /* Begin Category for Rnge Sel*/~
            begpart$25,                  /* Begin Part for Range Sel   */~
            catcode$4,                   /* Part Category Code         */~
            company$50,                  /* Company Header             */~
            core_inv_flag$1,             /* Core Value Inv Trans Flag  */~
            core_part$25,                /* Core Part Code             */~
            cost_method$1,               /* Part's Costing Method      */~
            cost1$10,                    /* Cost Set #1                */~
            cost2$10,                    /* Cost Set #2                */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            endcat$4,                    /* End Category for Range Sel */~
            endpart$25,                  /* Ending Part for Range Sel  */~
            eof%(2),                     /* End of STC files Flags     */~
            errormsg$79,                 /* Error message              */~
            ext1$12,                     /* Extension for 1st cost set */~
            ext2$12,                     /* Extension for 2nd cost set */~
            extvarpct$7,                 /* Extension Variance %%      */~
            file$(3)8,                   /* File names for STC files   */~
            from$4,                      /* Constant "FROM"            */~
            fromcat$4,                   /* From Category Code         */~
            hnyqkey$34,                  /* HNYQUAN Key                */~
            hnytot(1),                   /* On hand array for HNYTOTSB */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            inc_obsolete$1,              /* Include Obsolete Parts     */~
            lastcat$4,                   /* Last Category Code         */~
            lastpart$25,                 /* To test end of range       */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            lib$(3)8,                    /* Libraries for STC files    */~
            line2$79,                    /* Second Line of Screen Headr*/~
            obso$4,                      /* Obsolete Part Indicator    */~
            oh$10,                       /* On hand qty                */~
            partprnt$25,                 /* Print part number          */~
            partdesc$32,                 /* Print Description          */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            pf9$14,                      /* PF 9 Screen Literal        */~
            pf9k$1,                      /* PF 9 Key Enable            */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            printcat$5,                  /* Print for Category Code    */~
            print_limit$4,               /* Exception Reporting Limit  */~
            print_type$1,                /* Normal or Core Print Line  */~
            rpttype$1,                   /* Report Type Selected       */~
            runtime$8,                   /* Time Report Was Run        */~
            savecat$4,                   /* Holding Category Code      */~
            save_avail%(2),              /* Preserve AVAIL% Array      */~
            save_eof%(2),                /* Preserve EOF% Array        */~
            save_stccost(2),             /* Preserve STCCOST Array     */~
            set$1,                       /* Set identifier on report   */~
            setdesc$(2)32,               /* Set Descriptions           */~
            setname$(2)8,                /* Cost Set Name              */~
            sort$1,                      /* Sort by Part or Category   */~
            sortby$11,                   /* Sort literal for header    */~
            stccost(2),                  /* Standard Costs for the Sets*/~
            stckeys$(2)25,               /* Keys for the STC Files     */~
            sub_ext1$12,                 /* Subtotal for Set 1         */~
            sub_ext2$12,                 /* Subtotal for Set 2         */~
            sub_var2$12,                 /* Subtotal for Variance      */~
            to$2,                        /* Constant "TO"              */~
            tot_ext1$12,                 /* Extended total for set 1   */~
            tot_ext2$12,                 /* Extended total for set 2   */~
            tot_var2$12,                 /* Total variance of extension*/~
            userid$3,                    /* Current User Id            */~
            varpct$7,                    /* Percent of Change          */~
            var1$10,                     /* Variance between costs     */~
            var2$10,                     /* Variance between extension */~
            vol$(3)6,                    /* Volumes for STC files      */~
            workkey$29                   /* Workfile key               */

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
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
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
            * # 1 ! STCHNY1  ! Standard Cost Set- Inventory Standards   *~
            * # 2 ! STCHNY2  ! Standard Cost Set- Inventory Standards   *~
            * # 3 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 4 ! HNYMASTR ! Inventory Parts Master File              *~
            * # 5 ! CATEGORY ! Part Category File                       *~
            * # 6 ! HNYQUAN  ! Inventory Quantity File                  *~
            * # 9 ! WORKFILE ! Work File for sorting                    *~
            * #10 ! COREXREF ! Core Part Cross Reference File           *~
            * #11 ! STCHNY3  ! Standard Cost Set- Inventory Standards   *~
            * #12 ! STCHNY4  ! Standard Cost Set- Inventory Standards   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "STCHNY1",                                       ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos = 1,    keylen = 25

            select # 2, "STCHNY2",                                       ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos = 1,    keylen = 25

            select # 3, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select # 4, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select # 5, "CATEGORY",                                      ~
                        varc, indexed,                                   ~
                        recsize = 200,                                   ~
                        keypos  =   1, keylen = 4

            select # 6, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44          ~

            select # 9, "WORKFILE",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  29

            select #10, "COREXREF",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =   26, keylen =  50,                     ~
                        alt key  1, keypos =    1, keylen =  50,         ~
                            key  2, keypos =   76, keylen =  25, dup     ~

            select #11, "STCHNY3",                                       ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos = 1,    keylen = 25

            select #12, "STCHNY4",                                       ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos = 1,    keylen = 25

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#3, fs%(3%), f2%(3%), 0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%), 0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%), 0%, rslt$(5%))
            call "OPENCHCK" (#6, fs%(6%), f2%(6%), 0%, rslt$(6%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            from$ = "From"
            to$ = "To"

*        See if Core is on
            plowkey$ = "SWITCHS.COR"
            call "READ100" (#3, plowkey$, core_on%)
                if core_on% <> 1% then L10000
            get #3 using L09185, core_inv_flag$
L09185:         FMT POS(134), CH(1)
            if core_inv_flag$ <> "Y" then core_on% = 0%
            if core_on% <> 1% then L10000
                call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$ = " " : pf5$ = " " : pf16$ = "(16)Exit Program"
            gosub L29000

            for fieldnr% = 1% to 8%
                if fieldnr% > 1% then pf4$ = "(4)Previous Field"
L10120:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0% then L10240
L10140:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10220
L10170:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10140
                         if fieldnr% = 1% then L10120
                         goto L10170
L10220:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <>  0% then       L10140
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10140
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            pf4$  = " "
            pf5$  = " "
            pf16$ = "(16)Print Report"
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       print_report
                  if keyhit% <>  0% then       editpg1
L11160:     fieldnr% = cursor%(1%) - 7%
            if fieldnr% < 1% or fieldnr% > 8% then editpg1
            if fieldnr% = lastfieldnr% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
                  pf4$, pf5$, pf16$ = " "
L11220:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11220
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11220
                  lastfieldnr% = fieldnr%
            goto L11160

        REM *************************************************************~
            *             P R I N T    R E P O R T                      *~
            *************************************************************

        print_report
            gosub open_file
            if core_on% = 1% then gosub open_file_core
            if sort$ = "C" then call"WORKOPEN"(#9,"IO   ",recnbr%,f2%(9))
            if sort$ = "P" then sortby$ = "PART NUMBER"                  ~
                           else sortby$ = "CATEGORY"
            call "SHOSTAT" ("Printing Standard Cost Set Comparison")
            runtime$ = " "  :  call "TIME" (runtime$)
            call "COMPNAME" (12%, company$, u3%)
            page% = 0%  :  line% = 99%
            select printer(134)
            call "SETPRNT" ("STC006", " ", 0%, 0%)
            stckeys$(2%) = stckeys$(1%)
            hnyqkey$     = stckeys$(1%)

            tot_ext1$, tot_ext2$, tot_var2$ = " "
            sub_ext1$, sub_ext2$, sub_var2$ = " "
            tot_ext1,  tot_ext2,  tot_var2 = 0
            sub_ext1,  sub_ext2,  sub_var2 = 0
            mat eof%    = zer
            mat avail%  = zer
            mat stccost = zer

        report_loop
            partprnt$, partdesc$ = " "
            if actuals% <> 1% then L12240
                gosub data_load_2
                goto L12270
L12240:     wf% = 1%  :  gosub data_load
            wf% = 2%  :  gosub data_load

L12270:     if eof%(1%) = 1% and eof%(2%) = 1% then end_of_reads
            gosub set_the_variables

*        Check the part for obsolete or out of range
            call "DESCRIBE" (#4, partprnt$, partdesc$, 0%, f1%(4))
            get #4 using L12330, catcode$, obso$
L12330:         FMT POS(90), CH(4), POS(166), CH(4)
            if obso$ = "OBSO" and inc_obsolete$ <> "Y" then report_loop
            if catcode$ < fromcat$ or catcode$ > lastcat$ then report_loop


            call "HNYTOTSB" (partprnt$, " ", " ", hnytot(), 0%)

            if sort$ <> "C" then L12440
                if abs(varpct * 100) < print_limit then report_loop
                    gosub write_work
                    goto report_loop

L12440:     if rpttype$ <> "1" then L12500
                gosub unit_calculations
                if abs(varpct) < print_limit then report_loop
                    gosub print_detail
                    goto L12550

*        Report Type '5'
L12500:     gosub value_calculations
            if abs(extvarpct) < print_limit then report_loop
                gosub format_values
                gosub accumulate_totals
                gosub print_detail

L12550:     if core_on% = 1% then gosub core_values

            goto report_loop

        end_of_reads
            if sort$ = "C" then cat_report
            goto end_report

        print_detail
            if line% > 56% then gosub page_heading
            if print_type$ = "C" then L13445
            if rpttype$ = "1" then                                       ~
                print using L60200, partprnt$, partdesc$, set$, cost1$,   ~
                                   cost2$, var1$, varpct$                ~
                else                                                     ~
                print using L60490, partprnt$, partdesc$, cost1$, cost2$, ~
                                   oh$, ext1$, ext2$, var2$, extvarpct$
                goto L13470
L13445:     if rpttype$ = "1" then                                       ~
                print using L60200, "     Core Value:", core_part$, set$, ~
                                   cost1$, cost2$, var1$, varpct$        ~
                else                                                     ~
                print using L60490, "     Core Value:", core_part$,       ~
                                   cost1$, cost2$, oh$, ext1$, ext2$,    ~
                                   var2$, extvarpct$
L13470:     line% = line% + 1%
            return

        page_heading
            if page% = 0% then gosub print_params
            page% = page% + 1%
            print page
            print using L60040, date$, runtime$, company$
*          PRINT SKIP(1)
            if rpttype$ = "1" then print using L60060, page%              ~
                              else print using L60310, page%
*          PRINT SKIP(1)
            print using L60080, setname$(1), setdesc$(1), setname$(2),    ~
                               setdesc$(2), sortby$
            print skip(1)
            if rpttype$ = "1" then print using L60110                     ~
                              else print using L60370
            if rpttype$ = "1" then print using L60140                     ~
                              else print using L60410
            line% = 6%
            return

        end_report
            if rpttype$ = "1" then L13650
            if sort$ <> "C" then L13625
                if cat_count% < 1% then L13625
                     gosub format_sub_totals
                     gosub print_sub_totals
L13625:     call "CONVERT" (tot_ext1, 4.4, tot_ext1$)
            call "CONVERT" (tot_ext2, 4.4, tot_ext2$)
            call "CONVERT" (tot_var2, 4.4, tot_var2$)
            print using L60525
            print using L60540, tot_ext1$, tot_ext2$, tot_var2$
L13650:     print skip(2)
            runtime$ = " "  :  call "TIME" (runtime$)
            print using L60580, runtime$
            call "SETPRNT" ("STC006", " ", 0%, 1%)
            close printer
            call "FILEBGON" (#9)
            goto inputmode

        print_params
            print page
L13700:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L13720
                str(i$(), i%, 1%) = hex(20)
                goto L13700
L13720:     print using L60040, date$, runtime$, company$
            print using L60060, page%
            print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26)
            print "------------------------------------------------------~
        ~--------------------------"
            return

        write_work  /* Write work file for Category Sort */
            write #9 using L13850, catcode$, partprnt$, partdesc$, cost1, ~
                     cost2, variance1, varpct, hnytot(1%), set$
L13850:         FMT CH( 4), CH(25), CH(32), 5*PD(14,4), CH(1)
            return

        cat_report
            init (hex(00)) workkey$, savecat$  :  cat_count% = 0%
            call "READ104" (#9, workkey$, f1%(9))
                if f1%(9) = 0% then end_report
                goto L13910
        read_loop
            call "READNEXT" (#9, f1%(9))
                if f1%(9) = 0% then end_report
L13910:     get #9 using L13850, catcode$, partprnt$, partdesc$, cost1,   ~
                   cost2, variance1, varpct, hnytot(1%), set$
            if catcode$ <> " " then printcat$ = catcode$                 ~
                               else printcat$ = "BLANK"
            if catcode$ = savecat$ then L14000
                if line%  > 56% then gosub page_heading
                if rpttype$ = "1"  then L13960
                if cat_count% < 1% then L13960
                gosub format_sub_totals
                gosub print_sub_totals
L13960:         print skip(1)
                print using L60610, printcat$
                print skip(1)
                line% = line% + 3%
                savecat$ = catcode$
L14000:     if rpttype$ = "1" then L14030
                gosub value_calculations
                gosub format_values
                gosub accumulate_sub_totals
                gosub accumulate_totals
                goto L14035
L14030:     gosub unit_calculations
L14035:     gosub print_detail
            cat_count% = cat_count% + 1%
            if core_on% = 1% then gosub core_values
            goto read_loop

        set_the_variables
            if eof%(1%) = 1% then set_2
            if eof%(2%) = 1% then set_1
            if stckeys$(1%) = stckeys$(2%) then set_both
            if stckeys$(1%) > stckeys$(2%) then set_2

        set_1
            set$       = "1"
            cost1      = stccost(1%)
            cost2      = 0
            avail%(1%) = 0%
            partprnt$  = stckeys$(1%)
            variance1  = -cost1
            varpct     = -1
            return

        set_2
            set$       = "2"
            cost1      = 0
            cost2      = stccost(2%)
            avail%(2%) = 0%
            partprnt$  = stckeys$(2%)
            variance1  = cost2
            varpct     = 1
            return

        set_both
            set$       = "B"
            cost1      = stccost(1%)
            cost2      = stccost(2%)
            avail%(1%) = 0%
            avail%(2%) = 0%
            partprnt$  = stckeys$(1%)
            variance1  = 0
            varpct     = 0
            if cost1   = 0 and cost2 <> 0 then varpct = 1
            if cost2   = 0 and cost1 <> 0 then varpct = -1
            if cost1   = 0 and cost2 = 0 then return
            variance1  = -(cost1 - cost2)
            if varpct  = 0 then varpct = variance1 / cost1
            return

        unit_calculations
            varpct = 100 * varpct
            call "CONVERT" (variance1, 4.4, var1$)
            call "CONVERT" (varpct, 2.2, varpct$)
            call "CONVERT" (cost1, 4.4, cost1$)
            call "CONVERT" (cost2, 4.4, cost2$)
            if set$ = "1" then cost2$ = "      ----"
            if set$ = "2" then cost1$ = "      ----"
            if cost1 = 0  then varpct$ = "   ----"
            return

        value_calculations
            extvarpct = 0
            extension1 = cost1 * hnytot(1)
            extension2 = cost2 * hnytot(1)
            if set$ = "1" then variance2 = -extension1
            if set$ = "2" then variance2 =  extension2
            if set$ = "B" then variance2 = -(extension1 - extension2)
            if extension1 = 0 and extension2 <> 0 then extvarpct = 1
            if extension2 = 0 and extension1 <> 0 then extvarpct = -1
            if extension1 = 0 and extension2 =  0 then L15320
            if extvarpct = 0 then extvarpct = variance2 / extension1
            extvarpct = 100 * extvarpct * sgn(hnytot(1))
L15320:     return

        format_values
            call "CONVERT" (hnytot(1), 0.2, oh$)
            call "CONVERT" (variance2, 2.2, var2$)
            call "CONVERT" (extension1, 4.4, ext1$)
            call "CONVERT" (extension2, 4.4, ext2$)
            call "CONVERT" (extvarpct, 2.2, extvarpct$)
            call "CONVERT" (cost1, 4.4, cost1$)
            call "CONVERT" (cost2, 4.4, cost2$)
            if set$ = "1" then ext2$ = "      ------"
            if set$ = "2" then ext1$ = "      ------"
            if extension1 = 0 then extvarpct$ = "   ----"
            return

        accumulate_totals
            tot_ext1 = tot_ext1 + extension1
            tot_ext2 = tot_ext2 + extension2
            tot_var2 = tot_var2 + variance2
            return

        accumulate_sub_totals
            sub_ext1 = sub_ext1 + extension1
            sub_ext2 = sub_ext2 + extension2
            sub_var2 = sub_var2 + variance2
            return

        format_sub_totals
            call "CONVERT" (sub_ext1, 4.4, sub_ext1$)
            call "CONVERT" (sub_ext2, 4.4, sub_ext2$)
            call "CONVERT" (sub_var2, 4.4, sub_var2$)
            sub_ext1, sub_ext2, sub_var2 = 0
            cat_count% = 0%
            return

        print_sub_totals
            print using L60525
            print using L60630, sub_ext1$, sub_ext2$, sub_var2$
            print
            line% = line% + 3%
            if line% > 56% then gosub page_heading
            return

        REM *************************************************************~
            *  Test for Reman Part and Core Part and if a Core Part,    *~
            *  get the core costs and print them.                       *~
            *************************************************************
        core_values
            if core_on% <> 1% then return
            init (" ")  core_part$, plowkey$
            plowkey$ = partprnt$

            call "PLOWALTS" (#10, plowkey$, 0%, 25%, reman%)
                if reman% = 0% then return
            core_part$ = str(plowkey$,26%,25%)
            if core_part$ = " " then return

*        Save variables from regular path for later restoration
            mat save_avail%  = avail%
            mat save_eof%    = eof%
            mat save_stccost = stccost
            mat avail%  = zer
            mat eof%    = zer
            mat stccost = zer

*        Read the standard costs for the core part
            for i% = 11% to 12%
                if actuals% <> 1% or i% <> 11% then L16240
                     gosub core_actuals
                     goto L16300
L16240:         call "READ100" (#i%, core_part$, f1%(i%))
                     if f1%(i%) = 0% then eof%(i%-10%) = 1%
                     if f1%(i%) = 0% then L16300
                get #i% using L16280, stccost(i%-10%)
L16280:              FMT POS(52), PD(14,4)
                avail%(i%-10%) = 1%
L16300:     next i%

            if eof%(1%) = 1% and eof%(2%) = 1% then end_core_values

            gosub set_the_variables

            if rpttype$ <> "1" then L16410
                gosub unit_calculations
                goto L16460

*        Report Type '5'
L16410:     gosub value_calculations
            gosub format_values
            gosub accumulate_totals
            if sort$ = "C" then gosub accumulate_sub_totals

L16460:     print_type$ = "C"
            gosub print_detail
            print_type$ = " "

        end_core_values
            mat avail%  = save_avail%
            mat eof%    = save_eof%
            mat stccost = save_stccost
            return

*        Read the actual costs for the core part
        core_actuals
            plowkey$ = core_part$
            call "PLOWNEXT" (#06, plowkey$, 25%, f1%(6%))
                 if f1%(6%) = 0% then eof%(i%-10%) = 1%
                 if f1%(6%) = 0% then return
            get #06 using L16640, stccost(i%-10%)
L16640:          FMT POS(117), PD(14,4)
            avail%(i%-10%) = 1%
            return

        open_file   /* Open Std Cost Set and Set # Recs for Work File */
            for i% = 1% to 2%
                if actuals% = 1% and i% = 1% then L19260
                call "STCFOPEN" (setname$(i%), "Sxxxxx", #3, errormsg$,  ~
                                 #i%, #i%, #i%, #i%, #i%, #i%)
                if errormsg$ <> " " then exit_program
L19260:     next i%
            if actuals% = 1% then L19280
                call "GETNAMES" addr(#1%, file$(1), lib$(1), vol$(1))
L19280:     call "GETNAMES" addr(#2%, file$(2), lib$(2), vol$(2))
            call "GETNAMES" addr(#6%, file$(3), lib$(3), vol$(3))
            if actuals% = 1% then L19300
                call"READFDR"addr(file$(1), lib$(1), vol$(1), 0%, "RC",  ~
                                                              rec1%, u3%)
L19300:     call"READFDR"addr(file$(2),lib$(2),vol$(2),0%,"RC",rec2%,u3%)
            call"READFDR"addr(file$(3),lib$(3),vol$(3),0%,"RC",rec3%,u3%)
            if actuals% = 1% then rec1% = rec3%
            recnbr% = max(rec1%, rec2%) / 2%
            return

        open_file_core
            for i% = 11% to 12%
                if actuals% = 1% and i% = 11% then L19390
                call "STCFOPEN" (setname$(i%-10%), "Sxxxxx", #3,         ~
                                 errormsg$, #i%, #i%, #i%, #i%, #i%, #i%)
                if errormsg$ <> " " then exit_program
L19390:     next i%
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Part Number        */    ~
                              L20140,         /* Category Code      */    ~
                              L20200,         /* Cost Set #1        */    ~
                              L20300,         /* Cost Set #2        */    ~
                              L20400,         /* Report Type        */    ~
                              L20500,         /* Sort Sequence      */    ~
                              L20560,         /* Include Obsolete   */    ~
                              L20700          /* Exception Limit    */
            return

L20100: REM Def/Enable Part Number                 PART$
            if begpart$ <> " " then L20110
               begpart$ = "ALL"
               endpart$ = " "
L20110:     inpmessage$ = "Enter Part Number Range"
            return

L20140: REM Def/Enable Category Code               CATEGORY$
            if begcat$ <> " " then L20165
               begcat$ = "ALL"
               endcat$ = " "
L20165:     inpmessage$ = "Enter Category Code Range"
            return

L20200: REM Def/Enable Cost Set #1                 COST1$
            inpmessage$ = "Enter First Cost Set -or- PF9 for Actual Costs~
        ~ (Fixed Std Only)"
            return

L20300: REM Def/Enable Cost Set #2                 COST2$
            inpmessage$ = "Enter Second Cost Set"
            return

L20400: REM Def/Enable Report Type                 RPTTYPE$
            if actuals% = 0% then L20410
                rpttype$ = "5"
                inpmessage$ = "'Actuals' Report Type must be '5'."
                return
L20410:     inpmessage$ = "Enter '1' for Unit Cost Comparison, or '5' for~
        ~ Inventory Evaluation Comparison"
            if rpttype$ = " " then rpttype$ = "1"
            return

L20500: REM Def/Enable Sort Sequence               SORT$
            inpmessage$ = "Enter 'P' for by Part, or 'C' for by Category ~
        ~Code"
            if sort$ = " " then sort$ = "P"
            return

L20560: REM Def/Enable Include Obsolete Parts      INC_OBSOLETE$
            inpmessage$ = "Enter 'Y' to Include Obsolete Parts, or 'N' to~
        ~ Exclude Obsolete Parts"
            if inc_obsolete$ = " " then inc_obsolete$ = "N"
            return

L20700: REM Def/Enable Exception Reporting Limit   PRINT_LIMIT$
            inpmessage$ = "Enter the Percentage less than which to Exclud~
        ~e (Zero for No Exclusions)"
            if print_limit$ = " " then print_limit$ = " 0.0"
            return

L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, rpttype$, sort$,           ~
                      print_limit$,                                      ~
                      begpart$               , /* Beginning Number   */  ~
                      endpart$               , /* Ending Part Number */  ~
                      setname$()             , /* Cost Set Name      */  ~
                      setdesc$()             , /* Cost Set Descrip   */  ~
                      begcat$                , /* Beginning Category */  ~
                      endcat$                , /* Ending Category    */  ~
                      fromcat$               , /* Beginning Category */  ~
                      lastcat$               , /* Ending Category    */  ~
                      inc_obsolete$          , /* Include Obsolete   */  ~
                      stckeys$(1%)           , /* Cost Set #1 Key    */  ~
                      stckeys$(2%)           , /* Cost Set #2 Key    */  ~
                      hnyqkey$                 /* HNYQUAN Key        */
                actuals% = 0%
            return
        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        data_load
            if eof%(wf%)   = 1% then return
            if avail%(wf%) = 1% then return

            call "PLOWNEXT" (#wf%, stckeys$(wf%), 0%, f1%(wf%))
                if f1%(wf%) = 0% then set_eof_flags
                if stckeys$(wf%) > lastpart$ then set_eof_flags
                     get #wf% using L30130, stccost(wf%)
L30130:                   FMT POS(52), PD(14,4)
                     avail%(wf%) = 1%
                     return

        set_eof_flags
            eof%(wf%)    = 1%
            avail%(wf%)  = 0%
            stccost(wf%) = 0
            init (hex(ff))  stckeys$(wf%)
            return

        data_load_2   /* HNYQUAN & Std Cost Set */
            avail%(1%) = 1% : stccost(2%) = 0
L30410:     call "PLOWNEXT" (#06, hnyqkey$, 0%, f1%(6%))
                if f1%(6%) = 0% then set_eof_flags_2
            stckeys$(1%), stckeys$(2%) = str(hnyqkey$,,25%)
            get #06 using L30442, stccost(1%), cost_method$
L30442:         FMT POS(117), PD(14,4), POS(403), CH(1)
            if cost_method$ = "F" then L30460
                str(hnyqkey$,26%) = all(hex(ff))
                goto L30410
L30460:     call "READ100" (#02, stckeys$(2%), f1%(2%))
            avail%(2%) = f1%(2%)
                if f1%(2%) = 1% then get #02 using L30130, stccost(2%)
            str(hnyqkey$,26%) = all(hex(ff))
            return

        set_eof_flags_2
            eof%(1%), eof%(2%) = 1%
            return


        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              str(line2$,62%) = "STCCOMPR: " & str(cms2v$,,8%)
              if fieldnr% = 3% then pf9$ = "(9)Use Actuals"              ~
                               else pf9$ = " "
              if fieldnr% = 3% then pf9k$ = hex(09) else pf9k$ = hex(ff)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40180,         /* Part Number       */   ~
                                L40180,         /* Category Code     */   ~
                                L40180,         /* Cost Set #1       */   ~
                                L40180,         /* Cost Set #2       */   ~
                                L40190,         /* Report Type       */   ~
                                L40180,         /* Sort Sequence     */   ~
                                L40180,         /* Include Obsolete  */   ~
                                L40190          /* Exception Limit   */
              goto L40210

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40190:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40210:     accept                                                       ~
               at (01,02),                                               ~
                  "Compare Standard Cost Sets",                          ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,20), fac(hex(ac)), from$                  , ch(25),~
               at (06,50), fac(hex(ac)), to$                    , ch(25),~
                                                                         ~
               at (08,02), "Part Number",                                ~
               at (08,20), fac(lfac$(1%)), begpart$             , ch(25),~
               at (08,50), fac(lfac$(1%)), endpart$             , ch(25),~
                                                                         ~
               at (09,02), "Category Code",                              ~
               at (09,20), fac(lfac$(2%)), begcat$              , ch(04),~
               at (09,50), fac(lfac$(2%)), endcat$              , ch(04),~
                                                                         ~
               at (10,02), "Cost Set #1",                                ~
               at (10,20), fac(lfac$(3%)), setname$(1%)         , ch(08),~
               at (10,30), fac(hex(8c))  , setdesc$(1%)         , ch(32),~
                                                                         ~
               at (11,02), "Cost Set #2",                                ~
               at (11,20), fac(lfac$(4%)), setname$(2%)         , ch(08),~
               at (11,30), fac(hex(8c))  , setdesc$(2%)         , ch(32),~
                                                                         ~
               at (12,02), "Report Type?",                               ~
               at (12,20), fac(lfac$(5%)), rpttype$             , ch(01),~
                                                                         ~
               at (13,02), "Sort Sequence?",                             ~
               at (13,20), fac(lfac$(6%)), sort$                , ch(01),~
                                                                         ~
               at (14,02), "Include Obsolete?",                          ~
               at (14,20), fac(lfac$(7%)), inc_obsolete$        , ch(01),~
                                                                         ~
               at (15,02), "Exclude if Variance Under ##.# %",           ~
               at (15,28), fac(lfac$(8%)), print_limit$         , ch(04),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,38), fac(hex(8c)), pf9$                           ,~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(hex(000104050d0f10)&pf9k$),                          ~
               key (keyhit%)

               if keyhit% <> 9% then L40580
                  setname$(1%) = "*ACTUALS"
                  goto L40210

L40580:        if keyhit% <> 13% then L40620
                  call "MANUAL" ("STCCOMPR")
                  goto L40210

L40620:        if keyhit% <> 15% then L40660
                  call "PRNTSCRN"
                  goto L40210

L40660:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* Part Number       */     ~
                              L50150,         /* Category Code     */     ~
                              L50200,         /* Cost Set #1       */     ~
                              L50300,         /* Cost Set #2       */     ~
                              L50400,         /* Report Type       */     ~
                              L50500,         /* Sort Mode         */     ~
                              L50550,         /* Include Obsolete  */     ~
                              L50600          /* Exception Limit   */
            return

L50100: REM Test for Part Number                  PART$
            call "TESTRNGE" (begpart$, endpart$, stckeys$(1%), lastpart$,~
                             errormsg$, #4)
            return

L50150: REM Test for Category Code                CATEGORY$
            call "TESTRNGE" (begcat$, endcat$, fromcat$, lastcat$,       ~
                             errormsg$, #5)
            return

L50200: REM Test for Cost Set #1                  COST1$
            if setname$(1%) = "*ACTUALS" then L50292
                actuals% = 0%
                stckeys$(2%) = "STC.HDR." & setname$(1%)
                setdesc$(1%) = hex(06) & "Select Cost Set"
                call "PLOWCODE" (#3, stckeys$(2%), setdesc$(1%), 8%, 0.3,~
                                                                 f1%(3%))
                if f1%(3%) = 1% then L50250
                   errormsg$ = "Cost Set " & setname$(1%) & " Not Found"
                   return
L50250:         setname$(1%) = str(stckeys$(2%),9%,8%)
                call "PUTPAREN" (setdesc$(1%))
                return
L50292:     setdesc$(1%) = "(Fixed Std Costed Parts ONLY)"
            actuals% = 1%
            return

L50300: REM Test for Cost Set #2                  COST2$
            stckeys$(2%) = "STC.HDR." & setname$(2%)
            setdesc$(2%) = hex(06) & "Select Cost Set"
            call "PLOWCODE" (#3, stckeys$(2%), setdesc$(2%), 8%, 0.3,    ~
                                                                 f1%(3%))
            if f1%(3%) = 1 then L50355
               errormsg$ = "COST SET " & setname$(2%) & " NOT FOUND"
               return

L50355:     setname$(2%) = str(stckeys$(2%),9%,8%)
            call "PUTPAREN" (setdesc$(2%))
            if setname$(1%) <> setname$(2%) then return

            errormsg$ = "Cost sets must be unique"
            return

L50400: REM Test for Report Type                  RPTTYPE$
            if actuals% = 1% and rpttype$ = "1" then                     ~
                errormsg$ = "Report Type must be '5' for Actuals"
            if rpttype$ = "1" or rpttype$ = "5" then return
            errormsg$ = "Please enter '1' or '5'"
            return

L50500: REM Test for Sort Mode                    SORT$
            if sort$ = "P" or sort$ = "C" then return
            errormsg$ = "Please enter 'P' or 'C'"
            return

L50550: REM Test for Include Obsolete Parts       INC_OBSOLETE$
            if inc_obsolete$ = "Y" or inc_obsolete$ = "N" then return
            errormsg$ = "Please enter 'Y' or 'N'"
            return

L50600: REM Test for Exception Report Limit       PRINT_LIMIT$
            call "NUMTEST" (print_limit$, 0, 99.9, errormsg$, -1.1,      ~
                            print_limit)
            return

        REM *************************************************************~
            *           Here Come the IMAGE Statements                  *~
            *************************************************************

*       ** Report Lines for Unit Cost Version & General for both
L60040: %RUN DATE: ######## @ ########           ########################~
        ~####################################              STCCOMPR:STC006

L60060: %                                               COST SET COMPARIS~
        ~ON OF UNIT COSTS                                      PAGE: #####

L60080: %SET 1: ######## ################################ - SET 2: ######~
        ~## ################################  REPORT SEQUENCED BY ########~
        ~###

L60110: %  Part Number                   Part Description                ~
        ~    Set       Cost (1)       Cost (2)     $ Variance       % Var

L60140: %  -------------------------     --------------------------------~
        ~    ---     ----------     ----------     ----------     -------

L60200: %  #########################     ################################~
        ~     #      ##########     ##########     ##########     #######

*       ** Print Lines for Actuals Vs Std
        %                                           ACTUAL VALUATION COMP~
        ~ARISON TO COST SET                                    PAGE: #####
        %Part Number/Description             Store  Lot           Cost (1~
        ~)   Cost (2) Qty On Hnd    Value (1)    Value (2) $ Variance   % ~
        ~Var
        %----------------------------------   ---   ------      ---------~
        ~- ---------- ---------- ------------ ------------ ---------- ----~
        ~---

*       ** Print Lines for Evaluation Version
L60310: %                                          COST SET COMPARISON OF~
        ~ INVENTORY VALUATION                                  PAGE: #####

L60370: %Part Number               Part Description               Cost (1~
        ~)   Cost (2) Qty On Hnd    Value (1)    Value (2) $ Variance   % ~
        ~Var

L60410: %------------------------- ---------------------------- ---------~
        ~- ---------- ---------- ------------ ------------ ---------- ----~
        ~---

L60490: %######################### ############################ #########~
        ~# ########## ########## ############ ############ ########## ####~
        ~###

L60525: %                                                                ~
        ~                        ============ ============ ==========

L60540: %                                                                ~
        ~         REPORT TOTALS: ############ ############ ##########

*       ** End Report Print Line
L60580: %                                * * * * *   E N D   O F   R E P ~
        ~O R T   @ ########   * * * * *

L60610: %***** PARTS IN CATEGORY ######

L60630: %                                                                ~
        ~       CATEGORY TOTALS: ############ ############ ##########

        %                                                               C~
        ~ATEGORY VARIANCE TOTAL:                        #############

        %                                                                ~
        ~ REPORT VARIANCE TOTAL:                        #############

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

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
