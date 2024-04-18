        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  JJJJJ  BBBB    SSS   H   H  RRRR   TTTTT  RRRR   PPPP    *~
            *    J    B   B  S      H   H  R   R    T    R   R  P   P   *~
            *    J    BBBB    SSS   HHHHH  RRRR     T    RRRR   PPPP    *~
            *  J J    B   B      S  H   H  R   R    T    R   R  P       *~
            *   J     BBBB    SSS   H   H  R   R    T    R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBSHRTRP - This program identifies the jobs that have been*~
            *            kitted but not all the parts have been issued. *~
            *            The parts that have not been kitted are        *~
            *            reported upon.                                 *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/07/92 ! Original                                 ! RJH *~
            * 02/23/95 ! Corrected range logic                    ! JDH *~
            * 08/13/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            buyer_code$3,                /* Buyer Class Code           */~
            closedate$6,                 /* Date Job closed            */~
            columnttl$51,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fmbuyer_code$3,              /* Buyer Class Code Range     */~
            fmdate$10,                   /* Date                       */~
            fmjobnum$8,                  /* Job Number                 */~
            fmsched_code$3,              /* Scheduler Class  Range     */~
            fmtype$3,                    /* Part Type Range            */~
            head3$30,                    /* Header 3 Mesage            */~
            hibuyer_code$3,              /* Buyer Class Code Range     */~
            hidate$10,                   /* Date                       */~
            hijobnum$8,                  /* Job Number                 */~
            hisched_code$3,              /* Scheduler Class  Range     */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            jobnum$8,                    /* Job Number                 */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lodate$10,                   /* Date                       */~
            lobuyer_code$3,              /* Buyer Class Code Range     */~
            lojobnum$8,                  /* Job Number                 */~
            losched_code$3,              /* Scheduler Class  Range     */~
            lot$6,                       /* Lot                        */~
            orig_page_code$3,            /* Paging Criteria            */~
            page_code$3,                 /* Paging Criteria            */~
            part$25, partdescr$32,       /* Part Number/description    */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            pipquan$10,                  /* PIPOUT Quantity            */~
            pipdate$8,                   /* PIPOUT Date                */~
            piptime$8,                   /* PIPOUT Time Stamp          */~
            pldate$6,                    /* PIPOUT Start Date          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            qcquan$10,                   /* QC Quantities              */~
            quan(5), quan$10,            /* Book Quantities            */~
            rptid$6,                     /* Report ID                  */~
            rpttitle$60,                 /* Report Title               */~
            sched_code$3,                /* Buyer Class Code           */~
            sortby$1,                    /* Sort By Report Criteria    */~
            store$3,                     /* Store                      */~
            time$8,                      /* System Time                */~
            tobuyer_code$3,              /* Buyer Class Code Range     */~
            todate$10,                   /* Date                       */~
            today$8,                     /* Today's Date               */~
            tojobnum$8,                  /* Job Number                 */~
            tosched_code$3,              /* Buyer Class Code Range     */~
            totype$3,                    /* Part Type Range            */~
            type$3,                      /* Part Type                  */~
            userid$3                     /* Current User Id            */~

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
            * #01 ! HNYMASTR ! Inventory Master File                    *~
            * #02 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * #03 ! JBMASTR2 ! Production job master file               *~
            * #04 ! JBMATER2 ! Production job material used detail file *~
            * #05 ! PIPOUT   ! Planned inventory use detail rec         *~
            * #06 ! SYSFILE2 ! System Information File                  *~
            * #50 ! WORKFILE ! Temporary System Workfile                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  3, keypos =   26, keylen =  32, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  1, keypos =  102, keylen =   9, dup     ~

            select #02, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44          ~

            select #03, "JBMASTR2",                                      ~
                        varc,     indexed,  recsize = 1300,              ~
                        keypos =    1, keylen =   8,                     ~
                        alt key  2, keypos =   58, keylen =  25, dup,    ~
                            key  1, keypos = 1120, keylen =  19, dup     ~

            select #04, "JBMATER2",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    1, keylen =  22,                     ~
                        alt key  1, keypos =   23, keylen =  48          ~

            select #05, "PIPOUT",                                        ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~


            select #06, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =    1, keylen =  20

            select #50, "WORKFILE",                                      ~
                        varc,     indexed,  recsize =   83,              ~
                        keypos =    1, keylen =  44                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 0%, rslt$(01%))
            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 0%, rslt$(02%))
            call "OPENCHCK" (#03, fs%(03%), f2%(03%), 0%, rslt$(03%))
            call "OPENCHCK" (#04, fs%(04%), f2%(04%), 0%, rslt$(04%))
            call "OPENCHCK" (#05, fs%(05%), f2%(05%), 0%, rslt$(05%))
            call "OPENCHCK" (#06, fs%(06%), f2%(06%), 0%, rslt$(06%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            today$ = date$
            call "DATEOK" (today$, today%, errormsg$)
            call "READ100" (#06, "MONTHS OPEN", f1%(06%))
                if f1%(06%)= 0% then exit_program
            get #06, using L09120, pldate$
L09120:         FMT XX(32), CH(6)
            call "COMPNAME" (12%, company$, ret%)  :  ret% = ret%
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rptid$ = "JB0013"

            rpttitle$ = "Incomplete Kit Report         "
            f2%(50%) = 1%

            str(columnttl$, 1%) = "Beginning     "
            str(columnttl$,27%) = "Ending     "

            str(line2$,62%) = "JBSHRTRP: " & str(cms2v$,,8%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  6%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
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
            * Handles EDIT MODE for range selection screen.             *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       extract_data
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  6% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *           E X T R A C T   R E P O R T   D A T A           *~
            *-----------------------------------------------------------*~
            * Data Extraction section for report.                       *~
            *************************************************************
        extract_data

            call "SHOSTAT"  ("Testing Kitted Jobs")
            call "FILEBGON" (#50)
            call "WORKOPEN" (#50, "IO", 100%, f2%(50%))
            if fmjobnum$ = "ALL" or fmjobnum$ = "FIRST" then             ~
                plowkey$ = lojobnum$ else plowkey$ = fmjobnum$
            call "READ104" (#03, plowkey$, f1%(3%))
            goto L13140
        loop_jobmastr2
            call "READNEXT" (#03, f1%(3%))
L13140:         if f1%(3%) = 0% then goto print_report
            get #03 using L13160, jobnum$, closedate$
L13160:         FMT CH(8), POS(153), CH(6)
            if jobnum$ > hijobnum$ then print_report
            if closedate$ <> " " and closedate$ <> blankdate$ ~
                                          then loop_jobmastr2

*        We have an OPEN Job, lets check if anything has been issued. *
            call "READ100" (#04, jobnum$, f1%(4%))
                if f1%(4%) = 0% then goto loop_jobmastr2 /*No issued Prt*/

*        We have issued at least one part, Lets check for unreleased.  *
            plowkey$ = "JOB ORDER: " & jobnum$
L13260:     call "PLOWNEXT" (#05, plowkey$, 19%, f1%(5%))
                if f1%(5%) = 0% then goto loop_jobmastr2 /*No more */
            get #05 using L13290, part$, pipdate%, piptime$, pipquan
L13290:         FMT POS(20), CH(25), BI(4), CH(8), PD(14,4)
            if pipquan < 0.0 then L13260     /* Don't want By-Products */
            pipdate$ = " "
            call "DATE" addr("G+", pldate$, pipdate%-1%, pipdate$, err%)
                err% = err%
            if pipdate$ < lodate$ or pipdate$ > hidate$ then L13260

*        ** We now have an unissued Part so we check the Part Type, *
*        ** Buyer Class, and Scheduler Class                        *
            call "READ100" (#01, part$, f1%(1%))
            get #01 using L13380, type$, buyer_code$, sched_code$
L13380:         FMT POS(180), CH(3), POS(200), CH(3), POS(309), CH(3)
            convert type$ to type% , data goto L13500  /* Ignore problem */
            if type% < lotype% or type% > hitype% then L13260 /*Try Again*/
            if buyer_code$ < lobuyer_code$ or buyer_code$ > hibuyer_code$~
                                                  then L13260 /*Try Again*/
            if sched_code$ < losched_code$ or sched_code$ > hisched_code$~
                                                  then L13260 /*Try Again*/

*        We now have an unissued Part so we calc. the Book Quantities *
L13500:     sw% = 1%  /* Numeric Stores Only */
            store$, lot$ = " "
            call "HNYTOTSB" (part$, store$, lot$, quan(), sw%)

*        Write Record to Workfile Now *
            on pos("PBS" = sortby$) goto  L13544, L13546, L13548
L13544:         page_code$ = " "          :  goto L13550
L13546:         page_code$ = buyer_code$  :  goto L13550
L13548:         page_code$ = sched_code$
L13550:     write #50 using L13570, page_code$, part$, jobnum$, piptime$, ~
                          pipquan, pipdate$, quan(1%), quan(5%),         ~
                          type$, buyer_code$, sched_code$
L13570:         FMT CH(3), CH(25), CH(8), CH(8), PD(14,4), CH(6),        ~
                    PD(14,4), PD(14,4), CH(3), CH(3), CH(3)
            goto L13260       /* Loop to PIPOUT */

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Job Number             */~
                              L20200,         /* Date                   */~
                              L20300,         /* Part Type              */~
                              L20400,         /* Buyer Part Class Code  */~
                              L20500,         /* Production Scheduler Cd*/~
                              L20600          /* Report Sorted By       */
            return
L20100: REM Def/Enable Job Number                  FMJOBNUM$

            if fmjobnum$  = " " then  fmjobnum$  = "ALL"
            return

L20200: REM Def/Enable Date                        FMDATE$
            if fmdate$ = " " or fmdate$ = blankdate$ then fmdate$    = "FIRST"
            if todate$ = " " or todate$ = blankdate$ then todate$    = "TODAY"
            return

L20300: REM Def/Enable Part Typer                  FMTYPE$

            if fmtype$  = " " then  fmtype$  = "ALL"
            return

L20400: REM Def/Enable Buyer Class Code          FMBUYER_CODE$

            if fmbuyer_code$  = " " then  fmbuyer_code$  = "ALL"
            return


L20500: REM Def/Enable Scheduler Class Code      FMSCHED_CODE$

            if fmsched_code$  = " " then  fmsched_code$  = "ALL"
            return

L20600: REM Def/Enable Sort by                     SORT_BY$


            return


        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Job Number Range, 'FIRST', 'LAST', 'ALL', or '?' Wildcard~
        ~."                                                              ,~
         "Enter Planned Issue Date Range, 'FIRST', 'ALL', or 'TODAY'.  ",~
         "Enter Part Type Range, '000' thru '999' or 'ALL'.            ",~
         "Enter Buyer Class Range, or 'ALL'.                           ",~
         "Enter Scheduler Class Range, or 'ALL'.                       ",~
         "Enter Report Sort Method, 'P'art #, 'B'uyer, or 'S'cheduler."

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, fmdate$, fmjobnum$, fmtype$~
                    , hidate$, hijobnum$, lodate$, lojobnum$, todate$,   ~
                      tojobnum$, totype$, fmbuyer_code$, hibuyer_code$,  ~
                      lobuyer_code$, tobuyer_code$, fmsched_code$,       ~
                      hisched_code$, losched_code$, tosched_code$,       ~
                      sortby$
            call "ALLFREE"
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
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

        REM *************************************************************~
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************
        print_report
            call "SHOSTAT" ("Printing Report")
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            page_code$, orig_page_code$ = " "
            pcntr% =  0% : lcntr% = 99% /* Page & Line Counters */
            if lcntr% < 57% then goto L30160
                gosub page_head

*       * Loop thru the Workfile and Print each record **
L30160:     plowkey$ = all(hex(00)) :  itemcntr% = 0%
            call "READ104" (#50, plowkey$, f1%(50%))
            goto L30210
        loop_workfile
            call "READNEXT" (#50, f1%(50%))
L30210:         if f1%(50%) = 0% then end_report
            get #50 using L30250, page_code$, part$, jobnum$, pipquan,    ~
                                 pipdate$, quan, qcquan, type$,          ~
                                 buyer_code$, sched_code$
L30250:         FMT CH(3), CH(25), CH(8), POS(45), PD(14,4), CH(6),      ~
                    PD(14,4), PD(14,4), CH(3), CH(3), CH(3)
            call "DATEFMT" (pipdate$)
            call "CONVERT" (pipquan, 2.2, pipquan$)
            call "CONVERT" (   quan, 2.2,    quan$)
            call "CONVERT" ( qcquan, 2.2,  qcquan$)
            call "DESCRIBE" (#01, part$, partdescr$, 0%, f1%( 1%))

            itemcntr% = itemcntr% + 1%
            gosub  print_record
            goto   loop_workfile

        print_record
            if lcntr% < 57%  and orig_page_code$ = page_code$  then L30410
                gosub page_head
                gosub column_head
L30410:     print using L60200, part$, partdescr$, type$, jobnum$,        ~
                               pipquan$, pipdate$, quan$, qcquan$,       ~
                               buyer_code$, sched_code$

           lcntr% = lcntr%  +  1%
           return

        end_report                /* Report Ending Routine */
            time$ = " "  :  call "TIME" (time$)
            if itemcntr% > 0% then L34040
                print skip(2)  :  print using L60460
L34040:     print skip(2)
            print using L64987, time$    /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            goto inputmode

        page_head              /* Page Heading Print Routine */
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "JBSHRTRP"
            print using L60110, rpttitle$, pcntr%

            lcntr% = 3%
            if pcntr% = 0% then L34280
            pcntr% = pcntr% + 1%
            if sortby$ = "P" then L34250              /* Print & Return */
            if sortby$ = "B" then head3$ = "BUYER CLASS CODE IS: " &     ~
                                            page_code$                   ~
                  else  head3$ = "SCHEDULER CLASS CODE IS: " &  page_code$
            print using L60126, head3$
            lcntr% = lcntr% + 1%
            orig_page_code$ = page_code$
L34250:     print
            return
*        ** Page 0 Printing **
L34280:     gosub print_params
            lcntr% = 99%
            return

        column_head
            print using L60140
            print using L60170
            lcntr% = lcntr% + 2%
            return

        print_params           /* Print Page Zero */
L34390:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L34430
                str(i$(), i%, 1%) = hex(20)
                goto L34390
L34430:     print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            pcntr% = pcntr% + 1%
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40085,         /* Job Number        */   ~
                                L40085,         /* Date              */   ~
                                L40085,         /* Part Type         */   ~
                                L40085,         /* Buyer Class Code  */   ~
                                L40085,         /* Scheduler Code    */   ~
                                L40085          /* Report Sorted By   */
              goto L40100

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40085:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40100:     accept                                                       ~
               at (01,02),                                               ~
                  "Incomplete Kit Report        ",                       ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,32), fac(hex(ac)),   columnttl$           , ch(40),~
                                                                         ~
               at (07,02), "Job Number Range",                           ~
               at (07,32), fac(lfac$( 1%)), fmjobnum$           , ch(08),~
               at (07,58), fac(lfac$( 1%)), tojobnum$           , ch(08),~
                                                                         ~
               at (08,02), "Planned Issue Date Range",                   ~
               at (08,32), fac(lfac$( 2%)), fmdate$             , ch(10),~
               at (08,58), fac(lfac$( 2%)), todate$             , ch(10),~
                                                                         ~
               at (09,02), "Part Type Range",                            ~
               at (09,32), fac(lfac$( 3%)), fmtype$             , ch(03),~
               at (09,58), fac(lfac$( 3%)), totype$             , ch(03),~
                                                                         ~
               at (10,02), "Buyer Class Code Range",                     ~
               at (10,32), fac(lfac$( 4%)), fmbuyer_code$       , ch(03),~
               at (10,58), fac(lfac$( 4%)), tobuyer_code$       , ch(03),~
                                                                         ~
               at (11,02), "Scheduler Class Code Range",                 ~
               at (11,32), fac(lfac$( 5%)), fmsched_code$       , ch(03),~
               at (11,58), fac(lfac$( 5%)), tosched_code$       , ch(03),~
                                                                         ~
               at (12,02), "Report Sort Criteria",                       ~
               at (12,32), fac(lfac$( 6%)), sortby$             , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40575
                  call "MANUAL" ("JBSHRTRP") : goto L40100

L40575:        if keyhit% <> 15% then L40605
                  call "PRNTSCRN" : goto L40100

L40605:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40795     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                 (4)Previous Field      " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40765
                str(pf$(3%),64%)     = " " : str(pfkeys$,16%,1%) = hex(ff)
            if fieldnr% > 1% then L40775
L40765:         str(pf$(2%),18%,26%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L40775:     return

L40795: if fieldnr% > 0% then L40885  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40885:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* Job Number             */~
                              L50200,         /* Date                   */~
                              L50390,         /* Part Type              */~
                              L50600,         /* Buyer Part Class Code  */~
                              L50700,         /* Production Scheduler Cd*/~
                              L50800          /* Report Sorted By       */
            return

L50100: REM Test for Job Number                   FMJOBNUM$
            call "TESTRNGE"                                              ~
                  (fmjobnum$           , tojobnum$           ,           ~
                   lojobnum$           , hijobnum$           ,           ~
                   errormsg$, #03)
            return

L50200: REM Test for Date                         FMDATE$

            if fmdate$ <> "ALL"       then L50240
                fmdate$  =  "FIRST"
                todate$  =  "TODAY"

L50240:     if fmdate$  <>  "FIRST"   then  L50250
                lodate$  =  "19000101"
                call "DATFMTC" (lodate$)
                goto L50280
L50250:     if fmdate$  <>  "TODAY"   then  L50260
                lodate$  =   today$    :  goto L50280
L50260:     call "DATEOKC" (fmdate$, fmdate%, errormsg$)
                if errormsg$ <> " " then  return
            if fmdate% <= today% then L50270
                errormsg$ = "FROM Date, " & fmdate$                      ~
                             &  ", Can Not be Past Today."
                return
L50270:     lodate$  =   fmdate$

L50280:     if todate$  <>  "FIRST"   then  L50290
                hidate$  =  "19000101"
                call "DATFMTC" (hidate$)
                goto L50335
L50290:     if todate$ <> "TODAY"   then  goto L50300
                hidate$  =   today$    :  goto L50335
L50300:     if todate$ <> " " and todate$ <> blankdate$ then L50315
                todate$ = fmdate$
                hidate$ = lodate$ : goto L50335
L50315:     call "DATEOKC" (todate$, todate%, errormsg$)
                if errormsg$ <> " " then return
            if todate% <= today% then L50325
                errormsg$ = "TO Date, " & todate$                        ~
                             & ", Can Not be Past Today."
                return
L50325:     hidate$ = todate$

L50335:     call "DATUFMTC"  (lodate$)
            call "DATUFMTC"  (hidate$)
            if lodate$ > hidate$  then  errormsg$ = "End must be Equal"  ~
                                        &   " or Greater than Beginning" ~
                                  else  errormsg$ = " "
            fmdate% = fmdate%  :  todate% = todate%  /* Do nothing line */
            return

        REM Test for Part Type                    FMTYPE$
L50390:     if fmtype$ <> "ALL" then L50410
                totype$ = " " : lotype% = 0% : hitype% =  999%
                return
L50410:     convert fmtype$ to lotype%, data goto L50440
            if lotype% < 0% or lotype% > 999% then L50440 else L50455
L50440:         errormsg$ = "Please enter Type as numeric, 0 to 999"
                return
L50455:     convert lotype% to fmtype$, pic(000)
            if totype$ = " " and fmtype$ <> "ALL" then totype$ = fmtype$
            convert totype$ to hitype%, data goto L50440
            if hitype% < 0% or hitype% > 999% then L50440
            convert hitype% to totype$, pic(000)
            if hitype% >= lotype% then return
                errormsg$ =                                              ~
                   "From Type must be Less than or equal to To Type."
                return

L50600: REM Test for Buyer Class Code           FMBUYER_CODE$
            call "TESTRNGE"                                              ~
                  (fmbuyer_code$       , tobuyer_code$       ,           ~
                   lobuyer_code$       , hibuyer_code$       ,           ~
                   errormsg$     )
            return

L50700: REM Test for Scheduler                  FMSCHED_CODE$
            call "TESTRNGE"                                              ~
                  (fmsched_code$       , tosched_code$       ,           ~
                   losched_code$       , hisched_code$       ,           ~
                   errormsg$     )
            return

L50800: REM Test for Sort by                      SORTBY$
            if pos("PBS" = sortby$) <> 0%  then  return
                errormsg$ = "Sort Criteria are 'P', 'B', or 'S'."
                return




        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:JB0013

*       * Header Line 2
L60110: %                                                        ########~
        ~#################################                     PAGE: ####

*       * Header Line 3
L60126: %##############################


*       * Column Head 1
L60140: %PART NUMBER               PART DESCRIPTION                 TYPE ~
        ~JOB NBR    REQ QUAN REQ DATE   BOH QUAN    QC QUAN BUY CODE SCHD ~
        ~CDE
*       * Column Head 2
L60170: %------------------------- -------------------------------- ---- ~
        ~-------- ---------- -------- ---------- ---------- -------- -----~
        ~---

*       * Record Line
L60200:  %######################### ################################  ###~
        ~ ######## ########## ######## ########## ##########   ###      ##~
        ~#

*       * Null Records Printed line
L60460: %                                       * * * No Records Printed ~
        ~* * *

L64987:      %                          * * * * * * * * * *   E N D   O F~
        ~   R E P O R T   @   ########   * * * * * * * * * *

        REM THISPROGRAMWASGENERATEDBYGENRPPGMAPROPRIETRYPRODUCTOFCAELUS**~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
