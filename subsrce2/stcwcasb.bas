        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   TTTTT   CCC   W   W   CCC    AAA    SSS   BBBB    *~
            *  S        T    C   C  W   W  C   C  A   A  S      B   B   *~
            *   SSS     T    C      W   W  C      AAAAA   SSS   BBBB    *~
            *      S    T    C   C  W W W  C   C  A   A      S  B   B   *~
            *   SSS     T     CCC    W W    CCC   A   A   SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCWCASB - This program defines standard costs associated *~
            *            with work centers.                             *~
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
            * 03/23/87 ! Original                                 ! LKM *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "STCWCASB" (costset$,        /* Cost set ID                */~
                        setdesc$,        /* Cost set description       */~
                        nfgflag$,        /* Frozen flag                */~
                        buckets%,        /* Number of buckets          */~
                        bktids$(),       /* Bucket names               */~
                        bktdescr$(),     /* Bucket descriptions        */~
                        #1,              /* Work center master         */~
                        #2,              /* Work center standard cost  */~
                        #4,              /* GENCODES                   */~
                        #5,              /* SYSFILE2                   */~
                        #6)              /* STCLABOR                   */

        dim                                                              ~
            activity$4,                  /* Activity Code              */~
            actdescr$32,                 /* Activity Description       */~
            afac$1,                      /* Fac for Activity Descript  */~
            begwrkcntr$4,                /* Work Center for report     */~
            begactivity$4,               /* Activity for report        */~
            bfac$1,                      /* Fac for buckets            */~
            bucket$(6)10,                /* Input Bucket Names         */~
            bucketnbr$(6)2,              /* Bucket Numbers             */~
            bkts%(6),                    /* Bucket Numbers             */~
            bktids$(12)10,               /* Bucket Names               */~
            bktdescr$(12)20,             /* Bucket Descriptions        */~
            bktdesc2$(6)22,              /* Output Bucket Descriptions */~
            company$60,                                                  ~
            constant1$10,                                                ~
            constant2$10,                                                ~
            constant3$24,                                                ~
            constant4$11,                                                ~
            constant5$6,                                                 ~
            constant6$20,                                                ~
            constant7$11,                                                ~
            constant8$11,                                                ~
            constant9$5,                                                 ~
            cost$48,                     /* Argument for PACKZERO      */~
            cost$(6)10,                  /* Cost Table                 */~
            costdesc$(6)22,              /* Cost Descriptions          */~
            costs(6),                    /* Cost Amounts               */~
            costset$8,                   /* Cost Set Identification    */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dfac$(4)1,                   /* Facs for labor description */~
            edtmessage$79,               /* Edit screen message        */~
            endwrkcntr$4,                /* Ending Work center         */~
            endactivity$4,               /* Ending Activity            */~
            errormsg$79,                 /* Error message              */~
            factor$(4)4,                 /* Labor Class Multiplier     */~
            factor(4),                   /* Multipliers from record    */~
            i$(24)80,                    /* Screen Image               */~
            inactivity$4,                /* Activity from file         */~
            inwrkcntr$4,                 /* Work center from file      */~
            inpmessage$79,               /* Informational Message      */~
            labcosts(6),                 /* Costs from labor file      */~
            labcost(3),                  /* Buckets combined           */~
            labcost$(3)10,               /* For totals display         */~
            labor_class$(4)4,            /* Labor Class Data           */~
            labdescr$(4)32,              /* Labor Description          */~
            lfac$(10)1,                  /* Field Attribute Characters */~
            line$58,                     /* Print Line for PLOWCODE    */~
            line2$79,                    /* Second Line of Screen Headr*/~
            misc$34,                                                     ~
            nfgflag$1,                   /* Frozen/Global Flag         */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            pf6$14,                                                      ~
            pf8$13,                                                      ~
            pf9$22,                                                      ~
            pf11$19,                                                     ~
            pf12$17,                                                     ~
            pfkeys$13,                                                   ~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rec$110,                     /* Record Holder              */~
            rec2$110,                    /* Record Holder #2           */~
            setdesc$30,                  /* Cost Set Description       */~
            setdesc2$32,                 /* Cost Set for Display       */~
            srch(2),                     /* Used for search results    */~
            totcost(3),                  /* Labor and WC combined      */~
            totcost$(3)10,               /* For totals screen          */~
            userid$3,                    /* Current User Id            */~
            wrkcntr$4,                   /* Work Center                */~
            wrkcost$(3)10,               /* For totals screen          */~
            wrkcost(3),                  /* WC costs combined          */~
            wrkdescr$32                  /* Work Center Description    */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.00.00 09/08/87 Standard cost to 12             "
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
            * #1  ! WCMASTR  ! Work Center Master File                  *~
            * #2  ! STC####W ! Work Center Standard Cost Set            *~
            * #3  ! STCCHNGS ! Net Change File                          *~
            * #4  ! GENCODES ! File for Labor Class and Activity Codes  *~
            * #5  ! SYSFILE2 ! Used to Validate Bucket Names            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            setdesc2$ = setdesc$
            call "PUTPAREN" (setdesc2$)
            constant1$ = "     Value"
            constant2$ = "Bucket ID "
            constant3$ = "Bucket Description & Nr."
            constant4$ = "Labor Class"
            constant5$ = "Factor"
            constant6$ = "Labor Description   "
            constant7$ = "Work Center"
            constant8$ = "Labor Class"
            constant9$ = "Total"
            costdesc$(1) = "STANDARD FIXED DOLLARS"
            costdesc$(2) = "STANDARD FIXED DOLLARS"
            costdesc$(3) = "STANDARD DOLLARS/PART"
            costdesc$(4) = "STANDARD DOLLARS/PART"
            costdesc$(5) = "STANDARD DOLLARS/HOUR"
            costdesc$(6) = "STANDARD DOLLARS/HOUR"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            editmode = 0
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            pf4$ = " " : pf5$ = " " : pf16$ = "(16)Exit Program"
            pf6$ = "(6)Labor Costs"
            pf8$ = " "
            pf14$ = "(14)Print Report"
            pf12flag = 0
            pfkeys$ = hex(0001000000000d0f10000e0600)
            labptr = 0
            gosub L29000

            for fieldnr% = 1 to 10
                if fieldnr% < 2 then L10240
                   pf4$ = "(4)Previous Field"
                   str(pfkeys$,3,1) = hex(04)
                   pf16$ = " "
                   str(pfkeys$,9,2) = hex(00)
L10240:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 1 then L10280
                         if fieldnr% < 3 then L10450
                            goto L11000
L10280:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10360
L10310: REM              FIELDNR% = MAX(1%, FIELDNR% - 1%)
                         if writeact = 1                                 ~
                               then fieldnr% = max(1%, fieldnr% - 1%)    ~
                               else fieldnr% = max(2%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10280
                         if fieldnr% = 1% then L10240
                         goto L10310
L10360:               if keyhit% = 14% then print_mode
                      if keyhit% =  6% then gosub call_labor_sub
                      if keyhit% = 16 and fieldnr% = 1 then exit_program
                      if keyhit% <>  9 then L10420
                         gosub plow_wc_costs
                         if errormsg$ <> " " then L10280 else goto L10430
L10420:               if keyhit% <>  0 then       L10280
L10430:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10280
                      if newact = 1 then L10280
L10450:     next fieldnr%
            goto editpg1

        plow_wc_costs
            errormsg$ = " "
            plowkey$ = str(wrkcntr$) & str(activity$)
            misc$ = hex(06) & "Select Work Center/Activity" & str(" ")
            call "PLOWCODE" (#2%, plowkey$, misc$, 0%, 0.0, f1%(2))
            if f1%(2) = 1 then L10550
               errormsg$ = "WC/Activity code not found"
               return
L10550:     wrkcntr$ = str(plowkey$,1,4)
            activity$ = str(plowkey$,5,4)
            return

        call_labor_sub
            call "STCLBRSB" (costset$, setdesc$, nfgflag$, bktids$(),    ~
                            bktdescr$(), #5, #4, #6)
            return

L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            editmode = 1
            pf4$  = " "
            pf5$  = " "
            pf6$  = "(6)Labor Costs"
            pf8$  = "(8)See Totals"
            str(pfkeys$,13,1) = hex(08)
            pf14$ = "(14)Print Report"
            if nfgflag$ = "F" then pf16$ = "(16)Exit     "  else         ~
               pf16$ = "(16/32)Save/Exit"
            str(pfkeys$,3,1) = hex(00)
            str(pfkeys$,9,1) = hex(10)
            str(pfkeys$,10,1) = hex(20)
            inpmessage$ = edtmessage$
L11100:     gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 11 then add_labor_codes
                  if keyhit%  = 12 then       delete_record
                  if keyhit%  = 14 then print_mode
                  if keyhit%  =  6 then gosub call_labor_sub
                  if keyhit%  =  8 then see_totals
                  if keyhit% <> 16 then       L11145
                     if nfgflag$ = "F" then exit_program  else  datasave
L11145:           if keyhit%  = 32 then exit_program
                  if keyhit% <>  0 then       editpg1
                  if nfgflag$ = "F" then L11100
            fieldnr% = cursor%(1) - 4
            if fieldnr% < 4 or fieldnr% = 11 then editpg1
            if labptr = 0 then x = 15 else x = labptr + 10
            if fieldnr%  > x then editpg1
            if fieldnr% > 11 then L11195
               fieldnr% = 2
               goto L11205
L11195:     fieldnr% = 3 + ((fieldnr% - 12) * 2)
            if cursor%(2) > 20 then fieldnr% = fieldnr% + 1
L11205
*          IF FIELDNR% = LASTFIELDNR% THEN EDITPG1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
                  pf4$, pf5$, pf6$, pf8$, pf14$, pf16$ = " "
L11225:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11225
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11225
*                LASTFIELDNR% = FIELDNR%
            goto editpg1

        add_labor_codes
            editmode = 0
            pf6$, pf8$, pf14$, pf16$ = " "
            str(pfkeys$,9,2) = hex(00)
            svpf12flag = pf12flag
            pf12flag = 0
            i% = 3 + ((labptr - 1) * 2)
            labptr = 0
            for fieldnr% = i% to 10
                gosub'051(fieldnr%)
                if enabled% = 0 then L11350
L11320:         gosub'101(fieldnr%)
                if keyhit%  =  1 then gosub startover
                if keyhit% <>  0 then L11320
                gosub'151(fieldnr%)
                if errormsg$ <> " " then L11320
            next fieldnr%
L11350:     pf12flag = svpf12flag
            goto editpg1

        delete_record
L11370:     keyhit% = 2%
            call "ASKUSER" (keyhit%, "*** DELETE RECORD?", "Press RETURN ~
        ~to DELETE This Record", "- OR -", "Press PF1 to CANCEL Delete & R~
        ~eturn")
            if keyhit% = 1 then editpg1
            if keyhit% <> 0 then L11370
            dmode = 1
            gosub dataput
            dmode = 0
            goto inputmode

        see_totals
            pf11$, pf14$ = " "
            pf16$ = "(16)Return"
            mat totcost = zer :  mat labcost = zer
            init (" ") totcost$(), wrkcost$(), labcost$()
            if labptr = 0 then L11505
            for i% = 1 to labptr - 1
                call "READ100" (#6, labor_class$(i%), f1%(6))
                if f1%(6) <> 1 then editpg1
                   get #6, using L11470, labcosts()
L11470:            FMT POS (5), 6*PD(14,4)

           labcost(1) = labcost(1)+factor(i%)*(labcosts(1)+labcosts(2))
           labcost(2) = labcost(2)+factor(i%)*(labcosts(3)+labcosts(4))
           labcost(3) = labcost(3)+factor(i%)*(labcosts(5)+labcosts(6))
            next i%

L11505:     wrkcost(1) = costs(1) + costs(2)
            wrkcost(2) = costs(3) + costs(4)
            wrkcost(3) = costs(5) + costs(6)

            for i% = 1 to 3
                totcost(i%) = wrkcost(i%) + labcost(i%)
                call "CONVERT" (wrkcost(i%), 4.4, wrkcost$(i%))
                call "CONVERT" (labcost(i%), 4.4, labcost$(i%))
                call "CONVERT" (totcost(i%), 4.4, totcost$(i%))
            next i%

L11560:     gosub totals_screen
            if keyhit% = 1 then gosub startover
            if keyhit% <> 16 then L11560
            goto editpg1

        REM *************************************************************~
            *               P R I N T    L I S T I N G                  *~
            * --------------------------------------------------------- *~
            * Print Listing of Labor Rates File.                        *~
            *************************************************************
        print_mode
           begwrkcntr$, endwrkcntr$, begactivity$, endactivity$ = " "
           pf4$ = " "
           pf16$ = "(16)Exit"
           str(pfkeys$,9,1) = hex(10)
           str (pfkeys$,3,1) = hex(ff)
           str (pfkeys$,10,1) = hex(ff)
           for fieldnr% = 1% to 2%
               gosub'052(fieldnr%)
L12048:        gosub'102(fieldnr%)
               if keyhit%  =  1% then print_mode
               if keyhit% <> 16% then L12076
                  if editmode = 1 then editpg1 else inputmode
L12076:        if keyhit% <>  0% then L12048
               gosub'152(fieldnr%)
               if errormsg$ <> " " then L12048
            next fieldnr%

        REM Edit mode for report
L12104:     pf16$ = "(16/32)Prnt/Exit"
            str(pfkeys$,10,1) = hex(20)
            inpmessage$ = edtmessage$
L12116:     gosub'102(0%)
            if keyhit% =  1 then print_mode
            if keyhit% <> 32 then L12132
               if editmode = 1 then editpg1 else inputmode
L12132:     if keyhit% = 16 then L12200
            if keyhit% <> 0 then L12116
            if cursor%(1) <> 6 and cursor%(1) <> 7 then L12116
            fieldnr% = cursor%(1) - 5
            pf16$ = "(16)Exit"
            gosub'052(fieldnr%)
L12160:     gosub'102(fieldnr%)
            if keyhit%  =  1 then print_mode
            if keyhit% <> 16 then L12176
               if editmode = 1 then editpg1 else inputmode
L12176:     if keyhit% <>  0 then L12160
            gosub'152(fieldnr%)
            if errormsg$ <> " " then L12160
            goto L12104

        REM Print Routine
L12200:     call "SHOSTAT" ("Printing WC/Activity costs")
            call "TIME" (runtime$)
            call "COMPNAME" (12%, company$, u3%)
            page% = 0% : line% = 857%
            select printer(134)
            call "SETPRNT" ("STC002", " ", 0%, 0%)
            if begwrkcntr$ <> "ALL" then L12236
               begwrkcntr$ = all(hex(00))
               endwrkcntr$ = hex(ff)
L12236:     if begactivity$ <> "ALL" then L12252
               begactivity$ = all(hex(00))
               endactivity$ = hex(ff)
L12252:     if str(begwrkcntr$,,1)  = hex(00) and                        ~
               str(begactivity$,,1) = hex(00) then plowkey$ =all(hex(00))~
             else str(plowkey$,,8)  =                                    ~
                             begwrkcntr$ & begactivity$ addc all(hex(ff))
            count%   = 0%
            svwrkcntr$, svactivity$ = " "

        report_loop
            call "PLOWNEXT" (#2, plowkey$, 0%, f1%(2))
            if f1%(2) = 0% then end_report

            get #2 using L12296, wrkcntr$, activity$, costs(), bkts%(),   ~
                                labor_class$(), factor()
L12296:         FMT CH(4), CH(4), 6*PD(14,4), 6*BI(1), 4*CH(4), 4*PD(14,4)

*          IF WRKCNTR$ = " " OR ACTIVITY$ = " " THEN REPORT_LOOP
            if wrkcntr$ < begwrkcntr$ or wrkcntr$ > endwrkcntr$          ~
               then report_loop
            if activity$ < begactivity$ or activity$ > endactivity$      ~
               then report_loop

        REM Get descriptions
            readkey$ = str(wrkcntr$) & " "
            call "DESCRIBE" (#1%, readkey$, wrkdescr$, 1%, f1%(1))
            readkey$ = "WC ACTVTY" & str(activity$)
            call "DESCRIBE" (#4%, readkey$, actdescr$, 0%, f1%(4))

*       CHECK FOR WORK CENTER BREAK
            if wrkcntr$ = svwrkcntr$ then check_for_activity_break
               gosub print_new_work_center
               gosub print_column_hdrs
               svwrkcntr$ = wrkcntr$
               svactivity$ = activity$
               goto convert_data

        check_for_activity_break
            if activity$ = svactivity$ then convert_data
               svactivity$ = activity$
               print
               line% = line% + 1%
               goto convert_data

        convert_data
            for r% = 1% to 6%
                call "CONVERT" (costs(r%), 4.4, cost$(r%))
                if costs(r%) = 0% then cost$(r%) = " "
                gosub describe_bucket
            next r%

            for l% = 1% to 4%
                call "CONVERT" (factor(l%), 0.2, factor$(l%))
                readkey$ = "LBR CLASS" & str(labor_class$(l%))
                call "DESCRIBE" (#4,readkey$,labdescr$(l%),0%,f1%(4))
            next l%

        goto print_detail

        print_detail
            printed% = 0%
            for i% = 1% to 6%
                if costs(i%) = 0 then L12524
                   if line% < 56% then L12504
                      gosub page_heading
                      gosub print_column_hdrs
L12504:            if i% > 1% and printed% <> 0% then                    ~
                      activity$, actdescr$ = " "
                   print using L13070, activity$, actdescr$,costdesc$(i%),~
                                  cost$(i%), bucket$(i%), bktdesc2$(i%), ~
                                  bucketnbr$(i%)
                   printed% = 1%
                   line% = line% + 1%
L12524:     next i%
*          IF PRINTED% = 0% THEN REPORT_LOOP
            if printed% = 0% then print using L13070, activity$,actdescr$,~
                                                     " ", " ", " ", " ", ~
                                                     " "
                count% = count% + 1%
                print
                line% = line% + 1%
                for i% = 1% to 4%
                    if factor(i%) <> 0 then L12568
                       print
                       line% = line% + 1%
                       goto report_loop

L12568:                if i% > 1% then L12584
                          print using L13081, labor_class$(i%),           ~
                                             factor$(i%), labdescr$(i%)
                          goto L12592
L12584:                print using L13083, labor_class$(i%), factor$(i%), ~
                                          labdescr$(i%)
L12592:                line% = line% + 1%
                next i%
                print
                line% = line% + 1%
                goto report_loop

        print_new_work_center
            if page% = 0% or line% > 50% then gosub page_heading
            print using L13001, wrkcntr$, wrkdescr$
            print
            line% = line% + 2%
            return

        print_column_hdrs
            print using L13010
            print using L13050
            line% = line% + 2%
            return

        describe_bucket   /* Describe Pail for PAILS%(R%)    */
            bucket$(r%), bktdesc2$(r%), bucketnbr$(r%) = " "
            if bkts%(r%) = 0% then return
                readkey$ = "STC.BD." & str(userid$) & "."
                convert bkts%(r%) to str(readkey$,12,2), pic(#0)
                call "READ100" (#5, readkey$, f1%(5))
                if f1%(5) = 0% then return
                     get #5 using L12704, bucketnbr$(r%),                 ~
                                         bucket$(r%), bktdesc2$(r%)
L12704:                   FMT POS(12), CH(2), POS(21), CH(10), XX(4),    ~
                              CH(20)
                     return
        end_report
            if count% > 0% then L12740
                call "ASKUSER" (0%, "NOTHING TO PRINT",                  ~
                                "There are no codes to print.",  " ",    ~
                                "Press RETURN to Continue...")
                goto L12744
L12740:     print "** END OF REPORT **"
L12744:     call "SETPRNT" ("STC002", " ", 0%, 1%)
            close printer
            select crt
            for i% = 1% to 4%
                if labor_class$(i%) <> " " then L12753
                   factor$(i%) = " "
L12753:     next i%
            if editmode = 1 then goto editpg1 else goto inputmode

        page_heading
            page% = page% + 1%  :  line% = 5%
            print page
            print using L12804, date$, runtime$, company$
            print using L12812, page%
            print
            print using L12820, costset$, setdesc2$
            print
            return


L12804: %RUN DATE: ######## ########             ########################~
        ~####################################              STCWCASB:STC002
L12812: %                                                 WORK CENTER COS~
        ~TS AND DISTRIBUTION LISTING                           PAGE: ##
L12820: %COST SET: ########  ################################

L13001: %WORK CENTER: #### ################################
L13010: %   ACT     DESCRIPTION                       RATE CATEGORY      ~
        ~               RATE      BUCKET ID     BUCKET DESCRIPTION      NO
L13050: %   ----    ------------------------------    -------------------~
        ~---      ----------      ----------    --------------------    --
L13070: %   ####    ##############################    ###################~
        ~###      ##########      ##########    ####################    ##
L13081: %                        LABOR CLASS: ####     FACTOR: ####     D~
        ~ESCRIPTION: ##############################
L13083: %                                     ####             ####      ~
        ~            ##############################

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20230,         /* Work Center/Activity */  ~
                              L20280,         /* Costs                */  ~
                              L20400,         /* Labor Class (1)      */  ~
                              L21000,         /* Factor  (1)          */  ~
                              L21060,         /* Labor Class (2)      */  ~
                              L21080,         /* Factor  (2)          */  ~
                              L21140,         /* Labor Class (3)      */  ~
                              L21160,         /* Factor  (3)          */  ~
                              L21220,         /* Labor Class (4)      */  ~
                              L21240          /* Factor  (4)          */
            return
L20230: REM Def/Enable Work Center                 WRKCNTR$
            inpmessage$ = "Enter work center/activity or '?' to see valid~
        ~ codes"
            return
L20280: REM Def/Enable Costs                       COST$()
            inpmessage$ = "Enter cost distribution"
            return
L20400: REM Def/Enable Labor Class (1)             LABOR_CLASS$(1)
            inpmessage$ = "Enter blank, labor class, or '?' to see valid ~
        ~codes"
            return
L21000: REM Def/Enable Factor  (1)                 FACTOR$(1)
            if labor_class$(1) = " " then L21040
               if factor$ (1) = " " then factor$ (1) = "   1"
               inpmessage$ = "Enter multiplication factor"
               return
L21040:     enabled% = 0%
            labptr = 1
            return
L21060: REM Def/Enable Labor Class (2)             LABOR_CLASS$(2)
            inpmessage$ = "Enter blank, labor class, or '?' to see valid ~
        ~codes"
            return
L21080: REM Def/Enable Factor  (2)                 FACTOR$(2)
            if labor_class$(2) = " " then L21120
               if factor$ (2) = " " then factor$ (2) = "   1"
               inpmessage$ = "Enter multiplication factor and description"
               return
L21120:     enabled% = 0%
            labptr = 2
            return
L21140: REM Def/Enable Labor Class (3)             LABOR_CLASS$(3)
            inpmessage$ = "Enter blank, labor class, or '?' to see valid ~
        ~codes"
            return
L21160: REM Def/Enable Factor  (3)                 FACTOR$(3)
            if labor_class$(3) = " " then L21200
               if factor$ (3) = " " then factor$ (3) = "   1"
               inpmessage$ = "Enter multiplication factor and description"
               return
L21200:     enabled% = 0%
            labptr = 3
            return
L21220: REM Def/Enable Labor Class (4)             LABOR_CLASS$(4)
            inpmessage$ = "Enter blank, labor class, or '?' to see valid ~
        ~codes"
            return
L21240: REM Def/Enable Factor  (4)                 FACTOR$(4)
            if labor_class$(4) = " " then L21280
               if factor$ (4) = " " then factor$ (4) = "   1"
               inpmessage$ = "Enter multiplication factor and description"
               labptr = 0
               return
L21280:     enabled% = 0%
            labptr = 4
            return

        deffn'052(fieldnr%)
            inpmessage$ = " "
            on fieldnr% gosub L22150,         /* Work Center          */  ~
                              L22190          /* Activity             */
            return

L22150: REM Work Center
            inpmessage$ = "Enter range for work center"
            begwrkcntr$ = "ALL"
            return

L22190: REM Activity
            inpmessage$ = "Enter range for activity"
            begactivity$ = "ALL"
            return

L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$,                            ~
                      wrkcntr$               , /* Work Center        */  ~
                      wrkdescr$              , /* WC Description     */  ~
                      activity$              , /* Activity Code      */  ~
                      bucket$()              , /* Bucket Names       */  ~
                      bktdesc2$()            , /* Bucket Description */  ~
                      bucketnbr$()           , /* Bucket Numbers     */  ~
                      cost$()                , /* Costs              */  ~
                      actdescr$              , /* Activity Descript  */  ~
                      factor$()              , /* Multiplication Fac */  ~
                      labor_class$()         , /* Labor Class        */  ~
                      labdescr$()              /* Labor Description  */

            mat factor = zer
            mat bkts% = zer
            mat costs = zer

            newact, writeact = 0
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
        dataload
           get #2, using L30080,inwrkcntr$,inactivity$,costs(),bkts%(),   ~
                                 labor_class$(), factor()
L30080:     FMT CH(4), CH(4), 6*PD(14,4), 6*BI(1), 4*CH(4), 4*PD(14,4)

            if nfgflag$ <> "F" then pf12flag = 1
            if wrkcntr$ = " " then wrkcntr$ = inwrkcntr$
            if activity$ = " " then activity$ = inactivity$

            for i% = 1% to 6%
                if costs(i%) = 0 then L30210
                call "CONVERT" (costs(i%), 4.4, cost$(i%))
                j% = bkts%(i%)
                bucket$(i%) = bktids$(j%)
                bktdesc2$(i%) = bktdescr$(j%)
                if costs(i%) = 0 then L30210
                   readkey$ = "STC.BD." & str(userid$) & "."
                   convert j% to str(readkey$,12,2), pic(#0)
                   call "READ100" (#5, readkey$, f1%(5))
                   if f1%(5) = 0% then L30210
                      get #5 using L30197, bucketnbr$(i%)
L30197:               FMT POS(12), CH(2)
L30210:     next i%

            for i% = 1% to 4%
                if labor_class$(i%) <> " " then L30270
                   factor$(i%) = " "
                   labptr = i%
                   goto L30320

L30270:         call "CONVERT" (factor(i%), 0.2, factor$(i%))
                plowkey$ = "LBR CLASS" & str(labor_class$(i%)) & str(" ")
                call "DESCRIBE" (#4, plowkey$, labdescr$(i%), 0%, f1%(4))
            next i%

L30320:     return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            readkey$ = str(wrkcntr$) & str(activity$)
            call "READ101" (#2, readkey$, f1%(2))
            if dmode = 1 then L31140
            if f1%(2) <> 1% then L31160
               get #2, using L31100, rec$
L31100:        FMT CH(110)
               put rec2$ using L31121,wrkcntr$, activity$, cost$, bkts%(),~
                                     labor_class$(), factor()
L31121:        FMT CH(4), CH(4), CH(48), 6*BI(1), 4*CH(4), 4*PD(14,4)
               if rec$ = rec2$ then return
L31140:        delete #2
               if dmode = 1 then L31280
L31160:     call "PACKZERO" (costs(), cost$)
            write #2 using L31190, wrkcntr$, activity$, cost$, bkts%(),   ~
                                  labor_class$(), factor(), " ", " "
L31190:     FMT CH(4), CH(4), CH(48), 6*BI(1), 4*CH(4), 4*PD(14,4),      ~
                CH(250), CH(21)

            if writeact <> 1 then L31280
            write #4, using L31250, "WC ACTVTY", str(activity$), " ",     ~
                                    str(actdescr$), " 4", " "
L31250:     FMT CH(9), CH(4), CH(11), CH(30), CH(2), CH(72)
            writeact = 0

L31280:     call "READ100" (#2, readkey$, f1%(2))
            if nfgflag$ = "Y" then L31370
               readkey$ = "STC.HDR." & costset$
               call "READ101" (#5, readkey$, f1%(5))
               put #5 using L31330, "Y"
L31330:            FMT POS(420), CH(1)
               rewrite #5
               nfgflag$ = "Y"

L31370:     return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              line2$ = "Cost Set: " & costset$ & " " & setdesc2$
              str(line2$,62%) = "STCWCASB: " & str(cms2v$,,8%)

        REM Handle PF12
              if pf12flag <> 1 or nfgflag$ = "F" then L40064
                 pf12$ = "(12)Delete Record"
                 str(pfkeys$,6,1) = hex(0c)
                 goto L40076

L40064:          pf12$ = " "
                 str(pfkeys$,6,1) = hex(00)

L40076: REM Handle PF11
              if labptr = 0 or nfgflag$ = "F" then L40104
              if editmode <> 1 then L40104
                 pf11$ = "(11)Add Labor Codes"
                 str(pfkeys$,5,1) = hex(0b)
                 goto L40116

L40104:       pf11$ = " "
              str(pfkeys$,5,1) = hex(00)

L40116: REM Handle PF9
              if fieldnr% <> 1 then L40140
                 pf9$ = "(9)WC/Act This Costset"
              str(pfkeys$,4,1) = hex(09)
              goto L40152

L40140:       pf9$ = " "
              str(pfkeys$,4,1) = hex(00)

L40152: REM Set facs appropriately for edit mode
              if editmode <> 1 then L40192
                 init (hex(8c)) afac$, bfac$, dfac$(), lfac$()
                 if nfgflag$ = "F" then L40388
                    if fieldnr% > 0 then  L40316
                       init(hex(86)) lfac$()
                       bfac$ = hex(84)
                       init (hex(8c)) lfac$(1)
                       goto L40388

L40192: REM Set facs for input mode
              afac$ = hex(8c)
              if keyhit% <> 4 then L40232
                 lfac$(fieldnr% + 1) = hex(8c)
                 if fieldnr% <> 2 then bfac$ = hex(8c)
                 if fieldnr% <> 1 then L40316
                    afac$ = hex(80)
                    newact = 1
                    goto L40388

L40232:       if fieldnr% <> 1% then L40264
                 bfac$ = hex(9c)
                 init (hex(9c)) dfac$(), lfac$()
                 if newact <> 1 then L40316
                    lfac$(1) = hex(8c)
                    afac$ = hex(80)
                    goto L40388

L40264: REM Set facs for previous fields
              for i% = 1% to fieldnr% - 1%
                  lfac$(i%) = hex(8c)
              next i%

        REM Set facs for input mode on labor fields
              bfac$ = hex(8c)
              if fieldnr% < 4 then L40316
              r% = mod(fieldnr%-4, 2)
              if r% > 0 then L40316
                 s% = 1 + ((fieldnr% - 4)/2)
                 dfac$(s%) = hex(8c)

L40316:       on fieldnr% gosub L40368,         /* Work Center       */   ~
                                L40372,         /* Costs             */   ~
                                L40368,         /* Labor Class (1)   */   ~
                                L40372,         /* Factor (1)        */   ~
                                L40368,         /* Labor Class (2)   */   ~
                                L40372,         /* Factor (2)        */   ~
                                L40368,         /* Labor Class (3)   */   ~
                                L40372,         /* Factor (3)        */   ~
                                L40368,         /* Labor Class (4)   */   ~
                                L40372          /* Factor (4)        */
               goto L40388

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40368:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40372:           lfac$(fieldnr%) = hex(82)             /* Numeric    */
                      if fieldnr% = 2 then bfac$ = hex(81)
                      return

L40388:     accept                                                       ~
               at (01,02),                                               ~
                  "Work Center - Activity Data",                         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Work Center",                                ~
               at (05,26), fac(lfac$( 1)), wrkcntr$             , ch(04),~
               at (05,37), fac(hex(8c))  , wrkdescr$            , ch(32),~
                                                                         ~
               at (06,02), "Activity Code",                              ~
               at (06,26), fac(lfac$( 1)), activity$            , ch(04),~
               at (06,37), fac(afac$)    , actdescr$            , ch(32),~
                                                                         ~
               at (08,02), "Standards",                                  ~
               at (08,20), fac(hex(ac)) , constant1$            , ch(10),~
               at (08,37), fac(hex(ac)) , constant2$            , ch(10),~
               at (08,54), fac(hex(ac)) , constant3$            , ch(24),~
                                                                         ~
               at (09,04), "Fixed    #1",                                ~
               at (09,20), fac(lfac$( 2)), cost$(1)             , ch(10),~
               at (09,37), fac(bfac$),     bucket$(1)           , ch(10),~
               at (09,54), fac(hex(8c)), bktdesc2$(1)           , ch(20),~
               at (09,76), fac(hex(8c)), bucketnbr$(1)          , ch(02),~
                                                                         ~
               at (10,13), "#2",                                         ~
               at (10,20), fac(lfac$( 2)), cost$(2)             , ch(10),~
               at (10,37), fac(bfac$),     bucket$(2)           , ch(10),~
               at (10,54), fac(hex(8c)), bktdesc2$(2)           , ch(20),~
               at (10,76), fac(hex(8c)), bucketnbr$(2)          , ch(02),~
                                                                         ~
               at (11,04), "Per Part #1",                                ~
               at (11,20), fac(lfac$( 2)), cost$(3)             , ch(10),~
               at (11,37), fac(bfac$), bucket$(3)               , ch(10),~
               at (11,54), fac(hex(8c)), bktdesc2$(3)           , ch(20),~
               at (11,76), fac(hex(8c)), bucketnbr$(3)          , ch(02),~
                                                                         ~
               at (12,13), "#2",                                         ~
               at (12,20), fac(lfac$( 2)), cost$(4)             , ch(10),~
               at (12,37), fac(bfac$),     bucket$(4)           , ch(10),~
               at (12,54), fac(hex(8c)), bktdesc2$(4)           , ch(20),~
               at (12,76), fac(hex(8c)), bucketnbr$(4)          , ch(02),~
                                                                         ~
               at (13,04), "Per Hour #1",                                ~
               at (13,20), fac(lfac$( 2)), cost$(5)             , ch(10),~
               at (13,37), fac(bfac$),     bucket$(5)           , ch(10),~
               at (13,54), fac(hex(8c)), bktdesc2$(5)           , ch(20),~
               at (13,76), fac(hex(8c)), bucketnbr$(5)          , ch(02),~
                                                                         ~
               at (14,13), "#2",                                         ~
               at (14,20), fac(lfac$( 2)), cost$(6)             , ch(10),~
               at (14,37), fac(bfac$),     bucket$(6)           , ch(10),~
               at (14,54), fac(hex(8c)), bktdesc2$(6)           , ch(20),~
               at (14,76), fac(hex(8c)), bucketnbr$(6)          , ch(02),~
                                                                         ~
               at (15,02), fac(hex(8c)),  constant4$            , ch(11),~
               at (15,22), fac(hex(ac)),  constant5$            , ch(06),~
               at (15,37), fac(hex(ac)),  constant6$            , ch(32),~
                                                                         ~
               at (16,05), fac(lfac$( 3)), labor_class$(1)      , ch(04),~
               at (16,23), fac(lfac$( 4)), factor$(1)           , ch(04),~
               at (16,37), fac(dfac$( 1)), labdescr$(1)         , ch(32),~
                                                                         ~
               at (17,05), fac(lfac$( 5)), labor_class$(2)      , ch(04),~
               at (17,23), fac(lfac$( 6)), factor$(2)           , ch(04),~
               at (17,37), fac(dfac$( 2)), labdescr$(2)         , ch(32),~
                                                                         ~
               at (18,05), fac(lfac$( 7)), labor_class$(3)      , ch(04),~
               at (18,23), fac(lfac$( 8)), factor$(3)           , ch(04),~
               at (18,37), fac(dfac$( 3)), labdescr$(3)         , ch(32),~
                                                                         ~
               at (19,05), fac(lfac$( 9)), labor_class$(4)      , ch(04),~
               at (19,23), fac(lfac$(10)), factor$(4)           , ch(04),~
               at (19,37), fac(dfac$( 4)), labdescr$(4)         , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,17), fac(hex(8c)), pf9$                   , ch(22),~
               at (22,42), fac(hex(8c)), pf11$                  , ch(19),~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,42), fac(hex(8c)), pf12$                  , ch(17),~
               at (23,65), "(15)Print Screen",                           ~
               at (24,02), fac(hex(8c)), pf6$                   , ch(14),~
               at (24,20), fac(hex(8c)), pf8$                   , ch(13),~
               at (24,42), fac(hex(8c)), pf14$                  , ch(16),~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L40776
                  call "MANUAL" ("STCWCASB")
                  goto L40388

L40776:        if keyhit% <> 15 then L40792
                  call "PRNTSCRN"
                  goto L40388

L40792:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        deffn'102(fieldnr%)
              line2$ = "Cost Set: " & costset$ & " " & setdesc2$
              str(line2$,62%) = "STCWCASB: " & str(cms2v$,,8%)
              if fieldnr% > 0 then init (hex(8c)) lfac$() else           ~
                 init (hex(84)) lfac$()
              on fieldnr% gosub L41140,         /* Work Center       */   ~
                                L41140          /* Costs             */
               goto L42000

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L41140:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42000:     accept                                                       ~
               at (01,02),                                               ~
                  "Work Center - Activity Data",                         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Work Center",                                ~
               at (06,22), "From:",                                      ~
               at (06,30), fac(lfac$(1)),begwrkcntr$            , ch(04),~
               at (06,38), "To:",                                        ~
               at (06,44), fac(lfac$(1)),endwrkcntr$            , ch(04),~
                                                                         ~
               at (07,02), "Activity",                                   ~
               at (07,30), fac(lfac$(2)),begactivity$           , ch(04),~
               at (07,44), fac(lfac$(2)),endactivity$           , ch(04),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)First Field",                             ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L43070
                  call "MANUAL" ("STCWCASB")
                  goto L40388

L43070:        if keyhit% <> 15 then L43110
                  call "PRNTSCRN"
                  goto L40388

L43110:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        totals_screen
            line2$ = "Cost Set: " & costset$ & " " & setdesc2$
            str(line2$,62%) = "STCWCASB: " & str(cms2v$,,8%)

L43140:     accept                                                       ~
               at (01,02),                                               ~
                  "Work Center - Activity Data",                         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,20), fac(hex(ac)) , constant7$            , ch(11),~
               at (06,37), fac(hex(ac)) , constant8$            , ch(11),~
               at (06,54), fac(hex(ac)) , constant9$            , ch(11),~
                                                                         ~
               at (08,04), "Fixed",                                      ~
               at (08,20), fac(hex(8c)),   wrkcost$(1)          , ch(10),~
               at (08,37), fac(hex(8c)),   labcost$(1)          , ch(10),~
               at (08,54), fac(hex(8c)),   totcost$(1)          , ch(10),~
                                                                         ~
               at (09,04), "Per Part",                                   ~
               at (09,20), fac(hex(8c)),   wrkcost$(2)          , ch(10),~
               at (09,37), fac(hex(8c)),   labcost$(2)          , ch(10),~
               at (09,54), fac(hex(8c)),   totcost$(2)          , ch(10),~
                                                                         ~
               at (10,04), "Per Hour",                                   ~
               at (10,20), fac(hex(8c)),   wrkcost$(3)          , ch(10),~
               at (10,37), fac(hex(8c)),   labcost$(3)          , ch(10),~
               at (10,54), fac(hex(8c)),   totcost$(3)          , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,17), fac(hex(8c)), pf9$                   , ch(22),~
               at (22,42), fac(hex(8c)), pf11$                  , ch(19),~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,42), fac(hex(8c)), pf12$                  , ch(17),~
               at (23,65), "(15)Print Screen",                           ~
               at (24,42), fac(hex(8c)), pf14$                  , ch(16),~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L43900
                  call "MANUAL" ("STCWCASB")
                  goto L43140

L43900:        if keyhit% <> 15 then L43932
                  call "PRNTSCRN"
                  goto L43140

L43932:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return
        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50080,         /* Work Center/Activ */     ~
                              L50372,         /* Costs             */     ~
                              L50628,         /* Labor Code (1)    */     ~
                              L50688,         /* Factor/Descr (1)  */     ~
                              L50708,         /* Labor Code (2)    */     ~
                              L50768,         /* Factor/Descr (2)  */     ~
                              L50788,         /* Labor Code (3)    */     ~
                              L50848,         /* Factor/Descr (3)  */     ~
                              L51000,         /* Labor Code (4)    */     ~
                              L51180          /* Factor/Descr (4)  */
            return

L50080: REM Test for Work Center/Activity
            if newact = 1 then new_activity
            if wrkcntr$ <> " " or activity$ <> " " then check_work_center
               errormsg$="Both Work Center and Activity Code cannot be bl~
        ~ank!"
               return

        check_work_center
            if wrkcntr$ = " " then check_activity
               plowkey$ = str(wrkcntr$) & " "
               wrkdescr$ = hex(06) & "Select Valid Work Center"
               call "GETCODE" (#1%, plowkey$, wrkdescr$, 1%, 0, f1%(1))
               if f1%(1) <> 1% then L50164
                  wrkcntr$ = str(plowkey$,1,4)
                  if activity$ <> " " then check_activity
                     gosub read_costset_file
                     if f1%(2) <> 1 then return
                        gosub dataload
                        return clear all
                        goto editpg1

L50164:        errormsg$ = "Invalid Work Center"
               return

        check_cost_set
            readkey$ = str(wrkcntr$) & str(activity$)
            call "READ100" (#2%, readkey$, f1%(2))
            if f1%(2) <> 1% then L50220
               gosub dataload
               return clear all
               if nfgflag$ <> "F" then goto editpg1
                  edtmessage$ = "This cost set is frozen and may not be e~
        ~dited."
                  goto editpg1

L50220:     readkey$ = "    " & str(activity$)
            call "READ100" (#2, readkey$, f1%(2))
            if f1%(2) <> 1% then L50244
               gosub dataload
               return

L50244:     readkey$ = str(wrkcntr$) & "    "
            call "READ100" (#2, readkey$, f1%(2))
            if f1%(2) <> 1% then return
               gosub dataload
               return

        check_activity
            plowkey$ = "WC ACTVTY" & activity$
            actdescr$ = hex(06) & "Select Activity Code"
            call "PLOWCODE" (#4%,plowkey$,actdescr$,9%,0.3,f1%(4))
            if f1%(4) <> 1% then new_activity
               activity$ = str(plowkey$,10,4)
               if actdescr$ <> " " then call "PUTPAREN" (actdescr$)
               goto check_cost_set

        new_activity
        if newact = 1 then L50324
               newact = 1
               gosub check_cost_set
               return
L50324:     if actdescr$ <> " " then L50336
               errormsg$ = "Activity Description cannot be blank"
               return
L50336:     newact = 0
            writeact = 1
            return

        read_costset_file
               readkey$ = str(wrkcntr$) & str(activity$)
               call "READ100" (#2, readkey$, f1%(2))
               return

L50372: REM Test for Costs
            for i% = 1% to 6%
                if cost$(i%) <> " " or bucket$(i%) <> " " then L50408
L50384:            bkts%(i%) = 0
                   bktdesc2$(i%) = " "
                   bucketnbr$(i%) = " "
                   costs(i%) = 0
                   goto L50576

L50408:         if cost$(i%) = " " and bucket$(i%) <> " " then L50600
                call "NUMTEST" (cost$(i%),0,9e7,errormsg$,-4.4,costs(i%))
                if errormsg$ <> " " then return
                     if costs(i%) > 0 then L50428
                     cost$(i%) = " "
                     if cost$(i%) = " " and bucket$(i%) <> " " then L50600
                     goto L50384
L50428:         if bucket$(i%) = " " then L50480
                search str(bktids$()) = bucket$(i%) to srch() step 10
                if srch(1) = 0 then L50480
                  j%, bkts%(i%) = (srch(1)+9)/10
                  bucket$(i%) = bktids$(j%)
                  bktdesc2$(i%) = bktdescr$(j%)
                  readkey$ = "STC.BD." & str(userid$) & "."
                  convert j% to str(readkey$,12,2), pic(#0)
                  call "READ100" (#5, readkey$, f1%(5))
                  if f1%(5) = 0% then L50576
                     get #5 using L50512, bucketnbr$(i%)
                  goto L50576

L50480:         misc$ = hex(06) & "Select a Standard Cost bucket"
                plowkey$ = "STC.BD." & str(userid$) & "." & hex(00)
                f1%(5) = -(i% + 8%)
                call "PLOWCODE" (#5, plowkey$, misc$, 11%, .34, f1%(5))
                if f1%(5) = 1% then goto L50508
                   errormsg$ = "Cost bucket not found.  Try again."
                   return
L50508:        get #5 using L50512, bucketnbr$(i%)
L50512:            FMT POS(12), CH(2)
               convert str(plowkey$, 12, 2) to bkts%(i%),                ~
                   data goto L50612
               j% = bkts%(i%)
               if bucket$(i%) <> bktids$(j%) then prntflag = 1
               bucket$(i%) = bktids$(j%)
               bktdesc2$(i%) = str(misc$,15,20)
               if prntflag <> 1 then L50576
                  prntflag = 0
                  str(line$,1,10) = str(cost$(i%))
                  str(line$,11,7) = "      "
                  str(line$,18,10) = str(bucket$(i%))
                  str(line$,28,7) = "      "
                  str(line$,35,22) = str(bktdesc2$(i%))
                  str(line$,57,2) = str(bucketnbr$(i%))
                  print at(i%+8,19); hex(8c);line$
L50576:     next i%
            return

            errormsg$ = "All costs have not been assigned to a bucket"
            return

L50600:     errormsg$ = "One or more buckets has no corresponding amount"
            return

L50612:     errormsg$ = "Data conversion error on bucket number from PLOW~
        ~CODE"
            return

L50628: REM Test for Labor Class (1)
            if labor_class$(1) = " " then squeeze_labor_class
            plowkey$ = "LBR CLASS" & str(labor_class$(1))
            labdescr$(1) = hex(06) & "Select Labor Class Code"
            call "PLOWCODE" (#4,plowkey$,labdescr$(1),9%,0.32,f1%(4))
            if f1%(4) <> 1 then L50676
               labor_class$(1) = str(plowkey$,10,4)
            search labor_class$() = labor_class$(1) to srch() step 4
            if srch(2) = 0 then return
            errormsg$ = "Labor class code already defined on this record"
            return

L50676:     errormsg$ = "Labor Class not found.  Try again."
            return

L50688: REM Test for Factor
            call "NUMTEST" (factor$(1),0,9999,errormsg$,-0.2,factor(1))
            if errormsg$ <> " " then return
            return

L50708: REM Test for Labor Class (2)
            if labor_class$(2) = " " then squeeze_labor_class
            plowkey$ = "LBR CLASS" & str(labor_class$(2))
            labdescr$(2) = hex(06) & "Select Labor Class Code"
            call "PLOWCODE" (#4,plowkey$,labdescr$(2),9%,0.32,f1%(4))
            if f1%(4) <> 1 then L50756
               labor_class$(2) = str(plowkey$,10,4)
               search labor_class$() = labor_class$(2) to srch() step 4
               if srch(2) = 0 then return
            errormsg$ = "Labor class code already defined on this record"
            return

L50756:     errormsg$ = "Labor Class not found.  Try again."
            return

L50768: REM Test for Factor
            call "NUMTEST" (factor$(2),0,9999,errormsg$,-0.2, factor(2))
            if errormsg$ <> " " then return
            return

L50788: REM Test for Labor Class (3)
            if labor_class$(3) = " " then squeeze_labor_class
            plowkey$ = "LBR CLASS" & str(labor_class$(3))
            labdescr$(3) = hex(06) & "Select Labor Class Code"
            call "PLOWCODE" (#4,plowkey$,labdescr$(3),9%,0.32,f1%(4))
            if f1%(4) <> 1 then L50836
               labor_class$(3) = str(plowkey$,10,4)
            search labor_class$() = labor_class$(3) to srch() step 4
            if srch(2) = 0 then return
            errormsg$ = "Labor class code already defined on this record"
            return

L50836:     errormsg$ = "Labor Class not found.  Try again."
            return

L50848: REM Test for Factor
            call "NUMTEST" (factor$(3),0,9999,errormsg$,-0.2, factor(3))
            if errormsg$ <> " " then return
            return

L51000: REM Test for Labor Class (4)
            if labor_class$(4) = " " then squeeze_labor_class
            plowkey$ = "LBR CLASS" & str(labor_class$(4))
            labdescr$(4) = hex(06) & "Select Labor Class Code"
            call "PLOWCODE" (#4,plowkey$,labdescr$(4),9%,0.32,f1%(4))
            if f1%(4) <> 1 then L51150
               labor_class$(4) = str(plowkey$,10,4)
            search labor_class$() = labor_class$(4) to srch() step 4
            if srch(2) = 0 then return
            errormsg$ = "Labor class code already defined on this record"
            return

L51150:     errormsg$ = "Labor Class not found.  Try again."
            return

L51180: REM Test for Factor
            call "NUMTEST" (factor$(4),0,9999,errormsg$,-0.2,factor(4))
            if errormsg$ <> " " then return
            return

        squeeze_labor_class
            j% = fieldnr%/2
            factor$(j%) = " "  :  factor(j%) = 0
            labdescr$(j%) = " "
            if j% <> 4% then L51240
               labptr = 4
               return

L51240:     for i% = j% to 3%
                labor_class$(i%) = labor_class$(i%+1%)
                factor$(i%) = factor$(i%+1%)
                labdescr$(i%) = labdescr$(i%+1%)
                labor_class$(i%+1%) = " "
                factor$(i%+1%) = " " :  factor(i%+1%) = 0
                labdescr$(i%+1%) = " "
            next i%
            if labptr = 0 then labptr = 4 else labptr = labptr - 1
            return

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L52720,         /* Work Center/Activ */     ~
                              L52850          /* Costs             */
            return

L52720: REM Work Center
            if begwrkcntr$ <> " " or endwrkcntr$ <> " " then L52730
            errormsg$ = "Both beginning and ending range cannot be blank"

L52730:     if begwrkcntr$ = "ALL" then return
            if begwrkcntr$ = " " then begwrkcntr$ = str(hex(00))
            if endwrkcntr$ <> " " then L52820
               endwrkcntr$ = str(begwrkcntr$)
               return

L52820:     if begwrkcntr$ > endwrkcntr$ then errormsg$ = "Invalid Range"
            return

L52850: REM Activity
            if begactivity$ = "ALL" then return
            if begactivity$ <> " " or endactivity$ <> " " then L52920
            errormsg$ = "Both beginning and ending range cannot be blank"
L52920:     if begactivity$ = " " then begactivity$ = str(hex(00))
            if endactivity$ <> " " then L52970
               endactivity$ = str(begactivity$)
               return

L52970:   if begactivity$ > endactivity$ then errormsg$ = "Invalid Range"
            return

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
            end
