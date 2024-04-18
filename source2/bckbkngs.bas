        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBB    CCC   K   K  BBBB   K   K  N   N   GGG    SSS    *~
            *  B   B  C   C  K  K   B   B  K  K   NN  N  G      S       *~
            *  BBBB   C      KKK    BBBB   KKK    N N N  G GGG   SSS    *~
            *  B   B  C   C  K  K   B   B  K  K   N  NN  G   G      S   *~
            *  BBBB    CCC   K   K  BBBB   K   K  N   N   GGG    SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKBKNGS - Reads records in 1st pass from file BCKBKGRF   *~
            *            and prints a report.  Reads file in 2nd pass   *~
            *            and deletes same records.  Activity is based on*~
            *            a date/time stamp entered by the operator.     *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/19/86 ! Original                                 ! JRH *~
            * 02/02/88 ! Add printing of new cancelled orders and ! JDH *~
            *          !   change old cancelled to deleted        !     *~
            * 05/13/88 ! Honor BCKflag for deleted SOs            ! JDH *~
            * 01/12/94 ! Fixed Reason printing after blank reason.! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            allow_delete$1,              /* Allow deleteion of SOs Flag*/~
            catdesc$23,                  /* Category descriptions      */~
            category$17,                 /* Pass/category name         */~
            catgy_table$(4)17,           /* Pass/category descriptions */~
            company_name$60,             /* Name of company (COMPNAME) */~
            cursor%(2),                  /* Cursor location for edit   */~
            customer_code$9,             /* Customer code from BCKBKGRF*/~
            customer_name$30,            /* Customer name from BCKBKGRF*/~
            cutoff_date$8,               /* Backlog Booking Cutoff Date*/~
            cutoff_key$7,                /* For comparison to the file */~
            cutoff_time$5,               /* Backlog Booking Cutoff Time*/~
            date$8,                      /* Date for screen display    */~
            delete_yorn$1,               /* To delete or not to delete */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            reason$30,                   /* Reason code description    */~
            reason_code$9,               /* Reason code for adjustments*/~
            reason_key$24,               /* Key to GENCODES for reason */~
            rec_date_time$7,             /* Date & time from BCKBKGRF  */~
            record_type$1,               /* Record type from BCKBKGRF  */~
            rptid$6,                     /* Report ID                  */~
            so_nbr$16,                   /* Sales Order # from BCKBKGRF*/~
            sub_hdr$80,                  /* Report heading description */~
            time$8,                      /* Time of day for Headers    */~
            userid$3,                    /* Current User Id            */~
            yorn_hdr$16                  /* Description of DELETE_YORN$*/

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
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
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
            * #1  ! BCKBKGRF ! Sales Order Bookings Report File         *~
            * #2  ! GENCODES ! 1099 VALIDATION                          *~
            * #3  ! SYSFILE2 ! Caelus Managment System Gerneral Info    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "BCKBKGRF",                                      ~
                        varc,     indexed,  recsize = 88,                ~
                        keypos = 1,    keylen =  33

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #3,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            call "SHOSTAT" ("Opening Files, One Moment Please")

                rslt$(1 ) = "REQUIRED"
            call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 0%, rslt$(1 ))
                rslt$(2 ) = "REQUIRED"
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))
            call "OPENCHCK" (#3,  fs%( 3), f2%( 3), 0%, rslt$( 3))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "COMPNAME" (12%, company_name$, u3%)
            catgy_table$(1) = "NEW ORDERS:"
            catgy_table$(2) = "ADJUSTED ORDERS:"
            catgy_table$(3) = "DELETED ORDERS:"
            catgy_table$(4) = "CANCELLED ORDERS:"
            max_lines% = 56% : temp = 0
            rptid$ = "BCK001"
            call "EXTRACT" addr("ID", userid$)
            date$ = date : call "DATEFMT" (date$)
            reason_key$ = "SO REASON"
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

*        Get flag to see if they allow deletion of sales orders
            call "BCKSWTCH" ("BCK", "DELETE", allow_delete$, temp, u3%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$=" " : pf5$=" ": pf16$="(16)Exit Program"
            init(" ") errormsg$, inpmessage$, cutoff_date$, cutoff_time$
            delete_yorn$ = "N" /* Default is not to delete the records */

            for fieldnr% = 1 to 2
L10120:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10240
L10140:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10220
L10170:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10140
                         if fieldnr% = 1% then L10120
                         goto L10170
L10220:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10140
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10140
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            inpmessage$ = edtmessage$
            pf4$  = " "
            pf5$  = " "
            pf16$ = "(16)Print Data"
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       process_file
                  if keyhit% <>  0 then       edtpg1
L11150:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  2 then edtpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
                  pf4$, pf5$, pf16$ = " "
L11200:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11200
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11200
                  if cursor%(1) - 5 <> fieldnr% then L11150
            goto edtpg1

        REM *************************************************************~
            *     P R O C E S S   B A C K L O G   B O O K I N G S       *~
            *-----------------------------------------------------------*~
            * Prints & deletes from BCKBKGRF after input/edit routines. *~
            *************************************************************

        process_file
            convert str(cutoff_time$,,2) & str(cutoff_time$,4,2) & "0000"~
                to cutoff_time%
            put cutoff_key$, using L13100, cutoff_date%, cutoff_time%
L13100:         FMT BI(3), BI(4)
        REM *************************************************************~
            * CUTOFF_KEY$ NOW SET FOR COMPARE TO DATA FILE TIME & DATE  *~
            * STAMP.                                                    *~
            *************************************************************
            sub_hdr$ = "SALES ORDERS ENTERED THROUGH " & cutoff_date$ &  ~
                " @ " & cutoff_time$ & " -- "
            yorn_hdr$ = "(NOT DELETED)"
            if delete_yorn$ = "Y" then yorn_hdr$ = "(DELETED)"
            call "FMTTITLE" (sub_hdr$, yorn_hdr$, 2%)
            rtotal_gross, rtotal_disc, rtotal_net = 0 /*Zero Run Totals*/
            select printer(134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            page_nbr% = 0% : line_nbr% = 99%       /*Init print fields*/
            time$ = " " : call "TIME" (time$)

        REM THE FOLLOWING IS THE RECORD PRINTING LOGIC ******************
            call "SHOSTAT" ("Printing Open Order Bookings")
            for pass% = 1% to 4%
             if pass% = 3% and allow_delete$ = "N" then L13330
                category$ = catgy_table$ (pass%)
                convert pass% to plowkey$, pic (#)
                str(plowkey$,2,98) = all(hex(00))
                if pass% = 4% then reason_key$ = "CANREASON"
                gosub data_print_loop
L13330:     next pass%
            if line_nbr% > max_lines% then gosub page_heading
            print
            catdesc$ = "**** REPORT TOTALS:"
            call "FMTTITLE" (catdesc$, " ", 1%)
            print using L60220, catdesc$, rtotal_gross, rtotal_disc,      ~
                rtotal_net
            if line_nbr% > max_lines% then gosub page_heading
            print
            print using L60260    /* End of Report */
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)

        REM THE FOLLOWING IS THE RECORD DELETION LOGIC ******************
            if delete_yorn$ = "N" then goto inputmode
            call "SHOSTAT" ("Now Deleting Open Order Bookings")
            plowkey$ = all(hex(00))      /* Read entire file ... */
            brk% = 0%                    /* ... with no breaks */
        data_delete_read
            gosub bckbkgrf_reader
            if f1%(1) = 0 then goto inputmode
            delete #1          /* Record qualifies ... delete it */
            goto data_delete_read

        REM *************************************************************~
            *       M A I N   P R O C E S S I N G   L O O P             *~
            *-----------------------------------------------------------*~
            * Reads the file, selects records by CATEGORY$, prints de-  *~
            * tail records and category totals.                         *~
            *************************************************************~

        data_print_loop
            if line_nbr% > max_lines% - 1% then gosub page_heading
            print : print using L60250, category$
            line_nbr% = line_nbr% + 2%
            total_gross, total_disc, total_net = 0    /*Category totals*/
            brk% = 1%     /* Break on one character only */

        data_print_read
            gosub bckbkgrf_reader
            if f1%(1) = 0% then data_print_sub_totals  /* Break */
            if reason_code$ <> " " then L14190    /*Record qualifies ...*/
                reason$ = " "
                str(reason_key$,10,9) = reason_code$
                goto compute_net    /*... print it*/
L14190:     if reason_code$ = str(reason_key$,10,9) then compute_net
            str(reason_key$,10,9) = reason_code$
            call "READ100" (#2, reason_key$, f1%(2))
            reason$ = reason_code$
            if f1%(2) = 0% then compute_net /* Reason not in GENCODES */
            get #2 using L14250, reason$
L14250:         FMT XX(24), CH(30)

        compute_net
            bck_net = bck_gross - bck_disc /*Net of transaction record */
            if line_nbr% > max_lines% then gosub page_heading
            print using L60160, customer_code$, customer_name$, /*Detail*/~
                so_nbr$, reason$, bck_gross, bck_disc, bck_net  /*line*/
            line_nbr% = line_nbr% + 1%
            total_gross = total_gross + bck_gross /*Add category totals*/
            total_disc  = total_disc  + bck_disc
            total_net   = total_net   + bck_net
            goto data_print_read

        data_print_sub_totals
            print using L60190  /* Underscores */
            line_nbr% = line_nbr% + 1%
            if line_nbr% > max_lines% then gosub page_heading
            catdesc$ = "TOTAL " & category$
            call "FMTTITLE" (catdesc$, " ", 1%)
            print using L60220, catdesc$, total_gross, total_disc,        ~
                total_net
            rtotal_gross = rtotal_gross + total_gross /* Run totals */
            rtotal_disc  = rtotal_disc  + total_disc
            rtotal_net   = rtotal_net   + total_net
            return

        bckbkgrf_reader
            call "PLOWNXT1" (#1, plowkey$, brk%, f1%(1))
            if f1%(1) = 0% then return       /*End of category or file*/
                get #1 using L15060, record_type$, customer_code$,        ~
                     so_nbr$, rec_date_time$, customer_name$,            ~
                     reason_code$, bck_gross, bck_disc
L15060:     FMT CH(1), CH(9), CH(16), CH(7), CH(30), CH(9), 2*PD(14,4)

        REM *************************************************************~
            * IF RECORD DOESN'T QUALIFY (I.E., IS LATER THAN THE OPERA- *~
            * TOR-ENTERED KEY), RE-READ THE FILE FOR ANOTHER RECORD.    *~
            *************************************************************
            if rec_date_time$ > cutoff_key$ then bckbkgrf_reader
            return

        page_heading
            page_nbr% = page_nbr% + 1%  :  line_nbr% = 6%
            print page
            print using L60040, date$, time$, company_name$, "-" & rptid$
            print using L60070, sub_hdr$, page_nbr%
            print
            print using L60100
            print using L60130
            print
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100, /* Cutoff Date & Time       */~
                                    L20300  /* To delete or not to delet*/
                     return
L20100:     REM Order Cutoff Date & Time (CUTOFF_DATE$, CUTOFF_TIME$)
                inpmessage$ = "Enter the LATEST date and time you want "&~
                     "to print"
                if cutoff_date$ <> " " then L20230
                cutoff_date$ = date
                call "DATEFMT" (cutoff_date$)
L20230:         if cutoff_time$ <> " " then return
                cutoff_time$ = str(time,1,2) & ":" & str(time,3,2)
                return

L20300: REM DELETE OR NOT CODE                     DELETE_YORN$
            inpmessage$ = "Enter 'Y' to DELETE Printed Orders From the"  ~
                             &  " Register File After Printing"
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
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
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  str(line2$,62%) = "BCKBKNGS: " & str(cms2v$,,8%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40200, /* Cutoff Date & Time       */~
                                    L40170  /* To delete or not to delet*/
                  goto L40240

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40170:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L40200:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40240:     accept                                                       ~
               at (01,02),                                               ~
                  "Sales Order Bookings Print and Delete",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Sales Order Cutoff Date and Time",           ~
               at (06,39), fac(lfac$( 1)), cutoff_date$         , ch(08),~
               at (06,50), fac(lfac$( 1)), cutoff_time$         , ch(05),~
                                                                         ~
               at (07,02), "'Y' to DELETE; 'N' to retain",               ~
               at (07,39), fac(lfac$( 2)), delete_yorn$         , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(000104050d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L40560
                  call "MANUAL" ("BCKBKNGS")
                  goto L40240

L40560:        if keyhit% <> 15 then close_workstation
                  call "PRNTSCRN"
                  goto L40240

        close_workstation
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50110, /* Cutoff Date & Time       */~
                                    L50180  /* To delete or not to delet*/
                  return
L50110:     REM Order Cutoff Date & Time  (CUTOFF_DATE$ & CUTOFF_TIME$)
                call "DATEOK" (cutoff_date$, cutoff_date%, errormsg$)
                if errormsg$ <> " " then return
                call "TIMEOK" (cutoff_time$, u3, errormsg$)
                u3 = u3
                return

L50180:     REM To delete or not to delete         DELETE_YORN$
                if delete_yorn$ = "Y" then return
                if delete_yorn$ = "N" then return
                errormsg$ = "You must enter either a 'Y' or an 'N'"
                return

        REM *************************************************************~
            *             I M A G E   S T A T E M E N T S               *~
            *************************************************************

L60040: %RUN DATE: ######## @ ########      #############################~
        ~###############################                    BCKBKNGS######~
        ~#
L60070: %                         #######################################~
        ~#########################################               PAGE: ###~
        ~#
L60100: %CUST NO.  CUSTOMER NAME                  SALES ORDER NO.  REASON~
        ~ CODE DESCRIPTION          GROSS ORDER    ORDER DISCS      NET OR~
        ~DER
L60130: %--------- ------------------------------ ---------------- ------~
        ~------------------------ ------------- -------------- -----------~
        ~---
L60160: %######### ############################## ################ ######~
        ~####################### -##,###,###.## -##,###,###.## -##,###,###~
        ~.##
L60190: %                                                                ~
        ~                         ------------- -------------- -----------~
        ~---
L60220: %                                                                ~
        ~####################### -##,###,###.## -##,###,###.## -##,###,###~
        ~.##
L60250: %#################
L60260: %** END OF REPORT **

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
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
