        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  BBBB    CCC   K   K   CCC    AAA   N   N  RRRR   PPPP    *~
            *  B   B  C   C  K  K   C   C  A   A  NN  N  R   R  P   P   *~
            *  BBBB   C      KKK    C      AAAAA  N N N  RRRR   PPPP    *~
            *  B   B  C   C  K  K   C   C  A   A  N  NN  R   R  P       *~
            *  BBBB    CCC   K   K   CCC   A   A  N   N  R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKCANRP - Generates a report of cancelled sales orders   *~
            *            with selections by customer, salesperson, or   *~
            *            cancellation reason. Output is arranged by     *~
            *            date.                                          *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/21/88 ! Original                                 ! JDH *~
            * 04/26/88 ! Corrected Spelling Errors                ! JDH *~
            * 03/04/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 12/23/92 ! Page 0 Facs, Header, & End Report Time.  ! RJH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            cancelcode$9,                /* Cancellation code          */~
            canceldate$9,                /* Cancellation date          */~
            canceldesc$30,               /* Cancellation description   */~
            columnttl$51,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$9,                   /* Customer code              */~
            cusdesc$30,                  /* Customer description       */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fmcancelcode$9,              /* Cancellation Code          */~
            fmcuscode$9,                 /* Customer Code              */~
            fmsalesperson$4,             /* Salesperson                */~
            hicancelcode$9,              /* Cancellation Code          */~
            hicuscode$9,                 /* Customer Code              */~
            hisalesperson$4,             /* Salesperson                */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lastcus$9,                   /* Last customer              */~
            lastreason$9,                /* Last cancellation reason   */~
            lastperson$4,                /* Last salesperson           */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            locancelcode$9,              /* Cancellation Code          */~
            locuscode$9,                 /* Customer Code              */~
            losalesperson$4,             /* Salesperson                */~
            orderdate$8,                 /* Date of order              */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rpttype$1,                   /* Report Type                */~
            rptdesc$20,                  /* Report Type Description    */~
            rpttitle$60,                 /* Report Title               */~
            salespersoncode$4,           /* Salesperson code           */~
            salespersondesc$30,          /* Salesperson description    */~
            so$16,                       /* Sales order number         */~
            time$8,                      /* System Time                */~
            timeend$8,                   /* System Time @ Report End   */~
            tocancelcode$9,              /* Cancellation Code          */~
            tocuscode$9,                 /* Customer Code              */~
            tosalesperson$4,             /* Salesperson                */~
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
            cms2v$ = "R6.02.03 02/16/93 Customer Credit & Core Trackng  "
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
            * # 1 ! BCKMASTR ! BACKLOG MASTER FILE (GET STORE NUMBER)   *~
            * # 2 ! BCKLINES ! BACK LOG LINE ITEM FILE                  *~
            * # 3 ! BCKHMSTR ! History file for BCKMASTR                *~
            * # 4 ! BCKHLNES ! History file for BCKLINES                *~
            * # 5 ! SLMMASTR ! Salesman master file                     *~
            * # 6 ! GENCODES ! System General Codes file.               *~
            * # 7 ! CUSTOMER ! Customer Master File                     *~
            * # 9 ! WORKFILE1! Workfile for report                      *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select # 2, "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select # 3, "BCKHMSTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select # 4, "BCKHLNES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select # 5, "SLMMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   4

            select # 6, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select # 7, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select # 9, "WORK1",                                         ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  33,                     ~
                        alt key  1, keypos =   34, keylen =  28,         ~
                            key  2, keypos =   62, keylen =  33

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))
            call "OPENCHCK" (# 5, fs%( 5), f2%( 5), 0%, rslt$( 5))
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6), 0%, rslt$( 6))
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7), 0%, rslt$( 7))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)  :  call "TIME" (time$)
            call "COMPNAME" (12%, company$, u3%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "Cancelled Sales Order Report  " &               ~
                        "                              "

            str(columnttl$, 1) = "Beginning Number"
            str(columnttl$,27) = "Ending Number"

            str(line2$,62) = "BCKCANRP: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  4%
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
            if fieldnr% < 1% or fieldnr% >  4% then editpg1
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

        REM March once through the BCKMASTR file and then once through   ~
           BCKHMSTR file and build the workfiles with records that       ~
           satisfy the range parameters. The workfile contains all of the~
           data needed for the report. Alt1 key for Salesperson sort,    ~
           Alt2 for Cancellation reason sort, and primary key for sort by~
           Customer.

            call "SHOSTAT" ("Building the Workfiles")

*         Initialise
            init (" ") cancelcode$, canceldate$, canceldesc$, cuscode$,  ~
                       cusdesc$, orderdate$, salespersoncode$,           ~
                       salespersondesc$, so$
            qty, price, total, gtotal = 0

*         Set channel numbers and start search through BCKMASTR
            mc% = 1% : lc% = 2%
            init (hex(00)) readkey$

L13205:    call "READ102" (#mc%, readkey$, f1%(mc%))
                     if f1%(mc%) = 0% then check_if_done
           goto L13250

        next_rec
            call "READNEXT" (#mc%, f1%(mc%))
                if f1%(mc%) = 0% then check_if_done

L13250:     get #mc% using L13270, cuscode$, so$, salespersoncode$,       ~
                          orderdate$, canceldate$, cancelcode$, crhold$
L13270:     FMT CH(9), CH(16), POS(580), CH(4), POS(806), CH(6),         ~
                POS(839), CH(6), POS(848), CH(9), POS(875), CH(1)

           if crhold$ <> "C" then next_rec
           if cuscode$ < locuscode$ or cuscode$ > hicuscode$ then next_rec
           if salespersoncode$ < losalesperson$ or salespersoncode$ >    ~
                                 hisalesperson$ then next_rec
           if cancelcode$ < locancelcode$ or cancelcode$ > hicancelcode$ ~
              then next_rec

            gosub get_value
            gosub write_workfile
            goto next_rec

        write_workfile

            if f2%(9) = 0% then L13430

            call "WORKOPEN" (#9, "IO", 1000%, f2%(9))

L13430:     write #9, using L13450, cuscode$, orderdate$, so$,            ~
                      salespersoncode$, orderdate$, so$, cancelcode$,    ~
                      orderdate$, so$, canceldate$, gtotal
L13450:     FMT CH(9), CH(8), CH(16), CH(4), CH(8), CH(16), CH(9), CH(8),~
                CH(16), CH(8), PD(14,4)
            return

        get_value
            gtotal = 0
            str(plowkey$, 1, 16) = str(so$, 1, 16)
            str(plowkey$, 17, 3) = hex(00)

L13530:     call "PLOWNEXT" (#lc%, plowkey$, 16%, f1%(lc%))
                if f1%(lc%) = 0% then return

            get #lc% using L13570, qty, price
L13570:         FMT POS(93), PD(14,4), POS(141), PD(14,4)

            gtotal = gtotal + (qty * price)

            goto L13530

        check_if_done
            if mc% = 3% then generate_report
            mc% = 3% : lc% = 4%
            init (hex(00)) readkey$
            goto L13205

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Customer Code          */~
                              L20200,         /* Cancellation Code      */~
                              L20300,         /* Salesperson            */~
                              L20400          /* Report type            */
            return
L20100: REM Def/Enable Customer Code               FMCUSCODE$
            if fmcuscode$          = " " then                            ~
               fmcuscode$          = "ALL"
            return

L20200: REM Def/Enable Cancellation Code           FMCANCELCODE$
            if fmcancelcode$       = " " then                            ~
               fmcancelcode$       = "ALL"
            return

L20300: REM Def/Enable Salesperson                 FMSALESPERSON$
            if fmsalesperson$      = " " then                            ~
               fmsalesperson$      = "ALL"
            return

L20400: REM Def/Enable Report Type                 RPTTYPE$
            rpttype$ = "C"
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
         "Enter Customer Code                                          ",~
         "Enter Cancellation Code                                      ",~
         "Enter Salesperson                                            ",~
         "Sort by (C)ustomer, (S)alesperson, or Cancellation (R)eason  "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      fmcancelcode$, fmcuscode$, fmsalesperson$,         ~
                      hicancelcode$, hicuscode$, hisalesperson$,         ~
                      locancelcode$, locuscode$, losalesperson$,         ~
                      tocancelcode$, tocuscode$, tosalesperson$,         ~
                      rpttype$, rptdesc$
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
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
        generate_report
            call "SHOSTAT" ("Printing Report")
            select printer(134)
            call "SETPRNT" ("BCK011", " ", 0%, 0%)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
            if lcntr% > 56% then gosub page_head

*       * Report Generation Logic
            init (" ") cancelcode$, canceldate$, canceldesc$, cuscode$,  ~
                       cusdesc$, orderdate$, salespersoncode$,           ~
                       salespersondesc$, so$, lastcus$, lastperson$,     ~
                       lastreason$
            orderamt, total, gtotal = 0 : flag% = 0%


*         What key to plow on, get 1st record & into the loop
            init(hex(00)) plowkey$
            if rpttype$ = "C" then key% = 0%
            if rpttype$ = "S" then key% = 1%
            if rpttype$ = "R" then key% = 2%
            call "PLOWALTS" (#9, plowkey$, key%, 0%, f1%(9))
                goto L30270

        read_record
            call "READNEXT" (#9, f1%(9))
L30270:         if f1%(9) = 0% then end_report

            if lcntr% > 56% then gosub page_head

            get #9 using L30330, cuscode$, orderdate$, so$,               ~
                salespersoncode$, cancelcode$, canceldate$, orderamt
L30330:     FMT CH(9), CH(8), CH(16), CH(4), XX(24), CH(9), XX(24),      ~
                CH(8), PD(14,4)

            call "DATEFMT" (orderdate$)
            call "DATEFMT" (canceldate$)

            call "DESCRIBE" (#7, cuscode$, cusdesc$, 0%, f1%(7))
            call "DESCRIBE" (#5, salespersoncode$, salespersondesc$,     ~
                             0%, f1%(5))
            readkey$ = "CANREASON" & str(cancelcode$)
            call "DESCRIBE" (#6, readkey$, canceldesc$, 0%, f1%(6))

            if rpttype$ = "S" then person_print
            if rpttype$ = "R" then reason_print

        REM CUSTOMER_PRINT

            if cuscode$ <> lastcus$ then gosub print_totals
            if cuscode$ <> lastcus$ then total = 0
                gtotal = gtotal + orderamt : total = total + orderamt
            if cuscode$ <> lastcus$ then print_new_cus
            print using L60140, orderdate$, canceldate$, so$, cancelcode$,~
                  salespersoncode$, orderamt
            lcntr% = lnctr% + 1%
            goto read_record

        print_new_cus
            lastcus$ = cuscode$
            print
            print using L60170, cuscode$, cusdesc$, orderdate$,           ~
                canceldate$, so$, cancelcode$, salespersoncode$, orderamt
            lcntr% = lnctr% + 2%
            goto read_record

        print_totals
            if flag% <> 0% then L30666
            flag% = 1%
            return
L30666:     if lnctr% > 56% then page_head
            print using L60640
            print using L60200, total
            lcntr% = lnctr% + 3%
            return

        person_print

            if salespersoncode$ <> lastperson$ then gosub print_totals
            if salespersoncode$ <> lastperson$ then total = 0
                gtotal = gtotal + orderamt : total = total + orderamt
            if salespersoncode$ <> lastperson$ then print_new_person
            print using L60500, orderdate$, canceldate$, so$, cancelcode$,~
                  cuscode$, orderamt
            lcntr% = lnctr% + 1%
            goto read_record

        print_new_person
            lastperson$ = salespersoncode$
            print
            print using L60530, salespersoncode$, salespersondesc$,       ~
             orderdate$, canceldate$, so$, cancelcode$, cuscode$, orderamt
            lcntr% = lnctr% + 2%
            goto read_record

        reason_print

            if cancelcode$ <> lastreason$ then gosub print_totals
            if cancelcode$ <> lastreason$ then total = 0
                gtotal = gtotal + orderamt : total = total + orderamt
            if cancelcode$ <> lastreason$ then print_new_reason
            print using L60560, orderdate$, canceldate$, so$, cuscode$,   ~
                  salespersoncode$, orderamt
            lcntr% = lnctr% + 1%
            goto read_record

        print_new_reason
            lastreason$ = cancelcode$
            print
            print using L60590, cancelcode$, canceldesc$, orderdate$,     ~
                  canceldate$, so$, cuscode$, salespersoncode$, orderamt
            lcntr% = lnctr% + 2%
            goto read_record

        end_report                /* Report Ending Routine */
            gosub print_totals
            print skip(2)
            if lcntr% > 56% then page_head
            print using L60670
            print using L60620, gtotal
            print skip(2)
            if lcntr% > 56% then page_head
            timeend$ = " "   :   call "TIME" (timeend$)
            print using L64990, timeend$       /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            if f2%(9) <> 0% then L33970
            call "FILEBGON" (#9)
            f2%(9) = 1%
L33970:     goto inputmode

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            if pcntr% = 0% then gosub print_params
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "BCKCANRP"
            print using L60110, rpttitle$, pcntr%
            print
            lcntr% = 6%
            if rpttype$ = "C" then gosub print_cus_heading
            if rpttype$ = "S" then gosub print_person_heading
            if rpttype$ = "R" then gosub print_reason_heading
            return

        print_cus_heading
            print using L60230
            print using L60260
            print using L60290
            return

        print_person_heading
            print using L60320
            print using L60350
            print using L60380
            return

        print_reason_heading
            print using L60410
            print using L60440
            print using L60470
            return

        print_params           /* Print Page Zero */
            print page
L34515:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L34535
                str(i$(), i%, 1%) = hex(20)
                goto L34515
L34535:     print using L60070, date$, time$, company$, "BCKCANRP"
            print using L60110, rpttitle$, pcntr%
            print skip(3)
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
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

        FMT                 /* FILE: BCKMASTR                          */~
            CH(9),          /* Customer Code                           */~
            CH(16),         /* sales order number                      */~
            CH(16),         /* Purchase Order Number                   */~
            6*CH(30),       /* Ship To Name and Address                */~
            6*CH(30),       /* Sold-To Name and Address                */~
            CH(20),         /* terms of sale                           */~
            CH(20),         /* how ship information                    */~
            CH(20),         /* f.o.b. information                      */~
            2*CH(50),       /* Shipping Instructions                   */~
            CH(9),          /* sales account number                    */~
            CH(9),          /* Discounts Account                       */~
            3*CH(4),        /* salesman code                           */~
            3*BI(1),        /* Percentage of Sale credited to salesman.*/~
            CH(4),          /* region code                             */~
            CH(200),        /* Variable Fields Data Area               */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(3),          /* Store or Warehouse Code                 */~
            CH(6),          /* Order Date                              */~
            CH(6),          /* Date Document is to be cancelled.       */~
            CH(6),          /* Due Date                                */~
            CH(6),          /* Ship Date                               */~
            CH(6),          /* Date a transaction was entered          */~
            CH(3),          /* User ID of specific user                */~
            CH(6),          /* Date something changed                  */~
            CH(3),          /* User ID of Last Modification            */~
            CH(9),          /* Adjustment Reason Code                  */~
            CH(1),          /* Export Invoice Flag                     */~
            CH(1),          /* Price Code                              */~
            PD(14,4),       /* Discount Percentage                     */~
            PD(14,4),       /* Gross Open Amount                       */~
            CH(1),          /* Credit Hold Flag                        */~
            BI(2),          /* Binary Test                             */~
            BI(4),          /* Next Bill of Lading Number              */~
            CH(2),          /* customer type                           */~
            CH(9),          /* Acccount Cross Reference.               */~
            CH(108)         /* Unused Space                            */~

        FMT                 /* FILE: BCKLINES                          */~
            CH(9),          /* Customer Code                           */~
            CH(16),         /* sales order number                      */~
            CH(3),          /* Binary Test                             */~
            CH(3),          /* Purchase Order Line Number              */~
            CH(25),         /* Part Number                             */~
            CH(32),         /* Part description                        */~
            CH(4),          /* category code                           */~
            PD(14,4),       /* Order Quantity.                         */~
            PD(14,4),       /* Quantity Shipped                        */~
            PD(14,4),       /* Open Quantity                           */~
            PD(14,4),       /* Quantity scheduled for Shipment         */~
            PD(14,4),       /* Quantity Allocated                      */~
            PD(14,4),       /* Quantity Pre-Invoiced                   */~
            PD(14,4),       /* Unit Price                              */~
            CH(4),          /* Stocking UOM                            */~
            CH(4),          /* Pricing Unit of Measure                 */~
            PD(14,7),       /* Conversion Factor                       */~
            PD(14,4),       /* Unit Price                              */~
            PD(14,4),       /* Line Item Discount                      */~
            CH(1),          /* Taxable Purchase (Y/N)                  */~
            CH(9),          /* sales account number                    */~
            CH(9),          /* Discounts Account                       */~
            CH(6),          /* Due Date                                */~
            CH(6),          /* Due Date                                */~
            CH(6),          /* Ship Date                               */~
            CH(6),          /* Lot Number                              */~
            CH(8),          /* Code/# of a Project (i.e. Contract, Job,*/~
            CH(8),          /* Unused Space                            */~
            CH(1),          /* Demand Type - Must be 1,2,3,4,5,8, or 9 */~
            CH(1),          /* priority code                           */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(1),          /* Allocation Flag                         */~
            CH(8),          /* Invoice number                          */~
            CH(8),          /* Estimate Id. Number                     */~
            CH(3),          /* The specific BOM identifier for a Bill o*/~
            CH(35)          /* Unused Space                            */~

        FMT                 /* FILE: BCKHMSTR                          */~
            CH(9),          /* Customer Code                           */~
            CH(16),         /* sales order number                      */~
            CH(16),         /* Purchase Order Number                   */~
            6*CH(30),       /* Ship To Name and Address                */~
            6*CH(30),       /* Sold-To Name and Address                */~
            CH(20),         /* terms of sale                           */~
            CH(20),         /* how ship information                    */~
            CH(20),         /* f.o.b. information                      */~
            2*CH(50),       /* Shipping Instructions                   */~
            CH(9),          /* sales account number                    */~
            CH(9),          /* Discounts Account                       */~
            3*CH(4),        /* salesman code                           */~
            3*BI(1),        /* Percentage of Sale credited to salesman.*/~
            CH(4),          /* region code                             */~
            CH(200),        /* Variable Fields Data Area               */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(3),          /* Store or Warehouse Code                 */~
            CH(6),          /* Order Date                              */~
            CH(6),          /* Date Document is to be cancelled.       */~
            CH(6),          /* Due Date                                */~
            CH(6),          /* Ship Date                               */~
            CH(6),          /* Date a transaction was entered          */~
            CH(3),          /* User ID of specific user                */~
            CH(6),          /* Date something changed                  */~
            CH(3),          /* User ID of Last Modification            */~
            CH(9),          /* Adjustment Reason Code                  */~
            CH(1),          /* Export Invoice Flag                     */~
            CH(1),          /* Price Code                              */~
            PD(14,4),       /* Discount Percentage                     */~
            PD(14,4),       /* Gross Open Amount                       */~
            CH(1),          /* Credit Hold Flag                        */~
            BI(2),          /* Binary Test                             */~
            BI(4),          /* Next Bill of Lading Number              */~
            CH(2),          /* customer type                           */~
            CH(9),          /* Acccount Cross Reference.               */~
            CH(108)         /* Unused Space                            */~

        FMT                 /* FILE: BCKHLNES                          */~
            CH(9),          /* Customer Code                           */~
            CH(16),         /* sales order number                      */~
            CH(3),          /* Binary Test                             */~
            CH(3),          /* Purchase Order Line Number              */~
            CH(25),         /* Part Number                             */~
            CH(32),         /* Part description                        */~
            CH(4),          /* category code                           */~
            PD(14,4),       /* Order Quantity.                         */~
            PD(14,4),       /* Quantity Shipped                        */~
            PD(14,4),       /* Open Quantity                           */~
            PD(14,4),       /* Quantity scheduled for Shipment         */~
            PD(14,4),       /* Quantity Allocated                      */~
            PD(14,4),       /* Quantity Pre-Invoiced                   */~
            PD(14,4),       /* Unit Price                              */~
            CH(4),          /* Stocking UOM                            */~
            CH(4),          /* Pricing Unit of Measure                 */~
            PD(14,7),       /* Conversion Factor                       */~
            PD(14,4),       /* Unit Price                              */~
            PD(14,4),       /* Line Item Discount                      */~
            CH(1),          /* Taxable Purchase (Y/N)                  */~
            CH(9),          /* sales account number                    */~
            CH(9),          /* Discounts Account                       */~
            CH(6),          /* Due Date                                */~
            CH(6),          /* Due Date                                */~
            CH(6),          /* Ship Date                               */~
            CH(6),          /* Lot Number                              */~
            CH(8),          /* Code/# of a Project (i.e. Contract, Job,*/~
            CH(8),          /* Unused Space                            */~
            CH(1),          /* Demand Type - Must be 1,2,3,4,5,8, or 9 */~
            CH(1),          /* priority code                           */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(1),          /* Allocation Flag                         */~
            CH(8),          /* Invoice number                          */~
            CH(8),          /* Estimate Id. Number                     */~
            CH(3),          /* The specific BOM identifier for a Bill o*/~
            CH(35)          /* Unused Space                            */~

        FMT                 /* FILE: SLMMASTR                          */~
            CH(4),          /* salesman code                           */~
            CH(30),         /* salesman name                           */~
            3*CH(30),       /* address (3 lines @ 30 char. each)       */~
            CH(10),         /* phone number                            */~
            10*CH(20),      /* Variable Fields Data Area               */~
            CH(266)         /* Unused Space                            */~

        FMT                 /* FILE: GENCODES                          */~
            CH(9),          /* Logical File ID                         */~
            CH(15),         /* Generic for any code in the system      */~
            CH(30),         /* Generic for general code descriptions   */~
            CH(2),          /* Maximum allowable length of a Code      */~
            CH(0072)        /* Unused Space                            */~

        FMT                 /* FILE: CUSTOMER                          */~
            CH(9),          /* Customer Code                           */~
            CH(30),         /* Sort Name                               */~
            5*CH(30),       /* Sold-To Name and Address                */~
            CH(18),         /* Customer Billing Address-City           */~
            CH(2),          /* Customer Billing Address - State        */~
            CH(01),         /* Unused Space                            */~
            CH(09),         /* Customer Billing Address - Zip Code     */~
            CH(6),          /* date account opened (yymmdd)            */~
            4*CH(6),        /* date customer account last used         */~
            CH(3),          /* User ID of Last Modification            */~
            5*CH(30),       /* Ship To Name and Address                */~
            CH(18),         /* Customer's Ship to Address - City       */~
            CH(2),          /* Customer's Ship to Address - State      */~
            CH(01),         /* Unused Space                            */~
            CH(9),          /* Customer's Ship to Address - Zip Code   */~
            CH(20),         /* buyer's name                            */~
            CH(10),         /* Phone Number                            */~
            CH(9),          /* sales account number                    */~
            CH(9),          /* receivables account number              */~
            CH(9),          /* cash in bank account                    */~
            CH(9),          /* freight account                         */~
            CH(9),          /* Discounts Account                       */~
            CH(9),          /* sales tax account                       */~
            PD(14,4),       /* Discount Percentage                     */~
            CH(1),          /* Price Code                              */~
            PD(14,4),       /* credit limit                            */~
            CH(6),          /* The system date a file or record was las*/~
            CH(3),          /* User ID of Last Modification            */~
            CH(20),         /* terms of sale                           */~
            CH(20),         /* how ship information                    */~
            CH(20),         /* f.o.b. information                      */~
            CH(9),          /* Shipping Region Code                    */~
            CH(1),          /* Allow Backorders Flag                   */~
            CH(1),          /* Allow Late Shipments flag               */~
            2*CH(50),       /* Shipping Instructions                   */~
            3*CH(4),        /* salesman code                           */~
            3*BI(1),        /* Percentage of Sale credited to salesman.*/~
            CH(4),          /* region code                             */~
            4*PD(14,4),     /* customer current balance                */~
            CH(6),          /* Transaction date                        */~
            CH(9),          /* Acccount Cross Reference.               */~
            CH(9),          /* Bill-to Cross Reference                 */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(1),          /* General purpose status indicator        */~
            CH(1),          /* Taxable Purchase (Y/N)                  */~
            CH(25),         /* tax exemption number                    */~
            10*CH(20),      /* Variable Fields Data Area               */~
            CH(1),          /* purchase order requirement              */~
            CH(1),          /* Balance Forward or Open Item Flag       */~
            CH(1),          /* Print line(s)?                          */~
            CH(2),          /* customer type                           */~
            CH(10),         /* Sales Tax Code                          */~
            CH(9),          /* Finance Charge G/L Account Number       */~
            CH(1),          /* Finance Charge Code                     */~
            CH(156)         /* Unused Space                            */~

        FMT                 /* FILE: SYSFILE2                          */~
            CH(20),         /* Acts as variable text key to SYSFILE2 sy*/~
            CH(480)         /* Unused Space                            */~

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
              on fieldnr% gosub L40085,         /* Customer Code     */   ~
                                L40085,         /* Cancellation Code */   ~
                                L40085,         /* Salesperson       */   ~
                                L40085          /* Report Type       */
              goto L40100

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40085:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40100:     accept                                                       ~
               at (01,02),                                               ~
                  "Input Report Selection Criteria",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),   columnttl$           , ch(51),~
                                                                         ~
               at (07,02), "Customer Code",                              ~
               at (07,30), fac(lfac$( 1)), fmcuscode$           , ch(09),~
               at (07,56), fac(lfac$( 1)), tocuscode$           , ch(09),~
                                                                         ~
               at (08,02), "Cancellation Code",                          ~
               at (08,30), fac(lfac$( 2)), fmcancelcode$        , ch(09),~
               at (08,56), fac(lfac$( 2)), tocancelcode$        , ch(09),~
                                                                         ~
               at (09,02), "Salesperson",                                ~
               at (09,30), fac(lfac$( 3)), fmsalesperson$       , ch(04),~
               at (09,56), fac(lfac$( 3)), tosalesperson$       , ch(04),~
                                                                         ~
               at (10,02), "Report Type",                                ~
               at (10,30), fac(lfac$( 4)), rpttype$             , ch(01),~
               at (10,40), fac(hex(8c)),   rptdesc$             , ch(20),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40265
                  call "MANUAL" ("BCKCANRP") : goto L40100

L40265:        if keyhit% <> 15 then L40280
                  call "PRNTSCRN" : goto L40100

L40280:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40375     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40360
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% > 1% then L40365
L40360:         str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40365:     return

L40375: if fieldnr% > 0% then L40420  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40420:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
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
            on fieldnr% gosub L50100,         /* Customer Code          */~
                              L50200,         /* Cancellation Code      */~
                              L50300,         /* Salesperson            */~
                              L50400          /* Report Type            */
            return
L50100: REM Test for Customer Code                FMCUSCODE$
            call "TESTRNGE"                                              ~
                  (fmcuscode$          , tocuscode$          ,           ~
                   locuscode$          , hicuscode$          ,           ~
                   errormsg$)
            return

L50200: REM Test for Cancellation Code            FMCANCELCODE$
            call "TESTRNGE"                                              ~
                  (fmcancelcode$       , tocancelcode$       ,           ~
                   locancelcode$       , hicancelcode$       ,           ~
                   errormsg$)
            return

L50300: REM Test for Salesperson                  FMSALESPERSON$
            call "TESTRNGE"                                              ~
                  (fmsalesperson$      , tosalesperson$      ,           ~
                   losalesperson$      , hisalesperson$      ,           ~
                   errormsg$)
            return

L50400: REM Test for Report Type                  RPTTYPE$
            rptdesc$ = " "
            if rpttype$ = "C" then rptdesc$ = "CUSTOMER"
            if rpttype$ = "S" then rptdesc$ = "SALESPERSON"
            if rpttype$ = "R" then rptdesc$ = "CANCELLATION REASON"
            if rpttype$ = "C" or rpttype$ = "S" or rpttype$ = "R" then   ~
                                                             return
                errormsg$ = "'C', 'S', or 'R'  Please."
            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:BCK011

*       * Header Line 2
L60110: %                                                     ###########~
        ~#################################                     PAGE: ####

L60140: %                                                  ########    ##~
        ~######    ################    #########    ####    ##########.##

L60170: %   #########    ##############################    ########    ##~
        ~######    ################    #########    ####    ##########.##

L60200: %                                                  * TOTAL       ~
        ~                                                   ##########.##

L60230: %   CUSTOMER     CUSTOMER                          ORDER       CA~
        ~NCELLED                       CANCEL       SLSP

L60260: %   CODE         DESCRIPTION                       DATE        DA~
        ~TE        SALES ORDER         CODE         CODE      GROSS ORDER

L60290: %   ---------    ------------------------------    --------    --~
        ~------    ----------------    ---------    ----    -------------

L60320: %   SLSP    SALESPERSON                       ORDER       CANCELL~
        ~ED                       CANCEL       CUSTOMER

L60350: %   CODE    DESCRIPTION                       DATE        DATE   ~
        ~     SALES ORDER         CODE         CODE           GROSS ORDER

L60380: %   ----    ------------------------------    --------    -------~
        ~-    ----------------    ---------    ---------    -------------

L60410: %   CANCEL       CANCELLATION                      ORDER       CA~
        ~NCELLED                       CUSTOMER     SLSP

L60440: %   CODE         REASON                            DATE        DA~
        ~TE        SALES ORDER         CODE         CODE      GROSS ORDER

L60470: %   ---------    ------------------------------    --------    --~
        ~------    ----------------    ---------    ----    -------------

L60500: %                                             ########    #######~
        ~#    ################    #########    #########    ##########.##

L60530: %   ####    ##############################    ########    #######~
        ~#    ################    #########    #########    ##########.##

L60560: %                                                  ########    ##~
        ~######    ################    #########    ####    ##########.##

L60590: %   #########    ##############################    ########    ##~
        ~######    ################    #########    ####    ##########.##

L60620: %                                              * * * GRAND TOTAL ~
        ~                                                  ###########.##

L60640: %                                                                ~
        ~                                                   -------------

L60670: %                                                                ~
        ~                                                  ==============

        %** Report Title for page 0
        %############################################################

L64990:       %                            * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T  @  ########  * * * * * * * * * *

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
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
