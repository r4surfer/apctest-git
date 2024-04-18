        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   CCC   DDDD    AAA    QQQ   JJJJJ  TTTTT                 *~
            *  C   C  D   D  A   A  Q   Q    J      T                   *~
            *  C      D   D  AAAAA  Q   Q    J      T                   *~
            *  C   C  D   D  A   A  Q  QQ  J J      T                   *~
            *   CCC   DDDD   A   A   QQQQ   J       T                   *~
            *                            Q                              *~
            *-----------------------------------------------------------*~
            * CDAQJT   - This Program allows the user to review         *~
            *            and report on Time Card Data received via the  *~
            *            CDA to CMS Interface. Transactions are built   *~
            *            from the CDATOCMS update program.              *~
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
            * 08/23/88 ! Original                                 ! BPN *~
            * 06/12/89 ! Modified Reange test on Department Code  ! MJB *~
            *          !  Employee Code and Job Code              !     *~
            * 09/15/94 ! Modify for new CDA Hardware/software with! RJH *~
            *          !   the elimination of 'OP' transactions.  !     *~
            * 07/12/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "CDAQJT" (in_emp$, in_tcdate$)

        dim                                                              ~
            activ$(12)4,                 /* Activity Table             */~
            blankdate$8,                 /* Blank Date for comparison  */~
            cursor%(2),                  /* Cursor location for edit   */~
            columnttl$30,                /* Range Description          */~
            company$60,                  /* Company or Division Name   */~
            date$8,                      /* Date for screen display    */~
            dept$4,                      /* Dept Number                */~
            dfac$(14)1,                  /* Field Attribute Characters */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            f_data$(14)40,               /* Data area for Print        */~
            f_desc$(14)16,               /* Desc area for Print        */~
            fmdate$8,                    /* Date Range                 */~
            fmdept$4,                    /* Department Range           */~
            fmempl$12,                   /* Employee Number            */~
            fmjob$8,                     /* Job Number Range           */~
            fmtime$8,                    /* Time Range                 */~
            hdr1$(3)30,                  /* Summary Display Headings 1 */~
            hdr2$(7)30,                  /* Summary Display Headings 2 */~
            hdr3$40,                     /* Detail Display Headings    */~
            hidate$8,                    /* Date Range                 */~
            hidept$4,                    /* Department Range           */~
            hiempl$12,                   /* Employee Number            */~
            hijob$8,                     /* Job Number Range           */~
            hitime$8,                    /* Time Range                 */~
            hours(12),                   /* Hours for Display          */~
            i$(24)80,                    /* Screen Image               */~
            i1$(24)80,                   /* SCREEN IMAGE               */~
            in_emp$12, in_emp_last$12,   /* Employee Code Argument     */~
            in_tcdate$6,                 /* Time Card Date Arg         */~
            in_tcdate_last$6,            /*                            */~
            inpmessage$79,               /* Informational Message      */~
            job$8,                       /* Job Number                 */~
            jobh$(12)8,                  /* Job Number Hold            */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lodate$8,                    /* Date Range                 */~
            lodept$4,                    /* Department Range           */~
            loempl$12,                   /* Employee Number            */~
            lojob$8,                     /* Job Number Range           */~
            lotime$8,                    /* Time Range                 */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            print$9,                     /* Print Work area for totals */~
            print4$4,                    /* Print Work Area For SHIFT  */~
            raction$2,                   /* Action                     */~
            rbase$3,                     /* Base Prog                  */~
            rchange$3,                   /* Changed By                 */~
            rchng_date$8,                /* Changed On                 */~
            rcurr$3,                     /* Curr Prog                  */~
            rdate$8,                     /* Date                       */~
            rdept$4,                     /* DEPT NUMBER                */~
            rdvcid$3,                    /* Device ID                  */~
            readkey$99,                  /* MISCELLANEOUS READ/PLOW KEY*/~
            rerror$4,                    /* Fields in Error            */~
            rmessage$30,                 /* UPDATE MESSAGE             */~
            rovride$1,                   /* Override                   */~
            rpcid$3,                     /* PC ID                      */~
            rpttitle$60,                 /* Report Title               */~
            rrdate$8, rrtime$8,          /* Raw Received Date and Time */~
            rsource$1,                   /* Source                     */~
            rstatus$1,                   /* Status                     */~
            rtime$6,                     /* Time                       */~
            rtran$2,                     /* Tran Set                   */~
            rtrnr$6,                     /*            Trans Number    */~
            rtype$1,                     /* Type                       */~
            rvalid$4,                    /* Fields Validated           */~
            s$(7)1,                      /* Update Status              */~
            stv$(7)2,                    /* Actual Value of Status     */~
            sel_status$2,                /* Update Status              */~
            sfac$(12)1, summary$(12)79,  /* Summary Display and FACs   */~
            shift$(12)4,                 /* Shift Table                */~
            sumkey$(12)19,               /* Readkey for Each Sumry item*/~
            sumkey2$(12)19,              /* READKEY FOR EACH SUMRY ITEM*/~
            task$(12)6,                  /* Task Table                 */~
            tc_fmdate$8,                 /* Date Range                 */~
            tc_hidate$8,                 /* Date Range                 */~
            tc_lodate$8,                 /* Date Range                 */~
            tc_todate$8,                 /* Date Range                 */~
            tcdate$(12)8,                /* Time Card Date             */~
            todate$8,                    /* Date Range                 */~
            todept$4,                    /* Department Range           */~
            toempl$12,                   /* Employee Number            */~
            tojob$8,                     /* Job Number Range           */~
            totime$8,                    /* Time Range                 */~
            tr_tcdate$8,                 /* Time Card Date             */~
            traction$2,                  /* Action                     */~
            tractiv$4,                   /* Activity                   */~
            trdate$8,                    /* Date/Time                  */~
            trdept$4,                    /* Department                 */~
            trdept2$4,                   /* Department                 */~
            trdvc_id$3,                  /* Device ID                  */~
            trempl$12,                   /* Employee Code              */~
            trjob$8,                     /* Job Number                 */~
            trkey2$19,                   /* Transaction Second half ky */~
            trpc_id$3,                   /* PC ID                      */~
            trsrce$1,                    /* Source                     */~
            trstatus$2,                  /* Status                     */~
            trtask$6,                    /* Time Card Task             */~
            trtime$8,                    /* Time                       */~
            trupdmsg$30,                 /* Update Message             */~
            trwc$4,                      /* Work Center                */~
            update_msg$30,               /* Update Message             */~
            updmsg$(12)30,               /* Update Message             */~
            userid$3,                    /* Current User Id            */~
            wc$(12)4,                    /* WC Table                   */~
            wkdate$8,                    /* Work Date                  */~
            wktime$8                     /* Work Time                  */~

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
            * #01 ! CDARECVE ! Caelus Data Acquisition Network Host Rec *~
            * #02 ! CDAAUDJT ! Audit file for 'JT' Transactions         *~
            * #03 ! CDAHOLD  ! Holding file for first half of 2 part CD *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "CDARECVE",                                      ~
                        varc,     indexed,  recsize =  768,              ~
                        keypos =    2, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  20          ~

            select #02, "CDAAUDJT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   17, keylen =  19,                     ~
                        alt key  1, keypos =    5, keylen =  31,         ~
                            key  2, keypos =    1, keylen =  35

            select #03, "CDAHOLD",                                       ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =    1, keylen =  19,                     ~
                        alt key  1, keypos =   13, keylen =  11

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))

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
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press RETURN."

            columnttl$ = "From           To          "

            str(line2$,62) = "  CDAQJT: " & str(cms2v$,,8)

            stv$() = "P H W WRE ERI "

            msg_dsp% = 1%

*        Set up Display Variables and Descriptor Tables
            hdr1$(1) = "O/R                           "
            hdr1$(2) = "                              "
            hdr1$(3) = "O/R                           "

            hdr2$(1) = "  Date   Time  Ac             "
            hdr2$(1) = "  Date  " & hex(ac) & "Time " & hex(ac) & "Ac"
            hdr2$(2) = "St                            "
            hdr2$(3) = "Empl                          "
            hdr2$(4) = "Dept                          "
            hdr2$(5) = " Job #                        "
            hdr2$(6) = "Shift  Task   WC  Act   Hours "
            hdr2$(6) = "Shift" & hex(ac) & " Task " & hex(ac) & " WC "  &~
                       hex(ac) & "Actv" & hex(8cac) & " Hours" & hex(8c)
            hdr3a$   = "Field Desc"
            hdr3$    = "Data Field Contents "

*        Test Incomming Arguments and setup acordingly...
            if in_emp$ = " " and ~
               (in_tcdate$ = " " or in_tcdate$ = blankdate$) then L10000
            if in_emp$ = in_emp_last$ and in_tcdate$ = in_tcdate_last$   ~
                                                               then L12700

            in_emp_last$    = in_emp$
            in_tcdate_last$ = in_tcdate$
            fmdate$, fmtime$, tc_fmdate$, fmdept$, fmempl$, fmjob$ = "ALL"
            init("X") str(s$())
            if in_emp$ <> " " then let fmempl$, toempl$ = in_emp$
            if in_tcdate$ = " " or in_tcdate$ = blankdate$ then L09510
                tc_fmdate$ = in_tcdate$
                tc_todate$ = " "
                call "DATE" addr ("G+", in_tcdate$, -1%, fmdate$, r%)
                call "DATE" addr ("G+", in_tcdate$,  1%, todate$, r%)
L09510:     for fieldnr% = 1% to 7%
                gosub'151(fieldnr%)
            next fieldnr%
            goto start_screen2


L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  7%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10260
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 5% and keyhit% <> 6% then L10250
                        gosub'051(fieldnr%)
                        gosub'151(fieldnr%)
                        goto L10120
L10250:               if keyhit% <> 0% then       L10120
L10260:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%

           goto start_screen2

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       start_screen2
                  if keyhit%  = 12% then gosub purge_routine
                  if keyhit%  = 14% then gosub report_rtn
                  if keyhit%  = 16% then       exit_program
                  if keyhit%  = 32% then       exit_program
                  if keyhit% <>  0% then       editpg1
L11140:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  7% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11190:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit% <> 5% and keyhit% <> 6% then L11220
                     gosub'051(fieldnr%)
L11220:           if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11190
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11190
                  lastfieldnr% = fieldnr%
            goto L11140


        REM *************************************************************~
            *        D I S P L A Y   R O U T I N E   S C R E E N 2      *~
            *-----------------------------------------------------------*~
            * Handles operation of DISPLAY, UPDATE, DELETE for SCREEN2. *~
            *************************************************************

        start_screen2
            gosub init_screen2
*        Setup First Read of Audit File
            plowkey$ = all(hex(00))
            str(plowkey$,1,6) = lodate$
            call "PLOWNEXT" (#2, plowkey$, 0%, f1%(2))
                if f1%(2) = 1 then L12220

            goto L12710

*        Get Next Record for Validation
L12180:     call "PLOWNEXT" (#2, plowkey$, 0%, f1%(2))
                if f1%(2) = 0 then L12710

*        Setup key to select DATE
L12220:     wkdate$ = str(plowkey$,1,6)
            if wkdate$ > hidate$ then L12180
            str(plowkey$,7) = all(hex(00))
            str(plowkey$,7,6) = lotime$
L12270:     call "PLOWNEXT" (#2, plowkey$, 6%, f1%(2))
                if f1%(2) = 1% then L12320
            goto L12180

*        Setup key to select TIME
            wktime$ = str(plowkey$,7,6)
            if wktime$ > hitime$ then L12270

*        Get Values from record for continued validation
L12320:     get #2 using L12340, trdept$, trempl$, trstatus$, traction$,  ~
                                trjob$, tr_tcdate$
L12340:        FMT          /* FILE: CDAAUDJT                          */~
                  CH(4),    /* Department                              */~
                  CH(12),   /* Employee                                */~
                  POS(55),  /*                                         */~
                  CH(2),    /* Status                                  */~
                  CH(2),    /* Action (aka activity)                   */~
                  POS(59),  /*                                         */~
                  CH(8),    /* Job Number                              */~
                  POS(127), /*                                         */~
                  CH(6)     /* Time Card Date                          */~

*        Check if Time Card Date is within Range
            if tr_tcdate$ = blankdate$ then tr_tcdate$ = " "
            if tr_tcdate$ = " " then L12490
                call "DATEOK" (tr_tcdate$, tc_tstdate%, errormsg$)
                if tc_tstdate% < tc_lodate% or                           ~
                   tc_tstdate% > tc_hidate% then L12270

L12490
*        Check if Dept is within Dept range
            if trdept$ < lodept$ or trdept$ > hidept$ then L12270

*        Check if Empl is within Empl range
            if trempl$ < loempl$ or trempl$ > hiempl$ then L12270

*        Check if JOB is within JOB range
            if trjob$ < lojob$ or trjob$ > hijob$ then L12270

*        Check if Status is within Selection
            for s% = 1% to len(s$())
               if s$(s%) <> "X" then L12620
               if str(stv$(s%)) = trstatus$ then L12660
L12620:     next s%
            goto L12270

*        Record is within selection criteria
L12660:     dl% = dl% + 1%
            gosub load_audit
            if dl% < 12% then goto L12270

L12700
*        Check for records at end of file
L12710:     if dl% <> 0% then L12800
                errormsg$ = "No Records found within the Specified " &   ~
                            "Selection Criteria"
                goto editpg1
L12800:     lastfieldnr% = 0%
L12810:     gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                errormsg$ = " "
                if keyhit%  =  1% then gosub startover
                if keyhit%  =  2% then goto start_screen2
                if keyhit% <>  5% then       L12900
                    if dl% <> 12% then goto L12270
                      gosub init_screen2
                      dl% = 0
                      goto L12270
L12900:         if keyhit% <> 11% then L12930
                    msg_dsp% = mod(msg_dsp%, 2) + 1
                    goto L12810
L12930:         if keyhit%  = 14% then gosub report_rtn
                if keyhit%  = 16% then editpg1
                if keyhit%  = 32% then exit_program
              fieldnr% = cursor%(1%) - 6%
                if fieldnr% < 1% or fieldnr% > 12% then L12810
                if fieldnr% = lastfieldnr% then    L12810
                if keyhit%  =  0% then       display_raw_data
                if keyhit%  =  8% then gosub mark_resolved
                if keyhit%  =  9% then gosub unmark_resolved
              goto L12810

            next s%
            goto L12810

        display_raw_data
            wk% = fieldnr%
            if sumkey$(wk%) = " " then L12810
            gosub init_screen3
            readkey$ = sumkey$(wk%)
            f_desc$(2) = " 2) Employee    "
            f_desc$(3) = " 3) Job Code    "
            f_desc$(4) = " 4) Work Center "
            f_desc$(5) = " 5) WC Activity "
            f_desc$(6) = " 6) Non-Job Task"
            f_desc$(7) = " 7) Hours       "
            f_desc$(11%) = "11) O/R Shift"
            f_desc$(12%) = "12) O/R Dept"
            gosub load_screen3
                if loaded% = 0% then L12810
            if sumkey2$(wk%) <> " " then L13250
                inpmessage$ = "Press RETURN to see Summary Screen"
                goto L13260
L13250:     inpmessage$ = "Press RETURN to see 'OP' Transaction Detail"
L13260:     lastfieldnr% = 0%
            gosub'103(0%, 1%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% then       editpg1
                  if keyhit%  = 16% then       L12810
                  if keyhit%  = 32% then       exit_program
                  if keyhit%  =  0% then       L12810
*                IF KEYHIT%  =  0% THEN       DISPLAY_OP_RAW_DATA
            goto L12810

*       DISPLAY_OP_RAW_DATA
*          IF SUMKEY2$(WK%) = " " THEN 12810
*          GOSUB INIT_SCREEN3
*          READKEY$ = SUMKEY2$(WK%)
*          F_DESC$(2) = " 2)             "
*          F_DESC$(3) = " 3)             "
*          F_DESC$(4) = " 4)             "
*          F_DESC$(5) = " 5) Shift       "
*          F_DESC$(6) = " 6) Department  "
*          F_DESC$(7) = " 7)             "
*          GOSUB LOAD_SCREEN3
*          INPMESSAGE$ = "Press RETURN to see Summary Screen   "
*          LASTFIELDNR% = 0%
*          GOSUB'103(0%, 2%)           /* Display Screen - No Entry   */
*                IF KEYHIT%  =  1% THEN GOSUB STARTOVER
*                IF KEYHIT%  =  4% THEN       EDITPG1
*                IF KEYHIT%  = 16% THEN       12810
*                IF KEYHIT%  = 32% THEN       EXIT_PROGRAM
*                IF KEYHIT% <>  0% THEN       13470
            goto L12810

        REM *************************************************************~
            *             U P D A T E   R O U T I N E S                 *~
            *-----------------------------------------------------------*~
            * Update Records Includes Mark as Resolved, Line Update,    *~
            * and Line Delete Routines.                                 *~
            *************************************************************

        mark_resolved
            readkey$ = sumkey$(fieldnr%)
            call "READ101" (#2, readkey$, f1%(2))
            if f1%(2) = 0% then return
            get #2 using L14110, trstatus$
L14110:        FMT POS(55), CH(2)
            if trstatus$ = "E " or trstatus$ = "W " then L14160
              errormsg$ =                                                ~
               "Only Status 'E ' and 'W ' records can be Marked Resolved"
              return
L14160:     str(trstatus$,2,1) = "R"
            str(summary$(fieldnr%),19,2) = trstatus$
            put #2 using L14110, trstatus$
            rewrite #2
            return


        unmark_resolved
            readkey$ = sumkey$(fieldnr%)
            call "READ101" (#2, readkey$, f1%(2))
            if f1%(2) = 0% then return
            get #2 using L14270, trstatus$
L14270:        FMT POS(55), CH(2)
            if trstatus$ = "ER" or trstatus$ = "WR" then L14320
              errormsg$ =                                                ~
               "Only Status 'ER' and 'WR' records can be Unmarked"
              return
L14320:     str(trstatus$,2,1) = " "
            str(summary$(fieldnr%),19,2) = trstatus$
            put #2 using L14270, trstatus$
            rewrite #2
            return


        REM *************************************************************~
            *          P R I N T / P U R G E   R O U T I N E            *~
            *-----------------------------------------------------------*~
            * Print or Purge Records Per Selection Criteria             *~
            *************************************************************

        purge_routine
L16070:     u3% = 2%
            call "ASKUSER" (u3%, "PURGE RECORDS",                        ~
                     "Press RETURN to purge records based on Selection", ~
                     "Criteria shown on the screen.",                    ~
                     "-OR- Press PF-1 to ABORT Purge...")
            if u3%  = 1% then return
            if u3% <> 0% then L16070
            purge%  = 1%
            goto L16210

        report_rtn
            purge% = 0%
            gosub generate_report

L16210:     plowkey$ = all(hex(00))
            str(plowkey$,1,6) = lodate$
            call "PLOWNEXT" (#2, plowkey$, 0%, f1%(2))
                if f1%(2) = 1 then L16340

            errormsg$ = "No CDA Audit Records type (JT) to Process"
            return

*        Get Next Record for Validation
L16300:     call "PLOWNEXT" (#2, plowkey$, 0%, f1%(2))
                if f1%(2) = 0 then L16850

*        Setup key to select DATE
L16340:     wkdate$ = str(plowkey$,1,6)
            if wkdate$ > hidate$ then L16300
            str(plowkey$,7) = all(hex(00))
            str(plowkey$,7,6) = lotime$
L16380:     call "PLOWNEXT" (#2, plowkey$, 6%, f1%(2))
                if f1%(2) = 1% then L16470
            goto L16300

*        Setup key to select TIME
            wktime$ = str(plowkey$,7,6)
            if wktime$ > hitime$ then L16380

*        Get Values from record for continued validation
L16470:     get #2 using L16490, trdept$, trempl$, trkey2$, trstatus$,    ~
                                trjob$,  tr_tcdate$
L16490:        FMT CH(4), CH(12), POS(36), CH(19), CH(2), POS(59), CH(8),~
                   POS(127), CH(6)

*        Check transaction against range selections...
            call "DATEOK" (tr_tcdate$, tc_tstdate%, errormsg$)
            if tc_tstdate% < tc_lodate% or                               ~
                tc_tstdate% > tc_hidate% then L16380
            if trdept$ < lodept$ or trdept$ > hidept$ then L16380
            if trempl$ < loempl$ or trempl$ > hiempl$ then L16380
            if trjob$  < lojob$  or trjob$  > hijob$  then L16380
            for s% = 1% to len(s$())
                if s$(s%) <> "X" then L16770
                if str(stv$(s%)) = trstatus$ then L16810
L16770:     next s%
            goto L16380

*        Record is within criteria...
L16810:     if purge% = 1% then L16813
                gosub print_lines
                goto L16830
L16813:     call "DELETE" (#2, plowkey$, 19%)
            if str(trstatus$,,1) <> "E" then L16830
                call "DELETE" (#1, plowkey$, 19%)
                if trkey2$ <> " " then call "DELETE" (#1, trkey2$, 19%)
L16830:     goto L16380

L16850
*        Print End of Report
            if purge% = 0% then gosub end_report
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20170,         /* Date Range             */~
                              L20210,         /* Time Range             */~
                              L20260,         /* TC Date Range          */~
                              L20310,         /* Dept Range             */~
                              L20360,         /* Empl Range             */~
                              L20410,         /* Job Number Range       */~
                              L20460          /* Update Status          */
            return

L20170: REM Def/Enable Date Range                  FMDATE$
            if fmdate$ = " " or fmdate$ = blankdate$ then                ~
               fmdate$             = "ALL"
            return

L20210: REM Def/Enable Time Range                  FMTIME$
            if fmtime$             = " " then                            ~
               fmtime$             = "ALL"
            return

L20260: REM Def/Enable Time Card Range             TC_FMDATE$
            if tc_fmdate$ = " " or tc_fmdate$ = blankdate$ then         ~
               tc_fmdate$          = "ALL"
            return

L20310: REM Def/Enable Dept Range                  FMDEPT$
            if fmdept$ = " " then fmdept$ = "ALL"
            return

L20360: REM Def/Enable Employee                    FMEMPL$
            if fmempl$           = " " then                              ~
               fmempl$           = "ALL"
            return

L20410: REM Def/Enable Job Number                  FMJOB$
            if fmjob$            = " " then                              ~
               fmjob$            = "ALL"
            return

L20460: REM Def/Enable Update Status               SEL_STATUS$
            if s$() = " " then s$() = all ("X")
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if scrnr% > 1% then L28120
            if fieldnr% <> 0% then L28120
                inpmessage$ = edtmessage$
                return

L28120
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            if scrnr% = 2% then restore line = scrn2_msg, 1%
            if scrnr% = 3% then restore line = scrn3_msg, 1%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Date Range                                             ",~
         "Enter Time Range                                             ",~
         "Enter Time Card Date Range                                   ",~
         "Enter Department Range                                       ",~
         "Enter Employee Range                                         ",~
         "Enter Job Number Range                                       ",~
         "Enter a Non-Blank character for each Audit Status desired    "

        scrn2_msg  :  data                                               ~
         "Position Cursor and Press Return to see Transaction Detail   "

        scrn3_msg  :  data                                               ~
         "Press Return to see Summary Screen                           "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, tr_tcdate$,                ~
                      job$, trdept$, trempl$, trwc$, tractiv$, trtask$,  ~
                      trstatus$, trdate$, trdept2$,                      ~
                      update_msg$, updmsg$(), fmdept$, fmempl$,          ~
                      fmdate$, fmjob$, sel_status$,                      ~
                      hidept$, hidate$, hijob$, hiempl$,                 ~
                      lodept$, lodate$, lojob$, loempl$, todate$,        ~
                      tojob$, todept$, toempl$, s$(),                    ~
                      fmtime$, totime$, lotime$, hitime$,                ~
                      tc_fmdate$, tc_todate$, tc_lodate$, tc_hidate$

            return

        init_screen2
            init (" ") summary$(), sumkey$(), updmsg$(), errormsg$,      ~
                     inpmessage$, sumkey2$(), task$(), wc$(), activ$(),  ~
                     shift$(), jobh$()
            dl%, wk% = 0
            mat hours = zer
            return

        init_screen3
            init (" ") rstatus$, rdate$, rtime$, rpcid$, rdvcid$,        ~
                      rsource$, rtype$, rovride$, rbase$, rcurr$,        ~
                      rtran$, raction$, rchange$, rchng_date$,           ~
                      rmessage$, dept$, f_data$(), errormsg$,            ~
                      inpmessage$, f_desc$()
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
*        Load data from Audit File for Report
            get #2 using L32060, trdept$, trempl$, trdate$, trtime$,      ~
                trpc_id$, trdvc_id$, trsrce$, trkey2$, trstatus$,        ~
                traction$, trjob$, trwc$, tractiv$, trtask$, trhours,    ~
                trshift%, trdept2$, trupdmsg$, tr_tcdate$
            hours(dl%)   = trhours
            updmsg$(dl%) = trupdmsg$
            tcdate$(dl%) = tr_tcdate$
            jobh$(dl%)   = trjob$
            task$(dl%)   = trtask$
            wc$(dl%)     = trwc$
            activ$(dl%)  = tractiv$
            if trshift%  = 0% then L30200
                convert trshift% to shift$(dl%), pic (####)
L30200:     return

        load_audit
*        Load Data from Audit file to Summary area for display
            gosub dataload
            sumkey$(dl%) = plowkey$
            sumkey2$(dl%) = trkey2$
            call "DATEOK" (trdate$, trdate%, errormsg$)
            str(summary$(dl%),,8)  = trdate$
            call "TIMEOK" (trtime$, trtime, errormsg$)
            str(summary$(dl%), 10) = str(trtime$,,5)
            str(summary$(dl%), 16) = traction$
            str(summary$(dl%), 19) = trstatus$
            str(summary$(dl%), 22) = trempl$
            str(summary$(dl%), 35) = trdept2$
            return

        load_msg_amount
            for dl% = 1% to 12%
              if sumkey$(dl%) = " " then return
              str(summary$(dl%),50,30) = " "
              if msg_dsp% = 1% then L30470
                  if tcdate$(dl%) = " " or tcdate$(dl%) = blankdate$ then L30440
                     call "DATEOK" (tcdate$(dl%), trdate%, errormsg$)
L30440:              str(summary$(dl%), 40) = tcdate$(dl%)
                  str(summary$(dl%), 50)    = updmsg$(dl%)
                  goto L30550
L30470:       str(summary$(dl%), 40) = jobh$(dl%)
              str(summary$(dl%), 50) = shift$(dl%)
              str(summary$(dl%), 56) = task$(dl%)
              str(summary$(dl%), 63) = wc$(dl%)
              str(summary$(dl%), 68) = activ$(dl%)
              if hours(dl%) = 0 then L30550
              call "CONVERT"                                             ~
                       (hours(dl%), 2.2, str(summary$(dl%),72,8))
L30550:     next dl%
            return

        load_screen3
            call "READ100" (#1, readkey$, loaded%)
            if loaded% = 0% then return
            get #1 using L32380, rstatus$, rdate$, rtime$, rpcid$,        ~
                rdvcid$, rsource$, rtype$, raction$, rovride$, rbase$,   ~
                rcurr$, rtran$, rvalid$, rerror$, f_data$(),             ~
                rmessage$, rdept$, rchange$, rchng_date$, rrdate$,       ~
                rrtime$, rtrnr%
            call "DATEFMT" (rdate$)
            call "DATEFMT" (rrdate$)
            convert rtrnr% to rtrnr$, pic(######)
            call "TIMEOK"  (rrtime$, temp%, temp$)
                            temp% = temp% : temp$ = temp$
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L32060: FMT                 /* FILE: CDAAUDJT                          */~
            CH(4),          /* Department                              */~
            CH(12),         /* Employee Code                           */~
            CH(6),          /* Key1 Date                               */~
            CH(6),          /* Key1 Time                               */~
            CH(3),          /* Key1 PC ID                              */~
            CH(3),          /* Key1 Device Id                          */~
            CH(1),          /* Source                                  */~
            CH(19),         /* Primary key of transaction              */~
            CH(2),          /* Audit Status                            */~
            CH(2),          /* Code representing action selected or tak*/~
            CH(8),          /* Job Number                              */~
            CH(4),          /* Work Center Code                        */~
            CH(4),          /* Activity Code                           */~
            CH(6),          /* Time Card Task                          */~
            PD(14,4),       /* Hours                                   */~
            BI(4),          /* Shift Code                              */~
            CH(4),          /* Override Dept Code                      */~
            CH(30),         /* Update Message                          */~
            CH(6),          /* Time Card Date                          */~
            CH(124)         /* Unused Space                            */~

        FMT                 /* FILE: CDAHOLD                           */~
            CH(6),          /* Transaction date                        */~
            CH(6),          /* Time Transaction occurred               */~
            CH(03),         /* PC ID                                   */~
            CH(03),         /* Device ID                               */~
            CH(1),          /* Transaction Source                      */~
            CH(2),          /* CDA Transaction Set Identifier          */~
            CH(2),          /* Code representing action selected or tak*/~
            CH(41)          /* Unused Space                            */~

L32380: FMT                 /* FILE: CDARECVE                          */~
            CH(1),          /* Update Status                           */~
            CH(6),          /* Transaction date                        */~
            CH(6),          /* Time Transaction occurred               */~
            CH(03),         /* PC ID                                   */~
            CH(03),         /* Device ID                               */~
            CH(1),          /* Transaction Source                      */~
            CH(1),          /* CDA Transaction Type                    */~
            CH(2),          /* CDA Action Code                         */~
            CH(1),          /* Override Flag                           */~
            CH(3),          /* Base Program Number                     */~
            CH(3),          /* Curr Program Number                     */~
            CH(2),          /* Transaction Set                         */~
            CH(4),          /* Fields Validated                        */~
            CH(4),          /* Fields in Error                         */~
            14*CH(40),      /* CDA Transaction Data Fields             */~
            CH(30),         /* Update Message                          */~
            CH(4),          /* Department                              */~
            CH(3),          /* Changed by                              */~
            CH(6),          /* Changed Date                            */~
            CH(6),          /* Date Received                           */~
            CH(6),          /* Time Received                           */~
            BI(4),          /* Transaction Number                      */~
            CH(109)         /* Unused Space                            */

        REM *************************************************************~
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************
        generate_report
            call "SHOSTAT" ("Report Generation in Progress")
            call "COMPNAME" (12%, company$, 0%)
            rpttitle$ = "CDA TO CMS AUDIT REPORT TYPE (JT)"
            call "FMTTITLE" (rpttitle$, " ", 12%)
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("CDA002", " ", 0%, 0%)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
            return

*       * Report Generation Logic goes here
        print_lines
            get #2 using L32060, trdept$, trempl$, trdate$, trtime$,      ~
                trpc_id$, trdvc_id$, trsrce$, trkey2$, trstatus$,        ~
                traction$, trjob$, trwc$, tractiv$, trtask$, trhours,    ~
                trshift%, trdept2$, trupdmsg$, tr_tcdate$
            call "READ100" (#1, sumkey$(fieldnr%), f1%(1))
                if f1%(1) = 0 then L35270
            get #1 using L32380, rchange$, rchng_date$
               FMT POS(635), CH(3), CH(6)

L35270:     call "DATEOK" (trdate$, trdate%, errormsg$)
            call "TIMEOK" (trtime$, trtime, errormsg$)
            if rchng_date$ = " " or rchng_date$ = blankdate$ then L35310
                call "DATEOK" (rchng_date$, trdate%, errormsg$)
L35310:     if tr_tcdate$ = " " or tr_tcdate$ = blankdate$ then L35330
                call "DATEOK" (tr_tcdate$, trdate%, errormsg$)
            print$ = " "
L35330:     if trhours <> 0 then call "CONVERT" (trhours, 2.2, print$)
            print4$ = " "
            if trshift% <> 0% then convert trshift% to print4$, pic (###)
            if trsrce$ = " " then trsrce$ = "C"

            if lcntr% > 56% then gosub page_head
            print using L64120, trdate$, trtime$, trpc_id$, trdvc_id$,    ~
                 trstatus$, traction$, tr_tcdate$, trempl$, trjob$,      ~
                 trdept2$, print4$, trtask$, trwc$, tractiv$, print$,    ~
                 trupdmsg$
            lcntr% = lcntr% + 1%
            return

        end_report                /* Report Ending Routine */
            if pcntr% <> -1 then L35560
                errormsg$ = "No Records found within the Specified " &   ~
                            "Selection Criteria"
                keyhit% = 2%
                call "ASKUSER" (keyhit%, "****",                         ~
                    "SORRY, no records for Reporting were found",        ~
                    "using the requested selection criteria.",           ~
                    "Press RETURN to change your selection or to exit.")
                goto editpg1
L35560:     print skip(2)
            print using L64000     /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            return

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            if pcntr% = 0% then gosub print_params
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "  CDAQJT"
            print using L60110, rpttitle$, pcntr%
            print
            print using L64040
            print using L64070
            print using L64090
            lcntr% = 7%
            return

        print_params           /* Print Page Zero */
            print page
            tran(i1$(), hex(208c2084208620ac))replacing
            print using L60110, rpttitle$, pcntr%
            print skip(3)
            print using L60170
            print skip(2)
            print using L60190
            print using L60210, "Transaction Dates", fmdate$   , todate$
            print using L60210, "Transaction Times", fmtime$   , totime$
            print using L60210, "Time Card Dates  ", tc_fmdate$,tc_todate$
            print using L60210, "Department Codes ", fmdept$   , todept$
            print using L60210, "Employee Codes   ", fmempl$   , toempl$
            print using L60210, "Job Numbers      ", fmjob$    , tojob$
            print using L60230, s$(1), s$(2), s$(3)
            print using L60250, s$(4), s$(5), s$(6)
            print using L60261, s$(7)
            print skip(2)
            print using L60270
            pcntr% = pcntr% + 1%
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
            gosub'050(1%, fieldnr%)
            gosub set_pf1
            str(line2$,,44) = "Time Card Input (JT) - Selection "   &    ~
                              "Criteria"
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            on fieldnr% gosub L40230,         /* Date Range        */     ~
                              L40230,         /* Time Range        */     ~
                              L40230,         /* Time Card Range   */     ~
                              L40230,         /* Department Range  */     ~
                              L40230,         /* Employee Range    */     ~
                              L40230,         /* Job Number Range  */     ~
                              L40230          /* Audit Status      */
            goto L40260

                lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40230:         lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40260:     accept                                                       ~
               at (01,02),                                               ~
                  "CDA to CMS Audit and Report Functions",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,24), fac(hex(ac)),   columnttl$           , ch(30),~
                                                                         ~
               at (07,02), "Date Range",                                 ~
               at (07,24), fac(lfac$( 1)), fmdate$              , ch(08),~
               at (07,39), fac(lfac$( 1)), todate$              , ch(08),~
                                                                         ~
               at (08,02), "Time Range",                                 ~
               at (08,24), fac(lfac$( 2)), fmtime$              , ch(08),~
               at (08,39), fac(lfac$( 2)), totime$              , ch(08),~
                                                                         ~
               at (09,02), "Time Card Date Range",                       ~
               at (09,24), fac(lfac$( 3)), tc_fmdate$           , ch(08),~
               at (09,39), fac(lfac$( 3)), tc_todate$           , ch(08),~
                                                                         ~
               at (10,02), "Department Range",                           ~
               at (10,24), fac(lfac$( 4)), fmdept$              , ch(04),~
               at (10,39), fac(lfac$( 4)), todept$              , ch(04),~
                                                                         ~
               at (11,02), "Employee Range",                             ~
               at (11,24), fac(lfac$( 5)), fmempl$              , ch(12),~
               at (11,39), fac(lfac$( 5)), toempl$              , ch(12),~
                                                                         ~
               at (12,02), "Job Number Range",                           ~
               at (12,24), fac(lfac$( 6)), fmjob$               , ch(08),~
               at (12,39), fac(lfac$( 6)), tojob$               , ch(08),~
                                                                         ~
               at (13,02), "Audit Status",                               ~
               at (13,24), fac(lfac$( 7)), s$(1)                , ch(01),~
               at (13,26), "Processed",                                  ~
               at (13,44), fac(lfac$( 7)), s$(2)                , ch(01),~
               at (13,46), "On Hold   ",                                 ~
               at (13,58), fac(lfac$( 7)), s$(3)                , ch(01),~
               at (13,60), "Warning",                                    ~
                                                                         ~
               at (14,24), fac(lfac$( 7)), s$(4)                , ch(01),~
               at (14,26), "Warning/Resolved",                           ~
               at (14,44), fac(lfac$( 7)), s$(5)                , ch(01),~
               at (14,46), "Error",                                      ~
               at (14,58), fac(lfac$( 7)), s$(6)                , ch(01),~
               at (14,60), "Error/Resolved",                             ~
                                                                         ~
               at (15,24), fac(lfac$( 7)), s$(7)                , ch(01),~
               at (15,26), "In-Process",                                 ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40880
                  call "MANUAL" ("CDAQJT") : goto L40260

L40880:        if keyhit% <> 15 then L40910
                  call "PRNTSCRN" : goto L40260

L40910:        close ws
               call "SCREEN" addr ("C", u3%, "I", i1$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41100     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffff200dff0f1000)
            if fieldnr% = 1% then L41060
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L41060:     if fieldnr% > 1% then L41080
                str(pf$(1),18,20) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41080:     return

L41100: if fieldnr% > 0% then L41190  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "  (12)Purge Trans    (13)Instructions"
            pf$(2) = "(2)Display Transactions                 " &        ~
                     "  (14)Print Report   (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                     (16)Exit Routine"
            pfkeys$ = hex(0102ffffffffffffffff200c0d0e0f1000)
            return
L41190:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffff200dff0fff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
            gosub'050(2%, fieldnr%)
            gosub set_pf2
            gosub load_msg_amount
            str(line2$,,44) = "Review Time Cards (JT) - Summary"
            if msg_dsp% = 1% then L42180
                hdr1$(1) = "O/R                           "
                hdr1$(2) = "Time Card                     "
                hdr1$(3) = "                              "
                hdr2$(5) = "  Date                        "
                hdr2$(6) = "Audit Update Message      "
                goto L42240
L42180:     hdr1$(1) = "O/R                           "
            hdr1$(2) = "                              "
            hdr1$(3) = "O/R                           "
            hdr2$(5) = " Job #                        "
            hdr2$(6) = "Shift  Task   WC  Act   Hours "
            hdr2$(6) = "Shift" & hex(ac) & " Task " & hex(ac) & " WC "  &~
                       hex(ac) & "Actv" & hex(8cac) & " Hours" & hex(8c)

L42240:     init(hex(8c)) sfac$()

L42260:     accept                                                       ~
               at (01,02),                                               ~
                  "CDA to CMS Audit and Report Functions",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "---Transaction---",                          ~
               at (05,36), fac(hex(8c)), hdr1$(1)               , ch(03),~
               at (05,41), fac(hex(8c)), hdr1$(2)               , ch(09),~
               at (05,52), fac(hex(8c)), hdr1$(3)               , ch(03),~
                                                                         ~
               at (06,02), fac(hex(ac)), hdr2$(1)               , ch(17),~
               at (06,20), fac(hex(ac)), hdr2$(2)               , ch(02),~
               at (06,23), fac(hex(ac)), hdr2$(3)               , ch(12),~
               at (06,36), fac(hex(ac)), hdr2$(4)               , ch(04),~
               at (06,41), fac(hex(ac)), hdr2$(5)               , ch(08),~
               at (06,51), fac(hex(ac)), hdr2$(6)               , ch(30),~
                                                                         ~
               at (07,02), fac(sfac$( 1)), summary$( 1%)        , ch(79),~
               at (08,02), fac(sfac$( 2)), summary$( 2%)        , ch(79),~
               at (09,02), fac(sfac$( 3)), summary$( 3%)        , ch(79),~
               at (10,02), fac(sfac$( 4)), summary$( 4%)        , ch(79),~
               at (11,02), fac(sfac$( 5)), summary$( 5%)        , ch(79),~
               at (12,02), fac(sfac$( 6)), summary$( 6%)        , ch(79),~
               at (13,02), fac(sfac$( 7)), summary$( 7%)        , ch(79),~
               at (14,02), fac(sfac$( 8)), summary$( 8%)        , ch(79),~
               at (15,02), fac(sfac$( 9)), summary$( 9%)        , ch(79),~
               at (16,02), fac(sfac$(10)), summary$(10%)        , ch(79),~
               at (17,02), fac(sfac$(11)), summary$(11%)        , ch(79),~
               at (18,02), fac(sfac$(12)), summary$(12%)        , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L42680
                  call "MANUAL" ("CDAQJT") : goto L42260

L42680:        if keyhit% <> 15 then L42710
                  call "PRNTSCRN" : goto L42260

L42710:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L42900     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L42860
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L42860:     if fieldnr% > 2% then L42880
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L42880:     return

L42900: if fieldnr% > 0% then L43100  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over         (8)Mark As Resolve" &        ~
                     "d    (11)Dsply Msg    (13)Instructions "
            pf$(2) = "(2)First              (9)Unmark Resolved" &        ~
                     "     (14)Print Report (15)Print Screen "
            pf$(3) = "(5)Next Screen                          " &        ~
                     "     (32)Exit Display (16)Select Screen"
            pfkeys$ = hex(0102ffff05ffff0809ff0b200d0e0f1000)
            if dl% = 12% then L43090
                str(pf$(3),1,14)  = " "  :  str(pfkeys$, 5,1) = hex(ff)
L43090:     return
L43100:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
            gosub set_pf3
            str(line2$,,44) = "Review Time Cards (JT) - Detail"
            init(hex(86)) lfac$()
            init(hex(8c)) dfac$()

L44120:     accept                                                       ~
               at (01,02),                                               ~
                  "CDA to CMS Audit and Report Functions",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), "Update Sts",                                 ~
               at (04,13), fac(lfac$( 1)), rstatus$             , ch(01),~
               at (04,23), fac(hex(ac)), hdr3a$                 , ch(15),~
               at (04,40), fac(hex(ac)), hdr3$                  , ch(20),~
                                                                         ~
               at (05,02), "Date",                                       ~
               at (05,13), fac(lfac$( 1)), rdate$               , ch(08),~
               at (05,23), " 1)          ",                              ~
               at (05,40), fac(lfac$( 1)), f_data$(1)           , ch(40),~
                                                                         ~
               at (06,02), "Time",                                       ~
               at (06,13), fac(lfac$( 2)), rtime$               , ch(06),~
               at (06,23), fac(dfac$( 2)), f_desc$(2)           , ch(16),~
               at (06,40), fac(lfac$( 2)), f_data$(2)           , ch(40),~
                                                                         ~
               at (07,02), "PC ID",                                      ~
               at (07,13), fac(lfac$( 3)), rpcid$               , ch(03),~
               at (07,23), fac(dfac$( 3)), f_desc$(3)           , ch(16),~
               at (07,40), fac(lfac$( 3)), f_data$(3)           , ch(40),~
                                                                         ~
               at (08,02), "Device ID",                                  ~
               at (08,13), fac(lfac$( 4)), rdvcid$              , ch(03),~
               at (08,23), fac(dfac$( 4)), f_desc$(4)           , ch(16),~
               at (08,40), fac(lfac$( 4)), f_data$(4)           , ch(40),~
                                                                         ~
               at (09,02), "Source",                                     ~
               at (09,13), fac(lfac$( 5)), rsource$             , ch(01),~
               at (09,23), fac(dfac$( 5)), f_desc$(5)           , ch(16),~
               at (09,40), fac(lfac$( 5)), f_data$(5)           , ch(40),~
                                                                         ~
               at (10,02), "Type",                                       ~
               at (10,13), fac(lfac$( 6)), rtype$               , ch(01),~
               at (10,23), fac(dfac$( 6)), f_desc$(6)           , ch(16),~
               at (10,40), fac(lfac$( 6)), f_data$(6)           , ch(40),~
                                                                         ~
               at (11,02), "Override",                                   ~
               at (11,13), fac(lfac$( 7)), rovride$             , ch(01),~
               at (11,23), fac(dfac$( 7)), f_desc$(7)           , ch(16),~
               at (11,40), fac(lfac$( 7)), f_data$(7)           , ch(40),~
                                                                         ~
               at (12,02), "Base Prog",                                  ~
               at (12,13), fac(lfac$( 8)), rbase$               , ch(03),~
               at (12,23), " 8)          ",                              ~
               at (12,40), fac(lfac$( 8)), f_data$(8)           , ch(40),~
                                                                         ~
               at (13,02), "Curr Prog",                                  ~
               at (13,13), fac(lfac$( 9)), rcurr$               , ch(03),~
               at (13,23), " 9)          ",                              ~
               at (13,40), fac(lfac$( 9)), f_data$(9)           , ch(40),~
                                                                         ~
               at (14,02), "Tran Set",                                   ~
               at (14,13), fac(lfac$(10)), rtran$               , ch(02),~
               at (14,23), "10)          ",                              ~
               at (14,40), fac(lfac$(10)), f_data$(10)          , ch(40),~
                                                                         ~
               at (15,02), "Activity",                                   ~
               at (15,13), fac(lfac$(11)), raction$             , ch(02),~
               at (15,23), "11)          ",                              ~
               at (15,40), fac(lfac$(11)), f_data$(11)          , ch(40),~
                                                                         ~
               at (16,02), "Recvd Date",                                 ~
               at (16,13), fac(lfac$(12)), rrdate$              , ch(08),~
               at (16,23), fac(dfac$(12)), f_desc$(12)          , ch(16),~
               at (16,40), fac(lfac$(12)), f_data$(12)          , ch(40),~
                                                                         ~
               at (17,02), "      Time",                                 ~
               at (17,13), fac(lfac$(13)), rrtime$              , ch(08),~
               at (17,23), fac(dfac$(13)), f_desc$(13)          , ch(16),~
               at (17,40), fac(lfac$(13)), f_data$(13)          , ch(40),~
                                                                         ~
               at (18,02), "Department",                                 ~
               at (18,13), fac(lfac$(14)), rdept$               , ch(04),~
               at (18,23), "14)          ",                              ~
               at (18,40), fac(lfac$(14)), f_data$(14)          , ch(40),~
                                                                         ~
               at (19,02), "Trans Nmbr",                                 ~
               at (19,13), fac(lfac$(15)), rtrnr$               , ch(06),~
               at (19,23), "Message",                                    ~
               at (19,39), fac(lfac$(15)), rmessage$            , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L45070
                  call "MANUAL" ("CDAQJT") : goto L44120

L45070:        if keyhit% <> 15 then L45100
                  call "PRNTSCRN" : goto L44120

L45100:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf3
        if fieldnr% > 0% then L45240  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                     (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                     (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "  (32)Exit Display  (16)Summary Screen"
            pfkeys$ = hex(01ffff04ffffffffffffff200dff0f1000)
            return
L45240:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50170,         /* Date Range             */~
                              L50340,         /* Time Range             */~
                              L50540,         /* Time Card Range        */~
                              L50720,         /* Department Range       */~
                              L50790,         /* Employee Range         */~
                              L50860,         /* Job Number Range       */~
                              L50930          /* Audit Status           */
            return

L50170: REM Test for Date Range                   FMDATE$
            lodate$ = all(hex(00)) : hidate$ = all(hex(ff))
            lodate% = 0%           : hidate% = 99999999%
            if fmdate$ = " " or ~
               fmdate$ = blankdate$ then fmdate$ = "ALL"
            if fmdate$ = "ALL" then todate$ = " "
            if fmdate$ = "ALL" then L50310
                call "DATEOK" (fmdate$, lodate%, errormsg$)
                if errormsg$ <> " " then return
                convert lodate% to lodate$, pic(00000000)
                call "DATECONV" (lodate$)
                if todate$ <> " " or todate$ = blankdate$ then L50280
                     todate$ = fmdate$ : hidate$ = lodate$
                     hidate% = lodate%
                     goto L50310
L50280:     call "DATEOK" (todate$, hidate%, errormsg$)
            if errormsg$ <> " " then return
            convert hidate% to hidate$, pic(00000000)
            call "DATECONV" (hidate$)
L50310:     if lodate% > hidate% then errormsg$ =                        ~
                "FROM Date must be LESS than or EQUAL to the TO Date"
            return

L50340: REM Test for Time Range                   FMTIME$
            lotime$ = all(hex(00)) : hitime$ = all(hex(ff))
            lotime  = 0            : hitime  = 999999
            if fmtime$ = " "   then fmtime$ = "ALL"
            if fmtime$ = "ALL" then totime$ = " "
            if fmtime$ = "ALL" then L50500
                call "TIMEOK" (fmtime$, lotime, errormsg$)
                if errormsg$ <> " " then return
                lotime$ = str(fmtime$,,2) & str(fmtime$,4,2) &           ~
                          str(fmtime$,7,2)
                if totime$ <> " " then L50460
                     totime$ = fmtime$ : hitime = lotime
                     hitime$ = lotime$
                     goto L50500
L50460:     call "TIMEOK" (totime$, hitime, errormsg$)
            if errormsg$ <> " " then return
            hitime$ = str(totime$,,2) & str(totime$,4,2) &               ~
                          str(totime$,7,2)
L50500:     if lotime > hitime then errormsg$ =                          ~
                "FROM Time must be LESS than or EQUAL to the TO Time"
            return

L50540: REM Test for Time Card Range              TC_FMDATE$
            tc_lodate$ = all(hex(00)) : tc_hidate$ = all(hex(ff))
            tc_lodate% = 0%           : tc_hidate% = 99999999%
            if tc_fmdate$ = " " or ~
               tc_fmdate$ = blankdate$ then tc_fmdate$ = "ALL"
            if tc_fmdate$ = "ALL" then tc_todate$ = " "
            if tc_fmdate$ = "ALL" then L50680
                call "DATEOK" (tc_fmdate$, tc_lodate%, errormsg$)
                if errormsg$ <> " " then return
                convert tc_lodate% to tc_lodate$, pic(00000000)
                call "DATECONV" (tc_lodate$)
                if tc_todate$ <> " " and tc_todate$ <> blankdate$ then L50650
                     tc_todate$ = tc_fmdate$ : tc_hidate$ = tc_lodate$
                     tc_hidate% = tc_lodate%
                     goto L50680
L50650:     call "DATEOK" (tc_todate$, tc_hidate%, errormsg$)
            if errormsg$ <> " " then return
            convert tc_hidate% to tc_hidate$, pic(00000000)
            call "DATECONV" (tc_hidate$)
L50680:     if tc_lodate% > tc_hidate% then errormsg$ =                  ~
                "FROM Date must be LESS than or EQUAL to the TO Date"
            return

L50720: REM Test for Department Range             FMDEPT$
            call "TESTRNGE" (fmdept$, todept$,                           ~
                             lodept$, hidept$, errormsg$)
            if fmdept$ <> "ALL" then lodept$ = fmdept$

            return

L50790: REM Test for Employee                     FMEMPL$
            call "TESTRNGE" (fmempl$, toempl$,                           ~
                             loempl$, hiempl$, errormsg$)
            if fmempl$ <> "ALL" then loempl$ = fmempl$

            return

L50860: REM Test for Job Number                   FMJOB$
            call "TESTRNGE" (fmjob$, tojob$,                             ~
                             lojob$, hijob$, errormsg$)
            if fmjob$ <> "ALL" then lojob$ = fmjob$

            return

L50930: REM Test for Audit Status                SEL_STATUS$
            for j% = 1% to 7%
              if str(s$(),j%,1) <> " " then str(s$(),j%,1) = "X"
            next j%
            if s$() <> " " then return
                errormsg$ = "PLEASE SELECT AT LEAST ONE!"
                return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************~

*       * Header Line 1
L60070: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:CDA002

*       * Header Line 2
L60110: %                                     ###########################~
        ~#################################                     PAGE: ####

*       * Report Title for page 0
        %############################################################

L60170: %                         ------------------------- Report Select~
        ~ion Criteria ----------------------------------
L60190: %                                                            From~
        ~...                   To...
L60210: %                         ################################## ####~
        ~##################### #########################
L60230: %                         Audit Statuses Selected            # Pr~
        ~ocessed         # On Hold  # Warning
L60250: %                                                            # Wa~
        ~rning-Resolved  # Error    # Error-Resolved
L60261: %                                                            # In~
        ~-Process
L60270: %                         ---------------------------------------~
        ~-----------------------------------------------
L64000: %                                  * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   * * * * * * * * * *

*       * Detail Report Headings
L64040: %                  PC  DEV         TIME CRD                      ~
        ~ O/R  O/R                    ENTERED

L64070: %    DATE/TIME     ID  ID  ST ACT     DATE    EMPLOYEE    JOB NO ~
        ~ DEPT SHFT  TASK  W.C. ACTV   HOURS  UPDATE MESSAGE
L64090: %----------------- --- --- -- ---  -------- ------------ --------~
        ~ ---- ---- ------ ---- ---- -------- ----------------------------~
        ~--
L64120: %######## ######## ### ### ##  ##  ######## ############ ########~
        ~ #### #### ###### #### #### ######## ############################~
        ~##



        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
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
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            end
