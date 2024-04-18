        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   CCC   DDDD    AAA    QQQ   JJJJJ  IIIII                 *~
            *  C   C  D   D  A   A  Q   Q    J      I                   *~
            *  C      D   D  AAAAA  Q   Q    J      I                   *~
            *  C   C  D   D  A   A  Q  QQ  J J      I                   *~
            *   CCC   DDDD   A   A   QQQQ   J     IIIII                 *~
            *                            Q                              *~
            *-----------------------------------------------------------*~
            * CDAQJI   - Allows reviewing, changing, and purging of     *~
            *            Job-Inventory (JI) transactions received via   *~
            *            the CDA-to-CMS interface.                      *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            * proprietary assets of CAELUS, INCORPORATED, Spokane, WA   *~
            * embodying substantial creative efforts and confidential   *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1988  an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, WA.  All rights reserved.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/06/88 ! Original (Cloned from JO by Mr. Novak).  ! ERN *~
            * 09/15/94 ! Modify for new CDA Hardware/software with! RJH *~
            *          !   the elimination of 'OP' transactions.  !     *~
            * 01/31/96 ! PRR 13558 - No longer falls thru Status  ! RJH *~
            *          !   Filter when purging or printing.       !     *~
            * 09/04/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**


        sub "CDAQJI" (jobin$)

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cursor%(2),                  /* Cursor location for edit   */~
            columnttl$(2)25,             /* Range Description          */~
            company$60,                  /* Company or Division Name   */~
            date$8,                      /* Date for screen display    */~
            dfac$(14)1,                  /* Field Attribute Characters */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            f_data$(14)40,               /* Data area for Print        */~
            f_desc$(14)15,               /* Desc area for Print        */~
            fmact$5,                     /* From CDA Activity Code     */~
            fmdate$10,                   /*      Transaction Date      */~
            fmjob$8,                     /*      Job Number            */~
            fmpart$25,                   /*      Component part number */~
            fmtime$8,                    /*      Transaction Time      */~
            hdr1$(3)30,                  /* Summary Display Headings 1 */~
            hdr2$(2)79,                  /* Summary Display Headings 2 */~
            hdr3$40,                     /* Detail Display Headings    */~
            hiact$5,                     /* High Range- Activity Code  */~
            hidate$10,                   /*             Trans Date     */~
            hijob$8,                     /*             Job Number     */~
            hipart$25,                   /*             Part Number    */~
            hitime$8,                    /*             Trans Time     */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            job$8, jobin$8,              /* Job Number                 */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            loact$5,                     /* Low Range- Activity Code   */~
            lodate$10,                   /*            Trans Date      */~
            lojob$8,                     /*            Job Number      */~
            lopart$25,                   /*            Part Number     */~
            lotime$8,                    /*            Trans Time      */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Misc Plow Key              */~
            r1data$(14)40, r2data$(14)40,/* Raw Trans- Data Fields     */~
            raction$2,                   /*            Action          */~
            rbase$3,                     /*            Base Prog       */~
            rchange$3,                   /*            Changed By      */~
            rchng_date$8,                /*                    On      */~
            rcurr$3,                     /*            Curr Program    */~
            rdate$8,                     /*            Trans Date      */~
            rdept$4,                     /*            Dept (n/a)      */~
            rdvcid$3,                    /*            Device ID       */~
            rerror$4,                    /*            Fields in Err   */~
            rmessage$30,                 /*            Update Message  */~
            rovride$1,                   /*            Override        */~
            rpcid$3,                     /*            PC ID           */~
            rrdate$8, rrtime$,           /*            When received   */~
            rsource$1,                   /*            Source          */~
            rstatus$1,                   /*            Status          */~
            rtime$6,                     /*            Time            */~
            rtran$2,                     /*            Trans Set       */~
            rtrnr$6,                     /*            Trans Number    */~
            rtype$1,                     /*            Trans Type      */~
            rvalid$4,                    /*            Validations     */~
            readkey$99,                  /* Misc Read Key              */~
            rpttitle$60,                 /* Report Title               */~
            s$(6)1,                      /* Update Status              */~
            stv$(6)2,                    /* Actual Value of Status     */~
            sfac$(12)1, summary$(12,2)79,/* Summary Display and FACs   */~
            sumkey$(12)19,               /* Readkey for Each Sumry item*/~
            sumkey2$(12)19,              /* Readkey for each sumry item*/~
            toact$4,                     /* TO CDA Activity Code       */~
            todate$10,                   /*    Transaction Date        */~
            tojob$8,                     /*    Job Number              */~
            topart$25,                   /*    Component Part Number   */~
            totime$8,                    /*    Transaction Time        */~
            traction$2,                  /* Trans Activity Code (CDA)  */~
            trdate$8,                    /*       Date                 */~
            trdue$8,                     /*       New Due Date         */~
            trdvc_id$3,                  /*       Device ID            */~
            trjob$8,                     /*       Job                  */~
            trkey1$19,                   /*       First Half Key       */~
            trkey2$19,                   /*       Second half ky       */~
            trpc_id$3,                   /*       PC ID                */~
            trqty$10,                    /*       Quantity             */~
            trseq$6,                     /*       Sequence Number      */~
            trsrce$,                     /*       Source               */~
            trstatus$2,                  /*       Status               */~
            trtime$8,                    /*       Time                 */~
            trupdmsg$30,                 /*       Update Message       */~
            update_msg$30,               /* Update Message             */~
            userid$3                     /* Current User Id            */


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
            * #02 ! CDAAUDJI ! Audit file for 'JI' Transactions         *~
            * #03 ! CDAHOLD  ! Holding file for first half of 2 part CD *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "CDARECVE",                                      ~
                        varc,     indexed,  recsize =  768,              ~
                        keypos =    2, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  20          ~

            select #02, "CDAAUDJI",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   11, keylen =  19,                     ~
                        alt key  1, keypos =    3, keylen =  27,         ~
                            key  2, keypos =    1, keylen =  29

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

            columnttl$(1) = "From"
            columnttl$(2) = "To"

            str(line2$,62) = "  CDAQJI: " & str(cms2v$,,8)

            stv$() = "P H W WRE ER"

            s% = 1%

*        Set up Display Variables and Descriptor Tables
            hdr1$(1) = "---From--                     "
            hdr1$(2) = "----To---                     "
            hdr1$(3) = " Quantity  Quantity  Quantity "

            hdr2$(1) = "  Date  " & hex(ac) & "Time " & hex(ac) & "Act" &~
                       hex(ac) & "ST" & hex(ac) & "Job Nmbr" & hex(ac)  &~
                       "  Component Part Number  " & hex(ac) & "Str"    &~
                       hex(ac) & " Lot# " & hex(ac) & " Quantity "      &~
                       hex(8c)
            hdr2$(2) = "  Date  " & hex(ac) & "Time " & hex(ac) & "Act" &~
                       hex(ac) & "ST" & hex(ac) & "Job Nmbr" & hex(ac)  &~
                       "New Date" & hex(ac) & " To Job " & hex(ac)      &~
                       "        Update Message        "
            hdr2$(3) = "St        Vendor Name         "
            hdr2$(4) = "Step  WC                      "
            hdr2$(5) = "Step  WC                      "
            hdr2$(6) = "   Good    Scrapped   Rework  "
            hdr3a$   = "Field Desc"
            hdr3$    = "Data Field Contents "

*        Check incomming arguments...
            if jobin$ = " " then L10000
            if jobin$ = last_jobin$ then L12460

            fmjob$, tojob$, last_jobin$ = jobin$
            fmdate$, fmtime$, fmact$, fmpart$ = "ALL"
            s$() = all("X")
            todate$, totime$, tojob$, toact$, topart$ = " "
            for fieldnr% = 1% to 6%
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

            for fieldnr% = 1% to  6%
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
                      if keyhit% = 32% and fieldnr% = 1% then exit_program
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
L11080:     gosub'101(0%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       start_screen2
                  if keyhit%  = 12% then gosub report_or_purge
                  if keyhit%  = 14% then gosub report_or_purge
                  if keyhit%  = 16% then       exit_program
                  if keyhit%  = 32% then       exit_program
                  if keyhit% <>  0% then       editpg1
L11160:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  6% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11210:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit% <> 5% and keyhit% <> 6% then L11240
                     gosub'051(fieldnr%)
L11240:           if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11210
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11210
                  lastfieldnr% = fieldnr%
            goto L11160


        REM *************************************************************~
            *        D I S P L A Y   R O U T I N E   S C R E E N 2      *~
            *-----------------------------------------------------------*~
            * Handles operation of DISPLAY, UPDATE, DELETE for SCREEN2. *~
            *************************************************************

        start_screen2:
            gosub init_screen2
*        Setup First Read of Audit File
            plowkey$ = all(hex(00))
            str(plowkey$,,6) = str(lodate$,,6)
            call "PLOWNEXT" (#2, plowkey$, 0%, f1%(2))
            if f1%(2) = 1% then L12200 else L12470

L12150
*        Get Next Record for Validation
            call "PLOWNEXT" (#2, plowkey$, 0%, f1%(2))
            if f1%(2) = 0% then L12470

*        Setup key to select DATE
L12200:     if str(plowkey$,,6) > hidate$ then L12150
            str(plowkey$,7)   = all(hex(00))
            str(plowkey$,7,6) = lotime$
L12230:     call "PLOWNEXT" (#2, plowkey$, 6%, f1%(2))
            if f1%(2) = 1% then L12260 else L12150

L12260
*        Setup key to select TIME
            if str(plowkey$,7,6) > hitime$ then L12230

*        Get other values from record for continued validation...
            get #2 using L12310, trstatus$, trjob$, traction$, trpart$
L12310:         FMT CH(2), CH(8), POS(30), CH(2), POS(51), CH(25)
            if trjob$    <= lojob$  or trjob$     > hijob$  then L12230
            if traction$ <= loact$  or traction$  > hiact$  then L12230
            if trpart$   <= lopart$ or trpart$    > hipart$ then L12230
            for s1% = 1% to len(s$())
               if s$(s1%) <> "X" then L12380
               if str(stv$(s1%)) = trstatus$ then L12410
L12380:     next s1%
            goto L12230

L12410
*        Record IS within selection criteria
            dl% = dl% + 1%
            gosub load_audit
            if dl% < 12% then goto L12230

L12460
*        Check for records at end-of-file
L12470:     if dl% <> 0% then L12510
                errormsg$ = "No Records found within the Specified " &   ~
                            "Selection Criteria"
                goto editpg1
L12510:     lastfieldnr% = 0%
L12520:     gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                errormsg$ = " "
                if keyhit%  =  1% then gosub startover
                if keyhit%  =  2% then goto start_screen2
                if keyhit% <>  5% then       L12610
                    if dl% <> 12% then goto L12230
                      gosub init_screen2
                      dl% = 0
                      goto L12230
L12610:         if keyhit% <> 11% then L12640
                    s% = mod(s%, 2%) + 1%
                    goto L12520
L12640:         if keyhit%  = 14% then gosub report_or_purge
                if keyhit%  = 28% then gosub report_or_purge
                if keyhit%  = 16% then editpg1
                if keyhit%  = 32% then exit_program
              fieldnr% = cursor%(1%) - 6%
                if fieldnr% < 1% or fieldnr% > dl% then L12520
                if fieldnr% = lastfieldnr% then    L12520
                if keyhit%  =  0% then       display_raw_data
                if keyhit%  =  8% then gosub mark_resolved
                if keyhit%  =  9% then gosub unmark_resolved
                if keyhit% <> 10% then L12760
                    wk% = fieldnr%
                    gosub update_record
L12760:         if keyhit%  = 12% then gosub delete_record
              goto L12520


        display_raw_data
            wk% = fieldnr%
            if sumkey$(wk%) = " " then L12520
            gosub init_screen3
            readkey$ = sumkey$(wk%)
            f_desc$(2) = " 2) Job Number"
            f_desc$(3) = " 3) Comp Part "
            f_desc$(4) = " 4) Store     "
            f_desc$(5) = " 5) Lot       "
            f_desc$(6) = " 6) Quantity  "
            f_desc$(7) = " 7) New Date  "
            f_desc$(11%) = "11) To Job No."
            gosub load_screen3
            if f1%(1) = 1% then L12950
                errormsg$ = "Unable to find detail record"
                goto L12520
L12950:     if sumkey2$(wk%) <> " " then L12980
                inpmessage$ = "Press RETURN to see Summary Screen"
                goto L12990
L12980:     inpmessage$ = "Press RETURN to see 'OP' Transaction Detail"
L12990:     lastfieldnr% = 0%
            gosub'103(0%, 1%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% then       editpg1
                  if keyhit%  = 16% then       L12520
                  if keyhit%  = 32% then       exit_program
                  if keyhit%  =  0% then       L12520
*                IF KEYHIT%  =  0% THEN       DISPLAY_OP_RAW_DATA
            goto L12520

*       DISPLAY_OP_RAW_DATA
*          IF SUMKEY2$(WK%) = " " THEN 12520
*          GOSUB INIT_SCREEN3
*          READKEY$ = SUMKEY2$(WK%)
*          F_DESC$(2) = " 2)          "
*          F_DESC$(3) = " 3)          "
*          F_DESC$(4) = " 4) To Job   "
*          F_DESC$(5) = " 5)          "
*          F_DESC$(6) = " 6)          "
*          F_DESC$(7) = " 7)          "
*          GOSUB LOAD_SCREEN3
*          IF F1%(1) = 1% THEN 13220
*              ERRORMSG$ = "Unable to find detail record"
*              GOTO 12520
*          INPMESSAGE$ = "Press RETURN to see Summary Screen   "
*          LASTFIELDNR% = 0%
*          GOSUB'103(0%, 2%)           /* Display Screen - No Entry   */
*                IF KEYHIT%  =  1% THEN GOSUB STARTOVER
*                IF KEYHIT%  =  4% THEN       EDITPG1
*                IF KEYHIT%  = 16% THEN       12520
*                IF KEYHIT%  = 32% THEN       EXIT_PROGRAM
*                IF KEYHIT% <>  0% THEN       13240
            goto L12520

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
            get #2 using L14120, trstatus$
L14120:        FMT CH(2)
            if trstatus$ = "E " or trstatus$ = "W " then L14170
              errormsg$ =                                                ~
               "Only Status 'E ' and 'W ' records can be Marked Resolved"
              return
L14170:     str(trstatus$,2,1) = "R"
            str(summary$(fieldnr%,1),20,2) = trstatus$
            str(summary$(fieldnr%,2),20,2) = trstatus$
            put #2 using L14120, trstatus$
            rewrite #2
            return


        unmark_resolved
            readkey$ = sumkey$(fieldnr%)
            call "READ101" (#2, readkey$, f1%(2))
            if f1%(2) = 0% then return
            get #2 using L14300, trstatus$
L14300:        FMT CH(2)
            if trstatus$ = "ER" or trstatus$ = "WR" then L14350
              errormsg$ =                                                ~
               "Only Status 'ER' and 'WR' records can be Unmarked"
              return
L14350:     str(trstatus$,2,1) = " "
            str(summary$(fieldnr%,1),20,2) = trstatus$
            str(summary$(fieldnr%,2),20,2) = trstatus$
            put #2 using L14300, trstatus$
            rewrite #2
            return


        update_record
*        Get transaction(s) from CDARECVE...
            wk% = fieldnr%
            if str(summary$(wk%,1),20,1) = "E" then L14490
L14470:         errormsg$ = "Only 'E' Status records can be Modified"
                return
L14490:     call "READ100" (#2, sumkey$(wk%), f1%(2))      /* cdaaudji */
            if f1%(2) = 1% then L14530
                errormsg$ = "Could not relocate audit file record"
                return
L14530
*          GET #2 USING 14540, TRKEY2$
*              FMT POS(32), CH(19)
            call "READ100" (#1, sumkey$(wk%), f1%(1))  /* cdarecve #1  */
            if f1%(1) = 1% then L14590
               errormsg$ = "Could not locate transaction record"
               return
L14590:     get #1 using L14600, rstatus$, rtran$, r1data$()
L14600:         FMT CH(1), POS(31), CH(2), POS(41), 14*CH(40)
            if rstatus$ <> "E"  then L14470
            if rtran$   =  "JI" then L14660
                errormsg$ = "This transaction is not modifiable: " &     ~
                            " First half of transaction is missing."
                return
L14660
*          IF TRKEY2$ = " " THEN 14730
*              CALL "READ100" (#1, TRKEY2$, F1%(1))   /* cdarecve #2  */
*              IF F1%(2) = 1% THEN 14710
*                   ERRORMSG$ = "Could not find 2nd half of transaction"
*                   RETURN
*              GET #1 USING 14600, RSTATUS$, RTRAN$,  R2DATA$()

*        Display screen and get modifications...
            lastfieldnr% = 0%
L14750:     gosub'104(fieldnr%, 2%)
                if keyhit%  = 1%   then return  /* No harm, no foul    */
                if keyhit%  <> 16% then L14750

*        Resubmit Transaction(s), delete audit record...
            call "READ101" (#1, sumkey$(wk%), f1%(1))  /* cdarecve #1  */
            if f1%(1) = 1% then L14840
                errormsg$ = "Sorry, unable to record modifications"
                return
L14840:     put #1 using L14850, "N", r1data$(), userid$, date
L14850:         FMT POS(1), CH(1), POS(41), 14*CH(40), POS(635),         ~
                    CH(3), CH(6)
            rewrite #1
*          IF TRKEY2$ = " " THEN 14930
*              CALL "READ101" (#1, TRKEY2$, F1%(1))   /* cdarecve #2  */
*              IF F1%(1) = 0% THEN 14930
*              PUT #1 USING 14850, "N", R2DATA$(), USERID$, DATE
*              REWRITE #1
            str(summary$(wk%,1),20,2) = "**"
            str(summary$(wk%,2),20,2) = "**"
            call "DELETE" (#2, sumkey$(wk%), 19%)
            call "TASKUP" ("CD", u3%)    /* Start the update task      */
            return


        delete_record
            u3% = 2%
            call "ASKUSER" (u3%, "DELETE CDAAUDJI",                      ~
                            "Press PF-1 to cancel delete", "-OR-",       ~
                            "Press RETURN to DELETE " &                  ~
                                           str(summary$(fieldnr%,1),,17))
            if u3% <> 0% then return
*        Delete CDAAUDJI
            readkey$ = sumkey$(fieldnr%)
            call "READ100" (#2, readkey$, f1%(2))
            str(summary$(fieldnr%,1),20,2) = "D "
            str(summary$(fieldnr%,2),20,2) = "D "
            if f1%(2) <> 1% then L15250
                get #2 using L15140, trstatus$, trkey1$, trkey2$
L15140:             FMT CH(2), POS(11), CH(19), POS(32), CH(19)
                call "DELETE" (#2, readkey$, 19%)
*        Delete second half if it exists
                if trkey2$ <> " " then call "DELETE" (#1, trkey2$, 19%)
*        If Error record, Delete CDARECVE
                if str(trstatus$,1,1) = "E" then                         ~
                                        call "DELETE" (#1, trkey1$, 19%)
*        If Hold status, Delete CDAHOLD and CDARECVE
                if trstatus$ <> "H " then L15250
                    call "DELETE" (#1, trkey1$, 19%)
                    call "DELETE" (#3, trkey1$, 19%)
L15250:     return

        REM *************************************************************~
            *          P R I N T / P U R G E   R O U T I N E            *~
            *-----------------------------------------------------------*~
            * Print or Purge Records Per Selection Criteria             *~
            *************************************************************

        report_or_purge:
            gosub init_report
            if keyhit% = 14% then gosub generate_report
            if keyhit% <> 28% then L16160
                u3% = 2%
                call "ASKUSER" (u3%, "PURGE CDAAUDJI",                   ~
                     "Note: This will Purge 'ALL' records within your",  ~
                     "Selection Criteria, Press PF-1 to CANCEL Purge",   ~
                     "-OR-, Press RETURN to Purge Audit Records.")
                if u3% <> 0% then return
L16160:     plowkey$ = all(hex(00))
            str(plowkey$,1,6) = lodate$
            call "PLOWNEXT" (#2, plowkey$, 0%, f1%(2))
            if f1%(2) = 1% then L16270
                errormsg$ = "No CDA Audit Records Type 'JI' to Process"
                goto L11080

L16230
*        Get Next Record for Validation
L16240:     call "PLOWNEXT" (#2, plowkey$, 0%, f1%(2))
            if f1%(2) = 0% then L16540

L16270
*        Setup key to select DATE
            if str(plowkey$,1,6) > hidate$ then L16240
            str(plowkey$,7) = all(hex(00))
            str(plowkey$,7,6) = lotime$
L16310:     call "PLOWNEXT" (#2, plowkey$, 6%, f1%(2))
            if f1%(2) = 0% then L16230

*        Setup key to select TIME
            if str(plowkey$,7,6) > hitime$ then L16310

*        Get Values from record for continued validation
            get #2 using L16400, trstatus$, trjob$, traction$,            ~
                                trkey2$, trpart$
L16400:         FMT CH(2), CH(8), POS(30), CH(2), CH(19), CH(25)
            if trjob$    <= lojob$  or trjob$     > hijob$  then L16310
            if traction$ <= loact$  or traction$  > hiact$  then L16310
            if trpart$   <= lopart$ or trpart$    > hipart$ then L16310
            for s1% = 1% to len(s$())
               if s$(s1%) <> "X" then L16470
               if str(stv$(s1%)) = trstatus$ then L16490
L16470:     next s1%
            goto L16310  /* Must have failed */

L16490
*        Record is Valid for Report or Purge...
            if keyhit% = 14% then gosub print_lines
            if keyhit% = 28% then gosub purge_rtn
            goto L16310

L16540
*        Print End of Report if Producing Report
            if keyhit% = 14% then gosub end_report
            if keyhit% = 14% then return else editpg1


        purge_rtn
            call "DELETE" (#2, plowkey$, 19%)              /* cdaaudji */
            if str(trstatus$,1,1) <> "E" then return
                call "DELETE" (#1, plowkey$, 19%)          /* cdarecve */
                if trkey2$ <> " " then                                   ~
                        call "DELETE" (#1, trkey2$ , 19%)  /* cdarecve */
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20150,         /* Date Range             */~
                              L20200,         /* Time Range             */~
                              L20250,         /* Job Number Range       */~
                              L20300,         /* Step                   */~
                              L20350,         /* Work Center            */~
                              L20400          /* Update Status          */
            return
L20150: REM Def/Enable DATE RANGE                  FMDATE$
            if fmdate$ = " " or fmdate$ = blankdate$ then fmdate$ = "ALL"
            return

L20200: REM Def/Enable TIME RANGE                  FMTIME$
            if fmtime$ = " " then fmtime$ = "ALL"
            return

L20250: REM Def/Enable JOB NUMBER RANGE            FMJOB$
            if fmjob$ = " " then fmjob$ = "ALL"
            return

L20300: REM Def/Enable COMPONENT PART NUMBER RANGE FMPART$
            if fmpart$ = " " then fmpart$ = "ALL"
            return

L20350: REM Def/Enable CDA ACTIVITY                SEL_WC$
            if fmact$ = " " then fmact$ = "ALL"
            return

L20400: REM Def/Enable Update Status               S$()
            if s$() = " " then s$() = all("X")
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
         "Enter Date Range.                                            ",~
         "Enter Time Range.                                            ",~
         "Enter Job Number Range.                                      ",~
         "Enter Component Part Number Range.                           ",~
         "Enter CDA Activity Code Range.                               ",~
         "Enter a Non-Blank character for each Audit Status desired.   "

        scrn2_msg  :  data                                               ~
         "Position Cursor and Press Return to see Transaction Detail.  "

        scrn3_msg  :  data                                               ~
         "Press Return to see Summary Screen.                          "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, job$, trpart$, trdue$,     ~
                      trstore$, trlot$, trtojob$, trstatus$,             ~
                      trstatus$, trdate$, update_msg$,                   ~
                      fmdate$, fmjob$, fmpart$, topart$, fmact$, toact$, ~
                      fmtime$, hidate$, hijob$, lopart$, hipart$,        ~
                      hitime$, lodate$, lojob$, lotime$, todate$,        ~
                      tojob$, totime$, s$(), loact$, hiact$
            return

        init_screen2
            init (" ") summary$(), sumkey$(), errormsg$, inpmessage$,    ~
                       sumkey2$()
            dl%, wk%  = 0%
            return

        init_report
        REM GOSUB INITIALIZE_VARIABLES
            return

        init_screen3
            init (" ") rstatus$, rdate$, rtime$, rpcid$, rdvcid$,        ~
                      rsource$, rtype$, rovride$, rbase$, rcurr$,        ~
                      rtran$, raction$, rchange$, rchng_date$,           ~
                      rmessage$, f_data$(), errormsg$,                   ~
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
        dataload     /* Load data from Audit File for ?...             */
*        Load data from Audit File for Report
            get #2 using L32060, trstatus$, trjob$, trdate$, trtime$,     ~
                trpc_id$, trdvc_id$, trsrce$, traction$, trkey2$,        ~
                trpart$, trstore$, trlot$, trqty, trdue$, trtojob$,      ~
                trupdmsg$, trseq%
            call "DATEFMT" (trdue$)
            if str(traction$,,1) = "0" then str(traction$,,1) = " "
            convert trseq% to trseq$, pic(######)
            trqty$ = " "
            if trqty <> 0 then call "CONVERT" (trqty, 2.2, trqty$)
            return

        load_audit
*        Load Data from Audit file to Summary area for display
            gosub dataload
            sumkey$ (dl%) = plowkey$
            sumkey2$(dl%) = trkey2$
            call "DATEFMT" (trdate$)
            str(summary$(dl%,1),  1) = trdate$
            call "TIMEOK" (trtime$, trtime, errormsg$)
            str(summary$(dl%,1), 10) = str(trtime$,,5)
            str(summary$(dl%,1), 16) = traction$
            str(summary$(dl%,1), 20) = trstatus$
            str(summary$(dl%,1), 23) = trjob$
            str(summary$(dl%,1), 32) = trpart$
            str(summary$(dl%,1), 58) = trstore$
            str(summary$(dl%,1), 62) = trlot$
            str(summary$(dl%,1), 69) = trqty$
            summary$(dl%,2) = summary$(dl%,1)
            str(summary$(dl%,2), 32) = trdue$
            str(summary$(dl%,2), 41) = trtojob$
            str(summary$(dl%,2), 50) = trupdmsg$
            return


        load_screen3
            call "READ100" (#1, readkey$, f1%(1))
            if f1%(1%) = 0% then return

            get #1 using L32360, rstatus$, rdate$, rtime$, rpcid$,        ~
                rdvcid$, rsource$, rtype$, raction$, rovride$, rbase$,   ~
                rcurr$, rtran$, rvalid$, rerror$, f_data$(),             ~
                rmessage$, rdept$, rchange$, rchng_date$,                ~
                rrdate$, rrtime$, rtrnr%
            call "DATEFMT" (f_data$(7%))
            convert rtrnr% to rtrnr$, pic(######)
            call "DATEFMT" (rdate$)
            call "DATEFMT" (rrdate$)
            call "TIMEOK"  (rrtime$, temp, inpmessage$)
                            temp = temp  : inpmessage$ = " "
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L32060: FMT                 /* FILE: CDAAUDJI                          */~
            CH(2),          /* Audit Status                            */~
            CH(8),          /* Job Number                              */~
            CH(6),          /* Key1 Date                               */~
            CH(6),          /*      Time                               */~
            CH(3),          /*      PC ID                              */~
            CH(3),          /*      Device Id                          */~
            CH(1),          /*      Source                             */~
            CH(2),          /* CDA Activity (aka Action) Code          */~
            CH(19),         /* Primary key of 'OP' transaction         */~
            CH(25),         /* Part Number                             */~
            CH(3),          /* Store                                   */~
            CH(6),          /* Lot                                     */~
            PD(14,4),       /* Quantity                                */~
            CH(6),          /* New Due Date                            */~
            CH(8),          /* To Job Number                           */~
            CH(30),         /* Update Message                          */~
            BI(4)           /* Transaction Sequence Number             */

        FMT                 /* FILE: CDAHOLD                           */~
            CH(6),          /* Transaction date                        */~
            CH(6),          /* Time Transaction occurred               */~
            CH(03),         /* PC ID                                   */~
            CH(03),         /* Device ID                               */~
            CH(1),          /* Transaction Source                      */~
            CH(2),          /* CDA Transaction Set Identifier          */~
            CH(2),          /* Code representing action selected or tak*/~
            CH(41)          /* Unused Space                            */~

L32360: FMT                 /* FILE: CDARECVE                          */~
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
            CH(6),          /* Reciept Date                            */~
            CH(6),          /* Reciept Time                            */~
            BI(4),          /* Transaction Number                      */~
            CH(109)         /* Unused Space                            */~

        REM *************************************************************~
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************
        generate_report:
            call "SHOSTAT" ("Report Generation in Progress")
            call "COMPNAME" (12%, company$, 0%)
            rpttitle$ = "CDA TO CMS AUDIT REPORT TYPE (JI)"
            call "FMTTITLE" (rpttitle$, " ", 12%)
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("CDA003", " ", 0%, 0%)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
            return

*       * Report Generation Logic goes here
        print_lines:
            gosub dataload

            call "DATEFMT" (trdate$)
            call "TIMEOK"  (trtime$, trtime, errormsg$)

            if lcntr% > 56% then gosub page_head

            print using L60420, trdate$, trtime$, trseq$, traction$,      ~
                 trpc_id$, trdvc_id$, trjob$, trstore$, trlot$, trqty$,  ~
                 trtojob$, trdue$, trstatus$, trupdmsg$
            if trpart$ = " " then L35310
                print using L60450, trpart$
                lcntr% = lcntr% + 1%
L35310:     lcntr% = lcntr% + 1%
            return

        end_report                /* Report Ending Routine */
            if pcntr% <> -1 then L35380
                errormsg$ = "No Records found within the Specified " &   ~
                            "Selection Criteria"
L35380:     print skip(2)
            print using L60290     /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            return

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            if pcntr% = 0% then gosub print_params
            print page
            print using L60070, date$, time$, company$, "  CDAQJI"
            print using L60110, rpttitle$, pcntr%
            print
            print using L60330
            print using L60360
            print using L60390
            lcntr% = 7%
            return

        print_params           /* Print Page Zero */
            print page
            print using L60110, rpttitle$, pcntr%
            print skip(3)
            print using L60170
            print skip(2)
            print using L60190
            print using L60210, "Transaction Dates ", fmdate$, todate$
            print using L60210, "Transaction Times ", fmtime$, totime$
            print using L60210, "Job Numbers       ", fmjob$ , tojob$
            print using L60210, "Component Parts   ", fmpart$, topart$
            print using L60210, "CDA Activity Codes", fmact$ , toact$
            print using L60230, s$(1), s$(2), s$(3)
            print using L60250, s$(4), s$(5), s$(6)
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
            str(line2$,,61) = "Job-Inventory Movement (JI) - Selec" &    ~
                              "tion Criteria"
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            on fieldnr% gosub L40220,         /* Date Range        */     ~
                              L40220,         /* Time Range        */     ~
                              L40220,         /* Job Number Range  */     ~
                              L40220,         /* Component Part #  */     ~
                              L40220,         /* CDA Activity Code */     ~
                              L40220          /* Audit Status      */
            goto L40250

                lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40220:         lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40250:     accept                                                       ~
               at (01,02),                                               ~
                  "CDA to CMS Audit and Report Functions",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,22), fac(hex(ac)),   columnttl$(1)        , ch(25),~
               at (06,50), fac(hex(ac)),   columnttl$(2)        , ch(25),~
                                                                         ~
               at (07,02), "Date Range",                                 ~
               at (07,22), fac(lfac$( 1)), fmdate$              , ch(10),~
               at (07,50), fac(lfac$( 1)), todate$              , ch(10),~
                                                                         ~
               at (08,02), "Time Range",                                 ~
               at (08,22), fac(lfac$( 2)), fmtime$              , ch(08),~
               at (08,50), fac(lfac$( 2)), totime$              , ch(08),~
                                                                         ~
               at (09,02), "Job Number Range",                           ~
               at (09,22), fac(lfac$( 3)), fmjob$               , ch(08),~
               at (09,50), fac(lfac$( 3)), tojob$               , ch(08),~
                                                                         ~
               at (10,02), "Component Part",                             ~
               at (10,22), fac(lfac$( 4)), fmpart$              , ch(25),~
               at (10,50), fac(lfac$( 4)), topart$              , ch(25),~
                                                                         ~
               at (11,02), "CDA Activity Code",                          ~
               at (11,22), fac(lfac$( 5)), fmact$               , ch(05),~
               at (11,50), fac(lfac$( 5)), toact$               , ch(04),~
                                                                         ~
               at (12,02), "Audit Status",                               ~
               at (12,22), fac(lfac$( 6)), s$(1)                , ch(01),~
               at (12,24), "Processed",                                  ~
               at (12,42), fac(lfac$( 6)), s$(2)                , ch(01),~
               at (12,44), "On Hold   ",                                 ~
               at (12,56), fac(lfac$( 6)), s$(3)                , ch(01),~
               at (12,58), "Warning",                                    ~
                                                                         ~
               at (13,22), fac(lfac$( 6)), s$(4)                , ch(01),~
               at (13,24), "Warning/Resolved",                           ~
               at (13,42), fac(lfac$( 6)), s$(5)                , ch(01),~
               at (13,44), "Error",                                      ~
               at (13,56), fac(lfac$( 6)), s$(6)                , ch(01),~
               at (13,58), "Error/Resolved",                             ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40780
                  call "MANUAL" ("CDAQJI") : goto L40250

L40780:        if keyhit% <> 15 then L40810
                  call "PRNTSCRN" : goto L40250

L40810:        call "GETSCRN" ("C", i$(), cursor%(), u3%)
               return

        set_pf1
        if edit% = 2% then L41030     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffff200dff0f1000)
            if fieldnr% = 1% then L40960
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40960:     if fieldnr% > 1% then L41010
                str(pf$(1),18,20) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41010:     return

L41030: if fieldnr% > 0% then L41150  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "  (12)Purge Trans    (13)Instructions"
            pf$(2) = "(2)Display Transactions                 " &        ~
                     "  (14)Print Report   (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                     (16)Exit Program"
            pfkeys$ = hex(0102ffffffffffffffff200c0d0e0f1000ff)
            return
L41150:                              /*  Edit Mode - Enabled    */
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
            str(line2$,,61) = "Review Job/Inventory Movement (JI) -" &   ~
                              " Summary"
            init(hex(8c)) sfac$()

L42130:     accept                                                       ~
               at (01,02),                                               ~
                  "CDA to CMS Audit and Report Functions",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), fac(hex(ac)), hdr2$(s%)              , ch(79),~
                                                                         ~
               at (07,02), fac(sfac$( 1)), summary$( 1%,s%)     , ch(79),~
               at (08,02), fac(sfac$( 2)), summary$( 2%,s%)     , ch(79),~
               at (09,02), fac(sfac$( 3)), summary$( 3%,s%)     , ch(79),~
               at (10,02), fac(sfac$( 4)), summary$( 4%,s%)     , ch(79),~
               at (11,02), fac(sfac$( 5)), summary$( 5%,s%)     , ch(79),~
               at (12,02), fac(sfac$( 6)), summary$( 6%,s%)     , ch(79),~
               at (13,02), fac(sfac$( 7)), summary$( 7%,s%)     , ch(79),~
               at (14,02), fac(sfac$( 8)), summary$( 8%,s%)     , ch(79),~
               at (15,02), fac(sfac$( 9)), summary$( 9%,s%)     , ch(79),~
               at (16,02), fac(sfac$(10)), summary$(10%,s%)     , ch(79),~
               at (17,02), fac(sfac$(11)), summary$(11%,s%)     , ch(79),~
               at (18,02), fac(sfac$(12)), summary$(12%,s%)     , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L42460
                  call "MANUAL" ("CDAQJI") : goto L42130

L42460:        if keyhit% <> 15 then L42490
                  call "PRNTSCRN" : goto L42130

L42490:        call "GETSCRN" ("C", i$(), cursor%(), u3%)
               return

        set_pf2
        if edit% = 2% then L42670     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L42630
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L42630:     if fieldnr% > 2% then L42650
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L42650:     return

L42670: if fieldnr% > 0% then L42780  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over         (8)Mark As Resolve" &        ~
                     "d    (11)Alter Screen (13)Instructions "
            pf$(2) = "(2)First              (9)Unmark Resolved" &        ~
                     "  (12/28)Delete/Purge (15)Print Screen "
            pf$(3) = "(5)Next Screen       (10)Modify & Resubm" &        ~
                     "it   (14)Print Report (16)Select Screen"
            pfkeys$ = hex(0102ffff05ff2008090a0b0c0d0e0f10001c)
            if dl% = 12% then L42770
                str(pf$(3),1,14)  = " "  :  str(pfkeys$, 5,1) = hex(ff)
L42770:     return
L42780:                              /*  Edit Mode - Enabled    */
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
            str(line2$,,61) = "Review Job-Inventory Movement (JI) -" &   ~
                              " Detail"
            init(hex(86)) lfac$()
            init(hex(8c)) dfac$()

L44130:     accept                                                       ~
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
               at (06,23), fac(dfac$( 2)), f_desc$(2)           , ch(15),~
               at (06,40), fac(lfac$( 2)), f_data$(2)           , ch(40),~
                                                                         ~
               at (07,02), "PC ID",                                      ~
               at (07,13), fac(lfac$( 3)), rpcid$               , ch(03),~
               at (07,23), fac(dfac$( 3)), f_desc$(3)           , ch(15),~
               at (07,40), fac(lfac$( 3)), f_data$(3)           , ch(40),~
                                                                         ~
               at (08,02), "Device ID",                                  ~
               at (08,13), fac(lfac$( 4)), rdvcid$              , ch(03),~
               at (08,23), fac(dfac$( 4)), f_desc$(4)           , ch(15),~
               at (08,40), fac(lfac$( 4)), f_data$(4)           , ch(40),~
                                                                         ~
               at (09,02), "Source",                                     ~
               at (09,13), fac(lfac$( 5)), rsource$             , ch(01),~
               at (09,23), fac(dfac$( 5)), f_desc$(5)           , ch(15),~
               at (09,40), fac(lfac$( 5)), f_data$(5)           , ch(40),~
                                                                         ~
               at (10,02), "Type",                                       ~
               at (10,13), fac(lfac$( 6)), rtype$               , ch(01),~
               at (10,23), fac(dfac$( 6)), f_desc$(6)           , ch(15),~
               at (10,40), fac(lfac$( 6)), f_data$(6)           , ch(40),~
                                                                         ~
               at (11,02), "Override",                                   ~
               at (11,13), fac(lfac$( 7)), rovride$             , ch(01),~
               at (11,23), fac(dfac$( 7)), f_desc$(7)           , ch(15),~
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
               at (15,13), fac(lfac$(11%)), raction$            , ch(02),~
               at (15,23), fac(dfac$(11%)), f_desc$(11%)        , ch(15),~
               at (15,40), fac(lfac$(11%)), f_data$(11%)        , ch(40),~
                                                                         ~
               at (16,02), "Changed By",                                 ~
               at (16,13), fac(lfac$(12)), rchange$             , ch(03),~
               at (16,23), "12)          ",                              ~
               at (16,40), fac(lfac$(12)), f_data$(12)          , ch(40),~
                                                                         ~
               at (17,02), "        On",                                 ~
               at (17,13), fac(lfac$(13)), rchng_date$          , ch(08),~
               at (17,23), "13)          ",                              ~
               at (17,40), fac(lfac$(13)), f_data$(13)          , ch(40),~
                                                                         ~
               at (18,02), "Trans Nmbr",                                 ~
               at (18,13), fac(lfac$(14)), rtrnr$               , ch(06),~
               at (18,23), "14)          ",                              ~
               at (18,40), fac(lfac$(14)), f_data$(14)          , ch(40),~
                                                                         ~
               at (19,23), "Message",                                    ~
               at (19,39), fac(lfac$(15)), rmessage$            , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L45080
                  call "MANUAL" ("CDAQJI") : goto L44130

L45080:        if keyhit% <> 15 then return
                  call "PRNTSCRN" : goto L44130

        set_pf3
        if fieldnr% > 0% then L45210  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                     (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                     (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                     (16)Summary Screen"
            pfkeys$ = hex(01ffff04ffffffffffffff200dff0f1000)
            return
L45210:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffff200dff0fff00)
            return

        REM *************************************************************~
            *               U P D A T E   S C R E E N   2               *~
            *-----------------------------------------------------------*~
            * Update Screen                                             *~
            *************************************************************

        deffn'104(fieldnr%, edit%)
            gosub set_pf4
            str(line2$,,61) = "Modify Job-Inventory Movement (JI)"
            inpmessage$ = "Press PF1 to CANCEL Update, or " &            ~
                          "PF-16 to RESUBMIT transaction."

L46120:     accept                                                       ~
               at (01,02),                                               ~
                  "CDA to CMS Audit and Report Functions",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(ac)), hdr2$(1),                       ~
               at (06,02), fac(hex(8c)), summary$(wk%,1%),               ~
               at (08,32), fac(hex(ac)), str(hdr2$(2),31),               ~
               at (09,32), fac(hex(8c)), str(summary$(wk%,2%),31),       ~
                                                                         ~
               at (11,02), "Job Number",                                 ~
               at (11,25), fac(hex(81)), r1data$(2)                     ,~
               at (12,02), "Component Part",                             ~
               at (12,25), fac(hex(81)), r1data$(3)                     ,~
               at (13,02), "Store/Warehouse",                            ~
               at (13,25), fac(hex(81)), r1data$(4)                     ,~
               at (14,02), "Lot Number",                                 ~
               at (14,25), fac(hex(81)), r1data$(5)                     ,~
               at (15,02), "Quantity",                                   ~
               at (15,25), fac(hex(82)), r1data$(6)                     ,~
               at (16,02), "New Due Date (MMDDYY)",                      ~
               at (16,25), fac(hex(82)), r1data$(7)                     ,~
               at (17,02), "To Job Number",                              ~
               at (17,25), fac(hex(81)), r2data$(4)                     ,~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L46490
                  call "MANUAL" ("CDAQJI") : goto L46120

L46490:        if keyhit% <> 15 then L46520
                  call "PRNTSCRN" : goto L46120

L46520:        call "GETSCRN" ("C", i$(), cursor%(), u3%)
               return

        set_pf4
        if edit% = 2% then L46700     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffff200dff0f1000)
            if fieldnr% = 1% then L46660
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L46660:     if fieldnr% > 2% then L46680
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L46680:     return

L46700: if fieldnr% > 0% then L46810  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over         (8)Mark As Resolve" &        ~
                     "d    (11)Dsply Msg    (13)Instructions "
            pf$(2) = "(2)First              (9)Unmark Resolved" &        ~
                     "  (12/28)Delete/Purge (15)Print Screen "
            pf$(3) = "(5)Next Screen       (10)Modify & Resubm" &        ~
                     "it   (14)Print Report (16)Select Screen"
            pfkeys$ = hex(0102ffff05ff2008090a0b0c0d0e0f10001c)
            if dl% = 12% then L46800
                str(pf$(3),1,14)  = " "  :  str(pfkeys$, 5,1) = hex(ff)
L46800:     return
L46810:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Cancel Changes                       " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)RESUBMIT    "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f10ff)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50160,         /* Date Range             */~
                              L50340,         /* Time Range             */~
                              L50530,         /* Job Number Range       */~
                              L50600,         /* Component Part Range   */~
                              L50640,         /* CDA Activity Range     */~
                              L50680          /* Audit Status           */
            return

L50160: REM Test for Date Range                   FMDATE$
            lodate$ = all(hex(00)) : hidate$ = all(hex(ff))
            if fmdate$ = " " or fmdate$ = blankdate$ then fmdate$ = "ALL"
            if fmdate$ = "ALL" then todate$ = " "
            if fmdate$ = "ALL" then L50300
                call "DATEOKC" (fmdate$, 0%, errormsg$)
                if errormsg$ <> " " then return
                lodate$ = fmdate$
                call "DATUFMTC" (lodate$)
                if todate$ <> " " and todate$ <> blankdate$ then L50270
                     todate$ = fmdate$ : hidate$ = lodate$
                     hidate$ = lodate$
                     goto L50300
L50270:     call "DATEOKC" (todate$, 0%, errormsg$)
            if errormsg$ <> " " then return
            hidate$ = todate$
            call "DATUFMTC" (hidate$)
L50300:     if lodate$ > hidate$ then errormsg$ =                        ~
                "FROM Date must be LESS than or EQUAL to the TO Date"
            return

L50340: REM Test for Time Range                   FMTIME$
            lotime$ = all(hex(00)) : hitime$ = all(hex(ff))
            if fmtime$ = " " then fmtime$ = "ALL"
            if fmtime$ = "ALL" then totime$ = " "
            if fmtime$ = "ALL" then L50490
                call "TIMEOK" (fmtime$, lotime, errormsg$)
                if errormsg$ <> " " then return
                lotime$ = str(fmtime$,,2) & str(fmtime$,4,2) &           ~
                          str(fmtime$,7,2)
                if totime$ <> " " then L50450
                     totime$ = fmtime$ : hitime = lotime
                     hitime$ = lotime$
                     goto L50490
L50450:     call "TIMEOK" (totime$, hitime, errormsg$)
            if errormsg$ <> " " then return
            hitime$ = str(totime$,,2) & str(totime$,4,2) &               ~
                          str(totime$,7,2)
L50490:     if lotime > hitime then errormsg$ =                          ~
                "FROM Time must be LESS than or EQUAL to the TO Time"
            return

L50530: REM Test for Job Number Range             FMJOB$
            call "TESTRNGE" (fmjob$, tojob$, lojob$, hijob$, errormsg$)
            return

L50600: REM Test for COMPONENT PART RANGE         SEL_STEP$
            call "TESTRNGE" (fmpart$, topart$, lopart$, hipart$,         ~
                                                               errormsg$)
            return

L50640: REM Test for CDA ACTIVITY                 SEL_WC$
            call "TESTRNGE" (fmact$, toact$, loact$, hiact$, errormsg$)
            return

L50680: REM Test for Audit Status                S$()
            for j% = 1% to 6%
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
        ~#################################                 ########:CDA003

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
L60270: %                         ---------------------------------------~
        ~-----------------------------------------------
L60290: %                                  * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   * * * * * * * * * *

*       * Detail Report Headings
L60330: %                   Trans   CDA    PC  Device   Job              ~
        ~               To Job   New Due Updte Update Message /

L60360: %  Date     Time   Number Activity ID    ID    Number  Store Lot ~
        ~Nr   Quantity  Number    Date   Stats Part Number (if required)

L60390: %-------- -------- ------ -------- --- ------ -------- ----- ----~
        ~-- ---------- -------- -------- ----- ---------------------------~
        ~---
L60420: %######## ######## ######    ##    ###   ###  ########  ###  ####~
        ~## ########## ######## ########   ##  ###########################~
        ~###
L60450: %                                                                ~
        ~                                      #########################

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
