        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   CCC   DDDD    AAA    QQQ   JJJJJ   OOO                  *~
            *  C   C  D   D  A   A  Q   Q    J    O   O                 *~
            *  C      D   D  AAAAA  Q   Q    J    O   O                 *~
            *  C   C  D   D  A   A  Q  QQ  J J    O   O                 *~
            *   CCC   DDDD   A   A   QQQQ   J      OOO                  *~
            *                            Q                              *~
            *-----------------------------------------------------------*~
            * CDAQJO   - This Program allows the user to review,        *~
            *            change, and report on WC to WC Movement Data   *~
            *            received via the CDA to CMS Interface. Trans-  *~
            *            actions are from the CDATOCMS update program.  *~
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
            * 09/15/94 ! Modify for new CDA Hardware/software with! RJH *~
            *          !   the elimination of 'OP' transactions.  !     *~
            * 05/16/95 ! Unstring modified summary string for TO  ! RJH *~
            *          !   WCs & STEPs.                           !     *~
            * 05/16/95 ! Added screen descriptors or Detail Screen! RJH *~
            *          !   for WCs, STEPs, & Strp Complete.       !     *~
            * 11/16/95 ! PRR 13501 - Allow To/From Step to CH(7). ! RJH *~
            * 04/23/96 ! Tweek CDAAUDJO Record Format.            ! RJH *~
            * 09/04/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**


        sub "CDAQJO" (jobin$)

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cursor%(2),                  /* Cursor location for edit   */~
            columnttl$30,                /* Range Description          */~
            company$60,                  /* Company or Division Name   */~
            date$8,                      /* Date for screen display    */~
            dept$4,                      /* Dept Number                */~
            dfac$(7)1,                   /* Field Attribute Characters */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            f_data$(14)40,               /* Data area for Print        */~
            f_desc$(7)15,                /* Desc area for Print        */~
            fmdate$10,                   /* Date Range                 */~
            fmjob$8,                     /* Job Number Range           */~
            fmtime$8,                    /* Time Range                 */~
            good_qty(12),                /* Good QTY for Display       */~
            hdr1$(3)30,                  /* Summary Display Headings 1 */~
            hdr2$(6)30,                  /* Summary Display Headings 2 */~
            hdr3$40,                     /* Detail Display Headings    */~
            hidate$10,                   /* Date Range                 */~
            hijob$8,                     /* Job Number Range           */~
            hitime$8,                    /* Time Range                 */~
            hold_summary$85,             /* Hold Summary - Update      */~
            i$(24)80,                    /* Screen Image               */~
            i1$(24)80,                   /* SCREEN IMAGE               */~
            inpmessage$79,               /* Informational Message      */~
            job$8, jobin$8,              /* Job Number                 */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lodate$10,                   /* Date Range                 */~
            lojob$8,                     /* Job Number Range           */~
            lotime$8,                    /* Time Range                 */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            print$(3)10,                 /* Print Work area for totals */~
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
            rrdate$8, rrtime$,           /* Date, Time Trans received  */~
            rsource$1,                   /* Source                     */~
            rstatus$1,                   /* Status                     */~
            rtime$6,                     /* Time                       */~
            rtran$2,                     /* Tran Set                   */~
            rtrnr$6,                     /* Trans Number               */~
            rtype$1,                     /* Type                       */~
            rvalid$4,                    /* Fields Validated           */~
            rwk_qty(12),                 /* REWORK QTY FOR PRINT       */~
            s$(6)1,                      /* Update Status              */~
            scr_qty(12),                 /* SCRAP QTY FOR PRINT        */~
            stv$(6)2,                    /* Actual Value of Status     */~
            sel_status$2,                /* Update Status              */~
            sel_step$7,                  /* Step                       */~
            sel_wc$4,                    /* Work Center                */~
            sfac$(12)1, summary$(12)85,  /* Summary Display and FACs   */~
            sumkey$(12)19,               /* Readkey for Each Sumry item*/~
            sumkey2$(12)19,              /* READKEY FOR EACH SUMRY ITEM*/~
            todate$10,                   /* Date Range                 */~
            tojob$8,                     /* Job Number Range           */~
            totime$8,                    /* Time Range                 */~
            traction$2,                  /* ACTION                     */~
            trdate$8,                    /* Date/Time                  */~
            trdvc_id$3,                  /* DEVICE ID                  */~
            trgood_qty$10,               /* SCRAP QTY                  */~
            trjob$8,                     /* Job Number                 */~
            trkey1$19,                   /* Transaction First Half Key */~
            trkey2$19,                   /* Transaction Second half ky */~
            trpc_id$3,                   /* PC ID                      */~
            trrework_qty$10,             /* REWORK QTY                 */~
            trscrap_qty$10,              /* SCRAP QTY                  */~
            trsrce$,                     /* SOURCE                     */~
            trstatus$2,                  /* STATUS                     */~
            trstep_from$7,               /* STEP FROM                  */~
            trstep_to$7,                 /* STEP TO                    */~
            trtime$8,                    /* TIME                       */~
            trupdmsg$30,                 /* UPDATE MESSAGE             */~
            trwc_from$4,                 /* WORK CENTER                */~
            trwc_to$4,                   /* WORK CENTER TO             */~
            update_msg$30,               /* Update Message             */~
            updmsg$(12)30,               /* Update Message             */~
            userid$3,                    /* Current User Id            */~
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
            * #02 ! CDAAUDJO ! Audit file for 'JO' Transactions (WC Mov *~
            * #03 ! CDAHOLD  ! Holding file for first half of 2 part CD *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "CDARECVE",                                      ~
                        varc,     indexed,  recsize =  768,              ~
                        keypos =    2, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  20          ~

            select #02, "CDAAUDJO",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   11, keylen =  19

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

            str(line2$,62) = "  CDAQJO: " & str(cms2v$,,8)

            stv$() = "P H W WRE ER"

            msg_dsp% = 1%

*        Set up Display Variables and Descriptor Tables
            hdr1$(1) = "---From----                   "
            hdr1$(2) = "  ---To----                   "
            hdr1$(3) = "      Qty      Qty    Qty"

            hdr2$(1) = "    Date/Time                 "
            hdr2$(2) = "  Job #                       "
            hdr2$(3) = "S         Vendor Name         "
            hdr2$(4) = "Step   WC                     "
            hdr2$(5) = "Step   WC                     "
            hdr2$(6) = "     Good    Scrap Rework"
            hdr3a$   = "Field Desc"
            hdr3$    = "Data Field Contents "

*        Check incomming arguments...
            if jobin$ = " " then L10000
            if jobin$ = last_jobin$ then L12690

            fmjob$, tojob$, last_jobin$ = jobin$
            fmdate$, fmtime$    = "ALL"
            s$() = all("X")
            todate$, totime$, tojob$, sel_step$, sel_wc$ = " "
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
L11140:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  6% then editpg1
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

            goto L12700

*        Get Next Record for Validation
L12180:     call "PLOWNEXT" (#2, plowkey$, 0%, f1%(2))
                if f1%(2) = 0 then L12700

*        Setup key to select DATE
L12220:     wkdate$ = str(plowkey$,1,6)
            if wkdate$ > hidate$ then L12180
            str(plowkey$,7) = all(hex(00))
            str(plowkey$,7,6) = lotime$
L12260:     call "PLOWNEXT" (#2, plowkey$, 6%, f1%(2))
                if f1%(2) = 1% then L12310
            goto L12180

*        Setup key to select TIME
L12310:     wktime$ = str(plowkey$,7,6)
            if wktime$ > hitime$ then L12260

*        Get Values from record for continued validation
            get #2 using L12370, trstatus$, trjob$, tr_step$, tr_wc$
L12370:        FMT          /* FILE: CDAAUDJO                          */~
                  CH(2),    /* Status                                  */~
                  CH(8),    /* Job Number                              */~
                  POS(51),  /*                                         */~
                  CH(7),    /* From Step                               */~
                  CH(4)     /* From Work Center                        */~

*        Check if JOB is within JOB range
            if trjob$ < lojob$ or trjob$ > hijob$ then L12260

*        Check if Status is within Selection
            for s% = 1% to len(s$())
               if s$(s%) <> "X" then L12510
               if str(stv$(s%)) = trstatus$ then L12550
L12510:     next s%
            goto L12260

*        Check if WC is within selection
L12550:     if sel_wc$ = "ALL " then L12600
            if tr_wc$ = sel_wc$ then L12600
                goto L12260

*        Check if STEP is within selection
L12600:     if sel_step$ = "ALL " then L12650
            if tr_step$ = sel_step$ then L12650
                goto L12260

*        Record is within selection criteria
L12650:     dl% = dl% + 1%
            gosub load_audit
            if dl% < 12% then goto L12260

L12690
*        Check for records at end of file
L12700:     if dl% <> 0% then L12790
                errormsg$ = "No Records found within the Specified " &   ~
                            "Selection Criteria"
                goto editpg1
L12790:     lastfieldnr% = 0%
L12800:     gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                errormsg$ = " "
                if keyhit%  =  1% then gosub startover
                if keyhit%  =  2% then goto start_screen2
                if keyhit% <>  5% then       L12890
                    if dl% <> 12% then goto L12260
                      gosub init_screen2
                      dl% = 0
                      goto L12260
L12890:         if keyhit% <> 11% then L12920
                    msg_dsp% = mod(msg_dsp%, 2) + 1
                    goto L12800
L12920:         if keyhit%  = 14% then gosub report_or_purge
                if keyhit%  = 28% then gosub report_or_purge
                if keyhit%  = 16% then editpg1
              fieldnr% = cursor%(1%) - 6%
                if fieldnr% < 1% or fieldnr% > dl% then L12800
                if fieldnr% = lastfieldnr% then    L12800
                if keyhit%  =  0% then       display_raw_data
                if keyhit%  =  8% then gosub mark_resolved
                if keyhit%  =  9% then gosub unmark_resolved
                if keyhit% <> 10% then L13040
                    wk% = fieldnr%
                    gosub update_record
L13040:         if keyhit%  = 12% then gosub delete_record
              goto L12800

            next s%
            goto L12800

        display_raw_data
            wk% = fieldnr%
            if sumkey$(wk%) = " " then L12800
            gosub init_screen3
            readkey$ = sumkey$(wk%)
            f_desc$(2) = " 2) Employee  "
            f_desc$(3) = " 3) Job Code  "
            f_desc$(4) = " 4) From Step "
            f_desc$(5) = " 5) Good Qty  "
            f_desc$(6) = " 6) Scrap Qty "
            f_desc$(7) = " 7) Rework Qty"
            gosub load_screen3
*          IF SUMKEY2$(WK%) <> " " THEN 13250
            inpmessage$ = "Press RETURN to see Summary Screen"
*              GOTO 13260
*          INPMESSAGE$ = "Press RETURN to see 'OP' Transaction Detail"
            lastfieldnr% = 0%
            gosub'103(0%, 1%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% then       editpg1
                  if keyhit%  = 16% then       L12800
                  if keyhit%  =  0% then       L12800
*                IF KEYHIT%  =  0% THEN       DISPLAY_OP_RAW_DATA
            goto L12800

*       DISPLAY_OP_RAW_DATA
*          IF SUMKEY2$(WK%) = " " THEN 12800
*          GOSUB INIT_SCREEN3
*          READKEY$ = SUMKEY2$(WK%)
*          F_DESC$(2) = " 2) To Step  "
*          F_DESC$(3) = " 3) To WC    "
*          F_DESC$(4) = " 4)          "
*          F_DESC$(5) = " 5)          "
*          F_DESC$(6) = " 6)          "
*          F_DESC$(7) = " 7)          "
*          GOSUB LOAD_SCREEN3
*          INPMESSAGE$ = "Press RETURN to see Summary Screen   "
*          LASTFIELDNR% = 0%
*          GOSUB'103(0%, 2%)           /* Display Screen - No Entry   */
*                IF KEYHIT%  =  1% THEN GOSUB STARTOVER
*                IF KEYHIT%  =  4% THEN       EDITPG1
*                IF KEYHIT%  = 16% THEN       12800
*                IF KEYHIT% <>  0% THEN       13470
            goto L12800

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
L14110:        FMT CH(2)
            if trstatus$ = "E " or trstatus$ = "W " then L14160
              errormsg$ =                                                ~
               "Only Status 'E ' and 'W ' records can be Marked Resolved"
              return
L14160:     str(trstatus$,2,1) = "R"
            str(summary$(fieldnr%),28,2) = trstatus$
            put #2 using L14110, trstatus$
            rewrite #2
            return


        unmark_resolved
            readkey$ = sumkey$(fieldnr%)
            call "READ101" (#2, readkey$, f1%(2))
            if f1%(2) = 0% then return
            get #2 using L14270, trstatus$
L14270:        FMT CH(2)
            if trstatus$ = "ER" or trstatus$ = "WR" then L14320
              errormsg$ =                                                ~
               "Only Status 'ER' and 'WR' records can be Unmarked"
              return
L14320:     str(trstatus$,2,1) = " "
            str(summary$(fieldnr%),28,2) = trstatus$
            put #2 using L14270, trstatus$
            rewrite #2
            return


        update_record
*        Verify Status is 'E', only Status 'E' records can be updated
            wk% = fieldnr%
            if str(summary$(wk%),28,1) = "E" then L14460
               errormsg$ = "Only Status type 'E' records can be Modified"
               goto L15360

L14460
*        Move Summary field to Hold
            hold_summary$ = summary$(wk%)

*        Get ERROR record from CDARECVE
            readkey$ = sumkey$(wk%)
            call "READ100" (#1, readkey$, f1%(1))
                if f1%(1) = 0 then L14650
            get #1 using L32360, rstatus$, rdate$, rtime$, rpcid$,        ~
                  rdvcid$, rsource$, rtype$, raction$, rovride$, rbase$, ~
                  rcurr$, rtran$, rvalid$, rerror$, f_data$(),           ~
                  rmessage$, rdept$, rchange$, rchng_date$

            call "DATEFMT" (rdate$)

            if rtran$ = "JO" then L14640
               errormsg$ = "This transaction is not modifiable: " &      ~
                           " First half of transaction is missing."
               goto L15360

*        Redisplay screen with selected line enabled for Update
L14640:     lastfieldnr% = 0%
L14650:     gosub'104(fieldnr%, 2%)     /* Display Screen - No Entry   */
                if keyhit%  <> 1% then L14690
                    summary$(fieldnr%) = hold_summary$
                    return
L14690:         if keyhit%  <> 16% then goto L14650

*        Unstring Summary field for changes
            f_data$(3) = str(summary$(wk%),19,8)  /* JOB       */
            f_data$(4) = str(summary$(wk%),30,7)  /* FROM STEP */
            if msg_dsp% <> 1% then L14820
                if traction$ = "02" then L14800
                    f_data$(5) = str(summary$(wk%),54, 9)  /* GOOD QTY */
L14800:         f_data$(6) = str(summary$(wk%),64,8)      /* SCRAP QTY */
                f_data$(7) = str(summary$(wk%),72,7)     /* REWORK QTY */
                f_data$(12%) = str(summary$(wk%),42,7)   /* TO STEP  */
                f_data$(13%) = str(summary$(wk%),49,4)   /* TO WC    */
L14820:     rchng_date$ = date
            rchange$    = userid$

*        ADD CDARECVE "JO" RECORD
            call "READ101" (#1, readkey$, f1%(1))

            call "DATUNFMT" (rdate$)

            put #1 using L32360, "N", rdate$, rtime$, rpcid$,             ~
                  rdvcid$, rsource$, rtype$, raction$, rovride$, rbase$, ~
                  rcurr$, rtran$, rvalid$, rerror$, f_data$(),           ~
                  rmessage$, rdept$, rchange$, rchng_date$
            rewrite #1
            str(summary$(wk%),28,1) = "N"

            call "DELETE" (#2, readkey$, 19%)
            goto L15340

*        Start the Update
L15340:     call "TASKUP" ("CD", u3%)
L15360:     return


        delete_record
            u3% = 2%
            call "ASKUSER" (u3%, "DELETE CDAAUDJO",                      ~
                            "Press PF-1 to cancel delete", "-OR-",       ~
                            "Press RETURN to DELETE Audit Record.")
            if u3% <> 0% then return
*        Delete CDAAUDJO
            readkey$ = sumkey$(fieldnr%)
            call "READ100" (#2, readkey$, f1%(2))
                if f1%(2) <> 1% then L15620
            get #2 using L15500, trstatus$, trkey1$, trkey2$
L15500:        FMT CH(2), POS(11), CH(19), POS(32), CH(19)
            call "DELETE" (#2, readkey$, 19%)
*        Delete second half if it exists
            if trkey2$ = " " then L15560
                call "DELETE" (#1, trkey2$, 19%)
*        Check if Error record exists if so Delete
L15560:     if str(trstatus$,1,1) <> "E" then L15590
                call "DELETE" (#1, trkey1$, 19%)
*        Check if Hold Record exists if so Delete CDAHOLD and CDARECVE
L15590:     if trstatus$ <> "H " then L15620
                call "DELETE" (#1, trkey1$, 19%)
                call "DELETE" (#3, trkey1$, 19%)
L15620:     return

        REM *************************************************************~
            *          P R I N T / P U R G E   R O U T I N E            *~
            *-----------------------------------------------------------*~
            * Print or Purge Records Per Selection Criteria             *~
            *************************************************************

        report_or_purge
            gosub init_report
            if keyhit% = 14% then gosub generate_report
            if keyhit% <> 28% then L16160
                u3% = 2%
                call "ASKUSER" (u3%, "PURGE CDAAUDJO",                   ~
                     "Note: This will Purge 'ALL' records within your",  ~
                     "Selection Criteria, Press PF-1 to CANCEL Purge",   ~
                     "-OR-, Press RETURN to Purge Audit Records.")
                if u3% <> 0% then return
L16160:     plowkey$ = all(hex(00))
            str(plowkey$,1,6) = lodate$
            call "PLOWNEXT" (#2, plowkey$, 0%, f1%(2))
                if f1%(2) = 1 then L16290

            errormsg$ = "No CDA Audit Records type (JO) to Process"
            goto L11080

*        Get Next Record for Validation
L16250:     call "PLOWNEXT" (#2, plowkey$, 0%, f1%(2))
                if f1%(2) = 0 then L16810

*        Setup key to select DATE
L16290:     wkdate$ = str(plowkey$,1,6)
            if wkdate$ > hidate$ then L16250
            str(plowkey$,7) = all(hex(00))
            str(plowkey$,7,6) = lotime$
L16330:     call "PLOWNEXT" (#2, plowkey$, 6%, f1%(2))
                if f1%(2) = 1% then L16380
            goto L16250

*        Setup key to select TIME
L16380:     wktime$ = str(plowkey$,7,6)
            if wktime$ > hitime$ then L16330

*        Get Values from record for continued validation
            get #2 using L16450, trstatus$, trjob$, trkey2$, tr_step$,    ~
                  tr_wc$
L16450:        FMT          /* FILE: CDAAUDJO                          */~
                  CH(2),    /* Status                                  */~
                  CH(8),    /* Job Number                              */~
                  POS(32),  /*                                         */~
                  CH(19),   /* Second Half key                         */~
                  POS(51),  /*                                         */~
                  CH(7),    /* From Step                               */~
                  CH(4)     /* From Work Center                        */~

*        Check if JOB is within JOB range
            if trjob$ < lojob$ or trjob$ > hijob$ then L16330

*        Check if Status is within Selection
            for s% = 1% to len(s$())
               if s$(s%) <> "X" then L16610
               if str(stv$(s%)) = trstatus$ then L16650
L16610:     next s%
            goto L16330

*        Check if WC is within selection
L16650:     if sel_wc$ = "ALL " then L16700
            if tr_wc$ = sel_wc$ then L16700
                goto L16330

*        Check if STEP is within selection
L16700:     if sel_step$ = "ALL " then L16760
            if tr_step$ = sel_step$ then L16760
                goto L16330

*        Record is Valid for Report or Purge
*        Determine whether we are Reporting or Purging
L16760:     if keyhit% = 14% then gosub print_lines
            if keyhit% = 28% then gosub purge_rtn
            goto L16330

*        Print End of Report if Producing Report
L16810:     if keyhit% = 14% then gosub end_report
            if keyhit% = 14% then return else editpg1

        REM *************************************************************~
            *         P U R G E   R E C O R D   S E L E C T E D         *~
            *-----------------------------------------------------------*~
            * Purges Selected Records                                   *~
            *************************************************************

        purge_rtn
            call "DELETE" (#2, plowkey$, 19%)
            if str(trstatus$,1,1) <> "E" then L18120
                call "DELETE" (#1, plowkey$, 19%)
            if trkey2$ = " " then L18120
                call "DELETE" (#1, trkey2$, 19%)
L18120:     return

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
L20150: REM Def/Enable Date Range                  FMDATE$
            if fmdate$             = " " or              ~
               fmdate$             = blankdate$ then     ~
               fmdate$             = "ALL"
            return

L20200: REM Def/Enable Time Range                  FMTIME$
            if fmtime$             = " " then                            ~
               fmtime$             = "ALL"
            return

L20250: REM Def/Enable Job Number Range            FMJOB$
            if fmjob$              = " " then                            ~
               fmjob$              = "ALL"
            return

L20300: REM Def/Enable Step                        SEL_STEP$
            if sel_step$         = " " then                              ~
               sel_step$         = "ALL"
            return

L20350: REM Def/Enable Work Center                 SEL_WC$
            if sel_wc$           = " " then                              ~
               sel_wc$           = "ALL"
            return

L20400: REM Def/Enable Update Status               SEL_STATUS$
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
         "Enter Date Range                                             ",~
         "Enter Time Range                                             ",~
         "Enter Job Number Range                                       ",~
         "Enter Step                                                   ",~
         "Enter Work Center                                            ",~
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
            init(" ") errormsg$, inpmessage$,                            ~
                      trgood_qty$, job$, trrework_qty$, trscrap_qty$,    ~
                      trstatus$, trstep_from$, trstep_to$, trdate$,      ~
                      update_msg$, trwc_from$, trwc_to$, updmsg$(),      ~
                      fmdate$, fmjob$, sel_status$,                      ~
                      sel_step$, sel_wc$, fmtime$, hidate$, hijob$,      ~
                      hitime$, lodate$, lojob$, lotime$, todate$,        ~
                      tojob$, totime$, s$()

            return

        init_screen2
            init (" ") summary$(), sumkey$(), updmsg$(), errormsg$,      ~
                     inpmessage$, sumkey2$()
            dl%, wk% = 0
            mat good_qty = zer : mat scr_qty = zer : mat rwk_qty = zer
            return

        init_report
        REM GOSUB INITIALIZE_VARIABLES
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
            get #2 using L32060, trstatus$, trjob$, trdate$, trtime$,     ~
                trpc_id$, trdvc_id$, trsrce$, traction$, trkey2$,        ~
                trstep_from$, trwc_from$, trgood_qty, trscrap_qty,       ~
                trrework_qty, trstep_to$, trwc_to$, trupdmsg$,           ~
                trsetup_hour$, trsetup_unit$, trrun_hour$, trrun_unit$
            good_qty(dl%)   = trgood_qty
            scr_qty(dl%)    = trscrap_qty
            rwk_qty(dl%)    = trrework_qty
            updmsg$(dl%)    = trupdmsg$
            return

        load_audit
*        Load Data from Audit file to Summary area for display
            gosub dataload
            sumkey$(dl%) = plowkey$
            sumkey2$(dl%) = trkey2$
            call "DATEOK" (trdate$, trdate%, errormsg$)
            str(summary$(dl%),,8%) = trdate$
            call "TIMEOK" (trtime$, trtime, errormsg$)
            str(summary$(dl%), 10%) = trtime$
            str(summary$(dl%), 19%) = trjob$
            str(summary$(dl%), 28%) = trstatus$
            str(summary$(dl%), 30%) = trstep_from$
            str(summary$(dl%), 37%) = trwc_from$
            str(summary$(dl%), 42%) = trstep_to$
            str(summary$(dl%), 49%) = trwc_to$
            return

        load_msg_amount
            for dl% = 1% to 12%
              if sumkey$(dl%) = " " then return
              str(summary$(dl%),54,26) = " "
              if msg_dsp% = 1% then L30410
                  str(summary$(dl%), 54%) = updmsg$(dl%)
                  goto L30500
L30410:       if good_qty(dl%) = 0 then L30440
              call "CONVERT"                                             ~
                       (good_qty(dl%), 2.2, str(summary$(dl%),54%,9%))
L30440:       if scr_qty(dl%) = 0 then L30470
              call "CONVERT"                                             ~
                       (scr_qty(dl%), 1.1, str(summary$(dl%),64%,8%))
L30470:       if rwk_qty(dl%) = 0 then L30500
              call "CONVERT"                                             ~
                       (rwk_qty(dl%), 1.1, str(summary$(dl%),72,7))
L30500:     next dl%
            return

        load_screen3
            call "READ100" (#1, readkey$, f1%(1))
                if f1%(1%) = 0 then return
            get #1 using L32360, rstatus$, rdate$, rtime$, rpcid$,        ~
                rdvcid$, rsource$, rtype$, raction$, rovride$, rbase$,   ~
                rcurr$, rtran$, rvalid$, rerror$, f_data$(),             ~
                rmessage$, rdept$, rchange$, rchng_date$,                ~
                rrdate$, rrtime$, rtrnr%
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

L32060: FMT                 /* FILE: CDAAUDJO                          */~
            CH(2),          /* Audit Status                            */~
            CH(8),          /* Job Number                              */~
            CH(6),          /* Key1 Date                               */~
            CH(6),          /* Key1 Time                               */~
            CH(3),          /* Key1 PC ID                              */~
            CH(3),          /* Key1 Device Id                          */~
            CH(1),          /* Source                                  */~
            CH(2),          /* Code representing action selected or tak*/~
            CH(19),         /* Primary key of transaction              */~
            CH(7),          /* From Step                               */~
            CH(4),          /* From Work Center                        */~
            PD(14,4),       /* Quantity Good                           */~
            PD(14,4),       /* Qty Scrap                               */~
            PD(14,4),       /* Qty Rework                              */~
            CH(7),          /* To Step                                 */~
            CH(4),          /* To Work Center                          */~
            CH(30),         /* Update Message                          */~
            XX(4),          /* Transaction Sequence Number             */~
            CH(7),          /* Setup Work Center Hours                 */~
            CH(5),          /* Setup Work Center Units                 */~
            CH(7),          /* Run   Work Center Hours                 */~
            CH(5),          /* Run   Work Center Units                 */~
            XX(1),          /* Step Complete Flag                      */~
            CH(101)         /* Unused Space                            */

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
        generate_report
            call "SHOSTAT" ("Report Generation in Progress")
            call "COMPNAME" (12%, company$, 0%)
            rpttitle$ = "CDA TO CMS AUDIT REPORT TYPE (JO)"
            call "FMTTITLE" (rpttitle$, " ", 12%)
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("CDA001", " ", 0%, 0%)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
            return

*       * Report Generation Logic goes here
        print_lines
            get #2 using L32060, trstatus$, trjob$, trdate$, trtime$,     ~
                trpc_id$, trdvc_id$, trsrce$, traction$, trkey2$,        ~
                trstep_from$, trwc_from$, trgood_qty, trscrap_qty,       ~
                trrework_qty, trstep_to$, trwc_to$, trupdmsg$,           ~
                trsetup_hour$, trsetup_unit$, trrun_hour$, trrun_unit$
            call "READ100" (#1, plowkey$, f1%(1))
                if f1%(1) = 0 then L35270
            get #1 using L35250, rempl$, rchange$, rchng_date$
L35250:        FMT POS(81), CH(12), POS(635), CH(3), CH(6)

L35270:     call "DATEOK" (trdate$, trdate%, errormsg$)
            call "TIMEOK" (trtime$, trtime, errormsg$)
            call "DATEFMT" (rchng_date$)
            call "CONVERT" (trgood_qty, 2.2, print$(1))
            call "CONVERT" (trscrap_qty, 2.2, print$(2))
            call "CONVERT" (trrework_qty, 2.2, print$(3))

            if trsrce$ = " " then trsrce$ = "C"

            if lcntr% > 56% then gosub page_head
            print using L64120, trdate$, trtime$, trjob$, trstatus$,      ~
                 trpc_id$, trsrce$, rempl$, trstep_from$, trwc_from$,    ~
                 print$(1), print$(2), print$(3), trupdmsg$
            print using L64150, traction$, trdvc_id$, rchange$,           ~
                 rchng_date$, trstep_to$, trwc_to$,                      ~
                 trsetup_hour$, trsetup_unit$, trrun_hour$, trrun_unit$
            return

        end_report                /* Report Ending Routine */
            if pcntr% <> -1 then L35540
                errormsg$ = "No Records found within the Specified " &   ~
                            "Selection Criteria"
                keyhit% = 2%
                call "ASKUSER" (keyhit%, "****",                         ~
                    "SORRY, no records for Reporting were found",        ~
                    "using the requested selection criteria.",           ~
                    "Press RETURN to change your selection or to exit.")
                goto editpg1
L35540:     print skip(2)
            print using L64000     /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            return

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            if pcntr% = 0% then gosub print_params
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "  CDAQJO"
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
            print using L60210, "Transaction Dates   ", fmdate$  , todate$
            print using L60210, "Transaction Times   ", fmtime$  , totime$
            print using L60210, "Job Numbers         ", fmjob$   , tojob$
            print using L60210, "Selected Step       ", sel_step$, " "
            print using L60210, "Selected Work Center", sel_wc$  , " "
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
            str(line2$,,44) = "WC to WC Movement (JO) - Selection " &    ~
                              "Criteria"
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            on fieldnr% gosub L40220,         /* Date Range        */     ~
                              L40220,         /* Time Range        */     ~
                              L40220,         /* Job Number Range  */     ~
                              L40220,         /* Step              */     ~
                              L40220,         /* Work Center       */     ~
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
               at (06,22), fac(hex(ac)),   columnttl$           , ch(30),~
                                                                         ~
               at (07,02), "Date Range",                                 ~
               at (07,22), fac(lfac$( 1)), fmdate$              , ch(10),~
               at (07,37), fac(lfac$( 1)), todate$              , ch(10),~
                                                                         ~
               at (08,02), "Time Range",                                 ~
               at (08,22), fac(lfac$( 2)), fmtime$              , ch(08),~
               at (08,37), fac(lfac$( 2)), totime$              , ch(08),~
                                                                         ~
               at (09,02), "Job Number Range",                           ~
               at (09,22), fac(lfac$( 3)), fmjob$               , ch(08),~
               at (09,37), fac(lfac$( 3)), tojob$               , ch(08),~
                                                                         ~
               at (10,02), "Step",                                       ~
               at (10,22), fac(lfac$( 4)), sel_step$            , ch(07),~
                                                                         ~
               at (11,02), "Work Center",                                ~
               at (11,22), fac(lfac$( 5)), sel_wc$              , ch(04),~
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
                  call "MANUAL" ("CDAQJO") : goto L40250

L40780:        if keyhit% <> 15 then L40810
                  call "PRNTSCRN" : goto L40250

L40810:        close ws
               call "SCREEN" addr ("C", u3%, "I", i1$(), cursor%())
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
            gosub load_msg_amount
            str(line2$,,44) = "Review WC to WC Movement (JO) - Summary"
            if msg_dsp% = 1% then L42150
                hdr1$(3) = " "
                hdr2$(6) = "Audit Update Message  "
                goto L42180
L42150:     hdr1$(3) = "      Qty      Qty    Qty"
            hdr2$(6) = "     Good    Scrap Rework"

L42180:     init(hex(8c)) sfac$()

L42340:     accept                                                       ~
               at (01,02),                                               ~
                  "CDA to CMS Audit and Report Functions",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,31), fac(hex(8c)), hdr1$(1)               , ch(09),~
               at (05,42), fac(hex(8c)), hdr1$(2)               , ch(09),~
               at (05,55), fac(hex(8c)), hdr1$(3)               , ch(25),~
                                                                         ~
               at (06,02), fac(hex(ac)), hdr2$(1)               , ch(17),~
               at (06,20), fac(hex(ac)), hdr2$(2)               , ch(08),~
               at (06,29), fac(hex(ac)), hdr2$(3)               , ch(01),~
               at (06,31), fac(hex(ac)), hdr2$(4)               , ch(11),~
               at (06,43), fac(hex(ac)), hdr2$(5)               , ch(11),~
               at (06,55), fac(hex(ac)), hdr2$(6)               , ch(25),~
         /*    AT (07,02), FAC(HEX(80))  , TEMP$,    Pos Cursor        */~
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

               if keyhit% <> 13 then L42770
                  call "MANUAL" ("CDAQJO") : goto L42340

L42770:        if keyhit% <> 15 then L42800
                  call "PRNTSCRN" : goto L42340

L42800:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L42990     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L42950
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L42950:     if fieldnr% > 2% then L42970
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L42970:     return

L42990: if fieldnr% > 0% then L43100  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over         (8)Mark As Resolve" &        ~
                     "d    (11)Dsply Msg    (13)Instructions "
            pf$(2) = "(2)First              (9)Unmark Resolved" &        ~
                     "  (12/28)Delete/Purge (15)Print Screen "
            pf$(3) = "(5)Next Screen       (10)Modify & Resubm" &        ~
                     "it   (14)Print Report (16)Select Screen"
            pfkeys$ = hex(0102ffff05ffff08090a0b0c0d0e0f10001c)
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
            str(line2$,,44) = "Review WC to WC Movement (JO) - Detail"
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
               at (12,23), " 8) Setup Hours",                            ~
               at (12,40), fac(lfac$( 8)), f_data$(8)           , ch(40),~
                                                                         ~
               at (13,02), "Curr Prog",                                  ~
               at (13,13), fac(lfac$( 9)), rcurr$               , ch(03),~
               at (13,23), " 9) Setup Units",                            ~
               at (13,40), fac(lfac$( 9)), f_data$(9)           , ch(40),~
                                                                         ~
               at (14,02), "Tran Set",                                   ~
               at (14,13), fac(lfac$(10)), rtran$               , ch(02),~
               at (14,23), "10) Run Hours",                              ~
               at (14,40), fac(lfac$(10)), f_data$(10)          , ch(40),~
                                                                         ~
               at (15,02), "Activity",                                   ~
               at (15,13), fac(lfac$(11)), raction$             , ch(02),~
               at (15,23), "11) Run Units",                              ~
               at (15,40), fac(lfac$(11)), f_data$(11)          , ch(40),~
                                                                         ~
               at (16,02), "Changed By",                                 ~
               at (16,13), fac(lfac$(12)), rchange$             , ch(03),~
               at (16,23), "12) To Step  ",                              ~
               at (16,40), fac(lfac$(12)), f_data$(12)          , ch(40),~
                                                                         ~
               at (17,02), "        On",                                 ~
               at (17,13), fac(lfac$(13)), rchng_date$          , ch(08),~
               at (17,23), "13) To WC    ",                              ~
               at (17,40), fac(lfac$(13)), f_data$(13)          , ch(40),~
                                                                         ~
               at (18,02), "Trans Nmbr",                                 ~
               at (18,13), fac(lfac$(14)), rtrnr$               , ch(06),~
               at (18,23), "14) Step Cmplt",                             ~
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

               if keyhit% <> 13 then L45060
                  call "MANUAL" ("CDAQJO") : goto L44130

L45060:        if keyhit% <> 15 then L45090
                  call "PRNTSCRN" : goto L44130

L45090:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf3
        if fieldnr% > 0% then L45420  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                     (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                     (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                     (16)Summary Screen"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            return
L45420:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *               U P D A T E   S C R E E N   2               *~
            *-----------------------------------------------------------*~
            * Update Screen                                             *~
            *************************************************************

        deffn'104(fieldnr%, edit%)
            gosub set_pf4
            str(line2$,,44) = "Modify WC to WC Movement (JO)"
            inpmessage$ = "Press PF1 to CANCEL Update, or " &            ~
                          "PF-16 to RESUBMIT transaction."
            if msg_dsp% = 1% then L46230
              str(summary$(wk%),54,26) = " "
              if good_qty(wk%) = 0 then L46160
                call "CONVERT"                                           ~
                         (good_qty(wk%), 2.2, str(summary$(wk%),54,9))
L46160:       if scr_qty(wk%) = 0 then L46190
                call "CONVERT"                                           ~
                         (scr_qty(wk%), 2.2, str(summary$(wk%),64,8))
L46190:       if rwk_qty(wk%) = 0 then L46230
                call "CONVERT"                                           ~
                         (rwk_qty(wk%), 2.2, str(summary$(wk%),72,7))

L46230:     accept                                                       ~
               at (01,02),                                               ~
                  "CDA to CMS Audit and Report Functions",               ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Date",                                       ~
               at (06,14), fac(hex(8c)), str(summary$(wk%),,8)  , ch(08),~
               at (07,02), "Time",                                       ~
               at (07,14), fac(hex(8c)), str(summary$(wk%),10,8), ch(17),~
               at (08,02), "Job",                                        ~
               at (08,14), fac(hex(80)), str(summary$(wk%),19,8), ch(08),~
               at (09,02), "Status",                                     ~
               at (09,14), fac(hex(8c)), str(summary$(wk%),28,2), ch(02),~
               at (10,02), "From Step",                                  ~
               at (10,14), fac(hex(80)), str(summary$(wk%),30,7), ch(07),~
               at (11,02), "From WC",                                    ~
               at (11,14), fac(hex(8c)), str(summary$(wk%),37,4), ch(04),~
               at (12,02), "To Step",                                    ~
               at (12,14), fac(hex(80)), str(summary$(wk%),42,7), ch(07),~
               at (13,02), "To WC",                                      ~
               at (13,14), fac(hex(80)), str(summary$(wk%),49,4), ch(04),~
               at (14,02), "Good Qty",                                   ~
               at (14,14), fac(hex(82)), str(summary$(wk%),54,9), ch(09),~
               at (15,02), "Scrap Qty",                                  ~
               at (15,14), fac(hex(82)), str(summary$(wk%),64,8), ch(08),~
               at (16,02), "Rework Qty",                                 ~
               at (16,14), fac(hex(82)), str(summary$(wk%),72,7), ch(07),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L46640
                  call "MANUAL" ("CDAQJO") : goto L46230

L46640:        if keyhit% <> 15 then L46670
                  call "PRNTSCRN" : goto L46230

L46670:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf4
        if edit% = 2% then L46860     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L46820
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L46820:     if fieldnr% > 2% then L46840
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L46840:     return

L46860: if fieldnr% > 0% then L46970  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over         (8)Mark As Resolve" &        ~
                     "d    (11)Dsply Msg    (13)Instructions "
            pf$(2) = "(2)First              (9)Unmark Resolved" &        ~
                     "  (12/28)Delete/Purge (15)Print Screen "
            pf$(3) = "(5)Next Screen       (10)Modify & Resubm" &        ~
                     "it   (14)Print Report (16)Select Screen"
            pfkeys$ = hex(0102ffff05ffff08090a0b0c0d0e0f10001c)
            if dl% = 12% then L46960
                str(pf$(3),1,14)  = " "  :  str(pfkeys$, 5,1) = hex(ff)
L46960:     return
L46970:                              /*  Edit Mode - Enabled    */
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
                              L50600,         /* Step                   */~
                              L50640,         /* Work Center            */~
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
            call "TESTRNGE"                                              ~
                  (fmjob$              , tojob$              ,           ~
                   lojob$              , hijob$              ,           ~
                   errormsg$)
            return

L50600: REM Test for Step                         SEL_STEP$
            if sel_step$ = " " then sel_step$ = "ALL"
            return

L50640: REM Test for Work Center                  SEL_WC$
            if sel_wc$ = " " then sel_wc$ = "ALL"
            return

L50680: REM Test for Audit Status                SEL_STATUS$
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
        ~#################################                 ########:CDA001

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
L64000: %                                  * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   * * * * * * * * * *

*       * Detail Report Headings
L64040: %                           STAT  PC/          EMPL/     FROM STE~
        ~P/WC GOOD QTY/  SCRAP QTY/ REWORK QTY UPDATE MESSAGE/

L64070: %    DATE/TIME     JOB NMBR /ACT DVC ID SRC  CHANGED BY  TO STEP ~
        ~/ WC SETUP HOUR SETUP UNIT RUN HOUR   RUN UNIT
L64090: %----------------- -------- ---- ------ --- ------------ --------~
        ~---- ---------- ---------- ---------- ---------------------------~
        ~---
L64120: %######## ######## ########  ##    ###   #  ############ ####### ~
        ~#### ########## ########## ########## ###########################~
        ~###
L64150: %                            ##    ###      ### ######## ####### ~
        ~####  #######     #####      #######     #####

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
