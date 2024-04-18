        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  V   V  EEEEE  N   N  RRR     AAA   TTTTT  N   N  GGGGG   *~
            *  V   V  E      NN  N  R   R  A   A    T    NN  N  G       *~
            *  V   V  EEEEE  N N N  RRRR   AAAAA    T    N N N  G GGG   *~
            *   V V   E      N  NN  R   R  A   A    T    N  NN  G  G    *~
            *    V    EEEEE  N   N  R   R  A   A    T    N   N  GGGG    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VENRATNG - VENDOR ANALYSIS REPORT BY VENDOR               *~
            *            (Analize Vendor's Past Performance)            *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1989  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/20/89 ! Original                                 ! SID *~
            * 05/08/91 ! Added VBKHLNES to read if no hit VBKLINES! JDH *~
            * 04/10/92 ! PRR 12155 - Correceted header vendor #   ! MLJ *~
            *          !   when summary only thing on new page.   !     *~
            * 02/06/95 ! Changed method of calculating vendor     ! WPH *~
            *          ! rating for delivery performance. Removed !     *~
            *          ! some 'Sid-isms'. Must have been his first!     *~
            *          ! program.                                 !     *~
            * 02/15/95 ! More general overhaul - print summary or ! WPH *~
            *          ! all, expanded/rewrote summary section.   !     *~
            * 10/17/95 ! Corrected some calulations.              ! JDH *~
            * 10/26/95 ! Now looks for the original due date.     ! JDH *~
            * 08/27/96 ! Millie date conversion                   ! DER *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            acceptqty$10,                /* Accepted quantity          */~
            acptweight$3,                /* Accepted Weight            */~
            blankdate$8,                 /* blank unfmt date           */~
            company$60,                  /* Company Name               */~
            acceptrec$10,                /* Accepted Rec Quantity      */~
            acceptnum$10,                /* Number of Accepted Receipts*/~
            acceptnumperc$5,             /* Percent of Receipts Acceptd*/~
            acceptrecvperc$5,            /* Percent of Rec Qty Accepted*/~
            acceptvalue$10,              /* Value of Accepted Receipts */~
            acceptvalperc$5,             /* Percent of Rec. Val accpted*/~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            days$3,                      /* Days late or early         */~
            delscore$10,                 /* Delivery Score             */~
            earlyrecv$10,                /* Early Receipt Quantity     */~
            earlynum$10,                 /* Number of Early Receipts   */~
            earlynumperc$5,              /* Percent of Receipts Early  */~
            earlyrecvperc$5,             /* Percent of Receipt qty Erly*/~
            earlyvalue$10,               /* Value of Early Receipts    */~
            earlyvalperc$5,              /* Percent of Receipt val Erly*/~
            duedate$6,                   /* PO LI Current Due Date     */~
            dueorig$6,                   /* PO LI Original Due Date    */~
            edays$3,                     /* Early Grace Days           */~
            epoints$1,                   /* Early Points               */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            evslweight$3,                /* Early VS Late Wt Factor    */~
            extension$10,                /* PO Line total ext. Value   */~
            fmvendor$9,                  /* Starting Vendor Code Range */~
            fmdate$10,                   /* Starting Reciept Date Range*/~
            hven$9,                      /* Value of VENDOR$           */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            ldays$3,                     /* Late Grace Days            */~
            lpoints$1,                   /* Late Points                */~
            laterecv$10,                 /* Late Receipt Quantity      */~
            latenum$10,                  /* Number of Late Receipts    */~
            latenumperc$5,               /* Percent of Receipts late   */~
            laterecvperc$5,              /* Percent of Receipt qty late*/~
            latevalue$10,                /* Value  of Late Receipts    */~
            latevalperc$5,               /* Percent of Receipt val late*/~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            openpokey$99,                /* For plowing                */~
            orderqty$10,                 /* PO Order Quantity          */~
            otrecv$10,                   /* On time receipt quantity   */~
            otrecvperc$5,                /* On time receipt quantity % */~
            otnum$10,                    /* Number of on-time receipts */~
            otnumperc$5,                 /* Number of on-time rec as % */~
            otvalue$10,                  /* Value  of on-time receipts */~
            otvalperc$5,                 /* Value  of on-time rec as % */~
            part$25,                     /* Part Number                */~
            pf4$18,                      /* PF Screen Literals         */~
            pf5$16,                      /* PF Screen Literals         */~
            pf16$16,                     /* PF Screen Literals         */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            points$2,                    /* Delivery Rating Points     */~
            ponum$16,                    /* Purchase Order Number      */~
            possible$10,                 /* Max possible del. points   */~
            prnt_id$16,                  /* Report Program ID          */~
            rcvnum$16,                   /* Receiver Number            */~
            recvdate$6,                  /* Date of last receipt       */~
            rejweight$3,                 /* Rejected Weight            */~
            rejectcode$6,                /* Rejection Code             */~
            rejquan$10,                  /* Rejected quantity          */~
            rejectrec$10,                /* Rejected Rec Quantity      */~
            rejectnum$10,                /* Number of Rejected Receipts*/~
            rejectnumperc$5,             /* Percent of Receipts Rejectd*/~
            rejectrecvperc$5,            /* Percent of Rec Qty Rejected*/~
            rejectvalue$10,              /* Value of Rejected Receipts */~
            rejectvalperc$5,             /* Percent of Rec. Val Rejcted*/~
            rpttitle$40,                 /* Report Title               */~
            seq$3,                       /* Line Sequence Number       */~
            status$9,                    /* "EARLY" of "LATE"          */~
            sumflag$1,                   /* Print Summary or All flag  */~
            tempvendor$9,                /* For Comparison             */~
            todate$10,                   /* Ending reciept date range  */~
            tovendor$9,                  /* Ending Vender Code Range   */~
            totlrecv$10,                 /* Total Received Quantity    */~
            totdpoints$10,               /* Total Delivery Points      */~
            totqpoints$10,               /* Total Quality  Points      */~
            totepoints$10,tottepoints$10,/* Total Early/Too Early Pts  */~
            totlpoints$10,tottlpoints$10,/* Total Late/Too Late Points */~
            totapoints$10,               /* Total Accepted Points      */~
            totrpoints$10,               /* Total Rejected Points      */~
            totopoints$10,               /* Total On-Time  Points      */~
            tlaterecv$10,                /* Quantity received too late */~
            tlaterecvperc$5,             /* Qty received too late as % */~
            tearlyrecv$10,               /* Quantity received too early*/~
            tearlyrecvperc$5,            /* Qty received too early as %*/~
            totalvalue$10,               /* Total Received Value       */~
            tlatevalue$10,               /* Value of received too late */~
            tlatevalperc$5,              /* Value of rec too late as % */~
            tearlyvalue$10,              /* Value of received too early*/~
            tearlyvalperc$5,             /* Value of rec too early as %*/~
            tearlynumperc$5,             /* Numbr of rec too early as %*/~
            tlatenumperc$5,              /* Numbr of rec too late as % */~
            totalqtyrec$10,              /* Total Received Quantity    */~
            totfound$10,                 /* Total Received Shipments   */~
            vendorrating$10,             /* Vendor Rating              */~
            unffmdate$10,                /* unfmt from date            */~
            unftodate$10,                /* unfmt to date              */~
            userid$3,                    /* Current User Id            */~
            vendor$9,                    /* Vendor Code                */~
            venname$30                   /* Vendor Name                */

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
            * #01 ! RCVLINES ! Receiver Line Items File  (Purchasing)   *~
            * #02 ! VBKLINES ! Purchase Order Line Items File           *~
            * #03 ! VENDOR   ! VENDOR MASTER RECORD                     *~
            * #04 ! VBKHLNES ! Purchase Order Line Items History File   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "RCVLINES",                                      ~
                        varc,     indexed,  recsize =  800,              ~
                        keypos =   26, keylen =  52,                     ~
                        alt key  1, keypos =    1, keylen =  69,         ~
                            key  2, keypos =   42, keylen =  36,         ~
                            key  3, keypos =  128, keylen =  24          ~

            select #02, "VBKLINES",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  28                      ~

            select #03, "VENDOR",                                        ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup     ~

            select #04, "VBKHLNES",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  28                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            *        Initializes information necessary for program.     *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            call "COMPNAME" (12%, company$, ret%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            rpttitle$  = " Vendor Analysis Report By Vendor "
            pf4$  = "(4)Previous Field"
            pf16$ = "(16)Exit"
            str(line2$,62,18) = "VENRATNG: " & str(cms2v$,1,8)


        REM *************************************************************~
            *        I N P U T  M O D E  M A I N  P R O G R A M         *~
            *-----------------------------------------------------------*~
            *      Handles input mode for range selection screen.       *~
            *************************************************************

        inputmode
            pf16$ = "(16)Exit"
            gosub initialize_variables
            for fieldnr% = 1% to 8%
                if fieldnr% = 1% then pf4$ = " " else pf4$ = "(4)Previous"
L10100:         gosub'051(fieldnr%)     /* Default / Enables         */
                if enabled% = 0 then L10210
L10110:         gosub'101(fieldnr%)     /* Display / Accept          */
                     if keyhit% =  1% then gosub startover
                     if keyhit% <> 4% then L10190
L10140:                 fieldnr% = max(1%, fieldnr% - 1%)
                        gosub'051(fieldnr%)
                        if enabled% = 1% then L10110
                        if fieldnr% = 1% then L10100
                        goto L10140
L10190:              if keyhit% = 16% then exit_program
                     if keyhit% <> 0  then L10110
L10210:         gosub'151(fieldnr%)     /* Edit field for valid entry */
                     if errormsg$ <> " " then L10110
            next fieldnr%


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            inpmessage$ = edtmessage$
            pf4$  = " "
            pf5$  = " "
            pf16$ = "(16)PRINT REPORT"
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       generate_report
                  if keyhit% <>  0% then       editpg1
            oldfield% = 0%
L11300:     fieldnr% = cursor%(1) - 6%
            if (fieldnr% < 1%) or (fieldnr% > 8%) then editpg1
            if fieldnr% = oldfield% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then editpg1
                  pf4$, pf5$, pf16$ = " "
L11420:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11420
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11420
                  oldfield% = fieldnr%
            goto L11300

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S  F O R  R A N G E S     *~
            *-----------------------------------------------------------*~
            *       Sets defaults and enables fields range inputs       *~
            *************************************************************

            deffn'051(fieldnr%)
                enabled% = 1%

                on fieldnr% gosub        L20100, /* Vendor Code Range  */ ~
                                         L20160, /* Receipt Date Range */ ~
                                         L20190, /* Early Grace Days   */ ~
                                         L20210, /* Late  Grace Days   */ ~
                                         L20220, /* Early Points       */ ~
                                         L20250, /* Late Points        */ ~
                                         L20310, /* Quality Factor     */ ~
                                         L20390  /* Summary or All     */
                            return

L20100: REM Default/Enable for Vendor Code Range
            inpmessage$ = "Enter Vendor Range to Print or "              ~
                         &"'ALL' to include all Vendors."
            if fmvendor$ = " " then fmvendor$ = "ALL"
            if fmvendor$ = "ALL" then tovendor$ = " "
            return

L20160: REM Default/Enable for Range of Receipt Dates
            inpmessage$ = "Enter Range of Receipt Dates."
            if fmdate$ = " " or fmdate$ = blankdate$ then ~
               fmdate$ = "19010101"
            if fmdate$ = "19010101" then ~
               call "DATEOKC" (fmdate$, fmdate%, errormsg$)
            todate$ = date
            call "DATFMTC" (todate$)
            return

L20190: REM Default/Enable for Early Grace Days
            inpmessage$ = "How Early And Still Get Full Point Credit?"
            if edays$ = " " then edays$ = "3"
            return

L20210: REM Default/Enable for Late Grace Days
            inpmessage$ = "How Late And Still Get Full Point Credit?"
            if ldays$ = " " then ldays$ = "1"
            return

L20220: REM Default/Enable for Early Points
            inpmessage$ = "If On-time gets 10 pts, how many if too Early?"
            if epoints$ = " " then epoints$ = "6"
            return

L20250: REM Default/Enable for Late Points
            inpmessage$ = "If On-time gets 10 pts, how many if too Late?"
            if lpoints$ = " " then lpoints$ = "0"
            return

L20310: REM Default/Enable for Accept/Rejecte Weighting Factor
            inpmessage$ = "Enter the Accept vs Reject weighting "        ~
                         &"factor (0 - 100) "
            if acptweight$ = " " then acptweight$ = "100"
            return

L20390: REM Default/Enable for Print Summary or All Flag
            inpmessage$ = "Enter 'S' to print only the summary, or 'A' " ~
                         & "to print all details."
            if sumflag$ = " " then sumflag$ = "S"
            return

        REM *************************************************************~
            *   V A R I A B L E  F I E L D  I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            *      Initializes all defined screen variables to blank    *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$, sumflag$,                  ~
                      fmvendor$, tovendor$,  edays$, ldays$,             ~
                      fmdate$,   todate$,    epoints$, lpoints$,         ~
                      evslweight$, acptweight$, rejweight$
            gosub init_all_variables

        return

        REM *************************************************************~
            * S T A R T  O V E R  L A S T  C H A N C E   S C R E E N    *~
            *-----------------------------------------------------------*~
            * Gives the user the ability to start over wheh he wants to *~
            * or will return user back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *       G E N E R A T E  R E P O R T  S E C T I O N         *~
            *-----------------------------------------------------------*~
            *           Main Section for Report Generation              *~
            *************************************************************

        generate_report
            select printer(134)
            call "SHOSTAT" ("Report Generation in Progress")
            call "SETPRNT" ("VEN003", " ", 0%, 0%)
            prnt_id$ = "VENRATNG: VEN003"
            time$ = " " : call "TIME" (time$)
            page% = -1% : linecntr% = 99%
              found%, postdate%, fmdate%, todate% = 0%
              first% = 1%
              plowkey$ = str(lovendor$, 1, 9) & hex(00)
L30200:       call "PLOWALTS" (#1, plowkey$, 2%, 0%, f1%(1))
              if f1%(1) = 0% and found% =  0% then goto L30230            ~
                                              else goto L30310
L30230:          keyhit% = 2%
                 call "ASKUSER" (keyhit%, "**** No Records Found ****",  ~
                                 "Press PF1 to Start Over",              ~
                                 " --- OR ---",                          ~
                                 "Press PF16 to Exit Program")
                 if keyhit% = 1%  then goto inputmode
                 if keyhit% = 16% then goto exit_program

L30310:       if f1%(1) = 0% and found% <> 0% then print_stat_summary
              get #1 using L30380,                                        ~
                  part$,rcvnum$,vendor$,ponum$,seq$,duedate$,            ~
                  recvdate$,postdate%, totlrecv, rcvhold, qcquan, qchold,~
                  scrquan, rejquan, ohquan, unitcost, extension,         ~
                  rejectcode$, dueorig$

           if dueorig$ <> blankdate$ then duedate$ = dueorig$ /* Use the orig  */
                                                       /* due date if   */
                                                       /* it's available*/

L30380:       FMT CH(25), CH(16), CH(9), CH(16), CH(3), POS(116), CH(6), ~
                  CH(6), XX(16), BI(4), POS(156), PD(14,4), POS(164),    ~
                  PD(14,4), PD(14,4), PD(14,4), PD(14,4), PD(14,4),      ~
                  PD(14,4), POS(212), PD(14,7), POS(248), PD(14,4),      ~
                  POS(350), CH(6), POS(646), CH(6)

        REM Skip if the line is a freight charge
           if part$ = "*CARRIER FREIGHT CHARGE* " then L30200

L30440: REM Check to see if Due Date falls in user selected range
           fduedate$ = duedate$ : frecvdate$ = recvdate$
           call "DATEFMT" (fduedate$)
           call "DATEFMT" (frecvdate$)
           if (recvdate$ > unffmdate$ or recvdate$ = unffmdate$)  and    ~
              (recvdate$ < unftodate$ or recvdate$ = unftodate$)         ~
                 then goto L30540  /* Date is in range, go do compare */  ~
                 else goto L30200  /* Not in range, go get Next Record */

        REM Check to see if it is still the same Vendor
L30540:       if first% = 1% then tempvendor$ = str(vendor$, 1, 9)
              first% = 2%
              if tempvendor$ <> vendor$ then goto  print_stat_summary
              if vendor$ > hivendor$ and page% = -1% then L30230
              if vendor$ > hivendor$ then end_of_report
              found% = found% + 1%

        REM Calculate the Accepted Quantity
            acceptqty  = scrquan + rcvhold + qcquan + qchold  + ohquan

        REM Allow for someone else's definition of rejected
            rejquan = rejquan  /* ??? + SCRQUAN ??? */

        REM Check for open on P.O.
             orderqty  = 0  : remainqty  = 0
             openpokey$ = str(vendor$) & str(ponum$) & str(seq$)
             call "READ100" (#2, openpokey$, f1%(2))  /* Current File */
             if f1%(2) = 0% then L30710
                 get #2 using L30740, orderqty, remainqty
                 goto L30760
L30710:      call "READ100" (#4, openpokey$, f1%(4))  /* History File */
             if f1%(4) = 0% then remainqty  = 0                          ~
                else get #4 using L30740, orderqty ,remainqty
L30740:              FMT POS(93), PD(14,4), XX(8), PD(14,4)

L30760: REM Get Vendor Name from Vendor Master File
            call "READ100" (#3, vendor$, f1%(3))
            if f1%(3) = 0% then venname$ = "** No Vendor Name Found **"  ~
                           else get #3, using L30800, venname$
L30800:                                              FMT POS(40), CH(30)

        REM Received date minus duedate, Neg # means it's early
            days% = 0%
            points% = 10%
            status$ = "ON-TIME  "
            call "DATE" addr("G-", duedate$,recvdate$,days%,ret%)
            if ret% <> 0% then days% = 0%

            if days% <> 0% then L30960
                otnum% = otnum% + 1%
                otrecv   = otrecv   + totlrecv
                otvalue  = otvalue  + extension
                totopoints% = totopoints% + points%
                goto L30990 /* Delivery was on duedate */

L30960:     if days% > 0% then gosub calculate_late                      ~
                          else gosub calculate_early

L30990:     totdpoints% = totdpoints% + points%
            goto L31360

        calculate_late
*        Note that DAYS% is positive here
            if days% <= ld% then L31110
*             Too Late, so gets only late points
               points% = lp%
               tottlpoints% = tottlpoints% + points%
               tlatenum% = tlatenum% + 1%
               tlaterecv   = tlaterecv   + totlrecv
               tlatevalue  = tlatevalue  + extension
               status$ = "TOO LATE "
               return

L31110
*          Within Grace Days, so gets all 10 points
            latenum%   = latenum%   + 1%
            laterecv   = laterecv   + totlrecv
            latevalue  = latevalue  + extension
            status$ = "LATE     "
            points% = 10%
            totlpoints% = totlpoints% + points%
            return

        calculate_early
*        Note that DAYS% is negative here
            if abs(days%) > ed% then L31280
*              Within Grace Days, so gets all 10 points
                earlynum%   = earlynum%   + 1%
                earlyrecv   = earlyrecv   + totlrecv
                earlyvalue  = earlyvalue  + extension
                status$ = "EARLY    "
                points% = 10%
                totepoints% = totepoints% + points%
                return
L31280
*          Too Early, so gets only early points
            tearlynum% = tearlynum% + 1%
            tearlyrecv   = tearlyrecv   + totlrecv
            tearlyvalue  = tearlyvalue  + extension
            status$ = "TOO EARLY"
            points% = ep%
            tottepoints% = tottepoints% + points%
            return

        REM If accepted quantity is not 0 then do calculation
L31360:     if acceptqty  <> 0  then L31380                               ~
                            else L31430
L31380:     acceptnum%  = acceptnum%  + 1%
            acceptrec   = acceptrec   + acceptqty
            acceptvalue = acceptvalue + round((acceptqty  * unitcost),2)

        REM If the rejection quantity is not 0 then bump up counter
L31430:     if rejquan   <> 0  then L31450                                ~
                            else L31500
L31450:     rejectnum%  = rejectnum%  + 1%
            rejectrec   = rejectrec   + rejquan
            rejectvalue = rejectvalue + round((rejquan * unitcost),2)

        REM Print detail and go get next P.O. record when done
L31500:     totalaccept  = totalaccept  + acceptqty
            totalreject  = totalreject  + rejquan
            totalqtyrec  = totalqtyrec  + totlrecv
            totalvalue   = totalvalue   + extension

            convert points%   to points$,    pic(##)
            convert days%     to days$,      pic(###)
            call "CONVERT"(extension, 2.2, extension$)
            call "CONVERT"(orderqty, 2.2, orderqty$)
            call "CONVERT"(totlrecv, 2.2, totlrecv$)
            call "CONVERT"(acceptqty, 2.2, acceptqty$)
            call "CONVERT"(rejquan, 2.2, rejquan$)

            if page% = -1% then gosub print_params
            if sumflag$ = "S" then goto L30200 /* Go get next record */
            if found% = 1% then linecntr% = 56%
            if linecntr% > 55% then gosub print_heading
            print using L60230,fduedate$,frecvdate$,status$,days$,points$,~
                     ponum$, seq$, part$, orderqty$, totlrecv$, extension$
            print using L60250, acceptqty$, rejquan$, rejectcode$
            print skip(1)
            linecntr% = linecntr% + 3%
            goto L30200 /* Go get next record to print */

        print_stat_summary

            if found% = 0% then  L31850
                 earlynumperc   = earlynum%  * 100 / found%
                 tearlynumperc   = tearlynum%  * 100 / found%
                 rejectnumperc  = rejectnum% * 100 / found%
                 acceptnumperc  = acceptnum% * 100 / found%
                 latenumperc    = latenum%   * 100 / found%
                 tlatenumperc    = tlatenum%   * 100 / found%
                 otnumperc      = otnum%    * 100 / found%

L31850:     if totalqtyrec = 0 then L31940
                 earlyrecvperc  = earlyrecv  * 100 / totalqtyrec
                 tearlyrecvperc  = tearlyrecv  * 100 / totalqtyrec
                 rejectrecvperc = rejectrec  * 100 / totalqtyrec
                 acceptrecvperc = acceptrec  * 100 / totalqtyrec
                 laterecvperc   = laterecv   * 100 / totalqtyrec
                 tlaterecvperc   = tlaterecv   * 100 / totalqtyrec
                 otrecvperc      = otrecv      * 100 / totalqtyrec

L31940:     if totalvalue  = 0 then L32030
                 earlyvalperc   = earlyvalue  * 100 / totalvalue
                 tearlyvalperc   = tearlyvalue  * 100 / totalvalue
                 rejectvalperc  = rejectvalue * 100 / totalvalue
                 acceptvalperc  = acceptvalue * 100 / totalvalue
                 latevalperc    = latevalue   * 100 / totalvalue
                 tlatevalperc    = tlatevalue   * 100 / totalvalue
                 otvalperc    = otvalue   * 100 / totalvalue

L32030:       convert otnum% to otnum$, pic(##########)
              call "CONVERT"(otnumperc, 1.1, otnumperc$)
              call "CONVERT"(otrecv, 2.2, otrecv$)
              call "CONVERT"(otrecvperc, 1.1, otrecvperc$)
              call "CONVERT"(otvalue, 2.2, otvalue$)
              call "CONVERT"(otvalperc, 1.1, otvalperc$)

              convert latenum% to latenum$, pic(##########)
              call "CONVERT"(latenumperc, 1.1, latenumperc$)
              call "CONVERT"(laterecv, 2.2, laterecv$)
              call "CONVERT"(laterecvperc, 1.1, laterecvperc$)
              call "CONVERT"(latevalue, 2.2, latevalue$)
              call "CONVERT"(latevalperc, 1.1, latevalperc$)

              convert earlynum% to earlynum$, pic(##########)
              call "CONVERT"(earlynumperc, 1.1, earlynumperc$)
              call "CONVERT"(earlyrecv, 2.2, earlyrecv$)
              call "CONVERT"(earlyrecvperc, 1.1, earlyrecvperc$)
              call "CONVERT"(earlyvalue, 2.2, earlyvalue$)
              call "CONVERT"(earlyvalperc, 1.1, earlyvalperc$)

              convert acceptnum% to acceptnum$, pic(##########)
              call "CONVERT"(acceptnumperc, 1.1, acceptnumperc$)
              call "CONVERT"(acceptrec, 2.2, acceptrec$)
              call "CONVERT"(acceptrecvperc, 1.1, acceptrecvperc$)
              call "CONVERT"(acceptvalue, 2.2, acceptvalue$)
              call "CONVERT"(acceptvalperc, 1.1, acceptvalperc$)

              convert rejectnum% to rejectnum$, pic(##########)
              call "CONVERT"(rejectnumperc, 1.1, rejectnumperc$)
              call "CONVERT"(rejectrec, 2.2, rejectrec$)
              call "CONVERT"(rejectrecvperc, 1.1, rejectrecvperc$)
              call "CONVERT"(rejectvalue, 2.2, rejectvalue$)
              call "CONVERT"(rejectvalperc, 1.1, rejectvalperc$)

              convert tlatenum% to tlatenum$, pic(##########)
              call "CONVERT"(tlatenumperc, 1.1, tlatenumperc$)
              call "CONVERT"(tlaterecv, 2.2, tlaterecv$)
              call "CONVERT"(tlaterecvperc, 1.1, tlaterecvperc$)
              call "CONVERT"(tlatevalue, 2.2, tlatevalue$)
              call "CONVERT"(tlatevalperc, 1.1, tlatevalperc$)

              convert tearlynum% to tearlynum$, pic(##########)
              call "CONVERT"(tearlynumperc, 1.1, tearlynumperc$)
              call "CONVERT"(tearlyrecv, 2.2, tearlyrecv$)
              call "CONVERT"(tearlyrecvperc, 1.1, tearlyrecvperc$)
              call "CONVERT"(tearlyvalue, 2.2, tearlyvalue$)
              call "CONVERT"(tearlyvalperc, 1.1, tearlyvalperc$)

              convert found% to totfound$, pic(##########)
              call "CONVERT"(totalqtyrec, 2.2, totalqtyrec$)
              call "CONVERT"(totalvalue, 2.2, totalvalue$)
              call "RJUSTIFY" (epoints$)
              call "RJUSTIFY" (lpoints$)

              convert ed% to edays$, pic(###)
              convert ld% to ldays$, pic(###)

            gosub calc_vendor_rating
            convert totdpoints% to totdpoints$, pic(##########)
            convert totqpoints% to totqpoints$, pic(##########)
            convert totepoints% to totepoints$, pic(##########)
            convert tottepoints% to tottepoints$, pic(##########)
            convert totlpoints% to totlpoints$, pic(##########)
            convert tottlpoints% to tottlpoints$, pic(##########)
            convert totapoints% to totapoints$, pic(##########)
            convert totrpoints% to totrpoints$, pic(##########)
            convert totopoints% to totopoints$, pic(##########)
            call "CONVERT"(possible, 0.0, possible$)
            call "CONVERT"(delscore, 0.0, delscore$)
            call "CONVERT"(vendorrating, 2.2, vendorrating$)

            call "RJUSTIFY" (acptweight$)
            call "RJUSTIFY" (rejweight$)

            hven$   = vendor$
            summary_flag% = 1%
            vendor$ = tempvendor$
            gosub print_heading
            vendor$ = hven$
            print skip(3)

            print using L60260    /* Delivery Summary Banner */
            print using L60288
            print using L60275    /* column headers */
            print using L60280    /* column headers underline */

            print using L60285, "ON TIME  ", otnum$, otnumperc$,          ~
                otrecv$, otrecvperc$, otvalue$, otvalperc$,              ~
                " " & "10", totopoints$

            print using L60285, "EARLY*   ", earlynum$,  earlynumperc$,   ~
                earlyrecv$, earlyrecvperc$, earlyvalue$, earlyvalperc$,  ~
                " " & "10", totepoints$

            print using L60285, "LATE*    ", latenum$, latenumperc$,      ~
                laterecv$,    laterecvperc$, latevalue$, latevalperc$,   ~
                " " & "10", totlpoints$

            print using L60285, "TOO EARLY ",tearlynum$, tearlynumperc$,  ~
              tearlyrecv$, tearlyrecvperc$, tearlyvalue$, tearlyvalperc$,~
                "  " & epoints$, tottepoints$

            print using L60285, "TOO LATE ", tlatenum$, tlatenumperc$,    ~
                tlaterecv$, tlaterecvperc$, tlatevalue$, tlatevalperc$,  ~
                "  " & lpoints$, tottlpoints$

            print using L60290   /* Totals Line */
            print using L60295, edays$, ldays$, totdpoints$
            print using L60297, possible$
            print using L60300, delscore$
            print skip(3)

            print using L60305   /* Quality Summary Banner  */
            print using L60308
            print using L60310    /* column headers */
            print using L60280    /* column headers underline */

            print using L60285, "ACCEPTED", acceptnum$, acceptnumperc$,   ~
               acceptrec$,acceptrecvperc$, acceptvalue$, acceptvalperc$, ~
                          acptweight$, totapoints$

            print using L60285, "REJECTED", rejectnum$, rejectnumperc$,   ~
               rejectrec$, rejectrecvperc$, rejectvalue$, rejectvalperc$,~
                          rejweight$, totrpoints$

            print using L60290   /* Totals Line */
            print using L60320,  totqpoints$
            print skip(3)

            print using L60330  /* Total Activity Banner */
            print skip(1)
            print using L60340, totfound$,totalqtyrec$,totalvalue$
            print skip(3)

            print using L60400  /* Vendor Rating Banner */
            print using L60415

            print using L60420  /* Formula               */
            print using L60430  /* Formula Underline     */
            print using L60440, delscore$, totqpoints$, vendorrating$
            print skip (3)
            print using L60470   /* Notes text */
            print using L60480   /* Notes text */
            print using L60490   /* Notes text */
            linecntr% = 45%
            if f1%(1) = 0% then end_of_report
            if vendor$ > hivendor$ then end_of_report
            gosub init_all_variables
            goto L30440


        init_all_variables
            linecntr%, found%    = 0%
            first% = 1%
            otnum%, earlynum%, latenum%, tearlynum%, tlatenum% = 0%
            acceptnum%, rejectnum% = 0%

            otrecv, laterecv, earlyrecv, tearlyrecv, tlaterecv = 0
            acceptrec, rejectrec  = 0

            otvalue, latevalue, earlyvalue, tearlyvalue, tlatevalue = 0
            acceptvalue, rejectvalue = 0

            otnumperc, otrecvperc, otvalperc = 0
            latenumperc, laterecvperc, latevalperc = 0
            earlynumperc, earlyrecvperc, earlyvalperc = 0
            tearlynumperc, tearlyrecvperc, tearlyvalperc = 0
            tlatenumperc, tlaterecvperc, tlatevalperc = 0
            acceptnumperc, acceptrecvperc, acceptvalperc = 0
            rejectnumperc, rejectrecvperc, rejectvalperc = 0

            totalqtyrec  = 0  : totalvalue = 0
            totalaccept  = 0% : totalreject  = 0
            totdpoints%, totlpoints%, totepoints% = 0%
            tottlpoints%, tottepoints% = 0%
            totqpoints%, totapoints%, totrpoints% = 0%
            totopoints% = 0%
            possible  = 0
            delscore = 0


            vendorrating = 0
        return

        calc_vendor_rating

            totopoints% = otnum% * 10%
            totepoints% = earlynum% * 10%
            tottepoints% = tearlynum% * ep%
            totlpoints% = latenum% * 10%
            tottlpoints% = tlatenum% * lp%
            totdpoints% = totopoints% + totepoints% + totlpoints% +      ~
                                              tottepoints% + tottlpoints%
            possible  = found% * 10%

            delscore  = (totdpoints% / possible ) * 100

            if acceptrecvperc <> 100 then L33910
               totapoints% = (acceptrecvperc * 100) / 100
               totrpoints% = 0%
               goto L33930
L33910:     totapoints% = (acceptrecvperc * acptweight%) / 100
            totrpoints% = (rejectrecvperc * rejweight% ) / 100
L33930:     totqpoints% = totapoints% + totrpoints%

            vendorrating = (delscore + totqpoints%) / 2

        return

        REM *************************************************************~
            *              P R I N T  S E C T I O N                     *~
            *************************************************************

        print_heading
            print page
            page% = page% + 1%
            print using L60060, date$, time$, company$, prnt_id$
            print using L60080, rpttitle$, page%
            if page% = 0% then L34210 else linecntr% = 6%
            print skip(1)
            print using L60150, venname$, vendor$

            if summary_flag% = 1% then L34210


            print skip(1)
            print using L60170
            print using L60190

            print using L60194
            goto L34230
L34210:     linecntr% = 2%
            summary_flag% = 0%
L34230: return

        print_params
          gosub print_heading
          print skip(3)
          print using L60100 : print
          print using L60120 , "VENDOR CODE", fmvendor$, tovendor$
          print using L60120 , "RECEIPT DATE", fmdate$, todate$
          print using L60135 , "EARLY GRACE DAYS      ", edays$
          print using L60135 , "LATE GRACE DAYS       ", ldays$
          print using L60135 , "EARLY POINTS          ", epoints$
          print using L60135 , "LATE POINTS           ", lpoints$
          print using L60135 , "ACCEPT WEIGHT (VS REJECT)", acptweight$
          print using L60135 , "PRINT SUMMARY OR ALL  ", sumflag$
          print
          print using L60140
          print skip (7)
          print using L60500
          print skip (1)
          print using L60510
          print using L60520
          print using L60530
          print using L60540
          print using L60550
          print using L60560
          print using L60570
          print using L60580
          print using L60590

          linecntr% = 56%
        return

        REM *************************************************************~
            *                 S C R E E N  P A G E 1                    *~
            *-----------------------------------------------------------*~
            *             Document input and edit screen.               *~
            *************************************************************

            deffn'101(fieldnr%)
                if fieldnr% > 0% then init(hex(8c)) lfac$()              ~
                                 else init(hex(86)) lfac$()

            on fieldnr% gosub  L40060,  /* Vendor Range                */ ~
                               L40060,  /* Receipt Dates Range         */ ~
                               L40072,  /* Early Days                  */ ~
                               L40072,  /* Late Days                   */ ~
                               L40072,  /* Early Points                */ ~
                               L40072,  /* Late Points                 */ ~
                               L40072,  /* Accept vs Reject factor     */ ~
                               L40060   /* Summary or All flag         */

            goto   L40084   /* Go to ACCEPT screen         */

L40060: REM Set FAC's for uppercase only input
            lfac$(fieldnr%) = hex(81) /* Bright-Modify-Uppercase */
            return

L40072: REM Set FAC's for numberic only input
            lfac$(fieldnr%) = hex(82) /* Bright-Modify-Numberic  */
            return

L40084:     accept                                                       ~
                at (01,02), "Input report selection criteria",           ~
                at (01,66), "Today:"                             ,       ~
                at (01,73), fac(hex(8c)), date$                  ,ch(08),~
                at (02,02), fac(hex(ac)), line2$                 ,ch(79),~
                at (04,02), fac(hex(94)), errormsg$              ,ch(79),~
                at (07,02), "Vendor Code  "                      ,       ~
                at (07,42), fac(lfac$( 1)), fmvendor$            ,ch(09),~
                at (07,54), "To"                                 ,       ~
                at (07,60), fac(lfac$( 1)), tovendor$            ,ch(09),~
                at (08,02), "Receipt Date "                      ,       ~
                at (08,42), fac(lfac$( 2)), fmdate$              ,ch(10),~
                at (08,54), "To"                                 ,       ~
                at (08,57), fac(lfac$( 2)), todate$              ,ch(10),~
                at (09,02), "Early Grace Days"                   ,       ~
                at (09,42), fac(lfac$( 3)), edays$               ,ch(03),~
                at (10,02), "Late Grace Days"                    ,       ~
                at (10,42), fac(lfac$( 4)), ldays$               ,ch(03),~
                at (11,02), "Points for Too Early"               ,       ~
                at (11,42), fac(lfac$( 5)), epoints$             ,ch(01),~
                at (12,02), "Points for Too Late"                ,       ~
                at (12,42), fac(lfac$( 6)), lpoints$             ,ch(01),~
                at (13,02), "Accept weight (vs Reject) "         ,       ~
                at (13,42), fac(lfac$( 7)), acptweight$          ,ch(03),~
                at (14,02), "Print Summary or All (S/A)"         ,       ~
                at (14,42), fac(lfac$( 8)), sumflag$             ,ch(01),~
                at (21,02), fac(hex(a4)), inpmessage$            ,ch(79),~
                at (22,02), "(1)Start Over"                      ,       ~
                at (22,65), "(13)Instructions"                   ,       ~
                at (23,20), fac(hex(8c)), pf4$                   ,       ~
                at (23,65), "(15)Print Screen"                   ,       ~
                at (24,65), fac(hex(84)), pf16$                  ,       ~
                                                                         ~
                keys(hex(000104050d0f10))                        ,       ~
                key (keyhit%)

                if keyhit% <> 13% then L40430
                   call "MANUAL" ("VENRATNG")
                   goto L40084

L40430:         if keyhit% <> 15% then  L40470
                   call "PRNTSCRN"
                   goto L40084

L40470:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *                    T E S T  D A T A                       *~
            *-----------------------------------------------------------*~
            *            Test data for the items on screen 1.           *~
            *************************************************************

            deffn'151(fieldnr%)
                errormsg$ = " "

                on fieldnr% gosub L50086,  /* Vendor Code Range         */~
                                  L50100,  /* Date Range                */~
                                  L50150,  /* Early Days                */~
                                  L50180,  /* Late  Days                */~
                                  L50200,  /* Early Points              */~
                                  L50220,  /* Late  Points              */~
                                  L50240,  /* Quality  Weighting Factor */~
                                  L50280   /* Summary or All Flag       */
                            return

L50086: REM Test for Vendor Code Range
            call "TESTRNGE" (fmvendor$, tovendor$,                       ~
                             lovendor$, hivendor$, errormsg$)
            return

L50100: REM Test for Receipt Dates Range
            call "DATEOKC" (fmdate$, fmdate%, errormsg$)
            if errormsg$ <> " " then return
            call "DATEOKC" (todate$, todate%, errormsg$)
            if errormsg$ <> " " then return
            unffmdate$ = fmdate$ : unftodate$ = todate$
            call "DATUFMTC" (unffmdate$) : call "DATUFMTC" (unftodate$)
            if (unffmdate$ >  unftodate$)                                ~
                then errormsg$ = "From may not be greater then To."
            return

L50150: REM Test for Early Days
            call "NUMTEST" (edays$, 0, 100, errormsg$, 0.0, edays)
            ed% = edays
            return

L50180: REM Test for Late Days
            call "NUMTEST" (ldays$, 0, 100, errormsg$, 0.0, ldays)
            ld% = ldays
            return

L50200: REM Test for Early Points
            call "NUMTEST" (epoints$, 0, 9, errormsg$, 0.0, epoints)
            ep% = epoints
            return

L50220: REM Test for Late Points
            call "NUMTEST" (lpoints$, 0, 9, errormsg$, 0.0, lpoints)
            lp% = lpoints
            return

L50240: REM Test for Quality Weighting Percentage Factor
            if acptweight$ <> " " then L50250
                goto L50266
L50250:     convert acptweight$ to acptweight%, data goto L50266
            if acptweight% < 0% then L50266
            if acptweight% > 100% then L50266

            rejweight% =  (100% - acptweight%)
            convert rejweight% to rejweight$, pic(###)
            return

L50266:     errormsg$ = "Weighting factor must be between 0 and 100"
            return

L50280: REM Test for Print Summary or All
            if sumflag$ = "S" or sumflag$ = "A" then return
            errormsg$ = "Enter either 'S' for Summary or 'A' for All"
            return


        REM *************************************************************~
            *             I M A G E  S T A T E M E N T S                *~
            *-----------------------------------------------------------*~
            *        Image statements for report print lines.           *~
            *************************************************************

L60060: %RUN ########   ########               ##########################~
        ~#################################                   #############~
        ~###
L60080: %                                                  ##############~
        ~#############################################           PAGE:   #~
        ~###
L60100: %                             ------------------------- REPORT SE~
        ~LECTION PARAMETERS -------------------------
L60120: %                                ###############                 ~
        ~  #########     TO     #########
L60135: %                                #############################   ~
        ~  ###
L60140: %                             -----------------------------------~
        ~--------------------------------------------

L60150: %         VENDOR NAME:    ##############################       VE~
        ~NDER NUMBER:     ##########

L60170: %DATE      DATE

L60190: %DUE       RECEIVED  STATUS   DAYS POINTS PO NUMBER      LINE  PA~
        ~RT NO. /  ACCEPTED QTY PO QTY/REJ QTY  RECV QTY/REJ CODE       VA~
        ~LUE
L60194: %--------  -------- --------- ---- ------ -------------- ---- ---~
        ~---------------------- --------------  ----------------- --------~
        ~---
L60230: %########  ######## #########  ###   ##   ##############  ### ###~
        ~###################### ##########      ##########        ########~
        ~###
L60250: %                                                                ~
        ~         ############      ##########           ########


L60260: %        ----------------------------------------------- DELIVERY~
        ~ SUMMARY -------------------------------------------------

L60275: %          Status   Number of Receipts   %   Quantity Received   ~
        ~ %    Value of Receipts   %      Points    Delivery Points
L60280: %        ---------  ------------------ ----- ------------------ -~
        ~----  ----------------- -----    ------    ---------------
L60285: %        ##########     ##########     #####     ##########     #~
        ~####      ##########    #####      ###        ##########
L60288: %                                                                ~
        ~                                                 (1)
L60290: %                                                                ~
        ~                                              ----------
L60295: %        *Within Grace Days of:  Early ###  Late ###             ~
        ~                     Total Delivery Points =  ##########
L60297: %                                                                ~
        ~                           Possible Points =  ##########
L60300: %                                                                ~
        ~                           Delivery Score  =  ##########

L60305: %        ----------------------------------------------- QUALITY ~
        ~SUMMARY --------------------------------------------------

L60308: %                                                                ~
        ~                                                 (2)
L60310: %          Status   Number of Receipts   %   Quantity Received   ~
        ~ %    Value of Receipts   %      Weight    Quality Points
L60320: %                                                                ~
        ~                      Total Quality Score  =  ##########
L60330: %        ----------------------------------------------- TOTAL AC~
        ~TIVITY ---------------------------------------------------
L60340: %                      Total Shipments: ##########  Total Quantit~
        ~y: ##########   Total Value: ##########
L60400: %        ----------------------------------------------- VENDOR R~
        ~ATING ----------------------------------------------------

L60415: %                                                                ~
        ~               (3)
L60420: %                                    (Delivery Score + Quality Sc~
        ~ore) / 2 = Vendor Rating
L60430: %                                    ----------------  ----------~
        ~---        -------------
L60440: %                                    (   ##########  +  #########~
        ~#    / 2 =  ##########

L60470: %      Notes: (1) Delivery Points are calculated as: Number of Re~
        ~ceipts of that status X Points.
L60480: %             (2) Quality Points are calculated as: the % of Quan~
        ~tity Received of that status X Weight.
L60490: %             (3) Vendor Rating scale is maximum 100 points.
L60500: %                             Vendor rating scale is 0 to 100 poi~
        ~nts with 100 being maximum.
L60510: %                             One half the score is based on deli~
        ~very performance and half on quality (accepted
L60520: %                             vs rejected).  On-time delivery (wi~
        ~thin grace period) is worth 10 points per ship-
L60530: %                             ment.  Fewer points are given if ou~
        ~tside grace period.  Point penalties for early
L60540: %                             and late are defined by the user.  ~
        ~Total delivery points are calculated as total
L60550: %                             points earned, divided by the total~
        ~ maximum points possible, multiplied by 100.
L60560: %                             Quality points are calculated as th~
        ~e total accepted percent multipled by the
L60570: %                             accepted weight factor, plus the to~
        ~tal rejected percent multipled by the rejected
L60580: %                             weight factor.  Delivery and qualit~
        ~y points are then summed and divided by 2 to
L60590: %                             obtain rating.







L60620: %                                         ********** End of Repor~
        ~t (########) **********

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            *       Terminates execution (files closed automatically).  *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1989  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        end_of_report

            if linecntr% > 55% then print page
            print skip(1)
            time$ = " "  :  call "TIME" (time$)
            print using L60620, time$
            call "SETPRNT" ("VEN003", " ", 0%, 1%)
            goto inputmode

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
