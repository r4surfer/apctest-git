        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  PPPP   IIIII  IIIII  N   N  PPPP    *~
            *  H   H  NN  N  Y   Y  P   P    I      I    NN  N  P   P   *~
            *  HHHHH  N N N   YYY   PPPP     I      I    N N N  PPPP    *~
            *  H   H  N  NN    Y    P        I      I    N  NN  P       *~
            *  H   H  N   N    Y    P      IIIII  IIIII  N   N  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYPIINP - This program provides the means of recording / *~
            *            entering count quantities for a given count    *~
            *            session.  Several methods of entry are provided*~
            *            to cover most situations.                      *~
            *----------------------------------------------------------Q*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/16/86 ! Original                                 ! LDJ *~
            * 06/10/86 ! Corrected bug in Ticket Length due to    ! LDJ *~
            *          !   using function LGT, use LOG function   !     *~
            * 03/23/87 ! Added cases quantities                   ! HES *~
            * 03/24/87 ! File Layout changes                      ! LDJ *~
            * 05/21/87 ! Standard Cost Enhancements               ! MJB *~
            * 10/22/87 ! Fixed Standard Cost Enhancements         ! HES *~
            * 02/09/88 ! FIX PROBLEM WITH LOT FIELD BEING ENABLED ! BPN *~
            * 02/13/90 ! Added LOCATIONS File and Location verify.! JEF *~
            * 12/20/91 !* CMS/DEC 'MASK' Project/Added 'ALLFREE'  ! SID *~
            *          !* Set the count date equal to Last        !     *~
            *          !   Entered Date, Session Date or Today's  !     *~
            *          !   Date depending on the date of input.   !     *~
            *          !* Added 'SHOSTAT' at files open.          !     *~
            *          !* Added a call to 'NUMTEST' for the total !     *~
            *          !   qty count when performing a single     !     *~
            *          !   ticket entry                           !     *~
            *          !* Intialized qty count bucket variables   !     *~
            *          !   CASES(), CASEQ() prior to DATAPUT when !     *~
            *          !   performing a multiple entry and fast   !     *~
            *          !   count entry.                           !     *~
            * 01/06/92 !* PRR 12248 Fixed the Lot Tracking Error  ! SID *~
            * 02/24/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 04/06/92 !* Cycle Count Modifications & ...         ! RJH *~
            *          !* PRR 11876 - Unit of Measure display     !     *~
            *          !* PRR 12367 - Rentered New Part Cost Fix  !     *~
            * 06/04/92 !* PRR 11748 - Screen 2, OVERRIDE$ Variable!     *~
            *          !*  to not switch to 'Y' on 'ENTER'.       !     *~
            *          !* PRR 11453 - Check for existence of Cost !     *~
            *          !*  Picture (HNYPICST) Exit if not unless  !     *~
            *          !*  No HNYQUAN file.                       !     *~
            *          !* PRR 12387 - ENTERED_BY$ Variable set to !     *~
            *          !*  USERID$.                               !     *~
            * 10/08/92 !* Fix Extra tickts being defaluted as VOID! RJH *~
            * 11/13/92 !* PRR 12626 - READ on HOLD just prior to  ! RJH *~
            *          !*  PUT ing of data to Ticket File.        !     *~
            *          !* Fix misc. implied integer conversions.  !     *~
            * 04/12/93 !* Correct Date display on multiple tickets! RJH *~
            *          !*  for line numbers > 6.                  !     *~
            * 05/14/93 !* PRR 12879 - Single Ticket Entry edit    ! RJH *~
            *          !*  ergonomics improved (hopefully).       !     *~
            * 07/21/93 !* Fix ReadKey when testing for Cycle Count! RJH *~
            * 09/14/93 !* Use HNYPITKT's Primary Key for recount #! RJH *~
            * 10/04/93 !* PRR 13026 - Allow for extra tickets in  ! RJH *~
            *          !*  Cycle Counting but New Location only.  !     *~
            * 03/06/95 !* Intialized Total Cost variable TOT_COST !     *~
            *          !   prior to DATAPUT when performing a     !     *~
            *          !   multiple entry and fast count entry.   !     *~
            * 08/08/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED **

        dim                                                              ~
            accounted_for$1,             /* All Tickets Accounted For? */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            by$20,                       /* Counted By                 */~
            by$(13)20,                   /* Counted By                 */~
            bylit$20,                    /* Screen Literal for Count By*/~
            caseq(5),                    /* Quantity Per Case          */~
            cases(5),                    /* Cases Counted              */~
            caseq$(5)5,                  /* Quantity Per Case          */~
            cases$(5)9,                  /* Cases Counted              */~
            ccactflag$1,                 /* Cycle Count Activity Flag  */~
            ccsession$12,                /* Cycle Count Session Name   */~
            ccsesmsg$32,                 /* Cycle Count Session Message*/~
            check_digit$1,               /* Calc & Append Check Digit? */~
            cost1(12),                   /* 'SnapShot' Cost Buckets    */~
            cost$96,                     /* 12 cost bkts for override  */~
            costmsg$20,                  /* Screen 2 Costs Literal     */~
            costtype$1,                  /* Inventory Costing Method   */~
            count_date$8,                /* Date Counted               */~
            count_date$(13)8,            /* Count Date                 */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            datelit$8,                   /* Screen Literal for Date Cnt*/~
            dispvar2$19,                 /* Display Variable fr VarReas*/~
            edtmessage$79,               /* Edit screen message        */~
            entered_by$3,                /* Operator User ID           */~
            enter_date$6,                /* Date Entered               */~
            enter_time$6,                /* Time Entered               */~
            error$79,                    /* Error message              */~
            errormsg$79,                 /* Error message              */~
            extra$1,                     /* Extra/Supplemental Ticket? */~
            filekey$100,                 /* Miscellaneous Read Key     */~
            find_ticket$(1)12,           /* Ticket Number to Find/Posit*/~
            gvar$1, dfgvar$1,            /* Post G/L Variance $ & dflt */~
            hvar$1, dfhvar$1,            /* Update Inventory Qty's ?   */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lastby$20,                   /* Last Counted By Value      */~
            lastdate$8,                  /* Last Counted Date Value    */~
            lastpsl$34,                  /* Last Part/Store/Lot        */~
            lastticket$6,                /* Last Ticket Nbr Processed  */~
            last_ticket$12,              /* Last Ticket Nbr Entered    */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            loc$8,                       /* Location                   */~
            location$8,                  /* Location                   */~
            loc_valid$1,                 /* Location must be valid flag*/~
            lot$16,                      /* Lot                        */~
            lotno$16,                    /* Lot                        */~
            lot_or_loc$1,                /* Tickets Generated by Lot or*/~
            mfac$(13,6)1,                /* Field Attribute Characters */~
            mfactemp$6,                  /* Field Attribute Characters */~
            newpart_flag$1,              /* Extra Ticket New Part Flag */~
            override$1,                  /* Override Unit Costs    ?   */~
            part$25,                     /* Part Code                  */~
            partdescr$32,                /* Part Code                  */~
            part$(13)25,                 /* Part Code                  */~
            part_req$1,                  /* Part Numbers required ?    */~
            partlit$25,                  /* Screen Literal for Part #  */~
            pf4$17,                      /* PF  4 literal              */~
            pf5$17,                      /* PF  5 literal              */~
            pf8$17,                      /* PF  8 literal              */~
            pf10$22,                     /* PF 10 literal (UOM Toggle) */~
            pf16$13,                     /* PF 16 literal              */~
            plowkey$100,                 /* Miscellaneous Plow Key     */~
            prefix$3,                    /* Ticket Number Prefix (Opt) */~
            prompt$28,                   /* Find Ticket Prompt         */~
            qty$10,                      /* Quantity Counted           */~
            qty$(13)10,                  /* Quantity                   */~
            qtylit$10,                   /* Screen Literal For Qty     */~
            readkey$100,                 /* Miscellaneous Read Key     */~
            recount$1,                   /* Recount Number             */~
            recount$(13)1,               /* Recount Number             */~
            session_date$8,              /* Planned Count Date         */~
            session_nbr$2,               /* Count Session Number       */~
            session_nbrdescr$32,         /* Count Session Number       */~
            start_ticket$6,              /* First Ticket in Session    */~
            store$3,                     /* Warehouse                  */~
            storedescr$32,               /* Warehouse                  */~
            ticket$12,                   /* Ticket Number              */~
            ticket$(13)12,               /* Ticket No.                 */~
            tickt2$(13)12,               /* Ticket No. (for REDIM)     */~
            ticketlit$13,                /* Screen Literal for Ticket #*/~
            tkt$(1)12,                   /* Ticket Number (for ReDIM)  */~
            toggle$(13)20,               /* Toggle Accept Field        */~
            togglelit$20,                /* Toggle Accept Field Descr  */~
            tot_cost$10,                 /* Override Total Cost        */~
            uom$4,                       /* Unit of Measure (Stocking) */~
            uom$(13)20,                  /* Unit of Measure (Stocking) */~
            uomflag$1,                   /* Unit of Measure Display Flg*/~
            userid$3,                    /* Current User Id            */~
            varlit1$6,                   /* Display Header Variable    */~
            varlit2$6,                   /* Display Header Variable    */~
            varreason$6,                 /* Cycle Count Variance Reason*/~
            varreason$(13)6,             /* Cycle Count Variance Reason*/~
            varreasonline$30             /* Accept Screen Line         */~

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
            * #1  ! HNYPISYS ! Physical Inventory System Session Contro *~
            * #2  ! HNYPITKT ! Physical Inventory Ticket File           *~
            * #3  ! HNYMASTR ! Inventory Master File                    *~
            * #4  ! STORNAME ! Store Info File - Name/Address           *~
            * #5  ! HNYQUAN  ! Inventory Part / Store / Lot Quantity Fi *~
            * #6  ! SYSFILE2 ! CMS System Information File              *~
            * #7  ! HNYPICST ! Physical Inventory Costs Snapshot File   *~
            * # 8 ! LOCATION ! Location master File                     *~
            * #9  ! GENCODES ! General Codes File                       *~
            * #24 ! HNYCCDTL ! Cycle Count Detail File                  *~
            * #26 ! HNYCCSYS ! Cycle Count Session File                 *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "HNYPISYS",                                      ~
                        varc,     indexed,  recsize = 512,               ~
                        keypos =    7, keylen =  2,                      ~
                        alt key  1, keypos =    1, keylen =   8

            select #2,  "HNYPITKT",                                      ~
                        varc,     indexed,  recsize = 512,               ~
                        keypos =    1, keylen =  15,                     ~
                        alt key  1, keypos =   16, keylen =  52, dup,    ~
                            key  2, keypos =  313, keylen =  16          ~

            select #3,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #4,  "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3                      ~

            select #5,  "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =  1, keylen =  44

            select #6,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =   1, keylen =  20

            select #7,  "HNYPICST",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  46


            select #8,  "LOCATION",                                      ~
                         varc,    indexed,  recsize =  400,              ~
                         keypos =   1, keylen =  11,                     ~
                         alt key  1, keypos =   4, keylen =  11

            select #9 , "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24                      ~

            select #24, "HNYCCDTL",                                      ~
                        varc,     indexed,  recsize =  436,              ~
                        keypos =    1, keylen =  57,                     ~
                        alt key  2, keypos =   14, keylen =  44, dup,    ~
                            key  1, keypos =   13, keylen =  45, dup     ~

            select #26, "HNYCCSYS",                                      ~
                        varc,     indexed,  recsize =  249,              ~
                        keypos =    1, keylen =  12,                     ~
                        alt key  1, keypos =   43, keylen =  13, dup,    ~
                            key  2, keypos =   56, keylen =  10

            call "SHOSTAT"   ("Opening Files, One Moment Please")
                rslt$(1%) = "REQUIRED"
            call "OPENCHCK" (#1,  fs%(1%), f2%(1%), 0%, rslt$(1%))
                if fs%(1) < 0% then exit_program
                rslt$(2 ) = "REQUIRED"
            call "OPENCHCK" (#2,  fs%(2%), f2%(2%), 0%, rslt$(2%))
                if fs%(2) < 0% then exit_program
                rslt$(3 ) = "REQUIRED"
            call "OPENCHCK" (#3,  fs%(3%), f2%(3%), 0%, rslt$(3%))
                if fs%(3) < 0% then exit_program
                rslt$(4 ) = "REQUIRED"
            call "OPENCHCK" (#4,  fs%(4%), f2%(4%), 0%, rslt$(4%))
                if fs%(4) < 0% then exit_program
            call "OPENCHCK" (#5,  fs%(5%), f2%(5%), 0%, rslt$(5%))
            call "OPENCHCK" (#6,  fs%(6%), f2%(6%), 0%, rslt$(6%))
            call "OPENCHCK" (#7,  fs%(7%), f2%(7%), 0%, rslt$(7%))
            call "OPENCHCK" (#8,  fs%(8%), f2%(8%), 0%, rslt$(8%))
            call "OPENCHCK" (#9,  fs%(9%), f2%(9%), 0%, rslt$(9%))
            call "OPENCHCK" (#24, fs%(24%), f2%(24%), 0%, rslt$(24%))
            call "OPENCHCK" (#26, fs%(26%), f2%(26%), 0%, rslt$(26%))

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
            costmsg$ = "Inventory Cost Each"
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value and Press RETURN."
            ticketlit$ = "Ticket   Rcnt"
            partlit$   = "Part Code (If Required)"
            qtylit$    = "Quantity"
            bylit$     = "Counted By  (opt)"
            togglelit$ = bylit$
            datelit$   = "Date "
            ll%        = 6%

            call "READ100" (#6, "SWITCHS.HNY", f1%(6))
            if f1%(6) = 0% then L09220
            get #6 using L09210, loc_valid$
L09210:         FMT POS(107), CH(1)
L09220:
        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            init(" ") inpmessage$, session_nbr$, session_nbrdescr$,      ~
                      ccsesmsg$
            call "ALLFREE"

        input_page1
            errormsg$, last_ticket$ = " "
            fieldnr% = 1%
            screen% = 1%
            gosub'051(fieldnr%)
L10070:     gosub'101(fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       exit_program
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L10070
                  if keyhit%  =  2% then       single_entry
                  if keyhit%  =  3% then       multiple_entry
                  if keyhit%  =  4% then       count_sheet_entry
            goto L10070

        REM *************************************************************~
            *       I N P U T   M O D E   S I N G L E   E N T R Y       *~
            *-----------------------------------------------------------*~
            * Handles normal input for single ticket screen.            *~
            *************************************************************

        single_entry
            init(" ") errormsg$, inpmessage$, part$, partdescr$, store$, ~
                      storedescr$, lot$, loc$, qty$, hvar$, gvar$,       ~
                      override$, by$, count_date$, caseq$(),  cases$(),  ~
                      recount$, uom$, varreason$, newpart_flag$,tot_cost$
            screen% = 2%
            first_field% = 1%
            call "ALLFREE"

        input_page2

            pf16$ = "(16)Return" : pf4$ = "(4)Previous Field"
            pf5$ = "(5)Next Ticket"

        reenter_input
            for fieldnr% = first_field% to 12%
                                   /* Re-Initialize Lot Enabled Field */
                if first_field% = 1%  then lot_enable% = 0%
L10220:         mode% = 1%
                gosub'052(fieldnr%)
                      if enabled% = 0 then L10285
L10230:         gosub'102(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10270
L10245:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% > 0% then L10230
                         if fieldnr% = 1% then L10220
                         goto L10245
L10270:               if keyhit%  =  5% then       next_ticket
                      if keyhit%  = 16% then       input_page1
                      if keyhit% <>  0% then       L10230
L10285:         gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10230
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   S I N G L E   E N T R Y        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for single entry screen.   *~
            *************************************************************

        edtpg2
            pf16$ = "(16)Save Data" : pf4$ = " " : pf5$ = " "
            inpmessage$ = edtmessage$
            gosub'102(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       edtpg2
            oldfield% = 0%
            if cursor%(1%) = 10% or cursor%(1%) = 11% then cursor%(1%)= 9%
L10375:     if cursor%(1%) > 11% then cursor%(1%) = cursor%(1%) - 2%
            if cursor%(1%) >  13% then cursor%(1%) = cursor%(1%) - 1%
            fieldnr% = cursor%(1) - 4%
            if fieldnr% < 2% or fieldnr% > 12% then edtpg2
            if fieldnr% = 7% then fieldnr% = 6%
            if fieldnr% = 8% and cursor%(2%) < 52% then fieldnr% = 7%
            if oldfield% = fieldnr% then edtpg2
            mode% = 2%
            gosub'052(fieldnr%)
                  if enabled% = 0% then       edtpg2
L10420:     gosub'102(fieldnr%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L10420
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L10420
                  oldfield% = fieldnr%
                  if fieldnr% <> 12% then L10375
                  if override$ <> "Y" then L10375
                      call "HNYCDIST" ("E", part$, partdescr$,           ~
                            str(line2$,,60%), #6, cost$, tot_cost$,      ~
                            tot_cost)
                      goto L10375

        next_ticket
            plowkey$ = str(session_nbr$) & str(ticket$) &                ~
                       bin(99%-recount%,1)
            call "PLOWNXT1" (#2, plowkey$, 2%, f1%(2%))
            if f1%(2%) = 0% then L10515
               gosub load_ticket
               convert recount% to recount$, pic(0)
               first_field% = 1%
               goto input_page2
L10515:     u3% = 2%
            call "ASKUSER" (u3%, "***END OF FILE***",                    ~
                            "Sorry, You've already seen the last Ticket",~
                            "     ", "Please press RETURN to continue.")
            goto single_entry


        REM *************************************************************~
            *       I N P U T   M O D E   M U L T I P L E   E N T R Y   *~
            *-----------------------------------------------------------*~
            * Handles normal input for multiple tickets screen.         *~
            *************************************************************

        multiple_entry
            init(" ") ticket$(), part$(), qty$(), by$(), count_date$(),  ~
                      inpmessage$, errormsg$, recount$(),varreason$(),   ~
                      uom$(), toggle$()
            call "ALLFREE"

            screen% = 3% :  mode% = 1%
            pf16$ = "(16)Return" : pf4$ = "(4)Previous Line"
            pf10$ = "(10)UOM/CountBy Toggle"

            for line% = 1% to 13%
                if line% > 1% then pf16$ = "(16)Edit Mode"
L11120:         gosub'053(line%)
L11140:         if ccsession$ = " " then gosub'103(line%)                ~
                                    else gosub'104(line%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L11190
                         if line% = 1% then L11140
                         if ticket$(line%) = " " then gosub L11252
                         line% = max(1%, line% - 1%)
                         goto L11120
L11190:               if keyhit% <> 16% then       L11200
                         if ticket$(line%) <> " " then       L11210
                            gosub L11252
                            if line% > 1% then edtpg3
                            goto input_page1
L11200:               if keyhit% <>  0% then       L11120
L11210:         gosub'153(line%)
                      if errormsg$ <> " " then L11140
                if ticket$(line%) = " " then line% = 13%
                if keyhit%  = 16% then line% = 13%
            next line%
            goto edtpg3

L11252:     init(" ") ticket$(line%), part$(line%), qty$(line%),         ~
                      by$(line%), count_date$(line%), recount$(line%),   ~
                      inpmessage$, errormsg$, varreason$(line%),         ~
                      uom$(line%)
            return

        REM *************************************************************~
            *        E D I T   M O D E   M U L T I P L E   E N T R Y    *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for multiple entry screen. *~
            *************************************************************

        edtpg3
            pf16$ = "(16)Save Data" : pf4$ = " "
            mode% = 2%
            inpmessage$ = edtmessage$
            if ccsession$ = " " then gosub'103(0%) else gosub'104(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       edtpg3
            line% = cursor%(1) - 5%
            if line% < 1% or fieldnr% > 13% then edtpg3
            if ticket$(line%) = " " then edtpg3
            gosub'053(line%)
                  if enabled% = 0% then       edtpg3
                  pf16$ = " "
                  inpmessage$ = "Enter Changes and then press RETURN"
L11440:     if ccsession$ = " " then gosub'103(line%)                    ~
                                else gosub'104(line%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11440
            gosub'153(line%)
                  if errormsg$ <> " " then L11440
            goto edtpg3

        toggle_uom
            if uomflag$ = "Y" then L11580
              /* Switch Toggle Field from Count By to UOM */
                uomflag$ = "Y"
                for xn% = 1% to 13% :    by$(xn%) = toggle$(xn%) :next xn%
                for xn% = 1% to 13% :toggle$(xn%) = uom$(xn%)    :next xn%
                mfac$(line%,4%) = hex(8c)
                if screen% <> 4% then L11560
                for xn% = 1% to 13% : mfac$(xn%,4%) = hex(8c) : next xn%
L11560:         togglelit$ = "UOM"
                return
              /* Switch Toggle Field from UOM to Count By */
L11580:         uomflag$ = "N"
                for xn% = 1% to 13% :   uom$(xn%) = toggle$(xn%) :next xn%
                for xn% = 1% to 13% :toggle$(xn%) =  by$(xn%)    :next xn%
                if mode% = 1% then mfactemp$ = hex(81)                   ~
                              else mfactemp$ = hex(8c)
                if screen% <> 4% then L11616
                for xn% = 1% to 13%
                if ticket$(xn%) <> " " then L11612
                     xn% = 13%  :  goto L11614
L11612:         mfac$(xn%,4%) = mfactemp$
L11614:         next xn%
L11616:         togglelit$ = "Count By"
                mfac$(line%,4%) = hex(81)
             return

        REM *************************************************************~
            *       I N P U T   M O D E   C O U N T   E N T R Y         *~
            *-----------------------------------------------------------*~
            * Handles normal input for count sheets fast entry screen.  *~
            *************************************************************

        count_sheet_entry
            init(" ") ticket$(), part$(), qty$(), by$(), count_date$(),  ~
                      inpmessage$, errormsg$, recount$(), prompt$,       ~
                      find_ticket$(1), varreason$(), uom$(), toggle$()
            call "ALLFREE"
            uomflag$ = "N" : togglelit$ = "Count by" /* Set to Count By */
            screen% = 4% :  mode% = 1%
            pf16$ = "(16)Return" : pf8$ = "(8)Find Ticket:"
            pf10$ = "(10)UOM/CountBy Toggle"
            lfac$(1) = hex(81)

            gosub'054(0%)

L12140:     if ccsession$ = " " then  gosub'105(0%)                      ~
                                else  gosub'106(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  8% then       get_ticket
                  if keyhit%  = 16% then       input_page1
                  if keyhit% <>  0% then       L12140
            gosub'154(0%)
                  if errormsg$ <> " " then L12140
                  if str(ticket$(),1%) = " " then count_sheet_entry


        REM *************************************************************~
            *        E D I T   M O D E   F A S T   E N T R Y            *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for fast count sheet entry.*~
            *************************************************************

        edtpg4
            pf16$ = "(16)Save Data" : pf8$ = " " : find_ticket$(1) = " "
            init (hex(8c)) mfac$(), lfac$()
            mode% = 2%
            inpmessage$ = edtmessage$
            if ccsession$ = " " then  gosub'105(0%)  else  gosub'106(0%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       edtpg4
            line% = cursor%(1%) - 5%
            if line% < 1% or fieldnr% > 13% then edtpg4
            if ticket$(line%) = " " then edtpg4
            gosub'054(line%)
                  if enabled% = 0% then       edtpg4
                  pf16$ = " "
                  inpmessage$ = "Enter Changes and then press RETURN"
L12460:     if ccsession$ = " " then  gosub'105(line%)                   ~
                                else  gosub'106(line%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L12460
            gosub'154(line%)
                  if errormsg$ <> " " then L12460
            goto edtpg4

        get_ticket
            last_ticket$ = find_ticket$(1) addc all(hex(ff))
            goto count_sheet_entry

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            mat cases = zer : mat caseq = zer
            on screen% - 1% goto L19490
            REM *** Multiple Tickets Save ***
               if uomflag$ = "Y" then L19090
                   for n% = 1% to 13% : by$(n%) = toggle$(n%) : next n%
L19090:        for line% = 1% to 13%
                   if ticket$(line%) = " " then L19240
                   tot_cost, tot_cost1 = 0
                   recount% = 0%
                   convert recount$(line%) to recount%, data goto L19130
L19130:            readkey$ = str(session_nbr$) & str(ticket$(line%)) &  ~
                           bin(99%-recount%,1)
                   call "READ100" (#2, readkey$, f1%(2%))
                   REM *** No Test - Fatal if not there now anyway! ***
                   gosub load_ticket
                   mat cases = zer  :  mat caseq = zer
                   part$ = part$(line%)
                   qty = 0
                   if qty$(line%) = "VOID" then qty = -1
                   if qty = -1 then L19210
                   convert qty$(line%) to qty, data goto L19201
L19201:            cases(1) = qty
                   caseq(1) = 1
L19210:            by$ = by$(line%)
                   count_date$ = count_date$(line%)
                   varreason$ = varreason$(line%)
                   gosub dataput
L19240:        next line%
               goto L19520

L19490:     REM *** Single Ticket Save ***
            if qty$ = "VOID" then qty = -1
            if qty$ = "VOID" then L19500
            for i% = 1% to 5%
                convert cases$(i%) to cases(i%), data goto L19495
L19495:         convert caseq$(i%) to caseq(i%), data goto L19496
L19496:     next i%
L19500:     gosub dataput
            readkey$ = str(session_nbr$) & str(ticket$) & hex(ff)
            call "PLOWNEXT" (#2, readkey$, 2%, f1%(2%))
            if f1%(2%) = 1% then ticket$ = str(readkey$,3%,12%)

L19520:     gosub update_session_flag
            on screen% - 1% goto single_entry, multiple_entry,           ~
                                 count_sheet_entry

        update_session_flag
            if accounted_for$ = " " then return
            accounted_for$ = " "
            call "READ101" (#1, session_nbr$, f1%(1))
            REM *** no test - if gone we're screwed anyway ***
            put #1 using L19600, accounted_for$
L19600:     FMT POS(365), CH(1)
            rewrite #1
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100          /* Count Session Num*/
                  return
L20100:     REM Default/Enable for Count Session Number
                inpmessage$ = "Enter the Count Session Number to Report" ~
                            & " Qty's for & then Press PF 2, 3, or 4"
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

            deffn'052(fieldnr%)
                  enabled% = 0%
                  on fieldnr% gosub L21100,         /* Ticket Number    */~
                                    L21150,         /* Part Code        */~
                                    L21200,         /* Warehouse        */~
                                    L21250,         /* Lot              */~
                                    L21300,         /* Location         */~
                                    L21350,         /* Quantity Counted */~
                                    L21400,         /* Counted By       */~
                                    L21450,         /* Date Counted     */~
                                    L21485,         /* Variance Reason  */~
                                    L21500,         /* Update Inventory */~
                                    L21550,         /* Post G/L Variance*/~
                                    L21600          /* Override Unit Cos*/
                  return
L21100:     REM Default/Enable for Ticket Number
                inpmessage$ = "Enter the Recount Number only if present a~
        ~t end of Ticket Number (e.g. '-1')"
                enabled% = 1%
                return
L21150:     REM Default/Enable for Part Code
                if part_req$ = "Y" then enabled% = 1%
                if part_req$ = "Y" and extra$ = " " then part$ = " "
                if extra$ > " " and newpart_flag$ = "Y" then enabled% = 1%
                if extra$ > " " and mode% = 2% then enabled% = 1%
                inpmessage$ = "Enter the Part Number Counted"
                return
L21200:     REM Default/Enable for Warehouse
                if extra$ = " " then return
                  enabled% = 1%
                  inpmessage$ = "Enter the Warehouse Code where this " & ~
                                "ticket was counted"
                return
L21250:     REM Default/Enable for Lot
                inpmessage$ = "Enter the Lot Number"
                if extra$ = " "      then return
                call "LOTENABL" (part$, lot_enable%, ll%, #6, #3)
                enabled% = sgn(lot_enable%)
                return
L21300:     REM Default/Enable for Location
                if extra$ = " "          then return
                   enabled% = 1%
                inpmessage$="Enter the Location counted at (Blank is OK)"
                return
L21350:     REM Default/Enable for Quantity Counted
                enabled% = 1%
                inpmessage$="Enter the quantity counted or VOID to " &   ~
                            "Void ticket."
                if extra$ > " " and part$ = " " then cases$() = "VOID"
                if extra$ > " " and part$ = " " then enabled% = 0%
                return
L21400:     REM Default/Enable for Counted By
                if by$ = " " then by$ = lastby$
                if keyhit% = 4% then enabled% = 1%
                if pf4$ = " " then enabled% = 1%
                if by$ = " " then enabled% = 1%
                if extra$ > " " and part$ = " " then enabled% = 0%
                if qty$ = "VOID" then enabled% = 0%
                inpmessage$="Enter the person who counted this ticket " &~
                            "or leave blank"
                return
L21450:     REM Default/Enable for Date Counted
                if count_date$ = " " or count_date$ = blankdate$ ~
                                   then count_date$ = lastdate$
                if count_date$ = " " or count_date$ = blankdate$ ~
                                   then count_date$ = session_date$
                if keyhit% = 4% then enabled% = 1%
                if pf4$ = " " then enabled% = 1%
                inpmessage$="Enter the actual date counted "
                if extra$ > " " and part$ = " " then enabled% = 0%
                if qty$ = "VOID" then enabled% = 0%
                return
L21485:     REM Default/Enable for Variance Reason Code
                if ccsession$ = " " then return
                if keyhit% = 4% then enabled% = 1%
                if pf4$ = " " then enabled% = 1%
                if varreasonline$ <> " " then enabled% = 1%
                if qty$ = "VOID" then enabled% = 0%
                inpmessage$="Enter the variance reason code "           &~
                            "or leave blank"
                return
L21500:     REM Default/Enable for Update Inventory Qty's ?
                inpmessage$ = "Enter Y to Update, N to don't Update, " & ~
                              "or Leave Blank to Use Session Default"
                if pf4$ = " " then enabled% = 1%
                if extra$ > " " and part$ = " " then enabled% = 0%
                if qty$ = "VOID" then enabled% = 0%
                return
L21550:     REM Default/Enable for Post G/L Variance $    ?
                inpmessage$ = "Enter Y to Post, N to don't Post, " &     ~
                              "or Leave Blank to Use Session Default"
                if pf4$ = " " then enabled% = 1%
                if extra$ > " " and part$ = " " then enabled% = 0%
                if qty$ = "VOID" then enabled% = 0%
                return
L21600:     REM Default/Enable for Override Unit Costs    ?
                tot_cost, tot_cost1 = 0
                convert tot_cost$ to tot_cost, data goto L21605
L21605:         inpmessage$ = "Enter Y to Override & Manually Enter " &  ~
                              "Costs or N to use costs on file"
                if pf4$ > " " and tot_cost = 0 and (enter_date$ = " "    ~
                   or enter_date$ = blankdate$) then override$ = "Y"
                if override$ = " " then override$ = "N"
                if override$ = "Y" then enabled% = 1%
                if pf4$ = " " then enabled% = 1%
                if keyhit% = 4% then enabled% = 1%
                if extra$ > " " and part$ = " " then enabled% = 0%
                if extra$ > " " and part$ = " " then override$ = "N"
                if qty$ = "VOID" then enabled% = 0%
                if qty$ = "VOID" then override$ = "N"
                if enabled% = 0% then return
                readkey$ = str(part$) & str(store$) & lot$
                call "READ100" (#5, readkey$, f1%(5))
                if f1%(5) = 0% then  L21642
                get #5 using L21640, costtype$
L21640:         FMT POS(335), CH(1)
                goto L21655
L21642:         call "READ100" (#3, part$, f1%(3))
                if f1%(3) = 0% then return
                get #3 using L21645, costtype$
L21645:         FMT POS(307), CH(1)
L21655:         if pos("SFT" = costype$) > 0% then enabled% = 0%
                if pos("SFT" = costype$) > 0% then override$= "N"
                return

        REM *************************************************************~
            *  D E F A U L T / E N A B L E   F O R   P A G E   3  &  3A *~
            *-----------------------------------------------------------*~
            * Sets FAC's and ENABLES fields for Screen 3 & 3A of Input. *~
            *************************************************************

            deffn'053(line%)
              init(hex(8c))mfac$()
              if ccsession$ = " " then n% = 5%  else  n%  =  6%
              inpmessage$="Blank all fields on a line to exit Input Mode"
              for fieldnr% = 1% to n%
                  on fieldnr% gosub L22100,         /* Ticket No.       */~
                                    L22200,         /* Part Code        */~
                                    L22300,         /* Quantity         */~
                                    L22400,      /* Counted By/UOM Toggl*/~
                                    L22500,         /* Count Date       */~
                                    L22600          /* Variance Reason  */
              next fieldnr%
              if ticket$(line%) = " " then enabled% = 0% else enabled%=1%
              return
L22100:     REM Default/Enable for Ticket No.
                mfac$(line%,fieldnr%) = hex(81)
                return
L22200:     REM Default/Enable for Part Code
                if part_req$ = "Y" then mfac$(line%,fieldnr%) = hex(81)
                return
L22300:     REM Default/Enable for Quantity
                mfac$(line%,fieldnr%) = hex(81)
                return
L22400:     REM Default/Enable for Counted By/UOM Toggle
                if uomflag$ <> "Y" then L22410
                     mfac$(line%,fieldnr%) = hex(8c) :  return
L22410:         mfac$(line%,fieldnr%) = hex(81)
                if by$(line%) = " " and line% > 1% then by$(line%) =     ~
                   by$(line% - 1%)
                if by$(line%) = " " and line% = 1% then by$(line%) =     ~
                   lastby$
                return
L22500:     REM Default/Enable for Count Date
                mfac$(line%,fieldnr%) = hex(81)
                if (count_date$(line%) = " " or  ~
                    count_date$(line%) = blankdate$) ~
                    and line% > 1%  then         ~
                    count_date$(line%) = count_date$(line% - 1%)
                if (count_date$(line%) = " " or  ~
                    count_date$(line%) = blankdate$) ~
                    and line% = 1%  then         ~
                    count_date$(line%) = lastdate$
                if session_date$ >= date$  then L22550
                if (count_date$(line%) = " " or  ~
                    count_date$(line%) = blankdate$) ~
                    and line% = 1%  then         ~
                    count_date$(line%) = session_date$
L22550:         if (count_date$(line%) = " " or  ~
                    count_date$(line%) = blankdate$)~
                    and line% = 1%  then         ~
                    count_date$(line%) = date$
                return
L22600:     REM Default/Enable for Variance Reason
                mfac$(line%,fieldnr%) = hex(81)
                return

        REM *************************************************************~
            *  D E F A U L T / E N A B L E   F O R   P A G E   4  &  4A *~
            *-----------------------------------------------------------*~
            * Sets FAC's and ENABLES fields for Screen 4 & 4A of Input. *~
            *************************************************************
            deffn'054(line%)
              init(hex(8c))mfac$()
              if line% > 0% then L23350
              line% = 0%   :  n% = 5%
L23090:       plowkey$ = str(session_nbr$) & str(last_ticket$) &         ~
                         bin(99%,1)
L23110:       call "PLOWNXT1" (#2, plowkey$, 2%, f1%(2%))
              if f1%(2%) = 0% then L23310
L23130:       gosub load_ticket
              if extra$ <> " " then L23110
              readkey$ = plowkey$
              if qty >=0 then L23190      /* Is this a Voided Ticket ? */
                 call "PLOWNXT1" (#2, plowkey$,14%, f1%(2%))
                 if f1%(2%) = 1% then L23130
                 call "READ100" (#2, readkey$, f1%(2%))
                 gosub load_ticket
L23190:       line% = line% + 1%
              convert recount% to recount$, pic(0)
              last_ticket$ = ticket$
              ticket$    (line%) = ticket$
              recount$   (line%) = recount$
              part$      (line%) = part$
              qty$       (line%) = qty$
              count_date$(line%) = count_date$
              by$        (line%) = by$
              uom$       (line%) = uom$
              gosub get_uom_value
              if uomflag$ = "Y" then toggle$(line%) = uom$
                   toggle$(line%) = by$
              if ccsession$ = " " then L23280
                   varreason$(line%) = varreason$  :  n% = 6%
L23280:       gosub L23350
              if line% < 13% then L23090

L23310:       if line% > 0% then return
              errormsg$ = "NO MORE TICKETS!  USE PF-8 TO REPOSITION "
              return

L23350:       for fieldnr% = 1% to n%
                  on fieldnr% gosub L23450,         /* Ticket No.       */~
                                    L23480,         /* Part Code        */~
                                    L23530,         /* Quantity         */~
                                    L23560,     /* Counted By/UOM Toggle*/~
                                    L23630,         /* Count Date       */~
                                    L23710          /* Variance Reason  */
              next fieldnr%
              if ticket$(line%) = " " then enabled% = 0% else enabled%=1%
              return

L23450:     REM Default/Enable for Ticket No.
                mfac$(line%,fieldnr%) = hex(8c)
                return
L23480:     REM Default/Enable for Part Code
                if part_req$ = "Y" then mfac$(line%,fieldnr%) = hex(81)
                if part_req$ = "Y" and (enter_date$ = " " or ~
                   enter_date$ = blankdate$) then            ~
                   part$(line%) = " "
                return
L23530:     REM Default/Enable for Quantity
                mfac$(line%,fieldnr%) = hex(81)
                return
L23560:     REM Default/Enable for Counted By
                if uomflag$ <> "Y" then L23570
                    mfac$(line%,4%) = hex(8c) :  return
L23570:         mfac$(line%,fieldnr%) = hex(81)
        /*      IF BY$(LINE%) = " " AND LINE% > 1% THEN BY$(LINE%) =     ~
                   BY$(LINE% - 1%)
                IF BY$(LINE%) = " " AND LINE% = 1% THEN BY$(LINE%) =     ~
                   LASTBY$     */
                return
L23630:     REM Default/Enable for Count Date
                mfac$(line%,fieldnr%) = hex(81)
                if count_date$(line%) =  " " or count_date$(line%) = blankdate$~
                            then count_date$(line%) = lastdate$
                if session_date$ >= date$  then L23646
                if count_date$(line%) =  " " or count_date$(line%) = blankdate$~
                            then count_date$(line%) = session_date$
L23646:         if count_date$(line%) =  " " or count_date$(line%) = blankdate$~
                            then count_date$(line%) = date$

        /*      IF COUNT_DATE$(LINE%) = " " AND LINE% > 1% THEN          ~
                   COUNT_DATE$(LINE%) = COUNT_DATE$(LINE% - 1%)
                IF COUNT_DATE$(LINE%) = " " AND LINE% = 1% THEN          ~
                   COUNT_DATE$(LINE%) = LASTDATE$    */
                return
L23710:     REM Default/Enable for Variance Reason
                mfac$(line%,fieldnr%) = hex(81)
                return
        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
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
            call  "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            last_ticket$ = " "
            on screen% goto inputmode, single_entry, multiple_entry,     ~
                            count_sheet_entry

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            get #1 using L35030,                                          ~
            session_date$,  /* Planned Count Date                      */~
            lot_or_loc$,    /* Tickets Generated by Lot or Location ?  */~
            prefix$,        /* 1 to 3 character prefix to the P.I. Tick*/~
            start_ticket$,  /* First ticket number generated           */~
            lastticket$,    /* Last ticket number generated for a Count*/~
            check_digit$,   /* Check Digit ?                           */~
            part_req$,      /* Part Numbers required for count entry ? */~
            dfhvar$,        /* Default Inv Update Flag                 */~
            dfgvar$,        /* Default G/L Update Flag                 */~
            accounted_for$  /* All Tickets Accounted For ? (Y or blank)*/

            call "DATEFMT" (session_date$)

            REM *** Determine Length of Ticket Number Excluding Check# ***
            ticket$ = prefix$
            convert lastticket$ to ticket%
            p% = pos(ticket$ = " ")
            l% = log(ticket%) / log(10) + 1%
            if len(start_ticket$) > l% then l% = len(start_ticket$)
            l% = l% + p% - 1%
            if check_digit$ = "Y" then l% = l% + 1%
            ticket$ = " " : ticket% = 0%
            mat redim tkt$(1)l%
            mat redim tickt2$(13)l%
            gosub get_cycle_count_session
            return

        load_ticket
            get #2 using L35275,  /* FILE: HNYPITKT                     */~
               session_nbr$,/* Number corresponding to a Inventory     */~
                            /* Count Session                           */~
               ticket$,     /* Ticket Number to a Physical Inventory   */~
                            /* Count Item                              */~
               recount%,    /* A number decremented for each recount   */~
                            /* (99=original                            */~
               part$,       /* Part Number                             */~
               store$,      /* Store or Warehouse Code                 */~
                            /* Warehouse code                          */~
               lot$,        /* Lot Number                              */~
               loc$,        /* bin location                            */~
                            /* Actual Bin Location (from HNYQUAN file  */~
                            /* or HNYLOCNSfile)                        */~
               extra$,      /* Extra (supplemental) Ticket Flag: "X" = */~
                            /* extra, blank = not an extra             */~
               gvar$,       /* Flag or switch which controls posting   */~
                            /* to G/L                                  */~
               hvar$,       /* Flag or switch which controls posting   */~
                            /* to Inventory                            */~
               override$,   /* Override flag / indicator               */~
                            /* = "Y" if cost(s) are manually           */~
                            /* overridden                              */~
               by$,         /* Name of person who counted something    */~
               entered_by$, /* user-id of specific user                */~
                            /* User Id of Data Entry Operator - 1 for  */~
                            /* each count                              */~
               count_date$, /* Date something was counted              */~
               enter_date$, /* Date a transaction was entered          */~
                            /* Date entered into computer              */~
               enter_time$, /* The System Time when a transaction was  */~
                            /* entered                                 */~
               qty,         /* Quantity counted of something           */~
                            /* Actual Quantity On Hand according to    */~
                            /* the count. If less than 0 then ticket   */~
                            /* is a VOIDED ticket.                     */~
               cases(),     /* Quantity Counted for the current UOM    */~
                            /* (CASE)                                  */~
                            /* Case quantities table (upto 5)          */~
               caseq(),     /* # of Stocking (base) Units which go     */~
                            /* into this Unit                          */~
               locqty,      /* Quantity From HNYLOCNS file (if         */~
                            /* applicable)                             */~
               cost$,       /* 12 cost buckets                         */~
               tot_cost,    /* Total Standard Cost                     */~
               rcnt_2%,     /* A number decremented for each recount   */~
               varreason$   /* Cycle Count Variance Reason             */

            rcnt_2% = rcnt_2%         /* Do Nothing Line */
            if extra$ > " " and part$ = " "  then   newpart_flag$ = "Y"
            if entered_by$ <> " "  then L30415
                entered_by$ = userid$
L30415:     gosub load_book_values
            call "CONVERT" (qty, -.2,  qty$)
            for i% = 1% to 5%
                call "CONVERT" (cases(i%),  0.2, cases$(i%))
                call "CONVERT" (caseq(i%), -0.2, caseq$(i%))
                if caseq(i%) = 0 and cases(i%) = 0 then                  ~
                   caseq$(i%), cases$(i%) = " "
            next i%
*          CALL "CONVERT" (TOT_COST, -4.4, TOT_COST$)
            if tot_cost1 = 0.0 or tot_cost <> 0.0                        ~
                             then call "CONVERT" (tot_cost,-4.4,tot_cost$)
            call "DATEFMT" (count_date$)
            call "READ100" (#3, part$, f1%(3%))
                if f1%(3%) = 0% then L30525  /* Shouldn.t happen */
                get #3 using L36300,  partdescr$, uom$
L30525:     call "DESCRIBE" (#4, store$, storedescr$, 1%, f1%(4))
            recount% = 99% - recount%
            if varreason$ < hex(20) then varreason$ = hex(20)
            if qty=0 and (enter_date$ = " " or enter_date$ = blankdate$) ~
               then qty$, cases$(), caseq$() = " "
            if qty < 0 then qty$, cases$(1) = "VOID"
            return

        load_book_values
            if count_date$ <> " " and count_date$ <> blankdate$ then return
            filekey$ = str(session_nbr$) & str(part$) & str(store$) & lot$
            if filekey$ = key(#7) and fs(#7) = "00" then L30620
            call "READ100" (#7, filekey$, f1%(7))
                if f1%(7) = 0% then return
            qoh = 0
L30620:     get #7 using L36095,                                          ~
                qoh,        /* quantity on-hand                        */~
                            /* Frozen qty-on-hand from HNYQUAN record. */~
                            /* This is the 'snapshot' qty-on-hand.     */~
                cost1(),    /* 12 cost buckets                         */~
                tot_cost1   /* Total Standard Cost                     */~
                            /*         These are the frozen or         */~
                            /* 'snapshot' cost fields                  */
            call "PACKZERO" (cost1(), cost$)
            call "CONVERT" (tot_cost1, -4.4, tot_cost$)

            return

        get_cycle_count_session
            ccsession$, ccsesmsg$, varlit1$ = " " :  varlit2$ = "(OPT)"
            readkey$ = session_nbr$ & hex(00)
            call "PLOWALTS" (#26, readkey$, 2%, 2%, f1%(26%))
L30735:     if f1%(26%) =  0% then L30799
                if str(key(#26, 2%),,2) <> session_nbr$ then L30799
                if str(key(#26, 1%),,1) = "A"  then L30790

                call "READNEXT" (#26, f1%(26))
                goto L30735

L30790:         get #26 using L30798, ccsession$, ccactflag$
                ccsesmsg$ = "(Cycle Counting Session)"
                str(dispvar2$,13,1) = hex(99)
                varlit1$ = "Var"  :  varlit2$ = "Reason"
                varreasonline$ = "Variance Reason           :"
                return
L30798:     FMT  CH(12), POS(43), CH(1)
L30799:     varreasonline$ = " "
            return
        REM *************************************************************~
            *          P L A C E   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Places data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            call "DATUNFMT" (count_date$)
            tot_cost = 0
            convert tot_cost$ to tot_cost, data goto L31070
L31070:     recount% = 99% - recount%
            recount$ = bin(recount%, 1)
            readkey$ = str(session_nbr$) & str(ticket$) & recount$
            call "READ101" (#2, readkey$, f1%(2%))

            put #2 using L35275,  /* FILE: HNYPITKT                     */~
               session_nbr$,/* Number corresponding to a Inventory     */~
                            /* Count Session                           */~
               ticket$,     /* Ticket Number to a Physical Inventory   */~
                            /* Count Item                              */~
               recount%,    /* A number decremented for each recount   */~
                            /* (99=original                            */~
               part$,       /* Part Number                             */~
               store$,      /* Store or Warehouse Code                 */~
                            /* Warehouse code                          */~
               lot$,        /* Lot Number                              */~
               loc$,        /* bin location                            */~
                            /* Actual Bin Location (from HNYQUAN file  */~
                            /* or HNYLOCNSfile)                        */~
               extra$,      /* Extra (supplemental) Ticket Flag: "X" = */~
                            /* extra, blank = not an extra             */~
               gvar$,       /* Flag or switch which controls posting   */~
                            /* to G/L                                  */~
               hvar$,       /* Flag or switch which controls posting   */~
                            /* to Inventory                            */~
               override$,   /* Override flag / indicator               */~
                            /* = "Y" if cost(s) are manually           */~
                            /* overridden                              */~
               by$,         /* Name of person who counted something    */~
               entered_by$, /* user-id of specific user                */~
                            /* User Id of Data Entry Operator - 1 for  */~
                            /* each count                              */~
               count_date$, /* Date something was counted              */~
               date,        /* Date a transaction was entered          */~
                            /* Date entered into computer              */~
               time,        /* The System Time when a transaction was  */~
                            /* entered                                 */~
               qty,         /* Quantity counted of something           */~
                            /* Actual Quantity On Hand according to    */~
                            /* the count. If less than 0 then ticket   */~
                            /* is a VOIDED ticket.                     */~
               cases(),     /* Quantity Counted for the current UOM    */~
                            /* (CASE)                                  */~
                            /* Case quantities table (upto 5)          */~
               caseq(),     /* # of Stocking (base) Units which go     */~
                            /* into this Unit                          */~
               locqty,      /* Quantity From HNYLOCNS file (if         */~
                            /* applicable)                             */~
               cost$,       /* 12 Cost Buckets      (over-rides        */~
               tot_cost,    /* Total Standard Cost      by user)       */~
               recount%,    /* A number decremented for each recount   */~
               varreason$   /* Cycle Count Variance Reason             */

            rewrite #2
            /* Update Cycle Count File If Needed */
            if ccsession$ = " " then return
                readkey$ = str(ccsession$) &  "A" &  str(part$) &        ~
                           str(store$) &  lot$
                call "READ101" (#24, readkey$, f1%(24%))
                if f1%(24%) = 0% then return
                gosub get_variance
                put #24 using L31610, count_date$, userid$, qty ,         ~
                                     unitcost, unitvar, costvar,         ~
                                     varreason$, by$
                rewrite #24
L31610:     FMT POS(59), CH(6), CH(3), PD(14,4), PD(14,4), PD(14,4),     ~
                PD(14,4), POS(131), CH(6), CH(12)
            return

        get_variance
            unitcost, unitvar, costvar = 0.0
            readkey$ = str(session_nbr$) & str(part$) & str(store$) & lot$
            call "READ100" (#7, readkey$, f1%(7%))
            if f1%(7%) = 0% then return
            get #7 using L31750, bohqty, unitcost

            unitvar = qty - bohqty
            costvar = unitvar * unitcost

            return

L31750:    FMT  POS(47), PD(14,4), POS(151), PD(14,4) /*HNYPICST Picture*/

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: HNYPISYS                          */~
            CH(6),          /* Date something was counted              */~
            POS(319),                                                    ~
            CH(1),          /* Tickets Generated by Lot or Location ?  */~
            CH(3),          /* 1 to 3 character prefix to the P.I. Tick*/~
            CH(06),         /* First Ticket Number in Session          */~
            CH(06),         /* Last ticket number generated for a Count*/~
            CH(1),          /* Check Digit ?                           */~
            XX(6),          /* Number of extra Physical Inventory Ticke*/~
            XX(1),          /* Flag controlling what sequence tickets w*/~
            CH(1),          /* Part Numbers required for count entry ? */~
            POS(345), 2*CH(1), /* Inv & G/L Update Flags               */~
            POS(365),                                                    ~
            CH(1)           /* All Tickets Accounted For Y/N         ? */~

L35275: FMT                 /* FILE: HNYPITKT                          */~
            CH(2),          /* Number corresponding to a Inventory     */~
                            /* Count Session                           */~
            CH(12),         /* Ticket Number to a Physical Inventory   */~
                            /* Count Item                              */~
            BI(1),          /* A number decremented for each recount   */~
                            /* (99=original                            */~
            CH(25),         /* Part Number                             */~
            CH(3),          /* Store or Warehouse Code                 */~
                            /* Warehouse code                          */~
            CH(16),         /* Lot Number                              */~
            CH(8),          /* bin location                            */~
                            /* Actual Bin Location (from HNYQUAN file  */~
                            /* or HNYLOCNSfile)                        */~
            CH(1),          /* Extra (supplemental) Ticket Flag: "X" = */~
                            /* extra, blank = not an extra             */~
            CH(1),          /* Flag or switch which controls posting   */~
                            /* to G/L                                  */~
            CH(1),          /* Flag or switch which controls posting   */~
                            /* to Inventory                            */~
            CH(1),          /* Override flag / indicator               */~
                            /* = "Y" if cost(s) are manually           */~
                            /* overridden                              */~
            CH(20),         /* Name of person who counted something    */~
            CH(3),          /* user-id of specific user                */~
                            /* User Id of Data Entry Operator - 1 for  */~
                            /* each count                              */~
            CH(6),          /* Date something was counted              */~
            CH(6),          /* Date a transaction was entered          */~
                            /* Date entered into computer              */~
            CH(6),          /* The System Time when a transaction was  */~
                            /* entered                                 */~
            PD(14,4),       /* Quantity counted of something           */~
                            /* Actual Quantity On Hand according to    */~
                            /* the count. If less than 0 then ticket   */~
                            /* is a VOIDED ticket.                     */~
            5*PD(14,4),     /* Quantity Counted for the current UOM    */~
                            /* (CASE)                                  */~
                            /* Case quantities table (upto 5)          */~
            5*PD(14,4),     /* # of Stocking (base) Units which go     */~
                            /* into this Unit                          */~
            PD(14,4),       /* Quantity From HNYLOCNS file (if         */~
                            /* applicable)                             */~
            CH(96),         /* 12 cost buckets       (Over-ride        */~
            PD(14,4),       /* Total Standard Cost         by user)    */~
            POS(328), BI(1),/* A Number decremented for each recount   */~
            CH(6),          /* Cycle Count Variance Reason             */~
            CH(164)         /* Filler  (Internal, unused space)        */

L36095: FMT                 /* FILE: HNYPICST                          */~
            POS(47),        /* Position for Field ON-HAND              */~
            PD(14,4),       /* quantity on-hand                        */~
                            /* Frozen qty-on-hand from HNYQUAN record. */~
                            /* This is the 'snapshot' qty-on-hand.     */~
            13*PD(14,4)     /* 1 - 12 are 12 cost buckets              */~
                            /* 13 is total standard cost               */~
                            /*         These are the frozen or         */~
                            /* 'snapshot' costs.                       */

L36300: FMT                 /* FILE: HNYMASTR                          */~
            POS(26),        /* Position for Description                */~
            CH(32),         /* Part Description                        */~
            POS(74),        /* Position for Stocking UOM               */~
            CH(4)           /* Stocking UOM                            */~

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document input screen.                                    *~
            *************************************************************

            deffn'101(fieldnr%)
                  line2$ = "Select Entry Method"
                  str(line2$,62%) = "HNYPIINP: " & str(cms2v$,,8%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40150          /* Count Session Num*/
                  goto L40220

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40150:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40220:     accept                                                       ~
               at (01,02),                                               ~
                  "Enter Physical Inventory Counts",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02),                                               ~
        "  This function provides the means of recording / entering count~
        ~ quantities",                                                    ~
               at (06,02),                                               ~
        "  for the given Count Session.  Multiple means of entering the i~
        ~nformation on",                                                  ~
               at (07,02),                                               ~
        "  the tickets is provided.  You may enter the ticket information~
        ~ one ticket",                                                    ~
               at (08,02),                                               ~
        "  at a time or multiple tickets at a time.  Extra or supplementa~
        ~l tickets may",                                                  ~
               at (09,02),                                               ~
        "  only be entered one at a time.  Please enter the Count Session~
        ~ Number",                                                        ~
               at (10,02),                                               ~
        "  requested below and then select the appropriate PF Key for the~
        ~ action /",                                                      ~
               at (11,02),                                               ~
        "  entry method desired.",                                       ~
               at (14,02),                                               ~
                  "  Count Session Number    :",                         ~
               at (14,30), fac(lfac$( 1)), session_nbr$         , ch(02),~
               at (14,49), fac(hex(8c)),   session_nbrdescr$    , ch(32),~
               at (15,49), fac(hex(8c)),   ccsesmsg$            , ch(32),~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (23,02), "(1)Start Over",                              ~
               at (22,25), "(2)Enter  1 Ticket  at a Time",              ~
               at (23,25), "(3)Multiple Tickets at a Time",              ~
               at (24,25), "(4)Fast Count Sheet Entry    ",              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                                                                         ~
               keys(hex(00010203040d0f10)),                              ~
               key (keyhit%)

               if keyhit% <> 13% then L40770
                  call "MANUAL" ("HNYPIINP")
                  goto L40220

L40770:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L40220

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document input screen.                                    *~
            *************************************************************

            deffn'102(fieldnr%)
                  line2$ = "Single Ticket Entry Mode for Count Session " ~
                         & session_nbr$
                  str(line2$,62%) = "HNYPIINP: " & str(cms2v$,,8%)
                  tkt$(1) = ticket$
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                                   else init(hex(86)) lfac$()
                  on fieldnr% gosub L41150,         /* Ticket Number    */~
                                    L41150,         /* Part Code        */~
                                    L41150,         /* Warehouse        */~
                                    L41150,         /* Lot              */~
                                    L41150,         /* Location         */~
                                    L41150,         /* Quantity Counted */~
                                    L41150,         /* Counted By       */~
                                    L41150,         /* Date Counted     */~
                                    L41177,         /* Variance Reason  */~
                                    L41150,         /* Update Inventory */~
                                    L41150,         /* Post G/L Variance*/~
                                    L41150          /* Override Unit Cos*/
                  goto L41185

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L41150:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return
L41177:           REM Set FAC's for Variance Reason Input
                      if ccsession$ <> " "  then goto L41150
                          lfac$(fieldnr%) = hex(9c) : return

L41185:     accept                                                       ~
               at (01,02),                                               ~
                  "Enter Physical Inventory Counts",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02),                                               ~
                  "Ticket Number             :",                         ~
               at (05,30), fac(lfac$( 1)), tkt$(1%)                     ,~
               at (05,45),                                               ~
                  "Recount Number (blank = 0):",                         ~
               at (05,74), fac(lfac$( 1)), recount$             , ch(01),~
               at (06,02),                                               ~
                  "Part Code                 :",                         ~
               at (06,30), fac(lfac$( 2)), part$                , ch(25),~
               at (06,56), fac(hex(8c)),   partdescr$           , ch(25),~
               at (07,02),                                               ~
                  "Warehouse                 :",                         ~
               at (07,30), fac(lfac$( 3)), store$               , ch(03),~
               at (07,49), fac(hex(8c)),   storedescr$          , ch(32),~
               at (08,02),                                               ~
                  "Lot                       :",                         ~
               at (08,30), fac(lfac$( 4)), str(lot$,,ll%),               ~
               at (09,02),                                               ~
                  "Location                  :",                         ~
               at (09,30), fac(lfac$( 5)), loc$                 , ch(08),~
               at (10,02),                                               ~
        "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ~
        ~- - - - - - - -",                                                ~
               at (11,02), "Unit of Measure           :",                ~
               at (11,30), fac(hex(8c)), uom$                   , ch(04),~
                                                                         ~
               at (12,02),                                               ~
                  "Quantities Counted        :",                         ~
               at (13,02),                                               ~
                  "   (Units / Qty Per Unit) :",                         ~
               at (12,30), fac(lfac$( 6%)), cases$(1%)          , ch(09),~
               at (12,40), fac(lfac$( 6%)), caseq$(1%)          , ch(05),~
               at (12,48), fac(lfac$( 6%)), cases$(2%)          , ch(09),~
               at (12,58), fac(lfac$( 6%)), caseq$(2%)          , ch(05),~
               at (12,66), fac(lfac$( 6%)), cases$(3%)          , ch(09),~
               at (12,76), fac(lfac$( 6%)), caseq$(3%)          , ch(05),~
               at (13,30), fac(lfac$( 6%)), cases$(4%)          , ch(09),~
               at (13,40), fac(lfac$( 6%)), caseq$(4%)          , ch(05),~
               at (13,48), fac(lfac$( 6%)), cases$(5%)          , ch(09),~
               at (13,58), fac(lfac$( 6%)), caseq$(5%)          , ch(05),~
                                                                         ~
               at (13,64), "Total",                                      ~
               at (13,71), fac(hex(84)),   qty$                 , ch(10),~
                                                                         ~
               at (14,02),                                               ~
                  "Counted By                :",                         ~
               at (14,30), fac(lfac$( 7)), by$                  , ch(20),~
               at (14,52), "Date Counted:",                              ~
               at (14,66), fac(lfac$( 8)), count_date$          , ch(08),~
               at (15,02), fac(hex(8c)),   varreasonline$       , ch(28),~
               at (15,30), fac(lfac$( 9)), varreason$           , ch(06),~
                                                                         ~
               at (16,02),                                               ~
        "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ~
        ~- - - - - - - -",                                                ~
               at (17,02),                                               ~
                  "Update Inventory Qty's ?  :",                         ~
               at (17,30), fac(lfac$(10)), hvar$                , ch(01),~
               at (17,35), "Session Default Value is x",                 ~
               at (17,60), fac(hex(ac)),   dfhvar$              , ch(01),~
               at (18,02),                                               ~
                  "Post G/L Variance $    ?  :",                         ~
               at (18,30), fac(lfac$(11)), gvar$                , ch(01),~
               at (18,35), "Session Default Value is x",                 ~
               at (18,60), fac(hex(ac)),   dfgvar$              , ch(01),~
               at (19,02),                                               ~
                  "Override Unit Costs    ?  :",                         ~
               at (19,30), fac(lfac$(12)), override$            , ch(01),~
               at (19,35), fac(hex(ac)),   costmsg$             , ch(20),~
               at (19,57), fac(lfac$(13)), tot_cost$            , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), "(1)Start Over",                              ~
               at (23,25), fac(hex(84)),   pf4$                 , ch(17),~
               at (23,65), "(15)Print Screen",                           ~
               at (24,25), fac(hex(84)),   pf5$                 , ch(17),~
               at (24,65), fac(hex(84)), pf16$                  , ch(13),~
                                                                         ~
               keys(hex(000104050d0f10)),                                ~
               key (keyhit%)

               ticket$ = tkt$(1)

               if keyhit% <> 13 then L41605
                  call "MANUAL" ("HNYPIINP")
                  goto L41185

L41605:        if keyhit% <> 15 then L41630
                  call "PRNTSCRN"
                  goto L41185

L41630:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Input Screen for Multiple Tickets w/ PI Vars              *~
            *************************************************************

            deffn'103(line%)
                  if line% = 0% then init(hex(84))mfac$()
                  line2$="Multiple Tickets Entry Mode for Count Session "~
                         & session_nbr$
                  str(line2$,62%) = "HNYPIINP: " & str(cms2v$,,8%)
                  for u3%=1% to 13%: tickt2$(u3%)=ticket$(u3%): next u3%

L42110:     accept                                                       ~
               at (01,02),                                               ~
                  "Enter Physical Inventory Counts",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hex(ac)), ticketlit$             , ch(13),~
               at (05,16), fac(hex(ac)), partlit$               , ch(25),~
               at (05,42), fac(hex(ac)), qtylit$                , ch(10),~
               at (05,53), fac(hex(ac)), togglelit$             , ch(19),~
               at (05,73), fac(hex(ac)), datelit$               , ch(08),~
                                                                         ~
               at (06,02), fac(mfac$(01%,01%)), tickt2$    (01%)        ,~
               at (06,14), fac(mfac$(01%,01%)), recount$   (01%), ch(01),~
               at (06,16), fac(mfac$(01%,02%)), part$      (01%), ch(25),~
               at (06,42), fac(mfac$(01%,03%)), qty$       (01%), ch(10),~
               at (06,53), fac(mfac$(01%,04%)), toggle$    (01%), ch(19),~
               at (06,73), fac(mfac$(01%,05%)), count_date$(01%), ch(08),~
                                                                         ~
               at (07,02), fac(mfac$(02%,01%)), tickt2$    (02%)        ,~
               at (07,14), fac(mfac$(02%,01%)), recount$   (02%), ch(01),~
               at (07,16), fac(mfac$(02%,02%)), part$      (02%), ch(25),~
               at (07,42), fac(mfac$(02%,03%)), qty$       (02%), ch(10),~
               at (07,53), fac(mfac$(02%,04%)), toggle$    (02%), ch(19),~
               at (07,73), fac(mfac$(02%,05%)), count_date$(02%), ch(08),~
                                                                         ~
               at (08,02), fac(mfac$(03%,01%)), tickt2$    (03%)        ,~
               at (08,14), fac(mfac$(03%,01%)), recount$   (03%), ch(01),~
               at (08,16), fac(mfac$(03%,02%)), part$      (03%), ch(25),~
               at (08,42), fac(mfac$(03%,03%)), qty$       (03%), ch(10),~
               at (08,53), fac(mfac$(03%,04%)), toggle$    (03%), ch(19),~
               at (08,73), fac(mfac$(03%,05%)), count_date$(03%), ch(08),~
                                                                         ~
               at (09,02), fac(mfac$(04%,01%)), tickt2$    (04%)        ,~
               at (09,14), fac(mfac$(04%,01%)), recount$   (04%), ch(01),~
               at (09,16), fac(mfac$(04%,02%)), part$      (04%), ch(25),~
               at (09,42), fac(mfac$(04%,03%)), qty$       (04%), ch(10),~
               at (09,53), fac(mfac$(04%,04%)), toggle$    (04%), ch(19),~
               at (09,73), fac(mfac$(04%,05%)), count_date$(04%), ch(08),~
                                                                         ~
               at (10,02), fac(mfac$(05%,01%)), tickt2$    (05%)        ,~
               at (10,14), fac(mfac$(05%,01%)), recount$   (05%), ch(01),~
               at (10,16), fac(mfac$(05%,02%)), part$      (05%), ch(25),~
               at (10,42), fac(mfac$(05%,03%)), qty$       (05%), ch(10),~
               at (10,53), fac(mfac$(05%,04%)), toggle$    (05%), ch(19),~
               at (10,73), fac(mfac$(05%,05%)), count_date$(05%), ch(08),~
                                                                         ~
               at (11,02), fac(mfac$(06%,01%)), tickt2$    (06%)        ,~
               at (11,14), fac(mfac$(06%,01%)), recount$   (06%), ch(01),~
               at (11,16), fac(mfac$(06%,02%)), part$      (06%), ch(25),~
               at (11,42), fac(mfac$(06%,03%)), qty$       (06%), ch(10),~
               at (11,53), fac(mfac$(06%,04%)), toggle$    (06%), ch(19),~
               at (11,73), fac(mfac$(06%,05%)), count_date$(06%), ch(08),~
                                                                         ~
               at (12,02), fac(mfac$(07%,01%)), tickt2$    (07%)        ,~
               at (12,14), fac(mfac$(07%,01%)), recount$   (07%), ch(01),~
               at (12,16), fac(mfac$(07%,02%)), part$      (07%), ch(25),~
               at (12,42), fac(mfac$(07%,03%)), qty$       (07%), ch(10),~
               at (12,53), fac(mfac$(07%,04%)), toggle$    (07%), ch(19),~
               at (12,73), fac(mfac$(07%,05%)), count_date$(07%), ch(08),~
                                                                         ~
               at (13,02), fac(mfac$(08%,01%)), tickt2$    (08%)        ,~
               at (13,14), fac(mfac$(08%,01%)), recount$   (08%), ch(01),~
               at (13,16), fac(mfac$(08%,02%)), part$      (08%), ch(25),~
               at (13,42), fac(mfac$(08%,03%)), qty$       (08%), ch(10),~
               at (13,53), fac(mfac$(08%,04%)), toggle$    (08%), ch(19),~
               at (13,73), fac(mfac$(08%,05%)), count_date$(08%), ch(08),~
                                                                         ~
               at (14,02), fac(mfac$(09%,01%)), tickt2$    (09%)        ,~
               at (14,14), fac(mfac$(09%,01%)), recount$   (09%), ch(01),~
               at (14,16), fac(mfac$(09%,02%)), part$      (09%), ch(25),~
               at (14,42), fac(mfac$(09%,03%)), qty$       (09%), ch(10),~
               at (14,53), fac(mfac$(09%,04%)), toggle$    (09%), ch(19),~
               at (14,73), fac(mfac$(09%,05%)), count_date$(09%), ch(08),~
                                                                         ~
               at (15,02), fac(mfac$(10%,01%)), tickt2$    (10%)        ,~
               at (15,14), fac(mfac$(10%,01%)), recount$   (10%), ch(01),~
               at (15,16), fac(mfac$(10%,02%)), part$      (10%), ch(25),~
               at (15,42), fac(mfac$(10%,03%)), qty$       (10%), ch(10),~
               at (15,53), fac(mfac$(10%,04%)), toggle$    (10%), ch(19),~
               at (15,73), fac(mfac$(10%,05%)), count_date$(10%), ch(08),~
                                                                         ~
               at (16,02), fac(mfac$(11%,01%)), tickt2$    (11%)        ,~
               at (16,14), fac(mfac$(11%,01%)), recount$   (11%), ch(01),~
               at (16,16), fac(mfac$(11%,02%)), part$      (11%), ch(25),~
               at (16,42), fac(mfac$(11%,03%)), qty$       (11%), ch(10),~
               at (16,53), fac(mfac$(11%,04%)), toggle$    (11%), ch(19),~
               at (16,73), fac(mfac$(11%,05%)), count_date$(11%), ch(08),~
                                                                         ~
               at (17,02), fac(mfac$(12%,01%)), tickt2$    (12%)        ,~
               at (17,14), fac(mfac$(12%,01%)), recount$   (12%), ch(01),~
               at (17,16), fac(mfac$(12%,02%)), part$      (12%), ch(25),~
               at (17,42), fac(mfac$(12%,03%)), qty$       (12%), ch(10),~
               at (17,53), fac(mfac$(12%,04%)), toggle$    (12%), ch(19),~
               at (17,73), fac(mfac$(12%,05%)), count_date$(12%), ch(08),~
                                                                         ~
               at (18,02), fac(mfac$(13%,01%)), tickt2$    (13%)        ,~
               at (18,14), fac(mfac$(13%,01%)), recount$   (13%), ch(01),~
               at (18,16), fac(mfac$(13%,02%)), part$      (13%), ch(25),~
               at (18,42), fac(mfac$(13%,03%)), qty$       (13%), ch(10),~
               at (18,53), fac(mfac$(13%,04%)), toggle$    (13%), ch(19),~
               at (18,73), fac(mfac$(13%,05%)), count_date$(13%), ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (22,25), fac(hex(8c)),   pf10$                , ch(22),~
               at (23,02), "(1)Start Over",                              ~
               at (23,25), fac(hex(84)),   pf4$                 , ch(17),~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(84)), pf16$                  , ch(13),~
                                                                         ~
               keys(hex(0001040a0d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 10% then L43140
                  gosub toggle_uom
                  goto L42110

               if keyhit% <> 13% then L43140
                  call "MANUAL" ("HNYPIINP")
                  goto L42110

L43140:        if keyhit% <> 15% then L43171
                  call "PRNTSCRN"
                  goto L42110

L43171:        for u3% = 1% to 13%: ticket$(u3%)=tickt2$(u3%): next u3%
               if line% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return


        REM *************************************************************~
            *               S C R E E N   P A G E   3 A                 *~
            *-----------------------------------------------------------*~
            * Input Screen for Multiple Tickets w/ Cycle Count Vars     *~
            *************************************************************

            deffn'104(line%)
                  if line% = 0% then init(hex(84))mfac$()
                  line2$="Multiple Tickets Entry Mode for Count Session "~
                         & session_nbr$
                  str(line2$,62%) = "HNYPIINP: " & str(cms2v$,,8%)
                  for u3%=1% to 13%: tickt2$(u3%)=ticket$(u3%): next u3%

L44130:     accept                                                       ~
               at (01,02),                                               ~
                  "Enter Physical Inventory Counts",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(62),~
               at (04,75), fac(hex(8c)), varlit1$               , ch( 6),~
               at (05,02), fac(hex(ac)), ticketlit$             , ch(13),~
               at (05,16), fac(hex(ac)), partlit$               , ch(25),~
               at (05,42), fac(hex(ac)), qtylit$                , ch(10),~
               at (05,53), fac(hex(ac)), togglelit$             , ch(12),~
               at (05,66), fac(hex(ac)), datelit$               , ch(08),~
               at (05,75), fac(hex(ac)), varlit2$               , ch( 6),~
                                                                         ~
               at (06,02), fac(mfac$(01%,01%)), tickt2$    (01%)        ,~
               at (06,14), fac(mfac$(01%,01%)), recount$   (01%), ch(01),~
               at (06,16), fac(mfac$(01%,02%)), part$      (01%), ch(25),~
               at (06,42), fac(mfac$(01%,03%)), qty$       (01%), ch(10),~
               at (06,53), fac(mfac$(01%,04%)), toggle$    (01%), ch(12),~
               at (06,66), fac(mfac$(01%,05%)), count_date$(01%), ch(08),~
               at (06,75), fac(mfac$(01%,06%)), varreason$ (01%), ch(06),~
                                                                         ~
               at (07,02), fac(mfac$(02%,01%)), tickt2$    (02%)        ,~
               at (07,14), fac(mfac$(02%,01%)), recount$   (02%), ch(01),~
               at (07,16), fac(mfac$(02%,02%)), part$      (02%), ch(25),~
               at (07,42), fac(mfac$(02%,03%)), qty$       (02%), ch(10),~
               at (07,53), fac(mfac$(02%,04%)), toggle$    (02%), ch(12),~
               at (07,66), fac(mfac$(02%,05%)), count_date$(02%), ch(08),~
               at (07,75), fac(mfac$(02%,06%)), varreason$ (02%), ch(06),~
                                                                         ~
               at (08,02), fac(mfac$(03%,01%)), tickt2$    (03%)        ,~
               at (08,14), fac(mfac$(03%,01%)), recount$   (03%), ch(01),~
               at (08,16), fac(mfac$(03%,02%)), part$      (03%), ch(25),~
               at (08,42), fac(mfac$(03%,03%)), qty$       (03%), ch(10),~
               at (08,53), fac(mfac$(03%,04%)), toggle$    (03%), ch(12),~
               at (08,66), fac(mfac$(03%,05%)), count_date$(03%), ch(08),~
               at (08,75), fac(mfac$(03%,06%)), varreason$ (03%), ch(06),~
                                                                         ~
               at (09,02), fac(mfac$(04%,01%)), tickt2$    (04%)        ,~
               at (09,14), fac(mfac$(04%,01%)), recount$   (04%), ch(01),~
               at (09,16), fac(mfac$(04%,02%)), part$      (04%), ch(25),~
               at (09,42), fac(mfac$(04%,03%)), qty$       (04%), ch(10),~
               at (09,53), fac(mfac$(04%,04%)), toggle$    (04%), ch(12),~
               at (09,66), fac(mfac$(04%,05%)), count_date$(04%), ch(08),~
               at (09,75), fac(mfac$(04%,06%)), varreason$ (04%), ch(06),~
                                                                         ~
               at (10,02), fac(mfac$(05%,01%)), tickt2$    (05%)        ,~
               at (10,14), fac(mfac$(05%,01%)), recount$   (05%), ch(01),~
               at (10,16), fac(mfac$(05%,02%)), part$      (05%), ch(25),~
               at (10,42), fac(mfac$(05%,03%)), qty$       (05%), ch(10),~
               at (10,53), fac(mfac$(05%,04%)), toggle$    (05%), ch(12),~
               at (10,66), fac(mfac$(05%,05%)), count_date$(05%), ch(08),~
               at (10,75), fac(mfac$(05%,06%)), varreason$ (05%), ch(06),~
                                                                         ~
               at (11,02), fac(mfac$(06%,01%)), tickt2$    (06%)        ,~
               at (11,14), fac(mfac$(06%,01%)), recount$   (06%), ch(01),~
               at (11,16), fac(mfac$(06%,02%)), part$      (06%), ch(25),~
               at (11,42), fac(mfac$(06%,03%)), qty$       (06%), ch(10),~
               at (11,53), fac(mfac$(06%,04%)), toggle$    (06%), ch(12),~
               at (11,66), fac(mfac$(06%,05%)), count_date$(06%), ch(08),~
               at (11,75), fac(mfac$(06%,06%)), varreason$ (06%), ch(06),~
                                                                         ~
               at (12,02), fac(mfac$(07%,01%)), tickt2$    (07%)        ,~
               at (12,14), fac(mfac$(07%,01%)), recount$   (07%), ch(01),~
               at (12,16), fac(mfac$(07%,02%)), part$      (07%), ch(25),~
               at (12,42), fac(mfac$(07%,03%)), qty$       (07%), ch(10),~
               at (12,53), fac(mfac$(07%,04%)), toggle$    (07%), ch(12),~
               at (12,66), fac(mfac$(07%,05%)), count_date$(07%), ch(08),~
               at (12,75), fac(mfac$(07%,06%)), varreason$ (07%), ch(06),~
                                                                         ~
               at (13,02), fac(mfac$(08%,01%)), tickt2$    (08%)        ,~
               at (13,14), fac(mfac$(08%,01%)), recount$   (08%), ch(01),~
               at (13,16), fac(mfac$(08%,02%)), part$      (08%), ch(25),~
               at (13,42), fac(mfac$(08%,03%)), qty$       (08%), ch(10),~
               at (13,53), fac(mfac$(08%,04%)), toggle$    (08%), ch(12),~
               at (13,66), fac(mfac$(08%,05%)), count_date$(08%), ch(08),~
               at (13,75), fac(mfac$(08%,06%)), varreason$ (08%), ch(06),~
                                                                         ~
               at (14,02), fac(mfac$(09%,01%)), tickt2$    (09%)        ,~
               at (14,14), fac(mfac$(09%,01%)), recount$   (09%), ch(01),~
               at (14,16), fac(mfac$(09%,02%)), part$      (09%), ch(25),~
               at (14,42), fac(mfac$(09%,03%)), qty$       (09%), ch(10),~
               at (14,53), fac(mfac$(09%,04%)), toggle$    (09%), ch(12),~
               at (14,66), fac(mfac$(09%,05%)), count_date$(09%), ch(08),~
               at (14,75), fac(mfac$(09%,06%)), varreason$ (09%), ch(06),~
                                                                         ~
               at (15,02), fac(mfac$(10%,01%)), tickt2$    (10%)        ,~
               at (15,14), fac(mfac$(10%,01%)), recount$   (10%), ch(01),~
               at (15,16), fac(mfac$(10%,02%)), part$      (10%), ch(25),~
               at (15,42), fac(mfac$(10%,03%)), qty$       (10%), ch(10),~
               at (15,53), fac(mfac$(10%,04%)), toggle$    (10%), ch(12),~
               at (15,66), fac(mfac$(10%,05%)), count_date$(10%), ch(08),~
               at (15,75), fac(mfac$(10%,06%)), varreason$ (10%), ch(06),~
                                                                         ~
               at (16,02), fac(mfac$(11%,01%)), tickt2$    (11%)        ,~
               at (16,14), fac(mfac$(11%,01%)), recount$   (11%), ch(01),~
               at (16,16), fac(mfac$(11%,02%)), part$      (11%), ch(25),~
               at (16,42), fac(mfac$(11%,03%)), qty$       (11%), ch(10),~
               at (16,53), fac(mfac$(11%,04%)), toggle$    (11%), ch(12),~
               at (16,66), fac(mfac$(11%,05%)), count_date$(11%), ch(08),~
               at (16,75), fac(mfac$(11%,06%)), varreason$ (11%), ch(06),~
                                                                         ~
               at (17,02), fac(mfac$(12%,01%)), tickt2$    (12%)        ,~
               at (17,14), fac(mfac$(12%,01%)), recount$   (12%), ch(01),~
               at (17,16), fac(mfac$(12%,02%)), part$      (12%), ch(25),~
               at (17,42), fac(mfac$(12%,03%)), qty$       (12%), ch(10),~
               at (17,53), fac(mfac$(12%,04%)), toggle$    (12%), ch(12),~
               at (17,66), fac(mfac$(12%,05%)), count_date$(12%), ch(08),~
               at (17,75), fac(mfac$(12%,06%)), varreason$ (12%), ch(06),~
                                                                         ~
               at (18,02), fac(mfac$(13%,01%)), tickt2$    (13%)        ,~
               at (18,14), fac(mfac$(13%,01%)), recount$   (13%), ch(01),~
               at (18,16), fac(mfac$(13%,02%)), part$      (13%), ch(25),~
               at (18,42), fac(mfac$(13%,03%)), qty$       (13%), ch(10),~
               at (18,53), fac(mfac$(13%,04%)), toggle$    (13%), ch(12),~
               at (18,66), fac(mfac$(13%,05%)), count_date$(13%), ch(08),~
               at (18,75), fac(mfac$(13%,06%)), varreason$ (13%), ch(06),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (22,25), fac(hex(8c)),   pf10$                , ch(22),~
               at (23,02), "(1)Start Over",                              ~
               at (23,25), fac(hex(84)),   pf4$                 , ch(17),~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(84)), pf16$                  , ch(13),~
                                                                         ~
               keys(hex(0001040a0d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13% then L45210
                  call "MANUAL" ("HNYPIINP")
                  goto L44130

L45210:        if keyhit% <> 15% then L45242
                  call "PRNTSCRN"
                  goto L44130

L45242:        if keyhit% <> 10% then L45250
                  gosub toggle_uom
                  goto L44130

L45250:        for u3% = 1% to 13%: ticket$(u3%)=tickt2$(u3%): next u3%
               if line% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return


        REM *************************************************************~
            *               S C R E E N   P A G E   4                   *~
            *-----------------------------------------------------------*~
            * Input Screen for Fast Entry Mode w/ PI Vars               *~
            *************************************************************

            deffn'105(line%)
                  line2$="Count Sheets Fast Entry Mode for Count Session"~
                        & " " & session_nbr$
                  str(line2$,62%) = "HNYPIINP: " & str(cms2v$,,8%)
                  mat redim find_ticket$(1)l%

L47120:     accept                                                       ~
               at (01,02),                                               ~
                  "Enter Physical Inventory Counts",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hex(ac)), ticketlit$             , ch(13),~
               at (05,16), fac(hex(ac)), partlit$               , ch(25),~
               at (05,42), fac(hex(ac)), qtylit$                , ch(10),~
               at (05,53), fac(hex(ac)), togglelit$             , ch(19),~
               at (05,73), fac(hex(ac)), datelit$               , ch(08),~
                                                                         ~
               at (06,02), fac(mfac$(01%,01%)), ticket$    (01%)        ,~
               at (06,14), fac(mfac$(01%,01%)), recount$   (01%), ch(01),~
               at (06,16), fac(mfac$(01%,02%)), part$      (01%), ch(25),~
               at (06,42), fac(mfac$(01%,03%)), qty$       (01%), ch(10),~
               at (06,53), fac(mfac$(01%,04%)), toggle$    (01%), ch(19),~
               at (06,73), fac(mfac$(01%,05%)), count_date$(01%), ch(08),~
                                                                         ~
               at (07,02), fac(mfac$(02%,01%)), ticket$    (02%)        ,~
               at (07,14), fac(mfac$(02%,01%)), recount$   (02%), ch(01),~
               at (07,16), fac(mfac$(02%,02%)), part$      (02%), ch(25),~
               at (07,42), fac(mfac$(02%,03%)), qty$       (02%), ch(10),~
               at (07,53), fac(mfac$(02%,04%)), toggle$    (02%), ch(19),~
               at (07,73), fac(mfac$(02%,05%)), count_date$(02%), ch(08),~
                                                                         ~
               at (08,02), fac(mfac$(03%,01%)), ticket$    (03%)        ,~
               at (08,14), fac(mfac$(03%,01%)), recount$   (03%), ch(01),~
               at (08,16), fac(mfac$(03%,02%)), part$      (03%), ch(25),~
               at (08,42), fac(mfac$(03%,03%)), qty$       (03%), ch(10),~
               at (08,53), fac(mfac$(03%,04%)), toggle$    (03%), ch(19),~
               at (08,73), fac(mfac$(03%,05%)), count_date$(03%), ch(08),~
                                                                         ~
               at (09,02), fac(mfac$(04%,01%)), ticket$    (04%)        ,~
               at (09,14), fac(mfac$(04%,01%)), recount$   (04%), ch(01),~
               at (09,16), fac(mfac$(04%,02%)), part$      (04%), ch(25),~
               at (09,42), fac(mfac$(04%,03%)), qty$       (04%), ch(10),~
               at (09,53), fac(mfac$(04%,04%)), toggle$    (04%), ch(19),~
               at (09,73), fac(mfac$(04%,05%)), count_date$(04%), ch(08),~
                                                                         ~
               at (10,02), fac(mfac$(05%,01%)), ticket$    (05%)        ,~
               at (10,14), fac(mfac$(05%,01%)), recount$   (05%), ch(01),~
               at (10,16), fac(mfac$(05%,02%)), part$      (05%), ch(25),~
               at (10,42), fac(mfac$(05%,03%)), qty$       (05%), ch(10),~
               at (10,53), fac(mfac$(05%,04%)), toggle$    (05%), ch(19),~
               at (10,73), fac(mfac$(05%,05%)), count_date$(05%), ch(08),~
                                                                         ~
               at (11,02), fac(mfac$(06%,01%)), ticket$    (06%)        ,~
               at (11,14), fac(mfac$(06%,01%)), recount$   (06%), ch(01),~
               at (11,16), fac(mfac$(06%,02%)), part$      (06%), ch(25),~
               at (11,42), fac(mfac$(06%,03%)), qty$       (06%), ch(10),~
               at (11,53), fac(mfac$(06%,04%)), toggle$    (06%), ch(19),~
               at (11,73), fac(mfac$(06%,05%)), count_date$(06%), ch(08),~
                                                                         ~
               at (12,02), fac(mfac$(07%,01%)), ticket$    (07%)        ,~
               at (12,14), fac(mfac$(07%,01%)), recount$   (07%), ch(01),~
               at (12,16), fac(mfac$(07%,02%)), part$      (07%), ch(25),~
               at (12,42), fac(mfac$(07%,03%)), qty$       (07%), ch(10),~
               at (12,53), fac(mfac$(07%,04%)), toggle$    (07%), ch(19),~
               at (12,73), fac(mfac$(07%,05%)), count_date$(07%), ch(08),~
                                                                         ~
               at (13,02), fac(mfac$(08%,01%)), ticket$    (08%)        ,~
               at (13,14), fac(mfac$(08%,01%)), recount$   (08%), ch(01),~
               at (13,16), fac(mfac$(08%,02%)), part$      (08%), ch(25),~
               at (13,42), fac(mfac$(08%,03%)), qty$       (08%), ch(10),~
               at (13,53), fac(mfac$(08%,04%)), toggle$    (08%), ch(19),~
               at (13,73), fac(mfac$(08%,05%)), count_date$(08%), ch(08),~
                                                                         ~
               at (14,02), fac(mfac$(09%,01%)), ticket$    (09%)        ,~
               at (14,14), fac(mfac$(09%,01%)), recount$   (09%), ch(01),~
               at (14,16), fac(mfac$(09%,02%)), part$      (09%), ch(25),~
               at (14,42), fac(mfac$(09%,03%)), qty$       (09%), ch(10),~
               at (14,53), fac(mfac$(09%,04%)), toggle$    (09%), ch(19),~
               at (14,73), fac(mfac$(09%,05%)), count_date$(09%), ch(08),~
                                                                         ~
               at (15,02), fac(mfac$(10%,01%)), ticket$    (10%)        ,~
               at (15,14), fac(mfac$(10%,01%)), recount$   (10%), ch(01),~
               at (15,16), fac(mfac$(10%,02%)), part$      (10%), ch(25),~
               at (15,42), fac(mfac$(10%,03%)), qty$       (10%), ch(10),~
               at (15,53), fac(mfac$(10%,04%)), toggle$    (10%), ch(19),~
               at (15,73), fac(mfac$(10%,05%)), count_date$(10%), ch(08),~
                                                                         ~
               at (16,02), fac(mfac$(11%,01%)), ticket$    (11%)        ,~
               at (16,14), fac(mfac$(11%,01%)), recount$   (11%), ch(01),~
               at (16,16), fac(mfac$(11%,02%)), part$      (11%), ch(25),~
               at (16,42), fac(mfac$(11%,03%)), qty$       (11%), ch(10),~
               at (16,53), fac(mfac$(11%,04%)), toggle$    (11%), ch(19),~
               at (16,73), fac(mfac$(11%,05%)), count_date$(11%), ch(08),~
                                                                         ~
               at (17,02), fac(mfac$(12%,01%)), ticket$    (12%)        ,~
               at (17,14), fac(mfac$(12%,01%)), recount$   (12%), ch(01),~
               at (17,16), fac(mfac$(12%,02%)), part$      (12%), ch(25),~
               at (17,42), fac(mfac$(12%,03%)), qty$       (12%), ch(10),~
               at (17,53), fac(mfac$(12%,04%)), toggle$    (12%), ch(19),~
               at (17,73), fac(mfac$(12%,05%)), count_date$(12%), ch(08),~
                                                                         ~
               at (18,02), fac(mfac$(13%,01%)), ticket$    (13%)        ,~
               at (18,14), fac(mfac$(13%,01%)), recount$   (13%), ch(01),~
               at (18,16), fac(mfac$(13%,02%)), part$      (13%), ch(25),~
               at (18,42), fac(mfac$(13%,03%)), qty$       (13%), ch(10),~
               at (18,53), fac(mfac$(13%,04%)), toggle$    (13%), ch(19),~
               at (18,73), fac(mfac$(13%,05%)), count_date$(13%), ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (22,25), fac(hex(8c)),   pf10$                , ch(22),~
               at (23,02), "(1)Start Over",                              ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,25), fac(hex(84)),   pf8$                 , ch(15),~
               at (24,41), fac(lfac$(1)),  find_ticket$(1)              ,~
               at (24,65), fac(hex(84)), pf16$                  , ch(13),~
                                                                         ~
               keys(hex(0001080a0d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 10% then L48150
                  gosub toggle_uom
                  goto L47120

L48150:        if keyhit% <> 13% then L48190
                  call "MANUAL" ("HNYPIINP")
                  goto L47120

L48190:        if keyhit% <> 15% then L48230
                  call "PRNTSCRN"
                  goto L47120

L48230:        mat redim find_ticket$(1)12

               if line% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   4 A                 *~
            *-----------------------------------------------------------*~
            * Input Screen for Fast Entry Mode w/ Cycle Count Vars      *~
            *************************************************************

            deffn'106(line%)
                  line2$="Count Sheets Fast Entry Mode for Count Session"~
                        & " " & session_nbr$
                  str(line2$,62%) = "HNYPIINP: " & str(cms2v$,,8%)
                  mat redim find_ticket$(1)l%

L48420:     accept                                                       ~
               at (01,02),                                               ~
                  "Enter Physical Inventory Counts",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(62),~
               at (04,75), fac(hex(8c)), varlit1$               , ch( 6),~
               at (05,02), fac(hex(ac)), ticketlit$             , ch(13),~
               at (05,16), fac(hex(ac)), partlit$               , ch(25),~
               at (05,42), fac(hex(ac)), qtylit$                , ch(10),~
               at (05,53), fac(hex(ac)), togglelit$             , ch(12),~
               at (05,66), fac(hex(ac)), datelit$               , ch(08),~
               at (05,75), fac(hex(ac)), varlit2$               , ch( 6),~
                                                                         ~
               at (06,02), fac(mfac$(01%,01%)), ticket$    (01%)        ,~
               at (06,14), fac(mfac$(01%,01%)), recount$   (01%), ch(01),~
               at (06,16), fac(mfac$(01%,02%)), part$      (01%), ch(25),~
               at (06,42), fac(mfac$(01%,03%)), qty$       (01%), ch(10),~
               at (06,53), fac(mfac$(01%,04%)), toggle$    (01%), ch(12),~
               at (06,66), fac(mfac$(01%,05%)), count_date$(01%), ch(08),~
               at (06,75), fac(mfac$(01%,06%)), varreason$ (01%), ch(06),~
                                                                         ~
               at (07,02), fac(mfac$(02%,01%)), ticket$    (02%)        ,~
               at (07,14), fac(mfac$(02%,01%)), recount$   (02%), ch(01),~
               at (07,16), fac(mfac$(02%,02%)), part$      (02%), ch(25),~
               at (07,42), fac(mfac$(02%,03%)), qty$       (02%), ch(10),~
               at (07,53), fac(mfac$(02%,04%)), toggle$    (02%), ch(12),~
               at (07,66), fac(mfac$(02%,05%)), count_date$(02%), ch(08),~
               at (07,75), fac(mfac$(02%,06%)), varreason$ (02%), ch(06),~
                                                                         ~
               at (08,02), fac(mfac$(03%,01%)), ticket$    (03%)        ,~
               at (08,14), fac(mfac$(03%,01%)), recount$   (03%), ch(01),~
               at (08,16), fac(mfac$(03%,02%)), part$      (03%), ch(25),~
               at (08,42), fac(mfac$(03%,03%)), qty$       (03%), ch(10),~
               at (08,53), fac(mfac$(03%,04%)), toggle$    (03%), ch(12),~
               at (08,66), fac(mfac$(03%,05%)), count_date$(03%), ch(08),~
               at (08,75), fac(mfac$(03%,06%)), varreason$ (03%), ch(06),~
                                                                         ~
               at (09,02), fac(mfac$(04%,01%)), ticket$    (04%)        ,~
               at (09,14), fac(mfac$(04%,01%)), recount$   (04%), ch(01),~
               at (09,16), fac(mfac$(04%,02%)), part$      (04%), ch(25),~
               at (09,42), fac(mfac$(04%,03%)), qty$       (04%), ch(10),~
               at (09,53), fac(mfac$(04%,04%)), toggle$    (04%), ch(12),~
               at (09,66), fac(mfac$(04%,05%)), count_date$(04%), ch(08),~
               at (09,75), fac(mfac$(04%,06%)), varreason$ (04%), ch(06),~
                                                                         ~
               at (10,02), fac(mfac$(05%,01%)), ticket$    (05%)        ,~
               at (10,14), fac(mfac$(05%,01%)), recount$   (05%), ch(01),~
               at (10,16), fac(mfac$(05%,02%)), part$      (05%), ch(25),~
               at (10,42), fac(mfac$(05%,03%)), qty$       (05%), ch(10),~
               at (10,53), fac(mfac$(05%,04%)), toggle$    (05%), ch(12),~
               at (10,66), fac(mfac$(05%,05%)), count_date$(05%), ch(08),~
               at (10,75), fac(mfac$(05%,06%)), varreason$ (05%), ch(06),~
                                                                         ~
               at (11,02), fac(mfac$(06%,01%)), ticket$    (06%)        ,~
               at (11,14), fac(mfac$(06%,01%)), recount$   (06%), ch(01),~
               at (11,16), fac(mfac$(06%,02%)), part$      (06%), ch(25),~
               at (11,42), fac(mfac$(06%,03%)), qty$       (06%), ch(10),~
               at (11,53), fac(mfac$(06%,04%)), toggle$    (06%), ch(12),~
               at (11,66), fac(mfac$(06%,05%)), count_date$(06%), ch(08),~
               at (11,75), fac(mfac$(06%,06%)), varreason$ (06%), ch(06),~
                                                                         ~
               at (12,02), fac(mfac$(07%,01%)), ticket$    (07%)        ,~
               at (12,14), fac(mfac$(07%,01%)), recount$   (07%), ch(01),~
               at (12,16), fac(mfac$(07%,02%)), part$      (07%), ch(25),~
               at (12,42), fac(mfac$(07%,03%)), qty$       (07%), ch(10),~
               at (12,53), fac(mfac$(07%,04%)), toggle$    (07%), ch(12),~
               at (12,66), fac(mfac$(07%,05%)), count_date$(07%), ch(08),~
               at (12,75), fac(mfac$(07%,06%)), varreason$ (07%), ch(06),~
                                                                         ~
               at (13,02), fac(mfac$(08%,01%)), ticket$    (08%)        ,~
               at (13,14), fac(mfac$(08%,01%)), recount$   (08%), ch(01),~
               at (13,16), fac(mfac$(08%,02%)), part$      (08%), ch(25),~
               at (13,42), fac(mfac$(08%,03%)), qty$       (08%), ch(10),~
               at (13,53), fac(mfac$(08%,04%)), toggle$    (08%), ch(12),~
               at (13,66), fac(mfac$(08%,05%)), count_date$(08%), ch(08),~
               at (13,75), fac(mfac$(08%,06%)), varreason$ (08%), ch(06),~
                                                                         ~
               at (14,02), fac(mfac$(09%,01%)), ticket$    (09%)        ,~
               at (14,14), fac(mfac$(09%,01%)), recount$   (09%), ch(01),~
               at (14,16), fac(mfac$(09%,02%)), part$      (09%), ch(25),~
               at (14,42), fac(mfac$(09%,03%)), qty$       (09%), ch(10),~
               at (14,53), fac(mfac$(09%,04%)), toggle$    (09%), ch(12),~
               at (14,66), fac(mfac$(09%,05%)), count_date$(09%), ch(08),~
               at (14,75), fac(mfac$(09%,06%)), varreason$ (09%), ch(06),~
                                                                         ~
               at (15,02), fac(mfac$(10%,01%)), ticket$    (10%)        ,~
               at (15,14), fac(mfac$(10%,01%)), recount$   (10%), ch(01),~
               at (15,16), fac(mfac$(10%,02%)), part$      (10%), ch(25),~
               at (15,42), fac(mfac$(10%,03%)), qty$       (10%), ch(10),~
               at (15,53), fac(mfac$(10%,04%)), toggle$    (10%), ch(12),~
               at (15,66), fac(mfac$(10%,05%)), count_date$(10%), ch(08),~
               at (15,75), fac(mfac$(10%,06%)), varreason$ (10%), ch(06),~
                                                                         ~
               at (16,02), fac(mfac$(11%,01%)), ticket$    (11%)        ,~
               at (16,14), fac(mfac$(11%,01%)), recount$   (11%), ch(01),~
               at (16,16), fac(mfac$(11%,02%)), part$      (11%), ch(25),~
               at (16,42), fac(mfac$(11%,03%)), qty$       (11%), ch(10),~
               at (16,53), fac(mfac$(11%,04%)), toggle$    (11%), ch(12),~
               at (16,66), fac(mfac$(11%,05%)), count_date$(11%), ch(08),~
               at (16,75), fac(mfac$(11%,06%)), varreason$ (11%), ch(06),~
                                                                         ~
               at (17,02), fac(mfac$(12%,01%)), ticket$    (12%)        ,~
               at (17,14), fac(mfac$(12%,01%)), recount$   (12%), ch(01),~
               at (17,16), fac(mfac$(12%,02%)), part$      (12%), ch(25),~
               at (17,42), fac(mfac$(12%,03%)), qty$       (12%), ch(10),~
               at (17,53), fac(mfac$(12%,04%)), toggle$    (12%), ch(12),~
               at (17,66), fac(mfac$(12%,05%)), count_date$(12%), ch(08),~
               at (17,75), fac(mfac$(12%,06%)), varreason$ (12%), ch(06),~
                                                                         ~
               at (18,02), fac(mfac$(13%,01%)), ticket$    (13%)        ,~
               at (18,14), fac(mfac$(13%,01%)), recount$   (13%), ch(01),~
               at (18,16), fac(mfac$(13%,02%)), part$      (13%), ch(25),~
               at (18,42), fac(mfac$(13%,03%)), qty$       (13%), ch(10),~
               at (18,53), fac(mfac$(13%,04%)), toggle$    (13%), ch(12),~
               at (18,66), fac(mfac$(13%,05%)), count_date$(13%), ch(08),~
               at (18,75), fac(mfac$(13%,06%)), varreason$ (13%), ch(06),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65), "(13)Instructions",                           ~
               at (22,25), fac(hex(8c)),   pf10$                , ch(22),~
               at (23,02), "(1)Start Over",                              ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,25), fac(hex(84)),   pf8$                 , ch(15),~
               at (24,41), fac(lfac$(1)),  find_ticket$(1)              ,~
               at (24,65), fac(hex(84)), pf16$                  , ch(13),~
                                                                         ~
               keys(hex(0001080a0d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 10% then L49450
                  gosub toggle_uom
                  goto L48420

L49450:        if keyhit% <> 13% then L49490
                  call "MANUAL" ("HNYPIINP")
                  goto L48420

L49490:        if keyhit% <> 15% then L49530
                  call "PRNTSCRN"
                  goto L48420

L49530:        mat redim find_ticket$(1)12

               if line% > 0% then return
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
                  on fieldnr% gosub L50100          /* Count Session Num*/
                     return
L50100:     REM Test Data for Count Session Number
                call "GETCODE" (#1, session_nbr$,session_nbrdescr$, 1%,  ~
                                0, f1%(1))
                if f1%(1) = 0% then L50180
                if f2%(5) <> 0% then L50170 /* No HNYQUAN so not check on*/
                                                       /* HNYPICST File */

                plowkey$ = str(session_nbr$)
                call "PLOWNEXT" (#7, plowkey$, 2%, f1%(7))
                if f1%(7) = 1% then L50170
                     goto no_cost_file   /* Gota leave */

L50170:         gosub dataload   :  return
L50180:         errormsg$ =    "SESSION UNDEFINED OR NONE ON FILE"
                return

         no_cost_file
            keyhit% = 2%
            call "ASKUSER" (keyhit%, "** NO COST PICTURE FILE **",       ~
                 "No Cost Picture has been done for this session."     , ~
                 "You must capture the current cost via HNYPICAP."     , ~
                 "Press Any Key to Exit    "  )
            goto  exit_program

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51110,         /* Ticket Number    */~
                                    L51175,         /* Part Code        */~
                                    L51280,         /* Warehouse        */~
                                    L51395,         /* Lot              */~
                                    L51495,         /* Location         */~
                                    L51640,         /* Quantity Counted */~
                                    L51770,         /* Counted By       */~
                                    L51785,         /* Date Counted     */~
                                    L51840,         /* Variance Reason  */~
                                    L51900,         /* Update Inventory */~
                                    L51920,         /* Post G/L Variance*/~
                                    L51940          /* Override Unit Cos*/
                  lastpsl$ = str(part$) & str(store$) & lot$
                  return
L51110:     REM Test Data for Ticket Number
                call "NUMTEST" (recount$,0,9,errormsg$,0,recount)
                if errormsg$ > " " then return
                recount% = recount
                readkey$ = str(session_nbr$) & str(ticket$) &            ~
                           bin(99-recount,1)
                call "READ100" (#2, readkey$, f1%(2%))
                if f1%(2%) = 1% then L51160
                   errormsg$ = "Sorry, Ticket Not on File!"
                   return
L51160:         gosub load_ticket
                if extra$ <> " " then L51165
                     fieldnr% = 5%  :  return
L51165:         if part$ <> " " then  fieldnr% = 13%
                return
L51175:     REM Test Data for Part Code
                if part$ = " " then return
                newpart_flag$ = "N"    /* Start Default */
                if part$ = str(key(#2,1),,25%) then L51220
                   if extra$ > " " then  L51215                           ~
                                   else                                  ~
                       errormsg$ = "Wrong Part for this Ticket - Re-Enter"
                   return
L51215:         newpart_flag$ = "Y"
L51220:         call "READ100" (#3, part$, f1%(3%))
                if f1%(3%) = 1% then L51240
                errormsg$ = "Either Mis-Key or Part Not on File - If Latt~
        ~er Exit & Add Part"     :   return
L51240:         get #3 using L36300,  partdescr$, uom$
                if ccsession$ = " " then L51248  /*else test for CC Part */
                   plowkey$ = str(ccsession$) & "A" & str(part$) & hex(00)
                   call "PLOWNEXT" (#24, plowkey$, 38%, f1%(24%))
                   if f1%(24%) = 1% then L51248
                       errormsg$ = "Either Mis-Key or Part Not in this"  ~
                                    & " Cycle Count Session"
L51248:         if newpart_flag$ <> "Y" or errormsg$ <> " " then return
                first_field% = 3%
                init(" ") store$, storedescr$, lot$, loc$, qty$, hvar$,  ~
                          gvar$, caseq$(), cases$(), recount$, varreason$
                goto reenter_input
                return
L51280:     REM Test Data for Store/Warehouse
                if extra$ = " " then return
                if part$ = " " then return
                call "DESCRIBE" (#4, store$, storedescr$, 1%, f1%(4))
                if f1%(4) <> 0% then L51315
                    errormsg$ = "Warehouse Not Found in Store Master File"
                    return
L51315
*              IF STORE$ <> STR(KEY(#2,1),26,3) AND MODE% = 2% THEN 51270
                if store$ <> str(lastpsl$,26,3) and mode% = 2% then L51368
                /* Test for already used in another Session */
                    plowkey$ = str(part$) & str(store$) & hex(00)
L51335:             call "PLOWALTS" (#2, plowkey$, 1%, 28%, f1%(2%))
                    if f1%(2%) = 0% then L51359
                       if str(key(#2),,2%) = session_nbr$ then L51335
                       errormsg$ = "This PART / STORE Combination alrea"&~
                             "dy assigned to Session: " & str(key(#2),,2%)
                       return
L51359:         if ccsession$ = " " then return /*else test for CC Part */
                    plowkey$ = str(ccsession$) & "A" & str(part$) &      ~
                                 str(store$) & hex(00)
                    call "PLOWNEXT" (#24, plowkey$, 41%, f1%(24%))
                    if f1%(24%) = 1% then return
                        errormsg$ = "Either Mis-Key or Part/Store not in"~
                                    & " this Cycle Count Session"
                        return
L51368:         /* Store has changed */
                first_field% = 4%
                init(" ") lot$, loc$, qty$, hvar$,gvar$, caseq$(),       ~
                          cases$(), recount$, varreason$
                goto reenter_input

L51395:     REM Test Data for Lot
                if lot_enable% = 0% then L51482
                if lot_enable% <> 2% or lot$ <> " " then L51413
                     errormsg$ = "Lot Number required for this Part."
                     return
L51413:         if ccsession$ = " " then L51422  /*else test for CC Part */
                    plowkey$ = str(ccsession$) & "A" & str(part$)  &     ~
                                  str(store$) & str(lot$) & hex(00)
                    call "PLOWNEXT" (#24, plowkey$, 47%, f1%(24%))
                    if f1%(24%) = 1% then L51422
                        errormsg$ = "Either Mis-Key or Part Not in this" ~
                                    & " Cycle Count Session"
                        return

L51422:         plowkey$ = str(part$) & str(store$) & lot$
                call "READ100" (#5, plowkey$, f1%(5))
                if f1%(5) = 1% then L51465
                     call "LOTVALID" (part$, store$, lot$, #6, #3, #5,   ~
                                      errormsg$)
                     if errormsg$ > " " then return
*              IF LOT$ = STR(KEY(#2,1),29,6) OR MODE% = 1% THEN RETURN
                if lot$ = str(lastpsl$,29,6) or mode% = 1% then L51482
L51465:         /* Lot has changed */
                init(" ") qty$, hvar$,gvar$, override$,                  ~
                          caseq$(), cases$(), recount$, varreason$
L51482:         gosub load_book_values
                first_field% = 5%
                goto reenter_input

L51495:     REM Test Data for Loc - don't want to count same thing twice
                if extra$ = " " then return
                if part$ = " " then return
                plowkey$ = str(part$) & str(store$) & str(lot$) & hex(00)
L51515:         call "PLOWALTS" (#2, plowkey$, 1%, 44%, f1%(2%))
                if f1%(2%) = 0% then L51595
                   if str(key(#2),,14%) = str(readkey$,,14%) then L51515
                   get #2 using L51535, lotno$, location$
L51535:            FMT POS(44), CH(16), CH(8)
                   if lot_or_loc$ = "L" then L51570
                         errormsg$="Qty for this Part, Store, & Lot " &  ~
                                   " already recorded on ticket " &      ~
                                   str(key(#2),3%,12%)
                         fieldnr% = 4%
                         goto L51630
L51570:            if location$ <> loc$ then L51515
                   errormsg$="Qty for this Part, Store, Lot, & Loc"      ~
                           & " already recorded on ticket " &            ~
                             str(key(#2),3%,12%)
                         goto L51630
L51595:         if loc$ = " " then L51630
                if loc_valid$ = "N" then L51630
                if fs%(8) <> 1% then L51630
                call "READ100" (#8, str(store$) & str(loc$), f1%(8))
                     if f1%(8) = 1% then L51630
                     errormsg$ = "Location " & loc$ & " Not Defined " &  ~
                                 "for Warehouse " & store$
L51630:            call "READ100" (#2, readkey$, f1%(2%))
                   return
L51640: REM Test Data for Quantity Counted
            if cases$() > " " then L51660
               errormsg$ = "Quantity is Required Field!"
               return
L51660:     if str(cases$(),,1%) <> "V" then L51685
               qty$, cases$() = "VOID"
               caseq$() = " "
               qty = -1
               return
L51685:     qty, ttt  =  0
            for i% = 1% to 5%
                if cases$(i%) <> " " then L51705
                     caseq$(i%) = " " : goto L51735
L51705:         if caseq$(i%) = " " then caseq$(i%) = "1"
                call "NUMTEST" (cases$(i%),0,999999999,errormsg$,-.2,sss)
                     if errormsg$ <> " " then return
                call "NUMTEST" (caseq$(i%),.01, 99999, errormsg$, .2,qqq)
                     if errormsg$ <> " " then return
                qty = qty + sss*qqq
L51735:     next i%
            call "CONVERT"  (qty, -0.2, qty$)
            call "NUMTEST"  (qty$, 0, 9e7, errormsg$, 0, ttt)
               if errormsg$ <> " "  then return
            if extra$ > " " and part$ = " "  then errormsg$ =            ~
                "Either VOID the Ticket or Start Over & Enter a Part #"
            return
L51770:     REM Test Data for Counted By
                if by$ > " " then lastby$ = by$
                return
L51785:     REM Test Data for Date Counted
                if extra$ > " " and part$ = " " then return
                if qty$ = "VOID" then return
                call "DATEOK" (count_date$,u3%,errormsg$)
                if errormsg$ > " " then return
                call "DATUNFMT" (count_date$)
                if count_date$ > date then errormsg$ =                   ~
                   "Count Date may not be greater than Today"
                call "DATEFMT" (count_date$)
                if errormsg$ = " " then lastdate$ = count_date$
                return
L51840:     REM Test for Variance Reason Code
        validate_varreason
            if varreason$ = " " then return
            if varreason$ = "?" then varreason$  =  all(hex(20))
                plowkey$ = "VARREASON" &  varreason$
                descr$   = hex(06) & "Select Variance Reason Code"
                call "PLOWCODE" (#9, plowkey$, descr$, 9%, .3, f1%(9))
                if f1%(9) = 1% then L51890
                     errormsg$ = "Sorry, Variance Reason not on File"
                     return
L51890:         varreason$ = str(plowkey$, 10,6)
                return
L51900:     REM Test Data for Update Inventory Qty's ?
                if hvar$ = "Y" or hvar$ = "N" or hvar$ = " " then return
                errormsg$ = "Must be Y, N, or blank"
                return
L51920:     REM Test Data for Post G/L Variance $    ?
                if gvar$ = "Y" or gvar$ = "N" or gvar$ = " " then return
                errormsg$ = "Must be Y, N, or blank"
                return
L51940:     REM Test Data for Override Unit Costs    ?
                if override$  = "N" then return
                if override$ <> "Y" then L51980
                     if tot_cost <> 0 then return
                     if mode% = 2% then return
                     call "HNYCDIST" ("E", part$, partdescr$,            ~
                            str(line2$,,60), #6, cost$, tot_cost$,       ~
                            tot_cost)
                     return
L51980:         errormsg$ = "Must be Y, or N"
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 3 & 3A.                 *~
            *************************************************************

            deffn'153(line%)
              errormsg$ = " "
              if ticket$(line%)=" " and part$(line%)=" " and qty$(line%) ~
                 =" " and by$(line%)=" " and (count_date$(line%) = " " or~
                 count_date$(line%) = blankdate$) ~
                 then return

              for fieldnr% = 1% to n%
                  mfac$(line%,fieldnr%) = and hex(ef)
                  on fieldnr% gosub L52100,         /* Ticket No.       */~
                                    L52200,         /* Part Code        */~
                                    L52300,         /* Quantity         */~
                                    L52400,         /* Counted By       */~
                                    L52500,         /* Count Date       */~
                                    L52600          /* Variance Reason  */
                  if fieldnr% = 1% and errormsg$ > " " then fieldnr% = 6%
              next fieldnr%
              return

L52100:     REM Test Data for Ticket No.
                call "NUMTEST" (recount$(line%),0,9,error$,0,recount)
                if error$ > " " then errormsg$ = error$
                if error$ > " " then L52920
                readkey$ = str(session_nbr$) & str(ticket$(line%)) &     ~
                           bin(99-recount,1)
                call "READ100" (#2, readkey$, f1%(2%))
                if f1%(2) = 1% then L52155
                   errormsg$ = "Sorry, Ticket Not on File!"
                   goto L52920
L52155:         if part_req$ <> "Y" then part$(line%) = key(#2,1)
                if str(key(#2,1),,25%) > " " then L52168
                   errormsg$ = "Sorry, this ticket must be entered via " ~
                             & "the One Ticket Screen"
                   goto L52920
L52168:         for x% = 1% to 13%
                    if x% = line% then L52184
                    if ticket$(line%) <> ticket$(x%) or                  ~
                       recount$(line%) <> recount$(x%) then L52184
                       errormsg$ = "Sorry, you may not " &               ~
                                   "enter the same ticket twice."
                       gosub L52920
                       return
L52184:         next x%
                if errormsg$ = " " then gosub get_uom_value
                return
L52200:     REM Test Data for Part Code
                if part_req$ <> "Y" then return
                if f1%(2) = 0% then return
                if part$(line%) = str(key(#2,1),,25%) then L52250
                   errormsg$ = "Wrong Part for this Ticket - Try Again"
                   goto L52920
L52250:         call "READ100" (#3, part$(line%), f1%(3))
                if f1%(3) = 1% then return
                errormsg$ = "Either Mis-Key or Part Not on File - If Latt~
        ~er Exit & Add Part"
                goto L52920
L52300:     REM Test Data for Quantity
                if qty$(line%) > " " then L52310
                   errormsg$ = "Quantity is a Required Field !"
                   goto L52920
L52310:         if str(qty$(line%),,1%) <> "V" then L52350
                   qty$(line%) = "VOID"
                   return
L52350:         call "NUMTEST" (qty$(line%),0,999999999,error$,.2,qty)
                if error$ > " " then errormsg$ = error$
                if error$ > " " then L52920
                return
L52400:     REM Test Data for Counted By
                if by$(line%) > " " then lastby$ = by$(line%)
                if by$(line%) = " " and line% > 1% then by$(line%) =     ~
                   by$(line%-1%)
                return
L52500:     REM Test Data for Count Date
                error$ = " "
                if(count_date$(line%) = " " or ~
                   count_date$(line%) = blankdate$) and line% > 1% then  ~
                   count_date$(line%) = count_date$(line%-1%)
                if count_date$(line%) = " " or ~
                   count_date$(line%) = blankdate$ then L52560
                call "DATEOK" (count_date$(line%),u3%,error$)
                if error$ > " " then errormsg$ = error$
                if error$ > " " then L52920
                call "DATUNFMT" (count_date$(line%))
                if count_date$(line%)  > date then error$ =              ~
                   "Count Date may not be greater than Today"
                if error$ > " " then errormsg$ = error$
                call "DATEFMT" (count_date$(line%))
L52560:         if error$ = " " and count_date$(line%) <> " " and        ~
                   count_date$(line%) <> blankdate$                      ~
                   then lastdate$ = count_date$(line%)
                if error$ > " " then L52920
                return
L52600:     REM Test for Variance Reason Code
                varreason$ = varreason$(line%)
                gosub validate_varreason
                varreason$(line%) = varreason$
                return
            REM Set Field Fac to Blink
L52920:         mfac$(line%,fieldnr%) = or hex(10)
                return

        get_uom_value
            call "READ100" (#3, key(#2,1), f1%(3))
            if f1%(3) = 0  then return
            get #3 using  L52965, uom$(line%)
L52965:     FMT POS(74), CH(4)
            if uomflag$ = "Y" then toggle$(line%) = uom$(line%)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 4 & 4A.                 *~
            *************************************************************

            deffn'154(line%)
              errormsg$ = " "
              if line% > 0% then L53160
              for line% = 1% to 13%
                  if ticket$(line%) = " " then L53130
                  if qty$(line%) <> " " then L53120
                     if part_req$ = "Y" and part$(line%) > " " then L53120
                     init(" ") ticket$(line%), by$(line%), part$(line%), ~
                               count_date$(line%), recount$(line%),      ~
                               varreason$(line%)
                     init(hex(8c)) mfac$(line%,2%), mfac$(line%,3%),     ~
                                   mfac$(line%,4%), mfac$(line%,5%),     ~
                                   mfac$(line%,6%)
                     goto L53130
L53120:           gosub L53160
L53130:       next line%
              return

L53160:       if part$(line%) = " " and qty$(line%) = " " and            ~
                 by$(line%) = " "   and (count_date$(line%) = blankdate$ or ~
                                         count_date$(line%) = " ") then return

              for fieldnr% = 1% to n%
                  mfac$(line%,fieldnr%) = and hex(ef)
                  on fieldnr% gosub L53300,         /* Ticket No.       */~
                                    L53580,         /* Part Code        */~
                                    L53690,         /* Quantity         */~
                                    L53870,     /* Counted By/UOM Toggle*/~
                                    L53890,         /* Count Date       */~
                                    L54030          /* Variance Reason  */
                  if fieldnr% = 1% and errormsg$ > " " then fieldnr% = 6%
              next fieldnr%
              return

L53300:     REM Test Data for Ticket No.
                call "NUMTEST" (recount$(line%),0,9,error$,0,recount)
                if error$ > " " then errormsg$ = error$
                if error$ > " " then return
                readkey$ = str(session_nbr$) & str(ticket$(line%)) &     ~
                           bin(99-recount,1)
                call "READ100" (#2, readkey$, f1%(2%))
                if f1%(2%) = 1% then L53430
                   errormsg$ = "Sorry, Ticket Not on File!"
                   return
L53430:         if part_req$ <> "Y" then part$(line%) = key(#2,1)
                if str(key(#2,1),,25%) > " " then L53480
                   errormsg$ = "Sorry, this ticket must be entered via " ~
                             & "the One Ticket Screen"
                   return
L53480:         for x% = 1% to 13%
                    if x% = line% then L53560
                    if ticket$(line%) <> ticket$(x%) or                  ~
                       recount$(line%) <> recount$(x%) then L53560
                       errormsg$ = "Sorry, you may not " &               ~
                                   "enter the same ticket twice."
                       return
L53560:         next x%
                if errormsg$ = " " then gosub get_uom_value
                return
L53580:     REM Test Data for Part Code
                if part_req$ <> "Y" then return
                if f1%(2) = 0% then return
                if part$(line%) = str(key(#2,1),,25%) then L53640
                   errormsg$ = "Wrong Part for this Ticket - Try Again"
                   goto L52920
L53640
*              CALL "READ100" (#3, PART$(LINE%), F1%(3))
                if errormsg$ = " " then gosub get_uom_value
                if f1%(3) = 1% then return
                errormsg$ = "Either Mis-Key or Part Not on File - If Latt~
        ~er Exit & Add Part"
                goto L52920
L53690:     REM Test Data for Quantity
                if qty$(line%) > " " then L53730
                   errormsg$ = "Quantity is a Required Field !"
                   goto L52920
L53730:         if str(qty$(line%),,1%) <> "V" then L53830
                   qty$(line%) = "VOID"
                   return
L53830:         call "NUMTEST" (qty$(line%),0,999999999,error$,.2,qty)
                if error$ > " " then errormsg$ = error$
                if error$ > " " then L52920
                return
L53870:     REM Test Data for Counted By
                if by$(line%) > " " then lastby$ = by$(line%)            ~
                                    else by$(line%) = lastby$
                return
L53890:     REM Test Data for Count Date
                error$ = " "
                if count_date$(line%) = " " or ~
                   count_date$(line%) = blankdate$ then L53980
                call "DATEOK" (count_date$(line%),u3%,error$)
                if error$ > " " then errormsg$ = error$
                if error$ > " " then L52920
                call "DATUNFMT" (count_date$(line%))
                if count_date$(line%)  > date then error$ =              ~
                   "Count Date may not be greater than Today"
                if error$ > " " then errormsg$ = error$
                call "DATEFMT" (count_date$(line%))
L53980:         if error$ = " " and                      ~
                   count_date$(line%) <> " " and         ~
                   count_date$(line%) <> blankdate$ then ~
                          lastdate$ = count_date$(line%)
                if count_date$(line%) = " " or count_date$(line%) = blankdate$ ~
                                          then count_date$(line%) = lastdate$
                if error$ > " " then L52920
                return
            REM Test for Variance Reason Code
L54030:         varreason$ = varreason$(line%)
                gosub validate_varreason
                varreason$(line%) = varreason$
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
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program

            end
