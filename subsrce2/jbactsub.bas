        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  JJJJJ  BRRR     A     CCC   TTTTT   SSS   U   U  BBBB    *~
            *    J    B   B   A A   C   C    T    S      U   U  B   B   *~
            *    J    BBBB   AAAAA  C        T     SSS   U   U  BBBB    *~
            *  J J    B   B  A   A  C   C    T        S  U   U  B   B   *~
            *   J     BBBB   A   A   CCC     T     SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBACTSUB - Report actual movement of job part from work   *~
            *            center to work center.  Also allows entry of   *~
            *            actual set up and run times, who moved the     *~
            *            stuff, and quantitys scrapped & sent to rework.*~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/14/85 ! ORIGINAL (Re-write, changed to sub)      ! HES *~
            * 03/11/86 ! Slight structure changes                 ! HES *~
            * 10/26/86 ! Uses PLANRTE subroutine now              ! HES *~
            * 06/17/87 ! Standard Costing Changes                 ! ERN *~
            * 03/04/88 ! Now supports job specific travler        ! HES *~
            * 03/04/88 ! Now calls subroutine to purge WCOUTs     ! HES *~
            * 06/27/88 ! Changed call to WCDPURGE                 ! RJM *~
            *          ! Added new field to JBSTATUS file, JSFLAG$!     *~
            * 06/28/88 ! Added field to screen, Y/N Purge Wcouts? ! RJM *~
            *          ! Force 'TO W/C' to " " if to step = DONE  !     *~
            * 06/29/88 ! Changed Arqument List to default in some ! RJM *~
            *          !   data from JBTCINP (new parent).        !     *~
            *          !   AND, Setup & Run can be entered in     !     *~
            *          !   as hours or units.                     !     *~
            * 09/16/88 ! Mod to allow movement of Qty > Job Qty   ! MJB *~
            *          !   INTO first step to allow for yield     !     *~
            *          ! Changed handling of input for Purge WCOUT!     *~
            *          !   flag on first input screen.            !     *~
            * 10/04/88 ! Fixed input of Qty from Picking step     ! RJM *~
            * 08/13/90 ! Basic 4.3 & SSL Compatibility            ! KAB *~
            * 10/30/90 ! Changed large array variables to INIT    ! MJB *~
            * 05/28/91 ! ALLFREE.                                 ! JIM *~
            * 05/29/91 ! PRR 11895 fix PF(5) disabling other PFs. ! JIM *~
            * 04/08/92 ! PRR 11993, 12265. Fixed PF11 Add Line.   ! JDH *~
            *          ! PRR 12055. Calc Hrs from Unts @ DataLoad.!     *~
            * 05/07/93 ! Added reporting qty factor, entry of qty ! WPH *~
            *          ! moved in either job units or reporting   !     *~
            *          ! units, and new flag to indicate if step  !     *~
            *          ! is complete/closed. Changed rpt format.  !     *~
            *          ! Added new screen for setting program     !     *~
            *          ! defaults and made left/right             !     *~
            *          ! justification consistent.                !     *~
            * 06/28/94 ! Corrected bad branch to 65000s.          ! JDH *~
            * 07/07/94 ! 'From' step now always defaults from the ! JDH *~
            *          !   'To' step of the previous line.        !     *~
            * 07/13/94 ! Movement into a VEND step will ASKUSER if! RJH *~
            *          !  Shipment to Vendor s/b done via VSASHPSB!     *~
            * 07/16/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**~

            sub "JBACTSUB" (#1,               /* JBMASTR2 File UFB     */~
                            #2,               /* JBSTATUS File UFB     */~
                            #3,               /* HNYMASTR File UFB     */~
                            #4,               /* WCMASTR  File UFB     */~
                            #5,               /* PERMASTR File UFB     */~
                            #6,               /* GENCODES File UFB     */~
                            #7,               /* RTEMASTR File UFB     */~
                            #8,               /* JBCROSS2 File UFB     */~
                            #9,               /* BOMMASTR File UFB     */~
                            #10,              /* ENGMASTR File UFB     */~
                            #11,              /* SYSFILE2 File UFB     */~
                            injob$,           /* OPTIONAL JOB NUMBER   */~
                            inemp$,           /* OPTIONAL EMPLOYEE NO. */~
                            indate$,          /* OPTIONAL TRAN DATE    */~
                            intime$,          /* OPTIONAL TRAN TIME    */~
                            inhours)          /* OPTIONAL RUN HOURS    */~

        dim                                                              ~
            actv$(300,2)4,               /* FROM & TO ACTIVITY CODE    */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bomid$3,                     /* BOM ID PLANNED TO USE      */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            cj$10,                       /* CURRENT STEP QTY - job unit*/~
            cr$10,                       /* CURRENT STEP QTY - rpt unit*/~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            date$(300)8,                 /* TRANSACTION DATES          */~
            defstep$7,                   /* Default Step               */~
            descr$30,                    /* REPORT DESCRIPTION         */~
            dflt_runh$7,                 /* DEFAULT RUN HOURS          */~
            display$(3)79,               /* LINE ITMES FOR EDIT DISPLAY*/~
            div$(11)1,                   /* SCREEN DEVIDER             */~
            emp$(300)12,                 /* EMPLOYEE CODE              */~
            empdescr$30,                 /* EMPLOYEE NAME              */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            factor$30,                   /* TIME CONVERT FACTOR BY CNTR*/~
            filler$12,                   /* FILLER AT END OF RECORD    */~
            handfactor$(300)8,           /* Handling Units factor      */~
            header$79,                   /* HEADER FOR SCREEN          */~
            header1$79,                  /* HEADER FOR SCREEN          */~
            hfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            indate$(300)6,               /* LAST MOD DATE              */~
            indate$8,                    /* DEFAULT DATE OF MOVEMENT   */~
            inemp$12,                    /* DEFAULT EMPLOYEE CODE      */~
            injob$8,                     /* DEFAUTL JOB TO PROCESS     */~
            intime$8,                    /* DEFAULT TIME OF MOVEMENT   */~
            inuser$(300)4,               /* LAST MOD BY                */~
            jdate$(2)8,                  /* JOB START AND END DATES    */~
            jrfac$(2)1,                  /* FACs for units headers     */~
            job$8,                       /* JOB CODE                   */~
            jobdescr$32,                 /* JOB DESCRIPTION            */~
            jobpart$59,                  /* JOB PART TO BUILD          */~
            jobquan$10,                  /* JOB QUANTITY TO BUILD      */~
            jsflag$(300)1,               /* Purged Flag                */~
            lastdate$8,                                                  ~
            lfac$1,                      /* FIELD ATTRIBUTE CHARACTERS */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line$(300)4,                 /* LINE NUMBER FOR REFERENCE  */~
            line2$79,                    /* Screen line                */~
            message$79,                  /* INPUT MESSAGE              */~
            moved$(300)10,               /* QUANTITY MOVED             */~
            other$(11)9,                 /* SUMMARY SCREEN ONLY FIELD  */~
            paren$3,                     /* DEFAUT PARENTHESIS OPTION  */~
            part$25,                     /* JOB PART NUMBER            */~
            partdescr$34,                /* JOB PART DESCRIPTION       */~
            pfdescr$(3)79,               /* DESCRIPTION OF PFKEYS      */~
            plowkey$99,                  /* WORK VARIABLE              */~
            pfkeys$32,                   /* PF keys activation string  */~
            purg_fld_fac$1,              /* Fac for Purge Y/N ? Field  */~
            purge_flag$1,                /* Enable/Disable W/C Purging */~
            readkey$60,                  /* WORK VARIABLE              */~
            rewrk$(300)10,               /* QUANTITY TO REWORK JOB     */~
            rte$(255)200,                /* THE PLANNED ROUTING        */~
            rteact$(256)67,              /* FORMATTED WCOUT DATA       */~
            rteid$3,                     /* JOBS ROUTING AS PLANNED    */~
            run$(300)5,                  /* RUN TIME (UNITS)           */~
            rptqty$(3)10,                /* Report qty move/scrap/rwk  */~
            rfac$1,                      /* FACs for Reporting Units   */~
            runh$(300)7,                 /* RUN TIME (HOURS)           */~
            saverunh$7,                  /* TEMP RUN TIME (HOURS)      */~
            savesetuph$7,                /* TEMP SETUP TIME (HOURS)    */~
            scflag$(300)1,               /* Step closed flag  Y/N      */~
            scrap$(300)10,               /* QUANTITY SCRAPPED          */~
            setup$(300)5,                /* SETUP TIME (UNITS)         */~
            setuph$(300)7,               /* SETUP TIME (HOURS)         */~
            seq$3,                       /* Route sequence number      */~
            step$(300,2)7,               /* FROM & TO ROUTE STEPS      */~
            subtitle$(3)32,              /* SCREEN SUB TITLES          */~
            sw_handfactor$1,             /* Activate Handling Factor   */~
            sw_clear_wcout$1,            /* Default for "Clear WCOUT?" */~
            text$(300)30,                /* MOME TEXT FOR TRANS        */~
            tfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            time$(300)8,                 /* TRANSACTION TIME           */~
            title$50,                    /* DEFAULT REPORT TITLE       */~
            uheader$(2)10,               /* Reporting UOM Header string*/~
            used$(11)6,                  /* SUMMARY SCREEN ONLY FIELD  */~
            userid$3,                    /* YOU KNOW                   */~
            wc$(300,2)4                  /* FROM & TO WORK CENTERS     */

        dim f1%(64)                      /* = 1 IF READ WAS SUCCESSFUL */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! JBMASTR2 ! LEVEL 2 JOB MASTER FILE                  *~
            * # 2 ! JBSTATUS ! JOB STATUS TRACKING FILE                 *~
            * # 3 ! HNYMASTR ! INVENTORY MASTER (DESCRIPTIONS)          *~
            * # 4 ! WCMASTR  ! WORK CENTER MASTER FILE                  *~
            * # 5 ! PERMASTR ! PERSONNEL MASTER FILE                    *~
            * # 6 ! RTEACTIV ! STEP ACTIVITY CODES                      *~
            * # 7 ! RTEMASTR ! PRODUCTION ROUTINGS FILE                 *~
            * # 8 ! JBCROSS2 ! Additional Job Info                      *~
            * # 9 ! BOMMASTR ! BILL OF MATERIALS RELATIONSHIP FILE      *~
            * #10 ! ENGMASTR ! ENGINEERING MASTER FILE                  *~
            * #11 ! SYSFILE2 ! Caelus Management System Information     *~
            * #12 ! WCOUT    ! Planned work center use detail           *~
            * #13 ! RTEMAST2 ! Route step/sequence cross reference file *~
            * #14 ! VBKVSA   ! Vendor Service Advices file              *~
            * #15 ! VSAOUTIN ! Vendor Outside Processing Transfer Log   *~
            * #16 ! VENDOR   ! Vendor Master Records                    *~
            * #17 ! GENCODES ! System General Codes File                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #12, "WCOUT",                                         ~
                        varc,     indexed,  recsize =   68,              ~
                        keypos =    9, keylen =  23,                     ~
                        alt key  1, keypos =    1, keylen =  27

            select #13, "RTEMAST2",                                      ~
                        varc,     indexed,  recsize =   50,              ~
                        keypos =    1, keylen =  32

            select #14, "VBKVSA",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    5, keylen =   8,                     ~
                        alt key  6, keypos =   50, keylen =   4, dup,    ~
                            key  5, keypos =   41, keylen =  13, dup,    ~
                            key  4, keypos =   29, keylen =   6, dup,    ~
                            key  3, keypos =   13, keylen =  12, dup,    ~
                            key  2, keypos =    2, keylen =  11,         ~
                            key  1, keypos =    1, keylen =  12          ~

            select #15, "VSAOUTIN",                                      ~
                        varc,     indexed,  recsize =  316,              ~
                        keypos =    1, keylen =  26,                     ~
                        alt key  1, keypos =   44, keylen =   8, dup,    ~
                            key  2, keypos =   52, keylen =  23, dup

            select #16, "VENDOR"  ,                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup

            select #17, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            call "OPENCHCK" (#12, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#13, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#14, fs%, 0%, 0%, " ")
            call "OPENCHCK" (#15, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#16, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#17, 0%, 0%, 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)
            call "EXTRACT" addr ("ID", userid$)

            header$ = "Tran   Date     Time   STEP    WC   SETUP+RUN    M~
        ~OVED SCRPD+RWK ! STEP     WC"
            subtitle$(1) = "Moved From"
            subtitle$(2) = "Moved To"
            subtitle$(3) = "Other Info"

            uheader$(1%) = "Job Units "
            uheader$(2%) = "Mvmt Units"

            for i% = 1% to 300%
                convert i% to line$(i%), pic(###)
                str(line$(i%),4) = ")"
            next i%
            div$() = all("!")
            justin% = 0%

            dflt_runh$ = " "
            if inhours < .0001 then L09250
                     call "CONVERT" (inhours, 0.4, dflt_runh$)
L09250:     purge_flag$ = " "

*        Load Program Switchs Record...

            call "READ100" (#11, "SWITCHS.JBACTSUB", f1%(11%))
            if f1%(11%) = 1% then L09370
                put #11 using L09350, "SWITCHS.JBACTSUB", "NY" , " " , " "

L09350:             FMT CH(20), CH(2), CH(228), CH(250)
                write #11
L09370:     get #11 using L09390, sw_handfactor$, sw_clear_wcout$

L09390:         FMT POS(21), 2*CH(1)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            gosub L29000  /* Clear Variables For Input */
            if justin% = 0 then job$ = injob$
            if justin% = 0% then lastdate$ = indate$
            if lastdate$ = " " or lastdate$ = blankdate$ then lastdate$ = date$
            justin% = 123456789%
            maxlines%, printmode% = 0%

            for fieldnr% = 1% to 1%
                gosub'051(fieldnr%)
                      if enabled% = 0% then L10210
L10150:         gosub'101(1%, fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  9% then print_data
                      if keyhit%  = 16% and fieldnr% = 1 then exit_program
                      if keyhit% <> 17% then L10190
                          gosub mod_switchs
                          goto L10150
L10190:               if keyhit%  = 32% and fieldnr% = 1 then exit_program
                      if keyhit% <>  0% then       L10150
L10210:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10150
                next fieldnr%
                if maxlines% > 0% then summary

*        Get Line Item Data...
L10270:     if maxlines% = 300% then summary
            c% = maxlines% + 1%
            gosub inputlines
            gosub check_for_vendor_service
            if keyhit% <> 16% then L10330
                gosub columnone
                goto summary
L10330:     maxlines%=maxlines%+1%
            goto L10270

        inputlines
            factor$, errormsg$ = " "
            for fieldnr% = 1% to 13%
                gosub'052(fieldnr%)
                      if enabled% = 0% then L10570
L10410:         gosub'102(3%, fieldnr%)
                      if keyhit%  = 16% and fieldnr% = 1% then return
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  2% then       L10470
                         gosub columnone
                         goto inputlines
L10470:               if keyhit% <>  6% then       L10500
                         gosub prevline
                         goto L10410    /* COULD BE DIRECT TO NEXT  */
L10500:               if keyhit% <>  4% then       L10560
L10510:                  if fieldnr% < 2% then L10410
                         fieldnr% = fieldnr% - 1%
                         gosub'052(fieldnr%)
                         if enabled% <> 0% then L10410
                         goto L10510
L10560:               if keyhit%  <> 0% then    L10410
L10570:         gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10410
                next fieldnr%
                return

        columnone
                if c% > 1% then defstep$ = step$(c% - 1%, 2%)            ~
                           else defstep$ = " "
                step$(c%,1), wc$(c%,1), actv$(c%,1), moved$(c%),         ~
                scrap$(c%), rewrk$(c%), setup$(c%), run$(c%), date$(c%), ~
                step$(c%,2), wc$(c%,2), actv$(c%,2), time$(c%), emp$(c%),~
                text$(c%), jsflag$(c%), handfactor$(c%), scflag$(c%),    ~
                setuph$(c%), runh$(c%) = " "
            init(" ") rptqty$()
            return

        prevline
            if c%=1% then return
                  on fieldnr% goto  L11220,         /* FROM ROUTE STEP  */~
                                    L11230,         /* FROM WORK CENTER */~
                                    L11240,         /* FROM ACTIVITY    */~
                                    L11250,         /* HANDLING FACTOR  */~
                                    L11260,         /* QUANTITIES       */~
                                    L11300,         /* SET UP & RUN TIME*/~
                                    L11330,         /* TO ROUTE STEP    */~
                                    L11340,         /* TO WORK CENTER   */~
                                    L11350,         /* TO ACTIVITY      */~
                                    L11360,         /* DATE & TIME      */~
                                    L11380,         /* EMPLOYEE         */~
                                    L11390,         /* MEMO TEXT        */~
                                    L11400          /* STEP CLOSED FLAG */

                  return

L11220:         step$   (c%,1%) = step$   (c%-1%,1%):return
L11230:         wc$     (c%,1%) = wc$     (c%-1%,1%):return
L11240:         actv$   (c%,1%) = actv$   (c%-1%,1%):return
L11250:         handfactor$(c%) = handfactor$(c%-1%):return
L11260:         moved$    (c%) = moved$    (c%-1%)
                scrap$    (c%) = scrap$    (c%-1%)
                rewrk$    (c%) = rewrk$    (c%-1%):return

L11300:         setup$    (c%) = setup$    (c%-1%)
                run$      (c%) = run$      (c%-1%):return

L11330:         step$   (c%,2%) = step$   (c%-1%,2%):return
L11340:         wc$     (c%,2%) = wc$     (c%-1%,2%):return
L11350:         actv$   (c%,2%) = actv$   (c%-1%,2%):return
L11360:         date$     (c%) = date$     (c%-1%)
                time$     (c%) = time$     (c%-1%):return
L11380:         emp$      (c%) = emp$      (c%-1%):return
L11390:         text$     (c%) = text$     (c%-1%):return
L11400:         scflag$   (c%) = scflag$   (c%-1%):return

        mod_switchs       /* Allow user to change program switchs      */
            save_fieldnr% = fieldnr%
L11440:     lastfieldnr% = 0%
L11450:     gosub'106(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub reset_switchs
                  if keyhit%  = 16% then       save_switchs
                  if keyhit% <>  0% then       L11450
L11500:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  2% then mod_switchs
            if fieldnr% = lastfieldnr% then L11440
            gosub'056(fieldnr%)         /* Set Input Message           */
L11540:     gosub'106(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit% <>  0% then L11540
            gosub'156(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11540
                  lastfieldnr% = fieldnr%
            goto L11500

        reset_switchs     /* Set switches to what's stored on file     */
            call "READ100" (#11, "SWITCHS.JBACTSUB", f1%(11%))
                 if f1%(11%) <> 1% then return
            get #11 using L11650, sw_handfactor$, sw_clear_wcout$

L11650:         FMT POS(21), 2*CH(1)

            return

        save_switchs
            call "READ101" (#11, "SWITCHS.JBACTSUB", f1%(11%))
            put #11 using L11650, sw_handfactor$, sw_clear_wcout$

            rewrite #11
            purge_flag$ = sw_clear_wcout$
            keyhit%  = 17%
            fieldnr% = save_fieldnr%
            return   /* Goes back to selection screen */

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        editmode
            base% = 0%
            message$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Value And Press (ENTER)."

        summary              /* SUMMARY SCREEN */
            errormsg$ = " "
L12065:     insert%, quick_insert% = 0%
            message$="To Modify Line Item, Tab To Line And Press (ENTER)"
            purg_fld_fac$ = hex(86)

L12090:     gosub'110(0%)
              errormsg$ = " "
              if keyhit% = 1% then gosub startover
              if keyhit% = 2% then base% = 0%
              if keyhit% = 3% then base% = maxlines%-9%
              if keyhit% = 4% then base% = base%-9%
              if keyhit% = 5% then base% = base%+9%
              if keyhit% = 6% then base% = base%-1%
              if keyhit% = 7% then base% = base%+1%
              base%=max(0%,min(base%,maxlines%-11%))
              if keyhit% = 11% then       L12160
              if keyhit% = 12% then       L12160
              if keyhit% = 8% then       L12160
              if keyhit% = 16% then       datasave
              if keyhit% <>0 then L12090

L12160:       editcheck% = 0%
              if keyhit% <> 0% then L12185
              if cursor%(1) <> 7% then L12185
                     purg_fld_fac$ = hex(81)
                     message$ = "Enter Y if W/C Capacities are to be Purg~
        ~d upon Step Completion."
L12181:              gosub'110(0%)
                        if keyhit% =  1% then gosub startover
                        if keyhit% <> 0% then L12181
                     goto summary
L12185:       fieldnr% = cursor%(1%) - 9%
              if keyhit% = 0% and fieldnr% < 1% then fieldnr% = 1%
              if fieldnr% > 0% or keyhit% <> 11% then L12210
                c% = maxlines%
                goto insertmode
L12210:       if fieldnr% < 0% or fieldnr% > 11% then L12090
              if hfac$(fieldnr%) = hex(bc) then L12090
              c%=min(base%+fieldnr%,maxlines%)
              if keyhit%= 11% then insertmode
              if c%=0% then L12090
              fieldnr%=c%-base%

              if keyhit%=12% then deletemode
              if keyhit%<>0% then L12090

        edtpg2
            convert handfactor$(c%) to handfactor, data goto L12265
L12265:     quick_insert% = 0%
            editcheck% = 1%
            gosub refresh_quantities
            message$ = "To Modify Displayed Values, Position Cursor To De~
        ~sired Value And Press (ENTER)."
            REM Next Line Will Draw Out The Description For Lines Code(s)
            gosub'152(2%)
L12295:     errormsg$=" "
L12300:     gosub'102(4%, 0%)
                  errormsg$=" "
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then c%=1%
                  if keyhit%  =  3% then c%=max(1%, maxlines%)
                  if keyhit%  =  4% then c%=max(1%,c%-10)
                  if keyhit%  =  5% then c%=max(1%,min(c%+10,maxlines%))
                  if keyhit%  =  6% then c%=max(1%,c%-1%)
                  if keyhit%  =  7% then c%=min(maxlines%,c%+1%)
                  if keyhit%  =  9% then editmode
                  if keyhit%  = 11% then quick_insert
                  if keyhit%  = 16% then summary
                  if keyhit% <>  0% then edtpg2

*        Field number determination for edit mode screen
            fieldnr% = cursor%(1%) - 10%
            if fieldnr% < 1% or fieldnr% > 10% then L12295
            if fieldnr% < 4% then L12420    /* wc, rte, activity */
            if fieldnr% = 5% then L12295    /* no field */
            if cursor%(2) >= 40% then L12395
*       Lower Left side
            if fieldnr% = 4% then L12420   /* no edit handling factor */
            if fieldnr% > 8% then fieldnr% = 6% else fieldnr% = 5%
            goto L12440
L12395
*       Lower Right side
            if fieldnr% = 4% then L12295    /* no field */
            if fieldnr% > 9% then L12295    /* no field */
            fieldnr% = fieldnr% + 4%
            goto L12440
L12420:         errormsg$ = "Sorry, this field can't be changed. You " & ~
                            "must delete then re-enter the line."
                goto L12300

L12440:     gosub'062(fieldnr%)
                if enabled% = 0% then edtpg2
L12450:     gosub'102(7%, fieldnr%)
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  0% then L12450
            gosub'152(fieldnr%)
                if errormsg$ <> " " then L12450
            goto edtpg2

        quick_insert
              if maxlines% = 0% then edtpg2
              quick_insert% = 1%
              editcheck% = 0%
              fieldnr% = cursor%(1%) - 6%
              if fieldnr% > -3% and fieldnr% < 2% then L12525
                c% = maxlines%
                goto insertmode
L12525:       if c% = 1% then c% = 2%
              c%=max(min(c%+fieldnr%,maxlines%),0)
              goto insertmode

        REM *************************************************************~
            *       I N S E R T   &   D E L E T E   L O G I C           *~
            *                                                           *~
            * INSERT & DELETE CODE RESIDES HERE.                        *~
            *************************************************************

        insertmode: insert% = 1%
            if maxlines%=300% then summary

            REM Copy all Elements Up One...
            maxlines%=maxlines%+1%
            c% = c% + 1%
            if c%=maxlines% then L13180
                roll% = -1%
                for temp% = maxlines% to c% step -1
                     gosub roll_lines
                next temp%

L13180:     REM Get Line Item Data...
            gosub columnone
            gosub inputlines
            gosub check_for_vendor_service
            if keyhit% <> 16% then insertmode
            if c%-1 > base%+11 then base%=max(0%,min(c%-2,maxlines%-10))
            goto delete_line

        deletemode
            editcheck% = 1%
            purg_fld_fac$ = hex(84)
            gosub available1
            if low = 0 then L13320
                errormsg$ = "Can't delete line since items in Step "  &  ~
                            step$ & " are already moved forward."
                            goto L12065
L13320:     message$ = "To DELETE Flashing Line, press ENTER.  To Return ~
        ~without Delete, press PF1."
            gosub'112(fieldnr%)
                  if keyhit% = 1% then summary
                  if keyhit% <> 0% then deletemode

        delete_line
            if c%=maxlines% then L13440
                roll% = 1%
                for temp% = c% to maxlines%-1%
                     gosub roll_lines
                next temp%
L13440:     save%=max(c%-1%,1%)
            c%=maxlines%
            gosub columnone
            maxlines%=maxlines%-1%
            c%=min(save%,maxlines%)
            if quick_insert% = 1% then edtpg2
            base%=max(0%,min(base%,maxlines%-11%))
            goto summary

        roll_lines
                step$   (temp%,1) = step$   (temp%+roll%,1)
                wc$     (temp%,1) = wc$     (temp%+roll%,1)
                actv$   (temp%,1) = actv$   (temp%+roll%,1)
                handfactor$(temp%) = handfactor$(temp% + roll%)
                moved$    (temp%) = moved$    (temp%+roll%)
                scrap$    (temp%) = scrap$    (temp%+roll%)
                rewrk$    (temp%) = rewrk$    (temp%+roll%)
                setup$    (temp%) = setup$    (temp%+roll%)
                run$      (temp%) = run$      (temp%+roll%)
                step$   (temp%,2) = step$   (temp%+roll%,2)
                wc$     (temp%,2) = wc$     (temp%+roll%,2)
                actv$   (temp%,2) = actv$   (temp%+roll%,2)
                date$     (temp%) = date$     (temp%+roll%)
                time$     (temp%) = time$     (temp%+roll%)
                emp$      (temp%) = emp$      (temp%+roll%)
                text$     (temp%) = text$     (temp%+roll%)
                scflag$   (temp%) = scflag$   (temp%+roll%)
                jsflag$   (temp%) = jsflag$   (temp%+roll%)
        return

        refresh_quantities     /* and right justifies everthing */
            if handfactor = 1 then L13820
            convert moved$(c%) to temp, data goto L13760
                temp =  temp * handfactor
                call "CONVERT"(temp,  0.2, rptqty$(1%))
L13760:     convert scrap$(c%) to temp, data goto L13790
                temp =  temp * handfactor
                call "CONVERT"(temp,  0.2, rptqty$(2%))
L13790:     convert rewrk$(c%) to temp, data goto L13820
                temp =  temp * handfactor
                call "CONVERT"(temp,  0.2, rptqty$(3%))
L13820:     call "RJUSTIFY" (moved$(c%))
            call "RJUSTIFY" (scrap$(c%))
            call "RJUSTIFY" (rewrk$(c%))
            return

        REM *************************************************************~
            *       I N P U T   M O D E   F O R   P R I N T             *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        print_data
            errormsg$, startcode$, endcode$ = " "

            for fieldnr% = 1 to  2
                gosub'053(fieldnr%)
                      if enabled% = 0 then L15180
L15120:         gosub'103(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then inputmode
                      if keyhit% <>  0 then       L15120
                gosub'153(fieldnr%)
                      if errormsg$ <> " " then L15120
L15180:         next fieldnr%

        REM *************************************************************~
            *               P R I N T  C O D E S                        *~
            *                                                           *~
            * Prints a report showing Details tied to each Job.         *~
            *************************************************************

            select ws
            print at(05,02); hex(84); "Print In Progress"
            pagenumber% = 0%
            printmode% = 1%
            if startcode$ <> "ALL" then L16140
                readkey$ = all(hex(00))
                endcode$ = all(hex(ff))
                goto L16180
L16140:     str(readkey$,,8%) = str(startcode$,,8%) addc all(hex(ff))
            if endcode$ = " " then endcode$ = startcode$
            str(readkey$,9%) = all(hex(00))

L16180:     call "PLOWNEXT" (#2, readkey$, 0%, f1%(2%))
            if f1%(2%) = 0% then L16210
            if str(readkey$,,8%) <= endcode$ then L16250
L16210:         if pagenumber% <> 0% then print "  **  END OF REPORT **"
                close printer
                goto inputmode

L16250:     gosub L29000
            get #2, job$
            pageline% = 987654321%

            gosub load_data
            tran(jobpart$, hex(208c))replacing
            if maxlines% = 0% then L16550
            for c% = 1% to maxlines%
                gosub form_control
                indate$  =  indate$(c%)
                call "DATEFMT" (indate$)
                put errormsg$, using L16380, step$(c%,1), wc$(c%,1),      ~
                                            str(actv$(c%,1))
L16380:         %####### #### ####
                if step$(c%,1) <> " " then L16420
                     errormsg$ = "START ASSEMBLY  "

L16420:         moved$(c%) = str(moved$(c%),3)
                scrap$(c%) = str(scrap$(c%),3)
                rewrk$(c%) = str(rewrk$(c%),2)

                print using L17440, c%, date$(c%), time$(c%), errormsg$,  ~
                      setup$(c%), run$(c%), scflag$(c%), step$(c%,2),    ~
                      wc$(c%,2%),   actv$(c%,2), moved$(c%), scrap$(c%), ~
                      rewrk$(c%), emp$(c%), inuser$(c%), indate$
            next c%
            gosub form_control
L16550:     str(readkey$,9%) = all(hex(ff))
            goto L16180

        REM *************************************************************~
            *        P A G E   C O N T R O L   R O U T I N E            *~
            *                                                           *~
            * CONTROLS THE PAGING                                       *~
            *************************************************************

        form_control
                select printer (134)
                pageline% = pageline% + 1%
                if pageline% < 58% then return
                   print page
                   pagenumber% = pagenumber% + 1%
                   print using L17250, pagenumber%, date$
                   print using L17280, job$, jobdescr$
                   print using L17290, jobpart$, rteid$
                   print

                   print using L17360
                   print using L17400
                   print using L17480
                   pageline% = 8%
                   return

L17250: %PAGE ###                   J O B   S T A T U S   T R A N S A C T~
        ~ I O N S   L I S T I N G                              DATE: #####~
        ~###
L17280: %JOB NUMBER: ######## #########################################
L17290: %  JOB PART: ####################################################~
        ~#########                                        PLANNED ROUTING:~
        ~###

L17360: %                      ------------- F R O M ------------ -------~
        ~ T O ----- ---- Q U A N T I T I E S ---- -- MOVED BY - --- AUDIT ~
        ~---

L17400: %SEQ   DATE     TIME    STEP   WCTR ACTV SETUP  RUN  CLS?  STEP  ~
        ~ WCTR ACTV  MOVED    SCRAPPED  TO REWORK    EMPLOYEE   OPER   DAT~
        ~E

L17440: %### ######## ######## ################# ##### #####  #   #######~
        ~ #### #### ########  ########  #########  ############ #### #####~
        ~###

L17480: %--- -------- -------- ------- ---- ---- ----- ----- ---- -------~
        ~ ---- ---- --------  --------  ---------  ------------ ---- -----~
        ~---
        REM *************************************************************~
            *           M I C S .      S U B S                          *~
            *************************************************************
        check_for_vendor_service
            if fs% <> 1% or str(wc$(c%,2%),1%,1%) <> "V" then return
            readkey$ = str(job$) & step$(c%,2%)
            call "REDALT0" (#14, readkey$, 3%, f1%(14%))
            if f1%(14%) = 0% then return  /* Not in Vendor Service File */

           /* Ask if User wants to show movement to outside vendor */
L18080:     ask% = 2%
            call "ASKUSER" (ask%, "*** VENDOR SERVICE ACTIVITY ***",     ~
                           " Press PF 16  to Record Shipment to Vendor", ~
                           " Press RETURN to Continue WC Movements", " " )
            if ask% = 0% then return
            if ask% <> 16% then L18080
            /* Record movement to Outside Vendor for Service Activity */
            call "VSASHPSB"(#15, #14, #16, #1, #3, #17)

            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave

            message$ = "Saving Movement For Job Number:" & hex(84) &     ~
                                                           job$ & hex(8c)
            call "SHOSTAT" (message$)

            REM DELETE OLD REPORT FROM FILE
            readkey$ = str(job$) & hex(0000)
            call "DELETE" (#2, readkey$, 8%)

            gosub L31000    /* Write New Data To Files */
            if purge_flag$ <> "Y" then L19180
                 call "WCDPURGE" (job$, #12, #4, #2, #1)
L19180:     lastjob$, injob$ = job$
            if inemp$ <> " " then L65000   /* RETURN TO JBTCINP */
            goto inputmode

        REM *************************************************************~
            *   C O N V E R T S   W / C   H O U R S   T O   U N I T S   *~
            *************************************************************
        cnvrt_hours_2_units

            adjfactor = .00000001
            call "WCUN2HRS" (#4, wc$(c%,1%), factor, 0, " ")
            if factor <> 0 then adjfactor = 24/factor
            test1, test = round(test/adjfactor, 4)
            call "WCUN2HRS" (#4, wc$(c%,1%), 0, test, " ")
            return

        cvt_hrs2dec
            call "TIMEOK" (temp$, test, errormsg$)
                if errormsg$ <> " " then return clear                    ~
                   else call "CONVERT" (test, 0.4, str(temp$,,8))
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1% : message$=" "
            on fieldnr% gosub L20110          /* Job number and flag   */
            return

L20110: REM DEFAULT/ENABLE FOR JOB NUMBER
            message$ = "Enter A Blank Or Partial Value To Search" &      ~
                           " For Desired Job Number."

            purge_flag$ = sw_clear_wcout$

            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   D E T A I L     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 2 OF INPUT. *~
            *************************************************************

            deffn'052(fieldnr%)
                  enabled% = 1%:message$=" "
                  on fieldnr% gosub L21360,      /* FROM ROUTE STEP     */~
                                    L21409,      /* FROM WORK CENTER    */~
                                    L21409,      /* FROM ACTIVITY       */~
                                    L21421,      /* HANDLING FACTOR     */~
                                    L21460,      /* QUANTITIES          */~
                                    L21560,      /* SET UP & RUN TIME   */~
                                    L21720,      /* TO ROUTE STEP       */~
                                    L21920,      /* TO WORK CENTER      */~
                                    L22050,      /* TO ACTIVITY         */~
                                    L22170,      /* DATE & TIME         */~
                                    L22230,      /* EMPLOYEE            */~
                                    L22280,      /* MEMO TEXT           */~
                                    L22320       /* STEP CLOSED FLAG    */
            return

            deffn'062(fieldnr%)
                  enabled% = 1:message$=" "
                  on fieldnr% goto  L21390,      /* FROM ROUTE STEP     */~
                                    L21409,      /* FROM WORK CENTER    */~
                                    L21409,      /* FROM ACTIVITY       */~
                                    L21421,      /* HANDLING FACTOR     */~
                                    L21460,      /* QUANTITIES          */~
                                    L21570,      /* SET UP & RUN TIME   */~
                                    L21890,      /* TO ROUTE STEP       */~
                                    L22020,      /* TO WORK CENTER      */~
                                    L22140,      /* TO ACTIVITY         */~
                                    L22200,      /* DATE & TIME         */~
                                    L22250,      /* EMPLOYEE            */~
                                    L22290,      /* MEMO TEXT           */~
                                    L22320       /* STEP CLOSED FLAG    */
                     return

L21360: REM DEFAULT/ENABLE FOR FROM ROUTE STEP
            if step$(c%,1) = " " then step$(c%,1) = defstep$
            moved$(c%), scrap$(c%), rewrk$(c%) = " "
            init(" ")  rptqty$()
L21390:     message$ = "Enter Routing Step Material Was Moved FROM. U" & ~
                       "se PF8 To Review Standard Routing"
            if wc$() = " " then enabled% = 0%
            return

L21409: REM DEFAULT/ENABLE FOR FROM WC & ACTV
            enabled% = 0%
            return

L21421: REM DEFAULT/ENABLE FOR HANDLING FACTOR
                        /* Factor is normally obtained from source step */
            if handfactor$(c%) <> " " then L21456
                handfactor$(c%) = "1.00"
                handfactor      =  1
            if sw_handfactor$ = "Y" then L21433
                enabled% = 0%
                return
L21433:     init(hex(00)) plowkey$
            str(plowkey$,1,32) = str(part$,,)
            str(plowkey$,26,3) = str(rteid$,,)
            str(plowkey$,29,4) = str(step$(c%,1%),1,4)

                if step$(c%,1%) <> " " then L21444
                    seq$ = "  1"  /* on first movement, use factor for */
                    goto L21449    /* destination step                  */

L21444:         call "READ100"(#13, plowkey$, f1%(13%))
                    if f1%(13%) = 0% then L21456
                get #13 using L21447, seq$
L21447:             FMT POS(33), CH(3)

L21449:         str(plowkey$,29,3) = str(seq$,1,3)
                str(plowkey$,32,) = " "
                call "READ100"(#7, plowkey$, f1%(7%))
                    if f1%(7%) = 0% then L21456
                    get #7 using L21454, handfactor
L21454:              FMT  POS(184), PD(14,4)
                call "CONVERT" (handfactor, -0.2, handfactor$(c%))
L21456:         message$="Enter number of movement units per one unit "& ~
                         "the job part."
                return

L21460: REM DEFAULT/ENABLE FOR QUANTITY TO MOVE, SCRAP, & FOR REWORK
            if moved$(c%) <> " " then L21495
            gosub available
            if step$(c%,1%) = " " then avl = jquan
            call "CONVERT" (avl, -0.2, moved$(c%))
L21495:     message$="Enter Quantities. Scrap & Rework are memo only," & ~
                     " but not to W/C Purging."

            if handfactor = 1 then L21520
            convert moved$(c%) to temp, data goto L21506
                temp =  temp * handfactor
                call "CONVERT"(temp, -0.2, rptqty$(1%))
L21506:     convert scrap$(c%) to temp, data goto L21509
                temp =  temp * handfactor
                call "CONVERT"(temp, -0.2, rptqty$(2%))
L21509:     convert rewrk$(c%) to temp, data goto L21520
                temp =  temp * handfactor
                call "CONVERT"(temp, -0.2, rptqty$(3%))
L21520:     call "SPCESMSH" (moved$(c%), 0%)
            call "SPCESMSH" (scrap$(c%), 0%)
            call "SPCESMSH" (rewrk$(c%), 0%)
            return

L21560: REM DEFAULT/ENABLE FOR SET UP TIME
L21570:     if wc$(c%,1) = " " then enabled% = 0
            message$ = "Enter Time (units) The Part Spent In Setup "&    ~
                           "And Run At The FROM Work Center."
            if enabled% = 0 then return
            if run$(c%) <> " " and runh$(c%) <> " " then L21620
                run$(c%) = " "
                runh$(c%) = dflt_runh$
L21620:     call "SPCESMSH" (setup$(c%), 0%)
            call "SPCESMSH" (setuph$(c%), 0%)
            call "SPCESMSH" (run$(c%), 0%)
            call "SPCESMSH" (runh$(c%), 0%)
            savesetuph$ = setuph$(c%)
            saverunh$ = runh$(c%)
            return

L21720: REM DEFAULT/ENABLE FOR TO ROUTE STEP
            convert moved$(c%) to temp, data goto L21780
            if abs(temp) > .009 then L21780
                step$(c%,2) = " "
                enabled% = 0%
                return
L21780:     if step$(c%,2) <> " " then L21890
            if keyhit% = 4% then L21890
                gosub get_rte_seq_from
                if step$(c%,1) = " " then seq% = 0%
                seq% = seq% + 1%
                if seq% <= rmax% then L21860
                     if seq% > 1 then step$(c%,2) = "DONE"
                     goto L21890
L21860:         step$ = str(rte$(seq%),88,5)
                gosub format_step
                step$(c%,2) = step$
L21890:         message$ = "Enter TO Route Step If Known. The default "& ~
                           " is the planned next step. (WCOUT) "
                return

L21920: REM DEFAULT/ENABLE FOR TO WORK CENTER
            enabled% = 0%
            if step$(c%,2) = "DONE" then wc$(c%,2) = " "
            if step$(c%,2) = "DONE" then return
            convert moved$(c%) to temp, data goto L21990
                if abs(temp) > .009 then L21990
                return
L21990:     gosub get_wc_and_actv
            enabled% = error%
            if error% = 0 then wc$(c%,2) = wc$
L22020:     message$ = "Enter Work Center That The Job Part Was Moved" & ~
                       " TO."
            return

L22050: REM DEFAULT/ENABLE FOR TO ACTIVITY CODE
            enabled% = 0%
            if step$(c%,2) = "DONE" then return
            convert moved$(c%) to temp, data goto L22110
                if abs(temp) > .009 then L22110
                return
L22110:     gosub get_wc_and_actv
            enabled% = error%
            if error% = 0% then actv$(c%,2) = actv$
L22140:     message$ = "Enter Activity To Be Performed On The Job " &    ~
                       " Part In The TO Work Center."
            return

L22170: REM DEFAULT/ENABLE FOR TRANSACTION ACTUAL DATE & TIME
            if date$(c%) = " " or date$(c%) = blankdate$ ~
               then date$(c%) = lastdate$
            if time$(c%) = " " then time$(c%) = intime$
L22200:     message$ = "Enter Date And (optionally) Time The Movement" & ~
                       " Actually Occurred."
            return

L22230: REM DEFAULT/ENABLE FOR WHO MOVED
            if emp$(c%) = " " then emp$(c%) = inemp$
L22250:     message$ = "Enter The Employee Code (from payroll) Of The" & ~
                       " Employee Who Performed The Move."
            return

L22280: REM DEFAULT/ENABLE FOR MEMO TEXT
L22290:     message$ = "This Is Just A Place For You To Put Notes For" & ~
                       " Later Reference."
            return

L22320: REM DEFAULT/ENABLE FOR STEP CLOSED FLAG
            scflag$(c%) = "N"
            if step$(c%,1%) <> " " then L22326
                enabled% = 0%
                return

L22326:     if mvqty + scqty + rwqty >= jquan then scflag$(c%) = "Y"
            message$ = "Is the 'From' step now closed?  Enter 'Y'" &     ~
                           " or 'N'."
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 3 OF INPUT. *~
            *************************************************************

            deffn'053(fieldnr%)
                  enabled% = 1
                  on fieldnr% gosub L23110,         /* STARTING JOB     */~
                                    L23160          /* ENDING JOB       */
                     return
L23110:     REM DEFAULT/ENABLE FOR STARTING CODE
            startcode$ = "ALL"
            message$ = "ALL Will Print all Details On File. To Print Rang~
        ~es, Enter The Starting Job."
                return
L23160:     REM DEFAULT/ENABLE FOR ENDING CODE
            if startcode$ = "ALL" then enabled% = 0
            message$ = "Enter Last Job To Print.  Leave Blank To Only Pri~
        ~nt The 'Starting Job Number'."
                return

        REM *************************************************************~
            *  D E F A U L T / E N A B L E   F O R   S W I T C H E S    *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Switches screen.     *~
            *************************************************************

        deffn'056(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L23390,         /* Show Handling factor   */~
                              L23440          /* Default Clear WCOUTS?  */
            return

L23390: REM Def/Enable for Activate Handling Factor -  SW_HANDFACTOR$

            message$ = "Enter 'Y' to activate Movement Unit Field" &     ~
                       " or 'N' to not use Movement Factor."
            return

L23440: REM Def/Enable for Clear WCOUTS?       SW_CLEAR_WCOUT$

            message$ = "Enter Default for 'Should WCOUTs be Cleared " &  ~
                       "as Movement is Reported?' (Y/N)."
            return

L29000: REM *************************************************************~
            * Initialization Block                                      *~
            *************************************************************

            bomid$, rteid$, defstep$, descr$, empdescr$, errormsg$,      ~
            filler$, job$, jobdescr$, jobpart$, jobquan$, message$,      ~
            paren$, part$, partdescr$, title$ = " "

            init (" ") actv$(), date$(), emp$(), indate$(), inuser$(),   ~
                       jdate$(), jsflag$(), moved$(), rte$(), rteact$(), ~
                       rewrk$(), run$(), run$(), runh$(), scrap$(),      ~
                       setup$(), setuph$(), step$(), text$(), time$(),   ~
                       wc$(), handfactor$(), scflag$(), rptqty$()
            call "ALLFREE"
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
            startover% = 2%
            call "STARTOVR" (startover%)
            if startover% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *             L O A D   O L D   R E P O R T E D             *~
            *                                                           *~
            * LOADS OLD REPORTED FROM DISK FILES.                       *~
            *************************************************************

        load_data
            maxlines% = 0%
            call "GETCODE" (#1, job$, jobdescr$, 1%, 99, f1%(1%))
                if f1%(1%) <> 0% then L30060
                errormsg$ = "Job Not On File"
                return
L30060:     get #1, using L30065,part$,jquan,jdate$(1),temp$,jdate$(2)
L30065:     FMT XX(57),CH(25),PD(14,4),XX(56),2*CH(6),XX(15),CH(6)
            call "DATEFMT" (jdate$(1%))
            call "DATEFMT" (jdate$(2%))
            if temp$ = " " or temp$ = blankdate$ or printmode% <> 0% then L30090
                errormsg$ = "Job Closed On " & temp$
                call "DATEFMT" (str(errormsg$,15,8))
                return
L30090:     call "CONVERT" (jquan, -0.2, jobquan$)

            REM Find Routing this job was planned to use...
            call "GETCODE" (#3, part$, partdescr$, 1%, 99, f1%(3%))
                if f1%(3%) = 0% then partdescr$=hex(94)& "Not On File"
            jobpart$ = part$ & hex(8c) & partdescr$

            REM Load Routing this job was planned to use...
            call "PLNTRVLR" ("JOB ORDER: " & str(job$),                  ~
                        #7,              /* RTEMASTR FILE              */~
                        #12,             /* WCOUT FILE                 */~
                        #8,              /* JBCROSS2 FILE              */~
                        #9,              /* BOMMASTR FILE              */~
                        #10,             /* ENGMASTR FILE              */~
                        #11,             /* SYSFILE2 FILE              */~
                        #1,              /* PIPIN or JBMASTR2          */~
                        rteid$,          /* Route For Tag (Returned)   */~
                        bomid$,          /* Bill For Tag (Returned)    */~
                        rte$(),          /* Phantomized Routing        */~
                        rmax%,           /* Number Of Elements in Array*/~
                        rteact$(),       /* Traveler Array             */~
                        ptr%)            /* Number Of Elements in Array*/

            REM Put WCOUT data onto RTE$ array if WCOUT data exists...
                if ptr% = 0% then L30295   /* Use Standard Traveler */
                for i% = 1% to ptr%
                   rte$(i%) = " "
                   step$ = str(rteact$(i%),,7) : gosub unformat_step
                   str(rte$(i%),88,5) = step$
                   str(rte$(i%),32,4) = str(rteact$(i%),10,4)
                   str(rte$(i%),94,4) = str(rteact$(i%),34,4)
                next i%
                rmax% = ptr%

L30295:     REM Now Try And Load The Line Items
            handfactor = 1
            readkey$ = str(job$)
            str(readkey$,9) = all(hex(00))
L30310:     call "PLOWNEXT" (#2%, readkey$, 8%, f1%(2%))
                     if f1%(2) = 0% then return

            c%, maxlines% = maxlines% + 1%
            if printmode% <> 0% then L30340
            if c% = 1% then call "SHOSTAT" ("Recalling Existing Details")
L30340:     get #2, using L30405, job$, wc$(c%,2), actv$(c%,2), wc$(c%,1),~
                      date$(c%), time$(c%), actv$(c%,1), indate$(c%),    ~
                      step$(c%,1), step$(c%,2), emp$(c%), setup%, run%,  ~
                      moved, scrap, rewrk, text$(c%), inuser$(c%),       ~
                      jsflag$(c%), scflag$(c%)

            if scflag$ <> "Y" then scflag$ = "N"

            call "CONVERT" (moved, 0.2, moved$(c%))
            call "CONVERT" (scrap, 0.2, scrap$(c%))
            call "CONVERT" (rewrk, 0.2, rewrk$(c%))
            handfactor$(c%) = "1.00"
            convert setup% to setup$(c%), pic(#####)
                test = setup%
                call "WCUN2HRS" (#4, wc$(c%,1%), 0, test, " ")
                call "CONVERT" (test, 0.4, setuph$(c%))
            convert run% to run$(c%), pic(#####)
                test = run%
                call "WCUN2HRS" (#4, wc$(c%,1%), 0, test, " ")
                call "CONVERT" (test, 0.4, runh$(c%))
            call "DATEFMT" (date$(c%))
            goto L30310

L30405:         FMT CH(08),              /* JOB NUMBER                 */~
                    XX(4),               /* SEQUENCE NUMBER            */~
                    CH(04),              /* TO WORK CENTER             */~
                    CH(04),              /* TO ACTIVITY CODE           */~
                    XX(08),              /* JOB NUMBER                 */~
                    CH(04),              /* FROM WORK CENTER           */~
                    CH(6),               /* TRANSACTION DATE           */~
                    CH(8),               /* TRANSACTION TIME           */~
                    CH(04),              /* FROM ACTIVITY CODE         */~
                    CH(6),               /* SYSTEM DATE                */~
                    XX(8),               /* SYSTEM TIME                */~
                    CH(07),              /* FROM RTE STEP              */~
                    CH(07),              /* TO RTE STEP                */~
                    CH(12),              /* EMPLOYEE CODE              */~
                    BI(4),               /* SET UP TIME                */~
                    BI(4),               /* RUN TIME                   */~
                    PD(14,4),            /* QUANTITY MOVED             */~
                    PD(14,4),            /* QUANTITY SCRAPPED          */~
                    PD(14,4),            /* QUANTITY FOR REWORK JOB    */~
                    CH(30),              /* FREE TEXT                  */~
                    CH(04),              /* USERID WHO ENTERED TRANS   */~
                    CH(1),               /* PURGED WCOUT FLAG          */~
                    CH(1)                /* STEP CLOSED FLAG           */

L31000: REM *************************************************************~
            *       W R I T E   R E P O R T E D   T O   F I L E S       *~
            *                                                           *~
            * SAVES GOODIES IN A SAFE PLACE.                            *~
            *************************************************************

            if maxlines% = 0 then return

            REM Here we gooooo....
            if maxlines% = 0 then return
            for c% = 1 to maxlines%
            moved, scrap, rewrk = 0
            convert moved$(c%) to moved, data goto L31130
L31130:     convert scrap$(c%) to scrap, data goto L31140
L31140:     convert rewrk$(c%) to rewrk, data goto L31150
L31150:     convert setup$(c%) to setup%, data goto L31160
L31160:     convert run$(c%) to run%, data goto L31170
L31170:     call "DATUNFMT" (date$(c%))
            if indate$(c%) = " " or indate$ = blankdate$ then indate$(c%) = date
            if inuser$(c%) = " " then inuser$(c%) = userid$
            if step$(c%,2) = "DONE" then wc$(c%,2) = " "

L31210:     if date$(c%)   = " " then date$(c%) = blankdate$
            if indate$(c%) = " " then indate$(c%) = blankdate$
            write #2, using L31290, job$, c%, wc$(c%,2), actv$(c%,2),     ~
                      job$, wc$(c%,1), date$(c%), time$(c%), actv$(c%,1),~
                      indate$(c%), time, step$(c%,1), step$(c%,2),       ~
                      emp$(c%), setup%, run%, moved, scrap, rewrk,       ~
                      text$(c%), inuser$(c%), jsflag$(c%), scflag$(c%),  ~
                      " ", eod goto L31210
            next c%
            return

L31290:         FMT CH(08),              /* JOB NUMBER                 */~
                    BI(4),               /* SEQUENCE NUMBER            */~
                    CH(04),              /* TO WORK CENTER             */~
                    CH(04),              /* TO ACTIVITY CODE           */~
                    CH(08),              /* JOB NUMBER                 */~
                    CH(04),              /* FROM WORK CENTER           */~
                    CH(6),               /* TRANSACTION DATE           */~
                    CH(8),               /* TRANSACTION TIME           */~
                    CH(04),              /* FROM ACTIVITY CODE         */~
                    CH(6),               /* SYSTEM DATE                */~
                    CH(8),               /* SYSTEM TIME                */~
                    CH(07),              /* FROM RTE STEP              */~
                    CH(07),              /* TO RTE STEP                */~
                    CH(12),              /* EMPLOYEE CODE              */~
                    BI(4),               /* SET UP TIME                */~
                    BI(4),               /* RUN TIME                   */~
                    PD(14,4),            /* QUANTITY MOVED             */~
                    PD(14,4),            /* QUANTITY SCRAPPED          */~
                    PD(14,4),            /* QUANTITY FOR REWORK JOB    */~
                    CH(30),              /* FREE TEXT                  */~
                    CH(04),              /* USERID WHO ENTERED TRANS   */~
                    CH(1),               /* PURGED WCOUT FLAG          */~
                    CH(1),               /* STEP CLOSED FLAG           */~
                    CH(42)               /* FILLER                     */

        REM *************************************************************~
            *              S P E C I A L   R O U T I N E S              *~
            *                                                           *~
            * Not so special, just no better place to put.              *~
            *************************************************************

        get_wc_and_actv
            step$ = step$(c%,2)
            gosub get_rte_seq
            wc$, actv$ = " "
            error% = 1
            if seq% <> 0% then L35230  /* In Route */
                REM try And Find WC & ACTV if off route...
                if maxlines% < 1 then return
                for i% = 1 to maxlines%
                     if i% = c% then L35210
                     if step$(i%,2)<>step$(c%,2) then L35210
                          wc$ = wc$(i%,2%)
                          actv$ = actv$(i%,2%)
                          i% = maxlines%
                          error% = 0
L35210:         next i%
                return
L35230:     get rte$(seq%), using L35240, wc$, actv$
L35240:     FMT POS(32), CH(4), POS(94), CH(4)
            error% = 0
        return

        get_rte_seq_from
            step$ = step$(c%,1)
        get_rte_seq
            seq% = 0
            gosub unformat_step
            if rmax% = 0 then return
            for seq% = 1 to rmax%
                if str(rte$(seq%),88,5) = step$ then L35380
            next seq%
            seq% = 0
L35380: return

        available
            REM Find Quantity Available To Move...
            step$ = step$(c%,1) : avl = 0
            REM How much has been moved to/from this center/activity?
            if step$ = " " then avl = 9999999
            for i% = 1 to maxlines%
                if i% = c% then L35580
                if step$(i%,2)<>step$ then L35510
                   convert moved$(i%) to temp, data goto L35580
                   avl = round(avl + temp, 2)
                   goto L35580
L35510:         if step$(i%,1)<>step$ then L35580
                   convert moved$(i%) to temp, data goto L35580
                   avl = round(avl - temp, 2)
                   convert scrap$(i%) to temp, data goto L35580
                   avl = round(avl - temp, 2)
                   convert rewrk$(i%) to temp, data goto L35580
                   avl = round(avl - temp, 2)
L35580:     next i%

            REM What is the minimum to move? (applies only to edit mode)
            REM IF your editing a line, the old quantity move to the next
            REM center may have already been moved from that center to
            REM another, thus you can't say you didn't move it from here!
            available1
            step$ = step$(c%,2) : low, free_cap = 0
            if editcheck% = 0 then return
            if step$ = " " then return
            for i% = 1 to maxlines%
                if i% = c% then L35810
                if step$(i%,2)<>step$ then L35740
                   convert moved$(i%) to temp, data goto L35810
                   free_cap = round(free_cap + temp, 2)
                   goto L35810
L35740:         if step$(i%,1)<>step$ then L35810
                   convert moved$(i%) to temp, data goto L35810
                   free_cap = round(free_cap - temp, 2)
                   convert scrap$(i%) to temp, data goto L35580
                   free_cap = round(free_cap - temp, 2)
                   convert rewrk$(i%) to temp, data goto L35580
                   free_cap = round(free_cap - temp, 2)
L35810:     next i%
            low = max(-free_cap, 0)
        return

        format_step
            temp% = val(str(step$,5,1),1)
            str(step$,5) = " "
            if temp% = 0 then return
                step$ = step$ & "-"
                convert temp% to str(step$,pos(step$="-")+1), pic(00)
        return

        unformat_step
            temp% = 0
            if pos(step$="-") = 0 then L35980
            convert str(step$,pos(step$="-")+1) to temp%, data goto L35970
L35970:     str(step$,pos(step$="-")) = " "
L35980:     str(step$,5) = bin(temp%,1)
        return

        REM *************************************************************~
            *          I N P U T  /  E D I T  S C R E E N   2           *~
            *                                                           *~
            * MANAGES THE LINE DETAIL.                                  *~
            *************************************************************

         deffn'102(screen%, fieldnr%)
            gosub set_keys
            init(hex(84)) lfac$()
            rfac$ = hex(9c)   /* blank, protect */
            jrfac$(1%) = hex(ac)   /* dim, underline */
            jrfac$(2%) =  hex(9c)  /* blank, protect */
            if fieldnr% = 0% then L40086
            if fieldnr% < 5%  then L40090
L40086:     if handfactor = 1 then L40090
                jrfac$(2%) = hex(ac)
                rfac$ = hex(8c)

L40090:     if fieldnr%=0% then init(hex(86)) lfac$()
            header1$ = "JOB: " & job$ & " " & descr$
            str(header1$,54,8) = "Page 3"
            str(header1$,62) = "JBACTSUB: " & str(cms2v$,,8)

         REM Format line display for top of screen
            if wc$() <> " " then L40190   /* First line being entered? */
            lfac$, display$(2), display$(3) = hex(9c)
            display$(1) = hex(84) & "     ***  Enter Movement Transaction~
        ~s In THE ORDER THEY OCCURRED.  ***"
            goto L40500

L40190:     if fieldnr% > 1% then L40500
            lfac$ = hex(ac)
            k% = max(0, c%-2)
            init (" ") display$(), used$(), other$()
            save% = c%
            for i% = 1% to 3%
                c% = k%+i%
                if c% > maxlines% then L40460
                if (date$(c%) = " " or date$(c%) = blankdate$) ~
                    and c% = save% then L40370
                if date$(c%) = " "  or date$(c%) = blankdate$ then L40440
                temp, temp1 = 0
                convert setup$(c%) to temp, data goto L40310
L40310:         convert run$(c%) to temp1, data goto L40320
L40320:         call "CONVERT" (temp+temp1, 0.2, used$(i%))
                temp, temp1 = 0
                convert scrap$(c%) to temp, data goto L40350
L40350:         convert rewrk$(c%) to temp1, data goto L40360
L40360:         call "CONVERT" (temp+temp1, 0.2, other$(i%))
L40370:         put display$(i%), using L40400, line$(c%), date$(c%),     ~
                    time$(c%), step$(c%,1), wc$(c%,1), used$(i%),        ~
                    moved$(c%), other$(i%), "!", step$(c%,2), wc$(c%,2)
L40400:         FMT XX(1), CH(4), XX(1), CH(8), XX(1), CH(8), XX(1),     ~
                    CH(4), XX(2), CH(4), XX(3), CH(6), XX(1), CH(10),    ~
                    XX(1), CH(9), XX(1), CH(1), XX(1), CH(7), XX(1),     ~
                    CH(4)
L40440:         str(display$(i%),,1) = hex(8c)
                if c% = save% then str(display$(i%),,1) = hex(84)
L40460:     next i%
            c% = save%
            str(display$(3),,1) = " "

L40500:           on fieldnr% gosub L40660,      /* FROM ROUTE STEP     */~
                                    L40660,      /* FROM WORK CENTER    */~
                                    L40660,      /* FROM ACTIVITY       */~
                                    L40660,      /* HANDLING FACTOR     */~
                                    L40710,      /* QUANTITIES MOVED    */~
                                    L40660,      /* SET UP & RUN TIME   */~
                                    L40660,      /* TO ROUTE STEP       */~
                                    L40660,      /* TO WORK CENTER      */~
                                    L40660,      /* TO ACTIVITY         */~
                                    L40660,      /* DATE & TIME         */~
                                    L40660,      /* EMPLOYEE            */~
                                    L40630,      /* MEMO TEXT           */~
                                    L40660       /* STEP CLOSED FLAG    */
                     goto L40737

L40630:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40660:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40710: REM Screen Control for reporting in job or handling units
               if handfactor <> 1 then L40722
                  lfac$(fieldnr%) = hex(81)
                  jrfac$(2%) = hex(9c)
                  rfac$ = hex(9c)
                  return

L40722:        jrfac$(1%) = hex(ac)   /* dim, protect, underline */
               jrfac$(2%) = hex(ac)   /* dim, protect, underline */
               rfac$ = hex(81)        /* enabled for input */
               lfac$(fieldnr%) = hex(8c)
               return

L40737: accept                                                           ~
               at (01,02), "Report Progress Of Production Jobs",         ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                   ,ch(08),~
               at (02,02), fac(hex(ac)), header1$                ,ch(79),~
                                                                         ~
               at (04,02), fac(lfac$),   header$                 ,ch(79),~
               at (05,02), fac(hex(8c)), display$(1)             ,ch(79),~
               at (06,02), fac(hex(8c)), display$(2)             ,ch(79),~
               at (07,02), fac(hex(ac)), display$(3)             ,ch(79),~
               at (09,02), fac(hex(94)), errormsg$               ,ch(79),~
                                                                         ~
               at (08,02), "Job Part:",                                  ~
               at (08,12), fac(hex(84)),   jobpart$              ,ch(59),~
                                                                         ~
               at (10,02), fac(hex(ac)),   subtitle$(1)          ,ch(32),~
               at (11,04), "ROUTE STEP",                                 ~
               at (11,16), fac(lfac$(1)),  step$(c%,1)           ,ch(07),~
                                                                         ~
               at (12,04), "WORK CENTER",                                ~
               at (12,18), fac(lfac$(2)),  wc$(c%,1)             ,ch(04),~
                                                                         ~
               at (13,04), "ACTIVITY CODE",                              ~
               at (13,18), fac(lfac$(3)),  actv$(c%,1)           ,ch(04),~
                                                                         ~
               at (14,04), "Movement Unit Factor",                       ~
               at (14,25), fac(lfac$(4)),  handfactor$(c%)       ,ch(08),~
                                                                         ~
               at (15,17), fac(jrfac$(1%)), uheader$(1%)         ,ch(10),~
               at (15,28), fac(jrfac$(2%)), uheader$(2%)         ,ch(10),~
                                                                         ~
               at (16,04), "Qty Moved",                                  ~
               at (16,17), fac(lfac$(5)),  moved$(c%)            ,ch(10),~
               at (16,28), fac(rfac$   ),  rptqty$(1%)           ,ch(10),~
                                                                         ~
               at (17,04), "Qty Scrapped",                               ~
               at (17,17), fac(lfac$(5)),  scrap$(c%)            ,ch(10),~
               at (17,28), fac(rfac$   ),  rptqty$(2%)           ,ch(10),~
                                                                         ~
               at (18,04), "Qty To Rwk",                                 ~
               at (18,17), fac(lfac$(5)),  rewrk$(c%)            ,ch(10),~
               at (18,28), fac(rfac$   ),  rptqty$(3%)           ,ch(10),~
                                                                         ~
               at (19,04), "SetUp Hours XXXXXXX  Units XXXXX",           ~
               at (19,16), fac(lfac$(6)),  setuph$(c%)           ,ch(07),~
               at (19,31), fac(lfac$(6)),  setup$(c%)            ,ch(05),~
                                                                         ~
               at (20,04), "Run Hours   XXXXXXX  Units XXXXX",           ~
               at (20,16), fac(lfac$(6)),  runh$(c%)             ,ch(07),~
               at (20,31), fac(lfac$(6)),  run$(c%)              ,ch(05),~
                                                                         ~
               at (21,06), fac(hex(84)),   factor$               ,ch(30),~
                                                                         ~
               at (10,40), fac(hex(ac)),   subtitle$(2)          ,ch(32),~
               at (11,42), "ROUTE STEP",                                 ~
               at (11,55), fac(lfac$(7)),  step$(c%,2)           ,ch(07),~
                                                                         ~
               at (12,42), "WORK CENTER",                                ~
               at (12,57), fac(lfac$(8)),  wc$(c%,2)             ,ch(04),~
                                                                         ~
               at (13,42), "ACTIVITY CODE",                              ~
               at (13,57), fac(lfac$(9)),  actv$(c%,2)           ,ch(04),~
                                                                         ~
               at (15,40), fac(hex(ac)),   subtitle$(3)          ,ch(32),~
               at (16,42), "Moved On: Date           Time",              ~
               at (16,57), fac(lfac$(10)), date$(c%)             ,ch(08),~
               at (16,72), fac(lfac$(10)), time$(c%)             ,ch(08),~
                                                                         ~
               at (17,42), "Moved By Employee",                          ~
               at (17,60), fac(lfac$(11)), emp$(c%)              ,ch(12),~
                                                                         ~
               at (18,42), "Comments",                                   ~
               at (18,50), fac(lfac$(12)), text$(c%)             ,ch(30),~
                                                                         ~
               at (19,42), "Is Step Complete?",                          ~
               at (19,68), fac(lfac$(13)), scflag$(c%)           ,ch(01),~
                                                                         ~
               at (22,02), fac(hex(a4)), message$                ,ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(1)             ,ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(2)             ,ch(79),~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 8 then L41450
                  call "RTEDSPLY" (part$, rteid$, #7, #3)
                  goto L40737

L41450:        if keyhit% <> 13% then L41490
                  call "MANUAL" ("JBACTSUB")
                  goto L40737

L41490:        if keyhit% <> 15% then L41530
                  call "PRNTSCRN"
                  goto L40737

L41530:        if fieldnr% = 0% and keyhit% = 11% then L41550
               if fieldnr% <> 0% or keyhit% > 0% then return
L41550:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *          L I N E   S U M M A R Y   S C R E E N            *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 3 OF DOCUMENT.                    *~
            *************************************************************

            deffn'101(screen%,fieldnr%)
                  gosub set_keys
                  lfac$, purg_fld_fac$ = hex(8c)
                  init (hex(9c)) tfac$(), lfac$(), hfac$()
                  if fieldnr% > 0% then purg_fld_fac$ = hex(81)
                  header1$ = "Last Job Updated: XXXXXXXX"

                  str(header1$,19,8) = lastjob$
                  str(header1$,54,6) = "Page 1"
                  str(header1$,62) = "JBACTSUB: " & str(cms2v$,,8)

                  on fieldnr% gosub L42170          /* REPORT NUMBER    */
                     goto L42550

L42170:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$ = hex(81)
                      return

            deffn'110(fieldnr%)    /* Scan Mode */
            init (hex(8e)) tfac$()
            init (hex(84)) lfac$()
            init (hex(8c)) hfac$()
            lfac$ = hex(84)
            screen% = 5%
            goto L42340

            deffn'112(fieldnr%)    /* Delete Mode */
            init (hex(8c)) lfac$(), hfac$(), tfac$()
            hfac$(fieldnr%),lfac$(fieldnr%),tfac$(fieldnr%)=hex(94)
            lfac$ = hex(84)
            screen% = 6%
L42340:     gosub set_keys
            init (" ") other$(), used$()
            save% = c%
            for i% = 1% to 11%
                c% = base%+i%
                if date$(c%) = " " or date$(c%) = blankdate$ then L42490
                     temp, temp1 = 0
                     convert setup$(c%) to temp, data goto L42420
L42420:              convert run$(c%) to temp1, data goto L42430
L42430:              call "CONVERT" (temp+temp1, 0.2, used$(i%))
                     temp, temp1 = 0
                     convert scrap$(c%) to temp, data goto L42460
L42460:              convert rewrk$(c%) to temp1, data goto L42470
L42470:              call "CONVERT" (temp+temp1, 0.2, other$(i%))
                     goto L42500
L42490:         hfac$(i%), lfac$(i%), tfac$(i%) = hex(9c)
L42500:     next i%
            c% = save%
            header1$ = "Transaction Summary Screen"
            str(header1$,54,6) = "Page 2"
            str(header1$,62) = "JBACTSUB: " & str(cms2v$,,8)
L42550:     accept                                                       ~
               at (01,02), "Report Progress Of Production Jobs",         ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                   ,ch(08),~
               at (02,02), fac(hex(ac)), header1$                ,ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "JOB NUMBER",                                 ~
               at (04,14), fac(lfac$),    job$                   ,ch(08),~
               at (04,23), fac(hex(8c)),  descr$                , ch(32),~
               at (04,58), "ROUTE ID",                                   ~
               at (04,67), fac(hex(84)),  rteid$                 ,ch(03),~
               at (04,74), "BOM",                                        ~
               at (04,78), fac(hex(84)),  bomid$                 ,ch(03),~
               at (05,02), "PART NUMBER",                                ~
               at (05,14), fac(hex(84)),  part$                  ,ch(25),~
               at (05,40), fac(hex(8c)),  partdescr$            , ch(34),~
               at (06,02), "START DATE",                                 ~
               at (06,13), fac(hex(84)),  jdate$(1)              ,ch(08),~
               at (06,27), "PLANNED END DATE",                           ~
               at (06,44), fac(hex(84)),  jdate$(2)              ,ch(08),~
               at (06,58), "QTY TO BUILD",                               ~
               at (06,71), fac(hex(84)),  jobquan$               ,ch(10),~
               at (07,02), "CLEAR WCOUT's After Step?",                  ~
               at (07,28), fac(purg_fld_fac$),  purge_flag$      ,ch(01),~
                                                                         ~
               at (08,25), "..F R O M... ..UNITS..    ..QUANTITIES... ! .~
        ~...T O....",                                                     ~
               at (09,02), fac(hex(ae)),   header$               ,ch(79),~
                                                                         ~
               at (10,02), fac(tfac$( 1)),  line$  (base%+ 1)    ,ch(04),~
               at (11,02), fac(tfac$( 2)),  line$  (base%+ 2)    ,ch(04),~
               at (12,02), fac(tfac$( 3)),  line$  (base%+ 3)    ,ch(04),~
               at (13,02), fac(tfac$( 4)),  line$  (base%+ 4)    ,ch(04),~
               at (14,02), fac(tfac$( 5)),  line$  (base%+ 5)    ,ch(04),~
               at (15,02), fac(tfac$( 6)),  line$  (base%+ 6)    ,ch(04),~
               at (16,02), fac(tfac$( 7)),  line$  (base%+ 7)    ,ch(04),~
               at (17,02), fac(tfac$( 8)),  line$  (base%+ 8)    ,ch(04),~
               at (18,02), fac(tfac$( 9)),  line$  (base%+ 9)    ,ch(04),~
               at (19,02), fac(tfac$(10)),  line$  (base%+10)    ,ch(04),~
               at (20,02), fac(tfac$(11)),  line$  (base%+11)    ,ch(04),~
                                                                         ~
               at (10,07), fac(hfac$( 1)),  date$  (base%+ 1)    ,ch(08),~
               at (11,07), fac(hfac$( 2)),  date$  (base%+ 2)    ,ch(08),~
               at (12,07), fac(hfac$( 3)),  date$  (base%+ 3)    ,ch(08),~
               at (13,07), fac(hfac$( 4)),  date$  (base%+ 4)    ,ch(08),~
               at (14,07), fac(hfac$( 5)),  date$  (base%+ 5)    ,ch(08),~
               at (15,07), fac(hfac$( 6)),  date$  (base%+ 6)    ,ch(08),~
               at (16,07), fac(hfac$( 7)),  date$  (base%+ 7)    ,ch(08),~
               at (17,07), fac(hfac$( 8)),  date$  (base%+ 8)    ,ch(08),~
               at (18,07), fac(hfac$( 9)),  date$  (base%+ 9)    ,ch(08),~
               at (19,07), fac(hfac$(10)),  date$  (base%+10)    ,ch(08),~
               at (20,07), fac(hfac$(11)),  date$  (base%+11)    ,ch(08),~
                                                                         ~
               at (10,16), fac(hfac$( 1)),  time$  (base%+ 1)    ,ch(08),~
               at (11,16), fac(hfac$( 2)),  time$  (base%+ 2)    ,ch(08),~
               at (12,16), fac(hfac$( 3)),  time$  (base%+ 3)    ,ch(08),~
               at (13,16), fac(hfac$( 4)),  time$  (base%+ 4)    ,ch(08),~
               at (14,16), fac(hfac$( 5)),  time$  (base%+ 5)    ,ch(08),~
               at (15,16), fac(hfac$( 6)),  time$  (base%+ 6)    ,ch(08),~
               at (16,16), fac(hfac$( 7)),  time$  (base%+ 7)    ,ch(08),~
               at (17,16), fac(hfac$( 8)),  time$  (base%+ 8)    ,ch(08),~
               at (18,16), fac(hfac$( 9)),  time$  (base%+ 9)    ,ch(08),~
               at (19,16), fac(hfac$(10)),  time$  (base%+10)    ,ch(08),~
               at (20,16), fac(hfac$(11)),  time$  (base%+11)    ,ch(08),~
                                                                         ~
               at (10,25), fac(lfac$( 1)),  step$  (base%+ 1, 1) ,ch(07),~
               at (11,25), fac(lfac$( 2)),  step$  (base%+ 2, 1) ,ch(07),~
               at (12,25), fac(lfac$( 3)),  step$  (base%+ 3, 1) ,ch(07),~
               at (13,25), fac(lfac$( 4)),  step$  (base%+ 4, 1) ,ch(07),~
               at (14,25), fac(lfac$( 5)),  step$  (base%+ 5, 1) ,ch(07),~
               at (15,25), fac(lfac$( 6)),  step$  (base%+ 6, 1) ,ch(07),~
               at (16,25), fac(lfac$( 7)),  step$  (base%+ 7, 1) ,ch(07),~
               at (17,25), fac(lfac$( 8)),  step$  (base%+ 8, 1) ,ch(07),~
               at (18,25), fac(lfac$( 9)),  step$  (base%+ 9, 1) ,ch(07),~
               at (19,25), fac(lfac$(10)),  step$  (base%+10, 1) ,ch(07),~
               at (20,25), fac(lfac$(11)),  step$  (base%+11, 1) ,ch(07),~
                                                                         ~
               at (10,33), fac(lfac$( 1)),  wc$    (base%+ 1, 1) ,ch(04),~
               at (11,33), fac(lfac$( 2)),  wc$    (base%+ 2, 1) ,ch(04),~
               at (12,33), fac(lfac$( 3)),  wc$    (base%+ 3, 1) ,ch(04),~
               at (13,33), fac(lfac$( 4)),  wc$    (base%+ 4, 1) ,ch(04),~
               at (14,33), fac(lfac$( 5)),  wc$    (base%+ 5, 1) ,ch(04),~
               at (15,33), fac(lfac$( 6)),  wc$    (base%+ 6, 1) ,ch(04),~
               at (16,33), fac(lfac$( 7)),  wc$    (base%+ 7, 1) ,ch(04),~
               at (17,33), fac(lfac$( 8)),  wc$    (base%+ 8, 1) ,ch(04),~
               at (18,33), fac(lfac$( 9)),  wc$    (base%+ 9, 1) ,ch(04),~
               at (19,33), fac(lfac$(10)),  wc$    (base%+10, 1) ,ch(04),~
               at (20,33), fac(lfac$(11)),  wc$    (base%+11, 1) ,ch(04),~
                                                                         ~
               at (10,39), fac(hfac$( 1)),  used$  (01)         , ch(06),~
               at (11,39), fac(hfac$( 2)),  used$  (02)         , ch(06),~
               at (12,39), fac(hfac$( 3)),  used$  (03)         , ch(06),~
               at (13,39), fac(hfac$( 4)),  used$  (04)         , ch(06),~
               at (14,39), fac(hfac$( 5)),  used$  (05)         , ch(06),~
               at (15,39), fac(hfac$( 6)),  used$  (06)         , ch(06),~
               at (16,39), fac(hfac$( 7)),  used$  (07)         , ch(06),~
               at (17,39), fac(hfac$( 8)),  used$  (08)         , ch(06),~
               at (18,39), fac(hfac$( 9)),  used$  (09)         , ch(06),~
               at (19,39), fac(hfac$(10)),  used$  (10)         , ch(06),~
               at (20,39), fac(hfac$(11)),  used$  (11)         , ch(06),~
                                                                         ~
               at (10,46), fac(lfac$( 1)),  moved$ (base%+ 1)    ,ch(10),~
               at (11,46), fac(lfac$( 2)),  moved$ (base%+ 2)    ,ch(10),~
               at (12,46), fac(lfac$( 3)),  moved$ (base%+ 3)    ,ch(10),~
               at (13,46), fac(lfac$( 4)),  moved$ (base%+ 4)    ,ch(10),~
               at (14,46), fac(lfac$( 5)),  moved$ (base%+ 5)    ,ch(10),~
               at (15,46), fac(lfac$( 6)),  moved$ (base%+ 6)    ,ch(10),~
               at (16,46), fac(lfac$( 7)),  moved$ (base%+ 7)    ,ch(10),~
               at (17,46), fac(lfac$( 8)),  moved$ (base%+ 8)    ,ch(10),~
               at (18,46), fac(lfac$( 9)),  moved$ (base%+ 9)    ,ch(10),~
               at (19,46), fac(lfac$(10)),  moved$ (base%+10)    ,ch(10),~
               at (20,46), fac(lfac$(11)),  moved$ (base%+11)    ,ch(10),~
                                                                         ~
               at (10,57), fac(hfac$( 1)),  other$ ( 1)          ,ch(09),~
               at (11,57), fac(hfac$( 2)),  other$ ( 2)          ,ch(09),~
               at (12,57), fac(hfac$( 3)),  other$ ( 3)          ,ch(09),~
               at (13,57), fac(hfac$( 4)),  other$ ( 4)          ,ch(09),~
               at (14,57), fac(hfac$( 5)),  other$ ( 5)          ,ch(09),~
               at (15,57), fac(hfac$( 6)),  other$ ( 6)          ,ch(09),~
               at (16,57), fac(hfac$( 7)),  other$ ( 7)          ,ch(09),~
               at (17,57), fac(hfac$( 8)),  other$ ( 8)          ,ch(09),~
               at (18,57), fac(hfac$( 9)),  other$ ( 9)          ,ch(09),~
               at (19,57), fac(hfac$(10)),  other$ (10)          ,ch(09),~
               at (20,57), fac(hfac$(11)),  other$ (11)          ,ch(09),~
                                                                         ~
               at (10,67), fac(hfac$( 1)),  div$   (01)         , ch(01),~
               at (11,67), fac(hfac$( 2)),  div$   (02)         , ch(01),~
               at (12,67), fac(hfac$( 3)),  div$   (03)         , ch(01),~
               at (13,67), fac(hfac$( 4)),  div$   (04)         , ch(01),~
               at (14,67), fac(hfac$( 5)),  div$   (05)         , ch(01),~
               at (15,67), fac(hfac$( 6)),  div$   (06)         , ch(01),~
               at (16,67), fac(hfac$( 7)),  div$   (07)         , ch(01),~
               at (17,67), fac(hfac$( 8)),  div$   (08)         , ch(01),~
               at (18,67), fac(hfac$( 9)),  div$   (09)         , ch(01),~
               at (19,67), fac(hfac$(10)),  div$   (10)         , ch(01),~
               at (20,67), fac(hfac$(11)),  div$   (11)         , ch(01),~
                                                                         ~
               at (10,69), fac(lfac$( 1)),  step$  (base%+ 1, 2) ,ch(07),~
               at (11,69), fac(lfac$( 2)),  step$  (base%+ 2, 2) ,ch(07),~
               at (12,69), fac(lfac$( 3)),  step$  (base%+ 3, 2) ,ch(07),~
               at (13,69), fac(lfac$( 4)),  step$  (base%+ 4, 2) ,ch(07),~
               at (14,69), fac(lfac$( 5)),  step$  (base%+ 5, 2) ,ch(07),~
               at (15,69), fac(lfac$( 6)),  step$  (base%+ 6, 2) ,ch(07),~
               at (16,69), fac(lfac$( 7)),  step$  (base%+ 7, 2) ,ch(07),~
               at (17,69), fac(lfac$( 8)),  step$  (base%+ 8, 2) ,ch(07),~
               at (18,69), fac(lfac$( 9)),  step$  (base%+ 9, 2) ,ch(07),~
               at (19,69), fac(lfac$(10)),  step$  (base%+10, 2) ,ch(07),~
               at (20,69), fac(lfac$(11)),  step$  (base%+11, 2) ,ch(07),~
                                                                         ~
               at (10,77), fac(lfac$( 1)),  wc$    (base%+ 1, 2) ,ch(04),~
               at (11,77), fac(lfac$( 2)),  wc$    (base%+ 2, 2) ,ch(04),~
               at (12,77), fac(lfac$( 3)),  wc$    (base%+ 3, 2) ,ch(04),~
               at (13,77), fac(lfac$( 4)),  wc$    (base%+ 4, 2) ,ch(04),~
               at (14,77), fac(lfac$( 5)),  wc$    (base%+ 5, 2) ,ch(04),~
               at (15,77), fac(lfac$( 6)),  wc$    (base%+ 6, 2) ,ch(04),~
               at (16,77), fac(lfac$( 7)),  wc$    (base%+ 7, 2) ,ch(04),~
               at (17,77), fac(lfac$( 8)),  wc$    (base%+ 8, 2) ,ch(04),~
               at (18,77), fac(lfac$( 9)),  wc$    (base%+ 9, 2) ,ch(04),~
               at (19,77), fac(lfac$(10)),  wc$    (base%+10, 2) ,ch(04),~
               at (20,77), fac(lfac$(11)),  wc$    (base%+11, 2) ,ch(04),~
                                                                         ~
               at (22,02), fac(hex(a4)), message$                ,ch(79),~
               at (23,02), fac(hex(8c)), pfdescr$(1)             ,ch(79),~
               at (24,02), fac(hex(8c)), pfdescr$(2)             ,ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 8 then L44220
                  call "RTEDSPLY" (part$, rteid$, #7, #3)
                  goto L42550

L44220:        if keyhit% <> 13 then L44260
                  call "MANUAL" ("JBACTSUB")
                  goto L42550

L44260:        if keyhit% <> 15 then L44300
                  call "PRNTSCRN"
                  goto L42550

L44300:        close ws
               call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *               S W I T C H S   S C R E E N                 *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'106(fieldnr%, edit%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L44530,         /* Handling Factor   */   ~
                                L44530          /* Clear WCOUTs?     */

              goto L44560
                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L44530:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L44560:     accept                                                       ~
               at (01,02),                                               ~
                  "Record Workcenter Movement: Manage Program Switches", ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Allow Movement Conv. Factor:",               ~
               at (06,32), fac(lfac$( 1)), sw_handfactor$       , ch(01),~
                                                                         ~
               at (07,02), "Default for 'Clear WCOUTs?':",               ~
               at (07,32), fac(lfac$( 2)), sw_clear_wcout$      , ch(01),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   message$             , ch(79),~
               at (22,02), fac(hex(8c)),   pfdescr$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L45070
                  call "MANUAL" ("NEWSCRN ") : goto L44560

L45070:        if keyhit% <> 15% then L45100
                  call "PRNTSCRN" : goto L44560

L45100:        call "GETSCRN" ("C", i$(), cursor%(), u3%)
               u3% = u3%
               return

        set_pf1
        if edit% = 2% then L45280     /*  Input Mode             */
            pfdescr$(1) = "(1)Start Over                           " &   ~
                     "                       (13)Instructions"
            pfdescr$(2) = "                 (4)Previous Field      " &   ~
                     "                       (15)Print Screen"
            pfdescr$(3) = "                                        " &   ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L45240
                str(pfdescr$(3),64) = " " : str(pfkeys$,16,1) = hex(ff)
L45240:     if fieldnr% > 2% then L45260
                str(pfdescr$(2),18,26) = " " : str(pfkeys$, 4,1) = hex(ff)
L45260:     return

L45280: if fieldnr% > 0% then L45390  /*  Edit Mode - Select Fld */
            message$ = "To Modify a field, position cursor and press" &  ~
                       " RETURN."
            pfdescr$(1) = "(1)Reset Switchs                        " &   ~
                     "                       (13)Instructions"
            pfdescr$(2) = "                                        " &   ~
                     "                       (15)Print Screen"
            pfdescr$(3) = "                                        " &   ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L45390:                              /*  Edit Mode - Enabled    */
            pfdescr$(1) = "                                        " &   ~
                     "                       (13)Instructions"
            pfdescr$(2) = "                                        " &   ~
                     "                       (15)Print Screen"
            pfdescr$(3) = "                                        " &   ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                    S C R E E N  III                       *~
            *                                                           *~
            * Handles print range selection.                            *~
            *************************************************************

            REM Input Mode Screen Controler
            deffn'103(fieldnr%)
                  pfdescr$(1) ="                                       "&~
                               "     (13)Instructions   (15)Print Screen"
                  pfdescr$(2) ="                                       "&~
                               "                (16)Cancel Print Request"
                  pfkeys$ = hex(000d0f10)
                  init(hex(8c)) lfac$()
                  str(pfdescr$(2),55,1)=hex(84) /* Make Sure They See */
                  header1$= "Print Job Movement Transactions"
                  str(header1$,54,6) = "Page 4"
                  str(header1$,62) = "JBACTSUB: " & str(cms2v$,,8)
                  on fieldnr% gosub L47240,         /* START JOB        */~
                                    L47240          /* END JOB          */
                     goto L47310

L47240:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return

L47310:     accept                                                       ~
               at (01,02), "Report Progress Of Production Jobs",         ~
               at (01,67), "Date:", fac(hex(8c)), date$         , ch(08),~
               at (02,02), fac(hex(ac)), header1$               , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Starting Job Number",                                 ~
               at (06,30), fac(lfac$( 1)), startcode$           , ch(08),~
               at (07,02),                                               ~
                  "Ending Job Number",                                   ~
               at (07,30), fac(lfac$( 2)), endcode$             , ch(08),~
                                                                         ~
               at (22,02), fac(hex(a4)),   message$             , ch(79),~
               at (23,02), fac(hex(8c)),   pfdescr$(1)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfdescr$(2)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L47540
                  call "MANUAL" ("JBACTSUB")
                  goto L47310

L47540:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L47310

        REM *************************************************************~
            *                    S E T   K E Y S                        *~
            *                                                           *~
            * Sets PF Keys & Descriptions Based On SCREEN%...           *~
            *************************************************************

        set_keys                 /* Cleansliness Is Next To Godliness? */
            on screen% goto L49075,         /* Header, Input Mode       */~
                                 ,         /* Header, Edit Mode        */~
                            L49170,         /* Line Input Mode          */~
                            L49270,         /* Line Scrn, Edit Mode     */~
                            L49360,         /* Summary, Display Mode    */~
                            L49450,         /* Summary, Delete Mode     */~
                            L49490          /* All Scrns, Edit Field    */

L49075: REM  =-=-=-=-=-=-=-=-=-= Header, Input mode =-=-=-=-=-=-=-=-=-=-=
            pfdescr$(1) = "(1)Start Over    (4)Previous Field            ~
        ~(13)Instructions (15)Print Screen"
            pfdescr$(2) = "(17)Set Defaults (9)List Transactions         ~
        ~                 (16)Exit Program"
            pfkeys$ = hex(000104090a0d0f10ffffffffffff11ff)
            str(pfdescr$(2),63,1) = hex(84)

*        Flip Off Appropriate Fields
            if fieldnr% = 1% then L49150
                str(pfdescr$(2),63)    = " "    /* Shut Off Exit Optn  */
                str(pfkeys$,8,1) = hex(ff)
                str(pfdescr$(2),18,16) = " "    /* Shut Off Print Optn */
                str(pfkeys$,4,1) = hex(ff)
                str(pfdescr$(2), 1,17) = " "    /* Shut Off Defaults   */
                str(pfkeys$,15,1) = hex(ff)
                goto L49160
L49150:     str(pfdescr$(1),,35) = " "          /* Shut Off Prev Field */
            str(pfkeys$,2,2) = hex(ffff)
L49160: return

L49170: REM =-=-=-=-=-=-=-=-= Line Items, Input mode =-=-=-=-=-=-=-=-=-=
            pfdescr$(1) = "(1)Start Over    (4)Previous Field     (8)See ~
        ~Routing          (15)Print Screen"
            pfdescr$(2) = "(2)Restart Line  (6)Same As Prev Line         ~
        ~(13)Instructions                 "
            pfkeys$ = hex(0001020406080d0fffffffffffffffff)

*        Flip Off Appropriate Fields
            if c% > 1 then L49225
                str(pfdescr$(2),18,20) = " "    /* Shut Off Prev Line  */
                str(pfkeys$,5,1) = hex(ff)
L49225:     if fieldnr% <> 1 then L49260
                str(pfdescr$(1),18,17) = " "    /* Shut Off Prev Field */
                str(pfkeys$,4,1) = hex(ff)
                str(pfdescr$(2),66) = hex(84) & "(16)Edit Mode"
                str(pfkeys$,9,1) = hex(10)
                if insert% = 0 then L49260
                    str(pfdescr$(2),58) = hex(84)&"(16)Return To Summary"
L49260: return

L49270: REM =-=-=-=-=-=-=-=-= Line Items, Edit mode =-=-=-=-=-=-=-=-=-=-=
            pfdescr$(1) = "(1)Start Over    (4/6)Previous Trans.  (8)See ~
        ~Routing          (15)Print Screen"
            pfdescr$(2) = "(2)First Trans.  (5/7)Next Transactions       ~
        ~(13)Instr.  (16)Return To Summary"
            pfkeys$ = hex(000102040506070dff0f1003080bffff)
            str(pfdescr$(2),58,1) = hex(84)

*        Flip Off Appropriate Fields
            if c% > 1 then L49335
                str(pfdescr$(1),18,20)   = " "   /* Shut Off Prev Line */
                str(pfdescr$(2),,15)     = " "  /* Shut Off First Line */
                str(pfkeys$,3,2), str(pfkeys$,6,1) = hex(ffff)
L49335:     if c% < maxlines% then L49350
                str(pfdescr$(2),18,22)   = " "   /* Shut Off Next Line */
                str(pfkeys$,5,1), str(pfkeys$,7,1) = hex(ff)
L49350: return

L49360: REM =-=-=-=-=-=-=- Summary Screen, Display mode -=-=-=-=-=-=-=-=
            pfdescr$(1) = "(1)Start Over  (4/6)Prev Lines   (8)See Route ~
        ~(12)Delete Line  (15)Print Screen"
            pfdescr$(2) = "(2)First Line  (5/7)Next Lines  (11)Add Line  ~
        ~(13)Instructions    (16)Save Data"
            pfkeys$ = hex(00010204060507030b0c0d0e0f1008ffff)
            str(pfdescr$(2),66,1) = hex(84)

*        Flip Off Appropriate Fields
            if base% > 0 then L49425
                str(pfdescr$(2),,13)   = " "    /* Shut Off First Line */
                str(pfdescr$(1),16,15) = " "    /* Shut Off Prev Scrn  */
                str(pfkeys$,3,3) = hex(ffffff)
L49425:     if base%+11 < maxlines% then return
                str(pfdescr$(2),16,15) = " "    /* Shut Off Next Scrn  */
                str(pfkeys$,6,3) = hex(ffffff)
        return

L49450: REM =-=-=-=-=-=-=- Summary Screen, Delete mode -=-=-=-=-=-=-=-=-=
            pfdescr$(1) = "(1)Cancel Delete Request                      ~
        ~(13)Instructions (15)Print Screen"
            pfdescr$(2) = "(ENTER)Delete The Flashing Line               ~
        ~                                 "
            pfkeys$ = hex(00010d0fffffffffffffffffffffffff)
        return

L49490: REM =-=-=-=-=-=-=-= All Screens, Field Edit =-=-=-=-=-=-=-=-=-=
            pfdescr$(1) = "(1)Start Over                          (13)Ins~
        ~tructions        (15)Print Screen"
            pfdescr$(2) = "(ENTER) Validate Modification(s)              ~
        ~                                 "
            pfkeys$ = hex(00010d0e0fffffffffffffffffffffff)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50110          /* Job & Purge Flag */
                     return

L50110:     REM Test Data For Job Number
                call "GETCODE" (#1, job$, descr$, 1%, 0, f1%(1))
                if f1%(1) <> 0 then L50152
                    errormsg$ = hex(00)
                    return
L50152:     REM Test Data For Purge Flag
                if purge_flag$ = "Y" or purge_flag$ = "N" then L50165
                    errormsg$ = "Must Enter 'Y' or 'N' for CLEAR WCOUT's"
                    return
L50165:         gosub load_data
                if maxlines% = 0 then return
                return clear all
                goto editmode


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2. (Labor Detail)        *~
            *************************************************************

            deffn'152(test%)
            errormsg$ = " "
            indate$(c%) = date
            inuser$(c%) = userid$
                  on test%    gosub L51250,      /* FROM ROUTE STEP     */~
                                    L51420,      /* FROM WORK CENTER    */~
                                    L51510,      /* FROM ACTIVITY       */~
                                    L51540,      /* HANDLING FACTOR     */~
                                    L51610,      /* QUANTITIES          */~
                                    L52020,      /* SET UP & RUN TIME   */~
                                    L52430,      /* TO ROUTE STEP       */~
                                    L52620,      /* TO WORK CENTER      */~
                                    L52700,      /* TO ACTIVITY         */~
                                    L52800,      /* DATE & TIME         */~
                                    L52890,      /* EMPLOYEE            */~
                                    L52960,      /* MEMO TEXT           */~
                                    L52990       /* STEP CLOSED FLAG    */
                     return

L51250: REM TEST DATA FOR FROM ROUTE STEP
            wc$(c%,1), actv$(c%,1) = " "
            if step$(c%,1) = " " then return
            if maxlines% < 1 then return
               error% = 1
               for i% = 1 to maxlines%
                  if i% = c% then L51370
                     if step$(i%,2)<>step$(c%,1) then L51370
                          wc$(c%,1%)   = wc$(i%,2%)
                          actv$(c%,1%) = actv$(i%,2%)
                          i% = maxlines%
                          error% = 0
L51370:        next i%
            if error% = 1 then errormsg$ = "Step Not In Route/Never H"&  ~
                               "ad Materials Moved To It."
            return

L51420: REM TEST DATA FOR FROM WORK CENTER
            factor$ = " "
            if wc$(c%,1) = " " then return
            if wc$(c%,1) <> "VEND" then L51480
                factor$ = "One Unit = One Vendor Hour"
                return
L51480:     call "WCUN2HRS" (#4, wc$(c%,1), 0, 0, factor$)
                return

L51510: REM TEST DATA FOR FROM ACTIVITY CODE
            return

L51540: REM TEST DATA FOR HANDLING FACTOR
            call "NUMTEST" (handfactor$(c%),-9999999, 99999999,          ~
                                errormsg$,-0.2, handfactor)
            if handfactor <> 0 then  return
            errormsg$ = "The Factor Cannot Be 0."
            return

L51610: REM TEST DATA FOR QUANTITIES - MOVED
            gosub available : current = avl
            if handfactor = 1 then L51750

*       Tests for quantities if handling factor is active (not = 1)
            call "NUMTEST" (rptqty$(1%),  low * handfactor, 9e9,         ~
                                    errormsg$,-0.2, temp)
            moved, mvqty =  temp / handfactor
            if errormsg$ <> " " then error_msg
            call "CONVERT" (mvqty,  0.2, moved$(c%))

            REM QUANTITY TO SCRAP
                avl = avl - mvqty
                call "NUMTEST" (rptqty$(2%), 0, avl * handfactor,        ~
                                    errormsg$,-0.2, temp)
                if errormsg$ <> " " then error_msg2
                scqty =  temp / handfactor
                call "CONVERT" (scqty,  0.2, scrap$(c%))

            REM QUANTITY FOR REWORK JOB
                avl = avl - scqty
                call "NUMTEST" (rptqty$(3%), 0, avl * handfactor,        ~
                                    errormsg$,-0.2, temp)
                if errormsg$ <> " " then error_msg2
                rwqty =  temp / handfactor
                call "CONVERT" (rwqty,  0.2, rewrk$(c%))
                return

*       Test logic if not using handling factor
L51750:     call "NUMTEST" (moved$(c%),low,9e9, errormsg$,-0.2, mvqty)
            if errormsg$ <> " " then error_msg

            REM QUANTITY TO SCRAP
                avl = avl - mvqty
                call "NUMTEST" (scrap$(c%), 0, avl, errormsg$,-0.2, scqty)
                if errormsg$ <> " " then error_msg2
            REM QUANTITY FOR REWORK JOB
                avl = avl - scqty
                call "NUMTEST" (rewrk$(c%), 0, avl, errormsg$,-0.2, rwqty)
                if errormsg$ <> " " then error_msg2
                return

        error_msg
            if moved >= low then error_msg1
            k% = 2%
            call "ASKUSER" (k%, "* * * QUANTITY ERROR * * *"           , ~
                 "The quantity you are trying to move doesn't jibe with",~
                 "quantities moved into later steps.  Try editing the ", ~
              "downstream steps first.  Press any key to acknowledge.")
                  errormsg$ = hex(00)
                  return
        error_msg1
            call "CONVERT" (current , 2.2, cj$)
            call "CONVERT" (current * handfactor, 2.2, cr$)
            k% = 2%
            call "ASKUSER" (k%, "* * * QUANTITY ERROR * * *"           , ~
                 "There are currently " & cj$ & " Job Units (" & cr$ &   ~
                 " Reporting Units) in",                                 ~
                 "this step.  But you are trying to move more than this" ~
                 & " quantity out.",                                     ~
                 "Press any key to acknowledge; then correct the " &     ~
                 "quantity.")
            errormsg$ = hex(00)
            return

        error_msg2
            call "CONVERT" (current , 2.2, cj$)
            call "CONVERT" (current * handfactor, 2.2, cr$)
            k% = 2%
            call "ASKUSER" (k%, "* * * QUANTITY ERROR * * *"           , ~
                 "There are currently " & cj$ & " Job Units (" & cr$ &   ~
                 " Reporting Units) in",                                 ~
                 "this step.  But you are trying to move more than this" ~
                 & " quantity out.",                                     ~
                 "Press any key to acknowledge; then correct the " &     ~
                 "quantity.")
            errormsg$ = hex(00)
            return
L52020: REM TEST DATA FOR SETUP & RUN TIMES
            REM Decide which way the time was entered
            if setuph$(c%) <> savesetuph$ or setup$(c%) = " " then L52120

            REM Setup entered in WC Units
            call "NUMTEST" (setup$(c%), 0, 6e5,errormsg$,-0.4,test)
                if errormsg$ <> " " then return
            call "WCUN2HRS" (#4, wc$(c%,1%), 0, test, " ")
            goto L52200

L52120:     REM Setup entered in WC Hours
            if pos(setuph$(c%) = ":") = 0 then L52160
                temp$ = setuph$(c%)
                     gosub cvt_hrs2dec : setuph$(c%) = temp$
L52160:         call "NUMTEST" (setuph$(c%), 0, 6e5,errormsg$,-0.4,test)
                     if errormsg$ <> " " then return
                gosub cnvrt_hours_2_units
                call "CONVERT" (test1, 0.4, setup$(c%))
L52200:         call "CONVERT" (test, 0.4, setuph$(c%))

        REM RUN TIME
            REM Decide which way the time was entered
            if runh$(c%) <> saverunh$ or run$(c%) = " " then L52320

            REM Run entered in WC Units
            call "NUMTEST" (run$(c%), 0, 6e5,errormsg$,-0.4,test)
                if errormsg$ <> " " then return
            call "WCUN2HRS" (#4, wc$(c%,1%), 0, test, " ")
                goto L52400

L52320:     REM Run entered in WC Hours
            if pos(runh$(c%) = ":") = 0 then L52360
                temp$ = runh$(c%)
                gosub cvt_hrs2dec : runh$(c%) = temp$
L52360:     call "NUMTEST" (runh$(c%), 0, 6e5,errormsg$,-0.4,test)
                if errormsg$ <> " " then return
            gosub cnvrt_hours_2_units
            call "CONVERT" (test1, 0.4, run$(c%))
L52400:     call "CONVERT" (test, 0.4, runh$(c%))
            return

L52430: REM TEST DATA FOR TO ROUTE STEP
            if enabled% = 0 then return
            if step$(c%,2) = "DONE" then return
            if step$(c%,2) <> " " then L52490
                errormsg$ = "Sorry, 'To Step' Can Not Be Blank."
                return
L52490:     temp% = pos(step$(c%,2) = "-")
            if str(step$(c%,2),5) = " " and temp% = 0 then L52570
            if temp% > 0 then L52550
                errormsg$ = "Step Can't be Longer Then 4 Unless a Da" &  ~
                            "sh is Used to Denote Phantom Steps."
                return
L52550:     convert str(step$(c%,2),temp%) to temp%, data goto L52590
            if temp% > 99 then L52590
L52570:     defstep$ = step$(c%,2)
                return
L52590:     errormsg$ = "Invalid Phantom Indicator."
                return

L52620: REM TEST DATA FOR TO WORK CENTER
            if enabled% = 0 then return
            if wc$(c%,2) = "VEND" then return
            call "GETCODE" (#4, wc$(c%,2), " ", 0%, 0, f1%(4))
                if f1%(4) <> 0 then return
                errormsg$="Invalid Entry For 'To' Work Center"
                return

L52700: REM TEST DATA FOR TO ACTIVITY CODE
            if enabled% = 0% then return
            if actv$(c%,2) = " " then return
                readkey$ = "WC ACTVTY" & actv$(c%,2)
                call "PLOWCODE" (#6, readkey$, " ", 9%, 0.3, f1%(6))
                if f1%(6) = 1% then actv$(c%,2) = str(readkey$,10)
                if f1%(6) = 1% then L52620
                     errormsg$="Invalid Entry For 'To' Activity Code"
                     return

L52800: REM TEST DATA FOR TRANSACTION DATE & TIME
            if date$(c%) = " " or date$(c%) = blankdate$ then date$(c%) = date
            call "DATEOK" (date$(c%), 0%, errormsg$)
                if errormsg$ <> " " then return
            lastdate$ = date$(c%)
            if time$(c%) = " " then return
            call "TIMEOK" (time$(c%), temp, errormsg$)
            return

L52890: REM TEST DATA FOR EMPLOYEE WHO MOVED
            if emp$(c%) = " " then return
            call "GETEMPL" (#5, emp$(c%), " ", 0%, f1%(5))
                if f1%(5) <> 0 then return
            errormsg$ = "Invalid Entry For Employee Code"
            return

L52960: REM TEST DATA FOR MEMO TEXT
            return

L52990: REM TEST DATA FOR STEP CLOSED FLAG
            if scflag$(c%) = " " then return
            if scflag$(c%) = "Y" or scflag$(c%) = "N" then return
            errormsg$ = "Enter 'Y', 'N' or leave the field blank."
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 3.                       *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L53160,         /* STARTING JOB     */~
                                    L53190          /* ENDING JOB       */
                     return
L53160:     REM TEST DATA FOR STARTING CODE
            return

L53190:     REM TEST DATA FOR ENDING CODE
            if endcode$ = " " then return
            if startcode$ > endcode$ then errormsg$ = "Ending Job Number ~
        ~Can't Be Greater Then The Starting Job Number"
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 4.                       *~
            *************************************************************

            deffn'154(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L53360,         /* COPY FROM        */~
                                    L53410          /* COPY TO          */
                     return
L53360:     REM TEST DATA FOR REPORT TO BE COPIED
            call "GETCODE"(#1,startcode$,str(display$(1),,30),1%,0,f1%(1))
                if f1%(1) = 0 then errormsg$ = hex(00)
            return

L53410:     REM TEST DATA FOR NEW REPORT ID.
            call "GETCODE" (#1, endcode$, " ", 0%, 99, f1%(1))
                if f1%(1) <> 0 then errormsg$ = "Can't Be The Same As An ~
        ~existing Report: " & endcode$
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'156(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L53640,         /* Show Handling Factor   */~
                              L53670          /* Default "Clear WCOUTs?"*/
            return

L53640: REM Test for Activate Handling Factor -   SW_HANDFACTOR$
            if pos("YN" = sw_handfactor$) > 0% then return
                errormsg$ = "Please enter either 'Y' or 'N'"
            return

L53670: REM Test for Default Clear WCOUTs?        SW_CLEAR_WCOUT$
            if pos("YN" = sw_clear_wcout$) > 0% then return
                errormsg$ = "Please enter either 'Y' or 'N'"
                return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

        exit_program
            end
