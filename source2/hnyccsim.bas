        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y   CCC    CCC    SSS   IIIII  M   M   *~
            *  H   H  NN  N  Y   Y  C   C  C   C  S        I    MM MM   *~
            *  HHHHH  N N N   YYY   C      C       SSS     I    M M M   *~
            *  H   H  N  NN    Y    C   C  C   C      S    I    M   M   *~
            *  H   H  N   N    Y     CCC    CCC    SSS   IIIII  M   M   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYCCSIM - This program will simulate the cycle count     *~
            *            process using the HNYCCMST file.  The cycle    *~
            *            count period and simulation time are user set. *~
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
            * 03/11/92 ! Original                                 ! RJ1 *~
            * 01/12/93 ! Page 0 Facs fix                          ! RJH *~
            * 02/09/93 ! Insure Count Period is set               ! RJH *~
            * 04/07/93 ! Insure Count Period is set(really)       ! RJH *~
            * 08/08/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**


        dim                                                              ~
            abcclass$1,                  /* ABC Class                  */~
            actflag$1,                   /* Count session Activity Flag*/~
            askflag$1,                   /* Askuser Flag               */~
            aveperiod$10,                /* Average Cycle Count Period */~
            basedate$8,                  /* Temp Starting Date         */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            ccperiod$3,                  /* Cycle Count Period         */~
            cntdate$8,                   /* Count Date of Simulation   */~
            cntnbr$10,                   /* Count session number       */~
            cntperday$10,                /* Counts per Day             */~
            cntperiod$3,                 /* Counts per Day             */~
            cntquan$10,                  /* Quantity of parts/Session  */~
            columnttl$51,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            count$10,                    /* Item Count in a given Sessn*/~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            datelastcnt$8,               /* Last Count Date            */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            firstcntdate$8,              /* First Date Part Counted    */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lastccperiod$3,              /* Last Cycle Count Period    */~
            lastcntquan$10,              /* Last Quan of parts/Session */~
            lastlafactor$6,              /* Last  Look Ahead Factor    */~
            lastsimperiod$4,             /* Last  Simulation Period    */~
            lastsesdate$8,               /* Final Session Date         */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lookaheadfactor$6,           /* Look Ahead Factor          */~
            lot$16,                      /* Lot Name                   */~
            maxcntperiod$10,             /* Maximum Count Period       */~
            mincntperiod$10,             /* Minimum Count Period       */~
            nextsesdate$8,               /* Next Session Date          */~
            nextcntdate$8,               /* Next Date for CC Session   */~
            nxdate1$8,                   /* Temp Next Count Date       */~
            nxdate2$8,                   /* Temp Next Count Date       */~
            part$25,                     /* Part Name                  */~
            partstrlot$44,               /* Part/Store/Lot Key         */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plancntdate$8,               /* Planed Date for Next Count */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            prdchngdate$8,               /* Date Period changed by user*/~
            quan$10,                     /* Quantity from HNYQUAN      */~
            quantot$10,                  /* Printing Quantity Totals   */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            recdate$8,                   /* Record Start Date          */~
            rpttitle$80,                 /* Report Title               */~
            sessiondate$8,               /* Session Date               */~
            sessiondesc$30,              /* Session Description        */~
            sessionname$16,              /* Session Name               */~
            sessionnum$3,                /* Session Iterated Number    */~
            sessionpre$12,               /* Session Name Prefix        */~
            sessiontot$10,               /* Session Quantity Totals    */~
            simperiod$4,                 /* Simulation Time Period     */~
            startdate$8,                 /* Temp Starting Date         */~
            store$3,                     /* Store/Warehouse Name       */~
            testprddate$8,               /* Future Count Date for check*/~
            testsesname$16,              /* Session Name (Test)        */~
            totcntpercycle$10,           /* Counts/Cycle Totals        */~
            typeincl$5,                  /* Simulation Inclusion Type  */~
            time$8,                      /* System Time                */~
            userid$3,                    /* Current User Id            */~
            workfil0flag$1               /* Work File 0 Done Flag      */

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
            * #02 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * #03 ! HNYCCMST ! Cycle Count Master File                  *~
            * #50 ! WORKFIL0 ! Workfile for Master File Simulation      *~
            * #51 ! WORKFIL1 ! Workfile for Master File Updates         *~
            * #52 ! WORKFIL2 ! Workfile for Simulation Session Results  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #02, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44          ~

            select #03, "HNYCCMST",                                      ~
                        varc,     indexed,  recsize =  796,              ~
                        keypos =    1, keylen =  44,                     ~
                        alt key  1, keypos =   45, keylen =   6, dup,    ~
                            key  2, keypos =   81, keylen =   7, dup,    ~
                            key  3, keypos =   73, keylen =  15, dup

            select #50, "WORKFIL0",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  44,                     ~
                        alt key  1, keypos =   45, keylen =  16, dup,    ~
                            key  2, keypos =   55, keylen =   6, dup

            select #51, "WORKFIL1",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  44

            select #52, "WORKFIL2",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  70                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

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
            call "COMPNAME" (12%, company$, ret%)
            ret% = ret%

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "           Session Simulation --- "
            rptid$ = "HNY049"

            str(columnttl$, 1) = " "
            str(columnttl$,27) = " "

            str(line2$,62) = "HNYCCSIM: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  7%
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
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% >  7% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
L11150:     gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
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

            if workfil0flag$ = "Y" then  L13130  /* Chk if already done? */
                gosub open_workfiles
              /* ** Make Temp File of HNYCCMST ** */
                gosub build_master_workfil0

           /* Check for Valid Input Entries after Data from Master Check*/
L13130:     gosub check_ccperiod
            if errormsg$  = " " then  goto L13160
                fieldnr% = 4%  :  goto L11150
L13160:     gosub check_simperiod
            if errormsg$  = " " then  goto L13190
                fieldnr% = 5%  :  goto L11150
L13190:     gosub check_cntperday
            if errormsg$  = " " then  goto L13240
                fieldnr% = 6%  :  goto L11150

           /* ** Do the Simulation ** */
L13240:     gosub run_simulation       /* Lets do it */

           /* ** Print the Report ** */
            goto generate_report
           /* *****  END  Extract Data Sub **** */


        build_master_workfil0   /* ** Simulation File from HNYCCMST ** */

            call "SHOSTAT" ("BUILDING THE SIMULATION WORK FILE")
            counter% = 0% : mincntperiod = 99999999 : maxcntperiod = 0
            basedate$ = sessiondate$  :  call "DATUNFMT" (basedate$)
            plowkey$ = all(hex(00))

            call "READ104" (#03, plowkey$, f1%(03))
            goto L13630

        loop_master      /* Loop Poin */
            call "READNEXT" (#03, f1%(03))
L13630:     if f1%(03) = 0% then return

            get #03  using L35060, partstrlot$, datelastcnt$, cntnbr,     ~
                                  actflag$, nextcntdate$, recdate$,      ~
                                  abcclass$, cntperiod$, prdchngdate$

            /* ** Get Quantity ** */
            readkey$ = partstrlot$
            call "READ100" (#02, readkey$, f1%(02))
            if f1%(02) <> 0% then L13770
                   call "SHOSTAT" ("NO HNYQUAN RECORD TO MATCH HNYCCMST")
                   goto loop_master
L13770:     get #02  using L35100, quan

            /* ** Calc. Total Counts per Day ** */
            convert cntperiod$ to cntperiod, data goto loop_master
            if cntperiod = 0 then goto loop_master   /* Gota be set */
            cntperiod% = int(cntperiod)
            if quan > 0 then cntperday = quan / cntperiod                ~
                        else cntperday = 1 / cntperiod
            totcntperday = totcntperday + cntperday
            call "CONVERT" (cntperday, 4.4, cntperday$)
            counter% = counter% + 1%

            gosub set_original_next_count_date /* Set starting point */

            if nextcntdate$  <>  " " and  ~
               nextcntdate$  <>  blankdate$ then L13910

               nextcntdate$ = basedate$  /* Got to give it something */

L13910:     if mincntperiod > cntperiod then  mincntperiod = cntperiod
            if maxcntperiod < cntperiod then  maxcntperiod = cntperiod

            call "CONVERT" (quan   , 2.2 , quan$)

            if f1%(03) = 0% then return

            firstcntdate$ = " " : cntnbr$ = "         0"
            prdchngdate$ = basedate$
            put #50  using L35130, partstrlot$, cntnbr$, nextcntdate$,    ~
                                  actflag$, datelastcnt$, recdate$,      ~
                                  abcclass$, cntperiod$, quan$,          ~
                                  cntperday$, prdchngdate$, firstcntdate$
            write #50
            workfil0flag$ = "Y"

            goto loop_master
          /* *** End LOOP_MASTER Sub Function *** */

          /* *** End BUILD_MASTER_WORKFIL0 Sub Function *** */



        check_ccperiod
            errormsg$ = " "
            if lastccperiod$ = ccperiod$  then return /* Accept 2nd Time*/

            convert ccperiod$ to ccperiod
            if ccperiod <= mincntperiod then return
                call "CONVERT" (mincntperiod, -0.01, mincntperiod$)
                errormsg$ = "Simulation Sessions Period is Greater than" ~
                          & " the Minimum Period of " & mincntperiod$
                aukey% = 1%  :  gosub checking_askuser
                lastccperiod$ = ccperiod$
            return

        check_simperiod
            errormsg$ = " "
            if lastsimperiod$ = simperiod$ then return /*Accept 2nd Time*/

            if simperiod >= maxcntperiod then return
                call "CONVERT" (maxcntperiod, -0.01, maxcntperiod$)
                errormsg$ = "Simulation Length is Less than the"         ~
                          & " Maximum Period of " & maxcntperiod$
                aukey% = 2%  :  gosub checking_askuser
                lastsimperiod$ = simperiod$
            return

        check_cntperday   /* ** Input Quantity must be within 10% ** */
            errormsg$ = " "     /* ** User may accept second time ** */
            if lastcntquan$ = cntquan$   then return

            convert cntquan$ to cntquan
            totcntpercycle = ccperiod * totcntperday
            deltacnts = totcntpercycle - cntquan
            if abs(deltacnts) < 0.10 * totcntpercycle then return
                if deltacnts > 0.0 then errormsg$ = "too FEW"            ~
                                   else errormsg$ = "too MANY"
                call "CONVERT" (totcntpercycle, -0.01, totcntpercycle$)
                errormsg$ = "Simulation will Count " & errormsg$         ~
                          & " Items.  Recommended Quantiy is "           ~
                          & totcntpercycle$
                aukey% = 1%  :  gosub checking_askuser
                lastcntquan$ = cntquan$
            return

        set_original_next_count_date  /* Prime the pump for Sim Type    */
            simtype% = 2%                 /* 1)Every part starts equal  */
            if simtype% <> 1% then L14509  /* 2)Each starts 1/4 Prd Ahead*/
                nextcntdate$ = basedate$  /* 3)Use existing Date        */
L14509:     if simtype% <> 2% then L14513
                tempperiod% =  int(cntperiod% * 0.25)
                call "DATE" addr("G+", basedate$, tempperiod%,           ~
                                                 nextcntdate$, err%)
L14513:     return

        run_simulation   /* *** Three passes can be made thru the File. */
            /* for each simulation session.   They are:                 */
            /*1st) Check for NextCountDate Before SessionDate    "EARLY"*/
            /*2nd) Check for NextCountDate Before NextSess'nDate  "LATE"*/
            /*3rd) Grab Parts w/NextCountDate w/In LookAheadFactor"XTRA"*/
            /*    The NextCountDate will be Calculated and the Number of*/
            /* Counts Iterated.  The updated info will be written to a  */
            /* Temp File until End conditions met. Temp file will be    */
            /* used to Rewite Simulation Master File.   End Condition is*/
            /* ** When Total Count Quantity Exceeds the Session Limit   */
            /*(that Part will be the Last Part Included in that Session)*/
            /* or No valid parts to include (END of File). Simulations  */
            /* continue until time limit exceeded.                  *** */

            sessionnum = 1  :  totsesquan = 0.0
            testloop% = 0%  :  count  = 0
            convert  simperiod$ to simperiod
            simperiod% = int(simperiod)
            call "DATUNFMT" (sessiondate$)
            call "DATE" addr("G+", sessiondate$, simperiod%,lastsesdate$,~
                                                                   err%)
            cntdate$ = sessiondate$
            gosub set_session_name
L14660:     call "SHOSTAT" ("RUNNING SIMULATION  -- " &  sessionname$)

            /* Loop thru File checking  NextCountDates */
            gosub start_sim_test_loop  /* Start Type of Checking Loop*/
            if f1%(50) = 0% then return          /* Nothing in File */   ~
                            else L14790           /* Get Record */
        loop_simulation
            call "READNXT1" (#50, f1%(50))
L14740:     if f1%(50) <> 0% then L14790
                gosub start_sim_test_loop
                if testloop% < 4% then L14740                             ~
                                  else L15290 /* ** End This Session ** */

L14790:     get #50 using L35130, partstrlot$, cntnbr$, nextcntdate$,     ~
                                 actflag$, datelastcnt$, recdate$,       ~
                                 abcclass$, cntperiod$, quan$,           ~
                                 cntperday$, prdchngdate$, firstcntdate$

            call "READ100" (#51, partstrlot$, f1%(51))
            if f1%(51) = 1% then loop_simulation    /* Already included */

            convert cntperiod$ to cntperiod
            cntperiod% = cntperiod
            convert quan$ to quan

           /* LATE Using Current Session Earlier than NextCountDate */
            if testloop% <> 1% then L14900
                if nextcntdate$ >  cntdate$ then loop_simulation
                    if nextcntdate$ <  cntdate$ then typeincl$ = "LATE " ~
                                                else typeincl$ = "ONTIM"
                    goto L15030     /* Proccess */
           /* EARLY Using Next Session After NextCountDate */
L14900:     if testloop% <> 2% then L14960

               if nextcntdate$ > nextsesdate$ then loop_simulation       ~
                                                else L15030 /* Proccess */
           /* EXTRA Parts Using LookAheadFactor */
L14960:     if testloop% <> 3% then return        /*  Big Mess IF True */
                testprd% = int(cntperiod% * lookaheadfactor)
                call "DATE" addr("G+", nextsesdate$, testprd%,           ~
                                                   testprddate$, err%)
                if nextcntdate$ > testprddate$ then loop_simulation
            /* Test if Actual Count Period to Short */
                if datelastcnt$ = " " or datelastcnt$ = blankdate$       ~
                                   then L15030 /* Never Counted So Take */
                call "DATE" addr("G-", datelastcnt$, basedate$, testprd%,~
                                                                   err%)
                if testprd% < 0.75 * cntperiod then loop_simulation

                if totsesquan + quan > cntquan/*Going to Count TOO Many*/~
                     then loop_simulation
                  /* ELSE   Fall thru and proccess Extras   */
            /* ** Process Record ** */
L15030:     count = count + 1
            convert cntnbr$ to cntnbr
            cntnbr = cntnbr + 1
            call "CONVERT" (cntnbr, 0.01, cntnbr$)
            call "CONVERT" (count, 0.01, count$)
            plancntdate$ = nextcntdate$
            gosub set_nextcntdate
            if firstcntdate$ = " " or firstcntdate$ = blankdate$ ~
                                 then firstcntdate$ = cntdate$

            /* Set Temp Adjustment File */
            put #51 using L35130, partstrlot$, cntnbr$, nextcntdate$,     ~
                                 actflag$,  cntdate$   , recdate$,       ~
                                 abcclass$, cntperiod$, quan$,           ~
                                 cntperday$, prdchngdate$, firstcntdate$
            write #51

            /* Set Print File */
            put #52 using L35030, sessionname$, cntdate$, count$,         ~
                                 partstrlot$, plancntdate$, quan$,       ~
                                 cntperday$,         nextcntdate$,       ~
                                 cntperiod$,cntnbr$,typeincl$

            write #52

            totsesquan = totsesquan + quan
            if totsesquan < cntquan then goto loop_simulation
L15290:         gosub reset_master_workfil0
                goto start_next_session

L15320:     return
            /* ** END Build Simulation Function ** */

        start_next_session
            sessionnum = sessionnum + 1
            cntdate$ = nextsesdate$
            gosub set_session_name
            totsesquan = 0.0
            count  = 0   :  testloop% =  0%
            if lastsesdate$ >= cntdate$ then goto L14660 /* Loop Again */
                gosub reset_master_workfil0
                goto L15320              /* ** Ending Simulation ** */

        reset_master_workfil0
            readkey$ = all(hex(00))
            call "READ104" (#51, readkey$, f1%(51))
L15490:     if f1%(51) = 0 then L15670 /* All done this round */
            get #51 using L35130, partstrlot$, cntnbr$, nextcntdate$,     ~
                                 actflag$, datelastcnt$, recdate$,       ~
                                 abcclass$, cntperiod$, quan$,           ~
                                 cntperday$, prdchngdate$, firstcntdate$

            call "READ101" (#50, partstrlot$, f1%(50))
            if f1%(50) = 0% then L15670

            put #50 using L35130, partstrlot$, cntnbr$, nextcntdate$,     ~
                                 actflag$, datelastcnt$, recdate$,       ~
                                 abcclass$, cntperiod$, quan$,           ~
                                 cntperday$, prdchngdate$, firstcntdate$
            rewrite #50

            call "READNEXT" (#51, f1%(51))
            goto L15490

L15670:     call "FILEBGON" (#51)
            call "WORKOPEN" (#51, "IO", 100%, f2%(51))
            return

        start_sim_test_loop
            testloop% = testloop% + 1%
            plowkey$ = all(hex(00))
            if testloop% >  1% then L15770
                typeincl$ = "LATE"
                goto L15790
L15770:     if testloop% >  2% then L15810
                typeincl$ = "EARLY"
L15790:         call "REDALT4" (#50, plowkey$, 2%, f1%(50))
*              IF PLANCNTDATE$ = SESSIONDATE$ THEN TYPEINCL$ = "ONTIM"
                return
L15810:     if testloop% <> 3% then L15840
                typeincl$ = "EXTRA"
                call "REDALT4" (#50, plowkey$, 2%, f1%(50))
L15840:     return

        set_session_name
            convert sessionnum to sessionnum$, pic(000)
            sessionname$ = str(sessionpre$)  &  "-"  &  sessionnum$
            ccperiod% = int(ccperiod)
            call "DATE" addr("G+",cntdate$,ccperiod%,nextsesdate$,err%)
            return

        set_nextcntdate  /* Calc. the Most Advanced Next Count Date. */
            convert cntnbr$ to cntnbr
            cntnbr% = int(cntnbr)
            cntperiod% = int(cntperiod)

            if prdchngdate$ <> " " and prdchngdate$ <> blankdate$         ~
               and recdate$ < prdchngdate$ then startdate$ = prdchngdate$ ~
               else startdate$ = recdate$
            if firstcntdate$ > startdate$ then   startdate$ = firstcntdate$
            days% = cntnbr% * cntperiod%
            call "DATE" addr ("G+", startdate$, days%, nxdate1$,err%)
            call "DATE" addr ("G+", cntdate$, cntperiod%, nxdate2$,err%)
            if nxdate1$ < nxdate2$ then nextcntdate$ = nxdate2$          ~
                                   else nextcntdate$ = nxdate1$
            return

        open_workfiles
            call "FILEBGON" addr(#50)
            call "FILEBGON" addr(#51)
            call "FILEBGON" addr(#52)
            call "WORKOPEN" (#50, "IO", 100%, f2%(50))
            call "WORKOPEN" (#51, "IO", 100%, f2%(51))
            call "WORKOPEN" (#52, "IO", 100%, f2%(52))
            return

        checking_askuser
            askflag$ = "Y"
L16180:     call "ASKUSER" (aukey%, "** SIMULATION **  ",                ~
                            errormsg$,                                   ~
                            "Press PF(16) To Continue Simulation     ",  ~
                            "Press (RETURN) To RE-ENTER              ")
            if aukey% = 16% then goto L16270
            if aukey% <> 0% then goto L16180
            pf16$ = "(16)Continue Sim"  :  return

L16270:     errormsg$ = " "             :  return
          /* ** End AskUser Check ** */

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Session Name Pref      */~
                              L20200,         /* Session Description    */~
                              L20300,         /* Session Date           */~
                              L20400,         /* Cycle Count Perio      */~
                              L20500,         /* Sim. Time Period       */~
                              L20600,         /* Quantity of parts      */~
                              L20700          /* Look Ahead Factor      */
            return
L20100: REM Def/Enable Session Name Prefix           SESSNPRE$


            return

L20200: REM Def/Enable Session Description           SESSIONDESC$


            return

L20300: REM Def/Enable Session Date                  SESSIONDATE$
            sessiondate$ = date$

            return

L20400: REM Def/Enable Cycle Count Period            CCPERIOD$


            return

L20500: REM Def/Enable Simulation Time Period        SIMPERIOD$


            return

L20600: REM Def/Enable Quantity of parts/Session     CNTQUAN$


            return

L20700: REM Def/Enable Look ahead factor             LOOKAHEADFACTOR$
            lookaheadfactor$  =  "0.5"

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
         "Enter Simulation Name                                        ",~
         "Enter Simulation Description                                 ",~
         "Enter Simulation Start Date                                  ",~
         "Enter Simulation Period (days)                               ",~
         "Enter Number of Days for Simulation                          ",~
         "Enter Quantity of parts per Session to Count                 ",~
         "Enter Decimal Ratio of a Part's Period for Extra Parts Inclsn"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, ccperiod$, cntquan$,       ~
                      lastcntquan$, lastsimperiod$, lastccperiod$,       ~
                      sessiondate$,  sessionpre$, askflag$,              ~
                      simperiod$, sessiondesc$,workfil0flag$,            ~
                      lastlafactor$, lookaheadfactor$, cntdate$,         ~
                      nextsesdate$
            count, mincntperiod, maxcntperiod, totcntperday = 0
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
        generate_report
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("RPTID", " ", 0%, 0%)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
            str(rpttitle$,35,) = sessionpre$
            if lcntr% > 56% then gosub page_head

            testsesname$ = " "   :   quantot = 0  :  sessiontot = 0
            call "SHOSTAT" ("PRINTING REPORT NOW")

            /* ** Begin Report Loop  ** */
            plowkey$ = all(hex(00))
            call "READ104" (#52, plowkey$, f1%(52))
            goto L30230

        loop_printing
            call "READNEXT" (#52, f1%(52))
L30230:     if f1%(52) <> 0% then L30270
                gosub print_totals
                goto print_summary

L30270:     get #52 using L35030, sessionname$, cntdate$, count$,         ~
                                 partstrlot$, plancntdate$, quan$,       ~
                                 cntperday$,         nextcntdate$,       ~
                                 cntperiod$,cntnbr$, typeincl$

            lcntr% = lcntr% + 1%
            if lcntr% < 57 then L30370
                gosub page_head   :  gosub column_head


L30370:     if testsesname$ = sessionname$ then L30410
                gosub session_head
                testsesname$ = sessionname$

L30410:     call "DATEFMT" (nextcntdate$)
            call "DATEFMT" (plancntdate$)
            call "RJUSTIFY" (cntperiod$)
            convert quan$ to quan
            quantot = quantot + quan : sessiontot = sessiontot + quan
            part$  = str(partstrlot$,,25)
            store$ = str(partstrlot$,26,3)
            lot$   = str(partstrlot$,29,16)


            print using L60190, part$, store$, lot$, cntnbr$, typeincl$,  ~
                               cntperiod$,quan$,plancntdate$,nextcntdate$

        goto loop_printing
            /* *** End Print Loop *** */

        print_summary
            gosub page_head
            print using  L60286
            print
            print using  L60294
            print using  L60302
            lcntr% = lcntr% + 4%
            plowkey$ = all(hex(00))
            call "READ104" (#50, plowkey$, f1%(50))
            goto L30670

L30660:     call "READNEXT" (#50, f1%(50))
L30670:     if f1%(50) = 0% then goto end_report
            get #50  using L35130, partstrlot$, cntnbr$, nextcntdate$,    ~
                                  actflag$, datelastcnt$, recdate$,      ~
                                  abcclass$, cntperiod$, quan$,          ~
                                  cntperday$, prdchngdate$, firstcntdate$

            call "DATE" addr("G-",firstcntdate$,datelastcnt$,deltadate%, ~
                                                                   err%)
            convert cntnbr$ to cntnbr
            if cntnbr  > 1 then L30770
                aveperiod$ =  " "  :  goto L30784
L30770:     aveperiod = deltadate% / (cntnbr - 1)
            call "CONVERT" (aveperiod, 2.2, aveperiod$)
L30784:     call "RJUSTIFY" (cntnbr$)
            call "RJUSTIFY" (cntperiod$)
            part$  = str(partstrlot$,,25)
            store$ = str(partstrlot$,26,3)
            lot$   = str(partstrlot$,29,16)
            lcntr% = lcntr% + 1%
            if lcntr% < 57% then L30850
                gosub page_head
                print
                print using L60294
                print using L60302
                lcntr% = lcntr% + 3%
L30850:     print using L60310 , part$, store$, lot$, cntnbr$, cntperiod$,~
                                aveperiod$
            goto L30660

        column_head
            print using L60150
            print using L60175
            lcntr% = lcntr% + 2%
            return

        session_head
            call "DATEFMT" (cntdate$)
            if testsesname$ = " " then L31020
                call "CONVERT" (sessiontot , 2.2, sessiontot$)
                print using  L60230, sessiontot$
                lcntr% = lcntr% + 1%
L31020:     if lcntr% > 53 then gosub page_head
            print
            print using L60350, sessionname$, cntdate$
            lcntr% = lcntr% + 2%   :  sessiontot = 0
            gosub column_head
            return

        print_totals
            call "CONVERT" (sessiontot , 2.2, sessiontot$)
            print using  L60230, sessiontot$
            call "CONVERT" (quantot , 2.2, quantot$)
            print using  L60270, quantot$
            return


        end_report                /* Report Ending Routine */
            time$ = " "  :  call "TIME" (time$)
            print skip(2)
            print using L64990, time$        /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            goto inputmode

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "HNYCCSIM", rptid$
            print using L60110, rpttitle$, pcntr%
            print
            if pcntr% = 0% then gosub print_params
            lcntr% = 3%
            return

        print_params           /* Print Page Zero */
L34510:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L34550
                str(i$(), i%, 1%) = hex(20)
                goto L34510
L34550:     print skip(3)
            print tab(26);
            print "----------------------Simulation Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 19% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
*          PCNTR% = PCNTR% + 1%
            gosub page_head
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************
            /* ** WORKFIL2 for Session Report ** */
L35030:     FMT CH(16), CH(6), CH(10),CH(44), CH(6), CH(10), CH(10),     ~
                CH(6), CH(03), CH(10), CH(5)

            /* HNYCCMST File */
L35060:     FMT CH(44), POS(51), CH(6), POS(73), PD(14,4), CH(1), CH(6), ~
                CH(6),  POS(127), CH(1), POS(181), CH(3), POS(190), CH(6)

            /* HNYQUAN */
L35100:     FMT POS(69) , PD(14,4)

            /* WORKFIL0  and WORKFIL1 */
L35130:     FMT CH(44), CH(10), CH(6), CH(1), CH(6), CH(6),              ~
                CH(1), CH(3), CH(10), CH(10), CH(6), CH(6)

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
              on fieldnr% gosub L40095,         /* Session Name Pref */   ~
                                L40090,         /* Session Description*/  ~
                                L40095,         /* Session Date      */   ~
                                L40100,         /* Cycle Count Perio */   ~
                                L40100,         /* Sim. Time Period  */   ~
                                L40100,         /* Quantity of parts */   ~
                                L40100          /* Look Ahead Factor */
              goto L40110

L40090:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40095:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40100:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40110:     accept                                                       ~
               at (01,02),                                               ~
                  "Input Simulation Criteria",                           ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Simulation Name    ",                        ~
               at (06,32), fac(lfac$( 1)),   sessionpre$        , ch(12),~
                                                                         ~
               at (07,02), "Description        ",                        ~
               at (07,32), fac(lfac$( 2)),   sessiondesc$       , ch(30),~
                                                                         ~
               at (08,02), "Simulation Start Date",                      ~
               at (08,32), fac(lfac$( 3)),   sessiondate$       , ch(08),~
                                                                         ~
               at (09,02), "Simulation Period           ",               ~
               at (09,32), fac(lfac$( 4)),   ccperiod$          , ch(03),~
                                                                         ~
               at (10,02), "Simulation Length           ",               ~
               at (10,32), fac(lfac$( 5)),   simperiod$         , ch(04),~
                                                                         ~
               at (11,02), "Quantity of Parts/Session",                  ~
               at (11,32), fac(lfac$( 6)),   cntquan$           , ch(10),~
                                                                         ~
               at (12,02), "Look Ahead Factor        ",                  ~
               at (12,32), fac(lfac$( 7)),   lookaheadfactor$   , ch(06),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(84)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40315
                  call "MANUAL" ("HNYCCSIM") : goto L40110

L40315:        if keyhit% <> 15 then L40330
                  call "PRNTSCRN" : goto L40110

L40330:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40425     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40410
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% > 1% then L40415
L40410:         str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40415:     return

L40425: if fieldnr% > 0% then L40470  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Simulation"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            if askflag$ = "Y" then str(pf$(3),64,16) = pf16$
            return
L40470:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100,         /* Session Name Pref      */~
                              L50200,         /* Session Description    */~
                              L50300,         /* Session Date           */~
                              L50400,         /* Cycle Count Perio      */~
                              L50500,         /* Sim. Time Period       */~
                              L50600,         /* Quantity of parts      */~
                              L50700          /* Look Ahead Factor      */
            return
L50100: REM Test for Session Name Prefix          SESSIONPRE$
            if sessionpre$ <> " " then return
                errormsg$ = "Enter Session Name"
            return

L50200: REM Test for Session Description          SESSIONDESC$
            return

L50300: REM Test for Session Date                 SESSIONDATE$

            call "DATEOK" (sessiondate$, sessiondate%, errormsg$)
            call "DATEOK" (date$, date%, errormsg$)
            if errormsg$ <> " " then return
            if sessiondate% < date% then errormsg$ = "Session Date Must" ~
                               & " Be Greater or Equal to Today's Date"
            return

L50400: REM Test for Cycle Count Period           CCPERIOD$
            if ccperiod$ <> " " then L50410
                errormsg$ = "'Blank' Not Allowed"  :  return
L50410:     convert ccperiod$ to ccperiod
            if ccperiod > 0 then L50430
                errormsg$ = "Cycle Count Period Must be Greater Than Zero"
L50430:     if workfil0flag$ = "Y" then gosub check_ccperiod
            return

L50500: REM Test for Simulation Time Period       SIMPERIOD$
            if simperiod$ <> " " then L50520
                errormsg$ = "'Blank' Not Allowed"  :  return
L50520:     convert simperiod$ to simperiod
            if simperiod > 0 then L50550
                errormsg$ = "Simulation Period Must be Greater Than Zero"
L50550:     if workfil0flag$ = "Y" then gosub check_simperiod
            return

L50600: REM Test for Quantity of parts/Session    CNTQUAN$




            return

L50700: REM Test for Look Ahead Factor            LOOKAHEADFACTOR$
            /* Multiplier for how far into the fulture for          */
            /* NextCountDate to still be include as an EXTRA.   */
            if lookaheadfactor$  <> " " then L50725
                errormsg$ = "'Blank' Not Allowed"
L50725:     if lastlafactor$ = lookaheadfactor$ then return/*Must WantAh*/
            convert  lookaheadfactor$ to lookaheadfactor
            if lookaheadfactor  <= 1.0  then L50760
            errormsg$ = "Not Recomended to have Factor Greater Than 1.0"
            lastlafactor$ = lookaheadfactor$

L50760:     return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:######

*       * Header Line 2
L60110: %                                     ###########################~
        ~#################################                     PAGE: ####

*       * Column Header Line 1
L60150:  %      PART NUMBER                STR  LOT     NUM COUNTS  PICKD~
        ~ CNT PERIOD  QUANTITY    PLANNEDCNT  NEXT CNT

*       * Column Header Line 2
L60175:  %      -------------------------  ---  ------  ----------  -----~
        ~ ----------  ----------  ----------  ----------

*       * Report Line
L60190:  %      #########################  ###  ######  ##########  #####~
        ~        ###  ##########   ########    ########

*       * Session Totals
L60230: %                                                                ~
        ~  ** TOTAL: ########## **

*       * Grand Totals
L60270: %                                                          ***  G~
        ~RAND TOTAL: ##########  ***

*       * Summary Header Line 1
L60286: %                                ** SIMULATION SUMMARY **

*       * Summary Header Line 2
L60294: %      PART NUMBER                STR  LOT     NUM COUNTS  CNT PE~
        ~RIOD  AVG PERIOD

*       * Summary Header Line 3
L60302: %      -------------------------  ---  ------  ----------  ------~
        ~----  ----------

*       * Summary Line
L60310: %      #########################  ###  ######  ##########        ~
        ~ ###  ##########

*       * Simulation Header
L60350: % SIMULATION SESSION: ################   DATE:  ########

        %** Report Title for page 0
        %############################################################

L64990:         %                          * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   @   ########   * * * * * * * * * *

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
