        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y  TTTTT  U   U  RRRR   N   N   SSS    *~
            *  H   H  NN  N  Y   Y    T    U   U  R   R  NN  N  S       *~
            *  HHHHH  N N N   YYY     T    U   U  RRRR   N N N   SSS    *~
            *  H   H  N  NN    Y      T    U   U  R   R  N  NN      S   *~
            *  H   H  N   N    Y      T     UUU   R   R  N   N   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYTURNS - This Report will generate a report which shows *~
            *            the number of times the inventory has turned   *~
            *            for a Part.  The user will determine the source*~
            *            of the transaction values and current Inventory*~
            *            values used for the calculation of the ratios. *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/07/91 ! Original                                 ! RJB *~
            * 02/11/93 ! PRR 12340 - Errormsg spelling correction.! RJH *~
            *          ! PRR 12338 - Shostat spelling correction. !     *~
            *          ! PRR 12339 - Suprss 2nd srt if 1st by Part!     *~
            *          !  While in here, add time stamp to End of !     *~
            *          !   Report, tweek page 0 header, & clean up!     *~
            *          !   some undimensioned variables.          !     *~
            * 04/06/93 ! QC changes - Use DATA GOTO for possible  ! RJH *~
            *          !   Convert errors. Use new TESTRNGE for   !     *~
            *          !   Part and Category selections. Allign   !     *~
            *          !   Variable on Screen display & use Descr.!     *~
            *          !   for Sort Selections.                   !     *~
            * 05/09/95 ! PRR 13411 - Corrected possible divide by ! MLJ *~
            *          !   0 when calculating turns. Now rounds & !     *~
            *          !   checks result for <= 0 before dividing.!     *~
            * 12/06/95 ! PRR 13540 - Corrected determination of   ! JDH *~
            *          !   ending date when reporting on the last !     *~
            *          !   period of a 12 period year.  Thx Jeff. !     *~
            *          ! Added PF14 to view the report.           !     *~
            * 12/18/95 ! Chgd Deflt for Weight Factor. Ok Maree?  ! JDH *~
	    * 09/06/96 ! Millie date conversion                   ! DER *~
            * 04/04/97 ! Change PUTPARM call for NT Compatibility ! LDJ *~
            * 07/11/01 ! (EWD001) - Mod to add Vendor to Report.  ! CMG *~
            * 08/20/01 ! (EWD002) - Mod to add transaction code   ! CMG *~
            *          !            'IA' to turns calculation.    !     *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            basedate$8,                  /* G/L Period 1's Date        */~
            blankdate$8,                 /* blank unfmt date           */~
            company$60,                  /* Company Name               */~
            currprd$2,                   /* Current G/L Period         */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            ecat$4,                      /* Ending Part Category       */~
            edate$10,                    /* Ending Date                */~
            edtmessage$79,               /* Edit screen message        */~
            epart$25,                    /* Ending Part Number         */~
            eperiod$2,                   /* Ending G/L Period          */~
            evendor$9,                   /* Ending Vendor Code (EWD001)*/~
            errormsg$79,                 /* Error message              */~
            factor$4,                    /* Weighting Factor           */~
            firstdate$8,                 /* First Transaction Date     */~
            firstperiod$2,               /* First Transaction Period   */~
            gldate$(17)8,                /* G/L Period Dates           */~
            hnydate$6,                   /* Inventory Posting Date     */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            invvalue$14,                 /* Inventory Value (display)  */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            mcat$4,                      /* HNYMASTR Part Category     */~
            mpart$25,                    /* HNYMASTR Part Number       */~
            mpdesc$32,                   /* HNYMASTR Part Description  */~
            mvendor$9,                   /* HNYMASTR Vendor Code  EWD001 */~
            numprds$2,                   /* Max # of G/L Periods       */~
            pdescr$30,                   /* Primary Sort Description   */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowprime$99,                /* Primary PLOW KEY           */~
            plowtwo$99,                  /* Second  PLOW KEY           */~
            plowthree$99,                /* Third   PLOW KEY           */~
            psort$1,                     /* Primary Sort Flag          */~
            rcat1$4,                     /* Category Range (Low)       */~
            rcat2$4,                     /* Category Range (High)      */~
            rdate1$8,                    /* Temp     Date              */~
            rdate2$8,                    /* Temp     Date              */~
            rpart1$25,                   /* Part Number Range (Low)    */~
            rpart2$25,                   /* Part Number Range (High)   */~
            rven1$9,                     /* Vendor Code Range (Low)    */~
            rven2$9,                     /* Vendor Code Range (High)   */~
            runtime$8,                   /* Report Run Time            */~
            scat$4,                      /* Starting Part Category     */~
            sdate$10,                    /* Starting Date              */~
            sdescr$30,                   /* Secondary Sort Description */~
            spart$25,                    /* Starting Part Number       */~
            speriod$2,                   /* Starting G/L Period        */~
            svendor$9,                   /* Starting Vendor Code EWD001*/~
            ssort$1,                     /* Secondary Sort Flag        */~
            store$3,                     /* Store Code                 */~
            storedescr$30,               /* Store Description          */~
            trancode$2,                  /* Tranaction Code (HNYDETAL) */~
            tranvalue$14,                /* Transaction Value (display)*/~
            turns$13,                    /* Inventory Turns (Display)  */~
            userid$3,                    /* Current User Id            */~
            workkey$51                   /* Workfile Key               */

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
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #02 ! HNYMASTR ! Inventory Master File                    *~
            * #03 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * #05 ! HNYDETAL ! INVENTORY DETAILS                        *~
            * #06 ! STORNAME ! STORE INFORMATION FILE                   *~
            * #07 ! CATEGORY ! Part Category File                       *~
            * #08 ! WORKFILE ! Temporary System Workfile                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #02, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #03, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #05, "HNYDETAL",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =   43, keylen =   6, dup,    ~
                            key  2, keypos =   49, keylen =   2, dup

            select #06, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3

            select #07, "CATEGORY",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen =   4

            select #08, "WORKFILE",                                      ~
                        varc,     indexed,  recsize = 156,               ~
                        keypos =    1, keylen =  51

            select #09, "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos = 1, keylen =  9,                         ~
                        alt key  1, keypos = 10, keylen = 30, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
            call "OPENCHCK" (#05, fs%(05), f2%(05), 0%, rslt$(05))
            call "OPENCHCK" (#06, fs%(06), f2%(06), 0%, rslt$(06))
            call "OPENCHCK" (#07, fs%(07), f2%(07), 0%, rslt$(07))
            call "OPENCHCK" (#09, fs%(09), f2%(09), 0%, rslt$(09))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            init(" ") basedate$, currprd$, date$, edtmessage$, gldate$(),~
                      line2$
            numprds%, currprd% = 0%

            call "EXTRACT" addr("ID", userid$)
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "HNYTURNS: " & str(cms2v$,,8)

            if f2%(01) = 0% then L09200
L09150:         keyhit1% = 0%
                call "ASKUSER" (keyhit1%, "Unable to obtain G/L Dates",  ~
                                "Press any key to Exit Program.")
                goto exit_program

L09200:     call "READ100" (#1, "FISCAL DATES", f1%(1))
                if f1%(1) = 0% then L09150
                get #1 using L09230, numprds%, gldate$(), currprd%
L09230:             FMT POS(21), BI(2), 17*CH(8), BI(2)
                basedate$ = gldate$(1)
                call "DATEFMT" (basedate$)
                convert currprd% to currprd$, pic(00)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 8%
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
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then       dataload
                  if keyhit%  = 16% then       dataload
                  if keyhit% <>  0% then       editpg1
L11120:     if cursor%(1) < 12% then fieldnr% = cursor%(1%) - 6%
            if cursor%(1) = 14% then fieldnr% = cursor%(1%) - 8%
            if cursor%(1) > 16% then fieldnr% = cursor%(1%) - 10%
            if fieldnr% < 1% or fieldnr% > 8% then editpg1
L11170:     if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
            gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Period Range           */~
                              L20200,         /* Part Number Range      */~
                              L20300,         /* Part Category Ran      */~
                              L20800,         /* Vendor     (EWD001)    */~
                              L20400,         /* Store                  */~
                              L20500,         /* Weighting Factor       */~
                              L20600,         /* Primary Sort           */~
                              L20700          /* Secondary Sort         */
            return

L20100: REM Def/Enable Period Range                SPERIOD$,EPERIOD$
            if speriod$ <> " " then return
                speriod$ = "01" : eperiod$ = currprd$
                sdate$ = gldate$(1%)
                if numprds% = 12% and currprd% = 12% then                ~
                call "DATE" addr("G+", gldate$(currprd%+2%), -1%, edate$,~
                                                                    err%)~
                else                                                     ~
                call "DATE" addr("G+", gldate$(currprd%+1%), -1%, edate$,~
                                                                    err%)
                call "DATFMTC" (sdate$)
                call "DATFMTC" (edate$)
                return

L20200: REM Def/Enable Part Number Range           SPART$, EPART$
            if spart$ = " " then spart$ = "ALL"
            return

L20300: REM Def/Enable Part Category Range         SCAT$, ECAT$
            if scat$ = " " then scat$ = "ALL"
            return

L20800: REM Def/Enable Vendor Code                 SVENDOR$, EVENDOR$    (EWD001)
            if svendor$ = " " then svendor$ = "ALL"
            return

L20400: REM Def/Enable Stores Code                 STORE$
            if store$ = " " then store$ = "ALL"
            return

L20500: REM Def/Enable Weighting Factor            FACTOR$
            if factor$ <> " " then return
                factor = round(numprds% / (eperiod% - speriod% + 1), 1)
                call "CONVERT" (factor, 1.1, factor$)
                return

L20600: REM Def/Enable Primary Sort                PSORT$
            if psort$ = " " then psort$ = "1"
            return

L20700: REM Def/Enable Secondary Sort              SSORT$
            if psort$ <> "1" then L20750
                ssort$ = " "     /* No Secondary Sort w/Part as Primary */
                enabled% = 0%
                return
L20750:     if ssort$ <> " " then return
                ssort$ = "1"
                return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Period Range to be Reported Against (for Usage).       ",~
         "Enter Part Number Range, ALL, or '?' for a Listing.          ",~
         "Enter Part Category Range, ALL, or '?' for a Listing.        ",~
         "Enter Main Vendor Codes, ALL, or '?' for a Listing.          ",      /* (EWD001) */ ~
         "Specific Store Code to limit report to, 'ALL', or '?' for a Lis~
        ~ting.",                                                          ~
         "Enter Time Period Weight Factor. Roughly, # Yrly Prds divided b~
        ~y # Usage Prds.",                                                ~
         "Enter Primary Sort: 1 = By Part,  2 = By Category,  3 = By Turn~
        ~s,  4 = Vendor Code.",                                          ~
         "Optional Secondary Sort: 1 = By Part, 2 = By Category, 3 = By T~
        ~urns."

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            call "ALLFREE"
            init(" ") errormsg$, inpmessage$, ecat$, edate$, epart$,     ~
                      eperiod$, factor$, hnydate$, mcat$, mpart$,        ~
                      mpdesc$, numprds$, psort$, rcat1$, rcat2$, rpart1$,~
                      rpart2$, scat$, sdate$, spart$, speriod$, ssort$,  ~
                      store$, storedescr$, pdescr$, sdescr$, svendor$,   ~
                      evendor$

            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
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
            call "SHOSTAT" ("Data Retrieval and Calculations in Progress")
            if keyhit% = 14% then view% = 1% else view% = 0%
            init(hex(00)) plowprime$
            init(" ") firstdate$, firstperiod$
            firstperiod% = 0%
L30090:     call "PLOWNEXT" (#2, plowprime$, 0%, f1%(2))
                if f1%(2) = 0% then L30190
                     get #2 using L30120, mpart$, mpdesc$, mcat$, mvendor$
L30120:                  FMT CH(25), CH(32), POS(90), CH(4), POS(102), CH(9)

                     if mpart$ < rpart1$ or mcat$ < rcat1$ or mcat$ >    ~
                                                        rcat2$ then L30090

                                                           /*  (EWD001)  */
                     if mvendor$ < rven1$ or mvendor$ > rven2$ then L30090
                     if mpart$ > rpart2$ then L30190
                     gosub calc_turns
                     gosub build_workfile
                     goto L30090

L30190:     if firstperiod% = 0% then L30220
                firstdate$ = gldate$(firstperiod%)
                goto L30270
L30220:     for x% = speriod% to (eperiod% - 1%)
                if firstdate$ >= gldate$(x%) and firstdate$ <=           ~
                                                gldate$(x%+1%) then L30260
            next x%
L30260:     firstperiod% = x%
L30270:     call "DATEFMT" (firstdate$)
            convert firstperiod% to firstperiod$, pic(00)
            goto print_report

        REM *************************************************************~
            *                  P R I N T    R E P O R T                 *~
            *-----------------------------------------------------------*~
            * Read data from Workfile and prints the Report             *~
            *************************************************************

        print_report
            call "SHOSTAT" ("Printing Report")
            if f2%(8) <> 0% then L31465
            rpttran, rptinv, rptturns, subtran, subinv, subturns = 0
            page%, line% = 0%
            init(hex(00)) plowprime$, plowtwo$
L31075:     call "PLOWALTS" (#8, plowprime$, 0%, 0%, f1%(8))
                if f1%(8) = 0% then L31465
            if page% = 0% then gosub report_params
            if line% > 56% then gosub print_heading
            init(" ") tranvalue$, invvalue$, turns$
            get #8 using L31105, plowtwo$, mpart$, mpdesc$, mcat$,        ~
                                       mvendor$, tranvalue, invvalue, turns$
L31105:         FMT CH(51), CH(25), CH(32), CH(4), CH(9), 2*PD(14,4), CH(13)
            convert round(tranvalue,2) to tranvalue$, pic(-##########.##)
            convert round(invvalue,2)  to invvalue$,  pic(-##########.##)
                                                          /*  (EWD001)  */
            if psort$ <> "4" and ssort$ <> "4" then                       ~
                print using L31800, mpart$, mpdesc$, mcat$, tranvalue$,   ~
                                                        invvalue$, turns$ ~
            else                                                          ~
                print using L31805, mpart$, mpdesc$, mvendor$, tranvalue$,~
                                                        invvalue$, turns$ 
            line% = line% + 1%
            rpttran = rpttran + tranvalue
            rptinv  = rptinv  + invvalue
            if psort$ = "4" then L31260
            if psort$ <> "2" then L31075
                subtran  = subtran + tranvalue
                subinv   = subinv  + invvalue
                call "PLOWALTS" (#8, plowtwo$, 0%, 0%, f1%(8))
                     if f1%(8) = 0 then L31185
                if str(plowtwo$,1,4) = mcat$ then L31075
L31185:              init(" ") tranvalue$, invvalue$, turns$
                     subinv = round(subinv,2)
                     if subinv <= 0 or subtran < 0 then L31240
                          if subtran > 0 then L31200
                               turns$ = "          .00"
                               goto L31250
L31200:                   subturns = (round(subtran,2) * factor) /       ~
                                                                subinv
                          convert round(subturns,2) to turns$,           ~
                                                       pic(##########.##)
                          goto L31250
L31240:              turns$ = "** N/A **"
L31250:              convert round(subtran,2)  to tranvalue$,            ~
                                                      pic(-##########.##)
                     convert subinv to invvalue$,                        ~
                                                      pic(-##########.##)
                     print
                     print using L31800, " ", "****  TOTALS/TURNS FOR CAT"~
                          & "EGORY", mcat$, tranvalue$, invvalue$, turns$
                     print skip(2)
                     line% = line% + 4%
                     subtran, subinv, subturns = 0
                     goto L31075

L31260:         subtran  = subtran + tranvalue
                subinv   = subinv  + invvalue
                call "PLOWALTS" (#8, plowtwo$, 0%, 0%, f1%(8))
                     if f1%(8) = 0 then L31270
                if str(plowtwo$,1,9) = mvendor$ then L31075
L31270:              init(" ") tranvalue$, invvalue$, turns$
                     subinv = round(subinv,2)
                     if subinv <= 0 or subtran < 0 then L31280
                          if subtran > 0 then L31290
                               turns$ = "          .00"
                               goto L31300
L31290:                   subturns = (round(subtran,2) * factor) /       ~
                                                                subinv
                          convert round(subturns,2) to turns$,           ~
                                                       pic(##########.##)
                          goto L31300
L31280:              turns$ = "** N/A **"
L31300:              convert round(subtran,2)  to tranvalue$,            ~
                                                      pic(-##########.##)
                     convert subinv to invvalue$,                        ~
                                                      pic(-##########.##)
                     print
                     print using L31805, " ", "****  TOTALS/TURNS FOR VEN"~
                          & "DOR  ", mvendor$, tranvalue$, invvalue$, turns$
                     print skip(2)
                     line% = line% + 4%
                     subtran, subinv, subturns = 0
                     goto L31075


L31465:     if page% > 0% then L31566
                keyhit% = 0%
                call "ASKUSER" (keyhit%, " ", "No Records were found meet~
        ~ing the Parameters entered", "Press Enter to Continue")
                goto L31635
L31566:     init(" ") tranvalue$, invvalue$, turns$
            rptinv = round(rptinv,2)
            if rptinv <= 0 or rpttran < 0 then L31578
                if rpttran > 0 then L31575
                     turns$ = "          .00"
                     goto L31580
L31575:         rptturns = (round(rpttran,2)*factor) / rptinv
                convert round(rptturns,2) to turns$, pic(##########.##)
                goto L31580
L31578:     turns$ = "** N/A **"
L31580:     convert round(rpttran,2)  to tranvalue$,  pic(-##########.##)
            convert rptinv            to invvalue$,   pic(-##########.##)
            print skip(2)
            print using L31800, " ", "****  REPORT TOTALS/TURNS", " ",    ~
                                           tranvalue$, invvalue$, turns$
            print skip(2)
            runtime$ = " "  :   call "TIME" (runtime$)
            print "**** END OF REPORT  @ " & runtime$ & " ****"

            close #8 : f2%(8) = 1%
            call "FILEBGON" addr(#8)
            if view% = 1% then call "GETPRTNM" addr(file$, lib$, vol$)
L31635:     close printer
            if view% = 1% then gosub view_report
            call "SETPRNT" ("HNY042", " ", 0%, 1%)
            goto inputmode

        view_report
            REM Display printed file...
            close ws
            call "PUTPARM" addr("E", "INPUT   ",4%,                      ~
                     "FILE    ", file$, 8%, "LIBRARY ", lib$, 8%,        ~
                "VOLUME  ", vol$, 6%, "ACCESS  ", "PRINT ", 6%, "@",ret%)
            call"LINK"addr("DISPLAY ","S"," "," ",0%," "," ",0%, "N", 0%,~
			  ret%)
            call "SCRATCH" addr("F", file$, lib$, vol$, " ", " ", 0%)
            return

L31800: %#########################    ################################   ~
        ~   ####     -##########.##   -##########.##   ##########.##
L31805: %#########################    ################################   ~
        ~ #########  -##########.##   -##########.##   ##########.##


        report_params
            init(" ") company$, runtime$
            call "COMPNAME" (12%, company$, err%)
            call "TIME" (runtime$)
            call "SETPRNT" ("HNY042", " ", 0%, 0%)
            if view% = 1% then call "SET" addr("PM", "K")
            select printer
            gosub print_heading
            print skip(5)
            print using L32400
            print skip(2)
            print using L32420
            print using L32430, speriod$, sdate$, eperiod$, edate$
            print using L32450, spart$, epart$
            print using L32470, scat$, ecat$
            print using L32480, svendor$, evendor$             /*  (EWD001)  */
            print using L32490, store$
            print
            print using L32500
            print using L32550, factor
            print
            print using L32560
            if psort$ = "1" then print using L32570
            if psort$ = "2" then print using L32580
            if psort$ = "3" then print using L32590
            if psort$ = "4" then print using L32595           /*  (EWD001)  */
            if ssort$ = " " then print using L32600
            if ssort$ = "1" then print using L32610
            if ssort$ = "2" then print using L32620
            if ssort$ = "3" then print using L32630
            if ssort$ = "4" then print using L32635
            print skip(2)
            print using L32640, firstperiod$, firstdate$
            line% = 99%
            return

L32400: %                                        ----------  REPORT  PARA~
        ~METERS  ----------
L32420: %                         REPORT OPTIONS
L32430: %                              PERIOD RANGE:   FROM: ## (########~
        ~##)            TO: ## (##########)
L32450: %                              PART RANGE:     FROM: ############~
        ~#############  TO: #########################
L32470: %                              CATEGORY RANGE: FROM: ####        ~
        ~               TO: ####
L32480: %                              VENDOR  RANGE : FROM: #########   ~
        ~               TO: #########
L32490: %                              STORE CODE:     ###
L32500: %                         CALCULATIONS OPTIONS
L32550: %                              TIME PERIOD WEIGHTING FACTOR: ####
L32560: %                         SORT OPTIONS
L32570: %                              PRIMARY SORT IS BY PART NUMBER

L32580: %                              PRIMARY SORT IS BY PART CATEGORY

L32590: %                              PRIMARY SORT IS BY INVENTORY TURNS

L32595: %                              PRIMARY SORT IS BY VENDOR CODE    

L32600: %                              NO SECONDARY SORT WAS SELECTED

L32610: %                              SECONDARY SORT IS BY PART NUMBER

L32620: %                              SECONDARY SORT IS BY PART CATEGORY

L32630: %                              SECONDARY SORT IS BY INVENTORY TUR~
        ~NS
L32635: %                              SECONDARY SORT IS BY VENDOR CODE   
           
L32640: %                         THE FIRST TRANSACTION WAS FOUND IN PERI~
        ~OD:  ##  (########)

        print_heading
            print page
            print using L33200, date$, company$
            print using L33220, runtime$, page%
            print
            if page% = 0% then L33080             /*  (EWD001)  */
                if psort$ <> "4" and ssort$ <> "4" then          ~
                         print using L33240                      ~
                else                                             ~
                         print using L33245
                print using L33260
L33080:     page% = page% + 1%
            line% = 5%
            return

L33200: %RUN ########                    ################################~
        ~###########################                       HNYTURNS:HNY042
L33220: %    ########                                  INVENTORY  TURNOVE~
        ~R RATIO REPORT                                        PAGE:  ####
L33240: %PART NUMBER                  D E S C R I P T I O N              ~
        ~ CATEGORY      TRANS VALUE        INV VALUE      TURN RATIO
L33245: %PART NUMBER                  D E S C R I P T I O N              ~
        ~  VENDOR       TRANS VALUE        INV VALUE      TURN RATIO
L33260: %-------------------------    --------------------------------   ~
        ~ --------   --------------   --------------   -------------

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40145,     /* Period Range             */~
                                L40140,     /* Part Number Range        */~
                                L40140,     /* Part Category Range      */~
                                L40140,     /* Vendor Code  (EWD001)    */~
                                L40140,     /* Store Code               */~
                                L40145,     /* Weighting Factor         */~
                                L40145,     /* Primary Sort             */~
                                L40145      /* Secondary Sort           */
              goto L40155

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40140:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40145:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40155:     accept                                                       ~
               at (01,02),                                               ~
                  "Inventory Turnover Ratio Report",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Report Options:",                            ~
               at (07,04), "Period Range",                               ~
               at (07,23), fac(lfac$(01)), speriod$             , ch(02),~
               at (07,28), fac(hex(8c)), sdate$                 , ch(10),~
               at (07,51), fac(lfac$(01)), eperiod$             , ch(02),~
               at (07,56), fac(hex(8c)), edate$                 , ch(10),~
                                                                         ~
               at (08,04), "Part Range",                                 ~
               at (08,23), fac(lfac$(02)), spart$               , ch(25),~
               at (08,51), fac(lfac$(02)), epart$               , ch(25),~
                                                                         ~
               at (09,04), "Category Range",                             ~
               at (09,23), fac(lfac$(03)), scat$                , ch(04),~
               at (09,51), fac(lfac$(03)), ecat$                , ch(04),~
                                                                         ~
/*EWD001*/     at (10,04), "Vendor Code   ",                             ~
/*EWD001*/     at (10,23), fac(lfac$(04)), svendor$             , ch(09),~
/*EWD001*/     at (10,51), fac(lfac$(04)), evendor$             , ch(09),~
                                                                         ~
               at (11,04), "Store Code",                                 ~
               at (11,23), fac(lfac$(05)), store$               , ch(03),~
               at (11,28), fac(hex(8c))  , storedescr$          , ch(30),~
                                                                         ~
               at (13,02), "Calculation Options:",                       ~
                                                                         ~
               at (14,04), "Weighting Factor",                           ~
               at (14,23), fac(lfac$(06)), factor$              , ch(04),~
                                                                         ~
               at (16,02), "Sort Options:",                              ~
                                                                         ~
               at (17,04), "Primary Sort",                               ~
               at (17,23), fac(lfac$(07)), psort$               , ch(01),~
               at (17,28), fac(hex(8c)),   pdescr$              , ch(30),~
                                                                         ~
               at (18,04), "Secondary Sort",                             ~
               at (18,23), fac(lfac$(08)), ssort$               , ch(01),~
               at (18,28), fac(hex(8c)),   sdescr$              , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40460
                  call "MANUAL" ("HNYTURNS") : goto L40155

L40460:        if keyhit% <> 15 then L40475
                  call "PRNTSCRN" : goto L40155

L40475:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40570     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40550
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40550:     if fieldnr% > 2% then L40560
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40560:     return

L40570: if fieldnr% > 0% then L40615  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "    (14) View Report   (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0d0e0f1000)
            return
L40615:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100,       /* Period Range             */~
                              L50400,       /* Part Number Range        */~
                              L50600,       /* Part Category Range      */~
                              L51600,       /* Vendor Code  (EWD001)    */~
                              L50800,       /* Store Code               */~
                              L51200,       /* Weighting Factor         */~
                              L51300,       /* Primary Sort             */~
                              L51400        /* Secondary Sort           */
            return

L50100: REM Test for Period Range                 SPERIOD$, EPERIOD$
            if speriod$ <> " " then L50120
                errormsg$ = "Start Period Can Not be Blank."
                return
L50120:     convert speriod$ to speriod%, data goto L50285
            if eperiod$ <> " " then L50140
                errormsg$ = "End Period Can Not be Blank."
                return
L50140:     convert eperiod$ to eperiod%, data goto L50295
            if speriod% > 0% then L50165
                errormsg$ = "Starting Period MUST be greater than or equ"~
                          & "al to one (1)."
                return
L50165:     if speriod% <= currprd% then L50185
                errormsg$ = "The Starting Period Entered is greater than"~
                          & " the Current Period of: " & currprd$
                return
L50185:     if eperiod$ <> " " then L50200
                eperiod$ = speriod$ : eperiod% = speriod%
                goto L50240
L50200:     if eperiod% >= speriod% then L50220
                errormsg$ = "The Ending Period CANNOT be less than the S"~
                          & "tarting Period."
                return
L50220:     if eperiod% <= currprd% then L50240
                errormsg$ = "The Ending Period Entered is greater than t"~
                          & "he Current Period of: " & currprd$
                return
L50240:     sdate$ = gldate$(speriod%)
            rdate1$ = sdate$
            call "DATFMTC" (sdate$)
            if numprds% = 12% and eperiod% = 12% then                    ~
            call "DATE" addr("G+", gldate$(eperiod%+2%), -1%, edate$,    ~
                                                                    err%)~
            else                                                         ~
            call "DATE" addr("G+", gldate$(eperiod%+1%), -1%, edate$,    ~
                                                                    err%)
            rdate2$ = edate$
            call "DATFMTC" (edate$)
            return

L50285:     errormsg$ = "Please Enter a Number for the Starting Period."
            return
L50295:     errormsg$ = "Please Enter a Number for the Ending Period."
            return


L50400: REM Test for Part Number Range            SPART$, EPART$
            call "TESTRNGE" (spart$, epart$, rpart1$, rpart2$, errormsg$,~
                                                                      #02)
            return

L50600: REM Test for Part Category Range          SCAT$, ECAT$

            call "TESTRNGE" (scat$, ecat$, rcat1$, rcat2$, errormsg$, #07)

            return

L51600: REM Test for Vendor Range                 SVENDOR$, EVENDOR$

            call "TESTRNGE" (svendor$, evendor$, rven1$, rven2$, errormsg$, #09)

            return

L50800: REM Test for Store                        STORE$
            storedescr$ = " "
            if store$ = "ALL" then return
            if store$ <> " " then L50850
                errormsg$ = "The Store Code CANNOT be Blank."
                return
L50850:     if str(store$,1,1) = "?" then store$ = " "
            call "GETCODE" (#6, store$, " ", 0%, 0, f1%(6))
                if f1%(6) <> 0% then L50900
                     errormsg$ = "Invalid Store Code."
                     return
L50900:     if str(store$,1,1)>= "0" and str(store$,1,1)<= "9" then L50930
                errormsg$ = "This MUST be a Planned (numeric) Store."
                return
L50930:     call "DESCRIBE" (#06, store$, storedescr$, 0%, f1%(8%))
            return

L51200: REM Test for Weighting Factor             FACTOR$
            convert numprds% to numprds$, pic(#0)
            convert factor$ to factor, data goto L51275
            call "CONVERT" (factor, 1.1, factor$)

            if factor > 0 and factor < 100 then L51260
                errormsg$ = "The Weighting Factor MUST be a greater than"~
                          & " zero and less than 100."
L51260:     return

L51275:     errormsg$ = "Please Enter a Number for the Weighting Factor."
            return

L51300: REM Test for Primary Sort                 PSORT$   EWD001
            if psort$ < "1" or psort$ > "4" then L51340
                if psort$ <> "1" then L51322
                ssort$     = " "
                if edit%  <> 2% then fieldnr% = 9%
L51322:     if psort$ = ssort$ then L51510        /* Error Msg */
            goto set_sort_descrs
L51340:     errormsg$ = "The Primary Sort Options are 1, 2, 3, or 4 ONLY."
            return

        set_sort_descrs
            pdescr$, sdescr$ = " "
            if psort$ = "1" then pdescr$ = "(Part)"
            if psort$ = "2" then pdescr$ = "(Category)"
            if psort$ = "3" then pdescr$ = "(Inventory Turns)"
            if psort$ = "4" then pdescr$ = "(Vendor Code)"
            if ssort$ = "1" then sdescr$ = "(Part)"
            if ssort$ = "2" then sdescr$ = "(Category)"
            if ssort$ = "3" then sdescr$ = "(Inventory Turns)"
            if ssort$ = "4" then pdescr$ = "(Vendor Code)"
            return

L51400: REM Test for Secondary Sort               SSORT$
            if ssort$ = " " then goto set_sort_descrs
                if psort$ <> "1" then L51460
                     errormsg$ = "A Secondary Sort is not allowed when t"~
                               & "he Primary Sort is by Part Number."
                     return
L51460:         if ssort$ >= "1" and ssort$ <= "4" then L51500
                     errormsg$ = "The Secondary Sort Options are 1, 2, 3"~
                               & ", 4, or Blank ONLY."
                     return
L51500:         if ssort$ <> psort$ then goto set_sort_descrs
L51510:              errormsg$ = "The Primary and Secondary Sort Options"~
                               & " CANNOT be the same."
                     return


        REM *************************************************************~
            *                   S U B R O U T I N E S                   *~
            *-----------------------------------------------------------*~
            * Internal Non-Mainline subroutines                         *~
            *************************************************************

        calc_turns
            invvalue, tranvalue, turns = 0
            break% = 0%
            init(hex(00)) plowtwo$, plowthree$
            str(plowtwo$,,25) = str(mpart$,,25)
            break% = 25%
            if store$ = "ALL" then L55105
                str(plowtwo$,26,3) = str(store$,,3)
                break% = 28%
L55105:     plowthree$ = plowtwo$
L55120:     call "PLOWALTS" (#5, plowtwo$, 0%, break%, f1%(5))
                if f1%(5) = 0% then L55185
                if str(plowtwo$,26,1) < "0" or str(plowtwo$,26,1) > "9"  ~
                                                               then L55120
                get #5, using L55145, hnydate$, trancode$, qty, tcost
L55145:             FMT POS(43), CH(6), CH(2), POS(51), 2*PD(14,4)
REM             if trancode$ = "IW" or trancode$ = "JK" or trancode$ =   ~
REM                        "RT" or trancode$ = "IQ" then L55150 else L55120

                if trancode$ = "IW" or trancode$ = "JK" or trancode$ =   ~
                           "RT" or trancode$ = "IQ" or trancode$ = "IA"  ~
                                                  then L55150 else L55120
L55150:         if hnydate$ < rdate1$ or hnydate$ > rdate2$ then L55120
                     if firstdate$ = " " or firstdate$ = blankdate$ then L55170
                          if hnydate$ > firstdate$ then L55171
L55170:                        firstdate$ = hnydate$
L55171:                        qty = qty * (-1)
                               tranvalue = tranvalue + (qty * tcost)
                               goto L55120
L55185:     call "PLOWALTS" (#3, plowthree$, 0%, break%, f1%(3))
                if f1%(3) = 0% then L55540
            if str(plowthree$,26,1) < "0" or str(plowthree$,26,1) > "9"  ~
                                                               then L55185
                get #3, using L55210, qty, tcost
L55210:             FMT POS(69), PD(14,4), POS(117), PD(14,4)
                invvalue = invvalue + (qty * tcost)
                goto L55185

L55540:     if invvalue <= 0 or tranvalue <= 0 then L55590
                turns = (round(tranvalue,2) * factor) / round(invvalue,2)
L55590:     return

        build_workfile
            if turns = 0 and invvalue = 0 and tranvalue = 0 then L56240
            if f2%(8) = 0% then L56040
                call "WORKOPEN" (#08, "IO", 1000%, f2%(08))
L56040:     init(hex(00)) workkey$
            if invvalue <= 0 or tranvalue < 0 then L56052
                if tranvalue > 0 then L56050
                     turns$ = "          .00"
                     goto L56060
L56050:         convert round(turns,2) to turns$, pic(##########.##)
                goto L56060
L56052:     turns$ = "** N/A **"
L56060:     if psort$ <> "1" then L56090
                str(workkey$,,25) = str(mpart$,,25)
                goto L56210
L56090:     if psort$ <> "2" then L56170
                if ssort$ = "1" or ssort$ = " " then L56140
                if ssort$ = "4" then L56145
                     workkey$ = str(mcat$,,4) & str(turns$,,13) &        ~
                                                          str(mpart$,,25)
                goto L56210
L56140:            str(workkey$,,29) = str(mcat$,,4) & str(mpart$,,25)
                goto L56210
L56145:            str(workkey$,,29) = str(mcat$,,4) & str(mvendor$,,9)  ~
                                                     & str(mpart$,,25)
                goto L56210
L56170:     if psort$ <> "3" then L56180
                if ssort$ = "1" or ssort$ = " " then L56200
                if ssort$ = "4" then L56205
                     workkey$=str(turns$,,13) & str(mcat$,,4) &           ~
                                                          str(mpart$,,25)
                goto L56210
L56200:              str(workkey$,,38) = str(turns$,,13) & str(mpart$,,25)
                goto L56210
L56205:               str(workkey$,,29) = str(turns$,,4) & str(mvendor$,,9)  ~
                                                     & str(mpart$,,25)
                goto L56210 
L56180:     if ssort$ = "1" or ssort$ = " " then L56250
            if ssort$ = "3" then L56255
                 workkey$ = str(mvendor$,,9) & str(mcat$,,4) &     ~
                                               str(mpart$,,25) 
              goto L56210
L56250:          str(workkey$,,34) = str(mvendor$,,9) & str(mpart$,,25)
              goto L56210
L56255:          workkey$ = str(mvendor$,,9) & str(turns$,,13) &          ~
                                               str(mpart$,,25)

L56210:     write #8 using L56230, workkey$, mpart$, mpdesc$, mcat$,      ~
                                       mvendor$, tranvalue, invvalue, turns$
L56230:           FMT CH(51), CH(25), CH(32), CH(4), CH(9), 2*PD(14,4), CH(13)
L56240:     return

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
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
