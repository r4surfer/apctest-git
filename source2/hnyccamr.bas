        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y   CCC    CCC     A    M   M  RRRR    *~
            *  H   H  NN  N  Y   Y  C   C  C   C   A A   MM MM  R   R   *~
            *  HHHHH  N N N   YYY   C      C      AAAAA  M M M  RRRR    *~
            *  H   H  N  NN    Y    C   C  C   C  A   A  M   M  R   R   *~
            *  H   H  N   N    Y     CCC    CCC   A   A  M   M  R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYCCAMR - This program creats a report of the data in the*~
            *            Cycle Count Master File. The report shows the  *~
            *            assignment/non-assignment of specified fields  *~
            *            in the range of parts to report upon.          *~
            *                                                           *~
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
            * 07/13/92 ! Original                                 ! RJ1 *~
            * 01/12/93 ! Page 0 Facs fix                          ! RJH *~
            * 04/23/93 ! When Analysis Criteria is 'P' and null   ! RJH *~
            *          !  Range selected now returns to edit scrn.!     *~
            * 05/04/93 ! Add Count Factor Field (Pieces per Hour) ! RJH *~
            *          !  Analysis Criteria                       !     *~
            * 05/11/93 ! Now setting Analysis Criteria variables  !     *~
            *          !  before building Range Workfile.         !     *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**


        dim        /* ** Program Variables **                          */~
            analysistype$1,              /* Type of Analysis Criteria  */~
            category$4,                  /* Part Category Code         */~
            choicemsg$25,                /* Accept Scrn Choice Message */~
            columnttl$51,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            count_factor$10,             /* Count Rate Factor          */~
            countrate_flag$1,            /* Count Rate Source Flag     */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fmccgroup$6,                 /* Cycle Count Group Name:    */~
            hiccgroup$6,                 /* Cycle Count Group Name:    */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            loccgroup$6,                 /* Cycle Count Group Name:    */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            r$(24)80,                    /* Screen Image from Range Sel*/~
            rangeflag$1,                 /* Range method of selection  */~
            readkey$99,                  /* Misc. Read/Plow key        */~
            reporttype$1,                /* Report Type (incl/exclude) */~
            rptid$6,                     /* Report ID                  */~
            rpttitle$60,                 /* Report Title               */~
            rpttitle2$60,                /* Report Title               */~
            tempval$66,                  /* Temporary String Value     */~
            testvar$10,                  /* Test Variable              */~
            time$8,                      /* System Time                */~
            toccgroup$6,                 /* Cycle Count Group Name     */~
            transfreqfact$10,            /* Transaction Frequency Fact */~
            typemsg$8,                   /* Report Hdr Inc/exclude Msg */~
            userid$3                     /* Current User Id            */

            dim     /* ** Cycle Count Variables **                     */~
            abcclass$1,                  /* ABC Class                  */~
            ccgroup$6,                   /* Cycle Count Group Code     */~
            cntnbr$6,                    /* Number of Count Sessions   */~
            cntperiod$3,                 /* Count Period               */~
            columnhead1$130,             /* Column Header #1           */~
            columnhead2$130,             /* Column Header #2           */~
            lot$6,                       /* Lot Number of Part         */~
            part$25,                     /* Part Number                */~
            partdescr$32,                /* Part Description           */~
            store$3,                     /* Store/warehouse            */~
            totalcount$10,               /* Total of Counting Events   */~
            variable$200                 /* User Defined Variable Field*/

        dim f2%(64%),                    /* = 0 if the file is open    */~
            f1%(64%),                    /* = 1 if READ was successful */~
            fs%(64%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.04 06/29/93 SFC & Cycle Count Enhancements  "
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
            * #01 ! HNYMASTR ! Inventory Master File                    *~
            * #01 ! HNYQUAN  ! Inventory Store Quantity File            *~
            * #03 ! HNYCCMST ! Cycle Count Master File                  *~
            * #04 ! HNYCCDTL ! Cycle Count Session Detail File          *~
            * #05 ! HNYCCGRP ! Cycle Count Group File                   *~
            * #07 ! STORNAME ! Store Names and Addresses                *~
            * #09 ! CATEGORY ! Inventory Category Descriptions          *~
            *                                                           *~
            * #50 ! WORKFIL1 ! WORKFILE for Selection Sorting Subroutine*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup     ~

            select #02, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =   44
                                                                         ~
            select #03, "HNYCCMST",                                      ~
                        varc,     indexed,  recsize = 796,               ~
                        keypos =    1,  keylen = 44,                     ~
                        alt key  1, keypos =   45, keylen =   6, dup,    ~
                            key  2, keypos =   81, keylen =   7, dup,    ~
                            key  3, keypos =   73, keylen =  15, dup     ~

            select #04, "HNYCCDTL",                                      ~
                        varc,     indexed,  recsize = 436,               ~
                        keypos =    1,  keylen = 41,                     ~
                        alt key  1, keypos =   13, keylen =  45, dup,    ~
                            key  2, keypos =   14, keylen =  44, dup     ~

            select #05, "HNYCCGRP",                                      ~
                        varc,     indexed,  recsize =  616,              ~
                        keypos =    1, keylen =   6                      ~

            select #07, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3                      ~

            select #09, "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   4                      ~

            select #50, "WORKFIL1",                                      ~
                        varc,     indexed,  recsize = 400,               ~
                        keypos = 1,    keylen =   44                     ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))
            call "OPENCHCK" (#05, fs%(05), f2%(05), 0%, rslt$(05))
            call "OPENCHCK" (#09, fs%(09%), f2%(07%), 0%, rslt$(09%))
            rec% = val(str(rslt$(03%), 17%, 4%), 4)

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            ret% = 0%  :  variable$ = " "
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, ret%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "         CYCLE COUNT MASTER FILE ANALYSIS AND" &~
                        " REPORT"
            rptid$ = "HNY052"
            str(columnttl$, 1%) = " From"
            str(columnttl$,11%) = "To"
            choicemsg$ = "       Analysis Criteria "

            str(line2$,62%) = "HNYCCAMR: " & str(cms2v$,,8%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  3%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10270
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% = 08% and fieldnr% = 3% then L10230      ~
                                                 else L10260
L10230:                   gosub  set_analysis_criteria
                          gosub  pick_range
                      if rangeflag$ = "N" then L10100 else L10290

L10260:               if keyhit% <> 0% then       L10120
L10270:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
L10290:     next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles EDIT MODE for range selection screen.             *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  8% then L11100
                      gosub set_analysis_criteria
                      gosub pick_range
                      if analysistype$ = "P" then rangeflag$ = "Y"
L11100:           if keyhit%  = 16% then       extract_data
                  if keyhit% <>  0% then       editpg1
L11120:     if cursor%(1%) <> 7%  then  L11140
                fieldnr% =  1%  :  goto L11190
L11140:     if cursor%(1%) <> 14%  then  L11160
                fieldnr% =  2%  :  goto L11190
L11160:     if cursor%(1%) = 18%  then  fieldnr% =  3%                   ~
                else fieldnr% = 0%

L11190:     if fieldnr% < 1% or fieldnr% >  3% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11230:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11230
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11230
                  lastfieldnr% = fieldnr%
            goto L11120


        REM *************************************************************~
            *           E X T R A C T   R E P O R T   D A T A           *~
            *-----------------------------------------------------------*~
            * Data Extraction section for report.                       *~
            *************************************************************
        extract_data
            call "SHOSTAT"  ("ANALYZING THE CYCLE COUNT FILE")
            gosub set_columnhead
            gosub set_analysis_criteria
            gosub start_report

            if rangeflag$ = "Y" then goto range_method
            /* ELSE fall into CC GROUP Method */

            /* ** Cycle Count Group Method ** */
            if fmccgroup$ = "ALL"  or  fmccgroup$ =  "FIRST"             ~
                  then plowkey$ =  all(hex(00))                          ~
                  else plowkey$ =  fmccgroup$

            call "REDALT4" (#03, plowkey$, 1%, f1%(03%))
            goto L13290

        loop_group    /* CC Group Selection Loop */
            call "READNEXT" (#03, f1%(03))
L13290:     if  f1%(03%) = 0% then  goto print_report_totals
            if fmccgroup$ <> "ALL"  and                                  ~
               key(#03, 1%) > hiccgroup$ then goto print_report_totals
            gosub get_record_values
            gosub set_testvar
            if reporttype$ = "E" then L13360
                if testvar$ <= " " or testvar$ = "0" or testvar$ = "0.00"~
                                                    then L13380 else L13370
L13360:         if testvar$ <= " " or testvar$ = "0" or testvar$ = "0.00"~
                                                    then L13370 else L13380
L13370:     gosub print_record
L13380:     goto loop_group
            /* ** END of CC Group Method of Selection ** */

            FMT CH(25),CH(3),CH(16),CH(6), POS(127),CH(1)  /* HNYCCMST */
            FMT POS(90), CH(4), POS(180), CH(3)            /* HNYMASTR */

        range_method
            readkey$ = all(hex(00))
            call "READ104" (#50, readkey$, f1%(50%))
            goto L13510

        loop_range      /* Range Method Loop Thru Files */
            call "READNEXT" (#50, f1%(50%))
L13510:     if f1%(50%) = 0% then goto print_report_totals
            readkey$ = key(#50)
            call "READ100" (#03, readkey$, f1%(03%))
            if f1%(03%) = 0% then goto loop_range
            gosub get_record_values
            gosub set_testvar
            if reporttype$ = "E" then L13590
                if testvar$ <= " " or testvar$ = "0" or testvar$ = "0.00"~
                                                    then L13610 else L13600
L13590:         if testvar$ <= " " or testvar$ = "0" or testvar$ = "0.00"~
                                                    then L13600 else L13610
L13600:     gosub print_record
L13610:     goto loop_range
            /* ** END of Range Method Selection ** */

            /* HNYCCMST File */
L13650:     FMT CH(25),CH(3),CH(16),CH(6), POS(73), PD(14,4), POS(127),  ~
                 CH(1), POS(139), PD(14,4), POS(181), CH(3), POS(402),   ~
                 PD(14,4)

        get_record_values
            countrate_flag$ = " "
            get #03 using L13650, part$, store$, lot$, ccgroup$, cntnbr,  ~
                                 abcclass$ , transfreqfact, cntperiod$,  ~
                                 count_factor
            call "CONVERT" (cntnbr       , -0.01, cntnbr$)
            if analysistype% = 6% and count_factor <= 0.0                ~
                then gosub check_defaults_count_factor
            call "CONVERT" (count_factor , -2.2 , count_factor$)
            if transfreqfact = 0.0 then transfreqfact$ = "0" else        ~
                    call "CONVERT" (transfreqfact, 2.2, transfreqfact$)
            call "DESCRIBE" (#01, part$, partdescr$, 0%, f1%(01%))
           return

        loop_pop_range      /* Range Method Loop Thru Files */
            call "READNEXT" (#50, f1%(50%))
L13780:     if f1%(50%) = 0% then goto print_report_totals
            readkey$ = key(#50)
          /* ** Check for existence in HNYCCMST ** */
            call "READ100" (#03, readkey$, f1%(03%))
            if reporttype$ = "E" then L13830
                if f1%(03%) = 0% then L13870 else L13840
L13830:         if f1%(03%) = 0% then L13845 else L13870
L13840:     gosub get_record_values
L13845:     part$ = str(readkey$,,25%)
            call "DESCRIBE" (#01, part$, partdescr$, 0%, f1%(01%))
            gosub print_record
L13870:     goto loop_pop_range

        set_analysis_criteria
            if analysistype% = 4% then goto  population_check
            on analysistype% gosub set_abc_criteria      ,               ~
                                   set_period_criteria   ,               ~
                                   set_transfact_criteria,               ~
                                                         ,               ~
                                   set_counted_criteria  ,               ~
                                   set_count_factor
            return

        set_testvar
            on analysistype%  goto L14020, L14030, L14040, , L14050, L14055
L14020:     testvar$ = abcclass$      : return
L14030:     testvar$ = cntperiod$     : return
L14040:     testvar$ = transfreqfact$ : return
L14050:     testvar$ = cntnbr$        : return
L14055:     testvar$ = count_factor$  : return

        set_abc_criteria
            rpttitle2$ =                   "     PARTS " & typemsg$  &   ~
                         " ABC CLASSIFICATION"
            columnhead1$ = str(columnhead1$,,74%) & "ABC CLASS"
            columnhead2$ = str(columnhead2$,,74%) & "---------"
            rngtype% = 2%
            return

        set_period_criteria
            rpttitle2$ =                   "     PARTS " & typemsg$  &   ~
                         " COUNT PERIOD SETTING"
            columnhead1$ = str(columnhead1$,,74%) & "    PERIOD"
            columnhead2$ = str(columnhead2$,,74%) & "    ------"
            rngtype% = 2%
            return

        set_transfact_criteria
            rpttitle2$ =                   "     PARTS " & typemsg$  &   ~
                         " TRANSACTION FACTOR"
            columnhead1$ = str(columnhead1$,,74%) & "TRANS FACTOR"
            columnhead2$ = str(columnhead2$,,74%) & "------------"
            rngtype% = 2%
            return

        set_ccpop_criteria
            if reporttype$ = "E" then                                    ~
                rpttitle2$ =           "PARTS NOT PART OF CYCLE COUNTING"~
              else                                                       ~
                rpttitle2$ =           "CYCLE PART POPULATION LIST"
            columnhead1$ = str(columnhead1$,,74%)
            columnhead2$ = str(columnhead2$,,74%)
            rngtype% = 1%
            return

        set_counted_criteria
            if reporttype$ = "E" then                                    ~
                rpttitle2$ =                  "     PARTS THAT HAVE NOT" ~
              else                                                       ~
                rpttitle2$ =                  "     PARTS THAT HAVE"
            rpttitle2$ = rpttitle2$ & " BEEN COUNTED"
            columnhead1$ = str(columnhead1$,,74%) & "    COUNTS"
            columnhead2$ = str(columnhead2$,,74%) & "    ------"
            rngtype% = 2%
            return

        population_check
            gosub set_ccpop_criteria
            gosub start_report

            readkey$ = all(hex(00))
            call "READ104" (#50, readkey$, f1%(50%))
            goto L13780

        set_count_factor
            rpttitle2$ =                "        PARTS " & typemsg$  &   ~
                         " COUNT FACTOR SETTING"
            columnhead1$ = str(columnhead1$,,74%) & "COUNT FACTOR"
            columnhead2$ = str(columnhead2$,,74%) & "------------"
            rngtype% = 2%
            return


        REM *************************************************************~
            *          P I C K R A N G E  S E L E C T I O N             *~
            *-----------------------------------------------------------*~
            * Sets Report Items from Range Selection Method             *~
            *************************************************************
        pick_range
            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 0%, rslt$(02%))
            call "OPENCHCK" (#07, fs%(07%), f2%(07%), 0%, rslt$(07%))
            call "FILEBGON" addr(#50)
            call "WORKOPEN" (#50, "IO", rec% / 2%, f2%(50%))

            call "HNYCCRNG" ("N", rngtype%, #01, #02, #09, #07, #03, #50,~
                             rngcnter%,                                  ~
                            "Cycle Count Master File Analysis and Report"~
                            ,hex(84) & "(16)Return      ",               ~
                             r$() )
            if rngcnter% > 0%  then L16140
                gosub nothing_selected_msg
                rangeflag$ = "N"  :  goto L16160
L16140:     rangeflag$ = "Y"  :  fmccgroup$ = "RANGE" : toccgroup$ = " "

L16160:     return
           /* *** End Pick Range Sub *** */

        check_defaults_count_factor
            count_factor = 0
            call "READ100" (#1, str(part$) & str(store$) & lot$, f1%(1%))
            if f1%(1%) = 0% then return
            get #1 using L16300, category$, count_factor
            if count_factor > 0.0  then L16285
            call "READ100" (#9, category$, f1%(9%))
            if f1%(9%) = 0% then return
            get #9 using L16310, count_factor
            if count_factor <= 0.0 then return
L16285:         countrate_flag$ = "*"
            return
L16300: FMT POS(90), CH(4), POS(119), PD(14,4)     /* Hnymastr */
L16310: FMT POS(53), PD(14,4)                      /* Category */

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20180,         /* Criteria Selection     */~
                              L20300,         /* Report Type            */~
                              L20350          /* Cycle Count Group      */
            return

L20180: REM Def/Enable Analysis Type               ANALYSISTYPE$

            return

L20300: REM Def/Enable Report Type                 REPORTTYPE$

            return

L20350: REM Def/Enable Cycle Count Group Name      FMCCGROUP$
            if analysistype$ = "P" then L20380
            if fmccgroup$ = " " then fmccgroup$ = "ALL"
            return
L20380:     /* Cycle Count Population Check */
            enabled% = 0%
            rngtype% = 1%
            gosub pick_range
            if rngcnter% > 0% then return
                rangeflag$ = "Y" : fmccgroup$ = "RANGE" : toccgroup$ = " "
                return

        nothing_selected_msg
                u3% = 2%                           /* Window at bottom */
                call "ASKUSER" (u3%, "*** NOTHING SELECTED ***",         ~
                     "There are no HNYMASTR records that satisfy your c"&~
                     "riteria.", " ", "Press any PF key to acknowledge "&~
                     "and continue.")
                return


        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter the Analysis Criteria.                                 ",~
         "Enter 'E' to Print Excluded Parts, 'I' to Print Include Parts."~
         ,"Enter Cycle Count Group Name or PF 8 to Select Ranges.      "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, analysistype$,             ~
                      fmccgroup$, hiccgroup$, loccgroup$, toccgroup$,    ~
                      reporttype$, r$(), rangeflag$
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
        start_report
            select printer(134)
            call "SHOSTAT" ("PRINTING REPORT NOW")
            time$ = " "  :  call "TIME" (time$)
            totalcount = 0

            call "SETPRNT" ("RPTID", " ", 0%, 0%)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
            if lcntr% > 56% then gosub page_head
            if pcntr% = 1% then gosub column_head
            return

        print_record
            if lcntr% < 56% then L30200
                gosub page_head : gosub column_head : lcntr% = lcntr% + 6%
L30200:     on analysistype% goto L30210, L30206, L30204, L30210, L30206, L30206
L30204:         if testvar$ = "0" then  testvar$ = "0.0"
L30206:         call "RJUSTIFY" (testvar$)
L30210:     if analysistype% = 6% then L30230   /* Count Rate Factor */
            print using    L60250, part$, partdescr$, store$, lot$,       ~
                                  testvar$, " "
                goto L30240
L30230:     print using    L60250, part$, partdescr$, store$, lot$,       ~
                                  testvar$, countrate_flag$
L30240:     lcntr% = lcntr% + 1%
            totalcount  = totalcount  + 1

            return

        print_report_totals
            call "CONVERT" (totalcount,-0.01, totalcount$)
            print
            print using L60330, totalcount$
            goto end_report

        end_report                /* Report Ending Routine */
            time$ = " "  :  call "TIME" (time$)
            print skip(2)
            print using L64965, time$    /* End of report line */
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            if rangeflag$ = "Y" then call "FILEBGON" (#50)/*Zap Range WF*/
            goto inputmode

        page_head              /* Page Heading Print Routine */
            call "STRING" addr("CT", rpttitle2$, 60%)
            pcntr% = pcntr% + 1%
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "HNYCCAMR", rptid$
            print using L60110, rpttitle$, pcntr%
            print using L60134, rpttitle2$
            print
            if pcntr% = 0% then gosub print_params
            lcntr% = 4%
            return

        column_head
            print columnhead1$
            print columnhead2$
            lcntr% = lcntr% + 2%
            return

        set_columnhead
            columnhead1$ = "PART NUMBER                "          &      ~
                           "DESCRIPTION                       "   &      ~
                           "STR  LOT     "
            columnhead2$ = "-------------------------  "          &      ~
                           "--------------------------------  "   &      ~
                           "---  ------  "
            return

        print_params           /* Print Page Zero */
L30725:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L30760
                str(i$(), i%, 1%) = hex(20)
                goto L30725
L30760:     print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 18% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            if fmccgroup$ <> "RANGE" then L30910
                print
                for x% = 4% to 20% : print tab(26); r$(x%) : next x%
                print tab(26);
                print "--------------------------------------------------~
        ~------------------------------"
L30910:     gosub page_head
            return


        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

        FMT                 /* FILE: HNYCCMST                          */~
            CH(25),         /* Part Number (Product Code)              */~
            CH(3),          /* Store Name                              */~
            CH(16),         /* Lot Number                              */~
            CH(6),          /* Cycle Count Group Code Name             */~
            CH(6),          /* Date Last Counted                       */~
            PD(14,4),       /* Count Tolerance Percentage              */~
            PD(14,4),       /* Count Tolerance Quantity                */~
            PD(14,4),       /* Number of Counts                        */~
            CH(1),          /* Active Session Flag                     */~
            CH(6),          /* Next Count Date                         */~
            CH(6),          /* Record Start Date                       */~
            PD(14,4),       /* Cumulative Tolerance Hits               */~
            PD(14,4),       /* Cumulative Count Delta (+)              */~
            PD(14,4),       /* Cumulative Count Delta (+)              */~
            PD(14,4)        /* Cumulative BOH Quantity                 */

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
              on fieldnr% gosub L40170,         /* Analysis Criteria */   ~
                                L40170,         /* Report Type       */   ~
                                L40170          /* Cycle Count Group */

              goto L40200
                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40200:     accept                                                       ~
               at (01,02),                                               ~
                  "Cycle Count Master File Analysis and Report",         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,29), fac(hex(ac)), choicemsg$             , ch(36),~
                                                                         ~
               at (07,02), "Analysis Criteria      ",                    ~
               at (07,30), fac(lfac$( 1%)), analysistype$       , ch(01),~
                                                                         ~
               at (07,35), "A = ABC Class",                              ~
               at (08,35), "C = Count Period",                           ~
               at (09,35), "T = Transaction Factor",                     ~
               at (10,35), "P = Cycle Count Population  ",               ~
               at (11,35), "S = Count Session Participation",            ~
               at (12,35), "R = Count Rate Factor          ",            ~
                                                                         ~
               at (14,02), "Report Type ",                               ~
               at (14,30), fac(lfac$( 2%)), reporttype$         , ch(01),~
               at (14,35), "E = Excluded Parts             ",            ~
               at (15,35), "I = Included Parts             ",            ~
                                                                         ~
               at (17,29), fac(hex(ac)),   columnttl$           , ch(36),~
                                                                         ~
               at (18,02), "Cycle Count Group Name ",                    ~
               at (18,30), fac(lfac$( 3%)), fmccgroup$          , ch(06),~
               at (18,39), fac(lfac$( 3%)), toccgroup$          , ch(06),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(84)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40600
                  call "MANUAL" ("HNYCCAMR") : goto L40200

L40600:        if keyhit% <> 15% then L40630
                  call "PRNTSCRN" : goto L40200

L40630:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40880     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                 (8)Select Ranges       " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffff08ffffffff0dff0f1000)
            if fieldnr% <> 1% then L40790
                str(pf$(2%),18%,16%) = " ":str(pfkeys$, 8%,1%) = hex(ff)
                str(pf$(1%),18%,26%) = " ":str(pfkeys$, 4%,1%) = hex(ff)
L40790:     if fieldnr% <> 2% then L40820
                str(pf$(2%),18%,16%) = " ":str(pfkeys$, 8%,1%) = hex(ff)
                str(pf$(3%),62%,18%) = " ":str(pfkeys$,16%,1%) = hex(ff)
L40820:     if fieldnr% <> 3% then L40860
                str(pf$(3%),62%,18%) = " ":str(pfkeys$,16%,1%) = hex(ff)
                if fmccgroup$ = " " or fmccgroup$ = "ALL" then  return
                str(pf$(2%),18%,16%)  = " ":str(pfkeys$,8%,1%) = hex(ff)
L40860:     return

L40880: if fieldnr% > 0% then L40970  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            if rangeflag$ <> "Y" then return
                str(pf$(2%),18%,16%) = "(8)Select Ranges"
                str(pfkeys$, 8%,1%) = hex(08)
            return
L40970:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
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
            on fieldnr% gosub L50200,         /* Analysis Criteria      */~
                              L50260,         /* Report Type            */~
                              L50330          /* Cycle Count Group      */
            return

L50200: REM Test for Analysis Criteria            ANALYSISTYPE$
            analysistype% = pos("ACTPSR" = analysistype$)
            if analysistype% <> 0% then return
            errormsg$ = "'A', 'C', 'T', 'P', 'S', or 'R' Please."
            return

L50260: REM Test for Report Type                  REPORTTYPE$
            p% = pos("EI" = reporttype$)
            on p% + 1%   goto L50290, L50300, L50310
L50290:     errormsg$ = "'E', or 'I' Please."   : return
L50300:     typemsg$  = "WITHOUT"               : return
L50310:     typemsg$  = "WITH"                  : return

L50330: REM Test for Cycle Count Group Name       FMCCGROUP$
            if fmccgroup$ = "RANGE" then return
            if fmccgroup$ = " " and toccgroup$ = " " then doto% = 1%     ~
                                                     else doto% = 0%
            if fmccgroup$ = "ALL" then L50450
            if fmccgroup$ = "FIRST" then L50460
                tempval$ = hex(0684) & "Select FROM Cycle Count Group"
                call "GETCODE" (#05, fmccgroup$, tempval$, 0%, 0, f1%(5))
                if fmccgroup$ <> "?" then L50440
L50420:         errormsg$ = "'?' is Not a Valid Range" :  return

L50440:     if toccgroup$ <> " "  or doto% = 1% then  L50460
L50450:         toccgroup$ = fmccgroup$     :   goto  L50510
L50460:     if toccgroup$ = "LAST" then  L50510
                tempval$ = hex(0684) & "Select TO Cycle Count Group"
                call "GETCODE" (#05, toccgroup$, tempval$, 0%, 0, f1%(5))
                if toccgroup$ <> "?" then L50510 else L50420

L50510:     call "TESTRNGE"                                              ~
                  (fmccgroup$          , toccgroup$          ,           ~
                   loccgroup$          , hiccgroup$          ,           ~
                   errormsg$)

            if fmccgroup$ = "FIRST" then  loccgroup$ =  all(hex(20))
            if fmccgroup$ = "ALL" or toccgroup$ = "LAST" then            ~
                                                hiccgroup$ = all(hex(ff))
            return

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

*       * Header Line 3
L60134: %                                     ###########################~
        ~#################################

*       * Report Line 1
L60250: %#########################  ################################  ###~
        ~  ######  ########## #

L60330: %        TOTAL NUMBER OF ITEMS: ##########


        %** Report Title for page 0
        %############################################################

L64965:         %                          * * * * * * * * * *   E N D   ~
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
