        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y   CCC    CCC   H   H  RRRR   PPPP    *~
            *  H   H  NN  N  Y   Y  C   C  C   C  H   H  R   R  P   P   *~
            *  HHHHH  N N N   YYY   C      C      HHHHH  RRRR   PPPP    *~
            *  H   H  N  NN    Y    C   C  C   C  H   H  R   R  P       *~
            *  H   H  N   N    Y     CCC    CCC   H   H  R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYCCHRP - This program creats a report of the data in the*~
            *            Cycle Count Master File. The report shows the  *~
            *            cumulative statistics for parts in the selected*~
            *            range.                                         *~
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
            * 05/18/92 ! Original                                 ! RJ1 *~
            * 01/12/93 ! Page 0 Facs fix                          ! RJH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**


        dim        /* ** Program Variables **                          */~
            avebohqty$10,                /* Average Book on Hand Quan  */~
            aveperiod$5,                 /* Average Period             */~
            avequantity$10,              /* Average Quantity Counted   */~
            avevariance$10,              /* Average Variance Quantity  */~
            avevarprcnt$10,              /* Average Variance Percent   */~
            columnttl$51,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fmccgroup$6,                 /* Cycle Count Group Name:    */~
            hiccgroup$6,                 /* Cycle Count Group Name:    */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            loccgroup$6,                 /* Cycle Count Group Name:    */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            printkey$99,                 /* String of Key Value to Prnt*/~
            r$(24%)80,                   /* Screen Image from Range Sel*/~
            rangeflag$1,                 /* Range method of selection  */~
            readkey$99,                  /* Misc. Read/Plow key        */~
            reporttype$1,                /* Report Type (detl, summary)*/~
            rptid$6,                     /* Report ID                  */~
            rpttitle$60,                 /* Report Title               */~
            sortordmsg$18,               /* Accept Scrn Sort Order Msg */~
            sortmsg1$1,                  /* Accept Scrn Sort Order Msg */~
            sortmsg2$1,                  /* Accept Scrn Sort Order Msg */~
            sortmsg3$1,                  /* Accept Scrn Sort Order Msg */~
            sortnum1$1,                  /* First Sort Order Criteria  */~
            sortnum2$1,                  /* Second Sort Order Criteria */~
            sortnum3$1,                  /* Third Sort Order Criteria  */~
            tempval$66,                  /* Temporary String Value     */~
            time$8,                      /* System Time                */~
            toccgroup$6,                 /* Cycle Count Group Name:    */~
            userid$3                     /* Current User Id            */



            dim     /* ** Cycle Count Variables **                     */~
            abcclass$1,                  /* ABC Class                  */~
            actflag$1,                   /* Active Session Flag        */~
            arraycode$(6%)1,             /* Master Array Selection Code*/~
            arrayhdr1$(6%)44,            /* Master Array Header 1      */~
            arrayhdr2$(6%)44,            /* Master Array Header 2      */~
            arrayhdr3$(6%)44,            /* Master Array Header 3      */~
            arraylen%(6%),               /* Master Array Varibl Length */~
            arrayprntlen%(6%),           /* Master Array Print  Length */~
            arrayindx%(6%),              /* Master Array Sort Index    */~
            arrayval$(6%)44,             /* Master Array Variable Value*/~
            arrayvar$(6%)11,             /* Master Array Variable Name */~
            ccgroup$6,                   /* Cycle Count Group Code     */~
            cntnbr$6,                    /* Number of Count Sessions   */~
            cntperiod$3,                 /* Count Period               */~
            cnttlernper$10,              /* Count Tolerance in Percent */~
            cnttlernqty$10,              /* Count Tolerance Quantity   */~
            columnhead1$130,             /* Column Header #1           */~
            columnhead2$130,             /* Column Header #2           */~
            columnhead3$130,             /* Column Header #3           */~
            cumtlernhit$9,               /* Cumulative Tolerance Hits  */~
            datecnt$10,                  /* Last Count Date            */~
            firstcntdate$10,             /* First Count Date           */~
            lastval1$44,                 /* Previous 1st Value in PrKey*/~
            lastval2$44,                 /* Previous 2nd Value in PrKey*/~
            lot$6,                       /* Lot Number of Part         */~
            nextcntdate$10,              /* Next Count Date            */~
            part$25,                     /* Part Number                */~
            partstrlot$44,               /* Part/Store/Lot Key         */~
            recdate$10,                  /* Date Record was created    */~
            store$3,                     /* Store/warehouse            */~
            subtotcount$10,              /* Subtotal of Counting Events*/~
            subtothits$10,               /* Subtotal of Tolerance Hits */~
            subtotprcnt$5,               /* Subtotal of Tol Hit Percent*/~
            tolprcnt$6,                  /* Within Tolerance Percent   */~
            totalcount$10,               /* Total of Counting Events   */~
            totalhits$10,                /* Total of Tolerance Hits    */~
            totalprcnt$5,                /* Total of Tol Hit Percent   */~
            work2key$99,                 /* Workfile 2 Key             */~
            variable$200                 /* User Defined Variable Field*/

        dim f2%(64%),                    /* = 0 if the file is open    */~
            f1%(64%),                    /* = 1 if READ was successful */~
            fs%(64%),                    /* = 1 if file open, -1 if it */~
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
            * #01 ! HNYMASTR ! Inventory Master File                    *~
            * #01 ! HNYQUAN  ! Inventory Store Quantity File            *~
            * #03 ! HNYCCMST ! Cycle Count Master File                  *~
            * #04 ! HNYCCDTL ! Cycle Count Session Detail File          *~
            * #05 ! HNYCCGRP ! Cycle Count Group File                   *~
            * #07 ! STORNAME ! Store Names and Addresses                *~
            * #09 ! CATEGORY ! Inventory Category Descriptions          *~
            *                                                           *~
            * #50 ! WORKFIL1 ! WORKFILE for Selection Sorting Subroutine*~
            * #52 ! WORKFIL2 ! WORKFILE for Report Sort and Printing    *~
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

            select #52, "WORKFIL2",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos = 1,    keylen =58                        ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 0%, rslt$(01%))
            call "OPENCHCK" (#03, fs%(03%), f2%(03%), 0%, rslt$(03%))
            rec% = val(str(rslt$(03%),17%,4%),4%) / 2%
            call "OPENCHCK" (#04, fs%(04%), f2%(04%), 0%, rslt$(04%))
            call "OPENCHCK" (#05, fs%(05%), f2%(05%), 0%, rslt$(05%))


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

            rpttitle$ = "              CYCLE COUNT HISTORY FILE REPORT"

            rptid$ = "HNY050"
            str(columnttl$, 1%) = "From"
            str(columnttl$,10%) = "To"
            sortordmsg$ = "   Sort Criteria   "
            sortmsg1$   = "1"
            sortmsg2$   = "2"
            sortmsg3$   = "3"

            str(line2$,62%) = "HNYCCHRP: " & str(cms2v$,,8%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  2%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% = 08% and fieldnr% = 1%                 ~
                                                 then  gosub  pick_range ~
                                                 else L10210
                      if rangeflag$ = "N" then L10100 else L10240

L10210:               if keyhit% <> 0% then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
L10240:     next fieldnr%

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
L11120:     if cursor%(1%) <> 7%  then  L11140
                fieldnr% =  1%  :  goto L11190
L11140:     if cursor%(1%) <> 10%  then  L11160
                fieldnr% =  2%  :  goto L11190
L11160:     if cursor%(1%) = 16%  then  fieldnr% =  3%                   ~
                else fieldnr% = 0%

L11190:     if fieldnr% < 1% or fieldnr% >  2% then editpg1
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
            gosub initialize_sort_array
            gosub set_array_index /*Set the Array Index for Report Sort*/

            call "SHOSTAT"  ("BUILDING THE REPORT WORK FILE")

            call "WORKOPEN" (#52, "IO", rec%, f2%(52%))

            if rangeflag$ = "Y" then goto range_method
            /* ELSE fall into CC GROUP Method */

            /* ** Cycle Count Group Method ** */
            if fmccgroup$ = "ALL"  or  fmccgroup$ =  "FIRST"             ~
                  then plowkey$ =  all(hex(00))                          ~
                  else plowkey$ =  fmccgroup$

            call "REDALT4" (#03, plowkey$, 1%, f1%(03%))
            goto L13260

        loop_group    /* CC Group Selection Loop */
            call "READNEXT" (#03, f1%(03%))
L13260:     if  f1%(03) = 0% then  goto generate_report
            if fmccgroup$ <> "ALL"  and                                  ~
               key(#03, 1%) > hiccgroup$ then goto generate_report
            get #03 using L13680, part$, store$, lot$, ccgroup$, cntnbr,  ~
                                 cumtlernhit, abcclass$ , cntperiod$
            call "RJUSTIFY" (cntperiod$)
            if cntnbr < 1.0 then L13350
                tolprcnt = 100 * cumtlernhit / cntnbr
                call "CONVERT" (tolprcnt  , 2.2, tolprcnt$)
                goto L13360
L13350:     tolprcnt$ = " "
L13360:     call "CONVERT" (cntnbr    , 0.01, cntnbr$)

            gosub make_workfile2_key
            write  #52, work2key$
            goto loop_group
            /* ** END of CC Group Method of Selection ** */

            FMT CH(25),CH(3),CH(16),CH(6), POS(127),CH(1)  /* HNYCCMST */
            FMT POS(90), CH(4), POS(180), CH(3)            /* HNYMASTR */

        range_method
            readkey$ = all(hex(00))
            call "READ104" (#50, readkey$, f1%(51%))
            if f1%(51%) = 0% then goto generate_report  else L13540

        loop_range      /* Range Method Loop Thru Files */
            call "READNEXT" (#50, f1%(51%))
            if f1%(51%) = 0% then goto generate_report
L13540:     readkey$ = key(#50)
            call "READ100" (#03, readkey$, f1%(03%))
            if f1%(03%) = 0% then goto generate_report
            get #03 using L13680, part$, store$, lot$, ccgroup$, cntnbr,  ~
                                 cumtlernhit, abcclass$ , cntperiod$
            call "RJUSTIFY" (cntperiod$)
            if cntnbr < 1.0 then L13620
                tolprcnt = 100 * cumtlernhit / cntnbr
                call "CONVERT" (tolprcnt  , 2.2, tolprcnt$)
                goto L13625
L13620:     tolprcnt$ = " "

L13625:     call "CONVERT" (cntnbr    , 0.01, cntnbr$)

            gosub make_workfile2_key
            write  #52, work2key$
            goto loop_range
            /* ** END of Range Method Selection ** */

            /* HNYCCMST File */
L13680:     FMT CH(25),CH(3),CH(16),CH(6), POS(73), PD(14,4), POS(94),   ~
                PD(14,4), POS(127), CH(1), POS(181), CH(3)

        make_workfile2_key /* Set Work File & Key from Sort Array Index */
            partstrlot$ = str(part$) & str(store$) & str(lot$)
            arrayval$(1%) = abcclass$   : arrayval$(2%) = ccgroup$
            arrayval$(3%) = partstrlot$ : arrayval$(4%) = cntperiod$
            arrayval$(5%) = cntnbr$     : arrayval$(6%) = tolprcnt$
            work2key$ = arrayval$(arrayindx%(1%))
            pos% = 1%
            len% = arraylen%(arrayindx%(1%))

            for n% = 2% to 6%
            pos% = pos% + len%
            work2key$ = str(work2key$,,(pos% - 1%)) &                    ~
                                               arrayval$(arrayindx%(n%))
            len% = arraylen%(arrayindx%(n%))
            next n%

            return


        REM *************************************************************~
            *          P I C K R A N G E  S E L E C T I O N             *~
            *-----------------------------------------------------------*~
            * Sets Report Items from Range Selection Method             *~
            *************************************************************

        pick_range
            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 0%, rslt$(02%))
            call "OPENCHCK" (#07, fs%(07%), f2%(07%), 0%, rslt$(07%))
            call "OPENCHCK" (#09, fs%(09%), f2%(07%), 0%, rslt$(09%))
            call "FILEBGON" addr(#50)
            call "WORKOPEN" (#50, "IO", rec%, f2%(50%))

            call "HNYCCRNG" ("Y", 2%, #01, #02, #09, #07, #03, #50,      ~
                             rngcnter%,                                  ~
                            "Cycle Count History Report"       ,         ~
                            "(16)Continue    ",                          ~
                             r$() )
            if rngcnter% > 0%  then L16140
            rangeflag$ = "N"  :  goto L16160
L16140:     rangeflag$ = "Y"  :  fmccgroup$ = "RANGE" : toccgroup$ = " "

L16160:     return
           /* *** End Pick Range Sub *** */

        initialize_sort_array
            /* Initialize  Report sort order array and other report    */
            /* variables.                                              */

            arraycode$(1%) = "A"      : arrayvar$(1%) = "ABCCLASS$"
            arrayhdr1$(1%) = "   "    : arrayhdr2$(1%) = "ABC"
            arrayhdr3$(1%) = "---"    : arrayval$(1%) = abcclass$
            arraylen%(1%)  =  1%      : arrayprntlen%(1%) = 3%

            arraycode$(2%) = "G"      : arrayvar$(2%) = "CCGROUP$"
            arrayhdr1$(2%) = "CYCLE " : arrayhdr2$(2%) = "GROUP "
            arrayhdr3$(2%) = "------" : arrayval$(2%) = ccgroup$
            arraylen%(2%) = 6%        : arrayprntlen%(2%) = 6%

            arraycode$(3%) = "P"      : arrayvar$(3%) = "PARTSTRLOT$"
            arrayhdr1$(3%) = " "      : arrayval$(3%) = partstrlot$
            arrayhdr2$(3%) = "PART NUMBER               STR LOT   "
            arrayhdr3$(3%) = "------------------------- --- ------"
            arraylen%(3%) = 44%       : arrayprntlen%(3%) = 36%

            arraycode$(4%) = "C"    : arrayvar$(4%)  = "CNTPERIOD$"
            arrayhdr1$(4%) = "CNT"  : arrayhdr2$(4%) = "PRD"
            arrayhdr3$(4%) = "---"  : arrayval$(4%)  =  cntperiod$
            arraylen%(4%)  =  3%    : arrayprntlen%(4%) = 3%

            arraycode$(5%) = "N"      : arrayvar$(5%)  = "CNTNBR$"
            arrayhdr1$(5%) = "NUMBER" : arrayhdr2$(5%) = "COUNTS"
            arrayhdr3$(5%) = "------" : arrayval$(5%)  =  cntnbr$
            arraylen%(5%)  =  6%      : arrayprntlen%(5%) = 6%

            arraycode$(6%) = "T"      : arrayvar$(6%)  = "TOLPRCNT$"
            arrayhdr1$(6%) = "  TOL"  : arrayhdr2$(6%) = "PRCNT"
            arrayhdr3$(6%) = "-----"  : arrayval$(6%)  =  tolprcnt$
            arraylen%(6%)  =  5%      : arrayprntlen%(6%) = 5%

            return      /* END of INTITALIZE_SORT_ARRAY Sub  */

        set_array_index    /* Set the Array Index for Report Sorting */
            i% = 3%
            for n% = 1% to 6%
                p% = pos("AGPCNT" = arraycode$(n%))
                if sortnum1$ <> arraycode$(n%) then L19430
                     arrayindx%(1%) = p%  : goto L19480
L19430:         if sortnum2$ <> arraycode$(n%) then L19450
                     arrayindx%(2%) = p%  : goto L19480
L19450:         if sortnum3$ <> arraycode$(n%) then L19470
                     arrayindx%(3%) = p%  : goto L19480
L19470:         i% = i% + 1%  :  arrayindx%(i%) = p%
L19480:     next n%
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20130,         /* Cycle Count Group      */~
                              L20180          /* Sort Order             */

            return

L20130: REM Def/Enable Cycle Count Group Name      FMCCGROUP$
            if fmccgroup$ = " " then fmccgroup$ = "ALL"

            return

L20180: REM Def/Enable Sort Number                 SURTNUM#$



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
         "Enter Cycle Count Group Name or PF 8 to Select Ranges.       ",~
         "Enter In Order the Sort Criteria.                            "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      fmccgroup$, hiccgroup$, loccgroup$, toccgroup$,    ~
                      reporttype$, sortnum1$, sortnum2$, sortnum3$, r$()
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
            call "SHOSTAT" ("PRINTING REPORT NOW")
            time$ = " "  :  call "TIME" (time$)
            totalcount, totalhits = 0
            subtotcount, subtothits = 0
            call "SETPRNT" ("RPTID", " ", 0%, 0%)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
            if lcntr% > 56% then gosub page_head

            gosub set_column_headers
            if pcntr% = 1% then gosub column_head

*         ** Start Report Loop **

            plowkey$ = all(hex(00))
            call "READ102" (#52, plowkey$, f1%(52%))
            goto  L30240

        report_loop
            call "READNEXT" (#52, f1%(52%))
L30240:     if f1%(52%) = 0% then goto print_report_totals

            avebohqty, aveperiod, avequantity, avevariance = 0.0
            aveperiod$ = " "

            get #52 , work2key$
            gosub set_printkey  /* Set PrintKey Format */

            call "READ100" (#03, partstrlot$, f1%(03%))
            if f1%(03%) <> 0% then L30340
                call "SHOSTAT" ("KEY DID NOT MATCH HNYCCMST")
                goto report_loop
L30340:     get #03  using L35060,                                        ~
                part$, store$, lot$, ccgroup$, datecnt$, cnttlernper,    ~
                cnttlernqty, cntnbr , actflag$, nextcntdate$, recdate$,  ~
                cumtlernhit, cumcntdltap, cumcntdltam, cumbohqty,        ~
                firstcntdate$
            if cntnbr < 1.0 then L30500
                if cntnbr < 2.0 then goto L30475
                     call "DATE" addr("G-", firstcntdate$, datecnt$,     ~
                                                          tempday%,  err%)
                     err% = err%
                     aveperiod = tempday% / (cntnbr - 1)
                     goto L30480
L30475:         aveperiod$ = " "  :  goto L30485
L30480:         call "CONVERT" (aveperiod       , 2.2, aveperiod$)
L30485:         avevariance = (cumcntdltap + cumcntdltam) / cntnbr
                avebohqty   = cumbohqty / cntnbr
                avequantity = avebohqty + avevariance

L30500:     if avebohqty <> 0 then L30510
                avevarprcnt$ = " "  :  goto L30520
L30510:     avevarprcnt = 100 * avevariance / avebohqty
            call "CONVERT" (avevarprcnt     , 2.2 , avevarprcnt$)

L30520:     call "CONVERT" (cnttlernper     , 2.2 , cnttlernper$)
            call "CONVERT" (cnttlernqty     , 2.2 , cnttlernqty$)
            call "CONVERT" (cntnbr          , 2.2 , cntnbr$)
            call "CONVERT" (cumtlernhit     , 0.01, cumtlernhit$)
            call "CONVERT" (avevariance     , 2.2 , avevariance$)
            call "CONVERT" (avequantity     , 2.2 , avequantity$)
            call "CONVERT" (avebohqty       , 2.2 , avebohqty$)
            lcntr% = lcntr% + 1%
            if lcntr% < 56% then L30690
                gosub page_head : gosub column_head : lcntr% = lcntr% + 6%

L30690:     print using    L60250, printkey$, aveperiod$, cumtlernhit$,   ~
                                  avequantity$, avebohqty$, avevariance$,~
                                  avevarprcnt$


            subtotcount = subtotcount + cntnbr
            subtothits  = subtothits  + cumtlernhit
            totalcount  = totalcount  + cntnbr
            totalhits   = totalhits   + cumtlernhit

            goto report_loop
            /* *** End of Report Loop ** */

        print_subtotals
            if lcntr% < 7% and pcntr% = 1% then return
            if subtotcount = 0 then L31050
                subtotprcnt = 100 * subtothits / subtotcount
                call "CONVERT" (subtotprcnt,-2.2, subtotprcnt$)
                goto L31060
L31050:     subtotprcnt$ = " "
L31060:     call "CONVERT" (subtotcount, -0.01, subtotcount$)
            call "CONVERT" (subtothits , -0.01, subtothits$ )
*          PRINT USING 60200
            print using L60290, subtotcount$, subtothits$, subtotprcnt$
            print
            lcntr% = lcntr% + 2%
            subtotcount, subtothits = 0

            return

        print_report_totals
            gosub print_subtotals
            if totalcount = 0 then L31370
                totalprcnt = 100 * totalhits / totalcount
                call "CONVERT" (totalprcnt,-2.2, totalprcnt$)
                goto L31380
L31370:     totalprcnt$ = " "
L31380:     call "CONVERT" (totalcount,-0.01, totalcount$)
            call "CONVERT" (totalhits ,-0.01, totalhits$ )
            print using L60330, totalcount$, totalhits$, totalprcnt$

            goto end_report

        set_printkey /* Parse the HNYCCMST Key & Re-String for Printing */
            i% = arrayindx%(1%)
            len% = arraylen%(i%)  :  pos% = 1%
            prntlen% = arrayprntlen%(i%)  :  prntpos% = 1%
            printkey$ = str(work2key$,1,len%)
            if printkey$ <> lastval1$ then L33600
                printkey$ = " " : goto L33690
L33600:         lastval1$ = printkey$
                gosub print_subtotals
            if arrayvar$(i%) <> "PARTSTRLOT$" then L33690
                store$ = str(printkey$,26%,3%):part$ = str(printkey$,,25%)
                lot$ = str(printkey$,29%,6%)
                str(printkey$,  ,25%) = part$: str(printkey$,26%,1%) = " "
                str(printkey$,27%,3%) = store$:str(printkey$,30%,1%) = " "
                str(printkey$,31%, 6%) = lot$

L33690:     for n% = 2% to 6%
                i% = arrayindx%(n%)
                pos% = pos% + len%  :  len% = arraylen%(i%)
                prntpos% = prntpos% + prntlen%
                prntlen% = arrayprntlen%(i%)
                tempval$ =  str(work2key$,pos%,len%)

                if n% <> 2%  then L33830
                if printkey$ <> " "  then L33820
                if tempval$ <> lastval2$ then L33820
                     tempval$ = " " : goto L33830

L33820:         lastval2$ = tempval$
L33830:         if arrayvar$(i%) <> "PARTSTRLOT$" then L33900
                  store$ = str(tempval$,26%,3%):part$ = str(tempval$,,25%)
                  lot$ = str(tempval$,29%,6%)
                  str(tempval$,  ,25%) = part$ :str(tempval$,26%,1%) = " "
                  str(tempval$,27%,3%) = store$:str(tempval$,30%,1%) = " "
                  str(tempval$,31%, 6%) = lot$

L33900:         printkey$ = str(printkey$,,prntpos% + n% - 2% ) & tempval$
            next n%
            partstrlot$ = str(part$) & str(store$) & lot$

            return

        set_column_headers
            i% = arrayindx%(1%)
            columnhead1$ = arrayhdr1$(i%) : columnhead2$ = arrayhdr2$(i%)
            columnhead3$ = arrayhdr3$(i%)
            lentot% = arrayprntlen%(i%) + 1%
            for n% = 2% to 6%
                i% = arrayindx%(n%)
                columnhead1$ = str(columnhead1$,,lentot%) & arrayhdr1$(i%)
                columnhead2$ = str(columnhead2$,,lentot%) & arrayhdr2$(i%)
                columnhead3$ = str(columnhead3$,,lentot%) & arrayhdr3$(i%)
                lentot% = lentot% + arrayprntlen%(i%) + 1%
                next n%

                columnhead1$ = str(columnhead1$,,64%)                    ~
                               & "    AVG  TOLERANCE  AVE COUNT   AVG B" ~
                               & "OOK    AVERAGE   AVG PRCNT    "
                columnhead2$ = str(columnhead2$,,64%)                    ~
                               & " PERIOD       HITS   QUANTITY   QUANT" ~
                               & "ITY   VARIANCE    VARIANCE    "
                columnhead3$ = str(columnhead3$,,64%)                    ~
                               & " ------ ---------- ---------- -------" ~
                               & "--- ---------- ----------    "
                return

        end_report                /* Report Ending Routine */
            time$ = " "  :  call "TIME" (time$)
            print skip(2)
            print using L64965, time$    /* End of report line */
            print page
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            call "FILEBGON" (#52)   /* Close and Zap Work File */
            if rangeflag$ = "Y" then call "FILEBGON" (#50)/*Zap Range WF*/
            goto inputmode

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "HNYCCHRP", rptid$
            print using L60110, rpttitle$, pcntr%
            print
            if pcntr% = 0% then gosub print_params
            lcntr% = 3%
            return

        column_head
            print columnhead1$
            print columnhead2$
            print columnhead3$
            lcntr% = lcntr% + 3%
            return

        print_params           /* Print Page Zero */
L34805:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L34825
                str(i$(), i%, 1%) = hex(20)
                goto L34805
L34825:     print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            if fmccgroup$ <> "RANGE" then L34935
                print
                for x% = 4% to 20% : print tab(26); r$(x%) : next x%
                print tab(26);
                print "--------------------------------------------------~
        ~------------------------------"
L34935:     gosub page_head
            return


        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35060: FMT                 /* FILE: HNYCCMST                          */~
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
            PD(14,4),       /* Cumulative BOH Quantity                 */~
            POS(196),       /* position                                */~
            CH(6)           /* First Count Date                        */

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
              on fieldnr% gosub L40170,         /* Cycle Count Group */   ~
                                L40170          /* Sort Criteria     */


              goto L40200
                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40200:     accept                                                       ~
               at (01,02),                                               ~
                  "Cycle Count History Report     ",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),   columnttl$           , ch(18),~
                                                                         ~
               at (07,02), "Cycle Count Group Name ",                    ~
               at (07,30), fac(lfac$( 1%)), fmccgroup$          , ch(06),~
               at (07,39), fac(lfac$( 1%)), toccgroup$          , ch(06),~
                                                                         ~
               at (09,30), fac(hex(ac)), sortmsg1$              , ch(01),~
               at (09,32), fac(hex(ac)), sortmsg2$              , ch(01),~
               at (09,34), fac(hex(ac)), sortmsg3$              , ch(01),~
               at (09,41), fac(hex(ac)), sortordmsg$            , ch(18),~
                                                                         ~
               at (10,02), "Sort Order Criteria    ",                    ~
               at (10,30), fac(lfac$( 2%)), sortnum1$           , ch(01),~
               at (10,32), fac(lfac$( 2%)), sortnum2$           , ch(01),~
               at (10,34), fac(lfac$( 2%)), sortnum3$           , ch(01),~
                                                                         ~
               at (10,41), "A = ABC Class",                              ~
               at (11,41), "G = Group Name",                             ~
               at (12,41), "P = Part/Store/Lot",                         ~
               at (13,41), "C = Count Period            ",               ~
               at (14,41), "N = Number of Count Sessions",               ~
               at (15,41), "T = Within Tolerance Percent",               ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(84)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40620
                  call "MANUAL" ("HNYCCRPT") : goto L40200

L40620:        if keyhit% <> 15% then L40650
                  call "PRNTSCRN" : goto L40200

L40650:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40840     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                 (8)Select Ranges       " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffff08ffffffff0dff0f1000)
            if fieldnr% = 1% then L40810
                str(pf$(3%),,63%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
                                             str(pfkeys$, 8%,1%) = hex(ff)
            if fieldnr% > 1% then L40820
L40810:         str(pf$(2%),18%,26%) = " ": str(pfkeys$, 4%,1%) = hex(ff)
                if fmccgroup$ <> " " and fmccgroup$ <> "ALL"             ~
                                     then str(pfkeys$, 8%,1%) = hex(ff)
L40820:     return

L40840: if fieldnr% > 0% then L40930  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                     "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                     "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40930:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50130,         /* Cycle Count Group      */~
                              L50380          /* Sort Order             */

            return

L50130: REM Test for Cycle Count Group Name       FMCCGROUP$
            if fmccgroup$ = " " and toccgroup$ = " " then doto% = 1%     ~
                                                     else doto% = 0%
            if fmccgroup$ = "ALL" then L50200
            if fmccgroup$ = "FIRST" then L50210
                tempval$ = hex(0684) & "Select FROM Cycle Count Group"
                call "GETCODE" (#05, fmccgroup$, tempval$, 0%, 0, f1%(5))
                if fmccgroup$ <> "?" then L50190
L50185:         errormsg$ = "'?' is Not a Valid Range" :  return

L50190:     if toccgroup$ <> " "  or doto% = 1% then  L50210
L50200:         toccgroup$ = fmccgroup$     :   goto  L50260
L50210:     if toccgroup$ = "LAST" then  L50260
                tempval$ = hex(0684) & "Select TO Cycle Count Group"
                call "GETCODE" (#05, toccgroup$, tempval$, 0%, 0, f1%(5))
                if toccgroup$ <> "?" then L50260 else L50185

L50260:     call "TESTRNGE"                                              ~
                  (fmccgroup$          , toccgroup$          ,           ~
                   loccgroup$          , hiccgroup$          ,           ~
                   errormsg$)

            if fmccgroup$ = "FIRST" then  loccgroup$ =  all(hex(20))
            if fmccgroup$ = "ALL" or toccgroup$ = "LAST" then            ~
                                                hiccgroup$ = all(hex(ff))


            return

L50380: REM Test for Sort Order        SORTNUM1$, SORTNUM2$, SORTNUM3$
            p% = pos("AGPCNT" = sortnum1$)
            if p% = 0% then L50480
            p% = pos("AGPCNT" = sortnum2$)
            if p% = 0% then L50480
            if sortnum2$ = sortnum1$ then L50490
            p% = pos("AGPCNT" = sortnum3$)
            if p% = 0% then L50480
            if sortnum3$ = sortnum1$ or sortnum3$ = sortnum2$ then L50490 ~
                else return
L50480:     errormsg$ = "'A', 'G', 'P', 'C', 'N', or 'T' Please." :return
L50490:     errormsg$ = "Can not use same criteria twice."        :return
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

*       * Column Header Lines
        %################################################################~
        ~#################################################################~
        ~#######

*       * Report Sub-line
        %----------------------------------------------------------------~
        ~--------------------------------------------------------------


*       * Report Line 1
L60250: %################################################################~
        ~# ###### ######### ########## ########## ########## ##########


*       * Subtotal Line
L60290: %         ***  NUMBER OF COUNTS: ##########    TOLERANCE HITS: ##~
        ~########   PERCENT: ######  ***

*       * Total Line
L60330: % *****  TOTAL NUMBER OF COUNTS: ##########    TOLERANCE HITS: ##~
        ~########   PERCENT: ######   *****

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
