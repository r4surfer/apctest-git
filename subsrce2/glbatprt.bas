        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   GGG   L      BBBB    AAA   TTTTT  PPPP   RRRR   TTTTT   *~
            *  G   G  L      B   B  A   A    T    P   P  R   R    T     *~
            *  G      L      BBBB   AAAAA    T    PPPP   RRRR     T     *~
            *  G  GG  L      B   B  A   A    T    P      R   R    T     *~
            *   GGG   LLLLL  BBBB   A   A    T    P      R   R    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLBATPRT - Subroutine to print listing of batch records   *~
            *            with range by Batch Status and/or posting      *~
            *            date.                                          *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09-20-90 ! Original                                 ! RAC *~
            * 12/12/90 ! Added Management Status                  ! JDH *~
            * 06/25/91 ! Conditioned printing of Interim GL Export! JBK *~
            *          !   File data.                             !     *~
            * 07/18/91 ! Added Report ID.                         ! JDH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "GLBATPRT"    (#1,                  /* Batch file to print */~
                           interim_file_on$)    /* Interim Export file */

        dim                                                              ~
            columnttl$51,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fmbatch$3,                   /* Batch Status to Print      */~
            fmpostdate$8,                /* Posting Date Range         */~
            hibatch$1,                   /* Batch Status to Print      */~
            hipostdate$8,                /* Posting Date Range         */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            interim_file_on$1,           /* Interim Export File in use */~
            interim_file$8,              /* Interim Export File Name   */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lobatch$3,                   /* Batch Status to Print      */~
            lopostdate$8,                /* Posting Date Range         */~
            mgt_status$1,                /* Management Status          */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            pstseq$10,                   /* Printed posting sequence # */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rptid$6,                     /* Report ID                  */~
            rpttitle$60,                 /* Report Title               */~
            time$8,                      /* System Time                */~
            tobatch$1,                   /* Batch Status to Print      */~
            topostdate$8,                /* Posting Date Range         */~
            userid$3                     /* Current User Id            */~


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
        REM *************************************************************


                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************



        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (22%, company$, u3%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "Print CMS Posting Batch Record" &               ~
                        " Listing                      "
            rptid$ = "G/L010"

            str(columnttl$, 1) = "Beginning Code"
            str(columnttl$,27) = "Ending Code"

            str(line2$,62) = "GLBATPRT: " & str(cms2v$,,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

            gosub initialize_variables

            for fieldnr% = 1% to  2%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then end
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
                  if keyhit%  =  1% then end
                  if keyhit%  = 16% then       extract_data
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% >  2% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then end
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

*       * Insert Logic here for data extraction and workfile creation

            goto generate_report

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Batch Status           */~
                              L20200          /* Posting Date Rang      */
            return
L20100: REM Def/Enable Batch Status to Print       FMBATCH$
            if fmbatch$ = " " then fmbatch$ = "ALL"
            return

L20200: REM Def/Enable Posting Date Range          FMPOSTDATE$
            if fmpostdate$         = " " then                            ~
               fmpostdate$         = "ALL"
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
         "Enter Batch Status to Print, Blank for All                   ",~
         "Enter Posting Date Range                                     "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      fmbatch$, fmpostdate$, hibatch$, hipostdate$,      ~
                      lobatch$, lopostdate$, tobatch$, topostdate$
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *************************************************************

        REM *************************************************************~
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************
        generate_report
            call "SHOSTAT" ("Printing Batch Status Report")
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("GLX01", " ", 0%, 0%)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
            if lcntr% > 56% then gosub page_head

*       * Report Generation Logic goes here
            plowkey$ = lobatch$
            call "PLOWALTS" (#1, plowkey$, 1%, 0%, f1%)
                goto L30154
L30152:     call "READNEXT" (#1, f1%)
L30154:         if f1% = 0% then end_report
            get #1 using L30164, batchid$, userid$, modno$, jnlid$,       ~
                pstseq%, dateposted$, interim_file$, mgt_status$
L30164:     FMT CH(1), CH(3), CH(2), CH(3), BI(4), XX(2), CH(6), CH(8),  ~
                POS(62), CH(1)
            if batchid$ < lobatch$ then L30152
            if batchid$ > hibatch$ then end_report
            if dateposted$ < lopostdate$ then L30152
            if dateposted$ > hipostdate$ then L30152
            call "DATEFMT" (dateposted$)
            convert pstseq% to pstseq$, pic(#########)
            lcntr% = lcntr% + 1%
            if lcntr% > 56% then gosub page_head
            print using L60270, batchid$, mgt_status$, modno$, jnlid$,    ~
                               pstseq$, dateposted$, userid$,            ~
                               interim_file$
            goto L30152

        end_report                /* Report Ending Routine */
            print skip(2)
            print using L64990     /* End of report line */
            close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            goto L65000

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            if pcntr% = 0% then gosub print_params
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "GLBATPRT", rptid$
            print using L60110, rpttitle$, pcntr%
            print
            if interim_file_on$ <> "Y" then L34070
            print using L60160
            print using L60200
            print using L60240
            print using L60160
            goto L34077
L34070:     print using L60150
            print using L60190
            print using L60230
            print using L60150
L34077:     print
            lcntr% = 8%
            return

        print_params           /* Print Page Zero */
            print page
            tran(i$(), hex(208c2084208620ac))replacing
            print using L64980, rpttitle$
            print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            pcntr% = pcntr% + 1%
            return

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
              on fieldnr% gosub L40085,         /* Batch Status      */   ~
                                L40085          /* Posting Date Rang */
              goto L40095

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
                  lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40085:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40095:     accept                                                       ~
               at (01,02),                                               ~
                  "Input Report Selection Criteria",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),   columnttl$           , ch(51),~
                                                                         ~
               at (07,02), "Batch Status to Print",                      ~
               at (07,30), fac(lfac$( 1)), fmbatch$             , ch(03),~
               at (07,56), fac(lfac$( 1)), tobatch$             , ch(01),~
                                                                         ~
               at (08,02), "Posting Date Range",                         ~
               at (08,30), fac(lfac$( 2)), fmpostdate$          , ch(08),~
               at (08,56), fac(lfac$( 2)), topostdate$          , ch(08),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40240
                  call "MANUAL" ("GLBATPRT") : goto L40095

L40240:        if keyhit% <> 15 then L40255
                  call "PRNTSCRN" : goto L40095

L40255:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40350     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40335
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% > 1% then L40340
L40335:         str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40340:     return

L40350: if fieldnr% > 0% then L40395  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Return                               " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40395:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Return                               " &        ~
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
            on fieldnr% gosub L50100,         /* Batch Status           */~
                              L50200          /* Posting Date Rang      */
            return
L50100: REM Test for Batch Status to Print        FMBATCH$
            call "TESTRNGE"                                              ~
                  (fmbatch$            , tobatch$            ,           ~
                   lobatch$            , hibatch$            ,           ~
                   errormsg$)
            if fmbatch$ = "ALL" then return
            if fmbatch$ < "1" or fmbatch$ > "8" then L50118
            if tobatch$ < "1" or tobatch$ > "8" then L50118
            return
L50118:     errormsg$ = "Range must be between '1' and '8'"
            return

L50200: REM Test for Posting Date Range           FMPOSTDATE$
            if fmpostdate$ = "ALL" then L50310
            if topostdate$ = " " then topostdate$ = fmpostdate$
            call "DATEOKC" (fmpostdate$, temp1%, errormsg$)
            if errormsg$ <> " " then return
            call "DATEOKC" (topostdate$, temp2%, errormsg$)
            if errormsg$ <> " " then return
            if temp1% > temp2% then L50340
            lopostdate$ = fmpostdate$ : call "DATUFMTC" (lopostdate$)
            hipostdate$ = topostdate$ : call "DATUFMTC" (hipostdate$)
            return
L50310:     lopostdate$ = all(hex(00))
            hipostdate$ = all(hex(39))
            return
L50340:     errormsg$ = "Invalid Posting Date Range"
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
L60150: %+--------+--------+---------+-----------+----------+------+
L60160: %+--------+--------+---------+-----------+----------+------+-----~
        ~--------+

*       * Header Line 4
L60190: %! Batch  ! Module ! Journal ! Posting   !   Date   ! User !
L60200: %! Batch  ! Module ! Journal ! Posting   !   Date   ! User !   In~
        ~terim   !

*       * Header Line 5
L60230: %! Status !   ID   !   ID    ! Sequence  !  Posted  !  ID  !
L60240: %! Status !   ID   !   ID    ! Sequence  !  Posted  !  ID  ! Expo~
        ~rt File !

*       * Item Line
L60270: %   #:#       ##       ###     #########   ########    ###   ####~
        ~####

        %** Report Title for page 0
L64980: %############################################################

L64990: %                                  * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T   * * * * * * * * * *

L65000: REM THISPROGRAMWASGENERATEDBYGENRPPGMAPROPRIETRYPRODUCTOFCAELUS**~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program

            end
