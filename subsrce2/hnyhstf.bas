        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  H   H   SSS   TTTTT  FFFFF          *~
            *  H   H  NN  N  Y   Y  H   H  S        T    F              *~
            *  HHHHH  N N N   YYY   HHHHH   SSS     T    FFF            *~
            *  H   H  N  NN    Y    H   H      S    T    F              *~
            *  H   H  N   N    Y    H   H   SSS     T    F              *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYHSTF  - Setup or edit the HNYHSTRF records created by  *~
            *            HNYMVMF                                        *~
            *----------------------------------------------------------Q*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/09/87 ! Original                                 ! HES *~
            * 09/01/87 ! Changed to be a subroutine and handle    ! LKM *~
            *          ! history retention by fiscal year         !     *~
            * 03/23/92 ! Added Trans. Code 'JR' to 2nd screen.    ! JDH *~
            * 03/08/93 ! PRR 12806.  Correct edit field number.   ! JDH *~
            * 09/17/93 ! Reformated 2nd screen & added tran typ IZ! WPH *~
            * 06/27/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "HNYHSTF"  (#1,              /* HNYMASTR                   */~
                        #2,              /* STORENAME                  */~
                        #4)              /* SYSFILE2                   */

        dim                                                              ~
            array1(13),                  /* Work Variable              */~
            array2(13),                  /* Work Variable              */~
            array3(13),                  /* Work Variable              */~
            array4(13),                  /* Work Variable              */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bytype$(20)2,                /* Last Month By Type         */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dates$(17)8,                 /* Fiscal Dates               */~
            descr_map(4),                /* Description Map            */~
            errormsg$79,                 /* Error message              */~
            filler$18,                   /* Work Variable              */~
            hdr$(1)1,                    /* PLOWCODE header            */~
            header$(4)10,                /* Screen Title               */~
            header5$13,                  /* Screen Title               */~
            i$(24)80,                    /* Screen Image               */~
            incl_excl(2),                /* Include - Exclude          */~
            incl_excl$(2)1,              /* Include - Exclude          */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            month$(13,4)10,              /* December                   */~
            part$25,                     /* Part Number                */~
            partdescr$34,                /* Part Description           */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            pf6$20,                      /* PF 5 Screen Literal        */~
            plowdescr$30,                /* For PLOWCODE               */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey$50,                  /* Miscellaneous Read Key     */~
            store$3,                     /* Store Number               */~
            storedescr$32,               /* Store Description          */~
            tdate$8,                     /* Temp. Date                 */~
            udate$8,                     /* Temp. Date                 */~
            userid$3,                    /* Current User Id            */~
            work%(20),                   /* Work Variable              */~
            yearmonth$6                  /* Year (CCYYMM)              */

        dim f1%(32)                      /* = 1 if READ was successful */

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
            * # 1 ! HNYMASTR ! Inventory Master File                    *~
            * # 2 ! STORNAME ! STORE INFORMATION FILE                   *~
            * # 3 ! HNYHSTRF ! Caelus Management System Information     *~
            * # 4 ! SYSFILE2 ! System File                              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 3, "HNYHSTRF",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  32                      ~

            f2% = 0
            if fs% > 0% then L09000
            call "OPENCHCK" (# 3, fs%, f2%, 200%, " ")
            if fs% < 0% then L65000          /* Couldn't open or create */

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            header$(1) = " Add./Adj."
            header$(2) = "Total Cost"
            header$(3) = "     Usage"
            header$(4) = "Total Cost"
            header5$ = "Fiscal Period"
            dates% = dates%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
*          CALL "READ100" (#4, "FISCAL DATES", F1%(4))
*               IF F1%(4) = 1 THEN 10130
*               KH% = 2
*               CALL "ASKUSER" (KH%, "SYSFILE2 RECORD NOT FOUND",       ~
*                               "The Fiscal Dates could not be read.",  ~
*                               "Press any key to acknowledge.", " ")
*               GOTO 65000

*          GET #4, USING 10140,  PRDS%, DATES$()
*          FMT XX(20), BI(2), 17*CH(08)
*          PERIODS% = PRDS%

            pf4$, pf5$, pf6$ = " " : pf16$ = "(16)Exit Program"
            errormsg$, inpmessage$, storedescr$, partdescr$, month$(),   ~
            bytype$(), dates$(), filler$ = " "
            proceed% = 0%

            start% = 1
            finish% = 3
L10260:     for fieldnr% = start% to finish%
                gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 or proceed% <> 0 then L10430
L10290:         pf4$, pf6$ = " "
                if fieldnr% > 1% then pf4$ = "(4)Previous Field"
L10320:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10410
L10350:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10290
                         if fieldnr% = 1% then L10260
                         goto L10350
L10410:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <>  0% then L10320
L10430:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ = " " then L10450
                         if skipfield = 0 then L10320
L10450:     next fieldnr%
            if start% = 4% then L10500
               start% = 4%
               finish% = periods% + 3%
               goto L10260

L10500:     pf4$ = " " : pf6$ = "(6)Proceed To Edit"
            proceed% = 0%
            for fieldnr% = 1% to 16%
                if fieldnr% > 1% then pf4$ = "(4)Previous Field"
                gosub'052(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0% or proceed% <> 0% then L10660
L10560:         gosub'102(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10640
L10590:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L10560
                         if fieldnr% = 1% then L10500
                         goto L10590
L10640:               if keyhit% =  6% then proceed% = 1%
                      if keyhit% <>  0% and keyhit% <> 6% then L10560
L10660:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10560
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editmode
            pf4$, pf6$ = " "
            pf5$ = "(5)Next Page"
            if f1%(3%) <> 0 then pf6$  = "(12)Delete Record"
            pf16$ = "(16)Save Data"
            inpmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            lastfieldnr% = 0%
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  5% then       edit_page_two
                  if keyhit%  = 12% then gosub delete_record
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editmode
L11200:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 4% or fieldnr% > periods% + 3% then editmode
            if fieldnr% = lastfieldnr% then editmode
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editmode
                  pf4$, pf5$, pf16$ = " "
L11260:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11260
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11260
                  lastfieldnr% = fieldnr%
            goto L11200

        delete_record
            keyno% = 2%
            call "ASKUSER" (keyno%, "DELETE HISTORY DETAIL",             ~
                   "Press RETURN to delete this history detail",         ~
                   "Press PF-1 To Return to EDITMODE", " ")
                if keyno% <> 0% then return
            convert yearmonth$ to year%
            plowkey$ = str(part$) & str(store$) & bin(year%,4%)
            call "READ101" (#3, plowkey$, f1%(3))
                if f1%(3) = 0 then editmode
            delete #3
            plowkey$ = "FISCAL HISTORY" & str(yearmonth$,1%,6%) & " "
            call "READ101" (#4, plowkey$, f1%(4))
            if f1%(4) <> 1 then inputmode
            delete #4
            goto inputmode

        edit_page_two
            pf5$, pf6$ = " "
            pf4$ = "(4)Previous Page"
            pf16$ = "(16)Save Data"
            inpmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            lastfieldnr% = 0%
            gosub'102(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% then       editmode
                  if keyhit%  = 12% then       delete_record
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       edit_page_two
            if cursor%(1%) > 19% then edit_page_two
L11590:     fieldnr% = cursor%(1%) - 7%
            if cursor%(2%) > 39% then fieldnr% = fieldnr% + 12%
            if fieldnr% < 1% or fieldnr% > 17% then edit_page_two
            if fieldnr% = lastfieldnr% then edit_page_two
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edit_page_two
                  pf4$, pf5$, pf16$ = " "
L11650:     gosub'102(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11650
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11650
                  lastfieldnr% = fieldnr%
            goto L11590

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20260,         /* Part Number        */    ~
                              L20290,         /* Store Number       */    ~
                              L20320,         /* Year               */    ~
                              L20350,         /* January            */    ~
                              L20350,         /* February           */    ~
                              L20350,         /* March              */    ~
                              L20350,         /* April              */    ~
                              L20350,         /* May                */    ~
                              L20350,         /* June               */    ~
                              L20350,         /* July               */    ~
                              L20350,         /* August             */    ~
                              L20350,         /* September          */    ~
                              L20350,         /* October            */    ~
                              L20350,         /* November           */    ~
                              L20350,         /* December           */    ~
                              L20350
            return
L20260:     REM Def/Enable Part Number
                inpmessage$ = "Select Inventory Part Number"
                return
L20290:     REM Def/Enable Store Number
                inpmessage$ = "Select Store Number"
                return
L20320:     REM Def/Enable Year
               inpmessage$="Enter Beginning Year/Month of fiscal year"
               tdate$ = dates$(1)
               call "DATEFMT" (tdate$,0%,udate$)
               if yearmonth$ =  " " then yearmonth$ = str(udate$,1%,6%)
               return
L20350:     REM Def/Enable January
               inpmessage$="Enter Additions & Withdrawals For This Period"
                i% = fieldnr% - 3%
                if month$(i%,1%) = " " then return
                call "STRING" addr("LJ", month$(i%,1%), 10%)
                call "STRING" addr("LJ", month$(i%,2%), 10%)
                call "STRING" addr("LJ", month$(i%,3%), 10%)
                call "STRING" addr("LJ", month$(i%,4%), 10%)
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L21240,     /*  IA = Inventory Addition   */~
                              L21240,     /*  IS = Inter-store movement */~
                              L21240,     /*  IC = Cycle count adjust.  */~
                              L21240,     /*  IP = Physical adjustment  */~
                              L21240,     /*  IW = Inventory Withdrawal */~
                              L21240,     /*  JB = Job Byproduct        */~
                              L21240,     /*  JC = Job Completion       */~
                              L21240,     /*  JK = Job Kitting          */~
                              L21240,     /*  JT = Job Scrap            */~
                              L21240,     /*  PR = PO/QC to Rework      */~
                              L21240,     /*  PO = PO receipt           */~
                              L21240,     /*  PQ = QC to On-Hand        */~
                              L21240,     /*  RT = A/R transaction      */~
                              L21240,     /*  VT = A/P transaction      */~
                              L21240,     /*  IQ = Inventory to Proj.   */~
                              L21240,     /*  JR = To/From Rework Job   */~
                              L21240      /*  IZ = Part to Part Conv.   */
            return
L21240:     REM Def/Enable Last Month By Activity Type
                inpmessage$ = "Enter Highest Period In 'xx That This Acti~
        ~vity Occured For Part.  '0' if Never."
                str(inpmessage$,25,4) = yearmonth$
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
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
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

            convert yearmonth$ to year%
            plowkey$ = str(part$) & str(store$) & bin(year%,4%)
            call "READ100" (#3, plowkey$, f1%(3))
                if f1%(3) = 1 then L30090
                   goto L30130
L30090:     get #3, using L30110, array1(), array2(), array3(), array4(), ~
                                 work%(), filler$
L30110:     FMT POS(33), 52*PD(14,4), 20*BI(1), CH(32)

L30130:     readkey$ = "FISCAL HISTORY" & str(yearmonth$,1%,6%) & " "
            call "READ100" (#4, readkey$, f1%(4))
            if f1%(4) = 1 then L30200
               nodates = 1
               periods% = 13
               init (" ") dates$()
               goto L30210

L30200:     get #4 using L30201, periods%, dates$()
L30201:     FMT POS(21), BI(2), 17*CH(8)
            nodates = 0

L30210:     for i% = 1 to 20
                if f1%(3) = 1 then L30220
                   if i% > periods% then return else L30291
L30220:         if work%(i%) > periods% then work%(i%) = 0%
                convert work%(i%) to bytype$(i%), pic(##)
                gosub'152(i%)
                if i% > periods% then L30300
                if array1(i%) <> 0 then                                  ~
                   call "CONVERT" (array1(i%), 0.2, month$(i%,1%))
                if array2(i%) <> 0 then                                  ~
                   call "CONVERT" (array2(i%), 0.2, month$(i%,2%))
                if array3(i%) <> 0 then                                  ~
                   call "CONVERT" (array3(i%), 2.2, month$(i%,3%))
                if array4(i%) <> 0 then                                  ~
                   call "CONVERT" (array4(i%), 2.2, month$(i%,4%))
L30291:         call "DATEFMT" (dates$(i%))
L30300:     next i%
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            mat array1 = zer : mat array2 = zer
            mat array3 = zer : mat array4 = zer
            mat work%  = zer
            for i% = 1 to 20
                convert bytype$(i%) to work%(i%), data goto L31110
L31110:         if i% > periods% then L31160
                if month$(i%,1%) <> " " then                             ~
                   convert month$(i%,1%) to array1(i%), data goto L31121
L31121:         if month$(i%,2%) <> " " then                             ~
                   convert month$(i%,2%) to array2(i%), data goto L31131
L31131:         if month$(i%,3%) <> " " then                             ~
                   convert month$(i%,3%) to array3(i%), data goto L31141
L31141:         if month$(i%,4%) <> " " then                             ~
                   convert month$(i%,4%) to array4(i%), data goto L31151
L31151:         call "DATUNFMT" (dates$(i%))
L31160:     next i%

            convert yearmonth$ to year%
            plowkey$ = str(part$) & str(store$) & bin(year%,4%)
            call "READ101" (#3, plowkey$, f1%(3))
                if f1%(3) <> 0 then delete #3
            write #3, using L31220, plowkey$, array1(), array2(),         ~
                                    array3(), array4(), work%(), filler$
L31220:     FMT CH(32), 52*PD(14,4), 20*BI(1), CH(32)

            if nodates <> 1 then return
               str(dates$(),105,32) = " "
               put #4 using L31270, "FISCAL HISTORY", str(yearmonth$,1%,6%), " ", ~
                                   periods%, dates$()
L31270:        FMT CH(15), CH(4), CH(1), BI(2), 17*CH(8)
               write #4
               return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              line2$ = " "
              str(line2$,62%) = " HNYHSTF: " & str(cms2v$,,8%)
              if fieldnr% = 0% then L40065
                 init(hex(8c)) lfac$()
                 goto L40075

L40065:       init(hex(86)) lfac$()
L40075:       if fieldnr% = 0% then init(hex(84)) str(lfac$(),,3)
              if fieldnr% < 16 then L40100
              if periods% = 13 then L40100
                 month$(13,1), month$(13,2), month$(13,3), month$(13,4)  ~
                      = " "
                 lfac$(16) = hex(9c)
                 goto L40205
L40100:       on fieldnr% gosub L40190,         /* Part Number       */   ~
                                L40190,         /* Store Number      */   ~
                                L40195,         /* Year - Month      */   ~
                                L40190,         /* January           */   ~
                                L40190,         /* February          */   ~
                                L40190,         /* March             */   ~
                                L40190,         /* April             */   ~
                                L40190,         /* May               */   ~
                                L40190,         /* June              */   ~
                                L40190,         /* July              */   ~
                                L40190,         /* August            */   ~
                                L40190,         /* September         */   ~
                                L40190,         /* October           */   ~
                                L40190,         /* November          */   ~
                                L40190,         /* December          */   ~
                                L40190
              goto L40205

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40190:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40195:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40205:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Inventory Summary Usage History",              ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Part Number",                                ~
               at (04,15), fac(lfac$( 1)), part$                , ch(25),~
               at (04,45), fac(hex(8c)),   partdescr$           , ch(34),~
                                                                         ~
               at (05,02), "Store Number",                               ~
               at (05,15), fac(lfac$( 2)), store$               , ch(03),~
               at (05,45), fac(hex(8c)),   storedescr$          , ch(32),~
                                                                         ~
               at (06,02), "Year/Month",                                 ~
               at (06,15), fac(lfac$( 3)), yearmonth$           , ch(06),~
                                                                         ~
               at (07,04), fac(hex(ac)),   header5$             , ch(13),~
               at (07,20), fac(hex(ac)),   header$(1)           , ch(10),~
               at (07,32), fac(hex(ac)),   header$(2)           , ch(10),~
               at (07,49), fac(hex(ac)),   header$(3)           , ch(10),~
               at (07,61), fac(hex(ac)),   header$(4)           , ch(10),~
                                                                         ~
               at (08,07), fac(lfac$( 4)), dates$(1)            , ch(08),~
               at (08,20), fac(lfac$( 4)), month$(1,1)          , ch(10),~
               at (08,32), fac(lfac$( 4)), month$(1,3)          , ch(10),~
               at (08,49), fac(lfac$( 4)), month$(1,2)          , ch(10),~
               at (08,61), fac(lfac$( 4)), month$(1,4)          , ch(10),~
                                                                         ~
               at (09,07), fac(lfac$( 5)), dates$(2)            , ch(08),~
               at (09,20), fac(lfac$( 5)), month$(2,1)          , ch(10),~
               at (09,32), fac(lfac$( 5)), month$(2,3)          , ch(10),~
               at (09,49), fac(lfac$( 5)), month$(2,2)          , ch(10),~
               at (09,61), fac(lfac$( 5)), month$(2,4)          , ch(10),~
                                                                         ~
               at (10,07), fac(lfac$( 6)), dates$(3)            , ch(08),~
               at (10,20), fac(lfac$( 6)), month$(3,1)          , ch(10),~
               at (10,32), fac(lfac$( 6)), month$(3,3)          , ch(10),~
               at (10,49), fac(lfac$( 6)), month$(3,2)          , ch(10),~
               at (10,61), fac(lfac$( 6)), month$(3,4)          , ch(10),~
                                                                         ~
               at (11,07), fac(lfac$( 7)), dates$(4)            , ch(08),~
               at (11,20), fac(lfac$( 7)), month$(4,1)          , ch(10),~
               at (11,32), fac(lfac$( 7)), month$(4,3)          , ch(10),~
               at (11,49), fac(lfac$( 7)), month$(4,2)          , ch(10),~
               at (11,61), fac(lfac$( 7)), month$(4,4)          , ch(10),~
                                                                         ~
               at (12,07), fac(lfac$( 8)), dates$(5)            , ch(08),~
               at (12,20), fac(lfac$( 8)), month$(5,1)          , ch(10),~
               at (12,32), fac(lfac$( 8)), month$(5,3)          , ch(10),~
               at (12,49), fac(lfac$( 8)), month$(5,2)          , ch(10),~
               at (12,61), fac(lfac$( 8)), month$(5,4)          , ch(10),~
                                                                         ~
               at (13,07), fac(lfac$( 9)), dates$(6)            , ch(08),~
               at (13,20), fac(lfac$( 9)), month$(6,1)          , ch(10),~
               at (13,32), fac(lfac$( 9)), month$(6,3)          , ch(10),~
               at (13,49), fac(lfac$( 9)), month$(6,2)          , ch(10),~
               at (13,61), fac(lfac$( 9)), month$(6,4)          , ch(10),~
                                                                         ~
               at (14,07), fac(lfac$(10)), dates$(7)            , ch(08),~
               at (14,20), fac(lfac$(10)), month$(7,1)          , ch(10),~
               at (14,32), fac(lfac$(10)), month$(7,3)          , ch(10),~
               at (14,49), fac(lfac$(10)), month$(7,2)          , ch(10),~
               at (14,61), fac(lfac$(10)), month$(7,4)          , ch(10),~
                                                                         ~
               at (15,07), fac(lfac$(11)), dates$(8)            , ch(08),~
               at (15,20), fac(lfac$(11)), month$(8,1)          , ch(10),~
               at (15,32), fac(lfac$(11)), month$(8,3)          , ch(10),~
               at (15,49), fac(lfac$(11)), month$(8,2)          , ch(10),~
               at (15,61), fac(lfac$(11)), month$(8,4)          , ch(10),~
                                                                         ~
               at (16,07), fac(lfac$(12)), dates$(9)            , ch(08),~
               at (16,20), fac(lfac$(12)), month$(9,1)          , ch(10),~
               at (16,32), fac(lfac$(12)), month$(9,3)          , ch(10),~
               at (16,49), fac(lfac$(12)), month$(9,2)          , ch(10),~
               at (16,61), fac(lfac$(12)), month$(9,4)          , ch(10),~
                                                                         ~
               at (17,07), fac(lfac$(13)), dates$(10)           , ch(08),~
               at (17,20), fac(lfac$(13)), month$(10,1)         , ch(10),~
               at (17,32), fac(lfac$(13)), month$(10,3)         , ch(10),~
               at (17,49), fac(lfac$(13)), month$(10,2)         , ch(10),~
               at (17,61), fac(lfac$(13)), month$(10,4)         , ch(10),~
                                                                         ~
               at (18,07), fac(lfac$(14)), dates$(11)           , ch(08),~
               at (18,20), fac(lfac$(14)), month$(11,1)         , ch(10),~
               at (18,32), fac(lfac$(14)), month$(11,3)         , ch(10),~
               at (18,49), fac(lfac$(14)), month$(11,2)         , ch(10),~
               at (18,61), fac(lfac$(14)), month$(11,4)         , ch(10),~
                                                                         ~
               at (19,07), fac(lfac$(15)), dates$(12)           , ch(08),~
               at (19,20), fac(lfac$(15)), month$(12,1)         , ch(10),~
               at (19,32), fac(lfac$(15)), month$(12,3)         , ch(10),~
               at (19,49), fac(lfac$(15)), month$(12,2)         , ch(10),~
               at (19,61), fac(lfac$(15)), month$(12,4)         , ch(10),~
                                                                         ~
               at (20,07), fac(lfac$(16)), dates$(13)           , ch(08),~
               at (20,20), fac(lfac$(16)), month$(13,1)         , ch(10),~
               at (20,32), fac(lfac$(16)), month$(13,3)         , ch(10),~
               at (20,49), fac(lfac$(16)), month$(13,2)         , ch(10),~
               at (20,61), fac(lfac$(16)), month$(13,4)         , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,20), fac(hex(8c)), pf4$                           ,~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf5$                           ,~
               at (23,65), "(15)Print Screen",                           ~
               at (24,20), fac(hex(8c)), pf6$                           ,~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(00010405060c0d0f10)),                            ~
               key (keyhit%)

               if keyhit% <> 13 then L41450
                  call "MANUAL" ("HNYHSTF")
                  goto L40205

L41450:        if keyhit% <> 15 then L41490
                  call "PRNTSCRN"
                  goto L40205

L41490:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%)
              line2$ = "Part: " & part$
              line2$ = line2$ & "   Store: " & store$
              str(line2$,62%) = " HNYHSTF: " & str(cms2v$,,8%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()

            on fieldnr% gosub L42300,     /*  IA = Inventory Addition   */~
                              L42300,     /*  IS = Inter-store movement */~
                              L42300,     /*  IC = Cycle count adjust.  */~
                              L42300,     /*  IP = Physical adjustment  */~
                              L42300,     /*  IW = Inventory Withdrawal */~
                              L42300,     /*  JB = Job Byproduct        */~
                              L42300,     /*  JC = Job Completion       */~
                              L42300,     /*  JK = Job Kitting          */~
                              L42300,     /*  JT = Job Scrap            */~
                              L42300,     /*  PR = PO/QC to Rework      */~
                              L42300,     /*  PO = PO receipt           */~
                              L42300,     /*  PQ = QC to On-Hand        */~
                              L42300,     /*  RT = A/R transaction      */~
                              L42300,     /*  VT = A/P transaction      */~
                              L42300,     /*  IQ = Inventory to Proj.   */~
                              L42300,     /*  JR = To/From Rework Job   */~
                              L42300      /*  IZ = Part to Part Conv.   */
              goto L42340

L42300:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
                  lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42340:     accept                                                       ~
               at (01,02),                                               ~
                  "Manage Inventory Summary Usage History",              ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Part Number",                                ~
               at (04,15), fac(hex(84)),   part$                , ch(25),~
               at (04,45), fac(hex(8c)),   partdescr$           , ch(34),~
                                                                         ~
               at (05,04), "Last Period in which",                       ~
               at (06,04), "Part Had Activity of:",                      ~
                                                                         ~
               at (08,02), "Direct Addition",                            ~
               at (08,29), fac(lfac$( 1)), bytype$(1)           , ch(02),~
                                                                         ~
               at (09,02), "Inter-store Movement",                       ~
               at (09,29), fac(lfac$( 2)), bytype$(2)           , ch(02),~
                                                                         ~
               at (10,02), "Cycle Count Adjustment",                     ~
               at (10,29), fac(lfac$( 3)), bytype$(3)           , ch(02),~
                                                                         ~
               at (11,02), "Physical Adjustment",                        ~
               at (11,29), fac(lfac$( 4)), bytype$(4)           , ch(02),~
                                                                         ~
               at (12,02), "Direct Withdrawal",                          ~
               at (12,29), fac(lfac$( 5)), bytype$(5)           , ch(02),~
                                                                         ~
               at (13,02), "Reported as a Byproduct",                    ~
               at (13,29), fac(lfac$( 6)), bytype$(6)           , ch(02),~
                                                                         ~
               at (14,02), "Produced by a Job",                          ~
               at (14,29), fac(lfac$( 7)), bytype$(7)           , ch(02),~
                                                                         ~
               at (15,02), "Kitted (issued) To a Job",                   ~
               at (15,29), fac(lfac$( 8)), bytype$(8)           , ch(02),~
                                                                         ~
               at (16,02), "Scrapped From A Job",                        ~
               at (16,29), fac(lfac$( 9)), bytype$(9)           , ch(02),~
                                                                         ~
               at (17,02), "Reworked From QC",                           ~
               at (17,29), fac(lfac$(10)), bytype$(10)          , ch(02),~
                                                                         ~
               at (18,02), "Received on a P.O.",                         ~
               at (18,29), fac(lfac$(11)), bytype$(11)          , ch(02),~
                                                                         ~
               at (19,02), "Received from QC",                           ~
               at (19,29), fac(lfac$(12)), bytype$(12)          , ch(02),~
                                                                         ~
               at (08,42), "Shipped via A/R (sold)",                     ~
               at (08,69), fac(lfac$(13)), bytype$(13)          , ch(02),~
                                                                         ~
               at (09,42), "Added Through A/P (no PO)",                  ~
               at (09,69), fac(lfac$(14)), bytype$(14)          , ch(02),~
                                                                         ~
               at (10,42), "Moved to a Project",                         ~
               at (10,69), fac(lfac$(15)), bytype$(15)          , ch(02),~
                                                                         ~
               at (11,42), "Moved to/from a Rework Job",                 ~
               at (11,69), fac(lfac$(16)), bytype$(16)          , ch(02),~
                                                                         ~
               at (12,42), "Part to Part Conversion",                    ~
               at (12,69), fac(lfac$(17)), bytype$(17)          , ch(02),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,20), fac(hex(8c)), pf4$                           ,~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf5$                           ,~
               at (23,65), "(15)Print Screen",                           ~
               at (24,20), fac(hex(8c)), pf6$                           ,~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(00010405060c0d0f10)),                            ~
               key (keyhit%)

               if keyhit% <> 13 then L43210
                  call "MANUAL" ("HNYHSTF")
                  goto L42340

L43210:        if keyhit% <> 15 then L43250
                  call "PRNTSCRN"
                  goto L42340

L43250:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            skipfield = 0
            on fieldnr% gosub L50270,         /* Part Number       */     ~
                              L50310,         /* Store Number      */     ~
                              L50350,         /* Year - Month      */     ~
                              L50560,         /* January           */     ~
                              L50560,         /* February          */     ~
                              L50560,         /* March             */     ~
                              L50560,         /* April             */     ~
                              L50560,         /* May               */     ~
                              L50560,         /* June              */     ~
                              L50560,         /* July              */     ~
                              L50560,         /* August            */     ~
                              L50560,         /* September         */     ~
                              L50560,         /* October           */     ~
                              L50560,         /* November          */     ~
                              L50560,         /* December          */     ~
                              L50560
                return

L50270:     REM Test for Part Number
                call "GETCODE" (#1, part$, partdescr$, 1%, 0, f1%(1))
                return

L50310:     REM Test for Store Number
                call "GETCODE" (#2, store$, storedescr$, 1%, 0, f1%(2))
                return

L50350:     REM Test for Year - Month
                year% = 0%
                convert yearmonth$ to year%, data goto l50360
l50360:         descr_map(1%) = 29.04: descr_map(2%) = -1.06
                plowkey$ = str(part$) & str(store$) & bin(year%,4%)
                plowdescr$ =  hex(06) & "Select Beginning Year/Month"
                call "PLOWCODE" (#3,plowkey$,plowdescr$,9028%,0.30,f1%(3),~
                     hdr$(), 0, 0, incl_excl(), incl_excl$(), "D", " ", #3, ~
                     descr_map())
                if f1%(3) = 1 then L50430
                   if yearmonth$ <> " " then L50440
                      errormsg$ = "There is no usage history on file for ~
        ~this part"
                      return
L50430:         year% = val( str(plowkey$,29%,4%), 4%)
                convert year% to yearmonth$, pic (000000)
                yearmonth$ = str( yearmonth$, 1%, 6% )
L50440:         gosub L30000
                if nodates <> 1 then L50490
                   periods% = 13%
                   errormsg$ = "No dates on file for this fiscal year."
                   skipfield = 1
L50490:         if periods%=13% then L50520
                   dates$(13) = " "
                   lfac$(16) = hex(9c)
L50520:         if f1%(3) = 0 then return
                return clear all
                goto editmode

L50560:     REM Test for January - December
                i% = fieldnr% - 3%
                call "DATEOK" (dates$(i%), dates%, errormsg$)
                     if errormsg$ = " " then L50640
                        if i% < 13 then return
                           errormsg$ = " "
                           periods% = 12
                           return
L50640:         call "NUMTEST" (month$(i%,1), -9e7, 9e7,errormsg$,-0.2,0)
                     if month$(i%,1)="         0" then month$(i%,1)=" "
                     if errormsg$ <> " " then return
                call "NUMTEST" (month$(i%,3), -9e7, 9e7,errormsg$,-2.2,0)
                     if month$(i%,3)="      0.00" then month$(i%,3)=" "
                     if errormsg$ <> " " then return
                call "NUMTEST" (month$(i%,2), -9e7, 9e7,errormsg$,-0.2,0)
                     if month$(i%,2)="         0" then month$(i%,2)=" "
                     if errormsg$ <> " " then return
                call "NUMTEST" (month$(i%,4), -9e7, 9e7,errormsg$,-2.2,0)
                     if month$(i%,4)="      0.00" then month$(i%,4)=" "
                return

            REM Test for Last Activity Dates
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51240,     /*  IA = Inventory Addition   */~
                              L51240,     /*  IS = Inter-store movement */~
                              L51240,     /*  IC = Cycle count adjust.  */~
                              L51240,     /*  IP = Physical adjustment  */~
                              L51240,     /*  IW = Inventory Withdrawal */~
                              L51240,     /*  JB = Job Byproduct        */~
                              L51240,     /*  JC = Job Completion       */~
                              L51240,     /*  JK = Job Kitting          */~
                              L51240,     /*  JT = Job Scrap            */~
                              L51240,     /*  PR = PO/QC to Rework      */~
                              L51240,     /*  PO = PO receipt           */~
                              L51240,     /*  PQ = QC to On-Hand        */~
                              L51240,     /*  RT = A/R transaction      */~
                              L51240,     /*  VT = A/P transaction      */~
                              L51240,     /*  IQ = Inventory to Proj.   */~
                              L51240,     /*  JR = To/From Rework Job   */~
                              L51240      /*  IZ = Part to Part Conv.   */
                return

L51240:     REM Test for Last Month By Activity Type
                call "NUMTEST" (bytype$(fieldnr%),0,12,errormsg$,-.001,x)
                     if errormsg$ <> " " then return
                if x = 0 then return
                return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program

            end
