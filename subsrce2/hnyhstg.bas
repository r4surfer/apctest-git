        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  H   H   SSS   TTTTT   GGGG          *~
            *  H   H  NN  N  Y   Y  H   H  S        T    G              *~
            *  HHHHH  N N N   YYY   HHHHH   SSS     T    G  GGG         *~
            *  H   H  N  NN    Y    H   H      S    T    G    G         *~
            *  H   H  N   N    Y    H   H   SSS     T     GGGGG         *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYHSTG -  Setup or edit the HNYHSTRY records created by  *~
            *            HNYPST2                                        *~
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
            * 09/01/87 ! Made into a subroutine to accomodate     ! LKM *~
            *          ! fiscal and Gregorian calendar structures.!     *~
            *          ! This routine handles Gregorian.          !     *~
            * 03/23/92 ! Added Trans. Code 'JR' to 2nd screen.    ! JDH *~
            * 03/08/93 ! PRR 12805.  Correct edit field number.   ! JDH *~
            * 09/17/93 ! Reformated 2nd screen & added tran typ IZ! WPH *~
            * 06/27/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "HNYHSTG"  (#1,              /* HNYMASTR                   */~
                        #2)              /* STORENAME                  */

        dim                                                              ~
            array1(12),                  /* Work Variable              */~
            array2(12),                  /* Work Variable              */~
            array3(12),                  /* Work Variable              */~
            array4(12),                  /* Work Variable              */~
            bydescr$(20)9,               /* Month Description (screen) */~
            bytype$(20)2,                /* Last Month By Type         */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            errormsg$79,                 /* Error message              */~
            filler$18,                   /* Work Variable              */~
            header$(4)10,                /* Screen Title               */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            month$(12,4)10,              /* December                   */~
            names$(12)20,                /* Month Descriptions         */~
            part$25,                     /* Part Number                */~
            partdescr$34,                /* Part Description           */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            pf6$20,                      /* PF 5 Screen Literal        */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            store$3,                     /* Store Number               */~
            storedescr$32,               /* Store Description          */~
            tdate$8,                     /* Temp. Date                 */~
            udate$8,                     /* Temp. Date                 */~
            userid$3,                    /* Current User Id            */~
            work%(20),                   /* Work Variable              */~
            year$4                       /* Year                       */

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
            * # 3 ! HNYHSTRY ! Caelus Management System Information     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 3, "HNYHSTRY",                                      ~
                        varc,     indexed,  recsize =  450,              ~
                        keypos =    1, keylen =  30                      ~

            f2% = 0
            if fs% > 0% then L09000
            call "OPENCHCK" (# 3, f2%, fs%, 200%, " ")
            if fs% < 0% then L65000          /* Couldn't Open or Create */

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            header$(1) = " Add./Adj."
            header$(2) = "Total Cost"
            header$(3) = "     Usage"
            header$(4) = "Total Cost"

            names$(1) = "January"
            names$(2) = "February"
            names$(3) = "March"
            names$(4) = "April"
            names$(5) = "May"
            names$(6) = "June"
            names$(7) = "July"
            names$(8) = "August"
            names$(9) = "September"
            names$(10) = "October"
            names$(11) = "November"
            names$(12) = "December"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$, pf5$, pf6$ = " " : pf16$ = "(16)Exit Program"
            errormsg$, inpmessage$, storedescr$, partdescr$, month$(),   ~
            bytype$(), bydescr$(), filler$ = " "
            proceed% = 0%

L10120:     for fieldnr% = 1% to 15%
                gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0% or proceed% <> 0% then L10290
L10150:         pf4$, pf6$ = " "
                if fieldnr% > 1% then pf4$ = "(4)Previous Field"
                if fieldnr% > 3% then pf6$ = "(6)Proceed To Edit"
L10180:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10260
L10210:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10150
                         if fieldnr% = 1% then L10120
                         goto L10210
L10260:               if keyhit% =  6% then proceed% = 1%
                      if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% and keyhit% <> 6% then L10180
L10290:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10180
            next fieldnr%

L10330:     pf4$ = " " : pf6$ = "(6)Proceed To Edit"
            proceed% = 0%
            for fieldnr% = 1% to 16%
                if fieldnr% > 1% then pf4$ = "(4)Previous Field"
                gosub'052(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0% or proceed% <> 0% then L10490
L10390:         gosub'102(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10470
L10420:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L10390
                         if fieldnr% = 1% then L10330
                         goto L10420
L10470:               if keyhit% =  6% then proceed% = 1%
                      if keyhit% <>  0% and keyhit% <> 6% then L10390
L10490:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10390
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editmode
            pf4$, pf6$ = " "
            pf5$ = "(5)Next Page"
            if f1%(3) <> 0 then pf6$  = "(12)Delete Record"
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
            if fieldnr% < 4% or fieldnr% > 16% then editmode
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
            convert year$ to year%
            plowkey$ = str(part$) & str(store$) & bin(year%,2)
            call "READ101" (#3, plowkey$, f1%(3))
                if f1%(3) = 0 then editmode
            delete #3
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
                              L20350          /* December           */
            return
L20260:     REM Def/Enable Part Number
                inpmessage$ = "Select Inventory Part Number"
                return
L20290:     REM Def/Enable Store Number
                inpmessage$ = "Select Store Number"
                return
L20320:     REM Def/Enable Year
               inpmessage$="Which Year Of History Do You Wish To Manage?"
               return
L20350:     REM Def/Enable January
               inpmessage$="Enter Additions & Withdrawals For This Month"
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
                inpmessage$ = "Enter Highest Month In 'xxx That This Activ~
        ~ity Occured For Part.  '0' if Never."
                str(inpmessage$,24%,4%) = year$
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

            convert year$ to year%
            plowkey$ = str(part$) & str(store$) & bin(year%,2%)
            call "READ100" (#3, plowkey$, f1%(3))
                if f1%(3) = 0 then return
            get #3, using L30110, array1(), array2(), array3(), array4(), ~
                                 work%(), filler$
L30110:     FMT POS(31), 48*PD(14,4), 20*BI(1), CH(16)
            for i% = 1 to 20
                if work%(i%) > 12% then work%(i%) = 0%
                convert work%(i%) to bytype$(i%), pic(##)
                gosub'152(i%)
                if i% > 12 then L30210
                call "CONVERT" (array1(i%), 0.2, month$(i%,1%))
                call "CONVERT" (array2(i%), 0.2, month$(i%,2%))
                call "CONVERT" (array3(i%), 2.2, month$(i%,3%))
                call "CONVERT" (array4(i%), 2.2, month$(i%,4%))
L30210:     next i%
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
L31110:         if i% > 12 then L31160
                convert month$(i%,1%) to array1(i%), data goto L31130
L31130:         convert month$(i%,2%) to array2(i%), data goto L31140
L31140:         convert month$(i%,3%) to array3(i%), data goto L31150
L31150:         convert month$(i%,4%) to array4(i%), data goto L31160
L31160:     next i%

            convert year$ to year%
            plowkey$ = str(part$) & str(store$) & bin(year%,2%)
            call "READ101" (#3, plowkey$, f1%(3))
                if f1%(3) <> 0 then delete #3
            write #3, using L31220, plowkey$, array1(), array2(),         ~
                                    array3(), array4(), work%(), filler$
L31220:     FMT CH(30), 48*PD(14,4), 20*BI(1), CH(16)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              line2$ = " "
              str(line2$,62%) = " HNYHSTG: " & str(cms2v$,,8%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              if fieldnr% = 0% then init(hex(84)) str(lfac$(),,3)
              on fieldnr% gosub L40300,         /* Part Number       */   ~
                                L40300,         /* Store Number      */   ~
                                L40300,         /* Year              */   ~
                                L40300,         /* January           */   ~
                                L40300,         /* February          */   ~
                                L40300,         /* March             */   ~
                                L40300,         /* April             */   ~
                                L40300,         /* May               */   ~
                                L40300,         /* June              */   ~
                                L40300,         /* July              */   ~
                                L40300,         /* August            */   ~
                                L40300,         /* September         */   ~
                                L40300,         /* October           */   ~
                                L40300,         /* November          */   ~
                                L40300          /* December          */
              goto L40330

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40300:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40330:     accept                                                       ~
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
               at (06,02), "Year",                                       ~
               at (06,15), fac(lfac$( 3)), year$                , ch(04),~
                                                                         ~
               at (07,20), fac(hex(ac)),   header$(1)           , ch(10),~
               at (07,32), fac(hex(ac)),   header$(2)           , ch(10),~
               at (07,49), fac(hex(ac)),   header$(3)           , ch(10),~
               at (07,61), fac(hex(ac)),   header$(4)           , ch(10),~
                                                                         ~
               at (08,04), "January",                                    ~
               at (08,20), fac(lfac$( 4)), month$(1,1)          , ch(10),~
               at (08,32), fac(lfac$( 4)), month$(1,3)          , ch(10),~
               at (08,49), fac(lfac$( 4)), month$(1,2)          , ch(10),~
               at (08,61), fac(lfac$( 4)), month$(1,4)          , ch(10),~
                                                                         ~
               at (09,04), "February",                                   ~
               at (09,20), fac(lfac$( 5)), month$(2,1)          , ch(10),~
               at (09,32), fac(lfac$( 5)), month$(2,3)          , ch(10),~
               at (09,49), fac(lfac$( 5)), month$(2,2)          , ch(10),~
               at (09,61), fac(lfac$( 5)), month$(2,4)          , ch(10),~
                                                                         ~
               at (10,04), "March",                                      ~
               at (10,20), fac(lfac$( 6)), month$(3,1)          , ch(10),~
               at (10,32), fac(lfac$( 6)), month$(3,3)          , ch(10),~
               at (10,49), fac(lfac$( 6)), month$(3,2)          , ch(10),~
               at (10,61), fac(lfac$( 6)), month$(3,4)          , ch(10),~
                                                                         ~
               at (11,04), "April",                                      ~
               at (11,20), fac(lfac$( 7)), month$(4,1)          , ch(10),~
               at (11,32), fac(lfac$( 7)), month$(4,3)          , ch(10),~
               at (11,49), fac(lfac$( 7)), month$(4,2)          , ch(10),~
               at (11,61), fac(lfac$( 7)), month$(4,4)          , ch(10),~
                                                                         ~
               at (12,04), "May",                                        ~
               at (12,20), fac(lfac$( 8)), month$(5,1)          , ch(10),~
               at (12,32), fac(lfac$( 8)), month$(5,3)          , ch(10),~
               at (12,49), fac(lfac$( 8)), month$(5,2)          , ch(10),~
               at (12,61), fac(lfac$( 8)), month$(5,4)          , ch(10),~
                                                                         ~
               at (13,04), "June",                                       ~
               at (13,20), fac(lfac$( 9)), month$(6,1)          , ch(10),~
               at (13,32), fac(lfac$( 9)), month$(6,3)          , ch(10),~
               at (13,49), fac(lfac$( 9)), month$(6,2)          , ch(10),~
               at (13,61), fac(lfac$( 9)), month$(6,4)          , ch(10),~
                                                                         ~
               at (14,04), "July",                                       ~
               at (14,20), fac(lfac$(10)), month$(7,1)          , ch(10),~
               at (14,32), fac(lfac$(10)), month$(7,3)          , ch(10),~
               at (14,49), fac(lfac$(10)), month$(7,2)          , ch(10),~
               at (14,61), fac(lfac$(10)), month$(7,4)          , ch(10),~
                                                                         ~
               at (15,04), "August",                                     ~
               at (15,20), fac(lfac$(11)), month$(8,1)          , ch(10),~
               at (15,32), fac(lfac$(11)), month$(8,3)          , ch(10),~
               at (15,49), fac(lfac$(11)), month$(8,2)          , ch(10),~
               at (15,61), fac(lfac$(11)), month$(8,4)          , ch(10),~
                                                                         ~
               at (16,04), "September",                                  ~
               at (16,20), fac(lfac$(12)), month$(9,1)          , ch(10),~
               at (16,32), fac(lfac$(12)), month$(9,3)          , ch(10),~
               at (16,49), fac(lfac$(12)), month$(9,2)          , ch(10),~
               at (16,61), fac(lfac$(12)), month$(9,4)          , ch(10),~
                                                                         ~
               at (17,04), "October",                                    ~
               at (17,20), fac(lfac$(13)), month$(10,1)         , ch(10),~
               at (17,32), fac(lfac$(13)), month$(10,3)         , ch(10),~
               at (17,49), fac(lfac$(13)), month$(10,2)         , ch(10),~
               at (17,61), fac(lfac$(13)), month$(10,4)         , ch(10),~
                                                                         ~
               at (18,04), "November",                                   ~
               at (18,20), fac(lfac$(14)), month$(11,1)         , ch(10),~
               at (18,32), fac(lfac$(14)), month$(11,3)         , ch(10),~
               at (18,49), fac(lfac$(14)), month$(11,2)         , ch(10),~
               at (18,61), fac(lfac$(14)), month$(11,4)         , ch(10),~
                                                                         ~
               at (19,04), "December",                                   ~
               at (19,20), fac(lfac$(15)), month$(12,1)         , ch(10),~
               at (19,32), fac(lfac$(15)), month$(12,3)         , ch(10),~
               at (19,49), fac(lfac$(15)), month$(12,2)         , ch(10),~
               at (19,61), fac(lfac$(15)), month$(12,4)         , ch(10),~
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
                  call "MANUAL" ("HNYHSTG")
                  goto L40330

L41450:        if keyhit% <> 15 then L41490
                  call "PRNTSCRN"
                  goto L40330

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
              line2$ = line2$ & "   Year: " & year$
              str(line2$,62%) = " HNYHSTG: " & str(cms2v$,,8%)
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
               at (05,04), "Last Month in which",                        ~
               at (06,04), "Part Had Activity of:",                      ~
                                                                         ~
               at (08,02), "Direct Addition",                            ~
               at (08,29), fac(lfac$( 1)), bytype$(1)           , ch(02),~
               at (08,32), fac(hex(8c)),   bydescr$(1)          , ch(09),~
                                                                         ~
               at (09,02), "Inter-store Movement",                       ~
               at (09,29), fac(lfac$( 2)), bytype$(2)           , ch(02),~
               at (09,32), fac(hex(8c)),   bydescr$(2)          , ch(09),~
                                                                         ~
               at (10,02), "Cycle Count Adjustment",                     ~
               at (10,29), fac(lfac$( 3)), bytype$(3)           , ch(02),~
               at (10,32), fac(hex(8c)),   bydescr$(3)          , ch(09),~
                                                                         ~
               at (11,02), "Physical Adjustment",                        ~
               at (11,29), fac(lfac$( 4)), bytype$(4)           , ch(02),~
               at (11,32), fac(hex(8c)),   bydescr$(4)          , ch(09),~
                                                                         ~
               at (12,02), "Direct Withdrawal",                          ~
               at (12,29), fac(lfac$( 5)), bytype$(5)           , ch(02),~
               at (12,32), fac(hex(8c)),   bydescr$(5)          , ch(09),~
                                                                         ~
               at (13,02), "Reported as a Byproduct",                    ~
               at (13,29), fac(lfac$( 6)), bytype$(6)           , ch(02),~
               at (13,32), fac(hex(8c)),   bydescr$(6)          , ch(09),~
                                                                         ~
               at (14,02), "Produced by a Job",                          ~
               at (14,29), fac(lfac$( 7)), bytype$(7)           , ch(02),~
               at (14,32), fac(hex(8c)),   bydescr$(7)          , ch(09),~
                                                                         ~
               at (15,02), "Kitted (issued) To a Job",                   ~
               at (15,29), fac(lfac$( 8)), bytype$(8)           , ch(02),~
               at (15,32), fac(hex(8c)),   bydescr$(8)          , ch(09),~
                                                                         ~
               at (16,02), "Scrapped From A Job",                        ~
               at (16,29), fac(lfac$( 9)), bytype$(9)           , ch(02),~
               at (16,32), fac(hex(8c)),   bydescr$(9)          , ch(09),~
                                                                         ~
               at (17,02), "Reworked From QC",                           ~
               at (17,29), fac(lfac$(10)), bytype$(10)          , ch(02),~
               at (17,32), fac(hex(8c)),   bydescr$(10)         , ch(09),~
                                                                         ~
               at (18,02), "Received on a P.O.",                         ~
               at (18,29), fac(lfac$(11)), bytype$(11)          , ch(02),~
               at (18,32), fac(hex(8c)),   bydescr$(11)         , ch(09),~
                                                                         ~
               at (19,02), "Received from QC",                           ~
               at (19,29), fac(lfac$(12)), bytype$(12)          , ch(02),~
               at (19,32), fac(hex(8c)),   bydescr$(12)         , ch(09),~
                                                                         ~
               at (08,42), "Shipped via A/R (sold)",                     ~
               at (08,69), fac(lfac$(13)), bytype$(13)          , ch(02),~
               at (08,72), fac(hex(8c)),   bydescr$(13)         , ch(09),~
                                                                         ~
               at (09,42), "Added Through A/P (no PO)",                  ~
               at (09,69), fac(lfac$(14)), bytype$(14)          , ch(02),~
               at (09,72), fac(hex(8c)),   bydescr$(14)         , ch(09),~
                                                                         ~
               at (10,42), "Moved to a Project",                         ~
               at (10,69), fac(lfac$(15)), bytype$(15)          , ch(02),~
               at (10,72), fac(hex(8c)),   bydescr$(15)         , ch(09),~
                                                                         ~
               at (11,42), "Moved to/from a Rework Job",                 ~
               at (11,69), fac(lfac$(16)), bytype$(16)          , ch(02),~
               at (11,72), fac(hex(8c)),   bydescr$(16)         , ch(09),~
                                                                         ~
               at (12,42), "Part to Part Conversion",                    ~
               at (12,69), fac(lfac$(17)), bytype$(17)          , ch(02),~
               at (12,72), fac(hex(8c)),   bydescr$(17)         , ch(09),~
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
                  call "MANUAL" ("HNYHSTG")
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
            on fieldnr% gosub L50260,         /* Part Number       */     ~
                              L50300,         /* Store Number      */     ~
                              L50340,         /* Year              */     ~
                              L50430,         /* January           */     ~
                              L50430,         /* February          */     ~
                              L50430,         /* March             */     ~
                              L50430,         /* April             */     ~
                              L50430,         /* May               */     ~
                              L50430,         /* June              */     ~
                              L50430,         /* July              */     ~
                              L50430,         /* August            */     ~
                              L50430,         /* September         */     ~
                              L50430,         /* October           */     ~
                              L50430,         /* November          */     ~
                              L50430,         /* December          */     ~
                              L50540          /* Last Activity Dates*/
                return

L50260:     REM Test for Part Number
                call "GETCODE" (#1, part$, partdescr$, 1%, 0, f1%(1))
                return

L50300:     REM Test for Store Number
                call "GETCODE" (#2, store$, storedescr$, 1%, 0, f1%(2))
                return

L50340:     REM Test for Year
                tdate$ = date
                call "DATEFMT" (tdate$,0%,udate$)
                convert str(udate$,1%,4%) to top
                call "NUMTEST" (year$, 1900, top, errormsg$, 0.0, 0)
                     if errormsg$ <> " " then return
                gosub L30000
                if f1%(3) = 0 then return
                return clear all
                goto editmode

L50430:     REM Test for January - December
                i% = fieldnr% - 3%
                call "NUMTEST" (month$(i%,1), -9e7, 9e7,errormsg$,-0.2,0)
                     if errormsg$ <> " " then return
                call "NUMTEST" (month$(i%,3), -9e7, 9e7,errormsg$,-2.2,0)
                     if errormsg$ <> " " then return
                call "NUMTEST" (month$(i%,2), -9e7, 9e7,errormsg$,-0.2,0)
                     if errormsg$ <> " " then return
                call "NUMTEST" (month$(i%,4), -9e7, 9e7,errormsg$,-2.2,0)
                return

L50540:     REM Test for Last Activity Dates
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
                bydescr$(fieldnr%) = " "
                call "NUMTEST" (bytype$(fieldnr%),0,12,errormsg$,-.001,x)
                     if errormsg$ <> " " then return
                if x = 0 then return
                bydescr$(fieldnr%) = str(names$(x),1,9)
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
