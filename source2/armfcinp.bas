        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   M   M  FFFFF   CCC   IIIII  N   N  PPPP    *~
            *  A   A  R   R  MM MM  F      C        I    NN  N  P   P   *~
            *  AAAAA  RRRR   M M M  FFFF   C        I    N N N  PPPP    *~
            *  A   A  R R    M   M  F      C        I    N  NN  P       *~
            *  A   A  R  R   M   M  F       CCC   IIIII  N   N  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARMFCINP - INPUT AND EDIT FINANCE CHARGE TABLES           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/18/83 ! ORIGINAL (FINTABLE)                      ! KEN *~
            * 05/28/86 ! Name changed and screens standardized    ! ERN *~
            * 07/26/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            base(12),                    /* Base                       */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            compmsg$79,                  /* Computation Message        */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit Screen Message        */~
            errormsg$79,                 /* Error message              */~
            fac$(20,4)1,                 /* Table FACs                 */~
            fccode$2,                    /* Finance Charge Code        */~
            fccodedescr$30,              /* Description                */~
            fcdate$8,                    /* Effective Date             */~
            fcfc$1,                      /* Finance on Finance         */~
            header$(4)10,                /* Header for Table           */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Input Message              */~
            invbal$1,                    /* Invoice or Balance         */~
            grace$3,                     /* Assessment Level           */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Screen Line         */~
            maximum(12),                 /* Maximum                    */~
            minimum(12),                 /* Minimum                    */~
            percent(12),                 /* Percents                   */~
            plowkey$21                   /* Plowcode argument          */

        dim f2%(64),                     /* File Status Flags for      */~
            f1%(64)                      /* Record-on-file Flags       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con :
            cms2v$ = "04.17.01 11/07/86 Order processing & planning     "

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! System Secrets File                      *~
            *************************************************************

            select  #1, "SYSFILE2",                                      ~
                        varc, indexed,                                   ~
                        recsize = 500,                                   ~
                        keypos  = 1, keylen = 20

            call "OPENCHCK" (#1, 0%, f2%(1), 0%, " ")


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" ( blankdate$ )

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor" &~
                          " to Desired Value And Press (ENTER)."

            compmsg$ = "FC = BASE + ((BALANCE - MINIMUM) * PERCENT) /" & ~
                       " PERIODS-PER-YEAR"

            header$(1) = "   MINIMUM"
            header$(2) = "      BASE"
            header$(3) = "   PERCENT"
            header$(4) = "   MAXIMUM"

            str(line2$,62) = "ARMFCINP: " & str(cms2v$,,8)


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, fccode$, fccodedescr$,     ~
                      fcdate$, fcfc$, invbal$, grace$
            apercent, minamount = 0
            ppyear%  = 12
            bracket% = 1%
            edit%    = 0%
            mat minimum = zer
            mat maximum = zer
            mat base    = zer
            mat percent = zer

            for fieldnr% = 1% to 8%
                gosub'051(fieldnr%)
                      if enabled% = 0% then L10270
L10210:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10210
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10210
L10270:         next fieldnr%

            for fieldnr% = 1% to 2%
                gosub'052(fieldnr%)
                      if enabled% =  0 then L10370
L10320:         gosub'102(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  0 then       L10320
                gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10320
L10370:         next fieldnr%

            for line% = 1% to bracket%
                for fieldnr% = 1% to 4%
                     gosub'053 (fieldnr%)
                          if enabled% =   0 then L10480
L10430:              gosub'103 (line%,fieldnr%)
                          if keyhit%  =   1 then gosub startover
                          if keyhit% <>   0 then       L10430
                     gosub'153 (line%,fieldnr%)
                          if errormsg$ <> " " then L10430
L10480:         next fieldnr%
            next line%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * Handles operation of edit mode.                           *~
            *************************************************************
            edit% = 1%
        edtpg1
L11070:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  5 then       edtpg2
                  if keyhit%  = 12 then gosub delete_table
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11070
L11120:     fieldnr% = cursor%(1) - 5%
            if fieldnr% < 2% or fieldnr% > 8% then L11070
            if fieldnr% = 5% then L11070

            gosub'051(fieldnr%)
L11160:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11160
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11160
                  if fieldnr%   = cursor%(1) - 5% then L11070 else L11120

        edtpg2
L11240:     gosub'112(0%)                /* Get which item(s) to edit  */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  4 then       edtpg1
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11240
            fieldnr% = cursor%(1) - 5%
            if fieldnr% < 1% or fieldnr% = 2% then L11240
            if fieldnr% > 2% then L11410         /* Edit Bracket line   */

            gosub'052(2%)                /* Edit number of brackets    */
L11340:     gosub'112(2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11340
            gosub'152(2%)
                  if errormsg$ <> " " then L11340
            goto L11240

L11410:     fieldnr% = fieldnr% - 2%     /* Edit Bracket Line          */
            if fieldnr% > bracket% then L11240
            line%    = fieldnr%
            fieldnr% = int(cursor%(2)/16)
            if fieldnr% = 0% then L11540  /* Do entire line             */
            gosub'053(fieldnr%)          /* Do a specific field        */
L11470:     gosub'113(line%, fieldnr%)
                if keyhit%  =   1 then gosub startover
                if keyhit% <>   0 then L11470
            gosub'153(line%, fieldnr%)
                if errormsg$ <> " " then L11470
            goto L11240

L11540:     for fieldnr% = 1% to 4%
                gosub'053(fieldnr%)
L11560:         gosub'113(line%, fieldnr%)
                     if keyhit%  =   1 then gosub startover
                     if keyhit% <>   0 then L11560
                gosub'153(line%, fieldnr%)
                     if errormsg$ <> " " then L11560
            next fieldnr%
            goto L11240


        delete_table
            keyhit1% = 2%
            call "ASKUSER" (keyhit1%, "D E L E T E",                     ~
                            "Enter PF-16 to DELETE Finance Charge Table",~
                            "- OR -", "Press RETURN to CANCEL Delete.")
            if keyhit1% <> 16% then return
                plowkey$ = "FINANCECHARGETABLE" & fccode$
                call "DELETE" (#1, plowkey$, 20%)
                return clear all
                goto inputmode


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * --------------------------------------------------------- *~
            * Sets DEFAULTS and ENABLES Fields for the Page 1 of input. *~
            *************************************************************

            deffn'051(fieldnr%)
                enabled% = 1%
                  on fieldnr% gosub L20100,         /* Fin Chg Code     */~
                                    L20200,         /* Description      */~
                                    L20250,         /* Effective Date   */~
                                    L20300,         /* Annual Percent   */~
                                    L20400,         /* Periods per Year */~
                                    L20500,         /* Minimum Charge   */~
                                    L20600,         /* Assessment Level */~
                                    L20700          /* Fin on Fin       */
                     return

L20100
*        Default/Enable for FINANCE CHARGE CODE
            inpmessage$ = "Enter Table ID ('0'-'9' or '0*'-'9*')."
            return

L20200
*        Default/Enable for DESCRIPTION
            inpmessage$ = "Enter Description of Finance Charge Table."
            return

L20250
*        Default/Enable FOR EFFECTIVE DATE
            inpmessage$ = "Enter Date that table becomes effective."
            if fcdate$ = " " or fcdate$ = blankdate$ then fcdate$ = date$
            return

L20300
*        Default/Enable for ANNUAL PERCENT
            inpmessage$ = "Enter Annual Percent Rate."
            return

L20400
*        Default/Enable for PERIODS PER YEAR
            ppyear%  = 12%
            enabled% =  0%
            return

L20500
*        Default/Enable for MINIMUM CHARGE
            inpmessage$ = "Enter Minimum Finance Charge amount."
            return

L20600
*        Default/Enable for ASSESSMENT GRACE
            inpmessage$ = "Enter aging days after which finance charges"&~
                          " are charged"
            if grace$ = " " then grace$ = "30"
            return

L20700
*        Default/Enable for FINANCE ON FINANCE
            inpmessage$ = "Charge Finance Charges on Finance Charges?" & ~
                          " (Y/N)."
            if fcfc$ = " " then fcfc$ = "N"
            return



        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            * --------------------------------------------------------- *~
            * Sets DEFAULTS and ENABLES Fields for the page 2 of input. *~
            *************************************************************

            deffn'052(fieldnr%)
                enabled% = 1%
                  on fieldnr% gosub L21100,         /* CODE             */~
                                    L21200          /* BRACKETS         */
                     return

L21100
*        Default/Enable for CODES
            enabled% = 0%
            return

L21200
*        Default/Enable for BRACKETS
            inpmessage$ = "Enter number of brackets to appear in table."
            if bracket% = 0% then bracket% = 1
            return


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            * --------------------------------------------------------- *~
            *                TABLE SECTION OF SCREEN                    *~
            * Sets DEFAULTS and ENABLES Fields for the page 2 of input. *~
            *************************************************************

            deffn'053(fieldnr%)
                enabled% = 1%
                  on fieldnr% gosub L22150,         /* Minimum          */~
                                    L22190,         /* Base             */~
                                    L22230,         /* Percent          */~
                                    L22270          /* Maximum          */
                     return

L22150
*        Default/Enable for MINIMUM
            inpmessage$ = "Enter Minimum amount for this bracket."
            return

L22190
*        Default/Enable for BASE
            inpmessage$ = "Enter Base Charge Amount for this bracket."
            return

L22230
*        Default/Enable for PERCENT
            inpmessage$ = "Enter Annual Percentage Rate to be applied."
            return

L22270
*        Default/Enable for MAXIMUM
            inpmessage$ = "Enter Maximum amount for this bracket."
            return


        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover
L29918:     keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1%  = 1% then return
            if keyhit1% <> 0% then L29918
                return clear all
                goto inputmode


L30000: REM *************************************************************~
            *                  L O A D   D A T A                        *~
            * --------------------------------------------------------- *~
            * Load existing record from file.                           *~
            *************************************************************

            get #1 using L30190, fccodedescr$, fcdate$, apercent,         ~
                              ppyear%, minamount, grace$, fcfc$, invbal$,~
                              bracket%, minimum(), base(), percent(),    ~
                              maximum()

            call "DATEFMT" (fcdate$)

            if bracket% = 12% then L30170
                for i% = bracket% + 1%  to  12%
                    minimum(i%), base(i%), percent(i%), maximum(i%) = 0
                next i%
L30170:     goto edtpg1

L30190:     FMT      XX(20),             /* 'FINANCECHARGETABLE' & Code*/~
                     CH(30),             /* Description                */~
                     CH(6),              /* Effective Date             */~
                     PD(14,4),           /* Annual Percent             */~
                     BI(4),              /* Periods Per Year           */~
                     PD(14,4),           /* Minimum Charge             */~
                     CH(3),              /* Assessment Level           */~
                     CH(1),              /* Finance On Finance         */~
                     CH(1),              /* Not Used                   */~
                     BI(4),              /* Brackets                   */~
                     12*PD(14,4),        /* Minimums                   */~
                     12*PD(14,4),        /* Base Amounts               */~
                     12*PD(14,4),        /* Percents                   */~
                     12*PD(14,4),        /* Maximums                   */~
                     XX(31)              /* FILLER                     */

        REM *************************************************************~
            *                  S A V E   D A T A                        *~
            * --------------------------------------------------------- *~
            * Write data to file.                                       *~
            *************************************************************

        datasave
            call "READ101" (#1, "FINANCECHARGETABLE" & fccode$, f1%(1))
            call "DATUNFMT" (fcdate$)
            put #1 using L31170, "FINANCECHARGETABLE" & fccode$,          ~
                                fccodedescr$, fcdate$, apercent, ppyear%,~
                                minamount, grace$, fcfc$, " ",           ~
                                bracket%, minimum(), base(), percent(),  ~
                                maximum()," "
            if f1%(1) = 1% then rewrite #1 else write #1
            goto inputmode

L31170:     FMT      CH(20),             /* 'FINANCECHARGETABLE' & Code*/~
                     CH(30),             /* Description                */~
                     CH(6),              /* Effective Date             */~
                     PD(14,4),           /* Annual Percent             */~
                     BI(4),              /* Periods per Year           */~
                     PD(14,4),           /* Minimum Charge             */~
                     CH(3),              /* Assessment Level           */~
                     CH(1),              /* Finance on Finance         */~
                     CH(1),              /* Not Used                   */~
                     BI(4),              /* Brackets                   */~
                     12*PD(14,4),        /* Minimums                   */~
                     12*PD(14,4),        /* Base Amounts               */~
                     12*PD(14,4),        /* Percents                   */~
                     12*PD(14,4),        /* Maximums                   */~
                     CH(31)              /* FILLER                     */

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            * --------------------------------------------------------- *~
            * Inputs Document for First Time.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(8c)) lfac$()
                  init(hex(9c)) str(lfac$(), fieldnr%+1%)

                  on fieldnr% gosub L40240,         /* Table ID         */~
                                    L40210,         /* Description      */~
                                    L40240,         /* Effective Date   */~
                                    L40270,         /* Annual Percent   */~
                                    L40270,         /* Periods Per Year */~
                                    L40270,         /* Minimum Charge   */~
                                    L40270,         /* Assessment Level */~
                                    L40240          /* Fin ON Fin       */
                     goto L40310

L40210:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40240:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40270:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40310: accept                                                           ~
            at (01,02), "Finance Charge Tables  (Input Mode)",           ~
            at (01,66), "Today: ",                                       ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (04,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (06,02), "Finance Charge Table",                          ~
            at (06,30), fac(lfac$( 1)), fccode$                 , ch(02),~
            at (07,02), "Table Description",                             ~
            at (07,30), fac(lfac$( 2)), fccodedescr$            , ch(30),~
            at (08,02), "Table Effective Date",                          ~
            at (08,30), fac(lfac$( 3)), fcdate$                 , ch(08),~
            at (09,02), "Annual Percentage Rate",                        ~
            at (09,30), fac(lfac$( 4)), apercent             ,pic(##.##),~
            at (10,02), "Periods Per Year",                              ~
            at (10,30), fac(lfac$( 5)), ppyear%                 ,pic(##),~
            at (11,02), "Minimum Finance Charge",                        ~
            at (11,30), fac(lfac$( 6)), minamount            ,pic(##.##),~
            at (12,02), "Grace Days Allowed",                            ~
            at (12,30), fac(lfac$( 7)), grace$                  , ch(03),~
            at (13,02), "Finance On Finance?",                           ~
            at (13,30), fac(lfac$( 8)), fcfc$                   , ch(01),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), "(1)Start Over",                                 ~
            at (22,65), "(13)Instructions",                              ~
            at (23,65), "(15)Print Screen",                              ~
            at (24,65), "(16)Exit Program",                              ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L40690
                  call "MANUAL" ("ARMFCINP")
                  goto L40310

L40690:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40310

        REM *************************************************************~
            *      I N P U T / E D I T   S C R E E N   P A G E   2      *~
            * --------------------------------------------------------- *~
            * Input and Edit Modes for second screen.                   *~
            *************************************************************

            deffn'102(fieldnr%)          /* Input Mode - Header        */
                screen% = 0%
                init(hex(84)) lfac$()
                init(hex(9c)) fac$()
                on fieldnr%  gosub  L41135,         /* Table ID (Code)  */~
                                    L41150          /* # of Brackets    */
                goto L41320

            deffn'112(fieldnr%)          /* Edit Mode - Header         */
                screen% = 1%
                init(hex(84)) lfac$()
                if fieldnr% = 0% then init(hex(86)) lfac$(), fac$()
                if fieldnr% = 0% then inpmessage$ = edtmessage$
                if errormsg$ <> " " then L41100
                init(hex(84)) lfac$(1), fac$()
                init(hex(9c)) str(fac$(), 4*min(bracket%,12%)+1%)
L41100:         on fieldnr%  gosub  L41135,         /* Table ID         */~
                                    L41150          /* # of Brackets    */
                goto L41320

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41135:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L41150:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

            deffn'103(line%, fieldnr%)             /* Input Lines      */
                screen% = 0%
                init(hex(8c)) lfac$(), fac$()
                init(hex(9c)) str(fac$(), 4*min(bracket%,12%)+1%)
                on fieldnr%  gosub  L41300,         /* Minimum          */~
                                    L41300,         /* Base             */~
                                    L41300,         /* Percent          */~
                                    L41300          /* Maximum          */
                goto L41320

            deffn'113(line%, fieldnr%)             /* Edit Lines       */
                  screen% = 0%
                  init(hex(8c)) lfac$(), fac$()
                  init (hex(9c)) str(fac$(), 4*min(bracket%,12%)+1%)
                  on fieldnr% gosub L41300,         /* Minimum          */~
                                    L41300,         /* Base             */~
                                    L41300,         /* Percent          */~
                                    L41300          /* Maximum          */
                     goto L41320

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$(line%,fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(line%,fieldnr%) = hex(81)
                      return
L41300:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      fac$(line%,fieldnr%) = hex(82)
                      return

L41320: accept                                                           ~
            at (01,02), "Finance Charge Tables",                         ~
            at (01,66), "Today: ",                                       ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (03,02), "Table:",                                        ~
            at (03,09), fac(lfac$(1)), fccode$                  , ch(02),~
            at (03,12), fac(hex(8c)) , fccodedescr$             , ch(30),~
            at (04,12), fac(hex(8c)) , compmsg$                 , ch(65),~
            at (05,02), fac(hex(94)) , errormsg$                , ch(79),~
                                                                         ~
            at (06,02), "Number of Brackets:",                           ~
            at (06,22), fac(lfac$(2)), bracket%                , pic(##),~
                                                                         ~
            at (07,20), fac(hex(ac)) , header$(1)               , ch(10),~
            at (07,36), fac(hex(ac)) , header$(2)               , ch(10),~
            at (07,52), fac(hex(ac)) , header$(3)               , ch(10),~
            at (07,68), fac(hex(ac)) , header$(4)               , ch(10),~
            at (08,02), "Bracket  1",                                    ~
            at (09,02), "         2",                                    ~
            at (10,02), "         3",                                    ~
            at (11,02), "         4",                                    ~
            at (12,02), "         5",                                    ~
            at (13,02), "         6",                                    ~
            at (14,02), "         7",                                    ~
            at (15,02), "         8",                                    ~
            at (16,02), "         9",                                    ~
            at (17,02), "        10",                                    ~
            at (18,02), "        11",                                    ~
            at (19,02), "        12",                                    ~
                                                                         ~
            at (08,20), fac(fac$( 1,1)), minimum( 1)    ,pic(-######.##),~
            at (09,20), fac(fac$( 2,1)), minimum( 2)    ,pic(-######.##),~
            at (10,20), fac(fac$( 3,1)), minimum( 3)    ,pic(-######.##),~
            at (11,20), fac(fac$( 4,1)), minimum( 4)    ,pic(-######.##),~
            at (12,20), fac(fac$( 5,1)), minimum( 5)    ,pic(-######.##),~
            at (13,20), fac(fac$( 6,1)), minimum( 6)    ,pic(-######.##),~
            at (14,20), fac(fac$( 7,1)), minimum( 7)    ,pic(-######.##),~
            at (15,20), fac(fac$( 8,1)), minimum( 8)    ,pic(-######.##),~
            at (16,20), fac(fac$( 9,1)), minimum( 9)    ,pic(-######.##),~
            at (17,20), fac(fac$(10,1)), minimum(10)    ,pic(-######.##),~
            at (18,20), fac(fac$(11,1)), minimum(11)    ,pic(-######.##),~
            at (19,20), fac(fac$(12,1)), minimum(12)    ,pic(-######.##),~
                                                                         ~
            at (08,36), fac(fac$( 1,2)), base   ( 1)    ,pic(-######.##),~
            at (09,36), fac(fac$( 2,2)), base   ( 2)    ,pic(-######.##),~
            at (10,36), fac(fac$( 3,2)), base   ( 3)    ,pic(-######.##),~
            at (11,36), fac(fac$( 4,2)), base   ( 4)    ,pic(-######.##),~
            at (12,36), fac(fac$( 5,2)), base   ( 5)    ,pic(-######.##),~
            at (13,36), fac(fac$( 6,2)), base   ( 6)    ,pic(-######.##),~
            at (14,36), fac(fac$( 7,2)), base   ( 7)    ,pic(-######.##),~
            at (15,36), fac(fac$( 8,2)), base   ( 8)    ,pic(-######.##),~
            at (16,36), fac(fac$( 9,2)), base   ( 9)    ,pic(-######.##),~
            at (17,36), fac(fac$(10,2)), base   (10)    ,pic(-######.##),~
            at (18,36), fac(fac$(11,2)), base   (11)    ,pic(-######.##),~
            at (19,36), fac(fac$(12,2)), base   (12)    ,pic(-######.##),~
                                                                         ~
            at (08,52), fac(fac$( 1,3)), percent( 1)    ,pic(-######.##),~
            at (09,52), fac(fac$( 2,3)), percent( 2)    ,pic(-######.##),~
            at (10,52), fac(fac$( 3,3)), percent( 3)    ,pic(-######.##),~
            at (11,52), fac(fac$( 4,3)), percent( 4)    ,pic(-######.##),~
            at (12,52), fac(fac$( 5,3)), percent( 5)    ,pic(-######.##),~
            at (13,52), fac(fac$( 6,3)), percent( 6)    ,pic(-######.##),~
            at (14,52), fac(fac$( 7,3)), percent( 7)    ,pic(-######.##),~
            at (15,52), fac(fac$( 8,3)), percent( 8)    ,pic(-######.##),~
            at (16,52), fac(fac$( 9,3)), percent( 9)    ,pic(-######.##),~
            at (17,52), fac(fac$(10,3)), percent(10)    ,pic(-######.##),~
            at (18,52), fac(fac$(11,3)), percent(11)    ,pic(-######.##),~
            at (19,52), fac(fac$(12,3)), percent(12)    ,pic(-######.##),~
                                                                         ~
            at (08,68), fac(fac$( 1,4)), maximum( 1)    ,pic(-######.##),~
            at (09,68), fac(fac$( 2,4)), maximum( 2)    ,pic(-######.##),~
            at (10,68), fac(fac$( 3,4)), maximum( 3)    ,pic(-######.##),~
            at (11,68), fac(fac$( 4,4)), maximum( 4)    ,pic(-######.##),~
            at (12,68), fac(fac$( 5,4)), maximum( 5)    ,pic(-######.##),~
            at (13,68), fac(fac$( 6,4)), maximum( 6)    ,pic(-######.##),~
            at (14,68), fac(fac$( 7,4)), maximum( 7)    ,pic(-######.##),~
            at (15,68), fac(fac$( 8,4)), maximum( 8)    ,pic(-######.##),~
            at (16,68), fac(fac$( 9,4)), maximum( 9)    ,pic(-######.##),~
            at (17,68), fac(fac$(10,4)), maximum(10)    ,pic(-######.##),~
            at (18,68), fac(fac$(11,4)), maximum(11)    ,pic(-######.##),~
            at (19,68), fac(fac$(12,4)), maximum(12)    ,pic(-######.##),~
                                                                         ~
            at (21,02), fac(hex(a4)) ,  inpmessage$             , ch(79),~
            at (22,02), "(1)Start Over",                                 ~
            at (23,20), "(4)Previous Page",                              ~
            at (22,65), "(13)Instructions",                              ~
            at (23,65), "(15)Print Screen",                              ~
            at (24,65), "(16)Save Data   ",                              ~
               keys(hex(0001040d0f10)), key (keyhit%)

               if keyhit% <> 13 then L41790
                  call "MANUAL" ("ARMFCINP")
                  goto L41320

L41790:        if keyhit% <> 15 then L41810
                  call "PRNTSCRN"
                  goto L41320

L41810:        if screen% = 0 then return
                  close ws
                  call "SCREEN" addr("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            * --------------------------------------------------------- *~
            * Screen for EDITING Page 1 of Document.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(8c)) lfac$()
                  if fieldnr% = 0% then init(hex(86)) lfac$()
                  if fieldnr% = 0% then inpmessage$ = edtmessage$
                  on fieldnr% gosub L42240,         /* Table ID (Code)  */~
                                    L42210,         /* Description      */~
                                    L42240,         /* Effective Date   */~
                                    L42270,         /* Annual Percent   */~
                                    L42270,         /* Periods Per Year */~
                                    L42270,         /* Minimum Charge   */~
                                    L42270,         /* Assessment Level */~
                                    L42240          /* Fin on Fin       */
                     goto L42310

L42210:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L42240:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L42270:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L42310: accept                                                           ~
            at (01,02), "Finance Charge Tables  (Edit Mode)",            ~
            at (01,66), "Today: ",                                       ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (04,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (06,02), "Finance Charge Table",                          ~
            at (06,30), fac(lfac$( 1)), fccode$                 , ch(02),~
            at (07,02), "Table Description",                             ~
            at (07,30), fac(lfac$( 2)), fccodedescr$            , ch(30),~
            at (08,02), "Table Effective Date",                          ~
            at (08,30), fac(lfac$( 3)), fcdate$                 , ch(08),~
            at (09,02), "Annual Percentage Rate",                        ~
            at (09,30), fac(lfac$( 4)), apercent             ,pic(##.##),~
            at (10,02), "Periods Per Year",                              ~
            at (10,30), fac(lfac$( 5)), ppyear%                 ,pic(##),~
            at (11,02), "Minimum Finance Charge",                        ~
            at (11,30), fac(lfac$( 6)), minamount            ,pic(##.##),~
            at (12,02), "Grace Days Allowed",                            ~
            at (12,30), fac(lfac$( 7)), grace$                  , ch(03),~
            at (13,02), "Finance On Finance?",                           ~
            at (13,30), fac(lfac$( 8)), fcfc$                   , ch(01),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), "(1)Start Over",                                 ~
            at (22,65), "(13)Instructions",                              ~
            at (23,20), "(5)Next Screen",                                ~
            at (23,45), "(12)Delete Table",                              ~
            at (23,65), "(15)Print Screen",                              ~
            at (24,65), "(16)Save Data   ",                              ~
               keys(hex(0001050c0d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L42700
                  call "MANUAL" ("ARMFCINP")
                  goto L42310

L42700:        if keyhit% <> 15 then L42740
                  call "PRNTSCRN"
                  goto L42310

L42740:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* Table ID (Code)  */~
                                    L50200,         /* Description      */~
                                    L50250,         /* Effective Date   */~
                                    L50300,         /* Annual Percent   */~
                                    L50400,         /* Periods Per Year */~
                                    L50500,         /* Minimum Charge   */~
                                    L50600,         /* Assessment Level */~
                                    L50700          /* Fin on Fin       */
                     return

L50100
*        Test Data for FINANCE CHARGE CODE
            if fccode$ = " " then L50135
            if str(fccode$,1,1) < "0" or str(fccode$,1,1) > "9" then L50120
            if str(fccode$,2,1) = " " or str(fccode$,2,1) = "*" then L50130
L50120:         errormsg$ = "TABLE ID MUST BE '0'-'9' OR '0*'-'9*'"
                return
L50130:     if edit% = 1% then return
L50135:     plowkey$ = "FINANCECHARGETABLE" & fccode$
            call "PLOWCODE" (#1, plowkey$, " ", 18%, 0.30, f1%(1))
            fccode$    = str(plowkey$,19)
            if fccode$ = " " then errormsg$ = hex(00)
            if f1%(1)  = 0% then return
                return clear all
                goto L30000

L50200
*        Test Data for DESCRIPTION
            return

L50250
*        Test EFFECTIVE DATE
            call "DATEOK" (fcdate$, u3%, errormsg$)
            return

L50300
*        Test Data for ANNUAL PERCENT
            apercent = round(apercent, 2)
            if apercent >= 100 then L50340
            if apercent >= 0   then return
                errormsg$ = "NEGATIVE RATE NOT ALLOWED" : return
L50340:         errormsg$ = "RATE CANNOT EXCEED 99.99%" : return

L50400
*        Test Data for PERIODS PER YEAR
            ppyear% = 12%
            return

L50500
*        Test Data for MINIMUM CHARGE
            minamount = round (minamount,2)
            if minamount >= 0 and minamount <= 99.99 then return
                errormsg$ = "MINIMUM MUST BE 0 TO 99.99"
                return

L50600
*        Test Data for ASSESSMENT GRACE
            convert grace$ to grace%, data goto L50620 : goto L50630
L50620:         errormsg$ = "MUST 0 - 999" : return
L50630:     if grace% < 0% or grace% > 999% then L50620
            convert grace% to grace$, pic(##0)
            return

L50700
*        Test Data for FINANCE ON FINANCE
            if fcfc$ = "Y" or fcfc$ = "N" then return
                errormsg$ = "MUST BE 'Y' OR 'N'"
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Tests Data for the items on Page 2 (brackets entry hdr).  *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51100,         /* CODE             */~
                                    L51200          /* BRACKETS         */
                     return

L51100
*        Test Data for CODE (better never get here)
            return

L51200
*        Test Data for BRACKETS
            if bracket% >= 1% and bracket% <= 12% then L51240
                errormsg$ = "MUST HAVE 1 TO 12 BRACKETS"
                return
L51240:     if bracket% > 11% then return
                for i% = bracket%+1% to 12%
                     minimum(i%), base(i%), percent(i%), maximum(i%) = 0
                next i%
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Tests Data for Bracket entry on page 2.                   *~
            *************************************************************

            deffn'153(line%,fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L52200,         /* Minimum          */~
                                    L52300,         /* Base             */~
                                    L52400,         /* Percent          */~
                                    L52500          /* Maximum          */
                     return

L52200
*        Test MINIMUM
            minimum(line%) = round(minimum(line%), 2)
            if minimum(line%) >= 0 and minimum(line%) < 1000000 then     ~
                                                                  return
                errormsg$ = "MINIMUM MUST BE 0 - 999999.99"
                return

L52300
*        Test BASE
            base(line%) = round(base(line%), 2)
            if base(line%) >= 0 and base(line%) < 1000000 then return
                errormsg$ = "BASE MUST BE 0 - 999999.99"
                return

L52400
*        Test PERCENT
            percent(line%) = round(percent(line%), 2)
            if percent(line%) >= 0 and percent(line%) < 100 then return
                errormsg$ = "PERCENT MUST BE 0 - 99.99"
                return

L52500
*        Test MAXIMUM
            if line% = bracket% then maximum(line%) = 999999.99  else    ~
                                maximum(line%) = round(maximum(line%), 2)
            if maximum(line%) >= 0 and percent(line%) < 10000000         ~
                                                             then return
                errormsg$ = "MAXIMUM MUST BE 0 - 999999.99"
                return

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("ONE MOMENT PLEASE")

            end
