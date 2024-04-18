        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  RRRR    OOO   PPPP    SSS   Y   Y   SSS   IIIII  N   N   *~
            *  R   R  O   O  P   P  S      Y   Y  S        I    NN  N   *~
            *  RRRR   O   O  PPPP    SSS    YYY    SSS     I    N N N   *~
            *  R   R  O   O  P          S    Y        S    I    N  NN   *~
            *  R   R   OOO   P       SSS     Y     SSS   IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ROPSYSIN - This program allows entry and management of ROP*~
            *            system parameters                              *~
            *-----------------------------------------------------------*~
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
            * 04/30/87 ! Original                                 ! LKM *~
            * 05/31/89 ! Changed Purchasing and WC fixed cost     ! MJB *~
            *          !  defaults to "1" and test accordingly.   !     *~
            * 08/20/96 ! Modified for Century dates               ! DER *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparision */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            devratio$9,                  /* Deviation Ratio            */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            interest$6,                  /* Yearly Interest Rate       */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            pcent$6,                     /* Minimum Variation Percent  */~
            per_or_yearmon$4,            /* period or year-month BI(4) */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            pf12$17,                     /* PF12 Screen Literal        */~
            purch$10,                    /* Fixed Purchasing Cost      */~
            readkey$20,                  /* Miscellaneous read key     */~
            smco$9,                      /* Smoothing Coefficient      */~
            svyymm$8,                    /* Date Last Calculated       */~
            userid$3,                    /* Current User Id            */~
            wrkcntr$10                   /* Fixed Workcenter Cost      */

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
            * # 1 ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))

            if min(fs%()) < 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" ( date$, date%, svyymm$ )
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "READ101" (#1, "SWITCHS.HNY", f1%(1))
            if f1%(1) <> 1 then L65000
            get #1 using L09150, cal$
L09150:     FMT POS(95), CH(1)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            onfile = 0
            pf4$ = " " : pf5$ = " " : pf16$ = "(16)Exit Program"
            pf12$ = " "
            pfkeys$ = hex(000104050d0f10ffffffff)
            gosub L29000
            gosub dataload
            if f1%(1) = 1 then editpg1

            for fieldnr% = 1 to  6
                if fieldnr% > 1 then pf4$ = "(4)Previous Field"
L10170:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10290
L10190:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10270
L10220:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10190
                         if fieldnr% = 1% then L10170
                         goto L10220
L10270:               if keyhit% = 16 and fieldnr% = 1 then exit_program
                      if keyhit% <>  0 then       L10190
L10290:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10190
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            pf4$  = " "
            pf5$  = " "
            if onfile <> 1 then L11120
               pf12$ = "(12)Delete Record"
               str(pfkeys$,8,1) = hex(0c)
L11120:     inpmessage$ = edtmessage$
            pf16$ = "(16)Save/Exit"
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 12 then       delete_mode
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       editpg1
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  6 then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
                  pf4$, pf5$, pf16$ = " "
L11260:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11260
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11260
            goto L11120

        delete_mode
L11350:     keyhit% = 2%
            call "ASKUSER" (keyhit%, "*** DELETE RECORD? ***", "Press RET~
        ~URN to DELETE This Record", "- OR -","PRESS PF1 TO CANCEL DELETE ~
        ~& Return")
            if keyhit% = 1% then editpg1
            if keyhit% <> 0% then L11350
            delete #1
            goto inputmode

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto exit_program

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Smoothing Coeffic  */    ~
                              L20200,         /* Fixed Purchasing   */    ~
                              L20400,         /* Fixed Workcenter   */    ~
                              L20500,         /* Yearly Interest R  */    ~
                              L20610,         /* Deviation Ratio    */    ~
                              L21005          /* Minimum Variance % */
            return

L20100: REM Def/Enable Smoothing Coefficient       SMCO$
            inpmessage$ = "Enter Smoothing Coefficient (decimal number be~
        ~tween 0 and 1)."
            if smco$ = " " then smco$ = "      .20"
            return

L20200: REM Def/Enable Fixed Purchasing Cost       PURCH$
            if purch$ = " " then purch$ = "1"
            inpmessage$ = "Enter Fixed Purchasing Cost - '1' implies" &  ~
                          " NO Fixed Cost"
            return

L20400: REM Def/Enable Fixed Workcenter Cost       WRKCNTR$
            if wrkcntr$ = " " then wrkcntr$ = "1"
            inpmessage$ = "Enter Fixed Workcenter Cost - '1' implies" &  ~
                          " NO Fixed Cost"
            return

L20500: REM Def/Enable Yearly Interest Rate        INTEREST$
            inpmessage$ = "Enter Yearly Interest Rate (decimal number bet~
        ~ween 0 and 1)."
            if interest$ = " " then interest$ = "   .12"
            return

L20610: REM Def/Enable Yearly Interest Rate        DEVRATIO$
            inpmessage$ = "Enter Maximum Deviation Ratio"
            if devratio$ = " " then devratio$ = "      .80"
            return

L21005: REM Def/Enable Minimum Variation Percent   PCENT$
            inpmessage$ = "Enter Minimum percentage of variance needed to~
        ~ update ROP."
            if pcent$ = " " then pcent$ = "   .10"
            return

L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, readkey$,                  ~
                      smco$                  , /* Smoothing Coeffic  */  ~
                      purch$                 , /* Fixed Purchasing   */  ~
                      wrkcntr$               , /* Fixed Workcenter   */  ~
                      interest$              , /* Yearly Interest R  */  ~
                      devratio$              , /* Deviation Ratio    */  ~
                      pcent$                   /* Minimum Variance % */
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

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            str(readkey$,1,9) = "ROP PARAM"
            call "READ101" (#1, readkey$, f1%(1))
            if f1%(1) <> 1 then return
            get #1, using L30110, smco, purch, wrkcntr, interest,devratio,~
                                 pcent
L30110:         FMT POS(21), PD(14,7), 5*PD(14,4)

            call "CONVERT" (smco, 2.7, smco$)
            x% = pos (smco$ = "0")
            if x% <> 0 then str(smco$,x%,1) = " "
            call "CONVERT" (purch, 4.4, purch$)
            call "CONVERT" (wrkcntr, 4.4, wrkcntr$)
            call "CONVERT" (interest, 2.4, interest$)
            x% = pos (interest$ = "0")
            if x% <> 0% then str(interest$,x%,1) = " "
            call "CONVERT" (devratio, 2.2, devratio$)
            x% = pos (devratio$ = "0")
            if x% <> 0% then str(devratio$,x%,1) = " "
            call "CONVERT" (pcent, 2.2, pcent$)
            x% = pos (pcent$ = "0")
            if x% <> 0% then str(pcent$,x%,1) = " "
            onfile = 1
            goto editpg1

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            if f1%(1) = 1 then L31250
               readkey$ = "ROP PARAM"
               if cal$ = "G" then L31160
                  call "WHICHPER" (#1, date$, per_or_ccyymm%)
                  goto L31170

L31160:     convert str(svyymm$, 1%, 6% ) to per_or_ccyymm%

L31170:     write #1 using L31210, readkey$, smco, purch, wrkcntr,        ~
                                     interest,devratio,pcent," ",         ~
                                     per_or_ccyymm%, 0%, blankdate$, " ",~
                                     " "

L31210:        FMT CH(20), PD(14,7), 5*PD(14,4), CH(25), BI(4), BI(1),    ~
                   CH(6), CH(244), CH(152)
               return

L31250:     put #1, using L30110, smco, purch, wrkcntr, interest,devratio,~
                    pcent
            rewrite #1
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              str(line2$,62%) = "ROPSYSIN: " & str(cms2v$,,8%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40190,         /* Smoothing Coeffic */   ~
                                L40190,         /* Fixed Purchasing  */   ~
                                L40190,         /* Fixed Workcenter  */   ~
                                L40190,         /* Yearly Interest R */   ~
                                L40190,         /* Deviation Ratio   */   ~
                                L40190          /* Minimum Variance %*/
              goto L40210

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
                  lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40190:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40210:     accept                                                       ~
               at (01,02),                                               ~
                  "ROP System Parameters Management",                    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Smoothing Coefficient",                      ~
               at (06,32), fac(lfac$( 1)), str(smco$,2)         , ch(08),~
                                                                         ~
               at (07,02), "Fixed Purchasing Cost",                      ~
               at (07,30), fac(lfac$( 2)), purch$               , ch(10),~
                                                                         ~
               at (08,02), "Fixed Workcenter Cost",                      ~
               at (08,30), fac(lfac$( 3)), wrkcntr$             , ch(10),~
                                                                         ~
               at (09,02), "Yearly Interest Rate",                       ~
               at (09,35), fac(lfac$( 4)), str(interest$,2)     , ch(05),~
                                                                         ~
               at (10,02), "Deviation Ratio",                            ~
               at (10,32), fac(lfac$( 5)), str(devratio$,2)     , ch(08),~
                                                                         ~
               at (11,02), "Minimum Variance Percentage",                ~
               at (11,35), fac(lfac$(6)), str(pcent$,2)         , ch(05),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,47), fac(hex(8c)), pf12$                  , ch(17),~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys (pfkeys$),                                           ~
               key (keyhit%)

               if keyhit% <> 13 then L40590
                  call "MANUAL" ("ROPSYSIN")
                  goto L40210

L40590:        if keyhit% <> 15 then L40630
                  call "PRNTSCRN"
                  goto L40210

L40630:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* Smoothing Coeffic */     ~
                              L50200,         /* Fixed Purchasing  */     ~
                              L50400,         /* Fixed Workcenter  */     ~
                              L50500,         /* Yearly Interest R */     ~
                              L51005,         /* Deviation Ratio   */     ~
                              L51045          /* Variance Percent  */
            return

L50100: REM Test for Smoothing Coefficient        SMCO$
            call "NUMTEST" (smco$,.0000001,.9999999,errormsg$,-2.7,smco)
            if errormsg$ <> " " then return
            x% = pos (smco$ = "0")
            if x% <> 0 then str(smco$,x%,1) = " "
            return

L50200: REM Test for Fixed Purchasing Cost        PURCH$
            call "NUMTEST" (purch$,1,9e7,errormsg$,-4.4,purch)
            return

L50400: REM Test for Fixed Workcenter Cost        WRKCNTR$
            call "NUMTEST" (wrkcntr$,1,9e7,errormsg$,-4.4,wrkcntr)
            return

L50500: REM Test for Yearly Interest Rate         INTEREST$
            call "NUMTEST" (interest$,.0001,.9999,errormsg$,-2.4,interest)
            if errormsg$ <> " " then return
            x% = pos (interest$ = "0")
            if x% <> 0 then str(interest$,x%,1) = " "
            return

L51005: REM Test for Deviation Ratio              DEVRATIO$
            call "NUMTEST" (devratio$,0,.99,errormsg$,-2.2,devratio)
            if errormsg$ <> " " then return
            x% = pos (devratio$ = "0")
            if x% <> 0 then str(devratio$,x%,1) = " "
            return

L51045: REM Test for Minimum Variance Percentage  PCENT$
            call "NUMTEST" (pcent$,0,.99,errormsg$,-2.2,pcent)
            if errormsg$ <> " " then return
            x% = pos (pcent$ = "0")
            if x% <> 0 then str(pcent$,x%,1) = " "
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
            call "SHOSTAT" ("One Moment Please")

            end
