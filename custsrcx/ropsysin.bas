        REM *************************************************************~
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
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/30/87 ! Original                                 ! LKM *~
            * 05/31/89 ! Changed Purchasing and WC fixed cost     ! MJB *~
            *          !  defaults to "1" and test accordingly.   !     *~
            * 04/06/94 ! Additional Mods for APC New ROP Calc     ! RHH *~
            *          ! System                                   !     *~
            * 11/21/97 ! Mod for Upgrade to new Release R6.04.03  ! RHH *~
            * 04/08/98 ! Y2K modifications & CMS fmt changes      ! ERN *~
            *************************************************************

        dim                                                              ~
            blankdate$10,                /* PD fmt                     */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            devratio$9,                  /* Deviation Ratio            */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            interest$6,                  /* Yearly Interest Rate       */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pcent$6,                     /* Minimum Variation Percent  */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            pf12$17,                     /* PF12 Screen Literal        */~
            purch$10,                    /* Fixed Purchasing Cost      */~
            readkey$20,                  /* Miscellaneous read key     */~
            smco$9,                      /* Smoothing Coefficient      */~
            svyymm$8,                    /* Date Last Calculated       */~
            scrtime$8,                   /* Screen Time                */~
            userid$3,                    /* Current User Id            */~
            wrkcntr$10                   /* Fixed Workcenter Cost      */

        dim f2%(2%),                     /* = 0 if the file is open    */~
            f1%(2%),                     /* = 1 if READ was successful */~
            fs%(2%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(2%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) ROP System Parameters Management  "
            pname$ = "ROPSYSIN - Rev: R6.04"

        REM *************************************************************

            mat f2% = con
            mat f1% = zer
                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! Store APC ROP System Parameters          *~
            *************************************************************

            select # 1, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            call "SHOSTAT" (" Opening Files ")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 0%, rslt$(1%))

            if min(fs%()) < 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "DATUFMTC" (blankdate$)
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$, date%, svyymm$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            read #1,hold,key = "SWITCHS.HNY", eod goto L65000
               get #1 using L09130, cal$
L09130:          FMT POS(95), CH(1)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            onfile = 0
            pf4$ = " " : pf5$ = " " : pf16$ = "(16)Exit Program"
            pf12$ = " "
            pfkeys$ = hex(00010405ff0f10ffffffff)
            gosub L29000
            gosub dataload
            if rec% = 1% then goto editpg1

            for fieldnr% = 1% to  6%
                if fieldnr% > 1% then pf4$ = "(4)Previous Field"
L10170:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0% then L10290
L10190:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10270
L10220:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10190
                         if fieldnr% = 1% then L10170
                         goto L10220
L10270:              if keyhit% = 16% and fieldnr% = 1% then exit_program
                     if keyhit% <>  0% then       L10190
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
               str(pfkeys$,8%,1%) = hex(0c)
L11120:     inpmessage$ = edtmessage$
            pf16$ = "(16)Save/Exit"
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 12% then       delete_mode
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
            fieldnr% = cursor%(1) - 5%
            if fieldnr% < 1% or fieldnr% >  6% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
                  pf4$, pf5$, pf16$ = " "
L11260:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11260
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
            inpmessage$ = "Enter Safety Stock (Zero) Adjustment Percentag~
        ~e?"
            if smco$ = " " then smco$ = "      .20"
            return

L20200: REM Def/Enable Fixed Purchasing Cost       PURCH$
            if purch$ = " " then purch$ = "1"
            inpmessage$ = "Enter Fixed Purchasing Cost - '1' implies" &  ~
                          " NO Fixed Cost?"
            return

L20400: REM Def/Enable Fixed Workcenter Cost       WRKCNTR$
            if wrkcntr$ = " " then wrkcntr$ = "1"
            inpmessage$ = "Enter Fixed Workcenter Cost - '1' implies" &  ~
                          " NO Fixed Cost?"
            return

L20500: REM Def/Enable Yearly Interest Rate        INTEREST$
            inpmessage$ = "Enter Economic Order Quantity Adjustment Perce~
        ~ntage?"
            if interest$ = " " then interest$ = "   .12"
            return

L20610: REM Def/Enable Yearly Interest Rate        DEVRATIO$
            inpmessage$ = "Enter Safety Stock Deviation Percentage?"
            if devratio$ = " " then devratio$ = "      .10"
            return

L21005: REM Def/Enable Minimum Variation Percent   PCENT$
            inpmessage$ = "Enter ROP Standard Deviation Percentage?"

            if pcent$ = " " then pcent$ = "   .05"
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
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
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
            rec% = 0%
            str(readkey$,1%,9%) = "ROP PARAM"
            read #1,hold,key = readkey$, eod goto L30300
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
            if x% <> 0% then str(interest$,x%,1%) = " "
            call "CONVERT" (devratio, 2.2, devratio$)
            x% = pos (devratio$ = "0")
            if x% <> 0% then str(devratio$,x%,1%) = " "
            call "CONVERT" (pcent, 2.2, pcent$)
            x% = pos (pcent$ = "0")
            if x% <> 0% then str(pcent$,x%,1%) = " "
            onfile = 1
            rec% = 1%
            goto editpg1
L30300: return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            init(" ") readkey$
            rec% = 0%
            str(readkey$,1%,20%) = "ROP PARAM" 
            read #1,hold,key = readkey$, eod goto L31170
               rec% = 1%
            if rec% = 1% then goto L31230
               readkey$ = "ROP PARAM"
               if cal$ = "G" then L31140
                  call "WHICHPER" (#1, date$, per_or_ccyymm%)
                  goto L31170

L31140:        convert str(svyymm$, 1%, 6%) to per_or_ccyymm%

L31170:        write #1 using L31190, readkey$, smco, purch, wrkcntr,     ~
                                     interest,devratio,pcent," ",         ~
                                     per_or_ccyymm%, 0%, blankdate$, " ", ~
                                     " "

L31190:        FMT CH(20), PD(14,7), 5*PD(14,4), CH(25), BI(4), BI(1),   ~
                   CH(6), CH(244), CH(152)
               return

L31230:     put #1, using L30110, smco, purch, wrkcntr, interest,devratio,~
                    pcent
            rewrite #1
        return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              scrtime$ = " " : call "TIME" (scrtime$)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40210,         /* Safety Stock Adj %*/   ~
                                L40210,         /* Fixed Purch - N/A */   ~
                                L40210,         /* Fixed Work  - N/A */   ~
                                L40210,         /* EOQ Adj %         */   ~
                                L40210,         /* Safty Stk +- Adj  */   ~
                                L40210          /* ROP STD Dev +- %  */
              goto L40230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
                  lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40210:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40230:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (06,02), "Safety Stock Zero Adj ( % of EOQ )",         ~
               at (06,42), fac(lfac$( 1)), str(smco$,2%)        , ch(08),~
                                                                         ~
               at (07,02), "Fixed Purchasing Cost - N/A",                ~
               at (07,40), fac(lfac$( 2)), purch$               , ch(10),~
                                                                         ~
               at (08,02), "Fixed Workcenter Cost - N/A",                ~
               at (08,40), fac(lfac$( 3)), wrkcntr$             , ch(10),~
                                                                         ~
               at (09,02), "Economic Order Qty (+- % Adj Pcnt)",         ~
               at (09,45), fac(lfac$( 4)), str(interest$,2%)    , ch(05),~
                                                                         ~
               at (10,02), "Safety Stock ( +- % Adj Pcnt )    ",         ~
               at (10,42), fac(lfac$( 5)), str(devratio$,2%)    , ch(08),~
                                                                         ~
               at (11,02), "ROP Std Deviation (+- % Adj Pcnt) ",         ~
               at (11,45), fac(lfac$(6)), str(pcent$,2%)        , ch(05),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,47), fac(hex(8c)), pf12$                  , ch(17),~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$                          ,~
                                                                         ~
               keys (pfkeys$),                                           ~
               key (keyhit%)

               if keyhit% <> 15% then L40640
                  call "PRNTSCRN"
                  goto L40230

L40640: close ws
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
            if x% <> 0 then str(smco$,x%,1%) = " "
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
            if x% <> 0 then str(interest$,x%,1%) = " "
            return

L51005: REM Test for Deviation Ratio              DEVRATIO$
            call "NUMTEST" (devratio$,0,.99,errormsg$,-2.2,devratio)
            if errormsg$ <> " " then return
            x% = pos (devratio$ = "0")
            if x% <> 0 then str(devratio$,x%,1%) = " "
            return

L51045: REM Test for Minimum Variance Percentage  PCENT$
            call "NUMTEST" (pcent$,0,.99,errormsg$,-2.2,pcent)
            if errormsg$ <> " " then return
            x% = pos (pcent$ = "0")
            if x% <> 0 then str(pcent$,x%,1%) = " "
            return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")

        end
