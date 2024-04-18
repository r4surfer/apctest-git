        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   EEEEE  RRRR   EEEEE  X   X  PPPP   L       SSS    *~
            *  S      E      R   R  E       X X   P   P  L      S       *~
            *   SSS   EEEE   RRRR   EEEE     X    PPPP   L       SSS    *~
            *      S  E      R   R  E       X X   P      L          S   *~
            *   SSS   EEEEE  R   R  EEEEE  X   X  P      LLLLL   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SEREXPLS - Captures Part & Serial # and passes them to the*~
            *            subroutine SERXPLOD, which displays/prints a   *~
            *            2-level serial # explosion.  The Part/Serial # *~
            *            combination may be captured directly or via    *~
            *            Customer/Invoice/Line # or Job number.         *~
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
            * 02/12/87 ! Original                                 ! JIM *~
            * 06/09/87 ! HNYMASTR Format change                   ! MJB *~
            * 05/29/91 ! Added a Call to 'PICKYEAR' Serial Numbers! SID *~
            * 04/22/92 ! PRR 10996 - Changed (16)Exit Program to  ! MLJ *~
            *          !   (16)RETURN on Pick by Invoice and Pick !     *~
            *          !   by Job screens.                        !     *~
            *          ! PRR 11278 - Changed DIM on INVLINE$ from !     *~
            *          !   3 to 4 char.                           !     *~
            *          ! PRR 11042 - Length of Serial Number is   !     *~
            *          !   based on the MAX set in HNYFLAGS.      !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            choice$4,                    /* Year User Selected         */~
            currentfile$23,              /* Current Usage File         */~
            cursor%(2),                  /* Cursor location for edit   */~
            customer$9, custname$32,     /* Customer Code & name       */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fileid$4,                    /* File Prefix to find "SERM" */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            invline$4,                   /* Line Item Number           */~
            invoice$8,                   /* Invoice Number             */~
            job$8,                       /* Job Number                 */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            max$2,                       /* Maximum Serail # Length    */~
            msg$79,                      /* Misc messagees             */~
            partnbr$25, partdesc$34,     /* Part Number & description  */~
            pf14$23,                     /* PF 14 Screen Literal       */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf6$18,                      /* PF 6 Screen Literal        */~
            pf7$18,                      /* PF 7 Screen Literal        */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            serial$(1)20,                /* Serial Number              */~
            serdprname$8,                /* Ser. Detail File Name      */~
            sermprname$8,                /* Ser. Master File Name      */~
            userid$3                     /* Current User Id            */~

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
            cms2v$ = "R6.02.00 09/09/92 Cycle Counting & MPS Phase I    "
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
            *  #1 ! SERMASTR ! Serial Number Tracking Master File       *~
            *  #2 ! SERDETAL ! Serial Number Tracking Parent - Componen *~
            *  #3 ! HNYMASTR ! Inventory Master File                    *~
            *  #4 ! CUSTOMER ! CUSTOMER MASTER FILE                     *~
            *  #5 ! SYSFILE2 ! System Information File                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1, "SERMASTR",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #2, "SERDETAL",                                       ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =   46, keylen =  48,                     ~
                        alt key  1, keypos =    1, keylen =  93

            select #3, "HNYMASTR",                                       ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4

            select #4, "CUSTOMER",                                       ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #5, "SYSFILE2",                                       ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (#2, fs%( 2), f2%( 2), 0%, rslt$( 2))
            call "OPENCHCK" (#3, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (#4, fs%( 4), f2%( 4), 0%, rslt$( 4))
            call "OPENCHCK" (#5, fs%( 5), f2%( 5), 0%, rslt$( 5))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            currentfile$ = "Current DataBase File."
            str(line2$,1,) = "Archive File Year: " & currentfile$

        REM Get Serial Number Length...
            max$ = "6"
            plowkey$ = "SWITCHS.HNY         "
            call "READ100" (#5, plowkey$, f1%(5))
                if f1%(5) = 0% then L09270
            get #5 using L09260, max$
L09260:         FMT POS(42), CH(2)
L09270:     max% = 6
            convert max$ to max%
            mat redim serial$(1)max%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            inp% = 1%
            gosub L29000

            pf6$ = "(6)Pick by Invoice"
            pf7$ = "(7)Pick by Job"
            pf14$ = "(14)Select Archive Year"
            pf16$ = "(16)Exit Program"
            for fieldnr% = 1 to  2
L10130:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10280
L10150:         pf4$=" " : if fieldnr% > 1 then pf4$ = "(4)Previous Field"
L10160:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10240
L10190:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10150
                         if fieldnr% = 1% then L10130
                         goto L10190
L10240:               if keyhit%  =  6 then       inputmode_invoice
                      if keyhit%  =  7 then       inputmode_job
                      if keyhit%  = 14 then       select_archive_year
                      if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10160
L10280:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10160
            next fieldnr%
            goto editpg1

        inputmode_invoice
            inp% = 2%
            gosub L29000

            pf16$ = "(16)RETURN      "
            for fieldnr% = 1 to  4
L10370:         gosub'052(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10500
L10390:         pf4$=" " : if fieldnr% > 1 then pf4$ = "(4)Previous Field"
L10400:         gosub'102(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10475
L10430:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L10390
                         if fieldnr% = 1% then L10370
                         goto L10430
L10475:               if keyhit%  = 14 then       select_archive_year
                      if keyhit%  = 16 then       inputmode
                      if keyhit% <>  0 then       L10400
L10500:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10400
            next fieldnr%
            goto editpg2

        inputmode_job
            inp% = 3%
            gosub L29000

            pf16$ = "(16)RETURN      "
            for fieldnr% = 1 to  2
L10590:         gosub'053(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10720
L10610:         pf4$=" " : if fieldnr% > 1 then pf4$ = "(4)Previous Field"
L10620:         gosub'103(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10695
L10650:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'053(fieldnr%)
                         if enabled% = 1% then L10610
                         if fieldnr% = 1% then L10590
                         goto L10650
L10695:               if keyhit%  = 14 then       select_archive_year
                      if keyhit%  = 16 then       inputmode
                      if keyhit% <>  0 then       L10620
L10720:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10620
            next fieldnr%
            goto editpg3

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            pf4$, pf6$, pf7$, pf14$ = " "
            pf16$ = "(16)Explode Data"
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       explode
                  if keyhit% <>  0 then       editpg1
L11150:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  2 then editpg1
            if fieldnr% = lastfieldnr% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg1
                  pf4$, pf16$ = " "
L11210:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11210
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11210
                  lastfieldnr% = fieldnr%
            goto L11150

        editpg2
            pf4$  = " "
            pf16$ = "(16)Explode Data"
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'102(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       explode
                  if keyhit% <>  0 then       editpg2
L11380:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  4 then editpg2
            if fieldnr% = lastfieldnr% then editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg2
                  pf4$, pf16$ = " "
L11440:     gosub'102(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11440
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11440
                  lastfieldnr% = fieldnr%
            goto L11380

        editpg3
            pf4$  = " "
            pf16$ = "(16)Explode Data"
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'103(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       explode
                  if keyhit% <>  0 then       editpg3
L11610:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  2 then editpg3
            if fieldnr% = lastfieldnr% then editpg3
            gosub'053(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       editpg3
                  pf4$, pf16$ = " "
L11670:     gosub'103(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11670
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11670
                  lastfieldnr% = fieldnr%
            goto L11610

L15000: REM Test for Customer Code                CUSTOMER$
            custname$ = hex(06) & "Select a Customer Code"
            plowkey$ = "4" & customer$
            call "PLOWCODE" (#1, plowkey$, custname$, 8001%, 2.30,       ~
                f1%(1), null$(), 9, 0, null(), null$(), " ", " ", #4)
            if f1%(1) = 1% then goto L15080
                errormsg$ = "Customer Code not found in file.  Try again."
                return
L15080:     call "PUTPAREN" (custname$)
            customer$ = str(plowkey$,2,9)
            return

L15120: REM Test for Invoice Number               INVOICE$
            msg$ = hex(06) & "Select an Invoice Number"
            plowkey$ = "4" & str(customer$,,9) & invoice$
            call "PLOWCODE" (#1, plowkey$, msg$, 2010%, 2, f1%(1),       ~
                null$(), 8)
            if f1%(1) = 1% then goto L15210
                errormsg$ = "Invoice Number not found in file.  Try " &  ~
                     "again."
                return
L15210:     invoice$ = str(plowkey$,11,8)
            return

L15240: REM Test for Line Item Number             INVLINE$
            msg$ = hex(06) & "Select a Line Item Number"
            plowkey$ = "4" & str(customer$,,9) & str(invoice$,,8) &      ~
                invline$
            call "PLOWCODE" (#1, plowkey$, msg$, 2018%, 2, f1%(1),       ~
                null$(), 4)
            if f1%(1) = 1% then goto L15340
                errormsg$ = "Line number record not found in file.  Try"&~
                     " again."
                return
L15340:     invline$ = str(plowkey$,19,4)
            return

L15370: REM Test for Serial Number                SERIAL$
            msg$ = hex(06) & "Select a Serial Number"
            plowkey$ = "4" & str(customer$,,9) & str(invoice$,,8) &      ~
                str(invline$,,4)
            str(plowkey$,32) = serial$(1)
            call "PLOWCODE" (#1, plowkey$, msg$, 2031%, 2, f1%(1),       ~
                null$(), 20)
            if f1%(1) = 1% then goto L15480
                errormsg$ = "Customer/Invoice/Line/Serial # record not "&~
                     "found in file.  Try again."
                return
L15480:     serial$(1) = str(plowkey$,32,20)
            return

L15510: REM Test for Job Number                   JOB$
            msg$ = hex(06) & "Select a Job Number"
            plowkey$ = "1" & job$
            call "PLOWCODE" (#1, plowkey$, msg$, 2001%, 2, f1%(1),       ~
                null$(), 8)
            if f1%(1) = 1% then goto L15600
                errormsg$ = "Job number record not found in file.  " &   ~
                     "Try again."
                return
L15600:     job$ = str(plowkey$,2,8)
            return

L15630: REM Test for Serial Number                SERIAL$
            msg$ = hex(06) & "Select a Serial Number"
            plowkey$ = "1" & str(job$,,8)
            str(plowkey$,32) = serial$(1)
            call "PLOWCODE" (#1, plowkey$, msg$, 2031%, 2, f1%(1),       ~
                null$(), 20)
            if f1%(1) = 1% then goto L15730
                errormsg$ = "Job/Serial # record not found in file.  Tr"&~
                     "y again."
                return
L15730:     serial$(1) = str(plowkey$,32,20)
            return

        REM *************************************************************~
            *             S U B R O U T I N E   C A L L E R             *~
            *-----------------------------------------------------------*~
            * Calls SERXPLOD, passing found Part & Serial #s.           *~
            *************************************************************

        explode
            get #1 using L19080, partnbr$, serial$(1)
L19080:         FMT  POS(52), CH(25), CH(20)
            call "SERXPLOD" (partnbr$, serial$(1), #2, #3)
            on inp% goto inputmode, inputmode_invoice, inputmode_job
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L20130,         /* Part Number        */    ~
                              L20180          /* Serial Number      */
            return

L20130: REM Def/Enable Part Number                 PARTNBR$
            inpmessage$ = "Enter a Part number or partial to see availa"&~
                "ble codes"
            return

L20180: REM Def/Enable Serial Number               SERIAL$
            inpmessage$ = "Enter a Serial number or partial to see avai"&~
                "lable codes"
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L21150,         /* Customer Code      */    ~
                              L21200,         /* Invoice Number     */    ~
                              L21250,         /* Line Item Number   */    ~
                              L20180          /* Serial Number      */
            return

L21150: REM Def/Enable Customer Code               CUSTOMER$
            inpmessage$ = "Enter a Customer code or partial to see avai"&~
                "lable codes"
            return

L21200: REM Def/Enable Invoice Number              INVOICE$
            inpmessage$ = "Enter an Invoice number or partial to see av"&~
                "ailable codes"
            return

L21250: REM Def/Enable Line Item Number            INVLINE$
            inpmessage$ = "Enter a Line Item number or partial to see a"&~
                "vailable codes"
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  3  of Input. *~
            *************************************************************

        deffn'053(fieldnr%)
            inpmessage$ = " "
            enabled% = 1%
            on fieldnr% gosub L22130,         /* Job Number         */    ~
                              L20180          /* Serial Number      */
            return

L22130: REM Def/Enable Job Number                  JOB$
            inpmessage$ = "Enter a Job number or partial to see availab"&~
                "le codes"
            return

L29000: REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$,                            ~
                      partnbr$, partdesc$    , /* Part Number        */  ~
                      serial$(1)             , /* Serial Number      */  ~
                      customer$, custname$   , /* Customer Code      */  ~
                      invoice$               , /* Invoice Number     */  ~
                      invline$               , /* Line Item Number   */  ~
                      job$                     /* Job Number         */
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

        select_archive_year
            fileid$ = "SERM"
            call "PICKYEAR" (fileid$, choice$)
             if f2%(1) = 0% then close #1 : if f2%(2) = 0% then close #2
             if choice$ = "CURR" or choice$ = " "  then L30035 else L30040
L30035:      sermprname$="SERMASTR" : serdprname$="SERDETAL" : goto L30060
L30040:      sermprname$="SERM" & choice$
             serdprname$="SERD" & choice$
L30060:      call "PUTPRNAM" addr(#1, sermprname$)
             call "PUTPRNAM" addr(#2, serdprname$)
             call "OPENCHCK"(#1, fs%(1), f2%(1), 0%, rslt$(1))
             call "OPENCHCK"(#2, fs%(2), f2%(2), 0%, rslt$(2))
            if choice$ = "CURR" or choice$ = " "                         ~
                then str(line2$,20) = "Current DataBase File."           ~
                else str(line2$,20) = choice$ & hex(00)
            if inp% = 1% then goto inputmode
            if inp% = 2% then goto inputmode_invoice
            if inp% = 3% then goto inputmode_job

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%)
              gosub set_pfkeys1
              str(line2$,62%) = "SEREXPLS: " & str(cms2v$,,8%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40150,         /* Part Number       */   ~
                                L40150          /* Serial Number     */
              goto L40180

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40150:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40180:     accept                                                       ~
               at (01,02),                                               ~
                  "Serial Number Explosion",                             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,07), "Part Number:",                               ~
               at (06,20), fac(lfac$( 1)), partnbr$             , ch(25),~
               at (06,46), fac(hex(8c)),   partdesc$            , ch(34),~
                                                                         ~
               at (07,05), "Serial Number:",                             ~
               at (07,20), fac(lfac$( 2)), serial$(1),                   ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,20), fac(hex(8c)), pf4$                           ,~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), fac(hex(8c)), pf6$                           ,~
               at (23,65), "(15)Print Screen",                           ~
               at (24,20), fac(hex(8c)), pf7$                           ,~
               at (24,40), fac(hex(8c)), pf14$                          ,~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(pfkeys$), key (keyhit%)

               if keyhit% <> 13 then L40480
                  call "MANUAL" ("SEREXPLS")
                  goto L40180

L40480:        if keyhit% <> 15 then L40520
                  call "PRNTSCRN"
                  goto L40180

L40520:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        set_pfkeys1
            if fieldnr% <> 1% then L40600
               pfkeys$ = hex(00010406070d0e0f10)
               return
L40600:     pf14$ = " " : str(pfkeys$,7,1) = hex(ff) : return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%)
              gosub set_pfkeys2
              str(line2$,62%) = "SEREXPLS: " & str(cms2v$,,8%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L41170,         /* Customer Code     */   ~
                                L41170,         /* Invoice Number    */   ~
                                L41170,         /* Line Item Number  */   ~
                                L41170          /* Serial Number     */
              goto L41200

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L41170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L41200:     accept                                                       ~
               at (01,02),                                               ~
                  "Serial Number Explosion",                             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,05), "Customer Code:",                             ~
               at (06,20), fac(lfac$( 1)), customer$            , ch(09),~
               at (06,46), fac(hex(8c)),   custname$            , ch(32),~
                                                                         ~
               at (07,04), "Invoice Number:",                            ~
               at (07,20), fac(lfac$( 2)), invoice$             , ch(08),~
                                                                         ~
               at (08,02), "Line Item Number:",                          ~
               at (08,20), fac(lfac$( 3)), invline$             , ch(04),~
                                                                         ~
               at (09,05), "Serial Number:",                             ~
               at (09,20), fac(lfac$( 4)), serial$(1),                   ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,20), fac(hex(8c)), pf4$                           ,~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,40), fac(hex(8c)), pf14$                          ,~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(pfkeys$), key (keyhit%)

               if keyhit% <> 13 then L41540
                  call "MANUAL" ("SEREXPLS")
                  goto L41200

L41540:        if keyhit% <> 15 then L41580
                  call "PRNTSCRN"
                  goto L41200

L41580:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        set_pfkeys2
            if fieldnr% <> 1% then L41650
               pfkeys$ = hex(0001040d0e0f10) : return
L41650:     pf14$ = " " : str(pfkeys$,5,1) = hex(ff) : return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(fieldnr%)
              gosub set_pfkeys3
              str(line2$,62%) = "SEREXPLS: " & str(cms2v$,,8%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42150,         /* Job Number        */   ~
                                L42150          /* Serial Number     */
              goto L42180

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42150:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42180:     accept                                                       ~
               at (01,02),                                               ~
                  "Serial Number Explosion",                             ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,08), "Job Number:",                                ~
               at (06,20), fac(lfac$( 1)), job$                 , ch(08),~
                                                                         ~
               at (07,05), "Serial Number:",                             ~
               at (07,20), fac(lfac$( 2)), serial$(1),                   ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,20), fac(hex(8c)), pf4$                           ,~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,40), fac(hex(8c)), pf14$                          ,~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(pfkeys$), key (keyhit%)

               if keyhit% <> 13 then L42450
                  call "MANUAL" ("SEREXPLS")
                  goto L42180

L42450:        if keyhit% <> 15 then L42490
                  call "PRNTSCRN"
                  goto L42180

L42490:           close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        set_pfkeys3
            if fieldnr% <> 1% then L42560
               pfkeys$ = hex(0001040d0e0f10) : return
L42560:     pf14$ = " " : str(pfkeys$,5,1) = hex(ff) : return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50120,         /* Part Number       */     ~
                              L50240          /* Serial Number     */
            return

L50120: REM Test for Part Number                  PARTNBR$
            partdesc$ = hex(06) & "Select a Part Number"
            plowkey$ = str(partnbr$)
            call "PLOWCODE" (#1, plowkey$, partdesc$, -8025%, -.32,      ~
                f1%(1), null$(), 0, 0, null(), null$(), " ", " ", #3)
            if f1%(1) = 1% then goto L50200
                errormsg$ = "Part Number not found in file.  Try again."
                return
L50200:     partnbr$ = str(plowkey$,,25)
            call "PUTPAREN" (partdesc$)
            return

L50240: REM Test for Serial Number                SERIAL$
            msg$ = hex(06) & "Select a Serial Number"
            plowkey$ = str(partnbr$,,25) & serial$(1)
            call "PLOWCODE" (#1, plowkey$, msg$, 25%, 0, f1%(1))
            if f1%(1) = 1% then goto L50320
                errormsg$ = "Part/Serial # record not found in file. " & ~
                     " Try again."
                return
L50320:     serial$(1) = str(plowkey$,26,20)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L15000,         /* Customer Code     */     ~
                              L15120,         /* Invoice Number    */     ~
                              L15240,         /* Line Item Number  */     ~
                              L15370          /* Serial Number     */
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 3.                      *~
            *************************************************************

        deffn'153(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L15510,         /* Job Number        */     ~
                              L15630          /* Serial Number     */
            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
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
