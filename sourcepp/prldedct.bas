        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   L      DDDD   EEEEE  DDDD    CCC    TTTTT  *~
            *  P   P  R   R  L      D   D  E      D   D  C   C     T    *~
            *  PPPP   RRRR   L      D   D  EEEE   D   D  C         T    *~
            *  P      R   R  L      D   D  E      D   D  C   C     T    *~
            *  P      R   R  LLLLL  DDDD   EEEEE  DDDD    CCC      T    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRLDEDCT - INPUT AND LIST PAYROLL DEDUCTION DEFINITION    *~
            *            TABLE. STANDARD INPUT/LIST PROGRAM WITH A TWIST*~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/23/80 ! ORIGINAL                                 ! BCW *~
            * 12/09/80 ! MOD FOR ROUTINE NAME INSTEAD OF NUMBER.  ! BCW *~
            * 06/01/81 ! NO TEST FOR ROUTINE NAME **RISQUE**      ! TEM *~
            * 05/06/86 ! CHANGED SCREEN AND TESTS FOR ROUTINES    ! SGA *~
            * 09/06/86 ! Added DOL/HOUR routine                   ! HES *~
            * 12/04/86 ! Changed Mass. Income tax Defaults        ! SGA *~
            * 06/18/87 ! Added Virginia Income Tax Stuff          ! HES *~
            * 10/07/87 ! Added QBENS routine for cafeteria plans  ! HES *~
            *          ! Also added a straight % of gross to FIT  ! HES *~
            * 11/24/87 ! For Utah tax change of '87               ! HES *~
            * 01/05/88 ! Added SYSFLAT routine, print more on rpt ! HES *~
            * 02/05/88 ! Added Kansas Income tax Defaults         ! HES *~
            * 02/25/88 ! Added Oregon Income tax Defaults         ! HES *~
            * 07/03/88 ! Added North Carolina Income Tax Defaults ! MDE *~
            * 12/01/88 ! Updated California Income Tax Constants  ! KAB *~
            * 12/15/89 ! 1990 Repairs                             ! KAB *~
            * 12/02/91 ! 1991 Repairs                             ! KAB *~
            * 12/13/91 ! 1992 Repairs                             ! KAB *~
            * 12/20/91 ! Possible additional NC Update            ! KAB *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            *************************************************************

        dim                                                              ~
            amount(4),                   /* AMOUNTS FOR DISK I/O       */~
            amount$(4)12,                /* AMOUNTS FOR 4 AMOUNT FIELDS*/~
            applies$6,                   /* WHICH PAY PERIODS/MO TO DO */~
            blankline$79,                /* BLANK LINE FOR PRINT SCREEN*/~
            creditacct$16,               /* CREDIT ACCOUNT FOR DEDXN   */~
            creditacctdescr$32,          /* CREDIT ACCOUNT DESCRIPTION */~
            camount(6),                  /* AMOUNTS FOR 4 AMOUNT FIELDS*/~
            camount$(6)12,               /* AMOUNTS FOR 4 AMOUNT FIELDS*/~
            cdescr$(6)25,                /* DESCRIPTION OF NUMBERS     */~
            cmpname$60,                  /* COMPANY NAME FOR REPORT    */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDITING*/~
            date$8,                      /* SCREEN DATE FOR DISPLAY    */~
            debitacct$16,                /* DEBIT ACCOUNT THIS DEDXN   */~
            debitacctdescr$32,           /* DEBIT ACCOUNT DESCRIPTION  */~
            descr$12,                    /* DEDUCTION DESCRIPTION      */~
            descr$(4)15,                 /* DESCRIPTION OF NUMBERS     */~
            edtmessage$79,               /* EDIT MESSAGE FIELD         */~
            empflag$3,                   /* EMPLOYER/EMPLOYEE FLAG     */~
            errormsg$79,                 /* ERROR MESSAGE TEXT         */~
            fac$(25)1,                   /* FIELD ATTRIBUTE CHARACTERS */~
            firstcode$6,                 /* FIRST METHOD IN PRT RANGE  */~
            goal$10,                     /* GOAL AMOUNT THIS DEDUCTION */~
            head$40,                     /* Heading for ASKUSER        */~
            hi$80,                       /* Top Line for ASKUSER       */~
            mid$80,                      /* Mid Line for ASKUSER       */~
            lo$80,                       /* Low Line for ASKUSER       */~
            i$(24)80,                    /* SCREEN IMAGE (NOT USED)    */~
            inpmessage$79,               /* INPUT MESSAGE TEXT         */~
            lastcode$6,                  /* LAST METHOD IN PRINT RANGE */~
            lastmethod$6,                /* LAST METHOD CODE INPUT     */~
            linenumber%(3),              /* LINE POINTER FOR PRINT MODE*/~
            method$6,                    /* METHOD OF DEDUCTION CODE   */~
            prgm$8,                      /* PROGRAM NAME               */~
            prgmid$79,                   /* PROGRAM ID FOR SCREEN      */~
            prtacctdescr$30,             /* ACCT DESCRIPTION FOR PRINT */~
            prtamt$10,                   /* AMOUNT FOR PRINT MODE      */~
            prtamtdescr$15,              /* DESCRIPTIONS FOR AMOUNTS   */~
            prtapplies$6,                /* HOW OFTEN IT APPLIES TO YOU*/~
            prtcreditacct$16,            /* CREDIT ACCOUNT TO PRINT    */~
            prtdebitacct$16,             /* DEBIT ACCOUNT NUMBER       */~
            prtdescr$12,                 /* DESCRIPTION OF DEDXN METH  */~
            prtempflag$3,                /* EMPLOYEE PAYS FLAG         */~
            prtgoal$10,                  /* GOAL FOR PRINT MODE        */~
            prtmethod$6,                 /* METHOD CODE FOR PRINTING   */~
            prtroutine$8,                /* ROUTINE NAME FOR PRLDDUCT  */~
            record$200,                  /* FREE SPACE IN DDT RECORD.  */~
            rptid$6,                     /* REPORT ID NUMBER           */~
            routine$8,                   /* SUBROUTINE NAME FOR DEDXNS */~
            exempt$1,                    /* EXEMPT FLAG                */~
            table$1, prttable$1,         /* Y/N IF TAX TABLE USED      */~
            title$60,                    /* REPORT TITLE               */~
            xdescr$30                    /* EXTENDED DEDUC DESCRIPTION */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other          "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !  DESCRIPTION                             *~
            *-----+----------+------------------------------------------*~
            * # 1 ! PRLDDT   ! PAYROLL DEDUCTION DEFINITION TABLE       *~
            * # 2 ! GLMAIN   ! GENERAL LEDGER MAIN FILE                 *~
            * # 3 ! WORKFILE ! TEMPORARY FILE                           *~
            *************************************************************

            select # 1, "PRLDDT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 400,                                   ~
                        keypos = 1, keylen = 6

            select # 2, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            select # 3, "WORKFILE",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 70,                                    ~
                        keypos = 1, keylen = 8


*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, 0%, f2%( 1), 100%, " ")
            call "OPENCHCK" (# 2, 0%, f2%( 2),   0%, " ")
            call "WORKOPEN" (# 3, "IO   ", 30%, f2%( 3))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZE SCREEN INFORMATION.                            *~
            * ALSO, THIS ROUTINE LOADS PAYROLL SUBROUTINE INFO AND MAKES*~
            * SURE IT'S STILL VALID.  IF NO SUBROUTINE INFO, THEN WE GO *~
            * INTO INPUT MODE.  IF SUBROUTINE INFO, BUT NOT VALID, THEN *~
            * GO INTO EDIT SUBROUTINE INFORMATION MODE.                 *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

            prgm$ = "PRLDEDCT"
            rptid$ = "PRL005"
            call "COMPNAME" (12%, cmpname$, ret%)
            ret% = 0
            str(prgmid$,61,9) = "PRLDEDCT:"
            str(prgmid$,70,8) = str(cms2v$,1,8)
            title$ = "D E D U C T I O N    D E F I N I T I O N   L I S T ~
        ~I N G"

            head$ = "DELETE VERIFICATION"

            write #3 using L09710, "FIT", "Federal Income Tax"
            write #3 using L09710, "AZ-IT", "Arizona State Income Tax"
            write #3 using L09710, "AL-IT", "Alabama State Income Tax"
            write #3 using L09710, "AR-IT", "Arkansas State Income Tax"
            write #3 using L09710, "UT-IT", "Utah State Income Tax"
            write #3 using L09710, "CA-IT", "California State Income Tax"
            write #3 using L09710, "GA-IT", "Georgia State Income Tax"
            write #3 using L09710, "IA-IT", "Iowa State Income Tax"
            write #3 using L09710, "ID-IT", "Idaho State Income Tax"
            write #3 using L09710, "IL-IT", "Illinois State Income Tax"
            write #3 using L09710, "KS-IT", "Kansas State Income Tax"
            write #3 using L09710, "KT-IT", "Kentucky State Income Tax"
            write #3 using L09710, "MA-IT", "Mass. State Income Tax"
            write #3 using L09710, "MD-IT", "Maryland State Income Tax"
            write #3 using L09710, "ME-IT", "Main State Income Tax"
            write #3 using L09710, "MO-IT", "Missouri State Income Tax"
            write #3 using L09710, "OH-IT", "Ohio State Income Tax"
            write #3 using L09710, "OK-IT", "Oklahoma State Income Tax"
            write #3 using L09710, "OR-IT", "Oregon State Income Tax"
            write #3 using L09710, "VA-IT", "Virginia State Income Tax"
            write #3 using L09710, "VT-IT", "Vermont State Income Tax"
            write #3 using L09710, "NC-IT", "North Carolina Income Tax"
            write #3 using L09710, "NC-IT1", "North Carolina Income Tax, 19~
        ~90+"
            write #3 using L09710, "SC-IT", "South Carolina Income Tax"
            write #3 using L09710, "CT-IT", "Connecticut Income Tax"
            write #3 using L09710, "401K", "Employee 401k (IRA) Contributio~
        ~ns"

            write #3 using L09710, "401K+", "Employer 401k (IRA) Contributi~
        ~ons"
            write #3 using L09710, "401K+SYS", "Employer 401k Contributions~
        ~(SYSTEM)"

            write #3 using L09710, "QBENS", "Employee deductions for Qualif~
        ~ied Benefits Plan."

            write #3 using L09710, "$PERHOUR", "Based on hrs worked, Cents/~
        ~Hr (Allows $.000001 rate) Wrk Comp."

            write #3 using L09710, "DOL/HOUR", "Based on hrs worked, Dollar~
        ~s/Hr (Allows $.0001 rate)"

            write #3 using L09710, "FLAT"    , "Flat amount up to goal, rat~
        ~e & goal vary by employee"

            write #3 using L09710, "SYSFLAT" , "Flat amount up to goal, rat~
        ~e & goal are SYSTEM CONSTANTS"

            write #3 using L09710, "%_OF_NET", "% of net with goal, net = g~
        ~ross less PRECEEDING deductions"

            write #3 using L09710, "FICA"    , "Employee & employer FICA. '~
        ~QBENS' deductions are exempt"

            write #3 using L09710, "FICAEXMP", "Like FICA, except both 401K~
        ~ AND 'QBENS' are exempt"

            write #3 using L09710, "LIKEFICA", "Like FICA, except not effec~
        ~t by any 'QBENS' deductions"

            write #3 using L09710, "FICATYPE", "Same as LIKEFICA, except ra~
        ~te and limit vary by employee"

            write #3 using L09710, "WITHHOLD", "Percent of gross up to limi~
        ~t"

            write #3 using L09710, "TABLENET", "Table Calculation based on ~
        ~net"

            write #3 using L09710, "SYSTMPCT", "System % with Employee Modi~
        ~fier."

L09710:     FMT CH(8), CH(62)

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM FOR DEDUCTION DEFINITIONS.        *~
            *************************************************************

        inputmode
            init(" ") inpmessage$, errormsg$, empflag$, camount$(),      ~
                      method$, descr$, creditacct$, creditacctdescr$,    ~
                      debitacct$, debitacctdescr$, descr$(), amount$(),  ~
                      goal$, routine$, applies$, exempt$, record$,       ~
                      cdescr$(), xdescr$, table$

            call "ALLFREE"

            for fieldnr% = 1 to 8
                gosub'161(fieldnr%)
                      if enabled% =  0 then L10220
L10150:         gosub'201(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  3 and fieldnr% = 1 then printmode
                      if keyhit%  = 16 then L65000
                      if keyhit% <>  0 then L10150
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10150
L10220:         next fieldnr%

            for fieldnr% = 1 to 23
                gosub'162(fieldnr%)
                      if enabled% =  0 then L10320
L10270:         gosub'202(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then L65000
                      if keyhit% <>  0 then       L10270
                      if fieldnr% <> 1 then goto L10300
L10300:         gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10270
L10320:         next fieldnr%
                goto edtpg2

L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * EDIT MODE MAIN DRIVER ROUTINE FOR DEFINITIONS.            *~
            *************************************************************

        edtpg1
L11070:     gosub'211(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  5 then goto  edtpg2
                  if keyhit%  = 12 then gosub delete_record
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then L11070
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% > 8 then edtpg1

L11150:     gosub'211(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then       L11150
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11150
            goto edtpg1

        edtpg2
L11230:     gosub'212(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  4 then goto  edtpg1
                  if keyhit%  = 16 then goto  datasave
                  if keyhit% <>  0 then       L11230

            if cursor%(1) <> 5 and cursor% <> 11 then L11330
                  fieldnr% = cursor%(1) - 4
                  if fieldnr% = 1 and cursor%(2) > 28 then fieldnr% = 2
                  if cursor% = 11 then fieldnr% = 11
                  goto L11420
L11330:     if cursor%(1) < 7 or cursor%(1) > 11 then L11380
            fieldnr% = cursor%(1) * 2 - 11
            if fieldnr% = 11 then L11360
            if cursor%(2) > 20 then fieldnr% = fieldnr% + 1
L11360:     goto L11420

L11380:     if cursor%(1) < 14 or cursor%(1) > 20 then edtpg2
            fieldnr% = cursor%(1) * 2 - 16
            if cursor%(2) > 30 then fieldnr% = fieldnr% + 1

L11420:     gosub'164(fieldnr%)
                  if enabled% = 0 then edtpg2
            gosub'212(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then       L11420
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L11420
            goto edtpg2

        REM *************************************************************~
            *       P R I N T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * GETS RANGE OF DEDUCTIONS TO PRINT.                        *~
            *************************************************************

        printmode
            init(" ") errormsg$, firstcode$, lastcode$, blankline$

            firstcode$ = "ALL"
            str(prgmid$,1,12) = " "
            str(prgmid$,14,6) = " "

L12110:     gosub L44000
                  if keyhit%  =  1 then       inputmode
                  if keyhit%  = 16 then       L65000
            gosub L53000
                  if errormsg$ <> " " then L12110

            REM PLOW ROUTINE FOR PRINTING DDT LISTING.
                page% = 0
                line% = 1000
                tagprinted% = 1
                call "SHOSTAT" ("Printing Deduction Definition Table")

L12230:         call "PLOWNEXT" (#1, firstcode$, 0%, f1%(1))
                     if f1%(1) = 0 then L12300
                     if firstcode$ > lastcode$ then L12300
                gosub L32000              /* GET DDT CODE               */
                gosub L60000              /* AND PRINT IT.              */
                goto L12230

L12300:         REM RETURN FROM ROUTINE
                    close printer
                    goto inputmode

        REM *************************************************************~
            *                    W R I T E   D A T A                    *~
            *                                                           *~
            * WRITES DATA TO THE DEFINITION FILE, REPLACING THAT ALREADY*~
            * OUT THERE.                                                *~
            *************************************************************

        datasave
            gosub L31000
            lastmethod$ = method$
            goto inputmode

        REM *************************************************************~
            * D E F A U L T / E N A B L E   F O R   F I R S T   P A G E *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE FIRST PAGE.  THE *~
            * MOST INPORTANT THING THIS ROUTINE DOES IS SET INPMESSAGE$ *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L20100,         /* DEDXN METHOD     */~
                                    L20200,         /* DEDXN DESCRIPTION*/~
                                    L20250,         /* DESCR FOR CHECKS */~
                                    L20300,         /* EMPLOYEE FLAG    */~
                                    L20400,         /* CREDIT ACCOUNT   */~
                                    L20500,         /* DEBIT ACCOUNT    */~
                                    L20600,         /* APPLIES          */~
                                    L20700          /* EXEMPT FLAG      */
                    return
L20100:     REM DEFAULT/ENABLE FOR DEDUCTION METHOD
                enabled% = 1
                inpmessage$ = "To Print The Available Deduction Definitio~
        ~ns, Press (3)."
                return
L20200:     REM DEFAULT/ENABLE FOR DEDUCTION DESCRIPTION
                enabled% = 1
                inpmessage$ = "This is for reference only, please be as d~
        ~escriptive as possible."
                return
L20250:     REM DEFAULT/ENABLE FOR DESCRIPTION for checks
                enabled% = 1
                inpmessage$ = "If Description Left Blank, It Will Be Requ~
        ~ested On Deduction Input."
                return
L20300:     REM DEFAULT/ENABLE FOR EMPLOYEE/EMPLOYER SWITCH
                enabled% = 1
                empflag$ = "YES"
                inpmessage$ = "If This Field Is Yes, Then Employee Pays T~
        ~his Deduction (Default)."
                return
L20400:     REM DEFAULT/ENABLE FOR CREDIT ACCOUNT
                enabled% = 1
                inpmessage$ = "If Credit Account Is Blank, It Will Be Req~
        ~uested When Deductions Are Input."
                return
L20500:     REM DEFAULT/ENABLE FOR DEBIT ACCOUNT
                enabled% = 1
                inpmessage$ = "Debit Account May Be Left Blank For Prompt~
        ~ During Deduction Input."
                return
L20600:     REM DEFAULT/ENABLE FOR APPLIES FIELD
                enabled% = 1
                applies$ = "123456"
                inpmessage$ = "Enter A Digit For Each Pay Period This Mon~
        ~th That The Deduction Applies."
                return
L20700:     REM DEFAULT/ENABLE FOR APPLIES FIELD
                enabled% = 1
                exempt$ = "Y"
                inpmessage$ = "Enter 'Y' If It Is Possible For Some Pay T~
        ~o Be Exempt From This Deduction"
                return

        REM *************************************************************~
            *  S E T   D E F A U L T S   F O R   S E C O N D   P A G E  *~
            *                                                           *~
            * DEFAULT/ENABLE FOR THE SECOND PAGE.  THIS ROUTINE HELPS   *~
            * OUT BY WHIZZING BY WHEN THE DESCRIPTION IS BLANKED BEFORE *~
            * THIS ONE SO WE DON'T INPUT 4 AMOUNTS WHEN WE ONLY WANT 2  *~
            *************************************************************

            deffn'162(fieldnr%)
                  enabled% = 0 : inpmessage$ = " "
                  on fieldnr% gosub L21340,         /* ROUTINE NAME     */~
                                    L21370,         /* TABLE REQUIRED   */~
                                    L21400,         /* DESCRIPTION 1    */~
                                    L21490,         /* AMOUNT 1         */~
                                    L21400,         /* DESCRIPTION 2    */~
                                    L21490,         /* AMOUNT 2         */~
                                    L21400,         /* DESCRIPTION 3    */~
                                    L21490,         /* AMOUNT 3         */~
                                    L21400,         /* DESCRIPTION 4    */~
                                    L21490,         /* AMOUNT 4         */~
                                    L21530,         /* GOAL             */~
                                    L21400,         /* CDESCRIPTION 1   */~
                                    L21560,         /* CAMOUNT 1        */~
                                    L21400,         /* CDESCRIPTION 2   */~
                                    L21560,         /* CAMOUNT 2        */~
                                    L21400,         /* CDESCRIPTION 3   */~
                                    L21560,         /* CAMOUNT 3        */~
                                    L21400,         /* CDESCRIPTION 4   */~
                                    L21560,         /* CAMOUNT 4        */~
                                    L21400,         /* CDESCRIPTION 5   */~
                                    L21560,         /* CAMOUNT 5        */~
                                    L21400,         /* CDESCRIPTION 6   */~
                                    L21560          /* CAMOUNT 6        */
                    return
L21340:     REM DEFAULT/ENABLE FOR ROUTINE NAME
                enabled% = 1
                return
L21370:     REM DEFAULT/ENABLE FOR TABLE REQUIRED
                enabled% = 0
                return
L21400:     REM DEFAULT/ENABLE FOR DESCRIPTION
                v% = (fieldnr% - 1)/2
                inpmessage$ = " "
            REM ENABLED% = 0
                if fieldnr% > 9% then L21470
                   if v% >=1 and descr$(v%) <> " " then enabled% = 1
                   return
L21470:         if v%>=5 and cdescr$(v%-4) <> " " then enabled% = 1
                return
L21490:     REM DEFAULT/ENABLE FOR AMOUNT
                if descr$(v%) = " " then return
                   enabled% = 1
                   return
L21530:     REM DEFAULT/ENABLE FOR GOAL
                enabled% = g%
                if g% = 0% then goal$ = "      0.00"
                return
L21560:     REM DEFAULT/ENABLE FOR AMOUNT
                if cdescr$(v%-4) = " " then return
                   enabled% = 1
                   return

        REM *************************************************************~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *************************************************************

        deffn'164(fieldnr%)
            enabled% = 0%
            if e% = 0% then L22110
            if fieldnr% > (e%*2)+2 then L22110
               if fieldnr% <= 2 then L22110
               enabled% = 1%
               return
L22110:     if g% <> 1%  then  L22140
               if fieldnr% <> 11 then L22140
               enabled% = 1%
               return
L22140:     if c% = 0% then L22170
            if fieldnr% > (c%*2)+11 or fieldnr% < 12 then L22170
               enabled% = 1%
               return
L22170:     if fieldnr% <> 1% then return
               enabled% = 1%
               return

        REM *************************************************************~
            *                 D E L E T E   R O U T I N E               *~
            *                                                           *~
            * DELETES THE RECORD FROM THE FILE. THIS IS A SIMPLE JOB OF *~
            * WORK FOR US.                                              *~
            *************************************************************

        delete_record
            hi$ = "Press (RETURN) to delete this Method,"
            mid$ = "OR you may"
            lo$ = "Press PF1 to return to the edit mode screen."
            keyhit1% = 2%
            call "ASKUSER" (keyhit1%, head$, hi$, mid$, lo$)
            if keyhit1% <> 0% then return
            call "READ101" (#1, method$, f1%(1))
                 if f1%(1) <> 0 then delete #1     /*DELETE THE RECORD*/


            goto L10000

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode

L30000: REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *                                                           *~
            * LOADS THE DATA FROM THE FILE.  DESCRIBES ACCOUNTS, ETC.   *~
            *************************************************************

            call "READ101" (#1, method$, f1%(1))
                 if f1%(1) = 0 then return

            get #1, using L30300,                                         ~
                    method$, descr$, empflag$, creditacct$, debitacct$,  ~
                    applies$, descr$(1), descr$(2), descr$(3), descr$(4),~
                    amount(1), amount(2), amount(3), amount(4), goal,    ~
                    routine$, exempt$, cdescr$(), camount(), xdescr$,    ~
                    table$, record$

            convert goal to goal$, pic(-######.##)
            for temp% = 1 to 6
                if temp% > 4 then L30210
             if descr$(temp%) <> " " or amount(temp%) <> 0 then          ~
                convert amount(temp%) to amount$(temp%), pic(#######.####)
L30210:      if cdescr$(temp%) <> " " or camount(temp%) <> 0 then        ~
              convert camount(temp%) to camount$(temp%), pic(#######.####)
            next temp%
            call "DESCRIBE"(#2, creditacct$, creditacctdescr$, 1%, f1%(2))
                call "GLFMT" (creditacct$)
            call "DESCRIBE"(#2, debitacct$,  debitacctdescr$,  1%, f1%(2))
                call "GLFMT" (debitacct$)
            if empflag$ = "Y" then empflag$ = "YES"                      ~
                              else empflag$ = "NO"
            if table$   = " " then table$   = "N"

            gosub L58000
            return

L30300:     FMT CH(6),                   /* METHOD OF DEDUCTION        */~
                CH(12),                  /* DEDUCTION DESCRIPTION      */~
                CH(1),                   /* EMPLOYEE PAYS FLAG         */~
                CH(9),                   /* CREDIT ACCOUNT             */~
                CH(9),                   /* DEBIT ACCOUNT              */~
                CH(6),                   /* APPLIES                    */~
                CH(15),                  /* DESCRIPTION 1              */~
                CH(15),                  /* DESCRIPTION 2              */~
                CH(15),                  /* DESCRIPTION 3              */~
                CH(15),                  /* DESCRIPTION 4              */~
                PD(14,4),                /* AMOUNT 1                   */~
                PD(14,4),                /* AMOUNT 2                   */~
                PD(14,4),                /* AMOUNT 3                   */~
                PD(14,4),                /* AMOUNT 4                   */~
                PD(14,4),                /* GOAL                       */~
                CH(8),                   /* ROUTINE NAME               */~
                CH(1),                   /* EXEMPT FLAG                */~
                6*CH(25),                /* CONSTANT DESCRIPTIONS      */~
                6*PD(14,4),              /* CONSTANT AMOUNTS           */~
                CH(30),                  /* extended description       */~
                CH(1),                   /* TAX TABLE REQUIRED?        */~
                CH(19)                   /* FREE SPACE IN RECORD       */~

L31000: REM *************************************************************~
            *            W R I T E   D A T A   T O   F I L E            *~
            *                                                           *~
            * WRITES DATA TO FILE, WRITING OR REWRITING AS NEEDED.      *~
            *************************************************************

            mat amount = zer
            mat camount = zer
            for temp% = 1 to 6
                if temp% > 4 then L31120
                if amount$(temp%) <> " " then convert amount$(temp%) to  ~
                                                   amount(temp%)
L31120:         if camount$(temp%) <> " " then convert camount$(temp%) to~
                                                  camount(temp%)
                next temp%
            convert goal$ to goal
            if f1%(1) = 1 then delete #1
            if routine$ = " " then return
            call "GLUNFMT" (debitacct$)
            call "GLUNFMT" (creditacct$)

            write #1, using L31270,                                       ~
                    method$, descr$, empflag$, creditacct$, debitacct$,  ~
                    applies$, descr$(1), descr$(2), descr$(3), descr$(4),~
                    amount(1), amount(2), amount(3), amount(4), goal,    ~
                    routine$, exempt$, cdescr$(), camount(), xdescr$,    ~
                    table$, str(record$,,19)
            return

L31270:     FMT CH(6),                   /* METHOD OF DEDUCTION        */~
                CH(12),                  /* DEDUCTION DESCRIPTION      */~
                CH(1),                   /* EMPLOYEE PAYS FLAG         */~
                CH(9),                   /* CREDIT ACCOUNT             */~
                CH(9),                   /* DEBIT ACCOUNT              */~
                CH(6),                   /* APPLIES                    */~
                CH(15),                  /* DESCRIPTION 1              */~
                CH(15),                  /* DESCRIPTION 2              */~
                CH(15),                  /* DESCRIPTION 3              */~
                CH(15),                  /* DESCRIPTION 4              */~
                PD(14,4),                /* AMOUNT 1                   */~
                PD(14,4),                /* AMOUNT 2                   */~
                PD(14,4),                /* AMOUNT 3                   */~
                PD(14,4),                /* AMOUNT 4                   */~
                PD(14,4),                /* GOAL                       */~
                CH(8),                   /* ROUTINE NAME               */~
                CH(1),                   /* EXEMPT FLAG                */~
                6*CH(25),                /* CONSTANT DESCRIPTIONS      */~
                6*PD(14,4),              /* CONSTANT AMOUNTS           */~
                CH(30),                  /* extended description       */~
                CH(1),                   /* TAX TABLE REQUIRED?        */~
                CH(19)                   /* FREE SPACE IN DDT RECORD   */~

L32000: REM *************************************************************~
            *      L O A D   D A T A   F O R   P R I N T   M O D E      *~
            *                                                           *~
            * LOADS DATA FROM FILE, BUT DOES NOT FORMAT.  THIS SO THAT  *~
            * WE CAN FORMAT IN PRINT ROUTINE.                           *~
            *************************************************************

            get #1, using L32160,                                         ~
                    method$, descr$, empflag$, creditacct$, debitacct$,  ~
                    applies$, descr$(1), descr$(2), descr$(3), descr$(4),~
                    amount(1), amount(2), amount(3), amount(4), goal,    ~
                    routine$, cdescr$(), camount(), table$
            call "GLFMT" (creditacct$)
            call "GLFMT" (debitacct$)
            if table$ = "Y" then table$ = "*" else table$ = " "
            return

L32160:     FMT CH(6),                   /* METHOD OF DEDUCTION        */~
                CH(12),                  /* DEDUCTION DESCRIPTION      */~
                CH(1),                   /* EMPLOYEE PAYS FLAG         */~
                CH(9),                   /* CREDIT ACCOUNT             */~
                CH(9),                   /* DEBIT ACCOUNT              */~
                CH(6),                   /* APPLIES                    */~
                CH(15),                  /* DESCRIPTION 1              */~
                CH(15),                  /* DESCRIPTION 2              */~
                CH(15),                  /* DESCRIPTION 3              */~
                CH(15),                  /* DESCRIPTION 4              */~
                PD(14,4),                /* AMOUNT 1                   */~
                PD(14,4),                /* AMOUNT 2                   */~
                PD(14,4),                /* AMOUNT 3                   */~
                PD(14,4),                /* AMOUNT 4                   */~
                PD(14,4),                /* GOAL                       */~
                CH(8),                   /* ROUTINE NAME               */~
                XX(1),                   /* EXEMPT FLAG                */~
                6*CH(25),                /* CONSTANT DESCRIPTIONS      */~
                6*PD(14,4),              /* CONSTANT AMOUNTS           */~
                XX(30),                  /* SKIP EXTENDED DESCR        */~
                CH(1)                    /* TABLES REQUIRED            */

        REM *************************************************************~
            *              I N P U T   F I R S T   P A G E              *~
            *                                                           *~
            * INPUT FIRST PAGE OF DEFINITION.                           *~
            *************************************************************

            deffn'201(fieldnr%)
                  str(prgmid$,1,12) = "Last Method:"
                  str(prgmid$,14,6) = lastmethod$
                  init(hex(84)) fac$()
                  on fieldnr% gosub L40190,         /* METHOD/DEDUCTION */~
                                    L40160,         /* DEDUCTION DESCR  */~
                                    L40190,         /* SHORT DESCR      */~
                                    L40190,         /* EMPLOYEE FLAG    */~
                                    L40190,         /* CREDIT ACCOUNT   */~
                                    L40190,         /* DEBIT ACCOUNT    */~
                                    L40220,         /* APPLIES 123456   */~
                                    L40190          /* EXEMPT FLAG      */
                     goto L40260

L40160:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$(fieldnr%) = hex(80)
                      return
L40190:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(fieldnr%) = hex(81)
                      return
L40220:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      fac$(fieldnr%) = hex(82)
                      return

L40260:     accept                                                       ~
               at (01,02),                                               ~
                  "Enter Payroll Deduction and Accrual Methods",         ~
               at (01,64),                                               ~
                  "Today:",                                              ~
               at (01,71), fac(hex(8c)),  date$                 , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(94)),  errormsg$             , ch(79),~
               at (06,02),                                               ~
                  "METHOD OF DEDUCTION",                                 ~
               at (06,30), fac(fac$( 1)), method$               , ch(06),~
               at (07,02),                                               ~
                  "DEDUCTION DESCRIPTION",                               ~
               at (07,30), fac(fac$( 2)), xdescr$               , ch(30),~
               at (08,02),                                               ~
                  "DEFAULT DESCRIPTN FOR CHECKS",                        ~
               at (08,30), fac(fac$( 3)), descr$                , ch(12),~
               at (09,02),                                               ~
                  "PAID BY EMPLOYEE? (Y/N)",                             ~
               at (09,30), fac(fac$( 4)), empflag$              , ch(03),~
               at (10,02),                                               ~
                  "CREDIT (LIABILITY) ACCOUNT",                          ~
               at (10,30), fac(fac$( 5)), creditacct$           , ch(12),~
               at (10,49), fac(hex(8c)),  creditacctdescr$      , ch(32),~
               at (11,02),                                               ~
                  "DEBIT ACCOUNT",                                       ~
               at (11,30), fac(fac$( 6)), debitacct$            , ch(12),~
               at (11,49), fac(hex(8c)),  debitacctdescr$       , ch(32),~
               at (12,02),                                               ~
                  "APPLIES",                                             ~
               at (12,30), fac(fac$( 7)), applies$              , ch(06),~
               at (13,02),                                               ~
                  "CAN ANY EARNINGS BE EXEMPT?",                         ~
               at (13,30), fac(fac$( 8)), exempt$               , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),  inpmessage$           , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,20),                                               ~
                  "(3)Print Methods",                                    ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Exit Program",                                    ~
                                                                         ~
               keys(hex(0001030d0f10)),                                  ~
               key (keyhit%)

            if keyhit% <> 13 then L40730
                call "MANUAL" ("PRLDEDCT")
                goto L40260

L40730:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40260

        REM *************************************************************~
            *             I N P U T   S E C O N D   P A G E             *~
            *                                                           *~
            * INPUTS THE SECOND PAGE OF THE HEADER                      *~
            *************************************************************

            deffn'202(fieldnr%)
                  str(prgmid$,1,12) = "This Method:"
                  str(prgmid$,14,6) = method$
                  init(hex(84)) fac$()
                  on fieldnr% gosub L41220,         /* ROUTINE NAME     */~
                                    L41220,         /* TABLE REQUIRED ? */~
                                    L41220,         /* DESCRIPTION 1    */~
                                    L41235,         /* AMOUNT 1         */~
                                    L41220,         /* DESCRIPTION 2    */~
                                    L41235,         /* AMOUNT 2         */~
                                    L41220,         /* DESCRIPTION 3    */~
                                    L41235,         /* AMOUNT 3         */~
                                    L41220,         /* DESCRIPTION 4    */~
                                    L41235,         /* AMOUNT 4         */~
                                    L41235,         /* GOAL             */~
                                    L41220,         /* CDESCRIPTION 1   */~
                                    L41235,         /* CAMOUNT 1        */~
                                    L41220,         /* CDESCRIPTION 2   */~
                                    L41235,         /* CAMOUNT 2        */~
                                    L41220,         /* CDESCRIPTION 3   */~
                                    L41235,         /* CAMOUNT 3        */~
                                    L41220,         /* CDESCRIPTION 4   */~
                                    L41235,         /* CAMOUNT 4        */~
                                    L41220,         /* CDESCRIPTION 5   */~
                                    L41235,         /* CAMOUNT 5        */~
                                    L41220,         /* CDESCRIPTION 6   */~
                                    L41235          /* CAMOUNT 6        */
                     goto L41255

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$(fieldnr%) = hex(80)
                      return
L41220:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(fieldnr%) = hex(81)
                      return
L41235:           REM SET FAC'S FOR NUMERIC INPUT
                      fac$(fieldnr%) = hex(82)
                      return

L41255:     accept                                                       ~
               at (01,02),                                               ~
                  "Enter Payroll Deduction and Accrual Methods",         ~
               at (01,64),                                               ~
                  "Today:",                                              ~
               at (01,71), fac(hex(8c)),  date$                 , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(94)),  errormsg$             , ch(79),~
               at (05,02), "ROUTINE NAME",                               ~
               at (05,17), fac(fac$( 1)), routine$              , ch(08),~
               at (05,30), "IS A TAX TABLE REQUIRED ?",                  ~
               at (05,58), fac(fac$( 2)), table$                , ch(01),~
               at (06,02), "EMPLOYEE VARIABLE DESCRIPTION       AMOUNT", ~
               at (07,02), "1)",                                         ~
               at (07,5),  fac(fac$( 3)), descr$( 1)            , ch(15),~
               at (07,32), fac(fac$( 4)), amount$(1)            , ch(12),~
               at (08,02), "2)",                                         ~
               at (08,5),  fac(fac$( 5)), descr$( 2)            , ch(15),~
               at (08,32), fac(fac$( 6)), amount$(2)            , ch(12),~
               at (09,02), "3)",                                         ~
               at (09,5),  fac(fac$( 7)), descr$( 3)            , ch(15),~
               at (09,32), fac(fac$( 8)), amount$(3)            , ch(12),~
               at (10,02), "4)",                                         ~
               at (10,5),  fac(fac$( 9)), descr$( 4)            , ch(15),~
               at (10,32), fac(fac$(10)), amount$(4)            , ch(12),~
               at (11,02), "GOAL",                                       ~
               at (11,30), fac(fac$(11)), goal$                 , ch(10),~
               at (13,02), "SYSTEM CONSTANTS DESCRIPTION        AMOUNT", ~
               at (14,02), "1)",                                         ~
               at (14,5),  fac(fac$(12)), cdescr$( 1)           , ch(25),~
               at (14,32), fac(fac$(13)), camount$(1)           , ch(12),~
               at (15,02), "2)",                                         ~
               at (15,5),  fac(fac$(14)), cdescr$( 2)           , ch(25),~
               at (15,32), fac(fac$(15)), camount$(2)           , ch(12),~
               at (16,02), "3)",                                         ~
               at (16,5),  fac(fac$(16)), cdescr$( 3)           , ch(25),~
               at (16,32), fac(fac$(17)), camount$(3)           , ch(12),~
               at (17,02), "4)",                                         ~
               at (17,5),  fac(fac$(18)), cdescr$( 4)           , ch(25),~
               at (17,32), fac(fac$(19)), camount$(4)           , ch(12),~
               at (18,02), "5)",                                         ~
               at (18,5),  fac(fac$(20)), cdescr$( 5)           , ch(25),~
               at (18,32), fac(fac$(21)), camount$(5)           , ch(12),~
               at (19,02), "6)",                                         ~
               at (19,5),  fac(fac$(22)), cdescr$( 6)           , ch(25),~
               at (19,32), fac(fac$(23)), camount$(6)           , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),  inpmessage$           , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Exit Program",                                    ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

            if keyhit% <> 13 then L41570
                call "MANUAL" ("PRLDEDCT")
                goto L41255

L41570:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L41255

        REM *************************************************************~
            *               E D I T   F I R S T   P A G E               *~
            *                                                           *~
            * EDITS FIRST PAGE OF THE DOCUMENT.                         *~
            *************************************************************

            deffn'211(fieldnr%)
                  str(prgmid$,1,12) = "Last Method:"
                  str(prgmid$,14,6) = lastmethod$
                  init(hex(86)) fac$()
                  if fieldnr% <> 0 then init(hex(84)) fac$()
                  on fieldnr% gosub L42220,         /* METHOD/DEDUCTION */~
                                    L42190,         /* DEDUCTION DESCR  */~
                                    L42220,         /* SHORT DESCR      */~
                                    L42220,         /* EMPLOYEE FLAG    */~
                                    L42220,         /* CREDIT ACCOUNT   */~
                                    L42220,         /* DEBIT ACCOUNT    */~
                                    L42250,         /* APPLIES 123456   */~
                                    L42220          /* EXEMPT FLAG      */
                     goto L42290

L42190:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$(fieldnr%) = hex(80)
                      return
L42220:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(fieldnr%) = hex(81)
                      return
L42250:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      fac$(fieldnr%) = hex(82)
                      return

L42290:     accept                                                       ~
               at (01,02),                                               ~
                  "Enter Payroll Deduction and Accrual Methods",         ~
               at (01,64),                                               ~
                  "Today:",                                              ~
               at (01,71), fac(hex(8c)),  date$                 , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(94)),  errormsg$             , ch(79),~
               at (06,02),                                               ~
                  "METHOD OF DEDUCTION",                                 ~
               at (06,30), fac(fac$( 1)), method$               , ch(06),~
               at (07,02),                                               ~
                  "DEDUCTION DESCRIPTION",                               ~
               at (07,30), fac(fac$( 2)), xdescr$               , ch(30),~
               at (08,02),                                               ~
                  "DEFAULT DESCRIPTN FOR CHECKS",                        ~
               at (08,30), fac(fac$( 3)), descr$                , ch(12),~
               at (09,02),                                               ~
                  "PAID BY EMPLOYEE? (Y/N)",                             ~
               at (09,30), fac(fac$( 4)), empflag$              , ch(03),~
               at (10,02),                                               ~
                  "CREDIT (LIABILITY) ACCOUNT",                          ~
               at (10,30), fac(fac$( 5)), creditacct$           , ch(12),~
               at (10,49), fac(hex(8c)),  creditacctdescr$      , ch(32),~
               at (11,02),                                               ~
                  "DEBIT ACCOUNT",                                       ~
               at (11,30), fac(fac$( 6)), debitacct$            , ch(12),~
               at (11,49), fac(hex(8c)),  debitacctdescr$       , ch(32),~
               at (12,02),                                               ~
                  "APPLIES",                                             ~
               at (12,30), fac(fac$( 7)), applies$              , ch(06),~
               at (13,02),                                               ~
                  "CAN ANY EARNINGS BE EXEMPT?",                         ~
               at (13,30), fac(fac$( 8)), exempt$               , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),  edtmessage$           , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,20),                                               ~
                   "(5)Edit Next Page",                                  ~
               at (22,45),                                               ~
                  "(12)Delete",                                          ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Save Data",                                       ~
                                                                         ~
               keys(hex(0001050c0d0f10)),                                ~
               key (keyhit%)

            if keyhit% <> 13 then L42800
                call "MANUAL" ("PRLDEDCT")
                goto L42290

L42800:        if keyhit% <> 15 then L42850
                  call "PRNTSCRN"

                  goto L42290

L42850:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *              E D I T   S E C O N D   P A G E              *~
            *                                                           *~
            * EDITS THE SECOND PAGE OF THE DOCUMENT                     *~
            *************************************************************

            deffn'212(fieldnr%)
                  str(prgmid$,1,12) = "This Method:"
                  str(prgmid$,14,6) = method$
                  init(hex(84)) fac$()
                  if fieldnr% = 0 then init(hex(86)) fac$()
                  on fieldnr% gosub L43185,         /* ROUTINE NAME     */~
                                    L43185,         /* TABLE REQUIRED?  */~
                                    L43185,         /* DESCRIPTION 1    */~
                                    L43200,         /* AMOUNT 1         */~
                                    L43185,         /* DESCRIPTION 2    */~
                                    L43200,         /* AMOUNT 2         */~
                                    L43185,         /* DESCRIPTION 3    */~
                                    L43200,         /* AMOUNT 3         */~
                                    L43185,         /* DESCRIPTION 4    */~
                                    L43200,         /* AMOUNT 4         */~
                                    L43200,         /* GOAL             */~
                                    L43185,         /* CDESCRIPTION 1   */~
                                    L43200,         /* CAMOUNT 1        */~
                                    L43185,         /* CDESCRIPTION 2   */~
                                    L43200,         /* CAMOUNT 2        */~
                                    L43185,         /* CDESCRIPTION 3   */~
                                    L43200,         /* CAMOUNT 3        */~
                                    L43185,         /* CDESCRIPTION 4   */~
                                    L43200,         /* CAMOUNT 4        */~
                                    L43185,         /* CDESCRIPTION 5   */~
                                    L43200,         /* CAMOUNT 5        */~
                                    L43185,         /* CDESCRIPTION 6   */~
                                    L43200          /* CAMOUNT 6        */
                     goto L43220

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$(fieldnr%) = hex(80)
                      return
L43185:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(fieldnr%) = hex(81)
                      return
L43200:           REM SET FAC'S FOR NUMERIC INPUT
                      fac$(fieldnr%) = hex(82)
                      return

L43220:     accept                                                       ~
               at (01,02),                                               ~
                  "Enter Payroll Deduction and Accrual Methods",         ~
               at (01,64),                                               ~
                  "Today:",                                              ~
               at (01,71), fac(hex(8c)),  date$                 , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(94)),  errormsg$             , ch(79),~
               at (05,02), "ROUTINE NAME",                               ~
               at (05,17), fac(fac$( 1)), routine$              , ch(08),~
               at (05,30), "IS A TAX TABLE REQUIRED ?",                  ~
               at (05,58), fac(fac$( 2)), table$                , ch(01),~
               at (06,02), "EMPLOYEE VARIABLE DESCRIPTION       AMOUNT", ~
               at (07,02), "1)",                                         ~
               at (07,5),  fac(fac$( 3)), descr$( 1)            , ch(15),~
               at (07,32), fac(fac$( 4)), amount$(1)            , ch(12),~
               at (08,02), "2)",                                         ~
               at (08,5),  fac(fac$( 5)), descr$( 2)            , ch(15),~
               at (08,32), fac(fac$( 6)), amount$(2)            , ch(12),~
               at (09,02), "3)",                                         ~
               at (09,5),  fac(fac$( 7)), descr$( 3)            , ch(15),~
               at (09,32), fac(fac$( 8)), amount$(3)            , ch(12),~
               at (10,02), "4)",                                         ~
               at (10,5),  fac(fac$( 9)), descr$( 4)            , ch(15),~
               at (10,32), fac(fac$(10)), amount$(4)            , ch(12),~
               at (11,02), "GOAL",                                       ~
               at (11,30), fac(fac$(11)), goal$                 , ch(10),~
               at (13,02), "SYSTEM CONSTANTS DESCRIPTION        AMOUNT", ~
               at (14,02), "1)",                                         ~
               at (14,5),  fac(fac$(12)), cdescr$( 1)           , ch(25),~
               at (14,32), fac(fac$(13)), camount$(1)           , ch(12),~
               at (15,02), "2)",                                         ~
               at (15,5),  fac(fac$(14)), cdescr$( 2)           , ch(25),~
               at (15,32), fac(fac$(15)), camount$(2)           , ch(12),~
               at (16,02), "3)",                                         ~
               at (16,5),  fac(fac$(16)), cdescr$( 3)           , ch(25),~
               at (16,32), fac(fac$(17)), camount$(3)           , ch(12),~
               at (17,02), "4)",                                         ~
               at (17,5),  fac(fac$(18)), cdescr$( 4)           , ch(25),~
               at (17,32), fac(fac$(19)), camount$(4)           , ch(12),~
               at (18,02), "5)",                                         ~
               at (18,5),  fac(fac$(20)), cdescr$( 5)           , ch(25),~
               at (18,32), fac(fac$(21)), camount$(5)           , ch(12),~
               at (19,02), "6)",                                         ~
               at (19,5),  fac(fac$(22)), cdescr$( 6)           , ch(25),~
               at (19,32), fac(fac$(23)), camount$(6)           , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),  edtmessage$           , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (22,20),                                               ~
                  "(4)Edit First Page",                                  ~
               at (24,65),                                               ~
                  "(16)Save Data",                                       ~
                                                                         ~
               keys(hex(0001040d0f10)),                                  ~
               key (keyhit%)

            if keyhit% <> 13 then L43550
                call "MANUAL" ("PRLDEDCT")
                goto L43220

L43550:        if keyhit% <> 15 then L43570
                  call "PRNTSCRN"
                  goto L43220

L43570:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

L44000: REM *************************************************************~
            *         S C R E E N   F O R   P R I N T   M O D E         *~
            *                                                           *~
            * SCREEN FOR PRINT MODE -- GETS RANGE THEY WANT PRINTED.    *~
            *************************************************************

L44060:     accept                                                       ~
               at (01,02),                                               ~
                  "Print Deduction Methods List",                        ~
               at (01,64),                                               ~
                  "Today:",                                              ~
               at (01,71), fac(hex(8c)),  date$                 , ch(08),~
               at (02,02), fac(hex(ac)), prgmid$                , ch(79),~
               at (04,02), fac(hex(84)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "FIRST METHOD OF DEDUCTION",                           ~
               at (06,30), fac(hex(81)), firstcode$             , ch(06),~
               at (07,02),                                               ~
                  "LAST METHOD OF DEDUCTION",                            ~
               at (07,30), fac(hex(81)), lastcode$              , ch(06),~
               at (21,02), fac(hex(a4)), blankline$             , ch(79),~
               at (22,02),                                               ~
                  "(1)Input Mode",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65),                                               ~
                  "(16)Exit Program",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

            if keyhit% <> 13 then L44340
                call "MANUAL" ("PRLDEDCT")
                goto L44060

L44340:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FROM THE FIRST PAGE                            *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100,         /* METHOD OF DEDXN  */~
                                    L50200,         /* DEDXN DESCRIPTION*/~
                                    L50220,         /* DEDXN DESCRIPTION*/~
                                    L50300,         /* EMPLOYEE FLAG    */~
                                    L50400,         /* CREDIT ACCOUNT   */~
                                    L50500,         /* DEBIT ACCOUNT    */~
                                    L50600,         /* APPLIES          */~
                                    L50700          /* EXEMPT FLAG      */
                     return
L50100:     REM Test Data For Method Of Deduction
                if method$ <> " " then L50140
                   call "GETCODE" (#1, method$, " ", 0%, 0, f1%(1))
                       if f1%(1) <> 0 then L50140
                       errormsg$ = hex(00)
                       return
L50140:         gosub L30000
                if f1%(1) = 0 then return
                   return clear all
                   goto L11000
L50200:     REM Test Data For Deduction Description
                if xdescr$ = " " then errormsg$ = "This CANT be Blank"
                return
L50220:     REM Test Data For Deduction Description
                return
L50300:     REM Test Data For Employee Flag
                if str(empflag$, 1, 1) = "Y" then empflag$ = "YES"       ~
                                             else empflag$ = "NO"
                return
L50400:     REM Test Data For Credit Account
                creditacctdescr$ = " "
                if creditacct$ = " " then return
                   call "GETCODE" (#2, creditacct$, creditacctdescr$,    ~
                                         1%, 0, f1%(2))
                   if f1%(2) <> 0 then return
                      errormsg$ = "Credit Account Not On File :"         ~
                                         & creditacct$
                      return
L50500:     REM Test Data For Debit Account
                debitacctdescr$ = " "
                if debitacct$ = " " then return
                   call "GETCODE" (#2, debitacct$, debitacctdescr$,      ~
                                         1%, 0, f1%(2))
                   if f1%(2) <> 0 then return
                      errormsg$ = "Debit Account Not On File :"          ~
                                         & debitacct$
                      return
L50600:     REM Test Data For Applies Field
                if applies$ = " " then return
                if applies$ = "ONCE" then return
                      for temp% = 1 to 6
                          if str(applies$, temp%, 1) = " " then L50665
                          if str(applies$, temp%, 1) < "1" then L50675
                          if str(applies$, temp%, 1) > "6" then L50675
                          for u3% = 1 to 6
                              if u3% = temp% then L50660
                                 if str(applies$, u3%, 1) = str(applies$,~
                                         temp%, 1) then L50675
L50660:                       next u3%
L50665:                   next temp%
                     return
L50675:         errormsg$="Illegal Entry For 'APPLIES' Field :" & applies$
                return
L50700:     REM Test Data For Exempt Flag
                 if pos("YN" = exempt$) <> 0 then return
                errormsg$="Illegal Entry For 'EXEMPT' Field :" & exempt$
                return

        REM *************************************************************~
            *          T E S T   S E C O N D   P A G E   D A T A        *~
            *                                                           *~
            * TESTS DATA FROM THE SECOND PAGE.  NOTE THAT WE DO A CHECK *~
            * HERE IN THE DESCRIPTIONS FOR NONBLANKS ENTERED WHERE THERE*~
            * ARE BLANK DESCRIPTION LINES ABOVE.                        *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51175,         /* ROUTINE NAME     */~
                                    L51225,         /* TABLE REQUIRED ? */~
                                    L51240,         /* DESCRIPTION 1    */~
                                    L51280,         /* AMOUNT 1         */~
                                    L51250,         /* DESCRIPTION 2    */~
                                    L51280,         /* AMOUNT 2         */~
                                    L51260,         /* DESCRIPTION 3    */~
                                    L51280,         /* AMOUNT 3         */~
                                    L51270,         /* DESCRIPTION 4    */~
                                    L51280,         /* AMOUNT 4         */~
                                    L51355,         /* GOAL             */~
                                    L51240,         /* CDESCRIPTION 1   */~
                                    L51395,         /* CAMOUNT 1        */~
                                    L51250,         /* CDESCRIPTION 2   */~
                                    L51395,         /* CAMOUNT 2        */~
                                    L51260,         /* CDESCRIPTION 3   */~
                                    L51395,         /* CAMOUNT 3        */~
                                    L51270,         /* CDESCRIPTION 4   */~
                                    L51395,         /* CAMOUNT 4        */~
                                    L51260,         /* CDESCRIPTION 5   */~
                                    L51395,         /* CAMOUNT 5        */~
                                    L51270,         /* CDESCRIPTION 6   */~
                                    L51395          /* CAMOUNT 6        */
                    return

L51175:     REM Test Data For Routine Name
                call "GETCODE" (#3, routine$, " ", 0%, 0.62, f1%(3))
                gosub L54000
                gosub L58000
                return clear
                return
L51225:     REM Test Data For Table Required
              if pos(" YN"=table$)=0 then errormsg$ = "MUST BE 'Y' OR 'N'"
                   return
L51240:     REM Test Data For Description 1
                   return
L51250:     REM Test Data For Description 1
                      return
L51260:     REM Test Data For Description 3
                      return
L51270:     REM Test Data For Description 4
                      return
L51280:     REM Test Data For Amounts
                t% = (fieldnr%-2)/2
                if amount$(t%) = " " and descr$(t%) = " " then return
                if not(descr$(t%) = " " and amount$(t%) <> " ") then L51315
                   errormsg$ = "You Cannot Enter An Amount For A Blank De~
        ~scription!!"
                   return
L51315:         call "NUMTEST" (amount$(t%), 0, 9e7, errormsg$, -4.4, n)
                return
L51355:     REM Test Data For Goal
                call "NUMTEST" (goal$, 0, 9e7, errormsg$, -2.2, n)
                return
L51395:     REM Test Data For Constant Amounts
                t% = (fieldnr%-11)/2
                if camount$(t%) = " " and cdescr$(t%) = " " then return
                if not(cdescr$(t%) = " " and camount$(t%)<>" ") then L51430
                   errormsg$ = "You Cannot Enter An Amount For A Blank De~
        ~scription!!"
                   return
L51430:         call "NUMTEST" (camount$(t%), 0, 9e7, errormsg$, -4.4, n)
                return

L53000: REM *************************************************************~
            *       V A L I D A T E   R A N G E   T O   P R I N T       *~
            *                                                           *~
            * VALIDATES THE RANGE TO BE PRINTED.                        *~
            *************************************************************

             errormsg$ = " "
             REM Handles Case For "All" Deduction Methods
                 if firstcode$ <> "ALL" then L53120
                    init(hex(00)) firstcode$
                    init(hex(ff)) lastcode$
                    return
L53120:      REM Handles Case For Single Code
                 if lastcode$ <> " " then L53160
                    lastcode$ = firstcode$

L53160:      REM Handles Case For A Range Of Codes
                 if lastcode$ < firstcode$ then L53200
                    firstcode$ = firstcode$ addc all(hex(ff))
                    return
L53200:      REM Handles Error Message -- Last < First.
                 errormsg$ = "ILLEGAL RANGE!  Please Respecify."
                 return

L54000: REM *************************************************************~
            * V A L I D A T E   R O U T I N E   F O R   D E D U C T .   *~
            *                                                           *~
            * VALIDATES THE ROUTINE SELECTED FOR THE DEDUCTION.         *~
            *************************************************************
            table$, descr$(), amount$(), cdescr$(), camount$() = " "
            goal$       = "      0.00"

            if routine$ = "$PERHOUR" then L54240
            if routine$ = "DOL/HOUR" then L54270
            if routine$ = "AZ-IT"    then L54300
            if routine$ = "UT-IT"    then L54340
            if routine$ = "CA-IT"    then L54400
            if routine$ = "FICATYPE" then L54510
            if routine$ = "FIT"      then L54550
            if routine$ = "FLAT"     then L54600
            if routine$ = "SYSFLAT"  then L54630
            if routine$ = "%_OF_NET" then L54670
            if routine$ = "GA-IT"    then L54710
            if routine$ = "IA-IT"    then L56210
            if routine$ = "ID-IT"    then L54750
            if routine$ = "OR-IT"    then L54810
            if routine$ = "VA-IT"    then L54860
            if routine$ = "IL-IT"    then L54910
            if routine$ = "KS-IT"    then L54950
            if routine$ = "FICA"     then L55010
            if routine$ = "LIKEFICA" then L55010
            if routine$ = "FICAEXMP" then L55010
            if routine$ = "MA-IT"    then L55050
            if routine$ = "MD-IT"    then L55150
            if routine$ = "ME-IT"    then L55230
            if routine$ = "MO-IT"    then L56320
            if routine$ = "OH-IT"    then L55305
            if routine$ = "OK-IT"    then L56400
            if routine$ = "WITHHOLD" then L55345
            if routine$ = "401K"     then L55375
            if routine$ = "401K+"    then L55425
            if routine$ = "QBENS"    then L55475
            if routine$ = "NC-IT"    then L55515
            if routine$ = "TABLENET" then L55565
            if routine$ = "NC-IT1"   then L55605
            if routine$ = "SYSTMPCT" then L55685
            if routine$ = "AL-IT"    then L55735
            if routine$ = "KT-IT"    then L55825
            if routine$ = "SC-IT"    then L55955
            if routine$ = "401K+SYS" then L55895
            if routine$ = "CT-IT"    then L56025
            if routine$ = "VT-IT"    then L56105
            if routine$ = "AR-IT"    then L56130
            errormsg$ = "INVALID ROUTINE! PLEASE ENTER A VALID ROUTINE."
            return

L54240: REM INITIALIZE VARIABLES FOR ROUTINE $PERHOUR
            table$      = "N"
            cdescr$(1)  = "CENTS PER HOUR"
            camount$(1) = "      0.0000"
            return

L54270: REM INITIALIZE VARIABLES FOR ROUTINE DOL/HOUR
            table$      = "N"
            cdescr$(1)  = "DOLLARS PER HOUR"
            camount$(1) = "        0.00"
            return

L54300: REM INITIALIZE VARIABLES FOR ROUTINE AZ-IT
            table$      = "N"
            descr$(1)   = "% W/H RATE"
            descr$(2)   = "AMOUNT TO ADD"
            amount$(1)  = "      0.0000"
            amount$(2)  = "      0.0000"
            return

L54340: REM INITIALIZE VARIABLES FOR ROUTINE UT-IT
            table$      = "Y"
            descr$(1)   = "# OF EXEMPTIONS"
            descr$(2)   = "AMOUNT TO ADD"
            descr$(3)   = "%OF GROSS TO AD"
            amount$(1)  = "      0.0000"
            amount$(2)  = "      0.0000"
            amount$(3)  = "      0.0000"
            cdescr$(1)  = "% FIT DEDUCTION"
            camount$(1) = "      0.0000"
            return

L54400: REM INITIALIZE VARIABLES FOR ROUTINE CA-IT
            table$      = "Y"
            descr$(1)   = "# OF REG. EXEMPTIONS"
            descr$(2)   = "EST. DEDUCTIONS"
            descr$(3)   = "AMOUNT TO ADD"
            amount$(1)  = "      0.0000"
            amount$(2)  = "      0.0000"
            amount$(3)  = "      0.0000"
            cdescr$(1)  = "LOW STANDARD DED."
            cdescr$(2)  = "HIGH STANDARD DED."
            cdescr$(3)  = "TAX CREDIT PER EX."
            cdescr$(4)  = "EST. DED. CREDIT"
            cdescr$(5)  = "LOW INCOME EXMP-LOW"
            cdescr$(6)  = "LOW INCOME EXMP-HIGH"
            camount$(1) = "      0.0000"
            camount$(2) = "      0.0000"
            camount$(3) = "      0.0000"
            camount$(4) = "      0.0000"
            camount$(5) = "      0.0000"
            camount$(6) = "      0.0000"
            return

L54510: REM INITIALIZE VARIABLES FOR ROUTINE FICATYPE
            table$      = "N"
            descr$(1)   = "% TAX RATE"
            descr$(2)   = "WAGE LIMIT"
            amount$(1)  = "      0.0000"
            amount$(2)  = "      0.0000"
            return

L54550: REM INITIALIZE VARIABLES FOR ROUTINE FIT
            table$      = "Y"
            descr$(1)   = "# OF EXEMPTIONS"
            descr$(2)   = "AMOUNT TO ADD"
            descr$(3)   = "% GROSS TO ADD"
            amount$(1)  = "      0.0000"
            amount$(2)  = "      0.0000"
            amount$(3)  = "      0.0000"
            return

L54600: REM INITIALIZE VARIABLES FOR ROUTINE FLAT
            table$      = "N"
            descr$(1)   = "DOLLAR AMOUNT"
            amount$(1)  = "      0.0000"
            return

L54630: REM INITIALIZE VARIABLES FOR ROUTINE SYSFLAT
            table$      = "N"
            cdescr$(1)  = "DOLLAR AMOUNT"
            cdescr$(2)  = "ANNUAL GOAL"
            camount$(1) = "      0.0000"
            camount$(2) = "      0.0000"
            return

L54670: REM INITIALIZE VARIABLES FOR ROUTINE %_OF_NET
            table$      = "N"
            descr$(1)   = "PERCENT OF NET"
            descr$(2)   = "MINIMUM NET REQ"
            amount$(1)  = "      0.0000"
            amount$(2)  = "      0.0000"
            return

L54710: REM INITIALIZE VARIABLES FOR ROUTINE GA-IT
            table$      = "Y"
            descr$(1)   = "# OF EXEMPTIONS"
            descr$(2)   = "ADDT'L W/H AMNT"
            amount$(1)  = "      0.0000"
            amount$(2)  = "      0.0000"
            return

L54750: REM INITIALIZE VARIABLES FOR ROUTINE ID-IT
            table$      = "Y"
            descr$(1)   = "# OF EXEMPTIONS"
            descr$(2)   = "ADDT'L W/H AMNT"
            amount$(1)  = "      0.0000"
            amount$(2)  = "      0.0000"
            cdescr$(1)  = "SINGLE ZERO BRACKET AMNT."
            cdescr$(2)  = "MARRIED ZERO BRACKET AMNT"
            camount$(1) = "      0.0000"
            camount$(2) = "      0.0000"
            return

L54810: REM INITIALIZE VARIABLES FOR ROUTINE OR-IT
            table$      = "Y"
            descr$(1)   = "# OF EXEMPTIONS"
            descr$(2)   = "ADDT'L W/H AMNT"
            amount$(1)  = "      0.0000"
            amount$(2)  = "      0.0000"
            cdescr$(1)  = "MAX ANNUAL FIT ALLOWANCE"
            cdescr$(2)  = "PERSONAL ALLOWANCE      "
            camount$(1) = "      0.0000"
            camount$(2) = "      0.0000"
            return

L54860: REM INITIALIZE VARIABLES FOR ROUTINE VA-IT
            table$      = "Y"
            descr$(1)   = "# OF EXEMPTIONS"
            descr$(2)   = "ADDT'L W/H AMNT"
            amount$(1)  = "      0.0000"
            amount$(2)  = "      0.0000"
            cdescr$(1)  = "ZERO BRACKET AMOUNT"
            camount$(1) = "      0.0000"
            return

L54910: REM INITIALIZE VARIABLES FOR ROUTINE IL-IT
            table$      = "Y"
            descr$(1)   = "# OF EXEMPTIONS"
            descr$(2)   = "ADDT'L W/H AMNT"
            amount$(1)  = "      0.0000"
            amount$(2)  = "      0.0000"
            return

L54950: REM INITIALIZE VARIABLES FOR ROUTINE KS-IT
            table$      = "Y"
            descr$(1)   = "# OF EXEMPTIONS"
            descr$(2)   = "ADDT'L W/H AMNT"
            amount$(1)  = "      0.0000"
            amount$(2)  = "      0.0000"
            cdescr$(1)  = "% OF 'FIT' ALLNCE MARRIED"
            cdescr$(2)  = "% OF 'FIT' ALLNCE SINGLE"
            camount$(1) = "      0.0000"
            camount$(2) = "      0.0000"
            return

L55010: REM INITIALIZE VARIABLES FOR ROUTINE LIKEFICA
            table$      = "N"
            cdescr$(1)  = "TAX RATE PERCENTAGE"
            cdescr$(2)  = "WAGE LIMIT"
            camount$(1) = "      0.0000"
            camount$(2) = "      0.0000"
            return

L55050: REM INITIALIZE VARIABLES FOR ROUTINE MA-IT
            table$      = "N"
            descr$(1)   = "# OF EXEMPTIONS"
            descr$(2)   = "ADDT'L W/H AMNT"
            descr$(3)   = "EMPLOYEE BLIND?"
            amount$(1)  = "      0.0000"
            amount$(2)  = "      0.0000"
            amount$(3)  = "      0.0000"
            cdescr$(1)  = "MAXIMUM FICA DEDUCTION"
            cdescr$(2)  = "SINGLE EXEMPTION AMOUNT"
            cdescr$(3)  = "MULTIPLE DEP. EXEMPT AMNT"
            cdescr$(4)  = "FLAT DEP. EXEMPT. AMNT."
            cdescr$(5)  = "WITHHOLDING PERCENTAGE"
            camount$(1) = "      0.0000"
            camount$(2) = "      0.0000"
            camount$(3) = "      0.0000"
            camount$(4) = "      0.0000"
            camount$(5) = "      0.0000"
            return

L55150: REM INITIALIZE VARIABLES FOR ROUTINE MD-IT
            table$      = "Y"
            descr$(1)   = "# OF EXEMPTIONS"
            descr$(2)   = "LOCAL W/H RATE"
            descr$(3)   = "ADDT'L W/H AMNT"
            amount$(1)  = "      0.0000"
            amount$(2)  = "      0.0000"
            amount$(3)  = "      0.0000"
            cdescr$(1)  = "STANDARD DEDUCTION %"
            cdescr$(2)  = "STANDARD DEDUCTION LIMIT"
            cdescr$(3)  = "STANDARD DED. MINIMUM"
            camount$(1) = "      0.0000"
            camount$(2) = "      0.0000"
            camount$(3) = "      0.0000"
            return

L55230: REM INITIALIZE VARIABLES FOR ROUTINE ME-IT
            table$      = "Y"
            descr$(1)   = "# OF EXEMPTIONS"
            descr$(2)   = "ADDT'L W/H AMNT"
            descr$(3)   = "% W/H FOR LOCAL"
            amount$(1)  = "      0.0000"
            amount$(2)  = "      0.0000"
            amount$(3)  = "      0.0000"
            cdescr$(1)  = "STD DEDUCTION PERCENTAGE"
            cdescr$(2)  = "STD DEDUCTION FOR MARRIED"
            cdescr$(3)  = "STD DEDUCTION FOR SINGLE"
            camount$(1) = "      0.0000"
            camount$(2) = "      0.0000"
            camount$(3) = "      0.0000"
            return
L55305: REM INITIALIZE VARIABLES FOR ROUTINE OH-IT
            table$      = "Y"
            descr$(1)   = "# OF EXEMPTIONS"
            descr$(2)   = "ADDT'L W/H AMNT"
            amount$(1)  = "      0.0000"
            amount$(2)  = "      0.0000"
            return

L55345: REM INITIALIZE VARIABLES FOR ROUTINE WITHHOLD
            table$      = "N"
            descr$(1)   = "% W/H RATE"
            amount$(1)  = "      0.0000"
            return

L55375: REM INITIALIZE VARIABLES FOR ROUTINE 401K
            table$      = "N"
            descr$(1)   = "% OF GROSS"
            amount$(1)  = "      0.0000"
            descr$(2)   = "WAGE LIMIT"
            amount$(2)  = "      0.0000"
            descr$(3)   = "ADDT'L W/H AMNT"
            amount$(3)  = "      0.0000"
            return

L55425: REM INITIALIZE VARIABLES FOR ROUTINE 401K+
            table$      = "N"
            descr$(1)   = "PERCENT MATCHED"
            amount$(1)  = "      0.0000"
            descr$(2)   = "ADDTIONAL AMNT"
            amount$(2)  = "      0.0000"
            descr$(3)   = "YEARLY LIMIT"
            amount$(3)  = "      0.0000"
            return

L55475: REM INITIALIZE VARIABLES FOR ROUTINE QBENS
            table$      = "N"
            descr$(1)   = "% OF GROSS"
            amount$(1)  = "      0.0000"
            descr$(2)   = "ADDT'L FLAT AMT"
            amount$(2)  = "      0.0000"
            return

L55515: REM INITIALIZE VARIABLES FOR ROUTINE "NC-IT"
            table$      = "Y"
            descr$(1)   = "PERS. EXEMPTION."
            amount$(1)  = "      0.0000"
            cdescr$(1)  = "STD. DED. RATE"
            camount$(1) = "      0.0000"
            cdescr$(2)  = "STD. DED. AMT."
            camount$(2) = "      0.0000"
            return

L55565: REM INITIALIZE VARIABLES FOR ROUTINE "TABLENET"
            table$      = "Y"
            descr$(1)   = "# OF EXEMPTIONS"
            descr$(2)   = "ADDT'L W/H AMNT"
            amount$(1)  = "      0.0000"
            amount$(2)  = "      0.0000"
            return

L55605: REM INITIALIZE VARIABLES FOR ROUTINE "NC-IT1"
            table$      = "Y"
            descr$(1)   = "# OF EXEMPTIONS"
            descr$(2)   = "ADDT'L W/H AMNT"
            amount$(1)  = "      0.0000"
            amount$(2)  = "      0.0000"
            cdescr$(1)  = "STD. DED. 'M'"
            cdescr$(2)  = "STD. DED. 'S'"
            cdescr$(3)  = "STD. DED. 'H'"
            cdescr$(4)  = "STD. DED. 'Q'"
            cdescr$(5)  = "STD. DED. 'J'"
            cdescr$(6)  = "STD. DED. 'A'"
            camount$(1) = "      0.0000"
            camount$(2) = "      0.0000"
            camount$(3) = "      0.0000"
            camount$(4) = "      0.0000"
            camount$(5) = "      0.0000"
            camount$(6) = "      0.0000"
            return

L55685: REM INITIALIZE VARIABLES FOR ROUTINE "SYSTMPCT"
            table$      = "N"
            descr$(1)   = "% OF GROSS"
            amount$(1)  = "      0.0000"
            cdescr$(1)  = "% OF GROSS"
            cdescr$(2)  = "% MODIFIER"
            camount$(1) = "      0.0000"
            camount$(2) = "      0.0000"
            return

L55735: REM INITIALIZE VARIABLES FOR ROUTINE "AL-IT"
            table$      = "Y"
            descr$(1)   = "PERS. EXEMPTION."
            amount$(1)  = "      0.0000"
            descr$(2)   = "# OF DEPENDENTS  "
            amount$(2)  = "      0.0000"
            descr$(3)   = "ADDT'L W/H AMNT  "
            amount$(3)  = "      0.0000"
            cdescr$(1)  = "STD DED LIMIT-SINGLE"
            camount$(1) = "      0.0000"
            cdescr$(2)  = "STD DED LIMIT-MARRIED"
            camount$(2) = "      0.0000"
            cdescr$(3)  = "STD DED PERCENTAGE"
            camount$(3) = "      0.0000"
            cdescr$(4)  = "SINGLE EXEMPT AMT"
            camount$(4) = "      0.0000"
            cdescr$(5)  = "MARRIED EXEMPT AMT"
            camount$(5) = "      0.0000"
            cdescr$(6)  = "FIT ALLOWANCE %   "
            camount$(6) = "      0.0000"
            return

L55825: REM INITIALIZE VARIABLES FOR ROUTINE "KT-IT"
            table$      = "Y"
            descr$(1)   = "PERS. EXEMPTION."
            amount$(1)  = "      0.0000"
            descr$(2)   = "ADDT'L W/H AMNT  "
            amount$(2)  = "      0.0000"
            cdescr$(1)  = "STD DED AMOUNT"
            camount$(1) = "      0.0000"
            cdescr$(2)  = "EXEMPTION CREDIT"
            camount$(2) = "      0.0000"
            cdescr$(3)  = "FIT ADD ON %    "
            camount$(3) = "      0.0000"
            return

L55895: REM INITIALIZE VARIABLES FOR ROUTINE 401K+SYS
            table$      = "N"
            cdescr$(1)  = "COMPANY MATCHING %"
            cdescr$(2)  = "MAX MATCH %       "
            cdescr$(3)  = "ADDT'L AMOUNT     "
            cdescr$(4)  = "COMPANY LIMIT     "
            camount$(1) = "      0.0000"
            camount$(2) = "      0.0000"
            camount$(3) = "      0.0000"
            camount$(4) = "      0.0000"
            return

L55955: REM INITIALIZE VARIABLES FOR ROUTINE "SC-IT"
            table$      = "Y"
            descr$(1)   = "# OF DEPENDENTS "
            descr$(2)   = "ADDT'L W/H AMNT "
            amount$(1)  = "      0.0000"
            amount$(2)  = "      0.0000"
            cdescr$(1)  = "STD DED. PERCENT"
            cdescr$(2)  = "MAX AMOUNT 1 DEP"
            cdescr$(3)  = "MAX AMT 1+ DEP  "
            camount$(1) = "      0.0000"
            camount$(2) = "      0.0000"
            camount$(3) = "      0.0000"
            return

L56025: REM INITIALIZE VARIABLES FOR ROUTINE CT-IT
            table$      = "Y"
            descr$(1)   = "ADDT'L W/H AMNT "
            amount$(1)  = "      0.0000"
            cdescr$(1)  = "TAX PERCENT"
            cdescr$(2)  = "SINGLE (A) ANN. EXMPT MAX"
            cdescr$(3)  = "H/HHLD (B) ANN. EXMPT MAX"
            cdescr$(4)  = "MARRIED(C) ANN. EXMPT MAX"
            cdescr$(5)  = "OTHER  (D) ANN. EXMPT MAX"
            camount$(1) = "      0.0000"
            camount$(2) = "      0.0000"
            camount$(3) = "      0.0000"
            camount$(4) = "      0.0000"
            camount$(5) = "      0.0000"
            return

L56105: REM INITIALIZE VARIABLES FOR ROUTINE VT-IT
            table$      = "Y"
            descr$(1)   = "ADDT'L W/H AMNT "
            amount$(1)  = "      0.0000"
            return

L56130: REM INITIALIZE VARIABLES FOR ROUTINE AR-IT
            table$      = "Y"
            descr$(1)   = "NO. EXEMPTIONS  "
            descr$(2)   = "NO. DEPENDENTS  "
            descr$(3)   = "ADDT'L W/H AMNT "
            amount$(1)  = "      0.0000"
            amount$(2)  = "      0.0000"
            amount$(3)  = "      0.0000"
            cdescr$(1)  = "MAX DEDUCTION MARRIED (M)"
            cdescr$(2)  = "MAX DEDUCTION JOINT   (J)"
            cdescr$(3)  = "MAX DEDUCTION SINGLE  (S)"
            cdescr$(4)  = "STD DEDUCTION PERCENT    "
            cdescr$(5)  = "EXEMPTION CREDIT         "
            cdescr$(6)  = "DEPENDENT CREDIT         "
            camount$(1) = "      0.0000"
            camount$(2) = "      0.0000"
            camount$(3) = "      0.0000"
            camount$(4) = "      0.0000"
            camount$(5) = "      0.0000"
            camount$(6) = "      0.0000"
            return

L56210: REM INITIALIZE VARIABLES FOR ROUTINE IA-IT
            table$      = "Y"
            descr$(1)   = "NO. EXEMPTIONS  "
            descr$(2)   = "ADDT'L W/H AMNT "
            amount$(1)  = "      0.0000"
            amount$(2)  = "      0.0000"
            cdescr$(1)  = "STD DEDUCTION 0/1 EXEMPT "
            cdescr$(2)  = "STD DEDUCTION 2+  EXEMPT "
            cdescr$(3)  = "FIT ALLOWANCE %          "
            cdescr$(4)  = "EXEMPTION CREDIT (HIGH)  "
            cdescr$(5)  = "EXEMPTION CREDIT (LOW)   "
            cdescr$(6)  = "MAX EXEMPTIONS (HIGH)    "
            camount$(1) = "      0.0000"
            camount$(2) = "      0.0000"
            camount$(3) = "      0.0000"
            camount$(4) = "      0.0000"
            camount$(5) = "      0.0000"
            camount$(6) = "      0.0000"
            return

L56320: REM INITIALIZE VARIABLES FOR ROUTINE MO-IT
            table$      = "Y"
            descr$(1)   = "NO. EXEMPTIONS  "
            descr$(2)   = "ADDT'L W/H AMNT "
            amount$(1)  = "      0.0000"
            amount$(2)  = "      0.0000"
            cdescr$(1)  = "STD DEDUCTION MARRIED    "
            cdescr$(2)  = "STD DEDUCTION SINGLE     "
            cdescr$(3)  = "FIT ALLOWANCE %          "
            cdescr$(4)  = "ADDITIONAL DEP. ALLOWANCE"
            cdescr$(5)  = "ADD ALLOW. 'M' (S = M-1) "
            cdescr$(6)  = "MAX FEDERAL DEDUCTION    "
            camount$(1) = "      0.0000"
            camount$(2) = "      0.0000"
            camount$(3) = "      0.0000"
            camount$(4) = "      0.0000"
            camount$(5) = "      0.0000"
            camount$(6) = "      0.0000"
            return

L56400: REM INITIALIZE VARIABLES FOR ROUTINE OK-IT
            table$      = "Y"
            descr$(1)   = "NO. EXEMPTIONS  "
            descr$(2)   = "ADDT'L W/H AMNT "
            amount$(1)  = "      0.0000"
            amount$(2)  = "      0.0000"
            cdescr$(1)  = "STANDARD DEDUCTIONS %    "
            cdescr$(2)  = "MAXIMUM STANDARD DED     "
            cdescr$(3)  = "MINIMUM STANDARD DED     "
            cdescr$(4)  = "EXEMPTION CREDIT AMOUNT  "
            cdescr$(5)  = "FIXED STANDARD DEDUCTION "
            cdescr$(6)  = "MAXIMUM ANNUAL WAGE BREAK"
            camount$(1) = "      0.0000"
            camount$(2) = "      0.0000"
            camount$(3) = "      0.0000"
            camount$(4) = "      0.0000"
            camount$(5) = "      0.0000"
            camount$(6) = "      0.0000"
            return

L58000: REM *************************************************************~
            * S E T S   F I E L D S   T O   B E   E N A B L E D     .   *~
            *                                                           *~
            * SETS THE FIELDS TO BE ENABLED BASED UPON THE ROUTINE      *~
            *************************************************************
            c%, e%, g% = 0

            if routine$ = "$PERHOUR" then L58340
            if routine$ = "DOL/HOUR" then L58340
            if routine$ = "AZ-IT"    then L58380
            if routine$ = "UT-IT"    then L58420
            if routine$ = "CA-IT"    then L58460
            if routine$ = "FICATYPE" then L58510
            if routine$ = "FIT"      then L58550
            if routine$ = "FLAT"     then L58590
            if routine$ = "SYSFLAT"  then L58640
            if routine$ = "%_OF_NET" then L58680
            if routine$ = "GA-IT"    then L58740
            if routine$ = "IA-IT"    then L59860
            if routine$ = "ID-IT"    then L58780
            if routine$ = "OR-IT"    then L58821
            if routine$ = "VA-IT"    then L58830
            if routine$ = "IL-IT"    then L58880
            if routine$ = "KS-IT"    then L58912
            if routine$ = "FICA"     then L58920
            if routine$ = "LIKEFICA" then L58920
            if routine$ = "FICAEXMP" then L58920
            if routine$ = "MA-IT"    then L58960
            if routine$ = "MD-IT"    then L59010
            if routine$ = "ME-IT"    then L59060
            if routine$ = "MO-IT"    then L59910
            if routine$ = "OH-IT"    then L59100
            if routine$ = "OK-IT"    then L59955
            if routine$ = "WITHHOLD" then L59140
            if routine$ = "401K"     then L59190
            if routine$ = "401K+"    then L59240
            if routine$ = "QBENS"    then L59280
            if routine$ = "NC-IT"    then L59330
            if routine$ = "TABLENET" then L59380
            if routine$ = "NC-IT1"   then L59430
            if routine$ = "SYSTMPCT" then L59480
            if routine$ = "AL-IT"    then L59530
            if routine$ = "KT-IT"    then L59580
            if routine$ = "401K+SYS" then L59630
            if routine$ = "SC-IT"    then L59670
            if routine$ = "CT-IT"    then L59720
            if routine$ = "VT-IT"    then L59770
            if routine$ = "AR-IT"    then L59810
            return

L58340: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE $PERHOUR
            c% = 1%
            return

L58380: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE AZ-IT
            e% = 2%
            return

L58420: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE UT-IT
            e% = 3%
            c% = 1%
            return

L58460: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE CA-IT
            e% = 3%
            c% = 6%
            return

L58510: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE FICATYPE
            e% = 2%
            return

L58550: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE FIT
            e% = 3%
            return

L58590: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE FLAT
            e% = 1%
            g% = 1%
            return

L58640: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE SYSFLAT
            c% = 2%
            return

L58680: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE %_OF_NET
            g% = 1%
            e% = 2%
            return

L58740: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE GA-IT
            e% = 2%
            return

L58780: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE ID-IT
            e% = 2%
            c% = 2%
            return

L58821: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE OR-IT
            e% = 2%
            c% = 2%
            return

L58830: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE VA-IT
            e% = 2%
            c% = 1%
            return

L58880: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE IL-IT
            e% = 2%
            return

L58912: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE KS-IT
            e% = 2%
            c% = 2%
            return

L58920: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE LIKEFICA
            c% = 2%
            return

L58960: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE MA-IT
            e% = 3%
            c% = 5%
            return

L59010: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE MD-IT
            e% = 3%
            c% = 3%
            return

L59060: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE ME-IT
            e% = 3%
            c% = 3%
            return

L59100: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE OH-IT
            e% = 2%
            return

L59140: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE WITHHOLD
            g% = 1%
            e% = 1%
            return

L59190: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE 401K
            e% = 3%
            g% = 1%
            return

L59240: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE 401K+
            e% = 3%
            return

L59280: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE QBENS
            e% = 2%
            g% = 1%
            return

L59330: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE NC-IT
            e% = 1%
            c% = 2%
            return

L59380: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE TABLENET
            e% = 1%
            g% = 1%
            return

L59430: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE NC-IT1
            e% = 2%
            c% = 6%
            return

L59480: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE SYSTMPCT
            e% = 1%
            c% = 2%
            return

L59530: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE AL-IT
            e% = 3%
            c% = 6%
            return

L59580: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE KT-IT
            e% = 2%
            c% = 3%
            return

L59630: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE 401K+SYS
            c% = 4%
            return

L59670: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE SC-IT
            e% = 2%
            c% = 3%
            return

L59720: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE CT-IT
            e% = 1%
            c% = 5%
            return

L59770: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE VT-IT
            e% = 1%
            return

L59810: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE AR-IT
            e% = 3%
            c% = 6%
            return

L59860: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE IA-IT
            e% = 2%
            c% = 6%
            return

L59910: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE MO-IT
            e% = 2%
            c% = 6%
            return

L59955: REM SET VALUE FOR FIELDS TO BE ENABLED FOR ROUTINE OK-IT
            e% = 2%
            c% = 6%
            return

L60000: REM *************************************************************~
            *       P R I N T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * PRINT MODE MAIN PROGRAM DRIVES PRINT MODE FOR THE DDT.    *~
            *************************************************************

            colsdone% = 0
            mat linenumber% = con

            REM LOOP THROUGH COMPUTING AND PRINTING LINES UNTIL DONE.
L60100:         for column% = 1 to 3
                    on column% gosub L60280, L60490, L60760
                    next column%
                if colsdone% < 3 then L60200        /* IF NOT YET DONE  */
                   gosub L61510
                   REM HANDLE TAG LINE ("+-----+---+...")
                       if tagprinted% = 1 then return
                          print using L61960        /* SEPARATOR LINE   */
                          tagprinted% = 1
                          return                   /* NEXT PART        */
L60200:         gosub L61510              /* PAGE HEADING, IF NECCESSARY*/
                print using L62000, prtmethod$, prtdescr$, prtempflag$,   ~
                            prtcreditacct$, prtdebitacct$, prtacctdescr$,~
                            prtamtdescr$, prtamt$, prtgoal$, prtapplies$,~
                            prtroutine$, prttable$
                tagprinted% = 0
                goto L60100

L60280:     REM HANDLES FIRST  COLUMN--METHOD, DESCRIPTION, GOAL, ETC.
                on linenumber%(1) gosub L60310, L60420
                   return
L60310:         REM HANDLES FIRST  CASE--PRINT INFORMATION.
                    prtmethod$ = method$
                    prtdescr$  = descr$
                    if empflag$ = "Y" then prtempflag$ = "YES"           ~
                                      else prtempflag$ = "NO"
                    if goal = 0 then prtgoal$ = " "                      ~
                                else call "CONVERT" (goal, 2.2, prtgoal$)
                    prtroutine$ = routine$
                    prtapplies$ = applies$
                    prttable$   = table$
                    linenumber%(1) = 2
                    return
L60420:         REM HANDLES SECOND CASE--ZAP VARIABLES
                    prtmethod$, prtdescr$, prtempflag$, prtgoal$,        ~
                    prtroutine$, prtapplies$, prttable$ = " "
                    linenumber%(1) = 3
                    colsdone% = colsdone% + 1
                    return

L60490:     REM HANDLES SECOND COLUMN--ACCOUNT NUMBERS
                on linenumber%(2) gosub L60520, L60610, L60700
                   return
L60520:         REM HANDLES FIRST  CASE--NONBLANK CREDIT ACCOUNT
                    if creditacct$ = " " then L60610
                    prtcreditacct$ = creditacct$
                    prtdebitacct$  = " "
                    call "GETCODE" (#2, creditacct$, prtacctdescr$,      ~
                                                   0%, 99, f1%(2))
                    if f1%(2) = 0 then prtacctdescr$="Account Not On File"
                    linenumber%(2) = 2
                    return
L60610:         REM HANDLES SECOND CASE--NONBLANK DEBIT  ACCOUNT
                    if debitacct$  = " " then L60700
                    prtdebitacct$  = debitacct$
                    prtcreditacct$ = " "
                    call "GETCODE" (#2, debitacct$, prtacctdescr$,       ~
                                                   0%, 99, f1%(2))
                    if f1%(2) = 0 then prtacctdescr$="Account Not On File"
                    linenumber%(2) = 3
                    return
L60700:         REM HANDLES THIRD  CASE--ZAP VARIABLES
                    prtcreditacct$, prtdebitacct$, prtacctdescr$ = " "
                    linenumber%(2) = 4
                    colsdone% = colsdone% + 1
                    return

L60760:     REM HANDLES THIRD  COLUMN--AMOUNT/DESCRIPTION FIELDS
                on linenumber%(3) gosub L60790, L60850, L60910, L60970,L61030,~
                                 L61100, L61170, L61240, L61310, L61380, L61450
                   return
L60790:         REM HANDLES FIRST  CASE--NONBLANK AMOUNT/DESCRIPTION 1
                    if descr$(1) = " " then L60850
                    prtamtdescr$ = descr$(1)
                    call "CONVERT" (amount(1), 2.4, prtamt$)
                    linenumber%(3) = 2
                    return
L60850:         REM HANDLES SECOND CASE--NONBLANK AMOUNT/DESCRIPTION 2
                    if descr$(2) = " " then L60910
                    prtamtdescr$ = descr$(2)
                    call "CONVERT" (amount(2), 2.4, prtamt$)
                    linenumber%(3) = 3
                    return
L60910:         REM HANDLES THIRD  CASE--NONBLANK AMOUNT/DESCRIPTION 3
                    if descr$(3) = " " then L60970
                    prtamtdescr$ = descr$(3)
                    call "CONVERT" (amount(3), 2.4, prtamt$)
                    linenumber%(3) = 4
                    return
L60970:         REM HANDLES FOURTH CASE--NONBLANK AMOUNT/DESCRIPTION 4
                    if descr$(4) = " " then L61030
                    prtamtdescr$ = descr$(4)
                    call "CONVERT" (amount(4), 2.4, prtamt$)
                    linenumber%(3) = 5
                    return
L61030:         REM HANDLES FIFTH CASE--NONBLANK CAMOUNT/DESCRIPTION 1
                    if cdescr$(1) = " " then L61100
                    prtamtdescr$ = cdescr$(1)
                    call "CONVERT" (camount(1), 2.4, prtamt$)
                    prtgoal$ = "(CONSTANT)"
                    linenumber%(3) = 6
                    return
L61100:         REM HANDLES SIXTH CASE--NONBLANK CAMOUNT/DESCRIPTION 2
                    if cdescr$(2) = " " then L61170
                    prtamtdescr$ = cdescr$(2)
                    call "CONVERT" (camount(2), 2.4, prtamt$)
                    prtgoal$ = "(CONSTANT)"
                    linenumber%(3) = 7
                    return
L61170:         REM HANDLES SEVENTH CASE--NONBLANK CAMOUNT/DESCRIPTION 3
                    if cdescr$(3) = " " then L61240
                    prtamtdescr$ = cdescr$(3)
                    call "CONVERT" (camount(3), 2.4, prtamt$)
                    prtgoal$ = "(CONSTANT)"
                    linenumber%(3) = 8
                    return
L61240:         REM HANDLES EIGHTH CASE--NONBLANK CAMOUNT/DESCRIPTION 4
                    if cdescr$(4) = " " then L61310
                    prtamtdescr$ = cdescr$(4)
                    call "CONVERT" (camount(4), 2.4, prtamt$)
                    prtgoal$ = "(CONSTANT)"
                    linenumber%(3) = 9
                    return
L61310:         REM HANDLES NINTH CASE--NONBLANK CAMOUNT/DESCRIPTION 5
                    if cdescr$(5) = " " then L61380
                    prtamtdescr$ = cdescr$(5)
                    call "CONVERT" (camount(5), 2.4, prtamt$)
                    prtgoal$ = "(CONSTANT)"
                    linenumber%(3) = 10
                    return
L61380:         REM HANDLES TENTH CASE--NONBLANK CAMOUNT/DESCRIPTION 6
                    if cdescr$(6) = " " then L61450
                    prtamtdescr$ = cdescr$(6)
                    call "CONVERT" (camount(6), 2.4, prtamt$)
                    prtgoal$ = "(CONSTANT)"
                    linenumber%(3) = 11
                    return
L61450:         REM HANDLES ELEVENTH (LAST) CASE--ZAP VARIABLES
                    prtamt$, prtamtdescr$, prtgoal$ = " "
                    linenumber%(3) = 12
                    colsdone% = colsdone% + 1
                    return

L61510:     REM PAGE TITLE SUBROUTINE.
                select printer(134)
                line% = line% + 1
                if line% < 60 then return
                   if page% = 0 then L61600
                      if tagprinted% = 1 then L61600
                         print using L61960
                         tagprinted% = 1
L61600:            print page
                   page% = page% + 1
                   print using L61730, date$, cmpname$, page%
                   print using L61760, prgm$, title$, rptid$
                   print
                   print using L61800
                   print using L61840
                   print using L61880
                   print using L61920
                   print using L61960
                   line% = 9              /* SET STARTING LINE ON PAGE  */
                   return

L61730: %########                           #############################~
        ~###############################                         PAGE: ###~
        ~###
L61760: %########                             ###########################~
        ~##############################                        REPORT: ###~
        ~###

L61800: %+------+------------+---+---------------------------------------~
        ~-----------+--------------------------+----------+------+--------~
        ~+-+

L61840: %!      !            !EMP!       A C C O U N T  I N F O R M A T I~
        ~ O N       ! A M O U N T  F I E L D S !          !      !ROUTINE ~
        ~!T!

L61880: %!METHOD!DESCRIPTION !LOY+---------+---------+-------------------~
        ~-----------+---------------+----------+   GOAL   ! APPLY!  NAME  ~
        ~!B!

L61920: %!      !            !EE?!  CREDIT !  DEBIT  !    D E S C R I P T~
        ~ I O N     !  DESCRIPTION  !  AMOUNT  !          !      !        ~
        ~!L!

L61960: %+------+------------+---+---------+---------+-------------------~
        ~-----------+---------------+----------+----------+------+--------~
        ~+-+

L62000: %!######!############!###!#########!#########!###################~
        ~###########!###############!##########!##########!######!########~
        ~!#!

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            call "FILEBGON" (#3)
            end
