        REM *************************************************************~
            *                                                           *~
            *  PPPP   RRRR   JJJJJ  IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  P   P  R   R     J     I    NN  N  P   P  U   U    T     *~
            *  PPP    RRRR      J     I    N N N  PPPP   U   U    T     *~
            *  P      R R    J  J     I    N  NN  P      U   U    T     *~
            *  P      R   R   J J   IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PRJINPUT - INPUT/EDITS JOB MASTER RECORD AND BUDGET DETAIL*~
            *            RECORDS.                                       *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/12/81 ! ORIGINAL                                 ! TOM *~
            * 06/30/81 ! ADDED GL ACCOUNTS FOR LBR, PUR, MAT, MISC! TOM *~
            * 10/11/86 ! CUSTOMER changes; FINDNAME removed;      ! ERN *~
            *          !   Clean up and code optimization.        !     *~
            * 08/25/86 ! Added Additional overhead accounts       ! HES *~
            * 06/09/87 ! Added JBMASTR2 for edit on prj #         ! MJB *~
            * 05/03/88 ! Increased SLSTRAN$ from 10 to 12         ! ME? *~
            * 08/05/88 ! Increased 'ALL' SLSTRAN$ usages to 12    ! TLJ *~
            * 08/09/90 ! Activated GETCODE using '?' for all G/L  ! MJB *~
            *          !    Account input fields.                 !     *~
            *          !  Standardized screen literals for same.  !     *~
            *          !  Removed READ101 on JOBMASTR in test data!     *~
            *          !    section.                              !     *~
            *          !  Added input messages.                   !     *~
            * 05/28/91 ! PRR 11916 Enabled fields via REM'ing ... ! JIM *~
            * 05/28/91 !   ... IF stmts. Added tests for blanks.  ! JIM *~
            * 05/28/91 ! ALLFREE.                                 ! JIM *~
            * 06/11/91 ! Formerly Called JOBINPUT.                ! SID *~
            *          !   renamed any reference to a word 'JOB'  !     *~
            *          !   to 'PRJ' and Added PF4)Previous        !     *~
            * 08/23/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            budamount$(100)10,           /* BUDGET AMOUNT FOR INPUT    */~
            buddescr$(100)30,            /* BUDGET DESCRIPTION INPUT   */~
            budtype$(100)2,              /* BUDGET TYPE FOR INPUT      */~
            committed$10,                /* COMMITTED SCREEN DISPLAY   */~
            cursor%(2),                  /* SCREEN STUFF               */~
            cuscode$9,                   /* CUSTOMER CODE              */~
            cusdescr$30,                 /* CUSTOMER DECSRIBE          */~
            date$8,                      /* GOOD OLD DATE              */~
            dateclosed$8,                /* DATE PRJ IS CLOSED         */~
            dateopen$8,                  /* DATE PRJ WAS OPENED        */~
            edtcoltran$80,               /* FINDS THE PROPER COLUMN    */~
            edtmessage$79,               /* DISPLAY MESSAGE            */~
            errormsg$79,                 /* KEEP OPERATOR IN LINE      */~
            fac$(20,3)1,                 /* LINE ITEM ATTRIBUTES       */~
            freespace$(30)20,            /* SPARE CHANGE IN THE FILE   */~
            lbracct$16,                  /* LABOR GL ACCOUNT           */~
            mtrialacct$16,               /* MATERIAL GL ACCOUNT        */~
            pf4$11,                      /*                            */~
            pfkeys$7,                    /*                            */~
            prchseacct$16,               /* PURCHASES GL ACCOUNT       */~
            miscelacct$16,               /* MISCELLANEOUS GL ACCOUNT   */~
            numbers$24,                  /* SOME NUMBERS               */~
            hnytran$10,                  /* TRANSFERRED TO INVENTORY   */~
            i$(24)80,                    /* SCREEN VARIABLE            */~
            infomsg$79,                  /* INFOMATION ON SCREEN       */~
            inpmessage$79,               /* SCREEN DISPLAY             */~
            prjdescr$30,                 /* PRJ DESCRIPTION            */~
            prjnr$8,                     /* PRJ NUMBER                 */~
            labor$10,                    /* LABOR $ AMOUNT             */~
            lastprjnr$8,                 /* LAST PRJ NUMBER            */~
            lfac$(20)1,                  /* LINE ATTRIBUTES            */~
            line2$79,                    /* Second Screen Line         */~
            material$10,                 /* MATERIAL $ AMOUNT          */~
            misctran$10,                 /* MISCELLANEOUS $ AMOUNT     */~
            overhead$10,                 /* TOTAL $ OVERHEAD (LABOR)   */~
            movhdacct$16,                /* MATERIAL OVHD GL ACCOUNT   */~
            lovhdacct$16,                /* LABOR OVERHEAD GL ACCOUNT  */~
            partdescr$32,                /* PART NUMBER DESCRIPTION    */~
            partnr$25,                   /* PART NUMBER                */~
            pfkeys$(4)17,                /* PF KEYS FOR SCREEN         */~
            purchase$10,                 /* $ AMOUINT OF PURCHASES     */~
            quancom$10,                  /* QUANTITY COMMMITTED TO HNY */~
            quantity$10,                 /* QUANTITY TO BE MADE        */~
            readkey$50,                  /* PLOWNEXT READ KEY          */~
            seqnr$3,                     /* SEQENCE NO. FOR PRJBUDGT   */~
            slstran$12,                  /* $ AMOUNT TRANS. TO SALES   */~
            title$(4,2)64                /* TITLES FOR SCREEN          */

        dim f1%(64)                      /* RECORD-ON-FILE FLAGS       */


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
            * #5  ! JBMASTR2 ! JOB Master File                          *~
            * #6  ! HNYMASTR ! INVENTORY MAIN FILE                      *~
            * #7  ! JOBBUDGT ! WIP/JC BUDGET DETAIL FILE                *~
            * #8  ! CUSTOMER ! CUSTOMER MAIN FILE                       *~
            * #9  ! JOBMASTR ! JOB MASTER FILE                          *~
            * #10 ! GLMAIN   ! GENERAL LEDGER MAIN FILE                 *~
            *************************************************************

            select #5,  "JBMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1300,                                  ~
                        keypos = 1, keylen = 8

            select #6, "HNYMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 900,                                   ~
                        keypos = 1, keylen = 25,                         ~
                        alternate key 1, keypos = 102, keylen = 9, dup,  ~
                                  key 2, keypos = 90,  keylen = 4, dup

            select #7, "JOBBUDGT",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 300,                                    ~
                       keypos = 1, keylen = 13

            select #8,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #9, "JOBMASTR",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 700,                                   ~
                        keypos = 1, keylen = 8

            select #10, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 9

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#5,  0%, 0%,   0%, " ")
            call "OPENCHCK" (#6,  0%, 0%,   0%, " ")
            call "OPENCHCK" (#7,  0%, 0%, 200%, " ")
            call "OPENCHCK" (#8,  0%, 0%,   0%, " ")
            call "OPENCHCK" (#9,  0%, 0%, 100%, " ")
            call "OPENCHCK" (#10, 0%, 0%,   0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)
            str(line2$,62) = "PRJINPUT: " & str(cms2v$,,8)
            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press RETURN."

            init(hex(00)) edtcoltran$
            init(hex(01)) str(edtcoltran$,  1, 14)
            init(hex(02)) str(edtcoltran$, 15, 36)
            init(hex(03)) str(edtcoltran$, 51, 14)
            init(hex(04)) str(edtcoltran$, 65, 16)

            pfkeys$(1) = hex(00010204130d0f10ffffffffffffffffff)
            pfkeys$(2) = hex(0001020304050607090a0b0d0f10ffffff)
            pfkeys$(3) = hex(00010fffffffffffffffffffffffffffff)
            pfkeys$(4) = hex(00010fffffffffffffffffffffffffffff)


            title$(1,1) = "(1)Start Over  (2)Col 1  (4)Line Above      (1~
        ~6)Edit Mode"
            title$(2,1) = "(1)Start Over(2)Fisrt(3)Last(4)Prev(5)Next(6)D~
        ~own(7)Up"
            title$(2,2) = "(9)Header(11)Insert(12)Delete(15)Print Screen ~
        ~(16)Write Data"
            title$(3,1) = "Supply Requested Items and (ENTER) OR (1) to E~
        ~xit Insert Mode"
            title$(4,1) = "Press RETURN to Delete Flashing Line OR (1) to~
        ~ EXIT Delete."
            title$(1,2) = "'P' = Purchases  'I' = Inventory  'W' = Workst~
        ~ation"
        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            errormsg$, inpmessage$, prjnr$, partnr$, overhead$, cuscode$,~
            miscelacct$, quantity$, buddescr$(), budamount$(), purchase$,~
            dateclosed$, mtrialacct$, prchseacct$, budtype$(), dateopen$,~
            cusdescr$, committed$, misctran$, slstran$, hnytran$, labor$,~
            material$, quancom$, prjdescr$, lbracct$, movhdacct$,        ~
            lovhdacct$, infomsg$, partdescr$ = " "
            comseqnr, mattranseqnr, purchseqnr, lbrseqnr = 1
            init(hex(00)) freespace$(), numbers$
            call "ALLFREE"

            for fieldnr% = 1 to 13
L10100:         gosub'161(fieldnr%)
                      if enabled% = 0 then L10180
L10120:         gosub'201(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  <> 4 then       L10140
L10132:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'161(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10132
L10140:               if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10180:         next fieldnr%

            for fieldnr% = 1 to  9
L10210:         gosub'162(fieldnr%)
                      if enabled% = 0 then L10280
L10230:         gosub'202(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  <> 4 then       L10250
L10242:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'162(fieldnr%)
                         if enabled% = 1% then L10230
                         if fieldnr% = 1% then L10210
                         goto L10242
L10250:               if keyhit% <>  0 then       L10230
                gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10230
L10280:         next fieldnr%

        REM *************************************************************~
            *              I N P U T   L I N E   I T E M S              *~
            *                                                           *~
            * INPUTS LINE ITEMS AND TESTS FOR VALIDITY.                 *~
            *************************************************************

            maxlines%, screenline%, currentline% , line% = 0

L10380:     screenline% = screenline% + 1
            currentline% = currentline% + 1
            if screenline% <= 20 then L10450
               screenline% = 1
               line% = line% + 20
               if line% = 100  then editlines

L10450:     infomsg$ = " "
            for fieldnr% = 1 to 3
                gosub'163(fieldnr%)
                      if enabled% = 0 then L10570
L10490:         gosub'203(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  2 then gosub columnone
                      if keyhit%  =  4 then gosub lineabove
                      if keyhit% <> 16 or fieldnr% <> 1 then L10540
                        errormsg$ = " "
                        goto edtpg1
L10540:               if keyhit% <>  0 then       L10490
                gosub'153(fieldnr%)
                      if errormsg$ <> " " then L10490
L10570:         next fieldnr%
            maxlines% = maxlines% + 1
            goto  L10380

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR LINEAR SCREENS.        *~
            *************************************************************

        edtpg1
L11070:     gosub'211(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       editlines
                  if keyhit%  =  5 then       edtpg2
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11070
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 2% or fieldnr% > 14% then L11070
            if fieldnr% > 6% then fieldnr% = fieldnr% - 1%
            gosub'161(fieldnr%)
                  if enabled% = 0% then L11070
L11150:     gosub'211(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11150
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11150
            goto L11070

        edtpg2
L11230:     gosub'212(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       editlines
                  if keyhit%  =  4 then       edtpg1
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11230
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  9 then L11230

            gosub'162(fieldnr%)
                  if enabled% = 0% then L11230
L11310:     gosub'212(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11310
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L11310
            goto L11230


        REM *************************************************************~
            *        C O L U M N   O N E ,   L I N E   A B O V E        *~
            *                                                           *~
            * COLUMN ONE KEY AND LINE ABOVE KEY FUNCTIONS HANDLED HERE. *~
            *************************************************************

        columnone
            if fieldnr% = 1 then return
            budtype$(currentline%), buddescr$(currentline%),             ~
            budamount$(currentline%) = " "
            infomsg$ = " "
            fieldnr% = 1
            return

        lineabove
            if currentline% = 1 then return
            on fieldnr% gosub L14220,     /* BUDGET TYPE                */~
                              L14230,     /* BUDGET DESCRIPTION         */~
                              L14240      /* BUDGET AMOUNT              */

            return

L14220:  budtype$  (currentline%) =   budtype$  (currentline%-1)  : return
L14230:  buddescr$ (currentline%) =   buddescr$ (currentline%-1)  : return
L14240:  budamount$ (currentline%) = budamount$ (currentline%-1)  : return


L14270: REM *************************************************************~
            *              I N S E R T   M O D E   C O D E              *~
            *                                                           *~
            * HANDLES INSERTION OF A LINE ITEM INTO THE BUDGET DETAIL   *~
            *************************************************************

        insertmode
            if maxlines% = 100 then return         /* ARRAY FULL, CAN'T*/
            REM OTHERWISE, SET CURRENTLINE%, SCREENLINE%, AND COPY RIGHT
                screenline% = max(0, cursor%(1) - 4)
                if line% + screenline% < maxlines% then L14390
                   screenline% = maxlines% - line% /* TO INS AT END    */
L14390:         if screenline% <> 20 then L14420    /* BOTTOM OF PAGE   */
                   line% = line% + 1
                   screenline% = screenline% - 1
L14420:         currentline%, c% = screenline% + line%

            REM COPY ALL THE ELEMENTS UP ONE
                if c% >= maxlines% then L14530
                for temp% = maxlines% to c% step -1
              budtype$  (temp%+1)  =  budtype$ (temp%)
              buddescr$  (temp%+1) = buddescr$  (temp%)
              budamount$ (temp%+1) = budamount$ (temp%)

                    next temp%

L14530:         screenline% = screenline% + 1
                c%, currentline% = currentline% + 1

                init(" ") budtype$(c%), buddescr$(c%), budamount$(c%),   ~
                          errormsg$, infomsg$

            REM NOW INPUT THE LINE, ENABLE CANCEL OUT OPTION
                infomsg$ = " "
                for fieldnr% = 1 to 3
                    gosub'163(fieldnr%)
                          if enabled% = 0 then L14690
L14640:             gosub'223(fieldnr%)
                          if keyhit%  =  1 then L14760     /* END INSERT*/
                          if keyhit% <>  0 then L14640
                    gosub'153(fieldnr%)
                          if errormsg$ <> " " then L14640
L14690:             next fieldnr%

                maxlines%  = maxlines% + 1
                cursor%(1) = min(cursor%(1) + 1, 24)
                goto L14270
L14750:
L14760:     REM THIS ROUTINE ABORTS INSERT MODE AND DESTROYS SCREENLINE%
                c% = currentline%
                if currentline% <= maxlines% then gosub L14890

                temp% = maxlines% + 1
                init(" ") budtype$(temp%), buddescr$(temp%),             ~
                          budamount$(temp%), errormsg$, infomsg$

                if currentline% >= maxlines% and screenline% = 20        ~
                   then line% = max(0%, line%- 1%)
            return

L14890:     for temp% = currentline% to maxlines%
                budtype$  (temp%) = budtype$   (temp%+1)
                buddescr$ (temp%) = buddescr$  (temp%+1)
                budamount$(temp%) = budamount$ (temp%+1)

                next temp%
            return

        REM *************************************************************~
            *              D E L E T E   M O D E   C O D E              *~
            *                                                           *~
            * DELETES A LINE ITEM FROM THE BUDGET DETAIL.               *~
            *************************************************************

        deletemode
            if maxlines% = 0 then return
            screenline% = cursor%(1) - 4
            if screenline% < 1 then return
               currentline% = screenline% + line%
               if currentline% > maxlines% then return

L15100:     gosub'233(screenline%)
                  if keyhit%  =  1 then       return
                  if keyhit% <>  0 then       L15100

            c% = currentline%
            if currentline% < maxlines% then gosub L14750
                                         /* ACTUALLY DELETE LINE @C%   */
            temp% = maxlines%
            init(" ") budtype$(temp%), buddescr$(temp%),                 ~
                      budamount$(temp%), errormsg$, infomsg$
            maxlines% = maxlines% - 1
            return

        REM *************************************************************~
            *               E D I T   L I N E   I T E M S               *~
            *                                                           *~
            * EDITS LINE ITEMS, GOES AND SAVES DATA TO THE FILE, ETC.   *~
            *************************************************************

        editlines
            line%, currentline%, screenline% = 0

L15330:     gosub'213(0%)
                  if keyhit%  =  0 then       L15500
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       line%=0
                  if keyhit%  =  3 then       line%=max(0,maxlines%-20)
                  if keyhit%  =  4 then       line%=max(0,line%-15)
                  if keyhit%  =  5 then       line%=min(line%+15,        ~
                                                 max(0,maxlines%-20))
                  if keyhit%  =  6 then       line%=max(0,line%-1)
                  if keyhit%  =  7 then       line%=min(line%+1,         ~
                                                 max(0, maxlines%-20))
                  if keyhit%  =  9 then       edtpg1
                  if keyhit%  = 11 then gosub insertmode
                  if keyhit%  = 12 then gosub deletemode
                  if keyhit%  = 16 then       datasave
                  goto L15330

L15500:     REM NOW FIGURE OUT WHICH FIELD HE HIT.
                screenline% = max(0%, cursor%(1) - 4%)
                if screenline%  =  0 then L15330
                currentline% = screenline% + line%
                if currentline% > maxlines% then L15330
                fieldnr% = val(str(edtcoltran$,cursor%(2)))
                if fieldnr% = 0 then L15330
                if budtype$(currentline%) = " " then fieldnr% = 2

L15590:         gosub'213(fieldnr%)
                      on keyhit% gosub startover
                      if keyhit% <> 0 then L15590
                gosub'153(fieldnr%)
                      if errormsg$ <> " " then L15590

                goto L15330

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            inpmessage$ = "Saving Information for Proj. Number: " & prjnr$
            call "SHOSTAT" (inpmessage$)
            lastprjnr$ = prjnr$
            gosub L31000
            call "DELETE" (#7, prjnr$, 8%)
            gosub L31670
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'161(fieldnr%)
                  inpmessage$ = " "
                  enabled% = 0
                  on fieldnr% gosub L20100,         /* PRJ NUMBER       */~
                                    L20150,         /* PRJ DESCRIPTION  */~
                                    L20200,         /* DATE OPENED      */~
                                    L20250,         /* DATE CLOSED      */~
                                    L20300,         /* CUSTOMER CODE    */~
                                    L20350,         /* PART NUMBER      */~
                                    L20400,         /* QUANTITY         */~
                                    L20450,         /* QUANTITY COMPLETE*/~
                                    L20500,         /* DIRECT LABOR     */~
                                    L20550,         /* TOTAL PURCHASES  */~
                                    L20600,         /* TOTAL MATERIAL   */~
                                    L20650,         /* TOTAL COMMITTED  */~
                                    L20690          /* TOTAL LAB OVERHED*/
                     return
L20100: REM Default/Enable For Project Number
            enabled% = 1
            inpmessage$ = "Enter Project Number or '?' to Select"
            return

L20150: REM Default/Enable For Project Description
            enabled% = 1
            inpmessage$ = "Enter Project Description"
            return

L20200: REM Default/Enable For Date Opened
            dateopen$ = date$
            enabled% = 1
            inpmessage$ = "Enter the Date the Project was Opened"
            return

L20250: REM Default/Enable For Date Closed
            if prjonfile% = 0 then return
            enabled% = 1

            inpmessage$ = "Enter the Date the Project was Closed"
            return

L20300: REM Default/Enable For Customer Code
            enabled% = 1
            inpmessage$ = "Enter Customer Code or '?' to Select"
            return

L20350: REM Default/Enable For Part Number To Be Made
            enabled% = 1
           inpmessage$ = "Enter the Part Number to Make or <CR> to Select"
            return

L20400: REM Default/Enable For Quantity To Be Made
*          IF PARTNR$ = " " THEN RETURN
            enabled% = 1
            inpmessage$ = "Enter the Quantity of this Part to Make"
            return

L20450: REM Default/Enable For Quantity Completed To Date
*          IF PARTNR$ = " " THEN RETURN
            enabled% = 1
            inpmessage$ = "Enter the Quantity Completed to Date"
            return

L20500: REM Default/Enable For Total Direct Labor
*          IF PARTNR$ = " " THEN RETURN
            enabled% = 1
            inpmessage$ = "Enter Total Direct Labor Costs"
            return

L20550: REM Default/Enable For Total Purchases
*          IF PARTNR$ = " " THEN RETURN
            enabled% = 1
            inpmessage$ = "Enter Total Purchase Costs"
            return

L20600: REM Default/Enable For Total Material
*          IF PARTNR$ = " " THEN RETURN
            enabled% = 1
            inpmessage$ = "Enter Total Materials Costs"
            return

L20650: REM Default/Enable For Total Commited Inventory
*          IF PARTNR$ = " " THEN RETURN
            enabled% = 1
            inpmessage$ = "Enter Total Cost of Inventory Committed"
            return

L20690: REM Default/Enable For Total Labor Overhead Posted To Project
*          IF PARTNR$ = " " THEN RETURN
            enabled% = 1
            inpmessage$ = "Enter Total Overhead Costs"
            return


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 2 OF INPUT. *~
            *************************************************************

            deffn'162(fieldnr%)
                  inpmessage$ = " "
                  enabled% = 0
                  on fieldnr% gosub L21180,         /* HNY TRANSFERRED  */~
                                    L21230,         /* SALES TRANSFERRED*/~
                                    L21280,         /* MISC TRASFERRED  */~
                                    L21330,         /* GL LABOR ACCT.   */~
                                    L21370,         /* Lab overhead acct*/~
                                    L21410,         /* GL PURCHASE ACCT */~
                                    L21450,         /* GL MATERIAL ACCT */~
                                    L21490,         /* Mat overhead acct*/~
                                    L21530          /* GL MISC. ACCT.   */
                     return
L21180: REM DEFAULT/ENABLE FOR $$ TRANSFERRED TO INVENTORY
*          IF PARTNR$ = " " THEN RETURN
            enabled% = 1
            inpmessage$ = "Enter $$ Transferred from Project to Inventory"
            return

L21230: REM DEFAULT/ENABLE FOR $$ TRANSFERRED TO SALES
*          IF PARTNR$ = " " THEN RETURN
            enabled% = 1
            inpmessage$ = "Enter Sales Credited to this Project"
            return

L21280: REM DEFAULT/ENABLE FOR $$ TRANSFERRED MISC.
*          IF PARTNR$ = " " THEN RETURN
            enabled% = 1
            inpmessage$ = "Enter $$ Transferred from Project to Other "  ~
                        & "Projects"
            return

L21330: REM DEFAULT/ENABLE FOR GL LABOR ACCOUNT
            inpmessage$ = "Enter G/L Account Number for Labor" &         ~
                          " or '?' to select"
            enabled% = 1
            return

L21370: REM DEFAULT/ENABLE FOR GL LABOR OVERHEAD ACCOUNT
            inpmessage$ = "Enter G/L Account Number for Labor Overhead" &~
                          " or '?' to select"
            enabled% = 1
            return

L21410: REM DEFAULT/ENABLE FOR GL PURCHASE ACCOUNT
            inpmessage$ = "Enter G/L Account Number for Purchases" &     ~
                          " or '?' to select"
            enabled% = 1
            return

L21450: REM DEFAULT/ENABLE FOR GL MATERIAL ACCOUNT
            inpmessage$ = "Enter G/L Account Number for Material" &      ~
                          " or '?' to select"
            enabled% = 1
            return

L21490: REM DEFAULT/ENABLE FOR GL MATERIAL OVERHEAD ACCOUNT
            inpmessage$ = "Enter G/L Account for Material Overhead" &    ~
                          " or '?' to select"
            enabled% = 1
            return

L21530: REM DEFAULT/ENABLE FOR GL MISCELLANEOUS TRANSFER ACCOUNT
            inpmessage$ = "Enter G/L Account Number for Miscellaneous" & ~
                          " or '?' to select"
            enabled% = 1
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   L I N E  I T E M S      *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE LINE ITEMS.      *~
            *************************************************************

            deffn'163(fieldnr%)
                  enabled% = 0
                  on fieldnr% gosub L22125,         /* BUDGET TYPE      */~
                                    L22165,         /* BUDGET DESCRIPTN */~
                                    L22205          /* BUDGET AMOUNT    */
                     return
L22125:     REM DEFAULT/ENABLE FOR BUDGET TYPE
                enabled% = 1
                 return

L22165:     REM DEFAULT/ENABLE FOR BUDGET DESCRIPTION
                enabled% = 1
                   return

L22205:     REM DEFAULT/ENABLE FOR BUDGET AMOUNT
                enabled% = 1
                 return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
L29921:     u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            if u3% <> 0% then L29921
                return clear all
                goto inputmode

L30000: REM *************************************************************~
            *      R E T R E I V E   D A T A                            *~
            *  GETS THE DATA FOR THE JOB MASTER HEADER FILE             *~
            *************************************************************

          inpmessage$ = "Loading Information for Proj. Number: " & prjnr$
          call "SHOSTAT" (inpmessage$)

          get #9, using L30430, prjnr$, prjdescr$, dateopen$, dateclosed$,~
                  cuscode$, partnr$, quantity, quancom, labor, purchase, ~
                  material, committed, hnytran, slstran, misctran,       ~
                  comseqnr, lbrseqnr, mattranseqnr, purchseqnr,          ~
                  overhead, lbracct$, prchseacct$, mtrialacct$,          ~
                  miscelacct$, numbers$, movhdacct$, lovhdacct$,         ~
                  str(freespace$(),,250), str(freespace$(),251)

            partdescr$ = "Part not in file"
            call "DESCRIBE" (#6, partnr$, partdescr$, 0%, f1%(6))
            call "CONVERT" (quantity,  -0.2, quantity$)
            call "CONVERT" (quancom,   -0.2, quancom$)
            call "CONVERT" (labor,     -2.2, labor$)
            call "CONVERT" (purchase,  -2.2, purchase$)
            call "CONVERT" (material,  -2.2, material$)
            call "CONVERT" (committed, -2.2, committed$)
            call "CONVERT" (hnytran,   -2.2, hnytran$)
            call "CONVERT" (slstran,   -2.2, slstran$)
            call "CONVERT" (misctran,  -2.2, misctran$)
            call "CONVERT" (overhead,  -2.2, overhead$)
            call "DATEFMT" (dateopen$)
            if str(movhdacct$,,1) = hex(00) then movhdacct$ = " "
            if str(lovhdacct$,,1) = hex(00) then lovhdacct$ = " "
            call "GLFMT" (lbracct$)
            call "GLFMT" (prchseacct$)
            call "GLFMT" (mtrialacct$)
            call "GLFMT" (miscelacct$)
            call "GLFMT" (movhdacct$)
            call "GLFMT" (lovhdacct$)

            if dateclosed$ <> " " then call "DATEFMT" (dateclosed$)

            if cuscode$ = " " then L30410
            call "DESCRIBE" (#8, cuscode$, cusdescr$, 0%, f1%(8))
            if f1%(8) = 0 then cusdescr$ = "Customer Not On File"
L30410:         return

L30430:    FMT CH(08),                   /* PRJ NUMBER                 */~
               CH(30),                   /* PRJ DESCRIPTION            */~
               CH(06),                   /* DATE OPENED                */~
               CH(06),                   /* DATE CLOSED                */~
               CH(09),                   /* CUSTOMER CODE              */~
               CH(25),                   /* PART NUMBER TO BE MADE     */~
               PD(14,4),                 /* QUANTITY TO BE MADE        */~
               PD(14,4),                 /* QUANTITY COMPLETED TO DATE */~
               PD(14,4),                 /* TOTAL $ TO DIRECT LABOR    */~
               PD(14,4),                 /* TOTAL $ TO DIRECT PURCHASES*/~
               PD(14,4),                 /* TOTAL $ MATERIAL FROM HNY  */~
               PD(14,4),                 /* TOTAL $ COMMITTED TO HNY   */~
               PD(14,4),                 /* $ TRANSFERRED TO INVENTORY */~
               PD(14,4),                 /* $ TRANSFERRED TO SALES     */~
               PD(14,4),                 /* $ TRANSFERRED TO MISCELL.  */~
               PD(14,4),                 /* MATERIAL COMM. SEQUENCE NO.*/~
               PD(14,4),                 /* LABOR SEQUENCE NUMBER      */~
               PD(14,4),                 /* DIRCT MATERIAL TRANS. SEQ# */~
               PD(14,4),                 /* DIRECT PURCHASE SEQ. NO.   */~
               PD(14,4),                 /* TOTAL $ LABOR OVERHEAD     */~
               CH(09),                   /* LABOR G/L ACCOUNT          */~
               CH(09),                   /* PURCHASE G/L ACCOUNT       */~
               CH(09),                   /* MATERIAL G/L ACCOUNT       */~
               CH(09),                   /* MISCELLANEOUS G/L ACCOUNT  */~
               CH(24),                   /* NUMBERS                    */~
               CH(09),                   /* MAT OVERHEAD G/L ACCOUNT   */~
               CH(09),                   /* LAB OVERHEAD G/L ACCOUNT   */~
               CH(226),                  /* SPARE CHANGE               */~
               CH(200)                   /* SPARE CHANGE               */

L30720: REM *************************************************************~
            *      R E T R I E V E   T H E  B U D G E T  D A T A        *~
            *                                                           *~
            * RETRIEVES THE BUDGET DATA USING PLOWNEXT TO GET ALL       *~
            *************************************************************

              maxlines% = 0
              readkey$ = prjnr$
L30800:       call "PLOWNEXT" (#7, readkey$, 8%, f1%(7))
                  if f1%(7) = 0 then return
                  maxlines% = maxlines% + 1
              get #7, using L30900, budtype$(maxlines%),                  ~
                        buddescr$(maxlines%), budamount
              call "CONVERT" (budamount, 2.2, budamount$(maxlines%))
              if budtype$(maxlines%) = "P" then                          ~
                                    call "GLFMT" (buddescr$(maxlines%))
                go to L30800

L30900:     FMT XX(08),                            /* SKIP PRJ NUMBER  */~
                CH(02),                            /* BUDGET TYPE      */~
                XX(03),                            /* SEQUENCE NUMBER  */~
                CH(30),                            /* BUD DESCRIPTION  */~
                PD(14,4)                           /* BUDGET AMOUNT    */

L31000: REM *************************************************************~
            *      S A V E  T H E  D A T A                              *~
            *                                                           *~
            * SAVES THE DATA FOR JOBMASTR.                              *~
            *************************************************************

            call "DATUNFMT" (dateopen$)
            call "DATUNFMT" (dateclosed$)
            quantity, quancom, labor, purchase, material, committed,     ~
            hnytran, slstran, misctran, overhead = 0
             convert quantity$ to quantity, data goto L31100
L31100:      convert quancom$ to quancom, data goto L31110
L31110:      convert labor$ to labor, data goto L31120
L31120:      convert purchase$ to purchase, data goto L31130
L31130:      convert material$ to material, data goto L31140
L31140:      convert committed$ to committed, data goto L31150
L31150:      convert hnytran$ to hnytran, data goto L31160
L31160:      convert slstran$ to slstran, data goto L31170
L31170:      convert misctran$ to misctran, data goto L31180
L31180:      convert overhead$ to overhead, data goto L31190
L31190:      call "GLUNFMT" (lbracct$)
             call "GLUNFMT" (movhdacct$)
             call "GLUNFMT" (lovhdacct$)
             call "GLUNFMT" (prchseacct$)
             call "GLUNFMT" (mtrialacct$)
             call "GLUNFMT" (miscelacct$)

            call "READ101" (#9, prjnr$, f1%(9))
            if f1%(9) = 1 then delete #9

         write #9,using L31380, prjnr$, prjdescr$, dateopen$, dateclosed$,~
                  cuscode$, partnr$, quantity, quancom, labor, purchase, ~
                  material, committed, hnytran, slstran, misctran,       ~
                  comseqnr, lbrseqnr,  mattranseqnr, purchseqnr,         ~
                  overhead, lbracct$, prchseacct$, mtrialacct$,          ~
                  miscelacct$, numbers$, movhdacct$, lovhdacct$,         ~
                  str(freespace$(),,250), str(freespace$(),251)
            return

L31380:    FMT CH(08),                   /* PRJ NUMBER                 */~
               CH(30),                   /* PRJ DESCRIPTION            */~
               CH(06),                   /* DATE OPENED                */~
               CH(06),                   /* DATE CLOSED                */~
               CH(09),                   /* CUSTOMER CODE              */~
               CH(25),                   /* PART NUMBER TO BE MADE     */~
               PD(14,4),                 /* QUANTITY TO BE MADE        */~
               PD(14,4),                 /* QAUNTITY COMMITTED         */~
               PD(14,4),                 /* TOTAL $ TO DIRECT LABOR    */~
               PD(14,4),                 /* TOTAL $ TO DIRECT PURCHASES*/~
               PD(14,4),                 /* TOTAL $ MATERIAL FROM HNY  */~
               PD(14,4),                 /* TOTAL $ COMMITTED TO HNY   */~
               PD(14,4),                 /* $ TRANSFERRED TO INVENTORY */~
               PD(14,4),                 /* $ TRANSFERRED TO SALES     */~
               PD(14,4),                 /* $ TRANSFERRED TO MISCELL.  */~
               PD(14,4),                 /* MATERIAL COMM. SEQUENCE NO.*/~
               PD(14,4),                 /* LABOR SEQUENCE NUMBER      */~
               PD(14,4),                 /* DIRCT MATERIAL TRANS. SEQ# */~
               PD(14,4),                 /* DIRECT PURCHASE SEQ. NO.   */~
               PD(14,4),                 /* TOTAL $ LABOR OVERHEAD     */~
               CH(09),                   /* LABOR G/L ACCOUNT          */~
               CH(09),                   /* PURCHASE G/L ACCOUNT       */~
               CH(09),                   /* MATERIAL G/L ACCOUNT       */~
               CH(09),                   /* MISCELLANEOUS G/L ACCOUNT  */~
               CH(24),                   /* NUMBERS                    */~
               CH(09),                   /* MAT OVERHEAD G/L ACCOUNT   */~
               CH(09),                   /* LAB OVERHEAD G/L ACCOUNT   */~
               CH(226),                  /* SPARE CHANGE               */~
               CH(200)                   /* SPARE CHANGE               */

L31670: REM *************************************************************~
            *      W R I T E   B U D G E T  F I L E  D A T A            *~
            *                                                           *~
            * WRITES THE DATA IN THE BUDGET FILE WITH MAXLINES.         *~
            *************************************************************

             if maxlines% = 0 then return
                for temp% = 1 to maxlines%
                convert temp% to seqnr$, pic(###)
                convert budamount$(temp%) to budamount

               if budtype$(temp%) = "P" then                             ~
                                        call "GLUNFMT" (buddescr$(temp%))
               write #7, using L31890, prjnr$, budtype$(temp%), seqnr$,   ~
                         buddescr$(temp%), budamount,                    ~
                         str(freespace$(), 1)

                next temp%
                return

L31890:     FMT CH(08),                            /* PRJ NUMBER       */~
                CH(02),                            /* BUDGET TYPE      */~
                CH(03),                            /* SEQUENCE NUMBER  */~
                CH(30),                            /* BUDGET DESCR     */~
                PD(14,4),                          /* BUDGET AMOUNT    */~
                CH(249)                            /* SPARE CHANGE     */

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'201(fieldnr%)
               if fieldnr% = 1 then pf4$ = " " else pf4$ = "(4)Previous"
                  init(hex(84)) lfac$()
                  str(line2$,,50) = "Last Project: " & lastprjnr$
                  pfkeys$ = hex(0001040d0e0f10)
                  if fieldnr% = 1% then str(pfkeys$,3,1) = hex(ff)
                  on fieldnr% gosub L40130,         /* PRJ NUMBER       */~
                                    L40115,         /* PRJ DESCRIPTION  */~
                                    L40130,         /* DATE OPENED      */~
                                    L40130,         /* DATE CLOSED      */~
                                    L40130,         /* CUSTOMER CODE    */~
                                    L40130,         /* PART NUMBER      */~
                                    L40145,         /* QUANTITY         */~
                                    L40145,         /* QUANTITY COMPLETE*/~
                                    L40145,         /* DIRECT LABOR     */~
                                    L40145,         /* TOTAL PURCHASES  */~
                                    L40145,         /* TOTAL MATERIAL   */~
                                    L40145,         /* TOTAL COMMITTED  */~
                                    L40145          /* TOTAL LAB OVERHED*/
                     go to L40165

L40115:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40130:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40145:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40165:     accept                                                       ~
               at (01,02), "Manage Project Master File and Project Budget~
        ~s"                                                      ,        ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$,                          ~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (03,02), fac(hex(84)), infomsg$               , ch(79),~
               at (06,02),                                               ~
                  "Project Number",                                      ~
               at (06,33), fac(lfac$( 1)), prjnr$               , ch(08),~
               at (07,02),                                               ~
                  "Project Description",                                 ~
               at (07,33), fac(lfac$( 2)), prjdescr$            , ch(30),~
               at (08,02),                                               ~
                  "Date Project Opened",                                 ~
               at (08,33), fac(lfac$( 3)), dateopen$            , ch(08),~
               at (09,02),                                               ~
                  "Date Project Closed",                                 ~
               at (09,33), fac(lfac$( 4)), dateclosed$          , ch(08),~
               at (10,02),                                               ~
                  "Customer This Project is For",                        ~
               at (10,33), fac(lfac$( 5)), cuscode$             , ch(09),~
               at (10,43), fac(hex(8c)),   cusdescr$            , ch(30),~
               at (11,02),                                               ~
                  "Part Number To Be Made",                              ~
               at (11,33), fac(lfac$( 6)), partnr$              , ch(25),~
               at (12,02), "Part Description",                           ~
               at (12,33), fac(hex(8c)),   partdescr$           , ch(32),~
               at (13,02),                                               ~
                  "Quantity To Be Made",                                 ~
               at (13,33), fac(lfac$( 7)), quantity$            , ch(10),~
               at (14,02),                                               ~
                  "Quantity Completed To Date",                          ~
               at (14,33), fac(lfac$( 8)), quancom$             , ch(10),~
               at (15,02),                                               ~
                  "Labor Costs Incur. To Date",                          ~
               at (15,33), fac(lfac$( 9)), labor$               , ch(10),~
               at (16,02),                                               ~
                  "Cost of Purchases To Date",                           ~
               at (16,33), fac(lfac$(10)), purchase$            , ch(10),~
               at (17,02),                                               ~
                  "Cost of Materials To Date",                           ~
               at (17,33), fac(lfac$(11)), material$            , ch(10),~
               at (18,02),                                               ~
                  "Cost of Inventory Committed",                         ~
               at (18,33), fac(lfac$(12)), committed$           , ch(10),~
               at (19,02),                                               ~
                  "Overhead Costs Incurred",                             ~
               at (19,33), fac(lfac$(13)), overhead$            , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,26), fac(hex(8c)),   pf4$                 , ch(11),~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                                                                         ~
               keys(pfkeys$)                                     ,       ~
               key (keyhit%)

               if keyhit% <> 13 then L40530
                  call "MANUAL" ("PRJINPUT")
                  goto L40165

L40530:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40165

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   2      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'202(fieldnr%)
               if fieldnr% = 1 then pf4$ = " " else pf4$ = "(4)Previous"
                  init(hex(84)) lfac$()
                  str(line2$,,50) = " "
                  pfkeys$ = hex(0001040d0e0f10)
                  if fieldnr% = 1% then str(pfkeys$,3,1) = hex(ff)
                  on fieldnr% gosub L41260,         /* HNY TRANSFERRED  */~
                                    L41260,         /* SALES TRANSFERRED*/~
                                    L41260,         /* MISC TRASFERRED  */~
                                    L41230,         /* LABOR G/L ACCT.  */~
                                    L41230,         /* LAB OVHD G/L ACCT*/~
                                    L41230,         /* PURCH. G/L ACCT. */~
                                    L41230,         /* MATERIAL G/L ACCT*/~
                                    L41230,         /* MAT OVHD G/L ACCT*/~
                                    L41230          /* MISC G/L ACCOUNT */
                     goto L41300

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41230:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L41260:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41300:     accept                                                       ~
               at (01,02), "Manage Project Master File and Project Budget~
        ~s"                                                      ,        ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$,                          ~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(84)), infomsg$               , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "$$ Transferred To Inventory",                         ~
               at (06,35), fac(lfac$( 1)), hnytran$             , ch(10),~
               at (07,02),                                               ~
                  "Sales Credited To This Project",                      ~
               at (07,35), fac(lfac$( 2)), slstran$             , ch(12),~
               at (08,02),                                               ~
                  "Costs Transf To Other Projects",                      ~
               at (08,35), fac(lfac$( 3)), misctran$            , ch(10),~
               at (09,02),                                               ~
                  "Labor G/L Account",                                   ~
               at (09,35), fac(lfac$( 4)), lbracct$             , ch(12),~
               at (10,02),                                               ~
                  "Labor Overhead G/L Account",                          ~
               at (10,35), fac(lfac$( 5)), lovhdacct$           , ch(12),~
               at (11,02),                                               ~
                  "Purchase G/L Account",                                ~
               at (11,35), fac(lfac$( 6)), prchseacct$          , ch(12),~
               at (12,02),                                               ~
                  "Material G/L Account",                                ~
               at (12,35), fac(lfac$( 7)), mtrialacct$          , ch(12),~
               at (13,02),                                               ~
                  "Material Overhead G/L Acct",                          ~
               at (13,35), fac(lfac$( 8)), movhdacct$           , ch(12),~
               at (14,02),                                               ~
                  "Miscellaneous G/L Account",                           ~
               at (14,35), fac(lfac$( 9)), miscelacct$          , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (22,26), fac(hex(8c)),   pf4$                 , ch(11),~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                                                                         ~
               keys(pfkeys$)                                     ,       ~
               key (keyhit%)

               if keyhit% <> 13 then L41780
                  call "MANUAL" ("PRJINPUT")
                  goto L41300

L41780:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L41300

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'211(fieldnr%)
                  if fieldnr% = 0% then inpmessage$ = edtmessage$
                  init(hex(84)) lfac$()
                  str(line2$,,50) = " "
                  on fieldnr% gosub L42135,         /* PRJ NUMBER       */~
                                    L42120,         /* PRJ DESCRIPTION  */~
                                    L42135,         /* DATE OPENED      */~
                                    L42135,         /* DATE CLOSED      */~
                                    L42135,         /* CUSTOMER CODE    */~
                                    L42135,         /* PART NUMBER      */~
                                    L42150,         /* QUANTITY         */~
                                    L42150,         /* QUANTITY COMPLETE*/~
                                    L42150,         /* DIRECT LABOR     */~
                                    L42150,         /* TOTAL PURCHASES  */~
                                    L42150,         /* TOTAL MATERIAL   */~
                                    L42150,         /* TOTAL COMMITTED  */~
                                    L42150          /* TOTAL LAB OVERHED*/
                     goto L42170

L42120:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L42135:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L42150:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L42170:     accept                                                       ~
               at (01,02), "Manage Project Master File and Project Budget~
        ~s"                                                      ,        ~
               at (01,60), "Todays Date: ",                              ~
               at (01,73), fac(hex(8c)), date$,                          ~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (03,02), fac(hex(84)), infomsg$               , ch(79),~
               at (06,02),                                               ~
                  "Project Number",                                      ~
               at (06,32), fac(lfac$( 1)), prjnr$               , ch(08),~
               at (07,02),                                               ~
                  "Project Description",                                 ~
               at (07,32), fac(lfac$( 2)), prjdescr$            , ch(30),~
               at (08,02),                                               ~
                  "Date Project Opened",                                 ~
               at (08,32), fac(lfac$( 3)), dateopen$            , ch(08),~
               at (09,02),                                               ~
                  "Date Project Closed",                                 ~
               at (09,32), fac(lfac$( 4)), dateclosed$          , ch(08),~
               at (10,02),                                               ~
                  "Customer This Project is For",                        ~
               at (10,32), fac(lfac$( 5)), cuscode$             , ch(09),~
               at (10,43), fac(hex(8c)),   cusdescr$            , ch(30),~
               at (11,02),                                               ~
                  "Part Number To Be Made",                              ~
               at (11,32), fac(lfac$( 6)), partnr$              , ch(25),~
               at (12,02), "Part Description",                           ~
               at (12,32), fac(hex(8c)),   partdescr$           , ch(32),~
               at (13,02),                                               ~
                  "Quantity To Be Made",                                 ~
               at (13,32), fac(lfac$( 7)), quantity$            , ch(10),~
               at (14,02),                                               ~
                  "Quantity Completed To Date",                          ~
               at (14,32), fac(lfac$( 8)), quancom$             , ch(10),~
               at (15,02),                                               ~
                  "Labor Costs Incur. To Date",                          ~
               at (15,32), fac(lfac$( 9)), labor$               , ch(10),~
               at (16,02),                                               ~
                  "Cost of Purchases To Date",                           ~
               at (16,32), fac(lfac$(10)), purchase$            , ch(10),~
               at (17,02),                                               ~
                  "Cost of Materials To Date",                           ~
               at (17,32), fac(lfac$(11)), material$            , ch(10),~
               at (18,02),                                               ~
                  "Cost of Inventory Committed",                         ~
               at (18,32), fac(lfac$(12)), committed$           , ch(10),~
               at (19,02),                                               ~
                  "Overhead Costs Incurred",                             ~
               at (19,32), fac(lfac$(13)), overhead$            , ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,40), "(5)Next Page",                               ~
               at (24,02), "(2)Project Budget",                          ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Save Data",                              ~
                                                                         ~
               keys(hex(000102050d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L42485
                  call "MANUAL" ("PRJINPUT")
                  goto L42170

L42485:        if keyhit% <> 15 then L42505
                  call "PRNTSCRN"
                  goto L42170

L42505:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   2       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 2 OF DOCUMENT.                    *~
            *************************************************************

            deffn'212(fieldnr%)
                  if fieldnr% = 0% then inpmessage$ = edtmessage$
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L43260,         /* HNY TRANSFERRED  */~
                                    L43260,         /* SALES TRANSFERRED*/~
                                    L43260,         /* MISC TRASFERRED  */~
                                    L43200,         /* LABOR G/L ACCT.  */~
                                    L43200,         /* LAB OVHD G/L ACCT*/~
                                    L43200,         /* PURCH G/L ACCT.  */~
                                    L43200,         /* MATER. G/L ACCT. */~
                                    L43200,         /* MAT OVHD G/L ACCT*/~
                                    L43200          /* MISC. G/L ACCT.  */

                     goto L43300

L43200:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
                  REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L43260:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L43300:     accept                                                       ~
               at (01,02), "Manage Project Master File and Project Budget~
        ~s"                                                      ,        ~
               at (01,60), "Todays Date: ",                              ~
               at (01,73), fac(hex(8c)), date$,                          ~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(84)), infomsg$               , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "$$ Transferred To Inventory",                         ~
               at (06,35), fac(lfac$( 1)), hnytran$             , ch(10),~
               at (07,02),                                               ~
                  "Sales Credited To This Project",                      ~
               at (07,35), fac(lfac$( 2)), slstran$             , ch(12),~
               at (08,02),                                               ~
                  "Costs Transf To Other Projects",                      ~
               at (08,35), fac(lfac$( 3)), misctran$            , ch(10),~
               at (09,02),                                               ~
                  "Labor G/L Account",                                   ~
               at (09,35), fac(lfac$( 4)), lbracct$             , ch(12),~
               at (10,02),                                               ~
                  "Labor Overhead G/L Account",                          ~
               at (10,35), fac(lfac$( 5)), lovhdacct$           , ch(12),~
               at (11,02),                                               ~
                  "Purchase G/L Account",                                ~
               at (11,35), fac(lfac$( 6)), prchseacct$          , ch(12),~
               at (12,02),                                               ~
                  "Material G/L Account",                                ~
               at (12,35), fac(lfac$( 7)), mtrialacct$          , ch(12),~
               at (13,02),                                               ~
                  "Material Overhead G/L Acct",                          ~
               at (13,35), fac(lfac$( 8)), movhdacct$           , ch(12),~
               at (14,02),                                               ~
                  "Miscellaneous G/L Account",                           ~
               at (14,35), fac(lfac$( 9)), miscelacct$          , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (24,02), "(2)Project Budget",                          ~
               at (23,20), "(4)Previous Page",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Save Data",                              ~
                                                                         ~
               keys(hex(000102040d0f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L43800
                  call "MANUAL" ("PRJINPUT")
                  goto L43300

L43800:        if keyhit% <> 15 then L43840
                  call "PRNTSCRN"
                  goto L43300

L43840:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *   L I N E    I T E M   S C R E E N S                      *~
            *                                                           *~
            * BUDGET ITEM SCREENS                                       *~
            *************************************************************

            deffn'203(fieldnr%)                    /* INPUT MODE      */
                  screen% = 1
                   goto L44280

            deffn'213(fieldnr%)                    /* EDIT MODE        */
                  screen% = 2
                   init(hex(86)) fac$()
                   if fieldnr% = 0 then L44290
                    goto L44280

            deffn'223(fieldnr%)                    /* INSERT MODE      */
                  screen% = 3
                  goto L44280

            deffn'233(screenline%)                 /* DELETE MODE      */
                  screen% = 4
                  init(hex(84)) fac$()
                  for temp% = 1 to 3
                      fac$(screenline%, temp%) = hex(94)
                      next temp%
                  goto L44470

L44280:           init(hex(84)) fac$()
L44290:           on fieldnr% gosub L44400,         /* BUDGET TYPE      */~
                                    L44350,         /* BUD. DESCRIPTION */~
                                    L44430          /* BUD.  AMOUNT     */

                  goto L44470

L44350:           REM SET FAC'S FOR UPPER/LOWER CASE INPUT
               if budtype$(c%) = "P" then L44400    /*UPPER CASE ONLY   */
               if budtype$(c%) = "W" then L44400    /* FOR THESE        */
                      fac$(screenline%, fieldnr%) = hex(80)
                      return
L44400:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(screenline%, fieldnr%) = hex(81)
                      return
L44430:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      fac$(screenline%, fieldnr%) = hex(82)
                      return

L44470:     accept                                                       ~
               at (01,02), fac(hex(8c)),     title$(screen%,1)  , ch(64),~
               at (02,02), fac(hex(8c)),     title$(screen%,2)  , ch(64),~
               at (03,02), fac(hex(94)),     errormsg$          , ch(60),~
               at (04,02), fac(hex(84)),     infomsg$           , ch(79),~
               at (01,66), "++++++++++++++",                             ~
               at (02,66), "!",                                          ~
               at (02,68), fac(hex(8c)), prjnr$                 , ch(08),~
               at (02,79), "!",                                          ~
               at (03,66), "++++++++++++++",                             ~
                                                                         ~
               at (05,02), "Type"                                       ,~
               at (05,08), fac(fac$( 1,1)), budtype$ (line%+ 1) , ch(02),~
               at (06,08), fac(fac$( 2,1)), budtype$ (line%+ 2) , ch(02),~
               at (07,08), fac(fac$( 3,1)), budtype$ (line%+ 3) , ch(02),~
               at (08,08), fac(fac$( 4,1)), budtype$ (line%+ 4) , ch(02),~
               at (09,08), fac(fac$( 5,1)), budtype$ (line%+ 5) , ch(02),~
               at (10,08), fac(fac$( 6,1)), budtype$ (line%+ 6) , ch(02),~
               at (11,08), fac(fac$( 7,1)), budtype$ (line%+ 7) , ch(02),~
               at (12,08), fac(fac$( 8,1)), budtype$ (line%+ 8) , ch(02),~
               at (13,08), fac(fac$( 9,1)), budtype$ (line%+ 9) , ch(02),~
               at (14,08), fac(fac$(10,1)), budtype$ (line%+10) , ch(02),~
               at (15,08), fac(fac$(11,1)), budtype$ (line%+11) , ch(02),~
               at (16,08), fac(fac$(12,1)), budtype$ (line%+12) , ch(02),~
               at (17,08), fac(fac$(13,1)), budtype$ (line%+13) , ch(02),~
               at (18,08), fac(fac$(14,1)), budtype$ (line%+14) , ch(02),~
               at (19,08), fac(fac$(15,1)), budtype$ (line%+15) , ch(02),~
               at (20,08), fac(fac$(16,1)), budtype$ (line%+16) , ch(02),~
               at (21,08), fac(fac$(17,1)), budtype$ (line%+17) , ch(02),~
               at (22,08), fac(fac$(18,1)), budtype$ (line%+18) , ch(02),~
               at (23,08), fac(fac$(19,1)), budtype$ (line%+19) , ch(02),~
               at (24,08), fac(fac$(20,1)), budtype$ (line%+20) , ch(02),~
                                                                         ~
               at (05,16), "Desc"                                       ,~
               at (05,21), fac(fac$( 1,2)), buddescr$ (line%+ 1), ch(30),~
               at (06,21), fac(fac$( 2,2)), buddescr$ (line%+ 2), ch(30),~
               at (07,21), fac(fac$( 3,2)), buddescr$ (line%+ 3), ch(30),~
               at (08,21), fac(fac$( 4,2)), buddescr$ (line%+ 4), ch(30),~
               at (09,21), fac(fac$( 5,2)), buddescr$ (line%+ 5), ch(30),~
               at (10,21), fac(fac$( 6,2)), buddescr$ (line%+ 6), ch(30),~
               at (11,21), fac(fac$( 7,2)), buddescr$ (line%+ 7), ch(30),~
               at (12,21), fac(fac$( 8,2)), buddescr$ (line%+ 8), ch(30),~
               at (13,21), fac(fac$( 9,2)), buddescr$ (line%+ 9), ch(30),~
               at (14,21), fac(fac$(10,2)), buddescr$ (line%+10), ch(30),~
               at (15,21), fac(fac$(11,2)), buddescr$ (line%+11), ch(30),~
               at (16,21), fac(fac$(12,2)), buddescr$ (line%+12), ch(30),~
               at (17,21), fac(fac$(13,2)), buddescr$ (line%+13), ch(30),~
               at (18,21), fac(fac$(14,2)), buddescr$ (line%+14), ch(30),~
               at (19,21), fac(fac$(15,2)), buddescr$ (line%+15), ch(30),~
               at (20,21), fac(fac$(16,2)), buddescr$ (line%+16), ch(30),~
               at (21,21), fac(fac$(17,2)), buddescr$ (line%+17), ch(30),~
               at (22,21), fac(fac$(18,2)), buddescr$ (line%+18), ch(30),~
               at (23,21), fac(fac$(19,2)), buddescr$ (line%+19), ch(30),~
               at (24,21), fac(fac$(20,2)), buddescr$ (line%+20), ch(30),~
                                                                         ~
               at (05,52), "Amount"                                     ,~
               at (05,59), fac(fac$( 1,3)), budamount$(line%+ 1), ch(10),~
               at (06,59), fac(fac$( 2,3)), budamount$(line%+ 2), ch(10),~
               at (07,59), fac(fac$( 3,3)), budamount$(line%+ 3), ch(10),~
               at (08,59), fac(fac$( 4,3)), budamount$(line%+ 4), ch(10),~
               at (09,59), fac(fac$( 5,3)), budamount$(line%+ 5), ch(10),~
               at (10,59), fac(fac$( 6,3)), budamount$(line%+ 6), ch(10),~
               at (11,59), fac(fac$( 7,3)), budamount$(line%+ 7), ch(10),~
               at (12,59), fac(fac$( 8,3)), budamount$(line%+ 8), ch(10),~
               at (13,59), fac(fac$( 9,3)), budamount$(line%+ 9), ch(10),~
               at (14,59), fac(fac$(10,3)), budamount$(line%+10), ch(10),~
               at (15,59), fac(fac$(11,3)), budamount$(line%+11), ch(10),~
               at (16,59), fac(fac$(12,3)), budamount$(line%+12), ch(10),~
               at (17,59), fac(fac$(13,3)), budamount$(line%+13), ch(10),~
               at (18,59), fac(fac$(14,3)), budamount$(line%+14), ch(10),~
               at (19,59), fac(fac$(15,3)), budamount$(line%+15), ch(10),~
               at (20,59), fac(fac$(16,3)), budamount$(line%+16), ch(10),~
               at (21,59), fac(fac$(17,3)), budamount$(line%+17), ch(10),~
               at (22,59), fac(fac$(18,3)), budamount$(line%+18), ch(10),~
               at (23,59), fac(fac$(19,3)), budamount$(line%+19), ch(10),~
               at (24,59), fac(fac$(20,3)), budamount$(line%+20), ch(10),~
                                                                         ~
                                                                         ~
               keys(pfkeys$(screen%)),                                   ~
               key (keyhit%)

               if keyhit% <> 13 then L45280
                  call "MANUAL" ("PRJINPUT")
                  goto L44470

L45280:        if keyhit% <> 15 then L45320
                  call "PRNTSCRN"
                  goto L44470

L45320:        if screen% <> 2 then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  infomsg$,  errormsg$ = " "
                  on fieldnr% gosub L50220,         /* PRJ NUMBER       */~
                                    L50360,         /* PRJ DESCRIPTION  */~
                                    L50390,         /* DATE OPENED      */~
                                    L50430,         /* DATE CLOSED      */~
                                    L50520,         /* CUSTOMER CODE    */~
                                    L50590,         /* PART NUMBER      */~
                                    L50660,         /* QUANTITY         */~
                                    L50700,         /* QUANTITY COMPLETE*/~
                                    L50740,         /* DIRECT LABOR     */~
                                    L50780,         /* TOTAL PURCHASES  */~
                                    L50820,         /* TOTAL MATERIAL   */~
                                    L50860,         /* TOTAL COMMITTED  */~
                                    L50900          /* TOTAL LAB OVERHED*/
                     return
L50220: REM Test Data For Project Number
            prjonfile% = 0
            if prjnr$ <> " " then L50250
                errormsg$ = "Please Enter Project Number"
                return

L50250:     call "GETCODE" (#9, prjnr$, " ", 0%, 0.0, f1%(9))
                if f1%(9) <> 0 then L50300
                call "DESCRIBE" (#5, prjnr$, " ", 0%, f1%(5))
                     if f1%(5) = 0 then return
                errormsg$ = "Prj Number " & prjnr$ & " is already " &    ~
                            "assigned to a Production Prj."
                return
L50300:     gosub L30000
            gosub L30720
            return clear all
            prjonfile% = 1
            goto edtpg1

L50360: REM Test Data For Project Description
            return

L50390:     REM TEST DATA FOR DATE OPENED
              call "DATEOK" (dateopen$, dateo%, errormsg$)
                return

L50430:     REM TEST DATA FOR DATE CLOSED
             if dateclosed$ = " " or dateclosed$ = blankdate$ then return
             call "DATEOK" (dateclosed$, datec%, errormsg$)
            if errormsg$ <> " " then return
                call "DATEOK" (dateopen$, dateo%, errormsg$)
                if datec% >= dateo% then return
                 errormsg$ = "Must be after Date Opened"
                 return

L50520:     REM TEST DATA FOR CUSTOMER CODE
               cusdescr$ = " "
               if cuscode$ = " " then return
                 call "GETCODE" (#8, cuscode$, cusdescr$, 0%, 1, f1%(8))
                   if f1%(8) = 1 then return
            errormsg$ = "Customer not on file"   :  return

L50590:     REM TEST DATA FOR PART NUMBER TO BE MADE
               if partnr$ = " " then return
                 call "GETCODE" (#6, partnr$, partdescr$, 0%, 0, f1%(6))
                     if f1%(6) = 1 then return
                infomsg$ = "Part not on File"
                   return

L50660:     REM TEST DATA FOR QUANTITY TO BE MADE
                if quantity$ = " " then quantity$ = "0"
                call "NUMTEST" (quantity$, 0, 9e7, errormsg$, 0.2, temp)
                return

L50700:     REM TEST DATA FOR QUANTITY COMPLETED TO DATE
                if quancom$ = " " then quancom$ = "0"
                call "NUMTEST" (quancom$, 0, 9e7, errormsg$, 0.2, temp)
                return

L50740:     REM TEST DATA FOR TOTAL DIRECT LABOR
                if labor$ = " " then labor$ = "0"
                call "NUMTEST" (labor$, 0, 9e7, errormsg$, 2.2, temp)
                return

L50780:     REM TEST DATA FOR TOTAL PURCHASES
                if purchase$ = " " then purchase$ = "0"
                call "NUMTEST" (purchase$, 0, 9e7, errormsg$, 2.2, temp)
                return

L50820:     REM TEST DATA FOR TOTAL MATERIAL
                if material$ = " " then material$ = "0"
                call "NUMTEST" (material$, 0, 9e7, errormsg$, 2.2, temp)
                return

L50860:     REM TEST DATA FOR TOTAL COMMITED INVENTORY
                if committed$ = " " then committed$ = "0"
                call "NUMTEST" (committed$, 0, 9e7, errormsg$, 2.2, temp)
                return

L50900:     REM TEST DATA FOR TOTAL LABOR DOLLARS OVERHEAD
                if overhead$ = " " then overhead$ = "0"
                call "NUMTEST" (overhead$, 0, 9e7, errormsg$, 2.2, temp)
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2.                       *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51200,         /* HNY TRANSFERRED  */~
                                    L51240,         /* SALES TRANSFERRED*/~
                                    L51280,         /* MISC TRASFERRED  */~
                                    L51320,         /* LABOR G/L ACCT   */~
                                    L51400,         /* LAB OVERHEAD ACCT*/~
                                    L51490,         /* PURCH G/L ACCT   */~
                                    L51580,         /* MATERIAL G/L ACCT*/~
                                    L51670,         /* MAT OVERHEAD ACCT*/~
                                    L51760          /* MISC. G/L ACCT.  */
                     return


L51200:     REM TEST DATA FOR $$ TRANSFERRED TO INVENTORY
                if hnytran$ = " " then hnytran$ = "0"
                call "NUMTEST" (hnytran$, 0, 9e7, errormsg$, 2.2, temp)
                return

L51240:     REM TEST DATA FOR $$ TRANSFERRED TO SALES
                if slstran$ = " " then slstran$ = "0"
                call "NUMTEST"(slstran$,0,9999999999.9,errormsg$,2.2,temp)
                return

L51280:     REM TEST DATA FOR $$ TRANSFERRED MISC.
                if misctran$ = " " then misctran$ = "0"
                call "NUMTEST" (misctran$, 0, 9e7, errormsg$, 2.2, temp)
                return

L51320:     REM TEST DATA FOR THE LABOR G/L ACCOUNT
             if lbracct$ = " " then return
             if lbracct$ = "?" then lbracct$ = " "
             call "GETCODE" (#10, lbracct$, infomsg$, 1%, 0, f1%(10))
                   if f1%(10) = 1 then L51370
                   if lbracct$ = " " then return
             errormsg$ = "Labor account not on file " & lbracct$ : return
L51370:      str(infomsg$, 35) = "Acct not posted to G/L in this program"
             return

L51400:     REM TEST DATA FOR THE LABOR OVERHEAD G/L ACCOUNT
             if lovhdacct$ = " " then return
             if lovhdacct$ = "?" then lovhdacct$ = " "
             call "GETCODE" (#10, lovhdacct$, infomsg$, 1%, 0, f1%(10))
                   if f1%(10) = 1 then L51460
                   if lovhdacct$ = " " then return
             errormsg$ = "Overhead account not on file " & lovhdacct$
             return
L51460:      str(infomsg$,35) = "Acct not posted to G/L in this program"
             return

L51490:     REM TEST DATA FOR THE PURCHASE G/L ACCOUNT
             if prchseacct$ = " " then return
             if prchseacct$ = "?" then prchseacct$ = " "
             call "GETCODE" (#10, prchseacct$, infomsg$, 1%, 0, f1%(10))
                   if f1%(10) = 1 then L51550
                   if prchseacct$ = " " then return
             errormsg$ = "Purchase account not on file " & prchseacct$
             return
L51550:      str(infomsg$, 35) = "Acct not posted to G/L in this program"
             return

L51580:     REM TEST DATA FOR THE MATERIAL G/L ACCOUNT
             if mtrialacct$ = " " then return
             if mtrialacct$ = "?" then mtrialacct$ = " "
             call "GETCODE" (#10, mtrialacct$, infomsg$, 1%, 0, f1%(10))
                   if f1%(10) = 1 then L51640
                   if mtrialacct$ = " " then return
             errormsg$ = "Material account not on file " & mtrialacct$
             return
L51640:      str(infomsg$, 35) = "Acct not posted to G/L in this program"
             return

L51670:     REM TEST DATA FOR THE MATERAIL OVERHEAD G/L ACCOUNT
             if movhdacct$ = " " then return
             if movhdacct$ = "?" then movhdacct$ = " "
             call "GETCODE" (#10, movhdacct$, infomsg$, 1%, 0, f1%(10))
                   if f1%(10) = 1 then L51730
                   if movhdacct$ = " " then return
             errormsg$ = "Overhead account not on file " & movhdacct$
             return
L51730:      str(infomsg$,35) = "Acct not posted to G/L in this program"
             return

L51760:     REM TEST DATA FOR THE MISCELLANEOUS G/L ACCOUNT
             if miscelacct$ = " " then return
             if miscelacct$ = "?" then miscelacct$ = " "
             call "GETCODE" (#10, miscelacct$, infomsg$, 1%, 0, f1%(10))
                   if f1%(10) = 1 then L51820
                   if miscelacct$ = " " then return
             errormsg$ = "Misc. account not on file " & miscelacct$
             return
L51820:      str(infomsg$, 35) = "Acct not posted to G/L in this program"
             return

        REM *************************************************************~
            * E D I T   L I N E   I T E M S                             *~
            *                                                           *~
            *************************************************************
             deffn'153(fieldnr%)

                  errormsg$, infomsg$ = " "
                  c% = currentline%
                  on fieldnr% gosub L52130,         /* BUDGET TYPE      */~
                                    L52290,         /* BUDGET DECRIPTION*/~
                                    L52500          /* BUDGET AMOUNT    */

                  return
L52130: REM BUDGET TYPE SET UP DEFAULTS FOR TYPE
            if budtype$(c%) = " " then L52230
            if budtype$(c%) = "I" then L52250
            if budtype$(c%) = "P" then infomsg$ = "Description for this e~
        ~ntry must be a General Ledger Account"

            if budtype$(c%) = "W" then infomsg$ = "Description for this e~
        ~ntry is informational only!"

            return
L52230:     errormsg$ = "Budget type cannot be blank"     :  return

L52250: REM INVENTORY MOVEMENT DESCRIPTION FIELD
            buddescr$(c%) = "Total Inventory"
            return

L52290: REM BUDGET DESCRIPTION
             if budtype$(c%) = "P" then L52340
             return

L52340: REM CHECK TO MAKE SURE THE DESCRIPTION IS A GENERAL LEDGER NO.
*       ** IF BUDDESCR$(C%) = " " THEN 52400
            call "GETCODE" (#10, buddescr$(c%), infomsg$,0%,0,f1%(10))
               if buddescr$(c%) = " " then L52400
               if f1%(10) = 0 then L52390
               return
L52390:     errormsg$ = "Account number not on file"      :  return
L52400:     errormsg$ = "Account number cannot be blank"  :  return

L52500:     REM TEST DATA FOR BUDGET AMOUNT
            call "NUMTEST" (budamount$(currentline%), 0, 9e7, errormsg$, ~
                                                               2.2, temp)
            return

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")

            end
