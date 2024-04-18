        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   PPPP   L      IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  A   A  P   P  L        I    NN  N  P   P  U   U    T     *~
            *  AAAAA  PPPP   L        I    N N N  PPPP   U   U    T     *~
            *  A   A  P      L        I    N  NN  P      U   U    T     *~
            *  A   A  P      LLLLL  IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APLINPUT - ENTER/EDIT/MANAGE APPLICANT INFORMATION.  HAS  *~
            *            EXTRA PAGES OF DUMMY INPUT IN PLACE FOR        *~
            *            EASE OF CUSTOMIZING.  SUBS HANDLE THE          *~
            *            INTERVIEWER TEXT AND APPLICANT SKILLS BANK.    *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/14/83 ! ORIGINAL                                 ! GLW *~
            * 02/14/89 ! Happy Valentine's Day...                 ! MJB *~
            *          !  Removed BIRTHDATE from screens,         !     *~
            *          !  Defaulted Last Interview Date to current!     *~
            *          !  date and standardized program.          !     *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 07/23/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            car$60,                      /* CARRIER $ INTO 'SKILLSUB'  */~
            city$20,                     /* CITY                       */~
            county$20,                   /* COUNTY                     */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            dateaccepted$8,              /* DATE APPLICANT ACCEPTED JOB*/~
            dateaccepteddescr$32,        /* DATE APPLICANT ACCEPTED JOB*/~
            dateoffered$8,               /* DATE JOB OFFERED           */~
            dateoffereddescr$32,         /* DATE JOB OFFERED           */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            eeotitle$16,                 /* FOR JOB CLASS - EEO        */~
            employee$12,                 /* EMPLOYEE CODE ASSIGNED     */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            expcode$1,                   /* EXPERIENCE CODE            */~
            exppay$6,                    /* EXPECTED RATE OF PAY       */~
            fname$10,                    /* FIRST NAME                 */~
            gender$1,                    /* GENDER                     */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            joboffered$16,               /* ACTUAL JOB OFFERED         */~
            joboffereddescr$32,          /* ACTUAL JOB OFFERED         */~
            jobtitle$16,                 /* FOR JOB TITLE              */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            lintrby$16,                  /* LAST INTERVIEWED BY        */~
            lintron$8,                   /* LAST INTERVIEWED ON        */~
            line2$79,                    /* Screen Line 2              */~
            lname$15,                    /* LAST NAME                  */~
            mname$1,                     /* MIDDLE INITIAL             */~
            pass$162,                    /* FOR COMSUB                 */~
            offpay$6,                    /* RATE OF PAY OFFERED        */~
            rest1$240,                   /* REST OF RECORD             */~
            rest2$240,                   /* REST OF RECORD             */~
            req$5,                       /* FOR REQUISITION NUMBER     */~
            ssnum$11,                    /* SOCIAL SECURITY NUMBER     */~
            state$2,                     /* STATE                      */~
            status$(6)8,                 /* STATUS INFO                */~
            street1$30,                  /* STREET ADDRESS             */~
            street2$30,                  /* STREET ADDRESS             */~
            telephone$10,                /* TELEPHONE                  */~
            zip$9                        /* ZIP                        */~

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20                  /* TEXT FROM FILE OPENING     */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! APLSKILL ! Applicant skills inventory - personnel s *~
            * #02 ! REQSKILL ! Skills required for a requisition - pers *~
            * #03 ! EMPSKILL ! Employee skills inventory - personnel sy *~
            * #04 ! COMTERM  ! File of common terms for personel.       *~
            * #05 ! APLMASTR ! Applicant master file - part of personne *~
            * #06 ! REQMASTR ! Requisition master file - personnel syst *~
            * #07 ! EMPMASTR ! Employee master file                     *~
            * #08 ! PERMASTR ! Personnel master file-ties to EMPMASTR i *~
            * #09 ! PERFRNGE ! Fringe benefit file - personnel system   *~
            * #10 ! INSMASTR ! INSURANCE MASTER FILE - PERSONNEL SYSTEM *~
            * #11 ! HISMASTR ! EMPLOYMENT HISTORY MASTER FILE           *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *~
            *                                                           *~
            *************************************************************

            select #01, "APLSKILL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  100,                                  ~
                        keypos =   17, keylen =  13,                     ~
                        alt key  1, keypos =    1, keylen =  29

            select #02, "REQSKILL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  100,                                  ~
                        keypos =   17, keylen =   7,                     ~
                        alt key  1, keypos =    1, keylen =  23

            select #03, "EMPSKILL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  100,                                  ~
                        keypos =   17, keylen =  14,                     ~
                        alt key  1, keypos =    1, keylen =  30

            select #04, "COMTERM",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  162,                                  ~
                        keypos =   47, keylen =  16,                     ~
                        alt key  1, keypos =   17, keylen =  46,         ~
                            key  2, keypos =    1, keylen =  62

            select #05, "APLMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  600,                                  ~
                        keypos =   82, keylen =  11,                     ~
                        alt key  1, keypos =   56, keylen =  37,         ~
                            key  2, keypos =   50, keylen =  43,         ~
                            key  3, keypos =   34, keylen =  59,         ~
                            key  4, keypos =   18, keylen =  75,         ~
                            key  5, keypos =   13, keylen =  80,         ~
                            key  6, keypos =    1, keylen =  92

            select #06, "REQMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  370,                                  ~
                        keypos =   30, keylen =   5,                     ~
                        alt key  1, keypos =   24, keylen =  11,         ~
                            key  2, keypos =    8, keylen =  27,         ~
                            key  3, keypos =    7, keylen =  28,         ~
                            key  4, keypos =    1, keylen =  34

            select #07, "EMPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                         recsize = 136,                                  ~
                         keypos = 1, keylen = 12,                        ~
                         alt key  1, keypos = 70, keylen =  1, dup

            select #08, "PERMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  950,                                  ~
                        keypos =  39,  keylen = 12,                      ~
                        alt key  1, keypos =   28, keylen =  23,         ~
                            key  2, keypos =    2, keylen =  49,         ~
                            key  3, keypos =    1, keylen =  50

            select #09, "PERFRNGE",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 250,                                   ~
                        keypos = 17,   keylen = 15,                      ~
                     alt key 1, keypos = 1, keylen = 31

            select #10, "INSMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 800,                                   ~
                        keypos = 17,   keylen = 15,                      ~
                        alt key  1, keypos =    1, keylen =  31

            select #11, "EMPHSTRY",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =   39, keylen =  15,                     ~
                        alt key  1, keypos =   23, keylen =  31,         ~
                            key  2, keypos =    7, keylen =  47,         ~
                            key  3, keypos =    1, keylen =  53

*        Check to See if Payroll/Personnel is Active
            call "PRLEXTSB" ("PRL", prl%)
                if prl% = 99% then end (prl%)

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, 0%, f2%(01), 100%, rslt$(01))
            call "OPENCHCK" (#02, 0%, f2%(02), 100%, rslt$(02))
            call "OPENCHCK" (#03, 0%, f2%(03), 100%, rslt$(03))
            call "OPENCHCK" (#04, 0%, f2%(04), 100%, rslt$(04))
            call "OPENCHCK" (#05, 0%, f2%(05), 100%, rslt$(05))
            call "OPENCHCK" (#06, 0%, f2%(06), 100%, rslt$(06))
            call "OPENCHCK" (#07, 0%, f2%(07), 100%, rslt$(07))
            call "OPENCHCK" (#08, 0%, f2%(08), 100%, rslt$(08))
            call "OPENCHCK" (#09, 0%, f2%(09), 100%, rslt$(09))
            call "OPENCHCK" (#10, 0%, f2%(10), 100%, rslt$(10))
            call "OPENCHCK" (#11, 0%, f2%(11), 100%, rslt$(11))


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" ( blankdate$ )

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (RETURN)"

            str(line2$,62) = "APLINPUT: " & str(cms2v$,1,8)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode

            init (" ") city$, county$, dateaccepted$,                    ~
                 dateaccepteddescr$, dateoffered$, dateoffereddescr$,    ~
                 eeotitle$, employee$, errormsg$, expcode$, exppay$,     ~
                 fname$, gender$, inpmessage$, joboffered$,              ~
                 joboffereddescr$, jobtitle$, lintrby$, lintron$,        ~
                 lname$, mname$, offpay$, req$, ssnum$, status$(),       ~
                 state$, street1$, street2$, telephone$, zip$


            for fieldnr% = 1 to 13
L10100:         gosub'051(fieldnr%)
                      if enabled% = 0 then L10171
L10120:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  4% then goto L10173
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10120
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
L10171:         if keyhit% <> 4% then goto L10180
L10173:               fieldnr% = max(1%, fieldnr% - 1%)
                      goto L10100
L10180:         next fieldnr%

            for fieldnr% = 1 to 15
L10210:         gosub'052(fieldnr%)
                      if enabled% = 0 then L10272
L10230:         gosub'102(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  =  4% then goto L10274
                      if keyhit% <>  0 then       L10230
                gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10230
L10272:         if keyhit% <> 4% then goto L10280
L10274:               fieldnr% = max(1%, fieldnr% - 1%)
                      goto L10210
L10280:         next fieldnr%

            car$ = str(fname$,1,len(fname$)) & " " & str(mname$,1,1) &   ~
                                  " " & str(lname$,1,len(lname$))

            call "SKILLSUB" (#1, #2, #3, #4, #5, #6, #8, 1%, ssnum$, car$)

            goto edtpg1


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        edtpg1
            init(" ") inpmessage$
L11070:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  5 then       edtpg2
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11070
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% > 13 then L11070
            if fieldnr% =  4% then fieldnr% =  3%
            if fieldnr% =  5% then fieldnr% =  3%
            if fieldnr% =  7% then fieldnr% =  6%
            if fieldnr% =  8% then fieldnr% =  6%
            if fieldnr% =  9% then fieldnr% =  6%
            if fieldnr% = 10% then fieldnr% =  6%
            if fieldnr% = 11% then fieldnr% =  6%

            gosub'051(fieldnr%)
L11150:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11150
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11150
            goto edtpg1

        edtpg2
            init(" ") inpmessage$
L11230:     gosub'112(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  4 then       edtpg1
                  if keyhit%  =  5 then       L11230
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L11230
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% > 15 then L11230

           gosub'052(fieldnr%)
L11320:     gosub'112(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11320
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L11320
            goto edtpg2

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub L30000
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  init(" ") inpmessage$
                  enabled% = 1
                  on fieldnr% gosub L20100,         /* SS NUMBER        */~
                                    L20115,         /* APPLIC STATUS    */~
                                    L20150,         /* LAST NAME        */~
                                    L20200,         /* FIRST NAME       */~
                                    L20250,         /* MIDDLE INITIAL   */~
                                    L20300,         /* STREET ADDRESS   */~
                                    L20350,         /* STREET ADDRESS   */~
                                    L20400,         /* CITY             */~
                                    L20450,         /* COUNTY           */~
                                    L20500,         /* STATE            */~
                                    L20550,         /* ZIP              */~
                                    L20600,         /* TELEPHONE        */~
                                    L20700          /* GENDER           */
                     return

L20100: REM DEFAULT/ENABLE FOR SOCIAL SECURITY NUMBER
            inpmessage$ = "Enter Dashes If You Want Them, Just Be Consist~
        ~ent"
            return

L20115: REM DEFAULT/ENABLE FOR APPLICANT STATUS
            status$(1%) = "ACTIVE"
            inpmessage$ = "Enter 'A' For Active Or 'I' For Inactive"
            return

L20150: REM DEFAULT/ENABLE FOR LAST NAME
            inpmessage$ = "Tab To Enter All Three Parts Of The Name"
            return

L20200: REM DEFAULT/ENABLE FOR FIRST NAME
            if lname$ <> " " then enabled% = 0%
            return

L20250: REM DEFAULT/ENABLE FOR MIDDLE INITIAL
            if lname$ <> " " then enabled% = 0%
            return

L20300: REM DEFAULT/ENABLE FOR STREET ADDRESS
            inpmessage$ = "Tab To Enter All Of The Address Lines At Once"
            return

L20350: REM DEFAULT/ENABLE FOR STREET ADDRESS
            if street1$ <> " " then enabled% = 0%
            return

L20400: REM DEFAULT/ENABLE FOR CITY
            if street1$ <> " " then enabled% = 0%
            return

L20450: REM DEFAULT/ENABLE FOR COUNTY
            if street1$ <> " " then enabled% = 0%
            return

L20500: REM DEFAULT/ENABLE FOR STATE
            if street1$ <> " " then enabled% = 0%
            return

L20550: REM DEFAULT/ENABLE FOR ZIP
            if street1$ <> " " then enabled% = 0%
            return

L20600: REM DEFAULT/ENABLE FOR TELEPHONE
            inpmessage$ = "No Dashes are Needed Between the Area Code" & ~
                          " and the Number"
            return

L20700: REM DEFAULT/ENABLE FOR GENDER
            inpmessage$ = "Enter 'M' For Male, 'F' For Female Or '?'" &  ~
                         " for Unknown"
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 2 OF INPUT. *~
            *************************************************************

            deffn'052(fieldnr%)
                  init(" ") inpmessage$
                  enabled% = 1
                  on fieldnr% gosub L21100,         /* REQ NUM          */~
                                    L21150,         /* FOR JOB TITLE    */~
                                    L21200,         /* EEO CLASS        */~
                                    L21250,         /* LAST INTR BY     */~
                                    L21300,         /* LAST INTR ON     */~
                                    L21350,         /* EXP RATE OF PAY  */~
                                    L21400,         /* EXPERIENCE CODE  */~
                                    L21410,         /* PHYSICAL STATUS  */~
                                    L21420,         /* MILITARY STATUS  */~
                                    L21430,         /* USER SET STATUS  */~
                                    L21430,         /* USER SET STATUS  */~
                                    L21445,         /* REFERRED BY      */~
                                    L21450,         /* DATE JOB OFFERED */~
                                    L21600,         /* DATE ACCEPTED    */~
                                    L21650          /* EMP CODE ASSIGNED*/
                     return
L21100: REM DEFAULT/ENABLE FOR FOR REQUISITION NUMBER
            return

L21150: REM DEFAULT/ENABLE FOR FOR JOB TITLE
            inpmessage$ = "Enter The Job Title This Applicant Seeking"
            return

L21200: REM DEFAULT/ENABLE FOR FOR JOB CLASS - EEO
            inpmessage$ = "Enter The EEO Class For This Job"
            return

L21250: REM DEFAULT/ENABLE FOR LAST INTERVIEWED BY
            inpmessage$ = "Who Last Interviewed This Applicant?"
            return

L21300: REM DEFAULT/ENABLE FOR LAST INTERVIEWED ON
            if lintron$ = " " or lintron$ = blankdate$ then lintron$ = date$
            inpmessage$ = "Enter The Date Of The Last Interview"
            return

L21350: REM DEFAULT/ENABLE FOR EXPECTED RATE OF PAY
            inpmessage$ = "Enter The Rate Of Pay In Dollars Per Hour"
            return

L21400: REM DEFAULT/ENABLE FOR EXPERIENCE CODE
            inpmessage$ = "Excellent = 1, Good = 2, Fair = 3, Poor = 4"
            return

L21410: REM DEFAULT/ENABLE FOR PHYSICAL STATUS
            status$(2) = "NO HANDCP"
            inpmessage$ = "If Handicapped, Enter Class Of Handicap"
            return

L21420: REM DEFAULT/ENABLE FOR MILITARY STATUS
            status$(3) = "NONE    "
            inpmessage$ = "If Military Service, Enter Branch Followed By ~
        ~ 'R' If Now In Reserve"
            return

L21430: REM DEFAULT/ENABLE FOR USER SET STATUS
            enabled% = 0%
            return

L21445: REM DEFAULT/ENABLE FOR REFERRED BY
            inpmessage$ = "Who Referred This Applicant To Us?"
            return

L21450: REM DEFAULT/ENABLE FOR DATE JOB OFFERED
            inpmessage$ = "Leave Blank Until Job Actually Offered"
            return

L21600: REM DEFAULT/ENABLE FOR DATE APPLICANT ACCEPTED JOB
           inpmessage$ = "Leave Blank Until Applicant Accepts Job"
           if dateoffered$ = " " or dateoffered$ = blankdate$ then enabled% = 0%
           return

L21650: REM DEFAULT/ENABLE FOR EMPLOYEE CODE ASSIGNED
           inpmessage$ = "Enter The Actual Employee Code Assigned To" & ~
                         " This Applicant After Acceptance"
           if dateoffered$ = " " or dateoffered$ = blankdate$ then enabled% = 0%
           return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the user the ability to start over when he wants to *~
            * else return to the menu.  Notice that he has to push 2    *~
            * different buttons to start over--a little harder.         *~
            *************************************************************

        startover
L29940:     keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1%  = 1% then return
            if keyhit1% <> 0% then L29940
                return clear all
                goto inputmode


L30000: REM *************************************************************~
            *        S A V E   D A T A   O N   F I L E                  *~
            *                                                           *~
            * DELETE OLD RECORD AND WRITE A NEW ONE                     *~
            *************************************************************

           call "READ101" (#5, ssnum$, f1%(5))
           if f1%(5) = 1% then delete #5

           call "DATUNFMT" (lintron$)
           call "DATUNFMT" (dateoffered$)
           call "DATUNFMT" (dateaccepted$)

        put #5, using L35030, employee$, req$, jobtitle$, lintrby$,       ~
                             lintron$, lname$, fname$, mname$, ssnum$,   ~
                             eeotitle$, telephone$, street1$, street2$,  ~
                             city$, county$, state$, zip$, gender$,      ~
                             exppay$, expcode$, dateoffered$,            ~
                             dateaccepted$, status$(), rest1$, rest2$

           write #5
           return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

L35030: FMT                      /* FILE: APLMASTR                     */~
            CH(12),              /* employee code                      */~
            CH(5),               /* Requisition for employee, part of  */~
            CH(16),              /* job title                          */~
            CH(16),              /* Last interviewed by - part of pers */~
            CH(6),               /* Last interviewed on - part of pers */~
            CH(15),              /* Last name of person - part of pers */~
            CH(10),              /* First name of person               */~
            CH(1),               /* Middle name of person              */~
            CH(11),              /* Social security number             */~
            CH(16),              /* Job classification as per EEO.     */~
            CH(10),              /* Telephone number                   */~
            CH(30),              /* Street address line 1              */~
            CH(30),              /* Street address line 2              */~
            CH(20),              /* City in address                    */~
            CH(20),              /* County in address                  */~
            CH(2),               /* State in address                   */~
            CH(9),               /* Zip code in address                */~
            XX(6),               /* Not Used                           */~
            CH(1),               /* Gender of a person                 */~
            CH(6),               /* Expected rate of pay - part of per */~
            CH(1),               /* Experience rating or code - part o */~
            CH(6),               /* Date job offered - part of personn */~
            CH(6),               /* Date job accepted - part of person */~
            6*CH(8),             /* ALL SIX STATUS FIELDS              */~
            CH(152),             /* filler for rest of record or inter */~
            CH(129)              /* filler for rest of record or inter */


L36100: REM *************************************************************~
            *        G E T   D A T A   F R O M   F I L E                *~
            *                                                           *~
            *************************************************************

        get #5, using L35030, employee$, req$, jobtitle$, lintrby$,       ~
                             lintron$, lname$, fname$, mname$, ssnum$,   ~
                             eeotitle$, telephone$, street1$, street2$,  ~
                             city$, county$, state$, zip$, gender$,      ~
                             exppay$, expcode$, dateoffered$,            ~
                             dateaccepted$, status$(), rest1$, rest2$

           if dateoffered$ = " " or dateoffered$ = blankdate$ then goto L39620
           call "DATEOK" (dateoffered$, err%, errormsg$)
L39620:    if dateaccepted$ = " " or dateaccepted$ = blankdate$ then goto L39665
           call "DATEOK" (dateaccepted$, err%, errormsg$)
L39665:    if lintron$     = " " or lintron$ = blankdate$ then goto L39700
           call "DATEOK" (lintron$    , err%, errormsg$)
L39700:    return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L40145,         /* SS NUMBER        */~
                                    L40130,         /* APPLIC STATUS    */~
                                    L40130,         /* LAST NAME        */~
                                    L40130,         /* FIRST NAME       */~
                                    L40130,         /* MIDDLE INITIAL   */~
                                    L40130,         /* STREET ADDRESS   */~
                                    L40130,         /* STREET ADDRESS   */~
                                    L40130,         /* CITY             */~
                                    L40130,         /* COUNTY           */~
                                    L40130,         /* STATE            */~
                                    L40130,         /* ZIP              */~
                                    L40145,         /* TELEPHONE        */~
                                    L40130          /* GENDER           */
                     goto L40165

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40130:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40145:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40165:     accept                                                       ~
               at (01,02), "Input Applicant Information, Page 1",        ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$,                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Social Security Number",                     ~
               at (06,30), fac(lfac$( 1)), ssnum$               , ch(11),~
               at (07,02), "Applicant Status      ",                     ~
               at (07,30), fac(lfac$( 2)), status$(1)           , ch(11),~
               at (08,02), "Applicant Last Name",                        ~
               at (08,30), fac(lfac$( 3)), lname$               , ch(15),~
               at (09,02), "         First Name",                        ~
               at (09,30), fac(lfac$( 3)), fname$               , ch(10),~
               at (10,02), "     Middle Initial",                        ~
               at (10,30), fac(lfac$( 3)), mname$               , ch(01),~
               at (11,02), "Address, Line 1",                            ~
               at (11,30), fac(lfac$( 6)), street1$             , ch(30),~
               at (12,02), "         Line 2",                            ~
               at (12,30), fac(lfac$( 6)), street2$             , ch(30),~
               at (13,02), "           City",                            ~
               at (13,30), fac(lfac$( 6)), city$                , ch(20),~
               at (14,02), "         County",                            ~
               at (14,30), fac(lfac$( 6)), county$              , ch(20),~
               at (15,02), "     State Code",                            ~
               at (15,30), fac(lfac$( 6)), state$               , ch(02),~
               at (16,02), "       Zip Code",                            ~
               at (16,30), fac(lfac$(06)), zip$                 , ch(09),~
               at (17,02), "Telephone Number",                           ~
               at (17,30), fac(lfac$(12)), telephone$           , ch(10),~
               at (18,02), "Gender",                                     ~
               at (18,30), fac(lfac$(13)), gender$              , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
                                                                         ~
               at (22,02), "(1)Start Over    (4)Previous Field",         ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                                                                         ~
               keys(hex(0001040d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L40505
                  call "MANUAL" ("APLINPUT")
                  goto L40165

L40505:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40165

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   2      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'102(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L41125,         /* REQ NUM          */~
                                    L41125,         /* FOR JOB TITLE    */~
                                    L41125,         /* EEO CLASS        */~
                                    L41125,         /* LAST INTR BY     */~
                                    L41125,         /* LAST INTR ON     */~
                                    L41140,         /* EXP RATE OF PAY  */~
                                    L41125,         /* EXPERIENCE CODE  */~
                                    L41125,         /* PHYSICAL STATUS  */~
                                    L41125,         /* MILITRY STATUS   */~
                                    L41125,         /* USER SET STATUS  */~
                                    L41125,         /* USER SET STATUS  */~
                                    L41125,         /* REFERRED BY      */~
                                    L41125,         /* DATE JOB OFFERED */~
                                    L41125,         /* DATE ACCEPTED    */~
                                    L41125          /* EMP CODE ASSIGNED*/
                     goto L41160

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L41125:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L41140:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L41160:     accept                                                       ~
               at (01,02), "Input Applicant Information, Page 2",        ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$,                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "For Requisition Number",                     ~
               at (06,30), fac(lfac$( 1)), req$                 , ch(05),~
               at (06,50), "Appl Name",                                  ~
               at (06,65), fac(hex(8c  )), lname$               , ch(15),~
               at (07,02), "For Job Title",                              ~
               at (07,30), fac(lfac$( 2)), jobtitle$            , ch(16),~
               at (07,50), "Appl Status",                                ~
               at (07,65), fac(hex(8c  )), status$(1)           , ch(08),~
               at (08,02), "For Job Class - EEO",                        ~
               at (08,30), fac(lfac$( 3)), eeotitle$            , ch(02),~
               at (09,02), "Last Interviewed By",                        ~
               at (09,30), fac(lfac$( 4)), lintrby$             , ch(16),~
               at (10,02), "Last Interviewed On",                        ~
               at (10,30), fac(lfac$( 5)), lintron$             , ch(08),~
               at (11,02), "Expected Rate Of Pay",                       ~
               at (11,30), fac(lfac$( 6)), exppay$              , ch(06),~
               at (12,02), "Experience Code",                            ~
               at (12,30), fac(lfac$( 7)), expcode$             , ch(01),~
               at (13,02), "Physical Status    ",                        ~
               at (13,30), fac(lfac$( 8)), status$(2)           , ch(08),~
               at (14,02), "Military Status    ",                        ~
               at (14,30), fac(lfac$( 9)), status$(3)           , ch(08),~
               at (15,02), "User Set Status    ",                        ~
               at (15,30), fac(lfac$(10)), status$(4)           , ch(08),~
               at (16,02), "User Set Status     ",                       ~
               at (16,30), fac(lfac$(11)), status$(5)           , ch(08),~
               at (17,02), "Referred By    ",                            ~
               at (17,30), fac(lfac$(12)), status$(6)           , ch(08),~
               at (18,02), "Date Job Offered",                           ~
               at (18,30), fac(lfac$(13)), dateoffered$         , ch(08),~
               at (19,02), "Date Applicant Accepted Job",                ~
               at (19,30), fac(lfac$(14)), dateaccepted$        , ch(08),~
               at (20,02), "Employee Code Assigned",                     ~
                                                                         ~
               at (20,30), fac(lfac$(15)), employee$            , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over     (4)Previous Field",        ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                                                                         ~
               keys(hex(0001040d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L41555
                  call "MANUAL" ("APLINPUT")
                  goto L41160

L41555:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L41160


        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
            if fieldnr% = 0% then inpmessage$ = edtmessage$
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L42300,         /* SS NUMBER        */~
                                    L42270,         /* APPL STATUS      */~
                                    L42270,         /* LAST NAME        */~
                                    L42270,         /* FIRST NAME       */~
                                    L42270,         /* MIDDLE INITIAL   */~
                                    L42270,         /* STREET ADDRESS   */~
                                    L42270,         /* STREET ADDRESS   */~
                                    L42270,         /* CITY             */~
                                    L42270,         /* COUNTY           */~
                                    L42270,         /* STATE            */~
                                    L42270,         /* ZIP              */~
                                    L42300,         /* TELEPHONE        */~
                                    L42270          /* GENDER           */
                     goto L42340

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L42270:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L42300:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L42340:     accept                                                       ~
               at (01,02), "Edit Applicant Information, Page 1",         ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$,                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "Social Security Number",                     ~
               at (06,30), fac(lfac$( 1)), ssnum$               , ch(11),~
               at (07,02), "Applicant Status      ",                     ~
               at (07,30), fac(lfac$( 2)), status$(1)           , ch(11),~
               at (08,02), "Applicant Last Name",                        ~
               at (08,30), fac(lfac$( 3)), lname$               , ch(15),~
               at (09,02), "         First Name",                        ~
               at (09,30), fac(lfac$( 3)), fname$               , ch(10),~
               at (10,02), "     Middle Initial",                        ~
               at (10,30), fac(lfac$( 3)), mname$               , ch(01),~
               at (11,02), "Address, Line 1",                            ~
               at (11,30), fac(lfac$( 6)), street1$             , ch(30),~
               at (12,02), "         Line 2",                            ~
               at (12,30), fac(lfac$( 6)), street2$             , ch(30),~
               at (13,02), "           City",                            ~
               at (13,30), fac(lfac$( 6)), city$                , ch(20),~
               at (14,02), "         County",                            ~
               at (14,30), fac(lfac$( 6)), county$              , ch(20),~
               at (15,02), "     State Code",                            ~
               at (15,30), fac(lfac$( 6)), state$               , ch(02),~
               at (16,02), "       Zip Code",                            ~
               at (16,30), fac(lfac$(06)), zip$                 , ch(09),~
               at (17,02), "Telephone Number",                           ~
               at (17,30), fac(lfac$(12)), telephone$           , ch(10),~
               at (18,02), "Gender",                                     ~
               at (18,30), fac(lfac$(13)), gender$              , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,32), "(8)Manage Applicant Skills",                 ~
               at (23,02), "(5)Next Page",                               ~
               at (24,31), "(12)DELETE This Applicant",                  ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)SAVE DATA",                              ~
                                                                         ~
               keys(hex(000105080c0d0f10)),                              ~
               key (keyhit%)

               if keyhit% <> 8% then L43040
              car$ = str(fname$,1,len(fname$)) & " " & str(mname$,1,1) & ~
                      " " & str(lname$,1,len(lname$))
                  call "SKILLSUB" (#1,#2,#3,#4,#5,#6,#8, 1%, ssnum$, car$)
                  goto L42340

L43040:        if keyhit% <> 13 then L43080
                  call "MANUAL" ("APLINPUT")
                  goto L42340

L43080:        if keyhit% <> 15 then L43120
                  call "PRNTSCRN"
                  goto L42340

L43120:        if keyhit% <> 12% then L43210
                  gosub really_delete : if delkey% <> 28% then L42340
                  call "READ101" (#5, ssnum$, f1%(5))
                  if f1%(5) <> 1% then goto L42340
                  delete #5
                  filekey$ = str(ssnum$,1,11) & " "
                  call "DELETE" (#1, filekey$, 11%)
                  goto inputmode

L43210:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return


        really_delete
L43270:     delkey% = 2%
            call "ASKUSER" (delkey%, "***** DELETE CONFIRMATION *****",  ~
                 "Press PF-28 to DELETE this Applicant", "- or -",       ~
                 "Press PF-1  to cancel Deleteion")
            if delkey% = 28% or delkey% = 1% then return
            goto L43270


        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   2       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 2 OF DOCUMENT.                    *~
            *************************************************************

            deffn'112(fieldnr%)
            if fieldnr% = 0% then inpmessage$ = edtmessage$
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L44280,         /* REQ NUM          */~
                                    L44280,         /* FOR JOB TITLE    */~
                                    L44280,         /* EEO CLASS        */~
                                    L44280,         /* LAST INTR BY     */~
                                    L44280,         /* LAST INTR ON     */~
                                    L44310,         /* EXP RATE OF PAY  */~
                                    L44280,         /* EXPERIENCE CODE  */~
                                    L44280,         /* PHYSICAL STATUS  */~
                                    L44280,         /* MILITRY STATUS   */~
                                    L44280,         /* USER SET STATUS  */~
                                    L44280,         /* USER SET STATUS  */~
                                    L44280,         /* REFERRED BY      */~
                                    L44280,         /* DATE JOB OFFERED */~
                                    L44280,         /* DATE ACCEPTED    */~
                                    L44280          /* EMP CODE ASSIGNED*/
                     goto L44350

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L44280:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L44310:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L44350:     accept                                                       ~
               at (01,02), "Edit Applicant Information, Page 2",         ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$,                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02), "For Requisition Number",                     ~
               at (06,30), fac(lfac$( 1)), req$                 , ch(05),~
               at (06,50), "Appl Name",                                  ~
               at (06,65), fac(hex(8c  )), lname$               , ch(15),~
               at (07,02), "For Job Title",                              ~
               at (07,30), fac(lfac$( 2)), jobtitle$            , ch(16),~
               at (07,50), "Appl Status",                                ~
               at (07,65), fac(hex(8c  )), status$(1)           , ch(08),~
               at (08,02), "For Job Class - EEO",                        ~
               at (08,30), fac(lfac$( 3)), eeotitle$            , ch(02),~
               at (09,02), "Last Interviewed By",                        ~
               at (09,30), fac(lfac$( 4)), lintrby$             , ch(16),~
               at (10,02), "Last Interviewed On",                        ~
               at (10,30), fac(lfac$( 5)), lintron$             , ch(08),~
               at (11,02), "Expected Rate Of Pay",                       ~
               at (11,30), fac(lfac$( 6)), exppay$              , ch(06),~
               at (12,02), "Experience Code",                            ~
               at (12,30), fac(lfac$( 7)), expcode$             , ch(01),~
               at (13,02), "Physical Status    ",                        ~
               at (13,30), fac(lfac$( 8)), status$(2)           , ch(08),~
               at (14,02), "Military Status    ",                        ~
               at (14,30), fac(lfac$( 9)), status$(3)           , ch(08),~
               at (15,02), "User Set Status    ",                        ~
               at (15,30), fac(lfac$(10)), status$(4)           , ch(08),~
               at (16,02), "User Set Status     ",                       ~
               at (16,30), fac(lfac$(11)), status$(5)           , ch(08),~
               at (17,02), "Referred By    ",                            ~
               at (17,30), fac(lfac$(12)), status$(6)           , ch(08),~
               at (18,02), "Date Job Offered",                           ~
               at (18,30), fac(lfac$(13)), dateoffered$         , ch(08),~
               at (19,02), "Date Applicant Accepted Job",                ~
               at (19,30), fac(lfac$(14)), dateaccepted$        , ch(08),~
               at (20,02), "Employee Code Assigned",                     ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,32), "(8)Manage Applicant Skills",                 ~
               at (23,02), "(4)Previous Page",                           ~
               at (24,31), "(12)DELETE This Applicant",                  ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)SAVE DATA",                              ~
                                                                         ~
               keys(hex(000104080c0d0f10)),                              ~
               key (keyhit%)

               if keyhit% <> 8% then L45130
              car$ = str(fname$,1,len(fname$)) & " " & str(mname$,1,1) & ~
                      " " & str(lname$,1,len(lname$))
                call "SKILLSUB" (#1,#2,#3,#4,#5,#6,#8,  1%, ssnum$, car$)
                  goto L44350
L45130:        if keyhit% <> 13 then L45170
                  call "MANUAL" ("APLINPUT")
                  goto L44350

L45170:        if keyhit% <> 15 then L45220
                  call "PRNTSCRN"
                  goto L44350


L45220:        if keyhit% <> 12% then L45310
                  gosub really_delete : if delkey% <> 28% then L44350
                  call "READ101" (#5, ssnum$, f1%(5))
                  if f1%(5) <> 1% then goto L44350
                  delete #5
                  filekey$ = str(ssnum$,1,11) & " "
                  call "DELETE" (#1, filekey$, 11%)
                  goto inputmode

L45310:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50230,         /* SS NUMBER        */~
                                    L50330,         /* APL STATUS       */~
                                    L50380,         /* LAST NAME        */~
                                    L50430,         /* FIRST NAME       */~
                                    L50450,         /* MIDDLE INITIAL   */~
                                    L50470,         /* STREET ADDRESS   */~
                                    L50510,         /* STREET ADDRESS   */~
                                    L50530,         /* CITY             */~
                                    L50550,         /* COUNTY           */~
                                    L50570,         /* STATE            */~
                                    L50590,         /* ZIP              */~
                                    L50610,         /* TELEPHONE        */~
                                    L50660          /* GENDER           */
                     return

L50230: REM TEST DATA FOR SOCIAL SECURITY NUMBER
            if ssnum$ <> " " then goto L50270
                errormsg$ = "Social Security Number Cannot Be Blank"
                return
L50270:     call "READ100" (#5, ssnum$, f1%(5))
            if f1%(5) = 0% then return
            gosub L36100   /* Load Up Data */
            return clear
            goto edtpg1

L50330: REM TEST DATA FOR APPLICANT STATUS
            if str(status$(1),1,1) = "A" then status$(1) = "ACTIVE"
            if str(status$(1),1,1) <> "A" then status$(1) = "INACTIVE"
            return

L50380: REM TEST DATA FOR LAST NAME
            if lname$ <> " " then return
            errormsg$ = "Last Name Cannot Be Blank"
            return

L50430: REM TEST DATA FOR FIRST NAME
            return

L50450: REM TEST DATA FOR MIDDLE INITIAL
            return

L50470: REM TEST DATA FOR STREET ADDRESS
            if street1$ <> " " then return
            errormsg$ = "First Line Of Address Cannot Be Blank"
            return

L50510: REM TEST DATA FOR STREET ADDRESS
            return

L50530: REM TEST DATA FOR CITY
            return

L50550: REM TEST DATA FOR COUNTY
            return

L50570: REM TEST DATA FOR STATE
            return

L50590: REM TEST DATA FOR ZIP
            return

L50610: REM TEST DATA FOR TELEPHONE
            return

L50660: REM TEST DATA FOR GENDER
            if gender$ = "M" or gender$ = "F" or gender$ = "?" then return
            errormsg$ = "Gender Must Be 'M' Or 'F' Or '?' "
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2.                       *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51130,         /* REQ NUM          */~
                                    L51150,         /* FOR JOB TITLE    */~
                                    L51270,         /* EEO CLASS        */~
                                    L51380,         /* LAST INTR BY     */~
                                    L51490,         /* LAST INTR ON     */~
                                    L51520,         /* EXP RATE OF PAY  */~
                                    L51580,         /* EXPERIENCE CODE  */~
                                    L51650,         /* PHYSICAL STATUS  */~
                                    L51670,         /* MILITARY STATUS  */~
                                    L51690,         /* USER SET STATUS  */~
                                    L51710,         /* USER SET STATUS  */~
                                    L51730,         /* REFERRED BY      */~
                                    L51760,         /* DATE JOB OFFERED */~
                                    L51800,         /* DATE ACCEPTED    */~
                                    L51840          /* EMP CODE ASSIGNED*/
                     return

        REM Some of These Tests are on the Type(Class) of a Term         ~
            The Allowable Classes are Set in the Subroutine 'COMSUB'     ~
            Which Now has as Allowable Terms the Following -             ~
                        "LANGUAGE        "                               ~
                        "JOB TITLE       "   The Allowable Types Are     ~
                        "EEO JOB CLASS   "   Hard Coded In 'COMSUB' To   ~
                        "INTERVIEWER     "   Eliminate An Unnecessary    ~
                        "SKILL           "   File Read.  If You Need     ~
                        "TALENT          "   To Change The List, Be Sure ~
                        "DEPARTMENT      "   To Relink All Programs      ~
                        "DIVISION        "   Which Call 'COMSUB'.        ~
                        "MANUAL SKILLS   "                               ~
                        "INSURANCE       "                               ~
                        "INSURANCE POLICY"                               ~
                        "INSURANCE PROG  "                               ~
                        "INSURANCE PROGS "

L51130: REM TEST DATA FOR FOR REQUISITION NUMBER
            return

L51150: REM TEST DATA FOR FOR JOB TITLE
            init(" ") pass$
            str(pass$,47,16) = str(jobtitle$,1,16)
L51180:     call "COMSUB" (#4, 1%, pass$, 1%, rc%)
            if rc% =  0% then goto L51180
            if str(pass$,1,16) = "JOB TITLE       " then goto L51240
            errormsg$ = "That Term Is Not A JOB TITLE, Please Reenter"
            return

L51240:     str(jobtitle$,1,16) = str(pass$,47,16)
            return

L51270: REM TEST DATA FOR FOR JOB CLASS - EEO
             init(" ") pass$
             str(pass$,47,16) = str(eeotitle$,1,16)
L51300:      call "COMSUB" (#4,  1%, pass$, 1%, rc%)
             if rc% =  0% then goto L51300
             if str(pass$,1,16) = "EEO JOB CLASS   " then goto L51360
             errormsg$ = "That Term Is Not An EEO JOB CLASS, " &         ~
                         "Please Reenter"
             return

L51360:      str(eeotitle$,1,16) = str(pass$,47,16)
             return

L51380: REM TEST DATA FOR LAST INTERVIEWED BY
            init(" ") pass$
            str(pass$,47,16) = str(lintrby$,1,16)
L51410:     call "COMSUB" (#4, 1%, pass$, 1%, rc%)
            if rc% =  0% then goto L51410
            if str(pass$,1,16) = "INTERVIEWER     " then goto L51470
            errormsg$ = "That Term Is Not An INTERVIEWER, Please Reenter"
            return

L51470:     str(lintrby$,1,16) = str(pass$,47,16)
            return

L51490: REM TEST DATA FOR LAST INTERVIEWED ON
            call "DATEOK" (lintron$, err%, errormsg$)
            return

L51520: REM TEST DATA FOR EXPECTED RATE OF PAY
            call "NUMTEST" (exppay$, 0, 9e7, errormsg$, -2.2, er)
            er = er
            return

L51580: REM TEST DATA FOR EXPERIENCE CODE
            if expcode$ = "1" or expcode$ = "2" or                       ~
               expcode$ = "3" or expcode$ = "4" then return
            errormsg$ = "EXPERIENCE CODE Must be '1', '2', '3' or '4'"
            return

L51650: REM TEST DATA FOR PHYSICAL STATUS
            return

L51670: REM TEST DATA FOR MILITARY STATUS
            return

L51690: REM TEST DATA FOR USER SET STATUS
            return

L51710: REM TEST DATA FOR USER SET STATUS
            return

L51730: REM TEST DATA FOR REFERRED BY
            return

L51760: REM TEST DATA FOR DATE JOB OFFERED
            if dateoffered$ = " " or dateoffered$ = blankdate$ then return
            call "DATEOK" (dateoffered$, err%, errormsg$)
            return

L51800: REM TEST DATA FOR DATE APPLICANT ACCEPTED JOB
            if dateaccepted$ = " " or dateaccepted$ = blankdate$ then return
            call "DATEOK" (dateaccepted$, err%, errormsg$)
            return

L51840:  REM TEST DATA FOR EMPLOYEE CODE ASSIGNED
            call "READ100" (#8, employee$, f1%(8))
            if f1%(8) <> 1% then return
            get #8, using L51880, lname$, fname$, mname$
L51880:     FMT XX(1), CH(15), CH(10), CH(1)
            errormsg$    = "This Employee Code Is Assigned To " &        ~
                            str(lname$,1,len(lname$)) & ", "  &          ~
                            str(fname$,1,len(fname$)) & " "   &          ~
                            str(mname$,1,len(mname$))
            return


L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            call "SHOSTAT" ("One Moment Please")
            end
