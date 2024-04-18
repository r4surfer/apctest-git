        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  RRRR   EEEEE   QQQ   IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  R   R  E      Q   Q    I    NN  N  P   P  U   U    T     *~
            *  RRRR   EEEE   Q   Q    I    N N N  PPPP   U   U    T     *~
            *  R   R  E      Q Q Q    I    N  NN  P      U   U    T     *~
            *  R   R  EEEEE   QQQ   IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * REQINPUT - GENERAL PURPOSE REQUISITION MANAGEMENT PROGRAM *~
            *            FOR THE PERSONNEL SYSTEM.  NORMAL ENTER/EDIT   *~
            *            FEATURES.  ALSO ALLOWS ENTRY INTO THE          *~
            *            'REQSKILL' FILE BY CALLING 'SKILLSUB'.  USES   *~
            *            'COMSUB' TO CHECK COMMON PERSONNEL TERMS.      *~
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
            * 11/23/83 ! ORIGINAL                                 ! GLW *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 08/28/96 ! Changes for the year 2000.               ! DXL *~
            * 09/30/97 ! Changed SHOWMSG to SHOSTAT (1 call)      ! MLJ *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            deptcode$4,                  /* FOR DEPARTMENT             */~
            deptcodedescr$32,            /* FOR DEPARTMENT             */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            filler$251,                  /* END OF RECORD FILLER       */~
            filekey$20,                  /* FOR SKILLSUB               */~
            filledby$12,                 /* FILLED BY EMPLOYEE CODE    */~
            filledbydescr$32,            /* FILLED BY EMPLOYEE CODE    */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            jobtitle$16,                 /* FOR JOB TITLE              */~
            jobtitledescr$32,            /* FOR JOB TITLE              */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            namein$30,                   /* FOR SKILLSUB               */~
            pass$162,                    /* FOR COMTERM                */~
            raternge$6,                  /* RATE RANGE IN DOLLARS/HOUR */~
            reqdate$10,                  /* REQUISITION DATE           */~
            reqddate$10,                 /* DATE TO BE FILLED BY       */~
            reqfor$16,                   /* REQUISITION REQUESTED BY   */~
            reqfordescr$32,              /* REQUISITION REQUESTED BY   */~
            reqnr$5,                     /* REQUISITION NUMBER         */~
            ssnumber$11,                 /* SOC SEC NUMBER OF EMPLOYEE */~
            ssnumberdescr$26,            /* NAME OF SS NUM FROM APPLIC */~
            status$1,                    /* CURRENT REQUISITION STATUS */~
            status$(5)8                  /* PART TIME OR FULL TIME?    */

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

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
            *       FILE SELECTION AND OPEN CALLS                       *

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

        call "SHOSTAT" ("LINKING TO DATA BASE FOR REQUISITION MANAGEMENT")

           for i% = 1% to 11%
            call "OPENFILE" (#i%, "SHARE", f2%(i%), rslt$(i%), axd$(i%))
           if f2%(i%) = 0% then goto L01280
            call "OPENFILE" (#i%, "OUTPT", f2%(i%), rslt$(i%), axd$(i%))
                     close #i%
            call "OPENFILE" (#i%, "SHARE", f2%(i%), rslt$(i%), axd$(i%))
L01280:    next i%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."


        REM *************************************************************~
            *       I N P U T   M O D E   I N I T   T O   ' '           *~
            *                                                           *~
            * SETS ALL VARIABLES BACK TO BLANK FOR IMPUTMODE            *~
            *************************************************************

        inputmode

        init(" ")                                                        ~
            deptcode$ ,                  /* FOR DEPARTMENT             */~
            deptcodedescr$  ,            /* FOR DEPARTMENT             */~
            edtmessage$  ,               /* EDIT SCREEN MESSAGE        */~
            errormsg$  ,                 /* ERROR MESSAGE              */~
            filledby$ ,                  /* FILLED BY EMPLOYEE CODE    */~
            filledbydescr$  ,            /* FILLED BY EMPLOYEE CODE    */~
            inpmessage$  ,               /* INPUT MESSAGE              */~
            jobtitle$  ,                 /* FOR JOB TITLE              */~
            jobtitledescr$  ,            /* FOR JOB TITLE              */~
            raternge$ ,                  /* RATE RANGE IN DOLLARS/HOUR */~
            reqdate$ ,                   /* REQUISITION DATE           */~
            reqddate$ ,                  /* DATE TO BE FILLED BY       */~
            reqfor$  ,                   /* REQUISITION REQUESTED BY   */~
            reqfordescr$  ,              /* REQUISITION REQUESTED BY   */~
            reqnr$ ,                     /* REQUISITION NUMBER         */~
            ssnumber$  ,                 /* SOC SEC NUMBER OF EMPLOYEE */~
            ssnumberdescr$,              /* NAME FROM APPL FILE        */~
            status$ ,                    /* CURRENT REQUISITION STATUS */~
            status$( )


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************


            for fieldnr% = 1 to 15
L02130:         gosub'051(fieldnr%)
                      if enabled% = 0 then L02250
L02150:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% = 4% then goto L02230
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L02150
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L02150
                if keyhit% <> 4% then goto L02250
L02230:              fieldnr% = max(1%, fieldnr% - 1%)
                     goto L02130
L02250:         next fieldnr%

           filekey$ = str(reqnr$,1,5) & " "
           namein$ = "REQ # " & reqnr$ & ", FOR " & jobtitle$
        call "SKILLSUB" (#1, #2, #3, #4,#5,#6,#8,                        ~
                                             2%, filekey$, namein$)


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************
        editmode

           init(" ") inpmessage$

L02410:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L02410
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% > 15 then L02410
            if fieldnr% = 11% then goto L02410
            if fieldnr% = 12% then goto L02410
            if fieldnr% = 13% then goto L02410

            gosub'051(fieldnr%)
L02520:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L02520
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L02520
            goto L02410

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *                                                           *~
            * SAVES DATA ON FILE AFTER INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub L03940
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L02930 ,         /* REQ NUMBER       */~
                                    L02950 ,         /* REQUESTED BY     */~
                                    L02990 ,         /* REQUISITION DATE */~
                                    L03040 ,         /* FILLED BY DATE   */~
                                    L03080 ,         /* STATUS           */~
                                    L03130 ,         /* FOR DEPARTMENT   */~
                                    L03170 ,         /* JOB TITLE        */~
                                    L03210 ,         /* RATE RANGE       */~
                                    L03250 ,         /* PART/FULL TIME   */~
                                    L03300 ,         /* PERM/TEMP        */~
                                    L03350 ,         /* .                */~
                                    L03380 ,         /* .                */~
                                    L03410 ,         /* .                */~
                                    L03440 ,         /* FILLED BY        */~
                                    L03480           /* SS NUMBER        */
                     return
L02930:     REM DEFAULT/ENABLE FOR REQUISITION NUMBER
                return
L02950:     REM DEFAULT/ENABLE FOR REQUISITION REQUESTED BY
           inpmessage$ = "                                               ~
        ~    WHO IS THIS REQUISITION FOR?"
                return
L02990:     REM DEFAULT/ENABLE FOR REQUISITION DATE
           inpmessage$ = "                       ENTER THE DATE ON WHICH ~
        ~THE REQUISITION BECAME EFFECTIVE"
           reqdate$ = date
           call "DATFMTC" (reqdate$)
                return
L03040:     REM DEFAULT/ENABLE FOR DATE TO BE FILLED BY
           inpmessage$ = "                       ENTER THE DATE BY WHICH ~
        ~THIS OPENING NEEDS TO BE FILLED"
                return
L03080:     REM DEFAULT/ENABLE FOR CURRENT REQUISITION STATUS
           inpmessage$ = "                       O = OPEN, C = CLOSED, F ~
        ~= FILLED, H = CURRENTLY ON HOLD"
           status$ = "O"
                return
L03130:     REM DEFAULT/ENABLE FOR FOR DEPARTMENT
           inpmessage$ = "                                     WHICH DEPA~
        ~RTMENT IS THIS REQUISITION FOR?"
                return
L03170:     REM DEFAULT/ENABLE FOR FOR JOB TITLE
           inpmessage$ = "                                               ~
        ~         WHAT IS THE JOB TITLE?"
                return
L03210:     REM DEFAULT/ENABLE FOR RATE RANGE IN DOLLARS/HOUR
           inpmessage$ = "                                     ENTER THE ~
        ~ WAGE RANGE IN DOLLARS PER HOUR"
                return
L03250:     REM DEFAULT/ENABLE FOR PART TIME OR FULL TIME?
           inpmessage$ = "                                           IS T~
        ~HIS JOB PART TIME OR FULL TIME?"
           status$(1) = "FULLTIME"
                return
L03300:     REM DEFAULT/ENABLE FOR PERMANENT OR TEMPORARY?
           inpmessage$ = "                                               ~
        ~  IS IT PERMANENT OR TEMPORARY?"
           status$(2) = "PERM'ANT"
                return
L03350:     REM DEFAULT/ENABLE FOR .
           enabled% = 0%
                return
L03380:     REM DEFAULT/ENABLE FOR .
           enabled% = 0%
                return
L03410:     REM DEFAULT/ENABLE FOR .
           enabled% = 0%
                return
L03440:     REM DEFAULT/ENABLE FOR FILLED BY EMPLOYEE CODE
           inpmessage$ = "LEAVE BLANK UNTIL FILLED, THEN ENTER EMPLOYEE C~
        ~ODE HERE, OR THE SS # BELOW"
                return
L03480:     REM DEFAULT/ENABLE FOR SOC SEC NUMBER OF EMPLOYEE
           inpmessage$ = "IF YOU DID NOT KNOW THE EMPLOYEE CODE, ENTER TH~
        ~E SOCIAL SECURITY NUMBER"
           if filledby$ <> " " then enabled% = 0%
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
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.
L03730:     accept                                                       ~
               at (01,27),                                               ~
                  "***** START OVER COMMAND*****",                       ~
               at (04,02),                                               ~
                  "PRESS (1) TO RETURN TO DISPLAY",                      ~
               at (06,02),                                               ~
              "PRESS (ENTER) TO START OVER WITHOUT SAVING CURRENT ENTRY",~
                                                                         ~
               keys(hex(00010f)),                                        ~
               on hex(00010f) goto L03850 , L03880 , L03900
               return

L03850:        REM START OVER            (ENTER)
                   return clear
                   goto inputmode
L03880:        REM RETURN TO DISPLAY.    (P.F. KEY 1)
                   return
L03900:        REM PRINT SCREEN.         (P.F. KEY 15)
                   call "PRNTSCRN"
                   goto L03730

L03940: REM *************************************************************~
            *        W R I T E   T O   F I L E                          *~
            *                                                           *~
            *************************************************************

           call "READ101" (#6, reqnr$, f1%(6))
           if f1%(6) = 1% then delete #6

           call "DATUFMTC" (reqdate$)
           call "DATUFMTC" (reqddate$)

        put #6, using L05160 ,     /* FILE: REQMASTR                     */~
            reqddate$,           /* Date required - requisitions       */~
            status$,             /* Status indicator for level 2 plann */~
            reqfor$,             /* Who is this requisition for? - per */~
            reqdate$,            /* Date of requisition - personnel sy */~
            reqnr$,              /* Requisition number - personnel sys */~
            raternge$,           /* Rate range for requisition - perso */~
            deptcode$,           /* department code                    */~
            jobtitle$,           /* job title                          */~
            status$(),           /* General purpose status             */~
            filledby$,           /* Employee code of employee who fill */~
            ssnumber$,           /* Social security number             */~
            filler$              /* filler for rest of record or inter */

           write #6
           return

L04220: get #6, using L05160 ,     /* FILE: REQMASTR                     */~
            reqddate$,           /* Date required - requisitions       */~
            status$,             /* Status indicator for level 2 plann */~
            reqfor$,             /* Who is this requisition for? - per */~
            reqdate$,            /* Date of requisition - personnel sy */~
            reqnr$,              /* Requisition number - personnel sys */~
            raternge$,           /* Rate range for requisition - perso */~
            deptcode$,           /* department code                    */~
            jobtitle$,           /* job title                          */~
            status$(),           /* General purpose status             */~
            filledby$,           /* Employee code of employee who fill */~
            ssnumber$,           /* Social security number             */~
            filler$              /* filler for rest of record or inter */

           call "READ100" (#8, filledby$, f1%(8))
                     if f1%(8) <> 1% then goto L04370
                     get #8, using L04363, lname$, fname$
L04363:              FMT XX(1), CH(15), CH(10)
                     str(filledbydescr$,1) = lname$
                     str(filledbydescr$, len(lname$) + 2%) = fname$

L04370:    call "READ100" (#5, ssnumber$, f1%(5))
           if f1%(5) <> 1% then goto L04420
           get #5, using L04400     , ssnumberdescr$
L04400:    FMT XX(43), CH(26)

L04420:    call "DATFMTC" (reqdate$)
           call "DATFMTC" (reqddate$)
           init(" ") pass$
           str(pass$,47,16) = str(jobtitle$,1,16)
           call "COMSUB" (#4, 9%, pass$, 1%, rc%)
           if rc% =  0% then goto L04443
             jobtitledescr$ = str(pass$,17,30)

L04443:    init(" ") pass$
           str(pass$,47,16) = str(deptcode$,1,04)  & " "
           call "COMSUB" (#4, 9%, pass$, 1%, rc%)
           if rc% =  0% then goto L04460
            deptcodedescr$ = str(pass$,17,30)
L04460:    return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************


L05160: FMT                      /* FILE: REQMASTR                     */~
            CH(6),               /* Date required - requisitions       */~
            CH(1),               /* Status indicator for level 2 plann */~
            CH(16),              /* Who is this requisition for? - per */~
            CH(6),               /* Date of requisition - personnel sy */~
            CH(5),               /* Requisition number - personnel sys */~
            CH(6),               /* Rate range for requisition - perso */~
            CH(4),               /* department code                    */~
            CH(16),              /* job title                          */~
            5*CH(8),             /* General purpose status             */~
            CH(12),              /* Employee code of employee who fill */~
            CH(11),              /* Social security number             */~
            CH(247)              /* filler for rest of record or inter */


        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L05980 ,         /* REQ NUMBER       */~
                                    L05980 ,         /* REQUESTED BY     */~
                                    L05980 ,         /* REQUISITION DATE */~
                                    L05980 ,         /* FILLED BY DATE   */~
                                    L05980 ,         /* STATUS           */~
                                    L05980 ,         /* FOR DEPARTMENT   */~
                                    L05980 ,         /* JOB TITLE        */~
                                    L06010 ,         /* RATE RANGE       */~
                                    L05980 ,         /* PART/FULL TIME   */~
                                    L05980 ,         /* PERM/TEMP        */~
                                    L05980 ,         /* .                */~
                                    L05980 ,         /* .                */~
                                    L05980 ,         /* .                */~
                                    L05980 ,         /* FILLED BY        */~
                                    L05980           /* SS NUMBER        */
                     goto L06050

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L05980:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L06010:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L06050:     accept                                                       ~
               at (01,02),                                               ~
                  "MANAGE PERSONNEL REQUISITIONS",                       ~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (02,75),                                               ~
                  "PAGE 1",                                              ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "REQUISITION NUMBER",                                  ~
               at (06,30), fac(lfac$( 1)), reqnr$               , ch(05),~
               at (07,02),                                               ~
                  "REQUISITION REQUESTED BY",                            ~
               at (07,30), fac(lfac$( 2)), reqfor$              , ch(16),~
               at (07,49), fac(hex(8c)),   reqfordescr$         , ch(32),~
               at (08,02),                                               ~
                  "REQUISITION DATE",                                    ~
               at (08,30), fac(lfac$( 3)), reqdate$             , ch(10),~
               at (09,02),                                               ~
                  "DATE TO BE FILLED BY",                                ~
               at (09,30), fac(lfac$( 4)), reqddate$            , ch(10),~
               at (10,02),                                               ~
                  "CURRENT REQUISITION STATUS",                          ~
               at (10,30), fac(lfac$( 5)), status$              , ch(01),~
               at (11,02),                                               ~
                  "FOR DEPARTMENT",                                      ~
               at (11,30), fac(lfac$( 6)), deptcode$            , ch(04),~
               at (11,49), fac(hex(8c)),   deptcodedescr$       , ch(32),~
               at (12,02),                                               ~
                  "FOR JOB TITLE",                                       ~
               at (12,30), fac(lfac$( 7)), jobtitle$            , ch(16),~
               at (12,49), fac(hex(8c)),   jobtitledescr$       , ch(32),~
               at (13,02),                                               ~
                  "RATE RANGE IN DOLLARS/HOUR",                          ~
               at (13,30), fac(lfac$( 8)), raternge$            , ch(06),~
               at (14,02),                                               ~
                  "PART TIME OR FULL TIME?",                             ~
               at (14,30), fac(lfac$( 9)), status$(1)           , ch(08),~
               at (15,02),                                               ~
                  "PERMANENT OR TEMPORARY?",                             ~
               at (15,30), fac(lfac$(10)), status$(2)           , ch(08),~
               at (16,02),                                               ~
                  ".",                                                   ~
               at (16,30), fac(lfac$(11)), status$(3)           , ch(01),~
               at (17,02),                                               ~
                  ".",                                                   ~
               at (17,30), fac(lfac$(12)), status$(4)           , ch(01),~
               at (18,02),                                               ~
                  ".",                                                   ~
               at (18,30), fac(lfac$(13)), status$(5)           , ch(01),~
               at (19,02),                                               ~
                  "FILLED BY EMPLOYEE CODE",                             ~
               at (19,30), fac(lfac$(14)), filledby$            , ch(12),~
               at (19,49), fac(hex(8c)),   filledbydescr$       , ch(32),~
               at (20,02),                                               ~
                  "SOC SEC NUMBER OF EMPLOYEE",                          ~
               at (20,30), fac(lfac$(15)), ssnumber$            , ch(11),~
               at (20,49), fac(hex(8c)),   ssnumberdescr$       , ch(26),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER          (4)PREVIOUS FIELD",            ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(0001040d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L06840
                  call "MANUAL" ("REQINPUT")
                  goto L06050

L06840:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L06050

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L07160 ,         /* REQ NUMBER       */~
                                    L07160 ,         /* REQUESTED BY     */~
                                    L07160 ,         /* REQUISITION DATE */~
                                    L07160 ,         /* FILLED BY DATE   */~
                                    L07160 ,         /* STATUS           */~
                                    L07160 ,         /* FOR DEPARTMENT   */~
                                    L07160 ,         /* JOB TITLE        */~
                                    L07190 ,         /* RATE RANGE       */~
                                    L07160 ,         /* PART/FULL TIME   */~
                                    L07160 ,         /* PERM/TEMP        */~
                                    L07160 ,         /* .                */~
                                    L07160 ,         /* .                */~
                                    L07160 ,         /* .                */~
                                    L07160 ,         /* FILLED BY        */~
                                    L07160           /* SS NUMBER        */
                     goto L07230

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L07160:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L07190:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L07230:     accept                                                       ~
               at (01,02),                                               ~
                  "EDIT/REVIEW PERSONNEL REQUISITIONS",                  ~
               at (02,02),                                               ~
                  "DATE:",                                               ~
               at (02,09), fac(hex(8c)), date$                  , ch(08),~
               at (02,75),                                               ~
                  "PAGE 1",                                              ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "REQUISITION NUMBER",                                  ~
               at (06,30), fac(lfac$( 1)), reqnr$               , ch(05),~
               at (07,02),                                               ~
                  "REQUISITION REQUESTED BY",                            ~
               at (07,30), fac(lfac$( 2)), reqfor$              , ch(16),~
               at (07,49), fac(hex(8c)),   reqfordescr$         , ch(32),~
               at (08,02),                                               ~
                  "REQUISITION DATE",                                    ~
               at (08,30), fac(lfac$( 3)), reqdate$             , ch(10),~
               at (09,02),                                               ~
                  "DATE TO BE FILLED BY",                                ~
               at (09,30), fac(lfac$( 4)), reqddate$            , ch(10),~
               at (10,02),                                               ~
                  "CURRENT REQUISITION STATUS",                          ~
               at (10,30), fac(lfac$( 5)), status$              , ch(01),~
               at (11,02),                                               ~
                  "FOR DEPARTMENT",                                      ~
               at (11,30), fac(lfac$( 6)), deptcode$            , ch(04),~
               at (11,49), fac(hex(8c)),   deptcodedescr$       , ch(32),~
               at (12,02),                                               ~
                  "FOR JOB TITLE",                                       ~
               at (12,30), fac(lfac$( 7)), jobtitle$            , ch(16),~
               at (12,49), fac(hex(8c)),   jobtitledescr$       , ch(32),~
               at (13,02),                                               ~
                  "RATE RANGE IN DOLLARS/HOUR",                          ~
               at (13,30), fac(lfac$( 8)), raternge$            , ch(06),~
               at (14,02),                                               ~
                  "PART TIME OR FULL TIME?",                             ~
               at (14,30), fac(lfac$( 9)), status$(1)           , ch(08),~
               at (15,02),                                               ~
                  "PERMANENT OR TEMPORARY?",                             ~
               at (15,30), fac(lfac$(10)), status$(2)           , ch(08),~
               at (16,02),                                               ~
                  ".",                                                   ~
               at (16,30), fac(lfac$(11)), status$(3)           , ch(01),~
               at (17,02),                                               ~
                  ".",                                                   ~
               at (17,30), fac(lfac$(12)), status$(4)           , ch(01),~
               at (18,02),                                               ~
                  ".",                                                   ~
               at (18,30), fac(lfac$(13)), status$(5)           , ch(01),~
               at (19,02),                                               ~
                  "FILLED BY EMPLOYEE CODE",                             ~
               at (19,30), fac(lfac$(14)), filledby$            , ch(12),~
               at (19,49), fac(hex(8c)),   filledbydescr$       , ch(32),~
               at (20,02),                                               ~
                  "SOC SEC NUMBER OF EMPLOYEE",                          ~
               at (20,30), fac(lfac$(15)), ssnumber$            , ch(11),~
               at (20,49), fac(hex(8c)),   ssnumberdescr$       , ch(26),~
                                                                         ~
               at (21,02), fac(hex(84)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (23,02),                                               ~
                  "             ",                                       ~
               at (23,45),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,68),                                               ~
                  "(16)SAVE DATA",                                       ~
               at (24,02),                                               ~
                  "(8)SEE/MANAGE SKILLS DESIRED   (12)DELETE THIS REQUISI~
        ~TION",                                                           ~
               keys(hex(00080c0d0f10)),                                  ~
               key (keyhit%)

               if keyhit% = 8% then goto  L08140
               if keyhit% = 12% then goto L08195

               if keyhit% <> 13 then L08060
                  call "MANUAL" ("REQINPUT")
                  goto L07230

L08060:        if keyhit% <> 15 then L08100
                  call "PRNTSCRN"
                  goto L07230

L08100:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

L08140:    filekey$ = str(reqnr$,1,5) & " "
           namein$ = "REQ # " & reqnr$ & ", FOR " & jobtitle$
        call "SKILLSUB" (#1, #2, #3, #4, #5, #6, #8,                     ~
                                             2%, filekey$, namein$)
           goto L07230


L08195:    gosub really_delete : if delkey% <> 12% then L07230
           call "READ101" (#6, reqnr$, f1%(6))
           if f1%(6) = 1% then delete #6
           filekey$ = str(reqnr$,1,5) & " "
           call "DELETE" (#2, filekey$, 5%)
           goto inputmode

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L08500 ,         /* REQ NUMBER       */~
                                    L08580 ,         /* REQUESTED BY     */~
                                    L08600 ,         /* REQUISITION DATE */~
                                    L08630 ,         /* FILLED BY DATE   */~
                                    L08660 ,         /* STATUS           */~
                                    L08750 ,         /* FOR DEPARTMENT   */~
                                    L08870 ,         /* JOB TITLE        */~
                                    L08990 ,         /* RATE RANGE       */~
                                    L09040 ,         /* PART/FULL TIME   */~
                                    L09080 ,         /* PERM/TEMP        */~
                                    L09120 ,         /* .                */~
                                    L09140 ,         /* .                */~
                                    L09160 ,         /* .                */~
                                    L09180 ,         /* FILLED BY        */~
                                    L09220           /* SS NUMBER        */
                     return
L08500:     REM TEST DATA FOR REQUISITION NUMBER
           call "READ100" (#6, reqnr$, f1%(6))
           if f1%(6) <> 1% then return

           gosub L04220             /* LOAD DATA FROM FILE */
           return clear all
           goto editmode

L08580:     REM TEST DATA FOR REQUISITION REQUESTED BY
                return
L08600:     REM TEST DATA FOR REQUISITION DATE
           call "DATEOKC" (reqdate$, e%, errormsg$)
                return
L08630:     REM TEST DATA FOR DATE TO BE FILLED BY
           call "DATEOKC" (reqddate$, e%, errormsg$)
                return
L08660:     REM TEST DATA FOR CURRENT REQUISITION STATUS
           if status$ = "O" then return
           if status$ = "F" then return
           if status$ = "C" then return
           if status$ = "H" then return
           errormsg$ = "REQUISITION STATUS MUST BE O = OPEN, C = CLOSED, ~
        ~F = FILLED, OR H = HOLD"
                return

L08750:     REM TEST DATA FOR FOR DEPARTMENT
           init(" ") pass$
           str(pass$,47,16) = str(deptcode$,1,04)  & " "
L08780:    call "COMSUB" (#4, 1%, pass$, 1%, rc%)
           if rc% =  0% then goto L08780
           if str(pass$,1,16) =                                          ~
                        "DEPARTMENT      " then goto L08830
        errormsg$ = "THAT TERM IS NOT A DEPARTMENT, PLEASE REENTER"
L08830:     deptcode$ = str(pass$,47,16)
            deptcodedescr$ = str(pass$,17,30)
            return

L08870:     REM TEST DATA FOR FOR JOB TITLE
           init(" ") pass$
           str(pass$,47,16) = str(jobtitle$,1,16)
L08900:    call "COMSUB" (#4, 1%, pass$, 1%, rc%)
           if rc% =  0% then goto L08900
           if str(pass$,1,16) =                                          ~
                        "JOB TITLE       " then goto L08950
        errormsg$ = "THAT TERM IS NOT A JOB TITLE, PLEASE REENTER"
L08950:      jobtitle$ = str(pass$,47,16)
             jobtitledescr$ = str(pass$,17,30)
             return

L08990:     REM TEST DATA FOR RATE RANGE IN DOLLARS/HOUR
           convert raternge$ to i%, data goto L09020
                return
L09020:    errormsg$ = "PLEASE ENTER THE RATE RANGE IN DOLLARS PER HOUR"
                return
L09040:     REM TEST DATA FOR PART TIME OR FULL TIME?
          if str(status$(1),1,1) = "P" then status$(1) = "PARTTIME" else ~
                     status$(1) = "FULLTIME"
                return
L09080:     REM TEST DATA FOR PERMANENT OR TEMPORARY?
          if str(status$(2),1,1) = "P" then status$(2) = "PERM'ANT" else ~
                     status$(2) = "TEMP'ARY"
                return
L09120:     REM TEST DATA FOR .
                return
L09140:     REM TEST DATA FOR .
                return
L09160:     REM TEST DATA FOR .
                return
L09180:     REM TEST DATA FOR FILLED BY EMPLOYEE CODE
                     filledbydescr$ = " "
           call "READ100" (#8, filledby$, f1%(8))
                     if f1%(8) <> 1% then goto L09210
                     get #8, using L09198, lname$, fname$
L09198:              FMT XX(1), CH(15), CH(10)
                     str(filledbydescr$,1) = lname$
                     str(filledbydescr$, len(lname$) + 2%) = fname$

L09210:         return

L09220:     REM TEST DATA FOR SOC SEC NUMBER OF EMPLOYEE
           call "READ100" (#5, ssnumber$, f1%(5))
           if f1%(5) <> 1% then return
           get #5, using L09260     , ssnumberdescr$
L09260:    FMT XX(43), CH(26)
                return

        really_delete

        accept                                                           ~
           at(05,10), ">>>>>>>>>>>>> DO YOU REALLY WANT TO DELETE? <<<<<<~
        ~<<<<<<<<<<",                                                     ~
           at(07,10), "(12) DELETE IMMEDIATLY                    (1) DO N~
        ~ OT DELETE",                                                     ~
           keys(hex(010c)), key(delkey%)
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

            call "SHOWMSG" ("ONE MOMENT PLEASE")

            for u3% = 1 to 64
                if f2%(u3%) = 0 then close # u3%
                next u3%
            end
