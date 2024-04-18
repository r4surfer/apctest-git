        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   EEEEE  RRRR   IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  P   P  E      R   R    I    NN  N  P   P  U   U    T     *~
            *  PPPP   EEEE   RRRR     I    N N N  PPPP   U   U    T     *~
            *  P      E      R   R    I    N  NN  P      U   U    T     *~
            *  P      EEEEE  R   R  IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PERINPUT - STANDARD XXXINPUT PROGRAM EXCEPT-              *~
            *            -THIS PROGRAM ONLY WRITES TO 'PERMASTR'        *~
            *            -SUBROUTINES WRITE TO ALL OTHER FILES          *~
            *            -DELETE OPTION, DELETES DATA FROM ALL FILES    *~
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
            * 11/27/83 ! ORIGINAL                                 ! GLW *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 03/03/93 ! Virtually re-written bringing code up to ! MLJ *~
            *          !   standards.                             !     *~
            *          ! PRR 10025 - STARTOVER now available in   !     *~
            *          !   Edit mode.                             !     *~
            *          ! PRR 10026 - CURJOBEEO$ now displayed for !     *~
            *          !   16 instead of 3 (matches comterms).    !     *~
            *          ! PRR 10233 - No longer automatically goes !     *~
            *          !   into Skills upon completion of screen  !     *~
            *          !   2 input.  Use PF(8) to get there.      !     *~
            *          ! PRR 11194 - Added variable fields to     !     *~
            *          !   PERMASTR at pos 460.                   !     *~
            *          ! PRR 11279 - Delete confirmation spelling !     *~
            *          !   errors resolved by new ASKUSER.        !     *~
            *          ! PRR 12063 - Formatted SSN's no longer a  !     *~
            *          !   requirement.  Just consistemcy.        !     *~
            *          ! PRR 12065 - Dependants spelled correctly.!     *~
            *          ! PRR 12255 - Request for additional info  !     *~
            *          !   can be resolved using variable fields  !     *~
            *          !   now in PERMASTR.                       !     *~
            * 08/16/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            birthdate$10,                /* BIRTHDATE                  */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            car$100,                     /* FOR SKILLSUB               */~
            bonding$16,                  /* BONDING STATUS             */~
            citizenship$16,              /* CITIZENSHIP STATUS         */~
            city$20,                     /* CITY, COUNTY, STATE & ZIP  */~
            county$20,                   /* CITY, COUNTY, STATE & ZIP  */~
            state$2,                     /* CITY, COUNTY, STATE & ZIP  */~
            zip$9  ,                     /* CITY, COUNTY, STATE & ZIP  */~
            curjob$16,                   /* CURRENT JOB TITLE          */~
            curdept$4,                   /* CUR DEPT, SHIFT, SUPERVISOR*/~
            curshift$1 ,                 /* CUR DEPT, SHIFT, SUPERVISOR*/~
            cursupr$16,                  /* CUR DEPT, SHIFT, SUPERVISOR*/~
            curjobeeo$16,                /* EEO CLASS OF CURRENT JOB   */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            deletekey$100,               /* GENERAL PURPOSE DELETE KEY */~
            dependants$2,                /* NUMBER OF DEPENDANTS       */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            eeoc$3,                      /* EEO CODE                   */~
            empephon$10,                 /* TELEPHONE - RELATIONSHIP   */~
            empereln$16,                 /* EMERGENCY RELATIONSHIP     */~
            employee$12,                 /* EMPLOYEE CODE              */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            gender$1,                    /* GENDER                     */~
            header$79,                   /* Screen Title               */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            lasttermdate$10,             /* LAST TERMINATION DATE      */~
            lasttermjob$16,              /* LAST TERMINATION JOB TITLE */~
            lasttermreason$30,           /* LAST TERMINATION REASON    */~
            line2$79,                    /* SCREEN LINE 2              */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            lname$15,                    /* NAME (LAST, FIRST, MIDDLE) */~
            fname$10,                    /* NAME (LAST, FIRST, MIDDLE) */~
            mname$1 ,                    /* NAME (LAST, FIRST, MIDDLE) */~
            marital$1,                   /* MARITAL STATUS             */~
            military$16,                 /* MILITARY STATUS            */~
            notify$30,                   /* NOTIFY IN CASE OF EMERGENCY*/~
            origdate$10,                 /* ORIGINAL HIRE DATE         */~
            passport$16,                 /* PASSPORT STATUS            */~
            pass$162,                    /* FOR COMSUB                 */~
            pf$(3)79,                    /* PF KEY SCREEN LITERALS     */~
            pfkeys$32,                   /* PF KEY HEX VALUES          */~
            physical$16,                 /* PHYSICAL STATUS            */~
            rehiredate$10,               /* REHIRE DATE                */~
            sendate$10,                  /* SENIORITY DATE             */~
            skillsubkey$20,              /* FOR SKILLSUB               */~
            ssnumber$11,                 /* SOCIAL SECURITY NUMBER     */~
            status$1,                    /* CURRENT STATUS             */~
            street1$30,                  /* STREET ADDRESS             */~
            street2$30,                  /* STREET ADDRESS             */~
            telephone$10,                /* CURRENT TELEPHONE NUMBER   */~
            union$16,                    /* UNION STATUS               */~
            vf$200                       /* PERMASTR VARIABLE FIELDS   */~

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

            call "OPENCHCK" (#01, 0%, f2%(01%), 100%, rslt$(01%))
            call "OPENCHCK" (#02, 0%, f2%(02%), 100%, rslt$(02%))
            call "OPENCHCK" (#03, 0%, f2%(03%), 100%, rslt$(03%))
            call "OPENCHCK" (#04, 0%, f2%(04%), 100%, rslt$(04%))
            call "OPENCHCK" (#05, 0%, f2%(05%), 100%, rslt$(05%))
            call "OPENCHCK" (#06, 0%, f2%(06%), 100%, rslt$(06%))
            call "OPENCHCK" (#07, 0%, f2%(07%), 100%, rslt$(07%))
            call "OPENCHCK" (#08, 0%, f2%(08%), 100%, rslt$(08%))
            call "OPENCHCK" (#09, 0%, f2%(09%), 100%, rslt$(09%))
            call "OPENCHCK" (#10, 0%, f2%(10%), 100%, rslt$(10%))
            call "OPENCHCK" (#11, 0%, f2%(11%), 100%, rslt$(11%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor "&~
                          "To Desired Value And Press (ENTER)."

            str(line2$,62) = "PERINPUT: " & str(cms2v$,1%,8%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 15%
L10100:         gosub'051(fieldnr%)
                      if enabled% = 0% then L10170
L10120:         gosub'101(fieldnr%,1%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  4% then goto L10200
                      if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit% <>  0% then       L10120
L10170:         gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10120
                if keyhit% <> 4% then goto L10230
L10200:               if fieldnr% = 1% then goto L10230
                      fieldnr% = max(1%, fieldnr% - 1%)
                      goto L10100
L10230:     next fieldnr%

            for fieldnr% = 1% to 15%
L10260:         gosub'052(fieldnr%)
                      if enabled% = 0% then L10320
L10280:         gosub'102(fieldnr%, edit%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  4% then goto L10350
                      if keyhit% <>  0% then       L10280
L10320:         gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10280
                      if keyhit% <> 4% then goto L10380
L10350:               if fieldnr% = 1% then goto L10380
                      fieldnr% = max(1%, fieldnr% - 1%)
                      goto L10260
L10380:     next fieldnr%

        REM Variable Fields input...
            call "VFINPSUB" ("PERMASTR", "I", "Manage Personnel",        ~
                             str(header$,,60%), "NN", vf$, keyhit%)

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

        edtpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  5% then edtpg2
                  if keyhit%  = 12% then gosub deletemode
                  if keyhit%  = 16% then datasave
                  if keyhit% <>  0% then edtpg1
L11140:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 15% then edtpg1
            if fieldnr% = lastfieldnr% then edtpg1
            gosub'051(fieldnr%)
                  if enabled% =  0% then edtpg1
L11190:     gosub'101(fieldnr%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  5% then edtpg2
                  if keyhit%  = 12% then gosub deletemode
                  if keyhit% <>  0% then L11190
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L11190
                  lastfieldnr% = fieldnr%
            goto L11140

        edtpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% then edtpg1
                  if keyhit%  =  5% then edtpg3
                  if keyhit%  = 12% then gosub deletemode
                  if keyhit%  = 16% then datasave
                  if keyhit% <>  0% then edtpg2
L11380:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 15% then edtpg2
            if fieldnr% = lastfieldnr% then edtpg2
            gosub'052(fieldnr%)
                  if enabled% =  0% then edtpg2
L11430:     gosub'102(fieldnr%,2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  4% then edtpg1
                  if keyhit%  =  5% then edtpg3
                  if keyhit%  = 12% then gosub deletemode
                  if keyhit% <>  0% then L11430
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L11430
                  lastfieldnr% = fieldnr%
            goto L11380

        edtpg3
            call "VFINPSUB" ("PERMASTR", "E", "Manage Personnel",        ~
                             str(header$,,60), "YY", vf$, keyhit%)
            if keyhit% =  1% then inputmode
            if keyhit% =  4% then edtpg2
            if keyhit% =  5% then edtpg1
            if keyhit% = 16% then datasave
            goto edtpg1

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
                  enabled% = 1%
                  on fieldnr% gosub L20260,         /* EMPLOYEE CODE    */~
                                    L20320,         /* CURRENT STATUS   */~
                                    L20370,         /* NAMES            */~
                                    L20410,         /* SS NUMBER        */~
                                    L20460,         /* STREET ADDRESS   */~
                                    L20500,         /* CTY-CNTY-STATE-ZP*/~
                                    L20540,         /* TELEPHONE        */~
                                    L20580,         /* GENDER           */~
                                    L20620,         /* BIRTHDATE        */~
                                    L20660,         /* EEO CODE         */~
                                    L20700,         /* MARITAL STATUS   */~
                                    L20740,         /* DEPENDANTS       */~
                                    L20780,         /* BLANK LINE       */~
                                    L20820,         /* NOTIFY           */~
                                    L20870          /* TELE-RELATIONSHIP*/
                     return

L20260: REM DEFAULT/ENABLE FOR EMPLOYEE CODE...
            if edit% <> 2% then inpmessage$ = "Enter Employee Code Or L"&~
                                "eave Blank To Search."                  ~
                           else enabled% = 0%
            return

L20320: REM DEFAULT/ENABLE FOR CURRENT STATUS...
            if status$ = " " then status$ = "C"
            inpmessage$ = "Enter Status Code 'C', 'P', 'L', 'M' OR 'N'."
            return

L20370: REM DEFAULT/ENABLE FOR NAME (LAST, FIRST, MIDDLE)...
            inpmessage$ = "Enter Last Name, First Name & Middle Initial."
            return

L20410: REM DEFAULT/ENABLE FOR SOCIAL SECURITY NUMBER...
            inpmessage$ = "Enter SSN With Or Without Dashes, Just Be Co"&~
                          "nsistent."
            return

L20460: REM DEFAULT/ENABLE FOR STREET ADDRESS...
            inpmessage$ = "Enter Street Address."
            return

L20500: REM DEFAULT/ENABLE FOR CITY, COUNTY, STATE & ZIP...
            inpmessage$ = "Enter City, County, State & Zip Code."
            return

L20540: REM DEFAULT/ENABLE FOR CURRENT TELEPHONE NUMBER...
            inpmessage$ = "Enter Area Code & Telephone Number."
            return

L20580: REM DEFAULT/ENABLE FOR GENDER...
            inpmessage$ = "Enter Gender 'M'ale Or 'F'emale."
            return

L20620: REM DEFAULT/ENABLE FOR BIRTHDATE...
            inpmessage$ = "Enter Date Of Birth."
            return

L20660: REM DEFAULT/ENABLE FOR EEO CODE....
            inpmessage$ = "Enter EEC Code."
            return

L20700: REM DEFAULT/ENABLE FOR MARITAL STATUS...
            inpmessage$ = "Enter Marital Status 'M'arried Or 'S'ingle."
            return

L20740: REM DEFAULT/ENABLE FOR NUMBER OF DEPENDANTS...
            inpmessage$ = "Enter The Number Of Dependants."
            return

L20780: REM DEFAULT/ENABLE FOR BLANK LINE...
            enabled% = 0%
            return

L20820: REM DEFAULT/ENABLE FOR NOTIFY IN CASE OF EMERGENCY...
            inpmessage$ = "Enter Name Of Person To Be Notified In Case "&~
                          "Of An Emergency."
            return

L20870: REM DEFAULT/ENABLE FOR TELEPHONE, RELATIONSHIP...
            inpmessage$ = "Enter Emergency Telephone Number & Relations"&~
                          "hip Of Person To Be Notified."
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 2 OF INPUT. *~
            *************************************************************

            deffn'052(fieldnr%)
                  init(" ") inpmessage$
                  enabled% = 1%
                  on fieldnr% gosub L22260,         /* PHYSICAL STATUS  */~
                                    L22310,         /* MILITARY STATUS  */~
                                    L22350,         /* CITIZENSHIP      */~
                                    L22390,         /* PASSPORT STATUS  */~
                                    L22430,         /* UNION STATUS     */~
                                    L22470,         /* BONDING STATUS   */~
                                    L22510,         /* CURRENT JOB TITLE*/~
                                    L22550,         /* CUR JOB EEOC     */~
                                    L22590,         /* CUR D-S-S        */~
                                    L22640,         /* ORIG HIRE DATE   */~
                                    L22690,         /* SENIORITY DATE   */~
                                    L22740,         /* REHIRE DATE      */~
                                    L22790,         /* LAST TERM DATE   */~
                                    L22830,         /* LAST TERM JOB    */~
                                    L22880          /* LAST TERM REASON */
                     return

L22260: REM DEFAULT/ENABLE FOR PHYSICAL STATUS...
            if physical$ = " " then physical$ = "NO DISABILITY"
            inpmessage$ = "If Disabled, Enter Description Of Disability."
            return

L22310: REM DEFAULT/ENABLE FOR MILITARY STATUS...
            inpmessage$ = "Enter Military Status."
            return

L22350: REM DEFAULT/ENABLE FOR CITIZENSHIP STATUS...
            inpmessage$ = "Enter Citizenship Status."
            return

L22390: REM DEFAULT/ENABLE FOR PASSPORT STATUS...
            inpmessage$ = "Enter Passport Status."
            return

L22430: REM DEFAULT/ENABLE FOR UNION STATUS...
            inpmessage$ = "Enter Union Status."
            return

L22470: REM DEFAULT/ENABLE FOR BONDING STATUS...
            inpmessage$ = "Enter Bonding Status."
            return

L22510: REM DEFAULT/ENABLE FOR CURRENT JOB TITLE...
            inpmessage$ = "Enter Current Job Title."
            return

L22550: REM DEFAULT/ENABLE FOR EEO CLASS OF CURRENT JOB...
            inpmessage$ = "Enter EEO Class Of Current Job."
            return

L22590: REM DEFAULT/ENABLE FOR CUR DEPT, SHIFT, SUPERVISOR...
            inpmessage$ = "Enter Current Department, Shift And Supervis"&~
                          "or."
            return

L22640: REM DEFAULT/ENABLE FOR ORIGINAL HIRE DATE...
            if origdate$ = " " or origdate$ = blankdate$ then origdate$ = date$
            inpmessage$ = "Enter Original Hire Date."
            return

L22690: REM DEFAULT/ENABLE FOR SENIORITY DATE...
            if sendate$ = " " or sendate$ = blankdate$ then sendate$ = origdate$
            inpmessage$ = "Enter Seniority Date."
            return

L22740: REM DEFAULT/ENABLE FOR REHIRE DATE...
            if status$ <> "C" then enabled% = 0%
            inpmessage$ = "Enter Date Of Rehire."
            return

L22790: REM DEFAULT/ENABLE FOR LAST TERMINATION DATE...
            inpmessage$ = "Enter Date Of Last Termination."
            return

L22830: REM DEFAULT/ENABLE FOR LAST TERMINATION JOB TITLE...
            if lasttermdate$ = " " or ~
               lasttermdate$ = blankdate$ then enabled% = 0%
            inpmessage$ = "Enter Job Title At Last Termination."
            return

L22880: REM DEFAULT/ENABLE FOR LAST TERMINATION REASON...
            if lasttermdate$ = " " or ~
               lasttermdate$ = blankdate$ then enabled% = 0%
            inpmessage$ = "Enter Reason For Last Termination."
            return

        REM *************************************************************~
            *        I N I T I A L I Z E    V A R I A B L E S           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

        initialize_variables
            init(" ")  birthdate$, bonding$, car$, citizenship$, city$,  ~
                county$, state$, zip$, curdept$, curshift$, cursupr$,    ~
                curjob$, curjobeeo$, dependants$, edtmessage$, eeoc$,    ~
                empephon$, empereln$, employee$, errormsg$, gender$,     ~
                inpmessage$, lasttermdate$, lasttermjob$,                ~
                lasttermreason$, lname$, fname$, mname$, marital$,       ~
                military$, notify$, origdate$, passport$,                ~
                physical$, rehiredate$, sendate$, skillsubkey$,          ~
                ssnumber$, status$, street1$, street2$, telephone$,      ~
                union$, vf$

            edit% = 1%

            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

L30000: REM *************************************************************~
            *        P U T   &   W R I T E   P E R M A S T R            *~
            *                                                           *~
            *************************************************************

            call "READ101" (#8, employee$, f1%(8))
            if f1%(8) = 1% then delete #8

            call "DATUFMTC" (birthdate$)
            call "DATUFMTC" (rehiredate$)
            call "DATUFMTC" (sendate$)
            call "DATUFMTC" (lasttermdate$)
            call "DATUFMTC" (origdate$)

            put #8, using L35070, status$, lname$, fname$, mname$,        ~
                    ssnumber$, employee$, telephone$, street1$, street2$,~
                    city$, county$, state$, zip$, notify$, empephon$,    ~
                    empereln$, gender$, birthdate$, eeoc$,               ~
                    marital$, dependants$, physical$, military$,         ~
                    citizenship$, passport$, union$, bonding$,           ~
                    curjob$, curjobeeo$, curdept$, curshift$, cursupr$,  ~
                    origdate$, sendate$, rehiredate$, lasttermdate$,     ~
                    lasttermreason$, lasttermjob$, vf$, " "

            write #8
            return

L34000: REM *************************************************************~
            *        G E T   F R O M         P E R M A S T R            *~
            *                                                           *~
            *************************************************************

            get #8, using L35070, status$, lname$, fname$, mname$,        ~
                    ssnumber$, employee$, telephone$, street1$, street2$,~
                    city$, county$, state$, zip$, notify$, empephon$,    ~
                    empereln$, gender$, birthdate$, eeoc$,               ~
                    marital$, dependants$, physical$, military$,         ~
                    citizenship$, passport$, union$, bonding$,           ~
                    curjob$, curjobeeo$, curdept$, curshift$, cursupr$,  ~
                    origdate$, sendate$, rehiredate$, lasttermdate$,     ~
                    lasttermreason$, lasttermjob$, vf$

            call "DATFMTC" (birthdate$)
            call "DATFMTC" (rehiredate$)
            call "DATFMTC" (sendate$)
            call "DATFMTC" (lasttermdate$)
            call "DATFMTC" (origdate$)
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

        REM PERMASTR Format...
L35070:     FMT CH(1), CH(15), CH(10), CH(1), CH(11), CH(12), CH(10),    ~
                2*CH(30), 2*CH(20), CH(2), CH(9), CH(30), CH(10), CH(16),~
                CH(1), CH(6), CH(3), CH(1), CH(2), 8*CH(16), CH(4),      ~
                CH(1), CH(16), 4*CH(6), CH(30), CH(16), CH(200), CH(291)

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%, edit%)
                page% = 1%
                gosub set_pf
                if edit% <> 2% then L40120
                if fieldnr% = 0% then inpmessage$ = "To Modify, Positio"&~
                    "n Cursor to Desired Value and Press RETURN."
L40120:         if fieldnr% > 0% then init(hex(8c)) lfac$()              ~
                                 else init(hex(86)) lfac$()
                if fieldnr% = 0% then L40170
                    if fieldnr% = 12% then lfac$(fieldnr%) = hex(82) else~
                    lfac$(fieldnr%) = hex(81)

L40170:     accept                                                       ~
               at (01,02), "Manage Personnel Information, Page 1",       ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)),   date$                , ch(08),~
               at (02,02), fac(hex(ac)),   line2$,                       ~
               at (04,02), fac(hex(94)),   errormsg$            , ch(79),~
                                                                         ~
               at (06,02), "Employee Code",                              ~
               at (06,30), fac(lfac$(1%)), employee$            , ch(12),~
               at (07,02), "Current Status",                             ~
               at (07,30), fac(lfac$(2%)), status$              , ch(01),~
               at (08,02), "Name (Last, First, Middle)",                 ~
               at (08,30), fac(lfac$(3%)), lname$               , ch(15),~
               at (08,50), fac(lfac$(3%)), fname$               , ch(10),~
               at (08,70), fac(lfac$(3%)), mname$               , ch(01),~
               at (09,02), "Social Security Number",                     ~
               at (09,30), fac(lfac$(4%)), ssnumber$            , ch(11),~
               at (10,02), "Street Address",                             ~
               at (10,30), fac(lfac$(5%)), street1$             , ch(30),~
               at (10,61), fac(lfac$(5%)), street2$             , ch(20),~
               at (11,02), "City, County, State & Zip",                  ~
               at (11,30), fac(lfac$(6%)), city$                , ch(20),~
               at (11,51), fac(lfac$(6%)), county$              , ch(17),~
               at (11,69), fac(lfac$(6%)), state$               , ch(02),~
               at (11,72), fac(lfac$(6%)), zip$                 , ch(09),~
               at (12,02), "Current Telephone Number",                   ~
               at (12,30), fac(lfac$(7%)), str(telephone$,1%,3%), ch(03),~
               at (12,34), "-",                                          ~
               at (12,36), fac(lfac$(7%)), str(telephone$,4%,3%), ch(03),~
               at (12,40), "-",                                          ~
               at (12,42), fac(lfac$(7%)), str(telephone$,7%,4%), ch(04),~
               at (13,02), "Gender",                                     ~
               at (13,30), fac(lfac$(8%)), gender$              , ch(01),~
               at (14,02), "Birth Date",                                 ~
               at (14,30), fac(lfac$(9%)), birthdate$           , ch(10),~
               at (15,02), "EEO Code",                                   ~
               at (15,30), fac(lfac$(10%)), eeoc$               , ch(03),~
               at (16,02), "Marital Status",                             ~
               at (16,30), fac(lfac$(11%)), marital$            , ch(01),~
               at (17,02), "Number Of Dependants",                       ~
               at (17,30), fac(lfac$(12%)), dependants$         , ch(02),~
               at (19,02), "Notify In Case Of Emergency",                ~
               at (19,30), fac(lfac$(14%)), notify$             , ch(30),~
               at (20,02), "Telephone - Relationship",                   ~
               at (20,30), fac(lfac$(15%)),str(empephon$,1%,3%) , ch(03),~
               at (20,34), "-",                                          ~
               at (20,36), fac(lfac$(15%)),str(empephon$,4%,3%) , ch(03),~
               at (20,40), "-",                                          ~
               at (20,42), fac(lfac$(15%)),str(empephon$,7%,4%) , ch(04),~
               at (20,48), fac(lfac$(15%)), empereln$           , ch(16),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <>  8% then L40790
                  gosub detaildata
                  goto L40170

L40790:        if keyhit% <> 13% then L40830
                  call "MANUAL" ("PERINPUT")
                  goto L40170

L40830:        if keyhit% <> 15% then L40870
                  call "PRNTSCRN"
                  goto L40170

L40870:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf
            if edit% = 2% then L42150                     /* Input Mode  */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L42110
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L42110:     if fieldnr% > 1% then L42130
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L42130:     return

L42150:                                                  /*  Edit Mode  */
            pf$(1%) = "(1)Start Over                  (8)Employ" &       ~
                      "ee Detail              (13)Instructions"
            pf$(2%) = "(4)Prev Screen                (12)Delete" &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(5)Next Screen                          " &       ~
                      "                       (16)Save Data   "
            pfkeys$ = hex(01ffff0405ffff08ffffff0c0dff0f1000)
            if page% > 1% then L42250
                str(pf$(2%),1%,15%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L42250:     return

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   2      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'102(fieldnr%, edit%)
                page% = 2%
                gosub set_pf
                if edit% <> 2% then L44120
                if fieldnr% = 0% then inpmessage$ = "To Modify, Positio"&~
                        "n Cursor to Desired Value and Press RETURN."
L44120:         if fieldnr% > 0% then init(hex(8c)) lfac$()              ~
                                 else init(hex(86)) lfac$()
                if fieldnr% = 0% then L44170
                    lfac$(fieldnr%) = hex(81)

L44170:     accept                                                       ~
               at (01,02), "Manage Personnel Information, Page 2",       ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)),   date$                , ch(08),~
               at (02,02), fac(hex(ac)),   line2$,                       ~
               at (04,02), fac(hex(94)),   errormsg$            , ch(79),~
                                                                         ~
               at (06,02), "Physical Status",                            ~
               at (06,30), fac(lfac$(1%)), physical$            , ch(16),~
               at (07,02), "Military Status",                            ~
               at (07,30), fac(lfac$(2%)), military$            , ch(16),~
               at (08,02), "Citizenship Status",                         ~
               at (08,30), fac(lfac$(3%)), citizenship$         , ch(16),~
               at (09,02), "Passport Status",                            ~
               at (09,30), fac(lfac$(4%)), passport$            , ch(16),~
               at (10,02), "Union Status",                               ~
               at (10,30), fac(lfac$(5%)), union$               , ch(16),~
               at (11,02), "Bonding Status",                             ~
               at (11,30), fac(lfac$(6%)), bonding$             , ch(16),~
               at (12,02), "Current Job Title",                          ~
               at (12,30), fac(lfac$(7%)), curjob$              , ch(16),~
               at (13,02), "EEO Class Of Current Job",                   ~
               at (13,30), fac(lfac$(8%)), curjobeeo$           , ch(16),~
               at (14,02), "Dept, Shift, Supervisor",                    ~
               at (14,30), fac(lfac$(9%)), curdept$             , ch(04),~
               at (14,37), fac(lfac$(9%)), curshift$            , ch(01),~
               at (14,40), fac(lfac$(9%)), cursupr$             , ch(16),~
               at (15,02), "Original Hire Date",                         ~
               at (15,30), fac(lfac$(10%)), origdate$           , ch(10),~
               at (16,02), "Seniority Date",                             ~
               at (16,30), fac(lfac$(11%)), sendate$            , ch(10),~
               at (17,02), "Rehire Date",                                ~
               at (17,30), fac(lfac$(12%)), rehiredate$         , ch(10),~
               at (18,02), "Last Termination Date",                      ~
               at (18,30), fac(lfac$(13%)), lasttermdate$       , ch(10),~
               at (19,02), "Last Termination Job Title",                 ~
               at (19,30), fac(lfac$(14%)), lasttermjob$        , ch(08),~
               at (20,02), "Last Termination Reason",                    ~
               at (20,30), fac(lfac$(15%)), lasttermreason$     , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 8% then L44680
                   gosub detaildata
                   goto L44170

L44680:        if keyhit% <> 13% then L44720
                  call "MANUAL" ("PERINPUT")
                  goto L44170

L44720:        if keyhit% <> 15% then L44760
                  call "PRNTSCRN"
                  goto L44170

L44760:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50250,         /* EMPLOYEE CODE    */~
                                    L50380,         /* CURRENT STATUS   */~
                                    L50430,         /* NAMES            */~
                                    L50480,         /* SS NUMBER        */~
                                    L50530,         /* STREET ADDRESS   */~
                                    L50580,         /* CTY-CNTY-STATE-ZP*/~
                                    L50690,         /* TELEPHONE        */~
                                    L50720,         /* GENDER           */~
                                    L50770,         /* BIRTHDATE        */~
                                    L50810,         /* EEO CODE         */~
                                    L50840,         /* MARITAL STATUS   */~
                                    L50870,         /* DEPENDANTS       */~
                                    L50900,         /* BLANK LINE       */~
                                    L50930,         /* NOTIFY           */~
                                    L50960          /* TELE-RELATIONSHIP*/
                     return

L50250: REM TEST DATA FOR EMPLOYEE CODE...
            if employee$ <> " " then goto  L50320
            call "GETEMPL" (#8, employee$, " ", 0%, f1%(8%))
                if f1%(8%) <> 0% then L50320
            fieldnr% = fieldnr% - 1%
            return

L50320:     call "READ100" (#8, employee$, f1%(8%))
                if f1%(8%) <> 1% then return
            gosub L34000
            return clear all
            goto edtpg1

L50380: REM TEST DATA FOR CURRENT STATUS...
            if pos("CPLMN" = status$) <> 0 then return
                errormsg$ = "Status MUST be C, P, L, M or N"
                return

L50430: REM TEST DATA FOR NAME (LAST, FIRST, MIDDLE)...
            if lname$ <> " " then return
                errormsg$ = "Last Name CANNOT be blank"
                return

L50480: REM TEST DATA FOR SOCIAL SECURITY NUMBER...
            if ssnumber$ <> " " then return
                errormsg$ = "Social Security Number CANNOT be blank"
                return

L50530: REM TEST DATA FOR STREET ADDRESS...
            if street1$ <> " " then return
                errormsg$ = "First Street field CANNOT be blank"
                return

L50580: REM TEST DATA FOR CITY, COUNTY, STATE & ZIP...
            if city$ <> " " then L50620
                errormsg$ = "City CANNOT be blank"
                return
L50620:     if state$ <> " " then L50650
                errormsg$ = "State CANNOT be blank"
                return
L50650:     if zip$ <> " " then return
                errormsg$ = "Zip Code CANNOT be blank"
                return

L50690: REM TEST DATA FOR CURRENT TELEPHONE NUMBER...
            return

L50720: REM TEST DATA FOR GENDER
            if gender$ = "M" or gender$ = "F" then return
                errormsg$ =  "Gender MUST be M or F"
                return

L50770: REM TEST DATA FOR BIRTHDATE...
            call "DATEOKC" (birthdate$, err%, errormsg$)
            return

L50810: REM TEST DATA FOR EEO CODE...
            return

L50840: REM TEST DATA FOR MARITAL STATUS...
            if marital$ = "M" or marital$ = "S" then L50850
                errormsg$ = "Marital Status MUST be M or S"
L50850:     return

L50870: REM TEST DATA FOR NUMBER OF DEPENDANTS...
            if dependants$ <>  " " then L50876
                errormsg$ = "Number of Dependants CANNOT be blank"
L50876:     call "NUMTEST" (dependants$, 0, 99, errormsg$, 0.0, 0)
            return

L50900: REM TEST DATA FOR BLANK LINE...
            return

L50930: REM TEST DATA FOR NOTIFY IN CASE OF EMERGENCY...
            return

L50960: REM TEST DATA FOR TELEPHONE - RELATIONSHIP...
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 2.                       *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51250,         /* PHYSICAL STATUS  */~
                                    L51280,         /* MILITARY STATUS  */~
                                    L51310,         /* CITIZENSHIP      */~
                                    L51340,         /* PASSPORT STATUS  */~
                                    L51370,         /* UNION STATUS     */~
                                    L51400,         /* BONDING STATUS   */~
                                    L51430,         /* CURRENT JOB TITLE*/~
                                    L51540,         /* CUR JOB EEOC     */~
                                    L51650,         /* CUR D-S-S        */~
                                    L51680,         /* ORIG HIRE DATE   */~
                                    L51720,         /* SENIORITY DATE   */~
                                    L51760,         /* REHIRE DATE      */~
                                    L51810,         /* LAST TERM DATE   */~
                                    L51860,         /* LAST TERM JOB    */~
                                    L51890          /* LAST TERM REASON */
                     return

L51250: REM TEST DATA FOR PHYSICAL STATUS...
            return

L51280: REM TEST DATA FOR MILITARY STATUS...
            return

L51310: REM TEST DATA FOR CITIZENSHIP STATUS...
            return

L51340: REM TEST DATA FOR PASSPORT STATUS...
            return

L51370: REM TEST DATA FOR UNION STATUS...
            return

L51400: REM TEST DATA FOR BONDING STATUS...
            return

L51430: REM TEST DATA FOR CURRENT JOB TITLE...
            init(" ") pass$
            str(pass$,47%,16%) = str(curjob$,1%,16%)
L51460:     call "COMSUB" (#4, 1%, pass$, 1%, rc%)
                if rc% =  0% then goto L51460
            if str(pass$,1%,16%) = "JOB TITLE       " then goto L51510
                errormsg$ = "Entry is NOT a valid Job Title"
                return
L51510:     str(curjob$,1%,16%) = str(pass$,47%,16%)
            return

L51540: REM TEST DATA FOR EEO CLASS OF CURRENT JOB...
            init(" ") pass$
            str(pass$,47%,16%) = str(curjobeeo$,1%,16%)
L51570:     call "COMSUB" (#4,  1%, pass$, 1%, rc%)
                if rc% =  0% then goto L51570
            if str(pass$,1,16) = "EEO JOB CLASS   " then goto L51620
                errormsg$ = "Entry is NOT an EEO Job Class"
                return
L51620:     str(curjobeeo$,1%,16%) = str(pass$,47%,16%)
            return

L51650: REM TEST DATA FOR CUR DEPT, SHIFT, SUPERVISOR...
            return

L51680: REM TEST DATA FOR ORIGINAL HIRE DATE...
            call "DATEOKC" (origdate$, err%, errormsg$)
            return

L51720: REM TEST DATA FOR SENIORITY DATE...
            call "DATEOKC" (sendate$, err%, errormsg$)
            return

L51760: REM TEST DATA FOR REHIRE DATE...
            if rehiredate$ = " " or rehiredate$ = blankdate$ then return
            call "DATEOKC" (rehiredate$, err%, errormsg$)
            return

L51810: REM TEST DATA FOR LAST TERMINATION DATE...
            if lasttermdate$ = " " or lasttermdate$ = blankdate$ then return
            call "DATEOKC" (lasttermdate$, err%, errormsg$)
            return

L51860: REM TEST DATA FOR LAST TERMINATION JOB TITLE...
            return

L51890: REM TEST DATA FOR LAST TERMINATION REASON...
            return

        deletemode
L52010:    u3% = 0%
           call "ASKUSER" (u3%, "*** DELETE ***",                        ~
                "Press PF(12) To DELETE Employee: " & employee$,  "OR",  ~
                "Press PF(1) To RETURN Without Deleting.")
           if u3% = 1% then return
               if u3% <> 12% then L52010
           call "READ101" (#8, employee$, f1%(8%))
               if f1%(8%) = 1% then delete #8             /* PERMASTR  */
           deletekey$ = str(employee$,1%,12%) & " "
           call "DELETE" (#3, deletekey$, 12%)            /* EMPSKILL  */
           call "DELETE" (#9, deletekey$, 12%)            /* PERFRNGE  */
           call "DELETE" (#10, deletekey$, 12%)           /* INSMASTR  */
           call "DELETE" (#11, deletekey$, 12%)           /* HISMASTR  */
           return clear all
           goto inputmode

        detaildata
           init(" ") pass$
           pass$ = fname$
           str(pass$, len(fname$) + 2%) = mname$
           str(pass$, len(fname$) + len(mname$) + 3%) = lname$

L55060: accept                                                           ~
               at (01,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
               at (02,03),                                               ~
        "!      Please Select The Type Of Detail That You Would Like To S~
        ~ee For      !",                                                  ~
               at (03,03),                                               ~
        "!",                                                             ~
               at (03,28),                                               ~
        fac(hex(84)), str(pass$,1%,30%), ch(30),                         ~
               at (03,79),                                               ~
        "!",                                                             ~
               at (04,03),                                               ~
        "+------------+--------------------------------------------------~
        ~------------+",                                                  ~
               at (05,03),                                               ~
        "! PF - KEY   ! TYPE OF DETAIL SHOWN                             ~
        ~            !",                                                  ~
               at (06,03),                                               ~
        "+------------+--------------------------------------------------~
        ~------------+",                                                  ~
               at (07,03),                                               ~
        "!   (1)      ! Employee Skills Inventory                        ~
        ~            !",                                                  ~
               at (08,03),                                               ~
        "!   (2)      ! Fringe Benefits Granted                          ~
        ~            !",                                                  ~
               at (09,03),                                               ~
        "!   (3)      ! Insurance Programs In Force                      ~
        ~            !",                                                  ~
               at (10,03),                                               ~
        "!   (4)      ! History Of Employment With The Company           ~
        ~            !",                                                  ~
               at (11,03),                                               ~
        "!            !                                                  ~
        ~            !",                                                  ~
               at (12,03),                                               ~
        "!            !                                                  ~
        ~            !",                                                  ~
               at (13,03),                                               ~
        "+------------+--------------------------------------------------~
        ~------------+",                                                  ~
               at (14,03),                                               ~
        "(15)Print Screen                                (16)Return To Em~
        ~ployee Review" ,                                                 ~
           keys(hex(010203040f10)), key(hitkey%)

           if hitkey% <> 15% then goto  L55580
               call "PRNTSCRN"
               goto  L55060

L55580:    if hitkey% = 16% then return

           if hitkey% <> 1% then goto L55660
              car$ = str(fname$,1%,len(fname$))& " " &str(mname$,1%,1%) &~
                                        " " & str(lname$,1%,len(lname$))
              skillsubkey$ = str(employee$,1%,12%) & " "
              call "SKILLSUB" (#1,#2,#3,#4,#5,#6,#8,3%, skillsubkey$,car$)

L55660:    if hitkey% <> 2% then goto L55700
               call "FRNGESUB" (#1,#2,#3,#4,#5,#6,#7,#8,#9,employee$)
               goto L55060

L55700:    if hitkey% <> 3% then goto L55740
               call "INSURSUB" (#1,#2,#3,#4,#5,#6,#7,#8,#9,#10, employee$)
               goto L55060

L55740:    if hitkey% <> 4% then goto L55770
               call "HSTRYSUB" (#1,#2,#3,#4,#5,#6,#7,#8,#9,#10,#11,      ~
                                                               employee$)
L55770:    if hitkey% <> 16% then L55060
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

            call "SHOSTAT" ("Closing Files, One Moment Please")

            for u3% = 1% to 11%
                if f2%(u3%) = 0% then close # u3%
                next u3%
            end
