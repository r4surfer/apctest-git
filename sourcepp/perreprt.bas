        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  PPPP   EEEEE  RRRR   RRRR   EEEEE  PPPP   RRRR   TTTTT   *~
            *  P   P  E      R   R  R   R  E      P   P  R   R    T     *~
            *  PPPP   EEEE   RRRR   RRRR   EEEE   PPPP   RRRR     T     *~
            *  P      E      R   R  R   R  E      P      R   R    T     *~
            *  P      EEEEE  R   R  R   R  EEEEE  P      R   R    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PERREPRT - COMPLETE PRINTED REPORTING FROM ALL PERSONNEL  *~
            *            FILES.  HIGHLY STRUCTURED.  SIMPLE LOGIC.      *~
            *            DETERMINE FILE, KEY TO USE, STARTING POINT,    *~
            *            & ENDING POINT.  FROM THERE ON IT IS JUST      *~
            *            FORMAT STATEMENTS.  ONLY THE BASIC INFO IS     *~
            *            DISPLAYED.  USER CAN ADD MORE AS DESIRED.      *~
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
            * 12/02/83 ! ORIGINAL                                 ! GLW *~
            * 08/28/86 ! PRR# 4865 - CHANGED SO RANGES WORK RIGHT ! LKM *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            * 08/20/96 ! Changes for the year 2000.               ! DXL *~
            * 09/15/97 ! Changed SHOWMSG to SHOSTAT (25 calls)    ! MLJ *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim l$(950)1,                    /* GENERAL PURPOSE STRING     */~
            i$(24)80,   cursor%(2),                                      ~
            beg$(16)16, lst$(16)16,                                      ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            errormsg$80,                 /* Error Message for DATEOK   */~
            lfac$(16)1,                                                  ~
            plowkey$100,                                                 ~
            recdate$(3)10,               /* Temporary Date Variables   */~
            hdrvar$22,                                                   ~
            hdrvariable$37

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(64)4                    /* AXD POINTER FROM "FILEOPEN"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

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
            * #11 ! EMPHSTRY ! EMPLOYMENT HISTORY MASTER FILE           *~
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

        call "SHOSTAT" ("LINKING TO DATA BASE FOR PRINTING PERSONNEL REPO~
        ~RTS")

           for i% = 1% to 11%
            call "OPENFILE" (#i%, "SHARE", f2%(i%), rslt$(i%), axd$(i%))
           if f2%(i%) = 0% then goto L03150
            call "OPENFILE" (#i%, "OUTPT", f2%(i%), rslt$(i%), axd$(i%))
                     close #i%
            call "OPENFILE" (#i%, "SHARE", f2%(i%), rslt$(i%), axd$(i%))
L03150:    next i%

           blankdate$ = " "
           call "DATUFMTC" (blankdate$)

           date$ = date
           call "DATEFMT" (date$)

        FMT                      /* FILE: APLSKILL                     */~
            CH(16),              /* Skill for personnel system         */~
            CH(11),              /* Social security number             */~
            CH(2),               /* General purpose sequence number    */~
            CH(4),               /* Proficiency in a skill - personnel */~
            CH(42),              /* the text                           */~
            CH(25)               /* filler for rest of record or inter */~

        FMT                      /* FILE: REQSKILL                     */~
            CH(16),              /* Skill for personnel system         */~
            CH(05),              /* Requisition number - personnel sys */~
            CH(2),               /* General purpose sequence number    */~
            CH(4),               /* Proficiency in a skill - personnel */~
            CH(42),              /* the text                           */~
            CH(31)               /* filler for rest of record or inter */~

        FMT                      /* FILE: EMPSKILL                     */~
            CH(16),              /* Skill for personnel system         */~
            CH(12),              /* employee code                      */~
            CH(2),               /* General purpose sequence number    */~
            CH(4),               /* Proficiency in a skill - personnel */~
            CH(42),              /* the text                           */~
            CH(24)               /* filler for rest of record or inter */~

        FMT                      /* FILE: APLMASTR                     */~
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
            CH(6),               /* Birthdate of a person              */~
            CH(1),               /* Gender of a person                 */~
            CH(6),               /* Expected rate of pay - part of per */~
            CH(1),               /* Experience rating or code - part o */~
            CH(6),               /* Date job offered - part of personn */~
            CH(6),               /* Date job accepted - part of person */~
            6*CH(8),             /* ALL SIX STATUS FIELDS              */~
            CH(152),             /* filler for rest of record or inter */~
            CH(129)              /* filler for rest of record or inter */

        FMT                      /* FILE: REQMASTR                     */~
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

        FMT                      /* FILE: PERMASTR                     */~
            CH(1),               /* General purpose status indicator   */~
            CH(15),              /* Last name of person - part of pers */~
            CH(10),              /* First name of person               */~
            CH(1),               /* Middle name of person              */~
            CH(11),              /* Social security number             */~
            CH(12),              /* employee code                      */~
            CH(10),              /* Telephone number                   */~
            CH(30),              /* Street address line 1              */~
            CH(30),              /* Street address line 2              */~
            CH(20),              /* City in address                    */~
            CH(20),              /* County in address                  */~
            CH(2),               /* State in address                   */~
            CH(9),               /* Zip code in address                */~
            CH(30),              /* emergency contact                  */~
            CH(10),              /* emergency contact's phone number   */~
            CH(16),              /* Emergency contacts relationship to */~
            CH(1),               /* Gender of a person                 */~
            CH(6),               /* birth date                         */~
            CH(3),               /* minority (eeoc compliance) code    */~
            CH(1),               /* Marital status - personnel system  */~
            CH(2),               /* Number of dependants - personnel s */~
            CH(16),              /* A persons physical status - handyc */~
            CH(16),              /* A persons military status - vet, r */~
            CH(16),              /* Citizenship status - personnel sys */~
            CH(16),              /* Passport status - personnel system */~
            CH(16),              /* Union status - personnel system    */~
            CH(16),              /* Bonding status - personnel system  */~
            CH(16),              /* Current job title - personnel syst */~
            CH(16),              /* EEO class of current job - personn */~
            CH(4),               /* Current department                 */~
            CH(1),               /* Current shift worked               */~
            CH(16),              /* Current supervisor                 */~
            CH(6),               /* Original date hired                */~
            CH(6),               /* Seniority date                     */~
            CH(6),               /* Last date rehired                  */~
            CH(6),               /* Last date terminated               */~
            CH(30),              /* Reason for last termination        */~
            CH(16),              /* Job held when last terminated      */~
            CH(250),             /* filler for rest of record or inter */~
            CH(241)              /* filler for rest of record or inter */~

        FMT                      /* FILE: PERFRNGE                     */~
            CH(16),              /* Type of fringe benefit             */~
            CH(12),              /* employee code                      */~
            CH(3),               /* General purpose sequence number    */~
            CH(6),               /* Benefit eligibility date           */~
            CH(6),               /* Benefit start date                 */~
            CH(10),              /* Benefit, amount paid by employee   */~
            CH(10),              /* Benefit, max paid by employee      */~
            CH(30),              /* Benefit, frequency of employee pay */~
            CH(157)              /* filler for rest of record or inter */~

        FMT                      /* FILE: INSMASTR                     */~
            CH(16),              /* Type of insurance program          */~
            CH(12),              /* employee code                      */~
            CH(3),               /* General purpose sequence number    */~
            CH(6),               /* Benefit eligibility date           */~
            CH(6),               /* Benefit start date                 */~
            CH(10),              /* Amount of insurance coverage       */~
            CH(16),              /* Who is covered by insurance progra */~
            3*CH(10),            /* Premium amount(s) - often subscrip */~
            3*CH(30),            /* What is the premium for- often sub */~
            3*CH(10),            /* Deductable amount - often subscrip */~
            3*CH(30),            /* What is the deductable for? often  */~
            3*CH(1),             /* Is the deductable yet met? often s */~
            4*CH(10),            /* Charges accumulated year to date-o */~
            4*CH(30),            /* What are the charges for? often su */~
            3*CH(50),            /* Any free text information          */~
            CH(178)              /* filler for rest of record or inter */~

        FMT                      /* FILE: EMPHSTRY                     */~
            CH(6),               /* Date of next review                */~
            CH(16),              /* Job classification as per EEO.     */~
            CH(16),              /* job title                          */~
            CH(12),              /* employee code                      */~
            CH(3),               /* General purpose sequence number    */~
            CH(6),               /* Date from                          */~
            CH(6),               /* Date to                            */~
            CH(10),              /* Rate of pay in dollars and cents   */~
            CH(16),              /* Frequency of pay description       */~
            CH(30),              /* Name of supervisor                 */~
            CH(30),              /* Name of manager                    */~
            CH(6),               /* Last review date                   */~
            CH(30),              /* Last review by                     */~
            CH(10),              /* Last review increase amount        */~
            CH(10),              /* Last review increase percentage    */~
            CH(16),              /* Last review increase for descripti */~
            2*CH(50),            /* Last reviewer's comments           */~
            CH(6),               /* Date next review notice sent to su */~
            CH(171)              /* filler for rest of record or inter */~


        REM *************************************************************~
            *   INITIATE ALL CONTROL HERE  -  USER DETERMINES           *~
            *   THE FILE, THE KEY, THE STARTING AND THE ENDING VALUES   *~
            *                                                           *~
            *************************************************************

L06060:     init(" ") nul$
        accept                                                           ~
               at (01,03),                                               ~
        "  Personnel master printed          +---------------------------~
        ~------------+",                                                  ~
               at (02,03),                                               ~
        "  report specification form         !          Print a List of  ~
        ~            !",                                                  ~
               at (03,03),                                               ~
        "+-----------------------------------+-----------+------------+--~
        ~------------+",                                                  ~
               at (04,03),                                               ~
        "! Put In Order By                   ! EMPLOYEES ! APPLICANTS ! R~
        ~EQUISITIONS !",                                                  ~
               at (05,03),                                               ~
        "+-----------------------------------+-----------+------------+--~
        ~------------+",                                                  ~
               at (06,03),                                               ~
        "! REQUISITION NUMBER                !           !            !  ~
        ~            !",                                                  ~
               at (07,03),                                               ~
        "! EMPLOYEE CODE ASSIGNED            !           !            !  ~
        ~            !",                                                  ~
               at (08,03),                                               ~
        "! SOCIAL SECURITY NUMBER            !           !            !  ~
        ~            !",                                                  ~
               at (09,03),                                               ~
        "! NAME (LAST, FIRST MI)             !           !            !  ~
        ~            !",                                                  ~
               at (10,03),                                               ~
        "! STATUS                            !           !            !  ~
        ~            !",                                                  ~
               at (11,03),                                               ~
        "! JOB TITLES (HELD, APPLIED/REQD)   !           !            !  ~
        ~            !",                                                  ~
               at (12,03),                                               ~
        "! SKILL                             !           !            !  ~
        ~            !",                                                  ~
               at (13,03),                                               ~
        "! FRINGE BENEFITS GRANTED           !           !            !  ~
        ~            !",                                                  ~
               at (14,03),                                               ~
        "! INSURANCE PROGRAMS IN FORCE       !           !            !  ~
        ~            !",                                                  ~
               at (15,03),                                               ~
        "! EEOC CODE OF JOB                  !           !            !  ~
        ~            !",                                                  ~
               at (16,03),                                               ~
        "! DATE OF NEXT REVIEW               !           !            !  ~
        ~            !",                                                  ~
               at (17,03),                                               ~
        "! DATE OF REQUISITION               !           !            !  ~
        ~            !",                                                  ~
               at (18,03),                                               ~
        "! WHO MADE THE REQUISITION REQUEST  !           !            !  ~
        ~            !",                                                  ~
               at (19,03),                                               ~
        "! DATE REQUISITION NEEDED BY        !           !            !  ~
        ~            !",                                                  ~
               at (20,03),                                               ~
        "! DATE LAST INTERVIEWED             !           !            !  ~
        ~            !",                                                  ~
               at (21,03),                                               ~
        "! WHO LAST INTERVIEWED              !           !            !  ~
        ~            !",                                                  ~
               at (22,03),                                               ~
        "+-----------------------------------+-----------+------------+--~
        ~------------+",                                                  ~
               at (23,05),                                               ~
        "TAB cursor to BRIGHT BOX to select desired report, then (ENTER) ~
        ~to PRINT",                                                       ~
               at (24,03),                                               ~
        "(13)instructions    (15)print this screen   (16)exit without pri~
        ~nting reports",                                                  ~
           at(06,58), fac(hex(80)), nul$, ch(1),                         ~
           at(06,71), fac(hex(80)), nul$, ch(1),                         ~
           at(07,45), fac(hex(80)), nul$, ch(1),                         ~
           at(07,58), fac(hex(80)), nul$, ch(1),                         ~
           at(08,45), fac(hex(80)), nul$, ch(1),                         ~
           at(08,58), fac(hex(80)), nul$, ch(1),                         ~
           at(09,45), fac(hex(80)), nul$, ch(1),                         ~
           at(09,58), fac(hex(80)), nul$, ch(1),                         ~
           at(10,45), fac(hex(80)), nul$, ch(1),                         ~
           at(10,71), fac(hex(80)), nul$, ch(1),                         ~
           at(11,45), fac(hex(80)), nul$, ch(1),                         ~
           at(11,58), fac(hex(80)), nul$, ch(1),                         ~
           at(12,45), fac(hex(80)), nul$, ch(1),                         ~
           at(12,58), fac(hex(80)), nul$, ch(1),                         ~
           at(12,71), fac(hex(80)), nul$, ch(1),                         ~
           at(13,45), fac(hex(80)), nul$, ch(1),                         ~
           at(14,45), fac(hex(80)), nul$, ch(1),                         ~
           at(15,45), fac(hex(80)), nul$, ch(1),                         ~
           at(16,45), fac(hex(80)), nul$, ch(1),                         ~
           at(17,71), fac(hex(80)), nul$, ch(1),                         ~
           at(18,71), fac(hex(80)), nul$, ch(1),                         ~
           at(19,71), fac(hex(80)), nul$, ch(1),                         ~
           at(20,58), fac(hex(80)), nul$, ch(1),                         ~
           at(21,58), fac(hex(80)), nul$, ch(1),                         ~
           keys(hex(000d0f10)), key(keyhit%)


           if keyhit% = 16% then goto L65000

           if keyhit% = 0% then goto L09170

           if keyhit% <> 13% then goto  L09130
                     call "MANUAL" ("PERREPRT")
                     goto   L06060

L09130:    if keyhit% <> 15% then goto L06060
                     call "PRNTSCRN"
                     goto   L06060

L09170:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

           fieldnr% = cursor%(1) - 5%
           if fieldnr% < 1% or fieldnr% > 16% then goto L06060

           init(" ") beg$(), lst$()
           init(hex(8c)) lfac$()
           lfac$(fieldnr%) = hex(81)

L09260: accept                                                           ~
               at (01,03),                                               ~
        "  Personnel master printed          +---------------------------~
        ~------------+",                                                  ~
               at (02,03),                                               ~
        "  report specification form         ! SPECIFY RANGE, LEAVE BLANK~
        ~ FOR 'ALL'  !",                                                  ~
               at (03,03),                                               ~
        "+-----------------------------------+---------------------------~
        ~------------+",                                                  ~
               at (04,03),                                               ~
        "! Put In Order By                   ! BEGINNING WITH            ~
        ~            !",                                                  ~
               at (05,03),                                               ~
        "+-----------------------------------+---------------------------~
        ~------------+",                                                  ~
               at (06,03),                                               ~
        "! REQUISITION NUMBER                !                           ~
        ~            !",                                                  ~
               at (07,03),                                               ~
        "! EMPLOYEE CODE ASSIGNED            !                           ~
        ~            !",                                                  ~
               at (08,03),                                               ~
        "! SOCIAL SECURITY NUMBER            !                           ~
        ~            !",                                                  ~
               at (09,03),                                               ~
        "! NAME (LAST, FIRST MI)             !                           ~
        ~            !",                                                  ~
               at (10,03),                                               ~
        "! STATUS                            !                           ~
        ~            !",                                                  ~
               at (11,03),                                               ~
        "! JOB TITLES (HELD, APPLIED/REQD)   !                           ~
        ~            !",                                                  ~
               at (12,03),                                               ~
        "! SKILL                             !                           ~
        ~            !",                                                  ~
               at (13,03),                                               ~
        "! FRINGE BENEFITS GRANTED           !                           ~
        ~            !",                                                  ~
               at (14,03),                                               ~
        "! INSURANCE PROGRAMS IN FORCE       !                           ~
        ~            !",                                                  ~
               at (15,03),                                               ~
        "! EEOC CODE OF JOB                  !                           ~
        ~            !",                                                  ~
               at (16,03),                                               ~
        "! DATE OF NEXT REVIEW               !                           ~
        ~            !",                                                  ~
               at (17,03),                                               ~
        "! DATE OF REQUISITION               !                           ~
        ~            !",                                                  ~
               at (18,03),                                               ~
        "! WHO MADE THE REQUISITION REQUEST  !                           ~
        ~            !",                                                  ~
               at (19,03),                                               ~
        "! DATE REQUISITION NEEDED BY        !                           ~
        ~            !",                                                  ~
               at (20,03),                                               ~
        "! DATE LAST INTERVIEWED             !                           ~
        ~            !",                                                  ~
               at (21,03),                                               ~
        "! WHO LAST INTERVIEWED              !                           ~
        ~            !",                                                  ~
               at (22,03),                                               ~
        "+-----------------------------------+---------------------------~
        ~------------+",                                                  ~
               at (23,05),                                               ~
        "SPECIFY REPORT RANGE AS REQUESTED                  then (ENTER) ~
        ~to PRINT",                                                       ~
               at (24,03),                                               ~
        "(13)instructions    (15)print this screen   (16)exit without pri~
        ~nting reports",                                                  ~
           at (06,41), fac(lfac$(01)), beg$(01), ch(05),                 ~
           at (06,62), fac(lfac$(01)), lst$(01), ch(05),                 ~
           at (07,41), fac(lfac$(02)), beg$(02), ch(16),                 ~
           at (07,62), fac(lfac$(02)), lst$(02), ch(16),                 ~
           at (08,41), fac(lfac$(03)), beg$(03), ch(11),                 ~
           at (08,62), fac(lfac$(03)), lst$(03), ch(11),                 ~
           at (09,41), fac(lfac$(04)), beg$(04), ch(15),                 ~
           at (09,62), fac(lfac$(04)), lst$(04), ch(15),                 ~
           at (10,41), fac(lfac$(05)), beg$(05), ch(01),                 ~
           at (10,62), fac(lfac$(05)), lst$(05), ch(01),                 ~
           at (11,41), fac(lfac$(06)), beg$(06), ch(16),                 ~
           at (11,62), fac(lfac$(06)), lst$(06), ch(16),                 ~
           at (12,41), fac(lfac$(07)), beg$(07), ch(16),                 ~
           at (12,62), fac(lfac$(07)), lst$(07), ch(16),                 ~
           at (13,41), fac(lfac$(08)), beg$(08), ch(16),                 ~
           at (13,62), fac(lfac$(08)), lst$(08), ch(16),                 ~
           at (14,41), fac(lfac$(09)), beg$(09), ch(16),                 ~
           at (14,62), fac(lfac$(09)), lst$(09), ch(16),                 ~
           at (15,41), fac(lfac$(10)), beg$(10), ch(16),                 ~
           at (15,62), fac(lfac$(10)), lst$(10), ch(16),                 ~
           at (16,41), fac(lfac$(11)), beg$(11), ch(10),                 ~
           at (16,62), fac(lfac$(11)), lst$(11), ch(10),                 ~
           at (17,41), fac(lfac$(12)), beg$(12), ch(10),                 ~
           at (17,62), fac(lfac$(12)), lst$(12), ch(10),                 ~
           at (18,41), fac(lfac$(13)), beg$(13), ch(16),                 ~
           at (18,62), fac(lfac$(13)), lst$(13), ch(16),                 ~
           at (19,41), fac(lfac$(14)), beg$(14), ch(10),                 ~
           at (19,62), fac(lfac$(14)), lst$(14), ch(10),                 ~
           at (20,41), fac(lfac$(15)), beg$(15), ch(10),                 ~
           at (20,62), fac(lfac$(15)), lst$(15), ch(10),                 ~
           at (21,41), fac(lfac$(16)), beg$(16), ch(16),                 ~
           at (21,62), fac(lfac$(16)), lst$(16), ch(16),                 ~
           keys(hex(000d0f10)), key(keyhit%)

           if keyhit% = 16% then goto  L06060

           if keyhit% = 0% then goto L10260

           if keyhit% <> 13% then goto  L10250
                     call "MANUAL" ("PERREPRT")
                     goto   L09260

L10250:    if keyhit% <> 15% then goto L09260
                     call "PRNTSCRN"
                     goto   L09260

L10260: REM *************************************************************~
           *         CONTROL BRANCHING BASED ON WHERE CURSOR WAS        *~
           *         AFTER SETTING PLOWKEY AND B% (BREAK POINT)         *~
            *************************************************************

           init(" ") plowkey$
           if fieldnr% < 11% or fieldnr% = 13% or fieldnr% > 15% ~
                                                then non_date_init
           errormsg$ = " "
           if beg$(fieldnr%) = " " then test_end_date
           call "DATEOKC" (beg$(fieldnr%), 0%, errormsg$ )
           if errormsg$ = " " then test_end_date
           call "ASKUSER"(2%, "Error in Start Date", beg$(fieldnr%), ~
                                                     errormsg$, ~
                                           "Press any PF Key to Continue" )
           goto L09260
test_end_date
           if lst$(fieldnr%) = " " then date_range_chk
           call "DATEOKC" (lst$(fieldnr%), 0%, errormsg$ )
           if errormsg$ = " " then date_range_chk
           call "ASKUSER"( 2%, "Error in End Date", lst$(fieldnr%), errormsg$, ~
                                           "Press any PF Key to Continue" )
           goto L09260
date_range_chk
           call "DATUFMTC" (beg$(fieldnr%))
           call "DATUFMTC" (lst$(fieldnr%))
           if beg$(fieldnr%) <= lst$(fieldnr%) then date_init
           call "DATFMTC" (beg$(fieldnr%))
           call "DATFMTC" (lst$(fieldnr%))
           call "ASKUSER" ( 2%, "Error in Date Range", ~
                                       "From " & beg$(fieldnr%) & " is after", ~
                                       "To   " & lst$(fieldnr%), ~
                                       "Press any PF Key to Continue" )
           goto L09260
date_init
           b% = 6%
           if beg$(fieldnr%) = " " then beg$(fieldnr%) = blankdate$
           if lst$(fieldnr%) = blankdate$ then lst$(fieldnr%) = " "
           goto date_begin
non_date_init
           b% = 1%
           if beg$(fieldnr%) = "ALL" then beg$(fieldnr%) = " "
           if beg$(fieldnr%) = " " then goto L10350
           b% = max(len(beg$(fieldnr%)), 1%)
date_begin
           str(plowkey$,1%, b%) = str(beg$(fieldnr%),1%,b%)
L10350:    plowkey$ = plowkey$ addc all(hex(ff))
           pge% = 0%
           line% = 1000%

           select printer(134)
           if cursor%(1) = 6% and cursor%(2) = 58% then goto  L14310
           if cursor%(1) = 6% and cursor%(2) = 71% then goto  L16280
           if cursor%(1) = 7% and cursor%(2) = 45% then goto  L11050
           if cursor%(1) = 7% and cursor%(2) = 58% then goto  L14540
           if cursor%(1) = 8% and cursor%(2) = 45% then goto  L11280
           if cursor%(1) = 8% and cursor%(2) = 58% then goto  L14830
           if cursor%(1) = 9% and cursor%(2) = 45% then goto  L11610
           if cursor%(1) = 9% and cursor%(2) = 58% then goto  L15120
           if cursor%(1) = 10% and cursor%(2) = 45% then goto  L11940
           if cursor%(1) = 10% and cursor%(2) = 71% then goto  L16570
           if cursor%(1) = 11% and cursor%(2) = 45% then goto  L12270
           if cursor%(1) = 11% and cursor%(2) = 58% then goto  L15410
           if cursor%(1) = 12% and cursor%(2) = 45% then goto  L13620
           if cursor%(1) = 12% and cursor%(2) = 58% then goto  L13870
           if cursor%(1) = 12% and cursor%(2) = 71% then goto  L14090
           if cursor%(1) = 13% and cursor%(2) = 45% then goto  L13380
           if cursor%(1) = 14% and cursor%(2) = 45% then goto  L13120
           if cursor%(1) = 15% and cursor%(2) = 45% then goto  L12600
           if cursor%(1) = 16% and cursor%(2) = 45% then goto  L12860
           if cursor%(1) = 17% and cursor%(2) = 71% then goto  L16860
           if cursor%(1) = 18% and cursor%(2) = 71% then goto  L17150
           if cursor%(1) = 19% and cursor%(2) = 71% then goto  L17440
           if cursor%(1) = 20% and cursor%(2) = 58% then goto  L15700
           if cursor%(1) = 21% and cursor%(2) = 58% then goto  L15990
                     close printer
                     goto L06060


        REM *************************************************************~
           *         CONTROL PRINTING IN THIS SECTION                   *~
           *                                                            *~
            *************************************************************

L11050:    call "SHOSTAT" ("PRINTING EMPLOYEES BY EMPLOYEE CODE")
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           hdrvar$ = "EMPLOYEE CODE"
           k% =  0%                     /* SELECT THE KEY  TO PLOW ON  */
L11070:    call "PLOWALTS" (#8, plowkey$, k%, 0%, f1%(08))
                     if f1%(08) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #8, using  L11096   , str(l$(),1)
                     FMT CH(100)                   /* APLSKILL   */
                     FMT CH(100)                   /* REQSKILL   */
                     FMT CH(100)                   /* EMPSKILL   */
                     FMT CH(600)                   /* APLMASTR   */
                     FMT CH(370)                   /* REQMASTR   */
L11096:              FMT CH(950)                   /* PERMASTR   */
                     FMT CH(250)                   /* PERFRNGE   */
                     FMT CH(800)                   /* INSMASTR   */
                     FMT CH(500)                   /* EMPHSTRY   */
           if line% > 55% then gosub   L20110       /* THE HEADER */
              recdate$(1%) = str(l$(),229, 6)
              call "DATFMTC" (recdate$(1%))
              recdate$(2%) = str(l$(),396, 6)
              call "DATEFMT" (recdate$(2%))
           print using L20080    ,  str(l$(), 1, 1),                      ~
                                   str(l$(), 2,15),                      ~
                                   str(l$(),17,10),                      ~
                                   str(l$(),27, 1),                      ~
                                   str(l$(),39,12),                      ~
                                   str(l$(),28,11),                      ~
                                   str(l$(),228, 1),                     ~
                                   recdate$(1%),                         ~
                                   str(l$(),235, 3),                     ~
                                   str(l$(),238, 1),                     ~
                                   str(l$(),239, 2),                     ~
                                   str(l$(),337,16),                     ~
                                   str(l$(),369, 4),                     ~
                                   str(l$(),373, 1),                     ~
                                   recdate$(2%)
           line% = line% + 1%
           goto L11070

L11280:    call "SHOSTAT"
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           hdrvar$ = "SOCIAL SECURITY NUMBER"
           k% =  1%                     /* SELECT THE KEY  TO PLOW ON  */
L11310:    call "PLOWALTS" (#8, plowkey$, k%, 0%, f1%(08))
                     if f1%(08) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #8, using  L11390   , str(l$(),1)
                     FMT CH(100)                   /* APLSKILL   */
                     FMT CH(100)                   /* REQSKILL   */
                     FMT CH(100)                   /* EMPSKILL   */
                     FMT CH(600)                   /* APLMASTR   */
                     FMT CH(370)                   /* REQMASTR   */
L11390:              FMT CH(950)                   /* PERMASTR   */
                     FMT CH(250)                   /* PERFRNGE   */
                     FMT CH(800)                   /* INSMASTR   */
                     FMT CH(500)                   /* EMPHSTRY   */
              recdate$(1%) = str(l$(),229, 6)
              call "DATFMTC" (recdate$(1%))
              recdate$(2%) = str(l$(),396, 6)
              call "DATEFMT" (recdate$(2%))
           if line% > 55% then gosub   L20110       /* THE HEADER */
           print using L20080    ,  str(l$(), 1, 1),                      ~
                                   str(l$(), 2,15),                      ~
                                   str(l$(),17,10),                      ~
                                   str(l$(),27, 1),                      ~
                                   str(l$(),39,12),                      ~
                                   str(l$(),28,11),                      ~
                                   str(l$(),228, 1),                     ~
                                   recdate$(1%),                         ~
                                   str(l$(),235, 3),                     ~
                                   str(l$(),238, 1),                     ~
                                   str(l$(),239, 2),                     ~
                                   str(l$(),337,16),                     ~
                                   str(l$(),369, 4),                     ~
                                   str(l$(),373, 1),                     ~
                                   recdate$(2%)
           line% = line% + 1%
           goto L11310

L11610:    call "SHOSTAT" ("PRINTING EMPLOYEES BY NAME")
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           hdrvar$ = "EMPLOYEE NAME"
           k% =  2%                     /* SELECT THE KEY  TO PLOW ON  */
L11640:    call "PLOWALTS" (#8, plowkey$, k%, 0%, f1%(08))
                     if f1%(08) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #8, using  L11720   , str(l$(),1)
                     FMT CH(100)                   /* APLSKILL   */
                     FMT CH(100)                   /* REQSKILL   */
                     FMT CH(100)                   /* EMPSKILL   */
                     FMT CH(600)                   /* APLMASTR   */
                     FMT CH(370)                   /* REQMASTR   */
L11720:              FMT CH(950)                   /* PERMASTR   */
                     FMT CH(250)                   /* PERFRNGE   */
                     FMT CH(800)                   /* INSMASTR   */
                     FMT CH(500)                   /* EMPHSTRY   */
              recdate$(1%) = str(l$(),229, 6)
              call "DATFMTC" (recdate$(1%))
              recdate$(2%) = str(l$(),396, 6)
              call "DATEFMT" (recdate$(2%))
           if line% > 55% then gosub   L20110       /* THE HEADER */
           print using L20080    ,  str(l$(), 1, 1),                      ~
                                   str(l$(), 2,15),                      ~
                                   str(l$(),17,10),                      ~
                                   str(l$(),27, 1),                      ~
                                   str(l$(),39,12),                      ~
                                   str(l$(),28,11),                      ~
                                   str(l$(),228, 1),                     ~
                                   recdate$(1%),                         ~
                                   str(l$(),235, 3),                     ~
                                   str(l$(),238, 1),                     ~
                                   str(l$(),239, 2),                     ~
                                   str(l$(),337,16),                     ~
                                   str(l$(),369, 4),                     ~
                                   str(l$(),373, 1),                     ~
                                   recdate$(2%)
           line% = line% + 1%
           goto L11640

L11940:    call "SHOSTAT" ("PRINTING EMPLOYEES BY CURRENT STATUS")
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           hdrvar$ = "CURRENT STATUS"
           k% =  3%                     /* SELECT THE KEY  TO PLOW ON  */
L11970:    call "PLOWALTS" (#8, plowkey$, k%, 0%, f1%(08))
                     if f1%(08) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #8, using  L12050   , str(l$(),1)
                     FMT CH(100)                   /* APLSKILL   */
                     FMT CH(100)                   /* REQSKILL   */
                     FMT CH(100)                   /* EMPSKILL   */
                     FMT CH(600)                   /* APLMASTR   */
                     FMT CH(370)                   /* REQMASTR   */
L12050:              FMT CH(950)                   /* PERMASTR   */
                     FMT CH(250)                   /* PERFRNGE   */
                     FMT CH(800)                   /* INSMASTR   */
                     FMT CH(500)                   /* EMPHSTRY   */
              recdate$(1%) = str(l$(),229, 6)
              call "DATFMTC" (recdate$(1%))
              recdate$(2%) = str(l$(),396, 6)
              call "DATEFMT" (recdate$(2%))
           if line% > 55% then gosub   L20110       /* THE HEADER */
           print using L20080    ,  str(l$(), 1, 1),                      ~
                                   str(l$(), 2,15),                      ~
                                   str(l$(),17,10),                      ~
                                   str(l$(),27, 1),                      ~
                                   str(l$(),39,12),                      ~
                                   str(l$(),28,11),                      ~
                                   str(l$(),228, 1),                     ~
                                   recdate$(1%),                         ~
                                   str(l$(),235, 3),                     ~
                                   str(l$(),238, 1),                     ~
                                   str(l$(),239, 2),                     ~
                                   str(l$(),337,16),                     ~
                                   str(l$(),369, 4),                     ~
                                   str(l$(),373, 1),                     ~
                                   recdate$(2%)
           line% = line% + 1%
           goto L11970

L12270:    call "SHOSTAT" ("PRINTING ALL JOBS HELD BY EMPLOYEES")
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           k% =  1%                     /* SELECT THE KEY  TO PLOW ON  */
L12300:    call "PLOWALTS" (#11, plowkey$, k%, 0%, f1%(11))
                     if f1%(11) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #11, using  L12410   , str(l$(),1)
                     FMT CH(100)                   /* APLSKILL   */
                     FMT CH(100)                   /* REQSKILL   */
                     FMT CH(100)                   /* EMPSKILL   */
                     FMT CH(600)                   /* APLMASTR   */
                     FMT CH(370)                   /* REQMASTR   */
                     FMT CH(950)                   /* PERMASTR   */
                     FMT CH(250)                   /* PERFRNGE   */
                     FMT CH(800)                   /* INSMASTR   */
L12410:              FMT CH(500)                   /* EMPHSTRY   */
              recdate$(1%) = str(l$(),54,06)
              call "DATFMTC" (recdate$(1%))
              recdate$(2%) = str(l$(),60,06)
              call "DATFMTC" (recdate$(2%))
           if line% > 55% then gosub   L20280       /* THE HEADER */
           print using L20250    ,  str(l$(),39,12),                      ~
                                   str(l$(),23,16),                      ~
                                   recdate$(1%),                         ~
                                   recdate$(2%),                         ~
                                   str(l$(),66,10),                      ~
                                   str(l$(),76,16),                      ~
                                   str(l$(),  7,16),                     ~
                                   str(l$(), 92,30)
           line% = line% + 1%
           goto L12300

L12600:    call "SHOSTAT" ("PRINTING EEO CODE OF ALL JOBS HELD BY EMPS")
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           k% =  2%                     /* SELECT THE KEY  TO PLOW ON  */
L12630:    call "PLOWALTS" (#11, plowkey$, k%, 0%, f1%(11))
                     if f1%(11) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #11, using  L12740   , str(l$(),1)
                     FMT CH(100)                   /* APLSKILL   */
                     FMT CH(100)                   /* REQSKILL   */
                     FMT CH(100)                   /* EMPSKILL   */
                     FMT CH(600)                   /* APLMASTR   */
                     FMT CH(370)                   /* REQMASTR   */
                     FMT CH(950)                   /* PERMASTR   */
                     FMT CH(250)                   /* PERFRNGE   */
                     FMT CH(800)                   /* INSMASTR   */
L12740:              FMT CH(500)                   /* EMPHSTRY   */
              recdate$(1%) = str(l$(),54,06)
              call "DATFMTC" (recdate$(1%))
              recdate$(2%) = str(l$(),60,06)
              call "DATFMTC" (recdate$(2%))
           if line% > 55% then gosub   L20360       /* THE HEADER */
           print using L20250    ,  str(l$(),39,12),                      ~
                                   str(l$(),23,16),                      ~
                                   recdate$(1%),                         ~
                                   recdate$(2%),                         ~
                                   str(l$(),66,10),                      ~
                                   str(l$(),76,16),                      ~
                                   str(l$(),  7,16),                     ~
                                   str(l$(), 92,30)
           line% = line% + 1%
           goto L12630

L12860:    call "SHOSTAT" ("PRINTING DATE OF NEXT REVIEW FOR EMPLOYEES")
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           k% =  3%                     /* SELECT THE KEY  TO PLOW ON  */
L12890:    call "PLOWALTS" (#11, plowkey$, k%, 0%, f1%(11))
                     if f1%(11) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #11, using  L13000   , str(l$(),1)
                     FMT CH(100)                   /* APLSKILL   */
                     FMT CH(100)                   /* REQSKILL   */
                     FMT CH(100)                   /* EMPSKILL   */
                     FMT CH(600)                   /* APLMASTR   */
                     FMT CH(370)                   /* REQMASTR   */
                     FMT CH(950)                   /* PERMASTR   */
                     FMT CH(250)                   /* PERFRNGE   */
                     FMT CH(800)                   /* INSMASTR   */
L13000:              FMT CH(500)                   /* EMPHSTRY   */
              recdate$(1%) = str(l$(),01%,06%)
              call "DATEFMT" (recdate$(1%))
              recdate$(2%) = str(l$(),54%,06%)
              call "DATFMTC" (recdate$(2%))
              recdate$(3%) = str(l$(),60%,06%)
              call "DATFMTC" (recdate$(3%))
           if line% > 55% then gosub   L20540       /* THE HEADER */
           print using L20250    ,  str(l$(),39,12),                      ~
                                   str(l$(),23,16),                      ~
                                   recdate$(2%),                         ~
                                   recdate$(3%),                         ~
                                   str(l$(),66,10),                      ~
                                   str(l$(),76,16),                      ~
                                   recdate$(1%),                         ~
                                   str(l$(), 92,30)
           line% = line% + 1%
           goto L12890

L13120:    call "SHOSTAT" ("PRINTING LIST BY INSURANCE IN FORCE")
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           k% =  1%                     /* SELECT THE KEY  TO PLOW ON  */
L13150:    call "PLOWALTS" (#10, plowkey$, k%, 0%, f1%(10))
                     if f1%(10) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #10, using  L13250   , str(l$(),1)
                     FMT CH(100)                   /* APLSKILL   */
                     FMT CH(100)                   /* REQSKILL   */
                     FMT CH(100)                   /* EMPSKILL   */
                     FMT CH(600)                   /* APLMASTR   */
                     FMT CH(370)                   /* REQMASTR   */
                     FMT CH(950)                   /* PERMASTR   */
                     FMT CH(250)                   /* PERFRNGE   */
L13250:              FMT CH(800)                   /* INSMASTR   */
                     FMT CH(500)                   /* EMPHSTRY   */
              recdate$(1%) = str(l$(),32,06)
              call "DATEFMT" (recdate$(1%))
              recdate$(2%) = str(l$(),38,06)
              call "DATEFMT" (recdate$(2%))
           if line% > 55% then gosub   L20820       /* THE HEADER */
           print using  L20790   ,  str(l$(), 1,16),                      ~
                                   str(l$(),17,12),                      ~
                                   recdate$(1%),                         ~
                                   recdate$(2%),                         ~
                                   str(l$(),44,10),                      ~
                                   str(l$(),54,16)
           line% = line% + 1%
           goto L13150

L13380:    call "SHOSTAT" ("PRINTING LIST BY FRINGE BENEFITS GRANTED")
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           k% =  1%                     /* SELECT THE KEY  TO PLOW ON  */
L13410:    call "PLOWALTS" (#9 , plowkey$, k%, 0%, f1%( 9))
                     if f1%( 9) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #9 , using  L13500   , str(l$(),1)
                     FMT CH(100)                   /* APLSKILL   */
                     FMT CH(100)                   /* REQSKILL   */
                     FMT CH(100)                   /* EMPSKILL   */
                     FMT CH(600)                   /* APLMASTR   */
                     FMT CH(370)                   /* REQMASTR   */
                     FMT CH(950)                   /* PERMASTR   */
L13500:              FMT CH(250)                   /* PERFRNGE   */
                     FMT CH(800)                   /* INSMASTR   */
                     FMT CH(500)                   /* EMPHSTRY   */
              recdate$(1%) = str(l$(),32,06)
              call "DATEFMT" (recdate$(1%))
              recdate$(2%) = str(l$(),38,06)
              call "DATEFMT" (recdate$(2%))
           if line% > 55% then gosub   L21000       /* THE HEADER */
           print using  L20970   ,  str(l$(), 1,16),                      ~
                                   str(l$(),17,12),                      ~
                                   recdate$(1%),                         ~
                                   recdate$(2%),                         ~
                                   str(l$(),44,10),                      ~
                                   str(l$(),54,10),                      ~
                                   str(l$(),64,30)
           line% = line% + 1%
           goto L13410

L13620:    call "SHOSTAT" ("PRINTING LIST OF EMPLOYEE SKILLS")
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           k% =  1%                     /* SELECT THE KEY  TO PLOW ON  */
L13650:    call "PLOWALTS" (#3 , plowkey$, k%, 0%, f1%( 3))
                     if f1%( 3) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #3 , using  L13700   , str(l$(),1)
                     FMT CH(100)                   /* APLSKILL   */
                     FMT CH(100)                   /* REQSKILL   */
L13700:              FMT CH(100)                   /* EMPSKILL   */
                     FMT CH(600)                   /* APLMASTR   */
                     FMT CH(370)                   /* REQMASTR   */
                     FMT CH(950)                   /* PERMASTR   */
                     FMT CH(250)                   /* PERFRNGE   */
                     FMT CH(800)                   /* INSMASTR   */
                     FMT CH(500)                   /* EMPHSTRY   */
           if line% > 55% then gosub L21180         /* THE HEADER */
           print using  L21150   ,  str(l$(), 1,16),                      ~
                                   str(l$(),17,12),                      ~
                                   str(l$(),31,04),                      ~
                                   str(l$(),35,42)
           line% = line% + 1%
           goto L13650

L13870:    call "SHOSTAT" ("PRINTING LIST OF APPLICANT SKILLS")
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           k% =  1%                     /* SELECT THE KEY  TO PLOW ON  */
L13900:    call "PLOWALTS" (#1 , plowkey$, k%, 0%, f1%( 1))
                     if f1%( 1) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #1 , using  L13930   , str(l$(),1)
L13930:              FMT CH(100)                   /* APLSKILL   */
                     FMT CH(100)                   /* REQSKILL   */
                     FMT CH(100)                   /* EMPSKILL   */
                     FMT CH(600)                   /* APLMASTR   */
                     FMT CH(370)                   /* REQMASTR   */
                     FMT CH(950)                   /* PERMASTR   */
                     FMT CH(250)                   /* PERFRNGE   */
                     FMT CH(800)                   /* INSMASTR   */
                     FMT CH(500)                   /* EMPHSTRY   */
           if line% > 55% then gosub   L21360       /* THE HEADER */
           print using  L21330   ,  str(l$(), 1,16),                      ~
                                   str(l$(),17,11),                      ~
                                   str(l$(),30,04),                      ~
                                   str(l$(),34,42)
           line% = line% + 1%
           goto L13900

L14090:    call "SHOSTAT" ("PRINTING LIST OF SKILLS NEEDED ON REQUISITION~
        ~S")
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           k% =  1%                     /* SELECT THE KEY  TO PLOW ON  */
L14120:    call "PLOWALTS" (#2 , plowkey$, k%, 0%, f1%( 2))
                     if f1%( 2) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #2 , using  L14160   , str(l$(),1)
                     FMT CH(100)                   /* APLSKILL   */
L14160:              FMT CH(100)                   /* REQSKILL   */
                     FMT CH(100)                   /* EMPSKILL   */
                     FMT CH(600)                   /* APLMASTR   */
                     FMT CH(370)                   /* REQMASTR   */
                     FMT CH(950)                   /* PERMASTR   */
                     FMT CH(250)                   /* PERFRNGE   */
                     FMT CH(800)                   /* INSMASTR   */
                     FMT CH(500)                   /* EMPHSTRY   */
           if line% > 55% then gosub  L21540        /* THE HEADER */
           print using   L21510  ,  str(l$(), 1,16),                      ~
                                   str(l$(),17, 5),                      ~
                                   str(l$(),22,04),                      ~
                                   str(l$(),26,42)
           line% = line% + 1%
           goto L14120

L14310:    call "SHOSTAT" ("PRINTING LIST OF APPLICANTS FOR   REQUISITION~
        ~S")
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           k% =  5%                     /* SELECT THE KEY  TO PLOW ON  */
L14350:    call "PLOWALTS" (#5 , plowkey$, k%, 0%, f1%( 5))
                     if f1%( 5) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #5 , using  L14410   , str(l$(),1)
                     FMT CH(100)                   /* APLSKILL   */
                     FMT CH(100)                   /* REQSKILL   */
                     FMT CH(100)                   /* EMPSKILL   */
L14410:              FMT CH(600)                   /* APLMASTR   */
                     FMT CH(370)                   /* REQMASTR   */
                     FMT CH(950)                   /* PERMASTR   */
                     FMT CH(250)                   /* PERFRNGE   */
                     FMT CH(800)                   /* INSMASTR   */
                     FMT CH(500)                   /* EMPHSTRY   */
              recdate$(1%) = str(l$(),50%, 6%)
              call "DATEFMT" (recdate$(1%))
           if line% > 55% then gosub  L21720        /* THE HEADER */
           print using  L21690   ,  str(l$(),56,15),                      ~
                                   str(l$(),71,10),                      ~
                                   str(l$(),81,01),                      ~
                                   str(l$(),82,11),                      ~
                                   recdate$(1%),                         ~
                                   str(l$(),34,16),                      ~
                                   str(l$(),18,16),                      ~
                                   str(l$(),13, 5),                      ~
                                   str(l$(), 1,12)
           line% = line% + 1%
           goto L14350

L14540:    call "SHOSTAT" ("PRINTING LIST OF EMPLOYEE CODES ASSIGNED TO A~
        ~PPLICANTS")
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           k% =  6%                     /* SELECT THE KEY  TO PLOW ON  */
L14580:    call "PLOWALTS" (#5 , plowkey$, k%, 0%, f1%( 5))
                     if f1%( 5) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #5 , using  L14640   , str(l$(),1)
                     FMT CH(100)                   /* APLSKILL   */
                     FMT CH(100)                   /* REQSKILL   */
                     FMT CH(100)                   /* EMPSKILL   */
L14640:              FMT CH(600)                   /* APLMASTR   */
                     FMT CH(370)                   /* REQMASTR   */
                     FMT CH(950)                   /* PERMASTR   */
                     FMT CH(250)                   /* PERFRNGE   */
                     FMT CH(800)                   /* INSMASTR   */
                     FMT CH(500)                   /* EMPHSTRY   */
              recdate$(1%) = str(l$(),50%, 6%)
              call "DATEFMT" (recdate$(1%))
           if line% > 55% then gosub L21900         /* THE HEADER */
           print using  L21870   ,  str(l$(),56,15),                      ~
                                   str(l$(),71,10),                      ~
                                   str(l$(),81,01),                      ~
                                   str(l$(),82,11),                      ~
                                   recdate$(1%),                         ~
                                   str(l$(),34,16),                      ~
                                   str(l$(),18,16),                      ~
                                   str(l$(),13, 5),                      ~
                                   str(l$(), 1,12)
           line% = line% + 1%
           goto L14580

L14830:    call "SHOSTAT" ("PRINTING LIST APPLICANTS BY THEIR SOCIAL SECU~
        ~RITY NUMBERS")
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           k% =  0%                     /* SELECT THE KEY  TO PLOW ON  */
L14870:    call "PLOWALTS" (#5 , plowkey$, k%, 0%, f1%( 5))
                     if f1%( 5) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #5 , using  L14930   , str(l$(),1)
                     FMT CH(100)                   /* APLSKILL   */
                     FMT CH(100)                   /* REQSKILL   */
                     FMT CH(100)                   /* EMPSKILL   */
L14930:              FMT CH(600)                   /* APLMASTR   */
                     FMT CH(370)                   /* REQMASTR   */
                     FMT CH(950)                   /* PERMASTR   */
                     FMT CH(250)                   /* PERFRNGE   */
                     FMT CH(800)                   /* INSMASTR   */
                     FMT CH(500)                   /* EMPHSTRY   */
              recdate$(1%) = str(l$(),50%, 6%)
              call "DATEFMT" (recdate$(1%))
           if line% > 55% then gosub  L22080        /* THE HEADER */
           print using  L22050   ,  str(l$(),56,15),                      ~
                                   str(l$(),71,10),                      ~
                                   str(l$(),81,01),                      ~
                                   str(l$(),82,11),                      ~
                                   recdate$(1%),                         ~
                                   str(l$(),34,16),                      ~
                                   str(l$(),18,16),                      ~
                                   str(l$(),13, 5),                      ~
                                   str(l$(), 1,12)
           line% = line% + 1%
           goto L14870

L15120:    call "SHOSTAT" ("PRINTING LIST APPLICANTS BY THEIR NAMES      ~
        ~            ")
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           k% =  1%                     /* SELECT THE KEY  TO PLOW ON  */
L15160:    call "PLOWALTS" (#5 , plowkey$, k%, 0%, f1%( 5))
                     if f1%( 5) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #5 , using  L15220   , str(l$(),1)
                     FMT CH(100)                   /* APLSKILL   */
                     FMT CH(100)                   /* REQSKILL   */
                     FMT CH(100)                   /* EMPSKILL   */
L15220:              FMT CH(600)                   /* APLMASTR   */
                     FMT CH(370)                   /* REQMASTR   */
                     FMT CH(950)                   /* PERMASTR   */
                     FMT CH(250)                   /* PERFRNGE   */
                     FMT CH(800)                   /* INSMASTR   */
                     FMT CH(500)                   /* EMPHSTRY   */
              recdate$(1%) = str(l$(),50%, 6%)
              call "DATEFMT" (recdate$(1%))
           if line% > 55% then gosub  L22260        /* THE HEADER */
           print using  L22230   ,  str(l$(),56,15),                      ~
                                   str(l$(),71,10),                      ~
                                   str(l$(),81,01),                      ~
                                   str(l$(),82,11),                      ~
                                   recdate$(1%),                         ~
                                   str(l$(),34,16),                      ~
                                   str(l$(),18,16),                      ~
                                   str(l$(),13, 5),                      ~
                                   str(l$(), 1,12)
           line% = line% + 1%
           goto L15160

L15410:    call "SHOSTAT" ("PRINTING A LIST OF APPLICANTS BY JOB APPLIED ~
        ~FOR         ")
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           k% =  4%                     /* SELECT THE KEY  TO PLOW ON  */
L15450:    call "PLOWALTS" (#5 , plowkey$, k%, 0%, f1%( 5))
                     if f1%( 5) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #5 , using  L15510   , str(l$(),1)
                     FMT CH(100)                   /* APLSKILL   */
                     FMT CH(100)                   /* REQSKILL   */
                     FMT CH(100)                   /* EMPSKILL   */
L15510:              FMT CH(600)                   /* APLMASTR   */
                     FMT CH(370)                   /* REQMASTR   */
                     FMT CH(950)                   /* PERMASTR   */
                     FMT CH(250)                   /* PERFRNGE   */
                     FMT CH(800)                   /* INSMASTR   */
                     FMT CH(500)                   /* EMPHSTRY   */
              recdate$(1%) = str(l$(),50%, 6%)
              call "DATEFMT" (recdate$(1%))
           if line% > 55% then gosub  L22440        /* THE HEADER */
           print using  L22410   ,  str(l$(),56,15),                      ~
                                   str(l$(),71,10),                      ~
                                   str(l$(),81,01),                      ~
                                   str(l$(),82,11),                      ~
                                   recdate$(1%),                         ~
                                   str(l$(),34,16),                      ~
                                   str(l$(),18,16),                      ~
                                   str(l$(),13, 5),                      ~
                                   str(l$(), 1,12)
           line% = line% + 1%
           goto L15450

L15700:    call "SHOSTAT" ("PRINTING A LIST OF APPLICANTS BY DATE LAST IN~
        ~TERVIEWED   ")
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           k% =  2%                     /* SELECT THE KEY  TO PLOW ON  */
L15740:    call "PLOWALTS" (#5 , plowkey$, k%, 0%, f1%( 5))
                     if f1%( 5) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #5 , using  L15800   , str(l$(),1)
                     FMT CH(100)                   /* APLSKILL   */
                     FMT CH(100)                   /* REQSKILL   */
                     FMT CH(100)                   /* EMPSKILL   */
L15800:              FMT CH(600)                   /* APLMASTR   */
                     FMT CH(370)                   /* REQMASTR   */
                     FMT CH(950)                   /* PERMASTR   */
                     FMT CH(250)                   /* PERFRNGE   */
                     FMT CH(800)                   /* INSMASTR   */
                     FMT CH(500)                   /* EMPHSTRY   */
              recdate$(1%) = str(l$(),50%, 6%)
              call "DATEFMT" (recdate$(1%))
           if line% > 55% then gosub  L22620        /* THE HEADER */
           print using  L22590   ,  str(l$(),56,15),                      ~
                                   str(l$(),71,10),                      ~
                                   str(l$(),81,01),                      ~
                                   str(l$(),82,11),                      ~
                                   recdate$(1%),                         ~
                                   str(l$(),34,16),                      ~
                                   str(l$(),18,16),                      ~
                                   str(l$(),13, 5),                      ~
                                   str(l$(), 1,12)
           line% = line% + 1%
           goto L15740

L15990:    call "SHOSTAT" ("PRINTING A LIST OF APPLICANTS BY WHO LAST INT~
        ~ERVIEWED THEM")
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           k% =  3%                     /* SELECT THE KEY  TO PLOW ON  */
L16030:    call "PLOWALTS" (#5 , plowkey$, k%, 0%, f1%( 5))
                     if f1%( 5) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #5 , using  L16090   , str(l$(),1)
                     FMT CH(100)                   /* APLSKILL   */
                     FMT CH(100)                   /* REQSKILL   */
                     FMT CH(100)                   /* EMPSKILL   */
L16090:              FMT CH(600)                   /* APLMASTR   */
                     FMT CH(370)                   /* REQMASTR   */
                     FMT CH(950)                   /* PERMASTR   */
                     FMT CH(250)                   /* PERFRNGE   */
                     FMT CH(800)                   /* INSMASTR   */
                     FMT CH(500)                   /* EMPHSTRY   */
              recdate$(1%) = str(l$(),50%, 6%)
              call "DATEFMT" (recdate$(1%))
           if line% > 55% then gosub  L22800        /* THE HEADER */
           print using  L22770   ,  str(l$(),56,15),                      ~
                                   str(l$(),71,10),                      ~
                                   str(l$(),81,01),                      ~
                                   str(l$(),82,11),                      ~
                                   recdate$(1%),                         ~
                                   str(l$(),34,16),                      ~
                                   str(l$(),18,16),                      ~
                                   str(l$(),13, 5),                      ~
                                   str(l$(), 1,12)
           line% = line% + 1%
           goto L16030

L16280:    call "SHOSTAT" ("PRINTING A LIST OF REQUISITIONS BY REQUISITIO~
        ~N NUMBER     ")
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           k% =  0%                     /* SELECT THE KEY  TO PLOW ON  */
L16320:    call "PLOWALTS" (#6 , plowkey$, k%, 0%, f1%( 6))
                     if f1%( 6) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #6 , using  L16390   , str(l$(),1)
                     FMT CH(100)                   /* APLSKILL   */
                     FMT CH(100)                   /* REQSKILL   */
                     FMT CH(100)                   /* EMPSKILL   */
                     FMT CH(600)                   /* APLMASTR   */
L16390:              FMT CH(370)                   /* REQMASTR   */
                     FMT CH(950)                   /* PERMASTR   */
                     FMT CH(250)                   /* PERFRNGE   */
                     FMT CH(800)                   /* INSMASTR   */
                     FMT CH(500)                   /* EMPHSTRY   */
              recdate$(1%) = str(l$(),01%, 6%)
              call "DATEFMT" (recdate$(1%))
              recdate$(2%) = str(l$(),24%, 6%)
              call "DATEFMT" (recdate$(2%))
           if line% > 55% then gosub L22980         /* THE HEADER */
           print using  L22950   ,  str(l$(), 7, 1),                      ~
                                   str(l$(),30, 5),                      ~
                                   recdate$(1%),                         ~
                                   str(l$(), 8,16),                      ~
                                   recdate$(2%),                         ~
                                   str(l$(),35, 6),                      ~
                                   str(l$(),41, 4),                      ~
                                   str(l$(),45,16),                      ~
                                   str(l$(),101,12)
           line% = line% + 1%
           goto L16320

L16570:    call "SHOSTAT" ("PRINTING A LIST OF REQUISITIONS BY STATUS    ~
        ~             ")
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           k% =  3%                     /* SELECT THE KEY  TO PLOW ON  */
L16610:    call "PLOWALTS" (#6 , plowkey$, k%, 0%, f1%( 6))
                     if f1%( 6) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #6 , using  L16680   , str(l$(),1)
                     FMT CH(100)                   /* APLSKILL   */
                     FMT CH(100)                   /* REQSKILL   */
                     FMT CH(100)                   /* EMPSKILL   */
                     FMT CH(600)                   /* APLMASTR   */
L16680:              FMT CH(370)                   /* REQMASTR   */
                     FMT CH(950)                   /* PERMASTR   */
                     FMT CH(250)                   /* PERFRNGE   */
                     FMT CH(800)                   /* INSMASTR   */
                     FMT CH(500)                   /* EMPHSTRY   */
              recdate$(1%) = str(l$(),01%, 6%)
              call "DATEFMT" (recdate$(1%))
              recdate$(2%) = str(l$(),24%, 6%)
              call "DATEFMT" (recdate$(2%))
           if line% > 55% then gosub  L23160        /* THE HEADER */
           print using   L23130  ,  str(l$(), 7, 1),                      ~
                                   str(l$(),30, 5),                      ~
                                   recdate$(1%),                         ~
                                   str(l$(), 8,16),                      ~
                                   recdate$(2%),                         ~
                                   str(l$(),35, 6),                      ~
                                   str(l$(),41, 4),                      ~
                                   str(l$(),45,16),                      ~
                                   str(l$(),101,12)
           line% = line% + 1%
           goto L16610

L16860:    call "SHOSTAT" ("PRINTING A LIST OF REQUISITIONS BY DATE OF RE~
        ~QUISITION    ")
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           k% =  1%                     /* SELECT THE KEY  TO PLOW ON  */
L16900:    call "PLOWALTS" (#6 , plowkey$, k%, 0%, f1%( 6))
                     if f1%( 6) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #6 , using  L16970   , str(l$(),1)
                     FMT CH(100)                   /* APLSKILL   */
                     FMT CH(100)                   /* REQSKILL   */
                     FMT CH(100)                   /* EMPSKILL   */
                     FMT CH(600)                   /* APLMASTR   */
L16970:              FMT CH(370)                   /* REQMASTR   */
                     FMT CH(950)                   /* PERMASTR   */
                     FMT CH(250)                   /* PERFRNGE   */
                     FMT CH(800)                   /* INSMASTR   */
                     FMT CH(500)                   /* EMPHSTRY   */
              recdate$(1%) = str(l$(),01%, 6%)
              call "DATEFMT" (recdate$(1%))
              recdate$(2%) = str(l$(),24%, 6%)
              call "DATEFMT" (recdate$(2%))
           if line% > 55% then gosub  L23340        /* THE HEADER */
           print using   L23310  ,  str(l$(), 7, 1),                      ~
                                   str(l$(),30, 5),                      ~
                                   recdate$(1%),                         ~
                                   str(l$(), 8,16),                      ~
                                   recdate$(2%),                         ~
                                   str(l$(),35, 6),                      ~
                                   str(l$(),41, 4),                      ~
                                   str(l$(),45,16),                      ~
                                   str(l$(),101,12)
           line% = line% + 1%
           goto L16900

L17150:    call "SHOSTAT" ("PRINTING A LIST OF REQUISITIONS BY WHO MADE R~
        ~EQUEST       ")
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           k% =  2%                     /* SELECT THE KEY  TO PLOW ON  */
L17190:    call "PLOWALTS" (#6 , plowkey$, k%, 0%, f1%( 6))
                     if f1%( 6) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #6 , using  L17260   , str(l$(),1)
                     FMT CH(100)                   /* APLSKILL   */
                     FMT CH(100)                   /* REQSKILL   */
                     FMT CH(100)                   /* EMPSKILL   */
                     FMT CH(600)                   /* APLMASTR   */
L17260:              FMT CH(370)                   /* REQMASTR   */
                     FMT CH(950)                   /* PERMASTR   */
                     FMT CH(250)                   /* PERFRNGE   */
                     FMT CH(800)                   /* INSMASTR   */
                     FMT CH(500)                   /* EMPHSTRY   */
              recdate$(1%) = str(l$(),01%, 6%)
              call "DATEFMT" (recdate$(1%))
              recdate$(2%) = str(l$(),24%, 6%)
              call "DATEFMT" (recdate$(2%))
           if line% > 55% then gosub  L23520        /* THE HEADER */
           print using   L23490   , str(l$(), 7, 1),                      ~
                                   str(l$(),30, 5),                      ~
                                   recdate$(1%),                         ~
                                   str(l$(), 8,16),                      ~
                                   recdate$(2%),                         ~
                                   str(l$(),35, 6),                      ~
                                   str(l$(),41, 4),                      ~
                                   str(l$(),45,16),                      ~
                                   str(l$(),101,12)
           line% = line% + 1%
           goto L17190

L17440:    call "SHOSTAT" ("PRINTING A LIST OF REQUISITIONS BY DATE REQUI~
        ~RED          ")
           init (" ") l$()              /* GENERAL PURPOSE STRING      */
           k% =  4%                     /* SELECT THE KEY  TO PLOW ON  */
L17480:    call "PLOWALTS" (#6 , plowkey$, k%, 0%, f1%( 6))
                     if f1%( 6) <> 1% then goto L64000
                     if str(plowkey$,1%,b%) > str(lst$(fieldnr%),1%,b%)  ~
                     and lst$(fieldnr%) <> " " then L64000
           get #6 , using  L17550   , str(l$(),1)
                     FMT CH(100)                   /* APLSKILL   */
                     FMT CH(100)                   /* REQSKILL   */
                     FMT CH(100)                   /* EMPSKILL   */
                     FMT CH(600)                   /* APLMASTR   */
L17550:              FMT CH(370)                   /* REQMASTR   */
                     FMT CH(950)                   /* PERMASTR   */
                     FMT CH(250)                   /* PERFRNGE   */
                     FMT CH(800)                   /* INSMASTR   */
                     FMT CH(500)                   /* EMPHSTRY   */
              recdate$(1%) = str(l$(),01%, 6%)
              call "DATEFMT" (recdate$(1%))
              recdate$(2%) = str(l$(),24%, 6%)
              call "DATEFMT" (recdate$(2%))
           if line% > 55% then gosub  L23700        /* THE HEADER */
           print using  L23670    , str(l$(), 7, 1),                      ~
                                   str(l$(),30, 5),                      ~
                                   recdate$(1%),                         ~
                                   str(l$(), 8,16),                      ~
                                   recdate$(1%),                         ~
                                   str(l$(),35, 6),                      ~
                                   str(l$(),41, 4),                      ~
                                   str(l$(),45,16),                      ~
                                   str(l$(),101,12)
           line% = line% + 1%
           goto L17480

        REM *************************************************************~
           *         CONTROL HEADERS AND PRINT LINE FORMATS             *~
           *                                                            *~
            *************************************************************

        REM FOR THE 'PERMASTR' FILE
L20060: % REPORT OF EMPLOYEES BY #####################################   ~
        ~                                                          PAGE  #~
        ~##
L20070: %STATUS  NAME                       EMPLOYEE CODE  SS-NUMBER   ~
        ~G BIRTH       EEO MS NUM-DEP  CUR-JOB           DEPT  SHIFT   SR-D~
        ~ATE
L20080: % #   ############### ########## #  ############   ########### ~
        ~# ########## ###   #   ##     ################  ####    #     ####~
        ~####
L20110:    print page
           pge% = pge% + 1%
           hdrvariable$ = hdrvar$ & " AS OF " & date$
           print using L20060, hdrvariable$, pge%
           print
           print using L20070
           line% = 3%
           return

        REM FOR THE 'EMPHSTRY' FILE
L20190: % REPORT OF JOB TITLES HELD BY EMPLOYEES                         ~
        ~                REPORT DATE ########                      PAGE  #~
        ~##
L20220: %EMPLOYEE CODE    JOB TITLE HELD   FROM       TO                PAY PE~
        ~R               EEOC JOB CLASS    SUPERVISOR AT THE TIME

L20250: %################ ################ ########## ########## ########## ##~
        ~##############  ################  ##############################

L20280:    print page
           pge% = pge% + 1%
           print using L20190, date$, pge%
           print
           print using L20220
           line% = 3%
           return

L20360: REM FOR THE 'EMPHSTRY' FILE
L20370: % REPORT OF EEOC CLASS OF JOBS HELD BY EMPLOYEES                 ~
        ~                REPORT DATE ########                      PAGE  #~
        ~##
L20400: % EMPLOYEE CODE    JOB TITLE HELD    FROM      TO         PAY  PE~
        ~R               EEOC JOB CLASS     SUPERVISOR AT THE TIME

        % ################ ################  ######  ###### ########## ##~
        ~##############  ################   ##############################

           print page
           pge% = pge% + 1%
           print using L20370, date$, pge%
           print
           print using L20400
           line% = 3%
           return

L20540: REM FOR THE 'EMPHSTRY' FILE
L20550: % REPORT OF DATE OF NEXT REVIEW FOR EMPLOYEES                    ~
        ~                REPORT DATE ########                      PAGE  #~
        ~##
L20580: % EMPLOYEE CODE    JOB TITLE HELD    FROM      TO         PAY  PE~
        ~R               NEXT REVIEW DATE   SUPERVISOR

        % ################ ################  ######  ###### ########## ##~
        ~##############      ######         ##############################

           print page
           pge% = pge% + 1%
           print using L20550, date$, pge%
           print
           print using L20580
           line% = 3%
           return

        REM FOR THE 'EMPHSTRY' FILE
L20730: % REPORT OF INSURANCE IN FORCE FOR EMPLOYEES                     ~
        ~                REPORT DATE ########                      PAGE  #~
        ~##
L20760: % INSURANCE PROGRAM   EMPLOYEE CODE   ELIGDATE  STARTDATE  COVERA~
        ~GE AMOUNT   WHO IS COVERED

L20790: % ################    ############    ########   ########     ###~
        ~#######     ################

L20820:    print page
           pge% = pge% + 1%
           print using L20730, date$, pge%
           print
           print using L20760
           line% = 3%
           return

        REM FOR THE 'FRGMASTR' FILE
L20910: % REPORT OF FRINGE BENEFITS GRANTED TO EMPLOYEES                 ~
        ~                REPORT DATE ########                      PAGE  #~
        ~##
L20940: % BENEFIT             EMPLOYEE CODE   ELIGDATE  STARTDATE  AMT PD~
        ~ BY EMPL    MAX TO BE PAID BY EMPL

L20970: % ################    ############     ######     ######      ###~
        ~#######     ##########             ##############################

L21000:    print page
           pge% = pge% + 1%
           print using L20910, date$, pge%
           print
           print using L20940
           line% = 3%
           return

        REM FOR THE 'EMPSKILL' FILE
L21090: % REPORT OF EMPLOYEE SKILLS                                      ~
        ~                REPORT DATE ########                      PAGE  #~
        ~##
L21120: % SKILL               EMPLOYEE CODE   PROFICIENCY


L21150: % ################    ############        ####      #############~
        ~##############################

L21180:    print page
           pge% = pge% + 1%
           print using L21090, date$, pge%
           print
           print using L21120
           line% = 3%
           return

        REM FOR THE 'APLSKILL' FILE
L21270: % REPORT OF APPLICANT SKILLS                                     ~
        ~                REPORT DATE ########                      PAGE  #~
        ~##
L21300: % SKILL               APLIC SS-NUM    PROFICIENCY


L21330: % ################    ###########         ####      #############~
        ~##############################

L21360:    print page
           pge% = pge% + 1%
           print using L21270, date$, pge%
           print
           print using L21300
           line% = 3%
           return

        REM FOR THE 'REQSKILL' FILE
L21450: % REPORT OF SKILLS NEEDED ON REQUISITIONS                        ~
        ~                REPORT DATE ########                      PAGE  #~
        ~##
L21480: % SKILL               REQUISITION     PROFICIENCY


L21510: % ################      #####             ####      #############~
        ~##############################

L21540:    print page
           pge% = pge% + 1%
           print using L21450, date$, pge%
           print
           print using L21480
           line% = 3%
           return

        REM FOR THE 'APLMASTR' FILE
L21630: % REPORT OF APPLICANTS FOR REQUISITIONS                          ~
        ~                REPORT DATE ########                      PAGE  #~
        ~##
L21660: % NAME-LAST       FIRST      M    SS-NUMBER  LAST INTERVIEW ON/BY~
        ~          FOR JOB TITLE     REQNR  EMP-CODE ASSIGNED (IF ANY)

L21690: % ############### ########## #  ###########  ######## /  ########~
        ~########  ################  #####  ############

L21720:    print page
           pge% = pge% + 1%
           print using L21630, date$, pge%
           print
           print using L21660
           line% = 3%
           return

        REM FOR THE 'APLMASTR' FILE
L21810: % REPORT OF EMPLOYEE CODES ASSIGNED TO APPLICANTS                ~
        ~                REPORT DATE ########                      PAGE  #~
        ~##
L21840: % NAME-LAST       FIRST      M    SS-NUMBER  LAST INTERVIEW ON/BY~
        ~          FOR JOB TITLE     REQNR  EMP-CODE ASSIGNED (IF ANY)

L21870: % ############### ########## #  ###########  ######## /  ########~
        ~########  ################  #####  ############

L21900:    print page
           pge% = pge% + 1%
           print using L21810, date$, pge%
           print
           print using L21840
           line% = 3%
           return

        REM FOR THE 'APLMASTR' FILE
L21990: % REPORT OF APPLICANTS BY THEIR SOCIAL SECURITY NUMBERS          ~
        ~                REPORT DATE ########                      PAGE  #~
        ~##
L22020: % NAME-LAST       FIRST      M    SS-NUMBER  LAST INTERVIEW ON/BY~
        ~          FOR JOB TITLE     REQNR  EMP-CODE ASSIGNED (IF ANY)

L22050: % ############### ########## #  ###########  ######## /  ########~
        ~########  ################  #####  ############

L22080:    print page
           pge% = pge% + 1%
           print using L21990, date$, pge%
           print
           print using L22020
           line% = 3%
           return

        REM FOR THE 'APLMASTR' FILE
L22170: % REPORT OF APPLICANTS BY THEIR NAMES                            ~
        ~                REPORT DATE ########                      PAGE  #~
        ~##
L22200: % NAME-LAST       FIRST      M    SS-NUMBER  LAST INTERVIEW ON/BY~
        ~          FOR JOB TITLE     REQNR  EMP-CODE ASSIGNED (IF ANY)

L22230: % ############### ########## #  ###########  ######## /  ########~
        ~########  ################  #####  ############

L22260:    print page
           pge% = pge% + 1%
           print using L22170, date$, pge%
           print
           print using L22200
           line% = 3%
           return

        REM FOR THE 'APLMASTR' FILE
L22350: % REPORT OF APPLICANTS BY THE JOB THEY APPLIED FOR               ~
        ~                REPORT DATE ########                      PAGE  #~
        ~##
L22380: % NAME-LAST       FIRST      M    SS-NUMBER  LAST INTERVIEW ON/BY~
        ~          FOR JOB TITLE     REQNR  EMP-CODE ASSIGNED (IF ANY)

L22410: % ############### ########## #  ###########  ######## /  ########~
        ~########  ################  #####  ############

L22440:    print page
           pge% = pge% + 1%
           print using L22350, date$, pge%
           print
           print using L22380
           line% = 3%
           return

        REM FOR THE 'APLMASTR' FILE
L22530: % REPORT OF APPLICANTS BY THE DATE LAST INTERVIEWED              ~
        ~                REPORT DATE ########                      PAGE  #~
        ~##
L22560: % NAME-LAST       FIRST      M    SS-NUMBER  LAST INTERVIEW ON/BY~
        ~          FOR JOB TITLE     REQNR  EMP-CODE ASSIGNED (IF ANY)

L22590: % ############### ########## #  ###########  ######## /  ########~
        ~########  ################  #####  ############

L22620:    print page
           pge% = pge% + 1%
           print using L22530, date$, pge%
           print
           print using L22560
           line% = 3%
           return

        REM FOR THE 'APLMASTR' FILE
L22710: % REPORT OF APPLICANTS BY WHO LAST INTERVIEWED THEM              ~
        ~                REPORT DATE ########                      PAGE  #~
        ~##
L22740: % NAME-LAST       FIRST      M    SS-NUMBER  LAST INTERVIEW ON/BY~
        ~          FOR JOB TITLE     REQNR  EMP-CODE ASSIGNED (IF ANY)

L22770: % ############### ########## #  ###########  ######## /  ########~
        ~########  ################  #####  ############

L22800:    print page
           pge% = pge% + 1%
           print using L22710, date$, pge%
           print
           print using L22740
           line% = 3%
           return

        REM FOR THE 'REQMASTR' FILE
L22890: % REPORT OF REQUISITIONS BY REQUISITION NUMBER                   ~
        ~                REPORT DATE ########                      PAGE  #~
        ~##
L22920: % STATUS / REQNR   DATE REQD  WHO IS REQ FOR?   REQ-DATE  RATE-RA~
        ~NGE  FOR-DEPT  FOR-JOB-TITLE    EMPLOYEE CODE IF FILLED

L22950: %   #    / #####   ########   ################  ########     ####~
        ~##    ####     ################  ############

L22980:    print page
           pge% = pge% + 1%
           print using L22890, date$, pge%
           print
           print using L22920
           line% = 3%
           return

        REM FOR THE 'REQMASTR' FILE
L23070: % REPORT OF REQUISITIONS BY STATUS                               ~
        ~                REPORT DATE ########                      PAGE  #~
        ~##
L23100: % STATUS / REQNR   DATE REQD  WHO IS REQ FOR?   REQ-DATE  RATE-RA~
        ~NGE  FOR-DEPT  FOR-JOB-TITLE    EMPLOYEE CODE IF FILLED

L23130: %   #    / #####   ########   ################  ########     ####~
        ~##    ####     ################  ############

L23160:    print page
           pge% = pge% + 1%
           print using L23070, date$, pge%
           print
           print using L23100
           line% = 3%
           return

        REM FOR THE 'REQMASTR' FILE
L23250: % REPORT OF REQUISITIONS BY REQUISITION DATE                     ~
        ~                REPORT DATE ########                      PAGE  #~
        ~##
L23280: % STATUS / REQNR   DATE REQD  WHO IS REQ FOR?   REQ-DATE  RATE-RA~
        ~NGE  FOR-DEPT  FOR-JOB-TITLE    EMPLOYEE CODE IF FILLED

L23310: %   #    / #####   ########   ################  ########     ####~
        ~##    ####     ################  ############

L23340:    print page
           pge% = pge% + 1%
           print using L23250, date$, pge%
           print
           print using L23280
           line% = 3%
           return

        REM FOR THE 'REQMASTR' FILE
L23430: % REPORT OF REQUISITIONS BY WHO MADE THE REQUEST                 ~
        ~                REPORT DATE ########                      PAGE  #~
        ~##
L23460: % STATUS / REQNR   DATE REQD  WHO IS REQ FOR?   REQ-DATE  RATE-RA~
        ~NGE  FOR-DEPT  FOR-JOB-TITLE    EMPLOYEE CODE IF FILLED

L23490: %   #    / #####   ########   ################  ########     ####~
        ~##    ####     ################  ############

L23520:    print page
           pge% = pge% + 1%
           print using L23430, date$, pge%
           print
           print using L23460
           line% = 3%
           return

        REM FOR THE 'REQMASTR' FILE
L23610: % REPORT OF REQUISITIONS BY DATE REQUIRED                        ~
        ~                REPORT DATE ########                      PAGE  #~
        ~##
L23640: % STATUS / REQNR   DATE REQD  WHO IS REQ FOR?   REQ-DATE  RATE-RA~
        ~NGE  FOR-DEPT  FOR-JOB-TITLE    EMPLOYEE CODE IF FILLED

L23670: %   #    / #####   ########   ################  ########     ####~
        ~##    ####     ################  ############

L23700:    print page
           pge% = pge% + 1%
           print using L23610, date$, pge%
           print
           print using L23640
           line% = 3%
           return

L64000:    close printer
           goto L06060

L65000: REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
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
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            call "SHOSTAT" ("ONE MOMENT PLEASE")

            for u3% = 1 to 64
                if f2%(u3%) = 0 then close # u3%
                next u3%
            end
