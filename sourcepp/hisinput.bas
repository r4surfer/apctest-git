        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  IIIII   SSS   IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  H   H    I    S        I    NN  N  P   P  U   U    T     *~
            *  HHHHH    I     SSS     I    N N N  PPPP   U   U    T     *~
            *  H   H    I        S    I    N  NN  P      U   U    T     *~
            *  H   H  IIIII   SSS   IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HISINPUT - MANAGEMENT PROGRAM TO RECORD THE HISTORY OF    *~
            *            AN EMPLOYEES JOBS/PAY GRADES HELD WITH THE     *~
            *            COMPANY.  HAS A SUBROUTINE VERSION CALLED      *~
            *            'HSTRYSUB'.  INPUT/EDIT MODED ACCESSED FROM    *~
            *            'LINE SUMMARY' SCREEN.                         *~
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
            * 11/29/83 ! ORIGINAL                                 ! GLW *~
            * 02/23/88 ! This is now a driver that calls HSTRSUB  ! LKM *~
            * 10/08/92 ! Added Call to PRLEXTSB for SFC/PRL       ! JBK *~
            *          !  Separation Project.                     !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            errormsg$79                  /* ERROR MESSAGE              */

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.01 11/05/92 Payroll Switch & Other          "
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
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  12,                     ~
                        alt key  1, keypos =  321, keylen =   1, dup

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

        call "SHOSTAT" ("LINKING TO DATA BASE FOR EMPLOYMENT HISTORY")

           for i% = 1% to 11%
            call "OPENFILE" (#i%, "SHARE", f2%(i%), rslt$(i%), axd$(i%))
           if f2%(i%) = 0% then goto L07950
            call "OPENFILE" (#i%, "OUTPT", f2%(i%), rslt$(i%), axd$(i%))
                     close #i%
            call "OPENFILE" (#i%, "SHARE", f2%(i%), rslt$(i%), axd$(i%))
L07950:    next i%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

L09170: init(" ")                                                        ~
            errormsg$,                   /* ERROR MESSAGE              */~
            employee$                    /* EMPLOYEE CODE              */

           REM REMOVE THIS SECTION WHEN USED AS A SUBROUTINE
L09455: accept                                                           ~
               at (01,02),                                               ~
        "                          EMPLOYMENT HISTORY MANAGEMENT         ~
        ~             ",                                                  ~
               at (03,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
               at (04,03),                                               ~
        "!                                                               ~
        ~            !",                                                  ~
               at (05,03),                                               ~
        "!                                                               ~
        ~            !",                                                  ~
               at (06,03),                                               ~
        "!  THIS PROGRAM ALLOWS YOU TO MANAGE JUST THE  WORK HISTORY   FO~
        ~R YOUR      !",                                                  ~
               at (06,79),                                               ~
        "!",                                                             ~
               at (07,03),                                               ~
        "!  EMPLOYEES.  PLEASE ENTER THE CODE FOR THE EMPLOYEE DESIRED   ~
        ~            !",                                                  ~
               at (08,03),                                               ~
        "!",                                                             ~
               at (08,25), fac(hex(81)), employee$, ch(12),              ~
               at (08,79),                                               ~
        "!",                                                             ~
               at (09,03),                                               ~
        "!",                                                             ~
               at (09,05), fac(hex(94)), errormsg$, ch(70),              ~
               at (09,79),                                               ~
        "!",                                                             ~
               at (10,03),                                               ~
        "!",                                                             ~
               at (10,79),                                               ~
        "!",                                                             ~
               at (11,03),                                               ~
        "!  IF THE EMPLOYEE IS NOT ON FILE PLEASE EXIT AND USE THE NORMAL~
        ~ EMPLOYEE   !",                                                  ~
               at (12,03),                                               ~
        "!  INFORMATION MANAGEMENT PROGRAM TO INPUT ALL OF THE DATA REQUI~
        ~RED         !",                                                  ~
               at (13,03),                                               ~
        "!",                                                             ~
               at (13,79),                                               ~
        "!",                                                             ~
               at (14,03),                                               ~
        "!",                                                             ~
               at (14,79),                                               ~
        "!",                                                             ~
               at (15,03),                                               ~
        "!",                                                             ~
               at (15,79),                                               ~
        "!",                                                             ~
               at (16,03),                                               ~
        "!",                                                             ~
               at (16,79),                                               ~
        "!",                                                             ~
               at (17,03),                                               ~
        "!",                                                             ~
               at (17,79),                                               ~
        "!",                                                             ~
               at (18,03),                                               ~
        "!",                                                             ~
               at (18,79),                                               ~
        "!",                                                             ~
               at (19,03),                                               ~
        "!",                                                             ~
               at (19,79),                                               ~
        "!",                                                             ~
               at (20,03),                                               ~
        "!",                                                             ~
               at (20,79),                                               ~
        "!",                                                             ~
               at (21,03),                                               ~
        "!",                                                             ~
               at (21,79),                                               ~
        "!",                                                             ~
               at (22,03),                                               ~
        "+---------------------------------------------------------------~
        ~------------+",                                                  ~
               at (23,03),                                               ~
        "(ENTER) TO GATHER THE WORK HISTORY FOR THIS EMPLOYEE            ~
        ~             ",                                                  ~
               at (24,03),                                               ~
        "                               (15)PRINT SCREEN       (16)EXIT F~
        ~ROM PROGRAM  ",                                                  ~
           keys(hex(000f10)), key(hh%)
           if hh% = 16% then goto L65000
           if hh% <> 15% then goto L09895
                     call "PRNTSCRN"
                     goto L09455
L09895:    if hh% <> 0% then goto L09455
                     call "READ100" (#8, employee$, f1%(8))
                     if f1%(8) = 1% then goto L09965
                        errormsg$ = "NO SUCH EMPLOYEE ON FILE"
                        goto L09455

           REM REMOVE THE SECTION ABOVE WHEN USED AS A SUBROUTINE

L09965:    call "HSTRYSUB" (#1,#2,#3,#4,#5,#6,#7,#8,#9,#10,#11,employee$)

           goto L09170


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
