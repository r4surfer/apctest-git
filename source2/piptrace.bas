        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *                    P I P T R A C E                        *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPTRACE - THIS SUBROUTINE CAN CALL ITSELF TO NEST INTO   *~
            *            TRACING THE PIP DATA FOR A PARTICULAR JOB      *~
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
            * 06/14/84 ! ORIGINAL                                 ! JRW *~
            * 06/09/87 ! HNYMASTR, JBMASTR2 record length mods    ! JIM *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim reviewpart$25

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.00.00 09/08/87 Standard costs to 12            "
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
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #02 ! PIPIN    ! Planned inventory additions detail  feed *~
            * #03 ! PIPOUT   ! Planned inventory use detail rec  feeds  *~
            * #04 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #05 ! HNYMASTR ! Inventory Master File                    *~
            * #07 ! JBMASTR2 ! Production job master file               *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =    1, keylen =  20                      ~

            select #02, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

            select #03, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   64,                                  ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select #04, "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26          ~

            select #05, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

            select #07, "JBMASTR2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1300,                                 ~
                        keypos =    1, keylen =   8                      ~

           select #08, "DEMMASTR", varc, indexed,                        ~
                     recsize = 123 , keypos = 2   , keylen = 27,         ~
                     alt key 1, keypos =10, keylen = 19,                 ~
                         key 2, keypos = 1, keylen = 28

            call "SHOSTAT" ("Opening files; one moment please.")

            call "OPENFILE" (#01, "SHARE", f2%(01), rslt$(01), axd$(01))
            call "OPENFILE" (#02, "SHARE", f2%(02), rslt$(02), axd$(02))
            call "OPENFILE" (#03, "SHARE", f2%(03), rslt$(03), axd$(03))
            call "OPENFILE" (#04, "SHARE", f2%(04), rslt$(04), axd$(04))
            call "OPENFILE" (#05, "SHARE", f2%(05), rslt$(05), axd$(05))
            call "OPENFILE" (#07, "SHARE", f2%(07), rslt$(07), axd$(07))
            call "OPENFILE" (#08, "SHARE", f2%(08), rslt$(08), axd$(08))

        REM HERES THE CALL

            call "PIPTSUB" (reviewpart$, #1, #2, #3, #4, #5, #7, #8)

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
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

            call "SHOSTAT" ("One moment please")
            end
