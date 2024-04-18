        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   GGG   EEEEE  TTTTT  EEEEE  M   M  PPPP   L              *~
            *  G      E        T    E      MM MM  P   P  L              *~
            *  G GGG  EEEE     T    EEE    M M M  PPPP   L              *~
            *  G   G  E        T    E      M   M  P      L              *~
            *   GGG   EEEEE    T    EEEEE  M   M  P      LLLLL          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GETEMPL  -  Specialized GETCODE/PLOWCODE for Finding and/ *~
            *             or verifying Employee codes.  Currently acts  *~
            *             only as a driver to PLOWCODE.                 *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1984, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/18/84 ! ORIGINAL (SEMI-CONED FROM DESCRIBE)      ! HES *~
            * 07/03/84 ! CLONED FOR USE ON PERMASTER (PAYROLL)    ! HES *~
            *          ! MUCH CODE WAS LEFT THOUGH NOT NEEDED.    ! HES *~
            * 03/16/87 ! Removed obsolete subroutines.            ! ERN *~
            * 07/15/87 ! Rewrote to call PLOWCODE.                ! HES *~
            *************************************************************

            sub "GETEMPL" (#1,           /* PERMASTR File              */~
                                                                         ~
                          key$,          /* In:  Employee Code to Find */~
                                         /* Out: Employee Selected or  */~
                                         /* unchanged if not found/no  */~
                                         /* selection made.            */~
                                                                         ~
                          descr$,        /* Disregarded Coming In.     */~
                                         /* Selected Record  Descript  */~
                                         /* Going Out.                 */~
                                                                         ~
                          flag%,         /* Non Zero Causes Parenthesis*/~
                                         /* To Be Put Around Returned  */~
                                         /* Description                */~
                                                                         ~
                          f1%)           /* Return Status For Record.  */~
                                         /* 1 = Record Found/Selected  */~
                                         /* 0 = Rec Not On File.       */~

            dim code$1,                  /* Employee Status Code       */~
                descr$50,                /* Returned Description       */~
                descr_map(12),           /* PLOWCODE Argument          */~
                header$(2)80,            /* PLOWCODE Argument          */~
                incl_excl(1),            /* PLOWCODE Argument          */~
                incl_excl$(1)1,          /* PLOWCODE Argument          */~
                key$50,                  /* Read Key                   */~
                status$10,               /* Status Message             */~
                work$50                  /* Work Area                  */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L00650
            cms2v$ = "05.00.00 09/08/87 Standard cost to 12             "
L00650: REM *************************************************************

            select #2, "WORKFILE", indexed, recsize=12, keypos=1,keylen=1

        REM *************************************************************~
            *                  I N I T I A L I Z A T I O N              *~
            *************************************************************
            if f2% = 1% then main
            descr_map(01) = 002.15  :  descr_map(02) = 01  /* Last Name*/
            descr_map(03) = 017.10  :  descr_map(04) = 17  /* 1st Name */
            descr_map(05) = 027.01  :  descr_map(06) = 28  /* Mid Init */
            descr_map(07) = 369.04  :  descr_map(08) = 31  /* Dept     */
            descr_map(09) = 337.16  :  descr_map(10) = 36  /* Job Title*/
            descr_map(11) = -02.10  :  descr_map(12) = 53  /* Status   */
            header$(1) = "  Employee Code" : str(header$(1),18%) =       ~
                         "Last Name       First     Mid Dept Job Title"
            str(header$(1),70%) = "Cur Status"
               call "WORKOPEN" (#2, "IO   ", 10%, f2%)
               f2% = 1% : restore
L00840:        read code$, status$
               if status$ = " " then main
               write #2, code$, status$
               goto L00840

               data  " ", "UNDEFINED"
               data  "C", "CURRENT   "
               data  "N", "TERMINATED"
               data  "P", "PREVIOUS "
               data  "L", "ON LEAVE "
               data  "M", "ON LEAVE "
               data  " ", "         "

        main
            call "PLOWCODE" (#1, key$, descr$, 9000%, 0.8, f1%,          ~
                            header$(), 0,-1, incl_excl(), incl_excl$(),  ~
                            "Y", " ", #2, descr_map())


            REM Now Put Parentheses Around This Thing And Exit
            if descr$ = " " then L01120
            work$ = str(descr$,,15)
            work$ = work$ & ", " & str(descr$,17,10)
            if str(descr$,28,1) = " " then L01090
                work$ = work$ & " " & str(descr$,28,1) & "."
L01090:     descr$ = work$
            if descr$<>" " and flag%<>0% then descr$ = "(" & descr$ & ")"
            if f1% = 0 then descr$ = " "
L01120:     end      /* Ah-Ba-Dee Ah-Ba-Dee Thats All Folks! */

