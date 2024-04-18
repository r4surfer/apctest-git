        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   GGG   L       CCC   M   M  BBBB    SSS   U   U  BBBB    *~
            *  G      L      C   C  MM MM  B   B  S      U   U  B   B   *~
            *  G GGG  L      C      M M M  BBBB    SSS   U   U  BBBB    *~
            *  G   G  L      C   C  M   M  B   B      S  U   U  B   B   *~
            *   GGG   LLLLL   CCC   M   M  BBBB    SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLCMBSUB - This subroutine will be used to combine the    *~
            *            data of like accounts in order to eliminate    *~
            *            superfluous general ledger postings.           *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1989  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/06/89 ! Original                                 ! LAB *~
            * 10/05/90 ! Added Transaction Type for GL Export     ! JDH *~
            * 01/15/91 ! No read on hold of SYSFILE2              ! JDH *~
            * 02/10/93 ! Fixed POS for Suspense Acct., Added been ! JBK *~
            *          !  here before variable to save reads,     !     *~
            *          !  Dimmed Variables, Added % to integers.  !     *~
            * 01/14/94 ! Catch all possible postings.             ! JDH *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "GLCMBSUB" (passedin_acct$(), passedin_dbcr(),               ~
                        passedin_type$(), #1%, cntr%)

        dim                                                              ~
            f1%(10),                     /* FILE DIMENSION             */~
            location$2,                  /* PLACE HOLDER               */~
            passedin_acct$(50)109,       /* PASSED IN ACCOUNT ARRAY    */~
            passedin_dbcr(50,2),         /* PASSED IN DB/CR ARRAY      */~
            passedin_type$(50)2,         /* PASSED IN Type  ARRAY      */~
            readkey$99,                  /* Miscellaneous Read Key     */~
            suspense$16,                 /* System Suspense Account    */~
            temp_dbcr(50,2),             /* TEMPORARY DB/CR ARRAY      */~
            temp_type$(50)2,             /* TEMPORARY Type  ARRAY      */~
            temp_acct$(50)109            /* TEMPORARY ACCOUNT ARRAY    */~

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.00 03/02/94 General Release  Purchase Jobs  "
        REM *************************************************************

        REM *************************************************************~
            *         I  N  I  T  I  A  L  I  Z  A  T  I  O  N          *~
            *************************************************************

            if been_here_before% > 0% then L01988

*        Get the System Suspense account
            readkey$ = "FISCAL DATES"
            call "READ100" (#1, readkey$, f1%(1%))
                 if f1%(1%) = 0% then L01985

            get #1, using L01984, suspense$
L01984:         FMT POS(417), CH(16)
L01985:     been_here_before% = 1%

L01988:     init(" ") temp_acct$()
            mat temp_dbcr = zer
            counter% = 1%

        REM *************************************************************~
            *          M  A  I  N     S  E  C  T  I  O  N               *~
            *************************************************************

            for x% = 1% to cntr%
                if str(passedin_acct$(x%),1%,9%) = " " and               ~
                   (passedin_dbcr(x%,1%) <>0 or passedin_dbcr(x%,2%) <>0)~
                     then str(passedin_acct$(x%),1%,9%) = suspense$
                if str(passedin_acct$(x%),1%,9%) = " " then              ~
                     L02390  /* Next One */
                if str(passedin_acct$(x%),1%,9%) = suspense$ then L02340
                     /* DON'T COMBINE SUSPENSE ACCOUNT RECORDS */

                search str(temp_acct$(),1%)=str(passedin_acct$(x%),1%,9%)~
                          to location$ step 109% /* FIND TEMP ACCT REC */
                if location$ = hex(0000) then L02340
                     tmp% = int(val(location$,2%)/109% + 1%) /* Which */
                     temp_dbcr(tmp%,1%) = temp_dbcr(tmp%,1%) +           ~
                          passedin_dbcr(x%,1%)
                     temp_dbcr(tmp%,2%) = temp_dbcr(tmp%,2%) +           ~
                          passedin_dbcr(x%,2%)
                     if str(temp_acct$(tmp%),74%,36%) =                  ~
                        str(passedin_acct$(x%),74%,36%) then L02390
                     if str(temp_acct$(tmp%),74%,4%) = " " then L02390
                     if abs(temp_dbcr(tmp%,1%) - temp_dbcr(tmp%,2%)) >   ~
                        abs(passedin_dbcr(x%,1%) - passedin_dbcr(x%,2%)) ~
                          then L02390
                     str(temp_acct$(tmp%),74%,36%) =                     ~
                          str(passedin_acct$(x%),74%,36%)
                     goto L02390

        REM     THIS IS FOR ADDING NEW ELEMENTS TO THE TEMPORARY ARRAY

L02340:         temp_acct$(counter%)   = passedin_acct$(x%)
                temp_dbcr(counter%,1%) = passedin_dbcr(x%,1%)
                temp_dbcr(counter%,2%) = passedin_dbcr(x%,2%)
                temp_type$(counter%)   = passedin_type$(x%)
                counter% = counter% + 1%
                goto L02390
L02390:     next x%

        REM PREPARE TO RETURN TO CALLING PROGRAM

            y% = 0%
            init(" ") passedin_acct$()
            mat passedin_dbcr = zer
            for x% = 1% to 50%
                if temp_dbcr(x%,1%) - temp_dbcr(x%,2%) = 0 then L02570
                if temp_dbcr(x%,2%) > temp_dbcr(x%,1%) then L02510
                    temp_dbcr(x%,1%) = temp_dbcr(x%,1%) - temp_dbcr(x%,2%)
                    temp_dbcr(x%,2%) = 0
                    goto L02530

L02510:             temp_dbcr(x%,2%) = temp_dbcr(x%,2%) - temp_dbcr(x%,1%)
                    temp_dbcr(x%,1%) = 0
L02530:         y% = y% + 1%
                passedin_acct$(y%)   = temp_acct$(x%)
                passedin_dbcr(y%,1%) = temp_dbcr(x%,1%)
                passedin_dbcr(y%,2%) = temp_dbcr(x%,2%)
                passedin_type$(y%)   = temp_type$(x%)
L02570:     next x%


            end

        REM ENDOFPROGRAMENDOFPROGRAMENDOFPROGRAMENDOFPROGRAMENDOFPROGRAM
