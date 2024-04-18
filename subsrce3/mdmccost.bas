        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  M   M  DDDD   M   M   CCC    CCC    OOO    SSS   TTTTT   *~
            *  MM MM  D   D  MM MM  C   C  C   C  O   O  S        T     *~
            *  M M M  D   D  M M M  C      C      O   O   SSS     T     *~
            *  M   M  D   D  M   M  C   C  C   C  O   O      S    T     *~
            *  M   M  DDDD   M   M   CCC    CCC    OOO    SSS     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * MDMCCOST - RETURNS THE MANAGEMENT DMC FOR THE PART AND    *~
            *            COST SET REQUESTED.                            *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/15/90 ! Original                                 ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "MDMCCOST"   (part$,         /* Part to find costs for     */~
                          a_set$,        /* Cost Set (blank = current) */~
                          #2,            /* SYSFILE2 channel           */~
                          mdmc)          /* Management DMC             */

*        The values of PART$ and SET$ are not modified by the subroutine.
*

        dim                                                              ~
            a_set$8,                     /* Requested Set              */~
            err$79,                      /* File open error flag       */~
            last_a_set$8,                /* Last Requested Set open    */~
            mode$5,                      /* STCFOPEN Mode to Open      */~
            part$25                      /* Part Code                  */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 2 ! SYSFILE2 ! Caelus Management System Information     *~
            * #10 ! STCHNY   ! Standard Cost Set- Inventory Standards   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #10, "STCHNY",                                        ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =  1,   keylen = 25

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

*        Initialize return variables
            mdmc = 0

*        Open the files for the set requested
            if alreadybeenherebefore% = 1% then L09200
                last_a_set$ = hex(ff)
                alreadybeenherebefore% = 1%
L09200:     if a_set$ = last_a_set$ then L09260
                last_a_set$ = a_set$
                mode$ = "Sxxxxx"
                call "STCFOPEN" (a_set$, mode$, #2, err$,                ~
                                 #10, #10, #10, #10, #10, #10)
L09260:     if err$ <> " " then end    /* Won't find nuthin' anyhow  */
        REM *************************************************************~
            *             S U B R O U T I N E   L O G I C               *~
            *-----------------------------------------------------------*~
            * Get the costs requested.                                  *~
            *************************************************************

            call "READ100" (#10, part$, f1%)
            if f1% = 0% then end

            get #10 using L10100, mdmc
L10100:         FMT POS(364), PD(14,4)

            if mdmc > 20000000000 then mdmc = -1.0  /* must have been */
                                                    /* HEX 20s there  */

            end

