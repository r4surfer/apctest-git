        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   TTTTT   CCC    CCC    OOO    SSS   TTTTT   SSS    *~
            *  S        T    C   C  C   C  O   O  S        T    S       *~
            *   SSS     T    C      C      O   O   SSS     T     SSS    *~
            *      S    T    C   C  C   C  O   O      S    T        S   *~
            *   SSS     T     CCC    CCC    OOO    SSS     T     SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCCOSTS - Returns Standard Costs for the Part and Cost   *~
            *            Set requested.                                 *~
            *----------------------------------------------------------Q*~
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
            * 03/26/87 ! Original                                 ! ERN *~
            * 05/12/88 ! Return Specific Bucket for Pur. Costs    ! KAB *~
            * 04/23/93 ! PRR 10638 - Issue warning to user if last! JBK *~
            *          !  cost set implementation failed.         !     *~
            * 03/29/94 ! Branch around implementation warning for ! JBK *~
            *          !  STCIMPLE when actually implementing.    !     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "STCCOSTS"   (part$,         /* Part to find costs for     */~
                          a_set$,        /* Cost Set (blank = current) */~
                          #2,            /* SYSFILE2 channel           */~
                          a_rqst%,       /* Request Flag (see below)   */~
                          cost,          /* Total Standard Cost        */~
                          a_sum(),       /* Folded Costs               */~
                          a_bom(),       /* Bill-of-Material Costs     */~
                          a_rte(),       /* Route Costs                */~
                          a_dtl())       /* Misc Cost Details          */

*        The values of PART$ and SET$ are not modified by the subroutine.
*
*        RQST% is 1%, 2%, or 3% and specifies the configuration of costs
*          that are to be returned to the caller (only the desired costs
*          variables need be passed):
*              1% - Return total cost (COST) only
*              2% - Return total cost (COST) and Folded Costs (A_SUM())
*              3% - Return all costs variables.
*        A_RQST% > 10 Opens Files in INPUT Mode.
*        A_RQST% is returned (if variable) as:
*              1) > 0% Part Default Purchases Bucket
*              2) = 0% Used Cost Set Default Purchases Bucket
*              3) < 0% Prorate Costs

        dim                                                              ~
            a_bom(12),                   /* Bill of Material Costs     */~
            a_dtl(12),                   /* Misc Cost Details          */~
            a_rte(12),                   /* This Level Costs           */~
            a_set$8,                     /* Requested Set              */~
            a_sum(12),                   /* Folded Costs               */~
            bom(12),                     /* BOM Costs                  */~
            dtl(12),                     /* Misc Cost Details          */~
            err$79,                      /* File open error flag       */~
            last_a_set$8,                /* Last Requested Set open    */~
            map%(12), map$8,             /* Mapping and Map ID         */~
            mode$5,                      /* STCFOPEN Mode to Open      */~
            part$25,                     /* Part Code                  */~
            pdef$2,                      /* Part Default Cost Bucket   */~
            pgm_name$8,                  /* Calling Program Name       */~
            readkey$99,                  /* Key for Miscellaneous Reads*/~
            rte(12),                     /* Route Costs                */~
            sum(12)                      /* Folded Costs               */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.01 07/28/94 CMS Patch Release R6.03.01      "
        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 2 ! SYSFILE2 ! Caelus Management System Information     *~
            * #10 ! STCHNY   ! Standard Cost Set- Inventory Standards   *~
            * #11 ! STCMAPNG ! Standard Cost Set Mappings               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #10, "STCHNY",                                        ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =  1,   keylen = 25

            select #11, "STCMAPNG",                                      ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =  1,   keylen = 8

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

*        Initialize return variables
            rqst% = mod(a_rqst%, 10%)
            cost = 0
            mat sum = zer : mat bom = zer : mat rte = zer : mat dtl = zer

            if rqst% < 2% then L09160
                mat a_sum = zer
                if rqst% < 3% then L09160
                     mat a_bom = zer : mat a_rte = zer : mat a_dtl = zer

L09160
*        Open the files for the set requested
            if alreadybeenherebefore% = 1% then L09200
                call "EXTRACT" addr("CF", pgm_name$)
                last_a_set$ = hex(ff)
                readkey$ = "STCIMPLE.CONTROL"  /* Check Implementation */
                call "READ100" (#2, readkey$, imple_on%)
                alreadybeenherebefore% = 1%
L09200:     if a_set$ = last_a_set$ then L09250
                last_a_set$ = a_set$
            if a_rqst% < 10% then mode$ = "SxxxSx" else mode$ = "RxxxRx"
                call "STCFOPEN" (a_set$, mode$, #2, err$,                ~
                                 #10, #10, #10, #10, #11, #11)
L09250:     a_rqst% = -1%              /* Set for Bad                */
            if err$ <> " " then end    /* Won't find nuthin' anyhow  */

*        If Last cost set implementation did not complete, issue an
*        ASKUSER warning of the situation
            if imple_on% <> 1% then L10000
            if pgm_name$ = "STCIMPLE" then L10000
                call "ZASKUSER" (0%, "COST SET IMPLEMENTATTION",         ~
                   "Your last implementation did not complete.",         ~
                   "You must rectify this as soon as possible.",         ~
                   "Press RETURN to continue....")

L10000: REM *************************************************************~
            *             S U B R O U T I N E   L O G I C               *~
            *-----------------------------------------------------------*~
            * Get the costs requested.                                  *~
            *************************************************************

            call "READ100" (#10, part$, f1%)
            if f1% = 0% then end

            get #10 using L10100, map$, cost, bom(), rte(), dtl(), pdef$
L10100:         FMT POS(26), CH(8), POS(52), 37*PD(14,4), POS(362), CH(2)

            if pdef$ = "D" then a_rqst% = 0%
            convert pdef$ to temp%, data goto L10120
            if temp% > 0% and temp% < 12% then a_rqst% = temp%

L10120:     if rqst% < 2% then end

                mat map% = zer
                call "READ100" (#11, map$, f1%)
                if f1% = 1% then get #11 using L10170, map%()
L10170:              FMT POS(39), 12*BI(1)

                mat sum = rte + dtl
                for b% = 1% to 12%
                     if map%(b%) = 0% then map%(b%) = b%
                     sum(map%(b%)) = sum(map%(b%)) + bom(b%)
                next b%
                mat a_sum = sum

                if rqst% < 3% then end

                     mat a_bom = bom
                     mat a_rte = rte
                     mat a_dtl = dtl
                     end


