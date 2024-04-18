        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   TTTTT   CCC   FFFFF   OOO   L      DDDD   N   N   *~
            *  S        T    C   C  F      O   O  L      D   D  NN  N   *~
            *   SSS     T    C      FFF    O   O  L      D   D  N N N   *~
            *      S    T    C   C  F      O   O  L      D   D  N  NN   *~
            *   SSS     T     CCC   F       OOO   LLLLL  DDDD   N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * STCFOLDN - Returns Folded Costs for Cost arrays and Cost  *~
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
            * 06/09/87 ! Original                                 ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "STCFOLDN"   (part$,         /* Part to find costs for     */~
                          a_set$,        /* Cost Set (blank = current) */~
                          #2,            /* SYSFILE2 channel           */~
                          bom(),         /* IN- BOM Costs              */~
                          rte(),         /*     Route Costs            */~
                          dtl(),         /*     Misc Costs             */~
                          cost,          /* Out-Total Cost             */~
                          sum())         /*     Folded Costs           */

*        The values of PART$ and A_SET$ are not modified.
*

        dim                                                              ~
            a_set$8,                     /* Requested Set              */~
            bom(12),                     /* BOM Costs                  */~
            dtl(12),                     /* Misc Cost Details          */~
            err$79,                      /* File open error flag       */~
            last_a_set$8,                /* Last Requested Set open    */~
            map%(12), map$8,             /* Mapping and Map ID         */~
            mode$5,                      /* STCFOPEN Mode to Open      */~
            part$25,                     /* Part Code                  */~
            rte(12),                     /* Route Costs                */~
            sum(12)                      /* Folded Costs               */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.00.00 09/08/87 Standard cost to 12             "
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
            cost = 0
            mat sum = zer

*        Open the files for the set requested
            if alreadybeenherebefore% = 1% then L09140
                last_a_set$ = hex(ff)
                alreadybeenherebefore% = 1%
L09140:     if a_set$ = last_a_set$ then L09190
                last_a_set$ = a_set$
            mode$ = "SxxxSx"
                call "STCFOPEN" (a_set$, mode$, #2, err$,                ~
                                 #10, #10, #10, #10, #11, #11)
L09190:     if err$ <> " " then L10110  /* Won't find nuthin' anyhow  */

        REM *************************************************************~
            *             S U B R O U T I N E   L O G I C               *~
            *-----------------------------------------------------------*~
            * Get the costs requested.                                  *~
            *************************************************************

            map$ = hex(ff)
            call "READ100" (#10, part$, f1%)
            if f1% = 1% then get #10 using L10090, map$
L10090:         FMT POS(26), CH(8)

L10110:     mat map% = zer
            call "READ100" (#11, map$, f1%)
            if f1% = 1% then get #11 using L10140, map%()
L10140:         FMT POS(39), 12*BI(1)

            mat sum = rte + dtl
            for b% = 1% to 12%
                if map%(b%) = 0% then map%(b%) = b%
                sum(map%(b%)) = sum(map%(b%)) + bom(b%)
            next b%

            for b% = 1% to 12% : cost = cost + sum(b%) : next b%

            end


