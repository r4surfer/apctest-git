        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  EEEEE  N   N   AAA   BBBB   L      FFFFF   SSS   BBBB    *~
            *  E      NN  N  A   A  B   B  L      F      S      B   B   *~
            *  EEEE   N N N  AAAAA  BBBB   L      FFF     SSS   BBBB    *~
            *  E      N  NN  A   A  B   B  L      F          S  B   B   *~
            *  EEEEE  N   N  A   A  BBBB   LLLLL  F       SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ENABLFSB - This routine performs most of the tasks        *~
            *            related to setting and resetting 'soft coded'  *~
            *            Default/Enable switches for Full Screen Input  *~
            *            and Edit Programs.                             *~
            *            NOTE- Calling program should ensure that the   *~
            *             user attempting to modify settings is either  *~
            *             a module or data base administrator.          *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of Caelus, Incorporated, Spokane, Wa,         *~
            * embodying substantial creative efforts and confidential   *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (C) 1990, an unpublished work by Caelus,        *~
            * Incorporated, Spokane, Wa.  All Rights Reserved.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/28/90 ! ORIGINAL (Semi-Cloned from ENABLSUB)     ! MJB *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "ENABLFSB"   (function$,     /* Task to execute-           */~
                                         /* 'INIT' or 'MODIFY'         */~
                          program$,      /* Calling Program            */~
                          inpfac$(),     /* Input mode FAC array       */~
                          modfac$())     /* Modify mode FAC array      */


*        Notes regarding program setting of enables
*
*         ! VALUE ! INPUT ! EDIT !
*         +-------+-------+------+
*         !   0   !   N   !   N  !   Add 10 to disallow user
*         !   1   !   N   !   Y  !   modification of setting
*         !   2   !   Y   !   Y  !
*         !   3   !   Y   !   N  !
*         +-------+-------+------+

        dim                                                              ~
            first$2,                     /* First time in subrtn?      */~
            function$6,                  /* Function to execute        */~
            inpfac$(5)30,                /* Input mode FAC array       */~
            modfac$(5)30,                /* Modify mode FAC array      */~
            program$8,                   /* Calling program name       */~
            readkey$25,                  /* Read key Variable          */~
            set%(255)                    /* DEF/ENA settings           */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.00 09/09/92 Cycle Counting & MPS Phase I    "
        REM *************************************************************
        if first$ = " " then                                             ~
            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* It is an intrinsic part of the                 */
                     /* file opening routines.                         */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! ENABLES  ! System Enables Settings                  *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #1,  "ENABLES",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                        keypos =     1, keylen =  20


        if f2%(1) = 0% then L09000
            call "OPENCHCK" (#1, 0%, f2%(1), 100%, rslt$(1))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

            first$ = "NO"

        REM *************************************************************~
            *        F U N C T I O N   D R I V E R S                    *~
            * --------------------------------------------------------- *~
            * Branch per function.                                      *~
            *************************************************************

            if function$ = "INIT"    then init_enables
            if function$ = "MODIFY"  then modify_enables
            goto L65000


        REM *************************************************************~
            * I N I T I A L I Z E    E N A B L E    S W I T C H E S     *~
            * --------------------------------------------------------- *~
            * This function over-rides the program defaults with the    *~
            * copy found in ENABLES.   If no record is found, it is     *~
            * created here. Checking is done to see if fields have been *~
            * added or deleted.                                         *~
            *************************************************************
        init_enables
            mat set% = con            /* Initialize SET%() to 1's */
            mat set% = (99%) * set%   /* Now make it 99's */
            readkey$ = "ENABLES:" & str(program$,,8)
            call "READ100" (#1, readkey$, f1%(1))
            if f1%(1) = 0% then L11190
                get #1 using L11150, set%()
L11150:              FMT XX(20), 255*BI(1)
                gosub decode_set   /* Decode the SET%() array */
            goto L65000

L11190: modify_enables
            gosub encode_set
            gosub write_enables
            goto L65000

        REM *************************************************************~
            *     D E C O D E   S E T %   T O   F A C   A R R A Y S     *~
            * --------------------------------------------------------- *~
            * This function decodes the SET%() string from the ENABLES  *~
            * file into the FAC arrays for the calling program.         *~
            *************************************************************
        decode_set
            for j% = 1% to 5%
                for i% = 30%*j%-29% to 30%*j%
                    if set%(i%) <> 0% then L12130
                        str(inpfac$(j%),i%-(30%*(j%-1%)),1%) = hex(8c)
                        str(modfac$(j%),i%-(30%*(j%-1%)),1%) = hex(8c)
L12130:             if set%(i%)  = 1% then                               ~
                        str(inpfac$(j%),i%-(30%*(j%-1%)),1%) = hex(8c)
                    if set%(i%)  = 3% then                               ~
                        str(modfac$(j%),i%-(30%*(j%-1%)),1%) = hex(8c)
                next i%
            next j%
            return

        REM *************************************************************~
            *  E N C O D E   S E T %   F R O M   F A C   A R R A Y S    *~
            * --------------------------------------------------------- *~
            * This function encodes the FAC arrays from the calling     *~
            * program into the SET%() string for the ENABLES file.      *~
            *************************************************************
        encode_set
            j% = 1%
            set%(1%) = 13%
            for i% = 2% to 150%
                if i% >  30% then j% = 2%
                if i% >  60% then j% = 3%
                if i% >  90% then j% = 4%
                if i% > 120% then j% = 5%
                k% = i% - (30% * (j% - 1%))
                set%(i%) = 2%
                if str(inpfac$(j%), k%, 1%) = hex(bc) then set%(i%) = 99%
                if str(inpfac$(j%), k%, 1%) = hex(bc) then L13250
                if str(inpfac$(j%), k%, 1%) = hex(8c)                    ~
                                    then set%(i%) = set%(i%) - 1%
                if str(modfac$(j%), k%, 1%) = hex(8c) and set%(i%) = 1%  ~
                                    then set%(i%) = set%(i%) - 1%
                if str(modfac$(j%), k%, 1%) = hex(8c) and set%(i%) = 2%  ~
                                    then set%(i%) = set%(i%) + 1%
L13250:     next i%
            return




        REM *************************************************************~
            * W R I T E  -  E N A B L E S                               *~
            * --------------------------------------------------------- *~
            * Write/Rewrite Default/Enable array to file ENABLES        *~
            *************************************************************
        write_enables
            readkey$ = "ENABLES:" & str(program$,,8)
            call "READ101" (#1, readkey$, f1%(1))
            put #1 using L18100, readkey$, set%(), " "
L18100:         FMT CH(20), 255*BI(1), CH(225)
            if f1%(1) = 0% then write #1  else  rewrite #1
            return


L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * Closes all the files currently open, and also displays    *~
            * a message (ONLY if in foreground) while linking to the    *~
            * next program.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            end
