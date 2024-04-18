        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   AAA   RRRR   M   M   SSS   TTTTT  L      N   N   OOO    *~
            *  A   A  R   R  MM MM  S        T    L      NN  N  O   O   *~
            *  AAAAA  RRRR   M M M   SSS     T    L      N N N  O   O   *~
            *  A   A  R  R     I        S    T    L      N  NN  O   O   *~
            *  A   A  R   R  IIIII   SSS     T    LLLLL  N   N   OOO    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARMSTLNO - Derives Next Settlement Transaction ID or 'U'. *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/26/86 ! Original                                 ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "ARMSTLNO"   (billto$,       /* Bill-to Customer Code      */~
                          stlmnt$,       /* Settlement Number          */~
                          #1,            /* ARMTRIAL Channel           */~
                          #2,            /* SYSFILE2 Channel           */~
                          status%)       /* 0- OK; 1 = No '00' trans   */

*        STLMNT$ is passed in as the first 10 characters of the
*                settlement number.  It is then returned as the full
*                12 characters. The '00' transaction's next ID is upped.
*
*                If passed in as 'U' then the next unapplied sequence
*                number is returned (12 characters: U#######0000).
*

        dim                                                              ~
            billto$9,                    /* Bill-to Customer Number    */~
            next$2,                      /* Next Transaction ID        */~
            readkey$50,                  /* A Read Key                 */~
            stlmnt$12                    /* Settlement Number          */

        dim f1%(12)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.18.00 02/06/87 A/R PhaseII & SA Reports        "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! ARMTRIAL ! A/R Trial Balance                        *~
            * #2  ! SYSFILE2 ! System file- For next FC Number          *~
            *************************************************************~


        REM *************************************************************~
            *                 M A I N   L O G I C                       *~
            *-----------------------------------------------------------*~
            * Do what we are supposed to do.                            *~
            *************************************************************

        if stlmnt$ = "U" then L10290      /* Get next unapplied number  */

*        Assign Next Transaction ID
            readkey$ = str(billto$,,9) & str(stlmnt$,,10) & "00"
            call "READ101" (#1, readkey$, f1%(1))
            if f1%(1) = 0% then error_exit

            get #1 using L10140, next$
L10140:         FMT POS(85), CH(2)
            stlmnt$ = str(stlmnt$,,10) & str(next$,,2)

            next1% = val(str(next$,1,1))
            next2% = val(str(next$,2,1)) + 1%
                if next2% = 58% then next2% = 65%            /* 57 = 9 */
                if next2% > 90% then next1% = next1% + 1%    /* 90 = Z */
                if next2% > 90% then next2% = 48%            /* 48 = 0 */
                if next1% = 58% then next1% = 65%            /* 65 = A */
            next$ = bin(next1%) & bin(next2%)
            put #1 using L10140, next$
            rewrite #1
            goto ok_exit


L10290
*        Get the Next Unapplied Number
            readkey$ = "ARM.NEXT.UNAPPLIED"
            next% = 1%
            call "READ101" (#2, readkey$, f1%(2))
            if f1%(2) = 0% then L10360
                get #2 using L10350, next%
L10350:              FMT XX(20), BI(4)
L10360:     stlmnt$ = "U" & "1234567" & "0000"
            convert next% to str(stlmnt$,2,7), pic(0000000)
            next% = next% + 1%  :  if next% = 9999999% then next% = 1%
            put #2 using L10400, readkey$, next%, " ", " "
L10400:         FMT CH(20), BI(4), CH(226), CH(250)
            if f1%(2) = 0% then write #2  else  rewrite #2
            goto ok_exit


        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution.                                     *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            error_exit  :  status% = 1%  :  end

            ok_exit     :  status% = 0%  :  end

