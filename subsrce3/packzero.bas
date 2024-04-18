        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP    AAA    CCC   K   K  ZZZZZ  EEEEE  RRRR    OOO    *~
            *  P   P  A   A  C   C  K  K      Z   E      R   R  O   O   *~
            *  PPPP   AAAAA  C      KKK      Z    EEEE   RRRR   O   O   *~
            *  P      A   A  C   C  K  K    Z     E      R   R  O   O   *~
            *  P      A   A   CCC   K   K  ZZZZZ  EEEEE  R   R   OOO    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PACKZERO - Puts a numeric array to a string and sets all  *~
            *            zero values to all HEX 00's.  Thus, when the   *~
            *            string is written to disk, maximum compression *~
            *            is achieved.                                   *~
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
            * 03/24/87 ! Original                                 ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "PACKZERO" (a(), a$)

*        A() is the numeric array to be put into A$ (format PD(14,4)).
*        The maximum number of elements in the arrays = 31.

        dim a(31), a$248

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "05.00.00 09/08/87 Standard cost to 12             "

        REM *************************************************************~
            *            S U B R O U T I N E    L O G I C               *~
            *************************************************************

            init (hex(00)) a$
            a% = dim(a(),1)
            for i% = 1% to a%
                a(i%) = round(a(i%), 4)
                if a(i%) <> 0 then                                       ~
                     put str(a$,i%*8%-7%,8%) using L10100, a(i%)
L10100:                   FMT PD(14,4)
            next i%
            end

