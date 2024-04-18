        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   CCC    OOO   RRRR   DDDD    OOO    CCC    SSS   BBBB    *~
            *  C   C  O   O  R   R  D   D  O   O  C   C  S      B   B   *~
            *  C      O   O  RRRR   D   D  O   O  C       SSS   BBBB    *~
            *  C   C  O   O  R   R  D   D  O   O  C   C      S  B   B   *~
            *   CCC    OOO   R   R  DDDD    OOO    CCC    SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CORDOCSB - This subroutine generates Core Deposit module  *~
            *            Return Document ID numbers. Assignment of #s   *~
            *            takes into account that they may not be dupli- *~
            *            cated for the same Ship_To customer.           *~
            *                                                           *~
            *            Caveat Callers-- Your calling programs MUST    *~
            *            NOT CALL this subroutine if the Auto-Assign    *~
            *            flag (SYSFILE2 "SWITCHS.COR" POS(106)) is not  *~
            *            equal to 'Y'. Your arguments will be returned  *~
            *            unchanged.                                     *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/10/92 ! Original                                 ! JIM *~
            * 11/03/92 ! Made Re-entrant for retry option         ! KAB *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "CORDOCSB" (ship_to$,        /* Ship-To Customer Number    */~
                       document$)        /* 8-byte Document # to assign*/

        dim                                                              ~
            autoassgn$1,                 /* Is Auto-Assignment allowed?*/~
            document$8,                  /* Return the user's Doc ID   */~
            doc$8,                       /* Return the user's Doc ID   */~
            nextprefx$3,                 /* Doc ID Prefix, if any      */~
            readkey$99,                  /* Read Key                   */~
            ship_to$9,                   /* Ship-To Customer Number    */~
            sys2key$20,                  /* "SWITCHS.COR"              */~
            rslt$20                      /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.02.03 02/16/93 Customer Credit & Core Trackng  "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! SYSFILE2 ! Caelus Management System Information     *~
            * #02 ! CORCRMAS ! Core Bank Credit Master file.            *~
            * #03 ! CORHLDMA ! Core Receipt Hold Master file.           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #02, "CORCRMAS",                                      ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   10, keylen =  20,                     ~
                        alt key  1, keypos =    1, keylen =  29,         ~
                            key  2, keypos =   19, keylen =  11, dup,    ~
                            key  3, keypos =   36, keylen =  34, dup,    ~
                            key  4, keypos =   61, keylen =  40, dup,    ~
                            key  5, keypos =  601, keylen =  40, dup

            select #03, "CORHLDMA",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos = 1,    keylen = 17,                      ~
                        alt key  1, keypos =   10, keylen =  8, dup

*        Open Files if Possible.  Minor Catch 22 if they don't exist,
*        but the callers can create them if it's a problem.
            call "OPENCHCK" (#01, 0%, 0%, 0%, rslt$)
            call "OPENCHCK" (#02, 0%, 0%, 0%, rslt$)
            call "OPENCHCK" (#03, 0%, 0%, 0%, rslt$)

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            sys2key$ = "SWITCHS.COR"
            call "READ101" (#01, sys2key$, f1%)        /* SYSFILE2 */
               if f1% = 0% then goto exit_program      /* OUCH!!   */
            get #01 using L09100 , autoassgn$, nextprefx$, nextdocid%
L09100:         FMT POS(106), CH(1), CH(3), BI(4)
            if autoassgn$ <> "Y" then goto exit_program
*        Can't assign Document # if not allowed or SWITCHS.COR not found.

*        Based on the length of the prefix, compute maximum Doc ID #.

                for n% = 1% to 3%
                  i% = n%
                  if str(nextprefx$,n%,1%) = " " then goto L09220
                next n%
                i% = 4%

L09220:         pl% = i% - 1%
                on i% goto L09240 , L09250 , L09260 , L09270
L09240:         maximum% = 99999999% : goto put_doc_id
L09250:         maximum% =  9999999% : goto put_doc_id
L09260:         maximum% =   999999% : goto put_doc_id
L09270:         maximum% =    99999% : goto put_doc_id

        REM *************************************************************~
            * The work gets done here.                                  *~
            *************************************************************

        bump_doc_id:
            call "READ101" (#01, sys2key$, f1%)            /* SYSFILE2 */
                if f1% = 0% then goto exit_program  /* Not Likely Here */
            get #01 using L10080, nextdocid%
L10080:         FMT POS(110), BI(4)
        put_doc_id:
            next% = nextdocid% + 1%
            if next% > maximum% then next% = 1%
            put #01 using L10080, next%
            rewrite #1

*        Generate the next Return document ID #.
            convert nextdocid% to doc$, pic (0000000#)
            if pl% = 0% then L10220
               str(doc$,,pl%) = str(nextprefx$,,pl%)

L10220
*        Make sure the generated Doc Number is unique/available.
            readkey$ = str(doc$) & hex(000000)
            call "PLOWALTS" (#02, readkey$, 2%, 8%, f2%)   /* CORCRMAS */
            if f2% <> 0% then bump_doc_id /* It's a dup -- try another */
                                         /* It's OK vis-a-vis CORCRMAS */

            call "REDALT0" (#03, readkey$, 1%, f3%)        /* CORHLDMA */
            if f3% <> 0% then bump_doc_id /* It's a dup -- try another */
                                         /* It's OK vis-a-vis CORHLDMA */

            document$ = doc$

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1992  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "ALLFREE"
            end
