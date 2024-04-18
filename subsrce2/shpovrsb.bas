        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   SSS   H   H  PPPP    OOO   V   V  RRRR    SSS   BBBB    *~
            *  S      H   H  P   P  O   O  V   V  R   R  S      B   B   *~
            *   SSS   HHHHH  PPPP   O   O  V   V  RRRR    SSS   BBBB    *~
            *      S  H   H  P      O   O   V V   R   R      S  B   B   *~
            *   SSS   H   H  P       OOO     V    R   R   SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SHPOVRSB - performs comparison of the shipped quantity to *~
            *            the overshipment quantity criteria in the      *~
            *            hnymastr file.  If quantity if outside of range*~
            *            a warning message is displayed.                *~
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
            * 09/05/89 ! Original                                 ! LAB *~
            * 11/03/89 ! Added Part & Seq to warning              ! JDH *~
            * 05/15/90 ! Chngd so that ORDER no longer is passed  ! JDH *~
            *          !  in as a string.                         !     *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "SHPOVRSB" (part$,           /* PART KEY                   */~
                        seq$,            /* SEQUENCE                   */~
                        avail,           /* QUANTITY ORDERED           */~
                        order,           /* QUANTITY SHIPPED           */~
                        percent,         /* PERCENT ALLOWED            */~
                        units,           /* UNITS ALLOWED              */~
                        system,          /* SYSTEM PERCENT DEFAULT     */~
                        return% )        /* PF KEY INDICATOR           */~

        dim                                                              ~
            part$25                      /* Part number                */

        dim f2%(64)                      /* = 0 if the file is open    */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.02 07/06/90 Patch Release                   "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            return% = 16%  /* Starts out OK */

            allow = avail
            if units <> 0 or percent <> 0 then goto L09120
                allow = allow * (1 + system/ 100%)
                goto L09130
L09120:     allow = min(allow + units, allow * (1 + percent/100))
L09130:     if order > allow then goto askuser_out
            goto L65000

        askuser_out
L09161:     return% = 2% /* WINDOW AT BOTTOM  */
            call "ASKUSER" (return%, " ***  W A R N I N G  *** ",        ~
            " Quantity Shipped is Greater than Overshipment Defaults Al"&~
            "low ", "Seq: " & seq$ & "    Part: " & part$,               ~
            "Press PF(1) to reenter Shipped Quantity; PF(16) to accept "&~
            "Quantity as is.")

            if return% = 1% then goto L65000
            if return% <> 16% then goto L09161

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1989  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

            end
