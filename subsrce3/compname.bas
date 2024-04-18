        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC    OOO   M   M  PPPP   N   N   AAA   M   M  EEEEE   *~
            *  C   C  O   O  MM MM  P   P  NN  N  A   A  MM MM  E       *~
            *  C      O   O  M M M  PPPP   N N N  AAAAA  M M M  EEEE    *~
            *  C   C  O   O  M   M  P      N  NN  A   A  M   M  E       *~
            *   CCC    OOO   M   M  P      N   N  A   A  M   M  EEEEE   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * COMPNAME - Reads for Company Name and formats as told to. *~
            *            Actually returns division if non-blank else    *~
            *            company name.                                  *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/24/86 ! Original                                 ! ERN *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "COMPNAME" (instr%, name$, ret%)

*        INSTR%  XY where X = 1 if name to be expanded;
*                         Y = 0 to Left Justify, 1 to Right, 2 to Center
*
*        NAME$   Company Name.  Left unchanged if name not found on file
*
*        RET%    0 = All ok (got name, may or may not be expanded)
*                1 = Not found

        dim                                                              ~
            div$30,                      /* Company Division Name      */~
            comp$30,                     /* Company Name Field         */~
            name$60                      /* Company Name Field         */

        dim f2%(01),                     /* = 0 if the file is open    */~
            f1%(01),                     /* = 1 if READ was successful */~
            fs%(01),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(01)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.16.07 01/05/87 OS 7.10 Compatibility           "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20                      ~

            ret% = 1%
            call "OPENCHCK" (#1,  fs%(1), f2%(1), 0%, rslt$(1))

            if min(fs%()) < 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            comp$, div$ = " "

        REM *************************************************************~
            *                M A I N   L O G I C                        *~
            *-----------------------------------------------------------*~
            * Gets the Company Name and expands it if so requested.     *~
            *************************************************************

            call "READ100" (#1, "COMPANY TITLE", f1%(1))
            if f1%(1) = 0% then exit_program

            ret% = 0%     /* All's well on the western front */
            get #1 using L10110, comp$, div$
L10110:         FMT XX(20), CH(30), XX(90), CH(30)

            if div$ <> " " then L10170
               name$ = comp$
               goto L10510

L10170:     if comp$ = " " then L10220
            if len(str(name$)) < len(comp$) + len(div$) + 3% then L10220
               name$ = comp$ & " - " & div$
               goto L10510

L10220:        name$ = div$

*        Here we attempt to  E X P A N D  the name we just got.
L10510:     call "FMTTITLE" (name$, " ", instr%)

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution & Closes File.                       *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            if f2%(1) = 0% then close #1
            end
