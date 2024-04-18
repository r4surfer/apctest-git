        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  DDDD    AAA   TTTTT  EEEEE   OOO   FFFFF  FFFFF          *~
            *  D   D  A   A    T    E      O   O  F      F              *~
            *  D   D  AAAAA    T    EEEE   O   O  FFFF   FFFF           *~
            *  D   D  A   A    T    E      O   O  F      F              *~
            *  DDDD   A   A    T    EEEEE   OOO   F      F              *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * DATEOFF  - On passed code 'IG', returns a Gregorian date  *~
            *            based on the integer passed. On passed code    *~
            *            'GI', returns an integral value based on the   *~
            *            Gregorian date passed. Both operations use     *~
            *            January 1st, 1950 as their historical basis.   *~
            *            This subroutine only considers the century     *~
            *            01/01/1950 through 12/31/2049.                 *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1990  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/04/91 ! Original                                 ! JIM *~
            * 05/28/92 ! Modified (greatly) for YYMMDD dates.     ! JBK *~
            *          ! Will only handle dates between 01/01/50  !     *~
            *          ! and 12/31/99.  Will need to re-worked.   !     *~
            * 06/27/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "DATEOFF"      (op_code$,    /* 'GI'= Gregorian to Integer */~
                                         /* 'IG'= Integer to Gregorian */~
                           integer%,     /* # Days Received/Returned   */~
                                         /* 'IG'- may NOT be negative  */~
                                         /*     - returned unchanged   */~
                           gregdate$,    /* Greg date Recieved/Returned*/~
                                         /* 'GI'- 6, 8 or 10 bytes     */~
                                         /*     - formatted/unformatted*/~
                                         /*     - returned unchanged   */~
                                         /* 'IG'- 8 bytes minimum      */~
                                         /*     - returned CCYYMMDD    */~
                           ret%)         /* No meaning on input        */
                                         /* Returned 0%- No error      */
                                         /* Returned 1%- Error detected*/

        dim                                                              ~
            basedate$8,                  /* '500101'                   */~
            errormsg$79,                 /* Just what it implies       */~
            gregdate$10,                 /* Input Date                 */~
            greg$10,                     /* Subroutine work date area  */~
            op_code$2                    /* Operation to perform       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

        REM *************************************************************~
            *    S U B R O U T I N E   I N I T I A L I Z A T I O N S    *~
            *************************************************************

            if baseyr% <> 0% then goto L10000
                baseyr% = 50%
                basedate$ = "19500101"
                call "DATECONV" (basedate$)

L10000: REM *************************************************************~
            *        HEIGH HO, HEIGH HO, IT'S OFF TO WORK WE GO!        *~
            *************************************************************

            ret% = 1%                  /* Indicate 'there is an error' */
            on pos("GI" = op_code$) goto gregorian_to_integer,           ~
                integer_to_gregorian  /* 1st char determines operation */
            goto error_exit             /* Unknown operation requested */

        gregorian_to_integer
*        First, validate and normalize (YYMMDD) the passed Greg date.
            greg$ = gregdate$         /* MMDDYY ? YYMMDD ? Formatted ? */
            gosub normalize_to_yymmdd
            call "DATE" addr("G-", basedate$, greg$, integer%, ret%)
                if ret% <> 0% then error_exit
            goto ok_exit

        integer_to_gregorian
            if integer% < 0% then error_exit
                                           /* INTEGER% must be positive*/
            greg$ = " "                    /* Initialize DATE receiver */
            call "DATE" addr ("G+", basedate$, integer%, greg$, greg%)
            if greg% <> 0% then error_exit
            gosub normalize_to_yymmdd
            str(gregdate$,,8) = greg$        /* Pass it back to caller */
            goto ok_exit

        REM *************************************************************~
            *     M I S C E L L A N E O U S   S U B R O U T I N E S     *~
            *************************************************************

        normalize_to_yymmdd        /* For time being 'C' routines our  */
            call "DATEOKC"  (greg$, greg%, errormsg$)    /* MM/DD/YY ? */
                if greg% = 0% then goto error_exit        /*   Error ? */
            call "DATUFMTC" (greg$)                       /*  YYMMDD   */
            return

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
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        ok_exit
            ret% = 0%             /* Indicate 'no error in processing' */

        error_exit
            end
