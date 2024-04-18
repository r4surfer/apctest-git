        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  TTTTT  IIIII  M   M  EEEEE                               *~
            *    T      I    MM MM  E                                   *~
            *    T      I    M M M  EEEE                                *~
            *    T      I    M   M  E                                   *~
            *    T    IIIII  M   M  EEEEE                               *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * TIME     - Returns TIME in format HH;MM Xm  (8 CHAR).     *~
            *            If Input is blank then time returned is the    *~
            *            system time. If input is non-blank, then       *~
            *            input is returned formatted.  This is useful   *~
            *            to convert military time to am/pm. If input is *~
            *            non blank, then first five characters must be  *~
            *            HH;MM. *NO* testing is done, that's your job.  *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/02/83 ! ORIGINAL                                 ! HES *~
            * 12/03/86 ! Fixed AM when really PM, 0 hour to 12.   ! ERN *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            sub "TIME" (time$)

        dim time$8                       /* RETURN VARIABLE            */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.16.07 01/05/87 OS 7.10 Compatibility           "
        REM *************************************************************
            if time$ = " " then time$ = time
            convert str(time$,1,2) to h, data goto L00450
            convert str(time$,3,2) to m, data goto L00450
            time$ = "  :   AM"
            if h >= 12 then str(time$,6,3) = " PM"
            if h >  12 then h = h - 12
            if h  =  0 then h = 12
            convert h to str(time$,,2), pic(##)
            convert m to str(time$,4,2), pic(00)
            end

L00450: REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            time$ = " "
            end
