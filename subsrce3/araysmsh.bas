        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   AAA   RRRR    AAA   Y   Y   SSS   M   M   SSS   H   H   *~
            *  A   A  R   R  A   A   Y Y   S      MM MM  S      H   H   *~
            *  AAAAA  RRRR   AAAAA    Y     SSS   M M M   SSS   HHHHH   *~
            *  A   A  R   R  A   A    Y        S  M   M      S  H   H   *~
            *  A   A  R   R  A   A    Y     SSS   M   M   SSS   H   H   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * ARAYSMSH - Space smasher routine.  Looks for occurrences  *~
            *            of spaces in the input string array that that  *~
            *            are longer than the allowed number of spaces   *~
            *            passed in.  If found, the extra spaces are     *~
            *            deleted (including leading spaces before first *~
            *            non-blank character).  Effect is to shrink the *~
            *            input string array if possible.                *~
            *            Note that any string length is allowed.        *~
            *                                                           *~
            *            Example;  CALL "ARAYSMSH" (ARRAY$(), 2%)       *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/23/86 ! Original (cloned from SPCESMSH)          ! LDJ *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

         sub "ARAYSMSH" (input$(), spaces%)  /* Set SPACES% to 0 to    */~
                                         /* eliminate all spaces.      */

        dim input$(1)1                   /* Input string - dummy length*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L04400
            cms2v$ = "04.16.02 05/22/86 Sales Quote, Ven. Price & Misc  "
L04400: REM *************************************************************
            if str(input$(),1%) = " " then end
            i% = pos(input$() = " ")
L04700:     if i% = 0% or str(input$(),i%) = " " then end
            p% = pos(str(input$(),i%) = " ")
            if p% = 0% or p%+i% > len(str(input$())) then end
            p1% = pos(str(input$(),i%+p%) <> " ") + p% - 1%
            if p1% = 0% then end
            if (p1%+1%) - p% <= spaces% then L05500
            str(input$(),min(len(str(input$())),i%+p%-1%+spaces%))   =   ~
                str(input$(),i%+p1%)
L05500:     i% = i% + p%
            if i% >= len(input$()) then end
            goto L04700
