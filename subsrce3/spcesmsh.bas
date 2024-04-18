        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   SSS   PPPP    CCC   EEEEE   SSS   M   M   SSS   H   H   *~
            *  S      P   P  C   C  E      S      MM MM  S      H   H   *~
            *   SSS   PPPP   C      EEEE    SSS   M M M   SSS   HHHHH   *~
            *      S  P      C   C  E          S  M   M      S  H   H   *~
            *   SSS   P       CCC   EEEEE   SSS   M   M   SSS   H   H   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SPCESMSH - Space smasher routine.  Looks for occurrences  *~
            *            of spaces in the input string that that are    *~
            *            longer than the allowed number of spaces       *~
            *            passed in.  If found, the extra spaces are     *~
            *            deleted (including leading spaces before first *~
            *            non-blank character).  Effect is to shrink the *~
            *            input string if possible.                      *~
            *            Note that any string length is allowed.        *~
            *                                                           *~
            *            Example;  CALL "SPCESMSH" (RDATE$, 2%)         *~
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
            * 01/29/86 ! Original                                 ! LDJ *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

         sub "SPCESMSH" (input$, spaces%)/* Set SPACES% to 0 to        */~
                                         /* eliminate all spaces.      */

        dim input$1                      /* Input string - dummy length*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10022
            cms2v$ = "04.16.01 04/04/86 Physical inventory and miscel  "
L10022: REM *************************************************************
            if input$ = " " then end
            i% = pos(input$ = " ")
L10040:     if i% = 0% or str(input$,i%) = " " then end
            p% = pos(str(input$,i%) = " ")
            if p% = 0% or p%+i% > len(str(input$)) then end
            p1% = pos(str(input$,i%+p%) <> " ") + p% - 1%
            if p1% = 0% then end
            if (p1%+1%) - p% <= spaces% then L10120
            str(input$,min(len(str(input$)),i%+p%-1%+spaces%))   =       ~
                str(input$,i%+p1%)
L10120:     i% = i% + p%
            if i% >= len(input$) then end
            goto L10040
