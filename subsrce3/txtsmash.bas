        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  TTTTT  X   X  TTTTT   SSS   M   M   AAA    SSS   H   H   *~
            *    T     X X     T    S      MM MM  A   A  S      H   H   *~
            *    T      X      T     SSS   M M M  AAAAA   SSS   HHHHH   *~
            *    T     X X     T        S  M   M  A   A      S  H   H   *~
            *    T    X   X    T     SSS   M   M  A   A   SSS   H   H   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * TXTSMASH - Text smasher routine.   Takes a text string    *~
            *            array and removes all extraneous blanks        *~
            *            giving due consideration to word wrapping      *~
            *            at array element boundaries.                   *~
            *            (Warning; Garbage In = Garbage Out).           *~
            *                                                           *~
            *            Example;                                       *~
            *                    CALL "TXTSMASH" (TEXTIN$(), TEXTOUT$())*~
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
            * 04/23/86 ! Original                                 ! LDJ *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "TXTSMASH" (textin$(), textout$())

            dim textin$(1)1, textout$(1)1
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto    L00112
            cms2v$ = "04.16.02 05/22/86 Sales Quote, Ven. Price & Misc "
L00112: REM *************************************************************
            call "ARAYSMSH" (textin$(),1%) : init(" ") textout$()
            p%, i% = 1% : len% = pos(-str(textin$(),1%) <> " ")
            for x% = 1% to len%
                str(textout$(i%),p%,1%) = str(textin$(),x%,1%)
                if p% >= len(str(textout$(i%))) then L01400
                   p% = p% + 1% : goto L02000
L01400:         if str(textout$(i%),p%,1%) = " " then L01900
                REM *** Word Wrap Logic ***
                    y% = pos(-str(textout$(i%),,p%-1%) = " ")
                    if y% < 2% then L01900
                    str(textout$(i%),y%) = " " : x% = x% - (p% -y%)
L01900:         i% = min(dim(textout$(),1), i% + 1%) : p% = 1%
L02000:     next x%
            end
