        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  N   N  U   U  M   M  PPPP    AAA   RRRR   EEEEE  N   N   *~
            *  NN  N  U   U  MM MM  P   P  A   A  R   R  E      NN  N   *~
            *  N N N  U   U  M M M  PPPP   AAAAA  RRRR   EEEE   N N N   *~
            *  N  NN  U   U  M   M  P      A   A  R   R  E      N  NN   *~
            *  N   N   UUU   M   M  P      A   A  R   R  EEEEE  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * NUMPAREN - ROUTINE WHICH PUTS PARENTHESIS AROUND A        *~
            *            FORMATTED NUMBER STRING IF THE NUMBER IS < 0.  *~
            *            IRREGARDLESS OF THE SIGN OF N, THE WHOLE       *~
            *            STRING IS SHIFTED RIGHT ONE SPACE SO THE "("   *~
            *            DOESN'T FALL OFF THE LEFT EDGE.                *~
            *                                                           *~
            *   SYNTAX - CALL "NUMPAREN" (N, ARG$)                      *~
            *                                                           *~
            *            N    = NUMBER TO BE TESTED FOR SIGN            *~
            *                                                           *~
            *            ARG$ = RECEIVER STRING CONTAINING FORMATTED    *~
            *                   NUMBER N                                *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/11/82 ! ORIGINAL                                 ! ECR *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "NUMPAREN" (n, arg$)

        dim                                                              ~
           arg$20                        /* FORMATTED STRING           */

        REM SHIFT THE WHOLE STRING RIGHT, THEN TEST
            arg$ = " " & arg$
            if n >= 0 then  end

        REM PUT PARENTHESIS AROUND THE NEGATIVE NUMBER
              str(arg$, pos(arg$ <> " ") - 1, 1) = "("
              arg$ = arg$ & ")"
              end

