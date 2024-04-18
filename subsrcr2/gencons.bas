        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   GGG   EEEEE  N   N   CCC    OOO   N   N   SSS           *~
            *  G      E      NN  N  C   C  O   O  NN  N  S              *~
            *  G GGG  EEEE   N N N  C      O   O  N N N   SSS           *~
            *  G   G  E      N  NN  C   C  O   O  N  NN      S          *~
            *   GGG   EEEEE  N   N   CCC    OOO   N   N   SSS           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GENCONS  - SUBROUTINE FOR USE WITH GENPGM TO GENERATE THE *~
            *            CONSTANT LINES EXTERNALIZED TO REDUCE THE      *~
            *            BRANCH TABLE IN GENPGM.                        *~
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
            * 12/12/86 ! ORIGINAL                                 ! MJB *~
            * 03/12/96 ! Unixed                                   ! ERN *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            sub "GENCONS" (#3)

            dim tilde$1

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto  L10072
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
L10072: REM *************************************************************

            tilde$ = hex(7E)

            dim z$255: init (hex(20)) z$
                   put #3, str(z$,,80%): write #3, using L25060, tilde$
                   put #3, str(z$,,80%): write #3, using L25090, tilde$
                   put #3, str(z$,,80%): write #3, using L25120, tilde$
                   put #3, str(z$,,80%): write #3, using L25150, tilde$
                   put #3, str(z$,,80%): write #3, using L25180, tilde$
                   put #3, str(z$,,80%): write #3, using L25210, " "
                   put #3, str(z$,,80%): write #3, using L25240, " "
                   put #3, str(z$,,80%): write #3, using L25260, tilde$
                   put #3, str(z$,,80%): write #3, using L25280, tilde$
                   put #3, str(z$,,80%): write #3, using L25300, " "
                   put #3, str(z$,,80%): write #3, using L25320, " "
                   put #3, str(z$,,80%): write #3, using L25330, " "
                   put #3, str(z$,,80%): write #3, using L25350, " "
                   put #3, str(z$,,80%): write #3, using L25380, " "
                   put #3, str(z$,,80%): write #3, using L25400, " "
                   put #3, str(z$,,80%): write #3, using L25420, " "
        end

        REM *************************************************************~
            *        I M A G E   S T A T E M E N T S                    *~
            *-----------------------------------------------------------*~
            * Now the image statements for the constant lines.          *~
            *************************************************************

L25060: %    dim f2%(64),                     /* = 0 if the file is open    */#
L25090: %        f1%(64),                     /* = 1 if READ was successful */#
L25120: %        fs%(64),                     /* = 1 if file open, -1 if it */#
L25150: %                                     /*   doesn't exist, or 0 if   */#
L25180: %                                     /*   not yet checked (OPENCHCK*/#
L25210: %        rslt$(64)20                  /* Text from file opening     */#
L25240: %#
L25260: %rem *************************************************************#
L25280: %    *                  Release Version ID Section               *#
L25300: %    *************************************************************#
L25320: %    dim cms2v$50#
L25330: %    cms2v$ = "01.00.00 12/31/99 Pre-Release Version            "#
L25350: %rem *************************************************************#
L25380: %#
L25400: %    mat f2% = con#
L25420: %#


