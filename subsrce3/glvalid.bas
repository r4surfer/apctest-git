        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   GGG   L      V   V   AAA   L      IIIII  DDDD           *~
            *  G      L      V   V  A   A  L        I    D   D          *~
            *  G GGG  L      V   V  AAAAA  L        I    D   D          *~
            *  G   G  L       V V   A   A  L        I    D   D          *~
            *   GGG   LLLLL    V    A   A  LLLLL  IIIII  DDDD           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLVALID  - VALIDATE GL CODE FROM KEYBOARD ENTRY.          *~
            *            ENFORCE ZONING, CHARACTER, DEFAULTS, OR ANY    *~
            *            OTHER EDITS DESIRED.  RETURN 'NORMALIZED'      *~
            *            EXTERNAL G/L FOR DISPLAY/PRINT VIA GLFMT, AND  *~
            *            DERIVED INTERNAL REPRESENTATION FOR READS &    *~
            *            WRITES RETURNS ERRORMSG$ SAME AS DATEOK.       *~
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
            * 07/09/85 ! ORIGINAL                                 ! KEN *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            sub "GLVALID" (external$, internal$, errormsg$)
            dim errormsg$79,external$12,internal$12, temp$12

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.15.01 08/20/85 General Ledger & Purchasing    "
        REM *************************************************************
            errormsg$ =" "
            err% = len(external$)
            if err% <= 9% then L00600
               errormsg$="Account Number Can't Be More Then 9 Characters"
               init (hex(00)) internal$
               end

L00600:     internal$ = external$
            end

        REM BELOW IS PROTOTYPE FOR EXAMPLES IN GLFMT/UNFMT

            errormsg$ =" "
            err% = len(external$)
            if err% = 10% then short_test
            if err% = 11% then long_test
            if err% < 10% then errormsg$="Entry Too Short"
            if err% > 11% then errormsg$ = "Entry Too Long"
L10060:     errormsg$ =errormsg$ & ". Enter as NNNNNN/XXXX or NNNNNNXXXX"
            init (hex(00)) internal$
            end

        short_test
            temp$ = str(external$,,6) & "/" & str(external$,7)
            goto L10170

        long_test
            temp$ = external$
            if str(external$,7,1) <> "/" then L10220
L10170:     if pos(str(temp$,,6) < "0") <> 0% then L10220
            if pos(str(temp$,,6) > "9") <> 0% then L10220
            if pos(str(temp$,8,4) < "0") <> 0% then L10220
            if pos(str(temp$,8,4) > "9") <> 0% then L10220
            goto L10250
L10220:        errormsg$ = "Invalid Characters"
               goto L10060

L10250:     call "GLUNFMT" (temp$)
            internal$, external$ = temp$
            call "GLFMT" (external$)
            end

