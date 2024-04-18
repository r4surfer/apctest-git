        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   GGG   L      V   V   AAA   L      DDDD    222           *~
            *  G      L      V   V  A   A  L      D   D  2   2          *~
            *  G GGG  L      V   V  AAAAA  L      D   D     2           *~
            *  G   G  L       V V   A   A  L      D   D    2            *~
            *   GGG   LLLLL    V    A   A  LLLLL  DDDD   22222          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * GLVALD2  - VALIDATE GL CODE FROM KEYBOARD ENTRY.          *~
            *            ENFORCE ZONING, CHARACTER, DEFAULTS, OR ANY    *~
            *            OTHER EDITS DESIRED.  RETURN 'NORMALIZED'      *~
            *            EXTERNAL G/L FOR DISPLAY/PRINT VIA GLFMT2, AND *~
            *            DERIVED INTERNAL REPRESENTATION FOR READS &    *~
            *            WRITES RETURNS ERRORMSG$ SAME AS DATEOK.       *~
            *            This is a clone of GLVALID for Local Authority *~
            *            account numbers (Dual Books).                  *~
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
            * 11/12/87 ! ORIGINAL (straight steal from GLVALID).  ! JIM *~
            * 09/01/89 ! REMed out test formatting.               ! JDH *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            sub "GLVALD2" (external$, internal$, errormsg$)
            dim errormsg$79,external$12,internal$12, temp$12

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto    L00532
            cms2v$ = "R6.00.00 01/19/90 CMS2 / CMS-I Merge              "
L00532: REM *************************************************************

            errormsg$ =" "
            err% = len(external$)
            if err% <= 9% then L01076
               errormsg$="Account Number Can't Be More Then 9 Characters"
               init (hex(00)) internal$
               end

L01076:     internal$ = external$
            end

        REM Below is a prototype for examples in GLFMT2/GLUNFM2

            errormsg$ =" "
            err% = len(external$)
            if err% <> 9% then L10050
                  if str(external$,3,1) <> "." then L10050
                  if str(external$,5,1) <> "." then L10050
                  internal$ = external$
                  end
L10050:     if err% = 10% then long_test
            if err% < 10% then short_test
               errormsg$ = "Entry Too Long"
L10060:     errormsg$ =errormsg$ & ". Enter as A*XXXXXXXX or AXXXXXXXX"
            init (hex(00)) internal$
            end

        short_test
            if external$ = " " then L10220
            if str(external$,2,1)  = "*" then long_test
            temp$ = str(external$,1,1) & "*" & str(external$,2)
            goto L10160

        long_test
            temp$ = external$
L10160:     if str(temp$,2,1) <> "*" then L10220
            goto L10250
L10220:        errormsg$ = "Invalid Characters"
               goto L10060

L10250:     call "GLUNFM2" (temp$)
            internal$, external$ = temp$
            call "GLFMT2" (external$)
            end

