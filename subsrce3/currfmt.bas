        REM CAELUSINCSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSINCSPOKANEW*~
            *                                                           *~
            *   CCC   U   U  RRRR   RRRR   FFFFF  M   M  TTTTT          *~
            *  C   C  U   U  R   R  R   R  F      MM MM    T            *~
            *  C      U   U  RRRR   RRRR   FFFF   M M M    T            *~
            *  C   C  U   U  R   R  R   R  F      M   M    T            *~
            *   CCC    UUU   R   R  R   R  F      M   M    T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CURRFMT - Subroutine to format a monetary value using the *~
            *           parameters defined for the currency in CURMASTR.*~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS, INC.,  *~
            * SPOKANE, WA.  ALL RIGHTS RESERVED.                        *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/21/87 ! Original                                 ! MJB *~
            * 07/07/88 ! Elim superfluous dec pt when decimals = 0! JIM *~
            * 12/03/90 ! Modified to set overflow correctly and   ! MJB *~
            *          !  allow space for symbols as required.    !     *~
            * 09/23/90 ! Fix incorrect value when truncating -    ! GJM *~
            *          !  PRR 11711                               !     *~
            * 05/28/91 ! PRR 11910 Fix Right Justification.       ! JIM *~
            CAELUSINCSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSINCSPOKANEW*

        sub "CURRFMT" (amount, currency$, fmtamt$, symbols$)

        dim currency$4,                  /* Currency Code              */~
            round$1,                     /* To round or not to round?  */~
            lead$1,                      /* Format leading symbol      */~
            trail$1,                     /* Format trailing symbol     */~
            just$1,                      /* Justify Right or Left      */~
            statutory$4,                 /* Statutory currency code    */~
            symbols$1,                   /* Symbols - Y or N           */~
            temp$26                      /* Temporary work variable    */

        dim f2%(04),                     /* File status flags for      */~
            f1%(04),                     /* record-on-file flags       */~
            rslt$(04)20,                 /* return code from "OPENCHCK"*/~
            fs%(04)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
        REM *************************************************************

            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            *  #1 ! CURMASTR ! Multi-Currency Master file               *~
            *  #2 ! SYSFILE2 ! CMS System File                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select  #1, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4

            select  #2, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            call "OPENCHCK" (#1, fs%(1), f2%(1), 0%, rslt$(1))


*       ** Initialize Section
            if hereagain% = 2% then L09120
            call "OPENCHCK" (#2, fs%(2), f2%(2), 0%, rslt$(2))
            statutory$ = " "
            call "READ100" (#2, "SWITCHS.CUR", f1%(2))
            if f1%(2) <> 0% then get #2 using L09060, statutory$
L09060:         FMT POS(22), CH(4)
            round$ = "Y"  :  just$ = "R"
            lead$, trail$ = " "
            decml%, hereagain% = 2%
            close #2

L09120:     len2%, len1% = len(str(fmtamt$)) /* Don't EVER modify LEN2% */

        REM *************************************************************~
            * Main Logic Section                                        *~
            *************************************************************

            if currency$ = " " then currency$ = statutory$
            call "READ100" (#1, currency$, f1%(1))
                if f1%(1) = 0 then L10900
            get #1 using L10800, decml%, round$, lead$, trail$, just$
L10800:         FMT POS(36), BI(1), 4*CH(1)
L10900:     if round$ = "Y" then amount = round(amount, decml%)          ~
                            else amount = sgn(amount) *                  ~
                             (int(abs(amount)*10^(decml%)) / 10^(decml%))
            decml = decml%

L11200
*       * Format Number As Requested...
            convert amount to temp$, pic(-#############0.##########)
            call "STRING" addr ("LJ", temp$, 26%)
            if symbols$ = "Y" and lead$ <> " " then len1% = len1% - 1%
            if symbols$ = "Y" and trail$ <> " " then len1% = len1% - 1%
            if decml = 0 and len1% >= pos(temp$ = ".") then L11600
            if decml > 0 and len1% < pos(temp$ = ".") + decml then L12400
L11600:     str(temp$, max(pos(temp$ = ".") + decml + 1,                 ~
                             pos(-temp$ > "0") + 1)) = " "
            convert str(temp$,,len1%) to temp, data goto L12400
            if abs(amount - temp) < .1^(decml+1) then L12700
                if decml - 1 < 0 then L12400
                amount = round(amount, decml -1)
                goto L11200

L12400:         fmtamt$ = all ("#")
                end

L12700
*       * All Clear If Here...
            if decml% = 0%                                               ~
                then fmtamt$ = str(temp$,,len(temp$)-1)                  ~
                else fmtamt$ = temp$
            if symbols$ <> "Y" then L13200
                if lead$ <> " " then fmtamt$ = lead$ & fmtamt$
                if trail$ <> " " then fmtamt$ = fmtamt$ & trail$

L13200:     if just$ = "R" then call "STRING" addr("RJ", fmtamt$, len2%)
            end


        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS, INC.,  *~
            * SPOKANE, WA.  ALL RIGHTS RESERVED.                        *~
            *************************************************************

