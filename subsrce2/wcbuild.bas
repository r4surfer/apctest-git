        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  W   W   CCC   BBBB   U   U  IIIII  L      DDDD           *~
            *  W   W  C   C  B   B  U   U    I    L      D   D          *~
            *  W   W  C      BBBB   U   U    I    L      D   D          *~
            *  W W W  C   C  B   B  U   U    I    L      D   D          *~
            *   W W    CCC   BBBB    UUU   IIIII  LLLLL  DDDD           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * WCBUILD  - Rebuild WCMASTR file from details.             *~
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
            * 09/24/85 ! ORIGINAL (CLONED FROM WCRESTR)           ! HES *~
            * 10/24/86 ! WCOUT File Format Change                 ! HES *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

            sub "WCBUILD" (wc$, #11, #23)

        dim                                                              ~
            readkey$50,                                                  ~
            wc$4,                                                        ~
            wcout$4,                                                     ~
            used%(490)

        dim f1%(64)                      /* = 1 IF READ WAS SUCCESSFUL */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.17.01 11/20/86 Order process & planning #2     "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #11 ! WCMASTR  ! Work center master file                  *~
            * #23 ! WCOUT    ! Planned work center use detail rec       *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

        REM *************************************************************~
            *        R E S T O R E  W O R K  C E N T E R S              *~
            *                                                           *~
            * PROCESSING LOOP                                           *~
            *************************************************************

            if wc$ <> "ALL^" then L10110
            display " "
            call "SHOWMSG" ("Rebuilding Work Center Utilization")
            print at(22,02), " ", "Center", "Details Processed"

L10110:     REM Re-align the WCMASTR file...
            init (hex(00)) wcout$: f1%(23) = -1% : hits% = 1%
            readkey$ = all(hex(00))
            if wc$ <> "ALL^" then str(readkey$,,4) = wc$
            call "READ103" (#11, readkey$, f1%(11))
               if f1%(11) <> 0 then L10190
               goto L65000
L10180:     read #11, hold, eod goto L65000
L10190:     if wc$ <> "ALL^" and key(#11) <> wc$ then L65000

            mat used% = zer
            if key(#11) = "VEND" then L10420
            if f1%(23) < 0% then L10260
            if str(wcout$,1,1) = hex(ff) then L10420
            goto L10320
L10260:     init (hex(00))readkey$ : str(readkey$,,4) = key(#11)
            call "PLOWALTS" (#23, readkey$, 1%, 0%, f1%(23))
            if f1%(23) = 0 then L10410
L10290:     get #23, using L10300, wcout$, dd%, su%, run%
L10300:         FMT CH(4), BI(2), XX(25), 2*BI(4)
            used = su% + run%
L10320:     if str(wcout$,,4) > key(#11) then L10420
            if str(wcout$,,4) < key(#11) then L10380
            used%(dd%)=used%(dd%)+used
            if wc$ <> "ALL^" then L10380
            if mod(hits%,50)=0 then print at(23,02), " ", key(#11), hits%
            hits% = hits% + 1%
L10380:     read #23, eod goto L10410
            goto L10290

L10410:     init (hex(ff)) wcout$
L10420:     put #11, using L10430, used%()
L10430:     FMT POS(1040), 490*BI(2)
            rewrite #11
            if wc$ <> "ALL^" then L65000
            print at(23,02), " ", key(#11), hits%
            goto L10180

L65000: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
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
            ASSOCIATESOFSPOKANEWASHINGTONALLRIGHTSRESERVEDGENPGMGENPGMGEN

            if wc$ = "ALL^" then print at(22,02,70); at(23,02,70)
            end
