        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *   SSS   EEEEE  RRRR   M   M  K   K  W   W  RRRR   K   K   *~
            *  S      E      R   R  MM MM  K  K   W   W  R   R  K  K    *~
            *   SSS   EEEE   RRRR   M M M  KKK    W W W  RRRR   KKK     *~
            *      S  E      R  R   M   M  K  K   WW WW  R   R  K  K    *~
            *   SSS   EEEEE  R   R  M   M  K   K  W   W  R   R  K   K   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SERMKWRK - Make (Create) Serial Numbers Work/TIF/Detail   *~
            *            files.  Checks the passed in Work file UFB to  *~
            *            see if already open.  If yes just exits out.   *~
            *            Else reads SYSFILE2 to obtain the sizing params*~
            *            for the file.  If the PRNAME for the passed in *~
            *            UFB is SERTIF the file is opened and/or created*~
            *            using OPENCHCK. If the PRNAME is SERDETAL the  *~
            *            SERDETAL file is opened and/or created using   *~
            *            OPENCHCK; also if the PRNAME is SERHOLD.   If   ~
            *            the PRNAME is neither of the above then then   *~
            *            the assumption is that a workfile needs to be  *~
            *            created hence the use of WORKOPEN instead of   *~
            *            OPENCHCK.                                      *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/22/87 ! Original                                 ! LDJ *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        sub "SERMKWRK" (#1,              /* SYSFILE2 UFB               */~
                        avg_lines%,      /* Average # Lines per Documnt*/~
                        #2)              /* SERTIF, SERDETAL, SERWORK  */

        dim                                                              ~
            prname$8                     /* Parameter Reference Name   */~

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto   L09052
            cms2v$ = "04.19.00 03/13/87 Serial number, Lot tracking    "
L09052: REM *************************************************************
            call "GETUFBS1" addr(#2, f2%)
            if f2% = 1% then end
            call "GETPRNAM" addr(#2, prname$)

        REM *************************************************************~
            *          M A I N   R O U T I N E   B E G I N S            *~
            *************************************************************

            if fs(#1) = "00" and key(#1) = "SWITCHS.HNY" then L10120
                max_qty% = 100%
                call "READ100" (#1, "SWITCHS.HNY         ", f1%)
                if f1% = 0% then L10141
L10120:              get #1 using L10130, max_qty%, initial_size%, mult%
L10130:                   FMT POS(86),BI(2), POS(88), BI(3), BI(1)
L10141:     if prname$ = "SERDETAL" then worksize% =                     ~
                        min(50000%, max(1000%, initial_size% * mult%))   ~
            else                                                         ~
              worksize% = min(25000%, max(1000%, avg_lines% * max_qty%))

            if prname$ = "SERTIF" or prname$ = "SERDETAL" or             ~
                                     prname$ = "SERHOLD"  then           ~
                call "OPENCHCK" (#2, f1%, f2%, worksize%, "            ")~
            else                                                         ~
                call "WORKOPEN" (#2, "IO   ", worksize%, f2%)

            end
