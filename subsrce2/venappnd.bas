        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  V   V  EEEEE  N   N   AAA   PPPP   PPPP   N   N  DDDD    *~
            *  V   V  E      NN  N  A   A  P   P  P   P  NN  N  D   D   *~
            *  V   V  EEEE   N N N  AAAAA  PPPP   PPPP   N N N  D   D   *~
            *   V V   E      N  NN  A   A  P      P      N  NN  D   D   *~
            *    V    EEEEE  N   N  A   A  P      P      N   N  DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VENAPPND - HANDLES *ALL* FILE MAINTENANCE FOR THE VENDOR  *~
            *            APPENDIX FILE 'VENDOR1'.                       *~
            *            *******   FOR BARRE INTERNATIONAL ONLY  ********~
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
            * 10/14/83 ! ORIGINAL                                 ! HES *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**


*       -----------------------------------------------------------------

        sub "VENAPPND" (f%,              /* FUNTION CODE  1 = READ RCRD*/~
                                         /* 2 = WRITE RECORD           */~
                                         /* 3 = INPUT MODE             */~
                                         /* 4 = EDIT MODE              */~
                                                                         ~
                                                                         ~
                        record$(),       /* VENDOR2  RECORD            */~
                        text$,           /* TEXT FROM CORRESPONDING    */~
                                         /* VENDOR   RECORD (NOT WHOLE */~
                                         /* RECORD THOUGH.             */~
                        returncode%)     /* RETURN CODE  (MEANING      */~
                                         /* DEPENDS ON WHAT YOUR TRYING*/~
                                         /* TO DO)                     */

*       -----------------------------------------------------------------

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.15.08 02/14/86 VBK & VENDOR enhancements       "
        REM *************************************************************
        select #1, "VENDOR2", varc, indexed, recsize = 170,              ~
                   keypos = 1, keylen = 9

        goto L65000
        if first% = 0% then first%, f201% = 1%

        REM IF VENDOR2 FILE NOT OPEN, THEN CREATE IT
           if f201% = 0 then L01580
               call"OPENFILE"(#1,"SHARE",f201%,rslt$,axd$)
           if f201% = 0 then L01580
               call"OPENFILE"(#1,"OUTPT",f201%,rslt$,axd$)
               close #1
               call"OPENFILE"(#1,"SHARE",f201%,rslt$,axd$)

L01580: REM PROCESS FILE

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

            end
