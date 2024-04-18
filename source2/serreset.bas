        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   EEEEE  RRRR   RRRR   EEEEE   SSS   EEEEE  TTTTT   *~
            *  S      E      R   R  R   R  E      S      E        T     *~
            *   SSS   EEEE   RRRR   RRRR   EEEE    SSS   EEEE     T     *~
            *      S  E      R   R  R   R  E          S  E        T     *~
            *   SSS   EEEEE  R   R  R   R  EEEEE   SSS   EEEEE    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SERRESET - Resets Serial Number Status In-Process Status  *~
            *            flags to their previous status.  S/N records   *~
            *            with a Status of '6' are scratched.  S/N       *~
            *            records with a Status of '7' are restored to   *~
            *            their previous status.  All other status's are *~
            *            untouched (assumed to be stable or in process  *~
            *            in the SERTIF file).                           *~
            *----------------------------------------------------------Q*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/26/87 ! Original                                 ! LDJ *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            message$80,                  /* Status Message             */~
            oldlocation$30,              /* Prior Status Location      */~
            oldstatus$1,                 /* Prior Status Code          */~
            plowkey$96,                  /* Miscellaneous Read/Plow Key*/~
            plowkey2$96                  /* Miscellaneous Read/Plow Key*/~

        dim f1%(04),                     /* = 1 If read was successful */~
            f2%(04),                     /* File status flags for      */~
            rslt$(04)20,                 /* Return code                */~
            axd$(04)4                    /* Axd pointer                */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.19.00 03/13/87 Serial number, Lot tracking     "
        REM *************************************************************


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 2 ! SERTIF   ! Temporary Image File for S/N's Trans     *~
            * # 3 ! SERMASTR ! Serial Number Tracking Master File       *~
            *************************************************************~

            select #2,  "SERTIF",                                        ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  62

            select #3,  "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key 1, keypos = 32, keylen =  45,            ~
                            key 2, keypos =  1, keylen =  76

L02141:     k% = 0%
            call "ASKUSER" (k%, "Reset S/N Status      SERRESET:" &      ~
                                str(cms2v$,,8%),                         ~
            "This program resets Serial # Status Flags after a System " &~
            "or User Crash.",                                            ~
            "Please make sure that no one else is logged on, then press "~
            & "RETURN to Process",                                       ~
            "-OR- press PF16 to Exit without Resetting the Status Flags!")
            if k% = 16% then exit_program
            if k% <> 0% then L02141

            REM *** Open the files in IO Mode ***
            call "OPENFILE" (#2, "IO   ", f2%( 2), rslt$( 2), axd$( 2))
            call "OPENFILE" (#3, "IO   ", f2%( 3), rslt$( 3), axd$( 3))
            plowkey$ = "6" & hex(000000000000000000000000000000000000)
            plowkey2$ = "7" & hex(000000000000000000000000000000000000)

        REM *************************************************************~
            *             M A I N   P R O C E S S I N G                 *~
            *-----------------------------------------------------------*~
            * First Look for & Delete Status "6"'s, then restore Status *~
            * "7"'s.                                                    *~
            *************************************************************

        clear_status_six
            call "PLOWAL1" (#3, plowkey$, 2%, 1%, f1%(3))
            if f1%(3) = 0% then reset_status_seven
            message$ = "Removing Serial Number " & str(plowkey$,32%,20%)
            message$ = message$ & " (Part # = " & str(plowkey$,52%,20%)
            message$ = message$ & ")"
            call "SHOSTAT" (message$)
            delete #3
            six% = six% + 1%
            goto clear_status_six


        reset_status_seven
            call "PLOWAL1" (#3, plowkey2$, 2%, 1%, f1%(3))
            if f1%(3) = 0% then exit_program
            message$="Re-Setting Serial Number " & str(plowkey2$,32%,20%)
            message$=message$ & " (Part # = " & str(plowkey2$,52%,20%)
            message$=message$ & ")"
            call "SHOSTAT" (message$)
            get #3 using L10240, plowkey$, oldstatus$, oldlocation$
L10240:     FMT POS(216), CH(42), CH(1), CH(30)
            put #3 using L10260, oldstatus$, oldlocation$
L10260:     FMT CH(1), CH(30)
            rewrite #3
            seven% = seven% + 1%
            str(plowkey$,43%) = str(plowkey2$,32%,20%)
            call "DELETE" (#2, plowkey$, 62%)
            goto reset_status_seven

        exit_program
            print at(1,1);bell
            call "SHOSTAT" ("Serial Number Flags Resetting COMPLETE!")
            end
