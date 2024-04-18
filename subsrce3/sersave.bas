        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   EEEEE  RRRR    SSS    AAA   V   V  EEEEE          *~
            *  S      E      R   R  S      A   A  V   V  E              *~
            *   SSS   EEEE   RRRR    SSS   AAAAA  V   V  EEEE           *~
            *      S  E      R   R      S  A   A   V V   E              *~
            *   SSS   EEEEE  R   R   SSS   A   A    V    EEEEE          *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SERSAVE  - Handles proper update of the Serial Master and *~
            *            TIF files when Data Save processing is         *~
            *            initiated by a user.                           *~
            *-----------------------------------------------------------*~
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
            * 03/26/87 ! Added support for 'IP' Transaction Type  ! LDJ *~
            *          ! (Direct Inventory Maintenance - HNYQIPUT)!     *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "SERSAVE"  (index%,          /* Line Item Pointer.         */~
                        trantype$,       /* Source Transaction Type    */~
                        trankeyin$,      /* Source Transaction Key     */~
                        avg_lines%,      /* # Trans to Create File for */~
                        part$,           /* Part Code                  */~
                        userid$,         /* Current User ID            */~
                        status$,         /* Change Status to ...       */~
                        source$,         /* Change Status from ...     */~
                        clear%,          /* Clear TIF after Save?      */~
                                         /*  0% = NO, Status = Status  */~
                                         /*           OR HEX(40),      */~
                                         /*  1% = YES, Status = Status */~
                                         /*            Location=TRANKEY*/~
                                         /*  2% = NO,  Don't Chg Status*/~
                        #1,              /* SYSFILE2 UFB               */~
                        #2,              /* SERTIF UFB                 */~
                        #3,              /* SERMASTR UFB               */~
                        #4)              /* SERWORK  UFB               */

        dim                                                              ~
            datetime$7,                  /* System Date/Time Stamp     */~
            filekey$96,                  /* Miscellaneous Read/Plow Key*/~
            part$25,                     /* Part Code                  */~
            plowkey$96,                  /* Miscellaneous Read/Plow Key*/~
            readkey$96,                  /* Miscellaneous Read/Plow Key*/~
            source$1,                    /* Source Status Code         */~
            status$1,                    /* Destination Status Code    */~
            stat$1,                      /* Current Status             */~
            trankey$40,                  /* Source Transaction Key     */~
            trankeyin$40,                /* Source Transaction Key     */~
            trantype$2,                  /* Source Transaction Type    */~
            userid$3                     /* User ID of current user    */

        dim f1%(04)                      /* = 1 If read was successful */~


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "04.19.01 03/30/87 Patch release                   "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #5  ! SERDETAL ! Serial Number Relationships file         *~
            *************************************************************

            select #5,  "SERDETAL",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =   46, keylen =  48,                     ~
                        alt key  1, keypos =    1, keylen =  93

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            if clear% = 0% then status$ = status$ or hex(40)
            trankey$ = trankeyin$
            readkey$ = str(trantype$,,2%) & trankey$
            call "PLOWNEXT" (#2, readkey$, 42%, f1%(2))
            plowkey$ = bin(index%,3)
            call "PLOWNEXT" (#4, plowkey$, 3%, f1%(4))
            if f1%(2) + f1%(4) = 0% then exit_routine
            if f5% = 0% then call "OPENCHCK" (#5, f5%, 0%, 0%, " ")
            call "SERMKWRK" (#1, avg_lines%, #2) /* Make TIF if need to*/
            gosub compare_sn
            goto exit_routine

        REM *************************************************************~
            *             M A I N   P R O C E S S I N G                 *~
            *-----------------------------------------------------------*~
            * Compare Work File to TIF File.  Add where needed, delete  *~
            * where no longer found.                                    *~
            *************************************************************

        plow_tif
            call "PLOWNEXT" (#2, readkey$, 42%, f1%(2))
            goto compare_sn

        plow_work
            call "PLOWNEXT" (#4, plowkey$, 3%, f1%(4))
            goto compare_sn

        plow_both
            call "PLOWNEXT" (#2, readkey$, 42%, f1%(2))
            call "PLOWNEXT" (#4, plowkey$, 3%, f1%(4))

        compare_sn
            if f1%(2) + f1%(4) = 0% then return
            if f1%(4) = 0% then delete_from_tif
            if f1%(2) = 0% then write_to_tif
            if str(readkey$,43%,20%) = str(plowkey$,4%,20%) then plow_both
            if str(readkey$,43%,20%) > str(plowkey$,4%,20%) then         ~
               write_to_tif

        delete_from_tif
            call "DELETE" (#2, readkey$, 62%)
            REM *** Delete from Master ***
            filekey$ = str(part$,,25%) & str(readkey$,43%,20%)
            call "READ101" (#3, filekey$, f1%(3))
            if f1%(3) = 0% then plow_tif
            stat$ = str(key(#3,2),,1%) and hex(3f)
            if source$ = "6" and stat$ = "1" then L10380
            if trantype$="IP" and source$="6" and stat$="2" then L10380
            if stat$ = "7" and (trantype$ = "PO" or trantype$ = "PQ" or  ~
                                trantype$ = "VT") then L10380
            if stat$ =  "2" and trantype$ = "VT"  then L10380
            if stat$ <> "6" then L10400
L10380:         delete #3
                call "DELETE" (#5, filekey$, 45%)
                goto plow_tif
L10400:     if str(key(#3,2),,1%) < hex(40) and str(key(#3,2),,1%) <> "7"~
               then plow_tif  /* shouldn't happen but we'll test anyway*/
            put #3 using L10440, source$   /* Back into Source        */
            rewrite #3
L10440:     FMT CH(1)
            goto plow_tif

        write_to_tif
            call "GETDTTM" addr(datetime$)
            if clear% = 0% or clear% = 2% then                           ~
               write #2 using L10720, trantype$, trankey$,                ~
                    str(plowkey$,4%,20%), part$, 1, " ", eod goto L10520
L10520:     filekey$ = str(part$,,25%) & str(plowkey$,4%,20%)
            call "READ101" (#3, filekey$, f1%(3))
            if f1%(3) = 0% then plow_work
               if clear% = 0% then                                       ~
               put #3 using L10680, status$,           datetime$, userid$,~
                                   trantype$, trankey$
               if clear% = 1% then                                       ~
               put #3 using L10690, status$, trankey$, datetime$, userid$,~
                                   trantype$, trankey$
               if clear% = 1% and trantype$ = "WP" and status$ = "1" then~
               put #3 using L10691, trankey$        /* Job Number */
               if clear% = 2% then                                       ~
               put #3 using L10700,                    datetime$, userid$,~
                                   trantype$, trankey$
               if status$ = "2" then                                     ~
               put #3 using L10710, str(trankey$,,3%), str(trankey$,4%,16%)
                                 /*   Store                  Lot       */
               rewrite #3
               goto plow_work
L10680:        FMT CH(1),       POS(202),CH(7),CH(3),POS(216),CH(2),CH(40)
L10690:        FMT CH(1),CH(30),POS(202),CH(7),CH(3),POS(216),CH(2),CH(40)
L10691:        FMT POS(97), CH(8)
L10700:        FMT              POS(202),CH(7),CH(3),POS(216),CH(2),CH(40)
L10710:        FMT POS(183), CH(3), CH(16)         /* Store & Lot      */
L10720:        FMT CH(2), CH(40), CH(20), CH(25), PD(14,4), CH(5)

        exit_routine
            if clear% <> 1% then L10780
            readkey$ = str(trantype$,,2%) & trankey$
            call "DELETE" (#2, readkey$, 42%)
L10780:     readkey$ = bin(index%,3)
            call "DELETE" (#4, readkey$, 3%)
            end
