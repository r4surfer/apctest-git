        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   EEEEE  RRRR   X   X  PPPP   L       OOO   DDDD    *~
            *  S      E      R   R   X X   P   P  L      O   O  D   D   *~
            *   SSS   EEEE   RRRR     X    PPPP   L      O   O  D   D   *~
            *      S  E      R   R   X X   P      L      O   O  D   D   *~
            *   SSS   EEEEE  R   R  X   X  P      LLLLL   OOO   DDDD    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SERXPLOD - Receives passed-in Part & Serial numbers and   *~
            *            displays a 2-level explosion.                  *~
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
            * 02/13/87 ! Original                                 ! JRH *~
            * 03/31/95 ! PRR 13385. UNIX conditioning.            ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

            sub "SERXPLOD" (part$,       /* Passed Part number         */~
                            serial$,     /* Passed Serial number       */~
                            #1,          /* Passed SERDETAL channel    */~
                            #2)          /* Passed HNYMASTR channel    */


        dim                                                              ~
            descr_map(20),               /* PLOWCODE argument          */~
            dummy(1),                    /* PLOWCODE argument          */~
            dummy$(1)99,                 /* PLOWCODE argument          */~
            hdr$(3)132,                  /* PLOWCODE argument          */~
            l$2,                         /* Edited level # (L%)        */~
            msg$79,                      /* PLOWCODE argument          */~
            part$25, partdesc$34,        /* Part number & description  */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            serial$20,                   /* Serial number              */~
            stack$(20)45                 /* Trail of Part/Serials used */

        dim                                                              ~
            f1%(2)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.01 06/23/95 Patch Finalization of R6.04.01  "
        REM *************************************************************

        REM *************************************************************~
            *     S U B R O U T I N E   I N I T I L I Z A T I O N       *~
            *************************************************************

            l% = 0%
            if been_here_before% <> 0% then goto push_onto_stack
                been_here_before% = 1%
            hdr$(1) = "  Component Parts          Component Serial #s "& ~
                "Str Lot      Qty Used  Job #        Qty Moved   Qty From"
            dummy(1) = 0 : mat descr_map = zer
            descr_map( 1) =    1.25 : descr_map( 2) =    1    /*Comp Pt*/
            descr_map( 3) =   26.20 : descr_map( 4) =   26    /*Comp S#*/
            descr_map( 5) =   94.03 : descr_map( 6) =   46    /*Store #*/
            descr_map( 7) =   97.06 : descr_map( 8) =   50    /*Lot    */
            descr_map( 9) =  137.08 : descr_map(10) =   57.104/*QtyUsed*/
            descr_map(11) =  113.08 : descr_map(12) =   69    /*Job #  */
            descr_map(13) =  121.08 : descr_map(14) =   81.104/*QtyMovd*/
            descr_map(15) =  129.08 : descr_map(16) =   92.104/*QtyFrom*/
            goto push_onto_stack

        REM *************************************************************~
            *         M A I N   P R O C E S S I N G   L O O P           *~
            *************************************************************

        plowcode_explode
            hdr$(3) = hex(ac) & "Display Explosion for Serial No. " &    ~
                serial$
            str(hdr$(3),63) = "SERXPLOD: " & str(cms2v$,,8)
            call "DESCRIBE" (#2, part$, partdesc$, 1%, f1%(2))
            msg$ = hex(06) & "Part No. " & part$ & "  " & partdesc$
            convert l% to l$, pic (##)
            str(msg$,65) = "Level: " & l$
            call "PLOWCODE" (#1, plowkey$, msg$, 9045%, .01, f1%(1),     ~
                hdr$(), 0, 0, dummy(), dummy$(), "D", " ", #1,           ~
                descr_map())
            if f1%(1) = 0% then pop_off_from_stack
            get #1 using L10170, part$, serial$
L10170:         FMT CH(25), CH(20)

        push_onto_stack
            if l% = 20% then plowcode_explode
            l% = l% + 1%
            plowkey$ = str(part$,,25) & str(serial$,,20)
            plowkey$ = str(plowkey$,,45) & hex(00)
            stack$(l%) = plowkey$
            goto plowcode_explode

        pop_off_from_stack
            if l% = 1% then end
            l% = l% - 1%
            plowkey$ = str(stack$(l%),,45) & hex(00)
            part$ = str(plowkey$,,25)
            serial$ = str(plowkey$,26,20)
            goto plowcode_explode

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

            end
