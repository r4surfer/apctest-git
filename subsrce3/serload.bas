        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   EEEEE  RRRR   L       OOO    AAA   DDDD           *~
            *  S      E      R   R  L      O   O  A   A  D   D          *~
            *   SSS   EEEE   RRRR   L      O   O  AAAAA  D   D          *~
            *      S  E      R   R  L      O   O  A   A  D   D          *~
            *   SSS   EEEEE  R   R  LLLLL   OOO   A   A  DDDD           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SERLOAD  - Handles proper creation & loading of the       *~
            *            SERWORK file when a existing transaction is    *~
            *            called up for edit/review.  Intended to be     *~
            *            called from the DATALOAD section of a CMS      *~
            *            program.                                       *~
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
            * 02/22/94 ! Ensure that SERMASTR gets opened.        ! JDH *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "SERLOAD"  (index%,          /* Line Item Pointer.         */~
                        trantype$,       /* Source Transaction Type    */~
                        trankey$,        /* Source Transaction Key     */~
                        avg_lines%,      /* # Trans to Create File for */~
                        source$,         /* If non-blank Load Serial   */~
                        location$,       /* Numbers from this Source & */~
                                         /* Location, Else from TIF.   */~
                        #1,              /* SYSFILE2 UFB               */~
                        #2,              /* SERTIF   UFB               */~
                        #3,              /* SERMASTR UFB               */~
                        #4,              /* SERWORK  UFB               */~
                        records%,        /* # of WF records added      */~
                        #5)              /* SERHOLD UFB (type VT only) */

*        For type "VT", set SOURCE$ = "h" (lower case h) to indicate
*        that any SERMASTR records found are to be written to the file
*        SERHOLD.  The key to the hold file is the first 29 characters
*        of the TRANKEY$, which should be Vendor number, invoice number
*        and line sequence number.

        dim                                                              ~
            key$40,                      /* Source Transaction Key     */~
            mastr$(2)150,                /* Entire SERMASTR record     */~
            part$25,                     /* Part # S/N is Attached to  */~
            plowkey$96,                  /* Miscellaneous Read/Plow Key*/~
            readkey$96,                  /* Miscellaneous Read/Plow Key*/~
            location$30,                 /* S/N Location to Load from  */~
            serial$20,                   /* Serial Number              */~
            source$1,                    /* Status to Load From        */~
            trankey$40,                  /* Source Transaction Key     */~
            trantype$2,                  /* Source Transaction Type    */~
            type$2                       /* Source Transaction Type    */~

        dim f1%(04),                     /* = 1 If read was successful */~
            fs%(16)

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
        REM *************************************************************

            s%, records% = 0%
            type$ = trantype$ : key$ = trankey$
            if type$ = "VT" or type$ = "PO" or type$ = "PQ"              ~
                                                then load_from_tif2

            if source$ = " " or location$ = " " then load_from_tif       ~
                                                else load_from_master

        REM *************************************************************~
            *             M A I N   P R O C E S S I N G                 *~
            *-----------------------------------------------------------*~
            * Load Work File From TIF File (if present) and SOURCE$ is  *~
            * blank.  Else try to load from SERMASTR file using Source  *~
            * Status and Location.                                      *~
            *************************************************************

        load_from_tif
            if fs%(2) = 0% then call "OPENCHCK" (#2, fs%(2), 0%, 0%, " ")
            if fs%(3) = 0% then call "OPENCHCK" (#3, fs%(3), 0%, 0%, " ")
            plowkey$ = str(trantype$,,2%) & trankey$
L10110:     call "PLOWNEXT" (#2, plowkey$, 42%, f1%(2))
            if f1%(2) = 0% then exit_routine
            get #2 using L10200, part$
            call "SERMKWRK" (#1, avg_lines%, #4) /* Make Work File */
            write #4, using L10190, index%, str(plowkey$,43%,20%),        ~
                                   part$,  eod goto L10110
                records% = records% + 1%
            goto L10110
L10190:     FMT BI(3), CH(20), CH(25)
L10200:     FMT POS(63), CH(25)

        load_from_master
            if fs%(3) = 0% then call "OPENCHCK" (#3, fs%(3), 0%, 0%, " ")
            plowkey$ = str(source$,,1%) & location$
L10250:     call "PLOWALTS" (#3, plowkey$, 2%, 31%, f1%(3))
            if f1%(3) = 0% then exit_routine
            part$ = str(key(#3),,25%)
            serial$ = str(key(#3),26%,20%)
            call "SERMKWRK" (#1, avg_lines%, #4) /* Make Work File */
            write #4, using L10190, index%, serial$, part$, eod goto L10320
                records% = records% + 1%
L10320:     call "SERMKWRK" (#1, avg_lines%, #2) /* Make TIF  File */
            write #2, using L10360, type$, key$, serial$, part$, 0, " ",  ~
                      eod goto L10250
            goto L10250
L10360:     FMT CH(2), CH(40), CH(20), CH(25), PD(14,4), CH(5)

        load_from_tif2
            if fs%(2) = 0% then call "OPENCHCK" (#2, fs%(2), 0%, 0%, " ")
            if fs%(3) = 0% then call "OPENCHCK" (#3, fs%(3), 0%, 0%, " ")
            plowkey$ = str(trantype$,,2%) & trankey$
L10410:     call "PLOWNEXT" (#2, plowkey$, 42%, f1%(2))
            if f1%(2) = 0% then exit_routine
            get #2 using L10910, part$
            readkey$ = str(part$,,25%) & str(plowkey$,43%,20%)
            call "READ101" (#3, readkey$, f1%(3))
                if f1%(3) <> 0% then L10500
L10480:            call "DELETE" (#2, plowkey$, 62%)
                   goto L10410
L10500:     get #3, using L10510, str(readkey$,2,31), str(readkey$,33,42)
L10510:         FMT CH(31), POS(216), CH(42)
            get #3, str(mastr$(),,300)
            if str(readkey$,2,1) = hex(72) then                          ~
                       str(readkey$,1,1) = hex(72) else                  ~
                       str(readkey$,1,1) = "7"

            if str(readkey$,2,1) = "2" and                               ~
                str(readkey$,3,30) = location$ then L10650
            if str(readkey$,2,1) = "7" and                               ~
                str(readkey$,33,42) = str(plowkey$,1,42) then L10650
            if str(readkey$,2,1) = hex(72) and                           ~
                str(readkey$,33,42) = str(plowkey$,1,42) then L10650
            goto L10480

L10650:     put #3 using L10670, str(readkey$,1,1), str(plowkey$,1,42),   ~
                                str(readkey$,2,31)
L10670:         FMT CH(1), POS(216), CH(42), CH(31)
            rewrite #3
            if type$ <> "VT" then L10780
              if source$ <> "h" then L10850
                avg_lines2% = avg_lines% * 50%
                call "SERMKWRK" (#1, avg_lines2%, #5)  /* Hold file */
                s% = s% + 1%
                write #5 using L10750, str(trankey$,,29), s%, str(mastr$())
L10750:              FMT CH(29), BI(4), CH(300)
            goto L10850

L10780:         avg_lines2% = avg_lines% * 50%
                call "SERMKWRK" (#1, avg_lines2%, #5)  /* Hold file */
                write #5 using L10810, str(mastr$())
L10810:              FMT CH(300)

L10850:     call "SERMKWRK" (#1, avg_lines%, #4) /* Make Work File */
            write #4, using L10900, index%, str(plowkey$,43%,20%),        ~
                                   part$,  eod goto L10410
                records% = records% + 1%
            goto L10410
L10900:     FMT BI(3), CH(20), CH(25)
L10910:     FMT POS(63), CH(25)

        exit_routine
            end
