        REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                                                           *~
            *  H   H  N   N  Y   Y  BBBB   IIIII  N   N   SSS   BBBB    *~
            *  H   H  NN  N  Y   Y  B   B    I    NN  N  S      B   B   *~
            *  HHHHH  N N N   YYY   BBBB     I    N N N   SSS   BBBB    *~
            *  H   H  N  NN    Y    B   B    I    N  NN      S  B   B   *~
            *  H   H  N   N    Y    BBBB   IIIII  N   N   SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYBINSB - This is a subroutine to find primary bin       *~
            *            locations for items to be sorted by bin        *~
            *            before printing.  It will also locate several  *~
            *            bin locations for a part which can then be     *~
            *            printed on a pick list.                        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/06/90 ! ORIGINAL                                 ! JEF *~
            * 07/26/91 !(QC-FIXES)                                ! RJB *~
            *          !   1) Fixed Code to allow single or multi-!     *~
            *          !      pule Stores Code searches.          !     *~
            *          !   2) Standardized the number of Max Bins !     *~
            *          !      (Locations) to be the same for all  !     *~
            *          !      options.                            !     *~
            *          !   3) Added check to allow planned Stores !     *~
            *          !      (Numeric) only.                     !     *~
            * 08/23/91 !(QC-FIXES) Modified 'SAVE_BIN', 'SORT_BIN'! RJB *~
            *          !   , & 'END_TYPE_Q' to better control when!     *~
            *          !   the Primary Bin (HNYMASTR) Prints      !     *~
            * 09/09/91 !(QC-FIXES) Simplified Code,  made HNYQUAN ! RJB *~
            *          !   & HNYLOCNS searched independent from   !     *~
            *          !   each other to eliminate dup. printing  !     *~
            *          !   Rewrote SAVE_BIN to replace HNYQUAN    !     *~
            *          !   Locations with Dups from HNYLOCNS      !     *~
            * 07/27/92 ! PRR 12543 - When warehouse locations flag! JBK *~
            *          !   set to 'N', and print bins set to 'Y'  !     *~
            *          !   blank HNYQUAN lots with blank bins will!     *~
            *          !   will print.                            !     *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

        sub "HNYBINSB"     (mode$,       /* "S" = Search for Sorting   */~
                                         /* "P" = Search for Printing  */~
                            part$,       /* Part # to find bin for     */~
                          prtbin$,       /* Where to Start Search      */~
                         inc_loc$,       /* Include HNYLOCNS in Search */~
                           store$,       /* Store to limit search to   */~
                         qty_loc%,       /* How far to search for print*/~
                       bin_loc$(),       /* Array to return results in */~
                               #1,       /* HNYMASTR CHANNEL #         */~
                               #2,       /* HNYQUAN CHANNEL #          */~
                               #3)       /* HNYLOCNS CHANNEL #         */

        dim bin$8,                       /* Inventory Bin Location     */~
            binsave$20,                  /* Save Bin Location          */~
            binstore$3,                  /* Save Bin Location's Store  */~
            bin_loc$(100)20,             /* Array for Results of Search*/~
            been_here_before$1,          /* Initialization flag        */~
            bin_line$20,                 /* Temp Storage for Bin Data  */~
            dummy_store$3,               /* Blank Store for Filler     */~
            dummy_lot$6,                 /* Blank Lot for Filler       */~
            inc_loc$1,                   /* Flag to include HNYLOCNS   */~
            index%(1),                   /* Index for Search Statment  */~
            locnskey$99,                 /* Plow key for HNYLOCNS      *?~
            LOT$16,                      /* Lot Number                 */~
            part$25,                     /* Part # to find bin for     */~
            prtbin$1,                    /* Search Starting Point      */~
            readkey$99,                  /* Read Key for Plows         */~
            store$3                      /* Store Number               */


        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            fs%(64),                     /* = 1 if File Open, -1 if not*/~
            rslt$20                      /* File Check Results         */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50 : goto   L01932
            cms2v$ = "R6.02.00 09/09/92 Cycle Counting & MPS Phase I    "
L01932: REM *************************************************************
            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *              I N I T I A L I Z A T I O N                  *~
            *                                                           *~
            *                                                           *~
            *************************************************************

            if been_here_before$ <> " " then L09500

        REM Following code only executed first time in...

            call "OPENCHCK" (#03, fs%(3), 0%, 0%, rslt$)
            been_here_before$ = "1"

L09500: REM Following code executed each time in...

            init (" ")  bin_loc$(), bin$, binsave$
            init("_") dummy_store$, dummy_lot$
            quan_cnt% = 0%
            bin_cnt% = 1%

        REM *************************************************************~
            *      S E A R C H  A N D  L O C A T E  S E C T I O N       *~
            *                                                           *~
            *                                                           *~
            *************************************************************

        REM "Determine which type of search or print doing

            on pos("YAQM" = prtbin$) gosub type_y, type_a, type_q, type_m
L10090:     if mode$ = "P" then gosub sort_bins
            goto L65000

        REM Type 'Y' search or print - HNYMASTR only

        type_y
            gosub read_master
                bin_line$ = "M" & str(dummy_store$) & str(dummy_lot$) &  ~
                                                                str(bin$)
                gosub save_bin
                return

        REM Type 'A' search or print - HNYMASTR, HNYQUAN, HNYLOCNS(?)

        type_a
            gosub type_m
            return

        REM Type 'Q' search or print - HNYQUAN, HNYLOCNS(?)

        type_q
            init (hex(00)) readkey$, locnskey$
            str(readkey$,1,25) = str(part$,,25)
            break% = 25%
            if store$ = " " then L12090
                str(readkey$,26,3) = str(store$,,3)
                break% = 28%
L12090:     str(locnskey$,1,break%) = str(readkey$,1,break%)
L12100:     gosub read_quan
            if f1%(2) = 0% then L12180
                bin_line$ = "Q" & str(binstore$,,3) & str(lot$,1,6) &    ~
                                                            str(bin$,1,8)
                gosub save_bin
                if mode$ = "S" then L12340
                if qty_loc% > 0% and quan_cnt% >= qty_loc% then L12340
                goto L12100
L12180:     if inc_loc$ = "N" then L12280
L12190:         gosub read_locations
                if f1%(3) = 0% then L12280
                     bin_line$ = "L" & str(binstore$,,3) & str(lot$,1,6) ~
                                                         & str(bin$,1,8)
                     gosub save_bin
                     if mode$ = "S" then L12350
                     if qty_loc% > 0% and quan_cnt% >= qty_loc% then L12350
                     goto L12190

L12280:     if prtbin$ <> "Q" then L12350
                if bin_cnt% > 1% then L12350
                     gosub read_master
                     if bin$ = " "  then L12350
                          bin_line$ = "M" & str(dummy_store$,1,3) &      ~
                                      str(dummy_lot$,1,6) & str(bin$,1,8)
L12340:                   gosub save_bin
L12350:     return

        REM Type 'M' Search or Print - HNYMASTR, HNYQUAN, HNYLOCNS(?)

        type_m
            gosub read_master
                if bin$ = " " then L13130
                     bin_line$ = "M" & str(dummy_store$) &               ~
                                              str(dummy_lot$) & str(bin$)
                     gosub save_bin
                     if mode$ = "S" or qty_loc% = 1% then return
L13130:         gosub type_q
                return

        REM Read the HNYMASTR and get the bin number

        read_master
            call "READ100" (#1, part$, f1%(1))
                if f1%(1) = 0% then return
            get #1, using L20100, bin$
L20100:         FMT POS(155), CH(8)
            return

        REM Read then HNYQUAN file and locate bin number

        read_quan
L20330:     call "PLOWNEXT" (#2, readkey$, break%, f1%(2))
                if f1%(2) = 0% then L20420
            get #2, using L20360, binstore$, lot$, bin$, qtyoh
L20360:         FMT POS(42), CH(3), CH(16), CH(8), PD(14,4)
            if str(binstore$,,1) < "0" or str(binstore$,,1) > "9" then   ~
                                                                    L20330
            if inc_loc$ = "Y" then L20395
                if bin$ = " " and lot$ = " " then L20330  else L20400
L20395:         if bin$ = " " then L20330
L20400:              if prtbin$ = "A" then L20420
                          if round(qtyoh,2) <= 0 then L20330
L20420:     return

        REM Read the HNYLOCNS file and locate bin number

        read_locations
L20630:     call "PLOWALTS" (#3, locnskey$, 3%, break%, f1%(3))
                if f1%(3) = 0% then L20720
            get #3, using L20660, binstore$, bin$, lot$, qtyoh
L20660:         FMT POS(552), CH(3), CH(8), CH(6), POS(573), PD(14,4)
            if str(binstore$,,1) < "0" or str(binstore$,,1) > "9" then   ~
                                                                    L20630
                if bin$ = " " then L20630
                     if prtbin$ = "A" then L20720
                          if round(qtyoh,2) <= 0 then L20630
L20720:     return

        REM Save current bin data in array for return

        save_bin
            if mode$ <> "P" then goto L21210
                search str(bin_loc$(),2) = str(bin_line$,2,17) to        ~
                                                        index%() step 20%
                if index%(1) = 0% then L21160
                     if str(bin_line$,1,1) <> "L" then L21230
                          for x% = 1% to (bin_cnt% - 1%)
                               if str(bin_loc$(x%),2,17) <>              ~
                                           str(bin_line$,2,17) then L21150
                                    if str(bin_loc$(x%),1,1) = "M"       ~
                                                               then L21230
                                         str(bin_loc$(x%),1,1) = "L"
                                         goto L21230
L21150:                   next x%
L21160:         if str(bin_loc$(1%),5%,14%) <> str(bin_line$,5%,14%)     ~
                          then L21200
                     init(" ") bin_loc$(1)
                     goto L21210
L21200:         quan_cnt% = quan_cnt% + 1%
L21210:     bin_loc$(bin_cnt%) = str(bin_line$)
            bin_cnt% = bin_cnt% + 1%
L21230:     init(" ") bin_line$
            if bin_cnt% < 100% then return
                return clear all
                goto L10090

        REM Sort the secondary bin into lot, bin order

        sort_bins
            if mode$ <> "P" then L21720
                if bin_cnt% > 2% then L21590
                     if str(bin_loc$(1)) <> " " then L21720
                          bin_loc$(1) = str(bin_loc$(2))
                          init(" ") bin_loc$(2)
                          goto L21720
L21590:         init(" ") binsave$
                if str(bin_loc$(1)) = " " then L21620
                     binsave$ = str(bin_loc$(1))
L21620:         init(hex(00)) bin_loc$(1)
                call "SORT" addr (bin_loc$(), bin_cnt% - 1%, 20%,        ~
                                           bin_loc$(), 2%, 17%, "A", "S")
                if str(binsave$) = " " then L21680
                     bin_loc$(1) = str(binsave$)
                     goto L21720
L21680:         for x% = 1% to bin_cnt%-2%
                     bin_loc$(x%) = str(bin_loc$(x%+1%))
                next x%
                init(" ") bin_loc$(bin_cnt%-1%)
L21720:     return

        REM *************************************************************~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            *************************************************************

L65000: REM CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and proprie- *~
            * tary assets of CAELUS, INCORPORATED, Spokane, WA,    em-  *~
            * bodying substantial creative efforts  and confidential    *~
            * information.  Unauthorized use, copying, decompiling,     *~
            * translating, disclosure, or transfer of it is prohibited. *~
            * Copyright (c) 1983, an unpublished work by CAELUS,        *~
            * INCORPORATED, Spokane, wa.  All rights reserved.          *~
            CAELUS,INCORPORATEDCAELUS,INCORPORATEDCAELUS,INCORPORATEDCAEL

            end
