        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y   CCC   U   U  TTTTT  RRRR   PPPP    *~
            *  H   H  NN  N  Y   Y  C   C  U   U    T    R   R  P   P   *~
            *  HHHHH  N N N   YYY   C      U   U    T    RRRR   PPPP    *~
            *  H   H  N  NN    Y    C   C  U   U    T    R   R  P       *~
            *  H   H  N   N    Y     CCC    UUU     T    R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYCUTRP - Captures the relevant criteria, then prints the*~
            *            Inventory Valuation Report as of the Cut-Off   *~
            *            date. Report can be printed in Part/Store/Lot  *~
            *            sequence, Category/Part/Store/Lot sequence or  *~
            *            Store/Part/Store/Lot sequence.                 *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/19/91 ! Original.                                ! JIM *~
            * 07/21/93 ! PRR 12967.  Open Workfile w/better # recs! JDH *~
            *          ! PRR 12775.  Added Part Type Range & Sort.!     *~
            * 10/12/93 ! PRR 13032.  Added descr of cost method B.! JDH *~
            * 06/26/96 ! Millie Date Conversion                   ! DER *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            catg$4,                      /* Part Category code         */~
            catgory$(4)4,                /* Part Category Range        */~
            company_name$60,             /* Company Name               */~
            costset$8, set_desc$32,      /* Cost Set to use            */~
            costset_desc$60,             /* Cost Set Sub-Header        */~
            cursor%(2),                  /* Cursor location for edit   */~
            cut_off$10,                  /* Cut-Off Date               */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            hnydetal_key$42,             /* Key to HNYDETAL            */~
            i$(24)80,                    /* Screen Image               */~
            in_file$8,                   /* STCHNY file name           */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            line6$51,                    /* Screen Line #6             */~
            lot$6,                       /* Lot Number                 */~
            low_value$99,                /* Hex 00's                   */~
            method$1, meth_desc$12,      /* Cost method & description  */~
            part$25, partdesc$32,        /* Part Number & description  */~
            partnbr$(4)25,               /* Part Number Range          */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            pn$25, pn_desc$32,           /* Part break variables       */~
            postdate$8,                  /* HNYDETAL Posting Date      */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rptid$6,                     /* Report ID                  */~
            sort$4, sortdesc$32, so$4,   /* Sort scalars               */~
            sortcode$1, sortcode_desc$32,/* Sort by Part/Category/Store*/~
            st$3,                        /* Store break variable       */~
            stornbr$(4)3, stor$3,        /* Store Number Range & scalar*/~
            suom$4,                      /* Stocking Unit of Measure   */~
            time$8,                      /* Time of day                */~
            to_from$20,                  /* To/From Range Message      */~
            typ$3, type$(4)3,            /* Part Type & Range          */~
            typedescr$30,                /* Plowcode Argument          */~
            userid$3,                    /* Current User Id            */~
            yymmdd$10                    /* Cut-Off Date intrnl format */

        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            fs%(64),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! HNYQUAN  ! Inventory Costs & Quantity Master (Summa *~
            * #02 ! HNYDETAL ! INVENTORY DETAILS                        *~
            * #03 ! HNYMASTR ! Inventory Master File                    *~
            * #04 ! SYSFILE2 ! Caelus Management System Information     *~
            * #05 ! STCHNY   ! Standard Cost Set- Inventory Standards   *~
            * #06 ! CATEGORY ! Inventory category codes file            *~
            * #07 ! STORNAME ! Store Master                             *~
            * #08 ! GENCODES ! General Codes File                       *~
            * #20 ! WORKFILE ! Temporary System Workfile (Selected data)*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #02, "HNYDETAL",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  2, keypos =   49, keylen =   2, dup,    ~
                            key  1, keypos =   43, keylen =   6, dup

            select #03, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25

            select #04, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #05, "STCHNY",                                        ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos = 1,    keylen = 25

            select #06, "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   4

            select #07, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3

            select #08, "GENCODES",                                      ~
                        varc, indexed, recsize = 128,                    ~
                        keypos =    1,  keylen = 24

            select #20, "WORKFILE",           /* Selected HNYQUAN keys */~
                        varc,     indexed,  recsize =  50,               ~
                        keypos =  1,   keylen =  38

            call "SHOSTAT" ("Opening files; one moment please")

            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 0%, rslt$(01%))
                get rslt$(01%) using L02554, rec%
L02554:              FMT POS(17), BI(4)
                rec% = max(100%, rec% / 3%) /* for work file creation */
            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 0%, rslt$(02%))
            call "OPENCHCK" (#03, fs%(03%), f2%(03%), 0%, rslt$(03%))
            call "OPENCHCK" (#04, fs%(04%), f2%(04%), 0%, rslt$(04%))
            call "OPENCHCK" (#06, fs%(06%), f2%(06%), 0%, rslt$(06%))
            call "OPENCHCK" (#07, fs%(07%), f2%(07%), 0%, rslt$(07%))
            call "OPENCHCK" (#08, fs%(08%), f2%(08%), 0%, rslt$(08%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date : call "DATEFMT" (date$)
            edtmessage$ = "To modify displayed values, position cursor "&~
                "to desired value & press (RETURN)."
            str(line2$,62%) = "HNYCUTRP: " & str(cms2v$,,8%)
            call "COMPNAME" (12%, company_name$, u3%)
            rptid$ = "HNY054"
            max_lines% = 58%
            low_value$ = xor low_value$
            line6$ = "From:                     To:"

*        See if there are Part Types defined to Check against
            plowkey$ = "PARTTYPE "
            call "PLOWNEXT" (#08, plowkey$, 9%, types_on_file%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 7%
L10100:         gosub'051(fieldnr%)               /* Default / Enables */
                     if enabled% = 0% then L10220
L10120:         gosub'101(fieldnr%, 1%)           /* Display / Accept  */
                     if keyhit%  =  1% then gosub startover
                     if keyhit% <>  4% then L10200
L10150:                   fieldnr% = max(1%, fieldnr% - 1%)
                          gosub'051(fieldnr%)
                          if enabled% = 1% then L10120
                          if fieldnr% = 1% then L10100
                          goto L10150
L10200:              if keyhit% = 16% and fieldnr% = 1% then exit_program
                     if keyhit% <> 0% then L10120
L10220:         gosub'151(fieldnr%)      /* Edit Field for Valid Entry */
                     if errormsg$ <> " " then L10120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)             /* Display Screen - No Entry */
                if keyhit%  =  1% then gosub startover
                if keyhit%  = 16% then plow_thru_hnyquan
                if keyhit% <>  0% then editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% > 7% then editpg1
            if fieldnr% = lastfieldnr% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                if enabled% =  0% then editpg1
L11170:     gosub'101(fieldnr%, 2%)         /* Display & Accept Screen */
                if keyhit%  =  1% then gosub startover
                if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)          /* Edit Field for Valid Entry */
                if errormsg$ <> " " then L11170
                lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            * Honor the criteria, select the data, print the report.    *~
            *************************************************************

        plow_thru_hnyquan
            call "SHOSTAT" ("HNYQUAN records are being selected based o"&~
                "n your criteria.")
            call "FILEBGON" (#20)                /* Bye, bye, WorkFile */
            call "WORKOPEN" (#20, "IO   ", rec%, f1%(20%))/* Hello, WF */
            plowkey$ = xor plowkey$
            str(plowkey$,,25%) = partnbr$(3%)    /* Start HNYQUAN here */
            continue%, f1%(1%) = 0%            /* Start with a READ102 */

L12130
*        Plow through the HNYQUAN file here, looking for candidates.
            if f1%(1%) = 0%                                              ~
                then call "READ102" (#01, plowkey$, f1%(1%))             ~
                else call "READNEXT" (#01, f1%(1%))
            if f1%(1%) = 0% then goto end_of_hnyquan

*        Try to eliminate the record based on the selection criteria.
            get #01 using L12210, part$, stor$, lot$, dt_hand, method$
L12210:         FMT POS(17), CH(25), CH(3), CH(16), POS(69), PD(14,4),   ~
                     POS(403), CH(1)
            if part$ >  partnbr$(4%) then goto end_of_hnyquan
            if stor$ <= stornbr$(3%) then goto L12130
            if stor$ >  stornbr$(4%) then goto L12130

*        Try to eliminate the part based on the category or part type.
            call "READ100" (#03, part$, f1%(3%))           /* HNYMASTR */
                if f1%(3%) = 0% then goto L12130
            get #03 using L12310, catg$, typ$
L12310:         FMT POS(90), CH(4), POS(180), CH(3)
            if catg$ <= catgory$(3%) then goto L12130
            if catg$ >  catgory$(4%) then goto L12130
            if typ$ <= type$(3%) then goto L12130
            if typ$  > type$(4%) then goto L12130

*        The record can't be eliminated -- write it to the WORKFILE.
            sort$ = xor sort$
            if sortcode$ = "C" then sort$ = catg$
            if sortcode$ = "W" then sort$ = stor$
            if sortcode$ = "T" then sort$ = typ$
            write #20 using L12410, sort$, part$, stor$, lot$, dt_hand,   ~
                method$, typ$
L12410:         FMT CH(4), CH(25), CH(3), CH(6), PD(14,4), CH(1), CH(3)
            continue% = 1%  /* Indicate 'we've got something to print' */
            goto L12130

        end_of_hnyquan
            if continue% <> 0% then goto print_the_report
                call "ASKUSER" (2%, "*** NULL SET SELECTED ***",         ~
                     "There are no Parts that match your criteria.",     ~
                     " ", "Press (RETURN) to acknowledge & return.")
                goto inputmode

        print_the_report
            call "SHOSTAT" ("Inventory Valuation Cut-Off Report in prog"&~
                "ress. Please stand by.")
            select printer (134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            time$ = " " : call "TIME" (time$)
            page_nbr% = 0% : nbr_lines% = 999%   /* Force a page break */
            costset_desc$ = "COST SET: " & costset$ & " " & set_desc$
            call "FMTTITLE" (costset_desc$, " ", 2%)

*        Print 'PAGE ZERO' -- the selection screen.
L12622:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L12630
                str(i$(), i%, 1%) = hex(20)
                goto L12622
L12630:     pagesw% = 1%
            gosub page_0_heading
            pagesw% = 0%
            print skip (4)
            print using L60380, "         ---- SELECTION CRITERIA ----"
            print
            for n% = 6% to 19%
                print using L60380, i$(n%)    /* Selection Screen lines */
            next n%

*        Further initializations.
            init (hex(00)) so$, pn$, st$            /* Break variables */
            rn_cost, so_cost, pn_hand, pn_cost, st_hand, st_cost = 0
            plowkey$ = xor plowkey$ /* Start WORKFILE at the beginning */
            eorsw%, f1%(20%) = 0%              /* Start with a READ102 */

L12790
*        Plow through the WORKFILE file here. Print the report.
            if f1%(20%) = 0%                                             ~
                then call "READ102" (#20, plowkey$, f1%(20%))            ~
                else call "READNEXT" (#20, f1%(20%))
            if f1%(20%) = 0% then goto end_of_report

            get #20 using L12410, sort$, part$, stor$, lot$, dt_hand,     ~
                method$, typ$
            if sortcode$ = "P" then goto L12890  /* No 'sort' if 'Part' */
                if sort$ <> so$ then gosub sort_total
L12890:     if part$ <> pn$ then gosub part_total
            if stor$ <> st$ then gosub store_total
            hnydetal_key$ = str(part$) & str(stor$) & str(lot$) & hex(00)

L12930
*        Adjust DT_HAND by the appropriate transactions in HNYDETAL.
            call "PLOWNEXT" (#02, hnydetal_key$, 34%, f1%(2%))
                if f1%(2%) = 0% then L13020
            get #02 using L12970, postdate$, detail_qty
L12970:         FMT POS(43), CH(6), POS(51), PD(14,4)
            if postdate$ <= yymmdd$ then goto L13020
            dt_hand = dt_hand - detail_qty
            goto L12930

L13020
*        On-hand is adjusted. Final preparations & print the line.
            dt_hand = round(dt_hand, 2)
            dt_cost = round(stdcost * dt_hand, 2)
            meth_desc$ = "NO SELECTION"
            if method$ = "R" then meth_desc$ = "ACT LIFO"
            if method$ = "X" then meth_desc$ = "ACT LIFO/ADJ"
            if method$ = "A" then meth_desc$ = "AVG COST"
            if method$ = "B" then meth_desc$ = "MOD AVG COST"
            if method$ = "S" then meth_desc$ = "STD LIFO"
            if method$ = "F" then meth_desc$ = "FIXED STD"
            if method$ = "L" then meth_desc$ = "LAST COST"
            if method$ = "M" then meth_desc$ = "MANUAL COST"
            if method$ = "T" then meth_desc$ = "STD FIFO"
            if method$ = "P" then meth_desc$ = "ACT FIFO"
            if method$ = "Y" then meth_desc$ = "ACT FIFO/ADJ"
            if nbr_lines% > max_lines% then gosub page_heading
            print using L60200, part$, partdesc$, stor$, lot$,            ~
                meth_desc$, stdcost, dt_hand, suom$, dt_cost
            nbr_lines% = nbr_lines% + 1%
            st_hand = st_hand + dt_hand      /* Accumulate Store Total */
            st_cost = st_cost + dt_cost      /* Accumulate Store Total */
            lot% = lot% + 1%           /* Accumulate # Lots/this Store */
            goto L12790

        end_of_report
            eorsw% = 1%
            gosub sort_total
            if nbr_lines% > max_lines% then gosub page_heading
            print using L60350, rn_cost
            print
            time$ = " " : call "TIME" (time$)
            print using L60400, time$                  /* End of Report */
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)
            goto inputmode

        REM *************************************************************~
            *     M I S C E L L A N E O U S   S U B R O U T I N E S     *~
            *************************************************************

        page_heading
            page_nbr% = page_nbr% + 1%
            nbr_lines% = 7%
        page_0_heading
            print page
            print using L60040, date$, time$, company_name$,  /* Header */~
                "-" & rptid$
            print using L60070, cut_off$, page_nbr%     /* Sub-Header 1 */
            print using L60100, costset_desc$           /* Sub-Header 2 */
            if pagesw% <> 0% then return
            print
            if sortcode$ = "P" then goto L15210
                if sortcode$ = "C"    /* Print appropriate Sort Header */~
                     then print using L60120, sort$, sortdesc$
                if sortcode$ = "W"                                       ~
                     then print using L60130, str(sort$,,3%), sortdesc$
                if sortcode$ = "T"                                       ~
                     then print using L60135, str(sort$,,3%), sortdesc$
                print
                nbr_lines% = nbr_lines% + 2%
L15210:     print using L60140                        /* Column Headers */
            print using L60170                           /* Underscores */
            print
            return

        sort_total             /* Category OR Store total, if selected */
            gosub part_total
            if sortcode$ = "P" then goto sort_reset
            if so$ = str(low_value$,,4%) then goto sort_reset
                if nbr_lines% > max_lines% then gosub page_heading
                if sortcode$ = "C"     /* Print appropriate Sort total */~
                     then print using L60290, so$, sortdesc$, so_cost
                if sortcode$ = "W"                                       ~
                     then print using L60320, so$, sortdesc$, so_cost
                if sortcode$ = "T"                                       ~
                     then print using L60342, so$, sortdesc$, so_cost
                if eorsw% <> 1% then nbr_lines% = 999%   /* Page break */
        sort_reset
            rn_cost = rn_cost + so_cost        /* Accumulate Run Total */
            so_cost = 0
            if eorsw% = 1% then return
                if sortcode$ = "P" then goto L15490
                     if sortcode$ = "C" /* Describe selected Sort data */~
                          then call "DESCRIBE" (#06, sort$, sortdesc$,   ~
                               1%, f1%(6%))  /* Describe Part Category */
                     if sortcode$ = "W"                                  ~
                          then call "DESCRIBE" (#07, sort$, sortdesc$,   ~
                               1%, f1%(6%))     /* Describe Store Code */
                     if sortcode$ <> "T" then L15450
                          sortdesc$ = "(UNKNOWN PART TYPE)"
                          convert typ$ to typ%, data goto L15439
                          call "HNYTYPE" (typ%, sortdesc$, 1%)
L15439:                   if types_on_file% = 0% then L15490
                               readkey$ = "PARTTYPE " & str(typ$)
                               str(readkey$,13%) = all(hex(20))
                               call "READ100" (#08, readkey$, f1%(8%))
                               if f1%(8%) <> 1% then L15490
                                   get #08 using L15445, sortdesc$
L15445:                                FMT POS(25), CH(30)
                                   call "PUTPAREN" (sortdesc$)
                                   goto L15490
L15450:              if f1%(6%) <> 0% then goto L15490
                          if sortcode$ = "C"                             ~
                               then sortdesc$ = "(UNKNOWN PART CATEGORY)"~
                               else sortdesc$ = "(UNKNOWN WAREHOUSE CODE)"
L15490:         so$ = sort$                   /* Reset break variables */
                return

        part_total      /* No Part Total if first time or only 1 Store */
            gosub store_total
            if pn$ = str(low_value$,,25%) then goto part_reset
            if store% < 2% then goto L15590
                if nbr_lines% > max_lines% then gosub page_heading
                print using L60260, pn$, pn_desc$, pn_hand, pn_cost
                nbr_lines% = nbr_lines% + 1%
L15590:     print
            nbr_lines% = nbr_lines% + 1%
        part_reset
            so_cost = so_cost + pn_cost   /* Accumulate Category Total */
            pn_hand, pn_cost = 0
            if eorsw% = 1% then return
                stdcost = 0    /* Get the new part's standard cost ... */
                call "STCCOSTS" (part$, costset$, #04, 1%, stdcost)
                partdesc$ = "UNKNOWN PART CODE"
                suom$ = " "
                call "READ100" (#03, part$, f1%(3%))
                if f1%(3%) <> 0% then get #03 using L15720, partdesc$,    ~
                     suom$
L15720:              FMT POS(26), CH(32), POS(74), CH(4)
                pn$ = part$                   /* Reset break variables */
                pn_desc$ = partdesc$
                store% = 0%   /* Initialize number of Stores/this Part */
                return

        store_total  /* No total if 1st time, only 1 Lot or Store sort */
            if st$ = str(low_value$,,3%) then goto store_reset
            if sortcode$ = "W" then goto L15850
            if lot% < 2% then goto L15850
                if nbr_lines% > max_lines% then gosub page_heading
                print using L60230, pn$, pn_desc$, st$, st_hand, st_cost
                nbr_lines% = nbr_lines% + 1%
L15850:     store% = store% + lot%      /* Accumulate # Lots/this Part */
        store_reset
            pn_hand = pn_hand + st_hand       /* Accumulate Part Total */
            pn_cost = pn_cost + st_cost       /* Accumulate Part Total */
            st_hand, st_cost = 0
            if eorsw% = 1% then return
                st$ = stor$
                lot% = 0%      /* Initialize number of Lots/this Store */
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20160,         /* Part Number Range      */~
                              L20200,         /* Store Number Range     */~
                              L20240,         /* Part Category Range    */~
                              L20390,         /* Part Type Range        */~
                              L20280,         /* Cut-Off Date           */~
                              L20320,         /* Cost Set               */~
                              L20350          /* Sort Sequence Code     */
            return

L20160: REM Def/Enable Part Number Range           PARTNBR$()
            if str(partnbr$()) = " " then partnbr$(1%) = "ALL"
            return

L20200: REM Def/Enable Store Number Range          STORNBR$()
            if str(stornbr$()) = " " then stornbr$(1%) = "ALL"
            return

L20240: REM Def/Enable Part Category Range         CATGORY$()
            if str(catgory$()) = " " then catgory$(1%) = "ALL"
            return

L20280: REM Def/Enable Cut-Off Date                CUT_OFF$
            if cut_off$ = " " then cut_off$ = date
            if cut_off$ = date then call "DATFMTC" (cut_off$)
            return

L20320: REM Def/Enable Cost Set                    COSTSET$
            return

L20350: REM Def/Enable Sort Sequence Code          SORTCODE$
            if sortcode$ = " " then sortcode$ = "P"
            return

L20390: REM Def/Enable Part Type Range             TYPE$()
            if str(type$()) = " " then type$(1%) = "ALL"
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$                     /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Part Number Range, 'FIRST', 'LAST', 'ALL' or '?'.      ",~
         "Enter Warehouse Code Range, 'ALL' or '?'.                    ",~
         "Enter Part Category Range, 'ALL' or '?'.                     ",~
         "Enter Part Type Range, 'ALL' or '?'.                         ",~
         "Enter Cut-Off Date.                                          ",~
         "Enter Cost Set or '?' for list (blanks = the current set).   ",~
         "Enter for sort: 'P' by Part, 'C' by Category, 'W' by Whse, or '~
        ~T' by Part Type."

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$, catgory$(), cut_off$,      ~
                partnbr$(), stornbr$(), costset$, set_desc$, sortcode$,  ~
                sortcode_desc$, type$()
            call "ALLFREE"
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
            gosub'050(1%, fieldnr%)
            gosub set_pf1
            if fieldnr% > 0%                                             ~
                then init(hex(8c)) lfac$()                               ~
                else init(hex(86)) lfac$()
            on fieldnr% gosub L40210,         /* Part Number Range      */~
                              L40210,         /* Store Number Range     */~
                              L40210,         /* Part Category Range    */~
                              L40210,         /* Part Type Range        */~
                              L40210,         /* Cut-Off Date           */~
                              L40210,         /* Cost Set               */~
                              L40210          /* Sort Sequence Code     */
            goto L40240

            lfac$(fieldnr%) = hex(80) : return           /* Up / Low   */
L40210:     lfac$(fieldnr%) = hex(81) : return           /* Upper Only */
            lfac$(fieldnr%) = hex(82) : return           /* Numeric    */

L40240:     accept                                                       ~
                at (01,02), "Inventory Valuation at Cut-Off Date",       ~
                at (01,66), "Today:",                                    ~
                at (01,73), fac(hex(8c)), date$                 , ch(08),~
                at (02,02), fac(hex(ac)), line2$                , ch(79),~
                at (04,02), fac(hex(94)), errormsg$             , ch(79),~
                                                                         ~
                at (06,26), fac(hex(ac)),   line6$              , ch(51),~
                                                                         ~
                at (07,02), "Part Number Range",                         ~
                at (07,26), fac(lfac$( 1%)), partnbr$(1%)       , ch(25),~
                at (07,52), fac(lfac$( 1%)), partnbr$(2%)       , ch(25),~
                                                                         ~
                at (08,02), "Warehouse Code Range",                      ~
                at (08,26), fac(lfac$( 2%)), stornbr$(1%)       , ch(03),~
                at (08,52), fac(lfac$( 2%)), stornbr$(2%)       , ch(03),~
                                                                         ~
                at (09,02), "Part Category Range",                       ~
                at (09,26), fac(lfac$( 3%)), catgory$(1%)       , ch(04),~
                at (09,52), fac(lfac$( 3%)), catgory$(2%)       , ch(04),~
                                                                         ~
                at (10,02), "Part Type Range",                           ~
                at (10,26), fac(lfac$( 4%)), type$(1%)          , ch(03),~
                at (10,52), fac(lfac$( 4%)), type$(2%)          , ch(03),~
                                                                         ~
                at (11,02), "Cut-Off Date",                              ~
                at (11,26), fac(lfac$( 5%)), cut_off$           , ch(10),~
                                                                         ~
                at (12,02), "Cost Set",                                  ~
                at (12,26), fac(lfac$( 6%)), costset$           , ch(08),~
                at (12,46), fac(hex(8c)),    set_desc$          , ch(32),~
                                                                         ~
                at (13,02), "Sort Sequence Code",                        ~
                at (13,26), fac(lfac$( 7%)), sortcode$          , ch(01),~
                at (13,46), fac(hex(8c)),    sortcode_desc$     , ch(32),~
                                                                         ~
                at (21,02), fac(hex(a4)),   inpmessage$         , ch(79),~
                at (22,02), fac(hex(8c)),   pf$(1%)             , ch(79),~
                at (23,02), fac(hex(8c)),   pf$(2%)             , ch(79),~
                at (24,02), fac(hex(8c)),   pf$(3%)             , ch(79),~
                                                                         ~
                keys(pfkeys$), key(keyhit%)

            if keyhit% <> 13% then L40660
                call "MANUAL" ("HNYCUTRP") : goto L40240

L40660:     if keyhit% <> 15% then L40690
                call "PRNTSCRN" : goto L40240

L40690:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        set_pf1
        if edit% = 2% then L40880     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40840
                str(pf$(3%),64%)     = " " : str(pfkeys$,16%,1%) = hex(ff)
L40840:     if fieldnr% > 1% then L40860
                str(pf$(2%),18%,26%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
L40860:     return

L40880: if fieldnr% > 0% then L40970  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40970:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (13)Instructions"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50160,         /* Part Number Range      */~
                              L50210,         /* Store Number Range     */~
                              L50260,         /* Part Category Range    */~
                              L50810,         /* Part Type Range        */~
                              L50310,         /* Cut-Off Date           */~
                              L50380,         /* Cost Set               */~
                              L50680          /* Sort Sequence Code     */
            return

L50160: REM Test for Part Number Range            PARTNBR$()
            call "TESTRNGE" (partnbr$(1%), partnbr$(2%), partnbr$(3%),   ~
                partnbr$(4%), errormsg$, #03)
            return

L50210: REM Test for Store Number Range           STORNBR$()
            call "TESTRNGE" (stornbr$(1%), stornbr$(2%), stornbr$(3%),   ~
                stornbr$(4%), errormsg$, #07)
            return

L50260: REM Test for Part Category Range          CATGORY$()
            call "TESTRNGE" (catgory$(1%), catgory$(2%), catgory$(3%),   ~
                catgory$(4%), errormsg$, #06)
            return

L50310: REM Test for Cut-Off Date                 CUT_OFF$
            yymmdd$ = " "
            call "DATEOKC" (cut_off$, yymmdd%, errormsg$)
            if errormsg$ <> " " then return
            yymmdd$ = cut_off$
            call "DATUFMTC" (yymmdd$)
            return

L50380: REM Test for Cost Set                     COSTSET$
            set_desc$ = " "
            if costset$ <> " " then goto L50430
                set_desc$ = "(Current Cost Set)"
                return
L50430:     if costset$ = "?" then costset$ = " "
            plowkey$ = "STC.HDR." & costset$
            set_desc$ = hex(06) & "Select Cost Set"
            call "PLOWCODE" (#04, plowkey$, set_desc$, 8%, 0.30, f1%(4%))
            if f1%(4%) <> 0% then L50510
                errormsg$ = "You must select a Cost Set or leave blank "&~
                     "for 'Current Set'."
                return
L50510:     costset$ = str(plowkey$,9%)
            call "PUTPAREN" (set_desc$)
            get #04 using L50540, in_file$
L50540:         FMT POS(51), CH(4)
            in_file$ = "STC" & in_file$ & "H"
            f2%(5%) = 1%
            call "PUTPRNAM" addr(#05, in_file$)
            call "OPENFILE" (#05, "VALID", f2%(5%), rslt$(5%), " ")
            if f2%(5%) = 0% then return
                u3% = 2%
                call "ASKUSER" (u3%, "*** COST SET ARCHIVED? ***",       ~
                     "This Cost Set is not on disk and is assumed to",   ~
                     "have been archived.",                              ~
                     "Press (RETURN) to acknowledge and continue.")
                errormsg$ = "Cost Set Archived or Unavailable."
                return

L50680: REM Test for Sort Sequence Code           SORTCODE$
            sortcode_desc$ = " "
            if sortcode$ = "P" then sortcode_desc$ = "(Sort by Part Numbe~
        ~r)"
            if sortcode$ = "C" then sortcode_desc$ = "(Sort by Part Categ~
        ~ory)"
            if sortcode$ = "W" then sortcode_desc$ = "(Sort by Warehouse ~
        ~Code)"
            if sortcode$ = "T" then sortcode_desc$ = "(Sort by Part Type)~
        ~"
            if sortcode_desc$ <> " " then return
                errormsg$ = "Enter 'P' for Part, 'C' for Category, 'W' fo~
        ~r Store, or 'T' for Type"
                return

L50810: REM Test for Part Type Range              TYPE$()
            if type$(1%) <> "ALL" then L50820
                type$(2%) = " "
                goto L50940
L50820:     for i% = 1% to 2%
                if i% = 1% then to_from$ = "Part Type From Range"        ~
                           else to_from$ = "Part Type To Range"
                if type$(i%) = "?" and types_on_file% = 1% then L50900
                convert type$(i%) to type%, data goto L50950
                convert type% to type$(i%), pic(000)
                if type% < 0% or type% > 999% then L50950
                if types_on_file% <> 1% then L50920
L50900:              gosub check_gencode_for_part_type
                     if errormsg$ <> " " then return
                     if i% = 1% and type$(2%) = " " then type$(2%) =     ~
                                                                type$(1%)
L50920:     next i%

L50940:     call "TESTRNGE" (type$(1%), type$(2%), type$(3%), type$(4%), ~
                             errormsg$)
            return

L50950:         errormsg$ = "Please enter " & to_from$ &                 ~
                            " as numeric, 000 to 999"
                return

        check_gencode_for_part_type
            if type$(i%) = "?" then type$(i%) = all(hex(20))
            readkey$ = "PARTTYPE " &  type$(i%)
            typedescr$   = hex(06) & "Select " & to_from$
            call "PLOWCODE" (#08, readkey$, typedescr$, 9%, .3, f1%(8%))
            if f1%(8%) = 1% then L51070
                errormsg$ = to_from$ & " not on File"
                return
L51070:     type$(i%) = str(readkey$, 10%, 3%)
            return

        REM *************************************************************~
            *             I M A G E   S T A T E M E N T S               *~
            *************************************************************

L60040: %RUN DATE: ######## @ ########      #############################~
        ~###############################                      HNYCUTRP####~
        ~###
L60070: %                                    INVENTORY VALUATION REPORT A~
        ~S OF CUT-OFF DATE OF ##########                           PAGE: #~
        ~###
L60100: %                                   #############################~
        ~###############################
L60120: %       PART CATEGORY: #### ################################
L60130: %       WAREHOUSE CODE: ### ################################
L60135: %       PART TYPE CODE: ### ################################
L60140: % PART NUMBER               DESCRIPTION                      WHS ~
        ~LOT    COST METHOD      STD COST       ON HAND STKG      TOTAL CO~
        ~ST
L60170: % ------------------------- -------------------------------- --- ~
        ~------ ------------ ------------ ------------- ---- -------------~
        ~--
L60200: % ######################### ################################ ### ~
        ~ ###### ############ ###,###.#### -#####,###.## #### -#######,###~
        ~.##
L60230: % ######################### ################################ ### ~
        ~    *WAREHOUSE TOTAL:             -#####,###.##      -#######,###~
        ~.##
L60260: % ######################### ################################     ~
        ~  **     PART TOTAL:             -#####,###.##      -#######,###.~
        ~##
L60290: %       PART CATEGORY: #### ################################     ~
        ~ *** CATEGORY TOTAL:                                -#######,###.~
        ~##
L60320: %           WAREHOUSE: ### ################################      ~
        ~ ***WAREHOUSE TOTAL:                                -#######,###.~
        ~##
L60342: %           PART TYPE: ### ################################      ~
        ~ ***PART TYPE TOTAL:                                -#######,###.~
        ~##
L60350: %                                                                ~
        ~****      RUN TOTAL:                                -#######,###.~
        ~##
L60380: %                          ######################################~
        ~##########################################
L60400: %                                            ******** END OF REPO~
        ~RT  @  ########  ********

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One moment please")
            call "FILEBGON" (#20)                 /* Bye, bye WORKFILE */
            end
