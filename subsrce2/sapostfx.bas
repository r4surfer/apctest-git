        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS    AAA   PPPP    OOO    SSS   TTTTT  FFFFF  X   X   *~
            *  S      A   A  P   P  O   O  S        T    F       X X    *~
            *   SSS   AAAAA  PPPP   O   O   SSS     T    FFFF     X     *~
            *      S  A   A  P      O   O      S    T    F       X X    *~
            *   SSS   A   A  P       OOO    SSS     T    F      X   X   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SAPOSTFX - Being a close clone of SAPOSTSB, this sub-     *~
            *   routine performs the same function (posting to SASUMRY# *~
            *   file(s)) except that it does it to only one file at a   *~
            *   time -- the file requested in the calling syntax.       *~
            *   This subroutine is intended as a 'rescue' measure for   *~
            *   sites where their SASUMRY# file(s) require rebuilding,  *~
            *   due to loss of data, file corruption, greed or lust.    *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/21/91 ! Original (SAPOSTSB clone).               ! JIM *~
            * 09/27/94 ! PRR-13293 Non-stocked Parts now updated  ! RJH *~
            *          !   if SA Flag set to allow NSP's.         !     *~
            * 08/14/97 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "SAPOSTFX"   (detail$(),     /* SADETAIL Transaction Record*/~
                          #12, #13,      /* SYSFILE2, HNYMASTR UFBs    */~
                          f%)            /* SASUMRY file # 1 to 10     */

        dim                                                              ~
            altkeys$100,                 /* Alternate Keys in Summary  */~
            bookunits(13), bookamt(13),  /* Booking Data               */~
            costs(13),                   /* Shipment Costs             */~
            date$8,                      /* Format 1st Period Date     */~
            detail$(2)150,               /* SADETAIL Record and Input  */~
            fcstbookunits(13),           /* Forecast- Bookings Units   */~
            fcstbookamt  (13),           /*         - Bookings Value   */~
            fcstshipunits(13),           /*         - Shipped Units    */~
            fcstshipamt  (13),           /*         - Shipped Value    */~
            files$(10)2,                 /* Summary File Definitions   */~
            files%(10,2),                /* FILES$() in Interger Form  */~
            grp%(8,2),                   /* Group Pos's on SADETAIL Rec*/~
            nonstock$1,                  /* Update Non-Stock parts?    */~
            p%(1),                       /* Receiver for SEARCH        */~
            part$25,                     /* Part Number                */~
            periods$(13)6,               /* Periods from SA.YEARS.USED.*/~
            postdate$6,                  /* Post Date                  */~
            readkey$100,                 /* Misc Use Read Key          */~
            sales$1,                     /* Switch- Post Gross or Net  */~
            shipunits(13), shipamt(13),  /* Shipments Data             */~
            trans$1,                     /* Type of Transaction        */~
            years$120                    /* Years Used                 */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

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
            * #01 ! SASUMRY0 ! Sales Analysis Summary File              *~
            * #02 ! SASUMRY1 ! Sales Analysis Summary File              *~
            * #03 ! SASUMRY2 ! Sales Analysis Summary File              *~
            * #04 ! SASUMRY3 ! Sales Analysis Summary File              *~
            * #05 ! SASUMRY4 ! Sales Analysis Summary File              *~
            * #06 ! SASUMRY5 ! Sales Analysis Summary File              *~
            * #07 ! SASUMRY6 ! Sales Analysis Summary File              *~
            * #08 ! SASUMRY7 ! Sales Analysis Summary File              *~
            * #09 ! SASUMRY8 ! Sales Analysis Summary File              *~
            * #10 ! SASUMRY9 ! Sales Analysis Summary File              *~
            * #12 ! SYSFILE2 ! Caelus Management System Information     *~
            * #13 ! HNYMASTR ! Parts Master File                        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "SASUMRY0",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup,      ~
                            key 3, keypos =    7, keylen = 50, dup

            select #02, "SASUMRY1",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup,      ~
                            key 3, keypos =    7, keylen = 50, dup

            select #03, "SASUMRY2",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup,      ~
                            key 3, keypos =    7, keylen = 50, dup

            select #04, "SASUMRY3",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup,      ~
                            key 3, keypos =    7, keylen = 50, dup

            select #05, "SASUMRY4",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup,      ~
                            key 3, keypos =    7, keylen = 50, dup

            select #06, "SASUMRY5",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup,      ~
                            key 3, keypos =    7, keylen = 50, dup

            select #07, "SASUMRY6",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup,      ~
                            key 3, keypos =    7, keylen = 50, dup

            select #08, "SASUMRY7",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup,      ~
                            key 3, keypos =    7, keylen = 50, dup

            select #09, "SASUMRY8",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup,      ~
                            key 3, keypos =    7, keylen = 50, dup

            select #10, "SASUMRY9",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup,      ~
                            key 3, keypos =    7, keylen = 50, dup

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            if first_time% <> 0%       /* Do initializations only once */~
                then goto L10000                                          ~
                else first_time% = 1%

*        Load in File Definitions and Switches.
            call "READ100" (#12, "SWITCHS.SA", f1%(12%))   /* SYSFILE2 */
                if f1%(12%) = 0% then goto exit_program
            get #12 using L09140, sales$, nonstock$, files$()
L09140:         FMT POS(21), CH(1), POS(24), CH(1), POS(308), 10*CH(2)

*        Load up Years Used.
            years$ = "YYMMDD" : years% = 1%
            readkey$ = "SA.YEARS.USED." & all(hex(00))
L09190:     call "PLOWNEXT" (#12, readkey$, 14%, f1%(12%)) /* SYSFILE2 */
            if f1%(12%) = 0% then goto L09250
                years$ = years$ & str(readkey$,15%,6%)
                years% = years% + 1%  /* Point to the yr just appended */
                goto L09190

L09250
*        Define Summary File Groupings Position on SADETAIL Record.
            grp%(1%,1%) =  93% : grp%(1%,2%) = 25%  /* Part            */
            grp%(2%,1%) = 118% : grp%(2%,2%) =  4%  /* Part Category   */
            grp%(3%,1%) = 122% : grp%(3%,2%) =  9%  /* Customer Acct   */
            grp%(4%,1%) = 131% : grp%(4%,2%) =  9%  /* Customer Shipto */
            grp%(5%,1%) = 140% : grp%(5%,2%) =  2%  /* Customer Type   */
            grp%(6%,1%) = 142% : grp%(6%,2%) =  3%  /* Store           */
            grp%(7%,1%) = 145% : grp%(7%,2%) =  4%  /* Region          */
            grp%(8%,1%) = 149% : grp%(8%,2%) =  4%  /* Salesman        */

L10000: REM *************************************************************~
            *           P R O C E S S I N G   L O G I C                 *~
            *-----------------------------------------------------------*~
            * Logic to Update Sales Analysis Files.                     *~
            *************************************************************

*        Open/Create S/A Summary file(s) (SASUMRY#) per the value in F%.
            if fs%(f%) < 0% then goto exit_program
            if fs%(f%) > 0% then goto L10170
                call "OPENCHCK" (#f%, fs%(f%), f2%(f%), 500%, rslt$(f%))
                if fs%(f%) < 0% then goto exit_program
                files%(f%,1%), files%(f%,2%) = 0%
                convert str(files$(f%),1%,1%) to files%(f%,1%),          ~
                     data goto L10140
L10140:         convert str(files$(f%),2%,1%) to files%(f%,2%),          ~
                     data goto L10170

L10170
*        Extract some Key Info from Detail Record.
            get str(detail$()) using L10200, trans$, postdate$, units,    ~
                gross, net, part$
L10200:         FMT CH(1), POS(9), CH(6), POS(60), 3*PD(14,4),           ~
                     POS(93), CH(25)
            if units = 0 and gross = 0 and net = 0 then exit_program

*        Get the Period to Post to (after allowing for fiscal years).
            for poo% = years% to 2% step -1%
                if postdate$ >= str(years$,((poo%-1%)*6%)+1%,6%)         ~
                     then goto L10310
            next poo%
            goto exit_program  /* Post date not found in SA.YEAR.USED. */

L10310
*        Now we have the SA.YEARS.USED. record key. Get the period dates.
            readkey$ = "SA.YEARS.USED." & str(years$,((poo%-1%)*6%)+1%,6%)
            call "READ100" (#12, readkey$, f1%(12%))       /* SYSFILE2 */
                if f1%(12%) = 0% then goto exit_program    /* Ooooops! */
            get #12, using L10360, periods$()
L10360:         FMT POS(29), 13*CH(6)
            for period% = 12% to 1% step -1%
                if postdate$ >= periods$(period%) then goto L10420
            next period%
            goto exit_program  /* Post date not found in SA.YEAR.USED. */

L10420
*        See if Part is Stocked and if so, determine its cost.
            call "READ100" (#13, part$, f1%(13%))
            if f1%(13%) = 1%                                             ~
                then str(detail$(),92%, 1%) = "Y"                        ~
                else str(detail$(),92%, 1%) = "N"
            cost = 0
            if f1%(13%) = 1% then call "STCCOSTS" (part$, " ", #12, 1%,  ~
                cost)
            put str(detail$()) using L10510, cost
L10510:         FMT POS(84), PD(14,4)
            cost = cost * units

*        UPDATE_SUMMARIES ?           /* Update On-line Summary Files */
            if nonstock$ = "Y" then goto L10560 /* Want Non-Stock Parts?*/
            if str(detail$(),92%,1%) = "N" then exit_program /*Non-Stck*/
L10560:         amount = gross : if sales$ = "N" then amount = net

            if files$(f%) = " " or files%(f%,1) = 0% then goto L10970

            mat bookunits = zer  :  mat fcstbookunits = zer
            mat bookamt   = zer  :  mat fcstbookamt   = zer
            mat shipunits = zer  :  mat fcstshipunits = zer
            mat shipamt   = zer  :  mat fcstshipamt   = zer
            mat costs     = zer

            readkey$ = str(periods$(1%)) &                               ~
                str(detail$(), grp%(files%(f%,1%),1%),                   ~
                     grp%(files%(f%,1%),2%))
            if files%(f%,2%) <> 0% then str(readkey$,32%,25%) =          ~
                str(detail$(), grp%(files%(f%,2%),1%),                   ~
                     grp%(files%(f%,2%),2%))
            altkeys$ = str(readkey$,32%,25%) & str(readkey$,,6%) &       ~
                str(readkey$, 7%,25%)
            call "READ101" (#f%, readkey$, f1%(f%))
            if f1%(f%) = 0% then L10790
                get #f% using L10770, bookunits(), bookamt(), shipunits(),~
                     shipamt(), costs(), fcstbookunits(), fcstbookamt(), ~
                     fcstshipunits(), fcstshipamt()
L10770:                   FMT XX(56), 117*PD(14,4)

L10790
*        Update Bookings.
            if trans$ = "3" then L10840
                bookunits(period%) = bookunits(period%) + units
                bookamt  (period%) = bookamt  (period%) + amount

L10840
*        Update Shipments.
            if trans$ <> "3" then L10900
                shipunits(period%) = shipunits(period%) + units
                shipamt  (period%) = shipamt  (period%) + amount
                costs    (period%) = costs    (period%) + cost

L10900
*        Write/Rewrite Record.
            put #f% using L10940, readkey$, bookunits(), bookamt(),       ~
                shipunits(), shipamt(), costs(), fcstbookunits(),        ~
                fcstbookamt(), fcstshipunits(), fcstshipamt(), altkeys$
L10940:         FMT CH(56), 117*PD(14,4), CH(56)
            if f1%(f%) = 0% then write #f% else rewrite #f%

L10970
*        Now see if the Year's Used Record needs to be written.
            search years$ = periods$(1%) to p%() step 6%
            if p%(1%) <> 0% then exit_program
                date$ = periods$(1%) : call "DATEFMT" (date$)
                write #12 using L11030, "SA.YEARS.USED.", periods$(1%),   ~
                     periods$(1%), periods$(), " ", " "
L11030:              FMT CH(14), CH(6), CH(8), 13*CH(6), CH(200), CH(194)
                years$ = years$ & periods$(1%)
                years% = years% + 1%  /* Point to the yr just appended */

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
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            end
