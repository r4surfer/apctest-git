        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS    AAA   PPPP    OOO    SSS   TTTTT   SSS   BBBB    *~
            *  S      A   A  P   P  O   O  S        T    S      B   B   *~
            *   SSS   AAAAA  PPPP   O   O   SSS     T     SSS   BBBB    *~
            *      S  A   A  P      O   O      S    T        S  B   B   *~
            *   SSS   A   A  P       OOO    SSS     T     SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SAPOSTSB - Sales Analysis Posting Subroutine              *~
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
            * 09/13/86 ! Original                                 ! ERN *~
            * 05/13/87 ! Std Cost Changes.                        ! ERN *~
            * 02/05/88 ! Add check for new RecordType 5 = Cancel  ! JDH *~
            * 11/22/89 ! Expand SADETAIL record size.             ! JEF *~
            * 11/16/90 ! Added Management DMC to SADETAIL.        ! JDH *~
            * 05/15/91 ! PRR 11810.  Fixed FS22 on SADETAIL Write.! JDH *~
            * 02/12/93 ! PRRs 10886, 11079 Non-Stock Part option. ! JIM *~
            * 06/14/94 ! Added EOD to fix potential FS22.         ! JDH *~
            * 08/14/97 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "SAPOSTSB"   (detail$(),     /* Detail Transaction Record  */~
                          #12, #13)      /* SYSFILE2, HNYMASTR         */

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
            mgtrpt_on$1,                 /* Is Management Reporting on?*/~
            nonstock$1,                  /* Update Non-Stock parts?    */~
            p%(1),                       /* Receiver for Search        */~
            part$25,                     /* Part Number                */~
            periods$(13)6,               /* Current Year's Periods     */~
            postdate$6,                  /* Post Date                  */~
            postdtl$1,                   /* Post Detail Switch- O/S/B/N*/~
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
            * #1  ! SASUMRY0 ! Sales Analysis Summary File              *~
            * #2  ! SASUMRY1 ! Sales Analysis Summary File              *~
            * #3  ! SASUMRY2 ! Sales Analysis Summary File              *~
            * #4  ! SASUMRY3 ! Sales Analysis Summary File              *~
            * #5  ! SASUMRY4 ! Sales Analysis Summary File              *~
            * #6  ! SASUMRY5 ! Sales Analysis Summary File              *~
            * #7  ! SASUMRY6 ! Sales Analysis Summary File              *~
            * #8  ! SASUMRY7 ! Sales Analysis Summary File              *~
            * #9  ! SASUMRY8 ! Sales Analysis Summary File              *~
            * #10 ! SASUMRY9 ! Sales Analysis Summary File              *~
            * #11 ! SADETAIL ! Sales Analysis Detail Transactions       *~
            * #12 ! SYSFILE2 ! Caelus Management System Information     *~
            * #13 ! HNYMASTR ! Parts Master File                        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "SASUMRY0",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup

            select #2,  "SASUMRY1",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup

            select #3,  "SASUMRY2",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup

            select #4,  "SASUMRY3",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup

            select #5,  "SASUMRY4",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup

            select #6,  "SASUMRY5",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup

            select #7,  "SASUMRY6",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup

            select #8,  "SASUMRY7",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup

            select #9,  "SASUMRY8",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup

            select #10, "SASUMRY9",                                      ~
                        varc,     indexed,  recsize = 1048,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key 1, keypos =  993, keylen = 56,           ~
                            key 2, keypos = 1024, keylen = 25, dup

            select #11, "SADETAIL",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   8                      ~


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

        if first_time% <> 0% then L10000
            first_time% = 1%

*        Load in File Definitions and Switchs
            call "READ100" (#12, "SWITCHS.SA", f1%(12))
            if f1%(12) = 0% then exit_program

            get #12 using L09140, sales$, postdtl$, nonstock$, files$()
L09140:         FMT XX(20), 2*CH(1), XX(1), CH(1), POS(308), 10*CH(2)

*        Load up Years Used
            years$ = "YYMMDD"
            readkey$ = "SA.YEARS.USED." & all(hex(00))
L09200:     call "PLOWNEXT" (#12, readkey$, 14%, f1%(12))
            if f1%(12) = 0% then L09250
                years$ = years$ & str(readkey$,15,6)
                goto L09200

L09250
*        Define Summary File Groupings Position on SADETAIL Record.
            grp%(1,1) =  93% : grp%(1,2) = 25%      /* Part            */
            grp%(2,1) = 118% : grp%(2,2) =  4%      /* Part Category   */
            grp%(3,1) = 122% : grp%(3,2) =  9%      /* Customer Acct   */
            grp%(4,1) = 131% : grp%(4,2) =  9%      /* Customer Shipto */
            grp%(5,1) = 140% : grp%(5,2) =  2%      /* Customer Type   */
            grp%(6,1) = 142% : grp%(6,2) =  3%      /* Store           */
            grp%(7,1) = 145% : grp%(7,2) =  4%      /* Region          */
            grp%(8,1) = 149% : grp%(8,2) =  4%      /* Salesman        */

*        Open Sales Analysis Files
            for f% = 1% to 10%
                if files$(f%) = " " then L09450
                   call "OPENCHCK" (#f%, fs%(f%), f2%(f%), 0%, rslt$(f%))
                   if f2%(f%) <> 0% then files$(f%) = " "
                   files%(f%,1), files%(f%,2) = 0%
                   convert str(files$(f%),1,1) to files%(f%,1),          ~
                                                          data goto L09430
L09430:            convert str(files$(f%),2,1) to files%(f%,2),          ~
                                                          data goto L09450
L09450:     next f%
            if postdtl$ = "N" then L09500
                call "OPENCHCK" (#11, fs%(11), f2%(11), 1000%, rslt$(11))

L09500:     /* See if Management Reporting is on */
            call "READ100" (#12, "SWITCHS.GL", f1%(12))
            if f1%(12) = 1% then get #12 using L09530, mgtrpt_on$
L09530:         FMT POS(59), CH(1)

L10000: REM *************************************************************~
            *           P R O C E S S I N G   L O G I C                 *~
            *-----------------------------------------------------------*~
            * Logic to Update Sales Analysis Files.                     *~
            *************************************************************

*        Extract some Key Info from Detail Record
            get str(detail$()) using L10090, trans$, postdate$, units,    ~
                                          gross, net, part$
L10090:         FMT CH(1), POS(9), CH(6), POS(60), 3*PD(14,4),           ~
                    POS(93), CH(25)
            if units = 0 and gross = 0 and net = 0 then exit_program

*        Get the Period to Post to
            call "SAPERIOD" (postdate$, #12, periods$(), period%,        ~
                             err$)
            if err$ <> " " then exit_program  /* Not an Open Period    */


*        See if Part is Stocked and, if so, what it's Cost is
            call "READ100" (#13, part$, f1%(13))
            if f1%(13) = 1%                                              ~
                then str(detail$(),92, 1) = "Y" /* It's a stocked part */~
                else str(detail$(),92, 1) = "N"    /* Non-Stocked part */
            cost = 0
            if f1%(13) = 1% then                                         ~
                              call "STCCOSTS" (part$, " ", #12, 1%, cost)
            put str(detail$()) using L10255, cost
L10255:         FMT POS(84), PD(14,4)
            cost = cost * units

*        Determine Management DMC
            mdmc = 0
            if f1%(13) <> 1% then L10300
                mdmc = -1.0
                if mgtrpt_on$ <> "Y" then L10300
                     call "MDMCCOST" (part$, " ", #12, mdmc)
L10300:     put str(detail$()) using L10305, mdmc
L10305:         FMT POS(226), PD(14,4)

*        Update SADETAIL file
            if postdtl$ = "N" then update_summaries
            if postdtl$ = "S" and (trans$ = "1" or trans$ = "2" or       ~
                                   trans$ = "5") then update_summaries
            if postdtl$ = "O" and trans$ = "3" then update_summaries

L10380:         call "GETDTTM" addr(str(detail$(),2,7))
                write #11 using L10400, detail$(), eod goto L10380
L10400:              FMT 2*CH(150)

        update_summaries       /* Update On-line Summary Files         */
            if nonstock$ = "Y" then goto L10440 /* Want Non-Stock Parts?*/
            if str(detail$(),92,1) = "N" /* No- test for & drop Non-Stk*/~
                then exit_program     /* NSP & we don't want 'em- exit */
L10440:     amount = gross : if sales$ = "N" then amount = net

          for f% = 1% to 10%
            if files$(f%) = " " or files%(f%,1) = 0% then next_file

L10490:     mat bookunits = zer  :  mat fcstbookunits = zer
            mat bookamt   = zer  :  mat fcstbookamt   = zer
            mat shipunits = zer  :  mat fcstshipunits = zer
            mat shipamt   = zer  :  mat fcstshipamt   = zer
            mat costs     = zer

            readkey$ = str(periods$(1)) &                                ~
                str(detail$(), grp%(files%(f%,1),1), grp%(files%(f%,1),2))
            if files%(f%,2) <> 0% then str(readkey$,32,25) =             ~
                str(detail$(), grp%(files%(f%,2),1), grp%(files%(f%,2),2))
            altkeys$ = str(readkey$,32,25) & str(readkey$,,6) &          ~
                       str(readkey$, 7,25)
            call "READ101" (#f%, readkey$, onfile%)
            if onfile% = 0% then L10690
                get #f% using L10670, bookunits()    , bookamt(),         ~
                                     shipunits()    , shipamt(), costs(),~
                                     fcstbookunits(), fcstbookamt(),     ~
                                     fcstshipunits(), fcstshipamt()
L10670:              FMT XX(56), 117*PD(14,4)

L10690
*        Update Bookings
            if trans$ = "3" then L10730
                bookunits(period%) = bookunits(period%) + units
                bookamt  (period%) = bookamt  (period%) + amount
L10730
*        Update Shipments
            if trans$ <> "3" then L10780
                shipunits(period%) = shipunits(period%) + units
                shipamt  (period%) = shipamt  (period%) + amount
                costs    (period%) = costs    (period%) + cost

L10780
*        Write/ Rewrite Record
            put #f% using L10840, readkey$,                               ~
                                 bookunits()    , bookamt(),             ~
                                 shipunits()    , shipamt(), costs(),    ~
                                 fcstbookunits(), fcstbookamt(),         ~
                                 fcstshipunits(), fcstshipamt(), altkeys$
L10840:         FMT CH(56), 117*PD(14,4), CH(56)

            if onfile% = 0% then   write #f%, eod goto L10490
            if onfile% = 1% then rewrite #f%
          next_file  :  next f%


*        Now see if the Year's Used Record needs to be written
            search years$ = periods$(1) to p%() step 6
            if p%(1) <> 0% then exit_program
                date$ = periods$(1) : call "DATEFMT" (date$)
                write #12 using L10970, "SA.YEARS.USED.", periods$(1),    ~
                                       periods$(1%), periods$(), " ", " ",      ~
                                       eod goto L10980
L10970:              FMT CH(14), CH(6), CH(8), 13*CH(6), CH(200), CH(194)
L10980:         years$ = years$ & periods$(1)


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
