        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  H   H  N   N  Y   Y  PPPP   RRRR    CCC    SSS   BBBB    *~
            *  H   H  NN  N   Y Y   P   P  R   R  C   C  S      B   B   *~
            *  HHHHH  N N N    Y    PPPP   RRRR   C       SSS   BBBB    *~
            *  H   H  N  NN    Y    P      R   R  C   C      S  B   B   *~
            *  H   H  N   N    Y    P      R   R   CCC    SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYPRCSB - Display Inventory Procurement records for a    *~
            *            given part and/or a given part/vendor          *~
            *            combination.                                   *~
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
            * 07/14/83 ! ORIGINAL                                 ! KEN *~
            * 05/07/86 ! Renamed from PRCDSPS to HNYPRCSB.        ! LDJ *~
            *          !   Other significant changes included     !     *~
            *          !   calling VPRPSLCT to view vendor prices !     *~
            *          !   instead of duplicating the job here,   !     *~
            *          !   changes to the screen format to        !     *~
            *          !   accomodate new standards (nothing done !     *~
            *          !   about the variable names however).     !     *~
            * 06/16/86 ! Enhanced Price Catalogue Interface.      ! LDJ *~
            * 05/13/87 ! HNYPROC, HNYMASTR mods for Std Cost      ! JIM *~
            * 07/29/87 ! Allow input of nonstcked part & vencode  ! HES *~
            * 03/19/92 ! PRR 12146.  Part Descr not truncated now.! JDH *~
            * 04/25/95 ! PRR 11945,12026,12664. Added visability  ! JDH *~
            *          !   into procurement document.             !     *~
            * 06/27/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "HNYPRCSB" (a1$,             /* Vendor Code Passed In      */~
                        a2$,             /* Part Code Passed In        */~
                        vpr%,            /* Vendor Prices Display      */~
                                         /*   Enabled? 0%=Yes, 1% = No.*/~
                        #1,              /* HNYPROC  File UFB          */~
                        #2,              /* HNYMASTR File UFB          */~
                        #3,              /* VENDOR   File UFB          */~
                        #4)              /* VENPRICE File UFB          */~


        dim header$(2)79,                                                ~
            selections$(2)79,                                            ~
            blankdate$8,                 /* Blank date for comparison  */~
            cursor%(2),                  /* Screen Cursor Position     */~
            date$8,                                                      ~
            part$25,                                                     ~
            partcode$25,                 /* Part Code Arg to VPR sub   */~
            vencode$9,                                                   ~
            vendcode$9,                  /* Vendor Code Arg to VPR sub */~
            srcedoc$16,                  /* Source Document            */~
            srctype$1,                   /* Source Type - PO or Invoice*/~
            odate$8,                                                     ~
            partdescr$34,                                                ~
            venddescr$32,                                                ~
            blankline$79,                                                ~
            rdate$(10)8,                                                 ~
            part$(10)25,                                                 ~
            partd$(10)32,                                                ~
            vend$(10)9,                                                  ~
            vendd$(10,2)30,              /* Vendor Descr & Srce Doc    */~
            i$(24)80,                    /* Screen Image from SCREEN   */~
            leadtime$(10)11,             /* Calculated Actual Lead Time*/~
            odate$(10),                                                  ~
            fac$(3)1,                                                    ~
            line2$79,                    /* Screen Header Line         */~
            lfac$(10,4)1,                                                ~
            dfac$(10,2)1,                                                ~
            nfac$(10,2)1,                                                ~
            firstkey$100,                                                ~
            plowkey$100,                                                 ~
            readkey$100,                                                 ~
            incl(1),                     /* Dummy PLOWCODE Argument    */~
            incl$(1)1,                   /* Dummy PLOWCODE Argument    */~
            hdr$(2)79,                   /* Column Header Lines        */~
            dummy$79,                    /* Dummy PLOWCODE Argument    */~
            temp$8,                                                      ~
            tempdate$6,                                                  ~
            tempvend$9,                                                  ~
            temppart$25,                                                 ~
            a1$9,                                                        ~
            a2$25,                                                       ~
            qty$(10)10,                                                  ~
            qty(10),                                                     ~
            cst$(10)10,                                                  ~
            cst(10)


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            vencode$=a1$
            part$=a2$
            date$ = date
            call "DATEFMT" (date$)
            header$(1)=                                                  ~
         "  Date                                                         ~
        ~         Date "
            header$(1) = " "
            header$(2)=                                                  ~
         "Received Part/Vendor                               Quantity    ~
        ~Cost Ea Ordered"
            selections$(1)=                                              ~
        "(1)ReSelect (2)1st (5)Next (8)Prices (9)Tgl (13)Inst (15)Print S~
        ~creen (16)Exit"
            if vpr% <> 0% then str(selections$(1),28%,09%) = " "
            if part$ > " "  or vencode$ > " " then s% =1%
            init (" ") venddescr$, partdescr$, odate$
            blankline$ = "Leave Part, Vendor, & Date Blank to View ALL " ~
                       & "Procurements"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************

        inputmode
            tgl% = 1%
            init (hex(81)) fac$()
            init (hex(9c)) lfac$(), dfac$(), nfac$()
            on s% goto L10100
L10090:     gosub L40000
            if keyhit%=16 then L65000
            if keyhit%=1 then inputmode
L10100:     err%,s% = 0%
            partdescr$=" "
            if part$=" " or keyhit% = 8 then L10200
            partdescr$ = hex(06) & "Select the Part To Use"
            call "GETCODE" (#2, part$, partdescr$, 1%, 0, f1%)
        rem IF F1% > 0% THEN 10200 : goto L10200
                fac$(1)=hex(91)
                err%=1

L10200:     venddescr$=" "
            if vencode$=" " or keyhit% = 8 then L10300
            venddescr$ = hex(06) & "Select the Vendor To Use"
            call "GETCODE" (#3, vencode$, venddescr$, 1%, 0, f1%)
        rem IF F1% > 0% THEN 10300 : goto L10300
                fac$(2)=hex(91)
                err%=1

L10300:     if odate$<>"YES" then L10305
               goto L10400
L10305:     if odate$ = " " or odate$ = blankdate$ then L10400
            call "DATEOK" (odate$, eer%, errormsg$)
            if errormsg$=" " then L10400
                fac$(3)=hex(91)
                err%=1
                eer%=err%

L10400:     if err%<>0 then L10090
            partcode$ = part$ : vendcode$ = vencode$
            if keyhit% = 8% and vpr%= 0% then gosub show_vendor_prices

            init (hex(84)) fac$(),lfac$()
            l%=0
            keyhit%=99
            plow%=1
            ll%=0
            init (hex(00)) firstkey$

            if odate$=" " or odate$ = blankdate$ or odate$="YES" then L10610
            plow%=3
            temp$=odate$
            call "DATUNFMT" (temp$)
            str(firstkey$,1,6)=str(temp$,1,6)
            ll%=6
            if part$=" " then L10770
            str(firstkey$,7,25)=str(part$,1,25)
            ll%=ll%+25
            if vencode$=" " then L10770
            str(firstkey$,32,8)=str(vencode$,1,8)
            ll%=ll%+8
            goto L10770

L10610:     if vencode$<>" "  then L10660
            if odate$="YES" then L10730
            if part$=" " then L10770
            plow%=2
            str(firstkey$,1,25)=str(part$,1,25)
            ll%=25
            if vencode$=" " then L10770
            str(firstkey$,26,9)=str(vencode$,1,9)
            str(firstkey$,35,25)=str(part$,1,25)
            ll%=ll%+34
            goto L10770

L10660:     if vencode$=" " then L10770
            str(firstkey$,1,9)=str(vencode$,1,9)
            ll%=9
            if part$=" " then L10770
            str(firstkey$,10,25)=str(part$,1,25)
            ll%=ll%+25
            goto L10770

L10730:     plow%=4
            if part$=" " then L10770
            str(firstkey$,1,25)=str(part$,1,25)
            ll%=25

L10770:     on plow% gosub L11000,L12000,L13000,L14000
L10780:     gosub L40000
                if keyhit%=1 then inputmode
                if keyhit% <> 8% or vpr% <> 0% then L10800
                   partcode$, vendcode$ = " "
                   if cursor%(1) < 7% or cursor%(1) > 22% then L10796
                   partcode$ = part$((cursor%(1)-6%)/2% + 1%)
                   vendcode$ = vend$((cursor%(1)-6%)/2% + 1%)
L10796:            gosub show_vendor_prices
L10800:         if keyhit%=16 then L65000
                if keyhit%=0 then L10780
                goto L10770

L11000:     if keyhit%=5 then L11021
            plowkey$=firstkey$
L11021:     gosub L19040
L11030:     call "PLOWNEXT" (#1, plowkey$, ll%, f1%)
            if f1%=0 then L19000
            gosub L30000
            if l% > 7% then return
            l%=l%+1
            goto L11030

L12000:     if keyhit%=5 then L12010
            plowkey$=firstkey$
L12010:     gosub L19040
L12020:     call "PLOWALTS" (#1, plowkey$, 1%, ll%, f1%)
            if f1%=0 then L19000
            get #1, using L12050, temppart$, tempvend$
L12050:         FMT XX(6), CH(25), CH(9)
            if temppart$<> part$ then L19000
            if vencode$<>" " and vencode$<> tempvend$ then L12020
            gosub L30000
            if l% > 7% then return
            l%=l%+1
            goto L12020

L13000:     if keyhit%=5 then L13010
            plowkey$=firstkey$
L13010:     gosub L19040
L13020:     call "PLOWALTS" (#1, plowkey$, 2%, ll%, f1%)
            if f1%=0 then L19000
            get #1, using L13050, tempdate$, temppart$, tempvend$
L13050:         FMT CH(6), CH(25), CH(9)
            if str(temp$,1,6)<>str(tempdate$,1,6) then L19000
            if part$<>" " and part$<> temppart$ then L13020
            if vencode$<>" " and vencode$<>tempvend$ then L13020
            gosub L30000
            if l% > 7% then return
            l%=l%+1
            goto L13020

L14000:     gosub L19040
            if keyhit%=5 then L14035
            plowkey$=firstkey$
L14030:     call "PLOWALTS" (#1, plowkey$, 3%, ll%, f1%)
            if f1%=0 then L19000
            goto L14070
L14035:     read #1, eod goto L19000
L14070:     get #1, using L14080, temppart$, tempvend$
L14080:         FMT XX(6), CH(25), CH(9)
            if part$<>" " and temppart$<> part$ then L19000
            if vencode$<>" " and vencode$<> tempvend$ then L14030
            gosub L30000
            if l% > 7% then return
            l%=l%+1
            goto L14035

        show_vendor_prices
            if partcode$ = " " and vendcode$ = " " then partcode$ = part$
            if partcode$ = " " and vendcode$ =" " then vendcode$=vencode$
            if partcode$ = " " and vendcode$ > " " then L15140
               readkey$ = partcode$
               hdr$(1) = "  Part Code                     Description"
               incl(1) = 0
               dummy$ = hex(06) & "Select the Part to View Prices For"
               call "PLOWCODE" (#4, readkey$, dummy$, -8025%, -.32,      ~
                    f1%, hdr$(), 0,0, incl(), incl$(),"Y"," ",#2)
               if f1% = 0% then return
               partcode$ = readkey$
               call "VPRPSLCT" (partcode$, vendcode$, " ", 10%, " ", #4, ~
                                #2, #3, #1)
               return

L15140:     readkey$ = vendcode$
            incl(1) = 0
            hdr$(1) = "  Vendor    Vendor Name"
            dummy$ = hex(06) & "Select the Vendor to View Prices For"
            call "PLOWCODE" (#4, readkey$, dummy$,        -8009%, -2.3,  ~
                    f1%, hdr$(), 0,0, incl(), incl$(),"Y"," ",#3)
            if f1% = 0% then return
            vendcode$ = readkey$
            call "VPRVSLCT" (partcode$, vendcode$, " ", 10%, " ", #4,    ~
                                #3, #2, #1)
            return

L19000:     partd$(l%)="* * * END OF LIST * * *"
            dfac$(l%,1)=hex(84)
            return

L19040:     if l%=0 then L19090
            if part$(l%)<>" " then L19090
            if keyhit%=2 or keyhit%=8 then L19090
                return clear
                return
L19090:     l%=1
            mat qty = zer
            mat cst = zer
            init (" ") rdate$(),odate$(),part$(),partd$(),vend$(),       ~
                       vendd$(), leadtime$(), qty$(), cst$()
            init (hex(9c)) nfac$()
            init (hex(8c)) dfac$()
            return

L30000: REM *************************************************************~
            *        L O A D   D A T A  I N T O   A R R A Y             *~
            *************************************************************

            get #1, using L30500,rdate$(l%),part$(l%),vend$(l%),qty(l%),  ~
                     invcst, srctype$, odate$(l%), srcedoc$
            leadtime$(l%) = " "
            if rdate$(l%) = " " or rdate$(l%) = blankdate$ or   ~
               odate$(l%) = " " or odate$(l%) = blankdate$ then L30200
               call "DATE" addr("G-", str(odate$(l%),,6%),               ~
                                str(rdate$(l%),,6%), daydiff%, ret%)
               if ret% > 0% then L30200
               convert daydiff% to leadtime$, pic(-####)
               call "STRING" addr("LJ", leadtime$, 16%)
               leadtime$(l%) = "(Days=" & leadtime$
               call "SPCESMSH" (leadtime$(l%),0%)
               leadtime$(l%) = leadtime$(l%) & ")"
L30200:     call "DATEFMT" (rdate$(l%))
            call "DATEFMT" (odate$(l%))
            call "GETCODE" (#2, part$(l%), partd$(l%), 0%, 20, f1%)
            call "GETCODE" (#3, vend$(l%), vendd$(l%,1%), 0%, 20, f1%)
            vendd$(l%,2%) = "Source Document Not Available."
            if srctype$ = " " then vendd$(l%,2%) = " "
            if srctype$ = "P" then vendd$(l%,2%) = "PO  - " & srcedoc$
            if srctype$ = "I" then vendd$(l%,2%) = "Inv - " & srcedoc$
            if str(srcedoc$,16%,1%) = "+" then vendd$(l%,2%) =           ~
                       vendd$(l%,2%) & " others"
            if str(vend$(l%),,1%) = "*" then vendd$(l%,2%) = " "
            cst(l%) = round(invcst, 4)
            nfac$(l%,1),nfac$(l%,2)=hex(84)
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

L30500: FMT                      /* FILE: HNYPROC                      */~
            CH(6),               /* Date quantity received in HNYPROC  */~
            CH(25),              /* Part code                          */~
            CH(9),               /* Vendor code                        */~
            XX(25),              /* Part second variable               */~
            XX(6),               /* Reverse date (100000 - yymmdd) for */~
            PD(14,4),            /* Quantity of something in packed de */~
            PD(14,4),            /* Inventory cost per unit            */~
            XX(15),              /* Filler                             */~
            CH(1),               /* Source Document Type - PO or Inv   */~
            CH(6),               /* Date ordered for HNYPROC           */~
            XX(8),               /* Filler                             */~
            CH(16)               /* Source Document                    */

L40000: REM *************************************************************~
            *                                                           *~
            *        D I S P L A Y    I N F O   O N   S C R E E N       *~
            *                                                           *~
            *************************************************************
            line2$ = " "
            on plow% gosub L40070, L40080, L40090, L40100
            goto L40120
L40070:       line2$ = "By Vendor, Part, and Latest Received Date":return
L40080:       line2$ = "By Part, Vendor, and Latest Received Date":return
L40090:       line2$ = "By Receive Date, Part, and Vendor" : return
L40100:       line2$ = "By Part and Latest Received Date" : return

L40120:     for i% = 1 to 10
            call "CONVERT" (cst(i%), 2.4, cst$(i%))
            call "CONVERT" (qty(i%), 2.2, qty$(i%))
            next i%
            str(line2$,62%) = "HNYPRCSB: " & str(cms2v$,,8%)
            if err% > 0% then print at(1,1);bell

L40200:     accept                                                       ~
               at (01,02),                                               ~
        "Display Inventory Procurement History",                         ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), "Part:",                                      ~
               at (03,08), fac(fac$(01)), part$                 , ch(25),~
               at (03,36), "Vendor:",                                    ~
               at (03,44), fac(fac$(02)), vencode$              , ch(09),~
               at (03,55), "Received on Date:",                          ~
               at (03,73), fac(fac$(03)), odate$                , ch(08),~
               at (04,08), fac(hex(84)), partdescr$             , ch(34),~
               at (04,44), fac(hex(84)), venddescr$             , ch(32),~
               at (05,02), fac( hex(ac)), header$(1%)           , ch(79),~
               at (06,02), fac( hex(ac)), header$(2%)           , ch(79),~
                                                                         ~
               at (07,02), fac(lfac$(1,1)), rdate$(1)           , ch(08),~
               at (09,02), fac(lfac$(2,1)), rdate$(2)           , ch(08),~
               at (11,02), fac(lfac$(3,1)), rdate$(3)           , ch(08),~
               at (13,02), fac(lfac$(4,1)), rdate$(4)           , ch(08),~
               at (15,02), fac(lfac$(5,1)), rdate$(5)           , ch(08),~
               at (17,02), fac(lfac$(6,1)), rdate$(6)           , ch(08),~
               at (19,02), fac(lfac$(7,1)), rdate$(7)           , ch(08),~
               at (21,02), fac(lfac$(8,1)), rdate$(8)           , ch(08),~
                                                                         ~
               at (07,11), fac(lfac$(1,2)), part$ (1)           , ch(25),~
               at (09,11), fac(lfac$(2,2)), part$ (2)           , ch(25),~
               at (11,11), fac(lfac$(3,2)), part$ (3)           , ch(25),~
               at (13,11), fac(lfac$(4,2)), part$ (4)           , ch(25),~
               at (15,11), fac(lfac$(5,2)), part$ (5)           , ch(25),~
               at (17,11), fac(lfac$(6,2)), part$ (6)           , ch(25),~
               at (19,11), fac(lfac$(7,2)), part$ (7)           , ch(25),~
               at (21,11), fac(lfac$(8,2)), part$ (8)           , ch(25),~
                                                                         ~
               at (07,37), fac(dfac$(1,1)), partd$(1)           , ch(32),~
               at (09,37), fac(dfac$(2,1)), partd$(2)           , ch(32),~
               at (11,37), fac(dfac$(3,1)), partd$(3)           , ch(32),~
               at (13,37), fac(dfac$(4,1)), partd$(4)           , ch(32),~
               at (15,37), fac(dfac$(5,1)), partd$(5)           , ch(32),~
               at (17,37), fac(dfac$(6,1)), partd$(6)           , ch(32),~
               at (19,37), fac(dfac$(7,1)), partd$(7)           , ch(32),~
               at (21,37), fac(dfac$(8,1)), partd$(8)           , ch(32),~
                                                                         ~
               at (07,70), fac(dfac$(1,1)), leadtime$(1)        , ch(11),~
               at (09,70), fac(dfac$(2,1)), leadtime$(2)        , ch(11),~
               at (11,70), fac(dfac$(3,1)), leadtime$(3)        , ch(11),~
               at (13,70), fac(dfac$(4,1)), leadtime$(4)        , ch(11),~
               at (15,70), fac(dfac$(5,1)), leadtime$(5)        , ch(11),~
               at (17,70), fac(dfac$(6,1)), leadtime$(6)        , ch(11),~
               at (19,70), fac(dfac$(7,1)), leadtime$(7)        , ch(11),~
               at (21,70), fac(dfac$(8,1)), leadtime$(8)        , ch(11),~
                                                                         ~
               at (08,10), fac(lfac$(1,3)), vend$ (1)           , ch(09),~
               at (10,10), fac(lfac$(2,3)), vend$ (2)           , ch(09),~
               at (12,10), fac(lfac$(3,3)), vend$ (3)           , ch(09),~
               at (14,10), fac(lfac$(4,3)), vend$ (4)           , ch(09),~
               at (16,10), fac(lfac$(5,3)), vend$ (5)           , ch(09),~
               at (18,10), fac(lfac$(6,3)), vend$ (6)           , ch(09),~
               at (20,10), fac(lfac$(7,3)), vend$ (7)           , ch(09),~
               at (22,10), fac(lfac$(8,3)), vend$ (8)           , ch(09),~
                                                                         ~
               at (08,20), fac(dfac$(1,2)), vendd$(1,tgl%)      , ch(30),~
               at (10,20), fac(dfac$(2,2)), vendd$(2,tgl%)      , ch(30),~
               at (12,20), fac(dfac$(3,2)), vendd$(3,tgl%)      , ch(30),~
               at (14,20), fac(dfac$(4,2)), vendd$(4,tgl%)      , ch(30),~
               at (16,20), fac(dfac$(5,2)), vendd$(5,tgl%)      , ch(30),~
               at (18,20), fac(dfac$(6,2)), vendd$(6,tgl%)      , ch(30),~
               at (20,20), fac(dfac$(7,2)), vendd$(7,tgl%)      , ch(30),~
               at (22,20), fac(dfac$(8,2)), vendd$(8,tgl%)      , ch(30),~
                                                                         ~
               at (08,51), fac(nfac$(1,1)), qty$  (1)           , ch(10),~
               at (10,51), fac(nfac$(2,1)), qty$  (2)           , ch(10),~
               at (12,51), fac(nfac$(3,1)), qty$  (3)           , ch(10),~
               at (14,51), fac(nfac$(4,1)), qty$  (4)           , ch(10),~
               at (16,51), fac(nfac$(5,1)), qty$  (5)           , ch(10),~
               at (18,51), fac(nfac$(6,1)), qty$  (6)           , ch(10),~
               at (20,51), fac(nfac$(7,1)), qty$  (7)           , ch(10),~
               at (22,51), fac(nfac$(8,1)), qty$  (8)           , ch(10),~
                                                                         ~
               at (08,62), fac(nfac$(1,2)), cst$  (1)           , ch(10),~
               at (10,62), fac(nfac$(2,2)), cst$  (2)           , ch(10),~
               at (12,62), fac(nfac$(3,2)), cst$  (3)           , ch(10),~
               at (14,62), fac(nfac$(4,2)), cst$  (4)           , ch(10),~
               at (16,62), fac(nfac$(5,2)), cst$  (5)           , ch(10),~
               at (18,62), fac(nfac$(6,2)), cst$  (6)           , ch(10),~
               at (20,62), fac(nfac$(7,2)), cst$  (7)           , ch(10),~
               at (22,62), fac(nfac$(8,2)), cst$  (8)           , ch(10),~
                                                                         ~
               at (08,73), fac(lfac$(1,4)), odate$(1)           , ch(08),~
               at (10,73), fac(lfac$(2,4)), odate$(2)           , ch(08),~
               at (12,73), fac(lfac$(3,4)), odate$(3)           , ch(08),~
               at (14,73), fac(lfac$(4,4)), odate$(4)           , ch(08),~
               at (16,73), fac(lfac$(5,4)), odate$(5)           , ch(08),~
               at (18,73), fac(lfac$(6,4)), odate$(6)           , ch(08),~
               at (20,73), fac(lfac$(7,4)), odate$(7)           , ch(08),~
               at (22,73), fac(lfac$(8,4)), odate$(8)           , ch(08),~
                                                                         ~
               at (23,02), fac( hex(ac)), blankline$            , ch(79),~
               at (24,02), fac( hex(8c)), selections$(1%)       , ch(79),~
                                                                         ~
                keys(hex(0001020508090d0f10)), key(keyhit%)

                if keyhit% <> 9% then L41150
                     if tgl% = 1% then tgl% = 2% else tgl% = 1%
                     goto L40200

L41150:         if keyhit% <> 13 then L41180
                     call "MANUAL" ("HNYPRCSB")
                     goto L40200

L41180:         if keyhit% <> 15 then L41220
                     call "PRNTSCRN"
                     goto L40200

L41220:        close ws
               call "SCREEN" addr ("C", f1%, "I", i$(), cursor%())
               return

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
