        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  PPPP   IIIII  PPPP   M   M   AAA   PPPP                  *~
            *  P   P    I    P   P  MM MM  A   A  P   P                 *~
            *  PPPP     I    PPPP   M M M  AAAAA  PPPP                  *~
            *  P        I    P      M   M  A   A  P                     *~
            *  P      IIIII  P      M   M  A   A  P                     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * PIPMAP   - PRINTS A MAP OF ALL PIPIN'S, AND PIPOUT'S      *~
            *            SHOWN WEEKLY FOR 13 WEEKS AT A TIME.  USER     *~
            *            SELECTS HOW MANY QUARTERS OUT TO GO OR         *~
            *            WHETHER TO MAP FOR AN ECO CHANGE.              *~
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
            * 08/09/83 ! ORIGINAL                                 ! GLW *~
            * 09/22/86 ! BOMMASTR FORMAT CHANGED                  ! LKM *~
            * 05/14/87 ! Standard Costing Changes                 ! ERN *~
            * 08/22/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* DATE FOR SCREEN DISPLAY    */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            errormsg$79,                 /* ERROR MESSAGE              */~
            i$(24)80,                    /* SCREEN IMAGE               */~
            inpmessage$79,               /* INPUT MESSAGE              */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            lodate$(13)6,                /* For header                 */~
            to$(13)2,                    /* For header                 */~
            hidate$(13)6,                /* For header                 */~
            part$25,                     /* PART                       */~
            partdescr$32,                /* PART                       */~
            pipplow$48,                  /* PIPIN PLOW KEY             */~
            plowcross$47,                /* JBCROSS PLOW KEY           */~
            bom$3, b$3, rte$3, r$3,      /* BILLS AND ROUTES           */~
            pip%(490),                   /* PIP POSITION               */~
            pipplow1$37,                 /* PIPOUT PLOW KEY            */~
            readkey$100,                 /* GP READKEY                 */~
            junk$(2)1,                   /* WORK VARIABLE              */~
            title$79,                    /* SCREEN TITLE               */~
            quarters$1,                  /* NUMBER OF QUARTERS OUT, 1-4*/~
            qt$(13)7,                    /* QUANTS TO PRINT            */~
            stdyymmdd$(490)6,            /* DATE FROM CALENDAR  (STD)  */~
            tag$(1000)23,                /* TAG NUMBERS                */~
            qty(1000,13),                /* QUANTITIES                 */~
            tdate$10,                    /* Temporary Date             */~
            totin(13),                   /* TOTALS IN                  */~
            totout(13),                  /* TOTALS OUT                 */~
            m$66,                        /* SHOW MESSAGE  TEXT         */~
            nfw(13),                     /* NET FOR WEEK               */~
            udate$10,                    /* Temporary Date             */~
            yymmdd$(490)6,               /* DATE FROM CALENDAR         */~
            mwoy%(490),                  /* MONDAY WEEK OF YEAR        */~
            parpart$25, dc$16, dl$3,     /* FOR PEG MAP                */~
            stat$1, type$1, prior$1,     /* FOR PEG MAP                */~
            reqdate$6, demquant$10,      /* FOR PEG MAP                */~
            store$3, dlp$6, pcd$6,       /* FOR PEG MAP                */~
            bomplow$56,                  /* FOR PEG MAP                */~
            p$25, t$19                   /* FROM PIP                   */

        dim f2%(64),                     /* = 0 IF THE FILE IS OPEN    */~
            f1%(64),                     /* = 1 IF READ WAS SUCCESSFUL */~
            rslt$(64)20,                 /* TEXT FROM FILE OPENING     */~
            axd$(64)4                    /* ALT KEY POINTER FROM OPEN'G*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con :
            cms2v$ = "04.17.01 11/17/86 Order processing & planning     "

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* THE FILE OPENING ROUTINES.                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! PIPIN    ! Planned inventory additions detail       *~
            * #02 ! PIPOUT   ! Planned inventory use detail rec         *~
            * #03 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #04 ! HNYMASTR ! Inventory Master File                    *~
            * #05 ! CALMASTR ! Planning Production Calendar File        *~
            * #06 ! JBCROSS2 ! JOB TO BOM/RTE CROSS - JOB TRACKING      *~
            * #07 ! DEMMASTR ! MASTER FILE FOR PLANNING DEMANDS         *~
            * #08 ! BOMMASTR ! BILL OF MATERIALS RELATIONSHIP FILE      *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #01, "PIPIN",                                         ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   60,                                  ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48          ~

            select #08, "BOMMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos =  26, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 56

            select #02, "PIPOUT",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   64,                                  ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37          ~

            select #03, "PIPMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26          ~

            select #04, "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     ~

           select #07, "DEMMASTR", varc, indexed,                        ~
                     recsize = 123 , keypos = 2   , keylen = 27,         ~
                     alt key 1, keypos =10, keylen = 19,                 ~
                         key 2, keypos = 1, keylen = 28

            select #05, "CALMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1962,                                  ~
                        keypos =    1, keylen =   2                      ~

            select #6,  "JBCROSS2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 94,                                    ~
                        keypos=29, keylen = 19,                          ~
                        alt key 1, keypos = 1 , keylen = 47,             ~
                            key 2, keypos = 48, keylen = 47

            call "SHOSTAT" ("Linking to data base for PIP mapping")
            call "OPENFILE" (#01, "SHARE", f2%(01), rslt$(01), axd$(01))
            call "OPENFILE" (#02, "SHARE", f2%(02), rslt$(02), axd$(02))
            call "OPENFILE" (#03, "SHARE", f2%(03), rslt$(03), axd$(03))
            call "OPENFILE" (#04, "SHARE", f2%(04), rslt$(04), axd$(04))
            call "OPENFILE" (#05, "SHARE", f2%(05), rslt$(05), axd$(05))
            call "OPENFILE" (#06, "SHARE", f2%(06), rslt$(06), axd$(06))
            call "OPENFILE" (#07, "SHARE", f2%(07), rslt$(07), axd$(07))
            call "OPENFILE" (#08, "SHARE", f2%(08), rslt$(08), axd$(08))
            if min(f2%()) < 0% then goto L14600

            str(title$,64) = "PIPMAP: " & str(cms2v$,,8)

        REM *************************************************************~
            *                LOAD CALENDAR FOR YYMMDD$ & WW$            *~
            *                                                           *~
            *************************************************************

            call "READ100" (#05, "10", f1%(05))
                if f1%(05) <> 1 then goto L14600
            get #05, using L01890 , str(yymmdd$(),1,1470)
L01890:         FMT XX(2), CH(1470)

            call "READ100" (#05, "11", f1%(05))
                if f1%(05) <> 1 then goto L14600
            get #05, using L01940 , str(yymmdd$(),1471,1470)
L01940:         FMT XX(2), CH(1470)

            for u3% = 1% to 490%
                tdate$ = yymmdd$(u3%)
                call "DATEFMT" ( tdate$, 0%, udate$ )
                stdyymmdd$(u3%) = str( udate$, 3%, 6% )
            next u3%

            call "READ100" (#05, "60", f1%(05))
                if f1%(05) <> 1 then goto L14600
            get #05, using L01990 , mwoy%()
L01990:         FMT XX(2), 490*BI(4)

        REM CALCULATE WHAT WEEK WE ARE IN AND THE FIRST DAY OF THIS WEEK
        inputmode

           date$ = date
           for i = 1 to 490
           if yymmdd$(i) = date$ then goto L02100
           next i
           goto L14600

L02100:
           for j = i to 1 step -1
           if mwoy%(j) <> mwoy%(i) then goto  L02150
           next j

L02150:    fdfw% = j+1

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES INFORMATION NECESSARY FOR PROGRAM.            *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)

            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (ENTER)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * HANDLES NORMAL INPUT FOR DATA ENTRY SCREENS.              *~
            *************************************************************


            init(" ") errormsg$, inpmessage$ , part$, partdescr$,        ~
                     quarters$

            for fieldnr% = 1 to  2
                gosub'051(fieldnr%)
                      if enabled% = 0 then L02480
L02420:         gosub'101(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 and fieldnr% = 1 then L14600
                      if keyhit% <>  0 then       L02420
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L02420
L02480:         next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *                                                           *~
            * HANDLES OPERATION OF EDIT MODE FOR DATA ENTRY SCREENS     *~
            *************************************************************

L02560:     gosub'111(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       L02560
            fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  2 then L02560

L02630:     gosub'111(fieldnr%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L02630
            gosub'151(fieldnr%)
                  if errormsg$ <> " " then L02630
            goto L02560

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *                                                           *~
            *************************************************************
        datasave
           init(" ") m$
           m$ = "Printing PIP map for " & part$
           call "SHOSTAT" (m$)
           mat pip% = zer
           call "READ100" (#3, part$, f1%(3))
           if f1%(3) <> 1% then goto  L02840
           get #3, using L05470   , pip%()


L02840:    qtr = 1
           pagg = 0
           select printer(134)

L02880:    init(" ") tag$()
           mat totin = zer : mat totout = zer : mat qty = zer
           mat nfw = zer
           l, maxl = 0

        REM PLOW FOR THE PIPINS
           init(hex(00)) pipplow$
           put pipplow$, using L02960   , part$, fdfw% - 1%
L02960:    FMT CH(25), BI(4)
L02970:    call "PLOWALTS" (#1, pipplow$, 1%,  25%,  f1%(1))
                     if f1%(1) <> 1% then goto L03140      /* GET PIPOUTS */

           get #1, using L05330    , p$, d%, t$, q
           REM CALC THE WEEK THIS PIPIN IS IN
                     if d% < fdfw% then goto L02970
                     wi = (int((d% - fdfw%)/7)) + 1
                     if wi <= 0 then goto L02970
           if wi > 13 then goto  L03140                    /* GET PIPOUTS */
           l, maxl = l + 1
           if maxl > 1000 then L03100
           str(tag$(l), 5,19) = str(t$,1,19)
           qty(l,wi) = qty(l,wi) + q
L03100:    totin(wi) = totin(wi) + q
           nfw(wi) = nfw(wi) + q
           goto L02970

L03140: REM PLOW PIPOUTS
           lastin  = l + 1
           init(hex(00)) pipplow1$
           put pipplow1$, using L03180   , part$, fdfw% - 1%
L03180:    FMT CH(25), BI(4)
L03190:    call "PLOWALTS" (#2, pipplow1$, 1%,  25%,  f1%(2))
                     if f1%(2) <> 1% then goto L03360      /* GET PIP     */

           get #2, using L05400    , t$,  p$, d%,     q
           REM CALC THE WEEK THIS PIPIN IS IN
                     if d% < fdfw% then goto L03190
                     wi = (int((d% - fdfw%)/7)) + 1
                     if wi <= 0 then goto L03190
           if wi > 13 then goto  L03360                    /* GET PIP     */
           l, maxl = l + 1
           if maxl > 1000 then L03320
           str(tag$(l), 5,19) = str(t$,1,19)
           qty(l,wi) = qty(l,wi) + q
L03320:    totout(wi) = totout(wi) + q
           nfw(wi) = nfw(wi) - q
           goto L03190

L03360: REM PRINT FIRST HEADER, THEN IN, OUT, NET & PIP  SUMMARY
           gosub pagg_control
           init(" ") qt$()
           for k = 1 to 13
           if fdfw%-1% +  (k-1)*7 > 490 then goto L03420
           convert pip%(fdfw%-1% +((k-1)*7)) to qt$(k), pic(-######)
L03420:              next k
           t$ = "BEGINNING PIP      "
           print using L04270    , t$,      qt$(1), qt$(2), qt$(3),        ~
                        qt$(4), qt$(5), qt$(6), qt$(7), qt$(8),          ~
                        qt$(9), qt$(10), qt$(11), qt$(12), qt$(13)

           init(" ") qt$()
           for k = 1 to 13
           if totin( k) = 0 then goto  L03520
              convert totin( k) to qt$(k), pic(-######)
L03520:              next k
           t$ = "      TOTAL SOURCES"
           print using L04270    , t$,      qt$(1), qt$(2), qt$(3),        ~
                        qt$(4), qt$(5), qt$(6), qt$(7), qt$(8),          ~
                        qt$(9), qt$(10), qt$(11), qt$(12), qt$(13)

           init(" ") qt$()
           for k = 1 to 13
                   if totout( k) = 0 then goto  L03620
              convert totout(k) to qt$(k), pic(-######)
L03620:              next k
           t$ = "         TOTAL USES"

           print using L04270    , t$,      qt$(1), qt$(2), qt$(3),        ~
                        qt$(4), qt$(5), qt$(6), qt$(7), qt$(8),          ~
                        qt$(9), qt$(10), qt$(11), qt$(12), qt$(13)


           init(" ") qt$()
           for k = 1 to 13
           if nfw  ( k) = 0 then goto  L03740
              convert nfw  ( k) to qt$(k), pic(-######)
L03740:              next k
           t$ = "       NET FOR WEEK"
           print using L04270    , t$,      qt$(1), qt$(2), qt$(3),        ~
                        qt$(4), qt$(5), qt$(6), qt$(7), qt$(8),          ~
                        qt$(9), qt$(10), qt$(11), qt$(12), qt$(13)

           init(" ") qt$()
           for k = 1 to 13
           if fdfw%+6%  +  (k-1)*7 > 490 then goto L03840
           convert pip%(fdfw%+6% +((k-1)*7)) to qt$(k), pic(-######)
L03840:              next k
           t$ = "ENDING PIP         "
           print using L04270    , t$,      qt$(1), qt$(2), qt$(3),        ~
                        qt$(4), qt$(5), qt$(6), qt$(7), qt$(8),          ~
                        qt$(9), qt$(10), qt$(11), qt$(12), qt$(13)

           print
           pline = pline + 8
           print using  L04250
           l = 1
L03940:    if l <> lastin then goto L03980
           print
           print using L04260
           pline = pline + 2
L03980:    if pline > 60 then gosub pagg_control
           init(" ") qt$()
           for k = 1 to 13
           if qty(l, k) = 0 then goto  L04030
              convert qty(l, k) to qt$(k), pic(-######)
L04030:              next k

           print using L04270    , tag$(l), qt$(1), qt$(2), qt$(3),        ~
                        qt$(4), qt$(5), qt$(6), qt$(7), qt$(8),          ~
                        qt$(9), qt$(10), qt$(11), qt$(12), qt$(13)
           pline = pline + 1
           l = l + 1
            if l < 1001 then L04140
            if maxl < 1001 then L04150
            print using L04240
            goto L04150
L04140:    if l <= maxl then goto L03940
L04150:    qtr = qtr + 1
           if qtr > qtrsout then goto  L04210
           fdfw% = fdfw% + 91%
           if fdfw% > 490 then goto L04210
           goto L02880

L04210:    close printer

           goto inputmode
L04240: % ADDITIONAL PIP DETAIL EXISTS BUT CANNOT BE DISPLAYED
L04250: % SOURCES OF INVENTORY
L04260: % USES OF INVENTORY
L04270: % ####################### ####### ####### ####### ####### #######~
        ~ ####### ####### ####### ####### ####### ####### ####### #######
L04290: % PLANNED INVENTORY POSITION MAP FOR ######################### ##~
        ~############################## AS OF ########           PAGE ###
L04310: %                          ######  ######  ######  ######  ######~
        ~  ######  ######  ######  ######  ######  ######  ######  ######
L04330: %                          ######  ######  ######  ######  ######~
        ~  ######  ######  ######  ######  ######  ######  ######  ######
L04350: %                            ##      ##      ##      ##      ##  ~
        ~    ##      ##      ##      ##      ##      ##      ##      ##
        pagg_control
           print page
           pagg = pagg + 1
           print using L04290, part$, partdescr$, date$, pagg
           print skip(1)
           init(" ") lodate$(), to$(), hidate$()
*       * Beginning Dates
           for k% = 0% to 84% step 7%
                if fdfw% + k% > 490% then L04490
                i% = k% / 7% + 1%
                lodate$(i%) = stdyymmdd$(fdfw% + k%)
                to$(i%) = "TO"
           next k%
L04490:
*       * Ending Dates
           for k% = 6% to 90% step 7%
                if fdfw% + k% > 490% then L04590
                i% = (k% + 1%) / 7%
                hidate$(i%) = stdyymmdd$(fdfw% + k%)
           next k%
L04590:    hidate$(i% + 1%) = "FUTURE"

           print using L04310, lodate$( 1), lodate$( 2), lodate$( 3),      ~
                             lodate$( 4), lodate$( 5), lodate$( 6),      ~
                             lodate$( 7), lodate$( 8), lodate$( 9),      ~
                             lodate$(10), lodate$(11), lodate$(12),      ~
                             lodate$(13)

           print using L04350, to$( 1), to$( 2), to$( 3), to$( 4), to$( 5),~
                             to$( 6), to$( 7), to$( 8), to$( 9), to$(10),~
                             to$(11), to$(12), to$(13)

           print using L04330, hidate$( 1), hidate$( 2), hidate$( 3),      ~
                             hidate$( 4), hidate$( 5), hidate$( 6),      ~
                             hidate$( 7), hidate$( 8), hidate$( 9),      ~
                             hidate$(10), hidate$(11), hidate$(12),      ~
                             hidate$(13)

           print skip(1)
           pline = 6
           return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1
                  on fieldnr% gosub L04950 ,         /* PART             */~
                                    L04970           /* QUARTERS OUT     */
                     return
L04950:     REM DEFAULT/ENABLE FOR PART
                return
L04970:     REM DEFAULT/ENABLE FOR NUMBER OF QUARTERS OUT, 1-4
                quarters$ = "1"
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * OR WILL RETURN USER BACK TO WHERE THEY WERE.  MUST PUSH   *~
            * TWO BUTTONS TO START OVER FOR SAFETY.                     *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *                                                           *~
            * FORMAT STATEMENTS FOR DATA FILES.                         *~
            *************************************************************

L05330: FMT                      /* FILE: PIPIN                        */~
            CH(25),              /* Part code                          */~
            BI(4),               /* Date in subscript for PIP          */~
            CH(19),              /* Tag number in level 2 planning     */~
            PD(14,4)             /* Quantity of something in packed de */


L05400: FMT                      /* FILE: PIPOUT                       */~
            CH(19),              /* Tag number in level 2 planning     */~
            CH(25),              /* Part code                          */~
            BI(4),               /* Date out of PIP in date subscript  */~
            XX(8),               /* Time from the system clock         */~
            PD(14,4)             /* Quantity of something in packed de */~

L05470: FMT                      /* FILE: PIPMASTR                     */~
                                 /* Status indicator for level 2 plann */~
            XX(26),              /* Part code                          */~
            490*BI(4)            /* Planned inventory position         */





        FMT                      /* FILE: HNYMASTR                     */~
            CH(25),              /* part number                        */~
            CH(32),              /* part number description            */~
            CH(16),              /* vendor part number                 */~
            CH(4),               /* sales unit of measure              */~
            CH(4),               /* purchase unit of measure           */~
            PD(14,4),            /* conversion factor (buy to sell)    */~
            CH(4),               /* category code                      */~
            CH(8),               /* alternate part seq. no.            */~
            CH(9),               /* main vendor code                   */~
            XX(7),               /* filler for rest of record or inter */~
            PD(14,4),            /* price base                         */~
            PD(14,4),            /* price - a                          */~
            PD(14,4),            /* price - b                          */~
            PD(14,4),            /* price - c                          */~
            PD(14,4),            /* price - d                          */~
            PD(14,4),            /* price - e                          */~
            CH(4),               /* special/obsolete code              */~
            CH(10),              /* lead time (days)                   */~
            CH(10),              /* Type - used generically for specia */~
            CH(10),              /* minimum order qty. / minimum order */~
            CH(3),               /* buyer/planner code                 */~
            PD(14,4),            /* Standard run quantity for standard */~
            PD(14,4),            /* Material STD cost per unit         */~
            PD(14,4),            /* Labor STD cost per unit            */~
            PD(14,4),            /* Overhead STD per unit              */~
            CH(9),               /* inventory source account           */~
            CH(9),               /* inventory asset account            */~
            CH(9),               /* inventory expense account          */~
            CH(9),               /* Inventory Sales Account            */~
            CH(9),               /* Inventory Variance (material) acco */~
            CH(9),               /* Inventory Variance (Labor) Account */~
            CH(9),               /* Inventory Variance (overhead) Acco */~
            CH(40)               /* filler for rest of record or inter */~


        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L06070 ,         /* PART             */~
                                    L06100           /* QUARTERS OUT     */
                     goto L06140

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L06070:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L06100:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L06140:     accept                                                       ~
               at (01,02),                                               ~
                  "MASTER PLANNED INVENTORY POSITION MAPPING FUNCTIONS", ~
               at (01,66), "Today:", fac(hex(8c)), date$        , ch(08),~
               at (02,02), fac(hex(ac)), title$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "PART Number",                                         ~
               at (06,20), fac(lfac$( 1)), part$                , ch(25),~
               at (06,49), fac(hex(8c)),   partdescr$           , ch(32),~
               at (07,02),                                               ~
                  "# Quarters",                                          ~
               at (07,20), fac(lfac$( 2)), quarters$            , ch(01),~
                                                                         ~
           at(10,02), "Enter a PART number (or a partial PART number or l~
        ~eave blank in order",                                            ~
           at(11,02), "to search for valid PART numbers).  Then enter the~
        ~ number of calendar ",                                           ~
           at(12,02), "quarters to print (0 thru 6).",                   ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,20),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,02),                                               ~
                  "(8)MAP EFFECT OF ECO'S       (12)MAP DEMAND PEGGING", ~
               at (24,65),                                               ~
                  "(16)EXIT PROGRAM",                                    ~
                                                                         ~
               keys(hex(0001080c0d0f10)),                                ~
               key (keyhit%)

               if keyhit% = 1 then gosub startover

               if keyhit% <> 12 then L06570
                  gosub map_demand_pegging
                  goto inputmode

L06570:        if keyhit% <> 8  then L06610
                  gosub map_effect_of_ecos
                  goto inputmode

L06610:        if keyhit% <> 13 then L06650
                  call "MANUAL" ("PIPMAP  ")
                  goto L06140

L06650:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L06140

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'111(fieldnr%)
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L06840 ,         /* PART             */~
                                    L06870           /* QUARTERS OUT     */
                     goto L06910

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L06840:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L06870:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L06910:     accept                                                       ~
               at (01,02),                                               ~
                  "PRINT A PLANNED INVENTORY POSITION MAP",              ~
               at (01,66), "Today:", fac(hex(8c)), date$        , ch(08),~
               at (02,02), fac(hex(ac)), title$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "PART Number",                                         ~
               at (06,20), fac(lfac$( 1)), part$                , ch(25),~
               at (06,49), fac(hex(8c)),   partdescr$           , ch(32),~
               at (07,02),                                               ~
                  "# Quarters",                                          ~
               at (07,20), fac(lfac$( 2)), quarters$            , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   edtmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,25),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,45),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,64),                                               ~
                  "(16)PRINT REPORT",                                    ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L07240
                  call "MANUAL" ("PIPMAP  ")
                  goto L06910

L07240:        if keyhit% <> 15 then L07280
                  call "PRNTSCRN"
                  goto L06910

L07280:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L07430 ,         /* PART             */~
                                    L07480           /* QUARTERS OUT     */
                     return
L07430:     REM TEST DATA FOR PART
           call "GETCODE" (#4, part$, partdescr$, 0%, 0, f1%(4))
           if f1%(4) = 1% then return
           errormsg$ = "NO SUCH PART: " & part$
                return
L07480:     REM TEST DATA FOR NUMBER OF QUARTERS OUT, 1-4
                convert quarters$ to qtrsout   , data goto L07530
                if qtrsout    < 0 then goto   L07530
                if qtrsout    > 6 then goto   L07530
                return
L07530:    errormsg$ = "PLEASE SPECIFY 1 TO 6, NOT: " & quarters$
           return

        map_effect_of_ecos

           init(" ") bom$, rte$, part$, partdescr$

L07600:     accept                                                       ~
               at (01,02),                                               ~
                  "MAP THE EFFECT OF AN ECO ON THE PART AND BOM OR ROUTE ~
        ~SHOWN",                                                          ~
               at (01,66), "Today:", fac(hex(8c)), date$        , ch(08),~
               at (02,02), fac(hex(ac)), title$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "PART Number",                                         ~
               at (06,20), fac(hex(81)),   part$                , ch(25),~
               at (06,49), fac(hex(8c)),   partdescr$           , ch(32),~
               at (07,02),                                               ~
                  "Which BOM?",                                          ~
               at (07,20), fac(hex(81)),  bom$                  , ch(03),~
               at (08,02),                                               ~
                  "Which ROUTE?",                                        ~
               at (08,20), fac(hex(81)),  rte$                  , ch(03),~
                                                                         ~
           at(10,02), "Enter the PART and then either the BOM or the ROUT~
        ~E. Then press (ENTER).",                                         ~
           at(11,02), "All planned or released orders which employ(ed) th~
        ~e BOM or ROUTE will",                                            ~
           at(12,02), "be mapped for you along with all of their associat~
        ~ed PIP actions.",                                                ~
           at(13,02), "If the BOM is blank, the ECO is on the ROUTE.  If ~
        ~the ROUTE is blank",                                             ~
           at(14,02), "then the ECO is on the BOM.",                     ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,20),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,65),                                               ~
                  "(16)RETURN",                                          ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhii%)

               if keyhii% = 1 then gosub startover
               if keyhii% = 1 then L07600

               if keyhii% =  16 then return

               if keyhii% <> 13 then L08110
                  call "MANUAL" ("PIPMAP  ")
                  goto L07600

L08110:        if keyhii% <> 15 then goto L08150
                  call "PRNTSCRN"
                  goto L07600

L08150:    call "GETCODE" (#4, part$, partdescr$, 0%, 0, f1%(4))
           if f1%(4) = 1% then goto L08200
           errormsg$ = "NO SUCH PART: " & part$
           goto L07600

L08200:    m$ = "Gathering jobs using BOM " & bom$ & " or ROUTE " & rte$&~
                     " for " & part$
           call "SHOSTAT" (m$)
           if bom$ = " " then goto  L08690
           firstdate% = 490%
           lastdate% = 1%
           init(" ") plowcross$, errormsg$
           errormsg$ = "NO JOBS ARE PLANNED OR ACTIVE WITH BOM: " & bom$
           plowcross$ = str(part$,1,25) & str(bom$,1,3) & " "
L08290:    call "PLOWALTS" (#6, plowcross$, 2%, 25%, f1%(6))
                     if f1%(6) <> 1% then goto  L08430

           get #6, using L08330, t$
L08330:    FMT XX(28), CH(19)
           call "READ100" (#1, t$, f1%(1))
                     if f1%(1) <> 1% then goto L08290
           get #1, using L08370, d%
L08370:    FMT XX(25), BI(4)
           init(" ") errormsg$
           firstdate% = min(firstdate%, d%)
           lastdate% = max(lastdate%,d%)
           goto L08290

L08430:    if errormsg$ <> " " then goto L07600

           if lastdate% > fdfw% + 90% then goto  L08490
           quarters$ = "1"
           goto print_eco_map

L08490:    if lastdate% > fdfw% +180% then goto L08530
           quarters$ = "2"
           goto print_eco_map

L08530:    if lastdate% > fdfw% +270% then goto L08570
           quarters$ = "3"
           goto print_eco_map

L08570:    if lastdate% > fdfw% +360% then goto L08610
           quarters$ = "4"
           goto print_eco_map

L08610:    if lastdate% > fdfw% +450% then goto  L08650
           quarters$ = "5"
           goto print_eco_map

L08650:    quarters$ = "6"
           goto print_eco_map


L08690:    if rte$ <> " " then goto   L08730
           errormsg$ = "PLEASE SPECIFY EITHER A BOM OR A ROUTE"
           goto L07600

L08730:    firstdate% = 490%
           lastdate% = 1%
           init(" ") plowcross$, errormsg$
           errormsg$ = "NO JOBS ARE PLANNED OR ACTIVE WITH ROUTE: " & rte$
           plowcross$ = str(part$,1,25) & str(rte$,1,3) & " "
L08780:    call "PLOWALTS" (#6, plowcross$, 1%, 25%, f1%(6))
                     if f1%(6) <> 1% then goto  L08920

           get #6, using L08820, t$
L08820:    FMT XX(28), CH(19)
           call "READ100" (#1, t$, f1%(1))
                     if f1%(1) <> 1% then goto L08780
           get #1, using L08860, d%
L08860:    FMT XX(25), BI(4)
           init(" ") errormsg$
           firstdate% = min(firstdate%, d%)
           lastdate% = max(lastdate%,d%)
           goto L08780

L08920:    if errormsg$ <> " " then goto L07600

           if lastdate% > fdfw% + 90% then goto  L08980
           quarters$ = "1"
           goto print_eco_map

L08980:    if lastdate% > fdfw% +180% then goto L09020
           quarters$ = "2"
           goto print_eco_map

L09020:    if lastdate% > fdfw% +270% then goto L09060
           quarters$ = "3"
           goto print_eco_map

L09060:    if lastdate% > fdfw% +360% then goto L09100
           quarters$ = "4"
           goto print_eco_map

L09100:    if lastdate% > fdfw% +450% then goto  L09140
           quarters$ = "5"
           goto print_eco_map

L09140:    quarters$ = "6"
           goto print_eco_map

        REM *************************************************************~
            *             P R I N T   E C O M A P                       *~
            *                                                           *~
            *************************************************************
        print_eco_map
           convert quarters$ to qtrsout
           init(" ") m$
           m$ = "Printing PIP map for " & part$
           call "SHOSTAT" (m$)
           mat pip% = zer
           call "READ100" (#3, part$, f1%(3))
           if f1%(3) <> 1% then goto  L09320
           get #3, using L05470   , pip%()


L09320:    qtr = 1
           pagg = 0
           select printer(134)

L09360:    init(" ") tag$()
           mat totin = zer : mat totout = zer : mat qty = zer
           mat nfw = zer
           l, maxl = 0

        REM PLOW FOR THE PIPINS
           init(hex(00)) pipplow$
           put pipplow$, using L09440   , part$, fdfw% - 1%
L09440:    FMT CH(25), BI(4)
L09450:    call "PLOWALTS" (#1, pipplow$, 1%,  25%,  f1%(1))
                     if f1%(1) <> 1% then goto L09620      /* GET PIPOUTS */

           get #1, using L05330    , p$, d%, t$, q
           REM CALC THE WEEK THIS PIPIN IS IN
                     if d% < fdfw% then goto L09450
                     wi = (int((d% - fdfw%)/7)) + 1
                     if wi <= 0 then goto L09450
           if wi > 13 then goto  L09620                    /* GET PIPOUTS */
           l, maxl = l + 1
           if l > 1000 then L09580
           str(tag$(l), 5,19) = str(t$,1,19)
           qty(l,wi) = qty(l,wi) + q
L09580:    totin(wi) = totin(wi) + q
           nfw(wi) = nfw(wi) + q
           goto L09450

L09620: REM PLOW PIPOUTS
           lastin  = l+1
           init(hex(00)) pipplow1$
           put pipplow1$, using L09660   , part$, fdfw% - 1%
L09660:    FMT CH(25), BI(4)
L09670:    call "PLOWALTS" (#2, pipplow1$, 1%,  25%,  f1%(2))
                     if f1%(2) <> 1% then goto L09840      /* GET PIP     */

           get #2, using L05400    , t$,  p$, d%,     q
           REM CALC THE WEEK THIS PIPIN IS IN
                     if d% < fdfw% then goto L09670
                     wi = (int((d% - fdfw%)/7)) + 1
                     if wi <= 0 then goto L09670
           if wi > 13 then goto  L09840                    /* GET PIP     */
           l, maxl = l + 1
           if l > 1000 then L09800
           str(tag$(l), 5,19) = str(t$,1,19)
           qty(l,wi) = qty(l,wi) + q
L09800:    totout(wi) = totout(wi) + q
           nfw(wi) = nfw(wi) - q
           goto L09670

L09840: REM PRINT FIRST HEADER, THEN IN, OUT, NET & PIP  SUMMARY
           gosub paee_control
           init(" ") qt$()
           for k = 1 to 13
           if fdfw%-1% +  (k-1)*7 > 490 then goto L09900
           convert pip%(fdfw%-1% +((k-1)*7)) to qt$(k), pic(-######)
L09900:              next k
           t$ = "BEGINNING PIP      "
           print using L10810   , t$,      qt$(1), qt$(2), qt$(3),        ~
                        qt$(4), qt$(5), qt$(6), qt$(7), qt$(8),          ~
                        qt$(9), qt$(10), qt$(11), qt$(12), qt$(13)

           init(" ") qt$()
           for k = 1 to 13
           if totin( k) = 0 then goto  L10000
              convert totin( k) to qt$(k), pic(-######)
L10000:              next k
           t$ = "      TOTAL SOURCES"
           print using L10810   , t$,      qt$(1), qt$(2), qt$(3),        ~
                        qt$(4), qt$(5), qt$(6), qt$(7), qt$(8),          ~
                        qt$(9), qt$(10), qt$(11), qt$(12), qt$(13)

           init(" ") qt$()
           for k = 1 to 13
                   if totout( k) = 0 then goto  L10100
              convert totout(k) to qt$(k), pic(-######)
L10100:              next k
           t$ = "         TOTAL USES"

           print using L10810   , t$,      qt$(1), qt$(2), qt$(3),        ~
                        qt$(4), qt$(5), qt$(6), qt$(7), qt$(8),          ~
                        qt$(9), qt$(10), qt$(11), qt$(12), qt$(13)


           init(" ") qt$()
           for k = 1 to 13
           if nfw  ( k) = 0 then goto  L10220
              convert nfw  ( k) to qt$(k), pic(-######)
L10220:              next k
           t$ = "       NET FOR WEEK"
           print using L10810   , t$,      qt$(1), qt$(2), qt$(3),        ~
                        qt$(4), qt$(5), qt$(6), qt$(7), qt$(8),          ~
                        qt$(9), qt$(10), qt$(11), qt$(12), qt$(13)

           init(" ") qt$()
           for k = 1 to 13
           if fdfw%+6%  +  (k-1)*7 > 490 then goto L10320
           convert pip%(fdfw%+6% +((k-1)*7)) to qt$(k), pic(-######)
L10320:              next k
           t$ = "ENDING PIP         "
           print using L10810   , t$,      qt$(1), qt$(2), qt$(3),        ~
                        qt$(4), qt$(5), qt$(6), qt$(7), qt$(8),          ~
                        qt$(9), qt$(10), qt$(11), qt$(12), qt$(13)

           print
           pline = pline + 8
           print using  L10790
           l = 1
L10420:    if l <> lastin then goto L10460
           print
           print using L10800
           pline = pline + 2
L10460:    if pline > 60 then gosub paee_control
           init(" ") qt$()
           for k = 1 to 13
           if qty(l, k) = 0 then goto  L10510
              convert qty(l, k) to qt$(k), pic(-######)
L10510:              next k
           call "READ100" (#6, str(tag$(l),5,19), f1%(6))
                     if f1%(6) <> 1% then goto L10590
           get #6, using L10570, r$, b$
           if r$ = rte$ then str(tag$(l),1,1) = "*"
           if b$ = bom$ then str(tag$(l),1,1) = "*"
L10570:    FMT XX(25), CH(3), XX(44), CH(3)

L10590:    print using L10810   , tag$(l), qt$(1), qt$(2), qt$(3),        ~
                        qt$(4), qt$(5), qt$(6), qt$(7), qt$(8),          ~
                        qt$(9), qt$(10), qt$(11), qt$(12), qt$(13)
           pline = pline + 1
           l = l + 1
            if l < 1001 then L10680
            if maxl < 1001 then L10690
            print using L10780
            goto L10690
L10680:    if l <= maxl then goto L10420
L10690:    qtr = qtr + 1
           if qtr > qtrsout then goto  L10750
           fdfw% = fdfw% + 91%
           if fdfw% > 490 then goto L10750
           goto L09360

L10750:    close printer
           goto map_effect_of_ecos

L10780: % ADDITIONAL PIP DETAIL EXISTS BUT CANNOT BE SHOWN
L10790: % SOURCES OF INVENTORY
L10800: % USES OF INVENTORY
L10810: % ####################### ####### ####### ####### ####### #######~
        ~ ####### ####### ####### ####### ####### ####### ####### #######
L10830: % EFFECT OF AN ECO CHANGE ON ######################### ##########~
        ~###################### AS OF ########                   PAGE ###
L10850: % PLANNED JOBS WHICH USE BOM ### OR RTE ### ARE MARKED WITH A '*'~
        ~ BELOW
L10870: %                          ######  ######  ######  ######  ######~
        ~  ######  ######  ######  ######  ######  ######  ######  ######
L10890: %                          ######  ######  ######  ######  ######~
        ~  ######  ######  ######  ######  ######  ######  ######  ######
L10910: %                            TO      TO      TO      TO      TO  ~
        ~    TO      TO      TO      TO      TO      TO      TO      TO
        paee_control
           print page
           pagg = pagg + 1
           print using L10830  , part$ , partdescr$, date$, pagg
           print using L10850, bom$, rte$
           print
           print using L10870 , stdyymmdd$(fdfw% + 0%), stdyymmdd$(fdfw% + 7%),~
                                stdyymmdd$(fdfw% +14%), stdyymmdd$(fdfw% +21%),~
                                stdyymmdd$(fdfw% +28%), stdyymmdd$(fdfw% +35%),~
                                stdyymmdd$(fdfw% +42%), stdyymmdd$(fdfw% +49%),~
                                stdyymmdd$(fdfw% +56%), stdyymmdd$(fdfw% +63%),~
                                stdyymmdd$(fdfw% +70%), stdyymmdd$(fdfw% +77%),~
                                stdyymmdd$(fdfw% +84%)
           print using L10910
           print using L10890 , stdyymmdd$(fdfw% + 6%), stdyymmdd$(fdfw% +13%),~
                                stdyymmdd$(fdfw% +20%), stdyymmdd$(fdfw% +27%),~
                                stdyymmdd$(fdfw% +34%), stdyymmdd$(fdfw% +41%),~
                                stdyymmdd$(fdfw% +48%), stdyymmdd$(fdfw% +57%),~
                                stdyymmdd$(fdfw% +62%), stdyymmdd$(fdfw% +69%),~
                                stdyymmdd$(fdfw% +76%), stdyymmdd$(fdfw% +83%),~
                                stdyymmdd$(fdfw% +90%)
           print
           pline = 7
           return


        map_demand_pegging

           init(" ") dc$, dl$, stat$, type$, prior$, reqdate$, demquant$,~
                     store$, dlp$, pcd$, bomplow$, parpart$, errormsg$

L11240:     accept                                                       ~
               at (01,02),                                               ~
                  "MAP PEGGING OF EVENTS LEADING TO FULFILLMENT OF A DEMA~
        ~ND",                                                             ~
               at (01,66), "Today:", fac(hex(8c)), date$        , ch(08),~
               at (02,02), fac(hex(ac)), title$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (06,02),                                               ~
                  "Demand code",                                         ~
               at (06,20), fac(hex(81)),   dc$                  , ch(16),~
               at (07,02),                                               ~
                  "Demand line",                                         ~
               at (07,20), fac(hex(81)),  dl$                   , ch(03),~
                                                                         ~
           at(10,02), "Enter the demand code and line for the demand you ~
        ~wish to map and",                                                ~
           at(11,02), "press (ENTER).  The week in which this demand is p~
        ~lanned to be",                                                   ~
           at(12,02), "fulfilled will be shown as week TEN.   All associa~
        ~ted actions",                                                    ~
           at(13,02), "for the demand PART and all components will be sho~
        ~wn on the printed",                                              ~
           at(14,02), "reports.",                                        ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02),                                               ~
                  "P.F. KEYS ACTIVE:",                                   ~
               at (23,02),                                               ~
                  "(1)START OVER",                                       ~
               at (23,20),                                               ~
                  "(13)INSTRUCTIONS",                                    ~
               at (23,65),                                               ~
                  "(15)PRINT SCREEN",                                    ~
               at (24,65),                                               ~
                  "(16)RETURN",                                          ~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhii%)

               if keyhii% = 1 then gosub startover
               if keyhii% = 1 then L11240

               if keyhii% =  16 then return

               if keyhii% <> 13 then L11710
                  call "MANUAL" ("PIPMAP  ")
                  goto L11240

L11710:        if keyhii% <> 15 then goto L11750
                  call "PRNTSCRN"
                  goto L11240

L11750:    call "FMTTITLE" (dl$, " ", 1%)
L11760:    call "REDALT0" (#7, str(dc$,1,16) & str(dl$,1,3), 1%,  f1%(7))
           if f1%(7) = 1% then goto L11890
                  readkey$ = " "
                  call "PLOWCODE" (#7, readkey$, " ", 2016%, -1, f1%(7), ~
                                                           junk$(), 0.25)
                     if f1%(7) = 0 then L11860
                  dc$ = str(readkey$,,16)
                  dl$ = str(readkey$,17,3)
                  goto L11760

L11860:    errormsg$ = "NO SUCH DEMAND ON FILE: " & dc$ & dl$
           goto L11240

L11890:    get #7, using L11910, stat$, type$, prior$, reqdate$, part$,   ~
                     demquant$, bom$,    store$, dlp$, pcd$
L11910:    FMT CH(1), CH(1), CH(1), CH(6), XX(19), CH(25), CH(10), XX(04)~
           ,CH(3), XX(3), CH(3), CH(6), CH(6)

           if bom$ = " " then bom$ = "001"

           for i = 1 to 490
           if yymmdd$(i) = pcd$ then goto  L12030
           next i
           errormsg$ = "THIS DEMAND IS NOT PLANNED FOR COMPLETION WITHIN ~
        ~THE PLANNING PERIOD"
           goto L11240

L12030:    for j = i to 1 step -1
           if mwoy%(j) <> mwoy%(i) then goto  L12070
           next j

L12070:    fdfw% = j+1

           if fdfw% - 70% <= 0% then goto L12130
           fdfw% = fdfw% - 70%
           goto L12480

L12130:    if fdfw% - 63% <= 0% then goto L12170
           fdfw% = fdfw% - 63%
           goto                L12480

L12170:    if fdfw% - 56% <= 0% then goto L12210
           fdfw% = fdfw% - 56%
           goto           L12480

L12210:    if fdfw% - 49% <= 0% then goto L12250
           fdfw% = fdfw% - 49%
           goto      L12480

L12250:    if fdfw% - 42% <= 0% then goto L12290
           fdfw% = fdfw% - 42%
           goto L12480

L12290:    if fdfw% - 30% <= 0% then goto L12330
           fdfw% = fdfw% - 30%
           goto  L12480

L12330:    if fdfw% - 28% <= 0% then goto L12370
           fdfw% = fdfw% - 28%
           goto L12480

L12370:    if fdfw% - 21% <= 0% then goto L12410
           fdfw% = fdfw% - 21%
           goto L12480

L12410:    if fdfw% - 14% <= 0% then goto L12450
           fdfw% = fdfw% - 14%
           goto L12480

L12450:    if fdfw% -  7% <= 0% then goto L12480
           fdfw% = fdfw% -  7%

L12480: REM FIRST MAP FOR THE PARENT PART
           qtrsout = 1
           parpart$ = part$
           gosub loop_map
        REM NOW MAP EACH OF THE COMPONENTS IN TURN

           bomplow$ = str(part$,1,25) & str(bom$,1,3) & " "
L12550:    call "PLOWNEXT" (#8, bomplow$, 25%, f1%(8))
           if f1%(8) <> 1% then goto L12630
           if str(bomplow$,29,3) = "  0" then L12550
           get #8, using L12590, part$
L12590:    FMT CH(25)
           gosub loop_map
           goto L12550

L12630:    close printer
           goto map_demand_pegging

        loop_map
           init(" ") m$
           m$ = "Printing pegging map for part " & part$
           call "SHOSTAT" (m$)
           mat pip% = zer
           call "READ100" (#3, part$, f1%(3))
           if f1%(3) <> 1% then goto  L12760
           get #3, using L05470   , pip%()


L12760:    qtr = 1
           paaa = 0
           select printer(134)

L12800:    init(" ") tag$()
           mat totin = zer : mat totout = zer : mat qty = zer
           mat nfw = zer
           l, maxl = 0

        REM PLOW FOR THE PIPINS
           init(hex(00)) pipplow$
           put pipplow$, using L12880  , part$, fdfw% - 1%
L12880:    FMT CH(25), BI(4)
L12890:    call "PLOWALTS" (#1, pipplow$, 1%,  25%,  f1%(1))
                     if f1%(1) <> 1% then goto L13060     /* GET PIPOUTS */

           get #1, using L05330    , p$, d%, t$, q
           REM CALC THE WEEK THIS PIPIN IS IN
                     if d% < fdfw% then goto L12890
                     wi = (int((d% - fdfw%)/7)) + 1
                     if wi <= 0 then goto L12890
           if wi > 13 then goto  L13060                   /* GET PIPOUTS */
           l, maxl = l + 1
           if l > 1000 then L13020
           str(tag$(l), 5,19) = str(t$,1,19)
           qty(l,wi) = qty(l,wi) + q
L13020:    totin(wi) = totin(wi) + q
           nfw(wi) = nfw(wi) + q
           goto L12890

L13060: REM PLOW PIPOUTS
           lastin  = l + 1
           init(hex(00)) pipplow1$
           put pipplow1$, using L13100  , part$, fdfw% - 1%
L13100:    FMT CH(25), BI(4)
L13110:    call "PLOWALTS" (#2, pipplow1$, 1%,  25%,  f1%(2))
                     if f1%(2) <> 1% then goto L13310     /* GET PIP     */

           get #2, using L05400    , t$,  p$, d%,     q
           REM CALC THE WEEK THIS PIPIN IS IN
                     if d% < fdfw% then goto L13110
                     wi = (int((d% - fdfw%)/7)) + 1
                     if wi <= 0 then goto L13110
           if wi > 13 then goto  L13310                   /* GET PIP     */
           l, maxl = l + 1
           if l > 1000 then L13270
           str(tag$(l), 5,19) = str(t$,1,19)
           if str(t$,1,16) <> str(dc$,1,16) then goto L13260
           if str(t$,17,3) <> str(dl$,1,3) then goto L13260
           str(tag$(l),1,3) = ">>>"
L13260:    qty(l,wi) = qty(l,wi) + q
L13270:    totout(wi) = totout(wi) + q
           nfw(wi) = nfw(wi) - q
           goto L13110

L13310: REM PRINT FIRST HEADER, THEN IN, OUT, NET & PIP  SUMMARY
           gosub paaa_control
           init(" ") qt$()
           for k = 1 to 13
           if fdfw%-1% +  (k-1)*7 > 490 then goto L13370
           convert pip%(fdfw%-1% +((k-1)*7)) to qt$(k), pic(-######)
L13370:              next k
           t$ = "BEGINNING PIP      "
           print using L14220   , t$,      qt$(1), qt$(2), qt$(3),        ~
                        qt$(4), qt$(5), qt$(6), qt$(7), qt$(8),          ~
                        qt$(9), qt$(10), qt$(11), qt$(12), qt$(13)

           init(" ") qt$()
           for k = 1 to 13
           if totin( k) = 0 then goto  L13470
              convert totin( k) to qt$(k), pic(-######)
L13470:              next k
           t$ = "      TOTAL SOURCES"
           print using L14220   , t$,      qt$(1), qt$(2), qt$(3),        ~
                        qt$(4), qt$(5), qt$(6), qt$(7), qt$(8),          ~
                        qt$(9), qt$(10), qt$(11), qt$(12), qt$(13)

           init(" ") qt$()
           for k = 1 to 13
                   if totout( k) = 0 then goto  L13570
              convert totout(k) to qt$(k), pic(-######)
L13570:              next k
           t$ = "         TOTAL USES"

           print using L14220   , t$,      qt$(1), qt$(2), qt$(3),        ~
                        qt$(4), qt$(5), qt$(6), qt$(7), qt$(8),          ~
                        qt$(9), qt$(10), qt$(11), qt$(12), qt$(13)


           init(" ") qt$()
           for k = 1 to 13
           if nfw  ( k) = 0 then goto  L13690
              convert nfw  ( k) to qt$(k), pic(-######)
L13690:              next k
           t$ = "       NET FOR WEEK"
           print using L14220   , t$,      qt$(1), qt$(2), qt$(3),        ~
                        qt$(4), qt$(5), qt$(6), qt$(7), qt$(8),          ~
                        qt$(9), qt$(10), qt$(11), qt$(12), qt$(13)

           init(" ") qt$()
           for k = 1 to 13
           if fdfw%+6%  +  (k-1)*7 > 490 then goto L13790
           convert pip%(fdfw%+6% +((k-1)*7)) to qt$(k), pic(-######)
L13790:              next k
           t$ = "ENDING PIP         "
           print using L14220   , t$,      qt$(1), qt$(2), qt$(3),        ~
                        qt$(4), qt$(5), qt$(6), qt$(7), qt$(8),          ~
                        qt$(9), qt$(10), qt$(11), qt$(12), qt$(13)

           print
           pline = pline + 8
           print using  L14200
           l = 1
L13890:    if l <> lastin then goto L13930
           print
           print using L14210
           pline = pline + 2
L13930:    if pline > 60 then gosub paaa_control
           init(" ") qt$()
           for k = 1 to 13
           if qty(l, k) = 0 then goto  L13980
              convert qty(l, k) to qt$(k), pic(-######)
L13980:              next k

           print using L14220   , tag$(l), qt$(1), qt$(2), qt$(3),        ~
                        qt$(4), qt$(5), qt$(6), qt$(7), qt$(8),          ~
                        qt$(9), qt$(10), qt$(11), qt$(12), qt$(13)
           pline = pline + 1
           l = l + 1
           if l < 1001 then L14090
            if maxl < 1001 then L14100
            print using L14190
            goto L14100
L14090:    if l <= maxl then goto L13890
L14100:    qtr = qtr + 1
           if qtr > qtrsout then goto  L14160
           fdfw% = fdfw% + 91%
           if fdfw% > 490 then goto L14160
           goto L12800

L14160:    return


L14190: % ADDITIONAL PIP DETAIL EXISTS BUT CANNOT BE DISPLAYED
L14200: % SOURCES OF INVENTORY
L14210: % USES OF INVENTORY
L14220: % ####################### ####### ####### ####### ####### #######~
        ~ ####### ####### ####### ####### ####### ####### ####### #######
L14240: % MASTER PEGGING MAP FOR DEMAND ################ ###, STAT = # TYPE~
        ~ = # PRIOR = # REQ DATE = ######## PLN DATE = ########    PAGE ###
L14260: % DEMAND FOR ########## ######################### TO SHIP FROM WH~
        ~SE ### LAST PLANNED ########  (THIS IS PART ######################)
L14280: %                          ######  ######  ######  ######  ######~
        ~  ######  ######  ######  ######  ######  ######  ######  ######
L14300: %                          ######  ######  ######  ######  ######~
        ~  ######  ######  ######  ######  ######  ######  ######  ######
L14320: %                            TO      TO      TO      TO      TO  ~
        ~    TO      TO      TO      TO      TO      TO      TO      TO
        paaa_control
           print page
           paaa = paaa + 1
           tdate$ = reqdate$ : call "DATEFMT" (tdate$)
           udate$ = pcd$     : call "DATEFMT" (udate$)
           print using L14240  , dc$, dl$, stat$, type$, prior$, tdate$,~
                     udate$, paaa
           tdate$ = dlp$ : call "DATEFMT" (tdate$)
           print using L14260, demquant$, parpart$, store$, tdate$, part$
           print
           print using L14280 , stdyymmdd$(fdfw% + 0%), stdyymmdd$(fdfw% + 7%),~
                                stdyymmdd$(fdfw% +14%), stdyymmdd$(fdfw% +21%),~
                                stdyymmdd$(fdfw% +28%), stdyymmdd$(fdfw% +35%),~
                                stdyymmdd$(fdfw% +42%), stdyymmdd$(fdfw% +49%),~
                                stdyymmdd$(fdfw% +56%), stdyymmdd$(fdfw% +63%),~
                                stdyymmdd$(fdfw% +70%), stdyymmdd$(fdfw% +77%),~
                                stdyymmdd$(fdfw% +84%)
           print using L14320
           print using L14300 , stdyymmdd$(fdfw% + 6%), stdyymmdd$(fdfw% +13%),~
                                stdyymmdd$(fdfw% +20%), stdyymmdd$(fdfw% +27%),~
                                stdyymmdd$(fdfw% +34%), stdyymmdd$(fdfw% +41%),~
                                stdyymmdd$(fdfw% +48%), stdyymmdd$(fdfw% +57%),~
                                stdyymmdd$(fdfw% +62%), stdyymmdd$(fdfw% +69%),~
                                stdyymmdd$(fdfw% +76%), stdyymmdd$(fdfw% +83%),~
                                stdyymmdd$(fdfw% +90%)
           print
           pline = 7
           return

L14600: REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS****~
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

            call "SHOSTAT" ("Closing Files, One Moment Please")
            end
