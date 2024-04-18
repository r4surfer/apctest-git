        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  JJJJJ  BBBB   PPPP    OOO   TTTTT  PPPP   RRRR   TTTTT   *~
            *    J    B   B  P   P  O   O    T    P   P  R   R    T     *~
            *    J    BBBB   PPPP   O   O    T    PPPP   RRRR     T     *~
            *  J J    B   B  P      O   O    T    P      R  R     T     *~
            *   J     BBBB   P       OOO     T    P      R   R    T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBPOTPRT - Master formula calculator to pick the materials*~
            *            required for a job and prints the pick list    *~
            *            from existing lots.                            *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/08/83 ! ORIGINAL                                 ! ECR *~
            * 09/20/84 ! EXTENSIVE PRINT FORMAT CHANGES           ! JER *~
            * 02/20/87 ! Massive rewrite to current software level! MJB *~
            * 07/02/87 ! File format changes for standard cost;   ! JIM *~
            *          !   PIPMASTR, JBMATER2, WCOUT files removed!     *~
            * 03/23/89 ! Corrected program name on screens        ! MLJ *~
            * 03/24/89 ! Chg'd Balance Needed to display 0.00 when!     *~
            *          ! Eff Qty > Qty Avail and Eff Iss decimal  !     *~
            *          ! value .001 - .999                        ! MLJ *~
            * 11/03/89 ! Added File select for Job & Assy/BOM     ! MJB *~
            * 03/21/90 ! Added Multiple Bin Sort & Print Options. ! JEF *~
            * 05/01/91 !(PRR 11402) Major Overhaul of DELETELINE  ! RJB *~
            *          !     and QTY TEST Sections.               !     *~
            *************************************************************
        dim                                                              ~
            afac$(12)1,                                                  ~
            amp$1,                       /* "@"                        */~
            bin$8,                       /* Bin or Location for Lot    */~
            bin_loc$(100)20,             /* Array of possible Locations*/~
            colttl$79,                   /* Column Titles on Comp Scrn */~
            company$60,                  /* Company name for title     */~
            date$8,                      /* Date for screen display    */~
            errormsg$79,                 /* Error message              */~
            eff_deleted$10,              /* Lot's Effect. Qty Deleted  */~
            inpmessage$79,               /* Input message              */~
            inc_loc$1,                   /* Include HNYLOCNS File Flag */~
            index%(1),                   /* Index for Searches         */~
            infomsg$79,                  /* Info  message              */~
            hdr$(3)80,                   /* Lines for PLOWCODE         */~
            bomhdr$(3)80,                /* Lines for PLOWCODE (BOM)   */~
            loc_hd$(3)8,                 /* Col. Headings for locations*/~
            lot$6,                       /* Temporary Lot Code         */~
            lot_deleted$6,               /* Lot # of Line Deleted      */~
            old_store$3,                 /* Temporary Store Code       */~
            prtbin$1,                    /* Print Location Flag        */~
            parr$(200)25,                                                ~
            pf16$16,                     /* PF-16 Literal              */~
            plowdescr$80,                                                ~
            plowkey$50,                  /* Key for PLOW's             */~
            dsc_map(12),                 /* Description Map for PLOW   */~
            bomkey$57,                   /* BOMMASTR, BOMTEXT READKEY  */~
            bomqty$(200)10,              /* % per volume 0 - 100.0000% */~
            cursor%(2),                                                  ~
            edtmessage$79,               /* Edit screen message        */~
            effqty$(200,12)10,                                           ~
            effkit$(200,12)10,                                           ~
            factor(200,12),              /* Potency factors            */~
            oldfactor(200,12),           /* Potency factors            */~
            header$80,                                                   ~
            i$(24)80,                                                    ~
            i_x(2),                                                      ~
            i_x$(2)8,                                                    ~
            jbbomid$3,                   /* BOM ID eff when job planned*/~
            jbbomtext$26,                /* BOM ID text                */~
            jbdeaclass$4,                /* Job drug class (FDA)       */~
            jbdescr$32,                  /* Job description            */~
            jbnr$8,                                                      ~
            jbparr$25,                                                   ~
            jbparrdescr$32,                                              ~
            jbparrunit$4,                /* Unit of measure of job part*/~
            jbquantitt$10,                                               ~
            jbuomdescr$30,                                               ~
            lfac$( 9)1,                  /* FACS for pick screen detail*/~
            lfff$(15,3)1,                                                ~
            lpck$( 9)1,                  /* FACS for pick screen block */~
            line%(200),                                                  ~
            line2$79,                    /* Screen Line 2              */~
            line_cnt%(200),              /* Line Counter for Append    */~
            lpprt$25,                                                    ~
            ltt$(200,12)6,                                               ~
            parrdeaclass$(200)4,         /* DEA class of drug          */~
            parrdescr$(200)34,                                           ~
            parunit$(200)4,              /* Unit of measure for comps. */~
            pick$(200)1,                 /* Pick tabstop               */~
            pipott$(200)64,              /* Pipotts                    */~
            pot$(200,12)6,                                               ~
            potmult$(200,12)10,                                          ~
            potency(200,12),             /* Lot potencies              */~
            potunit$(200)8,              /* Standard potency units     */~
            qtt(200,12),                                                 ~
            quantitt$(200,12)10,         /* Quantity                   */~
            qoh(200,12),                 /* Quantity on hand for lot   */~
            qtyoh$(200,12)10,            /* Quantity on hand for lot   */~
            remquann$(200)10,                                            ~
            requann$(200)10,                                             ~
            stdate$8,                    /* Start date                 */~
            stdpot (200),                /* Standard potency from bom  */~
            stdpot$(200)6,               /* Standard potency from bom  */~
            store$3,                     /* Current Store Code         */~
            sysdate$6,                                                   ~
            taanr$56,                    /* Plowtag                    */~
            temp$25,                                                     ~
            textidh$4, textidc$(200)4,   /* Keys to text file          */~
            traa$80,                                                     ~
            total(3),                    /* Quantity Totals            */~
            total$(3)13,                 /* Quantity Totals            */~
            tpckqty$(200)10,             /* Total pick qty = req - remq*/~
            uomdescr$(200)30,            /* UOM Literal                */~
            warnmsg$79,                  /* Over Issue Warning Message */~
            whss$(200,12)3,              /* Warehouse                  */~
            wipacct$9,  userid$,                                         ~
            rftg$19, prtqty$15, prtqty1$15,                              ~
                                                                         ~
            readkey$34,                  /* HNYQUAN READKEY            */~
            expdate$(200,12)8            /* Expiration dates           */

        dim f2%(36),                     /* = 0 if the file is open    */~
            f1%(36),                     /* = 1 if READ was successful */~
            fs%(36),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(36)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.01.00 10/07/91 CMS General Release             "
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
            *   3 ! HNYMASTR ! Inventory master file                    *~
            *   4 ! JBMASTR2 ! Job Master File                          *~
            *   7 ! JBCREDIT ! Job finished goods ledger                *~
            *   6 ! STORNAME ! Store Master File                        *~
            *   8 ! GENCODES ! General Codes File                       *~
            *  11 ! HNYQUAN  ! Inventory quantity  file                 *~
            *  12 ! TXTFILE  ! System Text File                         *~
            *  13 ! HNYLOCNS ! Stock Location Master File               *~
            *  14 ! SYSFILE2 ! System Information File                  *~
            *  21 ! JBCROSS2 ! Cross reference of RTE & BOM planned for *~
            *  30 ! BOMMASTR ! BOM relationship file                    *~
            *  34 ! PIPOUT   ! Planned position out                     *~
            *  36 ! JBPIPXRF ! Job PIP xref                             *~
            *************************************************************~
            *                                                           *~
            *       FILE SELECTION AND OPEN CALLS                       *

            select #3, "HNYMASTR",                                       ~
                       varc,  indexed,  recsize = 900,                   ~
                       keypos = 1, keylen =   25,                        ~
                       alt key  1, keypos =  102, keylen =   9, dup,     ~
                           key  2, keypos =   90, keylen =   4, dup

            select #4, "JBMASTR2",                                       ~
                       varc,  indexed,  recsize = 1300,                  ~
                       keypos = 1, keylen = 8

            select #6,  "STORNAME",                                      ~
                        varc,  indexed,  recsize =  300,                 ~
                        keypos = 1, keylen = 3

            select #7,  "JBCREDIT",                                      ~
                        varc,  indexed,  recsize =  500,                 ~
                        keypos = 1, keylen = 22,                         ~
                        alt key  1, keypos = 23, keylen = 48

            select # 8, "GENCODES",                                      ~
                        varc, indexed, recsize = 128,                    ~
                        keypos =    1,  keylen = 24

            select #11, "HNYQUAN",                                       ~
                        varc,  indexed,  recsize =  650,                 ~
                        keypos = 17, keylen =  44,                       ~
                        alt key   1, keypos =   1, keylen =   44

            select #12, "TXTFILE",                                       ~
                         varc, indexed,  recsize = 2024,                 ~
                         keypos =  1, keylen =  11

            select #13, "HNYLOCNS",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =    1, keylen =  42,                     ~
                        alt key  1, keypos =  443, keylen =  42,         ~
                            key  2, keypos =  485, keylen =  42,         ~
                            key  3, keypos =  527, keylen =  42,         ~
                            key  4, keypos =  590, keylen =  42          ~

            select #14, "SYSFILE2",                                      ~
                        varc, indexed, recsize = 500,                    ~
                        keypos =      1, keylen =  20

            select #21, "JBCROSS2",                                      ~
                        varc,  indexed,  recsize =  94,                  ~
                        keypos =   29, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  47,         ~
                            key  2, keypos =   48, keylen =  47

            select #30, "BOMMASTR",                                      ~
                        varc,  indexed,  recsize = 150,                  ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  56

            select #34, "PIPOUT",                                        ~
                        varc,  indexed,  recsize =  64,                  ~
                        keypos = 1, keylen = 56,                         ~
                        alt  key 1, keypos = 20, keylen = 37

            select #36, "JBPIPXRF",                                      ~
                        varc,  indexed,  recsize =  63,                  ~
                        keypos = 1, keylen = 63,                         ~
                        alt key  1, keypos = 45, keylen=19


            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 3, fs%( 3), f2%( 3), 0%, rslt$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6), 0%, rslt$( 6))
            call "OPENCHCK" (# 7, fs%( 7), f2%( 7), 0%, rslt$( 7))
            call "OPENCHCK" (# 8, fs%( 8), f2%( 8), 0%, rslt$( 8))
            call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))
            call "OPENCHCK" (#13, fs%(13), f2%(13), 0%, rslt$(13))
            call "OPENCHCK" (#14, fs%(14), f2%(14), 0%, rslt$(14))
            call "OPENCHCK" (#21, fs%(21), f2%(21), 0%, rslt$(21))
            call "OPENCHCK" (#30, fs%(30), f2%(30), 0%, rslt$(30))
            call "OPENCHCK" (#34, fs%(34), f2%(34), 0%, rslt$(34))
            call "OPENCHCK" (#36, fs%(36), f2%(36), 0%, rslt$(36))


        REM *************************************************************~
            *              I N I T I A L I Z A T I O N                  *~
            *-----------------------------------------------------------*~
            * Initializes information required for the program          *~
            *************************************************************

            select printer(134)
            sysdate$ = date
            date$ = sysdate$
            call "DATEFMT" (date$)
            call "EXTRACT" addr ("ID", userid$)
            amp$ = "@"
            call "COMPNAME" (12%, company$, u3%) : u3% = 0% : hdr% = 0%
            edtmessage$ = "To Modify Displayed Values, Position Cursor To~
        ~ Desired Value And Press (RETURN)."

            header$ = "Str Lot    Exp Date"
            str(header$,22) = "Qty Avail @ Potency"
            str(header$,44) = "Eff Qty"
            str(header$,53) = "Qty to Kit"
            str(header$,65) = "Eff Issued"

            colttl$ = "  Part Number"
            str(colttl$,30)  = "Std Pot"
            str(colttl$,40)  = "Qty/Unit"
            str(colttl$,50)  = "Total Req'd"
            str(colttl$,68)  = "Issued"
            str(colttl$,76)  = "UOM"

            init (hex(00)) traa$
            init (hex(01)) str(traa$, 1%, 6%)
            init (hex(02)) str(traa$, 7%, 16%)
            init (hex(03)) str(traa$, 51%)

            dsc_map(1) =  42.03  :  dsc_map( 2) =  1
            dsc_map(3) =  45.06  :  dsc_map( 4) =  8
            dsc_map(5) =  69.08  :  dsc_map( 6) = 17.104
            dsc_map(7) = 410.08  :  dsc_map( 8) = 30.104
            dsc_map(9) = 404.061 :  dsc_map(10) = 45

            i_x(1)  = -69.080  :  i_x$(1) = hex(0000000000000000)
            i_x(2)  = -69.080  :  i_x$(2) = hex(000000000000000f)

            hdr$(1) = "  Store  Lot Nbr  Qty-On-Hand     Potency     Exp"~
                    & ". Date                       ."
            hdr$(2) = " "
            hdr$(3) = "Select the Lot You Want to Use"

            str(line2$,62) = "JBPOTPRT: " & str(cms2v$,,8)

            call "READ100" (#14, "SWITCHS.SFC", f1%(14))
                if f1%(14) = 0% then L10000
                get #14, using L09520, prtbin$, inc_loc$, qty_loc%
L09520:             FMT POS(26), CH(1), POS(40), CH(1), BI(1)

            if prtbin$ = "N" then L10000
                loc_hd$(1) = "LOCATION"
                loc_hd$(2) = "--------"
                loc_hd$(3) = "________"

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, warnmsg$
            init(hex(00)) taanr$

            init(" ") bomqty$(), jbnr$, jbdeaclass$, jbbomid$, pot$(),   ~
               jbbomtext$, jbdescr$, jbparr$, jbparrdescr$, jbparrunit$, ~
               jbquantitt$, expdate$(), ltt$(), lpprt$, quantitt$(),     ~
               pipott$(), parr$(), parrdeaclass$(), parrdescr$(),        ~
               parunit$(), pick$(), potunit$(), requann$(), remquann$(), ~
               stdate$, stdpot$(), taanr$, tpckqty$(), whss$(), textidh$,~
               potmult$(), qtyoh$(), effqty$(), effkit$(), jbuomdescr$,  ~
               textidc$()

            mat line%     = zer  :  mat qtt       = zer
            mat factor    = con  :  mat oldfactor = con
            mat qoh       = zer  :  mat stdpot    = zer
            mat potency   = zer  :  mat line_cnt% = zer
            pf16$ = "(16)Exit Program"
            plin% = 0%

            for fieldnr% = 1 to 3
                gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                    if enabled% = 0 then L10600
L10541:         gosub'101(fieldnr%)        /* Display & Accept Screen */
                    if keyhit%  =  1% then gosub startover
                    if keyhit%  = 16% and fieldnr% = 1% then exit_program
                    if keyhit% <>  0% then L10541
                gosub'151(fieldnr%)
                    if errormsg$ <> " " then L10541
L10600:     next fieldnr%

            if jbnr$ <> " " then gosub loadjob
            gosub loadbill

        REM *************************************************************~
            *    S U M M A R Y   P I C K   S C R E E N                  *~
            *************************************************************
        pickscreen
            inpmessage$ = "Position Cursor to Selected Line and Press" & ~
                          " RETURN"
L10650:     gosub'111
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then plin% = 0%
                  if keyhit%  =  3% then plin% = max(0%, mpart% - 8%)
                  if keyhit%  =  4% then plin% = max(0%, plin%  - 6%)
                  if keyhit%  =  5% then plin% =                         ~
                                      min( max(mpart%-6%, 0%),  plin%+6%)
                  if keyhit%  = 14% then       print_kit
                  if keyhit%  = 16% then exit_program
                  if keyhit% <>  0% then       L10650
            if errormsg$ <> " " then pickscreen
               spart% = (cursor%(1%) - 4%) / 2%
               if spart% < 1% or spart% > 8% then pickscreen
                  ipart% = spart% + plin%
                  if ipart% > mpart% then pickscreen
                     if lfac$(spart%) = hex(84) then inputlines          ~
                                                else editlines

        inputlines
            init(" ") errormsg$, warnmsg$, whss$(), ltt$(), expdate$(),  ~
                      qtyoh$(), pot$(), effqty$(), quantitt$(), effkit$()
            mat qoh = zer:mat potency =zer:mat factor = zer:mat qtt = zer
            init(hex(9c)) afac$()
            edt%, line_cnt%(ipart%) = 0%

            for line% = 1 to 12
               for thisfld% = 1 to  3
                   gosub'45 (ipart%, line%, thisfld%)
                         if enabled% = 0 then L11160
L11090:            gosub'33 (ipart%, line%, thisfld%)
                         if keyhit%  =  1 then gosub startover
                         if keyhit%  =  2 then gosub'100(ipart%,line%)
                         if keyhit%  =  9 and thisfld% = 1 then L11300
                         if keyhit%  =  5 and thisfld% = 1 then L11210
                         if keyhit%  = 16 and thisfld% = 1 then L11210
                         if keyhit% <>  0 then       L11090
                   gosub'41 (ipart%, line%, thisfld%)
                         if errormsg$ <> " " then L11090
L11160:        next thisfld%
               line%(ipart%) = line%(ipart%)+1
               line_cnt%(ipart%) = line_cnt%(ipart%) + 1%
               if qty <= 0 then L12000
            next line%
            if ipart% <= mpart% then pickscreen

L11210: REM Here if next part (PF5) or return (PF16) is selected
              whss$(ipart%,line%) = " "  :  afac$(line%) = hex(9c)
              if keyhit% = 16%        then pickscreen
              if ipart% + 1% > mpart% then pickscreen
                 ipart% = ipart% + 1%
                 if tpckqty$(ipart%) = " " then inputlines

L11300:     whss$(ipart%,line%) = " "  :  afac$(line%) = hex(9c)


L12000: editlines
            errormsg$ = " "
            lpprt$ = parr$(ipart%)
            edt% = 1
            currentpart% = ipart%
L12035:     inpmessage$ = "Position Cursor to Selected Line and Press" & ~
                          " RETURN"
L12060:     gosub'34 (currentpart%, 1%, 0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       currentpart%=1%
                  if keyhit%  =  3 then       currentpart%=mpart%
                  if keyhit%  =  4 then       L12640
                  if keyhit%  =  5 then       L12640
                  if keyhit%  = 11 then       L12180
                  if keyhit%  = 12 then       L12180
                  if keyhit%  = 16 then       pickscreen
                  if keyhit% <>  0 then       L12060
L12180:     currentline% = cursor%(1) - 7%
*          IF CURRENTLINE%=1 AND LINE%(CURRENTPART%)=0 AND KEYHIT%=11   ~
*                                     THEN 17020
            if line_cnt%(currentpart%) > 0 and keyhit% = 11 then         ~
                  appendline
            if line_cnt%(currentpart%) = 0 and keyhit% = 11 then         ~
                  inputlines
            if currentline% < 1 or currentline%> line%(currentpart%)     ~
                                       then L12060

                  if keyhit%  = 12 then deleteline
                  if keyhit%  = 11 then appendline
            thisfld%=val(str(traa$,cursor%(2),1))
            if thisfld% = 0 then L12060
L12280:     gosub'34 (currentpart%, currentline%, thisfld%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12280
            gosub'41 (currentpart%, currentline%, thisfld%)
                  if errormsg$ <> " " then L12280
            goto L12060

            edt% = 0%
            for thisfld% = 1 to 3
               gosub'45 (currentpart%, currentline%, thisfld%)
L12370:        gosub'33 (currentpart%, currentline%, thisfld%)
                     if keyhit%  =  1 then gosub startover
               if keyhit%  =  2 then gosub'100(currentpart%,currentline%)
                     if keyhit% <>  0 then L12370
               gosub'41 (currentpart%, currentline%, thisfld%)
                     if errormsg$ <> " " then L12370
            next thisfld%
            edt% = 1%
            goto L12060

L12640:     if keyhit% = 4 then currentpart% = max(currentpart%-1,1)
            if keyhit% = 5 then currentpart% = min(currentpart%+1,mpart%)
            ipart% = currentpart%
            if tpckqty$(ipart%) = " " then inputlines else editlines


        REM *************************************************************~
            *         S U B R O U T I N E S   F R O M   A B O V E       *~
            *-----------------------------------------------------------*~
            * Column One, Append Line and Delete Line                   *~
            *************************************************************
        deffn'100(part%, line%)     /* Return to Column One */
*         COLUMNONE
            if thisfld% = 1 then return
            whss$(part%,line%), ltt$(part%,line%), pot$(part%,line%),    ~
                expdate$(part%,line%), quantitt$(part%, line%),          ~
                potmult$(part%,line%), qtyoh$(part%, line%),             ~
                effqty$(part%,line%), inpmessage$ = " "
            potency(part%, line%) = 0
            factor (part%, line%) = 1.0
            thisfld% = 1%
            errormsg$ = " "
            return

*        Append Line
        appendline
           if line_cnt%(currentpart%) = 20 then L12060
           if line%(currentpart%)=20 then L12060
           line%(currentpart%) = line%(currentpart%) + 1%
           line_cnt%(currentpart%) = line_cnt%(currentpart%) + 1%
           currentline% = line_cnt%(currentpart%)

           init(" ") whss$    (currentpart%, currentline%),              ~
                     ltt$     (currentpart%, currentline%),              ~
                     expdate$ (currentpart%, currentline%),              ~
                     qtyoh$   (currentpart%, currentline%),              ~
                     pot$     (currentpart%, currentline%),              ~
                     effqty$  (currentpart%, currentline%),              ~
                     quantitt$(currentpart%, currentline%),              ~
                     effkit$  (currentpart%, currentline%)

            edt% = 0%
            for thisfld% = 1 to 3
               gosub'45 (currentpart%, currentline%, thisfld%)
L17250:        gosub'33 (currentpart%, currentline%, thisfld%)
                     if keyhit%  =  1 then gosub startover
               if keyhit%  =  2 then gosub'100(currentpart%,currentline%)
               if keyhit%  =  9 and thisfld% = 1 then L12060
                     if keyhit% <>  0 then L17250
               gosub'41 (currentpart%, currentline%, thisfld%)
                     if errormsg$ <> " " then L17250
            next thisfld%
            edt% = 1%
            goto L12035

*        Delete line here
        deleteline
            if line%(currentpart%)=0 then L12060
            lot_deleted$ = ltt$(currentpart%,currentline%)
            qoh_deleted  = qoh(currentpart%,currentline%)
            eff_deleted$ = effqty$(currentpart%,currentline%)
*        First Roll the Data and Screen Array's
            for i% = currentline% to line%(currentpart%)-1%
                whss$(currentpart%,i%)     = whss$(currentpart%,i%+1)
                ltt$(currentpart%,i%)      = ltt$(currentpart%,i%+1)
                expdate$(currentpart%,i%)  = expdate$(currentpart%,i%+1)
                qtyoh$(currentpart%,i%)    = qtyoh$(currentpart%,i%+1)
                qoh(currentpart%,i%)       = qoh(currentpart%,i%+1)
                pot$(currentpart%,i%)      = pot$(currentpart%,i%+1)
                potency(currentpart%,i%)   = potency(currentpart%,i%+1)
                factor(currentpart%,i%)    = factor(currentpart%,i%+1)
                effqty$(currentpart%,i%)   = effqty$(currentpart%,i%+1)
                quantitt$(currentpart%,i%) = quantitt$(currentpart%,i%+1)
                qtt(currentpart%,i%)       = qtt(currentpart%,i%+1)
                effkit$(currentpart%,i%)   = effkit$(currentpart%,i%+1)
                afac$(i%) = afac$(i%+1)
            next i%
            line%(currentpart%) = line%(currentpart%) - 1%
            line_cnt%(currentpart%) = line%(currentpart%)
*        Clean House
            for i% = (line%(currentpart%)+1%) to 12%
                init(" ") whss$(currentpart%,i%), ltt$(currentpart%,i%), ~
                     expdate$(currentpart%,i%), qtyoh$(currentpart%,i%), ~
                     pot$(currentpart%,i%), effqty$(currentpart%,i%),    ~
                     quantitt$(currentpart%,i%), effkit$(currentpart%,i%)
                qoh(currentpart%,i%), potency(currentpart%,i%),          ~
                        factor(currentpart%,i%), qtt(currentpart%,i%) = 0
                afac$(i%) = hex(9c)
            next i%
*        Adjust Qty Aval. from/for Each Lot
            for i% = 1% to line%(currentpart%)
                if ltt$(currentpart%,i%) <> lot_deleted$ then L18280
                if qoh(currentpart%,i%)  >= qoh_deleted  then L18277
                     call "CONVERT" (qoh_deleted, 2.4,                   ~
                                                 qtyoh$(currentpart%,i%))
                     effqty$(currentpart%,i%) = eff_deleted$
L18277:              qoh_deleted = 0
L18280:         convert qtyoh$(currentpart%,i%)    to last_qtyoh
                convert quantitt$(currentpart%,i%) to last_kit
                for x% = (i%+1%) to line%(currentpart%)
                     if x% > line%(currentpart%) then L18335
                     if ltt$(currentpart%,i%) <> ltt$(currentpart%,x%)   ~
                                                               then L18334
                     qoh(currentpart%,x%) = last_qtyoh - last_kit
                     effqty = qoh(currentpart%,x%) *                     ~
                                                 potency(currentpart%,x%)
                     call "CONVERT" (qoh(currentpart%,x%), 2.4,          ~
                                                 qtyoh$(currentpart%,x%))
                     call "CONVERT"(effqty, 2.4, effqty$(currentpart%,x%))
                     goto L18335
L18334:         next x%
L18335:     next i%
*        Adjust the Quantity Kitted and Remaning on the Job
            tpckqty = 0
            for i% = 1% to line%(currentpart%)
                convert effkit$(currentpart%,i%) to effkit
                tpckqty = tpckqty + effkit
            next i%
            convert requann$(currentpart%) to requann
            remquann = requann - tpckqty
            call "CONVERT" (tpckqty, 2.4, tpckqty$(currentpart%))
            call "CONVERT" (remquann, 2.4, remquann$(currentpart%))
            if tpckqty = 0 then tpckqty$(currentpart%) = " "
            errormsg$, warnmsg$ = " "
            if remquann < 0 then warnmsg$ = "WARNING: Effective Quantity"~
                               & " Issued Exceeds the Job's Requirments."
            goto L12060

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   H E A D E R   P A G E   *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields for the page 1 of input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 0%
            inpmessage$ = " "
                  on fieldnr% gosub L20130,         /* Job Number       */~
                                    L20200,         /* Part Number      */~
                                    L20490          /* Quantity         */
                  return

L20130: REM Default/enable for Job Number
            enabled% = 1%
            inpmessage$ = "Enter Job Number or blank  ( Enter '?' "  &   ~
                          "to select from file )"
            return

L20200: REM Default/enable for Part Number
            if jbnr$ = " " then enabled% = 1%
            inpmessage$ = "Enter Assembly Number & BOM ID  "  &          ~
                          "( Enter Partial or Leave Blank to Select )"
            return

L20490: REM Default/enable for Quantity
            if jbnr$ = " " then enabled% = 1%
            inpmessage$ = "Enter the Quantity you want to Make"
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *-----------------------------------------------------------*~
            * Sets defaults and enables fields for the page 2 of input. *~
            *************************************************************

            deffn'45 (part%, line%, thisfld%)
                  enabled% = 0%
                  inpmessage$ = " "
                  on thisfld% gosub L21140,         /* WAREHOUSE        */~
                                    L21210,         /* LOT              */~
                                    L21280          /* QUANTITY         */
                     return

L21140: REM Default/enable for warehouse
            if type%=0% then return
            enabled%=1%
            if line% = 1% then L21180
            whss$(part%,line%) =  whss$(part%,line%-1%)
L21180:     inpmessage$ = "Enter the Store Number "
            return

L21210: REM Default/enable for lot
            if type%=0% then return
            enabled%=1%
            inpmessage$ = "Leave Blank If You Wish To Review Lot Quantiti~
        ~es For This Part"
            return

L21280: REM Default/enable for quantity
            if line% > 1 and type% = 0% then return
            if type% <> 0% then L21370
            call "CONVERT" (optavail, -2.4, quantitt$(part%, line%))
            return

        REM * Determine deflt qty based on std remaining quantity needed ~
            * and mulitply by FACTOR = (STANDARD POTENCY / LOT POTENCY)

L21370:     enabled%=1%
            temp, temp2 = 0
            convert remquann$(part%) to remquann
            adjusted_qty = remquann * factor(part%, line%)
            temp = min((max(0, adjusted_qty)), qoh(part%,line%))
            if remquann <> 0 then temp2 = round(temp1 / remquann * 100, 2)
            if temp2 <= 0 or temp2 >= 100 then temp2 = 100
            convert temp2 to temp2$, pic(-###.##)
            inpmessage$ = "Lot " & ltt$(part%, line%) &" Can Supply "  & ~
                           temp2$ & "% of the Balance Needed"
            if temp = adjusted_qty then                                  ~
              call "CONVERT" (adjusted_qty, -2.4, quantitt$(part%,line%))~
              else call "CONVERT" (temp, -2.4, quantitt$(part%, line%))

            return


        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the user the ability to start over when he wants to *~
            * else return to the menu.  Notice that he has to push 2    *~
            * different buttons to start over--a little harder.         *~
            *************************************************************

        startover
L29890:     keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1%  = 1% then return
            if keyhit1% <> 0% then L29890
                return clear all
                goto inputmode


        REM *************************************************************~
            *          L O A D    J O B                                 *~
            *-----------------------------------------------------------*~
            * Load up job, get bom, units of measure, std potency, etc. *~
            *************************************************************

        loadjob
            get #4, using L30120, jbdescr$, str(taanr$,1,19), jbparr$,    ~
                                 qty, stdate$, wipacct$
L30120:         FMT XX(8),CH(30),CH(19),CH(25),PD(14,4),XX(56),CH(6),    ~
                    XX(6),CH(9)

            call "DATEFMT" (stdate$)
            call "CONVERT" (qty, 2.4, jbquantitt$)
            call "READ100" (#3, jbparr$, f1%(3))
            get #3, using L30220, jbparrdescr$, jbparrunit$
L30220:         FMT XX(25), CH(32), XX(16), CH(4)
            readkey$ = "UOM      " & jbparrunit$
            jbuomdescr$ = " "
            call "DESCRIBE" (#8, readkey$, jbuomdescr$, 1%, f1%(8))

*        Get BOM information from JBCROSS2
            if jbnr$ = " " then L30370
            call "READ100" (#21, taanr$, f1%(21))
                if f1%(21) = 0% then L30370
            get #21, using L30320, jbbomid$
L30320:         FMT XX(72), CH(3)
            return

        REM *************************************************************~
            *    L O A D   B I L L   O F   M A T E R I A L S            *~
            *-----------------------------------------------------------*~
            * Load up Bill of Materials                                 *~
            *************************************************************
        loadbill

L30370:     edt%, ipart%, mpart%, pass%=0
            plowkey$ = str(jbparr$,1%,25%) & str(jbbomid$,1%,3%) & "  0"

L30390:     if jbnr$ = " " then L30413
            gosub nextpart
                if f1%(34)   <> 1%  then return
            goto L30420

L30413:     gosub nextpart1
                if f1%(30)   <> 1%  then return
            goto L30660

L30420: REM Get BOM entry for this component
            bomkey$ = str(parr$(ipart%), 1%, 25%) & str(jbparr$, 1%, 25%)~
                    & str(jbbomid$, 1%, 3%)
            call "PLOWALTS" (#30, bomkey$, 1%, 53%, f1%(30))
                 if f1%(30) = 1% then L30660
                    infomsg$  = "Could Not Find Complete Bill For This Pa~
        ~rt: " & jbparr$
                    goto L30660
            get #30, using L30510, bomqty, textidc$(ipart%)
L30510:         FMT XX(56), PD(14,4), POS(95), CH(4)
            call "CONVERT" (bomqty, 2.4, bomqty$(ipart%))

L30660: REM Get next part
              mpart% = ipart%
              pick$(ipart%) = hex(0b)
              goto L30390

        REM *************************************************************~
            *  LOAD UP PARTS FROM PIPOUT                                *~
            *************************************************************
        nextpart
            inpmessage$=" "
            if ipart%=0 and pass%=0 then init(hex(00)) str(taanr$,20)
            call "PLOWNEXT" (#34, taanr$, 19%, f1%(34))
            if f1%(34)<>0 then L30860

                if ipart% > 0 then return
                if pass%=0 then                                          ~
                errormsg$ = "All components have been issued to this job"~
                             else                                        ~
                             errormsg$=" "
                return

L30860:     ipart%=ipart%+1
            pass%=1
            get #34, using L30890, pipott$(ipart%)
L30890:              FMT CH(64)
            get #34, using L30910, parr$(ipart%), qty
L30910:              FMT XX(19), CH(25), XX(12), PD(14,4)
L30920:     call "READ100" (#3, parr$(ipart%), f1%(3))
            if f1%(3)=0 then L31030
            get #3, using L30960, parrdescr$(ipart%), parunit$(ipart%),   ~
                                 type$, stdpot(ipart%)
L30960:         FMT XX(25), CH(32), POS(74), CH(4), POS(180), CH(03),    ~
                                    POS(203), PD(14,4)
            readkey$ = "UOM      " & parunit$(ipart%)
            call "DESCRIBE" (#8, readkey$, uomdescr$(ipart%), 0%, f1%(8))
            call "CONVERT" (stdpot(ipart%), 2.4, stdpot$(ipart%))
L31030:     call "CONVERT" (qty, 2.4, requann$(ipart%))
            call "CONVERT" (qty, 2.4, remquann$(ipart%))
            type%=500%
            convert type$ to type%, data goto L31070
L31070:     if type%<>0% then return
            inpmessage$="Build To Order Part"
            if ipart%=1% then L31110
            if parr$(ipart%)=parr$(ipart%-1%) then L31120
L31110:     init (" ") rftg$
L31120:    call"PLOWNEXT"(#36,str(pipott$(ipart%),1,44)&rftg$,44%,f1%(36))
            if f1%(36)<>0 then L31150
            type%=500%:return
L31150:     get #36, using L31160, rftg$
L31160:         FMT XX(44),CH(19)
            optavail=0
            if str(rftg$,1,2)<>"JO" then return
            call "PLOWNEXT"(#7,str(rftg$,12,8)&hex(0000000000),8%,f1%(7))
            if f1%(7)=0 then return
            get #7,using L31211, whss$(ipart%,1%),ltt$(ipart%,1%)
L31211:         FMT POS(48), CH(3), CH(6)

            plowkey$ = parr$(ipart%)
            str(plowkey$,26,3) = whss$(ipart%,1%)
            str(plowkey$,29) = ltt$(ipart%,1%)
            call"READ100"(#11, plowkey$, f1%(11))
                if f1%(11)=0 then return
            get #11, using L31250, optavail  /* qty on hand */
L31250:         FMT XX(68), PD(14,4)

            if optavail > 0 then return
           call"PLOWNEXT"(#36,str(pipott$(ipart%),1,44)&rftg$,44%,f1%(36))
            if f1%(36)=0 then return else goto L31150


        nextpart1:
        REM Get BOM entry for this component
            call "PLOWALTS" (#30, plowkey$, 0%, 28%, f1%(30))
                if f1%(30) <> 0% then L31725
            if ipart% > 0% then return
              infomsg$  = "Could Not Find Bill For This Part: " & jbparr$
            return

L31725:     ipart% = ipart% + 1%
            pass% = 1%
            get #30, using L31820, parr$(ipart%), bomqty, fact,           ~
                                  textidc$(ipart%)
L31820:         FMT CH(25), XX(31), PD(14,4), PD(14,4), POS(95), CH(4)
            call "CONVERT" (bomqty, 2.4, bomqty$(ipart%))

            bomkey$ = str(parr$(ipart%), 1%, 25%) & str(plowkey$,1%, 31%)

            qty = jbqty * bomqty * fact
            type$ = "500"
            goto L30920



        print_kit
           convert jbquantitt$ to temp
           call "SHOSTAT" ("Printing Calculated Formula Pick List")

           page%=0
           header% = 1%
           line%=99

        for part% = 1% to mpart%
           if line% > 56 then gosub page_head      /* PAGE CONTROL */
           convert requann$(part%) to temp
        /* Subroutine RNDCHEM not currently used */
           call "RNDCHEM" (temp, parrdeaclass$(part%), parunit$(part%),  ~
                           prtqty$, nn)
           call "CONVERT" (temp, 2.4, str(prtqty$,,12))
           str(prtqty$,14,2) = parunit$(part%)
           if qtt(part%,1%) <> 0 then L33260
                print using L35420, part%, parr$(part%), prtqty$,         ~
                                   stdpot$(part%)
                print skip(1)
                line% = line% + 2%
                goto L33660

        /* Subroutine RNDCHEM not currently used */
           call "RNDCHEM" (qtt(part%, 1%), parrdeaclass$(part%),         ~
                           parunit$(part%), prtqty1$, nn)
L33260:    call "CONVERT" (qtt(part%, 1%), 2.4, str(prtqty1$,,12))
           str(prtqty1$,14,2) = parunit$(part%)
           print using L35250, part%, parr$(part%), prtqty$,              ~
                              stdpot$(part%), loc_hd$(3)

           strlot$ = whss$(part%,1%) & "/" & ltt$(part%,1%)
           store$ = whss$(part%,1%) : lot$ = ltt$(part%,1%)
           old_store$ = " " : gosub find_bin
           call "CONVERT" (potency(part%, 1%), -2.4, pot$)
           print using L35280, strlot$, bin$, prtqty1$, pot$,             ~
                              effkit$(part%,1%), parunit$(part%)
           convert effkit$(part%,1%) to issued
           print skip(1)
           line% = line% + 3
           total(2) = qtt(part%, 1%)
           total(3) = issued
           gosub print_bin
           if line%(part%) <= 1% then L33550
           for i% = 2% to line%(part%)
                if line% > 56 then gosub page_head    /* PAGE CONTROL */
                print using L35310, loc_hd$(3)
                call "CONVERT" (potency(part%, i%), -2.4, pot$)
        /* Subroutine RNDCHEM not currently used */
                call "RNDCHEM" (qtt(part%, i%), parrdeaclass$(part%),    ~
                                parunit$(part%), prtqty1$, nn)
                call "CONVERT" (qtt(part%, i%), 2.4, str(prtqty1$,,12))
                str(prtqty1$,14,2) = parunit$(part%)
                strlot$ = whss$(part%,i%) & "/" & ltt$(part%,i%)
                store$ = whss$(part%,i%) : lot$ = ltt$(part%,i%)
                gosub find_bin
                print using L35280, strlot$, bin$, prtqty1$, pot$,        ~
                            effkit$(part%, i%), parunit$(part%)
                print skip(1)
                line% = line% + 3
                convert effkit$(part%,i%) to issued
                total(2) = total(2) + qtt(part%, i%)
                total(3) = total(3) + issued
                gosub print_bin
            next i%
L33550:
           total$(1) = str(prtqty$,3,10)
           call "STRING" addr("LJ", total$(1), 13%)
           total$(1) = total$(1) & " " & str(prtqty$,14,2)
        /* Subroutine RNDCHEM not currently used */
           call "RNDCHEM" (total(3), parrdeaclass$(part%),               ~
                           parunit$(part%), total$(3), nn)
           call "CONVERT" (total(3), -2.4, total$(3))
           total$(3) = total$(3) & " " & parunit$(part%)
        /* Subroutine RNDCHEM not currently used */
           call "RNDCHEM" (total(2), parrdeaclass$(part%),               ~
                           parunit$(part%), total$(2), nn)
           call "CONVERT" (total(2), -2.4, total$(2))
           total$(2) = total$(2) & " " & str(prtqty1$,14,2)
           print using L35350, total$(1), total$(2), total$(3)
           print skip(2)
           line% = line% + 3%
L33660: next part%

           print using L35390

*        Now print the bill of materials text - all 3 types
            header% = 2%
            gosub header_text
            if line% < 51% then L34023
            gosub page_head
L34023:     print skip(2)
            gosub component_text
            close printer
            hdr% = 0%
            goto inputmode

*        First the header text types, General and Procedural
        header_text
            if textidh$ = hex(20202020) or                               ~
               textidh$ = hex(00000000) or                               ~
               textidh$ = hex(ffffffff) then return
            hdr% = 1%
            gosub page_head
            print using L35470
            stat% = 0%
L34250:     call "TXTPRINT" (#12, f2%(12), 134%, textidh$, " ", 7%,      ~
                                  line%, 56%, "N", " ", stat%)
            if stat% = 0% then return
            gosub page_head
            goto L34250


*        And now print any existing Component Text
        component_text:
            for i% = 1% to mpart%
                if textidc$(i%) = hex(20202020) or                       ~
                   textidc$(i%) = hex(00000000) or                       ~
                   textidc$(i%) = hex(ffffffff) then L34520
                print using L35450, parr$(i%)
L34460:         stat% = 0%
                call "TXTPRINT" (#12, f2%(12), 134%, textidc$(i%), " ",  ~
                                  7%, line%, 56%, "N", " ", stat%)
                if stat% = 0% then L34515
                gosub page_head
                goto L34460
L34515:         print skip(1)
                line% = line% + 1%
L34520:     next i%
            return

        page_head

            if page% <> 0% then print page
            page% = page% + 1
            print using L35040, date$, company$, page%
            print using L35070
            line% = 2%
            if page% <> 1% then L34740
            print skip(1)
            print using L35490
            print using L35520
            print using L35550
            print using L35580
            print using L35610
            line% = line% + 6%
L34740:     print skip(1)
            print using L35100, jbnr$, jbquantitt$, jbparrunit$,          ~
                               jbparr$, jbbomid$
            print skip(1)
            if hdr% = 1% then L34850
            print using L35190, loc_hd$(1)
            print using L35220, loc_hd$(2)
            print skip(1)
            line% = line% + 6%
            return

L34850:     line% = line% + 3%
            return

        REM Find any locations that should be printed

        find_bin
            if prtbin$ = "N" then return
            if old_store$ = store$ then L34915
            call "HNYBINSB" ("P", parr$(part%), "Q", inc_loc$,           ~
                             store$, 0%, bin_loc$(), #3, #11, #13)
            old_store$ = store$
L34915:     bin$ = " "
            search str(bin_loc$(),2) = str(store$) & str(lot$)           ~
                          to index%() step 20%
                if index%(1) = 0% then return
            bin$ = str(bin_loc$(),index%(1) + 10%, 8%)
            init (" ") str(bin_loc$(),index%(1) - 1%, 20%)
            return

        REM Print extra bin locations if necessary

        print_bin
            bin_cnt% = 1%
L34975:     if qty_loc% = 0% then goto L34982
            if bin_cnt% = qty_loc% then return
L34982:     gosub find_bin
                if bin$ = " " then return
            if line% > 56% then gosub page_head
            print using L35650
            print using L35680, bin$
            print skip(1)
            line% = line% + 3%
            bin_cnt% = bin_cnt% + 1%
            goto L34975

        REM *************************************************************~
            *      N O W   T H E   I M A G E   S T A T E M E N T S      *~
            *************************************************************

L35040: %DATE RUN: ########                ##############################~
        ~##############################                          Page: ###

L35070: %                                                   CALCULATED FO~
        ~RMULA PICK LIST

L35100: %Job Number ######## FOR ########## #### OF PART NUMBER #########~
        ~################ BOM ID: ###

        %Part No ######################### ##############################~
        ~##  DEA ####  Pick by:____________________

        %Quantity To Make ########## ##


L35190: %     Part Number                Quantity Req'd @ Std Pot.  Str/L~
        ~ot     ########    Pick Quantity @ Lot Pot.    Eff. Qty

L35220: %------------------------------  -------------------------  -----~
        ~-----  ########  --------------------------  ----------

L35250: %###) ######################### ############### @ ######    ___/_~
        ~_____  ########  _______________

L35280: %                                                           #####~
        ~#####  ########  ############### @ ######    ########## ####

L35310: %                                                           ___/_~
        ~_____  ########  _______________


L35350: %**********  QUANTITY REQUIRED = #############  ACTUAL QUANTITY P~
        ~ICKED = #############  RESULTING EFFECTIVE QUANTITY = ###########~
        ~##

L35390: %                                            * * * * * END OF MAT~
        ~ERIALS LIST * * * * *

L35420: %###) ######################### ############### @ ######    NO PI~
        ~CK QUANTITY DESIGNATED!!

L35450: %COMPONENT TEXT FOR PART NUMBER: #########################

L35470: %ASSEMBLY GENERAL INSTRUCTIONS

L35490: %      ****************************************************  N O ~
        ~T I C E  ****************************************************

L35520: %      *     The following formula pick list was calculated using~
        ~ the quantities and potency factors as they exist on the    *

L35550: %      *  lot/quantity records.  The resulting quantity of the to~
        ~p assembly number may or may not equal the quantity         *

L35580: %      *  requested.  Please review this pick list carefully!!!  ~
        ~                                                            *

L35610: %      **********************************************************~
        ~*************************************************************

L35650: %                                                                ~
        ~       ________

L35680: %                                                                ~
        ~       ########


        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *-----------------------------------------------------------*~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'33 (part%, line%, thisfld%)
                if errormsg$ <> " " then L40070
                     errormsg$ = warnmsg$
L40070:         init(hex(84)) lfff$()
                afac$(line%) = hex(8c)
            temp$ = jbquantitt$
            call "STRING" addr("LJ", temp$, 25%)
            if jbnr$ = " " then L40150
            str(line2$,,60) = "Job # "  & jbnr$ & ", " & temp$ & " " &   ~
                               jbuomdescr$ & " of " & jbparr$
            goto L40180

L40150:     str(line2$,,60) = temp$ & " " & jbuomdescr$ & " of " &       ~
                            jbparr$ & " using BOM ID '" & jbbomid$ & "'"

L40180:           on thisfld% gosub L40250,         /* WAREHOUSE        */~
                                    L40250,         /* LOT              */~
                                    L40260          /* QUANTITY         */

                  goto L40280

            lfff$(line%, thisfld%) = hex(80) :  return  /* up - low */
L40250:     lfff$(line%, thisfld%) = hex(81) :  return  /* Upper    */
L40260:     lfff$(line%, thisfld%) = hex(82) :  return  /* Numeric  */

L40280:     accept                                                       ~
               at (01,02), "Issue Materials to Job Orders",              ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Total Required:",                            ~
               at (05,18), fac(hex(84)), requann$  (part%)      , ch(10),~
               at (05,29), "Total Issued:",                              ~
               at (05,44), fac(hex(84)), tpckqty$  (part%)      , ch(10),~
               at (05,55), "Balance Needed:",                            ~
               at (05,71), fac(hex(a4)), remquann$ (part%)      , ch(10),~
                                                                         ~
               at (06,03), "Part Number: ",                              ~
               at (06,16), fac(hex(84)), parr$(part%)           , ch(25),~
               at (06,71), fac(hex(8c)), uomdescr$(part%)       , ch(10),~
                                                                         ~
               at (07,03), fac(hex(ac)), header$                , ch(78),~
                                                                         ~
               at (08,03), fac(lfff$( 1,1)), whss$    (part%, 1), ch(03),~
               at (08,07), fac(lfff$( 1,2)), ltt$     (part%, 1), ch(06),~
               at (08,14), fac(hex(8c)),     expdate$ (part%, 1), ch(08),~
               at (08,23), fac(hex(8c)),     qtyoh$   (part%, 1), ch(10),~
               at (08,34), fac(afac$( 1)),   amp$               , ch(10),~
               at (08,36), fac(hex(8c)),     pot$     (part%, 1), ch(06),~
               at (08,43), fac(hex(8c)),     effqty$  (part%, 1), ch(10),~
               at (08,55), fac(lfff$( 1,3)), quantitt$(part%, 1), ch(10),~
               at (08,67), fac(hex(8c)),     effkit$  (part%, 1), ch(10),~
                                                                         ~
               at (09,03), fac(lfff$( 2,1)), whss$    (part%, 2), ch(03),~
               at (09,07), fac(lfff$( 2,2)), ltt$     (part%, 2), ch(06),~
               at (09,14), fac(hex(8c)),     expdate$ (part%, 2), ch(08),~
               at (09,23), fac(hex(8c)),     qtyoh$   (part%, 2), ch(10),~
               at (09,34), fac(afac$( 2)),   amp$               , ch(10),~
               at (09,36), fac(hex(8c)),     pot$     (part%, 2), ch(06),~
               at (09,43), fac(hex(8c)),     effqty$  (part%, 2), ch(10),~
               at (09,55), fac(lfff$( 2,3)), quantitt$(part%, 2), ch(10),~
               at (09,67), fac(hex(8c)),     effkit$  (part%, 2), ch(10),~
                                                                         ~
               at (10,03), fac(lfff$( 3,1)), whss$    (part%, 3), ch(03),~
               at (10,07), fac(lfff$( 3,2)), ltt$     (part%, 3), ch(06),~
               at (10,14), fac(hex(8c)),     expdate$ (part%, 3), ch(08),~
               at (10,23), fac(hex(8c)),     qtyoh$   (part%, 3), ch(10),~
               at (10,34), fac(afac$( 3)),   amp$               , ch(10),~
               at (10,36), fac(hex(8c)),     pot$     (part%, 3), ch(06),~
               at (10,43), fac(hex(8c)),     effqty$  (part%, 3), ch(10),~
               at (10,55), fac(lfff$( 3,3)), quantitt$(part%, 3), ch(10),~
               at (10,67), fac(hex(8c)),     effkit$  (part%, 3), ch(10),~
                                                                         ~
               at (11,03), fac(lfff$( 4,1)), whss$    (part%, 4), ch(03),~
               at (11,07), fac(lfff$( 4,2)), ltt$     (part%, 4), ch(06),~
               at (11,14), fac(hex(8c)),     expdate$ (part%, 4), ch(08),~
               at (11,23), fac(hex(8c)),     qtyoh$   (part%, 4), ch(10),~
               at (11,34), fac(afac$( 4)),   amp$               , ch(10),~
               at (11,36), fac(hex(8c)),     pot$     (part%, 4), ch(06),~
               at (11,43), fac(hex(8c)),     effqty$  (part%, 4), ch(10),~
               at (11,55), fac(lfff$( 4,3)), quantitt$(part%, 4), ch(10),~
               at (11,67), fac(hex(8c)),     effkit$  (part%, 4), ch(10),~
                                                                         ~
               at (12,03), fac(lfff$( 5,1)), whss$    (part%, 5), ch(03),~
               at (12,07), fac(lfff$( 5,2)), ltt$     (part%, 5), ch(06),~
               at (12,14), fac(hex(8c)),     expdate$ (part%, 5), ch(08),~
               at (12,23), fac(hex(8c)),     qtyoh$   (part%, 5), ch(10),~
               at (12,34), fac(afac$( 5)),   amp$               , ch(10),~
               at (12,36), fac(hex(8c)),     pot$     (part%, 5), ch(06),~
               at (12,43), fac(hex(8c)),     effqty$  (part%, 5), ch(10),~
               at (12,55), fac(lfff$( 5,3)), quantitt$(part%, 5), ch(10),~
               at (12,67), fac(hex(8c)),     effkit$  (part%, 5), ch(10),~
                                                                         ~
               at (13,03), fac(lfff$( 6,1)), whss$    (part%, 6), ch(03),~
               at (13,07), fac(lfff$( 6,2)), ltt$     (part%, 6), ch(06),~
               at (13,14), fac(hex(8c)),     expdate$ (part%, 6), ch(08),~
               at (13,23), fac(hex(8c)),     qtyoh$   (part%, 6), ch(10),~
               at (13,34), fac(afac$( 6)),   amp$               , ch(10),~
               at (13,36), fac(hex(8c)),     pot$     (part%, 6), ch(06),~
               at (13,43), fac(hex(8c)),     effqty$  (part%, 6), ch(10),~
               at (13,55), fac(lfff$( 6,3)), quantitt$(part%, 6), ch(10),~
               at (13,67), fac(hex(8c)),     effkit$  (part%, 6), ch(10),~
                                                                         ~
               at (14,03), fac(lfff$( 7,1)), whss$    (part%, 7), ch(03),~
               at (14,07), fac(lfff$( 7,2)), ltt$     (part%, 7), ch(06),~
               at (14,14), fac(hex(8c)),     expdate$ (part%, 7), ch(08),~
               at (14,23), fac(hex(8c)),     qtyoh$   (part%, 7), ch(10),~
               at (14,34), fac(afac$( 7)),   amp$               , ch(10),~
               at (14,36), fac(hex(8c)),     pot$     (part%, 7), ch(06),~
               at (14,43), fac(hex(8c)),     effqty$  (part%, 7), ch(10),~
               at (14,55), fac(lfff$( 7,3)), quantitt$(part%, 7), ch(10),~
               at (14,67), fac(hex(8c)),     effkit$  (part%, 7), ch(10),~
                                                                         ~
               at (15,03), fac(lfff$( 8,1)), whss$    (part%, 8), ch(03),~
               at (15,07), fac(lfff$( 8,2)), ltt$     (part%, 8), ch(06),~
               at (15,14), fac(hex(8c)),     expdate$ (part%, 8), ch(08),~
               at (15,23), fac(hex(8c)),     qtyoh$   (part%, 8), ch(10),~
               at (15,34), fac(afac$( 8)),   amp$               , ch(10),~
               at (15,36), fac(hex(8c)),     pot$     (part%, 8), ch(06),~
               at (15,43), fac(hex(8c)),     effqty$  (part%, 8), ch(10),~
               at (15,55), fac(lfff$( 8,3)), quantitt$(part%, 8), ch(10),~
               at (15,67), fac(hex(8c)),     effkit$  (part%, 8), ch(10),~
                                                                         ~
               at (16,03), fac(lfff$( 9,1)), whss$    (part%, 9), ch(03),~
               at (16,07), fac(lfff$( 9,2)), ltt$     (part%, 9), ch(06),~
               at (16,14), fac(hex(8c)),     expdate$ (part%, 9), ch(08),~
               at (16,23), fac(hex(8c)),     qtyoh$   (part%, 9), ch(10),~
               at (16,34), fac(afac$( 9)),   amp$               , ch(10),~
               at (16,36), fac(hex(8c)),     pot$     (part%, 9), ch(06),~
               at (16,43), fac(hex(8c)),     effqty$  (part%, 9), ch(10),~
               at (16,55), fac(lfff$( 9,3)), quantitt$(part%, 9), ch(10),~
               at (16,67), fac(hex(8c)),     effkit$  (part%, 9), ch(10),~
                                                                         ~
               at (17,03), fac(lfff$(10,1)), whss$    (part%,10), ch(03),~
               at (17,07), fac(lfff$(10,2)), ltt$     (part%,10), ch(06),~
               at (17,14), fac(hex(8c)),     expdate$ (part%,10), ch(08),~
               at (17,23), fac(hex(8c)),     qtyoh$   (part%,10), ch(10),~
               at (17,34), fac(afac$(10)),   amp$               , ch(10),~
               at (17,36), fac(hex(8c)),     pot$     (part%,10), ch(06),~
               at (17,43), fac(hex(8c)),     effqty$  (part%,10), ch(10),~
               at (17,55), fac(lfff$(10,3)), quantitt$(part%,10), ch(10),~
               at (17,67), fac(hex(8c)),     effkit$  (part%,10), ch(10),~
                                                                         ~
               at (18,03), fac(lfff$(11,1)), whss$    (part%,11), ch(03),~
               at (18,07), fac(lfff$(11,2)), ltt$     (part%,11), ch(06),~
               at (18,14), fac(hex(8c)),     expdate$ (part%,11), ch(08),~
               at (18,23), fac(hex(8c)),     qtyoh$   (part%,11), ch(10),~
               at (18,34), fac(afac$(11)),   amp$               , ch(10),~
               at (18,36), fac(hex(8c)),     pot$     (part%,11), ch(06),~
               at (18,43), fac(hex(8c)),     effqty$  (part%,11), ch(10),~
               at (18,55), fac(lfff$(11,3)), quantitt$(part%,11), ch(10),~
               at (18,67), fac(hex(8c)),     effkit$  (part%,11), ch(10),~
                                                                         ~
               at (19,03), fac(lfff$(12,1)), whss$    (part%,12), ch(03),~
               at (19,07), fac(lfff$(12,2)), ltt$     (part%,12), ch(06),~
               at (19,14), fac(hex(8c)),     expdate$ (part%,12), ch(08),~
               at (19,23), fac(hex(8c)),     qtyoh$   (part%,12), ch(10),~
               at (19,34), fac(afac$(12)),   amp$               , ch(10),~
               at (19,36), fac(hex(8c)),     pot$     (part%,12), ch(06),~
               at (19,43), fac(hex(8c)),     effqty$  (part%,12), ch(10),~
               at (19,55), fac(lfff$(12,3)), quantitt$(part%,12), ch(10),~
               at (19,67), fac(hex(8c)),     effkit$  (part%,12), ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (23,20), "(5)Next Part",                               ~
               at (22,65), "(13)Instructions",                           ~
               at (23,02), "(2)Column One",                              ~
               at (22,45), "(9)Edit Mode",                               ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Return      ",                           ~
                                                                         ~
               keys(hex(00010205090f10)),                                ~
               key (keyhit%)

               if keyhit% <> 13 then L41810
                  call "MANUAL" ("JBPOTPRT")
                  goto L40280

L41810:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40280

        REM *************************************************************~
            *       E D I T   M O D E   S C R E E N   P A G E   1       *~
            *                                                           *~
            * SCREEN FOR EDITING PAGE 1 OF DOCUMENT.                    *~
            *************************************************************

            deffn'34 (part%, line%, thisfld%)
                if errormsg$ <> " " then L42070
                     errormsg$ = warnmsg$
L42070:           if thisfld% = 0% then init(hex(86)) lfff$()            ~
                                   else init(hex(84)) lfff$()
            temp$ = jbquantitt$
            call "STRING" addr("LJ", temp$, 25%)
            if jbnr$ = " " then L42140
            str(line2$,,60) = "Job # "  & jbnr$ & ", " & temp$ & " " &   ~
                               jbuomdescr$ & " of " & jbparr$
            goto L42155

L42140:     str(line2$,,60) = temp$ & " " & jbuomdescr$ & " of " &       ~
                            jbparr$ & " using BOM ID '" & jbbomid$ & "'"

L42155:           on thisfld% gosub L42190,         /* WAREHOUSE        */~
                                    L42190,         /* LOT              */~
                                    L42195          /* QUANTITY         */

                  goto L42205

            lfff$(line%, thisfld%) = hex(80) :  return  /* up - low */
L42190:     lfff$(line%, thisfld%) = hex(81) :  return  /* Upper    */
L42195:     lfff$(line%, thisfld%) = hex(82) :  return  /* Numeric  */

L42205:     accept                                                       ~
               at (01,02), "Issue Materials to Job Orders",              ~
               at (01,67), "Date:",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Total Required:",                            ~
               at (05,18), fac(hex(84)), requann$  (part%)      , ch(10),~
               at (05,29), "Total Issued:",                              ~
               at (05,44), fac(hex(84)), tpckqty$  (part%)      , ch(10),~
               at (05,55), "Balance Needed:",                            ~
               at (05,71), fac(hex(a4)), remquann$ (part%)      , ch(10),~
                                                                         ~
               at (06,03), "Part Number: ",                              ~
               at (06,16), fac(hex(84)), parr$(part%)           , ch(25),~
               at (06,71), fac(hex(8c)), uomdescr$(part%)       , ch(10),~
                                                                         ~
               at (07,03), fac(hex(ac)), header$                , ch(78),~
                                                                         ~
               at (08,03), fac(lfff$( 1,1)), whss$    (part%, 1), ch(03),~
               at (08,07), fac(lfff$( 1,2)), ltt$     (part%, 1), ch(06),~
               at (08,14), fac(hex(8c)),     expdate$ (part%, 1), ch(08),~
               at (08,23), fac(hex(8c)),     qtyoh$   (part%, 1), ch(10),~
               at (08,34), fac(afac$( 1)),   amp$               , ch(10),~
               at (08,36), fac(hex(8c)),     pot$     (part%, 1), ch(06),~
               at (08,43), fac(hex(8c)),     effqty$  (part%, 1), ch(10),~
               at (08,55), fac(lfff$( 1,3)), quantitt$(part%, 1), ch(10),~
               at (08,67), fac(hex(8c)),     effkit$  (part%, 1), ch(10),~
                                                                         ~
               at (09,03), fac(lfff$( 2,1)), whss$    (part%, 2), ch(03),~
               at (09,07), fac(lfff$( 2,2)), ltt$     (part%, 2), ch(06),~
               at (09,14), fac(hex(8c)),     expdate$ (part%, 2), ch(08),~
               at (09,23), fac(hex(8c)),     qtyoh$   (part%, 2), ch(10),~
               at (09,34), fac(afac$( 2)),   amp$               , ch(10),~
               at (09,36), fac(hex(8c)),     pot$     (part%, 2), ch(06),~
               at (09,43), fac(hex(8c)),     effqty$  (part%, 2), ch(10),~
               at (09,55), fac(lfff$( 2,3)), quantitt$(part%, 2), ch(10),~
               at (09,67), fac(hex(8c)),     effkit$  (part%, 2), ch(10),~
                                                                         ~
               at (10,03), fac(lfff$( 3,1)), whss$    (part%, 3), ch(03),~
               at (10,07), fac(lfff$( 3,2)), ltt$     (part%, 3), ch(06),~
               at (10,14), fac(hex(8c)),     expdate$ (part%, 3), ch(08),~
               at (10,23), fac(hex(8c)),     qtyoh$   (part%, 3), ch(10),~
               at (10,34), fac(afac$( 3)),   amp$               , ch(10),~
               at (10,36), fac(hex(8c)),     pot$     (part%, 3), ch(06),~
               at (10,43), fac(hex(8c)),     effqty$  (part%, 3), ch(10),~
               at (10,55), fac(lfff$( 3,3)), quantitt$(part%, 3), ch(10),~
               at (10,67), fac(hex(8c)),     effkit$  (part%, 3), ch(10),~
                                                                         ~
               at (11,03), fac(lfff$( 4,1)), whss$    (part%, 4), ch(03),~
               at (11,07), fac(lfff$( 4,2)), ltt$     (part%, 4), ch(06),~
               at (11,14), fac(hex(8c)),     expdate$ (part%, 4), ch(08),~
               at (11,23), fac(hex(8c)),     qtyoh$   (part%, 4), ch(10),~
               at (11,34), fac(afac$( 4)),   amp$               , ch(10),~
               at (11,36), fac(hex(8c)),     pot$     (part%, 4), ch(06),~
               at (11,43), fac(hex(8c)),     effqty$  (part%, 4), ch(10),~
               at (11,55), fac(lfff$( 4,3)), quantitt$(part%, 4), ch(10),~
               at (11,67), fac(hex(8c)),     effkit$  (part%, 4), ch(10),~
                                                                         ~
               at (12,03), fac(lfff$( 5,1)), whss$    (part%, 5), ch(03),~
               at (12,07), fac(lfff$( 5,2)), ltt$     (part%, 5), ch(06),~
               at (12,14), fac(hex(8c)),     expdate$ (part%, 5), ch(08),~
               at (12,23), fac(hex(8c)),     qtyoh$   (part%, 5), ch(10),~
               at (12,34), fac(afac$( 5)),   amp$               , ch(10),~
               at (12,36), fac(hex(8c)),     pot$     (part%, 5), ch(06),~
               at (12,43), fac(hex(8c)),     effqty$  (part%, 5), ch(10),~
               at (12,55), fac(lfff$( 5,3)), quantitt$(part%, 5), ch(10),~
               at (12,67), fac(hex(8c)),     effkit$  (part%, 5), ch(10),~
                                                                         ~
               at (13,03), fac(lfff$( 6,1)), whss$    (part%, 6), ch(03),~
               at (13,07), fac(lfff$( 6,2)), ltt$     (part%, 6), ch(06),~
               at (13,14), fac(hex(8c)),     expdate$ (part%, 6), ch(08),~
               at (13,23), fac(hex(8c)),     qtyoh$   (part%, 6), ch(10),~
               at (13,34), fac(afac$( 6)),   amp$               , ch(10),~
               at (13,36), fac(hex(8c)),     pot$     (part%, 6), ch(06),~
               at (13,43), fac(hex(8c)),     effqty$  (part%, 6), ch(10),~
               at (13,55), fac(lfff$( 6,3)), quantitt$(part%, 6), ch(10),~
               at (13,67), fac(hex(8c)),     effkit$  (part%, 6), ch(10),~
                                                                         ~
               at (14,03), fac(lfff$( 7,1)), whss$    (part%, 7), ch(03),~
               at (14,07), fac(lfff$( 7,2)), ltt$     (part%, 7), ch(06),~
               at (14,14), fac(hex(8c)),     expdate$ (part%, 7), ch(08),~
               at (14,23), fac(hex(8c)),     qtyoh$   (part%, 7), ch(10),~
               at (14,34), fac(afac$( 7)),   amp$               , ch(10),~
               at (14,36), fac(hex(8c)),     pot$     (part%, 7), ch(06),~
               at (14,43), fac(hex(8c)),     effqty$  (part%, 7), ch(10),~
               at (14,55), fac(lfff$( 7,3)), quantitt$(part%, 7), ch(10),~
               at (14,67), fac(hex(8c)),     effkit$  (part%, 7), ch(10),~
                                                                         ~
               at (15,03), fac(lfff$( 8,1)), whss$    (part%, 8), ch(03),~
               at (15,07), fac(lfff$( 8,2)), ltt$     (part%, 8), ch(06),~
               at (15,14), fac(hex(8c)),     expdate$ (part%, 8), ch(08),~
               at (15,23), fac(hex(8c)),     qtyoh$   (part%, 8), ch(10),~
               at (15,34), fac(afac$( 8)),   amp$               , ch(10),~
               at (15,36), fac(hex(8c)),     pot$     (part%, 8), ch(06),~
               at (15,43), fac(hex(8c)),     effqty$  (part%, 8), ch(10),~
               at (15,55), fac(lfff$( 8,3)), quantitt$(part%, 8), ch(10),~
               at (15,67), fac(hex(8c)),     effkit$  (part%, 8), ch(10),~
                                                                         ~
               at (16,03), fac(lfff$( 9,1)), whss$    (part%, 9), ch(03),~
               at (16,07), fac(lfff$( 9,2)), ltt$     (part%, 9), ch(06),~
               at (16,14), fac(hex(8c)),     expdate$ (part%, 9), ch(08),~
               at (16,23), fac(hex(8c)),     qtyoh$   (part%, 9), ch(10),~
               at (16,34), fac(afac$( 9)),   amp$               , ch(10),~
               at (16,36), fac(hex(8c)),     pot$     (part%, 9), ch(06),~
               at (16,43), fac(hex(8c)),     effqty$  (part%, 9), ch(10),~
               at (16,55), fac(lfff$( 9,3)), quantitt$(part%, 9), ch(10),~
               at (16,67), fac(hex(8c)),     effkit$  (part%, 9), ch(10),~
                                                                         ~
               at (17,03), fac(lfff$(10,1)), whss$    (part%,10), ch(03),~
               at (17,07), fac(lfff$(10,2)), ltt$     (part%,10), ch(06),~
               at (17,14), fac(hex(8c)),     expdate$ (part%,10), ch(08),~
               at (17,23), fac(hex(8c)),     qtyoh$   (part%,10), ch(10),~
               at (17,34), fac(afac$(10)),   amp$               , ch(10),~
               at (17,36), fac(hex(8c)),     pot$     (part%,10), ch(06),~
               at (17,43), fac(hex(8c)),     effqty$  (part%,10), ch(10),~
               at (17,55), fac(lfff$(10,3)), quantitt$(part%,10), ch(10),~
               at (17,67), fac(hex(8c)),     effkit$  (part%,10), ch(10),~
                                                                         ~
               at (18,03), fac(lfff$(11,1)), whss$    (part%,11), ch(03),~
               at (18,07), fac(lfff$(11,2)), ltt$     (part%,11), ch(06),~
               at (18,14), fac(hex(8c)),     expdate$ (part%,11), ch(08),~
               at (18,23), fac(hex(8c)),     qtyoh$   (part%,11), ch(10),~
               at (18,34), fac(afac$(11)),   amp$               , ch(10),~
               at (18,36), fac(hex(8c)),     pot$     (part%,11), ch(06),~
               at (18,43), fac(hex(8c)),     effqty$  (part%,11), ch(10),~
               at (18,55), fac(lfff$(11,3)), quantitt$(part%,11), ch(10),~
               at (18,67), fac(hex(8c)),     effkit$  (part%,11), ch(10),~
                                                                         ~
               at (19,03), fac(lfff$(12,1)), whss$    (part%,12), ch(03),~
               at (19,07), fac(lfff$(12,2)), ltt$     (part%,12), ch(06),~
               at (19,14), fac(hex(8c)),     expdate$ (part%,12), ch(08),~
               at (19,23), fac(hex(8c)),     qtyoh$   (part%,12), ch(10),~
               at (19,34), fac(afac$(12)),   amp$               , ch(10),~
               at (19,36), fac(hex(8c)),     pot$     (part%,12), ch(06),~
               at (19,43), fac(hex(8c)),     effqty$  (part%,12), ch(10),~
               at (19,55), fac(lfff$(12,3)), quantitt$(part%,12), ch(10),~
               at (19,67), fac(hex(8c)),     effkit$  (part%,12), ch(10),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,20), "(2)First Part  (4)Prev Part  (11)Append",    ~
               at (22,65), "(13)Instructions",                           ~
               at (23,20), "(3)Last Part   (5)Next Part  (12)Delete",    ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Part Summary",                           ~
                                                                         ~
               keys(hex(0001020304050b0c0f10)),                          ~
               key (keyhit%)

               if keyhit% <> 15 then L43650
                  call "PRNTSCRN"
                  goto L42205

L43650:        close ws
               call "SCREEN" addr ("C", i%, "I", i$(), cursor%())
               return


        REM *************************************************************~
            *      S U M M A R Y   /   P I C K   S C R E E N            *~
            *-----------------------------------------------------------*~
            * Summary / picking screen.                                 *~
            *************************************************************
        deffn'111
            if errormsg$ <> " " then L44060
                errormsg$ = warnmsg$
L44060:     init(hex(86)) lpck$()      /* BRITE, PROTECT, NUMERIC    */
            init(hex(84)) lfac$()      /* BRITE, PROTECT, ALL        */

            for i% = 1% to 8%
                if tpckqty$(plin% + i%) = " " then L44190
                lpck$(i%) = hex(8e)  /* DIM,  PROTECT, NUMERIC     */
                lfac$(i%) = hex(8c)  /* DIM,  PROTECT, ALL         */
L44190:     next i%

            temp$ = jbquantitt$
            call "STRING" addr("LJ", temp$, 25%)
            if jbnr$ = " " then L44280
            if jbnr$ <> " " then str(line2$,,60) = "Job ## " & jbnr$ &   ~
                        ", " & temp$ & " " & jbuomdescr$ & " of " &      ~
                        jbparr$
            goto L44310
L44280:     str(line2$,,60) = temp$ & " " & jbuomdescr$ & " of " &       ~
                            jbparr$ & " using BOM ID '" & jbbomid$ & "'"

L44310: accept                                                           ~
               at (01,02), "Formula Calculation Components Screen",      ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(ac)), colttl$                , ch(79),~
                                                                         ~
               at (06,02), fac(lpck$(1)), pick$   (plin%+1%),     ch(01),~
               at (06,04), fac(lfac$(1)), parr$   (plin%+1%),     ch(25),~
               at (06,30), fac(lfac$(1)), stdpot$ (plin%+1%),     ch( 6),~
               at (06,39), fac(lfac$(1)), bomqty$ (plin%+1%),     ch(10),~
               at (06,52), fac(lfac$(1)), requann$(plin%+1%),     ch(10),~
               at (06,65), fac(lfac$(1)), tpckqty$(plin%+1%),     ch(10),~
               at (06,77), fac(lfac$(1)), parunit$(plin%+1%),     ch(04),~
                                                                         ~
               at (07,05), fac(hex(8c)),  parrdescr$(plin%+1%),   ch(34),~
                                                                         ~
               at (08,02), fac(lpck$(2)), pick$   (plin%+2%),     ch(01),~
               at (08,04), fac(lfac$(2)), parr$   (plin%+2%),     ch(25),~
               at (08,30), fac(lfac$(2)), stdpot$ (plin%+2%),     ch(10),~
               at (08,39), fac(lfac$(2)), bomqty$ (plin%+2%),     ch(10),~
               at (08,52), fac(lfac$(2)), requann$(plin%+2%),     ch(10),~
               at (08,65), fac(lfac$(2)), tpckqty$(plin%+2%),     ch(10),~
               at (08,77), fac(lfac$(2)), parunit$(plin%+2%),     ch(04),~
                                                                         ~
               at (09,05), fac(hex(8c)),  parrdescr$(plin%+2%),   ch(34),~
                                                                         ~
               at (10,02), fac(lpck$(3)), pick$   (plin%+3%),     ch(01),~
               at (10,04), fac(lfac$(3)), parr$   (plin%+3%),     ch(25),~
               at (10,30), fac(lfac$(3)), stdpot$ (plin%+3%),     ch(10),~
               at (10,39), fac(lfac$(3)), bomqty$ (plin%+3%),     ch(10),~
               at (10,52), fac(lfac$(3)), requann$(plin%+3%),     ch(10),~
               at (10,65), fac(lfac$(3)), tpckqty$(plin%+3%),     ch(10),~
               at (10,77), fac(lfac$(3)), parunit$(plin%+3%),     ch(04),~
                                                                         ~
               at (11,05), fac(hex(8c)),  parrdescr$(plin%+3%),   ch(34),~
                                                                         ~
               at (12,02), fac(lpck$(4)), pick$   (plin%+4%),     ch(01),~
               at (12,04), fac(lfac$(4)), parr$   (plin%+4%),     ch(25),~
               at (12,30), fac(lfac$(4)), stdpot$ (plin%+4%),     ch(10),~
               at (12,39), fac(lfac$(4)), bomqty$ (plin%+4%),     ch(10),~
               at (12,52), fac(lfac$(4)), requann$(plin%+4%),     ch(10),~
               at (12,65), fac(lfac$(4)), tpckqty$(plin%+4%),     ch(10),~
               at (12,77), fac(lfac$(4)), parunit$(plin%+4%),     ch(04),~
                                                                         ~
               at (13,05), fac(hex(8c)),  parrdescr$(plin%+4%),   ch(34),~
                                                                         ~
               at (14,02), fac(lpck$(5)), pick$   (plin%+5%),     ch(01),~
               at (14,04), fac(lfac$(5)), parr$   (plin%+5%),     ch(25),~
               at (14,30), fac(lfac$(5)), stdpot$ (plin%+5%),     ch(10),~
               at (14,39), fac(lfac$(5)), bomqty$ (plin%+5%),     ch(10),~
               at (14,52), fac(lfac$(5)), requann$(plin%+5%),     ch(10),~
               at (14,65), fac(lfac$(5)), tpckqty$(plin%+5%),     ch(10),~
               at (14,77), fac(lfac$(5)), parunit$(plin%+5%),     ch(04),~
                                                                         ~
               at (15,05), fac(hex(8c)),  parrdescr$(plin%+5%),   ch(34),~
                                                                         ~
               at (16,02), fac(lpck$(6)), pick$   (plin%+6%),     ch(01),~
               at (16,04), fac(lfac$(6)), parr$   (plin%+6%),     ch(25),~
               at (16,30), fac(lfac$(6)), stdpot$ (plin%+6%),     ch(10),~
               at (16,39), fac(lfac$(6)), bomqty$ (plin%+6%),     ch(10),~
               at (16,52), fac(lfac$(6)), requann$(plin%+6%),     ch(10),~
               at (16,65), fac(lfac$(6)), tpckqty$(plin%+6%),     ch(10),~
               at (16,77), fac(lfac$(6)), parunit$(plin%+6%),     ch(04),~
                                                                         ~
               at (17,05), fac(hex(8c)),  parrdescr$(plin%+6%),   ch(34),~
                                                                         ~
               at (18,02), fac(lpck$(7)), pick$   (plin%+7%),     ch(01),~
               at (18,04), fac(lfac$(7)), parr$   (plin%+7%),     ch(25),~
               at (18,30), fac(lfac$(7)), stdpot$ (plin%+7%),     ch(10),~
               at (18,39), fac(lfac$(7)), bomqty$ (plin%+7%),     ch(10),~
               at (18,52), fac(lfac$(7)), requann$(plin%+7%),     ch(10),~
               at (18,65), fac(lfac$(7)), tpckqty$(plin%+7%),     ch(10),~
               at (18,77), fac(lfac$(7)), parunit$(plin%+7%),     ch(04),~
                                                                         ~
               at (19,05), fac(hex(8c)),  parrdescr$(plin%+7%),   ch(34),~
                                                                         ~
                                                                         ~
               at (20,02), fac(lpck$(8)), pick$   (plin%+8%),     ch(01),~
               at (20,04), fac(lfac$(8)), parr$   (plin%+8%),     ch(25),~
               at (20,30), fac(lfac$(8)), stdpot$ (plin%+8%),     ch(10),~
               at (20,39), fac(lfac$(8)), bomqty$ (plin%+8%),     ch(10),~
               at (20,52), fac(lfac$(8)), requann$(plin%+8%),     ch(10),~
               at (20,65), fac(lfac$(8)), tpckqty$(plin%+8%),     ch(10),~
               at (20,77), fac(lfac$(8)), parunit$(plin%+8%),     ch(04),~
                                                                         ~
               at (21,05), fac(hex(8c)),  parrdescr$(plin%+8%),   ch(34),~
                                                                         ~
                                                                         ~
               at (22,02), fac(hex(a4)),  inpmessage$,            ch(79),~
               at (23,02), "(1)Start Over",                              ~
               at (23,18), "(2)First",                                   ~
               at (24,18), "(3)Last",                                    ~
               at (23,30), "(4)Previous",                                ~
               at (24,30), "(5)Next",                                    ~
               at (23,45), "(14)Print Kit List",                         ~
               at (24,45), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), "(16)Exit Program",                           ~
                                                                         ~
                                                                         ~
               keys(hex(0001020304050d0e0f10)),                          ~
               key(keyhit%)

               if keyhit% <> 13% then L45450
                  call "MANUAL" ("JBPOTPRT")
                  goto L44310

L45450:        if keyhit% <> 15% then L45490
                  call "PRNTSCRN"
                  goto L44310

L45490:        close ws
               call "SCREEN" addr ("C", i%, "I", i$(), cursor%())
               return


        REM *************************************************************~
            *      H E A D E R   S C R E E N                            *~
            *-----------------------------------------------------------*~
            * Header Screen                                             *~
            *************************************************************
        deffn'101(fieldnr%)

            str(line2$,,60) = " "
            if fieldnr% > 1% then pf16$ = " "
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
              on fieldnr% gosub L46160,         /* Job Number        */   ~
                                L46160,         /* Assy # / BOMID    */   ~
                                L46170          /* Quantity          */
              goto L46190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L46160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L46170:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L46190: accept                                                           ~
               at (01,02), "Formula Calculation Header",                 ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$,                   ch(08),~
               at (02,02), fac(hex(ac)), line2$,                  ch(79),~
                                                                         ~
               at (04,02), fac(hex(94)), errormsg$,               ch(79),~
                                                                         ~
               at (06,02), "Job Number ",                                ~
               at (06,20), fac(lfac$(1)), jbnr$                 , ch(08),~
               at (06,47), fac(hex(8c)), jbdescr$               , ch(32),~
                                                                         ~
               at (07,02), "Assembly Number ",                           ~
               at (07,20), fac(lfac$(2)), jbparr$               , ch(25),~
               at (07,47), fac(hex(8c)), jbparrdescr$           , ch(23),~
                                                                         ~
               at (08,02), "BOM ID",                                     ~
               at (08,20), fac(lfac$(2)), jbbomid$              , ch(03),~
               at (08,47), fac(hex(8c)), jbbomtext$             , ch(26),~
                                                                         ~
               at (09,02), "Quantity",                                   ~
               at (09,20), fac(lfac$(3)), jbquantitt$           , ch(10),~
               at (09,31), fac(hex(8c)), jbuomdescr$            , ch(21),~
                                                                         ~
               at (21,02), fac(hex(a4)),  inpmessage$,            ch(79),~
               at (22,02), "(1)Start Over",                              ~
               at (22,65), "(13)Instructions",                           ~
               at (23,65), "(15)Print Screen",                           ~
               at (24,65), fac(hex(8c)), pf16$,                   ch(16),~
                                                                         ~
               keys(hex(01020304050d0f1000)),                            ~
               key(keyhit%)

               if keyhit% <> 13% then L46540
                  call "MANUAL" ("JBPOTPRT")
                  goto L46190

L46540:        if keyhit% <> 15% then L46580
                  call "PRNTSCRN"
                  goto L46190

L46580:        close ws
               call "SCREEN" addr ("C", i%, "I", i$(), cursor%())
               return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Tests data for Header Screen                              *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub                                      ~
                                    L50300,         /* Job Number       */~
                                    L50500,         /* Assy # / BOMID   */~
                                    L51340          /* Quantity         */
                     return

L50300: REM Test data for job order
            if jbnr$ = " " then return
            if jbnr$ = "?" then jbnr$ = " "
            call "GETCODE" (#4, jbnr$, jbdescr$, 0%, 0, f1%(4))
                if f1%(4) = 1 then L50460
                if jbnr$ = " " then return
                errormsg$ = "Enter VALID Job Number or Blank"
                return
L50460:     return

L50500: REM Test data for Assembly Number
            readkey$ = jbparr$
            if jbbomid$ = "?  " then jbbomid$ = "   "
            str(readkey$,26,3) = jbbomid$
            bomhdr$(1), bomhdr$(3) = " "
            bomhdr$(2) = "  Assembly Number        BOMID"  &             ~
                         "   Assembly Description"
            bomhdr$(3) = "Select an Assembly Number for Use" &           ~
                         " - or -  PF-16 to Cancel Selection"
            call "PLOWCODE" (#30, readkey$, jbbomtext$, -1028%, -.30,    ~
                             f1%(30), bomhdr$())
                if f1%(30) = 0% then L51260
            jbparr$ = str(readkey$,,25)
            jbbomid$ = str(readkey$,26,3)
            call "GETCODE"(#3, jbparr$, jbparrdescr$, 0%, 0, f1%(3))
            get #3, using L50780, jbparrunit$
L50780:         FMT POS(74), CH(4)

            readkey$ = "UOM      " & jbparrunit$
            call "DESCRIBE" (#8, readkey$, jbuomdescr$, 0%, f1%(8))
            return

            get #30 using L51220, textidh$
L51220:         FMT POS(95), CH(4)
                return

L51260:     errormsg$ = "Please Enter a Valid Assembly / BOMID"
            return


L51340: REM Test data for quantity
            call "NUMTEST" (jbquantitt$, .01, 9e7, errormsg$, -1.4, jbqty)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'41 (part%, line%, thisfld%)
                  errormsg$, warnmsg$ = " "
                  on thisfld% gosub                                      ~
                                    L53140,         /* Store            */~
                                    L53170,         /* Lot              */~
                                    L53480          /* Quantity         */
                     return

L53140: REM Test data for warehouse
            if whss$(part%,line%) = " " then L53148
            readkey$ = whss$(part%,line%)
            call "DESCRIBE" (#6, readkey$, temp$, 0%, f1%(6))
                if f1%(6) = 1% then return
            errormsg$ = "Store '" & whss$(part%,line%) & "' is not on" & ~
                        " File, Please Re-Enter"
            return
L53148:     errormsg$ = "Store Cannot be Blank"
            return

L53170: REM Test data for lot
            plowkey$ = parr$(part%)
            str(plowkey$,26,3) = whss$(part%,line%)

            str(plowkey$,29,16) = ltt$(part%,line%)
            plowdescr$ = hex(06) & "Please select a lot for part '"      ~
                         & parr$(part%) & "' from those shown below"

            call "PLOWCODE" (#11, plowkey$, plowdescr$, 9025%, 0.01,     ~
                  f1%(11), hdr$(), 0, 0, i_x(), i_x$(), "D", "Y", #7,    ~
                  dsc_map())
                if f1%(11) = 1% then L53320
            errormsg$ = "You Must Select a Valid Lot for This Part"
            return

L53320:     get #11 using L53350, whss$(part%, line%), ltt$(part%, line%),~
                    qoh(part%, line%), expdate$(part%, line%),           ~
                    potency(part%, line%)
L53350:         FMT POS(42), CH(3), CH(16), POS(69), PD(14,4),           ~
                    POS(404), CH(6), PD(14,4)
            call "DATEFMT" (expdate$(part%, line%))
            call "CONVERT"(potency(part%,line%), -1.4, pot$(part%,line%))
            pline% = line%
L53382:     pline% = (pline% - 1%)
            if pline% = 0% then L53399
                if whss$(part%,line%) <> whss$(part%,pline%) then L53382
                    if ltt$(part%,line%) <> ltt$(part%,pline%) then L53382
                convert quantitt$(part%,pline%) to last_kit
                convert qtyoh$(part%,pline%)    to last_qtyoh
                qoh(part%,line%) = (last_qtyoh - last_kit)
            if qoh(part%,line%) > 0 then L53399
                errormsg$ = "Invalid lot, quantity available is zero"
                return
L53399:     call "CONVERT"(qoh(part%,line%), 1.4, qtyoh$(part%,line%))
            if potency(part%, line%) > 0 and stdpot(part%) > 0 then      ~
               factor(part%,line%) =stdpot(part%)/potency(part%,line%)   ~
             else                                                        ~
               factor(part%,line%) = 1.0
            call"CONVERT"(factor(part%,line%),2.6,potmult$(part%,line%))
            temp1 = qoh(part%,line%) * potency(part%,line%)
            call "CONVERT" (temp1, 1.4, effqty$(part%,line%))
            if edt% = 1% then gosub L53480       /*  reset qty  */
            return

L53480: REM Test data for quantity
            call "NUMTEST" (quantitt$(part%,line%), 0, 9e7, errormsg$,   ~
                           -0.4, temp)
                if errormsg$ <> " " then return
            for x% = 1% to line%(part%)
                convert qtyoh$(part%,x%) to last_qtyoh
                convert quantitt$(part%,x%) to last_kit
                for z% = (x%+1%) to line%(part%)
                     if z% > line%(part%) then L53640
                     if ltt$(part%,x%) <> ltt$(part%,z%) then L53630
                     qoh(part%,z%) = last_qtyoh - last_kit
                     call "CONVERT" (qoh(part%,z%), 2.4, qtyoh$(part%,z%))
                     effqty = qoh(part%,z%) * potency(part%,z%)
                     call "CONVERT" (effqty, 2.4, effqty$(part%,z%))
                     goto L53640
L53630:         next z%
L53640:     next x%
            if temp <= qoh(part%,line%) then L53675
                errormsg$ = "Exceeds Quantity Available From This Lot"
                return
L53675:     effkit = temp * potency(part%,line%)
            call "CONVERT" (effkit, 2.4, effkit$(part%,line%))
            convert quantitt$(part%,line%) to qtt(part%,line%)
            tpckqty = 0
            for x% = 1% to line%(part%)+1%
                if effkit$(part%,x%) = " " then L53740
                convert effkit$(part%,x%) to effkit
                tpckqty = tpckqty + effkit
L53740:     next x%
            call "CONVERT" (tpckqty, 2.4, tpckqty$(part%))
            if tpckqty = 0 then tpckqty$(part%) = " "
            convert requann$(part%) to requann
            remquann = requann - tpckqty
            call "CONVERT" (remquann, 2.4, remquann$(part%))
            if remquann >= 0 then L53830
                warnmsg$ = "WARNING: Effective Quantity issued exceeds t"~
                         & "he Job's Requirments."
L53830:     if edt% = 0% then return
            for x% = 1% to line%(part%)
                if qoh(part%,x%) <= 0 and qtt(part%,x%) > 0% then L53880
            next x%
            return
L53880:     currentline% = x%
            errormsg$ = "Last Editing Activity has caused this line's Qt"~
                      & "y Aval. to become Zero."
            return

        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
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
            * COPYRIGHT (C) 1982, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please...")
            end
