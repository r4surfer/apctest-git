        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBB    CCC   K   K   CCC   RRRR   IIIII  N   N  PPPP    *~
            *  B   B  C   C  K  K   C   C  R   R    I    NN  N  P   P   *~
            *  BBBB   C      KKK    C      RRRR     I    N N N  PPPP    *~
            *  B   B  C   C  K  K   C   C  R   R    I    N  NN  P       *~
            *  BBBB    CCC   K   K   CCC   R   R  IIIII  N   N  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKCRINP - Allows management of Credit Hold on Sales      *~
            *            Orders.                                        *~
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
            * 07/17/86 ! Original                                 ! ERN *~
            * 12/02/88 ! Teroson Credit Family Exposure Added     ! JDH *~
            * 05/24/90 ! Shortened time to load sceen, corrected  ! JDH *~
            *          !  resetting of Demand Status for released !     *~
            *          !  orders.                                 !     *~
            * 05/28/91 ! PRR 11906 Acknowledgement @ Cr Release.  ! JIM *~
            * 01/23/92 ! PRR 11905,11249,11623 Set How Held to 'M'! JDH *~
            *          ! PRR 12111 Display SOs with Open Qty even !     *~
            *          !  if Order Amount is $0.00.               !     *~
            * 11/13/92 ! Cust Credit- Dynamic fields from CCRMASTR! JIM *~
            * 01/05/93 ! Screen Header amts rounded per BCKCRHLD. ! JIM *~
            * 03/21/95 ! PRR 13143. Honor Cust Print Acks flag too! JDH *~
            *          ! PRR 13245. Added call to ARQCUSCR.       !     *~
            * 07/29/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            acknwldg$1,                  /* Print ACKs at BCKCRINP?    */~
            acks_cust$1,                 /* Print ACKs Flag for Cust.  */~
            bckpridxkey$29,              /* Key to BCKPRIDX file       */~
            billto$9,                    /* Bill-to Customer           */~
            billtoname$30,               /* Bill-to Customer Name      */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            cr$1,                        /* Order Credit Status        */~
            date$8,                      /* Date for screen display    */~
            demkey$19,                   /* Demand Key                 */~
            demstatus$1,                 /* Demand Status              */~
            dfltdue$8,                   /* Default Due Date           */~
            didx%(15),                   /* Display Index              */~
            errormsg$79,                 /* Error message              */~
            hdr1$1, hdr2$8, hdr3$9,      /* Column Headers             */~
            hdr4$30, hdr5$16, hdr6$10,   /*                            */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            order$8,                     /* Order Date                 */~
            parent_code$9,               /* Credit Parent's Code       */~
            pf$(3)79, pfkey$20,          /* PF Keys                    */~
            plowkey$50,                  /* Plowkey Variable           */~
            plowkey2$50,                 /* Plowkey Variable           */~
            shipto$9, shiptoname$30,     /* Ship-to Customer           */~
            so$16,                       /* Sales Order Number         */~
            xref$9                       /* Bill-to X-Ref              */

        dim idx$ (3200)10,               /* Order Index Array          */~
            info$(3200)80,               /* Order Information Array    */~
            oo   (3200),                 /* Open Order Amounts         */~
            od(2), ar(2),                /* Amounts for Family Exposure*/~
            store$(3200)3, store$3       /* Store/Whse #s for BCKPRIDX */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32)                      /* = 1 if file open, -1 if it */
                                         /*   doesn't exist, or 0 if   */
                                         /*   not yet checked (OPENCHCK*/

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
            * #1  ! CUSTOMER ! Customer Master File                     *~
            * #2  ! BCKMASTR ! Backlog master file                      *~
            * #3  ! DEMMASTR ! Demand Master File                       *~
            * #4  ! BCKLINES ! Back Log Line Item File                  *~
            * #8  ! BCKPRIDX ! BCK Print Index File                     *~
            * #9  ! CCRMASTR ! Customer Credit Master file              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #2,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #3,  "DEMMASTR",                                      ~
                        varc,     indexed,  recsize = 123,               ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28

            select #4,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #8,  "BCKPRIDX",                                      ~
                        varc,     indexed,  recsize =  45,               ~
                        keypos =  11,   keylen =  29,                    ~
                        alt key  1, keypos =    1, keylen =  39

            select #9,  "CCRMASTR",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen =   9

        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1%), f2%(1%), 0%, " ")
            call "OPENCHCK" (#2,  fs%(2%), f2%(2%), 0%, " ")
            call "OPENCHCK" (#3,  fs%(3%), f2%(3%), 0%, " ")
            call "OPENCHCK" (#4,  fs%(4%), f2%(4%), 0%, " ")
            call "OPENCHCK" (#9,  fs%(9%), f2%(9%), 0%, " ")

            if fs%(1) < 0% or fs%(2) < 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" ( blankdate$ )

            date$ = date
            call "DATEFMT" (date$)

            siz% = dim(idx$(),1) /* Number of elements in arrays  */
            m%   = siz% - 15%

            str(line2$,63%) = "BCKCRINP:" & str(cms2v$,,8%)

            hdr1$ = "S"
            hdr2$ = "Dflt Due"
            hdr3$ = "Ship-to"
            hdr4$ = "Ship-to Customer Name"
            hdr5$ = "Sales Order"
            hdr6$ = "  Open Amt"
            call "BCKSWTCH" ("BCK", "ACKNWLDG", acknwldg$, 0, u3%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Get Bill-to to work on.                                   *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, billto$, billtoname$,      ~
                      idx$(), info$(), store$(), store$
            call "ALLFREE"
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "             (16)Exit Program"
           pfkey$ = hex(01ffffffffffffffffffffff0dff0f10ffffff00)
           line2$ = " " : str(line2$,63%) = "BCKCRINP:" & str(cms2v$,,8%)

            for fieldnr% = 1% to 1%
                gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10430
L10330:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10330
L10430:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10330
            next fieldnr%

            gosub load_orders
            l% = 0%


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles display of orders and management of statuses.     *~
            *************************************************************

        editpg1
           pf$(1) = "(1)Start Over              (8)Release to Credit Li"&~
                    "mit          (13)Instructions"
           pf$(2) = "(2)First (4)Prev (6)Down   (9)Release All Orders  "&~
                    "             (15)Print Screen"
           pf$(3) = "(3)Last  (5)Next (7)Up    (10)Place All On Hold (2"&~
                    "6)Cus Credit (16)Save Data   "
           pfkey$ = hex(0102030405060708090affff0dff0f10ffff1a00)
           line2$ = billto$ & "  (" & billtoname$ & ")"
           str(line2$,45%,17%) = "Parent: " & parent_code$
           str(line2$,63%) = "BCKCRINP:" & str(cms2v$,,8%)
           if l% <> 0% then L11200
                str(pf$(2), 1,9) = " " : str(pfkey$,2,1) = hex(ff)
                str(pf$(2),10,7) = " " : str(pfkey$,4,1) = hex(ff)
                str(pf$(2),18,7) = " " : str(pfkey$,6,1) = hex(ff)
L11200:    if l% + 14% < maxlines% then L11240
                str(pf$(3), 1,9) = " " : str(pfkey$,3,1) = hex(ff)
                str(pf$(3),10,7) = " " : str(pfkey$,5,1) = hex(ff)
                str(pf$(3),18,7) = " " : str(pfkey$,7,1) = hex(ff)
L11240:     inpmessage$ = "Press (RETURN) to update Numbers. [#####"  &  ~
                          " Orders, Display starts at Order #####]"
                convert maxlines% to str(inpmessage$,36,5), pic(#####)
                convert l% + 1%   to str(inpmessage$,74,5), pic(#####)
            gosub update_numbers

        gosub'102
          if keyhit%  =  1 then gosub startover
          if keyhit%  =  2 then l% = 0%
          if keyhit%  =  3 then l% = max(0%, min(m%, maxlines% - 14%))
          if keyhit%  =  4 then l% = max(0%, l% - 14%)
          if keyhit%  =  5 then l% = max(0%, min(m%,l%+14%,maxlines%-14%))
          if keyhit%  =  6 then l% = max(0%, l% - 1%)
          if keyhit%  =  7 then l% = max(0%, min(m%,l%+1%,maxlines%-1%))
          if keyhit%  =  8 then       release_to_credit_limit
          if keyhit%  =  9 then       release_all_orders
          if keyhit%  = 10 then       hold_all_orders
          if keyhit%  = 16 then       datasave
          if keyhit% <>  0 then       editpg1
        goto editpg1

        release_to_credit_limit
            gosub update_numbers
            if avail <= 0 then editpg1
            for c% = 1% to maxlines%
                if str(info$(c%),2,1) = " " or oo(c%) > avail then L11490
                     avail = round(avail - oo(c%), 0)
                     str(info$(c%),2,1) = " "
L11490:              if avail <= 0 then editpg1
            next c%
            goto editpg1

        release_all_orders
            for c% = 1% to maxlines%
                str(info$(c%),2,1) = " "
            next c%
            goto editpg1

        hold_all_orders
            for c% = 1% to maxlines%
                str(info$(c%),2,1) = "H"
            next c%
            goto editpg1

        update_numbers
            held, rlsd = 0
            for c% = 1% to maxlines%
                if str(info$(c%),2,1) = " "                              ~
                     then rlsd = round(rlsd + oo(c%), 0)                 ~
                     else held = round(held + oo(c%), 0)
            next c%
            avail = round(limit - (fam_expsr - oo + rlsd), 0)
            return


        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub save_orders
            goto  inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20100          /* Bill-to Customer */
                     return
L20100:     REM Bill-to Customer                      BILLTO$
                return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads orders for the Bill-to Customer.                    *~
            *************************************************************
        load_orders
            maxlines%, c% = 0%
            call "SHOSTAT" ("Loading Orders for Bill-to...")
            call "REDALT0" (#1, billto$, 4%, f1%(1)) /* Primer Read    */
            if f1%(1) = 0% then end_load

        shipto_loop   /* Loop to get ship-tos for this bill-to */
            get #1 using L30130, shipto$, shiptoname$, xref$
L30130:         FMT CH(9), CH(30), POS(780), CH(9)
            if xref$ <> billto$ then next_shipto

*         Now get open orders for this ship-to
            plowkey$ = shipto$
          order_loop
            call "PLOWNEXT" (#2, plowkey$, 9%, f1%(2))
            if f1%(2) = 0% then next_shipto
                get #2 using L30220, so$, store$, order$, dfltdue$, disc, ~
                     opn, cr$
L30220:              FMT XX(9), CH(16), POS(803), CH(3), CH(6), XX(6),   ~
                          CH(6), POS(859), 2*PD(14,4), CH(1)
                opn = opn - round(opn * disc * .01, 2)
                if opn = 0 then line_test
L30260:              if dfltdue$ = " " or dfltdue$ = blankdate$ ~
                                         then dfltdue$ = order$
                     maxlines%, c% = c% + 1%
                     put idx$(c%) using L30290, dfltdue$, c%
L30290:                   FMT CH(6), BI(4)
                     call "DATEFMT" (dfltdue$)
                     oo(c%) = opn
                     str(info$(c%),  1,  1) = cr$
                     str(info$(c%),  2,  1) = cr$
                     str(info$(c%),  4,  8) = dfltdue$
                     str(info$(c%), 13,  9) = shipto$
                     str(info$(c%), 23, 30) = shiptoname$
                     str(info$(c%), 54, 16) = so$
                     store$(c%)             = store$
                     call "CONVERT" (opn, 2.2, str(info$(c%), 71, 10))
                     goto order_loop

            next_shipto
                call "READNEXT" (#1, f1%(1))
                if key (#1, 4%) <> billto$ then end_load
                if f1%(1) = 1% then shipto_loop

        end_load     /* We're finished- see if we've got anything      */
            if maxlines% > 0% then L30540
                call "ASKUSER" (2%, "NO ORDERS",                         ~
                         "There are no Open Orders for this Bill-to",    ~
                         " ", "Press any PF-Key to continue.")
                return clear all
                goto inputmode

*        Sort the Index array
L30540:     if maxlines% = 1% then return
                call "SORT" addr(str(idx$()), maxlines%, 10%)
                return

        line_test
            plowkey2$ = so$
          line_loop
            call "PLOWNEXT" (#4, plowkey2$, 16%, f1%(4))
            if f1%(4) = 0% then order_loop
                get #4 using L30640, qty_open
L30640:              FMT POS(109), PD(14,4)
                if qty_open > 0 then L30260  /* Got something, show it! */
                goto line_loop

        REM *************************************************************~
            *                    S A V E   D A T A                      *~
            *-----------------------------------------------------------*~
            * Updates orders with changes to credit status.             *~
            *************************************************************
        save_orders
            call "SHOSTAT" ("Saving Orders and Updating Demands...")
            for c% = 1% to maxlines%
                if str(info$(c%),1,1) <> " " then str(info$(c%),1,1) = "H"
                if str(info$(c%),2,1) <> " " then str(info$(c%),2,1) = "H"
                if str(info$(c%),1,1) = str(info$(c%),2,1) then L31370
                     gosub acknowledgement_tester
                     plowkey$ = str(info$(c%),13,9) & str(info$(c%),54,16)
                     call "READ101" (#2, plowkey$, f1%(2))
                     if f1%(2) = 0% then L31370
                          put #2 using L31150, str(info$(c%),2,1), "M"
L31150:                        FMT POS(875), CH(1), POS(897), CH(1)
                          rewrite #2

                /* Change approval statuses in the Demand Master File  */
                     demkey$ = str(info$(c%),54,16) & hex(00)
L31200:              call "PLOWAL1" (#3, demkey$, 1%, 16%, f1%(3))
                     if f1%(3) = 0% then L31370
                          get #3 using L31230, demstatus$
L31230:                        FMT CH(1)
                          if str(info$(c%),2,1) = "H" then L31300
                           /* Set any unapproved demands to approved */
                               if demstatus$ = " " then demstatus$ = "1"
                               if demstatus$ = "6" then demstatus$ = "7"
                               if demstatus$ = "8" then demstatus$ = "9"
                               goto L31340
L31300:                    /* Set any approved demands to unapproved */
                               if demstatus$ = "1" then demstatus$ = " "
                               if demstatus$ = "7" then demstatus$ = "6"
                               if demstatus$ = "9" then demstatus$ = "8"
L31340:                    /* Now rewrite demand                     */
                               put #3 using L31230, demstatus$
                               rewrite #3
                               goto L31200   /* Plow through lines */
L31370:     next c%
            return

        acknowledgement_tester        /* Try to write ACKs to BCKPRIDX */
            if acknwldg$ <> "Y" then return   /* Behavior Sw says 'no' */
            if acks_cust$ = "N" then return   /* Customer Sw says 'no' */
            if str(info$(c%),1,1) <> "H" then return /* Wasn't on Hold */
            if str(info$(c%),2,1) <> " " then return /* Wasn't Releasd */
*        All indicators say we should write an ACK record to BCKPRIDX.
            if f2%(8) = 1% then        /* Need to Open/Create BCKPRIDX */~
                call "OPENCHCK" (#8, fs%(8), f2%(8), 100%, " ")
            if fs%(8) < 1% then return                      /* Bummer! */
            bckpridxkey$ = "A" & str(info$(c%),13,9) &                   ~
                str(info$(c%),54,16)
            call "READ101" (#8, bckpridxkey$, f1%(8))
            put #8 using L31521, "A", store$(c%), " ", "A",               ~
                str(info$(c%),13,9), str(info$(c%),54,16), " ", date
L31521:         FMT CH(1), CH(3), CH(6), CH(1), CH(9), CH(16), CH(3),    ~
                     CH(6)
            if f1%(8) = 0% then write #8 else rewrite #8
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Enter Bill-to to maintain orders for.                     *~
            *************************************************************

            deffn'101(fieldnr%)
                init (hex(8c)) lfac$()

                on fieldnr% gosub L40180            /* Bill-to Customer */
                goto L40250

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40180:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40250:     accept                                                       ~
               at (01,02), "Sales Order Credit Hold Management",         ~
               at (01,67), "Today",                                      ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Bill-to Customer",                           ~
               at (06,30), fac(lfac$( 1)), billto$              , ch(09),~
               at (06,49), fac(hex(8c)),   billtoname$          , ch(32),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

               if keyhit% <> 13 then L40540
                  call "MANUAL" ("BCKCRINP")
                  goto L40250

L40540:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40250

        REM *************************************************************~
            *                 O R D E R   D I S P L A Y                 *~
            *-----------------------------------------------------------*~
            * Screen to display order and allow management of credit    *~
            * statuses.                                                 *~
            *************************************************************

        deffn'102    /* Note: L% = Top line element offset */
            init (hex(8c)) lfac$()
            mat didx% = con  :  mat didx% = (m%) * didx%
            for dl% = 1% to 15%          /* DL% = Display Line */
                c% = dl% + l%            /* IDX$() Element     */
                if c% > maxlines% then L41150
                     get idx$(c%) using L41130, didx%(dl%)
L41130:                   FMT XX(6), BI(4)
                     lfac$(dl%) = hex(81)
L41150:     next dl%

L41170: accept                                                           ~
            at (01,02), "Sales Order Credit Hold Management",            ~
            at (01,67), "Today",                                         ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
                                                                         ~
            at (03,02), "Orders:",                                       ~
            at (03,10), fac(hex(84)),   oo            , pic(-##,###,###),~
            at (04,23), "A/R:",                                          ~
            at (04,28), fac(hex(84)),   ar            , pic(-##,###,###),~
            at (03,23), "Orders On Hold:",                               ~
            at (03,39), fac(hex(84)),   held          , pic(-##,###,###),~
            at (03,53), "Orders Released:",                              ~
            at (03,70), fac(hex(84)),   rlsd          , pic(-##,###,###),~
            at (04,02), "Cr Lim:",                                       ~
            at (04,10), fac(hex(84)),   limit         , pic(-##,###,###),~
            at (04,40), "Fam Expsr:",                                    ~
            at (04,51), fac(hex(84)),   fam_expsr     , pic(-##,###,###),~
            at (04,63), "Avail:",                                        ~
            at (04,70), fac(hex(84)),   avail         , pic(-##,###,###),~
                                                                         ~
            at (05,02), fac(hex(ac)),   hdr1$,                           ~
            at (05,04), fac(hex(ac)),   hdr2$,                           ~
            at (05,13), fac(hex(ac)),   hdr3$,                           ~
            at (05,23), fac(hex(ac)),   hdr4$,                           ~
            at (05,54), fac(hex(ac)),   hdr5$,                           ~
            at (05,71), fac(hex(ac)),   hdr6$,                           ~
                                                                         ~
            at (06,02), fac(lfac$( 1)),  str(info$(didx%( 1)),2,1),      ~
            at (07,02), fac(lfac$( 2)),  str(info$(didx%( 2)),2,1),      ~
            at (08,02), fac(lfac$( 3)),  str(info$(didx%( 3)),2,1),      ~
            at (09,02), fac(lfac$( 4)),  str(info$(didx%( 4)),2,1),      ~
            at (10,02), fac(lfac$( 5)),  str(info$(didx%( 5)),2,1),      ~
            at (11,02), fac(lfac$( 6)),  str(info$(didx%( 6)),2,1),      ~
            at (12,02), fac(lfac$( 7)),  str(info$(didx%( 7)),2,1),      ~
            at (13,02), fac(lfac$( 8)),  str(info$(didx%( 8)),2,1),      ~
            at (14,02), fac(lfac$( 9)),  str(info$(didx%( 9)),2,1),      ~
            at (15,02), fac(lfac$(10)),  str(info$(didx%(10)),2,1),      ~
            at (16,02), fac(lfac$(11)),  str(info$(didx%(11)),2,1),      ~
            at (17,02), fac(lfac$(12)),  str(info$(didx%(12)),2,1),      ~
            at (18,02), fac(lfac$(13)),  str(info$(didx%(13)),2,1),      ~
            at (19,02), fac(lfac$(14)),  str(info$(didx%(14)),2,1),      ~
            at (20,02), fac(lfac$(15)),  str(info$(didx%(15)),2,1),      ~
                                                                         ~
            at (06,04), fac(hex(84)) ,  str(info$(didx%( 1)),4),         ~
            at (07,04), fac(hex(84)) ,  str(info$(didx%( 2)),4),         ~
            at (08,04), fac(hex(84)) ,  str(info$(didx%( 3)),4),         ~
            at (09,04), fac(hex(84)) ,  str(info$(didx%( 4)),4),         ~
            at (10,04), fac(hex(84)) ,  str(info$(didx%( 5)),4),         ~
            at (11,04), fac(hex(84)) ,  str(info$(didx%( 6)),4),         ~
            at (12,04), fac(hex(84)) ,  str(info$(didx%( 7)),4),         ~
            at (13,04), fac(hex(84)) ,  str(info$(didx%( 8)),4),         ~
            at (14,04), fac(hex(84)) ,  str(info$(didx%( 9)),4),         ~
            at (15,04), fac(hex(84)) ,  str(info$(didx%(10)),4),         ~
            at (16,04), fac(hex(84)) ,  str(info$(didx%(11)),4),         ~
            at (17,04), fac(hex(84)) ,  str(info$(didx%(12)),4),         ~
            at (18,04), fac(hex(84)) ,  str(info$(didx%(13)),4),         ~
            at (19,04), fac(hex(84)) ,  str(info$(didx%(14)),4),         ~
            at (20,04), fac(hex(84)) ,  str(info$(didx%(15)),4),         ~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)),   pf$(1)                  , ch(79),~
            at (23,02), fac(hex(8c)),   pf$(2)                  , ch(79),~
            at (24,02), fac(hex(8c)),   pf$(3)                  , ch(79),~
                keys(pfkey$), key(keyhit%)

               if keyhit% <> 26% then L41800
                  call "ARQCUSCR" (billto$)
                  goto L41170

L41800:        if keyhit% <> 13% then L41840
                  call "MANUAL" ("BCKCRINP")
                  goto L41170

L41840:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L41170

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50100          /* Bill-to Customer */
                  return

L50100
*        Bill-to Customer                      BILLTO$
            call "GETCODE" (#1, billto$, billtoname$, 0%, 1.30, f1%(1))
            if f1%(1) = 1% then L50140
                errormsg$ = "Bill-to Not on File." : return
L50140:     get #1 using L50160, acks_cust$, limit, xref$, parent_code$
L50160:         FMT POS(238), CH(1), POS(526), PD(14,4), POS(780), CH(9),~
                    POS(1049), CH(9)
            limit = round(limit, 0)
            oo, ar = 0                                          /* JIC */
            call "READ100" (#9, billto$, f1%(9%))          /* CCRMASTR */
            if f1%(9%) <> 0% then get #9 using L50174, oo, ar
L50174:         FMT POS(114), PD(14,4), XX(8), PD(14,4)
            oo = round(oo, 0)
            ar = round(ar, 0)
            if xref$ = billto$ then L50210
                errormsg$ = "Customer is not a Bill-to Customer."
                return
L50210:     par% = 0%
            call "BCKCRDSB" (billto$, od(), ar(), limit, par%)
            oo(2) = round(oo(2), 0) : ar(2) = round(ar(2), 0)
            fam_expsr = round(od(2) + ar(2), 0)
            if par% = 1% then parent_code$ = "*** P ***"
            return


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
            call "SHOSTAT" ("One Moment Please")

            end
