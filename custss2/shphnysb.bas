        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   H   H  PPPP   H   H  N   N  Y   Y   SSS   BBBB    *~
            *  S      H   H  P   P  H   H  NN  N  Y   Y  S      B   B   *~
            *   SSS   HHHHH  PPPP   HHHHH  N N N   YYY    SSS   BBBB    *~
            *      S  H   H  P      H   H  N  NN    Y        S  B   B   *~
            *   SSS   H   H  P      H   H  N   N    Y     SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SHPHNYSB - Shipping-Inventory Interface Subroutine.       *~
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
            * 08/06/86 ! Original                                 ! ERN *~
            * 01/28/87 ! Disallow blank G/L Accts                 ! ERN *~
            * 05/11/87 ! Standard Cost Enhancements (BIG changes) ! ERN *~
            * 09/29/87 ! Fixed zero costing before posting        ! DAW *~
            * 10/27/88 ! Lengthen HNYSHIP$ for formatted accounts ! KAB *~
            * 03/01/89 ! Allow for Store change                   ! KAB *~
            * 08/09/90 ! G/L export file modifications            ! RAC *~
            * 11/15/90 ! Add Management DMC to SHPHNYTF & SHPCOSTS! JDH *~
            * 07/24/92 ! MPS/PFM - added call to HNYUSESB.        ! MLJ *~
            * 07/22/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        sub "SHPHNYSB"   (caller$,       /* 'P' = Pre-Invoicing        */~
                                         /* 'I' = Invoicing            */~
                          store$,        /* Store Shipped From         */~
                          part$,         /* Part Number                */~
/*PAR000*/                subp$,         /* Subpart number             */~
                          seqnr$,        /* Line Sequence Number       */~
                          soseqnr$,      /* SO/BOL Sequence Number     */~
                          cuscode$,      /* Ship-to Number             */~
                          so$,           /* Sales Order Number         */~
                          bol$,          /* Bill of Lading Number      */~
                          invnr$,        /* Invoice Number             */~
                          price,         /* Unit Price (Stking UOM)    */~
                          postdate$,     /* Posting Date (YYMMDD)      */~
                          oldlot$(),     /* OLD Lot Numbers            */~
                          oldqty(),      /*     Quantities             */~
                          newlot$(),     /* NEW Lot Numbers            */~
                          newqty(),      /*     Quantities             */~
                          newcost,       /* New TOTAL Inventory Cost   */~
                          origuser$,     /* User who created trans     */~
                          #1,            /* SHPHNYTF Channel Number    */~
                          #2,            /* SYSFILE2                   */~
                          #3,            /* WORKFILE for HNYPOST2      */~
                          #4,            /* INVQUAN                    */~
                          #5,            /* INVDETAL                   */~
                          #6,            /* INVPOOL                    */~
                          #7,            /* INVMASTR                   */~
                          #8,            /* PIPMASTR                   */~
                          #9,            /* SFCUM2                     */~
                          #10,           /* GLMAIN                     */~
                          #11 )          /* GLDETAIL                   */


        dim                                                              ~
            acct$9,                      /* General Purpose Acct Code  */~
            bol$3,                       /* Bill of Lading Number      */~
            caller$1,                    /* Caller ID                  */~
            cogs$9,                      /* Cost of Goods Account      */~
            costs(12),                   /* Work array for costs       */~
            craccts$(8)9, crs(8),        /* Credit Information         */~
            crtype$(8)1,                 /* Type of Credit account     */~
            cuscode$9,                   /* Ship-to Customer Number    */~
            datetime$7,                  /* Date/Time Stamp            */~
            doc$20,                      /* Formatted Document ID      */~
            draccts$(8)9, drs(8),        /* Debit Information          */~
            drtype$(8)1,                 /* Type of Debit account      */~
            file$1,                      /* SHPHNYTF Work File ID      */~
            hnyactve$1,                  /* Inventory Active Flag      */~
            hnyassests$9,                /* Inventory Assests Account  */~
            hnyship$12,                  /* Interim COGS Account       */~
            invnr$8,                     /* Invoice Number             */~
            mgtrpt_on$1,                 /* Is Management Reporting on?*/~
            net$(60)6, net(60,4),        /* NET Lots/ Qtys & Costs     */~
            newlot$(30)6,                /* NEW Lot Numbers            */~
            newqty(30),                  /*     Lot Quantities         */~
            newcst(30,12),               /*     Inv Costs (net/new)    */~
            oldlot$(30)6,                /* OLD Lot Numbers            */~
            oldqty(30),                  /*     Lot Quantities         */~
            oldcst(30,12),               /*     Inv Costs (netted)     */~
            origuser$3,                  /* User who created trans     */~
            p(1),                        /* Receiver for SEARCH fnctn  */~
            part$25,                     /* Part Number                */~
/*PAR000*/  subp$20,                     /* Subpart Number             */~
            epart$45,                    /* Entire Part                */~
            plowkey$100,                 /* Multi-use Plow Key         */~
            postdate$6,                  /* Post Date                  */~
            readkey$100,                 /* Misc Use Read Key          */~
            seqnr$3,                     /* Line Sequence Number       */~
            shpcosts$(30)96,             /* Work array for record I/O  */~
            so$16,                       /* Sales Order Number         */~
            soseqnr$3,                   /* Sales Order Line Number    */~
            store$3, laststore$3,        /* Store Number               */~
            suspense$9,                  /* G/L suspense account       */~
            text$50,                     /* HNYDETAL Text              */~
            tfkey$50,                    /* SHPHNYTF Primary Key       */~
            usedate$8,                   /* Usage Capture Tran Date    */~
            userid$3,                    /* User ID                    */~
            usetype$5                    /* Usage Capture Type Code    */

        dim f1%(32)                      /* = 1 if READ was successful */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            *   1 ! SHPHNYTF ! Shipping Transaction File                *~
            *   2 ! SYSFILE2 ! CMS System Information File              *~
            *   3 ! WORKFILE ! Workfile for HNYPST2 G/L Adjustments     *~
            *   4 ! INVQUAN  ! Inventory Quantities File                *~
            *   5 ! INVDETAL ! Inventory Details                        *~
            *   6 ! INVPOOL  ! Inventory LIFO/FIFO Pools                *~
            *   7 ! INVMASTR ! Inventory Master                         *~
            *   8 ! PIPMASTR ! PIP Master File                          *~
            *   9 ! SFCUM2   ! Cummulative Forecast File                *~
            *  10 ! GLMAIN   ! General Ledger Chart of Accounts         *~
            *  11 ! GLDETAIL ! General Ledger Detail                    *~
            *  20 ! SHPCOSTS ! Shipping Costs (appendix of SHPLINES)    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #20 "SHPCOSTS",                                       ~
                       varc, indexed, recsize = 1500,                    ~
                       keypos = 1, keylen = 23


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
        if beenherebefore% <> 0% then L09210

*       *** ONE TIME INITIALIZATIONS ***********************************
            beenherebefore% = 1%
            call "EXTRACT" addr("ID", userid$)
            call "OPENCHCK" (#20, 0%, 0%, 100%, " ")

            /* See if Management Reporting is on */
            call "READ100" (#2, "SWITCHS.GL", f1%(2))
            if f1%(2) = 1% then get #2 using L09140, mgtrpt_on$
L09140:         FMT POS(59), CH(1)
            call "READ100" (#2, "FISCAL DATES", f1%(2))
            if f1%(2) = 1% then get #2 using L09143, suspense$
L09143:     FMT POS(417), CH(9)

*        Now see if the Inventory Interface is on or off
            call "BCKSWTCH" ("AR ", "HNYACTVE", hnyactve$, p(1), ret%)

L09210
*       *** END ONE TIME INITIALIZATIONS *******************************

        if hnyactve$ = "N" then exit_program

            if laststore$ = " "    then L09244
            if laststore$ = store$ then L09250
*        Get Interim COGS Account- HNYSHIP$
L09244:     hnyship$ = " "
            call "ARMGLGET" (3%, " ", " ",  " ", " ", store$, " ",       ~
                             #2, #2, #7, #2, #2, hnyship$)
            if hnyship$ = " " then hnyship$ = suspense$                  ~
                              else call "GLUNFMT" (hnyship$)
            laststore$ = store$
L09250
*        Concoct Document Number Based on who's calling
            if caller$ = "P" then doc$ = so$ & "-" & bol$ else           ~
                                  doc$ = "INVOICE: " & invnr$

*        Now determine if we've been here before for this line item
*        (the Deja Vu syndrome).  This section sets us up for restarts
            restart% = 0%
            gosub get_fileid
            plowkey$ = file$ & hex(00) & str(cuscode$,,9) &              ~
                       str(doc$,,20) & str(seqnr$,,3)
            call "READ100" (#1, plowkey$, restart%)
            if restart% = 0% then L09420
                call "STRTRLSE" addr(#1)
                get #1 using L09390, datetime$       /* Restarting       */
L09390:              FMT XX(119), CH(7)
                     goto L10000

L09420:     call "GETDTTM" addr(datetime$)         /* Not Restarting   */
            write #1 using L09450, plowkey$, file$, cuscode$, doc$, seqnr$,~
                                 " ", datetime$, userid$, date, soseqnr$
L09450:         FMT CH(46), CH(35), CH(9), CH(20), CH(3), CH(6), CH(7),  ~
                    CH(3), CH(6), CH(3)
            call "STRTRLSE" addr(#1)


L10000
*       ****************************************************************
*       **       S  U  B  R  O  U  T  I  N  E    L  O  G  I  C       ***
*       ****************************************************************


*       ****************************************************************
*        Create Netted Array by Lot Number.                            *
*        ------------------------------------------------------------- *
*        NET$() = Lot Number Index to array NET(L%, X%), where X% is   *
*                 defined by the following table; also to the arrays   *
*                 OLDCST() and NEWCST().                               *
*                             +---------+--------------+               *
*                             !Quantity ! Average Cost !               *
*                         +---+---------+--------------+               *
*                         !OLD!     1   !       2      !               *
*                         !NEW!     3   !       4      !               *
*                         +---+---------+--------------+               *
*         The caller is required to pack the lot numbers to the top    *
*         of the arrays passed in.  If no lots, the first lot contains *
*         the information (i.e., for other than the first element,     *
*         lot number = blank is not permitted.)  NEW Costs are set     *
*         originally = OLD; this makes calling HNYPOST easier.  They   *
*         are later set to the weighted average cost.                  *
*       ****************************************************************

*        Initialize arrays and read in any old costs on file.
            init (hex(ff)) net$()  :  init(hex(00)) shpcosts$()
            mat net = zer : mat oldcst = zer : mat newcst = zer
            net% = 0%
            plowkey$ = str(so$,,16) &str(bol$,,3) &str(soseqnr$,,3) & "1"
            call "READ100" (#20, plowkey$, f1%(20))
            if f1%(20) = 0% then L10390
                get #20 using L10330, str(shpcosts$(),,1440)
L10330:              FMT XX(23), CH(1440)
                str(plowkey$,23,1) = "2"
                call "READ100" (#20, plowkey$, f1%(20))
                if f1%(20) = 0% then L10390
                     get #20 using L10330, str(shpcosts$(),1441,1440)

L10390
*        Put the OLD data into the netted arrays
            for i% = 1% to 30%
              if oldlot$(i%) = " " and oldqty(i%) = 0 then L10570
                get str(shpcosts$(i%)) using L10430, costs()
L10430:              FMT 12*PD(14,4)
                search str(net$()) = str(oldlot$(i%)) to p() step 6
                if p(1) = 0 then p%,net% = net% + 1% else p% = (p(1)+5)/6
                net$(p%) = oldlot$(i%)
                net(p%,2) = 0
                for c% = 1% to 12%
                     oldcst(p%,c%) = ((net(p%,1)  * oldcst(p%,c%)) +     ~
                                      (oldqty(i%) * costs(c%))   ) /     ~
                                     (net(p%,1) + oldqty(i%))
                     net(p%,2) = net(p%,2) + oldcst(p%,c%)
                next c%
                net (p%,1) = net(p%,1) + oldqty(i%)
            next i%

L10570
*        Put the NEW data into the netted arrays.
            lastnew% = 0%
            for i% = 1% to 30%
              if newlot$(i%) = " " and newqty(i%) = 0 then L10680
                lastnew% = i%
                search str(net$()) = str(newlot$(i%)) to p() step 6
                if p(1) = 0 then p%,net% = net% + 1% else p% = (p(1)+5)/6
                net$(p%)   = newlot$(i%)
                net (p%,3) = net(p%,3) + newqty(i%)
            next i%

L10680:     mat newcst = oldcst          /* Set New = Old to start     */

            if net% = 0% then L12320      /* Remove in-process status   */

                for i% = 1% to net% : net(i%,4) = net(i%,2) : next i%


*       ****************************************************************
*        Now perform the updating.                                     *
*        --------------------------------------------------------------*
*        For each lot present the following steps are executed -       *
*         (1) If we are doing the update for invoicing and a BOL is    *
*             indicated, we reverse the interim COGS posting.          *
*         (2) Net quantities are then posted to inventory and the      *
*             new costs are determined.  Appropriate G/L postings are  *
*             also determined.                                         *
*         (3) The Transaction file (SHPHNYTF) is written.  This file   *
*             is used for both reporting and updating G/L.             *
*         (4) If the caller was invoicing, Lot Tracking is updated.    *
*        After all the above is done, the new costs are written to     *
*        SHPCOSTS and the total cost returned to the caller.           *
*       ****************************************************************

* PAR000 changed to invmastr
        init(" ") str(readkey$,,45), str(epart$,,45)

        str(readkey$, 1%,25%) = part$
        str(readkey$,26%,20%) = subp$
        str(epart$,   1%,45%) = str(readkey$,1%,45%)
 
        call "READ100" (#7, readkey$, stocked%)    /* INVMASTR */

        for l% = 1% to net%
          if restart% = 0% then L11070
            tfkey$ = file$ & str(cuscode$,,9) & str(doc$,,20) &          ~
                     str(seqnr$,,3) & str(datetime$) & str(net$(l%))
            call "READ100" (#1, tfkey$, f1%(1))
            if f1%(1) = 0% then L11070
                get #1 using L11040,                                      ~
                     newcst(l%, 1), newcst(l%, 2), newcst(l%, 3),        ~
                     newcst(l%, 4), newcst(l%, 5), newcst(l%, 6),        ~
                     newcst(l%, 7), newcst(l%, 8), newcst(l%, 9),        ~
                     newcst(l%,10), newcst(l%,11), newcst(l%,12)
L11040:                   FMT XX(409), 12*PD(14,4)
                goto nextlot

L11070:   drs%, crs% = 0%
          init(" ") draccts$(), craccts$()
          mat drs = zer
          mat crs = zer

          if stocked% = 0% then L11790

*       ******* STOCKED ITEM INVENTORY UPDATING ************************
*        First see if we need to Reverse any interim GL Postings.
            cogs$ = " "
            if caller$ <> "I" or bol$ = " " or net(l%, 1) = 0 then L11240
* PAR000

                call "INVGLGET" (epart$, store$, net$(l%), cogs$, 4%,#7,#4)
                if cogs$ = " " then cogs$ = suspense$
                oldcost = round(net(l%,1) * net(l%,2), 2)
                if oldcost = 0 then L11240
                     gltype$ = "C"
                     acct$ = cogs$    : glamt =  oldcost :gosub update_gl
                     gltype$ = "B"
                     acct$ = hnyship$ : glamt = -oldcost :gosub update_gl

L11240
*        Now Update Inventory via HNYPOST and record G/L Postings
            netqty = net(l%, 1) - net(l%, 3)  /* + = RTS, - = WDL */
            if netqty = 0 then L11790     /* No Inventory Movement      */
                ext = round(price * netqty, 2)
                for c% = 1% to 12% : costs(c%) = newcst(l%,c%) : next c%
                if caller$ = "P" then text$ = "BOL: " & doc$ else        ~
                                      text$ = doc$
                text$ = text$ & "; Customer: " & cuscode$
                hnyassests$ = " "
                call "INVPST2" (part$, subp$, store$, net$(l%), netqty,  ~
                                0, 0, 0,                                 ~
                                0, costs(), net(l%,4), price, ext,       ~
                                postdate$, "RT", text$, hnyassests$,     ~
                                cogs$, 3%, 4%, "01", " ", 0%, " ",       ~
                                origuser$, #4, #5, #2, #6, #7, #8, #9,~
                                #10, #11, #3, 1%, ret%)
                for c% = 1% to 12%
                     newcst(l%,c%) = costs(c%)
                next c%
* PAR000 do not believe need this
* because all of the files in the subroutine
* are not on the system
REM             gosub usage_capture

             /* Update G/L Postings for Inventory Movement             */
                amt = round(-netqty * net(l%,4), 2)
                if amt = 0 then L11540
                     if cogs$ = " " then cogs$ = suspense$
                     if hnyassests$ = " " then hnyassests$ = suspense$
                     acct$ = hnyship$
                     gltype$ = "B"
                     if caller$ = "P" then L11510
                     acct$ = cogs$
                     gltype$ = "C"
L11510:                                    glamt =  amt : gosub update_gl
                     gltype$ = "H"
                     acct$ = hnyassests$ : glamt = -amt : gosub update_gl

L11540:         plowkey$ = hex(00)
L11550:         call "PLOWNXT1" (#3, plowkey$, 0%, f1%(3))
                if f1%(3) = 0% then L11650
                     get #3 using L11580, acct$, adj$, dramt, cramt
L11580:                   FMT XX(25),CH(9),XX(64),CH(3),XX(33),2*PD(14,4)
                     if str(adj$,3,1) <> "H" then L11586
                     gltype$ = "H" : goto L11590
L11586:              if str(adj$,1,1) = "S" then gltype$ = "S"           ~
                        else gltype$ = "A"
L11590:              if acct$ = " " then acct$ = suspense$
                     glamt = dramt : if dramt <> 0 then gosub update_dr
                     glamt = cramt : if cramt <> 0 then gosub update_cr
                     delete #3
                     goto L11550

L11650
*        Update New Costs- Weighted average of old and new costs
*         formula = ((OLDQTY*OLDCOST) + (-NETQTY*NEWCOST)) / NEWQTY
            if netqty >= 0 then L11790  /* Return to Stock -> no change */
                net(l%,4) = 0
                if net(l%,3) = 0 then L11790
                     for c% = 1% to 12%
                          newcst(l%,c%) = ((net(l%,1) * oldcst(l%,c%)) + ~
                                   (-netqty * newcst(l%,c%))) / net(l%,3)
                          newcst(l%,c%) = round(newcst(l%,c%), 4)
                          net(l%,4) = net(l%,4) + newcst(l%,c%)
                     next c%

*       ******* END STOCKED ITEM INVENTORY UPDATING ********************


L11790
*        Now Write Shipments Transaction File Record (SHPHNYTF)
            netqty = net(l%, 3) - net(l%, 1)  /* NEWQTY - OLDQTY */
            gosub get_mdmc
            write #1 using L11910,                                        ~
                     file$, cuscode$, doc$, seqnr$, datetime$, net$(l%), ~
                     file$, postdate$, store$, part$, cuscode$, doc$,    ~
                                             seqnr$, net$(l%), datetime$,~
                     origuser$, netqty,                                  ~
                     draccts$(), drs(), craccts$(), crs(),               ~
                     newcst(l%, 1), newcst(l%, 2), newcst(l%, 3),        ~
                     newcst(l%, 4), newcst(l%, 5), newcst(l%, 6),        ~
                     newcst(l%, 7), newcst(l%, 8), newcst(l%, 9),        ~
                     newcst(l%,10), newcst(l%,11), newcst(l%,12),        ~
                     drtype$(), crtype$(), mdmc, " "
L11910:         FMT CH(1), CH(9), CH(20), CH(3), CH(7), CH(6),           ~
                    CH(1), CH(6), CH(3), CH(25), CH(9), CH(20), CH(3),   ~
                    CH(6), CH(7),                                        ~
                    CH(3), PD(14,4), 8*CH(9), 8*PD(14,4), 8*CH(9),       ~
                    8*PD(14,4), 12*PD(14,4), 16*CH(1), PD(14,4), CH(43)

*        Lastly, update Lot Tracking (Stocked Item, Invoicing only)
            if caller$ = "P"  or stocked%  = 0% then L12040
            if net$(l%) = " " or net(l%,3) = 0  then L12040
                call "LOTTRACK" ("H", part$, net$(l%), store$, " ", "C", ~
                                 str(cuscode$,,9) & str(invnr$,,8),      ~
                                 " ", " ", " ", net(l%,3), #7, #2)

L12040
*        Done a lot with one lot- do a lot with another lot
        nextlot  :  next l%

*        Now put New Costs into arrays to return them to caller.
          plowkey$ = str(so$,,16) & str(bol$,,3) & str(soseqnr$,,3)
          call "DELETE" (#20, plowkey$, 22%)     /* SHPCOSTS */
          init(hex(00)) shpcosts$()
          newcost = 0
          if stocked% = 0% or net% = 0% then L12320
            for l% = 1% to lastnew%
                search str(net$()) = str(newlot$(l%),,6) to p() step 6
                p% = (p(1) + 5) / 6
                for c% = 1% to 12%
                     costs(c%) = newcst(p%,c%)
                next c%
                newcost = newcost + (newqty(l%) * net(p%,4%))
                call "PACKZERO" (costs(), shpcosts$(l%))
            next l%
            if caller$ = "I" or lastnew% = 0% then L12320
                str(plowkey$,23) = "1"
                write #20 using L12300, plowkey$,                         ~
                                              str(shpcosts$(),,1440),    ~
                                              mdmc, " "
                str(plowkey$,23) = "2"
                if lastnew% > 15% then write #20 using L12300, plowkey$,  ~
                                              str(shpcosts$(), 1441),    ~
                                              mdmc, " "
L12300:              FMT CH(23), CH(1440), PD(14,4), CH(29)

L12320
*        Flag Updating for this line as completed (Remove In-process)
            tfkey$ = file$ & hex(00) & str(cuscode$,,9) & str(doc$,,20) &~
                     str(seqnr$,,3)
            call "DELETE" (#1, tfkey$, 34%)
            goto exit_program

*       ****************************************************************

        update_gl    /* Branch to DR/CR per sign of posting amount     */
            if glamt >= 0 then update_dr
                glamt = -glamt
                goto update_cr

        update_dr
            drs%           = drs% + 1%
            draccts$(drs%) = acct$
            drs(drs%)      = glamt
            drtype$(drs%)  = gltype$
            return

        update_cr
            crs%           = crs% + 1%
            craccts$(crs%) = acct$
            crs(crs%)      = glamt
            crtype$(crs%)  = gltype$
            return


        get_fileid   /* Get which section of the work file to write to */
            readkey$ = "0"
            call "READ101" (#1, readkey$, f1%(1))
            if f1%(1) = 0% then stop "CONTROL RECORD MISSING"
            get #1 using L12630, file$
L12630:         FMT XX(126), CH(1)
            return

        get_mdmc     /* Get the Management DMC                         */
            mdmc = -1.0
            if mgtrpt_on$ <> "Y" then return
                call "MDMCCOST" (part$, " ", #2, mdmc)
                return

        usage_capture   /* Post date & quantity changes to usage files */
            usetype$ = "UNKN"
            usedate$ = postdate$
            call "DATEOK" ( usedate$, 0%, " " )
            call "DATUNFMT" (usedate$)
            useqty = -netqty               /* change sign for HNYUSESB */
            call "HNYUSESB" (so$, seqnr$, store$, part$, "A", usedate$,  ~
                            usetype$, useqty)
            return

        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUSASSO~
            *                          E X I T                          *~
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


