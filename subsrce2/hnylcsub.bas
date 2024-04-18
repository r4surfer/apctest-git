        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  H   H  N   N  Y     Y  L       CCCC   SSS   U   U  BBBB  *~
            *  H   H  NN  N   Y   Y   L      C    C S      U   U  B   B *~
            *  HHHHH  N N N    Y Y    L      C       SSS   U   U  BBBB  *~
            *  H   H  N  NN     Y     L      C    C     S  U   U  B   B *~
            *  H   H  N   N     Y     LLLLL   CCCC   SSS    UUU   BBBB  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYLCSUB - Allows transfer of inventory from one location *~
            *            to another or direct addition to, or withdrawal*~
            *            from, any location.  User can view location    *~
            *            information by either part, part/lot, lot      *~
            *            or location.                                   *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1989, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/28/82 ! ORIGINAL                                 ! WPH *~
            * 01/29/89 ! Modified to bypass Part/Lot/Store entry  ! MLJ *~
            *          !  screen when called from HNYADDNS        !     *~
            * 2/90-3/90! Modifications for CMS interface with the ! MLJ *~
            *          !  Location system.                        !     *~
            * 7/91-8/91! Added OPENCHCK for LOCATIONS & HNYLOCNS  ! MLJ *~
            *          !  Activated ACTION 2% for HNYMOVE.  Fix'd !     *~
            *          !  misc QC rejections.                     !     *~
            * 08/06/91 ! OK.  I'll give it a stab!  HNYMOVE stuff.! JDH *~
            * 02/26/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 05/27/92 ! Added 'Remarks' (text) functionality     ! MLJ *~
            *          !  previously available only thru HNYLOCIN.!     *~
            * 10/12/92 ! Write to Location Audit File as needed   ! RJH *~
            * 11/17/92 ! PRR 12357 - Disable withdrawl field when ! RJH *~
            *          !  quan. goes to zero & record is deleted. !     *~
            *          !  Destinaltion location test for existing !     *~
            *          !  quantities now done on HNYLOCNS file.   !     *~
            *          !  Minor PF Key display screen changes.    !     *~
            * 09/30/93 ! Major logic re-vamp.  Hopefully much     ! MLJ *~
            *          !  cleaner & more eazsily maintained.      !     *~
            *          ! PRR 12781 - PF(25) now works properly.   !     *~
            *          ! PRR 12827 - Cleaned up numerous issues.  !     *~
            *          ! PRR 13026 - Now honors Lot Tracking flag.!      ~
            *          ! MISC - renumbered and modkilled due to   !     *~
            *          !  extent of changes.                      !     *~
            * 01/09/95 ! Corrected bugged that was rendering sub  ! HES *~
            *          !  completely inoperable.                  !     *~
            * 01/13/95 ! Misc clean-up. Contents of.              ! JDH *~
            * 05/22/95 ! PRR 13393 -Misc improvements in Action 7%! RJH *~
            *          !  Default store,PF1 frm Askuser, updates. !     *~
            * 12/12/95 ! Chgd ASKUSER msg when not in HNYLOCNS    ! JDH *~
            *          ! and, if so, allow to go to INPUTMODE.    !     *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC


        sub "HNYLCSUB" (partin$,   /* Part Number                      */~
                        storein$,  /* Store ID                         */~
                        lotin$,    /* Lot Number                       */~
                        qtyin,     /* quan to transfer,add,or withdraw */~
                        actionin%, /* See List of values below         */~
                        #1,        /* SYSFILE2   Misc. Info            */~
                        #2,        /* STORNAME   Warehouse information */~
                        #3,        /* USERINFO   User Information      */~
                        #4,        /* HNYMASTR   Part Master File      */~
                        #6,        /* HNYLOCNS   Location quantity file*/~
                        #7,        /* HNYQUAN    Part/Store/Lot Qty    */~
                        #8,        /* LOCATION   Location master file  */~
                        storemvto$,/* 'TO' Store for action 2% ONLY    */~
                        lotmvto$)  /* 'TO' Lot for action 2% ONLY      */

        REM ACTIONIN% (aka ACTION%) controls what the user does          ~
            1 = Prompt for part or part/lot or lot, action & qty         ~
            2 = Transfer quantity of a part between 2 locations          ~
            3 = Add quantity of part to one or more locations            ~
            4 = Withdraw quantity from one or more locations             ~
            5 = Allow simultaneous add and withdrawal                    ~
            6 = Prompt for a location and display its' contents          ~
            7 = Add quantity of part to a single location, create record ~
                                                                         ~
            NOTE- The caller can pass ACTIONIN% = 1, 2, 3, or 4.         ~
                  All other actions are activated internally only.       ~
                  If the user wants to perform other actions they        ~
                  can PF 1 to cancel the passed in action and proceed    ~
                  to do whatever they want.                              ~
                                                                         ~
            If you pass in a store ID, it is used.  If not, the          ~
            users default store is used.


        dim                                                              ~
            action$1,                   /* Action Variable             */~
            adate$6,                    /* Unfmtd Date For Audit       */~
            afac$(3)1,                  /* FAC for Add (24)            */~
            bottom$3,                   /* Last Line Displayed         */~
            call_pgm$8,                 /* Calling Program             */~
            cfac$1,                     /* Quantity Mod Col Head Fac   */~
            chgqty$(300)10,             /* Quant to subtract from loc  */~
            cursor%(2),                 /* Cursor Position             */~
            date$8,                     /* System Date for today       */~
            datetime$7,                 /* System Date/Time Stamp      */~
            defaultstore$3,             /* Users store in USERINFO     */~
            descr$79,                   /* Descrip for GETCODE/PLOWCODE*/~
            errormsg$79,                /* Error message at scrn top   */~
            field2$40,                  /* Store & Descr for Header 2  */~
            field3$5,                   /* Part Prompt (11 & 24)       */~
            field4$4,                   /* Lot Prompt (11 & 24)        */~
            field5$8,                   /* Add Qty Prompt (24)         */~
            field6$12,                  /* To Location Prompt (24)     */~
            field7$13,                  /* Transfer Qty Prompt (11)    */~
            field8$14,                  /* From Location Prompt (11)   */~
            field9$12,                  /* To Location Prompt (11)     */~
            field10$8,                  /* Location Column Heading     */~
            field11$6,                  /* Lot Column Heading          */~
            field12$10,                 /* Quantity Column Heading     */~
            field13$10,                 /* Quantity Mod Column Heading */~
            field14$25,                 /* Part Number Column Heading  */~
            fil1$1,                     /* Filler = HEX(00)            */~
            flocation$8,                /* From Location               */~
            fstore$3,                   /* From Store                  */~
            i$(24)80,                   /* Screen Image                */~
            inpmessage$79,              /* Input message - scrn bottom */~
            lcfac$1,                    /* Contents Loc/Store FAC      */~
            lfac$(300)1,                /* FAC for list                */~
            line$(300)52,               /* data displayed on screen    */~
            line1$65,                   /* First Screen Line           */~
            line2$79,                   /* Second Screen line          */~
            lclocation$8,               /* Location contents location  */~
            lcstore$3,                  /* Location contents store     */~
            location$(300)8,            /* Location Array              */~
            locn_audit$1,               /* Location changes Audited    */~
            lot$(300)6,                 /* Lot Number Array            */~
            lotin$6,                    /* lot  Number passed in       */~
            lotmvto$6,                  /* 'TO' Lot - action 2% ONLY   */~
            lot_unique$1,               /* Lot Number Unique           */~
            maxlines$3,                 /* Maximum Display Lines       */~
            mqfac$(300)1,               /* FAC for Modifiable qty      */~
            multiflag$1,                /* Multi Transaction Flag      */~
            p%(1),                      /* SEARCH Result               */~
            pcfac$(300)1,               /* Part FAC for Contents Of    */~
            pchfac$1,                   /* Part Column Heading FAC     */~
            part$(300)25,               /* Part Numbers for toggle     */~
            partin$25,                  /* Part Number passed in       */~
            pdescr$34,                  /* Part Description            */~
            pdescr$(300)32,             /* Part Descriptions array     */~
            pf$(3)79,                   /* PF Screen Literals          */~
            pfac$(2)1,                  /* FAC for Part/Lot (11 & 24)  */~
            pfkeys$32,                  /* PF Key Hex Values           */~
            plowkey$99,                 /* Misc Read/Plow Key          */~
            qty$10,                     /* Quantity                    */~
            qty$(300)10,                /* Quantity Array              */~
            qtyin$10,                   /* quantity passed in          */~
            qtylocated$10,              /* resolved portion of qtyin   */~
            qtyremaining$10,            /* unresolved portion of qtyin */~
            qfac$1,                     /* FAC for Quantity Tracking   */~
            qtymessage$(3)25,           /* Quantity Tracking Message   */~
            quant$(300)10,              /* Modifiable Qty Array        */~
            sdescr$32,                  /* Store Description           */~
            searchloc$8,                /* Search Location  PF(26)     */~
            searchstore$3,              /* Search Store     PF(26)     */~
            selectpart$25,              /* Part number selected        */~
            selectlot$6,                /* Lot number selected         */~
            selectstore$3,              /* Store number selected       */~
            store$(300)3,               /* Store selected              */~
            storein$3,                  /* Store passed in             */~
            storemvto$3,                /* 'TO' Store - action 2% ONLY */~
            sys_lot$1,                  /* System wide lot tracking    */~
            templot$6,                  /* Temp Lot Variable           */~
            text$(8)50,                 /* Stock location remarks      */~
            tlocation$8,                /* Destination Location        */~
            top$3,                      /* First Line of Display       */~
            tstore$3,                   /* Destination Store           */~
            tfac$(3)1,                  /* FAC for Transfer (11)       */~
            tquantity$10,               /* Transfer Quantity           */~
            txprt$25,                   /* Remarks Part #              */~
            txstr$3,                    /* Remarks Store #             */~
            txloc$8,                    /* Remarks Location #          */~
            txlot$6,                    /* Remarks Lot #               */~
            userid$3,                   /* User                        */~
            vlr$1                       /* Valid Locations Required?   */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            fs%(64),                                                     ~
            rslt$(64)20

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.03 08/12/96 Last Wang Release               "


            mat f2% = con

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! SYSFILE2 ! SYSTEM INFORMATION FILE (INVENTORY DATE) *~
            * # 2 ! STORNAME ! STORE MASTER FILE                        *~
            * # 3 ! USERINFO ! USER INFORMATION FILE                    *~
            * # 4 ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * # 6 ! HNYLOCNS ! Inventory quantity by location file      *~
            * # 7 ! HNYQUAN  ! INVENTORY QUANTITY INFORMATION FILE      *~
            * # 8 ! LOCATION ! Location master File                     *~
            * # 9 ! LOCAUDIT ! Location Change Audit File               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #09, "LOCAUDIT",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos =    1, keylen =  50,                     ~
                        alt key  1, keypos =   95, keylen =  42, dup     ~

            if been_here_before% = 1% then L09305
            been_here_before% = 1%

            call "OPENCHCK" (#6, fs%(6), f2%(6), 100%, rslt$(6))
            call "OPENCHCK" (#8, fs%(8), f2%(8), 100%, rslt$(8))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *************************************************************

            adate$, date$ = date
            call "DATEFMT" (date$)
            str(line2$,62%) = "HNYLCSUB: " &  cms2v$
            call "EXTRACT" addr ("ID", userid$)
            call "EXTRACT" addr ("CF", call_pgm$)

            field3$  =  "Part "
            field4$  =  "Lot "
            field5$  =  "Add Qty "
            field6$  =  "To Location "
            field7$  =  "Transfer Qty "
            field8$  =  "From Location "
            field9$  =  "To Location "
            field10$ =  "Location"
            field11$ =  "Lot   "
            field12$ =  "  Quantity"
            field14$ =  "Part Number             "

            fil1$    =  hex(00)

        REM  Load location switches...
            sys_lot$, vlr$, locn_audit$ = "N"
            call "READ100" (#1, "SWITCHS.HNY", f1%(1%))
                if f1%(1%) = 0% then L09185
            get #1 using L09155, sys_lot$, lot_unique$, vlr$, locn_audit$
L09155:         FMT POS(92), 2*CH(1), POS(107), CH(1), POS(120), CH(1)
            if vlr$ <> " " then L09170
                vlr$, locn_audit$ = "N"
L09170:     if locn_audit$ = "Y" then                                    ~
                 call "OPENCHCK" (#09, fs%(9%), f2%(9%), 100%, rslt$(9%))

L09185:     if storein$ = " " then L09200
                defaultstore$ = storein$
                goto L09305
L09200:     call "READ100" (#3, userid$, f1%(3))
                if f1%(3%) = 1% then L09245
L09210:             k% = 0%
                    call "ASKUSER" (k%, "*** ERROR ***",                 ~
                         "Unable to locate your user record.",           ~
                         "Contact your System Administrator.",           ~
                         "Press RETURN to acknowledge and Exit.")
                    if k% = 0% then exit_program
                        goto L09210
L09245:     get #3, using L09250, defaultstore$
L09250:         FMT POS(64), CH(3)

            if defaultstore$ <> " " then L09305
                k% =0%
                call "ASKUSER" (k%, "*** ERROR ***",                     ~
                     "Your Default Store/Warehouse has not been set.",   ~
                     "Contact your System Administrator.",               ~
                          "Press RETURN to acknowledge and Exit.")
                if k% = 0% then exit_program

L09305: REM *************************************************************~
            *        C A L L E R  C O N T R O L  C H E C K I N G        *~
            *-----------------------------------------------------------*~
            *  Determine if the caller is specifying a particular       *~
            *  part/store/lot and/or action to be performed.            *~
            *     Either -  a) proceed to prompt user for input         *~
            *               b) jump right into validation of passed data*~
            *                    if valid, show the list.  if not then  *~
            *                    show error message and prompt for data.*~
            *************************************************************

            gosub initialize_variables
            gtadd, gtsub, totaladd, totalsub, qtylocated, qtyremaining = 0
            action%, holdaction% = actionin%
            init(" ") qtymessage$()

            if action% = 1% then inputmode
                if action% <  2% or action%  > 4% then  exit_program

            sub% = 1%

*        Preliminary Store/Part/Lot verification...
            plowkey$ = all(hex(00))
            str(plowkey$,1%,3%) = str(storein$)
            str(plowkey$,4%,25%) = str(partin$)
            str(plowkey$,29%,6%) = str(lotin$)
            call "PLOWALTS" (#6, plowkey$, 1%, 34%, f1%(6%))
                if f1%(6%) <> 0% then L09485
L09445:     k% = 2%
            call "ASKUSER" (k%, "*** NO LOCATION RECORD ***",            ~
                 "Part: " & partin$ & "  Store: " & storein$ & "  Lot: "&~
                 lotin$, "does not exist in the Location Quantity File "&~
                 "(HNYLOCNS).", "Press ENTER to acknowledge and return "&~
                 "-OR- Press PF1 to Continue.")
            if k% = 0% then exit_program
            if k% = 1% then inputmode
                goto L09445

L09485:     if action% <> 2% then L09540
                if lotin$ = lotmvto$ then L09595
L09495:     k% = 2%
            call "ASKUSER" (k%, "*** LOCATION MGT PROCESSING NOTE ***",  ~
                 "Normal Location Transfers are for the same Part/Lot.", ~
                 "A Withdrawal and an Addition must be done to complete"&~
                 " this transaction.", "Press RETURN to continue.")
            if k% <> 0% then L09495
                action% = 1%
                goto inputmode

L09540:     if action% = 3% and qtyin = 0 then L09595
                if action% = 4% and qtyin = 0 then L09595
                    qtytracking% = 1%
                    qtyremaining = qtyin
                    call "CONVERT" (qtyin, 2.2, qtyin$)
                    call "CONVERT" (qtylocated, 2.2, qtylocated$)
                    call "CONVERT" (qtyremaining, 2.2, qtyremaining$)
                    qtymessage$(1%) = "Qty to Locate: " & qtyin$
                    qtymessage$(2%) = "Qty Located:   " & qtylocated$
                    qtymessage$(3)  = "Qty Remaining: " & qtyremaining$

L09595:     selectpart$  = partin$
            selectstore$ = storein$
            selectlot$   = lotin$
            gosub'051                                     /*      Parms */
            gosub'151                                     /* Edit Parms */
            if action% = 2% then keyhit% = 11%
            if action% = 3% then keyhit% =  8%
            if action% = 4% then keyhit% =  9%
            goto L11000

        REM *************************************************************~
            *        M A I N  C O N T R O L  S E C T I O N              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            selectlot$ = "ALL"
            holdaction% = action%
            action% = 1%

            gosub'051
L10100:     gosub'101(1%)
                  if keyhit%  = 1%  then inputmode
                  if keyhit% <> 26% then L10140
                      gosub empty_locations
                      goto L10100
L10140:           if keyhit%  = 16% then gosub exit_checking
                  if keyhit% <>  0% then L10100
            gosub'151
                  if errormsg$ <> " " then L10100

L11000: REM *************************************************************~
            *               G E N E R A T E   L I S T                   *~
            *************************************************************

            plowkey$ = all(hex(00))
            maxlines% = 0%

            if uselc% <> 1% then L11140
                str(plowkey$,1%,3%) = lcstore$
                str(plowkey$,4%,8%) = lclocation$
                break% = 11%
                akey%, keywas% =0%
                goto L11340

L11140:     if selectpart$ <> " " then L11210
                str(plowkey$,1%,3%) = selectstore$
                str(plowkey$,4%,6%) = selectlot$
                break% = 9%
                akey%  = 4%
                goto L11340

L11210:     if selectlot$ <> "ALL" then L11280
                str(plowkey$,1%,3%)  = selectstore$
                str(plowkey$,4%,25%) = selectpart$
                break% = 28%
                akey%  =  1%
                goto L11340

L11280:     str(plowkey$,1%,3%)  = selectstore$
            str(plowkey$,4%,25%) = selectpart$
            str(plowkey$,29%,6%) = selectlot$
            break% = 34%
            akey%  =  1%

L11340:     gosub data_load
            if sub% <> 1% then L12000
                quant$(1%) = qtyin$
                if keyhit% =  8% then add_quantity
                if keyhit% =  9% then withdraw_quantity
                if keyhit% = 11% then transfer_quantity

L12000: REM *************************************************************~
            *               D I S P L A Y   D A T A                     *~
            *************************************************************

            edit% = 0%
L12060:     gosub'102(edit%)
                if keyhit% =  1% then inputmode
                if keyhit% =  8% then add_quantity
                if keyhit% =  9% then withdraw_quantity
                if keywas% = 10% then L11000
                if keyhit% = 11% then transfer_quantity
                if keyhit% = 12% then change_quantity
                if keyhit% = 16% then gosub exit_checking
                if keyhit% = 24% then add_new_location
            goto L12060

        REM *************************************************************~
            *           A C T I O N   S E C T I O N                     *~
            *-----------------------------------------------------------*~
            *  All Quantity Manipulation Is Controlled Here.            *~
            *************************************************************

        add_quantity                               /* PF(8) or Action 3 */
            edit% = 1%
            action% = 3%
            field13$ = "       Add"
            inpmessage$ = "Enter Quantities To Add And Press RETURN."
L13110:     gosub'102(edit%)
                if keyhit% <> 1% then L13150
                    init(" ") quant$()
                    selectlot$ = templot$
                    goto L12000
L13150:     gosub'152
                if errormsg$ <> " " then L13110
            gosub save_changes
            if sub% = 1% then L13110 else L12000

        withdraw_quantity                          /* PF(9) or Action 4 */
            edit% = 1%
            action% = 4%
            field13$ = "  Withdraw"
            inpmessage$ ="Enter Quantities To Withdraw And Press RETURN."
L13250:     gosub'102(edit%)
                if keyhit% <> 1% then L13290
                    init(" ") quant$()
                    selectlot$ = templot$
                    goto L12000
L13290:     gosub'152
                if errormsg$ <> " " then L13250
            gosub save_changes
            if sub% = 1% then L13250 else L11000

        transfer_quantity                         /* PF(11) or Action 2 */
            edit% = 1%
            action% = 2%
            selectlot$ = " "
            inpmessage$ = "Enter Part, Lot, Qty To Transfer And The Sou"&~
                          "rce & Destination Locations."
L13400:     gosub'102(edit%)
                if keyhit% <> 26% then L13440
                    gosub empty_locations
                    goto L13400
L13440:         if keyhit% <> 1% then L13470
                    tquantity$, flocation$, tlocation$ = " "
                    selectlot$ = templot$
                    goto L12000
L13470:     gosub'152
                if errormsg$ <> " " then L13400
            gosub save_transfer_add
            if sub% = 1% then L13400 else L11000

        change_quantity                           /* PF(12) or Action 5 */
            edit% = 1%
            action% = 5%
            field13$ = "Adjustment"
            inpmessage$ ="Enter Adjustment Quantities (+/-) And Press R"&~
                         "ETURN."
L13580:     gosub'102(edit%)
                if keyhit% <> 1% then L13620
                    init(" ") quant$()
                    selectlot$ = templot$
                    goto L12000
L13620:     gosub'152
                if errormsg$ <> " " then L13580
            gosub save_changes
            goto L11000

        add_new_location
            edit% = 1%
            action% = 7%
            selectlot$ = " "
            inpmessage$ ="Enter The Part, Lot, Quantity And Location."
            cancel_action% = 0%
            if qtymessage$(1%) <> " " then tquantity$ = qtyremaining$
L13720:     gosub'102(edit%)
                if keyhit% <> 26% then L13760
                    gosub empty_locations
                    goto L13720
L13760:         if keyhit% <> 1% then L13790
                    tquantity$, tlocation$ = " "
                    selectlot$ = templot$
                    goto L12000
L13790:     gosub'152
                if errormsg$ <> " " then L13720
                if cancel_action% <> 0% then add_new_location
            gosub save_transfer_add
            goto L11000

        maintain_remarks
            init(" ") text$(), txprt$, txstr$, txloc$, txlot$
            if top% = 1% then l% = cursor%(1%) - 9%
                if top% = 11% then l% = cursor%(1%) + 1%
                    if top% = 21% then l% = cursor%(1%) + 11%
                        if top% = 31% then l% = cursor%(1%) + 21%
            plowkey$ = str(store$(l%)) & str(location$(l%)) &            ~
                                            str(part$(l%)) & str(lot$(l%))
            call "READ101" (#6, plowkey$, f1%(6%))
                if f1%(6%) <> 1% then L14020
            get #6 using L13950, txstr$, txloc$, txprt$, txlot$, text$()
L13950:         FMT CH(3), CH(8), CH(25), CH(6), 8*CH(50)
            gosub'104
                if keyhit% <> 16% then L14020
                    put #6 using L13950, txstr$, txloc$, txprt$, txlot$,  ~
                                        text$()
                    call "SHOSTAT" ("Updating Location Remarks")
                    rewrite #6
L14020:     l% = top%
            return

        empty_locations
            ret% = 0%
            searchstore$ = selectstore$
            searchloc$ = " "

            if action% <> 2% and action% <> 7% then L14140
                searchstore$ = tstore$
                searchloc$   = tlocation$

L14140:     call "HNYELCSB" (searchstore$, searchloc$, action%, #8, #6,  ~
                             #2, ret%)
            if ret% = 0% then return
            if action% <> 2% and action% <> 7% then return
                tlocation$ = searchloc$
            return

        contents_of
            errormsg$ = " "
            if lclocation$ <> " " then L14260
                errormsg$ = "Contents Location CANNOT Be Blank"
                goto L14330
L14260:     plowkey$ = all(hex(00))
                str(plowkey$,1%,3%) = str(lcstore$)
                str(plowkey$,4%,8%) = str(lclocation$)
            call "PLOWALTS" (#6, plowkey$, 0%, 11%, f1%(6%))
                if f1%(6%) <> 0% then return
            errormsg$ = "No Record Of Location " & lclocation$ & " In S"&~
                        "tore " & lcstore$
L14330:     init(hex(8c)) lfac$()
            lcfac$ = hex(81)
            return

        REM *************************************************************~
            *            D E F A U L T / E N A B L E                    *~
            *************************************************************

        deffn'051
             if selectpart$ = " " then selectpart$ = partin$

             if selectlot$  = " " then selectlot$ = lotin$
                 if selectlot$ = " " and action% <> 2%                   ~
                     then selectlot$ = "ALL"

             if selectstore$ = " " then selectstore$ = storein$
                 if selectstore$ = " " then selectstore$ = defaultstore$
             fstore$, tstore$ = selectstore$
             if lcstore$ = " " then lcstore$ = selectstore$

             tquantity$, flocation$, tlocation$ = " "
             if qtyin > 0 then tquantity$ = qtyin$

             return

        REM *************************************************************~
            *               I N I T I A L I Z A T I O N                 *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$, field2$,                   ~
                fstore$, tstore$, flocation$, tlocation$,                ~
                tquantity$, plowkey$, chgqty$(), store$(), text$(),      ~
                line$(), lclocation$, lcstore$, sdescr$, quant$(),       ~
                part$(), pdescr$(), lot$(), location$(), qty$(),         ~
                qtylocated$, qtyremaining$, selectpart$

            gtadd, gtsub, totaladd, totalsub, qtylocated, qtyremaining = 0
            qtytracking%, maxlines%, actionwas%, keywas% = 0%
        return

        REM *************************************************************~
            *                  D A T A   L O A D                        *~
            * --------------------------------------------------------- *~
            * Load arrays based on selection specified.                 *~
            *************************************************************

        data_load
            maxlines% = 0%
            for i% = 1% to 300%
                call "PLOWALTS" (#6, plowkey$, akey%, break%, f1%(6%))
                    if f1%(6%) = 0% then L30200
                get #6 using L30130, store$(i%), location$(i%), part$(i%),~
                                    lot$(i%), qty
L30130:             FMT CH(3), CH(8), CH(25), CH(6), POS(573), PD(14,4)
*              IF QTY > 0 THEN 30150 ELSE 30180
                call "CONVERT" (qty, 2.2, qty$(i%))
                call "DESCRIBE" (#4, part$(i%), pdescr$(i%), 0%, f1%(4%))
                maxlines% = maxlines% + 1%
            next i%

L30200:     if maxlines% = 0% then l% = 0% else l% = 1%
            convert maxlines% to maxlines$, pic(###)
            convert l% to top$, pic(###)
            bottom% = min(l% + 9%, maxlines%)
            convert bottom% to bottom$, pic(###)
            call "STRING" addr("LJ", top$, 3%, top$)
            call "STRING" addr("LJ", bottom$, 3%, bottom$)
            call "STRING" addr("LJ", maxlines$, 3%, maxlines$)

            return

        REM *************************************************************~
            *                  D A T A   S A V E                        *~
            *-----------------------------------------------------------*~
            * Update quantities, write to audit file.                   *~
            *************************************************************

        save_changes
            for i% = 1% to maxlines%
                if quant$(i%) = " " then L31430
                convert quant$(i%) to chgqty, data goto L31100
L31100
*              IF ACTION% = 3%  THEN CHGQTY = ABS(CHGQTY)
                if action% = 4%  then chgqty =  chgqty  * (-1)

                plowkey$ = str(store$(i%)) & str(location$(i%)) &        ~
                           str(part$(i%))  & str(lot$(i%))
                call "READ101" (#6, plowkey$, f1%(6%))
                     if f1%(6%) = 0% then L31430         /* JUST IN CASE */
                get #6 using L31180, currqty
L31180:              FMT POS(573), PD(14,4)
                origqty = currqty
                currqty = currqty + chgqty
                if currqty > 0 then L31240
                     delete #6
                     goto L31280
L31240:         call "CONVERT" (currqty, 2.2, qty$(i%))
                put #6 using L31180, currqty
                rewrite #6

L31280:         if locn_audit$ <> "Y" then L31430
                    convert action% to action$, pic(#)
                    multiflag$ = " "
                    datetime$ = " " : call "GETDTTM" addr(datetime$)

                write #9 using L31390, store$(i%), location$(i%),         ~
                     part$(i%), lot$(i%), fil1$, datetime$, " ", action$,~
                     multiflag$, currqty, origqty, location$(i%),        ~
                     store$(i%), lot$(i%), userid$, part$(i%),           ~
                     store$(i%), lot$(i%), location$(i%), " "

L31390:         FMT CH(3), CH(8), CH(25), CH(6), CH(1), CH(7), CH(6),    ~
                    2*CH(1), 2*PD(14,4), CH(8), CH(3), CH(6), CH(3),     ~
                    CH(25), CH(3), CH(6), CH(8), CH(34)

L31430:     next i%
            init(" ") quant$()
            return

        save_transfer_add
             if action% <> 2% then L31670
             convert tquantity$ to chgqty, data goto L31500
L31500:      chgqty =  chgqty  * (-1)            /* Decrease Source Qty */
             plowkey$ = str(fstore$) & str(flocation$) & str(selectpart$)~
                                                       & str(selectlot$)
             call "READ101" (#6, plowkey$, f1%(6%))
                 if f1%(6%) = 0% then L32110             /* JUST IN CASE */
             get #6 using L31180, currqty
             origqty = currqty
             currqty = currqty + chgqty
             if currqty > 0 then L31610
                 delete #6
                 goto L31670
L31610:      search location$() = str(flocation$) to p%() step 8%
             i% = p%(1%) / 8% + 1%
             call "CONVERT" (currqty, 2.2, qty$(i%))
             put #6 using L31180, currqty
             rewrite #6

L31670:      convert tquantity$ to chgqty, data goto L31680
L31680:      chgqty =  abs(chgqty)               /* Increase Destin Qty */
             plowkey$ = str(tstore$) & str(tlocation$) & str(selectpart$)~
                                                       & str(selectlot$)
             call "READ101" (#6, plowkey$, f1%(6%))
                 if f1%(6%) = 1% then L31760
                     origqty, currqty = 0
                     currqty = currqty + chgqty
                     goto L31850
L31760:      get #6 using L31180, currqty
             origqty = currqty
             currqty = currqty + chgqty
             search location$() = str(tlocation$) to p%() step 8%
             i% = p%(1%) / 8% + 1%
             call "CONVERT" (currqty, 2.2, qty$(i%))
             put #6 using L31180, currqty
             rewrite #6 :  goto L31990

L31850:      put #6 using L31920, tstore$, tlocation$, selectpart$,       ~
                     selectlot$, " ", " ", tstore$, selectpart$,         ~
                     selectlot$, tlocation$, tstore$, selectpart$,       ~
                     tlocation$, selectlot$, selectpart$, tstore$,       ~
                     tlocation$, selectlot$, " ", currqty, userid$,      ~
                     adate$, tstore$, selectlot$, selectpart$,           ~
                     tlocation$, " "
L31920:      FMT CH(3), CH(8), CH(25), CH(6), CH(200), CH(200), CH(3),   ~
                 CH(25),CH(6), CH(8), CH(3), CH(25), CH(8), CH(6),CH(25),~
                 CH(3), CH(8), CH(6), CH(4), PD(14,4), CH(3), CH(6),     ~
                 CH(3), CH(6), CH(25), CH(8), CH(69)

             write #6

L31990:      if locn_audit$ <> "Y" then L32110
                 convert action% to action$, pic(#)
                 multiflag$ = " "
                 datetime$ = " " : call "GETDTTM" addr(datetime$)

              write #9 using L31390, tstore$, tlocation$, selectpart$,    ~
                       selectlot$, fil1$, datetime$, " ", action$,       ~
                       multiflag$, currqty, origqty, flocation$, fstore$,~
                       selectlot$, userid$, selectpart$, tstore$,        ~
                       selectlot$, tlocation$, " "


L32110:       selectlot$ = "ALL"
              tquantity$, flocation$, tlocation$ = " "
              return

        REM *************************************************************~
            *          S E L E C T I O N   S C R E E N  1               *~
            *-----------------------------------------------------------*~
            *  Specifiy Part, Store, Lot for selection.                 *~
            *************************************************************

        deffn'101(edit%)
            keywas%, actionwas%, uselc% = 0%
            gosub set_pf1
            init(hex(81)) lfac$()
            lcfac$ = hex(9c)

L40120: accept                                                           ~
               at (01,02), "Warehouse Location Management",              ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Part Number",                                ~
               at (06,20), fac(lfac$(1%)), selectpart$          , ch(25),~
                                                                         ~
               at (07,02), "Lot Number",                                 ~
               at (07,20), fac(lfac$(1%)), selectlot$           , ch(06),~
                                                                         ~
               at (08,02), "Warehouse Number",                           ~
               at (08,20), fac(lfac$(1%)), selectstore$         , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
               at (24,50), fac(lcfac$),    lclocation$          , ch(08),~
               at (24,60), fac(lcfac$),    lcstore$             , ch(03),~
                                                                         ~
               keys(pfkeys$),  key(keyhit%)

               if keyhit% <> 10% then L40430
                   keywas% = 10% : lclocation$ = " "
                   init(hex(8c)) lfac$() :  lcfac$ = hex(81)
                   str(pf$(3%),64%,16%)= " " : str(pfkeys$,6%,1%) =hex(ff)
                   str(pf$(1%),18%,14%)= " " : str(pfkeys$,7%,1%) =hex(ff)
                   goto L40120

L40430:        if keywas% <> 10% then L40500
                   if keyhit% = 1% then return
                       gosub contents_of
                           if errormsg$ <> " " then L40120
                       action%, actionwas% = 6%
                       lcfac$ = hex(9c)
                       uselc% = 1%
                       return

L40500:        if keyhit% <> 13% then L40540
                   call "MANUAL" ("HNYLCSUB")
                   goto L40120

L40540:        if keyhit% <> 15% then L40580
                   call "PRNTSCRN"
                   goto L40120

L40580:        close ws
               call "SCREEN" addr("C", u3%, "I", i$(), cursor%())
               u3% = u3%
               return

        set_pf1
            pf$(1%)= "(1)Re-Select     (26)Empty Locs         " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                               (10)Conte" &        ~
                     "nts Of:                (16)Exit Program"
           inpmessage$ = "Enter Part And Lot, Or Just Lot And Press RET"&~
                         "URN."
            pfkeys$ = hex(00010a0d0f101a)
            return

        REM *************************************************************~
            *             D E T A I L   S C R E E N  2                  *~
            *-----------------------------------------------------------*~
            * Primary quantity display and manipulation screen.         *~
            *************************************************************

        deffn'102(edit%)
L42070:     if sub% <> 1% then L42130
                if qtytracking% <> 1% then L42130
                    if selectpart$ <> partin$ then L42110
                        if qtylocated <= qtyin then L42130
L42110:                     qtytracking% = 0%
                            init(" ") qtymessage$()
L42130:     gosub set_pf2
            line1$ = " "
L42150:     if action% = 6% or actionwas% = 6% then L42160 else L42190
L42160:         str(line1$,1%,22%) = "Contents of Location: "
                str(line1$,23%,8%) = lclocation$
                goto L42260
L42190:     if selectpart$ = " " then L42230
                str(line1$,1%,22%) = "Locations for Part:   "
                str(line1$,23%,43%) = selectpart$ & " " &  pdescr$
                goto L42260
L42230:     str(line1$,1%,21%) = "Locations for Lot:   "
            str(line1$,22%,6%) = selectlot$

L42260:     str(field2$) = selectstore$ & " " & sdescr$
            str(line2$,1%,59%) = "Warehouse (Store): " & field2$

            init(hex(9c)) lfac$(), pcfac$(), pchfac$
            if l% <> 0% then L42300
                l% = 1%  :  goto L42350

L42300:         for i% = 1% to maxlines%
                    lfac$(i%) = hex(8c)
                    if action% = 6% or actionwas% = 6% or                ~
                             selectpart$ = " " then pcfac$(i%) = hex(8c)
                next i%
L42350:     if action% = 6% or actionwas% = 6% or                        ~
                             selectpart$ = " " then pchfac$ = hex(ac)
            init(hex(9c)) afac$(), pfac$(), mqfac$(), tfac$(), cfac$,    ~
                          lcfac$
            if keywas% = 10% then lcfac$ = hex(81)
            if qtytracking% = 1% then qfac$ = hex(8c)                    ~
                                 else qfac$ = hex(9c)
            if edit% = 0% then L42570
                if action% <> 2% then L42470
                    pfac$(1%), tfac$(1%) = hex(8c)
                    pfac$(2%), tfac$(2%) = hex(81)
                    tfac$(3%) = hex(82) :  goto L42570
L42470:         if action% < 3% or action% > 5% then L42530
                    cfac$ = hex(a4)
                    for i% = 1% to maxlines%
                        mqfac$(i%) = hex(82)
                    next i%
                    goto L42570
L42530:         pfac$(1%), afac$(1%) = hex(8c)
                pfac$(2%), afac$(3%) = hex(81)
                afac$(2%) = hex(82)

L42570: accept                                                           ~
             at (01,02), fac(hex(8c)),      line1$              , ch(64),~
             at (01,66), "Today:",                                       ~
             at (01,73), fac(hex(8c)),      date$               , ch(08),~
             at (02,02), fac(hex(ac)),      line2$              , ch(79),~
             at (03,02), fac(hex(94)),      errormsg$           , ch(79),~
                                                                         ~
             at (04,55), fac(qfac$),        qtymessage$(1%)     , ch(25),~
             at (05,55), fac(qfac$),        qtymessage$(2%)     , ch(25),~
             at (06,55), fac(qfac$),        qtymessage$(3%)     , ch(25),~
                                                                         ~
             at (04,02), fac(pfac$(1%)),    field3$             , ch(05),~
             at (04,07), fac(pfac$(2%)),    selectpart$         , ch(25),~
             at (05,02), fac(pfac$(1%)),    field4$             , ch(04),~
             at (05,07), fac(pfac$(2%)),    selectlot$          , ch(06),~
                                                                         ~
             at (06,02), fac(afac$(1%)),    field5$             , ch(08),~
             at (06,16), fac(afac$(2%)),    tquantity$          , ch(10),~
             at (06,27), fac(afac$(1%)),    field6$             , ch(12),~
             at (06,42), fac(afac$(3%)),    tlocation$          , ch(08),~
             at (06,51), fac(afac$(3%)),    tstore$             , ch(03),~
                                                                         ~
             at (07,02), fac(tfac$(1%)),    field7$             , ch(13),~
             at (07,16), fac(tfac$(3%)),    tquantity$          , ch(10),~
             at (07,27), fac(tfac$(1%)),    field8$             , ch(14),~
             at (07,42), fac(tfac$(2%)),    flocation$          , ch(08),~
             at (07,51), fac(tfac$(2%)),    fstore$             , ch(03),~
             at (07,55), fac(tfac$(1%)),    field9$             , ch(12),~
             at (07,68), fac(tfac$(2%)),    tlocation$          , ch(08),~
             at (07,77), fac(tfac$(2%)),    tstore$             , ch(03),~
                                                                         ~
             at (09,02), fac(pchfac$),      field14$            , ch(25),~
             at (09,29), fac(hex(ac)),      field10$            , ch(08),~
             at (09,39), fac(hex(ac)),      field11$            , ch(06),~
             at (09,47), fac(hex(ac)),      field12$            , ch(10),~
             at (09,59), fac(cfac$),        field13$            , ch(10),~
                                                                         ~
             at (10,02), fac(pcfac$(l%+0%)),part$     (l% + 0%) , ch(25),~
             at (11,02), fac(pcfac$(l%+1%)),part$     (l% + 1%) , ch(25),~
             at (12,02), fac(pcfac$(l%+2%)),part$     (l% + 2%) , ch(25),~
             at (13,02), fac(pcfac$(l%+3%)),part$     (l% + 3%) , ch(25),~
             at (14,02), fac(pcfac$(l%+4%)),part$     (l% + 4%) , ch(25),~
             at (15,02), fac(pcfac$(l%+5%)),part$     (l% + 5%) , ch(25),~
             at (16,02), fac(pcfac$(l%+6%)),part$     (l% + 6%) , ch(25),~
             at (17,02), fac(pcfac$(l%+7%)),part$     (l% + 7%) , ch(25),~
             at (18,02), fac(pcfac$(l%+8%)),part$     (l% + 8%) , ch(25),~
             at (19,02), fac(pcfac$(l%+9%)),part$     (l% + 9%) , ch(25),~
                                                                         ~
             at (10,29), fac(lfac$(l%+0%)), location$ (l% + 0%) , ch(08),~
             at (11,29), fac(lfac$(l%+1%)), location$ (l% + 1%) , ch(08),~
             at (12,29), fac(lfac$(l%+2%)), location$ (l% + 2%) , ch(08),~
             at (13,29), fac(lfac$(l%+3%)), location$ (l% + 3%) , ch(08),~
             at (14,29), fac(lfac$(l%+4%)), location$ (l% + 4%) , ch(08),~
             at (15,29), fac(lfac$(l%+5%)), location$ (l% + 5%) , ch(08),~
             at (16,29), fac(lfac$(l%+6%)), location$ (l% + 6%) , ch(08),~
             at (17,29), fac(lfac$(l%+7%)), location$ (l% + 7%) , ch(08),~
             at (18,29), fac(lfac$(l%+8%)), location$ (l% + 8%) , ch(08),~
             at (19,29), fac(lfac$(l%+9%)), location$ (l% + 9%) , ch(08),~
                                                                         ~
             at (10,39), fac(lfac$(l%+0%)), lot$     (l% + 0%)  , ch(06),~
             at (11,39), fac(lfac$(l%+1%)), lot$     (l% + 1%)  , ch(06),~
             at (12,39), fac(lfac$(l%+2%)), lot$     (l% + 2%)  , ch(06),~
             at (13,39), fac(lfac$(l%+3%)), lot$     (l% + 3%)  , ch(06),~
             at (14,39), fac(lfac$(l%+4%)), lot$     (l% + 4%)  , ch(06),~
             at (15,39), fac(lfac$(l%+5%)), lot$     (l% + 5%)  , ch(06),~
             at (16,39), fac(lfac$(l%+6%)), lot$     (l% + 6%)  , ch(06),~
             at (17,39), fac(lfac$(l%+7%)), lot$     (l% + 7%)  , ch(06),~
             at (18,39), fac(lfac$(l%+8%)), lot$     (l% + 8%)  , ch(06),~
             at (19,39), fac(lfac$(l%+9%)), lot$     (l% + 9%)  , ch(06),~
                                                                         ~
             at (10,47), fac(lfac$(l%+0%)), qty$     (l% + 0%)  , ch(10),~
             at (11,47), fac(lfac$(l%+1%)), qty$     (l% + 1%)  , ch(10),~
             at (12,47), fac(lfac$(l%+2%)), qty$     (l% + 2%)  , ch(10),~
             at (13,47), fac(lfac$(l%+3%)), qty$     (l% + 3%)  , ch(10),~
             at (14,47), fac(lfac$(l%+4%)), qty$     (l% + 4%)  , ch(10),~
             at (15,47), fac(lfac$(l%+5%)), qty$     (l% + 5%)  , ch(10),~
             at (16,47), fac(lfac$(l%+6%)), qty$     (l% + 6%)  , ch(10),~
             at (17,47), fac(lfac$(l%+7%)), qty$     (l% + 7%)  , ch(10),~
             at (18,47), fac(lfac$(l%+8%)), qty$     (l% + 8%)  , ch(10),~
             at (19,47), fac(lfac$(l%+9%)), qty$     (l% + 9%)  , ch(10),~
                                                                         ~
             at (10,59), fac(mqfac$(l%+0%)),quant$   (l% + 0%)  , ch(10),~
             at (11,59), fac(mqfac$(l%+1%)),quant$   (l% + 1%)  , ch(10),~
             at (12,59), fac(mqfac$(l%+2%)),quant$   (l% + 2%)  , ch(10),~
             at (13,59), fac(mqfac$(l%+3%)),quant$   (l% + 3%)  , ch(10),~
             at (14,59), fac(mqfac$(l%+4%)),quant$   (l% + 4%)  , ch(10),~
             at (15,59), fac(mqfac$(l%+5%)),quant$   (l% + 5%)  , ch(10),~
             at (16,59), fac(mqfac$(l%+6%)),quant$   (l% + 6%)  , ch(10),~
             at (17,59), fac(mqfac$(l%+7%)),quant$   (l% + 7%)  , ch(10),~
             at (18,59), fac(mqfac$(l%+8%)),quant$   (l% + 8%)  , ch(10),~
             at (19,59), fac(mqfac$(l%+9%)),quant$   (l% + 9%)  , ch(10),~
                                                                         ~
             at (21,02), fac(hex(a4)), inpmessage$              , ch(79),~
             at (22,02), fac(hex(8c)), pf$(1%)                  , ch(79),~
             at (23,02), fac(hex(8c)), pf$(2%)                  , ch(79),~
             at (24,02), fac(hex(8c)), pf$(3%)                  , ch(79),~
             at (24,50), fac(lcfac$),  lclocation$              , ch(08),~
             at (24,60), fac(lcfac$),  lcstore$                 , ch(03),~
                                                                         ~
             keys(pfkeys$),  key(keyhit%)

             if keyhit% <> 2% then L43620
                 l% = 1%
                 goto L42070

L43620:      if keyhit% <> 3% then L43660
                 l% = max(maxlines% - 9%, 1%)
                 goto L42070

L43660:      if keyhit% <> 4% then L43700
                 l% = max(l% - 10%, 1%)
                 goto L42070

L43700:      if keyhit% <> 5% then L43740
                 l% = min(l% + 10%, maxlines% - 9%)
                 goto L42070

L43740:      if keyhit% <> 6% then L43780
                 l% = max(l% - 1%, 1%)
                 goto L42070

L43780:      if keyhit% <> 7% then L43820
                 l% = min(l% + 1%, maxlines% - 9%)
                 goto L42070

L43820:      if keyhit% <> 10% then L43880
                 keywas% = 10% :  lclocation$ = " "
                 uselc%  =  1%
                 str(pf$(3%),64%,16%)= " " :  str(pfkeys$,16%,1%)= hex(ff)
                 goto L42150

L43880:      if keywas% <> 10% then L43950
                 if keyhit% = 1% then return
                     gosub contents_of
                         if errormsg$ <> " " then L42150
                 action%, actionwas% = 6%
                 return

L43950:      if keyhit% <> 13% then L43990
                 call "MANUAL" ("HNYLCSUB")
                 goto L42070

L43990:      if keyhit% <> 15% then L44030
                 call "PRNTSCRN"
                 goto L42070

L44030:      if keyhit% <> 25% then L44110
                close ws
                call "SCREEN" addr("C", u3%, "I", i$(), cursor%())
                u3% = u3%
                if cursor%(1%) < 10% or cursor%(1%) > 19% then L42570
                    gosub maintain_remarks
                    goto L42070

L44110:      if keyhit% <> 26% then L44150
                 gosub empty_locations
                 goto L42070

L44150:      close ws
             call "SCREEN" addr("C", u3%, "I", i$(), cursor%())
             u3% = u3%
             return

        set_pf2
            if edit% = 1% then L44550
            errormsg$ = " "
            pf$(1%)= "(1)Re-Select     (26)Empty Locs (8/24)Add  (11)Tr"&~
                     "ansfer        (13)Instructions"
            pf$(2%)= "(2)First  (4)Prev  (6)Prev 1    (9)Wdrwal  (12)Ch"&~
                     "ange (25)Rmks (15)Print Screen"
            pf$(3%)= "(3)Last   (5)Next  (7)Next 1   (10)Contents of:  "&~
                     "              (16)Exit Program"
            pfkeys$ = hex(000102030405060708090a0b0c0d0f1018191a)

            top% = l%
            convert l% to top$, pic(###)
            bottom% = min(l% + 9%, maxlines%)
            convert bottom% to bottom$, pic(###)
            call "STRING" addr("LJ", top$, 3%, top$)
            call "STRING" addr("LJ", bottom$, 3%, bottom$)
            inpmessage$ = "Now Viewing Locations " & top$ & " Thru " &   ~
                           bottom$ & ", Of " & maxlines$  & " Locations."

            if maxlines% <= 10% then L44420
                if l% > 1% then L44480
L44420:     str(pf$(2%),1%,28%) = " "           /* Deactivate 2, 4, & 6 */
            str(pfkeys$,3%,1%) = hex(ff)
            str(pfkeys$,5%,1%) = hex(ff)
            str(pfkeys$,7%,1%) = hex(ff)

            if maxlines% <= 10% then L44490
L44480:         if l% + 9% >= maxlines% then L44490 else return
L44490:     str(pf$(3%),1%,28%) = " "           /* Deactivate 3, 5, & 7 */
            str(pfkeys$,4%,1%) = hex(ff)
            str(pfkeys$,6%,1%) = hex(ff)
            str(pfkeys$,8%,1%) = hex(ff)
            return

L44550:     pf$(1%)= "(1)Cancel Action (26)Empty Locs                  "&~
                     "              (13)Instructions"
            pf$(2%)= "(2)First  (4)Prev  (6)Prev 1                     "&~
                     "              (15)Print Screen"
            pf$(3%)= "(3)Last   (5)Next  (7)Next 1                     "&~
                     "                              "

            pfkeys$ = hex(00010203040506070d0f1a)

            if action% = 2% or action% = 7% then L44680
                str(pf$(1%),18%,14%) = " "
                str(pfkeys$,11%,1%)  = hex(ff)

L44680:     if maxlines% <= 10% then L44700
                if l% > 1% then L44760
L44700:     str(pf$(2%),1%,28%) = " "
            str(pfkeys$,3%,1%) = hex(ff)
            str(pfkeys$,5%,1%) = hex(ff)
            str(pfkeys$,7%,1%) = hex(ff)

            if maxlines% <= 10% then L44770
L44760:         if l% + 9% >= maxlines% then L44770 else return
L44770:     str(pf$(3%),1%,28%) = " "
            str(pfkeys$,4%,1%) = hex(ff)
            str(pfkeys$,6%,1%) = hex(ff)
            str(pfkeys$,8%,1%) = hex(ff)
            return

        REM *************************************************************~
            *             R E M A R K S   S C R E E N  3                *~
            *-----------------------------------------------------------*~
            *  Entry and edit of Location Remarks (text).               *~
            *************************************************************

        deffn'104
            gosub set_pf4
            init(hex(80)) lfac$()
L46090:
        accept                                                           ~
           at  (01,02), "Warehouse Location Management",                 ~
           at  (01,66), "Today:",                                        ~
           at  (01,73), fac(hex(8c)), date$                     , ch(08),~
           at  (02,02), fac(hex(ac)), line2$                    , ch(79),~
           at  (03,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
           at  (04,02), "Store:",                                        ~
           at  (04,12), fac(hex(84)), txstr$                    , ch(03),~
           at  (05,02), "Part:",                                         ~
           at  (05,12), fac(hex(84)), txprt$                    , ch(25),~
           at  (06,02), "Location:",                                     ~
           at  (06,12), fac(hex(84)), txloc$                    , ch(08),~
           at  (07,02), "Lot:",                                          ~
           at  (07,12), fac(hex(84)), txlot$                    , ch(06),~
                                                                         ~
           at  (10,12), fac(hex(80)), text$(1%)                 , ch(50),~
           at  (11,12), fac(hex(80)), text$(2%)                 , ch(50),~
           at  (12,12), fac(hex(80)), text$(3%)                 , ch(50),~
           at  (13,12), fac(hex(80)), text$(4%)                 , ch(50),~
           at  (14,12), fac(hex(80)), text$(5%)                 , ch(50),~
           at  (15,12), fac(hex(80)), text$(6%)                 , ch(50),~
           at  (16,12), fac(hex(80)), text$(7%)                 , ch(50),~
           at  (17,12), fac(hex(80)), text$(8%)                 , ch(50),~
                                                                         ~
           at  (21,02), fac(hex(a4)), inpmessage$               , ch(79),~
           at  (22,02), fac(hex(8c)), pf$(1%)                   , ch(79),~
           at  (23,02), fac(hex(8c)), pf$(2%)                   , ch(79),~
           at  (24,02), fac(hex(8c)), pf$(3%)                   , ch(79),~
                                                                         ~
           keys(pfkeys$),  key(keyhit%)

           if keyhit% <> 13% then L46460
               call "MANUAL" ("HNYLCSUB")
               goto L46090

L46460:    if keyhit% <> 15% then L46500
               call "PRNTSCRN"
               goto L46090

L46500:    return

        set_pf4
            pf$(1%)= "(1)Re-Select                                 "&    ~
                     "                  (13)Instructions"
            pf$(2%)= "                                             "&    ~
                     "                  (15)Print Screen"
            pf$(3%)= "                                             "&    ~
                     "                  (16)Update Rmks "
            pfkeys$ = hex(00010d0f10)
            inpmessage$ = "Enter Or Modify Remarks For This Part/Store/"&~
                          "Location/Lot."
            return

        REM *************************************************************~
            *         T E S T   D A T A  -  S C R E E N   1             *~
            *-----------------------------------------------------------*~
            * Test data for part, lot and store.                        *~
            *************************************************************

        deffn'151

            if action% <> 6% then L50200

        REM Test Location and Store for Contents Of...
            plowkey$ = all(hex(00))
            str(plowkey$,1%,3%) = str(lcstore$)
            str(plowkey$,4%,8%) = str(lclocation$)
            call "PLOWALTS" (#6, plowkey$, 0%, 11%, f1%(6%))
                if f1%(6%) = 1% then L50200
            errormsg$ = "No Contents Found For Store: " & lcstore$ &     ~
                        ", Location: " & lclocation$
            return

L50200: REM Test store & get descr for header...
            errormsg$ = " "
            call "GETCODE" (#2, selectstore$, sdescr$, 1%, 0, f1%(2%))
                if f1%(2%) = 1% then L50270
            errormsg$ = "Enter Or Select A Vaild Store Code"
            return

L50270: REM Stop here if selection is Contents Of...
            if action% = 6% then return

        REM Test Lot...
            if selectlot$ = " " or selectlot$ = "ALL" then L50370
                errormsg$ = "LOT-CHECK"
                call "LOTVALID" (str(selectpart$), str(selectstore$),    ~
                                 str(selectlot$), #1, #4, #7,errormsg$)
                if errormsg$ <> " " then return

L50370: REM If Part is blank, store/lot is validated against HNYLOCNS.   ~
            If part entered or passed, Part is first validated against   ~
            HNYMASTR, then part/store/lot is validated against HNYLOCNS. ~

        REM Test for blank part number and specific lot...
            if selectpart$ <> " " then L50550
                if selectlot$ <> "ALL" then L50470
            errormsg$ = "You Must Enter A Specific Lot When Part Is Blank"
            return

L50470: REM Test for Store/Lot in HNYLOCNS...
            plowkey$ = all(hex(00))
            plowkey$ = str(selectstore$) & str(selectlot$)
            call "PLOWALTS" (#6, plowkey$, 4%, 9%, f1%(6%))
                if f1%(6) = 1% then return
            errormsg$ = "No Locations Found For This Store/Lot"
            return

L50550: REM Test for Part in HNYMASTR...
            descr$ = hex(06) & "Valid Parts"
            call "GETCODE" (#4, selectpart$, descr$, 1%, .3, f1%(4%))
                if f1%(4%) = 1% then L50610
                    errormsg$ = "Enter A Valid Part Number Or Leave Blank"
                    return
L50610:     pdescr$ = descr$

        REM Test for Store/Part/Lot in HNYLOCNS...
            plowkey$ = all(hex(00))
            if selectlot$ <> "ALL" then L50720
              str(plowkey$,1%,28%) = str(selectstore$) & str(selectpart$)
            call "PLOWALTS" (#6, plowkey$, 1%, 28%, f1%(6%))
                if f1%(6%) <> 1% then return
                    templot$ = selectlot$
                    return

L50720:     str(plowkey$,1%,34%) = str(selectstore$) & str(selectpart$)  ~
                                                        & str(selectlot$)
            call "PLOWALTS" (#6, plowkey$, 1%, 34%, f1%(6%))
               if f1%(6%) <> 1% then L50760
                   templot$ = selectlot$
                   return
L50760:     errormsg$ = "No Locations Found For This Part/Lot/Store"
            return

        REM *************************************************************~
            *         T E S T   D A T A  -  S C R E E N   2             *~
            *-----------------------------------------------------------*~
            * Test for new quantities entered.                          *~
            *************************************************************

        deffn'152
            errormsg$ = " "
            total, totaladd, totalsub, addqty, subqty, oldqty, chgqty = 0
            on action% gosub         L52180,   /* Selection - No Edit   */~
                                     L52210,   /* Transfer Quantities   */~
                                     L53390,   /* Add Quantities        */~
                                     L53880,   /* Withdraw Quantities   */~
                                     L54270,   /* Change Quantities (+-)*/~
                                     L52180,   /* Contents Of - No Edit */~
                                     L54760    /* Add/Create            */
            return

L52180: REM Dummy Test - No Edit...
            return

L52210: REM Test for Transfer...
*          Test Part Number...
            pdescr$ = hex(06) & "Valid Parts"
            call "GETCODE" (#4, selectpart$, pdescr$, 1%, .3, f1%(4%))
                if f1%(4%) <> 0% then L52280
            errormsg$ = "Enter Or Select A Valid Part Number"
            return
L52280
*        Test Source Lot (Store/Part/Lot)...
            if selectlot$ = " " then L52390
                plowkey$ = all(hex(00))
                    str(plowkey$,1%,3%)  = str(fstore$)
                    str(plowkey$,4%,25%) = str(selectpart$)
                    str(plowkey$,29%,6%) = str(selectlot$)
                call "PLOWALTS" (#6, plowkey$, 1%, 34%, f1%(6%))
                    if f1%(6%) <> 0% then L52390
                errormsg$ = "No Record Of This Part/Lot In The Source L"&~
                            "ocation"
                return
L52390
*          Test Source Location (Store/Location/Part/Lot)...
            if flocation$ <> " " then L52430
                errormsg$ = "Source Location CANNOT Be Blank"
                return
L52430:     plowkey$ = str(fstore$) & str(flocation$) & str(selectpart$) ~
                       & str(selectlot$)
            call "READ100" (#6, plowkey$, f1%(6%))
            if f1%(6%) = 1% then L52490
                errormsg$ = "Source Location/Lot Does Not Exist"
                return
L52490
*        Are Valid Locations Required?.....
            if vlr$ <> "Y" then L52700
                plowkey$ = all(hex(00))
                plowkey$ = str(fstore$) & str(flocation$)
            call "READ100" (#8, plowkey$, f1%(8%))
                if f1%(8) = 1% then L52700
L52550:     k% = 2%
            call "ASKUSER" (k%, "*** VALID LOCATIONS REQUIRED ***",      ~
                 "Source Location " & flocation$ & " is not valid.",     ~
                 "Press RETURN to continue with this location -OR-",     ~
                 "Press any other PFKEY to select from a list of valid" &~
                 " locations.")
            if k% = 0% then L52700
                if k% > 32% then L52550
            descr$ = hex(06) & "Valid Locations For Store: " & fstore$
            str(plowkey$,4%,8%) = hex(00)
            call "PLOWCODE" (#8, plowkey$, descr$, 3%, 0, f1%(8%))
                if f1%(8%) = 1% then L52690
                    errormsg$ = "You Must Enter Or Select A Location"
                    return
L52690:     flocation$ = str(plowkey$,4%,8%)
L52700
*        Test Transfer Quantity...
            if tquantity$ <> " " then L52740
                errormsg$ = "Enter The Quantity To Be Transfered"
                return
L52740:     call "NUMTEST" (tquantity$,0,999999999,errormsg$,2.2,chgqty)
                if errormsg$ <> " " then return
            chgqty = abs(chgqty)
*        Is Source Location Quantity Sufficient?...
            plowkey$ = str(fstore$) & str(flocation$) & str(selectpart$)&~
                                                        str(selectlot$)
            call "READ100" (#6, plowkey$, f1%(6%))
            if f1%(6) = 1% then L52850
                errormsg$ = "No Quantities Exist For This Part/Lot/Loca"&~
                            "tion/Store"
                return
L52850:     get #6 using L52860, qty
L52860:         FMT POS(573), PD(14,4)
            if chgqty <= qty then L52930
                call "CONVERT" (qty, 2.2, qty$)
                call "STRING" addr("LJ", qty$, 10%, qty$)
                errormsg$ ="Insufficient Quantity In Source Location: " &~
                           qty$ & " Available"
                return
L52930
*        Test destination location...
            if flocation$ <> tlocation$ then L52972
                errormsg$ = "Source And Destination Locations Must No" & ~
                            "t Be The Same"
                return
L52972:     if tstore$ <> " " then L52980
                errormsg$ = "TO Store CANNOT Be Blank"
                return
L52980:     call "GETCODE" (#2, tstore$, sdescr$, 1%, 0, f1%(2%))
            if f1%(2%) = 1% then L53020
                errormsg$ = "Invalid Destination Store Code"
                return
L53020:     if tlocation$ <> " " then L53050
                errormsg$ = "TO Location CANNOT Be Blank"
                return
L53050:     init(hex(00)) plowkey$
            plowkey$ = str(tstore$) & str(tlocation$)
            if vlr$ <> "Y" then L53240
               call "READ100" (#8, plowkey$, f1%(8%))
                   if f1%(8%) = 1% then L53240
            k% = 2%
            call "ASKUSER" (k%, "*** VALID LOCATIONS REQUIRED ***",      ~
                 "TO location " & tlocation$ & " is invalid.",     "Pre"&~
                 "ss RETURN to continue with this locations -OR-", "Pre"&~
                 "ss any other PFKEY to select from a list of valid loc"&~
                 "ations.")
               if k% = 0% then L53240
            descr$ = hex(06) & "Valid Locations For Store: " & fstore$
            str(plowkey$,4%,8%) = hex(00)
            call "PLOWCODE" (#8, plowkey$, descr$, 3%, 0, f1%(8%))
                if f1%(8%) = 1% then L53230
                  errormsg$ = "You Must Enter Or Select A Valid Location"
                  return
L53230:     tlocation$ = str(plowkey$,4%,8%)
L53240
*          Is part currently stored in destination location? ...
            plowkey$ = str(tstore$) & str(tlocation$) & str(selectpart$) ~
                                                      & str(selectlot$)
            call "READ100" (#6, plowkey$, f1%(6%))
                if f1%(6%) = 1% then return
            k% = 2%
            call "ASKUSER" (k%, "*** CREATE NEW QUANTITY RECORD ***",    ~
                 "The selected Part/Lot combination is not currently "  &~
                 "stocked in the", "destination location.  Press RETURN"&~
                 " to continue with this location -OR-", "press PF1 to "&~
                 "cancel action.")
            if k% = 0% then return
                init(" ") tquantity$, flocation$, tlocation$
                return

L53390: REM Test for Addition...
            for i% = 1% to maxlines%
                if quant$(i%) = " " then L53770
                    convert quant$(i%) to addqty, data goto qty_error
                    addqty = abs(addqty)

                if vlr$ <> "Y" then L53600
                    plowkey$ = str(store$(i%)) & str(location$(i%)) &    ~
                               str(part$(i%)) & str(lot$(i%))
                    call "READ100" (#8, plowkey$, f1%(8%))
                        if f1%(8%) = 1% then L53600
                k% = 2%
                call "ASKUSER" (k%, "*** VALID LOCATIONS REQUIRED ***",  ~
                     "Location " & location$(i%) & " is not valid!",     ~
                     "Press Return to continue adding to this location ",~
                     "-OR- press any PFKEY to bypass this location and "&~
                     "continue.")
                if k% =  0% then L53600
                    quant$(i%) = " "
                    goto L53770

L53600:         if lot$(i%) = " " then L53750
                    plowkey$ = str(part$(i%)) & str(store$(i%)) &        ~
                                                str(lot$(i%))
                    call "READ100" (#7, plowkey$, f1%(7%))
                        if f1%(7%) = 1% then L53750
L53650:         k% = 2%
                call "ASKUSER" (k%, "*** INVALID LOT ***",               ~
                      lot$(i%) & " is not a valid inventory lot "        ~
                      & "for this part/store.", "You can only withdraw " ~
                      & "quantities from this lot.", "Press RETURN to "  ~
                      & "bypass this location/lot and continue.")
                if k% <> 0% then L53650
                    quant$(i%) = " "
                    goto L53770

L53750:     if qtytracking% <> 1% then L53770
                qtylocated = qtylocated + addqty
L53770:     next i%
            if qtytracking% <> 1% then L53860
                if qtylocated > qtyin then qtyremaining = 0 else         ~
                   qtyremaining = qtyin - qtylocated
                call "CONVERT" (qtylocated, 2.2, qtylocated$)
                call "CONVERT" (qtyremaining, 2.2, qtyremaining$)
                str(qtymessage$(1%),16%,10%) = qtyin$
                str(qtymessage$(2%),16%,10%) = qtylocated$
                str(qtymessage$(3%),16%,10%) = qtyremaining$
L53860:     return

L53880: REM Test for Withdrawal...
            for i% = 1% to maxlines%
                if quant$(i%) = " " then L54160
                    convert quant$(i%) to subqty, data goto qty_error
                    convert qty$(i%)   to oldqty, data goto qty_error
                if subqty <= oldqty then L53970
                    errormsg$ = "You Can Not Withdraw More Than The Qua"&~
                                "ntity Available"
                    return
L53970:         subqty = abs(subqty)

                if vlr$ <> "Y" then L54140
                    plowkey$ = str(store$(i%)) & str(location$(i%)) &    ~
                               str(part$(i%)) & str(lot$(i%))
                    call "READ100" (#8, plowkey$, f1%(8%))
                        if f1%(8%) = 1% then L54140
                k% = 2%
                call "ASKUSER" (k%, "*** VALID LOCATIONS REQUIRED ***",  ~
                     "Location " & location$(i%) & " is not valid!",     ~
                     "Press Return to continue withdrawing from from th"&~
                     "is location", "-OR- press any other PFKEY to bypa"&~
                     "ss this location and continue.")
                if k% = 0% then L54140
                    quant$(i%) = " "
                    goto L54160

L54140:     if qtytracking% <> 1% then L54160
                qtylocated = qtylocated + subqty
L54160:     next i%
            if qtytracking% <> 1% then L54250
                if qtylocated > qtyin then qtyremaining = 0 else         ~
                   qtyremaining = qtyin - qtylocated
                call "CONVERT" (qtylocated, 2.2, qtylocated$)
                call "CONVERT" (qtyremaining, 2.2, qtyremaining$)
                str(qtymessage$(1%),16%,10%) = qtyin$
                str(qtymessage$(2%),16%,10%) = qtylocated$
                str(qtymessage$(3%),16%,10%) = qtyremaining$
L54250:     return

L54270: REM Test for Change (Add and/or Withdrawal)...
            for i% = 1% to maxlines%
                if quant$(i%) = " "  then L54730
                    convert quant$(i%) to chgqty, data goto qty_error
                    if chgqty < 0  then subqty = abs(chgqty)
                    if chgqty >= 0 then addqty = abs(chgqty)
                    convert qty$(i%) to oldqty, data goto qty_error
                if subqty <= oldqty then L54390
                    errormsg$ = "You Can Not Withdraw More Than The Qua"&~
                                    "ntity Available"
                    return

L54390:         if lot$(i%) = " " then L54570
                    if addqty > 0 then L54410 else L54570
L54410:         plowkey$ = all(hex(00))
                plowkey$ = str(part$(i%)) & str(store$(i%)) &            ~
                                                            str(lot$(i%))
                call "READ100" (#7, plowkey$, f1%(7%))
                    if f1%(7%) = 1% or subqty > 0 then L54570

L54470:         k% = 2%
                call "ASKUSER" (k%, "*** INVALID LOT ***",               ~
                      lot$(i%) & " is not a valid inventory lot "        ~
                      & "for this part/store.", "You can only withdraw " ~
                      & "quantities from this lot.", "Press RETURN to "  ~
                      & "bypass this location/lot and continue.")
                if k% <> 0% then L54470
                    quant$(i%) = " "
                    goto L54730

L54570:         if vlr$ <> "Y" then L54720
                    plowkey$ = str(store$(i%)) & str(location$(i%)) &    ~
                               str(part$(i%)) & str(lot$(i%))
                    call "READ100" (#8, plowkey$, f1%(8%))
                        if f1%(8%) = 1% then L54720
                k% = 2%
                call "ASKUSER" (k%, "*** VALID LOCATIONS REQUIRED ***",  ~
                     "Location " & location$(i%) & " is not valid!",     ~
                     "Press RETURN to continue changing this location "& ~
                     "-OR-", "press any other PFKEY to bypass this loca"&~
                     "tion and continue.")
                if k% =  0% then L54720
                    quant$(i%) = " "
                    goto L54730

L54720:     total = total + addqty + subqty
L54730:     next i%
            return

L54760: REM Test for New Addition...
            if tquantity$ <> " " then L54800
                errormsg$ = "Add Quantity CANNOT Be Blank"
                return
L54800:     convert tquantity$ to tquan, data goto qty_error
            if tquan > 0 then L54832
                errormsg$ = "Add Quantity CANNOT Be Negative"
                return
L54832:     if tstore$ <> " " then L54840
                errormsg$ = "TO Store CANNOT Be Blank"
                return
L54840:     if selectlot$ = " " then L54930
                errormsg$ = "LOT-CHECK"
                call "LOTVALID" (" ", str(tstore$), str(selectlot$), #1, ~
                                 #4,#7, errormsg$)
                    if errormsg$ <> " " then return
                call "LOTVALID" (str(selectpart$), str(tstore$),         ~
                                str(selectlot$),#1,#4,#7, errormsg$)
                if errormsg$ <> " " then return

L54930:     if vlr$ <> "Y" then L55060
                    plowkey$ = str(tstore$) & str(tlocation$)
                    call "READ100" (#8, plowkey$, f1%(8%))
                        if f1%(8%) = 1% then L55060
L54970:         k% = 2%
                call "ASKUSER" (k%, "*** VALID LOCATIONS REQUIRED ***",  ~
                     "Location "& tlocation$ & " is not valid!",         ~
                     "Press Return to continue adding to this location"& ~
                     " -OR-", "Press PF1 to cancel action.")
                if k% = 0% then L55060
                    if k% <> 1% then L54970
                cancel_action% = 1%
                tquantity$, tlocation$ = " "
                tstore$ = defaultstore$
                return

L55060:     if qtytracking% <> 1% then return
                qtylocated = qtylocated + tquan
                if qtylocated > qtyin then qtyremaining = 0 else         ~
                   qtyremaining = qtyin - qtylocated
                call "CONVERT" (qtylocated, 2.2, qtylocated$)
                call "CONVERT" (qtyremaining, 2.2, qtyremaining$)
                str(qtymessage$(1%),16%,10%) = qtyin$
                str(qtymessage$(2%),16%,10%) = qtylocated$
                str(qtymessage$(3%),16%,10%) = qtyremaining$
                return

        qty_error
            errormsg$ = "Invalid Quantity, Please Re-Enter"
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

        exit_checking
            if qtyin = 0 then exit_program
            if qtylocated >= qtyin then exit_program
            if qtytracking% = 0% then exit_program

L65210:     k% = 0%
            call "ASKUSER" (k%, "*** REQUEST CONFIRMATION ***",          ~
               "You have not indicated locations for the entire transac"&~
               "tion quantity of ", qtyin$, "Press RETURN to continue w"&~
                "ith this program -OR- press PF16 to exit.")
            if k% = 16% then  exit_program
              if k% <> 0% then L65210
            return

        exit_program
            end
