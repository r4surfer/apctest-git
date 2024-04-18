        REM *************************************************************~
            *                                                           *~
            *  H   H  N   N  Y   Y V   V   L      RRRR   PPPP   TTTTT   *~
            *  H   H  NN  N   Y Y  V   V   L      R   R  P    P   T     *~
            *  HHHHH  N N N    Y   V   V   L      RRRR   PPPP     T     *~
            *  H   H  N  NN    Y    V  V   L      R   R  P        T     *~
            *  H   H  N   N    Y     V     LLLLL  R   R  P        T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYVLGUI - Sends summarized Inventory Valuation Data to   *~
            *            a Visual Basic client.                         *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+-----------------WHAT---------------------+-WHO-*~
            * 11/29/95 ! Original cloned from HNYVLRPT            ! LDJ *~
            *************************************************************

        dim                                                              ~
            account$12,                  /* INVENTORY ASSET ACCOUNT    */~
            accountdesc$30,              /* ACCOUNT DESCRIPTION        */~
            blankline$79,                /* USED IN SCREEN FORMAT      */~
            columnttl$51,                /* Column titles line         */~
            catcode$4,                   /* Part Category Code         */~
            catdescr$30,                 /* Category Code Description  */~
            cdir$32,                     /* CAELUS VB exe's directory  */~
            company$60,                  /* Company or Division Name   */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* MM/DD/YY FORMATED DATE     */~
            descr$32,                    /* DESCRIPTION                */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* ERROR MESSAGE FOR INPUT    */~
            fmcatcode$4,                 /* Starting Category Code     */~
            fmpart$25,                   /* Starting Part Number       */~
            fmstore$3,                   /* Starting Store Number      */~
            hnycstcd$1,                  /* INVENTORY COST METHOD      */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lastpart$25, lastpartdescr$32, /* Last Part Number Printed */~
            last_account_printed$12,     /* LAST G/L ACCOUNT PRINTED   */~
            last_acctdescr$30,           /* LAST G/L ACCOUNT PRINTED   */~
            line2$79,                    /* Screen Line #2             */~
            tocatcode$4,                 /* Ending Category Code       */~
            topart$25,                   /* Ending Part Number         */~
            last_ctgy_printed$25,        /* Last Category Printed      */~
            last_ctgydescr$30,           /* Last Category Printed      */~
            tostore$3,                   /* Ending Store Number        */~
            last_store_printed$3,        /* LAST STORE PRINTED         */~
            last_storedescr$30,          /* LAST STORE PRINTED         */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            lot$6,                       /* LOT NUMBER                 */~
            newreadkey$43,               /* READ KEY FOR WORK FILE     */~
            partnr$25,                   /* THIS PART NUMBER           */~
            partsprinted$12,             /* Edited # of parts printed  */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pgm$8,                       /* Current File (ProgName)    */~
            sortflag$1,                  /* ORDER TO SORT REPORT       */~
            storedesc$30,                /* STORE DESCRIPTION          */~
            storenr$3,                   /* STORE NUMBER               */~
            qty(5),                      /* QUANTITIES FROM DISK.      */~
            qtytype$1,                   /* TYPE OF INVENTORY QUANTITY */~
            time$08,                     /* TIME IN HOUR AND MINUTES   */~
            username$24, userid$3,uw$1   /* USER NAME & ID             */

        dim f2%(10),                     /* FILE STATUS FLAGS FOR      */~
            f1%(10),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(10)20,                 /* RETURN CODE FROM "FILEOPEN"*/~
            axd$(10)4                    /* AXD POINTER FROM "FILEOPEN"*/

        dim lopart$25, hipart$25, lostore$3, histore$3, locatcode$4,     ~
            hicatcode$4,           prompt$(7)28

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.03 08/12/96 Last Wang Release"
        REM *************************************************************
            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !          D E S C R I P T I O N           *~
            *-----+----------+------------------------------------------*~
            * # 2 ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * # 3 ! HNYQUAN  ! INVENTORY STORE QUANTITY DETAIL FILE     *~
            * # 4 ! STORNAME ! STORE NAMES AND ADDRESS                  *~
            * # 5 ! CATEGORY ! Part Category File                       *~
            * # 6 ! WORK2    ! WORK FILE                                *~
            * # 7 ! GLMAIN   ! GENERAL LEDGER MASTER FILE               *~
            * # 8 ! SYSFILE2 ! System Info (ranges for background)      *~
            *************************************************************

            select  #2, "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90,  keylen = 4, dup

            select #3,  "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 650,                                   ~
                        keypos = 17, keylen = 44,                        ~
                        alternate key 1, keypos =  1, keylen = 44

            select  #4, "STORNAME",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 3

            select # 5, "CATEGORY",                                      ~
                        varc, indexed,                                   ~
                        recsize = 200,                                   ~
                        keypos  =   1, keylen = 4

            select #6, "WORK6",          /* WORK FILE */                 ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 272,                                  ~
                         keypos = 1, keylen = 4,                         ~
                         alt key 1, keypos = 39, keylen = 34, dup,       ~
                             key 2, keypos =  5, keylen = 34, dup,       ~
                             key 3, keypos = 73, keylen = 29, dup

            select #7, "GLMAIN",         /* GENERAL LEDGER MASTER FILE */~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 300,                                  ~
                         keypos = 1, keylen = 9

            select #08,  "SYSFILE2",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

            call "CHECKGUI" addr(gui%)
            if gui% = 0% then exit_program

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (#2, "SHARE", f2%(2%), rslt$(2%), axd$(2%))
            call "OPENFILE" (#3, "SHARE", f2%(3%), rslt$(3%), axd$(3%))
            call "OPENFILE" (#4, "SHARE", f2%(4%), rslt$(4%), axd$(4%))
            call "OPENFILE" (#5, "SHARE", f2%(5%), rslt$(5%), axd$(5%))
            call "OPENFILE" (#7, "SHARE", f2%(7%), rslt$(7%), axd$(7%))
            call "OPENFILE" (#8, "SHARE", f2%(8%), rslt$(8%), axd$(8%))
            if f2%(2%) > 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * SETS DATES FOR SCREEN DISPLAY.                            *~
            *************************************************************

            call "EXTRACT" addr ("NA", username$, "ID", userid$,         ~
                     "CF", pgm$)
            cdir$ = "./caelus/vbapps/"
            date$ = date : call "DATEFMT" (date$)
            time$ = " "  : call "TIME" (time$)
            call "COMPNAME" (12%, company$, ret%)
            wseq% = 0  :  ret% = 0%
            partsprinted% = 0%
            recnbr% = val(str(rslt$(3%),17%,4%),4)
            uw$=hex(7f)
            str(columnttl$, 1) = "Beginning Code"
            str(columnttl$,27) = "Ending Code"
            prompt$(1%) = "Part Number"
            prompt$(2%) = "Store Number"
            prompt$(3%) = "Part Category"
            prompt$(4%) = "Quantity Type"
            prompt$(5%) = "Report Sort Order"
            prompt$(6%) = "Summary or Detail? (S/D)"
            prompt$(7%) = "Print in Background? (Y/N)"

            str(line2$,62%) = str(pgm$) & ": " & str(cms2v$,,8%)

            call "GETPARM" addr("ID",              /* Hidden Getparm   */~
                                "R",               /* 'R'equired       */~
                                "STARTUP ",        /* Prname           */~
                                "@",               /* Pf Key Receiver  */~
                                "0001",            /* Message Id       */~
                                "VLGUI ",          /* Message Issuer   */~
                                0%,                /* Message Lines    */~
                                "K",               /* Field Spec Type  */~
                                "SORTORDR",        /* Field Key Word   */~
                                sortflag$,         /* Field Variable   */~
                                01%,               /* Field Length     */~
                                "A",               /* Row Flag (Abs)   */~
                                13%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                50%,               /* Column Number    */~
                                "U",               /* Field Type       */~
                                "K",               /* Field Spec Type  */~
                                "QTYTYPE ",        /* Field Key Word   */~
                                qtytype$,          /* Field Variable   */~
                                01%,               /* Field Length     */~
                                "A",               /* Row Flag (Abs)   */~
                                15%,               /* Row              */~
                                "A",               /* Column Flag (Abs)*/~
                                50%,               /* Column Number    */~
                                "U")               /* Field Type       */

        REM *************************************************************~
            *         I N P U T   S E L E C T I O N S                   *~
            *-----------------------------------------------------------*~
            * Input Selection & Sort Criteria.                          *~
            *************************************************************

        inputmode1
            gosub initialize_variables

            for fieldnr% = 1% to 5%
L10400:         gosub'051(fieldnr%)
                    if enabled% = 0 then L10880
L10480:         gosub'101(fieldnr%, 1%)
                    if keyhit%  =  1% then gosub startover
                    if keyhit% <>  4% then L10800
L10600:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10480
                         if fieldnr% = 1% then L10400
                         goto L10600
L10800:             if keyhit% = 16% and fieldnr% = 1% then exit_program
                    if keyhit% <>  0 then L10480
L10880:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                    if errormsg$ <> " " then L10480
            next fieldnr%

        REM *************************************************************~
            *            E X T R A C T   S E C T I O N                  *~
            *-----------------------------------------------------------*~
            * Extract data for report based on input criteria.          *~
            *************************************************************

            REM start up VB App
            call "GUIVBRUN" ("hnyvlgui.exe" ,0%, return%)
            if return% < 0% then exit_program
            call "WORKOPEN" (#6, "OUTPUT", recnbr%, f2%(6%))
            call "SHOSTAT" ("Extracting Data...")
            convert sortflag$ to sort%

        readloop
            call "READNEXT" (#3, f1%(3%))
                if f1%(3%) = 0% then print_report
            get #3, using L11600, partnr$, storenr$, lot$, qty(),         ~
                                 cost, account$, hnycstcd$
L11600:         FMT POS(17), CH(25), CH(3), CH(6), POS(69), 5*PD(14,4),  ~
                             POS(117), PD(14,4), POS(259), CH(9),        ~
                             POS(403), CH(1)
            call "DESCRIBE" (#2, partnr$, descr$, 0%, f1%(2))
            if f1%(2) <> 0% then L11920
                descr$ = "Part Code Not on File"
                catcode$ = hex(00)
                goto L12000
L11920:     get #2 using L11960, catcode$
L11960:         FMT POS(90), CH(4)
L12000:     call "DESCRIBE" (#4, storenr$, storedesc$, 0%, f1%(4))
            if storedesc$ = " " then storedesc$ = "Store Code not on file"
            call "DESCRIBE" (#7, account$, accountdesc$, 0%, f1%(7))
            if accountdesc$ = " " then                                   ~
                                  accountdesc$ = "Asset Acct not on file"
            call "DESCRIBE" (#5, catcode$, catdescr$, 0%, f1%(5))
            if catdescr$ = " "                                           ~
                then catdescr$ = "Category Code not on file"
            wseq% = wseq% + 1%
            write #6 using L14160, wseq%, account$, partnr$, storenr$,    ~
                     partnr$, lot$, catcode$, partnr$, descr$,           ~
                     storedesc$, accountdesc$, catdescr$, qty(),         ~
                     cost, hnycstcd$
            goto readloop

        REM *************************************************************~
            *                  PRINT REPORT SECTION                     *~
            *                                                           *~
            *************************************************************

        print_report
            close #6
            call "WORKOPN2" (#6, "INPUT", 1000%, f2%(6%))
            partsprinted%, gpartsprinted%, newpart% =0%
            sum_cost, sum_qty, total_cat_cost, grand_total = 0
            storenr$, account$, lastpart$, catcode$ = " "
            init (hex(00)) newreadkey$
            convert qtytype$  to qtyp%
            okprint% = 0%  : first_time% = 1%
            if wseq% = 0% then wrap_up
            REM *** Message ID 1 = What Type of Report - 1,2, or 3
            call "SENDCMD"   (uw$ & "UWVBXHNYVLGUI,0," & sortflag$ & uw$)
*           CALL "GUIFNGER" (0%,0%," ",RETURN%) /* wait for response */
*          IF RETURN%<0 THEN CALL "ASKGUI" (0%,"No Response"," ",RETURN%)
*       ** Reads begin here
            call "REDALT4" (#6, newreadkey$, sort%, f1%(6%))
            call "SHOSTAT" ("Downloading Data...")
            goto L13640
        read_loop1
            call "READNEXT" (#6, f1%(6%))
L13640:        if f1%(6%) = 0 then wrap_up
            get #6 using L14160, wseq%, account$, partnr$, storenr$,      ~
                   partnr$, lot$, catcode$, partnr$, descr$,             ~
                   storedesc$, accountdesc$, catdescr$, qty(),           ~
                   cost, hnycstcd$
            call "GLFMT" (account$)
            if partnr$ <= lopart$ or partnr$ > hipart$ then read_loop1
            if catcode$ <= locatcode$ or                                 ~
                          catcode$ > hicatcode$ then read_loop1
            if storenr$ <= lostore$ or storenr$ > histore$ then read_loop1
            if qty(qtyp%) = 0 then read_loop1
            okprint% = okprint% + 1%
*       *** Format Statement for Work File
L14160:     FMT BI( 4),                  /* Record Sequence Number     */~
                CH( 9),                  /* Inv. Asset Account Number  */~
                CH(25),                  /* Part Number                */~
                CH( 3),                  /* Store Number               */~
                CH(25),                  /* Part Number                */~
                CH( 6),                  /* Lot Number                 */~
                CH( 4),                  /* Category Code              */~
                CH(25),                  /* Part Number                */~
                CH(32),                  /* Part Description           */~
                CH(30),                  /* Store Description          */~
                CH(30),                  /* Acct Description           */~
                CH(30),                  /* Category Description       */~
                5 * PD(14,4),            /* 5 Quantity Fields          */~
                PD(14,4),                /* Part Cost                  */~
                CH( 1)                   /* Inventory Costing Method   */

*       **** Report selection branching is done here
            if sort% > 1% then L15160
                if first_time% = 1% then L15080
                if last_store_printed$ = storenr$ then L15080
                     gosub print_store_totals
                     last_store_printed$ = storenr$
                     last_storedescr$    = storedesc$
L15080:         gosub print_detail
                goto read_loop1
L15160:     if sort% > 2% then L15480
                if first_time% = 1% then L15400
                if last_account_printed$ = account$ then L15400
                     gosub print_account_totals
                     last_account_printed$ = account$
                     last_acctdescr$       = accountdesc$
L15400:         gosub print_detail
                goto read_loop1
L15480:     if sort% > 3% then read_loop1
                if  first_time% = 1% then L15720
                if last_ctgy_printed$ = catcode$ then L15720
                     gosub print_ctgy_totals
                     last_ctgy_printed$ = catcode$
                     last_ctgydescr$    = catdescr$
L15720:         gosub print_detail
                goto read_loop1

        REM Print Part And Lot Information
        print_detail
            if lastpart$ <> " " then L16360
                first_time% = 0%
                lastpart$ = partnr$
                lastpartdescr$ = descr$
                last_store_printed$ = storenr$
                last_storedescr$    = storedesc$
                last_account_printed$ = account$
                last_acctdescr$       = accountdesc$
                last_ctgy_printed$ = catcode$
                last_ctgydescr$    = catdescr$
                newpart% = 1%
L16360:     if lastpart$ = partnr$ then L16440
                gosub print_part_totals
L16440:     totalcost = round(cost*qty(qtyp%),2)
            sum_qty = sum_qty + qty(qtyp%)
            sum_cost = sum_cost + totalcost
            if newpart% = 0% then L16680
                newpart% = 0%
                partsprinted% = partsprinted% + 1%
L16680:     return

        print_part_totals
            total_cat_cost = total_cat_cost + sum_cost
                if sum_qty = 0                                           ~
                     then average_cost = 0                               ~
                     else average_cost = round(sum_cost / sum_qty, 4)
            lastpart$ = partnr$
            lastpartdescr$ = descr$
            newpart% = 1%
            sum_qty, sum_cost = 0
            return

        REM *************************************************************~
            *    Prints Number Of Parts Printed, Total Cost For All     *~
            *    Parts Printed And Exits Program If End Of Print.       *~
            *************************************************************
        wrap_up
            if okprint% > 0% then L19400
                call "ASKGUI" (1%, "*** NULL SET SELECTED ***",          ~
                     "There is no Inventory Data to Summarize!",u3%)
                call "SENDCMD"  (uw$ & "UWVBXHNYVLGUI,99" & uw$)
                goto exit_program
L19400:     on sort% gosub print_store_totals,                           ~
                           print_account_totals,                         ~
                           print_ctgy_totals

            gosub'200(gpartsprinted%)
            call "CONVERT" (grand_total, 2.2,  grtotal$)
            call "SENDCMD"     (uw$ & "UWVBXHNYVLGUI,9," & grtotal$ & uw$)
            call "SHOSTAT" ("Data Downloaded, Waiting for Client Command ~
        ~(Press F16 to Cancel)")
L19610:     call "GUIFNGER" (0%,0%," ",sort%)
            if sort% <= 0% then L19610
            convert sort% to sortflag$,pic(0)
            on sort% goto print_report,print_report,print_report
            goto exit_program

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 0%
            on fieldnr% gosub L20150,      /* Part Range          */      ~
                              L20190,      /* Store Range         */      ~
                              L20230,      /* Category Range      */      ~
                              L20300,      /* Quantity Type       */      ~
                              L20330       /* Print Order         */
            return

L20150: REM Default/Enable For Part Range
            if fmpart$ = " " then fmpart$ = "ALL"
            return

L20190: REM Default/Enable For Store Range
            if fmstore$ = " " then fmstore$ = "ALL"
            return

L20230: REM Default/Enable For Category Range
            if fmcatcode$ = " " then fmcatcode$ = "ALL"
            return

        REM Default/Enable For Last Part Number
            return

L20300: REM Default/Enable For Quantity Type
            if qtytype$ = " " then qtytype$ = "1"
            return

L20330: REM Default/Enable For Print Order
            if sortflag$ = " " then sortflag$ = "3"
            return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Part Number range, partial, 'FIRST', 'LAST', 'ALL' or '?'~
        ~ Wildcard.",                                                     ~
         "Enter Store Number range, partial, 'ALL' or '?' Wildcard.",    ~
         "Enter Part Category range, partial, 'ALL' or '?' Wildcard.",   ~
         "Enter '1' - ON HAND, '2' - BACK ORDERED or '3' - ON ORDER.   ",~
         "Enter '1' sort by STORE, '2' by INVENTORY ASSET ACCOUNT or '3' ~
        ~by PART CATEGORY.",                                              ~
         "Enter 'S' for Summary report; otherwise 'D'.                 ",~
         "Enter 'Y' to print in the Background; otherwise, 'N'.        "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, qtytype$, sortflag$,       ~
                      fmcatcode$, fmpart$, fmstore$, hicatcode$,         ~
                      hipart$, histore$, locatcode$, lopart$, lostore$,  ~
                      tocatcode$, topart$, tostore$, blankline$,         ~
                      last_ctgy_printed$, last_store_printed$,           ~
                      last_account_printed$, last_acctdescr$,            ~
                      last_storedescr$, last_ctgydescr$

            return


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
            goto inputmode1

        REM *************************************************************~
            * Print subtotals on Store, Account or Category break.      *~
            *                                                           *~
            **************************************************************
        print_store_totals
            if partsprinted% = 0% then return
            gosub print_part_totals
            gosub'200(partsprinted%)
            call "CONVERT" (total_cat_cost, 2.2, total_cat_cost$)
            call "SENDCMD"     (uw$ & "UWVBXHNYVLGUI,101," &             ~
                  last_store_printed$ & uw$)
            call "SENDCMD"     (uw$ & "UWVBXHNYVLGUI,102," &             ~
                  last_storedescr$ & uw$)
            call "SENDCMD"     (uw$ & "UWVBXHNYVLGUI,103," &             ~
                  partsprinted$ & uw$)
            call "SENDCMD"     (uw$ & "UWVBXHNYVLGUI,104," &             ~
                   total_cat_cost$ & uw$)
            gpartsprinted% = gpartsprinted% + partsprinted%
            grand_total = grand_total + total_cat_cost
            total_cat_cost, sum_qty, sum_cost = 0
            partsprinted% = 0
            return

        print_account_totals
            if partsprinted% = 0 then return
            gosub print_part_totals
            gosub'200(partsprinted%)
            call "CONVERT" (total_cat_cost, 2.2, total_cat_cost$)
            call "SENDCMD"     (uw$ & "UWVBXHNYVLGUI,201," &             ~
                  last_account_printed$ & uw$)
            call "SENDCMD"     (uw$ & "UWVBXHNYVLGUI,202," &             ~
                  last_acctdescr$ & uw$)
            call "SENDCMD"     (uw$ & "UWVBXHNYVLGUI,203," &             ~
                  partsprinted$ & uw$)
            call "SENDCMD"     (uw$ & "UWVBXHNYVLGUI,204," &             ~
                   total_cat_cost$ & uw$)
            gpartsprinted% = gpartsprinted% + partsprinted%
            grand_total = grand_total + total_cat_cost
            total_cat_cost, sum_qty, sum_cost = 0
            partsprinted% = 0
            return

        print_ctgy_totals
            if partsprinted% = 0% then return
            gosub print_part_totals
            gosub'200(partsprinted%)
            call "CONVERT" (total_cat_cost, 2.2, total_cat_cost$)
            call "SENDCMD"     (uw$ & "UWVBXHNYVLGUI,301," &             ~
                   last_ctgy_printed$ & uw$)
            call "SENDCMD"     (uw$ & "UWVBXHNYVLGUI,302," &             ~
                   last_ctgydescr$ & uw$)
            call "SENDCMD"     (uw$ & "UWVBXHNYVLGUI,303," &             ~
                   partsprinted$ & uw$)
            call "SENDCMD"     (uw$ & "UWVBXHNYVLGUI,304," &             ~
                   total_cat_cost$ & uw$)
            gpartsprinted% = gpartsprinted% + partsprinted%
            grand_total = grand_total + total_cat_cost
            total_cat_cost, sum_qty, sum_cost = 0
            partsprinted% = 0%
            return

        deffn'200(xyz%)
            convert xyz% to partsprinted$, pic (####,###,##0)
            call "STRING" addr ("LJ", partsprinted$, 12%)
            return

        REM *************************************************************~
            *      S C R E E N    # 1   I N P U T / E D I T             *~
            *-----------------------------------------------------------*~
            * Input & Edit for Report Selection Criteria                *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40850,         /* Part Number       */   ~
                                L40850,         /* Store Number      */   ~
                                L40850,         /* Part Category     */   ~
                                L40850,         /* Quantity Type     */   ~
                                L40850          /* Sort Order        */
              goto L41000

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40850:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L41000:     accept                                                       ~
               at (01,02),                                               ~
                  "Input Report Selection Criteria",                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),   columnttl$           , ch(51),~
                                                                         ~
               at (07,02), fac(hex(8c))  , prompt$(1%)          , ch(28),~
               at (07,30), fac(lfac$(1%)), fmpart$              , ch(25),~
               at (07,56), fac(lfac$(1%)), topart$              , ch(25),~
                                                                         ~
               at (08,02), fac(hex(8c))  , prompt$(2%)          , ch(28),~
               at (08,30), fac(lfac$(2%)), fmstore$             , ch(03),~
               at (08,56), fac(lfac$(2%)), tostore$             , ch(03),~
                                                                         ~
               at (09,02), fac(hex(8c))  , prompt$(3%)          , ch(28),~
               at (09,30), fac(lfac$(3%)), fmcatcode$           , ch(04),~
               at (09,56), fac(lfac$(3%)), tocatcode$           , ch(04),~
                                                                         ~
               at (10,02), fac(hex(8c))  , prompt$(4%)          , ch(28),~
               at (10,30), fac(lfac$(4%)), qtytype$             , ch(01),~
                                                                         ~
               at (11,02), fac(hex(8c))  , prompt$(5%)          , ch(28),~
               at (11,30), fac(lfac$(5%)), sortflag$            , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L42600
                  call "MANUAL" (pgm$) : goto L41000

L42600:        if keyhit% <> 15 then L42750
                  call "PRNTSCRN" : goto L41000

L42750:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L43700     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L43500
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L43500:     if fieldnr% > 1% then L43600
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L43600:     return

L43700: if fieldnr% > 0% then L44150  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L44150:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0fff00)
            return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L45100,         /* Part Number            */~
                              L45200,         /* Store Number           */~
                              L45300,         /* Part Category          */~
                              L45500,         /* Quantity Type          */~
                              L45600          /* Print Order            */
            return

L45100: REM Test Data For Part Number Range
            call "TESTRNGE" (fmpart$, topart$, lopart$, hipart$,         ~
                             errormsg$, #2)
            return

L45200: REM Test Data For Store Number Range
            call "TESTRNGE" (fmstore$, tostore$, lostore$, histore$,     ~
                             errormsg$, #4)
            return

L45300: REM Test Data For Category Code Range
            call "TESTRNGE" (fmcatcode$, tocatcode$,                     ~
                             locatcode$, hicatcode$, errormsg$, #5)
            return

L45500: REM Test Data For Quantity Type Selected
            if qtytype$ = "1" or qtytype$ = "2" or                       ~
                                 qtytype$ = "3" then return
            errormsg$ = "Quantity Type Must Be '1', '2', '3'."
            return

L45600: REM Test Data For Print Order Selection
            if sortflag$ = "1" or sortflag$ = "2"                        ~
                               or sortflag$ = "3" then return
            errormsg$ = " Print Order Must Be '1', '2', or '3'."
            return

        REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND DISPLAYS MESSAGE *~
            * (ONLY IN FOREGROUND) WHILE WE LINK BACK TO THE MENU.      *~
            *************************************************************
        exit_program
             call "FILEBGON" (#6)
             call "SHOSTAT" ("One Moment Please")
             end
