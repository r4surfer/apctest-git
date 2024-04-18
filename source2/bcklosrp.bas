        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  BBBB    CCC   K   K  L       OOO    SSS   RRRR   PPPP    *~
            *  B   B  C   C  K  K   L      O   O  S      R   R  P   P   *~
            *  BBBB   C      KKK    L      O   O   SSS   RRRR   PPPP    *~
            *  B   B  C   C  K  K   L      O   O      S  R   R  P       *~
            *  BBBB    CCC   K   K  LLLLL   OOO    SSS   R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKLOSRP - Evaluate all open sales orders and print report*~
            *            showing current lateorders by part for category*~
            *            and part range selected.  Optionally show      *~
            *            summary only and user can include or not       *~
            *            include credit hold orders.  Includes aging.   *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/28/88 ! Original                                 ! WPH *~
            * 05/02/91 ! PRR 11669 Taking Pre-Invoiced Qty into   ! SID *~
            *          !   account in determining the Late Orders !     *~
            *          !   Shipping.                              !     *~
            * 03/09/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 01/11/93 ! Page 0 Facs, Header, & End Report Time.  ! RJH *~
            * 01/18/93 ! Line counting for page overflow fixed.   ! RJH *~
            * 03/31/93 ! PRR 12834.  Report ID change to BCK009.  ! JDH *~
            * 03/31/93 ! PRR 13265 - Grand Total now accumulates  ! RJH *~
            *          !   via Open Qty not Original Qty.         !     *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            duedate$6,                   /* Current line due date      */~
            chflag$1,                    /* Cancelled/credit hold flag */~
            catcode$4,                   /* Category code              */~
            catunits$10,                 /* Units per category         */~
            catvalue$10,                 /* Value per category         */~
            catavedays$3,                /* Ave. days BO per Category  */~
            columnttl$51,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            allunits$10,                 /* Total Units in report range*/~
            allvalue$10,                 /* Total Value in report range*/~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$9,                   /* Customer code              */~
            currentcat$4,                /* Current category code      */~
            currentpart$25,              /* Current part code          */~
            currentdescrip$32,           /* Current part description   */~
            date$8,                      /* Date for screen display    */~
            descrip$32,                  /* Part Description           */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            extprice$10,                 /* Extended BO value          */~
            chold$1,                     /* Include Credit Hold flag   */~
            fmcategory$4,                /* Range of Categories        */~
            detail$1,                    /* Detail Option              */~
            fmpart$25,                   /* Range of Parts             */~
            hicategory$4,                /* Range of Categories        */~
            hipart$25,                   /* Range of Parts             */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lastcat$4,                   /* Previous Category code     */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            locategory$4,                /* Range of Categories        */~
            lopart$25,                   /* Range of Parts             */~
            openqty$10,                  /* Late order quantity        */~
            part$25,                     /* Part number                */~
            partunits$10,                /* Units per part             */~
            partvalue$10,                /* Value per part             */~
            partavedays$3,               /* Ave. BO days per part      */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            ppart$25,                    /* Part number to print       */~
            pdescrip$32,                 /* Part descrip to print      */~
            pcatcode$4,                  /* Category code to print     */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rpttitle$60,                 /* Report Title               */~
            soline$3,                    /* Sales order line number    */~
            sonumber$16,                 /* Sales order number         */~
            time$8,                      /* System Time                */~
            today$6,                     /* Todays date unformated     */~
            tocategory$4,                /* Range of Categories        */~
            topart$25,                   /* Range of Parts             */~
            totunits$10,                 /* Grand total units          */~
            totvalue$10,                 /* Grand total value          */~
            totavedays$3,                /* Grand total average days   */~
            uprice$10,                   /* Unit Price for BO goods    */~
            userid$3,                    /* Current User Id            */~
            top$10,                      /* Total of percents s/b 100  */~
            tqp$10,                      /* Total of percents s/b 100  */~
            tvp$10                       /* Total of percents s/b 100  */

        dim ab%(2,4),                    /* Aging Buckets set by user  */~
            ageo%(5),                    /* Aged Orders per period 1-5 */~
            ageo(5),                     /* Aged Orders per period 1-5 */~
            ageq(5),                     /* Aged units  per period 1-5 */~
            ageq$(5)10,                  /* Aged units  per period 1-5 */~
            agev(5),                     /* Aged value  per period 1-5 */~
            agev$(5)10,                  /* Aged value  per period 1-5 */~
            ageop(5),                    /* Aged order percents    1-5 */~
            ageop$(5)10,                 /* Aged order percents    1-5 */~
            agevp(5),                    /* Aged value percents    1-5 */~
            agevp$(5)10,                 /* Aged value percents    1-5 */~
            ageqp(5),                    /* Aged units percents    1-5 */~
            ageqp$(5)10                  /* Aged units percents    1-5 */

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
            cms2v$ = "R6.04.00 02/24/95 CMS General Release R6.04.00    "
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
            * #01 ! BCKMASTR ! BACKLOG MASTER FILE (GET STORE NUMBER)   *~
            * #02 ! BCKLINES ! BACK LOG LINE ITEM FILE                  *~
            * #03 ! WORKFILE ! Temporary System Workfile                *~
            * #04 ! HNYMASTR ! Inventory Master File                    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup     ~

            select #02, "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19                      ~

            select #03, "WORKFILE",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos = 1,    keylen = 48

            select #04, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup     ~

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))

            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            today$ = date$
            call "DATEFMT" (date$)
            call "COMPNAME" (12%, company$, ret%)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "Current Late Order Status Report"


            str(columnttl$, 1) = "From          "
            str(columnttl$,27) = "To         "

            str(line2$,62) = "BCKLOSRP: " & str(cms2v$,,8)


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  5%
L10110:         gosub'051(fieldnr%, 1%)    /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%, 1%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10210:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles EDIT MODE for range selection screen.             *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       extract_data
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% > 9% then editpg1
            if lastfieldnr% = 5% then editpg1
            if fieldnr% = 5% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            if fieldnr%  > 5% then fieldnr% = 5%

            gosub'051(fieldnr%, 2%)     /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *           E X T R A C T   R E P O R T   D A T A           *~
            *-----------------------------------------------------------*~
            * Data Extraction section for report.                       *~
            *************************************************************
        extract_data
            call "SHOSTAT" ("Searching for Late Orders")
            call "WORKOPEN" (#3, "IO", 1000%, f2%(3)) /* set back IO*/
            plowkey$ = all(hex(00))
            call "PLOWNEXT" (#1, plowkey$, 0%, f1%(1))
               if f1%(1) = 0% then empty_file /* blowout if file empty */
            goto L13070
L13060:     call "READNEXT" (#1, f1%(1))
            if f1%(1) = 0% then generate_report

L13070:       get #1  using L13080, sonumber$, chflag$
L13080:       FMT POS(10), CH(16),  POS(875), CH(1)
              if chflag$ = "C"  then L13060  /* Skip Cancelled order */
              if chflag$ = "H" and chold$ = "N" then L13060 /* Skip it */
            readkey$ = all(hex(00))
            str(readkey$, 1,16) = str(sonumber$,,)
L13110:     call "PLOWNEXT" (#2, readkey$, 16%, f1%(2))
            if f1%(2) = 0% then L13060 /* on last line, go to next order */
              get #2  using L13140, cuscode$, soline$, part$, descrip$,   ~
               catcode$,           openqty, pre_inv_qty, uprice, duedate$
L13140:       FMT CH(9), XX(16), CH(3), XX(3), CH(25), CH(32), CH(4),    ~
                               POS(109), PD(14,4), POS(133), 2*PD(14,4), ~
                     POS(206), CH(6)
*       * Check for report range limits
            if part$ > hipart$ or part$ < lopart$ then L13110
            if catcode$ > hicategory$ or catcode$ < locategory$ then L13110

*       * Determine if Pre-Invoiced Shipping occurs
            openqty = max(0, (openqty - pre_inv_qty))
            if openqty <= 0 then L13110  /* if True, Orders is not late */
*       * Accumulate totals for qualified orders
            allvalue = allvalue + (openqty * uprice)
            allunits = allunits + openqty
            allorders% = allorders% + 1%

            if openqty > 0 and duedate$ < today$ then L13180
                  goto L13110  /* not a Late Order  line item */

            dayslate%, ret% = 0%
L13180:     call "DATE" addr("G-", duedate$, today$, dayslate%, ret%)

            write #3, using L13230 , catcode$, part$, sonumber$, soline$, ~
                  cuscode$, openqty, uprice, duedate$, dayslate%, descrip$

L13230:      FMT CH(4), CH(25), CH(16), CH(3), CH(9), PD(14,4), PD(14,4),~
                    CH(6), BI(4), CH(32)

            goto L13110

        empty_file
            keyhit% = 2%
            call "ASKUSER" ( keyhit%, "**** No Sales Orders Found ****", ~
                  "The Sales Order Master File was empty or not found",  ~
                  "Press RETURN to acknowledge.  The program will end.", ~
                  " ")

            goto exit_program

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%, edit%)
            enabled% = 1%
            if edit% = 2% then return

            on fieldnr% gosub L20100,         /* Category               */~
                              L20200,         /* Range of Parts         */~
                              L20300,         /* Detail Option          */~
                              L20400,         /* Include Credit Hold?   */~
                              L20500          /* Aging Buckets          */
            return
L20100: REM Def/Enable Range of Categories         FMCATEGORY$
            if fmcategory$         = " " then                            ~
               fmcategory$         = "ALL"
            return

L20200: REM Def/Enable Range of Parts              FMPART$
            if fmpart$             = " " then                            ~
               fmpart$             = "ALL"
            return

L20300: REM Def/Enable Detail Option                 DETAIL$
               detail$           = "D"

            return

L20400: REM Def/Enable Include Credit Hold?          CHOLD$
               chold$            = "Y"

            return

L20500: REM Def/Enable Aging Buckets                 AB%()
               ab%(1,1) = 1%
               ab%(2,1) = 7%
               ab%(1,2) = ab%(2,1) + 1%
               ab%(2,2) = 14%
               ab%(1,3) = ab%(2,2) + 1%
               ab%(2,3) = 21%
               ab%(1,4) = ab%(2,3) + 1%
               ab%(2,4) = 28%

            return


        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            if fieldnr% > 5% then fieldnr% = 5%
            restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Range of Categories                                    ",~
         "Enter Range of Parts                                         ",~
         "Enter 'D' = Full Detail, 'P' = Sum by Part, 'C' = Sum by Cat ",~
         "Include Credit Hold (Y/N)?                                   ",~
         "Enter the Time Periods for Calculation of Aging              "


        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, detail$, chold$,           ~
                                fmcategory$,          fmpart$,           ~
                                hicategory$,            hipart$,         ~
                                locategory$,            lopart$,         ~
                                tocategory$,            topart$
            allorders% = 0%
            allvalue, allunits, top, tqp, tvp = 0
            ab%(1,1) = 1%
            ab%(2,1) = 7%
            ab%(1,2) = 8%
            ab%(2,2) = 14%
            ab%(1,3) = 15%
            ab%(2,3) = 21%
            ab%(1,4) = 22%
            ab%(2,4) = 28%
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
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
            call"FILEBGON" (#3)
            goto inputmode

        REM *************************************************************~
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************
        generate_report
            call "SHOSTAT" ("Generating Report")
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" ("BCK009", " ", 0%, 0%)
            printed%, a% = 0%
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */
            if lcntr% > 56% then gosub page_head
            currentpart$, currentdescrip$, currentcat$ = " "
            totorders%, partorders%, catorders%     = 0%
            totdays%, partdays%, catdays% = 0%
            totunits, partunits, catunits = 0
            totvalue, partvalue, catvalue = 0
            mat ageo%  = zer
            mat ageq   = zer : mat ageqp   = zer
            mat agev   = zer : mat agevp   = zer
            mat ageop  = zer

*       * Here Goes
            plowkey$ = all(hex(00))            /* first time only */
            call "READ102" (#3, plowkey$, f1%(3)) /* to reset key */

            goto L30500

L30480:    call "READNEXT" (#3, f1%(3))
L30500:     if f1%(3) = 0% then  print_aging

            get   #3, using L30600 , catcode$, part$, sonumber$, soline$, ~
                  cuscode$, openqty, uprice, duedate$, dayslate%, descrip$

L30600:      FMT CH(4), CH(25), CH(16), CH(3), CH(9), PD(14,4), PD(14,4),~
                    CH(6), BI(4), CH(32)
             extprice = openqty * uprice
             call "CONVERT" (openqty,  2.2 , openqty$ )
             call "CONVERT" (uprice,   2.2 , uprice$  )
             call "CONVERT" (extprice, 2.2 , extprice$)
             ppart$ = part$   /* For display of first one, later blanked*/
             pdescrip$ = descrip$
             pcatcode$ = catcode$

*       * This is where we control normal printing
            if printed% <> 0% then L30880
               lastcat$ = catcode$  /* Special case for first timer */
               goto L31080
L30880:     if lcntr% > 56% then gosub page_head
            if part$ =  currentpart$ then L31040
               gosub part_totals

            if catcode$ = currentcat$ then L31060
               gosub cat_totals
               goto L31080
L31040:     if lcntr% = 6% then L31080 /* we just refreshed them, keep em*/
            ppart$, pdescrip$  = " "
L31060:     pcatcode$  = " "
L31080:     gosub build_totals
            gosub build_aging
            gosub print_line
            goto L30480

        build_totals
*       * Grand Total, Part Totals, and Category Totals Accumulators
             totorders%  = totorders%  + 1%
             partorders% = partorders% + 1%
             catorders%  = catorders%  + 1%

             totdays%  = totdays%  + dayslate%
             partdays% = partdays% + dayslate%
             catdays%  = catdays%  + dayslate%

             totunits  = totunits  + openqty
             partunits = partunits + openqty
             catunits  = catunits  + openqty

             totvalue  = totvalue  + extprice
             partvalue = partvalue + extprice
             catvalue  = catvalue  + extprice
          return

        build_aging
*       * Aging Bucket Accumulation for orders, quantity/units, and value

            if dayslate% >  ab%(2,1) then L31740
              ageo%(1) = ageo%(1) +  1%         /* First Range of days*/
              ageq(1)  = ageq(1)  +  openqty
              agev(1)  = agev(1)  +  extprice
              return

L31740:     if dayslate% > ab%(2,2) then L31860
              ageo%(2) = ageo%(2) +  1%         /* Second Range*/
              ageq(2)  = ageq(2)  +  openqty
              agev(2)  = agev(2)  +  extprice
              return

L31860:     if dayslate% > ab%(2,3) then L31980
              ageo%(3) = ageo%(3) + 1%         /*  Third Range */
              ageq(3)  = ageq(3)  +  openqty
              agev(3)  = agev(3)  +  extprice
              return

L31980:     if dayslate% > ab%(2,4) then L32100
              ageo%(4) = ageo%(4) + 1%         /*  Fourth Range */
              ageq(4)  = ageq(4)  +  openqty
              agev(4)  = agev(4)  +  extprice
              return

L32100:       ageo%(5) = ageo%(5) + 1%         /* over Fourth Range */
              ageq(5)  = ageq(5)  +  openqty
              agev(5)  = agev(5)  +  extprice
              return

        print_aging
            if printed% = 0% then end_report
            gosub part_totals
            gosub cat_totals
            gosub grand_totals
            a% = 1%
            gosub page_head

            toto = totorders%

            for i% = 1% to 5%

             if toto  > 0 then L32482
               ageop(i%) = 0
               goto L32520
L32482:        ageo(i%) = ageo%(i%)
               ageop(i%) = (ageo(i%)/toto) * 100
L32520:        top = top + ageop(i%)

             if totunits  > 0 then L32600
               ageop(i%) = 0
               goto L32620
L32600:        ageqp(i%) = (ageq(i%)/totunits) * 100
L32620:        tqp = tqp + ageqp(i%)

             if totvalue  > 0 then L32700
               agevp(i%) = 0
               goto L32720
L32700:        agevp(i%) = (agev(i%)/totvalue) * 100
L32720:        tvp = tvp + agevp(i%)

            call "CONVERT" (ageq(i%), 2.2, ageq$(i%))
            call "CONVERT" (agev(i%), 2.2, agev$(i%))
            call "CONVERT" (ageop(i%), 2.2, ageop$(i%))
            call "CONVERT" (ageqp(i%), 2.2, ageqp$(i%))
            call "CONVERT" (agevp(i%), 2.2, agevp$(i%))
            call "CONVERT" (top, 2.2, top$)
            call "CONVERT" (tqp, 2.2, tqp$)
            call "CONVERT" (tvp, 2.2, tvp$)

            next i%
            print
            print using L60430
            print
            print using L60440
            print using L60442
            print using L60450 , ab%(1,1), ab%(2,1),  ageo%(1), ageop$(1),~
                                ageq$(1), ageqp$(1), agev$(1), agevp$(1)
            print using L60460 , ab%(1,2), ab%(2,2),  ageo%(2), ageop$(2),~
                                ageq$(2), ageqp$(2), agev$(2), agevp$(2)
            print using L60470 , ab%(1,3), ab%(2,3),  ageo%(3), ageop$(3),~
                                ageq$(3), ageqp$(3), agev$(3), agevp$(3)
            print using L60480 , ab%(1,4), ab%(2,4),  ageo%(4), ageop$(4),~
                                ageq$(4), ageqp$(4), agev$(4), agevp$(4)
            print using L60490 ,           ab%(2,4),  ageo%(5), ageop$(5),~
                                ageq$(5), ageqp$(5), agev$(5), agevp$(5)

            print using L60442
            print using L60500 , totorders%, top$, totunits$, tqp$,       ~
                                totvalue$, tvp$


        end_report                /* Report Ending Routine */
            print skip(2)
            if printed% <> 0% then L33320
            print using L60250     /* No data to report message */
            goto L33340

L33320:     time$ = " "  :  call "TIME" (time$)
            print using L64990, time$    /* End of report line */
L33340:     close printer
            call "SETPRNT" (" ", " ", 0%, 1%)
            if f2%(3) <> 0% then L33460
            call"FILEBGON" (#3)
            f2%(3) =  1%

L33460:     goto inputmode

        page_head              /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
            print page        /* Top of Form */
            print using L60070, date$, time$, company$, "BCKLOSRP"
            print using L60110, rpttitle$, pcntr%
            print
            print
            if pcntr% <> 0% then L33660
                gosub print_params   :   goto page_head
L33660:     if a% = 1% then return   /* no column head on aging page */
            if detail$ <> "D" then L33780
            print using L60150  /* Column Heading for detail option */
            print using L60170
            goto L33940

L33780:     if detail$ <> "P" then  L33880
            print using L60176  /* Column Heading for part sumry option */
            print using L60180
            goto L33940

L33880:     print using L60185  /* Column Heading for cat sumry option */
            print using L60187

L33940:     lcntr% = 6%
            pcatcode$ = catcode$  /* re-set so they show on new page */
            ppart$    = part$
            pdescrip$ = descrip$
            lastcat$  = currentcat$
           return

        print_line
            if detail$ <> "D" then L34180
            lcntr% =  lcntr% + 1%
            print using L60200, pcatcode$, ppart$, pdescrip$, sonumber$,  ~
             soline$,  cuscode$, openqty$, uprice$, extprice$, dayslate%
L34180:     printed% = 1%
            currentpart$ = part$
            currentcat$ = catcode$
            currentdescrip$ = descrip$

           return


        print_params           /* Print Page Zero */
L34350:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L34420
                str(i$(), i%, 1%) = hex(20)
                goto L34350
L34420:     print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters ~
        ~--------------------------"
            print
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "------------------------------------------------------~
        ~--------------------------"
            return

        part_totals
            call "CONVERT" (partunits, 2.2, partunits$)
            call "CONVERT" (partvalue, 2.2, partvalue$)
            pdays  = partdays%
            po = partorders%
            if po  > 0 then L34820
               partavedays = 0
               goto L34840
L34820:        partavedays = pdays /po
L34840:     call "CONVERT" (partavedays , 0.0, partavedays$)
            if detail$ <> "D" then L35020
            print using L60320
            print using L60280, currentpart$, partorders%, partunits$,    ~
                 partvalue$, partavedays$
            print
            lcntr% =  lcntr% + 3%
            goto L35180

L35020:     if detail$ <> "P" then L35180
            if catcode$ = currentcat$ then pcatcode$ = " "


            print using L60520, lastcat$,  currentpart$, partorders%,     ~
                 partunits$,  partvalue$, partavedays$
            lcntr% =  lcntr% + 1%

L35180:     partunits, partvalue, partavedays = 0
            partunits$, partvalue$, partavedays$ = " "
            partorders%, partdays% = 0%
            lastcat$ = catcode$
            return

        cat_totals
            call "CONVERT" (catunits, 2.2, catunits$)
            call "CONVERT" (catvalue, 2.2, catvalue$)
            cd = catdays%
            co = catorders%
            if co > 0 then L35460
               catavedays = 0
               goto L35480
L35460:        catavedays = cd/co
L35480:     call "CONVERT" (catavedays , 0.0, catavedays$)
            if detail$ <> "D" then L35650
            print using L60320
            print using L60340, currentcat$, catorders%, catunits$,       ~
                 catvalue$, catavedays$
            print
            print
            lcntr% =  lcntr% + 4%
            goto  L35760

L35650:     if detail$ <> "P" then L35750
            print using L60325
            print using L60540, currentcat$, catorders%, catunits$,       ~
                 catvalue$, catavedays$
            print
            print
            lcntr% =  lcntr% + 4%
            currentcat$ = catcode$
            goto L35760

L35750:     print using L60580, currentcat$, catorders%, catunits$,       ~
                 catvalue$, catavedays$
            lcntr% =  lcntr% + 1%
L35760:     catunits, catvalue, catavedays = 0
            catunits$, catvalue$, catavedays$ = " "
            catorders%, catdays% = 0%

            if lcntr% > 56% then gosub page_head
            lastcat$ = currentcat$
            return

        grand_totals
            call "CONVERT" (totunits, 2.2, totunits$)
            call "CONVERT" (totvalue, 2.2, totvalue$)
            call "CONVERT" (allunits, 2.2, allunits$)
            call "CONVERT" (allvalue, 2.2, allvalue$)
            td = totdays%
            toto = totorders%
            if toto > 0 then L36080
              totavedays = 0
              goto L36100
L36080:     totavedays = td/toto
L36100:     call "CONVERT" (totavedays , 0.0, totavedays$)
            if detail$ <> "D" then L36280

            print using L60320
            print using L60380,totorders%, totunits$, totvalue$, totavedays
            print
            print using L60361, allorders%, allunits$, allvalue$
            return

L36280:     print using L60325
            print using L60412, totorders%, totunits$, totvalue$,         ~
                               totavedays, " "
            print
            print using L60415, allorders%, allunits$, allvalue$, " "
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40090,         /* Category          */   ~
                                L40090,         /* Range of Parts    */   ~
                                L40090,         /* Detail Option     */   ~
                                L40090,         /* Include Credit hold*/  ~
                                L40095          /* Aging Buckets      */
              goto L40105

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40090:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40095:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40105:     accept                                                       ~
               at (01,02),                                               ~
                  "Current Late Order Status Report",                    ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(ac)),   columnttl$           , ch(51),~
                                                                         ~
               at (07,02), "Range of Categories",                        ~
               at (07,30), fac(lfac$( 1)), fmcategory$          , ch(04),~
               at (07,56), fac(lfac$( 1)), tocategory$          , ch(04),~
                                                                         ~
               at (08,02), "Range of Parts",                             ~
               at (08,30), fac(lfac$( 2)), fmpart$              , ch(25),~
               at (08,56), fac(lfac$( 2)), topart$              , ch(25),~
                                                                         ~
               at (09,02), "Detail Option",                              ~
               at (09,30), fac(lfac$( 3)),   detail$            , ch(01),~
                                                                         ~
               at (10,02), "Include Credit Hold?",                       ~
               at (10,30), fac(lfac$( 4)),   chold$             , ch(01),~
               at (12,02), "Aging Breakdown:",                           ~
               at (12,21), fac(hex(8c)  ),   ab%(1,1)         , pic(###),~
               at (12,25), "to",                                         ~
               at (12,29), fac(lfac$( 5)),   ab%(2,1)         , pic(###),~
               at (12,33), "days",                                       ~
               at (13,21), fac(hex(8c)  ),   ab%(1,2)         , pic(###),~
               at (13,25), "to",                                         ~
               at (13,29), fac(lfac$( 5)),   ab%(2,2)         , pic(###),~
               at (14,21), fac(hex(8c)  ),   ab%(1,3)         , pic(###),~
               at (14,25), "to",                                         ~
               at (14,29), fac(lfac$( 5)),   ab%(2,3)         , pic(###),~
               at (15,21), fac(hex(8c)  ),   ab%(1,4)         , pic(###),~
               at (15,25), "to",                                         ~
               at (15,29), fac(lfac$( 5)),   ab%(2,4)         , pic(###),~
               at (16,24), "over",                                       ~
               at (16,29), fac(hex(8c)  ),   ab%(2,4)         , pic(###),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40290
                  call "MANUAL" ("BCKLOSRP") : goto L40105

L40290:        if keyhit% <> 15 then L40305
                  call "PRNTSCRN" : goto L40105

L40305:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40400     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40385
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% > 1% then L40390
L40385:         str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40390:     return

L40400: if fieldnr% > 0% then L40445  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40445:                              /*  Edit Mode - Enabled    */
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
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50100,         /* Category               */~
                              L50200,         /* Range of Parts         */~
                              L50300,         /* Detail Option          */~
                              L50400,         /* Include Credit Hold?   */~
                              L50500          /* Aging Buckets          */

            return
L50100: REM Test for Range of Categories          FMCATEGORY$
            call "TESTRNGE"                                              ~
                  (fmcategory$         , tocategory$         ,           ~
                   locategory$         , hicategory$         ,           ~
                   errormsg$)
            return

L50200: REM Test for Range of Parts               FMPART$
            call "TESTRNGE"                                              ~
                  (fmpart$             , topart$             ,           ~
                   lopart$             , hipart$             ,           ~
                   errormsg$)
            return

L50300: REM Test for Detail Option
            if detail$ = "D" or detail$ = "P" or detail$ = "C" then return
            errormsg$ = "Enter a 'D', 'P', or 'C'"
            return

L50400: REM Test for Include Credit Hold?
            if chold$ = "Y" or chold$ = "N" then return
            errormsg$ = "Enter a 'Y' or an 'N'"
            return

L50500: REM Test for Aging Buckets
            if ab%(2,1) > ab%(2,2) then  L50576
            if ab%(2,1) > ab%(2,3) then  L50576
            if ab%(2,1) > ab%(2,4) then  L50576
            if ab%(2,2) > ab%(2,3) then  L50576
            if ab%(2,2) > ab%(2,4) then  L50576
            if ab%(2,3) > ab%(2,4) then  L50576

            ab%(1,2) = ab%(2,1) + 1%
            ab%(1,3) = ab%(2,2) + 1%
            ab%(1,4) = ab%(2,3) + 1%
            return

L50576:     errormsg$ = "Enter the day ranges in ascending order"

            return


        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ######## @ ########              ###########################~
        ~#################################                 ########:BCK009

*       * Header Line 2
L60110: %                                                   #############~
        ~###################                                   PAGE: ####

*       * Subheading for Detail
L60150: %Cat  Part Number               Description                      ~
        ~Order Number      ln Customer      BO Qty Unit Price  Ext Price A~
        ~ge
L60170: %---- ------------------------- -------------------------------- ~
        ~---------------- --- --------- ---------- ---------- ---------- -~
        ~--
*       * Subheading for Part Summary Only
L60176: %Cat          Part             Late Orders    Units       Value  ~
        ~  Ave Age

L60180: %---- ------------------------- ----------  ----------  ---------~
        ~-   ---
*       * Subheading for Category Summary Only
L60185: %                Category      Late Orders    Units       Value  ~
        ~   Ave Age
L60187: %                  ----         ----------  ----------  ---------~
        ~-   ---
*       * Line data
L60200:    %#### ######################### ##############################~
        ~## ################ ### ######### ########## ########## #########~
        ~# ###

*       * No Data to Report
L60250: % * * * No Late orders found in Sales Order Master Files * * *

*       * Part totals for detail option
L60280:    %                    * Totals for part #######################~
        ~##          (Orders: ##########)  ##########            #########~
        ~# ###
        /* yes, the part and cat totals images are dif thanks to Wang*/

*       * Totals break line for detail mode
L60320: %                                                                ~
        ~------------------------------ ---------- ---------- ---------- -~
        ~--

*       * Totals break line for summary mode
L60325: %                               ----------  ----------  ---------~
        ~-   ---

*       * Category totals for detail option
L60340: %                  * * Totals for Category ####                  ~
        ~         (Orders: ##########)  ##########            ########## #~
        ~##
*       * File total Orders, units and value
L60361: %                * * * Totals for All Orders in Range            ~
        ~         (Orders: ##########)  ##########            ##########

*       * Grand Totals for detail format
L60380: %                * * * Grand Total Late Orders in Range          ~
        ~         (Orders: ##########)  ##########            ########## #~
        ~##
        %                * * * Late Percentages                          ~
        ~         (Orders: ##########)  ##########            ##########


*       * Grand Totals for Summary formats
L60412: %     Grand total Late Orders:  ##########  ##########  #########~
        ~#   ###                                                          ~
        ~##
L60415: %     Totals for All Orders:    ##########  ##########  #########~
        ~#   ###                                                          ~

*       * Aging Buckets
L60430: %  Aging of Late orders for report range:
L60440: %                            Orders        %           units     ~
        ~   %          value          %
L60442: %                          ----------  -----------  ----------  -~
        ~---------  -----------  ----------
L60450: %    ### to ### days late  ##########  ###########  ##########  #~
        ~#########  ###########  ##########
L60460: %    ### to ### days late  ##########  ###########  ##########  #~
        ~#########  ###########  ##########
L60470: %    ### to ### days late  ##########  ###########  ##########  #~
        ~#########  ###########  ##########
L60480: %    ### to ### days late  ##########  ###########  ##########  #~
        ~#########  ###########  ##########
L60490: %      over ### days late  ##########  ###########  ##########  #~
        ~#########  ###########  ##########

L60500: %                 Totals:  ##########  ###########  ##########  #~
        ~#########  ###########  ##########

*       * Part Totals for Part Summary Option
L60520: %#### ######################### ##########  ##########  #########~
        ~#   ###

*       * Category Totals for Part and Category Summary Option
L60540: %     Total for Category ####   ##########  ##########  #########~
        ~#   ###

*       * Category Summary option line
L60580: %                  ####         ##########  ##########  #########~
        ~#   ###

        %** Report Title for page 0
        %############################################################

L64990:      %                             * * * * * * * * * *   E N D   ~
        ~O F   R E P O R T  @  ########  * * * * * * * * * *

        REM THISPROGRAMWASGENERATEDBYGENRPPGMAPROPRIETRYPRODUCTOFCAELUS**~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1988  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
