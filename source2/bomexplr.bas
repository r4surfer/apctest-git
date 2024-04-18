        REM *************************************************************~
            *                                                           *~
            *  BBBB    OOO   M   M  EEEEE  X   X  PPPP   L      RRRR    *~
            *  B   B  O   O  MM MM  E       X X   P   P  L      R   R   *~
            *  BBBB   O   O  M M M  EEEE     X    PPPP   L      RRRR    *~
            *  B   B  O   O  M   M  E       X X   P      L      R  R    *~
            *  BBBB    OOO   M   M  EEEEE  X   X  P      LLLLL  R   R   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMEXPLR - Prints or displays a parts explosion of the BOM*~
            *            for a single part, and prints explosions for a *~
            *            range of parts.                                *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/25/83 ! ORIGINAL (Cloned For BOMEXPLS)           ! HES *~
            * 03/14/84 ! CORRECT BOM ID PROCESSING, DBL PRINTING  ! ECR *~
            *          ! ALSO, SCRATCH WORKFILES, NO EXIT AFTR PRT!     *~
            * 11/25/85 ! Added Text Print Option                  ! MJB *~
            * 08/12/86 ! BOMMASTR File Format Change, ergo stuff  ! HES *~
            * 10/14/86 ! Response to current part type, error if  ! KAB *~
            *          ! explosion is indicated but not found     !     *~
            * 12/30/86 ! Modified display format                  ! MJB *~
            * 04/09/87 ! Don't explode yielded parts, fix text prt! HES *~
            * 04/22/87 ! Reformat Printed Bill of Materials       ! MJB *~
            * 05/21/87 ! HNYMASTR- rec length mod for Std costs   ! JIM *~
            * 02/22/88 ! Added UOM to report & removed 7th field  ! RJM *~
            *          ! from the PF9 screen. Now calls SETPRNT.  !     *~
            * 04/06/88 ! CORRECTED SEVERAL PROBLEMS, RANGE SELECT ! BPN *~
            *          ! PF14-PRINT, STARTOVER LIGIC, EDIT OF     ! BPN *~
            *          ! REFERENCE PRINT OPTION.                  ! BPN *~
            * 06/14/88 ! REWORK - MOVED TIME SET FOR REPORT SO IT ! BPN *~
            *          ! CHANGES FOR EACH REPORT, CHANGED ASKUSER ! BPN *~
            *          ! MESSAGE.                                 ! BPN *~
            * 06/22/88 ! Mod to show/print " " UOM for 'RE' parts ! MJB *~
            * 02/17/89 ! Added PLOWCODE report for BOM Master List! MJB *~
            * 04/27/89 ! Added access to BOMBPDSP on assembly and ! MLJ *~
            *          ! component display screens.               !     *~
            * 06/20/89 ! Added option to print using DOT format,  ! MLJ *~
            *          !  fixed reporting of by-product quanties  !     *~
            * 08/03/90 ! Added option to print part text entered  ! MJB *~
            *          !  thru HNYINPUT.                          !     *~
            *          ! Added Batch Quantity to total quantity   !     *~
            *          !  calculation for display and print.      !     *~
            * 04/26/91 !(PRR 11851) Added flag for when a BOM is  ! RJB *~
            *          !     printed to replace using the page    !     *~
            *          !     counter for error reporting.  Also   !     *~
            *          !     added call to ALLFREE (new standard) !     *~
            * 06/21/91 !QC-FIXES Removed 'ALLFREE'.               !     *~
            * 03/25/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 10/08/92 ! PRR11741 Passed in Batch Quantity of '0' ! SID *~
            *          !  when calling 'BOMBPDSP'.                !     *~
            * 01/06/93 ! PRR 12286,12630,12731  Ref Locs prt again! JDH *~
            * 08/17/94 ! PRR 13176,13272 SubCompnent now MAY use  ! RJH *~
            *          !  parent qty in calc of Total Qty Required!     *~
            *          !   Also Display correctly uses Fixed Qty. !     *~
            *          !   Also Print Header shows Batch Qty used.!     *~
            *          ! PRR 11751 - Use chanel # for Test Range  !     *~
            *          !  to allow getcode list for Cat & Hny Rng !     *~
            * 08/31/95 ! PRR 13473. Fixed enable of last two      ! JDH *~
            *          !  fields on range printing.               !     *~
            *************************************************************

        dim                                                              ~
            assypart$25,                 /* Assembly part number       */~
            assypartkey$28,              /* Assembly part number - BOM */~
            assypartdescr$40,            /* Assembly part description  */~
            assystring$100,              /* For report header          */~
            astk$3,                      /* (Asterisk)                 */~
            batch_flag$1,                /* Use batch qty or single(1) */~
            bom$(490)3,                  /* BOM LIST                   */~
            bomid$3,                     /* Which alt BOM?             */~
            bybomid$(10)3,               /* BOM ARRAY - BY/CO PRODUCTS */~
            bypart$(10)25,               /* PART ARRAY - BY/CO PRODUCTS*/~
            bommkr$2,                    /* BOM MARKER                 */~
            calc$(6)8,                   /* Calc fields                */~
            cat$(4)4,                    /* Category Range             */~
            company$60,                  /* Company name for title     */~
            compbom$3,                                                   ~
            compbom$(20)3,                                               ~
            component$25,                /* Component part number      */~
            componentkey$28,             /* Component part number - BOM*/~
            count%(15),                  /* Item count of 15 levels    */~
            cursor%(2),                  /* Cursor location for display*/~
            cx$65,                       /* Temp comp and marker       */~
            cx2$40,                      /* 2nd line space             */~
            date$8,                      /* Calendar date for display  */~
            descr_map(16),               /* Plowcode argument          */~
            dotfmt$1,                    /* DOT Format Flag            */~
            dots$30,                     /* Dots for report            */~
            effdatef$8,                  /* Effectivity date formatted */~
            effdate$8,                   /* Effectivity date           */~
            errormsg$79,                 /* Error message text info    */~
            fmtsw$1,                     /* Ref print format flag      */~
            hdr$(3)133,                  /* PLOWCODE Screen Titles     */~
            i$(24)80,                    /* Screen image (not used)    */~
            incl(1),                     /* PLOWCODE Argument          */~
            incl$(1)1,                   /* PLOWCODE Argument          */~
            header$79,                   /* Screen Title               */~
            infomsg$79,                  /* Informative message text   */~
            inpmessage$79,               /* Input message text info    */~
            lfac$(20)1,                  /* Field attribute characters */~
            line1$(20)79,                /* Display for where used     */~
            line$(20)79,                 /* Text to display on screen  */~
            location$6,                  /* Reference location code    */~
            locqty$9,                    /* Location Quantity          */~
            maxlevels$2,                 /* Max # of levels to show    */~
            mkr$(20)2,                   /* Allowed bommarkers         */~
            mkrdes$(20)10,               /* Allowed bommarkers         */~
            p$(15)31,                    /* Read keys for 15 levels    */~
            p_qty(15),                   /* Assy part total quantity   */~
            part$25,                     /* Dummy argument part #      */~
            partr$(4)25,                 /* Part Number Range          */~
            partkey$28,                  /* Dummy argument part # - BOM*/~
            pfkeys$32,                   /* Function keys enabled lists*/~
            pfktext$(3)79,               /* Function keys enabled lists*/~
            plowkey$30,                  /* Key for PLOWCODE Report    */~
            print$(10)80,                /* Arrays to print info with  */~
            prtref$1,                    /* REFE Print Flag            */~
            prttxt$1,                    /* Text Print Flag (BOM text) */~
            prttxt2$1,                   /* Text Print Flag (part text)*/~
            primbom$3,                   /* Which alt BOM?             */~
            qty_method_flag$1,           /* Explosion Type, 'Y'=Single */~
                                         /*  Parent, 'N'=Parent Qty    */~
            readkey$50,                  /* Key for plow routines      */~
            readkey2$50,                 /* Key for plow routines      */~
            ref_key$62,                  /* Saves key for refer plow   */~
            ref_key$(06)62,              /* Saves key for refer display*/~
            ref_title$(5)16,             /* for horiz ref printing     */~
            ref_dash$(5)16,              /* for horiz ref printing     */~
            ref_detail$(5)16,            /* for horiz ref printing     */~
            rteid$3,                     /* Peg to routing             */~
            rptdescr$35,                 /* Heading for PLOW Report    */~
            savebom$3,                   /* 1st SCRN BOM - BOMBPDSP    */~
            tempdescr$34,                /* Temporary part description */~
            textidl$4,                   /* Line Item Text ID          */~
            textidp$4,                   /* Part Master file Text ID   */~
            textidh$4,                   /* Header    Text ID          */~
            time$8,                      /* Clock Time for display     */~
            tttle$79,                    /* Title for display entries  */~
            title$(2)30,                 /* Screen Column Headings     */~
            titledescr$79,               /* Show what part we're doing */~
            title1$79,                   /* Title for where used scrn  */~
            titledescr1$79,              /* Part for where used screen */~
            type$3,                      /* Part type from HNYMASTR    */~
            usrid$3,                     /* USERS LOGON ID             */~
            uom$4,                       /* Part unit of measure       */~
            x$1                          /* Batch-controlled part code */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            byqty(10)                    /* QTY ARRAY - BY/CO PRODUCTS */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
        REM *************************************************************
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
            * # 3 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 4 ! HNYMASTR ! INVENTORY MASTER FILE (NAME, DESCRIPTION)*~
            * # 5 ! BOMMASTR ! BILL OF MATERIALS RELATIONSHIP FILE      *~
            * # 6 ! BOMREFER ! LOCATION REFERENCE FILE                  *~
            * # 7 ! CATEGORY ! Inventory category codes file            *~
            * #11 ! ENGMASTR ! ENGINEERING MASTER FILE                  *~
            * #12 ! TXTFILE  ! SYSTEM TEXT FILE                         *~
            *************************************************************

            select  #3, "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20

            select  #4, "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90,  keylen = 9, dup

            select # 5, "BOMMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos =  26, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 56

           select # 6, "BOMREFER" ,                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  100,                                  ~
                        keypos = 26, keylen = 34,                        ~
                        alt key 1, keypos = 1, keylen = 59

           select #07, "CATEGORY",                                       ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   4

           select #11, "ENGMASTR" ,                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos = 1, keylen = 29

            select #12, "TXTFILE",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos =  1, keylen =  11

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 3, 0%, f2%( 3), 0%, " ")
            call "OPENCHCK" (# 4, 0%, f2%( 4), 0%, " ")
            call "OPENCHCK" (# 5, 0%, f2%( 5), 0%, " ")
            call "OPENCHCK" (# 6, 0%, f2%( 6), 0%, " ")
            call "OPENCHCK" (# 7, 0%, f2%(7%), 0%, " ")
            call "OPENCHCK" (#11, 0%, f2%(11), 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes all the variables needed to do the report.    *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            err% = 0%
            call "COMPNAME" (12%, company$, err%)

            call "EXTRACT" addr ("ID", usrid$)

        REM Load Up BOM Markers Table...
            readkey$ = "TABLE01:"
L09130:     call "PLOWNEXT" (#3, readkey$, 8%, f1%(3))
                if f1%(3) = 0 then L09230
            u3% = u3% + 1
            get #3, using L09170, mkr$(u3%), mkrdes$(u3%)
L09170:     FMT XX(8), CH(2), XX(40), CH(10)
            if u3% < 20 then L09130

L09230:     title1$ = "   QUANTITY                 LOCATION"
            title$(1) = "From"  :  title$(2) = "To  "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Input mode main program.                                  *~
            *************************************************************

        inputmode
            init(" ") errormsg$,inpmessage$,assypart$,maxlevels$,fmtsw$, ~
                      effdatef$,effdate$,bomid$,primbom$,prttxt$,prtref$,~
                      time$,dotfmt$,prttxt2$, batch_flag$,qty_method_flag$
            dots$ = ". . . . . . . . . . . . . . . "
            bomprinted%, page%, p% = 0%
            keyhit1% = 2%
            select ws

L10130:     for fieldnr% = 1% to 11%
                gosub'161(fieldnr%)
                      if enabled% = 0 then L10290
L10160:         gosub'201(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then L10240
L10190:                  fieldnr% = max(1, fieldnr% - 1%)
                         if fieldnr% = 1% then L10130
                         gosub'161(fieldnr%)
                         if enabled% <> 0 then L10160
                         goto L10190
L10240:               if keyhit%  =  9 then L11000    /* Range Print */
                      if keyhit%  = 14 then master_list
                      if keyhit%  = 16 and fieldnr% = 1 then L65000
                      if keyhit% <>  0 then       L10160
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10160
L10290:         next fieldnr%


        REM *************************************************************~
            *      A L L O W   E D I T   O F   S E L E C T I O N        *~
            *-----------------------------------------------------------*~
            *   also allows option to display or print the Explosion    *~
            *************************************************************

        editmode
            fieldnr% = 0%
            inpmessage$ = edtmessage$

            gosub'201(fieldnr%)
                if keyhit% = 1% then  gosub startover
                if keyhit% = 10% then gosub see_bycoprod
                if keyhit% = 14% then goto display_bom     /* DISPLAY  */
                if keyhit% = 30% then goto print_report
                if keyhit% = 16% and fieldnr% = 0% then L65000
                if keyhit% <> 0% then goto editmode

            fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 11% then goto editmode

            gosub'161(fieldnr%)
                if enabled% = 0% then goto editmode
L10710:     gosub'201(fieldnr%)
                if keyhit% = 1% then  gosub startover
                if keyhit% <> 0% then      L10710
            gosub'151(fieldnr%)
                if errormsg$ <> " " then goto L10710

            goto editmode

        REM PRINT BOM EXPLOSION FOR A SINGLE PART

        print_report

L10840:     infomsg$ = "Printing Explosion For" & hex(84) & assypart$

            call "SHOSTAT" (infomsg$)
            select printer (134)
            call "SETPRNT" ("BOM002", " ", 0%, 0%)
            gosub'200(assypart$)
            if bomprinted% > 0% then L10900
                keyhit1% = 2
                call "ASKUSER" (keyhit1%, "NO MATCHING RECORDS",         ~
                     "SORRY, No Parts eligible for printing were found", ~
                     "using the requested selection criteria.",          ~
                     "Press RETURN to change the parameters or to EXIT.")
                goto editmode
L10900:     print skip(1)
            print using L60450
            close printer
            call "SETPRNT" ("BOM002", " ", 0%, 1%)
            goto inputmode


L11000: REM *************************************************************~
            *           I N P U T   P R I N T   R A N G E               *~
            *-----------------------------------------------------------*~
            * Also gets the maximum number of levels to go for.         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, partr$(), cat$(), primbom$

L11080:         for fieldnr% = 1% to 11%
                    gosub'162(fieldnr%)
L11100:                   if enabled% = 0  then L11230
L11110:             gosub'202(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then L11190
L11140:                  fieldnr% = max(1, fieldnr% - 1%)
                         if fieldnr% = 1% then L11080
                         gosub'162(fieldnr%)
                         if enabled% <> 0 then L11100
                         goto L11140
L11190:               if keyhit%  = 16 then       inputmode
                      if keyhit% <>  0 then       L11110
                    gosub'152(fieldnr%)
                          if errormsg$ <> " " then L11110
L11230:             next fieldnr%

        REM *************************************************************~
            * A L L O W   E D I T   O F   S E L E C T I O N   R A N G E *~
            *************************************************************

        editmode2
            fieldnr% = 0%
            inpmessage$ = edtmessage$

            gosub'202(fieldnr%)
                if keyhit% = 1% then  gosub startover
                if keyhit% = 16% then      L13000       /* PRINT REPORT */
                if keyhit% <> 0% then      editmode2

            fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% > 11% then      editmode2

            gosub'162(fieldnr%)
                if enabled% = 0% then      editmode2
L11710:     gosub'202(fieldnr%)
                if keyhit% = 1% then gosub startover
                if keyhit% <> 0% then      L11710
            gosub'152(fieldnr%)
                if errormsg$ <> " " then   L11710

            goto editmode2

L12000: REM *************************************************************~
            *          D I S P L A Y   P L O W   R O U T I N E          *~
            *-----------------------------------------------------------*~
            * Plows through the bill of materials for the assembly part *~
            * number and prints                                         *~
            *************************************************************
        display_bom

            print at(4,1,80); hex(84); "Exploding Assembly..."
            l%, displayline%, by% = 0
            line$(), ref_key$(), compbom$(), bypart$(), bybomid$() = " "
        REM Set title description for screen showing.
            gosub'190(assypart$)
            assypartkey$ = str(assypart$,,25) & str(bomid$,,3)
            tttle$, titledescr$ = " "
                str(titledescr$,1) = "Assembly No. " & assypart$ & " "   ~
                                      & assypartdescr$

                str(tttle$, 1, 7) = "BOM:" & bomid$
                str(tttle$, 10, 11) = "UOM is " & uom$
                str(tttle$,23%) = "          Parent Qty*"         &      ~
                                  "(Qty*Size+Over)+Fix="          &      ~
                    hex(06) &  " Total Qty" & hex(06) & " UOM"
            if batch_flag$ = "N" then topbatch = batchqty                ~
                                 else topbatch = 1

            gosub'7(assypartkey$,topbatch)  /* P$ = part number to BOM */
            if displayline% <> 0 then gosub L12560  /* display partial  */
            goto inputmode               /* get next part to BOM       */

            deffn'7(partkey$, p_qty)
                  l% = l% + 1
                  p$(l%) = partkey$
                  p_qty(l%) = p_qty
                  count%(l%) = 0         /* Sequence number for print  */

L12180:           call "PLOWNEXT" (#5, p$(l%), 28%, f1%(5))
                       if f1%(5) = 0 then L12255
                  if str(p$(l%),29,3) <> "  0" then L12205
                     gosub L31000         /* Load BOM header info */
                     goto L12180
L12205:           gosub L30000            /* Load BOM record & info.    */
                  gosub'190(component$)  /* Get BOM for component     */
                  componentkey$ = str(component$,,25) & str(bomid$,,3)
                  count%(l%) = count%(l%) + 1
                  gosub L12275            /* Process print entry        */
                  if quantity * xused < 0 then L12180
                  if l% < maxlevels% then gosub'7(componentkey$, totalqty)
                                         /* Do comps if not at bottom  */
                  goto L12180

L12255:       REM End routine gracefully.
                      l% = l% - 1
                      return

L12275: REM Fill screen entries, and display when full.
*        First line
            displayline% = displayline% + 1
            by% = by% + 1
            if displayline% > 18 then L12560    /* SHOW SCREEN.     */
            call "DESCRIBE"(#4, component$, tempdescr$, 1%, f1%(4))
                bypart$(by%) = component$
            get #4 using L12305, uom$
L12305:         FMT POS(74), CH(4)
            tran(str(tempdescr$,3),"aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRs~
        ~StTuUvVwWxXyYzZ")replacing
            cx$ = component$ & " " & tempdescr$
            put str(line$(displayline%), 2 * l% - 1, 70),                ~
                using L12540, count%(l%), cx$
            if str(line$(displayline%),64,1) = " "                       ~
               then  str(line$(displayline%),64,1) = hex(06)
            if str(line$(displayline%),75,1) = " "                       ~
               then  str(line$(displayline%),75,1) = hex(06)
*        Second line
               displayline% = displayline% + 1
               cx2$ = " "
               if op$ = "Y" then cx2$ = "Option Part "
               search mkr$() = bommkr$ to cursor%() step 2
               if cursor%(1) = 0 then L12405
               if cx2$ = " " then cx2$ = mkrdes$((cursor%(1)+1)/2)       ~
                  else cx2$ = cx2$ & ", " & mkrdes$((cursor%(1)+1)/2)
               tran(str(cx2$,2),"aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStT~
        ~uUvVwWxXyYzZ")replacing
L12405:        if bomid$ = hex(000000) then cx2$=cx2$ & ", *No Eff. BOM*"
               if bomid$ = hex(000000) then L12420
               if bomid$ <> " " then cx2$ = cx2$ & ", BOM:" & bomid$
L12420:        ref_key$(by%) = ref_key$
               call "PLOWNEXT" (#6, str(ref_key$,26%), 31%, f1%(6))
               if f1%(6) <> 0 then astk$ = "(*)" else astk$ = " "
               compbom$(displayline%), compbom$(displayline%-1) = bomid$
                     bybomid$(by%) = bomid$
               put str(line$(displayline%), 2*l%-1), using L12550, astk$, ~
                                                     cx2$
               str(line$(displayline%),64,1) = hex(06)
               str(line$(displayline%),75,1) = hex(06)
*        Third line
               displayline% = displayline% + 1
               call "CONVERT" (quantity, -0.4, calc$(1))
               call "CONVERT" (xused, -0.4, calc$(2))
               call "CONVERT" (over, -0.4, calc$(3))
*             CALL "CONVERT" (TOPBATCH, -0.4, CALC$(4))
               call "CONVERT" (fixed, -0.4, calc$(6%))
               call "CONVERT" (p_qty(l%), -0.4, calc$(5%))
               str(line$(displayline%),1%) =                   calc$(5%) ~
                   & "("  & calc$(1%) & " x " & calc$(2%) & " + "        ~
                   & calc$(3%) & ") + " & calc$(6%) &  "="

               call "STRING" addr("RJ", line$(displayline%), 63%)
                     byqty(by%) = (quantity*xused+over)
               totalqty =            p_qty(l%) * (quantity*xused+over)   ~
                                                                + fixed
               call"CONVERT"(totalqty,1.4,str(line$(displayline%),65,10))

               str(line$(displayline%),64,1) = hex(06)
               str(line$(displayline%),75,1) = hex(06)
               if bommkr$ <> "RE" then                                   ~
                   str(line$(displayline%),76,4) = uom$
               return

L12540: %###. ###########################################################~
        ~######
L12550: % ###  ########################################

L12560:     REM DISPLAY CONTROLLER FOR FULL SCREEN.  HANDLES P.F. KEYS.
                keyhit1% = 1%
L12570:         gosub L41000
                      if keyhit%  =  2 then L12000
                      if keyhit%  =  5 then L12690
                      if keyhit%  = 10 then gosub see_compbyco
                      if keyhit%  = 14 then print_report
                if keyhit%  = 16 then editmode
                if keyhit%  =  8 then L12660  /*Habit Formed In BOMINPUT*/
                if keyhit% <>  0 then L12570
L12660:               fieldnr% = max(0, cursor%(1) - 4)
                      if fieldnr% < 1 then L12570
                      if fieldnr% > 18 then L12570
                      if line$(fieldnr%) = " " then L12570
                      goto L12715

L12690:         if l% = 0% then L12570
                displayline% , by% = 0
                init(" ") line$(), ref_key$(), bypart$(), bybomid$()
                goto L12275               /* WHEN SIGNALS FOR NEXT.     */

L12715: REM ************* CONTROLLER FOR REFERENCE SCREEN ***************
               ref_lin% = fieldnr%
               fieldnr% = int((fieldnr% - 1%) / 3%) + 1%
               ref_key$ = ref_key$(fieldnr%)
               init (" ") title1$, titledescr1$
               str(titledescr1$, 6) = "REFERENCE LIST FOR:  " &          ~
                                      str(ref_key$,, 25)
               x% = max( pos(line$(ref_lin%) = "(" ),1)
               if x%>57% then L12775
               titledescr1$ = titledescr1$ & " " &                       ~
                   str(line$(ref_lin%), x%, 58%-x%)

L12775:         str(title1$,10)="In Assembly: " & str(ref_key$,26,25)
                title1$= title1$ & "   BOM Id: " & str(ref_key$,51,3)
L12785:        str(ref_key$,57) = " "
               rline% = 0
L12795:        gosub locations_for_screen
               if line1$() = " " then L12570
L12805:             gosub L42000
                    keyhit1% = 1%
                    if keyhit%  = 16% then L12570
                    if keyhit%  =  0% then L12570 /* Just to be nice */
                    if keyhit%  = 14% then L10840
                    if keyhit%  =  2% then L12785
                    if keyhit%  =  5% then L12795
                    goto L12805


        locations_for_screen     /* READ ASSOCIATED LOCATION(S) IF ANY */
                line1$() = " "
                count%, u3% = 0
                horiz% = 1
L12870:         if count% = 60% then return
                call "PLOWNEXT" (#6, str(ref_key$,26%), 31%, f1%(6))
                       if f1%(6) = 0 then return
                gosub L35070                /* 'GET' IT  */
                count% = count% + 1
                if count% = 21 then u3% = 20
                if count% = 21 then horiz% = 27
                if count% = 41 then u3% = 40
                if count% = 41 then horiz% = 53
                rline% = rline% + 1
                call "CONVERT" (quantity_here, 0.2, str(tt$,,5))
                put str(line1$(count%-u3%),horiz%), using L12935, rline%, ~
                                                           tt$, location$
L12935:         %####)##### at ######
                goto L12870

L13000: REM *************************************************************~
            *                 P R I N T   R E P O R T                   *~
            *                                                           *~
            * Prints Requested Data...                                  *~
            *************************************************************

            readkey$ = partr$(3)
            select printer (134)
            call "SETPRNT" ("BOM002", " ", 0%, 0%)

L13100:     REM LOAD BILL OF MATERIALS RECORD.
L13110:         call "PLOWNEXT" (#5, readkey$, 0%, f1%(5))
                     if f1%(5) = 0 then L13280      /* ALL DONE */
                str(readkey$,26,3) = all(hex(ff))
                if str(readkey$,,25) > partr$(4) then L13280
            call "SHOSTAT"  ("Printing Bills of Materials               "~
                             & "For " & str(readkey$,,25))
                REM LOAD INVENTORY MASTER RECORD
                call "READ100" (#4, str(readkey$,,25), f1%(4))
                     if f1%(4) = 0 then L13100
                get #4, using L13210, cat$
L13210:         FMT POS(90), CH(4)

                if cat$ < cat$(3) or cat$ > cat$(4) then L13100
                assypart$ = str(readkey$,,25)
                gosub'200(assypart$)
                goto L13110

L13280: REM Now close the printer and start again
            if bomprinted% > 0% then L13360
            keyhit1% = 2
            call "ASKUSER" (keyhit1%, "NO MATCHING RECORDS",             ~
                "SORRY, No Parts eligible for printing were found",      ~
                "using the requested selection criteria.",               ~
                "Press RETURN to change the parameters or to EXIT.")
                goto editmode2
L13360:     if line% > 54% then gosub page_head
            print skip(1)
            print using L60450
            close printer
            call "SETPRNT" ("BOM002", " ", 0%, 1%)
            goto inputmode


        REM *************************************************************~
            *       P R I N T   B O M   M A S T E R   L I S T I N G     *~
            *-----------------------------------------------------------*~
            * Prints Listing of BOM Header Information                  *~
            *************************************************************
        master_list
            plowkey$ = " "
            rptdescr$ = hex(06) & "Bills of Materials Master List"
            incl(1) = 0  :  incl$(1) = " "
            hdr$(2) = "Assembly Number           BOMID  RTEID  BOM D" &  ~
                      "escription                 Apprvd On - By    " &  ~
                      "           Phantom      Batch Qty        "

                  /*   xxxxxxxxxxxxxxxxxxxxxxxxx   xxx    xxx  xxxxx   */
                  /*   xxxxxxxxxxxxxxxxxxxxxxxxx   xx/xx/xx   xxxxxx   */
                  /*   xxxxxxxxx     x         999999.99               */

            descr_map( 1) =  26.25  :  descr_map( 2) =   1.00
            descr_map( 3) =  51.03  :  descr_map( 4) =  28.00
            descr_map( 5) =  87.03  :  descr_map( 6) =  35.00
            descr_map( 7) =  57.30  :  descr_map( 8) =  41.00
            descr_map( 9) = 115.061 :  descr_map(10) =  74.00
            descr_map(11) = 121.15  :  descr_map(12) =  85.00
            descr_map(13) = 106.01  :  descr_map(14) = 105.00
            descr_map(15) = 107.08  :  descr_map(16) = 114.1042

            call "PLOWCODE"(#5, plowkey$, rptdescr$, -9028%, -0.3,       ~
                            f1%(5), hdr$(), 0,  0, incl(), incl$(),      ~
                            "r", " ", #3, descr_map() )

            goto inputmode

        REM *************************************************************~
            *  D I S P L A Y   A S S E M B L Y   B Y - P R O D U C T S  *~
            *************************************************************

            see_bycoprod
                 if primebom$ = " " then gosub'190(assypart$)
                 if primebom$ = " " then savebom$ = bomid$ else          ~
                       savebom$ = primebom$
                 call "BOMBPDSP" (assypart$, savebom$, 0)
                 return

        REM *************************************************************~
            * D I S P L A Y   C O M P O N E N T  B Y - P R O D U C T S  *~
            *************************************************************

            see_compbyco
                 if cursor%(1) > 4% then L16050
                 call "BOMBPDSP" (assypart$, savebom$, 1)
                 return
L16050:          if cursor%(1) > 4%  and cursor%(1) < 8%  then by% = 1%
                 if cursor%(1) > 7%  and cursor%(1) < 11% then by% = 2%
                 if cursor%(1) > 10% and cursor%(1) < 14% then by% = 3%
                 if cursor%(1) > 13% and cursor%(1) < 17% then by% = 4%
                 if cursor%(1) > 16% and cursor%(1) < 20% then by% = 5%
                 if cursor%(1) > 19% and cursor%(1) < 23% then by% = 6%
                 call "BOMBPDSP" (bypart$(by%), bybomid$(by%), byqty(by%))
                 return

        REM *************************************************************~
            *      G E T   E F F E C T I V E   B O M                    *~
            *                                                           *~
            * GETS THE CURRENT BOM FOR PASSED PART BASED ON DATEINDEX%  *~
            *************************************************************

            deffn'190(part$)
                 bomid$, rteid$, textidh$ = " "
                 call "READ100" (#4, part$, f1%(4))
                     if f1%(4) = 0% then return  /* NON - STOCKED      */
                 get #4, using L18110, uom$, textidp$, type$
L18110:              FMT POS(74), CH(4), POS(98), CH(4), POS(180), CH(3)
                 convert type$ to type%, data goto L18140
                    goto L18160
L18140:                bomid$ = hex(000000)           /* SET FOR ERROR */
                       return
L18160:          if type% > 0% and type% < 500% then return
                                           /* NON-PLANNED OR PURCHASED */
                 if part$ = assypart$ then L18200
                 if type% > 789% and type% < 800% then return /* TOOL  */
L18200:          bomid$ = hex(000000)                 /* SET FOR ERROR */
                 if part$<>assypart$ then L18250
                 if primbom$ =  " " then L18280
                     bomid$ = primbom$
                     goto L18390
L18250:          if compbom$ =  " " then L18280
                     bomid$ = compbom$
                     goto L18390
L18280:          if dateindex% = 0 then return   /* OFF PROD CALENDER? */
                    readkey2$ = str(part$,,25) & "1"
                    call "READ102" (#11, readkey2$, f1%(11))
                       if f1%(11) <> 1 then return
                    get #11, using L18330, readkey2$, bom$()
L18330:                FMT CH(29), 490 * CH(3)
                    if str(readkey2$,,25) <> str(part$,,25) then return
                    bomid$ = bom$(dateindex%)
                    if bomid$ = " " then L18140

L18390:    call "READ100" (#5,str(part$,,25)&str(bomid$,,3)&"  0",f1%(5))
               if f1%(5) <> 0 then gosub L31000 /* Load Bom Header Info */
           return

        REM *************************************************************~
            * P R I N T   P A R T S   E X P L O S I O N   F O R   O N E *~
            *                           P A R T                         *~
            *                                                           *~
            * PRINTS THE PARTS EXPLOSION FOR ONE PART NUMBER.  THIS     *~
            * ROUTINE GETS CALLED FROM THE DISPLAY CONTROLLER FOR ONE - *~
            * PART AND ALSO FROM THE RESULTS OF THE SORT ROUTINE.       *~
            *************************************************************

        deffn'200(assypart$)
            l%, p%, page% = 0%
            line% = 1000%
            time$ = " " : call "TIME" (time$)
            call "DESCRIBE" (#4, assypart$, assypartdescr$, 1%, f1%(4))
            gosub'190(assypart$)
            assypartkey$ = str(assypart$, 1, 25) & str(bomid$, 1, 3)
            if primbom$ =  " " then str(tttle$, 6, 30) = " BOM " &       ~
                 bomid$ & " EFFECTIVE ON " & effdatef$  else             ~
                 str(tttle$, 6, 30) = " BOM " & bomid$ & " AS SPECIFIED"
            if batch_flag$ = "N" then  topbatch = batchqty               ~
                                 else  topbatch = 1
            call "CONVERT" (topbatch, -2.2, topbatch$)
            assystring$ = assypart$ & " " & assypartdescr$               ~
                          & " USING " & str(tttle$,6,30)
            if prttxt$ = "Y" and p% = 0 then gosub header_text

            gosub'8(assypartkey$, topbatch) /* P$ = PART NUMBER TO BOM */

            return                       /* GET NEXT PART TO BOM       */

            deffn'8(partkey$, p_qty)
                  l% = l% + 1
                  p$(l%) = partkey$
                  p_qty(l%) = p_qty
                  count%(l%) = 0         /* SEQUENCE NUMBER FOR PRINT  */

L19128:           call "PLOWNEXT" (#5, p$(l%), 28%, f1%(5))
                       if f1%(5) = 0 then L19180
                  if str(p$(l%),29,3) = "  0" then L19128
                  gosub L30000            /* LOAD BOM RECORD & INFO.    */
                  gosub'190(component$)
                  componentkey$ = str(component$,1,25) & str(bomid$,1,3)
                  count%(l%) = count%(l%) + 1
                  gosub L19196            /* PROCESS PRINT ENTRY        */
                  if quantity * xused < 0 then L19128
                  if qty_method_flag$ = "Y" then ttlqty = 1.0
                  if l% < maxlevels% then gosub'8(componentkey$, ttlqty)
                                         /* DO COMPS IF NOT AT BOTTOM  */
                  goto L19128

L19180:           REM END ROUTINE GRACEFULLY.
                      l% = l% - 1
                      return

L19196:     REM ROUTINE TO PRINT ENTRY JUST LOADED.
                init(" ") print$()
                if dotfmt$ = "N" then L19224
                   d% = l% + l%
                   str(print$(1), 1, d%) = str(dots$)
                   str(print$(1),34, 25) = component$
                   goto L19240
L19224:         put str(print$(1), 3 * l% - 2, 30),                      ~
                        using L19232, count%(l%), component$
L19232:                       %###. #########################

L19240:         ttlqty =            p_qty(l%) * (quantity * xused + over)~
                                                                  + fixed
                call"CONVERT"(ttlqty, 4.4, str(print$(3),,10))
                call"CONVERT"(fixed, 4.4, str(print$(4),,10))
                if str(componentkey$, 26, 3) <> hex(000000) then L19264
                   print$(5) = "*NO BOM*"
                   goto L19284
L19264:         if str(componentkey$, 26, 3) <> hex(202020) then L19276
                   print$(5) = "  "
                   goto L19284
L19276:         str(print$(5),1,3) = str(componentkey$, 26, 3)
                str(print$(5),6,3) = rteid$
L19284:         print$(7) = step$
                if op$ = "Y" then print$(8) = "Y"
                if bommkr$ = "ST" then L19308
                search mkr$() = bommkr$ to cursor%() step 2
                     if cursor%(1) = 0 then print$(9) = " "  else        ~
                                    print$(9) = mkrdes$((cursor%(1)+1)/2)
L19308:         call "DESCRIBE"(#4, component$, tempdescr$, 0%, f1%(4))
                get #4 using L19316, uom$
L19316:              FMT POS(74), CH(4)
                if bommkr$ = "RE" then uom$ = " "
                if line% > 54% then gosub page_head

                print using L60210, print$(1), print$(3), uom$, print$(4),~
                                   print$(5), print$(7), print$(8), x$,  ~
                                   print$(9)
                line% = line% + 1%
                bomprinted% = 1%
                print$(1) = " "
                if dotfmt$ = "N" then L19364
                   str(print$(1),37,32) = tempdescr$
                   goto L19372
L19364:         str(print$(1), 3*l%+3, 32) = tempdescr$

L19372:         if prtref$ = "Y" then gosub locations
                if locsw% =  0% then print using L60180, print$(1)        ~
                                else print skip(1)
                line% = line% + 1%
                if prttxt$ = "Y" then gosub comp_text
                if prttxt2$ = "Y" then gosub hny_text
                return

        comp_text  /* Print any component text that was entered... */
            dis% = 3%*l%+4%
            if textidl$ = hex(20202020) or                               ~
               textidl$ = hex(00000000) or                               ~
               textidl$ = hex(ffffffff) then return

            stat% = 0%
L19424:     call "TXTPRINT"(#12, f2%(12), 134%, textidl$, "BOM002",      ~
                            dis%, line%, 60%, "N", " ", stat%)
            if stat% = 0% then L19448
            gosub page_head
            goto L19424

L19448:     print skip(1) : line% = line% + 1%
            return

        page_head
                print page
                page% = page% + 1
                print using L60000, date$, time$, company$
                if dotfmt$ = "N" then                                    ~
                    print using L60040, usrid$, page%                     ~
                else                                                     ~
                    print using L60062, usrid$, page%
                print skip(1)
                print using L60070, assystring$

                if qty_method_flag$ = "Y" then print using L60084         ~
                                          else print using L60086
*              PRINT USING 60091, ASSYPART$
                if batch_flag$ = "Y" then print using L60096              ~
                                     else print using L60094,topbatch$
                print skip(1)
                if dotfmt$ = "N" then L19528
                    print using L60132
                    print using L60162
                    print using L60272
                    goto L19540
L19528:         print using L60100
                print using L60140
                print using L60250
L19540:         line% = 10%
                if locsw% = 2% then locsw% = 1%
                if locsw% = 2% then print$(1) = " "
                return

        locations              /* Print References - vertical format */
            if fmtsw$ = "A" then locations_horiz
            locsw% = 0%
            str(ref_key$,57) = " "
L19576:     call "PLOWNEXT" (#6, str(ref_key$,26%), 31%, f1%(6))
                if f1%(6) = 0 then L19640
            get #6, using L19588, location$, quantity_here
L19588:         FMT POS(60), CH(6), PD(14,4)
            call "CONVERT" (quantity_here, 2.2, locqty$)
            if line% > 57 then gosub page_head
            if locsw% = 2% then L19620
            print using L60290, print$(1)
            print using L60330
            line% = line% + 2%
            locsw% = 1%
L19620:     print using L60350, locqty$, location$
            line% = line% + 1%
            locsw% = 2%
            goto L19576

L19640:     if locsw% = 0% then return
            print skip(1)  :  line% = line% + 1%
            print$(1) = " "
            return

        locations_horiz
            locsw% = 0%
            str(ref_key$,57) = " "
L19672:     init(" ") ref_title$(), ref_dash$(), ref_detail$()
            for i% = 1 to 5
                call "PLOWNEXT" (#6, str(ref_key$,26%), 31%, f1%(6))
                    if f1%(6) = 0 then L19724
                get #6, using L19692, location$, quantity_here
L19692:             FMT POS(60), CH(6), PD(14,4)
                call "CONVERT" (quantity_here, 2.2, locqty$)
                ref_title$(i%) = "REF QTY/LOCATION"
                ref_dash$(i%) = "-----------------"
                ref_detail$(i%) = locqty$ & "/" & location$
                if locsw% = 0% then locsw% = 1%
            next i%

L19724:     if locsw% = 0% then return
            if locsw% = 2% then L19776
                print using L60180, print$(1)
                line% = line% + 1%
                if line% > 57% then gosub page_head
L19744:         print using L60410, ref_title$(1), ref_title$(2),         ~
                                   ref_title$(3), ref_title$(4),         ~
                                                  ref_title$(5)
                print using L60410, ref_dash$(1),  ref_dash$(2),          ~
                                   ref_dash$(3),  ref_dash$(4),          ~
                                                  ref_dash$(5)
                line% = line% + 2%
                locsw% = 2%
L19776:         if line% < 59% then L19788
                    gosub page_head
                    goto L19744
L19788:         print using L60410, ref_detail$(1), ref_detail$(2),       ~
                                   ref_detail$(3), ref_detail$(4),       ~
                                                   ref_detail$(5)
                line% = line% + 1%
                if f1%(6) = 0 then return
                goto L19672

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 1%
                  inpmessage$ = " "
                  on fieldnr% gosub L20150,         /* ASSEMBLY PART #  */~
                                    L20200,         /* NUMBER OF LEVELS */~
                                    L20250,         /* EFFECTIVITY DATE */~
                                    L20320,         /* BOM ID           */~
                                    L20380,         /* Text Flag, BOM   */~
                                    L20424,         /* Text Flag, Part  */~
                                    L20440,         /* Refr loc Flag    */~
                                    L20500,         /* Refr Opt Flag    */~
                                    L20550,         /* DOT Format Flag  */~
                                    L20590,         /* Batch flag       */~
                                    L20700          /* Quantity Method  */
                     return
L20150:     REM DEFAULT/ENABLE FOR ASSEMBLY PART NUMBER
                inpmessage$ = "To Print Part Explosions For A Range Of Pa~
        ~rts, Press (9)."
                return
L20200:     REM DEFAULT/ENABLE FOR NUMBER OF LEVELS
         inpmessage$ = "The Maximum PRINTABLE Number Of Levels Is 15."
                if maxlevels$ = " " then maxlevels$ = "15"
                return
L20250:     REM DEFAULT/ENABLE FOR EFFECTIVITY DATE
                if effdatef$ = " " then effdatef$ = date$
                inpmessage$ = "Enter Bill Of Materials Effectivity Date F~
        ~or This Part"
                return

L20320:     REM DEFAULT/ENABLE FOR BOM ID
                inpmessage$ = "Leave Blank To See Effective Bill, or Ente~
        ~r The Specific BOM Id."
                return

L20380:     REM DEFAULT/ENABLE FOR TEXT PRINT FLAG
                if prttxt$ = " " then prttxt$ = "N"
                inpmessage$ = "Enter 'Y' to include Text on Single Level ~
        ~Bill of Material Explosion."
                return

L20424:     REM DEFAULT/ENABLE FOR TEXT PRINT FLAG, Part Text
                if prttxt2$ = " " then prttxt2$ = "N"
                inpmessage$ = "Enter 'Y' to include Part Master File Text~
        ~ on Bill of Materials"
                return

L20440:     REM DEFAULT/ENABLE FOR REFERENCE PRINT FLAG
                if prtref$ = " " then prtref$ = "N"
                inpmessage$ = "Enter 'Y' to include Reference Designators~
        ~ on report"
                return

L20500:     REM DEFAULT/ENABLE FOR REFERENCE PRINT FORMAT OPTION
                inpmessage$ = "Enter 'A' to print references ACROSS the p~
        ~age or 'D' to print DOWN the page"
                if prtref$ <> "Y" then enabled% = 0%
                return

L20550:     REM DEFAULT/ENABLE FOR PRINT USING DOT FORMAT
                inpmessage$ = "Enter 'Y' to print using DOT format or 'N'~
        ~ to print using INDENTED format"
                return

L20590:     REM DEFAULT/ENABLE FOR USING THE BATCH QTY OR SINGLE ASSEMBLY
                inpmessage$ = "Enter 'Y' to use a single assembly instead~
        ~ of the Batch quantity."

                if batch_flag$ = " " then batch_flag$ = "Y"
                return

L20700:     REM Default/Enable for using Quantity Method
                inpmessage$ = "Enter 'Y' to Explode per single Immediate ~
        ~Parent, 'N' per Top Level Parent Qty."

                if qty_method_flag$ = " " then qty_method_flag$ = "Y"
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'162(fieldnr%)
                  enabled% = 1%
                  inpmessage$ = " "
                  on fieldnr% gosub L23180,         /* PART NUMBER RANGE*/~
                                    L23220,         /* CATEGORY RANGE   */~
                                    L23270,         /* NUMBER OF LEVELS */~
                                    L23310,         /* EFFECTIVITY DATE */~
                                    L23370,         /* Text Flag, BOM   */~
                                    L23414,         /* Text Flag, Part  */~
                                    L23430,         /* Refr Loc Flag    */~
                                    L23490,         /* Refr Opt Flag    */~
                                    L23550,         /* DOT Format Flag  */~
                                    L23600,         /* Batch flag       */~
                                    L20700          /* Quantity Method  */
                     return
L23180:     REM DEFAULT/ENABLE FOR PART NUMBER RANGE
                if partr$(1%) = " " then partr$(1%) = "ALL"
            inpmessage$ ="Enter Range of Parts or '?' for selection List."

                return
L23220:     REM DEFAULT/ENABLE FOR CATEGORY RANGE
                inpmessage$ = "Enter Range of Part Categories or '?' for ~
        ~selection List."
                if cat$(1%) = " " then cat$(1%) = "ALL"
                return
L23270:     REM DEFAULT/ENABLE FOR NUMBER OF LEVELS
         inpmessage$ = "The Maximum PRINTABLE Number Of Levels Is 15."
                if maxlevels$ = " " then maxlevels$ = "15"
                return
L23310:     REM DEFAULT/ENABLE FOR EFFECTIVITY DATE
                if effdatef$ = " " then effdatef$ = date$
                inpmessage$ = "Enter Bill Of Materials Effectivity Date F~
        ~or This Part"
                return

L23370:     REM DEFAULT/ENABLE FOR TEXT PRINT FLAG
                if prttxt$ = " " then prttxt$ = "N"
                inpmessage$ = "Enter 'Y' to include component Text on Bil~
        ~l of Material"
                return

L23414:     REM DEFAULT/ENABLE FOR TEXT PRINT FLAG, Part Text
                if prttxt2$ = " " then prttxt2$ = "N"
                inpmessage$ = "Enter 'Y' to include Part Master File Text~
        ~ on Bill of Materials"
                return

L23430:     REM DEFAULT/ENABLE FOR REFERENCE PRINT FLAG
                if prtref$ = " " then prtref$ = "N"
                inpmessage$ = "Enter 'Y' to include Reference Designators~
        ~ on report"
                return

L23490:     REM DEFAULT/ENABLE FOR REFERENCE PRINT FORMAT OPTION
                inpmessage$ = "Enter 'A' to print references ACROSS the p~
        ~age or 'D' to print DOWN the page"
                if prtref$ <> "Y" then enabled% = 0%
                return

L23550:     REM DEFAULT/ENABLE FOR PRINT USING DOT FORMAT
                inpmessage$ = "Enter 'Y' to print using DOT format or 'N'~
        ~ to print using INDENTED format"
                return

L23600:     REM DEFAULT/ENABLE FOR USING THE BATCH QTY OR SINGLE ASSEMBLY
                inpmessage$ = "Enter 'Y' to use a single assembly instead~
        ~ of the Batch quantity."

                if batch_flag$ = " " then batch_flag$ = "Y"
                return

        REM *************************************************************~
            *               HEADER TEXT PRINT ROUTINE                    ~
            *************************************************************

        header_text
            p% = 1%
            if textidh$ = hex(20202020) or                               ~
               textidh$ = hex(00000000) or                               ~
               textidh$ = hex(ffffffff) then return
        REM Print BOM header text that was entered...
            gosub page_head
            dis% = 3%*l%+4%
            stat% = 0%
L25130:     call "TXTPRINT"(#12, f2%(12), 134%, textidh$, "BOM002",      ~
                  dis%, line%, 60%, "N", " ", stat%)
            if stat% = 0% then L25190
            gosub page_head
            goto L25130

L25190:     print skip(1) : line% = line% + 1%
            return

        REM *************************************************************~
            *             HNYMASTR TEXT PRINT ROUTINE                    ~
            *************************************************************
        hny_text
        REM Print any HNYMASTR text that was entered...
            dis% = 3%*l%+4%
            if textidp$ = hex(20202020) or                               ~
               textidp$ = hex(00000000) or                               ~
               textidp$ = hex(ffffffff) then return

            stat% = 0%
L25330:     call "TXTPRINT"(#12, f2%(12), 134%, textidp$, "BOM002",      ~
                            dis%, line%, 60%, "N", " ", stat%)
            if stat% = 0% then L25390
            gosub page_head
            goto L25330

L25390:     print skip(1) : line% = line% + 1%
            return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            keyhit1% = 2%
            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            return clear all
            goto inputmode

L30000: REM *************************************************************~
            * L O A D   B I L L   O F   M A T E R I A L S   R E C O R D *~
            *                                                           *~
            * LOADS THE BILL OF MATERIALS RECORD, WITH ITS ALL-IMPORTANT*~
            * QUANTITY AND SIZE FIELDS FROM THE BILL OF MATERIALS FILE. *~
            *************************************************************
            x$, ref_key$ = " "
            get #5, using L30140, component$, quantity, xused, fixed,     ~
                                 over, bommkr$, op$, compbom$, textidl$, ~
                                 step$
            get #5, str(ref_key$,,56)
            return

L30140:     FMT CH(25),                  /* COMPONENT PART NUMBER      */~
                XX(25),                  /* ASSEMBLY PART NUMBER       */~
                XX(3),                   /* BOM STRUCTURE ID           */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                PD(14,4),                /* QUANTITY REQUIRED          */~
                PD(14,4),                /* TIMES USED (SIZE)          */~
                PD(14,4),                /* FIXED QUANTITY             */~
                PD(14,4),                /* ADDED OVERAGE              */~
                CH(2),                   /* BOM MARKER                 */~
                CH(01),                  /* OPTIONAL COMPONENT FLAG    */~
                CH(3),                   /* COMPONENT BOM              */~
                CH(4),                   /* Line Item Text ID          */~
                CH(04)                   /* 'PICK BEFORE' RTE STEP ID. */

L31000: REM *************************************************************~
            * L O A D   B I L L   O F   M A T E R I A L S   R E C O R D *~
            *                                                           *~
            * LOADS THE BILL OF MATERIALS RECORD, WITH ITS ALL-IMPORTANT*~
            * QUANTITY AND SIZE FIELDS FROM THE BILL OF MATERIALS FILE. *~
            *************************************************************

            get #5, using L31330, rteid$, textidh$, batchqty
            if batchqty > 1 then x$ = "X" else x$ = " "
            return

L31330:     FMT POS(87), CH(3),          /* Route ID                   */~
                POS(90), CH(4),          /* TEXTID Header              */~
                POS(107), PD(14,4)       /* Batch Quantity             */


        REM *************************************************************~
            *       L O A D   R E F E R E N C E   L O C A T I O N       *~
            *                                                           *~
            * LOADS THE LOCATION REFERENCE ASSOCIATED WITH THIS ASSY    *~
            * ALSO HAS QUANTITY AT THIS LOCATION.                       *~
            *************************************************************

L35070:     get #6, using L35110, location$, quantity_here
            return

L35110:     FMT XX(25),                  /* COMPONENT PART NUMBER      */~
                XX(25),                  /* ASSEMBLY PART NUMBER       */~
                XX(3),                   /* BOM STRUCTURE ID           */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                XX(3),                   /* REFERENCE #SEQUENCE        */~
                CH(6),                   /* REFERENCE                  */~
                PD(14,4),                /* QUANTITY HERE              */~
                XX(27)                   /* FREE TEXT @ END OF RECORD  */~

        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'201(fieldnr%)
                init(hex(84)) lfac$()
                str(header$,62) = "BOMEXPLR: " & cms2v$
                if fieldnr% = 0% then gosub L40800 else gosub L40700

                  on fieldnr% gosub L40115,         /* ASSEMBLY PART #  */~
                                    L40130,         /* NUMBER OF LEVELS */~
                                    L40115,         /* EFFECTIVITY DATE */~
                                    L40115,         /* BOM ID           */~
                                    L40115,         /* Text Flag - BOM  */~
                                    L40115,         /* Text Flag - Part */~
                                    L40115,         /* Ref Loc Flag     */~
                                    L40115,         /* Ref Opt Flag     */~
                                    L40115,         /* DOT Format Flag  */~
                                    L40115,         /* Batch Flag       */~
                                    L40115          /* Quantity Method  */
                  goto L40150

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40115:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40130:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40150:     accept                                                       ~
               at (01,02), "Display/Print Assembly Explosions",          ~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Assembly Part Number",                       ~
               at (06,30), fac(lfac$( 1)), assypart$            , ch(25),~
                                                                         ~
               at (07,02), "Number Of Levels",                           ~
               at (07,30), fac(lfac$( 2)), maxlevels$           , ch(02),~
                                                                         ~
               at (08,02), "Effectivity Date",                           ~
               at (08,30), fac(lfac$( 3)), effdatef$            , ch(08),~
                                                                         ~
               at (09,02), "BOM Id (If Not Effective)",                  ~
               at (09,30), fac(lfac$( 4)), primbom$             , ch(03),~
                                                                         ~
               at (10,02), "Include BOM Text on Report?",                ~
               at (10,30), fac(lfac$( 5)), prttxt$              , ch(01),~
                                                                         ~
               at (11,02), "Include PART Text on Report?",               ~
               at (11,30), fac(lfac$( 6)), prttxt2$             , ch(01),~
                                                                         ~
               at (12,02), "Print Reference Locations?",                 ~
               at (12,30), fac(lfac$( 7)), prtref$              , ch(01),~
                                                                         ~
               at (13,02), "Reference Printing Option?",                 ~
               at (13,30), fac(lfac$( 8)), fmtsw$               , ch(01),~
                                                                         ~
               at (14,02), "Print Using DOT Format?",                    ~
               at (14,30), fac(lfac$( 9)), dotfmt$              , ch(01),~
                                                                         ~
               at (15,02), "Use a single Assembly?",                     ~
               at (15,30), fac(lfac$(10%)), batch_flag$         , ch(01),~
                                                                         ~
               at (16,02), "Immediate Parent Explosion?",                ~
               at (16,30), fac(lfac$(11%)), qty_method_flag$    , ch(01),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pfktext$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfktext$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfktext$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L40385
                  call "MANUAL" ("BOMEXPLR")
                  goto L40150

L40385:        if keyhit% <> 0% and keyhit% <> 8% then L40405
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

L40405:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L40150

L40700: REM *************************************************************~
            *               SELECT PF KEYS FOR FIRST FIELD              *~
            *                                                           *~
            *************************************************************

                pfktext$(1%) = "(1)Start Over           (9)Print Explosio~
        ~n for a Range         (13)Instructions"
                if fieldnr% > 1% then L40775
                pfktext$(2%) = "                                         ~
        ~                      (15)Print Screen"
                pfktext$(3%) = "                       (14)Print BOM Mast~
        ~er Listing           " & hex(84) & "(16)Exit Program"
                pfkeys$ = hex(0001090d0f100e)
                return

L40775:         pfktext$(1%) = "(1)Start Over                            ~
        ~                      (13)Instructions"
                pfktext$(2%) = "(4)Previous Field                        ~
        ~                      (15)Print Screen"
                pfktext$(3%) = " "
                pfkeys$ = hex(0001040d0f)
                return

L40800: REM *************************************************************~
            *           SELECT PF KEYS AFTER FIELDS ENTERED             *~
            *                                                           *~
            *************************************************************

                pfktext$(1%) = "(1)Start Over                            ~
        ~                      (13)Instructions"
                pfktext$(2%) = "               (10)See By/Co Products    ~
        ~ (14)Display List     (15)Print Screen"
                pfktext$(3%) = "                                         ~
        ~ (30)Print Report     (16)Exit Program"
                pfkeys$ = hex(00010a0d0e1e0f10)
                return

L41000: REM *************************************************************~
            *       P A R T S   E X P L O S I O N   D I S P L A Y       *~
            *                                                           *~
            * PARTS EXPLOSION DISPLAY SCREEN.  NO BIG DEAL, SINCE ALL   *~
            * THE COLUMNS ARE FORMATTED IN THE PRINT ROUTINE.           *~
            *************************************************************

L41070:     accept                                                       ~
               at (01,02),                                               ~
                  "(2)First     (5)Next     (14)Print BOM    Cursor and (~
        ~10) to See By/Co Products",                                      ~
               at (02,02),                                               ~
                  "Tab to (*) Part And Press (RETURN) To Display Referenc~
        ~e Locations    (16)Return",                                      ~
               at (03,02), fac(hex(84)), titledescr$            , ch(79),~
               at (04,02), fac(hex(ac)), tttle$                 , ch(79),~
               at (05,02), fac(hex(8e)), line$( 1)              , ch(79),~
               at (06,02), fac(hex(8c)), line$( 2)              , ch(79),~
               at (07,02), fac(hex(8c)), line$( 3)              , ch(79),~
               at (08,02), fac(hex(8e)), line$( 4)              , ch(79),~
               at (09,02), fac(hex(8c)), line$( 5)              , ch(79),~
               at (10,02), fac(hex(8c)), line$( 6)              , ch(79),~
               at (11,02), fac(hex(8e)), line$( 7)              , ch(79),~
               at (12,02), fac(hex(8c)), line$( 8)              , ch(79),~
               at (13,02), fac(hex(8c)), line$( 9)              , ch(79),~
               at (14,02), fac(hex(8e)), line$(10)              , ch(79),~
               at (15,02), fac(hex(8c)), line$(11)              , ch(79),~
               at (16,02), fac(hex(8c)), line$(12)              , ch(79),~
               at (17,02), fac(hex(8e)), line$(13)              , ch(79),~
               at (18,02), fac(hex(8c)), line$(14)              , ch(79),~
               at (19,02), fac(hex(8c)), line$(15)              , ch(79),~
               at (20,02), fac(hex(8e)), line$(16)              , ch(79),~
               at (21,02), fac(hex(8c)), line$(17)              , ch(79),~
               at (22,02), fac(hex(8c)), line$(18)              , ch(79),~
               at (24,02), "Exploded as per BATCH of the Top Assembly"  ,~
                                                                         ~
               keys(hex(00010205080a0d0e0f10)),                          ~
               key (keyhit%)

               if keyhit% <> 13 then L41370
                  call "MANUAL" ("BOMEXPLR")
                  goto L41000

L41370:        if keyhit% <> 0% and keyhit% <> 10% and keyhit% <> 8%     ~
                                                               then L41420
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

L41420:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L41070

L42000: REM *************************************************************~
            *       R E F E R E N C E               D I S P L A Y       *~
            *                                                           *~
            * IF A LINE ITEM IS SELECTED FROM THE EXPLOSION DISPLAY     *~
            * THIS ACCEPT WILL SHOW THE 'WHERE USED DETAIL'.            *~
            *************************************************************

L42070:     accept                                                       ~
               at (01,02),                                               ~
                  "(2)First  (5)Next      (14)Print Explosion (15)Print S~
        ~creen   (16)Return To BOM",                                      ~
               at (03,02), fac(hex(84)), titledescr1$           , ch(79),~
               at (04,02), fac(hex(a4)), title1$                , ch(79),~
               at (05,02), fac(hex(8c)), line1$( 1)             , ch(79),~
               at (06,02), fac(hex(8c)), line1$( 2)             , ch(79),~
               at (07,02), fac(hex(8c)), line1$( 3)             , ch(79),~
               at (08,02), fac(hex(8c)), line1$( 4)             , ch(79),~
               at (09,02), fac(hex(8c)), line1$( 5)             , ch(79),~
               at (10,02), fac(hex(8c)), line1$( 6)             , ch(79),~
               at (11,02), fac(hex(8c)), line1$( 7)             , ch(79),~
               at (12,02), fac(hex(8c)), line1$( 8)             , ch(79),~
               at (13,02), fac(hex(8c)), line1$( 9)             , ch(79),~
               at (14,02), fac(hex(8c)), line1$(10)             , ch(79),~
               at (15,02), fac(hex(8c)), line1$(11)             , ch(79),~
               at (16,02), fac(hex(8c)), line1$(12)             , ch(79),~
               at (17,02), fac(hex(8c)), line1$(13)             , ch(79),~
               at (18,02), fac(hex(8c)), line1$(14)             , ch(79),~
               at (19,02), fac(hex(8c)), line1$(15)             , ch(79),~
               at (20,02), fac(hex(8c)), line1$(16)             , ch(79),~
               at (21,02), fac(hex(8c)), line1$(17)             , ch(79),~
               at (22,02), fac(hex(8c)), line1$(18)             , ch(79),~
               at (23,02), fac(hex(8c)), line1$(19)             , ch(79),~
               at (24,02), fac(hex(8c)), line1$(20)             , ch(79),~
                                                                         ~
               keys(hex(000102050d0e0f10)),                              ~
               key (keyhit%)

               if keyhit% <> 13 then L42420
                  call "MANUAL" ("BOMEXPLR")
                  goto L42070

L42420:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L42070

        REM *************************************************************~
            * I N P U T   M A X L E V E L S %   F O R   S E L E C T O R *~
            *                                                           *~
            * THIS SCREEN HAS IT SIMPLE--IT JUST GETS MAXLEVELS% FOR    *~
            * SUPER-SELECTION. ALSO GETS UPDATE FLAG.                   *~
            *************************************************************

            deffn'202(fieldnr%)
                str(header$,62) = "BOMEXPLR: " & cms2v$
                gosub L43800
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L43280,         /* PART NUMBER RANGE*/~
                                    L43280,         /* CATEGORY RANGE   */~
                                    L43310,         /* MAX # OF LEVELS  */~
                                    L43280,         /* EFFECTVITY DATE  */~
                                    L43280,         /* Text Flag - BOM  */~
                                    L43280,         /* Text Flag - Part */~
                                    L43280,         /* Refr Loc Flag    */~
                                    L43280,         /* Refr Opt Flag    */~
                                    L43280,         /* DOT Format Flag  */~
                                    L43280,         /* Batch Flag       */~
                                    L43280          /* Qty Method       */
                     goto L43350

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L43280:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L43310:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L43350:     accept                                                       ~
               at (01,02), "Display/Print Assembly Explosions",          ~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,29), fac(hex(ac)), title$(1)              , ch(25),~
               at (06,55), fac(hex(ac)), title$(2)              , ch(25),~
                                                                         ~
               at (07,02), "Part Number",                                ~
               at (07,29), fac(lfac$( 1)), partr$(1)            , ch(25),~
               at (07,55), fac(lfac$( 1)), partr$(2)            , ch(25),~
                                                                         ~
               at (08,02), "Part Category",                              ~
               at (08,29), fac(lfac$( 2)), cat$(1)              , ch(04),~
               at (08,55), fac(lfac$( 2)), cat$(2)              , ch(04),~
                                                                         ~
               at (09,02), "Maximum Number Of Levels",                   ~
               at (09,30), fac(lfac$( 3)), maxlevels$           , ch(02),~
                                                                         ~
               at (10,02), "Effectivity Date",                           ~
               at (10,30), fac(lfac$( 4)), effdatef$            , ch(08),~
                                                                         ~
               at (11,02), "Include BOM Text In Print?",                 ~
               at (11,30), fac(lfac$( 5)), prttxt$              , ch(01),~
                                                                         ~
               at (12,02), "Include part Text In Print?",                ~
               at (12,30), fac(lfac$( 6)), prttxt2$             , ch(01),~
                                                                         ~
               at (13,02), "Print Reference Locations?",                 ~
               at (13,30), fac(lfac$( 7)), prtref$              , ch(01),~
                                                                         ~
               at (14,02), "Reference Printing Option?",                 ~
               at (14,30), fac(lfac$( 8)), fmtsw$               , ch(01),~
                                                                         ~
               at (15,02), "Print Using DOT Format?",                    ~
               at (15,30), fac(lfac$( 9)), dotfmt$              , ch(01),~
               at (16,02), "Use a single Assembly?",                     ~
               at (16,30), fac(lfac$(10%)), batch_flag$         , ch(01),~
                                                                         ~
               at (17,02), "Immediate Parent Explosion?",                ~
               at (17,30), fac(lfac$(11%)), qty_method_flag$    , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pfktext$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfktext$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfktext$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13 then L43761
                  call "MANUAL" ("BOMEXPLR")
                  goto L43350

L43761:        if keyhit% <> 0 and keyhit% <> 8 then L43770
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

L43770:        if keyhit% <> 15 then return
                  call "PRNTSCRN"
                  goto L43350
L43800: REM *************************************************************~
            *               SELECT PF KEYS FOR FIRST FIELD              *~
            *                                                           *~
            *************************************************************

                pfktext$(1%) = "(1)Start Over                            ~
        ~                      (13)Instructions"
                if fieldnr% > 1% then L43960
                if fieldnr% = 0% then L44020
                pfktext$(2%) = "                                         ~
        ~                      (15)Print Screen"
                pfktext$(3%) = "                                         ~
        ~                     " & hex(84) & "(16)Exit Screen"
                pfkeys$ = hex(00010d0f10)
                return

L43960:         pfktext$(2%) = "                        (4)Previous Field~
        ~                      (15)Print Screen"
                pfktext$(3%) = " "
                pfkeys$ = hex(0001040d0f)
                return

L44020:         pfktext$(2%) = "                                         ~
        ~                      (15)Print Screen"
                pfktext$(3%) = "                                         ~
        ~                     " & hex(84) & "(16)Print Report"
                pfkeys$ = hex(00010d0f10)
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50140,         /* ASSEMBLY PART #  */~
                                    L50310,         /* NUMBER OF LEVELS */~
                                    L50370,         /* EFFECTIVITY DATE */~
                                    L50480,         /* BOM ID           */~
                                    L50620,         /* Text Flag, BOM   */~
                                    L50634,         /* Text Flag, Part  */~
                                    L50650,         /* Refr Loc Flag    */~
                                    L50720,         /* Refr Opt Flag    */~
                                    L51590,         /* DOT Format Flag  */~
                                    L50790,         /* Batch Flag       */~
                                    L51650          /* Qty Method       */
                     return
L50140:     REM TEST DATA FOR ASSEMBLY PART NUMBER
                readkey$ = assypart$
                hdr$(2)="  Part Assemblies             Part Descriptions"
                hdr$(3) = hex(ac) & "Select the Starting Assy to Explode ~
        ~using Cursor & RETURN or PF16 to Exit"
                assypartdescr$ = hex(06) & "Select the Part Assembly to "~
                                                              & "Explode"
                incl(1) = 0
                call "PLOWCODE" (#5, readkey$, assypartdescr$, -8025%,   ~
                       -.32,f1%(5),hdr$(),3,75,incl(),incl$(),"Y","Y",#4)
                     if f1%(5) = 0 then L50290
                assypart$ = readkey$
                assypartdescr$ = "(NOT ON FILE)"
                call "DESCRIBE" (#4,assypart$,assypartdescr$,1%,f1%(4))
                return
L50290:         errormsg$ = "Unknown Assembly Part: " & assypart$
                return
L50310:     REM TEST DATA FOR NUMBER OF LEVELS
                if maxlevels$ = " " then maxlevels$ = "1"
                call "NUMTEST" (maxlevels$, 1, 15, errormsg$, 0.0, temp)
                     if errormsg$ <> " " then return
                maxlevels% = temp
                return
L50370:    REM TEST DATA FOR EFFECTIVITY DATE
               call "DATEOK" (effdatef$, u3%, errormsg$)
               if errormsg$ <> " " then return
                  effdate$ = effdatef$
                  call "DATUNFMT" (effdate$)
               call "PIPINDEX" (#3, effdate$, dateindex%, ret%)
                   if ret% <> 1% then return
               errormsg$ = "Planning Calander Base Date not Found.  Exit ~
        ~& Correct."
               return

L50480:    REM TEST DATA FOR SPECIFIC BOM ID
            if primbom$ = " " then return
            readkey$ = str(assypart$,,25) & str(primbom$,,3)
            hdr$()="  Listed Below Are The Existing BOMs For Part: " &   ~
                                                                assypart$
            errormsg$ = hex(06) & "Select Bill Of Materials"
            call "PLOWCODE" (#5, readkey$, errormsg$, 2025%, .30, f1%(5),~
                                                               hdr$(), 3)
            errormsg$ = " "
                if f1%(5) <> 0% then L50590
                errormsg$ = "BOM not on file" : return
L50590:     primbom$ = str(readkey$,26%,3%)
            return

L50620:    REM TEST DATA FOR TEXT PRINT FLAG, BOM
                if prttxt$ = " " then prttxt$ = "N"
                if pos("YN" = prttxt$) = 0 then L50690
                return

L50634:    REM TEST DATA FOR TEXT PRINT FLAG, Part
                if prttxt2$ = " " then prttxt2$ = "N"
                if pos("YN" = prttxt2$) = 0 then L50690
                return

L50650: REM Test data for reference print flag
                if prtref$ = " " then prtref$ = "N"
                if pos("YN" = prtref$) = 0 then L50690
                if prtref$ = "N" then fmtsw$ = " "
                return
L50690:         errormsg$ = "Please Enter 'Y' or 'N'"
                return

L50720: REM Test data for reference format flag
                if fmtsw$ = " " then fmtsw$ = "D"
                if pos("AD" = fmtsw$) = 0 then L50760
                return
L50760:         errormsg$ = "Please enter 'A' or 'D'"
                return

L50790: REM Test data for Batch qty Flag
                if pos("YN" = batch_flag$) <> 0% then return
                errormsg$ = "Please enter 'Y' or 'N'"
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51150,         /* PART NUMBER RANGE*/~
                                    L51190,         /* CATEGORY RANGE   */~
                                    L51230,         /* NUMBER OF LEVELS */~
                                    L51290,         /* EFFECTIVITY DATE */~
                                    L51400,         /* Text Flag, BOM   */~
                                    L51434,         /* Text Flag, Part  */~
                                    L51450,         /* Refr Loc Flag    */~
                                    L51520,         /* Refr Opt Flag    */~
                                    L51590,         /* DOT Format Flag  */~
                                    L50790,         /* Batch Flag       */~
                                    L51650          /* Qty Method       */
                     return
L51150:     REM TEST DATA FOR PART NUMBER RANGE
                call "TESTRNGE"   (partr$(1), partr$(2), partr$(3),      ~
                                   partr$(4), errormsg$, #4)
                return
L51190:     REM TEST DATA FOR CATEGORY RANGE
                call "TESTRNGE"   (cat$(1), cat$(2), cat$(3), cat$(4),   ~
                                              errormsg$, #7)
                return
L51230:     REM TEST DATA FOR NUMBER OF LEVELS
                if maxlevels$ = " " then maxlevels$ = "1"
                call "NUMTEST" (maxlevels$, 1, 15, errormsg$, 0.0, temp)
                     if errormsg$ <> " " then return
                maxlevels% = temp
                return
L51290:    REM TEST DATA FOR EFFECTIVITY DATE
               call "DATEOK" (effdatef$, u3%, errormsg$)
               if errormsg$ <> " " then return
                  effdate$ = effdatef$
                  call "DATUNFMT" (effdate$)
               call "PIPINDEX" (#3, effdate$, dateindex%, ret%)
                   if ret% <> 1% then return
               errormsg$ = "Planning Calander Base Date not Found.  Exit ~
        ~& Correct."
               return

L51400:    REM TEST DATA FOR TEXT PRINT FLAG
                if prttxt$ = " " then prttxt$ = "N"
                if pos("YN" = prttxt$) = 0 then L51490
                return

L51434:    REM TEST DATA FOR TEXT PRINT FLAG, Part
                if prttxt2$ = " " then prttxt2$ = "N"
                if pos("YN" = prttxt2$) = 0 then L51490
                return

L51450:    REM TEST DATA FOR REFERENCE PRINT FLAG
                if prtref$ = " " then prtref$ = "N"
                if pos("YN" = prtref$) = 0 then L51490
                return
L51490:         errormsg$ = "Please Enter 'Y' or 'N'"
                return

L51520: REM Test data for reference format flag
                if fmtsw$ = " " then fmtsw$ = "D"
                if pos("AD" = fmtsw$) = 0 then L51560
                return
L51560:         errormsg$ = "Please enter 'A' or 'D'"
                return

L51590: REM Test data for DOT print format
                if dotfmt$ = " " then dotfmt$ = "N"
                if dotfmt$ = "Y" or dotfmt$ = "N" then return
                errormsg$ = "Please enter 'Y' or 'N'"
                return

L51650: REM Test data for Qty Method Flag
                if pos("YN" = qty_method_flag$) <> 0% then return
                errormsg$ = "Please enter 'Y' or 'N'"
                return

L60000: %RUN DATE: ######## @ ########     ##############################~
        ~##############################                      BOMEXPLR: BOM~
        ~002

L60040: %BY: ###                                I N D E N T E D   B I L L~
        ~   O F   M A T E R I A L S                              PAGE: ###

L60062: %BY: ###                                     D O T  B I L L  O F ~
        ~  M A T E R I A L S                                     PAGE: ###

L60070: % ASSEMBLY NUMBER: ##############################################~
        ~################################################################

L60084: % Components are Exploded per Immediate Parent
L60086: % Components are Exploded per Top Level Parent

        % Batch-Controlled Parts ('X' In 'BC' Column) Are Exploded Per Ba~
        ~tch of #########################
L60094: % Batch Size is #############
L60096: % Batch Control is not being used; a Single Assembly Part is eval~
        ~uated

L60100: %                                                                ~
        ~          TOTAL QTY          FXD QTY  BOM  RTE  PICK  O B MARKER ~
        ~IF

L60132: % LEVEL                                                          ~
        ~          TOTAL QTY          FXD QTY  BOM  RTE  PICK  O B MARKER ~
        ~IF

L60140: % PART NUMBER / PART DESCRIPTION                                 ~
        ~           REQUIRED  UOM     PER RUN  ID   ID   STEP  P C NON STA~
        ~ND

L60162: %                                    PART NUMBER / PART DESCRIPTI~
        ~ON         REQUIRED  UOM     PER RUN  ID   ID   STEP  P C NON STA~
        ~ND

L60180: % ###############################################################~
        ~##############

L60210: % ###############################################################~
        ~######## ########## ##### ##########  ########  ####  # # #######~
        ~###

L60250: % ---------------------------------------------------------------~
        ~-------- ---------- ----- ----------  ---  ---  ----  - - -------~
        ~---

L60272: % 1-------5--------10--------15 ---------------------------------~
        ~-------- ---------- ----- ----------  ---  ---  ----  - - -------~
        ~---

L60290: % ###############################################################~
        ~##############    REF QTY  REF LOCTN

L60330: %                                                                ~
        ~                  -------  ---------

L60350: %                                                                ~
        ~                #########  ######

L60410: %                                            ################# ##~
        ~############### ################# ################# #############~
        ~####

L60450: %                                      * * * * *  E N D   O F   L~
        ~ I S T I N G  * * * * *

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end
