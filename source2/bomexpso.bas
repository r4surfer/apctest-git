        REM *************************************************************~
            *                                                           *~
            *  BBBB    OOO   M   M  EEEEE  X   X  PPPP    SSS    OOO    *~
            *  B   B  O   O  MM MM  E       X X   P   P  S      O   O   *~
            *  BBBB   O   O  M M M  EEEE     X    PPPP    SSS   O   O   *~
            *  B   B  O   O  M   M  E       X X   P          S  O   O   *~
            *  BBBB    OOO   M   M  EEEEE  X   X  P       SSS    OOO    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMEXPSO - Prints or displays an explosion of the BOM used*~
            *            for a sales order line, or a range of lines.   *~
            *            BOM is phantomized & optiomized.               *~
            *            Can be externally controlled through GETPARM,  *~
            *            just pass in SO key, report is generated with  *~
            *            no operator intervention.                      *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/12/86 ! ORIGINAL (Cloned BOMEXPLR)               ! HES *~
            * 05/13/87 ! HNYMASTR record length mod for Std Cost  ! JIM *~
            * 09/06/89 ! Modified to coincide w/modified TXTPRINT ! JDH *~
            * 02/14/94 ! PRR 13072 - Now uses BCKLINES File instead!RJH *~
            *          !  of DEMMASTR to allow all Sales Orders to!     *~
            *          !  be used to display BOM Explosion. May   !     *~
            *          !  now select what type of Quantity to use.!     *~
            *          ! Ergonomic changes- Edit Mode,PF4 PrevLine!     *~
            *          !  ,etc.                                   !     *~
            *          ! Dimensioned some variables & fixed Intergers   *~
            * 03/08/94 ! Increased record length for BOMSPEC       !WPH *~
            * 01/09/96 ! PRR 13551.  Now prints proper Rte ID.    ! JDH *~
            * 09/06/96 ! Changes for the year 2000.               ! DXL *~
            *************************************************************

        dim                                                              ~
            assmbom$3,                   /* ASSEMBLY BOM  NUMBER       */~
            assypart$25,                 /* ASSEMBLY PART NUMBER       */~
            assypartkey$28,              /* ASSEMBLY PART NUMBER - BOM */~
            assypartdescr$40,            /* ASSEMBLY PART DESCRIPTION  */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bom$(490)3,                  /* BOM LIST                   */~
            bomid$3,                     /* WHICH ALT BOM?             */~
            bommkr$2,                    /* BOM MARKER                 */~
            compbom$3,                                                   ~
            component$25,                /* COMPONENT PART NUMBER      */~
            componentkey$28,             /* COMPONENT PART NUMBER - BOM*/~
            count%(99,2),                /* ITEM COUNT OF 15 LEVELS    */~
            cursor%(2),                  /* CURSOR LOCATION FOR DISPLAY*/~
            cuscode$9,                   /* Customer Code              */~
            cx$25,                       /* TEMP COMP AND MARKER       */~
            date$8,                      /* CALENDAR DATE FOR SHOW OFF */~
            edtmessage$79,               /* EDIT  MESSAGE TEXT INFO    */~
            effdatef$8,                  /* EFFECTIVITY DATE FORMATTED */~
            effdate$8,                   /* EFFECTIVITY DATE           */~
            errormsg$79,                 /* ERROR MESSAGE TEXT INFO    */~
            ext(99),                     /* QUANTITY EXTENTION ARRAY   */~
            hdr$(3)79,                   /* PLOWCODE Screen Titles     */~
            i$(24)80,                    /* SCREEN IMAGE (NOT USED)    */~
            header$79,                   /* Screen Title               */~
            headerkey$31,                /* BOMMASTR header record key */~
            headline$80,                 /* Header Text Line           */~
            infomsg$79,                  /* INFORMATIVE MESSAGE TEXT   */~
            inpmessage$79,               /* INPUT MESSAGE TEXT INFO    */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line1$(20)79,                /* DISPLAY FOR WHERE USED     */~
            line$(20)79,                 /* TEXT TO DISPLAY ON SCREEN  */~
            location$6,                  /* REFERENCE LOCATION CODE    */~
            m$(99)1,                     /* RETAIN MARKERS             */~
            mask$132,                    /* MASK FOR TEXT PRINT        */~
            mkr$(20)2,                   /* ALLOWED BOMMARKERS         */~
            mkrdes$(20)10,               /* ALLOWED BOMMARKERS         */~
            op$1,                        /* Option Flag                */~
            opflag$12,                   /* Option Print Message       */~
            p$(99)31,                    /* READ KEYS FOR 99 LEVELS    */~
            part$25,                     /* DUMMY ARGUMENT PART #      */~
            part$(2,2)25,                /* Part Number Range          */~
            partkey$28,                  /* DUMMY ARGUMENT PART # - BOM*/~
            pfkeys$32,                   /* FUNCTION KEYS ENABLED LISTS*/~
            pfktext$(3)79,               /* FUNCTION KEYS ENABLED LISTS*/~
            pkey$19,                     /* FOR PRINT ONLY MODE        */~
            print$(10)80,                /* ARRAYS TO PRINT INFO WITH  */~
            prtref$1,                    /* Print References Flag      */~
            prttxt$1,                    /* Text Print Flag            */~
            qty$10,                      /* Quantity to be printed     */~
            qty_type$1,                  /* Type of Quantity Flag      */~
            readkey$50,                  /* KEY FOR PLOW ROUTINES      */~
            readkey2$50,                 /* KEY FOR PLOW ROUTINES      */~
            ref_key$62,                  /* SAVES KEY FOR REFER PLOW   */~
            ref_key$(20)62,              /* SAVES KEY FOR REFER DISPLAY*/~
            rteid$3,                     /* PEG TO ROUTING             */~
            so$(2,2)16,                  /* Sales Order Number Range   */~
            so$16,                       /* SALES ORDER NUMBER         */~
            so_descr$32,                 /* CUSTOMER DESCRIPTION       */~
            so_line$3,                   /* SALES ORDER LINE NUMBER    */~
            subtitle$(2)25,              /* SCREEN SUBTITLE            */~
            tempdescr$34,                /* TEMPORARY PART DESCRIPTION */~
            textidl$4,                   /* Line Item Text ID          */~
            textidh$4,                   /* Header    Text ID          */~
            title$79,                    /* TITLE FOR DISPLAY ENTRIES  */~
            titledescr$79,               /* SHOW WHAT PART WE'RE DOING */~
            title1$79,                   /* TITLE FOR WGERE USED SCRN  */~
            titledescr1$79               /* PART FOR WHERE USED SCREEN */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
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
            * # 2 ! CUSTOMER ! THE CUSTOMER MASTER FILE.                *~
            * # 3 ! SYSFILE2 ! Caelus Management System Information     *~
            * # 4 ! HNYMASTR ! INVENTORY MASTER FILE (NAME, DESCRIPTION)*~
            * # 5 ! BOMMASTR ! BILL OF MATERIALS RELATIONSHIP FILE      *~
            * # 6 ! BOMREFER ! LOCATION REFERENCE FILE                  *~
            * # 7 ! BOMSPEC  ! SPECIFIED OPTIONS FILE                   *~
            * #11 ! ENGMASTR ! ENGINEERING MASTER FILE                  *~
            * #12 ! TXTFILE  ! SYSTEM TEXT FILE                         *~
            * #16 ! BCKLINES ! Sales Order Master- Lines                *~
            *************************************************************

            select #2,  "CUSTOMER"                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1200,                                  ~
                        keypos=1, keylen=9

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

            select # 7, "BOMSPEC",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 150,                                   ~
                        keypos =  26, keylen = 54,                       ~
                        alt key  1, keypos = 57, keylen = 23

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

            select #16,  "BCKLINES",                                     ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#2, 0%, f2%(2), 0%, " ")
            call "OPENCHCK" (#3, 0%, f2%(3), 0%, " ")
            call "OPENCHCK" (#4, 0%, f2%(4), 0%, " ")
            call "OPENCHCK" (#5, 0%, f2%(5), 0%, " ")
            call "OPENCHCK" (#6, 0%, f2%(6), 0%, " ")
            call "OPENCHCK" (#7, 0%, f2%(7), 0%, " ")
            call "OPENCHCK" (#11, 0%, f2%(11), 0%, " ")
            call "OPENCHCK" (#16, 0%, f2%(6%), 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES ALL THE VARIABLES NEEDED TO DO THE REPORT.    *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            REM DATE INITIALIZATION
                date$ = date
                call "DATEFMT" (date$)
                f3% = 1%

            readkey$ = "TABLE01:"
L09120:     call "PLOWNEXT" (#3, readkey$, 8%, f1%(3))
                if f1%(3) = 0 then L09190
            u3% = u3% + 1
            get #3, using L09160, mkr$(u3%), mkrdes$(u3%)
L09160:     FMT XX(8), CH(2), XX(40), CH(10)
            if u3% < 20 then L09120

L09190:     subtitle$(1) = "Beginning"
            subtitle$(2) = "Ending"
            put mask$ using L19750, " ", " ", " ", " ", " ", " ", " "

*          CALL "GETPARM" ADDR ("ID", "S", "DEMKEY  ", "@", "0001",     ~
*               "NOSCRN", 0%, "K", "DEMAND  ", PKEY$, 19%, 5%, 32%, "U")
*          IF PKEY$ <> " " THEN 13000

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            errormsg$, inpmessage$, assypart$, bomid$, compbom$, prttxt$,~
            effdate$, effdatef$, so_line$, prtref$, qty_type$ = " "

            page%, np% = 0%
            keyhit1% = 2%
            select ws

L10130:     for fieldnr% = 1% to  3%
                gosub'161(fieldnr%)
                      if enabled% = 0% then L10290
L10160:         gosub'201(fieldnr%, 1%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10240
L10190:                  fieldnr% = max(1%, fieldnr% - 1%)
                         if fieldnr% = 1% then L10130
                         gosub'161(fieldnr%)
                         if enabled% <> 0 then L10160
                         goto L10190
L10240:               if keyhit%  =  9% then       L11000      /* PRINT  */
                      if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit% <>  0% then       L10160
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10160
L10290:         next fieldnr%

            goto edit_pg1

        edit_pg1
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'201(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       display_bom
                  if keyhit% <>  0% then       edit_pg1
L10400:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 3% then edit_pg1
            if fieldnr% = lastfieldnr% then    edit_pg1
            gosub'161(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       edit_pg1
L10450:     gosub'201(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L10450
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L10450
                  lastfieldnr% = fieldnr%
            goto L10400

L11000: REM *************************************************************~
            *              P R I N T   E X P L O S I O N S              *~
            *                                                           *~
            * GETS RANGE AND OPTIONS.                                   *~
            *************************************************************

            errormsg$, inpmessage$, so$(), part$() = " "
            REM NOW GET MAX NUMBER OF LEVELS TO PRINT OUT.
                for field% = 1% to 5%
L11140:             gosub'162(field%)
                          if enabled% = 0% then L11215
L11155:             gosub'202(field%, 1%)          /* GET FIELDS       */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L11195
L11170:                   field% = max(1%, field% - 1%)
                          gosub'162(field%)
                          if enabled% <> 0% then L11155
                          if field% = 1% then L11140
                             goto L11170
L11195:               if keyhit%  = 16% then       inputmode
                      if keyhit% <>  0% then       L11155
                    gosub'152(field%)
                          if errormsg$ <> " " then L11155
L11215:             next field%
            goto edit_pg2


        edit_pg2
            inpmessage$ = edtmessage$
            lastfieldnr% = 0%
            gosub'202(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       print_boms
                  if keyhit% <>  0% then       edit_pg2
L11305:     field% = cursor%(1%) - 5%
            if field% < 1% or field% > 5% then edit_pg2
            if field% = lastfieldnr% then    edit_pg2
            gosub'162(field%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       edit_pg2
L11355:     gosub'202(field%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11355
            gosub'152(field%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11355
                  lastfieldnr% = field%
            goto L11305

L12000: REM *************************************************************~
            *          D I S P L A Y   P L O W   R O U T I N E          *~
            *                                                           *~
            * PLOWS THROUGH THE BILL OF MATERIALS FOR THE ASSEMBLY PART *~
            * NUMBER AND PRINTS                                         *~
            *************************************************************
        display_bom
            call "SHOSTAT" ("EXPLODING ASSEMBLY PART")
            l%, displayline% = 0%
            call "DESCRIBE" (#4, assypart$, assypartdescr$, 0%, f1%(4%))
            line$(), ref_key$() = " "
            REM SET TITLE DESCRIPTION FOR SCREEN SHOWING.
            gosub'190(assypart$)                 /* Get BOM ID */
            assypartkey$ = str(assypart$,,25%) & str(bomid$,,3%)
            headerkey$ = str(assypartkey$) & "  0"
            gosub L31000
            title$, titledescr$ = " "
                str(titledescr$,6) = "Component Parts For: "             ~
                                     & assypart$ & " (" & assypartdescr$ ~
                                     & ")"
                if compbom$ =  " " then                                  ~
                 str(title$, 6, 30) = " BOM " & bomid$ & " Effective On "~
                                             & effdatef$                 ~
                else                                                     ~
                 str(title$, 6, 30) = " BOM " & bomid$ & " As Specified"
                 str(title$,70) = "Qty Needed"

            mat count% = zer
            pl%, l%, p% = 0%
            gosub'7(1%, assypartkey$, so_qty)         /* Here we go... */
            if displayline% <> 0 then gosub L12490  /* DISPLAY PARTIAL  */
            goto inputmode               /* GET NEXT PART TO BOM       */

        deffn'7(mode%, partkey$, netqty)
                  pl% = pl% + 1%           /* ACTUAL LEVEL */
                  l% = max(1, pl% - p%)    /* L% Is The 'Pseudo-Level'.*/
                  p$(pl%) = partkey$
                  ext(pl%) = netqty
                  if netqty = 0 and pl% > 1% then L12300

L12185:           call "PLOWNEXT" (#5, p$(pl%), 28%, f1%(5%))
                       if f1%(5%) = 0 then L12300
                  if str(p$(pl%),29%,3%) <> "  0" then L12210
*                   GOSUB 31000         /* LOAD BOM HEADER INFO */
                     goto L12185
L12210:           gosub L30000               /* Load BOM Record & Info. */
                  m$(pl%) = bommkr$
                  if np% > 0% or mode% = 1 then L12230
                     if prttxt$ = "Y" then gosub header_text
L12230:           count%(l%,1)=count%(l%,1)+1 /*Count Of Hits This Levl*/
                  opflag$ = " "
                  if op$ <> "Y" then L12250
                     count%(l%,2)=count%(l%,2)+1   /* Optns This Level */
                     gosub L33000         /* Load Replacement Component */
L12250:           gosub'190(component$)   /* GET BOM FOR COMPONENT     */
                  componentkey$ = str(component$,,25) & str(bomid$,,3)
                  headerkey$ = str(componentkey$) & "  0"
                  gosub L31000
                  netqty = netqty * ext(pl%) + fixed

                  on mode% gosub L12330, L19125 /* Process Entry */
                  if m$(pl%) = "P" then p% = p% + 1
                  if pl% < 99% then gosub'7(mode%, componentkey$, netqty)
                                         /* DO COMPS IF NOT AT BOTTOM  */
                  goto L12185

L12300:           REM END ROUTINE GRACEFULLY.
                      pl% = pl% - 1
                      if pl% = 0 then L12320
                      if m$(pl%)="P" then p% = p%-1   else count%(l%,1)=0
                      l% = max(1, pl% - p%)
L12320:     return /* Eventualy This Will Return Completely Out Of Here*/

L12330:     REM FILL SCREEN ENTRIES, AND DISPLAY WHEN FULL.
                displayline% = displayline% + 1
                if displayline% > 20 then L12490    /* SHOW SCREEN.     */
                if bommkr$ <> "ST" then L12365
                   cx$ = component$
                   if op$ = "Y" then cx$ = component$ & " [OP]"
                   goto L12385
L12365:         cx$ = component$ & " [" & bommkr$
                if op$ = "Y" then cx$ = cx$ & ",OP"
                cx$ = cx$ & "]"

L12385:            put str(line$(displayline%), min(2*l%-1,29), 38),     ~
                        using L12475, count%(l%,1), cx$
                   str(line$(displayline%),71) = "!"
                   call "CONVERT" (netqty, 0.2,                          ~
                                           str(line$(displayline%),72,8))
                   if bomid$ <> " " then str(line$(displayline%),62,9) = ~
                                                         " Bom:" & bomid$
                   displayline% = displayline% + 1
                   call "DESCRIBE"(#4, component$, tempdescr$, 1%,f1%(4))
                   str(line$(displayline%),71) = "!"
                   put str(line$(displayline%), min(2*l%-1,29), 39),     ~
                                                  using L12480, tempdescr$
                   ref_key$(displayline%),                               ~
                   ref_key$(displayline% - 1) = ref_key$
                   return

L12475: %###. #################################
L12480: %     ##################################

L12490:     REM DISPLAY CONTROLLER FOR FULL SCREEN.  HANDLES P.F. KEYS.
                keyhit1% = 1%
L12500:         gosub L41000
                      if keyhit%  =  2% then L12000
                      if keyhit%  =  5% then L12595
                      if keyhit% <> 14% then L12555
L12520:                  infomsg$ = "Printing Explosion For" & hex(84)   ~
                                                              & assypart$
                         call "SHOSTAT" (infomsg$)
                         return clear all
                         compbom$ = assmbom$
                         gosub'200(assypart$)
                         close printer
                         goto inputmode
L12555:         if keyhit%  = 16% then       inputmode
                if keyhit% <>  0% then       L12500
                      fieldnr% = max(0%, cursor%(1%) - 4%)
                      if fieldnr% < 1% then L12500
                      if fieldnr% > 20% then L12500
                      if line$(fieldnr%) = " " then L12500
                      goto L12615

L12595:         displayline% = 0%
                init(" ") line$(), ref_key$()
                goto L12330               /* WHEN SIGNALS FOR NEXT.     */

L12615: REM ************* CONTROLLER FOR REFERENCE SCREEN ***************

               ref_key$ = ref_key$(fieldnr%)
               fieldnr%=((fieldnr%+1%)/2%)*2%
               init (" ") title1$, titledescr1$
               str(titledescr1$, 6) = "REFERENCE LIST FOR:  " &          ~
                                      str(ref_key$,, 25)
               x% = max( pos(line$(fieldnr%) = "(" ),1)
               if x%>57% then L12675
               titledescr1$ = titledescr1$ & " " &                       ~
                   str(line$(fieldnr%), x%, 58%-x%)

L12675:         str(title1$,10)="For Part: " & str(ref_key$,26,25)
                title1$= title1$ & "   BOM Id: " & str(ref_key$,51,3)
L12685:        str(ref_key$,57) = " "
               rline% = 0
L12695:        gosub locations_for_screen
               if line1$() = " " then L12500
L12705:             gosub L42000
                    keyhit1% = 1%
                    if keyhit%  = 16% then L12500
                    if keyhit%  = 14% then L12520
                    if keyhit%  =  2% then L12685
                    if keyhit%  =  5% then L12695
                    goto L12705

        locations_for_screen     /* READ ASSOCIATED LOCATION(S) IF ANY */
                line1$() = " "
                counter%, u3% = 0%
                horiz% = 1%
L12770:         if counter% = 60% then return
                call "PLOWNEXT" (#06, str(ref_key$,26%), 31%, f1%(06%))
                       if f1%(06%) = 0% then return
                gosub L35070                /* 'GET' IT  */
                counter% = counter% + 1%
                if counter% = 21% then u3% = 20%
                if counter% = 21% then horiz% = 27%
                if counter% = 41% then u3% = 40%
                if counter% = 41% then horiz% = 53%
                rline% = rline% + 1%
                call "CONVERT" (quantity_here, 0.2, str(tt$,,5%))
                put str(line1$(counter%-u3%),horiz%), using L12830,rline%,~
                                                           tt$, location$
L12830:         %####)##### at ######
                goto L12770

        REM *************************************************************~
            *             P R I N T  C O N T R O L   L O O P            *~
            *                                                           *~
            * PLOWS THROUGH BCKLINES FILE LOOKING FOR CANDIDATES.       *~
            *************************************************************
        print_boms
            call "SHOSTAT"  ("Printing Bills of Materials")
            readkey$ = " "
            if pkey$ = " " then L13120
                str(readkey$,,19) = pkey$ addc all(hex(ff))
                prttxt$, prtref$ = "N"
                goto L13130
L13120:     readkey$ = so$(2,1)
L13130:     REM Plow BCKLINES File...
L13140:     call "PLOWNEXT" (#16, readkey$, 0%, f1%(16%))
                if f1%(16%) = 0% then L13310      /* All Done */
            if pkey$ <> " " then L13180
            if str(readkey$,,16) > so$(2,2) then L13310 /* All Done */
L13180:     gosub load_so_data
            if pkey$ <> " " then L13240
            if assypart$< part$(2,1) or assypart$> part$(2,2) then L13140
L13240:     so$ = readkey$
            so_line$ = str(readkey$,17)
            gosub format_so_data
            gosub set_quantity
            if errormsg$ = " " then gosub'200(assypart$)
            if pkey$ <> " " then L65000
            goto L13140

L13310:    REM NOW CLOSE THE PRINTER, AND START AGAIN
               close printer
               so$, so_descr$, so_line$ = " "
               goto inputmode

        REM *************************************************************~
            *      G E T   E F F E C T I V E   B O M                    *~
            *                                                           *~
            * GETS THE CURRENT BOM FOR PASSED PART BASED ON DATEINDEX%  *~
            *************************************************************

        deffn'190(part$)
                 bomid$ = "   "          /* DEFAULT IS NOTHING BOM     */
                 if compbom$ =  " " then L18110
                     bomid$ = compbom$
                     return
L18110:          if dateindex% = 0 then return   /* OFF PROD CALENDER? */
                    readkey2$ = str(part$,,25) & "1"
                    call "READ102" (#11, readkey2$, f1%(11))
                       if f1%(11) <> 1 then return
                    get #11, using L18160, readkey2$, bom$()
L18160:                FMT CH(29), 490 * CH(3)
                    if str(readkey2$,,25) <> str(part$,,25) then return
                    bomid$ = bom$(dateindex%)
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
            mat count% = zer
            l%, pl%, p% = 0
            if prttxt$ = "Y" then np% = 0%
            line% = 1000%  :  page% = 0%
            call "DESCRIBE" (#4, assypart$, assypartdescr$, 1%, f1%(4))
            gosub'190(assypart$)
            assypartkey$ = str(assypart$, 1, 25) & str(bomid$, 1, 3)
            headerkey$ = str(assypartkey$) & "  0"
            gosub L31000
            if compbom$ =  " " then str(title$, 6, 30) = " BOM " &       ~
                 bomid$ & " EFFECTIVE ON " & effdatef$  else             ~
                 str(title$, 6, 30) = " BOM " & bomid$ & " AS SPECIFIED"

            gosub'7(2%, assypartkey$, so_qty)  /* Hang on, here we gooo*/
            if page% <> 0% then print using L19705 /* TAG LINE AT END*/
            return                       /* GET NEXT PART TO BOM       */

L19125:     REM ROUTINE TO PRINT ENTRY JUST LOADED.
                print$() = " "
                print$(3) = component$
                if str(bommkr$,,1) = "P" then call "PUTPAREN" (print$(3))
                put str(print$(2), min(3*l%-2,43), 30),                  ~
                        using L19155, count%(l%,1), print$(3)
L19155:                       %###. #########################

                call"CONVERT"(netqty, 0.2, str(print$(3),,10))
                if str(bommkr$,,1) <> "P" then L19190
                     print$(1) = "TOP"
                     convert p%+1 to str(print$(1),4,3), pic(###)
                     goto L19195
L19190:         if p% > 0 then convert p% to str(print$(1),4,3), pic(###)
L19195:         print$(4) = opflag$
                print$(5) = bomid$
                print$(6) = rteid$ : if print$(5)=" " then print$(6)=" "
                if bommkr$ = "ST" then L19240
                search mkr$() = bommkr$ to cursor%() step 2
                     if cursor%(1) = 0 then print$(7) = " "  else        ~
                                    print$(7) = mkrdes$((cursor%(1)+1)/2)
L19240:         gosub L19395              /* PAGE CONTROL SUBROUTINE    */
                print using L19750, print$(1), print$(2), print$(3),      ~
                                   print$(4), print$(5), print$(6),      ~
                                   print$(7)

                call "DESCRIBE" (#4, component$, print$(10), 0%, f1%(4))
                if str(bommkr$,,1)="P" then call "PUTPAREN" (print$(10))
                print$(2) = " "
                str(print$(2), min(3*l%+3,43)) = print$(10)
                gosub L19395   /* PAGE CONTROL */
                print using L19750," ", print$(2), " ", " ", " ", " ", " "
                if prtref$ = "Y" then gosub locations
                if prttxt$ <> "Y" then return


                REM Print any component text that was entered...
                dis% = min(3*l%+4,44)
                stat% = 0%
                didit% = line%
L19335:         call "TXTPRINT"(#12, f2%(12), 134%, textidl$, "C",       ~
                      dis%, line%, 60%, "N", mask$, stat%)
                if stat% = 0% then L19370
                gosub L19395
                didit% = 1234567%
                goto L19335

L19370:         if didit% = line% then return /* No text was printed */
                gosub L19395
                print using L19750, " ", " ", " ", " ", " ", " ", " "
                return

L19395:     REM PAGE HEADER CONTROL ROUTINE.
                select printer(134)
                line% = line% + 1%
                if line% < 59 then return
                   if page% > 0% then print using L19705
                   print page
                   page% = page% + 1
                   print using L19680, page%, assypart$, assypartdescr$,  ~
                                      date$
                   print using L19690,str(title$,6,30), so$    , so_line$,~
                                     cuscode$, qty$
                   print
                   print using L19705
                   print using L19720
                   print using L19735
                   print using L19705
                   line% = 7
                   if printing_locations% = 0 then return
                      print using L19785
                      print using L19770
                      line% = 9
                      return

          locations              /* SHOW ASSOCIATED LOCATION(S) IF ANY */
                didit% = 1%
                printing_locations% = 0%
                print$(), str(ref_key$,57) = " "
L19530:         call "PLOWNEXT" (#6, str(ref_key$,26%), 31%, f1%(6))
                       if f1%(6) = 0 then L19605
                if printing_locations% <> 0 then L19570
                  printing_locations% = 1
                  np% = page%
                  gosub L19395 :if page%<>np% then L19570:print using L19770
                  gosub L19395 :if page%<>np% then L19570:print using L19785
                  gosub L19395 :if page%<>np% then L19570:print using L19770
L19570:         printing_locations% = 1
                gosub L35070
                print$(didit%*2-1) = location$
                call"CONVERT"(quantity_here,0.2,str(print$(didit%*2),,7))
                didit% = didit% + 1%
                if didit% = 6 then gosub print_location_line
                goto L19530
L19605:         if printing_locations% = 0 then return
                   if didit% > 1% then gosub print_location_line
                   printing_locations% = 0
                   print using L19820
                   gosub L19395    /* Form Control */
                   return

            print_location_line
                gosub L19395    /* Form Control */
                print using L19800, print$(1), print$(2), print$(3),      ~
                        print$(4), print$(5), print$(6), print$(7),      ~
                        print$(8), print$(9), print$(10)
                didit% = 1% : print$() = " "
            return

L19680: %PAGE#####   P A R T S   E X P L O S I O N   F O R: #############~
        ~############ ##################################          ########
L19690: %            USING ##############################       SO: #####~
        ~########### LINE:###   CUS: ######### QTY: ##########

L19705: %+-------+-------------------------------------------------------~
        ~----------------------+----------+--------------+---+---+--------~
        ~--+
L19720: %!PHANTOM!           C O M P O N E N T   P A R T S               ~
        ~                      ! QUANTITY ! OPTION PART  !BOM!RTE!MARKER I~
        ~F !
L19735: %! LEVEL ! NOTE-SEE LAST COLUMN FOR NON-STANDARD COMPONENTS (PHAN~
        ~TOM, OPTION, ETC.)    !   NEED   ! IF NON-BLANK !ID.!ID.!NOT STAN~
        ~D !
L19750: %!###### !#######################################################~
        ~######################!##########!##########    !###!###!########~
        ~##!

L19770: %!                                                       +-------~
        ~-------+--------------+--------------+--------------+------------~
        ~--+
L19785: %!                                         REFERENCES >> !LOCATIO~
        ~N   QTY!LOCATION   QTY!LOCATION   QTY!LOCATION   QTY!LOCATION   Q~
        ~TY!
L19800:  %!                                                       !######~
        ~ #######!###### #######!###### #######!###### #######!###### ####~
        ~###!

L19820: %!                                                       +-------~
        ~----------------------+----------+--------------+---+---+--------~
        ~--+

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

        deffn'161(fieldnr%)
                  enabled% = 0
                  inpmessage$ = " "
                  on fieldnr% gosub L20130,         /* Sales Order      */~
                                    L20180,         /* SO Line          */~
                                    L20220          /* Qty Type Flag    */
                     return
L20130:     REM DEFAULT/ENABLE FOR SO CODE
                enabled% = 1
                inpmessage$ = "To Print Explosions For A Range Of Sales O~
        ~rders/Sales Order Lines, Press (9)."
                return
L20180:     REM DEFAULT/ENABLE FOR SO  LINE
                enabled% = 1
                inpmessage$ = "Select Line To Explode."
                return
L20220:     REM DEFAULT/ENABLE FOR Quantity Type Flag      QTY_TYPE$
                inpmessage$ = "Enter Type of Quantity to Explode BOM agai~
        ~nst."
                enabled% = 1
                return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 2 OF INPUT. *~
            *************************************************************

        deffn'162(fieldnr%)
                  enabled% = 0
                  inpmessage$ = " "
                  on fieldnr% gosub L21140,         /* SO Range         */~
                                    L21190,         /* Part Range       */~
                                    L21240,         /* Text Print Flag  */~
                                    L21290,         /* Refr Print Flag  */~
                                    L21340          /* Qty Type Flag    */
                     return
L21140:     REM DEFAULT/ENABLE FOR SO RANGE
                enabled% = 1
                inpmessage$ = "Enter Range Of Sales Orders To Explode Ass~
        ~emblies For."
                return
L21190:     REM DEFAULT/ENABLE FOR PART RANGE
                enabled% = 1
                inpmessage$ = "Only S.O. Lines With Parts Within This Ran~
        ~ge Will Be Printed."
                return
L21240:     REM DEFAULT/ENABLE FOR TEXT PRINT FLAG
                inpmessage$ = "Enter 'Y' to include component Text on Bil~
        ~l of Material."
                enabled% = 1
                return
L21290:     REM DEFAULT/ENABLE FOR REFERENCE PRINT
                inpmessage$ = "Enter 'Y' to Print Reference Locations."
                enabled% = 1
                return

L21340:     REM DEFAULT/ENABLE FOR Quantity Type Flag      QTY_TYPE$
                inpmessage$ = "Enter Type of Quantity to Explode BOM agai~
        ~nst."
                enabled% = 1
                return

        REM *************************************************************~
            *               HEADER TEXT PRINT ROUTINE                    ~
            *************************************************************
        header_text
            select printer(134)
            if textidh$ = hex(20202020) or                               ~
               textidh$ = hex(00000000) or                               ~
               textidh$ = hex(ffffffff) then L25220
            lcntr% = 5%
L25090:     print page
            print skip(2)
            headline$ = "ASSEMBLY " & assypart$ & " BOMID "& bomid$
            print using L25130, headline$
L25130: %BILL OF MATERIAL TEXT FOR ######################################~
        ~#############
            print skip(2)
            call "TXTPRINT" (#12, f3%, 134%, textidh$, "GP", 10%,        ~
                              lcntr%, 60%, "Y", " ", stat%)
            if stat% = 0% then L25210
            goto L25090

L25210:     np% = 1%
L25220:     return

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *                                                           *~
            * GIVES THE USER THE ABILITY TO START OVER WHEN HE WANTS TO *~
            * ELSE RETURN TO THE MENU.  NOTICE THAT HE HAS TO PUSH 2    *~
            * DIFFERENT BUTTONS TO START OVER--A LITTLE HARDER.         *~
            *************************************************************

        startover: REM ALLOW USER OPPORTUNITY TO START OVER.

            call "STARTOVR" (keyhit1%)
            if keyhit1% = 1% then return

            so$ = " "
            return clear all
            goto inputmode

L30000: REM *************************************************************~
            * L O A D   B I L L   O F   M A T E R I A L S   R E C O R D *~
            *                                                           *~
            * LOADS THE BILL OF MATERIALS RECORD, WITH ITS ALL-IMPORTANT*~
            * QUANTITY AND SIZE FIELDS FROM THE BILL OF MATERIALS FILE. *~
            *************************************************************

            get #5, using L30130, component$, quantity, xused, fixed,     ~
                                 over, bommkr$, op$, compbom$, textidl$
            netqty = quantity * xused + over
            get #5, str(ref_key$,,56)
            return

L30130:     FMT CH(25),                  /* COMPONENT PART NUMBER      */~
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
                CH(4)                    /* Line Item Text ID          */

L31000: REM *************************************************************~
            * L O A D   B I L L   O F   M A T E R I A L S   R E C O R D *~
            *                                                           *~
            * LOADS THE BILL OF MATERIALS RECORD, WITH ITS ALL-IMPORTANT*~
            * QUANTITY AND SIZE FIELDS FROM THE BILL OF MATERIALS FILE. *~
            *************************************************************

            rteid$ = " "
            call "READ100" (#5, headerkey$, f1%(5%))
            if f1%(5%) = 1% then get #5, using L31100, rteid$
            return

L31100:     FMT XX(56),                  /* Key Area                   */~
                XX(30),                  /* DESCRIPTION                */~
                CH(3),                   /* WHICH ROUTE                */~
                CH(4),                   /* TEXTID Header              */~
                CH(1),                   /* DRAWING LOCATION           */~
                CH(2),                   /* DRAWING REVISION LEVEL     */~
                2*BI(3),                 /* CREATE FOLLOWED BY MOD DATE*/~
                CH(3),                   /* LAST USERS ID              */~
                CH(45)                   /* Filler Rest Record         */

        REM *************************************************************~
            *             L O A D   S A L E S   O R D E R   D A T A     *~
            *                                                           *~
            * LOADS PERTINENT DATA OFF THE BCKLINES FILE.               *~
            *************************************************************
        load_so_data
            get #16, using L32100, cuscode$, assypart$, org_qty, ship_qty,~
                                 open_qty, effdatef$, temp$, assmbom$
            compbom$ = assmbom$
            return
L32100:         FMT  CH(9), POS(32), CH(25), POS(93), PD(14,4), PD(14,4),~
                    PD(14,4), POS(200), CH(6), POS(212), CH(6), POS(263),~
                    CH(3)

L33000: REM *************************************************************~
            *          L O A D   O P T I O N   S E L E C T E D          *~
            *                                                           *~
            * LOADS THE REPLACEMENT PART FOR CURRENT OPTION PART.       *~
            *************************************************************

            opflag$ = "UNSELECTED"
            readkey2$ = " "
            put readkey2$, using L33090,so$,so_line$,l%,count%(l%,2)
L33090:     FMT CH(16), CH(03), 2*BI(2)

            call "REDALT0" (#7, readkey2$, 1%, f1%(7))
                if f1%(7) = 0 then return
            readkey2$ = component$
            opflag$ = "*BAD DATA*"
            if str(key(#7,0),,31) <> p$(pl%) then return
            get #7, using L33260, component$, quantity, xused

            opflag$ = " REPLACED"
            if component$ <> " " then L33230
                opflag$ = "*NOT USED*"
                component$ = readkey2$
                quantity, over, fixed = 0
L33230:     netqty = quantity * xused + over
        return

L33260:     FMT XX(25),                  /* COMPONENT PART NUMBER      */~
                XX(25),                  /* ASSEMBLY PART NUMBER       */~
                XX(3),                   /* BOM STRUCTURE ID           */~
                XX(3),                   /* BOM SEQUENCE NUMBER        */~
                XX(19),                  /* SO     CODE & LINE         */~
                XX(4),                   /* POINTER                    */~
                CH(25),                  /* REPLACMENT PART            */~
                PD(14,4),                /* QUANTITY USED              */~
                PD(14,4)                 /* SIZE (TIMES USED)          */~

        REM *************************************************************~
            *       L O A D   R E F E R E N C E   L O C A T I O N       *~
            *                                                           *~
            * LOADS THE LOCATION REFERENCE ASSOCIATED WITH THIS ASSY    *~
            * ALSO HAS QUANTITY AT THIS LOCATION.                       *~
            *************************************************************

L35070:     get    #06, using L35110,                                     ~
                       location$, quantity_here
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

        deffn'201(fieldnr%, edit%)
                gosub set_pf1
                if fieldnr% > 0% then init(hex(84)) lfac$()              ~
                                 else init(hex(86)) lfac$()
                str(header$,62) = "BOMEXPSO: " & cms2v$

                  on fieldnr% gosub L40130,         /* SO     Code      */~
                                    L40145,         /* SO     Line      */~
                                    L40130          /* Qty Type Flag    */
                  goto L40165

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40130:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40145:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40165:     accept                                                       ~
               at (01,02), "Review Sales Order Assembly Explosions",     ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Sales Order Number",                         ~
               at (06,22), fac(lfac$( 1)), so$                  , ch(16),~
               at (06,42), fac(hex(8c)),   so_descr$            , ch(32),~
                                                                         ~
               at (07,02), "Sales Order Line",                           ~
               at (07,22), fac(lfac$( 2)), so_line$             , ch(03),~
                                                                         ~
               at (08,02),"Quantity Type (1 = Original Order Quantity) ",~
               at (09,02),"              (2 = Current Order Quantity)  ",~
               at (10,02),"              (3 = Current Open Quantity)   ",~
               at (11,02),"              (4 = Only One Assembly Part)  ",~
               at (08,50), fac(lfac$( 3)), qty_type$            , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pfktext$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfktext$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfktext$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13% then L40325
                  call "MANUAL" ("BOMEXPSO")
                  goto L40165

L40325:        if keyhit% <> 15% then L40345
                  call "PRNTSCRN"
                  goto L40165

L40345:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
            if edit% = 2% then L40455
                pfktext$(1) = "(1)Start Over           (4)Previous Field ~
        ~                     (13)Instructions"
                pfktext$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfktext$(3) = "                        (9)Print Explosion~
        ~s                    (16)Exit Program"
                pfkeys$ = hex(000104090d0f10)
                if fieldnr% <> 1% then L40435
                    str(pfktext$(1%),24%,18%) = " "
                    str(pfkeys$,3%,1%)        = hex(ff)
                    return

L40435:             str(pfktext$(3%),64%,16%) = " "
                    str(pfkeys$,7%,1%)        = hex(ff)
                    return

L40455:     if fieldnr% > 0% then L40505
                pfktext$(1) = "(1)Start Over                             ~
        ~                     (13)Instructions"
                pfktext$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfktext$(3) = "                                          ~
        ~                     (16)Display BOM "
                pfkeys$ = hex(00010d0f10)
                return

L40505:         pfktext$(1) = "(1)Start Over                             ~
        ~                     (13)Instructions"
                pfktext$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfktext$(3) = "                                          ~
        ~                                     "
                pfkeys$ = hex(00010d0f)
                return

L41000: REM *************************************************************~
            *       P A R T S   E X P L O S I O N   D I S P L A Y       *~
            *                                                           *~
            * PARTS EXPLOSION DISPLAY SCREEN.  NO BIG DEAL, SINCE ALL   *~
            * THE COLUMNS ARE FORMATTED IN THE PRINT ROUTINE.           *~
            *************************************************************

L41070:     accept                                                       ~
               at (01,02),                                               ~
                  "(2)First  (5)Next      (14)Print Explosion (15)Print S~
        ~creen  (16)Next Assembly",                                       ~
               at (02,05),                                               ~
                  "Or Position Cursor To Line And Press (RETURN) to See R~
        ~eference Locations",                                             ~
               at (03,02), fac(hex(84)), titledescr$            , ch(79),~
               at (04,02), fac(hex(a4)), title$                 , ch(79),~
               at (05,02), fac(hex(8c)), line$( 1%)             , ch(79),~
               at (06,02), fac(hex(8c)), line$( 2%)             , ch(79),~
               at (07,02), fac(hex(8c)), line$( 3%)             , ch(79),~
               at (08,02), fac(hex(8c)), line$( 4%)             , ch(79),~
               at (09,02), fac(hex(8c)), line$( 5%)             , ch(79),~
               at (10,02), fac(hex(8c)), line$( 6%)             , ch(79),~
               at (11,02), fac(hex(8c)), line$( 7%)             , ch(79),~
               at (12,02), fac(hex(8c)), line$( 8%)             , ch(79),~
               at (13,02), fac(hex(8c)), line$( 9%)             , ch(79),~
               at (14,02), fac(hex(8c)), line$(10%)             , ch(79),~
               at (15,02), fac(hex(8c)), line$(11%)             , ch(79),~
               at (16,02), fac(hex(8c)), line$(12%)             , ch(79),~
               at (17,02), fac(hex(8c)), line$(13%)             , ch(79),~
               at (18,02), fac(hex(8c)), line$(14%)             , ch(79),~
               at (19,02), fac(hex(8c)), line$(15%)             , ch(79),~
               at (20,02), fac(hex(8c)), line$(16%)             , ch(79),~
               at (21,02), fac(hex(8c)), line$(17%)             , ch(79),~
               at (22,02), fac(hex(8c)), line$(18%)             , ch(79),~
               at (23,02), fac(hex(8c)), line$(19%)             , ch(79),~
               at (24,02), fac(hex(8c)), line$(20%)             , ch(79),~
                                                                         ~
               keys(hex(000102050d0e0f10)),                              ~
               key (keyhit%)

               if keyhit% <> 13% then L41440
                  call "MANUAL" ("BOMEXPSO")
                  goto L41000

L41440:        if keyhit% <> 00% then L41490
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

L41490:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L41070

L42000: REM *************************************************************~
            *       R E F E R E N C E               D I S P L A Y       *~
            *                                                           *~
            * IF A LINE ITEM IS SELECTED FROM THE EXPLOSION DISPLAY     *~
            * THIS ACCEPT WILL SHOW THE 'LOCATION DESIGNATORS'          *~
            *************************************************************

L42070:     accept                                                       ~
               at (01,02),                                               ~
                  "(2)First  (5)Next      (14)Print Explosion (15)Print S~
        ~creen   (16)Return To BOM",                                      ~
               at (03,02), fac(hex(84)), titledescr1$           , ch(79),~
               at (04,02), fac(hex(a4)), title1$                , ch(79),~
               at (05,02), fac(hex(8c)), line1$( 1%)            , ch(79),~
               at (06,02), fac(hex(8c)), line1$( 2%)            , ch(79),~
               at (07,02), fac(hex(8c)), line1$( 3%)            , ch(79),~
               at (08,02), fac(hex(8c)), line1$( 4%)            , ch(79),~
               at (09,02), fac(hex(8c)), line1$( 5%)            , ch(79),~
               at (10,02), fac(hex(8c)), line1$( 6%)            , ch(79),~
               at (11,02), fac(hex(8c)), line1$( 7%)            , ch(79),~
               at (12,02), fac(hex(8c)), line1$( 8%)            , ch(79),~
               at (13,02), fac(hex(8c)), line1$( 9%)            , ch(79),~
               at (14,02), fac(hex(8c)), line1$(10%)            , ch(79),~
               at (15,02), fac(hex(8c)), line1$(11%)            , ch(79),~
               at (16,02), fac(hex(8c)), line1$(12%)            , ch(79),~
               at (17,02), fac(hex(8c)), line1$(13%)            , ch(79),~
               at (18,02), fac(hex(8c)), line1$(14%)            , ch(79),~
               at (19,02), fac(hex(8c)), line1$(15%)            , ch(79),~
               at (20,02), fac(hex(8c)), line1$(16%)            , ch(79),~
               at (21,02), fac(hex(8c)), line1$(17%)            , ch(79),~
               at (22,02), fac(hex(8c)), line1$(18%)            , ch(79),~
               at (23,02), fac(hex(8c)), line1$(19%)            , ch(79),~
               at (24,02), fac(hex(8c)), line1$(20%)            , ch(79),~
                                                                         ~
               keys(hex(000102050d0e0f10)),                              ~
               key (keyhit%)

               if keyhit% <> 13% then L42410
                  call "MANUAL" ("BOMEXPSO")
                  goto L42070

L42410:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L42070

        REM *************************************************************~
            * I N P U T   M A X L E V E L S %   F O R   S E L E C T O R *~
            *                                                           *~
            * THIS SCREEN HAS IT SIMPLE--IT JUST GETS TEXT Y/N FOR      *~
            *************************************************************

        deffn'202(fieldnr%, edit%)
                gosub set_pf2
                str(header$,62) = "BOMEXPSO: " & cms2v$
                if fieldnr% > 0% then init(hex(84)) lfac$()              ~
                                 else init(hex(86)) lfac$()
                  on fieldnr% gosub L43130,         /* SALES ORDERS     */~
                                    L43130,         /* PARTS            */~
                                    L43130,         /* TEXT PRINT FLAG  */~
                                    L43130,         /* Refr Print Flag  */~
                                    L43130          /* Qty Type Flag    */
                     goto L43165

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L43130:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L43165:     accept                                                       ~
               at (01,02), "Print Sales Order Assembly Explosions",      ~
               at (01,60), "Todays Date:",                               ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,21), fac(hex(ac)), subtitle$(1%)          , ch(25),~
               at (05,50), fac(hex(ac)), subtitle$(2%)          , ch(25),~
                                                                         ~
               at (06,02), "Sales Order Range:",                         ~
               at (06,21), fac(lfac$( 1%)), so$(1%,1%)          , ch(16),~
               at (06,50), fac(lfac$( 1%)), so$(1%,2%)          , ch(16),~
                                                                         ~
               at (07,02), "Part Number Range:",                         ~
               at (07,21), fac(lfac$( 2%)), part$(1%,1%)        , ch(25),~
               at (07,50), fac(lfac$( 2%)), part$(1%,2%)        , ch(25),~
                                                                         ~
               at (08,02), "Print Free Text On Report?",                 ~
               at (08,32), fac(lfac$( 3%)), prttxt$             , ch(01),~
                                                                         ~
               at (09,02), "Print Reference Designators?",               ~
               at (09,32), fac(lfac$(4%)), prtref$              , ch(01),~
                                                                         ~
               at (10,02),"Quantity Type (1 = Original Order Quantity) ",~
               at (11,02),"              (2 = Current Order Quantity)  ",~
               at (12,02),"              (3 = Current Open Quantity)   ",~
               at (13,02),"              (4 = Only One Assembly Part)  ",~
               at (10,50), fac(lfac$(5%)), qty_type$            , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pfktext$(1%)         , ch(79),~
               at (23,02), fac(hex(8c)),   pfktext$(2%)         , ch(79),~
               at (24,02), fac(hex(8c)),   pfktext$(3%)         , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13% then L43370
                  call "MANUAL" ("BOMEXPSO")
                  goto L43165

L43370:        if keyhit% <> 15% then L43390
                  call "PRNTSCRN"
                  goto L43165

L43390:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
            if edit% = 2% then L43485
                pfktext$(1) = "(1)Start Over           (4)Previous Field ~
        ~                     (13)Instructions"
                pfktext$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfktext$(3) = "                                          ~
        ~                     (16)Return      "
                pfkeys$ = hex(0001040d0f10)
                if fieldnr% <> 1% then return
                     str(pfkeys$,3%,1%)          = hex(ff)
                     str(pfktext$(1%),24%,18%)   =  " "
                return
                               /* EDIT Mode   - SELECT  */
            if field% > 0% then L43535
L43485:         pfktext$(1) = "(1)Start Over                             ~
        ~                     (13)Instructions"
                pfktext$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfktext$(3) = "                                          ~
        ~                     (16)Explode BOMs"
                pfkeys$ = hex(00010d0f10)
                return

                               /* EDIT Mode   - ENABLED */
L43535:         pfktext$(1) = "(1)Start Over                             ~
        ~                     (13)Instructions"
                pfktext$(2) = "                                          ~
        ~                     (15)Print Screen"
                pfktext$(3) = "                                          ~
        ~                                     "
                pfkeys$ = hex(00010d0f)
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

        deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50120,         /* SO     Code      */~
                                    L50280,         /* SO     Line      */~
                                    L50680          /* Qty Type Flag    */
                     return
L50120:     REM TEST DATA FOR SO CODE
                if so$ = "?" then so$ = " "
                readkey$ = so$
                hdr$(1%) = " "
                hdr$(2%) = "  Sales Order        Customer Name"
                hdr$(3%) = hex(ac) & "Select the Sales Order to be review~
        ~ed by using Cursor & RETURN, PF16 to Exit"
                so_descr$ = hex(06) & "Select the Order to Explode"
            call "PLOWCODE" (#16, readkey$, so_descr$, -3016%, -0.09,    ~
                                  f1%(16%), hdr$(), 0, 1)
                     if f1%(16%) = 0% then L50260
                so$ = readkey$
                call "PUTPAREN" (so_descr$)
                return
L50260:         errormsg$ = "Unknown Sales Order Number: " & so$
                return

L50280:    REM TEST DATA FOR SO LINE
                if so_line$ = " " then L50320
                   call "NUMTEST" (so_line$, 0,999, errormsg$, -0.001, 0)
                     if errormsg$ <> " " then return
L50320:         readkey$ = str(so$) & str(so_line$)
                hdr$(1%) = "  Line       Part Number"
                hdr$(2%) = " "
                hdr$(3%) = hex(ac) & "Select the Sales Order Line by usin~
        ~g Cursor & RETURN, PF16 to Exit"
                errormsg$ = hex(06) & "Select the Line to Explode"
            call "PLOWCODE" (#16, readkey$, errormsg$,  3016%, -0.25 ,   ~
                                  f1%(16%), hdr$(), 0.25, 32)
                     if f1%(16%) = 0% then L50580
                so_line$ = str(readkey$,17%, 3%)
                gosub load_so_data

            format_so_data
                errormsg$ = " "
                if temp$ <> " " and temp$ <> blankdate$ then effdatef$ = temp$
                effdate$ = effdatef$
                call "DATEFMT" (effdatef$)
                call "PIPINDEX" (#3, effdate$, dateindex%, ret%)
                   if ret% <> 1% then return
                errormsg$ = "Planning Calander Base Date not Found.  Exit~
        ~ & Correct."
                return
L50580:         errormsg$ = "Invalid Order Line Number: " & so_line$
                return
L50600:    REM TEST DATA FOR TEXT PRINT FLAG
                if prttxt$ = " " then prttxt$ = "N"
                if pos("YN" = prttxt$) = 0 then L50640
                return
L50640:         errormsg$ = "Please Enter 'Y' or 'N'"
                return

           REM TEST DATA FOR Quantity Type Flag         QTY_TYPE$
        set_quantity
L50680:         if qty_type$ = " " then qty_type$ = "4"
                on pos("1234" = qty_type$) goto L50720, L50730, L50740, L50750
                   errormsg$ = "Please Enter Type: '1', '2', '3' or '4'."
                   return
L50720:         so_qty = org_qty               :   goto L50760
L50730:         so_qty = open_qty + ship_qty   :   goto L50760
L50740:         so_qty = open_qty              :   goto L50760
L50750:         so_qty = 1                     :   goto L50760
L50760:            call "CONVERT" (so_qty, -2.2, qty$)
                   return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

        deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51130,         /* SO Range         */~
                                    L51170,         /* Part Range       */~
                                    L50600,         /* Text Print Flag  */~
                                    L51210,         /* Text Print Flag  */~
                                    L51280          /* Qty Type Flag    */
                     return
L51130:     REM TEST DATA FOR SO RANGE
                call "TESTRNGE" (so$(1,1), so$(1,2), so$(2,1), so$(2,2), ~
                                                        errormsg$)
                return
L51170:     REM TEST DATA FOR PART RANGE
                call "TESTRNGE" (part$(1,1), part$(1,2), part$(2,1),     ~
                                             part$(2,2), errormsg$, #04)
                return
L51210:    REM TEST DATA FOR TEXT PRINT FLAG
                if prtref$ = " " then prtref$ = "N"
                if pos("YN" = prtref$) = 0 then L51250
                return
L51250:         errormsg$ = "Please Enter 'Y' or 'N'"
                return

L51280:    REM TEST DATA FOR Quantity Type Flag         QTY_TYPE$
                if qty_type$ = " " then qty_type$ = "4"
                if pos("1234" = qty_type$) <> 0% then return
                   errormsg$ = "Please Enter Type: '1', '2', '3' or '4'."
                   return


L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("One Moment Please")
            end
