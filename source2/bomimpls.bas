        REM *************************************************************~
            *                                                           *~
            *  BBBB    OOO   M   M  IIIII  M   M  PPPP   L       SSS    *~
            *  B   B  O   O  MM MM    I    MM MM  P   P  L      S       *~
            *  BBBB   O   O  M M M    I    M M M  PPPP   L       SSS    *~
            *  B   B  O   O  M   M    I    M   M  P      L          S   *~
            *  BBBB    OOO   M   M  IIIII  M   M  P      LLLLL   SSS    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMIMPLS - PARTS WHERE USED LIST PROGRAM.                 *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/17/81 ! ORIGINAL                                 ! BCW *~
            * 07/02/81 ! ADDED CLOSE PRINTER COMMAND FOR KEY 14   ! TOM *~
            * 07/20/81 ! CORRECT TO GET ALL OCCURANCES OF A PART  ! GLW *~
            * 09/20/82 ! ADD BOM MARKER                           ! GLW *~
            * 07/13/83 ! CALL TO 'MANUAL' ADDED                   ! HES *~
            * 07/13/83 ! Changed file open subroutines            ! HES *~
            * 09/22/86 ! BOMMASTR FORMAT CHANGED                  ! LKM *~
            * 05/13/87 ! HNYMASTR record length mod for Std Cost  ! JIM *~
            * 02/03/88 ! Program brought up to standards          ! RJM *~
            * 02/05/88 ! Added Options ie, ALL or EFFECTIVE BOM's ! RJM *~
            * 02/10/88 ! Added 'Where Used as Option' Option      ! RJM *~
            * 02/23/89 ! Changed Part Validation in Test Data     ! MJB *~
            *          !  Corrected Option Search Logic           !     *~
            * 07/24/89 ! Changed Range Print logic to do Options  ! MJB *~
            * 11/07/91 ! CMS/DEC 'MASK' Project                   ! SID *~
            * 02/27/92 ! Minor mods for DEC Compatibility.        ! JDH *~
            * 03/02/92 ! RePositioned BOM Sequence Number when    ! SID *~
            *          !   checking for the header record thru    !     *~
            *          !   BOMMASTR alternate key.                !     *~
            * 04/21/92 ! PRR 12398 Fixed 'ALL' range option.      ! JDH *~
            *          ! PRR 12125 Fixed Text Printing.           !     *~
            * 10/14/94 ! PRR 13303 Fixed range selection prompt   ! RJH *~
            *          !   overlay problem (UNIX) by adding chanel!     *~
            *          !   to testrang call (no internal getcode).!     *~
            *************************************************************

        dim                                                              ~
            assembly$25,                 /* ASSEMBLY PART NUMBER       */~
            assystring$114,              /* REPORT HEADING             */~
            ax$25,                       /* TEMP ASSEMBLY              */~
            bom$(490)3,                  /* BOM LIST                   */~
            bom_comp$25,                 /* COMPONENT FROM BOM         */~
            bomid$3,                     /* BOM STRUCTURE ID           */~
            bomid_save$3,                /* BOM STRUCTURE ID           */~
            bom_eff_flag$1,              /* FOR MARKING EFFECTIVE BOM  */~
            bommkr$2,                    /* BOM MARKER                 */~
            company$60,                  /* COMPANY NAME FOR TITLE     */~
            compbom$3,                   /* COMPONENT BOM              */~
            comppart$25,                 /* ASSEMBLY PART NUMBER       */~
            comppartdescr$34,            /* ASSEMBLY PART DESCRIPTION  */~
            count%(15),                  /* ITEM COUNT OF 15 LEVELS    */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDIT   */~
            date$8,                      /* CALENDAR DATE FOR SHOW OFF */~
            edtmessage$79,               /* EDIT SCREEN MESSAGE        */~
            effdatef$8,                  /* SCREEN BOM EFF. DATE       */~
            errormsg$79,                 /* ERROR MESSAGE TEXT INFO    */~
            header$79,                   /* SCREEN TITLE               */~
            i$(24)80,                    /* SCREEN IMAGE (not used)    */~
            infomsg$79,                  /* INFORMATIVE MESSAGE TEXT   */~
            inpmessage$79,               /* INPUT MESSAGE TEXT INFO    */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line$(20)79,                 /* TEXT TO DISPLAY ON SCREEN  */~
            maxlevels$2,                 /* MAX # OF LEVELS TO SHOW    */~
            mstrpart$25,                 /* CURRENT COMPONENT IN RANGE */~
            mstrpartdesc$34,             /* CURR COMPONENT NAME        */~
            mkr$(20)2,                   /* ALLOWED BOM MARKERS        */~
            mkrdes$(20)10,               /* ALLOWED BOM MARKERS        */~
            msg$(3)80,                   /* ASKUSER MESSAGES           */~
            nmbr$3,                      /* LINE NUMBER FORMAT STRING  */~
            op$1,                        /* COMPONENT OPTION FLAG      */~
            opassy$25,                   /* ASSEMBLY FROM OPTIONS FILE */~
            opbom$3,                     /* SPECIFIC BOM (OPTIONS FILE)*/~
            opflag$1,                    /* LOOK FOR PART AS OPTION?   */~
            oppart$25,                   /* COMP. PART (OPTIONS FILE)  */~
            reppart$25,                  /* REPLACEMENT (OPTIONS FILE) */~
            p$(15)60,                    /* READ KEYS FOR 15 LEVELS    */~
            part$25,                     /* DUMMY ARGUMENT PART #      */~
            part2$25,                    /* DUMMY ARGUMENT PART #      */~
            partr$(4)25,                 /* PART NUMBER RANGE          */~
            partnm$(2)34,                /* PART NUMBER RANGE NAMES    */~
            pfkeys$32,                   /* FUNCTION KEYS ENABLE LIST  */~
            pfktext$(3)79,               /* FUNCTION KEYS ENABLE LIST  */~
            pheader$114,                 /* HEADING FOR RANGE REPORT   */~
            print$(4)80,                 /* ARRAYS TO PRINT INFO WITH  */~
            prttxt$1,                    /* INCLUDE BOM TEXT ?         */~
            readkey$60,                  /* KEY FOR PLOW ROUTINES      */~
            readkey2$60,                 /* KEY FOR PLOW ROUTINES      */~
            readkey3$60,                 /* KEY FOR PLOW ROUTINES      */~
            readkey4$60,                 /* KEY FOR PLOW ROUTINES      */~
            temp$5,                      /*                            */~
            tempdescr$34,                /* TEMPORARY PART DESCRIPTION */~
            textidl$4,                   /* LINE ITEM TEXT ID          */~
            textidh$4,                   /* HEADER    TEXT ID          */~
            time$8,                      /* SYSTEM TIME FOR TITLES     */~
            title$79,                    /* TITLE FOR DISPLAY ENTRIES  */~
            title$(2)30,                 /* SCREEN COLUMN HEADINGS     */~
            titledescr$79                /* SHOW WHAT PART WE'RE DOING */~

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64)                      /* RECORD-ON-FILE FLAGS       */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.03.02 01/17/95 'A' PRRs & Critical Problems    "
        REM *************************************************************
            mat f2% = con

                     /* THE VARIABLE  F2%()            SHOULD NOT BE   */
                     /* MODIFIED.     IT IS  AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 3 ! SYSFILE2 ! CAELUS MANAGEMENT SYSTEM INFORMATION     *~
            * # 4 ! HNYMASTR ! INVENTORY MASTER FILE (NAME, DESCRIPTION)*~
            * # 5 ! BOMMASTR ! BILL OF MATERIALS RELATIONSHIP FILE      *~
            * # 6 ! HNYOPTNS ! PART OPTIONS FILE                        *~
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
                                   key 2, keypos = 90,  keylen = 4, dup

            select # 5, "BOMMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos =  26, keylen = 31,                      ~
                         alt key  1, keypos = 1, keylen = 56

            select # 6, "HNYOPTNS",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 100,                                  ~
                         keypos =  1, keylen = 54


            select #11, "ENGMASTR"                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2015,                                 ~
                         keypos = 1, keylen = 29

            select #12, "TXTFILE",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos = 1, keylen = 11

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 3, 0%, f2%( 3), 0%, " ")
            call "OPENCHCK" (# 4, 0%, f2%( 4), 0%, " ")
            call "OPENCHCK" (# 5, 0%, f2%( 5), 0%, " ")
            call "OPENCHCK" (# 6, 0%, f2%( 6), 0%, " ")
            call "OPENCHCK" (#11, 0%, f2%(11), 0%, " ")

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * INITIALIZES ALL THE VARIABLES NEEDED TO DO THE REPORT.    *~
            *************************************************************

            date$ = date
            call "DATEFMT" (date$)
            time$ = " "
            call "TIME" (time$)
            err% = 0%
            call "COMPNAME" (12%, company$, err%)

        REM Load Up BOM Markers Table...
            readkey$ = "TABLE01:"
L09140:     call "PLOWNEXT" (#3, readkey$, 8%, f1%(3))
                if f1%(3) = 0% then L09280
            bm% = bm% + 1%
            get #3, using L09180, mkr$(bm%), mkrdes$(bm%)
L09180:     FMT XX(8), CH(2), XX(40), CH(10)
            if bm% < 20% then L09140


L09280:     edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            title$ = "( ) after assmbly shows non-std use in that assmbly~
        ~         QUANTITY       SIZE"
          /*  ! QUANTITY !   SIZE   "   */

            title$(1) = "FROM"  :  title$(2) = "TO  "


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *                                                           *~
            * INPUT MODE MAIN PROGRAM.                                  *~
            *************************************************************

        inputmode
            init(" ") errormsg$, inpmessage$, comppart$, maxlevels$,     ~
                      effdatef$, effdate$, bomid$, prttxt$, bomid_save$, ~
                      opflag$, bom_eff_flag$, assystring$, pheader$,     ~
                      comppartdescr$, mstrpart$, mstrpartdesc$, textidl$

            prange% = 0%

L10100:     for fieldnr% = 1% to  5%
                gosub'161(fieldnr%)
                      if enabled% = 0% then L10260
L10130:         gosub'201(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         if fieldnr% = 1% then L10100
                         gosub'161(fieldnr%)
                         if enabled% <> 0% then L10130
                         goto L10160
L10210:               if keyhit%  =  9% then       L11000     /* PRINT  */
                      if keyhit%  = 16% and fieldnr% = 1% then L65000
                      if keyhit% <>  0% then       L10130
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L10130
L10260:     next fieldnr%

        REM *************************************************************~
            *      A L L O W   E D I T   O F   S E L E C T I O N        *~
            *-----------------------------------------------------------*~
            *   also allows option to display or print the Implosion    *~
            *************************************************************

        editmode
            fieldnr% = 0%
            inpmessage$ = edtmessage$

            gosub'201(fieldnr%)
                if keyhit% = 1% then  gosub startover
                if keyhit% = 14% then goto report_print
                if keyhit% = 16% then goto display_bom     /* DISPLAY  */
                if keyhit% <> 0% then goto editmode

            fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 5% then goto editmode

            gosub'161(fieldnr%)
                if enabled% = 0% then goto editmode
L10470:     gosub'201(fieldnr%)
                if keyhit% = 1% then  gosub startover
                if keyhit% <> 0% then      L10470
            gosub'151(fieldnr%)
                if errormsg$ <> " " then goto L10470

            goto editmode


        REM *************************************************************~
            *           PRINT REPORT FOR A SINGLE COMPONENT             *~
            *************************************************************

        report_print
           select printer (134)
           call "SETPRNT" ("BOM005", " ", 0%, 0%)
           infomsg$ = "Printing Where Used List For" & hex(84) & comppart$

           call "SHOSTAT" (infomsg$)
           gosub'200(comppart$)

           if page% > 0% then L10560
                gosub page_head
                print skip(2)
                print using L60400
L10560:         print skip(2)
                print using L60290
           if opflag$ = "Y" then gosub'80(comppart$)   /* Option Check */

           if page% > 0% then L10600
                print skip(2)
                print using L60430
L10600:    print skip(2)
           time$ = " " : call "TIME" (time$)
           print using L60280, time$
           close printer
           call "SETPRNT" ("BOM005", " ", 0%, 1%)
           goto inputmode


L11000: REM *************************************************************~
            *           I N P U T   P R I N T   R A N G E               *~
            *-----------------------------------------------------------*~
            * Also gets the maximum number of levels to go for.         *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, partr$(), partnm$()
            prange% = 1%

L11080:         for fieldnr% = 1% to 5%
                    gosub'162(fieldnr%)
                          if enabled% = 0% then L11230
L11110:             gosub'202(fieldnr%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L11190
L11140:                  fieldnr% = max(1%, fieldnr% - 1%)
                         if fieldnr% = 1% then L11080
                         gosub'162(fieldnr%)
                         if enabled% <> 0% then L11110
                         goto L11140
L11190:               if keyhit%  = 16% then       inputmode
                      if keyhit% <>  0% then       L11110
                    gosub'152(fieldnr%)
                          if errormsg$ <> " " then L11110
L11230:         next fieldnr%


        REM *************************************************************~
            *      A L L O W   E D I T   O F   S E L E C T I O N        *~
            *-----------------------------------------------------------*~
            *   also allows option to display or print the Implosion    *~
            *************************************************************

        editmode2
            fieldnr% = 0%
            inpmessage$ = edtmessage$

            gosub'202(fieldnr%)
                if keyhit% = 1% then  gosub startover
                if keyhit% = 16% then goto L13000       /* PRINT REPORT */
                if keyhit% <> 0% then goto editmode2

            fieldnr% = cursor%(1%) - 6%
            if fieldnr% < 1% or fieldnr% > 5% then goto editmode2

            gosub'162(fieldnr%)
                if enabled% = 0% then goto editmode2
L11480:     gosub'202(fieldnr%)
                if keyhit% = 1% then  gosub startover
                if keyhit% <> 0% then      L11480
            gosub'152(fieldnr%)
                if errormsg$ <> " " then goto L11480

            goto editmode2


        REM *************************************************************~
            *          D I S P L A Y   P L O W   R O U T I N E          *~
            *                                                           *~
            * PLOWS THROUGH THE WHERE USED LIST FOR THE COMPONENT PART  *~
            * NUMBER AND PRINTS.                                        *~
            *************************************************************

        display_bom

            call "SHOSTAT" ("Finding Where Part is Used, " &             ~
                            "One Moment Please.")

L12082:     l%, displayline% = 0%
            init(" ") line$()

            REM SET TITLE DESCRIPTION FOR SCREEN SHOWING.
                titledescr$ = " "
                str(titledescr$,  6) = "WHERE USED LIST FOR: " &         ~
                                       comppart$ & "  " & comppartdescr$

            gosub'7(comppart$)           /* P$ = PART NUMBER TO BOM    */
            if displayline% <> 0% then gosub L12800  /* DISPLAY PARTIAL */
            goto inputmode                   /* GET NEXT PART TO BOM   */

            deffn'7(part$)
                  l% = l% + 1%
                  p$(l%) = str(part$,,25) & hex(00)
                  count%(l%) = 0%        /* Sequence Number For Print */

L12230:           call "PLOWALTS" (#5, p$(l%), 1%, 25%, f1%(5))
                       if f1%(5) = 0% then L12330
                  if str(p$(l%),54,3) = "  0" then L12230 /* Header ? */
                  gosub L30000              /* Load Bom Record & Info. */
                  bomid_save$ = bomid$
                  gosub'190(assembly$)        /* Test For Effectivity */
                  count%(l%) = count%(l%) + 1%
                  gosub L12500                  /* Process Print Entry */
                  if bom_all_flag% = 0% and bomid$ <> bomid_save$        ~
                               then L12230

                  if l% < maxlevels% then gosub'7(assembly$)
                                         /* Do Assys If Not At Bottom */
                  goto L12230

L12330:           REM END ROUTINE GRACEFULLY.
                      l% = l% - 1%
                      return

L12500: REM *******  FILL SCREEN ENTRIES, AND DISPLAY WHEN FULL.  ******
                bom_eff_flag$ = " "
                if bom_all_flag% = 1% then L12525
                     if bomid$ = bomid_save$ then L12530  /* EFFECTIVE? */
                               count%(l%) = count%(l%) - 1%
                               return

L12525:            if bomid_save$ = bomid$ then bom_eff_flag$ = "*"
L12530:         displayline% = displayline% + 1%
                   ax$ = assembly$
                   if bommkr$ = "ST" then goto L12555
                   if str(ax$,20,6) <> "      " then L12555
                   str(ax$,20,6)= hex(84) & "(" & bommkr$ & ")" & hex(8c)
L12555:            put str(line$(displayline%), 2% * l% - 1%, 37),       ~
                        using L12700, count%(l%), ax$, bomid_save$,       ~
                                                      bom_eff_flag$
                   call "CONVERT" (quantity, 2.2,                        ~
                              str(line$(displayline%), 59, 10))
                   call "CONVERT" (xused, 2.2,                           ~
                              str(line$(displayline%), 70, 10))
                   displayline% = displayline% + 1%
                   call "DESCRIBE"(#4, assembly$, tempdescr$, 1%, f1%(4))
                   put str(line$(displayline%), 2% * l% - 1%, 38),       ~
                        using L12730, tempdescr$
                if displayline% > 19% then L12800    /* SHOW SCREEN.    */
                   return

L12700: %###. ######################### (###)#

L12730: %    ##################################


L12800:     REM DISPLAY CONTROLLER FOR FULL SCREEN.  HANDLES P.F. KEYS.

L12810:         gosub L41000
                      if keyhit% <>  2 then L12820
                               return clear all
                               goto L12082
L12820:               if keyhit%  =  5% or keyhit% = 0% then L12915
                      if keyhit% <> 14% then L12870
                               return clear all
                               goto report_print
L12870:               if keyhit% <> 16 then L12810
                               return clear all
                               goto inputmode
L12915:         if l% = 0% then L12810
                displayline% = 0
                init(" ") line$()
                goto L12500               /* WHEN SIGNALS FOR NEXT.     */

L13000: REM *************************************************************~
            *        P R I N T   R E P O R T   F O R   R A N G E        *~
            *                    MAINLINE CONTROL                       *~
            * Prints Requested Data...                                  *~
            *************************************************************

            select printer (134)
            call "SETPRNT" ("BOM005", " ", 0%, 0%)
            mstrpart$ = str(partr$(3),,25)
            call "PLOWNEXT" (#4, mstrpart$, 0%, f1%(4))
                goto L13140

        read_loop
            call "PLOWNEXT" (#4, mstrpart$, 0%, f1%(4))
L13140:         if f1%(4) = 0 then L13290
            if mstrpart$ > partr$(4) then L13290
            infomsg$ = "Processing Part Number " & hex(84) & mstrpart$
            call "SHOSTAT" (infomsg$)
            call "DESCRIBE" (#4, mstrpart$, mstrpartdesc$, 1%, f1%(4%))
            gosub'200(mstrpart$)
            if page% > 0% then L13240
                 gosub page_head
                 print skip(2)
                 print using L60400
L13240:          print skip(2)
                 print using L60290
            if opflag$ = "Y" then gosub'80(mstrpart$)  /* Option Check */
            goto read_loop

L13290: REM Now close the printer and start again
            time$ = " " : call "TIME" (time$)
            if page% > 0% then print skip(2)
            if page% > 0% then print using L60280, time$
            close printer
            call "SETPRNT" ("BOM005", " ", 0%, 1%)
            keyhit% = 0%
            if page% > 0% then  L13410
                     call "ASKUSER" (keyhit%, "*** WHERE USED LIST ***", ~
                                        "Part(s) NOT Used Anywhere !",   ~
                                        " ",                             ~
                                        "Press Return to Continue . . .")
L13410:     goto inputmode

        REM *************************************************************~
            *  F I N D   W H E R E   P A R T   I S   A N   O P T I O N  *~
            *-----------------------------------------------------------*~
            * Sequentially Reads The Part Options File To Find Where    *~
            * The Part Is Listed As A Valid Replacement.                *~
            *************************************************************

        deffn'80(part$)
            line% = 1000%
            opcount% = 0%
            readkey2$ = " "  :  str(readkey2$,54,1) = hex(00)
        loop_again
            call "PLOWNEXT" (#6, readkey2$, 0%, f1%(6%))
                if f1%(6%) = 0% then L15170
                    gosub L32000       /* Get Options Record */

                if str(part$,,25) = str(reppart$,,25) then gosub'81
                goto loop_again


L15170:     if opcount% > 0% then return
                gosub page_head2
                print skip (2)
                print tab(20), "This Part Is Not Listed As An Option !"

            return

        REM  Page Control Routine
        page_head2
            if line% < 55% then return
                print page
                page% = page% + 1%
                line% = 0%
                print using L60000, date$, time$, company$
                print using L60040, page%
                print using L60310
                print skip(1)
                print using L60300, part$
                print skip(1)
                print using L60320
                print using L60330
                print using L60340
                line% = 9%
            return

        REM *************************************************************~
            *   O P T I O N   F O U N D  /  F I N D   A S S E M B L Y   *~
            *                                                           *~
            *************************************************************

        deffn'81

            call "DESCRIBE" (# 4, oppart$, tempdescr$, 1%, f1%(4%))
            if f1%(4%) = 0% then tempdescr$ = "(Non Stocked Part!)"

            print$(1%) = oppart$ & " " & tempdescr$

            if opassy$ <> " " then L16180
                print$(2%) = " "  :  bom_eff_flag$ = " "
                temp$ = " "
                goto L16261

L16180:     call "DESCRIBE" (# 4, opassy$, tempdescr$, 1%, f1%(4%))
            if f1%(4%) = 0% then tempdescr$ = "(NON STOCKED PART!)"

            print$(2%) = opassy$ & " " & tempdescr$
            bomid$ = opbom$
            gosub'190(opassy$)
            if bomid$ = opbom$ then bom_eff_flag$ = "*"                  ~
                               else bom_eff_flag$ = " "
            temp$ = "(" & opbom$ & ")"
L16261:     opcount% = opcount% + 1%
            put nmbr$, using L16264, opcount%
L16264:         %##:
            gosub page_head2

            print using L60370, nmbr$, print$(1%), print$(2%),            ~
                               temp$, bom_eff_flag$
            line% = line% + 1%

            if opassy$ = " " then search_bom
                print skip (1)
                line% = line% + 1%

            return

        REM ------------  FIND BOM's THAT USE OPTION  ----------------

        search_bom
                bom_count% = 0%
                print$(1%) = " "
                readkey3$ = str(oppart$,,25) & hex(00)
           next_bom
                bom_eff_flag$ = " "  :  bomid_save$ = " "
                nmbr$ = " "  :  temp$ = " "

L16590:         call "PLOWALTS" (#5, readkey3$, 1%, 25%, f1%(5%))
                     if f1%(5%) = 1% then L16631
                          print$(2%) = "OPTION NOT FOUND IN ANY BOM"
                          if bom_count% = 0% then L16820  else L16900

L16631:         if str(readkey3$,54,3) = "  0" then L16590 /* Header ? */
                gosub L30000
                if op$ <> "Y" then L16900        /* Options Not Enabled */
                bomid_save$ = bomid$
                gosub'190(assembly$)
                if bom_all_flag% = 1% then L16760
                               /* Print ONLY effective BOM's */
                      if bomid$ = bomid_save$ then L16770 /* Effective? */
                               goto L16900

                               /* Print all BOM versions    */
                               /* mark effective ones w/ *  */
L16760:               if bomid_save$ = bomid$ then bom_eff_flag$ = "*"
L16770:         call "DESCRIBE" (#4, assembly$, tempdescr$, 1%, f1%(4%))
                     if f1%(4%) = 0% then tempdescr$ = "(NON STOCKED PART~
        ~)"
                print$(2%) = assembly$ & " " & tempdescr$
                temp$ = "(" & bomid_save$ & ")"
L16820:         bom_count% = bom_count% + 1%
                put nmbr$, using L16840, bom_count%
L16840:              %##:

                gosub page_head2
                print using L60350, " ", print$(1%), nmbr$, print$(2%),   ~
                                   temp$, bom_eff_flag$
                line% = line% + 1%
L16900:         if f1%(5%) = 1% then next_bom

                print skip (1)
                line% = line% + 1%
            return


        REM *************************************************************~
            *      G E T   E F F E C T I V E   B O M                    *~
            *                                                           *~
            * GETS THE CURRENT BOM FOR PASSED PART BASED ON DATEINDEX%  *~
            *************************************************************

            deffn'190(part2$)
                 bomid$, textidh$ = " "
                 call "READ100" (#4, part2$, f1%(4))
                     if f1%(4) = 0% then return  /* NON - STOCKED      */
                 get #4, using L18110, type$
L18110:              FMT POS(180), CH(3)
                 convert type$ to type%, data goto L18140
                    if type% = 800 then L18200        /* JUNK STATEMENT */
                    goto L18200
L18140:                bomid$ = hex(000000)           /* SET FOR ERROR */
                       return
L18200:          bomid$ = hex(000000)                 /* SET FOR ERROR */

                 if dateindex% = 0% then return  /* OFF PROD CALENDER? */
                    readkey4$ = str(part2$,,25) & "1"
                    call "READ102" (#11, readkey4$, f1%(11%))
                       if f1%(11%) <> 1% then return
                    get #11, using L18330, readkey4$, bom$()
L18330:                FMT CH(29), 490 * CH(3)
                    if str(readkey4$,,25) <> str(part2$,,25) then return
                    bomid$ = bom$(dateindex%)
                    if bomid$ = " " then L18140

          call "READ100" (#5,str(part2$,,25)&str(bomid_save$,,3)&"  0"   ~
                                                             ,f1%(5%))
               if f1%(5%) <> 0% then gosub L31000 /* Load Bom Header */
           return

        REM *************************************************************~
            * P R I N T   P A R T S   E X P L O S I O N   F O R   O N E *~
            *                           P A R T                         *~
            *                                                           *~
            * PRINTS THE WHERE USED LIST FOR ONE PART NUMBER.  THIS     *~
            * ROUTINE GETS CALLED FROM THE DISPLAY CONTROLLER FOR ONE - *~
            * PART DISPLAY AND ALSO FROM THE RESULTS OF THE SORT        *~
            * ROUTINE.                                                  *~
            *************************************************************

            deffn'200(part$)

            l% = 0%
            line% = 1000%: page% = 0%
            call "DESCRIBE" (#4, part$, comppartdescr$, 1%, f1%(4%))

            gosub'8(part$)               /* P$ = PART NUMBER TO BOM    */
            if page% > 0% then print using L60190
            return                       /* GET NEXT PART TO BOM       */

            deffn'8(part$)
                  l% = l% + 1%
                  p$(l%) = str(part$,,25) & hex(00)
                  count%(l%) = 0%        /* SEQUENCE NUMBER FOR PRINT  */

L19120:           call "PLOWALTS" (#5, p$(l%), 1%, 25%, f1%(5%))
                       if f1%(5%) = 0% then L19195
                  if str(p$(l%),54,3) = "  0" then L19120
                  gosub L30000            /* LOAD BOM RECORD & INFO.    */
                  bomid_save$ = bomid$
                  gosub'190(assembly$)   /* TEST FOR EFFECTIVITY       */
                  count%(l%) = count%(l%) + 1%
                  gosub L19300            /* PROCESS PRINT ENTRY        */
                  if bom_all_flag% = 0% and bomid$ <> bomid_save$        ~
                               then L19120

                  if l% < maxlevels% then gosub'8(assembly$)
                                         /* DO ASSYS IF NOT AT TOP.    */
                  goto L19120

L19195:           REM END ROUTINE GRACEFULLY.
                      l% = l% - 1%
                      return

L19300:  REM ------------  ROUTINE TO PRINT ENTRY JUST LOADED.  --------
            if prttxt$ = "Y" and page% = 0% then gosub component_text
                init(" ") print$(), bom_eff_flag$
                if bom_all_flag% = 1% then L19365

                               /* Print ONLY effective BOM's */
                      if bomid$ = bomid_save$ then L19380 /* Effective? */
                               count%(l%) = count%(l%) - 1%
                               return

                               /* Print all BOM versions    */
                               /* mark effective ones w/ *  */
L19365:               if bomid_save$ = bomid$ then bom_eff_flag$ = "*"

L19380:         put str(print$(1), 3% * l% - 2%, 37),                    ~
                        using L19395, count%(l%), assembly$, bomid_save$, ~
                                     bom_eff_flag$
L19395:                       %###. ######################### (###)#

                call "CONVERT" (quantity, 2.2, str(print$(2%),,10))
                call "CONVERT" (xused,    2.2, str(print$(3%),,10))

                     for i% = 1% to bm%
                          if bommkr$ = mkr$(i%) then L19440
                     next i%
L19440:              print$(4%) = mkrdes$(i%)
                     if bommkr$ = "ST" then print$(4%) = "          "

                gosub page_head          /* PAGE CONTROL SUBROUTINE */
                print using L60160, print$(1%), print$(2%), print$(3%),   ~
                                   print$(4%), type$
                init(" ") print$()
                call "DESCRIBE" (#4, assembly$, str(print$(1), 3%*l%+3%, ~
                                                   32), 0%, f1%(4))
                print using L60160, print$(1%), print$(2%), print$(3%),   ~
                                   print$(4%), " "
                line% = line% + 1%
                if prttxt$ <> "Y" then return


        REM Print any header text that was entered...
            dis% = 3%*l%+4%
            if textidh$ = hex(20202020) or                               ~
               textidh$ = hex(00000000) or                               ~
               textidh$ = hex(ffffffff) then return

            stat% = 0%
L19560:     call "TXTPRINT"(#12, f2%(12), 134%, textidh$, "BOM005",      ~
                            dis%, line%, 54%, "N", " ", stat%)
            if stat% = 0% then L19590
            gosub page_head
            goto L19560

L19590:     print skip(1) : line% = line% + 1%
            return


         REM -----------  BUILD HEADING BASED ON OPTION  ---------------
        bld_head
              if bom_all_flag% = 0% then L19680
                 assystring$ = assystring$ & "      ( All BOM Versions )"
                 goto L19990

L19680:       assystring$ = assystring$ & "        Effectivity Date:     ~
        ~"                 & effdatef$
           return

        REM Page Control Routine
        page_head
            line% = line% + 1%
            if line% < 55% then return
            if page% > 0% then print using L60190
            print page
            page% = page% + 1%
            line% = 0%
            print using L60000, date$, time$, company$
            print using L60040, page%
            print skip(1)
            if prange% = 0% then gosub L19800  else gosub L19860
            print skip(2)
            if bom_all_flag% = 0% then print using L60220                 ~
                                  else print using L60242
            print using L60250
            print using L60190
            line% = line% + 8%
            return

L19800:  REM ---------------  HEADING FOR SINGLE PART  -----------------
           assystring$ = comppart$ & ' ' & comppartdescr$
           gosub bld_head

           print using L60070, assystring$
           line% = line% + 1%
           return
L19860:  REM ---------------  HEADING FOR RANGE OF PARTS  --------------
           if partr$(1%) <> "ALL" then L19920
           assystring$ = " A L L   P A R T S                            "
           gosub bld_head
           print using L60100, assystring$
           goto L19975

L19920:    assystring$ = partr$(1%) & ' ' & partnm$(1%)
           gosub bld_head
           print using L60100, assystring$

           assystring$ = partr$(2%) & ' ' & partnm$(2%)
           print using L60130, assystring$
L19975:    print skip(1)
           pheader$ ="Where Used For " & mstrpart$ & "  " & mstrpartdesc$
           call "STRING" addr ("CT", pheader$, 114%)
           print pheader$

           line% = line% + 3%
L19990:    return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE PAGE 1 OF INPUT. *~
            *************************************************************

            deffn'161(fieldnr%)
                  enabled% = 0%
                  on fieldnr% gosub L20100,         /* COMPONENT PART # */~
                                    L20200,         /* NUMBER OF LEVELS */~
                                    L20300,         /* BOM EFFECT. DATE */~
                                    L20400,         /* OPTION LIST ?    */~
                                    L20500          /* PRINT BOM TEXT?  */
                     return

L20100:     REM DEFAULT/ENABLE FOR COMPONENT PART NUMBER
                enabled% = 1%
                inpmessage$ = "To Print Where Used Lists For A Range Of P~
        ~arts, Press (9)."
                return

L20200:     REM DEFAULT/ENABLE FOR NUMBER OF LEVELS
                enabled% = 1%
          inpmessage$ = "The Maximum Number Of Levels Is 15."
                if maxlevels$ = " " then maxlevels$ = "1 "
                return

L20300:     REM DEFAULT/ENABLE FOR BOM EFFECTIVITY DATE
                enabled% = 1%
          inpmessage$ = "The BOM Effectivity Date, or 'ALL' for all versi~
        ~ons."
                if effdatef$ = " " then effdatef$ = date$
                return

L20400:     REM ********  DEFAULT/ENABLE FOR WHERE USED AS OPTION FLAG *
                enabled% = 1%
          inpmessage$ = "ENTER 'Y' to Include a section showing where a p~
        ~art is listed as an Option."
                if opflag$ = " " then opflag$ = "N"
                return

L20500:     REM ********  DEFAULT/ENABLE FOR PRINT BOM TEXT  ***********
                enabled% = 1%
          inpmessage$ = "ENTER 'Y' to include Component Text on Where Use~
        ~d Lists."
                if prttxt$ = " " then prttxt$ = "N"
                return

        REM *************************************************************~
            * D E F A U L T / E N A B L E   F O R   R A N G E   P A G E *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES FIELDS FOR THE RANGE INPUT SCR. *~
            *************************************************************

            deffn'162(fieldnr%)
                  enabled% = 0%
                  inpmessage$ = " "
                  on fieldnr% gosub L21170,         /* PART NUMBER RANGE*/~
                                    L21250,         /* NUMBER OF LEVELS */~
                                    L21290,         /* EFFECTIVITY DATE */~
                                    L21350,         /* Is Option? Flag  */~
                                    L21420          /* Text Print Flag  */
                     return
L21170:     REM ********  DEFAULT/ENABLE FOR PART NUMBER RANGE  ********
                enabled% = 1%
            inpmessage$ = "Enter Range Of Parts To Print Implosions For."
                if partr$(1) = " " then partr$(1) = "ALL"
                return

L21250:     REM ********  DEFAULT/ENABLE FOR NUMBER OF LEVELS  *********
                enabled% = 1%
            inpmessage$ = "The Maximum Number Of Levels Is 15."
                if maxlevels$ = " " then maxlevels$ = "1"
                return
L21290:     REM ********  DEFAULT/ENABLE FOR EFFECTIVITY DATE  *********
                enabled% = 1%
                if effdatef$ = " " then effdatef$ = date$
          inpmessage$ = "The BOM Effectivity Date, or 'ALL' for all versi~
        ~ns"
                return

L21350:     REM ********  DEFAULT/ENABLE FOR WHERE USED AS OPTION FLAG *
                enabled% = 1%
          inpmessage$ = "ENTER 'Y' to Include a section showing where a p~
        ~art is listed as an Option."
                if opflag$ = " " then opflag$ = "N"
                return

L21420:     REM ********  DEFAULT/ENABLE FOR PRINT BOM TEXT  ***********
                enabled% = 1%
          inpmessage$ = "ENTER 'Y' to include Component Text on Where Use~
        ~d Lists."
                if prttxt$ = " " then prttxt$ = "N"
                return


        REM *************************************************************~
            *               HEADER TEXT PRINT ROUTINE                    ~
            *************************************************************

        component_text

            if textidl$ = hex(20202020) or                               ~
               textidl$ = hex(00000000) or                               ~
               textidl$ = hex(ffffffff) then return
        REM Print BOM component text that was entered...
            gosub page_head
            dis% = 3%*l%+4%
            stat% = 0%
L25130:     call "TXTPRINT"(#12, f2%(12), 134%, textidl$, "BOM005",      ~
                  dis%, line%, 54%, "N", " ", stat%)
            if stat% = 0% then L25190
            gosub page_head
            goto L25130

L25190:     print skip(1) : line% = line% + 1%
            return

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

            return clear all
            goto inputmode

L30000: REM *************************************************************~
            * L O A D   B I L L   O F   M A T E R I A L S   R E C O R D *~
            *                                                           *~
            * LOADS THE BILL OF MATERIALS RECORD, WITH ITS ALL-IMPORTANT*~
            * QUANTITY AND SIZE FIELDS FROM THE BILL OF MATERIALS FILE. *~
            *************************************************************

            get    #5, using L30110, bom_comp$,                           ~
                       assembly$, bomid$, quantity, xused, bommkr$,      ~
                       op$, compbom$, textidl$
            return

L30110:     FMT CH(25),                  /* COMPONENT PART NUMBER      */~
                CH(25),                  /* ASSEMBLY PART NUMBER       */~
                CH(3),                   /* BOM STRUCTURE ID           */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                PD(14,4),                /* QUANTITY REQUIRED          */~
                PD(14,4),                /* TIMES USED (SIZE)          */~
                POS(89),                                                 ~
                CH(2),                   /* BOM MARKER                 */~
                CH(01),                  /* OPTIONAL COMPONENT FLAG    */~
                CH(3),                   /* COMPONENT BOM              */~
                CH(4)                    /* Line Item Text ID          */~


L31000: REM *************************************************************~
            * L O A D   B I L L   O F   M A T E R I A L S   R E C O R D *~
            *             >>>>>>>>  BOM HEADER  <<<<<<<<                *~
            * LOADS THE BILL OF MATERIALS RECORD HEADER,                *~
            * POINTS TO TEXTID HEADER.                                  *~
            *************************************************************

            get #5, using L31100, textidh$
            return

L31100:     FMT XX(56),                  /* Key Area                   */~
                XX(30),                  /* DESCRIPTION                */~
                XX(3),                   /* WHICH ROUTE                */~
                CH(4)                    /* TEXTID Header              */~


L32000: REM *************************************************************~
            *      L O A D   P A R T   O P T I O N S   R E C O R D      *~
            *                                                           *~
            *                                                           *~
            *************************************************************

            get #6, using L32100, opassy$, opbom$, oppart$, reppart$
            return

L32100:     FMT CH(25),                  /* ASSEMBLY PART              */~
                CH(3),                   /* SPECIFIC BOM ID            */~
                CH(25),                  /* COMPONENT PART OPTION      */~
                XX(1),                   /* SEQ NMBR                   */~
                CH(25)                   /* REPLACEMENT PART           */


        REM *************************************************************~
            *      I N P U T   M O D E   S C R E E N   P A G E   1      *~
            *                                                           *~
            * INPUTS DOCUMENT FOR FIRST TIME.                           *~
            *************************************************************

            deffn'201(fieldnr%)
                  init(hex(84)) lfac$()
                  str(header$,62) = "BOMIMPLS: " & cms2v$
                                         /* SELECT VALID PF KEYS */
                  if fieldnr% = 0% then gosub L40800  else gosub L40700

                  on fieldnr% gosub L40110,         /* COMPONENT PART # */~
                                    L40125,         /* NUMBER OF LEVELS */~
                                    L40110,         /* EFFECTIVITY DATE */~
                                    L40110,         /* IS Option? Flag  */~
                                    L40110          /* Text Print Flag  */
                  goto L40145

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L40110:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L40125:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L40145:     accept                                                       ~
               at (01,02), "Print Where Used List",                      ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Component Part Number",                      ~
               at (06,36), fac(lfac$( 1)), comppart$            , ch(25),~
                                                                         ~
               at (07,02), "Number Of Levels",                           ~
               at (07,36), fac(lfac$( 2)), maxlevels$           , ch(02),~
                                                                         ~
               at (08,02), "Effectivity Date",                           ~
               at (08,36), fac(lfac$( 3)), effdatef$            , ch(08),~
                                                                         ~
               at (09,02), "Include List of Where an Option?",           ~
               at (09,36), fac(lfac$( 4)), opflag$              , ch(01),~
               at (09,40), "(Appears on printed listing ONLY)",          ~
                                                                         ~
               at (10,02), "Include Text On Report?",                    ~
               at (10,36), fac(lfac$( 5)), prttxt$              , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pfktext$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfktext$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfktext$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13% then L40315
                  call "MANUAL" ("BOMIMPLS")
                  goto L40145

L40315:        if keyhit% <> 0% then L40340
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

L40340:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L40145

L40700: REM *************************************************************~
            *               SELECT PF KEYS FOR FIRST FIELD              *~
            *                                                           *~
            *************************************************************

                pfktext$(1%) = "(1)Start Over                            ~
        ~                      (13)Instructions"
                if fieldnr% > 1% then L40770
                pfktext$(2%) = "                                         ~
        ~                      (15)Print Screen"
                pfktext$(3%) = "                        (9)Print Where Us~
        ~ed List for Range    " & hex(84) & "(16)Exit Program"
                pfkeys$ = hex(0001090d0f10)
                return

L40770:         pfktext$(2%) = "                        (4)Previous Field~
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
                pfktext$(2%) = "                                         ~
        ~" & hex(84) & "(14)Print Report" & hex(8c) &                     ~
        "    (15)Print Screen"
                pfktext$(3%) = "                                         ~
        ~                     " & hex(84) & "(16)Display List"
                pfkeys$ = hex(00010d0e0f10)
                return

L41000: REM *************************************************************~
            *       W H E R E   U S E D   L I S T   D I S P L A Y       *~
            *                                                           *~
            * WHERE USED LIST DISPLAY SCREEN.  NO BIG DEAL, SINCE ALL   *~
            * THE COLUMNS ARE FORMATTED IN THE PRINT ROUTINE.           *~
            *************************************************************

L41070:     accept                                                       ~
               at (01,02),                                               ~
                  "(2)First   (5)Next   (14)Print Report   (15)Print Scre~
        ~en  (16)Next Component",                                         ~
               at (03,02), fac(hex(84)), titledescr$            , ch(79),~
               at (04,02), fac(hex(a4)), title$                 , ch(79),~
               at (05,02), fac(hex(8c)), line$( 1)              , ch(79),~
               at (06,02), fac(hex(8c)), line$( 2)              , ch(79),~
               at (07,02), fac(hex(8c)), line$( 3)              , ch(79),~
               at (08,02), fac(hex(8c)), line$( 4)              , ch(79),~
               at (09,02), fac(hex(8c)), line$( 5)              , ch(79),~
               at (10,02), fac(hex(8c)), line$( 6)              , ch(79),~
               at (11,02), fac(hex(8c)), line$( 7)              , ch(79),~
               at (12,02), fac(hex(8c)), line$( 8)              , ch(79),~
               at (13,02), fac(hex(8c)), line$( 9)              , ch(79),~
               at (14,02), fac(hex(8c)), line$(10)              , ch(79),~
               at (15,02), fac(hex(8c)), line$(11)              , ch(79),~
               at (16,02), fac(hex(8c)), line$(12)              , ch(79),~
               at (17,02), fac(hex(8c)), line$(13)              , ch(79),~
               at (18,02), fac(hex(8c)), line$(14)              , ch(79),~
               at (19,02), fac(hex(8c)), line$(15)              , ch(79),~
               at (20,02), fac(hex(8c)), line$(16)              , ch(79),~
               at (21,02), fac(hex(8c)), line$(17)              , ch(79),~
               at (22,02), fac(hex(8c)), line$(18)              , ch(79),~
               at (23,02), fac(hex(8c)), line$(19)              , ch(79),~
               at (24,02), fac(hex(8c)), line$(20)              , ch(79),~
                                                                         ~
               keys(hex(02050d0e0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13% then L41370
                  call "MANUAL" ("BOMIMPLS")
                  goto L41070

L41370:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L41070

        REM *************************************************************~
            *        I N P U T   R A N G E   O F   P A R T S            *~
            *                                                           *~
            *************************************************************

            deffn'202(fieldnr%)
                str(header$,62) = "BOMIMPLS: " & cms2v$
                gosub L43800
                  init(hex(84)) lfac$()
                  on fieldnr% gosub L43190,         /* PART NUMBER RANGE*/~
                                    L43220,         /* MAX # OF LEVELS  */~
                                    L43190,         /* EFFECTVITY DATE  */~
                                    L43190,         /* Is Option? Flag  */~
                                    L43190          /* Text Print Flag  */
                     goto L43260

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L43190:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
L43220:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

L43260:     accept                                                       ~
               at (01,02), "Print Where Used Lists (Range of Parts)",    ~
               at (01,66), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), header$                , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,26), fac(hex(ac)), title$(1)              , ch(25),~
               at (06,52), fac(hex(ac)), title$(2)              , ch(25),~
                                                                         ~
               at (07,02), "Part Number",                                ~
               at (07,26), fac(lfac$( 1)), partr$(1)            , ch(25),~
               at (07,52), fac(lfac$( 1)), partr$(2)            , ch(25),~
                                                                         ~
               at (08,02), "Maximum Number Of Levels",                   ~
               at (08,30), fac(lfac$( 2)), maxlevels$           , ch(02),~
                                                                         ~
               at (09,02), "Effectivity Date",                           ~
               at (09,30), fac(lfac$( 3)), effdatef$            , ch(08),~
                                                                         ~
               at (10,02), "List Where Used as Option?",                 ~
               at (10,30), fac(lfac$( 4)), opflag$              , ch(01),~
                                                                         ~
               at (11,02), "Include Text on Report?",                    ~
               at (11,30), fac(lfac$( 5)), prttxt$              , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pfktext$(1)          , ch(79),~
               at (23,02), fac(hex(8c)),   pfktext$(2)          , ch(79),~
               at (24,02), fac(hex(8c)),   pfktext$(3)          , ch(79),~
                                                                         ~
               keys(pfkeys$),                                            ~
               key (keyhit%)

               if keyhit% <> 13% then L43640
                  call "MANUAL" ("BOMIMPLS")
                  goto L43260

L43640:        if keyhit% <> 0% then L43690
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

L43690:        if keyhit% <> 15% then return
                  call "PRNTSCRN"
                  goto L43260

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
                  on fieldnr% gosub L50140,         /* COMPONENT PART # */~
                                    L50350,         /* NUMBER OF LEVELS */~
                                    L50410,         /* EFFECTIVITY DATE */~
                                    L50520,         /* Text Print Flag  */~
                                    L50660          /* Options Flag     */
                     return
L50140: REM Test Data For Component Part Number
            readkey$ = comppart$
            comppartdescr$ = hex(06) & "Select the Part to Implode"
            call "GETCODE" (#4, readkey$, comppartdescr$, 0%, .32, f1%(4))
            if f1%(4) = 1% then comppart$ = readkey$
            if f1%(4) = 1% then return
                errormsg$ = "Please enter or select a valid part number"
                return

L50350: REM Test Data For Number Of Levels
            if maxlevels$ = " " then maxlevels$ = "1"
            call "NUMTEST" (maxlevels$, 1, 15, errormsg$, 0.0, temp)
                if errormsg$ <> " " then return
                    maxlevels% = temp
                    return

L50410: REM Test Data For Effectivity Date
            if effdatef$ = "ALL" then bom_all_flag% = 1%                 ~
                                 else bom_all_flag% = 0%
            if bom_all_flag% = 0% then L50440
                effdate$ = date$
                goto L50455

L50440:     call "DATEOK" (effdatef$, u3%, errormsg$)
            if errormsg$ <> " " then return
                effdate$ = effdatef$
L50455:         call "DATUNFMT" (effdate$)
                call "PIPINDEX" (#3, effdate$, dateindex%, ret%)
                    if ret% <> 1% then return
                errormsg$ = "Planning Calander Base Date not Found. " &  ~
                            "Exit & Correct."
                return

L50520: REM Test Data For Print Option Flag
            gosub'159(opflag$)
            if opflag$ <> "Y" then return
                gosub L50800
                return

L50660: REM Test Data For Text Print Flag
            gosub'159(prttxt$)
            return

        REM Warning Message
L50800:     msg$(1%) = hex(94) & "WARNING !" & hex(84)
            str(msg$(1%),41) = "It will take an Extended Period of Time"
            msg$(2%) = "to find where this Part is used as an Option."
            msg$(3%) = "Press RETURN to Continue. . ."
            keyhit1% = 0%
            call "ASKUSER" (keyhit1%, "WHERE USED AS AN OPTION",         ~
                            msg$(1%), msg$(2%), msg$(3%))
            return


        REM *************************************************************~
            *        TEST A FIELD FOR A 'Y' or 'N' ENTRY                *~
            *************************************************************
            deffn'159(x$)
                if x$ = " " then x$ = "N"
                if pos("YN" = x$) = 0 then L51070
                return
L51070:         errormsg$ = "Please Enter 'Y' or 'N'"
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *                                                           *~
            * TESTS DATA FOR THE ITEMS ON PAGE 1.                       *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L52160,         /* PART NUMBER RANGE*/~
                                    L50350,         /* NUMBER OF LEVELS */~
                                    L50410,         /* EFFECTIVITY DATE */~
                                    L50520,         /* Is Option? Flag  */~
                                    L50660          /* Text Print Flag  */
                     return

L52160:     REM **********  TEST DATA FOR PART NUMBER RANGE  ***********
                call "TESTRNGE"   (partr$(1%), partr$(2%), partr$(3%),   ~
                                   partr$(4%), errormsg$, #4)
            return

        REM ***** And Now the Image Statements *****
L60000: %Run Date: ######## @ ########     ##############################~
        ~##############################                      BOMIMPLS: BOM~
        ~005

L60040: %                                           P A R T S   W H E R E~
        ~ U S E D   L I S T                                      PAGE: ###

L60070: % Component Part :  #############################################~
        ~################################################################

L60100: % Range Of Components: ##########################################~
        ~################################################################

L60130: %                  To: ##########################################~
        ~################################################################

L60160: %################################################################~
        ~################ ########## ########## ##########  ###

L60190: %----------------------------------------------------------------~
        ~---------------- ---------- ---------- ---------- ----

L60220: %  This Component Is Used In The Assemblies Shown Below          ~
        ~  (BOM)           Quantity     Size      Non-Std  Part

L60242: %  This Component Is Used In The Assemblies Shown Below    (BOM) ~
        ~ * = Effective    Quantity     Size      Non-Std  Part

L60250: %  NOTE: See Last Col For Use Of Lower Level Comp. In Each Assemb~
        ~ly Shown Below    Required   (* Used)   Comp Use  Type

L60280: %                                 * * * * *  E N D   O F   R E P ~
        ~O R T (########) * * * * *
L60290: %                                        * * * * *  E N D   O F  ~
        ~ P A R T  * * * * *

L60300: % Replacement Part : ############################################~
        ~################################################################

L60310: %                               W H E R E   P A R T   I S   U S E~
        ~ D   A S   A N   O P T I O N

L60320: %                                                                ~
        ~                                                          SPECIFI~
        ~C

L60330: % Component Part To Be Optionally Replaced                    In ~
        ~Assembly Part                                             BOM ID


L60340: % ----------------------------------------------------------- ---~
        ~--------------------------------------------------------  -------


L60350: % ### ####################################################### ###~
        ~ #######################################################  ##### #

L60370: % ### ####################################################### ###~
        ~########################################################  ##### #

L60400: %     * * * * *  This Part is NOT USED in any Bill of Materials  ~
        ~* * * * *

L60430: %     * * * * *  This Part is NOT Currently assigned as a Replace~
        ~ment Part  * * * * *

L65000: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
