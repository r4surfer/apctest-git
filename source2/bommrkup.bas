        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                                                           *~
            *  BBBB    OOO   M   M  M   M  RRRR   K   K  U   U  PPPP    *~
            *  B   B  O   O  MM MM  MM MM  R   R  K  K   U   U  P   P   *~
            *  BBBB   O   O  M M M  M M M  RRRR   KKK    U   U  PPPP    *~
            *  B   B  O   O  M   M  M   M  R   R  K  K   U   U  P       *~
            *  BBBB    OOO   M   M  M   M  R   R  K   K   UUU   P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BOMMRKUP - BOM EXPLOSION SHOWING BOTH STANDARD COST AND   *~
            *            A USER SPECIFIED PRICE CATEGORY (A-E).         *~
            *            MARK-UP TO STANDARD IS ALSO SHOWN.             *~
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
            * 08/03/83 ! ORIGINAL                                 ! GLW *~
            * 02/01/84 ! CORRECT FMT FOR "MONTHS OPEN" @ 1710     ! ECR *~
            * 09/17/86 ! BOMMASTR FORMAT CHANGED                  ! LKM *~
            * 11/05/86 ! HNYMASTR FORMAT CHANGED (PRICES)         ! KAB *~
            * 02/02/87 ! Special Pricing Expansion                ! ERN *~
            * 05/13/87 ! HNYMASTR record length mod for Std Cost  ! JIM *~
            *          !   Get Standard costs via STCCOSTS.       !     *~
            * 06/11/87 ! Misc. mods to screen, report.            ! JIM *~
            * 02/02/89 ! Proj 7880714 new price set implementation! JIM *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim                                                              ~
            b$(490)3,                    /* ALL EFFECTIVE BOM'S        */~
            bom$3,                       /* WHICH BOM IS EFFECTIVE     */~
            component$25,                /* COMPONENT PART NUMBER      */~
            count%(16),                  /* INDENT INDEXES THIS LEVEL  */~
            cumulative$3,                /* CUMULATIVE ANALYSIS? FLAG  */~
            cursor%(2),                  /* CURSOR LOCATION FOR EDITS  */~
            date$8,                      /* SCREEN DISPLAY DATE        */~
            descr$32,                    /* DESCRIPTION OF PART        */~
            edttran$80,                  /* TRANSLATION STRING FOR EDIT*/~
            errormsg$79,                 /* ERROR MESSAGE TEXT INFO    */~
            fac$(16,2)1,                 /* FIELD ATTRIBUTE CHARACTERS */~
            hdrdate$45,                  /* DATE FOR REPORT PRINTING   */~
            hdrdescr$132,                /* BUILD XXX OF XXX MESSAGE   */~
            i$(24)80,                    /* SCREEN IMAGE (NOT USED)    */~
            infomsg$79,                  /* INFORMATIVE MESSAGE TEXT   */~
            inpmessage$79,               /* INPUT MESSAGE TEXT INFO    */~
            lfac$(20)1,                  /* FIELD ATTRIBUTE CHARACTERS */~
            line2$79,                    /* Screen Line #2             */~
            line10$79,                   /* Screen Line #10            */~
            maxlevels$2,                 /* MAXIMUM NUMBER OF LEVELS   */~
            mkr$2, mkk$2,                /* BOM MARKER                 */~
            p$(16)31,                    /* NEXT BOM REC EACH LEVEL    */~
            part$25,                     /* PART TO DO THIS LEVEL      */~
            part$(16)25,                 /* PART NUMBERS FOR MULTIPLE  */~
            part22$25,                   /* PART NUMBERS FOR MULTIPLE  */~
            price$1,                     /* THE PRICE CATEGORY (A-E)   */~
            prices(16),                  /* THE 5 PRICES (A-E)         */~
            pricekey$60,                 /* KEY FOR PRICE FIELD        */~
            print$(6)10,                 /* 5 NUMBERS, PRINT FORMATTED */~
            prtpart$100,                 /* PART NUMBER/DESCR INDENTED */~
            quantity$(16)10,             /* QUANTITIES FOR MULTIPLE OA */~
            r(16),                       /* QUANTITY REQUIRED TO HERE  */~
            readkey$100,                 /* KEY TO PROCESS IN READING  */~
            summary$3,                   /* PRINT SUMMARY REPORT?      */~
            tod$8                         /* FIRST DAY OF PLAN PERIOD   */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20,                 /* RETURN CODE FROM "OPENFILE"*/~
            axd$(64)4                    /* AXD POINTER FROM "OPENFILE"*/

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.00.00 01/19/90 CMS2 / CMS-I Merge              "
        REM *************************************************************
            mat f2% = con :

                     /* THE VARIABLES F2%() AND AXD$() SHOULD NOT BE   */
                     /* MODIFIED.   THEY ARE AN INTRINSIC PART OF THE  */
                     /* FILE OPEN SUBROUTINE.                          */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 4 ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * # 5 ! BOMMASTR ! BILL OF MATERIALS RELATIONSHIP FILE      *~
            * # 9 ! WORKFILE ! WORK FILE FOR CUMULATIVE ANALYZES        *~
            * #12 ! CPRPRICE ! PRICE FILE                               *~
            * #24 ! ENGMASTR ! ENGINEERING MASTER FILE                  *~
            * #34 ! SYSFILE2 ! SYSTEM FILE FOR MONTHS OPEN              *~
            *************************************************************

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

            select # 9, "SORTWORK",                                      ~
                         indexed,                                        ~
                         recsize = 91,                                   ~
                         keypos =   1, keylen =  25                      ~

            select #12, "CPRPRICE",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 700,                                   ~
                        keypos = 1, keylen = 47

           select #24, "ENGMASTR" ,                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2015,                                  ~
                        keypos = 1, keylen = 29

           select #34, "SYSFILE2" ,                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 20


            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENFILE" (# 4, "SHARE", f2%( 4), rslt$( 4), axd$( 4))
            call "OPENFILE" (# 5, "SHARE", f2%( 5), rslt$( 5), axd$( 5))
            call "OPENFILE" (#12, "SHARE", f2%(12), rslt$(12), axd$(12))
            call "OPENFILE" (#24, "SHARE", f2%(24), rslt$(24), axd$(24))
            call "OPENFILE" (#34, "SHARE", f2%(34), rslt$(34), axd$(34))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *                                                           *~
            * INITIALIZES FIELDS NECESSARY FOR PROCESSING THE REPORT.   *~
            *************************************************************

            date$ = date
            call "READ100" (#34, "MONTHS OPEN         ", f1%(34))
                     if f1%(34) <> 1% then goto L01750
            get #34, using L01710, tod$
L01710:     FMT XX(32), CH(6)
            call "DATE" addr ("G-", tod$, date$, today%, r%)
            today% = today% + 1%
            goto L01780
L01750:     today% = 1%
            r% = r%

L01780:     call "DATEFMT" (date$)
            REM SET UP STRINGS FOR EDIT TRANSLATION.
                init(hex(00)) str(edttran$,  1, 80)
                init(hex(01)) str(edttran$,  1, 27)
                init(hex(02)) str(edttran$, 28, 11)
                init(hex(03)) str(edttran$, 44, 26)
                init(hex(04)) str(edttran$, 70, 11)

            str(line2$,62) = "BOMMRKUP: " & str(cms2v$,1,8)
            line10$ = "Part Number              " & hex(8c)
            str(line10$,43) = hex(ac) & "Part Number              " &    ~
                hex(8c)

        REM *************************************************************~
            *               I N P U T   Q U E S T I O N S               *~
            *                                                           *~
            * INPUTS THE QUESTIONS AND THE PART NUMBER INFORMATION      *~
            * AND EXITS WHEN A BLANK PART NUMBER IS ENTERED.            *~
            *************************************************************

        inputmode
            init(" ") cumulative$, summary$, maxlevels$, part$(),        ~
             price$,  quantity$(), errormsg$, infomsg$, inpmessage$
            if anotherflag% = 1% then infomsg$ = "Your BOM's at Standard ~
        ~are Printed, Do You Wish Others?"

            for fieldnr% = 1 to 3
                gosub'161(fieldnr%)
                      if enabled% =  0 then L02080
L02020:         gosub'201(fieldnr%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit%  = 16 then       L09160
                      if keyhit% <>  0 then       L02020
                gosub'151(fieldnr%)
                      if errormsg$ <> " " then L02020
L02080:         next fieldnr%

            for screenline% = 1 to 16
                c% = screenline%
                for fieldnr% = 1 to 2
                    gosub'163(fieldnr%)
                          if enabled% = 0 then L02220
L02150:             gosub'203(fieldnr%)
                          if keyhit%  =  1 then gosub startover
                          if keyhit%  = 16 then       L09160
                          if keyhit% <>  0 then       L02150
                    gosub'153(fieldnr%)
                          if c% > 1% and part$(c%) = " " then L02240
                          if c% <> 1% or part$(c%) <> " " then L02200
                               errormsg$ = "You must enter at least one"&~
                                         " Part Number"
L02200:                   if errormsg$ <> " " then L02150
                    next fieldnr%
L02220:         next screenline%

L02240: REM *************************************************************~
            *                E D I T   R E S P O N S E S                *~
            *                                                           *~
            * EDITS BOTH THE HEADER AND LINE ITEMS RESPONSES FOR THE    *~
            * ORDER ANALYSIS PARAMETERS.                                *~
            *************************************************************

            init(" ") errormsg$, inpmessage$, infomsg$
         inpmessage$ = "PF(16) for BOM w/ Prices -or- cursor to line & Pr~
        ~ess (RETURN) to Edit"

L02350:     gosub'201(0%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       L03500
                  if keyhit% <>  0 then       L02350
            if cursor%(1) > 11% then L02480           /* EDIT TABLE DATA  */
               fieldnr% = cursor%(1) - 5
               if fieldnr% =  2% then goto L02420
               if fieldnr% =  3% then goto L02420
               goto L02350
L02420:           gosub'201(fieldnr%)
                        if keyhit%  =  1 then gosub startover
                        if keyhit% <>  0 then       L02420
                  gosub'151(fieldnr%)
                        if errormsg$  <> " " then L02420
                  goto L02350
L02480:     REM EDIT TABULAR DATA FIELDS.
                if cursor%(1) < 12 or cursor%(1) > 19 then L02350
                   fieldnr% = val(str(edttran$, cursor%(2)))
                   c%, screenline% = 2 * (cursor%(1) - 12) + 1
                   if fieldnr% = 0 then L02350
                      REM SET SCREENLINE% FOR COLUMN 2
                          if fieldnr% < 3 then L02570
                             fieldnr% = 2 - mod(fieldnr%, 2)
                             c%, screenline% = screenline% + 1
L02570:               gosub'203(fieldnr%)
                            if keyhit%  =  1 then gosub startover
                            if keyhit% <>  0 then       L02570
                      gosub'153(fieldnr%)
                            if errormsg$ <> " " then L02570
            REM "Smash" the Part #s to compress out blank ones.
                     call "LINSMASH" (part$())
                      goto L02350

        REM *************************************************************~
            *      D E F A U L T / E N A B L E   F O R   I N P U T      *~
            *                                                           *~
            * SETS DEFAULTS AND INPUTS QUESTIONS FOR LINEAR INPUT.      *~
            *************************************************************

            deffn'161(fieldnr%)
                  inpmessage$ = " "
                  on fieldnr% gosub L02760 ,         /* CUMULATIVE       */~
                                    L02800 ,         /* MAX # OF LEVELS  */~
                                    L02860           /* Price Code       */
                     return
L02760:     REM DEFAULT/ENABLE FOR CUMULATIVE
                enabled% = 0%
                return
L02800:     REM DEFAULT/ENABLE FOR MAXIMUM NUMBER OF LEVELS
                enabled% = 1%
                maxlevels$ = "3"
                inpmessage$ = "Priced BOM'S allow up to '9' Levels to be ~
        ~Printed"
                return
L02860:     REM DEFAULT/ENABLE FOR PRINT SUMMARY REPORT
                enabled% = 1%
                price$ = "A"
                inpmessage$ = "Enter Selected Price Code 'A' thru 'P'"
                return

        REM *************************************************************~
            *      D E F A U L T / E N A B L E   F O R   P A R T S      *~
            *                                                           *~
            * SETS DEFAULTS AND ENABLES INPUT FOR THE PART NUMBER SCREEN*~
            * A FIELD IS DISABLED IF THE PREVIOUS PART FIELD IS BLANK.  *~
            *************************************************************

            deffn'163(fieldnr%)
                  enabled% = 0
                  inpmessage$ = " "
                  on fieldnr% gosub L03040 ,         /* PART NUMBER      */~
                                    L03120           /* QUANTITY TO BUILD*/
                     return
L03040:     REM DEFAULT/ENABLE FOR PART NUMBER
                enabled% = 1
             inpmessage$ = "Enter the desired Part # or partial to see "&~
                "list. Leave blank to Edit."
                   return

L03120:     REM DEFAULT/ENABLE FOR QUANTITY TO BUILD
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

L03500: REM *************************************************************~
            *   P E R F O R M   S U M M A R Y    A N A L Y S I S        *~
            *                                                           *~
            *                                                           *~
            * SAME TECHNIQUE AS IN BOMOA.  THE SYSTEM DOESN'T SEE ANY   *~
            * ON-HAND INVENTORY SO THE COMPLETE SUMMARY BILL IS PROC-   *~
            * SSED FOR EACH PART, THEN CUMULATED FOR ALL ASSEMBLIES     *~
            * SPECIFIED.                                                *~
            *************************************************************

            page% = 0
            call "SHOSTAT" ("BOM Mark up to Standard in Process")
            call "WORKOPEN" (# 9, "IO   ", 1000%, f2%(9))

            for part% = 1 to 16
                if str(part$(part%),1,25) = " " then L03780
                   l% = 0
                   line% = 1000
                put #5, using L03690 , part$(part%), " ","001"," ",        ~
                                   0, 1, 0, 0, " ", " "
L03690:            FMT CH(25), CH(25), CH(3),CH(3), 4*PD(14,4), CH(2),   ~
                       POS(99), CH(04)
                   gosub L05700            /* FORMAT HDRDESCR$           */
                   gosub'7(" ", 1)       /* START WITH FAKE RECORD.    */
                   REM PRINT TAG LINE FOR THIS PART'S OA.
                       if tagprinted% <> 0 then L03780
                          gosub L05050     /* PAGE CONTROL SUBROUTINE    */
                          if tagprinted% <> 0 then L03780
                             print using L05320
                             tagprinted% = 1
L03780:         next part%

             anotherflag% = 1%
             close #9
             call "FILEBGON" (# 9)
             close printer
             goto inputmode

            deffn'7(part$, r(l%+1))
                  l% = l% + 1
                     gosub'22 (part$, bom$)
                  p$(l%) = str(part$,1,25) & str(bom$,1,3)
                  count%(l%) = 0         /* SEQUENCE NUMBER FOR PRINT  */
                  if l% = 1 then L03900    /* HANDLE FAKE REC FOR LEV 1  */

L03880:           call "PLOWNEXT" (#5, p$(l%), 28%, f1%(5))
                       if f1%(5) = 0 then L04050
                  if str(p$(l%),29,3) = "  0" then L03880
L03900:           gosub L08840             /* LOAD BOM RECORD & INFO.    */
                  gosub'10(component$)   /* LOAD COMPONENT INFORMATION */
                  needed = 1
                  REM CHECK TO SEE IF THIS PART HAS COMPONENTS OR NOT
                     gosub'22 (component$, bom$)
                      readkey$ = str(component$,1,25) & str(bom$,1,3) &  ~
                                 "  0"
                      call "READ100" (#5, readkey$, f1%(5))
                      if f1%(5) = 0 then component% = 0                  ~
                                    else component% = 1
                  count%(l%) = count%(l%) + 1
                  if component% = 0 or l% = maxlevels% + 1               ~
                     then gosub L04560               /* ATOM CASE        */~
                     else gosub L04090               /* (SUB)ASSEMBLY    */
                  goto L03880

L04050:           REM END ROUTINE GRACEFULLY.
                      l% = l% - 1
                      return

L04090:     REM ROUTINE TO COST   PARTS FOR THOSE THAT ARE MANUFACTURED
                if needed <= avail then dig% = 0% else dig% = 1%
                build  = max(0, needed - avail)
                if mkr$ = "UU" then goto L04150
                if mkr$ = "PH" then goto L04150
                manuf = manuf + build
L04150:         if mkr$ = "PH" then goto L04170
                committed = committed + needed
L04170:         if mkr$ = "UU" then mkk$ = "UU"
                if mkr$ = "PH" then mkk$ = "PH"
                if mkr$ = "OP" then mkk$ = "OP"
                rewrite #9, using L05620 , component$, descr$, onhand,     ~
                                         committed ,buy, manuf,   mkk$
                REM PRINT AN ENTRY.
                    init(" ") print$()
                    if mkr$ <> "PH" then goto L04300
                    init(" ") print$()
                     print$(2) = "PHANTOM "
                     goto L04500

L04300: REM Get total standard cost via STCCOSTS -- 05/13/87 -- JIM *****
                call "STCCOSTS" (component$, " ", #34, 1%, stdcost)

                    mat prices = zer
                    pricekey$ = "C" & component$
                    call "CPRUPDSB" (#12, 0%, "00", pricekey$, 0%,f1%(12))
                       if f1%(12) = 0% then L04337
                    get #12, using L04336, prices()
L04336:                 FMT POS(57), 16*PD(14,4)
L04337:             prices(selected%) = max(prices(selected%), 0)
            call "CONVERT" (prices(selected%), 2.4, print$(1))

             if needed * stdcost = 0 then print$(3) = "      0.00"       ~
                 else call "CONVERT" (needed * stdcost, 2.4, print$(3))
             if needed * stdcost <> 0 then goto L04456
                    print$(2) = "      0.00"
                    goto L04470
L04456:     call "CONVERT" (prices(selected%) / (needed * stdcost),      ~
                    2.2, print$(2))

L04470:              if mkr$ <> "UU" then goto L04500
                     print$(2) = "USEUP PART"

L04500:             gosub L04870           /* PRINT INDENTED ENTRY.      */
                REM NOW DIG IF ANY TO BUILD FROM COMPONENTS.
                    if dig% = 0 then return
                       gosub'7 (component$, build)
                       return

L04560:     REM ROUTINE TO COMMIT PARTS FOR THOSE THAT ARE PURCHASED
                committed = committed + needed
                buyy   = max(0, needed - avail)
                buy = buy + buyy
                if mkr$ = "UU" then mkk$ = "UU"
                if mkr$ = "PH" then mkk$ = "PH"
                if mkr$ = "OP" then mkk$ = "OP"
                rewrite #9, using L05620 , component$, descr$, onhand,     ~
                          committed , buy, manuf,   mkk$
                REM PRINT AN ENTRY FOR ATOMS.
                    init(" ") print$()

        REM Get total standard cost via STCCOSTS -- 05/13/87 -- JIM *****
                call "STCCOSTS" (component$, " ", #34, 1%, stdcost)

                    mat prices = zer
                    pricekey$ = "C" & component$
                    call "CPRUPDSB" (#12, 0%, "00", pricekey$, 0%,f1%(12))
                       if f1%(12) = 0% then L04683
                    get #12, using L04682, prices()
L04682:                 FMT POS(57), 16*PD(14,4)
L04683:             prices(selected%) = max(prices(selected%), 0)
            call "CONVERT" (prices(selected%), 2.4, print$(1))

             if needed * stdcost = 0 then print$(3) = "      0.00"       ~
                else call "CONVERT" (needed * stdcost, 2.4, print$(3))
                    if needed * stdcost <> 0 then goto L04755
                     print$(2) = "      0.00"
                     goto L04840
L04755:     call "CONVERT" (prices(selected%) / (needed * stdcost),      ~
                    2.2, print$(2))
L04840:             gosub L04870
                return

L04870:         REM ROUTINE THAT INDENTS, FORMATS ENTRIES, AND PRINTS.
                    prtpart$ = " "
                    put str(prtpart$, l% * 3 - 2, 30), using L05020 ,      ~
                        count%(l%), component$
                    gosub L05050
                    print using L05400 , prtpart$, print$(1), print$(2),   ~
                            print$(3)
                    tagprinted% = 0
                    prtpart$ = " "
                    put str(prtpart$, l% * 3 - 2, 37), using L05030 ,      ~
                        descr$
                    gosub L05050
            print using L05400 , prtpart$, " ", " ", " "
                    tagprinted% = 0
                    return
L05020:                 %###. #########################
L05030:                 %     ################################

L05050:         REM PAGE CONTROL ROUTINE FOR THE ORDER ANALYSIS
                    select printer (134)
                    line% = line% + 1
                    if line% < 60 then return
                       if page% = 0 then L05130
                          if tagprinted% <> 0 then L05130
                             print using L05320
                             tagprinted% = 1
L05130:                page% = page% + 1
                       print page
                       call "DATE" addr("HD", hdrdate$)
                       print using L05280 , page%
                       print
                       print
                       print using L05300  , hdrdescr$, hdrdate$
                       print using L05320
                       print using L05340
                       print using L05360, price$
                       print using L05380
                       print using L05320
                       line% = 10%
                       return

L05280: %PAGE####  B I L L   O F   M A T E R I A L S   M A R K - U P   A ~
        ~T   P R I C E   C A T E G O R Y   S P E C I F I E D
L05300: %          ######################################################~
        ~############     #############################################
L05320: %+----------------------------------------------------------+----~
        ~------+----------+----------+
L05340: %!                                                          ! CAT~
        ~EGORY ! MARK-UP  !   UNIT   !
L05360: %!     P A R T   C O D E   /   D E S C R I P T I O N        !    ~
        ~ #    !   TO     ! STANDARD !
L05380: %!                                                          !   P~
        ~RICE  ! STANDARD !   COST   !
L05400: %!##########################################################!####~
        ~######!##########!##########!

            REM ROUTINE TO LOAD COMPONENT PART INFORMATION.
                deffn'10(part$)
L05450:         call "READ101" (#9, part$, f1%(9))
                     if f1%(9) = 0 then L05530       /* MAKE NEW WF ENTRY*/
              get #9, using L05620 , part$, descr$, onhand, committed,     ~
                     buy, manuf, mkk$

                avail = 0
                return

L05530:         REM IF NOT FOUND, CREATE NEW WORK FILE ENTRY FOR THIS P/N.
                    toh, buy, manuf, onhand, committed = 0
                    call "DESCRIBE" (#4, part$, descr$, 0%, f1%(4))
                    write #9, using L05620 , part$, descr$, toh   , 0 , 0, ~
                               0, mkr$

                    goto L05450

L05620:         FMT CH(25),              /* PART NUMBER KEY TO FILE    */~
                    CH(32),              /* PART DESCRIPTION           */~
                    PD(14,4),            /* QUANTITY ON HAND           */~
                    PD(14,4),            /* QUANTITY COMMITTED         */~
                    PD(14,4),            /* QUANTITY TO BUY            */~
                    PD(14,4),            /* QUANTITY TO MANUFACTURE    */~
                    CH(2)                /* BOM MARKER                 */

L05700: REM Formats HDRDESCR$, Which tellsshat we asked for!
                call "DESCRIBE" (#4, part$(part%), readkey$, 1%, f1%(4))
                if f1%(4) = 0 then readkey$ = "(PART NOT ON FILE)"
                hdrdescr$ = "PRICE & STD COST MARK-UP FOR PART "         ~
                             & part$(part%) & " " & readkey$
                return

        REM *************************************************************~
            *  A N S W E R   Q U E S T I O N S / I N P U T   P A R T S  *~
            *                                                           *~
            * ANSWER QUESTIONS AND INPUT PART NUMBER(S) TO PROCESS FOR  *~
            * THE ORDER ANALYSIS BIT.                                   *~
            *************************************************************

            deffn'201(fieldnr%)
                  if fieldnr% = 0% then pf16$ = "(16)Print Report"       ~
                                   else pf16$ = "(16)Exit Program"
                  init(hex(84)) lfac$(), fac$()
                  on fieldnr% gosub L07110 ,         /* CUMULATIVE?      */~
                                    L07110 ,         /* MAXIMUM # LEVELS */~
                                    L07110           /* PRINT SUMMARY?   */
                     goto L07340

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      lfac$(fieldnr%) = hex(80)
                      return
L07110:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM SET FAC'S FOR NUMERIC ONLY INPUT
                      lfac$(fieldnr%) = hex(82)
                      return

            deffn'203(fieldnr%)
                  init(hex(84)) fac$(), lfac$()
                  on fieldnr% gosub L07270 ,         /* PART NUMBER      */~
                                    L07300           /* QUANTITY TO MAKE */
                     goto L07340

                  REM SET FAC'S FOR UPPER/LOWER CASE INPUT
                      fac$(screenline%, fieldnr%) = hex(80)
                      return
L07270:           REM SET FAC'S FOR UPPER CASE ONLY INPUT
                      fac$(screenline%, fieldnr%) = hex(81)
                      return
L07300:           REM SET FAC'S FOR NUMERIC ONLY INPUT
                      fac$(screenline%, fieldnr%) = hex(82)
                      return

L07340:     accept                                                       ~
               at (01,02),                                               ~
               "Bill Of Materials Mark-Up Analysis",                     ~
               at (01,59), "Today's Date:",                              ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
               at (05,02), fac(hex(84)), infomsg$               , ch(79),~
               at (07,02),                                               ~
                  "How Many BOM Levels Do You Want Costed? (1 thru 9)",  ~
               at (07,55),                                               ~
                  fac(lfac$(2)), maxlevels$, ch(1),                      ~
               at (08,02),                                               ~
                  "Which Price Category Do You Want? ('A' thru 'P')",    ~
               at (08,55), fac(lfac$(3)),   price$              , ch( 1),~
               at (10,02), fac(hex(ac)),    line10$             , ch(79),~
               at (12,02), fac(fac$( 1,1)), part$    ( 1)       , ch(25),~
               at (12,45), fac(fac$( 2,1)), part$    ( 2)       , ch(25),~
               at (13,02), fac(fac$( 3,1)), part$    ( 3)       , ch(25),~
               at (13,45), fac(fac$( 4,1)), part$    ( 4)       , ch(25),~
               at (14,02), fac(fac$( 5,1)), part$    ( 5)       , ch(25),~
               at (14,45), fac(fac$( 6,1)), part$    ( 6)       , ch(25),~
               at (15,02), fac(fac$( 7,1)), part$    ( 7)       , ch(25),~
               at (15,45), fac(fac$( 8,1)), part$    ( 8)       , ch(25),~
               at (16,02), fac(fac$( 9,1)), part$    ( 9)       , ch(25),~
               at (16,45), fac(fac$(10,1)), part$    (10)       , ch(25),~
               at (17,02), fac(fac$(11,1)), part$    (11)       , ch(25),~
               at (17,45), fac(fac$(12,1)), part$    (12)       , ch(25),~
               at (18,02), fac(fac$(13,1)), part$    (13)       , ch(25),~
               at (18,45), fac(fac$(14,1)), part$    (14)       , ch(25),~
               at (19,02), fac(fac$(15,1)), part$    (15)       , ch(25),~
               at (19,45), fac(fac$(16,1)), part$    (16)       , ch(25),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02),                                               ~
                  "(1)Start Over",                                       ~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65), fac(hex(8c)), pf16$,                   ch(16),~
                                                                         ~
               keys(hex(00010d0f10)),                                    ~
               key (keyhit%)

               if keyhit% <> 13 then L08000
                  call "MANUAL" ("BOMMRKUP")
                  goto L07340

L08000:        if keyhit% <> 15 then L08040
                  call "PRNTSCRN"
                  goto L07340

L08040:        REM GET CURSOR LOCATION FOR EDIT MODE COMPUTATIONS.
                   close ws
                   call "SCREEN" addr ("C", 0%, "I", i$(), cursor%())
                   return

        REM *************************************************************~
            *    T E S T   D A T A   F O R   L I N E A R   I N P U T    *~
            *                                                           *~
            * TESTS DATA FOR LINEAR INPUT MODE.  CHECK THE PARAMETERS   *~
            * TO SEE THAT THEY ALL LINE UP.                             *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L08220 ,         /* CUMULATIVE?      */~
                                    L08260 ,         /* MAX # OF LEVELS  */~
                                    L08380           /* SUMMARY REPORT   */
                     return
L08220:     REM TEST DATA FOR CUMULATIVE? FLAG
                return

L08260: REM TEST DATA FOR MAXIMUM NUMBER OF LEVELS
            if maxlevels$ = " " then maxlevels$ = "9"
            if maxlevels$ >= "1" and maxlevels$ <= "9" then L08330
               errormsg$ = "Invalid Entry For Maximum Number Of Levels: "~
                             & maxlevels$
            return

L08330:     convert maxlevels$ to maxlevels%
            if maxlevels% >= 1 and maxlevels% <= 9  then return
            errormsg$= "Maximum Number Of Levels Must Be Between 1 & 9: "~
                          & maxlevels$
            return

L08380: REM TEST DATA for Price Code
            if price$ > "P" or price$ < "A" then goto L08422
            selected% = val(price$) - 64%
            return

L08422:     errormsg$ = "Please Enter Price Category A thru P"
            return

        REM *************************************************************~
            *      T E S T   T A B U L A R   I N F O R M A T I O N      *~
            *                                                           *~
            * TESTS TABULAR INFORMATION TO MAKE SURE THAT THE DATA IS   *~
            * OK.  ZAP QUANTITY IF BLANK PART NUMBER ENTERED.           *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L08550 ,         /* PART NUMBER      */~
                                    L08700           /* QUANTITY TO BUILD*/
                     return
L08550:     REM TEST DATA FOR PART NUMBER
                if part$(c%) <> " " then L08590
                     quantity$(c%) = " " : return
L08590:         call "GETCODE" (#4, part$(c%), infomsg$, 1%, 0, f1%(4))
                if f1%(4) = 0 then L08680
                gosub'22 (part$(c%), bom$)
                readkey$ = str(part$(c%),1,25) & str(bom$,1,3) & "  0"
                call "READ100" (#5, readkey$, f1%(5))
                if f1%(5) = 0 then L08660
                return
L08660:         errormsg$ = "Part Has No Components: " & part$(c%)
                return
L08680:         errormsg$ = "Part Number Not On File: " & part$(c%)
                return
L08700:     REM TEST DATA FOR QUANTITY TO BUILD
                if quantity$(c%) = " " then quantity$(c%) = "1"
                call "NUMTEST" (quantity$(c%), 1, 9e7, errormsg$, -0.2,  ~
                                quantity)
                return

L08840: REM *************************************************************~
            * L O A D   B I L L   O F   M A T E R I A L S   R E C O R D *~
            *                                                           *~
            * LOADS THE BILL OF MATERIALS RECORD FROM DISK.             *~
            *************************************************************

            get    #5, using L08940 ,                                      ~
                       component$, quantity, xused, mkr$
                     xused = xused
            return

L08940:     FMT CH(25),                  /* COMPONENT PART NUMBER      */~
                XX(25),                  /* ASSEMBLY PART NUMBER       */~
                XX(3),                   /* WHICH ALTERNATE BOM        */~
                XX(3),                   /* SEQUENCE NUMBER            */~
                PD(14,4),                /* QUANTITY REQUIRED          */~
                PD(14,4),                /* TIMES USED (SIZE)          */~
                POS(89),                                                 ~
                CH(2)                    /* BOM MARKER                 */

        REM FIND THE CURRENTLY EFFECTIVE BILL STRUCTURE FOR THE PART
        REM PASSED IN

        deffn'22(part22$, bom$)
           bom$ = "001"
           call "READ100" (#24, str(part22$,1,25) & "1001", f1%(24))
                     if f1%(24) <> 1% then return

           get #24, using L09110    ,  b$()
L09110:        FMT  XX(29),  490*CH(3)
               bom$ = b$(today%)
               return


L09160: REM *************************************************************~
            *                          E X I T                          *~
            *                                                           *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *************************************************************

            call "SHOSTAT" ("Closing Files, One Moment Please")
            end
