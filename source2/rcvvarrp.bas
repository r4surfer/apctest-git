        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  RRRR    CCC   V   V  V   V   AAA   RRRR   RRRR   PPPP    *~
            *  R   R  C   C  V   V  V   V  A   A  R   R  R   R  P   P   *~
            *  RRRR   C      V   V  V   V  AAAAA  RRRR   RRRR   PPPP    *~
            *  R   R  C   C   V V    V V   A   A  R   R  R   R  P       *~
            *  R   R   CCC     V      V    A   A  R   R  R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * RCVVARRP - THIS PROGRAM REPORTS THE DIFFERENCE BETWEEN THE*~
            *            PRICE ON THE PURCHASE ORDER AND THE INVOICED   *~
            *            PRICE. IT ALSO REPORTS ON THE DIFFERENCE       *~
            *            THE STANDARD COST AND THE INVOICED PRICE.      *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/10/87 ! Original                                 ! JDH  ~
            * 06/06/88 ! Clean up for R5.01                       ! JDH  ~
            * 08/28/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            blankdate$8,                 /* Blank Date for Comparison  */~
            company$50,                  /* Company Header             */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            edate$10,                    /* Unformated Ending Date     */~
            enddate$10,                  /* ENDING   DATE              */~
            endpart$25,                  /* ENDING   PART NUMBER       */~
            endvend$9,                   /* ENDING   VENDOR CODE       */~
            errormsg$79,                 /* Error message              */~
            flag$1,                      /* Flag for first record      */~
            from$4,                      /* Constant "FROM"            */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lastpart$25,                 /* Last Part # used           */~
            lastvend$9,                  /* Last Vendor Code used      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            part$25,                     /* Part Number                */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            pono$16,                     /* Purchase Order Number      */~
            printcode$1,                 /* Print by 0, 1, 2, or 3     */~
            printmsg$21,                 /* Type of Report Message     */~
            rcvdate$8,                   /* Date Received from RCVLINES*/~
            rcvno$16,                    /* Receiver Number            */~
            sdate$10,                    /* Unformated Startdate       */~
            set$8,                       /* Needed to open STCHNY      */~
            sortcode$1,                  /* SORT BY (P)ART or (V)ENDOR */~
            startdate$10,                /* STARTING DATE              */~
            startpart$25,                /* STARTING PART NUMBER       */~
            startvend$9,                 /* STARTING VENDOR CODE       */~
            today$8,                     /* TODAY'S DATE               */~
            to$2,                        /* Constant "TO"              */~
            uom$4,                       /* Unit Of Measure            */~
            userid$3,                    /* Current User Id            */~
            vbklseq$3,                   /* PO Line Sequence Number    */~
            vendcode$9                   /* Vendor Code                */

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
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

            enddate%, startdate%, err%, ret% = 0%
            pprice, iprice, sprice = 0

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * # 1 ! HNYMASTR ! Inventory Master File                    *~
            * # 2 ! VENDOR   ! VENDOR MASTER RECORD                     *~
            * # 3 ! STCHNY   ! Standard Cost Set- Inventory Standards   *~
            * # 4 ! RCVLINES ! Receiver Line Items File  (Purchasing)   *~
            * # 5 ! WORK1    ! Temporary System Workfile                *~
            * # 6 ! SYSFILE2 ! Caelus Management System Information     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select # 1, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup     ~

            select # 2, "VENDOR",                                        ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup     ~

            select # 3, "STCHNY",                                        ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos = 1,    keylen = 25                       ~

            select # 4, "RCVLINES",                                      ~
                        varc,     indexed,  recsize =  800,              ~
                        keypos =   26, keylen =  52,                     ~
                        alt key  1, keypos =    1, keylen =  69,         ~
                            key  2, keypos =   42, keylen =  36,         ~
                            key  3, keypos =  128, keylen =  24          ~

            select # 5, "WORK1",                                         ~
                        varc,     indexed,  recsize =  160,              ~
                        keypos =    1, keylen =  81,                     ~
                        alt key  1, keypos =   82, keylen =  42, dup,    ~
                            key  2, keypos =   40, keylen =  42, dup

            select # 6, "SYSFILE2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 500,                                  ~
                         keypos = 1, keylen = 20


            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (# 1, fs%( 1), f2%( 1), 0%, rslt$( 1))
            call "OPENCHCK" (# 2, fs%( 2), f2%( 2), 0%, rslt$( 2))
        REM CALL "OPENCHCK" (# 3, FS%( 3), F2%( 3), 0%, RSLT$( 3))
            call "OPENCHCK" (# 4, fs%( 4), f2%( 4), 0%, rslt$( 4))
            /* WILL OPEN THE WORKFILE WHEN NEEDED */
            call "OPENCHCK" (# 6, fs%( 6), f2%( 6), 0%, rslt$( 6))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            today$=date$
            call "DATEFMT" (date$)
            set$ = " "
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "RCVVARRP: " & str(cms2v$,,8)

            call "STCFOPEN" (set$, "Sxxxxx", #6, errormsg$,              ~
                                                 #3, #3, #3, #3, #3, #3)
            if errormsg$ = " " then L09140
                call "ASKUSER" (0%, "STANDARD COST MATRIX",              ~
                                "Unable to open Cost Set:", errormsg$,   ~
                                "Press RETURN to exit display...")
                goto exit_program

L09140:     from$ = "From"
            to$ = "To"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  8%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
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
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       datasave
                  if keyhit% <>  0% then       editpg1
L11111:     if cursor%(1%) < 7% then cursor%(1%) = 7%
            if cursor%(1%) > 13% then cursor%(1%) = 13%
            if cursor%(1%) = 11% then cursor%(1%) = 9%
                  fieldnr% = cursor%(1%) - 5%
            if cursor%(1%) <> 7% then goto L11126
            if cursor%(2%) < 50% then fieldnr% = 1% else fieldnr% = 2%
            goto L11140
L11126:     if cursor%(1%) <> 8% then goto L11131
            if cursor%(2%) < 50% then fieldnr% = 3% else fieldnr% = 4%
            goto L11140
L11131:     if cursor%(1%) <> 9% then L11140
            if cursor%(2%) < 50% then fieldnr% = 5% else fieldnr% = 6%

L11140:     if fieldnr% = lastfieldnr% then    editpg1
L11150:     gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%

            if fieldnr% = 1% or fieldnr% = 3% or fieldnr% = 5% then L11250
            goto L11111

L11250:     fieldnr% = fieldnr% + 1%
            goto L11150

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave

            gosub build_the_files
            gosub print_report

              if f2%(5) <> 0% then L19990
              call "FILEBGON" (#5)
              f2%(5) = 1%

L19990:     goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* STARTING PART NUM      */~
                              L20200,         /* ENDING   PART NUM      */~
                              L20300,         /* STARTING DATE          */~
                              L20400,         /* ENDING   DATE          */~
                              L20500,         /* STARTING VENDOR        */~
                              L20600,         /* ENDING   VENDOR        */~
                              L20700,         /* (P)ART or (V)END       */~
                              L20800          /* 0, 1, 2, or 3          */
            return
L20100: REM Def/Enable STARTING PART NUMBER        STARTPART$
            if startpart$ <> " " then return
            if endpart$ <> " " then return
            startpart$ = "ALL"
            return

L20200: REM Def/Enable ENDING   PART NUMBER        ENDPART$
            if startpart$ <> "ALL" then L20225
            enabled% = 0% : return
L20225:     if startpart$ = "FIRST" then endpart$ = "LAST"
            if endpart$ = " " and startpart$ <> "ALL" then L20250
            return
L20250:     endpart$ = startpart$
            return

L20300: REM Def/Enable STARTING DATE               STARTDATE$
            if startdate$ <> " " and startdate$ <> blankdate$ then return
            call "DATE" addr("G+", today$, -30%, startdate$, err%)
            call "DATFMTC" (startdate$)
            return

L20400: REM Def/Enable ENDING   DATE               ENDDATE$
            if enddate$ <> " " and enddate$ <> blankdate$ then return
            enddate$ = today$
            call "DATFMTC" (enddate$)
            return

L20500: REM Def/Enable STARTING VENDOR CODE        STARTVEND$
            if startvend$ <> " " then return
            if endvend$ <> " " then return
            startvend$ = "ALL"
            return

L20600: REM Def/Enable ENDING   VENDOR CODE        ENDVEND$
            if startvend$ <> "ALL" then L20625
            enabled% = 0% : return
L20625:     if startvend$ = "FIRST" then endvend$ = "LAST"
            if endvend$ = " " and startvend$ <> "ALL" then L20650
            return
L20650:     endvend$ = startvend$
            return

L20700: REM Def/Enable SORT BY (P)ART or (V)ENDOR  SORTCODE$
            if sortcode$ <> " " then L20790
            sortcode$ = "P"
L20790:     return

L20800: REM Def/En Print By 0, 1, 2, or 3           PRINTCODE$
            if printcode$ <> " " then L20890
            printcode$ = "0"
L20890:     return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28055
                inpmessage$ = edtmessage$
                return

L28055
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Starting Part Number                                   ",~
         "Enter  Ending  Part Number                                   ",~
         "Enter Starting Date                                          ",~
         "Enter  Ending  Date                                          ",~
         "Enter Starting Vendor Code                                   ",~
         "Enter  Ending  Vendor Code                                   ",~
         "Enter  'P' for Sort by PART or 'V' for Sort by VENDOR        ",~
         "0 = PO or STD var. <> 0;  1 = PO var. <> 0;  2 = STD var. <> 0;~
        ~    3 = All"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      enddate$, endpart$, endvend$, sortcode$,           ~
                      startdate$, startpart$, startvend$, printcode$
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
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
            *    B U I L D   T H E   W O R K F I L E   R E C O R D S    *~
            *-----------------------------------------------------------*~
            * MARCH THROUGH RCVLINES AND BUILD THE WORKFILE WITH RECORDS*~
            *      THAT SATISFY THE PART, DATE, AND VENDOR RANGES.      *~
            *   THE WORKFILE INCLUDES ALL DATA NEEDED FOR THE REPORT.   *~
            *     ALT 1 FOR SORT BY PART, ALT 2 FOR SORT BY VENDOR.     *~
            *************************************************************
        build_the_files
            call "SHOSTAT" ("BUILDING THE WORKFILES")

            REM INITIALISE to 0 or " "
                init(" ") sdate$, edate$, part$, vendcode$, rcvdate$,    ~
                          lastpart$, uom$
                pprice, iprice, sprice, quantity = 0
                time%,  rdate% = 0%

            sdate$ = startdate$
            call "DATUFMTC" (sdate$)
            edate$ = enddate$
            call "DATUFMTC" (edate$)


            init(hex(00)) plowkey$

            if startpart$ <> "ALL" then L30304
            init(hex(00)) startpart$
            init(hex(ff)) endpart$

L30304:     if startpart$ = "FIRST" then init(hex(00)) startpart$
            if endpart$ = "LAST" then init(hex(ff)) endpart$

            ret% = 0%
            str(plowkey$, 1, 25) = str(startpart$, 1, 25)

        next_rec

            call "PLOWALTS" (#4, plowkey$, 1%, 0%, f1%(1))
                if f1%(1) = 0% then return

            get  #4, using L30430,  part$, rcvno$, vendcode$, pono$,      ~
                           vbklseq$, rdate%, time%, quantity, pprice,    ~
                           uom$, iprice

L30430:         FMT  CH(25), CH(16), CH(9), CH(16), CH(3), BI(4),        ~
                     BI(4), POS(156), PD(14,4), POS(212), PD(14,7),      ~
                     POS(244), CH(4), POS(364), PD(14,7)


            if part$ > endpart$ then return

            if startvend$ = "ALL" then L30540
              if startvend$ = "FIRST" then L30515
              if vendcode$ < startvend$ then next_rec
L30515:       if endvend$ = "LAST" then L30540
              if vendcode$ > endvend$ then next_rec

L30540:     rdate% = rdate% + 19000000
            convert rdate% to rcvdate$, pic(########)
            call "DATECONV" (rcvdate$)
              if rcvdate$ < sdate$ then next_rec
              if rcvdate$ > edate$ then next_rec

            if lastpart$ = part$ then L30650
            call "READ100" (#3, part$, f1%(1))
              if f1%(1) = 0 then L30640
              get #3, using L30620 , sprice
L30620:       FMT POS(52), PD(14,4)
              goto  L30650
L30640:       sprice = 0
L30650:     lastpart$ = part$

            gosub write_workfile
            goto next_rec

        REM WRITE THE RECORDS ONTO WORKFILE ON CHANNEL #5
        write_workfile

            if f2%(5) = 0% then goto L30770

            call "WORKOPEN" (#5, "IO", 4000%, f2%(5))

L30770:     write #5, using L30810,  rcvno$, pono$, vbklseq$, time%,      ~
                  vendcode$, part$, rcvdate$, part$, vendcode$, rcvdate$,~
                    quantity, uom$, pprice, iprice, sprice

L30810:     FMT      CH(16), CH(16), CH(3), BI(4), CH(9), CH(25), CH(8), ~
                     CH(25), CH(9), CH(8), PD(14,4), CH(4), PD(14,7),    ~
                     PD(14,7), PD(14,4)

            return

        REM *************************************************************~
            *             P R I N T    R E P O R T                      *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *                                                           *~
            *************************************************************
        print_report
            init(" ") part$, vendcode$, rcvdate$, uom$, lastpart$,       ~
                      lastvend$
            quantity, pprice, iprice, sprice, povar, stdvar = 0
            poper, stdper = 0
            flag$ = "F" : printmsg$ = " "
             if printcode$ = "0" then printmsg$ = "* ALL VARIANCE *"
             if printcode$ = "1" then printmsg$ = "* ONLY PO VARIANCE *"
             if printcode$ = "2" then printmsg$ = "* ONLY STD VARIANCE *"

            call "SHOSTAT" ("Printing Variance Report")
            call "COMPNAME" (12%, company$, u3%)
            page% = 0% : line% = 999%
            select printer(134)
            call "SETPRNT" ("RCV006", " ", 0%, 0%)

        REM SET COUNTERS
            count% = 0%
            potot, intot = 0


            REM What KEY to plow on and Get first record & into the loop.
            init(hex(00)) plowkey$
            if sortcode$ = "P" then key% = 1% else key% = 2%
            call "PLOWALTS" (#5, plowkey$, key%, 0%, f1%(5))
            goto L31370

        read_record
            call "READNEXT" (#5, f1%(5))
L31370:     if f1%(5) = 0% then end_report

            gosub check_page

            get #5, using L31410, part$, vendcode$, rcvdate$, quantity,   ~
                                 uom$, pprice, iprice, sprice
L31410:     FMT     POS(82), CH(25), CH(9), CH(8), PD(14,4), CH(4),      ~
                    PD(14,7), PD(14,7), PD(14,4)
            call "DATEFMT" (rcvdate$)

            if sortcode$ = "V" then vend_print
         REM PART_PRINT
            if count% = 0% then L31490
            if part$ <> lastpart$ then gosub ave_var
L31490:     if part$ <> lastpart$ then count% = 0%
            if part$ <> lastpart$ then let potot = 0
            if part$ <> lastpart$ then let intot = 0
            count% = count% + 1%
            potot = potot + pprice
            intot = intot + iprice
            spr = sprice
            povar = pprice - iprice
              if pprice = 0 then pprice = .0000001
            poper = povar / pprice * 100
            stdvar = sprice - iprice
              if sprice = 0 then sprice = .0000001
            stdper = stdvar / sprice * 100
              if printcode$="0" and povar=0 and stdvar=0 then read_record
              if printcode$ = "1" and povar = 0 then read_record
              if printcode$ = "2" and stdvar = 0 then read_record

            if count% = 1% then print_new_part
            if lastvend$ <> vendcode$ then print_new_vendor

            print using L34740, rcvdate$, quantity, pprice, iprice,       ~
                               povar, poper, stdvar, stdper
            line% = line% + 1%
            goto read_record

         print_new_part
            lastpart$ = part$ : lastvend$ = vendcode$ : flag$ = "N"
            print
            print using L34775, part$, sprice, vendcode$, rcvdate$,       ~
                  quantity, pprice, iprice, povar, poper, stdvar, stdper
            line% = line% + 2%
            goto read_record


         ave_var
            if count% = 1% then return
            aveppr = potot / count%
            aveipr = intot / count%
            avepovar = aveppr - aveipr
              if aveppr = 0 then aveppr = .0000001
            avepoper = avepovar / aveppr * 100
            avestdvar = spr - aveipr
              if spr = 0 then spr = .0000001
            avestdper = avestdvar / spr * 100
            print using L34700
            print using L34810, aveppr, aveipr, avepovar, avepoper,       ~
                               avestdvar, avestdper
            print
            line% = line% + 3%
              return

         print_new_vendor
            lastvend$ = vendcode$
            print using L34845, vendcode$, rcvdate$, quantity, pprice,    ~
                                iprice, povar, poper, stdvar, stdper
            line% = line% + 1%
            goto read_record

          check_page
            if line% <= 56% then return
            goto print_page_heading

         print_page_heading
            page% = page% + 1% : line% = 7%
            print page
            print using L34505, date$, company$
            print using L34520, page%
            print using L34535, startdate$, enddate$, printmsg$
            print
            if sortcode$ = "P" then print_part_heading
            if sortcode$ = "V" then print_vend_heading

         print_part_heading
            print using L34550
            print using L34560
            print using L34570
            return

         print_vend_heading
            print using L34645
            print using L34655
            print using L34665
            return


         vend_print
            if count% = 0% then L33030
            if vendcode$ <> lastvend$ then lastpart$ = " "
            if part$ <> lastpart$ then gosub ave_var
L33030:     if part$ <> lastpart$ then count% = 0%
            if part$ <> lastpart$ then let potot = 0
            if part$ <> lastpart$ then let intot = 0
            count% = count% + 1%
            potot = potot + pprice
            intot = intot + iprice
            spr = sprice
            povar = pprice - iprice
              if pprice = 0 then pprice = .0000001
            poper = povar / pprice * 100
            stdvar = sprice - iprice
              if sprice = 0 then sprice = .0000001
            stdper = stdvar / sprice * 100
              if printcode$="0" and povar=0 and stdvar=0 then read_record
              if printcode$ = "1" and povar = 0 then read_record
              if printcode$ = "2" and stdvar = 0 then read_record

            if lastvend$ <> vendcode$ then print_new_vend
            if lastpart$ <> part$ then print_new_prt

            print using L34740, rcvdate$, quantity, pprice, iprice,       ~
                               povar, poper, stdvar, stdper
            line% = line% + 1%
            goto read_record

         print_new_vend
            lastpart$ = part$ : lastvend$ = vendcode$ : flag$ = "N"
            print
            print using L34860, vendcode$, part$, sprice, rcvdate$,       ~
                  quantity, pprice, iprice, povar, poper, stdvar, stdper
            line% = line% + 2%
            goto read_record

         print_new_prt
            lastpart$ = part$
            print using L34870, part$, sprice, rcvdate$, quantity, pprice,~
                                iprice, povar, poper, stdvar, stdper
            line% = line% + 1%
            goto read_record

        end_report
            if count% > 0% then L34070
                call "ASKUSER" (0%, "NOTHING TO PRINT",                  ~
                                "There is no activity to print.",  " ",  ~
                                "Press RETURN to Continue...")
            goto L34120

L34070:     gosub ave_var

            print
            print "** END OF REPORT **"
L34120:     call "SETPRNT" ("RCV006", " ", 0%, 1%)
            close printer
        return

        REM THE USINGS
L34505: %RUN DATE: ########                      ########################~
        ~####################################                RCVVARRP:RCV0~
        ~06
L34520: %                                                  PURCHASE PRICE~
        ~ VARIANCE REPORT                                          PAGE: #~
        ~##
L34535: %                                          FOR THE PERIOD: ####~
        ~###### THRU ##########           #####################

L34550: %                            CURRENT                 DATE        ~
        ~         PO         INVOICE   PO/INVOICE  PO/INV STD/INVOICE STD/~
        ~INV
L34560: %  PART NUMBER               STD COST    VENDOR    RECEIVED  QUAN~
        ~TITY  UNIT PRICE  UNIT PRICE   VARIANCE    VAR %   VARIANCE   VAR~
        ~ %
L34570: %-------------------------  ----------  ---------  --------  ----~
        ~----  ----------  ----------  ----------  ------  ----------  ---~
        ~---
L34645: %                                       CURRENT      DATE        ~
        ~          PO       INVOICE    PO/INVOICE  PO/INV STD/INVOICE STD/~
        ~INV
L34655: % VENDOR       PART NUMBER             STD COST    RECEIVED  QUAN~
        ~TITY  UNIT PRICE  UNIT PRICE   VARIANCE    VAR %   VARIANCE   VAR~
        ~ %
L34665: %---------  -------------------------  ----------  --------  ----~
        ~----  ----------  ----------  ----------  ------  ----------  ---~
        ~---
L34700: %                                                                ~
        ~      ----------  ----------  ----------  ------  ----------  ---~
        ~---
L34740: %                                                  ########  ####~
        ~###   -######.##  -######.##  -######.##  -###.#  -######.##  -##~
        ~#.#
L34775: %#########################  -######.##  #########  ########  ####~
        ~###   -######.##  -######.##  -######.##  -###.#  -######.##  -##~
        ~#.#
L34810: %                                         AVERAGE PRICES AND VARI~
        ~ANCE: -######.##  -######.##  -######.##  -###.#  -######.##  -##~
        ~#.#
L34845: %                                       #########  ########  ####~
        ~###   -######.##  -######.##  -######.##  -###.#  -######.##  -##~
        ~#.#
L34860: %#########  #########################  -######.##  ########  ####~
        ~###   -######.##  -######.##  -######.##  -###.#  -######.##  -##~
        ~#.#
L34870: %           #########################  -######.##  ########  ####~
        ~###   -######.##  -######.##  -######.##  -###.#  -######.##  -##~
        ~#.#

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

        FMT                 /* FILE: HNYMASTR                          */~
            CH(25),         /* Part Number                             */~
            CH(32),         /* Part Number Description                 */~
            CH(16),         /* Generic Part Code                       */~
            CH(4),          /* Stocking UOM                            */~
            CH(4),          /* Pricing Unit of Measure                 */~
            PD(14,4),       /* Conversion Factor                       */~
            CH(4),          /* Part Category Code                      */~
            CH(4),          /* Low Level Code (Traditional MRP / Cost R*/~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(9),          /* main vendor code                        */~
            CH(1),          /* Cycle Count Category                    */~
            CH(6),          /* Cycle Count Date                        */~
            CH(9),          /* Filler (Internal, unused space)         */~
            CH(1),          /* Taxable Purchase (Y/N)                  */~
            BI(2),          /* ATC Horizon Days                        */~
            CH(1),          /* Lot Tracking Required Flag (Y/N).       */~
            CH(1),          /* Serial Number Tracking Required Flag (Y/*/~
            CH(1),          /* Disallow quantities for this part to go */~
            CH(4),          /* User definable (free text) Class code fo*/~
            CH(2),          /* Drawing Size                            */~
            CH(16),         /* Drawing Reference                       */~
            CH(8),          /* Bin Location                            */~
            CH(1),          /* Backflush flag                          */~
            CH(2),          /* Filler (Internal, unused space)         */~
            CH(4),          /* special/obsolete code                   */~
            CH(10),         /* lead time (days)                        */~
            CH(3),          /* Type of Part in HNYMASTR file           */~
            CH(07),         /* Filler (Internal, unused space)         */~
            CH(10),         /* minimum order qty. / minimum order multi*/~
            CH(3),          /* buyer/planner code                      */~
            PD(14,4),       /* Standard Potency Factor.                */~
            CH(96),         /* Filler (Internal, unused space)         */~
            CH(1),          /* inventory costing method                */~
            CH(1),          /* Filler (Internal, unused space)         */~
            CH(3),          /* Second buyer / planner                  */~
            CH(6),          /* Filler (Internal, unused space)         */~
            PD(14,4),       /* Safety stock level                      */~
            PD(14,4),       /* Manufacturing/Purchasing increment      */~
            CH(1),          /* priority code                           */~
            CH(9),          /* Inventory Source Account for Purchases  */~
            CH(9),          /* Inventory Source Account for Manufacture*/~
            CH(9),          /* Inventory Assets Account                */~
            CH(9),          /* Cost of Goods Sold Account              */~
            CH(9),          /* Sales Distribution Account              */~
            CH(9),          /* Inventory Adjustments Account           */~
            12*CH(9),       /* Inventory Variance Accounts             */~
            CH(9),          /* Discounts Account                       */~
            CH(200),        /* Variable Fields Data Area               */~
            CH(195)         /* Filler (Internal, unused space)         */~

        FMT                 /* FILE: VENDOR                            */~
            CH(9),          /* Vendor Code                             */~
            CH(30),         /* Vendor Description (aka Sort Name)      */~
            CH(30),         /* vendor name                             */~
            5*CH(30),       /* vendor billing address                  */~
            CH(20),         /* contact's name                          */~
            CH(10),         /* vendor phone number                     */~
            CH(9),          /* Purchases Account Number                */~
            CH(9),          /* Payables Account                        */~
            CH(9),          /* cash in bank account                    */~
            CH(9),          /* discounts taken account                 */~
            PD(14,4),       /* bills due (days) (for prox, this < 0)   */~
            PD(14,4),       /* discounts due (days) (for prox. this is */~
            PD(14,4),       /* vendor discount percent                 */~
            PD(14,4),       /* current outstanding balance             */~
            CH(9),          /* Price - Cost Variance G/L Account Code  */~
            CH(87),         /* Filler (Internal, unused space)         */~
            CH(1),          /* P.O. Conformation Flag (Y/N)            */~
            CH(1),          /* Freight Terms                           */~
            CH(30),         /* F.O.B.                                  */~
            CH(1),          /* Taxable Purchase (Y/N)                  */~
            CH(30),         /* ship via free text field                */~
            CH(4),          /* Vendor Type                             */~
            CH(9),          /* Interim Liabilities Account Code        */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(9),          /* Main vendor number.                     */~
            CH(12),         /* Federal Tax Identification Number       */~
            CH(4),          /* 1099 Category Code                      */~
            CH(9),          /* freight account                         */~
            CH(73)          /* Filler (Internal, unused space)         */~

        FMT                 /* FILE: STCHNY                            */~
            CH(25),         /* Part code                               */~
            CH(8),          /* Mapping ID                              */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(3),          /* The specific BOM identifier for a Bill o*/~
            CH(3),          /* The specific routing to use for a Bill. */~
            PD(14,4),       /* Standard run quantity for standard costi*/~
            PD(14,4),       /* Total Standard Cost                     */~
            12*PD(14,4),    /* Costs derived from the BOM              */~
            12*PD(14,4),    /* Route Costs                             */~
            12*PD(14,4),    /* Miscellaneous Cost Detail's Values      */~
            CH(3),          /* The specific BOM identifier for a Bill o*/~
            CH(3),          /* The specific routing to use for a Bill. */~
            CH(147)         /* Filler (Internal, unused space)         */~

        FMT                 /* FILE: RCVLINES                          */~
            CH(25),         /* Part code                               */~
            CH(16),         /* Receiver Control Number                 */~
            CH(9),          /* Vendor code                             */~
            CH(16),         /* Purchase Order Number                   */~
            CH(3),          /* Purchase Line Sequence Number (not ITEM */~
            BI(4),          /* System Date                             */~
            BI(4),          /* The System Time when a transaction was e*/~
            CH(32),         /* part number description                 */~
            CH(6),          /* Date Ordered (Purchase Order Date)      */~
            CH(6),          /* P.O. Line Item Due Date                 */~
            CH(6),          /* P.O. Line Item Date of Last Receipt -Las*/~
            CH(16),         /* Packing Slip # for last quantity receive*/~
            BI(4),          /* System Date                             */~
            BI(4),          /* The System Time when a transaction was e*/~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            PD(14,4),       /* Total Quantity Received To Date - Purcha*/~
            PD(14,4),       /* Total Quantity currently in Receiving Ho*/~
            PD(14,4),       /* Quantity currently in QC (Pending)-Purch*/~
            PD(14,4),       /* Total Quantity currently in QC Hold (P.O*/~
            PD(14,4),       /* Total quantity rejected /  scrapped.    */~
            PD(14,4),       /* Total Quantity Returned to Vendor.      */~
            PD(14,4),       /* Total quantity moved to on-hand inventor*/~
            PD(14,7),       /* Purchase Price                          */~
            2*PD(14,7),     /* Filler (Internal, unused space)         */~
            PD(14,4),       /* # of Stocking (base) Units which go into*/~
            CH(4),          /* Unit of Measure                         */~
            PD(14,4),       /* Extension Amount (Quantity * Price)     */~
            PD(14,4),       /* Value of Receiver Line that HASN'T been */~
            PD(14,4),       /* Value of Receiver Line that HAS been inv*/~
            PD(14,4),       /* Accounts Payable Adjustment Amount      */~
            PD(14,4),       /* Returned To Vendor Adjustment Amount    */~
            PD(14,4),       /* Filler (Internal, unused space)         */~
            CH(9),          /* Payables Account                        */~
            CH(9),          /* General Ledger Account Number           */~
            CH(9),          /* General Ledger Account Number           */~
            CH(9),          /* Expenses Account Number.                */~
            CH(3),          /* Store or Warehouse Code                 */~
            CH(6),          /* Which lot in inventory - always used wit*/~
            CH(3),          /* Warehouse or Store                      */~
            CH(6),          /* Lot Number                              */~
            CH(6),          /* rejection code  ccccss fmt              */~
            PD(14,4),       /* Quantity of Something                   */~
            PD(14,7),       /* Unit Price per Vendor's Invoice.        */~
            2*PD(14,4),     /* Filler (Internal, unused space)         */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            BI(4),          /* serial index to link SERTIF             */~
            CH(8),          /* Job Number                              */~
            PD(14,4),       /* Total Cost                              */~
            12*PD(14,4),    /* Inventory Costs                         */~
            PD(14,4),       /* Total Cost                              */~
            12*PD(14,4),    /* Inventory Costs                         */~
            CH(185)         /* Filler (Internal, unused space)         */~

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40105,         /* STARTING PART NUM */   ~
                                L40105,         /* ENDING   PART NUM */   ~
                                L40105,         /* STARTING DATE     */   ~
                                L40105,         /* ENDING   DATE     */   ~
                                L40105,         /* STARTING VENDOR   */   ~
                                L40105,         /* ENDING   VENDOR   */   ~
                                L40105,         /* (P)ART or (V)END  */   ~
                                L40105          /* PRINTCODE$        */
              goto L40120

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40105:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40120:     accept                                                       ~
               at (01,02),                                               ~
                  "Purchase Price Variance Report",                      ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,20), fac(hex(ac)), from$                  , ch(25),~
               at (06,50), fac(hex(ac)), to$                    , ch(25),~
                                                                         ~
               at (07,02), "Part Number",                                ~
               at (07,20), fac(lfac$( 1)), startpart$           , ch(25),~
               at (07,50), fac(lfac$( 2)), endpart$             , ch(25),~
                                                                         ~
               at (08,02), "Date",                                       ~
               at (08,20), fac(lfac$( 3)), startdate$           , ch(10),~
               at (08,50), fac(lfac$( 4)), enddate$             , ch(10),~
                                                                         ~
               at (09,02), "Vendor Code",                                ~
               at (09,20), fac(lfac$( 5)), startvend$           , ch(09),~
               at (09,50), fac(lfac$( 6)), endvend$             , ch(09),~
                                                                         ~
               at (12,02), "Sort By",                                    ~
               at (12,20), fac(lfac$( 7)), sortcode$            , ch(01),~
                                                                         ~
               at (13,02), "Print By",                                   ~
               at (13,20), fac(lfac$( 8)), printcode$           , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40320
                  call "MANUAL" ("RCVVARRP") : goto L40120

L40320:        if keyhit% <> 15 then L40335
                  call "PRNTSCRN" : goto L40120

L40335:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40430     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40410
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40410:     if fieldnr% > 1% then L40420
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40420:     return

L40430: if fieldnr% > 0% then L40475  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40475:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100,         /* STARTING PART NUM      */~
                              L50200,         /* ENDING   PART NUM      */~
                              L50300,         /* STARTING DATE          */~
                              L50400,         /* ENDING   DATE          */~
                              L50500,         /* STARTING VENDOR        */~
                              L50600,         /* ENDING   VENDOR        */~
                              L50700,         /* (P)ART or (V)END       */~
                              L50800          /* PRINTCODE              */
            return
L50100: REM Test for STARTING PART NUMBER         STARTPART$
            if startpart$ = "ALL" or startpart$ = "FIRST" then return
            call "GETCODE" (#1, startpart$, " ", 0%, 0, f1%(1))
            if f1%(1) = 1% then return
            errormsg$ = "NO SUCH PART NUMBER IN THE FILES!"
            return

L50200: REM Test for ENDING   PART NUMBER         ENDPART$
            if startpart$ = "ALL" then return
            if endpart$ = "LAST" then return
            call "GETCODE" (#1, endpart$, " ", 0%, 0, f1%(1))
            if f1%(1) = 1% then L50242
            errormsg$ = "NO SUCH PART NUMBER IN THE FILES!" : return
L50242:     if startpart$ = "FIRST" then return
            if endpart$ >= startpart$ then return
            errormsg$ = "ENDING PART MUST BE > = STARTING PART!"
            return

L50300: REM Test for STARTING DATE                STARTDATE$
            call "DATEOKC" (startdate$, startdate%, errormsg$)
            return

L50400: REM Test for ENDING   DATE                ENDDATE$
            call "DATEOKC" (enddate$, enddate%, errormsg$)
            if errormsg$ <> " " then return
            if enddate% >= startdate% then return
            errormsg$ = "ENDING DATE MUST BE AFTER STARTING DATE!"
            return

L50500: REM Test for STARTING VENDOR CODE         STARTVEND$
            if startvend$ = "ALL" or startvend$ = "FIRST" then return
            call "GETCODE" (#2, startvend$, " ", 0%, 0, f1%(2))
            if f1%(2) = 1% then return
            errormsg$ = "NO SUCH VENDOR IN THE FILES!"
            return

L50600: REM Test for ENDING   VENDOR CODE         ENDVEND$
            if startvend$ = "ALL" then return
            if endvend$ = "LAST" then return
            call "GETCODE" (#2, endvend$, " ", 0%, 0, f1%(2))
            if f1%(2) = 1% then L50642
            errormsg$ = "NO SUCH VENDOR IN THE FILES!"
L50642:     if startvend$ = "FIRST" then return
            if endvend$ >= startvend$ then return
            errormsg$ = "ENDING VENDOR MUST BE > = STARTING VENDOR!"
            return

L50700: REM Test for SORT BY (P)ART or (V)ENDOR   SORTCODE$
            if sortcode$ = "P" then return
            if sortcode$ = "V" then return
            errormsg$ = "ENTER 'P' to SORT by PART; 'V' to SORT by VENDOR"
            return

L50800: REM Test for PRINT by 0, 1, 2, or 3       PRINTCODE$
            if printcode$ = "0" then return
            if printcode$ = "1" then return
            if printcode$ = "2" then return
            if printcode$ = "3" then return
            errormsg$ = "ENTER   0,  1,  2,  or  3"
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
            * COPYRIGHT (C) 1987  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
