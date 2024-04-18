        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC    SSSS   OOO    AAA   V   V   GGG    *~
            *  A   A  P   P  C   C  S      O   O  A   A  V   V  G   G   *~
            *  AAAAA  PPPP   C       SSS   O   O  AAAAA  V   V  G       *~
            *  A   A  P      C   C      S  O   O  A   A   VVV   G  GG   *~
            *  A   A  P       CCC   SSSS    OOO   A   A    V     GGG    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * APCSOAVG - Monthly Sales Average price by Product and     *~
            *            Color.                                         *~
            *                                                           *~
            * Record Layout for - (APCSOAVG)                            *~
            *   1. CH(3)       -  Product (MODEL$)    (Primary Key)     *~
            *   2. CH(1)       -  Product (COLOR$)    (Primary Key)     *~
            *   3. 12*PD(14,4) -  Total QTY Product by Month (APC_QTY())*~
            *   4. 12*PD(14,4) -  Total Price Product by Mon (APC_PC() )*~
            *   5. 12*PD(14,4) -  Total QTY Zero Product     (APC_QTZ())*~
            *   6. CH(6)       -  Date Last Order Entered ( DTE$ )      *~
            *   7. CH(3)       -  Filler - Total = 300                  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF ALUMINUM PRODUCTS CORP., INC., RURAL HALL, *~
            * N.C. AND IS CONFIDENTIAL INFORMATION. UNATHORIZED USE,    *~
            * COPYING, DECOMPILING, TRANSLATING, DISCLOSURE, OR TRANSFER*~
            * OF IT IS PROHIBITED. COPYRIGHT (C) 1991, AN UNPUBLISHED   *~
            * WORK BY ALUMINUM PRODUCTS CORP., INC., RURAL HALL, N.C.   *~
            * ALL RIGHTS RESERVED.                                      *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/22/92 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 11/11/97 ! Mod for Upgrade to new Release R6.04.03  ! RHH *~
            * 04/02/98 ! Y2K modifications                        ! ERN *~
            *************************************************************

        dim                                                              ~
            calendar$9,                  /* SELECTED MONTH             */~
            calendar$(12)9,              /* Calendar Months            */~
            brk_tab$(30)4,               /* Report Break Table         */~
            yr$4, yr_bi$2,               /* Calendar Year              */~
            apc_qty(12), apc_qtz(12),    /* Total Qty's, TOT an Zero To*/~
            apc_pc(12),                  /* Total Selling Price by Mon */~
            apc_key$4,                   /* Primary KEY                */~
            apc_dte$8,                   /* Date of Last Order Entered */~
            stat$1,                      /* (Y)es or (N)o Clear Prev DT*/~
            period$2,                    /* REPORTING MONTH            */~
            month$2,                     /*                            */~
            model$3,                     /* PART MODEL CODE            */~
            mod_desc$30,                 /* MODEL DESC                 */~
            color$1,                     /* COLOR CODE                 */~
            col_desc$6,                  /* COLOR DESC                 */~
            units$10,                    /* TOTAL UNITS                */~
            z_units$10,                  /* TOTAL UNITS ZERO PRICE     */~
            price$10,                    /* TOTAL PRICE                */~
            average$10,                  /* AVERAGE PRICE              */~
            avg$10,                      /* AVERAGE PRICE - NO CHG UNIT*/~
            avg1$10,                     /* AVG - INCLUDES ALL PRODUCT */~
            avg2$10,                     /* AVG - EXCLUDES NO CHARGE   */~
            ytd1$10,                     /* AVG YTD - ALL PRODUCT      */~
            ytd2$10,                     /* AVG YTD - EXCLUDES NO CHARG*/~
            descript$32,                 /* DESCRIPTION                */~
            readkey$24,                  /* Gencodes Key               */~
            company$40,                  /* For Report Company Name    */~
            print_title$40,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
            cursor%(25),                 /* Cursor location for edit   */~
            date$10,                     /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(10),                     /* = 0 if the file is open    */~
            f1%(10),                     /* = 1 if READ was successful */~
            fs%(10),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(New) Monthly Sales Average Price       "
            pname$ = "APCSOAVG - Rev: R6.04"

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
            * #01 ! APCSOAVG ! SALES AVERAGE COST FILE                  *~
            * #02 ! GENCODES ! SYSTEM MASTER CODE TABLE FILES           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #01,  "APCSOAVG",                                     ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =    1, keylen =   4

            select #02, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01),200%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02),  0%, rslt$(02))

            f1%(1), f1%(2) = 0%

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATFMTC" (date$)
            yr$ = str(date$,7%,4%)
            convert yr$ to yr%, data goto L00900
L00900:     yr_bi$ = bin(yr%, 2)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            calendar$( 1) = "January  "
            calendar$( 2) = "February "
            calendar$( 3) = "March    "
            calendar$( 4) = "April    "
            calendar$( 5) = "May      "
            calendar$( 6) = "June     "
            calendar$( 7) = "July     "
            calendar$( 8) = "August   "
            calendar$( 9) = "September"
            calendar$(10) = "October  "
            calendar$(11) = "November "
            calendar$(12) = "December "

            brk_tab$( 1) = "0100"
            brk_tab$( 2) = "1000"   :   brk_tab$(16) = "XXXX"
            brk_tab$( 3) = "2110"   :   brk_tab$(17) = "XXXX"
            brk_tab$( 4) = "3000"   :   brk_tab$(18) = "XXXX"
            brk_tab$( 5) = "3430"   :   brk_tab$(19) = "XXXX"
            brk_tab$( 6) = "5000"   :   brk_tab$(20) = "XXXX"
            brk_tab$( 7) = "6000"   :   brk_tab$(21) = "XXXX"
            brk_tab$( 8) = "6500"   :   brk_tab$(22) = "XXXX"
            brk_tab$( 9) = "7000"   :   brk_tab$(23) = "XXXX"
            brk_tab$(10) = "7210"   :   brk_tab$(24) = "XXXX"
            brk_tab$(11) = "7950"   :   brk_tab$(25) = "XXXX"
            brk_tab$(12) = "8140"   :   brk_tab$(26) = "XXXX"
            brk_tab$(13) = "8510"   :   brk_tab$(27) = "XXXX"
            brk_tab$(14) = "9060"   :   brk_tab$(28) = "XXXX"
            brk_tab$(15) = "9990"   :   brk_tab$(29) = "XXXX"
                                    :   brk_tab$(30) = "XXXX"

            brk_tab% = 2%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   2%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
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
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 2% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            goto L11120

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        print_report
            call "APCSOSUB" (stat$, #1)

            gosub generate_report
        return clear all
        goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20130,         /* Month for Report      */ ~
                              L20170          /* Clear Prev Data (Y/N) */
         return

L20130: REM Reporting Month                        PERIOD$
        REM PERIOD$ = "1"
         return

L20170: REM Clear Data Yes or No                   STAT$
        REM STAT$ = "N"
         return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter a Valid Reporting Month. ( 01 thru 12 )                ",~
         "Clear Current Data (Y)es or (N)o. Default = 'N'              "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, apc_key$, color$, model$,  ~
                      month$, col_desc$, units$, price$, average$,       ~
                      rpt_time$, print_title$, readkey$, company$,       ~
                      errormsg$, period$, stat$, apc_dte$, calendar$,    ~
                      mod_desc$, descript$, avg$
        return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF ALUMINUM PRODUCTS CORP., INC., RURAL HALL, *~
            * N.C. AND IS CONFIDENTIAL INFORMATION. UNATHORIZED USE,    *~
            * COPYING, DECOMPILING, TRANSLATING, DISCLOSURE, OR TRANSFER*~
            * OF IT IS PROHIBITED. COPYRIGHT (C) 1991, AN UNPUBLISHED   *~
            * WORK BY ALUMINUM PRODUCTS CORP., INC., RURAL HALL, N.C.   *~
            * ALL RIGHTS RESERVED.                                      *~
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
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            get #1, using L30080, model$, color$, apc_qty(), apc_pc(),    ~
                                                 apc_qtz(), apc_dte$
L30080:       FMT CH(3), CH(1), 12*PD(14,4), 12*PD(14,4), 12*PD(14,4),   ~
                                                          CH(6)
                                         /* Total Quantity for Month   */
            convert apc_qty(month%) to units$,   pic(#######.##)
                                         /* Total Price for Month      */
            convert apc_pc(month%) to price$,    pic(#######.##)
                                         /* Total Qty for Zero Prods   */
            convert apc_qtz(month%) to z_units$, pic(#######.##)

            brk_tab$ = model$ & color$
            if apc_qty(month%) > 0 then goto L30230
               average = 0.0
               goto L30240
                                         /* Average Price for all Prod */
                                         /* Sold (Including No Charge) */
L30230:     average = round(apc_pc(month%) / apc_qty(month%), 2)
L30240:     convert average to average$, pic(#######.##)

            avg = round(apc_qty(month%) - apc_qtz(month%), 2)
            if avg > 0 then goto L30320
               avg = 0.0                 /* Zero When Negative         */
               goto L30330
                                         /* Average Price Deducting    */
                                         /* No Charge Products         */
L30320:     avg = round(apc_pc(month%)/ avg, 2)
L30330:     convert avg to avg$, pic(#######.##)

            col_desc$ = "N/A"
            readkey$  = "COLOR    " & color$
            call "DESCRIBE" (#2, readkey$, descript$, 0%, f1%(2%))
            col_desc$ = str(descript$,6%,6%)
            if f1%(2%) = 0% then col_desc$ = " N/A  "

            readkey$  = "MODEL    " & model$
            call "DESCRIBE" (#2, readkey$, descript$, 0%, f1%(2%))
            mod_desc$ = str(descript$,1%,30%)

            call "DATEFMT" (apc_dte$)
            ytd_qty, ytd_pc, ytd_qtz = 0.0
            avg1, avg2 = 0.0
            for i% = 1% to 12%
                ytd_qty = round(ytd_qty + apc_qty(i%), 2)
                ytd_pc  = round(ytd_pc  + apc_pc(i%), 2)
                ytd_qtz = round(ytd_qtz + apc_qtz(i%), 2)
            next i%
                                         /* (AVG1) = YTD AVERAGE CHARGE*/
                                         /*          ALL PRODUCTS      */
            avg1 = round(ytd_pc/ytd_qty, 2)
            convert avg1 to avg1$, pic(#######.##)

                                         /* (AVG2) = YTD AVERAGE CHARGE*/
                                         /*          EXCLUDE NO CHARGE */
            avg2 = round(ytd_qty - ytd_qtz, 2)
            if avg2 > 0 then goto L30640
               avg2 = 0.0
               goto L30650
L30640:     avg2 = round(ytd_pc/avg2, 2)
L30650:     convert avg2 to avg2$, pic(#######.##)

        return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT

        REM RETURN CLEAR ALL
        REM GOTO INPUTMODE


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

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
              on fieldnr% gosub L40170,         /* Report Period       */ ~
                                L40160          /* Clear Data          */
              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40170:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(10),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Price Analysis Report Month  :",             ~
               at (06,35), fac(lfac$( 1)), period$              , ch(02),~
               at (06,40), fac(hex(84)),   calendar$            , ch(09),~
                                                                         ~
               at (07,02), "Clear All Previous Data (Y/N):",             ~
               at (07,35), fac(lfac$( 2)), stat$                , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40440
                  call "PRNTSCRN"
                  goto L40190

L40440:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40630     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40590
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40590:     if fieldnr% > 1% then L40610
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40610:     return

L40630: if fieldnr% > 0% then L40720  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L40720:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50120,         /* Reporting Period      */ ~
                              L50270          /* Clear Previous Data   */
            return

L50120: REM Price Analysis Period               PERIOD$
            month% = 1% : calendar$ = " "
            if period$ <> " " then goto L50160
               goto L50230
L50160:     convert period$ to period%, data goto L50230

            convert period% to period$, pic(00)

            if period% < 1% or period% > 12% then goto L50230
            month% = period%
            calendar$ = calendar$(period%)
            goto L50250
L50230:     errormsg$ = "Must Enter a Valid Pricing Month."
            period$, calendar$ = " "
L50250:  return

L50270: REM Clear Previous Data                 STAT$
            if stat$ = " " then stat$ = "N"
            if stat$ <> "Y" and stat$ <> "N" then goto L50310
        return
L50310:     errormsg$ = "Must Enter Either 'Y' or 'N'."
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L55040: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~-+
                                                   /* COLUMN 1 HEADER */
L55070: %!MONTH: ######### ####            ##############################~
        ~##########                                          PAGE : ###   ~
        ~ !

L55110: %!---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~-!

L55150: %!Model! Color!<-------- Description ------->!Last SO.!*Tot Units~
        ~!No Chg Unt!Tot  Price!*Avg Price!Avg  Price!*YTD  Avg ! YTD  Avg~
        ~ !

L55190: %!-----!------!------------------------------!--------!----------~
        ~!----------!----------!----------!----------!----------!---------~
        ~-!

L55230: %! ### !######!##############################!########!##########~
        ~!##########!##########!##########!##########!##########!#########~
        ~#!

L55270: %! *** !******! BREAK TOTAL FOR (###)        !********!##########~
        ~!##########!##########!##########!##########!##########!#########~
        ~#!

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        select_printer
            page_no% = 0%
            lcnt%    = 99%
            print_title$ = "Monthly Cost Analysis Report"
            call "FMTTITLE" (print_title$, " ", 12%)
            date$ = date  :  call "DATFMTC" (date$)
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "SETPRNT" ("APCAVG", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCAVG", " ", 0%, 1%)
        return

        generate_report
            call "SHOSTAT" ("Creating Cost Analysis Report")
          tot_unt1, tot_unt2, tot_unt3, tot_pc, tot_pc1, tot_pc2 = 0.0
          tot_ytd_qty, tot_ytd_qtz, tot_ytd_pc, ytd1, ytd2 = 0.0
          avg1, avg2, ytd_qty, ytd_qtz, ytd_pc = 0.0
            gosub select_printer
            apc_key$ = all(hex(00))
            read #1,key > apc_key$, eod goto generate_done
            goto L60310
        generate_next
            read #1, eod goto generate_done
L60310:       gosub dataload
              gosub print_detail
            goto generate_next
        generate_done
            str(brk_tab$(brk_tab%-1%),1%,3%) = brk_tab$
            gosub print_break
            print using L55040
            gosub close_printer
        return

        print_header
          page_no% = page_no% + 1%
          print page
          print using L55040
          print using L55070,calendar$(period%),yr$,print_title$,page_no%
          print using L55110
          print using L55150
          lcnt% = 4%
        return

        print_detail
          if lcnt% < 60% then goto L60550
             if lcnt% <> 99% then print using L55040
             gosub print_header
L60550:   if brk_tab$ < brk_tab$(brk_tab%) then goto L60580
             gosub print_break
             brk_tab% = brk_tab% + 1%
L60580:
            tot_unt1 = round(tot_unt1 + apc_qty(month%), 2)
            tot_unt2 = round(tot_unt2 + apc_qtz(month%), 2)
            tot_pc   = round(tot_pc   + apc_pc(month%),  2)

            tot_ytd_qty = round(tot_ytd_qty + ytd_qty, 2)
            tot_ytd_qtz = round(tot_ytd_qtz + ytd_qtz, 2)
            tot_ytd_pc  = round(tot_ytd_pc  + ytd_pc , 2)

          print using L55190
          print using L55230, model$, col_desc$, mod_desc$, apc_dte$,     ~
                             units$, z_units$, price$, average$, avg$,   ~
                             avg1$, avg2$

          lcnt% = lcnt% + 2%
        return

        print_break
            convert tot_unt1 to tot_unt1$, pic(#######.##)

            convert tot_unt2 to tot_unt2$, pic(#######.##)

            convert tot_pc   to tot_pc$,  pic(#######.##)

            if tot_unt1 > 0 then goto L60850
               tot_pc1 = 0
               goto L60860
L60850:     tot_pc1 = round(tot_pc / tot_unt1, 2)
L60860:     convert tot_pc1 to tot_pc1$, pic(#######.##)

            tot_unt3 = round(tot_unt1 - tot_unt2, 2)
            if tot_unt3 > 0 then goto L60920
               tot_pc2 = 0
               goto L60930
L60920:     tot_pc2 = round(tot_pc / tot_unt3, 2)
L60930:     convert tot_pc2 to tot_pc2$, pic(#######.##)
            ytd1 = round(tot_ytd_pc/tot_ytd_qty, 2)
            convert ytd1 to ytd1$, pic(#######.##)

            ytd2 = round(tot_ytd_qty - tot_ytd_qtz, 2)
            if ytd2 > 0 then goto L61010
               ytd2 = 0.0
               goto L61020
L61010:     ytd2 = round(tot_ytd_pc/ ytd2, 2)
L61020:     convert ytd2 to ytd2$, pic(#######.##)

          print using L55190
          print using L55270, str(brk_tab$(brk_tab%- 1%),1%,3%),tot_unt1$,~
                              tot_unt2$, tot_pc$, tot_pc1$, tot_pc2$,    ~
                              ytd1$, ytd2$

          lcnt% = lcnt% + 2%
          tot_unt1, tot_unt2, tot_unt3, tot_pc, tot_pc1, tot_pc2 = 0.0
          tot_ytd_qty, tot_ytd_qtz, tot_ytd_pc = 0.0
          ytd1, ytd2 = 0.0
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF ALUMINUM PRODUCTS CORP., INC., RURAL HALL, *~
            * N.C. AND IS CONFIDENTIAL INFORMATION. UNATHORIZED USE,    *~
            * COPYING, DECOMPILING, TRANSLATING, DISCLOSURE, OR TRANSFER*~
            * OF IT IS PROHIBITED. COPYRIGHT (C) 1991, AN UNPUBLISHED   *~
            * WORK BY ALUMINUM PRODUCTS CORP., INC., RURAL HALL, N.C.   *~
            * ALL RIGHTS RESERVED.                                      *~
            *-----------------------------------------------------------*~

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
