        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDPLN54                             *~
            *  Creation Date     - 07/14/98                             *~
            *  Last Modified Date- 07/20/98                             *~
            *  Description       - This Program Scans the 'ARIMASTR'    *~
            *                      and 'ARILINES' File and Totals       *~
            *                      Product and Product Dollars for      *~
            *                      Specified Size                       *~
            *                                                           *~
            *  Special Notes     - (APCQUICK), (EWDPLN54), (EWDPLN55)   *~
            *                      are Basically all the Same Program   *~  
            *                                                           *~
            *  Special Comments  - VALS() - Buckets for Each Model      *~
            *                      (1 thru 10) - Color Quantities       *~
            *                      (11)       - Total No. of Sales Order*~
            *                      (12)       - Total for all Colors    *~
            *                      (13)       - Total Price             *~
            *                      (14)       - total Width Inches      *~
            *                      (15)       - Total Height Insches    *~   
            *                                                           *~
            *  Special Table     - (PLANQUICK) Parameter Table          *~
            *                      (01 thru 09) -Beginning Date (YYMMDD)*~
            *                      (10 thru 19) -Ending Date    (YYMMDD)*~
            *                      (20 thru 24) -Model Code     (MMM)   *~
            *                      (25 thru 27) -Test Quantity  (QQQ)   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/25/95 ! New Program for (APC) - INITIALIZE WITH  ! RHH *~
            *          !   LAST INVOICE NO FOR '95' DATA.         !     *~
            * 06/28/95 ! Mod add Average Width and Height for Size! RHH *~
            * 07/15/97 ! Mod to add new table (PLANQUICK) to      ! RHH *~
            *          !   create Multiple reports with (1) single!     *~
            *          !   program Run.                           !     *~
            *          !                                          !     *~
            * 11/06/97 ! Revision Update For 60403                ! DJD *~
            *          !                                          !     *~
            * 11/11/97 ! Added fourth key to ARIMASTR (60403)     ! DJD *~
            * 07/20/98 ! Mod to Print Detail for 10 Colors        ! RHH *~
            *          !                                          !     *~
            * 04/18/02 ! Mod to change company name (EWD0001)     ! TLM *~
            *************************************************************

        dim                                                              ~
            qty_wa$10, qty_ha$10,        /* Average Width an Height Siz*/~
            readkey$24, desc$30,         /* GENCODES Key               */~
            inv_key$8, rhh$30,           /* ARIMASTR Primary Key       */~
            postdate$6,                  /* Invoice Date, Posting Date */~
            inv_typ$1,                   /* Invoice Type Code          */~
            line_key$20, sav_key$17,     /* ARILINES Key               */~
            beg_dte$6, beg_date$10, x$10,/* Beginning Date             */~
            end_dte$6, end_date$10,      /* Ending Date                */~
            prod_cd$3, prod_cd_desc$30,  /* Model and Description      */~
            mod$3, prod_qty$4,           /* REPORT MODEL/DESCRIPTION   */~
            cc$1, mode$5,                /* Color Code / File Opn Mode */~
            prod_wt$4, wd$7,             /* Model Width Size - 8's     */~
            prod_ht$3, ht$6,             /* Model Height Size - 8's    */~
            cus_code$9, part$25,         /* CUSTOMER/PART NO           */~
            prod_fac_w$8,                /* Factor for Scan + Width    */~
            prod_fac_h$8,                /* Factor for Scan + Height   */~
            sze$30,                      /* EIGHTH FRACTIONS           */~
            t$(10%)50, qtys(5%,3%),      /* Display Data on Screen     */~
            vals(16%), t_vals(16%),      /* REPORT VALUES / Totals     */~
            wrk_key$28,                  /* Primary Work Key           */~
            col$(15%)6,                  /* Print Color Quantities     */~
            t_col$12, t_prc$14,          /* Print Totals               */~
            date$10,                     /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            cursor%(2%),                 /* Cursor location for edit   */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,print_title$46,    /* PF Key Hex Values          */~
            company$40, line2$79,        /* Comapny Name and Title     */~
            temp1$10, temp2$10,          /* Temp storage               */~
            userid$3                     /* Current User Id            */~

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        dim d1$(125%)6,                  /* Beginning Date             */~
            d2$(125%)6,                  /* Ending Date                */~
            d3$(125%)3,                  /* Model Code                 */~
            d4$(125%)3,                  /* Product Quantity           */~
            yy$4                         /* Invoice Year               */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "06.04.03 07/20/98 Sales Quick Count Report (Color)"
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
            * #1  ! ARIMASTR ! Invoice Master File                      *~
            * #2  ! ARILINES ! Invoice Line Items File                  *~
            * #3  ! GENCODES ! GENERAL SYSTEM CODES FILE                *~
            * #4  ! CUSTOMER ! CUSTOMER MASTER FILE                     *~
            * #5  ! EWDPLN54 ! QUICK WORK FILE                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  17,                     ~
                        alt key  1, keypos =   10, keylen =   8, dup,    ~
                            key  2, keypos =   18, keylen =  16, dup,    ~
                            key  3, keypos =   34, keylen =  16, dup,    ~
                            key  4, keypos = 1783, keylen =  26, dup

            select #2,  "ARILINES",                                      ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =    1, keylen =  20

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =   24

            select #4,   "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #5,  "EWDPLN55",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos =    1, keylen =   28

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%), 0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%), 0%, rslt$(3%))
        REM CALL "OPENCHCK" (#4, FS%(4%), F2%(4%), 0%, RSLT$(4%))
            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATFMTC" (date$)
            call "COMPNAME" (12%, company$, ret%)                /* (EWD0001) */
REM            company$ = "   Ellison Window and Doors - Welcome   "
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62%) = "EWDPLN55: " & str(cms2v$,,8)

            sze$ = "1/81/43/81/25/83/47/8         "
            gosub load_table

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  7%
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
            * Handles EDIT MODE for range selection screen.             *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then gosub process_data
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% >  7% then editpg1
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
            *           E X T R A C T   R E P O R T   D A T A           *~
            *-----------------------------------------------------------*~
            * Data Extraction section for report.                       *~
            *************************************************************

        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
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
         "Enter the Beginning Posting Date?                            ",~
         "Enter the Ending Posting Date?                               ",~
         "Enter a Valid Model,'000' = All?,Print Qty's Greater, 0 = All",~
         "Enter a Valid Product Width (4), Inches=(3),Eighth's=(1) ?   ",~
         "Enter a Valid Product Height (3), Inches=(2),Eights=(1) ?    ",~
         "Enter a Valid + Factor in Decimal Inches for Width or 0.0?   ",~
         "Enter a Valid + Factor in Decimal Inches for Height or 0.0?  "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, beg_dte$, beg_date$,       ~
                      end_dte$, end_date$, prod_cd$, prod_cd_desc$,      ~
                      prod_wt$, prod_ht$, wd$, ht$, readkey$,            ~
                      line_key$, prod_fac_w$, prod_fac_h$, prod_qty$
            rpt% = 0%
        return

        REM *************************************************************~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(fieldnr%)
L40080:       gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40210,                /* Beginning Date*/~
                                L40210,                /* Ending Date   */~
                                L40210,                /* Product Code  */~
                                L40210,                /* Product Width */~
                                L40210,                /* Product Height*/~
                                L40210,                /* Factor Width  */~
                                L40210                 /* Factor Height */
              goto L40240

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40210:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40240:     accept                                                       ~
               at (01,02),                                               ~
                  "APC Quick Product Count Utility Screen",              ~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (05,02), "Begining Posting Date:",                     ~
               at (05,25), fac(lfac$(1%)), beg_date$            , ch(10),~
                                                                         ~
               at (06,02), "Ending Posting Date  :",                     ~
               at (06,25), fac(lfac$(2%)), end_date$            , ch(10),~
                                                                         ~
               at (07,02), "Product Code/Rpt Qty :",                     ~
               at (07,25), fac(lfac$(3%)), prod_cd$             , ch(03),~
               at (07,30), fac(lfac$(3%)), prod_qty$            , ch(04),~
               at (07,40), fac(hex(84)), prod_cd_desc$          , ch(30),~
                                                                         ~
               at (08,02), "Product Width (4)    :",                     ~
               at (08,25), fac(lfac$(4%)), prod_wt$             , ch(04),~
               at (08,40), fac(hex(84)), wd$                    , ch(08),~
                                                                         ~
               at (09,02), "Product Height (3)   :",                     ~
               at (09,25), fac(lfac$(5%)), prod_ht$             , ch(03),~
               at (09,41), fac(hex(84)), ht$                    , ch(07),~
                                                                         ~
               at (10,02), "Factor + Dec. Width  :",                     ~
               at (10,25), fac(lfac$(6%)), prod_fac_w$          , ch(08),~
                                                                         ~
               at (11,02), "Factor + Dec. Height :",                     ~
               at (11,25), fac(lfac$(7%)), prod_fac_h$          , ch(08),~
                                                                         ~
               at (12,12), fac(hex(84)), t$( 1%)                , ch(50),~
               at (13,12), fac(hex(84)), t$( 2%)                , ch(50),~
               at (14,12), fac(hex(84)), t$( 3%)                , ch(50),~
               at (15,12), fac(hex(84)), t$( 4%)                , ch(50),~
               at (16,12), fac(hex(84)), t$( 5%)                , ch(50),~
               at (17,12), fac(hex(84)), t$( 6%)                , ch(50),~
               at (18,12), fac(hex(84)), t$( 7%)                , ch(50),~
               at (19,12), fac(hex(84)), t$( 8%)                , ch(50),~
               at (20,12), fac(hex(84)), t$( 9%)                , ch(50),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 14% then goto L40780
                  rpt% = 1%
                  if fieldnr% = 1% then inc% = 1%
                  gosub process_data

L40780:        if keyhit% <> 15 then L40810
                  call "PRNTSCRN" : goto L40080

L40810:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41020     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (14)Print Report"
            pf$(2%) = "                 (4)Previous Field      " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L40960
                str(pf$(3%),64%)  = " " : str(pfkeys$,16%,1%) = hex(ff)
L40960:     if fieldnr% > 1% then L40980
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40980:     if fieldnr% = 1% or fieldnr% = 4% then goto L41000
                str(pf$(1%),64%) = " " : str(pfkeys$,14%,1%) = hex(ff)
L41000: return

L41020: if fieldnr% > 0% then L41110  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Process Data"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return
L41110:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0fff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50170,                 /* Beg Post Date*/  ~
                              L50260,                 /* End Post Date*/  ~
                              L50380,                 /* Product Code */  ~
                              L50580,                 /* Product Width*/  ~
                              L50730,                 /* Product Height*/ ~
                              L50880,                 /* Factor + Width*/ ~
                              L51000                  /* Factor + Heigh*/
            return

L50170: REM Beginning Posting Date                BEG_DTE$, BEG_DATE$
            date% = 0%
            call "DATEOKC" (beg_date$, date%, errormsg$)
            if date% = 0% then return
            x$ = beg_date$
            call "DATUFMTC"(x$)
            beg_dte$ = str(x$,1%,6%)
        return

L50260: REM Ending Posting Date                   END_DTE$, END_DATE$
            if end_date$ <> " " then goto L50300
               end_date$ = date

L50300:     date% = 0%
            call "DATEOKC" (end_date$, date%, errormsg$)
            if date% = 0% then return
            x$ = end_date$
            call "DATUFMTC"(x$)
            end_dte$ = str(x$,1%,6%)
        return

L50380: REM Product Code/Model                    PROD_CD$
            if prod_qty$ <> " " then goto L50410
L50400:        prod_qty$ = "0"
L50410:     convert prod_qty$ to prod_qty%, data goto L50400

            convert prod_qty% to prod_qty$, pic(####)
            if prod_cd$ <> " " then goto L50490
L50450:        prod_cd$ = "000"
               prod_cd_desc$ = "(000) = All Model Codes"
               return

L50490:     if prod_cd$ = "000" then goto L50450
            gosub lookup_model
            if prod_cd_desc$ <> " " then return
               goto L50540
        return
L50540:     errormsg$ = "(Error) - Invalid Product Model Code?"
            init(" ")prod_cd$,prod_cd_desc$, prod_qty$
        return

L50580: REM Product Width (4)                     PROD_WT$, WD$
            wd$ = " " : s_width = 0.0
            if prod_wt$ <> " " then goto L50630
               goto L50690

L50630:     convert prod_wt$ to prod_wt%, data goto L50690

            convert prod_wt% to prod_wt$,pic(0000)
            if prod_wt% = 0% then goto L50690
            gosub std_width
        return
L50690:     errormsg$ = "(Error) - Invalid Product Width?"
            init(" ") prod_wt$, wd$
        return

L50730: REM Product Height (3)                    PROD_HT$, HT$
            ht$ = " " : s_height = 0.0
            if prod_ht$ <> " " then goto L50780
               goto L50840

L50780:     convert prod_ht$ to prod_ht%, data goto L50840

            convert prod_ht% to prod_ht$,pic(000)
            if prod_ht% = 0% then goto L50840
            gosub std_height
        return
L50840:     errormsg$ = "(Error) - Invalid Product Height?"
            init(" ") prod_ht$, ht$
        return

L50880: REM Product Factor (8)                    PROD_FAC_W$
            init(" ") t$()
            prod_fac_w = 0.0
            if prod_fac_w$ <> " " then goto L50940
L50920:        prod_fac_w$ = "000.0000"

L50940:     convert prod_fac_w$ to prod_fac_w, data goto L50920

            convert prod_fac_w to prod_fac_w$,pic(###.####)

        return

L51000: REM Product Factor (8)                    PROD_FAC_H$
            init(" ") t$()
            prod_fac_h = 0.0
            if prod_fac_h$ <> " " then goto L51060
L51040:        prod_fac_h$ = "000.0000"

L51060:     convert prod_fac_h$ to prod_fac_h, data goto L51040

             convert prod_fac_h to prod_fac_h$,pic(###.####)

        return

        std_width       /* Convert Standard Width to Fraction in 8'ths */
                        /* WD$ = Width & Fraction, F0 = Width Fraction */
           str(wd$,1%,3%) = str(prod_wt$,1%,3%)          /* Width  (3) */
           if str(wd$,1%,1%) = "0" then str(wd$,1%,1%) = " "
           f0% = 0%                                 /* Build Fractions */
           convert str(prod_wt$,4%,1%) to f0%,data goto L51200 /*Width */

           goto L51210
L51200:      f0% = 8%
L51210:    if f0% = 0% then f0% = 9%
           str(wd$,4%,1%) = " "          /* Build Width with Fraction  */
           str(wd$,5%,3%) = str(sze$,(f0%*3%) - 2%, 3%)
           if rpt% = 1% then return
              a1 = 0.0 : a2 = 0.0
              convert str(prod_wt$,1%,3%) to a1, data goto L51270
L51270:
              convert str(prod_wt$,4%,1%) to a2, data goto L51290
L51290:
              s_width = a1 + (a2/8.0)
        return

        std_height   /* Convert Standard Heighth to Fraction in 8'ths  */
                     /* HT$ = Height & Fraction, F1% = Height Fraction */
           str(ht$,1%,2%) = str(prod_ht$,1%,2%)          /* Height (2) */
           f1% = 0%                                 /* Build Fractions */
           convert str(prod_ht$,3%,1%) to f1%,data goto L51400 /*Height*/

           goto L51410
L51400:      f1% = 8%
L51410:    if f1% = 0% then f1% = 9%
           str(ht$,3%,1%) = " "          /* Build Height with Fraction */
           str(ht$,4%,3%) = str(sze$,(f1%*3%) - 2%, 3%)
           if rpt% = 1% then return
              a1 = 0.0 : a2 = 0.0
              convert str(prod_ht$,1%,2%) to a1, data goto L51470
L51470:
              convert str(prod_ht$,3%,1%) to a2, data goto L51490
L51490:
              s_height = a1 + (a2/8.0)
        return

        REM *************************************************************~
            *          F o r m a t   S t a t e m e n t s                *~
            *-----------------------------------------------------------*~
            * Image Statements                                          *~
            *************************************************************

L55060: %+---------------------------------------------------------------~
        ~---------------------------------------------------------------+

L55090: %!---------------------------------------------------------------~
        ~---------------------------------------------------------------!

L55120: %! Date: ########## @ ########        ###########################~
        ~#############                                       Page: #### !

L55150: %! From: ########## to ##########  ##############################~
        ~################                                      ######## !

L55180: %! Model: ### ####################                               ~
        ~                                     Print Qty's Greater: #### !

L55210: %!Model! Width !Height!White !Bronz !Cocoa !Beige !NatOak!HilOak!~
        ~Brs/No!Brs/Ho!Wh/Brs!Bg/Brs!!Orders! Total Color!  Total Price !

L55240: %!-----!-------!------!------!------!------!------!------!------!~
        ~------!------!------!------!!------!------------!--------------!

L55270: %! ### !#######!######!######!######!######!######!######!######!~
        ~######!######!######!######!!######!############!##############!

L55300: %! Report Average Width : ##########  Average Height : ##########~
        ~                                                               !

        REM *************************************************************~
            *          S p e c i a l   S u b r o u t i n e s            *~
            *-----------------------------------------------------------*~
            * Subroutines                                               *~
            *************************************************************

        print_header                         /* GENERIC REPORT HEADING */
          if lcnt% <> 99% then print using L55060
          page_no% = page_no% + 1%
          print page
          print using L55060
          print using L55120, date$,rpt_time$,company$,page_no%
          print using L55150, beg_date$,end_date$,print_title$,"EWDPLN54"
          print using L55180, prod_cd$, str(prod_cd_desc$,1%,20%),        ~
                             prod_qty$
          print using L55090
          print using L55210
          lcnt% = 6%
        return

        print_detail
          if lcnt% > 57% then gosub print_header

          print using L55240
          print using L55270, mod$, wd$, ht$, col$(1%),col$(2%), col$(3%),~
                             col$(4%), col$(5%), col$(6%), col$(7%),      ~
                             col$(8%), col$(9%), col$(10%), col$(11%),    ~
                             t_col$, t_prc$

          lcnt% = lcnt% + 2%
        return

        print_totals
            for k% = 1% to 11%
                convert t_vals(k%) to col$(k%), pic(######)

            next k%


            if qty_t < 1 then qty_t = 1.0
            qty_wa = round( qty_w / qty_t, 4)
            qty_ha = round( qty_h / qty_t, 4)
            convert qty_wa to qty_wa$, pic(####.####-)

            convert qty_ha to qty_ha$, pic(####.####-)

            convert t_vals(12%) to t_col$, pic(####,###,###)

            convert t_vals(13%) to t_prc$, pic(##,###,###.##-)

            if lcnt% > 55% then gosub print_header

            print using L55240
          print using L55270,"***"," Total "," **** ",  col$(1%),col$(2%),~
                            col$(3%),col$(4%), col$(5%), col$(6%), col$(7%),~
                            col$(8%), col$(9%), col$(10%), col$(11%),     ~
                            t_col$, t_prc$
          print using L55090
          print using L55300, qty_wa$, qty_ha$
          lcnt% = 60%
          mat t_vals = zer
        return

        lookup_model
            init(" ") readkey$, prod_cd_desc$
            str(readkey$,1%,9%)   = "MODEL    "
            str(readkey$,10%,15%) = prod_cd$
            read #3,key = readkey$,using L60720, prod_cd_desc$,           ~
                                                eod goto L60730
L60720:        FMT POS(25), CH(30)
L60730: return

        process_data
           if inc% = 0% then goto L60800
              if inc% > t_max% then goto exit_program
                 gosub load_data

L60800:    call "SHOSTAT" ("Scanning (Invoiced) Sales Orders")
           if rpt% = 0% then goto L60850
              mode% = 1% : gosub open_work
              mode% = 3% : gosub open_work

L60850:    cnt% = 0%
           rhh$ = "Invoices Scanned -- [xxxxxxxx]" 
           mat qtys = zer
           mat vals = zer                /* Report Values              */
           qty_w = 0.0 : qty_h = 0.0 : qty_t = 0.0
           init(" ") inv_key$, t$(), yy$
           temp1$ = beg_dte$
           call "DATFMTC" (temp1$, date%, temp2$)
           yy$ = str(temp2$,1%,4%)     /* Initialize to Start Scan   */
           if yy$ = "1995" then inv_key$ = "00184550"
           if yy$ = "1996" then inv_key$ = "00255757"
           if yy$ = "1997" then inv_key$ = "00341000"
           if yy$ = "1998" then inv_key$ = "00439586"

        arimastr_loop
            read #1,key 1% > inv_key$,using L60990, cus_code$, inv_key$,  ~
                             postdate$, invdisc, inv_typ$,               ~
                             eod goto process_done
L60990:        FMT CH(9), CH(8), POS(533), CH(6), POS(801), PD(14,4),    ~
                   POS(891), CH(1)

            cnt% = cnt% + 1%
            if mod(cnt%,500) <> 0 then goto L61070
               convert cnt% to str(rhh$,22%,8%), pic(########)
               print at(03,26);hex(84);rhh$;

L61070:     if postdate$ < beg_dte$ or postdate$ > end_dte$ then         ~
                                                       goto arimastr_loop
            p% = pos("OIAC" = inv_typ$)
            if p% = 0% then goto arimastr_loop
            chk% = 0%
            line_key$ = " "
            str(line_key$,1%,9%)  = cus_code$
            str(line_key$,10%,8%) = inv_key$
            sav_key$ = str(line_key$,1%,17%)
        arilines_loop
            read #2,key > line_key$,using L61190,line_key$, part$, qtyshp,~
                              price, linedisc, eod goto arimastr_loop
L61190:         FMT CH(20), XX(3), CH(25), POS(93), PD(14,4), POS(133),  ~
                    PD(14,4), PD(14,4)
            if str(line_key$,1%,17%) <> sav_key$ then goto arimastr_loop
            if len(part$) < 19 then goto arilines_loop
            if prod_cd$ = "000" then goto L61260          /* All Models */
               if str(part$,1%,3%) <> prod_cd$ then goto arilines_loop

L61260:     gosub calc_size
            if rpt% = 1% then goto L61350      /* Skip Check - Report   */
                                              /* is for all Sizes.     */
               w1 = s_width : w2 = s_width + prod_fac_w
               if c_width < w1 or c_width > w2 then goto arilines_loop

               h1 = s_height : h2 = s_height + prod_fac_h
               if c_height < h1 or c_height > h2 then goto arilines_loop

L61350:     lineext = round(qtyshp * price, 2)      /* Extended Price  */
                                         /* Line Item Discounted Price */
            lineitm = round(lineext * (1.0 - (linedisc/100.0)), 2)
                                    /* Line Item Price After Order Disc*/
            lineitm = round(lineitm * (1.0 - (invdisc/100.0)), 2)

            if rpt% = 0% then gosub screen_calcs                         ~
                         else gosub update_work
            goto arilines_loop

        process_done
            init(" ") t$()
            if rpt% = 1% then gosub gen_rpt
               t$( 1%) = " Invoice Type   No Inv's Qty Ship Prod. Price "
               t$( 2%) = "--------------  -------- -------- ------------"
               t$( 3%) = "Invoice S.O     ######## ######## ############"
               t$( 4%) = "Direct Invoice  ######## ######## ############"
               t$( 5%) = "Adj. Invoice    ######## ######## ############"
               t$( 6%) = "Credit Memo     ######## ######## ############"
               t$( 7%) = "                -------- -------- ------------"
               t$( 8%) = " (Grand Total)  ######## ######## ############"
               t$( 9%) = "Average Width = XXXXXXXXXX Height = XXXXXXXXXX"
               if qty_t < 1 then qty_t = 1.0
               qty_wa = round( qty_w / qty_t, 4)     /* Average Width  */
               qty_ha = round( qty_h / qty_t, 4)     /* Average Height */
               convert qty_wa to str(t$(9%),17%,10%), pic(####.####-)

               convert qty_ha to str(t$(9%),37%,10%), pic(####.####-)

            for i% = 1% to 5%
                k% = 2%
                if i% = 5% then k% = 3%
            convert qtys(i%,1%) to str(t$(k%+i%),17%,8%),pic(####,###)

            convert qtys(i%,2%) to str(t$(k%+i%),26%,8%),pic(###,###-)

          convert qtys(i%,3%) to str(t$(k%+i%),35%,12%),pic(####,###.##-)

            next i%
        return clear all
        goto inputmode

        calc_size
            a1 = 0.0 : a2 = 0.0
            convert str(part$,13%,3%) to a1, data goto L61800
L61800:
            convert str(part$,16%,1%) to a2, data goto L61820
L61820:
            c_width = a1 + (a2/8.0)
            a1 = 0.0 : a2 = 0.0
            convert str(part$,17%,2%) to a1, data goto L61860
L61860:
            convert str(part$,19%,1%) to a2, data goto L61880
L61880:
            c_height = a1 + (a2/8.0)
        return

        calc_average
           qty_w = round(qty_w + (c_width * qtyshp),4) /* TOTAL WIDTH  */
           qty_h = round(qty_h + (c_height * qtyshp),4) /* TOTAL HEIGHT*/
           qty_t = round(qty_t + qtyshp, 4)            /* TOTAL SHIPPED*/
        return

        screen_calcs
            if chk% = 1% then goto L62040
               qtys(p%,1%) = qtys(p%,1%) + 1%   /* Count Invoice Types */
               qtys(5%,1%) = qtys(5%,1%) + 1%   /* Count Totals        */
               chk% = 1%

L62040:     qtys(p%,2%) = round(qtys(p%,2%) + qtyshp, 2)    /* Shipped */
            qtys(p%,3%) = round(qtys(p%,3%) + lineitm, 2)   /* Price   */
                                                       /* Grand Totals */
            qtys(5%,2%) = round(qtys(5%,2%) + qtyshp, 2)    /* Shipped */
            qtys(5%,3%) = round(qtys(5%,3%) + lineitm, 2)   /* Price   */
            if p% = 1% then gosub calc_average
        return

        report_calcs
            mod$ = str(part$,1%,3%)
            cc$  = str(part$,4%,1%)
            prod_wt$ = str(part$,13%,4%)
            prod_ht$ = str(part$,17%,3%)
            gosub std_width                     /* WD$ = Long Form     */
            gosub std_height                    /* HT$ = Long Form     */
        return

        report_values                           /* Report Values for   */
            if chk% = 1% then goto L62270
               vals(11%) = vals(11%) + 1        /* Count S.O.          */
               chk% = 1%  

L62270:     k% = pos("2356ABCDEF" = cc$)            /* Convert Color Code  */
            if k% = 0% then goto L62310
               vals(k%) = round(vals(k%)  + qtyshp, 2) /* Color Shipped*/

L62310:     vals(12%)   = round(vals(12%) + qtyshp, 2) /* Total Color  */
            vals(13%)   = round(vals(13%)   + lineitm, 2)   /* Price   */
        return

        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#5,mode$, 500%, f2%)
            if f2% <> 0% then goto L62430
        return
L62430:     call "SHOSTAT" ("Error - Cannot Open (EWDPLN54)") : stop
        return
        delete_work
            call "FILEBGON" (#5)
        return

        update_work
            if p% <> 1% then return                 /* FOR REPORT ONLY */
               gosub calc_average                   /* INVOICED ORDERS */
               mat vals = zer
               gosub report_calcs
               wrk_key$ = " " : f0% = 0%
               str(wrk_key$,1%,3%)   = mod$
               str(wrk_key$,4%,7%)   = wd$
               str(wrk_key$,11%,6%)  = ht$
               str(wrk_key$,17%,12%) = " "
               read #5,hold,key = wrk_key$,using L62610, vals(),          ~
                                                           eod goto L62630
L62610:           FMT POS(29), 15*PD(14,4)
               f0% = 1%
L62630:        gosub report_values
               if f0% = 0% then goto L62690
                  put #5,using L62610, vals()
                    FMT POS(29), 15*PD(14,4)
                  rewrite #5
        return
L62690:        put #5,using L62700, wrk_key$, vals()
L62700:          FMT CH(28), 15*PD(14,4)
               write #5, eod goto L62730
        return
L62730:     call "SHOSTAT" ("(Error)- Updating WorkFile?") : stop
        return

        gen_rpt
            call "SHOSTAT" ("Creating Analysis Report")
            rhh$ = "Records Printed --- [xxxxxxxx]"

            page_no% = 0%
            lcnt%    = 99%
            date$ = date  :  call "DATFMTC" (date$)
            init(" ") rpt_time$, wrk_key$
            call "TIME" (rpt_time$)
            call "SETPRNT" (" ", "APCQ", 2000%, 0%)
            print_title$="Sales Analysis by Size of Only Invoiced Orders"
            call "FMTTITLE" (print_title$, " ", 12%)
            select printer (134)
            sav_mod$ = " "
            mat t_vals = zer
        gen_next
            read #5,key > wrk_key$, using L62930, wrk_key$,vals(),        ~
                                                 eod goto gen_done
L62930:        FMT CH(28), 15*PD(14,4)
            cnt% = cnt% + 1%
            if mod(cnt%,10%) <> 0 then goto L62940
               convert cnt% to str(rhh$,22%,8%), pic(00000000)
               print at(03,26);hex(84);rhh$;

L62940:     mod$ = str(wrk_key$,1%,3%)                 /* Model Code   */
            wd$  = str(wrk_key$,4%,7%)                 /* Width Long   */
            ht$  = str(wrk_key$,11%,6%)                /* Height Long  */
            if mod$ <> sav_mod$ then goto L63010
               sav_mod$ = mod$
               if lcnt% <> 99% then gosub print_totals

L63010:     for k% = 1% to 15%               /* Accum. Totals for Model */
                t_vals(k%) = t_vals(k%) + vals(k%)
            next k%

            if prod_qty% = 0 then goto L63080            /* All Qty's    */
               if vals(12%) < prod_qty% then goto gen_next

L63080:     for k% = 1% to 11%
                convert vals(k%) to col$(k%), pic(######)

            next k%

            convert vals(12%) to t_col$, pic(####,###,###)

            convert vals(13%) to t_prc$, pic(##,###,###.##-)

            gosub print_detail
            goto gen_next
        gen_done
            gosub print_totals
            print using L55060
            close printer
            gosub delete_work
        return clear all
            if inc% <> 0% then goto process_data                         ~
                          else goto inputmode

        load_table
            init(" ") readkey$, d1$(), d2$(), d3$(), d4$(), desc$
            t_max% = 0%
            str(readkey$,1%,9%)   = "PLANQUICK"
            str(readkey$,10%,15%) = "00"
        load_table_nxt
            read #3,key > readkey$, using L63430, readkey$, desc$,        ~
                                                 eod goto load_table_done
L63430:        FMT CH(24), CH(30)
            if str(readkey$,1%,9%) <> "PLANQUICK" then                   ~
                                                  goto load_table_done
               t_max% = t_max% + 1%
               d1$(t_max%) = str(desc$,1%,6%)   /* Begin Date          */
               call "DATECONV" (d1$(t_max%))
               d2$(t_max%) = str(desc$,10%,6%)  /* Ending Date         */
               call "DATECONV" (d2$(t_max%))
               d3$(t_max%) = str(desc$,20%,3%)  /* Model Code          */
               d4$(t_max%) = str(desc$,25%,3%)  /* Product Quantity    */
               goto load_table_nxt
        load_table_done
            inc% = 0%
        return

        load_data
            init(" ") beg_date$, beg_dte$, end_date$, end_dte$, prod_cd$,~
                      prod_cd_desc$, prod_qty$
            prod_qty% = 0%
            beg_dte$  = d1$(inc%)         /* Beginning Production date */
            beg_date$ = beg_dte$
            end_dte$  = d2$(inc%)         /* Ending Production Date    */
            end_date$ = end_dte$
            prod_cd$  = d3$(inc%)         /* Product/Model Code        */
            prod_qty$ = d4$(inc%)         /* Product Test Quantity     */
            call "DATFMTC" (beg_date$)    /* Format Begin Date         */
            call "DATFMTC" (end_date$)    /* Format End Date           */
            gosub lookup_model
            convert prod_qty$ to prod_qty%, data goto L63700
L63700:
            inc% = inc% + 1%
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            *************************************************************

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
