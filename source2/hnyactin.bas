        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  H   H  N   N  Y   Y   AAA    CCC   TTTTT  IIIII  N   N   *~
            *  H   H  NN  N  Y   Y  A   A  C   C    T      I    NN  N   *~
            *  HHHHH  N N N   YYY   AAAAA  C        T      I    N N N   *~
            *  H   H  N  NN    Y    A   A  C   C    T      I    N  NN   *~
            *  H   H  N   N    Y    A   A   CCC     T    IIIII  N   N   *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * HNYACTIN - Manages the Part/Activity Cross Reference file *~
            *            HNYACTXF.                                      *~
            *-----------------------------------------------------------*~
            *  This program contains valuable trade secrets and         *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/15/94 ! Original                                 ! ERN *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            act_code$4,                  /* Activity Code              */~
            act_code_descr$32,           /* Activity Code Descr        */~
            bucket$2, bucket_descr$30,   /* Standard Cost Bucket       */~
            bucket_ids$(12)10,           /* Bucket IDs                 */~
            buyer_class$3,               /* Buyer Class Code           */~
            buyer_class_descr$32,        /* Buyer Class Code Descr     */~
            cnv_factor$10,               /* Conversion Factor          */~
            contract_descr$30,           /* Contract Description       */~
            contract_id$16,              /* Default Contract ID        */~
            contract_line$4,             /*                  Line      */~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            part$25,                     /* Part Number                */~
            part_descr$32,               /* Part Description           */~
            part_type$3,                 /* Part Type Code             */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            set$8, setid$4,              /* STC Data                   */~
            text$(392,1)70,              /* Text Array                 */~
            text_id$4,                   /* Text ID                    */~
            unit_price$10,               /* Unit Price                 */~
            uom$4,                       /* Stocking Unit of Measure   */~
            uom_descr$32,                /* Stocking Unit of Measure   */~
            userid$3,                    /* Current User Id            */~
            vendor$9, vendor_descr$30,   /* Default Vendor Code        */~
            vendor_part$25,              /* Vendor Part Number         */~
            vf$200                       /* Var fields in a string     */

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
            * #01 ! HNYACTXF ! HNY, WC ACTIVITY CROSS REFERENCE         *~
            * #02 ! SYSFILE2 ! Caelus Management System Information     *~
            * #03 ! HNYMASTR ! Inventory Master File                    *~
            * #04 ! GENCODES ! System General Codes file.               *~
            * #05 ! VENDOR   ! Vendor Master File                       *~
            * #06 ! TXTFILE  ! System Text File                         *~
            * #07 ! VPCMASTR ! Purchasing Contract File                 *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "HNYACTXF",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =  1,   keylen =  29,                     ~
                        alt key   1,   keypos =  26, keylen =   4, dup

            select #02, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =  1,   keylen =  20

            select #03, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #04, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #05, "VENDOR",                                        ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos = 1, keylen =  9,                         ~
                        alt key  1, keypos = 10, keylen = 30, dup

            select #06, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11

            select #07, "VPCMASTR",                                      ~
                        varc,     indexed,  recsize = 600,               ~
                        keypos =  10,  keylen =  20,                     ~
                        alt key  1, keypos =    1, keylen =  29,         ~
                            key  2, keypos =   60, keylen =  26, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 1%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 1%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))
            call "OPENCHCK" (#05, fs%(05), f2%(05), 0%, rslt$(05))
            call "OPENCHCK" (#06, fs%(06), f2%(06), 0%, rslt$(06))
            call "OPENCHCK" (#07, fs%(07), f2%(07), 0%, rslt$(07))

            if fs%(1) + fs%(2) + fs%(3) + fs%(4) + fs%(5) + fs%(6) = 6   ~
                                                                then L09000
                call "ASKUSER" (keyhit%, "FILE(S) UNAVAILABLE",          ~
                                "Unable to open all files",              ~
                                " ", "Press <RETURN> to continue...")
                goto exit_program


L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,62) = "HNYACTIN: " & str(cms2v$,,8)

*        Get data required to edit and Describe Cost Bucket input.
            buckets% = 2%
            call "STCSETID" (buckets%, #2, set$, setid$, bucket_ids$())
            if buckets% = 0% then                                        ~
                bucket_descr$ = "-- No Std Cost Set in force --"


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  9%
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
                      if keyhit% = 14% and fieldnr% = 1% then L10230
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

            str(line2$,,60) = "Part: "& part$ & "  Activity: "& act_code$
            call "VFINPSUB" ("HNYACTXF", "I", "Part/Activity X-ref   ",  ~
                             str(line2$,,60), "NN", vf$, keyhit%)
            if keyhit% = 1% then inputmode

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            str(line2$,,60) = "Part: "& part$ & "  Activity: "& act_code$
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  5% then       editpg2
                  if keyhit%  =  8% then gosub delete_record
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 25% then gosub edit_text
                  if keyhit% <>  0% then       editpg1
L11160:     fieldnr% = cursor%(1%) - 6%
            if fieldnr% = 0% then fieldnr% = 1%
            if fieldnr% < 3% or fieldnr% >  9% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11220:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11220
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11220
                  lastfieldnr% = fieldnr%
            goto L11160

        editpg2
            call "VFINPSUB" ("HNYACTXF", "E", "Part/Activity X-ref   ",  ~
                             str(line2$,,60), "YN", vf$, keyhit%)
            if keyhit% =  1% then inputmode
            if keyhit% =  4% then editpg1
            if keyhit% = 16% then datasave
                             goto editpg1


        edit_text
            call "TXTINSUB" (#06, f2%(6), "037", str(line2$,,60),        ~
                                                      text_id$, text$())
            return

        delete_record
            call "ASKUSER" (key%, "** CONFIRM DELETION **",              ~
                "Press PF-8 to DELETE this Record", "-OR-",              ~
                "Press RETURN to CANCEL deletion.")
            if key% = 0% then return
                if key% <> 8% then delete_record
                     readkey$ = str(part$) & act_code$
                     call "DELETE" (#1, readkey$, 29%)
                     call "TXTFUTIL" (#6, f2%(6), "DELE", text_id$)
                     return clear all
                     goto inputmode

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            gosub dataput
            goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Part Number            */~
                              L20300,         /* Activity Code          */~
                              L20400,         /* Buyer Class Code       */~
                              L20500,         /* Stocking UOM           */~
                              L20600,         /* Conversion Factor      */~
                              L20700,         /* Unit Price / Bucket    */~
                              L20800,         /* Vendor                 */~
                              L20850,         /* Dflt Contract          */~
                              L20900          /* Vendor Part Number     */
            return


L20100: REM Def/Enable Part Number                 PART$
            return

L20300: REM Def/Enable Activity Code               ACT_CODE$
            return

L20400: REM Def/Enable Buyer Class Code            BUYER_CLASS$
            return

L20500: REM Def/Enable Stocking Unit of Measure    UOM$
            return

L20600: REM Def/Enable Conversion Factor           CNV_FACTOR$
            return

L20700: REM Def/Enable Unit Price, Bucket          UNIT_PRICE$, BUCKET$
            if bucket$ = " " then bucket$ = "1"
            return

L20800: REM Def/Enable Vendor Code                 VENDOR$
            if contract_id$ <> " " then enabled% = 0%
            return

L20850: REM Def/Enable Default Contract            CONTRACT_ID, _LINE$
            return

L20900: REM Def/Enable Vendor Part Number          VENDOR_PART$
            return


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
         "Enter Part Number.                                           ",~
         "Enter Activity Code.                                         ",~
         "Enter Buyer Class Code.                                      ",~
         "Enter Stocking Unit of Measure.                              ",~
         "Enter Conversion Factor.                                     ",~
         "Enter Unit Price and Standard Cost Bucket Number.            ",~
         "Enter default Vendor Code for this activity.                 ",~
         "Enter default Contract ID and Line.                          ",~
         "Enter Vendor's part number.                                  "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      act_code$, act_code_descr$, buyer_class$,          ~
                      buyer_class_descr$, cnv_factor$, part$,            ~
                      part_descr$, unit_price$, uom$, uom_descr$, vf$,   ~
                      bucket$, bucket_descr$, vendor$, vendor_descr$,    ~
                      vendor_part$, contract_id$, contract_line$,        ~
                      contract_descr$
            str(line2$,,60) = " "
            text_id$ = all(hex(ff))
            call "TXTFUTIL" (#06, f2%(6), "INTL", text_id$)
            if buckets% = 0% then                                        ~
                bucket_descr$ = "-- No Std Cost Set in force --"
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
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
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            readkey$ = str(part$,,25) & act_code$
            call "READ100" (#1, readkey$, f1%(1))
            if f1%(1) = 1% then L30130
                cnv_factor = 1
                unit_price = 1
                bucket%    = 1%
                goto format_numbers
L30130:     get #1 using L35030, part$, act_code$, buyer_class$, uom$,    ~
                           cnv_factor, unit_price, bucket%, vendor$,     ~
                           vf$, text_id$, vendor_part$, contract_id$,    ~
                           contract_line$
            readkey$ = part$
            call "DESCRIBE" (#3, readkey$, part_descr$, 0%, f1%(3))
            readkey$ = "WC ACTVTY" & act_code$
            call "DESCRIBE" (#4, readkey$, act_code_descr$, 0%, f1%(4))
            readkey$ = "BYCLASSES" & buyer_class$
            call "DESCRIBE" (#4, readkey$, buyer_class_descr$, 0%, f1%(4))
            readkey$ = "UOM      " & uom$
            call "DESCRIBE" (#4, readkey$, uom_descr$, 0%, f1%(4))
            call "DESCRIBE" (#5, vendor$ , vendor_descr$, 0%, f1%(5))
            readkey$ = str(contract_id$,,16) & contract_line$
            if readkey$ <> " " then                                      ~
                call "DESCRIBE" (#7, readkey$ , contract_descr$, 0%,     ~
                                                                 f1%(7%))
            call "TXTFUTIL" (#06, f2%(6), "LOAD", text_id$)

        format_numbers
            call "CONVERT" (cnv_factor, -0.7   , cnv_factor$)
            call "CONVERT" (unit_price, -0.7   , unit_price$)
            convert bucket% to bucket$, pic(##)
            if bucket% < 10% then bucket$ = str(bucket$,2)
            if buckets% = 0% then                                        ~
                bucket_descr$ = "-- NO STD COST SET IN FORCE --"         ~
            else                                                         ~
                bucket_descr$ = bucket_ids$(bucket%)
            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
            convert cnv_factor$ to cnv_factor
            convert unit_price$ to unit_price
            convert bucket$     to bucket%
            readkey$  = str(part$,,25) & act_code$
            call "READ101" (#1, readkey$, f1%(1))
            put #1 using L35030, part$, act_code$, buyer_class$, uom$,    ~
                           cnv_factor, unit_price, bucket%, vendor$,     ~
                           vf$, text_id$, vendor_part$, contract_id$,    ~
                           contract_line$, " "
            if f1%(1) = 0% then                                          ~
                write   #1                                               ~
            else                                                         ~
                rewrite #1
            call "TXTFUTIL" (#06, f2%(6), "TOS2", text_id$)
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: HNYACTXF                          */~
            CH(25),         /* Part Number (Product Code)              */~
            CH(4),          /* Activity to be performed                */~
            CH(3),          /* Buyer Class Code                        */~
            CH(4),          /* Unit of Measure                         */~
            PD(14,7),       /* Conversion Factor                       */~
            PD(14,7),       /* Unit Price                              */~
            BI(4),          /* Std Cost Bucket Number                  */~
            CH(9),          /* Default Vendor Code                     */~
            CH(200),        /* Variable Fields Data Area               */~
            CH(4),          /* Internal ID to text in TXTFILE          */~
            CH(25),         /* Vendor Part Number                      */~
            CH(16),         /* Contract ID                             */~
            CH(04),         /* Contract Line                           */~
            CH(198)         /* Unused filler area in record            */~

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
              on fieldnr% gosub L40115,         /* Part Number       */   ~
                                L40115,         /* Activity Code     */   ~
                                L40115,         /* Buyer Class Code  */   ~
                                L40115,         /* Stocking UOM      */   ~
                                L40120,         /* Conversion Factor */   ~
                                L40120,         /* Unit Price, Bucket*/   ~
                                L40115,         /* Vendor            */   ~
                                L40115,         /* Contract          */   ~
                                L40110          /* Vendor Part Number*/
              goto L40130

L40110:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40115:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40120:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40130:     accept                                                       ~
               at (01,02),                                               ~
                  "Part, WC Activity Cross Reference",                   ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Part Number",                                ~
               at (06,30), fac(lfac$( 1)), part$                , ch(25),~
                                                                         ~
               at (07,02), "Part Description",                           ~
               at (07,30), fac(hex(8c)),   part_descr$          , ch(32),~
                                                                         ~
               at (08,02), "Activity Code",                              ~
               at (08,30), fac(lfac$( 2)), act_code$            , ch(04),~
               at (08,49), fac(hex(8c)),   act_code_descr$      , ch(32),~
                                                                         ~
               at (09,02), "Buyer Class Code",                           ~
               at (09,30), fac(lfac$( 3)), buyer_class$         , ch(03),~
               at (09,49), fac(hex(8c)),   buyer_class_descr$   , ch(32),~
                                                                         ~
               at (10,02), "Stocking Unit of Measure",                   ~
               at (10,30), fac(lfac$( 4)), uom$                 , ch(04),~
               at (10,49), fac(hex(8c)),   uom_descr$           , ch(32),~
                                                                         ~
               at (11,02), "Conversion Factor",                          ~
               at (11,30), fac(lfac$( 5)), cnv_factor$          , ch(10),~
                                                                         ~
               at (12,02), "Unit Price- Std Cost Bucket",                ~
               at (12,30), fac(lfac$( 6)), unit_price$          , ch(10),~
               at (12,42), fac(lfac$( 6)), bucket$              , ch(02),~
               at (12,49), fac(hex(8c)),   bucket_descr$        , ch(32),~
                                                                         ~
               at (13,02), "Default Vendor",                             ~
               at (13,30), fac(lfac$( 7)), vendor$              , ch(09),~
               at (13,49), fac(hex(8c)),   vendor_descr$        , ch(32),~
                                                                         ~
               at (14,02), "Default Contract - Line",                    ~
               at (14,30), fac(lfac$( 8)), contract_id$         , ch(16),~
               at (14,47), fac(lfac$( 8)), contract_line$       , ch(04),~
               at (14,52), fac(hex(8c)),   contract_descr$      , ch(27),~
                                                                         ~
               at (15,02), "Vendor's Part Number",                       ~
               at (15,30), fac(lfac$( 9)), vendor_part$         , ch(09),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40415
                  call "MANUAL" ("HNYACTIN") : goto L40130

L40415:        if keyhit% <> 15 then L40430
                  call "PRNTSCRN" : goto L40130

L40430:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40535     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "(14) List File         (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0d0e0f1000)
            if fieldnr% = 1% then L40505
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40505:     if fieldnr% < 2% then L40515
                str(pf$(2),38,20) = " "  :  str(pfkeys$,14,1) = hex(ff)
L40515:     if fieldnr% > 3% then L40525
                str(pf$(2),18,20) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40525:     return

L40535: if fieldnr% > 0% then L41060  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                   (8) Delete Record    " &        ~
                     "(25) Manage Text       (15)Print Screen"
            pf$(3) = "(5) Next Screen                         " &        ~
                     "                       (16)Save Data   "
            pfkeys$ = hex(01ffffff05ffff08ffff19ff0dff0f1000)
            if text_id$ = hex(00000000) or text_id$ = " " or             ~
               text_id$ = hex(ffffffff)                      then return
                str(pf$(2),39,1) = hex(84)
                str(pf$(2),57,1) = hex(8c)
                return
L41060:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50190,         /* Part Number            */~
                              L50480,         /* Activity Code          */~
                              L50640,         /* Buyer Class Code       */~
                              L50780,         /* Stocking UOM           */~
                              L50890,         /* Conversion Factor      */~
                              L50940,         /* Unit Price,Cost Bucket */~
                              L51110,         /* Default Vendor Code    */~
                              L51220,         /* Default Contract       */~
                              L51310          /* Vendor's Part Number   */
            return

L50190: REM Test for Part Number                  PART$
            if keyhit% <> 14% then L50330
                plowkey$ = " "
                errormsg$ = hex(06) & "Part Number, Activities on file."
                call "PLOWCODE" (#1, plowkey$, errormsg$, 0%, 0.0, f1%(1))
                errormsg$ = hex(00)
                if f1%(1) = 0% then return
                     part$     = str(plowkey$, ,25)
                     act_code$ = str(plowkey$,26,4)
                     gosub dataload
                     if f1%(1) = 0% then return
                          return clear all
                          goto editpg1

L50330:     if part$ <> " " then L50380
                call "GETCODE" (#3, part$, " ", 0%, 3.32, f1%(3%))
                if f1%(3%) <> 0% then L50420
                     errormsg$ = hex(00)
                     return
L50380:     call "READ100" (#3, part$, f1%(3))
            if f1%(3) <> 0% then L50420
                errormsg$ = "Part not on file.  Please re-enter."
                return
L50420:     get #3 using L50440, part$, part_descr$, uom$, part_type$,    ~
                                buyer_class$
L50440:         FMT CH(25), CH(32), XX(16), CH(4), POS(180), CH(3),      ~
                    POS(200), CH(3)
            return

L50480: REM Test for Activity Code                ACT_CODE$
            plowkey$ = "WC ACTVTY" & act_code$
            act_code_descr$ = " "
            call "PLOWCODE" (#4, plowkey$, act_code_descr$, 9%, .30,     ~
                                                                 f1%(4))
            if f1%(4) = 1% then L50560
                errormsg$ = "Activity Code not on file."
                return
L50560:     act_code$ = str(plowkey$,10)

            gosub dataload
            if f1%(1) = 0% then L50620
                return clear all
                goto editpg1
L50620:     return

L50640: REM Test for Buyer Class Code             BUYER_CLASS$
            if buyer_class$ = " " then return

            plowkey$ = "BYCLASSES" & buyer_class$
            buyer_class_descr$ = hex(06) & "Select Buyer Class Code"
            f1%(4) = -9%
            call "PLOWCODE" (#4, plowkey$, buyer_class_descr$, 9%, 0.3,  ~
                                                                  f1%(4))
            errormsg$ = "Invalid Buyer Class Code.  Please re-enter."
            if f1%(4) = 0% then L50760
                errormsg$ = " "
                buyer_class$ = str(plowkey$,10)
L50760:     return

L50780: REM Test for Stocking Unit of Measure     UOM$
            plowkey$ = "UOM      " & uom$
            uom_descr$ = hex(06) & "Select Unit of Measure Code."
            f1%(4) = -10%
            call "PLOWCODE" (#4, plowkey$, uom_descr$, 9%, 0.3, f1%(4))
            errormsg$ = "Invalid Unit of Measure Code.  Please re-enter."
            if f1%(4) = 0% then L50760
                errormsg$ = " "
                uom$ = str(plowkey$,10)
            return

L50890: REM Test for Conversion Factor            CNV_FACTOR$
            if cnv_factor$ = " " then cnv_factor$ = "1"
            call "NUMTEST" (cnv_factor$, 1e-7, 9e7, errormsg$, 0.7, temp)
            return

L50940: REM Test for Unit Price                   UNIT_PRICE$
            if unit_price$ = " " then unit_price$ = "0"
            call "NUMTEST" (unit_price$, 0, 9e7, errormsg$, 0.7, temp)
            if errormsg$ <> " " then return

            if bucket$ = " " or buckets% = 0% then bucket$ = "1"
            bucket_descr$ = " "
            if buckets% > 0% then L51040
                bucket_descr$ = "-- No Std Cost Set in force --"
                return
L51040:     buckets = buckets%
            call "NUMTEST" (bucket$, 1, buckets, errormsg$, 0.0, bucket)
            if errormsg$ <> " " then return
                bucket%       = bucket
                bucket_descr$ = bucket_ids$(bucket%)
                return

L51110: REM Test for Default Vendor Code          VENDOR$
            vendor_descr$ = " "
            if vendor$    = " " then return
            vendor_descr$ = hex(06) & "Select Default Vendor Code."
            f1%(5) = -13%
            call "PLOWCODE" (#5, vendor$, vendor_descr$, 0%, 0.3, f1%(5))
            if vendor$ = " " then return
                if f1%(5) = 0% then                                      ~
                     errormsg$ = "Invalid Vendor Code.  Please re-enter."
                return

L51220: REM Test for Default Contract              CONTRACT_ID$, _LINE$
            contract_descr$  = " "
            if contract_id$ <> "?" then L51240
                contract_line$ = " "
                goto L51260
L51240:     if contract_id$ <> " " then L51252
                contract_line$ = " "
                return
L51252:     readkey$    = str(contract_id$,,16) & contract_line$
            call "DESCRIBE" (#7, readkey$ , contract_descr$, 0%,         ~
                                                                  f1%(7))
            if f1%(7%) = 1% then L51304
L51260:           call "VPCPIKME" (" "    , contract_id$, contract_line$,~
                                    "A", act_code$, " ", #7, #5, f1%(7%))
                  if contract_id$ = " " then return
                     readkey$ = str(contract_id$,,16) & contract_line$
                     call "DESCRIBE" (#7, readkey$ , contract_descr$, 0%,~
                                                                  f1%(7))
                     if f1%(7%) = 1% then L51304
                          errormsg$ = "Invalid Contract Specified."
                          return
L51304:     get #7 using L51305, vendor$
L51305:         FMT CH(9)
            return

L51310: REM Test for Vendor's Part Number          VENDOR_PART$
            return


        REM THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAELUS,INC~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
