        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  V   V   SSS     A     SSS   H   H  TTTTT   SSS   BBBB    *~
            *  V   V  S       A A   S      H   H    T    S      B   B   *~
            *  V   V   SSS   AAAAA   SSS   HHHHH    T     SSS   BBBB    *~
            *   V V       S  A   A      S  H   H    T        S  B   B   *~
            *    V     SSS   A   A   SSS   H   H    T     SSS   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * VSASHTSB - Prints a Vendor Service Sheet.                 *~
            *            When a Job Step is 'VEND', a sheet is printed  *~
            *            for the outside process based on the VBKVSA    *~
            *            Vendor Service Advice File.                    *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1994  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/11/94 ! Original                                 ! RJ2 *~
            * 11/10/94 ! Added calls to SETPRINT - assigned RPTID.! LDJ *~
            *          ! Also corrected text print routines and   !     *~
            *          ! data load stuff.  Also made alot of      !     *~
            *          ! changes to the print format, also ...    !     *~
            * 07/22/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "VSASHTSB" (                                                 ~
                         arg1$,          /* Advice Number              */~
                         arg2$,          /* How Ship                   */~
                         arg3,           /* Ship Quantity              */~
                         arg4$,          /* Shipping Document Number   */~
                         arg5$,          /* Shipping Date              */~
                         errormsg$   )   /* Error Message              */

        dim                                                              ~
            act_code$4,                  /* Activity Code              */~
            act_descr$30,                /* Activity Description       */~
            advice$8,                    /* Advice Number              */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            comment$40,                  /* Comment                    */~
            company$30,                  /* Company/Division Name      */~
            contract_id$16,              /* Contract ID                */~
            contract_line$4,             /* Contract ID Line           */~
            contract_descr$30,           /* Contract ID Line           */~
            date$8,                      /* Date for screen display    */~
            errormsg$79,                 /* Error message              */~
            howship$30,                  /* How Ship                   */~
            job_nbr$8,                   /* Job Number                 */~
            part$25,                     /* Part Number (activity)     */~
            po$16,                       /* PO Number                  */~
            po_line$3,                   /* PO Line Number             */~
            po_ln_txt$4,                 /* PO Line Text Id            */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            ship_doc$20,                 /* Shipping Document Number   */~
            ship_date$8,                 /* Shipping Date              */~
            ship_qty$10,                 /* Ship Quantity              */~
            step$4,                      /* Route Step                 */~
            time$8,                      /* System Time                */~
            txtid$4,                     /* Text Id                    */~
            uom$4,                       /* Unit of Measure Code       */~
            uom_descr$30,                /* Unit of Measure Description*/~
            userid$3,                    /* Current User Id            */~
            venname$30,                  /* Vendor Name                */~
            vendor$9,                    /* Vendor Code                */~
            venaddr$(5)30,               /* Vendor Address             */~
            wc_start_date$8,             /* Work Center Start Date     */~
            workcntr$4,                  /* Work Center                */~
            xref_txtid$4                 /* Advice Xref Text Id        */

        dim f2%(14),                     /* = 0 if the file is open    */~
            f1%(14),                     /* = 1 if READ was successful */~
            fs%(14),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(14)20                  /* Text from file opening     */

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

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! VBKVSA   ! Vendor Service Advices file              *~
            * #02 ! VENDOR   ! VENDOR MASTER RECORD                     *~
            * #03 ! GENCODES ! System General Codes file.               *~
            * #04 ! TXTFILE  ! System Text File                         *~
            * #05 ! VBKMASTR ! Purchase Order Header File               *~
            * #06 ! VBKLINES ! Purchase Order Line Items File           *~
            * #07 ! VPCMASTR ! Vendor Purchase Master File              *~
            * #08 ! HNYACTXF ! Vendor Service Cross Reference File      *~
            * #09 ! JBMASTR2 ! Job master file                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "VBKVSA",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    5, keylen =   8,                     ~
                        alt key  6, keypos =   50, keylen =   4, dup,    ~
                            key  5, keypos =   41, keylen =  13, dup,    ~
                            key  4, keypos =   29, keylen =   6, dup,    ~
                            key  3, keypos =   13, keylen =  12, dup,    ~
                            key  2, keypos =    2, keylen =  11,         ~
                            key  1, keypos =    1, keylen =  12          ~

            select #02, "VENDOR",                                        ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup     ~

            select #03, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24                      ~

            select #04, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11                      ~

            select # 5, "VBKMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1030,                                 ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  10,  keylen =  16

            select # 6, "VBKLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  700,                                  ~
                        keypos =    1, keylen =  28,                     ~
                        alt key 1, keypos = 333, keylen = 20, dup

            select #7 , "VPCMASTR",                                      ~
                         varc,     indexed,  recsize =  600,             ~
                         keypos =  10, keylen = 20,                      ~
                         alt key  1, keypos =  1, keylen = 29,           ~
                             key  2, keypos = 60, keylen = 26, dup

            select #8 , "HNYACTXF",                                      ~
                         varc,     indexed,  recsize =  512,             ~
                         keypos =   1, keylen = 29,                      ~
                         alt key  1, keypos = 26, keylen =  4, dup

            select #9, "JBMASTR2",                                       ~
                       varc,                                             ~
                       indexed,                                          ~
                       recsize = 1300,                                   ~
                       keypos = 1, keylen = 8,                           ~
                       alt key  1, keypos = 1120, keylen =  19, dup,     ~
                           key  2, keypos =   58, keylen =  25, dup

            call "OPENCHCK" (#01, fs%(01%), f2%(01%), 0%, rslt$(01%))
            call "OPENCHCK" (#02, fs%(02%), f2%(02%), 0%, rslt$(02%))
            call "OPENCHCK" (#03, fs%(03%), f2%(03%), 0%, rslt$(03%))
            call "OPENCHCK" (#04, fs%(04%), f2%(04%), 0%, rslt$(04%))
            call "OPENCHCK" (#05, fs%(05%), f2%(05%), 0%, rslt$(05%))
            call "OPENCHCK" (#06, fs%(06%), f2%(06%), 0%, rslt$(06%))
            call "OPENCHCK" (#07, fs%(07%), f2%(07%), 0%, rslt$(07%))
            call "OPENCHCK" (#08, fs%(08%), f2%(08%), 0%, rslt$(08%))
            call "OPENCHCK" (#09, fs%(09%), f2%(09%), 0%, rslt$(09%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            call "COMPNAME" (2%, company$, comp%)

            advice$    = arg1$     /* Advice Number              */
            howship$   = arg2$     /* How Ship                   */
            ship_qty   = arg3      /* Ship Quantity              */
            ship_doc$  = arg4$     /* Shipping Document Number   */
            ship_date$ = arg5$     /* Shipping Date              */

        REM *************************************************************~
            *     P R I N T   V E N D O R   S E R V I C E   S H E E T   *~
            *-----------------------------------------------------------*~
            * Print data on file for Vendor Service Advice.             *~
            *************************************************************

            gosub initialize_variables
            gosub dataload
            if errormsg$ <> " " then exit_program
            call "SETPRNT" ("VSA001", "VSAS", 1000%, 0%)
            select printer(134)
            gosub print_data
            close printer
            call "SETPRNT" ("VSA001", "VSAS", 0000%, 1%)
            goto exit_program

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") contract_id$,                                      ~
                      act_code$, act_descr$, comment$, contract_line$,   ~
                      contract_descr$, job_nbr$, po$, po_line$, step$,   ~
                      uom$, uom_descr$, vendor$,venname$, venaddr$(),    ~
                      wc_start_date$, workcntr$, ship_qty$

            po_qty, po_rcvd, po_out, vsa_qty = 0
            time$ = " "  :  call "TIME" (time$)
            page% = 0%

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
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            call "SHOSTAT" ("Printing Vendor Service Sheet")

            readkey$ = advice$
            call "READ100" (#1, readkey$, f1%(1%))
            if f1%(1%) <> 0% then L30140
                errormsg$ = "VSA: " & advice$ & " Not on File"
                return

L30140:     get #1 using L35030,  job_nbr$, step$, workcntr$,             ~
                                           wc_start_date$, vendor$,      ~
                                 act_code$, contract_id$, contract_line$,~
                                 po$, po_line$, comment$, vsa_qty, uom$

            if po$ = " " then L30390   /* No PO So lets find what we can */
            readkey$ = po$
            call "READ100" (#5, readkey$, f1%(5%))
            if f1%(5%) = 0% then L30390
            get #5 using L30260, venname$, venaddr$()
L30260:         FMT POS(26), CH(30), 5*CH(30)

            readkey$ = str(vendor$) & str(po$) & po_line$
            call "READ100" (#6, readkey$, f1%(6%))
            if f1%(6%) = 1% then                                         ~
            get #5 using L30350, po_qty,po_rcvd, po_out, po_ln_txt$
L30350:       FMT POS(93), PD(14,4), PD(14,4), PD(14,4),POS(306), CH(4)
            goto L30480

L30390:     /* No PO So get vendor address from Vendor File */
            readkey$ = vendor$
            call "READ100" (#2, readkey$, f1%(2%))
            if f1%(2%) <> 0% then  L30450      /* No Vendor on file */
                errormsg$ = "Vendor: " & vendor$ & " Not on File"
                return
L30450:     get #2 using L30460, venname$, venaddr$()
L30460:         FMT POS(40), CH(30), 5*CH(30)

L30480:     /* Get Part - Activity Xref Text ID */
            readkey$ = job_nbr$
            call "READ100" (#9, readkey$, f1%(9%))
            if f1%(9%) =  0% then  L30600    /* Job not on File */
            get #9 using L30530, part$
L30530:         FMT POS(58), CH(25)
            readkey$ = str(part$)  & act_code$
            call "READ100" (#8, readkey$, f1%(8%))
            if f1%(8%)  = 0% then  L30600
            get #8 using L30580, xref_txtid$
L30580:         FMT POS(266), CH(4)

L30600:     /* Fill in the details */
            if contract_id$ = " "  then L30650
            readkey$ = str(contract_id$) & contract_line$
            call "DESCRIBE" (#7, readkey$, contract_descr$, 0%, f1%(7%))

L30650:     if act_code$ = " " then L30690
            readkey$ = "WC ACTVTY" & act_code$
            call "DESCRIBE" (#3 , readkey$, act_descr$, 0%, f1%(3%))

L30690:     if uom$ = " " then L30720
            readkey$ = "UOM      " & uom$
            call "DESCRIBE" (#3 , readkey$, uom_descr$, 1%, f1%(3%))
L30720:
            if ship_date$ <> " " and ship_date$ <> blankdate$ then L30750
                ship_date$ = wc_start_date$
L30750:     call "DATEFMT" (ship_date$)

           /* Set Best Guess Ship Quantity */
            if ship_qty  <>  0  then L30820
            if po$        = " " then L30850     /* Try for VSA */
                ship_qty = po_qty - po_rcvd
                if ship_qty < po_out then ship_qty = po_out
L30820:    if ship_qty < 0 then L30880      /* No Negatives */
           call "CONVERT" (ship_qty, -2.2, ship_qty$)
           goto L30880
L30850:    if vsa_qty = 0 then L30880
                call "CONVERT" (vsa_qty, -2.2, ship_qty$)

L30880:    /* Any More Tests ???? */
            call "LINSMASH" (venaddr$())

            return

        REM *************************************************************~
            *          S T U F F   D A T A   O N T O   S H E E T        *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables onto Printer           *~
            *************************************************************
        print_data
            gosub print_heading
            print using L60080, venname$
            for x% = 1% to 5%
                if venaddr$(x%) > " " then print using L60100, venaddr$(x%)
            next x%
            print skip (2)
            print using L60220, contract_id$, contract_line$,             ~
                                   contract_descr$
            print using L60250, job_nbr$
            print using L60260, workcntr$
            print
            print using L60270, howship$
            print using L60280, ship_doc$
            print using L60300, ship_date$
            print
            print using L60382, po$, po_line$
            line% = 22%
            /* Print PO Line Text */
            if po_ln_txt$ = " " or po_ln_txt$ = hex(ffffffff) then L31280
            txtid$ = po_ln_txt$
L31265:     call "TXTPRINT"(#04, f2%(4%), 134%, txtid$, "VSA001" , 9%,   ~
                            line%, 56%, "Y", " ", comp%)
            if comp% = 0% then L31280
                gosub print_heading
                goto L31265

L31280:     print
            print using L60320, ship_qty$, uom$, uom_descr$
            print using L60350, act_code$, act_descr$

            /* Print Advice Xref Text Id */
            if xref_txtid$ = " " or xref_txtid$=hex(ffffffff) then L31435
            txtid$ = xref_txtid$
L31410:     call "TXTPRINT"(#04, f2%(4%), 134%, txtid$, "VSA001" , 9%,   ~
                            line%, 56%, "Y", " ", comp%)
            if comp% = 0% then L31435
                gosub print_heading
                goto L31410

L31435:     print
            print using L60400
            return

        print_heading
            page% = page% + 1%
            print page
            print using L60050, date$, company$, page%
            print using L60070, time$, cms2v$
            print using L60075, advice$
            print skip (3)
            line% = 5%
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: VBKVSA                            */~
            POS(13),        /* Status Indicator                        */~
                            /* Buyer/planner code                      */~
                            /* Advice Number                           */~
            CH(8),          /* Job Number                              */~
            CH(4),          /* RTE Step To Identify A Route Line       */~
            CH(4),          /* Work Center ID                          */~
            CH(06),         /* WC Start Date                           */~
            POS(41),        /* WC End Date                             */~
            CH(9),          /* Vendor                                  */~
            CH(04),         /* Activity to be performed                */~
            CH(16),         /* Purchasing Contract ID                  */~
            CH(4),          /* Contract Line Number                    */~
            CH(16),         /* Purchase Order Number                   */~
            CH(03),         /* Purchase Order Line Number              */~
            POS(94),        /* Automatic Adjustment Flag               */~
            CH(40),         /* Comment                                 */~
            PD(14,4),       /* Quantity to Buy                         */~
            CH(4)           /* Unit of Measure                         */~
                            /* Unit Price                              */~
                            /* Standard Cost Bucket                    */~
                            /* Vendor Part Number                      */~
                            /* Transaction currency code               */~
                            /* Unused filler area in record (reserved b*/~

        REM *************************************************************~
            *             I M A G E   S T A T E M E N T S               *~
            *                                                           *~
            *-----+----------+------------------------------------------*~

*        Header
L60050: %Run Date: ########            ##############################    ~
        ~  Page:##
L60070: %    Time: ########                 VENDOR SERVICE SHEET         ~
        ~VSA001:########

L60075: %                               FOR SERVICE ADVICE: ########

L60080: %Service Supplier : #############################

L60100: %                   #############################

L60220: %Service Contract : ################ ####   #####################~
        ~#######

L60250: %Our Job Number   : ########

L60260: %Our Work Center  : ####

L60270: %How Ship/Ship Via: ##############################
L60280: %Shipping Document: ####################

L60300: %Ship By Date     : ########

L60320: %Quantity         :  ##########  Units: ####  ###################~
        ~#############

L60350: %Service Activity : #### - ################################

L60382: %Our P.O. Number  : ################ ###

L60400: %                       *** END - VENDOR SERVICE SHEET - END ***

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

            end
