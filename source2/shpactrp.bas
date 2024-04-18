        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *   SSS   H   H  PPPP    AAA    CCC   TTTTT  RRRR   PPPP    *~
            *  S      H   H  P   P  A   A  C   C    T    R   R  P   P   *~
            *   SSS   HHHHH  PPPP   AAAAA  C        T    RRRR   PPPP    *~
            *      S  H   H  P      A   A  C   C    T    R  R   P       *~
            *   SSS   H   H  P      A   A   CCC     T    R   R  P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SHPACTRP - This program creates a report of shipping      *~
            *            activities with selections by date, customer   *~
            *            code, shipping carrier, and invoice type. The  *~
            *            report may contain details of individual parts *~
            *-----------------------------------------------------------*~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/01/91 ! Original                                 ! RJH *~
            * 11/30/92 ! PRR 12625  Corrected printing of other   ! JDH *~
            *          !   than Pre-Inv, SOs, and Manual.         !     *~
            * 12/07/92 ! Eliminated scheduled BOLs from list.     ! JDH *~
            * 12/30/93 ! PRR 13079. DIMed readkey. Happy New Year.! JDH *~
            * 04/04/95 ! PRR 13187 - Print Xref Parts.            ! RJH *~
            * 08/23/96 ! Century date conversion                  ! DER *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        dim                                                              ~
            blankdate$8,                 /* blank unfmt date           */~
            bolnum$3,                    /* Bill of Lading Number      */~
            carrier$7,                   /* Carrier Code               */~
            carrierdesc$30,              /* Carrier Description        */~
            columnttl$51,                /* Column titles line         */~
            company$60,                  /* Company or Division Name   */~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$9,                   /* Customer code              */~
            cuscodeorg$9,                /* Original Cuscode in loop   */~
            cusdesc$30,                  /* Customer Description       */~
            cusshiptots$4,               /* Customer shipment number   */~
            cusvaltotals$12,             /* Customer total value shiped*/~
            cusfrghttots$10,             /* Customer total freight cost*/~
            date$8,                      /* Date for screen display    */~
            datetime$7,                  /* Date timme value           */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            fmcarrierrange$6,            /* Carrier Range              */~
            fmcustomerrange$9,           /* Customer Range             */~
            fmshipdate$10,               /* Date Range                 */~
            freight$10,                  /* Freight Charges            */~
            frghttots$10,                /* Freight totals             */~
            hicarrierrange$6,            /* Carrier Range              */~
            hicustomerrange$9,           /* Customer Range             */~
            hishipdate$10,               /* Date Range                 */~
            i$(24)80,                    /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            invoicenum$17,               /* Invoice Number             */~
            invd$1,                      /* Invoiced Flag (Y or N)     */~
            invdflag$1,                  /* Invoiced Flag print chr    */~
            invdtotflag$1,               /* Invoiced Flag prn ch (tots)*/~
            invdate$8,                   /* Invoice date               */~
            invreport$(7),               /* Invoice Report Code        */~
            invtype$7,                   /* Invoice type code          */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            locarrierrange$6,            /* Carrier Range              */~
            locustomerrange$9,           /* Customer Range             */~
            loshipdate$10,               /* Date Range                 */~
            partdetailflag$1,            /* Include Part Detail?       */~
            partdesc$32,                 /* Part Description           */~
            partnum$25,                  /* Part Number                */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            plowkeyline$99,              /* Plow key for line items    */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            preinvoiceflag$1,            /* Pre-invoice option         */~
            printingpartflag$1,          /* Flag if printing part nums */~
            readkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rptid$6,                     /* Report ID                  */~
            rpttitle$60,                 /* Report Title               */~
            shipdate$8,                  /* Ship date                  */~
            shipdateorg$8,               /* Ship date Original         */~
            shiptotals$10,               /* Total shipment value       */~
            shipqnt$10,                  /* Shipped Quantity           */~
            shipvalue$10,                /* Shipment value             */~
            solnum$16,                   /* Sales Order Number         */~
            solnumorg$16,                /* Sales Order number original*/~
            stockuom$4,                  /* Stocking UOM               */~
            store$3,                     /* Store code                 */~
            temp$60,                     /* Temporary variable         */~
            time$8,                      /* System Time                */~
            tocarrierrange$6,            /* Carrier Range:             */~
            tocustomerrange$9,           /* Customer Range:            */~
            toshipdate$10,               /* Date Range:                */~
            userid$3,                    /* Current User Id            */~
            valtotals$12,                /* Total shipment value       */~
            xref_cus$1,                  /* Print Customer Xref Part   */~
            xref_sys_cus$1,              /* Print Customer Xref Part   */~
            xref_mnf$1,                  /* Print Manufactur Xref Part */~
            xref_sys_mnf$1,              /* Print Manufactur Xref Part */~
            xref_descr$32,               /* Xref Part # Description    */~
            xref_part$25,                /* Xref Part Number           */~
            xref_type$1                  /* Xref Part Type(Cust or Mnf)*/

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

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! ARILINES ! Invoice Line Items File                  *~
            * #02 ! ARIMASTR ! Invoice Master File                      *~
            * #03 ! BCKLINES ! BACK LOG LINE ITEM FILE                  *~
            * #04 ! CUSTOMER ! Customer Master File                     *~
            * #05 ! GENCODES ! System General Codes file.               *~
            * #06 ! HNYMASTR ! Inventory Master File                    *~
            * #07 ! SHPHDRS  ! Shipment Scheduling / Pre-Invoicing- Hea *~
            * #08 ! SHPLINES ! Shipment Scheduling / Pre-Invoicing- Lin *~
            * #40 ! WORKFILE ! Temporary System Workfile                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "ARILINES",                                      ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =    1, keylen =  20                      ~

            select #02, "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  17,                     ~
                        alt key  3, keypos =   34, keylen =  16, dup,    ~
                            key  2, keypos =   18, keylen =  16, dup,    ~
                            key  1, keypos =   10, keylen =   8, dup     ~

            select #03, "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19                      ~

            select #04, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  5, keypos = 1049, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  1, keypos =   10, keylen =  30, dup     ~

            select #05, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24                      ~

            select #06, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  3, keypos =   26, keylen =  32, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  1, keypos =  102, keylen =   9, dup     ~

            select #07, "SHPHDRS",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  28                      ~

            select #08, "SHPLINES",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =   10, keylen =  22                      ~

            select #40, "WORKFILE",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  41

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#01, fs%(01), f2%(01), 0%, rslt$(01))
            call "OPENCHCK" (#02, fs%(02), f2%(02), 0%, rslt$(02))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
            call "OPENCHCK" (#04, fs%(04), f2%(04), 0%, rslt$(04))
            call "OPENCHCK" (#05, fs%(05), f2%(05), 0%, rslt$(05))
            call "OPENCHCK" (#06, fs%(06), f2%(06), 0%, rslt$(06))
            call "OPENCHCK" (#07, fs%(07), f2%(07), 0%, rslt$(07))
            call "OPENCHCK" (#08, fs%(08), f2%(08), 0%, rslt$(08))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            call "COMPNAME" (12%, company$, ret%)
            ret% = ret%                /* Prevents used only once error*/
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            rpttitle$ = "                 "         &                    ~
                        "Shipping Activity Report"
            rptid$ = "SHP010"
            str(columnttl$, 1) = "Beginning Code"
            str(columnttl$,27) = "Ending Code"

            str(line2$,62) = "SHPACTRP: " & str(cms2v$,,8)

            call "BCKSWTCH" ("BCK", "XREF_CUS", xref_sys_cus$,           ~
                               xref_sys_cus%, ret%)
                   xref_sys_cus% = xref_sys_cus%  /* Do nothing line */
            call "BCKSWTCH" ("BCK", "XREF_MNF", xref_sys_mnf$,           ~
                               xref_sys_mnf%, ret%)
                   xref_sys_mnf% = xref_sys_mnf%  /* Do nothing line */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles INPUT MODE for range selection screen.            *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  6%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                     if keyhit%  =  1% then gosub startover
                     if keyhit% <>  4% then       L10210
L10160:                   fieldnr% = max(1%, fieldnr% - 1%)
                          gosub'051(fieldnr%)
                          if enabled% = 1% then L10130
                          if fieldnr% = 1% then L10110
                          goto L10160
L10210:              if keyhit% = 16% and fieldnr% = 1% then exit_program
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
            if fieldnr% > 6 then fieldnr% = fieldnr% - 1%  /* allow for*/~
                                                /* 2 Lines in Field 6% */
            if fieldnr% < 1% or fieldnr% >  6% then editpg1
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
        extract_data

        REM March through the SHPHDRS file to find the non-invoiced      ~
           records that satisfy the range parameters.  Then march through~
           the ARIMASTR file for the  invoiced records.  Extract the     ~
           necessary data for each record of interest and write to  the  ~
           temporary WORKFILE.

            call "WORKOPEN"  (#40, "IO", 500%, f2%(40))

            init (" ") cuscode$, shipdate$, sonum$, bolnum$, store$,     ~
                       carrier$, invoicenum$, invdate$, invtype$
            freight, stockprice, discprcnt, shipvalue = 0

            if fmcustomerrange$ = "ALL"                                  ~
                then   fmcustomerrange$ = all(hex(00))
            if preinvoiceflag$ <> "Y" then goto plow_arimastr

*           * PLOW SHPHDRS  ON #07  (NOT INVOICED) *
            str(plowkey$,  1,  9) = str(fmcustomerrange$, 1, 9)
            str(plowkey$, 10, 18) = hex(00)

            call "SHOSTAT"   ("SEARCHING THE SHIPPING FILES")
         plowloop1
            call "PLOWNEXT"  (#07, plowkey$, 0%,  f1%(07))
                if f1%(07) = 0 then  plow_arimastr

            get #07 using L36350,                                         ~
                          cuscode$, sonum$, bolnum$, store$,             ~
                          carrier$, shipdate$, freight, invoicenum$

            if cuscode$ >  hicustomerrange$ then plow_arimastr

            /* Schld, not shipped */
            if shipdate$ = " " or shipdate$ = blankdate$ then plowloop1

*           *  Check for Valid Range  *
            if invoicenum$ <> " " then plowloop1

            if fmshipdate$ = "ALL" then L13310
            if shipdate$ < loshipdate$ or shipdate$ > hishipdate$        ~
                then plowloop1         /* PLOWNEXT */

L13310:     if fmcarrierrange$ = "ALL"  then L13345
                if   carrier$ < locarrierrange$  or                      ~
                     carrier$ > hicarrierrange$                          ~
                   then plowloop1
L13345
*           ***  Processing a Valid Shipment  ***
*              *  Capture Records  *
            str(plowkeyline$,  1, 22) = str(sonum$, 1, 16) &             ~
                                        str(bolnum$, 1, 3) &  hex(00)

            shipvalue = 0

L13380:     call "PLOWNEXT" (#08, plowkeyline$, 19%, f1%(08))/*SHPLINES */
            if f1%(08) <> 0%  then  L13405
                gosub write_work_file
                goto plowloop1

L13405:     qnt, linevalue, discamt, discprcnt, stockprice = 0

            get #08 using L13420,  seqnum$, qnt
L13420:        FMT   POS(29), CH(3), PD(14,4)

            readkey$ = str(sonum$, 1, 16) & str(seqnum$, 1, 3)
            call "READ100" (#03, readkey$, f1%(03))       /* BCKLINES */
            if f1%(03) = 0%   then  L13480
                get #03 using L13450, stockprice, discprcnt
L13450:         FMT  POS(141), PD(14,4), POS(173), PD(14,4)

                linevalue = qnt * stockprice
                discamt   = linevalue * discprcnt
                shipvalue = shipvalue + linevalue - discamt
L13480:     goto L13380      /* Back to PLOWNEXT */
*           * END PLOWLOOP1 *

        plow_arimastr  /* ON #02  (INVOICED)  */

            str(plowkey$,  1,  9) = str(fmcustomerrange$, 1, 9)
            str(plowkey$, 10, 18) = hex(00)

            call "SHOSTAT"   ("SEARCHING THE INVOICE FILES")
        plowloop2    /* Main subfunction of PLOW_AIRMASTR */

            call "PLOWNEXT"  (#02, plowkey$, 0%,  f1%(02))
                if f1%(02) = 0 then  generate_report

            get #02 using L35385,                                         ~
                       cuscode$, invoicenum$, sonum$, bolnum$, shipdate$,~
                       carrier$, invdate$, grossamt, discprcnt, discamt, ~
                       freight,  store$, invtype$

*           * Check for Valid Range  *
            if cuscode$ >  hicustomerrange$ then generate_report
            if shipdate$  = " " or shipdate$ = blankdate$ then ~
               shipdate$  = invdate$
            if fmshipdate$ = "ALL" then L13891

            if shipdate$ < loshipdate$  or  shipdate$ > hishipdate$      ~
                then plowloop2     /* PLOWNEXT */

L13891:     if fmcarrierrange$ = "ALL"  then L13900
                if  carrier$ < locarrierrange$   or                      ~
                    carrier$ > hicarrierrange$                           ~
                   then plowloop2
L13900:    gosub check_invtype
           if invtypeok$ <> "Y" then plowloop2

*           * Process a Valid Shipment *
            shipvalue = grossamt  +  discamt
            gosub write_work_file
            goto plowloop2            /* Loop back to PLOWNEXT */


        write_work_file

L16060:     call "GETDTTM" addr(datetime$)   /* For time stamp */

            write #40 using L16120,                                       ~
                         cuscode$, shipdate$, sonum$, bolnum$,           ~
                         datetime$, store$, carrier$, shipvalue,         ~
                         freight, invoicenum$, invtype$, eod goto L16060

L16120:     FMT                         /* FILE: WORKFILE */             ~
                 CH(9), CH(6), CH(16), CH(3), CH(7), CH(3), CH(6),       ~
                 PD(14,4), PD(14,4), CH(17), CH(7)

            return    /* END Sub-Function WRITE_WORK_FILE */

        check_invtype    /* Check for Valid Inventory Type and Expand */
           invtypeok$ = "N"
           if invtype$ = " " then return
           if invtype$ <> "M" then L16246                     /* Manual */
               if invreport$(1%) =  " " then return
               invtype$ = "MANUAL "  :  invtypeok$ = "Y"
               return
L16246:    if invtype$ <> "O" then L16250                /* Sales Order */
               if invreport$(2%) =  " " then return
               invtype$ = "S.ORDER"  :  invtypeok$ = "Y"
               return
L16250:    if invtype$ <> "A"  then L16270                   /* Adjust */
               if invreport$(3%) =  " " then return
               invtype$ = "ADJUST "  :  invtypeok$ = "Y"
               return
L16270:    if invtype$ <> "C" then L16290                    /* Credit */
               if invreport$(4%) =  " " then return
               invtype$ = "CREDIT "  :  invtypeok$ = "Y"
               return
L16290:    if invtype$ <> "D" then L16310                    /* Direct */
               if invreport$(5%) =  " " then return
               invtype$ = "DIRECT "  :  invtypeok$ = "Y"
               return
L16310:    if invtype$ <> "X" then L16350               /* Export Order */
               if invreport$(6%) =  " " then return
               invtype$ = "EXPORT "  :  invtypeok$ = "Y"
               return
*         IF INVTYPE$ <> "F" THEN 16350                    /* Finance */
*             IF INVREPORT$(8%) =  " " THEN RETURN
*             INVTYPE$ = "FINANCE"  :  INVTYPEOK$ = "Y"
*             RETURN
L16350:    if invtype$ <> "G" then L16420                   /* Generate */
               if invreport$(7%) =  " " then return
               invtype$ = "GENERAT"  :  invtypeok$ = "Y"

L16420:    return     /* END Sub-Function CHECK_INVTYPE  */

        check_xref_print_settings
            xref_cus$ = xref_sys_cus$
            xref_mnf$ = xref_sys_mnf$

            call "READ100" (#4, cuscode$, f1%(4%))
            if f1%(4%) = 0% then return   /* Shouldn't happen */

            get #4 using L16550, temp_cus$, temp_mnf$
L16550:         FMT POS(1092), CH(1), CH(1)

            if temp_cus$ = " " then L16600
                if pos("YNB" = temp_cus$) <> 0% then xref_cus$ = temp_cus$

L16600:     if temp_mnf$ = " " then L16630
                if pos("YNB" = temp_mnf$) <> 0% then xref_mnf$ = temp_mnf$

L16630:     return


        REM *************************************************************~
            *     D E F A U L T / E N A B L E S   F O R   R A N G E S   *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields Range Inputs             *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20100,         /* Date Range:            */~
                              L20200,         /* Customer Range:        */~
                              L20300,         /* Carrier Range:         */~
                              L20400,         /* Part Detail Flag       */~
                              L20600,         /* Pre-Invoice Flag       */~
                              L20500          /* Invoice Report Code    */

            return
L20100: REM Def/Enable Date Range                  FMSHIPDATE$
            if fmshipdate$ <> " " then return
                fmshipdate$ = "ALL"
                toshipdate$ = " "
                return

L20200: REM Def/Enable Customer Range              FMCUSTOMERRANGE$
            if fmcustomerrange$    = " " then                            ~
               fmcustomerrange$    = "ALL"
            return

L20300: REM Def/Enable Carrier Range               FMCARRIERRANGE$
            if fmcarrierrange$     = " "                                 ~
               then  fmcarrierrange$     = "ALL"
            return

L20400: REM Def/Enable Include Part Detail?          PARTDETAILFLAG$
            if partdetailflag$ =  " "    then partdetailflag$ = "N"
            return

L20500: REM Def/Enable Invoice Selection Code      INVREPORT$()
            if str(invreport$()) <> " " then return
                invreport$(1), invreport$(2), invreport$(5),             ~
                invreport$(6)  = "X"
                invreport$(3), invreport$(4), invreport$(7)  = " "

                return

L20600: REM Def/Enable Pre-Invoice Flag ?            PARTDETAILFLAG$
            if preinvoiceflag$ =  " "   then preinvoiceflag$ = "Y"
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
            restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg  :  data                                               ~
         "Enter Date Range:                                            ",~
         "Enter Customer Range:                                        ",~
         "Enter Carrier Range:                                         ",~
         "Include Part Detail ('Y' or 'N')?                            ",~
         "Include Pre-Invoiced Shipments ('Y' or 'N')?                 ",~
         "Place a Non-Blank Character Next to Types to Print.          "


        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, invreport$(),              ~
                      fmcarrierrange$, fmcustomerrange$,                 ~
                      fmshipdate$, hicarrierrange$,                      ~
                      hicustomerrange$, hishipdate$,                     ~
                      locarrierrange$, locustomerrange$,                 ~
                      loshipdate$, tocarrierrange$, preinvoiceflag$,     ~
                      tocustomerrange$, partdetailflag$, toshipdate$
            return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
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
            *       G E N E R A T E   R E P O R T   S E C T I O N       *~
            *-----------------------------------------------------------*~
            * Main section for report generation.                       *~
            *************************************************************
        REM March through the WORKFILE and extract the information and   ~
            keep running totals.  Print each line of information.  Check ~
            for changed Customer Codes and print totals at each new      ~
            Customer Code.  Print Grand Totals at end of report.

        generate_report
            call  "SHOSTAT" ("PRINTING REPORT")
            select printer(134)
            time$ = " "  :  call "TIME" (time$)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            pcntr% = -1% : lcntr% = 99% /* Page & Line Counters */

*       * Report Generation Logic **
            init ("  ") cuscode$, shipdate$, sonum$, bolnum$, datetime$, ~
                        store$, carrier$, invoicenum$, invd$, invdflag$, ~
                        invdtotflag$
            shipvalue, freight = 0

*          ** Start Report Loop  **

            cusshiptots%, shiptotals% = 0%
            cusvaltotals,  cusfrghttots, valtotals, frghttots  = 0
            plowkey$ = all(hex(00))
            call "READ102" (#40, plowkey$, f1%(40))
            goto L30430

        reportloop
            call "READNEXT" (#40, f1%(40))
L30430:     if f1%(40) = 0% then goto  print_report_totals
*           * Set Original Values *
            cuscodeorg$  = cuscode$
            shipdateorg$ = shipdate$
            solnumorg$   = sonum$
*           * Set New Values *
            get #40 using  L16120,                                        ~
                        cuscode$, shipdate$, sonum$, bolnum$,            ~
                        datetime$, store$, carrier$, shipvalue,          ~
                        freight, invoicenum$, invtype$

            call "DESCRIBE"  (#05, carrier$, carrierdesc$, 0%, f1%(5))
            call "CONVERT"   (shipvalue, 2.2, shipvalue$)
            call "CONVERT"   (freight, 2.2, freight$)
*           * Check for Invoice *
            invd$ = "Y"
            if invoicenum$ <> " " then L30620
                invd$     = "N"
                invdtotflag$ = "*"

*           * Compare Original to New Values *
L30620:     if cuscode$ <> cuscodeorg$ then  goto new_cuscode

            cusshiptots%  = cusshiptots%  + 1%
            cusvaltotals  = cusvaltotals + shipvalue
            cusfrghttots  = cusfrghttots + freight
            if invd$ = "N" then invdflag$ = "*"

            if lcntr% < 55    then L30662
                gosub page_head
                goto print_new_cuscode

L30662:     if partdetailflag$ = "Y"  then  goto print_new_cuscode
            lcntr%            = lcntr% + 1
            printingpartflag$ = "N"
            if shipdate$ <> shipdateorg$ then goto print_new_shipdate
            if solnum$ <> solnumorg$ then goto print_new_solnum
            goto print_new_bolnum
*           * END Sub-Function REPORTLOOP *

         new_cuscode
            if cuscodeorg$  <> " "  then  gosub print_custotals          ~
                else gosub page_head

            cusshiptots% = 1%
            cusvaltotals = shipvalue
            cusfrghttots = freight
            if invd$ = "N" then invdflag$ = "*" else invdflag$ = " "

            gosub check_xref_print_settings

            call "DESCRIBE" (#04, cuscode$, cusdesc$, 0%, f1%(4))
            if lcntr% > 54 then gosub page_head
            goto print_new_cuscode

        part_detail_loop
            if partdetailflag$ = "N" then goto reportloop
            if invoicenum$ = " " then plow_shiplines

*           * Plow ARILINES *
            str(plowkeyline$, 1, 9) =  str(cuscode$,1 ,9)
            str(plowkeyline$,10, 8) =  str(invoicenum$,1 ,8)
            str(plowkeyline$,18, 3) =  hex(00)

L31080:     call "PLOWNEXT"   ( #01, plowkeyline$, 17%, f1%(1))
            if f1%(1) <> 0% then L31110
                print
                lcntr% = lcntr% + 1%
                goto reportloop

L31110:     get #01 using L31115, seqnum$, partnum$, shipqnt, stockuom$
L31115:         FMT POS(18), CH(3), POS(24), CH(25), POS(93), PD(14,4),  ~
                     POS(117), CH(4)
            if shipqnt = 0.0 then goto L31080   /* Supress zero quantity */

            if xref_cus$ = " " or xref_cus$ = "N" then L31160
                /* Get Xref Part Number, if available */
                call "PTUSEDSB" ( "R", "ARI ",                           ~
                              str(plowkeyline$,,17%),  seqnum$,          ~
                              xref_part$, xref_descr$, xref_type$, ret%)
                if ret% = 0% then xref_part$,xref_descr$,xref_type$ = " "

L31160:     gosub print_partnum
            goto L31080       /* Plow Next AIRLINES */

        plow_shiplines
            str(plowkeyline$, 1,16) =  str(sonum$,1 ,16)
            str(plowkeyline$,17, 3) =  str(bolnum$,1 ,3)
            str(plowkeyline$,20, 3) =  hex(00)
            partnum$, stockuom$ = " "

L31330:     call "PLOWNEXT" ( #08, plowkeyline$, 19%, f1%(8))
            if f1%(8) <> 0% then L31360
                print
                lcntr% = lcntr% + 1%
                goto reportloop

L31360:     get #08 using L31370, seqnum$, shipqnt
L31370:         FMT POS(29), CH(3), PD(14,4)
            if shipqnt = 0.0 then L31330

            readkey$ = str(sonum$, 1, 16) & str(seqnum$, 1, 3)
            call "READ100" (#03, readkey$, f1%(03))       /* BCKLINES */
            if f1%(03) = 0%   then  L31450
                get #03 using L31430, partnum$, stockuom$
L31430:         FMT  POS(32), CH(25), POS(149), CH(4)
                if xref_cus$ = " " or xref_cus$ = "N" then L31450
                    /* Get Xref Part Number, if available */
                    call "PTUSEDSB" ( "R", "BCK ", str(readkey$,,16%),   ~
                                      seqnum$, xref_part$, xref_descr$,  ~
                                      xref_type$, ret%)
                    if ret% = 0%                                         ~
                             then xref_part$,xref_descr$,xref_type$ = " "

L31450:     gosub print_partnum
            goto L31330

        sum_grandtotals
            shiptotals% = shiptotals% + cusshiptots%
            valtotals   = valtotals   + cusvaltotals
            frghttots   = frghttots   + cusfrghttots
            return

*        ******** PRINTING SUB_FUNCTIONS  *********

        print_new_cuscode
            call "DATEFMT" (shipdate$)
            print using L60310, cuscode$, str(cusdesc$,,28), shipdate$,   ~
                               sonum$, bolnum$, store$, invoicenum$,     ~
                               invtype$, carrier$, shipvalue$, freight$
            call "DATUNFMT" (shipdate$)
            lcntr% = lcntr% + 1%
            printingpartflag$ = "N"
            goto part_detail_loop

        print_new_shipdate
            call "DATEFMT" (shipdate$)
            print using L60370, shipdate$, sonum$, bolnum$, store$,       ~
                               invoicenum$, invtype$, carrier$,          ~
                               shipvalue$, freight$
            call "DATUNFMT" (shipdate$)
            goto part_detail_loop

        print_new_solnum
            print using L60430, sonum$, bolnum$, store$, invoicenum$,     ~
                               invtype$, carrier$, shipvalue$, freight$
            goto part_detail_loop

        print_new_bolnum
            print using L60474, bolnum$, store$, invoicenum$,             ~
                               invtype$, carrier$, shipvalue$, freight$
            goto part_detail_loop

        print_partnum
            if lcntr% > 54 then gosub page_head
            call "CONVERT"   (shipqnt,  2.2, shipqnt$)
            call "DESCRIBE"  (#06, partnum$, partdesc$, 0%, f1%(6))
            print using  L60590, partnum$, partdesc$, shipqnt$, stockuom$
            lcntr% = lcntr% + 1
            printingpartflag$ = "Y"

            if xref_cus$ = " " or xref_cus$ = "N" or xref_part$ = " "    ~
                                                              then L32748
                if xref_type$ = "M" then print using L60620,"Manufactor "&~
                                     "Part No: ", xref_part$, xref_descr$~
                                    else print using L60620,"Customer " & ~
                                     "Part No: ", xref_part$, xref_descr$
                lcntr% = lcntr% + 1

L32748:     xref_part$, xref_descr$ = " "
            return

         print_custotals
            cusshiptots = cusshiptots%
            call "CONVERT" (cusshiptots , -0.01, cusshiptots$)
            call "CONVERT" (cusvaltotals,  -2.2, cusvaltotals$)
            call "CONVERT" (cusfrghttots,  -2.2, cusfrghttots$)
            if lcntr% > 54 then gosub page_head
            if printingpartflag$ = "N" then print
            print using L60510, cuscodeorg$, cusshiptots$, invdflag$,     ~
                               cusvaltotals$, cusfrghttots$
            print
            gosub sum_grandtotals
            if printingpartflag$ = "N" then  lcntr% = lcntr% + 3         ~
                else    lcntr% = lcntr% + 2
            return

         print_report_totals
            cuscodeorg$ = cuscode$
            gosub print_custotals     /* DO THE LAST CUSTOTAL */
            shiptotals = shiptotals%
            call "CONVERT" (shiptotals,   -0.01, shiptotals$)
            call "CONVERT" (valtotals,    -2.2 , valtotals$ )
            call "CONVERT" (frghttots,    -2.2 , frghttots$ )
            print using L60550, shiptotals$, invdtotflag$, valtotals$,    ~
                               frghttots$
            goto end_report


        print_report_sub_heading
            print using L60160         /* Customer sub-header line 1 */
            print using L60210         /* Customer sub-header line 2 */
            print using L60260         /* Customer sub-header line 3 */
            return
        end_report                /* Report Ending Routine */
            print skip(2)
            time$ = " " : call "TIME" (time$)
            print using L64990, time$     /* End of report line */
            close printer
            call "SETPRNT" (rptid$ , " ", 0%, 1%)
            call "FILEBGON" (#40) /* Close and Zap Work File */
            goto inputmode

        page_head                 /* Page Heading Print Routine */
            pcntr% = pcntr% + 1%
L34030:     print page        /* Top of Form */
            print using L60070, date$, time$, company$, "SHPACTRP", rptid$
            print using L60110, rpttitle$, pcntr%
            print
            if pcntr% <> 0% then goto L34070
                gosub print_params
                goto L34030
L34070:     lcntr% = 6%
            gosub print_report_sub_heading
            return

        print_params              /* Print Page Zero */
L34520:     i% = pos(str(i$()) > hex(7f))
            if i% = 0% then L34550
                str(i$(), i%, 1%) = hex(20)
                goto L34520
L34550:     print skip(3)
            print tab(26);
            print "------------------------- Report Selection Parameters"~
                  &  "--------------------------"
            print
            for x% = 6% to 17% : print tab(26); i$(x%) : next x%
            print tab(26);
            print "-----------------------------------------------------"~
                  &  "---------------------------"
            pcntr% = pcntr% + 1%
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35385: FMT                 /* FILE: ARIMASTR                          */~
            CH(9),          /* Customer Code                           */~
            CH(8),          /* Invoice Number                          */~
            POS(34),                                                     ~
            CH(16),         /* sales order number                      */~
            CH(3),          /* Bill of Lading Number                   */~
            POS(413),                                                    ~
            CH(6),          /* Ship Date                               */~
            POS(459),                                                    ~
            CH(6),          /* Shipping Carrier Code                   */~
            POS(521),                                                    ~
            CH(6),          /* Invoice Date                            */~
            POS(793),                                                    ~
            PD(14,4),       /* Gross Invoice Amount                    */~
            PD(14,4),       /* Discount Percent                        */~
            PD(14,4),       /* Discount Amount                         */~
            PD(14,4),       /* Freight Amount                          */~
            POS(870),                                                    ~
            CH(3),          /* Store or Warehouse Code                 */~
            POS(891),                                                    ~
            CH(1)           /* Invoice Type                            */

L36350: FMT                 /* FILE: SHPHDRS                           */~
            CH(9),          /* Customer Ship-to Address Identifier     */~
            CH(16),         /* sales order number                      */~
            CH(3),          /* Bill of Lading Number                   */~
            CH(3),          /* Warehouse or Store                      */~
            POS(38),                                                     ~
            CH(6),          /* Shipping Carrier Code                   */~
            POS(184),                                                    ~
            CH(6),          /* Ship Date                               */~
            POS(226),                                                    ~
            PD(14,4),       /* freight amount                          */~
            CH(8)           /* INVOICE NUMBER                          */

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
              on fieldnr% gosub L40090,         /* Date Range:       */   ~
                                L40090,         /* Customer Range:   */   ~
                                L40090,         /* Carrier Range:    */   ~
                                L40090,         /* Part Detail Flag  */   ~
                                L40090,         /* PreInvoice Flag   */   ~
                                L40090          /* Inventory type    */
              goto L40105

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40090:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40105:     accept                                                       ~
               at (01,02),                                               ~
                  "Shipping Activity Report",                            ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,30), fac(hex(a4)),   columnttl$           , ch(51),~
                                                                         ~
               at (07,02), "Date Range:",                                ~
               at (07,32), fac(lfac$( 1)), fmshipdate$          , ch(10),~
               at (07,58), fac(lfac$( 1)), toshipdate$          , ch(10),~
                                                                         ~
               at (08,02), "Customer Range:",                            ~
               at (08,32), fac(lfac$( 2)), fmcustomerrange$     , ch(09),~
               at (08,58), fac(lfac$( 2)), tocustomerrange$     , ch(09),~
                                                                         ~
               at (09,02), "Carrier Range:",                             ~
               at (09,32), fac(lfac$( 3)), fmcarrierrange$      , ch(06),~
               at (09,58), fac(lfac$( 3)), tocarrierrange$      , ch(06),~
                                                                         ~
               at (10,02), "Include Part Detail:",                       ~
               at (10,32), fac(lfac$( 4)), partdetailflag$      , ch(01),~
                                                                         ~
               at (11,02), "Include Pre-Invoice Shipment:",              ~
               at (11,32), fac(lfac$( 5)), preinvoiceflag$      , ch(01),~
                                                                         ~
               at (12,02), "Select Invoice Report Code: ",               ~
               at (12,34), "MANUAL    SALES ORDER     ADJUST    CREDIT ",~
               at (13,34), "DIRECT    EXPORT ORDER    GENERATED        ",~
               at (12,32), fac(lfac$( 6)), invreport$( 1)       , ch(01),~
               at (12,42), fac(lfac$( 6)), invreport$( 2)       , ch(01),~
               at (12,58), fac(lfac$( 6)), invreport$( 3)       , ch(01),~
               at (12,68), fac(lfac$( 6)), invreport$( 4)       , ch(01),~
               at (13,32), fac(lfac$( 6)), invreport$( 5)       , ch(01),~
               at (13,42), fac(lfac$( 6)), invreport$( 6)       , ch(01),~
               at (13,58), fac(lfac$( 6)), invreport$( 7)       , ch(01),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13 then L40610
                  call "MANUAL" ("SHPACTRP") : goto L40105

L40610:        if keyhit% <> 15 then L40640
                  call "PRNTSCRN" : goto L40105

L40640:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40830     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40800
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
            if fieldnr% > 1% then L40810
L40800:         str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40810:     return

L40830: if fieldnr% > 0% then L40920  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40920:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50100,         /* Date Range:            */~
                              L50200,         /* Customer Range:        */~
                              L50300,         /* Carrier Range:         */~
                              L50400,         /* Part Detail Flag       */~
                              L50500,         /* Pre-Invoice Flag       */~
                              L50600          /* Inventory Report Code  */

            return
L50100: REM Test for Date Range                   FMSHIPDATE$

            if fmshipdate$ <> "ALL"    then L50150
                loshipdate$ = "19001010"
                call "DATFMTC"  (loshipdate$)
                hishipdate$ = "20991231"
                call "DATFMTC"  (hishipdate$)
                toshipdate$ = " "
                goto L50178
L50150:     call "DATEOKC" (fmshipdate$, fmshipdate%, errormsg$)
            if errormsg$ <> " " then  return
            if toshipdate$ = " " or toshipdate$ = blankdate$ then ~
               toshipdate$ = fmshipdate$
            call "DATEOKC" (toshipdate$, toshipdate%, errormsg$)
            if errormsg$ <> " " then return

            loshipdate$ = fmshipdate$
            hishipdate$ = toshipdate$
L50178:     call "DATUFMTC"  (loshipdate$)
            call "DATUFMTC"  (hishipdate$)
            if loshipdate$ > hishipdate$  then                           ~
                errormsg$ = "TO MUST BE EQUAL TO OR GREATER THAN FROM"   ~
                else  errormsg$ = " "
            return

            fmshipdate% = toshipdate%      /* Do nothing line */

L50200: REM Test for Customer Range               FMCUSTOMERRANGE$

            if fmcustomerrange$ = " " then fmcusflag$ = " "              ~
                else   fmcusflag$ = "N"
            if fmcustomerrange$ = "ALL" then L50231
            if fmcustomerrange$ = "FIRST" then L50235
               temp$ = hex(0686) & "SELECT FROM CUSTOMER"
               call "GETCODE" (#4, fmcustomerrange$, temp$, 0%, 0, f1%(4))

            if tocustomerrange$ <> " " then L50235
                if fmcusflag$ = " " then L50237
L50231:         tocustomerrange$ = fmcustomerrange$
                goto L50243
L50235:     if tocustomerrange$ = "LAST" then L50243
L50237:         temp$ = hex(0686) & "SELECT TO CUSTOMER"
                call "GETCODE" (#4, tocustomerrange$, temp$, 0%, 0,f1%(4))

L50243:     call "TESTRNGE"                                              ~
                  (fmcustomerrange$    , tocustomerrange$    ,           ~
                   locustomerrange$    , hicustomerrange$    ,           ~
                   errormsg$)
            if fmcustomerrange$ = "FIRST" then fmcustomerrange$ =        ~
                                               all(hex(00))
            return

L50300: REM Test for Carrier Range                FMCARRIERRANGE$
            call "TESTRNGE"                                              ~
                  (fmcarrierrange$     , tocarrierrange$     ,           ~
                   locarrierrange$     , hicarrierrange$     ,           ~
                   errormsg$)
            return

L50400: REM Test for Include Part Detail?           PARTDETAILFLAG$
             if partdetailflag$ <> "Y" and partdetailflag$ <> "N"        ~
                 then errormsg$ = "'Y' OR 'N', PLEASE."
            return

L50500: REM Test for Pre Invoiced Flag?           PREINVOICEFLAG$
             if preinvoiceflag$ <> "Y" and preinvoiceflag$ <> "N"        ~
                 then errormsg$ = "'Y' OR 'N', PLEASE."
            return

L50600: REM Test for Inventory Report Code        INVREPORT$()
            if str(invreport$()) = " " and preinvoiceflag$ = "N" then    ~
              errormsg$ = "PREINVOICE OR AN INVOICE TYPE MUST BE SELECTED"
            return

        REM *************************************************************~
            *              I M A G E   S T A T E M E N T S              *~
            *-----------------------------------------------------------*~
            * Image Statements for report print lines.                  *~
            *************************************************************

*       * Header Line 1
L60070: %RUN ########   ########              ###########################~
        ~#################################                 ########:######

*       * Header Line 2
L60110: %                                     ###########################~
        ~#################################                     PAGE: ####


*        Report sub-header line 1
L60160: %CUSTOMER  CUSTOMER                       SHIPMENT SALES ORDER   ~
        ~            INVOICE          INVOICE CARRIER   SHIPMENT    FREIGH~
        ~T

*        Report sub-header line 2
L60210: %CODE      DESCRIPTION                    DATE     NUMBER        ~
        ~    BOL STR NUMBER           TYPE    CODE        AMOUNT    CHARGE~
        ~S

*        Report sub-header line 3
L60260: %--------- ------------------------------ -------- --------------~
        ~--- --- --- ---------------- ------- ------- ---------- ---------~
        ~-

*        New Customer line
L60310: %######### ############################## ######## ##############~
        ~### ### ### ################ ####### ####### ########## #########~
        ~#

*        New Shipment line
L60370: %                                         ######## ##############~
        ~### ### ### ################ ####### ####### ########## #########~
        ~#

*        New Sales Order Number
L60430: %                                                  ##############~
        ~### ### ### ################ ####### ####### ########## #########~
        ~#

*        New BOL line
L60474: %                                                                ~
        ~    ### ### ################ ####### ####### ########## #########~
        ~#

*        Customer Totals
L60510: %      ** #########  NUMBER OF SHIPMENTS ####    VALUE SHIPPED # ~
        ~############  FREIGHT CHARGES ##########

*        Grand Totals
L60550: %      ***   TOTAL   NUMBER OF SHIPMENTS ####    VALUE SHIPPED # ~
        ~############  FREIGHT CHARGES ##########

*        Part Number
L60590: %            PART CODE: ######################### DESCR: ########~
        ~######################## QTY: #########   UOM: ####

L60620: %        ################### #########################  #########~
        ~#########################

L64990: %                                  * * * * * * * *   E N D   O F ~
        ~  R E P O R T   @ ########   * * * * * * * * * *

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
            *  Copyright (c) 1991  an unpublished work by CAELUS,       *~
            *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
            CAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUS***

        exit_program
            call "SHOSTAT" ("Closing Files, One Moment Please")

            end
