        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   SSS   H   H  PPPP    SSS    CCC   X   X  RRRR   PPPP    *~
            *  S      H   H  P   P  S      C   C   X X   R   R  P   P   *~
            *   SSS   HHHHH  PPPP    SSS   C        X    RRRR   PPPP    *~
            *      S  H   H  P          S  C   C   X X   R R    P       *~
            *   SSS   H   H  P       SSS    CCC   X   X  R  R   P       *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * SHPSCXRP - (Clone of SHPSCHRP)    Prints a listing of     *~
            *            of scheduled shipments for EXPORT only.        *~
            *            Records are selected from the SHPHDRS file,    *~
            *            sorted by store/date/carrier, and then         *~
            *            printed.                                       *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/10/86 ! Original                                 ! JRH *~
            * 04/16/87 ! Corrected SLCTSORT key length param      ! JRH *~
            * 11/02/87 ! Export Invoicing Version                 ! MJB *~
            * 09/22/89 ! Changed Report ID & Address length.      ! JDH *~
            * 09/19/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            actl_ship$6,                 /* Date actually shipped      */~
            appr$3,                      /* Approved literal           */~
            bckmastr_key$25,             /* Key to BCKMASTR file       */~
            bill_lade$3,                 /* Bill of lading number      */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            brk_carr$6,                  /* Carrier code for break     */~
            brk_cust$9,                  /* Customer code for break    */~
            brk_date$6,                  /* Date for break             */~
            brk_stor$3,                  /* Store code for break       */~
            carr_code$6,                 /* Carrier code               */~
            carr_name$30,                /* Carrier name               */~
            company_name$60,             /* Company name from COMPNAME */~
            cust_code$9,                 /* Customer code from SHPHDRS */~
            cust_dest$30,                /* Customer city from BCKMASTR*/~
            cust_name$30,                /* Customer name from BCKMASTR*/~
            dat8$8,                      /* Edited date for print      */~
            date$8,                      /* Date for screen display    */~
            eodsw$1,                     /* End of data switch         */~
            errormsg$79,                 /* Error message              */~
            expappr$1,                   /* Export Approval Flag       */~
            export$1,                    /* Export Order flag          */~
            gencodes_key$24,             /* Key to GENCODES file       */~
            hdr$40,                      /* ASKUSER constant           */~
            invoicenr$8,                 /* Invoice # from SHPHDRS     */~
            msg$(3)80,                   /* ASKUSER messages           */~
            one_time$1,                  /* Controls 1-time print logic*/~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            rptid$6,                     /* Report ID                  */~
            ship_date$6,                 /* Shipping date              */~
            sord_nbr$16,                 /* Sales order number         */~
            sortkey$44,                  /* Key for sorting WORKFILE   */~
            stor_code$3,                 /* Store code                 */~
            stor_name$32,                /* Store name                 */~
            time$8,                      /* Time of day stamp          */~
            userid$3                     /* Current User Id            */

        dim f2%(32),                     /* = 0 if the file is open    */~
            f1%(32),                     /* = 1 if READ was successful */~
            fs%(32),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32)20                  /* Text from file opening     */

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
            * #3  ! SHPHDRS  ! Shipment Scheduling / Pre-Invoicing- Hea *~
            * #5  ! BCKMASTR ! BACKLOG MASTER FILE (GET STORE NUMBER)   *~
            * #6  ! GENCODES ! GENERAL CODES FILE                       *~
            * #7  ! STORNAME ! STORE INFORMATION FILE                   *~
            * #9  ! WORKFILE ! Temporary System Workfile                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #3,  "SHPHDRS",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  28                      ~

            select #5,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup     ~

            select #6,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =  24                      ~

            select #7,  "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3                      ~

            select #9,  "WORKFILE", consec, recsize = 44

            call "SHOSTAT" ("Opening Files, One Moment Please")

                rslt$(3 ) = "REQUIRED"
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
            get rslt$(3) using L02390, shp_recs%
L02390:         FMT POS(17), BI(4)

                rslt$(5 ) = "REQUIRED"
            call "OPENCHCK" (#5,  fs%(5 ), f2%(5 ), 0%, rslt$(5 ))
                rslt$(6 ) = "REQUIRED"
            call "OPENCHCK" (#6,  fs%(6 ), f2%(6 ), 0%, rslt$(6 ))
                rslt$(7 ) = "REQUIRED"
            call "OPENCHCK" (#7,  fs%(7 ), f2%(7 ), 0%, rslt$(7 ))

            if min(fs%()) < 0% then exit_program

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

L09060:     u3% = 2%
            call "ASKUSER" (u3%, "* EXPORT SHIPPING SCHEDULE REPORT *",  ~
                            "Press RETURN to continue with this report", ~
                            "-- OR --",                                  ~
                            "Press PF(16) to cancel and return to menu")
            if u3% = 16% then goto exit_program
            if u3% <> 0% then goto L09060

            if shp_recs% > 0% then L09220
L09150:         retn% = 2%
L09160:         hdr$ = "*** CANCEL REQUEST ***"
                msg$(1) = "There is no EXPORT shipping data to analyze"
                msg$(3) = "Press RETURN to cancel program"
                call "ASKUSER" (retn%, hdr$, msg$(1), msg$(2), msg$(3))
                if retn% <> 0% then L09150
                goto exit_program /* NOTHING TO DO; DON'T DO ANYTHING */
L09220:     call "COMPNAME" (12%, company_name$, retn%)
            rptid$ = "SHP008"
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

            call "SHOSTAT"                                               ~
                ("Records are being selected ... Please stand by")
            plowkey$ = all(hex(00))
            shp_recs% = max(50%, (shp_recs% * .75))
            nbr_recs% = 0%   /* Initialize count of records selected */
            call "WORKOPN2" (#9, "OUTPT", shp_recs%, f2%(9))

        plow_thru_shphdrs
            call "PLOWNEXT" (#3, plowkey$, 0%, f1%(3))
            if f1%(3) = 0% then sort_work_file

            get #3 using L13220, cust_code$, sord_nbr$, bill_lade$,       ~
                stor_code$, ship_date$, carr_code$, actl_ship$,          ~
                invoicenr$, export$, expappr$

        REM Record layout of the 'SHPHDRS' file *****
L13220:         FMT CH(9),    /* KEY */ /* Customer Code (Ship to)     */~
                    CH(16),   /* KEY */ /* Sales Order Number          */~
                    CH(3),    /* KEY */ /* Bill of lading number       */~
                    CH(3),              /* Store number                */~
                    CH(6),              /* Scheduled ship date         */~
                    CH(6),              /* Carrier code                */~
                    XX(20),             /* How ship                    */~
                    XX(20),             /* FOB                         */~
                    XX(100),            /* Shipping inst (2*CH(50))    */~
                    CH(6),              /* Date actually shipped       */~
                    XX(20),             /* Freight bill number         */~
                    XX(8),              /* Cartons (PD(14,4))          */~
                    XX(8),              /* Shipping weight (PD(14,4))  */~
                    XX(8),              /* Freight amount (PD(14,4))   */~
                    CH(8),              /* Invoice number              */~
                    XX(4),              /* Text ID                     */~
                    CH(1),              /* Export Order flag           */~
                    CH(1),              /* Export Approval Flag        */~
                    XX(53)              /* Filler                      */

            if actl_ship$ <> " " and actl_ship$ <> blankdate$ then ~
                                      plow_thru_shphdrs  /* Eliminate */
            if invoicenr$ <> " " then plow_thru_shphdrs  /*  Shipped  */
            if export$ <> "Y" then plow_thru_shphdrs  /* and Domestic */

        REM BUILD THE SORT KEY ******************************************
            str(sortkey$,  1,  3) = stor_code$
            str(sortkey$,  4,  6) = ship_date$
            str(sortkey$, 10,  6) = carr_code$
            str(sortkey$, 16,  9) = cust_code$
            str(sortkey$, 25, 16) = sord_nbr$
            str(sortkey$, 41,  3) = bill_lade$
            str(sortkey$, 44,  1) = expappr$

        REM OUTPUT TO THE WORKFILE **************************************
            write #9 using L13510, sortkey$
L13510:         FMT CH(44)
            nbr_recs% = nbr_recs% + 1%
            goto plow_thru_shphdrs

        sort_work_file
            if nbr_recs% = 0% then L09160
            call "SLCTSORT" (#9, len(str(sortkey$)))

        REM PRINT/DISPLAY LOGIC BEGINS HERE ****************************
            one_time$, eodsw$ = "0"
            call "SHOSTAT" ("EXPORT Shipping Schedule Report in Progress")
            page_nbr% = 0% : line_nbr% = 99%
            init (" ") sortkey$
            select printer(134)
            call "SETPRNT" (rptid$, " ", 0%, 0%)
            call "TIME" (time$)

        read_sorted_workfile
            read #9 using L13510, sortkey$, eod goto end_of_report
            get sortkey$ using L13720, stor_code$, ship_date$, carr_code$,~
                cust_code$, sord_nbr$, bill_lade$, expappr$
L13720:              FMT CH(3), CH(6), CH(6), CH(9), CH(16), CH(3), CH(1)
            if one_time$ = "1" then data_print_setup
                gosub cust_reset
                gosub carr_reset
                gosub date_reset
                gosub store_reset
                one_time$ = "1"

        data_print_setup
            if stor_code$ <> brk_stor$ then gosub store_total
            if ship_date$ <> brk_date$ then gosub date_total
            if carr_code$ <> brk_carr$ then gosub carrier_total
            if cust_code$ <> brk_cust$ then gosub customer_total
            bckmastr_key$ = str(cust_code$,,9) & sord_nbr$
            cust_dest$ = " "
            cust_name$ = "Customer/Sales Order not found"
            call "READ100" (#5, bckmastr_key$, f1%(5))
            if f1%(5) = 0% then print_it
                get #5 using L13910, cust_name$, cust_dest$
L13910:              FMT POS(42), CH(30), XX(120), CH(30)
        print_it
            appr$ = " "  :  if expappr$ = "Y" then appr$ = "YES"
            if line_nbr% > 56% then gosub page_heading
            print using L61350, dat8$, appr$, carr_name$, cust_code$,     ~
                cust_name$, cust_dest$, str(sord_nbr$,,16) & "-" &       ~
                bill_lade$
            line_nbr% = line_nbr% + 1%
            goto read_sorted_workfile

        page_heading
            page_nbr% = page_nbr% + 1%  :  line_nbr% = 9%
            print page
            print using L60040, date$, time$, company_name$, "-" & rptid$
            print using L60070, page_nbr%
            print
            print using L60100
            print using L60120, "#"
            print using L60150  /*UNDERSCORES*/
            print
            print using L61700, stor_code$, stor_name$ : print
            return

        end_of_report
            eodsw$ = "1"
            if line_nbr% > 56 then gosub page_heading
            gosub store_total
            print using L62750 /* END OF REPORT */
            close printer
            call "SETPRNT" (rptid$, " ", 0%, 1%)

        REM SCRATCH THE WORK FILE HOLDING CURRENT LINE ITEMS ************
            close #9
            call "FILEBGON" (#9)
            goto exit_program

        store_total
            print using L62400 /* UNDERSCORES */
            gosub date_total_no_underscore
        store_reset
            if eodsw$ = "1" then return
            brk_stor$ = stor_code$
            call "DESCRIBE" (#7, stor_code$, stor_name$, 1%, f1%(7))
            if f1%(7) = 0% then stor_name$ = "(Unknown Store Code)"
            gosub page_heading
            return

        date_total
            print using L62510 /* UNDERSCORES */
            line_nbr% = line_nbr% + 1%
        date_total_no_underscore
            gosub carr_total_no_underscore
        date_reset
            if eodsw$ = "1" then return
            dat8$, brk_date$ = ship_date$
            call "DATEOK" (dat8$, retn%, errormsg$)
            return

        carrier_total
            print using L62540 /* UNDERSCORES */
            line_nbr% = line_nbr% + 1%

        carr_total_no_underscore
            gosub cust_total_no_underscore

        carr_reset
            if eodsw$ = "1" then return
            brk_carr$ = carr_code$
            gencodes_key$ = str("CARRIERS ",,9) & carr_code$
            call "DESCRIBE" (#6, gencodes_key$, carr_name$, 0%, f1%(6))
            if f1%(6) = 0% then carr_name$ = "Unknown Carrier Code"
            return

        customer_total
        cust_total_no_underscore
            if line_nbr% > 56 then gosub page_heading
            print
            line_nbr% = line_nbr% + 1%
        cust_reset
            if eodsw$ = "1" then return
            brk_cust$ = cust_code$
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            *             I M A G E   S T A T E M E N T S               *~
            *************************************************************

L60040: %RUN DATE: ######## @ ########      #############################~
        ~###############################                    SHPSCXRP######~
        ~#
L60070: %                                                 EXPORT SHIPPING~
        ~ SHEDULE REPORT                                         PAGE: ###~
        ~#
L60100: %SCHEDULED                                SHIP TO   SHIP TO      ~
        ~                  SHIP TO
L60120: %SHIP DATE APR CARRIER NAME (SHORT)      CUSTOMER  CUSTOMER NAME ~
        ~                 CUSTOMER LOCATION              SALES ORDER #   -~
        ~BOL
L60150: %--------- --- ------------------------- --------- --------------~
        ~---------------- ------------------------------ -----------------~
        ~---
L61350: %########  ### ######################### ######### ##############~
        ~################ ############################## #################~
        ~###
L61700: %     STORE: ### ################################
L62400: %----------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~---
L62510: %          ------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~---
L62540: %                                         -----------------------~
        ~-----------------------------------------------------------------~
        ~---
L62750: %                                                        ** END O~
        ~F REPORT **

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
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
