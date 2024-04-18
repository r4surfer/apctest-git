        REM THISPROGRAMWASGENERATEDUSINGTHEGENRPPGMPROGRAMWHICHISAPROPRIE~
            *                                                           *~
            *  JJJJJ  BBBB   V   V   SSS    AAA    SSS   U   U  BBBB    *~
            *    J    B   B  V   V  S      A   A  S      U   U  B   B   *~
            *    J    BBBB   V   V   SSS   AAAAA   SSS   U   U  BBBB    *~
            *  J J    B   B   V V       S  A   A      S  U   U  B   B   *~
            *   J     BBBB     V     SSS   A   A   SSS    UUU   BBBB    *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * JBVSASUB - Performs Job related management of the VBKVSA  *~
            *            file.                                          *~
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
            * 06/17/94 ! Original                                 ! ERN *~
            * 08/26/95 ! Corrected inability to loop and get more ! JDH *~
            *          !  than one consecutive VSA. Thanks Ricky. !     *~
            PRODUCTOFCAELUSINCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERV**

        sub "JBVSASUB" (  #2,            /* SYSFILE2                   */~
                          #4,            /* WCOUT                      */~
                          mode$,         /* ADD, RESet, DELete         */~
                          job_nbr$,      /* Job Number                 */~
                          job_part$,     /* Job's part to make         */~
                          job_qty)       /* Total Qty to Make          */

*        JOB_QTY serves as DELete mode flag -- 0 indicating that ALL
*        records are to be deleted, <> 0 that only Open records with
*        Auto Purge = "Y"

*        NOTE: VBKVSA records with VSAOUTIN records are NOT deleted by
*              this routine.

        dim                                                              ~
            act_code$4, act_code2$4,     /* Activity Code              */~
            advice$8,                    /* Advice Number              */~
            auto_adj_flag$1,             /* Auto Adj Flag              */~
            buyer$3,                     /* Buyer Class Code           */~
            comment$40,                  /* Comment                    */~
            contract$20,                 /* Contract ID and Line       */~
            job_nbr$8,                   /* Job Number                 */~
            job_part$25,                 /* Job Part                   */~
            mode$3,                      /* Calling Mode               */~
            plowkey$99,                  /* Miscellaneous Read/Plow Key*/~
            po$16,                       /* PO Number                  */~
            po_line$3,                   /* PO Line Number             */~
            readkey$99,                  /* All purpose Read key       */~
            rte_step$4, rte_step2$4,     /* Route Step                 */~
            status$1,                    /* VSA Status (O/R/P)         */~
            uom$4,                       /* Unit of Measure Code       */~
            vendor$9,                    /* Vendor Code                */~
            vendor_part$25,              /* Vendor Part Number         */~
            wc$4, wc2$4,                 /* Work Center                */~
            wc_start_date$8,             /* Work Center Start Date     */~
            wc_end_date$8,               /* Work Center End Date       */~
            wcout_key$30, wcout_key2$30  /* WCOUT Key Variables        */

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
            cms2v$ = "R6.04.02 11/13/95 Precious Metals                 "
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
            * #01 ! VBKVSA   ! Vendor Service Advices                   *~
            * #02 ! SYSFILE2 ! Caelus Management System Information     *~
            * #03 ! HNYACTXF ! HNY, WC Activity Cross Reference         *~
            * #04 ! WCOUT    ! Planned work center use detail rec       *~
            * #05 ! VBKOUTIN ! VSA Shipment Log                         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "VBKVSA",                                        ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos = 5,    keylen = 8,                       ~
                        alt key  1, keypos =    1, keylen =  12,         ~
                            key  2, keypos =    2, keylen =  11,         ~
                            key  3, keypos =   13, keylen =  12, dup,    ~
                            key  4, keypos =   29, keylen =   6, dup,    ~
                            key  5, keypos =   41, keylen =  13, dup,    ~
                            key  6, keypos =   50, keylen =   4, dup

            select #03, "HNYACTXF",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =    1, keylen =  29,                     ~
                        alt key  1, keypos =   26, keylen =   4          ~

            select #05, "VSAOUTIN",                                      ~
                        varc, indexed, recsize =  316,                   ~
                        keypos =   1, keylen =  26,                      ~
                        alt key    1, keypos =  44, keylen =  8, dup,    ~
                            key    2, keypos =  52, keylen = 23, dup

*       ** Files opened below in Initialization section.


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

*        First, see if we're supposed to generate VSA's.
            call "VBKSWTCH" ("VSA GEN  ", temp$, temp, ret%)
            if temp$ <> "Y" then exit_program else temp = temp

*        Get the base planning calendar date...
            call "READ100" (#2, "MONTHS OPEN", f1%(2))
            if f1%(2) = 0% then end
            get #2 using L09150, day1$
L09150:         FMT XX(32), CH(6)

*        OK, open a couple of files.
            call "OPENCHCK" (#01, fs%(01), f2%(01), 1%, rslt$(01))
            call "OPENCHCK" (#03, fs%(03), f2%(03), 0%, rslt$(03))
            call "OPENCHCK" (#05, fs%(05), f2%(05), 0%, rslt$(05))

            if f2%(1) = 0% and f2%(3) = 0% then L09280
                call "ASKUSER" (0%, "** JBVSASUB: OPEN FILE ERROR **",   ~
                        "Unable to open VBKVSA and/or HNYACTXF file(s).",~
                        "Am, therefore unable to maintain VSAs.",        ~
                        "Press any PF-Key to continue...")
                goto exit_program

L09280
*        Set up some variables.  Well, one anyhow.
            del_qty = job_qty


        REM *************************************************************~
            *                     M A I N                               *~
            * --------------------------------------------------------- *~
            * Do what which we were asked to do.                        *~
            *************************************************************~

            if mode$ = "ADD" then gosub add_mode

            if mode$ = "RES" then gosub reset_mode

            if mode$ = "DEL" then gosub delete_mode

            goto exit_program


*       ---------------------------------------------------------------*
        add_mode
*       ---------------------------------------------------------------*
*        Add VSAs for the first time or as part of a reset.
*        Caller needs to be careful -- this routine does not do any
*        cleanup work.  If there is suspicion that advices may have
*        already been placed for the Job then the RESet function
*        should be used.

            if job_qty = 0 then return

            wcout_key$ = "JOB ORDER: " & str(job_nbr$) & hex(00000000)

        wcout_loop
            call "PLOWNEXT" (#4, wcout_key$, 19%, f1%(4))
            if f1%(4) = 0% then return

            get #4 using L10330, wc$, wc_date%, rte_step$, act_code$
L10330:         FMT CH(4), BI(2), POS(40), CH(4), POS(48), CH(4)
            if wc$ <> "VEND" or act_code$ = " " then wcout_loop

*        We got one.  Make sure there isn't one already in VBKVSA.
            readkey$ = str (job_nbr$,,8) & rte_step$
            call "REDALT0" (#1, readkey$, 3%, f1%(1))
            if f1%(1) = 1% then wcout_loop

*        Try to read in HNYACTXF record.
            readkey$ = str(job_part$,,25) & act_code$
            call "READ100" (#3, readkey$, f1%(3))
            if f1%(3) = 0% then wcout_loop         /* We tried */

            get #3 using L10490, buyer$, uom$, cnv_factor, unit_price,    ~
                                bucket%, vendor$, vendor_part$, contract$
L10490:         FMT XX(29), CH(3), CH(4), 2*PD(14,7), BI(4), CH(9),      ~
                    POS(270), CH(25), CH(20)

*        OK fine.  Concoct a VBKVSA record for this guy.
            status$  = "O"

            gosub get_next_advice_number

            wc_date% = wc_date% - 1%
            call "DATE" addr("G+", day1$, wc_date%, wc_start_date$, ret%)
            if ret% <> 0% then wc_start_date$ = date

            wc_date% = wc_date% + 1%
            gosub get_wc_end_date
            wc_date% = wc_date% - 1%
            call "DATE" addr("G+", day1$, wc_date%, wc_end_date$, ret%)
            if ret% <> 0% then wc_end_date$ = wc_start_date$

            auto_adj_flag$ = "Y"

            qty = round(job_qty * cnv_factor, 4)

            if bucket% < 1% or bucket% > 12% then bucket% = 1%

            write #1 using L35030, status$, buyer$, advice$, job_nbr$,    ~
                     rte_step$, wc$, wc_start_date$, wc_end_date$,       ~
                     vendor$, act_code$, contract$, po$, po_line$,       ~
                     auto_adj_flag$, comment$, qty, uom$, unit_price,    ~
                     bucket%, vendor_part$, " "
            goto wcout_loop


        get_wc_end_date
            wcout_key2$ = wcout_key$
          get_wc_end_date_loop
            call "PLOWNEXT" (#4, wcout_key2$, 19%, f1%(4))
            if f1%(4) = 0% then return

            get #4 using L10870, wc2$, wc_date2%, rte_step2$, act_code2$
L10870:         FMT CH(4), BI(2), POS(40), CH(4), POS(48), CH(4)
            if wc2$       = wc$       and rte_step2$ = rte_step$ and     ~
               act_code2$ = act_code$ and wc_date2%  > wc_date%     then ~
                     wc_date% = wc_date2%
            goto get_wc_end_date_loop


        get_next_advice_number
            readkey$ = "NEXT VSA NUMBER"
            call "READ101" (#2, readkey$, f1%(2))
            if f1%(2) = 0% then                                          ~
                advice% = 1%                                             ~
            else                                                         ~
                get #2 using L11020, advice%
L11020:              FMT XX(20), BI(4)
            convert advice% to advice$, pic(00000000)
            advice% = advice% + 1%
            put #2 using L11060, readkey$, advice%, " "
L11060:         FMT CH(20), BI(4), CH(250), CH(226)
            if f1%(2) = 0% then write #2 else rewrite #2
            return


*       ---------------------------------------------------------------*
        reset_mode
*       ---------------------------------------------------------------*
*        First DELete all open VSAs and then ADD them back in.

            gosub delete_mode

            gosub add_mode

            return


*       ---------------------------------------------------------------*
        delete_mode
*       ---------------------------------------------------------------*
*        2 ways to go here.  If qty = 0 then we kill everyone regardless
*        of how loud they scream.  If qty is not 0 then we just kill the
*        open guys.

            plowkey$ = str(job_nbr$,,8) & hex(000000)

        delete_loop
            call "PLOWAL1" (#1, plowkey$, 3%, 8%, f1%(1))
            if f1%(1) = 0% then return
            get #1 using L12250, status$, advice$, auto_adj_flag$
L12250:         FMT CH(1), XX(3), CH(8), POS(93), CH(1)
            call "REDALT0" (#5, advice$, 1%, f1%(5%))
            if f1%(5) = 1% then delete_loop
                if del_qty = 0 then delete #1
                if del_qty = 0 then delete_loop
                     if auto_adj_flag$ = "N" then delete_loop
                          if status$ = "O" then delete #1
                          goto delete_loop


        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: VBKVSA                            */~
            CH(1),          /* Status Indicator                        */~
            CH(3),          /* Buyer/planner code                      */~
            CH(8),          /* Advice Number                           */~
            CH(8),          /* Job Number                              */~
            CH(4),          /* RTE Step To Identify A Route Line       */~
            CH(4),          /* Work Center ID                          */~
            CH(06),         /* WC Start Date                           */~
            CH(06),         /* WC End Date                             */~
            CH(9),          /* Purchasing Code for Supplier/Manufacture*/~
            CH(04),         /* Activity to be performed                */~
            CH(20),         /* Purchasing Contract ID                  */~
            CH(16),         /* Purchase Order Number                   */~
            CH(03),         /* Purchase Order Line Number              */~
            CH(1),          /* Auto Adjust Flag                        */~
            CH(40),         /* Comment                                 */~
            PD(14,4),       /* Quantity to Buy                         */~
            CH(4),          /* Unit of Measure                         */~
            PD(14,7),       /* Unit Price                              */~
            BI(4),          /* Standard Cost Buckets                   */~
            CH(25),         /* Vendor Part Number                      */~
            CH(118)         /* Currency code & Filler                  */~

        FMT                 /* FILE: HNYACTXF                          */~
            CH(25),         /* Part Number (Product Code)              */~
            CH(4),          /* Activity to be performed                */~
            CH(3),          /* Buyer Class Code                        */~
            CH(4),          /* Unit of Measure                         */~
            PD(14,7),       /* Conversion Factor                       */~
            PD(14,7),       /* Unit Price                              */~
            BI(4),          /* Cost Bucket                             */~
            CH(9),          /* Vendor Code                             */~
            CH(200),        /* Variable Fields Data Area               */~
            CH(4),          /* Internal ID to text in TXTFILE          */~
            CH(25),         /* Vendor Part Number                      */~
            CH(218)         /* Unused filler area in record (reserved b*/~

        FMT                 /* FILE: WCOUT                             */~
            CH(4),          /* Work Center code                        */~
            BI(2),          /* Date out of PIP in date subscript form f*/~
            BI(2),          /* Sequence Number                         */~
            CH(19),         /* Tag number in level 2 planning          */~
            BI(2),          /* Date out of PIP in date subscript form f*/~
            BI(2),          /* Sequence Number                         */~
            BI(4),          /* Set Up time in WC units                 */~
            BI(4),          /* Run time in hours per part              */~
            CH(4),          /* RTE Step To Identify A Route Line       */~
            BI(1),          /* Phantom counter for route steps         */~
            BI(2),          /* alternate part seq. no.                 */~
            BI(1),          /* Concurrent Workcenter Seq. relative to p*/~
            CH(04),         /* Activity to be performed                */~
            CH(17)          /* Unused filler area in record (reserved b*/~

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
