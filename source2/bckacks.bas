        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *  BBBB    CCC   K   K   AAA    CCC   K   K   SSS           *~
            *  B   B  C   C  K  K   A   A  C   C  K  K   S              *~
            *  BBBB   C      KKK    AAAAA  C      KKK     SSS           *~
            *  B   B  C   C  K  K   A   A  C   C  K  K       S          *~
            *  BBBB    CCC   K   K  A   A   CCC   K   K   SSS           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * BCKACKS  - Prints an acknowledgement for any order entered*~
            *            and/or modified after a given date.  Allows the*~
            *            operator to vary the report based on the date &*~
            *            Store Code ranges.                             *~
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
            * 09/16/86 ! Original                                 ! JIM *~
            * 05/13/87 ! HNYMASTR record length mod for Std Cost  ! JIM *~
            * 05/15/87 ! Added SO # Range For Print               ! HES *~
            * 09/17/87 ! Added ability to print options           ! HES *~
            * 08/03/88 ! Print currency & description on Ack.     ! JIM *~
            * 09/29/88 ! Price now not formatted via CURRFMT      ! JDH *~
            * 10/19/88 ! Extension now calc'ed on stocking price. ! JDH *~
            * 05/01/89 ! Formatted zip code w/dash.               ! JDH *~
            * 12/27/89 ! Added Option Auto-Replace code           ! MJB *~
            * 05/29/90 ! Tweaked zip code fmt, adj addresses.     ! JDH *~
            * 12/18/91 ! Modified to allow BOM Header Text as a   ! LDJ *~
            *          ! valid text source.  (Uses effective BOM  !     *~
            *          ! based on current due date if there is an !     *~
            *          ! effective bill, other wise uses 1st bill !     *~
            *          ! on file for part.                        !     *~
            * 11/06/92 ! PRR 12411- Fix probs re BOM ID text print! JIM *~
            * 11/09/92 ! PRR 12676- Compute Extension per SO Entry! JIM *~
            * 11/09/92 ! PRR 11620- Intelligent SO # range entry  ! JIM *~
            * 11/09/92 !   via a WORKFILE & PLOWCODE; Store range ! JIM *~
            * 11/09/92 !   entry now uses new TESTRNGE & STORNAME.! JIM *~
            * 01/19/94 ! Change the WORKFILE record size to       ! SID *~
            *          !   # of rec in file #3 divided by 4.      !     *~
            * 02/01/94 ! WORKFILE only created if necessary,      ! JDH *~
            *          !   because it's sloowww.                  !     *~
            * 03/08/94 ! Changed record length for BOMSPEC and    ! WPH *~
            *          !  removed HNYOPTN2 file (obsolete)        !     *~
            * 12/08/94 ! PRR 13323.  Price printed honoring flag  ! JDH *~
            *          !  pertaining to discounts.                !     *~
            * 12/30/94 ! Precious Metal Surcharge Message.        ! RJH *~
            * 03/10/95 ! PRR 13187 - Print Xref Parts.            ! RJH *~
            * 07/07/95 ! Added ability to print Customer Text.    ! JDH *~
            * 07/29/96 ! Changes for the year 2000.               ! DXL *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            a$1,                         /* BCKPRIDX record type       */~
            bcklines_key$19,             /* Key to BCKLINES            */~
            bckmastr_key$25,             /* Key to BCKMASTR            */~
            bill_to$(6)31,               /* Bill-to name from CUSTOMER */~
            billto_xref$9,               /* Bill-to xref from BCKMASTR */~
            bom$(490)3,                  /* Effective BOM's            */~
            bol$3,                       /* BCKPRIDX/WORKFILE BOL #    */~
            bomid$3,                     /* BOM Rev ID                 */~
            comp_pn$25,                  /* COMPONENT PART NUMBER      */~
            criteria_date$8,             /* Date entered by operator   */~
            currency$4, currdesc$32,     /* Currency Code              */~
            cursor%(2),                  /* Cursor location for edit   */~
            cus$9,                       /* BCKPRIDX/WORKFILE Customer */~
            cust_code$9,                 /* Customer code from BCKPRIDX*/~
            date$8,                      /* Date for display & print   */~
            desc$68,                     /* Descriptions for totals    */~
            discount$10,                 /* Edited line discount amount*/~
            discsw$1,                    /* Controls discount functions*/~
            dmap(10),                    /* PLOWCODE Display Map       */~
            due_curr$8,                  /* Original due date- BCKLINES*/~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            extension$10,                /* Edited line extension amt  */~
            fob$20,                      /* F.O.B. from BCKMASTR       */~
            from_stor$3,                 /* Store code for compare     */~
            fmtamount$14,                /* Formatted currency amount  */~
            hdr$40,                      /* ASKUSER constant           */~
            hdr$(3)79,                   /* PLOWCODE Headers           */~
            high_stor$3,                 /* Store code for capture     */~
            how_ship$20,                 /* How to ship from BCKMASTR  */~
            i$(24)80,                    /* Screen Image               */~
            inex(4), inex$(4)16,         /* PLOWCODE Include/Exclude   */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            linedisc$30,                 /* Edited line discount       */~
            low_stor$3,                  /* Store code for capture     */~
            mask$85,                     /* Text mask                  */~
            msg$(3)79,                   /* ASKUSER constants          */~
            optkey$90,                   /* Work Variable              */~
            optkey1$83,                  /* Work Variable              */~
            optkey2$90,                  /* Work Variable              */~
            orderdate$8,                 /* Order date from BCKMASTR   */~
            orderdisc$14,                /* Edited order discount      */~
            orddisclit$24,               /* Order Discount Literal     */~
            part_nmbr$25,                /* Part number from BCKLINES  */~
            part_desc$32,                /* Part descr from BCKLINES   */~
            part_type$3,                 /* Part Type (Purch or Mfg)   */~
            pf16$16,                     /* PF 16 Screen Literal       */~
            pf4$18,                      /* PF 4 Screen Literal        */~
            pf5$16,                      /* PF 5 Screen Literal        */~
            plowkey$99, pmsg$79,         /* Miscellaneous Read/Plow Key*/~
            pm_key$99,                   /* Miscellaneous Read/Plow Key*/~
            pm_descr$30,                 /* Precious Metal Item Descrip*/~
            pm_ext$10,                   /* Precious Metal Extension   */~
            pm_inv$1,                    /* Precious Metal INV Flag    */~
            pm_item$10,                  /* Precious Metal Item Code   */~
            pm_msg$74,                   /* Precious Metal Message     */~
            pm_msg2$74,                  /* Precious Metal Message     */~
            pm_on$1,                     /* Precious Metal ON Flag     */~
            pm_price$10,                 /* Precious Metal Price       */~
            pm_so$1,                     /* Precious Metal SO Flag     */~
            po$16,                       /* PO number from BCKMASTR    */~
            po_line$3,                   /* PO line # from BCKLINES    */~
            price$10,                    /* Edited unit price          */~
            price_uom$4,                 /* Pricing UOM from BCKMASTR  */~
            print_flag$1,                /* Print Flag                 */~
            qty_open$10,                 /* Edited open quantity       */~
            qty_ordr$10,                 /* Edited order quantity      */~
            readkey$64,                  /* Miscell Read variable      */~
            readkey2$64,                 /* Miscell Read variable      */~
            rptid$6,                     /* Report ID constant         */~
            setdat$6,                    /* BCKPRIDX/WORKFILE Setup dt */~
            ship_to$(6)31,               /* Ship to from BCKMASTR      */~
            shipinstr$(2)50,             /* Ship instructions- BCKMASTR*/~
            shipping$9,                  /* Shipping literal           */~
            shpdat$6,                    /* BCKPRIDX/WORKFILE Ship date*/~
            so$16,                       /* Sales Order # from BCKPRIDX*/~
            so$(2,2)16,                  /* Sales Order # Range For Pri*/~
            so_seqnr$3,                  /* SO sequence # from BCKLINES*/~
            sodate$8,                    /* Setup date from BCKPRIDX   */~
            soe_bom$3,                   /* BOM entered @ SO Entry     */~
            sold_to$(6)31,               /* Sold to from BCKMASTR      */~
            stock_uom$4,                 /* Stocking UOM from BCKMASTR */~
            stor_code$3,                 /* Store # from BCKPRIDX      */~
            str$3,                       /* BCKPRIDX/WORKFILE Store #  */~
            tdate$8,                     /* Temporary Date             */~
            term_code$20,                /* Terms code from BCKMASTR   */~
            term_desc$30,                /* Terms descrip from ARMTERMS*/~
            to_stor$3,                   /* High Store for compare     */~
            txtid$4,                     /* Common Text ID for lookup  */~
            txtid_cus$4,                 /* Custmr Text ID for lookup  */~
            type$1,                      /* Type code from BCKPRIDX    */~
            userid$3,                    /* Current User Id            */~
            xref_cus$1,                  /* Print Customer Xref Part   */~
            xref_mnf$1,                  /* Print Manufactur Xref Part */~
            xref_descr$32,               /* Xref Part # Description    */~
            xref_part$25,                /* Xref Part Number           */~
            xref_type$1                  /* Xref Part Type(Cust or Mnf)*/

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
            * #1  ! SYSFILE2 ! Caelus Management System Information     *~
            * #2  ! ENGMASTR ! Engineering Master (BOM Effectivity File)*~
            * #3  ! BCKPRIDX ! Sales Order Document Print Index         *~
            * #4  ! BCKMASTR ! Backlog Master File (Get Store Number)   *~
            * #5  ! BCKLINES ! Back Log Line Item File                  *~
            * #6  ! TXTFILE  ! System Text File                         *~
            * #7  ! CUSTOMER ! Customer Master File                     *~
            * #8  ! ARMTERMS ! A/R Payment Terms Codes                  *~
            * #9  ! HNYMASTR ! Inventory Master File                    *~
            * #10 ! HNYOPTNS ! Option Parts Replacements List           *~
            * #11 ! BOMSPEC  ! Options selected file                    *~
            * #13 ! BCKLNCUR ! Currency-specific BCK lines              *~
            * #14 ! CURMASTR ! Multi-Currency Master file               *~
            * #15 ! BOMMASTR ! Bill of Material Master File             *~
            * #16 ! STORNAME ! Store Master File                        *~
            * #17 ! BCKPRIDX ! Sales Order Document Print Index (SPLIN) *~
            * #18 ! WORKFILE ! Contains BCKPRIDX 'A's for PLOWCODE.     *~
            * #20 ! BCKPMSLD ! Precious Metal SO Shadow File            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =    1, keylen =  20

            select #2,  "ENGMASTR",                                      ~
                        varc,     indexed,  recsize =2015,               ~
                        keypos =    1, keylen =  29

            select #3,  "BCKPRIDX",                                      ~
                        varc,     indexed,  recsize = 45,                ~
                        keypos =   11, keylen =  29,                     ~
                        alt key 1, keypos =    1, keylen =  39

            select #4,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key 1,  keypos = 26,   keylen = 16

            select #5,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #6,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =   1, keylen =  11

            select #7,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key 1,  keypos = 10,   keylen =  30,         ~
                            key 2,  keypos = 424,  keylen =   9,         ~
                            key 3,  keypos = 771,  keylen =   9,         ~
                            key 4,  keypos = 780,  keylen =   9

            select #8,  "ARMTERMS",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =   1, keylen =  20

            select #9, "HNYMASTR", varc, indexed, recsize = 900,         ~
                        keypos =   1, keylen =  25,                      ~
                    alt key 1, keypos = 102, keylen =  9, dup,           ~
                        key 2, keypos =  90, keylen =  4, dup

            select #10, "HNYOPTNS",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos = 1, keylen = 54

            select #11, "BOMSPEC",                                       ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  54,                     ~
                        alt key  1, keypos =   57, keylen =  23

            select #13,  "BCKLNCUR",                                     ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =   5,  keylen = 19,                      ~
                        alt key  1, keypos =   1, keylen =  23

            select #14, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4

            select #15,  "BOMMASTR",                                     ~
                        varc,     indexed,  recsize = 150,               ~
                        keypos =  26,  keylen = 31,                      ~
                        alt key  1, keypos =   1, keylen =  56

            select #16, "STORNAME",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 3

            select #17, "BCKPRIDX", /* Alternate file channel for SPLIN*/~
                        varc,     indexed,  recsize = 45,                ~
                        keypos =   11, keylen =  29,                     ~
                        alt key 1, keypos =    1, keylen =  39

            select #18, "WORKFILE",                                      ~
                        varc,     indexed,  recsize = 43,                ~
                        keypos =    1, keylen =  25,                     ~
                        alt key 1, keypos =   17, keylen =  9, dup

            select #20, "BCKPMSLD",                                      ~
                        varc,     indexed,  recsize = 150,               ~
                        keypos = 1,    keylen =  29,                     ~
                         alternate key 1, keypos = 30, keylen = 25, dup, ~
                                   key 2, keypos = 73, keylen =  9, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1 ), f2%(1 ), 0%, rslt$(1 ))
            call "OPENCHCK" (#2,  fs%(2 ), f2%(2 ), 0%, rslt$(2 ))
            call "OPENCHCK" (#3,  fs%(3 ), f2%(3 ), 0%, rslt$(3 ))
            call "OPENCHCK" (#4,  fs%(4 ), f2%(4 ), 0%, rslt$(4 ))
            call "OPENCHCK" (#5,  fs%(5 ), f2%(5 ), 0%, rslt$(5 ))
            call "OPENCHCK" (#6,  fs%(6 ), f2%(6 ), 0%, rslt$(6 ))
            call "OPENCHCK" (#7,  fs%(7 ), f2%(7 ), 0%, rslt$(7 ))
            call "OPENCHCK" (#8,  fs%(8 ), f2%(8 ), 0%, rslt$(8 ))
            call "OPENCHCK" (#9,  fs%(9 ), f2%(9 ), 0%, rslt$(9 ))
            call "OPENCHCK" (#10, fs%(10), f2%(10), 0%, rslt$(10))
            call "OPENCHCK" (#11, fs%(11), f2%(11), 0%, rslt$(11))
            call "OPENCHCK" (#13, fs%(13), f2%(13), 0%, rslt$(13))
            call "OPENCHCK" (#14, fs%(14), f2%(14), 0%, rslt$(14))
            call "OPENCHCK" (#15, fs%(15), f2%(15), 0%, rslt$(15))
            call "OPENCHCK" (#16, fs%(16), f2%(16), 0%, rslt$(16))
            call "OPENCHCK" (#20, fs%(20), f2%(20), 0%, rslt$(20))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            rptid$ = "BCK002"
            max_lines% = 38%
            date$ = date
            call "DATEFMT" (date$)
            call "BCKSWTCH" ("BCK", "DISCS   ", discsw$, disc1or2, comp%)
            discsw$ = "N"
            if disc1or2 = 2 then discsw$ = "Y"
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

*        Check if Precious Metal Surcharge is on
            pm_on$ = "N"
            call "READ100" (#01, "SWITCHS.BCK", f1%(01%))
            if f1%(01%) = 1% then get #01 using L09500, pm_on$, xref_cus$, ~
                                                      xref_mnf$
L09500:         FMT POS(60), CH(1), POS(63), CH(1), CH(1)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            pf4$="(4)Previous Field" : pf5$=" ": pf16$="(16)Exit Program"
            init(" ") errormsg$, inpmessage$, high_stor$
            low_stor$, so$() = "ALL"
            if criteria_date$ <> " " then L10130
                criteria_date$ = date : call "DATEFMT" (criteria_date$)

L10130:     for fieldnr% = 1 to  3
L10140:         gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
                      if enabled% = 0 then L10260
L10160:         gosub'101(fieldnr%)     /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10240
L10190:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10160
                         if fieldnr% = 1% then L10140
                         goto L10190
L10240:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10160
L10260:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10160
            next fieldnr%
            goto edtpg1

        create_bckpridx_workfile
            call "SHOSTAT" ("Building Workfile for Selection Screen...")
            call "FILEBGON" (#18)                 /* Bye-bye, WORKFILE */
            count% = 0%     /* Indicate 'no Acknowledgements to print' */
            recnbr% = val(str(rslt$(3),17%,4%),4%)
            recnbr% = max(100%, recnbr% / 4%)
            call "WORKOPEN" (#18, "IO   ", recnbr%, f1%(18%))
            call "OPENFILE" (#17, "SPLIN", f2%(17%), rslt$(17%), " ")
        splin_thru_bckpridx
            call "READNEXT" (#17, f1%(17%))
                if f1%(17%) = 0% then goto end_of_splin
            get #17 using L10400, a$, str$, shpdat$, cus$, so$, bol$,     ~
                setdat$
L10400:         FMT CH(1), CH(3), CH(6), XX(1), CH(9), CH(16), CH(3),    ~
                     CH(6)
            if a$ <> "A" then goto end_of_splin
            write #18 using L10461, so$, cus$, bol$, str$, shpdat$,       ~
                setdat$                           /* Write it all, JIC */
L10461:         FMT CH(16), CH(9), 2*CH(3), 2*CH(6)
            count% = 1%          /* Indicate 'there are Acks to print' */
            goto splin_thru_bckpridx

        end_of_splin
            close #17            /* BCKPRIDX Alternate (SPLIN) channel */
            workfile_built% = 1%
            if count% <> 0% then return
            call "ASKUSER" (0%, "*** NO DATA TO PRINT ***", "There are "&~
                "no S.O. Acknowledgements to be printed.", " ", "Press "&~
                "any PF key to acknowledge and exit.")
            return clear all
            goto exit_program

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edtpg1
            inpmessage$ = edtmessage$
            pf4$  = " "
            pf5$  = " "
            pf16$ = "(16)Print Data"
            gosub'101(0%)               /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  = 16 then       datasave
                  if keyhit% <>  0 then       edtpg1
L11150:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 1 or fieldnr% >  3 then edtpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% = 0% then       edtpg1
                  pf4$, pf5$, pf16$ = " "
L11200:     gosub'101(fieldnr%)         /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11200
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11200
                if cursor%(1) - 5% <> fieldnr% then L11150
            goto edtpg1

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            call "SHOSTAT" ("Sales Order Acknowledgement Report in " &   ~
                "Progress")
            plowkey$ = "A"
            str(plowkey$,2) = all(hex(00))
            nbr_recs% = 0%
        plow_thru_bckpridx
            call "PLOWNXT1" (#3, plowkey$, 1%, f1%(3))
            if f1%(3) = 0% then goto end_of_report
            get #3 using L13190, stor_code$, type$, cust_code$, so$,      ~
                sodate$

        REM RECORD LAYOUT FOR FILE 'BCKPRIDX' ***************************
L13190:         FMT  XX(1),              /* 'A'ck, 'B'ol, or 'P'ick    */~
                     CH(3),              /* Store code                 */~
                     XX(6),              /* Ship date                  */~
                     CH(1),              /* Type code (A,B,P) again    */~
                     CH(9),              /* Customer code              */~
                     CH(16),             /* Sales order number         */~
                     XX(3),              /* Bill of lading number      */~
                     CH(6)               /* Setup date                 */

            if stor_code$ < from_stor$ then plow_thru_bckpridx
            if stor_code$ > to_stor$ then plow_thru_bckpridx
            if so$ <= so$(2,1) or so$ > so$(2,2) then plow_thru_bckpridx
            sodate% = 0%
            if len(sodate$) <> 6% then sodate_chk
            tdate$ = sodate$
            call "DATEFMT" ( tdate$, sodate% )
sodate_chk: if sodate% > date% then plow_thru_bckpridx
            call "DATEFMT" (sodate$)
            bckmastr_key$ = str(cust_code$,,9) & so$
            call "READ100" (#4, bckmastr_key$, f1%(4))
                if f1%(4) <> 0% then L13370
                gosub bckpridx_deleter
                goto plow_thru_bckpridx
L13370:     get #4 using L13420, po$, ship_to$(), sold_to$(), term_code$, ~
                how_ship$, fob$, shipinstr$(), txtid$, orderdate$,       ~
                odiscpercent, currency$, pm_so$, pm_inv$

        REM RECORD LAYOUT FOR FILE 'BCKMASTR' ***************************
L13420:         FMT  XX(9),              /* Customer code              */~
                     XX(16),             /* Sales Order number         */~
                     CH(16),             /* Purchase Order number      */~
                     6*CH(30),           /* Ship-to name & address     */~
                     6*CH(30),           /* Sold-to name & address     */~
                     CH(20),             /* Terms code                 */~
                     CH(20),             /* How to ship                */~
                     CH(20),             /* FOB                        */~
                     2*CH(50),           /* Shipping instructions      */~
                     XX(9),              /* Sales account number       */~
                     XX(9),              /* Discount account number    */~
                     XX(12),             /* Salesman codes             */~
                     XX(3),              /* Commission split           */~
                     XX(4),              /* Region code                */~
                     XX(200),            /* Variable fields            */~
                     CH(4),              /* Text ID                    */~
                     XX(3),              /* Store code                 */~
                     CH(6),              /* Order date (original)      */~
                     XX(6),              /* Cancel date                */~
                     XX(6),              /* Default due date           */~
                     XX(6),              /* Ship date                  */~
                     XX(6),              /* Date originally entered    */~
                     XX(3),              /* User ID                    */~
                     XX(6),              /* Date last changed          */~
                     XX(3),              /* User ID                    */~
                     XX(9),              /* Reason code for adjustment */~
                     XX(1),              /* Sales analysis period      */~
                     XX(1),              /* Price code                 */~
                     PD(14, 4),          /* Discount percent           */~
                     XX(8),              /* Gross open amount          */~
                     XX(1),              /* Credit hold flag           */~
                     XX(2),              /* General sequence number    */~
                     XX(4),              /* Next BOL number            */~
                     XX(2),              /* Customer type              */~
                     XX(9),              /* Account cross-reference    */~
                     CH(4),              /* Currency code              */~
                     XX(1),              /* How Held                   */~
                     CH(1),              /* PM SO Flag                 */~
                     CH(1),              /* PM INV Flag                */~
                     XX(101)             /* Filler                     */

            if nbr_recs% = 0% then gosub one_time_routine
            call "DATEFMT" (orderdate$)
            if sold_to$() = " " then sold_to$() = ship_to$()
            txtid_cus$ = all(hex(ff))
            call "READ100" (#7, cust_code$, f1%(7%))        /* Ship-to */
                if f1%(7%) = 0% then goto smasher
            get #7 using L14372, billto_xref$, txtid_cus$
L14372:                   FMT POS(780), CH(9), CH(4)
            if sold_to$(1) <> "BILL-TO" then goto smasher
                if billto_xref$ = cust_code$ then goto smasher
                call "READ100" (#7, billto_xref$, f1%(7))   /* Bill-to */
                if f1%(7) = 0% then goto smasher
                     get #7 using L14390, sold_to$(), bill_to$()
L14390:                   FMT POS(40), 6*CH(30), POS(253), 6*CH(30)
                if sold_to$() = " " then sold_to$() = bill_to$()
                if sold_to$(1) = "BILL-TO" then sold_to$() = bill_to$()

        smasher
            if str(ship_to$(6),17,1) <> " " or str(ship_to$(6),16,1) <>  ~
                " " or pos(str(ship_to$(6),27,4) = " ") > 0% then L14436
                  temp$ = str(ship_to$(6),27,4)
                  str(ship_to$(6),28,4) = temp$
                  str(ship_to$(6),27,1) = "-"
L14436:     call "SPCESMSH" (ship_to$(6), 2%)

            if str(sold_to$(6),17,1) <> " " or str(sold_to$(6),16,1) <>  ~
                " " or pos(str(sold_to$(6),27,4) = " ") > 0% then L14446
                  temp$ = str(sold_to$(6),27,4)
                  str(sold_to$(6),28,4) = temp$
                  str(sold_to$(6),27,1) = "-"
L14446:     call "SPCESMSH" (sold_to$(6), 2%)

            call "LINSMASH" (ship_to$())
            call "LINSMASH" (sold_to$())
            currdesc$ = " "
            if currency$ <> " " then                                     ~
                call "DESCRIBE" (#14, currency$, currdesc$, 1%, f1%(14))
            call "DESCRIBE" (#8, term_code$, term_desc$, 0%, f1%(8))
            if f1%(8) = 0% then goto use_term_code
            if term_desc$ <> " " then goto bckmastr_continue

        use_term_code
            term_desc$ = term_code$

        bckmastr_continue
            gosub check_pmetal_surcharge_settings
            gosub check_xref_print_settings
            page_nbr%, ordertotal = 0
            gosub page_heading
            gosub'200(txtid$)
            gosub'200(txtid_cus$)
            bcklines_key$ = so$
            str(bcklines_key$,17) = all(hex(00))

        bcklines_loop
            call "PLOWNEXT" (#5, bcklines_key$, 16%, f1%(5))
            if f1%(5) <> 0% then goto L15130
                gosub end_of_lines
                goto plow_thru_bckpridx
L15130:     get #5 using L15180, so_seqnr$, po_line$, part_nmbr$,         ~
                part_desc$, qty_ordr, qty_open, stock_prc, stock_uom$,   ~
                price_uom$, convfctr, price_prc, ldiscpercent, due_curr$,~
                txtid$, soe_bom$, pm_adj%

            call "READ100" (#13, bcklines_key$, f1%(13))
                if f1%(13) <> 0% then get #13 using L15158, stock_prc,    ~
                                                           price_prc
L15158:         FMT POS(24), PD(14,4), PD(14,4)

             /* Get Xref Part Number, if available */
             call "PTUSEDSB" ( "R", "BCK ",                              ~
                              str(bcklines_key$,,16%),  so_seqnr$,       ~
                              xref_part$, xref_descr$, xref_type$, ret%)
             if ret% = 0% then xref_part$,xref_descr$,xref_type$ = " "

        REM RECORD LAYOUT FOR FILE 'BCKLINES' ***************************
L15180:         FMT  XX(9),              /* Customer code              */~
                     XX(16),             /* Sales Order number         */~
                     CH(3),              /* Sales Order sequence number*/~
                     CH(3),              /* Purchase Order line number */~
                     CH(25),             /* Part number                */~
                     CH(32),             /* Part description           */~
                     XX(4),              /* Category code              */~
                     PD(14, 4),          /* Order quantity             */~
                     XX(8),              /* Quantity shipped           */~
                     PD(14, 4),          /* Open order quantity        */~
                     XX(8),              /* Quantity scheduled         */~
                     XX(16),             /* Filler                     */~
                     PD(14, 4),          /* Unit price @ stocking UOM  */~
                     CH(4),              /* Stocking UOM               */~
                     CH(4),              /* Pricing UOM                */~
                     PD(14, 7),          /* Conversion factor          */~
                     PD(14, 4),          /* Unit price @ pricing UOM   */~
                     PD(14, 4),          /* Discount percent           */~
                     XX(1),              /* Taxable code               */~
                     XX(9),              /* Sales account number       */~
                     XX(9),              /* Discount account number    */~
                     XX(6),              /* Original Due date          */~
                     CH(6),              /* Current Due date           */~
                     XX(6),              /* Ship date                  */~
                     XX(6),              /* Lot number                 */~
                     XX(8),              /* Project code               */~
                     XX(8),              /* Filler                     */~
                     XX(1),              /* Demand type                */~
                     XX(1),              /* Priority                   */~
                     CH(4),              /* Text ID                    */~
                     XX(1),              /* Allocation flag            */~
                     XX(8),              /* Invoice number             */~
                     XX(8),              /* Estimate number            */~
                     CH(3),              /* SOE BOM to use             */~
                     POS(278), BI(1),    /* PM Surcharge Added Flag    */~
                     XX(22)              /* Filler                     */

            call "DATEFMT" (due_curr$)
            stock_prc = round(price_prc / convfctr, 4)
            extension = round(price_prc * qty_open / convfctr, 2)
            linedisc$ = " "
            discount = 0
            if discsw$ = "N" then goto net_extension
                if ldiscpercent = 0 then goto bcklines_continue
                convert ldiscpercent to linedisc$, pic(-###.##)
                str(linedisc$,8) = "% DISCOUNT"
                discount = -(extension * ldiscpercent) / 100
                call "CURRFMT" (discount, currency$, discount$, "N")
                goto bcklines_continue
        net_extension   /* Price and Extension printed less discount */
            extension = extension - ((extension * ldiscpercent) / 100)
            price_prc = price_prc - ((price_prc * ldiscpercent) / 100)
        bcklines_continue
            call "CONVERT" (qty_ordr, 2.2, qty_ordr$)
            call "CONVERT" (qty_open, 2.2, qty_open$)
            call "CONVERT" (price_prc, 2.4, price$)
            call "CURRFMT" (extension, currency$, extension$, "N")
            if linedisc$ = " " and max_lines% - nbr_lines% < 2% then     ~
                gosub page_heading
            if linedisc$ <> " " and max_lines% - nbr_lines% < 3% then    ~
                gosub page_heading

            if xref_part$ = " " then L16122
            if xref_type$ = "M" and xref_mnf$ <> "Y" then L16122
            if xref_type$ = "C" and xref_cus$ <> "Y" then L16122
            if xref_type$ <> "C" and xref_type$ <> "M" then L16122
                part_nmbr$ = xref_part$ : part_desc$ = xref_descr$

L16122:     print using L62400, so_seqnr$, po_line$, part_nmbr$,          ~
                qty_ordr$, qty_open$, price$, extension$
            nbr_lines% = nbr_lines% + 1%
            print using L62600, part_desc$, due_curr$, stock_uom$,        ~
                price_uom$
            nbr_lines% = nbr_lines% + 1%
            if xref_type$ = "M" and xref_mnf$ = "B" then L16143 else L16148
L16143:         print using L62620, "Manufactor Part No.:", xref_part$,   ~
                                    "(" & xref_descr$ & ")"
                goto L16154             /* Iterate */
L16148:     if xref_type$ = "C" and xref_cus$ = "B" then L16150 else L16160
L16150:         print using L62620, "Customer Part No.:", xref_part$,     ~
                                    "(" & xref_descr$ & ")"
L16154:         nbr_lines% = nbr_lines% + 1%

L16160:     if pm_adj% <> 1%  or pm_msg$ & pm_msg2$ = " " then L16175
*              PRINT USING 62700, PM_MSG$
*              NBR_LINES% = NBR_LINES% + 1%
                gosub print_pm_surcharges
L16175:     if linedisc$ = " " then goto print_detail_text
            print using L62710, linedisc$, discount$
            nbr_lines% = nbr_lines% + 1%
        print_detail_text
            gosub print_options
            spacer% = 0%
            gosub'200(txtid$)
            ordertotal = ordertotal + extension + discount

        REM PRINT THE PART TEXT (IF ANY) FROM THE 'HNYMASTR' FILE *******
            call "READ100" (#9, part_nmbr$, f1%(9))
            if f1%(9) = 0% then L16275
            get #9 using L16265, txtid$
L16265:         FMT  POS(98), CH(4)      /* HNYMASTR part text ID      */
            gosub'200(txtid$)
L16275:     if spacer% <> 0% then L16282
            print : nbr_lines% = nbr_lines% + 1%
L16282:     if f1%(9%) = 1% then gosub print_bom_text
            goto bcklines_loop

        print_options
            optkey$ = bcklines_key$
            str(optkey$,20) = all(hex(00))
L16310:     call "PLOWALTS" (#11, optkey$, 1%, 19%, f1%(11))
                if f1%(11) = 0 then return
            get #11 using L16325, optkey1$, comp_pn$
L16325:     FMT CH(56), POS(80), CH(25)   /* Option Selected */

            REM Now we need to search out the 'Print' flag.
            REM First Check For Assembly Specific Options List...
            optkey2$ = str(optkey1$,26,28) /* Assy + BOM */
            str(optkey2$,29) = str(optkey1$,,25) /* Orig Comp */
L16355:     str(optkey2$,54) = hex(00)
L16360:     call "PLOWNEXT" (#10, optkey2$, 53%, f1%(10))
                if f1%(10) <> 0 then L16445
                if str(optkey2$,,28) = " " then L16310
                REM Ok, Look For Part Specific Options...
                   str(optkey2$,,28) = " "      /* Any Assy */
                   goto L16355

L16445:     get #10, using L16450, optkey1$, print_flag$
L16450:         FMT POS(55), CH(25), CH(1)
            if optkey1$ <> comp_pn$ then L16360
            if pos("YD"=print_flag$) = 0 then L16310 /*Skip To Next opt*/

            call "DESCRIBE" (#9, comp_pn$, part_desc$, 0%, f1%(9))
                if f1%(9) = 0 then L16310 /* Skip To Next Option */
            temp% = 2%
            if print_flag$ = "D" then temp% = 1%
            if max_lines% - nbr_lines% < temp% then gosub page_heading
            if print_flag$ = "D" then L16505
               print using L63210, comp_pn$
L16505:     print using L63220, part_desc$
            nbr_lines% = nbr_lines% + temp%
            goto L16310 /* Go Get Next Option */

        end_of_lines
            if max_lines% - nbr_lines% < 2% then gosub page_heading
            print using L63240      /* Underscore for total */
            call "CURRFMT" (ordertotal, currency$, fmtamount$, "Y")
            if currency$ <> " "                                          ~
                then desc$ = currency$ & " " & currdesc$ & " " &         ~
                     "SALES ORDER TOTAL:"                                ~
                else desc$ = "SALES ORDER TOTAL:"
            call "STRING" addr ("RJ", desc$, len(str(desc$)))
            print using L63000, desc$, fmtamount$
            nbr_lines% = nbr_lines% + 2%
            if odiscpercent = 0 then goto bckpridx_deleter
                discount = -round((ordertotal * odiscpercent) / 100, 2)
                convert odiscpercent to orddisclit$, pic(-###.##)
                str(orddisclit$,8) = "% ORDER DISCOUNT:"
                call "CURRFMT" (discount, currency$, orderdisc$, "N")
                if nbr_lines% > max_lines% then gosub page_heading
                print using L63110, orddisclit$, orderdisc$
                nbr_lines% = nbr_lines% + 1%
                if nbr_lines% > max_lines% then gosub page_heading
                ordtotal = ordertotal + discount
                call "CURRFMT" (ordtotal, currency$, fmtamount$, "Y")
                desc$ = "NET SALES ORDER:"
                call "STRING" addr ("RJ", desc$, len(str(desc$)))
                print using L63000, desc$, fmtamount$
        bckpridx_deleter
            delete #3
            return

        end_of_report
            if nbr_recs% <> 0% then goto end_of_job
L17090:         comp% = 2%
                hdr$ = "*** NULL SET SELECTED ***"
                msg$(1) = "There are no records that satisfy your" &     ~
                     " criteria"
                msg$(3) = "Press RETURN to continue"
                call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
                if comp% <> 0% then L17090
                goto inputmode
        end_of_job
            close printer
            call "SETPRNT" ("BCK002", " ", 0%, 1%)
            goto exit_program

        page_heading
            page_nbr% = page_nbr% + 1% : nbr_lines% = 0%
            if page_nbr% > 1% then print using L62710,                    ~
                ">>>>>  C O N T I N U E D  <<<<<", " CONTINUED"
            print page
            print skip(4)
            print using L60040, so$, date$, page_nbr%
            print skip(2)
            if currency$ = " "                                           ~
                then print using L60080, " ", po$, cust_code$, orderdate$ ~
                else print using L60080, "CURRENCY: " & currency$ & " " & ~
                     currdesc$, po$, cust_code$, orderdate$
            print skip(3) /* SKIP TO SOLT-TO, SHIP-TO PORTION OF FORM */
            print using L61000, sold_to$(1), ship_to$(1)
            print using L61000, sold_to$(2), ship_to$(2)
            print using L61000, sold_to$(3), ship_to$(3)
            print using L61000, sold_to$(4), ship_to$(4)
            print using L61000, sold_to$(5), ship_to$(5)
            print using L61000, sold_to$(6), ship_to$(6)
            print skip(3) /* SKIP TO TERMS, SHIP VIA, FOB, STORE LINE */
            print using L62000, term_desc$, how_ship$, fob$, stor_code$
            print skip(2) /* SKIP TO BODY OF FORM */
            if page_nbr% > 1% then return
            if shipinstr$() = " " then return
                shipping$ = "SHIPPING:"
                for si% = 1 to 2
                     if shipinstr$(si%) = " " then goto L17520
                          print using L62200, shipping$, shipinstr$(si%)
                          shipping$ = " "
                          nbr_lines% = nbr_lines% + 1%
L17520:         next si%
            return

        one_time_routine
            nbr_recs% = 1%
            select printer(87)
            call "SETPRNT" ("BCK002", " ", 0%, 0%)
            return

        print_bom_text    /* Print BOM HDR Text (If Any) From BOMMASTR */
            bomid$ = " "                /* Indicate 'no effective BOM' */
            txtid$ = all(hex(ff))       /* Indicate 'no text to print' */
            get #9 using L17608, part_type$
L17608:         FMT  POS(180), CH(3)      /* HNYMASTR part type         */
            convert part_type$ to part_type%, data goto L17780
            if part_type% > 0% and part_type% < 500% then return
            if part_type% > 789% and part_type% < 800% then return
            if soe_bom$ = " " then goto L17710     /* BOM entered @ SOE? */
                bomid$ = soe_bom$      /* Yup -- use it for text lookup */
                goto L17734
L17710:     call "DATUNFMT" (due_curr$)  /* Nope -- do it the hard way */
            call "PIPINDEX" (#1, due_curr$, dateindex%, ret%)
            if ret% = 1% then return /* Planning Calendar Messed Up!   */
            if dateindex% = 0% then return /* Off Planning Calendar!   */
            readkey2$ = str(part_nmbr$,,25%) & "1" & hex(00)
            call "PLOWNEXT" (#2, readkey2$, 26%, f1%(2%))  /* ENGMASTR */
            if f1%(2%) = 0% then return
            get #2, using L17727, bom$()
L17727:        FMT POS(30), 490 * CH(3)
            bomid$ = bom$(dateindex%)
L17734:     if bomid$ = " " then return    /* No BOM? Get outta Dodge! */
            readkey$ = str(part_nmbr$,,25%) & str(bomid$) & hex(00)
            call "PLOWNEXT" (#15, readkey$, 28%, f1%(15%)) /* BOMMASTR */
            if f1%(15%) <> 0% then get #15, using L17755, txtid$
L17755:         FMT POS(90),CH(4)
            gosub'200(txtid$)
L17780:     return

        check_pmetal_surcharge_settings
            pm_msg$, pm_msg2$ = " "
            if pm_on$ <> "Y" then return
            if pm_so$ <> "Y" then L17850
               pm_msg$ = " PM Surcharges have been added.  Break down is:"
L17850:     if pm_inv$ <> "Y"  then return
                pm_msg2$ = " PM Surcharge may be Adjusted at Invoicing."
                if pm_msg$ <> " " then return
                    pm_msg2$ = "PM Surcharge will be Added at Invoicing."

            return

        check_xref_print_settings
            call "READ100" (#7, cust_code$, f1%(7%))
            if f1%(7%) = 0% then return   /* Shouldn't happen */
            get #7 using L17920, temp_cus$, temp_mnf$
L17920:         FMT POS(1092), CH(1), CH(1)

            if temp_cus$ = " " then L17945
                if pos("YNB" = temp_cus$) <> 0% then xref_cus$ = temp_cus$

L17945:     if temp_mnf$ = " " then L17990
                if pos("YNB" = temp_mnf$) <> 0% then xref_mnf$ = temp_mnf$

L17990:     return

        print_pm_surcharges
            if pm_msg$ = " " then L18470
                print using L62700, pm_msg$
                nbr_lines% = nbr_lines% + 1%
                pm_key$ = str(so$) & so_seqnr$
                str(pm_key$, 20%, 1%) = hex(00)
L18070:         call "PLOWNEXT" (#20, pm_key$, 19%, f1%(20%))
                if f1%(20%) = 0% then L18470  /* All done */
                get #20 using L18090, pm_item$, pm_price, pm_factor,      ~
                                     pm_descr$
L18090:            FMT POS(20), CH(10), POS(55), PD(14,7), PD(14,7),     ~
                       POS(91), CH(30)
                if pm_item$ <> " " then L18200  /* PM Items */
               /* Print Original Part w/out surcharge */
                pm_price = pm_price / convfctr
                ext = round(pm_price * qty_open, 2)
                call "CONVERT" (pm_price, 2.4, pm_price$)
                call "CONVERT" (ext     , 2.2, pm_ext$)

                if max_lines% - nbr_lines% < 3% then gosub page_heading

                print using L63270, part_nmbr$, pm_price$, pm_ext$
                nbr_lines% = nbr_lines% + 1%
                goto L18070  /* Back for PM Items */

L18200:        /* Print PM Items */
                pm_ext   = round( pm_price * pm_factor * qty_open, 2)
                pm_price = round( pm_price * pm_factor, 4)
                call "CONVERT" (pm_ext,   2.2, pm_ext$)
                call "CONVERT" (pm_price, 2.4, pm_price$)
                if max_lines% - nbr_lines% < 3% then gosub page_heading
                print using L63300, pm_item$, pm_descr$, pm_price$, pm_ext$
                nbr_lines% = nbr_lines% + 1%
                goto L18070  /* Back for more */

L18470:     if pm_msg2$ = " " then return
                print using L62700, pm_msg2$
                nbr_lines% = nbr_lines% + 1%

            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20120,         /* LATEST SO DATE   */~
                                    L20170,         /* STORE CODE       */~
                                    L20210          /* SO RANGE         */
                     return

L20120:     REM LATEST SO DATE                        CRITERIA_DATE$
                inpmessage$ = "Enter the LATEST Sales Order Date to " &  ~
                     "print"
                return

L20170:     REM STORE CODE                            STOR_CODE$
                inpmessage$ = "Enter range of STORE CODES, partial, '?'"&~
                     "or 'ALL'"
                return

L20210:     REM SALES ORDER NUMBER RANGE              SO$()
                inpmessage$ = "Enter range of SALES ORDER #s, partial, "&~
                      "'?' or 'ALL'"
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
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2% /* OPEN WINDOW AT BOTTOM */
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'101(fieldnr%)
                  str(line2$,62%) = "BCKACKS : " & str(cms2v$,,8%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                  else                  init(hex(86)) lfac$()
                  on fieldnr% gosub L40200,         /* LATEST SO DATE   */~
                                    L40200,         /* STORE CODE       */~
                                    L40200          /* SO# RANGE        */
                  if errormsg$ > " " and fieldnr% > 0% then              ~
                     lfac$(fieldnr%) = or hex(10)
                  goto L40270

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40200:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return

L40270:     accept                                                       ~
               at (01,02),                                               ~
                  "Sales Order Acknowledgements",                        ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,02), "Sales Order date",                           ~
               at (06,25), fac(lfac$( 1)), criteria_date$       , ch(08),~
                                                                         ~
               at (07,02), "Store code range",                           ~
               at (07,25), fac(lfac$( 2)), low_stor$            , ch(03),~
               at (07,52), fac(lfac$( 2)), high_stor$           , ch(03),~
                                                                         ~
               at (08,02), "Sales Order # range",                        ~
               at (08,25), fac(lfac$( 3)), so$(1,1)             , ch(16),~
               at (08,52), fac(lfac$( 3)), so$(1,2)             , ch(16),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,65),                                               ~
                  "(13)Instructions",                                    ~
               at (23,02),                                               ~
                  "(1)Start Over",                                       ~
               at (23,20), fac(hex(8c)), pf4$                           ,~
               at (23,65),                                               ~
                  "(15)Print Screen",                                    ~
               at (24,65), fac(hex(84)), pf16$                          ,~
                                                                         ~
               keys(hex(0001040d0f10)),                                  ~
               key (keyhit%)

               if keyhit% <> 13 then L40720
                  call "MANUAL" ("BCKACKS ")
                  goto L40270

L40720:        if keyhit% <> 15 then L40760
                  call "PRNTSCRN"
                  goto L40270

L40760:        if fieldnr% > 0% then return
                  close ws
                  call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                  return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50130,         /* LATEST SO DATE   */~
                                    L50170,         /* STORE CODE       */~
                                    L50230          /* STORE CODE       */
                  return

L50130:     REM LATEST SO DATE                        CRITERIA_DATE$
                call "DATEOK" (criteria_date$, date%, errormsg$)
                return

L50170:     REM STORE CODE                            STOR_CODE$
                call "TESTRNGE" (low_stor$, high_stor$, from_stor$,      ~
                     to_stor$, errormsg$, #16)
                from_stor$ = addc (hex(01))
                return

L50230: REM SO NUMBER RANGE                           SO$()
            if pos(so$(1%,1%) = "?") = 0% then goto L50290
                if workfile_built% =0% then gosub create_bckpridx_workfile
                str(so$(1%,1%), pos(so$(1%,1%) = "?"), 1%) = hex(00)
                gosub'210(so$(1%,1%), "FROM")
                if errormsg$ <> " " then return
                so$(1%,1%) = str(plowkey$,,16%)

L50290:     if pos(so$(1%,2%) = "?") = 0% then goto L50470
                if workfile_built% =0% then gosub create_bckpridx_workfile
                str(so$(1%,2%), pos(so$(1%,2%) = "?"), 1%) = hex(00)
                gosub'210(so$(1%,2%), "TO")
                if errormsg$ <> " " then return
                so$(1%,2%) = str(plowkey$,,16%)

L50470:     call "TESTRNGE" (so$(1%,1%), so$(1%,2%), so$(2%,1%),         ~
                so$(2%,2%), errormsg$)
            return

        REM *************************************************************~
            *      PRINT TEXT FOR EITHER THE HEADER OR DETAIL LINES     *~
            *************************************************************

            deffn'200(txtid$)
            if txtid$ = hex(ffffffff) then return
            if txtid$ = " " then return
                txt% = (page_nbr% * 100%) + nbr_lines%
                comp% = 0%
L50600:         call "TXTPRINT" (#6, f2%(6),  87%, txtid$, "BCK002", 9%, ~
                     nbr_lines%, max_lines%, "T", mask$, comp%)
                if comp% = 0% then goto L50650
                     gosub page_heading
                     goto L50600
L50650:         if txt% = (page_nbr% * 100%) + nbr_lines% then return
                     print
                     nbr_lines% = nbr_lines% + 1%
                     spacer% = 1%
                     return

        deffn'210(plowkey$, temp$)
            init (" ") hdr$(), inex$(), pmsg$
            mat inex = zer
            mat dmap = zer
            hdr$(1%) = "  S.O. #           Customer Code & Name        "&~
                "               Setup  Shipped"
            pmsg$ = hex(06) & "Please select a '" & temp$ & "' Sales Or"&~
                "der  #."
            dmap(1%)  =   1.16  : dmap(2%)  =  1
            dmap(3%)  =  17.09  : dmap(4%)  = 18
            dmap(5%)  = -10.30  : dmap(6%)  = 28
            dmap(7%)  =  38.061 : dmap(8%)  = 59
            dmap(9%)  =  32.061 : dmap(10%) = 68
*        Plow WORKFILE to show S. O. #s available in BCKPRIDX.
            call "PLOWCODE" (#18, plowkey$, pmsg$, -9000%,  .3, f1%(18%),~
                hdr$(), 0, -17, inex(), inex$(), "d", " ", #07, dmap())
            if f1%(18%) <> 0% then return        /* Hit/Selection made? */
                errormsg$ = "You must select a '" & temp$ & "' Sales Or"&~
                     "der #."
                return

        REM *************************************************************~
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************

L60040: %                                                      ##########~
        ~###### ######## ####
L60080: %###############################################  ###############~
        ~# ######### ########
L61000: %          ###############################             ##########~
        ~#####################
L62000: %  ############################   ####################   ########~
        ~############   ###
L62200: %          ######### ############################################~
        ~######
L62400: %### ### ################################ ########## ########## #~
        ~######### ##########
L62600: %        ################################  ########  ####       #~
        ~###
L62620: %        ################### #########################  #########~
        ~#########################
L62700: %        ########################################################~
        ~####################
L62710: %        ################################                        ~
        ~          ##########
L63000: % ###############################################################~
        ~##### ##############
L63110: %                                             ###################~
        ~##### ##############

L63210: %        #########################
L63220: %        ################################   OPTION

L63240: %                                                                ~
        ~      --------------

L63270: %          #########################               @ ##########  ~
        ~Ext: ##########

L63300: %          ##########  ##########################  @ ##########  ~
        ~Ext: ##########

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
