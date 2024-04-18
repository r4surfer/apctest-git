        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *                                                           *~
            *   CCC   U   U   SSS   IIIII  N   N  PPPP   U   U  TTTTT   *~
            *  C   C  U   U  S        I    NN  N  P   P  U   U    T     *~
            *  C      U   U   SSS     I    N N N  PPPP   U   U    T     *~
            *  C   C  U   U      S    I    N  NN  P      U   U    T     *~
            *   CCC    UUU    SSS   IIIII  N   N  P       UUU     T     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * CUSINPUT - Manages Customer Master File.                  *~
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
            * 06/12/86 ! Original                                 ! ERN *~
            * 09/03/86 ! Bill-to/Ship-to Open Order Logic         ! JIM *~
            * 12/31/86 ! Added Copy Function                      ! MJB *~
            * 04/09/87 ! Fixed Copy Function                      ! MJB *~
            * 11/10/87 ! Added Customer default Currency Code.    ! JIM *~
            * 09/09/88 ! Added Customer's Credit Parent logic.    ! JIM *~
            * 11/11/88 ! Customer can't enter self as Crdt Parent.! JDH *~
            * 04/20/89 ! Added # days for credit hold             ! JDH *~
            * 08/15/89 ! Honors Multi-currency flag.              ! JDH *~
            * 09/22/89 ! Added indication if a Credit Parent.     ! JDH *~
            * 11/22/89 ! Added Industry and Country codes.        ! JEF *~
            * 03/20/90 ! Fixed PRR 11323.  Added GOSUB after a    ! SID *~
            *          ! GET to BALS() for rounding purposes.     !     *~
            * 11/01/90 ! Added ICC & ICOC for Mgmnt Values Project! JDH *~
            * 04/29/91 ! Minor Format changes for ICC & ICOC Codes! JBK *~
            *          !    and added call to ALLFREE.            !     *~
            * 05/29/91 ! Fixed PRR 11389.  No Credit Parent setup ! JBK *~
            *          !   allowed for a Ship-To customer         !     *~
            * 11/16/92 ! Cust Credit- Dynamic fields from CCRMASTR! JIM *~
            * 11/18/92 ! Cust Credit- Added calls to ARQCUSCR.    ! JIM *~
            * 12/01/92 ! Added PF(10) call to CORPARSB- Core Track! JDH *~
            * 02/03/93 ! PRR 12614  Added Export(Y/N) field.      ! JDH *~
            *          ! PRR 12680  Stopped back door approach to !     *~
            *          !   correction 05/29/91 above.             !     *~
            *          ! PRR 13016  Corrected GET of ICC/ICOC.    !     *~
            * 05/13/93 ! Fixed Data Conversion Error when Bill-to ! JDH *~
            *          !   X-Ref is changed.  Wrong file specified!     *~
            * 08/09/93 ! PRR 12980  Added dash to zip.            ! JDH *~
            * 10/25/93 ! Added support of Shipment Scheduling     ! MLJ *~
            *          !  Priority Code (screen 2, line 12).  To  !     *~
            *          !  do this, had to move ICOC code & prompt !     *~
            *          !  from scn 2, line 4 to scn 3, line 20.   !     *~
            * 06/06/94 ! Change test on Ship Priority to allow 1-5! RJH *~
            * 07/01/94 ! Added new field 'price code for options' ! WPH *~
            * 10/24/94 ! PRR 13306  Copy doesn't copy Account or  ! JDH *~
            *          !  Billto Xref if customer being copied is !     *~
            *          !  its own Account or Billto Xref.         !     *~
            * 12/19/94 ! Add Precious Metal Surcharge Flags for   ! RJH *~
            *          !  SO Entry and Invoicing.                 !     *~
            * 03/16/95 ! PRR 13187-Add Xref Part flags for Printng! RJH *~
            * 03/20/95 ! PRR 13143. Print flag for acknowledgments! JDH *~
            *          ! PRR 13267. Added fax number.             !     *~
            * 12/12/97 ! All (EWD) mods hve been moved to the most! RHH *~
            *          ! Current Caelus release. All mods are     !     *~
            *          ! maked with (EWD)                         !     *~
            * 09/18/00 ! Mod to only allow one person in a        ! CMG *~
            *          !    customer at a time. (EWD001)          !     *~
            * 09/19/00 ! Mod to take old text out of TXTFILE when ! CMG *~
            *          !    data saved with new textid. (EWD002)  !     *~
            * 07/19/01 ! Mod to change the field Industry Code to ! CMG *~
            *          !    Atrium Divison Code.        (EWD003)  !     *~
            * 01/28/03 ! Mod to turn off lock to only allow one   ! CMG *~
            *          !    person in a customer at a time and    !     *~
            *          !    to add a new status of 'N' for New    !     *~
            *          !    customer but Francois to pass 'A' on  !     *~
            *          !    order not 'N'               (EWD004)  !     *~
            * 02/28/06 ! Mod for North East          (AWD005)     ! CMG *~
            * 02/07/07 ! Mod to validate how ship    (AWD006)     ! CMG *~
            * 04/24/07 ! Mod to last modify date     (AWD007)     ! CMG *~
            * 06/26/07 ! Mod for new status 'O'      (AWD008)     ! CMG *~
            * 09/22/17 ! Add warning msg on deletes  (CR1128)     ! RDB *~
            * 02/05/19 ! Add display of store xref    CR188       ! RDB *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**

        dim                                                              ~
            acct$12,                     /* Acct # for test/descr rtn  */~
            acctdescr$32,                /* Descr  for test/descr rtn  */~
            accttype$1,                  /* Type   for test/descr rtn  */~
            acctxref$9,                  /* Account Cross Reference    */~
            acctxrefdescr$32,            /* Account Cross Reference    */~
            acks$1,                      /* Print Acknowledgements?    */~
            aracct$12,                   /* Net Invoice Distribution   */~
            aracctdescr$32,              /* Net Invoice Distribution   */~
            baldate$8,                   /* High A/R Balance Date      */~
            bal$1,                       /* Balance Type (BF, O/I)     */~
            bals(4),                     /* Summary Balance Info       */~
            billxref$9,                  /* Bill-to Cross Reference    */~
            billxrefdescr$32,            /* Bill-to Cross Reference    */~
            bo$1,                        /* Allow Backorders?          */~
            cashacct$12,                 /* Cash-in-Bank Account       */~
            cashacctdescr$32,            /* Cash-in-Bank Account       */~
            ccrdflts$200,                /* CCRMASTR default values    */~
            comm%(3), comm$(3)3,         /* Commission Split %s        */~
            contact$20,                  /* Customer Contact           */~
            copycus$9,                   /* Customer Code to Copy      */~
            country$3, countrydesc$30,   /* Country Code & description */~
            countryflag$1,               /* Country codes on-file (y,n)*/~
            crchng$8, cruser$3,          /* CR Limit change audit      */~
            crparent$9, crpardescr$32,   /* Credit parent & description*/~
            curr_on_flag$1,              /* Multi-currency on? Y or N  */~
            currency$4, currdesc$32,     /* Currency code, description */~
            curr_msg$13,                 /* Screen message for currency*/~
            cursor%(2),                  /* Cursor location for edit   */~
            customer$9,                  /* Customer Code              */~
            cust_add$35,                 /* (EWD) - Screen text Msg    */~ 
            date$8,                      /* Date for screen display    */~
            defaults$(10)120,            /* Defaults Record            */~
            disc$5,                      /* Standard Order Discount %  */~
            discacct$12,                 /* Sales Discounts Account    */~
            discacctdescr$32,            /* Sales Discounts Account    */~
            docs$1,                      /* Docs: S/L/B/N              */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            exempt$25,                   /* Tax Exemption Number       */~
            export$1,                    /* Normally an Export Cust?   */~
            fcacct$12,                   /* Finance Charge Account     */~
            fcacctdescr$32,              /* Finance Charge Account     */~
            fctable$1,                   /* Finance Charge Table       */~
            fctabledescr$32,             /* Finance Charge Table       */~
            fob$20,                      /* FOB                        */~
            frtacct$12,                  /* Freight Account            */~
            frtacctdescr$32,             /* Freight Account            */~
            hicldate$6,                  /* CCRMASTR High Cr Lim Date  */~
            howship$20,                  /* How Ship                   */~
            i$(24)80,                    /* Screen Image               */~
            icc$6,                       /* Intercompany Corporate Code*/~
            icc_d$6,                     /* Intercompany Corporate Code*/~
            icc_prompt$23,               /* Intercompany Corporate Code*/~
            iccflag$1,                   /* Interco Corp GENCODES exist*/~
            icoc$9,                      /* Intercompany Ownership Code*/~
            icoc_d$9,                    /* Intercompany Ownership Code*/~
            icoc_prompt$22,              /* Intercompany Ownership Code*/~
            icoc_desc$30,                /* Interco Owner Name         */~
            division$4, divisiondesc$30, /* Atrium Division Code EWD003*/~
            divisionflag$1,              /* Divison Flag         EWD003*/~
            inpmessage$79,               /* Informational Messages     */~
            lastcash$8,                  /* Last Cash Receipts Date    */~
            lastchng$8,                  /* Date record last changed   */~
            lastinv$8,                   /* Date of last invoice       */~
            lastused$8,                  /* Date record last update    */~
            lastuser$3,                  /* User who made last change  */~
            lateship$1,                  /* Allow Late Shipments?      */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            limit$10,                    /* Credit Limit               */~
            line2$79,                    /* Second Line of Screen Headr*/~
            misc$50,                     /* Junk Field                 */~
            mgtval_on$1,                 /* Are Management Values used?*/~
            nbrdays$3, cdchng$8, cduser$3, /* Credit # days fields     */~
            opened$8,                    /* Date account opened        */~
            origbillto$9,                /* Original Bill-to on read   */~
            pc$1,                        /* Standard Price Code        */~
            pco$1,                       /* Price code for options     */~
            pcdescr$32,                  /* Standard Price Code        */~
            pcodescr$32, rhh$32,         /* Price code for optns (EWD) */~
            pf$(3)79,                    /* PF Keys available          */~
            pfkeys$(32)1,                /* PF Keys available          */~
            phone$(2)10,                 /* Phone Number & Fax Number  */~
            pm_inv$1,                    /* Precious Metal INV Flag    */~
            pm_on$1,                     /* Precious Metal ON Flag     */~
            pm_so$1,                     /* Precious Metal SO Flag     */~
            poreqd$1,                    /* PO Required?               */~
            readkey$50,                  /* Misc. Purpose Read Key     */~
            regiondescr$32, rh$32,       /* Sales Region Code (EWD)    */~
            sales$12,                    /* Sales Distribution Account */~
            salesdescr$32,               /* Sales Distribution Account */~
            salesmen$(3)4,               /* Salesmen/ Commission Splits*/~
            salesmendescr$(3)32,         /* Salesmen/ Commission Splits*/~
            scust$9, ssort$30,           /* Save fields for copy       */~
            scr%(4,18), set%(255),       /* Soft Enable Tables         */~
            shipcode$1,                  /* Shipping Priority Code     */~
            shipinstr$(2)50,             /* Shipping Instructions      */~
            shiprgn$9,                   /* Shipping Region Code       */~
            shiprgndescr$32,             /* Shipping Region Code       */~
            shipto$(6)30,                /* Ship-to Name and Address   */~
            soldto$(6)30,                /* Sold-to Name and Address   */~
            sort$30,                     /* Customer Sort Name         */~
            status$1,                    /* Customer Status            */~
            statusdescr$32,              /* Customer Status            */~
            storexref$9,                 /* Store xref CR1855          */~
            taxable$1,                   /* Customer Taxable?          */~
            taxacct$12,                  /* Sales Tax Account          */~
            taxacctdescr$32,             /* Sales Tax Account          */~
            taxcode$10,                  /* Sales Tax Code             */~
            taxcodedescr$32,             /* Sales Tax Code             */~
            temp$60,                     /* Misc. Temp Variable        */~
            terms$20, termsdescr$30,     /* Payment Terms (Code)       */~
            text$(392%,1%)70, textid$4,  /* Text Routine Elements      */~
            saveid$4,                    /* Save textid  (EWD002)      */~
            type$2,                      /* Customer Type Code         */~
            typedescr$32,                /* Customer Type Code         */~
            userid$3,                    /* Current User Id            */~
            tstuserid$3,                 /* Test Id for Gencodes EWD001*/~
            vf$200,                      /* Variable Fields            */~
            xref_cus$1,                  /* Print Customer Xref Part   */~
            xref_mnf$1,                  /* Print Manufactur Xref Part */~
            hdr$40, msg$(3%)79           /* Askuser Messages (EWD001)  */
            

        dim ne_invplt$1,                 /* NE Invoice Plant     (AWD005) */~
            ne_invdesc$30                /* NE invoice plant desc (AWD005)*/

        dim f2%(32%),                    /* = 0 if the file is open    */~
            f1%(32%),                    /* = 1 if READ was successful */~
            fs%(32%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(32%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R6.04.02 11/13/95 Precious Metals         (EWD)   "
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
            * #01 ! CUSTOMER ! Customer Master File                     *~
/*AWD005*/  * #02 ! TXTCUST  ! System Text File                         *~
            * #05 ! CURMASTR ! Multi-Currency Master file               *~
            * #10 ! GENCODES ! General Codes File                       *~
            * #11 ! STXCODES ! Sales Tax Code File                      *~
            * #12 ! SLMMASTR ! Salesman master file                     *~
            * #13 ! ARMTERMS ! A/R Payment Terms Codes                  *~
            * #14 ! SYSFILE2 ! Caelus Management System General Informa *~
            * #15 ! GLMAIN   ! General Ledger Main File                 *~
            * #31 ! CCRMASTR ! Customer Credit Master file              *~
            *************************************************************~
            *       File Selection and Open Calls                       *~

            select #01,  "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #02,  "TXTCUST",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =   11

            select #05, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4

            select #10, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #11, "STXCODES",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  10

            select #12, "SLMMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   4

            select #13, "ARMTERMS",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  20

            select #14, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #15, "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            select #31, "CCRMASTR",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen =   9

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#01, fs%( 1%), f2%( 1%), 200%, rslt$( 1%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%),   0%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%),   0%, rslt$(11%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%),   0%, rslt$(12%))
            call "OPENCHCK" (#13, fs%(13%), f2%(13%),   0%, rslt$(13%))
            call "OPENCHCK" (#14, fs%(14%), f2%(14%),   0%, rslt$(14%))
            call "OPENCHCK" (#15, fs%(15%), f2%(15%),   0%, rslt$(15%))
            call "OPENCHCK" (#31, fs%(31%), f2%(31%), 200%, rslt$(31%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date : call "DATEFMT" (date$)

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            gosub init_enables
            call "READ100" (#14, "SWITCHS.COR", core_track%)/* SYSFILE2 */

*        See if User is an administator
            call "CMSMACHK" ("ARM", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then admin% = 1%

*        Read in defaults record
            customer$ = all(hex(00))
            call "READ100" (#01, customer$, defaults%)
            if defaults% = 0% then L09310
                get #01 using L09220, defaults$()
L09220:              FMT 10*CH(120)
            call "READ100" (#31, customer$, f1%(31%))
            if f1%(31%) = 0%                                             ~
                then put ccrdflts$ using L35700, customer$, 0, 0, 0, " ", ~
                     0, " ", 0, " ", 0, 0%, " ", " ", " ", " ", " ", 0,  ~
                     0, 0, 0, " ", " ", " "         /* Create Defaults */~
                else get #31 using L09290, ccrdflts$
L09290:              FMT CH(200)

L09310
*        Check for Multi-Currency
            curr_on_flag$ = "N" : curr_msg$ = " "
            call "READ100" (#14, "SWITCHS.CUR", f1%(14))
            if f1%(14) <> 0% then get #14 using L09350, curr_on_flag$
L09350:         FMT POS(21), CH(1)
            if curr_on_flag$ <> "Y" then L09400
               call "OPENCHCK" (#05, fs%( 5), f2%( 5),   0%, rslt$( 5))
               curr_msg$ = "Currency code"

L09400
*        Check for GENCODES existence            /*  (EWD003)  */
            divisionflag$, countryflag$, iccflag$ = "N"
            readkey$ = "DIVISION "
            call "PLOWNEXT" (#10, readkey$, 9%, f1%(10))
            if f1%(10) = 1% then divisionflag$ = "Y"

            readkey$ = "COUNTRYCD"
            call "PLOWNEXT" (#10, readkey$, 9%, f1%(10))
            if f1%(10) = 1% then countryflag$ = "Y"

            readkey$ = "CORPORATE"
            call "PLOWNEXT" (#10, readkey$, 9%, f1%(10))
            if f1%(10) = 1% then iccflag$ = "Y"

*        Check if Management Values are beening used
            mgtval_on$ = "N"
            icc_prompt$, icoc_prompt$ = " "
            call "READ100" (#14, "SWITCHS.GL", f1%(14))
            if f1%(14) = 1% then get #14 using L09590, mgtval_on$
L09590:         FMT POS(59), CH(1)
            if mgtval_on$ <> "Y" then L09640
                icc_prompt$  = "Intercompany Corp. Code"
                icoc_prompt$ = "Intercompany Ownership"

L09640
*        Check if Precious Metal Surcharge is on
            pm_on$ = "N"
            call "READ100" (#14, "SWITCHS.BCK", f1%(14%))
            if f1%(14%) = 1% then get #14 using L09670, pm_on$
L09670:         FMT POS(60), CH(1)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub delete_customer    /* (EWD001) - Now take out of Gencode */        
            gosub init_for_inputmode
            call "ALLFREE"

            for fieldnr% = 1% to 8%           /* (AWD005) */
L10110:         gosub'050(1%, fieldnr%, 1%)
                      if enabled% = 0% then L10260
                gosub'051(fieldnr%)     /* Check Enables, Set Defaults*/
L10140:         gosub'101(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10230
                         if fieldnr% = 1% then L10230
L10180:                  fieldnr% = max(2%, fieldnr% - 1%)
                         gosub'050(1%, fieldnr%, 1%)
                         if enabled% = 1% then L10140
                         if fieldnr% = 2% then L10110
                         goto L10180
L10230:               if keyhit%  = 16 then       exit_program
                      if keyhit%  = 17 and fieldnr% = 1% then L10260
                      if keyhit% <>  0 then       L10140
L10260:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10140
            next fieldnr%

            for fieldnr% = 1% to 17%
                gosub'050(2%, fieldnr%, 1%) /* Set enables, input msg */
                     if fieldnr% = 1% and account% = 1% then L10500
                     if fieldnr% = 2% and billto%  = 1% then L10500
                     if fieldnr% = 3% and parent%  = 1% then L10500
                     if enabled% = 0% then L10480
L10360:         gosub'052(fieldnr%)     /* Set Defaults               */
                      if enabled% = 0% then L10480
L10380:         gosub'102(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10460
L10410:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'050(2%, fieldnr%, 1%)
                         if enabled% = 1% then L10380
                         if fieldnr% = 1% then L10360
                         goto L10410
L10460:               if keyhit%  = 16% then       exit_program
                      if keyhit% <>  0% then       L10380
L10480:         gosub'152(fieldnr%)
                      if errormsg$ <> " " then L10380
L10500:     next fieldnr%

            for fieldnr% = 1% to 18%
                gosub'050(3%, fieldnr%, 1%)
                      if enabled% = 0 then L10660
L10550:         gosub'053(fieldnr%)
L10560:         gosub'103(fieldnr%, 1%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10640
L10590:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'050(3%, fieldnr%, 1%)
                         if enabled% = 1% then L10560
                         if fieldnr% = 1% then L10550
                         goto L10590
L10640:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10560
L10660:         gosub'153(fieldnr%)
                      if errormsg$ <> " " then L10560
            next fieldnr%

            for fieldnr% = 1% to 9%
                gosub'050(4%, fieldnr%, 1%)
                      if enabled% = 0 then L10840
L10730:         gosub'054(fieldnr%)
L10740:         gosub'104(fieldnr%, 1%)
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10820
L10770:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'050(4%, fieldnr%, 1%)
                         if enabled% = 1% then L10740
                         if fieldnr% = 1% then L10730
                         goto L10770
L10820:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10740
L10840:         gosub'154(fieldnr%)
                      if errormsg$ <> " " then L10740
            next fieldnr%

*        Do Input for Variable Fields
            call "CUINPSUB" ("CUSTOMER", "I", "Manage Customer Master",  ~
                             str(line2$,,60), "NN", vf$, keyhit%)
            if keyhit% = 1% then inputmode


*        And Appendix File
            call "CUSAPPND" (3%, customer$, u3%)


        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            gosub'050(1%, 0%, 2%)       /* Set input message           */
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  3 then       editpg6
                  if keyhit%  =  5 then       editpg2
                  if keyhit%  = 10 then gosub core_tracking
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 25 then gosub edit_text
                  if keyhit%  = 26 then gosub show_cust_credit
                  if keyhit%  = 29 then       L11170
                  if keyhit% <>  0 then       editpg1
L11170:     fieldnr% = cursor%(1) - 3%
                if fieldnr% <> 1% or cursor%(2) < 46% then L11210
                     fieldnr%  = 7%             
                     goto L11260
L11210:         if fieldnr%  < 2 or  fieldnr% >  17 then editpg1
                if fieldnr% >= 3 and fieldnr% <=  8 then fieldnr% = 3
                if fieldnr% >= 9 and fieldnr% <= 14 then fieldnr% = 4
                if fieldnr% = 15 then fieldnr% = 5
                if fieldnr% = 16 then fieldnr% = 6
                if fieldnr% = 17 then fieldnr% = 8   /*AWD005*/
L11260:         if fieldnr% = lastfieldnr% then editpg1
            if keyhit% <> 29% then L11300
                gosub'049(1%, fieldnr%)
                goto editpg1
L11300:     gosub'050(1%, fieldnr%, 2%) /* Set input message, enables  */
                  if enabled% = 0% then       editpg1
L11320:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11320
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11320
                          lastfieldnr% = fieldnr%
                          goto L11170

        editpg2
            gosub'050(2%, 0%, 2%)
            lastfieldnr% = 0%
            gosub'102(0%, 2%)
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then       editpg1
                  if keyhit%  =  3% then       editpg6
                  if keyhit%  =  4% then       editpg1
                  if keyhit%  =  5% then       editpg3
                  if keyhit%  = 10% then gosub core_tracking
                  if keyhit%  = 16% then       datasave
                  if keyhit%  = 25% then gosub edit_text
                  if keyhit%  = 26% then gosub show_cust_credit
                  if keyhit%  = 29% then       L11530
                  if keyhit% <>  0% then       editpg2
L11530:     fieldnr% = cursor%(1%) - 3%
                if fieldnr% = 16% then fieldnr% = 17%
                if fieldnr% = 15% and cursor%(2) >32% then fieldnr% = 16%
                if fieldnr% < 1% or fieldnr% > 17% then editpg2
                if fieldnr% = lastfieldnr% then editpg2
                if fieldnr% = 1% and account% = 1% then editpg2
                if fieldnr% = 2% and billto%  = 1% then editpg2
                if fieldnr% = 3% and parent%  = 1% then editpg2
            if keyhit% <> 29% then L11630
                gosub'049(2%, fieldnr%)
                goto editpg2
L11630:     gosub'050(2%, fieldnr%, 2%)
                  if enabled% = 0% then       editpg2
L11650:     gosub'102(fieldnr%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11650
            gosub'152(fieldnr%)
                  if errormsg$ <> " " then L11650
                     lastfieldnr% = fieldnr%
                     goto L11530

        editpg3
            gosub'050(3%, 0%, 2%)
            lastfieldnr% = 0%
            gosub'103(0%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       editpg1
                  if keyhit%  =  3 then       editpg6
                  if keyhit%  =  4 then       editpg2
                  if keyhit%  =  5 then       editpg4
                  if keyhit%  = 10 then gosub core_tracking
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 25 then gosub edit_text
                  if keyhit%  = 26 then gosub show_cust_credit
                  if keyhit%  = 29 then       L11860
                  if keyhit% <>  0 then       editpg3
L11860:     fieldnr% = cursor%(1) - 3%
                if fieldnr% =  9 then fieldnr% = 8%
                if fieldnr% = 10 then fieldnr% = 8%
                if fieldnr% > 10 then fieldnr% = fieldnr% - 2%
                if fieldnr% = 15% then fieldnr% = 18% /* Last Field-ICOC*/
                if fieldnr% = 14% and cursor%(2%) >23% then fieldnr% = 15%
                if fieldnr% = 15% and cursor%(2%) >47% then fieldnr% = 16%
                if fieldnr% = 16% and cursor%(2%) >54% then fieldnr% = 17%
                if fieldnr% < 1 or fieldnr% > 18 then editpg3
                if fieldnr% = lastfieldnr% then editpg3
            if keyhit% <> 29% then L11950
                gosub'049(3%, fieldnr%)
                goto editpg3
L11950:     gosub'050(3%, fieldnr%, 2%)
                  if enabled% = 0% then       editpg3
L11970:     gosub'103(fieldnr%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11970
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11970
                     lastfieldnr% = fieldnr%
                     goto L11860

        editpg4
            gosub'050(4%, 0%, 2%)
            lastfieldnr% = 0%
            gosub'104(0%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       editpg1
                  if keyhit%  =  3 then       editpg6
                  if keyhit%  =  4 then       editpg3
                  if keyhit%  =  5 then       editpg5
                  if keyhit%  = 10 then gosub core_tracking
                  if keyhit%  = 16 then       datasave
                  if keyhit%  = 25 then gosub edit_text
                  if keyhit%  = 26 then gosub show_cust_credit
                  if keyhit%  = 29 then       L12180
                  if keyhit% <>  0 then       editpg4
L12180:     fieldnr% = cursor%(1%) - 4%
                if fieldnr% < 1% or fieldnr% >  9% then editpg4
                if fieldnr% = lastfieldnr% then editpg4
            if keyhit% <> 29% then L12240
                gosub'049(4%, fieldnr%)
                goto editpg4
L12240:     gosub'050(4%, fieldnr%, 2%)
                  if enabled% = 0% then       editpg4
L12260:     gosub'104(fieldnr%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12260
            gosub'154(fieldnr%)
                  if errormsg$ <> " " then L12260
                     lastfieldnr% = fieldnr%
                     goto L12180

        editpg5      /* Edit Appendix file         */
            if str(customer$,,1) = hex(00) then L12380
            call "CUSAPPND" (4%, customer$, u3%)
            if u3% <> 99% then L12400
L12380:         if keyhit% = 4% then editpg4   /* Ignore appendix call */
                if keyhit% = 5% then editpg6
L12400:     if u3%  = 1% then inputmode        /* Branch per pf key    */
            if u3%  = 2% then editpg1          /* returned in U3%      */
            if u3%  = 3% then editpg6
            if u3%  = 4% then editpg4
            if u3%  = 5% then editpg6
            if u3% = 16% then datasave

        editpg6      /* Edit Variable Fields       */
            call "CUINPSUB" ("CUSTOMER", "E", "Manage Customer Master",  ~
                             str(line2$,,60), "YY", vf$, keyhit%)
            gosub edit_fax                     /* (EWD) - Check Fax No */
            if keyhit% =  1% then inputmode
            if keyhit% =  4% then editpg5
            if keyhit% =  5% then editpg1
            if keyhit% = 16% then datasave
                             goto editpg1
        edit_fax                               /* (EWD) - Begin        */
            init(" ") errormsg$, rh$
            if str(vf$,1%,7%) <> "       " then goto L12500
               str(vf$,1%,20%) = " "
               return
L12500:     x1% = 0%
            rh$ = str(vf$,1%,20%)
            if len(rh$) < 7 or len(rh$) > 11 then goto L12600
            convert rh$ to x1%, data goto L12600

            return
L12600:        errormsg$ =hex(94)&"(Error)-Invalid FaX phone no."&hex(8c)
            return                             /* (EWD) - End          */   

        edit_text
            if str(customer$,,1) = hex(00) then return
            call "TXTINSUB" (#02, f2%(2), "012", str(line2$,,60),        ~
                                                       textid$, text$())
            a% = len(text$())                  /* (EWD) - Begin        */
            if a% < 2% then textid$ = all(hex(ff))
            cust_add$ = " "
            if textid$ <> hex(ffffffff) then                             ~
                          cust_add$ = "(Additional Customer Information)"  
            return                             /* (EWD) - End          */

        show_cust_credit
            if retrieved% = 0% then return
            if customer$ = hex(000000000000000000) then return
            call "ARQCUSCR" (customer$)
            return
 
        core_tracking
            call "CORPARSB" (customer$)
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            if str(customer$,,1) <> hex(00) then L19170
*        See if we are to save defaults record to file.
            savedefaults% = 0%
            if admin% <> 1% then L19170
L19110:         savedefaults% = 2%
                call "ASKUSER" (savedefaults%, "SAVE DEFAULTS ON FILE?", ~
                     "PRESS PF-16 TO SAVE Defaults on file", "- OR -",   ~
                     "RETURN to use for current session only.")
                if savedefaults% <> 16% and savedefaults% <> 0% then L19110

L19170
*        Now execute the save data logic
            gosub save_data
            gosub delete_customer    /* (EWD001) - Now take out of Gencode */
            goto inputmode


        REM *************************************************************~
            *            D E F A U L T S  F O R   P A G E   1           *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS for Screen  1 of Input.                     *~
            *************************************************************

            deffn'051(fieldnr%)
                  on fieldnr% gosub L20160,         /* Customer Code    */~
                                    L20190,         /* Sort Name        */~
                                    L20220,         /* Ship-to          */~
                                    L20250,         /* Sold-to          */~
                                    L20280,         /* Division Code    */~
                                    L20310,         /* Country Code     */~
                                    L20340,         /* ICC Code         */~
                                    L20380          /* NE Fields        */
                     return

L20160
*        Customer Code                         CUSTOMER$
            return

L20190
*        Customer Sort Name                    SORT$
            return

L20220
*        SHIP-TO Name and Address              SHIPTO$
            return

L20250
*        SOLD-TO Name and Address              SOLDTO$
            return

L20280
*        Division Code                         DIVISION$          EWD003
            return

L20310
*        Country Code                          COUNTRY$
            return

L20340
*        Intercompany Corparation Code         ICC$
            return

L20380
*        New North East Invoice plant field      NE_INVPLT$      (AWD005)              
                    ne_invplt$ = "1"
            return
        REM *************************************************************~
            *            D E F A U L T S  F O R   P A G E   2           *~
            *-----------------------------------------------------------*~
            * Sets DEFAULT fields for Screen 2 of Input.                *~
            *************************************************************

            deffn'052(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L21270,         /* Account X-Ref    */~
                                    L21290,         /* Bill-to X-Ref    */~
                                    L21310,         /* Credit Parent    */~
                                    L21330,         /* Customer Contact */~
                                    L21350,         /* Phone Number/Fax */~
                                    L21370,         /* PO Required?     */~
                                    L21390,         /* Std Price Code   */~
                                    L21400,         /* Price Code/Option*/~
                                    L21410,         /* Order Discount   */~
                                    L21425,         /* Shipping Priority*/~
                                    L21430,         /* How Ship         */~
                                    L21450,         /* FOB              */~
                                    L21470,         /* Shipping Region  */~
                                    L21550,         /* Export Customer? */~
                                    L21490,         /* Allow Backorders?*/~
                                    L21510,         /* Allow Late Ship? */~
                                    L21530          /* Shipping Instr   */
                     return

L21270
*        Account Cross Reference               ACCTXREF$
            return
L21290
*        Bill-to Cross Reference               BILLXREF$
            return
L21310
*        Credit Parent                         CRPARENT$
            return
L21330
*        Customer Contact                      CONTACT$
            return
L21350
*        Phone Number & Fax                    PHONE$()
            return
L21370
*        PO Required?                          POREQD$
            return
L21390
*        Standard Price Code                   PC$
            return
L21400
*        Price code for options                PCO$
            return
L21410
*        Standard Order Discount %             DISC$
            return
L21425
*        Shipping Priority Code                SHIPCODE$
            return
L21430
*        How Ship                              HOWSHIP$
            return
L21450
*        FOB                                   FOB$
            return
L21470
*        Shipping Region Code                  SHIPRGN$
            return
L21490
*        Allow Backorders?                     BO$
            return
L21510
*        Allow Late Shipments?                 LATESHIP$
            return
L21530
*        Shipping Instructions                 SHIPINSTR$
            return
L21550
*        Normally an Export Customer?          EXPORT$
            return

        REM *************************************************************~
            *           D E F A U L T S   F O R   P A G E   3           *~
            *-----------------------------------------------------------*~
            * Sets DEFAULT fields for Screen 3 of Input.                *~
            *************************************************************

            deffn'053(fieldnr%)
                  on fieldnr% gosub L22230,         /* Customer Status  */~
                                    L22250,         /* Currency code    */~
                                    L22270,         /* Tax Exemption #  */~
                                    L22290,         /* Sales Tax Code   */~
                                    L22310,         /* Customer Taxable?*/~
                                    L22330,         /* Customer Type    */~
                                    L22350,         /* Sales Region Code*/~
                                    L22370,         /* Salesmen/ Comm % */~
                                    L22390,         /* Payment Terms    */~
                                    L22410,         /* Credit Limit     */~
                                    L22430,         /* Credit # days    */~
                                    L22450,         /* FC Table         */~
                                    L22470,         /* Balance Type     */~
                                    L22490,         /* Doc Printing     */~
                                    L22540,         /* Print C Xref Part*/~
                                    L22560,         /* Print M Xref Part*/~
                                    L22580,         /* Print Acknowledge*/~
                                    L22510          /* ICOC Code        */
                     return

L22230
*        Customer Status                       STATUS$
            return
L22250
*        Currency code                         CURRENCY$
            return
L22270
*        Tax Exemption Number                  EXEMPT$
            return
L22290
*        Sales Tax Code                        TAXCODE$
            return
L22310
*        Customer Taxable?                     TAXABLE$
            return
L22330
*        Customer Type Code                    TYPE$
            return
L22350
*        Sales Region Code                     REGION$
            return
L22370
*        Salesmen/ Commission Split            SALESMEN$(3)
            return
L22390
*        Payment Terms (Code)                  TERMS$
            return
L22410
*        Credit Limit                          LIMIT$
            return
L22430
*        Credit # days                         NBRDAYS$
            return
L22450
*        Finance Charge Table                  FCTABLE$
            return
L22470
*        Balance Type                          BAL$
            return
L22490
*        Document Printing                     DOCS$
            return
L22510
*        Intercompany Ownership Code           ICOC$
            return
L22540
*        Print Customer Xref Parts             XREF_CUS$
            return
L22560
*        Print Manufactr Xref Parts            XREF_MNF$
            return
L22580
*        Print Acknowledgements                ACKS$
            return

        REM *************************************************************~
            *           D E F A U L T S   F O R   P A G E   4           *~
            *-----------------------------------------------------------*~
            * Sets DEFAULT fields for Screen 4 of Input.                *~
            *************************************************************

            deffn'054(fieldnr%)
                  on fieldnr% gosub L23160,         /* Sales Distr      */~
                                    L23180,         /* Sales Discounts  */~
                                    L23200,         /* Net Invoice Distr*/~
                                    L23220,         /* Cash-in-Bank Acct*/~
                                    L23240,         /* Freight Account  */~
                                    L23260,         /* Sales Tax Account*/~
                                    L23280,         /* FC Acct          */~
                                    L23310,         /* PM SO Flag       */~
                                    L23360          /* PM INV Flag      */
                     return

L23160
*        Sales Distribution Account            SALES$
            return
L23180
*        Sales Discounts Account               DISCACCT$
            return
L23200
*        Net Invoice Distribution              ARACCT$
            return
L23220
*        Cash-in-Bank Account                  CASHACCT$
            return
L23240
*        Freight Account                       FRTACCT$
            return
L23260
*        Sales Tax Account                     TAXACCT$
            return
L23280
*        Finance Charge Account                FCACCT$
            return

L23310
*        Default/Enable for Precious Metal Surcharge at SO Flag
            return

L23360
*        Default/Enable for Precious Metal Surcharge at INV Flag
            return

        REM *************************************************************~
            *        I N P U T   M E S S A G E S  &  E N A B L E S      *~
            * --------------------------------------------------------- *~
            * Sets Enable Flag, Input Message, and Standard PF Keys.    *~
            *************************************************************
        deffn'050(s%, f%, edit%)  /* EDIT%: 1=Input Mode; 2=Edit Mode */
            if f% <> 0% then L27080
                inpmessage$ = edtmessage$
                goto L27208

L27080
*        First Define the Input Message
            r% = scr%(s%, f%)  /* Get sequential field number          */
            restore line = L27480, r%     /* Position for Read          */
            read inpmessage$             /* Read Input Message         */

*        Now set the Field Enable Flag
            call "ENABLSUB" ("SET", "CUSINPUT", scr%(), set%(), s%, f%,  ~
                             edit%, enabled%)
            if curr_on_flag$ <> "Y" and s% = 3% and f% = 2%              ~
                                                      then enabled% = 0%
            if str(customer$,,1) = hex(00) and s% = 1% and f% = 2%       ~
                                                      then enabled% = 0%
            if mgtval_on$ = "Y" then L27194
                if s% = 1% and f% = 7% then enabled% = 0%
                if s% = 3% and f% = 18% then enabled% = 0%

L27194:     if pm_on$ = "Y" then L27208
                if s% = 4% and f% = 8% then enabled% = 0%
                if s% = 4% and f% = 9% then enabled% = 0%

L27208
*        And, finally, set up the standard PF Key display and keys.
            line2$ = "Customer: " & customer$ & " (" & sort$ & ")"
            str(line2$,62%) = "CUSINPUT: " & str(cms2v$,,8%)

        if edit% = 2% then L27320         /* Input Mode                 */
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                 (4)Previous Field                "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "             (16)Exit Program"
           str(pfkeys$()) = hex(01ffff04ffffffffffffffff0dff0f10ffffff00)
           if fieldnr% > 2% then goto L27304
            str(pf$(2%),18%,17%) = " " : str(pfkeys$(),4%,1%) = hex(ff)
L27304:    return

L27320:  if f% > 0% then L27408           /* Edit Mode- Select Field    */
           pf$(1) = "(1)Start Over                            (12)Delet"&~
                    "e Customer   (13)Instructions"
           pf$(2) = "(2)First Screen  (4)Prev Screen          (25)Manag"&~
                    "e Text       (15)Print Screen"
           pf$(3) = "(3)Last Screen   (5)Next Screen          (26)Custo"&~
                    "mer Credit   (16)Save Data   "
           str(pfkeys$())=hex(0102030405ffffffffffff0c0dff0f10ff191d001a)
           if retrieved% <> 0% then goto L27384
                str(pf$(3%),42%,19%) = " " : str(pfkeys$(21%)) = hex(ff)
L27384:    

REM           if retrieved% = 1% and core_track% = 1% and str(customer$,,1) ~
REM                <> hex(00) then goto L27390
REM                str(pf$(1),42,15) = " " : str(pfkeys$(),10,1) = hex(ff)
L27390:    return

                                         /* Edit Mode- Field Enabled   */
L27408:    pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           str(pfkeys$()) = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

L27480: data                                                             ~
        /* Screen 1                                                    */~
         "Enter Customer Code or leave blank to see codes on file.     ",~
         "Enter Customer Sort Name.                                    ",~
         "Enter Ship-to Name and Address.                              ",~
         "Enter Sold-to, 'BILL-TO', or leave blank if same as ship-to. ",~
                                                                         ~
        /* Screen 2                                                    */~
         "Enter Account's Customer Code or leave blank if same.        ",~
         "Enter Bill-to's Customer Code or leave blank if same.        ",~
         "Enter Customer's Contact Name.                               ",~
         "Enter Customer's Phone Number and Fax Number.                ",~
         "Enter 'Y' if a PO is required from this customer, else 'N'.  ",~
         "Enter the Standard Price Code for this customer.             ",~
         "Enter the Standard Order Discount Percent for this customer. ",~
         "Enter How Ship instructions.                                 ",~
         "Enter standard FOB description.                              ",~
         "Enter Shipping Region Code or leave blank.                   ",~
         "Enter 'Y' if customer allows backorders, else enter 'N'.     ",~
         "Enter 'Y' if late shipments are allowed, else enter 'N'.     ",~
         "Enter Shipping Instructions for this customer.               ",~
                                                                         ~
        /* Screen 3                                                    */~
         "'A'=Active, 'I'=Inactive, 'H'=Hold, 'D'=Flagged for Delete.  ",~
         "Enter Customer's Sales Tax Exemption Number.                 ",~
         "Enter Sales Tax Code for this customer.                      ",~
         "Is this customer normally charged sales tax? (Y/N)           ",~
         "Enter Customer Type code or leave blank.                     ",~
         "Enter Sales Region Code for this customer.                   ",~
         "Enter Salesman Code(s) and percent of sale for commissions.  ",~
         "Enter Standard Payment Terms Description or Code.            ",~
         "Enter Customer's Credit Limit (used w/ Bill-tos only).       ",~
         "Enter Table ID for applying Finance Charges.                 ",~
         "Enter 'O' for Open Item -or- 'B' for Balance Forward.        ",~
         "'S'tatements Only; 'L'ate Notes Only; 'B'oth, -or- 'N'either.",~
                                                                         ~
        /* Screen 4                                                    */~
         "Enter Sales Distribution Account (may be left blank).        ",~
         "Enter Sales Discounts Account (may be left blank).           ",~
         "Enter Net Invoice Distribution (A/R) Account.                ",~
         "Enter Cash-in-Bank Account (used for cash invoices).         ",~
         "Enter Freight Account (may be left blank).                   ",~
         "Enter Sales Tax Account (may be left blank).                 ",~
         "Enter Finance Charge Account (may be left blank).            ",~
                                                                         ~
        /* And the after-thoughts                                      */~
         "Enter Customer's Default Currency Code.                      ",~
         "Enter this customer's 'Credit Parent' cross-reference.       ",~
         "Enter Customer's Credit number of days before Credit Hold.   ",~
         "Enter Customer's Division Code or leave blank.               ",~
         "Enter Customer's Country Code or leave blank.                ",~
         "Enter Intercompany Corporation Code or leave blank.          ",~
         "Enter Intercompany Ownership Code or leave blank.            ",~
         "Enter 'Y' if customer is normally an Export customer.        ",~
         "Enter the Default Shipping Priority Code for this customer. (1=~
        ~High; 5=Low)",                                                   ~
         "Enter the Price Code for Options Sold to this Customer.      ",~
         "Enter 'Y' if Precious Metal Surcharge are to be Added at SO Ent~
        ~ry"                                                             ,~
         "Enter 'Y' if Precious Metal Surcharge are to be Updated at Invo~
        ~icing."                                                         ,~
         "Enter 'N'=Print CMS Only, 'Y'=Print Cust Xref Parts, 'B'=Print ~
        ~Both Xref & CMS."                                               ,~
         "Enter 'N'=Print CMS Only, 'Y'=Print Mnfct Xref Parts, 'B'=Print~
        ~ Both Xref & CMS."                                              ,~
         "Enter 'Y' to Print Acknowledgements for Sales Orders.",         ~
         "Enter Invoice Plant  ."  /*(AWD005)*/

        REM *************************************************************~
            *         I N I T I A L I Z E   V A R I A B L E S           *~
            * --------------------------------------------------------- *~
            * Initialize variables prior to Input Mode.                 *~
            *************************************************************
        init_for_inputmode

            init(" ") errormsg$, inpmessage$, currency$, currdesc$,      ~
                      customer$, sort$, soldto$(), shipto$(), pco$,      ~
                      acctxref$, billxref$, contact$, phone$(), poreqd$, ~
                      pc$, disc$, howship$, fob$, shiprgn$, bo$, export$,~
                      lateship$, shipinstr$(), status$, exempt$,         ~
                      taxcode$, taxable$, type$, region$, salesmen$(),   ~
                      comm$(), terms$, limit$, fctable$, bal$, docs$,    ~
                      sales$, discacct$, aracct$, cashacct$, frtacct$,   ~
                      taxacct$, fcacct$, opened$, lastused$, lastinv$,   ~
                      lastcash$, lastchng$, lastuser$, crchng$, cruser$, ~
                      baldate$, vf$, crparent$, crpardescr$, nbrdays$,   ~
                      acctxrefdescr$, billxrefdescr$, pcdescr$, cdchng$, ~
                      shiprgndescr$, statusdescr$, regiondescr$, cduser$,~
                      termsdescr$, typedescr$, salesdescr$, pcodescr$,   ~
                      discacctdescr$, aracctdescr$, cashacctdescr$,      ~
                      frtacctdescr$, taxacctdescr$, fcacctdescr$,        ~
                      fctabledescr$, taxcodedescr$, salesmendescr$(),    ~
                      division$, divisiondesc$, country$, countrydesc$,  ~
                      icc$, icoc$, icoc_desc$, hicldate$, shipcode$,     ~
                      pm_so$, pm_inv$, xref_cus$, xref_mnf$, acks$,      ~
                      cust_add$, rhh$,                         /* (EWD) */~
                      ne_invplt$ /*AWD005 */
                                                           /*  (EWD003)  */

            retrieved% = 0%
            mat bals  = zer
            mat comm% = zer
            hiclimit, limit, origlimit = 0
            textid$ = all(hex(ff))
            call "TXTFUTIL" (#02, f2%(2), "INTL", textid$)
            return

        REM *************************************************************~
            *         I N I T I A L I Z E   E N A B L E S               *~
            * --------------------------------------------------------- *~
            * Initialize Soft Enable Settings.                          *~
            *************************************************************
        init_enables
*        Define Screen, Field Cross Ref and Field Enable Settings.
            mat set% = con   : mat set% = (99%) * set%
            mat scr% = zer
            scr%(1, 1) =  1% : set%( 1) = 13%      /* Customer Code    */
            scr%(1, 2) =  2% : set%( 2) =  2%      /* Sort Name        */
            scr%(1, 3) =  3% : set%( 3) =  2%      /* Sold-to          */
            scr%(1, 4) =  4% : set%( 4) =  2%      /* Ship-to          */
/*EWD003*/  scr%(1, 5) = 40% : set%(40) =  2%      /* Division Code    */
            scr%(1, 6) = 41% : set%(41) =  2%      /* Country Code     */
            scr%(1, 7) = 42% : set%(42) =  2%      /* ICC Code         */
/*AWD005*/  scr%(1, 8) = 52% : set%(52) =  2%      /* Invoice Plant    */

            scr%(2, 1) =  5% : set%( 5) =  2%      /* Acct Xref        */
            scr%(2, 2) =  6% : set%( 6) =  2%      /* Bill-to Xref     */
            scr%(2, 3) = 38% : set%(38) =  2%      /* Credit Parent    */
            scr%(2, 4) =  7% : set%( 7) =  2%      /* Contact          */
            scr%(2, 5) =  8% : set%( 8) =  2%      /* Phone Number     */
            scr%(2, 6) =  9% : set%( 9) =  2%      /* PO Required      */
            scr%(2, 7) = 10% : set%(10) =  2%      /* Price Code       */
            scr%(2, 8) = 46% : set%(46) =  2%      /* Options Price Cde*/
            scr%(2, 9) = 11% : set%(11) =  2%      /* Order Disc %     */
            scr%(2,10) = 45% : set%(45) =  2%      /* Ship Priority    */
            scr%(2,11) = 12% : set%(12) =  2%      /* How Ship         */
            scr%(2,12) = 13% : set%(13) =  2%      /* FOB              */
            scr%(2,13) = 14% : set%(14) =  2%      /* Shipping Region  */
            scr%(2,14) = 44% : set%(44) =  2%      /* Export Customer? */
            scr%(2,15) = 15% : set%(15) =  2%      /* Allow B/Os?      */
            scr%(2,16) = 16% : set%(16) =  2%      /* Allow Late Ship? */
            scr%(2,17) = 17% : set%(17) =  2%      /* Shipping Instr   */

            scr%(3, 1) = 18% : set%(18) =  2%      /* Customer Status  */
            scr%(3, 2) = 37% : set%(37) =  2%      /* Currency code    */
            scr%(3, 3) = 19% : set%(19) =  2%      /* Tax Exempt #     */
            scr%(3, 4) = 20% : set%(20) =  2%      /* Sales Tax Code   */
            scr%(3, 5) = 21% : set%(21) =  2%      /* Cust Taxable?    */
            scr%(3, 6) = 22% : set%(22) =  2%      /* Type Code        */
            scr%(3, 7) = 23% : set%(23) =  2%      /* Sales Region     */
            scr%(3, 8) = 24% : set%(24) =  2%      /* Salesmen/Comm%   */
            scr%(3, 9) = 25% : set%(25) =  2%      /* Payment Terms    */
            scr%(3,10) = 26% : set%(26) =  2%      /* Credit Limit     */
            scr%(3,11) = 39% : set%(39) =  2%      /* Credit # days    */
            scr%(3,12) = 27% : set%(27) =  2%      /* FC Table         */
            scr%(3,13) = 28% : set%(28) =  2%      /* Balance Type     */
            scr%(3,14) = 29% : set%(29) =  2%      /* Doc Printing     */
            scr%(3,15) = 49% : set%(49) =  2%      /* Print C XRef Part*/
            scr%(3,16) = 50% : set%(50) =  2%      /* Print M XRef Part*/
            scr%(3,17) = 51% : set%(51) =  2%      /* Print Acknowledge*/
            scr%(3,18) = 43% : set%(43) =  2%      /* ICOC Code        */

            scr%(4, 1) = 30% : set%(30) =  2%      /* GL-Sales         */
            scr%(4, 2) = 31% : set%(31) =  2%      /*   -Sales Discs   */
            scr%(4, 3) = 32% : set%(32) =  2%      /*   -Net Invoice   */
            scr%(4, 4) = 33% : set%(33) =  2%      /*   -Cash-in-Bank  */
            scr%(4, 5) = 34% : set%(34) =  2%      /*   -Freight       */
            scr%(4, 6) = 35% : set%(35) =  2%      /*   -Sales Tax     */
            scr%(4, 7) = 36% : set%(36) =  2%      /*   -Fin Charges   */
            scr%(4, 8) = 47% : set%(47) =  2%      /* PM SO Flag       */
            scr%(4, 9) = 48% : set%(48) =  2%      /* PM INV Flag      */
*        The next available item is number 53.     /*AWD005*/

            call "ENABLSUB" ("INIT", "CUSINPUT", scr%(), set%(),         ~
                                                         0%, 0%, 0%, 0%)
            return

        REM *************************************************************~
            *           R E S E T   S O F T   E N A B L E S             *~
            * --------------------------------------------------------- *~
            * Allow User to modify enable settings.                     *~
            *************************************************************
        deffn'049(s%, f%)      /* Screen and Field Numbers             */
            if admin% <> 1% then return            /* Not Authorized   */
            call "ENABLSUB" ("MODIFY", "CUSINPUT", scr%(), set%(),       ~
                              s%, f%, 0%, 0%)
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
        load_data
            retrieved% = 0%    /* Indicate not retrieved from CUSTOMER */
            if keyhit% = 17% then customer$ = all(hex(00)) /* Defaults */
            if keyhit% = 17% then sort$     = "** DEFAULTS **"

            gosub check_customer          /* (EWD001) - Check for Customer */
            if cusaccess% <> 0% then gosub check_cust_error /* in Gencodes */
            if cusaccess% <> 0% then goto inputmode
            gosub add_customer           /* (EWD001) - Add Cust to Gencode */
            
            call "READ100" (#01, customer$, onfile%)
            if onfile% = 1% then L30080
                if keyhit% = 17% or defaults% = 0% then return
                     put #01 using L30065, defaults$()
L30065:                   FMT 10*CH(120)
                     put #31 using L30075, ccrdflts$
L30075:                   FMT CH(200)
L30080:     get #01 using L35060, temp$, sort$, soldto$(), opened$,       ~
                pm_so$, pm_inv$, phone$(2%), acks$,                      ~
                lastuser$, shipto$(), contact$, phone$(1%), sales$,      ~
                aracct$, cashacct$, frtacct$, discacct$, taxacct$, disc, ~
                pc$, limit, crchng$, cruser$, terms$, howship$, fob$,    ~
                shiprgn$, bo$, lateship$, shipinstr$(), salesmen$(),     ~
                comm%(), region$, shipcode$, pco$, storexref$,           ~
                acctxref$, billxref$, textid$, status$,                  ~
                taxable$, exempt$, vf$, poreqd$, bal$, docs$, type$,     ~
                taxcode$, fcacct$, fctable$, currency$, crparent$,       ~
                nbrdays%, cdchng$, cduser$, division$, country$,         ~
                icc_d$, icoc_d$, export$, xref_cus$, xref_mnf$,          ~
                ne_invplt$, temp$     /* (AWD005) */
                                                          /*  (EWD003)  */
            if onfile% = 1% and copy% = 0% then retrieved% = 1%
            if acks$ = " " then acks$ = "Y"
            lastused$, lastinv$, lastcash$, lastchng$, baldate$,         ~
                hicldate$ = " "
            mat bals = zer
            hiclimit = 0
            call "READ100" (#31, customer$, f1%(31%))
            if f1%(31%) <> 0% then get #31 using L35640, hiclimit,        ~
                hicldate$, lastused$, lastinv$, lastcash$, lastchng$,    ~
                baldate$, bals()
            icc$ = icc_d$ : icoc$ = icoc_d$
            if mgtval_on$ = "Y" then L30185
                init (" ") icc$, icoc$ /* Don't display if no mgt rpting*/
L30185:     gosub round_balance   /* 54200 */
            convert nbrdays% to nbrdays$, pic (##0)
            origdays% = nbrdays%
            origbillto$ = billxref$
            if copy% <> 1% then L30225
                if customer$ = acctxref$ then acctxref$ = scust$ /* Own */
                if customer$ = billxref$ then billxref$ = scust$ /*XREF.*/
                customer$ = scust$  /* Note- rest of resets done where */
                sort$     = ssort$  /*       copy was executed from.   */
                textid$   = all(hex(ff))
L30225:     if str(customer$,,1) = hex(00) then sort$ = "** DEFAULTS **"
            if onfile% <> 0% then L30260
                sort$, opened$, lastchng$, lastuser$, lastused$, cdchng$,~
                lastinv$, lastcash$, crchng$, cruser$, baldate$, cduser$,~
                origbillto$ = " "
                mat bals = zer
                textid$ = all(hex(ff))
L30260:     call "TXTFUTIL" (#02, f2%(2), "LOAD", textid$)
            call "CUSAPPND" (1%, customer$, u3%)
            call "DATEFMT" (opened$)
            call "DATEFMT" (lastused$)
            call "DATEFMT" (lastinv$)
            call "DATEFMT" (lastcash$)
            call "DATEFMT" (lastchng$)
            call "DATEFMT" (crchng$)
            call "DATEFMT" (cdchng$)
            call "DATEFMT" (baldate$)
            call "CONVERT" (disc   , 2.2, disc$   )
            call "CONVERT" (limit  , 0.0, limit$  )
                origlimit = limit
            if region$ = " " then L30340
                readkey$ = "REGIONS  " & region$
                call "DESCRIBE" (#10, readkey$, regiondescr$, 0%,f1%(10))
L30340:     for i% = 1% to 3%
                if salesmen$(i%) = " " then L30365
                     call "DESCRIBE" (#12, salesmen$(i%),                ~
                                         salesmendescr$(i%), 0%, f1%(12))
                     convert comm%(i%) to comm$(i%), pic(###)
L30365:     next i%
            acct$ =  sales$    : gosub describe_acct
                sales$    = acct$ : salesdescr$ = acctdescr$
            acct$ =  discacct$ : gosub describe_acct
                discacct$ = acct$ : discacctdescr$ = acctdescr$
            acct$ =  aracct$   : gosub describe_acct
                aracct$   = acct$ : aracctdescr$ = acctdescr$
            acct$ =  cashacct$ : gosub describe_acct
                cashacct$ = acct$ : cashacctdescr$ = acctdescr$
            acct$ =  frtacct$  : gosub describe_acct
                frtacct$  = acct$ : frtacctdescr$ = acctdescr$
            acct$ =  taxacct$  : gosub describe_acct
                taxacct$  = acct$ : taxacctdescr$ = acctdescr$
            acct$ =  fcacct$   : gosub describe_acct
                fcacct$   = acct$ : fcacctdescr$ = acctdescr$
            if pc$ = " " then L30451
                readkey$ = "PRICECODE" & pc$
                call "DESCRIBE" (#10, readkey$, pcdescr$, 0%, f1%(10))
L30451:     if pco$ = " " then L30453
                readkey$ = "PRICECODE" & pco$
                call "DESCRIBE" (#10, readkey$, pcodescr$, 0%, f1%(10))
L30453:     readkey$ = "CUST RPT " & customer$         /* (EWD)        */
                call "DESCRIBE" (#10, readkey$, rhh$,  0%, f1%(10%))
                if f1%(10%) <> 0% then pcdescr$ = rhh$ /* (EWD)        */  
            if shiprgn$ = " " then L30470
                readkey$ = "SHPREGION" & shiprgn$
                call "DESCRIBE" (#10, readkey$, shiprgndescr$, 0%,f1%(10))
L30470:     if type$ = " " then L30485
                readkey$ = "CUS TYPES" & type$
                call "DESCRIBE" (#10, readkey$, typedescr$, 0%, f1%(10))
L30485:     if status$ = "A" then statusdescr$ = "ACTIVE"
            if status$ = "H" then statusdescr$ = "ON HOLD"
            if status$ = "I" then statusdescr$ = "INACTIVE"
            if status$ = "D" then statusdescr$ = "TO BE DELETED"
            if status$ = "N" then statusdescr$ = "NEW ACCOUNT"     /*  (EWD004) */
            if status$ = "O" then statusdescr$ = "Obsolete Account" /* (AWD008) */
            call "DESCRIBE" (#11, taxcode$, taxcodedescr$, 0%, f1%(11))
            if curr_on_flag$ <> "Y" then currency$ = " "
            if curr_on_flag$ <> "Y" then L30525
                call "DESCRIBE" (#05, currency$, currdesc$, 0%, f1%(5))
L30525:     if crparent$ <> " " then                                     ~
                call "DESCRIBE" (#01, crparent$, crpardescr$, 0%, f1%(1))
            fctabledescr$ = "No Finance Charges"
            if fctable$ = " " then L30555
                readkey$ = "FINANCECHARGETABLE" & fctable$
                call "DESCRIBE" (#14, readkey$, fctabledescr$, 0%,f1%(14))
L30555:     termsdescr$ = "0% 0 Days, Net 0 Days"
            if terms$ = " " then L30570
            call "DESCRIBE" (#13, terms$, termsdescr$, 0%, f1%(13))
L30570:
*        See if this guy is used as an Account, Bill-to, and/or Credit
*        Parent.
            account% = 0%
            call "REDALT0" (#01, customer$, 3%, f1%(1)) /* Account Xref */
            if f1%(1) = 0% then L30640
L30600:         if key(#01) = customer$ then L30620
                     account% = 1%
                     acctxrefdescr$ = "** Referenced **"
                     goto L30640
L30620:         call "READNEXT" (#01, f1%(1))
                if f1%(1) = 0% then L30640
                     if key(#01, 3%) = customer$ then L30600

L30640:     billto% = 0%
            call "REDALT0" (#01, customer$, 4%, f1%(1))
            if f1%(1) = 0% then L30695
L30655:         if key(#01) = customer$ then L30675
                     billto% = 1%
                     billxrefdescr$ = "** Referenced **"
                     goto L30695
L30675:         call "READNEXT" (#01, f1%(1))
                if f1%(1) = 0% then L30695
                     if key(#01, 4%) = customer$ then L30655

L30695:     if acctxref$ = customer$ then acctxref$ = " "
            if acctxref$ = " " or account% = 1% then L30715
                call "DESCRIBE" (#01, acctxref$, acctxrefdescr$, 0%, u3%)

L30715:     if billxref$ = customer$ then billxref$ = " "
            if billxref$ = " " or billto% = 1% then L30735
                call "DESCRIBE" (#01, billxref$, billxrefdescr$, 0%, u3%)

L30735:     parent% = 0%
            call "REDALT0" (#01, customer$, 5%, f1%(1))
            if f1%(1) = 0% then L30790
L30750:         if key(#01) = customer$ then L30770
                     parent% = 1%
                     crpardescr$ = "** Referenced **"
                     goto L30790
L30770:         call "READNEXT" (#01, f1%(1))
                if f1%(1) = 0% then L30790
                     if key(#01, 4%) = customer$ then L30750

L30790:     if crparent$ = customer$ then crparent$ = " "
            if crparent$ = " " or parent% = 1% then L30805
                call "DESCRIBE" (#01, crparent$, crpardescr$, 0%, u3%)
L30805:     if division$ = " " then L30820             /*  (EWD003)  */
                readkey$ = "DIVISION " & division$
                call "DESCRIBE" (#10, readkey$, divisiondesc$, 0%,f1%(10))
L30820:     if country$ = " " then L30835
                readkey$ = "COUNTRYCD" & country$
                call "DESCRIBE" (#10, readkey$, countrydesc$, 0%,f1%(10))
L30835:     if icoc$ = " " then L30845
                call "DESCRIBE" (#01, icoc$, icoc_desc$, 0%, u3%)
L30845:     /*Test the PM vars for junk do to using a new filler area */
             if pm_so$ <> "Y" and pm_so$ <> "N" then pm_so$ = " "
             if pm_inv$ <> "Y" and pm_inv$ <> "N" then pm_inv$ = " "

            return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        save_data
            gosub check_access                         /* (EWD)      */
            if security% = 0% then goto L31999         /* (EWD)      */
 
            call "DATUNFMT" (opened$)
            call "DATUNFMT" (crchng$)
            call "DATUNFMT" (baldate$)
REM            if limit = origlimit then L31110
/* <AWD007 >*/
                crchng$ = date  :  cruser$ = userid$

L31110:     call "DATUNFMT" (cdchng$)
            if nbrdays% = origdays% then L31140
                cdchng$ = date  :  cduser$ = userid$
L31140:     call "GLUNFMT" (sales$    )
            call "GLUNFMT" (discacct$ )
            call "GLUNFMT" (aracct$   )
            call "GLUNFMT" (cashacct$ )
            call "GLUNFMT" (frtacct$  )
            call "GLUNFMT" (taxacct$  )
            call "GLUNFMT" (fcacct$   )
                                                              /* (EWD) */
            saveid$ = all(hex(00))                                          
            if textid$ <> hex(ffffffff) then saveid$ = str(textid$,1%,4%)
            
            if textid$ <> hex(ffffffff) then                              ~
               call "TXTFUTIL" (#02, f2%(2%), "WCPY", textid$) /* (EWD) */
            if str(customer$,,1) = hex(00) then L31240
                if acctxref$ = " " then acctxref$ = customer$
                if billxref$ = " " then billxref$ = customer$
L31240:     if str(customer$,,1) <> hex(00) then L31280
                acctxref$, billxref$ = " "
                sort$ = all(hex(00))

L31280:     call "READ101" (#01, customer$, f1%(1))
            if f1%(1) = 0% then opened$ = date
            if mgtval_on$ <> "Y" then L31320
                icc_d$ = icc$ : icoc_d$ = icoc$
L31320:     put #01 using L35060, customer$, sort$, soldto$(), opened$,   ~
                pm_so$, pm_inv$, phone$(2%), acks$,                      ~
                userid$, shipto$(), contact$, phone$(1%), sales$,        ~
                aracct$, cashacct$, frtacct$, discacct$, taxacct$, disc, ~
                pc$, limit, crchng$, cruser$, terms$, howship$, fob$,    ~
                shiprgn$, bo$, lateship$, shipinstr$(), salesmen$(),     ~
                comm%(), region$, shipcode$, pco$, storexref$,           ~
                acctxref$, billxref$, textid$, status$,                  ~
                taxable$, exempt$, vf$, poreqd$, bal$, docs$, type$,     ~
                taxcode$, fcacct$, fctable$, currency$, crparent$,       ~
                nbrdays%, cdchng$, cduser$, division$, country$, icc_d$, ~
/*AWD005*/      icoc_d$, export$, xref_cus$, xref_mnf$, ne_invplt$,      ~
                " "
                                                       /*  (EWD003)   */
            if str(customer$,,1) = hex(00) and savedefaults% <> 16%      ~
                then L31710
            if f1%(1) = 0% then write #01 else rewrite #01
            gosub delete_old_text                     /* (EWD002)  */

*        Now update fields in CCRMASTR the shadow file.
            lastused$, lastinv$, lastcash$, lastchng$, baldate$,         ~
                hicldate$ = " "
            mat bals = zer
            hiclimit = 0
            call "READ101" (#31, customer$, f1%(31%))
            if f1%(31%) <> 0% then get #31 using L35640, hiclimit,        ~
                hicldate$, lastused$, lastinv$, lastcash$, lastchng$,    ~
                baldate$, bals()
            if f1%(31%) = 1% then gosub round_balance
*        Update High Credit Limit as indicated.
            if customer$ = hex(000000000000000000) then goto L31610
            if limit <> 0 and limit <= hiclimit then goto L31610
                hiclimit = limit         /* Bump the High Credit Limit */
                hicldate$ = date    /* Bump the High Credit Limit Date */
L31610:     if f1%(31%) = 0%                                             ~
                then put #31 using L35700, customer$, 0, 0, hiclimit,     ~
                     hicldate$, 0, " ", 0, " ", 0, 0%, lastused$,        ~
                     lastinv$, lastcash$, date, baldate$, bals(),        ~
                     userid$, date, " "  /* Create new CCRMASTR record */~
                else put #31 using L35640, hiclimit, hicldate$, lastused$,~
                     lastinv$, lastcash$, date, baldate$, bals(),        ~
                     userid$, date/* Else update existing CCRMASTR rec */
            if f1%(31%) = 0% then write #31 else rewrite #31

L31710:     if str(customer$,,1) <> hex(00) then L31750
                get #01, str(defaults$())
                defaults% = 1%
                return
L31750:     call "TXTFUTIL" (#02, f2%(2), "TOS2", textid$)
            call "CUSAPPND" (2%, customer$, u3%)

*        Adjust Open Order Dollars on Bill-to if required.
            if origbillto$ = billxref$ or f1%(1) = 0% or bals(2) = 0     ~
                                                             then return
                call "READ101" (#31, origbillto$, f1%(31%))
                if f1%(31%) = 0% then L31880
                     get #31 using L31840, temp
L31840:                   FMT POS(114), PD(14,4)
                     temp = temp - bals(2)
                     put #31 using L31840, temp
                     rewrite #31
L31880:         call "READ101" (#31, billxref$, f1%(31%))
                if f1%(31%) = 0% then return
                     get #31 using L31840, temp
                     temp = temp + bals(2)
                     put #31 using L31840, temp
                     rewrite #31
                return
L31999: call "SHOSTAT" ("No Update, Changes Not Saved")    /* (EWD) */
        call "PAUSE" addr(500%)                            /* (EWD) */
        return                                             /* (EWD) */

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35060: FMT                 /* FILE #01 -- CUSTOMER                    */~
            CH(9),          /* Customer Code                           */~
            CH(30),         /* Sort Name                               */~
            6*CH(30),       /* Sold-to Name and Address                */~
            CH(6),          /* Date account opened                     */~
            CH(1),          /* Precious Metal SO entry Flag            */~
            CH(1),          /* Precious Metal Invoicing Flag           */~
            CH(10),         /* Fax number                              */~
            CH(1),          /* Print Acknowledgements at SO Entry Flag */~
            XX(11),         /* Filler (Dynamic Dates moved to CCRMASTR)*/~
            CH(3),          /* User ID of Last Modification            */~
            6*CH(30),       /* Ship-to Name and Address                */~
            CH(20),         /* Contact                                 */~
            CH(10),         /* Phone number                            */~
            CH(9),          /* Sales Account Number                    */~
            CH(9),          /* Receivables Account Number              */~
            CH(9),          /* Cash-in-Bank Account                    */~
            CH(9),          /* Freight Account                         */~
            CH(9),          /* Sales Discounts Account                 */~
            CH(9),          /* Sales tax account                       */~
            PD(14,4),       /* Order Discount Percent                  */~
            CH(1),          /* Pricing Code                            */~
            PD(14,4),       /* Credit Limit                            */~
            CH(6),          /* Date CR Limit last modified             */~
            CH(3),          /* User ID of Last Modification            */~
            CH(20),         /* Payment Terms                           */~
            CH(20),         /* How Ship Information                    */~
            CH(20),         /* FOB Information                         */~
            CH(9),          /* Shipping Region Code                    */~
            CH(1),          /* Allow Backorders Flag                   */~
            CH(1),          /* Allow Late Shipments flag               */~
            2*CH(50),       /* Shipping Instructions                   */~
            3*CH(4),        /* Salesman codes                          */~
            3*BI(1),        /* Percentage of Sale credited to salesman.*/~
            CH(4),          /* Sales Region Code                       */~
            CH(1),          /* Shipping Priority Code                  */~
            CH(1),          /* Price code for options                  */~
            CH(9),          /* Store Xref                              */~
            XX(21),         /* Filler (Balances move to CCRMASTR)      */~
            XX(6),          /* Filler (Dynamic Date move to CCRMASTR)  */~
            CH(9),          /* Account Xref                            */~
            CH(9),          /* Bill-to Xref                            */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(1),          /* Customer Status                         */~
            CH(1),          /* Customer Taxable?(Y/N)                  */~
            CH(25),         /* Tax Exemption Number                    */~
            CH(200),        /* Variable Fields Data Area               */~
            CH(1),          /* Purchase Order Required?                */~
            CH(1),          /* Balance Type                            */~
            CH(1),          /* Document Printing                       */~
            CH(2),          /* Customer Type                           */~
            CH(10),         /* Sales Tax Code                          */~
            CH(9),          /* Finance Charge G/L Account Number       */~
            CH(1),          /* Finance Charge Code                     */~
            CH(4),          /* Currency code                           */~
            CH(9),          /* Credit Parent                           */~
            BI(2),          /* Credit number of days                   */~
            CH(6),          /* Date # days last modified               */~
            CH(3),          /* User ID of Last Modification            */~
            CH(4),          /* Division Code                           */~
            CH(3),          /* Country Code                            */~
            CH(6),          /* Intercompany Corporation Code           */~
            CH(9),          /* Intercompany Ownership Code             */~
            CH(1),          /* Customer Normally an Export Customer?   */~
            CH(1),          /* Print Customer Xref Part Flag           */~
            CH(1),          /* Print Manufactr Xref Part Flag          */~
            CH(1),          /* North East Invoicing Plant (AWD005)     */~
            CH(106)         /* Filler                                  */

L35640:     FMT /* File #31- CCRMASTR Master file (partial; input)     */~
                POS(26), PD(14,4),/* 26/ 8- High Credit Limit $        */~
                CH(6),           /*  34/ 6- High Credit Limit Date     */~
                POS(84), 5*CH(6),/*  84/30- 'Dynamic' dates fr CUSTOMER*/~
                4*PD(14,4),      /* 114/32- 'Dynamic' amnts fr CUSTOMER*/~
                POS(146), CH(3), /* 146/ 3- User Last Modified         */~
                CH(6)            /* 149/ 6- Date Last Modified         */

L35700:     FMT /* File #31- CCRMASTR Master file (complete; write)    */~
                CH(9),         /*   1/ 9-   Customer Code (Key)        */~
                PD(14,4),      /*  10/ 8-   Total Sales                */~
                PD(14,4),      /*  18/ 8-   Total Credits              */~
                PD(14,4),      /*  26/ 8-   High Credit Limit          */~
                CH(6),         /*  34/ 6-   High Credit Limit Date     */~
                PD(14,4),      /*  40/ 8-   Last Invoice Amount        */~
                CH(8),         /*  48/ 8-   Last Invoice Number        */~
                PD(14,4),      /*  56/ 8-   Last Payment Amount        */~
                CH(10),        /*  64/10-   Last Payment Check #       */~
                PD(14,4),      /*  74/ 8-   Average # Days to Pay      */~
                BI(2),         /*  82/ 2-   # Invoices in Average Days */~
                5*CH(6),       /*  84/30-   'Dynamic' dates fr CUSTOMER*/~
                4*PD(14,4),    /* 114/32-   'Dynamic' amnts fr CUSTOMER*/~
                CH(3),         /* 146/ 3-   User Last Modified         */~
                CH(6),         /* 149/ 6-   Date Last Modified         */~
                CH(46)         /* 155/46-   Filler                     */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            if edit% <> 2% then L40088
                str(pf$(2),,31) = " " : pfkeys$(2), pfkeys$(4) = hex(ff)
L40088:     if edit% <> 1% or fieldnr% <> 1% then L40120
                str(pf$(1%),42%,20%) = "(17)Manage Defaults"
                pfkeys$(17) = hex(11)

L40120:     if edit% <> 1% or fieldnr% <> 3% then L40152
                str(pf$(3%),,20%) = "(3)Copy Customer"
                pfkeys$(3) = hex(03)

L40152:     on fieldnr% gosub       L40248,         /* Customer Code    */~
                                    L40248,         /* Sort Name        */~
                                    L40248,         /* Sold-to          */~
                                    L40248,         /* Ship-to          */~
                                    L40248,         /* Division Code    */~
                                    L40248,         /* Country Code     */~
                                    L40248,         /* ICC Code         */~
/*AWD005*/                          L40248          /* NE Invoice Plant */
            goto L40304

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L40248:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return
                                                       /* (EWD)       */      
L40304:     accept                                                       ~
               at (01,02), "Customer Master Management",                 ~
               at (01,54), "Page 1",                                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(40),~
                                                                         ~
               at (04,02), "Customer Code",                              ~
               at (04,30), fac(lfac$( 1)), customer$            , ch(09),~
               at (03,45), fac(hex(84)), cust_add$              , ch(35),~
                                                                         ~
               at (04,46), fac(hex(8c)),   icc_prompt$          , ch(23),~
               at (04,70), fac(lfac$( 7)), icc$                 , ch(06),~
                                                                         ~
               at (05,02), "Customer Sort Name",                         ~
               at (05,30), fac(lfac$( 2)), sort$                , ch(30),~
                                                                         ~
               at (06,02), "Ship-to Name and Address",                   ~
               at (06,30), fac(lfac$( 3)), shipto$(1)           , ch(30),~
               at (07,30), fac(lfac$( 3)), shipto$(2)           , ch(30),~
               at (08,30), fac(lfac$( 3)), shipto$(3)           , ch(30),~
               at (09,30), fac(lfac$( 3)), shipto$(4)           , ch(30),~
               at (10,30), fac(lfac$( 3)), shipto$(5)           , ch(30),~
               at (11,30), fac(lfac$( 3)), str(shipto$(6), 1,17), ch(17),~
               at (11,48), fac(lfac$( 3)), str(shipto$(6),19, 2), ch(02),~
               at (11,51), fac(lfac$( 3)), str(shipto$(6),22, 5), ch(05),~
               at (11,57), "-",                                          ~
               at (11,59), fac(lfac$( 3)), str(shipto$(6),27, 4), ch(04),~
                                                                         ~
               at (12,02), "Sold-to Name and Address",                   ~
               at (12,30), fac(lfac$( 4)), soldto$(1)           , ch(30),~
               at (13,30), fac(lfac$( 4)), soldto$(2)           , ch(30),~
               at (14,30), fac(lfac$( 4)), soldto$(3)           , ch(30),~
               at (15,30), fac(lfac$( 4)), soldto$(4)           , ch(30),~
               at (16,30), fac(lfac$( 4)), soldto$(5)           , ch(30),~
               at (17,30), fac(lfac$( 4)), str(soldto$(6), 1,17), ch(17),~
               at (17,48), fac(lfac$( 4)), str(soldto$(6),19, 2), ch(02),~
               at (17,51), fac(lfac$( 4)), str(soldto$(6),22, 5), ch(05),~
               at (17,57), "-",                                          ~
               at (17,59), fac(lfac$( 4)), str(soldto$(6),27, 4), ch(04),~
                                                                         ~
               at (18,02), "Atrium Division Code",                       ~
/* EWD003 */   at (18,30), fac(lfac$( 5)), division$            , ch(04),~
/* EWD003 */   at (18,48), fac(hex(8c)),   divisiondesc$        , ch(30),~
                                                                         ~
               at (19,02), "Customer's Country Code",                    ~
               at (19,30), fac(lfac$( 6)), country$             , ch(03),~
               at (19,48), fac(hex(8c)),   countrydesc$         , ch(30),~
                                                                         ~
/*(AWD005)*/   at (20,02), "Customer's Invoice Plant",                   ~
/*(AWD005)*/   at (20,30), fac(lfac$( 8)), ne_invplt$           , ch(01),~
/*(AWD005)*/   at (20,48), fac(hex(8c)),   ne_invdesc$          , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(str(pfkeys$())),                               ~
                     key (keyhit%)
                     
                     
               if keyhit% <> 12 then goto L40751
                  gosub delete_cust
                  return clear all
                  goto inputmode
               
L40751:        if keyhit% <> 13 then L40752
                  call "MANUAL" ("CUSINPUT")
                  goto L40304

L40752:        if keyhit% <> 15 then L40784
                  call "PRNTSCRN"
                  goto L40304

L40784:     if edit% = 2% or keyhit% <> 3% then L40920
              if fieldnr% <> 3% then L40304
                copycus$ = " "
                misc$ = hex(06) & "Please Select Customer To Copy"
                call "GETCODE" (#01, copycus$, misc$, 0%, 0, f1%(1))
                if f1%(1) = 0% then L40304
                     scust$    = customer$  :  ssort$ = sort$
                     customer$ = copycus$
                     copy%     = 1% : gosub load_data : copy% = 0%
                     opened$, lastchng$, lastuser$, lastused$, lastinv$, ~
                     lastcash$, crchng$, cruser$, baldate$, cdchng$,     ~
                     cduser$ = " "
                     onfile% = 0%
                     mat bals = zer
                     return clear all
                     goto editpg1

L40920:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            on fieldnr% gosub       L41232,         /* Account X-Ref    */~
                                    L41232,         /* Bill-to X-Ref    */~
                                    L41232,         /* Credit Parent    */~
                                    L41208,         /* Customer Contact */~
                                    L41232,         /* Phone Number/Fax */~
                                    L41232,         /* PO Required?     */~
                                    L41232,         /* Std Price Code   */~
                                    L41232,         /* Opt Price Code   */~
                                    L41256,         /* Order Discount   */~
                                    L41256,         /* Shipping Priority*/~
                                    L41208,         /* How Ship         */~
                                    L41208,         /* FOB              */~
                                    L41232,         /* Shipping Region  */~
                                    L41232,         /* Export Customer? */~
                                    L41232,         /* Allow Backorders?*/~
                                    L41232,         /* Allow Late Ship? */~
                                    L41208          /* Shipping Instr   */
            goto L41288

L41208:           REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L41232:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L41256:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return
                                                        /* (EWD)      */
L41288:     accept                                                       ~
               at (01,02), "Customer Master Management",                 ~
               at (01,54), "Page 2",                                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(40),~
               at (03,45), fac(hex(84)), cust_add$              , ch(35),~
                                                                         ~
               at (04,02), "Account Cross Reference",                    ~
               at (04,30), fac(lfac$( 1)), acctxref$            , ch(09),~
/* CR1855 */ ~
               at (04,40), fac(hex(8c)),   storexref$           , ch(09),~
               at (04,49), fac(hex(8c)),   acctxrefdescr$       , ch(32),~
                                                                         ~
               at (05,02), "Bill-to Cross Reference",                    ~
               at (05,30), fac(lfac$( 2)), billxref$            , ch(09),~
               at (05,49), fac(hex(8c)),   billxrefdescr$       , ch(32),~
                                                                         ~
               at (06,02), "Credit Parent X-ref",                        ~
               at (06,30), fac(lfac$( 3)), crparent$            , ch(09),~
               at (06,49), fac(hex(8c)),   crpardescr$          , ch(32),~
                                                                         ~
               at (07,02), "Customer Contact",                           ~
               at (07,30), fac(lfac$( 4)), contact$             , ch(20),~
                                                                         ~
               at (08,02), "Phone Number",                               ~
               at (08,30), fac(lfac$( 5)), str(phone$(1%), 1, 3), ch(03),~
               at (08,34), fac(lfac$( 5)), str(phone$(1%), 4, 3), ch(03),~
               at (08,38), fac(lfac$( 5)), str(phone$(1%), 7, 4), ch(04),~
               at (08,45), "Fax  ",                                      ~
               at (08,50), fac(lfac$( 5)), str(phone$(2%), 1, 3), ch(03),~
               at (08,54), fac(lfac$( 5)), str(phone$(2%), 4, 3), ch(03),~
               at (08,58), fac(lfac$( 5)), str(phone$(2%), 7, 4), ch(04),~
                                                                         ~
               at (09,02), "PO Required?",                               ~
               at (09,30), fac(lfac$( 6)), poreqd$              , ch(01),~
                                                                         ~
               at (10,02), "Standard Price Code",                        ~
               at (10,30), fac(lfac$( 7)), pc$                  , ch(01),~
               at (10,49), fac(hex(8c)),   pcdescr$             , ch(32),~
                                                                         ~
               at (11,02), "Price Code for Options",                     ~
               at (11,30), fac(lfac$( 8)), pco$                 , ch(01),~
               at (11,49), fac(hex(8c)),   pcodescr$            , ch(32),~
                                                                         ~
               at (12,02), "Standard Order Discount %",                  ~
               at (12,30), fac(lfac$( 9)), disc$                , ch(05),~
                                                                         ~
               at (13,02), "Shipping Priority Code",                     ~
               at (13,30), fac(lfac$(10)), shipcode$            , ch(01),~
                                                                         ~
               at (14,02), "How Ship",                                   ~
               at (14,30), fac(lfac$(11)), howship$             , ch(20),~
                                                                         ~
               at (15,02), "FOB",                                        ~
               at (15,30), fac(lfac$(12)), fob$                 , ch(20),~
                                                                         ~
               at (16,02), "Shipping Region Code",                       ~
               at (16,30), fac(lfac$(13)), shiprgn$             , ch(09),~
               at (16,49), fac(hex(8c)),   shiprgndescr$        , ch(32),~
                                                                         ~
               at (17,02), "Export Customer?",                           ~
               at (17,30), fac(lfac$(14)), export$              , ch(01),~
                                                                         ~
               at (18,02), "Allow Backorders?",                          ~
               at (18,30), fac(lfac$(15)), bo$                  , ch(01),~
                                                                         ~
               at (18,40), "Allow Late Shipments?",                      ~
               at (18,63), fac(lfac$(16)), lateship$            , ch(01),~
                                                                         ~
               at (19,02), "Shipping Instructions",                      ~
               at (19,30), fac(lfac$(17)), shipinstr$(1)        , ch(50),~
               at (20,30), fac(lfac$(17)), shipinstr$(2)        , ch(50),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)), pf$(1)                 , ch(79),~
               at (23,02), fac(hex(8c)), pf$(2)                 , ch(79),~
               at (24,02), fac(hex(8c)), pf$(3)                 , ch(79),~
                   keys(str(pfkeys$())),                                 ~
                   key (keyhit%)

               if keyhit% <> 13 then L41872
                  call "MANUAL" ("CUSINPUT")
                  goto L41288

L41872:        if keyhit% <> 15 then L41904
                  call "PRNTSCRN"
                  goto L41288

L41904:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'103(fieldnr%, edit%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                                   else init(hex(86)) lfac$()
                  on fieldnr% gosub L42145,         /* Customer Status  */~
                                    L42145,         /* Currency code    */~
                                    L42145,         /* Tax Exemption #  */~
                                    L42145,         /* Sales Tax Code   */~
                                    L42145,         /* Customer Taxable?*/~
                                    L42145,         /* Customer Type    */~
                                    L42145,         /* Sales Region Code*/~
                                    L42145,         /* Salesmen/ Comm % */~
                                    L42145,         /* Payment Terms    */~
                                    L42160,         /* Credit Limit     */~
                                    L42160,         /* Credit # days    */~
                                    L42145,         /* FC Table         */~
                                    L42145,         /* Balance Type     */~
                                    L42145,         /* Document Printing*/~
                                    L42145,         /* Print C Xref Part*/~
                                    L42145,         /* Print M Xref Part*/~
                                    L42145,         /* Print Acknowledge*/~
                                    L42145          /* ICOC Code        */
                  goto L42180

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L42145:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L42160:           REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return
                                                          /* (EWD)    */
L42180:     accept                                                       ~
               at (01,02), "Customer Master Management",                 ~
               at (01,54), "Page 3",                                     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (03,02), fac(hex(94)), errormsg$              , ch(40),~
               at (03,45), fac(hex(84)), cust_add$              , ch(34),~
                                                                         ~
               at (04,02), "Customer Status",                            ~
               at (04,30), fac(lfac$( 1)), status$              , ch(01),~
               at (04,49), fac(hex(8c)),   statusdescr$         , ch(32),~
                                                                         ~
               at (05,02), fac(hex(8c)),   curr_msg$            , ch(13),~
               at (05,30), fac(lfac$( 2)), currency$            , ch(04),~
               at (05,49), fac(hex(8c)),   currdesc$            , ch(32),~
                                                                         ~
               at (06,02), "Tax Exemption Number",                       ~
               at (06,30), fac(lfac$( 3)), exempt$              , ch(25),~
                                                                         ~
               at (07,02), "Sales Tax Code",                             ~
               at (07,30), fac(lfac$( 4)), taxcode$             , ch(10),~
               at (07,49), fac(hex(8c)),   taxcodedescr$        , ch(32),~
                                                                         ~
               at (08,02), "Customer Taxable?",                          ~
               at (08,30), fac(lfac$( 5)), taxable$             , ch(01),~
                                                                         ~
               at (09,02), "Customer Type Code",                         ~
               at (09,30), fac(lfac$( 6)), type$                , ch(02),~
               at (09,49), fac(hex(8c)),   typedescr$           , ch(32),~
                                                                         ~
               at (10,02), "Sales Region Code",                          ~
               at (10,30), fac(lfac$( 7)), region$              , ch(04),~
               at (10,49), fac(hex(8c)),   regiondescr$         , ch(32),~
                                                                         ~
               at (11,02), "Salesmen/ Commission Splits",                ~
               at (11,30), fac(lfac$( 8)), salesmen$(1)         , ch(04),~
               at (11,37), fac(lfac$( 8)), comm$(1)             , ch(03),~
               at (11,49), fac(hex(8c))  , salesmendescr$(1)    , ch(32),~
               at (12,30), fac(lfac$( 8)), salesmen$(2)         , ch(04),~
               at (12,37), fac(lfac$( 8)), comm$(2)             , ch(03),~
               at (12,49), fac(hex(8c))  , salesmendescr$(2)    , ch(32),~
               at (13,30), fac(lfac$( 8)), salesmen$(3)         , ch(04),~
               at (13,37), fac(lfac$( 8)), comm$(3)             , ch(03),~
               at (13,49), fac(hex(8c))  , salesmendescr$(3)    , ch(32),~
                                                                         ~
               at (14,02), "Payment Terms (Code)",                       ~
               at (14,30), fac(lfac$( 9)), terms$               , ch(20),~
               at (14,52), fac(hex(8c)),   termsdescr$          , ch(29),~
                                                                         ~
               at (15,02), "Credit Limit",                               ~
               at (15,30), fac(lfac$(10)), limit$               , ch(10),~
               at (15,49), "Last Changed MM/DD/YY by UUU",               ~
               at (15,62), fac(hex(8c)),   crchng$              , ch(08),~
               at (15,74), fac(hex(8c)),   cruser$              , ch(03),~
                                                                         ~
               at (16,02), "Credit # of days",                           ~
               at (16,30), fac(lfac$(11)), nbrdays$             , ch(03),~
               at (16,49), "Last Changed MM/DD/YY by UUU",               ~
               at (16,62), fac(hex(8c)),   cdchng$              , ch(08),~
               at (16,74), fac(hex(8c)),   cduser$              , ch(03),~
                                                                         ~
               at (17,02), "Finance Charge Table",                       ~
               at (17,30), fac(lfac$(12)), fctable$             , ch(01),~
               at (17,49), fac(hex(8c)),   fctabledescr$        , ch(32),~
                                                                         ~
               at (18,02), "Open Item -or- Bal Fwd?",                    ~
               at (18,30), fac(lfac$(13)), bal$                 , ch(01),~
                                                                         ~
               at (19,02), "Doc. Printing - A/R: ",                      ~
               at (19,23), fac(lfac$(14)), docs$                , ch(01),~
               at (19,29), "XRef Parts: Cust?",                          ~
               at (19,47), fac(lfac$(15)), xref_cus$            , ch(01),~
               at (19,49), "Mfg?",                                       ~
               at (19,54), fac(lfac$(16)), xref_mnf$            , ch(01),~
               at (19,60), "Acknowledgements:",                          ~
               at (19,78), fac(lfac$(17)), acks$                , ch(01),~
                                                                         ~
               at (20,02), fac(hex(8c)),   icoc_prompt$         , ch(22),~
               at (20,30), fac(lfac$(18)), icoc$                , ch(09),~
               at (20,49), fac(hex(8c)),   icoc_desc$           , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)), pf$(1)                 , ch(79),~
               at (23,02), fac(hex(8c)), pf$(2)                 , ch(79),~
               at (24,02), fac(hex(8c)), pf$(3)                 , ch(79),~
                   keys(str(pfkeys$())),                                 ~
                   key (keyhit%)

               if keyhit% <> 13 then L42680
                  call "MANUAL" ("CUSINPUT")
                  goto L42180

L42680:        if keyhit% <> 15 then L42700
                  call "PRNTSCRN"
                  goto L42180

L42700:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *               S C R E E N   P A G E   4                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

            deffn'104(fieldnr%, edit%)
                  if fieldnr% > 0% then init(hex(8c)) lfac$()            ~
                                   else init(hex(86)) lfac$()
                  on fieldnr% gosub L43210,         /* Sales Distr      */~
                                    L43210,         /* Sales Discounts  */~
                                    L43210,         /* Net Invoice Distr*/~
                                    L43210,         /* Cash-in-Bank Acct*/~
                                    L43210,         /* Freight Account  */~
                                    L43210,         /* Sales Tax Account*/~
                                    L43210,         /* FC Acct          */~
                                    L43210,         /* PM SO Flag       */~
                                    L43210          /* PM INV Flag      */
                  goto L43280

                  REM Set FAC's for Upper/Lower Case Input
                      lfac$(fieldnr%) = hex(80)
                      return
L43210:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
                  REM Set FAC's for Numeric Only Input
                      lfac$(fieldnr%) = hex(82)
                      return
                                                             /* (EWD)  */
L43280: accept                                                           ~
            at (01,02), "Customer Master Management",                    ~
            at (01,54), "Page 4",                                        ~
            at (01,66), "Today:",                                        ~
            at (01,73), fac(hex(8c)), date$                     , ch(08),~
            at (02,02), fac(hex(ac)), line2$                    , ch(79),~
            at (03,02), fac(hex(84)), cust_add$                 , ch(35),~
            at (04,02), fac(hex(94)), errormsg$                 , ch(79),~
                                                                         ~
            at (05,02), "Sales Distribution Account",                    ~
            at (05,30), fac(lfac$( 1)), sales$                  , ch(12),~
            at (05,49), fac(hex(8c)),   salesdescr$             , ch(32),~
                                                                         ~
            at (06,02), "Sales Discounts Account",                       ~
            at (06,30), fac(lfac$( 2)), discacct$               , ch(12),~
            at (06,49), fac(hex(8c)),   discacctdescr$          , ch(32),~
                                                                         ~
            at (07,02), "Net Invoice Distribution",                      ~
            at (07,30), fac(lfac$( 3)), aracct$                 , ch(12),~
            at (07,49), fac(hex(8c)),   aracctdescr$            , ch(32),~
                                                                         ~
            at (08,02), "Cash-in-Bank Account",                          ~
            at (08,30), fac(lfac$( 4)), cashacct$               , ch(12),~
            at (08,49), fac(hex(8c)),   cashacctdescr$          , ch(32),~
                                                                         ~
            at (09,02), "Freight Account",                               ~
            at (09,30), fac(lfac$( 5)), frtacct$                , ch(12),~
            at (09,49), fac(hex(8c)),   frtacctdescr$           , ch(32),~
                                                                         ~
            at (10,02), "Sales Tax Account",                             ~
            at (10,30), fac(lfac$( 6)), taxacct$                , ch(12),~
            at (10,49), fac(hex(8c)),   taxacctdescr$           , ch(32),~
                                                                         ~
            at (11,02), "Finance Charge Account",                        ~
            at (11,30), fac(lfac$( 7)), fcacct$                 , ch(12),~
            at (11,49), fac(hex(8c)),   fcacctdescr$            , ch(32),~
                                                                         ~
            at (12,02),                                                  ~
             "Precious Metal Surcharge at SO Entry?",                    ~
            at (12,49), fac(lfac$(8%)), pm_so$                  , ch(01),~
            at (13,02),                                                  ~
             "Precious Metal Surcharge Update at Invoicing?",            ~
            at (13,49), fac(lfac$(9%)), pm_inv$                 , ch(01),~
                                                                         ~
            at (14,20), "S U M M A R Y   I N F O R M A T I O N",         ~
            at (15,02), "Account Opened", at(15,35), "BILL Open Orders", ~
            at (16,02), "Last Activity ", at(16,35), "SHIP Open Orders", ~
            at (17,02), "Last Invoice  ", at(17,35), "Open A/R       ",  ~
            at (18,02), "Last Payment  ", at(18,35), "High A/R Balance", ~
            at (19,02), "Last Changed  ",                                ~
            at (15,18), fac(hex(8c)), opened$                   , ch(08),~
            at (16,18), fac(hex(8c)), lastused$                 , ch(08),~
            at (17,18), fac(hex(8c)), lastinv$                  , ch(08),~
            at (18,18), fac(hex(8c)), lastcash$                 , ch(08),~
            at (19,18), fac(hex(8c)), lastchng$                 , ch(08),~
            at (19,27), "by", at(19,30), fac(hex(8c)), lastuser$, ch(03),~
            at (15,53), fac(hex(8c)), bals(1),      pic(-###,###,###.00),~
            at (16,53), fac(hex(8c)), bals(2),      pic(-###,###,###.00),~
            at (17,53), fac(hex(8c)), bals(3),      pic(-###,###,###.00),~
            at (18,53), fac(hex(8c)), bals(4),      pic(-###,###,###.00),~
            at (18,69), "on", at(18,72), fac(hex(8c)), baldate$ , ch(08),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)), pf$(1)                    , ch(79),~
            at (23,02), fac(hex(8c)), pf$(2)                    , ch(79),~
            at (24,02), fac(hex(8c)), pf$(3)                    , ch(79),~
                keys(str(pfkeys$())),                                    ~
                key (keyhit%)

               if keyhit% <> 13 then L43930
                  call "MANUAL" ("CUSINPUT")
                  goto L43280

L43930:        if keyhit% <> 15 then L43970
                  call "PRNTSCRN"
                  goto L43280

L43970:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50180,         /* Customer Code    */~
                                    L50280,         /* Sort Name        */~
                                    L50330,         /* Ship-to          */~
                                    L50380,         /* Sold-to          */~
                                    L50410,         /* Division Code    */~
                                    L50530,         /* Country Code     */~
                                    L50650,         /* ICC Code         */~
/*AWD005*/                          L50800          /* Invoice Plant    */
                  if str(customer$,,1) = hex(00) then errormsg$ = " "
                  return

L50180
*        Customer Code                         CUSTOMER$
            if customer$ <> " " or keyhit% = 17% then L50230
                call "GETCODE" (#01, customer$, " ", 0%, 1.30, f1%(1))
                if f1%(1) = 1% then L50230
                     errormsg$ = hex(00) : return
L50230:     gosub load_data
            cust_add$ = " "
            if textid$ <> hex(ffffffff) then                              ~
               cust_add$ = "(Additional Customer Information)"

            if onfile% = 0% then return
                return clear all
                goto editpg1

L50280
*        Customer Sort Name                    SORT$
            if str(customer$,,1) = hex(00) or sort$ <> " " then return
                errormsg$ = "Sort Name can not be left blank."
                return

L50330
*        Ship-to Name and Address              SHIPTO$
            if str(shipto$()) <> " " then return
                errormsg$ = "Ship-to information can not be left blank."
                return

L50380
*        Sold-to Name and Address              SOLDTO$
            return

L50410
*        Atrium Division Code                  DIVISION$           EWD003
            divisiondesc$ = " "
            if division$  = " " or divisionflag$ = "N" then return
                readkey$ = "DIVISION " & division$
                call "PLOWCODE" (#10, readkey$, divisiondesc$, 9%, .30,  ~
                                                                 f1%(10))
                if f1%(10) = 1% then L50500
                     errormsg$ = "Division Code not on file."
                     return
L50500:         division$ = str(readkey$,10)
                return

L50530
*        Country Code                          COUNTRY$
            countrydesc$ = " "
            if country$  = " " or countryflag$ = "N" then return
                readkey$ = "COUNTRYCD" & country$
                call "PLOWCODE" (#10, readkey$, countrydesc$, 9%, .30,   ~
                                                                 f1%(10))
                if f1%(10) = 1% then L50620
                     errormsg$ = "Country Code not on file."
                     return
L50620:         country$ = str(readkey$,10)
                return

L50650
*        Intercompany Corporation Code         ICC$
            if icc$ = " " or iccflag$ = "N" then return
            if mgtval_on$ <> "Y" then return
                if icoc$ = " " then L50720
                     errormsg$ = "Cannot be an Intercompany Corporation"&~
                                 " and be owned by another."
                     return
L50720:         readkey$ = "CORPORATE" & icc$
                call "PLOWCODE" (#10, readkey$, temp$, 9%, .30, f1%(10))
                if f1%(10) = 1% then L50780
                     errormsg$ = "Intercompany Corporation Code not on "&~
                                 "file."
                     return
L50780:         icc$ = str(readkey$,10, 6)
                return

L50800
*       AWD005 North East invoice plant         NE_INVPLT$
            ne_invdesc$ = " "
            if ne_invplt$  = " " or ne_invplt$ = "0" then return
                readkey$ = "PLANTINV " & ne_invplt$
                call "PLOWCODE" (#10, readkey$, ne_invdesc$, 9%, .30,   ~
                                                                 f1%(10))
                if f1%(10) = 1% then L50820
                     errormsg$ = "Invoice Plant not in PLANTINV."
                     return
L50820:         ne_invplt$ = str(readkey$,10)

         return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

            deffn'152(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L51156,         /* Account X-Ref    */~
                                    L51246,         /* Bill-to X-Ref    */~
                                    L51336,         /* Credit Parent    */~
                                    L51444,         /* Customer Contact */~
                                    L51456,         /* Phone Number/Fax */~
                                    L51474,         /* PO Required?     */~
                                    L51504,         /* Std Price Code   */~
                                    L51539,         /* Options Price Cde*/~
                                    L51558,         /* Order Discount   */~
                                    L51608,         /* Shipping Priority*/~
                                    L51618,         /* How Ship         */~
                                    L51636,         /* FOB              */~
                                    L51654,         /* Shipping Region  */~
                                    L51940,         /* Export Customer? */~
                                    L51726,         /* Allow Backorders?*/~
                                    L51756,         /* Allow Late Ship? */~
                                    L51786          /* Shipping Instr   */
                  if str(customer$,,1) = hex(00) then errormsg$ = " "
                  return

L51156
*        Account Cross Reference               ACCTXREF$
            acctxrefdescr$ = " "
            if acctxref$ = customer$ then acctxref$ = " "
            if acctxref$ = " " then return
                call "GETCODE" (#01, acctxref$, acctxrefdescr$, 0%, 1.30,~
                                                                  f1%(1))
                if f1%(1) = 1% then L51210
                     errormsg$ = "Cross Reference must be a valid code."
                     return
L51210:         get #01 using L51216, temp$
L51216:              FMT XX(770), CH(9)
                if temp$ = acctxref$ then return
                     errormsg$ = "This Customer is not an Account."
                     return

L51246
*        Bill-to Cross Reference               BILLXREF$
            billxrefdescr$ = " "
            if billxref$ = customer$ then billxref$ = " "
            if billxref$ = " " then return
                call "GETCODE" (#01, billxref$, billxrefdescr$, 0%, 1.30,~
                                                                  f1%(1))
                if f1%(1) = 1% then L51300
                     errormsg$ = "Cross Reference must be a vaild code."
                     return
L51300:         get #01 using L51306, temp$
L51306:              FMT XX(779), CH(9)
                if temp$ = billxref$ then L51326
                     errormsg$ = "This Customer is not a Bill-to."
                     return
L51326:         if crparent$ = " " then return
                     errormsg$ = "You cannot have both a Bill-to and " & ~
                                 "Credit Parent for the same Customer."
                     return

L51336
*        Credit Parent                         CRPARENT$
            crpardescr$ = " "
            if crparent$ = " " then return
            call "PLOWCODE" (#01, crparent$, crpardescr$, 0%, .30, f1%(1))
            if f1%(1) <> 0% then L51378
                errormsg$ = "Enter a Credit Parent or blanks."
                return
L51378:     if crparent$ <> customer$ then L51396
                errormsg$ = "You can't enter the customer as it's parent"
                return
L51396:     get #01 using L51402, temp$
L51402:              FMT POS(1049), CH(9)
            if temp$ = " " then L51420
                errormsg$ = "This Customer has a Credit Parent." : return
L51420:     if billxref$ = " " or billxref$ = customer$ then return
                errormsg$ = "Credit Parents can only be assigned to Bill"~
                            & " to Customers."
                return
L51444
*        Customer Contact                      CONTACT$
            return
L51456
*        Phone Number & Fax                    PHONE$()
            return

L51474
*        PO Required?                          POREQD$
            if poreqd$ = "Y" or poreqd$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L51504
*        Standard Price Code                   PC$         /* (EWD)  */
            if (pc$ >= "A" and pc$ <= "Z") or                            ~
               (pc$ >= "0" and pc$ <= "9") then L51534
                errormsg$ = "Price Code must be 'A'-'Z' or '0'-'9'."
                return                                     /* (EWD)  */
L51534:     readkey$ = "PRICECODE" & pc$
            call "DESCRIBE" (#10, readkey$, pcdescr$, 0%, f1%(10))
            readkey$ = "CUST RPT " & customer$
               call "DESCRIBE" (#10,readkey$, rhh$, 0%, f1%(10))
               if f1%(10%) <> 0 then pcdescr$ = rhh$ 
            return

L51539
*        Price Code for Options                PCO$        /* (EWD) */
            if pco$ = " " then return   /* allow blank for default */
            if (pco$ >= "A" and pco$ <= "Z") or                          ~
               (pco$ >= "0" and pco$ <= "9") then L51545
                errormsg$ = "Price Code must be 'A'-'Z' or '0'-'9'."
                return                                     /* (EWD) */
L51545:     readkey$ = "PRICECODE" & pco$
            call "DESCRIBE" (#10, readkey$, pcodescr$, 0%, f1%(10))
            return

L51558
*        Standard Order Discount %             DISC$
            if disc$ = " " then disc$ = "0"
            convert disc$ to disc, data goto L51576 : goto L51582
L51576:         errormsg$ = "Illegal numeric entry." : return
L51582:     if disc >= 0 and disc <= 99.99 then L51600
                errormsg$ = "Discount % must be between 0 and 99.99."
                return
L51600:     call "CONVERT" (disc, 2.2, disc$)
            return

L51608
*        Shipping Priority Code                SHIPCODE$
            if shipcode$ = " " then shipcode$ = "3"
               if shipcode$ >= "1" and shipcode$ <= "5" then return
            errormsg$ = "Shipping Priority Must Be 1 thru 5."
            return

L51618
*        How Ship                              HOWSHIP$
* <AWD006  Validate How Ship >
            if howship$ = " " then return
            readkey$ = "HOWSHIP  " & howship$
            call "PLOWCODE" (#10, readkey$, " ", 9%, .30,   ~
                                                     f1%(10))
                if f1%(10) = 1% then L51628
                     errormsg$ = "Howship Code not on file."
                     return
L51628:         howship$ = str(readkey$,10)


            return
* </ AWD006  Validate How Ship >


L51636
*        FOB                                   FOB$
            return

L51654
*        Shipping Region Code                  SHIPRGN$
            shiprgndescr$ = " "
            if shiprgn$   = " " then return
                readkey$ = "SHPREGION" & shiprgn$
                call "PLOWCODE" (#10, readkey$, shiprgndescr$, 9%, .30,  ~
                                                                 f1%(10))
                if f1%(10) = 1% then L51708
                     errormsg$ = "Shipping Region Code not on file."
                     return
L51708:         shiprgn$ = str(readkey$,10)
                return

L51726
*        Allow Backorders?                     BO$
            if bo$ = "Y" or bo$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L51756
*        Allow Late Shipments?                 LATESHIP$
            if lateship$ = "Y" or lateship$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L51786
*        Shipping Instructions                 SHIPINSTR$
            return

L51940
*        Normally an Export Customer?          EXPORT$
            if pos(" YN" = export$) <> 0% then return
                errormsg$ = "Invalid Export Flag, Please enter 'Y', 'N'"&~
                            ", or Blank."
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 3.                      *~
            *************************************************************

            deffn'153(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L52150,         /* Customer Status  */~
                                    L52210,         /* Currency code    */~
                                    L52252,         /* Tax Exemption #  */~
                                    L52270,         /* Sales Tax Code   */~
                                    L52318,         /* Customer Taxable?*/~
                                    L52348,         /* Customer Type    */~
                                    L52420,         /* Sales Region Code*/~
                                    L52492,         /* Salesmen/ Comm % */~
                                    L52624,         /* Payment Terms    */~
                                    L52672,         /* Credit Limit     */~
                                    L52738,         /* Credit # days    */~
                                    L52786,         /* FC Table         */~
                                    L52852,         /* Balance Type     */~
                                    L52882,         /* Document Print   */~
                                    L52962,         /* Xref Customer Prt*/~
                                    L52974,         /* Xref Manufctr Prt*/~
                                    L52986,         /* Print Acknowledge*/~
                                    L52910          /* ICOC Code        */

                  if str(customer$,,1) = hex(00) then errormsg$ = " "
                  return

L52150
*        Customer Status                       STATUS$
            if status$ = "A" or status$ = "D" or status$ = "H" or        ~
               status$ = "I" or status$ = "N" then L52174          /*  (EWD004) */
            if status$ = "O" then L52174   /*(AWD008)*/
                errormsg$ = "Enter 'A', 'D', 'H', 'I', 'N' or 'O'."  : return
L52174:     if status$ = "A" then statusdescr$ = "Active"
            if status$ = "D" then statusdescr$ = "To Be Deleted"
            if status$ = "H" then statusdescr$ = "On Hold"
            if status$ = "I" then statusdescr$ = "Inactive"
            if status$ = "N" then statusdescr$ = "New Account"     /*  (EWD004) */
            if status$ = "O" then statusdescr$ = "Obsolete Account" /* (AWD008) */
            return

L52210
*        Currency code                         CURRENCY$
            currdesc$ = " " : if currency$ = " " then return
            if curr_on_flag$ <> "Y" then return  /* Just to make sure */
            call "GETCODE" (#05, currency$, currdesc$, 0%, .3, f1%(5))
            if f1%(5) <> 0% then return
                errormsg$ = "Currency code not found on file." : return

L52252
*        Tax Exemption Number                  EXEMPT$
            return

L52270
*        Sales Tax Code                        TAXCODE$
            taxcodedescr$ = " " : if taxcode$ = " " then return
                call "GETCODE" (#11, taxcode$, taxcodedescr$, 0%, 0.30,  ~
                                                                 f1%(11))
                if f1%(11) = 1% then return
                     errormsg$ = "Tax Code not on file."
                     return

L52318
*        Customer Taxable?                     TAXABLE$
            if taxable$ = "Y" or taxable$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L52348
*        Customer Type Code                    TYPE$
            typedescr$ = " "
            if type$   = " " then return
                readkey$ = "CUS TYPES" & type$
                call "PLOWCODE" (#10, readkey$, typedescr$, 9%, .30,     ~
                                                                 f1%(10))
                if f1%(10) = 1% then L52402
                     errormsg$ = "Customer Type Code not on file."
                     return
L52402:         type$ = str(readkey$,10)
                return

L52420
*        Sales Region Code                     REGION$
            regiondescr$ = " "
            if region$   = " " then return
                readkey$ = "REGIONS  " & region$
                call "PLOWCODE" (#10, readkey$, regiondescr$, 9%, .30,   ~
                                                                 f1%(10))
                if f1%(10) = 1% then L52474
                     errormsg$ = "Sales Region Code not on file."
                     return
L52474:         region$ = str(readkey$,10)
                return

L52492
*        Salesmen/ Commission Splits           SALESMEN$()
            total% = 0%
            for i% = 1% to 3%
                if salesmen$(i%) <> " " then L52528
                     salesmendescr$(i%), comm$(i%) = " "
                     comm%(i%) = 0%  :  goto L52594
L52528:         call "GETCODE" (#12, salesmen$(i%), salesmendescr$(i%),  ~
                                                       0%, 0.30, f1%(12))
                if f1%(12) = 1% then L52558
                     errormsg$ = "Salesman Code " & salesmen$(i%) &      ~
                                 " not on file."  :  return
L52558:         if comm$(i%) = " " then comm$(i%) = "0"
                convert comm$(i%) to comm%(i%), data goto L52576
                goto L52582
L52576:              errormsg$ = "Commission % must be 0 - 100." : return
L52582:         total% = total% + comm%(i%)
                convert comm%(i%) to comm$(i%), pic(##0)
L52594:     next i%
            if total% <= 100% then return
                errormsg$ = "Total Commission Splits can not exceed 100%."
                return

L52624
*        Payment Terms (Code)                  TERMS$
            termsdescr$ = "0% 0 Days, Net 0 Days"
            if terms$ = " " then return
                call "GETCODE" (#13, terms$, termsdescr$, 0%, .3, f1%(13))
                if f1%(13)= 1% then return
                     termsdescr$ = "0% 0 DAYS NET 0 DAYS"
                     return

L52672
*        Credit Limit                          LIMIT$
            limit = 0  :  if limit$ = " " then limit$ = "0"
            convert limit$ to limit, data goto L52690  :  goto L52696
L52690:         errormsg$ = "Illegal numeric entry"   :  return
L52696:     limit = round(limit, 0)
            if limit >= 0 and limit <= 99999999 then L52720
                errormsg$ = "Credit Limit range: 0 - 99,999,999."
                return
L52720:     call "CONVERT" (limit, 0.0, limit$)
            return

L52738
*        Credit # days                         NBRDAYS$
            nbrdays% = 0% : if nbrdays$ = " " then nbrdays$ = "0"
            call "NUMTEST" (nbrdays$, 0, 300, errormsg$, 0, temp)
            if errormsg$ <> " " then return
                nbrdays% = temp
                convert nbrdays% to nbrdays$, pic (##0)
                return

L52786
*        Finance Charge Table                  FCTABLE$
            fctabledescr$ = "No Finance Charges"
            if fctable$ = " " then return
                readkey$ = "FINANCECHARGETABLE" & fctable$
                call "PLOWCODE" (#14, readkey$, fctabledescr$, 18%, .30, ~
                                                                 f1%(14))
                if f1%(14) = 1% then L52834
                     errormsg$ = "Table ID not found on file." : return
L52834:         fctable$ = str(readkey$,19,1)
                return

L52852
*        Balance Type                          BAL$
            if bal$ = "O" or bal$ = "B" then return
                errormsg$ = "Enter 'O', or 'B'."
                return

L52882
*        Document Printing                     DOC$
            if pos("SLBN" = docs$) <> 0% then return
                errormsg$ = "Enter 'S', 'L', 'B', or 'N'."
                return

L52910
*        Intercompany Ownership Code           ICOC$
            icoc_desc$ = " "
            if icoc$ = " " or mgtval_on$ <> "Y" then return
            call "GETCODE" (#01, icoc$, icoc_desc$, 0%, 1.30, f1%(1%))
            if f1%(1) <> 0% then L52924
                errormsg$ = "Enter a valid customer or blanks."
                return
L52924:     if icoc$ <> customer$ then L52930
                errormsg$ = "Cannot be owned by self."
                return
L52930:     if icc$ = " " then L52936
                errormsg$ = "An Intercompany Corporation cannot be owned."
                return
L52936:     get #01 using L52938, temp$
L52938:         FMT POS(1076), CH(6)
            if temp$ <> " " then return
            u3% = 2%
            call "ASKUSER" (u3%, " *** ICC  WARNING *** ", "The Inter" & ~
                            "company Owner picked has no", "Inter" &     ~
                            "company Corporate Code.", "Press RETURN" &  ~
                            " to acknowledge.")
            return

L52962
*        Test Data for XREF_CUS$
            if xref_cus$ = "N" or xref_cus$ = "Y" or xref_cus$ = " "     ~
                               or xref_cus$ = "B" then return
                errormsg$ = "Enter 'Y', 'N', 'B', or blank."
                return

L52974
*        Test Data for XREF_CUS$
            if xref_mnf$ = "N" or xref_mnf$ = "Y" or xref_mnf$ = " "     ~
                               or xref_mnf$ = "B" then return
                errormsg$ = "Enter 'Y', 'N', 'B', or blank."
                return

L52986
*        Print Acknowledgements?               ACKS$
            if acks$ = "Y" or acks$ = "N" then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 4.                      *~
            *************************************************************

            deffn'154(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L53180,         /* Sales Distr      */~
                                    L53230,         /* Sales Discounts  */~
                                    L53280,         /* Net Invoice Distr*/~
                                    L53330,         /* Cash-in-Bank Acct*/~
                                    L53380,         /* Freight Account  */~
                                    L53430,         /* Sales Tax Account*/~
                                    L53480,         /* FC Acct          */~
                                    L53514,         /* PM SO Flag       */~
                                    L53524          /* PM INV Flag      */
                  if str(customer$,,1) = hex(00) then errormsg$ = " "
                  return

L53180
*        Sales Distribution Account            SALES$
            acct$ = sales$ : gosub test_acct
            sales$ = acct$ :  salesdescr$ = acctdescr$
            return

L53230
*        Sales Discounts Account               DISCACCT$
            acct$ = discacct$ : gosub test_acct
            discacct$ = acct$ : discacctdescr$ = acctdescr$
            return

L53280
*        Net Invoice Distribution              ARACCT$
            acct$ = aracct$ : gosub test_acct
            aracct$ = acct$ : aracctdescr$ = acctdescr$
            return

L53330
*        Cash-in-Bank Account                  CASHACCT$
            acct$ = cashacct$ : gosub test_acct
            cashacct$ = acct$ : cashacctdescr$ = acctdescr$
            return

L53380
*        Freight Account                       FRTACCT$
            acct$ = frtacct$ : gosub test_acct
            frtacct$ = acct$ : frtacctdescr$ = acctdescr$
            return

L53430
*        Sales Tax Account                     TAXACCT$
            acct$ = taxacct$ : gosub test_acct
            taxacct$ = acct$ : taxacctdescr$ = acctdescr$
            return

L53480
*        Finance Charge Account                FCACCT$
            acct$ = fcacct$ : gosub test_acct
            fcacct$ = acct$ : fcacctdescr$ = acctdescr$
            return

L53514
*        Test Data for Precious Metal Surcharge at SO Entry
            if pos(" YN" = pm_so$) > 0 then return
                errormsg$ = "Enter 'Y' or 'N'."
                return

L53524
*        Test Data for Precious Metal Surcharge at Invoice Update
            if pos(" YN" = pm_inv$) > 0 then return
                errormsg$ = "Enter 'Y' or 'N'."
                return


        test_acct         /* Test and describe account number          */
            if acct$ = " " then L53660
            call "GETCODE" (#15, acct$, acctdescr$, 0%, 0.30, f1%(15))
            if f1%(15) = 1% then L53700
                if acct$ = " " then L53660
                     acctdescr$ = "** NOT ON FILE **"
                     accttype$  = " "
                     return

        describe_acct    /* Describe account number                    */
            call "DESCRIBE" (#15, acct$, acctdescr$, 0%, f1%(15))
            if f1%(15) = 1% then L53690
L53660:         acctdescr$ = "[Use System Default]"
                accttype$  = " "
                return
L53690:     call "GLFMT" (acct$)
L53700:     get #15 using L53710, accttype$
L53710:              FMT XX(39), CH(1)
            return


        round_balance
            bals(1) = round(bals(1),2)
            bals(2) = round(bals(2),2)
            bals(3) = round(bals(3),2)
            bals(4) = round(bals(4),2)
        return

        check_access                                  /* (EWD) - Begin  */ 
            init(" ") readkey$
            security% = 0%
            str(readkey$,1%,9%) = "APCACCESS"
            str(readkey$,10%,15%) = userid$
            read #10,key = readkey$, eod goto L53800
            security% = 1%
L53800:     return                                    /* (EWD) - End   */

        check_customer                                /* (EWD001) - Begin */
            return                                    /*     (EWD004)     */
            cusaccess% = 0%
            if str(customer$,1%,9%) = " " then return
               gosub read_customer
               
               if found% = 0 then return
               if str(tstuserid$,1%,3%) = str(userid$,1%,3%) then return
               
            cusaccess% = 1%                /* Someone already in Customer */
        return                                        

        add_customer
            return                                    /*     (EWD004)     */
            gosub read_customer
            if found% <> 0% then delete #10
            
            put #10, using L53810, str(readkey$,1%,18%), str(userid$,1%,3%)
L53810:        FMT CH(24), CH(30)

            write #10
        return

        delete_customer
            return                                    /*     (EWD004)     */
            if str(customer$,1%,9%) = " " or str(customer$,1%,1%) = hex(00) then return
            gosub read_customer
            delete #10

            str(customer$,1%,9%) = " "
        return

        read_customer
            init(" ") readkey$, tstuserid$
            found% = 0%
            str(readkey$,1%,9%)  = "CUSACCESS"
            str(readkey$,10%,9%) = customer$
            read #10, hold, key = readkey$, using L53830, tstuserid$,       ~
                                               eod goto L53820
L53830:         FMT XX(24), CH(03)             
            found% = 1%
L53820: return

        
        check_cust_error
             errormsg$ = tstuserid$ & " is already in that Customer, access is Denied! "
             gosub error_prompt
             init(" ") customer$
        return

        delete_old_text
            readkey$ = all(hex(00))
            str(readkey$,1%,1%) = "M"
            str(readkey$,2%,3%) = "   "
            str(readkey$,5%,4%) = saveid$
        delete_text_next
          read #02, hold, key > readkey$, using L53840, readkey$,          ~
                                             eod goto delete_text_done
L53840:          FMT CH(11)
              if str(readkey$,5%,4%) <> str(saveid$,1%,4%) then            ~
                                                 goto delete_text_done
                     delete #02
                goto delete_text_next
        delete_text_done
        return

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return                                             /* (EWD001) - End   */
        
        
        delete_cust
          read #01, hold, key = customer$, eod goto delete_cust_done
             
/* CR1128 */ gosub warning_msg
             if u3% <> 16% then goto delete_cust_done
             
             delete #01
        delete_cust_done
        return     

/* CR1128 */        
        warning_msg
            hdr$     = "***** Warning Warning *****"
            msg$(1%) = "You are deleting customer " &customer$
            msg$(2%) = "Please confirm delete by pressing PF16 "     
            msg$(3%) = "or any key to cancel"

L64550:     u3% = 2%
            call "ASKUSER" (u3%, hdr$, msg$(1%), msg$(2%), msg$(3%))
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
            * COPYRIGHT (C) 1986  AN UNPUBLISHED WORK BY CAELUS ASSO-   *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
