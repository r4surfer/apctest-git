        REM *************************************************************~
            *                                                           *~
            *  PPPP    OOO   PPPP   RRRR   IIIII  N   N  TTTTT          *~
            *  P   P  O   O  P   P  R   R    I    NN  N    T            *~
            *  PPPP   O   O  PPPP   RRRR     I    N N N    T            *~
            *  P      O   O  P      R   R    I    N  NN    T            *~
            *  P       OOO   P      R   R  IIIII  N   N    T            *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * POPRINT  - PRINTS PURCHASE ORDERS.  This program is       *~
            *  provided to print the CAELUS Standard Forms Purchase     *~
            *  Order.                                                   *~
            *----------------------------------------------------------Q*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/16/81 ! ORIGINAL                                 ! TEM *~
            * 12/31/85 ! TOTAL REWRITE                            ! MJB *~
            * 01/28/86 ! Added Is/Was Printing Option             ! ERN *~
            * 10/30/86 ! Modified to print CMS Standard Form      ! MJB *~
            * 04/05/88 ! Prints right Store Name, (SEND CHECK TO) ! RJM *~
            *          !   Inits TAXYES$ to 'NO' @ 30252          !     *~
            * 11/03/88 ! Amounts now in transaction currency      ! JDH *~
            * 04/17/89 ! Text not printed when vend part code used! GGO *~
            * 09/07/89 ! Currency designation suppressed if MC Off! JDH *~
            * 11/26/91 ! PRR 12105 - UOM factor limitation chg'd  ! MLJ *~
            *          !   from 10000 - 1000000, REM'd prev line.!      *~
            * 05/21/92 ! PRR 11410 - Added RETURN @ line # 10790  ! MLJ *~
            *          !   eliminating double print of revision   !     *~
            *          !   lines.                                 !     *~
            *          ! PRR 11946 - 'ADVISE' noe prints in price !     *~
            *          !   column if status = 'W' & price = 0.    !     *~
            *          ! PRR 12028 - Based on 2 new switches in   !     *~
            *          !   VBKFLAGS, prints revision date on PO   !     *~
            *          !   header and prints Received qty & Net   !     *~
            *          !   Due qty for each line.                 !     *~
            * 05/25/93 ! Now prints up to 3 'Boiler plate's.      ! JDH *~
            * 07/14/93 ! PRR 12979 - Format Zip Code.             ! JDH *~
            *          ! PRR 12983 - Fixed bad branch in 9000's   !     *~
            *          !   skipping Rev. Date & Qty print flags.  !     *~
            * 09/08/93 ! MISC - #9 & #10 only opened if MC is on. ! MLJ *~
            *          !   #7 & #8 only opened if ISWAS$ = 'Y'.   !     *~
            *          !   Program renumber w/blank modcode making!     *~
            *          !   room for screen processing.            !     *~
            *          ! PRR 13005 - Added selection capability   !     *~
            *          !   for printing of purchase orders.       !     *~
            * 09/24/93 ! Added ASKUSER msg when no POs found to   !     *~
            *          !   meet selection criteria - none printed.! MLJ *~
            * 12/03/93 ! Changed PLOWAL1 to PLOWALTS on line 9250.! MLJ *~
            * 03/31/94 ! PRR 13132. Added vendor & vpr to POs.    ! JDH *~
            * 05/13/94 ! PRR 13171 - Now honors output seq flag.  ! MLJ *~
            * 07/05/94 ! Print Contract ID,Line #,& Descr if there! MLJ *~
            * 02/03/95 ! Fixed ISWAS printing.  Only print recv'd ! JDH *~
            *          !   & BO qtys if line has had qty recv'd.  !     *~
            * 08/31/95 ! PRR 13500. Fixed enabling of edit fields.! JDH *~
            * 08/23/96 ! Changes for the year 2000.               ! DXL *~
            * 02/12/98 ! Mods for (EWD) to Print our Part Number  ! RHH *~
            *          !   on the Purchase Order at (L60185)      !     *~
            * 04/07/98 ! Make Upgrades to latest Version Caleus   ! RHH *~
            *          !   (EWD) Mods                             !     *~
            * 05/24/00 ! Make mods to allow for faxing of Purch.  ! CMG *~
            *          !   Orders.  (EWD0001)                     !     *~
            * 03/04/04 ! (EWD002) Mods to for the new version of  ! CMG *~
            *          !          Rightfax                        !     *~	
            * 03/16/06 ! (AWD003) - modification for North East   ! CMG *~		 
            *************************************************************

        dim                                                              ~
            acct$16,                     /* LINE ITEM SALES ACCOUNT #  */~
            addr$(5)31,                  /* Vendor address lines       */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            buyer$3,                     /* Buyer ID                   */~
            canceldate$8,                /* CANCELLATION DATE INFO     */~
            cat$4,                       /* INVENTORY CATEGORY CODE    */~
            cdatedue$8,                  /* Old Due Date               */~
            cdescr$32,                   /* Old line description       */~
            changedate$8,                /* Change Date Header         */~
            changeid$3,                  /* Change USERID              */~
            chgdate$8,                   /* Change Date LI             */~
            chtotal$12,                  /* Total of lines printed     */~
            cline$70,                    /* Change Line Print          */~
            confflag$1,                  /* Confirmation Flag          */~
            confyes$3,                   /* Confirmation Literal       */~
            conname$20,                  /* CONTACT'S NAME             */~
            contract_id$16,              /* Contract ID Code           */~
            contract_line$4,             /* Contract Line Number       */~
            contract_descr$30,           /* Contract Description       */~
            cpytextid$4,                 /* Copy Text ID               */~
            cpytxtname$(3)10,            /* Copy Text Name             */~
            currency$4, currdesc$32,     /* Currency Code & Description*/~
            cursor%(2),                  /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            datedue$8,                   /* LINE ITEM DATE DUE         */~
            datenext$8,                  /* DATE OF NEXT RECEIPT       */~
            daterecd$8,                  /* DATE OF LAST RECEIPT       */~
            defduedate$8,                /* DEFAULT DUE DATE           */~
            defjob$8,                    /* DEFAULT JOB NUMBER         */~
            defstore$3,                  /* DEFAULT STORE NUMBER       */~
            descr$32,                    /* DESCRIPTION THIS LINE ITEM */~
            dollmax$10,                  /* Maximum Dollar Limit       */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            expdate$6,                   /* Expiration Date            */~
            ext$12,                      /* EXTENSION AMOUNT           */~
            from_buyer$3,                /* From Buyer ID Code         */~
            from_po$16,                  /* From Purchase Order Number */~
            from_podate$10,              /* From Purchase Order Date   */~
            from_store$3,                /* From Store Number          */~
            from_vendor$9,               /* From Vendor Number         */~
            fob$30,                      /* F.O.B. Location            */~
            frterms$1,                   /* Freight Terms              */~
            head$50,                     /* Header for ASKUSER         */~
            headerkey$25,                /* KEY TO VBKMASTR            */~
            hi$79,                       /* Hi line for ASKUSER        */~
            hsts$1,                      /* Changed Header Status      */~
            i$(24)80,                    /* Screen Image               */~
            ibuyer$3,                    /* LI Buyer                   */~
            idelto$20,                   /* Deliver to                 */~
            iduedate$6,                  /* LI Due Date                */~
            iliref$5,                    /* LI Reference number        */~
            inpmessage$79,               /* Informational Message      */~
            ipo$16,                      /* LI PO Number               */~
            irev$2,                      /* LI Revision Number         */~
            iseq$3,                      /* LI Sequence Number         */~
            istat$1,                     /* LI Status                  */~
            iswas$1,                     /* Show Changes?              */~
            item$3,                      /* LINE ITEM ITEM NUMBER      */~
            itype$3,                     /* Inv Type Designator        */~
            ivend$9,                     /* LI Vendor Number           */~
            iwhoreq$20,                  /* Who requisitioned          */~
            job$8,                       /* JOB NUMBER INFORMATION     */~
            lastinvnr$16,                /* LAST INVOICE NUMBER        */~
            lfac$(20)1,                  /* Field Attribute Characters */~
            line2$79,                    /* Screen Line #2             */~
            lo$79,                       /* Lo line for ASKUSER        */~
            lot$6,                       /* LOT NUMBER INFORMATION     */~
            lsts$1,                      /* Change Line Status         */~
            mcon$1,                      /* Multi-currency on flag     */~
            meas$4,                      /* VENDOR'S UNIT OF MEASURE   */~
            mid$79,                      /* Mid line for ASKUSER       */~
            noafter$6,                   /* No receive after date      */~
            nobefore$6,                  /* No receive before date     */~
            ohpost$1,                    /* Post to onhand flag        */~
            orderdate$8,                 /* PURCHASE ORDER DATE        */~
            origdate$6,                  /* Original enter date        */~
            origid$3,                    /* Original enter USERID      */~
            part$25,                     /* PART NUMBER INFORMATION    */~
            part2$25,                    /* PART NUMBER FOR LINE DELE  */~
            partkey$25,                  /* PART KEY                   */~
            payacct$16,                  /* PAYABLES ACCOUNT DEFAULT   */~
            pdescr$45,                   /* PLOWCODE Description       */~
            pf$(3)79,                    /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            ponumber$16,                 /* PURCHASE ORDER NUMBER      */~
            poord$10,                    /* PO Sort Flag Variable      */~
            poprdate$1,                  /* Print Revision Date Flag   */~
            poprpnd$1,                   /* Print Quantities Flag      */~
            price$10,                    /* UNIT PRICE (FLEXI-PRINT)   */~
            prtflg$1,                    /* PO Print Flag              */~
            prtponbr$16,                 /* Print Flag from VBKPOPRT   */~
            prtvndr$9,                   /* Vendor Number frm VBKPOPRT */~
            pslip$16,                    /* Last Packing Slip Number   */~
            qtyonord$10,                 /* QUANTITY BACKORDERED       */~
            qtyorig$10,                  /* QUANTITY ORIGINALLY ORDERED*/~
            qtyrecd$10,                  /* QUANTITY SOLD INFORMATION  */~
            readkey$50,                  /* KEY FOR PLOW ROUTINES      */~
            readkey2$50,                 /* KEY FOR PLOW IN DELE LINES */~
            readkey3$99,                 /* Read Key                   */~
            readcur$50,                  /* KEY FOR PLOW Currency Lines*/~
            recdate$6,                   /* Receiving Date             */~
            revdate$23,                  /* PO Revision Date           */~
            revnbr$2,                    /* PO Revision Number         */~
            saddr$(5)31,                 /* Ship To Print Address      */~
            shipvia$30,                  /* Ship Via ?                 */~
            shpaddr$(5)31,               /* Ship TO Address            */~
            shpname$30,                  /* Ship TO Name               */~
            sname$30,                    /* Ship To Print Name         */~
            stamp$7,                     /* Buffer Date/Time Stamp     */~
            statutory$4,                 /* Statutory currency         */~
            straddr$(3)30,               /* Store Address              */~
            strname$30,                  /* Store Name                 */~
            taxyes$3,                    /* Taxable Literal            */~
            textid1$4,                   /* Text ID                    */~
            textid2$4,                   /* Text ID                    */~
            to_buyer$3,                  /* To Buyer ID Code           */~
            to_po$16,                    /* To Purchase Order Number   */~
            to_podate$10,                /* To Purchase Order Date     */~
            to_store$3,                  /* To Store Number            */~
            to_vendor$9,                 /* To Vendor Number           */~
            total$12,                    /* Total order amount         */~
            txbl$1,                      /* Taxable Flag               */~
            var1$16,                     /* FROM Variable              */~
            var2$16,                     /* TO Variable                */~
            variable$(10)20,             /* VARIABLE FIELDS            */~
            vbklseq$3,                   /* Last line used Seq no.     */~
            venbfcode$6,                 /* Vendor Buy From Code       */~
            vencode$9,                   /* CUSTOMER CODE NUMBER       */~
            venname$30,                  /* CUSTOMER NAME              */~
            ventext$4,                   /* Vendor Text ID             */~
            ventype$4,                   /* Vendor type Code           */~
            vpart$25,                    /* Vendor Part Number         */~
            vprtext$4                    /* Vendor Price Cat Text ID   */

        dim f2%(64),                     /* FILE STATUS FLAGS FOR      */~
            f1%(64),                     /* RECORD-ON-FILE FLAGS       */~
            rslt$(64)20                  /* RETURN CODE FROM "OPENCHCK"*/
/* (EWD0001)  Begin  */
        dim a$80,                        /* Print Line on Purchase Ord */~
            fax$1,                       /* Do You Want to Fax?        */~
            buy_name$30,                 /* Buyer Full Name            */~
            desc$30,                     /* Descriptions from Gencodes */~		 
            file$8,                      /* Inv Print File             */~
            library$8,                   /* Library Name = APCDATA     */~			
	    pur_flag$1,                  /* Flag to fax or not?        */~
            pur_no$11,                   /* Vendor Buy From Fax Number */~
            pur_key$24,                  /* Save File Key              */~
            pur_key1$16,                 /* Purchase Readkey           */~			
            script$8,                    /* FAX SHELL SCRIPT           */~
            volume$6,                    /* DISK VOLUME = CARLOS       */~
            title$35                     /* Summary Title              */
/* (EWD0001)  End  */

        dim schema$8                     /* (AWD003) Schema            */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy            "
        REM *************************************************************
            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* It is an intrinsic part of the file opening    */
                     /* subroutine.                                    */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  !          D E S C R I P T I O N           *~
            *-----+----------+------------------------------------------*~
            * # 1 ! VBKPOPRT ! PO Print file                            *~
            * # 2 ! VBKMASTR ! PO Header master file                    *~
            * # 3 ! VBKLINES ! PO Line item master file                 *~
            * # 4 ! TXTFILE  ! System Text File                         *~
            * # 5 ! HNYMASTR ! Part Master File                         *~
            * # 6 ! STORNAME ! Store Master File                        *~
            * # 7 ! VBKCHNGH ! PO Changes File- Headers                 *~
            * # 8 ! VBKCHNGL ! PO Changes File- Lines                   *~
            * # 9 ! VBKLNCUR ! Currency Line Item Information           *~
            * #10 ! CURMASTR ! Currency Master File                     *~
            * #11 ! SYSFILE2 ! Caelus Management System Information     *~
            * #12 ! VENDOR   ! Vendor Master File                       *~
            * #13 ! VENPRICE ! Vendor current prices - all vendors, all *~
            * #14 ! VPCMASTR ! Vendor Purchase Contract Master File     *~
/*EWD0001*/ * #15 ! VENDORBF ! Vendor Buy From file                     *~
/*EWD0001*/ * #16 ! ROYAL    ! Dummy Name To Build a Flat File for Fax  *~
/*EWD0001*/ * #17 ! GENCODES ! MASTER TABLE FILE                        *~			
            *************************************************************

            select #1,  "VBKPOPRT",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 36,                                    ~
                        keypos = 13, keylen = 16,                        ~
                        alt key 1, keypos = 4, keylen = 25,              ~
                            key 2, keypos = 1, keylen = 28

            select #2,  "VBKMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1030,                                  ~
                        keypos = 1, keylen = 25,                         ~
                        alt key 1, keypos = 10, keylen = 16

            select #3,  "VBKLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 700,                                   ~
                        keypos = 1, keylen = 28

            select #4, "TXTFILE",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 2024,                                  ~
                        keypos = 1, keylen = 11

            select #5,  "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #6,  "STORNAME",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 3

            select #7,  "VBKCHNGH",                                      ~
                        varc, indexed,                                   ~
                        recsize = 1163,                                  ~
                        keypos = 1, keylen = 32

            select #8,  "VBKCHNGL",                                      ~
                        varc, indexed,                                   ~
                        recsize = 736,                                   ~
                        keypos = 1, keylen = 35

            select #9,  "VBKLNCUR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 100,                                   ~
                        keypos = 5, keylen = 28,                         ~
                        alt key 1, keypos = 1, keylen = 32

            select #10, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4

            select #11, "SYSFILE2",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  500,                                  ~
                         keypos =    1, keylen =  20

            select #12, "VENDOR",                                        ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos = 1, keylen =  9,                         ~
                        alt key  1, keypos = 10, keylen = 30, dup

            select #13, "VENPRICE",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   10, keylen =  59,                     ~
                        alt key  1, keypos =    1, keylen =  34, dup,    ~
                            key  2, keypos =   35, keylen =  34

            select #14, "VPCMASTR",                                      ~
                         varc,     indexed,  recsize =  600,             ~
                         keypos =  10, keylen = 20,                      ~
                         alt key  1, keypos =  1, keylen = 29,           ~
                             key  2, keypos = 60, keylen = 26, dup
/*(EWD0001) -- Add file #15 & OPENCHCK stmt.  Also #16 flat file for faxing */
/*(EWD0001) -- File #17 & OPENCHCK stmt. */							 
            select #15, "VENDORBF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 500,                                   ~
                        keypos = 1, keylen = 15

            select #16, "ROYAL", varc, consec, recsize =  80
			
            select #17, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #18, "USERLCMS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos =    1, keylen =   3,                     ~
                        alt key 1, keypos =  4, keylen = 30, dup

            select #19, "VBKPOPWK",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen =  16                        
                        
            call "SHOSTAT" ("Opening files, one moment please")
            call "OPENCHCK" (# 1, 0%, f2%( 1%), 0%, rslt$( 1%))
            call "OPENCHCK" (# 2, 0%, f2%( 2%), 0%, rslt$( 2%))
            call "OPENCHCK" (# 3, 0%, f2%( 3%), 0%, rslt$( 3%))
            call "OPENCHCK" (# 4, 0%, f2%( 4%), 0%, rslt$( 4%))
            call "OPENCHCK" (# 5, 0%, f2%( 5%), 0%, rslt$( 5%))
            call "OPENCHCK" (# 6, 0%, f2%( 6%), 0%, rslt$( 6%))
            call "OPENCHCK" (#11, 0%, f2%(11%), 0%, rslt$(11%))
            call "OPENCHCK" (#12, 0%, f2%(12%), 0%, rslt$(12%))
            call "OPENCHCK" (#13, 0%, f2%(13%), 0%, rslt$(13%))
            call "OPENCHCK" (#14, 0%, f2%(14%), 0%, rslt$(14%))
            call "OPENCHCK" (#15, 0%, f2%(15%), 0%, rslt$(15%))
            call "OPENCHCK" (#17, 0%, f2%(17%), 0%, rslt$(17%))
            call "OPENCHCK" (#18, 0%, f2%(18%), 0%, rslt$(18%))

            mode% = 1% : gosub open_work
            mode% = 3% : gosub open_work            
						
        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *          Initializes important program variables...       *~
            *************************************************************

            blankdate$ = " "               :       fax% = 0%
            call "DATUFMTC" (blankdate$)

            curamt, iqhold, iqonhand, iqqc, iqqchold, iqrej, origamt = 0
            iqrewrk = 0
            prt% = 0%

* (AWD003) Next 3 lines
            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #17, schema_err%)

            mcon$ = "N"
            call "READ100" (#11, "SWITCHS.CUR", f1%(11))
                if f1%(11) = 0% then L09160
            get #11 using L09110, mcon$
L09110:         FMT POS(21), CH(1)
            if mcon$ <> "Y" then L09160
                call "OPENCHCK" (#9,  0%, f2%(9%) , 0%, rslt$(9%))
                call "OPENCHCK" (#10, 0%, f2%(10%), 0%, rslt$(10%))

L09160:     call "VBKSWTCH" ("POPRDATE", poprdate$, poord, f1%(1))
                 if poprdate$ <> "Y" then poprdate$ = "N"

            call "VBKSWTCH" ("POPRPND ", poprpnd$, poord, f1%(1))
                 if poprpnd$ <> "Y" then poprpnd$ = "N"

            call "VBKSWTCH" ("POSORT  ", poord$, poord, f1%(1))
            poord% = poord
            init(hex(00)) readkey$
            call "PLOWALTS" (#1, readkey$, poord%, 0%, f1%(1))
                if f1%(1) = 0 then L64500

            call "VBKSWTCH" ("POISWAS ", iswas$, poord, f1%(1))
            if iswas$ <> "Y" then iswas$ = "N"
            if iswas$ <> "Y" then L09340
                call "OPENCHCK" (#7, 0%, f2%(7%), 0%, rslt$(7%))
                call "OPENCHCK" (#8, 0%, f2%(8%), 0%, rslt$(8%))
                if f2%(7%) <> 0% or f2%(8%) <> 0% then iswas$ = "N"

L09340:     date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            str(line2$,1%,33%) = "Purchase Order Selection Criteria"
            str(line2$,62%) = " POPRINT: " & str(cms2v$,,8%)

            var1$ = "From            "
            var2$ = "To              "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for selection screen.                *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  6%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then L10200
                         fieldnr% = max(1%, fieldnr% - 1%)
                         goto L10100
L10200:               if keyhit% = 16%  then L65000
                      if keyhit% <> 0%  then L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for selection screen.      *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       L11250
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 6%
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

L11250:     if from_buyer$ <> "ALL" then L11270
                init(hex(00)) from_buyer$ : init(hex(ff)) to_buyer$
L11270:     if from_store$ <> "ALL" then L11290
                init(hex(00)) from_store$ : init(hex(ff)) to_store$
L11290:     if from_vendor$ <> "ALL" then L11310
                init(hex(00)) from_vendor$ : init(hex(ff)) to_vendor$
L11310:     if from_po$ <> "ALL" then L11330
                init(hex(00)) from_po$ : init(hex(ff)) to_po$
L11330:     if from_podate$ <> "ALL" then L11370
                init(hex(00)) from_podate$ : init(hex(ff)) to_podate$
                goto L11400

L11370:     call "DATUFMTC" (from_podate$)
            call "DATUFMTC" (to_podate$)

L11400:     call "SETPRNT" ("VBK001", " ", 0%, 0%)
            call "SHOSTAT" ("Selecting & Printing Purchase Orders")
            select printer(87)

        REM *************************************************************~
            * P L O W   R O U T I N E   F O R   N E X T   O R D E R     *~
            * Get next order in POPRINT file.  Set up & print P.O.      *~
            *************************************************************

            init(hex(00)) readkey$
            if poord% = 0% then str(readkey$) = str(from_po$)
            if poord% = 1% then str(readkey$) = str(from_vendor$) &      ~
                                                str(from_po$)
            if poord% = 2% then str(readkey$) = hex(000000) &            ~
                                str(from_vendor$) & str(from_po$)

         call "REDALT5" (#1, readkey$, poord%, f1%(1%))
             if f1%(1%) = 0% then L64500
                    goto L14070
        read_loop
            call "READNXT1" (#1, f1%(1))
                if f1%(1) = 0 then L65000
L14070:     get #1 using L14080, prtvndr$, prtponbr$, prtflg$, stamp$
L14080:         FMT XX(3), CH(9), CH(16), CH(1), CH(7)
            if prtflg$ = "N" then del_vbkpoprt
            if prtvndr$ < from_vendor$ or                                ~
               prtvndr$ > to_vendor$ then read_loop
            if prtponbr$ < from_po$  or                                  ~
               prtponbr$ > to_po$ then read_loop
            str(headerkey$,1,9) = prtvndr$
            str(headerkey$,10,16) = prtponbr$
            call "SHOSTAT" ("Selecting & Printing Purchase Orders")
            
        REM Get next P.O. header record to print
            call "READ100" (#2, headerkey$, f1%(2))
                 if f1%(2) = 0 then del_vbkpoprt
            sel% = 1%
            gosub L30000              /* Get and format header info */
            if sel% = 0% then read_loop
            pagenumber%, lastseq% = 0%
            total, chtotal = 0
/* Move delete here because can not have to 'hold' at one time!!  */
            delete #1                 
            gosub L18000              /* Page header Routine  */
            gosub head_text

        REM Plow through and print line items
            init(hex(00)) readkey$
            str(readkey$,  1) = vencode$
            str(readkey$, 10) = ponumber$

L14270:     call "PLOWNEXT" (#3,  readkey$, 25%, f1%(3))
                 if f1%(3) = 0 then L14750
            gosub L32000              /* Get and format line item   */
            if iswas$ <> "Y" then L14340
                lastl% = lastseq% + 1%
                nextl% = seq% - 1%
                if lastl% <> seq% then gosub line_deletes
L14340:     lastseq% = seq%
            if prtflg$ <> "R" then L14390
            if revnbr$ <> irev$ then L14710
                chtotal = chtotal + ext

L14390:     if lcntr% > 51 then gosub L18000      /* Page header  */
            call "DATEFMT" (datedue$)
            partkey$ = part$
*           if vpart$ <> " " then part$ = vpart$       /* (EWD) - Begin */  
            print skip(1)           
            if istat$ = "W" and price = 0 then price$ = "    ADVISE"
            print using L60180, item$, irev$, vpart$, meas$, datedue$,    ~
                               qtyorig$, price$, ext$
            print using L60185, part$                   /* (EWD) Begin */
            print using L60210, descr$
            lcntr% = lcntr% + 4%                        /* (EWD) End   */
                gosub fax_blank                         /* (EWD0001) Begin  */
            str(a$,1%,3%)   = item$
            str(a$,5%,2%)   = irev$
            str(a$,10%,25%) = vpart$
            str(a$,36%,4%)  = meas$
            str(a$,41%,8%)  = datedue$
            str(a$,50%,9%)  = str(qtyorig$,2%,10%)
            str(a$,59%,10%) = price$
            str(a$,69%,10%) = str(ext$,3%,10%)
                gosub print_line                        
            str(a$,10%,25%) = part$
                gosub print_line
            str(a$,10%,32%) = descr$
                gosub print_line                       /* (EWD0001) End  */
            if contract_id$ = " " then L14490
                print using L60222, contract_id$, contract_line$,         ~
                                                     contract_descr$
            str(a$,1%,18%)  = "        Contract: "     /* (EWD0001) Begin  */
            str(a$,19%,16%) = contract_id$
            str(a$,37%,4%)  = contract_line$
            str(a$,43%,30%) = contract_descr$
                gosub print_line                       /* (EWD0001) End  */
            lcntr% = lcntr% + 1%
L14490:     if part$ = " " then L14640
            if seq%  = 0% then L14530
            if iswas$ = "Y" and f1%(7) = 1% then gosub line_aorc
            if f1%(7) = 0% and poprpnd$ = "Y" then gosub received_and_due
L14530:     call "READ100" (#5, partkey$, f1%(5))
              if f1%(5) = 0% then L14640
            get #5 using L14560, pntextid$
L14560:         FMT POS(98), CH(4)
L14570:     cntr% = lcntr%
            call "TXTPRINT" (#4, f2%(4), 87%, pntextid$, "VBK001", 5%,   ~
                  lcntr%, 54%, "N", " ", stat%)
                                                       /* (EWD0001)     */                  
            if pur_flag$ = "Y" then                                      ~
            call "PURPRINT" (#4, f2%(4), #16, 82%, pntextid$, "VBK001",  ~
                  5%, cntr%, 54%, "N", " ", stat%)                  
            if stat% = 0% then L14640
            gosub L18000
            print skip(1)  :  lcntr% = lcntr% + 1%
            gosub fax_blank                           /* (EWD0001)   */
            goto L14570

L14640:     cntr% = lcntr%
            call "TXTPRINT" (#4, f2%(4), 87%, textid2$, "VBK001", 5%,    ~
                  lcntr%, 54%, "N", " ", stat%)
                                                       /* (EWD0001)   */
            if pur_flag$ = "Y" then                                      ~
            call "PURPRINT" (#4, f2%(4), #16, 82%, textid2$, "VBK001",   ~
                  5%, cntr%, 54%, "N", " ", stat%)                  
            if stat% = 0% then L14660
            gosub L18000
            print skip(1)  :  lcntr% = lcntr% + 1%
            gosub fax_blank                           /* (EWD0001)  */
            goto L14640

L14660:     if vpart$ = " " then L14710
            readkey3$ = str(partkey$) & str(vencode$) & str(vpart$)
            call "READ100" (#13, readkey3$, f1%(13%))
              if f1%(13%) = 0% then L14710
            get #13 using L14668, vprtext$
L14668:         FMT POS(130), CH(4)
L14670:     cntr% = lcntr%
            call "TXTPRINT" (#4, f2%(4), 87%, vprtext$, "VBK001", 5%,    ~
                  lcntr%, 54%, "N", " ", stat%)
                                                       /* (EWD0001)   */
            if pur_flag$ = "Y" then                                      ~
            call "PURPRINT" (#4, f2%(4), #16, 82%, vprtext$, "VBK001",   ~
                  5%, cntr%, 54%, "N", " ", stat%)                  
            if stat% = 0% then L14710
            gosub L18000
            print skip(1)  :  lcntr% = lcntr% + 1%
            gosub fax_blank                           /* (EWD0001)  */
            goto L14670

L14710:     total = total + ext
            goto L14270


L14750: REM Space to the bottom of the page and print totals
        if lcntr% > 44% then gosub L18000
        print skip(5)
        print using L60210, "Please Acknowledge Receipt"
        print
        print using L60210, "Date Received  ___________"
        print
        print using L60210, "Date to Ship  ____________"
        lcntr% = lcntr% + 10%
                                                       /* (EWD0001) Begin */
        gosub fax_blank  :  gosub fax_blank  :  gosub fax_blank
        gosub fax_blank  :  gosub fax_blank
        str(a$,10%,26%) = "Please Acknowledge Receipt"
            gosub print_line
            gosub fax_blank
        str(a$,10%,26%) = "Date Received  ___________"
            gosub print_line
            gosub fax_blank
        str(a$,10%,26%) = "Date to Ship  ____________"
            gosub print_line
            gosub fax_blank
                                                       /* (EWD0001) End  */
            gosub format_stuff
            if iswas$ <> "Y" then L14810
                lastl% = lastseq% + 1% : nextl% = 999%
                convert vbklseq$ to nextl%, data goto L14800 /* Hope Not */
L14800:         if lastl% <= nextl% then gosub line_deletes
L14810:     if prtflg$ <> "R" then L14900
            if lcntr% > 54% then gosub L18000
            print skip (54% - lcntr%)
            for i% = lcntr% to 52%                        /* (EWD0001)  */
                gosub fax_blank                                           
            next i%                                       /* (EWD0001)  */
            if mcon$ = "Y" then L14870
            print using L60290, chtotal$
            str(a$,1%,42%)  = "  "                        /* (EWD0001)  */
            str(a$,44%,24%) = "PRINTED ITEMS TOTAL.... "                  
            str(a$,69%,12%) = chtotal$                                    
                gosub print_line                          /* (EWD0001)  */
            goto L14880
L14870:     print using L60260, currdesc$, chtotal$
            str(a$,1%,9%) = "ORDER IN "                   /* (EWD0001)  */
            str(a$,11%,32%) = currdesc$                                   
            str(a$,44%,24%) = "PRINTED ITEMS TOTAL.... "                  
            str(a$,69%,12%) = chtotal$                                     
                gosub print_line                                          
L14880:     print            : gosub fax_blank            /* (EWD0001)  */
            goto L14930
L14900:     print skip (54% - lcntr%)
            for i% = lcntr% to 52%                        /* (EWD0001)  */
                gosub fax_blank                                           
            next i%                                                         
            if mcon$ = "Y" then print using L60320, currdesc$ else print

            if mcon$ = "Y" then str(a$,1%,9%) = "ORDER ID "  /* (EWD0001)  */
            if mcon$ = "Y" then str(a$,11%,32%) = currdesc$                  
            if mcon$ = "Y" then gosub print_line else gosub fax_blank              
            print
            gosub fax_blank                               /* (EWD0001)  */
L14930:     print using L60230, total$
            if pur_flag$ = "Y" then gosub get_buyer_name
            str(a$,7%,30%)  = buy_name$                   /* (EWD0001)  */
            str(a$,66%,12%) = total$            
                gosub print_line                                             
            if pur_flag$ = "Y" then print using L60230, "   F A X E D"
        del_vbkpoprt
            if pur_flag$ = "Y" then gosub complete_fax
                                                        /* (EWD0001) */

            goto read_loop

        format_stuff
            if mcon$ = "N" then call "CONVERT" (chtotal, 2.2, chtotal$)  ~
                else call "CURRFMT" (chtotal, currency$, chtotal$, "Y")
            if mcon$ = "N" then call "CONVERT" (total, 2.2, total$)      ~
                else call "CURRFMT" (total, currency$, total$, "Y")
            call "DESCRIBE" (#10, currency$, currdesc$, 1%, f1%(10))
                if currdesc$ = " " then currdesc$ = "(" & currency$ & ")"
            return

        get_buyer_name
           if str(buyer$,1%,3%) = " " then return
           read #18, key = buyer$, using L14500, buy_name$, eod goto L14510
L14500:        FMT XX(3), CH(20)
      
L14510:    return       

        REM *************************************************************~
            * P O   I S / W A S   R O U T I N E S                       *~
            *************************************************************
        line_aorc    /* Line Item has been added or deleted            */
            cline$ = " "
            if lsts$ = "C" then L16120
            if hsts$ = "A" then return
                cline$ = "REVISION: ABOVE LINE ADDED"
                gosub print_cline
                if poprpnd$ = "Y" then gosub received_and_due
                return

L16120
*        This section handles changed lines.
            call "READ100" (#8, str(vencode$,,9) & str(ponumber$,,16) &  ~
                                str(iseq$,,3) & stamp$, f1%(8))
            if f1%(8) = 0% then L16340              /* Check qtys flag */
                get #8 using L16190, cdescr$, cqtyrecd, cqtyonord, cprice,~
                                    cext, cdatedue$
                     cext = cext
L16190:              FMT XX(92), CH(32), XX(12), 2*PD(14,4), PD(14,7),   ~
                         PD(14,4), XX(9), CH(6)
                if cdescr$ = descr$ then L16240
                  cline$ = "REVISION: DESCRIPTION CHANGED FROM " & cdescr$
                  gosub print_cline
L16240:         ordnow   =  qtyonord +  qtyrecd
                ordwas   = cqtyonord + cqtyrecd
                if abs(ordnow - ordwas) < .01 then L16340
                     ordchng = ordnow - ordwas
                     ordchng = round(ordchng / factor, 4)
                     call "CONVERT" (ordchng, -2.4, qtyonorder$)
                     call "STRING" addr("LJ", qtyonorder$, 10%)
                  cline$ = "REVISION: ORDER QUANTITY CHANGED BY "        ~
                                      & qtyonorder$
                  gosub print_cline
L16340:         if poprpnd$ = "Y" then gosub received_and_due
                if f1%(8) = 0% then return
                if cprice = price then L16410
                  cprice = round(cprice * factor, 4)
                  call "CONVERT" (cprice, -2.4, price$)
                  cline$ = "REVISION: UNIT PRICE CHANGED FROM " & price$
                  gosub print_cline
L16410:         call "DATEFMT" (cdatedue$)
                if cdatedue$ = datedue$ then return
                  cline$ = "REVISION: DUE DATE CHANGED FROM " & cdatedue$
                  gosub print_cline
                  return

        print_cline  /* Print Change Line */
            if lcntr% > 54% then gosub L18000
            if lcntr% <> 21% then L16510
            print skip(1)  :  lcntr% = lcntr% + 1%
            gosub fax_blank                                /* (EWD0001) */
L16510:     print col(10), cline$
            str(a$,10%,70%) = cline$
                gosub print_line
            lcntr% = lcntr% + 1%
            return

        line_deletes
            readkey2$ = str(vencode$,,9) & str(ponumber$,,16) &          ~
                       "###" & stamp$
            for d% = lastl% to nextl%
                     convert d% to str(readkey2$,26,3), pic(###)
                     call "READ100" (#8, readkey2$, f1%(8))
                     if f1%(8) = 0% then L16700 /* Gone, but before last */
                          get #8 using L16630, part2$
L16630:                        FMT XX(36), XX(31), CH(25)
                          cline$ = " " : gosub print_cline
                          cline$ = " " & str(readkey2$,26,3) & "    "    ~
                                   & str(part2$) & "  **DELETED**"
                          if lcntr% > 54% then gosub L18000
                          print cline$
                          str(a$,1%,80%) = cline$       /* (EWD0001) End  */
                              gosub print_line
                          lcntr% = lcntr% + 1%
L16700:     next d%
            return

        received_and_due
            if qtyrecd = 0 then return /* Only print if there has been */
                                       /* receipts against this line.  */
            call "STRING" addr("LJ", qtyrecd$, 10%)
            call "STRING" addr("LJ", qtyonord$, 10%)
            cline$ = "RECEIVED TO DATE  = " & qtyrecd$
            gosub print_cline
            cline$ = "BACKORDER NET DUE = " & qtyonord$
            gosub print_cline
            return

L18000: REM *************************************************************~
            *          P . O .   H E A D E R   P R I N T                *~
            *************************************************************

            prt% = 1%                  :    slcntr% = lcntr%
            print page
            pagenumber% = pagenumber% + 1

            print using L60030, ponumber$, revnbr$
            print skip(2)
            print using L60060, orderdate$, pagenumber%
            print skip(3)
            print using L60090, venname$, sname$
            print using L60090, addr$(1), saddr$(1)
            print using L60090, addr$(2), saddr$(2)
            print using L60090, addr$(3), saddr$(3)
            print using L60090, addr$(4), saddr$(4)
            print using L60090, addr$(5), saddr$(5)
            print skip(3)
            print using L60120, vencode$, shipvia$, fob$
            print skip(2)
            if poprdate$ = "N" then revdate$ = " "
            print using L60150, conname$, buyer$, confyes$, taxyes$,      ~
                               revdate$
            print skip(1)
            lcntr% = 21%
/* (EWD0001)  Begin  */
            gosub check_vendor
	    if pur_flag$ = "N" then return

            if pagenumber% <> 1% then gosub page_break
            if pagenumber% <> 1% then goto not_first
            gosub print_line
            lcntr% = lcntr% + 1%
            gosub print_line
            lcntr% = lcntr% + 1%
not_first
            str(a$,55%,16%) = ponumber$
	    str(a$,72%,2%)  = revnbr$
	       gosub print_line
	    gosub fax_blank
	    str(a$,59%,8%) = orderdate$
	    convert pagenumber% to str(a$,72%,3%), pic(##0)
	       gosub print_line
	    gosub fax_blank     :     gosub fax_blank
	    str(a$,6%,31%)  = venname$
	    str(a$,49%,31%) = sname$
	       gosub print_line
	    for i% = 1% to 5%
	        str(a$,6%,31%)  = addr$(i%)
                str(a$,49%,31%) = saddr$(i%)
		gosub print_line
            next i%
	    gosub fax_blank     :     gosub fax_blank     :     gosub fax_blank
	    str(a$,2%,9%)   = vencode$
            str(a$,16%,30%) = shipvia$
            str(a$,50%,30%) = fob$	
	       gosub print_line
	    gosub fax_blank     :     gosub fax_blank
            str(a$,2%,20%)  = conname$
            str(a$,26%,3%)  = buyer$
            str(a$,35%,3%)  = confyes$
            str(a$,45%,3%)  = taxyes$
            str(a$,53%,23%) = revdate$
               gosub print_line
            gosub fax_blank
        return 
			
        check_vendor
            if fax% <> 0% then return            /* So will not check vendor */
                                                 /* twice on same PO         */
            init(" ") pur_key1$, pur_no$, pur_flag$
            pur_no% = 0%
            pur_flag$ = "N"
            if fax$ = "N" then return
	    gosub read_buy_from
            call "SPCSMASH" (pur_no$)
            if len(pur_no$) < 7% then goto L18030
            convert pur_no$ to pur_no%, data goto L18030
			
            a$        = all(hex(20))
            date$     =  date
            library$  = "APCDATA "
            volume$   = "CARLOS"
            call "DATFMTC"  (date$)
            counter%, error% = 0%
            gosub set_file_name
               if error% <> 0% then goto L65500
            gosub open_file
REM            gosub setup_fax            /*  (EWD002)  */
        return
L18030:     pur_flag$ = "N"
        return
        
        read_buy_from
	    str(pur_key1$,1%,9%)  =  prtvndr$
	    str(pur_key1$,10%,6%) =  venbfcode$			
            read #15, key = pur_key1$, using L18010, pur_no$,             ~
			                           eod goto L18020
L18010:       FMT POS(341), CH(10)
            if str(pur_no$,1%,10%) <> " " then pur_flag$ = "Y"
L18020:	return	

/* (EWD0001)  End  */				
        head_text    /*  Check for header text printing   */
          for n% = 1% to 3%
            if cpytxtname$(n%) = " " then L18430
            init(" ") readkey$
            str(readkey$,1%,1%) = "C"
            str(readkey$,2%,10%) = cpytxtname$(n%)
            call "READ100" (#4, readkey$, f1%(4%))
                if f1%(4%) = 0% then L18430
            get #4 using L18360, cpytextid$
L18360:         FMT POS(42), CH(04)
L18370:     cntr% = lcntr%
            call "TXTPRINT" (#4, f2%(4%), 87%, cpytextid$, "VBK001", 10%,~
                  lcntr%, 55%, "N", " ", stat%)
                                                       /* (EWD0001)  */
            if pur_flag$ = "Y" then                                      ~
            call "PURPRINT" (#4, f2%(4), #16, 82%, cpytextid$, "VBK001", ~
                  5%, cntr%, 54%, "N", " ", stat%)                  
            if stat% = 0% then L18430
            gosub L18000
            print skip(1)  :  lcntr% = lcntr% + 1%
            goto L18370
L18430:   next n%

L18450:     cntr% = lcntr%
            call "TXTPRINT" (#4, f2%(4), 87%, textid1$,   "VBK001", 10%, ~
                  lcntr%, 55%, "N", " ", stat%)
                                                       /* (EWD0001)  */
            if pur_flag$ = "Y" then                                      ~
            call "PURPRINT" (#4, f2%(4), #16, 82%, textid1$, "VBK001",   ~
                  5%, cntr%, 54%, "N", " ", stat%)
            if stat% = 0% then L18520
            gosub L18000
            print skip(1)  :  lcntr% = lcntr% + 1%
            goto L18450

L18520:     readkey3$ = str(vencode$)
            call "READ100" (#12, readkey3$, f1%(12%))
                if f1%(12%) = 0% then L18590
            get #12 using L18528, ventext$
L18528:         FMT POS(490), CH(4)
L18530:     cntr% = lcntr%
            call "TXTPRINT" (#4, f2%(4), 87%, ventext$,   "VBK001", 10%, ~
                  lcntr%, 55%, "N", " ", stat%)
            if pur_flag$ = "Y" then                                      ~
            call "PURPRINT" (#4, f2%(4), #16, 82%, ventext$, "VBK001",   ~
                  5%, cntr%, 54%, "N", " ", stat%)                  
            if stat% = 0% then L18590
            gosub L18000
            print skip(1)  :  lcntr% = lcntr% + 1%
            goto L18530
L18590:     return

        page_break
            i% = slcntr%
            if pagenumber% > 2% then i%, slcntr% = 55%        /*  (EWD002)        */
                                                          /* Needs to be here */
                                                          /* for pages after 2*/
                                                          /* to line up!!!!   */
            if i% >= 53% then goto no_blanks              
            for i% = slcntr% to 53%                       /* (EWD0001)  */
                gosub fax_blank                
                slcntr% = slcntr% + 1%
            next i%                                     
no_blanks
            str(a$,10%,32%) = " >>>>>  C O N T I N U E D  <<<<<"
            gosub print_line
                slcntr% = slcntr% + 1%
            gosub fax_blank
                slcntr% = slcntr% + 1%
            gosub fax_blank
                slcntr% = slcntr% + 1%

            gosub get_buyer_name
            str(a$,7%,30%)  = buy_name$
            gosub print_line
                slcntr% = slcntr% + 1%            

            i% = slcntr%
            for i% = slcntr% to 63%                      
                gosub fax_blank
                slcntr% = slcntr% + 1%                           
            next i%                                       /* (EWD0001)  */
        return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20150,         /* Buyer ID Range         */~
                              L20200,         /* Store Number Range     */~
                              L20260,         /* Vendor Number Range    */~
                              L20320,         /* PO Number Range        */~
                              L20380,         /* PO Date Range          */~
                              L20390          /* Do you want to fax?    */                              
        return

L20150: REM Def/Enable Buyer ID Code Range
            if from_buyer$ = " " then from_buyer$ = "ALL"
            inpmessage$ = "Enter Buyer ID Code Range."
            return

L20200: REM Def/Enable Store Number Range
            if from_store$ = " " then from_store$ = "ALL"
            inpmessage$ = "Enter Store Number Range, Blank Or '?' To Se"&~
                          "lect."
            return

L20260: REM Def/Enable Vendor Number Range
            if from_vendor$ = " " then from_vendor$ = "ALL"
            inpmessage$ = "Enter Vendor Number Range, Blank Or '?' To S"&~
                          "elect."
            return

L20320: REM Def/Enable Purchase Order Number Range
            if from_po$ = " " then from_po$ = "ALL"
            inpmessage$ = "Enter Purchase Order Range, Blank Or '?' To "&~
                          "Select."
            return

L20380: REM Def/Enable Purchase Order Date Range
            if from_podate$ = " " or ~
               from_podate$ = blankdate$ then from_podate$ = "ALL"
            inpmessage$ = "Enter Purchase Order Date Range."
            return

L20390: REM Do You Want to Fax?                          FAX$ (EWD0001) 
            if fax$ = " " then fax$ = "N"
            inpmessage$ = "Do You Want to Fax (Y)or(N)?"
            return
        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") errormsg$, inpmessage$,                            ~
                      from_buyer$, from_store$, from_vendor$,            ~
                      from_po$, from_podate$, to_buyer$, to_store$,      ~
                      to_vendor$, to_po$, to_podate$, fax$
             return

        REM *************************************************************~
            * This program contains valuable trade secrets and          *~
            *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
            *  embodying substantial creative efforts and confidential  *~
            *  information.  Unauthorized use, copying, decompiling,    *~
            *  translating, disclosure, or transfer of it is prohibited.*~
            *  Copyright (c) 1993  an unpublished work by CAELUS,       *~
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

L30000: REM *************************************************************~
            * G E T   O R D E R   H E A D E R   A N D   F O R M A T     *~
            *-----------------------------------------------------------*~
            * Gets order header which PLOWNEXT routine located, and     *~
            * print-format as much of the information as you can. This  *~
            * includes formatting all the numbers and dates.            *~
            *************************************************************

            init (" ") addr$(), saddr$(), straddr$(), shpaddr$(),        ~
                       hsts$, currency$, currdesc$, changedate$, revdate$
            currency$ = statutory$
            oldvbklseq% = 1000%
            get   #2, using L30810,                                       ~
                      vencode$, ponumber$, venname$, addr$(1), addr$(2), ~
                      addr$(3), addr$(4), addr$(5), conname$, payacct$,  ~
                      lastinvnr$, str(variable$(), 1), orderdate$,       ~
                      canceldate$, defjob$, defduedate$, defstore$,      ~
                      recdate$, buyer$, confflag$, frterms$, fob$, txbl$,~
                      shipvia$, ventype$, venbfcode$, textid1$, revnbr$, ~
                      chgdate$, dollmax$, expdate$, vbklseq$, origdate$, ~
                      origid$, changedate$, changeid$, origamt,          ~
                      curamt, cpytxtname$(1%), shpname$, shpaddr$(1),    ~
                      shpaddr$(2), shpaddr$(3), shpaddr$(4), shpaddr$(5),~
                      cpytxtname$(2%), cpytxtname$(3%)

            if orderdate$ >= from_podate$ and                            ~
               orderdate$ <= to_podate$ then L30236 else L30244
L30236:     if defstore$ >= from_store$ and                              ~
               defstore$ <= to_store$ then L30240 else L30244
L30240:     if buyer$ >= from_buyer$ and                                 ~
               buyer$ <= to_buyer$ then L30250
L30244:            sel% = 0%
                   return

L30250:     call "DATEFMT"  (orderdate$)
            call "DATEFMT" (changedate$)
                revdate$ = "REVISION DATE: " & changedate$
            call "GLFMT" (payacct$)
            call "DATEFMT" (canceldate$)
            call "DATEFMT" (defduedate$)
            confyes$ = " "
            taxyes$ = "NO"
            if confflag$ = "Y" then confyes$ = "YES"
            if txbl$ = "Y" then taxyes$ = "YES"

            if str(addr$(5%),17%,1%) <> " " or str(addr$(5%),16%,1%) <>  ~
                " " or pos(str(addr$(5%),27%,4%) = " ") > 0% then L30420
                  temp$ = str(addr$(5%),27%,4%)
                  str(addr$(5%),28%,4%) = temp$
                  str(addr$(5%),27%,1%) = "-"
L30420:     call "SPCESMSH" (addr$(5%), 2%)
            call "LINSMASH" (addr$( ))

            if shpname$ <> " " then L30580
            call "READ100" (#6, defstore$, f1%(6))
                if f1%(6) = 0 then L30700
            get #6 using L30500, strname$, straddr$(1), straddr$(2),      ~
                                straddr$(3)
L30500:         FMT XX(3), XX(30), CH(30), 3*CH(30)

            for i = 1 to 3
                saddr$(i) = straddr$(i)
            next i
            sname$ = strname$
            goto L30700

L30580:     sname$ = shpname$
            if str(shpaddr$(5%),17%,1%) <> " " or                        ~
               str(shpaddr$(5%),16%,1%) <> " " or                        ~
               pos(str(shpaddr$(5%),27%,4%) = " ") > 0% then L30650
                  temp$ = str(shpaddr$(5%),27%,4%)
                  str(shpaddr$(5%),28%,4%) = temp$
                  str(shpaddr$(5%),27%,1%) = "-"
L30650:     call "SPCESMSH" (shpaddr$(5%), 2%)
            for i = 1 to 5
                saddr$(i) = shpaddr$(i)
            next i

L30700:     call "LINSMASH" (saddr$( ))

            if iswas$ = "N" then return
                call "READ100" (#7, str(vencode$,,9) &                   ~
                                     str(ponumber$,,16) & stamp$, f1%(7))
                if f1%(7) = 0% then return
                     get #7 using L30770, hsts$, oldvbklseq$
L30770:                   FMT POS(33), CH(1), POS(623), CH(3)
                     convert oldvbklseq$ to oldvbklseq%, data goto L30790
L30790:              return

L30810:     FMT CH(9),                   /* Vendor code                */~
                CH(16),                  /* Purchase order number      */~
                CH(30),                  /* Vendor name                */~
                5*CH(30),                /* Address(3) & buy from addr */~
                CH(20),                  /* Contact's name             */~
                CH(09),                  /* Payables account default   */~
                CH(16),                  /* Last invoice number        */~
                CH(200),                 /* Variable fields (10 * 20)  */~
                CH(6),                   /* Date of order              */~
                CH(6),                   /* Cancellation date          */~
                CH(8),                   /* Default job number         */~
                CH(6),                   /* Default due date           */~
                CH(3),                   /* Default store number       */~
                CH(6),                   /* Receiving Date             */~
                CH(3),                   /* PO Buyer                   */~
                CH(1),                   /* Confirmation Flag Y/N      */~
                CH(1),                   /* Freight Terms              */~
                CH(30),                  /* F.O.B. Point               */~
                CH(1),                   /* Taxable purchase           */~
                CH(30),                  /* Ship Via                   */~
                CH(4),                   /* Vendor Type                */~
                CH(6),                   /* Vendor buy from code       */~
                CH(4),                   /* Text ID                    */~
                CH(2),                   /* Revision Number            */~
                CH(6),                   /* Change Date                */~
                CH(10),                  /* Maximum Dollar Limit       */~
                CH(6),                   /* Expiration Date            */~
                CH(3),                   /* VBK Lines Seq Number       */~
                CH(6),                   /* Original date entered      */~
                CH(3),                   /* Original User ID           */~
                CH(6),                   /* Last PO Change date        */~
                CH(3),                   /* Change User ID             */~
                2*PD(14,4),              /* Amounts orig & current     */~
                CH(10),                  /* Copy Text Name (1)         */~
                6*CH(30),                /* Ship to Address            */~
                XX(01),                  /* Print Flag                 */~
                XX(09),                  /* Price Cost Variance Acct   */~
                2*CH(10),                /* Copy Text Name (2 & 3)     */~
                CH(184)                  /* Filler                     */

L32000: REM *************************************************************~
            *           G E T   O R D E R   L I N E   I T E M           *~
            *-----------------------------------------------------------*~
            * Gets an order line item from the file.  All the print-    *~
            * formatting is handled by the routine that calls this.     *~
            *************************************************************

            get   #3,  using L32480, ivend$, ipo$, iseq$, item$, part$,   ~
                       descr$, cat$, qtyorig, qtyrecd, qtyonord, price,  ~
                       ext, acct$, datedue$, daterecd$, datenext$, lot$, ~
                       job$, store$, ohpost$, itype$, pslip$, vpart$,    ~
                       meas$, factor, iduedate$, textid2$, istat$, irev$,~
                       idelto$, iwhoreq$, iliref$, nobefore$, noafter$,  ~
                       ibuyer$, iqhold, iqqc, iqqchold, iqonhand, iqrej, ~
                       iqrewrk, contract_id$, contract_line$
            if iswas$ = "Y" then item$ = iseq$

                readcur$ = readkey$
            call "READ100" (#9, readcur$, f1%(9))
                if f1%(9) <> 0% then get #9 using L32210, currency$,      ~
                                                         price, ext
L32210:         FMT CH(4), POS(33), PD(14,7), PD(14,4)

            if part$ = " " then L32410    /* Description only line      */

            seq% = 0% : convert iseq$ to seq%, data goto L32280
            if seq% > oldvbklseq% then lsts$ = "A" else lsts$ = "C"

L32280: REM COMPUTE EXTENSION FROM QUANTITY BACKORDERED
*          IF FACTOR < .0001 THEN FACTOR = 1
            if factor > 100000 then factor = 1
            call "CONVERT" (round(qtyrecd / factor,4), 2.4, qtyrecd$ )
            call "CONVERT" (round(price * factor,4), 2.4, price$)
            call "CONVERT" (round(qtyorig / factor,4), 2.4, qtyorig$)
            call "CONVERT" (round(qtyonord/ factor,4), 2.4, qtyonord$)
            ext = round(qtyorig * price, 2)
                if mcon$ = "N" then call "CONVERT" (ext, 2.4, ext$)      ~
                   else call "CURRFMT" (ext, currency$, ext$, "N")
            call "GLFMT" (acct$)
            call "DESCRIBE" (#14, str(contract_id$) & contract_line$,    ~
                                          contract_descr$, 0, f1%(14%))
            return

L32410:     init(" ")  item$, part$, cat$, acct$, datedue$, lot$,        ~
                       job$, daterecd$, datenext$,  job$,                ~
                       qtyrecd$, price$, store$, meas$,                  ~
                       qtyorig$, qtyonord$, ext$, contract_id$,          ~
                       contract_line$, contract_descr$
            ext = 0
            return

L32480:     FMT CH(9),                   /* Vendor code                */~
                CH(16),                  /* Purchase order number      */~
                CH(3),                   /* Sequence number            */~
                CH(3),                   /* Item number                */~
                CH(25),                  /* Part number                */~
                CH(32),                  /* Description                */~
                CH(4),                   /* Category code              */~
                PD(14,4),                /* Quantity originally ordered*/~
                PD(14,4),                /* Quantity shipped           */~
                PD(14,4),                /* Quantity backordered       */~
                PD(14,7),                /* Unit price                 */~
                PD(14,4),                /* Extension                  */~
                CH(9),                   /* Purchase account number    */~
                CH(6),                   /* Date due information       */~
                CH(6),                   /* Date of last receipt       */~
                CH(6),                   /* Date of next shipment      */~
                CH(6),                   /* Lot number                 */~
                CH(8),                   /* Job number                 */~
                CH(3),                   /* Store number               */~
                CH(1),                   /* On Hand Posting Option     */~
                CH(3),                   /* Inv Type Designator        */~
                CH(16),                  /* Last Pack Slip #           */~
                CH(25),                  /* Vendor Part Number         */~
                XX(66),                  /* Filler                     */~
                CH(4),                   /* Vend Unit Of Measure       */~
                PD(14,4),                /* Conversion Factor          */~
                CH(6),                   /* Original Due Date          */~
                CH(4),                   /* Text ID                    */~
                CH(1),                   /* Line Item Status Flag      */~
                CH(2),                   /* Line Item Rev Number       */~
                CH(20),                  /* Deliver to name            */~
                CH(20),                  /* Who Requisitioned          */~
                CH(5),                   /* Line Item Ref Number       */~
                CH(6),                   /* No Receipt before date     */~
                CH(6),                   /* No Receipt after date      */~
                CH(3),                   /* Buyer Code                 */~
                PD(14,4),                /* Qty in Recv Hold           */~
                PD(14,4),                /* Qty in QC                  */~
                PD(14,4),                /* Qty in QC Hold             */~
                PD(14,4),                /* Qty to On Hand             */~
                PD(14,4),                /* Qty rejected               */~
                PD(14,4),                /* Qty to rework              */~
                POS(561),                                                ~
                CH(16),                  /* Contract ID  Code          */~
                CH( 4)                   /* Contract Line Number       */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
            gosub set_pf1
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            if fieldnr% > 0% then lfac$(fieldnr%) = hex(81)

L40120:     accept                                                       ~
               at (01,02), "Print Purchase Orders",                      ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(ac)), line2$                 , ch(79),~
               at (04,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (06,25), fac(hex(ac)),   var1$                , ch(16),~
               at (06,43), fac(hex(ac)),   var2$                , ch(16),~
                                                                         ~
               at (07,02), "Buyer ID Code",                              ~
               at (07,25), fac(lfac$(1%)), from_buyer$          , ch(03),~
               at (07,43), fac(lfac$(1%)), to_buyer$            , ch(03),~
                                                                         ~
               at (08,02), "Store Number",                               ~
               at (08,25), fac(lfac$(2%)), from_store$          , ch(03),~
               at (08,43), fac(lfac$(2%)), to_store$            , ch(03),~
                                                                         ~
               at (09,02), "Vendor Number",                              ~
               at (09,25), fac(lfac$(3%)), from_vendor$         , ch(09),~
               at (09,43), fac(lfac$(3%)), to_vendor$           , ch(09),~
                                                                         ~
               at (10,02), "Purchase Order Number",                      ~
               at (10,25), fac(lfac$(4%)), from_po$             , ch(16),~
               at (10,43), fac(lfac$(4%)), to_po$               , ch(16),~
                                                                         ~
               at (11,02), "Purchase Order Date",                        ~
               at (11,25), fac(lfac$(5%)), from_podate$         , ch(10),~
               at (11,43), fac(lfac$(5%)), to_podate$           , ch(10),~
                                                                         ~
               at (12,02), "Do You Want to Fax?",                        ~
               at (12,25), fac(lfac$(6%)), fax$                 , ch(01),~
                                                                         ~                                                                         
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 13% then L40530
                   call "MANUAL" ("POPRINT")
                   goto L40120

L40530:        if keyhit% <> 15% then L40570
                   call "PRNTSCRN"
                   goto L40120

L40570:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40760     /*  Input Mode             */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
            if fieldnr% = 1% then L40720
                str(pf$(3%),64%)  = " " : str(pfkeys$,16%,1%) = hex(ff)
L40720:     if fieldnr% > 1% then L40740
                str(pf$(2),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40740:     return

L40760: if fieldnr% > 0% then L40860  /*  Edit Mode - Select Fld */
            inpmessage$ = edtmessage$
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
                     "                       (16)Print POs   "
            pfkeys$ = hex(01ffffffffffffffffffffff0dff0f1000)
            return
L40860:                          /*  Edit Mode - Enabled    */
            pf$(1%)= "(1)Start Over                           " &        ~
                     "                       (13)Instructions"
            pf$(2%)= "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3%)= "                                        " &        ~
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
            on fieldnr% gosub L50150,         /* Buyer ID Code          */~
                              L50210,         /* Store Number           */~
                              L50460,         /* Vendor Number          */~
                              L50710,         /* PO Number              */~
                              L50960,         /* PO Date                */~
                              L51500          /* Do You Want to Fax?    */                              
        return

L50150: REM Test for Buyer ID Code Range
            errormsg$ = " "
            call "TESTRNGE" (from_buyer$, to_buyer$, " ", " ", errormsg$)
            return

L50210: REM Test for Store Number Range
            errormsg$ = " "
            if from_store$ <> "ALL" then L50260
                to_store$ = " "
                return
L50260:     call "TESTRNGE" (from_store$, to_store$, " ", " ",           ~
                             errormsg$, #6)
            return

L50460: REM Test for Vendor Number Range
            errormsg$ = " "
            if from_vendor$ <> "ALL" then L50510
                to_vendor$ = " "
                return
L50510:     call "TESTRNGE" (from_vendor$, to_vendor$, " ", " ",         ~
                             errormsg$, #12)
            return

L50710: REM Test for Purchase Order Number Range
            errormsg$ = " "
            if from_po$ <> "ALL" then L50760
                to_po$ = " "
                return
L50760:     if from_po$ = "?" then from_po$ = " "
            pdescr$ = hex(06) & "Select Starting Purchase Order Number"
            call "PLOWCODE" (#1, from_po$, pdescr$, 0%, 0.0, f1%(1%))
             if f1%(1%) = 1% then L50830
                 if from_po$ <> " " then L50830
                     errormsg$ = "Starting PO Number CANNOT Be Blank"
                     return
L50830:          if to_po$ <> " " then L50860
                     to_po$ = from_po$
                     return
L50860:     if to_po$ = "?" then to_po$ = " "
            pdescr$ = hex(06) & "Select Ending Purchase Order Number"
            call "PLOWCODE" (#1, to_po$, pdescr$, 0%, 0.0, f1%(1%))
                if f1%(1%) = 1% then L50910
                   if to_po$ <> " " then L50910
                     to_po$ = from_po$
L50910:     if from_po$ <= to_po$ then L50940
                errormsg$ ="Starting PO Number CANNOT Be Greater Than E"&~
                           "nding PO Number"
L50940:         return

L50960: REM Test for Purchase Order Date Range
            errormsg$ = " "
            if from_podate$ <> "ALL" then L51010
                to_podate$ = " "
                return
L51010:     if from_podate$ <> " " and from_podate$ <> blankdate$ then L51040
                errormsg$ = "Starting Purchase Order Date CANNOT Be Blank"
                return
L51040:     call "DATEOKC" (from_podate$, r%, errormsg$)
                if errormsg$ <> " " then return
            if to_podate$ <> " " and to_podate$ <> blankdate$ then L51090
                to_podate$ = from_podate$
                return
L51090:     call "DATEOKC" (to_podate$, r%, errormsg$)
                if errormsg$ <> " " then return
            call "DATUFMTC" (from_podate$)
            call "DATUFMTC" (to_podate$)
            if from_podate$ <= to_podate$ then L51170
                errormsg$ = "Starting Date Can Not Be Greater Than Endi"&~
                            "ng Date"
L51170:     call "DATFMTC" (from_podate$)
            call "DATFMTC" (to_podate$)
            return

L51500: REM Test For Do You Want to Fax?             FAX$
              if fax$ = "N" or fax$ = "Y" then return
              errormsg$ = "Do you want to fax must be 'Y' or 'N'"
            return

        REM *************************************************************~
            *    IMAGE STATEMENTS FOR ALL P.O. PRINT LINES              *~
            *************************************************************
L60030: %                                                              ##~
        ~##############   ##

L60060: %                                                                ~
        ~  ########      ###

L60090: %          ###############################            ###########~
        ~####################

L60120: %    #########      ##############################    ###########~
        ~###################

L60150: %  ####################     ###      ###       ###     ##########~
        ~#############

L60180: % ### ## #########################  ####  ######## ########## ###~
        ~####### ############
                                                    /* (EWD) - Mod */
L60185: %        ######################### 

L60210: %        ################################

L60222: %        Contract: ################  ####  ######################~
        ~########

L60230: %                                                                ~
        ~        ############

L60260: %  ORDER IN ################################     PRINTED ITEMS TO~
        ~TAL.... ############

L60290: %                                                PRINTED ITEMS TO~
        ~TAL.... ############

L60320: %  ORDER IN ################################

L60340: %!---------------------------------------------------------------~
        ~--------------!

L60350: %!########## @ ########   #######################################~
        ~#    Page: ###!

L60360: %!--------------------!--------------!-------!-------------------~
        ~--------------!

L60370: %!  ################  !  #########   ! ##### !  #################~
        ~##############!

L60380: %!   Purchase Order   ! Vendor Code  ! Buyer !  Fax Number       ~
        ~              !        
        
L64500: REM *************************************************************~
            *     P O   P R I N T   F I L E  E M P T Y   ! ! ! !        *~
            *************************************************************
            head$ = " NO PURCHASE ORDERS! "
            hi$ = "There are NO Purchase Orders on file to be printed"
            mid$ = " "
            lo$ = "Please press RETURN to exit this program."
            pfkey1% = 0%
            call "ASKUSER" (pfkey1%, head$, hi$, mid$, lo$)
            goto L65150
		
        REM *************************************************************~
            *     O P E N  &  S E T U P  F A X  F I L E                 *~
            *************************************************************
/* (EWD0001)  */

        update_faxed_log
           read #19,key = ponumber$, eod goto L62845
        return
L62845:    put #19, using L62875, ponumber$, vencode$, buyer$, pur_no$
           write #19, eod goto L62880
L62875:      FMT CH(16), CH(09), CH(03), CH(11)
L62880:return

        create_report
            title$ = " EWD Faxed Purchase Orders Summary "
            pageno% = 0%
            lcnt%   = 99%
            date$ = date
            init(" ") xtime$
            call "TIME" (xtime$)
            call "DATFMTC" (date$)
            call "SETPRNT" ("VBK002", " ", 0%, 0%)
            select printer(134)
            ponumber$ = " "
        create_rpt_nxt
           read #19,key > ponumber$, using L62875, ponumber$, vencode$, ~
                                    buyer$, pur_no$, eod goto create_rpt_done
           gosub prt_detail
           goto create_rpt_nxt
        create_rpt_done
           print using L60340
           call "SETPRNT" ("VBK002", " ", 0%, 1%)
        return

        prt_detail
            if lcnt% > 57% then gosub prt_header
               print using L60360
               print using L60370, ponumber$, vencode$, buyer$, pur_no$
            lcnt% = lcnt% + 2%
        return

        prt_header
            if lcnt% <> 99% then print using L60340
            pageno% = pageno% + 1%
            print page
            print using L60340
            print using L60350, date$, xtime$, title$, pageno%
            print using L60340
            print using L60380
            lcnt% = 4%
        return
        
        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#19,mode$, 500%, f2%)
            if f2% <> 0% then goto L63010

        return
L63010:     call "SHOSTAT" ("ERROR - CANNOT OPEN (VBKPOPWK)") : stop
        return
        delete_work
            call "FILEBGON" (#19)
        return
        
	open_file
	    open nodisplay #16, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%

            fax% = 1%
        return

        set_file_name
            init(" ") pur_key$, desc$, file$, script$
            str(pur_key$,1%,9%)   = "PLAN FAXP"
            str(pur_key$,10%,15%) = "FAXPUR"

        check_next_file
            read #17,hold,key > pur_key$, using L10210, pur_key$, desc$,  ~
                eod goto check_next_done
L10210:         FMT CH(24), CH(30)
            if str(pur_key$,1%,15%) <> "PLAN FAXPFAXPUR" then            ~
                    goto check_next_done
            if str(desc$,1%,1%) <> "-"                   then            ~
                    goto check_next_file
            put #17, using L10280, "*"
            rewrite #17
L10280:         FMT POS(25), CH(1)

            file$       = str(pur_key$,10%,8%)
            script$     = str(desc$,3%,8%)
        return

        check_next_done
            counter%    = counter% + 1%
            if counter% < 4% then goto set_file_name
            error%      = 1%                       /* EXIT TRY LATER */
        return		
        setup_fax
                                        	    /* EWD RIGHTFAX */
          a$ = all(hex(20))
REM          a$ = "<TONAME Christe Gregory --- TESTING>"        
          a$ = "<TONAME: " & prtvndr$ & ponumber$ & " >" 
          gosub print_line
 
          a$ = all(hex(20))
*          a$ = "<TOFAXNUM: 764-1501>"    /*  EWD Fax No. for Testing */
          call "SPCSMASH" (pur_no$)                   
          if len(pur_no$) = 10 then pur_no$ = "1" & pur_no$
          a$ = "<TOFAXNUM: " & pur_no$ & ">"
          gosub print_line                               
	
          a$ = all(hex(20))
          a$ = "<NOCOVER>"
          gosub print_line

          a$ = all(hex(20))
* (AWD003)

          if schema% = 1% then a$ = "<FORMTYPE: PO>"
          if schema% = 3% then a$ = "<FORMTYPE: PO>"
          if schema% = 2% then a$ = "<FORMTYPE: NEPO>"
          gosub print_line

        return

        fax_blank
         a$ = all(hex(20))
         gosub print_line
        return

        print_line
            if pur_flag$ = "N" then a$ = all(hex(20))
            if pur_flag$ = "N" then return
					
            write #16, using L55030, a$, hex(0D), eod goto L61550
L55030:    FMT CH(79), CH(01)
            a$ = all(hex(20))
        return
L61550:     error% = 5%
            a$ = all(hex(20))
        return clear all			

        complete_fax
            if fax% <> 1% then return       /* So will try to fax if file not */
                                            /* open, should not happen        */
            gosub setup_fax                 /*  (EWD002)                      */
            counter%       = 0% : fx1% = 0% : fx2% = 0%
            if error%      = 1% then L65500
            close #16
                if error% <> 0% then delete_fax

            call "LINK" addr(script$, fx1%, fx2%)
                if fx1%    > 0% then error% = 4%

           call "SHOSTAT" ("  F A X I N G  " )
           call "PAUSE" addr(300%)
        delete_fax
            call "FILEBGON" (#16)          /* Scratch Fax File        */
            fax% = 0%
L65140:     read    #17,hold,key = pur_key$, eod goto L65190
            put     #17, using L10280, "-"
            rewrite #17

            gosub update_faxed_log
            
        if error% <> 0% then goto L65500
        return

L65190:     counter%       = counter% + 1%
            if counter%    < 4% then L65140
            error%         = 6%

L65500: convert error% to temp$, pic(##0)
        call "SHOSTAT" ("Error Code = " & temp$)
        stop			
            call "SHOSTAT" ("One Moment Please")

/* (EWD0001) End  */            
L65000: REM *************************************************************~
            *                          E X I T                          *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING BACK TO   *~
            * THE NEXT PROGRAM.                                         *~
            *************************************************************

            if fieldnr% = 1% or prt% = 1% then L65150
                head$ = "  No Purchase Orders Printed! "
                hi$ = "No Purchase Orders meet criteria entered,"
                mid$ = " "
                lo$ = "Please press RETURN to exit."
                pfkey1% = 0%
                call "ASKUSER" (pfkey1%, head$, hi$, mid$, lo$)

L65150:     call "SETPRNT" ("VBK001", " ", 0%, 1%)
            gosub create_report
            gosub delete_work
            call "SHOSTAT" ("One Moment Please")

            end
