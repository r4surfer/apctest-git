        REM *************************************************************~
            *                                                           *~
            *   AAA   PPPP    CCC   IIIII  N   N  V   V  BBBB    OOO    *~
            *  A   A  P   P  C   C    I    NN  N  V   V  B   B  O   O   *~
            *  AAAAA  PPPP   C        I    N N N  V   V  BBBB   O   O   *~
            *  A   A  P      C   C    I    N  NN   V V   B   B  O   O   *~
            *  A   A  P       CCC   IIIII  N   N    V    BBBB    OOO    *~
            *                                                           *~
            * --------------------------------------------------------- *~
            * SHPACTIN - "Pre-Invoicing".  Allows recording of actual   *~
            *            quantities shipped prior to invoicing.         *~
            *            (Single Sales Order at a time)                 *~
            *                                                           *~
            * APCINVBO - "Pre-Invoicing".  Create a BOL with the Actual *~
            *            quantities shipped and an Innvoice in the      *~
            *            applicable Session (EWD00?), for all S.O. that *~
            *            are in a specified Load. (That do not have a   *~
            *            BOL already Cut.)                              *~  
            *                                                           *~  
            *     NOTE - (SHPACTIN) is for a ingle (1) Sales Order      *~
            *            (APCINVBO) is all Sales Orders on a Load       *~
            *                                                           *~
            *            (APCPLA44) Is Used to Remove Items from a load *~
            *                       in which case no BOL will be Cut if *~
            *                       the Entire S.O. is removed.         *~
            *                                                           *~
            *                       It is also used to Remove and Flag  *~
            *                       Line/Items and Quantities as        *~
            *                       Backorders.                         *~
            *                                                           *~
            *            (EWDPLN52) Is the Backorder/Remove from Load   *~
            *                       Primary Report. Showing all Line    *~
            *                       Items Removed or Backordered.       *~
            *                                                           *~           
            * --------------------------------------------------------- *~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/02/86 ! Original                                 ! ERN *~
            * 02/04/87 ! Enhanced Lot Tracking; Serial Numbers.   ! ERN *~
            * 02/13/87 ! 'HNYHOLD' logic added.                   ! KAB *~
            * 05/13/87 ! Standard Cost Enhancements.              ! ERN *~
            * 11/03/87 ! Mod to ignore Export Orders              ! MJB *~
            * 12/21/87 ! Multi-currency mods.                     ! JIM *~
            * 02/03/88 ! Added check for 'C'ancelled orders       ! JDH *~
            * 08/18/89 ! Added Package Units, See SO & See BOL.   ! JDH *~
            * 09/25/89 ! Over/Under Shipment project              ! LAB *~
            * 11/01/89 ! Over/Under Shipment project              ! JDH *~
            * 11/08/89 ! OVER/UNDER SHIPMENT PROJECT              ! LAB *~
            * 05/11/90 ! Changed literal 'Available' to 'Uncommit-! JDH *~
            *          !  ted' & now is the qty that has not been !     *~
            *          !  schdld or shipped. Chngd qtys use by    !     *~
            *          !  SHPOVRSB. Chngd update of scheduled amt.!     *~
            *          !  Now honors no neg. lot flag.            !     *~
            * 05/17/90 ! Part descr now from BCKLINES not HNYMASTR! JDH *~
            * 09/27/90 ! Correct acceptable ship qtys from cutover! JDH *~
            *          !  scheduled BOL.                          !     *~
            * 01/28/91 ! Make Full Screen Entry/Edit on 1st screen! JDH *~
            * 05/20/91 ! Added location mngmnt from pending R6.01.! JDH *~
            * 06/07/91 ! PRR 11586.  On delete properly reset     ! JDH *~
            *          !   SERMASTR.  PS. Happy birthday to me.   !     *~
            * 06/26/91 ! Added call to ALLFREE.                   ! JDH *~
            * 11/25/91 ! PRR 12136.  Test date so existing BOL to ! JDH *~
            *          !   mod or delete is within posting window.!     *~
            *          ! PRR 12130.  Added call to HNYQDISP.      !     *~
            * 12/26/91 ! Fixed so that if BOL shipped date is     ! SID *~
            *          !   blank, ASKUSER for shipped date outside!     *~
            *          !   of posting window will still appear.   !     *~
            *          ! Added 0E to PFKEY$ to activate           !     *~
            *          !   PF)14 StrLot Qty.                      !     *~
            * 04/06/92 ! PRR 12353. Can't flag complete line zero.! JDH *~
            * 10/23/92 ! Now have visability & access to seq #100.! MLJ *~
            * 02/26/93 ! PRR 12783. Existing BOL lines never skipd! JDH *~
            * 03/01/93 ! PRR 11699,12133.  Ship Complete Option.  ! JDH *~
            * 04/15/93 ! PRR 12859.  Fixed Lot enforcement.       ! JDH *~
            *          ! Added psuedo soft-enable for Package flds!     *~
            * 01/13/94 ! Added SO/line to filler of SHPLINES      ! WPH *~
            * 02/21/94 ! PRR 12522,13092. Stopped leftover 't'    ! JDH *~
            *          !   in-process serial number records.      !     *~
            * 03/08/94 ! Changed record length for BOMSPEC        ! WPH *~
            * 08/23/96 ! Century date conversion                  ! DER *~
            * 01/28/93 ! Original - Create BOL and Invoice(EWD)   ! RHH *~
            * 05/04/94 ! Mod - Store S.O. Load Number in the Print! RHH *~
            *          !   BOL File. (BCKPRIDX)                   !     *~
            * 08/25/97 ! Mod - Set-Up session's for Seven (7) days! RHH *~
            *          !     - Also create BOL's for an entire    !     *~
            *          !     - load when specified.               !     *~
            * 12/18/97 ! Mod for Upgrade to R6.04.03              ! RHH *~
            * 04/01/98 ! (Y2K) (EWD) Upgrade to Millie Version    ! RHH *~
            * 04/06/98 ! Special Mods for 2nd BOL being Cut (EWD) ! RHH *~
            * 04/24/98 ! Special Mod to Set Status in (APCPLNOR)  ! RHH *~
            * 05/25/98 ! (EWD001) - Mod to add userid 'GDC' to    ! RHH *~
            *          !   EDIT_SESSION test.                     !     *~
            * 06/15/98 ! (EWD002) - Mod for Automatic BOL Generate! RHH *~
            *          !  If BOL Exists then S.O. is Skipped.     !     *~
            *          !  If the Entire S.O. is removed by        !     *~
            *          !  (APCPLA44) then no BOL is Cut at all    !     *~
            *          !  nor is an Invoice Created.              !     *~
            * 06/15/98 ! (Note) - Records are not Deleted from    !     *~
            *          !        (EWDBOLRM) file used by EWDPLN52  !     *~
            * 08/28/98 ! (EWD003) Mod to correct the Ship Date    ! RHH *~
            * 08/06/99 ! (EWD004) Insure that no dups are created !     *~
            * 11/19/99 ! (EWD005) Mod to correct bug with         ! RHH *~
            *          !     'skip_message' also made change to   !     *~
            *          !     correct no BOL problem. Found that   !     *~
            *          !     S.O. was being locked in 'BCKBUFFR'  !     *~  
            * 12/15/99 ! (EWD007) Mod to correct bug with         ! CMG *~
            *          !     EWOBOLRM file to read on SO instead  !     *~
            *          !     of Load and SO because of Loads being!     *~
            *          !     moved.                               !     *~                
            * 12/16/99 ! (EWD008) Mod to replace openchecks with  ! CMG *~
            *          !          "EWDOPENS".  Also add open_error!     *~  
            *          !          routine.                        !     *~
            * 04/01/05 ! (AWD009) Mod to use arrival date instead ! CMG *~
            *          !  of todaysdate for posting date & session!     *~
            * 05/04/05 ! (AWD010) Mod to close a trailer when the ! CMG *~
            *          !         load is closed                   !     *~
            * 05/26/05 ! (AWD011) Mod to prevent BOL from getting ! CMG *~
            *          !      generated if inventory has not been !     *~
            *          !      updated.                            !     *~
            * 02/09/06 ! (PAR000) mod for subpart                 ! CMG *~
            * 08/26/08 ! (AWD012) mods for daily warehouse data   ! CMG *~
            *06/29/2012! (AWD013) do not invoice CV0999 painted   ! CMG *~
            *11/15/2012! (AWD014) mods for ship audit             ! CMG *~
            *02/27/2015! (AWD015) mods for a log file             ! CMG *~
            *05/05/2017! (CR0534) Session ID format change        ! RDB *~
            *08/28/2017! (CR1098) Harvey Invoices                 ! CMN *~
            *08/28/2017! (CR1098) Harvey Invoices                 ! CMN *~
            *08/28/2017! (CR1098) Harvey Invoices                 ! CMN *~
            *09/22/2017! CR890 Add truck load finish date & time  ! RDB *~
            *          !    to AWDAPPLD.                          ! RDB *~
            *02/06/2018! CR9999 Add AD0999 customer to skip       ! RDB *~
            *05/16/2018! CR1490 Add Paint SO# to customer to skip ! RDB *~
            *09/16/2019! (CR2239) Add ST0001 to skip invoice      ! CMN *~
            *05/10/2021! CR2829 Add Bcklin2 file                  ! RDB *~
            *07/29/2021! CR2868 Add new paint customer to skip    ! RDB *~
            *************************************************************

        dim                                                              ~
            askmsg$(3%)78,               /* ASKUSER messages           */~
            avail(100%), avail$(100%)10, /* Qty Avail for Scheduling   */~
            blankdate$8,                 /* blank unfmt date           */~
            bol$3,                       /* Bill of Lading Number      */~
            carrier$6,                   /* Carrier                    */~
            carriername$30,              /* Carrier Name               */~
            cartons$10,                  /* Cartons Shipped            */~
            committed(100%),             /* Shipped & scheduled        */~
            comp$(100%)1,                /* Completion flag            */~
            crflag$1,                    /* Order Credit Status Flag   */~
            curr$1,                      /* Currency flag              */~
            currency$4, currdesc$32,     /* Currency code & description*/~
            currline$79,                 /* Freight charge prompt      */~
            cursor%(2%),                 /* Cursor location for edit   */~
            cuscode$9,                   /* Ship-to Cutomer Code       */~
            cusname$30,                  /* Ship-to Customer Name      */~
            date$8, fdate$10,            /* Date for screen display    */~
            def_percent(100%),           /* Default % overship allowed */~
            def_unit(100%),              /* Def. units overship allowed*/~
            disp$(100%)3,                /* Field to display 3 flags   */~
            dfltlot$(100%)6,             /* Default Lot Number Array   */~
            dfltlot$6,                   /* Default Lot Number         */~
            errhdr$16,                   /* Error message header       */~
            errormsg$79,                 /* Error message              */~
            errormsg_temp$79,            /* Error message              */~
            export$1,                    /* Export Order Flag          */~
            fob$20,                      /* FOB                        */~
            frtamt$10,                   /* Freight Charges Amount     */~
            frtbill$20,                  /* Frt/Air Bill Number        */~
            hdr1$03, hdr2$28, hdr3$03,   /* Column Headings            */~
            hdr4$01, hdr5$10, hdr6$10,   /*                            */~
            hdr7$17, hdr8$04, hdr9$08,   /*                            */~
            hdr10$21,                    /*                            */~
            hnyactve$1,                  /* Inventory Active?          */~
            holdlot$(100%,30%)6,         /* Just Remembering What Was  */~
            holdqty(100%,30%),           /*                            */~
            holdmark$(100%,30%)1,        /*                            */~
            howship$20,                  /* How Ship                   */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            instr$(2%)50,                /* Shipping Instructions      */~
            invnr$8,                     /* Invoice Number             */~
            ivtext$(113%,1%)70,          /* Invoice Text Array (Inv)   */~
            lfac$(20%)1, lfax$(20%)1,    /* Field Attribute Characters */~
            line2$79,                    /* Second Line of Screen Headr*/~
            lot$(100%,30%)6,             /* Lot Distr- Lot Numbers     */~
            lotqtys(100%,30%),           /* Lot Distr- Quantities      */~
            lotqty1$(100%)10,            /* Lot Distr- 1st Qty Slot    */~
            lot1was$(100%)6,             /* What is was before change  */~
            mark$(100%,30%)1,            /* Marks processed (HNYHOLD)  */~
            msg$79,                      /* Message to TXTDSPLY routine*/~
            newlot$(30%)6,               /* Lot Distr- New List of Lots*/~
            newlotqty(30%),              /* Lot Distr- New Quantities  */~
            newlotqty$(30%)10,           /*                            */~
            nfac$(20%)1,                 /* Field Attribute Characters */~
            oldlot$(30%)6,               /*                for lots    */~
            oldlotqty(30%),              /*                for qtys    */~
            order(100%),                 /* Current order qty          */~
            overship$1,                  /* Overshipment allowed flag  */~
            pfac$(20%)1,                 /* Field Attribute Characters */~
            pack_qty$(100%)4,            /* Number of packages sent    */~
            pack_type$(100%)8,           /* Package types sent         */~
            paktypname$30,               /* Package types name dummy.  */~
/*PAR000*/  epart$(100%)45,              /* Entire Part                */~
            part$(100%)25,               /* Part Numbers               */~
/*PAR000*/  subp$(100%)25,               /* sub part numbers           */~
            partdesc$(100%)32,           /* Part Descriptions          */~
            pf$(3%)79, pfkey$22,         /* PF Keys                    */~
            plowkey$80,                  /* Misc. Plow Key             */~
            pn_or_desc$(100%)28,         /* for PN or Description      */~
            price(100%),                 /* Unit Price (Stking UOM)    */~
            printdate$8,                 /* Document Print Date        */~
            printbol$1,                  /* BOL Doc Print Flag         */~
            readkey$50, readkey1$50,     /* Multi-Use Read Keys        */~
            schflag$(100%)1,             /* Already Scheduled Flag     */~
            schld(100%), schld$(100%)10, /* Qty Scheduled (Shipped)    */~
            schlddate$8,                 /* Scheduled Ship Date        */~
            seqnr$(100%)3,               /* Line Item Sequence Numbers */~
            shipdate$8,                  /* Scheduled Ship Date        */~
            sn_index1%(30%),             /* Serial No. Index- Temporary*/~
            sn_index2%(100%,30%),        /*            Index- Line/Lot */~
            sn_loc$30,                   /*            Location        */~
            sn_no$20,                    /* Serial # to updte on delete*/~
            sn_part$25,                  /* Part   # to updte on delete*/~
            sn_trankey$40,               /*            Transaction Key */~
            so$16,                       /* Sales Order - BOL          */~
            sotext$(113%,1%)70,          /* Text Array (SO)            */~
            statutory$4,                 /* Statutory currency code    */~
            stocked%(100%),              /* Stocked = 1 else 0         */~
            store$3, strs$(30%)3,        /* Shipping Store             */~
            textso$(100%)4, textiv$(100%)4,/* Line Text IDs (SO, Inv)  */~
            textso$4, textiv$4,          /* Header Text ID (SO, Inv)   */~
            txt$(100%)1,                 /* Line Item Text Avail Flag  */~
            ufac$(20%)1,                 /* Field Attribute Characters */~
            userid$3,                    /* Current User Id            */~
                                         /* (EWD) - Begin              */~  
            or_rec$170,                  /* (APCPLNOR) - Record        */~  
            load$5, or_key$8, ld_key$5,  /* APC Load Key               */~
            ld_desc$30, ewd_msg$79,      /* (EWD002) for BOL'S         */~
            sc_key$27, sav_so$8,         /* (EWD002) for BOL'S         */~
            rm_key$16, sav_load$5,       /* (EWD002) for BOL'S         */~
            or_status$2, so_msg$48,      /* (EWD002) for BOL'S         */~
            sc_st$2,                     /* (EWD002) for BOL'S         */~  
            post_dte$6,                  /* TODAY'S DATE FOR POSTING   */~
            session$6, sav_session$6,    /* Save Session's Name        */~
            first_session$6,             /* First Session (AWD009)     */~
            sessions$(10%)6,             /* Seven day of wk (AWD009)   */~ 
            postdate$(10%)6,             /* Post Dates      (AWD009)   */~
            testdate$6,                  /* Test date       (AWD009)   */~
            app_key$8,                   /* AWDAPPOR Key    (AWD009)   */~
            app_dte$6,                   /* Arrival Date    (AWD009)   */~
            app_drop$2,                  /* Appian Drop     (AWD009)   */~
            sav_postdate$6,              /* Save            (AWD009)   */~
            postdate$6,                  /*                            */~
            days$(7%)9,                  /* Days of the Week           */~
            days$9,                      /* Day of the Week            */~
            dte$6,                       /* Today's Date               */~
            inv_time$8,                  /* Today's Time               */~
            status$1,                    /* Session - Status Code      */~
            s_desc$70,                   /* Status Code Description    */~
            hdr$40,                      /* ASKUSR - Header Message    */~
            msg$(3%)79,                  /* ASKUSR - Message (EWD) End */~
            weight$10,                   /* Shipment Weight            */~
            currdate$8,                  /* Current Date               */~
            calc_time$8,                 /* Current time CR890         */~
            ld_app_rec$128,              /* (awdappld) New Load File CR890 */~
            ld_app_key0$5                /* Load No. Primary key   CR890   */

        dim trailer_key$20,              /* Trailer Key     (AWD010)   */~
            sav_trailer$20,              /* Save Key        (AWD010)   */~
            trailer_rec$128              /* Trailer Record  (AWD010)   */

        dim sc_inv$1                     /* APCPLNSC Inv Flag  (AWD011)*/

        dim f2%(64%),                    /* = 0 if the file is open    */~
            f1%(64%),                    /* = 1 if READ was successful */~
            fs%(64%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64%)20                 /* Text from file opening     */


        dim                              /* PAR000                     */~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4             /* Additional Info Fields     */
            
/* (AWD015) */
        dim                                                              ~
            adt_cuscode$9,               /* Audit Cuscode              */~
            adt_so$8,                    /* Audit SO                   */~
            adt_seq$3,                   /* Line Sequence Number       */~
            adt_po$16,                   /* Audit PO Number            */~
            program$9,                   /* Program Name               */~
            function$20,                 /* Funcation Name             */~
            notes$128                    /* Notes                      */ 
            
/*CR1098*/
         dim table$9,                    /* Table To Read              */~
             genkey$15,                  /* GENCODES Key to Read       */~
             descr1$30,                  /* Description                */~
             codeLen$2,                  /* Code Length                */~
             descr2$30,                  /* Description                */~
             descr3$30                   /* Description                */ 

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21                      /* (EWD) - Begin */
            apc$   = "(EWD) Pre-Invoicing (Y2K) Complient               "
            pname$ = "APCINVBO - Rev: R7.00"
                                                      /* (EWD) - End   */
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
            * #1  ! SHPHDRS  ! Shipment Scheduling / Pre-Invoicing- Hea *~
            * #2  ! SHPLINES ! Shipment Scheduling / Pre-Invoicing- Lin *~
            * #3  ! GENCODES ! General Codes File                       *~
            * #4  ! BCKPRIDX ! SO Document Print Index File             *~
            * #5  ! BCKMASTR ! Backlog master file                      *~
            * #6  ! BCKLINES ! Back Log Line Item File                  *~
            * #7  ! GLMAIN   ! General Ledger Chart of Accounts         *~
            * #8  ! GLDETAIL ! General Ledger Detail Trial Balance      *~
            * #9  ! BCKBUFFR ! Backlog buffer for SO headers            *~
            * #10 ! APCPLNOR ! Planning S.O. Header History       (EWD) *~ 
            * #11 ! PIPMASTR ! Planned Inventory Position Master File   *~
/*PAR000*/  * #12 ! INVMASTR ! Inventory Master File                    *~
/*PAR000*/  * #13 ! INVDETAL ! Inventory detail file                    *~
/*PAR000*/  * #14 ! INVPOOL  ! LIFO/FIFO Pools                          *~
            * #15 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #16 ! TXTFILE  ! System Text File                         *~
            * #17 ! SYSFILE2 ! System Misc File                         *~
/*PAR000*/  * #18 ! INVQUAN  ! Inventory Quantities File                *~
            * #19 ! SHPHNYTF ! Shipments/Inventory Transactions File    *~
            * #20 ! HNYPSTGL ! HNYPOST-G/L Transactions Work File       *~
            * #21 ! DEMMASTR ! Demand Master File                       *~
            * #22 ! PIPIN    ! PIP In File                              *~
            * #23 ! PIPOUT   ! PIP Out File                             *~
            * #24 ! WCMASTR  ! Work Center Master                       *~
            * #25 ! WCOUT    ! Planned Work Center Use Detail Record    *~
            * #26 ! SFMASTR2 ! Sales Forecast Master File               *~
            * #27 ! JBCROSS2 ! Cross Reference of RTE & BOM planned for *~
            * #28 ! PIPCROSS ! Hard Peg Cross Reference                 *~
            * #29 ! JBPIPXRF ! Option Part Harder Peg                   *~
            * #30 ! BOMSPEC  ! Options Selected File                    *~
            * #31 ! STORNAME ! Store Master File                        *~
            * #32 ! SERMASTR ! Serial Number Tracking Master File       *~
            * #33 ! SERWORK  ! Temporary Serial #'s Work File           *~
            * #34 ! SERTIF   ! Serial #'s Trans Image File (Buffer)     *~
            * #35 ! USERINFO ! User's Posting Dates                     *~
            * #36 ! HNYLOCNS ! Location Quantity Detail File            *~
            * #37 ! LOCATION ! Location Master File                     *~
            * #38 ! CATEGORY ! Category file                 (EWD) Begin*~
            * #39 ! STXCODES !                                          *~
            * #40 ! CURMASTR ! Multi-Currency Master file               *~
            * #41 ! CUSTOMER ! Customer Master File                     *~
            * #42 ! ARMTERMS ! Caelus Terms Master File.                *~
            * #43 ! UDPSESSN ! Caelus Master Session Control File       *~
            * #44 ! ARMTRIAL ! A/R Trial Balance File                   *~
            * #45 ! APCPLNSC ! Planning Master Scheduling File          *~
            * #46 ! ARIBUFFR ! A/R Buffer File Header                   *~
            * #47 ! ARIBUF2  ! A/R Buffer File Lines                    *~
            * #48 ! ARINUMBR !                                          *~
            * #59 ! EWDBOLRM ! Remove From Load Line Items (EWD002)     *~
            * #50 ! APCPLNLD ! Planning/Scheduling Load Maste(EWD) End  *~
            * #51 ! AWDAPPOR ! Appian New Order Header File             *~
            * #52 ! AWDTRAIL ! Track Trailer and Load Assigned  (AWD010)*~ 
/*PAR000*/  * #53 ! INVMASTR ! Inventory Master File Finished Goods     *~
/*PAR000*/  * #54 ! INVQUAN  ! Inventory Quantities File  Finished Goods*~
/*PAR000*/  * #55 ! INVDETAL ! Inventory detail file  finsished goods   *~
            * #57 ! AWDAPPLD ! Appian Load File                 (CR890) *~
            * #60 ! BCKLIN2  ! Back lines 2 file with GS128             *~
/*PAR000*/  * #63 ! BCKSUBPT ! Sub Part File                            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "SHPHDRS",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =  1,   keylen = 28

            select #2,  "SHPLINES",                                      ~
                        varc,     indexed,  recsize = 600,               ~
                        keypos =  10,  keylen = 22

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #4,  "BCKPRIDX",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =   11, keylen =  29,                     ~
                        alt key  1, keypos =    1, keylen =  39

            select #5,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #6,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #7,  "GLMAIN",                                        ~
                        varc,     indexed,  recsize =   300,             ~
                        keypos =   1, keylen =  9

            select #8,  "GLDETAIL",                                      ~
                        varc,     indexed,  recsize =  160,              ~
                        keypos =    1, keylen =  26

            select #9,  "BCKBUFFR",                                      ~
                        varc,     indexed,  recsize = 1020,              ~
                        keypos =    1, keylen =  10,                     ~
                        alt key  1, keypos =    4, keylen =   7, dup,    ~
                            key  2, keypos =   30, keylen =  16          ~

                                                    /* (EWD) - Begin  */
            select #10, "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos =    1, keylen =  51,                     ~
                        alt key  1, keypos =   27, keylen = 25,          ~
                            key  2, keypos =   70, keylen =  8, dup,     ~
                            key  3, keypos =   78, keylen =  8, dup,     ~
                            key  4, keypos =   52, keylen =  8,          ~
                            key  5, keypos =   36, keylen = 16, dup
                                                     /* (EWD) - End    */  

             select #11, "PIPMASTR",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26

            select #12, "INVMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 1024,                                 ~
                         keypos = 1, keylen = 45,                        ~
                         alternate key 1, keypos = 122, keylen = 9, dup, ~
                                   key 2, keypos = 110, keylen = 4, dup, ~
                                   key 3, keypos = 46, keylen = 32, dup

            select #13, "INVDETAL",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  62,                     ~
                        alt key  1, keypos =   63, keylen =   6, dup,    ~
                            key  2, keypos =   69, keylen =   2, dup

            select #14, "INVPOOL",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  58

            select #15, "SFCUM2",                                        ~
                        varc,     indexed,  recsize = 1985,              ~
                        keypos =    1, keylen =  25

            select #16, "TXTFILE",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  11

            select #17, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize = 500,               ~
                        keypos =    1, keylen =  20

            select #18,  "INVQUAN",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   768,                                 ~
                        keypos =   17, keylen =  64,                     ~
                        alt key  1, keypos =    1, keylen =  64

            select #19, "SHPHNYTF",                                      ~
                        varc,     indexed,  recsize = 572,               ~
                        keypos = 1, keylen = 46,                         ~
                        alternate key 1, keypos = 47, keylen = 80

            select #20, "HNYPSTGL",                                      ~
                        varc,     indexed,  recsize = 160,               ~
                        keypos = 10, keylen = 10

            select #21, "DEMMASTR",                                      ~
                        varc,     indexed,  recsize =  123,              ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28

            select #22, "PIPIN",                                         ~
                        varc,     indexed,  recsize =   60,              ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48

            select #23, "PIPOUT",                                        ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37

            select #24, "WCMASTR",                                       ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    2, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =   6

            select #25, "WCOUT",                                         ~
                        varc,     indexed,  recsize =   68,              ~
                        keypos =    9, keylen =  23,                     ~
                        alt key  1, keypos =    1, keylen =  27

            select #26, "SFMASTR2",                                      ~
                        varc,     indexed,  recsize = 2024,              ~
                        keypos =    1, keylen =  25

            select #27, "JBCROSS2",                                      ~
                        varc,     indexed,  recsize =  94,               ~
                        keypos =  29,  keylen = 19,                      ~
                        alt key  1, keypos =    1, keylen =  47,         ~
                            key  2, keypos =   48, keylen =  47

            select #28, "PIPCROSS",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =  1,   keylen = 71,                      ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33

            select #29, "JBPIPXRF",                                      ~
                        varc,     indexed,  recsize =  63,               ~
                        keypos = 1,    keylen =  63,                     ~
                        alt key  1, keypos =   45, keylen =  19

            select #30, "BOMSPEC",                                       ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  54,                     ~
                        alt key  1, keypos =   57, keylen =  23

            select #31, "STORNAME"                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =  3

            select #32, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #33, "SERWORK",                                       ~
                        varc,     indexed,  recsize =  48,               ~
                        keypos = 1, keylen = 23

            select #34, "SERTIF",                                        ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos = 1, keylen = 62

            select #35, "USERINFO",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos = 1, keylen =  3

            select #36, "HNYLOCNS",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos = 1, keylen =  42,                        ~
                        alt key 1,  keypos = 443, keylen = 42,           ~
                            key 2,  keypos = 485, keylen = 42,           ~
                            key 3,  keypos = 527, keylen = 42,           ~
                            key 4,  keypos = 590, keylen = 42

            select #37, "LOCATION",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos = 1, keylen =  11,                        ~
                        alt key 1,  keypos =   4, keylen = 11

                                                      /* (EWD) - Begin */
            select #38, "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   4

            select #39, "STXCODES",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  10

            select #40, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4
                                                 
            select #41, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup

            select #42, "ARMTERMS",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  20

            select #43, "UPDSESSN",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =  4,   keylen = 17,                      ~
                        alt key  1, keypos =     1, keylen =  20

            select #44, "ARMTRIAL",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  2

            select #45, "APCPLNSC",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 24,   keylen = 10,                      ~
                        alt key  1, keypos =    7, keylen =  27,         ~
                            key  2, keypos =    1, keylen =  33

        REM - SPECIAL FILES FOR CREATING AN INVOICE

            select #46, "ARIBUFFR",                                      ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos = 1, keylen =   17,                       ~
                        alt key  1, keypos = 2001, keylen =   24

            select #47, "ARIBUF2",                                       ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =   1,  keylen = 20

            select #48, "ARINUMBR",                                      ~
                        varc,     indexed,  recsize =   17,              ~
                        keypos =  1,   keylen = 17,                      ~
                        alt key  1, keypos =    10, keylen =  8, dup
                                                    /* (EWD002) Begin  */
            select #49, "EWDBOLRM",                                      ~
                        varc,     indexed,  recsize =   32,              ~
                        keypos =  1,   keylen = 16,                      ~
                        alt key  1, keypos =     6, keylen = 11
                                                    /* (EWD002) End    */
            select #50, "APCPLNLD",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 11,   keylen =  5,                      ~
                        alt key  1, keypos =    3, keylen =  13,         ~
                            key  2, keypos =    1, keylen =  15
                                                      /* (EWD) - End   */


                                                      /*  (AWD009) - Beg */
            select #51, "AWDAPPOR",                                      ~
                        varc,     indexed,  recsize =  384,              ~
                        keypos =   1, keylen =   8,                      ~
                        alt key  1, keypos =    9, keylen =  40,         ~
                            key  2, keypos =   15, keylen =  34,         ~
                            key  3, keypos =   17, keylen =  32

                                                        /* (AWD009) - End */

                                                          /* (AWD010)   */
            select #52, "AWDTRAIL",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =  20,                     ~
                        alt key  1, keypos =   21, keylen =  20
                                                          /* (AWD010)   */

                                                      /* (EWD008) - Begin */

/* (AWD012) */
            select #53, "ORABOL",                                        ~
                        varc,     indexed,  recsize = 512,               ~
                        keypos =    1, keylen =  32,                     ~
                        alt key  1, keypos =   13, keylen =  20, dup,    ~
                            key  2, keypos =  186, keylen =   8, dup,    ~
                            key  3, keypos =   22, keylen =  11 
                            
/* (AWD014) */                            
            select #56, "SHPAUDIT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =  1,   keylen = 37                            
                            
/* CR890 */                        
           select #57, "AWDAPPLD",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =   12, keylen =    5,                    ~
                        alt key 1,  keypos =  1,   keylen =  16,         ~
                            key 2,  keypos =  2,   keylen =  15,         ~
                            key 3,  keypos = 17,   keylen =  15

/* CR2829 */
            select #60, "BCKLIN2",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   10, keylen =  19
                                 
/* CR347 - part number PAR000 */
            select #63, "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~ 
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup   

 
            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%( 1%), f2%( 1%), 100%, rslt$( 1%))
            call "OPENCHCK" (#2,  fs%( 2%), f2%( 2%), 200%, rslt$( 2%))
            filename$ = "GENCODES" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            call "OPENCHCK" (#4,  fs%( 4%), f2%( 4%), 100%, rslt$( 4%))
            filename$ = "BCKMASTR" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLINES" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GLMAIN  " : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GLDETAIL" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
            call "OPENCHCK" (#9,  fs%( 9%), f2%( 9%), 100%, rslt$( 9%))
                                                     /* (EWD) - Begin */
            filename$ = "APCPLNOR" : call "EWDOPEN" (#10, filename$, err%)
            if err% <> 0% then gosub open_error
                                                    /* (EWD) - End   */
            filename$ = "PIPMASTR" : call "EWDOPEN" (#11, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "INVMASTR" : call "EWDOPEN" (#12, filename$, err%)
            if err% <> 0% then gosub open_error
REM            filename$ = "INVDETAL"  call "EWDOPEN" (#13, filename$, err%)
REM            if err% <> 0% then gosub open_error
            call "OPENCHCK" (#13,  fs%(13%), f2%(13%), 100%, rslt$(13%))

            filename$ = "INVPOOL " : call "EWDOPEN" (#14, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "SFCUM2  " : call "EWDOPEN" (#15, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "TXTFILE " : call "EWDOPEN" (#16, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "SYSFILE2" : call "EWDOPEN" (#17, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "INVQUAN " : call "EWDOPEN" (#18, filename$, err%)
            if err% <> 0% then gosub open_error
            call "OPENCHCK" (#19, fs%(19%), f2%(19%), 100%, rslt$(19%))
            call "WORKOPEN" (#20, "IO   ", 10%, f2%(20%))
            filename$ = "DEMMASTR" : call "EWDOPEN" (#21, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "PIPIN   " : call "EWDOPEN" (#22, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "PIPOUT  " : call "EWDOPEN" (#23, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "WCMASTR " : call "EWDOPEN" (#24, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "WCOUT   " : call "EWDOPEN" (#25, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "SFMASTR2" : call "EWDOPEN" (#26, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "JBCROSS2" : call "EWDOPEN" (#27, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "PIPCROSS" : call "EWDOPEN" (#28, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "JBPIPXRF" : call "EWDOPEN" (#29, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BOMSPEC " : call "EWDOPEN" (#30, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "STORNAME" : call "EWDOPEN" (#31, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "SERMASTR" : call "EWDOPEN" (#32, filename$, err%)
            if err% <> 0% then gosub open_error
REM            filename$ = "SERTIF  "  call "EWDOPEN" (#34, filename$, err%)
REM            if err% <> 0% then gosub open_error
            call "OPENCHCK" (#34,  fs%(34%), f2%(34%), 100%, rslt$(34%))

            filename$ = "USERINFO" : call "EWDOPEN" (#35, filename$, err%)
            if err% <> 0% then gosub open_error
            call "OPENCHCK" (#36, fs%(36%), f2%(36%), 100%, rslt$(36%))
            call "OPENCHCK" (#37, fs%(37%), f2%(37%), 100%, rslt$(37%))
                                                    /* (EWD) - Begin */
            filename$ = "CATEGORY" : call "EWDOPEN" (#38, filename$, err%)
            if err% <> 0% then gosub open_error
REM            filename$ = "STXCODES"  call "EWDOPEN" (#39, filename$, err%)
REM            if err% <> 0% then gosub open_error
            call "OPENCHCK" (#39,  fs%(39%), f2%(39%), 100%, rslt$(39%))


            filename$ = "CUSTOMER" : call "EWDOPEN" (#41, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "ARMTERMS" : call "EWDOPEN" (#42, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "UPDSESSN" : call "EWDOPEN" (#43, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "ARMTRIAL" : call "EWDOPEN" (#44, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSC" : call "EWDOPEN" (#45, filename$, err%)
            if err% <> 0% then gosub open_error
            call "OPENCHCK" (#46, fs%(46%), f2%(46%), 8000%, rslt$(46%))
            call "OPENCHCK" (#47, fs%(47%), f2%(47%), 8000%, rslt$(47%))
            filename$ = "ARINUMBR" : call "EWDOPEN" (#48, filename$, err%)
            if err% <> 0% then gosub open_error
                                                   /* (EWD002) Begin */
            call "OPENCHCK" (#49, fs%(49%), f2%(49%), 100%, rslt$(49%))
                                                   /* (EWD002) End   */
            filename$ = "APCPLNLD" : call "EWDOPEN" (#50, filename$, err%)
            if err% <> 0% then gosub open_error
                                                    /* (EWD) - End   */
                                                    /* (EWD008) - End  */
                                                    /* (AWD009) - Beg  */
REM            filename$ = "AWDAPPOR"  call "EWDOPEN" (#51, filename$, err%)
REM            if err% <> 0% then gosub open_error
            call "OPENCHCK" (#51,  fs%(51%), f2%(51%), 100%, rslt$(51%))
                                                    /* (AWD009) - End  */

                                                    /* (AWD010) - Beg  */
            filename$ = "AWDTRAIL" : call "EWDOPEN" (#52, filename$, err%)
            if err% <> 0% then gosub open_error
                                                    /* (AWD010) - End  */
/* (AWD012) */
            call "OPENCHCK" (#53,  fs%(53%), f2%(53%), 200%, rslt$(53%))
/* (AWD014) */
            call "OPENCHCK" (#56,  fs%(56%), f2%(56%), 200%, rslt$(56%))  
/* CR890 */ filename$ = "AWDAPPLD" : call "EWDOPEN" (#57, filename$, err%)
            if err% <> 0% then gosub open_error            
/* CR2829 */
            call "OPENCHCK" (#60,  fs%(60%), f2%(60%), 200%, rslt$(60%))
            
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#63, filename$, err%)
            if err% <> 0% then gosub open_error


            if f2%(19) = 0% then L04380
                call "OPENCHCK" (#19, fs%(19), f2%(19), 200%, rslt$(19))
                write #19 using L04360, "0", "0", "1", " ", " "
L04360:              FMT CH(46), CH(80), CH(1), CH(200), CH(245)

L04380
*        Check for Multi-Currency
            curr$ = "N" : statutory$ = " "
            call "READ100" (#17, "SWITCHS.CUR", f1%(17))
               if f1%(17) = 0% then L09000
            get #17 using L04430, curr$, statutory$
L04430:         FMT POS(21), CH(1), CH(4)
            if curr$ <> "Y" then goto L09000
                call "OPENCHCK"(#40, fs%(40), f2%(40), f1%(40), rslt$(40))

L09000: REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            * --------------------------------------------------------- *~
            * Initializes information necessary for program.            *~
            *************************************************************

                                                   /* (EWD) - Begin   */
            call "EXTRACT" addr("ID", userid$)
            date$  = date : call "DATEFMT" (date$)
            fdate$ = date : call "DATFMTC" (fdate$)   /* (EWD) - Date  */
            program$ = "APCINVBO"                     /* (AWD015)      */
            shipaudit% = 1%                           /* (AWD015)      */
            
                                               /* (EWD002) - Begin (A) */
        mainline                               /* Set-up Screen to     */ 
            init(" ") errormsg$                /* Process BOL'S by Load*/
            gosub check_session                /* Load/Sesion Input    */
REM            if inv% = 0% then goto L09100      /* No input errors      */
                                                  /*  (AWD009)  */
            if inv% = 0% and other_inv% = 0 then goto L09100 
               if other_inv% <> 0% then goto L09060
               if inv% = 2% then goto L09050
                  gosub prompt_user            /* Invoice Session Error*/
                  goto exit_program
L09060:        gosub prompt_user                 /* (AWD009)  */
               goto exit_program                 /* (AWD009)  */   
L09050:     gosub error_prompt                 /* Prod. Load Error     */
            goto mainline                      /* In case of <Return>  */     
                                               /* PF(16) Exit Program  */
L09100:     gosub prompt_begin
            if comp% = 14% then goto L09110  /* Process BOL'S for Load */
               goto exit_program

L09110:     call "BCKSWTCH" ("AR ", "HNYACTVE", hnyactve$, temp, return%)
                                               /* (EWD) - End      (A) */
                                               /* Initialize Screen    */
            blankdate$ = " "
            call "DATUNFMT" (blankdate$)
            call "EXTRACT" addr("ID", userid$)
            hdr1$ = "Seq"
            hdr2$ = "Part Number/Description"
            hdr3$ = "TCS"
            hdr4$ = " "
            hdr5$ = "Uncommittd"
            hdr6$ = "   Shipped"
            hdr7$ = "Lot      Quantity"    :   lot% = 6%
            hdr8$ = "PQty"
            hdr9$ = "PackType"
            hdr10$ = "Part Number/Descr.   "

            call "BCKSWTCH" ("BCK", "OVRSHIP ", overship$, temp, u3%)
                temp  = 0    /* Just so I don't get a compile error */
            call "BCKSWTCH" ("BCK", "OVRSHIP%", temp$, sys_def_percent,  ~
                              u3%)
                temp$ = " "  /* Just so I don't get a compile error */

*        See if operator is an administrator or not
            call "CMSMACHK" ("BCK", lfac$(1%), lfac$(2%))
            if lfac$(1%) = "Y" or lfac$(2%) = "Y" then admin% = 1%

*        See if Package Fields are enabled or not
            package_enabled% = 2%   /* Start as enabled */
            call "READ100" (#17, "SHPACTIN.PACKAGE.EN", f1%(17%))
            if f1%(17%) = 1% then get #17 using L09276, package_enabled%
L09276:         FMT POS(21), BI(1)

*        Clear any Sales Order for this User that was in process.
            readkey$ = all(hex(00))  :  str(readkey$,,3) = userid$
            call "READ101" (#9, readkey$, f1%(9))  /* BCKBUFFR */
            if f1%(9) = 0% then L10000
                so$ = key(#9, 2%)
                get #9 using L09350, readkey$
L09350:              FMT XX(10), CH(8)
                u3% = 2%
                if readkey$ = "SHPACTIN" then L09450
                     pf$(1) = "You did not complete processing Sales" &  ~
                              " Order " & so$
                     pf$(2) = "in the program " & str(readkey$,,8) & "."
                     pf$(3) = "Press (RETURN) to exit this program."
                     call "ASKUSER" (u3%, "IN-PROCESS MESSAGE",          ~
                                     pf$(1), pf$(2), pf$(3))
                     goto exit_program
L09450:         call "ASKUSER" (u3%, "RESTART NOTE",                     ~
                     "Processing for the Sales Order shown below was",   ~
                     "not completed.  Press (RETURN) To Continue", so$)
                get #9 using L09490, bol$, seqnr$(1)
L09490:              FMT XX(45), CH(3), CH(3)
                if bol$ <> " " then L09540
                     delete #9
                     goto L10000

L09540
*        Restart Logic- Complete Processing of Line Item in-process
            inpmessage$ = "Restarting BOL: " & bol$ & "/" & seqnr$(1)
            call "SHOSTAT" (inpmessage$)
            c%, maxlines%, restart% = 1%
            get #9 using L09620,  cuscode$, so$, bol$, seqnr$(1), part$(1),~
                                store$, schld(1), delete%, newlot$(),    ~
                                newlotqty(), shipdate$, price(1),        ~
                                sn_index1%()
L09620:         FMT XX(20), CH(9), CH(16), 2*CH(3), CH(25), CH(3),       ~
                    PD(14,4), BI(1), 30*CH(6), 30*PD(14,4), CH(6),       ~
                    PD(14,4), 30*BI(4)
            goto restart  /* Fake out the line item portion of save    */
            return_from_restart
                restart% = 0%
                gosub clear_in_process   /* We're technically done     */
                gosub initialize_variables
                gosub load_data          /* Load up the BOL            */
                if errormsg$ <> " " then L10000 else edit_header

L10000: REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            * --------------------------------------------------------- *~
            * Handles normal input for data entry screens.              *~
            *************************************************************
                                        /* (EWD002) - Begin        (B) */
                                        /* (APCPLNSC) - File           */
                                                      /* Point 'A'     */
        init(" ") sc_key$, sav_so$, sav_load$, sc_inv$
        so_msg$ = "Processing S.O. [xxxxxxxx] Drop [xx] Adj's [xxx]"
        sav_load$ = load$
        str(sc_key$,1%,5%) = sav_load$

        inputmode
            read #45,hold,key 1% > sc_key$, using L10005, sc_key$,      ~
                              adt_cuscode$, sc_inv$, sc_st$, eod goto close_trailer
                                                           /* (AWD010) */
                                                           /* (AWD011) */
L10005:        FMT POS(7), CH(27), POS(59), CH(09), POS(104), CH(1), POS(110), CH(2)          
            if str(sc_key$,1%,5%) <> sav_load$ then goto close_trailer
            if sc_inv$ = "N" then goto inventory_error
               gosub update_sc           /* (APCPLNSC) - File          */
               
/* (AWD015) */

            init(" ") adt_so$, adt_seq$, adt_po$, function$, notes$
            seq% = 0%
            function$ = "inputmode"
            adt_so$   = str(sc_key$,18%,8%)
            convert str(sc_key$,25%,2%) to seq%, data goto badSC_Seq
            
            convert seq% to adt_seq$, pic(###)
            
badSC_Seq:            
            notes$ = "read apcplnsc & update sc status to 18"
            if shipaudit% = 1% then gosub shpAuditLog   
/* (\AWD015) */            
            if sav_so$ = str(sc_key$,18%,8%) then goto inputmode

               sav_so$ = str(sc_key$,18%,8%)
               so$     = sav_so$         /* Set Sales Order            */
               gosub check_status        /* (APCPLNOR) = File          */
               if or_status% > 17% then goto inputmode
                                              /* Skip if BOL Exists    */
                                              /* (EWD004) No Invoice   */  
            init(" ") errormsg$, inpmessage$, so$, bol$
            gosub initialize_variables

            so$ = sav_so$                     /* Set Sales Order       */
            str(so_msg$,18%,8%) = so$
            str(so_msg$,34%,2%) = str(sc_key$,11%,2%)

            gosub L50105                      /* Load S.O. and Line    */
                                              /* Items from (BCKLINES) */
            if len(errormsg$) < 5 then goto L10010 /* E r r o r        */ 
            for x% = 1% to maxlines%
                gosub'152(x%)                 /* Edit Line Items       */
            next x%
 
L10010:     if len(errormsg$) < 5 then goto L10070 /* Clean Load of S.O*/
               errormsg$ = "(Err) Creating Bol for ("&so$&") will be Skipped"
               gosub error_prompt
               gosub clear_in_process
               goto inputmode                 /* PF(16) Exit Program   */
L10070:     gosub check_removes               /* Adjust BOL'S for S.O. */
            convert t_qty% to str(so_msg$,45%,3%), pic(###)
            print at(04,17);hex(a4);so_msg$;  /* Screen Display        */

            gosub lookup_arrival              /* (AWD009)              */ 
            gosub datasave                    /* Create BOL and Invoice*/
            gosub update_status               /* Set BOL Status = '18' */
                                              /* in (APCPLNOR)         */
         goto inputmode                       /* Continue Until all the*/
                                              /* Sales Orders are Procd*/

         check_removes                        /* (EWDBOLRM) - File     */
            t_qty% = 0%
            init(" ") rm_key$
REM            str(rm_key$,1%,5%) = sav_load$ /* (EWD007) - Take load out of read */
            str(rm_key$,1%,8%) = so$          /* Change rm_key$ value to just SO */
         check_removes_nxt
            read #49,key 1% > rm_key$, using L10075, rm_key$, rm_qty%,      ~
                                              eod goto check_removes_done
L10075:        FMT POS(6), CH(11), BI(2)      /* (EWD007) - Begin - Take load out */
REM            if str(rm_key$,1%,5%) <> sav_load$ then goto check_removes_done
            if str(rm_key$,1%,8%) <> so$ then goto check_removes_done 
               ln_item% = 0%
               convert str(rm_key$,9%,3%) to ln_item%, data goto L10080
                                                  /* (EWD007) - End    */  

L10080                                        /* Test for Valid Line It */
               if ln_item% < 1% or ln_item% > maxlines% then             ~
                                              goto check_removes_nxt
                                              /* Adjust Scheduled Qty   */
                                              /* based on Removal       */
               schld(ln_item%) = schld(ln_item%) - rm_qty%
               if schld(ln_item%) < 0 then schld(ln_item%) = 0.0
               call "CONVERT" (schld(ln_item%), 2.2, schld$(ln_item%))

               gosub'152(ln_item%)            /* Edit Line Item         */
                                              /* Do Not allow Negative  */
               t_qty% = t_qty% + rm_qty%
               goto check_removes_nxt         /* Adjust all Applicable  */
         check_removes_done                   /* line items on S.O.     */   
         return

         update_sc                            /* (APCPLNSC) - Update    */
            sc_st% = 0%                       /* all Line Items on S.O. */
            convert sc_st$ to sc_st%, data goto L10085
L10085
            if sc_st% > 17% then return       /* BOL already Cut        */
            put #45, using L10090, "18", date /* Set BOL Cut Flag       */      
L10090:       FMT POS(110), CH(2), CH(6)      /* use Todays date        */
            rewrite #45
         return
                                              /* (EWD002) End Mods  (B) */  
  
            for fieldnr% = 1% to 2%
L10110:         gosub'051(fieldnr%)      /* Check Enables, Set Defaults*/
                     if enabled% = 0 then L10230
L10130:         gosub'101(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  4 then       L10210
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10210:               if keyhit%  = 16 then       exit_program
                      if keyhit% <>  0 then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

*        Check Lines for Lot numbers; get Serial #s.
            edit% = 1%
        if maxlines% = 0% then L11000
            for c% = 1% to maxlines%
                call "LOTENABL" (part$(c%), lot_enable%, lot%, #17, #12)
                if lot_enable%  = 0% then lot$(c%,1%) = " "
L10330:         gosub'152(c%)            /* Test data, get Dist, S/Ns  */
                if errormsg$ = " " then L10480
                     top% = c%  :  fieldnr% = 1%
                     gosub'052                    /* Set input message */
L10420:              if avail_toggle% = 0% then                          ~
                        gosub'102(c%, fieldnr%, 3%)  /* Get input    */  ~
                        else                                             ~
                        gosub'103(c%, fieldnr%, 3%)  /* Get input    */
                          if keyhit%  =  1% then gosub startover
                          if keyhit%  =  0% then L10330 else L10420
L10480:     next c%
            errormsg$ = " "

L11000: REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            * --------------------------------------------------------- *~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        edit_header
            lastfield%  = 0%
            inpmessage$ = "Position Cursor and Press (RETURN) to Edit"
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
            errormsg$ = " "
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  2 then       edit_lines
                  if keyhit%  = 12 then gosub delete_from_schedule
                  if keyhit%  = 16 then gosub datasave
                  if keyhit%  = 26 then gosub display_so_hdr_text
                  if keyhit%  = 28 then gosub capture_inv_header_text
                  if keyhit% <>  0 then       edit_header
L11170:     fieldnr% = cursor%(1) - 5
            if fieldnr% < 2% or fieldnr% > 12% then edit_header
        edit_header_fs    /* Start Full Screen Edit */
            fieldnr% = 2%
            if fieldnr% = lastfield% then edit_header
            gosub'051(fieldnr%)         /* Set Input Message           */
L11220:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11220
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11220
                  lastfield% = fieldnr%
                  goto L11170

        edit_lines
            edit% = 2%
            lastfieldnr% = 0%
            inpmessage$ = "Position Cursor and Press (RETURN) to Edit" & ~
                          " Shipment Quantity and/or Lot(s)."
            gosub create_disp
            if avail_toggle% = 0% then                                   ~
               gosub'102(0%, 0%, 1%)    /* Display Screen - No Entry   */~
               else                                                      ~
               gosub'103(0%, 0%, 1%)    /* Display Screen - No Entry   */
            errormsg$ = " "
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  2% then gosub first
                  if keyhit%  =  3% then gosub last
                  if keyhit%  =  4% then gosub prev
                  if keyhit%  =  5% then gosub nexts
                  if keyhit%  =  6% then gosub down
                  if keyhit%  =  7% then gosub up
                  if keyhit%  =  9% then gosub edit_header
                  if keyhit%  = 10% then avail_toggle% = 1%- avail_toggle%
                  if keyhit%  = 11% then gosub toggle_pn_desc
                  if keyhit%  = 12% then gosub ship_complete
                  if keyhit%  = 22% then gosub flag_complete
                  if keyhit%  = 26% then gosub display_so_line_text
                  if keyhit%  = 28% then gosub capture_inv_line_text
                  if keyhit%  = 29% then gosub set_package_enables
                  if keyhit%  = 16% then gosub datasave
                  if keyhit% <>  0% then       edit_lines
L11550:     gosub calc_line_number       /* DERIVES C%, FIELDNR% */
            if errormsg$ <> " " then edit_lines
            if fieldnr% = lastfieldnr% then edit_lines
            gosub'052                   /* Set INPMESSAGE$             */
L11580:     if avail_toggle% = 0% then                                   ~
               gosub'102(c%, fieldnr%, 2%) /* Display & Accept Screen  */~
               else                                                      ~
               gosub'103(c%, fieldnr%, 2%) /* Display & Accept Screen  */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L11580
            gosub'152(c%)               /* Edit Field for Valid Entry  */
                  if str(errormsg$,,7) = "WARNING" then L11710
                     if errormsg$ <> " " then L11580
L11710:           lastfieldnr% = fieldnr%
                  goto L11550

        REM *************************************************************~
            *             M I S C    R O U T I N E S                    *~
            * --------------------------------------------------------- *~
            * Some routines invoked by the PF keys above.               *~
            *************************************************************

        first : top% = 1%                                        : return
        last  : top% = max(1%, min(86%, maxlines%-14%))          : return
        prev  : top% = max(1%, top%-14%)                         : return
        nexts : top% = max(1%, min(86%, top%+14%, maxlines%-14%)): return
        down  : top% = max(1%, top%-1%)                          : return
        up    : top% = max(1%, min(86%, top%+1%, maxlines%-1%))  : return


        display_so_hdr_text
            msg$ = str(line2$,,60)
            call "TXTDSPLY" (#16, f2%(16), "013", msg$, textso$,         ~
                             sotext$())
            return

        display_so_line_text
            gosub calc_line_number : if c% = 0% then return
            msg$ = "Customer: " & cuscode$ & " (" & cusname$ & ")  BOL: "~
                   & so$ & "-" & bol$ & "  Line: " & seqnr$(c%)
            call "TXTDSPLY" (#16, f2%(16), "014", msg$, textso$(c%),     ~
                             sotext$())
            return

        capture_inv_header_text
            msg$ = str(line2$,,60)
            call "TXTINSUB" (#16, f2%(16), "015", msg$, textiv$,         ~
                             ivtext$())
            return

        capture_inv_line_text
            gosub calc_line_number : if c% = 0% then return
            msg$ = "Customer: " & cuscode$ & " (" & cusname$ & ")  BOL: "~
                & so$ & "-" & bol$ & "  Line: " & seqnr$(c%)
            call "TXTINSUB" (#16, f2%(16), "016", msg$, textiv$(c%),     ~
                             ivtext$())
            return

        calc_line_number
            fieldnr% = cursor%(1) - 5%
            c% = cursor%(1) - 6% + top%
            if c% >= 1% and c% <= maxlines% then return
                c% = 0%
                errormsg$ = "Position Cursor to the Line for Display."
                return

        clear_in_process
            readkey$ = all(hex(00))
            str(readkey$,,3) = userid$
            call "DELETE" (#9, readkey$, 10%)
            return


        delete_from_schedule
*        (1)Reduce scheduled qtys on order line items; update inventory
*        (2)Delete Header and Lines from Scheduled file
*        (3)Remove in-process status
L12490:     u3% = 2%
            call "ASKUSER" (u3%, "DELETE FROM SCHEDULE",                 ~
                            "Enter PF-16 to remove Order from Schedule", ~
                            "-OR-", "(RETURN) to abort Delete.")
            if u3% = 0% then return
            if u3% <> 16% then L12490
                if bolnew% = 1% then L12670
                     delete% = 1%
                     init(" ") newlot$(), lot$()   /* Clear New Arrays */
                     mat newlotqty = zer
                     mat lotqtys   = zer
                     mat schld     = zer

                     /* Take care of serial numbered parts in schedule */
                     plowkey$ = all(hex(00))
L12574:              call "PLOWNXT1" (#33, plowkey$, 0%, f1%(33))
                     if f1%(33) = 0% then L12598
                          get #33 using L12580, sn_no$, sn_part$
L12580:                        FMT POS(4), CH(20), CH(25)
                          readkey$ = str(sn_part$) & sn_no$
                          call "READ101" (#32, readkey$, f1%(32))
                          if f1%(32) = 0% then L12594  /* Shouldn't be */
                               put #32 using L12590, "2", " ", " "
L12590:                             FMT CH(1), POS(216), CH(2), CH(40)
                               rewrite #32
L12594:                   delete #33
                          goto L12574

L12598:              gosub save_data
                     delete% = 0%

                     readkey$ = str(cuscode$) & str(so$) & str(bol$)
                     call "DELETE" (#1, readkey$, 28%)
                     call "TXTFUTIL" (#16, f2%(16), "DELE", textiv$)
                     readkey$ = str(so$) & str(bol$) & hex(00)
                     call "DELETE" (#2, readkey$, 19%) /* Just in Case */
                     for c% = 1% to maxlines%
                         call "TXTFUTIL" (#16, f2%(16), "DELE",          ~
                                          textiv$(c%))
                     next c%
                     gosub delete_print_file
L12670:         gosub clear_in_process
                return clear all
                goto inputmode

        toggle_pn_desc
            for i% = 1% to maxlines%
                if toggl% = 0% then pn_or_desc$(i%) = partdesc$(i%)      ~
                               else pn_or_desc$(i%) = part$(i%)
            next i%
            if toggl% = 0% then toggl% = 1% else toggl% = 0%
            return


        flag_complete
            gosub calc_line_number : if c% = 0% then return
            if comp$(c%) = " " then comp$(c%) = "C" else comp$(c%) = " "
            return

        create_disp
            for x% = 1% to 100%
                str(disp$(x%),1,1) = txt$(x%)
                str(disp$(x%),2,1) = comp$(x%)
                str(disp$(x%),3,1) = schflag$(x%)
            next x%
            return

        ship_complete
            ser_prob%, lot_prob%, qty_prob% = 0%
            for i% = 1% to maxlines%
                if avail(i%) = 0 then for_loop /* No need to do rest */
                /* 1st test for serial numbered parts */
                call "SERENABL" (part$(i%), e%, l%, #17, #12)
                if e% = 0% then L12990
                     ser_prob% = 1%
L12960:              schld(i%) = 0%
L12970:              call "CONVERT" (schld(i%), 2.2, schld$(i%))
                     goto for_loop
L12990:         /* 2nd test for full lot tracked part */
                call "LOTENABL" (part$(i%), e%, l%, #17, #12)
                if e% <> 2% then L13050
                     /* Now we assume that the lot is valid or blank */
                     /* Blank is a no-no for strict lot tracked parts */
                     if dfltlot$(i%) <> " " then L13050
                         lot_prob% = 1%
                         goto L12960
L13050:         /* 3rd we use the default lot to check qty availablity */
                /* AVAIL(I%) is the qty not scheduled, pre-invoiced, or */
                /*   invoiced.  (i.e. the uncommitted quantity.)        */
                /* TQ is the qty defaulted for the part on prev lines.  */
                tq = 0
                for j% = 1% to (i%-1%)
* PAR000
                     if epart$(j%) <> epart$(i%) then L13068
                          tq = tq + lotqtys(j%,1%)
L13068:              next j%
*PAR000
                call "INVAVAIL" (#12, #18, epart$(i%), store$, dfltlot$(i%),~
                                 errormsg$, avail(i%) + tq,    ~
                                 temp_avail, ret%)
                if errormsg$ = " " then L13130
                     qty_prob% = 1%
                     schld(i%) = max (0, min (avail(i%), temp_avail - tq))
L13110:              if schld(i%) = 0 then L12970
                          lot$(i%,1%) = dfltlot$(i%)
                          lotqtys(i%,1%) = schld(i%)
                          call "CONVERT" (lotqtys(i%,1%),2.2,lotqty1$(i%))
                          lot1was$(i%) = lot$(i%,1%)
                          if e% <> 2% then L12970
                              if lot$(i%,1%) = " " then lotqty1$(i%) = " "
                              goto L12970
L13130:         /* Lastly we can really ship complete since all is ok */
                schld(i%) = avail(i%)
                goto L13110
            for_loop : next i%
            errormsg$ = " " : ret% = 0%
            if ser_prob% + lot_prob% + qty_prob% = 0% then return
                askmsg$() = " "
                if ser_prob% = 1% then askmsg$(1%) = "Serial Numbered " &~
                     "Part's"
                if ser_prob% = 1% and lot_prob% = 1% then askmsg$(1%) =  ~
                     askmsg$(1%) & " and"
                if lot_prob% = 1% then askmsg$(1%) = askmsg$(1%) &       ~
                     " Strict Lot Tracked Part's"
                if ser_prob% = 1% or lot_prob% = 1% then askmsg$(1%) =   ~
                     askmsg$(1%) & " quantities set to zero."
                if qty_prob% = 1% then askmsg$(2%) = "Lot Protected " &  ~
                     "Part's quantities set to maximum available."
                askmsg$(3%) = "Press RETURN to acknowledge...."
                u3% = 1%
                call "ASKUSER" (u3%, "SHIP COMPLETE  CONFLICT",          ~
                                askmsg$(1%), askmsg$(2%), askmsg$(3%))
                return

        set_package_enables
            if package_enabled% = 2% then                                ~
                askmsg$(1%) = "Package Fields are currently ENABLED."    ~
                                     else                                ~
                askmsg$(1%) = "Package Fields are currently DISABLED."
            askmsg$(2%) = "Press PF2 to Enable Package Fields -or-"
            askmsg$(3%) = "Press RETURN to Disable Package Fields."
L13570:     u3% = 2%
            call "ASKUSER" (u3%, "*** PACKAGE FIELDS SOFT ENABLES ***",  ~
                            askmsg$(1%), askmsg$(2%), askmsg$(3%))
            if u3% <> 0% and u3% <> 2% then L13570
            package_enabled% = u3%
            call "READ101" (#17, "SHPACTIN.PACKAGE.EN", f1%(17%))
            put #17 using L13640, "SHPACTIN.PACKAGE.EN", package_enabled%,~
                                                                     " "
L13640:         FMT CH(20), BI(1), CH(479)
            if f1%(17%) = 1% then rewrite #17 else write #17
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            * --------------------------------------------------------- *~
            * Saves data on file after INPUT/EDITING.                   *~
            *************************************************************

        datasave
            for c% = 1% to maxlines%
                if schld(c%) > 0 then L19130
            next c%
            goto L19130                    /* (EWD) Mod 04/23/98 */

            errormsg$ = "There is nothing indicated as shipped."
            return

        REM return clear all
L19130:     gosub save_data
        REM goto inputmode                 /* (EWD002) Disable   */
            return                         /* (EWD002) Continue  */ 

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            * --------------------------------------------------------- *~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

            deffn'051(fieldnr%)
                  enabled% = 1%
                  on fieldnr% gosub L20210,         /* Sales Order - BOL*/~
                                    L20260          /* All the rest     */
                     return

L20210
*        Sales Order - BOL                     SO$
            inpmessage$ = "Enter Sales Order (and BOL to recall a" &     ~
                          " previously scheduled order)."
            return

L20260
*        Scheduled Ship Date                   SHIPDATE$
            if shipdate$ = " " or shipdate$ = blankdate$ then ~
               shipdate$ = post_dte$           /* (EWD002)  */
            inpmessage$ = "Enter Header Data."

*        Carrier                               CARRIER$

*        How Ship                              HOWSHIP$

*        FOB                                   FOB$

*        Shipping Instructions                 INSTR$(2)

*        BOL Print Flag                        PRINTBOL$
            if printbol$ = " " then printbol$ = "Y"

*        Freight Bill Number                   FRTBILL$

*        Cartons Shipped                       CARTONS$

*        Shipment Weight                       WEIGHT$

*        Freight Amount                        FRTAMT$
            inpmessage$ = inpmessage$ &                                  ~
                          " Freight Charges in Statutory currency."
            if curr$ <> "Y" then return
            if currency$ = " " then return
            if currency$ = statutory$ then return
            inpmessage$ = inpmessage$ & " Freight in " & currency$ & " " ~
                                      & currdesc$
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   2     *~
            * --------------------------------------------------------- *~
            * Sets DEFAULTS and ENABLES fields for Screen  2  of Input. *~
            *************************************************************

        deffn'052
            enabled% = 1%
            on fieldnr% gosub L21100                /* Scheduled        */
            return

L21100
*        Scheduled Quantities                  SCHLD$
            inpmessage$ = "Enter Changes and then Press (RETURN)."
            return

        REM *************************************************************~
            *         I N I T I A L I Z E    V A R I A B L E S          *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, cuscode$, partdesc$(),     ~
                      cusname$, shipdate$, carrier$, carriername$,       ~
                      howship$, fob$, instr$(), seqnr$(), part$(),       ~
                      txt$(), textso$, textso$(), avail$(), lot$(),      ~
                      schld$(), schflag$(), store$, printbol$, frtbill$, ~
                      cartons$, weight$, frtamt$, dfltlot$, lotqty1$(),  ~
                      newlot$(), oldlot$(), printdate$, schlddate$,      ~
                      holdlot$(), mark$(), holdmark$(), pn_or_desc$(),   ~
                      currency$, currdesc$, currline$, comp$(),          ~
                      pack_qty$(), pack_type$(), dfltlot$(), or_key$,    ~
/*PAR000*/            subp$(), epart$(), bol$
                                                        /* (EWD)  Begin*/
            post_dte$ = date              
                                                        /* (EWD)  End  */

            init (hex(ff)) textiv$, textiv$()
            call "TXTFUTIL" (#16, f2%(16), "INTL", textiv$)
            init(hex(00)) str(lot1was$())
            top%    = 1%  : toggl% = 0%
            bolnew% = 0%  : avail_toggle% = 1%
            posting_window_ok% = 1%
            cartons, weight, frtamt = 0
            mat avail   = zer : mat newlotqty = zer : mat oldlotqty = zer
            mat schld   = zer : mat lotqtys   = zer : mat holdqty   = zer
            mat def_percent = zer : mat def_unit = zer
            mat sn_index1% = zer
            mat sn_index2% = zer
            call "ALLFREE"
            return

        REM *************************************************************~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *************************************************************

        startover
L29440:     u3% = 2%
            call "STARTOVR" (u3%)
            if u3%  = 1% then return
            if u3% <> 0% then L29440
        startover2
                return clear all              /* Wants to Start Over   */
L29500:         call "SERSTOVR" (0%, "4", "2", #32, #33)
                gosub clear_in_process
                goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            * --------------------------------------------------------- *~
            * Cases- (1) Order is not scheduled --> Setup, goto INPUT.  *~
            *        (2) Order is scheduled but not shipped --> Setup   *~
            *            and goto INPUT.                                *~
            *        (3) Order is shipped  --> Goto EDIT.               *~
            *        (4) Order is invoiced --> Notify and get out.      *~
            *************************************************************
        load_data
* PAR000 
REM         init(" ") sav_sc_key$
REM         str(sav_sc_key$,1%,27%) = sc_key$


*        First verify that the Sales Order exists.
            fnd% = 0% : so_saver$ = so$
L30055:     readkey$ = str(so$) & hex(00)
            call "PLOWNEXT" (#6, readkey$, 16%, f1%(6))  /* BCKLINES */
            if f1%(6) = 1% then L30080
                if fnd% <> 0% then goto L30076 /*Been here before?-Error*/
                if len(so$) > 7% then goto L30076 /*Too long?- Error */
                if pos(so$ = ".") <> 0% then goto L30076 /* Dots?- Error*/
L30070:         convert so$ to so%, data goto L30076 /*Not Num?- Error */
                convert so% to so$, pic (00000000) /* Zero-fill it */
                fnd% = 1%  /* Indicate 'been here before' */
                goto L30055 /* Try again with zero-filled SO$ */

L30076:         errormsg$ = "Sales Order is not on file."
                so$ = so_saver$
                return

L30080:     get #6 using L30085, cuscode$
L30085:         FMT CH(9)

/* (AWD015) */
            init(" ") adt_cuscode$, adt_so$, adt_seq$, adt_po$, function$, notes$
            seq% = 0%
            function$ = "load_data"
            adt_cuscode$ = cuscode$
            adt_so$   = str(readkey$,1%,8%)
            notes$ = "read bcklines to get cuscode"
            if shipaudit% = 1% then gosub shpAuditLog   
/* (\AWD015) */            

            readkey$ = str(cuscode$) & so$
            call "READ100" (#5, readkey$, f1%(5))     /* BCKMASTR */
            if f1%(5) = 0% then L30070  /* Hopefully never happens */
                get #5 using L30110, export$, crflag$, currency$
L30110:              FMT POS(857), CH(1), POS(875), CH(1), POS(893), CH(4)
REM                if crflag$ = " " or crflag$ = "A" then L30130
             if crflag$ <> "H" or crflag$ <> "C" then L30130
             if crflag$ = "H" then errormsg$ = "Order is on Credit Hold."
             if crflag$ = "C" then errormsg$ = "Order has been cancelled"
                     return
L30130:         if export$ <> "Y" then goto L30150
                   errormsg$ = "You may not select EXPORT Orders for " & ~
                               "Pre-Invoicing."
                return
L30150
*        Now look at the Bill of Lading
/* (AWD015) */
            init(" ") adt_cuscode$, adt_so$, adt_seq$, adt_po$, function$, notes$
            seq% = 0%
            function$ = "load_data"
            adt_cuscode$ = cuscode$
            adt_so$   = str(readkey$,1%,8%)
            notes$ = "bol$ check <> blank goto L30290"
            if shipaudit% = 1% then gosub shpAuditLog   
/* (\AWD015) */            
********** Check bol$
         if bol$ <> " " then L30290
         
*        Case where Order is not Scheduled.
          /* Check that there is nothing scheduled for this order      */
                readkey$ = str(so$) & hex(00)
                call "PLOWNEXT" (#2, readkey$, 16%, f1%(2))
                if f1%(2) = 0% then L30220
                     u3% = 2%         
                                                 /* (EWD002) - Begin   */
         REM         call "ASKUSER" (u3%, "CONTINUE???",                 ~
         REM              "There are BOLs outstanding for this Order.",  ~
         REM              " ", "Hit PF-16 to continue, RETURN to abort.")
         REM         if u3% = 16% then L30220    /* (EWD002) Disable   */   
         REM              return clear all
         REM              goto inputmode         /* (EWD002) Disabel   */
         REM                 errormsg$ = "There are BOLs Outstanding"
         REM                 return                 /* (EWD002) - End     */
                bol$ = str(readkey$,17%,3%)
                bolnew% = 1%
                goto found_shplines
         
L30220:         gosub check_inprocess  :  if errormsg$ <> " " then return
                bol$ = "NEW"  :  bolnew% = 1%
                
                
/* (AWD015) */
found_shplines:
            init(" ") adt_cuscode$, adt_so$, adt_seq$, adt_po$, function$, notes$
            seq% = 0%
            function$ = "load_data"
            adt_cuscode$ = cuscode$
            adt_so$   = str(readkey$,1%,8%)
            notes$ = "bol <> blank, check_inprocess called, bol$ = NEW, bolnew% = 1%"
            if shipaudit% = 1% then gosub shpAuditLog   
/* (\AWD015) */                            

          /* Load defaults and other data from order         */
                get #5 using L30245, cusname$, howship$, fob$, instr$(),  ~
                                    textso$, store$
L30245:              FMT XX(41), CH(30), POS(422), 2*CH(20), 2*CH(50),   ~
                         POS(799), CH(4), CH(3)
                gosub currency_test
                shipped% = 0%
/* (AWD015) */
            init(" ") adt_cuscode$, adt_so$, adt_seq$, adt_po$, function$, notes$
            seq% = 0%
            function$ = "load_data"
            adt_cuscode$ = cuscode$
            adt_so$   = str(readkey$,1%,8%)
            notes$ = "calling load_lines, if bcklines doesn't exist " ~ 
                     & "then deletes shplines #2 and returns"
            if shipaudit% = 1% then gosub shpAuditLog   
/* (\AWD015) */                        
                gosub load_lines    /* Load in SO Lines      */
REM                if shplne% <> 0% then c% = shplne%
                
                return

L30290
*        Bill of Lading Specified - load it up
            get #5 using L30300, cusname$, textso$
L30300:         FMT XX(41), CH(30), POS(799), CH(4)
            readkey$ = str(cuscode$) & str(so$) & bol$
            call "READ100" (#1, readkey$, f1%(1))
            if f1%(1) = 1% then L30330
                errormsg$ = "Bill of Lading is not in scheduling file."
                return
L30330:     get #1 using L30335, invnr$
L30335:         FMT POS(234), CH(8)
            if invnr$ = " " then L30360
               init(" ") invnr$ : goto L30360 /* (EWD) - 04/06/98     */
                                              /* 2nd BOL Cut          */
                errormsg$ = "Bill of Lading Invoiced on Invoice "& invnr$
                return
* PAR000
L30360:     gosub check_inprocess  
REM            gosub lookup_subpart
REM            gosub lookup_sc_qty
REM            subp$(c%) = str(bcksubpt_rec$,48%,20%)
            if errormsg$ <> " " then return

            shipped% = 0%
            get #1 using L30390, store$, schlddate$, carrier$, howship$,  ~
                                fob$, instr$(), shipdate$, frtbill$,     ~
                                cartons, weight, frtamt, textiv$, export$
L30390:         FMT XX(28), CH(3), 2*CH(6), 2*CH(20), 2*CH(50), CH(6),   ~
                    CH(20), 3*PD(14,4), POS(242), CH(4), CH(1)
            if shipdate$ <> " " and shipdate$ <> blankdate$ then ~
               shipped% = 1% else shipdate$ = schlddate$

/* (AWD015) */
            init(" ") adt_cuscode$, adt_so$, adt_seq$, adt_po$, function$, notes$
            seq% = 0%
            function$ = "load_data"
            adt_cuscode$ = cuscode$
            adt_so$   = str(readkey$,9%,8%)
            notes$ = "read and loaded SHPHDRS data "
            if shipaudit% = 1% then gosub shpAuditLog   
/* (\AWD015) */                 
            call "WHICHMON" (#17, shipdate$, posting_window_ok%)
            call "DATEFMT" (shipdate$)
            call "READ100" (#3, "CARRIERS " & str(carrier$), f1%(3))
                if f1%(3) = 1% then get #3 using L30425, carriername$
L30425:              FMT XX(24), CH(30)
            if export$ <> "Y" then goto L30450
                errormsg$ = "This is an EXPORT Bill of Lading"
                return

L30450:     call "TXTFUTIL" (#16, f2%(16), "LOAD", textiv$)
            if cartons <> 0 then call "CONVERT" (cartons, 2.2, cartons$)
            if weight  <> 0 then call "CONVERT" (weight , 2.2, weight$ )
            call "CONVERT" (frtamt, 2.2, frtamt$)
            gosub currency_test
            gosub load_lines
* PAR000
REM            init(" ") sc_key$
REM            str(sc_key$,1%,27%) = sav_sc_key$
            return


        load_lines
            maxlines%, sn_index% = 0%
            readkey$  = str(so$) & hex(00)
L30505:     call "PLOWNEXT" (#6, readkey$, 16%, f1%(6))
            if f1%(6) = 1% then L30600
              /* Kill any orphaned line items in the scheduling file   */
              /* (Assumed 0 scheduled otherwise line couldn't have     */
              /* been removed from the Sales Order                     */
                readkey1$ = str(so$) & str(bol$) & hex(00)
L30535:         call "PLOWNEXT" (#2, readkey1$, 19%, f1%(2))
                if f1%(2) <> 0% then L30560
                         mat holdlot$ = lot$
                         mat holdqty  = lotqtys
                         return
L30560:         get #2 using L30565, readkey$, str(readkey$,17), temp$
L30565:               FMT XX(9), CH(16), XX(3), CH(3), POS(460), CH(4)
                call "READ100" (#6, readkey$, f1%(6))
                if f1%(6) = 1% then L30535
                call "DELETE" (#2, readkey1$, 22%)
                call "TXTFUTIL" (#16, f2%(16), "DELE", temp$)
                goto L30535

L30600
*        Now load the Sales Order's Lines and the BOL if there
            get #6 using L30610, openqty
L30610:         FMT POS(109), PD(14,4)
            if bolnew% = 0% then L30625   /* Defer qty test if not new */
                if openqty <= 0 then L30505 /* Not open. Get next line */

L30625:     c%, maxlines% = maxlines% + 1%
            get #6 using L30640, seqnr$(c%), part$(c%), partdesc$(c%),    ~
                                invd, opn, schld, pre_inv, price(c%),    ~
                                dfltlot$, textso$(c%)
L30640:         FMT XX(25), CH(3), XX(3), CH(25), CH(32), POS(101),      ~
                    3*PD(14,4), POS(133), PD(14,4), POS(141), PD(14,4),  ~
                    POS(218), CH(6), POS(242), CH(4)
            pn_or_desc$(c%) = part$(c%)
* PAR000
            so_inv$  = so$
            item_no$ = seqnr$(c%)
            gosub lookup_subpart                /* PAR000 */
REM            gosub lookup_sc_qty                 /* PAR000 */
            subp$(c%) = str(bcksubpt_rec$,48%,20%)     /* PAR000 */
            epart$(c%) = str(part$(c%),,25%) & str(subp$(c%),,20%) /* PAR000 */

/* (AWD015) */
            init(" ") adt_cuscode$, adt_so$, adt_seq$, adt_po$, function$, notes$
            seq% = 0%
            function$ = "   "
            adt_cuscode$ = cuscode$
            adt_so$   = str(readkey1$,1%,8%)
            adt_seq$  = seqnr$(c%)
            notes$ = "read bcklines, bcksubpt and loaded 6 qtys, bolnew% ->"
            convert bolnew% to str(notes$,60%,1%), pic(#)
            
            if shipaudit% = 1% then gosub shpAuditLog   
/* (\AWD015) */              
            if textso$(c%) <> hex(ffffffff) and textso$(c%) <> " "       ~
                then txt$(c%) = "T"
            call "READ100" (#12, epart$(c%), stocked%(c%))      /* PAR000 */
            if stocked%(c%) = 0% then goto L30675
                get #12%, using L30673, def_percent(c%), def_unit(c%)
L30673:              FMT POS(742), PD(14,4), PD(14,4)        /*PAR000 */
L30675:     if bolnew% = 1% then                                         ~
                             sn_index2%(c%,1), sn_index% = sn_index% + 1%
            if bolnew% = 1% then L30855
              readkey1$ = str(so$) & str(bol$) & str(seqnr$(c%))
              call "READ100" (#2, readkey1$, f1%(2%))
              if f1%(2%) = 1% then L30708
                if openqty > 0 then L30855
                     seqnr$(c%), part$(c%), pn_or_desc$(c%) = " "
                     epart$(c%), subp$(c%)  = " "    /* PAR000 */
                     c%, maxlines% = maxlines% - 1%
                     goto L30505
L30708:         get #2 using L30715, schld(c%), oldlot$(), oldlotqty(),   ~
                              textiv$(c%), pack_qty$(c%), pack_type$(c%),~
                               comp$(c%)
L30715:              FMT POS(32), PD(14,4), 30*CH(6), 30*PD(14,4),       ~
                         POS(460), CH(4), CH(4), CH(8), CH(1)
                     call "TXTFUTIL" (#16, f2%(16), "LOAD", textiv$(c%))
                for i% = 1% to 30%
                  lot$   (c%, i%) = oldlot$  (i%)
                  lotqtys(c%, i%) = oldlotqty(i%)
                  if (oldlot$(i%) = " " and oldlotqty(i%) = 0)  or       ~
                                                shipped% <> 1% then L30850
                     sn_index% = sn_index% + 1%
                     sn_index2%(c%,i%) = sn_index%
                     sn_trankey$ = str(so$) & str(bol$) & str(seqnr$(c%))
                          convert i% to str(sn_trankey$,23,2), pic(00)
                     call "SERLOAD" (sn_index2%(c%,i%), "RS",            ~
                                     sn_trankey$, 25%, " ", " ",         ~
                                     #17, #34, #32, #33, wf%)
                     if wf% = 0% and lotqtys(c%,i%) = 0 then             ~
                                                   sn_index2%(c%,i%) = 0%
                     call "SERENABL" (part$(c%), sn_enable%, k%, #17,#12)
                     if wf% = lotqtys(c%,i%) or sn_enable% = 0% then L30850
                          put inpmessage$ using L30820, seqnr$(c%),       ~
                                          lot$(c%,i%), lotqtys(c%,i%), wf%
L30820:                   %Seq ###, Lot ######   Shipped: #######.##, Ser~
        ~ial Numbers: #######.##
                          call "ASKUSER" (2%, "SERIAL NUMBERS",          ~
                     "A mismatch between the quantity shipped and the",  ~
                     "number of serial numbers exists as follows:",      ~
                     inpmessage$)
L30850:         next i%
L30855:     if bolnew% = 1% then schld(c%),schld = opn      /*(EWD) Mod*/
            if shipped% = 1% then L30865
                lot$(c%, 1) = dfltlot$  :  lotqtys(c%, 1) = schld(c%)
                dfltlot$(c%) = dfltlot$ /* For ship complete */
L30865:     call "CONVERT" (lotqtys(c%, 1), 2.2, lotqty1$(c%))
            if schld(c%) <> 0   then lot1was$(c%) = lot$(c%,1)
            if lot$(c%,1) = " " then lotqty1$(c%) = " "
            order(c%) = opn + invd
            committed(c%) = invd + pre_inv + schld - schld(c%)
            avail(c%) = opn - schld - pre_inv + schld(c%)
            call "CONVERT" (avail(c%), 2.2, avail$(c%)) /* Uncommitted */
            call "CONVERT" (schld(c%), 2.2, schld$(c%))
            if schld + pre_inv <> schld(c%) then schflag$(c%) = "S"
            goto L30505


        check_inprocess
*        See if order is already in buffer.  If so can't touch
                                                        /* (EWD005) Fix  */
        REM    call "REDALT0" (#9, so$, 2%, f1%(9))
        REM    if f1%(9) = 0% then L30950
         read #9,hold,key 2% = so$, eod goto L30950
            delete #9
            goto L30950
                                                        /* (EWD005)      */    
L30935:         errormsg$ = "Order is already being processed.(Exit)?"
                gosub error_prompt                      /* (EWD005)      */
                return

L30950
*        All Ok-fine.  Flag Order as in-process.
            readkey$ = " "
            str(readkey$, 1, 3) = userid$
            str(readkey$, 4, 7) = all(hex(00))
            str(readkey$,11, 8) = "SHPACTIN"
            str(readkey$,21,25) = str(cuscode$) & so$
            write #9 using L30990, readkey$, " ", " ", " ", " ",          ~
                                  eod goto L30935
L30990:         FMT CH(45), 3*CH(250), CH(225)
            return


        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            * --------------------------------------------------------- *~
            * (1)Write BOL to Scheduling files.                         *~
            * (2)Adjust Inventory Quanties (by Part and Lots)           *~
            * (3)Adjust Scheduled Quantities on Order line items.       *~
            * (4)Create an Invoice in Open Session            (EWD)     *~
            * (5)Remove order in-process flag.                (EWD)     *~
            *************************************************************
        save_data
                                     /* (EWD002) - Do not Display Info */
*           print at(04,02), "Saving Data and Updating Inventory...     "

*        See if we need to assign a BOL Number
        if bol$ = "NEW" then gosub next_bol  :  goto L31140

            next_bol
                readkey$ = str(cuscode$) & so$
                call "READ101" (#5, readkey$, f1%(5))  /* BCKMASTR */
                get #5 using L31100, bol%
L31100:              FMT POS(878), BI(4)
                convert bol% to bol$, pic(##0)

                gosub check_bol
                bol% = bol% + 1%  :  if bol% = 1000% then bol% = 1%
                put #5 using L31100, bol%
                rewrite #5
                call "STRING" addr("LJ", bol$, 3%)
                return

         check_bol
                bol% = 0%
                convert bol$ to bol%, data goto L31110
L31110:
                convert bol% to bol$, pic(##0)
         return

L31140
*        Now write the BOL Header (Even for a delete).
            call "DATUNFMT" (shipdate$)
            init(" ") shipdate$                 /* (EWD003)      */
            shipdate$ = post_dte$

            readkey$ = str(cuscode$) & str(so$) & str(bol$)
            call "READ101" (#1, readkey$, f1%(1))
            put #1 using L35030,   cuscode$, so$, bol$, store$, shipdate$,~
                                  carrier$, howship$, fob$, instr$(),    ~
                                  shipdate$, frtbill$, cartons, weight,  ~
                                  frtamt, textiv$, export$, " "
            if f1%(1) = 0% then write #1  else  rewrite #1
            call "TXTFUTIL" (#16, f2%(16), "TOS2", textiv$)

*        Now write the lines, adjusted the qty scheduled in BCKLINES,
*        and update the inventory with the quantities shipped.

            for c% = 1% to maxlines%
              /* First we'll get the old line item data  */
                init (" ") oldlot$()
                mat oldlotqty = zer
                oldschld      = 0
          restart /* Entry point if we are restarting          */
                readkey$ = str(so$) & str(bol$) & str(seqnr$(c%))
                call "READ100" (#2, readkey$, f1%(2))
                if f1%(2) = 0% then L31265
                     get #2 using L31260, oldschld, oldlot$(), oldlotqty()
L31260:                   FMT XX(31), PD(14,4), 30*CH(6), 30*PD(14,4)
L31265:       /* Now set-up the New Data for the Line        */
                if restart% = 1% then L31300
                     for i% = 1% to 30%
                          newlot$  (i%)  = lot$  (c%, i%)
                          newlotqty(i%)  = lotqtys(c%, i%)
                          sn_index1%(i%) = sn_index2%(c%, i%)
                     next i%
L31300:         newschld = schld(c%)

            /* Update in-process record w/ this line item's data       */
                readkey$ = " "
                str(readkey$, 1, 3) = userid$
                str(readkey$, 4, 7) = all(hex(00))
                str(readkey$,11, 8) = "SHPACTIN"
                str(readkey$,21,25) = str(cuscode$) & so$
                call "READ101" (#9, str(readkey$,,10), f1%(9))

REM                rewrite #9 using L31370, readkey$, bol$, seqnr$(c%),      ~
REM                                        part$(c%), store$,               ~
REM                                        newschld, delete%, newlot$(),    ~
REM                                        newlotqty(), shipdate$,          ~
REM                                        price(c%), sn_index1%(), " ", " "
                put #9 using L31370, readkey$, bol$, seqnr$(c%),      ~
                                        part$(c%), store$,               ~
                                        newschld, delete%, newlot$(),    ~
                                        newlotqty(), shipdate$,          ~
                                        price(c%), sn_index1%(), " ", " "
                if f1%(9) = 0% then write#9 else rewrite #9

L31370:              FMT CH(45), 2*CH(3), CH(25), CH(3),  PD(14,4),      ~
                         BI(1), 30*CH(6), 30*PD(14,4), CH(6), PD(14,4),  ~
                         30*BI(4), CH(200), CH(178)

            /* Post Inventory  */
                gosub place_hold
                                    /* Replace 'shipdate$' with todays */
                                    /* post_dte$           (EWD) Mod   */
* PAR000 change file channels to INV files added subp number
                call "SHPHNYSB"  ("P", store$, part$(c%), subp$(c%),     ~
                                               seqnr$(c%),  seqnr$(c%),  ~
                                  cuscode$, so$, bol$, " ", price(c%),   ~
                                  post_dte$, oldlot$(), oldlotqty(),     ~
                                  newlot$(), newlotqty(), newcost,       ~
                                  userid$, #19, #17, #20, #18, #13, #14, ~
                                  #12, #11, #15, #7, #8, " ", " ", " ")
                                  newcost = newcost
              /* Write Line Item To Scheduled File           */
                readkey$ = str(so$) & str(bol$) & str(seqnr$(c%))
                if delete% = 0% then L31465
                     call "DELETE" (#2, readkey$, 22%)
                     goto L31500
L31465:         call "READ101" (#2, readkey$, f1%(2))
                put #2 using L35195, cuscode$, so$, bol$, seqnr$(c%),     ~
                                    schld(c%), newlot$(), newlotqty(),   ~
                                    textiv$(c%), pack_qty$(c%),          ~
                                    pack_type$(c%), comp$(c%),           ~
                                    so$, seqnr$(c%), " "
                if f1%(2) = 0% then write #2  else  rewrite #2
                call "TXTFUTIL" (#16, f2%(16), "TOS2", textiv$(c%))

L31500:       /* Update Scheduled and Pre-Invoiced Qtys in BCKLINES    */
                if oldlotqty(1) = 0 then oldpreinv = 0 else              ~
                                         oldpreinv = oldschld
                if oldschld = newschld and oldpreinv =newschld then L31665
                     readkey$ = str(so$) &  str(seqnr$(c%))
                     call "READ101" (#6, readkey$, f1%(6))
                     get #6 using L31535, schld, preinv
L31535:                   FMT POS(117), PD(14,4), POS(133), PD(14,4)
                     if shipped% = 0% then schld = schld - oldschld
                     preinv = preinv - oldpreinv + newschld
                     put #6 using L31535, schld, preinv
                     rewrite #6

                                                   /* (EWD) - Begin    */
              /* Update Planning Info (PIPs)                           */
                call "APCPIPSB" (so$,              /* Sales Order #    */~
                                 seqnr$(c%),       /* Sequence #       */~
                                 store$,           /* Store Code       */~
                                 " ",              /* Credit Hold Flag */~
                                 #6 ,              /* BCKLINES,        */~
                                 #21,              /* DEMMASTR         */~
                                 #17,              /* SYSFILE2         */~
                                 #11,              /* PIPMASTR         */~
                                 #22,              /* PIPIN            */~
                                 #23,              /* PIPOUT           */~
                                 #30,              /* BOMSPEC          */~
                                 #24,              /* WCMASTR          */~
                                 #25,              /* WCOUT            */~
                                 #26,              /* SFMASTR2         */~
                                 #15,              /* SFCUM2           */~
                                 #27,              /* JBCROSS2         */~
                                 #28,              /* PIPCROSS         */~
                                 #29 )             /* JBPIPXRF         */
                                                   /* (EWD) - End      */

L31665:       /* Update Serial Number TIF                              */
                for i% = 1% to 30%
                  if sn_index1%(i%) = 0% then L31705
                     sn_trankey$ = str(so$) & str(bol$) & str(seqnr$(c%))
                     convert i% to str(sn_trankey$,23,2), pic(00)
                     call "SERSAVE" (sn_index1%(i%), "RS", sn_trankey$,  ~
                                     25%, part$(c%), userid$, "4", "2",  ~
                                     0%, #17, #34, #32, #33)
L31705:         next i%

            if restart% = 1% then L31730
            next c%

L31730
*        Write the Shipping Documents Print File
            gosub delete_print_file
            if restart% = 1% then return_from_restart
            if delete% <> 0%  or printbol$ = "N" then L31785
                printdate$ = shipdate$
                init(" ") printdate$            /* (EWD003)     */
                printdate$ = post_dte$
                if printbol$ = "I" then printdate$ = blankdate$
                                                         /* (EWD) Begin*/
                write #4 using L31770, "B", store$, printdate$, "B",     ~
                                      cuscode$, so$, bol$, date
             
L31770:         FMT CH(1), CH(3), CH(6), CH(1), CH(9), CH(16), CH(3),    ~
                    CH(6)                        
L31785
*        Lastly, clear the Order In-process flag

            gosub create_invoice
            gosub clear_in_process
            return                                      /* (EWD) End   */


        delete_print_file
*        Remove Pick List and BOL print Requests
            readkey$ = "P" & str(cuscode$) & str(so$) & bol$
            call "DELETE" (#4, readkey$, 29%)
            readkey$ = "B" & str(cuscode$) & str(so$) & bol$
            call "DELETE" (#4, readkey$, 29%)
            return


        place_hold
            if hnyactve$ = "N" then return /* INVENTORY INACTIVE       */
            for l% = 1% to 30%
                if mark$(c%,l%) <> " " then L31990
                if lotqtys(c%,l%) = 0  then L31985
                   lotqty, postqty = 0
                   for i% = l% to 30%
                     if mark$   (c%,i%) <> " " then L31915
                     if lotqtys (c%,i%)  = 0   then L31910
                     if lot$    (c%,i%) <> lot$   (c%,l%) then L31915
                        lotqty = lotqty + lotqtys(c%,i%)
L31910:                 mark$(c%,i%) = "*"
L31915:            next i%

                   for i% = 1% to 30%
                     if holdmark$(c%,i%) <> " " then L31955
                     if holdqty  (c%,i%)  = 0   then L31950
                     if holdlot$ (c%,i%) <> lot$    (c%,l%) then L31955
                        postqty = postqty + holdqty (c%,i%)
L31950:                 holdmark$(c%,i%) = "*"
L31955:            next i%

                held = max(0, lotqty - postqty)
                if held = 0 then L31985
                call "HNYHOLD" (#18, part$(c%), store$, lot$(c%,l%),     ~
                                          held, return%)
L31985:         mark$(c%,l%) = "*"
L31990:     next l%
            return


        currency_test
            if curr$ <> "Y" then return
            if currency$ = " " then currency$ = statutory$
            if currency$ = statutory$ then return
            call "DESCRIBE" (#40, currency$, currdesc$, 1%, f1%(40))
            currline$ = "Freight charge stated per S.O. currency: " &    ~
                currency$ & " " & currdesc$
            return

        REM *************************************************************~
            *        F O R M A T    S T A T E M E N T S                 *~
            * --------------------------------------------------------- *~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35030: FMT                 /* FILE: SHPHDRS                           */~
            CH(9),          /* Customer Ship-to Address Identifier     */~
            CH(16),         /* Sales order number                      */~
            CH(3),          /* Bill of Lading Number                   */~
            CH(3),          /* Store Code                              */~
            CH(6),          /* Ship Date                               */~
            CH(6),          /* Shipping Carrier Code                   */~
            CH(20),         /* how ship information                    */~
            CH(20),         /* f.o.b. information                      */~
            2*CH(50),       /* Shipping Instructions                   */~
            CH(6),          /* Ship Date                               */~
            CH(20),         /* Freight/ Air Bill Number                */~
            PD(14,4),       /* Number of Cartons                       */~
            PD(14,4),       /* Shipment Weight                         */~
            PD(14,4),       /* Freight Amount                          */~
            XX(8),          /* Invoice Number                          */~
            CH(4),          /* Text ID                                 */~
            CH(1),          /* Export Flag                             */~
            CH(54)          /* Filler For Rest of Record               */

L35195: FMT                 /* FILE: SHPLINES                          */~
            CH(9),          /* Customer Ship-to Address Identifier     */~
            CH(16),         /* Sales order number                      */~
            CH(3),          /* Bill of Lading Number                   */~
            CH(3),          /* General purpose sequence number         */~
            PD(14,4),       /* Quantity scheduled for Shipment         */~
            30*CH(6),       /* Lot Number                              */~
            30*PD(14,4),    /* Lot Quantities                          */~
            CH(4),          /* Text ID                                 */~
            CH(4),          /* Package quantity                        */~
            CH(8),          /* Package type                            */~
            CH(1),          /* Completed flag                          */~
            CH(16),         /* Sales order number                      */~
            CH(3),          /* SO Line item number                     */~
            CH(105)         /* FILLER                                  */

        REM *************************************************************~
            *   A C C E S S    L O C A T I O N    M A N A G E M E N T   *~
            *___________________________________________________________*~
            * Call HNYLCSUB and pass the Part, Store, Lot and Quantity  *~
            * so that the user does not have to re-enter them.          *~
            *************************************************************

         locations

            idx% = cursor%(1) - 6% + top%

            if avail$(idx%) <> " " then L37150
               qty = 0
               goto L37170

L37150:     convert avail$(idx%) to qty

L37170:     call "HNYLCSUB"  (part$(idx%),                               ~
                              store$,                                    ~
                              lot$(idx%,1),                              ~
                              qty,                                       ~
                              4%,    /*  Withdrawl Mode                */~
                              #17,   /*  SYSFILE2                      */~
                              #31,   /*  STORNAME                      */~
                              #35,   /*  USERINFO                      */~
                              #12,   /*  INVMASTR                      */~
                              #36,   /*  HNYLOCNS                      */~
                              #18,   /*  INVQUAN                       */~
                              #37)   /*  LOCATION                      */
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            * --------------------------------------------------------- *~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
            line2$ = "Customer: " & cuscode$ & " (" & cusname$ & ")"
            if store$ <> " " then line2$ = line2$ & "  Store: " & store$
            str(line2$,62) = "APCINVBO: " & "R7.00.00"        /* (EWD) */
            if fieldnr% > 0% then init(hex(8c)) lfac$(), nfac$(), ufac$()~
                             else init(hex(86)) lfac$(), nfac$(), ufac$()
            gosub setpf1
            on fieldnr%  gosub      L40130,         /* Sales Order - BOL*/~
                                    L40145          /* All the rest     */
            goto L40180

L40130:           REM Set FAC's for Upper Case Only Input
                      lfac$(fieldnr%) = hex(81)
                      return
L40145:           REM Set FAC's for all the rest
                      lfac$(fieldnr%) = hex(80)
                      nfac$(fieldnr%) = hex(82)
                      ufac$(fieldnr%) = hex(81)
                      return

                                               /* (EWD) Next 15 Lines  */  
L40180: accept                                                           ~
            at (01,02), "Shipment Pre-Invoicing",                        ~
            at (01,64), "Today:",                                        ~
            at (01,71), fac(hex(8c)),  fdate$                   , ch(10),~
            at (02,51), "Invoicing Session Id:",                         ~
            at (02,73), fac(hex(84)),  session$                 , ch(06),~
            at (03,02), fac(hex(ac)),  line2$                   , ch(79),~
            at (04,02), fac(hex(94)),  errormsg$                , ch(79),~
                                                                         ~
            at (06,02), "Sales Order - BOL",                             ~
            at (06,30), fac(lfac$( 1)), so$                     , ch(16),~
            at (06,47), "-",                                             ~
            at (06,49), fac(lfac$( 1)), bol$                    , ch(03),~
            at (06,60), "Load No.:",                                     ~
            at (06,70), fac(hex(84)),   load$                   , ch(05),~
                                                                         ~
            at (07,02), "Date Shipped (Post Date)",                      ~
            at (07,30), fac(ufac$( 2)), shipdate$               , ch(08),~
                                                                         ~
            at (08,02), "Carrier",                                       ~
            at (08,30), fac(ufac$( 2)), carrier$                , ch(06),~
            at (08,49), fac(hex(8c)),   carriername$            , ch(32),~
                                                                         ~
            at (09,02), "How Ship",                                      ~
            at (09,30), fac(lfac$( 2)), howship$                , ch(20),~
                                                                         ~
            at (10,02), "FOB",                                           ~
            at (10,30), fac(lfac$( 2)), fob$                    , ch(20),~
                                                                         ~
            at (11,02), "Shipping Instructions",                         ~
            at (11,30), fac(lfac$( 2)), instr$(1)               , ch(50),~
            at (12,30), fac(lfac$( 2)), instr$(2)               , ch(50),~
                                                                         ~
            at (13,02), "Print BOL? (Y/N/I)",                            ~
            at (13,30), fac(ufac$( 2)), printbol$               , ch(01),~
                                                                         ~
            at (14,02), "Freight/Air Bill Number",                       ~
            at (14,30), fac(lfac$( 2)), frtbill$                , ch(20),~
                                                                         ~
            at (15,02), "Number of Cartons Shipped",                     ~
            at (15,30), fac(nfac$( 2)), cartons$                , ch(10),~
                                                                         ~
            at (16,02), "Shipment Weight",                               ~
            at (16,30), fac(nfac$( 2)), weight$                 , ch(10),~
                                                                         ~
            at (17,02), "Freight Charges",                               ~
            at (17,30), fac(nfac$( 2)), frtamt$                 , ch(10),~
            at (18,02), fac(hex(8c)),   currline$               , ch(79),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)),   pf$(1)                  , ch(79),~
            at (23,02), fac(hex(8c)),   pf$(2)                  , ch(79),~
            at (24,02), fac(hex(8c)),   pf$(3)                  , ch(79),~
                keys(pfkey$), key(keyhit%)


            if keyhit% <> 13% then L40455
                call "MANUAL" ("SHPACTIN")
                goto L40180

L40455:     if keyhit% <> 15% then L40472
               call "PRNTSCRN"
               goto L40180

L40472:     if keyhit% <> 10% then L40480
                readkey$ = " "
                errormsg$ = hex(06) & "Select Sales Order To Schedule"
                call "GETCODE"(#5, readkey$, errormsg$, 0%, 0.46, f1%(5))
                if f1%(5) <> 1% then L40180
                     so$ = str(readkey$,10,16) : bol$ = " " : keyhit%=0%
                     return

L40480:     if keyhit% <> 14% then L40491
                readkey$ = " "
                call "GETCODE" (#1, readkey$, " ", 0%, 0.03, f1%(1))
                if f1%(1) <> 1% then L40180
                     so$  = str(readkey$,10,16)
                     bol$ = str(readkey$,26, 3) : keyhit% = 0%
                     return

L40491:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
            return

        setpf1
        if edit% = 2% then L40565         /* Input Mode                 */
           pf$(1) = "(1)Start Over                       (10)See SOs on"&~
                    " file        (13)Instructions"
           pf$(2) = "                 (4)Previous Field  (14)See BOLs o"&~
                    "n file       (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "             (16)Exit Program"
           pfkey$ = hex(01ffff04ffffffffff0affff0d0e0f10ffffff00ffff)
           if fieldnr% = 1% then L40544
                str(pf$(1),36,21), str(pf$(2),36,21) = " "
                str(pfkey$,10,1), str(pfkey$,14,1) = hex(ff)
                str(pf$(3),64,16) = " "
                str(pfkey$,16,1) = hex(ff)
L40544:    if fieldnr% > 2% then L40555
                str(pf$(2),18,18) = " "
                str(pfkey$, 4, 1) = hex(ff)
L40555:    return

L40565:  if fieldnr% > 0% then L40620     /* Edit Mode- Select Field    */
*         INPMESSAGE$ = " "
           pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "(2)Line Items                (26)Display Order Tex"&~
                    "t            (15)Print Screen"
           pf$(3) = "(12)Delete From Schedule     (28)Manage INV Header"&~
                    " Text        (16)Save Data   "
           pfkey$ = hex(0102ffffffffffffffffff0c0dff0f10ff1aff00191c)
           return

                                         /* Edit Mode- Field Enabled   */
L40620:    pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00ffff)
           return

        REM *************************************************************~
            *               S C R E E N   P A G E   2A                  *~
            * --------------------------------------------------------- *~
            * Document Input and Edit Screen.  Can't see Uncommitd here.*~
            *************************************************************

        deffn'102(c%, fieldnr%, edit%)
            line2$ = "Customer: " & cuscode$ & "  BOL: " & so$ & "-" &   ~
                     bol$ & "  Store: " & store$
            str(line2$, 62) = "APCINVBO: " & "R7.00.00"      /* (EWD) */
            gosub setpf2
            if str(errormsg$,,7) = "WARNING" or edit% = 3% then L41120
            if errormsg$ <> " " then L41210
L41120:         init(hex(8c)) lfac$(), lfax$(), pfac$()
                if edit% >= 2% then L41170
                     init(hex(86)) lfac$()
                     init(hex(84)) lfax$(), pfac$()
                     goto L41210
L41170:         init(hex(81)) lfac$(fieldnr%)
                if package_enabled%= 2% then init(hex(81)) pfac$(fieldnr%)
                call "LOTENABL" (part$(c%), lot_enable%, lot%, #17, #12)
                if lot_enable% > 0% then init(hex(81)) lfax$(fieldnr%)

                                                 /* (EWD) Next 8 Lines */
L41210: accept                                                           ~
            at (01,02), "Shipment Pre-Invoicing",                        ~
            at (01,64), "Today:",                                        ~
            at (01,71), fac(hex(8c)),  fdate$                   , ch(10),~
            at (02,51), "Invoicing Session Id:",                         ~
            at (02,73), fac(hex(84)), session$                  , ch(06),~
            at (03,02), fac(hex(ac)),  line2$                   , ch(79),~
            at (04,02), fac(hex(94)),  errormsg$                , ch(79),~
                                                                         ~
            at (05,02), fac(hex(ac)), hdr1$                     , ch(03),~
            at (05,06), fac(hex(ac)), hdr2$                     , ch(28),~
            at (05,35), fac(hex(ac)), hdr3$                     , ch(03),~
            at (05,39), fac(hex(ac)), hdr8$                     , ch(04),~
            at (05,44), fac(hex(ac)), hdr9$                     , ch(08),~
            at (05,53), fac(hex(ac)), hdr6$                     , ch(10),~
            at (05,64), fac(hex(ac)), hdr7$                     , ch(17),~
            at (06,02), fac(hex(80)),   seqnr$      (top% +  0%), ch(03),~
                                                                         ~
            at (06,02), fac(hex(84)),   seqnr$      (top% +  0%), ch(03),~
            at (07,02), fac(hex(84)),   seqnr$      (top% +  1%), ch(03),~
            at (08,02), fac(hex(84)),   seqnr$      (top% +  2%), ch(03),~
            at (09,02), fac(hex(84)),   seqnr$      (top% +  3%), ch(03),~
            at (10,02), fac(hex(84)),   seqnr$      (top% +  4%), ch(03),~
            at (11,02), fac(hex(84)),   seqnr$      (top% +  5%), ch(03),~
            at (12,02), fac(hex(84)),   seqnr$      (top% +  6%), ch(03),~
            at (13,02), fac(hex(84)),   seqnr$      (top% +  7%), ch(03),~
            at (14,02), fac(hex(84)),   seqnr$      (top% +  8%), ch(03),~
            at (15,02), fac(hex(84)),   seqnr$      (top% +  9%), ch(03),~
            at (16,02), fac(hex(84)),   seqnr$      (top% + 10%), ch(03),~
            at (17,02), fac(hex(84)),   seqnr$      (top% + 11%), ch(03),~
            at (18,02), fac(hex(84)),   seqnr$      (top% + 12%), ch(03),~
            at (19,02), fac(hex(84)),   seqnr$      (top% + 13%), ch(03),~
            at (20,02), fac(hex(84)),   seqnr$      (top% + 14%), ch(03),~
                                                                         ~
            at (06,06), fac(hex(84)), pn_or_desc$   (top% +  0%), ch(28),~
            at (07,06), fac(hex(84)), pn_or_desc$   (top% +  1%), ch(28),~
            at (08,06), fac(hex(84)), pn_or_desc$   (top% +  2%), ch(28),~
            at (09,06), fac(hex(84)), pn_or_desc$   (top% +  3%), ch(28),~
            at (10,06), fac(hex(84)), pn_or_desc$   (top% +  4%), ch(28),~
            at (11,06), fac(hex(84)), pn_or_desc$   (top% +  5%), ch(28),~
            at (12,06), fac(hex(84)), pn_or_desc$   (top% +  6%), ch(28),~
            at (13,06), fac(hex(84)), pn_or_desc$   (top% +  7%), ch(28),~
            at (14,06), fac(hex(84)), pn_or_desc$   (top% +  8%), ch(28),~
            at (15,06), fac(hex(84)), pn_or_desc$   (top% +  9%), ch(28),~
            at (16,06), fac(hex(84)), pn_or_desc$   (top% + 10%), ch(28),~
            at (17,06), fac(hex(84)), pn_or_desc$   (top% + 11%), ch(28),~
            at (18,06), fac(hex(84)), pn_or_desc$   (top% + 12%), ch(28),~
            at (19,06), fac(hex(84)), pn_or_desc$   (top% + 13%), ch(28),~
            at (20,06), fac(hex(84)), pn_or_desc$   (top% + 14%), ch(28),~
                                                                         ~
            at (06,35), fac(hex(84)),   disp$       (top% +  0%), ch(03),~
            at (07,35), fac(hex(84)),   disp$       (top% +  1%), ch(03),~
            at (08,35), fac(hex(84)),   disp$       (top% +  2%), ch(03),~
            at (09,35), fac(hex(84)),   disp$       (top% +  3%), ch(03),~
            at (10,35), fac(hex(84)),   disp$       (top% +  4%), ch(03),~
            at (11,35), fac(hex(84)),   disp$       (top% +  5%), ch(03),~
            at (12,35), fac(hex(84)),   disp$       (top% +  6%), ch(03),~
            at (13,35), fac(hex(84)),   disp$       (top% +  7%), ch(03),~
            at (14,35), fac(hex(84)),   disp$       (top% +  8%), ch(03),~
            at (15,35), fac(hex(84)),   disp$       (top% +  9%), ch(03),~
            at (16,35), fac(hex(84)),   disp$       (top% + 10%), ch(03),~
            at (17,35), fac(hex(84)),   disp$       (top% + 11%), ch(03),~
            at (18,35), fac(hex(84)),   disp$       (top% + 12%), ch(03),~
            at (19,35), fac(hex(84)),   disp$       (top% + 13%), ch(03),~
            at (20,35), fac(hex(84)),   disp$       (top% + 14%), ch(03),~
                                                                         ~
            at (06,39), fac(pfac$( 1)), pack_qty$   (top% +  0%), ch(04),~
            at (07,39), fac(pfac$( 2)), pack_qty$   (top% +  1%), ch(04),~
            at (08,39), fac(pfac$( 3)), pack_qty$   (top% +  2%), ch(04),~
            at (09,39), fac(pfac$( 4)), pack_qty$   (top% +  3%), ch(04),~
            at (10,39), fac(pfac$( 5)), pack_qty$   (top% +  4%), ch(04),~
            at (11,39), fac(pfac$( 6)), pack_qty$   (top% +  5%), ch(04),~
            at (12,39), fac(pfac$( 7)), pack_qty$   (top% +  6%), ch(04),~
            at (13,39), fac(pfac$( 8)), pack_qty$   (top% +  7%), ch(04),~
            at (14,39), fac(pfac$( 9)), pack_qty$   (top% +  8%), ch(04),~
            at (15,39), fac(pfac$(10)), pack_qty$   (top% +  9%), ch(04),~
            at (16,39), fac(pfac$(11)), pack_qty$   (top% + 10%), ch(04),~
            at (17,39), fac(pfac$(12)), pack_qty$   (top% + 11%), ch(04),~
            at (18,39), fac(pfac$(13)), pack_qty$   (top% + 12%), ch(04),~
            at (19,39), fac(pfac$(14)), pack_qty$   (top% + 13%), ch(04),~
            at (20,39), fac(pfac$(15)), pack_qty$   (top% + 14%), ch(04),~
                                                                         ~
            at (06,44), fac(pfac$( 1)), pack_type$  (top% +  0%), ch(08),~
            at (07,44), fac(pfac$( 2)), pack_type$  (top% +  1%), ch(08),~
            at (08,44), fac(pfac$( 3)), pack_type$  (top% +  2%), ch(08),~
            at (09,44), fac(pfac$( 4)), pack_type$  (top% +  3%), ch(08),~
            at (10,44), fac(pfac$( 5)), pack_type$  (top% +  4%), ch(08),~
            at (11,44), fac(pfac$( 6)), pack_type$  (top% +  5%), ch(08),~
            at (12,44), fac(pfac$( 7)), pack_type$  (top% +  6%), ch(08),~
            at (13,44), fac(pfac$( 8)), pack_type$  (top% +  7%), ch(08),~
            at (14,44), fac(pfac$( 9)), pack_type$  (top% +  8%), ch(08),~
            at (15,44), fac(pfac$(10)), pack_type$  (top% +  9%), ch(08),~
            at (16,44), fac(pfac$(11)), pack_type$  (top% + 10%), ch(08),~
            at (17,44), fac(pfac$(12)), pack_type$  (top% + 11%), ch(08),~
            at (18,44), fac(pfac$(13)), pack_type$  (top% + 12%), ch(08),~
            at (19,44), fac(pfac$(14)), pack_type$  (top% + 13%), ch(08),~
            at (20,44), fac(pfac$(15)), pack_type$  (top% + 14%), ch(08),~
                                                                         ~
            at (06,53), fac(lfac$( 1)), schld$      (top% +  0%), ch(10),~
            at (07,53), fac(lfac$( 2)), schld$      (top% +  1%), ch(10),~
            at (08,53), fac(lfac$( 3)), schld$      (top% +  2%), ch(10),~
            at (09,53), fac(lfac$( 4)), schld$      (top% +  3%), ch(10),~
            at (10,53), fac(lfac$( 5)), schld$      (top% +  4%), ch(10),~
            at (11,53), fac(lfac$( 6)), schld$      (top% +  5%), ch(10),~
            at (12,53), fac(lfac$( 7)), schld$      (top% +  6%), ch(10),~
            at (13,53), fac(lfac$( 8)), schld$      (top% +  7%), ch(10),~
            at (14,53), fac(lfac$( 9)), schld$      (top% +  8%), ch(10),~
            at (15,53), fac(lfac$(10)), schld$      (top% +  9%), ch(10),~
            at (16,53), fac(lfac$(11)), schld$      (top% + 10%), ch(10),~
            at (17,53), fac(lfac$(12)), schld$      (top% + 11%), ch(10),~
            at (18,53), fac(lfac$(13)), schld$      (top% + 12%), ch(10),~
            at (19,53), fac(lfac$(14)), schld$      (top% + 13%), ch(10),~
            at (20,53), fac(lfac$(15)), schld$      (top% + 14%), ch(10),~
                                                                         ~
            at (06,64), fac(lfax$( 1)), str(lot$(top% +  0%, 1),1,lot%) ,~
            at (07,64), fac(lfax$( 2)), str(lot$(top% +  1%, 1),1,lot%) ,~
            at (08,64), fac(lfax$( 3)), str(lot$(top% +  2%, 1),1,lot%) ,~
            at (09,64), fac(lfax$( 4)), str(lot$(top% +  3%, 1),1,lot%) ,~
            at (10,64), fac(lfax$( 5)), str(lot$(top% +  4%, 1),1,lot%) ,~
            at (11,64), fac(lfax$( 6)), str(lot$(top% +  5%, 1),1,lot%) ,~
            at (12,64), fac(lfax$( 7)), str(lot$(top% +  6%, 1),1,lot%) ,~
            at (13,64), fac(lfax$( 8)), str(lot$(top% +  7%, 1),1,lot%) ,~
            at (14,64), fac(lfax$( 9)), str(lot$(top% +  8%, 1),1,lot%) ,~
            at (15,64), fac(lfax$(10)), str(lot$(top% +  9%, 1),1,lot%) ,~
            at (16,64), fac(lfax$(11)), str(lot$(top% + 10%, 1),1,lot%) ,~
            at (17,64), fac(lfax$(12)), str(lot$(top% + 11%, 1),1,lot%) ,~
            at (18,64), fac(lfax$(13)), str(lot$(top% + 12%, 1),1,lot%) ,~
            at (19,64), fac(lfax$(14)), str(lot$(top% + 13%, 1),1,lot%) ,~
            at (20,64), fac(lfax$(15)), str(lot$(top% + 14%, 1),1,lot%) ,~
                                                                         ~
            at (06,71), fac(lfax$( 1)), lotqty1$    (top% +  0%), ch(10),~
            at (07,71), fac(lfax$( 2)), lotqty1$    (top% +  1%), ch(10),~
            at (08,71), fac(lfax$( 3)), lotqty1$    (top% +  2%), ch(10),~
            at (09,71), fac(lfax$( 4)), lotqty1$    (top% +  3%), ch(10),~
            at (10,71), fac(lfax$( 5)), lotqty1$    (top% +  4%), ch(10),~
            at (11,71), fac(lfax$( 6)), lotqty1$    (top% +  5%), ch(10),~
            at (12,71), fac(lfax$( 7)), lotqty1$    (top% +  6%), ch(10),~
            at (13,71), fac(lfax$( 8)), lotqty1$    (top% +  7%), ch(10),~
            at (14,71), fac(lfax$( 9)), lotqty1$    (top% +  8%), ch(10),~
            at (15,71), fac(lfax$(10)), lotqty1$    (top% +  9%), ch(10),~
            at (16,71), fac(lfax$(11)), lotqty1$    (top% + 10%), ch(10),~
            at (17,71), fac(lfax$(12)), lotqty1$    (top% + 11%), ch(10),~
            at (18,71), fac(lfax$(13)), lotqty1$    (top% + 12%), ch(10),~
            at (19,71), fac(lfax$(14)), lotqty1$    (top% + 13%), ch(10),~
            at (20,71), fac(lfax$(15)), lotqty1$    (top% + 14%), ch(10),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)),   pf$(1%)                 , ch(79),~
            at (23,02), fac(hex(8c)),   pf$(2%)                 , ch(79),~
            at (24,02), fac(hex(8c)),   pf$(3%)                 , ch(79),~
                keys(pfkey$), key(keyhit%)

            if keyhit% <> 13% then L42760
                call "MANUAL" ("SHPACTIN")
                goto L41210

L42760:     if keyhit% <> 15% then L42800
               call "PRNTSCRN"
               goto L41210

L42800:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

            if keyhit% <> 8% then L42820
                gosub locations
               goto L41210

L42820:     if keyhit% <> 14% then L42830
                idx% = cursor%(1%) - 6% + top%           /* PAR000 */
                call "INVQDISP" (epart$(idx%), #12, #18, #18, #17)
                goto L41210

L42830:     return

        setpf2
        if edit% > 1% then L43000         /* Display Mode               */
           pf$(1%)= "(1)Strt Ovr  (4)Prev (7)Up       (10)TCS/Uncmtd (1"&~
                    "2)Ship Complete (13)Instr    "
           pf$(2%)= "(2)First     (5)Next (8)Location (11)PN/Descr   (2"&~
                    "2)Flag Complete (15)Prnt Scrn"
           pf$(3%)= "(3)Last      (6)Down (9)Hdr Scrn (14)StrLot Qty (2"&~
                    "6/28)SO/Inv Txt (16)Save Data"
           pfkey$ = hex(0102030405060708090a0b0c0d0e0f10161a1c1d00)
           if admin% <> 1% then str(pfkey$,20%,1%) = hex(ff)
           if top% > 1% then L42950
              str(pf$(2),1,13),str(pf$(1),14,8),str(pf$(3),14,8) = " "
              str(pfkey$,2,1), str(pfkey$,4,1), str(pfkey$,6,1) = hex(ff)
L42950:    if top% + 14% < maxlines% then L42970
              str(pf$(3),1,13),str(pf$(2),14,8),str(pf$(1),22,12) = " "
              str(pfkey$,3,1), str(pfkey$,5,1), str(pfkey$,7,1) = hex(ff)
L42970:    if bolnew% = 1% then L42982
              str(pf$(1%),49%,17%) = " " : str(pfkey$,12%,1%) = hex(ff)
L42982:    return
                                         /* Edit Mode- Field Enabled   */
L43000:    pf$(1%)= "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2%)= "                                                  "&~
                    "             (15)Print Screen"
           pf$(3%)= "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00ffff)
           return

        REM *************************************************************~
            *               S C R E E N   P A G E   2B                  *~
            * --------------------------------------------------------- *~
            * Document Input and Edit Screen.  Can see Uncommited here. *~
            *************************************************************

        deffn'103(c%, fieldnr%, edit%)
            line2$ = "Customer: " & cuscode$ & "  BOL: " & so$ & "-" &   ~
                     bol$ & "  Store: " & store$
            str(line2$, 62) = "APCINVBO: " & "R7.00.00"     /* (EWD) */
            gosub setpf2
            if str(errormsg$,,7) = "WARNING" or edit% = 3% then L44130
            if errormsg$ <> " " then L44220
L44130:         init(hex(8c)) lfac$(), lfax$(), pfac$()
                if edit% >= 2% then L44180
                     init(hex(86)) lfac$()
                     init(hex(84)) lfax$(), pfac$()
                     goto L44220
L44180:         init(hex(81)) lfac$(fieldnr%)
                if package_enabled%= 2% then init(hex(81)) pfac$(fieldnr%)
                call "LOTENABL" (part$(c%), lot_enable%, lot%, #17, #12)
                if lot_enable% > 0% then init(hex(81)) lfax$(fieldnr%)

                                                 /* (EWD) Next 8 Lines */
L44220: accept                                                           ~
            at (01,02), "Shipment Pre-Invoicing",                        ~
            at (01,64), "Today:",                                        ~
            at (01,71), fac(hex(8c)),  fdate$                   , ch(10),~
            at (02,51), "Invoicing Session Id:",                         ~
            at (02,73), fac(hex(84)),  session$                 , ch(06),~
            at (03,02), fac(hex(ac)),  line2$                   , ch(79),~
            at (04,02), fac(hex(94)),  errormsg$                , ch(79),~
                                                                         ~
            at (05,02), fac(hex(ac)), hdr1$                     , ch(03),~
            at (05,06), fac(hex(ac)), hdr10$                    , ch(21),~
            at (05,28), fac(hex(ac)), hdr5$                     , ch(10),~
            at (05,39), fac(hex(ac)), hdr8$                     , ch(04),~
            at (05,44), fac(hex(ac)), hdr9$                     , ch(08),~
            at (05,53), fac(hex(ac)), hdr6$                     , ch(10),~
            at (05,64), fac(hex(ac)), hdr7$                     , ch(17),~
            at (06,02), fac(hex(80)),   seqnr$      (top% +  0%), ch(03),~
                                                                         ~
            at (06,02), fac(hex(84)),   seqnr$      (top% +  0%), ch(03),~
            at (07,02), fac(hex(84)),   seqnr$      (top% +  1%), ch(03),~
            at (08,02), fac(hex(84)),   seqnr$      (top% +  2%), ch(03),~
            at (09,02), fac(hex(84)),   seqnr$      (top% +  3%), ch(03),~
            at (10,02), fac(hex(84)),   seqnr$      (top% +  4%), ch(03),~
            at (11,02), fac(hex(84)),   seqnr$      (top% +  5%), ch(03),~
            at (12,02), fac(hex(84)),   seqnr$      (top% +  6%), ch(03),~
            at (13,02), fac(hex(84)),   seqnr$      (top% +  7%), ch(03),~
            at (14,02), fac(hex(84)),   seqnr$      (top% +  8%), ch(03),~
            at (15,02), fac(hex(84)),   seqnr$      (top% +  9%), ch(03),~
            at (16,02), fac(hex(84)),   seqnr$      (top% + 10%), ch(03),~
            at (17,02), fac(hex(84)),   seqnr$      (top% + 11%), ch(03),~
            at (18,02), fac(hex(84)),   seqnr$      (top% + 12%), ch(03),~
            at (19,02), fac(hex(84)),   seqnr$      (top% + 13%), ch(03),~
            at (20,02), fac(hex(84)),   seqnr$      (top% + 14%), ch(03),~
                                                                         ~
            at (06,06), fac(hex(84)), pn_or_desc$   (top% +  0%), ch(25),~
            at (07,06), fac(hex(84)), pn_or_desc$   (top% +  1%), ch(25),~
            at (08,06), fac(hex(84)), pn_or_desc$   (top% +  2%), ch(25),~
            at (09,06), fac(hex(84)), pn_or_desc$   (top% +  3%), ch(25),~
            at (10,06), fac(hex(84)), pn_or_desc$   (top% +  4%), ch(25),~
            at (11,06), fac(hex(84)), pn_or_desc$   (top% +  5%), ch(25),~
            at (12,06), fac(hex(84)), pn_or_desc$   (top% +  6%), ch(25),~
            at (13,06), fac(hex(84)), pn_or_desc$   (top% +  7%), ch(25),~
            at (14,06), fac(hex(84)), pn_or_desc$   (top% +  8%), ch(25),~
            at (15,06), fac(hex(84)), pn_or_desc$   (top% +  9%), ch(25),~
            at (16,06), fac(hex(84)), pn_or_desc$   (top% + 10%), ch(25),~
            at (17,06), fac(hex(84)), pn_or_desc$   (top% + 11%), ch(25),~
            at (18,06), fac(hex(84)), pn_or_desc$   (top% + 12%), ch(25),~
            at (19,06), fac(hex(84)), pn_or_desc$   (top% + 13%), ch(25),~
            at (20,06), fac(hex(84)), pn_or_desc$   (top% + 14%), ch(25),~
                                                                         ~
            at (06,28), fac(hex(84)),   avail$      (top% +  0%), ch(10),~
            at (07,28), fac(hex(84)),   avail$      (top% +  1%), ch(10),~
            at (08,28), fac(hex(84)),   avail$      (top% +  2%), ch(10),~
            at (09,28), fac(hex(84)),   avail$      (top% +  3%), ch(10),~
            at (10,28), fac(hex(84)),   avail$      (top% +  4%), ch(10),~
            at (11,28), fac(hex(84)),   avail$      (top% +  5%), ch(10),~
            at (12,28), fac(hex(84)),   avail$      (top% +  6%), ch(10),~
            at (13,28), fac(hex(84)),   avail$      (top% +  7%), ch(10),~
            at (14,28), fac(hex(84)),   avail$      (top% +  8%), ch(10),~
            at (15,28), fac(hex(84)),   avail$      (top% +  9%), ch(10),~
            at (16,28), fac(hex(84)),   avail$      (top% + 10%), ch(10),~
            at (17,28), fac(hex(84)),   avail$      (top% + 11%), ch(10),~
            at (18,28), fac(hex(84)),   avail$      (top% + 12%), ch(10),~
            at (19,28), fac(hex(84)),   avail$      (top% + 13%), ch(10),~
            at (20,28), fac(hex(84)),   avail$      (top% + 14%), ch(10),~
                                                                         ~
            at (06,39), fac(pfac$( 1)), pack_qty$   (top% +  0%), ch(04),~
            at (07,39), fac(pfac$( 2)), pack_qty$   (top% +  1%), ch(04),~
            at (08,39), fac(pfac$( 3)), pack_qty$   (top% +  2%), ch(04),~
            at (09,39), fac(pfac$( 4)), pack_qty$   (top% +  3%), ch(04),~
            at (10,39), fac(pfac$( 5)), pack_qty$   (top% +  4%), ch(04),~
            at (11,39), fac(pfac$( 6)), pack_qty$   (top% +  5%), ch(04),~
            at (12,39), fac(pfac$( 7)), pack_qty$   (top% +  6%), ch(04),~
            at (13,39), fac(pfac$( 8)), pack_qty$   (top% +  7%), ch(04),~
            at (14,39), fac(pfac$( 9)), pack_qty$   (top% +  8%), ch(04),~
            at (15,39), fac(pfac$(10)), pack_qty$   (top% +  9%), ch(04),~
            at (16,39), fac(pfac$(11)), pack_qty$   (top% + 10%), ch(04),~
            at (17,39), fac(pfac$(12)), pack_qty$   (top% + 11%), ch(04),~
            at (18,39), fac(pfac$(13)), pack_qty$   (top% + 12%), ch(04),~
            at (19,39), fac(pfac$(14)), pack_qty$   (top% + 13%), ch(04),~
            at (20,39), fac(pfac$(15)), pack_qty$   (top% + 14%), ch(04),~
                                                                         ~
            at (06,44), fac(pfac$( 1)), pack_type$  (top% +  0%), ch(08),~
            at (07,44), fac(pfac$( 2)), pack_type$  (top% +  1%), ch(08),~
            at (08,44), fac(pfac$( 3)), pack_type$  (top% +  2%), ch(08),~
            at (09,44), fac(pfac$( 4)), pack_type$  (top% +  3%), ch(08),~
            at (10,44), fac(pfac$( 5)), pack_type$  (top% +  4%), ch(08),~
            at (11,44), fac(pfac$( 6)), pack_type$  (top% +  5%), ch(08),~
            at (12,44), fac(pfac$( 7)), pack_type$  (top% +  6%), ch(08),~
            at (13,44), fac(pfac$( 8)), pack_type$  (top% +  7%), ch(08),~
            at (14,44), fac(pfac$( 9)), pack_type$  (top% +  8%), ch(08),~
            at (15,44), fac(pfac$(10)), pack_type$  (top% +  9%), ch(08),~
            at (16,44), fac(pfac$(11)), pack_type$  (top% + 10%), ch(08),~
            at (17,44), fac(pfac$(12)), pack_type$  (top% + 11%), ch(08),~
            at (18,44), fac(pfac$(13)), pack_type$  (top% + 12%), ch(08),~
            at (19,44), fac(pfac$(14)), pack_type$  (top% + 13%), ch(08),~
            at (20,44), fac(pfac$(15)), pack_type$  (top% + 14%), ch(08),~
                                                                         ~
            at (06,53), fac(lfac$( 1)), schld$      (top% +  0%), ch(10),~
            at (07,53), fac(lfac$( 2)), schld$      (top% +  1%), ch(10),~
            at (08,53), fac(lfac$( 3)), schld$      (top% +  2%), ch(10),~
            at (09,53), fac(lfac$( 4)), schld$      (top% +  3%), ch(10),~
            at (10,53), fac(lfac$( 5)), schld$      (top% +  4%), ch(10),~
            at (11,53), fac(lfac$( 6)), schld$      (top% +  5%), ch(10),~
            at (12,53), fac(lfac$( 7)), schld$      (top% +  6%), ch(10),~
            at (13,53), fac(lfac$( 8)), schld$      (top% +  7%), ch(10),~
            at (14,53), fac(lfac$( 9)), schld$      (top% +  8%), ch(10),~
            at (15,53), fac(lfac$(10)), schld$      (top% +  9%), ch(10),~
            at (16,53), fac(lfac$(11)), schld$      (top% + 10%), ch(10),~
            at (17,53), fac(lfac$(12)), schld$      (top% + 11%), ch(10),~
            at (18,53), fac(lfac$(13)), schld$      (top% + 12%), ch(10),~
            at (19,53), fac(lfac$(14)), schld$      (top% + 13%), ch(10),~
            at (20,53), fac(lfac$(15)), schld$      (top% + 14%), ch(10),~
                                                                         ~
            at (06,64), fac(lfax$( 1)), str(lot$(top% +  0%, 1),1,lot%) ,~
            at (07,64), fac(lfax$( 2)), str(lot$(top% +  1%, 1),1,lot%) ,~
            at (08,64), fac(lfax$( 3)), str(lot$(top% +  2%, 1),1,lot%) ,~
            at (09,64), fac(lfax$( 4)), str(lot$(top% +  3%, 1),1,lot%) ,~
            at (10,64), fac(lfax$( 5)), str(lot$(top% +  4%, 1),1,lot%) ,~
            at (11,64), fac(lfax$( 6)), str(lot$(top% +  5%, 1),1,lot%) ,~
            at (12,64), fac(lfax$( 7)), str(lot$(top% +  6%, 1),1,lot%) ,~
            at (13,64), fac(lfax$( 8)), str(lot$(top% +  7%, 1),1,lot%) ,~
            at (14,64), fac(lfax$( 9)), str(lot$(top% +  8%, 1),1,lot%) ,~
            at (15,64), fac(lfax$(10)), str(lot$(top% +  9%, 1),1,lot%) ,~
            at (16,64), fac(lfax$(11)), str(lot$(top% + 10%, 1),1,lot%) ,~
            at (17,64), fac(lfax$(12)), str(lot$(top% + 11%, 1),1,lot%) ,~
            at (18,64), fac(lfax$(13)), str(lot$(top% + 12%, 1),1,lot%) ,~
            at (19,64), fac(lfax$(14)), str(lot$(top% + 13%, 1),1,lot%) ,~
            at (20,64), fac(lfax$(15)), str(lot$(top% + 14%, 1),1,lot%) ,~
                                                                         ~
            at (06,71), fac(lfax$( 1)), lotqty1$    (top% +  0%), ch(10),~
            at (07,71), fac(lfax$( 2)), lotqty1$    (top% +  1%), ch(10),~
            at (08,71), fac(lfax$( 3)), lotqty1$    (top% +  2%), ch(10),~
            at (09,71), fac(lfax$( 4)), lotqty1$    (top% +  3%), ch(10),~
            at (10,71), fac(lfax$( 5)), lotqty1$    (top% +  4%), ch(10),~
            at (11,71), fac(lfax$( 6)), lotqty1$    (top% +  5%), ch(10),~
            at (12,71), fac(lfax$( 7)), lotqty1$    (top% +  6%), ch(10),~
            at (13,71), fac(lfax$( 8)), lotqty1$    (top% +  7%), ch(10),~
            at (14,71), fac(lfax$( 9)), lotqty1$    (top% +  8%), ch(10),~
            at (15,71), fac(lfax$(10)), lotqty1$    (top% +  9%), ch(10),~
            at (16,71), fac(lfax$(11)), lotqty1$    (top% + 10%), ch(10),~
            at (17,71), fac(lfax$(12)), lotqty1$    (top% + 11%), ch(10),~
            at (18,71), fac(lfax$(13)), lotqty1$    (top% + 12%), ch(10),~
            at (19,71), fac(lfax$(14)), lotqty1$    (top% + 13%), ch(10),~
            at (20,71), fac(lfax$(15)), lotqty1$    (top% + 14%), ch(10),~
                                                                         ~
            at (21,02), fac(hex(a4)),   inpmessage$             , ch(79),~
            at (22,02), fac(hex(8c)),   pf$(1%)                 , ch(79),~
            at (23,02), fac(hex(8c)),   pf$(2%)                 , ch(79),~
            at (24,02), fac(hex(8c)),   pf$(3%)                 , ch(79),~
                keys(pfkey$), key(keyhit%)


            if keyhit% <> 13% then L45770
                call "MANUAL" ("SHPACTIN")
                goto L44220

L45770:     if keyhit% <> 15% then L45810
               call "PRNTSCRN"
               goto L44220

L45810:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

            if keyhit% <> 8% then L45830
               gosub locations
               goto L44220

L45830:     if keyhit% <> 14% then L45840
                idx% = cursor%(1%) - 6% + top%           /*PAR000*/
                call "INVQDISP" (epart$(idx%), #12, #18, #18, #17)
                goto L44220

L45840:     return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

            deffn'151(fieldnr%)
                  errormsg$ = " "
                  on fieldnr% gosub L50105,         /* Sales Order - BOL*/~
                                    L50195          /* All the rest     */
                  return

L50105
*        Sales Order - BOL                     SO$
            if so$ <> " " then L50125
                errormsg$ = "Sales Order may not be left blank."
                return
L50125:     gosub load_data  :  if errormsg$ <> " " then return
            gosub get_load                          /* (EWD)          */
            if maxlines% > 0% then L50161
                                                /* (EWD002) Disable   */
                errormsg$ = "(Error) No Open Line Items"
                return
                                                /* (EWD002) Exit      */
                u3% = 2%
                call "ASKUSER" (2%, "NO OPEN LINES",                     ~
                     "There are no Open Line Items on this Sales Order.",~
                     "Press any PF Key to Continue...", " ")
                goto startover2
L50161:     if posting_window_ok% = 0% then posting_window_error
            if bolnew% = 1% or errormsg$ <> " " or shipped% = 0%         ~
                                                             then return
         REM    return clear all                /* (EWD002) - Disabel */  
                printbol$ = "Y"
         return                                 /* (EWD002) - Mod     */
         REM    goto edit_header                /* (EWD002) - Disable */

L50195
*        Scheduled Ship Date                   SHIPDATE$
            call "DATEOK" (shipdate$, u3%, errormsg$)
            if errormsg$ <> " " then return
                call "DATUNFMT" (shipdate$)
                call "WHICHMON" (#17, shipdate$, month%)
                if month% <> 0% then L50240
                     errormsg$ = "Date is not open for postings."
                     call "DATEFMT" (shipdate$)
                     return
L50240:         call "DATEFMT" (shipdate$)

*        Carrier                               CARRIER$
            readkey$ = "CARRIERS " & carrier$
            carriername$ = hex(06) & "Select Carrier. PF-16 if none."
            call "PLOWCODE" (#3, readkey$, carriername$, 9%, 0.30, f1%(3))
            if f1%(3) = 1% then L50285
                carrier$, carriername$ = " " : goto L50300
L50285:     carrier$ = str(readkey$,10)

L50300
*        How Ship                              HOWSHIP$

*        FOB                                   FOB$

*        Shipping Instructions                 INSTR$()

*        Print BOL Flag                        PRINTBOL$
            if pos("YNI" = printbol$) <> 0% then L50370
                errormsg$ = "Enter 'Y'es, 'I'mmediate, or 'N'o  for " &  ~
                            "Printing BOL."
                return

L50370
*        Freight/Air Bill Number               FRTBILL$

*        Cartons Shipped                       CARTONS$
            errhdr$ = "Cartons Shipped:"
            if cartons$ = " " then cartons = 0
            if cartons$ = " " then L50440
              convert cartons$ to cartons, data goto L50405 : goto L50415
L50405:         errormsg$ = errhdr$ & " Invalid numeric entry."   : return
L50410:         errormsg$ = errhdr$ & " Entry cannot be negative.": return
L50415:       if cartons < 0 then L50410
                call "CONVERT" (cartons, 2.2, cartons$)
                if cartons = 0 then cartons$ = " "

L50440
*        Shipment Weight                       WEIGHT$
            errhdr$ = "Shipment Weight:"
            if weight$ = " " then weight = 0
            if weight$ = " " then L50485
                convert weight$ to weight, data goto L50405
                if weight < 0 then L50410
                     call "CONVERT" (weight, 2.2, weight$)
                     if weight = 0 then weight$ = " "

L50485
*        Freight Charges                       FRTAMT$
            errhdr$ = "Freight Charges:"
            if frtamt$ = " " then frtamt$ = "0"
            convert frtamt$ to frtamt, data goto L50405
            call "CONVERT" (frtamt, 2.2, frtamt$)
            return
                                                /* (EWD) - Begin      */
        get_load
            init(" ") load$, or_key$
            str(or_key$,1%,8%)  = str(so$,1%,8%)
            read #10,key 4% = or_key$, using L50500, load$,             ~
                                                 eod goto get_load_done
L50500:      FMT POS(94), CH(5)
        get_load_done
        return

        check_status                            /* (EWD002) Status Chk*/
             or_status% = 0%                    /* (APCPLNOR) - File  */
             init(" ") or_key$, or_status$, errormsg$
             str(or_key$,1%,8%) = str(so$,1%,8%)
             read #10,key 4% = or_key$, using L50505, or_status$,       ~
                                              eod goto check_status_done
L50505:         FMT POS(60), CH(2)
             convert or_status$ to or_status%, data goto L50510
L50510
        check_status_done
             if or_status% < 18% then return
                errormsg$ = "(Error)-BOL already Exists for ("&so$&")?"
                gosub skip_message              /* (EWD004)           */ 
        return 

        update_status                           /* (EWD002) - Update  */
             init(" ") or_key$, or_rec$         /* (APCPLNOR) - File  */
             str(or_key$,1%,8%) = str(so$,1%,8%)
             read #10,hold,key 4% = or_key$, using L50520, or_rec$,     ~
                                             eod goto update_status_done
L50520:         FMT CH(170)   
              str(or_rec$,60%,2%)  = "18"
              str(or_rec$,62%,8%)  = date
              str(or_rec$,146%,8%) = date
              put #10, using L50520, or_rec$
              rewrite #10
        update_status_done
        return
                                                /* (EWD002) - End     */


        lookup_arrival                          /* (AWD009) - Update  */
             init(" ") app_key$, app_dte$       /* (AWDAPPOR) - File  */
             init(" ") app_drop$
             str(app_key$,1%,8%) = str(so$,1%,8%)
             read #51,hold,key = app_key$, using L50560, app_drop$,   ~
                                     app_dte$, eod goto no_app_order
L50560:         FMT POS(22), CH(02), POS(49), CH(06)   

                    if app_drop$ = "99" then goto no_app_order
        
        return
        no_app_order
             app_dte$ = date
        return
                                                /* (AWD009) - End     */


        posting_window_error
            /* We got here because the BOL called up has a ship date
               outside our posting window.  Deletions or modifications
               can't post inventory movement or the GL correctly.  */
            u3% = 2%
            call "ASKUSER" (u3%, "*** POSTING WINDOW ERROR ***",         ~
                "The BOL's Date Shipped (Post Date) is no longer within"&~
                " your Posting Window.", "To post 'as is', Module Dates"&~
                " and Current Period Open need adjusting.", "Press PF 1"&~
                " to Startover -or- Press RETURN to modify Date Shipped.")
            if u3% =  1% then L29500
            if u3% <> 0% then posting_window_error
            goto edit_header_fs

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            * --------------------------------------------------------- *~
            * Test data for a line item (Qty Shipped and Lots).         *~
            *************************************************************

        deffn'152(c%)

*        Scheduled Quantities, Lots    SCHLD$(), LOT$(), LOTQTY1$()
*        Next  Check Out the Shipped Quantity
            errormsg$ = " "
            if schld$(c%) = " " then schld$(c%) = "0"
            convert schld$(c%) to schld(c%), data goto L51080 : goto L51090
L51080:         errormsg$ = "Invalid numeric entry for Quantity Shipped."
                return
L51090:     call "CONVERT" (schld(c%), 2.2, schld$(c%))
            if schld(c%) >= 0 then L51097
                errormsg$ = "Quantity Shipped Can't be less than Zero."
                return
L51097:     if schld(c%) <= avail(c%) then L51130
               avail(c%) = schld(c%) : goto L51130 /* (EWD) - 04/06/98 */
                                                   /* 2nd BOL Cut      */
            if overship$  = "Y" then goto L51115
                errormsg$ = "Quantity Shipped must be between Zero and" &~
                            " Quantity Uncommitted."
                return
L51115:   /* If Overshipping allowed check out the qty & restrictions*/
            if edit% <> 2% then L51130
            tot_ship = committed(c%) + schld(c%)
L51119:     call "SHPOVRSB" (part$(c%), seqnr$(c%), order(c%), tot_ship, ~
               def_percent(c%), def_unit(c%), sys_def_percent, u3%)
            if u3% = 16% then L51130
            if u3% <> 1% then L51119
                errormsg$ = "Please reenter ship quantity."
                return
L51130:   /* If Shipped = 0 then clear lot and SN info and return   */
            if schld(c%) <> 0 then L51155
                lc% = 1% : lotqty1$(c%) = " "
                lot1was$(c%) = all(hex(00))
                goto clear_lot_info

L51155
*        Check out Lot Quantity (must be > 0 and <= Qty Shipped)
            if lot_enable% > 0%  then L51170
                  lot$(c%, 1)  = " " : lotqty1$(c%) = schld$(c%)
L51170:     if lotqty1$(c%) = " " then lotqty1$(c%) = schld$(c%)
            convert lotqty1$(c%) to lotqtys(c%,1%), data goto L51185
            goto L51195
L51185:         errormsg$ = "Invalid numeric entry for Lot Quantity."
                return
L51195:     if lotqtys(c%,1%)>0 and lotqtys(c%,1)<=schld(c%) then L51215
                errormsg$ = "Lot Quantity must be greater than Zero" &   ~
                            " and less than or equal to Qty Shipped."
                return
L51215:     if lot_enable% < 2% or lot$(c%,1) <> " " then L51235
                errormsg$ = "Lot Number is required for this part."
                return

L51235: /* Test First Lot number.                                      */
            call "SERENABL" (part$(c%), sn_enable%, k%, #17, #12)
            if lot$(c%, 1%) =  " " and lot_enable% < 2% then L51295
* PAR000
                readkey$ = str(epart$(c%)) & str(store$) & lot$(c%,1%)
                call "READ100" (#18, readkey$, f1%(18))
                if f1%(18) = 1% then L51295
                    errormsg$ = "WARNING: Lot Number " & lot$(c%,1%) &   ~
                                " not on file for Part " & epart$(c%)
                    if lot_enable% = 2% then                             ~
                      errormsg$ = "Invalid Lot Number.  Please re-enter."~
                                        else L51295
                    return

L51295:     k% = 1% : gosub test_quantity
                      if errormsg$ <> " " then exit_test

*        If the first lot number has changed we need to clear and get
*        new serial numbers- the LOTDISN sub won't help us here.
            if str(lot1was$(c%),,1) = hex(00) then L51395
            if lot1was$(c%) = lot$(c%,1) or sn_enable% = 0% then L51395
                if sn_index2%(c%,1) <> 0% then                           ~
                   call "SERSTOVR" (sn_index2%(c%,1), "4", "2", #32, #33)
                if bol$ = "NEW" then gosub next_bol
                if sn_index2%(c%,1) <> 0% then L51355
                     sn_index2%(c%,1), sn_index% = sn_index% + 1%
L51355:         sn_loc$ = str(store$) & lot$(c%,1%)
                sn_trankey$ = str(so$) & str(bol$) & str(seqnr$(c%)) &   ~
                                                                     "01"
                call "SERSELCT" (part$(c%), sn_loc$, lotqtys(c%,1%),     ~
                                 sn_index2%(c%,1), 25%, "RS",            ~
                                 sn_trankey$, "4", "2", errormsg$,       ~
                                 #17, #12, #32, #33)

L51395:     if lotqtys(c%,1%) <> 0 and lotqtys(c%,1%) <> schld(c%)       ~
                                                       then invoke_distr

*        Distribution to a single lot.  Get Serial Numbers.
            lc% = 2% : gosub clear_lot_info
            if sn_enable% = 0% then exit_test
                if bol$ = "NEW" then gosub next_bol
                if sn_index2%(c%,1) <> 0% then L51440
                     sn_index2%(c%,1), sn_index% = sn_index% + 1%
L51440:         sn_loc$ = str(store$) & lot$(c%,1%)
                sn_trankey$ = str(so$) & str(bol$) & str(seqnr$(c%)) &   ~
                                                                     "01"
                call "SERSELCT" (part$(c%), sn_loc$, schld(c%),          ~
                                 sn_index2%(c%,1), 25%, "RS",            ~
                                 sn_trankey$, "4", "2", errormsg$,       ~
                                 #17, #12, #32, #33)
            goto exit_test

        invoke_distr    /* QTY <> SHIPPED   */
            u3% = 0% : newlotqty$(),newlot$() = " " : mat newlotqty = zer
            for i% = 1% to 30%
                if lotqtys(c%,i%) = 0 then L52000
                newlot$  (i%)  = lot$  (c%, i%)
                newlotqty(i%)  = lotqtys(c%, i%)
                sn_index1%(i%) = sn_index2%(c%, i%)
                call "CONVERT" (newlotqty(i%), 0.2, newlotqty$(i%))
                u3% = i%
            next i%
L52000:     strs$(1) = hex(ff)  :  strs$(2) = store$
            if bol$  = "NEW" and sn_enable% > 0% then gosub next_bol
            sn_trankey$ = str(so$) & str(bol$) & str(seqnr$(c%)) & "??"
            call "LOTDISN"  (part$(c%), strs$(), newlot$(), newlotqty$(),~
                             errormsg$, 1%, u3%, 30%, schld(c%),         ~
                             sn_index1%(), " ", 0, c%, 25%, "RS",        ~
                             sn_trankey$, " ", "2", "4",                 ~
                             #12, #18, #31, #17, #32, #34, #33)
            for i% = 1% to 30%
                lot$(c%, i%) = newlot$(i%)
                if newlotqty$(i%) = " " then newlotqty(i%) = 0 else      ~
                                 convert newlotqty$(i%) to newlotqty(i%)
                lotqtys   (c%,i%) = newlotqty (i%)
                sn_index2%(c%,i%) = sn_index1%(i%)
            next i%

            if errormsg$ <> " " then exit_test

            for k% = 1% to 30%
                if lotqtys(c%,k%) = 0 then L52250
                gosub test_quantity
                if errormsg$ = " " then L52250
                   errormsg$ = "Distribution Conflict - Lot:"
                   errormsg$ = errormsg$ & " " & lot$(c%,k%)
                     goto exit_test
L52250:     next k%
            goto exit_test


        exit_test    /* Set LOTQTY1$ variable      */
            gosub package_stuff_test
            call "CONVERT" (lotqtys(c%,1%), 2.2, lotqty1$(c%))
            lot1was$(c%) = lot$(c%,1)
            return


        clear_lot_info
            if lc% = 2% then goto L52390
            if pack_qty$(c%) = " " and pack_type$(c%) = " " then L52390
                  errormsg$ = "Pack Information invalid if zero shipped."
                  return
L52390:     for i% = lc% to 30%
                lot$   (c%, i%) = " "
                lotqtys(c%, i%) = 0
                if sn_index2%(c%, i%) = 0% then L52450
                     call "SERSTOVR" (sn_index2%(c%,i%), "4", "2",       ~
                                                               #32, #33)
L52450:     next i%
            return


        test_quantity
            errormsg_temp$ = errormsg$  /* Hold onto Lot Warning, JIC */
* PAR000
            call "INVAVAIL" (#12, #18, epart$(c%), store$, lot$(c%,k%),   ~
                                          errormsg$, 9e9, temp1, return%)
            if errormsg$ = " " then errormsg$ = errormsg_temp$
            errormsg_temp$ = " "
            if errormsg$ = " " then return
            temp = 0
            for i% = 1% to maxlines%
* PAR000
                if epart$(i%) <> epart$(c%) then L52620
                   for j% = 1% to 30%
                       if lot$(i%,j%) <> lot$(c%,k%) then L52590
                          temp = temp + lotqtys(i%,j%)
                       if shipped% = 0% then L52610
L52590:                if holdlot$(i%,j%) <> lot$(c%,k%) then L52610
                          temp = temp - holdqty(i%,j%)
L52610:            next j%
L52620:     next i%
            if temp <= 0     then errormsg$ = " "
            if temp <= temp1 then errormsg$ = " "
            return

        package_stuff_test
*        Package Quantities                    PACK_QTY$
                if pack_qty$(c%) = " " then pack_qty$(c%) = "0"
                convert pack_qty$(c%) to tempqty, data goto L52750
                if tempqty >= 0 then L52780
                     errormsg$ = "Packing Quantity cannot be negative."
                     return

L52750:              errormsg$ = "Invalid Numeric Entry for Package " &  ~
                                 "Quantity."
                     return
L52780:         call "CONVERT" (tempqty, 0.0, pack_qty$(c%))
                if pack_qty$(c%) = "   0" then pack_qty$(c%) = " "

*        Package Types                         PACK_TYPE$
           if pack_type$(c%) = " " then L53030
                readkey$ = "PACKUNIT " & pack_type$(c%)
                call "PLOWCODE" (#3, readkey$, paktypname$, 9%, .3,      ~
                     f1%(3))
                if f1%(3) = 0% then return
                     pack_type$(c%) = str(readkey$,10)
L53030:     return
 
                                                    /* (EWD) - Begin   */
        REM *************************************************************~
            *          S P E C I A L   S U B R O U T I O N S            *~
            *---------------------------------------------------------- *

        create_invoice
           if str(cuscode$,1%,6%) = "CV0999" then return /* (AWD013) */
           if str(cuscode$,1%,6%) = "AD0999" then return /* (CR9999) */
           if str(cuscode$,1%,6%) = "CT0999" then return /* (CR2868) */
           if str(cuscode$,1%,6%) = "ST0001" then return /* (CR2239) */
           if str(so$,1%,1%) = "D" then return           /* (CR1490) */
           gosub calculate_day                         /*  (AWD009)  */
           
/*CR1098*/ init(" ") table$, genkey$, codeLen$, descr1$, descr2$, descr3$
           generr% = 0%
           table$ = "SESSION"
           genkey$ = so$
           call "GENREAD" (table$, genkey$, descr1$, codeLen$, descr2$,  ~
                           descr3$, generr%)
           if generr% = 0% then session$ = str(descr1$,01%,06%)
/*CR1098*/           
        call  "APCINVSB" (cuscode$,      /* Customer Code              */~
                          so$,           /* Sales Order Number         */~
                          bol$,          /* Bill of Lading Number      */~
                          userid$,       /* User Id                    */~
                          session$,      /* APC001-T,F,S,S APC001-M,T,W*/~
                          postdate$,     /* Invoice Date = Post Date   */~
                          sav_load$,     /* LoadProductShipped (AWD012)*/~
                          "APCINVSB",    /* Program (AWD014)           */~                          
                          #46,           /* (ARIBUFFR )-Invoice Headers*/~
                          #47,           /* (ARIBUF2 )-Invoice Lines   */~
                          #17,           /* (SYSFILE2)-                */~
                          #31,           /* (STORNAME)- Store Names    */~
                          #48,           /* (ARINUMBR)- Inv Control    */~
                          #44,           /* (ARMTRIAL)- A/R TRIAL BAL  */~
                          #5,            /* (BCKMASTR)- S.O. Headers   */~
                          #6,            /* (BCKLINES)- S.O. Lines     */~
                          #1,            /* (SHPHDRS) - Ship Sched Head*/~
                          #2,            /* (SHPLINES)- Ship Sched Line*/~
                          #42,           /* (ARMTERMS)- A/R Payment Ter*/~
                          #41,           /* (CUSTOMER)- Customer Master*/~
                          #12,           /* (INVMASTR)-                */~
                          #38,           /* (CATEGORY)-                */~
                          #39,           /* (STXCODES)-                */~
                          #63,           /* (BCKSUBPT)     PAR000      */~
                          #53,           /* (ORABOL)    (AWD012)       */~
                          #56,           /* (SHPAUDIT)  (AWD014)       */~
                          #60,           /* (BCKLIN2)   CR2829         */~
                          inv% )         /* 0 = OK, <> = 0 (ERRORS)    */
                                         /* 1 = ALREADY INVOICED       */
                                         /* 2 = S.O. NOT ON FILE       */
                                         /* 3 = BOL  NOT ON FILE       */
        return

        calculate_day                                 /*  (AWD009)     */
            day% = 7%      
            testdate$ =  str(app_dte$,1%,6%)
            call "DATUFMTC" (testdate$)
            call "DAY" addr(str(testdate$,,6%), day%)
 
            day% = day% - 1%
            if day% = 0% then day% = 7%
            
            for k% = 1% to 10%
                if postdate$(k%) = app_dte$ then goto post_found

            next k%
REM                 POSTDATE$ = SAV_POSTDATE$            /* DEFAULT */
REM                 SESSION$  = SAV_SESSION$             /* DEFAULT */
REM                 SESSION$  = SESSIONS$(7%)    /* SEVENTH SESSION */
REM                 POSTDATE$ = POSTDATE$(7%)    /* SEVENTH SESSION */
            init(" ") testdate$
REM            TESTDATE$ = POSTDATE$(1%)
            testdate$ = date
            call "DATE" addr("G+",testdate$, 1%, testdate$, err%)
            
            sav_k% = 1%
            for k% = 1% to 10%        /* look for session 10 days out */
REM                 IF TESTDATE$ >= POSTDATE$(K%) THEN NEXT_K
              if testdate$ = postdate$(k%) then goto post_found
REM                    SAV_K% = K%
REM                    TESTDATE$ = POSTDATE$(K%)
REM NEXT_K
            next k%
            session$  = sessions$(sav_k%)
            postdate$ = postdate$(sav_k%)
        return
        post_found
              session$  = sessions$(k%)
              postdate$ = postdate$(k%)
        return                                        /*  (AWD009)     */
                                         /* (EWD002) - Begin Mods  (C) */
        check_session                    /* Set-Up Session and Load    */
                                         /* Screen.                    */
            init(" ") sav_session$, inv_time$, days$(), dte$, load$,    ~
                      ld_desc$, line2$, first_session$
            str(line2$,63%) = "APCINVBO R7.00.00"
            inv_time$ = time
            inv_time% = 0%
            convert str(inv_time$,1%,4%) to inv_time%, data goto L60390
L60390:
            days$(1%) = "MONDAY   "
            days$(2%) = "TUESDAY  "
            days$(3%) = "WEDNESDAY"
            days$(4%) = "THURSDAY "
            days$(5%) = "FRIDAY   "
            days$(6%) = "SATURDAY "
            days$(7%) = "SUNDAY   "
            dte$ = date
            call "DATE" addr("GD", dte$, days$, inv% )
            for i% = 1% to 7%
                if days$ = days$(i%) then goto L60520
            next i%
            
L60520:     currdate$ = date
            call "DATEFMT" (currdate$)
            
            session$ = "170101" : load$ = "00000"
            day% = i%
            nxt_day% = i% + 1%
            if i% = 7% then nxt_day% = 1%
            session$ = str(currdate$,7%,2%)& ~
                       str(currdate$,1%,2%)&str(currdate$,4%,2%)
           
            sav_session$ = session$
            first_session$ = session$         /* (AWD009)  */
            gosub setup_session
            if inv% = 1% then gosub session1_closed   /* Today already closed */
            sav_postdate$ = postdate$         /* (AWD009)  */
            if inv% = 1% then return
            gosub setup_other_sessions        /* (AWD009)  */
            session$ = first_session$         /* (AWD009)  */
            if other_inv% = 1% then return    /* (AWD009)  */
            gosub session_screen
            if sav_session$ <> session$ then gosub setup_session
         return

         session1_closed
            currdate$ = date
L60648:     call "DATE" addr("G+", currdate$, 1%, dte$, inv%)
            call "DATE" addr("GD", dte$, days$, inv% )
            currdate$ = dte$
            if days$ = days$(6%) or days$ = days$(7%) then goto L60648
            
            call "DATEFMT" (currdate$)
            session$ = str(currdate$,7%,2%)& ~
                      str(currdate$,1%,2%)&str(currdate$,4%,2%)
            gosub setup_session
            sav_session$ = session$
            first_session$ = session$         /* (AWD009)  */
         return 
         
         setup_session                         /* Check for Valid Session*/ 
            inv% = 1%
            s_desc$ = "No A/R Session "& session$
            str(sav_key$,1%,8%) = "ARIUPDTE"
            str(sav_key$,9%,6%) = session$
            readkey$ = all(hex(00))
L60650:     read #43,key > readkey$,using L60670, readkey$, postdate$,    ~
                                                  status$, eod goto L60790
L60670:         FMT POS(4), CH(17), POS(41), CH(6), CH(1)
            if sav_key$ <> str(readkey$,1%,14%) then goto L60650
            if status$ = "D" then goto L60750 /* Valid Session         */
               if status$ = "C" then s_desc$ = "A/R Session Closed   "
               if status$ = "V" then s_desc$ = "A/R Session in Verify"
               if status$ = "U" then s_desc$ = "A/R Session Updating "
               return                        /* Possible Error Conditions */                          

L60750:     inv% = 0%                         /* No Error Flag         */
            s_desc$ = "Valid  Session "& session$
L60790: return
                                              /* (EWD002) Session Mods */
        prompt_user                           /* Session Error Display */  
            comp% = 2%
            hdr$ = "*** BOL/Invoicing Creation ***"
            msg$(1) = " *************  Notify Accounting  ************* "
            msg$(2) = "************* " & s_desc$ & " *************"
            msg$(3) = "Press <Return> to Exit Processing ...........    "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return
                                              /* (EWD002) - BOL Prompt */
        prompt_begin
            comp% = 2%
            hdr$ = "**** BOL/Invoice Creation ****" 
            msg$(1) = " ********** Start Creating BOL'S for  ********** "
            msg$(2) = "************* Load No (" & load$ &") *************"
            msg$(3) = "Press <Return> to Exit, PF(14) to Continue?      "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return
                                              /* (EWD002)              */
        session_screen                        /* (EWD002) Add Load No. */
L60940:    pfkey$ = hex(ffffffffffffffffffffffffff0e0f10ffffff00ffff)
           ewd_msg$="Press <Return> To Edit, PF(14) to Begin?"
           date$ = date : call "DATEFMT" (date$)
        accept                                                           ~
            at (01,02), "Shipment Pre-Invoicing",                        ~
            at (01,64), "Today:",                                        ~
            at (01,71), fac(hex(8c)),  fdate$                   , ch(10),~
            at (02,02), fac(hex(ac)),  line2$                   , ch(79),~
            at (04,02), fac(hex(94)),  errormsg$                , ch(79),~
                                                                         ~
            at (06,02), "Enter Load No. to Cut BOL'S     :",             ~
            at (06,40), fac(hex(81)), load$                     , ch(05),~
            at (06,50), fac(hex(84)), ld_desc$                  , ch(30),~  
                                                                         ~
            at (07,02), "Current Invoice Session Selected:",             ~
            at (07,40), fac(hex(81)),   session$                , ch(06),~
                                                                         ~
            at (09,20), "****************************************",      ~
            at (10,20), "*               N o t e                *",      ~
            at (11,20), "* The appropriate invoicing session is *",      ~
            at (12,20), "* displayed above, if necessary You    *",      ~
            at (13,20), "* may override by typing in a valid    *",      ~
            at (14,20), "* 'Session Id'. After 11:00 PM(forward)*",      ~
            at (15,20), "*                                      *",      ~
            at (16,20), "*  FORMAT YYMMDD   Ex. 171205          *",      ~
            at (17,20), "*                      Dec 5 2017      *",      ~
            at (18,20), "*                                      *",      ~
            at (19,20), "*                                      *",      ~
            at (20,20), "*                                      *",      ~
            at (21,20), "****************************************",      ~
            at (24,02), fac(hex(a4)), ewd_msg$                  , ch(79),~
                keys(pfkey$), key(keyhit%)

            if keyhit% <> 15 then L61240
               call "PRNTSCRN"
               goto L60940

L61240:     gosub edit_session
            gosub edit_load

            if keyhit% = 14% then goto L61250
               goto L60940
L61250: close ws
        call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        edit_session                        /* See if Session Changed */ 
            init (" ") s_desc$, errormsg$
            inv% = 0%            
            if sav_session$ = session$ then return
               if userid$ = "CMG" or userid$ = "RDB" then return
               for i% = 1% to 10%
                  if session$ = sessions$(i%) then goto L61390
               next i%
               if session$ <> sessions$(10%) then goto L61440
L61390:
               if inv_time% < 2300% then goto L61440
        return
L61440:     inv% = 1%                   /* Session error has occurred */
            s_desc$ = session$ & " is not VALID?"
            init(" ") session$               /* , sav_session$   */
            errormsg$ = s_desc$
        return

        edit_load                              /* (EWD002) Activate   */
           load% = 0%                          /* (APCPLNLD) - File   */
           if load$ = "00000" then goto L61620
           init(" ") ld_key$, ld_desc$
           ld_key$ = load$
           read #50,key = ld_key$, using L61560, ld_desc$, ld_status$,   ~
                                                         eod goto L61620
L61560:       FMT POS(16), CH(30), POS(94), CH(2)

           if ld_status$ < "16" then goto L61610  
REM           convert load$ to load%, data goto L61620

        return
L61610:    s_desc$ = ld_key$ & " is not Loaded Complete?"
           goto L61630
L61620:    s_desc$ = "(Error) Invalid Load Number?"
L61630:    errormsg$ = s_desc$
           init(" ") ld_key$, load$, ld_desc$
           inv% = 2%
        return

        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press <Return> To Continue, or PF(16) to Exit?"
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
           if comp% = 16% then exit_program
        return
                                            /* (EWD002) - End Mods (C) */

        skip_message                        /* (EWD004)/(EWD005)       */
           comp% = 2%                           
           hdr$ = "** (Skipped) (Skipped) (Skipped)  **"
           msg$(1%) = " - - - - - - - S k i p p e d - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press <Return> or any PF(Key) To Continue?"
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
                                            /* (EWD003) - End Mods (C) */

/* (EWD008) */
        open_error                                                            
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
            err% = 0%
        return
/* (EWD008) */

/* (AWD009)  BEG */
        setup_other_sessions
             init(" ") sessions$(), postdate$()
             other_inv% = 1%

             sessions$(1%) = first_session$
             postdate$(1%) = postdate$

             for i% = 2% to 10%             /* Check for nine other session */
L61725:          call "DATE" addr("G+", dte$, 1%, dte$, inv%)
                 call "DATE" addr("GD", dte$, days$, inv% )
                 currdate$ = dte$

                 call "DATEFMT" (currdate$)
                 session$ = str(currdate$,7%,2%)& ~
                       str(currdate$,1%,2%)&str(currdate$,4%,2%)
  
                 gosub setup_session
                 if (inv% = 1%) and (days$ = days$(6%) or days$ = days$(7%)) ~
                   then    goto L61725  /* if session not found, skip weekend */
                 if inv% = 1% then goto L61730
                 sessions$(i%) = session$
                 postdate$(i%) = postdate$

            next i%
                other_inv% = 0%
L61730:
        return
/* (AWD009)  END */

/* (AWD010)  BEG */

        close_trailer
             init(" ") trailer_key$, sav_trailer$, trailer_rec$
                                    
             gosub set_load_finish            /* CR890  */
                                    
             str(trailer_key$,1%,1%) = "0"
             str(trailer_key$,2%,5%) = load$
             str(sav_trailer$,1%,6%) = str(trailer_key$,1%,6%)
             read #52, hold, key 1% > trailer_key$, using L62000,            ~
                                         trailer_rec$, eod goto mainline
L62000:            FMT CH(128)
                       
                       str(trailer_key$,1%,20%) = str(trailer_rec$,21%,20%)
                       if str(trailer_key$,1%,6%) <> str(sav_trailer$,1%,6%) ~
                                                           then goto mainline

                       str(trailer_rec$,1%,1%)  = "1"
                       str(trailer_rec$,21%,1%) = "1"
                       str(trailer_rec$,55%,6%) = date
                       str(trailer_rec$,61%,8%) = time
                     
                       delete #52

                       write #52, using L62000, trailer_rec$
                                       
                goto mainline

/* (AWD010)  END */
REM--------------------------------------------------
REM Set the finish date and time for the load - CR890
REM--------------------------------------------------
        set_load_finish                             /* Update (AWDAPPLD)  */
            init(" ") ld_app_key0$, ld_app_rec$
            ld_app_key0$ = load$
            read #57,hold,key 0% = ld_app_key0$, using L51000, ld_app_rec$,~
                                                      eod goto L51200
L51000:        FMT CH(128)  
            
            if str(ld_app_rec$,103%,4%) > " " then return
            
            delete #57
            
            str(ld_app_rec$,97%,6%) = date 
            calc_time$ = time                      /* Military - HHMMSSXX */
            str(ld_app_rec$,103%,4%) =  str(calc_time$,1%,4%) 

            write #57 using L51000, ld_app_rec$, eod goto L51500          

L51200:     return
        
L51500:     errormsg$ = "(Error) - Unable to set load start date and time"
            gosub error_prompt     
        return

/* (AWD011)  BEG */
        inventory_error
            comp% = 2%
            hdr$ = "*** INVENTORY ERROR ***"
            msg$(1) = " *************  You haven't updated  *********** "
            msg$(2) = " ********* Inventory for this load  ******* "
            msg$(3) = "Press <Return> to Exit Processing ...........    "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
         goto exit_program                    
/* (AWD011)  END  */

        lookup_subpart                            /* PAR000  */
            init(" ") bcksubpt_rec$, flds$(), info_flds$()
            flag$ = "0"                  /* Sales Order Info         */
            pgm$  = "1" 

            convert so_inv$ to so_inv%, data goto sub_part1
sub_part1:
            convert item_no$ to item_no%, data goto sub_part2
sub_part2:
            convert so_inv% to so_inv$, pic(00000000)
         
            convert item_no% to item_no$, pic(###)

REM         call "SHOSTAT" (" SO AND ITEM " & so_inv$ & item_no$)   stop

        call "AWDBKSUB"   (flag$,        /* Flag 0=SalesOrder 1=Invoice*/~
                          pgm$,          /* Calling Program 0=BCKUPDTE */~
                                         /* 1=Any Other 2=Delete       */~
                                         /* 3=Invoice                  */~
                          so_inv$,       /* SO or Invoice Num to lookup*/~
                          item_no$,      /* Item Number                */~
                          bcksubpt_rec$, /* Record If BCKUPDTE then    */~
                                         /* pass in else pass out      */~
                          flds$(),       /* Part Number Fields         */~
                          info_flds$(),  /* Information Fields         */~
                          #63,           /* BCKSUBPT File              */~
                          err1%)         /* Error Code                 */
           
            if err1% <> 0% then                                          ~
                   str(bcksubpt_rec$,48%,20%) = "00000                    "

            if err1% = 0% then return
            return

            errormsg$ = "BCKSUBPT ERR= "&so_inv$ & " Line= " & item_no$  ~
                                      & " Flag= " & flag$


        return                                    /* PAR000 */

/* (AWD015) */
        shpAuditLog        
            put #56, using SHPAUDIT_FMT, date, time, adt_cuscode$, adt_so$,   ~
                   adt_seq$, bol$, invnr$, adt_po$, shipdate$, userid$,       ~
                   session$, load$, program$, function$, notes$
                   
            write #56, eod goto NoAuditWrite
NoAuditWrite:
        return
        
                 
SHPAUDIT_FMT:  FMT CH(06), CH(08), CH(09), CH(08), CH(03), CH(03), CH(09), ~
                   CH(16), CH(06), CH(03), CH(06), CH(05), CH(09), CH(20), ~
                   CH(128) 
/* (\AWD015) */

        REM *************************************************************~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * Terminates execution (files closed automatically).        *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")
            call "FILEBGON" (#20)
            call "FILEBGON" (#33)
            end


