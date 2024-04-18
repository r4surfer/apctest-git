        REM *************************************************************~
            *                                                           *~
            *  Program Name      - BCKFASTR - (Modified Version for APC)*~
            *  Creation Date     - 05/01/96                             *~
            *  Last Modified Date- 05/28/98                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Fast Sales Order Entry Utility       *~
            *                      Program.                             *~
            *                                                           *~
            *  Code Tables Used  - PLAN HOWS, PLAN CUTO, PLAN DELV      *~
            *                      PLAN FOB                             *~
            *  Subroutine Used   - (APCPLN3B) - Calculate Due Date      *~
            *                      (APCPL3SB) - Display Customer Info   *~
            *                      (APCPLN8B) - Display S.O. Status     *~
            *                      (APCPL10B) - Password Override for   *~
            *                                   Delivery Code (APCPL10B)*~
            *  Special Comments  - PF(8) - Display Customer Info        *~
            *                      PF(10)- Display Sales Order Status   *~
            *                      PF(14)- Save and Fax a S.O. Ack.     *~
            *                                                           *~
            *     CRHOLD$ = "H" the the S.O. Status% will be set to     *~
            *               99% Which means S.O. cannot be Assigned to  *~
            *               a Load.                                     *~
            *             - (32120) - Set Status to 99% Credit Hold     *~
            *             - (40625) - Cannot delete Hdr for Planned Ord *~
            *             - (43980) - Cannot Delete Line Items Planned  *~
            *             - (61855) - Display Credit Limit Msg. 61880   *~
            *             - (32100) - Set Fax flag S_FAX$, and Call Sub *~
            *             - (32130) - to 'SEND_FAX' then Startover.     *~
            *             - (32140)                                     *~
            *                                                           *~
            *     BCK$    = Four Flags used to test Devlivery           *~
            *             - (1%,1%) = Set to "Y" for Hinge Code = 97    *~
            *             - (2%,1%) = Set to "Y" for Hinge Code = 98    *~
            *             - (3%,1%) = Set to "Y" for Hinge Code = 99 Ok *~
            *             - (4%,1%) = Set to "Y" for W/F Mulled Ok      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * (EWD)  M O D I F I C A T I O N S  to Date                 *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *          ! Last Caelus Change for Release 6.0       ! JDH *~
            * 03/01/90 ! Changd PIC for exchange rate reverse date! JDH *~
            * 07/05/90 ! Numerious Modifications for APC          ! RHH *~
            *          ! (1) - Display Customer Type on Screen    !     *~
            *          ! (2) - Display and Access Customer Msg    !     *~
            *          !       Text. ( Wired to PF(14) )          !     *~
            *          ! (3) - When using Manage Text,Display SO  !     *~
            *          !       Text Assigned to Customer 'APCSOLST!     *~
            *          ! (4) - Calculate Due Date and Req. Ship   !     *~
            *          !       Date. 'APCDATE'                    !     *~
            *          ! (5) - Check PO'S. Can only have (1) SO   !     *~
            *          !       for Customer's PO. ( CHECK_PO )    !     *~
            * 07/25/90 ! (6) - Build Part and Price LINE - 52030  ! RHH *~
            * 08/08/90 ! APC MOD FOR OPEN ORDER QTY LINE 53216    ! RHH *~
            * 09/17/91 ! APC MOD SKIP CREDIT CHECK INVOICE 19537  ! RHH *~
            * 05/01/92 ! APC Modifications for Catalog and Special! RHH *~
            *          !     Customer Pricing                     !     *~
            *          ! ( line - 16651 ) Set Customer Special    !     *~
            *          !                  Price for Part PRICE(C%)!     *~
            *          ! ( GOSUB PRICING_STUFF )                  !     *~
            * 02/16/94 ! APC Mod Put into Place the New Pricing   ! RHH *~
            *          !     Interface Module (APCPR0SB) New Part !     *~
            *          !     Build and Price Calc Routine.        !     *~
            * 02/01/96 ! APC Mod for new planning and clean up of ! RHH *~
            *          !     Sales Order entry.                   !     *~
            *          !    - Remove Sub 'MANUAL'                 !     *~
            *          !    - Remove Sub 'PIPATCSB' Check ATC     !     *~
            *          !    - Remove Sub 'BCKPRCSB' Cost and Price!     *~
            *          !    - Remove Sub 'HNYQDISP' Check Qty's   !     *~
            *          !    - Allow for Editing S.O. Header       !     *~
            *          !    - Mod to Part Number edit at 51535    !     *~
            *          !      Eliminate 2 Key Strokes.            !     *~
            * 05/01/96 ! APC Mods for New Planning                ! RHH *~
            *          !              Disabled CUR$ Currency      !     *~
            *          !   Disabled Enables for Fields 3,4,5 on   !     *~
            *          !   Screens '101 and '102 (Position at PO) !     *~
            *          ! Added New Subroutine to Calc Due Date    !     *~
            *          ! New Routine (CHECK_SPECIALS) to insure   !     *~
            *          ! that the Proper Delivery Code has been   !     *~
            *          ! entered.                                 !     *~
            * 06/06/96 ! Mods - S_FAX$ Flage to Send a Fax        ! RHH *~
            *          !        Look_up Model and Verify          !     *~
            *          !        Check for Valid Delivery Codes    !     *~
            *          !        New Password Subroutine           !     *~
            * 06/17/96 ! Mod  - LOOKUP_SO to Check the STATUS% of ! RHH *~
            *          !        a S.O. If S.O. Assigned to a Load !     *~
            *          !        (STATUS% > 1%) and not equal to   !     *~
            *          !        99% then Cannot be Deleted.       !     *~
            * 01/08/97 ! Mod all edit of fields in header detail  ! RHH *~
            *          !     Screen '102 lines 41100 - 41110      !     *~
            * 02/21/97 ! Mod to 'BUILD_CATEGORY' Eliminate the    ! RHH *~
            *          !    need for customer service to enter    !     *~
            *          !    any G/L Account Numbers.              !     *~
            *          !    Special Category Codes base on HowShip!     *~
            *          !    SAMP, DISP, MISC, PART, SALV, SCRA    !     *~
            * 08/19/97 ! Mods - CHECK_SPECIALS allow 312, 321, and! RHH *~
            *          !        Wood Surround to go with one (1)  !     *~
            *          !        week deliveries. Clean-Up Copy    !     *~
            *          !        Sales Order. Insure that an       !     *~
            *          !        Assigned or Planned Sales Order   !     *~
            *          !        cannot be saved or modified.      !     *~
            * 02/27/98 ! Mod for Checking Upgrade to R6.04.03     ! RHH *~
            *          !   ( Will Use Caelus Version R7.00.00 )   !     *~
            *-----------------------------------------------------------*~
            * All (Caelus) M O D I F I C A T I O N S  to Date           *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/13/87 ! ORIGINAL                                 ! ERN *~
            * 05/15/87 ! HNYMASTR record length mod.              ! JIM *~
            *          !   HNYGLGET gets acct #5 (sales). Was #4. !     *~
            *          !   Added SYSFILE2 to CPRASSGN.            !     *~
            * 11/03/87 ! Added Export Flag Maintenance            ! MJB *~
            * 11/17/87 ! Added Multi-currency, CURMASTR, BCKLNCUR,! JIM *~
            *          !   CURCONVR.                              !     *~
            * 01/15/88 ! Added Check to History Files             ! JDH *~
            * 03/21/88 ! Added Cancelled sales order logic.       ! JDH *~
            * 04/27/88 ! Take out ability to delete & have only   ! JDH *~
            *          !   called up orders able to be cancelled  !     *~
            * 07/01/88 ! Open Order amt stored in Statutory curr. ! JIM *~
            * 08/04/88 ! Update CUSTOMER A/R w/ transaction curr. ! JIM *~
            * 08/04/88 ! Correct CUSTOMER BILLTO/SHIPTO FMT stmt. ! JIM *~
            * 08/04/88 ! HNYMASTR conversion factor to 7 decimals.! JIM *~
            * 08/04/88 ! Can't delete last line of a Sales Order. ! JIM *~
            * 08/29/88 ! Corrected Credit display on last screen. ! JDH *~
            * 09/28/88 ! Rounded Credit control amounts & line    ! JDH *~
            *          !   extentions to 2 decimal places.        !     *~
            * 09/29/88 ! Currency accums now use open qty & Save  ! JDH *~
            *          !   screen has proper values for Canceld SO!     *~
            * 10/05/88 ! Added DEMMASTR to call to BCKNEXT        ! JDH *~
            * 10/11/88 ! Added Same as Previous Line logic,       ! JDH *~
            *          ! Corrected prog name at IN_PROCESS Msg,   !     *~
            *          ! Added check for Allow SO Deletion Flag,  !     *~
            *          ! Added check to DEMMASTR for SO#,         !     *~
            *          ! Fixed precision of calc for UOM conv fctr!     *~
            *          ! Fixed 100 line item entry logic,         !     *~
            *          ! Initialised ORIGDATE$ & ORIGUSER$,       !     *~
            *          ! Inhibit prev field back to 1,  Whew!!    !     *~
            * 12/02/88 ! Added Credit Parent Logic.               ! JDH *~
            * 02/16/89 ! Proj 7890107 FSI/FSE & Soft Enables.     ! JIM *~
            * 02/16/89 ! Proj 7890206 Four SO Entry Modifications.! JIM *~
            * 02/16/89 ! Added 5th Alternate Key to CUSTOMER file.! JIM *~
            * 02/17/89 ! Fixed screen positioning of fields.      ! JIM *~
            * 03/21/89 ! Re-established 'No price' error message. ! JDH *~
            * 04/20/89 ! Changed screen literal for line item     ! JDH *~
            * 04/20/89 !  Shipped Qty to Invoicd Qty; Corrected   !     *~
            * 04/20/89 !  FMT statement @ 17200; Added Credit # of!     *~
            * 04/20/89 !  days past due; Added see Prices & Cost  !     *~
            * 05/15/89 ! Added Copy feature and Check ATC         ! JDH *~
            * 09/25/89 ! Corrected convert error for extension    ! JDH *~
            * 10/04/89 ! Disabled Currency field in edit mode.    ! JDH *~
            * 11/07/89 ! Added call to HNYQDISP.                  ! JDH *~
            * 12/07/89 ! Moved GET of MINSOQTY/MINSOINC to 50000s.! JDH *~
            * 12/14/89 ! Fixed doubling of open order amount when ! MJB *~
            *          !  recalling an order.                     !     *~
            * 12/21/89 ! Fixed stocking/pricing conversion when   ! JDH *~
            *          !  converting to statutory.                !     *~
            * 12/27/89 ! Initialized CONV(C%) to one, not zero.   ! JDH *~
            * 03/01/90 ! Changd PIC for exchange rate reverse date! JDH *~
            * 05/25/90 ! Init Shipping Qtys when copying order,   ! JDH *~
            *          !  Allow copy from history files.  Allow   !     *~
            *          !  Deletes if order copied.                !     *~
            * 06/04/90 ! Allow entry of Cust Name and search via  ! GJM *~
            *          !   Sortname AIX when Cust No. not known   !     *~
            * 06/04/90 ! Other changes to initial searches also.  ! LDJ *~
            *          !   Now left justifies numeric fields on   !     *~
            *          !   Line Item Detail Screen.  Issues       !     *~
            *          !   ASKUSER warning if SHIP DATE falls on  !     *~
            *          !   a weekend.                             !     *~
            *          !   Corrected bug on line item full screen !     *~
            *          !   edit of numeric fields - if error      !     *~
            *          !   fields would be reset to previous      !     *~
            *          !   value.  Added PF29 - Manage Options -  !     *~
            *          !   to line item edit screen.              !     *~
            * 07/03/90 ! Fixed QC rejections since Larry didn't.  ! JDH *~
            * 09/26/90 ! Disabled 'Invoiced Qty' for Line Item,   ! JDH *~
            *          !   Hdr Edit allowed from Summary Screen.  !     *~
            * 10/24/90 ! Fixed STARTOVER after copy function      ! MJB *~
            * 04/04/91 ! PRR 11837 Added a lot enable check       ! SID *~
            *          ! PRR 11801 If No Bill to, the set Bill to !     *~
            *          !     equal to Ship to.                    !     *~
            *          ! PRR 11838 Added Error Check              !     *~
            *          ! PRR 11882 Fixed UOM Conversion           !     *~
            *          ! PRR 11722 Added Codes to avoid users from!     *~
            *          !     deleting of Shipped, Invoiced and    !     *~
            *          !     Scheduled of Line Items.             !     *~
            * 05/24/91 ! PRR 11892 Avoid Cancel Reason Description! JIM *~
            * 05/24/91 ! PRR 11931 Replicate Previous Line        ! JIM *~
            * 06/13/91 ! ALLFREE.                                 ! JIM *~
            * 07/19/91 ! Ensured that Allocation Qty is zero if   ! JDH *~
            *          !   if Allocation Flag is 'N'.             !     *~
            * 02/04/92 ! PRR 11909 Check status of BillTo, Parent.! JDH *~
            *          ! PRR 10609 Honor Default Demand Type swtch!     *~
            *          ! PRR 11967 Allow Cancel of LIs with BOLs. !     *~
            *          ! PRR 11905, 11249, 11623  Honor new Auto- !     *~
            *          !   Hold option (Override manual or not).  !     *~
            *          ! PRR 10611, 10612  LI Text in Inputmode.  !     *~
            *          ! PRR 12110  Hdr fields same pos all scrns.!     *~
            * 04/20/92 ! Orders w/o open qty not on CR Hold.      ! JDH *~
            * 05/27/92 ! PRR 12425, 12447 Weekend msg now can exit! JDH *~
            *          !   to input new date. No reshow if same.  !     *~
            *          ! Added ATC msg for ship date on LI Screen.!     *~
            * 07/24/92 ! PRR 12531 Added Allocation type 'P' for  ! JDH *~
            *          !   Previously Allocated.                  !     *~
            * 10/26/92 ! Added call to SZRINSUB for entry of size ! WPH *~
            *          ! runs and then auto-create line items.    !     *~
            * 11/13/92 ! Cust Credit- Dynamic fields from CCRMASTR! JIM *~
            * 11/13/92 ! Cust Credit- Added calls to ARQCUSCR.    ! JIM *~
            * 11/20/92 ! Brought Core Deposit Module to R6.02.03. ! JIM *~
            * 12/01/92 ! PRR 12709  Size Run Price 0 if not found.! JDH *~
            *          ! PRR 12488 - If set, use default value for!     *~
            *          !   Allocation Method & Demand Type from   !     *~
            *          !   the Part Master file (HNYMASTR).       !     *~
            *          ! Clean up some implied integers in arrays.!     *~
            * 02/02/93 ! SO Entry Pgms back in sync;  Default str ! JDH *~
            *          !   from USERINFO, Calc default date from  !     *~
            *          !   offset, Override of Credit Hold, Cust  !     *~
            *          !   Status Check subroutine.               !     *~
            *          ! PRR 12614 Export Flag defaults from Cust.!     *~
            *          ! PRR 11200 Warning if PO already used.    !     *~
            * 08/05/93 ! 1) Added Customer/Manufacturer part cross! MLJ *~
            *          !  referencing ability on input vis PFkey  !     *~
            *          !  22 and 23.                              !     *~
            *          ! 2) Added PF22 Pt Toggle on line summary  !     *~
            *          !  to toggle between CMS and Cust or MFR   !     *~
            *          !  part numbers.                           !     *~
            * 08/17/93 !Support for Extra Line Generation (core,?)! KAB *~
            * 10/25/93 ! Added Multi-Line Quote Cutover logic.    ! JIM *~
            * 10/25/93 ! Selected implied integer convs fixed.    ! JIM *~
            * 10/25/93 ! PRRs 11858 & 12869- SO# now grabbed as   ! JIM *~
            *          !   per BCKINPUT- s/b fewer wasted SO#s.   !     *~
            * 10/25/93 ! Enhanced SHOSTAT to show SO# being saved.! JIM *~
            * 10/25/93 ! PRR 12809- Avoid double call to BCKMINSB.! JIM *~
            * 10/26/93 ! PRR 13025- Added calls to HNYMSTSB.      ! JIM *~
            * 10/28/93 ! Added Last SO # Saved to Header Line 1.  ! JIM *~
            * 11/08/93 ! Force U3% = 0% at CALL to TASKUP.        ! JIM *~
            * 11/19/93 ! Added support for Shipping Priority Code.! MLJ *~
            * 03/07/94 ! Changed BOMSPEC record length            ! WPH *~
            * 03/24/94 ! PRR 12915.  Change all ship dates.       ! JDH *~
            *          ! Honor Use ONLY A/R for credit limit check!     *~
            * 04/05/94 ! PRR 13133. PF27 to ATC honoring Horizon. ! JDH *~
            *          ! Also, ATC values blink if negative.      !     *~
            * 06/06/94 ! Fixed line seq. on cutover quotes.       ! JDH *~
            * 07/05/94 ! Added PF(11)Date Run on Line Item Screen.! MLJ *~
            *          !   Calls new DATRUNSB for creation of     !     *~
            *          !   multiple SO lines.                     !     *~
            * 07/19/94 ! Changed to honor new BOMOPSUB as the only! WPH *~
            *          !   means to select the BOM version and the!     *~
            *          !   primary means to determine price of    !     *~
            *          !   type 000 parts.                        !     *~
            * 07/19/94 ! Changed to honor new BOMOPSUB as the only! WPH *~
            *          !   way to pick the BOM version, and the   !     *~
            *          !   primary means of determining price for !     *~
            *          !   type 000 parts.  Also corrected bugs   !     *~
            *          !   in MLQ cutover logic and now cutover   !     *~
            *          !   MLQSPHDR to BOMSPHDR.                  !     *~
            * 11/29/94 ! Added PARTTYPE$() and now suppresses     ! WPH *~
            *          !   PF29 for options if not generic part.  !     *~
            * 12/29/94 ! Added Precious Metal Surcharge Additions ! RJH *~
            *          !   to SO Price and Extention, governed by !     *~
            *          !   three PM Flags (Sys, SO, INV).         !     *~
            * 03/21/95 ! PRR 13143.  Added Print Acknowledgements?! JDH *~
            * 04/05/95 ! PRR - 13187 Track Cross-Reference Parts  ! RJH *~
            *          !   for future reference and printing on   !     *~
            *          !   acknowledgements, and invoices, etc.   !     *~
            * 04/05/95 ! PRR 13358 - BOMSPEC now removed when PF1 ! RJH *~
            *          !   Startover from input(append) mode.     !     *~
            * 04/26/95 ! PRR 13283 - Test for BCK-ARI conflict.   ! RJH *~
            * 06/28/95 ! Added Header and Line Date Tests for     ! JBK *~
            *          !   MLQ Quote Cutover.  Make sure that     !     *~
            *          !   'Due Dates' and 'Req. Ship Dates' for  !     *~
            *          !   Both Header and Lines are Valid.       !     *~
            * 06/29/95 ! Modified Quotation Selection so that only! JBK *~
            *          !   Quotions belonging to the Customer     !     *~
            *          !   can be selected for Cutover.  (This is !     *~
            *          !   partially due to need to protect       !     *~
            *          !   possible cross reference data create   !     *~
            *          !   as part the quote process.             !     *~
            * 07/11/95 ! Corrected Calls to CMSLINK.              ! JBK *~
            * 08/14/95 ! PRR 13491 - Modified Copy of previous    ! JBK *~
            *          !   line, for ship qty and alocation mthd. !     *~
            * 02/27/96 ! Acks print unless customer set to 'N'.   ! JDH *~
            * 09/05/96 ! Changes for the year 2000.               ! DXL *~
            * 07/07/97 ! Pass unformated date to PIPATCDZ         ! DXL *~
            * 04/01/98 ! Mods for (Y2K) Update and with (EWD)     ! RHH *~
            *          !   Special Mods to Order Entry and VIQS   !     *~
            * 05/28/98 ! (EWD001) Wood Surround Consolidation Code! RHH *~
            *          !   sc_grp$                                !     *~
            * 09/29/98 ! (EWD002) Can only change 'EDI' Sales     ! RHH *~
            *          !   Orders. All Orders entered throught    !     *~
            *          !   VIQS. Note - Can Fax                   !     *~    
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        dim                                                              ~
            acctxref$9,                  /* Account Xref               */~
            adjmsg$24,                   /* Adj/Cancel reason msg      */~
            adjrsn$9, adjrsndescr$30,    /* Adjustment Reason          */~
            alloc(100), alloc$(100)1,    /* Allocation Quantity/Flag   */~
            allocsave$1,                 /* Saves allocation flag      */~
            allocdflt$1,                 /* Allocation Flag Default    */~
            allocqty$10,                 /* Allocation Qty             */~
            allow_delete$1,              /* Allow Deletion of SOs Flag */~
            allow_delete_save$1,         /* Save Allow Delete Flag     */~
            ar(2), oo(2),                /* A/R, On Order amounts      */~
            atc$(100,2)41,               /* ATC msg for part/ship date */~
            atc1$8, atch1$8,             /* ATC 1 for part/ship date   */~
            atc2$8, atch2$8, atchz$3,    /* ATC 2 for part/ship date   */~
            askmsg$(3)79,                /* ASKUSER Messages           */~
            billto$9,                    /* Bill-to Customer ID        */~
            blankdate$8,                 /* Blank Date for Comparison  */~
            bom$(100)3,                  /* BOM for each line          */~
            can$1,                       /* Cancel Reason Code Here?   */~
            canceldate$8,                /* Cancellation Date          */~
            cat$(100)4,                  /* Part Category              */~
            comm%(3), comm$(3),          /* Commission Split %s        */~
            conv(100), conv$10,          /* Conversion Pricing->Stockng*/~
            convdate$6,                  /* Currency conversion date   */~
            cr_ar_only$1,                /* Use Only A/R for Cr Limit  */~
            crflag$1,                    /* Put Orders on Hold?        */~
            crhold$1,                    /* Order Credit Status        */~
            crmsg$(2)23,                 /* On Credit Hold Message     */~
            currkey$50,                  /* Currency lines read key    */~
            curr$1, currtype$1,          /* SYSFILE2 Currency codes    */~
            currency$4, currdesc$(2)32,  /* Currency code& description */~
            curr2$4,                     /* Currency Code testing      */~
            cursor%(2),                  /* Cursor location for edit   */~
            cuscode$9,                   /* Customer Code              */~
            custaxable$1,                /* Customer Taxable? (Y/N)    */~
            custype$2,                   /* Customer Type Code         */~
            date$8,                      /* Date for screen display    */~
            datetime$7,                  /* Buffer Date/Time Stamp     */~
            day$9,                       /* Day of the Week            */~
            demstatus$1,                 /* Demand Status              */~
            demstatusmsg$22,             /* Demand Status Message      */~
            demtype$(100)1,              /* Planning Demand Type       */~
            descr(12),                   /* PLOWCODE Argument          */~
            descr$(100)32,               /* Part Description           */~
            default$30,                  /* Default overrides from sub */~
            dflt_dem_type$1,             /* Default Demand Type for SOs*/~
            dfltdue$8,                   /* Default Due Date           */~
            dfltbol$1, dfltpick$1,       /* Default Print Settings Y/N */~
            dfltacks$1,                  /* Default Print Settings Y/N */~
            dfltsales$12,                /* Default Sales Account      */~
            dfltship$8,                  /* Default Req'd Ship Date    */~
            dfltstr$3,                   /* Default Store from USERINFO*/~
            discacct$12,                 /* Sales Discounts Account    */~
            discacctl$(100)12,           /* Sales Discounts (Lines)    */~
            discamt$10,                  /* Line Item Discount Amount  */~
            dr_date$(36)8,               /* Date Run - Due Date Array  */~
            dr_qty$(36)10,               /* Date Run - Quantity Array  */~
            dr_ship$(36)8,               /* Date Run - Ship Date Array */~
            duedate$(100)8,              /* Due Date                   */~
            e_lines%(100),               /* Extra Lines Possible       */~
            enabled%(26),                /* Enabled flags              */~
            errormsg$79,                 /* Error message              */~
            estnbr$8,                    /* Estimate Number            */~
            export$1,                    /* Export Flag                */~
            expdate$8,                   /* Quote Expire Date          */~
            ext$10,                      /* Line Item Extension        */~
            fac25$1,                     /* PF(25) FAC                 */~
            filler$(100)54,              /* Line Filler Data           */~
            fob$20,                      /* FOB                        */~
            hdr_map$(24)80,              /* Header Fields Map          */~
            hfac$(25)1,                  /* Header Item FACs           */~
            hifld%(5,2), lofld%(5,2),    /* Field # ranges             */~
            how_held$1,                  /* 'M'anual or 'blank'        */~
            howship$20,                  /* How Ship                   */~
            i$(24)80,                    /* Screen Image               */~
            incl(1), incl$(1)1,          /* PLOWCODE Include Parameters*/~
            inpmessage$79,               /* Informational Message      */~
            inputmsg$79,                 /* Input message for ASKDATE  */~
            invnbr$8,                    /* Invoice number             */~
            item$(100)3,                 /* P.O. Item Number           */~
            lfac$(30)1,                  /* Line Item FACs             */~
            xfac$1,                      /* Xref MFR Code FAC          */~
            lastchanged$8, lastuser$,    /* Last Change Info           */~
            lastplan$8,                  /* Date Last Planned          */~
            line1$79,                    /* First Line of Screen Headr */~
            linedisc(100), linedisc$6,   /* Line Item Discount %       */~
            line_map$(24)80,             /* Line Item Fields Map       */~
            lot$(100)6,                  /* Lot Number                 */~
            lsts$(100)6,                 /* Line Status                */~
                                         /*  1,3 = ' ', ADD, DEL, RES  */~
                                         /*  4,1 = s if scheduled      */~
                                         /*  5,1 = p if pre-invoiced   */~
                                         /*  6,1 = O if options exist  */~
            manadjre$1,                  /* Mandatory Adj Reason Code? */~
            mfgcode$12,                  /* Xref MFG Code              */~
            mfgdescr$30,                 /* PLOWCODE Description       */~
            mlqyorn$1, mlqdelt$1,        /* MLQ processing params      */~
            mlquote_cut%(100),           /* Indicates C/O this session */~
            mlquote_seq$(100)11,         /* Quote & Seq LI derived from*/~
            mscdescr$32,                 /* Misc. Description          */~
            msg$79,                      /* Misc use Message           */~
            newdate$8,                   /* Change date for line items */~
            nonstockmsg$(100)6,          /* Non-Stock Part Flag & Msg  */~
            offset_due$3,                /* Offset Days for Due Date   */~
            offset_shp$3,                /* Offset Days for Ship Date  */~
            opc$1,                       /* Price code for options     */~
            opdlrs(4),                   /* BOMOPSUB Cost, Price Info  */~
            openqty(100), openqty$10,    /* Open Order Quantity        */~
            oldline$3,                   /* Quote line number          */~
            order(100), order$10,        /* Original Order Quantity    */~
            orderdate$8,                 /* Order Date                 */~
            orderdisc$6,                 /* Order Discount Percent     */~
            origdate$8, origuser$3,      /* Originally Entered, User   */~
            origdue$(100)8,              /* Original Due Dates         */~
            override_crhld$1,            /* Allow override of cr hold? */~
            parent$9,                    /* Credit Parent X-ref        */~
            part$(100)25, savepart$25,   /* Part Code                  */~
            partflag$4,                  /* Special/Obsolete Flag      */~
            parttaxable$1,               /* Part Taxable? (Y/N/ )      */~
            parttype$(100)3,             /* Part Type                  */~
            passpart$(72)25,             /* Part array from SZRINSUB   */~
            passqty$(72)10,              /* Qty  array from SZRINSUB   */~
            pc$1,                        /* Price Code                 */~
            pcd$8,                       /* Planned Completion Date    */~
            pf$(3)79, pfkey$32,          /* PF Prompts and Keys        */~
            pickprint$1,                 /* Pick/BOL/Ack Print Flag    */~
            pickprint$(3)1,              /* Pick/BOL/Ack Print Array   */~
            plan$79,                     /* Planning Info Line         */~
            plowhdr$(3)79,               /* PLOWCODE Headers           */~
            plowmap(16),                 /* PLOWCODE Descr. Mapping    */~
            plowkey$99,                  /* Misc Read/Plow Key         */~
            plowkey2$99,                 /* Misc Read/Plow Key         */~
            plowkey3$99,                 /* Misc Read/Plow Key         */~
            pmp%(3,2),                   /* Message to prompt with     */~
            pm_hdr_dsply$30,             /* Display header variable    */~
            pm_inv$1,                    /* Precious Metal INV Flag    */~
            pm_sys_inv$1,                /* Precious Metal INV Flag    */~
            pm_on$1,                     /* Precious Metal ON Flag     */~
            pm_so$1,                     /* Precious Metal SO Flag     */~
            pm_sys_so$1,                 /* Precious Metal SO Flag     */~
            pm_adj%(100),                /* PM Price Adjusted Flag     */~
            pm_base(100),                /* PM Base Price Amount       */~
            pm_cus_inv$1,                /* PM INV Flag (Customer)     */~
            pm_cus_so$1,                 /* PM INV Flag (Customer)     */~
            po$16,                       /* Purchase Order Number      */~
            poreqd$1,                    /* PO Required?               */~
            preinv(100), preinv$10,      /* Qtys Pre-Invoice           */~
            price(100), price$10,        /* Unit Price (Pricing UOM)   */~
            pricestk(100),               /* Unit Price (Stockng UOM)   */~
            priceuom$(100)4,             /* Pricing Unit of Measure    */~
            priority$(100)1,             /* Planning Priority Code     */~
            project$(100)8,              /* Project Number             */~
            qtyschld(100), qtyschld$10,  /* Total Qty in Shipping      */~
            quotenbr$8, quotesave$8,     /* Quotation # & Work Area    */~
            record$150,                  /* Temporary variable         */~
            readkey$50,                  /* Misc Read Key              */~
            refdesc$(100)30,             /* Reference Part Description */~
            refpart$(100)25,             /* Reference Part Number      */~
            reftype$(100)1,              /* Ref Type 'M'anuf, 'C'ustmr */~
            region$4,                    /* Region Code                */~
            salesacct$(100)12,           /* Sales Distr. Account       */~
            salesman$(3)4,               /* Salesman Code / Split %    */~
            sc_grp$1,                    /* (EWD001) Wood S/F Code     */~
            seq$(100)3,                  /* Line Item Number           */~
            scode$4,                     /* XREF Source Code           */~
            shipcode$1,                  /* Cust Deflt Ship Priority   */~
            scr%(2,30), set%(255),       /* Soft Enables / Screen Refs */~
            scrtext$9,                   /* Screen Text Message        */~
            sfac$(15)1,                  /* Summary- FACs              */~
            ship(100), ship$10,          /* Quantity Shipped           */~
            shipcode$(100)1,             /* Shipping Priority Code     */~
            shipdate$(100)8,             /* Required Ship Date         */~
            shipinstr$(2)50,             /* Shipping Instructions      */~
            shipto$(6)30,                /* Ship-to                    */~
            smryh$79,                    /* Sum Hdr Display Variable   */~
            smryhdr$79, smry$(10)79,     /* Summary Display Variables  */~
            so$16, last_so$16,           /* Sales Order Number         */~
            soassgn$3,                   /* Sales Order # Asgnmnt Flag */~
            soldto$(6)30,                /* Sold-to                    */~
            statutory$4,                 /* Statutory currency code    */~
            stkuom$(100)4,               /* Stocking Unit of Measure   */~
            store$3,                     /* Store Code                 */~
            taxable$(100)1,              /* Line Taxable? (Y/N)        */~
            temp$16,                     /* Temporary Variable         */~
            tempcus$9, tempso$16,        /* Temp Customer Code, SO#    */~
            terms$20,                    /* Payment Terms (Code)       */~
            testdate$8,                  /* Variable for testing dates */~
            text$(113,1)70,              /* Text Array                 */~
            textid$4, textidl$(100)4,    /* Text ID- Hdr, Lines        */~
            tlines%(2),                  /* Lines Recap                */~
            topen(4), torder(4),         /* Dollars Recap              */~
            txt$4,                       /* For testing TEXTID values  */~
            userid$3,                    /* Current User Id            */~
            vf$200,                      /* Variable Fields            */~
            weekday$9, weekdayh$9        /* Day of week                */

        dim f2%(64), f1%(64), fs%(64)    /* File Open & Read Statuses  */

                                         /* (EWD) - Begin Variables    */
        dim size$1, bck$10,              /* (O)pening, or (E)xact      */~
            desc$32, sp$2,               /* HOWSHIP and FOB Info.      */~
            upd$1,                       /* Update Prices (Y)es, (N)o  */~
            pc(36%),                     /* Calculated Prices          */~
            acct$(2%)9,                  /* 1% = SALES, 2% = DISCOUNT  */~
            cat_key$4,                   /* Build Category Lookup      */~
            cstat_msg$(5%)10,            /* Status descriptions        */~
                                         /* CURRKEY$50 - REMOVED       */~
            cus$3,                       /* APC - TEXT Y or N          */~
            cus_txt$4,                   /* APC - CUSTOMER TEXT        */~
            lcus$9,                      /* Customer Code              */~
            d1$15, d2$15, d3$30,         /* (APCPL3AB) - SUBROUTINE    */~
            hdr$40, msg$(3%)79,          /* ASKUSER TEXT               */~
            ms$(10%)79,                  /* Header Fields Map          */~
            keepso$16,                   /* SAVE S.O. FOR COPY         */~
            or_hows$2, or_cat$4,         /* Build Category Code        */~
            ref$(15%)2,                  /* Ref. Type Codes Catalog    */~
            ref1$(15%)2,                 /* Ref. Type Codes Spec Cat.  */~
            ref_p(15%),                  /* Ref. Prices APC Cat        */~
            ref_p1(15%),                 /* Ref. Prices Special Cat.   */~
            s_fax$1,                     /* 'Y' - Save Order Send Fax  */~
            s_23m$3, s_23$8,             /* USED FOR SERIES NAMING     */~
            code$2, cod$10               /* Sales Order Number         */
                                         /* (EWD) - End of Variables   */ 
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim cms2v$50
            cms2v$ = "R7.00.00 10/29/97 Year 2000 Compliancy(EWD) Mods "
        REM *************************************************************

            mat f2% = con

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #01 ! CUSTOMER ! Customer Master File                     *~
            * #02 ! SYSFILE2 ! Caelus Management System General Informa *~
            * #03 ! GLMAIN   ! General Ledger Main File                 *~
            * #04 ! HNYMASTR ! Inventory Master File                    *~
            * #05 ! BCKMASTR ! Backlog master file                      *~
            * #06 ! BCKLINES ! Back Log Line Item File                  *~
            * #07 ! SLMMASTR ! Salesman master file                     *~
            * #08 ! GENCODES ! General Codes File                       *~
            * #09 ! BCKBUFFR ! Backlog buffer for SO headers            *~
            * #10 ! BCKBUF2  ! Buffer for line items                    *~
            * #11 ! CATEGORY ! Inventory category codes file            *~
            * #12 ! STORNAME ! Store Info File - Name/Address           *~
            * #13 ! HNYQUAN  ! Inventory Part / Store / Lot Quantity Fi *~
            * #14 ! JOBMASTR ! WIP/JC Job Master File                   *~
            * #15 ! HNYGENER ! generic xref file                        *~
            * #16 ! BOMMASTR ! BOM relationship file                    *~
            * #17 ! BOMSPEC  ! options selected file                    *~
            * #18 ! BOMSPHDR ! Header file for BOMSPEC                  *~
            * #19 ! CALMASTR ! Planning Production Calendar File        *~
            * #20 ! ENGMASTR ! Engineering Master Filer                 *~
            * #21 ! ARMTERMS ! A/R Payment Terms Codes                  *~
            * #22 ! TXTFILE  ! System Text File                         *~
            * #23 ! DEMMASTR ! Demand Master File                       *~
            * #24 ! PIPMASTR ! Planned Inventory Position Master File   *~
            * #25 ! PIPIN    ! Planned inventory additions detail       *~
            * #26 ! PIPOUT   ! Planned inventory use detail             *~
            * #27 ! PIPCROSS ! hard peg cross reference file            *~
            * #28 ! SFCUM2   ! Cumulative sales forecast file           *~
            * #29 ! HNYDETAL ! Inventory Movement Detail File           *~
            * #30 ! BCKHLNES ! SO History Line Detail                   *~
            * #31 ! BCKHMSTR ! Sales Order History Master               *~
            * #32 ! USERINFO ! INDIVIDUAL USER DEFAULT INFORMATION      *~
            * #33 ! HNYALTRS ! Alternate Parts File                     *~
            * #34 ! VENDOR   ! Vendor Master File                       *~
            * #35 ! HNYPROC  ! Procurement History File                 *~
            * #36 ! VENPRICE ! Vendor Price Catalogue File              *~
            * #40 ! CURMASTR ! Multi-Currency Master file               *~
            * #41 ! BCKLNCUR ! Currency-specific BCK line items         *~
            * #42 ! CURCONVR ! Multi-Currency Conversion Tables         *~
            * #43 ! BCKHCLNS ! SO History Lines Currency Specific       *~
            * #44 ! USERLCMS ! CAELUS User List File                    *~
            * #45 ! CUSPTXRF ! Customer Part Number Xref File           *~
            * #46 ! MLQMASTR ! Quotation Master file                    *~
            * #47 ! MLQLINES ! Quotation Line Item file                 *~
            * #48 ! MLQLNCUR ! Currency-specific MLQ line items         *~
            * #49 ! MLQSPEC  ! BOMSPEC file for quotes                  *~
            * #50 ! MLQSPHDR ! BOMSPEC header file for quotes           *~
            * #51 ! CCRMASTR ! Customer Credit Master file              *~
            * #52 ! MLQPMSLD ! MLQ Shadow file of Precious Metal Srchges*~
            * xxx !          ! (EWD) - Begin Files                      *~  
            * #55 ! CPRPRICE ! Customer Price Master File               *~
            * #56 ! APCSKUNO ! APC SKU NUMBER MASTER FILE               *~
            * #57 ! APCPCMST ! Pricing Master Definition File           *~
            * #58 ! APCPCMSK ! Pricing key Definiton File               *~
            * #59 ! APCPCMSD ! Pricing Master Calc Definiton File       *~
            * #60 ! APCPLNOR ! Planning S.O. Header Master File         *~
            * XXX !          ! (EWD) - End of Files                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #01, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #02, "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #03, "GLMAIN",                                        ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   9

            select #04, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #05, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #06, "BCKLINES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #07, "SLMMASTR",                                      ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   4

            select #08, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #09, "BCKBUFFR",                                      ~
                        varc,     indexed,  recsize =  1020,             ~
                        keypos = 1, keylen =   10,                       ~
                        alt key  1, keypos =    4, keylen =   7, dup,    ~
                            key  2, keypos =   30, keylen =  16

            select #10, "BCKBUF2",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =  10,  keylen = 19

            select #11, "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   4

            select #12, "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3

            select #13, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #14, "JOBMASTR",                                      ~
                        varc,     indexed,  recsize =  700,              ~
                        keypos =   1,  keylen =  8

            select #15, "HNYGENER",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =   17, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  41

            select #16, "BOMMASTR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  31,                     ~
                        alt key  1, keypos =    1, keylen =  56

            select #17, "BOMSPEC",                                       ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  54,                     ~
                        alt key  1, keypos =   57, keylen =  23

            select #18, "BOMSPHDR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  53,                     ~
                        alt key  1, keypos =   35, keylen =  19

            select #19, "CALMASTR",                                      ~
                        varc,     indexed,  recsize = 1962,              ~
                        keypos =    1, keylen =   2

            select #20, "ENGMASTR",                                      ~
                        varc,     indexed,  recsize = 2015,              ~
                        keypos =    1, keylen =  29

            select #21, "ARMTERMS",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =    1, keylen =  20

            select #22, "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  11

            select #23, "DEMMASTR",                                      ~
                        varc,     indexed,  recsize =  123,              ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28,         ~
                            key  3, keypos =   29, keylen =  25, dup

            select #24, "PIPMASTR",                                      ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    2, keylen =  25,                     ~
                        alt key  1, keypos =    1, keylen =  26

            select #25, "PIPIN",                                         ~
                        varc,     indexed,  recsize =    60,             ~
                        keypos =   30, keylen =  19,                     ~
                        alt key  1, keypos =    1, keylen =  48

            select #26, "PIPOUT",                                        ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  56,                     ~
                        alt key  1, keypos =   20, keylen =  37

            select #27, "PIPCROSS",                                      ~
                        varc,     indexed,  recsize =   150,             ~
                        keypos =    1, keylen =  71,                     ~
                        alt key  1, keypos =   20, keylen =  52,         ~
                            key  2, keypos =   39, keylen =  33

            select #28, "SFCUM2",                                        ~
                        varc,     indexed,  recsize =  1985,             ~
                        keypos =    1, keylen =  25

            select #29, "HNYDETAL",                                      ~
                        varc,     indexed,  recsize =   150,             ~
                        keypos =      1, keylen = 42,                    ~
                        alternate key 1, keypos = 43, keylen = 6, dup,   ~
                                  key 2, keypos = 49, keylen = 2, dup

            select #30, "BCKHLNES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #31, "BCKHMSTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =   1,  keylen = 25,                      ~
                        alt key  1, keypos =  26, keylen =  16, dup

            select #32, "USERINFO",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos = 1, keylen = 3

            select #33, "HNYALTRS",                                      ~
                        varc,     indexed,  recsize =  60,               ~
                        keypos =    1, keylen =  33

            select #34,  "VENDOR",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 600,                                  ~
                         keypos = 1, keylen = 9,                         ~
                         alt key 1, keypos = 10, keylen = 30, dup

            select #35, "HNYPROC",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 134,                                  ~
                         keypos =32, keylen = 40,                        ~
                         alternate key 1, keypos = 7, keylen = 65,       ~
                                   key 2, keypos = 1, keylen = 40, dup,  ~
                                   key 3, keypos =41, keylen = 31, dup

            select #36, "VENPRICE",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 256,                                  ~
                         keypos = 10, keylen = 59,                       ~
                         alternate key 1, keypos = 1, keylen = 34, dup,  ~
                                   key 2, keypos =35, keylen = 34

            select #40, "CURMASTR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  4

            select #41,  "BCKLNCUR",                                     ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =   5,  keylen = 19,                      ~
                        alt key  1, keypos =   1, keylen =  23

            select #42, "CURCONVR",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  11

            select #43,  "BCKHCLNS",                                     ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =   5,  keylen = 19,                      ~
                        alt key  1, keypos =   1, keylen =  23

            select #44, "USERLCMS",                                      ~
                        varc,     indexed,  recsize =  400,              ~
                        keypos =    1, keylen =   3,                     ~
                        alt key  1, keypos =    4, keylen =  30, dup

            select #45, "CUSPTXRF",                                      ~
                        varc,     indexed,  recsize =  100,              ~
                        keypos =   26, keylen =  35,                     ~
                        alt key  1, keypos =  1, keylen = 60

            select #46,  "MLQMASTR",                                     ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =   10, keylen =   8,                     ~
                        alt key  1, keypos =    1, keylen =  17

            select #47,  "MLQLINES",                                     ~
                        varc,     indexed,  recsize = 400,               ~
                        keypos =  10,  keylen = 11

            select #48,  "MLQLNCUR",                                     ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =   5,  keylen = 11,                      ~
                        alt key  1, keypos =   1, keylen =  15

            select #49, "MLQSPEC",                                       ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =   26, keylen =  54,                     ~
                        alt key  1, keypos =   57, keylen =  23

            select #50, "MLQSPHDR",                                      ~
                        varc,     indexed,  recsize =  150,              ~
                        keypos =    1, keylen =  53,                     ~
                        alt key  1, keypos =   35, keylen =  19

            select #51, "CCRMASTR",                                      ~
                        varc,     indexed,  recsize = 200,               ~
                        keypos =    1, keylen =   9

            select #52, "MLQPMSLD",                                      ~
                        varc,     indexed,  recsize = 150,               ~
                        keypos = 1,    keylen =  21,                     ~
                         alternate key 1, keypos = 22, keylen = 25, dup, ~
                                   key 2, keypos = 65, keylen =  9, dup

                                                /* (EWD) - Begin      */
                                                /* (EWD) - 07/25/90 */
            select #55, "CPRPRICE",                                      ~
                        varc,     indexed,  recsize = 700,               ~
                        keypos =   1,  keylen = 47

            select #56, "APCSKUNO"                                       ~
                        varc,     indexed,  recsize =  73,               ~
                        keypos = 1,    keylen =  28,                     ~
                        alt key  1, keypos  =    29, keylen = 28, dup

            select #57, "APCPCMST"                                       ~
                        varc,     indexed,  recsize = 102,               ~
                        keypos = 9,    keylen =  40,                     ~
                        alt key  1, keypos  =     1, keylen = 8

            select #58, "APCPCMSK"                                       ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos = 1,    keylen =   5

            select #59, "APCPCMSD"                                       ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos = 1,    keylen =   9

            select #60, "APCPLNOR",                                      ~
                        varc,     indexed,  recsize = 170,               ~
                        keypos =    1, keylen =  51,                     ~
                        alt key  1, keypos =   27, keylen =  25,         ~
                            key  2, keypos =   70, keylen =   8, dup,    ~
                            key  3, keypos =   78, keylen =   8, dup,    ~
                            key  4, keypos =   52, keylen =   8,         ~
                            key  5, keypos =   36, keylen =  16, dup
                                                   /* (EWD) - End      */
        call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#01, 0%, f2%( 1%),   0%, " ")
            call "OPENCHCK" (#02, 0%, f2%( 2%),   0%, " ")
            call "OPENCHCK" (#03, 0%, f2%( 3%),   0%, " ")
            call "OPENCHCK" (#04, 0%, f2%( 4%),   0%, " ")
            call "OPENCHCK" (#05, 0%, f2%( 5%), 200%, " ")
            call "OPENCHCK" (#06, 0%, f2%( 6%), 400%, " ")
            call "OPENCHCK" (#07, 0%, f2%( 7%),   0%, " ")
            call "OPENCHCK" (#08, 0%, f2%( 8%),   0%, " ")
            call "OPENCHCK" (#09, 0%, f2%( 9%), 200%, " ")
            call "OPENCHCK" (#10, 0%, f2%(10%), 400%, " ")
            call "OPENCHCK" (#11, 0%, f2%(11%),   0%, " ")
            call "OPENCHCK" (#12, 0%, f2%(12%),   0%, " ")
            call "OPENCHCK" (#13, 0%, f2%(13%),   0%, " ")
            call "OPENCHCK" (#14, 0%, f2%(14%),   0%, " ")
            call "OPENCHCK" (#15, 0%, f2%(15%),   0%, " ")
            call "OPENCHCK" (#16, 0%, f2%(16%),   0%, " ")
            call "OPENCHCK" (#17, 0%, f2%(17%), 100%, " ")
            call "OPENCHCK" (#18, 0%, f2%(18%), 100%, " ")
            call "OPENCHCK" (#19, 0%, f2%(19%),   0%, " ")
            call "OPENCHCK" (#20, 0%, f2%(20%),   0%, " ")
            call "OPENCHCK" (#21, 0%, f2%(21%),   0%, " ")
            call "OPENCHCK" (#22, 0%, f2%(22%),   0%, " ")
            call "OPENCHCK" (#23, 0%, f2%(23%), 100%, " ")
            call "OPENCHCK" (#24, 0%, f2%(24%),   0%, " ")
            call "OPENCHCK" (#25, 0%, f2%(25%),   0%, " ")
            call "OPENCHCK" (#26, 0%, f2%(26%),   0%, " ")
            call "OPENCHCK" (#27, 0%, f2%(27%),   0%, " ")
            call "OPENCHCK" (#28, 0%, f2%(28%),   0%, " ")
            call "OPENCHCK" (#29, 0%, f2%(29%),   0%, " ")
            call "OPENCHCK" (#30, 0%, f2%(30%),   0%, " ")
            call "OPENCHCK" (#31, 0%, f2%(31%),   0%, " ")
            call "OPENCHCK" (#32, 0%, f2%(32%),   0%, " ")
            call "OPENCHCK" (#45, fs%(45%), f2%(45%), 0%, " ")
            call "OPENCHCK" (#51, 0%, f2%(51%), 200%, " ")
                                                     /* (EWD) - Begin  */
                                                 /* (EWD) - 02/16/94 */
            call "OPENCHCK" (#55, 0%, f2%(55%), 500%, " ")
            call "OPENCHCK" (#56, 0%, f2%(56%),   0%, " ")
            call "OPENCHCK" (#57, 0%, f2%(57%),   0%, " ")
            call "OPENCHCK" (#58, 0%, f2%(58%),   0%, " ")
            call "OPENCHCK" (#59, 0%, f2%(59%),   0%, " ")
                                                 /* (EWD) - 06/17/96 */
            call "OPENCHCK" (#60, 0%, f2%(60%), 100%, " ")
                                                     /* (EWD) - End    */

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
                                                     /* (EWD) - Begin */
                  ms$(1%) = "* Sales Order Acknowledgement Fax Sent  *"
                  ms$(2%) = "(1) Fax Not Sent, Can't Open Fax File?   "
                  ms$(3%) = "(2) Fax Not Sent, Not a Fax Customer?    "
                  ms$(4%) = "(3) Fax Not Sent,Sales Order not on File?"
                  ms$(5%) = "(4) Fax Not Sent,HP Shell Script Failed? "
                  ms$(6%) = "(5) Fax Not Sent,S.O. Ack. Print Error?  "
                  ms$(7%) = "(6) Fax Sent,Unable to Delete Fax File?  "
                                                     /* (EWD) - End   */

            blankdate$ = " "
            call "DATUFMTC" (blankdate$)

            call "READ100" (#02, "SWITCHS.COR", core%)
            call "EXTRACT" addr("ID", userid$)
            call "READ100" (#32, userid$, f1%(32%))
            if f1%(32%) = 1% then get #32 using L09033, dfltstr$
L09033:         FMT POS(64), CH(3)
            date$ = date : call "DATEFMT" (date$)
            call "SHOSTAT" ("Initializing...")
            ll% = 6%

            ref%, refflag%, cust% = 0%                    /* Xref Flags */
            cms% = 1%                                     /* Xref Flag  */
            xref% = fs%(45%)                              /* Xref Flag  */


*        Set some flags and misc variables
            call "BCKSWTCH" ("BCK", "SOASSIGN", soassgn$  , temp, u3%)
            call "BCKSWTCH" ("BCK", "CRHOLD  ", crflag$   , temp, u3%)
            call "BCKSWTCH" ("BCK", "OFFSTDUE", offset_due$, offsetd, u3%)
                offsetd% = offsetd
            call "BCKSWTCH" ("BCK", "OFFSTSHP", offset_shp$, offsets, u3%)
                offsets% = offsets
            call "BCKSWTCH" ("BCK", "OVRRIDCR", override_crhld$, temp,u3%)
            call "BCKSWTCH" ("BCK", "ALLOCATE", allocdflt$, temp, u3%)
            call "BCKSWTCH" ("BCK", "PICKLIST", dfltpick$ , temp, u3%)
            call "BCKSWTCH" ("BCK", "BOL     ", dfltbol$  , temp, u3%)
            call "BCKSWTCH" ("BCK", "CRHOLDAR", cr_ar_only$, temp, u3%)
            call "BCKSWTCH" ("BCK", "DELETE", allow_delete$, temp, u3%)
            call "BCKSWTCH" ("BCK", "MANADJRE", manadjre$, temp, u3%)
            call "BCKSWTCH" ("BCK", "DEM_TYPE", dflt_dem_type$, temp, u3%)
                if dflt_dem_type$ <> "2" then dflt_dem_type$ = "1"
            gosub init_enables
            allow_delete_save$ = allow_delete$

            readkey$ = all(hex(00))      /* GENCODES Directory         */
            str(readkey$, 10) = "UOM"
            call "READ100" (#08, readkey$, uom%)

            line1$   = "Sales Order Entry:"
            str(line1$,53%)= date$ & " BCKFASTR: " & str(cms2v$,,8%)
            smryhdr$ = "Seq Part Number                Order Qty"     &  ~
                       "   Open Qty   Open Ext Rqd Ship Status"

                                                   /* (EWD) - Begin    */
            cstat_msg$(1%) = "ACTIVE"
            cstat_msg$(2%) = "UNKNOWN"
            cstat_msg$(3%) = "INACTIVE"
            cstat_msg$(4%) = "HELD"
            cstat_msg$(5%) = "DELETE"
                                                   /* (EWD) - End      */
            init(hex(00)) hdr_map$()
            init(hex(01)) str(hdr_map$ ( 2%),  1%) /* Customer Number  */
            init(hex(02)) str(hdr_map$ ( 2%), 22%) /* Sales Order #    */
            init(hex(03)) str(hdr_map$ ( 3%),  1%) /* Ship-to          */
            init(hex(03)) str(hdr_map$ ( 4%),  1%) /* Ship-to          */
            init(hex(03)) str(hdr_map$ ( 5%),  1%) /* Ship-to          */
            init(hex(03)) str(hdr_map$ ( 6%),  1%) /* Ship-to          */
            init(hex(03)) str(hdr_map$ ( 7%),  1%) /* Ship-to          */
            init(hex(03)) str(hdr_map$ ( 8%),  1%) /* Ship-to          */
            init(hex(04)) str(hdr_map$ ( 2%), 47%) /* Store Number     */
            init(hex(05)) str(hdr_map$ ( 2%), 65%) /* Export Flag      */
            init(hex(06)) str(hdr_map$ ( 3%), 47%) /* PO               */
            init(hex(07)) str(hdr_map$ ( 4%), 47%) /* How Ship         */
            init(hex(08)) str(hdr_map$ ( 5%), 47%) /* FOB              */
            init(hex(09)) str(hdr_map$ ( 6%), 47%) /* Order Date       */
            init(hex(0a)) str(hdr_map$ ( 6%), 68%) /* Due Date         */
            init(hex(0b)) str(hdr_map$ ( 7%), 47%) /* Req'd Ship       */
            init(hex(0c)) str(hdr_map$ ( 8%), 47%) /* PM for SO        */
            init(hex(0d)) str(hdr_map$ ( 8%), 68%) /* PM for Inv       */
            init(hex(0e)) str(hdr_map$ (10%),  1%) /* Sold-to          */
            init(hex(0e)) str(hdr_map$ (11%),  1%) /* Sold-to          */
            init(hex(0e)) str(hdr_map$ (12%),  1%) /* Sold-to          */
            init(hex(0e)) str(hdr_map$ (13%),  1%) /* Sold-to          */
            init(hex(0e)) str(hdr_map$ (14%),  1%) /* Sold-to          */
            init(hex(0e)) str(hdr_map$ (15%),  1%) /* Sold-to          */
            init(hex(0f)) str(hdr_map$ (10%), 47%) /* Cancel Date      */
            init(hex(10)) str(hdr_map$ (11%), 47%) /* Price Code       */
            init(hex(11)) str(hdr_map$ (12%), 47%) /* Order Disc %     */
            init(hex(12)) str(hdr_map$ (13%), 47%) /* Payment Terms    */
            init(hex(13)) str(hdr_map$ (14%), 47%) /* Region           */
            init(hex(14)) str(hdr_map$ (15%), 47%) /* Salesman #1      */
            init(hex(14)) str(hdr_map$ (16%), 47%) /* Salesman #2      */
            init(hex(15)) str(hdr_map$ (17%),  1%) /* Sales Account    */
            init(hex(14)) str(hdr_map$ (17%), 47%) /* Salesman #3      */
            init(hex(16)) str(hdr_map$ (18%),  1%) /* Sales Discs Acct */
            init(hex(17)) str(hdr_map$ (19%),  1%) /* Ship Instruction */
            init(hex(17)) str(hdr_map$ (20%),  1%) /* Ship Instruction */

            init(hex(00)) line_map$()
            init(hex(01)) str(line_map$(14%),  1%) /* Part Number      */
            init(hex(04)) str(line_map$(14%), 35%) /* Part Descr       */
            init(hex(05)) str(line_map$(15%),  1%) /* Category         */
            init(hex(06)) str(line_map$(15%), 12%) /* Due Date         */
            init(hex(07)) str(line_map$(15%), 25%) /* Ship Date        */
            init(hex(08)) str(line_map$(15%), 40%) /* Stocking UOM     */
            init(hex(09)) str(line_map$(15%), 57%) /* Pricing  UOM     */
            init(hex(0a)) str(line_map$(15%), 67%) /* Conversion       */
            init(hex(02)) str(line_map$(16%),  1%) /* Order Qty        */
            init(hex(0b)) str(line_map$(16%), 18%) /* Allocation       */
            init(hex(0c)) str(line_map$(16%), 36%) /* Open Qty         */
            init(hex(0d)) str(line_map$(16%), 52%) /* Lot Number       */
            init(hex(0e)) str(line_map$(16%), 63%) /* Shipped          */
            init(hex(03)) str(line_map$(17%),  1%) /* Currency code    */
            init(hex(0f)) str(line_map$(17%), 12%) /* Price            */
            init(hex(10)) str(line_map$(17%), 29%) /* Line Disc %      */
            init(hex(11)) str(line_map$(17%), 60%) /* Taxable?         */
            init(hex(12)) str(line_map$(18%),  1%) /* PO Item          */
            init(hex(13)) str(line_map$(18%), 13%) /* Project          */
            init(hex(14)) str(line_map$(18%), 29%) /* Sales Acct       */
            init(hex(15)) str(line_map$(18%), 54%) /* Sales Disc Acct  */
            init(hex(16)) str(line_map$(19%),  1%) /* Ship Priority    */
            init(hex(17)) str(line_map$(19%), 13%) /* Priority         */
            init(hex(18)) str(line_map$(19%), 27%) /* Demand Type      */
*          INIT(HEX(19)) STR(LINE_MAP$(19%), 34%) /* Open             */
*          INIT(HEX(1A)) STR(LINE_MAP$(20%),  1%) /* Open             */

            lofld%(1%, 1%) = 1% : hifld%(1%, 1%) =  2%
            lofld%(1%, 2%) = 3% : hifld%(1%, 2%) = 13%
            lofld%(2%, 1%) = 3% : hifld%(2%, 1%) = 23%
            lofld%(3%, 1%) = 1% : hifld%(3%, 1%) =  3%
            lofld%(3%, 2%) = 2% : hifld%(3%, 2%) = 25%

            pmp%(1,1) = 1% : pmp%(1,2) = 2%
            pmp%(2,1) = 3%
            pmp%(3,1) = 4% : pmp%(3,2) = 5%

*        See if operator is an administrator or not
            call "CMSMACHK" ("BCK", lfac$(1), lfac$(2))
            if lfac$(1) = "Y" or lfac$(2) = "Y" then admin% = 1%

*        Check if a Sales Order is in-process for this User.  If one
*        is and it is from another program, inform and abort.  If it
*        is from this program then reload and continue.
            readkey$ = all(hex(00))  :  str(readkey$,,3) = userid$
            call "READ101" (#09, readkey$, f1%(9))
            if f1%(9) = 0% then L09745      /* Nothing in Process       */
                so$ = key(#09, 2%)
                get #09, str(readkey$,,20)
                if str(readkey$,11,8) = "BCKFASTR" then L09720
                     pf$(1) = "You did not complete processing Sales" &  ~
                              " Order " & so$
                     pf$(2) = "in the program " & str(readkey$,11,8) & "."
                     pf$(3) = "Press (RETURN) to exit this program."
                     call "ASKUSER" (keyhit%, "IN-PROCESS MESSAGE",      ~
                                     pf$(1), pf$(2), pf$(3))
                     goto exit_program
L09720:         delete #09
                call "DELETE" (#10, so$, 16%)
                call "PTUSEDSB" ("D", "BCK ", so$, "ALL",                ~
                                 " ", " ", " ", ret%)/* Del Xref Shadow */

L09745
*        Check for Multi-Currency
            curr$ = "N" : statutory$, currtype$ = " "
            call "READ100" (#02, "SWITCHS.CUR", f1%(2))
            if f1%(2) <> 0% then get #02 using L09785, curr$, statutory$,  ~
                currtype$
L09785:         FMT POS(21), CH(1), CH(4), POS(27), CH(1)
            if curr$ <> "Y" then statutory$, currtype$ = " "
            if curr$ <> "Y" then goto L09820
                call "OPENCHCK" (#40, 0%, f2%(40%),   0%, " ")
                call "OPENCHCK" (#41, 0%, f2%(41%), 400%, " ")
                call "OPENCHCK" (#42, 0%, f2%(42%),   0%, " ")
                call "OPENCHCK" (#43, 0%, f2%(43%),   0%, " ")

L09820
*        See if CANREASON files in place
            can$ = "N"
            readkey$ = "CANREASON"
            call "PLOWNEXT" (#08, readkey$, 9%, f1%(8%))
              if f1%(8%) = 1% then can$ = "Y"

*        Get Multi-Line Quotation processing values, if present.
            mlqdelt$, mlqyorn$ = "N"/* Default = Negatory on MLQ front */
            call "READ100" (#02, "SWITCHS.MLQ", f1%(2%))   /* SYSFILE2 */
            if f1%(2%) <> 0% then get #02 using L09920, mlqyorn$, mlqdelt$
L09920:         FMT POS(32), CH(1), POS(43), CH(1)
            if mlqyorn$ <> "Y" then goto L09972     /* No MLQ processing */
                call "OPENCHCK" (#46, fs%(46%), f2%(46%), 0%, " ")
                call "OPENCHCK" (#47, fs%(47%), f2%(47%), 0%, " ")
                call "OPENCHCK" (#49, fs%(49%), f2%(49%), 0%, " ")
                call "OPENCHCK" (#50, fs%(50%), f2%(50%), 0%, " ")
                if curr$ <> "Y" then goto L09972       /* OPEN MLQLNCUR? */
                     call "OPENCHCK" (#48, fs%(48%), f2%(48%), 0%, " ")

L09972
*        Check if Precious Metal Surcharge is on
            pm_on$, pm_sys_so$, pm_sys_inv$ = "N"
            call "READ100" (#02, "SWITCHS.BCK", f1%(02%))
            if f1%(02%) = 1% then get #02 using L09980, pm_on$,            ~
                                                  pm_sys_so$, pm_sys_inv$
L09980:         FMT POS(60), 3*CH(1)
            pm_hdr_dsply$, pm_hdr_dsply2$ = " "
            if pm_on$ <> "Y" then L10000
                  pm_hdr_dsply$  = "PM Surcharge at SO?"
                  pm_hdr_dsply2$ = "at INV?"
                  call "OPENCHCK" (#52, fs%(52%), f2%(52%), 0%, " ")

L10000: REM *************************************************************~
            *       I N P U T   M O D E   -  H E A D E R                *~
            *-----------------------------------------------------------*~
            * Handles input for abbreviated Header Screen.              *~
            *************************************************************
        inputmode
            gosub init_for_input

            short% = 1% /* Validate 1st 11 Header fields only */
            mostin% = 0%
            for fieldnr% = 1% to 2%     /* First Screen               */
                gosub'050(1%, fieldnr%, 1%)
                gosub L20000
L10120:         gosub'101(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <> 27% then goto L10140
                          appendquote% = 0%     /* Not appending lines */
                          gosub cutover_quote_from_top
L10140:               if keyhit%  =  3 and fieldnr% = 2 then             ~
                                                   gosub copy_function
                      if keyhit%  = 16 and fieldnr% = 1% then exit_program
                      if keyhit% <>  0 then       L10120
                gosub'151(fieldnr%, 1%) /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
                      mostin% = max(mostin%, fieldnr%)
            next fieldnr%

*        Do Input for Variable Fields
            call "VFINPSUB" ("BCKMASTR", "I", "Manage Sales Orders   ",  ~
                             "Customer: " & cuscode$ & "  SO: " & so$,   ~
                             "NN", vf$, keyhit%)
            if keyhit% = 1% then startover2

            goto appendlines


        copy_function     /* Allow selection of order to copy in       */
            readkey$ = " "
            inpmessage$ = hex(0684) & "Select Order to COPY from Curre"& ~
                                    "nt files"
            if f2%(31) = 0% then inpmessage$ = inpmessage$ &             ~
                                            " or PF16 for History Copy"
            call "GETCODE" (#05, readkey$, inpmessage$, 0%, 1.16, f1%(5))
            if f1%(5) = 0% then copy_history
               copy_what% = 5%
               goto begin_copy
        copy_history
            if f2%(31) <> 0% then return   /* No History File */
            inpmessage$ = hex(0684) & "Select Order to COPY from Histo"& ~
                                      "ry files"
            call "GETCODE" (#31, readkey$, inpmessage$, 0%, 1.16, f1%(31))
            if f1%(31%) = 0% then return
            copy_what% = 31%

        begin_copy
*        Now get Order, new dates, etc., etc., etc..
            return clear
            keepcus$ = cuscode$
            keepso$  = so$
            cuscode$ = str(readkey$,,9)
            so$      = str(readkey$,10)
            copy% = 1%
            allow_delete$ = "Y"
            gosub copy_order

            soonfile% = 0%
            if cuscode$ = keepcus$ then L10610
                cuscode$  = keepcus$
                call "READ100" (#01, cuscode$, f1%(1))
                                                 /* (EWD) - Begin      */
REM             get #01 using L10600, soldto$(), dfltacks$, shipto$()
REM L10600: REM FMT POS(40), 6*CH(30), POS(238), CH(1), POS(253), 6*CH(30)

                get #01 using L10604, soldto$(), dfltacks$, shipto$(),   ~
                    orderdisc, pc$, terms$, howship$, fob$, shipinstr$(),~
                               salesman$(), comm%(), region$
L10604:         FMT POS(40), 6*CH(30), POS(238), CH(1), POS(253),        ~ 
                                                 6*CH(30), POS(517),     ~
                    PD(14,4), CH(1), POS(543), 3*CH(20), POS(614),       ~
                    2*CH(50), 3*CH(4), 3*BI(1), CH(4)
                                                 /* (EWD) - End        */
L10610:     so$       = keepso$
            orderdate$, origdate$ = date
                call "DATEFMT" (orderdate$)
                call "DATEFMT" (origdate$ )
                canceldate$ = " "
            origuser$ = userid$
            lastchanged$, lastuser$ = " "
            nextbol% = 1%
            mostin% = 0%

            option% = 0% : parttype$(c%)     = " "
            for c% = 1% to maxlines%
                if option% = 1% then L10860
                call "READ100" (#04, part$(c%), f1%(4%))
                if f1%(4%) <> 0% then get #04, using L10855,              ~
                                             parttype$(c%), alloc$(c%)
L10855:              FMT POS(180), CH(3), POS(242), CH(1)
                if parttype$(c%)    = "000" then option% = 1%
L10860:         convert c% to seq$(c%), pic(##0)
                ship(c%), qtyschld(c%), alloc(c%), preinv(c%) = 0
                openqty(c%) = order(c%)
                origdue$(c%), duedate$(c%) = dfltdue$
                shipdate$(c%) = dfltship$
                if alloc$(c%) = " " then alloc$(c%) = allocdflt$
                lsts$(c%) = " A"
            next c%
            if option% = 0% then L10941
                u3% = 2%
                call "ASKUSER" (u3%, "OPTION PART COPIED", "At least 1 "&~
                     "Option part has been copied.", "Option selections"&~
                     " have NOT been copied and MUST be made manually.", ~
                     "Press RETURN to confirm")
                option% = 0%
L10941:     gosub adjust_currency_conversion
            lastseq% = maxlines%
            copy% = 0%
            goto L10970

        adjust_currency_conversion
            if currency$ = " " then currency$ = statutory$
            if currency$ = statutory$ then return
            call "DATREVRS" (orderdate$, rev_date$, errormsg$)
            if errormsg$ <> " " then return
            currkey$ = str(currtype$) & str(currency$) & str(rev_date$,,6)
            call "PLOWNEXT" (#42, currkey$, 5%, f1%(42%))  /* CURCONVR */
            if f1%(42%) = 0% then return
            get #42 using L10966, convdate$, conveqv, convunt
L10966:         FMT POS(12), CH(6), 2*PD(14,7)
            return

L10970:     fieldnr% = 2%
                copy1% = 1%
                gosub'050(1%, fieldnr%, 1%)
L10976:         gosub'101(fieldnr%, 1%) /* Display & Accept Screen    */
                      if keyhit%  =  1 then gosub startover
                      if keyhit% <>  0 then       L10976
                gosub'151(fieldnr%, 1%) /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10976
                      copy1% = 0%
                      mostin% = max(mostin%, fieldnr%)
                                                  /* (EWD) - Begin */
            for c% = 1% to maxlines%
                convert c% to seq$(c%), pic(##0)
                ship(c%), qtyschld(c%) = 0
                openqty(c%) = order(c%)
                origdue$(c%), duedate$(c%) = dfltdue$
                shipdate$(c%) = dfltship$
                lsts$(c%) = " A"
            next c%
            lastseq% = maxlines%
            copy% = 0%
                                                  /* (EWD) - End    */ 
            goto summary_screen

        REM *************************************************************~
            *             S U M M A R Y   S C R E E N                   *~
            * --------------------------------------------------------- *~
            * Summary Display and Main Control Screen.                  *~
            *************************************************************

        summary_screen
            gosub'050(1%, 0%, 2%)       /* Set input message           */
L11100:     gosub'101(0%, 2%)           /* Display Screen - No Entry   */
            mls% = maxlines%
            errormsg$ = " "
            if keyhit%  =  1% then gosub startover
            if keyhit%  =  2% then top% = 1%
            if keyhit%  =  3% then top% = mls%-9%
            if keyhit%  =  4% then top% = top%-9%
            if keyhit%  =  5% then top% = min(mls%-9%, top%+9%)
            if keyhit%  =  6% then top% = top%-1%
            if keyhit%  =  7% then top% = min(mls%-9%, top%+1%)
                                  top% = max( 1%, top%)
            if keyhit%  =  9% then       header_detail
            if keyhit%  = 10% then       cancel_order
            if keyhit%  = 11% then       appendlines
            if keyhit%  = 12% then gosub delete_order
            if keyhit%  = 16% then       datasave
            if keyhit%  = 18% then goto L11280
            if keyhit%  = 23% then       mod_all_dates
            if keyhit%  = 25% then gosub edit_text_hdr
            if keyhit%  = 26% then gosub call_customer_credit
            if keyhit%<>27% then goto L11250
                appendquote% = 1%           /* Append Quote Lines only */
                gosub cutover_quote_from_top
L11250:     if keyhit%  = 29% then       L11430
            if keyhit% <>  0% then       summary_screen
            if cursor%(1%) < 11% then    edit_short_hdr
L11280:     c% = top% + cursor%(1%) - 11%
            if c% < 1% or c% > maxlines% then summary_screen
            if keyhit% <> 18% then goto L11300
                gosub show_part_master_data
                goto summary_screen
L11300:     if str(lsts$(c%),,3) <> "DEL" then L11370
                keyhit1% = 2%
                call "ASKUSER" (keyhit1%, "REACTIVATE LINE",             ~
                      "Enter PF-16 to Reactive Deleted Line Item",       ~
                      "- OR - ", "Hit any PF-Key to Return to Display.")
                if keyhit1% <> 16% then summary_screen
                str(lsts$(c%),,3) = "RES"
L11370:     savetop% = top%
            top% = max(1%, c% - 1%)
            gosub edit_line_item
            top% = savetop%
            goto summary_screen

L11430:     fieldnr% = val(str(hdr_map$(cursor%(1)), cursor%(2)))
            if fieldnr% < 3% or  fieldnr% > 11% then summary_screen
            gosub'049(1%, fieldnr%)
            goto summary_screen

        mod_all_dates
            inputmsg$ = "To Reset ALL Line Item's Required Ship Date."
            k% = 0% : d% = 0%
            call "ASKDATE" (k%, "CHANGE ALL SHIP DATES!", inputmsg$,     ~
                     date, "20991231", newdate$, d%)
            if k% <> 0% then summary_screen

            for z% = 1% to maxlines%
                shipdate$(z%) = newdate$
                call "DATEFMT" (shipdate$(z%))
            next z%
            goto summary_screen


        delete_order
            if maxlines% > 0% then L11640
                errormsg$ = "There are no lines to delete!"
                return
L11640:     u3% = 2%
            call "ASKUSER" (u3%, "DELETE ORDER",                         ~
                "Enter PF-16 to DELETE the entire Sales Order",          ~
                "-OR- Press (RETURN) to EXIT Delete Function",           ~
         "Note: Scheduled, Shipped, Invoiced Lines will not be deleted.")
            if u3% <> 16% then return
                left% = 0%
                for c% = 1% to maxlines%
                     if str(lsts$(c%),,3) = "DEL" then L11770
                     if qtyschld(c%) = 0 then L11751
L11740:                   left% = left% + 1%
                          goto L11770
L11751:              if ship  (c%) = 0 then L11752 else L11740
L11752:              if preinv(c%) = 0 then L11760 else L11740
L11760:              str(lsts$(c%),,3) = "DEL"
L11770:         next c%
                if left% <> 0% then errormsg$ =                          ~
         "Note: Scheduled, Shipped, Invoiced Lines will not be deleted."
                if left% <> 0% then return
                     return clear
                     goto datasave


        cancel_order
                                                      /* (EWD) - Begin */
            errormsg$ = " "                           
            return                          /* Cannnot Cancell Orders  */  
                                                      /* (EWD) - End   */
             if oldflag% = 1% then L11855
                 errormsg$ = "This Order has NOT been Saved, therefore "&~
                             "it cannot be Cancelled."
                   goto L11100
L11855:      if crhold$ <> "C" then L11870
                 errormsg$ = "Order already cancelled"
                   goto L11100
L11870:      if maxlines% = 0% then  L11915
               for c% = 1 to maxlines%
                 if ship(c%) = 0 then L11895
                 goto cancel_lines
L11895:          if qtyschld(c%) = 0 then L11906
                 goto cancel_lines
L11906:          if preinv(c%)   = 0 then L11910
                 goto cancel_lines
L11910:        next c%
L11915:      u3% = 2%
             call "ASKUSER" (u3%, " ** CANCEL ORDER ** ",                ~
                             "Press PF10 to cancel order", "- or -",     ~
                             "PF16 to abort cancellation.")
             if u3% <> 10% and u3% <> 16% then L11915
             if u3% = 16% then L11100
             crhold$ = "C"
               for c% = 1 to maxlines%
                 openqty(c%) = 0
               next c%
             goto datasave

        cancel_lines
L11971:      u3% = 2%
             call "ASKUSER" (u3%, " ** CANCEL LINES ** ",                ~
                             "Some Line Items have open BOLs or have " & ~
                             "been Shipped & Invoiced.",                 ~
                             "Press PF10 to set the Open Quantity to " & ~
                             "the BOL Quantities", "- or -  Press PF16"& ~
                             " to abort cancellation.")
             if u3% <> 10% and u3% <> 16% then L11971
             if u3% = 16% then L11100
               for c% = 1 to maxlines%
                 openqty(c%) = qtyschld(c%) + preinv(c%)
               next c%
             goto datasave

        REM *************************************************************~
            *        H E A D E R   S C R E E N  -  D E T A I L          *~
            * --------------------------------------------------------- *~
            * Full Detail Header Screen.                                *~
            *************************************************************

        header_detail
            short% = 0% /* Validate all 23 Header fields */
            gosub'050(2%, 0%, 2%)       /* Set input message           */
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  errormsg$ = " "
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  9 then       variable_fields
                  if keyhit%  = 16 then       summary_screen
                  if keyhit%  = 25 then gosub edit_text_hdr
                  if keyhit%  =  0 then       L12230
                  if keyhit% <> 29 then       header_detail
                fieldnr% = val(str(hdr_map$(cursor%(1)), cursor%(2)))
                if fieldnr% < 3% or  fieldnr% > 23% then header_detail
                gosub'049(1%, fieldnr%)
                goto header_detail
L12230:     gosub'050(2%, 1%, 2%)       /* Set input message           */
L12235:     gosub'102(1%, 2%)           /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12235
            gosub'151(2%, 2%)           /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L12235
                          goto header_detail

        variable_fields
            call "VFINPSUB" ("BCKMASTR", "E", "On-Line Order Entry   ",  ~
                             "Customer: " & cuscode$ & "  SO: " & so$,   ~
                             "YN", vf$, keyhit%)
            if keyhit% =  1% then startover2
            if keyhit% =  4% then header_detail
            if keyhit% = 16% then datasave
                             goto header_detail

        REM *************************************************************~
            *        H E A D E R   S C R E E N  -  S H O R T            *~
            * --------------------------------------------------------- *~
            * Short Header Screen.                                      *~
            *************************************************************

        edit_short_hdr
            short% = 1% /* Validate 1st 13 Header fields */
            gosub'050(1%, 2%, 2%)       /* Set input message           */
L12720:     gosub'101(2%, 2%)           /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L12720
            gosub'151(2%, 2%)           /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L12720
                          goto summary_screen

        REM *************************************************************~
            *              L I N E   I T E M  S C R E E N S             *~
            * --------------------------------------------------------- *~
            * Routines for appending and modifying Line Items.          *~
            *************************************************************

        appendlines
L13070:     if maxlines%  = 100% then L13180
            if lastseq%  >= 999% then L13180
            if maxlines% + total_e_lines% <  100% then L13090
               gosub extra_load_conflict

L13090:         c%   = maxlines% + 1%
                top% = max(1%, c% - 2%)
                gosub clear_line
                lsts$(c%) = "ADD"
                convert lastseq% + 1% to seq$(c%), pic(###)
                gosub inputline
                     if keyhit% <> 16% then L13070
                gosub clear_line
L13180:         top% = max(1%, min(91%,maxlines%-9%))  /* Last Screen   */
                goto summary_screen

        inputline
            clear%  = 1%                          /* For Options Entry */
            orderqtysave = -1            /* Set for BCKMINSB avoidance */
            for fieldnr% = 1% to 2%          /* First Line Item Screen */
                gosub'050(3%, fieldnr%, 1%)
L13280:         gosub'103(fieldnr%, 1%)
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  =  2% then gosub restart_line
                      if keyhit% <>  6% then goto L13390
                          gosub replicate_prev_line
                          if e_warn% <> 0% then inputline
                          goto L13460   /* Assume as valid as prev line */
L13390:               if keyhit%  = 16% and fieldnr% = 1% then return
                      if keyhit%  = 18% then gosub show_part_master_data
                      if keyhit%  =  9% then size_run
                      if keyhit%  = 11% then date_run
                      if keyhit%  = 22% or keyhit% = 23% then L13410
                      if keyhit% <>  0 then       L13280
L13410:         gosub'153(fieldnr%, 1%)
                      if errormsg$ <> " " then L13280
                      mostin% = max(mostin%, fieldnr%)
            next fieldnr%

L13460:     maxlines% = maxlines% + 1%     /* Officially welcome line  */
            lastseq%  = lastseq%  + 1%     /* to the Sales Order       */
            convert lastseq% to seq$(c%), pic(###)
            append% = 1%
            gosub edit_line_item
            append% = 0%
            keyhit% = 99%
            return

        edit_line_item
            gosub describe_line
L13600:     gosub'050(3%, 0%, 2%)
            gosub'103(0%, 2%)
                  errormsg$ = " "
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  6% then gosub prev_line
                  if keyhit%  =  7% then gosub next_line
                  if keyhit%  = 12% then gosub delete_line
                  if keyhit%  = 16% then       return
                  if keyhit%  = 18% then gosub show_part_master_data
                  if keyhit%  = 25% then gosub edit_text_line
                  if keyhit%  =  0% then       L13905
                  if keyhit% <> 29% then       edit_line_item
                fieldnr% = val(str(line_map$(cursor%(1)), cursor%(2)))
                if fieldnr% <> 1% then L13860
                     clear% = 0% :  gosub options
                     goto L13600
L13860:         if fieldnr% < 2% or fieldnr% > 25% then L13600
                gosub'049(2%, fieldnr%)
                goto L13600
L13905:     gosub'050(3%, 2%, 2%)
L13910:     gosub'103(2%, 2%)
                  if keyhit%  =  1 then gosub startover
                  if keyhit% <>  0 then L13910
            gosub'153(2%, 2%)     /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13910
                     goto edit_line_item


        prev_line
            if append% = 1% or c% = 1% then return
                nc% = c%        /* Find 1st Previous Active Line Item  */
L14040:         nc% = nc% - 1%
                if nc% = 0% then return
                if str(lsts$(nc%),,3) = "DEL" then L14040
                     goto L14160


        next_line
            if append% = 1% or c% = maxlines% then return
                nc% = c%       /* Find Next Active Line Item           */
L14130:         nc% = nc% + 1%
                if nc% > maxlines% then return
                if str(lsts$(nc%),,3) = "DEL" then L14130
L14160:              c%   = nc%
                     top% = max(1%, c% - 1%)
                     return clear
                     goto edit_line_item

        delete_line
           if qtyschld(c%) = 0 then L14231
             errormsg$ = "Can't delete a Scheduled Line Item"    : return
L14231:    if preinv(c%)   = 0 then L14234
             errormsg$ = "Can't delete a Pre-Invoiced Line Item" : return
L14234:    if ship  (c%)   = 0 then L14240
             errormsg$ = "Can't delete a Shipped Line Item"      : return
L14240:     u3% = 2%
            call "ASKUSER" (u3%, "DELETE LINE",                          ~
                            "Press PF-16 to DELETE this Line Item",      ~
                            "-OR-", "Press (RETURN) to abort Delete.")
            if u3% <> 16% then return
                str(lsts$(c%),,3) = "DEL"
                return clear
                return

        replicate_prev_line
            if c% < 2% then return

                e_warn%        = 0%
                total_e_lines% = total_e_lines% - e_lines%(c%)
                e_lines%(c%)   = 0%

                part$     (c%) = part$     (c%-1%)
                refpart$  (c%) = refpart$  (c%-1%)
                refdesc$(c%)   = refdesc$  (c%-1%)
                reftype$(c%)   = reftype$  (c%-1%)
                descr$    (c%) = descr$    (c%-1%)
                refdesc$  (c%) = refdesc$  (c%-1%)
                nonstockmsg$(c%) = nonstockmsg$(c%-1%)
                cat$      (c%) = cat$      (c%-1%)
                duedate$  (c%) = duedate$  (c%-1%)
                shipdate$ (c%) = shipdate$ (c%-1%)
                stkuom$   (c%) = stkuom$   (c%-1%)
                pricestk  (c%) = pricestk  (c%-1%)
                call "CONVERT" (pricestk(c%-1%), 4.4, pricestk$)
                priceuom$ (c%) = priceuom$ (c%-1%)
                conv      (c%) = conv      (c%-1%)
                call "CONVERT" (conv(c%-1%), -7.7, conv$)
                order     (c%) = order     (c%-1%)
                call "CONVERT" (order(c%-1%), -2.2, order$)
*              ALLOC$    (C%) = ALLOC$    (C%-1%)
                call "READ100" (#04, part$(c%), f1%(4%))
                if f1%(4%) <> 0% then get #04, using L14446, alloc$(c%)
L14446:              FMT POS(242), CH(1)
                if alloc$(c%) = " " then alloc$(c%) = allocdflt$
                openqty   (c%) = openqty   (c%-1%)
                call "CONVERT" (openqty(c%-1%), -2.2, openqty$)
                lot$      (c%) = lot$      (c%-1%)
                ship      (c%) = 0
                call "CONVERT" (ship(c%), -2.2, ship$)
                price     (c%) = price     (c%-1%)
                pm_base(c%)    = price(c%)
                call "CONVERT" (price(c%-1%), -4.4, price$)
                linedisc  (c%) = linedisc  (c%-1%)
                call "CONVERT" (linedisc(c%-1%), -2.2, linedisc$)
                taxable$  (c%) = taxable$  (c%-1%)
                item$     (c%) = item$     (c%-1%)
                project$  (c%) = project$  (c%-1%)
                salesacct$(c%) = salesacct$(c%-1%)
                discacctl$(c%) = discacctl$(c%-1%)
                shipcode$ (c%) = shipcode$ (c%-1%)
                priority$ (c%) = priority$ (c%-1%)
                demtype$  (c%) = demtype$  (c%-1%)
                gosub get_atc

                call "ARIEXTRA" (cuscode$, part$(c%), " ",               ~
                                 e_lines%(c%), #2)
                total_e_lines% = total_e_lines% + e_lines%(c%)
                if c% + total_e_lines% <= 100% then L14675
                   temp% = 0%
                   gosub extra_append_conflict
                     if u3% = 16% then return
                        gosub clear_line
                        e_warn% = 1%
L14675:         return

        show_part_master_data        /* It's a call to HNYMSTSB is all */
            if part$(c%) = " " then return
            call "HNYMSTSB" (part$(c%), #02/* SYSFILE2 */,               ~
                #04/* HNYMASTR */, #11/* CATEGORY */, #03/* GLMAIN   */, ~
                #33/* HNYALTRS */, #34/* VENDOR   */, #35/* HNYPROC  */, ~
                #36/* VENPRICE */, #15/* HNYGENER */, #24/* PIPMASTR */, ~
                #20/* ENGMASTR */, #25/* PIPIN    */, #26/* PIPOUT   */, ~
                #13/* HNYQUAN  */, #28/* SFCUM2   */, #22/* TXTFILE  */, ~
                #08/* GENCODES */, #16/* BOMMASTR */)
            return

        REM *************************************************************~
            *          M I S C   S U P P O R T   R O U T I N E S        *~
            * --------------------------------------------------------- *~
            * Routines which apply to most of the screen functions.     *~
            *************************************************************

        call_customer_credit
            if cuscode$ = " " then return
            call "ARQCUSCR" (cuscode$)
            return


        deffn'040(txt$)
            fac25$ = hex(84) /* Highlight PF(25)Manage Text */
            if txt$ = hex(00000000) or txt$ = hex(ffffffff) or txt$ = " "~
                then fac25$ = hex(8c) /* Dim PF(25)Manage Text */
            return

        edit_text_hdr
            call "TXTINSUB" (#22, f2%(22%), "013", str(line1$,,19%),     ~
                textid$, text$())
            return


        edit_text_line
            call "TXTINSUB" (#22, f2%(22%), "014", str(line1$,,19%),     ~
                textidl$(c%), text$())
            return


        describe_line     /* Add descriptors to already existing line  */
            call "CONVERT" (order   (c%), -2.2, order$   )
            call "CONVERT" (openqty (c%), -2.2, openqty$ )
            call "CONVERT" (ship    (c%), -2.2, ship$    )
            call "CONVERT" (conv    (c%), -7.7, conv$    )
            call "CONVERT" (qtyschld(c%), -2.2, qtyschld$)
            call "CONVERT" (alloc   (c%), -2.2, allocqty$)
            call "CONVERT" (preinv  (c%), -2.2, preinv$  )
            allocsave$ = alloc$(c%)
            gosub pricing_stuff
            gosub check_for_options

            call "DATUNFMT" (shipdate$(c%))
            call "DATE" addr("GD", str(shipdate$(c%),,6), weekday$, u3%)
            call "DATEFMT"  (shipdate$(c%))

         /* Load and Describe the Demand for this Line Item            */
            pcd$, lastplan$, demstatus$ = " "
            if crhold$ <> "H" then demstatus$ = "1"
            demstatusmsg$ = "No Demand on File"
            plan$ = "Plan: " & demstatusmsg$
            readkey$ = str(so$) & str(seq$(c%))
            call "REDALT0" (#23, readkey$, 1%, demand%)
            if demand% = 0% then return
                get #23 using L16420, demstatus$, bom$(c%),               ~
                                              lastplan$, pcd$
L16420:              FMT CH(1), POS(68), CH(3), POS(77), 2*CH(6)
                call "DATEFMT" (lastplan$)
                call "DATEFMT" (pcd$     )

              /* Determine Demand Status, construct Plan info line.    */
                on pos(" 16789" = demstatus$) goto L16500, L16510, L16520,  ~
                                                   L16530, L16540, L16550
                   demstatusmsg$ = "Unknown Status        " : goto L16560
L16500:            demstatusmsg$ = "Unplanned, Unapproved " : goto L16560
L16510:            demstatusmsg$ = "Unplanned, Approved   " : goto L16560
L16520:            demstatusmsg$ = "Planned Late, Unapprvd" : goto L16560
L16530:            demstatusmsg$ = "Planned Late, Approved" : goto L16560
L16540:            demstatusmsg$ = "Planned Ok, Unapproved" : goto L16560
L16550:            demstatusmsg$ = "Planned OK, Approved  " : goto L16560
L16560:     plan$ = "Plan: " & demstatusmsg$ & "."
            if demstatus$ < "6" then return
                plan$ = plan$ & "  Planned on " & lastplan$ &            ~
                                "; P.C.D is " & pcd$ & "."
                return

        pricing_stuff
*        Calculates dependant variables (for line C%) and formats
*        the fields related to pricing of the line item.
                                                   /* (EWD) - Begin     */
            if sp% > 0% and p1 > 0.1 and edit% = 1% then                 ~
                                          price(c%) = p1   /*SPEC PRICE*/
                                                   /* (EWD) - End       */
            gosub adj_for_pm_surcharge  /*If PM effected part adjust the*/
                                    /* Price for PM Surcharge as Needed */
            pricestk(c%) = round(price(c%) / conv(c%)      , 4)
            ext          = round(price(c%) * openqty(c%) / conv(c%), 2)
            discamt      = round(ext * linedisc(c%) *.01   , 2)
            ext          = round(ext - discamt             , 2)
            call "CONVERT" (price(c%)   ,-4.4, price$   )
            call "CONVERT" (linedisc(c%),-2.2, linedisc$)
            call "CONVERT" (ext         , 2.2, ext$     )
            call "CONVERT" (discamt     , 2.2, discamt$ )
            call "CONVERT" (pricestk(c%), 4.4, pricestk$)
            return


        clear_line   /* Clear variables for line C%                    */
            part$(c%), descr$(c%), item$(c%), cat$(c%), order$, openqty$,~
            ship$, stkuom$(c%), priceuom$(c%), linedisc$, taxable$(c%),  ~
            nonstockmsg$(c%), pricestk$, discamt$, ext$, duedate$(c%),   ~
            shipdate$(c%), origdue$(c%), priority$(c%), demtype$(c%),    ~
            salesacct$(c%), discacctl$(c%), lot$(c%), bom$(c%), conv$,   ~
            price$, weekday$, errormsg$, qtyschld$, project$(c%),        ~
            lsts$(c%), allocqty$, preinv$, seq$(c%), alloc$(c%),         ~
            filler$(c%), pcd$, lastplan$, demstatus$, demstatusmsg$,     ~
            allocsave$, refpart$(c%), refdesc$(c%), mfgcode$,            ~
            mlquote_seq$(c%), shipcode$(c%), atc$(c%,1%), atc$(c%,2%),   ~
            parttype$(c%), reftype$(c%) = " "

            order(c%), openqty(c%), ship(c%), price(c%), pm_base(c%),    ~
            pricestk(c%), linedisc(c%), alloc(c%), preinv(c%),           ~
                qtyschld(c%) = 0
            pm_adj%(c%) = 0%
            conv(c%) = 1

            mat opdlrs = con
            mat opdlrs = (-1) * opdlrs

            textidl$(c%) = all(hex(ff))

            total_e_lines% = total_e_lines% - e_lines%(c%)
            e_lines%(c%), mlquote_cut%(c%)                           = 0%

            return


        options /* Call Options Processing Subroutine                  */
            if so$ = " " then gosub assign_so_number

            get #01 using L17200, opc$,   custype$
L17200:        FMT POS(734), CH(1), POS(1023), CH(2)
            opdlrs(3) = price(c%)
               if opc$ = " " then opc$ = pc$
         return                          /* (EWD) - Mod Not Used       */         
            call "BOMOPSUB"                                              ~
                    (clear%,             /* Clear B4 Input (0=N 1= Y)  */~
                     part$(c%),          /* Part Number                */~
                     str(so$) & str(seq$(c%)),     /* Demand Code      */~
                     shipdate$(c%),      /* Date Required              */~
                     bom$(c%),           /* BOM ID                     */~
                     cuscode$,           /* Customer                   */~
                     custype$,           /* Customer Type              */~
                     cat$(c%),           /* Part Category              */~
                     opc$,               /* Price Code                 */~
                     orderdate$,         /* Order Date                 */~
                     opdlrs(),           /* Cost, Prices Info          */~
                     #04,                /* 'HNYMASTR'                 */~
                     #16,                /* 'BOMMASTR'                 */~
                     #17,                /* 'BOMSPEC'                  */~
                     #02,                /* 'SYSFILE2'                 */~
                     #20,                /* 'ENGMASTR'                 */~
                     #18,                /* 'BOMSPHDR'                 */~
                     u3%)                /* 0 = ALL OK, 1 = ERROR      */
            clear% = 0%
            price(c%) = opdlrs(4)

            gosub pricing_stuff
            return


        check_for_options
            str(lsts$(c%),6) = " "
            readkey$ = all(hex(00))
            str(readkey$,,19) = str(so$) & seq$(c%)
            call "PLOWALTS" (#17, readkey$, 1%, 19%, f1%(17))
            if f1%(17) = 1% then str(lsts$(c%),6) = "O"
            return

        set_summary
*        Defines Contents and FACs for Summary Window.  WNDW% defines
*        length.
            init(" ") smry$()
            if wndw% = 3% then init(hex(8c)) sfac$() else                ~
                               init(hex(86)) sfac$()
            top% = max(1%, min(top%, 101% - wndw%))
            if wndw% = 3% then sfac$(c% - top% + 1%) = hex(84)
            sfac$(wndw%) = or hex(20)

            for s% = 1%  to  wndw%
              s1% = top% + s% - 1%       /* Line Number */
              if seq$(s1%) = " " then L17740
                str(smry$(s%), 1) = seq$  (s1%)
                if cms% = 1% then                                        ~
                    str(smry$(s%),5%) = part$(s1%) else                  ~
                    str(smry$(s%),5%) = refpart$(s1%)
                call "CONVERT" (order(s1%)  , 2.2, str(smry$(s%),31,10))
                call "CONVERT" (openqty(s1%), 2.2, str(smry$(s%),42,10))
                  if conv(s1%) = 0 then L17680
                temp  = round(openqty(s1%) * price(s1%) / conv(s1%),2)
                  goto L17690
L17680:         temp  = round(openqty(s1%) * pricestk(s1%), 2)
L17690:         temp1 = round(temp * linedisc(s1%) * .01  , 2)
                temp  = round(temp - temp1                , 2)
                call "CONVERT" (temp        , 2.2, str(smry$(s%),53,10))
                str(smry$(s%),64) = shipdate$(s1%)
                str(smry$(s%),73) = lsts$(s1%)
L17740:     next s%
            return

        date_run
            convert seq$(c%) to cl%, data goto L18020
L18020:     call "DATRUNSB" (cuscode$, so$, cl%, c%, part$(c%-1%),       ~
                            orderdate$, dr_date$(), dr_qty$(),dr_ship$(),~
                            offsets%, #24, #04, #28, #19, #25, #26, #29, ~
                            #23, #27, ret%)
            if ret% = 32% then inputline              /* Abort Date Run */
            call "SHOSTAT" ("Creating Multiple Order Lines...")

            for i% = 1% to 36%
                if dr_date$(i%) = " " or dr_date$(i%) = blankdate$ then L18530

                part$(c%)        = part$(c%-1%)
                refpart$(c%)     = refpart$(c%-1%)
                refdesc$(c%)     = refdesc$(c%-1%)
                reftype$(c%)     = reftype$(c%-1%)
                descr$(c%)       = descr$(c%-1%)
                refdesc$(c%)     = refdesc$(c%-1%)
                nonstockmsg$(c%) = nonstockmsg$(c%-1%)
                cat$(c%)         = cat$(c%-1%)
                duedate$(c%)     = dr_date$(i%)
                shipdate$(c%)    = dr_ship$(i%)
                origdue$(c%)     = dr_date$(i%)
                stkuom$(c%)      = stkuom$(c%-1%)
                parttype$(c%)    = parttype$(c%-1%)
                pricestk(c%)     = pricestk(c%-1%)
                priceuom$(c%)    = priceuom$(c%-1%)
                conv(c%)         = conv(c%-1%)
L18220:         convert dr_qty$(i%) to order(c%), data goto L18220
                alloc$(c%)       = alloc$(c%-1%)
                convert dr_qty$(i%) to openqty(c%), data goto L18250
L18250:         lot$(c%)         = lot$(c%-1%)
                ship(c%)         = ship(c%-1%)
                price(c%)        = price(c%-1%)
                pm_base(c%)      = price(c%)
                pm_adj%(c%)      = pm_adj%(c%-1%)
                linedisc(c%)     = linedisc(c%-1%)
                taxable$(c%)     = taxable$(c%-1%)
                item$(c%)        = item$(c%-1%)
                project$(c%)     = project$(c%-1%)
                salesacct$(c%)   = salesacct$(c%-1%)
                discacctl$(c%)   = discacctl$(c%-1%)
                shipcode$(c%)    = shipcode$(c%-1%)
                priority$(c%)    = priority$(c%-1%)
                demtype$(c%)     = demtype$(c%-1%)
                lsts$(c%)        = lsts$(c%-1%)

                gosub get_atc

                convert cl% to seq$(c%), pic(##0)
                call "ARIEXTRA" (cuscode$, part$(c%), " ",               ~
                                                        e_lines%(c%), #2)
                total_e_lines% = total_e_lines% + e_lines%(c%)
                if c% + total_e_lines% <= 100% then L18470
                    gosub excess_lines
L18470:         if maxlines% = 100% then L18550
                    if lastseq% >= 999% then L18550
                maxlines% = maxlines% + 1%
                c% = c% + 1%  :  cl% = cl% + 1%
                lastseq% = lastseq% + 1%

L18530:     next i%

L18550:     top% = max(1%, min(91%,maxlines%-9%))  /* Last Screen   */
            goto summary_screen

        excess_lines
L18590: % Current No. of Lines: ###  (+ Implied Lines: ###) : Total #####
            put askmsg$(1%) using L18590, c%, total_e_lines%,             ~
                                         c% + total_e_lines%
            u3% = 2%
            call "ASKUSER" (u3%, "* * * MAXIMUM LINES CONFLICT * * *",   ~
                  askmsg$(1%),                                           ~
                 "Some of the Implied Lines may not be generated",       ~
                 "Press any PF key to confirm and continue.")
            return

        adj_for_pm_surcharge        /* If PM effected part adjust the   */
                                    /* Price for PM Surcharge as Needed */
            if pm_on$ <> "Y" then return
            if pm_so$ <> "Y" then return
            if abs(pm_base(c%) - price(c%)) < 0.0001 then return

            s_charge = 0
            qty = openqty(c%)
            call "PMCALSUB" (cuscode$, part$(c%),qty,0,1,"C", orderdate$,~
                             s_charge, " ", " ", pm_so$, pm_inv$, rslt%)

            if rslt% = 0% then return  /*No PM Associated with this Part*/

            /* 1st Check if MLQ */
            if mlquote_cut%(c%) = 0% then L18800  /* Jump over MLQ Stuff */
                readkey$ = str(mlquote_seq$(c%)) & hex(20)
                call "READ100" (#52, readkey$, f1%(52%))
                    if f1%(52%) = 0%  then L18800
                get #52 using L18765, pm_code$, pm_base, pm_price
L18765:           FMT POS(20), CH(10), POS(55), PD(14,7), PD(14,7)
                if pm_code$ <> " " then L18800
                s_charge = qty * (pm_price - pm_base)
                pm_base(c%), price(c%) = pm_price
                pm_adj%(c%) = 1%
                return
L18800:     /* Add the precious metal surcharge to the price */
            if qty = 0 then  return  /* No PM set for zero qty */
                price(c%) = price(c%) + s_charge * conv(c%) / qty
                pm_adj%(c%) = 1%
                pm_base(c%) = price(c%)

                return

        display_pm_surcharges

            qty = openqty(c%)
            call "PMCALSUB" (cuscode$, part$(c%),qty,0,1,"D", orderdate$,~
                             s_charge, " ", " ", pm_so$, pm_inv$, rslt%)
            return

        REM *************************************************************~
            *             S A V E   D A T A   O N   F I L E             *~
            *-----------------------------------------------------------*~
            * 1- Sum up Order and see if it is on Credit Hold.          *~
            *    ... Credit information is gathered by s/r BCKCRDSB.    *~
            * 2- Get any change data required.                          *~
            * 3- Get final save approval.                               *~
            *************************************************************

        datasave
            curtemp3 = 0
            curtemp4 = 0
*        FIRST add up the order and determine if it is on credit hold.
                                                  /* (EWD) - Begin     */
            mat tlines% = zer
            inpmessage$ = " "
            sp$ = "01"
            gosub check_credit
                                                  /* (EWD) - End       */
*        NOW, Display screen.
            pickprint$ = " "
                if dfltpick$ = "Y" then pickprint$(2%) = "X"
                if dfltbol$  = "Y" then pickprint$(3%) = "X"
                if dfltacks$ <>"N" then pickprint$(1%) = "X"
                gosub encode_pickprint

            if crhold$ = "C" then goto L19750
            if modfld% = 1% and manadjre$ = "Y" then goto L19750
L19660:     gosub'104(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  9 then       exit_datasave
                  if keyhit%  = 14 then       save_sales_order 
                  if keyhit%  = 16 then       save_sales_order
                  if keyhit% <>  0 then       L19660
L19750:     gosub'104(1%, 2%)          /* Display & Accept Screen     */
                  if keyhit%  =  1 then gosub startover
                  if keyhit%  =  9 then       exit_datasave
                  if keyhit% <>  0 then       L19750
            gosub'154(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L19750
                  goto L19660

        exit_datasave     /* Branch back to editing of order */
            call "READ100" (#01, cuscode$, f1%(1))
            goto summary_screen

L20000: REM *************************************************************~
            *     D E F A U L T S               F O R   P A G E   1     *~
            *************************************************************

            if fieldnr% <> 2% then return

*        Store                                 STORE$
            if store$ = " " then store$ = dfltstr$

L20080
*        Default Due Date                      DFLTDUE$
                if offset_due$ = " " and mlq_hdate% <> 0% then return
                if offset_due$ = " " then L20170
                     tempdate$ = orderdate$
                     call "DATUNFMT" (tempdate$)
                     call "DATE" addr ("G+", tempdate$, offsetd%,        ~
                                             dfltdue$, u3%)
                     call "DATEFMT" (dfltdue$)
                     if mlq_hdate% <> 0% then return

L20170
*        Default Req'd Ship Date               DFLTSHIP$
                if offset_shp$ = " " or ~
                      dfltdue$ = " " or dfltdue$ = blankdate$ then return
                     tempdate$ = dfltdue$
                     call "DATUNFMT" (tempdate$)
                     call "DATE" addr ("G+", tempdate$, -offsets%,       ~
                                             dfltship$, u3%)
                     call "DATEFMT" (dfltship$)

            return

L22000: REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   3     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  3  of Input. *~
            *************************************************************

*        Part Description                      DESCR$()
            if nonstockmsg$(c%) <> " " then goto L22420
                get #04 using L22390, descr$(c%), stkuom$(c%),            ~
                                     priceuom$(c%), conv(c%), cat$(c%),  ~
                                parttype$(c%),   alloc$(c%),demtype$(c%),~
                                     priority$(c%)
L22390:       FMT XX(25), CH(32), POS(74), CH(4), CH(4), PD(14,7), CH(4),~
               POS(180), CH(3),  POS(242), CH(1), CH(1), POS(334), CH(1)

L22420
*        Part Category                         CAT$()
            gosub BUILD_CATEGORY                      /* (EWD)    */
                       /* Gotten above */

*        Due Date                              DUEDATE$()
            duedate$(c%) = dfltdue$

*        Required Ship Date                    SHIPDATE$()
            shipdate$(c%) = dfltship$

*        Stocking Unit of Measure (Non-stock parts only) STKUOM$
                       /* Gotten above */

*        Original Order Quantity               ORDER$

*        Allocation Qty, Instructions          ALLOC$()
            if alloc$(c%) = " " then alloc$(c%) = allocdflt$
            allocsave$ = alloc$(c%)

*        Open Order Quantity                   OPENQTY$
            openqty$ = order$

*        Lot                                   LOT$()

*        Total Quantity Shipped                SHIP$

*        Currency code                         CURRENCY$

*        Pricing Unit of Measure               PRICEUOM$()
                       /* Gotten above */

*        Conversion Pricing -> Stocking        CONV$
            if nonstockmsg$(c%) <> " " then L22910
                       /* Gotten above */
                call "CONVERT" (conv(c%), 7.7, conv$)
                goto L23010
L22910:     if stkuom$(c%) = priceuom$(c%) then conv$ = "1"
            if stkuom$(c%) = priceuom$(c%) then L23010
                readkey$ = "UOMCONV  " & str(priceuom$(c%)) & "-" &      ~
                                         str(stkuom$  (c%))
                call "READ100" (#08, readkey$, f1%(8))
                if f1%(8) <> 0% then get #08 using L22980, conv$
L22980:                   FMT XX(24), CH(10)

L23010
*        Unit Price                            PRICE$  /* (EWD) - Mod */
*           if parttype$(c%)= "000" then L23110 /*skip pricing if generic*/
            gosub call_cprassgn

* L23110
*        Line Item Discount %                  LINEDISC$

*        Line Taxable? (Y/N)                   TAXABLE$()
            get #01 using L23160, custaxable$
L23160:         FMT XX(793), CH(1)
            parttaxable$ = " "
            if nonstockmsg$(c%) =" " then get #04 using L23190,parttaxable$
L23190:         FMT XX(126), CH(1)
            if parttaxable$ = "N" then taxable$(c%) = "N"
            if parttaxable$ = "Y" then taxable$(c%) = "Y"
            if parttaxable$ = " " then taxable$(c%) = custaxable$

*        PO Item                               ITEM$()

*        Shipping Priority Code                SHIPCODE$()
            if shipcode$(c%) <> " " then L23280
                get #1 using L23260, shipcode$
L23260:             FMT POS(733), CH(1)
                if shipcode$ = " " then shipcode$ = "3"
                shipcode$(c%) = shipcode$

L23280
*        Planning Priority Code                PRIORITY$()
                       /* Gotten above */
                if priority$(c%) < "A" or priority$(c%) > "Z" then       ~
                                                      priority$(c%) = "A"

*        Planning Demand Type                  DEMTYPE$()
            if demtype$(c%) = " " then  demtype$(c%) =  dflt_dem_type$
             demtype$(c%) = "1"                /* (EWD) - Mod          */
                                                      /* (EWD) - Begin */
*        Project Number                        PROJECT$() 

*        Sales Distr. Account                  SALESACCT$()
            if len(cat$(c%)) = 4 then salesacct$(c%) = acct$(1%)
 
*           call "HNYGLGET" (part$(c%), store$, lot$(c%), salesacct$(c%),~
*                            5%, #04, #13)
            if salesacct$(c%) <> " " then call "GLFMT" (salesacct$(c%))
*           if salesacct$(c%) <> " " then goto L23540
*               call "ARMGLGET" (1%, cuscode$, part$(c%), cat$(c%), " ", ~
*                                store$, " ", #02, #01, #04, #11, #03,   ~
*                                salesacct$(c%))
                if salesacct$(c%) = " " then salesacct$(c%) = dfltsales$


*        Sales Discounts Account               DISCACCTL$()
            if len(cat$(c%)) = 4 then discacctl$(c%) = acct$(2%)

*           if nonstockmsg$(c%) <> " " then L23590
*               call "ARMGLGET" (2%, cuscode$, part$(c%), cat$(c%), " ", ~
*                                store$, " ", #02, #01, #04, #11, #03,   ~
*                                discacctl$(c%))
            if discacctl$(c%) = " " then discacctl$(c%) = discacct$


*        Open Field

*        Open Field
            return

        call_cprassgn
            get #01 using L23710, custype$
L23710:         FMT XX(1022), CH(2)
            call "CPRASSGN" (cuscode$, custype$, part$(c%), cat$(c%),    ~
                pc$, orderdate$, currtype$, currency$, opdlrs(2),        ~
                order(c%), #02, #04, price(c%), linedisc(c%), errormsg$)

            if sp% = 2% then errormsg$ = " "            /* EDI Price */
            if errormsg$ <> " " then return
                gosub pricing_stuff
                return
                                                    /* (EWD) - End   */
        size_run
            if dfltdue$ = " " or dfltdue$ = blankdate$ then ~
               default_not_set_warning
            if dfltship$ = " " or dfltship$ = blankdate$ then ~
               default_not_set_warning

            max% = maxlines%
            p% = 0%
            limit% = dim(part$(),1)
            call "SZRINSUB" (1%, max%, passpart$(), passqty$(), default$,~
                             #2, #4, #13, #19, #23, #24, #25, #26, #27,  ~
                             #28, #29, limit%)
            if passpart$(1%) = " " then abort_size_run
            for i% = 1% to 72%
               if passpart$(i%) = " " then L24520   /* all done */

               c%   = maxlines% + 1%
               gosub clear_line

               part$(c%) = passpart$(i%)
               order$    = passqty$(i%)
               convert order$ to order(c%), data goto L24200
L24200:        convert order$ to openqty(c%), data goto L24220

L24220:        call "READ100" (#4, part$(c%), f1%(4%))
                   if f1%(4%) = 0% then L24520

               gosub L22000     /* Load up the defaults */
               origdue$(c%) = dfltdue$
               lsts$(c%) = "ADD"
               if price(c%) <= 0% then gosub price_warning

               call"NUMTEST"(linedisc$,-50,100,errormsg$,2.2,linedisc(c%))
               if errormsg$ =  " " then L24350
                  linedisc$ = "0.00" : linedisc(c%) = 0
                  errormsg$ = " "

L24350:        if default$ = " " then L24450
               if str(default$,1,1) <> " " then                          ~
                                    alloc$(c%) = str(default$,1,1)
               if str(default$,2,1) <> " " then                          ~
                                    taxable$(c%) = str(default$,2,1)
               if str(default$,3,1) <> " " then                          ~
                                    demtype$(c%) = str(default$,3,1)
               if str(default$,4,1) <> " " then                          ~
                                    priority$(c%) = str(default$,4,1)

L24450:        maxlines% = maxlines% + 1%  /* Officially welcome line  */
               lastseq%  = lastseq%  + 1%  /* to the Sales Order       */
               convert lastseq% to seq$(c%), pic(###)
               if maxlines% = 100% then L24520
               if lastseq% >= 999% then L24520
               next i%

L24520:        top% = max(1%, min(91%,maxlines%-9%))  /* Last Screen   */


               goto summary_screen

        abort_size_run
            goto L13280


        price_warning
            price(c%) = 0
            if p% = 1% then return
            k% = 2%
            call "ASKUSER" (k%, "* * * WARNING * * *",                   ~
              "A price cannot be automatically determined for one or",   ~
              " more of the line items.  Please review all prices after",~
            " line items are created.  Press any key to acknowledge.")
            p% = 1%
            return

        default_not_set_warning
            call "ASKUSER" (k%, "* * * ABORT * * *",                     ~
              "The Default Ship Date and/or Default Due Date not set",   ~
              "Set them and then try this function again.",              ~
              "Press any key to acknowledge.")
            goto abort_size_run

        REM *************************************************************~
            * Routines related to Multi-Line Quotation Cutover.         *~
            *************************************************************

        cutover_quote_from_top  /* Entire Sales Order from a Quotation */
            if appendquote% = 0%                                         ~
                then msg$ = hex(06) & "Select Quotation for Cutover to "&~
                     "Sales Order."                                      ~
                else msg$ = hex(06) & "Select Quotation to Append Line "&~
                     "Items from."
            mat incl = zer : mat descr = zer
            init (" ") incl$(), plowhdr$()
*          U3% = 2%                           /* Window at the bottom */
*          CALL "ASKUSER" (U3%, "*** QUOTE SELECTION CRITERION ***",    ~
*              "Press PF(4) to select Quote by Quote Number.", "Press "&~
*              "PF(8) to select Quote by Customer Code.", "Press PF(1)"&~
*              " to abort Quotation Cutover.")
*          IF U3% =  1% THEN RETURN                  /* Abort Cutover */
*          IF U3% =  4% THEN GOTO 25350 /* Select by Quotation Number */
*          IF U3% <> 8% THEN GOTO 25120   /* Select by Customer Code? */
*              PLOWHDR$(1%) = "  Customer  Name                       "&~
*                   "    Quote #  Description"
*              IF CUSCODE$ = "?" THEN CUSCODE$ = " "
*              READKEY$ = CUSCODE$
*              DESCR( 1%) =    1.09  : DESCR( 2%) =  1
*              DESCR( 3%) =  -10.30  : DESCR( 4%) = 11
*              DESCR( 5%) =   10.08  : DESCR( 6%) = 42
*              DESCR( 7%) = 1001.30  : DESCR( 8%) = 51
*              CALL "PLOWCODE" (#46, READKEY$, MSG$, 9000%, 1.3,        ~
*                   F1%(46%), PLOWHDR$(), 0, 1001, INCL(), INCL$(),     ~
*                   "d", " ", #01, DESCR())    /* MLQMASTR & CUSTOMER */
*              IF F1%(46%) <> 0% THEN GOTO 25500/* Quotation selected */
*                   ERRORMSG$ = HEX(00)          /* No selection made */
*                   RETURN

*        Select a Quotation by Quotation Number.
            quotenbr$ = " "                 /* No other value possible */
            plowhdr$(1%) = "  Quote #  Description                     "&~
                "Quote      Due     Open Amt  Expires"
            descr( 1%) =   10.08  : descr( 2%) =  1
            descr( 3%) = 1001.30  : descr( 4%) = 10
            descr( 5%) =  806.061 : descr( 6%) = 41
            descr( 7%) =  818.061 : descr( 8%) = 50
            descr( 9%) =  867.08  : descr(10%) = 59.1042
            descr(11%) = 1055.061 : descr(12%) = 70
            readkey$ = cuscode$
            call "PLOWCODE" (#46, readkey$, msg$, 9009%, 1.3, f1%(46%),  ~
                plowhdr$(), 0, 1001, incl(), incl$(), "d", " ", #46,     ~
                descr())                                   /* MLQMASTR */
            if f1%(46%) <> 0% then goto L25500    /* Quotation Selected */
                errormsg$ = hex(00)               /* No Selection Made */
                return

L25500
*        A Quotation has been selected- grab it & set up a Sales Order.
            mc% = 46% : lc% = 47% : cc% = 48%   /* MLQ file channel #s */
            mlq_line_date% = 0%
            get #46 using L25525, temp$, quotenbr$, curr2$, /* MLQMASTR */~
                                                           expdate$
L25525:         FMT CH(9), CH(8), POS(893), CH(4), POS(1055), CH(6)
            if expdate$ = " " or expdate$ = blankdate$ then L25540
            testdate$ = orderdate$
            call "DATUNFMT" (testdate$)
            if expdate$ >= testdate$ then L25540
            call "DATEFMT" (expdate$)
L25532:     u3% = 2%
            call "ASKUSER" (u3%, "*** QUOTE EXPIRATION WARNING ***",     ~
                "Quote Number " & quotenbr$ & " has Expired on " &       ~
                expdate$ & ".", "Press PF(8) to Continue With the Cut" & ~
                "over.", "Press PF(1) to Abort the Quotation Cutover.")
            if u3% = 1% then return
            if u3% = 8% then L25540
                goto L25532
L25540:     if curr2$ = " " then curr2$ = statutory$            /* JIC */
            if appendquote% = 0% then goto L25640
*        Test for and prohibit the mixing of different currencies.
                if curr2$ = currency$ then goto append_quote_lines /*OK*/
                     call "ASKUSER" (0%, "*** CAN'T MIX CURRENCIES ***", ~
                          "The Sales Order is in " & currency$ & " curr"&~
                          "ency.", "The Quote lines you wish to append "&~
                          "are in " & curr2$ & ".", "Press any PF key t"&~
                          "o acknowledge.")
                     return
L25640:     cuscode$ = temp$
            gosub get_header_fields

*        Now read & store the Quotation Line Items for the Sales Order.
            c%, maxlines% = 0%
        append_quote_lines
            plowkey3$ = quotenbr$ & hex(00)
L25730:     call "PLOWNEXT" (#47, plowkey3$, 8%, f1%(47%)) /* MLQLINES */
            if f1%(47%) = 0% then goto cutover_quote_exit     /* Done? */
            if maxlines% = dim(part$(),1%) then cutover_quote_exit/* ? */
                c%, maxlines% = maxlines% + 1%  /* No - Clr & Str Line */
                lastseq% = lastseq% + 1%
                gosub clear_line
                readkey$ = key(#47)
                str(lsts$(c%),1,3) = "ADD"
                gosub get_line_item_fields
                convert lastseq% to seq$(c%), pic(###)
                get #47 using L25810, oldline$
L25810:              FMT POS(18), CH(3)
                gosub cutover_options  /* for this line */
                gosub check_for_options
                gosub cutover_xref_parts
                put mlquote_seq$(c%) using L25830, quotenbr$, oldline$
L25830:              FMT CH(8), CH(3)
                mlquote_cut%(c%) = 1%/* Indicate 'Cutover this session'*/
                goto L25730

        cutover_quote_exit   /* Get out of here from here only, please */
            if mlq_line_date% = 0% then L25874
                u3% = 0%
                call "ASKUSER" (u3%, "*** QUOTE LINE DATE WARNING ***",  ~
                      "For at Least One Line Cutover From the Quote, " & ~
                      "Either or Both the Line 'Order", "Date' and/or "& ~
                      "'Required Ship Date' Were Defaulted to the "    & ~
                      "Header Dates.", "Press Any PF Key to Continue...")

L25874:     gosub adjust_currency_conversion
            custonly% = 0%                                      /* JIC */
            if cuscode$ <> " " then goto L25980
                errormsg$ = "You must select a valid Customer code."
                fieldnr%, custonly% = 1%  /* Gotta get a Customer Code */
                gosub'050(1%, fieldnr%, 1%)      /* Input msg, enables */
L25920:         gosub'101(fieldnr%, 1%)     /* Display & Accept Screen */
                call "GETSCRN" ("C", " ", cursor%(), 0%)
                if keyhit%  = 1% then gosub startover
                if keyhit% <> 0% then L25920
                gosub'151(fieldnr%, 1%)  /* Edit Field for Valid Entry */
                if errormsg$ <> " " then L25920
                custonly% = 0%           /* Got Customer Code OK, fine */
L25980:     call "DESCRIBE" (#01, cuscode$, cusdescr$, 0%, f1%(1%))
            if f1%(1%) <> 0% then goto L26020    /* Customer not found? */
                cusdescr$ = "Customer not on file"             /* Nope */
                goto summary_screen
L26020:     get #01 using L26030, poreqd$                   /* CUSTOMER */
L26030:         FMT POS(1020), CH(1)
            if po$ <> " " or poreqd$ <> "Y" then goto summary_screen
                errormsg$ = "PO is required for this Customer."
                fieldnr% = 2%      /* Header Screen 1 ancillary fields */
                gosub'050(1%, fieldnr%, 1%)      /* Input msg, enables */
                enabled%(6%) = 1%           /* Force the PO field Open */
L26080:         gosub'101(fieldnr%, 1%)     /* Display & Accept Screen */
                call "GETSCRN" ("C", " ", cursor%(), 0%)
                if keyhit%  = 1% then gosub startover
                if keyhit% <> 0% then L26080
                gosub'151(fieldnr%, 1%)  /* Edit Field for Valid Entry */
                if errormsg$ <> " " then goto L26080
                goto summary_screen

        cutover_options
            init(hex(00)) plowkey2$
            str(plowkey2$,1,8 ) = str(quotenbr$,,)
            str(plowkey2$,9,8 ) = hex(20)
            str(plowkey2$,17,3 ) = str(oldline$,1,3)
L26214:     call "PLOWALTS" (#49, plowkey2$, 1%,19%,f1%(49%)) /*MLQSPEC */
            if f1%(49%) = 0% then goto do_mlqsphdr
            get #49, using L26230, record$
L26230:        FMT CH(150)
            if so$ = " " then gosub assign_so_number
            str(record$, 57, 16) =  str(so$,1,16)
            str(record$, 73, 3) =  str(seq$(c%),1,3) /* new line seq */
            write #17, using L26230, record$
            str(lsts$(c%),6) = "O"
            goto L26214

        do_mlqsphdr
            init(hex(00)) plowkey2$
            str(plowkey2$,1,8 ) = str(quotenbr$,,)
            str(plowkey2$,9,8 ) = hex(20)
            str(plowkey2$,17,3 ) = str(oldline$,1,3)
            call "REDALT0" (#50, plowkey2$, 1%, f1%(50%)) /*MLQSPHDR*/
            if f1%(50%) = 0% then return
            get #50, using L26560, record$
L26560:        FMT CH(150)
            str(record$, 35, 19) =  str(so$,,) & str(seq$(c%),,)
            write #18, using L26230, record$
            return

        cutover_xref_parts
            call "PTUSEDSB" ("R", "MLQ ", quotenbr$, oldline$,           ~
                          refpart$(c%), refdesc$(c%), reftype$(c%), ret%)
            if ret% <> 0% then L26950
                refdesc$(c%), reftype$(c%) = " "
                refpart$(c%) = "** No Cross Reference **"
                return
L26950:     refflag% = 1%
            return

        REM *************************************************************~
            *        I N I T I A L I Z E   F O R   I N P U T            *~
            * --------------------------------------------------------- *~
            * Clear variables prior to input mode.                      *~
            *************************************************************
        init_for_input
            init(" ") errormsg$, inpmessage$, cuscode$, so$, po$,        ~
                      shipto$(), soldto$(), canceldate$, dfltdue$,       ~
                      dfltship$, howship$, fob$, shipinstr$(), pc$,      ~
                      orderdisc$, terms$, region$, salesman$(), comm$(), ~
                      dfltsales$, discacct$, part$(), descr$(), seq$(),  ~
                      cat$(), order$, openqty$, ship$, priceuom$(),      ~
                      conv$, price$, linedisc$, taxable$(), duedate$(),  ~
                      shipdate$(), priority$(), demtype$(), salesacct$(),~
                      discacctl$(), lot$(), project$(), lsts$(), vf$,    ~
                      nonstockmsg$(), stkuom$(), pricestk$, ext$,        ~
                      origdue$(), qtyschld$, weekday$, weekdayh$,        ~
                      alloc$(), item$(), adjrsn$, smry$(), allocqty$,    ~
                      orderdate$, filler$(), export$, currency$,         ~
                      convdate$, adjrsndescr$, origdate$, origuser$,     ~
                      allocsave$, crhold$, how_held$, atc$(), refpart$(),~
                      refdesc$(), mfgcode$, mlquote_seq$(), quotesave$,  ~
                      shipcode$(),scode$,dr_date$(),dr_qty$(),dr_ship$(),~
                      pm_so$, pm_inv$, pickprint$(),dfltacks$,reftype$(),~
                      keepcus$, cus$,                                    ~
                      custype$, cod$, bck$, sp$, keepso$
                                                    /* (EWD) - Mods   */
            allow_delete$ = allow_delete_save$
            store$ = "300"                          /* (EWD) - Mod    */
            cus_txt% = 0% : cus$ = "NO "            /* (EWD) - Mod    */

            init(hex(8c)) sfac$()
            nextbol%, top%, cms%, toggle%                            = 1%
            appendquote%, maxlines%, c%, lastseq%, modfld%, oldflag%,    ~
                soonfile%, custonly%                                 = 0%
            origopen = 0 : conveqv, convunt = 1
            mat comm%    = zer
            mat order    = zer
            mat openqty  = zer
            mat ship     = zer
            mat price    = zer
            mat pricestk = zer
            mat preinv   = zer
            mat qtyschld = zer
            mat alloc    = zer
            mat mlquote_cut% = zer
            smryh$ = smryhdr$

            code$ = "00"                                /* (EWD) - Mod */
            status% = 0%                                /* (EWD) - Mod */
            init (hex(ff)) textid$, textidl$(), cus_txt$ /* (EWD) Mode */
            call "TXTFUTIL" (#22, f2%(22), "INTL", textid$)
            call "ALLFREE"
            sc_grp$ = "Z"                               /* (EWD001)    */
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
            scr%(1, 2) =  2% : set%( 2) = 13%      /* Sales Order #    */
            scr%(1, 3) =  3% : set%( 3) =  2%      /* Ship-to          */
            scr%(1, 4) =  4% : set%( 4) =  2%      /* Store Code       */
            scr%(1, 5) = 45% : set%(45) =  2%      /* Export Flag      */
            scr%(1, 6) =  5% : set%( 5) =  2%      /* Customer PO      */
            scr%(1, 7) =  6% : set%( 6) =  2%      /* How Ship         */
            scr%(1, 8) =  7% : set%( 7) =  2%      /* FOB              */
            scr%(1, 9) =  8% : set%( 8) =  2%      /* Order Date       */
            scr%(1,10) =  9% : set%( 9) =  2%      /* Default Due Date */
            scr%(1,11) = 10% : set%(10) =  2%      /* Dflt Ship Date   */
            scr%(1,12) = 48% : set%(48) =  2%      /* PM Schrge SO flg */
            scr%(1,13) = 49% : set%(49) =  2%      /* PM Schrge INVflg */
            scr%(1,14) = 11% : set%(11) =  2%      /* Sold-to          */
            scr%(1,15) = 12% : set%(12) =  2%      /* Cancel Date      */
            scr%(1,16) = 13% : set%(13) =  2%      /* Price Code       */
            scr%(1,17) = 14% : set%(14) =  2%      /* Order Discount % */
            scr%(1,18) = 15% : set%(15) =  2%      /* Terms Code       */
            scr%(1,19) = 16% : set%(16) =  2%      /* Region           */
            scr%(1,20) = 17% : set%(17) =  2%      /* Salesmen / Split */
            scr%(1,21) = 18% : set%(18) =  2%      /* Sales Account    */
            scr%(1,22) = 19% : set%(19) =  2%      /* Sales Disc Acct  */
            scr%(1,23) = 20% : set%(20) =  2%      /* Shipping Instrs  */

            scr%(2, 1) = 21% : set%(21) = 13%      /* Part Code        */
            scr%(2, 4) = 22% : set%(22) =  2%      /* Part Descr       */
            scr%(2, 5) = 23% : set%(23) =  2%      /* Part Category    */
            scr%(2, 6) = 24% : set%(24) =  2%      /* Due Date         */
            scr%(2, 7) = 25% : set%(25) =  2%      /* Required Ship    */
            scr%(2, 8) = 26% : set%(26) =  2%      /* Stocking UOM     */
            scr%(2, 9) = 27% : set%(27) =  2%      /* Pricing  UOM     */
            scr%(2,10) = 28% : set%(28) =  2%      /* Conversion Fctr  */
            scr%(2, 2) = 29% : set%(29) =  2%      /* Order Qty        */
            scr%(2,11) = 30% : set%(30) =  1%      /* Alloc Qty        */
            scr%(2,12) = 31% : set%(31) =  2%      /* Open Qty         */
            scr%(2,13) = 32% : set%(32) =  2%      /* Dflt Lot         */
            scr%(2,14) = 33% : set%(33) = 10%      /* Quantity Shipped */
            scr%(2, 3) = 46% : set%(46) =  2%      /* Currency code    */
            scr%(2,15) = 34% : set%(34) =  2%      /* Unit Price       */
            scr%(2,16) = 35% : set%(35) =  2%      /* Disc Percent     */
            scr%(2,17) = 36% : set%(36) =  2%      /* Taxable?         */
            scr%(2,18) = 37% : set%(37) =  2%      /* PO Item Number   */
            scr%(2,19) = 38% : set%(38) =  2%      /* Project Number   */
            scr%(2,20) = 39% : set%(39) =  2%      /* Sales Account    */
            scr%(2,21) = 40% : set%(40) =  2%      /* Sales Disc Acct  */
            scr%(2,22) = 47% : set%(47) =  2%      /* Shipng Priority  */
            scr%(2,23) = 41% : set%(41) =  2%      /* Demand Priority  */
            scr%(2,24) = 42% : set%(42) =  2%      /* Demand Type      */
            scr%(2,25) = 43% : set%(43) = 99%      /* Open Field       */
            scr%(2,26) = 44% : set%(44) = 99%      /* Open Field       */
*       * Next available slot is #50

            call "ENABLSUB" ("INIT", "BCKFASTR", scr%(), set%(),         ~
                                                         0%, 0%, 0%, 0%)
            return

        REM *************************************************************~
            *           R E S E T   S O F T   E N A B L E S             *~
            * --------------------------------------------------------- *~
            * Allow User to modify enable settings.                     *~
            *************************************************************
        deffn'049(s%, f%)      /* Screen and Field Numbers             */
            if admin% <> 1% then return            /* Not Authorized   */
            call "ENABLSUB" ("MODIFY", "BCKFASTR", scr%(), set%(),       ~
                              s%, f%, 0%, 0%)
            return


        REM *************************************************************~
            *        I N P U T   M E S S A G E S  &  E N A B L E S      *~
            * --------------------------------------------------------- *~
            * Sets Enable Flag, Input Message, and Standard PF Keys.    *~
            *************************************************************
        deffn'050(s%, f%, edit%)  /* EDIT%: 1=Input Mode; 2=Edit Mode */
        if f% <> 0% then L28160
            if s% = 1% then inpmessage$ =                                ~
                "Position cursor to select a Line Item OR press an enab"&~
                "led PF key."
            if s% = 2% then inpmessage$ =                                ~
                "Press (RETURN) to edit Header data fields OR press an "&~
                "enabled PF key."
            if s% = 3% then inpmessage$ =                                ~
                "Press (RETURN) to edit Line Item fields OR press an en"&~
                "abled PF key."
            return

L28160
*        First Define the Input Message
            if custonly% = 0% then goto L28169
                inpmessage$ = "Enter a Customer Code or blanks to see a"&~
                     " list."
                goto L28220
L28169:     r% = pmp%(s%, f%)           /* Get sequential field number */
            z% = max(1%, s% - 1%)
            restore line = L28370, r%     /* Position for Read          */
            read inpmessage$             /* Read Input Message         */

L28220
*        Now set the Field Enable Flag
            mat enabled% = zer
            lo% = lofld%(s%, f%) : hi% = hifld%(s%, f%)
            for x% = lo% to hi%
                call "ENABLSUB" ("SET", "BCKFASTR", scr%(), set%(), z%,  ~
                     x%, edit%, enabled%(x%))
            next x%
            if s% = 3% and (c% <> 1% or maxlines% > 1% or curr$ <> "Y"   ~
                or edit% = 2%) then enabled%(3) =  0%
            if s% = 1% and pm_on$ <> "Y" then enabled%(12%) = 0%
            if s% = 2% and pm_on$ <> "Y" then enabled%(12%) = 0%
            if s% = 1% and pm_on$ <> "Y" then enabled%(13%) = 0%
            if s% = 2% and pm_on$ <> "Y" then enabled%(13%) = 0%

            return

L28370: data                                                             ~
        /* Screen 1 -- Main Screen                                     */~
         "Enter Customer Code (or Name) and/or Sales Order Number.",     ~
         "Enter/Edit Header data fields, then press (RETURN).",          ~
                                                                         ~
        /* Screen 2 -- Header Detail                                   */~
         "Enter/Edit Header data fields, then press (RETURN).",          ~
                                                                         ~
        /* Screen 3- Line Items                                        */~
         "Enter the Part Code, Order Quantity and Currency Code for this ~
        ~line item.",                                                     ~
         "Enter the Line Item-related data fields, then press (RETURN)."

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants.   *~
            *************************************************************
        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if copy1% = 1% then copy1% = 0%
            cms% = 1%
            if u3% = 1% then return

*        Wants to start order over
            return clear all
          startover2   /* Entry point for Start Over in VF subrtn    */
            readkey$ = all(hex(00))       /* Clear In-process Flag */
            str(readkey$,,3) = userid$
            call "DELETE" (#09, readkey$, 10%)
            gosub clear_all_options
            goto inputmode

        restart_line      /* Only allowed in Input Mode      */
L29150:     u3% = 2%
            call "ASKUSER" (u3%, "RESTART LINE",                         ~
                            "Press (RETURN) to RESTART Line Item",       ~
                            "- OR -", "Press PF-1 to EXIT Restart.")
            if u3% = 1% then return
                if u3% <> 0% then L29150
                     return clear
                     gosub clear_line_options
                     gosub clear_line_text
                     gosub clear_line
                     goto  appendlines

        clear_line_text
            call "TXTFUTIL" (#22, f2%(22), "XOUT", textidl$(c%))
            return

        clear_all_options  /* Clear options that will become orphans   */
            if maxlines% = 0% then return
                c8% = 1%  :  c9% = max(maxlines%, c%) /*Get the last one*/
                goto clear_options
        clear_line_options /* Clear any options entered for line C%    */
                c8%, c9% = c%  :  str(lsts$(c%),,3) = "ADD"
        clear_options
            if so$ = " " then return
            for c% = c8% to c9%
              if str(lsts$(c%),,3) <> "ADD" then L29350
                init(hex(00)) readkey$
                str(readkey$,,19) = str(so$) & str(seq$(c%))
                call "REDALT1"(#18, readkey$, 1%, f1%(18))  /* header */
                   if f1%(18) = 0% then L29310
                     delete #18

L29310:         call "PLOWAL1" (#17, readkey$, 1%, 19%, f1%(17))
                if f1%(17) = 0% then L29350
                     delete #17
                     goto L29310
L29350:     next c%
            return

        REM *************************************************************~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            *************************************************************

        REM *************************************************************~
            *             L O A D   S A L E S   O R D E R               *~
            * --------------------------------------------------------- *~
            * Load up the Sale Order if on file.  If the Order is not   *~
            * on file then set it up with all kinds of defaults.        *~
            *************************************************************
        load_sales_order
            soonfile% = 0%
            adjrsndescr$ = " "

*        First see if the order in question is in the master file
            readkey$  = str(so$) & hex(00)
            call "PLOWNEXT" (#06, readkey$, 16%, soonfile%)
            if soonfile% = 0% then allow_delete$ = "Y"
            if soonfile% = 0% then L30220
            get #06, tempcus$
            if cuscode$ = " " then cuscode$ = tempcus$
            if cuscode$ = tempcus$ then L30220
                errormsg$ = "Order already assigned to Customer " &      ~
                             tempcus$
                return

L30220
*        See if order is already in buffer.  If so can't touch
            call "REDALT0" (#09, so$, 2%, f1%(9))
            if f1%(9) = 0% then L30270
L30250:         errormsg$ = "Order is already being processed."
                return
L30270:     if cuscode$ = " " then return
            call "BCKPREFX" (so$, errormsg$)
            if errormsg$ <> " " then return

*        See if order number is already used as a demand code
            if soonfile% <> 0% then L30400
            readkey$ = all(hex(00))
            str(readkey$,1,16) = str(so$)
            call "PLOWALTS" (#23, readkey$, 1%, 16%, f1%(23))
            if f1%(23) = 0% then L30400
              errormsg$ = "Order Number is already used as a Demand Code"
              return

L30400
*        All Ok-fine.  If this is a new Order then set it up; if it is
*        an Old Order then load the data from the master files.
            if soonfile% = 0% then new_order_setup

            print at(09,02,79), hex(94) &                                ~
                                "Loading Sales Order from Master..."
            gosub lookup_so                          /* (EWD) - Mod  */   
            oldflag% = 1%
            readkey$ = " "
            str(readkey$, 1, 3) = userid$
            str(readkey$, 4, 7) = all(hex(00))
            str(readkey$,11, 8) = "BCKFASTR"
            str(readkey$,21,25) = str(cuscode$) & so$
            write #09 using L30540, readkey$, " ", " ", " ", " ",         ~
                                  eod goto L30250
L30540:         FMT CH(45), 3*CH(250), CH(225)

            call "READ100" (#01, cuscode$, f1%(1))  /* May have changed */
        copy_order  /* Entry point for copy function */
            mc% = 5% : lc% = 6% : cc% = 41%        /* Current Files    */
            if copy% <> 1% then L30620
            if copy_what% <> 31% then L30620
               mc% = 31% : lc% = 30% : cc% = 43%   /* History Files    */
L30620:     readkey$ = str(cuscode$) & so$
            call "READ100" (#mc%, readkey$, f1%(mc%))
            gosub get_header_fields

*        Now Read in line items from BCKBUF2
            c%, maxlines% = 0%
            readkey$ = str(so$) & hex(00)
L30690:     call "PLOWNEXT" (#lc%, readkey$, 16%, f1%(lc%))
            if f1%(lc%) = 1% then L30770
                if copy% = 1% and crhold$ = "C" then gosub copy_cancelled
                convert seq$(maxlines%) to temp%
                lastseq% = max(lastseq%, temp%)
                if maxlines% + total_e_lines% <= 100% then L30760
                   gosub extra_load_conflict
L30760:         return
L30770:     c%, maxlines% = c% + 1%
            gosub get_line_item_fields
            goto L30690

        copy_cancelled
L30820:     u3% = 2%
            call "ASKUSER" (u3%, "*** ATTENTION ***",                    ~
                           "This is a Cancelled Order!", "PF3 will copy"&~
                           " & restore the original quantities in the "& ~
                           "new order.", "PF16 will 'STARTOVER' without"&~
                           " copy.")
            if u3% <> 3% and u3% <> 16% then L30820
            if u3% = 3% then L30920
            return clear all
            goto startover2
L30920:     crhold$ = " "
            for c% = 1 to maxlines%
                openqty(c%) = order(c%)
            next c%
            return

        new_order_setup
*        Get all the Header Level Defaults that we can.  This section
*        eliminates the need for the Defaults Code Section.
            orderdate$ = date$
            get #01 using L31050, soldto$(), pm_cus_so$, pm_cus_inv$,     ~
                                            shipto$(), orderdisc, pc$,   ~
                                terms$, howship$, fob$, shipinstr$(),    ~
                                salesman$(), comm%(), region$, shipcode$,~
                                export$
L31050:         FMT POS(40), 6*CH(30), POS(226), 2*CH(1),                ~
                                       POS(253), 6*CH(30), POS(517),     ~
                    PD(14,4), CH(1), POS(543), 3*CH(20), POS(614),       ~
                    2*CH(50), 3*CH(4), 3*BI(1), CH(4), CH(1), POS(1091), ~
                    CH(1)
            if shipcode$ = " " then shipcode$ = "3"
            call "CONVERT" (orderdisc, 2.2, orderdisc$)
                                                  /* (EWD) - Begin    */
            howship$ = "00/00 - Our Truck   "
            fob$ = " "
                                                  /* (EWD) - End      */
            for i% = 1% to 3%
                if salesman$(i%) <> " " then                             ~
                                 convert comm%(i%) to comm$(i%), pic(##0)
            next i%
            if export$ = " " then export$ = "N" /* Default Export value */
            c%, lastseq%, maxlines% = 0%
            origopen, holdorig = 0

            gosub set_pm_flags

            return

        get_header_fields       /* MC% must be set before calling this */
            if mc% = 46% then hold% = lastseq%
            get #mc% using L35060, po$, shipto$(), soldto$(), terms$,     ~
                howship$, fob$, shipinstr$(), dfltsales$, discacct$,     ~
                salesman$(), comm%(), region$, vf$, textid$, store$,     ~
                orderdate$, canceldate$, dfltdue$, dfltship$,            ~
                origdate$, origuser$, lastchanged$, lastuser$, export$,  ~
                pc$, orderdisc, origopen, crhold$, lastseq%, nextbol%,   ~
                currency$, how_held$, pm_so$, pm_inv$
            if mc% = 46% then lastseq% = hold%
            if currency$ = " " then currency$ = statutory$
            gosub set_pm_flags
            if copy% = 1% then                                           ~
                    call "TXTFUTIL" (#22, f2%(22), "COPY", textid$) else ~
                    call "TXTFUTIL" (#22, f2%(22), "LOAD", textid$)
            holdorig  = origopen
            origopen = origopen - round((origopen * orderdisc * .01), 2)
            if copy% = 1% then origopen = 0
            call "GLFMT" (dfltsales$)
            call "GLFMT" (discacct$)
            for i% = 1% to 3%
                if salesman$(i%) = " " then L31380
                     convert comm%(i%) to comm$(i%), pic(##0)
L31380:     next i%
            if mc% = 46% then orderdate$ = date/* Use today for Quotes */
            call "DATEFMT" (orderdate$)
            call "DATEFMT" (canceldate$)
            call "DATEFMT" (dfltdue$)
            if dfltship$ <> " " and dfltship$ <> blankdate$ then          ~
                call "DATE" addr("GD", str(dfltship$,,6), weekday$, u3%)
            call "DATEFMT" (dfltship$)
            call "DATEFMT" (origdate$)
            call "DATEFMT" (lastchanged$)
            call "CONVERT" (orderdisc, 2.2, orderdisc$)
            if mc% = 46% then gosub mlq_header_date_check
            return

        get_line_item_fields             /* Pre-set LC%, CC%, READKEY$ */
            get #lc% using L35830, tempcus$, tempso$,                     ~
                seq$(c%), item$(c%), part$(c%), descr$(c%),              ~
                cat$(c%), order(c%), ship(c%), openqty(c%),              ~
                qtyschld(c%), alloc(c%), preinv(c%), pricestk(c%),       ~
                stkuom$(c%), priceuom$(c%), conv(c%), price(c%),         ~
                linedisc(c%), taxable$(c%), salesacct$(c%),              ~
                discacctl$(c%), origdue$(c%), duedate$(c%),              ~
                shipdate$(c%), lot$(c%), project$(c%), temp$,            ~
                demtype$(c%), priority$(c%), textidl$(c%), alloc$(c%),   ~
                invnbr$, estnbr$, bom$(c%), mlquote_seq$(c%),            ~
                shipcode$(c%), pm_adj%(c%), sc_grp$ ,filler$(c%)
                                                    /* (EWD001)   */
            pm_base(c%) = price(c%)

            if shipcode$(c%) <> " " then L31620
                if shipcode$ <> " " then L31618
                    get #1 using L31616, shipcode$
L31616:                 FMT POS(733), CH(1)
                    if shipcode$ = " " then shipcode$ = "3"
L31618:         shipcode$(c%) = shipcode$
L31620: REM Get Corresponding Customer Part Number...
            /* Test if Copy and Copy for same Customer */
            if keepcus$ = " " or keepcus$ = cuscode$ then L31624 else L31670
            /* Get Cross reference part from shadow file */
L31624:     scode$ = "BCK "
            if lc% = 30% then scode$ = "BCKH"
            if lc% = 47% then goto L31670
            call "PTUSEDSB" ("R", scode$, tempso$, seq$(c%),             ~
                          refpart$(c%), refdesc$(c%), reftype$(c%), ret%)

            if ret% = 1% then refflag% = 1% else                         ~
                 refpart$(c%) = "** No Cross Reference **"
L31670: REM Get transaction amounts from the BCK 'shadow' file (BCKLNCUR)
            if currency$ = statutory$ then goto L31770 /* Not Stat, tho */
            if curr$ <> "Y" then goto L31770 /* Nor if no multi-currency*/
            call "READ100" (#cc%, readkey$, f1%(cc%))
            if f1%(cc%) = 0% then goto L31770
            get #cc% using L31730, pricestk(c%), price(c%)
L31730:         FMT POS(24), 2*PD(14,4)
            pm_base(c%) = price(c%)
                if c% = 1% then get #cc% using L31760, currency$,         ~
                    convdate$, conveqv, convunt
L31760:             FMT CH(4), POS(40), CH(6), 2*PD(14,7)
L31770:     if copy% = 1% then                                           ~
                    call "TXTFUTIL" (#22, f2%(22), "COPY", textidl$(c%)) ~
                else                                                     ~
                    call "TXTFUTIL" (#22, f2%(22), "LOAD", textidl$(c%))
            call "DATEFMT" (origdue$ (c%))
            call "DATEFMT" (duedate$ (c%))
            call "DATEFMT" (shipdate$(c%))
            call "GLFMT"  (salesacct$(c%))
            call "GLFMT"  (discacctl$(c%))
            if qtyschld(c%) <> 0 then str(lsts$(c%),4,1) = "s"
            if preinv  (c%) <> 0 then str(lsts$(c%),5,1) = "p"
            gosub check_for_options
            call "READ100" (#04, part$(c%), f1%(4))
            if f1%(4) <> 0% then L31902
                nonstockmsg$(c%) = "NonStk"
                refpart$(c%) = "** No Cross reference **"
                goto  L31910
L31902:     get #4, using L31903, parttype$(c%)
L31903:         FMT POS(180), CH(3)
L31910:     if lc% = 47% then gosub mlq_lines_date_check
            gosub get_atc
            call "ARIEXTRA" (cuscode$, part$(c%), " ", e_lines%(c%), #2)
            total_e_lines% = total_e_lines% + e_lines%(c%)
            return

        REM *************************************************************~
            *             S A V E   S A L E S   O R D E R               *~
            * --------------------------------------------------------- *~
            * Write the Sales Order to the buffer file.  Update the     *~
            * Customer file with the credit dollars and then yell       *~
            * at the update to wake it up.                              *~
            *************************************************************

        save_sales_order
            s_fax$ = " " : check% = 0%              /* (EWD) - Begin   */
            if keyhit% = 14% then s_fax$ = "Y"
            if len(so$) < 8 then goto L32120        /* For the copy of */
                                                    /* Planned S.O.    */
            call "APCPL10B" (str(so$,1%,8%),cuscode$,po$,sp$,check%,#60)
L32120: REM IF CRHOLD$ = "H" THEN CHECK% = 1%       /* Can't Save S.O. */
            if s_fax$ = "Y" then gosub send_fax

        REM if origuser$ <> "EDI" then check% = 1%  /* (EWD002) Fix    */
  
            if check% = 1% then goto startover2     /* Exit Save Order */
            gosub assign_so_number
                                                    /* (EWD) - End     */ 
            call "SHOSTAT" ("Now Saving Sales Order " & so$)
            last_so$ = so$
            if curr$ <> "Y" then goto L32130
                currkey$ = str(so$) & hex(00)
                call "DELETE" (#41, currkey$, 16%)

L32130:     if maxlines% = 0% then L32450
            for c% = 1% to maxlines%
                gosub save_line
            next c%

L32450
*        Next write out the updated Header and move all text
            call "GETDTTM" addr(datetime$)
            gosub save_header
            call "TXTFUTIL" (#22, f2%(22), "SAV2", textid$)

*        Update the Customer(s) with the Open Order Dollars
          if abs(curtemp4 - origopen) < .01 then L32720
            call "READ100" (#01, cuscode$, f1%(1%))
            get #01 using L32540, billto$
L32540:         FMT POS(780), CH(9)
            topen(1%), topen(2%) = 0                            /* JIC */
            call "READ101" (#51, cuscode$, f1%(51%))       /* CCRMASTR */
            if f1%(51%) <> 0% then get #51 using L32544, topen(1), topen(2)
L32544:         FMT POS(114), 2*PD(14,4)
            if billto$ = cuscode$ then                                   ~
                topen(1) = topen(1) - origopen + curtemp4
                topen(2) = topen(2) - origopen + curtemp4
            if f1%(51%) = 0% then put #51 using L36590, cuscode$, 0, 0, 0,~
                " ", 0, " ", 0, " ", 0, 0%, " ", " ", " ", " ", " ", 0,  ~
                0, 0, 0, " ", " ", " "
            put #51 using L32590, date, topen(1), topen(2), userid$, date
L32590:         FMT POS(84), CH(6), POS(114), 2*PD(14,4), POS(146),      ~
                     CH(3), CH(6)
            if f1%(51%) = 0% then write #51 else rewrite #51

            if billto$ = cuscode$ then L32720
L32630:         call "READ100" (#01, billto$, f1%(1))
                if f1%(1) = 1% then L32640
                 billto$ = str(cuscode$) /* No Bill to for this Ship to */
                 goto L32630
L32640:     topen(1%), topen(2%) = 0                            /* JIC */
            call "READ101" (#51, billto$, f1%(51%))        /* CCRMASTR */
            if f1%(51%) <> 0% then get #51 using L32544, topen(1), topen(2)
            topen(1%) = topen(1%) - origopen + curtemp4
            if f1%(51%) = 0% then put #51 using L36590, billto$, 0, 0, 0, ~
                " ", 0, " ", 0, " ", 0, 0%, " ", " ", " ", " ", " ", 0,  ~
                0, 0, 0, " ", " ", " "
            put #51 using L32590, date, topen(1), topen(2), userid$, date
            if f1%(51%) = 0% then write #51 else rewrite #51

L32720
*        Write to Precious Metal Shadow File if needed

            if maxlines% = 0% then L32800
            for c% = 1% to maxlines%
                if pm_adj%(c%) <> 1% then L32760
                call "PMCALSUB" (cuscode$, part$(c%), openqty(c%),       ~
                                 price(c%), conv(c%), "S", orderdate$,   ~
                                 s_charge, "S",str(so$) & seq$(c%),      ~
                                 pm_so$, pm_inv$, rslt%)
L32760:     next c%

L32800
*        FINALLY, Delete Quotes per criteria; Crank up the update.
            if mlqdelt$ = "N" then goto L32810 /* System behavior sw=NO */
                for c% = 1% to maxlines%
                     gosub quote_deletion
                next c%
L32810:     u3% = 0%
            call "TASKUP" ("SO", u3%)
            goto inputmode

        quote_deletion/* Maybe delete some Quotes from that sub-module */
            if mlquote_seq$(c%) = " " then return/* Any Quote cutover? */
            if mlquote_cut%(c%) = 0%  then return /* C/O this session? */
            if str(mlquote_seq$(c%),,8%) = quotesave$ then return
            quotesave$ = str(mlquote_seq$(c%),,8%)/* Consider each once*/
            if mlqdelt$ = "Y" then goto L32900/* System behavior sw=YES */
*        MLQDELT$ must be 'A'- Ask the user whether to delete the Quote.
L32865:         u3% = 0%                       /* Window in the middle */
                call "ASKUSER" (u3%, "*** DELETE THIS QUOTE? ***",       ~
                     "Quote # " & quotesave$ & " was cutover to this S."&~
                     "O. DELETE the Quote?", "Press PF(12) to DELETE th"&~
                     "e Quote.", "Press (RETURN) to RETAIN the Quote.")
                if u3% =   0% then return
                if u3% <> 12% then goto L32865
L32900
*        Well, here we go, deleting the Quote from MLQMASTR/LINES/LNCUR.
            call "DELETE" (#46, quotesave$, 8%)   /* Bye-bye, MLQMASTR */
            call "DELETE" (#47, quotesave$, 8%)   /* Bye-bye, MLQLINES */
            if curr$ = "Y"                       /* Bye-bye, MLQLNCUR? */~
                then call "DELETE" (#48, quotesave$, 8%)
                if pm_adj%(c%) <> 1% then L32917
                call "PMCALSUB" (cuscode$, part$(c%), openqty(c%),       ~
                          price(c%), conv(c%), "R", " ", s_charge, "M",  ~
                          quotesave$, " ", " ", rslt%) /* Bye PM Shadow*/
L32917:  /* Del Xref Shadow */
            call "PTUSEDSB" ("D", "MLQ ", quotesave$, "ALL",             ~
                              " ", " ", " ", ret%)
*        And now clean out option files, starting with MLQSPEC
            init(hex(00)) plowkey2$
            str(plowkey2$,1,8 ) = str(quotesave$)
L32928:     call "PLOWAL1" (#49, plowkey2$, 1%, 8%,f1%(49%)) /*MLQSPEC */
            if f1%(49%) = 0% then return
            delete #49

*        And followed by MLQSPHDR
            init(hex(00)) readkey$
            str(readkey$,1,8 ) = str(mlquote_seq$(c%),,8%)
            str(readkey$,9,8 ) = hex(20)
            str(readkey$,17,3 ) = str(plowkey2$,17%,3%)

            call "REDALT1" (#50, plowkey2$, 1%, f1%(50%)) /*MLQSPHDR*/
            if f1%(50%) = 0% then L32928
            delete #50
            goto L32928

        REM *************************************************************~
            *                S A V E   H E A D E R                      *~
            * --------------------------------------------------------- *~
            * Save the Header for the Order being worked on.            *~
            *************************************************************
        save_header

*        Unformat Dates and G/L Accounts
            call "GLUNFMT"  (dfltsales$)
            call "GLUNFMT"  (discacct$ )
            call "DATUNFMT" (orderdate$ )
            call "DATUNFMT" (canceldate$)
            call "DATUNFMT" (dfltdue$   )
            call "DATUNFMT" (dfltship$  )
            call "DATUNFMT" (origdate$  )
            if origdate$ = " " or origdate$ = blankdate$ then origdate$ = date
            if origuser$ = " " then origuser$ = userid$

            readkey$ = all(hex(00))
            str(readkey$,,3) = userid$
            call "DELETE" (#09, str(readkey$,,10), 10%)
            str(readkey$, 4, 7) = datetime$
            str(readkey$,11   ) = pickprint$

            topen(1) = topen(1) + topen(2)
            curtemp4 = round(curtemp3 * orderdisc * .01, 2)
            curtemp4 = curtemp3 - curtemp4

            get #01 using L33350, acctxref$, custype$
L33350:         FMT POS(771), CH(9), POS(1023), CH(2)

            put #09 using L35410, str(readkey$,,20),                      ~
                cuscode$, so$, po$, shipto$(), soldto$(), terms$,        ~
                howship$, fob$, shipinstr$(), dfltsales$, discacct$,     ~
                salesman$(), comm%(), region$, vf$, textid$, store$,     ~
                orderdate$, canceldate$, dfltdue$, dfltship$,            ~
                origdate$, origuser$, date, userid$, adjrsn$, export$,   ~
                pc$, orderdisc, curtemp3, crhold$, lastseq%, nextbol%,   ~
                custype$, acctxref$, currency$, how_held$, pm_so$,       ~
                pm_inv$, " "
            if soonfile% = 0% then put #09 using L33480, " "
L33480:         FMT POS(859), CH(9)  /* Blank Change Audits       */

            write #09
            return


        assign_so_number
            if so$ = " " then call "BCKNEXT" (#12, #06, #10, #30, #23,   ~
                store$, so$)
            return

        set_pm_flags
            if pm_on$ <> "Y" then return
            if pm_so$ <> " " then L33725
                if pm_cus_so$  = " " then pm_so$  = pm_sys_so$           ~
                                 else pm_so$  = pm_cus_so$
L33725:     if pm_inv$ <> " " then return
                if pm_cus_inv$ = " " then pm_inv$ = pm_sys_inv$          ~
                                 else pm_inv$ = pm_cus_inv$
            return

        REM *************************************************************~
            *                    S A V E   L I N E                      *~
            * --------------------------------------------------------- *~
            * Save the Line Item (C%) to the Buffer file.               *~
            *************************************************************
        save_line
            if str(lsts$(c%),,3) <> "DEL" then L34243
                call "TXTFUTIL" (#22, f2%(22), "XOUT", textidl$(c%))
                saveseq$ = seq$(c%)  :  savepart$ = part$(c%)
                gosub clear_line
                seq$(c%) = saveseq$  :  part$(c%) = savepart$
                lsts$(c%)  = "DEL"     /* Flag as deleted- here   */
                descr$(c%) = hex(ff)   /* Flag as deleted- update */
*       RHH                               /* (EWD) Mod - 01/26/96 */
L34243:     if change% = 0% then goto L34260
                origdue$(c%), duedate$(c%) = dfltdue$
                shipdate$(c%) = dfltship$
*       RHH                               /* (EWD) - End          */

L34260:     call "DATUNFMT" (origdue$ (c%))
            call "DATUNFMT" (duedate$ (c%))
            call "DATUNFMT" (shipdate$(c%))
            call "GLUNFMT"  (salesacct$(c%))
            call "GLUNFMT"  (discacctl$(c%))

            if curr$ <> "Y" then goto L34660
            if currency$ = statutory$ then goto L34660
            if descr$(c%) = hex(ff) then goto L34660
                write #41 using L36170, currency$, so$, seq$(c%),         ~
                     pricestk(c%), price(c%), convdate$, conveqv,        ~
                     convunt, " "

L34660:     curtemp1 = round((pricestk(c%) * conveqv), 4)
            curtemp2 = round((price   (c%) * conveqv), 4)
            dummy    = round((openqty(c%) * curtemp2 / conv(c%)), 2)
            curtemp3 = curtemp3 + (dummy -                               ~
                       round((dummy*(linedisc(c%)/100)), 2))
            if alloc$(c%) = "N" then alloc(c%) = 0

            put #10 using L35830, cuscode$, so$, seq$(c%),                ~
                     item$(c%), part$(c%), descr$(c%), cat$(c%),         ~
                     order(c%), ship(c%), openqty(c%), qtyschld(c%),     ~
                     alloc(c%), preinv(c%), curtemp1, stkuom$(c%),       ~
                     priceuom$(c%), conv(c%), curtemp2, linedisc(c%),    ~
                     taxable$(c%), salesacct$(c%), discacctl$(c%),       ~
                     origdue$(c%), duedate$(c%), shipdate$(c%), lot$(c%),~
                     project$(c%), " ", demtype$(c%), priority$(c%),     ~
                     textidl$(c%), alloc$(c%), invnbr$, estnbr$,         ~
                     bom$(c%), mlquote_seq$(c%), shipcode$(c%),          ~
                     pm_adj%(c%), sc_grp$, filler$(c%)
                                                  /* (EWD001) - Mod  */
            write #10

              /* Add Cross reference part to shadow file */
              if xref%    = 0% then L34920  /* Must have a Xref File */
              if refpart$(c%) = "** No Cross Reference **"  then L34920
              if reftype$(c%) = " "  then L34920
                  call "PTUSEDSB" ("W", "BCK ", so$, seq$(c%),           ~
                                   refpart$(c%), refdesc$(c%),           ~
                                   reftype$(c%), ret%)

L34920:     return

        REM *************************************************************~
            *             F O R M A T    S T A T E M E N T S            *~
            *-----------------------------------------------------------*~
            * FORMAT Statements for Data Files.                         *~
            *************************************************************

L35060: FMT                 /* FILE #05 -- BCKMASTR  (READ ONLY)       */~
            XX(9),          /* Customer Code                           */~
            XX(16),         /* Sales Order Mumber                      */~
            CH(16),         /* Purchase Order Number                   */~
            6*CH(30),       /* Ship-To Name and Address                */~
            6*CH(30),       /* Sold-To Name and Address                */~
            CH(20),         /* Payment Terms                           */~
            CH(20),         /* How Ship Information                    */~
            CH(20),         /* F.O.B. Information                      */~
            2*CH(50),       /* Shipping Instructions                   */~
            CH(9),          /* Sales account number                    */~
            CH(9),          /* Discounts Account                       */~
            3*CH(4),        /* Salesman Codes                          */~
            3*BI(1),        /* Percentage of Sale credited to salesman.*/~
            CH(4),          /* Region code                             */~
            CH(200),        /* Variable Fields                         */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(3),          /* Store Code                              */~
            CH(6),          /* Order Date                              */~
            CH(6),          /* Cancellation Date                       */~
            CH(6),          /* Due Date default                        */~
            CH(6),          /* Date Released                           */~
            CH(6),          /* Originally Input On (date)              */~
            CH(3),          /* Originally Input By (user)              */~
            CH(6),          /* Last Modified On (date)                 */~
            CH(3),          /* Last Modified By (user)                 */~
            XX(9),          /* Adjustment Reason Code                  */~
            CH(1),          /* Export Flag                             */~
            CH(1),          /* Pricing Code                            */~
            PD(14,4),       /* Order Discount Percent                  */~
            PD(14,4),       /* Open Order Amount                       */~
            CH(1),          /* Credit Hold Flag                        */~
            BI(2),          /* Last Sequence Number Used               */~
            BI(4),          /* Next BOL Number                         */~
            POS(893), CH(4),/* Currency code                           */~
            CH(1),          /* How put on hold                         */~
            CH(1),          /* PM Surcharge SO Flag                    */~
            CH(1)           /* PM Surcharge INV Flag                   */

L35410: FMT                 /* FILE #09 -- BCKBUFFR                    */~
            CH(20),         /* User ID, Date/Time, PGM or Print Flag   */~
            CH(9),          /* Customer Code                           */~
            CH(16),         /* Sales Order Mumber                      */~
            CH(16),         /* Purchase Order Number                   */~
            6*CH(30),       /* Ship-To Name and Address                */~
            6*CH(30),       /* Sold-To Name and Address                */~
            CH(20),         /* Payment Terms                           */~
            CH(20),         /* How Ship Information                    */~
            CH(20),         /* F.O.B. Information                      */~
            2*CH(50),       /* Shipping Instructions                   */~
            CH(9),          /* Sales account number                    */~
            CH(9),          /* Discounts Account                       */~
            3*CH(4),        /* Salesman Codes                          */~
            3*BI(1),        /* Percentage of Sale credited to salesman.*/~
            CH(4),          /* Region code                             */~
            CH(200),        /* Variable Fields                         */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(3),          /* Store Code                              */~
            CH(6),          /* Order Date                              */~
            CH(6),          /* Cancellation Date                       */~
            CH(6),          /* Due Date default                        */~
            CH(6),          /* Date Released                           */~
            CH(6),          /* Originally Input On (date)              */~
            CH(3),          /* Originally Input By (user)              */~
            CH(6),          /* Last Modified On (date)                 */~
            CH(3),          /* Last Modified By (user)                 */~
            CH(9),          /* Adjustment Reason Code                  */~
            CH(1),          /* Export Flag                             */~
            CH(1),          /* Pricing Code                            */~
            PD(14,4),       /* Order Discount Percent                  */~
            PD(14,4),       /* Open Order Amount                       */~
            CH(1),          /* Credit Hold Flag                        */~
            BI(2),          /* Last Sequence Number Used               */~
            BI(4),          /* Next BOL Number                         */~
            CH(2),          /* Customer Type Code                      */~
            CH(9),          /* Account X-Ref                           */~
            CH(4),          /* Currency code                           */~
            CH(1),          /* How put on hold                         */~
            CH(1),          /* PM Surcharge SO Flag                    */~
            CH(1),          /* PM Surcharge INV Flag                   */~
            CH(101)         /* Filler                                  */

L35830: FMT                 /* FILES #10 & #06 -- BCKBUF2 AND BCKLINES */~
            CH(9),          /* Customer Code                           */~
            CH(16),         /* Sales Order number                      */~
            CH(3),          /* Sequence Number                         */~
            CH(3),          /* Item Number                             */~
            CH(25),         /* Part Number                             */~
            CH(32),         /* Part Number description                 */~
            CH(4),          /* Category code                           */~
            PD(14,4),       /* Order Quantity                          */~
            PD(14,4),       /* Quantity Shipped (total)                */~
            PD(14,4),       /* Quantity Open                           */~
            PD(14,4),       /* Quantity scheduled for Shipment         */~
            PD(14,4),       /* Quantity Allocated                      */~
            PD(14,4),       /* Quantity Pre-Invoiced                   */~
            PD(14,4),       /* Unit Price @ Stocking UOM               */~
            CH(4),          /* Stocking UOM                            */~
            CH(4),          /* Pricing Unit of Measure                 */~
            PD(14,7),       /* Conversion Factor (Pricing to Stkng)    */~
            PD(14,4),       /* Unit Price                              */~
            PD(14,4),       /* Pricing Discount Percent                */~
            CH(1),          /* Taxable (y/n) indicator                 */~
            CH(9),          /* Sales Account number                    */~
            CH(9),          /* Discounts Account                       */~
            CH(6),          /* Due Date - Original                     */~
            CH(6),          /* Due Date - Current                      */~
            CH(6),          /* Required Ship Date                      */~
            CH(6),          /* Lot Number                              */~
            CH(8),          /* Code/# of a Project                     */~
            CH(8),          /* Filler                                  */~
            CH(1),          /* Demand Type                             */~
            CH(1),          /* Priority Code                           */~
            CH(4),          /* Internal ID to text in TXTFILE.         */~
            CH(1),          /* Allocation Flag                         */~
            CH(8),          /* Invoice Number                          */~
            CH(8),          /* Estimate Number                         */~
            CH(3),          /* Specific BOM version                    */~
            CH(11),         /* MLQ number and line                     */~
            CH(1),          /* Ship priority code                      */~
            BI(1),          /* Precious Metal Surcharge Added Flag     */~
            CH(1),          /* (EWD001) Wood Surround Group Code       */~
            CH(21)          /* (EWD001) Filler                         */

L36170: FMT                 /* FILE #41 -- BCKLNCUR                    */~
            CH(4),          /* Currency code                           */~
            CH(16),         /* Sales Order number                      */~
            CH(3),          /* Sequence Number                         */~
            PD(14,4),       /* Unit Price @ Stocking UOM               */~
            PD(14,4),       /* Unit Price                              */~
            CH(6),          /* Conversion factor effective date        */~
            PD(14,7),       /* Currency conversion factor              */~
            PD(14,7),       /* # Units per statutory currency unit     */~
            CH(39)          /* Filler                                  */

L36590:     FMT /* File #51- CCRMASTR Master file (input/output)       */~
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
            * Extra Lines Conflict Warnings/Errors                      *~
            *************************************************************
        extra_load_conflict

L37050: % Current No. of Lines: ###  (+ Implied Lines: ###) : Total #####

            put askmsg$(1%) using L37050, maxlines%, total_e_lines%,      ~
                                         maxlines% + total_e_lines%
            askmsg$(2%) = "Some of the Implied Lines may not be generated"
            askmsg$(3%) = "Press any PF key to confirm and continue"
            u3% = 2%
            call "ASKUSER" (u3%, "* * * MAXIMUM LINES CONFLICT * * *",   ~
                            askmsg$(1%), askmsg$(2%), askmsg$(3%))
            return

        extra_append_conflict
            put askmsg$(1%) using L37050, c%, total_e_lines% + temp%,     ~
                                         c% + total_e_lines% + temp%
            askmsg$(2%) = "Some of the Implied Lines may not be generated"
            askmsg$(3%) = "Press PF16 to continue, Press RETURN to" &    ~
                          " re-enter Part Code"
L37220:     u3% = 2%
            call "ASKUSER" (u3%, "* * * APPEND LINES CONFLICT * * *",    ~
                            askmsg$(1%), askmsg$(2%), askmsg$(3%))
            if u3% = 16% then return
            if u3% =  0% then return
               goto L37220

        mlq_header_date_check
                mlq_hdate% = 1%  :  errormsg$ = " "
                gosub L50630
                     if errormsg$ <> " " then L38060
                if dfltdue$ <> " " and dfltdue$ <> blankdate$ then L38120
                gosub L20080
L38060:         gosub'050(1%,2%,1%)
                mat enabled% = zer  :  enabled%(10%) = 1%
L38080:         gosub'101(2%,2%)
                     if keyhit% = 1% then gosub startover
                errormsg$ = " "
                gosub L50630
                     if errormsg$ <> " " then L38080
L38120:         mlq_hdate% = 2%  :  errormsg$ = " "
                gosub L50670
                     if errormsg$ <> " " then L38129
                if dfltship$ <> " " and dfltship$ <> blankdate$ then L38140
                gosub L20170
L38129:         gosub'050(1%,2%,1%)
                mat enabled% = zer  :  enabled%(11%) = 1%
L38131:         gosub'101(2%,2%)
                     if keyhit% = 1% then gosub startover
                errormsg$ = " "
                gosub L50670
                     if errormsg$ <> " " then L38131
L38140:     errormsg$ = " "
            mlq_hdate% = 0%
            return

        mlq_lines_date_check
            mlq_ddate% = 1%  :  errormsg$ = " "
            gosub L52420
                if errormsg$  = " " then L38308
            if dfltdue$ = " " or dfltdue$ = blankdate$ then L38290
                duedate$(c%) = dfltdue$
                mlq_line_date% = mlq_line_date% + 1%
                goto L38306
L38290:     gosub'050(3%,2%,1%)
            mat enabled% = zer  :  enabled%(6%) = 1%
            gosub'103(2%,1%)
                if keyhit% = 1% then startover
            errormsg$ = " "
L38306:     gosub L52420
                if errormsg$ <> " " then L38290
L38308:     mlq_ddate% = 2%  :  errormsg$ = " "
            gosub L52460
                if errormsg$  = " " then L38410
            if dfltship$ = " " or dfltship$ = blankdate$ then L38360
                shipdate$(c%) = dfltship$
                mlq_line_date% = mlq_line_date% + 1%
                goto L38390
L38360:     gosub'050(3%,2%,1%)
            mat enabled% = zer  :  enabled%(7%) = 1%
            gosub'103(2%,1%)
                if keyhit% = 1% then gosub startover
            errormsg$ = " "
L38390:     gosub L52460
                if errormsg$ <> " " then L38360
L38410:     errormsg$ = " "
            mlq_ddate% = 0%
            return

        REM *************************************************************~
            *        H E A D E R  /  S U M M A R Y   S C R E E N        *~
            *-----------------------------------------------------------*~
            * Abbreviated Header and Line Item Summary Screen.          *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
            str(line1$,20%,15%) = "Main Screen"       /* (EWD) - Begin */
            str(line1$,35%,15%) = "Currency: " & currency$
            call "APCPL3AB" (code$,status%,"PLAN STAT",d1$,d2$,d3$,#8)
            str(line1$,35%,15%) = hex(94) & str(d1$,1%,13%) & hex(ac)
                                                      /* (EWD) - End   */
            scrtext$ = "Ship-to :"
L40034:     wndw% = 10%  :  gosub set_summary
            gosub setpf1
            init(hex(86)) hfac$()
            if fieldnr% = 0% then L40130

            init(hex(8c)) hfac$()
            if custonly% = 0% then goto L40050
                hfac$(1%) = hex(81)
                goto L40130
L40050:     for x% = 1% to 13%
                if enabled%(x%) = 1% then hfac$(x%) = hex(81)
            next x%                            /* (EWD) MOD - 04/19/96 */
            if enabled%( 3%) = 1% then hfac$( 3%) = hex(84)
            if enabled%( 4%) = 1% then hfac$( 4%) = hex(84)
            if enabled%( 5%) = 1% then hfac$( 5%) = hex(84)
                                               /* (EWD) Mod - End      */
            hfac$(15) = hfac$(3)
            if fieldnr% = 1% and errormsg$ <> " " then L40075
            if fieldnr% <> 1% or cuscode$  <> " " then L40082
L40075:     hfac$(15) = hex(81)
            scrtext$="Cust.Name"
L40082:     if fieldnr% = 1% then hfac$(2%) = hex(81)

                                 /* (EWD) Mods custype$, cus$,   */
L40130:     accept                                                       ~
               at (01,02), fac(hex(ac)), line1$                 , ch(79),~
               at (09,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (02,02), "Customer:",                                  ~
               at (02,12), fac(hfac$( 1)), cuscode$             , ch(09),~
                                                                         ~
               at (05,02), "TYP:",                                       ~
               at (05,07), fac(hex(84)),   custype$             , ch(02),~
               at (07,02), "TXT:",                                       ~
               at (07,07), fac(hex(84)),   cus$                 , ch(03),~
                                                                         ~
               at (02,24), "SO: ",                                       ~
               at (02,28), fac(hfac$( 2)), so$                  , ch(16),~
                                                                         ~
               at (03,02), fac(hex(8c)),   scrtext$             , ch(09),~
               at (03,12), fac(hfac$(15)), shipto$(1)           , ch(30),~
               at (04,12), fac(hfac$( 3)), shipto$(2)           , ch(30),~
               at (05,12), fac(hfac$( 3)), shipto$(3)           , ch(30),~
               at (06,12), fac(hfac$( 3)), shipto$(4)           , ch(30),~
               at (07,12), fac(hfac$( 3)), shipto$(5)           , ch(30),~
               at (08,12), fac(hfac$( 3)), str(shipto$(6), 1,17), ch(17),~
               at (08,30), fac(hfac$( 3)), str(shipto$(6),19, 2), ch(02),~
               at (08,33), fac(hfac$( 3)), str(shipto$(6),22, 9), ch(09),~
                                                                         ~
               at (02,45), "Store Code",                                 ~
               at (02,58), fac(hfac$( 4)), store$               , ch(03),~
                                                                         ~
               at (02,65), "Export?",                                    ~
               at (02,73), fac(hfac$( 5)), export$              , ch(01),~
                                                                         ~
               at (03,45), "PO Number",                                  ~
               at (03,58), fac(hfac$( 6)), po$                  , ch(16),~
                                                                         ~
               at (04,45), "How Ship",                                   ~
               at (04,58), fac(hfac$( 7)), howship$             , ch(20),~
                                                                         ~
               at (05,45), "FOB",                                        ~
               at (05,58), fac(hfac$( 8)), fob$                 , ch(20),~
                                                                         ~
               at (06,45), "Order Date",                                 ~
               at (06,58), fac(hfac$( 9)), orderdate$           , ch(08),~
               at (06,68), "Due "    ,                                   ~
               at (06,73), fac(hfac$(10)), dfltdue$             , ch(08),~
                                                                         ~
               at (07,45), "Req'd Ship",                                 ~
               at (07,58), fac(hfac$(11)), dfltship$            , ch(08),~
               at (07,68), fac(hex(8c))  , weekdayh$            , ch(09),~
                                                                         ~
               at (08,45), fac(hex(8c))  , pm_hdr_dsply$        , ch(28),~
               at (08,65), fac(hfac$(12)), pm_so$               , ch(01),~
               at (08,68), fac(hex(8c))  , pm_hdr_dsply2$       , ch(07),~
               at (08,76), fac(hfac$(13)), pm_inv$              , ch(01),~
                                                                         ~
               at (10,02), fac(hex(ac))  , smryh$               , ch(79),~
               at (11,02), fac(sfac$( 1)), smry$( 1)            , ch(79),~
               at (12,02), fac(sfac$( 2)), smry$( 2)            , ch(79),~
               at (13,02), fac(sfac$( 3)), smry$( 3)            , ch(79),~
               at (14,02), fac(sfac$( 4)), smry$( 4)            , ch(79),~
               at (15,02), fac(sfac$( 5)), smry$( 5)            , ch(79),~
               at (16,02), fac(sfac$( 6)), smry$( 6)            , ch(79),~
               at (17,02), fac(sfac$( 7)), smry$( 7)            , ch(79),~
               at (18,02), fac(sfac$( 8)), smry$( 8)            , ch(79),~
               at (19,02), fac(sfac$( 9)), smry$( 9)            , ch(79),~
               at (20,02), fac(sfac$(10)), smry$(10)            , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

               if keyhit% <> 22% then L40406
                   if edit% <> 2% then L40406
                       if cms% = 1% then L40400
                           cms% = 1%  :  ref% = 0%
                           smryh$ = smryhdr$
                           goto L40034
L40400:                cms% = 0%  :  ref% = 1%
                       smryh$ = "Seq Reference Part Number      "  &     ~
                                str(smryhdr$,32%,48%)
                       goto L40034

L40406:     if keyhit% <> 26% then goto L40410
                gosub call_customer_credit
                goto L40130

                                                       /* (EWD) Begin */  
L40410:        gosub close_out_screen
               if close_out% = 1% then goto L40130
                return
                                                       /* (EWD) End   */
        setpf1
           if str(terms$,1%,3%) = "COD" then cod$ = "C O D"      /*(EWD)*/
           if str(terms$,1%,5%) = "COD C" then cod$ = "C O D-CERT"
           if str(terms$,1%,6%) = "2% COD" then cod$ = "2% C O D"/*(EWD)*/

        if edit% = 2% then L40524         /* Input Mode                 */
           pf$(1) = "(1)Start Over    (3)Copy Existing Order           "&~
                    "             (13)Instructions"
           pf$(2) = "                 (8)Customer Display     (26)Custo"&~
                    "mer Credit   (15)Print Screen"
           pf$(3) = "                 (10)S.O. Display        (27)Cutov"&~
                    "er Quotation (16)Exit Program"
           pfkey$ = hex(01ff03ffffffff08ff0affff0dff0f10ffff1a1b00)
           if fieldnr% = 2% and mlqyorn$ = "Y" and errormsg$ = " "       ~
                then goto L40504
                str(pf$(3%),42%,21%) = " " : str(pfkey$,20%,1%) = hex(ff)
L40504:    if copy1%   = 1% then L40506
           if fieldnr% > 1% then L40512
L40506:         str(pf$(1%),18%,22%) = " " : str(pfkey$,3%,1%) = hex(ff)
                str(pf$(2%),42%,19%) = " " : str(pfkey$,19%,1%) = hex(ff)
                if copy1% = 1% then L40512
           return
L40512:         str(pf$(3%),64%,16%) = " " : str(pfkey$,16%,1%) = hex(ff)
           return

L40524:  if fieldnr% > 0% then L40585     /* Edit Mode- Select Option   */
           pf$(1) = "(1)S/Ovr (9)Hdr Detail (12)Delete (18)Part Data   "&~
                    "(22)Toggle   (13)Instructions"
           pf$(2) = "(2)First (4)Prev (6)Dn (10)Cancel (23)All Dates   "&~
                    "(25)Hdr Text (15)Print Screen"
           pf$(3) = "(3)Last  (5)Next (7)Up (11/27)App Lns/Qt          "&~
                    "(26)Cus Crdt (16)End Order   "
           str(pf$(2%),63%,1%) = hex(8c)/* Hilite PF(25) if text exists */
           gosub'040(textid$)
           str(pf$(2%),50%,1%) = fac25$
           pfkey$= hex(01020304050607ff090a0b0c0dff0f10ff191d1a161b121700)
                                               /* (EWD) Mod - 10/22/96 */
           if status% > 1% and status% <> 99% then goto L40570

           if mlqyorn$ = "Y" then goto L40566
                str(pf$(3%),24%,17%) = "(11)Append Lines "
                str(pfkey$,22%,1%) = hex(ff)
L40566:    if refflag% = 1% then L40568
               str(pf$(1%),51%,10%) = " " : str(pfkey$,21%,1%) = hex(ff)
L40568:    if allow_delete$ <> "N" then L40575
           if str(lsts$(c%),2) = "A" or str(lsts$(c%),2) = "R" then L40575
L40570:       str(pf$(1%),24%,10%) = " " : str(pfkey$,12%,1%) = hex(ff)
L40575:    return

                                         /* Edit Mode- Field Enabled   */
L40585:    pf$(1) = "                                                  "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(ffffffffffffffffffffffff0dff0fffffffff00)
           return

        REM *************************************************************~
            *           D E T A I L   H E A D E R   S C R E E N         *~
            *-----------------------------------------------------------*~
            * Full Header Screen for Editing.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)                  /* (EWD) - Begin */
            str(line1$,20%,15%) = "Header Detail"
            str(line1$,35%,15%) = "Currency: " & currency$
            call "APCPL3AB" (code$, status%, "PLAN STAT",d1$,d2$,d3$,#8)
            str(line1$,35%,15%) = hex(94) & str(d1$,1%,13%) & hex(ac)
                                                    /* (EWD) - End   */
            gosub setpf2
            init(hex(86)) hfac$()
            if fieldnr% = 0% then L41175

            init(hex(8c)) hfac$()
            for x% = 1% to 22%
                if enabled%(x%) = 1% then hfac$(x%) = hex(81)
            next x%
            if enabled%(23%) = 1% then hfac$(23%) = hex(80)
                                              /* (EWD) - Next 12 Lines */
L41175:     accept                                                       ~
               at (01,02), fac(hex(ac)), line1$                 , ch(79),~
               at (09,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (02,02), "Customer:",                                  ~
               at (02,12), fac(hfac$( 1)), cuscode$             , ch(09),~
                                                                         ~
               at (05,02), "TYP:",                                       ~
               at (05,07), fac(hex(84)),   custype$             , ch(02),~
               at (07,02), "TXT:",                                       ~
               at (07,07), fac(hex(84)),   cus$                 , ch(03),~
                                                                         ~
               at (02,24), "SO: ",                                       ~
               at (02,28), fac(hfac$( 2)), so$                  , ch(16),~
                                                                         ~
               at (03,02), "Ship-to :",                                  ~
               at (03,12), fac(hfac$( 3)), shipto$(1)           , ch(30),~
               at (04,12), fac(hfac$( 3)), shipto$(2)           , ch(30),~
               at (05,12), fac(hfac$( 3)), shipto$(3)           , ch(30),~
               at (06,12), fac(hfac$( 3)), shipto$(4)           , ch(30),~
               at (07,12), fac(hfac$( 3)), shipto$(5)           , ch(30),~
               at (08,12), fac(hfac$( 3)), str(shipto$(6), 1,17), ch(17),~
               at (08,30), fac(hfac$( 3)), str(shipto$(6),19, 2), ch(02),~
               at (08,33), fac(hfac$( 3)), str(shipto$(6),22, 9), ch(09),~
                                                                         ~
               at (02,45), "Store Code",                                 ~
               at (02,58), fac(hfac$( 4)), store$               , ch(03),~
                                                                         ~
               at (02,65), "Export?",                                    ~
               at (02,73), fac(hfac$( 5)), export$              , ch(01),~
                                                                         ~
               at (03,45), "PO Number",                                  ~
               at (03,58), fac(hfac$( 6)), po$                  , ch(16),~
                                                                         ~
               at (04,45), "How Ship",                                   ~
               at (04,58), fac(hfac$( 7)), howship$             , ch(20),~
                                                                         ~
               at (05,45), "FOB",                                        ~
               at (05,58), fac(hfac$( 8)), fob$                 , ch(20),~
                                                                         ~
               at (06,45), "Order Date",                                 ~
               at (06,58), fac(hfac$( 9)), orderdate$           , ch(08),~
               at (06,68), "Due",                                        ~
               at (06,73), fac(hfac$(10)), dfltdue$             , ch(08),~
                                                                         ~
               at (07,45), "Req'd Ship",                                 ~
               at (07,58), fac(hfac$(11)), dfltship$            , ch(08),~
               at (07,68), fac(hex(8c))  , weekdayh$            , ch(09),~
                                                                         ~
               at (08,45), fac(hex(8c))  , pm_hdr_dsply$        , ch(28),~
               at (08,65), fac(hfac$(12)), pm_so$               , ch(01),~
               at (08,68), fac(hex(8c))  , pm_hdr_dsply2$       , ch(07),~
               at (08,76), fac(hfac$(13)), pm_inv$              , ch(01),~
                                                                         ~
               at (10,02), "Sold-to :",                                  ~
               at (10,12), fac(hfac$(14)), soldto$(1)           , ch(30),~
               at (11,12), fac(hfac$(14)), soldto$(2)           , ch(30),~
               at (12,12), fac(hfac$(14)), soldto$(3)           , ch(30),~
               at (13,12), fac(hfac$(14)), soldto$(4)           , ch(30),~
               at (14,12), fac(hfac$(14)), soldto$(5)           , ch(30),~
               at (15,12), fac(hfac$(14)), str(soldto$(6), 1,17), ch(17),~
               at (15,30), fac(hfac$(14)), str(soldto$(6),19, 2), ch(02),~
               at (15,33), fac(hfac$(14)), str(soldto$(6),22, 9), ch(09),~
                                                                         ~
               at (10,45), "Cancel Date",                                ~
               at (10,58), fac(hfac$(15)), canceldate$          , ch(08),~
                                                                         ~
               at (11,45), "Price Code",                                 ~
               at (11,58), fac(hfac$(16)), pc$                  , ch(01),~
                                                                         ~
               at (12,45), "Order Disc %",                               ~
               at (12,58), fac(hfac$(17)), orderdisc$           , ch(06),~
                                                                         ~
               at (13,45), "Paymnt Terms",                               ~
               at (13,58), fac(hfac$(18)), terms$               , ch(20),~
                                                                         ~
               at (14,45), "Region Code",                                ~
               at (14,58), fac(hfac$(19)), region$              , ch(04),~
                                                                         ~
               at (15,45), "Salesman / %",                               ~
               at (15,58), fac(hfac$(20)), salesman$(1)         , ch(04),~
               at (15,64), fac(hfac$(20)), comm$(1)             , ch(03),~
               at (16,58), fac(hfac$(20)), salesman$(2)         , ch(04),~
               at (16,64), fac(hfac$(20)), comm$(2)             , ch(03),~
               at (17,58), fac(hfac$(20)), salesman$(3)         , ch(04),~
               at (17,64), fac(hfac$(20)), comm$(3)             , ch(03),~
                                                                         ~
               at (17,02), "Sales Account Default",                      ~
               at (17,30), fac(hfac$(21)), dfltsales$           , ch(12),~
                                                                         ~
               at (18,02), "Sales Discounts Account",                    ~
               at (18,30), fac(hfac$(22)), discacct$            , ch(12),~
                                                                         ~
               at (19,02), "Shipping Instructions",                      ~
               at (19,30), fac(hfac$(23)), shipinstr$(1)        , ch(50),~
               at (20,30), fac(hfac$(23)), shipinstr$(2)        , ch(50),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

            if keyhit% <> 26% then goto L41630
                gosub call_customer_credit
                goto L41175

                                                    /* (EWD) - Begin */
L41630:        gosub close_out_screen
               if close_out% = 1% then goto L41175
               return                               /* (EWD) - End   */

        setpf2                            /* (EWD) - Begin              */
           if str(terms$,1%,3%) = "COD" then cod$ = "C O D"
           if str(terms$,1%,5%) = "COD C" then cod$ = "C O D-CERT"
           if str(terms$,1%,6%) = "2% COD" then cod$ = "2% C O D"

         if fieldnr% > 0% then L41790     /* Edit Mode- Select Option   */
           pf$(1) = "(1)Start Over      (8)Customer Display            "&~
                    "             (13)Instructions"
           pf$(2) = "                   (9)Variable Fileds   (25)Manage"&~
                    " Text        (15)Print Screen"
           pf$(3) = "                   (10)S.O. Display     (26)Custom"&~
                    "er Credit    (16)Summary Scrn"
           str(pf$(2),56,1) = hex(8c) /* Hilight PF(25) if text exists */
           gosub'040(textid$)
           str(pf$(2),40,1) = fac25$
           pfkey$ = hex(01ffffffffffff08090affff0dff0f10ff191d1a00)
           return
                                         /* (EWD) - End                */
                                         /* Edit Mode- Field Enabled   */
L41790:    pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

        REM *************************************************************~
            *             L I N E   I T E M   S C R E E N               *~
            *-----------------------------------------------------------*~
            * Screen for entering and editing of line items.            *~
            *************************************************************

        deffn'103(fieldnr%, edit%)                  /* (EWD) - Begin  */
            str(line1$,20%,15%) = "Line Entry/Edit"
            str(line1$,35%,15%) = " "
            call "APCPL3AB" (code$, status%,"PLAN STAT",d1$,d2$,d3$,#8)
            str(line1$,35%,15%) = hex(94) & str(d1$,1%,13%) & hex(ac)
                                                    /* (EWD) - End    */
            xfac$ = hex(9c)  :  lastkey% = 0%
            str(line1$,20%,15%) = "Line Entry/Edit"
            str(line1$,35%,15%) = " "
            wndw% = 3%  :  gosub set_summary
            gosub setpf3
            init(hex(86)) lfac$() : init(hex(8c)) hfac$()
            if fieldnr% = 0% then goto L42420
            init(hex(8c)) lfac$()
            if part$(c%) <> " " then                                     ~
                 call "LOTENABL" (part$(c%), enabled%(13%), 6%, #02, #04)
            if enabled%( 1%) = 1% then lfac$( 1%) = hex(81) /* Part #   */
            if enabled%( 2%) = 1% then lfac$( 9%) = hex(80) /* Order Qty*/
            if enabled%( 3%) = 1% then lfac$(14%) = hex(81) /* Curr Cd  */
            if enabled%( 4%) = 1% then lfac$( 2%) = hex(80) /* P/Descrip*/
            if enabled%( 5%) = 1% then lfac$( 3%) = hex(81) /* Prod Cat */
            if enabled%( 6%) = 1% then lfac$( 4%) = hex(81) /* Due Date */
            if enabled%( 7%) = 1% then lfac$( 5%) = hex(81) /* Shp Date */
            if enabled%( 8%) = 1% then lfac$( 6%) = hex(80) /* Stk UOM  */
            if enabled%( 9%) = 1% then lfac$( 7%) = hex(80) /* Price UOM*/
            if enabled%(10%) = 1% then lfac$( 8%) = hex(82) /* Conv Fctr*/
            if enabled%(11%) = 1% then lfac$(10%) = hex(81) /* Alloc Flg*/
            if enabled%(12%) = 1% then lfac$(11%) = hex(80) /* Open Qty */
            if enabled%(13%) = 1% or enabled%(13%) = 2%                  ~
                                  then lfac$(12%) = hex(81) /* Lot      */
*          IF ENABLED%(14%) = 1% THEN LFAC$(13%) = HEX(80) /* Ship Qty */
            if enabled%(15%) = 1% then lfac$(15%) = hex(82) /* U/Price  */
            if enabled%(16%) = 1% then lfac$(16%) = hex(82) /* Disc %   */
            if enabled%(17%) = 1% then lfac$(17%) = hex(81) /* Taxable ?*/
            if enabled%(18%) = 1% then lfac$(18%) = hex(80) /* PO Line #*/
            if enabled%(19%) = 1% then lfac$(19%) = hex(81) /* Project  */
            if enabled%(20%) = 1% then lfac$(20%) = hex(81) /* Sale Acct*/
            if enabled%(21%) = 1% then lfac$(21%) = hex(81) /* Disc Acct*/
            if enabled%(22%) = 1% then lfac$(22%) = hex(81) /* Shp Pri  */
            if enabled%(23%) = 1% then lfac$(23%) = hex(81) /* Dem Pri  */
            if enabled%(24%) = 1% then lfac$(24%) = hex(81) /* Dem Type */
            if enabled%(25%) = 1% then lfac$(25%) = hex(81) /* Whatever */
            if enabled%(26%) = 1% then lfac$(26%) = hex(81) /* Whatever */

                                                /* (EWD) Next 12 Lines */ 
L42420:     accept                                                       ~
               at (01,02), fac(hex(ac)), line1$                 , ch(79),~
               at (09,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (02,02), "Customer:",                                  ~
               at (02,12), fac(hfac$( 1)), cuscode$             , ch(09),~
                                                                         ~
               at (05,02), "TYP:",                                       ~
               at (05,07), fac(hex(84)),   custype$             , ch(02),~
               at (07,02), "TXT:",                                       ~
               at (07,07), fac(hex(84)),   cus$                 , ch(03),~
                                                                         ~
               at (02,24), "SO: ",                                       ~
               at (02,28), fac(hfac$( 2)), so$                  , ch(16),~
                                                                         ~
               at (03,02), "Ship-to :",                                  ~
               at (03,12), fac(hfac$( 3)), shipto$(1)           , ch(30),~
               at (04,12), fac(hfac$( 3)), shipto$(2)           , ch(30),~
               at (05,12), fac(hfac$( 3)), shipto$(3)           , ch(30),~
               at (06,12), fac(hfac$( 3)), shipto$(4)           , ch(30),~
               at (07,12), fac(hfac$( 3)), shipto$(5)           , ch(30),~
               at (08,12), fac(hfac$( 3)), str(shipto$(6), 1,17), ch(17),~
               at (08,30), fac(hfac$( 3)), str(shipto$(6),19, 2), ch(02),~
               at (08,33), fac(hfac$( 3)), str(shipto$(6),22, 9), ch(09),~
                                                                         ~
               at (02,45), "Store Code",                                 ~
               at (02,58), fac(hfac$( 4)), store$               , ch(03),~
                                                                         ~
               at (02,65), "Export?",                                    ~
               at (02,73), fac(hfac$( 5)),export$               , ch(01),~
                                                                         ~
               at (03,45), "PO Number",                                  ~
               at (03,58), fac(hfac$( 6)), po$                  , ch(16),~
                                                                         ~
               at (04,45), "How Ship",                                   ~
               at (04,58), fac(hfac$( 7)), howship$             , ch(20),~
                                                                         ~
               at (05,45), "FOB",                                        ~
               at (05,58), fac(hfac$( 8)), fob$                 , ch(20),~
                                                                         ~
               at (06,45), "Order Date",                                 ~
               at (06,58), fac(hfac$( 9)), orderdate$           , ch(08),~
               at (06,68), "Due Date",                                   ~
               at (06,73), fac(hfac$(10)), dfltdue$             , ch(08),~
                                                                         ~
               at (07,45), "Req'd Ship",                                 ~
               at (07,58), fac(hfac$(11)), dfltship$            , ch(08),~
               at (07,68), fac(hex(8c))  , weekdayh$            , ch(09),~
                                                                         ~
               at (08,45), fac(hex(8c))  , pm_hdr_dsply$        , ch(28),~
               at (08,65), fac(hfac$(12)), pm_so$               , ch(01),~
               at (08,68), fac(hex(8c))  , pm_hdr_dsply2$       , ch(07),~
               at (08,76), fac(hfac$(13)), pm_inv$              , ch(01),~
                                                                         ~
               at (10,02), fac(hex(ac))  , smryhdr$             , ch(79),~
               at (11,02), fac(sfac$( 1)), smry$( 1)            , ch(79),~
               at (12,02), fac(sfac$( 2)), smry$( 2)            , ch(79),~
               at (13,02), fac(sfac$( 3)), smry$( 3)            , ch(79),~
               at (14,02), fac(hex(80))  , temp$, /* Pos Cursor        */~
                                                                         ~
               at (14,02), "Part",                                       ~
               at (14,08), fac(lfac$( 1)), part$(c%)            , ch(25),~
                                                                         ~
               at (14,34), "Descr",                                      ~
               at (14,40), fac(lfac$( 2)), descr$(c%)           , ch(32),~
               at (14,74), fac(hex(8c))  , nonstockmsg$(c%)     , ch(06),~
                                                                         ~
               at (15,02), "Catgy",                                      ~
               at (15,08), fac(lfac$( 3)), cat$(c%)             , ch(04),~
                                                                         ~
               at (15,13), "Due",                                        ~
               at (15,17), fac(lfac$( 4)), duedate$(c%)         , ch(08),~
                                                                         ~
               at (15,26), "Ship",                                       ~
               at (15,31), fac(lfac$( 5)), shipdate$(c%)        , ch(08),~
                                                                         ~
               at (15,40), "UOMS: Stkng",                                ~
               at (15,52), fac(lfac$( 6)), stkuom$(c%)          , ch(04),~
               at (15,57), "Prcg",                                       ~
               at (15,62), fac(lfac$( 7)), priceuom$(c%)        , ch(04),~
               at (15,67), "Cnv",                                        ~
               at (15,71), fac(lfac$( 8)), conv$                , ch(10),~
                                                                         ~
               at (16,02), "Order",                                      ~
               at (16,08), fac(lfac$( 9)), order$               , ch(10),~
               at (16,19), "Alloc Flag",                                 ~
               at (16,30), fac(lfac$(10)), alloc$(c%)           , ch(01),~
               at (16,36), "Open",                                       ~
               at (16,41), fac(lfac$(11)), openqty$             , ch(10),~
               at (16,52), "Lot",                                        ~
               at (16,56), fac(lfac$(12)), str(lot$(c%),,ll%),           ~
               at (16,63), "Invoicd",                                    ~
               at (16,71), fac(hex(8c)),   ship$                , ch(10),~
                                                                         ~
               at (17,02), "Curr",                                       ~
               at (17,07), fac(lfac$(14)), currency$            , ch(04),~
               at (17,12), "Price",                                      ~
               at (17,18), fac(lfac$(15)), price$               , ch(10),~
               at (17,29), "Disc%",                                      ~
               at (17,35), fac(lfac$(16)), linedisc$            , ch(06),~
               at (17,42), "OpnExt",                                     ~
               at (17,49), fac(hex(8c))  , ext$                 , ch(10),~
               at (17,60), "Tax?",                                       ~
               at (17,65), fac(lfac$(17)), taxable$(c%)         , ch(01),~
               at (17,67), "Sch",                                        ~
               at (17,71), fac(hex(8c)),   qtyschld$            , ch(10),~
                                                                         ~
               at (18,02), "PO Ln",                                      ~
               at (18,08), fac(lfac$(18)), item$(c%)            , ch(03),~
               at (18,13), "Proj",                                       ~
               at (18,18), fac(lfac$(19)), project$(c%)         , ch(08),~
               at (18,29), "Sales Acct",                                 ~
               at (18,40), fac(lfac$(20)), salesacct$(c%)       , ch(12),~
               at (18,53), "Sales Disc Acct",                            ~
               at (18,69), fac(lfac$(21)), discacctl$(c%)       , ch(12),~
                                                                         ~
               at (19,02), "Ship Prty",                                  ~
               at (19,12), fac(lfac$(22)), shipcode$(c%)        , ch(01),~
               at (19,14), "Plan: Prty",                                 ~
               at (19,25), fac(lfac$(23)), priority$(c%)        , ch(01),~
               at (19,27), "Demand",                                     ~
               at (19,34), fac(lfac$(24)), demtype$(c%)         , ch(01),~
               at (19,37), fac(hex(8c))  , atc$(c%,toggle%)     , ch(41),~
                                                                         ~
               at (20,02), fac(hex(ac))  , plan$                , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
               at (24,31), fac(xfac$),     mfgcode$             , ch(12),~
                     keys(pfkey$), key(keyhit%)

               if keyhit% <> 23% then L43592
                   lastkey% = keyhit%
                   init(hex(81)) xfac$ : init(hex(84)) lfac$()
                   inpmessage$ = "Enter The Manufacturer's Code For Thi"&~
                                 "s Part Number."
                   goto L42420

L43592:     if keyhit% <> 26% then goto L43600
                gosub call_customer_credit
                goto L42420

L43600:        if keyhit% <> 10% then L43652
                    call "BCKPRCSB" (cuscode$, custype$, part$(c%),      ~
                          cat$(c%), pc$, currency$, currtype$, order(c%),~
                          #2, #1, #4, #40)
                    goto L42420

L43652:        if keyhit% <> 14% then L43660
                  gosub display_corebank
                  goto L42420

L43660:        if keyhit% <> 13 then L43690
                  call "MANUAL" ("BCKFASTR") : goto L42420

L43690:        if keyhit% <> 15 then L43720
                  call "PRNTSCRN" : goto L42420

L43720:        if keyhit% <> 8% then L43760
                  call "PIPATCSB" (part$(c%), #24, #04, #28, #19, #25,   ~
                                   #26, #29, #23, #27) : goto L42420

L43760:        if keyhit% <> 24% then L43790
                  call "HNYQDISP" (part$(c%), #4, #13, #13, #2)

L43790:        if keyhit% <> 25% then L43796
                  gosub edit_text_line : goto L42420

L43796:        if keyhit% <> 27% then L43805
                  if toggle% = 1% then toggle% = 2% else toggle% = 1%
                  goto L42420

L43805:        if keyhit% <> 20% then L43820
                  gosub display_pm_surcharges
                  goto L42420

L43820:         close ws
                call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
                return

        setpf3                                         /* (EWD) - Begin */
           if str(terms$,1%,3%) = "COD" then cod$ = "C O D"       
           if str(terms$,1%,5%) = "COD C" then cod$ = "C O D-CERT"
           if str(terms$,1%,6%) = "2% COD" then cod$ = "2% C O D"
                                                       /* (EWD) - End   */
        if xref% = 0% then L43870
           init(hex(00)) plowkey$
           str(plowkey$,1%,1%) = "C" : str(plowkey$,2%,9%) = str(cuscode$)
           call "READ104" (#45, plowkey$, f1%(45%))
               if f1%(45%) = 0% then L43870
           if str(plowkey$,1%,10%) = "C" & str(cuscode$) then cust% = 1%
L43870: if edit% = 2% then L44090         /* Input Mode                 */
           pf$(1) = "(1)Start Over  (8/24)ATC/QTY                 (22)C"&~
                    "ust Pt Xref  (13)Instructions"
           pf$(2) = "(2)Restart Line (9)Size Run  (23)Mfg Pt Xref (25)L"&~
                    "ine Text     (15)Print Screen"
           pf$(3) = "(6)Copy (10)Prc (11)Date Run                 (26)C"&~
                    "ust Credit   (16)Exit Append "
           gosub hilite_text
           pfkey$ = hex(0102ffffff06ff08090a0bff0dff0f10ffff18191a161700)
           if xref% = 1% then L43940
                str(pf$(1%),46%,16%) = " " : str(pfkey$,22%,1%) = hex(ff)
                str(pf$(2%),30%,15%) = " " : str(pfkey$,23%,1%) = hex(ff)
L43940:    if cust% = 1% then L43944
                str(pf$(1%),46%,16%) = " " : str(pfkey$,22%,1%) = hex(ff)
L43944:    if fieldnr% = 1% then L43952
                str(pf$(1%),46%,16%) = " " : str(pfkey$,22%,1%) = hex(ff)
                str(pf$(2%),30%,15%) = "(18)Part Data"
                str(pfkey$,23%,1%) = hex(ff)
                str(pfkey$,18%,1%) = hex(12)
L43952:    if core% = 0% or fieldnr% = 1% then L43960
                str(pf$(1%),30%,13%) = "(14)Core Bank"
                str(pfkey$,14%,1%) = hex(0e)
L43960:    if fieldnr% <> 1% then goto L44010
                str(pf$(3%),9%,7%)   = " " : str(pfkey$,10%,1%) = hex(ff)
                str(pf$(1%),16%,13%) = " " : str(pfkey$,19%,1%) = hex(ff)
                /* Combined 'em */           str(pfkey$, 8%,1%) = hex(ff)
                str(pf$(2%),46%,13%) = " " : str(pfkey$,20%,1%) = hex(ff)
L44010:    if fieldnr% < 2% then goto L44050
                str(pf$(2),17,11) = " " : str(pfkey$,09, 1) = hex(ff)
                str(pf$(3),64,15) = " " : str(pfkey$,16, 1) = hex(ff)
                str(pf$(3), 1, 7) = " " : str(pfkey$, 6, 1) = hex(ff)
L44050:    if c% > 1% then goto L44061
                str(pf$(3), 1,7) = " " : str(pfkey$, 6, 1) = hex(ff)
L44061:    if c% > 1% then L44070
                str(pf$(3%),17%,12%) = " " : str(pfkey$,11%,1%) = hex(ff)
L44070:    return

L44090:  if fieldnr% > 0% then L44310     /* Edit Mode- Select Option   */
           pf$(1) = "(1)Start Over  (8/24)ATC/QTY                      "&~
                    "            (27)ATC Tgl (13)I"
           pf$(2) = "(6)Prev Line   (10)Prices    (18)Part Data   (25)L"&~
                    "ine Text     (15)Print Screen"
           pf$(3) = "(7)Next Line   (12)Delete Ln                 (26)C"&~
                    "ust Credit   (16)Summary Scrn"
           gosub hilite_text
           xfac$ = fac25$
           mfgcode$ = "  "
           if parttype$(c%)  = "000" then mfgcode$ = "(29)Options"
           pfkey$ = hex(01ffffffff060708ff0aff0c0dff0f1018191d1a121b00ff)
                                               /* (EWD) Mod - 10/22/96 */
           if status% > 1% and status% <> 99% then goto L44200

           if core% = 0% then L44180
                str(pf$(1%),30%,13%) = "(14)Core Bank"
                str(pfkey$,14%,1%) = hex(0e)
L44180:    if allow_delete$ <> "N" then L44230
           if str(lsts$(c%),1,1) = "A" or                                ~
                                   str(lsts$(c%),1,1) = "R" then L44230
L44200:        str(pf$(3%),16%,13%) = " "
               str(pfkey$,12%,1%) = hex(ff)
L44230:    if append% <> 1% then L44280
                str(pf$(3)   ,68) = "Next Line"
                str(pf$(2), 1,12) = " "   :  str(pfkey$, 6,1) = hex(ff)
                str(pf$(3), 1,12) = " "   :  str(pfkey$, 7,1) = hex(ff)
                str(pf$(3),16,13) = " "   :  str(pfkey$,12,1) = hex(ff)
L44280:    if pm_adj%(c%) <> 1% then L44290
                str(pf$(1%),46%,13%) = "(20)PM Charge"
                str(pfkey$,24%,1%) = hex(14)
L44290:    return
                                         /* Edit Mode- Field Enabled   */
L44310:    pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

        hilite_text
           str(pf$(2),59,1) = hex(8c) /* Hilight PF(25) if text exists */
           gosub'040(textidl$(c%))
           str(pf$(2),45,1) = fac25$
           return

        display_corebank
            ret% = 0%
            close ws
            call "CMSLINK" addr(#44, userid$, "R", "CORDSPLY",           ~
                                "        ", "      ", " ", "N",          ~
                                "                        ",              ~
                                    "                ",  "N", u3%, ret%)
            return

        REM *************************************************************~
            *           D A T A   S A V E   S C R E E N                 *~
            *-----------------------------------------------------------*~
            * Some last decisions to make and data to fill in.          *~
            *************************************************************

        deffn'104(fieldnr%, edit%)                  /* (EWD) - Begin */
            str(line1$,20%,15%) = "Order Recap"
            str(line1$,35%,15%) = " "
            call "APCPL3AB" (code$, status%,"PLAN STAT",d1$,d2$,d3$,#8)
            str(line1$,35%,15%) = hex(94) & str(d1$,1%,13%) & hex(ac)
                                                    /* (EWD) - End   */
            init (hex(8c)) hfac$()
            init (hex(86)) lfac$()
            gosub setpf4 : gosub set_currency_description
            inpmessage$ = "Press (RETURN) to edit Recap data fields OR "&~
                "press an enabled PF key."
            if fieldnr% = 0% then goto L45150
                init (hex(81)) lfac$()
                inpmessage$ = "Enter/Edit Order Recap fields, then pres"&~
                    "s (RETURN)."

                                              /* (EWD) Next 12 Lines */
L45150:     accept                                                       ~
               at (01,02), fac(hex(ac)), line1$                 , ch(79),~
               at (09,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (02,02), "Customer:",                                  ~
               at (02,12), fac(hfac$( 1)), cuscode$             , ch(09),~
                                                                         ~
               at (05,02), "TYP:",                                       ~
               at (05,07), fac(hex(84)),   custype$             , ch(02),~
               at (07,02), "TXT:",                                       ~
               at (07,07), fac(hex(84)),   cus$                 , ch(03),~
                                                                         ~
               at (02,24), "SO: ",                                       ~
               at (02,28), fac(hfac$( 2)), so$                  , ch(16),~
                                                                         ~
               at (03,02), "Ship-to :",                                  ~
               at (03,12), fac(hfac$( 3)), shipto$(1)           , ch(30),~
               at (04,12), fac(hfac$( 3)), shipto$(2)           , ch(30),~
               at (05,12), fac(hfac$( 3)), shipto$(3)           , ch(30),~
               at (06,12), fac(hfac$( 3)), shipto$(4)           , ch(30),~
               at (07,12), fac(hfac$( 3)), shipto$(5)           , ch(30),~
               at (08,12), fac(hfac$( 3)), str(shipto$(6), 1,17), ch(17),~
               at (08,30), fac(hfac$( 3)), str(shipto$(6),19, 2), ch(02),~
               at (08,33), fac(hfac$( 3)), str(shipto$(6),22, 9), ch(09),~
                                                                         ~
               at (02,45), "Store Code",                                 ~
               at (02,58), fac(hfac$( 4)), store$               , ch(03),~
                                                                         ~
               at (02,65), "Export?",                                    ~
               at (02,73), fac(hfac$( 5)), export$              , ch(01),~
                                                                         ~
               at (03,45), "PO Number",                                  ~
               at (03,58), fac(hfac$( 6)), po$                  , ch(16),~
                                                                         ~
               at (04,45), "How Ship",                                   ~
               at (04,58), fac(hfac$( 7)), howship$             , ch(20),~
                                                                         ~
               at (05,45), "FOB",                                        ~
               at (05,58), fac(hfac$( 8)), fob$                 , ch(20),~
                                                                         ~
               at (06,45), "Order Date",                                 ~
               at (06,58), fac(hfac$( 9)), orderdate$           , ch(08),~
               at (06,68), "Due Date",                                   ~
               at (06,73), fac(hfac$(10)), dfltdue$             , ch(08),~
                                                                         ~
               at (07,45), "Req'd Ship",                                 ~
               at (07,58), fac(hfac$(11)), dfltship$            , ch(08),~
               at (07,68), fac(hex(8c))  , weekdayh$            , ch(09),~
                                                                         ~
               at (08,45), fac(hex(8c))  , pm_hdr_dsply$        , ch(28),~
               at (08,65), fac(hfac$(12)), pm_so$               , ch(01),~
               at (08,68), fac(hex(8c))  , pm_hdr_dsply2$       , ch(07),~
               at (08,76), fac(hfac$(13)), pm_inv$              , ch(01),~
                                                                         ~
               at (10,02), fac(hex(80))  , str(temp$,,1),                ~
               at (10,02), fac(hex(8c)),   adjmsg$              , ch(24),~
               at (10,30), fac(lfac$( 1)), adjrsn$              , ch(09),~
               at (10,49), fac(hex(8c)),   adjrsndescr$         , ch(30),~
                                                                         ~
               at (11,02), "Print Docs:  Pick List x     BOL x     Acknow~
        ~ledgements x",                                                   ~
               at (11,25), fac(lfac$( 2)), pickprint$(2%)       , ch(01),~
               at (11,35), fac(lfac$( 2)), pickprint$(3%)       , ch(01),~
               at (11,58), fac(lfac$( 2)), pickprint$(1%)       , ch(01),~
                                                                         ~
               at (12,02), "Currency code",                              ~
               at (12,30), fac(hex(8c)),   currency$            , ch(04),~
               at (12,49), fac(hex(8c)),   currdesc$(1)         , ch(32),~
               at (13,49), fac(hex(8c)),   currdesc$(2)         , ch(32),~
                                                                         ~
               at (14,25), "----ORDER----",                              ~
               at (14,41), "----OPEN -----",                             ~
               at (15,02), "Gross Order Amount",                         ~
               at (15,25), fac(hex(84)), torder(1),  pic(-######,###.00),~
               at (15,41), fac(hex(84)), topen (1),  pic(-######,###.00),~
               at (16,02), "Line Item Discounts",                        ~
               at (16,25), fac(hex(84)), torder(2),  pic(-######,###.00),~
               at (16,41), fac(hex(84)), topen (2),  pic(-######,###.00),~
               at (17,02), "Order Level Discounts",                      ~
               at (17,25), fac(hex(a4)), torder(3),  pic(-######,###.00),~
               at (17,41), fac(hex(a4)), topen (3),  pic(-######,###.00),~
               at (18,02), "Net Order Amounts",                          ~
               at (18,25), fac(hex(84)), torder(4),  pic(-######,###.00),~
               at (18,41), fac(hex(84)), topen (4),  pic(-######,###.00),~
                                                                         ~
               at (14,57), "-Statutory Credit Info-",                    ~
               at (15,57), "Bill-to Cust",                               ~
               at (15,70), fac(hex(84))  , billto$              , ch(09),~
               at (16,57), "A/R Balance:",                               ~
               at (16,70), fac(hex(84))  , ar(1)       , pic(-#####,###),~
               at (17,57), "Open Orders:",                               ~
               at (17,70), fac(hex(a4))  , oo(1)       , pic(-#####,###),~
               at (18,57), "      Total:",                               ~
               at (18,70), fac(hex(84))  , totalar1    , pic(-#####,###),~
               at (19,57), fac(hex(84))  , crmsg$(1),                    ~
               at (20,57), fac(hex(84))  , crmsg$(2),                    ~
                                                                         ~
               at (20,10), fac(hex(84)), tlines%(1),           pic(###0),~
               at (20,15), "Lines remaining on order.",                  ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                     keys(pfkey$), key(keyhit%)

            if keyhit% <> 24% then L46032
                crhold$ = " " : crmsg$() = " "
                str(pf$(2%),42%,20%) = " " : str(pfkey$,18%,1%) = hex(ff)
                goto L45150

L46032:     if keyhit% <> 26% then goto L46040
                gosub call_customer_credit
                goto L45150

                                                     /* (EWD) Begin    */
L46040:        gosub close_out_screen
               if close_out% = 1% then goto L45150
                return
                                                     /* (EWD) - End    */
        setpf4                                        /* (EWD) - Begin */
           if str(terms$,1%,3%) = "COD" then cod$ = "C O D"
           if str(terms$,1%,5%) = "COD C" then cod$ = "C O D-CERT"
           if str(terms$,1%,6%) = "2% COD" then cod$ = "2% C O D"
                                                      /* (EWD) - End   */
        if edit% = 2% then L46250         /* Input Mode                 */
           pf$(1) = "(1)Start Over           (8)Customer Display       "&~
                    "             (13)Instructions"
           pf$(2) = "                        (10)S.O. Display          "&~
                    "             (15)Print Screen"
           pf$(3) = "                                         (26)Custo"&~
                    "mer Credit                   "
           pfkey$ = hex(01ffffffffffff08ff0affff0dff0fffffff1a00)
           return

L46250:  if fieldnr% > 0% then L46360     /* Edit Mode- Select Option   */
           pf$(1) = "(1)Start Over    (8)Customer Display              "&~
                    "             (14)Fax S.O. Ack"
           pf$(2) = "                 (9)Return to edit Order (24)Overr"&~
                    "ide Cr Hold  (15)Print Screen"
           pf$(3) = "                 (10)S.O. Display        (26)Custo"&~
                    "mer Credit   (16)Save Order  "
           pfkey$ = hex(01ffffffffffff08090affff0d0e0f10ff181a00)
           if override_crhld$ = "Y" and crhold$ = "H" and                ~
                                              how_held$ <> "M" then L46330
              str(pf$(2%),42%,20%) = " " : str(pfkey$,18%,1%) = hex(ff)
L46330:    return

                                         /* Edit Mode- Field Enabled   */
L46360:    pf$(1) = "(1)Start Over                                     "&~
                    "             (13)Instructions"
           pf$(2) = "                                                  "&~
                    "             (15)Print Screen"
           pf$(3) = "                                                  "&~
                    "                             "
           pfkey$ = hex(01ffffffffffffffffffffff0dff0fffffffff00)
           return

        set_currency_description
            init (" ") currdesc$()
            if currency$ = statutory$ then return
            call "DESCRIBE" (#40, currency$, currdesc$(1), 0%, f1%(40))
            call "CONVERT" (convunt, 2.7, str(currdesc$(2), 1,10))
            currdesc$(2) = currdesc$(2) & " " & currency$
            currdesc$(2) = currdesc$(2) & "/" & statutory$
            return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%, edit%)
            if edit% = 2% then modfld% = 1% /* Fld enabled in EDITMODE */
            errormsg$ = " "
            on fieldnr%      gosub  L50065,         /* Cust Cd, SO #    */~
                                    L50355          /* Rest of fields   */
            return

L50065
*        Test CUSTOMER
           if cuscode$ = " " and so$ <> " " then L50235  /* (EWD) Mod */
            if cuscode$ <> " " or shipto$(1) = " " then L50155
                init(" ") plowhdr$(), incl$()
                mat incl = zer   : mat plowmap = zer
                plowhdr$(1) = "   Customer   Customer Name              "
                plowmap(1) =   1.09 : plowmap(2) =  2    /* Cust Code */
                plowmap(3) =  10.30 : plowmap(4) = 13    /* Cust Name */
                mscdescr$ = hex(06) & "Select Customer Code"
                call "PLOWCODE" (#1, shipto$(1), mscdescr$, 9000%, 1.30, ~
                                     f1%(1), plowhdr$(), 0, 0, incl(),   ~
                                     incl$(), "D", " ", #1, plowmap() )
                if f1%(1) <> 1% then L50175
                cuscode$ = str(mscdescr$,2%,9%)
                if custonly% = 0% then errormsg$ = hex(00)
                return

L50155:     if cuscode$ = " " and so$ <> " " then L50235
              if so$ = " " then L50160
              gosub check_hist
              if errormsg$ <> " " then goto L50176
L50160:         mscdescr$ = hex(06) & "Select Customer Code"
                call "PLOWCODE" (#01, cuscode$, mscdescr$, 0%,.30, f1%(1))
                if f1%(1) = 1% then L50178
L50175:              errormsg$ = "Customer not on file."
L50176:              enabled%(1%) = 1%
                     return
L50178:         if custonly% <> 0% then return
                                                 /* (EWD) - Begin */
                get #01 using L50184, soldto$(), dfltacks$, shipto$(),    ~
                          billto$, cus_txt$, custype$, currency$, parent$
L50184:              FMT XX(39), 6*CH(30), POS(238), CH(1), POS(253),    ~
                   6*CH(30), POS(780), CH(9), ch(4),POS(1023), CH(2),    ~
                         POS(1045), CH(4), CH(9)
                if curr$ <> "Y" then currency$ = " "
                if cus_txt$ <> " " and cus_txt$ <> hex(ffffffff) then    ~
                                       cus_txt% = 1%
                if cus_txt% = 0% then L50200
                   call "TXTFUTIL" (#22, f2%(22%), "LOAD", cus_txt$)
                   cus$ = "YES"
                                                 /*  (EWD) - End  */
L50200:
*        Sales Order Number                    SO$
            if so$ <> " " then L50235
                if soassgn$ = "m" then                                   ~
                     errormsg$ = "Sales Order may not be left blank."
                if errormsg$ <> " " then goto L50294
                     gosub new_order_setup
                     goto  L50321
L50235:     if so$ <> "?" then L50292
                init(" ") plowhdr$()
                mat incl = zer   : mat plowmap = zer : q% = 11%
                msg$ = hex(068c)
                plowhdr$(3%) = hex(a4) & "Select Sales Order to Edit"
                if cuscode$ <> " " then plowhdr$(3%) = plowhdr$(3%) &    ~
                  " for Customer: " & cuscode$ & " - " & shipto$(1)
                readkey$ = str(cuscode$) & hex(00)
                if cuscode$ = " " then u3% = 9000% else u3% = 9009%
                plowhdr$(1) = "  Sales Order       PO Number          " &~
                              "Date       Date      Open Amt"
                if cuscode$ <> " " then L50260
                plowhdr$(1%) = "  Customer  " & str(plowhdr$(1%),2%)
                q% = 0%
                plowmap(1) =   1.09 : plowmap(2) =  1    /* Cust Code */
                plowmap(3) = -10.30 : plowmap(4) = 1002  /* Cust Name */
L50260:         plowmap(5) =  10.16 : plowmap(6) = 12-q% /* SO #      */
                plowmap(7) =  26.16 : plowmap(8) = 30-q% /* PO #      */
                plowmap(9) = 806.061: plowmap(10)= 48-q% /* Order Date*/
                plowmap(11)= 818.061: plowmap(12)= 58-q% /* Due   Date*/
                plowmap(13)= 867.08 : plowmap(14)=68.1042 - q% /* Order*/
                str(msg$,51%-q%) = "Order      Due"
                call "PLOWCODE" (#05, readkey$, msg$, u3%, 0.30, f1%(5), ~
                 plowhdr$(),0,-1, incl(),incl$(), "d"," ", #1, plowmap())
                if f1%(5) = 1% then L50290
                     errormsg$ = hex(00) : goto L50294
L50290:         cuscode$ = str(readkey$,,9)
                so$      = str(readkey$,10)
L50292:         gosub check_hist
                  if errormsg$ = " " then goto L50297
L50294:              enabled%( 2) = 1%
                     return
L50297:     call "BCKARISB" (so$, errormsg$) /* Check for SO in ARI SSN */
            if errormsg$ <> " " then L50294

            gosub load_sales_order
            if crhold$ = "C" then L50332
L50303:     if errormsg$ <> " " then goto L50294
                if soonfile% = 0% then L50318
                     call "READ100" (#01, cuscode$, f1%(1))
                     get #01 using L50311, dfltacks$
L50311:                   FMT POS(238), CH(1)
                                                /* (EWD) - Begin */
                     sp$ = "00"
                     gosub check_credit
                     return clear all
                     goto summary_screen
L50318:         if cuscode$ = " " then L50160 /* Go get customer        */
L50321:              gosub customer_status_checker
                        sp$ = "00"
                        gosub check_credit
                        return
L50325:     return clear all
            goto startover2
                                                /* (EWD) - End    */
L50332:     u3% = 2%
            call "ASKUSER" (u3%, "** CANCELLED ORDER **", "* Press PF10"&~
                " to Re-activate Order", "- Open Quantities MIGHT need "&~
                "to be Changed -", "* Press PF16 to exit")
            if u3% <> 16% and u3% <> 10% then L50332
            if u3% = 16% then L50325
              crhold$ = " "
              if maxlines% = 0% then L50352
                for c% = 1 to maxlines%
                  openqty(c%) = order(c%)
                next c%
L50352:       goto L50303

L50355
*        Ship-to                               SHIPTO$
            if str(shipto$()) <> " " then goto L50380
                errormsg$ = "Ship-to may not be blank"
                enabled%( 3) = 1%
                return

L50380
*        Store Code                            STORE$
            mscdescr$ = hex(06) & "Please Select Store."
            f1%(12) = -9%
            store$ = "300"               /* (EWD) - Mod 01/26/96 */
            call "PLOWCODE" (#12, store$, mscdescr$, 0%, .3, f1%(12))
            if f1%(12) = 1% then L50410
                errormsg$ = "Invalid Store Code."
                enabled%( 4) = 1%
                return
L50410:     if edit% = 2% then goto L50460
                call "ARMGLGET" (1%, cuscode$, " ", " ", " ", store$,    ~
                                 " ", #02, #01, #04, #11, #03, dfltsales$)
                call "ARMGLGET" (2%, cuscode$, " ", " ", " ", store$,    ~
                                 " ", #02, #01, #04, #11, #03, discacct$)

L50460: REM Validate Export Order flag          EXPORT$
            if export$ = " " then export$ = "N"
            if export$ = "Y" or export$ = "N" then goto L50540
                errormsg$ = "Enter 'Y' or 'N' for Export Order flag."
                enabled%( 5) = 1%
                return

L50540
*        PURCHASE ORDER NUMBER                 PO$
          if po$ = " " then goto L50545
               gosub check_po                   /* (EWD) MOD 0 01/26/96 */
               if po% = 1% then goto L50065
               goto L50600
L50545:         get #01 using L50546, poreqd$
L50546:              FMT POS(1020), CH(1)
                if poreqd$ <> "Y" then goto L50560
                     errormsg$ = "PO is required for this Customer."
L50552:              enabled%( 6) = 1%
                     return
L50560
*        Now test for duplicate PO for this customer
            if po$ = " " then L50600
                call "BCKDUPPO" (cuscode$, so$, po$, errormsg$)
                if errormsg$ <> " " then L50552

L50600
*        How Ship                              HOWSHIP$
         gosub check_howship                   /* (EWD) - Mod 02/16/98 */

*        FOB                                   FOB$
         gosub check_fob                       /* (EWD) - Mod 02/16/96 */

*        Order Date                            ORDERDATE$
            if len(orderdate$) < 6 then orderdate$ = date /* (EWD)-Mod */
            call "DATEOK" (orderdate$, u3%, errormsg$)
            if errormsg$ = " " then goto L50630
                errormsg$ = errormsg$ & " (Order Date)"
                enabled%( 9) = 1%
                return

L50630
*        Default Due Date                      DFLTDUE$
            if (dfltdue$ = " " or dfltdue$ = blankdate$) and ~
                   mlq_hdate% <> 0% then return
                                               /* (EWD) - Begin 04/18/96*/
               call "APCPLN3B" (dfltdue$, cuscode$, fob$, #1, #8)
               gosub check_fob
                                               /* (EWD) - End           */
            if (dfltdue$ = " " or dfltdue$ = blankdate$) and ~
                   copy% = 0% then goto L50670
                call "DATEOK" (dfltdue$, u3%, errormsg$)
                if errormsg$ = " " then goto L50650
L50646:              errormsg$ = errormsg$ & " (Due Date)"
                     enabled%(10) = 1%
                     return
L50650:         testdate$ = dfltdue$
                gosub order_date_check
                if errormsg$ <> " " then L50646
            if mlq_hdate% <> 0% then return

L50670
*        Default Req'd Ship Date               DFLTSHIP$
            if (dfltship$ = " " or dfltship$ = blankdate$) and ~
                    mlq_hdate% <> 0% then return
               dfltship$ = dfltdue$            /* (EWD) - 02/22/98  */
  
            if (dfltship$ = " " or dfltship$ = blankdate$) and ~
                    copy% = 0% then goto L50795
                call "SPCSMASH" (dfltship$)
                if str(dfltship$,,1) <> "-" then L50745
                     if dfltdue$ = " " or dfltdue$ = blankdate$ then L50710
                     convert dfltship$ to temp%, data goto L50705
                     goto L50725
L50705:                   errormsg$ = "Invalid days offset." : goto L50751
L50710:                   errormsg$ = "Can not use offset if Due Date" & ~
                                      " is left blank."
                          goto L50751
L50725:              call "DATUNFMT" (dfltdue$)
                     call "DATE" addr("G+", str(dfltdue$,,6), temp%,     ~
                                      str(dfltship$,,6), u3%)
                     call "DATEFMT" (dfltdue$)
L50745:         call "DATEOK" (dfltship$, u3%, errormsg$)
                if errormsg$ = " " then goto L50755
L50751:              errormsg$ = errormsg$ & " (Ship Date)"
                     enabled%(11) = 1%
                     return
L50755:         testdate$ = dfltship$ : testc% = 0%
                gosub order_date_check
                if errormsg$ <> " " then goto L50751
                     gosub check_for_weekend
                     call "DATUNFMT" (dfltship$)
                     call "DATE" addr("GD", str(dfltship$,,6), weekdayh$,~
                          u3%)
                     call "DATEFMT" (dfltship$)
                     if mlq_hdate% <> 0% then return

L50795
*        Test Data for Precious Metal Surcharge at SO Entry
            if pm_on$ <> "Y" then L50850
            if pos("YN" = pm_so$) > 0 then L50850
                errormsg$ = "Enter 'Y' or 'N' for PM SO Surcharge"
                enabled%(12) = 1%
                return

*        Test Data for Precious Metal Surcharge at Invoice Update
            if pos("YN" = pm_inv$) > 0 then L50850
                errormsg$ = "Enter 'Y' or 'N' for PM INV Surcharge"
                enabled%(13) = 1%
                return

L50850
*        Sold-to                               SOLDTO$
            if short% <> 0% then return /* Validating 11 or 21 fields? */
            if soldto$(1) = " " then str(soldto$()) = " "

*        Cancellation Date                     CANCELDATE$
            if canceldate$ = " " or canceldate$ = blankdate$ then goto L50910
                call "DATEOK" (canceldate$, u3%, errormsg$)
                if errormsg$ = " " then goto L50890
L50886:              errormsg$ = errormsg$ & " (Cancel Date)"
                     enabled%(15) = 1%
                     return
L50890:         testdate$ = canceldate$
                gosub order_date_check
                     if errormsg$ <> " " then L50886

L50910
*        Price Code Default                    PC$
                                               /* (EWD) - Begin       */
            if (pc$ >= "A" and pc$ <= "Z") or                            ~
               (pc$ >= "0" and pc$ <= "9") then goto L50955
                errormsg$ = "Valid Price Codes are 'A'-'Z' and '0'-'9'"
                                               /* (EWD) - End         */
                enabled%(16) = 1%
                return

L50955
*        Order Discount Percent                ORDERDISC$
            orderdisc = 0
            if orderdisc$ = " " then orderdisc$ = "0"
            convert orderdisc$ to orderdisc, data goto L50975 : goto L50980
L50975:         errormsg$ = "Order Discount must be -25 to 100%"
                enabled%(17) = 1%
                return
L50980:     if orderdisc < -25 or orderdisc > 100 then L50975
            call "CONVERT" (orderdisc, 2.2, orderdisc$)

*        Payment Terms (Code)                  TERMS$
            if terms$ = " " then goto L51035
                mscdescr$ = hex(06) & "Select Payment Terms Code"
                call "PLOWCODE" (#21, terms$, mscdescr$, 0%, .3, f1%(21))

L51035
*        Region Code                           REGION$
            mscdescr$ = " " : if region$ = " " then goto L51085
                mscdescr$ = hex(06) & "Select Sales Region"
                readkey$ = "REGIONS  " & region$
                f1%(8) = -14%
                call "PLOWCODE" (#08, readkey$, mscdescr$, 9%,.3,f1%(8))
                if f1%(8) = 1% then L51070
                     errormsg$ = "Invalid Sales Region Code."
                     enabled%(19) = 1%
                     return
L51070:         region$ = str(readkey$,10)

L51085
*        Salesman Code / Split %               SALESMAN$()
            total% = 0%
            for i% = 1% to 3%
                if salesman$(i%) <> " " then L51112
                     mscdescr$, comm$(i%) = " "
                     comm%(i%) = 0%  :  goto L51170
L51112:         f1%(7) = -14%
                mscdescr$ = hex(06) & "Select Salesman Code"
                call "PLOWCODE" (#07, salesman$(i%), mscdescr$,          ~
                                                       0%, 0.30, f1%(7))
                if f1%(7) = 1% then L51140
                     errormsg$ = "Salesman Code " & salesman$(i%) &      ~
                                 " not on file."
L51136:              enabled%(20) = 1%
                     return
L51140:         if comm$(i%) = " " then comm$(i%) = "0"
                convert comm$(i%) to comm%(i%), data goto L51155
                goto L51160
L51155:              errormsg$ = "Commission % must be 0 - 100."
                     goto L51136
L51160:         total% = total% + comm%(i%)
                convert comm%(i%) to comm$(i%), pic(##0)
L51170:     next i%
            if total% <= 100% then goto L51195
                errormsg$ = "Total Commission Splits can not exceed 100%."
                goto L51136

L51195
*        Sales Account Default                 DFLTSALES$
            mscdescr$ = " " : if dfltsales$ = " " then goto L51235
                mscdescr$ = hex(06) & "Select Sales Acct Default."
                call "PLOWCODE" (#03,dfltsales$,mscdescr$,0%,.3,f1%(3))
                if f1%(3) = 1% then goto L51235
                     errormsg$ = "Invalid Sales Account Code"
                     enabled%(21) = 1%
                     return

L51235
*        Sales Discounts Account               DISCACCT$
            mscdescr$ = hex(06) & "Select Sales Discounts Account."
            call "PLOWCODE" (#03, discacct$, mscdescr$, 0%, 0.3, f1%(3))
            if f1%(3) = 1% then goto L51270
                errormsg$ = "Invalid Sales Discounts Account"
                enabled%(22) = 1%
                return

L51270
*        Shipping Instructions                 SHIPINSTR$()
            return

*        See if order is in history files
        check_hist
            readkey$  = str(so$) & hex(00)
            call "PLOWNEXT" (#30, readkey$, 16%, f1%(30))
              if f1%(30) = 0% then L51360
                errormsg$ = "Sales Order number " & so$ & " is in the" & ~
                            " History Files."
L51360:         return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the Line Item Elements.                     *~
            *************************************************************

        deffn'153(fieldnr%, edit%)
            if edit% = 2% then modfld% = 1% /* Fld enabled in EDITMODE */
            errormsg$ = " "
            on fieldnr%      gosub  L52070,         /* P/N, Qty, Curr   */~
                                    L52370          /* Rest of fields   */
            if edit% = 2% and errormsg$ = " " then gosub describe_line
            return

L52070
*        Part Code                             PART$()
            if keyhit% = 22% or lastkey% = 23% then L52160
            gosub build_part                   /* (EWD) - Begin    */
            if part$(c%) = " " then L52088
            if len(part$(c%)) < 19 then goto L52102

                call "READ100" (#04, part$(c%), f1%(4%))
                if f1%(4%) = 1% then L52104
                     call "HNYGREF" (part$(c%), #15, #04, f1%(4%))
                     if f1%(4%) = 0% then L52102
                          call "READ100" (#04, part$(c%), f1%(4%))
                          if f1%(4%) = 1% then L52104
                             goto L52102
                                               /* (EWD) - End     */
L52088:     f1%(4%) = -14%
            call "PLOWCODE" (#04, part$(c%), " ", 0%, 0.32, f1%(4%))
            if f1%(4%) = 1% then L52104
                if part$(c%) <> " " then L52102
                     errormsg$ = "Part Code can't be blank"
L52098:              enabled%(1%) = 1%
                     return
L52102:         nonstockmsg$(c%) = "NonStk"  :  goto L52200
L52104:     get #04 using L52106, stkuom$(c%), cat$(c%), partflag$,       ~
                parttype$(c%)
L52106:              FMT POS(74), CH(4), POS(90), CH(4), POS(166), CH(4),~
                          POS(180), CH(3)

        REM Get Corresponding Customer Part For This CMS Part...
            if refpart$(c%) = " " then                                   ~
                 refpart$(c%) = "** No Cross reference **"
*          CALL "PTXREFSB" (2%, "C", CUSCODE$, REFPART$(C%),            ~
*              REFDESC$(C%), PART$(C%), DESCR$(C%), #4, RET%)
*          IF RET% = 1% THEN REFTYPE$(C%) = "C"
*          IF RET% = 1% THEN REFFLAG% = 1% ELSE                         ~
*              REFPART$(C%) = "** No Cross reference **"
            if parttype$(c%)  = "000" and so$ = " " then                 ~
                                                   gosub assign_so_number
L52120: REM Finish Checking CMS Part...
            if partflag$ = " " then goto L52142
                u3% = 2%
                call "ASKUSER" (u3%, "PART FLAGGED",                     ~
                          "This Part is flagged as '" & partflag$ & "'.",~
                          "Press PF-16 to continue and use Part,",       ~
                          "Or press (RETURN) to re-enter Part Number.")
                if u3% = 16% then L52142
L52136:            errormsg$ = "Re-enter Part Number"
                   goto L52098

L52142:     call "ARIEXTRA" (cuscode$, part$(c%), " ", temp%, #2)
            if temp% <= 0% then L52200
            if c% + total_e_lines% + temp% <= 100% then L52152
               gosub extra_append_conflict
               if u3% = 0% then L52136
L52152:           e_lines%(c%) = temp%
                  total_e_lines% = total_e_lines% + temp%
                  goto L52200
        REM Get CMS Part For This Cross Reference Part...
L52160:     refpart$(c%) = part$(c%)
            if lastkey% = 23% then L52171
        REM Customer Cross Reference...
            call "PTXREFSB" (1%, "C", cuscode$, refpart$(c%),            ~
                 refdesc$(c%), part$(c%), descr$(c%), #4, ret%)
            if ret% = 1% then reftype$(c%) = "C"
            if ret% = 1% then L52193
                if part$(c%) = " " then                                  ~
                    errormsg$ = "Part Number CANNOT Be Blank" else       ~
                    errormsg$ = "No Cross Ref Part For This Customer Part"
                goto L52098
L52171: REM Manufacturer's Cross Reference...
            if mfgcode$ = "?" then mfgcode$ = " "
            mfgdescr$ = hex(06) & "Select Manufacturer's Code"
            readkey$ = "MFG CODES" & str(mfgcode$,1%,9%)
            call "PLOWCODE" (#8, readkey$, mfgdescr$, 9%, .3, f1%(8%))
               if f1%(8%) = 1% then L52178
                    errormsg$ = "Invalid Manufacturer's Code" : goto L52098
L52178:     mfgcode$ = str(readkey$,10%)
            call "PTXREFSB" (1%, "M", str(mfgcode$,1%,9%),  refpart$(c%),~
                 refdesc$(c%), part$(c%), descr$(c%), #4, ret%)
            if ret% = 1% then reftype$(c%) = "M"
            if ret% = 1% then L52187
                if part$(c%) = " " then                                  ~
                   errormsg$ = "Part Number CANNOT Be Blank"  else       ~
                   errormsg$ = "No Cross Ref Part For This Mfg Part"
                   goto L52098
L52187:     call "GETCODE" (#4, part$(c%), " ", 0%, 0, f1%(4%))
            if f1%(4%) = 1% then L52193
                if part$(c%) = " " then                                  ~
                    errormsg$ = "Part Number CANNOT Be Blank"  else      ~
                    errormsg$ = "Part No Longer Exists In HNYMASTR File"
                    goto L52098
L52193:     get #04 using L52194, cat$(c%), partflag$
L52194:         FMT POS(90), CH(4), POS(166), CH(4)
            refflag% = 1%
            goto L52120

L52200
*        Check for Valid Model First         /* (EWD) - Begin 02/16/98 */
         gosub lookup_model
         if errormsg$ <> " " then goto L52098 /* (EWD) - End           */            
*        Original Order Quantity               ORDER$
            gosub test_order_quantity
                if errormsg$ = " " then goto L52230
                     enabled%( 2) = 1%
                     return

L52230
*        Currency code                       CURRENCY$
            gosub test_currency_code
                if errormsg$ <> " " then return else goto L52325

        test_currency_code
            if c% = 1% and edit% <> 2% and curr$ = "Y" then L52244
            if enabled%(3) = 0% then return
L52244:     if currency$ = " " then currency$ = statutory$
            call "GETCODE" (#40, currency$, " ", 0%, 0, f1%(40))
            if f1%(40) <> 0% then goto L52260
                errormsg$ = "Invalid Currency code."
L52256:         enabled%( 3) = 1%
                return
L52260:     convdate$ = " " : conveqv, convunt = 1
            if currency$ = statutory$ then return
            call "DATREVRS" ( orderdate$, rev_date$, errormsg$ )
            if errormsg$ <> " " then goto L52256
            currkey$ = str(currtype$) & str(currency$) & str(rev_date$,,6)
            call "PLOWNEXT" (#42, currkey$, 5%, f1%(42))
            if f1%(42) <> 0% then goto L52315
                errormsg$ = "Invalid Currency Code for this transaction."
                goto L52256
L52315:     get #42 using L52320, convdate$, conveqv, convunt
L52320:         FMT POS(12), CH(6), 2*PD(14,7)
            return

L52325
*        Force look-up of price
            gosub L22000             /* Get Part default values */
*          GOSUB 23010             /* Force CPRASSGN to get price */
            errormsg$ = " "
            if price(c%) < 0 then price(c%) = 0

L52350
*        Apply Part Code Min Quantity and Min Increment to Order Quantity
            minsoqty, minsoinc = 0
            if nonstockmsg$(c%) <> " " then return
            call "READ100" (#04, part$(c%), f1%(4))
                if f1%(4) <> 1% then L52357
            get #04 using L52356, minsoqty, minsoinc
L52356:         FMT POS(706), 2*PD(14,4)
L52357:     if fieldnr% = 2% and order(c%) = orderqtysave then return
                orderqtysave = order(c%)
L52359:     call "BCKMINSB" (order(c%), minsoqty, minsoinc, u3%)
            if u3%  = 16% then return
            if u3% <>  1% then goto L52359
                errormsg$ = hex(00)
                return

L52370
*        Part Description                      DESCR$()

*        Part Category                         CAT$()
            mscdescr$ = hex(06) & "Select Part Category"
            f1%(11) = -14%
            call "PLOWCODE" (#11, cat$(c%), mscdescr$, 0%, 0.3, f1%(11))
            if f1%(11) = 1% or cat$(c%) = " " then goto L52415
                errormsg$ = "Category Code not on file"
                enabled%( 5) = 1%
                return

L52415
*        Due Date                              DUEDATE$()
L52420:     call "DATEOK" (duedate$(c%), u3%, errormsg$)
            if errormsg$ = " " then goto L52440
L52430:         errormsg$ = errormsg$ & " (Due Date)"
                enabled%( 6) = 1%
                return
L52440:     testdate$ = duedate$(c%)
            gosub order_date_check : if errormsg$ <> " " then L52430
               if origdue$(c%) = " " or origdue$(c%) = blankdate$ ~
                  then origdue$(c%) = duedate$(c%)
            if mlq_ddate% <> 0% then return

L52460
*        Required Ship Date                    SHIPDATE$()
            call "SPCSMASH" (shipdate$(c%))
            if str(shipdate$(c%),,1) <> "-" then L52510
                convert shipdate$(c%) to temp%, data goto L52485
                goto L52490
L52485:              errormsg$ = "Invalid days offset." : goto L52520
L52490:         call "DATUNFMT" (duedate$(c%))
                call "DATE" addr("G+", str(duedate$ (c%),,6), temp%,     ~
                                       str(shipdate$(c%),,6), u3%)
                call "DATEFMT" (duedate$(c%))
L52510:     call "DATEOK"  (shipdate$(c%), u3%, errormsg$)
            if errormsg$ = " " then goto L52530
L52520:         errormsg$ = errormsg$ & " (Ship Date)"
L52521:         enabled%( 7) = 1%
                return
L52530:     testdate$ = shipdate$(c%) : testc% = c%
            gosub order_date_check
            if errormsg$ <> " " then goto L52520
                 gosub check_for_weekend
                 if errormsg$ <> " " then goto L52521
                 call "DATUNFMT" (shipdate$(c%))
                 call "DATE" addr("GD", str(shipdate$(c%),,6), weekday$, ~
                     u3%)
                 call "DATEFMT"  (shipdate$(c%))
                 gosub get_atc
                 if mlq_ddate% <> 0% then return

*        Get Options for this part (Input Mode Only)
            if edit% = 1% then gosub options  /* (EWD) Mod 03/05/98 */

*        Stocking Unit Of Measure              STKUOM$()
            stkuom$(c%) = "EA"                 /* (EWD) Mod 02/16/98 */
            mscdescr$ = " "  :  if uom% = 1% then L52600
                if stkuom$(c%) <> " " then goto L52640
                     errormsg$ = "Stocking Unit of Measure can't be blank"
L52596:              enabled%( 8) = 1%
                     return
L52600:     mscdescr$ = hex(06) & "Select Stocking UOM"
            readkey$ = "UOM      " & stkuom$(c%)
            f1%(8) = -14%
            call "PLOWCODE" (#08, readkey$, mscdescr$, 9%, 0.30, f1%(8))
            if f1%(8) = 1% then stkuom$(c%) = str(readkey$,10)           ~
                           else errormsg$   = "Stocking UOM not on file"
            if errormsg$ <> " " then goto L52596

L52640
*        Pricing Unit of Measure               PRICEUOM$()
            priceuom$(C%) = "EA"               /* (EWD) Mod 02/16/98 */
            mscdescr$ = " "  :  if uom% = 1% then L52665
                if priceuom$(c%) <> " " then goto L52705
                     errormsg$ = "Pricing Unit of Measure can't be blank"
L52661:              enabled%( 9) = 1%
                     return
L52665:     mscdescr$ = hex(06) & "Select Pricing UOM"
            readkey$ = "UOM      " & priceuom$(c%)
            f1%(8) = -14%
            call "PLOWCODE" (#08, readkey$, mscdescr$, 9%, 0.30, f1%(8))
            if f1%(8) = 1% then priceuom$(c%) = str(readkey$,10)         ~
                           else errormsg$    = "Pricing UOM not on file"
            if errormsg$ <> " " then goto L52661

L52705
*        Conversion Pricing to Stocking        CONV$
            if priceuom$(c%)= stkuom$(c%) or conv$= " " then conv$ = "1"
            call "NUMTEST" (conv$, 0, 9e7, errormsg$, 7.7, conv(c%))
            if errormsg$ = " " then goto L52731
                errormsg$ = errormsg$ & " (Conv Factor)"
                enabled%(10) = 1%
                return

L52731
*        Original Order Quantity               ORDER$
            gosub test_order_quantity
                if errormsg$ = " " then goto L52742
                     enabled%( 2) = 1%
                     return

L52742
*        Allocation Qty, Instruction           ALLOCQTY$
            if pos("NACZP" = alloc$(c%)) > 0% then goto L52756
                errormsg$ = "Enter 'N', 'A', 'P', 'C', or 'Z' for" &     ~
                     " Allocation Flag."
L52751:         enabled%(11) = 1%
                return
L52756:     if alloc$(c%) = allocsave$ then goto L52765
                allocsave$ = alloc$(c%)
                if demstatus$ < "2" then goto L52765
                     errormsg$ = "Must Unplan to change Allocation."
                     goto L52751

L52765
*        Open Order Quantity                   OPENQTY$
            if openqty$ = " " then openqty$ = "0"
            q$ = openqty$  :  gosub uom_trans  :  openqty$ = q$
            call "NUMTEST" (openqty$, 0,9e7, errormsg$, 2.2, openqty(c%))
            if errormsg$ = " " then goto L52795
                errormsg$ = errormsg$ & " (Open Qty)"
L52792:         enabled%(12) = 1%
                return
L52795:     if openqty(c%) >= qtyschld(c%) then L52825
                errormsg$ = "Cannot reduce Open Quantity less than" &    ~
                            " Quantity Scheduled."
                goto L52792

L52825
*        Lot Number                            LOT$()
            if lot$(c%) <> " " then call "LOTVALID" (part$(c%), store$,  ~
                                      lot$(c%), #02, #04, #13, errormsg$)
            if errormsg$ = " " then goto L52850
L52846:         enabled%(13) = 1%
                return
L52850:     call "LOTUNQUE" (part$(), lot$(), c%, #02, errormsg$)
            if errormsg$ <> " " then goto L52846

*        Total Quantity Shipped                SHIP$
            if ship$ = " " then ship$ = "0"
            q$ = ship$  :   gosub uom_trans  :  ship$ = q$
            call "NUMTEST" (ship$, 0,9e7, errormsg$, 2.2, ship(c%))
            if errormsg$ = " " then goto L52890
                errormsg$ = errormsg$ & " (Ship Qty)"
                enabled%(14) = 1%
                return

L52890
*        Currency code                       CURRENCY$
            gosub test_currency_code
                if errormsg$ <> " " then return

*        Unit Price                            PRICE$
            if price$ <> " " then goto L52904   /* (EWD) - Mod   */
*           if parttype$(c%)= "000" then L52904 /*skip pricing if generic*/
                gosub call_cprassgn
            if errormsg$ = " " then goto L52903
                enabled%(15) = 1%
                return
L52903:         call "CONVERT" (price(c%), -4.4, price$)
L52904:     call "NUMTEST" (price$, 0,9e7, errormsg$, 4.4, price(c%))
            if errormsg$ = " " then goto L52915
                errormsg$ = errormsg$ & " (Unit Price)"
                enabled%(15) = 1%
                return

L52915
*        Line Item Discount %                  LINEDISC$
            call "NUMTEST" (linedisc$,-50,100,errormsg$,2.2,linedisc(c%))
            gosub pricing_stuff
            if errormsg$ = " " then goto L52930
                errormsg$ = errormsg$ & " (Discount)" : enabled%(16) = 1%
                return

L52930
*        Test for Extension being too large
            if ext < 1e9 then L52940
                errormsg$ = "Extension is TOO large, please "         &  ~
                            "reduce Order Quantity."
                fieldnr% = 1%
                return

L52940
*        Line Taxable? (Y/N)                   TAXABLE$()
            if pos("YN" = taxable$(c%)) <> 0 then goto L52960
                errormsg$ = "Enter 'Y' or 'N' for Taxable"
                enabled%(17) = 1%
                return

L52960
*        P.O. Item                             ITEM$()

*        Project Number                        PROJECT$()
            if project$(c%) = " " then goto L53035
            f1%(14) = -14%
            mscdescr$ = hex(06) & "Select Project Code"
            call "PLOWCODE"(#14, project$(c%), mscdescr$, 0%, .3, f1%(14))
            if f1%(14) = 1% then L53000
                errormsg$ = "Project not on file."
L52996:         enabled%(19) = 1%
                return
L53000:     get #14, using L53005, testdate$
L53005:         FMT XX(44), CH(06)
            if testdate$ = " " or testdate$ = blankdate$ or ~
               testdate$ > date$ then goto L53035
                call "DATEFMT" (testdate$)
                errormsg$ = "Project was closed on " & testdate$
                goto L52996

L53035
*        Sales Distr. Account                  SALESACCT$()
            mscdescr$ = hex(06) & "Select Sales Account."
            f1%(3) = -14%
            call "PLOWCODE" (#03,salesacct$(c%),mscdescr$,0%,.3,f1%(3))
            if f1%(3) = 1% then goto L53075
                errormsg$ = "Invalid Sales Account Code"
                enabled%(20) = 1%
                return

L53075
*        Sales Discounts Account               DISCACCTL$()
            mscdescr$ = hex(06) & "Select Sales Discounts Account."
            f1%(3) = -14%
            call "PLOWCODE" (#03,discacctl$(c%),mscdescr$,0%,.3,f1%(3))
            if f1%(3) = 1% then goto L53107
                errormsg$ = "Invalid Sales Discounts Account"
                enabled%(21) = 1%
                return

L53107
*        Shipping Priority Code                SHIPCODE$()
            if shipcode$(c%) = " " then shipcode$(c%) = shipcode$
                if shipcode$(c%) >= "0" and                              ~
                   shipcode$(c%) <= "5" then L53115
                errormsg$ = "Enter 0 - 5 for Shipping Priority."
                enabled%(22) = 1%
                return

L53115
*        Planning Priority Code                PRIORITY$()
            if priority$(c%) >= "A" and priority$(c%) <= "Z" then L53140
                errormsg$ = "Enter 'A' - 'Z' for Priority Code."
                enabled%(23) = 1%
                return

L53140
*        Planning Demand Type                  DEMTYPE$()
*        Last L.I. field. If OK, test Order Qty vs. HNYMASTR minimums.
            if demtype$(c%) = "1" or demtype$(c%) = "2" then L52350
                errormsg$ = "Planning Demand type must be '1' or '2'"
                enabled%(24) = 1%
                return

*        Open Field
*          Field Disabled
            return

*        Open Field
*          Field Disabled
            return

        test_order_quantity
                                               /* (EWD) - Begin      */
            if order$ = " " then order$ = "1"
            openqty$ = order$
                                               /* (EWD) - End        */
            q$ = order$  :  gosub uom_trans  :  order$ = q$
            call "NUMTEST" (order$, 0, 9e7, errormsg$, 2.2, order(c%))
            if errormsg$ <> " " then errormsg$ = errormsg$ & " (Order Q"&~
                "ty)"
            return

        get_atc
            pip%, shelf%, atc1%, atc2%, err%, atch1%, atch2%, horzn% = 0%
            atc$(c%,1%), atc$(c%,2%) = " "
            call "DATUNFMT" (shipdate$(c%))
            call "PIPATCDZ" (part$(c%), shipdate$(c%), #24, #02, #28,    ~
                            pip%, shelf%, atc1%, atc2%, err%, atch1%,    ~
                            atch2%, horzn%)
            call "DATEFMT" (shipdate$(c%))
            if err% <> 1% then L53340
                atc1$ = "    DATE" : atc2$ = "PROBLEM"
L53340:     if err% <> 2% then L53360
                atc1$ = "     NOT" : atc2$ = "AVAIL"
L53360:     atc$(c%,1%) = "ATC1/2 for Ship Date: " & atc1$ & " / " & atc2$
                if err% <> 0% then return
            convert atc1%  to  atc1$, pic(-#######)
            convert atc2%  to  atc2$, pic(-#######)
            convert atch1% to atch1$, pic(-#######)
            convert atch2% to atch2$, pic(-#######)
            convert horzn% to atchz$, pic(###)
            call "STRING" addr("LJ",  atc2$, 8%)
            call "STRING" addr("LJ", atch2$, 8%)
            atc$(c%,1%) = "ATC1/2 for Ship Date: " & atc1$ & " / " & atc2$
            if atc1% < 0% then str(atc$(c%,1%),22%,1%) = hex(94)         ~
                          else str(atc$(c%,1%),22%,1%) = hex(84)
            str(atc$(c%,1%),31%,1%) = hex(8c)
            if atc2% < 0% then str(atc$(c%,1%),33%,1%) = hex(94)         ~
                          else str(atc$(c%,1%),33%,1%) = hex(84)
            atc$(c%,2%) = "ATC w/Horizon of xxx: "& atch1$ &" / " & atch2$
            str(atc$(c%,2%),18%,3%) = atchz$
            if atch1% < 0% then str(atc$(c%,2%),22%,1%) = hex(94)        ~
                           else str(atc$(c%,2%),22%,1%) = hex(84)
            str(atc$(c%,2%),31%,1%) = hex(8c)
            if atch2% < 0% then str(atc$(c%,2%),33%,1%) = hex(94)        ~
                           else str(atc$(c%,2%),33%,1%) = hex(84)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Data Save Screen               *~
            *************************************************************


        deffn'154(fieldnr%)
            errormsg$ = " "

*        Adj Reason Code
            if crhold$ = "C" then L55190
            if adjrsn$ = " " and manadjre$ <> "Y" then L55270
            adjrsndescr$ = hex(06) & "Select Adjust Reason Code"
            readkey$ = "SO REASON" & adjrsn$
            call "PLOWCODE" (#08, readkey$, adjrsndescr$, 9%, 0.3, f1%(8))
            if f1%(8) = 1% then adjrsn$ = str(readkey$,10) else          ~
                            errormsg$ = "Invalid Adjustment Reason Code."
            if errormsg$ <> " " then return
            if modfld% <> 1% or manadjre$ <> "Y" then goto L55270
                if adjrsn$ <> " " then goto L55270
                    errormsg$ = "Adjustment Reason Code may not be blank."
                    return

L55190:     if can$ <> "Y" then L55270
            adjrsndescr$ = hex(06) & "Select Cancel Reason Code"
            readkey$ = "CANREASON" & adjrsn$
            call "PLOWCODE" (#08, readkey$, adjrsndescr$, 9%, 0.3, f1%(8))
            if f1%(8) = 1% then adjrsn$ = str(readkey$,10) else          ~
                          errormsg$ = "Invalid Cancellation Reason Code."
            if errormsg$ <> " " then return

L55270
*        Pick List, BOL, and Acknowledgement Printing
*          No real validation needed, so just encode PICKPRINT$
        encode_pickprint
            pickprint% = 0%
            for i% = 1% to 3%
                if pickprint$(i%) = " " then L55350
                     pickprint$(i%) = "X"
                     pickprint% = pickprint% + (1% + ((i% - 1%) * 2%))
L55350:         next i%
            convert pickprint% to pickprint$, pic(#)
            return

        REM *************************************************************~
            *         M I S C .   T E S T   S U B R O U T I N E S       *~
            *-----------------------------------------------------------*~
            * Some subroutines to aid the above test sections.          *~
            *************************************************************

        order_date_check
*         Checks that TESTDATE$ (valid and formatted) is on or after
*         the order date
            call "DATUNFMT" (testdate$)
            call "DATUNFMT" (orderdate$)
            if testdate$ < orderdate$ then                               ~
                            errormsg$ = "Must be on or after Order Date"
            call "DATEFMT" (orderdate$)
            if errormsg$ = " " then return
               errormsg$ = errormsg$ & " (" & orderdate$ & ")"
               return

        check_for_weekend
            call "DAY" addr(str(testdate$,,6%), day%)
            if day% > 1% and day% < 7% then return
            if day% = 1% then day$ = "SUNDAY" else day$ = "SATURDAY"
            call "DATEFMT" (testdate$)
            if testc% = 0% then L56401  /* Must be the Default Ship Date */
                if testdate$ = dfltship$ then L56412  /* Seen Msg Before */
L56401:     weekend% = 2%
            call "ASKUSER" (weekend%, "*** WARNING ***", "The Req'd Ship"~
                    & " Date, " & testdate$ & ", Falls on " & day$ & ".",~
                    "Press PF-1 to Acknowledge Warning and Continue   ", ~
                    "-or- Press RETURN to Re-enter Date.")
            if weekend% = 1% then L56412
                if weekend% <> 0% then L56401
                     errormsg$ = "Please Re-enter Required Ship Date."
L56412:     call "DATUNFMT" (testdate$)
            return

        uom_trans  /* Translate entry from UOM entered to Stocking UOM */
            p% = pos(q$ = "/") : if p% = 0% then return
            if p% < 10% then L56460
                errormsg$ = "Invalid specification for UOM" : return
L56460:     u$ = str(q$,p%+1%) : q$ = str(q$,,p%-1%)
            convert q$ to q, data goto L56480  :  goto L56490
L56480:         errormsg$ = "Invalid entry for Quantity" : return
L56490:     readkey$ = "UOMCONV  " & str(u$,,4) & "-" & stkuom$(c%)
            call "READ100" (#08, readkey$, f1%(8))
            if f1%(8) = 1% then L56540
L56520:         errormsg$ = "Conversion factor invalid for UOM " & u$
                return
L56540:     get #08 using L56550, c$
L56550:         FMT XX(24), CH(10)
            convert c$ to c, data goto L56520
            q = round(q*c, 4)  :  call "CONVERT" (q, 4.4, str(q$,,10))
            return

                                                 /* (EWD) Mod 02/16/94 */
        REM *************************************************************~
            *         M I S C .   T E S T   S U B R O U T I N E S       *~
            *-----------------------------------------------------------*~
            * Some subroutines to aid the above test sections.          *~
            *************************************************************

        customer_status_checker
*        Checks the Customer status code and returns if it is 'Active'.
*        Otherwise, displays a cautionary message to the operator.
*        Then, either returns normally or aborts the operation, de-
*        pending on the operator's response.
*                                         /* Use Old Routine 02/24/98 */
*        call "CUSCHECK" (cuscode$, cr_ar_only$, #01, ret%) /* Remove */
*        if ret% = 0% then goto L60110
 
            call "READ100" (#01, cuscode$, f1%(1%))
            get #01 using L60100, cstatus$
L60100:         FMT POS(793), CH(1)
            if pos("AIHD"=cstatus$) < 2% then return
            keyhit% = 2%   /* Open Window at Bottom */
            call "ASKUSER" (keyhit%,                                     ~
                            "*** NOTE CUSTOMER'S STATUS CODE ***",       ~
                            "You are entering a NEW order number and " & ~
                            "the Customer's status is '" &               ~
                            cstat_msg$(pos("AXIHD"=cstatus$)) & "'",     ~
                            "Press PF(16) to continue",                  ~
                            "or (RETURN) to cancel the operation")
            if keyhit% = 16 then return
                return clear all
                goto inputmode

* L60110
            return clear all
            goto startover2

      check_po                /* Cannot have more than one Sales Order */
          po% = 0%            /* for a Customer PO EWD MOD - 02/01/96  */
          change% = 0%        /* Flag used for S.O. Header Change      */
          read #5,key 1% = po$, using L60125, msg$, eod goto L60185
            goto L60150
L60120:   read #5, using L60125, msg$
L60125:     FMT CH(41)
L60150:   if po$ <> str(msg$,26%,16%) then goto L60185
          if cuscode$ <> str(msg$,1%,9%) then goto L60120
             so$ = str(msg$,10%,16%)
             if soonfile% = 0% then goto L60180
                change% = 1%
                goto L60185
L60180:      po% = 1%  : fieldnr% = 2%
             init(" ") cuscode$
L60185:   msg$ = " "
        return


        build_part                             
L60190:   err% = 0%  : size$ = "Y" : mat pc = zer
          upd$ = "Y"                     /* TEMP SET FOR UPDATE        */
            p1    = 0.0    /* Spc Customer Price, Dealer Catalog Always*/
            sp%   = 0%     /* (0%) Dealer Price Catalog (Only)         */
                           /* (1%) Spc Customer Catalog Price in (P1)  */
                           /* (2%) SPECIAL CUSTOMER EDI PRICE IN (P1)  */
                           /* Note - Information from 'CUS PRICE' Table*/

            call "APCPR0SB" ( part$(c%),  /* Part Number to Build    */  ~
                              descr$(c%), /* Part Description        */  ~
                              size$,      /* (O)pening, (E)xact Size */  ~
                              upd$,       /* Update Prices (Y)es,(N)o*/  ~
                              pc(),       /* Calculated Prices       */  ~
                              p1,         /* Special Customer Price  */  ~
                              sp%,        /* Special Prc Cd 0,1,2    */  ~
                              cuscode$,   /* Customer Code           */  ~
                              ref$(),     /* Ref Type Codes Catalog  */  ~
                              ref1$(),    /* Ref Type Coses Spec Cat */  ~
                              ref_p(),    /* Ref Prices APC Catalog  */  ~
                              ref_p1(),   /* Ref Prices Special Cat. */  ~
                              #55,        /* CPRPRICE FILE           */  ~
                              #8,         /* GENCODES FILE           */  ~
                              #1,         /* CUSTOMER FILE           */  ~
                              #4,         /* HNYMASTR FILE           */  ~
                              #57,        /* APCPCMST FILE           */  ~
                              #56,        /* APCSKUNO FILE           */  ~
                              #58,        /* APCPCMSK FILE           */  ~
                              #59,        /* APCPCMSD FILE           */  ~
                              err% )      /* ERROR CODE              */
                                          /* 0% = Part and Cat. Prc  */
                                          /* 1% = Part Cat. an Spc   */
                                          /* 2% = EDI Price          */
                                          /* 3% = No Price Error Cat */
                                          /* 4% = Cat. Error Special */
                                          /* 5% = Invalid Part Price */
                                          /* 6% = Invalid Part Build */

            if str(part$(c%),1%,1%) = "+" then goto L60580
               if len(part$(c%)) < 10% then goto L60750
L60580:     opt% = 1% : s_23% = 0%
            s_23m$ = str(part$(c%),1%,3%) /* Set Model/Product Code    */
            call "APCPRXSB" (opt%,      /* 0%=Input, 1% = Verify Only  */~
                             cuscode$,  /* Customer Code               */~
                             s_23m$,    /* New Product Model Code      */~
                             s_23$,     /* New Series Code(Description)*/~
                             s_23%,     /* Description Length          */~
                             #1,        /* CUSTOMER                    */~
                             #8,        /* (GENCODES)                  */~
                             x_er% )    /* 0% = OK, 1% = NO CODE'S     */
           if opt% <> 2% then goto L60720  /* Re-Calc Price             */
              str(part$(c%),1%,3%) = s_23m$
              goto L60190

L60720:    if x_er% <> 0% then goto L60750 /* Series Not Applicable     */
              str(descr$(c%),1%,8%) = str(s_23$,1%,8%)

L60750:    init(" ") s_23$, s_23m$
        return

        check_howship
L60790:     init(" ") readkey$, or_hows$, or_cat$
            str(readkey$,1%,9%)   = "PLAN HOWS"
            str(readkey$,10%,15%) = str(howship$,1%,2%)
            read #8,key = readkey$, using L60830, desc$,eod goto L60920
L60830:        FMT POS(25), CH(30)
            str(howship$,3%,18%) = "/" & desc$
            or_hows$ = str(howship$,1%,2%)
            if or_hows$ = "02" or or_hows$ = "04" or or_hows$ = "06" or  ~
               or_hows$ = "22" then or_cat$ = "SAMP"
            if or_hows$ = "03" or or_hows$ = "05" then or_cat$ = "DISP"
            if or_hows$ = "30" then or_cat$ = "SALV"
            if or_hows$ = "31" then or_cat$ = "SCRA"

        return
L60920:     str(howship$,1%,2%) = "00"
            goto L60790

        check_fob
            readkey$ = all(hex(00))
            str(readkey$,1%,9%)   = "PLAN DELV"
            str(readkey$,10%,15%) = str(fob$,1%,2%)
            read #8,key = readkey$, using L61000, desc$,eod goto L61020
L61000:        FMT POS(25), CH(30)
            str(fob$,3%,18%) = "/" & desc$
L61020: return                               /* (EWD) - End            */

        close_out_screen                     /* (EWD) - Begin          */
            close_out% = 0%
            if keyhit% <> 8 then goto L61140
               lcus$ = cuscode$
               call "APCPL3SB" ( lcus$,      /* Customer Code          */~
                                 #1,         /* FILE = (CUSTOMER)      */~
                                 #8,         /* FILE = (GENCODES)      */~
                                 #7,         /* FILE = (SLMMASTR)      */~
                                 #22)        /* FILE = (TXTFILE )      */
               close_out% = 1%
L61140:     if keyhit% <> 10 then goto L61145
         call "APCPLN8B" ( opn%, /* Sales Order Display                */~
                          #8, /* GENCODES-Master Code Tables File      */~
                          #1, /* CUSTOMER-Customer Master Schedule File*/~
                          #7, /* SLMMASTR-Salesman Master File         */~
                          #22)/* TXTFILE -System Text File             */
                          close_out% = 1%
L61145:        if keyhit% <> 13 then L61220
                  call "MANUAL" ("BCKFASTR") 

               close_out% = 1%
L61220:     if keyhit% <> 15 then L61250
               call "PRNTSCRN"
               close_out% = 1%
L61250:     close ws
            call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return
                                                    /* (EWD) - End   */
        check_credit                                /* (EWD) - Begin */
            msg$(2) = "           C R E D I T   W A R N I N G           "
            init(" ") crmsg$(), crhold$
            bck$ = "NNNN"
            mat torder  = zer
            mat topen   = zer
            mat tlines% = zer
            inpmessage$ = " "
            if crhold$ <> "C" then L61300
              adjmsg$ = "Cancellation Reason Code"
                  goto L61310
L61300:       adjmsg$ = "Adjustment Reason Code"

*         Read in Bill-to (even if same as ship-to).
L61310:     read #1,key = cuscode$, using L61380, billto$, eod goto L61400
L61380:         FMT XX(779), CH(9)
            if billto$ = " " then L61400
L61390:     call "READ100" (#01, billto$, f1%(1%))
            if f1%(1%) <> 0% then goto L61470
L61400:         u3% = 2%
                call "ASKUSER" (u3%, "*** MISSING BILL-TO CUSTOMER ***", ~
                     "There is no Bill-to Customer (" &billto$& ") for:",~
                     "Ship-to Customer " &cuscode$& " (" & shipto$(1) &  ~
                     ")", "Press (RETURN) to acknowledge & continue")
                billto$ = str(cuscode$)
                goto L61390

L61470
*        Call BCKCRDSB to compute Credit Limit, A/R & Open Order amounts.
*        It does this via the "Credit Parent" logic.
            par% = 0%        /* Not used in this program */
            call "BCKCRDSB" (billto$, oo(), ar(), crlimit, par%)


*         Add up the order we are working on
            tqty = 0 /* Adding apples & oranges just to see if anything */
                     /* if open for shipping.  Used in credit hold.     */

            if maxlines% = 0% then L61810
             for c% = 1% to maxlines%
                if str(lsts$(c%),,3) <> "DEL"                            ~
                                       then tlines%(1) = tlines%(1) + 1% ~
                                       else tlines%(2) = tlines%(2) + 1%
                if str(lsts$(c%),,3)  = "DEL" then L61690
                ext       = round(order(c%) * price(c%) / conv(c%), 2)
                tqty      = tqty + order(c%)
                tdisc     = round(ext * linedisc(c%) *.01, 2)
                torder(1%) = torder(1%) + ext
                torder(2%) = torder(2%) - tdisc

                ext       = round(openqty(c%) * price(c%) / conv(c%), 2)
                tdisc     = round(ext * linedisc(c%) *.01, 2)
                topen (1%) = topen (1%) + ext
                topen (2%) = topen (2%) - tdisc
                gosub check_specials
L61690:     next c%
            torder(4%) = torder(1%) + torder(2%)
            torder(3%) = round(torder(4%) * orderdisc * .01, 2)
            torder(3%) = 0 - torder(3%)
            torder(4%) = torder(4%) + torder(3%)

            topen (4%) = topen (1%) + topen (2%)
            topen (3%) = round(topen (4%) * orderdisc * .01, 2)
            topen (3%) = 0 - topen (3%)
            topen (4%) = topen (4%) + topen (3%)

            oo(1%) = oo(1%) - origopen + (topen(4%) * conveqv)
L61810:     oo(2%) = oo(2%) - origopen + (topen(4%) * conveqv)
            totalar1 = ar(1%) + oo(1%)
            if cr_ar_only$ <> "Y" then totalar2 = ar(2%) + oo(2%)        ~
                                  else totalar2 = ar(2%)
*        Credit Hold Options are: (Note: CRHOLD$ for Hold or Cancelled)
*          'N' - No Automatic Hold; CRHOLD$ & HOW_HELD$ stay the same
*          'Y' - Automatic Hold, no override if Held or Released Manually
*          'O' - Automatic Hold, override of Manual Setting

*       RHH
            sp$ = "00"
            if bck$ = "NNNN" then goto L61970
        REM    IF STR(BCK$,4%,1%) = "Y" THEN SP$ = "06"   /* W/F is Ok */
               if str(bck$,1%,2%) <> "NN" then sp$ = "11"
               if str(fob$,1%,2%) >= sp$ then sp$ = "00"
               if sp$ = "00" then goto L61970
            hdr$ = "* * * *  F O B   F O B   F O B * * * *"
           msg$(1) = "The (FOB) Delivery Code must be equal to or greater~
        ~ than (" & sp$ & ")"
           msg$(2)="S a l e s  O r d e r  W i l l  N o t  B e  S a v e d"
              gosub credit_message
*       RHH
L61970:       if so$ <> " " then goto L61990
                 topen(4%) = topen(4%) + 1.0
L61990:       if crhold$ <> "C" then L61992
                how_held$ = " " : goto L62180
L61992:     if crflag$ <> "O" then L61996
L61994:         crhold$, how_held$ = " " : goto L61998
L61996:     if crflag$ = "N" then L62180    /* Flags stay 'as are'. */
            if how_held$ <> "M" then L61994 /*To get this far CRFLAG$='Y'*/
                if crhold$ = "H" then                                    ~
                    crmsg$(1) = "*'Manually' Held Order*"
                goto L62180
L61998:     if tqty = 0 and topen(4) = 0 then L62180 /* Nothing there */
            if totalar2 > crlimit        then L62080 /* Over Cr Limit */

*            CALL "BCKDAYSB" (BILLTO$, U3%)
*            IF U3% = 0% THEN GOTO L62180
             goto L62180

                crmsg$(1%) = "* # Days Late Exceeded*" : goto L62090
L62080:         crmsg$(1%) = "*Credit Limit Exceeded*"
L62090: REM     CRMSG$(2%) = "*Sales Order Not Saved*"
                crmsg$(2%) = "*S.O. Cannot be Planed*"
                crhold$    = "H" /* S. O. is on credit hold */
            hdr$ = "*Credit Limit Exceeded By (XXXXXXXXX)*"
            msg$(1) = "  Please Notify Credit, S.O. Cannot be Planned?  "
            x = totalar2 - crlimit
            convert x to str(hdr$,28%,9%), pic(#####.##-)

            gosub credit_message
L62180: return

        credit_message
            comp% = 8%
            msg$(3%)= "   Press <RETURN> or Any (PF) Key To Continue.   "
            call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        check_specials
            if sp$ = "00" then return
               gosub lookup_tempered 
                                               /* Check Tempered Glass */
            if zz% > 0% and str(part$(c%),1%,3%) <> "312"                ~
                        and str(part$(c%),1%,3%) <> "321"                ~
                                                then str(bck$,1%,1%) = "Y"
            if zz% = 2% then str(bck$,1%,1%) = "Y"
                                               /* Check Diamond Grid   */
            if str(part$(c%),8%,2%) = "97" or str(part$(c%),8%,2%) = "98"~
                                       then str(bck$,2%,1%) = "Y"
        return

        lookup_tempered
            zz% = 0%
            init(" ") readkey$
            str(readkey$,1%,9%) = "PLAN TEMP"
            str(readkey$,10%,15%) = str(part$(c%),5%,2%)
            read #8,key = readkey$, eod goto L62700
               zz% = 1%
               return
L62700:     init(" ") readkey$
            str(readkey$,1%,9%) = "PLANGLOUT"  /* Outside Purchase */
            str(readkey$,10%,15%) = str(part$(c%),7%,2%)
            read #8,key = readkey$, eod goto L62705
                zz% = 2%
L62705: return

        lookup_model
            init(" ") readkey$
            str(readkey$,1%,9%) = "MODEL    "
            str(readkey$,10%,15%) = str(part$(c%),1%,3%)
            read #8,key = readkey$, eod goto L62710
        return
L62710:     errormsg$ = "(Error) = Invalid Product Code"
        return

        lookup_so
            status% = 88% : code$ = "88"
            read #60,key 4% = so$, using L62770, code$,                   ~
                                                     eod goto L62800
L62770:        FMT POS(60), CH(2)
            convert code$ to status%, data goto L62790
L62790:
L62800: return

        send_fax
            call "SHOSTAT" ("Faxing Customer Acknowledgement")
            call "APCFAXSD" (cuscode$,       /* Customer CODE          */~
                             so$,            /* Customer Sales Order   */~
                             error%,         /* 1%=Not Open, 2%=No Fax */~
                             #8,             /* (GENCODES) 3%=NOT SAVED*/~
                             #5,             /* (BCKMASTR)             */~
                             #6,             /* (BCKLINES)             */~
                             #22,            /* (TXTFILE )             */~
                             #1,             /* (CUSTOMER)             */~
                             #21,            /* (ARMTERMS)             */~
                             #4,             /* (HNYMASTR)             */~
                             #17)            /* (BOMSPEC )             */
            check% = 1%                      /* CLEAR BUFFER AND START */
            if error% = 0% then                                          ~
                           hdr$ = "*** Fax Sent  Fax Sent  Fax Sent ***" ~
                      else hdr$ = "*** Error   Error   Error   Error***"
            msg$(1%) = " - - - - - - - - - - - - - - - - - - - - - "
            msg$(2%) = ms$(error% + 1%)
            gosub credit_message
        return

        build_category
            init(" ") cat_key$, cat$(c%), acct$()
            if len(part$(c%)) < 19 then cat_key$ = "PART"
            if len(cat_key$) > 3 then goto L63150    /* Line is Part    */
            if len(or_cat$) < 4 then goto L63120     /* Line is Special */
               cat_key$ = or_cat$
               goto L63150
L63120:     str(cat_key$,1%,3%) = str(part$(c%),1%,3%) /*Based on Model*/
            str(cat_key$,4%,1%) = "2"

L63150:     read #11,key = cat_key$, using L63170, cat$(c%), acct$(),     ~
                                                           eod goto L63180
L63170:        FMT CH(4), POS(35), 2*CH(9)
L63180: return
                                              /* (EWD) - Begin     */


        REM CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC~
            *                          E X I T                          *~
            * --------------------------------------------------------- *~
            * CLOSES ALL THE FILES CURRENTLY OPEN, AND ALSO DISPLAYS    *~
            * A MESSAGE (ONLY IF IN FOREGROUND) WHILE LINKING TO THE    *~
            * NEXT PROGRAM.                                             *~
            *-----------------------------------------------------------*~
            * THIS PROGRAM CONTAINS VALUABLE TRADE SECRETS AND PROPRIE- *~
            * TARY ASSETS OF CAELUS ASSOCIATES, INC., SPOKANE, WA, EM-  *~
            * BODYING SUBSTANTIAL CREATIVE EFFORTS  AND CONFIDENTIAL    *~
            * INFORMATION.  UNAUTHORIZED USE, COPYING, DECOMPILING,     *~
            * TRANSLATING, DISCLOSURE, OR TRANSFER OF IT IS PROHIBITED. *~
            * COPYRIGHT (C) 1983, AN UNPUBLISHED WORK BY CAELUS ASSSO-  *~
            * CIATES, INC., SPOKANE, WA.  ALL RIGHTS RESERVED.          *~
            CAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSASSOC

        exit_program
            call "SHOSTAT" ("One Moment Please")
            end
