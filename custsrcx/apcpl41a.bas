        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPL41A                             *~
            *  Creation Date     - 08/01/96                             *~
            *  Last Modified Date- 02/23/2017                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Last Modified By  - Christie Sanders                     *~
            *                                                           *~
            *  Description - Report Selections with Date, Dept, Load,   *~
            *                Begin/End Barcode.                         *~ 
            *            (1) - Print or Restart Label(s).               *~
            *                * Dept (Given) Load N/A, Only Specified    *~
            *                  Dept is Applicable, No 'ALPHA','STOCK' or*~
            *                  Pulls. Pulls Only for '102' and '104'    *~
            *                * Dept (ALL) Get Load from Barcode and     *~
            *                  Exclude Support and Pull's accept all    *~
            *                  other Dept's.                            *~
            *            (2) - Prod. Labels by Product/Completion Date  *~
            *                * Prod Date, Dept, Load Poss. Entries      *~
            *                  Dept (Given) Load may be Applicable.     *~
            *                * Dept (ALL) Then the Load is Required.    *~
            *            (3) - Pull From Stock Labels by Load/Completion*~
            *                * Prod Date, Dept, Shift not Applicable.   *~
            *                * Load Number is Required. Only Dept's     *~
            *                  (102) and (104) will be Printed for Load.*~
            *            (4) - Prod. Report by Product/Completion Date  *~
            *            (5) - Pull From Stock Report by Load/Completion*~
            *            (6) - Staging/Loading Report by Load Cust/Prod *~
            *                                                           *~
            *  Subroutines Used - (APCLDSUB) - Options Description-Only *~
            *                                  May Change       (PAR000)*~
            *                     (APCDESCR) - Long,Print,Size Descript.*~
            *                                  May Change       (PAR000)*~
            *                     (APCPL41B) - Print Prod Labels        *~
            *                                  File = (APCBARPD)        *~
            *                     (APCPL41B) (APCBARPD) Not Used(PAR000)*~
            *                                                           *~
            *                     (APCPL41C) - Create Glass Rack Data   *~
            *                                  Sub Changed for  (PAR000)*~
            *                     (APCPLN1B) - Planning Tables Display  *~
            *                     (APCPRZSB) - New Series Numbers       *~
            *                     (EWDNOMSZ) - Convert Open to Nominal  *~
            *                                  (EWD007)                 *~
            *   CMG               (AWDDESCR) - Long,Print,Size Descript.*~
            *                                  May Change       (PAR000)*~
            *                                                           *~
            *  Special Comments  - All Tables Excep (MODEL) can have    *~
            *                      Alphnumeric values.                  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/12/96 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 03/20/97 ! Mods to Print "w" on the label for all   ! RHH *~
            *          !   Mulled Products. Dept=104 and Stock    !     *~
            * 04/03/97 ! Mods to Series Name is 1st of Print      ! RHH *~
            *          !   Description.                           !     *~
            * 05/21/97 ! Mods Clean-up printing of private labels ! RHH *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 11/18/97 ! Mods to Production Label. Add Job Name   ! RHH *~
            *          ! to Label. Add Sales Order and Series     !     *~
            *          ! name to Warranty Stub.                   !     *~
            * 03/09/98 ! Mod to Correct Problem                   ! RHH *~
            * 03/20/98 ! Y2K                                      ! LDJ *~
            * 04/07/98 ! Mods for two New Consolidation Reports   ! RHH *~
            *          !    OPTION% = 6 ---- No Parts on Report   !     *~
            *          !    OPTION% = 7 ---- Parts Only Report    !     *~
            *          ! (New Sub) - check_selection              !     *~
            * 05/17/98 ! Mod for rpt_3 to create report for dept  ! RHH *~
            *          !   "044" Wood Surround (EWD001)           !     *~
            * 06/03/98 ! (EWD002) Mod for new Lowes Special Labels! RHH *~
            * 06/26/98 ! (EWD003) Mod to Label file for Dept      ! RHH *~
            * 11/05/98 ! (EWD004) Mod for Private Labels          ! RHH *~
            * 03/05/99 ! (EWD005) Allocate Racks for Glass        ! RHH *~
            *          !   New Subroutine (APCPL41C)              !     *~
            * 04/15/99 ! (EWD006) Mod to sort work file by spacer ! RHH *~
            *          !   for Dept's 047, 048 for Glass Rack     !     *~
            *          !   labels.                                !     *~    
            * 07/19/99 ! (EWD007) Mod's for the new Production    ! RHH *~
            *          !   labels printing on the Zebra printers. !     *~
            *          !   New Subroutine (EWDNOMSZ) Convert the  !     *~
            *          !   window opening size to Nominal.        !     *~
            *          !   (PLAN NEWC) New table for New Construct!     *~
            *          !   ion models. Also 3rd line of text      !     *~
            *          !   xxxxx(16)xxxxxxx/xx(7)XX               !     *~
            *          !   Contractor/Location                    !     *~
            * 07/19/99 ! (EWD008) Mod to APCPL41C to check glass  ! RHH *~
            *          !   for size restrictions. Departments are !     *~
            *          !   different and vary.                    !     *~
            * 08/06/99 ! (EWD009) Mod to Lowes special labels.    ! RHH *~
            *          !   New table (PLANLABSP) to override the  !     *~
            *          !   associated with a specific sku No.     !     *~
            *          !   keys off of the 'upc_code$'            !     *~
            * 09/09/99 ! (EWD010) Mod to Switch to new version    ! RHH *~
            *          !   for opening Files 'EWDOPEN'            !     *~
            * 11/15/99 ! (EWD011) Mod to Add 'FOAM' and 'SAMPLE'  ! RHH *~
            *          !   to the production label.               !     *~
            * 11/17/99 ! (EWD012) Mod to Replace 'Due Date' on the! RHH *~
            *          !   report with Production Seq. No.        !     *~
            * 01/20/00 ! (EWD013) Mod Samp/Disp/Lit  lit_flg$     ! RHH *~
            * 02/24/00 ! (EWD014) Mod to expand the Product Desc  ! RHH *~
            *          !          on the production label to 60 Ch!     *~
            * 10/25/00 ! (EWD015) Mod to put Temp glass in racks  ! RHH *~
            *          !          leave a hole marked 'TTTTT'     !     *~
            * 07/10/01 ! (EWD016) Mod for Special Shapes. Dept.   ! RHH *~
            *          !          Special Shape Stock '104'       !     *~
            * 10/09/01 ! (EWD017) Mod to add Totals after drops   ! CMG *~
            *          !          when report for loads (opt 5)   !     *~
            * 12/20/01 ! (EWD018) Mod to fix glass rack labels.   ! CMG *~
            * 08/12/02 ! (EWD019) Mod to put UPC code on Lowe's   ! RHH *~
            *          !          Special labels. upc_code$       !     *~
            * 01/02/02 ! (EWD020) Mod to add Drop Ship for how    ! CMG *~
            *          !          ship code '08'.                 !     *~
            * 03/12/03 ! (EWD021) Mod to allow numeric or alpha   ! CMG *~
            *          !          load numbers.                   !     *~
            * 06/04/03 ! (EWD022) Mod to add Customer Code to the ! RHH *~
            *          !          label file. For 100% Inspection !     *~
            * 06/06/03 ! (EWD023) Mod add new report selection.   ! CMG *~
            * 06/18/03 ! (EWD024) Mod to put Parent Load on Report! CMG *~
            * 10/21/03 ! (EWD025) Mod for Tempered Glass Rack Lbls! CMG *~
            * 11/05/03 ! (EWD026) Mod to keep specials on Rack Lb ! CMG *~
            * 02/20/04 ! (EWD027) Mod for a new 'specials' report ! CMG *~
            *          !           selection based off of quality !     *~
            *          !           specifications.                !     *~
            * 03/16/04 ! (EWD028) Mod for Line text               ! CMG *~
            * 04/01/04 ! (EWD029) Mod for Load range for option 8 ! CMG *~
            * 06/01/04 ! (EWD030) Mod to add tempered back on the ! CMG *~
            *          !          on the rack labels for dept 048 !     *~
            *          !          and 047.  These departments have!     *~
            *          !          a special sort so they are not  !     *~
            *          !          on the rack lbls since tempered !     *~
            * 06/23/04 ! (EWD031) Mod to put counter on 100% lbl  ! CMG *~
            * 07/21/04 ! (EWD032) Mod for new dept 054            ! CMG *~
            * 08/10/04 ! (EWD033) Mods for additional glass and   ! CMG *~
            *          !          liting codes on the special     !     *~
            *          !          check list.                     !     *~
            * 08/31/04 ! (AWD034) Mod to give dept '047' and '048 ! CMG *~
            *          !   same rack orders as all other depts.   !     *~
            * 05/19/05 ! (AWD035) Mod to add how ship code to wrk ! CMG *~
            *          !   file on rack lbls and pass to rack rnte!     *~
            * 05/19/05 ! (AWD036) Mod to add a flag in the prd    ! CMG *~
            *          !   file for UPS                           !     *~
            * 08/02/05 ! (AWD037) Mod to add Selection '1' to     ! RHH *~
            *          !          print a Single label for'WWW'   !     *~
            *          !          login.                          !     *~
            * 08/16/05 ! (AWD038) Mod to Add Selection 10 to      ! RHH *~
            *          !          Single NFRC Label.              !     *~
            * 01/01/06 ! (PAR000) CR347 Mod for new Sub Part No.  ! RHH *~
            * 03/03/06 ! (PAR001) Mod for the new production label! RHH *~
            *          !    changes. Size Change 640 to 1024      !     *~
            * 03/16/06 ! (PAR002) Mod to Correct Label Description! RHH *~
            * 07/27/06 ! (PAR003) Mod to add new Special Mull     ! RHH *~
            *          !          code to the Production Label.   !     *~
            * 09/25/06 ! (PAR004) Mod to the label file to change ! RHH *~
            *          !          the size of Room location from  !     *~
            *          !          7 to 16. For printing on the    !     *~
            *          !          Shipping Label. Also correct the!     *~
            *          !          Job name to full size.          !     *~
            *          !          (EWDPRDLB) Add new fields for   !     *~
            *          !          job_name$ (688%,16%) and        !     *~
            *          !          room (704%,16%) from            !     *~
            *          !               str(text_d$(3%),pp%+1%,16%)!     *~
            *          !     text_d$(3%),<Contractor>/<Room Loc>  !     *~
            * 03/07/07 ! (AWD039) Mod for tempered and ds rack lbs! CMG *~
            * 01/09/08 ! (AWD040) Mod for new dept 074            ! CMG *~
            * 09/11/08 ! (AWD041) mod for SCLMR                   ! CMG *~
            * 02/23/09 ! (AWD042) mod for gold or silver amaa lbl ! CMG *~
            *07/21/2009! (AWD043) mod for M when mull clip for lbl! CMG *~
            *08/09/2009! (AWD044) don't list warrenties on report ! CMG *~
            *11/23/2009! (AWD045) modification for Rack labels    ! CMG *~
            *02/11/2010! (AWD046) added AWDPLNDT to option 02     ! DES *~
            *09/14/2010! (AWD047) mod to add hidden key to delete ! CMG *~
            *          !    a day of rack labels  PF12            !     *~
            *03/23/2011! (AWD048) mod for rack intercept changes  ! CMG *~
            *04/30/2012! (AWD049) mod for 8900 Applied Fin Option ! CMG *~
            *03/05/2013! (AWD050) mod for foam filled, foam wrap  ! CMG *~
            *05/06/2013! (AWD051) add schema                      ! CMG *~
            *10/03/2013! (AWD052) mod for dept 103 and FactPrep   ! CMG *~
            *05/30/2014! (AWD053) mod for 65 series applied fin   ! CMG *~
            *07/18/2014! (AWD054) mod to key off series instead   ! CMG *~
            *          !  of mdls in ELLISON04 for "C" on lbl     !     *~
            *10/20/2014! (AWD055) mod to add screen code "7".     ! PWW *~
            *03/13/2015! (AWD056) mod to "ALL" Depts so that it   ! PWW *~
            *          ! excludes certain depts automatically.    !     *~
            *          ! Also disable Load Required.              !     *~
            *04/10/2015! (IM8013) mod to add additional series to ! PWW *~
            *          ! mdl8900.                                 !     *~
            *05/18/2015! (IM8022) mod to remove awdplngr and lamin! CMG *~
            *07/24/2015! SR67021  mod to examine all records in   ! PWW *~
            *05/18/2015!  ewdplnex when applying the (G)old stamp.!     *~
            *08/26/2015! SR68269  mod to include dept 104 if ALL  ! PWW *~
            *          !  depts are selected & disregard PLAN SUPP!     *~
            *09/02/2015! (SR68332) Mod to add new fields from     ! PWW *~
            *          !          oradesc2 if it is a part.       !     *~
            *10/28/2015! (SR70134) Mod to expand L1_DESC in       ! PWW *~
            *          !          oradesc2 from 200 to 250.       !     *~
            *06/02/2016! (SR75267) Mod to make APPLFIN dynamic by ! PWW *~
            *          !          using GENCODES APPLNAIL.        !     *~
            *06/17/2016! (SR75604) Mod to add spacer/intercept to ! PWW *~
            *          !          Prod Label.                     !     *~
            *07/21/2016!  CR00548  Mod to remove counter on 100%  ! PWW *~
            *          !          inspections as this is now done !     *~
            *          !          in EWDPLA71.                    !     *~
            *10/03/2016! CR00604 mod to seperate Tempered from DS ! PWW *~
            *          !         on Rack Labels.                  !     *~
            *02/23/2017! (SR79626) shipping reports not matching  ! CMN *~
            *06/23/2017! (CR00950) add RDB and RBN to the delete  ! RDB *~
            *08/16/2017! (CR1089) generate dept 102 production    ! RDB *~
            *          !       label record when using ALL for TX !     *~
            *10/18/2017! (CR1174) Add shipment block              ! RDB *~
            *05/16/2018! (CR1515) Remove reset of foam$ on Repair ! RDB *~
            *          !        and Fix_it orders.  It causes     !     *~
            *          !        to not print in EWDPLN71.         !     *~
            *12/28/2018! (CR1828) EWDPRDLB cust code change       ! DES *~
            *05/17/2019! (CR2031) Add PlyGem PO to Stg/Load Rpt #5! RDB *~
            *08/05/2019! CR2140 Add dept 072 to ALL array         ! RDB *~
            *08/08/2019! CR2172 Add uppercase to series for FIN   ! RDB *~
            *09/11/2019! CR2218 Change 072 to 027                 ! RDB *~
            *10/26/2019! CR2296 New NC STC rack label             ! RDB *~
            *12/04/2019! CR2219 Allow A loads in Dept 104 to print! RDB *~
            *          ! production labels                        !     *~
            *01/28/2022! CR3005 Add dept 007 to NC ALL array      ! RDB *~
            *02/16/2022! CR3027 Sort prd labels like TX           ! RDB *~
			*04/27/2022! CR3084 Add load nbr to p_sort dept 102 NC! RDB *~
            *************************************************************
   
        dim                              /* FILE = APCPLNSC            */~
            prod_desc$80,                /* (PAR002) Label Description */~
            mods$(10%)3,                 /* PLYER - Special Models     */~
            d_prod$1,                    /* PRODUCT LINE               */~
            d_mod$3,                     /* MODEL                      */~
            s_23m$3,                     /* Model Code for Series      */~
            s_23$8,                      /* New Series Code            */~
            s_so$8,                      /* Sales Order       (EWD004) */~
            s_ln$3,                      /* Sales Order Line  (EWD004) */~
            s_prv$30,                    /* Private Label Name(EWD004) */~
            s_1$2,                       /* Private Label Code(EWD004) */~
            sav_cust$9,                  /* For LOOKUP_CUST            */~
            sav_load$5,                  /* For LOOKUP_HEADER          */~
            sav_dept$3,                  /* For LOOKUP_DEPT            */~
            sav_st$2,                    /* For LOOKUP_STATUS          */~
            sav_drop$2,                  /* Save Drop Number           */~
            sav_part$25, save_part$45,   /* Save Part Number   (PAR000)*/~
            sav_shft$2, spec_part$25,    /* Save Shift Code            */~
            sp_part$25, sp_shape$1,      /* Save for special Shape EWD016*/~
            tot_load$8,tot_stage$8,      /* Load and Stage Values      */~
            tot_warr$8,                  /* Number of Warranties (AWD041)*/~
            tot_count$8, tot_loaded$8,   /* Total Loaded               */~
            tot_bol$8, tot_closed$8,     /* Total BOL's and Invoiced   */~
            tot_tran$8, tot_rga$8,       /* Total Trans & RGA (SR79626)*/~
            tot_undef$8,                 /* Total Undefined            */~
            tot_msg1$30,                 /* Staging Message            */~
            tot_msg2$30,                 /* Loading Message            */~
            tot_msg3$30,                 /* BOL'S Message              */~
            tot_msg4$30                  /* Invoiced Message           */
 
        dim                              /* FILE = APCPLNDT            */~
            pd_bar$18, scr_dept$3,       /* Prod Tracking Barcode      */~
            scr_shft$2,                  /* Prod Tracking Dept/Shift   */~
            dtl_load$5,                  /* Load Number        (Header)*/~
            dtl_item_no$2,               /* S.O. Line Item No. (Header)*/~
            dtl_so$8,                    /* Sales Order Number         */~
            dtl_element_no$4,            /* Sales Order Element No.    */~
            dtl_ord_qty$4,               /* Sales Order Order Quantity */~
            dtl_prod$6, sav_dte3$6,      /* Production/Completion Date */~
            dtl_sort_prod$5,             /* Production/BIN Sort Code   */~
            dtl_part$25,                 /* Part Number                */~
            dtl_sub_part$20,             /* New Sub Part No.   (PAR000)*/~
            dtl_sub_info$10,             /* New Sub Info Fields(PAR001)*/~
            dtl_new_part$45,             /* New Dtl Part No.   (PAR000)*/~
            sub_flg$1,                   /* Error Location Flg (PAR000)*/~
            dtl_cuscode$9, apc_cust$6,   /* Customer Number            */~
            dtl_txt$4,                   /* Line Item Text Id          */~
            dtl_key$18,                  /* Primary Key                */~
            dtl_t$3,                     /* (M)ake or (P)ull           */~
            sav_key$61,                  /* Save Alt Key (1)           */~
            sav_key1$11,                 /* Save initial Key Value TEXT*/~
            sav_key2$57,                 /* SAVE INITIAL KEY VALUE     */~
            dtl_desc$15,                 /* Status Description         */~
            sclmr$3,                     /* SCLMR       (AWD041)       */~
            hg$2,                        /* Hinge Code  (AWD041)       */~
            sz$100,                      /* (AWD041) Size              */~
            calc$9                       /* (AWD041) fraction size     */


        dim                              /* ( LABEL   )                */~
            readkey$100, desc$30,        /* Dummy Lookup Key           */~
            cust_name$30,                /* Customer Name              */~
            cust_city$18,                /* Customer City              */~
            cust_st$2,                   /* Customer State             */~
            wood_mull$28,                /* WOOD/FACTORY               */~
            stock$5, s_key$25,           /* Stock or Blank             */~
            sav_stock$45,                /* Save Stock Part No.(PAR000)*/~
            stock_flag$1,                /* Stock Flag (M) or (S)      */~
            dept$30,                     /* Department Description     */~
            dept1$25,                    /* Department Desc. LABEL     */~
            apc_scr$120,                 /* Screen Description         */~
            apc_prt$60,                  /* Print Description          */~
/*PAR000*/  sub_scr$120,                 /* Subpart Description        */~
/*PAR000*/  sub_prt$60,                  /* Subpart Print Description  */~
            apc_sze$20, sze$30,          /* Size Long Form             */~
            apc_sod$40,                  /* Special Options Description*/~
            textid$4,                    /* Line Item Text  ID         */~
            text_desc$60, text_d$(3%)70, /* Line Item Text             */~
            width$7,                     /* Width                      */~
            height$6,                    /* Height                     */~
            upc_code$12,                 /* UPC BAR CODE               */~
            sku_key$28,                  /* Sku Primary Key            */~
            sku_code$3,                  /* Customer Sku Code          */~
            skuno$25, sku_label$2,       /* Sku Number, Label Code     */~
            sp_rec$102, sp_key$41        /* Special Label Record       */

        dim                              /* FILE = APCWORK4            */~
/*EWD020*/  mode$5, wrk_rec$256,         /* WORK FILE          (PAR000)*/~
            wrk_key$55,                  /* DATA COLLECTION - PRIME KY */~
            warranty$8,                  /* (EWD006) - Warranty Id     */~
            spacer$4,                    /* (EWD006) - Spacer Thickness*/~
            rmk_key$12                   /* (EWD006) - Remake Key      */

        dim                              /* (Program Variables)        */~
            lit_flg$1,                   /* Literature Flag   (EWD013) */~
            filename$8,                  /* Used by EWDOPEN   (EWD010) */~
            prv$15, job_name$16,         /* Private Label / Job(PAR004)*/~
            count$4, cnt$4,              /* Print Counters             */~
            hdr$40, msg$(3%)79,          /* Ask User Prompt            */~
            apcbar$(1000%)60,            /* BAR CODE AND CUSTOMER CODE */~
            sn$(1000%)5,                 /* Save Product Seq Number    */~
            sv_pload$(1000%)5,           /* Save Parent Load           */~
            apc_desc$30,                 /* LOAD DESCRIPTION           */~
            end_desc$30,                 /* LOAD DESCRIPTION  (EWD029) */~
            sav_desc$30,                 /* LOAD DESCRIPTION  (EWD029) */~
            apc_load$5,                  /* LOAD NUMBER                */~
            cl$2,                        /* Color CODE                 */~
            cl_desc$30,                  /* Color DESCRIPTION          */~
            scr$(20%)50,                 /* Screen Text Messages       */~
            scr_txt$(10%)20,             /* Prompt Text Messages       */~
            scr_code$2,                  /* Screen Code (0) or (1)     */~
            scr_date$10,                 /* Production Date - Beginning*/~
            scr_date_end$10,             /* Production Date - Ending   */~
            scr_bar$18,                  /* Starting Bar Code          */~
            scr_bar_code$18,             /* Ending Bar Code            */~
            scr_load$8,                  /* Screen Load Number         */~
            end_load$5,                  /* END Load Number  (EWD029)  */~
            scr_msg$30,                  /* Screen Message Process Type*/~
            scr_msg1$41,                 /* Screen Message Completion  */~
            scr_dte$8,                   /* Completion Date FORMATTED  */~
            scr_dte1$10,                 /* Production Date Unformatted*/~
            scr_dte2$10,                  /* Production Date Unformatted*/~
            text_key$11,                 /* Text File Key              */~
            text$(3%)70,                 /* Text (2) Lines             */~
            part_desc$32,                /* Part Description           */~  
            mod$3,                       /* Part Model Number          */~
            text_flag$1,                 /* Line Item Text (Y) or (N)  */~
            company$25,                  /* Company Name               */~
            print_title$60,              /* Report Title               */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            message$257,                 /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim schema$8                     /* Schema (AWD051)            */

        dim f2%(64%),                    /* = 0 if the file is open    */~
            f1%(64%),                    /* = 1 if READ was successful */~
            fs%(64%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64%)20                 /* Text from file opening     */

                                         /* (New) - (APCPLNDT) - File  */
        dim dt_rec$256,                  /* Planning Detail Record     */~
            dt_key$23,                   /* Primary Key - Barcode      */~
            dt_key1$57,                  /* Alt Key 1 - Production Date*/~
            dt_key2$51,                  /* Alt Key 2 - Scanned Date   */~
            dt_key3$23,                  /* Alt Key 3 - Load/Drop Key  */~
            dt_dept$3,                   /* Department Code            */~
            dt_ref$8,                    /* Reference/Warranty Code    */~
            dt_seq$5,                    /* Production Sequence Number */~
            dt_shft$2,                   /* Production Shift           */~
            dt_proc$2,                   /* Process Code               */~
            dt_wood$3,                   /* Wood Surround Code         */~
            dt_st$2,                     /* Detail Status Code         */~
            dt_dte$6,                    /* Detail Status Date         */~
            sc_pload$5,                  /* Parent Load Number         */~
                                         /* (New) - (APCPLNLD) - File  */~
            ld_load$5,                   /* Load Number                */~
            ld_desc$30,                  /* Load Description           */~
            ld_dtp1$8, ld_dte1$6,        /* Load Production Date (Plan)*/~
            ld_dtp2$8, ld_dte2$6,        /* Load Completion Date (Plan)*/~
            ld_dtp3$8, ld_dte3$6,        /* Load Load Date       (Plan)*/~
            ld_shp_blk$3,                /* Shipment Block CR1174      */~
                                         /* (New) - (APCPLNOR) - File  */~
            or_key$8,                    /* Customer Sales Order       */~
            or_due$8,                    /* Customer Due Date          */~
            or_route$5,                  /* Customer Route Code        */~
            or_po$16,                    /* Customer P.O.              */~
            or_hows$2,                   /* Customer How Ship  (EWD020)*/~
            how_txt$10,                  /* Text for Drop Ship (EWD020)*/~
            or_drop$2,                   /* Customer Drop Number       */~
            bcklnes_key$19,              /* BCKLINES Key       (EWD024)*/~
            config_lne$3                 /* Configuration Line (EWD024)*/

        dim                              /* FILE = EWDPRDLB    (EWD007)*/~
            pd_lab$(4%)256,              /* Label Record       (PAR001)*/~
            pd_fil$256, pd_fil1$82,      /* Filler Area        (AWD036)*/~
            pd_key$35, pd_key1$23,       /* Primary Key                */~
            pd_sort_p$5,                 /* Primary Sort Code          */~
            pd_sort_s$5,                 /* Secondary Sort Key         */~
            p_sav_dept$3,                /* Save Dept                  */~
            dt_special$10,               /* Line Item Special Flags    */~
            wd$7, ht$6,                  /* For converting Opening     */~
            p_part$25,                   /* Part Number Opening        */~
            p_sub_part$20,               /* New Sub Part No.   (PAR000)*/~
            p_sub_info$10,               /* New Sub Info Data  (PAR001)*/~
            p_send$21,                   /* Production Send Info.      */~
            p_nominal$6, nominal$7,      /* Nominal Size based on Open */~
            o_width$9, o_height$8,       /* Long Form Opening          */~
            p_oth$36, p_oth1$24,         /* Other Information  (EWD014)*/~
            p_oth2$40,                   /* 2nd part of Descrip(PAR001)*/~
            p_due$10,                    /* Due Date Formatted         */~
            p_make$10,                   /* Make Date Formatted        */~
            p_stat$5,                    /* Make, Pull, Stock          */~
            p_samp$1,                    /* (EWD011)0=No, 1=Samp,2=Disp*/~
            p_foam$1,                    /* (EWD011) (Y)es or (N)o     */~
            p_fin$1,                     /* (EWD014) (Y)es or (N)o     */~
            p_ups$1,                     /* (PAR001) UPS Flag 0 or 1   */~
            p_specialmull$1,             /* (PAR003) Spec Mull Shapes  */~
            screen$1,                    /* (EWD011) Screen Code       */~
            locks$1,                     /* (EWD014) Lock Codes        */~
            pgpo$20,                     /* PlyGem PO CR2031           */~
            pq_key$18, pq_key1$23,       /* (EWDPRDQQ) - S.O. Cross-Ref*/~
            pq_item$3, pq_dept$3,        /* S.O. Piece Count           */~
            pq_qty$3,                    /* S.O. total Quantity        */~
            pq_sav$8                     /* Save S.                    */
                                         /* (EWD007) - End             */

                                         /* (AWD037)                   */
        dim rlib$8,                      /* Run library for link call  */~
            rvol$6,                      /* Run volume for link call   */~
            run$8                        /* Program to Run             */
                                         /* (AWD037)                   */

                                         /* (PAR000)                   */
        dim                                                              ~
            sav_so$10,                   /* Save Sales Order & Line    */~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4             /* Additional Info Fields     */

         dim mdlNum$100,                 /* Model Number      (AWD049) */~
             mdlNum(30)                  /* Model Number               */
/*(AWD054) */
        dim series$16,                   /* WW Series                  */~
            style$10                     /* WW Style                   */
/*(AWD056) + */
/*CR00604 + */
        dim nc_depts$99,                 /* Don't include for ALL      */~
            ntx_depts$99,                /* Don't include for ALL      */~
            nc_depts6$128,               /* Do include for ALL         */~
            ntx_depts6$128                /* Do include for ALL

            nc_depts$ = "001004007010011021032044054066074095099100102106108"
  /* NC_DEPTS% = LEN(NC_DEPTS$) - 2%   */
            ntx_depts$ = "001009011013021035041044054074095100106" /* (CR1259) */
  /* NTX_DEPTS% = LEN(NTX_DEPTS$) - 2%  */
            nc_depts6$ = "002005006007008014017018019027028036047048049053055"
            nc_depts6% = 49%
            ntx_depts6$ = "000001002003004011012013015016021027029030031032033"~
               &  "040041044045046057058059062070071074095100101102"
            ntx_depts6% = 97%
/*CR00604 - */
/*(AWD056) - */ 
/*SR68332*/    
        dim oradescr_key$11,             /* ORADESC2 Read Key          */~
            oradescr_rec$(4%)256,         /* ORADESC2 Record           */~
            intercept$2                  /* SR75604  Intercept Type    */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21                          /* (PAR004)   */
            apc$   = "Production Reports by Department 9/25/06"
            pname$ = "APCPL41A - PAR:  1.01"              /* (PAR004)   */

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
            * #1  ! APCWORK4 ! MASTER SORT WORK FILE            (PAR000)*~
            *     !          !     Record Length Change to 256          *~
            * #2  ! APCPLNDT ! New Production Master Detail             *~
            * #3  ! APCPLNOR ! New Planning S.O. Header History         *~
            * #4  ! APCPLNSC ! New Planning S.O. Line Item File         *~
            * #5  ! GENCODES ! Master System Table File                 *~
            * #6  ! CUSTOMER ! Customer Master file                     *~ 
            * #7  ! HNYMASTR ! Part Master File                         *~
            * #8  ! EWDSTOCK ! BIN Location for Strd Parts old=APCSTOCK *~
            * #9  ! TXTFILE  ! Master Text File                         *~
            * #10 ! BCKLINES ! S.O. Line Item Detail File               *~
            * #11 ! AMTBOMIF ! Master VALIDITY FILE                     *~
            * #12 ! APCSKUNO ! APC CUSTOMER SKU NUMBERS- No Chg (PAR000)*~
            * #13 ! BCKMASTR ! S.O. HEADER FILE                         *~
            * #14 ! EWDLABSP ! Lowes Special Label Production   (EWD002)*~
            *     !          !     No Chg to File for Part No.  (PAR000)*~
            * #15 ! APCPLNLD ! Planning/Scheduling Load Master File     *~
            * #16 ! APCPLNGR ! Master Glass Tracking File       (PAR000)*~
            *     !          !     File size Chg to 384.                *~
            * #17 ! EWDPLNRK ! Glass Master Rack File- No Chg   NOT USED*~
            * #18 ! EWDPRDLB ! New Production Labels-No File Chg(PAR001)*~
            * #19 ! APCPCMST ! New Production Labels            (EWD007)*~
            * #20 ! EWDPRDQQ ! New Total Qty for S.O.           (EWD007)*~
            * #21 ! APCPLNDT ! Use to Calculate value for EWDPRDQQ      *~
            *     !          !     File Size Chg to 384.        (PAR000)*/~
            * #23 ! AWDPLNRK ! Glass Rack File for ds and temp  (AWD039)*~
            * #24 ! EWDPLNES ! Energy Star Master File          (AWD042)*~
            * #25 ! AWDPLURK ! Ultra Rack Labels                (AWD045)*~
            * #26 ! AWDPLUDR ! Ultra Double Strength Temp Rack Label(AWD045)*~
            * #27 ! AWDPLNDT ! New Production Master History    (AWD046)*~
            * #28 ! AWDPLNDT ! New Production Master History    (AWD046)*~
            * #63 ! BCKSUBPT ! New Sub Part Number File         (PAR000)*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

                                                            /* (EWD020) */
                                                            /* (PAR000) */
            select #1,   "APCWORK4",                                     ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =   55
                                                            /* (PAR000) */
            select #2,   "APCPLNDT",                                     ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   24, keylen =   23,                    ~
                        alt key  1, keypos =   47, keylen =  57,         ~
                            key  2, keypos =   53, keylen =  51,         ~
                            key  3, keypos =    1, keylen =  23, dup,    ~
                            key  4, keypos =   96, keylen =   8, dup

            select #3,   "APCPLNOR",                                     ~
                        varc,     indexed,  recsize = 170,               ~
                        keypos =    1, keylen =   51,                    ~
                        alt key  1, keypos =   27, keylen =  25,         ~
                            key  2, keypos =   70, keylen =   8, dup,    ~
                            key  3, keypos =   78, keylen =   8, dup,    ~
                            key  4, keypos =   52, keylen =   8,         ~
                            key  5, keypos =   36, keylen =  16, dup

            select #4,   "APCPLNSC",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   24, keylen =   10,                    ~
                        alt key  1, keypos =    7, keylen =  27,         ~
                            key  2, keypos =    1, keylen =  33

            select #5,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #6,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key 1,  keypos = 10,   keylen =  30, dup,    ~
                            key 2,  keypos = 424,  keylen =   9, dup,    ~
                            key 3,  keypos = 771,  keylen =   9, dup,    ~
                            key 4,  keypos = 780,  keylen =   9, dup,    ~
                            key 5,  keypos = 1049, keylen =   9, dup

            select #7,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup
                                                        /* (PAR000)     */
                                          /* Old=APCSTOCK, New=EWDSTOCK */
            select #8,  "EWDSTOCK",                                      ~
                        varc,     indexed,  recsize =   128,             ~
                        keypos =    1, keylen =  52,                     ~
                        alt key  1, keypos =    8, keylen =  52
                                                        /* PAR000)      */
            select #9,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =   1, keylen =  11

            select #10, "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #11, "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32

            select #12, "APCSKUNO",                                      ~
                        varc,     indexed,  recsize =  73,               ~
                        keypos =    1, keylen =  28,                     ~
                        alt key  1, keypos =   29, keylen =  28, dup

            select #13, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #14, "EWDLABSP",                                      ~
                        varc,     indexed,  recsize =   102,             ~
                        keypos =    1, keylen = 41,                      ~
                        alt key  1, keypos = 42, keylen =  41

            select #15, "APCPLNLD",                                      ~
                        varc,     indexed,  recsize =   128,             ~
                        keypos =   11, keylen =  5,                      ~
                        alt key  1, keypos =  3, keylen =  13,           ~
                            key  2, keypos =  1, keylen =  15
                                                   /* (EWD005) Begin    */
                                                   /* (PAR000) SIZE Chg */
            select #16, "APCPLNGR",                                      ~
                        varc,     indexed,  recsize =  512,              ~
                        keypos =   22, keylen =   12,                    ~
                        alt key 1,  keypos =  7,   keylen =  27,         ~
                            key 2,  keypos =  1,   keylen =  33,         ~
                            key 3,  keypos =  13,  keylen =  21

            select #17, "AWDRK1",                                        ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  9,                      ~
                        alt key  1, keypos = 10, keylen =  14
                                                    /* (EWD005) End     */
                                                    /* (EWD007) - New   */
                                                    /* (PAR001)         */
            select #18, "EWDPRDLB",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =   35,                    ~
                        alt key  1, keypos = 278, keylen =  23
                                                    /* (EWD007) - File  */
                                                    /* (PAR001)         */
            select #19,   "AWDPCMST",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    9, keylen =   53,                    ~ 
                        alt key  1, keypos =    1, keylen =  8

            select #20,   "EWDPRDQQ",                                    ~
                        varc,     indexed,  recsize = 32,                ~
                        keypos =    1, keylen =   18

            select #21,  "APCPLNDT",                                     ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   24, keylen =   23,                    ~
                        alt key  1, keypos =   47, keylen =  57,         ~
                            key  2, keypos =   53, keylen =  51,         ~
                            key  3, keypos =    1, keylen =  23, dup,    ~
                            key  4, keypos =   96, keylen =   8, dup
  
                                                       /* (PAR000)      */

/* (AWD039) */
/*            select #23, "AWDRK2",                                        ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  9,                      ~
                        alt key  1, keypos = 10, keylen =  14
*/

              select #23, "AWDPLNRK",                                    ~
                        varc,     indexed,  recsize =   128,             ~
                        keypos =    1, keylen = 12,                      ~
                        alt key  1, keypos = 13, keylen =  14


/* (AWD042) */
            select #24,  "EWDPLNEX",                                     ~
                        varc,     indexed,  recsize =   80,              ~
                        keypos =  1,   keylen =   6

/* (AWD045) */
            select #25, "AWDRK3",                                        ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  9,                      ~
                        alt key  1, keypos = 10, keylen =  14


            select #26, "AWDRK4",                                        ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  9,                      ~
                        alt key  1, keypos = 10, keylen =  14

              /* <AWD046>     */
            select #27,  "AWDPLNDT",                                     ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   24, keylen =   23,                    ~
                        alt key  1, keypos =   47, keylen =  57,         ~
                            key  2, keypos =   53, keylen =  51,         ~
                            key  3, keypos =    1, keylen =  23, dup,    ~
                            key  4, keypos =   96, keylen =   8, dup
            select #28,  "AWDPLNDT",                                     ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   24, keylen =   23,                    ~
                        alt key  1, keypos =   47, keylen =  57,         ~
                            key  2, keypos =   53, keylen =  51,         ~
                            key  3, keypos =    1, keylen =  23, dup,    ~
                            key  4, keypos =   96, keylen =   8, dup

              /* </AWD046>    */
/* (AWD047) */
            select #29, "AWDPLPTO",                                      ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  9,                      ~
                        alt key  1, keypos = 10, keylen =  14
            select #30, "AWDPLPT2",                                      ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  9,                      ~
                        alt key  1, keypos = 10, keylen =  14
/* (/AWD047) */


/* (AWD042) */
/* (AWD048) */
            select #31, "AWDRK1DT",                                      ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  9,                      ~
                        alt key  1, keypos = 10, keylen =  14

            select #32, "AWDRK2DT",                                      ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  9,                      ~
                        alt key  1, keypos = 10, keylen =  14

            select #33, "AWDRK3DT",                                      ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  9,                      ~
                        alt key  1, keypos = 10, keylen =  14

            select #34, "AWDRK4DT",                                      ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  9,                      ~
                        alt key  1, keypos = 10, keylen =  14


            select #35, "AWDRK5",                                        ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  9,                      ~
                        alt key  1, keypos = 10, keylen =  14

            select #36, "AWDRK5DT",                                      ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  9,                      ~
                        alt key  1, keypos = 10, keylen =  14

            select #37, "AWDRK6",                                        ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  9,                      ~
                        alt key  1, keypos = 10, keylen =  14

            select #38, "AWDRK6DT",                                      ~
                        varc,     indexed,  recsize =    64,             ~
                        keypos =    1, keylen =  9,                      ~
                        alt key  1, keypos = 10, keylen =  14
                                                        /* (PAR000)     */
            select #63, "BCKSUBPT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  11,                     ~
                        alt key  1, keypos =  12, keylen =  11, dup,     ~
                            key  2, keypos =  23, keylen =  45, dup
                                                       /* (SR68332)    */
            select #58, "ORADESC2",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =   1,   keylen = 11

                                                       /* (PAR000)     */
            call "SHOSTAT" ("Opening Files, One Moment Please")

                                                   /* (EWD010)         */
            filename$ = "APCPLNDT" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNOR" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSC" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "CUSTOMER" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "HNYMASTR" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
                                                    /* (PAR000)        */
        REM    filename$ = "EWDSTOCK"
        REM    call "EWDOPEN" (#8, filename$, err%)
        REM    if err% <> 0% then gosub open_error
            call "OPENCHCK" (#8, fs%(8%), f2%(8%),500%, rslt$(8%))


            filename$ = "TXTFILE" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLINES" : call "EWDOPEN" (#10, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AMTBOMIF" : call "EWDOPEN" (#11, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCSKUNO" : call "EWDOPEN" (#12, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKMASTR" : call "EWDOPEN" (#13, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "EWDLABSP" : call "EWDOPEN" (#14, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNLD" : call "EWDOPEN" (#15, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNGR" : call "EWDOPEN" (#16, filename$, err%)
            if err% <> 0% then gosub open_error

            call "OPENCHCK" (#17, fs%(17%), f2%(17%),500%, rslt$(17%))

            filename$ = "EWDPRDLB" : call "EWDOPEN" (#18, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDPCMST" : call "EWDOPEN" (#19, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "EWDPRDQQ" : call "EWDOPEN" (#20, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDT" : call "EWDOPEN" (#21, filename$, err%)
            if err% <> 0% then gosub open_error
                                                          /* (EWD010)    */
                                                          /* (EWD025)    */
/* (AWD023) */
            call "OPENCHCK" (#23, fs%(23%), f2%(23%),500%, rslt$(23%))

/* (AWD042) */
            filename$ = "EWDPLNEX" : call "EWDOPEN" (#24, filename$, err%)
            if err% <> 0% then gosub open_error

/* (AWD045) */
            call "OPENCHCK" (#25, fs%(25%), f2%(25%),500%, rslt$(25%))
            call "OPENCHCK" (#26, fs%(26%), f2%(26%),500%, rslt$(26%))
              /* <AWD046>     */
            call "OPENCHCK" (#27, fs%(27%), f2%(27%),500%, rslt$(27%))
            call "OPENCHCK" (#28, fs%(28%), f2%(28%),500%, rslt$(28%))
              /* </AWD046>    */
              /* <AWD047>     */
            call "OPENCHCK" (#29, fs%(29%), f2%(29%),500%, rslt$(29%))
            call "OPENCHCK" (#30, fs%(30%), f2%(30%),500%, rslt$(30%))
              /* </AWD047>     */
            call "OPENCHCK" (#31, fs%(31%), f2%(31%),500%, rslt$(31%))
            call "OPENCHCK" (#32, fs%(32%), f2%(32%),500%, rslt$(32%))
            call "OPENCHCK" (#33, fs%(33%), f2%(33%),500%, rslt$(33%))
            call "OPENCHCK" (#34, fs%(34%), f2%(34%),500%, rslt$(34%))
                                                           /* (PAR000)  */
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#63, filename$, err%)
            if err% <> 0% then gosub open_error
                                                           /* (PAR000)  */
            call "OPENCHCK" (#35, fs%(35%), f2%(35%),500%, rslt$(35%))
            call "OPENCHCK" (#36, fs%(36%), f2%(36%),500%, rslt$(36%))
            call "OPENCHCK" (#37, fs%(37%), f2%(37%),500%, rslt$(37%))
            call "OPENCHCK" (#38, fs%(38%), f2%(38%),500%, rslt$(38%))
/*SR68332 */
            filename$ = "ORADESC2" : call "EWDOPEN" (#58, filename$, err%)
            if err% <> 0% then gosub open_error

            mat f1% = zer
            mat fs% = zer
            init(" ") rslt$()
        fx% = 2%
            scr_hist$ = " "
 
        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
        REM - Special Models for PLYER Labels
            mods$( 1%) = "706" : mods$( 2%) = "712" : mods$( 3%) = "713"
            mods$( 4%) = "722" : mods$( 5%) = "732" : mods$( 6%) = "781"
            mods$( 7%) = "782" : mods$( 8%) = "783"
            max_mods% = 8%

           sz$ = " 1/16 1/8  3/16 1/4  5/16 3/8  7/16 1/2  9/16 5/8 11/16~
        ~ 3/4 13/16 7/8 15/16     "                /* (AWD041)         */


            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #5, schema_err%)



        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  2%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)  
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg1
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then gosub begin_process
                  if keyhit%  = 12% then gosub delete_rack_data  /* (AWD047) */
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% > 2% then editpg1
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
            *             P R O C E S S   D A T A                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        begin_process
            init(" ") sav_so$
            call "SHOSTAT" ("Creating Report Selection ("&scr_code$&")")

        fx% = 2%
            if selection% < 3% then goto L19140 /* Skip Open Print File */
               company$ = "* APC Building Products *"          /* (25) */
               lcnt% = 99% : page_no% = 0%
               call "SETPRNT" ("APCL"&scr_code$, " ", 3000%, 0%)
               select printer (134)
               goto L19210
L19140:                                         /* (EWD007) - 06/30/99 */

L19210:     on selection% gosub rpt_2, rpt_2, rpt_3, rpt_4, rpt_5,       ~
                                                     rpt_6, rpt_3, rpt_5,~
                                                     rpt_3
                                                      /*  (EWD027)   */
            if selection% > 2% then goto L19290                          ~
                               else goto L19300 /* (EWD007)            */
                                                      /* CLOSE PRINTER */
L19290:     call "SETPRNT" ("APCL"&scr_code$, " ", 0%, 1%)
L19300: return clear all
        goto inputmode

        delete_rack_data           /* (AWD047)  */
          if userid$ = "CMG" or userid$ = "CGN" or userid$ = "CG1" ~
                       or userid$="PWW" or userid$ = "PWN"         ~
                       or userid$="RDB" or userid$ = "RBN"         ~
                           then goto delete_data
            return
        delete_data
            del% = 1%
            mode% = 1% : gosub open_work
            mode% = 3% : gosub open_work
            gosub allocate_racks
            del% = 0%
        return clear all                   /* (/AWD047) */
        goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            on fieldnr% gosub L20140,         /* Report Selection      */ ~
                              L20200          /* Screen Prompts        */

         return
 
L20140: REM Report Selection                       SCR_CODE$
            gosub screen_message
            init(" ") scr_msg$, scr_msg1$
         return

L20200: REM Screen Prompts                         SCR_TXT$()
            init(" ") scr_txt$()
            gosub screen_message_a
         return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28130
                inpmessage$ = edtmessage$
                return

L28130
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            if fieldnr% = 1% then return
            if selection% = 1% then inpmessage$ =                        ~
         " Department and Begin/Ending Barcode are Required?           "
            if selection% = 4% then inpmessage$ =                        ~
         " Pull From Stock Load Number Required?                       "
/*EWD023*/  if selection% = 5% or selection% = 8% then inpmessage$ =      ~
         " Staging or Loading, Load Number Required?                   "
            return

        scrn1_msg  :  data                                               ~
         " Enter a Valid Report Selection ( 1 thru 10 ).                ",~
         " Production Date and Department (Required),Load No.(Optional)?"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, scr$(), scr_code$,         ~
                      scr_msg$, scr_msg1$, scr_date$, scr_date_end$,     ~
                      scr_dept$, dept1$, scr_load$, apc_desc$, scr_bar$, ~
                      scr_bar_code$, scr_shft$, dt_shft$, dt_proc$,      ~
                      scr_dte$, scr_dte1$, scr_dte2$, apc_load$,         ~
                      sav_dept$, dept$, end_load$, end_desc$, scr_hist$
            gosub screen_message                         /*  (EWD029)   */

            del% = 0%
        return

        REM *************************************************************~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
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
        REM  DATALOAD
        REM  RETURN

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Update Store Data and Part Data                           *~
            *************************************************************

        REM DATAPUT
        REM RETURN

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

                                         /* APCPLNDT - Detail Record   */
L35050:     FMT CH(256)                  /* Detail Record              */

                                         /* APCPLNOR - Header Record   */
            FMT CH(170)                  /* Header Record              */

                                         /* (PAR000)                   */
                                         /* APCWORK4 -  Work Record    */
L35110:     FMT CH(256)                  /* Line Item Record           */
                                         /* (PAR000) Change Rec Size   */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
L40070:       gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40200,         /* Report Selection  */   ~
                                L40210          /* Prod. Beg/End Date*/
                                                /* Department Code   */
                                                /* Load Number       */
                                                /* Bar Code          */
                                                /* Bar Code Code     */
              if lfac$(3) = hex(81) and scr_hist$ <> "Y" then scr_hist$ = "N"
              goto L40330
                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
                  lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40200:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */
L40210:     if selection% <> 1% and selection% <> 10% then goto L40250
                                                /* (AWD037)           */
                                                /* (AWD038)           */
           lfac$(2), lfac$(3), lfac$(4), lfac$(5), lfac$(6), lfac$(7) = hex(84)

        REM       lfac$(3%), lfac$(5%), lfac$(6%) = hex(81)  /* On Fields */
        REM       lfac$(2%), lfac$(4%) = hex(84)             /* Off Fields*/
                                                /* (AWD037)           */
               return                                     /*  (EWD023) */
L40250:     lfac$(3) = hex(84)
            if selection% = 2% then lfac$(3) = hex(81)
            if selection% <> 4% and selection% <> 5% and               ~
                          selection% <> 8% then goto L40290
                                                          /* (EWD029)  */
               lfac$(5%) = hex(81)                        /* On Field  */
               lfac$(2), lfac$(4), lfac$(6), lfac$(7) = hex(84)

               if selection% = 4% then return
               lfac$(5%), lfac$(6%) = hex(81)             /* On Field  */
               lfac$(2%), lfac$(4%), lfac$(7%) = hex(84)
               return

L40290:     lfac$(2), lfac$(4), lfac$(5) = hex(81)   /* On Fields */
            lfac$(6%), lfac$(7%) = hex(84)                /* Off Fields*/
            return

L40330:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Report Selection:",                          ~
               at (05,25), fac(lfac$(1%)), scr_code$            , ch(02),~
               at (05,30), fac(hex(84)), scr_msg1$              , ch(41),~
               at (06,02), fac(hex(84)), scr_txt$(1%)           , ch(20),~
/*(Y2K, LDJ)*/ at (06,25), fac(lfac$(2%)), scr_date$            , ch(10),~
/*(Y2K, LDJ)*/ at (06,37), fac(lfac$(2%)), scr_date_end$        , ch(10),~
               at (06,50), fac(hex(84)), scr_txt$(5%)           , ch(20), ~
               at (06,72), fac(lfac$(3%)), scr_hist$            , ch(01),~
               at (07,02), fac(hex(84)), scr_txt$(2%)           , ch(20),~
               at (07,25), fac(lfac$(4%)), scr_dept$            , ch(03),~
/*(Y2K, LDJ)*/ at (07,37), fac(hex(84)), dept1$                 , ch(25),~
/*(Y2K, LDJ)*/ at (07,63), fac(hex(84)), scr_txt$(6%)           , ch(08),~
/*(Y2K, LDJ)*/ at (07,72), fac(lfac$(4%)), scr_shft$            , ch(02),~
               at (08,02), fac(hex(84)), scr_txt$(3%)           , ch(20),~
               at (08,25), fac(lfac$(5%)), scr_load$            , ch(05),~
/*(Y2K, LDJ)*/ at (08,37), fac(hex(84)), apc_desc$              , ch(30),~
                                                                             ~
                                                                             ~
/* (EWD029)    at (09,02), fac(hex(84)), scr_txt$(4%)           , ch(20), */ ~
/* (EWD029)    at (09,25), fac(lfac$(6%)), scr_bar$             , ch(18), */ ~
/* (EWD029)    at (10,02), fac(hex(84)), scr_txt$(5%)           , ch(20), */ ~
/* (EWD029)    at (10,25), fac(lfac$(7%)), scr_bar_code$        , ch(18), */ ~
                                                                             ~
/* (EWD029) */ at (09,02), fac(hex(84)), scr_txt$(4%)           , ch(20),~
               at (09,25), fac(lfac$(6%)), end_load$            , ch(05),~
               at (09,37), fac(hex(84)), end_desc$              , ch(30),~
                                                                         ~
               at (11,16), fac(hex(84)), scr$( 1%)              , ch(50),~
               at (12,16), fac(hex(84)), scr$( 2%)              , ch(50),~
               at (13,16), fac(hex(84)), scr$( 3%)              , ch(50),~
               at (14,16), fac(hex(84)), scr$( 4%)              , ch(50),~
               at (15,16), fac(hex(84)), scr$( 5%)              , ch(50),~
               at (16,16), fac(hex(84)), scr$( 6%)              , ch(50),~
               at (17,16), fac(hex(84)), scr$( 7%)              , ch(50),~
               at (18,16), fac(hex(84)), scr$( 8%)              , ch(50),~
               at (19,16), fac(hex(84)), scr$( 9%)              , ch(50),~
               at (20,16), fac(hex(84)), scr$(10%)              , ch(50),~
               at (21,16), fac(hex(84)), scr$(11%)              , ch(50),~
               at (22,16), fac(hex(84)), scr$(12%)              , ch(50),~
                                                                         ~
               at (23,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 10% then goto L40790
                  gosub display_codes
                  goto L40070

L40790:        if keyhit% <> 15 then goto L40830
                  call "PRNTSCRN"
                  goto L40330

L40830:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40940     /*  Input Mode             */
            pf$(1%)= "(1)Start Over        (10)Display Depts  " &        ~
                     "  (15)Print Screen     (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffff0affffffff0f1000)
            return

L40940: if fieldnr% > 0% then L40990  /*  Edit Mode - Select Fld */
            pf$(1%)= "(1)Start Over        (10)Display Depts  " &        ~
                     "  (15)Print Screen     (16)Print Report"
            pfkeys$ = hex(01ffffffffffffffff0aff0cffff0f1000)
            return
L40990:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50180,         /* Report Selection      */ ~
                              L50330          /* Production Date       */
                                              /* Product Line          */
                                              /* Load Number           */
                                              /* Begin Barcode         */
                                              /* Ending Barcode        */


            return

L50180: REM Report Selection                      SCR_CODE$
            if scr_code$ <> " " then goto L50210
               scr_code$ = "3"
L50210:     convert scr_code$ to selection%, data goto L50280

            convert selection% to scr_code$, pic(00)
                                                      /*  (EWD023)  */
                                                      /*  (EWD027)  */
                                                      /*  (AWD037)  */
                                                      /*  (AWd038)  */
            if selection% < 1% or selection% > 10% then goto L50280
               scr_msg1$ = str(scr$(selection% + 2%),9%,41%)
                                                      /* (AWD037)   */
                                                      /* (AWD038)   */
            if selection% =  1% then goto L50290
            if selection% = 10% then goto L50300
                                                      /* (AWD037)   */
        return
L50280:     errormsg$ = "Invalid Report Selection (1 thru 10)."
            gosub error_prompt
            init(" ") scr_code$
        return
                                              /* (AWD037)           */
L50290:
            run$ = "EWDPLN71"
            gosub Run_Program
        return clear all
        goto inputmode
                                              /* (AWD037)           */
                                              /* (AWD038)           */
L50300:
            run$ = "EWDPLN77"
            gosub Run_Program
        return clear all
        goto inputmode
                                              /* (AWD038)           */
L50330: REM Screen Prompts
            err% = 0%
            gosub check_date
            if err% <> 0% then return
            gosub check_dept
            if err% <> 0% then return
            gosub check_load
            if err% <> 0% then return
            gosub check_bar
            if err% <> 0% then return
            gosub check_bar_code
        return

        check_date                                             /*  (EWD027) */
            if selection% <> 2% and selection% <> 3% and selection% <> 6% ~
                                and selection% <> 7% and selection% <> 9% ~
                                 then return
            if scr_date$ <> " " then goto L50500
               scr_date$ = date$
L50500:     call "DATEOKC" (scr_date$, date%, errormsg$)
            if date% = 0% then goto L50580
            scr_dte1$ = scr_date$
            call "DATUFMTC" (scr_dte1$)
            if scr_date_end$ <> " " then goto L50560
                scr_date_end$ = scr_date$
L50560:     call "DATEOKC" (scr_date_end$, date%, errormsg$)
            if date% <> 0% then goto L50620
L50580:        gosub error_prompt
               err% = 1%
               init(" ") scr_date$, scr_date_end$, scr_dte1$, scr_dte2$
               return
L50620:     scr_dte2$ = scr_date_end$
            call "DATUFMTC" (scr_dte2$)
        return

        check_dept                                      /*  (EWD027) */
            if selection% <> 1% and selection% <> 2% and selection% <> 3%~
               and selection% <> 6% and selection% <> 7% and selection% <> 9% ~
                                             then return
            if scr_dept$ <> " " then goto L50710
               goto L50720
L50710:     if str(scr_dept$,1%,1%) <> "A" then goto L50760
L50720:        scr_dept$ = "ALL"
               dept$  = "(ALL) - Departments"
               dept1$ = "(ALL) - Departments"
               goto L50820
L50760:     scr_dept% = 0%
            convert scr_dept$ to scr_dept%,data goto L50800

            convert scr_dept% to scr_dept$,pic(000)
L50800:     dt_dept$ = scr_dept$
            gosub lookup_dept
L50820:     if len(scr_shft$) > 1 then goto L50850
L50830:        scr_shft$ = "AA"
               return
L50850:     if str(scr_shft$,1%,1%) = "A" then goto L50830
               convert scr_shft$ to scr_shft%, data goto L50910

               convert scr_shft% to scr_shft$, pic(00)

        return
L50910:     errormsg$ = "(Error) - Invalid Dept/Shift Code?"
            gosub error_prompt
            init(" ") scr_dept$, dept1$, dept$, sav_dept$, scr_shft$
            err% = 1%
        return

        check_load
            if selection% = 1% then return
            if selection% <> 2% and selection% <> 3% and                 ~
               selection% <> 6% and selection% <> 7% and                 ~
               selection% <> 9% then goto L51030
                                                     /* (EWD027)  */
/* AWD056      if scr_dept$ = "ALL" then goto L51030      Load Required */
               if len(scr_load$) > 1 then goto L51030
                  return                              /* Load Optional */
L51030:     scr_load% = 0%
                                                      /*  (EWD021)     */
REM            if pos("AS" = str(scr_load$,1%,1%)) > 0 then goto L51090
               convert scr_load$ to scr_load%, data goto L51190

               convert scr_load% to scr_load$, pic(00000)
               goto L51120
L51190:     convert str(scr_load$,2%,4%) to scr_load%, data goto L51160

            convert scr_load% to str(scr_load$,2%,4%), pic(0000)
L51120:     ld_load$ = scr_load$            /* Remove GOSUB STOCK_LOAD */
            gosub lookup_load
            if apc% = 0% then goto L51160
                                                   /*  (EWD029)   - BEG    */
            if selection% <> 5% and selection% <> 8% then return
               if end_load$ = " " then end_load$ = scr_load$
               end_load% = 0%

               convert end_load$ to end_load%, data goto L52190

               convert end_load% to end_load$, pic(00000)
               goto L52120
L52190:     convert str(end_load$,2%,4%) to end_load%, data goto L51160

            convert end_load% to str(end_load$,2%,4%), pic(0000)
L52120:     ld_load$ = end_load$
            init(" ") sav_desc$
            sav_desc$ = apc_desc$
            gosub lookup_load
            end_desc$ = apc_desc$
            apc_desc$ = sav_desc$
                                                   /*  (EWD029)  - END   */
        return
L51160:     errormsg$ = "(Error) - Invalid Load Number?"
            gosub error_prompt
            init(" ") ld_dtp1$, ld_dte1$, ld_dtp2$, ld_dte2$, ld_dtp3$,  ~
                      ld_dte3$, apc_desc$, scr_load$, sav_load$, ld_load$
        return

        check_bar
           if selection% <> 1% then return
              dt_key$ = all(hex(00))
              dt_key$ = scr_bar$
              read #fx%,key > dt_key$, using L51270, dt_key$, eod goto L51300
L51270:          FMT POS(24), CH(23)
              if str(dt_key$,1%,18%) <> scr_bar$ then goto L51300
        return
L51300:    errormsg$ = "(Error) - Invalid Beginning Barcode Entered?"
           gosub error_prompt
           init(" ") scr_bar$, scr_bar_code$
           err% = 1%
        return

        check_bar_code
           if selection% <> 1% then return
           if scr_bar_code$ <> " " then goto L51400
              scr_bar_code$ = scr_bar$
L51400:    if str(scr_bar_code$,1%,5%) <> "99999" then goto L51430
              scr_bar_code$ = "999999999999999999"
              return
L51430:    dtl_key$ = all(hex(00))
           dtl_key$ = scr_bar_code$
           read #fx%,key = dtl_key$, using L51270, dt_key$, eod goto L51490
           if str(dt_key$,1%,18%) <> scr_bar_code$ then goto L51490
        REM  IF SCR_BAR$ > SCR_BAR_CODE$ THEN GOTO 51490
        return
L51490:    errormsg$ = "(Error) - Invalid Ending Barcode Entered?"
           gosub error_prompt
           init(" ") scr_bar_code$
           err% = 1%
        return
  
        lookup_load                          /* (APCPLNLD) - Load File */
          if sav_load$ = ld_load$ then return
            apc% = 0%
            read #15,key = ld_load$, using L51600, ld_desc$, ld_dtp1$,    ~
                           ld_dtp2$, ld_dtp3$, ld_shp_blk$, ~
                                  eod goto lookup_load_done  /* CR1174 ShpBLK */
L51600:       FMT POS(16), CH(30), POS(70), 3*CH(8), POS(121), CH(3)
            ld_dte1$ = str(ld_dtp1$,1%,6%)   /* Planned Prod. Unformat */
            ld_dte2$ = str(ld_dtp2$,1%,6%)   /* Planned Comp. Unformat */
            ld_dte3$ = str(ld_dtp3$,1%,6%)   /* Planned Load. Unformat */
            call "DATEFMT" (ld_dtp1$)        /* Planned Prod. Date     */
            call "DATEFMT" (ld_dtp2$)        /* Planned Comp. Date     */
            call "DATEFMT" (ld_dtp3$)        /* Planned Load  Date     */
            sav_load$ = ld_load$
            apc_desc$ = ld_desc$
            apc% = 1%
        lookup_load_done
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************
                                                   /* Report Header */
L55090: %########@########                                    ###########~
        ~##############                                          APCPL41A:

L55120: %User Id: ###                        ############################~
        ~################################                      Page: #####

L55150: %########@########          #########################


L55180: %Id: ###   ######################################################~
        ~######Page: ###

L55210: %Load: #####  Description: ##############################

L55230: %Dept: ###  Load: #####    Description: #########################~
        ~#####
        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        screen_message
                                                 /* (AWD037)          */
                                                 /* (AWD038)          */
          init(" ") scr$(), scr_txt$()
          scr$(1%) ="**************************************************"
          scr$(2%) ="*         Report Options (By Production Date)    *"
          scr$(3%) ="* (1) - Print Single Production Barcode Label    *"
          scr$(4%) ="* (2) - Production Barcode Labels (Create)       *"
          scr$(5%) ="* (3) - Consolidation Report By Department       *"
          scr$(6%) ="* (4) - Finished Goods Pull Report               *"
          scr$(7%) ="* (5) - Staging/Loading Report by Load           *"
          scr$(8%) ="* (6) - Consol. Report By Dept. (No Parts)       *"
          scr$(9%) ="* (7) - Consol. Report By Dept. (Parts Only)     *"
          scr$(10%)="* (8) - Not Loaded Report                        *"
          scr$(11%)="* (9) - 'Specials' CheckList                     *"
          scr$(12%)="* (10)- Print Single NFRC Label                  *"
                                                 /* (AWD038)  */
                                                 /* (AWD037)  */
                                                 /* (EWD027)  */
        return

        screen_message_a
            scr_txt$(1%) = "Beg/End Prod. Date :"
            scr_txt$(2%) = "Department Code    :"
              scr_txt$(6%) = "Shift  :"
            scr_txt$(3%) = "Load Number        :"
REM            scr_txt$(4%) = "Beginning Barcode  "            /* (EWD029) */
REM            scr_txt$(5%) = "Ending Barcode     "            /* (EWD029) */
            scr_txt$(4%) = "End Load Number    :"              /* (EWD029) */
            scr_txt$(5%) = "Do you want history:"              /* (AWD046) */

            if selection% <> 2% then init(" ") scr_txt$(5%)    /* (AWD046) */

            if selection% <> 1% then goto L60150  /* By Dept Beg/End Bar*/
               init(" ") scr_txt$(1%), scr_txt$(3%)
               return
L60150:
                                                     /*  (EWD029)          */
            REM init(" ") scr_txt$(4%), scr_txt$(5%) /*Clear Barcode Prompt*/
            if selection% <> 5% and selection% <> 8% then scr_txt$(4%) = " "
            if selection% = 2% or selection% = 3%  or selection% = 6% or  ~
               selection% = 7% or selection% = 9% then return    /*(EWD027) */
               init(" ") scr_txt$(1%), scr_txt$(2%), scr_txt$(6%)
        return

        print_header                         /* Generic Report Heading */
          init(" ") scr_dte3$, rpt_time$
          str(scr_dte3$,1%,6%) = dtl_prod$
          call "DATEFMT" (scr_dte3$)
          print_title$ = "Production Report For (" & scr_dte3$ & ")"
          call "FMTTITLE" (print_title$, " ", 12%)
          call "TIME" (rpt_time$)
          page_no% = page_no% + 1%
          print page
          print using L55090, date$, rpt_time$, company$
          print using L55120, userid$, print_title$, page_no%
          print
          lcnt% = 3%
        return

        print_header_a
          init(" ") rpt_time$ : call "TIME" (rpt_time$)
          page_no% = page_no% + 1%
          print page
          print using L55150, date$, rpt_time$, company$
          print using L55180, userid$, print_title$, page_no%
          print
          print using L55230, sav_dept$, ld_load$, apc_desc$
          print
          lcnt% = 5%
        return

        get_piece
           dtl_so$         = str(dt_rec$,24%,8%)   /* Sale Order No.   */
           dtl_item_no$    = str(dt_rec$,32%,2%)   /* Line Item No.    */
           dtl_element_no$ = str(dt_rec$,34%,4%)   /* Piece Number     */
           dtl_ord_qty$    = str(dt_rec$,38%,4%)   /* S.O. Quantity    */
           dtl_prod$       = str(dt_rec$,47%,6%)   /* Production Date  */
           dtl_load$       = str(dt_rec$, 1%,5%)   /* Production Load  */
           ld_load$        = str(dt_rec$, 1%,5%)   /* Production Load  */
           dt_dept$        = str(dt_rec$,42%,3%)   /**Department Code  */
           dt_proc$        = str(dt_rec$,45%,2%)   /* Process Code     */
           dt_dte$         = str(dt_rec$,53%,6%)   /* Status Date      */
           dt_st$          = str(dt_rec$,64%,2%)   /* Current Status   */
           dt_shft$        = str(dt_rec$,104%,2%)  /* Prod Shift Code  */
           dtl_sort_prod$  = str(dt_rec$,106%,5%)  /* Product Sort     */
           dtl_part$       = str(dt_rec$,189%,25%) /* Part Number      */
           dt_ref$         = str(dt_rec$,96%,8%)   /**Prod Warranty No */
           dtl_cuscode$    = str(dt_rec$,124%,9%)  /* Customer Code    */
           apc_cust$       = dtl_cuscode$          /* Customer (6)     */
           dtl_txt$        = str(dt_rec$,236%,4%)  /* Line Item Text   */
           dt_seq$         = str(dt_rec$,111%,5%)  /* Prod. Seq No.    */
           dt_shp_blk$     = str(dt_rec$,)
           if selection% = 6% then goto skip_txt
           dtl_text$ = " NO"
           gosub'099(dtl_txt$)
           if txt% = 1% then dtl_text$ = "YES"
skip_txt
           pd_bar$   = str(dt_rec$,24%,18%)        /* Prod Barcode     */
           dt_key$   = str(dt_rec$,24%,23%)        /* Primary Key      */
           dt_key1$  = str(dt_rec$,47%,57%)        /* Alt Key 1        */
           dt_key2$  = str(dt_rec$,53%,51%)        /* Alt Key 2        */
           dt_key3$  = str(dt_rec$, 1%,23%)        /* Alt Key 3        */
           d_prod$   = str(dt_rec$,189%,1%)        /* Product Line N/A */
           d_mod$    = str(dt_rec$,189%,3%)        /* Model Code       */
           dt_special$= str(dt_rec$,220%,10%)      /* Special Flags    */
                                                   /* (EWD007)         */
           dt_wood$  = str(dt_rec$,217%,3%)        /* Wood Surround Cod*/
           scr_dte3$ = dtl_prod$                   /* Set Prod. Date   */
           call "DATEFMT" (scr_dte3$)

           if selection% = 6% then goto skip_subpart
                                                   /* (PAR000)         */
           init(" ") so_inv$, item_no$
           so_inv$  = dtl_so$
           item_no$ = dtl_item_no$
           gosub lookup_sub_part
           dtl_sub_part$ = str(bcksubpt_rec$,48%,20%)
           dtl_sub_info$ = str(bcksubpt_rec$,132%,9%) & " "
           str(dtl_new_part$,1%,25%)  = dtl_part$
           str(dtl_new_part$,26%,20%) = dtl_sub_part$
/* (AWD054) */
           series$ = str(bcksubpt_rec$,169%,16%)
           style$  = str(bcksubpt_rec$,185%,10%)
           mdl8900% = 0%
/*         if str(series$,1%,4%) = "1800" then                            ~
              mdl8900% = 1%    Ellison Brand 8900 */
/*         if str(series$,1%,4%) = "8900" then                            ~
              mdl8900% = 1%    Atrium Brand 8900 */
/*         if str(series$,1%,4%) = "8000" then                            ~
              mdl8900% = 1%    ViewPoint Brand 8900 */
/*         if str(series$,1%,4%) = "3900" then                            ~
              mdl8900% = 1%    Reliabilt Brand 8900 */
/*         if str(series$,1%,4%) = "90  " then                            ~
              mdl8900% = 1%    Lansing Brand 8900 */
/*         if str(series$,1%,4%) = "65  " then                            ~
              mdl8900% = 1%    Safe Harbor 65 Series All Brands */
/* (\AWD054) */
/* (IM8013) +  */
/*         if str(series$,1%,4%) = "1801" then                            ~
              mdl8900% = 1%    SilentGuard Series*/
/*         if str(series$,1%,4%) = "8901" then                            ~
              mdl8900% = 1%    SilentGuard Series*/
/*         if str(series$,1%,4%) = "8001" then                            ~
              mdl8900% = 1%    SilentGuard Series*/
/*         if str(series$,1%,4%) = "91  " then                            ~
              mdl8900% = 1%    SilentGuard Series*/
/*         if str(series$,1%,4%) = "3901" then                            ~
              mdl8900% = 1%    SilentGuard Series*/

/* (IM8013) -  */
/*SR75267 + Remove all hard code above and access GENCODES instead       */
           gosub lookup_mdl8900
/*SR75267 - */
                                                   /* (PAR000)         */
skip_subpart:
        return

        check_support
           supp% = 0%
           init(" ") readkey$
           str(readkey$,1%,9%)   = "PLAN SUPP"
           str(readkey$,10%,15%) = dt_dept$
           read #5,key = readkey$, eod goto check_support_done
           supp% = 1%
        check_support_done
        return

        rpt_2
/* (AWD049) */
/* (AWD054) - change to key off series */
REM           GOSUB LOAD_8900_MDLS
REM           GOSUB LOAD_65_MDLS                         /*(AWD053)*/

           sub_flg$ = "2"                             /* (PAR000) Rpt 2 */
           fx% = 2%
           if scr_hist$ = "Y" then fx% = 27%          /* (AWD046) */
           init(" ") dt_key1$,sav_dept$, sav_part$, sav_stock$,sav_cust$,~
                     sav_load$, or_key$, dt_key3$, p_sav_dept$
           pd_sort% = 0%                                /* (EWD007)     */
           call "SHOSTAT" ("Creating Prod/Pull Labels") /* Dept/Load    */
           dt_key1$ = all(hex(00))
           str(dt_key1$,1%,6%) = str(scr_dte1$,1%,6%)   /* Prod Date    */
           read #fx%,key 1% > dt_key1$, using L35050, dt_rec$,              ~
                                                      eod goto rpt_2_done
           goto L60805
        rpt_2_next
           read #fx%, using L35050, dt_rec$, eod goto rpt_2_done
L60805:         gosub get_piece
           if dtl_prod$ > str(scr_dte2$,1%,6%) then goto rpt_2_done
/* (AWD056) + */
           if scr_dept$ <> "ALL" then goto L60829     /* 'Alpha','Stock' */
              if schema% <> 1% then goto ntx_schema
                 for i% = 1% to nc_depts% step 3%
                   if dt_dept$ = str(nc_depts$,i%, 3%) then goto rpt_2_next
                 next i%
              goto L60830
       ntx_schema
                 for i% = 1% to ntx_depts% step 3%
                   if dt_dept$ = str(ntx_depts$,i%, 3%) then goto rpt_2_next
                 next i%
              goto L60830
/* (AWD056) - */

L60829:
              if scr_dept$ <> dt_dept$ then goto rpt_2_next

L60830:    if len(scr_load$) < 5 then goto L60865
              if scr_load$ <> dtl_load$ then goto rpt_2_next
                 if scr_dept$ = dt_dept$ then goto L60870
/* CR2219 + */
REM                    if dt_dept$ = "102" or dt_dept$ = "104" then         ~
REM                                                          goto rpt_2_next
                  if schema% = 2% and (dt_dept$ = "102" or dt_dept$ = "104") ~ 
                              then goto rpt_2_next
                  if schema% = 1% and dt_dept$ = "102" then goto rpt_2_next
                  
                  goto L60870
                                             /* Skip 'A' Loads         */
                                             /* Monify for NC print    */
L60865:     if schema% = 2% then goto L60867
            if pos("S" = str(dtl_load$,1%,1%)) > 0 then goto rpt_2_next
            if pos("A" = str(dtl_load$,1%,1%)) > 0  and dt_dept$ <> "104"  ~
                             then goto rpt_2_next
            goto L60870
            
L60867:     if pos("AS" = str(dtl_load$,1%,1%)) > 0 then goto rpt_2_next
/* CR2219 - */
L60870:     gosub check_support
/*SR68269*/ if scr_dept$ = "ALL" and dt_dept$ = "104" then supp% = 0%
            if dt_dept$ = "102" and scr_dept$ = "102" then supp% = 0%
            if scr_dept$ = "ALL" and dt_dept$ = "102"  and schema% = 2% ~
                          then supp% = 0%
            if dt_dept$ = "104" and scr_dept$ = "104" then supp% = 0%
            if supp% = 1% then goto rpt_2_next
            if scr_shft$ = "AA" then goto L60900
               if dt_shft$ <> scr_shft$ then rpt_2_next
L60900: 
            gosub print_label
            goto rpt_2_next
        rpt_2_done
        scr_hist$ = " "
        return

L60925: %                                        Department: ############~
        ~##################

L60940: %Shift Code: ##     Load Number: #####

L60950: %Seq. !Qty  Mod CL <---- Description and Size ----> @ <-------- S~
        ~pecial Options --------->  Load  Dp Warranty  <----- Bar Code ---~
        ~->
L60965: %-----!---- --- -- -------------------------------- - -----------~
        ~-------------------------  ----- -- --------  -------- -- ---- --~
        ~--
L60980: %#####!#### ### ## ################################ # ###########~
        ~#########################  ######################################~
        ~##
L60995: %                                                        Special ~
        ~Text:############################################################

L61050: %                                                        Parent L~
        ~oad :############################################################

/* (AWD041) */
L61055: %                   SCLMR: ################              Parent L~
        ~oad :############################################################


L61010: % Total Product: ########

        rpt_3_header
            gosub print_header
            print using L60925, dept$
            print using L60940, sav_shft$, sav_load$
            print
            print using L60950
            print using L60965
            lcnt% = lcnt% + 5%
        return

        check_product                           /* Control Page Breaks */
           if selection% = 9% then return                /*  (EWD027)   */
           if sav_load$ = dtl_load$ then goto L61090     /* For Load    */
              sav_load$ = dtl_load$                     /*  Department */
              gosub rpt_3_done                          /*  Shift      */
L61090:    if sav_dept$ = dt_dept$ then goto L61110
              sav_dept$ = dt_dept$
              gosub lookup_dept
              gosub rpt_3_done
L61110:    if sav_shft$ = dt_shft$ then return
              sav_shft$ = dt_shft$
              gosub rpt_3_done
        return

        rpt_3
           sub_flg$ = "3"                     /* (PAR000) - Rpt 3      */
           call "SHOSTAT" ( "Printing Production Report" )
           rack% = 0%                         /* (EWD006) - Rack Labels*/
           print_title$ = "Production Report For (" & scr_dte3$ & ")"
           call "FMTTITLE" (print_title$, " ", 12%)
           end_of_file% = 0% : tot_count% = 0%
           init(" ") dt_key1$,sav_dept$, sav_part$, sav_stock$,sav_cust$,~
                     sav_load$, or_key$, sav_key$, sav_key2$, sn$(),     ~
                     apcbar$(), sav_shft$, sav_dte3$, sv_pload$()

           dt_key1$ = all(hex(00))          /* Primary Department Key */
           str(dt_key1$,1%,6%) = str(scr_dte1$,1%,6%)
           sav_key$ = dt_key1$
           read #fx%,key 1% > dt_key1$, using L35050, dt_rec$,              ~
                                                    eod goto end_of_file
           sav_load$ = str(dt_rec$,1%,5%)
           sav_dept$ = str(dt_rec$,42%,3%)
           sav_shft$ = str(dt_rec$,104%,2%)
           sav_dte3$ = str(dt_rec$,47%,6%)
           goto L61235
        rpt_3_next
            read #fx%, using L35050, dt_rec$, eod goto end_of_file
L61235:     gosub get_piece
            gosub check_selection             /* Sel (6),(7) Reports    */
            if tst_selection% = 1% then goto rpt_3_next

            if dtl_prod$ > str(scr_dte2$,1%,6%) then goto end_of_file
            if scr_dept$ = "ALL" then goto L61260         /* Check Load */
               if scr_dept$ <> dt_dept$ then goto rpt_3_next

L61260:     if len(scr_load$) < 5 then goto L61295
               if scr_load$ <> dtl_load$ then goto rpt_3_next
                 if scr_dept$ = dt_dept$ then goto L61300
                    if dt_dept$ = "102" or dt_dept$ = "104" then         ~
                                                          goto rpt_3_next
                  goto L61300
                                             /* Skip 'A'               */
L61295:     if pos("A" = str(dtl_load$,1%,1%)) > 0 then goto rpt_3_next
L61300:     gosub check_support         /* Department is Specified.    */
                                        /* (EWD001) - 06/02/98 Begin   */
            if dt_dept$ = "044" and scr_dept$ = "044" then supp% = 0%
                                        /*        (EWD032)             */
            if dt_dept$ = "054" and scr_dept$ = "054" then supp% = 0%
                                        /*        (AWD040)             */
            if dt_dept$ = "074" and scr_dept$ = "074" then supp% = 0%
            if dt_dept$ = "102" and scr_dept$ = "102" then supp% = 0%
            if dt_dept$ = "104" and scr_dept$ = "104" then supp% = 0%
            if supp% = 1% then goto rpt_3_next
                                                        /*  (EWD024)   */
REM         IF DT_DEPT$ <> "044" THEN GOTO RPT_3_NEXT_A /* (EWD001)    */

            init(" ") sc_pload$, sv_pload$
               gosub lookup_parent      /* (EWD001) - 06/02/98         */
            if dt_dept$ <> "044" then goto rpt_3_next_a /* (EWD001)    */
               if str(sc_pload$,1%,1%) = "A" then goto rpt_3_next
                                        /* (EWD001) - 06/02/98 End     */
        rpt_3_next_a
                                                /* (EWD027)  */
            if selection% = 9% then gosub check_specials
            if specials% = 1% then rpt_3_next
            if sav_key2$ <> " " then goto L61370
               str(sav_key$,1%,25%) = str(dt_rec$,189%,25%) /*Part No*/
                                                            /* (PAR000) */
                                                            /* Sub Part */
               str(sav_key$,26%,20%)= str(dtl_new_part$,26%,20%)
                                                            /* (PAR001) */
               str(sav_key$,46%,10%)= dtl_sub_info$

REM               call "SHOSTAT" (" SAVE KEY " & sav_key$ )  stop
               sav_key2$ = dt_key1$
               gosub check_product
               count% = 1%
               init(" ") count$
               gosub set_barcode_3
               tot_count% = tot_count% + 1%
               goto rpt_3_next

L61370:     if str(sav_key2$,1%,17%) <> str(dt_key1$,1%,17%) or          ~
               str(sav_key2$,20%,30%) <> str(dt_key1$,20%,30%) then      ~
                                                               goto L61410
               count% = count% + 1%
               gosub set_barcode_3
               tot_count% = tot_count% + 1%
               goto rpt_3_next
                                                       /* Print Detail */
L61410:     if sav_key2$ <> " " then goto L61415 else goto rpt_3_done
L61415:        dtl_part$     = str(sav_key$,1%,25%)
               dtl_sub_part$ = str(sav_key$,26%,20%)   /* (PAR000)     */
               dtl_sub_info$ = str(sav_key$,46%,10%)   /* (PAR001)     */

REM          call "SHOSTAT" ("part and sub " & dtl_part$ & dtl_sub_part$)  stop
               dtl_prod$ = str(sav_key2$,1%,6%)
               mod$      = str(dtl_part$,1%,3%)
               sav_txt$  = dtl_txt$
               dtl_txt$  = str(apcbar$(1%),44%,4%)
                 fx% = 2%
               gosub lookup_part
               gosub lookup_stock                      /* (EWD016)     */
               apc_sod$ = " "

               sclmr$ = str(dtl_part$,20,3)
               hg$    = str(dtl_part$,9,2)
               sclmr% = 0%
               if hg$ < "70" or hg$ > "97" then goto not_sclmr
                  if sclmr$ = "000" then goto not_sclmr
                    clmr = 0.00

                    convert sclmr$ to clmr, data goto not_sclmr
                    clmr = clmr / 10.0
                    a% = int(clmr) : b = (clmr - a%) / 0.8
                    clmr = a% + b
                    calc = clmr
                    gosub con_fract
                    sclmr% = 1%

not_sclmr:

               if len(dtl_part$) > 18 then                               ~
                         call "APCLDSUB" (dtl_part$, apc_sod$, #11, err%)
                                                       /* (RHHTEST)    */
              if len(dtl_part$) > 18 then                               ~
               call "AWDDESCR" (dtl_part$, dtl_sub_part$, apc_scr$, apc_prt$,~
                                   sub_scr$, sub_prt$, apc_sze$, #11, err% )
               apc_sod$ = apc_sod$ & sub_prt$

REM               call "SHOSTAT" (" dtl part " & dtl_part$ & apc_sod$) stop
REM               call "SHOSTAT" (" dtl sub " & dtl_sub_part$ & sub_sod$)  stop


               gosub lookup_color
               for i% = 1% to count%                   /* Print Detail */
                                                       /* (EWD005)     */
REM                   GOSUB SORT_RACK_DATA             /* (EWD006)     */
                                                       /* (EWD005)     */

                   if sav_dte3$ <> dtl_prod$ then goto L61485
                   if lcnt% < 60% then goto L61495
L61485:               sav_dte3$ = dtl_prod$
                      gosub rpt_3_header
L61495:            if count$ <> " " then L61540
                      convert count% to cnt$, pic(####)
                      count$ = cnt$
                      print using L60980, sn$(i%), cnt$, mod$, cl$,       ~
                           part_desc$, stock_flag$, str(apc_sod$,1%,36%),~
                           str(apcbar$(i%),1%,40%)
                      sav_cust$ = str(apcbar$(i%),48%,9%)
                      init(" ") part_desc$
                      goto L61580
L61540:            if sav_cust$ = str(apcbar$(i%),48%,9%) then goto L61560
                      sav_cust$ = str(apcbar$(i%),48%,9%)
                      dtl_cuscode$ = str(apcbar$(i%),48%,9%)
                  fx% = 2%
                      gosub lookup_part
L61560:            print using L60980, sn$(i%), " ", " ", " ", part_desc$,~
                                      " ", " ", str(apcbar$(i%),1%,40%)
                   init(" ") part_desc$

L61580:            lcnt% = lcnt% + 1%
                   dtl_txt$     = str(apcbar$(i%),44%,4%)
                   gosub lookup_text
                   if text_flag$ = "N" then goto L61635
                      if text_d$(1%) <> " " then                         ~
                         print using L60995, text_d$(1%)
                      if text_d$(2%) <> " " then                         ~
                         print using L60995, text_d$(2%)
                      lcnt% = lcnt% + 1%
                      if text_d$(1%) <> " " and text_d$(2%) <> " " then  ~
                         lcnt% = lcnt% + 1%     /* Add Additional Line */
L61635:        next i%
                                                        /* (AWD041) */
                                                        /*  (EWD024) */
               if sclmr% = 0% then print using L61050, sv_pload$(i%)
               if sclmr% <> 0% then print using L61055, calc$, sv_pload$(i%)


            init (" ") apcbar$(), sav_key2$, count$, sn$(), sav_key$,    ~
                       sv_pload$()
            dtl_txt$ = sav_txt$
            lcnt% = lcnt% + 1% : print
            if end_of_file% = 1% then goto rpt_3_done
           goto rpt_3_next_a
        rpt_3_done
           if end_of_file% = 1% and tot_count% <> 0% then gosub allocate_racks
           if tot_count% = 0% then return        /* (EWD006) Primary   */
                                                 /* Exit of routine    */
           convert tot_count% to tot_count$, pic(########)
           print
           print using L61010, tot_count$
           tot_count% = 0% : tot_count$ = " "
           lcnt% = 99%
        return

        end_of_file
          end_of_file% = 1%
          goto L61410
                                                /* (EWD015) 10/25/00    */
        check_selection                         /* New Without Parts or */
          tst_selection% = 0%                   /* Parts only. No Temp. */
          if selection% <> 6% and selection% <> 7% then return
             if selection% = 7% then goto L61640
                if len(dtl_part$) < 19 then tst_selection% = 1%
                                                   /*  (EWD026)            */
REM                   GOSUB LOOKUP_TEMP            /* (EWD015)             */
REM                   IF TEMP% = 1% THEN TST_SELECTION% = 1%
                return
L61640:   if len(dtl_part$) > 18% then tst_selection% = 1% /*Parts Only*/
        return

        check_specials                             /*  (EWD027) - Begin  */
            specials% = 0%
            if str(dtl_part$,5%,3%) <> "WAR" then goto special_part
                  specials% = 1%
                  return
special_part
            if str(dtl_part$,5%,2%) = "89" then return          /*  Glass  */
            if str(dtl_part$,7%,2%) = "99" then return          /*  Liting */
            if str(dtl_part$,7%,2%) = "72" or                  /* (EWD033) */~
               str(dtl_part$,7%,2%) = "73" then return

            gosub check_glass

            if glass% = 0% then return
            p% = 0%                                           /* (PAR000)  */
        REM    p% = pos("ABCD" = str(dtl_part$,11%,1%))       /* Screen    */
            if str(dtl_sub_part$, 5%,1%) = "1" then p% = 1%
            if str(dtl_sub_part$, 5%,1%) = "4" then p% = 1%   /* (AWD050) */
            if p% <> 0% then return                           /* With Foam */

            p% = 0%                                           /* (PAR000)  */
        REM    p% = pos("HIJKLMOXYZ" = str(dtl_part$,12%,1%)) /* Locks     */
            p% = pos("HJMO" = str(dtl_part$,12%,1%))
            if p% <> 0% then return                           /* Brass     */

            p% = 0%                                           /* (PAR000)  */
        REM    p% = pos("CDEF" = str(dtl_part$,4%,1%))        /* Color     */
            if str(dtl_sub_part$,1%,1%) = "3" then p% = 1%
            if p% <> 0% then return                           /* Brass Grid*/

            gosub check_models
            if models% = 0% then return

            specials% = 1%
        return

        check_glass
               glass% = 0%
               init(" ") readkey$
               str(readkey$,1%,9%)   = "PRICEGRID"
               str(readkey$,10%,15%) = str(dtl_part$,7%,2%)   /* Grid  */

               read #5,key = readkey$, eod goto check_100
                    return                        /* Has Sash Only Grids */
check_100:
REM               init(" ") readkey$
REM               str(readkey$,1%,9%)   = "PLAN 100 "
REM               str(readkey$,10%,15%) = str(dtl_cuscode$,1%,6%)


REM               read #5,key = readkey$, eod goto check_obs
REM                    return                        /* 100% Inspection     */
REM check_obs
               init(" ") readkey$
               str(readkey$,1%,9%)   = "GED 001  "
               str(readkey$,10%,15%) = str(dtl_part$,5%,2%)   /* Glass  */

               read #5,key = readkey$, using L63680, desc$, eod goto check_gls
                           p% = 0%
                           p% = pos(desc$ = "O")      /* Has Obscure   */
                           if p% <> 0% then return
                                                        /*  (EWD033) BEG */
check_gls
               init(" ") readkey$
               str(readkey$,1%,9%)   = "SPECCHECK"
               str(readkey$,10%,15%) = str(dtl_part$,5%,2%)   /* Glass  */

               read #5,key = readkey$, eod goto glass_done
                           return


        glass_done
               glass% = 1%
                                                        /*  (EWD033) END */
        return

        check_models                        /* Put DryWall Models on Report */
            models% = 0%
               init(" ") readkey$
               str(readkey$,1%,9%)   = "PLAN DRYW"
               str(readkey$,10%,15%) = str(dtl_part$,1%,3%)   /* Model  */

               read #5,key = readkey$, eod goto not_dry_wall
                    return

        not_dry_wall
            models% = 1%
        return                                    /*  (EWD027)  -  END   */


        rpt_6
           sub_flg$ = "6"
           call "SHOSTAT" ( "Printing Production Report - Rack " )
           rack% = 0%
           print_title$ = "Production Report For (" & scr_dte3$ & ")"
           call "FMTTITLE" (print_title$, " ", 12%)
           end_of_file% = 0% : tot_count% = 0%
           init(" ") dt_key1$,sav_dept$, sav_part$, sav_stock$,sav_cust$,~
                     sav_load$, or_key$, sav_key$, sav_key2$, sn$(),     ~
                     apcbar$(), sav_shft$, sav_dte3$, sv_pload$()

           dt_key1$ = all(hex(00))          /* Primary Department Key */
           str(dt_key1$,1%,6%) = str(scr_dte1$,1%,6%)
           sav_key$ = dt_key1$
           read #2,key 1% > dt_key1$, using L35050, dt_rec$,              ~
                                                    eod goto rpt_6_done
           sav_load$ = str(dt_rec$,1%,5%)
           sav_dept$ = str(dt_rec$,42%,3%)
           sav_shft$ = str(dt_rec$,104%,2%)
           sav_dte3$ = str(dt_rec$,47%,6%)
           goto L61240
        rpt_6_next
            read #2, using L35050, dt_rec$, eod goto rpt_6_done
L61240:
            dtl_prod$ = str(dt_rec$,47%,6%)
            if dtl_prod$ > str(scr_dte2$,1%,6%) then goto rpt_6_done
            dt_dept$  = str(dt_rec$,42%,3%)
/*CR00604 + */
            if scr_dept$ <> "ALL" then goto L61264         /* Check Load */
               if schema% <> 1% then goto TX_depts
               for ji% = 1% to nc_depts6% step 3%
                 if dt_dept$ = str(nc_depts6$,ji%,3%) then goto L61265
               next ji%
            goto rpt_6_next
        TX_depts
               for ji% = 1% to ntx_depts6% step 3%
                 if dt_dept$ = str(ntx_depts6$,ji%,3%) then goto L61265
               next ji%
            goto rpt_6_next
/*CR00604 - */
L61264:        if scr_dept$ <> dt_dept$ then goto rpt_6_next

L61265:
            gosub get_piece
            gosub check_selection             /* Sel (6),(7) Reports    */
            if tst_selection% = 1% then goto rpt_6_next


            if len(scr_load$) < 5 then goto L61296
/*CR00604      if scr_load$ <> dtl_load$ then goto rpt_3_next * Correct this bug as a part of CR00604 */
               if scr_load$ <> dtl_load$ then goto rpt_6_next
                 if scr_dept$ = dt_dept$ then goto L61310
                    if dt_dept$ = "102" or dt_dept$ = "104" then         ~
                                                          goto rpt_6_next
                  goto L61310
                                             /* Skip 'A'               */
L61296:     if pos("A" = str(dtl_load$,1%,1%)) > 0 then goto rpt_6_next
L61310:     gosub check_support         /* Department is Specified.    */
                                        /* (EWD001) - 06/02/98 Begin   */
            if dt_dept$ = "044" and scr_dept$ = "044" then supp% = 0%
                                        /*        (EWD032)             */
            if dt_dept$ = "054" and scr_dept$ = "054" then supp% = 0%
                                        /*        (AWD040)             */
            if dt_dept$ = "074" and scr_dept$ = "074" then supp% = 0%
            if dt_dept$ = "102" and scr_dept$ = "102" then supp% = 0%
            if dt_dept$ = "104" and scr_dept$ = "104" then supp% = 0%
            if supp% = 1% then goto rpt_6_next
   
            if dt_dept$ <> "044" then goto rpt_6_next_a
               if str(sc_pload$,1%,1%) = "A" then goto rpt_6_next
        rpt_6_next_a

            gosub sort_rack_data
           goto rpt_6_next
        rpt_6_done
           gosub allocate_racks

        return


        allocate_racks
           if rack% = 0% and del% = 0% then return
REM           IF SELECTION% <> 6% THEN RETURN  /*(AWD047)*/
           if selection% <> 6% and keyhit% <> 12% then return
REM            call "SHOSTAT" (" CALLING APCPL41C ")   stop

           del% = 0%                       /* (AWD047) */
           if keyhit% <> 12% then goto not_allocate_delete
              del% = 1%
              dtl_prod$ = scr_dte1$

not_allocate_delete:

           init(" ") wrk_rec$, wrk_key$, or_hows$
REM           ff% = 17%                /* (AWD039) */
           ff% = 23%                /* (AWD050) */  
           pass% = 0%
/* (AWD050) */
next_pass:
           pass% = pass% + 1%
           convert pass% to cmg$, pic(000)
       
REM           call "SHOSTAT"("PASS -->  " & cmg$)  stop

/*CR00604  if pass% = 14% then goto next_pass                          */
/*CR00604  if pass% > 16% then goto rack_labels_done                   */
/* CR2296 */   
           if pass% > 21% and schema% = 2% then goto rack_labels_done
           if pass% > 22% and schema% = 1% then goto rack_labels_done 
                                                
           wrk_key$ = all(hex(00))

L61650:    read #1,key > wrk_key$, using L61660, wrk_rec$, eod goto L61670
L61660:         /* FMT CH(55) */ FMT CH(256)

              wrk_key$  = str(wrk_rec$,1%,55%)

              dtl_prod$ = str(wrk_key$,1%,6%)
              dt_dept$  = str(wrk_key$,7%,3%)
              warranty$ = str(wrk_key$,19%,8%)
              userid$   = str(wrk_key$,27%,3%)
              or_hows$  = str(wrk_key$,35%,2%)             /* (AWD035) */
              dt_seq$   =  str(wrk_rec$,56%,5%)
              rk_err% = 0%                   /* (EWD005) Allocate Rack */
                                             /* (EWD008) check_size    */
                                             /* (PAR000) Changed       */
                                             /* APCPLNGR            Chg*/
       

              call "APCPL41C" (warranty$,           /* Warranty Id     */~
                               dtl_prod$,           /* Production Date */~
                               dt_dept$,            /* Department Code */~
                               userid$,             /* Userid          */~
                               or_hows$,            /* Hows   (AWD035) */~
                               del%,                /* (AWD047)        */~
                               #5,                  /* (GENCODES) File */~
                               #16,                 /* (APCPLNGR) File */~
                               #ff%,                /* (AWDPLNRK) File */~
                               pass%,               /* Number of times */~
                               schema%,             /* Schema          */~
                               rk_err% )            /* Error Code 0%=Ok*/
                                                    /* (EWD005) End    */
REM                 goto clear_data_next
           goto L61650
L61670:     
              rk_err% = 0%                          /* (EWD006) Done   */
              call "APCPL41C" (warranty$,           /* Warranty Id     */~
                               dtl_prod$,           /* Production Date */~
                               "DDD",               /* Department Code */~
                               userid$,             /* Userid          */~
                               or_hows$,            /* Hows   (AWD035) */~
                               del%,                /* (AWD047)        */~
                               #5,                  /* (GENCODES) File */~
                               #16,                 /* (APCPLNGR) File */~
                               #ff%,                /* (AWDPLNRK) File */~
                               pass%,               /* Number of times */~
                               schema%,             /* Schema          */~
                               rk_err% )            /* Error Code 0%=Ok*/
                                                    /* (EWD006) End    */

           goto next_pass


rack_labels_done:
           gosub delete_work
        return

                                                    /* (PAR000)        */
                                                    /* (EWD006)        */
        sort_rack_data
           if selection% <> 6% then return

           if rack% <> 0% then goto L61680
              mode% = 1% : gosub open_work          /* only Open Once  */
              mode% = 3% : gosub open_work

L61680:    rack% = rack% + 1%

           init(" ") wrk_rec$
           str(wrk_rec$,1%,6%)   = dtl_prod$        /* Production Date */
           str(wrk_rec$,7%,3%)   = dt_dept$
           str(wrk_rec$,10%,4%)  = "0000"           /* Spacer          */
           str(wrk_rec$,14%,5%)  = dt_seq$          /* Dept Seq. No.   */
           str(wrk_rec$,19%,8%)  = dt_ref$          /* Warranty        */
           str(wrk_rec$,27%,3%)  = userid$          /* Userid          */
           convert rack% to str(wrk_rec$,30%,5%), pic(#####)
                                                    /* Unique Key      */
           str(wrk_rec$,35%,2%)  = or_hows$         /* (AWD035)        */
REM PUT SEQ IN POS 56 BECAUSE KEY IS 1 - 55, DO NOT WANT TO EFFECT SORT
           str(wrk_rec$,56%,5%)  = dt_seq$

           put #1, using L61710, wrk_rec$
L61710:        FMT CH(256)                          /* (PAR000)        */
                                                    /* (EWD020)        */
           write #1, eod goto L61715
L61715:
        return  

L61730: %Prod Part Description                 Qty Req'd  Bin  Part Numbe~
        ~r
L61740: %---- -------------------------------- --------- ----- ----------~
        ~---------------
L61750: % ### ################################    ####   ##### ##########~
        ~###############
L61760: %     ########################################

L61770: %     ###################################       #####

        rpt_4_header                               /* INVENTORY PULL */
            gosub print_header_a
            print using L61730
            print using L61740
            lcnt% = lcnt% + 2%
        return

        rpt_4
            sub_flg$ = "4"                        /* (PAR000) - Rpt - 4 */
            tot_count% = 0%
            print_title$ = "Pull from Stock for Load Number: "&scr_load$
            call "FMTTITLE" (print_title$, " ", 12%)
            call "SHOSTAT" ( "Printing Pull From Stock Report" )
            init(" ") sav_key2$, apcbar$(), sn$(), sav_key$, sav_dept$,  ~
                      sav_load$, sav_part$, sv_pload$
            dt_key1$  = all(hex(00))
            ld_load$  = scr_load$
            gosub lookup_load
            str(dt_key1$,1%,6%)  = ld_dte1$       /* Planned Prod Date */
            read #fx%,key 1% > dt_key1$, using L35050, dt_rec$,             ~
                                                     eod goto L62035
            goto L61895
        rpt_4_next
            read #fx%, using L35050, dt_rec$, eod goto L62035
L61895:     gosub get_piece
            if dt_dept$ <> "102" and dt_dept$ <> "104" then              ~
                                                       goto rpt_4_next
            if sav_dept$ <> " " then goto L61925
               sav_dept$ = dt_dept$

L61925:     if sav_dept$ = dt_dept$ then goto rpt_4_next_a
               sav_dept$ = dt_dept$
               gosub rpt_4_done
        rpt_4_next_a
            if dtl_prod$ > ld_dte3$ then goto L62035
            if scr_load$ <> dtl_load$ then goto rpt_4_next
            if sav_key2$ <> " " then goto L62000
               str(sav_key$,1%,25%) = str(dt_rec$,189%,25%) /*Part No  */
               str(sav_key$,26%,5%) = str(dt_rec$,106%,5%)  /*Prod Srt */
                                                            /*(PAR000) */
               str(sav_key$,31%,20%)= dtl_sub_part$
                                                            /* (PAR000)*/
               str(sav_key$,51%,10%)= dtl_sub_info$         /* (PAR001)*/

               sav_key2$ = dt_key1$
               count% = 1%
               count$ = " "
               gosub set_barcode
               tot_count% = tot_count% + 1%
               goto rpt_4_next
L62000:     if str(sav_key2$,1%,17%) <> str(dt_key1$,1%,17%) or          ~
               str(sav_key2$,20%,30%) <> str(dt_key1$,20%,30%) then      ~
                                                               goto L62035
               count% = count% + 1%
               gosub set_barcode
               tot_count% = tot_count% + 1%
               goto rpt_4_next
L62035:     if sav_key2$ <> " " then goto L62050        /* Print Detail */
               goto rpt_4_done

L62050:     dtl_part$      = str(sav_key$,1%,25%)
            mod$           = str(dtl_part$,1%,3%)
            dtl_sort_prod$ = str(sav_key$,26%,5%)
            dtl_sub_part$  = str(sav_key$,31%,20%)      /* (PAR000)     */
            dtl_sub_info$  = str(sav_key$,51%,10%)      /* (PAR001)     */

            fx% = 2%
            gosub lookup_part
            apc_sod$ = " "
            if len(dtl_part$) > 18 then                                  ~
                         call "APCLDSUB" (dtl_part$, apc_sod$, #11, err%)
                                                       /* (RHHTEST)    */
              if len(dtl_part$) > 18 then                               ~
               call "AWDDESCR" (dtl_part$, dtl_sub_part$, apc_scr$, apc_prt$,~
                                   sub_scr$, sub_prt$, apc_sze$, #11, err% )
               apc_sod$ = apc_sod$ & sub_prt$
            convert count% to count$, pic(####)

            if lcnt% > 60% then gosub rpt_4_header
            print using L61750, mod$, part_desc$, count$, dtl_sort_prod$, ~
                               dtl_part$
            print using L61760, apc_sod$
            print
            lcnt% = lcnt% + 3%           /* PRINT ASSOCIATED BAR CODES */
            for i% = 1% to count%        /* FOR LABELS TO USE          */
                if lcnt% > 60% then gosub rpt_4_header
                   print using L61770, str(apcbar$(i%),1%,35%), sn$(i%)
                   lcnt% = lcnt% + 1%
            next i%
            print
            lcnt% = lcnt% + 1%
            init (" ") apcbar$(), sav_key2$, count$, sn$(), sav_key$, ~
                       sv_pload$
            goto rpt_4_next_a

        rpt_4_done
            if tot_count% = 0% then return
            if lcnt% = 99% then return
            convert tot_count% to tot_count$, pic(########)
            print using L61010, tot_count$
            tot_count% = 0% : tot_count$ = " "
            lcnt% = 99%
        return

L62220: %Planned Production Date: ########    Completion Date: ########  ~
        ~        Load Date: ########
                                                           /* (EWD012) */
L62230: %Drop Customer  <------- Customer Name ------> <----- Part Number~
        ~ -----> P/M <---- Bar Code -----> Prod Seq < Status > P.O. Number
L62240: %---- --------- ------------------------------ ------------------~
        ~------- --- --------------------- -------- ---------- -----------
L62250: % ##  ######### ############################## ##################~
        ~####### ### ######## ## #### ####   #####  ########## ###########
                                                           /* (EWD012) */

L62265: %     Route: ##### Load: ##### Ref: ######## ##################, ~
        ~## ############################################################

L62280: %----------------------------------------------------------------~
        ~-----------------------------------------------------------------

L62295: % Total Product For Load: ########           Undefined : ########~
        ~      Total Trans: ########           Total RGA: ########
L62300: % Total Product Staged  : ######## ##############################
L62305: % Total Product Loaded  : ######## ##############################
L62310: % Total Product B.O.L.  : ######## ##############################
L62315: % Total Product Invoiced: ######## ##############################
L62320: % Total Product In Drop : ######## ##############################

        rpt_5_header
            gosub print_header
REM            print using L55210, sav_load$, apc_desc$
            print using L55210, scr_load$, apc_desc$
            print using L62220, ld_dtp1$, ld_dtp2$, ld_dtp3$
            print
            print using L62230
            print using L62240
            lcnt% = lcnt% + 5%
        return

        rpt_5
            init(" ") sav_drop$, sav_part$, sav_cust$, sav_st$,          ~
                      tot_load$, tot_stage$, tot_loaded$, tot_bol$,      ~
                      tot_closed$, tot_undef$, sav_load$, save_part$,    ~
                      sav_dept$, how_txt$, tot_warr$, tot_tran$, tot_rga$
            tot_load% = 0% : tot_stage%  = 0% : tot_loaded% = 0%
            tot_bol%  = 0% : tot_closed% = 0% : tot_undef% = 0%
            tot_warr% = 0%   /* (AWD041) */
            tot_tran% = 0% : tot_rga% = 0%  /* (SR79626) */
            print_title$ = "Staging/Loading Summary Report"
            call "FMTTITLE" (print_title$, " ", 12%)
            ld_load$ = scr_load$
            gosub lookup_load
            gosub sort_data_col

            call "SHOSTAT" ("Staging/Loading Summary Report")
            wrk_key$ = all(hex(00))
        rpt_5_next
            read #1,key > wrk_key$,using L35110, wrk_rec$,                ~
                                                      eod goto rpt_5_done
            gosub get_work
            if str(dtl_part$,5,4) = "WARR" then goto L62730  /* AWD044 */
            gosub lookup_cust
            fx% = 2%
            gosub lookup_part
            if len(apc_prt$) > 30 then goto L62500
               gosub lookup_text
               if text_flag$ = "Y" then                                  ~
                  str(apc_prt$,35%,26%) = " TEXT:"&str(text_desc$,1%,20%)
L62500:     gosub lookup_status
            dtl_t$ = "Mak"
            if dt_dept$ = "095" then dtl_t$ = "Stk"
            if dt_dept$ = "102" then dtl_t$ = "PSk"
            if dt_dept$ = "104" then dtl_t$ = "PMl"        /* (EWD020) */
            if str(wrk_rec$,129%,2%) = "08" then how_txt$ = "DROP SHIP"
            if lcnt% < 60% then goto L62540
               gosub rpt_5_header
               goto L62545
L62540:     if sav_drop$ = or_drop$ then goto L62560
                                                    /*  (EWD017)  Begin */
               if sav_drop$ <> " " then gosub print_drop_total
L62545:        sav_drop$ = or_drop$                 /*  (EWD017) - END  */
                                                    /* (PAR000)         */
               str(save_part$,1%,25%)  = dtl_part$
               str(save_part$,26%,20%) = dtl_sub_part$
               goto L62580
L62560:
           if str(save_part$,1%,25%) = dtl_part$ and                    ~
              str(save_part$,26%,20%)= dtl_sub_part$ then goto L62675

                 str(save_part$,1%,25%)  = dtl_part$
                 str(save_part$,26%,20%) = dtl_sub_part$
                 goto L62635
                                                    /* (PAR000)         */
L62580:     if lcnt% < 11% then goto L62590
               print using L62280 : lcnt% = lcnt% + 1%
                                                           /* (EWD012)  */
L62590:     print using L62250, or_drop$, dtl_cuscode$, cust_name$,       ~
                               dtl_part$, dtl_t$, dtl_so$, dtl_item_no$,  ~
                               dtl_element_no$, dtl_ord_qty$, dt_seq$,    ~
                               str(dtl_desc$,1%,10%), str(or_po$,1%,11%)

               if str(how_txt$,1%,10%) = " " then print                   ~
               else print using L62250, " ", how_txt$       /* (EWD020)  */

               print using L62265, or_route$, sc_pload$,     dt_ref$,     ~
                                  cust_city$, cust_st$, apc_prt$
               lcnt% = lcnt% + 3%
               goto L62700
L62635:     print using L62250, " ", how_txt$, " ",        /* (EWD020) */ ~
                               dtl_part$, dtl_t$, dtl_so$, dtl_item_no$,  ~
                               dtl_element_no$, dtl_ord_qty$, dt_seq$,    ~
                               str(dtl_desc$,1%,10%), str(or_po$,1%,11%)
               print using L62265, or_route$, sc_pload$,     dt_ref$,     ~
                                                       " ", " ", apc_prt$
               lcnt% = lcnt% + 2%
               goto L62700
L62675:     print using L62250, " ", how_txt$, " ", " ",   /* (EWD020) */ ~
                                          dtl_t$, dtl_so$, dtl_item_no$,  ~
                               dtl_element_no$, dtl_ord_qty$, dt_seq$,    ~
                               str(dtl_desc$,1%,10%), str(or_po$,1%,11%)
                                                           /*  (EWD028) */
               print using L62265, " ", " ", " ", " ", " ", apc_prt$
               lcnt% = lcnt% + 2%
                                                          /* (EWD012)   */
L62700:                                                 /* BUILD TOTALS */
            tot_drop% = tot_drop% + 1%
            tot_load% = tot_load% + 1%
            if dt_st% <> 99% then goto L62730
               tot_undef% = tot_undef% + 1%
               goto rpt_5_next

L62730:
/* (SR79626) + */
            if dt_st% = 26% then tot_tran% = tot_tran% + 1%
            if dt_st% = 32% then tot_rga% = tot_rga% + 1%
            if dt_st% = 26% or dt_st% = 32% then goto rpt_5_next
/* (SR79626) - */
            if dt_st% > 13% then tot_stage%  = tot_stage%   + 1%
            if dt_st% > 14% then tot_loaded% = tot_loaded%  + 1%
            if dt_st% > 16% then tot_bol%    = tot_bol%     + 1%
            if dt_st% > 18% then tot_closed% = tot_closed%  + 1%
            if str(dtl_part$,5,4) = "WARR" then tot_warr% = tot_warr% + 1%
            goto rpt_5_next
        rpt_5_done
            gosub print_drop_total                         /*  (EWD017)  */

            convert tot_load% to tot_load$,     pic(########)

            convert tot_warr% to tot_warr$,     pic(########)

            convert tot_stage% to tot_stage$,   pic(########)

            convert tot_loaded% to tot_loaded$, pic(########)

            convert tot_bol% to tot_bol$,       pic(########)

            convert tot_closed% to tot_closed$, pic(########)

            convert tot_undef% to tot_undef$,   pic(########)

            convert tot_tran% to tot_tran$,   pic(########)

            convert tot_rga% to tot_rga$,   pic(########)

            if tot_load% = tot_stage% then                               ~
                           tot_msg1$ = "****** Staging Complete ******"  ~
                      else tot_msg1$ = "** Staging is Not Complete ** "
            if tot_load% = tot_loaded% then                              ~
                           tot_msg2$ = "****** Loading Complete ******"  ~
                      else tot_msg2$ = "** Loading is Not Complete ** "
            if tot_load% = tot_bol% then                                 ~
                           tot_msg3$ = "****** B. O. L. Complete *****"  ~
                      else tot_msg3$ = "** B. O. L. is Not Complete **"
            if tot_load% = tot_closed% then                              ~
                           tot_msg4$ = "****** Invoiced Complete *****"  ~
                      else tot_msg4$ = "** Invoicing is Not Complete *"
            print
            print using L62295, tot_load$,   tot_undef$, tot_tran$, tot_rga$
            print using L62300, tot_stage$,  tot_msg1$
            print using L62305, tot_loaded$, tot_msg2$
            print using L62310, tot_bol$,    tot_msg3$
            print using L62315, tot_closed$, tot_msg4$
            gosub delete_work
        return
            call "SHOSTAT" ("Error - Reading Barcode?") : stop
            goto rpt_5_next

        print_drop_total                        /*  (EWD017)  Begin  */
            print
            print using L62320, tot_drop%, " "
            print
            lcnt% = lcnt% + 3%
            tot_drop% = 0%
        return                                  /*  (EWD017)  - End  */


        set_barcode
          str(apcbar$(count%),1%,8%)  = dtl_so$
          str(apcbar$(count%),9%,1%)  = "-"
          str(apcbar$(count%),10%,2%) = dtl_item_no$
          str(apcbar$(count%),12%,1%) = "-"
          str(apcbar$(count%),13%,4%) = dtl_element_no$
          str(apcbar$(count%),17%,1%) = "-"
          str(apcbar$(count%),18%,4%) = dtl_ord_qty$
          str(apcbar$(count%),22%,4%) = "    "
          str(apcbar$(count%),26%,9%) = dtl_cuscode$
          sn$(count%) = dt_seq$                    /* Save Seq Number */
          sv_pload$(count%) = sc_pload$
        return

        set_barcode_3
             gosub lookup_po
             str(apcbar$(count%),1%,5%)  = dtl_load$
             str(apcbar$(count%),6%,1%)  = " "
             str(apcbar$(count%),7%,2%)  = or_drop$
             str(apcbar$(count%),9%,1%)  = " "
             str(apcbar$(count%),10%,8%) = dt_ref$
             str(apcbar$(count%),18%,2%) = "  "
             str(apcbar$(count%),20%,8%) = dtl_so$
             str(apcbar$(count%),28%,1%) = "-"
             str(apcbar$(count%),29%,2%) = dtl_item_no$
             str(apcbar$(count%),31%,1%) = "-"
             str(apcbar$(count%),32%,4%) = dtl_element_no$
             str(apcbar$(count%),36%,1%) = "-"
             str(apcbar$(count%),37%,4%) = dtl_ord_qty$
             str(apcbar$(count%),44%,4%) = dtl_txt$
             str(apcbar$(count%),48%,9%) = dtl_cuscode$
             sn$(count%) = dt_seq$                 /* Save Seq Number */
             sv_pload$(count%) = sc_pload$
        return

        lookup_cust                           /* Look Up Customer Info */
            if sav_cust$ = dtl_cuscode$ then return
               init(" ") cust_name$, cust_city$, cust_st$, or_route$
               read #6,key = dtl_cuscode$, using L63135, cust_name$,      ~
                                     cust_city$, cust_st$, or_route$,     ~
                                     eod goto lookup_cust_done
L63135:          FMT POS(253), CH(30), POS(403), CH(18), CH(2), POS(980),~
                     CH(5)
        lookup_cust_done
            sav_cust$ = dtl_cuscode$
        return

        deffn'099(textid$)
            txt% = 0%
            if textid$ = hex(00000000) or textid$ = hex(ffffffff)        ~
                                           or textid$ = " " then return
            txt% = 1%
        return

        lookup_text                           /* Look Up Text Id       */
            init(" ") text_desc$, textid$, text_key$, sav_key1$, text$(),~
                      text_d$()
            text_flag$ = "N"
            textid$ = dtl_txt$
            gosub'099(textid$)
            if txt% = 0% then return

            text_key$ = all(hex(00))
            str(text_key$,1%,1%) = "M"
            str(text_key$,2%,3%) = "   "
            str(text_key$,5%,4%) = textid$
            str(text_key$,9%,1%) = "1"
            sav_key1$ = text_key$
            read #9,key > text_key$, eod goto L63325
               get #9, using L63285, text_key$,text$()
L63285:          FMT CH(11), POS(64), 3*CH(70)
            if str(sav_key1$,1%,9%) <> str(text_key$,1%,9%) then         ~
                                                            goto L63325
            if text$(1%) <> " " then text_desc$ = str(text$(1%),1%,70%)  ~
                                else text_desc$ = str(text$(2%),1%,70%)
                                                /* (EWD007) - Text (2) */
            text_d$(1%) = str(text$(1%),1%,70%)
            text_d$(2%) = str(text$(2%),1%,70%)
            text_d$(3%) = str(text$(3%),1%,70%)
            if text_desc$ <> " " then text_flag$ = "Y"
L63325: return

        lookup_po                             /* Look Up P.O. Number   */
            gosub lookup_pgpo                 /* CR2031 */
            if or_key$ = dtl_so$ then return  /* (APCPLNOR) - File     */
               init(" ") or_key$, or_route$, or_due$, or_drop$, or_po$,   ~
                         or_hows$
               or_key$ = dtl_so$
               read #3,key 4% = or_key$, using L63365, or_due$,           ~
                              or_route$, or_drop$, or_po$, or_hows$,      ~
                              eod goto L63370
L63365:           FMT CH(8), XX(2), CH(5), XX(9), CH(2), XX(9), CH(16),   ~
                      POS(92), CH(2)
                                              /* (EWD020) Add Hows to  */
                                              /* read & FMT stmt       */
L63370: return 

        lookup_pgpo  
/* CR2031 */
              init(" ") s_key$, pgpo$
              str(s_key$,1%,9%)   = dtl_cuscode$
              str(s_key$,10%,16%) = dtl_so$
              read #13,key = s_key$, using L63380, pgpo$,             ~
                                                      eod goto L63385
L63380:          FMT POS(920), CH(20)  
L63385: return

        lookup_job
              init(" ") s_key$, job_name$
              str(s_key$,1%,9%)   = dtl_cuscode$
              str(s_key$,10%,16%) = dtl_so$
              read #13,key = s_key$, using L63410, job_name$,             ~
                                                           eod goto L63415
L63410:          FMT POS(619), CH(16)  
L63415: return
        lookup_special                        /* Check Plyer Special   */
            if str(dtl_cuscode$,1%,4%) <> "PL01" then return
               if apc_cust$ <> "PL0100" and apc_cust$ <> "PL0125" and    ~
                  apc_cust$ <> "PL0126" then return
               for i% = 1% to max_mods%
                   if d_mod$ = mods$(i%) then goto L63460
               next i%
        return
L63460:     stock$ = "PLYER"
        return

        lookup_part                           /* Check HNYMASTR        */
               sav_part$ = dtl_part$
               init(" ") part_desc$, upc_code$, apc_prt$, apc_sze$,      ~
/*PAR000*/               width$, height$, apc_scr$, sub_scr$, sub_prt$

               if len(dtl_part$) > 18% then goto mfg_part
                  part_desc$, apc_prt$ = "Component Part"
                  gosub lookup_text
                  if text_flag$ = "Y" then part_desc$,apc_prt$=text_desc$
                  width$, height$ = "N/A"
                  goto mfg_series
        mfg_part
               read #7,key = dtl_part$,using L63540,part_desc$, upc_code$,~
                                    apc_prt$, apc_sze$, eod goto nonstock
L63540:           FMT XX(25), CH(32), POS(566), CH(12), POS(606), CH(60),~
                      CH(20)
               goto mfg_size
        nonstock
REM            call "APCDESCR" (dtl_part$, apc_scr$, apc_prt$, apc_sze$, ~
                                                             #11, err% )
* PAR000 CMG
               call "AWDDESCR" (dtl_part$, dtl_sub_part$, apc_scr$, apc_prt$,~
                                   sub_scr$, sub_prt$, apc_sze$, #11, err% )
               str(part_desc$,1%,16%)  = str(apc_prt$,1%,16%)
               str(part_desc$,17%,16%) = str(apc_sze$,1%,16%)

        mfg_size
            width$  = str(apc_sze$,1%,7%)
            height$ = str(apc_sze$,11%,6%)
        mfg_series
            s_23% = 0%
            s_23m$ = str(dtl_part$,1%,3%)
            s_so$  = dtl_so$                         /* (EWD004) Begin */
            s_ln$  = dtl_item_no$
            init(" ") s_prv$, prv$, s_1$
            prv% = 1%                                /* Use BCKLINES   */
                                                     /* (PAR000) No Chg*/
            call "APCPRZSB" (prv%, s_1$, dtl_cuscode$, s_23m$, s_so$,    ~
                                   s_ln$, s_prv$, s_23$, s_23%,          ~
                                   #6, #5, #fx%, #10, x_er% )

            prv$ = str(s_prv$,1%,15%)                /* Private Label  */

            if x_er% <> 0% then return
               if len(dtl_part$) < 18% then return
                  str(part_desc$,1%,8%) = s_23$      /* Series Code    */
                  str(apc_prt$,1%,8%)   = s_23$

        return                                       /* (EWD004) End   */

        lookup_dept                           /* Look Up Department    */
            if sav_dept$ = dt_dept$ then return
               init(" ") dept$, dept1$, readkey$
               str(readkey$,1%,9%)   = "PLAN DEPT"
               str(readkey$,10%,15%) = dt_dept$
               read #5,key = readkey$,using L63680, dept$, eod goto L63695
L63680:           FMT POS(25), CH(32)
               dept1$ = str(dept$,1%,25%)
               sav_dept$ = dt_dept$
L63695: return

        lookup_color                          /* Look Up COLOR         */
            init(" ") cl$, cl_desc$, readkey$
            readkey$ = "COLOR    " & str(dtl_part$,4%,1%)
            read #5,key = readkey$, using L63680, cl_desc$, eod goto L63730
            cl$ = str(cl_desc$,1%,2%)
L63730: return
                                              /* (EWD016)              */
        lookup_stock                          /* (PAR000) - Change     */
            if sav_stock$ = dtl_new_part$ then return
               sav_stock$ = dtl_new_part$
               init(" ") stock$, readkey$, sp_shape$
               stock_flag$ = "M"
               gosub check_special_shapes_stock
               if len(spec_part$) > 18 then                               ~
                  str(dtl_new_part$,1%,25%) = spec_part$

               str(readkey$,1%,45%) = dtl_new_part$
               read #8,key 1% > readkey$, using L63780, readkey$,         ~
                                               eod goto lookup_stk_done
L63780:        FMT XX(7), CH(52)
               if dtl_new_part$ <> str(readkey$,1%,45%) then               ~
                                               goto lookup_stk_done
                  stock$ = "STOCK" : stock_flag$ = "S"
                  if shapes% = 1% then sp_shape$ = "N" 
                  if fram%   = 1% then sp_shape$ = "Y"   /* Frame      */

        lookup_stk_done
               dtl_part$ = sp_part$
        return                                /* (PAR000)              */
                                              /* (EWD016)              */

                                              /* (PAR000) - No Change  */
        lookup_sku                            /* Lookup Sku Number     */
            init(" ") sku_code$, skuno$       /* Get Sku Code from Cust*/
            init(" ") sku_label$              /* (EWD002) New Labels   */
            read #6,key = dtl_cuscode$, using L63830, sku_code$,          ~
                                               eod goto lookup_sku_done
L63830:        FMT POS(1000), CH(3)
            if len(sku_code$) <> 3 then goto lookup_sku_done
               str(sku_key$,1%,3%)   = sku_code$
               str(sku_key$,4%,25%)  = dtl_part$
               read #12,key 1% = sku_key$, using L63860, skuno$,          ~
                                      sku_label$, eod goto lookup_sku_done
L63860:        FMT XX(3), CH(25), POS(65), CH(2)   /* (EWD002) Label   */
        lookup_sku_done
        return
                                               /* (PAR000)             */
        lookup_status
            if sav_st$ = dt_st$ then return
            sav_st$ = dt_st$
            init(" ") dtl_desc$, readkey$, save_part$
            str(readkey$,1%,9%)   = "PLAN STAT"
            str(readkey$,10%,15%) = dt_st$
            read #5,key = readkey$,using L63680, dtl_desc$, eod goto L63990
            convert dt_st$ to dt_st%, data goto L63990

        return
L63990:     dtl_desc$ = "UNDEFINED      "
            dt_st% = 99%
        return

        lookup_wood
            init(" ") readkey$, desc$
                                                   /* (EWD013)       */
            convert dt_wood$ to dt_wood%, data goto L64000

            if dt_wood% > 1% and dt_wood% < 81% then dt_wood$ = "000"

L64000:     if str(dt_wood$,1%,1%) = "0" then                          ~
                                             str(dt_special$,4%,1%) = "N"

            str(readkey$,1%,9%) = "APC WOOD "      /* (EWD013)       */
            str(readkey$,10%,3%) = dt_wood$
            read #5,key = readkey$,using L64035, desc$, eod goto L64040
L64035:         FMT POS(25), CH(30)
L64040: return
                                                   /* (EWD009) Lowe's */
        lookup_special_lab
            if sku_code$ <> "002" and sku_code$ <> "003" and          ~
               sku_code$ <> "005" then return

            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "PLANLABSP"
            str(readkey$,10%,15%) = upc_code$
            read #5,key = readkey$, using L64035, desc$,                ~
                                            eod goto lookup_special_done
               sku_label$ = str(desc$,1%,2%)
               init(" ") skuno$
               skuno$ = str(desc$,6%,25%)
        lookup_special_done
        return
                                                 /* (EWD009) Lowe's    */

        lookup_100                                   /*  (EWD031)      */
              plan_100%, counter%, label% = 0%
              init(" ") readkey$, desc$
              str(readkey$,1%,9%)   = "PLAN 100 "
REM           str(readkey$,10%,15%) = str(dtl_cuscode$,1%,6%)
			  str(readkey$,10%,15%) = str(dtl_cuscode$,1%,9%) /* CR1828 */

/* CR00548    read #5,hold, key = readkey$, using L64035, desc$,         */
/*                                              eod goto done_100        */
/*CR00548 */  read #5,      key = readkey$, using L64035, desc$,          ~
                                                eod goto done_100

                   convert str(desc$,28%,3%) to counter%, data goto done_100
                   if counter% = 0% then goto done_100
                   plan_100% = 1%
                                         /* 999 means no count, all labels */
                   if counter% = 999% then goto done_100
                   gosub lookup_label
                   if label% = 1% then goto done_100
                      counter% =  counter% - 1%
                      convert counter% to str(desc$,28%,3%), pic(000)

/* CR00548            rewrite #5, using L64035, desc$                    */

        done_100
        return

/* (AWD042) begin */
        lookup_amaa
           if dtl_part$ <> "5S22010006060524540" then goto pww_test
           gold% = 0%
       pww_test

           gold% = 0%                /* cant have gold on painted colors */
           if str(dtl_part$,4%,1%) = "I" then return
           if str(dtl_part$,4%,1%) = "J" then return
           if str(dtl_part$,4%,1%) = "K" then return
           if str(dtl_part$,4%,1%) = "L" then return
           if str(dtl_part$,4%,1%) = "M" then return
           if str(dtl_part$,4%,1%) = "N" then return
           if str(dtl_part$,4%,1%) = "O" then return
           if str(dtl_part$,4%,1%) = "P" then return

/*SR67021 + */
           gosub lookup_lamn
           if gls_lamn% <> 1% then goto not_lamn
              gold% = 1%
           return
       not_lamn
/*SR67021 - */
           gosub check_gold_model
           if gold_model% = 0% then return


           gosub get_decimal
           if dec% = 0% then return
           amaa_width, amaa_height = 0.00
           init(" ") readkey$
           str(readkey$,1,3) = str(dtl_part$,1%,3%)
/*SR67021  str(readkey$,4,3) = "000"      dp group */

/*SR67021  read #24, key = readkey$, using amaa_fmt, amaa_width, amaa_height, ~
                                                   eod goto amaa_done   */
/*SR67021*/
readex:    read #24, key > readkey$, using amaa_fmt, readkey$, amaa_width,    ~
                                     amaa_height, eod goto amaa_done
           if str(readkey$,1%,3%) <> str(dtl_part$,1%,3%) then goto amaa_done
           on schema% goto nfrc_nc, nfrc_tx
       nfrc_nc
           if str(readkey$,4%,3%) <> "000" and str(readkey$,4%,3%) <> "020" and~
              str(readkey$,4%,3%) <> "021" then goto readex
           goto nfrc_check
       nfrc_tx
           if str(readkey$,4%,3%) <> "001" and str(readkey$,4%,3%) <> "002" and~
              str(readkey$,4%,3%) <> "003" then goto readex
nfrc_check
/*amaa_fmt:        FMT POS(7), PD(14,4), PD(14,4)    */
  amaa_fmt:        FMT CH(06), POS(7), PD(14,4), PD(14,4)  /*SR67021 */
/*SR67021        if width > amaa_width or height > amaa_height then return  */
                 if width > amaa_width or height > amaa_height then goto readex
                    gold% = 1%
        amaa_done
        return

        check_gold_model
           gold_model% = 0%
           init(" ") readkey$
           str(readkey$,1%,9%)   = "PLAN GOLD"
           str(readkey$,10%,15%) = str(dtl_part$,1%,3%)
           read #5,key = readkey$, eod goto gold_done

           gold_model% = 1%
        gold_done
        return

        get_decimal
           dec% = 0%
           width, height = 0.00
           dec_w, dec_h  = 0.00
           convert str(dtl_part$,13,3) to width, data goto bad_dec

           convert str(dtl_part$,16,1) to dec_w, data goto bad_dec

           dec_w = round(dec_w / 8,4)
           width = width + dec_w


           convert str(dtl_part$,17,2) to height, data goto bad_dec

           convert str(dtl_part$,19,1) to dec_h, data goto bad_dec

           dec_h = round(dec_h / 8,4)
           height = height + dec_h

              dec% = 1%
        bad_dec
        return
/* (AWD042) - end */
        lookup_label
              label% = 0%
              init(" ") pd_key1$
              str(pd_key1$,1%,8%)     = str(pd_bar$,1%,8%)
REM              str(pd_key1$,19%,3%)    = dt_dept$
REM              str(pd_key1$,22%,2%)    = dt_proc$

              read #18,key 1% > pd_key1$, using L64080,  pd_key1$,         ~
                                                eod goto lookup_label_done
L64080:              FMT POS(278), CH(23)

                      if str(pd_key1$,1%,8%) <> str(pd_bar$,1%,8%)         ~
                                                then goto lookup_label_done
                      label% = 1%

        lookup_label_done
        return

                                                     /*  (EWD031)      */


        print_label
            gosub lookup_dept
            gosub lookup_part
            gosub lookup_stock                   /* (EWD016)           */
            gosub lookup_sku                     /* ALSO PRIVATE_LABEL */
            gosub lookup_special                 /* For PLYER          */
            gosub lookup_job                     /* Get Job Name       */
            gosub lookup_cust                    /* PRIVATE LABEL      */
            gosub lookup_load
            gosub lookup_po
            gosub lookup_text
            gosub lookup_special_lab             /* (EWD009) - Special */
            gosub lookup_100                     /*  (EWD031)          */
            gosub lookup_amaa                    /* (AWD042) */

            gosub lookup_wood
            str(wood_mull$,1%,16%) = or_po$    /* S.O PO NUMBER        */
            str(wood_mull$,17%,6%) = str(desc$,1%,6%) /* W/F CODE      */
            str(wood_mull$,23%,2%) = dt_shft$  /* SHIFT CODE           */
            str(wood_mull$,25%,2%) = dt_proc$  /* PROD PROCESS CODE    */
                                               /* (EWD007)             */
            if str(dtl_load$,1%,1%) = "S" then gosub format_stock

            gosub build_label

            if dt_dept$ = p_sav_dept$ then goto PD_1
               pd_sort% = 0%
               p_sav_dept$ = dt_dept$

PD_1:   pd_sort% = pd_sort% + 1%
        convert pd_sort% to pd_sort$, pic(00000)

        pd_key$ = all(hex(00))
        str(pd_key$,1%,6%)  = dtl_prod$
        str(pd_key$,7%,5%)  = "00000"              /* Primary Sort Code*/
		
/* CR3084 Added dept 102 NC only to print in load number */
        if dt_dept$ = "102" and schema% = 1% ~
		    then str(pd_key$,7%,5%) = str(dt_rec$,1%,5%)   /* load nbr */		
        str(pd_key$,12%,3%) = dt_dept$             /* Production Dept  */
        str(pd_key$,15%,2%) = dt_shft$             /* Production shift */
        str(pd_key$,17%,5%) = pd_sort$             /* Secondary Sort Cd*/
REM        if schema% = 2% then str(pd_key$,17%,5%) = dt_seq$ /* (AWD051) */
        str(pd_key$,17%,5%) = dt_seq$              /* CR3027 */
        str(pd_key$,22%,5%) = dtl_load$            /* Production Load  */
        str(pd_key$,27%,9%) = dt_seq$ & "    "     /* Filler Area      */

        read #18,hold,key = pd_key$, eod goto PD_2
           delete #18
PD_2:   read #18,hold,key 1% = pd_key1$, eod goto PD_2A
           delete #18

PD_2A:                                             /* (PAR001)         */
        str(pd_lab$(),1%,35%)    = pd_key$
        str(pd_lab$(),278%,23%)  = pd_key1$
        put #18, using PD_3, pd_lab$()
PD_3:     FMT 4*CH(256)
                                                   /* (PAR001)        */
        write #18, eod goto ERR_TRAP

                                                   /* (EWD007 - Barcode*/
                                                   /*    label         */
PD_4:                                              /* (EWD004) - Lowes */
        if len(skuno$) < 4 then return             /* Not Lowes        */
           sku_label% = 0%
           convert sku_label$ to sku_label%, data goto PD_X
PD_X
           if sku_label% < 1% then return          /* Not Applicable   */

           init(" ") sp_rec$, sp_key$
           str(sp_rec$,1%,6%)   = dtl_prod$        /* Production Date  */
           str(sp_rec$,7%,2%)   = sku_label$       /* Label code       */
           str(sp_rec$,9%,3%)   = dt_dept$         /* Dept (EWD003)    */
           str(sp_rec$,12%,5%)  = dt_seq$          /* Prod Seq No      */
           str(sp_rec$,17%,25%) = skuno$           /* Sku Number       */
           str(sp_rec$,42%,6%)  = dtl_prod$        /* Production Date  */
           str(sp_rec$,48%,2%)  = sku_label$       /* Label Code       */
           str(sp_rec$,50%,3%)  = dt_dept$         /* Dept (EWD003)    */
           str(sp_rec$,53%,5%)  = dt_seq$          /* Prod Seq No      */
           str(sp_rec$,58%,25%) = dtl_part$        /* MFG Part No.     */
           str(sp_rec$,83%,5%)  = dtl_load$        /* Load No. (EWD003)*/
                                                   /* (EWD019) UPC Code*/
           str(sp_rec$,88%,11%) = str(upc_code$,1%,11%)
                                                   /* UPC Code for Lowe*/
           str(sp_rec$,99%,4%) = " "               /* Filler Area      */
                                                   /* (EWD019)         */
           sp_key$ = str(sp_rec$,1%,41%)           /* Primary Key      */
           read #14,hold,key = sp_key$, eod goto L64050
              delete #14
L64050:    put #14, using L64100, sp_rec$
L64100:      FMT CH(102)
           write #14, eod goto L64125

                                                   /* (EWD002) End     */
L64125: return

ERR_TRAP:                                           /* (EWD007)        */
           errormsg$ = "(Error)Label for(Key) Already Exists??--> " &    ~
                        pd_key$
           gosub error_prompt
        goto PD_4

        display_codes
            table% = 1%
            call "APCPLN1B" (table%, #5 )
        return

        format_stock
                                       /* PD_BAR$   = Bar Code         */
            str(upc_code$,1%,12%)= dt_ref$& "    " /* UPC Bar Code     */
            init(" ") cust_name$                   /* Customer Name    */
            init(" ") dtl_cuscode$                 /* Customer Code    */
                                      /* SCR_DTE3$  = Production Date  */
            init(" ") cust_city$                   /* Ship To City     */
            init(" ") cust_st$                     /* Customer State   */
            init(" ") wood_mull$                   /* P.O.-W/F         */

                                       /* DTL_LOAD$ = Load Number      */
            init(" ") or_drop$                     /* Customer Drop No */
                                       /* DT_REF$   = Warranty Id      */
            init(" ") ld_dtp3$                     /* Load Date        */
            init(" ") stock$                       /* Stock or Blank   */
                                       /* DTL_PART$ = Part Number      */
                                       /* DT_DEPT$  = Department       */
                                       /*  APC_PRT$ = Part Long Desc   */
            init(" ") text_d$()                    /* Text When Applic */
                                       /* WIDTH$    = Width - Long     */
                                       /* HEIGHT$   = Height - Long    */
            init(" ") skuno$                       /* Sku Number       */
            prv$         = "STOCK  STOCK   "       /* NOT APPLICABLE   */
            text_d$(1%)  = "STOCK   STOCK   STOCK   STOCK   STOCK   "
            text_d$(1%)  = text_d$(1%) & "STOCK   STOCK  "

            wood_mull$ = "                N/A -  "
            if str(dtl_part$,9%,2%) <> "99" then return
               wood_mull$ = "                MSP -  "

        return

        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"
            call "WORKOPN2" (#1,mode$, 500%, f2%)
            if f2% <> 0% then goto L64485
        return
L64485:     call "SHOSTAT" ("Error - Cannot Open (APCWORK4)") : stop
        return
        delete_work
            call "FILEBGON" (#1)
        return

      sort_data_col
        sub_flg$ = "5"                     /* (PAR000) - Rpt - 5    */
        mode% = 1% : gosub open_work
        mode% = 3% : gosub open_work
        call "SHOSTAT" ("Sorting Data Collection Load")
        init(" ") dt_key3$, or_key$
        str(dt_key3$,1%,5%) = scr_load$
        read #fx%,key 3% > dt_key3$, using L35050, dt_rec$,             ~
                                                  eod goto sort_data_done
         goto L64570
      sort_data_nxt
        read #fx%, using L35050, dt_rec$, eod goto sort_data_done
L64570:   gosub get_piece
                                                       /*  (EWD029) */
REM IF DTL_LOAD$ <> SCR_LOAD$ THEN GOTO SORT_DATA_DONE
          if dtl_load$ > end_load$ then goto sort_data_done
    
          if dt_dept$ = "095" or dt_dept$ = "102" or                ~
                                    dt_dept$ = "104" then goto L64605
 
          gosub check_support
          if supp% = 1% then goto sort_data_nxt
L64605:                                              /* (EWD023) */
REM Skip Loaded Product if screen selection is 8%
REM IF SELECTION% = 8% AND STR(DT_REC$,64%,2%) > "15" THEN GOTO SORT_DATA_NXT
/* (SR79626) */
          if selection% <> 8% then goto notSelection8
            if str(dt_rec$,64%,2%) > "15" and str(dt_rec$,64%,2%) <> "26" ~
               and str(dt_rec$,64%,2%) <> "32" then goto sort_data_nxt

notSelection8:
             init(" ") wrk_rec$
             gosub lookup_po
             convert or_drop$ to or_drop%, data goto L64610

L64610:
             new_drop% = 99%
             new_drop% = new_drop% - or_drop%

             convert new_drop% to new_drop$, pic(00)
/*(EWD029)*/ str(wrk_rec$,1%,5%)   = dtl_load$     /* Load Number    */
             str(wrk_rec$,6%,2%)   = new_drop$     /* Drop Number    */
             str(wrk_rec$,8%,9%)   = dtl_cuscode$  /* Customer Code  */
             if pgpo$ > " " then str(wrk_rec$,17%,16%) = pgpo$    else ~
                  str(wrk_rec$,17%,16%) = or_po$        /* Customer P.O.  */
             str(wrk_rec$,33%,18%) = pd_bar$       /* Barcode        */
             str(wrk_rec$,51%,3%)  = dt_dept$      /* Department Code*/
             str(wrk_rec$,54%,2%)  = dt_proc$      /* Process Code   */
             gosub lookup_parent
             str(wrk_rec$,56%,5%)  = sc_pload$     /* Parent Load    */
             str(wrk_rec$,61%,8%)  = dt_ref$       /* Warranty Number*/
             str(wrk_rec$,69%,8%)  = dtl_so$       /* S.O. Number    */
             str(wrk_rec$,77%,2%)  = dtl_item_no$  /* Line Item      */
             str(wrk_rec$,79%,4%)  = dtl_element_no$ /* Piece Number */
             str(wrk_rec$,83%,4%)  = dtl_ord_qty$  /* Order Quantity */
             str(wrk_rec$,87%,25%) = dtl_part$     /* Part No.       */
             str(wrk_rec$,112%,2%) = dt_st$        /* Status Code    */
             str(wrk_rec$,114%,4%) = dtl_txt$      /* Line Item Text */
                                                   /* (EWD012)       */
             str(wrk_rec$,118%,6%) = dt_seq$ & " " /* Prod Seq. No.  */
                                                   /* (EWD012)       */
             str(wrk_rec$,124%,5%) = or_route$     /* Route Code     */
             str(wrk_rec$,129%,2%) = or_hows$      /* How Ship (EWD020)*/
REM STR(WRK_REC$,131%,13%)= " "        /* (EWD020)       */
             str(wrk_rec$,131%,2%)= or_drop$       /* (EWD020)       */
                                                   /* (PAR000)       */
             str(wrk_rec$,133%,20%) = dtl_sub_part$

             str(wrk_rec$,153%,103%)= " "          /* (PAR000)       */

            put #1, using L35110, wrk_rec$          /* Length = 144  */
            write #1, eod goto L64740               /* (EWD020)       */
            goto sort_data_nxt
        sort_data_done
        return
L64740:     call "SHOSTAT" ("Error - Updating Sort File ") : stop
            goto sort_data_nxt

        lookup_parent

           init(" ") readkey$, sc_pload$
           str(readkey$,1%,8%) = dtl_so$
           str(readkey$,9%,2%) = dtl_item_no$
           read #4,key = readkey$, using L64780, sc_pload$, eod goto L64785
L64780:       FMT POS(105), CH(5)
L64785: return

        error_prompt
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           comp% = 2%
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        open_error                                    /* (EWD004)        */
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return
                                                      /* (EWD004)        */
        get_work
            init(" ") how_txt$                        /* (EWD020)        */
            wrk_key$        =  str(wrk_rec$,1%,55%)

            scr_load$       =  str(wrk_rec$,1%,5%)   /* Load Number    */
            or_drop$        =  str(wrk_rec$,131%,2%) /* Drop Number    */
            dtl_cuscode$    =  str(wrk_rec$,8%,9%)   /* Customer Code  */
            or_po$          =  str(wrk_rec$,17%,16%) /* Customer P.O.  */
            pd_bar$         =  str(wrk_rec$,33%,18%) /* Barcode        */
            dt_dept$        =  str(wrk_rec$,51%,3%)  /* Department Code*/
            dt_proc$        =  str(wrk_rec$,54%,2%)  /* Process Code   */

            sc_pload$       =  str(wrk_rec$,56%,5%)  /* Parent Load    */
            dt_ref$         =  str(wrk_rec$,61%,8%)  /* Warranty Number*/
            dtl_so$         =  str(wrk_rec$,69%,8%)  /* S.O. Number    */
            dtl_item_no$    =  str(wrk_rec$,77%,2%)  /* Line Item      */
            dtl_element_no$ =  str(wrk_rec$,79%,4%)  /* Piece Number   */
            dtl_ord_qty$    =  str(wrk_rec$,83%,4%)  /* Order Quantity */
            dtl_part$       =  str(wrk_rec$,87%,25%) /* Part No.       */
            dt_st$          =  str(wrk_rec$,112%,2%) /* Status Code    */
            dtl_txt$        =  str(wrk_rec$,114%,4%) /* Status Date    */
                                                     /* (EWD012) Seq No*/
            dt_seq$         =  str(wrk_rec$,118%,5%) /* Prod Seq. No.  */
                                                     /* (1) Blank Char */
                                                     /* (EWD012)       */
            or_route$       =  str(wrk_rec$,124%,5%) /* Route Code     */
                                                     /* (PAR000)       */
            dtl_sub_part$   =  str(wrk_rec$,133%,20%)

        return

                                                /* (EWD007) - Primary  */
                                                /*    routine          */
                                                /* (PAR001)            */
        build_label
            init(" ") pd_lab$(), p_due$, p_make$, p_send$, p_stat$,      ~
                      p_oth$, p_oth1$,p_nominal$, p_part$, nominal$,     ~
                      pd_key$, pd_sort_p$, pd_sort_s$, pd_key1$, screen$,~
                      locks$, pd_fil$, pd_fil1$, p_sub_part$, p_sub_info$,~
                      p_ups$, prod_desc$

                                                /* (PAR001)            */
            p_samp$ = "0"                       /* (EWD011) Samp/Disp  */
            p_foam$ = "N"                       /* (EWD011) Foam Wrap  */
            p_fin$  = "N"                       /* (EWD014) No Fin     */
            gosub check_nominal

            str(pd_lab$(),36%,3%)   = str(dtl_part$,1%,3%)
                                                /* (PAR001)            */
                                                /* Model Code          */
            rr% = len(p_send$) + 1%
            if str(dt_special$,4%,1%) = "Y" then                         ~
               str(p_send$,rr%,5%) = "WOOD/"

            rr% = len(p_send$) + 1%
                                                /* (EWD011)            */
            if str(dt_special$,5%,1%) = "Y" then p_samp$ = "1"
            if str(dt_special$,5%,1%) = "Y" then                         ~
               str(p_send$,rr%,5%) = "SAMP/"
                                                /* Either or not both  */
            rr% = len(p_send$) + 1%
                                                /* (EWD011) Display    */
            if str(dt_special$,6%,1%) = "Y" then p_samp$ = "2"
            if str(dt_special$,6%,1%) = "Y" then                         ~
               str(p_send$,rr%,5%) = "DISP/"

            rr% = len(p_send$) + 1%
            if str(dt_special$,7%,1%) = "Y" then                         ~
               str(p_send$,rr%,4%) = "UPS/"

            rr% = len(p_send$) + 1%
            if str(dtl_part$,1%,1%) = "9"   then                         ~
               str(p_send$,rr%,7%) = "BAY-BOW"
                                                /* (PAR001)            */
            str(pd_lab$(),39%,21%)  = p_send$
                                                /* Wood/Samp/UPS/B/B   */
                                                /* (PAR001)            */
            str(pd_lab$(),60%,70%) = text_d$(1%)/* Prioduction Text    */
            str(pd_lab$(),130%,70%)= text_d$(2%)/* Glass Text          */
            str(pd_lab$(),200%,70%)= " "        /* Available           */
                                                /* (PAR001)            */
            str(pd_lab$(),270%,8%)  = s_23$     /* Series Name         */
                                                /* (PAR001)            */
            str(pd_lab$(),278%,18%) = pd_bar$   /* Production Barcode  */
                                                /* (PAR001)            */
            str(pd_lab$(),296%,3%)  = dt_dept$  /* Production Dept     */
                                                /* (PAR001)            */
            str(pd_lab$(),299%,2%)  = dt_proc$  /* Production Process  */

            str(pd_key1$,1%,18%)    = pd_bar$
            str(pd_key1$,19%,3%)    = dt_dept$
            str(pd_key1$,22%,2%)    = dt_proc$
                                                /* (PAR001)            */
            str(pd_lab$(),301%,10%) = prv$      /* Private Label Name  */
                                                /* (PAR001)            */
            str(pd_lab$(),311%,5%)  = dt_seq$   /* Production Seq No.  */
                                                /* (PAR001)            */
            str(pd_lab$(),316%,19%) = cust_name$/* Customer Name       */
                                                /* (PAR001)            */
            str(pd_lab$(),335%,16%) = or_po$    /* Customer P.O. No.   */

            pq_dept$ = dt_dept$
            gosub assign_quantity
            dt_dept$ = pq_dept$
                                                /* (PAR001)            */
            str(pd_lab$(),351%,3%)  = pq_item$  /* Piece Count for S.O.*/
                                                /* (PAR001)            */
            str(pd_lab$(),354%,3%)  = pq_qty$   /* Total S.O. quantity */

                                                /* (PAR001)            */
            str(pd_lab$(),357%,8%)  = str(pd_bar$,1%,8%)  /* S.O. No.  */
                                                /* (PAR001)            */
            str(pd_lab$(),365%,18%) = cust_city$/* Customer City       */
                                                /* (PAR001)            */
            str(pd_lab$(),383%,2%)  = cust_st$  /* Customer State Code */

            if len(text_d$(3%)) < 2% then                                ~
               text_d$(3%) = "NA/NA"

            pp% = pos(text_d$(3%) = "/")
            if pp% = 1% then goto L64790        /* No Contractor       */
                                                /* (PAR001)            */
            str(pd_lab$(),385%,16%) = str(text_d$(3%),1%,pp% - 1%)
                                                /* Contractor          */
                                                /* (PAR001)            */
L64790:                                         /* (PAR004)            */
            str(pd_lab$(),688%,16%) = job_name$ /* Job Name            */
            str(pd_lab$(),401%,10%) = "          " /* Filler Area      */
                                                /* (PAR001)            */
                                                /* (PAR004)            */
            str(pd_lab$(),704%,16%) = str(text_d$(3%),pp% + 1%,16%)
            str(pd_lab$(),411%,7%)  = "       " /* Filler Area         */
                                                /* Room Location       */
                                                /* (PAR004)            */
                                                /* (PAR001)            */
            str(pd_lab$(),418%,6%)= p_nominal$  /* Nominal size        */

            gosub conv_open
            o_width$  = wd$ & "  "
            o_height$ = ht$ & "  "
                                                /* (PAR001)            */
            str(pd_lab$(),424%,19%)= o_width$ & " X " & o_height$
                                                /* Opening Wd and Ht   */
                                                /* (PAR001)            */
            str(pd_lab$(),443%,19%)= width$ & " X " & height$
                                                /* Exact Wd and Ht     */
                                                /* 1st part of Description */
                                                /* (PAR002)            */
            x% = len(apc_prt$)
            prod_desc$ = str(apc_prt$,1%,x%) & " " & str(sub_prt$,1%,40%)
/*SR68332 + */
            if len(dtl_part$) >= 19 then goto L_SKIP_PART
               init(" ") oradescr_key$
               str(oradescr_key$,1%,8%) = so_inv$
               str(oradescr_key$,9%,3%) = item_no$
               read #58, key key% = oradescr_key$, eod goto L_SKIP_PART

               get #58, using L00100, oradescr_rec$()
L00100:          FMT 4*CH(256)
L00110:          FMT PD(15,4)
               get str(oradescr_rec$(),55%,8%) using L00110, widthes
               get str(oradescr_rec$(),63%,8%) using L00110, heightes
               convert widthes  to widthes$, pic(##0.0###)
               convert heightes to heightes$, pic(##0.0###)
               str(pd_lab$(),443%,19%)= widthes$ & " X " & heightes$
                                                   /* Exact Wd and Ht     */
/*             prod_desc$ = str(oradescr_rec$(),271%, 77%)                */
/*SR70134*/    prod_desc$ = str(oradescr_rec$(),321%, 77%)

        L_SKIP_PART
/*SR68332 - */
            p_oth$ = str(prod_desc$,1%,36%)
                                                /* (PAR002)           */

                                                /* (PAR001)            */
* CMG PAR000
REM         p_oth1$            = str(apc_prt$,37%,24%)  /* (EWD014)    */
                                                /* (NEW Sub Part Number*/
            p_oth1$            = dtl_sub_part$ & "    "
                                           /* New Extended Description */
                                                /* (PAR002)            */
            p_oth2$            = str(prod_desc$,37%,40%)

                                                /* (PAR001)            */

            str(pd_lab$(),462%,36%)= p_oth$     /* 1st 36 Char of Descr*/
                                                /* (PAR001)            */
                                                /* Not Printed on Label*/
            str(pd_lab$(),498%,15%)= str(skuno$,1%,15%) /* Sku Number  */

            p_due$ = or_due$
            call "DATFMTC" (p_due$)             /* (PAR001)            */
            str(pd_lab$(),513%,10%)= p_due$     /* Customer Due Date   */
                                                /* Formatted           */
                                                /* (PAR001)            */
            str(pd_lab$(),523%,25%)= dtl_part$  /* MFG Part Number (25)*/
                                                /* (PAR001)            */
            str(pd_lab$(),548%,2%) = or_drop$   /* Customer Drop No.   */
                                                /* (PAR001)            */
            str(pd_lab$(),550%,5%) = dtl_load$  /* Production Load No. */

            p_make$ = dtl_prod$
            call "DATFMTC" (p_make$)            /* (PAR001)            */
            str(pd_lab$(),555%,10%)= p_make$    /* Production Make Dte */
                                                /* Formatted           */
                                                /* (PAR001)            */
            str(pd_lab$(),565%,10%)= dt_ref$    /* Customer Warranty Id*/
                                                /* (PAR001)            */
            str(pd_lab$(),575%,11%)= str(upc_code$,1%,11%) /* UPC Code */
                                                /* Not Printed On Label*/
                                                /* (PAR001)            */
            str(pd_lab$(),586%,3%) = dt_dept$   /* Prod. Dept Code     */
                                                /* 2nd Occurence (296) */
            p_stat$ = "MAKE "
            if str(dtl_load$,1%,1%) = "S" then p_stat$ = "STOCK"
            if dt_dept$ = "102" or dt_dept$ = "104" then                ~
               p_stat$ = "PULL "                /* (EWD016) Shapes     */
                                                /* (PAR001)            */
            str(pd_lab$(),589%,5%) = p_stat$    /* Make, Pull, Stock   */

                                                /* Wood Surround Code  */
                                                /* (PAR001)            */
            str(pd_lab$(),594%,6%) = str(wood_mull$,17%,6%)
                                                /* (EWD011)            */
            if len(dtl_part$) < 19 then goto L_SKIP

               ss% = 0% : s1% = 0%              /* (PAR000)   Foam     */
               screen$ = str(dtl_part$,11%,1%)
        REM       ss% = pos("ABCD" = screen$)
               if str(dtl_sub_part$,5%,1%) = "1" then ss% = 1%
               if str(dtl_sub_part$,5%,1%) = "4" then ss% = 1%
/*             s1% = pos("456" = screen$)          Sash                */
/*AWD055*/     s1% = pos("4567" = screen$)      /* Sash                */
               if ss% <> 0% then p_foam$ = "Y"
                                                /* (EWD014)            */
                                                /* (EWD024)   BEG      */
/* CR1515 approved for removal by NC and TX */
REM              if or_hows$ <> "20" and or_hows$ <> "23" then goto not_repair
REM                 if p_foam$ = "N" then p_foam$ = "0"
REM                 if p_foam$ = "Y" then p_foam$ = "1"

REM   not_repair:
                                                /* (EWD024)   END      */
               ss% = 0%
               locks$  = str(dtl_part$,12%,1%)  /* (PAR000)   Fins     */
               ss% = pos("125HJ" = locks$)      /* No Fin Codes        */
               p_fin$ = "Y"
               if ss% <> 0% then p_fin$ = "N"
               if s1% <> 0% then p_fin$ = "N"

L_SKIP:     str(pd_lab$(),600%,1%) = p_samp$    /* (PAR001)            */
            str(pd_lab$(),601%,1%) = p_foam$    /* (PAR001)            */
            str(pd_lab$(),602%,1%) = p_fin$     /* (PAR001)            */
                                                /* (EWD014)            */
                                                /* (PAR001)            */
            str(pd_lab$(),603%,24%)= p_oth1$    /* New Sub Part No.    */
                                                /* (PAR001)            */
            str(pd_lab$(),627%,1%) = sp_shape$  /* (EWD016) for Frames */
                                                /* Frame Shape (Y)or(N)*/

                                                /*  (EWD031)           */
                                                /* (PAR001)            */
REM            if plan_100% <> 0% then                                     ~
            str(pd_lab$(),628%,6%) = str(dtl_cuscode$,1%,6%)            ~
            else                                                        ~
            str(pd_lab$(),628%,6%) = " "

            str(pd_lab$(),628%,6%) = "      "                  
            str(pd_lab$(),727%,9%) = str(dtl_cuscode$,1%,9%) /* CR1828 */
                                                /* (PAR001)            */
                                                /*  (EWD031)           */


                                                /* (EWD022) 100% Insp  */

            gosub lookup_config                 /*  (EWD024)           */

/* (AWD043) */
            if str(dtl_sub_part$,6,1) = "3" then p_specialmull$ = "M"

                                                /* (PAR001)            */
                                                /* W.W. Config Line No.*/
            str(pd_lab$(),634%,2%) = str(config_lne$,2%,2%)
                                                /* (PAR003)            */
                                                /* Special Mull code   */
                                                /* for Shapes Cross Dock*/
            str(pd_lab$(),687%,1%) = p_specialmull$
                                                /* (PAR003)            */
            gosub lookup_ups                    /* (AWD036)            */
            p_ups$ = "0"                        /* (PAR001)            */
            if ups% = 1% then p_ups$ = "1"
            str(pd_lab$(),636%,1%) = p_ups$
                                                /* (PAR001)            */

                                                /* (PAR001)            */
            str(pd_lab$(),637%,10%) = dtl_sub_info$
                                                /* (PAR001)            */
                                                /* New Sub Part Info   */
            str(pd_lab$(),647%,40%) = p_oth2$

                                                /* (PAR001)            */
                                                /* (PAR003)            */
                                                /* (PAR004)            */
REM         str(pd_lab$(),720%,223%) = " "      /* Filler Area 1       */
            str(pd_lab$(),720%,7%)   = " "      /* Filler Area 1       */
            str(pd_lab$(),736%,207%) = " "      /* Filler Area 1       */
                                                /* (PAR004)            */

/* (AWD042) Amaa Gold Labels */
            if gold% = 1% then str(pd_lab$(),720%,1%) = "G"
/* (AWD049)*/
REM            GOSUB CHECK_8900_MDL     /* (AWD054) comment out check */
            if mdl8900% = 1% and locks$ = "3" then str(pd_lab$(),721%,1%) = "F"
            if mdl8900% = 1% and locks$ = "4" then str(pd_lab$(),721%,1%) = "F"
            if mdl8900% = 1% and locks$ = "6" then str(pd_lab$(),721%,1%) = "F"
            if mdl8900% = 1% and locks$ = "M" then str(pd_lab$(),721%,1%) = "F"
            if mdl8900% = 1% and locks$ = "O" then str(pd_lab$(),721%,1%) = "F"
/*(\AWD049) */

/*SR75604 + */ 
            gosub lookup_intercept
            gosub lookup_triplepane
            if tripane% <> 0% and intercept% <> 4% then intercept% = 4%
            if tripane% <> 0% and intercept$ <> "04" then intercept$ = "04"
            str(pd_lab$(),722%,2%) = intercept$
            str(pd_lab$(),724%,3%) = ld_shp_blk$  /* CR1174 */

/*SR75604 - */
                                                /* (PAR001)            */
            str(pd_lab$(),943%,82%)  = " "      /* Filler Area 2       */

                                                /* (EWD011) - End Chg  */
        return                                  /* (EWD007) - End of   */
                                                /*    primary Routine  */

        lookup_ups                              /* (AWD036)            */
           ups% = 0%
           str(readkey$,1%,9%)   = "PLAN UPS "
           str(readkey$,10%,15%) = or_hows$        /* How Ship */
           read #5,key = readkey$, eod goto ups_done
                 ups% = 1%
        ups_done
        return                                   /* (AWD036)            */

        lookup_config                           /*  (EWD024)    BEG    */
            init(" ") bcklnes_key$, p_specialmull$
            line% = 0%
            config_lne% = 0%
            str(bcklnes_key$,1%,16%) = str(pd_bar$,1%,8%)
            convert str(pd_bar$,9%,2%) to line%, data goto no_config

            convert line% to str(bcklnes_key$,17%,3%), pic(###)

                                                /* (PAR003)             */
            read #10, key = bcklnes_key$, using L64600, config_lne$,        ~
                                           p_specialmull$, eod goto no_config

L64600:                FMT POS(284), CH(3), POS(289), CH(1)
                                                /* (PAR003)             */
                 convert config_lne$ to config_lne%, data goto no_con_lne

                 convert config_lne% to config_lne$, pic(##0)
no_con_lne:

                 if str(config_lne$,2%,2%) = " " then                       ~
                         str(config_lne$,2%,2%) = str(bcklnes_key$,18%,2%)

        no_config
        return                                  /*  (EWD024)    END    */

        conv_open /* Convert Standard Width/Height to Fraction in 8'ths*/
                  /* F0% = Width Fraction, F1% = Height Fraction       */
                  /* WD$ = Width & Fraction, HT$ = Height & Fraction   */
           sze$ = "1/81/43/81/25/83/47/8         "

           init(" ") wd$, ht$
           if lit_flg$ = "N" then goto L64795            /* (EWD013)   */
              wd$ = str(p_part$,13%,4%)
              ht$ = str(p_part$,17%,3%)
              return
                                                         /* (EWD013)   */
L64795:    str(wd$,1%,3%) = str(p_part$,13%,3%)          /* Width  (3) */
           if str(wd$,1%,1%) = "0" then str(wd$,1%,1%) = " "
           str(ht$,1%,2%) = str(p_part$,17%,2%)          /* Height (2) */
           f0% = 0% : f1% = 0%                      /* Build Fractions */
           convert str(p_part$,16%,1%) to f0%,data goto L64800 /*Width */

           convert str(p_part$,19%,1%) to f1%,data goto L64800 /*Height*/

           goto L64810
L64800:      f0% = 8% : f1% = 8%
L64810:    if f0% = 0% then f0% = 9%
           if f1% = 0% then f1% = 9%
           str(wd$,4%,1%) = " "          /* Build Width with Fraction  */
           str(wd$,5%,3%) = str(sze$,(f0%*3%) - 2%, 3%)
           str(ht$,3%,1%) = " "          /* Build Height with Fraction */
           str(ht$,4%,3%) = str(sze$,(f1%*3%) - 2%, 3%)
        return


        check_nominal
            init(" ") nominal$, readkey$, p_nominal$
            p_err% = 0%
            lit_flg$ = "N"                               /* (EWD013)   */
            if str(dtl_part$,1%,3%) = "003" then lit_flg$ = "Y"
            if lit_flg$ = "N" then goto L64815
               p_part$     = dtl_part$
               p_sub_part$ = dtl_sub_part$               /* (PAR000)   */
               p_sub_info$ = dtl_sub_info$               /* (PAR001)   */
               goto L64820
                                                         /* (EWD013)   */
L64815:     call "EWDNOMSZ" ("E", dtl_part$, p_part$, nominal$, dtl_cuscode$,~
                                                      #19, #5, p_err%)

            str(p_nominal$,1%,3%) = str(nominal$,1%,3%)
            str(p_nominal$,4%,3%) = str(nominal$,5%,3%)
            call "SPCESMSH" (p_nominal$, 0%)

            str(readkey$,1%,9%)   = "PLAN NEWC"
            str(readkey$,10%,15%) = str(dtl_part$,1%,3%)
            read #5,key = readkey$, eod goto L64820
        return
L64820:     init(" ") nominal$, p_nominal$
        return

        assign_quantity
            init(" ") pq_key$, pq_item$, pq_qty$
            pq_key$ = pd_bar$
            read #20,key = pq_key$, using L64830, pq_item$, pq_qty$,     ~
                                                 eod goto create_quantity
L64830:        FMT POS(19), 2*CH(3)
            return
        create_quantity
            init(" ") pq_key1$, pq_sav$
            qq% = 0% : pass% = 0%
            pq_sav$ = str(pd_bar$,1%,8%)
            str(pq_key1$,1%,8%) = pq_sav$
        create_quantity_nxt
        fy% = 21%
        if fx% = 27% then fy% = 28%
            read #fy%,key > pq_key1$, using L64840, pq_key1$,             ~
                                            eod goto create_quantity_done
L64840:        FMT POS(24), CH(23)
            if str(pq_key1$,1%,8%) <> pq_sav$ then                       ~
                                            goto create_quantity_done
               dt_dept$ = str(pq_key1$,19%,3%)
               gosub check_support                  /* Pull from stock */
               if dt_dept$ = "102" then supp% = 0%  /* should all count*/
               if dt_dept$ = "104" then supp% = 0%  /* in total quantity*/

               if supp% = 1% then goto create_quantity_nxt
               qq% = qq% + 1%
               if pass% = 0% then goto create_quantity_nxt
                  convert qq% to pq_item$, pic(000)
                  read #20,hold,key = str(pq_key1$,1%,18%),eod goto L64845
                     delete #20

L64845:           write #20, using L64850, str(pq_key1$,1%,18%), pq_item$,~
                                           pq_qty$, " ", eod goto L64860
L64850:               FMT CH(18), CH(3), CH(3), CH(8)
               goto create_quantity_nxt
         create_quantity_done
               if pass% = 1% then goto assign_quantity
               convert qq% to pq_qty$, pic(000)
               pass% = 1%
               qq% = 0%
               init(" ") pq_key1$
               str(pq_key1$,1%,8%) = pq_sav$
               goto create_quantity_nxt

        return

L64860: errormsg$ = "(Error) Building Qty for ( "&pd_bar$&" )"
        gosub error_prompt
        goto create_quantity_nxt

                                      /* (EWD007) - end of build_label */
                                                 /* (EWD016)          */
        check_special_shapes_stock
            sp_part$ = dtl_part$
            call "EWDSHAPE" (shapes%, fram%, sp_part$, spec_part$, #5)
            dtl_part$ = spec_part$
        return

                                                 /* (EWD016)           */

                                               /* (EWD030)   BEGIN    */
        find_glass
           glass% = 0%
           init(" ") rmk_key$
           str(rmk_key$,1%,8%) = warranty$

           gosub check_clear
           if clear% <> 1% then gosub check_temp
           if clear% <> 1% and temp% <> 1% then goto no_glass
              glass% = 1%
        return
        no_glass
        return

        check_clear
        clear% = 0%
           read #16,key > rmk_key$, using L61690, rmk_key$, spacer$,  ~
                                                         eod goto no_clear
L61690:           FMT POS(22), CH(12), POS(171), CH(4)

           if str(warranty$,1%,8%) <> str(rmk_key$,1%,8%) then goto no_clear
REM           rm_key$ = str(rm_rec$,22%,12%)

        clear% = 1%
        no_clear
        return

        check_temp
           init(" ") rmk_key$
           str(rmk_key$,1%,8%) = warranty$
        temp% = 0%
           read #22,key > rmk_key$, using L61690, rmk_key$, spacer$,  ~
                                                 eod goto no_temp

           if str(warranty$,1%,8%) <> str(rmk_key$,1%,8%) then goto no_temp

        temp% = 1%
        no_temp
        return
                                               /* (EWD030)    END    */

                                               /* (AWD037)           */
        Run_Program:
           return% = 0% : comp% = 0%
           init(" ") rlib$, rvol$
           call "PROCLINK" (run$, rlib$, rvol$, return%, comp%)
        return
                                               /* (AWD037)           */
                                               /* (PAR000)           */
        lookup_sub_part
                                               /* (RHHTEST)          */
        REM    if sav_so$ = str(pd_bar$,1%,10%) then return
        REM       sav_so$ = str(pd_bar$,1%,10%)
            init(" ") bcksubpt_rec$, flds$(), info_flds$(), dtl_sub_part$,~
                      dtl_sub_info$
            flag$ = "0"                  /* Sales Order Info         */
            pgm$  = "1"
            err1% = 0%

            convert so_inv$ to so_inv%, data goto convert_alpha

            convert so_inv% to so_inv$, pic(00000000)

            goto order_converted

convert_alpha:
            convert str(so_inv$,2%,7%) to so_inv%, data goto sub_part1
sub_part1:
            convert so_inv% to str(so_inv$,2%,7%), pic(0000000)

order_converted:
            convert item_no$ to item_no%, data goto sub_part2
sub_part2:
            convert item_no% to item_no$, pic(###)

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
               str(bcksubpt_rec$,48%,20%) = "00000               "

           dtl_sub_part$ = str(bcksubpt_rec$,48%,20%)

           dtl_sub_info$ = str(bcksubpt_rec$,132%,9%) & " "

REM           call "SHOSTAT" ("Sub Part = " & dtl_sub_part$)
REM            stop

            if err1% = 0% then return

            return

            errormsg$ = "Load Error S.O.= "&so_inv$ & " Line= " &         ~
                         item_no$ & " Flag= " & sub_flg$
            gosub error_prompt
            err1% = 0%
        return
                                                         /* (PAR000)    */

                                                 /* (AWD041) */
        con_fract                            /* Convert to Sixteenth's */
              calc = round( calc, 4 ) : calc$ = " "
              a% = int(calc) : b% = int((calc - a%) * 10000)
              if b% = 0% then goto L61800                /****************/
                 d% = int(b%/625)                      /* Conversion of*/
                 if mod(b%,625) <> 0 then d% = d% + 1% /* Decimals to  */
                 b% = d%                               /*  Sixteenth's */
                 if b% <> 16% then goto L61800           /****************/
                    a% = a% + 1% : b% = 0%          /* A% = Whole Part */
L61800:       convert a% to str(calc$,1%,3%), pic(###)
              if b% <> 0% then                                           ~
                              str(calc$,5%,5%) = str(sz$,(b%*5%) - 4%,5%)
        return

        exit_program
            call "SHOSTAT" ("One Moment Please")
        end



/* (AWD049)*/

        load_8900_mdls
           init(" ") readkey$, desc$, savekey$, mdlNum$
           pos%=1%
           str(readkey$,1%,9%) = "ELLISON04"
           str(readkey$,10%,4%) = "8900"
           savekey$ = readkey$
        mdls_8900_next
           read #5, key > readkey$, using L03040, readkey$, desc$,   ~
                                               eod goto mdls_8900_done
L03040:           FMT CH(24), CH(30)

              if str(readkey$,1%,14%) <> str(savekey$,1%,14%)        ~
                                              then goto mdls_8900_done

              str(mdlNum$,pos%,4%) = str(desc$,1%,3%) & ","
              pos% = pos% + 4%
              goto mdls_8900_next
        mdls_8900_done
        return

/* (AWD053) */
        load_65_mdls
           init(" ") readkey$, desc$, savekey$
REM           , MDLNUM$
REM           POS%=1%
           str(readkey$,1%,9%) = "ELLISON04"
           str(readkey$,10%,2%) = "65"
           savekey$ = readkey$
        mdls_65_next
           read #5, key > readkey$, using L03040, readkey$, desc$,   ~
                                               eod goto mdls_65_done

              if str(readkey$,1%,12%) <> str(savekey$,1%,12%)        ~
                                              then goto mdls_65_done

              str(mdlNum$,pos%,4%) = str(desc$,1%,3%) & ","
              pos% = pos% + 4%
              goto mdls_65_next
        mdls_65_done
        return
/* (\AWD053) */

        check_8900_mdl
          mdl8900% = 0%
          mdlNum% = 0%
          search mdlNum$ = str(dtl_part$,1%,3%) to mdlNum()

          mdlNum% = int(mdlNum())
          if mdlNum% <> 0% then mdl8900% = 1%
        return


/*(\AWD049)*/

/*SR67021 + */
        lookup_lamn
          gls_lamn% = 0%
          str(readkey$,1%,9%)   = "PLAN LAMN"
          str(readkey$,10%,15%) = str(dtl_part$,5%,2%)
          read #5,key = readkey$, eod goto notLamn
             gls_lamn% = 1%
        notLamn
        return
   
/*SR67021 - */
/*SR75267 + */
        lookup_mdl8900
           mdl8900% = 0%
           str(readkey$,1%,9%)   = "APPLNAIL "
           str(readkey$,10%,15%) = str(series$,1%,8%)   /* CR2172 from 5 to 8 */
           call "UPPERCASE" (readkey$, err%)            /* CR2172 */
           read #5,key = readkey$, eod goto lookup_mdl8900_done
                 mdl8900% = 1%
        lookup_mdl8900_done
        return
/*SR75267 - */
/*SR75604 + */
        lookup_triplepane
         tripane% = 0%
         init(" ") readkey$
         str(readkey$,1%,9%)   = "PLANTRIPL"
         str(readkey$,10%,15%) = str(dtl_part$,5%,2%)      /* Glass        */
         read #3,key = readkey$, eod goto notriplepane
              tripane% = 1%
        notriplepane
        return

        lookup_intercept
/* (AWD117) */
          if str(p_oth1$,17%,1%) <> "0" then goto subpart_intercept
          init(" ") readkey$, desc$, intercept$
          intercept% = 1%
          str(readkey$,1,9)  = "INTERCEPT"
          str(readkey$,10,3) = str(dtl_part$,1%,3%)        /* Model        */
          str(readkey$,13,2) = str(dtl_part$,5%,2%)        /* Glass        */

           read #5, key = readkey$, using L61131, desc$,  ~
                                   eod goto no_intercept_glass

               convert str(desc$,1,2) to intercept%, data goto intercept_done
           goto intercept_done

        no_intercept_glass
          str(readkey$,1,9)  = "INTERCEPT"
          str(readkey$,10,3) = str(dtl_part$,1%,3%)        /* Model        */
          str(readkey$,13,2) = str(dtl_part$,5%,1%) & "*"  /* Glass        */

           read #5, key = readkey$, using L61131, desc$,  ~
                                   eod goto no_intercept_all

               convert str(desc$,1,2) to intercept%, data goto intercept_done
           goto intercept_done

         no_intercept_all
           init(" ") readkey$
           str(readkey$,1,9)  = "INTERCEPT"
           str(readkey$,10,3) = str(dtl_part$,1%,3%)        /* Model        */
           str(readkey$,13,2) = "**"

           read #5, key = readkey$, using L61131, desc$,  ~
                                   eod goto intercept_done

               convert str(desc$,1,2) to intercept%, data goto intercept_done

        intercept_done
          convert intercept% to intercept$,pic(00)
        return
L61131:        FMT POS(25), CH(30)

/* (AWD117) */
        subpart_intercept
          convert str(p_oth1$,17%,1%) to intercept%, data goto badsubpart_intercept
          goto intercept_done
        badsubpart_intercept
         str(p_oth1$,17%,1%) = "0"
         goto lookup_intercept
/*SR75604 - */

