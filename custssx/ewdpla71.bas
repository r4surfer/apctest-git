        REM *************************************************************~
            *            (RHHTEST)    Test Print Turned (off)            *~
            *                                                           *~
            *      Note- Bay-Bow Test for Line = 25%  in yy$(??)        *~
            *      Note- UPC Test for Line     = 86% (Turned Off)       *~
            *                                        (AWD028)           *~
            *      Note- 100% Inspection test for Line = 27% in yy$(??) *~
            *                                 and Line = 102% (Text)    *~
            *  Subroutine Name   - EWDPLA71                             *~
            *  Creation Date     - 04/08/99                             *~
            *  Last Modified Date- 04/30/2012                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Last Mod's By     - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Write records to a file to print a   *~
            *                      production label.                    *~
            *                                                           *~
            *                      Print File  = MFGPROD                *~
            *                      Script File = MFGPROD                *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDPLA71 - Generates the label format and data to print   *~
            *            production labels. The resulting file is routed*~
            *            to the label printer via a script.             *~
            *                                                           *~
            *          - ERROR% = 0% - Lbl Ok and Sent                  *~
            *                     1% - Could Not Open Lbl Print File    *~
            *                     4% - Shell Script Error               *~
            *                     5% - Print Error Creating Label       *~
            *                     6% - Could Not Reset Table Flag       *~
            *                     7% - Label Data Format Error          *~
            *                     8% - No Data for Label                *~
            *                     9% - Schema Lookup Error              *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/28/99 ! Original - Copied & Mod (sub) EWDPLA65.  ! BWS *~
            * 06/06/99 ! (EWD001) - Revised Label                 ! RHH *~
            * 07/16/99 ! (EWD002) - Max of Five at Same Time      ! RHH *~
            * 07/19/99 ! (EWD003) - Fix Barcode Prob with Stock   ! RHH *~
            * 07/21/99 ! (EWD004) - Mod Add W/F Code to Label     ! RHH *~
            * 07/22/99 ! (EWD005) - Mod For Wood Surround 'W'     ! RHH *~
            * 09/09/99 ! (EWD006) - Mod to High Lit the Exact Size! RHH *~
            * 09/22/99 ! (EWD007) - Mod to High Lit Bay Bow       ! RHH *~
            * 11/19/99 ! (EWD008) - Mod to Put 'SAMPLE' on Label  ! RHH *~
            *          !            and 'FOAM' for Screen Codes   !     *~
            *          !            'A', 'B', 'C' and 'D'         !     *~
            * 02/10/00 ! Correct  - Sample Problem with 'W'       ! RHH *~
            * 02/23/00 ! (EWD009) - Print two labels for a sample ! RHH *~
            *          !            code '011'                    !     *~
            * 02/24/00 ! (EWD010) - Increase the Length of the    ! RHH *~
            *          !            product Description to 60     !     *~
            * 02/24/00 !          - Mod to put 'NOFIN' on label   !     *~
            *          !            lb_fin$                       !     *~
            * 04/03/00 ! Label Chg  Highlight the P.O. Number     ! RHH *~
            * 04/11/00 ! (EWD011) - Major Mod to switch to a      ! RHH *~
            *          !            Single File.                  !     *~
            * 04/24/00 ! (EWD012) - Mod to Change Part range for  ! RHH *~
            *          !            Samples. New Code '027'       !     *~
            *          !            New Range (012 thru 026)      !     *~
            * 06/20/01 ! (EWD013) - Mod to 'INS OAK' or 'INS BIRCH'!CMG *~
            *          !            on lable if applicable        !     *~
            * 07/10/01 ! (EWD014) - Mod to label to put 'FLORIDA' ! RHH *~
            *          !            for special shape pulls to    !     *~
            *          !            distinquish from product      !     *~
            * 08/14/01 ! (EWD015) - Mod to Change Part range for  ! CMG *~
            *          !            Samples. New Code '035'       !     *~
            *          !            New Range (012 thru 026)      !     *~
            * 08/30/02 ! (EWD016) - Mod to add new field to label ! RHH *~
            *          !            showing the Sales Order Line  !     *~
            *          !            item number.                  !     *~
            * 09/17/02 ! (EWD017) Fix for Special Shapes Grid Code! CMG *~
            * 06/04/03 ! (EWD018) Dispay 'P' for all Parts and set! RHH *~
            *          !          '*100%*'for customers with 100  !     *~
            *          !          Percent Inspection. Also replace!     *~
            *          !          Drop code with Customer Code    !     *~
            * 06/09/03 ! (EWD019) Put Drop Code back on Label     ! RHH *~
            * 06/11/03 ! (EWD020) Improve Print speed             ! RHH *~
            * 06/16/03 ! (EWD021) Put Last three digits of SO and ! RHH *~
            *          !          and Wizard config no.           !     *~
            * 06/17/03 ! (EWD022) Put Wood Surround Seq. No. on   ! RHH *~
            *          !          production label. (WXXXXX)      !     *~
            * 06/26/03 ! (EWD023) Fix Sample Part Problem         ! RHH *~
            * 06/30/03 ! (EWD024) Mod to put Day of the week      ! RHH *~
            * 02/02/04 ! (AWD025) Mod for Continusous Head and    ! RHH *~
            *          !          and Screens. Put an 'A' instead !     *~
            *          !          of 'W' for Mull Codes 'A01',    !     *~
            *          !          'A16', 'A17'. Two new Boxes for !     *~
            *          !          Screens.  'check_screens'       !     *~
            * 05/21/05 ! (AWD026) Mod for Mull codes to put a "D" ! RHH *~
            *          !          special mull codes.             !     *~
            *          !          New table 'PROD MULL'           !     *~
            * 06/29/04 ! (AWD027) Two Changes. 1. Change the Prod ! RHH *~
            *          !          Seq. number to print white on   !     *~
            *          !          black background. 2. Put a New  !     *~
            *          !          to flag Specials with 'SP'      !     *~
            *          !          subroutine 'check_specials'     !     *~
            *          !          use the same logic as in        !     *~
            *          !          'apcpl41a'.                     !     *~
            * 06/30/04 ! (AWD028) Replace UPC Code with Production! RHH *~
            *          !          Sequence Number.                !     *~
            * 08/11/04 ! (AWD029) Mod to update Specials check for! RHH *~
            *          !          glass and liting.               !     *~
            * 05/06/05 ! (AWD030) Mode to 'check_screen' for the  ! RHH *~
            *          !          that are vague and say with     !     *~
            *          !          screen.                         !     *~
            * 06/28/05 ! (AWD031) Change madeto put UPS on the    ! RHH *~
            *          !          Production Label                !     *~
            * 09/29/05 ! (AWD032) Mod to Production label to put  ! RHH *~
            *          !          a large 'B' for Bay Bow Windows !     *~
            * 01/01/05 ! (PAR000) CR347 Mods for New Sub Part No. ! RHH *~
            * 02/07/06 ! (PAR001) Mod to lb_foam$ logic.          ! RHH *~
            * 03/03/06 ! (PAR002) Mod to Production label for the ! RHH *~
            *          !          Sub Part Number and related     !     *~
            *          !          Description.                    !     *~
            * 03/20/06 ! (PAR003) Mod for print labels at North   ! RHH *~
            *          !          East                            !     *~
            * 04/10/06 ! (PAR004) Mod to Fix old label code for   ! RHH *~
            *          !          Specials                        !     *~
            * 04/12/06 ! (PAR005) Mod to move the 100% Inspection ! RHH *~
            *          !          to the right about 1/4 inch. If !     *~
            *          !          move too far will conflict with !     *~
            *          !          the printing of 'SAMPLE'        !     *~
            * 07/27/06 ! (PAR006) Mod to put 'C' on label for     ! RHH *~
            *          !          Special Shape Mull Code         !     *~
            *10/17/2007! (AWD007) mod for 100% on bay/bow         ! CMG *~
            *11/30/2007! (AWD008) mod for < 100% on bay/bow       ! DES *~
            * 03/05/08 ! (AWD009) mod for SDL                     ! CMG *~
            *01/05/2009! (AWD010) Add screen codes A, B, C & J    ! DES *~
            *01/12/2009! (AWD011) Add "STK" for Lowe's stock      ! DES *~
            *02/24/2009! (AWD012) Add 'G' for Gold Amaa Label     ! CMG *~
            *02/25/2009! (AWD013) Mod for bay bow seat            ! CMG *~
            *05/12/2009! (AWD014) mod to remove STK for 2910 Tax  ! CMG *~
            *          !           Credit Windows                 !     *~
            *05/15/2009! (AWD015) Add mull clip flag for Florida  ! DES *~
            *07/21/2009! (AWD016) mod for M mull clip             ! CMG *~
            *09/14/2009! (AWD017) Lowes stock changes             ! DES *~
            *09/29/2009! (AWD018) Add "BLINDS"                    ! DES *~
            *01/08/2009! (AWD019) mod for bay/bow inverse         ! CMG *~
            *03/22/2010! (AWD020) Add "E" if subpart5 = "2" @ bot ! DES *~
            *04/05/2010! (AWD021) Add "E" if subpart5 = "2" by load DES *~
            *04/30/2012! (AWD022) mod for 8900 Applied Fin Option ! CMG *~
            *10/16/2012! (AWD023) mod for Casing/Applied Fin to   ! CMG *~
            *          !   override Wood Surround                 !     *~
            *06/06/2013! (AWD024) mod for NTX fin code            ! CMG *~
            *02/24/2014! (AWD025) mod for Plan 100 to prevent un- ! PWW *~
            *          !   wanted inspections for Model "N". Added!     *~
            *          !   an "M" to pos(30) of Desc field in     !     *~
            *          !   GENCODES for all Model tests.          !     *~
            *03/25/2014! (AWD026) mod for cust_code TH9050 to show! MES *~
            *          !   Thermal instead of from ELLISON01      !     *~
            *05/12/2014! (AWD027) mod for Fin/Flange Combo        ! CMG *~
            *07/18/2014! (AWD033) mod for 7/8" SDL text.          ! PWW *~
            *07/18/2014! (AWD034) mod for Plan 100 to prevent un- ! PWW *~
            *          !   wanted inspections for Color "9". Used !     *~
            *          !   mod (AWD025) to fix. Check for "M" in  !     *~
            *          !   pos(30) of GENCODES desc field.        !     *~
            *10/15/2014! (AWD035) mod for Dept #043 to get FIN.   ! PWW *~
            *12/16/2014! (AWD036) Created a new template with     !     *~
            *          !   BarTender. Template name is I\ocumentation\  *~
            *          !   Developers\BarTenderTemplates\Prod Label\    *~
            *          !   Production Labelz2.btw.                      *~
            *02/20/2015! (AWD037) Remove City, State & nominal sz ! PWW *~
            *          !   and move items up to make room for new !     *~
            *          !   Prod Desc which will pull GENCODES.    !     *~
            *          !   Also get series from bcksubpt.         !     *~
            *03/16/2015! (AWD038) Added Ccreen Code.              ! PWW *~
            *04/02/2015! (IM7982) Make bold Product Name.         ! PWW *~
            *06/01/2015! (SR65493) Add Warranty ALI info.         ! PWW *~
            *06/30/2015! (SR66611) Make Load BOLD.                ! PWW *~
            *07/14/2015! (SR66919) Make "SAMPLE" Bold & bigger    ! PWW *~
            *08/10/2015! (SR67685) Make Prod Name Bold & bigger   ! PWW *~
            *09/02/2015! (SR68332) Mod to add new fields from     ! PWW *~
            *          !          oradesc2 if it is a part.       !     *~
            *          !                                          !     *~
            *10/08/2015! SR69520 Mod to change BarCode to a       ! PWW *~
            *          !         vertical format. Rearranged some !     *~
            *          !         fields to make room.             !     *~
            * 01/04/16 ! SR67154  2016 NFRC reg changes for foam. ! PWW *~
            *03/01/2016! SR72918 Mod to add Coastal hardware back ! PWW *~
            *          !         in place after loosing it during !     *~
            *          !         middle text block reformat. See  !     *~
            *          !         I;Documentation>Developers>BarTen!     *~
            *          !         derTemplates>Prod Label>Prod Labe!     *~
            *          !         l Text Layout - Technical        !     *~
            *06/21/2016! (SR75604) Mod to add spacer/intercept to ! PWW *~
            *          !          Prod Label.                     !     *~
            *07/21/2016! CR00548  Mod to add 100% inspections     ! PWW *~
            *          !          for Models.                     !     *~
            *          !                                          !     *~
            *08/04/2016! CR00581 Added second Alpha Letter. See   ! PWW *~
            *          !         I;Documentation>Developers>BarTen!     *~
            *          !         derTemplates>Prod Label>Prod Labe!     *~
            *          !         l Alpha Letter Layout.           !     *~
            *03/24/2017!SR80066 Add TX Dept 130 to FIN/FLANGE     ! MES *~
            *05/25/2017! CR0978 Increase SKU from 6 to 8          ! RDB *~ 
            *06/05/2017! CR 986 New screen mesh field added       ! RDB *~
            *          !SR80871 stop label print dept 101 TX      ! RDB *~
            *08/01/2017!CR1051  Remove ^XB which causes issue on  ! RDB *~
            *          !        on last label tear off            ! RDB *~
            *08/10/2017!CR1073  Change label layout for AD LANE   ! RDB *~
            *09/21/2017!CR1125  New coast paint P,Q,R,S           ! RDB *~
            *09/21/2017!CR1127  Remove color for BBG (4) grid type! RDB *~ 
            *10/18/2017!CR1174  Changes to label to add Shipment Block ! RDB *~
            *10/20/2017!CR1187  Include B on label if Bay guts         ! RDB *~
            *01/09/2018!CR1257  New NAMI Certification in Warranty area! RDB *~
            *02/09/2018!CR1313  Increase print size of the color text  ! RDB *~
            *03/02/2018!CR1181  New coastal paint U                    ! RDB *~
            *09/24/2018!CR1654  Add FTBOLT and WODC to label           ! RDB *~
            *11/09/2018!CR1790  Remove no print dept 101 TX            ! RDB *~
            *12/20/2018!CR1829  convert cust to 9 char for Dallas      ! DES *~
            *01/29/2019!CR1906  WindowNation                           ! RDB *~
            *02/21/2019!CR1918  PlyGem PO                              ! RDB *~
            *02/21/2019!CR1934  PlyGem SKU                             ! RDB *~
            *02/22/2019!CR1940  Add PG performance lbl # to prod label ! RDB *~
            *05/01/2019!CR2001  Add Oradesc2 PG Series to label        ! RDB *~
            *10/25/2019!CR2307  Add Flex screen to label boxes         ! RDB *~
            *12/19/2019!CR2375  Remove brand on label for some dealers ! RDB *~
            *04/23/2020!CR2532  Add new garden window model            ! RDB *~
            *04/23/2020!CR2513  Change to correct liting               ! RDB *~
            *10/02/2020!CR2690  Valide SKU in awdskuxr                 ! RDB *~
            *11/05/2020!CR2710  Tommy request Superspacer NAMI blank   ! RDB *~
            *01/13/2021!CR2748  Filter some email for SKU by model     ! RDB *~
            *04/19/2021!CR2818  New NAMI number for Texas              ! RDB *~
            *05/10/2021!CR2825  New Quality dept inspection QC print   ! RDB *~
            *11/17/2021!CR2957  Dept 076 chg to slider models          ! RDB *~
            *11/19/2021!CR2958  New NC Warranty NAMI nbr for intercp 06! RDB *~
            *05/11/2023!CR3316  For parts only add qty from desc to txt! RDB *~
            *          !        field if no text exits                 !     *~
            *******************************************************************
    
        sub "EWDPLA71" (been_here%,      /* Zero (Only 1st Time)       */~
                        rec$(),          /* Prod Label Data    (PAR002)*/~
                        #1,              /* GENCODES           (EWD018)*/~
                        #2,              /* APCPLNDT           (EWD022)*/~
                        #4,              /* BCKLINES           (AWD007)*/~
                        #3,              /* BCKSUBPT           (PAR000)*/~
                        #58,             /* ORADESC2            SR68332*/~
                        #8,              /* AWDAPPLS            CR1073 */~
                        #9,              /* AWDSKUXR            CR2690 */~
                        #16,             /* BCKMASTR            CR1918 */~
                        error%)          /* Return Code                */

        dim                                                              ~
            schema$8,                    /* (PAR003) Schema Switch     */~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
            a$100, b$100,                /* Print Lines for Label      */~
            lbl$(75%)55,                 /* Label Data Array   (EWD001)*/~
            hdr$40,                      /* ASKUSER Header Text        */~
            msg$(3%)79,                  /* ASKUSER Info Text          */~
            lb_text$210,                 /* Mfg. Text Holding  (EWD001)*/~
            fs$3,                        /* End of Lbl Records         */~
            file$8,                      /* Lbl Print File             */~
            library$8,                   /* Library Name = APCDATA     */~
            script$8,                    /* Lbl SHELL SCRIPT           */~
            volume$6,                    /* DISK VOLUME = CARLOS       */~
            dt_key0$23,                  /* Primary Key        (EWD022)*/~
            dt_seq$5,                    /* Sequence No.       (EWD022)*/~
            sc1$30,                      /* No Screen Codes    (AWD025)*/~
            sc2$30,                      /* 1/2 Screen Codes   (AWD025)*/~
            sc3$30,                      /* Full Screen Codes  (AWD025)*/~
            muttin$15,                   /* Vertical Horizontal CR2513 */~
            vhmuttin$15,                 /* Vertical Horizontal CR2513 */~
            rec$(4%)256,                 /* Rec Array Prod. Lbl(PAR002)*/~
            email_data$78,               /* Text for Email CR2690      */~
            yy2_test$100, b_test$100     /*Testing  pww */

        dim lb_mdl$3,                    /* Model No.                  */~
            lb_send$21, qty_of$10,       /* Send-To Info (Mfg.)       */~
            lb_txt$(4%)30,               /* Mfg. Text Array    (EWD001)*/~
            lb_serno$8,                  /* Series No.                 */~
            lb_bc$18,                    /* Production Barcode ID      */~
            lb_line$2,                   /* S.O. Line Item No. (EWD016)*~
            lb_sernm$10,                 /* Private Label Name         */~
            lb_seq$5,                    /* Sequence Number            */~
            lb_cust$19,                  /* Customer Name              */~
            lb_po$16,                    /* Purchase Order No.         */~
            lb_itmno$3,                  /* Item No. (X in X of Y)     */~
            lb_itmtot$3,                 /* Item Total (Y in X of Y)   */~
            lb_so$8,                     /* Sales Order No.            */~
            lb_city$18,                  /* Customer's City            */~
            lb_state$2,                  /* Customer's State           */~
            lb_cont$16,                  /* Contractor                 */~
            lb_job$10,                   /* Job No./Name               */~
            lb_room$7,                   /* Room Name                  */~
            lb_nomsz$6,                  /* Nominal Size               */~
            lb_opnsz$19,                 /* Opening Size               */~
            lb_exsz$19,                  /* Exact Size                 */~
            lb_info$(2%)18, prod_desc$77,/* Other Info Array   (PAR002)*/~
            lb_oth1$24,                  /* New Sub Part No.   (PAR002)*/~
            lb_oth2$40,                  /* New Sub Part Descr (PAR002)*/~
            lb_sku$15,                   /* SKU No.                    */~
            lb_duedt$10,                 /* Due Date (Formatted)       */~
            lb_part$25,                  /* Mfg. Part No.              */~
            lb_drop$2,                   /* Drop Code                  */~
            lb_load$5,                   /* Load Code                  */~
            lb_makedt$10,                /* Make Date (Formatted)      */~
            lb_wnty$10,                  /* Warranty ID                */~
            lb_upc$11,                   /* UPC No.                    */~
            lb_dept$3,                   /* Department Code            */~
            lb_pms$5,                    /* Pull/Make/Stock            */~
            lb_wood$6,                   /* Wood Surround Code         */~
            lb_w$1, lb_w2$1,             /* (EWD004) W or Blank        */~
            bay$3,                       /* For Bay Bow        (EWD007)*/~
            lb_samp$1,                   /* 0=No,1=Samp,2=Disp (EWD008)*/~
            lb_foam$1,                   /* (Y)es or (N)o      (EWD008)*/~
            lb_fin$1,                    /* (Y)es or (N)o      (EWD010)*/~
            lb_frame$1,                  /* Frame (Y)es or (N)o(EWD014)*/~
            lb_cust_code$9,              /* Customer Code      (EWD018)*/~
            filler$6,                    /* Old Customer Code          */~
            lb_cust_sav$9,               /* Save Customer Code (AWD007)*/~
            lb_inspect$6,                /* 100% Inspection    (EWD018)*/~
            lb_config$2,                 /* Config code        (EWD021)*/~
            lb_config_txt$17,            /* Config String      (EWD021)*/~
            lb_config_txt3$17,           /* Line (3)           (EWD022)*/~
            lb_mull$3,                   /* Save Mull Code     (AWD025)*/~
            lb_screen$1,                 /* Save Screen Code   (AWD025)*/~
            lb_box1$1,                   /* Use for Screen test(AWD025)*/~
            lb_box2$1,                   /* Use for Screen test(AWD025)*/~
            testdate$10,                 /* Calulate Day       (EWD024)*/~
            day$1,                       /* Day of the Week    (EWD024)*/~
            lb_glass$2, lb_lit$2,        /* Glass and Liting   (AWD029)*/~
            lb_sub_info$10,              /* New Info Fields    (PAR002)*/~
            lb_ups$1,                    /* UPS Flag -         (AWD031)*/~
            lb_specialmull$1,            /* Special Mull Code  (PAR006)*/~
            lb_gold$10,                  /* Gold Amaa          (AWD012)*/~
            yy$(200%)100,                /* Buffer                     */~
            yy2$(200%)100,               /* Buffer                     */~
            xx$(200%)100,                /* Buffer                     */~
            parent_cust$9,               /* Parent Customer            */~
            so_number$16,                /* Sales Order        (AWD007)*/~
            lb_applfin$1,                /* Applied Fin Option (AWD022)*/~
            lb_sillopt$1,                /* (AWD027) silloption        */~
            lb_docks$4,                  /* CR1073 new dock and dock 1 */~
            lb_dock$2,                   /* CR1073 new Appain dock     */~
            lb_dock1$1,                  /* CR1073 new Appain dock 1   */~
            scrmesh$4,                   /* Screen Mesh new field (CR 986)*/~
            lb_shpblk$3                  /* Shipment Block CR1174      */             

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ Fwas successful */~
            axd$4,                       /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        dim                                                              ~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            sub_part_nbr$20,             /* Subpart # in BCKSUBPT CR1257*/~
            descr$20,                    /* Gencode descr field CR1257 */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4,            /* Additional Info Fields     */~
            intercept$2                  /* SR75604  Intercept Type    */
        dim interdesc$(99)5              /* SR75604  intercept desc    */
        dim gc_key$24, gc_desc$32        /* SR75604                    */
        dim lb_app_key0$20               /* Primary Key                */

                                         /* (PAR000)                   */
        dim mst_key$25,                  /* BCKMASTR Key  CR1918       */~
            bck_pg_po$20,                /* BCKMASTR PlyGem PO         */~
            bck_qc_flag$1,               /* BCKMASTR Quality Dept Flag */~
            hld_desc$250,                /* ORADESC2 L2_description CR3316*/~
            pg_film$1,                   /* Film only PlyGem CR1940    */~
            pg_series$25,                /* ORADESC2 PG series         */~
            pg_prflbl$4                  /* ORADESC2 performance label CR1940 */
            
/* <AWD011> */
        dim tmp_bar$18, lowes_key$23, lowes_sku$10      /* CR1934 */
/* </AWD011> */
/*SR68332*/
REM     dim oradescr_key$11,             /* ORADESC2 Read Key          */~
            oradescr_rec$(4%)256         /* ORADESC2 Record            */

/* CR2690 */
        dim skukey$100                   /* Read key                   */
            
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(AWD) Generate Production Labels        "
            pname$ = "EWDPLA71 - Rev: R8.00"

        REM *************************************************************
            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #5  ! MFGPROD  ! Print File For Production Labels         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #5, "ROYAL", varc, consec, recsize =  100


            twice% = 0%                           /* (EWD009)      */
                                                  /* Flag to print */
                                                  /* Two Labels    */
L10000:                                           /* (EWD009)      */
            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            sel% = error%
            error%     = 0%
            nbr_lines% = 0%
            fs$ = "^FS"
                                                  /* Fix Bay/Bow Prob */
                                                  /* (03/15/06)       */
            init(" ") lb_cust_code$, bay$
REM        lb_cust_code$ = str(rec$(),628%,6%)     /* (AWD019)         */
        lb_cust_code$ = str(rec$(),727%,9%)     /* (CR1829)         */
             
        if str(lb_cust_code$,1,5) = "BA111" then bay$ = "BAY"
  REM          bay$ = str(rec$(),301%,3%)            /* (EWD007)         */


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
            init(" ") xx$()
            if been_here% > 0% then goto L01000
               gosub load_interdesc                   /* SR75604   */
               gosub load_label           /* Build Label Format yy$() */
               gosub set_file_name        /* Create Label Print File  */
               gosub open_file            /* Open Label Print file    */

L01000:        if sel% = 99% then goto exit_print

                  been_here% = been_here% + 1%  /* (EWD020)           */
                  copy yy$() to xx$()
                  gosub begin_process

                  goto exit_sub

        set_file_name                             /* (RHHTEST)       */
            init(" ") file$, script$
                                                  /* (PAR003)        */
            err%    = 0%
            call "SCHEMA" (schema$,      /* What switch 1-NC 2-NE      */~
                           schema%,      /* Schema                     */~
                           #1,           /* GENCODES                   */~
                           err% )        /* error                      */

            if err% = 0% then goto SS_1
               errormsg$ = "(Error) Schema Lookup Error)"
               gosub error_prompt
               error% = 9%
               end

SS_1:                                                /* (PAR002)        */
                                                     /* (EWD014)        */
                                                     /* (RHHTEST)       */

            if schema% <> 1% then goto SS_2
                                                     /* North Caolina   */
               file$    = "MFGPROD"
/* temp        file$    = "MPWPROD"                                     */
               script$  = "MFGPROD"
/* temp        script$  = "MPWPROD"                                     */

               library$ = "APCDATA "
               volume$  = "CARLOS"
               goto SS_3
                                                     /* North East      */
SS_2:
               file$    = "NEPROD"
               script$  = "NEPROD"

               library$ = "NEDATA  "
               volume$  = "NE    "

SS_3:
        REM    file$   = "MFGTEST"                /* (EWD010)        */
       REM   script$ = "MFGTEST"

        REM    file$   = "NEATEST"
        REM    script$ = "NEATEST"

        return                                    /* (PAR002)        */

        open_file
            call "OPENFILE" (#5, "IO   ", f2%(5%), rslt$(5%), axd$ )
            if f2%(5%) <> 0% then goto L01100
               gosub file_exists
               if comp% <> 16% then goto exit_sub
                  call "FILEBGON" (#5)

L01100:    open nodisplay #5, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return


        REM *************************************************************~
            *                P R O C E S S   D A T A                    *~
            *************************************************************
        begin_process                             /* (EWD001)          */
                                                  /* (EWD008) 11/19/99 */
                                                  /* (AWD031) 06/15/05 */
                                                  /* (PAR002) 02/20/06 */
            get str(rec$()), using L35000, lb_mdl$, lb_send$, lb_text$,  ~
                lb_serno$, lb_bc$, lb_sernm$, lb_seq$, lb_cust$, lb_po$, ~
                lb_itmno$, lb_itmtot$, lb_so$, lb_city$, lb_state$,      ~
                lb_cont$, lb_job$, lb_room$, lb_nomsz$, lb_opnsz$,       ~
                lb_exsz$, lb_info$(), lb_sku$, lb_duedt$, lb_part$,      ~
                lb_drop$, lb_load$, lb_makedt$, lb_wnty$, lb_upc$,       ~
                lb_dept$, lb_pms$, lb_wood$, lb_samp$, lb_foam$, lb_fin$,~
                lb_oth1$, lb_frame$, filler$, lb_config$, lb_ups$,       ~
                lb_sub_info$, lb_oth2$, lb_specialmull$, lb_gold$,       ~
/*SR75604*/     lb_applfin$, lb_intercept$, lb_shpblk$, lb_cust_code$,   ~
                            eod goto end_process, data goto bad_data_flag

  
                                                  /* lb_oth1$ = New Sub*/
                                                  /* Part Number       */
                                                  /* (PAR002)          */
                                                  /* (EWD010) 02/24/00 */
                                                  /* (EWD001) ???      */
                                                  /* (EWD008) 11/19/99 */
                                                  /* (EWD016) 08/30/02 */
                                                  /* (EWD018) 06/04/03 */
                                                  /* (EWD021) 06/16/03 */
                                                  /* (AWD031) 05/26/05 */
/* CR1790 */                                                  
 /* removed 11092018 if lb_dept$ = "101" and schema% = 2% then return SR80871*/
                                                       /* Prod Text    */
                lb_txt$(1%) = str(lb_text$,1%,30%) & "                              "
                lb_txt$(2%) = str(lb_text$,31%,30%)& "                              "
                                                       /* Glass Text   */
                lb_txt$(3%) = str(lb_text$,71%,30%) &"                              "
                lb_txt$(4%) = str(lb_text$,101%,30%)&"                              "
/*<AWD033>+ */

          if str(lb_oth1$,8%,1%) = "1" and str(lb_oth1$,9%,1%) = "3" then ~
             str(lb_txt$(4%),24%,7%) = "7/8 SDL"
/*<AWD033>- */
                qty_of$ = "XXX OF XXX"

                                                       /* (EWD001) ??? */
                                                       /* (EWD018)     */
            init (" ") lbl$(), lb_inspect$, lb_mull$, lb_screen$, lb_sillopt$
            lb_screen$ = str(lb_part$,11%,1%)          /* (AWD025)     */
            lb_sillopt$ = str(lb_oth1$,18%,1%)         /* (AWD027)     */

/*          lbl$(01%) = " " & lb_mdl$    & " " & fs$      Model       3*/
/*AWD036*/  lbl$(01%) = lb_mdl$               & fs$   /* Model       3*/
                                                       /* Send Text  21*/
            lbl$(02%) = lb_send$              & fs$
                                                     /* Quantity of10  */
            str(qty_of$,1%,3%) = str(lb_bc$,12%,3%)  /* Line Item Piece*/
            str(qty_of$,8%,3%) = str(lb_bc$,16%,3%)  /* Line Item Total*/

            lbl$(03%) = qty_of$               & fs$

            lbl$(04%) = lb_txt$(1%)           & fs$    /* Text (1)     */
                                                       /* Text (2)   30*/
            lbl$(05%) = lb_txt$(2%)           & fs$
                                                       /* Text (3)   30*/
            lbl$(06%) = lb_txt$(3%)           & fs$
                                                       /* Text (4)   30*/
            lbl$(07%) = lb_txt$(4%)           & fs$
                                                       /* Department  3*/
            lbl$(08%) = lb_dept$              & fs$
                                                       /* Pull/Make/s 5*/
            lbl$(09%) = lb_pms$               & fs$
                                                       /* Make Date  10*/
            lbl$(10%) = lb_makedt$            & fs$
                                                       /* Series Name 8*/
/*AWD037 + */
            so_inv$ = lb_so$
            item_no$ = str(lb_bc$,9%,2%)                /* S.O. Line Item*/
            gosub lookup_sub_part  /*AWD037  get series from bcksubpt  */
            if str(bcksubpt_rec$,169%,1%) = " " or                           ~
               str(bcksubpt_rec$,169%,1%) = hex(00) then normal_series_style
            if str(bcksubpt_rec$,185%,1%) = " " or                           ~
               str(bcksubpt_rec$,185%,1%) = hex(00) then normal_series_style
            p_series$ = str(bcksubpt_rec$,169%, 16%)
            p_style$  = str(bcksubpt_rec$,185%, 10%)
            pp% = pos(p_series$ = " ")
            pp2% = pos(p_style$ = " ")
            lb_serno$ = str(p_series$,1%, pp% - 1%) & " " & str(p_style$,1%, pp2% - 1%)

normal_series_style
/*AWD037 - */
                                                       /* Series Name 8*/
            lb_serno$ = lb_serno$ & "        "
            lbl$(11%) = lb_serno$             & fs$
                                                       /* Barcode    18*/
            lbl$(12%) = lb_bc$                & fs$
                                                       /* Private Lb 10*/
/*(AWD019) */
            if bay$ = "BAY" then lb_sernm$ = "BAY/BOW"
/*(AWD026) */
            if str(lb_cust_code$,1,6) = "TH9050" then lb_sernm$ = "THERMAL"
            lb_sernm$ = lb_sernm$ & "          "
            lbl$(13%) = lb_sernm$             & fs$
            if bay$ = "BAY" then                                         ~
               lbl$(13%) = " " & lb_sernm$ & " " & fs$ /* (EWD007)     */
            if str(lb_sernm$,1%,10%) = "WindowNati"  then ~
                lbl$(13%) = "WdwNation" & fs$
/* CR2375 */
            gosub check_cust_brand
            if lblbrand% = 1%  then  lbl$(13%) = desc$ & fs$
            
                                                       /* Sequence No 5*/
            lbl$(14%) = lb_seq$               & fs$
                                                       /* Cust Name  19*/
            lb_cust$ = lb_cust$ & "                   "
            lbl$(15%) = lb_cust$              & fs$
                                                       /* P.O No.    16*/
            lb_po$ = lb_po$ & "                "
            lbl$(16%) = lb_po$                & fs$

            gosub lookup_pg_po
            if bck_pg_po$ > " " then lbl$(16%) = bck_pg_po$ & " " & fs$        
            
                                                       /* Sales Ord   8*/
            lbl$(17%) = lb_so$                & fs$
                                                       /* P.O. Piece  3*/
            lbl$(18%) = lb_itmno$             & fs$
                                                       /* P.O total   3*/
            lbl$(19%) = lb_itmtot$            & fs$
                                                       /* City       18*/
            lb_city$ = lb_city$ & "                  "
            lbl$(20%) = lb_city$              & fs$
                                                       /* State       2*/
            lbl$(21%) = lb_state$             & fs$
                                                       /* (EWD021)     */

            init(" ") lb_config_txt$

            lb_config_txt$ = str(lb_so$,6%,3%) & "-" & lb_config$ &      ~
/*                           "    " & str(lb_bc$,12%,3%) & "-"         */~
/*SR69520*/                  "   " & str(lb_bc$,12%,3%) & "-"            ~
                                    & str(lb_bc$,16%,3%)

                                                       /* remove Contr */
                                                       /* Job Name     */
                                                       /* Room Loc     */
                                                       /* Nominal Size */
                                                       /* Line 1     34*/
            lbl$(22%) = "NOMINAL SIZE: " & lb_nomsz$  & fs$
                                                       /* Line 2     17*/
            lbl$(23%) = lb_config_txt$  & fs$
                                                       /* Line 3     17*/
            lb_config_txt3$ = "                 "      /* (EWD024)     */
            gosub calculate_day
            lbl$(24%) = lb_config_txt3$  & fs$

                                                       /* lbl$(25%) Avail */
                                                       /* (EWD021)     */

                                                       /* Opening Sz 19*/
            lb_opnsz$ = lb_opnsz$ & "          "
            lbl$(26%) = lb_opnsz$             & fs$
                                                       /* Exact Size 19*/
            lb_exsz$ = lb_exsz$ & "          "
            lbl$(27%) = lb_exsz$              & fs$
                                                       /* Prod Desc  52*/
                                                       /* (EWD010)     */
                                                       /* (PAR002)     */
            prod_desc$ = lb_info$(1%) & lb_info$(2%) & " " & lb_oth2$
                                                       /* lb_oth2$ New Descr */

/*SR68332   if len(lb_part$) < 19 then prod_desc$ = lb_txt$(1%) & lb_txt$(2%)*/
/*SR68332*/ if len(lb_part$) < 19 then prod_desc$ =                        ~
                      lb_info$(1%) & lb_info$(2%) & lb_oth2$

            lbl$(28%) = str(prod_desc$,1%,23%)  & fs$  /* 1st line Descr */
            lbl$(29%) = str(prod_desc$,24%,29%) & fs$  /* 2nd line Descr */
            lbl$(30%) = str(prod_desc$,53%,24%) & fs$  /* 3rd Line Descr */

                                                       /* (PAR002)     */
                                                       /* (EWD010)     */
                                                       /* Sku No     15*/
            lb_sku$ = lb_sku$ & "               "
                                                       /* (PAR002)     */
                                                       /* Sku not Printed */
        REM    lbl$(30%) = lb_sku$               & fs$
                                                       /* Due Date    10*/
            lbl$(31%) = lb_duedt$             & fs$
                                                       /* Part No.    25*/
                                                       /* (PAR002)      */
            lb_part$ = lb_part$ & "                "
                                                       /* (PAR006)      */
REM            lb_part% = len(lb_part$)                   /* Part Number Length */
REM            if lb_specialmull$ = "C" then                                 ~
                                     str(lb_part$,lb_part%-2%,3%) = "   "

/* (AWD013) */
REM         lbl$(32%) = str(lb_part$,1%,25%) & " " & str(lb_oth1$,1%,12%) & fs$
            lbl$(32%) = str(lb_part$,1%,25%) & " " & str(lb_oth1$,1%,20%) & fs$
                                                       /* (PAR006)      */

                                                       /* (PAR002)      */

                                                       /* (EWD018)      */
                                                       /* Remove Drop   */

                                                       /* Add Cust Code */
            lb_mull% = 1%                              /* (AWD025)      */
                                                       /* Fix           */

            if len(lb_part$) < 20 then goto skip_mull
                                                       /* Store Code    */
               if len(lb_part$) < 23 then lb_mull$ = str(lb_part$,20%,3%)
               if len(lb_part$) > 22 then lb_mull$ = str(lb_part$,23%,3%)
               lb_mull% = pos("0123456789" = str(lb_mull$,1%,1%) )

                                                       /* Fix          */
        REM       lb_mull% = pos("0123456789" = str(lb_mull$,23%,1%) )
skip_mull:
                                                       /* (AWD025)      */

                                                       /* Cust Code    6*/
        REM    lbl$(33%) = lb_cust_code$         & fs$
                                                       /* (EWD019)     8*/
                                                       /* Put Drop Back */
/*(AWD022)*/
REM            lbl$(33%) = "DROP " & lb_drop$       & fs$
/*CR1073 */
            gosub get_dock
            lbl$(70) = lb_docks$  &  fs$

            if lb_applfin$ <> " " then lbl$(33%) = "ApplFin" & fs$

/*SR67154+*/if lb_fin$  = "N" then lbl$(33%) = "NOFIN" & fs$
            if schema% = 2% and lb_fin$  = "N" then lbl$(33%) = "FIN "   ~
                                          & str(lb_sub_info$,4%,1%) & fs$
/*SR67154-*/if lb_sillopt$ = "6" then lbl$(33%) = "FIN   " & fs$

                                                       /* UPC Code    11*/
            lb_upc$ = lb_upc$ & "           "
            lbl$(34%) = lb_upc$               & fs$
                                                       /* Load Number  5*/
            lbl$(35%) = lb_load$              & fs$
                                                       /* Make Date   10*/
            lbl$(36%) = lb_makedt$            & fs$
                                                       /* Warranty Id  8*/
            lbl$(37%) = str(lb_wnty$,1%,8%)   & fs$
                                                       /* Sales Order  8*/
            lbl$(38%) = lb_so$                & fs$
                                                       /*Wood Surr Code3*/
            lbl$(39%) = "W/F: " & lb_wood$    & fs$    /* (EWD004)      */
                                                       /* (EWD005)      */

            lb_w$, lb_w2$ = " "
            if str(lb_wood$,1%,3%) <> "N/A" then lb_w$ = "W"
            if len(lb_part$) < 25 then lb_w$ = " "
            gosub check_samples

            if len(lb_part$) < 20 then goto LW_1
               if lb_mull% = 0% then lb_w$ = "W"
        REM       goto LW_2

LW_1:       if ss% = 0% then goto LW_2
        REM    if ss% > 0% then lb_w$ = " "            /* (AWD025)      */
                                                       /* (EWD012)      */
                                                       /* (EWD015)      */
            if ss% > 11% and ss% < 29% then lb_w$ = "P"
            if ss% > 11% and ss% < 29% then lb_wood$ = "Part  "
                                                       /* (EWD015)      */
                                                       /* (EWD012)      */
LW_2:       if str(lb_so$,1%,1%) = "S" then lb_w$ = " "
                                                       /* (EWD018)      */

            gosub check_parts
            lbl$(39%) = "W/F: " & lb_wood$    & fs$    /* (EWD004)      */
            lbl$(40%) = lb_w$                 & fs$
                                                       /* (EWD018)      */

                                                       /* (EWD001)      */
                                                       /* (EWD008) Beg  */
/*SR67154   Change Field #41 to new Field #64                           */

            gosub lookup_quality_flg                   /* CR2825        */
            
/*          lb_samp$ = "1"     pwww T E M P   * * * ! */
            lbl$(64%) = "      "              & fs$
            if lb_samp$ <> "1" then goto LS_2
               lbl$(64%) = "SAMPLE" & fs$

            if bck_qc_flag$ = "1" then lbl$(64%) = "SMP/QC" & fs$
                                                       /* (EWD023)      */
               if lb_w$ = "P" then goto LS_1
         REM         lb_w$    = " "                    /* (AWD025)      */
         REM         lb_wood$ = " "                    /* (AWD025)      */

LS_1:          lbl$(39%) = "W/F: " & lb_wood$    & fs$
               lbl$(40%) = lb_w$                 & fs$
               goto LS_3

LS_2:       if lb_samp$ <> "2" then goto LS_3
               lbl$(64%) = "DISPLY" & fs$
/* CR2825 */
            if bck_qc_flag$ = "1" then lbl$(64%) = "DSP/QC" & fs$
                                                       /* (EWD023)      */
               if lb_w$ = "P" then goto LS_2A
        REM          lb_w$    = " "                    /* (AWD025)      */
        REM          lb_wood$ = " "                    /* (AWD025)      */

LS_2A:         lbl$(39%) = "W/F: " & lb_wood$    & fs$
               lbl$(40%) = lb_w$                 & fs$

LS_3:
                                                       /* (AWD031)      */

            if lb_ups$ = "1" then lbl$(64%) = " U P S" & fs$
/* CR2825 */
            if lb_ups$ = "1" and bck_qc_flag$ = "1" then ~
                  lbl$(64%) = "UPS/QC" & fs$

/* CR2825 */                                                    
            if bck_qc_flag$ <> "1" then goto LS_3A
            if lb_samp$ <> "1" and lb_samp$ <> "2" and lb_ups$ <> "1" then ~
                   lbl$(64%) = " QC   " & fs$
            
LS_3A:
            lbl$(42%) = "     "               & fs$
            lbl$(68%) = "   "               & fs$

/*CR00581 + */
            if str(lb_oth1$,8%,1%) <> "1" then goto skip_SDL
            lb_w2$ = "S"
            if str(lb_oth1$,9%,1%) = "2" then lbl$(68%) = "SMR" & fs$     ~
                                         else lbl$(68%) = "SDL" & fs$
        skip_SDL
        
/*CR 986 */  
            scrmesh$ = "    "
            lbl$(69%) = "    " & fs$
            if str(bcksubpt_rec$,62%,1%) = "2" then scrmesh$ = "WIRE"   
            if str(bcksubpt_rec$,62%,1%) = "3" then scrmesh$ = "HVY "
            if str(bcksubpt_rec$,62%,1%) = "4" then scrmesh$ = "PET "
            if str(bcksubpt_rec$,62%,1%) = "9" then scrmesh$ = "CLR "
            lbl$(69%) = scrmesh$ & fs$
            
/*CR00581*/ gosub lookup_mull             /* moved up from below */
            if lb_specialmull$ <> "C" then goto skip_special_mull
            lb_w2$ = "C"
        skip_special_mull

/* (AWD016) */
 
/*CR00581 + */
            if lb_specialmull$ <> "M" then goto skip_special_mull2
            lb_w$ = "M"
        skip_special_mull2
/* (AWD009) beg */
/* (AWD009) end */
/* (AWD022) if optional applied fin is enabled and there is nothing the Wood */
/* field then put a C so the mfg prod lines will send it to the casing dept  */
/* else the mfg prod lines will wrap and send to staging/shipping            */
REM            IF LB_APPLFIN$ <> " " AND LB_W$ = " " THEN LB_W$ = "C"
REM            IF LB_W$ = "C" THEN LBL$(40%) = LB_W$ & FS$
/* (AWD023) if optional applied fin is enabled override wood surround code  */
/*CR00581 + */
            if lb_applfin$ = " " then goto skip_field67
            lb_w$ = "C"
        skip_field67
            lbl$(40%) = lb_w$                 & fs$
            lbl$(67%) = lb_w2$                & fs$


/*CR00581 - */
/* <AWD020> */
            lbl$(52%) = " " & fs$
            if str(lb_oth1$,5%,1%) = "2" then lbl$(52%) = "E" & fs$
/*SR72918*/ lbl$(65%) = "       "
/* CR1125 CR1181 new U Coastal */
            if pos("3DEPQRSU" = str(lb_oth1$,4%,1%)) <> 0%                 ~
/*SR75604 fix */           then lbl$(65%) = "Coastal" & fs$
            gosub AWD037_get_gencodes          /* Rob Becks New fields */
/*SR75604 + */
            lbl$(66%) = "   " & fs$
            if lb_intercept$ = "  " then goto skip_intercept
            convert lb_intercept$ to gl_intercept%, data goto dataerr1

dataerr1:

            lbl$(66%) = interdesc$(gl_intercept%) & fs$
        skip_intercept
/*SR75604 - */

            lbl$(71%) = lb_shpblk$ & fs$            /* CR1174 */        

/* </AWD020> */
            if len(lb_part$) < 19 then goto read_loop_prep
                                                       /* (PAR000)      */
            if str(lb_oth1$,5%,1%) = "1" then lb_foam$= "Y"

/*SR67154*/ if str(lb_oth1$,5%,1%) = "1" or str(lb_oth1$,5%,1%) = "4"     ~
               then lbl$(42%) = "Foam/Wrap" & fs$

/*SR67154*/ if str(lb_oth1$,5%,1%) = "3" or str(lb_oth1$,5%,1%) = "4"     ~
               then lbl$(41%) = "Spray/Foam" & fs$

/*SR67154   if lb_foam$ = "Y" then lbl$(42%) = "FOAM"  & fs$            */
/*SR67154   if lb_foam$ = "1" then lbl$(42%) = "FOAM"  & fs$            */
                                                       /* (PAR001)      */
                                                       /* Foam Supercedes */
                                                       /* NoFin         */
REM if window has foam then do not look at fin logic
            if lb_foam$ = "Y" or lb_foam$ = "1" then goto LS_4

/*SR67154    if lb_fin$  = "N" then lbl$(42%) = "NOFIN" & fs$           */
/*(AWD024) */
/*67154      if schema% = 2% and lb_fin$  = "N" then lbl$(42%) = "FIN "   ~
                                           & str(lb_sub_info$,4%,1%) & fs$ */
/*(AWD027) */
             if lb_dept$ = "019" then goto isFinFlange
             if lb_dept$ = "047" then goto isFinFlange
REM CR2957             if lb_dept$ = "006" then goto isFinFlange
/*(AWD035)*/ if lb_dept$ = "043" then goto isFinFlange
/*(SR80066) */
            if schema% = 2% and lb_dept$ = "012" then goto isFinFlange
            if schema% = 2% and lb_dept$ = "013" then goto isFinFlange
            if schema% = 2% and lb_dept$ = "003" then goto isFinFlange
REM             IF LB_DEPT$ <> "019" AND LB_DEPT$ <> "047" THEN GOTO LS_4
             goto LS_4
isFinFlange:
               if lb_sillopt$ = "4" then lbl$(42%) = "FLANGE" & fs$
               if lb_sillopt$ = "5" then lbl$(42%) = "FINFL " & fs$
/*SR67154      if lb_sillopt$ = "6" then lbl$(42%) = "FIN   " & fs$     */
               if lb_sillopt$ = "7" then lbl$(42%) = "REPL  " & fs$
                                                       /* (EWD010)      */
                                                       /* (EWD008) End  */
LS_4:

/* (AWD013) */
REM            p% = 0%                                    /* (EWD013) BEG  */
REM            p% = pos( "KLMN" = lb_screen$ )
REM            if p% = 0 then goto L20500

REM Only put NA if bay bow but not garden windows      CR2532 995 added

               if str(lb_oth1$,12,1) = "0" and   ~
                  str(lb_part$,1,1) = "9" and    ~
                  str(lb_part$,1,3) <> "995" and ~       
                  str(lb_part$,1,3) <> "997" and ~
                  str(lb_part$,1,3) <> "998" then ~
                      lbl$(42%) = "NA" & fs$

               if str(lb_oth1$,12,1) = "1" then ~
                      lbl$(42%) = "OAK" & fs$

               if str(lb_oth1$,12,1) = "2" then ~
               lbl$(42%) = "INS OAK" & fs$

               if str(lb_oth1$,12,1) = "3" then ~
               lbl$(42%) = "INS BIR" & fs$

               if str(lb_oth1$,12,1) = "4" then ~
               lbl$(42%) = "BIR" & fs$

REM L20500:
REM            p% = 0%
REM            p% = pos( "OPQR" = lb_screen$ )
REM            if p% = 0 then goto L20600
REM               lbl$(42%) = "INS OAK" & fs$

REM L20600:                                                /* (EWD013) END  */
                                                       /* Special Shape Frame */
           if lb_frame$ = "Y" then lbl$(42%) = "FLORIDA" & fs$
                                                       /* (EWD014)      */
                                                       /* (EWD016)      */
           lb_line$ = str(lb_bc$,9%,2%)                /* S.O. Line Item*/
              lbl$(43%) = " " & lb_line$ & " " & fs$
                                                       /* (EWD016)      */
                                                       /* (EWD018)      */

                                                       /* (AWD007) */
           lb_cust_sav$ = lb_cust_code$
           if str(lb_cust_code$,1,5) = "BA111" then gosub lookup_cust
           gosub check_customer
           gosub check_mdl_100
           gosub check_color_100
/*CR00548*/gosub check_3digit_model

           lb_cust_code$ = lb_cust_sav$
                                                       /* (AWD007/) */
           lbl$(44%) = lb_inspect$ & fs$

                                                       /* (EWD018)      */
                                                       /* (EWD022)      */
                                                       /* (AWD025)      */
                                                       /* (AWD026)      */
                                        /* Continous Head Mull Test     */
/*        gosub lookup_mull             CR00581                         */
                                                       /* (AWD026)      */
                                                       /* (AWD025)      */
          gosub lookup_seq
                                                       /* (AWD026)      */
                                                       /* (EWD022)      */

                                                       /* (AWD025)      */
          gosub check_screens
          lbl$(45%) = lb_box1$ & fs$
          lbl$(46%) = lb_box2$ & fs$
       /* <AWD021> */
          if str(lb_oth1$,5%,1%) = "2" and lb_box1$ = " " then        ~
                                            lbl$(45%) = "E" & fs$
       /* </AWD021> */
                                                       /* (AWD025)      */
                                                       /* (AWD027)      */
          gosub check_specials

          lbl$(48%) = "   " & fs$
          lbl$(47%) = "  " & fs$
          lbl$(50%) = "   " & fs$
          lbl$(51%) = "0,0,0^FS"
/* CR1654 */
          lbl$(72%) = "    " & fs$
          lbl$(73%) = "    " & fs$
          r% = 0%
          r% = pos("TUV" = str(lb_part$,11%,1%))
          if len(lb_part$) > 18 and r% > 0 then lbl$(72%) = "FTBOLT" & fs$
          if str(bcksubpt_rec$,63%,1%) = "2" then lbl$(73%) = "WOCD" & fs$

/* CR1940 */
          lbl$(74%) = "   " & fs$
          pg_prflbl$ = "   "
          gosub find_pg_prflbl
          lbl$(74%) = pg_prflbl$ & fs$
          
          lbl$(75%) = "        " & fs$
          pg_film$  = " "
          if schema% = 2% then pg_film$ = str(bcksubpt_rec$,52%,1%)
          if pg_film$ = "5"  then lbl$(75%) = "Film: I" & fs$ 
          if pg_film$ = "6"  then lbl$(75%) = "Film: E" & fs$ 
          if pg_film$ = "7"  then lbl$(75%) = "Film: B" & fs$
          
/* CR2001 */
          if pg_series$ > " " then lbl$(53%) = str(pg_series$,1%,20%) & fs$
          
      lowes_stock = 0
      gosub check_lowes_stock
REM   if lowes_stock = 1 then lbl$(48) = "STOCK" & fs$
/* <AWD017> */
      if lowes_stock <> 1 then goto L20650
/* CR0978 increase from 6 to 8*/
          lbl$(48) = "S " & str(lowes_sku$,1,9) & fs$  

L20650:          
/* CR1934 */ 
      pg_series%  = 0%
      if str(lb_part$,1%,1%) = "J" then pg_series% = 1%
      if pg_series% = 1% then lbl$(48) = lowes_sku$  & fs$
          /* get Lowes Sku */
/* CR2690 Validate SKU on awdskuxr if not reset to blank */
      if lowes_stock = 1 and pg_series% = 0% then gosub check_awdskuxr
      
/* </AWD017> */

          if specials% = 0% then lbl$(47%) = "SP" & fs$
                                                       /* (AWD027)      */

                                                       /* (AWD032)      */

          lbl$(49%) = lb_gold$ & fs$                   /* (AWD012)      */
          
/* CR1173 print X for Larson project sound windows */

            str(readkey$,1%,9%)   = "PLAN STC "         /* New Table     */
            str(readkey$,10%,15%) = str(lb_part$,5%,2%) /* Glass Code    */

            read #1,key = readkey$, using L62000, desc$,                  ~
                          eod goto L20670
                lbl$(49%) = "X" & fs$   
L20670:
/* CR1173 - */
                                                       /* (PAR006)      */
          if str(lb_part$,1%,1%) <> "9" then goto L20700
/*CR00581 */ lb_w$ = "B"                               /* Bay/Bow       */
             lbl$(40%) = lb_w$                 & fs$
/*CR00581 */ lbl$(67%) = lb_w2$                & fs$
             goto read_loop_prep
L20700:
/*CR1187 + */
         if str(lb_so$,1%,1%) <> "C" then goto L20725
             lb_w$ = "B"                               /* Bay/Bow guts  */
             lbl$(40%) = lb_w$                 & fs$
             lbl$(67%) = lb_w2$                & fs$
             goto read_loop_prep
L20725:
/*CR1187 - */

/* <AWD015> */
REM   if str(rec$(),608,1) <> "3" then goto read_loop
      if str(rec$(),608,1) <> "3" then goto L20750
           lbl$(51) = "54,144,54^FS"
       lbl$(50) = "MULL CLIP" & fs$
/* </AWD015> */
L20750:
/* <AWD018> */
          des$ = str(rec$(),603,20)
REM   if str(rec$(),603,1) <> "4" then goto read_loop
      if str(des$,1,1) <> "4" then goto read_loop_prep
           lbl$(51) = "54,144,54^FS"
           lbl$(50) = "  BLIND  " & fs$
/* </AWD018> */
                                                       /* PAR006)       */
    read_loop_prep
        if been_here% = 1% then gosub load_image

    read_loop
        init(" ") a$
        b$ = all(hex(00))
        nbr_lines% = nbr_lines% + 1%
        a$ = xx$(nbr_lines%)
        if a$ = " " then end_process
        a_len% = len(a$)                   /* Calc Length of Data String */
        str(b$,1%,a_len%) = str(a$,1%,a_len%) /* Put into b$ Data from a$ */
        convert str(a$,1%,2%) to ln%, data goto skip_data
                                           /* Look for a Field Number    */
        l_len% = len(lbl$(ln%))            /* Find Length of Data Element*/
                                           /* in the Label data array    */
        b_len% = (a_len% - 2%) + l_len%    /* Adjust for 2 digit field No*/

        b$ = all(hex(00))                  /* Initialize Print string    */
                                           /* 1st set Font data for print*/
                                           /* 2nd append Actual Data that*/
                                           /*     will be printed        */
        str(b$,1%,b_len%) = str(a$,3%,a_len%-2%)                           ~
            & str(lbl$(ln%),1%,l_len%)
      skip_data
         b_test$ = b$              /* pww test  */

                                           /* (AWD019)                  */
        if nbr_lines% = 1% and been_here% = 1% then                        ~
                               b$ = hex(7e) & str(a$,2%,a_len%)
        if nbr_lines% = 1% and been_here% > 1% then                        ~
                               goto read_loop
                                           /* (EWD020)                   */
/*      if been_here% > 1% and                                             ~
                         str(a$,1%, 5%) = "^ILR:" then goto read_loop */


                                           /* Special test U.P.C. Code   */
                                           /* (EWD021)                   */
                                           /* (AWD027) Location Chg      */
                                           /* (AWD028)                   */
        REM if nbr_lines% = 86% and len(lb_upc$) < 5 then goto read_loop
                                           /* EWD016                     */
                                           /* Skip contractor            */

                                           /* Skip Room Location         */

                                           /* (EWD006) - 09/09/1999      */
                                           /* (EWD020)                   */
REM     if nbr_lines% = 25% and bay$ <> "BAY"   then goto read_loop
                                           /* (EWD007) - 09/22/1999      */
                                           /* (EWD020)                   */
REM     if nbr_lines% = 27% and lb_inspect$ <> "*100%*" then goto read_loop

        gosub print_line
        if a$ = "^XZ" then end_process       /* Last Line */
        goto read_loop



    bad_data_flag
        error% = 7%


    end_process
        if nbr_lines% = 0% then error% = 8%
        return

        check_samples
            ss% = 0%
            if str(lb_part$,1%,1%) = "9" then return

            if len(lb_part$) < 20 then goto LS2      /* Quick Test      */
            convert str(lb_part$,20%,3%) to ss%, data goto LS1

            if ss% < 1% or ss% > 80% then goto LS1   /* Not Samp/Disp   */
                                                     /*   (EWD017)      */
            if str(lb_part$,7%,2%) > "99" then goto LS1
REM            if ss% = 11% then twice% = twice% + 1%   /* (EWD009)        */

        return                                       /* Code Found      */
LS1:        convert str(lb_part$,23%,3%) to ss%, data goto LS2

            if ss% < 1% or ss% > 80% then goto LS2
REM            if ss% = 11% then twice% = twice% + 1%   /* (EWD009)        */
                                                     /* Code Found      */
        return
LS2:        ss% = 0%
        return

/* <AWD011> */
check_lowes_stock
           lowes_stock = 0
           init(" ") lowes_key$, tmp_bar$
           str(lowes_key$,1%,18%) = lb_bc$
           str(lowes_key$,19%,3%) = "000"
           str(lowes_key$,22%,2%) = "00"
           read #2,key >= lowes_key$, using LSEQ2, tmp_bar$, lowes_sku$,   ~
                                                   eod goto LW0
LSEQ2:     FMT POS(24), CH(18), POS(245), CH(10)     /* CR1934 */
           if tmp_bar$ <> lb_bc$ then goto LW0
           if lowes_sku$ > "00000" then lowes_stock = 1
       /* (AWD014) */
           if lowes_sku$ = "88910" then lowes_stock = 0
           if lowes_sku$ = "2910"  then lowes_stock = 0
       if lowes_sku$ = "231061" then lowes_stock = 0
       if lowes_sku$ = "197556" then lowes_stock = 0
       if lowes_sku$ = "36277" then lowes_stock = 0
       /* (\AWD014) */
LW0:    return
/* </AWD011> */ 
                                                     /* (EWD022)        */
        lookup_seq
            init(" ") dt_key0$, dt_seq$

/*SR67154*/ goto lookup_seq_done    /* We will implement this later as a new field */

            str(dt_key0$,1%,18%) = lb_bc$
            str(dt_key0$,19%,3%) = "044"
            str(dt_key0$,22%,2%) = "01"
            read #2,key = dt_key0$, using LSEQ, dt_seq$,              ~
                                                 eod goto lookup_seq_done
LSEQ:            FMT POS(111), CH(05)
                                                    /* (AWD026)         */
              if lb_w$ = "W" or lb_w2$ = "W"                               ~
/*CR00581*/       then str(lb_config_txt3$,12%,6%) = "W" & dt_seq$

              if lb_w$ = "D" or lb_w2$ = "D"                               ~
/*CR00581*/       then str(lb_config_txt3$,12%,6%) = "D" & dt_seq$
                                                    /* (AWD026)         */
              init(" ") lbl$(24%)
              lbl$(24%) = lb_config_txt3$ & fs$

            lookup_seq_done
        return
                                                    /* (EWD022)         */
/* CR2690 */
        check_awdskuxr
          
          init(" ") skukey$
          str(skukey$,1%,14%) = "X_LO" & lowes_sku$
          read #9,key = skukey$, eod goto L40000
             return
L40000:     
          init(" ") skukey$
          str(skukey$,1%,14%) = "X_MB" & lowes_sku$
          read #9,key = skukey$, eod goto L40005
             return
L40005:
          init(" ") skukey$
          str(skukey$,1%,14%) = "X_SU" & lowes_sku$
          read #9,key = skukey$, eod goto L40010
             return     
L40010:
REM          lbl$(48%) = "   " & fs$  
          init(" ") email_data$
          
/* CR2748 check gencodes file to filter some models from sending email */
       /* bay/bow guts never get SKU */
          if str(lb_so$,1%,1%) = "C" then goto noemail   
          
          init(" ") readkey$
          str(readkey$,1%,9%)   = "EMAILSKU"        
          str(readkey$,10%,15%) = str(lb_part$,1%,3%)     /* Model  */

          read #1,key = readkey$, using L62000, desc$, eod goto nofilter 
          
          if str(desc$,1%,1%) = "N" then goto noemail

nofilter:
          str(email_data$,01,01) = hex(22)
          str(email_data$,02,60) =                                 ~
        "SKU missing: " & lowes_sku$ & " SO: " & lb_so$ & " PART: " & lb_part$
          str(email_data$,78,01) = hex(22)
          call "SKUMAIL" (email_data$)   
noemail:
        return

        calculate_day                               /* (EWD024)         */
            testdate$ =  lb_makedt$
            call "DATUFMTC" (testdate$)
            call "DAY" addr(str(testdate$,,6%), day%)

            day% = day% - 1%
            if day% = 0% then day% = 7%

            convert day% to day$, pic(#)

            str(lb_config_txt3$,1%,5%) = "DAY-" & day$

        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************
                                                       /* (PAR002)     */
L35000:     FMT                          /* Production Label Data      */~
                XX(35),                  /* Skip Primary Key           */~
                CH(3),                   /* Model No.                  */~
                CH(21),                  /* Send-To Info (Mfg.)        */~
                CH(210),                 /* Mfg. Text Array  (EWD001)  */~
                CH(8),                   /* Series No.                 */~
                CH(18),                  /* Production Barcode ID      */~
                XX(5),           /* Alternate Key Fields(EWD001)*/~
                CH(10),                  /* Private Label Name         */~
                CH(5),                   /* Sequence Number            */~
                CH(19),                  /* Customer Name              */~
                CH(16),                  /* Purchase Order No.         */~
                CH(3),                   /* Item No. (X in X of Y)     */~
                CH(3),                   /* Item Total (Y in X of Y)   */~
                CH(8),                   /* Sales Order No.            */~
                CH(18),                  /* Customer's City            */~
                CH(2),                   /* Customer's State           */~
                CH(16),                  /* Contractor                 */~
                CH(10),                  /* Job No./Name               */~
                CH(7),                   /* Room Name                  */~
                CH(6),                   /* Nominal Size               */~
                CH(19),                  /* Opening Size               */~
                CH(19),                  /* Exact Size                 */~
                2*CH(18),                /* Other Info Array           */~
                CH(15),                  /* SKU No.                    */~
                CH(10),                  /* Due Date (Formatted)       */~
                CH(25),                  /* Mfg. Part No.              */~
                CH(2),                   /* Drop Code                  */~
                CH(5),                   /* Load Code                  */~
                CH(10),                  /* Make Date (Formatted)      */~
                CH(10),                  /* Warranty ID                */~
                CH(11),                  /* UPC No.                    */~
                CH(03),                  /* Department Code            */~
/*POS(594)*/    CH(05),                  /* Pull/Make/Stock            */~
                CH(06),                  /* Wood Surround code (EWD004)*/~
                CH(01),                  /* 0=No,1=Samp,2=Disp (EWD008)*/~
                CH(01),                  /* Foam (Y)es or (N)o (EWD008)*/~
                CH(01),                  /* NoFin(Y)es or (N)o (EWD010)*/~
                CH(24),                  /* New Sub Part No    (PAR002)*/~
                CH(01),                  /* Shape Frame Y or N (EWD014)*/~
                CH(06),                  /* S.O. Customer Code (EWD018)*/~
                CH(02),                  /* W.W Config Line No (PAR002)*/~
                CH(01),                  /* UPS Flag 0 or 1    (PAR002)*/~
                CH(10),                  /* New Par Info Fields(PAR002)*/~
                CH(40),                  /* New Sub Part Descr (PAR002)*/~
                CH(01),                  /* New Special Mull code(PAR006)*/~
                XX(32),                  /* Job & Room (AWD012)        */~
                CH(1),                   /* Gold Amaa Label (AWD012)   */~
                CH(1),                   /* Applied Fin Option(AWD022) */~
                CH(2),                   /* Intercept  SR75604         */~
                CH(3),                   /* Shipment Block CR1174      */~
                CH(09)                   /* New Customer Code          */


        REM *************************************************************~
            *            I M A G E   S T A T E M E N T S                *~
            *************************************************************
L55030:     FMT CH(100)

        REM *************************************************************~
            *           S P E C I A L   R O U T I N E S                 *~
            *************************************************************

                                                        /* (PAR000)     */
        lookup_sub_part
            init(" ") bcksubpt_rec$, flds$(), info_flds$()
            flag$ = "0"                  /* Sales Order Info           */
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
                          #3,            /* BCKSUBPT File              */~
                          err1%)         /* Error Code                 */

            if err1% <> 0% then str(bcksubpt_rec$,48%,20%) = "00000                    "
            if err1% = 0% then return

            err1% = 0%
        return
                                                         /* (PAR000)    */
/* CR1918 */
        lookup_pg_po
            init(" ") mst_key$, bck_pg_po$
            str(mst_key$,1%,9%) = lb_cust_code$ 
            str(mst_key$,10%,16%) = lb_so$

            read #16, key = mst_key$, using L60110, bck_pg_po$,   ~
                                                       eod goto L60130

L60110:         FMT POS(920), CH(20)
        
L60130: return

/* CR2825 */       
        lookup_quality_flg
            
            init(" ") mst_key$, bck_qc_flag$, dt_key0$       
                    
            str(dt_key0$,1%,18%) = lb_bc$
            str(dt_key0$,19%,3%) = lb_dept$
            str(dt_key0$,22%,2%) = "01"
            read #2,key = dt_key0$, using LCUST, dt_cust$,              ~
                                                 eod goto L60140
LCUST:          FMT POS(124), CH(09)

            str(mst_key$,1%,9%) = dt_cust$
            str(mst_key$,10%,16%) = lb_so$

            read #16, key = mst_key$, using L60140, bck_qc_flag$,   ~
                                                       eod goto L60140

L60140:         FMT POS(990), CH(01)
        return        
        
        print_line
            write #5, using L55030, b$, eod goto L61550
        return

L61550:     error% = 5%
        return clear all

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************
        exit_sub

            if twice% = 1% then goto L10000
        end

        exit_print
            b$ = all(hex(00))
            b$ = "^XA^EG^XZ"
            gosub print_line
            b$ = "^XA^PWN^MNY^MMT^MTT^MD0^LH0,0^LL610^PW1690"
            gosub print_line
            b$ = "^PR4,4,8^JMA"
            gosub print_line
REM         b$ = "^FO50,50^CIO^A0,138,238^FDDUMMY^FS"
REM         gosub print_line
            b$ = "^PQ1,0,1,Y"
REM         b$ = "^PQ1"
            gosub print_line
            b$ = "^XZ"
            gosub print_line
            lb1% = 0% : lb2% = 0%

            close #5

            call "LINK" addr(script$, lb1%, lb2%)
            if lb1%    > 0% then error% = 4%

            call "FILEBGON" (#5)          /* Scratch 'MFGPROD'        */
                                          /*      or 'NEPROD'         */
        end

        load_label
                                                  /* (AWD025)         */
                                                  /* Multiple Meaning */
                                                  /* 389DHNR          */
                                                  /* (AWD030)         */
                                                  /* (PAR000) Out A   */
                                                  /* Take out 389DHNR */
           sc1$ = "0456EKOIYZ"                    /* No Screen Codes  */

                                                  /* (AWD030) Add-89DHNR*/
                                                  /* (PAR000) Out B,D */
           sc2$ = "1389DFHLNPRCJW"                 /* Half Screen Codes CR2307*/

                                                  /* (PAR000) Out C   */
           sc3$ = "2GMQSTUABX"                    /* Full Screen Codes CR2307*/
                                                  /* (8) = Screen Only*/
                                                  /* (8) - Half Screen*/
                                                  /* (AWD030)         */

                                                  /* (AWD025)         */

        init(" ") yy$()                           /* (EWD009)         */

           yy$( 1%) = "^JO"                 /* (EWD020)         */
           yy$( 2%) = "^XA"                 /* (EWD020)         */
           yy$( 3%) = "^ILR:PIMAGE.GRF^FS"
           yy$( 4%) = "^MNY^PMN^MMT"
           yy$( 5%) = "^MTT^MD0"             /* CR1051 - remove XB  */
                                                  /* (EWD020)         */
        REM   yy$(06%) = "^PR4"               /* PR Trying to eliminate backfeed */
           yy$(06%) = "^PR4,4,8"               /* PR Label Print Speed Original */
        REM   yy$(06%) = "^PR3,4,8"               /* a = 3 Inch per Sec  */
        REM   yy$(06%) = "^PR2,4,8"               /* a = 2 Inch per Sec  */
        REM   yy$(06%) = "^PR1,4,8"               /* a = 1 Inch per Sec  */
                                                  /* a = 4 Inch per Sec Print Speed   */
                                                  /* b = 4 Slew  SPeed   */
                                                  /* c = 8 Back Feed Spee*/

           yy$(07%) = "^XFR:IDATA.ZPL"            /* 256 + 128 Mem    */
REM        yy$(07%) = "^COY,4096"                  /* 256 + 128 Mem    */
REM        yy$(07%) = "^COY,362"                  /* 256 + 128 Mem    */
                                                  /* (-) 22k Intern   */


/*<AWD036> + */
           yy$(08%) = "01^FN1^FD"     /* N14^FS" */
           yy$(09%) = "16^FN2^FD"     /*155-SAMPLES 2 24^FS*/
           yy$(10%) = "13^FN3^FD"     /*NORANDEX^FS*/
           yy$(11%) = "43^FN4^FD"     /* 01 ^FS*/
           yy$(12%) = "27^FN5^FD" /* 20 X 20            ^FS*/
           yy$(13%) = "44^FN6^FD"     /*  *100%*^FS  */
           yy$(14%) = "45^FN7^FD"       /* X^FS  */
           yy$(15%) = "46^FN8^FD"      /* X^FS  */
           yy$(16%) = "47^FN9^FD"     /* SP^FS */
           yy$(17%) = "14^FN10^FD"     /*  00300^FS */
           yy$(18%) = "14^FN11^FD"     /* 00300 ^FS */
           yy$(19%) = "02^FN12^FD"      /* WOOD/SAMP/^FS */
           yy$(20%) = "03^FN13^FD"     /* 002 OF 005^FS */
           yy$(21%) = "04^FN14^FD"   /*X-----------X----------------X^FS*/
           yy$(22%) = "05^FN15^FD"   /*X----------------------------X^FS*/
           yy$(23%) = "06^FN16^FD"   /*X----------------------------X^FS*/
           yy$(24%) = "07^FN17^FD"   /*X------------X---------------X^FS*/
           yy$(25%) = "08^FN18^FD"      /* 036^FS */
           yy$(26%) = "10^FN19^FD"     /* 02 27 2014^FS */
           yy$(27%) = "11^FN20^FD"     /* 200 SH^FS */
           yy$(28%) = "12^FN21^FD>:"/* 128B */
           yy$(29%) = "15^FN22^FD"   /* NORANDEX #155 (NEW^FS */
           yy$(30%) = "17^FN23^FD"     /* 04739157^FS */
           yy$(31%) = "18^FN24^FD"     /* 002^FS */
           yy$(32%) = "19^FN25^FD"     /* 010^FS */

           yy$(33%) = "23^FN26^FD" /* 157-        1            002-005^FS*/
           yy$(34%) = "24^FN27^FD"     /* DAY-4^FS */
           yy$(35%) = "26^FN28^FD"     /* 20 1 2 X 30 1 2^FS */
           yy$(36%) = "31^FN29^FDDUE: "   /* 03 13 2014^FS */
           yy$(37%) = "49^FN30^FD"     /* G^FS */
           yy$(38%) = "32^FN31^FD" /*N142AM690096020030001     111A0000000000100000^FS*/
           yy$(39%) = "33^FN32^FD"     /* ApplFin^FS */
           yy$(40%) = "35^FN33^FD"     /* 59919^FS */
           yy$(41%) = "36^FN34^FD"     /* 02/27/2014^FS */
           yy$(42%) = "37^FN35^FD"     /* 24310394^FS */
           yy$(43%) = "38^FN36^FD"     /* 04739157^FS */
           yy$(44%) = "40^FN37^FD"     /* S^FS */
           yy$(45%) = "41^FN38^FD"     /* SAMPLE^FS */
           yy$(46%) = "42^FN39^FD"     /* FLORIDA^FS */
/*         yy$(47%) = "48^FN40^FD"     /* S 123456^FS Lowes Stock "S sku#" */
/*CR0978*/ yy$(47%) = "48^FN40^FD"     /* S 12345678^FS Lowes Stock "S sku#" */
           yy$(48%) = "49^FN41^FD"     /* G^FS */
/*SR67154  yy$(49%) = "51^FN42^GB"                                            */
/*SR67154  yy$(49%) = "^FO967,457^GB54,144,54^FS"                             */
/*SR67154*/yy$(49%) = "51^FO967,457^GB"
           yy$(50%) = "50^FN43^FD"     /* MULL CLIP^FS */
           yy$(51%) = "52^FN44^FD"     /* E^FS */
           
/*         yy$(52%) = "^FN45^FDSEQ NO.:^FS"                                   */
/*SR69520 CR1073 yy$(52%) = "^FN45^FDSEQ^FS"  ***** */
           yy$(52%) = "70^FN70^FD"          /* CR1073 */
/*         if bay$ = "BAY" then                                                ~
  SR69520     yy$(52%) = "^FN46^FDSEQ:^FS"                                    */
  
           yy$(53%) = "39^FN47^FD"     /* W/F Part  ^FS */
           yy$(54%) = "53^FN48^FD"
           yy$(55%) = "55^FN49^FD"
           yy$(56%) = "56^FN50^FD"
           yy$(57%) = "54^FN51^FD"
           yy$(58%) = "57^FN52^FD"
           yy$(59%) = "58^FN53^FD"
           yy$(60%) = "59^FN54^FD"
           yy$(61%) = "60^FN55^FD"
           yy$(62%) = "61^FN56^FD"
           yy$(63%) = "62^FN57^FD"
           yy$(64%) = "63^FN58^FD"       /* SR65493 */
           yy$(65%) = "12^FN59^FD"       /* SR69520 */
           yy$(66%) = "64^FN60^FD"       /* SR67154 */
           yy$(67%) = "65^FN61^FD"       /* SR72918 */
           yy$(68%) = "66^FN62^FD"       /* SR75604 */
           yy$(69%) = "67^FN63^FD"       /* CR00581 */
           yy$(70%) = "68^FN64^FD"       /* CR00581 */
           yy$(71%) = "69^FN69^FD"
           yy$(72%) = "71^FN71^FD"       /* CR1174 */
           yy$(73%) = "72^FN72^FD"       /* CR1654 */
           yy$(74%) = "73^FN73^FD"       /* CR1654 */
           yy$(75%) = "74^FN74^FD"       /* CR1940 */
           yy$(76%) = "75^FN75^FD"       /* CR1940 */
           
           yy$(77%) = "^PQ1,0,1,Y"
           yy$(78%) = "^XZ"

/*<AWD036> - */


        return

        load_image

        init(" ") yy2$()                           /* (EWD009)         */

           yy2$( 1%) = "  "
           yy2$( 2%) = "  "
           yy2$( 3%) = "  "


           yy2$( 4%) = "  "
           yy2$( 5%) = "^XA^EG"                 /* (EWD020)         */
           yy2$( 6%) = "^LH0,0"
           yy2$(07%) = "^LL610^PW1690"

           yy2$(08%) = "^JMA"



/*<AWD036> + */
           yy2$(09%) = "^FO1596,38"
           yy2$(10%) = "^GB52,114,52^FS"
           yy2$(11%) = "^CI0"
/*         yy2$(12%) = "^FO1354,181"                                   */
/*SR69520*/yy2$(12%) = "^FO1354,163"
/*         yy2$(13%) = "^GB42,105,3^FS"                                */
/*SR69520*/yy2$(13%) = "^GB42,112,3^FS"
           yy2$(14%) = "^FO1350,21"
           yy2$(15%) = "^GB0,567,2^FS"
/*         yy2$(16%) = "^FO1018,38"                                    */
/*SR69520*/yy2$(16%) = "^FO1119,166"
           yy2$(17%) = "^GB54,96,54^FS"
/*         yy2$(18%) = "^FT1023,38"                                    */
/*SR69520*/yy2$(18%) = "^FT1124,166"
           yy2$(19%) = "^AFR,52,13^FR^FDP.O.#:^FS"
/*         yy2$(20%) = "^FO1018,165"                                   */
/*CR1934*/ yy2$(20%) = "^FO1116,268"
/*CR1934*/ yy2$(21%) = "^GB58,254,58^FS"
/*         yy2$(22%) = "^FO764,38"                                     */
/*SR69520*/yy2$(22%) = "^FO853,171"
           yy2$(23%) = "^GB54,144,54^FS"
/*         yy2$(24%) = "^FT769,38"                                     */
/*SR69520*/yy2$(24%) = "^FT858,171"
           yy2$(25%) = "^AFR,52,13^FR^FDEXACT SZ:^FS"
           yy2$(26%) = "^FO132,19"
           yy2$(27%) = "^GB0,571,2^FS"
/*         yy2$(28%) = "^FO1056,556"                                   */
/*SR69520*/yy2$(28%) = "^FO1237,470"        /* CR1073   Drop*/  
           yy2$(29%) = "^GB106,160,106^FS"  /* CR1073 */
/*         yy2$(30%) = "^FO764,241"                                    */
/*SR69520*/yy2$(30%) = "^FO853,323"
/*         yy2$(31%) = "^GB54,320,54^FS"                               */
/*SR69520*/yy2$(31%) = "^GB54,288,54^FS"
           yy2$(32%) = "^FO142,203"
           yy2$(33%) = "^GB54,192,54^FS"
           yy2$(34%) = "^FO463,544"
           yy2$(35%) = "^GB40,41,3^FS"
           yy2$(36%) = "^FO513,544"
           yy2$(37%) = "^GB41,41,3^FS"
REM  ??                ^FT519,546
REM  ??                ^A0R,45,77^FDY^FS
/*         yy2$(38%) = "^FO1020,468"                                   */
/*SR69520*/yy2$(38%) = "^FO1122,530"
           yy2$(39%) = "^GB54,79,3^FS"
/*         yy2$(40%) = "^FO1119,508"                                   */
/*SR69520*/  
/* CR 1073 yy2$(40%) = "^FO1284,381" *********************************** */
/* CR 1073    yy2$(41%) = "^GB54,112,54^FS"  */
           yy2$(40%) = "^FO967,343"               /* CR1073 01 CR1174 */
           yy2$(41%) = "^GB54,64,54^FS"
           yy2$(42%) = "^FO272,267"
           yy2$(43%) = "^GB106,320,106^FS"
           yy2$(44%) = "^FT1606,216"
           yy2$(45%) = "^AFR,52,13^FDSD:^FS"
/*         yy2$(46%) = "^FT1517,40"                                    */
/*SR69520*/yy2$(46%) = "^FT1518,25"
           yy2$(47%) = "^AFR,52,13^FDTEXT:^FS"
/*         yy2$(48%) = "^FT1355,38"                                    */
/*SR69520*/yy2$(48%) = "^FT1355,25"
           yy2$(49%) = "^AFR,52,13^FDDEPT:^FS"
/*         yy2$(50%) = "^FT1355,208"                                   */
/*SR69520*/yy2$(50%) = "^FT1355,190"
           yy2$(51%) = "^AFR,52,13^FDMAKE^FS"
/*         yy2$(52%) = "^FT1355,302"                                   */
/*SR69520*/yy2$(52%) = "^FT1355,284"
           yy2$(53%) = "^AFR,52,13^FDMAKE DATE:^FS"
REM        yy2$(53%) = "^A0R,20,28^FD047391570100020005^FS"
/*         yy2$(54%) = "^FT1073,38"                                    */
/*SR69520*/yy2$(54%) = "^FT1175,178"
           yy2$(55%) = "^AFR,52,13^FDCUST:^FS"
/*         yy2$(56%) = "^FT969,38"                                     */
/*SR69520*/yy2$(56%) = "^FT1061,173"
           yy2$(57%) = "^AFR,52,13^FDS.O.#:^FS"
/*         yy2$(58%) = "^FT969,457"                                    */
/*SR69520*/yy2$(58%) = "^FT1061,495"
           yy2$(59%) = "^AFR,52,13^FDOF^FS"
/*         yy2$(60%) = "^FT820,38"                                     */
/*SR69520*/yy2$(60%) = "^FT909,171"
           yy2$(61%) = "^AFR,52,13^FDOPEN SIZE:^FS"
/*           yy2$(62%) = "^FT1302,381"           CR1174 */          /* CR1073 */
/*           yy2$(63%) = "^AFR,52,13^FDLANE:^FS"  CR1174 */        /* CR1073 */
           yy2$(64%) = "^FT207,38"
           yy2$(65%) = "^AGR,60,40^FDLOAD:^FS"
/*         yy2$(66%) = "^FT88,178"              SR65493  */
           yy2$(66%) = "^FT89,419"           /* SR65493  */
/*         yy2$(67%) = "^AFR,52,13^FDDO NOT REMOVE^FS"     SR65493 */
           yy2$(67%) = "^A0R,51,26^FDDO NOT REMOVE^FS"  /* SR65493 */
           yy2$(68%) = "^FT56,25"
           yy2$(69%) = "^ADR,36,10^FDMAKE DATE:^FS"
           yy2$(70%) = "^FT56,342"
           yy2$(71%) = "^ADR,36,10^FDWARRANTY:^FS"
           yy2$(72%) = "^FT15,25"
           yy2$(73%) = "^ADR,36,10^FDSALESORDER:^FS"
/*         yy2$(74%) = "^FO814,468"                                    */
/*SR69520  yy2$(74%) = "^FO778,455"                                    */
/*CR0978*/ yy2$(74%) = "^FO779,417"   /* CR1934*/
/*         yy2$(75%) = "^GB0,142,54^FS"                                */
/*SR69520  yy2$(75%) = "^GB0,143,54^FS"                                */
/*CR0978*/ yy2$(75%) = "^GB0,172,54^FS"
           yy2$(76%) = "^ISR:PIMAGE.GRF,N"
           yy2$(77%) = "^XZ"
 
           yy2$(78%) = "^XA"
           yy2$(79%) = "^DFR:IDATA.ZPL^FS"
           yy2$(80%) = "^CI0"
           yy2$(81%) = "^FT1597,45^ABR,44,28^FR^FN1^FS"     /* #01 N14^FS" */
/*         yy2$(82%) = "^FT1023,165^AFR,52,13^FR^FN2^FS"       #16 155-SAMPLES 2 24^FS*/
/*CR1918*/ yy2$(82%) = "^FT1125,268^A0R,56,24^FR^FN2^FS"    /* #16 155-SAMPLES 2 24^FS*/
/*         yy2$(83%) = "^FT1123,38^AFR,52,26^FN3^FS"        #13 NORANDEX^FS*/
/*SR67685  yy2$(83%) = "^FT1124,38^A0R,56,60^FN3^FS"        #13 NORANDEX^FS*/
/*SR69520*/yy2$(83%) = "^FT1239,26^A0R,56,60^FN3^FS"     /* #13 NORANDEX^FS*/
/*         yy2$(84%) = "^FT1061,556^AFR,52,13^FR^FN4^FS"        #43  01 ^FS*/
/*SR69520*/
           yy2$(84%) = "^FT972,343^AFR,52,13^FR^FN4^FS"  /* #43 01 ^FS CR1174 */
   
/*         yy2$(85%) = "^FT769,241^AFR,52,13^FR^FN5^FS"     #27 20 X 20            ^FS*/
/*SR69520*/yy2$(85%) = "^FT858,323^AFR,52,13^FR^FN5^FS" /*  #27 20 X 20            ^FS*/
           yy2$(86%) = "^FT147,203^AFR,52,26^FR^FN6^FS"     /* #44  *100%*^FS  */
           yy2$(87%) = "^FT512,546^ABR,44,28^FN7^FS"       /* #45  X^FS  */
           yy2$(88%) = "^FT463,546^ABR,44,28^FN8^FS"      /* #46  X^FS  */
/*         yy2$(89%) = "^FT1033,470^A0R,45,65^FN9^FS"        #47  SP^FS */
/*SR69520*/yy2$(89%) = "^FT1134,532^A0R,45,65^FN9^FS"     /* #47  SP^FS */
/*         yy2$(90%) = "^FT1124,522^AFR,52,13^FR^FN10^FS"        #14  00300^FS */
/*SR69520 CR1073 yy2$(90%) = "^FT1289,381^AFR,52,13^FR^FN10^FS" #14  00300^FS */
  
           yy2$(90%) = "^FT1252,470^AFR,104,26^FR^FN70^FS"     /* CR1073  */

           yy2$(91%) = "^FT287,267^AFR,104,52^FR^FN11^FS"   /* #14 00300 ^FS */
           yy2$(92%) = "^FT1607,266^AFR,52,13^FN12^FS"      /* #02 WOOD/SAMP/^FS */
           yy2$(93%) = "^FT1562,231^AFR,52,13^FN13^FS"     /*  #03 002 OF 005^FS */
/*         yy2$(94%) = "^FT1518,145^AFR,52,13^FN14^FS"      #04 X-----------X----------------X^FS*/
/*SR69520*/yy2$(94%) = "^FT1518,127^AFR,52,13^FN14^FS"   /* #04 X-----------X----------------X^FS*/
/*         yy2$(95%) = "^FT1479,145^AFR,52,13^FN15^FS"      #05 X----------------------------X^FS*/
/*SR69520*/yy2$(95%) = "^FT1479,127^AFR,52,13^FN15^FS"   /* #05 X----------------------------X^FS*/
/*         yy2$(96%) = "^FT1441,145^AFR,52,13^FN16^FS"      #06 X----------------------------X^FS*/
/*SR69520*/yy2$(96%) = "^FT1441,127^AFR,52,13^FN16^FS"   /* #06 X----------------------------X^FS*/
/*         yy2$(97%) = "^FT1403,145^AFR,52,13^FN17^FS"      #07 X------------X---------------X^FS*/
/*SR69520*/yy2$(97%) = "^FT1403,127^AFR,52,13^FN17^FS"   /* #07 X------------X---------------X^FS*/
/*         yy2$(98%) = "^FT1355,127^AFR,52,13^FN18^FS"          #08 036^FS */
/*SR69520*/yy2$(98%) = "^FT1355,114^AFR,52,13^FN18^FS"       /* #08 036^FS */
/*         yy2$(99%) = "^FT1355,461^AFR,52,13^FN19^FS"        #10  02 27 2014^FS */
/*SR69520*/yy2$(99%) = "^FT1355,444^AFR,52,13^FN19^FS"     /* #10  02 27 2014^FS */
/*         yy2$(100%) = "^FT1178,38^ABR,33,21^FN20^FS"        #11  200 SH^FS */
/*SR69520*/yy2$(100%) = "^FT1299,25^ABR,33,21^FN20^FS"     /* #11  200 SH^FS */
/*         yy2$(101%) = "^FO1237,127^BY2,2.0,98^BCR,98,Y,N,N^FR^FN21^FS"   #12  128B */
/*SR69520*/yy2$(101%) = "^FO721,39^BY2,2.0,98^BCI,98,N,N,N^FR^FN21^FS"/* #12  128B */
/*         yy2$(102%) = "^FT1073,152^AFR,52,13^FN22^FS"      #15  NORANDEX #155 (NEW^FS */
/*SR69520*/yy2$(102%) = "^FT1175,267^AFR,52,13^FN22^FS"   /* #15  NORANDEX #155 (NEW^FS */
/*         yy2$(103%) = "^FT969,165^AFR,52,13^FN23^FS"        #17  04739157^FS */
/*SR69520*/yy2$(103%) = "^FT1061,279^AFR,52,13^FN23^FS"     /* #17  04739157^FS */
/*         yy2$(104%) = "^FT969,393^AFR,52,13^FN24^FS"        #18  002^FS */
/*SR69520*/yy2$(104%) = "^FT1061,431^AFR,52,13^FN24^FS"     /* #18  002^FS */
/*         yy2$(105%) = "^FT969,546^AFR,52,13^FN25^FS"        #19  010^FS */
/*SR69520*/yy2$(105%) = "^FT1061,546^AFR,52,13^FN25^FS"     /* #19  010^FS */
/*IM7982*/
/*         yy2$(106%) = "^FT931,38^ABR,33,21^FN26^FS"    #23  157-        1            002-005^FS*/
/*SR69520*/yy2$(106%) = "^FT1020,162^ABR,33,21^FN26^FS" /* #23  157-        1            002-005^FS*/
/*         yy2$(107%) = "^FT878,38^AFR,52,26^FN27^FS"        #24  DAY-4^FS */
/*SR69520*/yy2$(107%) = "^FT969,165^AFR,52,26^FN27^FS"     /* #24  DAY-4^FS */
/*         yy2$(108%) = "^FT820,203^AFR,52,13^FN28^FS"        #26  20 1 2 X 30 1 2^FS */
/*SR69520*/yy2$(108%) = "^FT909,355^AFR,52,13^FN28^FS"     /* #26  20 1 2 X 30 1 2^FS */
/*         yy2$(109%) = "^FT782,178^AFR,52,13^FN29^FS"    #31  03 13 2014^FS */
/*CR0978*/ yy2$(109%) = "^FT785,169^AFR,52,13^FN29^FS"   /* #31  03 13 2014^FS */
           yy2$(110%) = "^FT462,508^AFR,52,13^FN30^FS"     /* #49   G^FS */
/*         yy2$(111%) = "^FT385,25^A0R,62,28^FN31^FS"    #32  N142AM690096020030001     111A0000000000100000^FS*/
/*SR69520*/yy2$(111%) = "^FT385,26^A0R,62,28^FN31^FS" /* #32  N142AM690096020030001     111A0000000000100000^FS*/
           yy2$(112%) = "^FT342,38^AFR,52,13^FN32^FS"     /* #33  ApplFin^FS */
/*         yy2$(113%) = "^FT207,330^AGR,60,40^FN33^FS"        #35  59919^FS */
/*SR66611*/yy2$(113%) = "^FT205,305^A0R,79,120^FN33^FS"     /* #35  59919^FS */
           yy2$(114%) = "^FT56,178^ADR,36,10^FN34^FS"     /* #36  02/27/2014^FS */
           yy2$(115%) = "^FT56,482^ADR,36,10^FN35^FS"     /* #37  24310394^FS */
           yy2$(116%) = "^FT15,203^ADR,36,10^FN36^FS"     /* #38  04739157^FS */
           yy2$(117%) = "^FT255,64^AGR,120,80^FN37^FS"     /* #40  S^FS */
/*SR66919  yy2$(118%) = "^FT144,406^ABR,44,14^FN38^FS"        #41  SAMPLE^FS */
/*SR66919  yy2$(118%) = "^FT148,406^A0R,56,57^FN38^FS"        #41  SAMPLE^FS */
/*SR67154*/yy2$(118%) = "^FT144,406^ABR,44,14^FN38^FS"     /* #41  SAMPLE^FS */
           yy2$(119%) = "^FT144,25^ABR,44,14^FN39^FS"     /* #42  FLORIDA^FS */
/*         yy2$(120%) = "^FT817,470^AFR,52,13^FR^FN40^FS"        #48  S 123456^FS Lowes Stock "S sku#" */
/*SR69520  yy2$(120%) = "^FT782,457^AFR,52,13^FR^FN40^FS"        #48  S 123456^FS Lowes Stock "S sku#" */
/*CR0978*/ yy2$(120%) = "^FT787,419^AFR,52,13^FR^FN40^FS"       /* #48  S 12345678^FS Lowes Stock "S sku#" */
           yy2$(121%) = "^FT15,495^ADR,36,10^FN41^FS"     /* #49  G^FS */
/*         yy2$(122%) = "^FO873,292^FR^GB^FN42^FS"     #51   */
/*SR69520  yy2$(122%) = "^FO967,457^FR^GB^FN42^FS"     #51   */
/*SR67154*/yy2$(122%) = "^FO967,457^FN42^FS"  /* Bug Fix!  #51   */
/*         yy2$(123%) = "^FT878,292^AFR,52,13^FR^FN43^FS"        #50  MULL CLIP^FS */
/*SR69520*/yy2$(123%) = "^FT972,457^AFR,52,13^FR^FN43^FS"     /* #50  MULL CLIP^FS */
           yy2$(124%) = "^FT15,546^ADR,36,10^FN44^FS"     /* #52   E^FS */
/*         yy2$(125%) = "^FT1123,393^ABR,44,14^FN45^FS"                            */
/*SR69520*/yy2$(125%) = "^FT1286,317^ABR,44,14^FN45^FS"
/*         if bay$ = "BAY" then                                                ~
              yy2$(125%) = "^FT1127,428^A0R,56,32^FN46^FS"                     ~
  SR69520     yy2$(125%) = "^FT1286,263^A0R,56,32^FN46^FS"                    */
           yy2$(126%) = "^FT1562,444^AFR,52,13^FN47^FS"     /* #39   W/F Part  ^FS */
           yy2$(127%) = "^FT617,25^AFR,52,13^FN48^FS"               /* #53   */
           yy2$(128%) = "^FT579,25^AFR,52,13^FN49^FS"               /* #55   */
           yy2$(129%) = "^FT541,25^AFR,52,13^FN50^FS"               /* #56   */
         /*  yy2$(130%) = "^FT617,330^AFR,52,13^FN51^FS"     */          /* #54   */
/*CR1313*/ yy2$(130%) = "^FT627,292^AER,56,15^FN51^FS"               /* #54   */
           yy2$(131%) = "^FT554,546^AFR,52,13^FN52^FS"               /* #57   */
           yy2$(132%) = "^FT503,25^AFR,52,13^FN53^FS"               /* #58   */
           yy2$(133%) = "^FT503,292^AFR,52,13^FN54^FS"               /* #59   */
           yy2$(134%) = "^FT465,25^AFR,52,13^FN55^FS"               /* #60   */
           yy2$(135%) = "^FT426,25^AFR,52,13^FN56^FS"               /* #61   */
           yy2_test$ = yy2$(135%)
           yy2$(136%) = "^FT426,203^AFR,52,13^FN57^FS"               /* #62   */
           yy2_test$ = yy2$(136%)
           yy2$(137%) = "^FT100,025^ADR,36,10^FN58^FS"       /*SR65493  #63   */
           yy2_test$ = yy2$(137%)
/*SR69520*/yy2$(138%) = "^FO727,161^GB487,0,3^FS"            /* Vertical Line Seperator*/
/*SR69520*/yy2$(139%) = "^FT728,178^A0R,62,48^FN59^FS"       /* Barcode Number */
/*SR67154  yy2$(140%) = "^FT664,178^A0R,62,96^FN60^FS"          SAMPLE #64     */
/*CR00581*/yy2$(140%) = "^FT673,152^A0R,51,80^FN60^FS"       /* SAMPLE #64     */
/*SR72918*/yy2$(141%) = "^FT1226,355^AFR,52,13^FN61^FS"       /* Coastal #65    */
/*SR75604*/yy2$(142%) = "^FO430,355^GB41,57,41^^FS"       /* Intercept #66    */
/*SR75604*/yy2$(143%) = "^FT435,355^A0R,39,31^FR^FN62^FS"    /* Intercept #66  */
/*CR00581*/yy2$(144%) = "^FT255,152^AGR,120,80^FR^FN63^FS"/* 2nd Alpha Char #67*/
/*CR00581*/yy2$(145%) = "^FT673,470^A0R,051,80^FR^FN64^FS" /* SDL            #68*/
           yy2$(146%) = "^FT435,431^A0R,34,46^FR^FN69^FS" /* Screen Mesh   #69*/
           yy2$(147%) = "^FO1288,317^GB62,144,62^FS"    /* CR1174 Shipment Blk*/
           yy2$(148%) = "^FT1298,317^AGR60,40^FR^FN71^FS"  /* CR1174 */
/* CR1654 add footbolt and sash limiter beside of text at top of label */
           yy2$(149%) = "^FT1429,25^AFR,52,13^FR^FN72^FS"     /* footbolt */
           yy2$(150%) = "^FT1467,25^AFR,52,13^FR^FN73^FS"     /* sash limiter */
/* CR1654 - */
/* CR1934 */
           yy2$(151%) = "^FT1399,26^A0R,34,38^FR^FN74^FS" /* PG PermLbl CR1940*/
           yy2$(152%) = "^FT1615,495^A0R,34,38^FR^FN75^FS"  /* PG Film  */           
           yy2$(153%) = "^XZ"
           yy2$(154%) = "^***"
 
           for i% = 1% to 153% step 1%             /* CR1940 */
             if str(yy2$(i%),1%, 4%) = "^***" then goto load_image_done
             if str(yy2$(i%),1%, 2%) = "  " then goto next_i
             b$ = all(hex(00))
REM          if str(yy2$(i%),1%, 3%) <> "^XA" then goto load_cont
REM          a_len% = len(yy2$(i%))
REM          yy2$(i%) = hex(7e) & str(yy2$(i%),2%,a_len%)
REM load_cont:   
             a$ = yy2$(i%)
             a_len% = len(a$)                   /* Calc Length of Data String */
             str(b$,1%,a_len%) = str(a$,1%,a_len%) /* Put into b$ Data from a$ */
             gosub print_line
        next_i
           next i%
        load_image_done
           return

        file_exists
          comp% = 2%
          hdr$ = "*** Production File Exists ***"
                                          /* (PAR003)             */
          if schema% = 1% then                                          ~
             msg$(1%) = "        The File (MFGPROD) Already Exists.      "~
                          else                                             ~
             msg$(1%) = "        The File (NEPROD) Already Exists.       "
                                          /* (PAR003)             */
          msg$(2%) = "       P r o d u c t i o n   L a b e l s         "
          msg$(3%) = "Press <RETURN> To Exit Prog, or PF(16) to Delete."
          call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
                                                      /* (PAR003)     */
        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

                                                      /* (PAR003)     */
                                                      /* (EWD018)     */
        check_customer
            init(" ") readkey$, desc$, lb_inspect$
            str(readkey$,1%,9%)   = "PLAN 100 "
            str(readkey$,10%,15%) = lb_cust_code$
            read #1,key = readkey$, using L62000, desc$, eod goto L61690
           /* <AWD008> */
           if str(desc$,27,1) <> "-"   then goto L61690
           if str(desc$,27,4) = "-999" then goto L61680
           if str(desc$,27,4) = "-000" then goto L61690
            read #1,hold,key = readkey$, using L62000, desc$, eod goto L61690
               convert str(desc$,28,3) to cnt%, data goto L61690
           cnt% = cnt% - 1%
           convert cnt% to str(desc$,28,3), pic (000)
               rewrite #1, using L62000,desc$

           /* </AWD008> */
L61680:        lb_inspect$ = "*100%*"
L61690: return

        check_mdl_100
          if lb_inspect$ <> " " then return /* already set */
          init(" ") readkey$, desc$
          str(readkey$,1%,9%)   = "PLAN 100 "
          str(readkey$,10%,15%) = str(lb_part$,1%,1%)
          read #1,key = readkey$, using L62000, desc$, eod goto mdl_100_done
            if str(desc$,30,1) <> "M" then goto mdl_100_done /* </AWD025> */
               lb_inspect$ = "*100%*"
        mdl_100_done
        return
/*CR00548 + */
        check_3digit_model
          if lb_inspect$ <> " " then return /* already set */
          init(" ") readkey$, desc$
          str(readkey$,1%,9%)   = "PLAN 100 "
          str(readkey$,10%,15%) = str(lb_part$,1%,3%)
          read #1,key = readkey$, using L62000, desc$, eod goto check_3digit_model_done
            if str(desc$,30,1) <> "M" then goto check_3digit_model_done /* </AWD025> */
               lb_inspect$ = "*100%*"
        check_3digit_model_done
        return
/*CR00548 - */
        check_color_100
          if lb_inspect$ <> " " then return /* already set */
          init(" ") readkey$, desc$
          str(readkey$,1%,9%)   = "PLAN 100 "
          str(readkey$,10%,15%) = str(lb_part$,4%,1%)
          read #1,key = readkey$, using L62000, desc$, eod goto color_100_done
            if str(desc$,30,1) <> "C" then goto color_100_done   /*<AWD034> */
              lb_inspect$ = "*100%*"
        color_100_done
        return
        
/*CR2375 */
        check_cust_brand
          init(" ") readkey$, desc$
          lblbrand% = 0%
          str(readkey$,1%,9%)   = "LBLBRAND "
          str(readkey$,10%,15%) = lb_cust_code$ 
          read #1,key = readkey$, using L62000, desc$, eod goto cust_brand_done
              lblbrand% = 1%
        cust_brand_done
        return
        
        lookup_cust                                   /* (AWD007) */
          p% = pos(lb_po$ = "/")
          if p% = 0% then return

          init(" ") so_number$
          so_number% = 0%
          str(so_number$,1,8) = str(lb_po$,p%+1,len(lb_po$))
          convert so_number$ to so_number%, data goto bad_so

          convert so_number% to str(so_number$,1,8), pic(00000000)

bad_so:

          read #4, key = so_number$, using bcklines_fmt, parent_cust$, ~
                                         eod goto lookup_cust_done

                     lb_cust_code$ = parent_cust$

bcklines_fmt:        FMT CH(09)
        lookup_cust_done
        return                                        /* (AWD007/) */

                                                      /* Check Parts  */
        check_parts
            if len(lb_part$) < 19% then goto L62005
            p% = pos("456" = lb_screen$)
            if p% <> 0% then goto L62005

            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "PLANPARTS"
            str(readkey$,10%,15%) = str(lb_part$,1%,3%)
            read #1,key = readkey$, using L62000, desc$, eod goto L62010
L62000:        FMT POS(25), CH(30)
L62005:     lb_w$ = "P"
            lb_wood$ = "Part  "
L62010: return

        get_dock
            init(" ") lb_app_key0$, lb_dock$, lb_dock1$, lb_docks$
            str(lb_app_key0$,1%,18%)  = lb_bc$
            str(lb_app_key0$,19%,2%)  = lb_drop$

            read #8, key 0% = lb_app_key0$, using L62040, lb_dock$, lb_dock1$, ~
                                         eod goto L62030

L62040:  FMT POS(577), CH(2), POS(596), CH(1)                 
           
            str(lb_docks$,2%,2%) = lb_dock$ 
            str(lb_docks$,4%,1%) = lb_dock1$
            
L62030: return
                                                      /* (EWD018)      */
                                                      /* (AWD026)      */
        lookup_mull
/*CR00581   if lb_w$ = "C" then return                (AWD009) casing */
/*          if lb_w$ = "S" then return                                */

            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "PROD MULL"
            str(readkey$,10%,15%) = lb_mull$
            read #1,key = readkey$, using L62000, desc$, eod goto L62020
/*CR00581*/ lb_w$ = str(desc$,1%,1%)
            lbl$(40%) = lb_w$                 & fs$
            lbl$(67%) = lb_w2$                & fs$

L62020: return
                                                     /* (AWD026)      */

        check_screens                                /* (AWD025)      */
                                                     /* (AWD030)      */
            lb_box1$ = " "
            lb_box2$ = " "
            p% = pos(sc1$ = lb_screen$)              /* No Screen     */
            if p% = 0% then goto L62100

               return
L62100:
            p% = pos(sc2$ = lb_screen$)              /* Half Screen   */
            if p% = 0% then goto L62200
               lb_box2$ = "X" 
               return
L62200:
            p% = pos(sc3$ = lb_screen$)              /* Full Screen   */
            if p% = 0% then return
               lb_box1$ = "X"
               lb_box2$ = "X"
        return                                       /* (AWD025)      */

        check_specials                               /* (AWD027)-Begin*/
            specials% = 0%
            if str(lb_part$,5%,3%) <> "WAR" then goto special_part
                  specials% = 1%
                  return
special_part
                                                     /* (AWD029) Glass    */
            lb_glass$ = str(lb_part$,5%,2%)
            lb_lit$   = str(lb_part$,7%,2%)

            if lb_glass$ = "89" then return          /*  Glass            */
            gosub check_glass_code
            if glass_code% = 1% then return

            if lb_lit$ = "99" or lb_lit$ = "72" or lb_lit$ = "73"         ~
                                          then return          /*  Liting */
                                                     /* (AWD029)          */
            gosub check_glass
                                                              /* (PAR004)  */
            if glass% = 0% then return
            if str(lb_oth1$,5%,1%) = "1"  then return         /* New Part  */
                                                              /* With Foam */

            p% = 0%                                           /* (PAR004)  */
            p% = pos("HJMO" = str(lb_part$,12%,1%))            /* Locks     */
            if p% <> 0% then return                           /* Brass     */

            if str(lb_oth1$,3%,1%) = "5" then return          /* Grid Color*/
                                                              /* Brass     */

            gosub check_models
            if models% = 0% then return

            specials% = 1%
        return

        check_models                               /* Put DryWall Models as Special */
            models% = 0%
               init(" ") readkey$
               str(readkey$,1%,9%)   = "PLAN DRYW"
               str(readkey$,10%,15%) = str(lb_part$,1%,3%)     /* Model  */

               read #1,key = readkey$, eod goto not_dry_wall
                    return

        not_dry_wall
            models% = 1%
        return

        check_glass
               glass% = 0%                         /* Assume a Special   */
               init(" ") readkey$
               str(readkey$,1%,9%)   = "PRICEGRID"
               str(readkey$,10%,15%) = str(lb_part$,7%,2%)      /* Grid  */

               read #1,key = readkey$, eod goto check_100
                    return                        /* Has Sash Only Grids */
check_100:
                                                  /* Check for Obscure   */
               init(" ") readkey$
               str(readkey$,1%,9%)   = "GED 001  "
               str(readkey$,10%,15%) = str(lb_part$,5%,2%)     /* Glass  */

               read #1,key = readkey$, using L62000, desc$, eod goto glass_done
                           p% = 0%
                           p% = pos(desc$ = "O")        /* Has Obscure   */
                           if p% <> 0% then return
        glass_done
               glass% = 1%
        return
                                                        /* (AWD027) - End*/
                                                        /* (AWD029)      */
        check_glass_code
            glass_code% = 0%
            init(" ") readkey$
            str(readkey$,1%,9%)   = "SPECCHECK"         /* New Table     */
            str(readkey$,10%,15%) = lb_glass$           /* Glass Code    */

             read #1,key = readkey$, eod goto check_glass_code_done
                glass_code% = 1%

        check_glass_code_done

        return                                          /* (AWD029)    */
        
/* CR1940 CR2001 */
        find_pg_prflbl
           init(" ") oradescr_key$ 
           str(oradescr_key$,1%,8%) = so_inv$                              
           str(oradescr_key$,9%,3%) = item_no$                             
           read #58, key key% = oradescr_key$, eod goto L02900
                                                                           
           get #58, using L02100, pg_series$, pg_prflbl$                          
L02100:          FMT POS(962), CH(25), CH(04)
           
L02900: return

/*AWD037 + */
        AWD037_get_gencodes

            lbl$(53%) = "   " & fs$
            lbl$(54%) = "   " & fs$
            lbl$(55%) = "   " & fs$
            lbl$(56%) = "   " & fs$
            lbl$(57%) = "   " & fs$
            lbl$(58%) = "   " & fs$
            lbl$(59%) = "   " & fs$
            lbl$(60%) = "   " & fs$
            lbl$(61%) = "   " & fs$
            lbl$(62%) = "   " & fs$
            lbl$(63%) = "   " & fs$                     /* SR65493 */
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "MODEL    "         /* New Table     */
            str(readkey$,10%,15%) = str(lb_part$,1%,3%) /* Model Code    */

            read #1,key = readkey$, using L62000, desc$,                  ~
                          eod goto gencodes1
            lbl$(53%) = str(desc$,1%,15%) & fs$
        gencodes1
            str(readkey$,1%,9%)   = "COLOR    "         /* New Table     */
            str(readkey$,10%,15%) = str(lb_part$,4%,1%) /* Color Code    */

            read #1,key = readkey$, using L62000, desc$,                  ~
                          eod goto gencodes2
            lbl$(54%) = str(desc$,1%,15%) & fs$
        gencodes2
            str(readkey$,1%,9%)   = "GLASS    "         /* New Table     */
            str(readkey$,10%,15%) = str(lb_part$,5%,2%) /* Glass Code    */

            read #1,key = readkey$, using L62000, desc$,                  ~
                          eod goto gencodes3
            lbl$(55%) = str(desc$,1%,30%) & fs$
        gencodes3
/* CR2513 */
            if len(lb_part$) < 19 then goto gencodes4  
                                    /* Part done in gencodes10 */
            if str(lb_part$,7%,2%) = "00" then goto oldlookup
            gosub setLITING
            if litingerr% = 0% then goto gencodes4
oldlookup:
            str(readkey$,1%,9%)   = "LITING   "         /* New Table     */
            str(readkey$,10%,15%) = str(lb_part$,7%,2%) /* Liting Code   */

            read #1,key = readkey$, using L62000, desc$,                  ~
                          eod goto gencodes4
            lbl$(56%) = str(desc$,1%,30%) & fs$
        gencodes4
            str(readkey$,1%,9%)   = "GRDSIZE  "         /* New Table     */
            str(readkey$,10%,15%) = str(lb_oth1$,2%,1%) /* Grid Size Cd  */

            read #1,key = readkey$, using L62000, desc$,                  ~
                          eod goto gencodes5
            lbl$(57%) = str(desc$,1%,4%) & fs$
        gencodes5
            str(readkey$,1%,9%)   = "GRDTYPE  "         /* New Table     */
            str(readkey$,10%,15%) = str(lb_oth1$,1%,1%) /* Grid Type Cd  */

            read #1,key = readkey$, using L62000, desc$,                  ~
                          eod goto gencodes6
            lbl$(58%) = str(desc$,1%,15%) & fs$
        gencodes6
            str(readkey$,1%,9%)   = "GRDCOLOR "         /* New Table     */
            str(readkey$,10%,15%) = str(lb_oth1$,3%,1%) /* Grid Color Cd  */

            read #1,key = readkey$, using L62000, desc$,                  ~
                          eod goto gencodes7
/*CR1127*/  if str(lb_oth1$,1%,1%) = "4"  then goto gencodes7
            lbl$(59%) = str(desc$,1%,15%) & fs$
        gencodes7
            str(readkey$,1%,9%)   = "HINGE    "         /* New Table     */
            str(readkey$,10%,15%) = str(lb_part$,9%,2%) /* Hinge Code    */

            read #1,key = readkey$, using L62000, desc$,                  ~
                          eod goto gencodes8
            lbl$(60%) = str(desc$,1%,30%) & fs$
        gencodes8
            str(readkey$,1%,9%)   = "LOCKS    "         /* New Table     */
            str(readkey$,10%,15%) = str(lb_part$,12%,1%) /* Lock  Code    */

            read #1,key = readkey$, using L62000, desc$,                  ~
                          eod goto gencodes9
            lbl$(61%) = str(desc$,1%,9%) & fs$
        gencodes9
            str(readkey$,1%,9%)   = "SCREEN   "         /* New Table     */
            str(readkey$,10%,15%) = str(lb_part$,11%,1%) /* Screen Code  */

            read #1,key = readkey$, using L62000, desc$,                  ~
                          eod goto add_warr_text
            lbl$(62%) = str(desc$,1%,3%) & fs$
/* SR65493 + */
 
        add_warr_text
/* CR1257 + */
            gosub lookup_intercept
            if schema% = 1% then goto NCNami
             lbl$(63%) = "NAMI-201373" & fs$     /* default to intercept */
             if intercept$ = "03" then lbl$(63%) = "NAMI-101373" & fs$  /* Duralite */
/* lookup intercept has subpart intercept set if field 17 is not 0  */
             if intercept$ = "06" then lbl$(63%) = "NAMI-301373" & fs$   /* CR2818 */
            
            goto gencodes10
NCNami:     
            lbl$(63%) = "NAMI-302187" & fs$     /* default to intercept */
            if intercept$ = "03" then lbl$(63%) = "NAMI-102187" & fs$ /* Duralite */
            
/* CR2710 Temporary drop NAMI number for superspacer */
REM  if intercept$ = "04" then lbl$(63%) = "NAMI-202187" & fs$ /* Superspacer */
            if intercept$ = "04" then lbl$(63%) = "           " & fs$ /* Superspacer */
            
/* CR2958 NC do use intercept 06 */
             if intercept$ = "06" then lbl$(63%) = "NAMI-402187" & fs$
             
/* CR1257 - */
REM  ---- old code ------            
REM            str(readkey$,1%,9%)   = "PLANARGON"         /* New Table     */
REM            str(readkey$,10%,15%) = str(lb_part$,5%,2%) /* Glass Code    */
REM
REM            read #1,key = readkey$, using L62000, desc$,                  ~
REM                          eod goto no_argon
REM            lbl$(63%) = "ALI ADW-8 ASTM 2190-ARG" & fs$
REM            if schema% = 1% then gencodes10
REM            str(lbl$(63%),9%, 1%) = "1"
REM            goto gencodes10
REM        no_argon
REM            lbl$(63%) = "ALI ADW-8 ASTM 2190    " & fs$
REM            if schema% = 1% then gencodes10
REM            str(lbl$(63%),9%, 1%) = "1"
/*SR68332 + */
        gencodes10
/* CR3319 for parts check qty in description;add to text line if none exists */        
          if len(lb_part$) >= 19 then goto AWD037_get_gencodes_done
           if lb_txt$(1%) > " " then goto L00115
           
           init(" ") oradescr_key$, hld_desc$
           w% = 0%
           str(oradescr_key$,1%,8%) = so_inv$                              
           str(oradescr_key$,9%,3%) = item_no$                             
           read #58, key key% = oradescr_key$, eod goto AWD037_get_gencodes_done
                                                                           
REM           get #58, using L00100, oradescr_rec$()                          
              get #58, using L00101, hld_desc$
L00100:          FMT 4*CH(256)  
L00101:          FMT POS(321), CH(250)                                           
L00110:          FMT PD(15,4)  
                                            
/*         get str(oradescr_rec$(),55%,8%) using L00110, widthes           ~
           get str(oradescr_rec$(),63%,8%) using L00110, heightes          ~
           convert widthes  to widthes$, pic(##0.0###)                     ~
           convert heightes to heights$, pic(##0.0###)                     ~
           lbl$(27%) = widthes$ & " X " & heightes$ & fs$                 */
           
/* CR3316 */
            w% = pos(hld_desc$ = "QTY")
            if w% = 0 then goto L00115
            lbl$(04%) = str(hld_desc$,w%,30%) & fs$
L00115:
           lbl$(53%) = str(prod_desc$,1%, 30%) & fs$
           lbl$(55%) = str(prod_desc$,31%, 30%) & fs$
           lbl$(56%) = str(prod_desc$,61%, 17%) & fs$
           lbl$(54%) = fs$
           lbl$(57%) = fs$
           lbl$(58%) = fs$
           lbl$(59%) = fs$
           lbl$(60%) = fs$
           lbl$(61%) = fs$
           lbl$(62%) = fs$

/*SR68332 - */
        AWD037_get_gencodes_done
/* SR65493 - */

        return
        
        REM *************************************************************~
            * CR2513                                                    *~
            *   Get Standard, 1 LITING or Oriel LITING information      *~ 
            *************************************************************
        setLITING
           init(" ") muttin$, vhmuttin$, lbl$(56%)
           vert% = 0% : horz% = 0% : er% = 0% : litingerr% = 0% : x% = 7%
           lits$ = "0"
/* coded to match glass label */
           if str(lb_part$,7%,2%) = "A0" then litingerr% = 1%

           if str(lb_part$,7%,2%) = "57" then goto set_mut
           if str(lb_part$,7%,1%) = "V" and (str(p_style$,1%,2%) <> "SH" and ~
              str(p_style$,1%,2%) <> "DH")                                   ~
                then goto set_mut
           
           if str(lb_part$,11%,1%) = "5" or ~
              str(lb_part$,11%,1%) = "7" then L61030
           view$ = "T"     /* TOP View */               

L61025:    call "APCGSLIT" (lb_part$,         /* MFG Part Number     */~
                            muttin$,          /* Grid Vert/Horiz Code*/~
                            lits$,            /* No. of Lits         */~
                            view$,            /* T or B              */~
                            vert%,            /* Number of Verticals */~
                            horz%,            /* Number of Horizontal*/~
                            #1,               /* (GENCODES)          */~
                            er1% )            /* Error Code          */

           if er1% <> 0% then goto L61030
           
           mtt% = pos(muttin$ = "x")
           
           convert vert% to vert$, pic(0)     
           convert horz% to horz$, pic(0)  
           vhmuttin$ = horz$ & "Hx" & vert$ & "V"           
           if mtt% = 0% then lbl$(56%) = muttin$  ~
                        else lbl$(56%) = vhmuttin$ 

L61030:           
           init(" ") muttin$, vhmuttin$
           if str(lb_part$,11%,1%) = "4" or ~
              str(lb_part$,11%,1%) = "6" then L61035
              
           view$ = "B"   /* BOTTOM View */                

           call "APCGSLIT" (lb_part$,         /* MFG Part Number     */~
                            muttin$,          /* Grid Vert/Horiz Code*/~
                            lits$,            /* No. of Lits         */~
                            view$,            /* T or B              */~
                            vert%,            /* Number of Verticals */~
                            horz%,            /* Number of Horizontal*/~
                            #1,               /* (GENCODES)          */~
                            er2% )            /* Error Code          */

           if er1% <> 0% and er2% <> 0% then litingerr% = 1%
           if er2% <> 0% then goto L61035     

           mtt% = pos(muttin$ = "x")
           if er1% = 0% then x% = 7% else x% = 1%
           if str(lbl$(56%),7%,1%) <> " " then x% = 9% 
           if str(lbl$(56%),8%,1%) <> " " then x% = 10% 
           str(lbl$(56%),x%,10%) = "-         "   /* clear space after - */
           
           convert vert% to vert$, pic(0)     
           convert horz% to horz$, pic(0)
           vhmuttin$ = horz$ & "Hx" & vert$ & "V"                 
           if mtt% = 0% then str(lbl$(56%),x%+2%,12%)  = muttin$  ~
                        else str(lbl$(56%),x%+2%,12%)  = vhmuttin$ 
                        
REM   /*TESTING*/    lbl$(56%) = lbl$(56%) & " " & lb_bc$  
           str(lbl$(56%),31%,10%) = fs$ 
           return
L61035:   
REM   /*TESTING*/    lbl$(56%) = lbl$(56%) & " " & lb_bc$            
           str(lbl$(56%),31%,10%) = fs$ 
           return
set_mut:
           sonly% = 0%
           sonly% = pos("4567" = str(lb_part$,11%,1%) )
           
           if str(lb_part$,7%,2%) = "57" then lbl$(56%) = "PERIM - PRAIRIE"
           
           if sonly% <> 0% then goto L61040
           if str(lb_part$,7%,2%) = "V0" then lbl$(56%) = "CUST_VAL - CUST_VAL"
           if str(lb_part$,7%,2%) = "V1" then lbl$(56%) = "VALA1 - VALA1"
           if str(lb_part$,7%,2%) = "V2" then lbl$(56%) = "VALA2 - VALA2"
           if str(lb_part$,7%,2%) = "V3" then lbl$(56%) = "VALA3 - VALA3"
           if str(lb_part$,7%,2%) = "V4" then lbl$(56%) = "VALA4 - VALA4"
           if str(lb_part$,7%,2%) = "V5" then lbl$(56%) = "VALA5 - VALA4"
           str(lbl$(56%),31%,10%) = fs$ 
        return 
L61040:
           if str(lb_part$,7%,2%) = "V0" then lbl$(56%) = "CUST_VAL"
           if str(lb_part$,7%,2%) = "V1" then lbl$(56%) = "VALA1"
           if str(lb_part$,7%,2%) = "V2" then lbl$(56%) = "VALA2"
           if str(lb_part$,7%,2%) = "V3" then lbl$(56%) = "VALA3"
           if str(lb_part$,7%,2%) = "V4" then lbl$(56%) = "VALA4"
           if str(lb_part$,7%,2%) = "V5" then lbl$(56%) = "VALA5"
           
           str(lbl$(56%),31%,10%) = fs$ 
        return 
            

/*AWD037 - */

/*SR75604 + */
        load_interdesc
          init(" ") gc_key$, gc_desc$, interdesc$()
          str(gc_key$,1,9) = "INTERDESC"
        interdesc_next
          read #1, key > gc_key$, using INTERDESC_FMT, gc_key$, gc_desc$, ~
                                              eod goto interdesc_done
INTERDESC_FMT:         FMT  CH(24), CH(32)
                  if str(gc_key$,1,9) <> "INTERDESC" then goto interdesc_done

                  intercept% = 9%
                  convert str(gc_key$,10,2) to intercept%,      ~
                                         data goto interdesc_done

                  if intercept% > 9% then intercept% = 9%

                  interdesc$(intercept%) = str(gc_desc$,1,5)
                  goto interdesc_next

         interdesc_done
         return

        lookup_intercept
          sub_part_nbr$ = str(bcksubpt_rec$,48%,20%)
          if str(sub_part_nbr$,17%,1%) <> "0" then goto subpart_intercept
          init(" ") readkey$, descr$, intercept$
          intercept% = 1%
          str(readkey$,1,9)  = "INTERCEPT"
          str(readkey$,10,3) = str(lb_part$,1%,3%)
          str(readkey$,13,2) = str(lb_part$,5%,2%)

           read #1, key = readkey$, using L61131, descr$,  ~
                                   eod goto no_intercept_glass
L61131:        FMT POS(25), CH(30)
               convert str(descr$,1,2) to intercept%,               ~
                                          data goto intercept_done
           goto intercept_done

        no_intercept_glass
          str(readkey$,1,9)  = "INTERCEPT"
          str(readkey$,10,3) = str(lb_part$,1%,3%)
          str(readkey$,13,2) = str(lb_part$,5%,1%) & "*"

           read #1, key = readkey$, using L61131, descr$,  ~
                                   eod goto no_intercept_all

               convert str(descr$,1,2) to intercept%,               ~
                                             data goto intercept_done
           goto intercept_done

         no_intercept_all
           init(" ") readkey$
           str(readkey$,1,9)  = "INTERCEPT"
           str(readkey$,10,3) = str(lb_part$,1%,3%)
           str(readkey$,13,2) = "**"

           read #1, key = readkey$, using L61131, descr$,  ~
                                   eod goto intercept_done

               convert str(descr$,1,2) to intercept%,               ~
                                              data goto intercept_done

        intercept_done
          convert intercept% to intercept$,pic(00)
        return

        subpart_intercept
          convert str(sub_part_nbr$,17%,1%) to intercept%,             ~
                                       data goto badsubpart_intercept
          goto intercept_done
        badsubpart_intercept
          intercept$ = "01"
        return