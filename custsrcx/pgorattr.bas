        REM *************************************************************~
            *                                                           *~
            *  Program Name      - PGORATTR                             *~
            *  Creation Date     - 10/30/2018                           *~
            *  Last Modified Date-                                      *~
            *  Written By        - Ricky Beane                          *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New Program to Build | delimited     *~
            *                      files to import into FTP.            *~
            *  Code Tables Used  -                                      *~
            *                                                           *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/11/18 ! New Program                              ! RDB *~
            * 03/01/19 ! Additions after initial testing          ! RDB *~
            * 03/06/19 ! Mull/Wood label                          ! RDB *~
            * 03/27/19 ! Corrections map, grid, size and text     ! RDB *~
            * 04/11/19 ! Changes to order text and add part number! RDB *~
            * 04/28/19 ! PG read of Oradesc fixed; apply map logic! RDB *~
            * 04/29/19 ! New logic and Glasses change for PG order! RDB *~
            * 05/01/19 ! New Plan Trip and Plan Quad              ! RDB *~
            * 05/21/19 ! Buyout Size                              ! RDB *~
            * 05/22/19 ! CR2037 add LITING to a new attribute     ! RDB *~
            * 05/31/19 ! CR2052 Skip seq# 00000 from sending      ! RDB *~
            *          !        Include buyout nominal calc       ! RDB *~
            * 06/12/19 ! CR2070 Add lot # and PG contact info     ! RDB *~
            * 06/21/19 ! CR2090 Order size type attribute         ! RDB *~
            * 02/17/20 ! CR2427 Add Room information to attributes! RDB *~
            *04/13/2020! CR1066   New DP rating for 150 Casing    ! RDB *~ 
            *04/19/2021! CR2818 New NAMI number for Texas         ! RDB *~
            *05/06/2021! CR2829 GS128 information                 ! RDB *~
            *10/18/2021! CR2930 Job size increase by Lowes        ! RDB *~
            *06/30/2023! CR3345 Add distributor fields            ! RDB *~
            *************************************************************

        dim                              /* (CATEGORY) - FILE          */~
            readkey$100,                 /* Readkey                    */~
            fields$(500%)256             /* Generic Fields             */

        dim                              /* (Program) - Variables      */~
            hdr$45, msg$(3%)79,          /* Askuser - Var's            */~
            userid$3,                    /* Current User Id            */~
            readdate$10                  /* Read date                  */
            
        dim dt_key$23,                   /* (APCPLNDT)-Tracking File   */~
            dt_ref$8,                    /* Warranty                   */~
            dt_seq$5,                    /* Sequence Number            */~
            dt_date$6, lb_dte$10,        /* Production Date            */~
            dt_part$25,                  /* Part number                */~
            dt_bar$18,                   /* Barcode                    */~
            dt_dept$3,                   /* Planned department         */~
            dt_load$5,                   /* Planned load               */~
            dt_txt$4,                    /* Line Item Text             */~
            dt_wood$3,                   /* Wood                       */~
            dt_cust$9                    /* Customer                   */
  
        dim tr_rec$256,                  /* Trigger record             */~
            tr_key$64,                   /* Trigger key #1             */~
            filetype$20,                 /* Trigger file type          */~
            transmit$1,                  /* 0 not sent, 1 sent         */~
            date$6,                      /* Trigger date               */~
            time$6,                      /* Trigger time               */~
            filetype2$20,                /* Trigger file type          */~
            fillerA$180,                 /* Trigger filler             */~
            salesorder$8,                /* Sales Order Number         */~
            linenbr$3                    /* Sales Order Line Number    */
       
        dim l1desc$250,                  /* Line text 1                */~    
            l2descA$250,                 /* Line text                  */~
            l2descB$250,                 /* Line text part 2           */~           
            l1mutype$50,                 /* L1 Mu Type                 */~
            l3mulltype$25,               /* L3 Mull Type               */~
            pgdprating$10,               /* PlyGem DP Rating           */~
            pgcpd$30,                    /* PlyGem NFRC CPD            */~
            pgseries$25,                 /* PlyGem Series              */~
            pgperlbl$4,                  /* PlyGem Performance Label   */~ 
            pgbuyout$2,                  /* PlyGem Buyout product      */~
            pgthdline$3,                 /* PlyGem The Home Depot Line */~
            pgtdi$8,                     /* PlyGem TDI number          */~
            pgfla$12                     /* PlyGem Florida Approval    */
        
        dim p_nominal$6, nominal$7,      /* Nominal Size               */~
            p_wd$3, p_ht$3,              /* Nominal width and Height   */~
            buy_part$25,                 /* JJJ buyout parts           */~
            p_part$25,                   /* Part Number                */~
            sub_part$20,                 /* Subpart number             */~
            pb_part$25,                  /* Hold area on buyout        */~
            currdte$10,                  /* Current date               */~
            upddte$6,                    /* Updated Trigger status date*/~
            updtime$6,                   /* Updated Trigger status time*/~
            itemType$10,                 /* Item Type                  */~
            bseries$20,                  /* Family / Series            */~
            text_key$11,                 /* Text File Key              */~
            text$(3%)70,                 /* Text (2) Lines             */~
            text_d$(3%)70,               /* Text descriptions          */~
            txt_room$16,                 /* Room information from txtfi*/~
            text_flag$1,                 /* Line Item Text (Y) or (N)  */~
            sav_key1$11,                 /* Save initial Key Value TEXT*/~
            filler1$245,                 /* Record filler space        */~
            L2_widthns$10,               /* WW nominal size width      */~
            L2_heightns$10,              /* WW nominal size height     */~
            L2_unit_size_code$2          /* Order Exact or Nominal     */
                  
        dim pgdate$10,                   /* Current date               */~
            pgtime$8,                    /* Current time               */~
            pgctr$8,                     /* Read DT counter            */~
            pgcompany$20,                /* Company                    */~
            pgtype$10,                   /* Order type                 */~
            pgso$8,                      /* Sales order number         */~
            pgline$3,                    /* Line number                */~
            pgattr$50,                   /* Attribute description      */~
            pg_array$(200%)50%,          /* Array of all attributes    */~
            pgvalue$125,                 /* Initial attribute value    */~
            pgvalues$(200%)125%,         /* Attribute values           */~
            pgfiller$6                   /* File filler                */
            
        dim                                                              ~
            part_desc$32,                /* Part Description           */~
            wdesc$30,                    /* Wood Surround Description  */~
            wood$3,                      /* Wood from mulling script   */~
            wood_scrpt$2,                /* Wood Surround script code  */~
            upc_code$12,                 /* Attribute values           */~
            apc_prt$60,                  /* Print Description          */~
            apc_sze$20,                  /* Attribute values           */~
            width$7,                     /* width                      */~
            height$6,                    /* Height                     */~
            apc_scr$120,                 /* Screen description         */~
            sub_scr$120,                 /* Subpart Screen description */~
            sub_prt$60,                  /* Subpart Print description  */~
            vhmuttin$25,                 /* Vertical Horizontal return */~
            s_23m$3,                     /* Model Code for Series      */~
            s_23$8                       /* New Series Code            */
            
        dim                                                               ~
            ufactor$10,                      /* UFactor                 */~
            sheat$10,                        /* Solar Heat              */~
            vtranmit$10,                     /* Visible Transmittance   */~
            cpdnumber$30,                    /* Cpd Number              */~
            warehouse$4,                     /* Warehouse               */~
            series$10,                       /* Series                  */~
            style$10,                        /* Style                   */~
            spacercode$10,                   /* SpacerCode              */~
            igthickness$10,                  /* IG THICKNESS            */~
            pane1$5,                         /* Pane1                   */~
            pane2$5,                         /* Pane2                   */~
            pane3$5,                         /* Pane3                   */~
            gridcode$5,                      /* Grid Code               */~
            gridsize$10,                     /* Grid Size               */~
            gapfill1$10,                     /* GapFill1                */~
            gapfill2$10,                     /* GapFill2                */~
            testdate$10,                     /* Calulate Day            */~
            day$1,                           /* Day of the Week         */~
            hinge$12,                        /* Hinge Description       */~
            drywall$7,                       /* Drywall                 */~ 
            seriesmull$20,                   /* Single line series & mul*/~            
            s_key$25,                        /* Bckmastr key            */~
            job_name$20,                     /* BCKMASTR job name CR2930*/~
            shipname$30,                     /* BACKMASTR shipto name   */~
            address1$30,                     /* BCKMASTR ADDRESS LINE 1 */~ 
            address2$30,                     /* BCKMASTR ADDRESS LINE 2 */~ 
            address3$30,                     /* BCKMASTR ADDRESS LINE 3 */~ 
            address4$30,                     /* BCKMASTR ADDRESS LINE 4 */~ 
            address5$30,                     /* BCKMASTR ADDRESS LINE 5 */~
            prv$15,                          /* Private Label           */~
            framecode$5,                     /* FrameCode               */~
            wd$7, ht$6,                      /* For converting Opening  */~
            sze$30,                          /* Size Long Form          */~
            stc$3,                           /* STC Rating              */~
            view$3,                          /* TOP/BOT                 */~
            specialmull$1,                   /* Special mull flag       */~
            sashcode$5                       /* SashCode                */

        dim lb_resu$4,                   /* Resident U-Factor          */~
            lb_resheat$4,                /* Resident Heat Coeff        */~
            lb_resvisible$4,             /* Resident Visible Trans     */~
            lb_dp$2,                     /* Design Pressure            */~
            lb_glass_op$24,              /* Glass option               */~
            lb_width$7,                  /* Window Width               */~
            buy_width$7,                 /* Window Width buy part      */~
            lb_height$6,                 /* Window Height              */~
            buy_height$6,                /* Window Height buy part     */~
            lb_style$30,                 /* Vinyl Window Style         */~
            lb_style2$30                 /* Vinyl Window Style         */
           
        dim eg_key$6,                    /* Primary key                */~
            eg_model$3,                  /* Model Code                 */~
            eg_group$3,                  /* Group Code                 */~
            eg_fl$12,                    /* Florida Product Approval   */~
            lb_fl$12,                    /* Design Pressure            */~
            lb_tdi$12,                   /* (AWD040) TDI Number        */~
            gen_data$30,                 /* Gencode description        */~
            eg_fill$44,                  /*                            */~
            eg_dp$2,                     /* Design Pressure            */~
            g022_tdi$12                  /* Group 022 TDI reset  CR1066*/
            
        dim bck_key$19,                  /* Bcklines                   */~
            bck_cust$9,                  /* Bcklines Customer          */~
            bck_so$16,                   /* Bcklines Sales Order       */~
            bck_group$1,                 /* Bcklines Group             */~
            bck_config$2,                /* Bcklines Config Type Code  */~
            bck_prv_code$2,              /* Bcklines Private Label     */~
            bck_conf_ln$3,               /* Bcklines WW Config Lines   */~
            prv_code$2                   /* Private Label Code         */
        
        dim                               /* Atrium NFRC data section  */~
            line1sideA$70, line2sideA$70, line3side1$70, line3side2$70,  ~       
            line4side1$70, line4side2$70, line5side1$70, line5side2$70,  ~       
            line6side1$70, line6side2$70, line7side1$70, line7side2$70,  ~       
            line8side1$70, line8side2$70, line9sideA$70 
/* CR2070 */
        dim b2_cust$9,                   /* Bckmast2 cusmter code      */~
            b2_so$16,                    /* Bckmast2 sales order number*/~
            b2_nameorderby$20,           /* Bckmast2 WW order by       */~
            b2_pg_contact$100,           /* Bckmast2 PlyGem Contact    */~
            b2_distordernbr$8,           /* Bckmast2 Dist Order Nbr    */~
            b2_custpo$22,                /* Bckmast2 Customer PO       */~
            b2_distcustpo$12,            /* Bckmast2 Dist Cust PO      */~
            lotnbr$40                    /* Lot number from WW         */
            
        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(15%)20                 /* Text from file opening     */

        dim pipe$1,                      /* Comma for EWDAPPSN File    */~
            cnt$4,                       /* File counter for key       */~
            lpcnt$4,                     /* Attribute write loop count */~
            file$8,                      /* File Name                  */~
            library$8,                   /* Library Name = APCDATA     */~
            volume$6                     /* DISK VOLUME = CARLOS       */

        dim schema$8                     /* Schema                     */     

        dim gs128$20,                    /* GS128 Barocde number CR2829*/~
            bc2_so$16,                   /* Backline2 sales order      */~
            bc2_lne$3,                   /* Backline2 sequence         */~
            shp_sep_type$3,              /* Backline2 ship separate typ*/~
            shp_line_nbr$3,              /* Backline2 ship line nbr    */~
            shp_barcode$18               /* Backline2 ship barcode     */

        REM *************************************************************

            mat f2% = con
            mat fs% = zer
            rslt$() = " "

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                     S E L E C T                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! PGORATTR ! Trigger file of Orders to process        *~
            * #2  ! APCPLNDT ! Planning Tracking File                   *~
            * #3  ! GENCODES ! Master Code Tables File                  *~  
            * #4  ! APCPCMST !                                          *~
            * #5  ! EWDPLNEX ! Energy Star Master Database              *~
            * #6  ! NFRCDATA !                                          *~
            * #7  ! NFRCMDL  !                                          *~
            * #8  ! NFRCGLS  !                                          *~
            * #9  ! TXTFILE  ! Master Text File                         *~
            * #10 ! PGORDRAT ! Ply Gem data file Order Attributes       *~
            * #11 ! BCKLINES ! S.O. Line Item Detail File               *~
            * #12 ! AMTBOMIF ! Master VALIDITY FILE                     *~
            * #13 ! BCKMASTR ! S.O. HEADER FILE                         *~
            * #14 ! CUSTOMER ! Customer Master file                     *~
            * #15 ! HNYMASTR ! Part Master File                         *~
            * #16 ! APCPLNLD ! Planning/Scheduling Load Master File     *~
            * #21 ! ORADESC2 ! WW Description File                      *~
            * #22 ! BCKSUBPT ! Sub Part File                            *~
            * #23 ! BCKMAST2 ! Additional Master data from WW           *~
            * #24 ! BCKLIN2  ! Back lines 2                             *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1, "PGORATTR",                                       ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    21, keylen =   44,                   ~
                        alt key 1, keypos = 1, keylen = 64,              ~
                            key 2, keypos = 54, keylen = 11, dup  
                   
            select #2,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 24,   keylen = 23,                      ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup
                            
            select #3, "GENCODES",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24            

            select #4,  "AWDPCMST",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    9, keylen =   53,                    ~
                        alt key  1, keypos =    1, keylen =  8
                        
            select #5,  "EWDPLNEX",                                      ~
                        varc,     indexed,  recsize =  80,               ~
                        keypos =    1, keylen =  6

            select #6,  "NFRCDATA",                                      ~
                        varc,     indexed,  recsize = 1024,               ~
                        keypos = 1,    keylen = 104,                      ~
                        alt key  1, keypos    =   1, keylen = 24, dup,    ~
                            key  2, keypos    = 105, keylen = 30, dup

            select #7,  "NFRCMDL",                                       ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 25,    keylen = 20,                     ~
                        alt key 1, keypos = 1, keylen = 44

            select #8,  "NFRCGLS",                                       ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 1,    keylen = 7
                                                                               
            select #9,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =   1, keylen =  11

/* output file for Ply Gem */                       
            select #10, "PGORDRAT",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    1, keylen = 26
                        
            select #11, "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #12, "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32
                       
            select #13, "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup
 
            select #14,  "CUSTOMER",                                     ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key 1,  keypos = 10,   keylen =  30, dup,    ~
                            key 2,  keypos = 424,  keylen =   9, dup,    ~
                            key 3,  keypos = 771,  keylen =   9, dup,    ~
                            key 4,  keypos = 780,  keylen =   9, dup,    ~
                            key 5,  keypos = 1049, keylen =   9, dup
  
            select #15,  "HNYMASTR",                                     ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup,    ~
                            key  3, keypos =   26, keylen =  32, dup

            select #16, "APCPLNLD",                                      ~
                        varc,     indexed,  recsize =   128,             ~
                        keypos =   11, keylen =  5,                      ~
                        alt key  1, keypos =  3, keylen =  13,           ~
                            key  2, keypos =  1, keylen =  15
                                    
            select #21, "ORADESC2",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =   1,   keylen = 11
                            
            select #22, "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup

            select #23, "BCKMAST2",                                      ~
                       varc,      indexed,  recsize = 1000,              ~
                       keypos  =    1, keylen =  25,                     ~
                       alt key   1, keypos =   26, keylen =  16, dup
/* CR2829 */
            select #24, "BCKLIN2",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   10, keylen =  19
                        
            filename$ = "PGORATTR" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "APCPLNDT" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "GENCODES" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "AWDPCMST" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            
            filename$ = "EWDPLNEX" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error            
            filename$ = "NFRCDATA" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "NFRCMDL"  : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "NFRCGLS"  : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "TXTFILE" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLINES"  : call "EWDOPEN" (#11, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AMTBOMIF"  : call "EWDOPEN" (#12, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKMASTR" : call "EWDOPEN" (#13, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "CUSTOMER" : call "EWDOPEN" (#14, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "HNYMASTR" : call "EWDOPEN" (#15, filename$, err%)
            if err% <> 0% then gosub open_error    
            filename$ = "APCPLNLD" : call "EWDOPEN" (#16, filename$, err%)
            if err% <> 0% then gosub open_error   
            
            call "OPENCHCK" (#21, fs%(21%), f2%(21%), 25%, rslt$(21%))   
            
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#22, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKMAST2" : call "EWDOPEN" (#23, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLIN2"  : call "EWDOPEN" (#24, filename$, err%)
            if err% <> 0% then gosub open_error
            
            mat f1% = zer
            
REM         pg_array$(XXX%)="12345678901234567890123456789012345678901234567890"
            pg_array$(1%)  ="Actual_Unit_Height                                " 
            pg_array$(2%)  ="Actual_Unit_Width                                 "
            pg_array$(3%)  ="DELIVERY_ROOM                                     "
            pg_array$(4%)  ="Frame_Color                                       "
            pg_array$(5%)  ="JAMB_TYPE                                         "
            pg_array$(6%)  ="ORDER_LINE_ITEM_TEXT1                             "
            pg_array$(7%)  ="ORDER_LINE_ITEM_TEXT2                             "
            pg_array$(8%)  ="ORDER_LINE_ITEM_TEXT3                             "
            pg_array$(9%)  ="ORDER_LINE_ITEM_TEXT4                             "
            pg_array$(10%) ="ORDER_LINE_ITEM_TEXT5                             "
            pg_array$(11%) ="PRINTABLE_CALL_SIZE                               "
            pg_array$(12%) ="UNIT_APPLIED                                      "
            pg_array$(13%) ="TOTAL_NUMBER_OF_HORIZONTAL_MULLS                  "
            pg_array$(14%) ="TOTAL_NUMBER_OF_VERTICAL_MULLS                    "
            pg_array$(15%) ="PACKAGING                                         "
            pg_array$(16%) ="UNIT_TYPE                                         "
            pg_array$(17%) ="GLASS_TYPE                                        "
            pg_array$(18%) ="IGUCERT                                           "
            pg_array$(19%) ="AWS_LABEL_ID                                      "
            pg_array$(20%) ="ENERGY_STAR_IMAGE                                 "
            pg_array$(21%) ="PRODUCTSERIES                                     "
            pg_array$(22%) ="LONG_CPD                                          "
            pg_array$(23%) ="FRAME_TYPE_DESCRIPTION                            "
            pg_array$(24%) ="GLAZE                                             "
            pg_array$(25%) ="GRID_TYPE_DESCRIPTION                             "
            pg_array$(26%) ="NFRC_GLASS_DESCRIPTION                            "
            pg_array$(27%) ="GAS_FILL_DESCRIPTION                              "
            pg_array$(28%) ="NFRC_PRODUCT_TYPE_DESCRIPTION                     "
            pg_array$(29%) ="UFACTOR                                           "
            pg_array$(30%) ="METRIC_UFACTOR                                    "
            pg_array$(31%) ="SHGC                                              "
            pg_array$(32%) ="VLT                                               "
            pg_array$(33%) ="NFRC_DP                                           "
            pg_array$(34%) ="STC_RATING                                        "
            pg_array$(35%) ="STC_RATING_NOTE1                                  "
            pg_array$(36%) ="IG_CONSTRUCTION_NOTE1                             "
            pg_array$(37%) ="IG_CONSTRUCTION                                   "
            pg_array$(38%) ="IGU_THICKNESS_FRACTION                            "
            pg_array$(39%) ="AWSFLAPPROVAL                                     "
            pg_array$(40%) ="AWSTXAPPROVAL                                     "
            pg_array$(41%) ="ATRIUM NFRC LINE 1 DATA                           "
            pg_array$(42%) ="ATRIUM NFRC LINE 2 DATA                           "
            pg_array$(43%) ="ATRIUM NFRC LINE 3A DATA                          "
            pg_array$(44%) ="ATRIUM NFRC LINE 3B DATA                          "
            pg_array$(45%) ="ATRIUM NFRC LINE 4A DATA                          "
            pg_array$(46%) ="ATRIUM NFRC LINE 4B DATA                          "
            pg_array$(47%) ="ATRIUM NFRC LINE 5A DATA                          "
            pg_array$(48%) ="ATRIUM NFRC LINE 5B DATA                          "
            pg_array$(49%) ="ATRIUM NFRC LINE 6A DATA                          "
            pg_array$(50%) ="ATRIUM NFRC LINE 6B DATA                          "
            pg_array$(51%) ="ATRIUM NFRC LINE 7A DATA                          "
            pg_array$(52%) ="ATRIUM NFRC LINE 7B DATA                          "
            pg_array$(53%) ="ATRIUM NFRC LINE 8A DATA                          "
            pg_array$(54%) ="ATRIUM NFRC LINE 8B DATA                          "
            pg_array$(55%) ="ATRIUM NFRC LINE 9 DATA                           "
            pg_array$(56%) ="SEQUENCE NUMBER                                   "
            pg_array$(57%) ="DEPARTMENT                                        "
            pg_array$(58%) ="SALES ORDER                                       "
            pg_array$(59%) ="LOAD NUMBER                                       "
            pg_array$(60%) ="MODEL                                             "
            pg_array$(61%) ="BARCODE                                           "            
            pg_array$(62%) ="PRODUCTION DATE                                   "
            pg_array$(63%) ="DP APPLIES TO WINDOWS UP TO                       "
            pg_array$(64%) ="OPENING HEIGHT                                    "
            pg_array$(65%) ="OPENING WIDTH                                     "
            pg_array$(66%) ="JOB NAME                                          "
            pg_array$(67%) ="CUSTOMER INSTRUCTIONS                             "
            pg_array$(68%) ="BRAND                                             "
            pg_array$(69%) ="SHIPPING BLOCK                                    "
            pg_array$(70%) ="MULL LABEL LINE 2                                 "
            pg_array$(71%) ="MULL LABEL LINE 3                                 "
            pg_array$(72%) ="MULL LABEL LINE 4                                 "
            pg_array$(73%) ="MULL LABEL LINE 5                                 "
            pg_array$(74%) ="MULL LABEL LINE 6                                 "
            pg_array$(75%) ="MULL LABEL LINE 7                                 "
            pg_array$(76%) ="MULL LABEL LINE 8                                 "
            pg_array$(77%) ="MULL LABEL LINE 9                                 "
            pg_array$(78%) ="MULL LABEL LINE 10                                "
            pg_array$(79%) ="ORDER_LINE_ITEM_TEXT6                             "
            pg_array$(80%) ="PART_NUMBER                                       "  
            pg_array$(81%) ="UNIT_CONFIG                                       "  
            pg_array$(82%) ="TOP_LITING                                        "            
            pg_array$(83%) ="BOTTOM_LITING                                     "
            pg_array$(84%) ="LOT_BLD                                           "
            pg_array$(85%) ="CONTACT                                           "
            pg_array$(86%) ="UNIT_SIZE_CODE                                    "    
            pg_array$(87%) ="GS128_LOWES                                       "  
            pg_array$(88%) ="DISTRIBUTOR_ORDER_NUMBER                          "
            pg_array$(89%) ="CUSTOMER_PO                                       "
            pg_array$(90%) ="DISTRIBUTOR_CUSTOMERS_PO                          "  
            pg_array$(91%) ="SHIPPING_SEPARATE_TYPE                            "  
            pg_array$(92%) ="SHIPPING_LINE_NUMBER                              "  
            pg_array$(93%) ="SHIPPING_BARCODE                                  "  
          
            pg_max% = 93%  
            
        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            
            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #3, schema_err%)  
            if schema% = 5% then schema% = 2%            

             file$   = "PGORDRAT"
             ff% = 10%   
             volume$ = "NE2"
             gosub open_file
             
        REM **** begin_files_analysis    ******
            gosub initialize_variables

            gosub read_trigger

            goto exit_program
            
              
        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") tr_key$, fields$(), filename$, hdr$, msg$(),~
                      date$, library$, volume$, readdate$, pgfiller$, ~
                      cnt$
                      
            pipe$ = "|"
            cnt% = 0%  :  dtcnt% = 0% : ldcnt% = 0%
            
            tr_key$ = all(hex(00))
 
            sze$ = "1/81/43/81/25/83/47/8         "
            sze(1%) = .1250
            sze(2%) = .2500
            sze(3%) = .3750
            sze(4%) = .5000
            sze(5%) = .6250
            sze(6%) = .7500
            sze(7%) = .8750
            sze(8%) = .9999
            
        return

        REM *************************************************************~
            * Gather data for schedule header file and write record     *~
            *************************************************************
        read_trigger

            rec% = 0%
            str(tr_key$,1%,20%) = "ORDERATTR"
            str(tr_key$,21%,1%) = "0"
            
            read #1, hold, key 1% >= tr_key$, using L01000, tr_rec$,  ~
                  eod goto read_trg_done 
L01000:       FMT CH(256)

                goto L01100
                   
        read_trg_nxt
            init(" ") fields$()
  
            rec% = 0%
            read #1, hold, eod goto read_trg_done

L01100:         cnt% = cnt% + 1%
                if cnt% >= 9999 then cnt% = 1%
                
                get #1%, using L01200, filetype$, transmit$, plndate, time$, ~
                            filetype2$, salesorder$, linenbr$, upddte$,    ~
                            updtime$, fillerA$

L01200:           FMT CH(20), CH(1), CH(6), CH(6), CH(20), CH(8), CH(3), ~
                      CH(6), CH(6), CH(180)
 
              if filetype$ <> "ORDERATTR" or transmit$ <> "0" then read_trg_done
                           
               soLne$      = linenbr$
               solne% = 0%
               convert soLne$ to solne%, data goto badLine

               convert solne% to soLne$, pic(##0)
                 
               gosub lookup_bcklines
               if prv_code$ = "18" or prv_code$ = "19" ~
                   then atrium% = 0%   ~
                   else atrium% = 1%
               gosub lookup_bcklin2                 /* CR2829 */
                   
               gosub get_planned_data
               if dtcnt% = 0% then read_trg_nxt
               
               lpcnt% = 0%
               
               for x% = 1% to pg_max%         /* pg_max for PG attribute list*/
                 pgattr$ = pg_array$(x%)
                 pgvalue$ = pgvalues$(x%)
                 convert lpcnt% to lpcnt$, pic(0000)
                 pgctr$  = cnt$ & lpcnt$
                 gosub write_upload_schd
                 lpcnt% = lpcnt% + 1%
               next x%
               if crash% = 1% then goto read_trg_nxt 
               
               delete #1
               
               str(upddte$,1%,6%) = date
               updtime$ = time
               transmit$ = "1"
               put #1, using L01200, filetype$, transmit$, plndate, time$, ~
                            filetype2$, salesorder$, linenbr$, upddte$,  ~
                            updtime$, fillerA$
               write #1, eod goto read_trg_nxt
            
               rec% = 1%            
               goto read_trg_nxt
            
        read_trg_done
               gosub write_eof
        return

badLine:
              solne% = 0%
        return
        
        REM *************************************************************~
            * Read all of the records from DT                           *~
            *************************************************************        
        get_planned_data
            init(" ") dt_key$, prev_bc$, dt_load$, dt_bar$, dt_dept$, dt_ref$,~
                      dt_seq$, dt_cust$, dt_part$
                      
            dtcnt% = 0%
            str(dt_key$,1%,8%) = salesorder$
            str(dt_key$,9%,2%) = linenbr$
            str(dt_key$,11%,8%) = "00000000"
            
        get_nxt_planned
            read #2, key > dt_key$, using L03000, dt_load$, dt_key$, ~ 
                           dt_date$, dt_ref$, dt_seq$, dt_cust$,     ~
                           dt_part$, dt_wood$, dt_txt$, ~
                                eod goto plan_done
                    
L03000:    FMT CH(5), POS(24), CH(23), POS(47), CH(6), POS(96), CH(8), ~
               POS(111), CH(5), POS(124), CH(9), POS(189), CH(25),     ~
               POS(217), CH(3), POS(236), CH(4)
            
            if str(dt_key$,1%,8%) <> salesorder$ or    ~
               str(dt_key$,9%,2%) <> str(linenbr$,1%,2%)  then plan_done

            dt_bar$ = str(dt_key$,1%,18%)
            dt_dept$ = str(dt_key$,19%,3%)

            if dt_seq$ = "00000" then goto get_nxt_planned    /* CR2052 */
            
            dtcnt% = dtcnt% + 1%  
            
            if dt_dept$ = "001" then goto get_nxt_planned
            
            if prev_bc$ = str(dt_key$, 1%, 8%) then goto get_nxt_planned
            
            gosub dept_desc
            gosub readBcksubpt
            
            gosub set_fields

            prev_bc$  = str(dt_key$, 1%, 8%)
            
            goto get_nxt_planned
            
        plan_done
        return

        REM *************************************************************~
            * Set fields for file output                                *~
            *************************************************************
        set_fields
            init(" ") pgdate$, pgtime$, pgctr$, pgcompany$,   ~
                      pgtype$, pgso$, pgline$, pgattr$, pgvalue$, ~
                      pgvalues$()
            
            currdte$  = date : call "DATFMTC" (currdte$)
            convert cnt% to cnt$, pic(0000)
            timefmt$ = time

            pgdate$ = currdte$
            pgtime$ = timefmt$

            pgcompany$ = "DALLAS"
            pgtype$ = "SO"
            pgso$ = salesorder$
               linenbr% = 0%
               convert linenbr$ to linenbr%
            convert linenbr% to pgline$, pic(##0)
            gosub load_pgvalues
            
        return
        
        REM *************************************************************~
            * Process array of information on sales order               *~
            *************************************************************
        load_pgvalues
        
          gosub get_decimal
          /*  convert height to pgvalues$(1%), pic(###.####)  */
          /*  convert width  to pgvalues$(2%), pic(###.####)  */
          
          pgvalues$(3%) = " "
          code$  = str(dt_part$,4%,1%)
          gosub color_desc
  
          pgvalues$(5%) = "N"
          
          error% = 0%  :   mull% = 0%
          call "APCAMULL" (dt_part$, mull%, wood$, wdesc$,   ~
                             wood_scrpt$, #3, error%)
          gosub readOradesc2   
          if mull% = 0% then goto L03500     /* swap description if NOT mull */
          pgvalues$(6%)  = str(l1desc$,001%,  125%)    /*changed 12012018 */
          pgvalues$(7%)  = str(l1desc$,126%,  125%)  
          pgvalues$(8%)  = str(l2descA$,001%, 125%)
          pgvalues$(9%)  = str(l2descA$,126%, 125%)
          pgvalues$(10%) = str(l2descB$,001%, 125%)
          pgvalues$(79%) = str(l2descB$,126%, 125%)  /*20190410 */
          goto L03600
L03500:          
          pgvalues$(6%)  = str(l2descA$,001%,  125%)    /*changed 12012018 */
          pgvalues$(7%)  = str(l2descA$,126%,  125%)  
          pgvalues$(8%)  = str(l2descB$,001%, 125%)
          pgvalues$(9%)  = str(l2descB$,126%, 125%)
          pgvalues$(10%) = str(l1desc$,001%, 125%)
          pgvalues$(79%) = str(l1desc$,126%, 125%) /*20190410 */
          
L03600:   pgvalues$(80%) = dt_part$

          gosub check_nominal
          pgvalues$(11%)= p_nominal$
          pgvalues$(12%)= " "

          pgvalues$(15%) = "STANDARD"   
/* CR2037 */
          gosub setLITING
          gosub processNRFCtype
/* CR2829 */
          pgvalues$(87%) = gs128$
/* CR3345 */
          pgvalues$(88%) = b2_distordernbr$
          pgvalues$(89%) = b2_custpo$ 
          call "STRING" addr("LJ", b2_distcustpo$, 12%)
          pgvalues$(90%) = b2_distcustpo$
          call "STRING" addr("LJ", shp_sep_type$, 3%)
          pgvalues$(91%) = shp_sep_type$
          call "STRING" addr("LJ", shp_line_nbr$, 3%)
          pgvalues$(92%) = shp_line_nbr$
          pgvalues$(93%) = shp_barcode$
        return
        
        REM *************************************************************~
            * Get NFRC data for Ply Gem or Atrium                       *~
            *************************************************************        
        processNRFCtype      
          
           if atrium% = 1% then goto  L04000
           
           pgvalues$(15%) = "STANDARD"    /* PACKAGING   */
           gosub NFRC_data   
           gosub prod_lbl_data
           gosub prod_mull_lbl
           pgvalues$(19%) = pgperlbl$     /* AWS_LABEL_ID */
           pgvalues$(21%) = pgseries$     /* PG Production Series */
           pgvalues$(22%) = pgcpd$        /* LONG_CPD */
           convert pgufactor to pgvalues$(29%), pic(#######0.0#####) /*UFACTOR*/
           convert pgshgc to pgvalues$(31%), pic(#######0.0#####) /* SHGC */
           pgvalues$(33%) = pgdprating$   /* NFRC_DP  */
           pgvalues$(39%) = pgfla$        /* AWSFLAPPROVAL to be sent WW */ 
           pgvalues$(41%) = "FL Prd Approval:             " & pgfla$ & ~
                            "       TDI: " & pgtdi$ /*ATRIUM NFRC LINE 1 DATA*/
           pgvalues$(42%) = "Glazing complies with ASTM E 1300 "
                      /*ATRIUM NFRC LINE 2 DATA*/
           convert pgvistrans to pgvistrans$, pic(#######0.0#####)
           pgvalues$(32%) = pgvistrans$ 
           if str(dt_part$,5%,3%) = "BUY" then gosub conv_ora_size /* 05212019*/
           
           return
L04000:            
           pgvalues$(15%) = "ATRIUM  " 
           gosub NFRC_data   
           gosub prod_lbl_data
           gosub prod_mull_lbl
                                                         
        return 

        REM *************************************************************~
            * Read BCKSUBPT file for series and other information       *~
            *************************************************************        
          readBcksubpt
              init(" ") readkey$
              str(readkey$,01%,08%) = salesorder$
              str(readkey$,09%,03%) = soLne$

              read #22, key = readkey$, using BCKSUBPT_FMT, sub_part$, ~
                              specialmull$, bseries$, itemType$, stc$, ~
                              L2_widthns$, L2_heightns$, L2_unit_size_code$, ~
                              eod goto bcksubptDone

BCKSUBPT_FMT:       FMT POS(48), CH(20), POS(152), CH(01), POS(169), CH(16), ~
                        CH(10), CH(03), POS(201), CH(10), CH(10), CH(02)

          bcksubptDone
          return

        REM *************************************************************~
            * Read the Oracle Description 2 file for text and size      *~
            *************************************************************
          readOradesc2
REM              linetypeid% = 0%
REM              pgufactor  = 0%
REM              pgshgc     = 0%
REM              pgvistrans = 0%
              init(" ") l1desc$, l2descA$, l2descB$, l1mutype$, ~
                        l3mulltype$, pgdprating$,               ~
                        pgcpd$, pgseries$, pgperlbl$,           ~
                        pgbuyout$, pgthdline$, pgtdi$, pgfla$
                        
              init(" ") readkey$
              str(readkey$,01%,08%) = salesorder$
              str(readkey$,09%,03%) = soLne$
              
              read #21, key = readkey$, using ORADESC2_FMT,                   ~ 
                              or_width, or_height, l1desc$,                   ~
                              l2descA$, l2descB$,                             ~
                              pgdprating$, pgufactor, pgshgc,                 ~
                              pgvistrans, pgcpd$, pgseries$, pgperlbl$,       ~
                              pgbuyout$, pgthdline$,                          ~
                              pgtdi$, pgfla$,                                 ~ 
                                                         eod goto oradesc2Done

ORADESC2_FMT:     FMT POS(55), PD(14,4), PD(14,4), CH(250), CH(250), CH(250), ~
                        POS(898), CH(10), PD(14,4), PD(14,4),                 ~
                        PD(14,4), CH(30), CH(25), CH(04), CH(02), CH(03),     ~
                        CH(08), CH(12)    
                                    
          oradesc2Done
          return
          
/* 05212019*/          
          conv_ora_size
                 ww% = int(or_width)                   /* Whole Width    */
                 hh% = int(or_height)                  /* Whole Height   */

                 ww1 = or_width - ww%                    /* Width Fraction */
                 hh1 = or_height - hh%                 /* Height Franction*/

                 for f0% = 1% to 8%
                     if sze(f0%) > ww1 then goto LFX
                 next f0%

LFX:             f0% = f0% - 1%
                 for f1% = 1% to 8%
                     if sze(f1%) > hh1 then goto LFY
                 next f1%

LFY:            f1% = f1% - 1%
                 convert ww% to str(lb_width$,1%,3%), pic(###)

                 convert hh% to str(lb_height$,1%,2%), pic(##)

                 str(lb_width$,4%,1%) = " "    /* Build Width with Fraction  */
                 if f0% > 0% then                                         ~
                            str(lb_width$,5%,3%) = str(sze$,(f0%*3%) - 2%, 3%)
                 str(lb_height$,3%,1%) = " "   /* Build Height with Fraction */
                 if f1% > 0% then                                         ~
                            str(lb_height$,4%,3%) = str(sze$,(f1%*3%) - 2%, 3%)
                 for i% = 1% to 7%
                    if str(lb_width$,i%,1%) = hex(00) ~
                         then str(lb_width$,i%,1%) = " "      
                 next i%                         
                 for i% = 1% to 6%
                    if str(lb_height$,i%,1%) = hex(00) ~
                         then str(lb_height$,i%,1%) = " "      
                 next i%
                             
                 pgvalues$(1%)  = lb_height$               /* EXACT HEIGHT    */
                 pgvalues$(2%)  = lb_width$                /* EXACT WIDTH     */
          return
          
        REM *************************************************************~
            * Determine the nomial size (width height)                  *~
            *************************************************************            
        check_nominal
            init(" ") nominal$, p_wd$, p_ht$, p_part$, p_nominal$
            p_err% = 0% 

            if str(dt_part$,5%,3%) = "BUY" then goto L50000
            
            /* p_part$ is a return value with the opening size */
            call "EWDNOMSZ" ("E", dt_part$, p_part$, nominal$, dt_cust$,  ~
                                                          #4, #3, p_err%)
                                                          
            if p_err% > 0%  and    ~
               L2_unit_size_code$ = "NS" then goto L49999   /* CR2090 */
   
            str(p_nominal$,1%,3%) = str(nominal$,1%,3%)
            str(p_nominal$,4%,3%) = str(nominal$,5%,3%)
            call "SPCESMSH" (p_nominal$, 0%)
            
            return
            
L49999:      
            str(p_nominal$,1%,4%) = str(L2_widthns$,1%,4%)
            str(p_nominal$,5%,4%) = str(L2_heightns$,1%,4%)
            call "SPCESMSH" (p_nominal$, 0%)              

            return 
            
/* CR2052 */
L50000:     
            if or_width <= 0.00 or or_height <= 0 then return
            
            ww% = int(or_width)                   /* Whole Width    */
            hh% = int(or_height)                  /* Whole Height   */
            convert ww% to str(buy_width$,1%,3%), pic(000)
            convert hh% to str(buy_height$,1%,2%), pic(00)
            if str(buy_width$,1%,1%) = " " then str(buy_width$,1%,1%) = "0"
            if str(buy_height$,1%,1%) = " " then str(buy_height$,1%,1%) = "0"
            
            ww1 = (or_width - ww%)  * 8%               /* Width Fraction */
            hh1 = (or_height - hh%) * 8%               /* Height Franction*/
            convert ww1 to str(buy_width$,4%,1%), pic(0)
            convert hh1 to str(buy_height$,3%,1%), pic(0)
            
            str(buy_part$,1%,10%) = dt_part$ 
            str(buy_part$,11%,2%) = "00" 
            str(buy_part$,13%,4%) = buy_width$ 
            str(buy_part$,17%,3%) = buy_height$      

            call "EWDNOMSZ" ("E", buy_part$, pb_part$, nominal$, dt_cust$,  ~
                                                          #4, #3, p_err%)
   
            str(p_nominal$,1%,3%) = str(nominal$,1%,3%)
            str(p_nominal$,4%,3%) = str(nominal$,5%,3%)
            call "SPCESMSH" (p_nominal$, 0%)  
          
        return
        
        REM *************************************************************~
            * Calculate the decimal of the actual size in part number   *~
            *************************************************************
        get_decimal
           dec% = 0%
           width, height = 0.00
           dec_w, dec_h  = 0.00
           convert str(dt_part$,13,3) to width, data goto bad_dec

           convert str(dt_part$,16,1) to dec_w, data goto bad_dec

           dec_w = round(dec_w / 8,4)
           width = width + dec_w


           convert str(dt_part$,17,2) to height, data goto bad_dec

           convert str(dt_part$,19,1) to dec_h, data goto bad_dec

           dec_h = round(dec_h / 8,4)
           height = height + dec_h

              dec% = 1%
        bad_dec
        return

        REM *************************************************************~
            *   Get department description                              *~
            *************************************************************
        dept_desc
           desc$ = "N/A"
           readkey$ = " "
           str(readkey$,1%,9%)    = "PLAN DEPT"
           str(readkey$,10%,15%)  = dt_dept$
           read #3,key = readkey$, using L60000, desc$, eod goto L60020
L60000:        FMT POS(25), CH(30)

L60020: return

        REM *************************************************************~
            *   Get color code and description                          *~
            *************************************************************
        color_desc
           pgvalues$(4%) = "NA"
           readkey$ = " "
           str(readkey$,1%,9%)    = "COLOR"
           str(readkey$,10%,15%)  = code$
           read #3,key = readkey$, using L60000, desc$, eod goto L61020

              pgvalues$(4%) = str(desc$,1%,2%)
L61020: return  

        REM *************************************************************~
            * CR2037                                                    *~
            *   Get Standard, 1 LITING or Oriel LITING information      *~ 
            *************************************************************
        setLITING
           init(" ") pgvalues$(82%), pgvalues$(83%), muttin$, vhmuttin$
           vert% = 0% : horz% = 0% : er% = 0%
           if str(dt_part$,7%,2%) <> "00" then goto L61025
              muttin$ = "        " : lits$ = "0"
             return

           view$ = "T"     /* TOP View */               

L61025:    call "APCGSLIT" (dt_part$,         /* MFG Part Number     */~
                            muttin$,          /* Grid Vert/Horiz Code*/~
                            lits$,            /* No. of Lits         */~
                            view$,            /* T or B              */~
                            vert%,            /* Number of Verticals */~
                            horz%,            /* Number of Horizontal*/~
                            #3,               /* (GENCODES)          */~
                            er% )             /* Error Code          */

           if er% <> 0% then goto L61030
           
           mtt% = pos(muttin$ = "x")
           
           convert vert% to vert$, pic(0)     
           convert horz% to horz$, pic(0)  
           vhmuttin$ = horz$ & "HX" & vert$ & "V"           
           if mtt% = 0% then pgvalues$(82%) = muttin$  ~
                        else pgvalues$(82%) = vhmuttin$ 
L61030:           
           init(" ") muttin$, vhmuttin$
           view$ = "B"   /* BOTTOM View */                

           call "APCGSLIT" (dt_part$,         /* MFG Part Number     */~
                            muttin$,          /* Grid Vert/Horiz Code*/~
                            lits$,            /* No. of Lits         */~
                            view$,            /* T or B              */~
                            vert%,            /* Number of Verticals */~
                            horz%,            /* Number of Horizontal*/~
                            #3,               /* (GENCODES)          */~
                            er% )             /* Error Code          */

           if er% <> 0% then goto L61035     

           mtt% = pos(muttin$ = "x")
                     
           convert vert% to vert$, pic(0)     
           convert horz% to horz$, pic(0)
           vhmuttin$ = horz$ & "HX" & vert$ & "V"                 
           if mtt% = 0% then pgvalues$(83%) = muttin$  ~
                        else pgvalues$(83%) = vhmuttin$ 
L61035:                 
        return 
            
        REM *************************************************************~
            *   Caculate and set all NFRC data                          *~
            *************************************************************        
        NFRC_data
 
           init(" ") warehouse$, series$, style$, spacercode$,         ~
              igthickness$, pane1$, pane2$, pane3$, gridcode$,         ~
              gridsize$, gapfill1$, gapfill2$, framecode$, sashcode$,  ~
              lb_cpdnum$

           ufactor, sheat, vtranmit, U_Fac, SHGC = 0.00
           s = 0
           sc = 0
           nc = 0
           n = 0
           map$ = "map0.bmp"

           call "AWDPLA64" (dt_part$, sub_part$, ufactor$, sheat$,       ~
                            vtranmit$, cpdnumber$, warehouse$,           ~
                            series$, style$, spacercode$, igthickness$,  ~
                            pane1$, pane2$, pane3$, gridcode$, gridsize$,~
                            gapfill1$, gapfill2$, framecode$, sashcode$, ~
                            #6, #7, #8, #3, err%)

           err% = 0%
           lb_cpdnum$ = cpdnumber$
           if atrium% = 0% then ufactor = pgufactor ~
            else convert ufactor$ to ufactor, data goto bad_ufactor

bad_ufactor:
           if atrium% = 0% then sheat = pgshgc ~
             else convert sheat$ to sheat, data goto bad_sheat

bad_sheat:
           convert vtranmit$ to vtranmit, data goto bad_vtranmit

bad_vtranmit:
           if ufactor > 0.00 then convert ufactor to lb_resu$,pic(0.00)
           if sheat > 0.00 then convert sheat to lb_resheat$, pic(0.00)
           if vtranmit > 0.00 then convert vtranmit to lb_resvisible$, pic(0.00)

           U_Fac = ufactor
           SHGC  = sheat
           gosub lookup_Door
           
REM lookupIsEnergy
           if U_Fac = 0.00 or SHGC = 0.00 then setNFRC

           if door% = 1% then goto isDoor

           if U_Fac <= 0.27 then energy_star% = 1% /* enable in 2016 */
           if U_Fac <= 0.30 and SHGC <= 0.40 then energy_star% = 1%
   
           if U_Fac =  0.30 and SHGC >= 0.42 then energy_star% = 1%
           if U_Fac =  0.29 and SHGC >= 0.37 then energy_star% = 1%
           if U_Fac =  0.28 and SHGC >= 0.32 then energy_star% = 1%
           
           if U_Fac <= 0.30 and SHGC <= 0.25 then energy_star% = 1%
           if U_Fac <= 0.40 and SHGC <= 0.25 then energy_star% = 1%
           goto setcodes

isDoor:
           if U_Fac <= 0.30 and SHGC <= 0.40 then energy_star% = 1%
           
setcodes:
           if door% = 0% then goto window

           if U_Fac > 0.30 or SHGC > 0.40 then goto copy_map   
   
           if U_Fac > 0.30 or SHGC >  0.25 then goto skip_ssc
           s = 1
           sc = 1
skip_ssc:
           if U_Fac >  0.30 or SHGC > 0.40 then goto skip_nnc
           nc = 1
           n = 1
           goto skip_nnc

window:                           
           if U_Fac <= 0.40 and SHGC <= 0.25 then s  = 1
           if U_Fac <= 0.30 and SHGC <= 0.25 then sc = 1
           if U_Fac <= 0.30 and SHGC <= 0.40 then nc = 1
           if U_Fac <= 0.27                  then n  = 1 /*enable in 2016 */
           if U_Fac  = 0.28 and SHGC >= 0.32 then n  = 1 /*enable in 2016 */
           if U_Fac  = 0.29 and SHGC >= 0.37 then n  = 1 /*enable in 2016 */
           if U_Fac  = 0.30 and SHGC >= 0.42 then n  = 1 /*enable in 2016 */
skip_nnc:
copy_map: 
           if n = 0 and nc = 0 and sc = 0 and s = 0 then map$ = "map0.bmp"
           if n = 0 and nc = 0 and sc = 0 and s = 1 then map$ = "map10.bmp"
           if n = 0 and nc = 0 and sc = 1 and s = 0 then map$ = "map7.bmp"
           if n = 0 and nc = 0 and sc = 1 and s = 1 then map$ = "map9.bmp"
           if n = 0 and nc = 1 and sc = 0 and s = 0 then map$ = "map4.bmp"
           if n = 0 and nc = 1 and sc = 1 and s = 0 then map$ = "map6.bmp"
           if n = 0 and nc = 1 and sc = 1 and s = 1 then map$ = "map8.bmp"
           if n = 1 and nc = 0 and sc = 0 and s = 0 then map$ = "map2.bmp"
           if n = 1 and nc = 1 and sc = 0 and s = 0 then map$ = "map3.bmp"
           if n = 1 and nc = 1 and sc = 1 and s = 0 then map$ = "map5.bmp"
           if n = 1 and nc = 1 and sc = 1 and s = 1 then map$ = "map1.bmp"
setNFRC:    
          gosub lookup_glass
          gosub lookup_series_style
          
          /* 20190521 set unit type from part
          pgvalues$(16%) = " "                  /* UNIT_TYPE */
          pgvalues$(81%) = "            "       /* UNIT_CONFIG */
          if str(dt_part$,11%,1%) = "4" then pgvalues$(81%) = "Top Sash Only"
          if str(dt_part$,11%,1%) = "5" then pgvalues$(81%) = "Bottom Sash Only"
          if str(dt_part$,11%,1%) = "6" then pgvalues$(81%) = "Glass Only"
          if str(dt_part$,11%,1%) = "7" then pgvalues$(81%) = "Glass Only"

          if energy_star% = 1% then pgvalues$(17%) = "LE"       /* GLASS_TYPE */
          
          gosub set_NAMI
          pgvalues$(18%) = NAMI$                /* IGUCERT    */                     
          pgvalues$(19%) = " "                  /* AWS_LABEL_ID  */                   
          pgvalues$(20%) = map$                 /* ENERGY_STAR_IMAGE  */               
          pgvalues$(21%) = lb_series$           /* PRODUCTSERIES */                  
          pgvalues$(22%) = cpdnumber$           /* LONG_CPD      */           
          pgvalues$(23%) = style$               /* FRAME_TYPE_DESCRIPTION */ 
REM        "This product is ENERGY STAR Qualified in Highlighted Regions"   
          glaze$ = "DualGlazed"
          
          gosub lookup_triple     
          pgvalues$(24%) = glaze$               /* GLAZE  */  
          
          gosub lookup_grid_desc          
          pgvalues$(25%) = grid$                /* GRID_TYPE_DESCRIPTION */           
          pgvalues$(26%) = lb_glass_op$         /* NFRC_GLASS_DESCRIPTION  */
          if atrium% = 0% and str(sub_part$,7%,1%) = "1" then   ~
              gapfill1$ = "Argon"          
          pgvalues$(27%) = gapfill1$            /* GAS_FILL_DESCRIPTION   */          
          pgvalues$(28%) = lb_style$            /* NFRC_PRODUCT_TYPE_DESCRIPT */   
          pgvalues$(29%) = lb_resu$             /* UFACTOR  */                          
          pgvalues$(30%) = " "                  /* METRIC_UFACTOR  */                 
          pgvalues$(31%) = lb_resheat$          /* SHGC */                            
          pgvalues$(32%) = lb_resvisible$       /* VLT  */       

          gosub lookup_design          
          pgvalues$(33%) = lb_dp$               /* NFRC_DP */                         
          pgvalues$(36%) = " "                  /* IG_CONSTRUCTION_NOTE1 */           
          pgvalues$(37%) = lb_glass_op$         /* IG_CONSTRUCTION */                 
          pgvalues$(38%) = igthickness$         /* IGU_THICKNESS_FRACTION */          
          pgvalues$(39%) = lb_fl$               /* AWSFLAPPROVAL  */                  
          pgvalues$(40%) = " "                  /* AWSTXAPPROVAL  */                  

          gosub check_Atrium_label_data  
          pgvalues$(34%) = stc$                 /* STC_RATING  */                     
          pgvalues$(35%) = " "                  /* STC_RATING_NOTE1 */              
          pgvalues$(41%) =  line1sideA$         
          pgvalues$(42%) =  line2sideA$
          pgvalues$(43%) =  line3side1$
          pgvalues$(44%) =  line3side2$
          pgvalues$(45%) =  line4side1$
          pgvalues$(46%) =  line4side2$
          pgvalues$(47%) =  line5side1$
          pgvalues$(48%) =  line5side2$
          pgvalues$(49%) =  line6side1$
          pgvalues$(50%) =  line6side2$
          pgvalues$(51%) =  line7side1$
          pgvalues$(52%) =  line7side2$
          pgvalues$(53%) =  line8side1$
          pgvalues$(54%) =  line8side2$
          pgvalues$(55%) =  line9sideA$
          
          lb_x$ = hex(20) & "X " & hex(20) & hex(20)
          pgvalues$(63%) =  lb_width$ & hex(22) & lb_x$ & lb_height$ & hex(22)
          call "SPCSMASH" (pgvalues$(63%))
          if str(pgvalues$(63%),2%,1%) = "X" then pgvalues$(63%) = "       "
          
          gosub lookup_text
          if len(text_d$(3%)) < 2% then                                ~
               text_d$(3%) = "NA/NA"

          pp% = pos(text_d$(3%) = "/")
          if pp% = 1% then goto nocont            /* No Contractor       */
          pgvalues$(67%) = str(text_d$(3%),1%,pp% - 1%)  /* Contractor       */
nocont:  
/* CR2427 adding the room information from the txtfile beyond the contractor */
          init(" ") txt_room$
          txt_room$ = str(text_d$(3%),pp% + 1%,16%)  /* Room from txtfile */
          pgvalues$(3%) = txt_room$
          
          gosub conv_open
          gosub lookup_part
          pgvalues$(1%)  = height$                /* EXACT HEIGHT    */
          pgvalues$(2%)  = width$                 /* EXACT WIDTH     */
          pgvalues$(64%) = ht$                    /* OPENING HEIGHT  */          
          pgvalues$(65%) = wd$                    /* OPENING WIDTH   */  
          pgvalues$(68%) = prv$                   /* BRAND           */         
 
          gosub lookup_job          
          pgvalues$(66%) = job_name$             /* JOB NAME        */  
          pgvalues$(84%) = lotnbr$               /* PG lot # CR2070 */

          gosub readBckmast2                      /* CR2070 */
          pgvalues$(85%) = b2_pg_contact$           

          gosub lookup_load
          pgvalues$(69%) = ld_shp_blk$
          
          pgvalues$(86%) = L2_unit_size_code$      /* CR2090 */
          
        return
        
        lookup_Door
          door% = 0%
          str(readkey$,1%,9%)   = "PLAN DOOR"
          str(readkey$,10%,15%) = str(dt_part$,1%,3%)
          read #3,key = readkey$, eod goto notDoor
             door% = 1%
        notDoor
        return
        
        set_NAMI
            init(" ") NAMI$ 
            gosub lookup_intercept
            if schema% = 1% then goto NCNami
            NAMI$ = "NAMI-201373" 
            if intercept$ = "03" then NAMI$ = "NAMI-101373"
            if intercept$ = "06" then NAMI$ = "NAMI-301373"   /* CR2818 */
            return
NCNami:     
            NAMI$ = "NAMI-302187"                     /* default to intercept */
            if intercept$ = "03" then NAMI$ = "NAMI-102187"  /* Duralite */
            if intercept$ = "04" then NAMI$ = "NAMI-202187"  /* Superspacer */
            
        return 
        
        lookup_intercept
         
          if str(sub_part$,17%,1%) <> "0" then goto subpart_intercept
          init(" ") readkey$, descr$, intercept$
          intercept% = 1%
          str(readkey$,1,9)  = "INTERCEPT"
          str(readkey$,10,3) = str(lb_part$,1%,3%)
          str(readkey$,13,2) = str(lb_part$,5%,2%)

           read #3, key = readkey$, using L60000, descr$,  ~
                                   eod goto no_intercept_glass

              convert str(descr$,1,2) to intercept%, data goto intercept_done
           goto intercept_done

        no_intercept_glass
          str(readkey$,1,9)  = "INTERCEPT"
          str(readkey$,10,3) = str(dt_part$,1%,3%)
          str(readkey$,13,2) = str(dt_part$,5%,1%) & "*"

           read #3, key = readkey$, using L60000, descr$,  ~      
                                  eod goto no_intercept_all

               convert str(descr$,1,2) to intercept%,  data goto intercept_done
           goto intercept_done

         no_intercept_all
           init(" ") readkey$
           str(readkey$,1,9)  = "INTERCEPT"
           str(readkey$,10,3) = str(dt_part$,1%,3%)
           str(readkey$,13,2) = "**"

           read #3, key = readkey$, using L60000, descr$,  ~
                                   eod goto intercept_done

               convert str(descr$,1,2) to intercept%, data goto intercept_done

        intercept_done
          convert intercept% to intercept$,pic(00)
        return
        
        subpart_intercept
          convert str(sub_part$,17%,1%) to intercept%, ~
                  data goto badsubpart_intercept
          goto intercept_done
        badsubpart_intercept
         str(sub_part$,17%,1%) = "0"
         goto lookup_intercept
         
        lookup_design
              init(" ") eg_key$, lb_dp$, lb_width$, lb_height$, lb_fl$, lb_tdi$
              lb_dp$ = "NA"                            /* Design Press   */
              g022_tdi$ = " "                          /* CR1066 */

              if len(dt_part$) < 19% then goto L69300
              
              model$ = str(dt_part$,1%,3%)
              gosub lookup_lamn
              
              gold% = 0%                /* cant have gold on painted colors */
REM              if str(dt_part$,4%,1%) = "I" then goto nogold
REM              if str(dt_part$,4%,1%) = "J" then goto nogold
REM              if str(dt_part$,4%,1%) = "K" then goto nogold
REM              if str(dt_part$,4%,1%) = "L" then goto nogold
REM              if str(dt_part$,4%,1%) = "M" then goto nogold
REM              if str(dt_part$,4%,1%) = "N" then goto nogold
REM              if str(dt_part$,4%,1%) = "O" then goto nogold
REM              if str(dt_part$,4%,1%) = "P" then goto nogold
              if gls_lamn% = 1% then gold% = 1%
              gosub check_gold_model
REM nogold:              
              str(eg_key$,1%,3%) = model$
              str(eg_key$,4%,3%) = "000"
              gosub lookup_data
              if eg_model% = 1% and gold% = 1%  then lb_fl$ = eg_fl$

              if schema% = 1% then gosub NC_Ratings   
              if schema% = 2% then gosub TX_Ratings
              if eg_model% = 0% then goto L69300

                 lb_dp$ = eg_dp$

                 ww% = int(eg_res)                     /* Whole Width    */
                 hh% = int(eg_nonres)                  /* Whole Height   */

                 ww1 = eg_res - ww%                    /* Width Fraction */
                 hh1 = eg_nonres - hh%                 /* Height Franction*/

                 for f0% = 1% to 8%
                     if sze(f0%) > ww1 then goto LFO
                 next f0%

LFO:             f0% = f0% - 1%
                 for f1% = 1% to 8%
                     if sze(f1%) > hh1 then goto LF1
                 next f1%

LF1:             f1% = f1% - 1%
                 convert ww% to str(lb_width$,1%,3%), pic(###)

                 convert hh% to str(lb_height$,1%,2%), pic(##)

                 str(lb_width$,4%,1%) = " "    /* Build Width with Fraction  */
                 if f0% > 0% then                                         ~
                            str(lb_width$,5%,3%) = str(sze$,(f0%*3%) - 2%, 3%)
                 str(lb_height$,3%,1%) = " "   /* Build Height with Fraction */
                 if f1% > 0% then                                         ~
                            str(lb_height$,4%,3%) = str(sze$,(f1%*3%) - 2%, 3%)
                 for i% = 1% to 7%
                    if str(lb_width$,i%,1%) = hex(00) ~
                         then str(lb_width$,i%,1%) = " "      
                 next i%                         
                 for i% = 1% to 6%
                    if str(lb_height$,i%,1%) = hex(00) ~
                         then str(lb_height$,i%,1%) = " "      
                 next i%
L69300:              
              str(eg_key$,1%,3%) = model$
              str(eg_key$,4%,3%) = "001"
              if gls_lamn% = 1% then str(eg_key$,4%,3%) = "004"  /* Lamn */
              gosub lookup_data
              if eg_model% = 1% and gold% = 1%  then lb_tdi$ = eg_fl$
              if g022_tdi$ <> " " then lb_tdi$ = g022_tdi$      /* CR1066 */
        return

        TX_Ratings
           lb_dp$ = "NA"
           txRate% = 1%
           g022_tdi$ = " "        /* CR1066 */
           str(eg_key$,1%,3%) = model$
/* CR1066 */
           if str(sub_part$,6%,1%) = "2"  and   ~
              series$ = "150"             and   ~
             (style$ = "SH"               or    ~
              style$ = "2SL") then goto TX150Casing
              
           if gls_lamn% = 1% then goto TXLamn  /* Lamn Glass */
        TX_Next
           convert txRate% to str(eg_key$,4%,3%), pic(000)
           gosub lookup_data
           if eg_model% = 0% and txRate% = 1% then return
           if eg_model% = 0% and txRate% <> 1% then goto TX_Updte
           lb_dp$ = eg_dp$

           txRate% = txRate% + 1%
           if width > eg_res or height > eg_nonres then TX_Next
        return
        TX_Updte
          eg_model% = 1%  /* previous record found */
        return
        TXLamn
          str(eg_key$,4%,3%) = "004"
          gosub lookup_data
          if eg_model% = 0% then return
          lb_dp$ = eg_dp$
        return
/* CR1066 */
        TX150Casing
          str(eg_key$,4%,3%) = "022"
          gosub lookup_data
          if eg_model% = 0% then return
          g022_tdi$ = eg_fl$
          lb_dp$ = eg_dp$
        return 
        
        NC_Ratings        /* Check Size Value from '000' lookup */
/* CR1066 */
           g022_tdi$ = " "        /* CR1066 */
           if str(sub_part$,6%,1%) = "2"  and   ~
              series$ = "150"             and   ~
             (style$ = "SH"               or    ~
              style$ = "2SL") then goto NC150Casing
           ncRate% = 20%             /* Second NC lookup value  */
           if width > eg_res or height > eg_nonres then NC_Next
           return
        NC_Next
           str(eg_key$,1%,3%) = model$
           convert ncRate% to str(eg_key$,4%,3%), pic(000)
           gosub lookup_data
           if eg_model% = 0% and ncRate% = 20% then return
           if eg_model% = 0% and ncRate% <> 20% then goto NC_Updte
           lb_dp$ = eg_dp$

           ncRate% = ncRate% + 1%
           if width > eg_res or height > eg_nonres then NC_Next
        return
        NC_Updte
          eg_model% = 1%  /* previous record found */
        return
/* CR1066 */
        NC150Casing
          str(eg_key$,4%,3%) = "022"
          gosub lookup_data
          if eg_model% = 0% then return
          g022_tdi$ = eg_fl$
          lb_dp$ = eg_dp$
        return 
        
        lookup_lamn
          gls_lamn% = 0%
          str(readkey$,1%,9%)   = "PLAN LAMN"
          str(readkey$,10%,15%) = str(dt_part$,5%,2%)
          read #4,key = readkey$, eod goto notLamn
             gls_lamn% = 1%
             
        notLamn
        return
        
        lookup_data
           eg_model% = 0%
           read #5,key = eg_key$, using L75000, eg_model$, eg_group$,~
                                        eg_res, eg_nonres, eg_dp$,   ~
                                        eg_fl$, eg_fill$,            ~
                                        eod goto L75999
                                        
L75000:     FMT CH(3),                      /* Model Code              */~
                CH(3),                      /* Group Code              */~
                PD(14,4),                   /* Redidential Value       */~
                PD(14,4),                   /* Non-Residential         */~
                CH(2),                      /* Design Pressure         */~
                CH(12),                     /* Design Pressure         */~
                CH(44)                      /* Design Pressure         */
                
           eg_model% = 1%

L75999: return  

        check_gold_model
           gold_model% = 0%
           init(" ") readkey$
           str(readkey$,1%,9%)   = "PLAN GOLD"
           str(readkey$,10%,15%) = str(dt_part$,1%,3%)
           read #3,key = readkey$, eod goto gold_done
              gold_model% = 1%
              gosub check_amaa
        gold_done
        return
        
        check_amaa
           amaa_width, amaa_height = 0.00
           init(" ") readkey$
           str(readkey$,1,3) = str(dt_part$,1%,3%)

readex:    read #5, key > readkey$, using amaa_fmt, readkey$, amaa_width,    ~
                                     amaa_height, eod goto amaa_done
           if str(readkey$,1%,3%) <> str(dt_part$,1%,3%) then goto amaa_done
           on schema% goto nfrc_nc, nfrc_tx
       nfrc_nc
           if str(readkey$,4%,3%) <> "000" and str(readkey$,4%,3%) <> "020" and~
              str(readkey$,4%,3%) <> "021" then goto readex
           goto nfrc_check
       nfrc_tx
           if str(readkey$,4%,3%) <> "001" and str(readkey$,4%,3%) <> "002" and~
              str(readkey$,4%,3%) <> "003" then goto readex
nfrc_check
  amaa_fmt:        FMT CH(06), POS(7), PD(14,4), PD(14,4)  

                 if width > amaa_width or height > amaa_height then goto readex
                    gold% = 1%
  amaa_done:
        return        
        
        return
        
        lookup_glass
            init(" ") readkey$, desc$, lb_glass_op$
            glass% = 0%
            gl$ = str(dt_part$,5%,2%)
            
            if len(dt_part$) < 19% then goto L76999        
            if gl$ = "89" or gl$ = "99" or gl$ = "00" then goto L76999        

            str(readkey$,1%,9%)  = "GLASSES  "
            str(readkey$,10%,2%) = gl$
            read #3,key = readkey$, using L60000, desc$, eod goto L76999

              lb_glass_op$ = str(desc$,1%, 24%)
              if atrium% = 0% and str(sub_part$,7%,1%) = "1" then ~
                    lb_glass_op$ = lb_glass_op$ & " Argon"

              glass% = 1%

L76999: return    

        lookup_triple
           trip% = 0%
           init(" ") gen_key$
           str(gen_key$,1%,9%)  = "PLANTRIPL"
           str(gen_key$,10%,2%) = str(dt_part$,5%,2%)
           read #3, key = gen_key$, eod goto trip_out
             trip% = 1%
             glaze$ = "TrplGlazed" 
             goto trip_out

trip_out
        return  
        
        lookup_grid_desc
                  gosub check_grid
                  grid$ = "*No Grid* "

                  if gg% = 1% then grid$ = "*5/8 Grid*"
                  if gg% = 1% and str(sub_part$,2%,1%) = "2" then ~
                                           grid$ = "*3/4 Grid*"

                  if gg% = 1% and str(sub_part$,2%,1%) = "3" then ~
                                           grid$ = "* 1  Grid*"

                  if gg% = 1% and str(sub_part$,2%,1%) = "4" then ~
                                           grid$ = "*3/8 Grid*"

                  if gg% = 6% then grid$ = "*5/8 Grid*"
                  if gg% = 6% and str(sub_part$,2%,1%) = "2" then ~
                                           grid$ = "*3/4 Grid*"

                  if gg% = 6% and str(sub_part$,2%,1%) = "3" then ~
                                           grid$ = "* 1  Grid*"

                  if gg% = 6% and str(sub_part$,2%,1%) = "4" then ~
                                           grid$ = "*3/8 Grid*"

                  if gg% = 5% then grid$ = "*SDL Grid*"

                  if gg% = 6% and str(sub_part$,2%,1%) = "3" then ~
                                           grid$ = "* 1  Grid*"

        return

        check_grid                                   /* Check for Grid  */
            gg% = 0%                                 /* No Grid         */
            init(" ") grid$
            grid$  = str(dt_part$,7%,2%)                /* Check Liting    */

            if grid$ = "00" then goto GG1            /* No Grid - Exit  */
                                                     /* Check Liting for*/
                                                     /* no Grid 1st digit*/
            p% = pos("ABCDEFGHIJKLMNOPQRSTUVWXYZ" = str(grid$,1%,1%) )
                                                     /* 1st Alpha and the*/
                                                     /* 2nd Zero-No Grid*/
            if p% <> 0% and str(grid$,2%,1%) = "0" then goto GG1

                                                     /* Check Grid Sizr    */
                                                     /* (GRDSIZE) Table    */
                                                     /* 1% = 5/8 Inch Grid */
            if str(sub_part$,2%,1%) = "1" then gg% = 1%

                                                     /* 2% = 3/4 Inch Grid */
/* (AWD022)  3/4 inch grid is less than 3/4 inch so use 5/8 */
            if str(sub_part$,2,1) = "2" then gg% = 1%

                                                     /* 3% = 1 Inch Grid   */
/* (AWD022)  1 inch grid is less than 1 inch so use 5/8 */
            if str(sub_part$,2,1) = "3" then gg% = 1%

                                                     /* 4% = 3/8 Inch Grid */
/* (AWD022)  3/8 inch grid is less than 3/ inch so use 5/8 */
            if str(sub_part$,2%,1%) = "4" then gg% = 1%


                                                     /* 5% = SDL Grid      */
            if str(sub_part$,8,2) = "11" then gg% = 5%      

/* New gg for coutour Florida windows group codes */

            if (str(dt_part$,1,1) = "F" or str(dt_part$,1,1) = "S")    ~
                and (str(sub_part$,2,1) = "3" and                ~
                     str(sub_part$,1,1) = "2") then gg% = 6%

            if (str(dt_part$,1,1) = "F" or str(dt_part$,1,1) = "S")    ~
                and (str(sub_part$,2,1) = "1" and                ~
                     str(sub_part$,1,1) = "2") then gg% = 6%

/* (AWD031) check gencodes table PLNCNTGRP table to determine */
/* if model has 3/4 inch glass, if so then 1 inch grid */
/* has group code 6                                    */
            cntgrp% = 0%
            if str(sub_part$,2,1) = "3" and                ~
                 str(sub_part$,1,1) = "2" then gosub checkPLNCNTGRP
            if cntgrp% = 1% then gg% = 6%

        return
GG1:        gg% = 0%                                 /* No Grid Found   */
        return 
              
        checkPLNCNTGRP
            init(" ") readkey$
            str(readkey$,1%,9%)   = "PLNCNTGRP"
            str(readkey$,10%,3%) = str(dt_part$,1%,3%)
            read #3,key = readkey$, eod goto noPLNCNTGRP
                  cntgrp% = 1%
        noPLNCNTGRP
        return
        
        lookup_series_style
           init(" ") readkey$, desc$, p_series$, p_style$, lb_series$, lb_style$

            if len(dt_part$) < 19% then goto L82120
                                               
            p_series$ = bseries$
            if atrium% = 0% then style$ = itemType$
            p_style$  = style$
            if atrium% = 0%  then  lb_style2$ = style$

            pp% = pos(p_series$ = " ")
            if str(p_series$,1%,1%) = " " or                           ~
               str(p_series$,1%,1%) = hex(00) then normal_series_style
            if str(p_style$,1%,1%) = " " or                           ~
               str(p_style$,1%,1%) = hex(00) then normal_series_style
    
            str(readkey$,1%,9%) = "ELLISON03"
            str(readkey$,10%,10%) = p_style$
            lb_style$ = p_style$
            lb_series$ = "SERIES: " & p_series$
            goto bcksubpt_series_style

        normal_series_style
            lb_series$ = "SERIES: " & str(p_series$,1%, pp% - 1%)

            str(readkey$,1%,9%) = "ELLISON03"
            pp2% = 12% - pp%
            lb_style2$ = str(p_series$,pp%+1%,pp2%)
            if atrium% = 0%  then  lb_style2$ = style$
            str(readkey$,10%,pp2%) = str(p_series$,pp%+1%,pp2%)
            
        bcksubpt_series_style
            read #3,key = readkey$, using L60000, desc$, eod goto L82120
            str(lb_style$,1%,24%) = "VINYL " & str(desc$,1%,18%)

            if str(p_series$,1%,4%) = "4000" or str(p_series$,1%,4%) = "4002" ~
                  then str(lb_style$,1%,24%) = "ALUMN " & str(desc$,1%,18%)
            if str(p_series$,1%,4%) = "300 " or str(p_series$,1%,4%) = "330 " ~
                  then str(lb_style$,1%,24%) = "ALUMN " & str(desc$,1%,18%)
            if str(p_series$,1%,4%) = "375 "                                 ~
                  then str(lb_style$,1%,24%) = "ALUMN " & str(desc$,1%,18%)

            if str(p_series$,1%,3%) = "85 " or str(p_series$,1%,3%) = "95 " ~
                  then str(lb_style$,1%,24%) = "ALUMN " & str(desc$,1%,18%)
            if str(p_series$,1%,3%) = "95 "                                 ~
                  then str(lb_style$,1%,24%) = "ALUMN " & str(desc$,1%,18%)
            if str(p_series$,1%,4%) = "330 " or str(p_series$,1%,5%) = "R4000"~
                  then str(lb_style$,1%,24%) = "ALUMN " & str(desc$,1%,18%)
L82120:
        return
        
        REM *************************************************************~
            *   Atrium NFRC reporting data                              *~
            *************************************************************    
      check_Atrium_label_data
      
            trip% = 0%
            gosub lookup_triple
            
            str(line1sideA$,1%,29%)  = "FL Prd Approval: " & lb_fl$ 
            str(line1sideA$,30%,30%) = "       TDI: " & lb_tdi$ 
            
                                    
            line2sideA$ = "Glazing complies with ASTM E 1300" 

            init(" ") gen_data$
            gen_key$ = "ELLISON05" & str(dt_part$,1,3) & "00"
 
            read #3, key >= gen_key$, using GENCODE, gen_key$,            ~
                                                gen_data$, eod goto no_code
                                                
GENCODE:    FMT CH(24), CH(30)
                                                
            if str(dt_part$,1,3) <> str(gen_key$,10,3) then goto no_code
            goto yes_code
            
no_code:    pp% = pos(series$ = " ")
            gen_key$ = "FLORIDACD" & str(lb_style2$,1%,10%) 
            goto lookup_flcd   

yes_code:   x = 0.0
            y = 0.0
            for l = 1 to 16
               if str(gen_data$,l,1) = " " and x > 0 then y = l
               if str(gen_data$,l,1) = " " and x = 0 then x = l
               if x > 0 and y > 0 then l = 16
            next l
            gen_key$ = "FLORIDACD" & str(gen_data$,x+1,y - x)

lookup_flcd
            init(" ") gen_data$
            read #3, key = gen_key$, using GENCODE, gen_key$,              ~
                                                gen_data$, eod goto no_flcode
no_flcode:
            x = 1
            y = 1
            for l = 15 to 1 step -1
              x = l
              if str(gen_data$,l,1) > " " then l = 1
            next l
            for l = 30 to 16 step -1
              y = l - 15
              if str(gen_data$,l,1) > " " then l = 16
            next l

            line3side1$ = str(gen_data$,01,x) & " Glazing"   
            line3side2$ = str(gen_data$,16,y) & " Glazing" 
            if str(gen_data$,16%,y) = " " then line3side2$ = line3side1$
            if str(dt_part$,11,1) = "4" then goto skipGenClear1
            if str(gen_data$,1,15) = "              " then         ~
                     line3side1$ = " "
skipGenClear1:

            if str(dt_part$,11,1) = "5" or str(dt_part$,11,1) = "6" then ~
               skipGenClear2                     
            if str(gen_data$,16,y) = "              " then         ~
                     line3side2$ = " "                   
skipGenClear2:
                     
            line4side1$ = "Single-Strength Annealed" 
            line5side1$ = "Airspace" 
            line5side2$ = "Airspace" 

            lamn% = 0%
            line7side1$ = " "
            line7side2$ = " "
            line8side1$ = " "
            line8side2$ = " "
            if trip% = 1% then line7side1$ = "Airspace" 
            if trip% = 1% then line7side2$ = "Airspace" 

            gen_key$ = "PLAN DBLE" & str(dt_part$,5,2)
            read #3, key = gen_key$, using GENCODE, gen_key$,              ~
                                  gen_data$, eod goto skip_double

            line4side1$ = "Double-Strength Annealed" 
skip_double:

            gen_key$ = "PLAN LAMN" & str(dt_part$,5,2)
            read #3, key = gen_key$, using GENCODE, gen_key$,              ~
                                  gen_data$, eod goto skip_lamn
            lamn% = 1%
            line4side1$ = "Double-Strength Annealed" 
skip_lamn:
  
            gen_key$ = "PLAN TEMP" & str(dt_part$,5,2)
            read #3, key = gen_key$, using GENCODE, gen_key$,              ~
                                  gen_data$, eod goto skip_tempered

            line4side1$ = "Double-Strength Tempered" 
skip_tempered:

           init(" ") gen_key$
           str(gen_key$,1%,9%)  = "PLAN TRIP"
           str(gen_key$,10%,2%) = str(dt_part$,5%,2%)
           read #3, key = gen_key$, eod goto skip_trip1

            line4side1$ = "Triple-Strength"
skip_trip1:
           init(" ") gen_key$
           str(gen_key$,1%,9%)  = "PLAN QUAD"
           str(gen_key$,10%,2%) = str(dt_part$,5%,2%)
           read #3, key = gen_key$, eod goto skip_quad

            line4side1$ = "Quad-Strength"

skip_quad:

            if trip% = 1% then line8side1$ = line4side2$
            if trip% = 1% then line8side2$ = line4side2$

            line4side2$ = line4side1$
            if lamn% = 0% then line6side2$ = line4side2$
            if lamn% = 0% then line6side1$ = line4side2$
            if lamn% = 1% then line6side2$ = "Laminated Annealed" 
            if lamn% = 1% then line6side1$ = "Laminated Annealed" 

            if str(line3side2$,1,3) <> " " then goto not_blank
            line4side2$ = " "
            line6side2$ = " "
            line5side2$ = " "
            line7side2$ = " "
            line8side2$ = " "

not_blank:  if str(line3side1$,1,3) <> " " then goto not_blank2
            line4side1$ = " "
            line6side1$ = " "
            line5side1$ = " "
            line7side1$ = " "
            line8side1$ = " "
            
not_blank2: if str(dt_part$,11,1) <> "4" then goto not_4
            line3side2$ = " "
            line4side2$ = " "
            line5side2$ = " "
            line6side2$ = " "
            line7side2$ = " "
            line8side2$ = " "            
        goto not_5_6

not_4:      if str(dt_part$,11,1) <> "5" and                          ~
               str(dt_part$,11,1) <> "6" then goto not_5_6
            line3side1$ = " "
            line4side1$ = " "
            line5side1$ = " "
            line6side1$ = " "
            line7side1$ = " "
            line8side1$ = " "

not_5_6:

        if str(dt_part$,5,2) <> "AZ" then goto notSingleGlaze
            line5side1$ = " "
            line6side1$ = " "
            line7side1$ = " "
            line8side1$ = " "

            line5side2$ = " "
            line6side2$ = " "
            line7side2$ = " "
            line8side2$ = " "
            
notSingleGlaze:

        init (" ") oitc$ : lb_stc$ = "   " : lb_stc% = 0 
           line9sideA$ = " "
           if stc$ <= " " then goto no_apr
           if str(stc$,1%,1%) = "0" then goto no_apr
           for stci% = 1 to 3
               if str(stc$,stci%,1%) = " " then goto next_stci
               lb_stc% = lb_stc% + 1
               str(lb_stc$,lb_stc%,1%) = str(stc$,stci%,1%)
next_stci: next stci%
           stc$ = lb_stc$
           oitc$ = "28"
           line9sideA$ = "STC: (" & stc$ & ")" & "  OITC: (" & oitc$ & ")" &  ~
                     " EWR: (  )" & " "
no_apr:
       return  
       
       lookup_job
              init(" ") s_key$, job_name$, lotnbr$
              str(s_key$,1%,9%)   = dt_cust$
              str(s_key$,10%,16%) = salesorder$
              read #13,key = s_key$, using bckmstr, shipname$, address1$,    ~
                     address2$, address3$, address4$, address5$, job_name$,  ~
                                           eod goto no_job
/* CR2930 job name 16 to 20 char */                                           
bckmstr:          FMT POS(42), 6*CH(30), POS(619), CH(20)  
                lotnbr$ = address4$
no_job: return

/* CR2070 */
            readBckmast2
              
              init(" ") readkey$, pg_contact$, b2_distcustpo$, ~
                 b2_distordernbr$, b2_custpo$
              str(readkey$,01%,09%) = dt_cust$
              str(readkey$,10%,16%) = salesorder$
              
/* CR3345 new fields */
              read #23, key = readkey$, using BCKMAST2_FMT, b2_cust$,   ~
                         b2_so$, b2_nameorderby$, b2_pg_contact$,       ~
                         b2_distordernbr$, b2_custpo$, b2_distcustpo$,  ~
                            eod goto bckmast2Done

BCKMAST2_FMT:       FMT CH(09), CH(16), CH(20), CH(100), CH(08), CH(22), CH(12)

            bckmast2Done
            return
            
        REM *************************************************************~
            *   Production NFRC label data found at the bottom          *~
            *************************************************************        
        prod_lbl_data
            pgvalues$(56%) =  dt_seq$
            pgvalues$(57%) =  dt_dept$            
            pgvalues$(58%) =  salesorder$
            pgvalues$(59%) =  dt_load$
            pgvalues$(60%) =  str(dt_part$,1%,3%)
            pgvalues$(61%) =  str(dt_bar$,1%,8%)  & "-" & str(dt_bar$,9%,2%) ~
                              & "-" & str(dt_bar$,11%,4%) & "-" &             ~
                              str(dt_bar$,15%,4%)
            lb_dte$ = dt_date$
            call "DATFMTC" (lb_dte$)                              
            pgvalues$(62%) =  lb_dte$      
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
            textid$ = dt_txt$
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
   
        lookup_part                           /* Check HNYMASTR        */
               init(" ") part_desc$, upc_code$, apc_prt$, apc_sze$,      ~
                      width$, height$, apc_scr$, sub_scr$, sub_prt$

               if len(dt_part$) > 18% then goto mfg_part
                  part_desc$, apc_prt$ = "Component Part"
                  gosub lookup_text
                  if text_flag$ = "Y" then part_desc$,apc_prt$=text_desc$
                  width$, height$ = "N/A"
                  goto mfg_series
        mfg_part
               read #15,key = dt_part$,using L63540,part_desc$, upc_code$,~
                                    apc_prt$, apc_sze$, eod goto nonstock
L63540:           FMT XX(25), CH(32), POS(566), CH(12), POS(606), CH(60),~
                      CH(20)
               goto mfg_size
        nonstock

               call "AWDDESCR" (dt_part$, sub_part$, apc_scr$, apc_prt$,~
                                   sub_scr$, sub_prt$, apc_sze$, #12, err% )
               str(part_desc$,1%,16%)  = str(apc_prt$,1%,16%)
               str(part_desc$,17%,16%) = str(apc_sze$,1%,16%)

        mfg_size
            width$  = str(apc_sze$,1%,7%)
            height$ = str(apc_sze$,11%,6%)
        mfg_series
            s_23% = 0%
            s_23m$ = str(dt_part$,1%,3%)
            s_so$  = salesorder$                        
            s_ln$  = linenbr$
            init(" ") s_prv$, prv$, s_1$
            prv% = 1%                                /* Use BCKLINES   */
                                                     
            call "APCPRZSB" (prv%, s_1$, dt_cust$, s_23m$, s_so$,    ~
                                   s_ln$, s_prv$, s_23$, s_23%,          ~
                                   #14, #3, #2, #11, x_er% )

            prv$ = str(s_prv$,1%,15%)                /* Private Label  */

            if x_er% <> 0% then return
               if len(dt_part$) < 18% then return
                  str(part_desc$,1%,8%) = s_23$      /* Series Code    */
                  str(apc_prt$,1%,8%)   = s_23$

        return                                      
        
        conv_open /* Convert Standard Width/Height to Fraction in 8'ths*/
                  /* F0% = Width Fraction, F1% = Height Fraction       */
                  /* WD$ = Width & Fraction, HT$ = Height & Fraction   */
           sze$ = "1/81/43/81/25/83/47/8         "

           init(" ") wd$, ht$
           if str(p_part$,1%,3%) <> "003" then goto L64795            
              wd$ = str(p_part$,13%,4%)
              ht$ = str(p_part$,17%,3%)
              return
                                                         
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

        lookup_load                          /* (APCPLNLD) - Load File */

            read #16,key = dt_load$, using L71600, ld_shp_blk$, ~
                                  eod goto lookup_load_done  
L71600:       FMT POS(121), CH(3)

        lookup_load_done
        return
   
        lookup_bcklines
             init(" ") prv_code$, bck_key$
             str(bck_key$,1%,16%) = salesorder$
             str(bck_key$,17%,3%) = soLne$

             read #11, key = bck_key$, using L78001, bck_cust$, bck_so$, ~
                   bck_group$, bck_config$, bck_prv_code$, bck_conf_ln$, ~
                      eod goto L78000
     
L78001:      FMT CH(09), CH(16), POS(279), CH(01), CH(02), CH(02), CH(03)

             if str(bck_so$,1%,8%) <> salesorder$  then goto L78000
             
             prv_code$ = bck_prv_code$   /* private label code */
L78000:
        return
   
        lookup_bcklin2
             init(" ") prv_code$, bck_key$, gs128$, shp_sep_type$, ~
                   shp_line_nbr$, shp_barcode$
                   
             str(bck_key$,1%,16%) = salesorder$
             str(bck_key$,17%,3%) = soLne$
             
/* CR3345 new distributor shipping information */
             read #24, key = bck_key$, using L78005, bc2_so$, bc2_lne$, ~
                   gs128$, shp_sep_type$, shp_line_nbr$, shp_barcode$,  ~
                      eod goto L78006
     
L78005:      FMT POS(10), CH(16), CH(03), POS(39), CH(20), CH(03), CH(03), ~
                 CH(18)

             if str(bck_key$,1%,8%) <> salesorder$  then goto L78006
             if str(bck_key$,17%,3%) <> so_lne$     then goto L78006
             
L78006:
        return
        
        REM *************************************************************~
            * Write the mull/wood label data                            *~
            *************************************************************
        prod_mull_lbl

            if mull% = 0% then return
            
            if str(sub_part$,13%,1%) = "1" then ~
               pgvalues$(70%) = "NEW SILL"              /* New Sill */
            if str(sub_part$,13%,1%) = "1" then ~
               pgvalues$(71%) = "ANGLE"                 /* Angle    */

            pgvalues$(72%) = wdesc$                     /* Wood Surround Code */
            
            pgvalues$(73%) = str(dt_bar$,6%,3%) & " - " & bck_conf_ln$  
                                    /* Family Number */

            day% = 9%
            testdate$ =  str(dt_date$,1%,6%)
            call "DATUFMTC" (testdate$)
            call "DAY" addr(str(testdate$,,6%), day%)
            day% = day% - 1%
            if day% = 0% then day% = 7%
            convert day% to day$, pic(#)
            pgvalues$(74%) = "DAY " & day$              /* Production Day Nbr */

            gosub lookup_drywall     
            pgvalues$(75%) = drywall$                   /* Drywall  */
            if specialmull$ = "C" then pgvalues$(75%) = "CASING"
            if str(sub_part$,1%,1%) = "4" then pgvalues$(75%) = "SDL"
            if specialmull$ = "C" and pgvalues$(75%) = "SDL" ~
                            then pgvalues$(75%) = "CAS/SDL"
            
            init(" ") seriesmull$ 
            if str(sub_part$,6%,1%) = "3"    then seriesmull$ = "MULL CLIP" 
            
            if str(bseries$,1%,4%)  = "35  " then seriesmull$ = "1 TUBE"  
            if str(bseries$,1%,4%)  = "130 " then seriesmull$ = "1-1/4TUBE"               
            pgvalues$(76%) = seriesmull$                /* Mull clip Series */
            
            gosub lookup_hinge
            pgvalues$(77%) = hinge$                     /* Hinge Code  */

            if bck_group$ <= "0" then goto no_ws_data
            if str(bck_config$,2,1) <= "0" then goto no_ws_data
            pgvalues$(78%) = str(bck_config$,2,1) & "," & bck_group$     
                                  /* config type code, group  */ 
no_ws_data:
            
        return

        lookup_hinge
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)  = "HINGE    "
            str(readkey$,10%,3%) = str(dt_part$,9%,2%)
            read #3,key = readkey$,using L60000, desc$, eod goto L78400

            p% = 0%
            p% = pos(desc$ = "-")
            hinge$ = str(desc$,1%, p%-1%)

L78400:    return        


        lookup_drywall                           
          init(" ") readkey$, drywall$
          str(readkey$,1%,9%)  = "PLAN DRYW"
          str(readkey$,10%,3%) = str(dt_part$,1%,3%)             
          read #3,key = readkey$,using L60000, desc$, eod goto L78500
            drywall$ = "DRYWALL"
L78500:   return                                  

        REM *************************************************************~
            * Write the Schedule Header record                          *~
            *************************************************************
        write_upload_schd
            crash% = 0%
            if pgcompany$ <> "DALLAS" then goto L79500
            write #10, using L08000, pgdate$, pgtime$, pgctr$, pipe$,  ~        
                          pgcompany$, pipe$,  ~
                          pgtype$, pipe$,     ~
                          pgso$, pipe$,       ~
                          pgline$, pipe$,     ~
                          pgattr$, pipe$,     ~
                          pgvalue$, pipe$,    ~
                          pgfiller$, pipe$,   ~
                                  eod goto crash

                  ldcnt%  = ldcnt%  + 1

L79500:   return                                  
 
crash:  
        crash% = 1%
        return
           
L08000:          FMT CH(10), CH(8), CH(8), CH(1),                             ~
                     CH(20), CH(1), CH(10), CH(1),                            ~
                     CH(8), CH(1), CH(3), CH(1), CH(50),                      ~
                     CH(1), CH(125), CH(1), CH(06), CH(1)
           
             /* date                           10/11/2018  */
             /* time                           14153311    */
             /* read so counter                ########    */
             /* company                        DALLAS      */
             /* order_type                     SO          */
             /* order_number                   09004546    */
             /* line_number                    1           */
             /* attribute                                  */
             /* attr_value                                 */
      
        REM *************************************************************~
            *                        E R R O R                          *~
            *************************************************************
        open_error
           hdr$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = "(Open Error) - File = " & filename$
           msg$(3%) = "Press Any Key To Continue."
        return

        REM *************************************************************~
            * Open file in special library                              *~
            *************************************************************
        open_file
            init(" ") library$, volume$
            library$        = "ORAFILES"
            volume$ = "NE2"                                            
            if schema% = 1% then volume$ = "CARLO2"

             open nodisplay #ff%,  output, space = 500%,                 ~
                dpack   = 500%, ipack = 500%, file = file$,              ~
                library = library$, volume = volume$, blocks = 30%
        return

        REM *************************************************************~
            * Write the End of File counter                             *~
            *************************************************************
        write_eof
            convert ldcnt%  to wcnt$, pic(000000)
            EOF$ = "@EOF#"  
            init(" ") filler1$ 
            write #10, using L90000, EOF$, wcnt$, filler1$, eod goto crash
            
L90000:       FMT CH(5), CH(6), CH(245)
        return 
        
        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************~

        exit_program
               call "ALLFREE"
        end
