        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN03                             *~
            *  Creation Date     - 03/14/03                             *~
            *  Last Modified Date- 04/14/2019                           *~
            *  Written By        - Christie Gregory                     *~
            *                                                           *~
            *  Description       - Utility Program used to Upload Appian*~
            *                      Trucks and Routes.                   *~
            *                      - (1) Delete Sales Order from Load-A *~
            *                      - (2) Create New Load Number.        *~
            *                      - (2) Move Sales Order to Load (B).  *~
            *                      - (3) Rebuild Sort Index             *~
            *                      - (4) Rebuild Drop Seq.              *~
            *                      - (5) Rebuild Seq No.      - N/A     *~
            *                                                           *~
            *  Code Tables Used  -                                      *~
            *                                                           *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/14/03 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            * 03/15/04 ! (EWD001) Mod to prevent shipping from    !     *~
            *          !          uploading orders not assigned   !     *~
            *          !          to a load!                      !     *~
            * 05/17/05 ! (AWD002) mod to skip back hauls on upload! CMG *~
            * 03/22/06 ! (AWD003) Mod for North East              ! CMG *~
            * 05/22/06 ! (AWD004) Mod for PeopleNet               ! CMG *~
            * 09/07/06 ! (AWD005) mod to use last stop time instead!CMG *~
            *          ! of the end time in the upload file as the!     *~
            *          ! truck end time                           !     *~
            *06/03/2011! (AWD006) - add orcl usr & pswd lookup    ! CMG *~
            *09/26/2012! (AWD007) - mods for ensenda              ! CMG *~
            *05/06/2013! (AWD008) - bolt and scheam mods          ! CMG *~
            *02/02/2014! (AWD009) - mod to change to Penske NTX   ! CMG *~
            *01/23/2015! (AWD010) - mods for NTX and Penske       ! CMG *~
            *10/19/2017! (CR1174) - add shipment block processing ! RDB *~
            *04/14/2019! (CR1984) - mod to ATLaS remote order lne ! CMN *~
            *05/22/2019! (CR2038) add shipto to pgorlntr          ! CMN *~
			*06/20/2022! CR3115 increase truck length, all impacts! RDB *~
            *************************************************************

        dim                                                              ~
            or_rec$170,                  /* S.O. Header History Rec    */~
            or_po$16,                    /* Purchase Order Number      */~
            or_hows$2,                   /* How Ship Code              */~
            or_region$2,                 /* Region Code                */~
            or_cuscode$9,                /* Customer Code              */~
            dt_key$23,                   /* 'DT' Key                   */~
            dt_bar$18,                   /* 'DT' Key                   */~
            dt_index$30,                 /* 'DT' Key                   */~
            dt_part$25,                  /* 'DT' Key                   */~
            dt_rec$256,                  /* Planning Detail Record     */~
            sc_rec$128,                  /* (APCPLNSC) - Line Items    */~
            sc_so$8,                     /*                            */~
            sc_line$2,                   /*                            */~
            sc_drop_seq$5,               /* DROP SEQ. NO.              */~
            sc_drop$2,                   /* CUSTOMER DROP NO           */~
            sc_key$10, sc_key1$27,       /* LINE ITEM LOAD KEY         */~
            sc_load$5, new_load$5,       /* S.O. LOAD AND NEW LOAD     */~
            sc_part$25,                  /* Part Number                */~
            l1$8,                        /* Next Load Number           */~
            sort_key1$60,                /* USED BY (APCPLN9B) SORT    */~
            pl_sort$12,                  /* USED FOR (APCPLNDT) SORT   */~
            pl_key$11,                   /* PRIMARY KEY - (APCPLNUC)   */~
            hdr$40, msg$(3%)79,          /* ASKUSER Arrays             */~
            date$8,                      /* Date for screen display    */~
            errormsg$79,                 /* Error message              */~
            userid$3,                    /* Current User Id            */~
            sav_load$5,                  /* Save Load Number           */~
            apor_key$8,                  /* Appian EWDAPPOR Key        */~
            apsc_key$10,                 /* Appian EWDAPPSC Key        */~
            apld_key$5,                  /* Appian EWDAPPLD Key        */~
            apor_rec$(3%)128,            /* Appian EWDAPPOR Record     */~
            apsc_rec$(3%)128,            /* Appian EWDAPPSC Record     */~
            apld_rec$128,                /* Appian EWDAPPLD Record     */~
/*(AWD004)*/ rtrec$(2%)256,              /* Upload Record              */~
/*(AWD004)*/rtkey$58,                    /* Upload Key   CR3115        */~
            ear_dis$6,                   /* Earliest Dispatch Date     */~
            route$4,                     /* Route Number               */~
            stop$2,                      /* Stop Number                */~
            arrv_tme$4,                  /* Arrival Time               */~
            dept_tme$4,                  /* Departure Time             */~
            unload_dte$10,               /* Unload at Customer Date    */~
            customer$6,                  /* Customer Code              */~
            so_number$8,                 /* Sales Order Number         */~
/*AWD010*/  truck$6,                     /* Truck Number    CR3115     */~
/*AWD010*/  sav_truck$6,                 /* Save Truck Number  CR3115  */~
            tindex$4,                    /* Route Number   CR3115      */~
            sav_index$4,                 /* Save Route for Index CR3115*/~
            sav_drop$2,                  /* Save Drop Number           */~
            start_dte$10,                /* Start Date                 */~
            start_tme$4,                 /* Start Time                 */~
            end_dte$10,                  /* End Date                   */~
            end_tme$4,                   /* End Time                   */~
            cust_name$30,                /* Customers Name             */~
            cust_addr$(2%)30,            /* Customers Address          */~
            cust_city$18,                /* Customers City             */~
            cust_state$2,                /* Customers State            */~
            cust_zip$10,                 /* Customers Zip              */~
            bcklines_key$19,             /* BCKLINES Readkey           */~
            config_lne$3,                /* Configuration Line         */~
            apc_scr$120,                 /* Screen Display Text        */~
            apc_prt$60,                  /* Print Display Text         */~
            apc_sze$20,                  /* Long Form of Size          */~
            s_23m$3,                     /* Model Code for Series      */~
            s_23$8,                      /* New Series Code            */~
            s_so$8,                      /* Sales Order                */~
            s_ln$3,                      /* Sales Order Line           */~
            s_prv$30,                    /* Private Label Name         */~
            s_1$2,                       /* Private Label Code         */~
            store$3,                     /* Read Key for STORNAME      */~
/*(AWD004)*/ load_units$8,               /* Loading/Cubic Units        */~
/*(AWD004)*/ total_units$8,              /* Total Pieces               */~
/* CR3115 Increase next 4 arrays from 999 to 9999                      */~
/*(AWD004)*/ lst_cuscode$(9999%)9,       /* Last Customer              */~
/*(AWD004)*/ lst_shipto$(9999%)3,        /* last ship to code          */~
/*(AWD004)*/ lst_date$(9999%)6,          /* last date for truck        */~
/*(AWD004)*/ lst_time$(9999%)4,          /* last time for truck        */~
/*(AWD004)*/ sav_dp$2,                   /* Save Drop for AWDAPPDP     */~
/*(AWD004)*/ awdappdp_key$7,             /* AWDAPPDP readkey           */~
/*(AWD004)*/ sav_data$256,               /* Save data                  */~
/*(AWD004)*/ wrk_dte$10,                 /* Unformatted date           */~
/*(AWD004)*/ sav_dept$4                  /* Save Departure time        */

        dim                                                              ~
             grossOpenAmt$8,             /* (AWD007) Gross Open Amt     */~
             truckEqCodes$10,            /* (AWD007) Truck File EQ code */~
             distToPrevStop$10,          /* (AWD007) Distance Traveled  */~
             carrierID$2                 /* (AWD007) Carrier ID         */


        dim                              /* (APCPLNLD) - FILE          */~
            ld_region$2,                 /* Load Region Code (Primary) */~
            ld_wk_dte$10,                /* Calculated Production Date */~
            ld_load$5,                   /* Load Number (0),(A),(S)    */~
            ld_desc$30,                  /* Load Description           */~
            ld_dts1$10,                  /* Scheduled Production Date  */~
            ld_dts2$10,                  /* Scheduled Completion Date  */~
            ld_dts3$10,                  /* Scheduled Load Date        */~
            ld_dtp1$10,                  /* Planned Production Date    */~
            ld_dtp2$10,                  /* Planned Completion Date    */~
            ld_dtp3$10,                  /* Planned Load Date          */~
            ld_status$2,                 /* Current Status of Load     */~
            ld_date$10,                  /* Current Status Date        */~
            ld_yr$2,                     /* Scheduled Production Year  */~
            ld_wk$2,                     /* Scheduled Production Week  */~
            ld_day$1,                    /* Scheduled Production Day   */~
            ld_sister$2,                 /* Sister Compay Code         */~
/*CR1174*/  ld_userid$3,                 /* User ID Staging/Shipping   */~
/*CR1174*/  ld_time_stage$4,             /* Time Staging/Shipping      */~
/*CR1174*/  ld_awd$3,                    /* AWD Default                */~
/*CR1174*/  ld_shp_blk$3,                /* Shipment Block             */~
/*CR1174*/  ld_fil$5                     /* Filler Area                */

        dim ld_app_dte$10,               /* Appian Load Date           */~
            ap_ear_tme$4,                /* Appian Earlies Dispatch Tme*/~
            ap_flag$1,                   /* Have Appian been uploaded  */~
            ap_up_dte$10,                /* Appian Upload Date         */~
            ap_type$1,                   /* Appian Load or Not         */~
            sav_ap_type$1,               /* Appian Load or Not         */~
            ap_desc$30,                  /* Appian Load Description    */~
            ap_filler$45,                /* Filler Area                */~
            eqcodes$10                   /* EQ Codes         (AWD002)  */

        dim filename$8,                  /* Used By EWDOPEN            */~
            axd$(25)4,                   /*                            */~
            f2%(25%),                    /* = 0 if the file is open    */~
            f1%(25%),                    /* = 1 if READ was successful */~
            fs%(25%),                    /* = 1 if file open, -1 if it */~
            rslt$(25%)20                 /* Text from file opening     */

        dim                              /*                            */~
            error$256,                   /* Error String               */~
            ora_seqnr$3,                 /* Oracle SO Seq Number       */~
            ora_desc$250,                /* Oracle Part Description    */~
            server$25,                   /* Connection String          */~
            user$25,                     /* User Name to Connect       */~
            pass$25,                     /* Password to Connect        */~
            field$256,                   /* Query Return Field         */~
            stmt1$250,                   /* First Query String         */~
            stmt2$250,                   /* Second Query String        */~
            stmt3$250,                   /* Second Query String        */~
            stmt4$250,                   /* Second Query String        */~
            stmt5$250,                   /* Second Query String        */~
            stmt6$250,                   /* Second Query String        */~
            stmt7$250,                   /* Second Query String        */~
            stmt8$250,                   /* Second Query String        */~
            fields$(4%)100               /* String of Oracle Info      */


        dim ship_to$3                    /* customer ship to (AWD004)   */

        dim                              /* (AWD008)                   */~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4             /* Additional Info Fields     */
/* (CR1984) */
        dim status$2,                   /* Status code                  */~
            pgmname$10                  /* Program Name                 */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "Special Utility to Upload Applan Loads"
            pname$ = "AWDPLN03 - Rev: R6.04"

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
            * #1  ! APCPLNDT ! Production Master Detail                 *~
            * #2  ! APCPLNLD ! Planning/Scheduling Load Master File     *~
            * #3  ! APCPLNOR ! Sales Order Header History               *~
            * #4  ! GENCODES ! System Master Code Table Files           *~
            * #5  ! STORNAME ! Store Information File                   *~
            * #6  ! APCPLNSC ! Planning Master Scheduling File          *~
            * #7  ! APCPLNUC ! Planning Master Units Capacity file      *~
            * #11 ! AWDAPPLD ! Appian Load Header Information           *~
            * #14 ! AWDAPPOR ! Appian Order Header Information          *~
            * #15 ! AWDAPPSC ! Appian Master Scheduling File            *~
            * #16 ! BCKLINES ! BACK LOG LINE ITEM FILE                  *~
            * #17 ! AMTBOMIF ! Define Valid Options for Parts           *~
            * #18 ! CUSTOMER ! Customer Master Information              *~
            * #19 ! AWDAPPDP ! Appian Drop File           (AWD004)      *~
            * #21 ! SYSFILE2 ! Caelus Management System General Informa *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   24, keylen =   23,                    ~
                        alt key  1, keypos =   47, keylen =  57,         ~
                            key  2, keypos =   53, keylen =  51,         ~
                            key  3, keypos =    1, keylen =  23, dup,    ~
                            key  4, keypos =   06, keylen =   8, dup

            select #2,  "APCPLNLD",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 11,   keylen =  5,                      ~
                        alt key 1, keypos =  3, keylen = 13,             ~
                            key 2, keypos =  1, keylen = 15

            select #3,  "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =   170,             ~
                        keypos =    1, keylen =  51,                     ~
                        alt key  1, keypos =   27, keylen =  25,         ~
                            key  2, keypos =   70, keylen =   8, dup,    ~
                            key  3, keypos =   78, keylen =   8, dup,    ~
                            key  4, keypos =   52, keylen =   8,         ~
                            key  5, keypos =   36, keylen =  16, dup

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #5,  "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3


            select #6,   "APCPLNSC",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   24, keylen =   10,                    ~
                        alt key  1, keypos =    7, keylen =  27,         ~
                            key  2, keypos =    1, keylen =  33

            select #7,   "APCPLNUC",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =   11,                    ~
                        alt key  1, keypos =    7, keylen =  11

            select #10, "DTHOLDF2",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =  24,  keylen =  23,                     ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup
             select #11, "AWDAPPLD"                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =   12, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =  16,         ~
                            key  2, keypos =    2, keylen =  15,         ~
                            key  3, keypos =   17, keylen =  15



            select #13, "RTUPLOAD",                                      ~
                        varc,     consec ,  recsize =  286       /* (AWD002) */
                                                                 /* (AWD004) */

            select #14, "AWDAPPOR",                                      ~
                        varc,     indexed,  recsize =  384,              ~
                        keypos =   1, keylen =   8,                      ~
                        alt key  1, keypos =    9, keylen =  40,         ~
                            key  2, keypos =   15, keylen =  34,         ~
                            key  3, keypos =   17, keylen =  32

            select #15, "AWDAPPSC",                                      ~
                        varc,     indexed,  recsize =  384,              ~
                        keypos =    1, keylen =  10

            select #16, "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19

            select #17, "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32                      ~


            select #18, "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key 1,  keypos = 10,   keylen =  30,         ~
                            key 2,  keypos = 424,  keylen =   9,         ~
                            key 3,  keypos = 771,  keylen =   9,         ~
                            key 4,  keypos = 780,  keylen =   9

            select #19, "AWDAPPDP",                /*(AWD004)*/          ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =   7

            select #20, "AWDUPLD",                                       ~
                        varc,     indexed,  recsize = 512,   /*(AWD004)*/~
                        keypos =   1,  keylen = 58           /*(AWD007)reclen*/

            select #21,  "SYSFILE2",                                      ~
                         varc,     indexed,  recsize =  500,              ~
                         keypos =    1, keylen =  20

            select #22, "PGORLNTR",                    /* (CR1984) */    ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   21, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  64,         ~
                            key  2, keypos =   54, keylen =  11, dup

            select #63, "BCKSUBPT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  11,                     ~
                        alt key  1, keypos =  12, keylen =  11, dup,     ~
                            key  2, keypos =  23, keylen =  45, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            filename$ = "APCPLNDT" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNLD" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNOR" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "STORNAME" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSC" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNUC" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDAPPLD" : call "EWDOPEN" (#11, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLINES" : call "EWDOPEN" (#16, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AMTBOMIF" : call "EWDOPEN" (#17, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "CUSTOMER" : call "EWDOPEN" (#18, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "PGORLNTR" : call "EWDOPEN" (#22, filename$, err%) /*(CR1984)*/
            if err% <> 0% then gosub open_error
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#63, filename$, err%)
            if err% <> 0% then gosub open_error

            call "OPENFILE" (#13, "INPUT", f2%(13), rslt$(13), axd$(13))
REM            call "OPENFILE" (#13, "OUTPT", f2%(13), rslt$(13), axd$(13))


            call "OPENCHCK" (#10, fs%(10%), f2%(10%),100%, rslt$(10%))
            call "OPENCHCK" (#14, fs%(14%), f2%(14%),500%, rslt$(14%))
            call "OPENCHCK" (#15, fs%(15%), f2%(15%),500%, rslt$(15%))
/*(AWD004)*/
            call "OPENCHCK" (#19, fs%(19%), f2%(19%),500%, rslt$(19%))

            call "OPENCHCK" (#20, fs%(20%), f2%(20%), 0%, rslt$(20%))
            if fs%(20%) < 0% then goto create_sort_file


                 call "FILEBGON" (#20)


create_sort_file:

            call "OPENCHCK" (#20, fs%(20%), f2%(20%),500%, rslt$(20%))

/*(AWD006) */
            filename$ = "SYSFILE2" : call "EWDOPEN" (#21, filename$, err%)
            if err% <> 0% then gosub open_error


REM  **********************************************************************
REM  If the index file is deleted then need to run below OPENFILE to create
REM  dat and idx file.  Then copy over actual dat file over just created
REM  dat file.  DCHECK the file and program will run.
REM      call "OPENFILE" (#13, "OUTPT", f2%(13), rslt$(13), axd$(13))
REM  **********************************************************************

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

/* (AWD008) */
            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #4, schema_err%)

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

            check% = 0%
            call "APCPASSW"("AWDPLN03", userid$, check%)
                if check% <> 0% then goto exit_program

        beenherebefore% = 0%
        sav_truck$ = "999999"            /*(AWD004) CR3115 */
        ld_cnt%    = 0%
		sav_index$ = "1"                           /* Default CR3115 */	
        gosub sort_appian
        init(" ") sav_truck$          /*(AWD004)*/
        sav_dp$   = "99"              /*(AWD004)*/
        sav_data$ = "99"              /*(AWD004)*/
        tot_load_units, tot_total_units = 0.00  /*(AWD004)*/
        tot_grossOpenAmt, tot_distToPrevStop = 0.00  /*(AWD007)*/
        status$ = "04"
        pgmname$ = "AWDPLN03"
        call "SHOSTAT" (" Connecting to ORACLE")

        gosub oracle_connect
        if oci_err% <> 0% then goto oracle_error

        call "SHOSTAT" (" LOADING DATA ")
        rtkey$ = all(hex(00))
insert_order:
            if beenherebefore% = 1% then gosub update_appld
            if beenherebefore% = 1% then gosub oracle_delete
            gosub read_upload
            gosub read_subpart              /*!!@@ this cannot work, bad logic*/
            beenherebefore% = 1%
            if upload% <> 1% then goto insert_done

            call "SHOSTAT" ("Inserting S.O.("&so_number$&") ")

            sc_drop_seq$ = "00000"
            sc_drop$     = "00"

            sc_so$       = so_number$               /* Insert Sales Order */
            stop% = 0%
            convert stop$ to stop%, data goto L19180

L19180:     convert stop% to sc_drop$, pic(00)

            gosub update_header                /* Update with New Load */
            if load% = 0% then goto insert_order


            gosub oracle_flush
            gosub oracle_procedure
            gosub oracle_line

            gosub update_app_header


/*(AWD004)*/
            if sav_dp$ = "99" then sav_dp$ = stop$   /*sav_dp$ = "99" init drop*/
            if sav_dp$ <> stop$ then gosub update_awdappdp


            tot_load_units  = tot_load_units + load_units
            tot_total_units = tot_total_units + total_units
/* (AWD007) */
            tot_grossOpenAmt   = tot_grossOpenAmt + grossOpenAmt
REM Sum up distances for NC
            if schema% <> 2% then                               ~
                tot_distToPrevStop = tot_distToPrevStop + distToPrevStop
REM Only Sum first for each stop for Penske TX data
            if schema% = 2% and tot_distToPrevStop <= 0.00 then ~
               tot_distToPrevStop = tot_distToPrevStop + distToPrevStop

            sav_dept$       = dept_tme$
/*(AWD004) - end */


            init(" ") sc_key$, sc_rec$
            str(sc_key$,1%,8%) = sc_so$
        insert_order_nxt                                      /* #6-APCPLNSC */
            read #6,key > sc_key$, using L19350, sc_rec$,                 ~
                                                     eod goto insert_order
L19350:        FMT CH(128)


                                               /* Never found Sales Order!!  */

            if str(sc_key$,9%,2%) = " " and                               ~
                       str(sc_rec$,24%,8%) <> sc_so$ then goto L19470


            sc_key$ = str(sc_rec$,24%,10%)
            if sc_so$ <> str(sc_key$,1%,8%) then goto insert_order
               sc_line$ = str(sc_rec$,32%,2%)
               sc_load$ = str(sc_rec$,7%,5%)
               gosub update_lines
               if schema% = 2% then gosub updateRemoteOrderLine   /* (CR1984) */

            goto insert_order_nxt
        insert_done
           gosub update_awdappdp   /* (AWD004) - update last drop */


        return clear all
        goto exit_program
L19470:     errormsg$ = "(Error) - Unable to Insert S.O. -- "& sc_so$
            gosub error_prompt
            goto insert_order


        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
REM         dataload

REM         return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
REM         dataput

REM         return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        load_prompt
           comp% = 2%
           hdr$ = "*****Assign A Regular Load Number*****"
           msg$(1%) = "                                           "
           msg$(2%) = "** Load Number Assigned (" & new_load$ & ") **"
           goto L62320
        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
L62320:    msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        oracle_error
             init(" ") errormsg$
             errormsg$ = "Can Not Connect to ORACLE.  Contact System Support!!"
             gosub error_prompt
        return clear all
        goto exit_program

        read_upload
          upload% = 0%
          init(" ") start_dte$, start_tme$, truck$, stop$,   ~
                    customer$, so_number$, end_dte$, end_tme$, route$,       ~
                    arrv_tme$, dept_tme$, unload_dte$, cust_name$,           ~
                    cust_addr$(1%), cust_addr$(2%), cust_city$, cust_state$, ~
                    cust_zip$, ship_to$
                                                             /* #20-AWDUPLD */
          read #20, hold, key > rtkey$, using L10450, start_dte$, start_tme$,  ~
                                truck$, stop$, customer$, end_dte$, end_tme$,  ~
                                route$, arrv_tme$, dept_tme$, so_number$,      ~
                                unload_dte$, cust_name$, cust_addr$(1%),       ~
                                cust_addr$(2%), cust_city$, cust_state$,       ~
/*(AWD004)*/                    cust_zip$, load_units, total_units,            ~
                                ship_to$, grossOpenAmt, distToPrevStop,        ~
                                carrierID$, /*(AWD007)*/  eod goto upload_done

                 str(rtkey$,1%,8%) = start_dte$
                 str(rtkey$,9%,4%) = start_tme$
                 str(rtkey$,13%,6%) = truck$       /* CR3115 */
                 str(rtkey$,19%,2%) = stop$
                 str(rtkey$,21%,6%) = customer$
                 str(rtkey$,27%,8%) = end_dte$
                 str(rtkey$,35%,4%) = end_tme$

                 str(rtkey$,39%,4%) = route$
                 str(rtkey$,43%,4%) = arrv_tme$
                 str(rtkey$,47%,4%) = dept_time$

                 str(rtkey$,51%,8%) = so_number$

                delete #20

                if str(sav_data$,1%,2%) = "99" then gosub setup_sav_data

           upload% = 1%
        upload_done
        return

        setup_sav_data
             str(sav_data$,1%,58%) = str(rtkey$,1%,58%)  /* Read Key CR3115 */

REM WRK_DTE$ = END_DTE$
REM CALL "DATUFMTC" (WRK_DTE$)
REM STR(SAV_DATA$,32%,6%) = WRK_DTE$
REM STR(SAV_DATA$,38%,4%) = END_TME$

             str(sav_data$,59%,4%) = route$
             str(sav_data$,63%,4%) = arrv_tme$
             str(sav_data$,67%,4%) = dept_tme$

             wrk_dte$ = unload_dte$
             call "DATUFMTC" (wrk_dte$)
             str(sav_data$,71%,6%) = wrk_dte$
             str(sav_data$,77%,3%) = ship_to$

             sav_dp$ = stop$

             tot_load_units, tot_total_units = 0.00
             tot_grossOpenAmt, tot_distToPrevStop = 0.00  /*(AWD007)*/
        return

        sort_appian
          call "SHOSTAT" (" Sorting Data for Trucks ")
          init(" ") rtrec$(), rtkey$  /* (AWD002) */ 
                                                             /* #13-RTUPLOAD */
          read #13, hold, using L10430, rtrec$(), eod goto sort_appian_done

L10430:         FMT 2*CH(256)    /*(AWD004) */


                 eqcodes$ = str(rtrec$(),231%,10%)        /* EQ Codes (AWD002)*/
                 for i% = 1% to 10% step 2%               /* EQ Codes (AWD002)*/
                     if str(eqcodes$,i%,2%) = "BH" then goto sort_appian
                 next i%                                  /* EQ Codes (AWD002)*/

/* CR3115 */
                 truck$ = str(rtrec$(),197%,6%)             /* Truck   CR3115 */
				 tindex$ = str(rtrec$(),1%,4%)	            /* Route   CR3115 */					 
/* (AWD010) */
REM                 if schema% = 2% then truck$ = str(rtrec$(),2%,3%)
/*(AWD004) */
/* CR3115 */
                 if str(sav_truck$,1%,6%) = "999999" then sav_truck$ = truck$   
                 if str(sav_truck$,1%,6%) = "999999" then sav_index$ = tindex$	 
                 if sav_truck$ = truck$ then goto same_truck

                   convert sav_truck$ to truck%, data goto bad_truck
/* CR3115 */
                   convert sav_index$ to tindex%, data goto bad_truck	
				   
                   lst_cuscode$(tindex%) = customer$
                   lst_shipto$(tindex%)  = ship_to$
                   wrk_dte$ = end_dte$
                   call "DATUFMTC" (wrk_dte$)
                   lst_date$(tindex%)    = wrk_dte$
/* (AWD005) */
REM LST_TIME$(TRUCK%)    = END_TME$
                   lst_time$(tindex%)    = dept_tme$

                   sav_truck$           = truck$
				   sav_index$           = tindex$	 /* CR3115 */
/* (AWD004) - end keep information by truck */

same_truck:
                 route$ = str(rtrec$(),1%,4%)              /* Truck             */
/* (AWD010) */
REM CR3115                 if schema% = 2% then route$ = str(rtrec$(),2%,3%)
                 stop$  = str(rtrec$(),5%,2%)              /* Stop              */
                 convert stop$ to stop%, data goto bad_stop

bad_stop:        convert stop% to stop$, pic(#0)
                 arrv_tme$ = str(rtrec$(),7%,4%)           /* Arrv At Customers */
                 dept_tme$ = str(rtrec$(),11%,4%)          /* Dept From Cust    */
                 unload_dte$ = str(rtrec$(),15%,10%)       /* Unloading at Cust */
                 customer$ = str(rtrec$(),25%,6%)          /* Customer          */
/* (AWD004) */
                 ship_to$  = str(rtrec$(),31%,3%)

                 so_number$ = str(rtrec$(),34%,8%)         /* Sales Order Num   */
                 if str(so_number$,1%,8%) = "99999999" then goto sort_appian
/*(AWD003) - begin */
                 convert so_number$ to so_number%, data goto convert_alpha

                 convert so_number% to so_number$, pic(00000000)


convert_alpha:


                 convert str(so_number$,2%,7%) to so_number%,  ~
                    data goto upload_done

                 convert so_number% to str(so_number$,2%,7%), pic(0000000)

/* (AWD003) - done */

                 cust_name$ = str(rtrec$(),42%,30%)        /* Cust Name         */
                 cust_addr$(1%) = str(rtrec$(),77%,30%)    /* Cust Addr 1       */
                 cust_addr$(2%) = str(rtrec$(),112%,30%)   /* Cust Addr 2       */
                 cust_city$ = str(rtrec$(),147%,18%)       /* Cust City         */
                 cust_state$ = str(rtrec$(),172%,2%)       /* Cust State        */
                 cust_zip$ = str(rtrec$(),187%,10%)        /* Cust Zip          */
                 p% = 0%
                 p% = pos(cust_zip$ = "-")
                 if p% = 0% then goto not_nine
                    cust_zip$ = str(cust_zip$,1%,5%) & str(cust_zip$,7%,4%)
not_nine:

                 start_dte$ = str(rtrec$(),203%,10%)       /* Disp Dte frm Wel  */
                 start_tme$ = str(rtrec$(),213%,4%)        /* Dispatch Time     */
                 end_dte$ = str(rtrec$(),217%,10%)         /* Truck at Welcome  */
                 end_tme$ = str(rtrec$(),227%,4%)          /* Time truck back   */
                 load_units$ = str(rtrec$(),241%,8%)       /*(AWD004) load units*/
                 total_units$ = str(rtrec$(),249%,8%)      /*(AWD004) totalUnits*/
                 grossOpenAmt$ = str(rtrec$(),257%,8%)     /*(AWD007)GrossOpnAmt*/
                 truckEqCodes$ = str(rtrec$(),267%,10%)    /*(AWD007)TrkFEQCde  */
                 distToPrevStop$ = str(rtrec$(),277%,10%)  /*(AWD007)Distance   */

/*(AWD004) - begin */
                 load_units, total_units, grossOpenAmt, distToPrevStop = 0.00
                 convert load_units$ to load_units, data goto bad_data1

bad_data1:

                 convert total_units$ to total_units, data goto bad_data2

bad_data2:

                 convert grossOpenAmt$ to grossOpenAmt, data goto bad_data3

bad_data3:

                 convert distToPrevStop$ to distToPrevStop, data goto bad_data4

bad_data4:
/*(AWD007)*/
                 carrierTruck% = 0%
                 convert truck$ to carrierTruck%, data goto badCarrierTruck

badCarrierTruck:
                 carrierID$ = "01"
                 if carrierTruck% >= 100% then carrierID$ = "02"
/*(/AWD007)*/

/* (AWD008) */
/* (AWD009) change to carrierID -> 04 Penske */
REM IF SCHEMA% = 2% THEN CARRIERID$ = "03"

                 if schema% = 2% then carrierID$ = "04"

                 call "DATUFMTC" (unload_dte$)
                 call "DATUFMTC" (start_dte$)
                 call "DATUFMTC" (end_dte$)

                 str(rtkey$,1%,8%) = start_dte$
                 str(rtkey$,9%,4%) = start_tme$
                 str(rtkey$,13%,6%) = truck$
                 str(rtkey$,19%,2%) = stop$
                 str(rtkey$,21%,6%) = customer$
                 str(rtkey$,27%,8%) = end_dte$
                 str(rtkey$,35%,4%) = end_tme$

                 str(rtkey$,39%,4%) = route$
                 str(rtkey$,45%,4%) = arrv_tme$
                 str(rtkey$,49%,4%) = dept_time$

                 str(rtkey$,51%,8%) = so_number$
			 
                                                             /* #20-AWDUPLD */
                 read #20, hold, key = rtkey$, eod goto not_sort

                      delete #20

        not_sort
                 put #20, using L10450,                                      ~
                          start_dte$,                                        ~
                          start_tme$,                                        ~
                          truck$,                                            ~
                          stop$,                                             ~
                          customer$,                                         ~
                          end_dte$,                                          ~
                          end_tme$,                                          ~
                          route$,                                            ~
                          arrv_tme$,                                         ~
                          dept_tme$,                                         ~
                          so_number$,                                        ~
                          unload_dte$,                                       ~
                          cust_name$,                                        ~
                          cust_addr$(1%),                                    ~
                          cust_addr$(2%),                                    ~
                          cust_city$,                                        ~
                          cust_state$,                                       ~
                          cust_zip$,                                         ~
                          load_units,                 /*(AWD004)*/           ~
                          total_units,                /*(AWD004)*/           ~
                          ship_to$,                   /*(AWD004)*/           ~
                          grossOpenAmt,               /*(AWD007)*/           ~
                          distToPrevStop,             /*(AWD007)*/           ~
                          carrierID$                  /*(AWD007)*/           


                  write #20


                     goto sort_appian

        sort_appian_done
* (AWD004) save all of the last stop information
            convert truck$ to truck%, data goto bad_truck
			convert tindex$  to tindex%, data goto bad_truck
            lst_cuscode$(tindex%) = customer$
            lst_shipto$(tindex%)  = ship_to$
            wrk_dte$ = end_dte$
            call "DATUFMTC" (wrk_dte$)
            lst_date$(tindex%)    = wrk_dte$
/* (AWD005) */
REM LST_TIME$(TRUCK%)    = END_TME$

            lst_time$(tindex%) = dept_tme$
bad_truck:
        return

        update_header
            load% = 0%
            gosub find_header_load /* Look up current load, check if app load */
            gosub lookup_header_load   /* Check to see if SO already uploaded */
            if sav_ap_type$ = "Y" then goto already_app
            if str(truck$,1%,3%) = "   " then goto load_error
/* CR3115 */			
            if str(sav_truck$,1%,6%) <> str(truck$,1%,6%) then gosub assign_load

                                                             /* #3-APCPLNOR */
            read #3,hold,key 4% = sc_so$,using L61030, or_rec$,           ~
                                                           eod goto L61100
L61030:        FMT CH(170)


REM SAV_LOAD$ = STR(OR_REC$,94%,5%)
REM GOSUB LOOKUP_HEADER_LOAD
REM IF SAV_AP_TYPE$ = "Y" THEN GOTO ALREADY_APP
REM DELETE #3

            sav_drop$ = str(or_rec$,25%,2%)
            or_po$ = str(or_rec$,36%,16%)
            or_hows$ = str(or_rec$,92%,2%)
            or_region$ = str(or_rec$,9%,2%)
            or_cuscode$ = str(or_rec$,27%,9%)

            if str(truck$,1%,3%) = "   " then goto load_error   /*   (EWD001)  */
            if str(sav_truck$,1%,5%) <> str(truck$,1%,5%) then gosub assign_load

            sc_drop_seq% = sc_drop_seq% + 10%
            convert sc_drop_seq% to sc_drop_seq$, pic(00000)
            str(or_rec$,25%,2%) = sc_drop$              /* Drop Number */
            str(or_rec$,94%,5%) = new_load$             /* Load Number */

            delete #3

            load% = 1%

            put #3, using L61030, or_rec$

            write #3, eod goto L61100
        already_app
        return
L61100:     errormsg$ = "(Error) - Updating Header (APCPLNOR) " &sc_so$
            gosub error_prompt
            init(" ") errormsg$
        return

        update_app_header
            gosub lookup_header_load   /* Check to see if SO already uploaded */
            init(" ") apor_key$, apor_rec$()
            apor_key$ = sc_so$
                                                             /* #14-AWDAPPOR */
            read #14, hold, key = apor_key$, eod goto no_app_header

                 delete #14

        no_app_header
           str(apor_rec$(),1%,8%) = sc_so$
           str(apor_rec$(),9%,6%) = ear_dis$
           str(apor_rec$(),15%,2%) = or_region$
           str(apor_rec$(),17%,5%) = new_load$
           str(apor_rec$(),22%,2%) = sc_drop$
           str(apor_rec$(),24%,9%) = or_cuscode$

           str(apor_rec$(),33%,16%) = or_po$
           str(apor_rec$(),49%,6%) = unload_dte$
           str(apor_rec$(),55%,4%) = arrv_tme$
           str(apor_rec$(),59%,4%) = dept_tme$
           str(apor_rec$(),63%,5%) = truck$
           str(apor_rec$(),68%,5%) = sav_load$
           str(apor_rec$(),73%,2%) = sav_drop$
           str(apor_rec$(),75%,1%) = "N"
           str(apor_rec$(),76%,30%) = cust_addr$(1%)
           str(apor_rec$(),106%,30%) = cust_addr$(2%)
           str(apor_rec$(),136%,18%) = cust_city$
           str(apor_rec$(),154%,2%) = cust_state$
           str(apor_rec$(),156%,9%) = str(cust_zip$,1%,9%)
           str(apor_rec$(),165%,2%) = or_hows$
           str(apor_rec$(),167%,30%) = cust_name$
           str(apor_rec$(),197%,6%) = start_dte$

           put str(apor_rec$(),203%,8%) using L61490, load_units
L61490:            FMT PD(14,4)

           put str(apor_rec$(),211%,8%) using L61490, total_units

* STR(APOR_REC$(),203%,8%) = LOAD_UNITS$
* STR(APOR_REC$(),211%,8%) = TOTAL_UNITS$

/* (AWD004) */
           str(apor_rec$(),219%,3%) = ship_to$
/*(AWD007) add dist and gross amt*/
           put str(apor_rec$(),222%,8%) using L61490, grossOpenAmt

           put str(apor_rec$(),230%,8%) using L61490, distToPrevStop
/*(/AWD007)*/

           str(apor_rec$(),238%,2%) = carrierID$

           put #14, using L61500, apor_rec$()

           write #14, eod goto L61590
L61500:          FMT 3*CH(128)
        return
L61590:     errormsg$ = "(Error) - Updating APPIAN Header (EWDAPPOR) " &sc_so$
            gosub error_prompt
            init(" ") errormsg$
        return

        update_lines                                         /* #6-APCPLNSC */
            read #6,hold,key = sc_key$, using L61180, sc_rec$,           ~
                                                         eod goto L61280

               delete #6

            sc_part$ = str(sc_rec$,34%,25%)
            str(sc_rec$,12%,5%)  = sc_drop_seq$  /* Sched Drop Seq.    */
            str(sc_rec$,17%,2%)  = sc_drop$      /* Sched Drop No.     */
            str(sc_rec$,7%,5%)   = new_load$     /* Curr Assigned Load */
            str(sc_rec$,105%,5%) = sav_load$      /* Orig. Parent Load  */

            put #6, using L61180, sc_rec$
            write #6, eod goto L61290

        gosub update_app_lines
        gosub update_dtl
L61280: return
L61290:     errormsg$ = "(Error) - Updating Line (APCPLNSC) " &sc_key$
            gosub error_prompt
            init(" ") errormsg$
        return

        updateRemoteOrderLine                     /* (CR1984) *//* (CR2038) */
          call "APCORLNS" (or_cuscode$, sc_so$, sc_line$, status$, pgmname$, ~
                                                                  #22, error%)
          error% = 0%
        return

        lookup_header_load            /* Check to see if SO already uploaded */
                                                             /* #11-AWDAPPLD */
            read #11, key = sav_load$, using L61300, ear_dis$, sav_ap_type$,  ~
                                                  eod goto no_load_hdr
L61300:                 FMT POS(2), CH(6), POS(83), CH(2)
        no_load_hdr
        return

        update_app_lines
           gosub bck_lines
           gosub oracle_fields
           if str(ora_desc$,1%,250%) = " " then gosub get_caelus_desc
           init(" ") apsc_key$, apsc_rec$()
           str(apsc_key$,1%,10%) = str(sc_key$,1%,10%)
                                                             /* #15-AWDAPPSC */
           read #15, hold, key = apsc_key$, eod goto no_app_lines

                      delete #15
        no_app_lines

            str(apsc_rec$(),1%,8%) = str(sc_key$,1%,8%)
            str(apsc_rec$(),9%,2%) = str(sc_key$,9%,2%)
            str(apsc_rec$(),11%,250%) = ora_desc$
            str(apsc_rec$(),261%,3%) = config_lne$

            put #15, using L61500, apsc_rec$()
            write #15, eod goto L61650
        return
L61650:     errormsg$ = "(Error) - Updating APPIAN Lines (EWDAPPSC) " &sc_so$
            gosub error_prompt
            init(" ") errormsg$
        return


        bck_lines
            init(" ") bcklines_key$
            str(bcklines_key$,1%,8%)  = str(sc_key$,1%,8%)
            convert str(sc_key$,9%,2%) to sc_lines%, data goto L61690

L61690:
            convert sc_lines% to str(bcklines_key$,17%,3%), pic(###)
                                                             /* #16-BCKLINES */
            read #16, key = bcklines_key$, using L61700, config_lne$,   ~
                                                  eod goto no_bck_lines
L61700:            FMT POS(284), CH(3)

                 if str(config_lne$,1%,3%) = " " then                              ~
                         config_lne$ = str(bcklines_key$,17%,3%)
        no_bck_lines
        return

        update_dtl
            init(" ") dt_key$
            str(dt_key$,1%,8%) = sc_so$
            str(dt_key$,9%,2%) = sc_line$
        update_dtl_nxt                                       /* #1-APCPLNDT */
            read #1,hold,key > dt_key$, using L61410, dt_rec$,            ~
                                                 eod goto update_dtl_done
L61410:        FMT CH(256)
            dt_key$ = str(dt_rec$,24%,23%)
            if str(dt_key$,1%,8%) <> sc_so$ then return
            if str(dt_key$,9%,2%) <> sc_line$ then return
               delete #1

               str(dt_rec$,1%,5%) = new_load$
               str(dt_rec$,6%,5%) = sc_drop_seq$
               str(dt_rec$,11%,2%)= sc_drop$
               gosub sort_dtl

               put #1, using L61410, dt_rec$
               write #1, eod goto L61560
               gosub update_dtholdfl
               goto update_dtl_nxt
        update_dtl_done
        return
L61560:     errormsg$ = "(Error) - Updating Detail (APCPLNDT) " &dt_key$
            gosub error_prompt
            init(" ") errormsg$
        return


        update_appld
            init(" ") sc_key1$
            str(sc_key1$,1%,5%) = sav_load$                  /* #6-APCPLNSC */
            read #6,key 1% > sc_key1$, using L61710, sc_key1$, eod  ~
                                                goto updte_appld_done

L61710:               FMT POS(7), CH(27)

               if str(sc_key1$,1%,5%) <> sav_load$ then goto updte_appld_done
       return
       updte_appld_done
            init(" ") apld_key$, apld_rec$
            str(apld_key$,1%,5%) = sav_load$
                                                             /* #11-AWDAPPLD */
            read #11, hold, key = apld_key$, using L61750, apld_rec$, ~
                                                       eod goto no_appld
L61750:            FMT CH(128)

               delete #11
            str(apld_rec$,1%,1%) = "5"
            put #11, using L61750, apld_rec$
            write #11, eod goto no_appld

        return
        no_appld
            errormsg$ = "(Error) - Updating Appian Load (EWDAPPLD) " &apld_key$
            gosub error_prompt
            init(" ") errormsg$
        return


        sort_dtl
              init(" ") pl_key$, pl_sort$, sort_key1$
              pl_sort$ = "02678L9A    "
        REM - Build Index (DT_INDEX$)
              str(pl_key$,1%,2%)  = str(dt_rec$,230%,2%)
              str(pl_key$,3%,2%)  = str(dt_rec$,232%,2%)
              str(pl_key$,5%,2%)  = str(dt_rec$,104%,2%)
              str(pl_key$,7%,3%)  = str(dt_rec$,42%,3%)
              str(pl_key$,10%,2%) = str(dt_rec$,45%,2%)      /* #7-APCPLNUC */
              read #7,key = pl_key$,using L61720, pl_sort$,               ~
                                                 eod goto L61740
L61720:          FMT POS(18), CH(12)

L61740:       call "APCPLN9B" ( "0",         /* Set Flag for Data File */~
                                pl_sort$,    /* Sort Code From APCPLNUC*/~
                                dt_rec$,     /* (APCPLNDT) Record      */~
                                bcksubpt_rec$, /* BCKSUBPT (AWD008)    */~
                                sort_key1$,  /* Output Index Built     */~
                                #4 )         /* (GENCODES)             */

              str(dt_rec$,66%,30%) = str(sort_key1$,1%,30%)
        return


        assign_load
         if sav_ap_type$ = "Y" then return

         if ld_cnt% > 0% then gosub update_awdappdp  /*(AWD004) */
         sc_drop_seq% = 0%
         gosub lookup_parent_load

         gosub get_nxt_load
                                                             /* #2-APCPLNLD */
         read #2,hold,key = new_load$, eod goto L31210

               delete #2                           /* Delete Load       */

L31210:     REM  call "DATUFMTC" (date$)
            ld_desc$ = "APPIAN LOAD   " & date$

        put #2, using L61150,                                             ~
            ld_region$,                  /* Load Region Code (Primary) */~
            ld_wk_dte$,                  /* Calculated Production Date */~
            new_load$,                   /* Load Number (0),(A),(S)    */~
            ld_desc$,                    /* Load Description           */~
            ld_dts1$,                    /* Scheduled Production Date  */~
            ld_dts2$,                    /* Scheduled Completion Date  */~
            ld_dts3$,                    /* Scheduled Load Date        */~
            ld_dtp1$,                    /* Planned Production Date    */~
            ld_dtp2$,                    /* Planned Completion Date    */~
            ld_dtp3$,                    /* Planned Load Date          */~
            ld_status$,                  /* Current Status of Load     */~
            ld_date$,                    /* Current Status Date        */~
            ld_yr$,                      /* Current Production Year    */~
            ld_wk$,                      /* Scheduled Production Week  */~
            ld_day$,                     /* Scheduled Production Day   */~
            ld_sister$,                  /* Sister Code                */~
            ld_userid$,                  /* User ID in Scanning        */~
            ld_time_stage$,              /* Time staging               */~
            ld_awd$,                     /* Default AWD                */~
            ld_shp_blk$,                 /* Shipment Block             */~
            ld_fil$                      /* Filler Area                */

            write #2, eod goto dataput_done
            gosub put_appld
         sav_truck$ = truck$
        dataput_done
        return
        put_appld
            ap_ear_tme$ = "9999"
            ld_app_dte$, ap_up_dte$ = date
            ap_type$ = "Y"
            ap_flag$ = "5"
            ap_desc$ = ld_desc$


                                                             /* #11-AWDAPPLD */
            read #11,hold, key = new_load$, eod goto no_appld_put
                 delete #11

        no_appld_put

            put #11, using L61200,                                       ~
            ap_flag$,                    /* Appian Uploaded            */~
            ld_app_dte$,                 /* Appian Load Date           */~
            ap_ear_tme$,                 /* Appian Earlies Dispatch Tme*/~
            new_load$,                   /* Load Number                */~
            start_dte$,                  /* Appian Truck Start Date    */~
            start_tme$,                  /* Appian Truck Start Time    */~
            new_load$,                   /* Load Number                */~
            end_dte$,                    /* Appian Truck End Date      */~
            end_tme$,                    /* Appian Truck End Time      */~
            truck$,                      /* Appian Truck Number        */~
            ap_up_dte$,                  /* Appian Upload Date         */~
            ap_desc$,                    /* Appian Load Description    */~
            ap_type$,                    /* Is Appian Load or Not      */~
            ap_filler$                   /* Appian Filler Area         */

            write #11, eod goto appld_done
        appld_done
        return


        get_nxt_load                  /* L1$ = Regular load           0%  */
           new_load$, l1$ = " "
           store$ = "000"
           err% = 1%                                         /* #5-STORENAME */
           read #5,hold,key = store$, using L60600, l1$,                  ~
                                                        eod goto L60960
L60600:       FMT POS(154), CH(8)

              err% = 2%                 /* Assign Standard Load Number */
              convert l1$ to l1%, data goto L60960

              l1% = l1% + 1
              convert l1% to l1$, pic(00000000)          /* SIZE = 8 */
              if l1$ = "00099999" then l1$ = "00000001"
              new_load$ = str(l1$,4%,5%)

              put #5, using L60600, l1$
              rewrite #5
         err% = 0%
         ld_cnt% = ld_cnt% + 1%          /* (AWD004) */
         gosub load_prompt
        return
L60960:    if err% = 1% then                                             ~
              errormsg$ = "(Error) - Unable TO Read Store(000)?"
           if err% = 2% then                                             ~
              errormsg$ = "(Error) - Unable Assign Regular Load Number?"
           new_load$ = " "
           gosub error_prompt
        return

        lookup_parent_load                                   /* #2-APCPLNLD */
           read #2,key = sav_load$, eod goto no_parent

        get #2, using L61150,                                             ~
            ld_region$,                  /* Load Region Code (Primary) */~
            ld_wk_dte$,                  /* Calculated Production Date */~
            ld_load$,                    /* Load Number (0),(A),(S)    */~
            ld_desc$,                    /* Load Description           */~
            ld_dts1$,                    /* Scheduled Production Date  */~
            ld_dts2$,                    /* Scheduled Completion Date  */~
            ld_dts3$,                    /* Scheduled Load Date        */~
            ld_dtp1$,                    /* Planned Production Date    */~
            ld_dtp2$,                    /* Planned Completion Date    */~
            ld_dtp3$,                    /* Planned Load Date          */~
            ld_status$,                  /* Current Status of Load     */~
            ld_date$,                    /* Current Status Date        */~
            ld_yr$,                      /* Current Production Year    */~
            ld_wk$,                      /* Scheduled Production Week  */~
            ld_day$,                     /* Scheduled Production Day   */~
            ld_sister$,                  /* Sister Code                */~
            ld_userid$,                  /* User ID in Scanning        */~
            ld_time_stage$,              /* Time staging               */~
            ld_awd$,                     /* Default AWD                */~
            ld_shp_blk$,                 /* Shipment Block             */~
            ld_fil$                      /* Filler Area                */

        no_parent
        return

        lookup_app_parent
            init(" ") sav_ap_type$                           /* #11-AWDAPPLD */
            read #11, key = sav_load$, eod goto no_app_parent

            get #11, using L60990, sav_ap_type$

L60990:           FMT POS(83), CH(2)
        no_app_parent
        return


        get_caelus_desc
           str(ora_desc$,1%,16%) = "<< E R R O R >> "
           call "APCDESCR" (sc_part$,apc_scr$,apc_prt$,apc_sze$,#17,er%)
           if er% <> 0% then goto no_caelus_desc
              len% = 0%
              len% = len(apc_scr$)
              str(ora_desc$,1%,len%)  = str(apc_scr$,1%,len%)
              str(ora_desc$,(len%+1%),20%) = str(apc_sze$,1%,20%)


            s_23% = 0%
            s_23m$ = str(sc_part$,1%,3%)
            s_so$  = str(sc_so$,1%,8%)                       /* #4-GENCODES */
            s_ln$  = str(sc_line$,1%,3%)                     /* #16-BCKLINES */
            init(" ") s_prv$, s_1$, s_23$                    /* #18-CUSTOMER */
            prv% = 1%                                 /* Use BCKLINES    */
            call "APCPRZSB" (prv%, s_1$, or_cuscode$, s_23m$, s_so$,       ~
                                   s_ln$, s_prv$, s_23$, s_23%,            ~
                                   #18, #4, #16, #16, x_er% )
            if x_er% <> 0% then return
               str(ora_desc$,1%,8%) = s_23$

        no_caelus_desc
        return

        oracle_connect
REM USER$   = "MSSQL"
REM PASS$   = "MSSQL"
            gosub get_user_pswd       /* (AWD006) */

            server$, stmt1$, stmt2$ = " "

            oci_err% = 0%
            no_fields% = 0%

            call "CONNECT" (user$, pass$, server$, oci_err%)
        return

/* (AWD006) beg */
        get_user_pswd
            call "READ100" (#21, "ORACLE PASSWORD", f1%(21%))   /* SYSFILE2 */
            if f1%(21%) <> 0% then get #21 using ORCL_PSWD, user$, pass$
ORCL_PSWD:         FMT POS(21), CH(50), POS(50)

        return

/* (AWD006) END */
* !!@@ Should replace with description from ORADESC2
        oracle_procedure
            init(" ") stmt1$
            str(stmt1$,1%,24%) = "CALL MSSQL.RPT_DISPLAY('"
            str(stmt1$,25%,20%) = str(so_number$,1%,8%) & "', '" & userid$ & "')"
            gosub oracle_exec
        return

        oracle_line
            if oci_err% < 0% then return
            init(" ") stmt1$
            str(stmt1$,1%,40%)   = "SELECT * FROM MSSQL.DISPLAY WHERE SALESO"
            str(stmt1$,41%,26%)  = "RDER = '" & str(so_number$,1%,8%) & "' AND USER"
            str(stmt1$,67%,27%)  = "ID = '" & userid$ & "' ORDER BY DISPLAY"
            str(stmt1$,94%,40%)  = "ORDER ASC, LINENUMBER ASC, CONFIG DESC, "
            str(stmt1$,134%,15%) = "UNITID ASC   "

            gosub oracle_flush
            gosub oracle_query
        return

        oracle_delete
            if oci_err% < 0% then return
            init(" ") stmt1$, error$
            str(stmt1$,1%,40%)  = "DELETE FROM MSSQL.DISPLAY WHERE SALESORD"
            str(stmt1$,41%,24%) = "ER = '" & str(so_number$,1%,8%) & "' AND USER"
            str(stmt1$,65%,27%) = "ID = '" & userid$ & "'"
            gosub oracle_flush
            gosub oracle_exec

            gosub oracle_flush
        return

        oracle_fields
            if oci_err% < 0% then return
            init(" ") ora_desc$, fields$()
            gosub oracle_fetch
            if oci_err% > 0 or oci_err% < -1 then return
            gosub get_oracle_print
            if str(fields$(),4%,1%) <> "0" and str(sc_part$,1%,1%) <> "9" ~
                 then goto oracle_fields
        return

        get_oracle_print
            field_num% = 5%
            gosub oracle_getfield
            ora_seqnr$ = field$

            pos% = 1%
            for field_num% = 6% to 11%
                gosub oracle_getfield
                str(fields$(),pos%, field_len%) = field$
                pos% = pos% + field_len%
            next field_num%

            ora_desc$ = str(fields$(),32%,250%)

        return

        oracle_discnnct
            call "DISCNNCT" (oci_err%)
        return

        oracle_query
            oci_err% = 0%
            call "QUERY" (stmt1$, stmt2$, oci_err%)
        return

        oracle_exec
            oci_err% = 0%
            call "EXEC" (stmt1$, stmt2$, oci_err%)
        return

        oracle_exec1
            oci_err% = 0%
            call "EXEC1" (stmt1$, stmt2$, stmt3$, stmt4$, stmt5$, ~
                          stmt6$, stmt7$, stmt8$, oci_err%)
            init(" ") stmt1$, stmt2$, stmt3$, stmt4$, stmt5$, ~
                      stmt6$, stmt7$, stmt8$

        return

        oracle_fetch
            oci_err% = 0%
            no_fields% = 0%
            call "FETCH" (no_fields%, oci_err%)
        return

        oracle_getfield
            oci_err% = 0%
            field_len% = 0%
            init(" ") field$, name$
            call "FIELDINF" (field_num%, field$, name$, field_len%, oci_err%)
        return

        open_error
           comp% = 2%
           hdr$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = "(Open Error) - File = " & filename$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        load_error                                            /*   (EWD001)   */
           comp% = 2%
           hdr$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = "Orders Not Assigned to a Load--Contact Systems!!"
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        goto exit_program                                    /*   (EWD001)    */

/*(AWD004) - begin */
        update_awdappdp
              init(" ") awdappdp_key$
              str(awdappdp_key$,1%,5%) = new_load$
              str(awdappdp_key$,6%,2%) = sav_dp$
                                                             /* #19-AWDAPPDP */
              read #19, hold, key = awdappdp_key$, eod goto no_awdappdp

                       delete #19

no_awdappdp:


              convert str(sav_data$,13%,6%) to truck%, data goto bad_truck1
/* CR3115 */			  
			  convert str(sav_data$,59%,4%) to tindex%, data goto bad_truck1

bad_truck1:


              put #19, using AWDAPPDP_FMT1, new_load$, sav_dp$,  ~
                          str(sav_data$,21%,6%), /*Customer*/    ~
                          str(sav_data$,77%,3%), /*shipto  */    ~
                          str(sav_data$,1%,8%),  /*startdte*/    ~
                          str(sav_data$,9%,4%),  /*starttme*/    ~
                          str(sav_data$,71%,6%), /*unload/arrivaldte*/ ~
                          str(sav_data$,63%,4%), /*arrivaltme*/  ~
                          str(sav_data$,71%,6%), /*depart dte*/  ~
                          sav_dept$,             /*depttme*/     ~
                          lst_cuscode$(tindex%),  /*lastcuscode*/ ~
                          lst_shipto$(tindex%),   /*lastshipto*/  ~
                          lst_date$(tindex%),     /*enddte    */  ~
                          lst_time$(tindex%),     /*endtme    */  ~
                          tot_total_units,       /*pieces   */   ~
                          tot_load_units,        /*loadunits*/   ~
/*(AWD007)*/              tot_grossOpenAmt,      /*GrossOpenAmt*/~
/*(AWD007*/               tot_distToPrevStop,    /*DistToPrevSt*/~
                          " "                    /*filler   */




               write #19


                 gosub setup_sav_data

         return
/*(AWD004) - end   */

        update_dtholdfl
        dt_bar$ = str(dt_rec$,24,18)
        dt_key$ = str(dt_rec$,24,23)                         /* #10-DTHOLDF2 */
        read #10, hold, key = dt_key$, eod goto skip_delete
            delete #10
skip_delete: write #10, using L63866, dt_rec$
L63866:     FMT CH(256)
        dt_bar$ = str(dt_rec$,24,18)
            return

        update_apcplndt
        des_cnt = 0
            dt_key$ = "000000000000000000000"

    read_loop
        read #10, hold, key > dt_key$, hold, using L63866, dt_rec$,    ~
                     eod goto end_update
        dt_key$ = str(dt_rec$,24,23)
            dt_part$ = str(dt_rec$,189%,25%)
        dt_load$ = str(dt_rec$,1,5)
        dt_drop_seq$ = str(dt_rec$,6,5)
        dt_drop$ = str(dt_rec$,11,2)
        dt_bar$ = str(dt_rec$,24,18)
            dt_dept$ = str(dt_rec$,42,3)
        dt_proc$ = str(dt_rec$,45,2)
/*-------------------------------------------*/
            dt_index$ = str(dt_rec$,66,30)
            dt_seq$   = str(dt_rec$,111,5)
/*-------------------------------------------*/
            init(" ") stmt1$, stmt2$, stmt3$, stmt4$, stmt5$, stmt6$, ~
                      stmt7$, stmt8$
            stmt1$  = "UPDATE DTS_PLAN_DETAIL " & ~
              " SET LOAD_NUMBER = '" & dt_load$ & "', " & ~
          " PROD_DROP_NO = '" & dt_drop$ & "', " & ~
          " SORT_INDEX = '" & dt_index$ & "', " & ~
          " PRODUCTION_SEQUENCE = '" & dt_seq$ & "', " & ~
          " DROP_SEQUENCE = '" & dt_drop_seq$ & "' WHERE "
            stmt2$ = "BARCODE = '" & dt_bar$ & "' AND PLAN_DEPARTMENT = '" & ~
           dt_dept$ & "' AND PROCESS_CODE = '" & dt_proc$ & "'"
            gosub oracle_flush
            gosub oracle_exec1

            if oci_err% <> 0% then gosub oracle_error1

            delete #10

            des_cnt = des_cnt + 1
        if des_cnt < 500 then goto read_loop
        des_cnt = 0
              goto read_loop
            delete #10
end_update:

        stmt1$ = "COMMIT"
            gosub oracle_flush
            gosub oracle_exec
        return

        oracle_error1
REM            oci_err% = 0%

            init(" " ) error$
            call "ERROR" (error$)
            call "SHOSTAT" ("ERROR --> " & error$)

        return

                                                    /* (EWD007)       */

        oracle_flush
            oci_err% = 0%
            call "FLUSH" (oci_err%)
        return

                                                    /*  (EWD007)      */


/* (AWD012) - Begin */

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

                                         /* (APCPLNLD) - FILE          */
L61150: FMT CH(02),                      /* Load Region Code (Primary) */~
            CH(08),                      /* Calculated Production Date */~
            CH(05),                      /* Load Number (0),(A),(S)    */~
            CH(30),                      /* Load Description           */~
            CH(08),                      /* Scheduled Production Date  */~
            CH(08),                      /* Scheduled Completion Date  */~
            CH(08),                      /* Scheduled Load Date        */~
            CH(08),                      /* Planned Production Date    */~
            CH(08),                      /* Planned Completion Date    */~
            CH(08),                      /* Planned Load Date          */~
            CH(02),                      /* Current Status of Load     */~
            CH(08),                      /* Current Status Date        */~
            CH(02),                      /* Scheduled Production Year  */~
            CH(02),                      /* Scheduled Production Week  */~
            CH(01),                      /* Scheduled Production Day   */~
            CH(02),                      /* Sister Code                */~
            CH(03),                      /* User ID in Scanning        */~
            CH(04),                      /* Time staging               */~
            CH(03),                      /* Default AWD                */~
            CH(03),                      /* Shipment Block CR1174      */~
            CH(05)                       /* Filler Area        EWD006  */

L61180:      FMT CH(128)

L61200: FMT                              /* (EWDAPPLD) - File          */~
            CH(1),                       /* Appian Upload Flag         */~
            CH(6),                       /* Appian Earliest Upload Date*/~
            CH(4),                       /* Earliest Dispatch Time     */~
            CH(5),                       /* Load Number                */~
            CH(6),                       /* Appian Load Start Date     */~
            CH(4),                       /* Appian Load Start Time     */~
            CH(5),                       /* Load Number                */~
            CH(6),                       /* Appian Load End Date       */~
            CH(4),                       /* Appian Load End Time       */~
            CH(5),                       /* Appian Truck Number        */~
            CH(6),                       /* Appian Upload Date         */~
            CH(30),                      /* Appian Load Description    */~
            CH(1),                       /* Appian Type                */~
            CH(45)                       /* Filler                     */
			
/* CR3115 truck size */
L10450:          FMT CH(8), CH(4), CH(6), CH(2), CH(6), CH(8), CH(4), CH(4), ~
                     CH(4), CH(4), CH(8), CH(8), CH(30), CH(30), CH(30),     ~
                     CH(18), CH(2), CH(9), PD(14,4), PD(14,4), CH(3),/*(AWD004) */~
                     PD(14,4), PD(14,4), CH(02)  /*(AWD007)*/

AWDAPPDP_FMT1:       FMT CH(05), CH(02), CH(09), CH(03), CH(06), CH(04), ~
                         CH(06), CH(04), CH(06), CH(04), CH(09), CH(03), ~
                         CH(06), CH(04), PD(14,4), PD(14,4), PD(14,4),   ~
                         PD(14,4), CH(153)            /*(AWD007)*/


        find_header_load                                     /* #3-APCPLNOR */
           read #3, key 4% = sc_so$,using L61030, or_rec$,            ~
                                    eod goto no_header_load

             sav_load$ = str(or_rec$,94%,5%)
        no_header_load
        return

        read_subpart                                         /* #63-BCKSUBPT */
           init(" ") flag$, pgm$, bcksubpt_rec$, flds$(), info_flds$()

           so_inv%, item_no%, suberr1% = 0%
           flag$ = "0"
           pgm$  = "1"

           convert so_inv$ to so_inv%, data goto convert_alpha_sub

           convert so_inv% to so_inv$, pic(00000000)
               goto order_converted

convert_alpha_sub:

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
                          suberr1%)      /* Error Code                 */

        return
        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            convert ld_cnt% to cmg$, pic(#######0)

            call "SHOSTAT" ("Updating Database, Please Wait")
            gosub update_apcplndt
            call "SHOSTAT" (" Number of Loads Created -->  " & cmg$)
        close #10
REM         call "FILEBGON" (#10)            /* Department Capacities   */
            gosub oracle_discnnct

            end


