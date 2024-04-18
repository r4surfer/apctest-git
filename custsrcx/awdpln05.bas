      REM ***************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN05                             *~
            *  Creation Date     - 06/16/03                             *~
            *  Last Modified Date- 10/25/2016                           *~
            *  Written By        - Roy H. Hoffman                       *~
            *  Modifications By  - Christie Norman                      *~
            *                                                           *~
            *  Description       - New Appian Shipping Labels           *~
            *                      By Load, Drop, Customer, PO          *~
            *                      Range Specified for SO               *~
            *                      Barcode Range Specific               *~
            *                      Also used to print the new loading   *~
            *                      labels to be put in the Trailer (2)  *~
            *                                                           *~
            *  Code Tables Used  - (PLANPARTS), (EWDAPPHOW)             *~
            *                                                           *~
            *  Subroutine Used   - AWDPLA05 (Print Shipping Labels)     *~
            *                      APCPASSW (Password Sub (AWD004))     *~
            *                                                (AWD009)   *~
            *                    - AWDPLC05 (Print Loading Labels)      *~
            *  Special Comments  - Shipping Label file. (AWDAPPLS)      *~
            *                                                           *~
            *                    - AWDPLB05 (Wood Surround Label)       *~
            *                      Called from 'APCSCANN'               *~
            *                                                           *~
            *                    - New Printer Code for NE Data         *~
            *                      'Z' is for Printing Staging Labels   *~
            *                      her for NE Windows going direct to   *~
            *                      the customer.                        *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/16/03 ! (New) Program - Copied & Mod AWDPLN04.   ! RHH *~
            * 06/30/03 ! (AWD001) Mods to Label                   ! RHH *~
            * 08/20/03 ! (AWD002) Modification for printing of    ! RHH *~
            *          !    Shipping labels for all products.     !     *~
            *          !    Print Labels for those products that  !     *~
            *          !    don't go through APPIAN.              !     *~
            * 08/27/03 ! (AWD003) Modification to Promp when      ! RHH *~
            *          !    Print labels with Selection (2), (4)  !     *~
            * 05/05/04 ! (AWD004) Modification to Password Protect! RHH *~
            *          !    Selection (2) the creation of Non-    !     *~
            *          !    Appian Lables. To eliminate the       !     *~
            *          !    creation of those labels by mistake.  !     *~
            * 06/16/04 ! (AWD005) Modification to add a delete    ! CMG *~
            * 06/22/04 ! (AWD006) Mod to increase the size of the ! RHH *~
            *          !    production sequence number, and print !     *~
            *          !    in the box on the bottom left side.   !     *~
            * 06/30/04 ! (AWD007) Mod to add a "T" printer code   ! RHH *~
            *          !    for Testing the Shipping Label.       !     *~
            * 07/16/04 ! (AWD008) Mod to increase the size of the ! RHH *~
            *          !    drop number.                          !     *~
            * 04/04/05 ! (AWD009) Mod to print new Loading label  ! RHH *~
            *          !    with trailer number and barcode for   !     *~
            *          !    load.                                 !     *~
            * 04/04/05 ! (AWD010) Mod to Change Job name to 16    ! RHH *~
            *          !    characters on Shipping label. Also    !     *~
            *          !    Mod to High lit the P.O. Number on the!     *~
            *          !    Shipping Label.                       !     *~
            * 06/07/05 ! (AWD011) Mod to delete_trailer. Chg Code ! RHH *~
            *          !    to allow delete.                      !     *~
            * 10/17/05 ! (AWD012) Mod to change loading dock door ! RHH *~
            *          !    three Characters. Add an L,S,B,U to the!    *~
            *          !    of the number- NNA and Print on Label !     *~
            * 10/27/05 ! (AWD013) Mod to update Dock door for     ! RHH *~
            *          !    non Appian Loads.                     !     *~
            * 12/28/05 ! (AWD014) Mod for two new printers in     ! RHH *~
            *          !    shipping                              !     *~
            * 01/01/06 ! (PAR000) CR347 Mod New Sub Part No.      ! RHH *~
            * 03/03/06 ! (PAR001) Mod for New Sub Part Number     ! RHH *~
            *          !    in the Production Label File.         !     *~
            * 03/20/06 ! (PAR002) Mod to print Shipping labels    ! RHH *~
            *          !    for the North East.                   !     *~
            * 04/12/06 ! (PAR003) Mods for NE to Reprint Labels   ! RHH *~
            *          ! and Print Trailer Labels.                !     *~
            * 04/20/06 ! (PAR004) Mod for Special NE Print 'Z' to ! RHH *~
            *          !    Print in North Carolina.              !     *~
            * 05/08/06 ! (PAR005) Remove Lock on Load Record      ! RHH *~
            * 09/25/06 ! (PAR006) Mod to change the Room location ! RHH *~
            *          !    from 7 to 16 Characters. Also a new   !     *~
            *          !    field was created for Job Name. Now is!     *~
            *          !    a single field.                       !     *~
            *          !    no Change to (AWDPLB05 and AWDPLC05)  !     *~
            * 10/09/06 ! (AWD015) add create date and prd date to ! CMG *~
            *          !     AWDAPPLS label file                  !     *~
            *09/25/2008! (AWD016) add drop # calc for awdpla05    ! DES *~
            *10/29/2009! (AWD017) add print option E              ! DES *~
            *05/25/2011! (AWD018) mod to validate load number     ! CMG *~
            *01/20/2016! (SR70701) add option for printer G       ! CMG *~
            *04/01/2016! (SR73756) add option for alpha loading   ! MES *~
            *10/25/2016! (CR456) mod for DC Center                ! CMG *~
            *05/15/2017! (CR932) mod to add BP back on label      ! CMN *~
            *08/10/2017! (CR1073) allow blank, H, L, B            ! RDB *~
            *02/21/2019! CR1918 add PlyGem PO                     ! RDB *~
            *03/03/2020! CR2450 Add ALLFREE command               ! RDB *~
            *03/03/2021! CR2787 Duplicate SO in label file by load! RDB *~
			*01/24/2023! CR3234 Block P loads from creating labels! RDB *~
            *************************************************************
  
        dim                                                              ~
            schema$8,                    /* (PAR003) Schema Switch     */~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
            sc_process$1, sc_process_n$35,/* 2=Print, 2=Reprint,3=Insert*/~
            sc_load$5, sc_load_name$35,  /* Appian Load and Load Name  */~
            sc_prt$1,                    /* Printer Selection          */~
            sc_dock$3,                   /* Loading Dock Number(AWD012)*/~
            sc_drop$2,                   /* Apian Drop Number          */~
            sc_so$8, sc_so_name$35,      /* Appian S.O. and Cust Name  */~
            sc_barcode$18,               /* Production Barcode no.     */~
            ld_app_rec$128,              /* (awdappld) New Load File   */~
            ld_app_key0$5,               /* Load No. Primary key       */~
            or_app_rec$(3%)128,          /* (AWDAPPOR) Appian Header Fl*/~
            or_rec$170,                  /* (APCPLNOR) S.O. Header(AWD002)*/~
            sc_rec$128,                  /* (APCPLNSC) S.O. Sched (AWD002)*/~
            sc_key1$27,                  /* (APCPLNSC) Key 1   (AWD002)*/~
            bckmst_key$25,               /* BCKMASTR Readkey   (AWD002)*/~
            address1$30,                 /* City, State, Zip   (AWD002)*/~
            or_app_key0$8,               /* S.O. Primary Key           */~
            or_app_key3$32,              /* Load, Drop, Cust, PO       */~
            lb_key1$23,                  /* Secondary Key - Barcode    */~
            lb_rec$(4%)256,              /* Prod LB Record Data(PAR001)*/~
            ap_start_time$4,             /* Appian Time to Start Deliv */~
            sc_app_key0$10,              /* Primary Appian Line File   */~
            sc_app_rec$(3%)128,          /* (AWDAPPSC) Appian Line File*/~
            lb_app_key0$20,              /* Primary Key                */~
            lb_app_key2$32,              /* 2nd Alt Key                */~
            lb_app_rec$(4%)256,          /* (AWDAPPLS) Label   (PAR006)*/~
            counts$30,                   /* Show counts                */~
            filename$8,                  /* Used by EWDOPEN            */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            time$8,                      /* System time                */~
            address2$(2%)30,             /* Customer Address           */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3,                    /* Current User Id            */~
            table$9, code$3,             /* Table and Code lookup SR73756*/ ~
            dock_door$30                 /* Dock Door Description SR73756*/

        dim f2%(64)                      /* File Open & Read Statuses  */

/* <AWD016> */
        dim curr_drop$2,                   /* Appian Drop Number        */~
            curr_load$5,                   /* Appian Load Number        */~
            curr_po$16,                    /* Appian Purchase Order No. */~
            ap_dropnbr$8,                  /* Appian Purchase Order No. */~
            chr_tbl$(27)1,                 /* Appian Purchase Order No. */~
            prev_drop$2,                   /* Appian Drop Number        */~
            prev_load$5,                   /* Appian Load Number        */~
            prev_po$16                     /* Appian Purchase Order No. */
/* </AWD016> */

        dim                              /* (AWDAPPLS) - Label File    */~
            ap_bar$18,                   /* Production Barcode         */~
            ap_drop$2,                   /* Appian Drop Number         */~
            ap_region$2,                 /* Appian Region Code         */~
            ap_load$5,                   /* Appian Load Number         */~
            ap_cuscode$9,                /* Appian Customer Code       */~
            ap_po$16,                    /* Appian Purchase Order No.  */~
            ap_mull$1,                   /* Wood Surround or Part Code */~
            ap_so$8,                     /* Appian S.O. Number         */~
            ap_line$2,                   /* S.O. Line Number           */~
            ap_series_style$8,           /* Product Series an Style    */~
            ap_seq$5,                    /* Production Sequence Number */~
            ap_ear_dis$6,                /* Appian Earliest Disp Date  */~
            ap_config_lne$3,             /* W.W. Config Code           */~
            ap_arr_dte$6,                /* Appian Arrival Date        */~
            ap_cusname$30,               /* Customer Name for S.O.     */~
            ap_address$60,               /* Customer Address           */~
            ap_city$18,                  /* Customer City              */~
            ap_state$2,                  /* Customer State Code        */~
            ap_zip$9,                    /* Customer Zip Code          */~
            ap_part$25,                  /* MFG Part Number            */~
            ap_sub_part$20,              /* New Sub Part Number(PAR000)*/~
            ap_desc$250,                 /* Oracle Description         */~
            ap_howship$2,                /* How Ship Code              */~
            ap_old_load$5,               /* Old Caelus Load No.        */~
            ap_old_drop$2,               /* Old Caelus Drop No.        */~
            ap_act_dte$6,                /* App Act. Arrival Date      */~
            ap_act_tme$4,                /* App Act Arr Time Military  */~
            ap_truck$5,                  /* App Truck Number           */~
            ap_start_date$6,             /* App Ship Date              */~
            ap_shp_tme$4,                /* App Act Ship Time Military */~
            ap_scan_dte$6,               /* App Scann Date             */~
            ap_scan_tme$4,               /* App Scan Time Military     */~
            ap_wood$6,                   /* Wood Surr Fabrication Code */~
            ap_samp$1,                   /* Sample or Display 0,1,2    */~
            ap_contract$16,              /* Contractor Name            */~
            ap_job$10, ap_job1$6,        /* Filler Area        (PAR006)*/~
            ap_room$6,                   /* Filler Area        (PAR006)*/~
            ap_job_new$16,               /* New Job Name       (PAR006)*/~
            ap_room_new$16,              /* New Room Location  (PAR006)*/~
            ap_nominal$6,                /* Niminal Window Size        */~
            ap_dock$2, ap_dock1$1,       /* Loading Dock Door  (AWD012)*/~
            ap_stag_dte$6,               /* Date Printed and Staged    */~
            ap_stag_tme$4,               /* Time Printed and Staged    */~
            ap_label$1,                  /* App Label (Y)or(N) (AWD002)*/~
            ap_filler1$256,              /* Filler Area 1      (PAR006)*/~
            ap_filler2$120,              /* Filler Area 2      (PAR006)*/~
            ap_prd_dte$6                 /* Production date    (AWD015)*/
REM         ap_filler2$128,              /* Filler Area 2      (PAR006)*/~

        dim or_load$5                    /* (AWD018) OR Load number    */

                                         /* (AWD009)                   */
        dim                              /* (AWDTRAIL) - Trailer File  */~
            sc_trailer$8,                /* Trailer Number             */~
            tr_key0$20, tr_rec$128,      /* Primary key                */~
            tr_key1$20,                  /* Alt Key (1)                */~
            tr_stat$1,                   /* Trailer Status Code        */~
            tr_trailer$8, sc_trailer_desc$30,/* Trailer Number         */~
            tr_load$5,                   /* Load Number Assigned       */~
            tr_date$10,                  /* Date Load AssignedToTrailer*/~
            tr_open_dte$10,              /* Trailer Open Date          */~
            tr_open_tme$8,               /* Trailer Open Time          */~
            tr_closed_dte$10,            /* Trailer Closed Date        */~
            tr_closed_tme$8,             /* Trailer Closed Time        */~
            tr_fil$60                    /* Filler Area                */~
                                         /* (AWD009)                   */

                                         /* (PAR000)                   */
        dim                                                              ~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4             /* Additional Info Fields     */

                                         /* (PAR000)                   */
        dim message$256
        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(AWD) Generate New Shipping Labels    "
            pname$ = "AWDPLN05 - 09/25/2006"               /* (PAR006) */
                                                           /* (PAR005) */
                                                           /* (PAR002) */
                                                           /* (PAR001) */
                                                           /* (PAR000) */
                                                           /* (AWD013) */
                                                           /* (AWD012) */
                                                           /* (AWD011) */
                                                           /* (AWD009) */
                                                           /* (AWD007) */
                                                           /* (AWD008) */

        REM *************************************************************


                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

            mat f2% = con


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! AWDAPPOR ! Appian New Order Header File             *~
            * #2  ! AWDAPPSC ! Appian New S.O. Line Item File           *~
            * #3  ! AWDAPPLD ! Appian New Loads File                   *~
            * #4  ! GENCODES ! System Master Code Table Files           *~
            * #5  ! EWDPRDLB ! Production Label Data File -     (PAR001)*~
            * #6  ! APCPLNOR ! (NEW) S.O. Header Histroy        (AWD002)*~
            * #7  ! APCPLNSC ! Planning Master Schedule File    (AWD002)*~
            * #8  ! AWDAPPLS ! Appian New Shipping Label File   (PAR006)*~
            * #9  ! AWDTRAIL ! Track Trailer and Load Assigned  (AWD009)*~
            * #10 ! BCKMASTR ! S.O. Header File                 (CR1918)*~
            * #63 ! BCKSUBPT ! New Sub Part Number File         (PAR000)*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

             select #1, "AWDAPPOR",                                      ~
                        varc,     indexed,  recsize =  384,              ~
                        keypos =    1, keylen =    8,                    ~
                        alt key 1,  keypos =  9,   keylen =  40,         ~
                            key 2,  keypos = 15,   keylen =  34,         ~
                            key 3,  keypos = 17,   keylen =  32

            select #2, "AWDAPPSC",                                       ~
                        varc,     indexed,  recsize =  384,              ~
                        keypos =    1, keylen =   10

            select #3, "AWDAPPLD",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =   12, keylen =    5,                    ~
                        alt key 1,  keypos =  1,   keylen =  16,         ~
                            key 2,  keypos =  2,   keylen =  15,         ~
                            key 3,  keypos = 17,   keylen =  15

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

                                                        /* (PAR001)     */
            select #5, "EWDPRDLB",                                       ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =   35,                    ~
                        alt key  1, keypos = 278, keylen =  23
                                                        /* (PAR001)     */
                                                        /* (AWD002)     */

            select #6,  "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos = 1,    keylen = 51,                      ~
                        alt key 1, keypos = 27, keylen = 25,             ~
                            key 2, keypos = 70, keylen =  8, dup,        ~
                            key 3, keypos = 78, keylen =  8, dup,        ~
                            key 4, keypos = 52, keylen =  8,             ~
                            key 5, keypos = 36, keylen = 16, dup


            select #7,  "APCPLNSC",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 24,   keylen = 10,                      ~
                        alt key 1, keypos =  7, keylen = 27,             ~
                            key 2, keypos =  1, keylen = 33


                                                        /* (AWD002)     */
                                                        /* (PAR006)     */

            select #8, "AWDAPPLS",                                       ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =   20,                    ~
                        alt key 1,  keypos = 21,   keylen =  34, dup ,   ~
                            key 2,  keypos = 23,   keylen =  32, dup ,   ~
                            key 3,  keypos = 56,   keylen =  10, dup
                                                          /* (PAR006)   */
                                                          /* (AWD002)   */
                                                          /* (AWD009)   */
            select #9,  "AWDTRAIL",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =  20,                     ~
                        alt key  1, keypos =   21, keylen =  20
                                                          /* (AWD009)   */
                                                          /* CR1918     */
            select #10,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup
                                                          /* (AWD002)   */
                                                        /* (PAR000)     */
            select #63, "BCKSUBPT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  11,                     ~
                        alt key  1, keypos =  12, keylen =  11, dup,     ~
                            key  2, keypos =  23, keylen =  45, dup

        chr_tbl$(01) = " "
        chr_tbl$(02) = "A"
        chr_tbl$(03) = "B"
        chr_tbl$(04) = "C"
        chr_tbl$(05) = "D"
        chr_tbl$(06) = "E"
        chr_tbl$(07) = "F"
        chr_tbl$(08) = "G"
        chr_tbl$(09) = "H"
        chr_tbl$(10) = "I"
        chr_tbl$(11) = "J"
        chr_tbl$(12) = "K"
        chr_tbl$(13) = "L"
        chr_tbl$(14) = "M"
        chr_tbl$(15) = "N"
        chr_tbl$(16) = "O"
        chr_tbl$(17) = "P"
        chr_tbl$(18) = "Q"
        chr_tbl$(19) = "R"
        chr_tbl$(20) = "S"
        chr_tbl$(21) = "T"
        chr_tbl$(22) = "U"
        chr_tbl$(23) = "V"
        chr_tbl$(24) = "W"
        chr_tbl$(25) = "X"
        chr_tbl$(26) = "Y"
        chr_tbl$(27) = "Z"
                                                       /* (PAR000)     */

            call "SHOSTAT" ("Opening Files, One Moment Please")

            filename$ = "AWDAPPOR" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDAPPSC" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDAPPLD" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "GENCODES" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "EWDPRDLB" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
                                                          /* (AWD002)    */

            filename$ = "APCPLNOR" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNSC" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
                                                          /* (AWD002)    */
            call "OPENCHCK" (#08, 0%, f2%(8%), 100%, " ") /* Create File */
                                                          /* (AWD002)   */
                                                          /* (AWD009)    */
            call "OPENCHCK" (#09, 0%, f2%(9%), 100%, " ") /* Create File */
                                                          /* (AWD009)   */

            filename$ = "BCKMASTR" : call "EWDOPEN" (#10, filename$, err%)
            if err% <> 0% then gosub open_error

                                                           /* (PAR000)  */
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#63, filename$, err%)
            if err% <> 0% then gosub open_error
                                                           /* (PAR000)  */

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
            call "TIME" (time$)

            err%    = 0%                        /* (PAR003)             */
            call "SCHEMA" (schema$,             /* What switch 1-NC 2-NE*/~
                           schema%,             /* Schema               */~
                           #4,                  /* GENCODES             */~
                           err% )               /* error                */

            if err% = 0% then goto SS_1
               errormsg$ = "(Error) Schema Lookup Error!!!"
               gosub error_prompt
               end

SS_1:                                           /* (PAR003)              */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   6%
L10110:         gosub'051(fieldnr%)        /* Default - Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display - Accept  */
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
                  if keyhit%  = 16% then gosub create_labels
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 6% then editpg1
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

                                                       /* (AWD009)     */
        REM *************************************************************~
            *     I N P U T   T O   P R I N T   L O A D   L A B E L     *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode_a
            gosub initialize_trailer

            for fieldnr% = 1% to   2%
L11110:         gosub'052(fieldnr%)        /* Default - Enables */
                      if enabled% = 0% then L11230
L11130:         gosub'102(fieldnr%, 1%)    /* Display - Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L11215
L11160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L11130
                         if fieldnr% = 1% then L11110
                         goto L11160
L11215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L11130
L11230:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L11130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   F O R   L O A D   L A B E L    *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then gosub create_trailer_labels
                  if keyhit%  =  8% then gosub delete_trailer
                  if keyhit% <>  0% then       editpg2
L12120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 2% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L12170:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L12170
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L12170
                  lastfieldnr% = fieldnr%
            goto L12120

                                                       /* (AWD009)     */
        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%

        return
                                                      /* (AWD009)      */
        deffn'052(fieldnr%)
            enabled% = 1%

        return
                                                      /* (AWD009)      */
        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
            *-----------------------------------------------------------*~
            * Initializes Variable Field Input Messages                 *~
            *************************************************************

        deffn'050(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28110
                inpmessage$ = edtmessage$
                return

L28110
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return
                                                    /* (AWD002)         */
        scrn1_msg  :  data                                               ~
         "Enter Proc? (1) Create App.,(2) Print App.,(3) Create No-App.,~
         ~ (4) Print Non-App.?",~
         "Enter a Valid Appian Load Number (Required) and Printer (A)-(I)?",~
         "Enter a Valid Loading Dock Number (Optional) (00-99) or Alpha?",~
         "Enter a Valid Appian Drop Number or 'AL'?                    ",~
         "Enter a Valid Customer Sales Order or 'ALL'?                 ",~
         "Enter a Valid Production Barcode or 'ALL'?                   "
                                                   /* (AWD002)          */
                                                   /* (AWD009)          */
        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28210
                inpmessage$ = edtmessage$
                return

L28210
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Valid trailer Number that is not in use(Required)?   ",~
         "Enter a Valid Appian Load Number (Required) and Printer (A)-(I)?"

                                                   /* (AWD009)          */

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
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, sc_process$, sc_process_n$, ~
                      sc_load$, sc_load_name$, sc_dock$, sc_drop$, sc_so$,~
                      sc_so_name$, sc_barcode$, sc_prt$, ap_filler1$,     ~
                      ap_filler2$, ap_label$, dock_door$  /* (PAR006)    */
                                                          /* (AWD012)    */
                                                          /* (AWD002)    */
             delete% = 0%                                 /* (AWD005)    */

        return
                                                        /* (AWD009)     */
        initialize_trailer
            init(" ") errormsg$, inpmessage$, sc_trailer$, sc_load$,     ~
                      tr_key0$, tr_stat$, tr_trailer$, tr_load$,         ~
                      tr_open_dte$, tr_open_tme$, tr_closed_dte$,        ~
                      tr_closed_tme$, tr_fil$, tr_rec$, tr_key1$, tr_date$

        return
                                                       /* (AWD009)     */

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        REM dataload

        REM return



        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        dataput
/* CR2787 */
            gosub check_dup_so
            
            init(" ") lb_app_key0$
            rec% = 0%

            str(lb_app_key0$,1%,18%)  = ap_bar$
            str(lb_app_key0$,19%,2%)  = ap_drop$

            read #8,hold,key 0% = lb_app_key0$, using L60150,            ~
                                          lb_app_rec$(), eod goto L31000
               delete #8


               if delete% = 1% then return                    /*(AWD005) */

               str(lb_app_rec$(),577%,2%) = str(sc_dock$,1%,2%) /*(AWD012)*/

               str(lb_app_rec$(),596%,1%) = str(sc_dock$,3%,1%) /*(AWD012)*/

REM    call "SHOSTAT" (" SC_DOCK$ --> " & sc_dock$)  stop
REM    call "SHOSTAT" (" AP_DOCK$ --> " & AP_dock$)  stop
REM    call "SHOSTAT" (" LB_REC   --> " & str(lb_app_rec$(),577%,2%))  stop

               put #8, using L60150, lb_app_rec$()
               write #8, eod goto L31000

               rec% = 1%


               goto dataput_done         /* Finished-Preserve Exisiting*/

L31000:
                                         /* (AWD010)                   */
        REM gosub lookup_job             /* (PAR006) Remove Lookup     */
                                         /* (AWD010)                   */

                                         /* (AWDAPPLS)                 */
        gosub calc_drop_nbr
        put #8, using L35040,                                            ~
            ap_bar$,                     /* Production Barcode         */~
            ap_drop$,                    /* Appian Drop Number         */~
            ap_region$,                  /* Appian Region Code         */~
            ap_load$,                    /* Appian Load Number         */~
            ap_drop$,                    /* Appian Drop Number         */~
            ap_cuscode$,                 /* Appian Customer Code       */~
            ap_po$,                      /* Appian Purchase Order No.  */~
            ap_mull$,                    /* Wood Surround or Part Code */~
            ap_so$,                      /* Appian S.O. Number         */~
            ap_line$,                    /* S.O. Line Number           */~
            ap_series_style$,            /* Product Series an Style    */~
            ap_seq$,                     /* Production Sequence Number */~
            ap_ear_dis$,                 /* Appian Earliest Disp Date  */~
            ap_config_lne$,              /* W.W. Config Code           */~
            ap_arr_dte$,                 /* Appian Arrival Date        */~
            ap_cusname$,                 /* Customer Name for S.O.     */~
            str(ap_address$,1%,30%),     /* Customer Address (1)       */~
            str(ap_address$,31%,30%),    /* Customer Address (2)       */~
            ap_city$,                    /* Customer City              */~
            ap_state$,                   /* Customer State Code        */~
            ap_zip$,                     /* Customer Zip Code          */~
            ap_part$,                    /* MFG Part Number            */~
            ap_desc$,                    /* Oracle Description         */~
            ap_howship$,                 /* How Ship Code              */~
            ap_old_load$,                /* Old Caelus Load No.        */~
            ap_old_drop$,                /* Old Caelus Drop No.        */~
            ap_act_dte$,                 /* App Act. Arrival Date      */~
            ap_act_tme$,                 /* App Act Arr Time Military  */~
            ap_truck$,                   /* App Truck Number           */~
            ap_start_date$,              /* App Ship Date              */~
            ap_shp_tme$,                 /* App Act Ship Time Military */~
            ap_scan_dte$,                /* App Scann Date             */~
            ap_scan_tme$,                /* App Scan Time Military     */~
            ap_wood$,                    /* Wood Surr Fabrication Code */~
            ap_samp$,                    /* Sample Disp Flag. 0, 1, 2  */~
            ap_contract$,                /* Contractor Name            */~
            ap_job$,                     /* Filler Area        (PAR006)*/~
            ap_room$,                    /* Filler Area        (PAR006)*/~
            ap_nominal$,                 /* Niminal Window             */~
            ap_dock$,                    /* Loading Dock Number        */~
            ap_stag_dte$,                /* Date Printed and Staged    */~
            ap_stag_tme$,                /* Time Printed and Staged    */~
            ap_label$,                   /* App Label (Y)or(N) (AWD002)*/~
            ap_job1$,                    /* Filler Area        (PAR006)*/~
            ap_dock1$,                   /* Additional Dock SLB(AWD012)*/~
            ap_job_new$,                 /* New Job Name       (PAR006)*/~
            ap_room_new$,                /* New Room Location  (PAR006)*/~
            ap_prd_dte$,                 /* Production date    (AWD015)*/~
            date,                        /* Creation Date      (AWD015)*/~
            ap_filler1$,                 /* Filler Area 1      (PAR006)*/~
            ap_filler2$,                 /* Filler Area 2      (PAR006)*/~
            ap_dropnbr$                  /* drop # for awdpla05        */

            write #8, eod goto dataput_done

            rec% = 1%
        dataput_done

        return

/* <AWD016> */
calc_drop_nbr:
            curr_load$ = ap_load$
            curr_drop$ = ap_drop$
            curr_po$   = ap_po$
        if curr_load$ = prev_load$ and                      ~
           curr_drop$ = prev_drop$ and                      ~
           curr_po$   = prev_po$   then goto continue
        if curr_load$ <> prev_load$ or                       ~
           curr_drop$ <> prev_drop$ then goto reset_ex

            if d2 > 1 then goto double_chr
            d1 = d1 + 1
        if d1 < 28 then goto continue
            d1 = 2
            d2 = 2
            goto continue

double_chr: d2 = d2 + 1
            if d2 < 28 then goto continue
            d1 = d1 + 1
            d2 = 2
        goto continue

reset_ex:   d1 = 2
            d2 = 1

continue:   message$ = prev_po$ & "/" & prev_load$ & "/" & prev_drop$ & "#"
            prev_load$ = curr_load$
            prev_drop$ = curr_drop$
            prev_po$   = curr_po$
            ap_dropnbr$ = str(curr_load$,4,2) & "-" & ap_drop$ & ~
                        chr_tbl$(d1) & chr_tbl$(d2)

            return
/* </AWD016> */

/* CR2787 */
        check_dup_so
            init(" ") lb_app_key0$

            str(lb_app_key0$,1%,18%)  = ap_bar$
            str(lb_app_key0$,19%,2%)  = "  "

        nxt_check_dup
            read #8,hold,key 0% > lb_app_key0$, using L60150,            ~
                                          lb_app_rec$(), eod goto L34000
              
              lb_app_key0$ = str(lb_app_rec$(),1%,20%)
              
              if str(lb_app_rec$(),1%,18%) <> ap_bar$ then L34000
              if str(lb_app_rec$(),23%,5%) =  ap_load$ then goto nxt_check_dup
              
              delete #8
L34000:
                       
        return
        
        trailer_update
            trailer% = 0%
            init(" ") tr_key0$
            str(tr_key0$,1%,1%)  = "0"
            str(tr_key0$,2%,8%)  = sc_trailer$
            str(tr_key0$,10%,5%) = sc_load$

            read #9,hold,key 0% > tr_key0$, using L35060, tr_rec$,        ~
                                              eod goto trailer_update_now
                                              /* Mod                    */
               if str(tr_key0$,1%,14%) <> str(tr_rec$,1%,14%) then        ~
                                                   goto trailer_update_now

               tr_stat$       = "0"        /* Don't change existing data */
               tr_trailer$    = sc_trailer$
               tr_load$       = sc_load$
               str(tr_date$,1%,6%) = str(tr_rec$,15%,6%)
               str(tr_open_dte$,1%,6%) = str(tr_rec$,41%,6%)
               tr_open_tme$   = str(tr_rec$,47%,8%)
               tr_closed_dte$ = str(tr_rec$,55%,6%)
               tr_closed_tme$ = str(tr_rec$,61%,8%)
               tr_fil$        = "             "

               delete #9
               goto trailer_update_now_1

trailer_update_now

            tr_stat$                = "0"         /* Open              */
            tr_trailer$             = sc_trailer$
            tr_load$                = sc_load$
            str(tr_date$,1%,6%)     = date
            str(tr_open_dte$,1%,6%) = date
            tr_open_tme$            = time
            tr_closed_dte$          = " "
            tr_closed_tme$          = "00000000"
            tr_fil$                 = "             "

trailer_update_now_1
                                         /* (AWDTRAIL) - Trailer File  */
        put #9, using L35050,                                            ~
                tr_stat$,                /* Trailer Status Code        */~
                tr_trailer$,             /* Trailer Number             */~
                tr_load$,                /* Load Number Assigned       */~
                tr_date$,                /* Date Load Assigned to Trail*/~
                tr_stat$,                /* Trailer Status Code        */~
                tr_load$,                /* Load Number Assigned       */~
                tr_trailer$,             /* Trailer Number             */~
                tr_date$,                /* Date Load Assinged         */~
                tr_open_dte$,            /* Trailer Open Date          */~
                tr_open_tme$,            /* Trailer Open Time          */~
                tr_closed_dte$,          /* Trailer Closed Date        */~
                tr_closed_tme$,          /* Trailer Closed Time        */~
                tr_fil$                  /* Filler Area                */~

            write #9, eod goto trailer_update_done

            trailer% =1%
trailer_update_done

        return
                                         /* (AWD009)                   */

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

                                         /* File = (AWDAPPLS)          */
L35040: FMT CH(18),                      /* Production Barcode         */~
            CH(02),                      /* Appian Drop Number         */~
            CH(02),                      /* Appian Region Code         */~
            CH(05),                      /* Appian Load Number         */~
            CH(02),                      /* Appian Drop Number         */~
            CH(09),                      /* Appian Customer Code       */~
            CH(16),                      /* Appian Purchase Order No.  */~
            CH(01),                      /* Wood Surround or Part Code */~
            CH(08),                      /* Appian S.O. Number         */~
            CH(02),                      /* S.O. Line Number           */~
            CH(08),                      /* Product Series an Style    */~
            CH(05),                      /* Production Sequence Number */~
            PD(11,1),                    /* Appian Earliest Disp Date  */~
            CH(03),                      /* W.W. Config Code           */~
            PD(11,1),                    /* Appian Arrival Date        */~
            CH(30),                      /* Customer Name for S.O.     */~
            CH(30),                      /* Customer Address (1)       */~
            CH(30),                      /* Customer Address (2)       */~
            CH(18),                      /* Customer City              */~
            CH(02),                      /* Customer State Code        */~
            CH(09),                      /* Customer Zip Code          */~
            CH(25),                      /* MFG Part Number            */~
            CH(250),                     /* Oracle Description         */~
            CH(02),                      /* How Ship Code              */~
            CH(05),                      /* Old Caelus Load No.        */~
            CH(02),                      /* Old Caelus Drop No.        */~
            PD(11,1),                    /* App Act. Arrival Date      */~
            CH(04),                      /* App Act Arr Time Military  */~
            CH(05),                      /* App Truck Number           */~
            PD(11,1),                    /* App Ship Date              */~
            CH(04),                      /* App Act Ship Time Military */~
            PD(11,1),                    /* App Scann Date             */~
            CH(04),                      /* App Scan Time Military     */~
            CH(06),                      /* Wood Surr Fab. Code        */~
            CH(01),                      /* Sample or Disp 0,1,2       */~
            CH(16),                      /* Contractor Name            */~
            CH(10),                      /* Customer Job name          */~
            CH(06),                      /* Room Location              */~
            CH(06),                      /* Niminal Window Size        */~
            CH(02),                      /* Loading Dock Number        */~
            PD(11,1),                    /* Date Label Printed and Stag*/~
            CH(04),                      /* Time Label Printed-Stag    */~
            CH(01),                      /* APP Label (Y)or(N) (AWD002)*/~
            CH(06),                      /* Last Six of Job    (AWD010)*/~
            CH(01),                      /* Addit for Dock Char(AWD012)*/~
            CH(16),                      /* Customer Job Name  (PAR006)*/~
            CH(16),                      /* Customer Room Loc  (PAR006)*/~
            CH(06),                      /* Prd Date           (AWD015)*/~
            CH(06),                      /* Date Created       (AWD015)*/~
            CH(256),                     /* Filler Area 1      (PAR006)*/~
            CH(120),                     /* Filler Area 2      (PAR006)*/~
            CH(008)                      /* drop #             (AWD016)*/

                                         /* (AWDTRAIL) - Trailer File  */
L35050: FMT     CH(01),                  /* Trailer Status Code        */~
                CH(08),                  /* Trailer Number             */~
                CH(05),                  /* Load Number Assigned       */~
                CH(06),                  /* Date Assigned              */~
                CH(01),                  /* Trailer Status             */~
                CH(05),                  /* Load Number Assigned       */~
                CH(08),                  /* Trailer Number             */~
                CH(06),                  /* Date Assigned              */~
                CH(06),                  /* Trailer Open Date          */~
                CH(08),                  /* Trailer Open Time          */~
                CH(06),                  /* Trailer Closed Date        */~
                CH(08),                  /* Trailer Closed Time        */~
                CH(60)                   /* Filler Area                */~

                                         /* (AWDTRAIL) - Trailer File  */
L35060: FMT     CH(128)                   /* Trailer Record            */~

                                         /* (AWD009)                   */
  
        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
                                                         /* (AWD002)   */
              on fieldnr% gosub L40170,     /* Process 1=CrtA,2=PrtA,  */~
                                            /* 3=CrtN,4=PrtN           */~
                                L40160,     /* Appian Load Number      */~
                                L40160,     /* Loading Dock Door Number*/~
                                L40160,     /* Appian Drop Number      */~
                                L40160,     /* Customer S.O. Number    */~
                                L40160      /* Production Barcode      */

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low    */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only  */
L40170:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric     */
                                                        /* (AWD002)    */
                                                        /* (AWD012)    */
L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Proc 1, 2, 3, 4  :",                         ~
               at (03,25), fac(lfac$(1%)), sc_process$          , ch(01),~
               at (03,40), fac(hex(84)),   sc_process_n$        , ch(35),~
                                                                         ~
               at (04,02), "Appian Load No.  :",                         ~
               at (04,25), fac(lfac$(2%)), sc_load$             , ch(05),~
               at (04,35), fac(lfac$(2%)), sc_prt$              , ch(01),~
               at (04,40), fac(hex(84)),   sc_load_name$        , ch(35),~
                                                                         ~
               at (05,02), "Loading Dock Door:",                         ~
               at (05,25), fac(lfac$(3%)), sc_dock$             , ch(03),~
               at (05,35), fac(hex(84)),   dock_door$           , ch(35),~
                                                                         ~
               at (06,02), "Appian Drop No.  :",                         ~
               at (06,25), fac(lfac$(4%)), sc_drop$             , ch(02),~
                                                                         ~
               at (07,02), "Sales Order No.  :",                         ~
               at (07,25), fac(lfac$(5%)), sc_so$               , ch(08),~
               at (07,40), fac(hex(84)),   sc_so_name$          , ch(35),~
                                                                         ~
               at (08,02), "Prod. Barcode No.:",                         ~
               at (08,25), fac(lfac$(6%)), sc_barcode$          , ch(18),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
                                               /*  (AWD009)           */
               if keyhit% <> 10% then goto L40200
                  return clear all
                  goto inputmode_a
                                               /*  (AWD009)           */
                                               /*  (AWD005) -  BEGIN  */
L40200:        if keyhit% <> 12% then goto L40410
                  pass% = 0%
                  call "APCPASSW" ("AWDPLN05", userid$, pass%)
                       if pass% <> 0% then goto L40420
                  delete% = 1%
                  gosub delete_data
                 return clear all
                 goto inputmode
                                               /*  (AWD005) -  END    */

L40410:        if keyhit% <> 15% then goto L40420
                  call "PRNTSCRN"
                  goto L40190

L40420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        call "ALLFREE"                /* CR2450                  */
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &        ~
                      "                     (10)Trailer Labels"
            pf$(2%) = "                                        " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &        ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffff0affffffff0f1000)
            if fieldnr% = 1% then L40570
                                            /* (AWD009)               */
                str(pf$(1%),60%) = " " : str(pfkeys$,10%,1%) = hex(ff)
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                      (12)Delete Data   " &        ~
                      "                      (15)Print Screen "
            pf$(3%) = "                                        " &        ~
                      "                      (16)PRINT LABELS "
            pfkeys$ = hex(01ffffffffffffffffffff0cffff0f1000)

            if sc_process$ = "1" then                                     ~
                str(pf$(3%),67%) = "CR Appian LB"

            if sc_process$ = "2" then                                     ~
                str(pf$(3%),67%) = "Print Appian"

            if sc_process$ = "3" then                                     ~
                str(pf$(3%),67%) = "CR NonAppian"

            if sc_process$ = "4" then                                     ~
                str(pf$(3%),67%) = "Print NonApp"

            return
L40700:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                                       "
            pf$(3%) = "                                        " &        ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return
                                                      /* (AWD002)      */
                                                      /* (AWD009)      */
        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'060(1%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
                                                         /* (AWD002)   */
              on fieldnr% gosub L41160,     /* Trailer Number          */~
                                L41160      /* Appian Load Number      */

              goto L41190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low    */
L41160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only  */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric     */
                                                        /* (AWD002)    */
L41190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Trailer Number   :",                         ~
               at (03,25), fac(lfac$(1%)), sc_trailer$          , ch(08),~
               at (03,40), fac(hex(84)),   sc_trailer_desc$     , ch(30),~
                                                                         ~
               at (04,02), "Appian Load No.  :",                         ~
               at (04,25), fac(lfac$(2%)), sc_load$             , ch(05),~
               at (04,35), fac(lfac$(2%)), sc_prt$              , ch(01),~
               at (04,40), fac(hex(84)),   sc_load_name$        , ch(35),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <> 15% then goto L41420
                  call "PRNTSCRN"
                  goto L41190

L41420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        call "ALLFREE"                /* CR2450                  */
        if edit% = 2% then L41610     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &        ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L41570
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L41570:     if fieldnr% > 1% then L41590
                str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L41590:     return

L41610: if fieldnr% > 0% then L41700  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                      (15)Print Screen "
            pf$(3%) = "(8)Delete Trailer                       " &        ~
                      "                      (16)PRINT LABELS "
            pfkeys$ = hex(01ffffffffffff08ffffff0cffff0f1000)

            return
L41700:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                                       "
            pf$(3%) = "                                        " &        ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return
                                                      /* (AWD009)      */
        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "

            on fieldnr% gosub L50010,        /* Process 1, 2, 3, 4     */~
                              L50050,        /* Appian Load No.        */~
                              L50075,        /* Loading Dock Number    */~
                              L50100,        /* Appian Drop No. or AL  */~
                              L50200,        /* Cust. S.O. No. or ALL  */~
                              L50300         /* Prod. Barcode No or ALL*/

            return

L50010: Rem Enter a Reprint Code              sc_process$, sc_process_n$
                                           /* (1) = Create Appian Labels */
                                           /* (2) = Print Shipping Labels*/
                                           /* (3) = Create Non-Appian Lab*/
                                           /* (4) = Print Non_Appian Lab */
            init(" ") sc_process_n$
            p% = pos("1234" = sc_process$)   /* (AWD002)               */
            if p% = 0% then goto L50015
            if sc_process$ = "1" then                                  ~
                       sc_process_n$ = "Create Appian Shipping Labels   "
            if sc_process$ = "2" then                                  ~
                       sc_process_n$ = "Print Appian Shipping Labels    "
            if sc_process$ = "3" then                                  ~
                       sc_process_n$ = "Create Non-Appian Ship   Labels "
            if sc_process$ = "4" then                                  ~
                       sc_process_n$ = "Print Non-Appian Shipping Labels"
                                                   /* (AWD004)        */
            if sc_process$ <> "3" then return

               pass% = 1%
               call "APCPASSW" ("AWDPLN05", userid$, pass%)
                      if pass% <> 0% then goto L50020

                                                  /* (AWD004)        */

        return
L50015:     errormsg$ = "(Error) Invalid Label Process Code (1,2,3,4) ?"
            gosub error_prompt
            init (" ") sc_process$, sc_process_n$
        return                                    /* (AWD002)        */
                                                  /* (AWD004)        */
L50020:     errormsg$ = "(Error) Invalid Password 'You cannot run process'"
            gosub error_prompt
            init(" ") sc_process$, sc_process_n$
        return
                                                  /* (AWD004)        */
L50050: Rem Enter a Valid Appian Load Number      sc_load$, sc_load_name$
            init(" ") sc_load_name$

            convert sc_load$ to sc_load%, data goto L50055

            convert sc_load% to sc_load$, pic(00000)

            goto L50057
L50055:     convert str(sc_load$,2%,4%) to scr_load%, data goto L50064

            convert scr_load% to str(sc_load$,2%,4%), pic(0000)
L50057:                                            /* Printer Selection*/
           if sc_prt$ = " " then sc_prt$ = "A"
                                                   /* (AWD007)        */
                                                   /* (AWD014)        */
                                                   /* (PAR004)        */
                                                   /* (SR70701)       */
                                                   /* (CR456) +       */
REM           IF SC_PRT$ <> "A" AND SC_PRT$ <> "B" AND SC_PRT$ <> "T" AND ~
REM              SC_PRT$ <> "C" AND SC_PRT$ <> "D" AND SC_PRT$ <> "Z" AND ~
REM              SC_PRT$ <> "E" AND SC_PRT$ <> "G" THEN SC_PRT$ = "A"
            p% = 0%
            p% = pos("ABCDEFGTZHIJ" = scr_prt$)
            if p% = 0% then scr_prt$ = "A"
                                                   /* (CR456)  -      */
                                                   /* (PAR004)        */
                                                   /* (AWD014)        */
            gosub check_load
            if sc_load% = 0% then goto L50064
                                                   /* Verify Selection*/
            if sc_load% = 2% then goto L50059
                                                   /* Must be Appian  */
               if sc_process$ <> "1" and sc_process$ <> "2" then       ~
                                                   goto L50066
                  goto L50062
L50059:                                            /* Non_Appian      */
               if sc_process$ <> "3" and sc_process$ <> "4" then       ~
                                                   goto L50068
/* CR3234 */
            if str(sc_load$,1%,1%) = "P" and schema% = 1% then goto L50069												   

L50062:
                                                   /* (AWD013)        */
        REM       IF SC_PROCESS$ = "1" OR SC_PROCESS$ ="3" THEN RETURN

                                                   /* Lookup Dock door*/
               gosub lookup_dock_door
           if dock% = 0% then return               /* (AWD013)        */
               fieldnr% = 3%                       /* for Processes   */
               lfac$(3%) = hex(84)                 /* (2) and (4)     */

        return

L50064:     errormsg$ = "(Error) Invalid Load Number, Not Defined?"
            gosub error_prompt
            init(" ") sc_load$, sc_load_name$
        return
L50066:     errormsg$ = "(Error) Invalid Appian Load Number?"
            gosub error_prompt
            init(" ") sc_load$, sc_load_name$
        return
L50068:     errormsg$ = "(Error) Invalid Non-Appian Load Number?"
            gosub error_prompt
            init(" ") sc_load$, sc_load_name$
        return
/* CR3234 */		
L50069:     errormsg$ ="(Error) Load number cannot begin with P"
            gosub error_prompt
            init(" ") sc_load$, sc_load_name$
        return
                                                   /* (AWD002)       */
L50075: Rem Loading Dock Number                            sc_dock$
            if sc_dock$ <> " " then goto L50080
            if sc_dock$ <> " " then goto L50080
               sc_dock$ = "00 "                    /* (AWD012)       */

L50080:
/* (CR932) */ IF STR(SC_DOCK$,1%,2%) <> "BP" THEN GOTO L50081
               if str(sc_dock$,3%,1%) <> "B" then goto L50081
REM STR(SC_DOCK$,3%,1%) = " "
               gosub bp_prompt
               if comp% = 0% then goto L50090
               goto L50084
/*(SR73756)*/
L50081:        table$ = "DOCK DOOR"
               code$ = sc_dock$
               gosub check_code
               if code% = 0% then goto L50082
               sc_dock$ = str(code$,1%,3%)
               ap_dock$ = str(sc_dock$,1%,2%)

               goto L50085

L50082:     convert str(sc_dock$,1%,2%) to sc_dock%, data goto L50090

            convert sc_dock% to str(sc_dock$,1%,2%), pic(00)

L50084:     ap_dock$ = str(sc_dock$,1%,2%)   

            if str(sc_dock$,3%,1%) = "S" then goto L50085 /*(AWD013)REMOVE MEG*/
            if str(sc_dock$,3%,1%) = "L" then goto L50085 /*(AWD013)REMOVE MEG*/
            if str(sc_dock$,3%,1%) = "B" then goto L50085
            if str(sc_dock$,3%,1%) = "U" then goto L50085 /*(AWD013)REMOVE MEG*/
            if str(sc_dock$,3%,1%) = "H" then goto L50085 /* CR1073 */
            if str(sc_dock$,3%,1%) = " " then goto L50085 /* CR1073 */
            
            goto L50090
            
        /*    str(sc_dock$,3%,1%) = " "          (CR932) (CR1073 remove)*/
        
L50085:
            ap_dock1$= str(sc_dock$,3%,1%)        /* (AWD012)      */

        return
L50090:     errormsg$ = "(Error) Invalid Loading Dock Number?"
            gosub error_prompt
            init(" ") sc_dock$
        return

L50100: Rem Enter a valid Appian Drop Number               sc_drop$
                                                    /* (AWD002)      */
            if sc_drop$ <> " " then goto L50104
               sc_drop$ = "AL"

L50104:     if sc_drop$ = "AL" then return

            convert sc_drop$ to sc_drop%, data goto L50110

            convert sc_drop% to sc_drop$, pic(00)


               gosub check_drop
               if sc_drop% = 0% then goto L50110

        return
L50110:     errormsg$ = "(Error) Invalid Load Drop Number?"
            gosub error_prompt
            init(" ") sc_drop$
        return

L50200: Rem Enter a Valid S.O. No.              sc_so$, sc_so_name$
            init(" ") sc_so_name$
            if sc_so$ <> " " then goto L50205
               sc_so$ = "ALL      "

L50205:     if str(sc_so$,1%,3%) <> "ALL" then goto L50210
               sc_so_name$ = "All Sales Orders for Appian Load"
               return

L50210:      gosub check_so
            if sc_so% = 0% then goto L50220
        return

L50220:     errormsg$ = "(Error) Invalid Sales Order for Appian Load?"
            gosub error_prompt
            init(" ") sc_so$, sc_so_name$
        return

L50300: Rem Enter a Production Barcode      sc_barcode$
            if sc_barcode$ <> " " then goto L50302
               sc_barcode$ = "ALL               "

L50302:     if str(sc_barcode$,1%,3%) <> "ALL" then goto L50310
               sc_barcode$ = "ALL               "
        return

L50310:     gosub check_barcode
            if sc_barcode% = 0% then goto L50320
        return
L50320:     errormsg$ = "(Error) Invalid Production Barcode?"
            gosub error_prompt
            init(" ") sc_barcode$
        return
                                                   /* (AWD009)          */
        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************


        deffn'152(fieldnr%)
            errormsg$ = " "

            on fieldnr% gosub L50400,        /* Trailer Number         */~
                              L50500         /* Appian Load No.        */

            return

L50400: REM Trailer Number                            sc_trailer$
             gosub lookup_trailer
             if check% = 0% then goto L50525

             init(" ") tr_key0$
             str(tr_key0$,1%,1%) = "0"           /*Look for Open Trailer */
             str(tr_key0$,2%,8%) = sc_trailer$   /*Set Trailer No.       */

             read #9, key 0% > tr_key0$, using L35060, tr_rec$,           ~
                                                        eod goto L50420

             IF str(tr_rec$,1%,9%) = str(tr_key0$,1%,9%) then goto L50430
L50420:
        return
L50525:     errormsg$ = "(Error)-Invalid Trailer Number?"
            gosub error_prompt
            init(" ") sc_trailer$
        return
                                                 /*Allow Reprint of Label*/
L50430:     sc_load$ = str(tr_rec$,10%,5%)
            gosub L50500
            fieldnr% = 2%
        return

L50500: REM Load Number                      sc_load$
            sc_load% = 0%
            convert sc_load$ to sc_load%, data goto L50510
L50510:
            convert sc_load% to sc_load$, pic(00000)

            if sc_prt$ = " " then sc_prt$ = "A"
                                                  /* (AWD014)         */
                                                  /* (PAR004)         */
                                                  /* (SR70701)        */
                                                  /* (CR456)    +     */
REM            IF SC_PRT$ <> "A" AND SC_PRT$ <> "B" AND SC_PRT$ <> "T" AND ~
REM               SC_PRT$ <> "C" AND SC_PRT$ <> "D" AND SC_PRT$ <> "Z" AND ~
REM               SC_PRT$ <> "E" AND SC_PRT$ <> "G"  THEN SC_PRT$ = "A"
            p% = 0%
            p% = pos("ABCDEFGTZHIJ" = scr_prt$)
            if p% = 0% then scr_prt$ = "A"
                                                  /* (CR456)   -      */
                                                  /* (PAR004)         */
                                                  /* (AWD014)         */

            gosub check_load
                                               /* Check for Valid        */
                                               /* Appian/Non-Appian Load */
            if sc_load% = 0% then goto L50520
                                               /* Check load Assigned    */

            init(" ") tr_key1$
            str(tr_key1$,1%,1%) = "0"          /* Look for Open Trailer  */
            str(tr_key1$,2%,5%) = sc_load$     /* Set load No/           */

             read #9, key 1% > tr_key1$, using L35060, tr_rec$,          ~
                                                           eod goto L50515

             IF str(tr_rec$,21%,6%) = str(tr_key1$,1%,6%) then goto L50530
L50515:
        return
L50520:     errormsg$ = "(Error)-Invalid Appian/Non-Appian Load Number?"
            gosub error_prompt
            init(" ") sc_load$
        return
L50530:     sc_trailer$ = str(tr_rec$,27%,8%)
            gosub lookup_trailer
            errormsg$ = "Note: Load already assigned to Open trailer("  ~
                                                       & sc_trailer$ &")!"
            gosub error_prompt
            init(" ") errormsg$
            fieldnr% = 2%
        return

        lookup_trailer
            check% = 0%
            init(" ") readkey$, desc$, sc_trailer_desc$
            str(readkey$,1%,9%)   = "AWD TRAIL"
            str(readkey$,10%,15%) = sc_trailer$
            read #4,key = readkey$, using L51400, desc$, eod goto L50540
            sc_trailer_desc$ = desc$
            check% = 1%
L50540: return
                                                       /* (AWD009)       */

        check_load                                     /* Check(AWDAPPLD)*/
            sc_load% = 0%
            init(" ") ld_app_key0$, ld_app_rec$
            ld_app_key0$ = sc_load$

            read #3,key 0% = ld_app_key0$, using L51000, ld_app_rec$,    ~
                                                          eod goto L51050
L51000:        FMT CH(128)
                                                  /* Check for Appian    */
            if str(ld_app_rec$,83%,1%) = "Y" then sc_load% = 1%
                                                  /* Check Non-Appian    */
            if str(ld_app_rec$,83%,1%) = "N" then sc_load% = 2%
                                                  /* Check for Error     */
            if sc_load% = 0% then return

               sc_load_name$  = str(ld_app_rec$,53%,30%)
               ap_start_time$ = str(ld_app_rec$,23%,4%)
                                                  /* Loading Dock door   */
                                                  /* (AWD012)            */
               str(sc_dock$,1%,2%) = str(ld_app_rec$,84%,2%)
               str(sc_dock$,3%,1%) = str(ld_app_rec$,86%,1%)

               if sc_dock$ <> " " then goto L51040
                  sc_dock$ = "00 "

L51040:        ap_dock$  = str(sc_dock$,1%,2%)
               ap_dock1$ = str(sc_dock$,3%,1%)
                                                  /* (AWD012)            */
L51050: return

        check_drop                                  /* Check (AWDAPPOR)  */
            sc_drop% = 0%
            init(" ") or_app_key3$, or_app_rec$()
            str(or_app_key3$,1%,5%) = sc_load$
            str(or_app_key3$,6%,2%) = sc_drop$
            read #1,key 3% > or_app_key3$, using L51100, or_app_rec$(), ~
                                                       eod goto L51150
L51100:        FMT 3*CH(128)

            if str(or_app_key3$,1%,7%) <> str(or_app_rec$(),17%,7%)     ~
                                                      then goto L51150
               sc_drop% = 1%
                                                             /* (AWD005) */
L51150:     if sc_drop% <> 1% and sc_process$ = "3" then gosub check_apc_sc


        return

        check_apc_sc                                  /* (AWD005) - BEG  */
             init(" ") sc_key1$
             str(sc_key1$,1%,5%) = sc_load$

             read #7, key 1% > sc_key1$, using L51200, sc_key1$,          ~
                                             eod goto check_apc_sc_done

        check_apc_sc_nxt
             read #7, using L51200, sc_key1$, eod goto check_apc_sc_done
L51200:             FMT POS(7), CH(27)

                if str(sc_key1$,1%,5%) <> sc_load$ then               ~
                                                 goto check_apc_sc_done
                if str(sc_key1$,11%,2%) <> sc_drop$ then              ~
                                                  goto check_apc_sc_nxt
                if str(sc_key1$,11%,2%) > sc_drop$ then               ~
                                                 goto check_apc_sc_done
                      sc_drop% = 1%

        check_apc_sc_done
        return                                        /* (AWD005) - END  */

        check_so                                     /* Check (AWDAPPOR) */
            sc_so% = 0%
            init(" ") or_app_key0$, or_app_rec$()
            or_app_key0$ = sc_so$
            read #1, key 0% = or_app_key0$, using L51100, or_app_rec$(), ~
                                                        eod goto L51250
               sc_so_name$ = str(or_app_rec$(),167%,30%)
               sc_so% = 1%
L51250: return

        check_barcode
            sc_barcode% = 0%
            init(" ") lb_key1$, lb_rec$()                   /* (PAR001) */
            str(lb_key1$,1%,18%) = sc_barcode$
            read #5, key 1% > lb_key1$, using L51300, lb_key1$,          ~
                                                         eod goto L51350
L51300:        FMT POS(278), CH(23)
            if sc_barcode$ <> str(lb_key1$,1%,18%) then goto L51350
               sc_barcode% = 1%
L51350: return                                              /* (PAR001) */

        check_label                            /* Code Exists - No Label */
            check% = 0%                        /* Specific How Ships     */
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "EWDAPPHOW"
            str(readkey$,10%,15%) = str(or_app_rec$(),165%,2%)
            read #4,key = readkey$, using L51400, desc$, eod goto L51450
L51400:        FMT POS(25), CH(30)
            check% = 1%
L51450: return

        check_code                         /* Check Code GENCODES SR73756*/
           code% = 0%
           init(" ") readkey$, desc$
           str(readkey$,1%,9%)   = table$
           str(readkey$,10%,15%) = code$
           read #4,key = readkey$, using L12070, desc$, eod goto L12090
L12070:       FMT POS(25), CH(30)
           dock_door$ = desc$
           code% = 1%
L12090: return

        update_load                                /* Update (AWDAPPLD)  */
            init(" ") ld_app_key0$, ld_app_rec$
            ld_app_key0$ = sc_load$
            read #3,hold,key 0% = ld_app_key0$, using L51000, ld_app_rec$,~
                                                      eod goto L51500

            delete #3
                                                     /* (AWD012)        */
            str(ld_app_rec$,84%,2%) = str(sc_dock$,1%,2%)
            str(ld_app_rec$,86%,1%) = str(sc_dock$,3%,1%)
                                                     /* (AWD012)        */
            write #3 using L51000, ld_app_rec$, eod goto L51500

        return
L51500:     errormsg$ = "(Error) updating (AWDAPPLD) " & sc_load$
            gosub error_prompt
        return

        lookup_dock_door
                                                    /* (AWD013)         */
            dock% = 0%
            init(" ") ld_app_key0$, ld_app_rec$
            ld_app_key0$ = sc_load$
                                                    /* (PAR005)         */
                                             /* 05/08/06 - Remove Lock  */
            read #3,key 0% = ld_app_key0$, using L51000, ld_app_rec$,   ~
                                                      eod goto L52000
                                              /* (AWD012)               */
            str(sc_dock$,1%,2%) = str(ld_app_rec$,84%,2%)
            str(sc_dock$,3%,1%) = str(ld_app_rec$,86%,1%)

            ap_dock$  = str(sc_dock$,1%,2%)
            ap_dock1$ = str(sc_dock$,3%,1%)
                                                     /* (AWD012)         */
            convert ap_dock$ to dock%, data goto L51600

L51600:                                              /* (AWD013)         */

L52000: return

        delete_trailer
            gosub delete_trailer_verify

            init(" ") tr_key0$, tr_rec$
            str(tr_key0$,1%,1%) = "0"          /* Look for Open Trailer  */
            str(tr_key0$,2%,8%) = sc_trailer$  /* Set Trailer No.        */

            read #9,hold,key 0% > tr_key0$, using L35060, tr_rec$,       ~
                                                           eod goto L52010

            IF str(tr_rec$,1%,9%) = str(tr_key0$,1%,9%) then goto L52020
L52010:
            errormsg$ = "(Error) - No Trailer Record Deleted."
            gosub error_prompt
            goto L52030
L52020:
            delete #9                                /* Primary Key      */

            sc_load$ = str(tr_rec$,10%,5%)
            init(" ") tr_key0$, tr_rec$
            str(tr_key0$,1%,1%) = "0"           /* Look for Open Trailer */
            str(tr_key0$,2%,5%) = sc_load$      /* Set Load Number       */

            read #9,hold,key 1% > tr_key0$, using L35060, tr_rec$,        ~
                                                            eod goto L52025

            IF str(tr_rec$,21%,6%) <> str(tr_key0$,1%,6%) then goto L52025

               delete #9                             /* Alt Key One      */
L52025:
            errormsg$ = "Trailer Record (" & sc_trailer$ & ") Deleted !!"
            gosub error_prompt
L52030:
        return clear all
        goto inputmode

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

                                                  /* (AWD002)           */
        create_labels
            call "SHOSTAT" (sc_process_n$)
            ap_label$ = "Y"

            init(" ") or_app_key3$, or_app_rec$()
            count% = 0%
                                                  /* Reprint labels Only*/
                                                  /* No Creation        */
            if sc_process$ = "3" then goto create_non_appian_labels

            if sc_process$ = "2" then goto print_labels    /* Appian    */
            if sc_process$ = "4" then goto print_labels    /* Non-Appian*/
                                                  /* (AWD002)           */

            str(or_app_key3$,1%,5%) = sc_load$    /* Appian Load Number */

                                                  /* Appian Drop Number */
            if sc_drop$ <> "AL" then str(or_app_key3$,6%,2%) = sc_drop$

        create_labels_next
            read #1,key 3% > or_app_key3$, using L51100, or_app_rec$(),  ~
                                                eod goto create_labels_done
               if str(or_app_key3$,1%,5%) <> str(or_app_rec$(),17%,5%)   ~
                                        then goto create_labels_done
                  or_app_key3$ = str(or_app_rec$(),17%,32%)

                  if sc_drop$ = "AL" then goto create_labels_next1
                                                  /* Test for Insert    */
                     if sc_drop$ <> str(or_app_key3$,6%,2%) then          ~
                                                 goto create_labels_next
        create_labels_next1
                                                  /* Check for Valid    */
                                                  /* to print           */
                  gosub check_label
                  if check% = 1% then goto create_labels_next
                                                  /* No Label           */

                                                  /* App Drop Number    */
                  ap_drop$    = str(or_app_rec$(),22%,2%)
                                                  /* App Region Code    */
                  ap_region$  = str(or_app_rec$(),15%,2%)
                                                  /* App Load Number    */
                  ap_load$    = str(or_app_rec$(),17%,5%)
                                                  /* App Drop - Done    */
                                                  /* App Cust Code      */
                  ap_cuscode$ = str(or_app_rec$(),24%,9%)
                                                  /* App P.O. Number    */
                  ap_po$      = str(or_app_rec$(),33%,16%)
                                                  /* Wood or Part       */
                  ap_mull$ = " "
                                                  /* App S.O. Number    */
                  ap_so$      = str(or_app_rec$(),1%,8%)
/* (AWD018) beg */
                  gosub lookup_so
                  if ap_load$ <> or_load$ then goto create_labels_next
/* (AWD018) end */
                                                  /* App Earliest Dis Dt*/
                  ap_ear_dis$ = str(or_app_rec$(),9%,6%)
                                                  /* App Arrival Date   */
                  ap_arr_dte$ = str(or_app_rec$(),49%,6%)
                                                  /* App Cust Name      */
                  ap_cusname$ = str(or_app_rec$(),167%,30%)
                                                  /* App Cust Addr      */
                  ap_address$ = str(or_app_rec$(),76%,60%)
                                                  /* App Cust City      */
                  ap_city$    = str(or_app_rec$(),136%,18%)
                                                  /* App Cust State     */
                  ap_state$   = str(or_app_rec$(),154%,2%)
                                                  /* App Cust Zip       */
                  ap_zip$     = str(or_app_rec$(),156%,9%)
                                                  /* S.O. Howship       */
                  ap_howship$ = str(or_app_rec$(),165%,2%)
                                                  /* Caelus Old Load    */
                  ap_old_load$= str(or_app_rec$(),68%,5%)
                                                  /* Caelus Old Drop    */
                  ap_old_drop$= str(or_app_rec$(),73%,2%)
                                                  /* App Act Arr Date   */
                  ap_act_dte$ = " "
                                                  /* App Act Arr Time   */
                  ap_act_tme$ = "0000"
                                                  /* App Truck No.      */
                  ap_truck$   = str(or_app_rec$(),63%,5%)
                                                  /* App Start/Ship Dte */
                  ap_start_date$ = str(or_app_rec$(),197%,6%)
                                                  /* App Actual Ship Tim*/
                  ap_shp_tme$ = ap_start_time$    /* From (AWDAPPLD)    */

                                                  /* App Scan Date      */
                  ap_scan_dte$= " "
                                                  /* App Scan Time      */
                  ap_scan_tme$= "0000"

                                               /* Get Label Detail Data */
                  gosub prod_label_detail

            goto create_labels_next

        create_labels_done

                                             /* Remove label Printing */
            gosub update_load
        return clear all
        goto inputmode


        prod_label_detail                    /* (EWDPRDLB) - label Fl */
           init(" ") lb_rec$(), lb_key1$, ap_line$
           ap_prd_dte$ = all(hex(00))           /* (AWD015) */
                                             /* Barcode Key           */
           str(lb_key1$,1%,8%) = ap_so$
                                             /* (PAR001)              */
           prod_label_detail_nxt
              read #5,key 1% > lb_key1$, using L60050, lb_rec$(),      ~
                                         eod goto prod_label_detail_done
L60050:          FMT 4*CH(256)
                                             /* Set Alt Key           */
                                             /* (PAR001)              */
              lb_key1$ = str(lb_rec$(),278%,23%)
              if str(lb_key1$,1%,8%) <> ap_so$ then                ~
                                            goto prod_label_detail_done
                 if str(lb_key1$,9%,2%) <> ap_line$ then          ~
                                            gosub get_part_desc
                                             /* Set Label Barcode     */
                 ap_bar$ = str(lb_key1$,1%,18%)
                                             /* Set Series Style      */
                                             /* (PAR001)              */
                 ap_series_style$ = str(lb_rec$(),270%,8%)
                                             /* Set Production Seq No.*/
                                             /* (PAR001)              */
                 ap_seq$ = str(lb_rec$(),311%,5%)
                                             /* Set MFG Part No.      */
                                             /* (PAR001)              */
                 ap_part$ = str(lb_rec$(),523%,25%)
                                             /* (PAR000)              */
                 init(" ") so_inv$, item_no$
                 so_inv$ = str(ap_bar$,1%,8%)/* Sales Order Number    */
                 item_no$= str(ap_bar$,9%,2%)/* Line Item No.         */
                 gosub lookup_sub_part
                 ap_sub_part$ = str(bcksubpt_rec$,48%,20%)

                                             /* (PAR000)              */
                                             /* Set Wood Fab Code     */
                                             /* (PAR001)              */
                 ap_wood$ = str(lb_rec$(),594%,6%)
                                             /* Set Samp. Display Flag*/
                                             /* 0=No,1=Samp,2=Disp    */
                                             /* (PAR001)              */
                 ap_samp$ = str(lb_rec$(),600%,1%)
                                             /* Contractor Name       */
                                             /* (PAR001)              */
                 ap_contract$ = str(lb_rec$(),385%,16%)
                                             /* Job Name              */
                                             /* (PAR001)              */
                                             /* (PAR006) - New Job    */
                 ap_job$  = "          "     /* Filler Area           */
                 ap_job1$ = "      "
                 ap_job_new$ = str(lb_rec$(),688%,16%) /* New Job Name*/

                                             /* Room Location         */
                                             /* (PAR001)              */
                 ap_room$ = "      "         /* Filler Area           */

                 ap_room_new$ = str(lb_rec$(),704%,16%) /* New Room   */

                                             /* (PAR006)              */
                                             /* Nominal Window Size   */
                                             /* (PAR001)              */
                 ap_nominal$ = str(lb_rec$(),418%,6%)

                                             /* (AWD001)              */
                                             /* Date Printed and Staged*/
                 ap_stag_dte$ = " "
                                             /* Time Printed and Staged*/
                 ap_stag_tme$ = "0000"
                                             /* (AWD001)               */

                                             /* Write Shipping Label  */
                                             /* (AWD015)              */
                 ap_prd_dte$ = str(lb_rec$(),1%,6%)

                 gosub dataput
                 if rec% = 0% and sc_process$ = "1" then gosub update_error


                 goto prod_label_detail_nxt

        prod_label_detail_done

        return

        get_part_desc                                 /* (AWDAPPSC)      */
                                                      /* (AWD002)        */
            if sc_process$ = "3" then  goto get_prod_description


           ap_line$ = str(lb_key1$,9%,2%)             /* S.O Line No.    */
           init(" ") sc_app_key0$, ap_desc$, ap_config_lne$, sc_app_rec$()

           str(sc_app_key0$,1%,8%) = ap_so$           /* App S.O Number  */
           str(sc_app_key0$,9%,2%) = ap_line$         /* App Line Number */
           read #2,key 0% = sc_app_key0$, using L60100, sc_app_rec$(),    ~
                                                eod goto get_part_desc_done
L60100:       FMT 3*CH(128)
           ap_desc$       = str(sc_app_rec$(),11%,250%)
           ap_config_lne$ = str(sc_app_rec$(),261%,3%)

        get_part_desc_done
        return

        print_labels
            counts$ = "Created(xxxxx) Printed(00000)"
            convert count% to str(counts$,9%,5%), pic(#####)

            call "SHOSTAT" (counts$)
                                                  /* (AWD003)       */
            gosub print_labels_verify
                                                  /* (AWD003)       */
            been_here% = 0%
            lbl%       = 0%
            switch% = 1%
                                                  /* (AWD007)       */
            if sc_prt$ = "A" then switch% = 1%    /* MFGSTAG        */
            if sc_prt$ = "B" then switch% = 1%    /* MFGSHIP        */
                                                  /* (PAR003)       */
            if sc_prt$ = "B" and schema% = 2% then                   ~
                                  switch% = 2%    /* MFGSHIP        */
                                                  /* (PAR003)       */
            if sc_prt$ = "T" then switch% = 3%    /* MFGTEST        */
                                                  /* (AWD014)       */
            if sc_prt$ = "C" then switch% = 4%    /* MFGRGA         */
            if sc_prt$ = "D" then switch% = 5%    /* MFGUPS         */
            if sc_prt$ = "E" then switch% = 7%    /* AWD017         */
                                                  /* (PAR004)       */
            if sc_prt$ = "Z" then switch% = 6%    /* MFGNEA         */
            if sc_prt$ = "G" then switch% = 8%    /* MFGCROSD (SR70701)*/
                                                  /* (AWD014)       */
           if scr_prt$ = "H" then switch% = 10%   /* MFGDCZA (CR456)   */
           if scr_prt$ = "I" then switch% = 11%   /* MFGDCZB (CR456)   */
           if scr_prt$ = "J" then switch% = 12%   /* MFGDCZC (CR456)   */


                                                  /* Staging Prt = 1%*/

            lb_app_key2$ = all(hex(00))
            str(lb_app_key2$,1%,5%) = sc_load$    /* All of Load    */

            read #8, key 2% > lb_app_key2$, using L60150, lb_app_rec$(), ~
                                            eod goto print_labels_done
            goto L60160

        print_labels_next
            read #8, using L60150, lb_app_rec$(),                        ~
                                            eod goto print_labels_done
L60150:        FMT 4*CH(256)                      /* (PAR006)          */

L60160:                                           /* Check Appian Load */
            if sc_load$ <> str(lb_app_rec$(),23%,5%) then               ~
                                            goto print_labels_done

                                                  /* Check Appian Drop */
            if sc_drop$ = "AL" then goto L60200
               if sc_drop$ <> str(lb_app_rec$(),28%,2%) then            ~
                                            goto print_labels_next
                                                  /* Check Appian S.O. */
L60200:
            if str(sc_so$,1%,3%) = "ALL" then goto L60250
               if sc_so$ <> str(lb_app_rec$(),56%,8%) then              ~
                                            goto print_labels_next
                                                 /* Check Appian Barcode */
L60250:
            if str(sc_barcode$,1%,3%) = "ALL" then goto L60300
               if sc_barcode$ <> str(lb_app_rec$(),1%,18%) then         ~
                                            goto print_labels_next
L60300:  
                                                  /* (AWD006)       */
                                                  /* (PAR000)       */
                                                  /* (PAR002)       */
            err% = 0%
/* CR1918 send file to subroutine */
            call "AWDPLA05" (switch%, been_here%, lb_app_rec$(),         ~
                                                       #4, #5, #63, #10, err%)
                if err% <> 0% then gosub print_error


            lbl% = lbl% + 1%
            if switch% <> 3% then goto L60350   /* (AWD007)          */
                                                /* (RHHTEST)         */
               if lbl% > 10% then goto print_labels_done
                                                /* (RHHTEST)         */
L60350:
            if mod(lbl%,25%) <> 0 then goto print_labels_next
               convert lbl% to str(counts$,24%,5%), pic(#####)

               call "SHOSTAT" (counts$)
            goto print_labels_next

        print_labels_done
                                                 /* (AWD006)        */
                                                 /* Finished        */
                                                 /* (PAR000)        */
                                                 /* (PAR002)        */
/* CR1918 send file to subroutine */                                                 
            call "AWDPLA05" (switch%, been_here%, lb_app_rec$(),         ~
                                                       #4, #5, #63, #10, 99%)
                if err% <> 0% then gosub print_error
            gosub load_results


        return clear all
        goto inputmode

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        open_error
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return

        update_error
            errormsg$ = "(Unabel to Update) - Shipping Label file"
            gosub error_prompt
        return


        load_results
           k% = 2%
           hdr$     = "***** Label Generation Results *****"
           msg$(1%) = "This run generated xxxxx label(s)."
           msg$(2%) = "---"
           msg$(3%) = "Press <ENTER> to Acknowledge & Continue"
           convert lbl% to str(msg$(1%),20%,5%), pic(####0)
           if lbl% <> 0% then L61000
               msg$(1%) = "NO LABELS GENERATED!!!"
               str(msg$(3%),,13%) = " Press <PF16>"
L61000:    call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
           if lbl% <> 0% and k% <>  0% then load_results
           if lbl%  = 0% and k% <> 16% then load_results
           lbl% = 0%
        return
  
        print_error
            hdr$     = "***** Label Printing Error *****"
            msg$(1%) = "ERROR OCCURRED WHILE PRINTING LABELS!!!"
            msg$(2%) = "Return Code (AWDPLA05) = "
            msg$(3%) = "Press <ENTER> to Continue, <PF16> to Exit"

            convert err% to str(msg$(2%),26%,2%), pic(#0)
L61010:     k% = 2%
            call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
                if k%  =  0% then return
                if k% <> 16% then goto L61010
            return clear all
            goto inputmode
                                                       /* (AWD002)      */
        lookup_customer                                /* Get Customer  */
           bckmst_key$, ap_cusname$, address2$(), address1$ = " "
           ap_city$, ap_state$, ap_zip$ = " "

           str(bckmst_key$,1%,9%)   = ap_cuscode$
           str(bckmst_key$,10%,16%) = ap_so$
           read #10, key = bckmst_key$, using L61050, ap_cusname$,       ~
                                        address2$(), address1$,          ~
                                        eod goto lookup_customer_done
L61050:       FMT POS(42), CH(30), 2*CH(30), POS(192), CH(30)

            for k% = 1% to 30%
               if str(ap_cusname$,k%,1%) = "," then ~
                   str(ap_cusname$,k%,1%) = " "
            next k%

           call "SPCESMSH" (address1$,1%,30%)
           p%, len% = 0%
           len% = len(address1$)

            for k% = 1% to 60%
                if str(address2$(),k%,1%) = "," then ~
                   str(address2$(),k%,1%) = " "
            next k%
            for k% = 1% to 30%
                if str(address1$,k%,1%) = "," then ~
                   str(address1$,k%,1%) = " "
            next k%

           for k% = 1% to len%
              p% = pos("0123456789" = str(address1$,k%,1%))
              if p% <> 0% then goto L61060
           next k%

L61060:    ap_zip$   = str(address1$,k%,len%)
           ap_state$ = str(address1$,k%-3%,2%)
           ap_city$  = str(address1$,1%,k%-4%)

           str(ap_address$,1%,30%)  = str(address2$(),1%,30%)
           str(ap_address$,31%,30%) = str(address2$(),31%,30%)

        lookup_customer_done
        return
                                               /* (AWD002)             */

                                               /* Get S.O. Information */
        lookup_so                              /* (AWD002)             */
            read #6,key 4% = ap_so$, using L61100, ap_region$,         ~
                     ap_po$, ap_howship$, or_load$, eod goto lookup_so_done


L61100: FMT POS(9), CH(2),  /* Region Code                             */~
            POS(36), CH(16),/* PO Number                               */~
            POS(92), CH(2), /* S.O How Ship Code                       */~
            CH(5)           /* SO Load Number                          */

        lookup_so_done
        return

        get_prod_description                   /* Non-Appian Product  */
                                               /* (PAR001)            */
            ap_part$ = str(lb_rec$(),523%,25%)
                                               /* (PAR001)            */
            str(ap_desc$,1%,36%) = str(lb_rec$(),462%,36%)
                                               /* (PAR001)            */
            str(ap_desc$,37%,24%) = str(lb_rec$(),647%,24%)
                                               /* (PAR001)            */
                                               /* Store three lines of*/
                                               /* Text                */
                                               /* (PAR001)            */
            if len(ap_part$) < 19 then                                  ~
               str(ap_desc$,1%,210%) = str(lb_rec$(),60%,210%)

        return
                                               /* (AWD002)             */

                                              /* (AWD002)                */
        create_non_appian_labels              /* Create Non-Appian Labels*/
            call "SHOSTAT" (sc_process_n$)
            ap_label$ = "N"

            init(" ") or_rec$, sc_key1$, sc_rec$

            count% = 0%

            str(sc_key1$,1%,5%) = sc_load$    /* Caelus Load Number      */

         create_non_appian_next
            read #7,key 1% > sc_key1$, using non_ap1, sc_rec$,              ~
                                            eod goto create_non_appian_done
non_ap1:      FMT CH(128)
               if sc_load$ <> str(sc_rec$,7%,5%) then                       ~
                                                goto create_non_appian_done
                  sc_key1$ = str(sc_rec$,7%,27%)

                  if sc_drop$ = "AL" then goto create_non_appian_next1
                                              /* Test for Insert         */
                     if sc_drop$ <> str(sc_rec$,17%,2%) then                ~
                                                 goto create_non_appian_next
        create_non_appian_next1
                                              /* Skip if in Appian Data  */
                                              /* Base                    */
        REM      gosub check_label
        REM      if check% = 0% then goto create_non_appian_next
                                              /* No Label                */

                                              /* Caelus Drop Number      */
                  ap_drop$    = str(sc_rec$,17%,2%)
                                              /* App Region Code         */
                                              /* Caelus Load Number      */
                  ap_load$    = str(sc_rec$,7%,5%)
                                                /* Customer Code         */
                  ap_cuscode$ = str(sc_rec$,59%,9%)
                                                  /* App P.O. Number    */
                                                  /* Wood or Part       */
                  ap_mull$ = " "
                                                  /* Caelus S.O. Number */
                  ap_so$      = str(sc_rec$,24%,8%)
                                                  /* App Earliest Dis Dt*/
                  ap_ear_dis$ = "000000"
                                                  /* App Arrival Date   */
                  ap_arr_dte$ = "      "
                                                  /* Get Customer Info  */
                  gosub lookup_customer
                                                  /* Get S.O. Info.     */
                  gosub lookup_so                 /* Get Region Code    */
                                                  /*     PO Number      */
                                                  /*     How ship Code  */

                                                  /* App Cust Name      */
                                                  /* S.O. Howship       */
                                                  /* Caelus Old Load    */
                  ap_old_load$= str(sc_rec$,7%,5%)
                                                  /* Caelus Old Drop    */
                  ap_old_drop$= str(sc_rec$,17%,2%)
                                                  /* App Act Arr Date   */
                  ap_act_dte$ = " "
                                                  /* App Act Arr Time   */
                  ap_act_tme$ = "0000"
                                                  /* Not Applic         */
                  ap_truck$   = "00000"
                                                  /* Not Applic         */
                  ap_start_date$ = date
                                                  /* Not Applic         */
                  ap_shp_tme$ = "0000"

                                                  /* App Scan Date      */
                  ap_scan_dte$= " "
                                                  /* App Scan Time      */
                  ap_scan_tme$= "0000"
                                               /* Get Label Detail Data */
                  gosub prod_label_detail

            goto create_non_appian_next

        create_non_appian_done

                                             /* Remove label Printing */
                                             /* (AWD013) Update Dock  */
                                             /*    Door Number        */
           gosub update_load
        return clear all
        goto inputmode
                                             /* (AWD002)              */

                                             /* (AWD003)              */
        print_labels_verify

            hdr$     = "*** You Are Printing Labels ****"
            msg$(1%) = "Do You Wish To Continue Printing Labels"
            msg$(2%) = "If 'YES', Press <ENTER> to Continue !!!"
            msg$(3%) = "Otherwise <PF16> to Exit Printing?     "

L61200:     k% = 2%
            call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
                if k%  =  0% then return
                if k% <> 16% then goto L61200
            return clear all
            goto inputmode
                                             /* (AWD003)              */

        delete_trailer_verify                /* (AWD009)              */
            tic% = 0%                        /* (AWD011)              */
            hdr$     = "*** You Are Deleting Trailer ***"
            msg$(1%) = "Do You Wish To Continue Trailer Delete "
            msg$(2%) = "If 'YES', Press PF(XX) to Continue  !!!"
            msg$(3%) = "Otherwise <PF16> to Exit Delete?       "

L61210:     k% = 2%
            call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
                if k%  =  32% then return     /* (AWD011)             */
                tic% = tic% + 1%              /* (AWD011)             */
                if tic% = 3% then goto L61215 /* (AWD011)             */
                if k% <> 16% then goto L61210
L61215:
            return clear all
            goto inputmode
                                              /* (AWD009)             */
        bp_prompt
           comp% = 2%
           hdr$ = "** 'Changed' Appian Dock Door to (BP) **"
           msg$(1%) = " - - - -  A r e   Y o u   S u r e  - - - - "
           msg$(2%) = " Press Any (PF) Key to Continue, or Press  "
           msg$(3%) = " <Return> to Abort 'Change' to (BP)?       "
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return


        delete_data                               /*  (AWD005) - BEGIN */
            lb_app_key2$, lb_app_rec$() = all(hex(00))
            str(lb_app_key2$,1%,5%) = sc_load$    /* All of Load    */

            read #8,hold,key 2% > lb_app_key2$, using L60150,lb_app_rec$(),~
                                            eod goto delete_data_done
            goto L61300

        delete_data_next
            read #8, hold, using L60150, lb_app_rec$(),                   ~
                                            eod goto delete_data_done

L61300:     str(lb_app_key2$,1%,32%) = str(lb_app_rec$(),23%,32%)
                                                  /* Check Appian Load */
            if sc_load$ <> str(lb_app_key2$,1%,5%) then               ~
                                            goto delete_data_done

                                                  /* Check Appian Drop */
            if sc_drop$ = "AL" then goto L61350
               if sc_drop$ <> str(lb_app_key2$,6%,2%) then            ~
                                            goto delete_data_next
                                                  /* Check Appian S.O. */
L61350:
            if str(sc_so$,1%,3%) = "ALL" then goto L61400
               if sc_so$ <> str(lb_app_rec$(),56%,8%) then              ~
                                            goto delete_data_next
                                                 /* Check Appian Barcode */
L61400:
            if str(sc_barcode$,1%,3%) = "ALL" then goto L61450
               if sc_barcode$ <> str(lb_app_rec$(),1%,18%) then         ~
                                            goto delete_data_next
L61450:
               delete #8
               goto delete_data_next

        delete_data_done
        return                                    /*  (AWD005) - END   */

                                                  /* (AWD009)       */
        create_trailer_labels
            been_here% = 0%
            if sc_prt$ = "A" then switch% = 1%    /* MFGSTAG        */
            if sc_prt$ = "B" then switch% = 2%    /* MFGSHIP        */
            if sc_prt$ = "T" then switch% = 3%    /* MFGTEST        */
                                                  /* (AWD014)       */
            if sc_prt$ = "C" then switch% = 4%    /* MFGRGA         */
            if sc_prt$ = "D" then switch% = 5%    /* MFGUPS         */
            if sc_prt$ = "E" then switch% = 7%    /* AWD017         */
                                                  /* (PAR004)       */
            if sc_prt$ = "Z" then switch% = 6%    /* MFGNEA         */
                                                  /* (PAR004)       */
                                                  /* (AWD014)       */
            if sc_prt$ = "G" then switch% = 8%    /* MFGCROSD (SR70701)*/
            if sc_prt$ = "H" then switch% = 10%   /* MFGDCZA (CR456)   */
            if sc_prt$ = "I" then switch% = 11%   /* MFGDCZB (CR456)   */
            if sc_prt$ = "J" then switch% = 12%   /* MFGDCZC (CR456)   */

            call "SHOSTAT" ("Creating and Printing Loading Labels")
            gosub trailer_label_prompt
            if comp% = 0% then goto create_trailer_labels_2

            gosub trailer_update

            lbl% = 2%
create_trailer_labels_1
                                             /* (PAR002)                 */
                call "AWDPLC05" (switch%, been_here%, tr_trailer$,        ~
                                                        tr_load$, #4, err%)
                if err% <> 0% then gosub print_error

                lbl% = lbl% - 1%
                if lbl% <> 0% then goto create_trailer_labels_1

                err% = 99%                   /* Clear Work File, Scratch */
                                             /* (PAR002)                 */
                call "AWDPLC05" (switch%, been_here%, tr_trailer$,        ~
                                                       tr_load$, #4, err%)
                if err% <> 0% then gosub print_error

create_trailer_labels_2
        return clear all
        goto inputmode
                                                  /* (AWD009)            */
                                                  /* (AWD010)            */
        REM lookup_job                            /* (PAR006) Remove     */

        REM return
                                                   /* (AWD010)           */
                                                   /* (AWD009)           */
        trailer_label_prompt
           comp% = 2%
           hdr$ = "*** You are printing LOADING Labels  ***"
           msg$(1%) = " - - - -  A r e   Y o u   S u r e  - - - - "
           msg$(2%) = " Press Any (PF) Key to Continue, or Press  "
           msg$(3%) = "<Return> to Abort 'Printing' Loading Labels"
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
                                                    /* (AWD009)          */
                                                    /* (AWD010)          */

                                                    /* (PAR000)          */
        lookup_sub_part
            init(" ") bcksubpt_rec$, flds$(), info_flds$()
            flag$ = "0"                             /* Sales Order Info  */
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

            if err1% <> 0% then                                       ~
                  str(bcksubpt_rec$,48%,20%) = "00000000000000000000"
            if err1% = 0% then return

            err1% = 0%
        return
                                                         /* (PAR000)    */

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program

        end

