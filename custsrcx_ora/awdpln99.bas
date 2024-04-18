        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN99                             *~
            *  Creation Date     - 04/29/03                             *~
            *  Last Modified Date- 05/22/2019                           *~
            *  Written By        - Christie Gregory                     *~
            *                                                           *~
            *  Description       - Utility Program used to Insert a     *~
            *                      Sales Order into an Appian Load      *~
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
            * 04/29/03 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            * 05/27/08 ! (AWD001) - mod for status check          ! CMG *~
            *06/03/2011! (AWD002) - add orcl usr & pswd lookup    ! CMG *~
            *01/09/2019! CR1829 - Dallas conversion               ! DES *~
            *          ! added logic for bcksubpt_rec for call to !     *~
            *          ! to APCPLN9B (parameters had changed)     !     *~
            *04/29/2019! (CR1996) Atrium Load Inserts upload to   ! CMN *~
            *          !          send update to ATLaS            !     *~
            *05/22/2019! (CR2038) add shipto to pgorlntr          ! CMN *~
            *07/31/2019! CR2151 - Determine drop nbr not just 99  ! RDB *~
            *          !          Allow drop nbr entry            !     *~
            *************************************************************

        dim                                                              ~
            or_rec$170,                  /* S.O. Header History Rec    */~
            or_po$16,                    /* Purchase Order Number      */~
            or_hows$2,                   /* How Ship Code              */~
            or_region$2,                 /* Region Code                */~
            or_cuscode$9,                /* Customer Code              */~
            dt_key$23,                   /* 'DT' Key                   */~
            dt_key3$23,                  /* DT key 3                   */~
            dt_rec$256,                  /* Planning Detail Record     */~
            sc_rec$128,                  /* (APCPLNSC) - Line Items    */~
            sc_so$8,                     /*                            */~
            sc_line$2,                   /*                            */~
            sc_drop_seq$5,               /* DROP SEQ. NO.              */~
            sc_drop$2,                   /* CUSTOMER DROP NO           */~
            sc_key$10, sc_key1$27,       /* LINE ITEM LOAD KEY         */~
            sc_load$5, new_load$5,       /* S.O. LOAD AND NEW LOAD     */~
            sc_part$25,                  /* Part Number                */~
            sort_key1$60,                /* USED BY (APCPLN9B) SORT    */~
            sc_chk_drop$2,               /* CR2151 changing drop nbrs  */~
            pl_sort$12,                  /* USED FOR (APCPLNDT) SORT   */~
            pl_key$11,                   /* PRIMARY KEY - (APCPLNUC)   */~
            hdr$40, msg$(3%)79,          /* ASKUSER Arrays             */~
            userid$3,                    /* Current User Id            */~
            sav_load$5,                  /* Save Load Number           */~
            apor_key$8,                  /* Appian EWDAPPOR Key        */~
            apsc_key$10,                 /* Appian EWDAPPSC Key        */~
            sav_ap_type$1,               /* Appian Load or Not         */~
            apld_key$5,                  /* Appian EWDAPPLD Key        */~
            apor_rec$(3%)128,            /* Appian EWDAPPOR Record     */~
            apsc_rec$(3%)128,            /* Appian EWDAPPSC Record     */~
            apld_rec$128,                /* Appian EWDAPPLD Record     */~
            ear_dis$6,                   /* Earliest Dispatch Date     */~
            route$4,                     /* Route Number               */~
            stop$2,                      /* Stop Number                */~
            arrv_tme$4,                  /* Arrival Time               */~
            dept_tme$4,                  /* Departure Time             */~
            unload_dte$10,               /* Unload at Customer Date    */~
            customer$9,                  /* Customer Code              */~
            save_custcode$9,             /* Save lookup customer CR2151*/~
            a_custxref$9,                /* Customer Xref Moving CR2151*/~
            b_custxref$9,                /* Customer Xref To     CR2151*/~
            list_cust$(20%)9,            /* List customers on drop 2151*/~
            so_number$8,                 /* Sales Order Number         */~
            truck$2,                     /* Truck Number               */~
            sav_drop$2,                  /* Save Drop Number           */~
            start_dte$10,                /* Start Date                 */~
            start_tme$4,                 /* Start Time                 */~
            end_dte$10,                  /* End Date                   */~
            end_tme$4,                   /* End Time                   */~
            cust_name$30,                /* Customers Name             */~
            cust_addr$(2%)30,            /* Customers Address          */~
            cust_city$18,                /* Customers City             */~
            cust_state$2,                /* Customers State            */~
            cust_zip$9,                  /* Customers Zip              */~
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
            s_1$2                        /* Private Label Code         */

        dim filename$8,                  /* Used By EWDOPEN            */~
            axd$(25)4,                   /*                            */~
            f2%(25%),                    /* = 0 if the file is open    */~
            f1%(25%),                    /* = 1 if READ was successful */~
            fs%(25%)                     /* = 1 if file open, -1 if it */

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
            fields$(4%)100               /* String of Oracle Info      */

        dim cursor%(24%),                /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32                    /* PF Key Hex Values          */

        dim                                                              ~
            in1$5,                       /* Load Number                */~
            load_d$30,                   /* Load Number and Description*/~
            ap_type$1,                   /* Is this a Appian Load      */~
            in2$8,                       /* Sales Order Number         */~
            in3$5,                       /* Drop Sequence Number       */~
            in4$2,                       /* Drop Number                */~
            from$(10%)38, too$(10%)38,   /* For Display Screen         */~
            or_key$8,                    /* APCPLNOR Readkey           */~
            dt_so$8,                     /* Sales Order Number         */~
            dt_cuscode$9,                /* Customer Code              */~
            dt_load$5,                   /* Load Number                */~
            dt_load_d$30,                /* Load Number and Description*/~
            dt_po$16,                    /* Purchase Order Number      */~
            dt_drop_seq$5,               /* Drop Sequence Number       */~
            dt_drop$5,                   /* Drop Number                */~
            bckmst_key$25,               /* BCKMASTR Readkey           */~
            dt_cuscode_d$30,             /* Customer Description       */~
            dt_address$(5%)30,           /* Customer Address           */~
            dt_city$18,                  /* Customer City              */~
            dt_state$2,                  /* Customer State             */~
            dt_zip$9,                    /* Customer Zip               */~
            status$2                     /* (AWD001) Status Check      */

        dim  flag$1, pgm$1, so_inv$8, so_item$11, sav_so_inv$11,         ~
	     item_no$3, bcksubpt_rec$256, flds$(35%)4,                   ~
	     info_flds$(35%)4, dt_sub_part$20, dt_sub_info$20
         
        dim atlstatus$2,                /* Status code        (CR1996)  */~
            pgmname$10,                 /* Program Name       (CR1996)  */~
            schema$8                    /* Schema             (CR1996)  */



        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "Special Utility to Insert into Applan Loads"
            pname$ = "AWDPLN99 - Rev: R7.00"

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
            * #19 ! BCKMASTR ! Sales Order Master File                  *~
            * #20 ! SYSFILE2 ! Caelus Management System General Informa *~
/*CR1996*/  * #21 ! PGORLNTR ! PlyGem ATLaS Trigger Remote Order Line Fi*~
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

             select #11, "AWDAPPLD"                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =   12, keylen =   5,                     ~
                        alt key  1, keypos =    1, keylen =  16,         ~
                            key  2, keypos =    2, keylen =  15,         ~
                            key  3, keypos =   17, keylen =  15

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

            select #19,  "BCKMASTR",                                     ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =   1,  keylen = 25,                      ~
                        alt key 1,  keypos = 26,   keylen = 16

            select #20,  "SYSFILE2",                                     ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =    1, keylen =  20

            select #21, "PGORLNTR",                    /* (CR1996) */    ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   21, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  64,         ~
                            key  2, keypos =   54, keylen =  11, dup
                        

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
            filename$ = "AWDAPPOR" : call "EWDOPEN" (#14, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDAPPSC" : call "EWDOPEN" (#15, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLINES" : call "EWDOPEN" (#16, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AMTBOMIF" : call "EWDOPEN" (#17, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "CUSTOMER" : call "EWDOPEN" (#18, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKMASTR" : call "EWDOPEN" (#19, filename$, err%)
            if err% <> 0% then gosub open_error
/*(AWD002) */
            filename$ = "SYSFILE2" : call "EWDOPEN" (#20, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "PGORLNTR" : call "EWDOPEN" (#21, filename$, err%)
            if err% <> 0% then gosub open_error


REM         call "OPENFILE" (#13, "INPUT", f2%(13), rslt$(13), axd$(13))


            mat f1% = zer

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


            schema_err%, schema% = 0%                           /* (CR1996) */
            init(" ") schema$                                   /* (CR1996) */
            call "SCHEMA" (schema$, schema%, #4, schema_err%)   /* (CR1996) */ 
            
            beenherebefore% = 0%
            gosub oracle_connect
            if oci_err% <> 0% then goto oracle_error


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   4%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10220
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10200
L10150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10150
L10200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10120
L10220:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
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
                  if keyhit%  = 16% then gosub insert_order
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 4% then editpg1
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
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************


        insert_order
            if beenherebefore% = 1% then gosub update_appld
            if beenherebefore% = 1% then gosub oracle_delete
            gosub read_upload
            beenherebefore% = 1%
            if upload% <> 1% then goto insert_done

            call "SHOSTAT" ("Inserting S.O.("&so_number$&") ")

            atlstatus$ = "04"                          /* (CR1996) */
            pgmname$   = "AWDPLN99"                   /* (CR1996) */

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
REM call "SHOSTAT" ("Inserting S.O.("&so_number$&") ")

REM SC_DROP_SEQ$ = "00000"
REM SC_DROP$     = "00"

REM SC_SO$       = SO_NUMBER$               /* INSERT SALES ORDER */
REM STOP% = 0%
REM CONVERT STOP$ TO STOP%, DATA GOTO L19180

REM L19180     CONVERT STOP% TO SC_DROP$, PIC(00)

REM GOSUB UPDATE_HEADER                /* UPDATE WITH NEW LOAD */
REM IF LOAD% = 0% THEN GOTO INSERT_ORDER
            gosub update_app_header




            init(" ") sc_key$, sc_rec$
            str(sc_key$,1%,8%) = sc_so$
        insert_order_nxt
            read #6,key > sc_key$, using L19350, sc_rec$,                 ~
                                                     eod goto insert_order
L19350:        FMT CH(128)


                                               /* Never found Sales Order!!  */

            if str(sc_key$,9%,2%) = " " and                               ~
                       str(sc_rec$,24%,8%) <> sc_so$ then goto L19470


            sc_key$ = str(sc_rec$,24%,10%)
            if sc_so$ <> str(sc_key$,1%,8%) then goto insert_done
               sc_line$ = str(sc_rec$,32%,2%)
               sc_load$ = str(sc_rec$,7%,5%)
               gosub update_lines
               if schema% = 2% then gosub updateRemoteOrderLine   /* (CR1996) */
               
            goto insert_order_nxt
        insert_done
            if beenherebefore% = 1% then gosub update_appld
            if beenherebefore% = 1% then gosub oracle_delete
        return clear all
REM        goto exit_program
        goto inputmode

L19470:     errormsg$ = "(Error) - Unable to Insert S.O. -- "& sc_so$
            gosub error_prompt
            goto insert_order

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
         return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G ES     *~
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

        scrn1_msg  :  data                                               ~
         "Enter the Appian Load Number to insert to?                   ",~
         "Enter the Sales Order to be inserted into an Appian Load?    ",~
         "Enter the Drop Sequence Number to be moved to?               ",~
         "Enter the Drop Number to be moved to?                        "


        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

            initialize_variables
                init(" ") in1$, load_d$, in2$, in3$, from$(), too$(), ~
                          sc_chk_drop$, in4$


            return


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
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              call "ALLFREE" 
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40190,         /* Load Number         */ ~
                                L40190,         /* Sales Order Number  */ ~
                                L40190,         /* Drop Sequence Num   */ ~
                                L40190          /* Drop Number         */
              goto L40220

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40190:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40220:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Load Number         :",                      ~
               at (04,25), fac(lfac$(1%)), in1$                 , ch(05),~
               at (04,50), fac(hex(84)),   load_d$              , ch(30),~
                                                                         ~
               at (05,02), "Sales Order Number  :",                      ~
               at (05,25), fac(lfac$(2%)), in2$                 , ch(08),~
                                                                         ~
               at (06,02), "Drop Sequence Number:",                      ~
               at (06,25), fac(lfac$(3%)), in3$                 , ch(05),~
                                                                         ~
               at (07,02), "Drop Number         :",                      ~
               at (07,25), fac(lfac$(4%)), in4$                 , ch(02),~
                                                                         ~                                                                         ~
               at (10,02), fac(hex(84)),   from$( 1%)           , ch(38),~
               at (11,02), fac(hex(84)),   from$( 2%)           , ch(38),~
               at (12,02), fac(hex(84)),   from$( 3%)           , ch(38),~
               at (13,02), fac(hex(84)),   from$( 4%)           , ch(38),~
               at (14,02), fac(hex(84)),   from$( 5%)           , ch(38),~
               at (15,02), fac(hex(84)),   from$( 6%)           , ch(38),~
               at (16,02), fac(hex(84)),   from$( 7%)           , ch(38),~
               at (17,02), fac(hex(84)),   from$( 8%)           , ch(38),~
               at (18,02), fac(hex(84)),   from$( 9%)           , ch(38),~
                                                                         ~
               at (10,40), fac(hex(84)),   too$( 1%)            , ch(38),~
               at (11,40), fac(hex(84)),   too$( 2%)            , ch(38),~
               at (12,40), fac(hex(84)),   too$( 3%)            , ch(38),~
               at (13,40), fac(hex(84)),   too$( 4%)            , ch(38),~
               at (14,40), fac(hex(84)),   too$( 5%)            , ch(38),~
               at (15,40), fac(hex(84)),   too$( 6%)            , ch(38),~
               at (16,40), fac(hex(84)),   too$( 7%)            , ch(38),~
               at (17,40), fac(hex(84)),   too$( 8%)            , ch(38),~
               at (18,40), fac(hex(84)),   too$( 9%)            , ch(38),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40740
                  call "PRNTSCRN"
                  goto L40220

L40740:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())              
               return

        set_pf1
        if edit% = 2% then L40940     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f1000)
            if fieldnr% = 1% then L40900
                str(pf$(3%),64%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
L40900:     if fieldnr% > 1% then L40920
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40920:     return

L40940: if fieldnr% > 0% then L41030  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                 (15)Print Screen      "
            pf$(3) = "                                        " &        ~
                     "                 (16)Insert Data       "
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L41030:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
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
            call "ALLFREE" 
            on fieldnr% gosub L50075,         /* Load Number           */ ~
                              L50155,         /* Sales Order Number    */ ~
                              L50225,         /* Drop Sequence Number  */ ~
                              L50425          /* Drop Number           */

            return


L50075: REM Insert Load Number                    IN1$
           apld_key$ = in1$
           gosub lookup_load
           if ap_type$ = "N" then goto L50095
           load_d$ = dt_load_d$
        return
L50095:     errormsg$ = "(Error) Invalid Appian Load?"
            gosub error_prompt
            init(" ") in1$, load_d$
        return

L50155: REM Insert From Data                      IN2$
            if in1$ <> " " then goto L50170
               goto L50200
L50170:
            gosub lookup_or
            if status$ < "03" or status$ > "16" then gosub check_pass
            if pass% <> 0% then goto L50210
            if lookup% <> 1% then goto L50200
               gosub format_from
        return
L50200:     errormsg$ = "(Error) Invalid From Data Entered for Select?"
            gosub error_prompt
            init(" ") in2$, from$()
        return
        check_pass
             call "APCPASSW" ("AWDPLN99", userid$, pass%)
        return
                                                       /*  (AWD001)  */
L50210:
            errormsg$ = "(Error) Invalid password, no soup for you!"
REM ERRORMSG$ = "(ERROR) CANNOT INSERT NOT PLANNED OR INVOICED ORDER!"
            gosub error_prompt
            init(" ") in2$, too$()
        return

L50225: REM Customer Drop Seq. Number            IN3$
            in4$ = "99"
/* CR2151 */
            gosub find_drop_add
            if sc_chk_drop$ <> "00" then in4$ = sc_chk_drop$
            
            if in3$ <> " " then goto L50375
               in3$ = "99999"
            in3% = 0%
L50375:     convert in3$ to in3%, data goto L50390

            convert in3% to in3$, pic(00000)
            gosub lookup_drop_info
            if dt_drop_seq$ = "Error" then goto L50390
            gosub format_too
        return
L50390:     errormsg$ = "(Error) Invalid Drop Sequence Number?"
            gosub error_prompt
            init(" ") in3$, too$()
        return

/* CR2151 */
L50425:     if in4$ <> " " then goto L50435
               in4$ = "99"
            
L50435:     convert in4$ to in4%, data goto L50440
            convert in4% to in4$, pic(00)
            if in4% < 90% then gosub validate_customer  /* CR2151 */
            if warn% = 1% then return                   /* CR2151 */
            gosub format_too
        return 
        
L50440:     errormsg$ = "(Error) Invalid Drop Number?"
            gosub error_prompt
            init(" ") in4$, too$()
        return

/* CR2151 */
        validate_customer
           warn% = 0%: r% = 1%
           init(" ") customer$, a_custxref$, b_custxref$, save_custcode$
           
           customer$ = dt_cuscode$
           gosub lookup_custxref
           a_custxref$ = custxref$
           
           gosub find_current_drop
           
           for r% = 1% to i% 
             customer$ = list_cust$(r%)
             gosub lookup_custxref
             b_custxref$ = custxref$
           
             if a_custxref$ = b_custxref$ then return
           next r%
           
           warn% = 1%
           gosub warn_cust
           if warn% = 1% then init(" ") in4$, too$()
        return

        warn_cust
           comp% = 2%
           hdr$ = "***** W A R N I N G  *****"
           msg$(1%) = "Customer Differences"
           msg$(2%) = "Press Any Key To Continue."
           msg$(3%) = "Press F16 To Cancel."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
           if comp% <> 16% then warn% = 0%
        return 
        
        lookup_custxref
/*  This is the Window Wizard Ship to cross reference customer */
            init(" ") custxref$
            read #18, key = customer$, using L50450, custxref$, eod goto L50451
L50450:         FMT POS(771), CH(09)  
L50451:        
        return
        
        find_current_drop
           init(" ") dt_key3$, save_custcode$
           i% = 1%
           str(dt_key3$,1%,5%) = in1$
read_next:
           read #1,key 3% > dt_key3$, using L50460, dt_rec$,     ~
                                                   eod goto L50465

L50460:       FMT CH(256)

             dt_key3$ = str(dt_rec$,1%,23%)
             if str(dt_rec$,1%,5%) <> in1$ then return
             if str(dt_rec$,11%,2%) <> in4$ then goto read_next

             save_custcode$ = str(dt_rec$,124%,9%)
             if list_cust$(i%) = save_custcode$ then goto read_next
             i% = i% + 1%
             list_cust$(i%) = save_custcode$
             goto read_next
L50465:             
        return
/* CR2151 */
        
        lookup_or                                /* Sales Order and    */
            status$ = "00"                       /* (AWD001)   */
            lookup% = 0%                         /* P.O. Lookup        */
            init(" ") or_key$, dt_so$, dt_cuscode$, dt_load$, dt_po$,    ~
                      dt_drop_seq$, dt_drop$, or_rec$

            str(or_key$,1%,8%) = in2$         /* Sales Order Lookup */

            read #3, hold, key 4% = or_key$, using L50475, or_rec$,        ~
                                                  eod goto lookup_or_done
L50475:        FMT CH(170)
            dt_so$      = str(or_rec$,52%,8%)
            dt_cuscode$ = str(or_rec$,27%,9%)
            dt_load$    = str(or_rec$,94%,5%)
            dt_po$      = str(or_rec$,36%,16%)
            dt_drop$    = str(or_rec$,25%,2%)
            status$     = str(or_rec$,60%,2%)
REM GOSUB LOOKUP_DROP_INFO
REM IF DT_DROP_SEQ$ = "ERROR" THEN GOTO LOOKUP_DT_DONE
            gosub lookup_customer
            apld_key$ = dt_load$
            gosub lookup_load
            lookup% = 1%
        lookup_or_done
        return

/* CR2151 */
        find_drop_add
            init(" ") sc_key1$, dt_drop_seq$
            sc_chk_drop$ = "99"
            drop_exists% = 0%
            str(sc_key1$,1%,5%) = in1$

        find_nxt_drop
            read #6,key 1% > sc_key1$, using L50500, sc_key1$, eod goto L50580

L50500:        FMT POS(7), CH(27)
              
              if str(sc_key1$,01%,05%) <> in1$ then goto L50580
              if str(sc_key1$,11%,02%) < "90" then goto find_nxt_drop

              drop_exists% = 1%               
              if sc_chk_drop$ > str(sc_key1$,11%,02%) then ~
                sc_chk_drop$ = str(sc_key1$,11%,02%)
                
            goto find_nxt_drop
L50580: 
            convert sc_chk_drop$ to sc_chk_drop%, data goto L50599
            if drop_exists% = 1% then sc_chk_drop% = sc_chk_drop% - 1%            
            convert sc_chk_drop% to sc_chk_drop$, pic(00)
        return
        
L50599:
            sc_chk_drop$ = "00"
        return

        lookup_drop_info
            init(" ") sc_key1$, dt_drop_seq$
            str(sc_key1$,1%,5%) = in1$
            str(sc_key1$,6%,5%) = in3$
            read #6,key 1% > sc_key1$, using L50670, sc_key1$, eod goto L50685

L50670:        FMT POS(7), CH(27)

            if str(sc_key1$,1%,5%) <> in1$ then return
            if str(sc_key1$,6%,5%) = in3$ and                                ~
                            str(sc_key1$,18%,8%) <> in2$ then goto L50685
REM DT_DROP_SEQ$ = STR(SC_KEY1$,5%,5%)
REM DT_DROP$     = STR(SC_KEY1$,10%,2%)
        return
L50685:     dt_drop_seq$ = "Error"
            dt_drop$     = "00"
        return

        lookup_customer
            init(" ") dt_cuscode_d$, dt_address$(), bckmst_key$
            str(bckmst_key$,1%,9%) = dt_cuscode$
            str(bckmst_key$,10%,16%) = dt_so$
            read #19,key = bckmst_key$, using L50725, dt_cuscode_d$,           ~
                                        dt_address$(), eod goto lookup_customer_done
L50725:        FMT POS(42), CH(30), 5*CH(30)


           len% = len(dt_address$(5%))
           for k% = 1% to len%
              p% = pos("0123456789" = str(dt_address$(5%),k%,1%))
              if p% <> 0% then goto L61470
           next k%

L61470:    dt_zip$   = str(dt_address$(5%),k%,len%)
           dt_state$ = str(dt_address$(5%),k%-3%,2%)
           dt_city$  = str(dt_address$(5%),1%,k%-4%)
        lookup_customer_done
        return

        lookup_load
            ap_type$ = "N"
            init(" ") dt_load_d$
            read #2,key = apld_key$, using L50765, dt_load_d$,                 ~
                                            eod goto lookup_load_done
L50765:        FMT POS(16), CH(30)
            gosub lookup_app_load
        lookup_load_done
        return

        lookup_app_load

             read #11, key = apld_key$, using L50785, ap_type$, eod goto app_load_done

L50785:        FMT POS(83), CH(1)

        app_load_done
        return

        format_from
            init(" ") from$()
            from$(1%) = "<-  Insert Sales Order 'From Data'  ->"
            from$(2%) = "--------------------------------------"
            from$(3%) = "Load (XXXXX)    :XXXXXXXXXXXXXXXXXXXX "
            from$(4%) = "Cust (XXXXXXXXX):XXXXXXXXXXXXXXXXXXXX "
            from$(5%) = "Sales Order     :XXXXXXXX             "
            from$(6%) = "P.O. Number     :XXXXXXXXXXXXXXXX     "
            from$(7%) = "                                   "
            from$(8%) = "Curr Drop Seq   :XXXXX                "
            from$(9%) = "Curr Drop No.   :XX                   "

            str(from$(3%),7%,5%)   = dt_load$
            str(from$(3%),18%,20%) = dt_load_d$
            str(from$(4%),7%,9%)   = dt_cuscode$
            str(from$(4%),18%,20%) = dt_cuscode_d$
            str(from$(5%),18%,8%)  = dt_so$
            str(from$(6%),18%,16%) = dt_po$
REM STR(FROM$(7%),18%,8%)  = LD_DTP3$
            str(from$(8%),18%,5%)  = dt_drop_seq$
            str(from$(9%),18%,2%)  = dt_drop$
REM STR(IN1_KEY$,1%,8%)    = DT_SO$
        return

        format_too
            init(" ") too$()
            too$(1%) = "<-  Insert Sales Order 'Into Data'  ->"
            too$(2%) = "--------------------------------------"
            too$(3%) = "Load (XXXXX)    :XXXXXXXXXXXXXXXXXXXX "
            too$(4%) = "Cust (XXXXXXXXX):XXXXXXXXXXXXXXXXXXXX "
            too$(5%) = "Sales Order     :XXXXXXXX             "
            too$(6%) = "P.O. Number     :XXXXXXXXXXXXXXXX     "
            too$(7%) = "                                   "
            too$(8%) = "Curr Drop Seq   :XXXXX                "
            too$(9%) = "Curr Drop No   .:XX                   "

            str(too$(3%),7%,5%)   = in1$
            str(too$(3%),18%,20%) = load_d$
            str(too$(4%),7%,9%)   = dt_cuscode$
            str(too$(4%),18%,20%) = dt_cuscode_d$
            str(too$(5%),18%,8%)  = in2$
            str(too$(6%),18%,16%) = dt_po$
REM STR(TOO$(7%),18%,8%)  = LD_DTP3$
            str(too$(8%),18%,5%)  = in3$
            str(too$(9%),18%,2%)  = in4$
REM STR(IN2_KEY$,1%,8%)   = DT_SO$
REM SCR_LOAD$ = DT_LOAD$
           return



        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
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
                 route$ = "9999"
REM                  stop$  = "99"
/* CR2151 */     stop$ = in4$
                 arrv_tme$ = "9999"
                 dept_tme$ = "9999"
                 unload_dte$ = " "
                 customer$ = dt_cuscode$
                 so_number$ = in2$
                 cust_name$ = dt_cuscode_d$
                 cust_addr$(1%) = dt_address$(1%)
                 cust_addr$(2%) = dt_address$(2%)
                 cust_city$ = dt_city$
                 cust_state$ = dt_state$
                 cust_zip$ = dt_zip$
                 truck$ = "99999"
                 start_dte$ = " "
                 start_tme$ = "9999"
                 end_dte$ = " "
                 end_tme$ = "9999"

           upload% = 1%
REM UPLOAD_DONE
        return


        update_header
            load% = 0%
REM IF STR(SAV_TRUCK$,1%,2%) <> STR(TRUCK$,1%,2%) THEN GOSUB ASSIGN_LOAD
                                                     /* #3 APCPLNOR */
            read #3,hold,key 4% = sc_so$,using L61030, or_rec$,           ~
                                                           eod goto L61100
L61030:        FMT CH(170)


            sav_load$ = str(or_rec$,94%,5%)
            gosub lookup_header_load
REM IF SAV_AP_TYPE$ = "Y" THEN GOTO ALREADY_APP

               delete #3
            sav_drop$ = str(or_rec$,25%,2%)
            or_po$ = str(or_rec$,36%,16%)
            or_hows$ = str(or_rec$,92%,2%)
            or_region$ = str(or_rec$,9%,2%)
            or_cuscode$ = str(or_rec$,27%,9%)


            new_load$ = in1$
            sc_drop_seq$ = in3$

            str(or_rec$,25%,2%) = sc_drop$              /* Drop Number */
            str(or_rec$,94%,5%) = new_load$             /* Load Number */
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
            gosub lookup_header_load
            init(" ") apor_key$, apor_rec$()
            apor_key$ = sc_so$

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
           str(apor_rec$(),156%,9%) = cust_zip$
           str(apor_rec$(),165%,2%) = or_hows$
           str(apor_rec$(),167%,30%) = cust_name$
           str(apor_rec$(),197%,6%) = start_dte$

REM STR(APOR_REC$(),197%,188%) = " "

           put #14, using L61500, apor_rec$()

           write #14, eod goto L61590
L61500:          FMT 3*CH(128)
        return
L61590:     errormsg$ = "(Error) - Updating APPIAN Header (EWDAPPOR) " &sc_so$
            gosub error_prompt
            init(" ") errormsg$
        return

        update_lines
REM  SC_DROP_SEQ% = SC_DROP_SEQ% + 10%
REM  CONVERT SC_DROP_SEQ% TO SC_DROP_SEQ$, PIC(00000)
                                                /* #6 APCPLNSC */
            read #6,hold,key = sc_key$, using L61180, sc_rec$,           ~
                                                         eod goto L61280

               delete #6

            sc_part$ = str(sc_rec$,34%,25%)
            str(sc_rec$,12%,5%)  = sc_drop_seq$  /* Sched Drop Seq.    */
            str(sc_rec$,17%,2%)  = sc_drop$      /* Sched Drop No.     */
            str(sc_rec$,7%,5%)   = new_load$     /* Curr Assigned Load */
REM  STR(SC_REC$,105%,5%) = SC_LOAD$      /* ORIG. PARENT LOAD  */
            str(sc_rec$,105%,5%) = sav_load$      /* Orig. Parent Load  */

            put #6, using L61180, sc_rec$
            write #6, eod goto L61290

        gosub update_app_lines
        gosub update_dtl
REM        gosub update_appld
L61280: return
L61290:     errormsg$ = "(Error) - Updating Line (APCPLNSC) " &sc_key$
            gosub error_prompt
            init(" ") errormsg$
        return

        lookup_header_load

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

            read #16, key = bcklines_key$, using L61700, config_lne$,   ~
                                                  eod goto no_bck_lines
L61700:            FMT POS(284), CH(3)

        no_bck_lines
        return

        update_dtl
            init(" ") dt_key$
            str(dt_key$,1%,8%) = sc_so$
            str(dt_key$,9%,2%) = sc_line$
        update_dtl_nxt
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
               goto update_dtl_nxt
        update_dtl_done
        return
L61560:     errormsg$ = "(Error) - Updating Detail (APCPLNDT) " &dt_key$
            gosub error_prompt
            init(" ") errormsg$
        return


        update_appld
            init(" ") sc_key1$
            str(sc_key1$,1%,5%) = sav_load$
            read #6,key 1% > sc_key1$, using L61710, sc_key1$, eod goto updte_appld_done

L61710:               FMT POS(7), CH(27)

                  if str(sc_key1$,1%,5%) <> sav_load$ then goto updte_appld_done
       return
       updte_appld_done
            init(" ") apld_key$, apld_rec$
            str(apld_key$,1%,5%) = sav_load$

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

        lookup_sub_part
            init(" ") bcksubpt_rec$, flds$(), info_flds$(),           ~
                      dt_sub_part$, dt_sub_info$
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
REM SAV_SO_INV$ = STR(DT_REC$,24%,11%)
            sav_so_inv$ = str(dt_rec$,24%,10%)
            call "AWDBKSUB"   (flag$,        /* Flag 0=SalesOrder 1=Invoice*/~
                               pgm$,         /* Calling Program 0=BCKUPDTE */~
                                             /* 1=Any Other 2=Delete       */~
                                             /* 3=Invoice                  */~
                               so_inv$,      /* SO or Invoice Num to lookup*/~
                               item_no$,     /* Item Number                */~
                               bcksubpt_rec$, /* Record If BCKUPDTE then    */~
                                             /* pass in else pass out      */~
                               flds$(),       /* Part Number Fields         */~
                               info_flds$(),  /* Information Fields         */~
                               #7,            /* BCKSUBPT File              */~
                               err1%)         /* Error Code                 */
                    
            /* dt_sub_part$ & dt_sub_info$ not used in this program */
            /* but we have bcksubpt_rec$ for the call to APCPLN9B   */

            if err1% <> 0% then str(bcksubpt_rec$,48%,20%) = "00000000000000000000"
            if err1% <> 0% then str(bcksubpt_rec$,132%,9%) = "         "
              
            dt_sub_part$ = str(bcksubpt_rec$,48%,20%)
            dt_sub_info$ = str(bcksubpt_rec$,132%,20%)
               
            if err1% = 0% then return
        return  


        sort_dtl
              init(" ") pl_key$, pl_sort$, sort_key1$
              pl_sort$ = "02678L9A    "
        REM - Build Index (DT_INDEX$)
              str(pl_key$,1%,2%)  = str(dt_rec$,230%,2%)
              str(pl_key$,3%,2%)  = str(dt_rec$,232%,2%)
              str(pl_key$,5%,2%)  = str(dt_rec$,104%,2%)
              str(pl_key$,7%,3%)  = str(dt_rec$,42%,3%)
              str(pl_key$,10%,2%) = str(dt_rec$,45%,2%)
              read #7,key = pl_key$,using L61720, pl_sort$,               ~
                                                 eod goto L61740
L61720:          FMT POS(18), CH(12)

              so_inv$  = str(dt_rec$,24%,8%)               
              item_no$ = str(dt_rec$,32%,2%) 
	          so_item$ = so_inv$ & item_no$
              if sav_so_inv$ <> so_item$ then gosub lookup_sub_part

L61740:       call "APCPLN9B" ( "0",         /* Set Flag for Data File */~
                                pl_sort$,    /* Sort Code From APCPLNUC*/~
                                dt_rec$,     /* (APCPLNDT) Record      */~
                                bcksubpt_rec$, /* bcksubpt rec           */~
                                sort_key1$,  /* Output Index Built     */~
                                #4 )         /* (GENCODES)             */
              str(dt_rec$,66%,30%) = str(sort_key1$,1%,30%)
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
            s_so$  = str(sc_so$,1%,8%)
            s_ln$  = str(sc_line$,1%,3%)
            init(" ") s_prv$, s_1$, s_23$
            prv% = 1%                                 /* Use BCKLINES    */
            call "APCPRZSB" (prv%, s_1$, or_cuscode$, s_23m$, s_so$,       ~
                                   s_ln$, s_prv$, s_23$, s_23%,            ~
                                   #18, #4, #16, #16, x_er% )
            if x_er% <> 0% then return
               str(ora_desc$,1%,8%) = s_23$

        no_caelus_desc
        return

        oracle_connect
REM            user$   = "MSSQL"
REM            pass$   = "MSSQL"
            gosub get_user_pswd       /* (AWD002) */
            server$, stmt1$, stmt2$ = " "

            oci_err% = 0%
            no_fields% = 0%

            call "CONNECT" (user$, pass$, server$, oci_err%)
        return

/* (AWD002) beg */
        get_user_pswd
            call "READ100" (#20, "ORACLE PASSWORD", f1%(20%))   /* SYSFILE2 */
            if f1%(20%) <> 0% then get #20 using ORCL_PSWD, user$, pass$
ORCL_PSWD:         FMT POS(21), CH(50), POS(50)

        return

/* (AWD002) END */

/* + (CR1996) */
        updateRemoteOrderLine                                  /* (CR2038)  */
          call "APCORLNS" (or_cuscode$, sc_so$, sc_line$, atlstatus$,       ~
                           pgmname$, #21, error%)
          error% = 0%
        return
/* - (CR1996) */

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

REM            call "SHOSTAT" ("stmt1 " & str(stmt1$,1%,40%))  stop
REM            call "SHOSTAT" (" " & str(stmt1$,41%,26%))  stop
REM            call "SHOSTAT" (" " & str(stmt1$,67%,27%))  stop
REM            call "SHOSTAT" (" " & str(stmt1$,94%,37%))  stop
REM            call "SHOSTAT" (" " & str(stmt1$,131%,15%))  stop

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
REM            call "ERROR" (error$)
REM            call "SHOSTAT" ("ERROR ERROR RETURN VALUE --> " & error$)
REM            stop

            gosub oracle_flush
        return

        oracle_fields
            if oci_err% < 0% then return
            init(" ") ora_desc$, fields$()
            gosub oracle_fetch
            if oci_err% > 0 or oci_err% < -1 then return
REM            oracle% = 1%
            gosub get_oracle_print
            if str(fields$(),4%,1%) <> "0" then goto oracle_fields
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

        oracle_flush
            oci_err% = 0%
            call "FLUSH" (oci_err%)
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
REM            call "GETFIELD" (field_num%, field$, oci_err%)
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

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

                                         /* (APCPLNLD) - FILE          */
L61180:      FMT CH(128)


        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            gosub oracle_discnnct
            call "SHOSTAT" ("One Moment Please")

            end


