        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCEDI02                             *~
            *  Creation Date     - 08/15/92                             *~
            *  Last Modified Date- 04/05/05                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Modified Bu       - Christie Gregory                     *~
            *  Description       - Primary EDI Utility Program          *~
            *                      (1) Report PO'S Not Processed        *~
            *                      (2) Create S.O. from P.O's           *~
            *                      (3) Report PO'S Processed            *~
            *                      (4) Purge Proc. Data to Hist         *~
            *                      (5) Purge History From System        *~
            *                      (6) Delete (Un-Processed) Dat        *~
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
            * 08/15/92 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 10/12/92 ! Mod to Select Trading Partner            ! RHH *~
            * 12/17/92 ! MOD - UPDATE S.O. BOOKING FILE (#16)     ! RHH *~
            * 01/04/93 ! MOD to (APCEDIPO) Subroutine to Assign   ! RHH *~
            *          !   Sales GL Acct. No. Based on Model and  !     *~
            *          !   Assignment in 'EDI ACCTS' Table in     !     *~
            *          !   GENCODES File .                        !     *~
            * 11/21/94 ! Mod Remove File 'APCPRICE' This file is  ! RHH *~
            *          !   No Longer Used.                        !     *~
            *          ! Note - S.O. Price Obtained from Sku No.  !     *~
            *          !        File Only. Default is Zero.       !     *~
            * 01/08/97 ! Mod for new Version of Planning          ! RHH *~
            * 03/14/97 ! Mods to CHECK_FOB Override Routine       ! RHH *~
            * 04/21/97 ! Mod to Support the New Customer Interface! RHH *~
            *          !   VIQS. New EDI Partner (099) Data did   !     *~
            *          !   not come in via EDI. Sales Orders      !     *~
            *          !   created from the new Price Quote System!     *~
            *          !   2nd Mod to Obtain G/L Account from the !     *~
            *          !   (CATEGORY) File.                       !     *~
            * 06/02/97 ! Mods to include all the changes made to  ! RHH *~
            *          !   (APCEDIQT) - VIQS processing routine.  !     *~
            * 06/05/97 ! Mods to turn on SEND_FAX and to update   ! RHH *~
            *          !   the daily Sales register file used by  !     *~
            *          !   program (APCRPT04).                    !     *~
            *          !                                          !     *~
            * 10/31/97 ! Change Version ID To Reflect 60403       ! DJD *~
            *          !                                          !     *~
            * 01/13/98 ! y2k compliance                           ! DJD *~
            * 08/14/98 ! (EWD001) New Files APCPLNSA & EWDSCHED   ! RHH *~
            * 11/09/98 ! (EWD002) Mod remove (APCPCRQ - File )    ! RHH *~
            * 11/21/00 ! (EWD003) Mod to have enough room in      ! CMG *~
            *          !          APCEDI for all 8 digits of SO   !     *~            
            * 04/05/05 ! (AWD004) Mod to change reclen of APCPLNSD! CMG *~
            *************************************************************

        dim                                                              ~
            tt%(15%), mod$3,             /* SAVE REPORT TOTAL / PRODUCT*/~
            due_date$8,                  /* Due Date Override Formatted*/~
            due_dte$8,                   /* Due Date - Unformatted     */~
            hdr$40,                      /* ASKUSER                    */~
            msg$(3%)79,                  /* ASKUSER                    */~
            scr$(10%)30,                 /* Screen Selections          */~
            readkey$50,                  /* GENCODES Key               */~
                                         /* GENCODES Description       */~
            fob$20,                      /* FOB - DELIVERY             */~
            fob_desc$32,                 /* FOB - DESCRIPTION          */~
            ord_key$25,                  /* BCKMASTR AND BCKLINES KEY  */~
            bck_rec$200,                 /* BCKLINES RECORD            */~
            ord_ord$16,                  /* SAVE SALES ORDER CALCING   */~
            ord_tot$8,                   /* S.O. TOTAL DOLLARS         */~
            save_tot$8,                  /* SAVE S.O. TOTAL DOLLARS    */~
            save_so$8,                   /* SAVE SALES ORDER           */~
            edi_buf$(6%)20,              /* BUFFER FOR 'APCEDIPO'      */~
            edi_p$15,                    /* Partner ID Number/Dun's    */~
            edi_partner$3,               /* Trading Partner Code       */~
            edi_partner_desc$32,         /* Trading Partner Description*/~
            edi_rec$102,                 /* Master PO Control File     */~
            edi_sel$1,                   /* EDI Process Selection      */~
            edi_desc$32,                 /* EDI Process Description    */~
            edi_key$41,                  /* (APCEDI) Primary Key       */~
            edi_key1$9,                  /* (APCEDI) Alt Key (1)       */~
            edi_control$9,               /* Last (4)-GS, Last (5)-ST   */~
            edi_processed$1,             /* Rec Processed ( Y or N )   */~
            edi_dun$15,                  /* Parent Partner Id (DUN'S)  */~
            edi_store$6,                 /* Parent Store Number        */~
            edi_po$16,                   /* Parent Purchase Order No.  */~
            edi_ln$3,                    /* Parent PO Line Item No.    */~
            edi_sku$25,                  /* Parent SKU# - Stock Part   */~
            edi_qty$10,                  /* Parent PO Line Item Qty    */~
            edi_dte1$6,                  /* Requested Ship Date        */~
            edi_dte2$6,                  /* Actual Ship Date           */~
            edi_dt1$8,                   /* Requested Ship Date        */~
            edi_dt2$8,                   /* Actual Ship Date           */~
            edi_so$8,                    /* S.O. ASSIGNED              */~
            edi_filler$3,                /* Filler Area                */~
            sku_key$28,                  /* Primary Sku No. Key        */~
            sku_code$3,                  /* Customer Sku Code          */~
            edi_part$25,                 /* APC Stock MFG Part Number  */~
            edi_cust$9,                  /* APC Customer Code          */~
            save_po$16,                  /* Parent Purchase Order No.  */~
            save_cust$9,                 /* APC Customer Code          */~
            company$60,                  /* For Report Company Name    */~
            print_title$40,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(30%),                    /* = 0 if the file is open    */~
            f1%(30%),                    /* = 1 if READ was successful */~
            fs%(30%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(30%)20                 /* Text from file opening     */

/* <<<<<<<<<< Y2K >>>>>>>>>> */
	dim blankdate$8			 /* used for empty dates       */
/* <<<<<<<<<< Y2K >>>>>>>>>> */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "**(New) EDI Processing Master Utility **"
            pname$ = "APCEDI02 - Rev: 06.04"
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
            * #1  ! CUSTOMER ! Master Customer File                     *~
            * #2  ! GENCODES ! System Master Code Tables                *~
            * #3  ! APCEDIRF ! EDI Customer Cross Reference             *~
            * #4  ! APCEDI   ! Master PO Control File                   *~
            * #5  ! APCSKUNO ! Master CUSTOMER SKU NO. FILE             *~
            * #6  ! BCKMASTR ! S.O. Master Header File                  *~
            * #7  ! BCKLINES ! S.O. Detail Line Items File              *~
            * #8  ! STORNAME ! System Master Store Records              *~
            * #9  ! BCKBUF2  ! S.O. Detail Buffer File                  *~
            * #10 ! BCKHLNES !                                          *~
            * #11 ! DEMMASTR ! Demmand Master File                      *~
            * #12 ! APCPCRQ  ! COMM FILE FOR VIQS PRICE QUOTE (EWD002)  *~
            * #13 ! HNYMASTR ! Inventory Master File                    *~
            * #14 ! CPRPRICE ! CAELUS MASTER PRICE FILE                 *~
            * #15 ! APCPLNSA ! APC DAILY SALES REGISTER FILE            *~
            * #16 ! BCKBKGRF ! SALES ORDER BOOKING FILE                 *~
            * #17 ! CATEGORY ! USE FILE TO OBTAIN G/L SALES ACCTS       *~
            * #19 ! BCKPRIDX ! S.O. PRINT INDEX - ACKNOWLEDGEMENTS      *~
            * #20 ! HNYQUAN  ! Inventory Quantities                     *~
            * #21 ! APCPLNOR ! New Planning S.O. Header                 *~
            * #22 ! APCPLNSC ! New Planning S.O. Line Item Detail       *~
            * #23 ! APCPULLS ! Pulls From Inventory                     *~
            * #24 ! APCPLNDP ! Department UPMH and Products             *~
            * #25 ! APCPLNSD ! New Planning Scheduling File             *~
            * #29 ! EWDSCHED ! New Special Scheduling File (EWD001)     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize =  1200,             ~
                        keypos =  1,   keylen =  9,                      ~
                        alt key  1, keypos  =    10, keylen = 30, dup,   ~
                            key  2, keypos  =   424, keylen =  9, dup,   ~
                            key  3, keypos  =   771, keylen =  9, dup,   ~
                            key  4, keypos  =   780, keylen =  9, dup

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #3,  "APCEDIRF",                                      ~
                        varc,     indexed,  recsize =  32,               ~
                        keypos =    1, keylen =  21,                     ~
                        alt key  1, keypos  = 22, keylen = 9

            select #4,  "APCEDI",                                        ~
                        varc,     indexed,  recsize = 102,               ~
                        keypos =   10, keylen =  41,                     ~
                        alt key  1, keypos  = 1, keylen = 9, dup

            select #5,  "APCSKUNO",                                      ~
                        varc,     indexed,  recsize =  73,               ~
                        keypos =    1, keylen =  28,                     ~
                        alt key 1, keypos   = 29, keylen = 28, dup

            select #6,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize =  1000,             ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #7,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #8,  "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =    1, keylen =   3

            select #9,  "BCKBUF2",                                       ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =  10,  keylen = 19

            select #10, "BCKHLNES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #11, "DEMMASTR",                                      ~
                        varc,     indexed,  recsize =  123,              ~
                        keypos =    2, keylen =  27,                     ~
                        alt key  1, keypos =   10, keylen =  19,         ~
                            key  2, keypos =    1, keylen =  28,         ~
                            key  3, keypos =   29, keylen =  25, dup

            select #13, "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #14, "CPRPRICE",                                      ~
                        varc,     indexed,  recsize = 700,               ~
                        keypos =   1,  keylen = 47

            select #15, "APCPLNSA",                                      ~
                        varc,     indexed,  recsize =  32,               ~
                        keypos =  11,   keylen =  17,                    ~
                        alt key  1, keypos =    1, keylen =  19, dup

            select #16,  "BCKBKGRF",                                     ~
                        varc,     indexed,  recsize =  88,               ~
                        keypos =    1, keylen =   33

            select #17, "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos =    1, keylen =   4

            select #19, "BCKPRIDX",                                      ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =  11,   keylen =  29,                    ~
                        alt key  1, keypos =    1, keylen =  39

            select #20, "HNYQUAN",                                       ~
                        varc,     indexed,  recsize =  650,              ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44

            select #21,  "APCPLNOR",                                     ~
                        varc,     indexed,  recsize = 170,               ~
                        keypos =    1, keylen =   51,                    ~
                        alt key  1, keypos =   27, keylen =  25,         ~
                            key  2, keypos =   70, keylen =   8, dup,    ~
                            key  3, keypos =   78, keylen =   8, dup,    ~
                            key  4, keypos =   52, keylen =   8,         ~
                            key  5, keypos =   36, keylen =  16, dup

            select #22,  "APCPLNSC",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =   24, keylen =   10,                    ~
                        alt key  1, keypos =    7, keylen =  27,         ~
                            key  2, keypos =    1, keylen =  33

            select #23,  "APCPULLS",                                     ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =   10, keylen =   10,                    ~
                        alt key  1, keypos =    1, keylen =  19,         ~
                            key  2, keypos =   20, keylen =  25, dup

            select #24,  "APCPLNDP",                                     ~
                        varc,     indexed,  recsize =  32,               ~
                        keypos =   11, keylen =   12,                    ~
                        alt key  1, keypos =    9, keylen =  14,         ~
                            key  2, keypos =    4, keylen =  12,         ~
                            key  3, keypos =    1, keylen =  15
/* (AWD004) - Mod to key and reclen */
            select #25, "APCPLNSD",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =  23


            select #29, "EWDSCHED",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  38,                     ~
                        alt key  1, keypos =    16, keylen =  23

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(01%), f2%(01%),  0%, rslt$(01%))
            call "OPENCHCK" (#2, fs%(02%), f2%(02%),  0%, rslt$(02%))
            call "OPENCHCK" (#3, fs%(03%), f2%(03%),  0%, rslt$(03%))
            call "OPENCHCK" (#4, fs%(04%), f2%(04%),500%, rslt$(04%))
            call "OPENCHCK" (#5, fs%(05%), f2%(05%),  0%, rslt$(05%))
            call "OPENCHCK" (#6, fs%(06%), f2%(06%),  0%, rslt$(06%))
            call "OPENCHCK" (#7, fs%(07%), f2%(07%),  0%, rslt$(07%))
            call "OPENCHCK" (#8, fs%(08%), f2%(08%),  0%, rslt$(08%))
            call "OPENCHCK" (#9, fs%(09%), f2%(09%),  0%, rslt$(09%))
            call "OPENCHCK" (#10,fs%(10%), f2%(10%),  0%, rslt$(10%))
            call "OPENCHCK" (#11,fs%(11%), f2%(11%),  0%, rslt$(11%))
            call "OPENCHCK" (#13,fs%(13%), f2%(13%),  0%, rslt$(13%))
            call "OPENCHCK" (#14,fs%(14%), f2%(14%),  0%, rslt$(14%))
            call "OPENCHCK" (#15,fs%(15%), f2%(15%),  0%, rslt$(15%))
            call "OPENCHCK" (#16,fs%(16%), f2%(16%),100%, rslt$(16%))
            call "OPENCHCK" (#17,fs%(17%), f2%(17%),100%, rslt$(17%))
            call "OPENCHCK" (#19,fs%(19%), f2%(19%),100%, rslt$(19%))

            call "OPENCHCK" (#20,fs%(20%), f2%(20%),  0%, rslt$(20%))
            call "OPENCHCK" (#21,fs%(21%), f2%(21%),  0%, rslt$(21%))
            call "OPENCHCK" (#22,fs%(22%), f2%(22%),  0%, rslt$(22%))
            call "OPENCHCK" (#23,fs%(23%), f2%(23%),  0%, rslt$(23%))
            call "OPENCHCK" (#24,fs%(24%), f2%(24%),  0%, rslt$(24%))
            call "OPENCHCK" (#25,fs%(25%), f2%(25%),  0%, rslt$(25%))
            call "OPENCHCK" (#29,fs%(29%), f2%(29%),100%, rslt$(29%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)

/* <<<<<<<<<< Y2K >>>>>>>>>> */
            call "DATUFMTC" (blankdate$)
/* <<<<<<<<<< Y2K >>>>>>>>>> */

            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            for fieldnr% = 1% to  3%
L10090:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10210
L10110:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10190
L10140:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10110
                         if fieldnr% = 1% then L10090
                         goto L10140
L10190:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10110
L10210:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10110
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
                  if keyhit%  = 14% then gosub begin_process
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11130:     fieldnr% = cursor%(1%) - 5%
            if fieldnr% < 1% or fieldnr% > 3% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11180:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11180
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11180
                  lastfieldnr% = fieldnr%
            goto L11130

        REM *************************************************************~
            *     M A I N   L I N E   P R O C E S S I N G   A R E A     *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        begin_process
            err% = 0%
            gosub prompt_user
            if comp% = 0% then goto L19280
            on edi_sel% gosub print_report, create_so, print_report,     ~
                              purge_data, p_data, delete_data
        return clear all
        goto inputmode

        create_so
            init(" ") edi_buf$()
            str(edi_buf$(1%),1%,3%)  = edi_partner$
            str(edi_buf$(2%),1%,15%) = edi_p$
            str(edi_buf$(3%),1%,8%)  = due_dte$
            str(edi_buf$(4%),1%,20%) = fob$

                                           /* Process Transmitted PO'S */
                                           /* EDI_SEL$ = 1             */
            call "APCEDIPO" (#1, #2, #3, #4, #5, #6, #7, #8, #9, #10,    ~
                             #11, #13, #14, #15, #16, #17, #19, #20,     ~
                             #21, #22, #23, #24, #25, #29,               ~
                             edi_buf$(), err%)
L19280: return clear all
        goto inputmode

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
         "Enter a Valid EDI Process Selection ( 1 thru 6 ).            ",~
         "Enter a Valid EDI Trading Partner Selection or (A) for (ALL).",~
         "Enter a Valid Due Date to Override/Blank with FOB Delivery.  "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, readkey$, edi_key$,        ~
                      edi_sel$, edi_desc$, scr$(), edi_control$,         ~
                      edi_processed$, edi_dun$, edi_store$, edi_po$,     ~
                      edi_ln$, edi_sku$, edi_qty$, edi_dte1$, edi_dte2$, ~
                      edi_filler$, sku_key$, edi_cust$, sku_code$,       ~
                      edi_part$, readkey$, cnt$, edi_key1$, edi_so$,     ~
                      edi_partner$, edi_partner_desc$, edi_p$, due_date$,~
                      due_dte$, fob$, fob_desc$

            scr$( 1) = "(Sel)<----- Description ----->"
            scr$( 2) = " (1) Report PO'S Not Processed"
            scr$( 3) = " (2) Create S.O. from P.O's   "
            scr$( 4) = " (3) Report PO'S Processed    "
            scr$( 5) = " (4) Purge Proc. Data to Hist "
            scr$( 6) = " (5) Purge History From System"
            scr$( 7) = " (6) Delete (Un-Processed) Dat"
            scr$( 8) = "                              "
            scr$( 9) = "                              "
            scr$(10) = "                              "

            mat tt% = zer
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

        REM DATALOAD
        REM RETURN

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
                                       /* APCEDIRF - Partner Cross-Ref */
            FMT CH(15),                /* DUN'S Number                 */~
                CH(06),                /* Partner Store Number         */~
                CH(09),                /* APC Customer Number          */~
                CH(02)                 /* Filler Area                  */

                                       /* APCEDI - Master PO Control F */
            FMT CH(09),                /* Last (4)-GS, Last (5)-ST     */~
                CH(01),                /* Processed (Y) or (N)         */~
                CH(15),                /* Parent Partner Control No    */~
                CH(06),                /* Parent Store Number          */~
                CH(16),                /* Parent Purchase Order Number */~
                CH(03),                /* Parent PO Line Item Number   */~
                CH(25),                /* Parent SKU# (Stock Part No.) */~
                CH(10),                /* Parent PO Line Item Quantity */~
                CH(06),                /* Requested Ship Date          */~
                CH(06),                /* Actual Ship Date             */~
                BI(2),                 /* S.O. NUMBER ASSIGNED         */~
                CH(03)                 /* Filler Area                  */

                                         /* APCSKUNO -Sku Number File  */
            FMT CH(03),                  /* Customer Sku Code          */~
                CH(25),                  /* Sku Number                 */~
                CH(03),                  /* Customer Sku Code          */~
                CH(25),                  /* APC MFG Part Number        */~
                CH(09)                   /* Sku Filler Area            */

        FMT                              /* (APCPRCRQ) - Pricing File  */~
            CH(1),                       /* Processed Flag (1=Yes,0=No)*/~
            CH(16),                      /* VIQS Assigned Quote No.    */~
            CH(3),                       /* Line Item On Quote         */~
            CH(3),                       /* User Id Who Entered Quote  */~
            PD(14,4),                    /* Catalog Price = PC(1%)     */~
            CH(25),                      /* Valid Part Number          */~
            CH(1),                       /* (O)pening,(E)xact,(F)ixed  */~
            PD(14,4),                    /* Customer Price P(SRCE%)    */~
            PD(14,4),                    /* Customer Special Price     */~
            BI(1),                       /* Special Pricing Code SP%   */~
            CH(9),                       /* Customer Code              */~
            15*CH(2),                    /* STD Price Ref Codes        */~
            15*CH(2),                    /* SPC Price Ref Codes        */~
            15*PD(14,4),                 /* Reference Price -(Standard)*/~
            15*PD(14,4),                 /* Reference Price - (Special)*/~
            BI(1),                       /* Error 0% = Ok, <>0%=No Pric*/~
            BI(1),                       /* Task Control Flag          */~
            BI(4),                       /* Header Text Id             */~
            BI(4),                       /* Line Item Text Id          */~
            CH(16),                      /* Sales Order Number         */~
            CH(16),                      /* Customer P.O. Number       */~
            CH(16),                      /* Customer Job Name          */~
            CH(6),                       /* Date Quote Was Created     */~
            CH(6),                       /* Date Sales Order Created   */~
            CH(59)                       /* Filler Area                */

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
              on fieldnr% gosub L40170,         /* Process Selection */   ~
                                L40170,         /* Partner Selection */   ~
                                L40170          /* Due Date Override */
              goto L40200

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40200:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "(*)- Only Applicable for Selection (2).",    ~
                                                                         ~
               at (06,02), "  Process Selection:",                       ~
               at (06,25), fac(lfac$( 1)), edi_sel$             , ch(01),~
               at (06,30), fac(hex(84)),   edi_desc$            , ch(30),~
                                                                         ~
               at (07,02), "  Trading Partner's:",                       ~
               at (07,25), fac(lfac$( 2)), edi_partner$         , ch(03),~
               at (07,30), fac(hex(84)),   edi_partner_desc$    , ch(30),~
                                                                         ~
               at (08,02), "* Due Date Override:",                       ~
               at (08,25), fac(lfac$( 3)), due_date$            , ch(08),~
                                                                         ~
               at (09,02), "* FOB - Delivery   :",                       ~
               at (09,25), fac(lfac$( 3)), fob$                 , ch(20),~
                                                                         ~
               at (10,25), fac(hex(84)), scr$( 1%)              , ch(30),~
               at (11,25), fac(hex(84)), scr$( 2%)              , ch(30),~
               at (12,25), fac(hex(84)), scr$( 3%)              , ch(30),~
               at (13,25), fac(hex(84)), scr$( 4%)              , ch(30),~
               at (14,25), fac(hex(84)), scr$( 5%)              , ch(30),~
               at (15,25), fac(hex(84)), scr$( 6%)              , ch(30),~
               at (16,25), fac(hex(84)), scr$( 7%)              , ch(30),~
               at (17,25), fac(hex(84)), scr$( 8%)              , ch(30),~
               at (18,25), fac(hex(84)), scr$( 9%)              , ch(30),~
               at (19,25), fac(hex(84)), scr$(10%)              , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L40650
                  call "PRNTSCRN"
                  goto L40200

L40650:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40800     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return

L40800: if fieldnr% > 0% then L40890  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                   (14)Begin Processing"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
L40890:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50140,         /* Process Selection     */ ~
                              L50340,         /* Partner Selection-ALL */ ~
                              L50630          /* Due Date Override     */
                                             /* With FOB Delivery     */
            return

L50140: REM EDI Process Selection                 EDI_SEL$
           edi_sel% = 0%
           edi_processed$ = " "
           if edi_sel$ <> " " then goto L50200
              goto L50280

L50200:    convert edi_sel$ to edi_sel%, data goto L50280

           convert edi_sel% to edi_sel$, pic(0)
           if edi_sel% < 1% or edi_sel% > 6% then goto L50280
              edi_desc$ = str(scr$(edi_sel% + 1%),6%,25%)
              if edi_sel$ = "1" then edi_processed$ = "N"
              if edi_sel$ = "3" then edi_processed$ = "Y"
        return
L50280:    errormsg$ = "(Error) - Process Selection Required?"
           gosub error_prompt
           init(" ") edi_sel$, edi_desc$, edi_processed$
           edi_sel% = 0%
        return

L50340: REM EDI Partner Selection                 EDI_PARTNER$
           if edi_sel% = 2% and str(edi_partner$,1%,1%) = "A" then       ~
                                                               goto L50580
           if str(edi_partner$,1%,1%) <> "A" then goto L50420
              edi_partner$ = "ALL"
              edi_partner_desc$ = "(A)ll Valid Partners. "
              if edi_sel% <> 2% then fieldnr% = 3%
              return
L50420:    convert edi_partner$ to edi_partner%, data goto L50540

           convert edi_partner% to edi_partner$, pic(000)
           init(" ") readkey$
           str(readkey$,1%,9%)   = "PARTNERS "
           str(readkey$,10%,15%) = edi_partner$
           read #2,key = readkey$, using L50500, edi_partner_desc$,       ~
                                                eod goto L50540
L50500:       FMT POS(25), CH(30)
           edi_p$ = str(edi_partner_desc$,1%,15%)
           if edi_sel% <> 2% then fieldnr% = 3%
        return
L50540:    errormsg$ = "(Error) - Invalid Partner Selection Entered?"
           gosub error_prompt
           init(" ") edi_partner$, edi_partner_desc$, edi_p$
        return
L50580:    errormsg$ = "(Error) - (A)ll is Not Valid for Selection (2)?"
           gosub error_prompt
           init(" ") edi_partner$, edi_partner_desc$, edi_p$
        return

L50630: REM Due Date Override                     DUE_DATE$
           if due_date$ <> " " then goto L50670
              init(" ") due_date$, due_dte$, fob$
              return
L50670:    date% = 0%
           call "DATEOK" (due_date$, date%, errormsg$ )
           if date% = 0% then goto L50750
              due_dte$ = due_date$
              call "DATUNFMT" (due_dte$)
              gosub check_fob
              if fob% = 0% then goto L50790
        return
L50750:    errormsg$ = "(Error) - Invalid Date Entered?"
           gosub error_prompt
           init(" ") due_date$, due_dte$, fob$
        return
L50790:    errormsg$ = "(Error) - Invalid FOB Code Entered?"
           gosub error_prompt
           init(" ") due_date$, due_dte$, fob$
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                   /* Report Header */
L55050: %!######## ########                          ####################~
        ~####################                                    APCEDI02:~
        ~!

L55090: %!User Id: ###                                    ###############~
        ~###############                                       Page: #####~
        ~!

L55130: %+---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~+

L55170: %!---------------------------------------------------------------~
        ~-----------------------------------------------------------------~
        ~!

                                                   /* Column Header   */
L55220: %!Parent Cont No.!Parent!APC  Cust!Parent PO Number! Ln!<-- Paren~
        ~t Sku Number -->!<--- APC Part Number --->!Qty !S.O. Tot!S.0. ASN~
        ~!

L55260: %!---------------!------!---------!----------------!---!---------~
        ~----------------!-------------------------!----!--------!--------~
        ~!

                                                   /* Detail Data   */
L55310: %!###############!######!#########!################!###!#########~
        ~################!#########################!####!########!########~
        ~!

L55350: %!               !      !         !################!   !         ~
        ~                ! S.O   T O T A L ------->!    !########!        ~
        ~!

L55390: %!               !      !         !                !   !         ~
        ~                ! TOTAL ALL SALES ORDERS->!    !########!        ~
        ~!

L55430: %+--------------------------------------------------------------+
L55440: %! ######## @ ########   EDI Report Totals                      !
L55450: %!                                                              !
L55460: %!                                                              !
L55470: %! (0) Aluminum Frame Screens ------------->  (######)          !
L55480: %!                                                              !
L55490: %! (1) Storm Windows ---------------------->  (######)          !
L55500: %!                                                              !
L55510: %! (2) Storm Doors ------------------------>  (######)          !
L55520: %!                                                              !
L55530: %! (3) Patio Doors ------------------------>  (######)          !
L55540: %!                                                              !
L55550: %! (4) (Available) ------------------------>  (######)          !
L55560: %!                                                              !
L55570: %! (5) Aluminum Prime Windows ------------->  (######)          !
L55580: %!                                                              !
L55590: %! (6) Vinyl Replacement Windows ---------->  (######)          !
L55600: %!                                                              !
L55610: %! (7) Vinyl Prime Windows ---------------->  (######)          !
L55620: %!                                                              !
L55630: %! (8) Vinyl Casement Windows ------------->  (######)          !
L55640: %!                                                              !
L55650: %! (9) Bay and Bow Windows ---------------->  (######)          !
L55660: %!                                                              !
L55670: %! (A) Vinyl Patio Doors (312) ------------>  (######)          !
L55680: %!                                                              !
L55690: %! (B) Vinyl Hopper (870) ----------------->  (######)          !
L55700: %!                                                              !
L55710: %+--------------------------------------------------------------+

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        prompt_user
            comp% = 2%
            hdr$ = "** EDI Master PO Processing **"
            msg$(1) = " **********  Do You Wish to Continue  ********** "
            msg$(2) = "Press <RETURN> To 'Exit Process', or Press Any   "
            msg$(3) = "(PF) Key To Continue...........                  "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

        report_status
            comp% = 2%
            x% = len(edi_desc$)
            hdr$ = "****"&str(edi_desc$,1%,x%)&"****"
            msg$(1) = " ******* No Data Found Matching Criteria ******* "
            msg$(2) = "                                                 "
            msg$(3) = "         Press any Key To Continue......         "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

        select_printer
            call "SHOSTAT" ("Creating EDI Process Report")
            cnt% = 0% : page_no% = 0% : lcnt% = 99%
            date$ = date  :  call "DATEFMT" (date$)
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "SETPRNT" ("APCEDI", " ", 0%, 0%)
            print_title$ = "APC EDI PO Process Report"
            call "FMTTITLE" (print_title$, " ", 12%)
            call "FMTTITLE" (edi_desc$, " ", 12%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("APCEDI", " ", 0%, 1%)
        return

        print_header
          if lcnt% <> 99% then print using L55130
          page_no% = page_no% + 1%
          print page
          print using L55130
          print using L55050, date$, rpt_time$, print_title$
          print using L55090, userid$, edi_desc$, page_no%
          print using L55170
          print using L55220
          print using L55260
          lcnt% = 6%
          save_po$ = edi_po$
          save_cust$ = edi_cust$
        return

        print_detail
          if lcnt% > 56% then gosub print_header
             if save_po$ = edi_po$ and save_cust$ = edi_cust$            ~
                                                  then goto L60680
                print using L55260
                lcnt% = lcnt% + 1%
                if edi_sel$ <> "3" then goto L60650
                   print using L55350, save_po$, save_tot$
                   print using L55260
                   lcnt% = lcnt% + 2%
L60650:         save_po$   = edi_po$
                save_cust$ = edi_cust$

L60680:   if lcnt% > 56% then gosub print_header
          print using L55310, edi_dun$, edi_store$, edi_cust$, edi_po$,   ~
                             edi_ln$, edi_sku$, edi_part$, edi_qty$,     ~
                             edi_dt2$, edi_so$
                                     /* BUILD REPORT TOTALS BY PRODUCT */
              convert str(edi_part$,1%,1%) to x%, data goto L60850

              convert edi_qty$ to y%, data goto L60850

              mod$ = str(edi_part$,1%,3%)
              if mod$ = "312" or mod$ = "870" then goto L60820
                 tt%(x% + 1%) = tt%(x% + 1%) + y%
                 goto L60850

L60820:       if mod$ = "312" then tt%(11%) = tt%(11%) + y%
              if mod$ = "870" then tt%(12%) = tt%(12%) + y%

L60850:   lcnt% = lcnt% + 1%
        return

        print_report
            gosub select_printer
            init(" ") save_so$, save_po$, save_cust$, tot_report$
            tot_report = 0.0
            edi_key$ = all(hex(00))                /* Processed Flag   */
            str(edi_key$,1%,1%) = edi_processed$   /* 'Y' or 'N'       */
            if edi_partner$ <> "ALL" then str(edi_key$,2%,15%) = edi_p$
                                                   /* 15 Char Dun's No */
            read #4,key > edi_key$, using L61020, edi_rec$,               ~
                                                      eod goto print_done
            save_po$ = str(edi_rec$,32%,16%)
            goto L61030
        next_part                                  /* Read Next Loop   */
            read #4, using L61020, edi_rec$, eod goto print_done
L61020:       FMT CH(102)
L61030:     if edi_processed$ <> str(edi_rec$,10%,1%) then               ~
                                                          goto print_done
               edi_dun$   = str(edi_rec$,11%,15%)
               quote% = 0%                         /* Set Price Quote */
               if edi_dun$ = "XXXXXXXXXXXXXXX" then quote% = 1%
               if edi_partner$ = "ALL" then goto L61110
                  if edi_p$ <> edi_dun$ then goto next_part
                                              /* Check for Price Quote */
L61110:        if quote% = 1% then goto L61170
                  store% = 0%
                  convert str(edi_rec$,26%,6%) to store%, data goto L61140
L61140:
                  convert store% to edi_store$, pic(000000)
                  goto L61190
L61170:        edi_store$ = str(edi_rec$,26%,6%)
                                                  /* When QUOTE% = 1%  */
L61190:        edi_po$    = str(edi_rec$,32%,16%) /* Contains Quote Id */
               edi_ln$    = str(edi_rec$,48%,3%)  /* Contains Quote Ln */

               edi_sku$   = str(edi_rec$,51%,25%) /* Contains Part No  */
               edi_qty$   = str(edi_rec$,76%,10%)
               edi_qty%   = 0%
               convert edi_qty$ to edi_qty%, data goto L61260
L61260:
               convert edi_qty% to edi_qty$, pic(0000)
               edi_dte1$  = str(edi_rec$,86%,6%)  /* Requseted Ship DTE*/
               edi_dte2$  = str(edi_rec$,92%,6%)  /* Actual Ship DTE   */
               edi_dt1$   = edi_dte1$
               edi_dt2$   = edi_dte2$
               call "DATEFMT" (edi_dt1$)
               call "DATEFMT" (edi_dt2$)          /* (EWD003)         */
               get str(edi_rec$,98%,4%), using L61350, edi_so%
L61350:            FMT BI(4)
               if edi_processed$ = "N" then edi_so% = 0%
               convert edi_so% to edi_so$, pic(00000000)
               if edi_sel$ <> "3" then goto L61490
                  if save_so$ <> " " then goto L61440
                     gosub lookup_customer
                     save_so$ = edi_so$
                     gosub sales_order

L61440:           if save_so$ = edi_so$ then goto L61490
                     save_tot$ = ord_tot$
                     save_so$  = edi_so$
                     gosub sales_order

L61490:        gosub lookup_customer
               cnt% = cnt% + 1%
            gosub print_detail
            goto next_part
        print_done
            if cnt% <> 0% then goto L61570
               gosub report_status
               goto L61680
L61570:     if edi_sel$ <> "3" then goto L61640
               convert tot_report to tot_report$, pic(#####.##)

               print using L55260
               print using L55350, edi_po$, ord_tot$
               print using L55260
               print using L55390, tot_report$
L61640:     print using L55130

            gosub print_totals

L61680:     gosub close_printer
        return clear all
        goto inputmode

        lookup_customer
            if quote% = 0% then goto L61770
               edi_cust$ = edi_store$          /* Price Quote System    */
               goto L61820

L61770:     readkey$ = " "
            str(readkey$,1%,15%) = edi_dun$
            str(readkey$,16%,6%) = edi_store$
            read #3,key = readkey$, using L61810,edi_cust$,eod goto L61840
L61810:        FMT POS(22), CH(9)
L61820:     gosub lookup_sku
        return
L61840:     edi_cust$ = "Undefined"
            edi_part$ = "Undefined"
        return

        lookup_sku
            if quote% = 0% then goto L61940
               edi_part$ = edi_sku$           /* Price Quote System    */
               sku% = 1%
               return

L61940:     sku% = 0%                        /* Read Customer Sku Code */
            init(" ") edi_part$, sku_code$
            read #1,key = edi_cust$,using L61970, sku_code$,eod goto L62080
L61970:        FMT POS(1000), CH(3)

            sku_key$ = all(hex(00))
            str(sku_key$,1%,3%)  = sku_code$
            str(sku_key$,4%,25%) = edi_sku$
            read #5,key = sku_key$, using L62030,edi_part$, eod goto L62060
L62030:       FMT POS(32), CH(25)
            sku% = 1%
        return
L62060:     edi_part$ = "Undefined"
        return
L62080:     edi_part$ = "Undf Code"
        return

        purge_data                                  /* FLAG AS HISTORY */
            call "SHOSTAT" ("Purging Processed PO'S to History")
            cnt% = 0%
            edi_key$ = all(hex(00))
            str(edi_key$,1%,1%) = "Y"
            if edi_partner$ <> "ALL" then str(edi_key$,2%,15%) = edi_p$
        purge_next
            read #4,hold,key > edi_key$, using L62200, edi_rec$,          ~
                                                      eod goto purge_done
L62200:       FMT CH(102)
            edi_key$ = str(edi_rec$,10%,41%)
            if str(edi_key$,1%,1%) <> "Y" then goto purge_done
               if edi_partner$ = "ALL" then goto L62260
                  if edi_p$ <> str(edi_key$,2%,15%) then goto purge_next

L62260:        delete #4
               str(edi_rec$,10%,1%) = "Z"                   /* HISTORY */
               write #4, using L62200, edi_rec$, eod goto L62380
               cnt% = cnt% + 1%
               goto purge_next
        purge_done
            convert cnt% to cnt$, pic(00000)
            call "SHOSTAT"                                               ~
                 ("Number of Records Moved to History ( "&cnt$&" )")
            stop
        return clear all
        goto inputmode
L62380:     call "SHOSTAT" ("(Error)-Purging to History --> "& edi_key$)
            stop
            goto purge_next

        p_data                                       /* DELETE HISTORY */
            call "SHOSTAT" ("Purging History Data")
            cnt% = 0%
            edi_key$ = all(hex(00))
            str(edi_key$,1%,1%) = "Z"
            if edi_partner$ <> "ALL" then str(edi_key$,2%,15%) = edi_p$
        p_next
            read #4,hold,key > edi_key$, using L62510, edi_key$,          ~
                                                      eod goto p_done
L62510:       FMT XX(9), CH(41)
            if mod(cnt%,25%) <> 0 then goto L62540
               print at(03,38);hex(84);"[";cnt%;"]"
L62540:     if str(edi_key$,1%,1%) <> "Z" then goto p_done
               if edi_partner$ = "ALL" then goto L62580
                  if edi_p$ <> str(edi_key$,2%,15%) then goto p_next

L62580:        delete #4
               cnt% = cnt% + 1%
               goto p_next
        p_done
            convert cnt% to cnt$, pic(00000)
            call "SHOSTAT" ("Number of Records Purged ( "&cnt$&" )")
            stop
        return clear all
        goto inputmode

        sales_order
             ord_disc, ln_disc, ord_tot = 0.0
             init(" ") ord_key$
             str(ord_key$,1%,9%)  = edi_cust$
             str(ord_key$,10%,8%) = edi_so$
             read #6,key = ord_key$, using L62750, ord_disc,              ~
                                                          eod goto L62760
L62750:         FMT POS(859), PD(14,4)
L62760:
             ord_key$ = all(hex(00))
             str(ord_key$,1%,8%) = edi_so$
             read #7,key > ord_key$, using L62830, bck_rec$,eod goto L62960
             goto L62840
        next_sales_order
             read #7, using L62830, bck_rec$, eod goto L62960
L62830:         FMT CH(200)
L62840:      ord_ord$ = str(bck_rec$,10%,16%)
             get str(bck_rec$,93%,8%) using L62860, ord_qty
L62860:        FMT PD(14,4)
             get str(bck_rec$,165%,16%) using L62880, ord_price, ln_disc
L62880:        FMT 2*PD(14,4)
             if ord_ord$ <> edi_so$ then goto L62960
                tot_price =  round(  ord_price * ord_qty, 2)
                                         /* CALCULATE LINE DISCOUNT */
                discamt     =  round( tot_price * ln_disc * .01, 2)
                tot_price =  round( tot_price - discamt, 2)
                ord_tot = round(ord_tot + tot_price, 2)
          goto next_sales_order
L62960:  discamt =  round( ord_tot * ord_disc * .01, 2)
         ord_tot =  round( ord_tot - discamt, 2)
         convert ord_tot to ord_tot$, pic(#####.##)
         tot_report = tot_report + ord_tot
        return

        delete_data                                  /* DELETE N's     */
            call "SHOSTAT" ("Deleting Invalid Records/Sales Orders")
            edi_key$ = all(hex(00))
            str(edi_key$,1%,1%) = "N"
            if edi_partner$ <> "ALL" then str(edi_key$,2%,15%) = edi_p$
        delete_next
            read #4,hold,key > edi_key$, using L63100, edi_rec$,          ~
                                                    eod goto delete_done
L63100:       FMT CH(102)
            edi_key$ = str(edi_rec$,10%,41%)
            edi_sku$ = str(edi_rec$,51%,25%)
            gosub lookup_sku
            if sku% = 1% then goto delete_next
            if str(edi_key$,1%,1%) <> "N" then goto delete_done
               if edi_partner$ = "ALL" then goto L63190
                  if edi_p$ <> str(edi_key$,2%,15%) then goto delete_next

L63190:        delete #4
               goto delete_next
        delete_done

        return clear all
        goto inputmode

        check_fob
            fob% = 0%
            readkey$ = all(hex(00))
            str(readkey$,1%,9%)   = "PLAN DELV"
            str(readkey$,10%,15%) = str(fob$,1%,2%)
            read #2,key = readkey$, using L63320, fob_desc$,eod goto L63350
L63320:        FMT POS(25), CH(30)
            str(fob$,3%,18%) = "/" & fob_desc$
            fob% = 1%
L63350: return

        print_totals
          print page
          print skip(5)
          print using L55430
          print using L55440, date$, rpt_time$
          print using L55450
          print using L55460
          print using L55470, tt%(1)
          print using L55480
          print using L55490, tt%(2)
          print using L55500
          print using L55510, tt%(3)
          print using L55520
          print using L55530, tt%(4)
          print using L55540
          print using L55550, tt%(5)
          print using L55560
          print using L55570, tt%(6)
          print using L55580
          print using L55590, tt%(7)
          print using L55600
          print using L55610, tt%(8)
          print using L55620
          print using L55630, tt%(9)
          print using L55640
          print using L55650, tt%(10)
          print using L55660
          print using L55670, tt%(11)
          print using L55680
          print using L55690, tt%(12)
          print using L55700
          print using L55710
        return

        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end
