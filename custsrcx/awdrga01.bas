        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDRGA01                             *~
            *  Creation Date     - 05/16/05                             *~
            *  Last Modified Date- 06/09/06                             *~
            *  Written By        - Roy H. Hoffman                       *~
            *  Last Modified By  - Roy H. Hoffman                       *~ 
            *                                                           *~
            *  Description       - New RGA Data Entry and Barcode Label *~
            *                      print.                               *~
            *                                                           *~
            *  Code Tables Used  -                                      *~
            *                      (RGASTATUS) '---' Save Last Serial   *~
            *                                  Number Assigned          *~
            *        (RGAREASON) RGA Reason Code Associated with Return *~
            *        (RGASTATUS) RGA Salvage Tracking Status Code       *~
            *        (RGATR-LOC) RGA Trailer/Location                   *~
            *        (RGASURGE ) RGA Surge Sku's for use by Planning    *~
            *        (RGASALVAG) 2005 Salvage Costs by Department       *~  
            *                                                           *~
            *  Subroutine Used   - AWDRGB01 (Print RGA Barcode Labels)  *~
            *                                                           *~
            *                                                           *~
            *  Special gosubs -                                         *~
            *                                                           *~
            *                                                           *~ 
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/16/05 ! (New) Entry and Labels for the RGA System! RHH *~
            * 05/26/05 ! (AWD001) Change the size of the Serial   ! RHH *~
            *          !    Number on the RGA Label. And add      !     *~
            *          !    Customer Code to the Database         !     *~
            * 01/01/06 ! (PAR000) CR347 Mods for the new Sub Part ! RHH *~
            *          !    Number. Also Change for New RGA       ! &   *~
            *          !    Printer.                              ! CMG *~
            * 06/09/06 ! (PAR001) Mods for Change to (APCPLNWT)   ! RHH *~
            *          !                                          !     *~  
            *************************************************************

        dim                                                              ~
            readkey$50,                  /* GENCODES Lookup            */~
            sc_serial$8,                 /* RGA Serial Number          */~
            sc_return_dte$10,            /* RGA Return Date (Entry)    */~
            sc_warranty$8,               /* Warranty ID                */~
            sc_barcode$18,               /* Production Barcode         */~
            sc_complaint$8, sc_comp$8,   /* Complaint number           */~
            sc_so_ord$8,                 /* Sales Order                */~
            sc_so_line$3,                /* Sales Order Line Item      */~
            sc_rga_no$4,                 /* RGA Number Assigned        */~
            sc_rga_ln$2,                 /* RGA Line Number            */~
            sc_status$3,sc_status_desc$30, /* RGA Status Code          */~  
            sc_reason$3,sc_reason_desc$30, /* RGA Reason Code          */~
            sc_trailer$6,                /* Trailer/Location           */~
            sc_trailer_desc$30,          /* Trailer Description        */~
            sc_dept$3, sc_dept_desc$30,  /* Production Dept and Descr  */~
            sc_quality$1, sc_quality_dte$10, /* Flag for Quality Check */~
            sc_comment$32,               /* Additional Information     */~
            sc_part$25, sc_part_desc$30, /* Part No. and Description   */~
            sc_sub_part$20,              /* Sub Part Number    (PAR000)*/~
            sub_flg$1,                   /* Error Location Flag(PAR000)*/~        
            sc_price$10,                 /* Window Price               */~
            sc_sal_tot$10,               /* Salvage Total Price        */~
            sc_sal_mat$10,               /* Salvage Material Price     */~
            sc_sal_lab$10,               /* Salvage Labor Price        */~
            sc_sal_ovh$10,               /* Salvage Overhead Price     */~
            sc_salvage_desc$30,          /* Salvage Descr for Lookup   */~
            sc_comp_cd$7,                /* Complaint Code Assoc. Comp */~
            sc_comp_desc$30,             /* Complaint Code Descr       */~
            sc_reas_cd$3,                /* Complaint Reason Code      */~
            sc_reas_desc$30,             /* Complaint Reason Descript  */~
            sc_credit$10,                /* RGA Credit Amount          */~
            sc_credit_dte$10,            /* Dated of Creidt Amount     */~
            sc_support$3,                /* Support Dept Lookup        */~
            sc_orig_date$10,             /* S.O. Original Order Date   */~ 
            sc_cust$9,                   /* S.O Customer Code          */~
            wrk_key$50, wrk_rec$(2%)256, /* Work Key and Record        */~
            ar_rec$(8%)250,              /* ARIMASTR Header Rec        */~
            rga_key0$8, rga_rec$(2%)256, /* RGA Primary Key            */~      
            rga_key1$11,                 /* RGA Alt Key 1              */~      
            rga_key2$8,                  /* RGA Alt Key 2              */~      
            rga_key3$18,                 /* RGA Alt Key 3              */~      
            rga_key4$5,                  /* RGA Alt Key 4              */~      
            rga_key5$6,                  /* RGA Alt Key 5              */~      
            rga_key6$11,                 /* RGA Alt Key 6              */~      
            rga_key7$25,                 /* RGA Alt Key 7              */~
            rga_dt_key$10,rga_dt_rec$180,/* Rga Detail Primary Key     */~       
            filename$8,                  /* Used by EWDOPEN            */~ 
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            date$8,                      /* Date for screen display    */~
            calc_time$8,                 /* User for time calc         */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            wandchar$1,                  /* Wand Character - Scanner   */~
            userid$3                     /* Current User Id            */

        dim f2%(10%),                    /* = 0 if the file is open    */~
                                         /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */ 

                                         /* (AWDRGAHD) - Header Data   */
        dim rga_status$3,                /* RGA Status/Store(RGASTATUS)*/~
            rga_serial$8,rga_serial_desc$128,/* RGA Serial No. (Barcode)*/~
            rga_warranty_id$8,           /* Warranty Id from Sales Ord */~
            rga_so_barcode$18,           /* Barcode assoc. with S. O.  */~
            rga_comp_no$5,               /* Complaint No. with S. O.   */~
            rga_number$4,                /* RGA No. Assigned to Prod.  */~
            rga_number_ln$2,             /* RGA Line Item No.          */~  
            rga_sales_ord$8,             /* Sales Ord. Assoc. W/Prod.  */~		 
            rga_sales_ln$3,              /* Line Item Assoc. W/S.O.    */~
            rga_part$25,                 /* RGA Part No. / MFG Part No.*/~
            rga_descr$30,                /* Part No. Description       */~			
	    rga_comp_code$7,             /* Compalint Code W/Complaint */~
            rga_comp_reas$3,             /* Reason Code W/ Complaint   */~ 
            rga_trailer_loc$6,           /* Trailer/Location Assigned  */~
            rga_reason_cd$3,             /* RGA Reason Code (RGAREASON)*/~
            rga_comment$32,              /* Additional Info. Assoc RGA */~			
            rga_qty_chk$1,               /* Flagged for QTY Check Y/N  */~
            rga_qty_dte$10,              /* RGA Date Checked by Quality*/~
            rga_value$10,                /* RGA Sales Value            */~
            rga_sal_value$10,            /* RGA Salvage Total Value    */~
            rga_sal_mat$10,              /* RGA Salvage Material Value */~
            rga_sal_labor$10,            /* RGA Salvage Labor Value    */~
            rga_sal_over$10,             /* RGA Salvage Overhead Value */~
            rga_credit_amt$10,           /* Credit Amt Assoc. W/S.O.   */~
            rga_credit_dte$10,           /* Date Assoc. with Credit Amt*/~
            rga_orig_date$10,            /* Original Order Date of S.O.*/~
            rga_return_dte$10,           /* Date Assoc. w. RGA Return  */~
            rga_return_tme$4,            /* Time 1st Entere into System*/~
            rga_init_usr$3,              /* User Id for Initial Entry  */~
            rga_dept$3,                  /* Production Department Code */~
            rga_stat_dte$10,             /* Data of Last Status Change */~
            rga_cust$9,                  /* Customer Code              */~
            rga_sub_part$20,             /* RGA Sub Part No.   (PAR000)*/~
            rga_filler$227               /* RGA Header Filler  (PAR000)*/
 
                                         /* (AWDRGADT)- Tracking Detail*/
        dim                              /* RGA Status/Store(RGASTATUS)*/~
                                         /* RGA Serial No. (Barcode)   */~
            rga_status_seq$2,            /* Add. Data for Same Status  */~
                                         /* RGA Reason Code (RGAREASON)*/~
                                         /* RGA Status/Store(RGASTATUS)*/~
                                         /* Add. Data for Same Status  */~		 
            rga_status_dte$10,           /* Date Associated W Status   */~
            rga_status_usr$3,            /* User Id Assoc. W Status Chg*/~
            rga_status_tme$4,            /* Time Assoc. with Status    */~			
	                                 /* Trailer / Location Detail  */~
                                         /* Addional Information       */~ 
            rga_fill_dt$61               /* RGA Detail Filler          */

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

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21                                         
            apc$   = "(AWD) Create RGA Barcode Label and Data"
            pname$ = "AWDRGA01 - 06/09/2006"               /* (PAR001) */ 
                                                           /* (PAR000) */
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
            * #1  ! AWDRGAHD ! RGA Tracking Header File         (PAR000)*~
            * #2  ! AWDRGADT ! RGA Tracking Detail File                 *~
            * #3  ! APCPLNDP ! Planning Master Dept File for Model      *~
            * #4  ! APCPLNDT ! Planning Master Detail File              *~
            * #5  ! GENCODES ! Master Table File                        *~
            * #6  ! APCPLNWT ! Warranty Id Cross-REF to S.O.    (PAR001)*~
            * #7  ! APCCOMPT ! Complaint Tracking Master File           *~
            * #8  ! BCKMASTR ! S.O. Header Master                       *~
            * #9  ! BCKLINES ! S.O. Line Item Detail                    *~ 
            * #10 ! ARIMASTR ! Invoice Master- Headers                  *~
            * #11 ! ARILINES ! Invoice Master- Line Items               *~
            * #12 ! APCRGAHD ! APC RGA Header Master File               *~
            * #14 ! APCRGADT ! APC RGA Detail Master File               *~
            * #63 ! BCKSUBPT ! New Sub Part Number File         (PAR000)*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select  #1, "AWDRGAHD",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 512,                                   ~
                        keypos = 10, keylen = 8,                          ~
                        alt key 1, keypos =  1, keylen =  17,            ~
                            key 2, keypos = 18, keylen =   8, dup,       ~
                            key 3, keypos = 26, keylen =  18, dup,       ~
                            key 4, keypos = 44, keylen =   5, dup,       ~
                            key 5, keypos = 49, keylen =   6, dup,       ~
                            key 6, keypos = 55, keylen =  11, dup,       ~
                            key 7, keypos = 66, keylen =  25, dup  
 

            select #2,  "AWDRGADT",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 128,                                   ~
                        keypos =  7, keylen = 10,                        ~
                        alt key 1, keypos  =  1, keylen = 16

            select  #3, "APCPLNDP",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  32,                                   ~
                        keypos = 11, keylen = 12,                        ~
                        alt key 1, keypos =  9, keylen =  14,            ~
                            key 2, keypos =  4, keylen =  12,            ~
                            key 3, keypos =  1, keylen =  12

            select #4,  "APCPLNDT",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  256,                                  ~
                        keypos =   24, keylen =  23,                     ~
                        alt key  1, keypos =   47, keylen =  57,         ~
                            key  2, keypos =   53, keylen =  51,         ~
                            key  3, keypos =    1, keylen =  23, dup,    ~
                            key  4, keypos =   96, keylen =   8, dup  

            select #5,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24
                                                         /* (PAR001)    */    
            select #6,  "APCPLNWT",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  128,                                  ~
                        keypos =    1, keylen =   8,                     ~
                        alt key  1, keypos =    9, keylen =  10, dup,    ~
                            key  2, keypos =    9, keylen =  18
 
                                                         /* (PAR001)    */
                                                         /* (PAR000)    */
            select #7,  "APCCOMPT",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  256,                                  ~
                        keypos =    1, keylen =   5,                     ~
                        alt key  1, keypos =    6, keylen =  10,         ~
                            key  2, keypos =   16, keylen =  16, dup,    ~
                            key  3, keypos =   32, keylen =   8, dup,    ~
                            key  4, keypos =   40, keylen =  13, dup  

            select #8,  "BCKMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1000,                                   ~
                        keypos = 1, keylen = 25,                         ~
                        alt key 1, keypos  = 26, keylen = 16, dup

            select #9,  "BCKLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 10, keylen = 19

            select #10, "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  17,                     ~
                        alternate key 1, keypos = 10, keylen =  8, dup,  ~
                                  key 2, keypos = 18, keylen = 16, dup,  ~
                                  key 3, keypos = 34, keylen = 16, dup,  ~
                                  key 4, keypos = 1783, keylen = 26

            select #11, "ARILINES",                                      ~
                        varc,     indexed,  recsize =  750,              ~
                        keypos =    1, keylen =  20

            select #12, "APCRGAHD",                                      ~
                        varc,     indexed,  recsize =   80,              ~
                        keypos =   12, keylen =   4,                     ~
                        alt key  1, keypos =  10, keylen =   6,          ~
                            key  2, keypos =   1, keylen =  15

            select #14, "APCRGADT",                                      ~
/*(PAR000)*/            varc,     indexed,  recsize =  512,              ~
                        keypos =   12, keylen =   6,                     ~
                        alt key  1, keypos =  10, keylen =   8,          ~
                            key  2, keypos =   1, keylen =  17

                                                        /* (PAR000)     */
            select #63, "BCKSUBPT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  11,                     ~
                        alt key  1, keypos =  12, keylen =  11, dup,     ~
                            key  2, keypos =  23, keylen =  45, dup

                                                       /* (PAR000)     */

            call "SHOSTAT" ("Opening Files, One Moment Please")


            call "OPENCHCK" (#1, fs%(1%), f2%(1%),500%, rslt$(1%))

            call "OPENCHCK" (#2, fs%(2%), f2%(2%),500%, rslt$(2%))

            filename$ = "APCPLNDP" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "APCPLNDT" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "GENCODES" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "APCPLNWT" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "APCCOMPT" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "BCKMASTR" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "BCKLINES" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "ARIMASTR" : call "EWDOPEN" (#10, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "ARILINES" : call "EWDOPEN" (#11, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "APCRGAHD" : call "EWDOPEN" (#12, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "APCRGADT" : call "EWDOPEN" (#14, filename$, err%)
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

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   18%
L10110:         gosub'051(fieldnr%)        /* Default - Enables */
                      if enabled% = 0% then L10240
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
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
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
                  if keyhit%  = 16% then gosub create_rga_label
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 18% then editpg1
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
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
           
        return

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
                                                             
        scrn1_msg  :  data                                                 ~
         "**Enter a Serial Number for Lookup Or <RETURN> to have Assigned.",~
         "**Enter a Warranty Id for Lookup or <RETURN> if not known.     ",~
         "**Enter the Production Barcode or <RETURN> if not know.        ",~
         "**Enter the Complaint Number or <RETURN> if not known.         ",~
         "Enter RGA Number and Line Item Assigned or <RETURN> if not known.",~
         "Enter Sales Order with Line Item or <RETURN> if not known.     ",~
         "Enter a Valid RGA Status Code <Required>?                      ",~
         "Enter a Valid RGA Reason Code <Required>?                      ",~
         "Enter or Verify Manufacture Part Number.                       ",~
         "Enter a Valid Trailer / Location if Applicable?                ",~
         "Enter the Production Department or <RETURN> inf not Known.     ",~
         "Enter 'Y' to Flag for Quality Check or 'N' if not Required.    ",~
         "Enter Additinal information if Necessary.                      ",~
         "Enter or Verify Window Price.                                  ",~
         "Enter or Verify Salvage Price Total.                           ",~
         "Enter or Verify Complaint Code if known.                       ",~
         "Enter or Verify Complaint Reason Code if known.                ",~
         "Enter or Verify RGA Credit Amount and Date if known.           "


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
            init(" ") errormsg$, inpmessage$,sc_serial$, sc_return_dte$,     ~
                sc_warranty$, sc_barcode$, sc_complaint$, sc_so_ord$, sc_so_line$,~
                sc_rga_no$, sc_status$, sc_status_desc$, sc_reason$,         ~
                sc_reason_desc$, sc_trailer$, sc_dept$, sc_dept_desc$,       ~
                sc_quality$, sc_quality_dte$, sc_comment$, sc_part$,         ~
                sc_part_desc$, sc_price$, sc_sal_tot$, sc_sal_mat$,          ~
                sc_sal_lab$, sc_sal_ovh$, sc_comp_cd$, sc_comp_desc$,        ~
                sc_reas_cd$, sc_comp_desc$, sc_credit$, sc_credit_dte$,      ~
                wandchar$, wrk_key$, wrk_rec$(), sc_trailer_desc$, sc_cust$,   ~
                sc_orig_date$, rga_key1$, rga_key7$,sc_reas_desc$, sc_rga_ln$, ~
                sc_sub_part$  
                                                                   /* (PAR000)  */
        REM init_header
            init(" ") rga_status$, rga_serial$, rga_warranty_id$, rga_so_barcode$,    ~
                rga_comp_no$, rga_number$, rga_sales_ord$, rga_sales_ln$, rga_part$,  ~
                rga_descr$, rga_comp_code$, rga_comp_reas$, rga_trailer_loc$,         ~
                rga_reason_cd$, rga_comment$, rga_qty_chk$, rga_qty_dte$, rga_value$, ~
                rga_sal_value$, rga_sal_mat$, rga_sal_labor$, rga_sal_over$,          ~
                rga_credit_amt$, rga_credit_dte$, rga_orig_date$, rga_return_dte$,     ~
                rga_return_tme$, rga_init_usr$, rga_dept$, rga_stat_dte$, rga_filler$, ~
                rga_number_ln$, rga_cust$, rga_sub_part$ 
                                                                   /* (PAR0001) */
        REM init_detail
            init(" ") rga_status$, rga_serial$, rga_status_seq$, rga_reason_cd$,  ~
                      rga_status_dte$, rga_status_usr$, rga_status_tme$,          ~
                      rga_trailer_loc$, rga_comment$, rga_fill_dt$
 
        return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

                                         /* Load Data                  */
        load_header                      /* (AWDRGAHD) - File          */
 
        get #1, using L35040,            /* (Header Data)              */~
            str(rga_return_dte$,1%,6%),  /* Date Assoc. w. RGA Return  */~
            rga_status$,                 /* RGA Status/Store(RGASTATUS)*/~
            rga_serial$,                 /* RGA Serial No. (Barcode)   */~
            rga_warranty_id$,            /* Warranty Id from Sales Ord */~
            rga_so_barcode$,             /* Barcode assoc. with S. O.  */~
            rga_comp_no$,                /* Complaint No. with S. O.   */~
            rga_number$,                 /* RGA No. Assigned to Prod.  */~
            rga_number_ln$,              /* RGA Line Item Number       */~ 
            rga_sales_ord$,              /* Sales Ord. Assoc. W/Prod.  */~		 
            rga_sales_ln$,               /* Line Item Assoc. W/S.O.    */~
            rga_part$,                   /* RGA Part No. / MFG Part No.*/~
            rga_descr$,                  /* Part No. Description       */~			
	    rga_comp_code$,              /* Compalint Code W/Complaint */~
            rga_comp_reas$,              /* Reason Code W/ Complaint   */~ 
            rga_trailer_loc$,            /* Trailer/Location Assigned  */~
            rga_reason_cd$,              /* RGA Reason Code (RGAREASON)*/~
            rga_comment$,                /* Additional Info. Assoc RGA */~			
            rga_qty_chk$,                /* Flagged for QTY Check Y/N  */~
            str(rga_qty_dte$,1%,6%),     /* RGA Date Checked by Quality*/~
            rga_value,                   /* RGA Sales Value            */~
            rga_sal_value,               /* RGA Salvage Total Value    */~
            rga_sal_mat,                 /* RGA Salvage Material Value */~
            rga_sal_labor,               /* RGA Salvage Labor Value    */~
            rga_sal_over,                /* RGA Salvage Overhead Value */~
            rga_credit_amt,              /* Credit Amt Assoc. W/S.O.   */~
            str(rga_credit_dte$,1%,6%),  /* Date Assoc. with Credit Amt*/~
            str(rga_orig_date$,1%,6%),   /* Original Order Date of S.O.*/~
            rga_return_tme$,             /* Time 1st Entere into System*/~
            rga_init_usr$,               /* User Id for Initial Entry  */~
            rga_dept$,                   /* Production Department Code */~
            rga_status_seq$,             /* RGA Last Status Seq Used   */~
            str(rga_stat_dte$,1%,6%),    /* Date of Last Status Change */~
            rga_cust$,                   /* Sales Order Customer Code  */~
            rga_sub_part$,               /* Sub Part No. Data  (PAR000)*/~
            rga_filler$                  /* RGA Header Filler          */
 

        return

        load_detail                      /* (AWDRGADT) - File          */

        get #2, using L35050,            /* (Detail File )             */~
            str(rga_status_dte$,1%,6%),  /* Date Associated W Status   */~
            rga_serial$,                 /* RGA Serial No. (Barcode)   */~
            rga_status_seq$,             /* Add. Data for Same Status  */~
            rga_status$,                 /* RGA Status/Store(RGASTATUS)*/~
            rga_reason_cd$,              /* RGA Reason Code (RGAREASON)*/~
            rga_status_usr$,             /* User Id Assoc. W Status Chg*/~
            rga_status_tme$,             /* Time Assoc. with Status    */~			
	    rga_trailer_loc$,            /* Trailer / Location Detail  */~
            rga_comment$,                /* Addional Information       */~ 
            rga_fill_dt$                 /* RGA Detail Filler          */


        return


 
        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

                                         /* Write Data                 */
        write_header                     /* (AWDRGAHD) - File          */
           init(" ") rga_key0$, calc_time$, rga_rec$()
    
           rga_key0$ = rga_serial$
           read #1,hold,key 0% = rga_key0$, using L35060, rga_rec$(),    ~
                                                          eod goto L30010
               delete #1

               put #1, using L35060, rga_rec$()

               goto L30015               /* Finished-Preserve Exisiting*/

L30010:   
           str(rga_return_dte$,1%,6%) = date
           calc_time$                 = time
           str(rga_return_tme$,1%,2%) = str(calc_time$,1%,2%)  /* Set Hour   */
           str(rga_return_tme$,3%,2%) = str(calc_time$,3%,2%)  /* Set Min    */

           rga_init_usr$              = userid$
           rga_status_seq$            = "00"   
           str(rga_stat_dte$,1%,6%)   = date
    
        put #1, using L35040,            /* (Header Data)              */~
            str(rga_return_dte$,1%,6%),  /* Date Assoc. w. RGA Return  */~
            rga_status$,                 /* RGA Status/Store(RGASTATUS)*/~
            rga_serial$,                 /* RGA Serial No. (Barcode)   */~
            rga_warranty_id$,            /* Warranty Id from Sales Ord */~
            rga_so_barcode$,             /* Barcode assoc. with S. O.  */~
            rga_comp_no$,                /* Complaint No. with S. O.   */~
            rga_number$,                 /* RGA No. Assigned to Prod.  */~
            rga_number_ln$,              /* RGA Line Item Number       */~
            rga_sales_ord$,              /* Sales Ord. Assoc. W/Prod.  */~		 
            rga_sales_ln$,               /* Line Item Assoc. W/S.O.    */~
            rga_part$,                   /* RGA Part No. / MFG Part No.*/~
            rga_descr$,                  /* Part No. Description       */~			
	    rga_comp_code$,              /* Compalint Code W/Complaint */~
            rga_comp_reas$,              /* Reason Code W/ Complaint   */~ 
            rga_trailer_loc$,            /* Trailer/Location Assigned  */~
            rga_reason_cd$,              /* RGA Reason Code (RGAREASON)*/~
            rga_comment$,                /* Additional Info. Assoc RGA */~			
            rga_qty_chk$,                /* Flagged for QTY Check Y/N  */~
            str(rga_qty_dte$,1%,6%),     /* RGA Date Checked by Quality*/~
            rga_value,                   /* RGA Sales Value            */~
            rga_sal_value,               /* RGA Salvage Total Value    */~
            rga_sal_mat,                 /* RGA Salvage Material Value */~
            rga_sal_labor,               /* RGA Salvage Labor Value    */~
            rga_sal_over,                /* RGA Salvage Overhead Value */~
            rga_credit_amt,              /* Credit Amt Assoc. W/S.O.   */~
            str(rga_credit_dte$,1%,6%),  /* Date Assoc. with Credit Amt*/~
            str(rga_orig_date$,1%,6%),   /* Original Order Date of S.O.*/~
            rga_return_tme$,             /* Time 1st Entere into System*/~
            rga_init_usr$,               /* User Id for Initial Entry  */~
            rga_dept$,                   /* RGA Production Depart Code */~
            rga_status_seq$,             /* RGA Last Status Seq Used   */~
            str(rga_stat_dte$,1%,6%),    /* Date of Last Status Change */~
            rga_cust$,                   /* Sales Order Customer Code  */~
            rga_sub_part$,               /* Sub Part No. Data  (PAR000)*/~
            rga_filler$                  /* RGA Header Filler          */
L30015: 
            write #1, eod goto write_header_error	

                                         /* Do Not Allow Change To     */
                                         /* RGA Detail Record          */
            if assign_flag% = 0% then gosub write_detail 

                                         /* Load for Label             */
           init(" ") rga_key0$, rga_rec$()
           rga_key0$ = rga_serial$
           read #1,key 0% = rga_key0$, using L35060, rga_rec$(),        ~
                                            eod goto read_header_error

        return

write_header_error:
            errormsg$ = "(Error) Writing Header for (Serial) " & rga_serial$
            gosub error_prompt
        return
read_header_error:
            errormsg$ = "(Error) Reading Header for (Serial) " & rga_serial$
            gosub error_prompt
        return
 
        write_detail                     /* (AWDRGADT) - File          */
           init(" ") rga_dt_key$
           str(rga_dt_key$,1%,8%) = rga_serial$
           str(rga_dt_key$,9%,2%) = "00"

           read #2,hold,key 0% = rga_dt_key$, using L31000, rga_dt_rec$,    ~
                                                          eod goto L31010
L31000:        FMT CH(128)

               delete #2

               put #2, using L31000, rga_dt_rec$

               goto L31015               /* Finished-Preserve Exisiting*/

L31010: 
        rga_status_seq$ = "00"  
        str(rga_status_dte$,1%,6%) = date
        rga_status_usr$            = userid$   
        calc_time$                 = time
        str(rga_status_tme$,1%,2%) = str(calc_time$,1%,2%)  /* Set Hour   */
        str(rga_status_tme$,3%,2%) = str(calc_time$,3%,2%)  /* Set Min    */

        put #2, using L35050,            /* (Detail File )             */~
            str(rga_status_dte$,1%,6%),  /* Date Associated W Status   */~
            rga_serial$,                 /* RGA Serial No. (Barcode)   */~
            rga_status_seq$,             /* Add. Data for Same Status  */~
            rga_status$,                 /* RGA Status/Store(RGASTATUS)*/~
            rga_reason_cd$,              /* RGA Reason Code (RGAREASON)*/~
            rga_status_usr$,             /* User Id Assoc. W Status Chg*/~
            rga_status_tme$,             /* Time Assoc. with Status    */~			
	    rga_trailer_loc$,            /* Trailer / Location Detail  */~
            rga_comment$,                /* Addional Information       */~ 
            rga_fill_dt$                 /* RGA Detail Filler          */
L31015:
            write #2, eod goto write_detail_error

        return

write_detail_error:

            errormsg$ = "(Error) Writing Detail for (Serial) " & rga_serial$
            gosub error_prompt
        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

                                         /* File = (AWDRGAHD)          */

L35040:                                                                  ~
            FMT CH(06),                  /* Date Assoc. w. RGA Return  */~
                CH(03),                  /* RGA Status/Store(RGASTATUS)*/~
                CH(08),                  /* RGA Serial No. (Barcode)   */~
                CH(08),                  /* Warranty Id from Sales Ord */~
                CH(18),                  /* Barcode assoc. with S. O.  */~
                CH(05),                  /* Complaint No. with S. O.   */~
                CH(04),                  /* RGA No. Assigned to Prod.  */~
                CH(02),                  /* RGA Line Item Number       */~
                CH(08),                  /* Sales Ord. Assoc. W/Prod.  */~		 
                CH(03),                  /* Line Item Assoc. W/S.O.    */~
                CH(25),                  /* RGA Part No. / MFG Part No.*/~
                CH(30),                  /* Part No. Description       */~			
	        CH(07),                  /* Compalint Code W/Complaint */~
                CH(03),                  /* Reason Code W/ Complaint   */~ 
                CH(06),                  /* Trailer/Location Assigned  */~
                CH(03),                  /* RGA Reason Code (RGAREASON)*/~
                CH(32),                  /* Additional Info. Assoc RGA */~			
                CH(01),                  /* Flagged for QTY Check Y/N  */~
                CH(06),                  /* RGA Date Checked by Quality*/~
                PD(14,4),                /* RGA Sales Value            */~
                PD(14,4),                /* RGA Salvage Total Value    */~
                PD(14,4),                /* RGA Salvage Material Value */~
                PD(14,4),                /* RGA Salvage Labor Value    */~
                PD(14,4),                /* RGA Salvage Overhead Value */~
                PD(14,4),                /* Credit Amt Assoc. W/S.O.   */~
                CH(06),                  /* Date Assoc. with Credit Amt*/~
                CH(06),                  /* Original Order Date of S.O.*/~
                CH(04),                  /* Time 1st Entere into System*/~
                CH(03),                  /* User Id for Initial Entry  */~
                CH(03),                  /* Production Dept Code       */~
                CH(02),                  /* RGA Last Seq used in Detail*/~
                CH(06),                  /* Date of Last Status Change */~
                CH(09),                  /* Sales Order Customer Code  */~
                CH(20),                  /* Sub Part No. Data  (PAR000)*/~
                CH(227)                  /* RGA Header Filler  (PAR000)*/

                                         /* File = (AWDRGADT)          */

L35050:                                                                  ~
            FMT CH(06),                  /* Date Associated W Status   */~
                CH(08),                  /* RGA Serial No. (Barcode)   */~
                CH(02),                  /* RGA Tracking Seq No.       */~
                CH(03),                  /* RGA Status/Store(RGASTATUS)*/~
                CH(03),                  /* RGA Reason Code (RGAREASON)*/~
                CH(03),                  /* User Id Assoc. W Status Chg*/~
                CH(04),                  /* Time Assoc. with Status    */~			
	        CH(06),                  /* Trailer / Location Detail  */~
                CH(32),                  /* Addional Information       */~ 
                CH(61)                   /* RGA Detail Filler          */

L35060:                                                                  ~
            FMT 2*CH(256)                /*RGA Header Record           */

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
                                                         
              on fieldnr% gosub L40160,     /* Serial Number - Ret Dte */~
                                L40160,     /* Warranty ID for Window  */~
                                L40160,     /* Prod. Barcode           */~
                                L40160,     /* Complaint Number        */~
                                L40160,     /* RGA Number Assigned     */~
                                L40160,     /* S.O. Line - Orig Ord Dte*/~
                                L40160,     /* RGA Status Code         */~
                                L40160,     /* RGA Reason Code         */~
                                L40160,     /* Mfg Part No. and Descr  */~
                                L40160,     /* Trailer/Location        */~
                                L40160,     /* MFG Department Code     */~
                                L40160,     /* Flag for Quality Chk DTE*/~
                                L40155,     /* Additional Comments     */~ 
                                L40165,     /* Dollar Price of Window  */~
                                L40165,     /* Salvage Mat-Lab-OvH-Tot */~
                                L40160,     /* RGA Complaint Reason    */~
                                L40160,     /* RGA Complaint Code (New)*/~
                                L40160      /* Credit Amt and Date     */

              goto L40190

L40155:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low    */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only  */
L40165:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric     */
                                                       
L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "**Serial No(assigned):",                     ~
               at (03,25), fac(lfac$(1%)), sc_serial$           , ch(08),~
               at (03,35), fac(lfac$(1%)), wandchar$            , ch(01),~
               at (03,40), fac(hex(84)), sc_return_dte$         , ch(10),~
                                                                         ~
               at (04,02), "**Warranty Id.       :",                     ~
               at (04,25), fac(lfac$(2%)), sc_warranty$         , ch(08),~
                                                                         ~  
               at (05,02), "**Production Barcode :",                     ~
               at (05,25), fac(lfac$(3%)), sc_barcode$          , ch(18),~
                                                                         ~
               at (06,02), "**Complaint Number   :",                     ~
               at (06,25), fac(lfac$(4%)), sc_complaint$        , ch(08),~
                                                                         ~
               at (07,02), "RGA No./Ln Assigned  :",                     ~
               at (07,25), fac(lfac$(5%)), sc_rga_no$           , ch(04),~
               at (07,35), fac(lfac$(5%)), sc_rga_ln$           , ch(02),~
                                                                         ~
               at (08,02), "Sales Order/Line Item:",                     ~
               at (08,25), fac(lfac$(6%)), sc_so_ord$           , ch(08),~
               at (08,35), fac(lfac$(6%)), sc_so_line$          , ch(03),~
               at (08,40), fac(lfac$(6%)), sc_orig_date$        , ch(10),~
               at (08,60), "Customer:",                                  ~
               at (08,70), fac(hex(84)), sc_cust$               , ch(09),~ 
                                                                         ~
               at (09,02), "RGA Status Code      :",                     ~
               at (09,25), fac(lfac$(7%)), sc_status$           , ch(03),~
               at (09,40), fac(hex(84)), sc_status_desc$        , ch(30),~ 
                                                                         ~
               at (10,02), "RGA Reason Code      :",                     ~
               at (10,25), fac(lfac$(8%)), sc_reason$           , ch(03),~
               at (10,40), fac(hex(84)), sc_reason_desc$        , ch(30),~ 
                                                                         ~
               at (11,02), "MFG Part Number:",                           ~
               at (11,20), fac(lfac$(9%)), sc_part$             , ch(25),~
               at (11,50), fac(hex(84)), sc_part_desc$          , ch(30),~ 
                                                                         ~
               at (12,02), "Trailer/Location     :",                     ~
               at (12,25), fac(lfac$(10%)), sc_trailer$         , ch(06),~
               at (12,40), fac(hex(84)),  sc_trailer_desc$      , ch(30),~
                                                                         ~
               at (13,02), "Production Department:",                     ~
               at (13,25), fac(lfac$(11%)), sc_dept$            , ch(03),~
               at (13,40), fac(hex(84)), sc_dept_desc$          , ch(30),~ 
                                                                         ~
               at (14,02), "Flag for Quality Chk :",                     ~
               at (14,25), fac(lfac$(12%)), sc_quality$         , ch(01),~
               at (14,40), fac(hex(84)), sc_quality_dte$        , ch(10),~ 
                                                                         ~
               at (15,02), "Comment              :",                     ~
               at (15,25), fac(lfac$(13%)), sc_comment$         , ch(30),~
                                                                         ~
               at (16,02), "Product Price        :",                     ~
               at (16,25), fac(lfac$(14%)), sc_price$           , ch(10),~
                                                                         ~
               at (17,02), "Product Salvage Value:",                     ~
               at (17,25), fac(lfac$(15%)), sc_sal_tot$         , ch(10),~
               at (17,37), fac(lfac$(15%)), sc_sal_mat$         , ch(10),~ 
               at (17,49), fac(lfac$(15%)), sc_sal_lab$         , ch(10),~ 
               at (17,61), fac(lfac$(15%)), sc_sal_ovh$         , ch(10),~ 
                                                                         ~
               at (18,02), "Complaint Code       :",                     ~
               at (18,25), fac(lfac$(16%)), sc_reas_cd$         , ch(03),~
               at (18,35), fac(hex(84)), sc_reas_desc$          , ch(30),~ 
                                                                         ~
               at (19,02), "New Complaint Code   :",                     ~
               at (19,25), fac(lfac$(17%)), sc_comp_cd$         , ch(07),~
               at (19,35), fac(hex(84)), sc_comp_desc$          , ch(30),~ 
                                                                         ~
               at (20,02), "Credit               :",                     ~
               at (20,25), fac(lfac$(18%)), sc_credit$          , ch(10),~
               at (20,40), fac(hex(84)), sc_credit_dte$         , ch(10),~ 
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

              
               if keyhit% <> 8% then goto L40200
                  gosub delete_serial_no      /* Delete a Serial Number */
                  return clear all
                  goto inputmode      

L40200:        if keyhit% <> 9% then goto L40220
                  tab% = 26%
                  gosub display_codes
                  goto L40190

L40220:        if keyhit% <> 11% then goto L40420
                  tab% = 27%
                  gosub display_codes
                  goto L40190

L40420:        if keyhit% <> 15% then goto L40430
                  call "PRNTSCRN"
                  goto L40190

L40430:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &        ~
                      "                       (11)RGA Reason  "
            pf$(2%) = "                 (8)Delete Ser. No.     " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                 (9)RGA Status          " &        ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffff0809ff0bffffff0f1000)

            if fieldnr% = 1% then L40570
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,8%,1%)  = hex(ff)
                str(pf$(3%),64%)     = " " : str(pfkeys$,16%,1%) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,8%,1%)  = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                 (8)Delete Ser. No.     " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &        ~
                      "                       (16)PRINT LABEL "
            pfkeys$ = hex(01ffffffffffff08ffffffffffff0f1000)

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
  
        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "

            on fieldnr% gosub L50010,        /* Lookup Serial Number  */~
                              L50050,        /* Lookup Warranty Id    */~
                              L50100,        /* Lookup Prod Barcode   */~
                              L50150,        /* Lookup Complaint No.  */~
                              L50250,        /* Lookup RGA Number     */~
                              L50200,        /* Lookup Sales Ord Line */~
                              L50300,        /* Lookup RGA Status Code*/~
                              L50350,        /* Lookup RGA Reason Code*/~
                              L50600,        /* MFG Part Number       */~
                              L50400,        /* Lookup RGA Trailer/Loc*/~
                              L50450,        /* Lookup Prod Dept Code */~
                              L50500,        /* Lookup Chk Quality Flg*/~
                              L50550,        /* Additional Comments   */~
                              L50650,        /* Sales Order Price     */~
                              L50700,        /* Lookup Salvage Total  */~
                              L50800,        /* Comp Reason Code Old  */~
                              L50750,        /* Comp Code (New)       */~
                              L50850         /* Credit Amount         */

            return
                                             /* (AWDRGAHD)            */
L50010: Rem Lookup Serial Number                sc_serial$
            assign_flag% = 0%                /* When Done Assign Serial Number */

        
            gosub read_convert_header

               if rga_flag% = 0% then goto L50030
                                             /* Record on File            */
               assign_flag% = 99%            /* Do Not Assign Serial No.  */
               fieldnr% = 18%
        return
L50030:
            init (" ") sc_serial$, rga_key0$
        return 

L50050: Rem lookup Warranty ID              sc_warranty$
                                           /* 1st Check RGA Database   */
            if len(sc_warranty$) < 5 then return        /* No Data Entry */

            init(" ") rga_key2$, rga_rec$()
            rga_key2$ = sc_warranty$         /* Set Alt Key (2) Dups  */ 

            read #1,key 2% = rga_key2$, using L35060, rga_rec$(), eod goto L50070
                                             /* Check to see if found    */
                                          
            if str(rga_rec$(),18%,8%) = sc_warranty$ then goto L50080
                                        
L50070:      
                    
            gosub lookup_warranty_data       /* Check Warranty Database  */
            init(" ") sc_serial$, rga_serial$
        return
                                             /* Found in RGA Database    */
L50080:
        sc_serial$ = str(rga_rec$(),10%,8%)    /* Set the Serial Number    */
        goto L50010                          /* and Covert Data          */
 
L50100: Rem lookup Production Barcode        sc_barcode$
                                             /* 1st Check RGA Database   */
            if len(sc_barcode$) < 5 then return     /* No Data Entry     */
  
            init(" ") rga_key3$, rga_rec$()
                                             /* Leave off last Eight digits*/      
            rga_key3$ = str(sc_barcode$,1%,8%) /* Set Alt Key (3) Dups   */
            read #1,key 3% > rga_key3$, using L35060, rga_rec$(), eod goto L50120
            goto L50115
L50110:        read #1, using L35060, rga_rec$(), eod goto L50120

L50115:        rga_key3$ = str(rga_rec$(),26%,18%)
                                             /* Check for greater        */
               if str(rga_rec$(),26%,18%) > sc_barcode$ then goto L50120
                                             /* Check to see if found    */
               if str(rga_rec$(),26%,18%) = sc_barcode$ then goto L50140
            goto L50110                      /* Check next one           */
L50120:      
            gosub lookup_production_barcode  /* Check Prod. Barcode Data */
            init(" ") sc_serial$, rga_serial$ 
        return
L50140:
            sc_serial$ = str(rga_rec$(),10%,8%)/* Set the Serial Number    */
            goto L50010                      /* and Convert Data         */

L50150: Rem lookup Complaint Number          sc_complaint$
                                             /* 1st Check RGA Database    */
            init(" ") rga_key4$, rga_rec$(), wrk_rec$()
            if len(sc_complaint$) < 5 then return    /* No Data Entry     */
            gosub pack_comp_number           /* Pack Complaint Number     */     
                                             /* Leave off last digit      */      
            rga_key4$ = str(sc_comp$,1%,5%)  /* Set Alt Key (4) Dups      */
            read #1,key 4% = rga_key4$, using L35060, rga_rec$(), eod goto L50160
                                             /* Check to see if found    */
               if str(rga_rec$(),44%,5%) = str(sc_comp$,1%,5%) then goto L50170
L50160:                                      /* Lookup Complaint Data    */
            gosub lookup_complaint_number  

        return
L50170:
            sc_serial$ = str(rga_rec$(),10%,8%)/* Set the Serial Number    */
            goto L50010                      /* and Convert Data         */

 
L50250: Rem lookup RGA Number                sc_rga_no$
                                             /* 1st Check RGA Database    */
            init(" ") rga_key5$, rga_rec$()
            if len(sc_rga_no$) < 3 then return     /* No Data Entry       */
                                             /* Set Alt Key (5)           */
            rga_key5$ = sc_rga_no$            
            read #1,key 5% > rga_key5$, using L35060, rga_rec$(), eod goto L50270
                                             /* Check to see if found    */
               if str(rga_rec$(),49%,4%) <> sc_rga_no$ then goto L50270
                                             /* May be the one ??? Dups  */
               if str(rga_rec$(),53%,2%) = sc_rga_ln$   then goto L50280
L50270:                                      /* No Data On File          */
               gosub lookup_rga_number
        return
L50280:
            sc_serial$ = str(rga_rec$(),10%,8%)/* Set the Serial Number    */
            goto L50010                      /* and Convert Data         */


L50200: Rem lookup Sales Order Line Item     sc_so_ord$, sc_so_line$
                                             /* 1st Check RGA Database    */
            gosub convert_sales_order

            if sc_so_ord% = 0% then return   /* No Data Entered           */
 
            init(" ") rga_key6$, rga_rec$()
                                             /* Leave off line Item       */      
            str(rga_key6$,1%,8%) = sc_so_ord$/* Set Alt Key (6)           */ 
            read #1,key 6% > rga_key6$, using L35060, rga_rec$(), eod goto L50240
            goto L50235
L50230:     read #1, using L35060, rga_rec$(), eod goto L50240
                                       
L50235:        rga_key6$ = str(rga_rec$(),55%,11%)
                                             /* Check for greater S.O.   */
               if str(rga_rec$(),55%,8%) > sc_so_ord$ then goto L50240
               if str(rga_rec$(),63%,3%) > sc_so_line$ then goto L50240 

                                             /* Check to see if found    */
               if str(rga_rec$(),55%,8%) = sc_so_ord$ and                    ~
                  str(rga_rec$(),63%,3%) = sc_so_line$ then goto L50245
            goto L50230                      /* Check next one           */

L50240:                                
            gosub lookup_sales_order         /* Lookup Sales Order Data  */           
 
        return
L50245:
            sc_serial$ = str(rga_rec$(),10%,8%) /* Set the Serial Number    */
            gosub L50010                     /* and Convert Data         */
            assign_flag% = 0%
            init(" ") sc_serial$, rga_serial$
            fieldnr% = 6%
 
        return

L50300: REM RGA Status Code                 sc_status$
            if assign_flag% = 99% then goto L50305
               if sc_status$ <> "002" and sc_status$ <> "004" and ~
                  sc_status$ <> "006" then goto L50310
L50305:
            gosub lookup_rga_status
            if check% = 0% then goto L50310
            rga_status$ = sc_status$

        return
L50310:     errormsg$ = "(Error) Invalid RGA Status Code? (002,004, or 006)"
            gosub error_prompt
            init (" ") sc_status$, rga_status$
        return

L50350: REM RGA Reason Code                 sc_reason$
            gosub lookup_rga_reason
            if check% = 0% then goto L50360
            rga_reason_cd$ = sc_reason$
 
        return
L50360:     errormsg$ = "(Error) Invalid RGA Reason Code? (Required Entry)"
            gosub error_prompt
            init(" ") sc_reason$, rga_reason_cd$
        return
 
L50600: REM MFG Part Number                 sc_part$
            if len(sc_part$) < 4 then goto L50610
               gosub find_depart 
               rga_dept$ = sc_dept$ 
               rga_part$ = sc_part$
                                            /* (PAR000)            */
               rga_sub_part$ = sc_sub_part$
                                            /* (PAR000)            */ 
               rga_descr$ = sc_part_desc$
        return
                                            /* (PAR000)            */    
L50610:     init(" ") sc_part$, sc_part_desc$, sc_sub_part$
        return

L50400: REM RGA Trailer Location            sc_trailer$, sc_trailer_desc$
                                            /* Lookup Trailer Code */
            if assign_flag% <> 99% then sc_trailer$ = "T00000"

            gosub lookup_trailer_loc
            if check% = 0% then goto L50510
            rga_trailer_loc$ = sc_trailer$

        return
L50510:     errormsg$ = "(ERROR) Invalid Trailer Code?"
            gosub error_prompt
            init(" ") sc_trailer$, rga_trailer_loc$, sc_trailer_desc$
        return

L50450: REM Production Dept Code            sc_dept$
                                        /* Check fr Dept Entry */
            if len(sc_dept$) < 2 then goto L50470
            gosub lookup_dept
            if check% = 0% then goto L50460
            rga_dept$ = sc_dept$

            gosub lookup_salvage

        return
L50460:     errormsg$ = "(ERROR) Invalid Department Code?"
            gosub error_prompt
L50470:
            init(" ") sc_dept$, rga_dept$
        return

L50500: REM Check for Quality Flag          sc_quality$

            if assign_flag% <> 99% then sc_quality$ = "N"

            if sc_quality$ <> "Y" and sc_quality$ <> "N" then      ~
                                            sc_quality$ = "N"
               rga_qty_chk$ = sc_quality$            
        return

L50550: REM Additional Comments             sc_comments$
            rga_comment$ = sc_comment$
        return

L50650: REM Price From Sales Order          sc_price$
                                            /* Lookup Window Price   */
        if len(sc_price$) < 8 then gosub lookup_sales_order
        return

L50700: REM Salvage Total                   sc_sal_tot$
                                            /* Lookup Salvage Total  */
            if len(sc_sal_tot$) < 8 then gosub lookup_salvage
 


        return

L50750: REM Complaint code from Complaint   sc_comp_cd$
           


        return


L50800: REM Reason Code from Complaint      sc_reas_cd $sc_reas_desc$
            if len(sc_reas_cd$) < 2 then goto L50820 
            gosub lookup_comp_code
            if check% = 0% then goto L50810
               rga_comp_reas$ = sc_reas_cd$

        return
L50810:     errormsg$ = "(ERROR) Invalid Complaint reason code Code?"
            gosub error_prompt
L50820:
            init(" ") sc_reas_cd$, sc_reas_desc$, rga_comp_reas$
        return

L50850: REM Credit Amount                   sc_credit$
                                            /* Lookup Credit Amt   */
            sc_credit = 0.0 
            if Len(sc_credit$) < 5 then gosub lookup_credit

                                            /* Credit Value        */ 
            convert sc_credit$ to sc_credit, data goto L50860
L50860:
            convert sc_credit to sc_credit$, pic(####.####-)

            rga_credit_amt = sc_credit
                                            /* Credit Date        */
            if len (sc_credit_dte$) < 5 then return 
                                            /* No Date            */
               date% = 0%
               call "DATEOKC" (sc_credit_dte$, date%, errormsg$)
               if errormsg$ <> " " then goto L50870
               rga_credit_dte$ = sc_credit_dte$
               call "DATUFMTC" (rga_credit_dte$)

        return
L50870:        errormsg$ = "(Error) Invalid RGA Credit Date?"
               init(" ") sc_credit_dte$, rga_credit_dte$
               gosub error_prompt
        return 
 
        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        read_convert_header

            sc_serial% = 0%
            if len(sc_serial$) < 1 then return

            convert sc_serial$ to sc_serial%, data goto L50900
L50900:
            convert sc_serial% to sc_serial$, pic(00000000)

            rga_flag% = 0%
            init(" ") rga_key0$, wrk_key$, sc_comp$, rga_flag$
            rga_key0$ = sc_serial$

            read #1,key 0% = rga_key0$, eod goto read_convert_header_done

            gosub load_header                       /* Get Header Data  */

            str(wrk_key$,1%,8%) = rga_serial$
            str(wrk_key$,9%,2%) = "00"              /* 1st Entry        */

            read #2,key 0% = str(wrk_key$,1%,10%), eod goto read_convert_header_Done

            gosub load_detail

            sc_serial$     = rga_serial$            /* RGA Serial Number*/

            sc_return_dte$ = rga_return_dte$        /* RGA Entry Date   */ 
            call "DATFMTC" (sc_return_dte$)

            sc_warranty$   = rga_warranty_id$       /* Prod Warranty ID */ 

            sc_barcode$    = rga_so_barcode$        /* Production Barcode*/ 

            str(sc_comp$,1%,5%) = rga_comp_no$      /* I.S. Complaint No.*/ 
            gosub unpack_comp_number

            sc_rga_no$     = rga_number$            /* RGA Number       */
            sc_rga_ln$     = rga_number_ln$         /* Rga Line item    */ 

            sc_so_ord$     = rga_sales_ord$         /* Sales Order      */
            sc_so_line$    = rga_sales_ln$          /* Sales Line Item bbb*/
            sc_cust$       = rga_cust$

            sc_orig_date$  = rga_orig_date$         /* Original S.O. Date*/ 
            call "DATFMTC" (sc_orig_date$)

            sc_status$     = rga_status$            
            gosub lookup_rga_status

            sc_reason$     = rga_reason_cd$
            gosub lookup_rga_reason

            sc_trailer$    = rga_trailer_loc$

            sc_dept$       = rga_dept$
            gosub lookup_dept

            sc_quality$    = rga_qty_chk$

            str(sc_quality_dte$,1%,6%) = str(rga_qty_dte$,1%,6%)
            call "DATFMTC" (sc_quality_dte$)

            sc_comment$     = rga_comment$

            sc_part$        = rga_part$
            sc_part_desc$   = rga_descr$
                                                   /* (PAR000)             */
            sc_sub_part$    = rga_sub_part$
                                                   /* (PAR000)             */       
            convert rga_value to sc_price$, pic(####.####-)

            convert rga_sal_value to sc_sal_tot$, pic(####.####-)

            convert rga_sal_mat to sc_sal_mat$, pic(####.####-)

            convert rga_sal_labor to sc_sal_lab$, pic(####.####-)

            convert rga_sal_over to sc_sal_ovh$, pic(####.####-)

            sc_comp_cd$      = rga_comp_code$     /* Complaint Code from Complaint */
        REM    gosub lookup_comp_code             /* (7) Characters                */


            sc_reas_cd$      = rga_comp_reas$      /* Reason Code from Complaint   */
            gosub lookup_comp_code                 /* (3) Characters               */ 

            convert rga_credit_amt to sc_credit$, pic(####.####-)

            sc_credit_dte$   = rga_credit_dte$
            call "DATFMTC" (sc_credit_dte$)
     
            rga_flag% = 1%
        read_convert_header_done
            convert rga_flag% to rga_flag$, pic(#)

        return

        find_depart
            init(" ") wrk_key$, wrk_rec$(), sc_support$, sc_dept$

            str(wrk_key$,1%,3%) = str(sc_part$,1%,3%)
        find_depart_next
            read #3,key 3% > str(wrk_key$,1%,15%), using L60000, str(wrk_rec$(),1%,32%),~
                                                    eod goto find_depart_done
L60000:          FMT CH(32)
            if str(wrk_rec$(),1%,3%) <> str(sc_part$,1%,3%) then goto find_depart_done
               str(wrk_key$,1%,15%) = str(wrk_rec$(),1%,15%)
               sc_support$ = str(wrk_rec$(),11%,3%)         /* Set Department */   
               gosub lookup_support_dept
                                                         /* Found a Support*/
               if check% = 1% then goto find_depart_next /* Department     */

                  sc_dept$ =sc_support$                  /* Production Dept*/
                                                         /* Found          */


        find_depart_done

        return

        lookup_salvage
            check% = 0%  
            init(" ") readkey$, sc_salvage_desc$, sc_sal_mat$, sc_sal_lab$,~
                      sc_sal_ovh$, sc_sal_tot$

            str(readkey$,1%,9%)  = "RGASALVAG"
            str(readkey$,10%,3%) = sc_dept$ 
            read #5,key = readkey$, using L6005, sc_salvage_desc$, eod goto L60010
L6005:          FMT POS(25), CH(30)
            check% = 1%

            sc_sal_mat$ = str(sc_salvage_desc$,1%,7%)     /* Material    */
            sc_sal_lab$ = str(sc_salvage_desc$,8%,7%)     /* Labor       */
            sc_sal_ovh$ = str(sc_salvage_desc$,15%,7%)    /* Overhead    */
            sc_sal_tot$ = str(sc_salvage_desc$,22%,7%)    /* Total       */
L60010:
            sc_sal_mat, sc_sal_lab, sc_sal_ovh, sc_sal_tot = 0.0

            convert sc_sal_mat$ to sc_sal_mat, data goto L60020
L60020:
            convert sc_sal_lab$ to sc_sal_lab, data goto L60030
L60030:          
            convert sc_sal_ovh$ to sc_sal_ovh, data goto L60040
L60040:
            convert sc_sal_tot$ to sc_sal_tot, data goto L60050
L60050:
            convert sc_sal_mat to sc_sal_mat$, pic(####.####-)

            convert sc_sal_lab to sc_sal_lab$, pic(####.####-)

            convert sc_sal_ovh to sc_sal_ovh$, pic(####.####-)

            convert sc_sal_tot to sc_sal_tot$, pic(####.####-)
 
            rga_sal_value = sc_sal_tot
            rga_sal_mat   = sc_sal_mat
            rga_sal_labor = sc_sal_lab
            rga_sal_over  = sc_sal_ovh

        return

        lookup_warranty_data

                                                   /* 1st Check (APCPLNDT) */
            init(" ")wrk_key$, wrk_rec$(), sc_so_ord$, sc_so_line$

            str(wrk_key$,1%,7%) = str(sc_warranty$,1%,7%)
                                             /* Leave off last digit      */      
            read #4,key 4% > str(wrk_key$,1%,8%), using L60110,            ~
                                                  wrk_rec$(), eod goto L60140
L60110:        FMT CH(256)
               goto L60130
                                             /* Read Next for Dups       */ 
L60120:       read #4, using L60110, wrk_rec$(), eod goto L60140
L60130:
               str(wrk_key$,1%,8%) = str(wrk_rec$(),96%,8%)
                                             /* Check for greater        */
               if str(wrk_rec$(),96%,8%) > sc_warranty$ then goto L60140
                                             /* Check to see if found    */
               if str(wrk_rec$(),96%,8%) = sc_warranty$ then goto L60190
            goto L60120                      /* Check next one           */
L60140:                                      /* Lookup in (APCPLNWT)     */
                                             /* (PAR001)                 */
            init(" ") wrk_key$, wrk_rec$()
            str(wrk_key$,1%,8%) = sc_warranty$
            read #6, key 0% = str(wrk_key$,1%,8%), using L60125,          ~
                                   str(wrk_rec$(),1%,128%), eod goto L60185
L60125:        FMT CH(128)
            sc_so_ord$  = str(wrk_rec$(),9%,8%)  /* Sales Order          */
            sc_so_line$ = str(wrk_rec$(),17%,2%) /* S.O. Line Item       */

                                             /* (PAR001)                 */
            gosub lookup_sales_order
           
L60185:                                      /* No data Found            */
        return
                                             /* Found in (APCPLNDT)      */
L60190:

        sc_warranty$ = str(wrk_rec$(),96%,8%)  /* Warranty Id            */
        sc_barcode$  = str(wrk_rec$(),24%,18%) /* Prod Barcode           */
        sc_so_ord$   = str(sc_barcode$,1%,8%)/* Sales Order              */
        sc_so_line$  = str(sc_barcode$,9%,2%)/* S.O. Line Item           */
        sc_dept$     = str(wrk_rec$(),42%,3%)  /* Department Code        */ 
        sc_part$     = str(wrk_rec$(),189%,25%)/* Part Number            */
        sc_price = 0.0                       /* Product Price            */
                                             /* (PAR000)                 */

        sub_flg$ = "1"
        so_inv$  = sc_so_ord$
        item_no$ = sc_so_line$
        gosub lookup_sub_part
        sc_sub_part$ = str(bcksubpt_rec$,48%,20%)
                                             /* (PAR000)                 */
  
        get str(wrk_rec$(),133%,8%), using L60196, sc_price
L60196:     FMT PD(14,4)
        convert sc_price to sc_price$, pic(####.####-)

        rga_dept$        = sc_dept$
        rga_warranty_id$ = sc_warranty$
        rga_so_barcode$  = sc_barcode$
        rga_sales_ord$   = sc_so_ord$
        rga_so_line$     = "0" & sc_so_line$
        rga_part$        = sc_part$
                                          
        rga_sub_part$    = sc_sub_part$      /* (PAR000)                 */
 
        rga_value        = sc_price
        rga_cust$        = str(wrk_rec$(),124%,9%) 

        gosub lookup_salvage

        gosub lookup_sales_order


        return                               /* Finished                 */

        lookup_sales_order                   /* Check (BCKLINES) Not Hist*/
                                             /* Part Number and Descr    */
                                             /* Price and Customer Code  */ 
            init(" ") wrk_key$, wrk_rec$(), sc_cust$, sc_price$
            gosub convert_sales_order
 
            str(wrk_key$,1%,16%) = sc_so_ord$
            str(wrk_key$,17%,3%) = sc_so_line$
            read #9, key = str(wrk_key$,1%,19%), using L60110, wrk_rec$(),     ~
                                                 eod goto L60220
            sc_part$      = str(wrk_rec$(),32%,25%) /* Part Number           */
            sc_part_desc$ = str(wrk_rec$(),57%,30%) /* Part Description      */
            sc_price = 0.0 
            get str(wrk_rec$(),141%,8%), using L60196, sc_price
            convert sc_price to sc_price$, pic(####.####-)

                                                  /* (PAR000)              */
            sub_flg$ = "2"
            so_inv$  = sc_so_ord$
            item_no$ = sc_so_line$
            gosub lookup_sub_part
            sc_sub_part$ = str(bcksubpt_rec$,48%,20%)
            rga_sub_part$= sc_sub_part$
                                                  /* (PAR000)              */
  
            rga_part$      = sc_part$
            rga_value      = sc_price
            rga_sales_ord$ = sc_so_ord$
            convert sc_so_line% to rga_sales_ln$, pic(000)

            sc_cust$ = str(wrk_rec$(),1%,9%)      /* 2nd Get Customer Code */
            rga_cust$= sc_cust$

                                                  /* Check (BCKMASTR)      */
                                                  /* for Original Order Dte*/
            init(" ") wrk_key$, wrk_rec$(), sc_orig_date$
            str(wrk_key$,1%,9%)   = sc_cust$
            str(wrk_key$,10%,16%) = sc_so_ord$
            read #8,key = str(wrk_key$,1%,25%), using L60215,               ~
                                    str(sc_orig_date$,1%,6%), eod goto L60220
L60215:        FMT POS(806), CH(6)
            rga_orig_date$ = sc_orig_date$
            call "DATFMTC" (sc_orig_date$)

            gosub lookup_complaint
L60220:                                           /* Not on File           */
                                                  /* No Data Found         */
                                                  /* May be in History     */ 
        return 
                                                  /* (RHHTEST)             */
        lookup_complaint
            init(" ") wrk_rec$, wrk_rec$, sc_comp$
            str(wrk_key$,1%,8%)   = rga_sales_ord$
            str(wrk_key$,9%,2%)   = str(rga_sales_ln$,2%,2%)
            read #7,key 1% = str(wrk_key$,1%,10%), using L60225,             ~
                             str(sc_comp$,1%,5%), eod goto L60230
L60225:        FMT CH(05)
            gosub unpack_comp_number
  
            gosub lookup_complaint_number
L60230:
        return
                                                  /* (RHHTEST)             */    

        lookup_production_barcode
                                             /* 1st Check (APCPLNDT)      */
            init(" ")wrk_key$, wrk_rec$()      /* Special Check Department  */

            str(wrk_key$,1%,17%) = str(sc_barcode$,1%,17%)
                                             /* Leave off last digit      */      
L60300:                                      /* Set Alt Key (0)           */
            read #4,key 0 > str(wrk_key$,1%,23%), using L60110, wrk_rec$(),  ~
                                                           eod goto L60320
               str(wrk_key$,1%,23%) = str(wrk_rec$(),24%,23%)
                                             /* Check for greater        */
               if str(wrk_rec$(),24%,18%) > sc_barcode$ then goto L60320
                                             /* Check to see if found    */
               if str(wrk_rec$(),24%,18%) = sc_barcode$ then goto L60390
            goto L60300                      /* Check next one           */
L60320:                                      /* 2nd Check (BCKLINES)     */

            sc_so_ord$   = str(sc_barcode$,1%,8%)  /* Sales Order        */
            sc_so_line$  = str(sc_barcode$,9%,2%) /* S.O. Line Item     */
            init(" ") sc_barcode$            /* Can't Use Barcode        */

            gosub lookup_sales_order
           
                                             /* No data Found            */
        return
                                             /* Found        (APCPLNDT)  */
L60390:                                      /* Check Production Depart  */

        sc_support$  = str(wrk_rec$(),42%,3%)  /* Department Code        */ 
        gosub lookup_support_dept
        if check% = 1% then goto L60300      /* Support Department Found */ 
                                             /* Check another Detail Rec */
        sc_dept$     = str(wrk_rec$(),42%,3%) 
        sc_warranty$ = str(wrk_rec$(),96%,8%)  /* Warranty Id            */
        sc_barcode$  = str(wrk_rec$(),24%,18%) /* Prod Barcode           */
        sc_so_ord$   = str(sc_barcode$,1%,8%)/* Sales Order              */
        sc_so_line$  = str(sc_barcode$,9%,2%)/* S.O. Line Item           */
        sc_part$     = str(wrk_rec$(),189%,25%)/* Part Number            */
        sc_price = 0.0                       /* Product Price            */
        get str(wrk_rec$(),133%,8%), using L60396, sc_price
L60396:     FMT PD(14,4)
        convert sc_price to sc_price$, pic(####.####-)
                                             /* (PAR000)                 */
        sub_flg$ = "3" 
        so_inv$  = sc_so_ord$
        item_no$ = sc_so_line$
        gosub lookup_sub_part
        sc_sub_part$  = str(bcksubpt_rec$,48%,20%)
        rga_sub_part$ = sc_sub_part$
                                             /* (PAR000)                 */ 
        rga_dept$        = sc_dept$
        rga_warranty_id$ = sc_warranty$
        rga_so_barcode$  = sc_barcode$
        rga_sales_ord$   = sc_so_ord$
        rga_so_line$     = "0" & sc_so_line$
        rga_part$        = sc_part$
        rga_value        = sc_price

        gosub lookup_salvage
        gosub lookup_sales_order
           
        return                               /* Finished                 */

        lookup_complaint_number              /* 1st Check (APCCOMPT)     */

            init(" ") wrk_key$, wrk_rec$()
            gosub pack_comp_number           /* Pack Complaint Number    */
            str(wrk_key$,1%,5%) = str(sc_comp$,1%,5%)
            read #7,key 0% = str(wrk_key$,1%,5%), using L60400,           ~
                                      str(wrk_rec$(),1%,150%), eod goto L60410
L60400:        FMT CH(256)                   /* (PAR000)                 */
            str(sc_comp$,1%,5%) = str(wrk_rec$(),1%,5%)  /* Complaint Number */
            gosub unpack_comp_number

            sc_so_ord$    = str(wrk_rec$(),6%,8%)
            sc_so_line$   = str(wrk_rec$(),14%,2%)
            sc_rga_no$    = str(wrk_rec$(),32%,4%)
            sc_rga_ln$    = str(wrk_rec$(),36%,2%)
            sc_part$      = str(wrk_rec$(),53%,25%)
                                                        /* (PAR000)         */
            sc_sub_part$  = str(wrk_rec$(),144%,20%)
 
            sc_reas_cd$   = str(wrk_rec$(),82%,3%)      /* Complaint Code   */
            str(sc_orig_date$,1%,6%) = str(wrk_rec$(),111%,6%)  
                                                      /* Special Code     */
            str(sc_comp_cd$,1%,1%) = str(wrk_rec$(),144%,1%)
            str(sc_comp_cd$,2%,1%) = str(wrk_rec$(),145%,1%)
            str(sc_comp_cd$,3%,1%) = str(wrk_rec$(),146%,1%)
            str(sc_comp_cd$,4%,1%) = str(wrk_rec$(),147%,1%)
            str(sc_comp_cd$,5%,1%) = str(wrk_rec$(),148%,1%)
            str(sc_comp_cd$,6%,1%) = str(wrk_rec$(),149%,1%)
            str(sc_comp_cd$,7%,1%) = str(wrk_rec$(),150%,1%)

            rga_comp_no$   = str(wrk_rec$(),1%,5%)      /* Formatted        */
            rga_sales_ord$ = sc_so_ord$
            rga_sales_ln$  = sc_so_line$
            rga_number$    = sc_rga_no$
            rga_number_ln$ = sc_rga_ln$
            rga_part$      = sc_part$
                                                        /* (PAR000)         */
            rga_sub_part$  = sc_sub_part$

            str(rga_orig_date$,1%,6%) = str(sc_orig_date$,1%,6)
            call "DATFMTC" (sc_orig_date$)
            rga_comp_reas$            = sc_reas_cd$
            rga_comp_code$            = sc_comp_cd$
            rga_cust$      = str(wrk_rec$(),44%,9%)

            gosub lookup_comp_code                /* Complaint Reason Descr */

        return 
L60410:                                                 /* No Data Found  */ 

        return                

        pack_comp_number
            init(" ") sc_comp$
            sc_comp% = 0%
            sc_comp$ = sc_complaint$                   
            convert sc_comp$ to sc_comp%, data goto PACK_1
PACK_1:
            init(" ") sc_comp$
            put str(sc_comp$,1%,4%), using PACK_2, sc_comp%
            str(sc_comp$,5%,1%) = " "
        return

        unpack_comp_number
            sc_complaint% = 0%
            get str(sc_comp$,1%,4%), using PACK_2, sc_complaint%
PACK_2:             FMT BI(4)
            convert sc_complaint% to sc_complaint$, pic(00000000)

        return 

        lookup_rga_status                          /* RGA Status Code      */
            check% = 0%  
            init(" ") readkey$, sc_status_desc$
            str(readkey$,1%,9%)  = "RGASTATUS"
            str(readkey$,10%,3%) = sc_status$ 
            read #5,key = readkey$, using L60420, sc_status_desc$, eod goto L60430
L60420:        FMT POS(25), CH(30)
            check% = 1%
            rga_status$ = sc_status$
        return
L60430:
        return

        lookup_rga_number
            init(" ") wrk_key$, wrk_rec$()
            if len(sc_rga_no$) < 3 then goto L60510
                                                /* 1st (APCRGAHD)      */  
            str(wrk_key$,1%,4%) = sc_rga_no$
        read #12,key 0% = str(wrk_key$,1%,4%), using L60500, str(wrk_rec$(),1%,80%), ~
                                                  eod goto L60510
L60500:     FMT CH(80)
                                                /* 2nd (APCRGADT)      */
        init(" ") wrk_key$
        str(wrk_key$,1%,4%) = sc_rga_no$
        str(wrk_key$,5%,2%) = sc_rga_ln$
        init(" ") wrk_rec$()
        read #14,key 0% = str(wrk_key$,1%,6%), using L60110, wrk_rec$(), ~
                                                eod goto L60510
           sc_so_ord$    = str(wrk_rec$(),54%,8%)
           sc_so_line$   = str(wrk_rec$(),62%,2%) 
           init(" ") sc_comp$
           if len(sc_complaint$) < 3 then str(sc_comp$,1%,5%) = str(wrk_rec$(),18%,5%)
           rga_comp_no$ = str(sc_comp$,1%,5%)
           if len(sc_complaint$) < 3 then gosub unpack_comp_Number

           sc_part$       = str(wrk_rec$(),29%,25%)
           rga_number$    = sc_rga_no$
           rga_number_ln$ = sc_rga_ln$
           rga_part$      = sc_part$
           rga_sales_ord$ = sc_so_ord$
           rga_sales_ln$  = sc_so_line$          
           gosub lookup_sales_order
 L60510:
        return

        lookup_credit
           call "SHOSTAT" ("Looking up Credit for -- " & sc_so_ord$ )

           init(" ") wrk_key$, ar_rec$(), sc_credit$, sc_credit_dte$
           str(wrk_key$,1%,8%) = str(sc_so_ord$,1%,7%)
           read #10,key 3% > str(wrk_key$,1%,8%), using L60530, ar_rec$(), ~
                                                            eod goto L60570
L60530:       FMT CH(2000)

           goto L60550
L60540:
           read #10, using L60530, ar_rec$(), eod goto L60570

L60550:       str(wrk_key$,1%,8%) = str(ar_rec$(),34%,8%)

              if str(ar_rec$(),34%,8%) > sc_so_ord$ then goto L60570
              if sc_so_ord$ = str(ar_rec$(),34%,8%) then goto L60560
           goto L60540
L60560:                                            /* Credit found       */      

           if str(ar_rec$(),891%,1%) <> "C" then goto L60540
                                                   /* Check RGA Reason Code*/
           if str(ar_rec$(),892%,2%) <> "02" and                          ~
              str(ar_rec$(),892%,2%) <> "10" Then goto L60540 

           get str(ar_rec$(),809%,8%), using L60396, sc_credit
           convert sc_credit to sc_credit$, pic(####.####-)

           rga_credit_amt  = sc_credit
           rga_credit_amt$ = sc_credit$

           sc_credit_dte$  = str(ar_rec$(),553%,6%)
           rga_credit_dte$ = sc_credit_dte$
           call "DATFMTC" (sc_credit_dte$)

        return
L60570:

        return

        lookup_rga_reason                          /* RGA Reason Code      */ 
            check% = 0%
            init(" ") readkey$, sc_reason_desc$
            str(readkey$,1%,9%)  = "RGAREASON"
            str(readkey$,10%,3%) = sc_reason$ 
            read #5,key = readkey$, using L60420, sc_reason_desc$, eod goto L60430
            check% = 1%
            rga_reason_cd$ = sc_reason$
        return

        lookup_dept                               /* Prod Dept Code        */ 
            check% = 0%
            init(" ") readkey$, sc_dept_desc$
            str(readkey$,1%,9%)  = "PLAN DEPT"
            str(readkey$,10%,3%) = sc_dept$ 
            read #5,key = readkey$, using L60420, sc_dept_desc$, eod goto L60430
            check% = 1%
            rga_dept$ = sc_dept$
        return

        lookup_comp_code                         /* Complaint Code For Complaint */ 
            check% = 0%
            init(" ") readkey$, sc_reas_desc$
            str(readkey$,1%,9%)  = "COMPLAINT"
            str(readkey$,10%,3%) = sc_reas_cd$ 
            read #5,key = readkey$, using L60420, sc_reas_desc$, eod goto L60430
            check% = 1%
            rga_comp_reas$ = sc_reas_cd$
        return

        lookup_trailer_loc                       /* Reason Code for Complaint    */ 
            check% = 0%
            init(" ") readkey$, sc_trailer_desc$
            str(readkey$,1%,9%)  = "RGATR-LOC"
            str(readkey$,10%,6%) = sc_trailer$ 
            read #5,key = readkey$, using L60420, sc_trailer_desc$, eod goto L60430
            check% = 1%
            if check% = 1% then rga_trailer_loc$ = sc_trailer$
            if sc_trailer$ = "T00000" then sc_trailer_desc$ = "Trailer Not Assigned"

        return

        lookup_support_dept                     /* Check for Support Dept       */ 

            check% = 0%
            init(" ") readkey$
            str(readkey$,1%,9%)  = "PLAN SUPP"
            str(readkey$,10%,3%) = sc_support$ 
            read #5,key = readkey$, eod goto L60430
                                                /* Found Support Department     */ 
            check% = 1%
      
        return

        assign_serial_no
            if assign_flag% = 99% then return           /* Do Not Assign Number */

            init(" ") readkey$, rga_serial_desc$
            str(readkey$,1%,9%)  = "RGASTATUS"
            str(readkey$,10%,15%) = "---"  
            read #5,hold,key = readkey$, using ASSIGN_1, rga_serial_desc$, eod goto ASSIGN_2
ASSIGN_1:        FMT CH(128)

            delete #5
                                      /* Table Value Plus 1           */
            rga_serial$ = str(rga_serial_desc$, 26%, 8%)
            convert rga_serial$ to rga_serial%, data goto ASSIGN_2

            rga_serial% = rga_serial% + 1%

            convert rga_serial% to rga_serial$, pic(00000000)

            str(rga_serial_desc$,25%,8%) = rga_serial$
                                      /* Save Last Serial Number Used */
            write #5, using ASSIGN_1, rga_serial_desc$, eod goto ASSIGN_2

            sc_serial$ = rga_serial$

        return
ASSIGN_2:
            errormsg$ = "(ERROR)-Unable to Assign Serial No. " 
            gosub error_prompt
        return clear all
        goto inputmode

        create_rga_label

            gosub assign_serial_no

            been_here% = 0%

            call "SHOSTAT" ("Printing Serial No. " & rga_serial$)
            CALL "PAUSE" ADDR(100%)

            gosub write_header
                                                /* (AWD001) 05/18/05        */
            call "AWDRGB01" (been_here%, rga_rec$(), #5, err%)
            if err% <> 0% then gosub print_error

            err% = 99%                          /* Clear Work File, Scratch */
            call "AWDRGB01" (been_here%, rga_rec$(), #5, err%)
            if err% <> 0% then gosub print_error
 
        return clear all
        goto inputmode


        delete_serial_no
           init(" ") rga_key0$, rga_rec$()
    
           rga_key0$ = rga_serial$
           read #1,hold,key 0% = rga_key0$, using L35060, rga_rec$(),     ~
                                                eod goto delete_serial_err1
           delete_serial% = 0%
           convert str(rga_rec$(),249%,2%) to delete_serial%,               ~
                                                data goto delete_serial_1

delete_serial_1
           
           if delete_serial% > 0% then goto delete_serial_err
 
               delete #1
               call "SHOSTAT" ("Serial Number (" & rga_serial$ & ") Deleted!!")
               call "PAUSE" ADDR(100%)

        return clear all
        goto inputmode
delete_serial_err:
           errormsg$ = "(Error) Cannot delete (" & rga_serial$ & ") Scanning Data Exists?"
           gosub error_prompt
        return
delete_serial_err1:
           errormsg$ = "(Error) Serial Number not on File????"
        return

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

        print_error  
            hdr$     = "***** Label Printing Error *****"
            msg$(1%) = "ERROR OCCURRED WHILE PRINTING LABELS!!!"
            msg$(2%) = "Return Code (AWDRGB01) = "   
            msg$(3%) = "Press <ENTER> to Continue, <PF16> to Exit"

            convert err% to str(msg$(2%),26%,2%), pic(#0)    
L64000:     k% = 2%
            call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
                if k%  =  0% then return
                if k% <> 16% then goto L64000
            return clear all
            goto inputmode

        display_codes
            call "APCPLN1B" (tab%, #5)
        return
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
                          #63,           /* BCKSUBPT File              */~
                          err1%)         /* Error Code                 */
           
            if err1% <> 0% then str(bcksubpt_rec$,48%,20%) = "00000                    "
            if err1% = 0% then return

            errormsg$ = "Read Error-S.O.= "&so_inv$&" Line= "& item_no$&" Flg= "&sub_flg$ 
            gosub error_prompt
            err1% = 0%
        return

        convert_sales_order
            sc_so_ord%  = 0%
            sc_so_line% = 1%
            convert sc_so_ord$  to sc_so_ord%, data goto convert_sales_alpha

            convert sc_so_ord% to sc_so_ord$, pic(00000000)

            goto convert_sales_line

convert_sales_alpha: 
            convert str(so_so_ord$,2%,7%) to so_so_ord%, data goto convert_sales_alpha1
convert_sales_alpha1:

            convert so_so_ord% to str(so_so_ord$,2%,7%), pic(0000000)

convert_sales_line:
            convert sc_so_line$ to sc_so_line%, data goto convert_sales_line1
convert_sales_line1:

            convert sc_so_line% to sc_so_line$, pic(###)

        return
                                                         /* (PAR000)    */                       
        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program

        end

