        REM *************************************************************~
            *                                              (AWD005)     *~
            *         Print Deafult set to --- aes_prt_default$ = "1"   *~
            *                                              3844Z Small  *~
            *                                                           *~
            *  Program Name      - AWDPLN07                             *~
            *  Creation Date     - 09/14/04                             *~
            *  Last Modified Date- 09/12/06                             *~
            *  Written By        - Roy H. Hoffman                       *~
            *  Modifications By  - Roy H. Hoffman                       *~ 
            *                                                           *~
            *  Description       - New AES Barcode Labels               *~
            *                      By Vendor, By PO, By Item            *~
            *                                                           *~
            *  Code Tables Used  - (COLOR    )                          *~
            *                      (AESSERIAL) Save Last Serial Number  *~
            *                                  Assigned.                *~
            *                                                           *~
            *  Subroutine Used   - AWDPLA07 (Print Barcode Labels)      *~
            *                                                           *~
            *  Special Comments  - The word "DELETE" will be in the     *~
            *                      (aes_rec$,94%,6%) when the Serial    *~
            *                      number has been deleted. The field   *~
            *                      name is 'aes_vendor$'                *~
            *                                                           *~
            *  Special gosubs - lookup_po (Verify PO Number)            *~
            *                 - lookup_po_item (Verify PO and Item No)  *~
            *                 - lookup_raw_material (Get Material Info) *~
            *                 - lookup_color (Color Description)        *~
            *                 - lookup_cut_length (Find Material Length)*~
            *                 - search_labels (Info Fr last Label Print)*~
            *                 - assign_serial_no (Get Next Serial No.)  *~
            *                                                           *~
            *                                             (AWD004)      *~
            *  aes_prt_flg$   - 'A' =              'B' =                *~
            *                                                           *~
            *                                                           *~  
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/20/05 ! (New) Label Program For AES Inventory    ! RHH *~
            *          !       Labels.                            !     *~
            * 06/27/05 ! (AWD001) Mod to add Current Available    ! RHH *~
            *          !       quantity for a P.O. line item.     !     *~
            *          !       Also add new error check logic     !     *~
            * 07/12/05 ! (AWD002) Mod to Delete PO for When only  ! RHH *~
            *          !       scanned by AES.                    !     *~
            * 03/01/06 ! (AWD003) Mod to Add the Quantity to the  ! RHH *~
            *          !       strips at the bottom of the label  !     *~
            * 08/04/06 ! (AWD004) Mod to to Print AES Labels      ! RHH *~
            *          !       using Zerbra 220XI Printer or the  !     *~
            *          !       Zebra 3844Z Small Printer.         !     *~
            * 09/12/06 ! (AWD005) Reset the default to the 3844Z  ! RHH *~
            *          !       small printer                      !     *~    
            * 09/13/06 ! (AWD006) mods for voiding tickets        ! DES *~
            * 11/02/06 ! (AWD007) don't default qty               ! DES *~
            *************************************************************

        dim                                                              ~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
	    hnykey$25, hnydesc$32,       /* HYNMASTR fields            */~
            rcvxref_rec$256,             /* RCVXREF record  AWD006     */~
            rcvxref_key$28,              /* RCVXREF key     AWD006     */~
            sc_po$16,                    /* Purchase Order Number      */~
            sc_item$3,                   /* Purchase Order Item Number */~
            sc_raw_no$25,                /* AES Raw Material Part No.  */~
            sc_rack_qty$10,              /* AES Rack Quantity for Label*/~
            sc_available$10,             /* Line Item Availabel Quantity*/~
            sc_raw$14,                   /* Raw Material Number        */~
            sc_raw_d$32,                 /* Raw Material Description   */~
            status$15,                   /* Label Status 0,1,2         */~  
            vb_key1$16,                  /* VBKMASTR Alt key (1)       */~
            vbline_key$28,               /* VBKLINES Master Key        */~
            vbline_rec$(3%)200,          /* VBKLINES Record            */~
            part_cat$4,                  /* Raw Mat Category Code      */~
            part_class$4,                /* Raw Mat Class Code         */~
            part_type$3,                 /* Raw Mat Type Code          */~
            filename$8,                  /* Used by EWDOPEN            */~ 
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            spec_errormsg$79,            /* Special Errormsg   (AWD001)*/~
            current_avail$10,            /* Available Quantity (AWD001)*/~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(10%),                    /* = 0 if the file is open    */~
                                         /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */ 

                                         /* (AESPRDLB) - Label File    */
        dim aes_po$16,                   /* Purchase Order Number      */~
            aes_item$3,                  /* Purchase Order Item Number */~
            aes_pan_no$3,                /* Pan Id Assoc. with PO Item */~
            aes_serial$30, sc_serial$30, /* Serial Number Assigned (8) */~
            aes_delivery$6,              /* Product Delivery Date      */~
            aes_deliv$10,                /* Delivery Formatted         */~
            aes_part$25,                 /* Part Number (Raw Materia)  */~		 
            aes_descr$32,                /* Part Number Description    */~
            aes_vendor$9,                /* Vendor Code                */~
            aes_qty$10,                  /* Label Quantity Pan Size    */~			
	    aes_ord_qty$10,              /* PO Order Qty for Line Item */~
            aes_used_qty$10,             /* PO Line Item Qty Used      */~ 
            aes_pan$10,                  /* Raw Matereial Pan Size     */~
            aes_color$8,                 /* Product Color              */~
            aes_cut$8,                   /* Product Cut Length         */~			
            aes_loc$8,                   /* Prod Loc. (Bin Location)   */~
            aes_oper$10,                 /* Aes Operator               */~
            aes_pin$3,                   /* AeS Pin Number             */~
            aes_ext$10,                  /* XT Number                  */~
            aes_scan_dte$6,              /* Date Scan by AES into Inv  */~
            aes_scan_dte1$10,            /* Date Scanned AES Screen    */~
            aes_scan_tme$4,              /* Time Scan by AES into Inv M*/~
            aes_scan_tme1$5,             /* Time Scanned AES Screen    */~
            aes_scan_usr$3,              /* Aes Userid than scanned Prd*/~
            aes_scan_qty$10,             /* Actual Qty Scanned into Inv*/~
            aes_awd_qty$10,              /* Actual Qty Scanned in Prod */~
            aes_awd_dte$6,               /* Date Scan by AWD into Inv  */~
            aes_awd_dte1$10,             /* Date AWD Scanned Screen    */~
            aes_awd_tme$4,               /* Time Scan by AWD into Inv M*/~
            aes_awd_tme1$5,              /* Time Scanned AWD Screen    */~
            aes_awd_usr$3,               /* AWD Userid than scanned Prd*/~
            aes_date_line$6,             /* Date line Item Complete    */~
            aes_ln_comp$1,               /* Line Item Complete (Y or N)*/~
            aes_ln_qty$10,               /* Actual Line item qty Comp  */~
            aes_recv$1,                  /* Label Rack Qty Recv 0,1,2  */~
            aes_rcv_dte$6,               /* Date Label Rack Qty Recv   */~
            aes_rcv_dte1$10,             /* Date Data Posted Screen    */~
            aes_rcv_tme$4,               /* Time label Rack Qty Recv   */~
            aes_rcv_tme1$5,              /* Time Data Posted screen    */~ 
            aes_rcv_usr$3,               /* Rack Receiver Priocessed By*/~
            aes_fil$4,                   /* Filler Area                */~
            aes_prt_flg$1,               /* AES Prt Flag 1 or 2(AWD004)*/~
            aes_prt_flgd$5,              /* AES Prt Flag Descr (AWD004)*/~
            aes_prt_default$1,           /* AES Default Printer(AWD004)*/~
            aes_key$30,                  /* Primary Key                */~
            aes_key1$8,                  /* Alt Key 1                  */~
            aes_key2$25,                 /* Alt Key 2                  */~
            aes_key3$31,                 /* Alt Key 3                  */~
            aes_rec$256, aes_rec_sav$256,/* Label Record               */~
            aes_serial_desc$128,         /* Use to assign serial no.   */~
            dd$(5%)40,                   /* Debug text                 */~
            x$1, y$1,                    /* Cut Length Masks           */~
            rec$1                        /* Successful Update          */
 
                                         /* Search PO's Variables      */
        dim hh$79, ss$79, val$(600%)79,  /* Sreen Header and Detail    */~
            pg_dte$6,                    /* Today's Date               */~
            pg_dte1$6,                   /* 180 Days Old               */~
            vq_ordr$9, k$3,              /* PO Item Quantity           */~
            vq_out$9,                    /* Item Quantity Available    */~
            vdt_due$10,                  /* PO Line Item Due Date      */~
            srch$36,                     /* Search Count Text          */~
            cnt$8, pageno$16,            /* Item Count and Page No     */~
            d_title$30                   /* PO Screen Title            */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21                                              
            apc$   = "(AWD) Generate AES Barcode Labels     "
            pname$ = "AWDPLN07 - 08/04/2006"              /* (AWD004)  */

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
            * #1  ! AESPRDLS ! AES Barcode Label Database               *~
            * #2  ! VBKMASTR ! PO Header master file                    *~
            * #3  ! VBKLINES ! PO Line item master file                 *~
            * #4  ! HNYMASTR ! Part Master File                         *~
            * #5  ! GENCODES ! Master Table File                        *~
            * #7  ! RCVSCN   ! Receiver Master TIF              (AWD002)*~
            * #8  ! RCVSCN2  ! Receiver Line Item TIF           (AWD002)*~ 
            * #13 ! RCVXREF  ! Receiver Cross reference         (AWD006)*~ 
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select  #1, "AESPRDLS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 256,                                   ~
                        keypos = 1, keylen = 30,                         ~
                        alt key 1, keypos = 37, keylen = 25, dup,        ~
                            key 2, keypos = 31, keylen = 31, dup,        ~
                            key 3, keypos = 239, keylen = 13, dup  
 

            select #2,  "VBKMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1030,                                  ~
                        keypos = 1, keylen = 25,                         ~
                        alt key 1, keypos = 10, keylen = 16

            select #3,  "VBKLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 700,                                   ~
                        keypos = 1, keylen = 28

            select #4,  "HNYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  900,                                  ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup

            select #5,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24
                                                     /* (AWD002)        */        

            select #7,  "RCVSCN",                                        ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos = 12, keylen = 16,                       ~
                         alternate key 1, keypos = 1, keylen = 11   

            select #8,  "RCVSCN2",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 800,                                  ~
                         keypos = 1, keylen = 28

                                                     /* (AWD002)        */
            select #13, "RCVXREF",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 256,                                   ~
                        keypos = 1, keylen = 28,                         ~
                        alt key 1, keypos = 29, keylen = 8,              ~
                            key 2, keypos = 37, keylen = 42, dup,        ~
                            key 3, keypos = 79, keylen = 16, dup
                                                     /* (AWD006)        */

            call "SHOSTAT" ("Opening Files, One Moment Please")

        REM     filename$ = "AESPRDLB" 
        REM     call "EWDOPEN" (#1, filename$, err%)
        REM     if err% <> 0% then gosub open_error

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),500%, rslt$(1%))

            filename$ = "VBKMASTR" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "VBKLINES" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "HNYMASTR" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "GENCODES" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
                                                     /* (AWD002)        */
            call "OPENCHCK" (#7, fs%(7%), f2%(7%),500%, rslt$(7%))
            call "OPENCHCK" (#8, fs%(8%), f2%(8%),500%, rslt$(8%))
                                                     /* (AWD002)        */
                                                     /* (AWD006)        */
            call "OPENCHCK" (#13, fs%(13%), f2%(13%),500%, rslt$(13%))
                                                     /* (AWD006)        */

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            time$ = time
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
	   aes_prt_flg$ = " "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

        inputmode_1 
	    fieldcnt% = 3%     
            if aes_prt_flg$ = "A" or aes_prt_flg$ = "B" then fieldcnt% = 2%
            for fieldnr% = 1% to fieldcnt%
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
                      if errormsg$ <> " " and                        ~
                         str(errormsg$,1,3) <> "(Wa" then L10130
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
                  if keyhit%  = 16% then gosub create_rack_label
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%
            fieldnr% = 2%
            if fieldnr% < 1% or fieldnr% > fieldcnt% then editpg1
REM         if fieldnr% = lastfieldnr% then    editpg1
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
            *       I N P U T   M O D E   S C R E E N   T W O           *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode_a
            gosub initialize_variables

            for fieldnr% = 1% to   1%
L12210:         gosub'052(fieldnr%)        /* Default - Enables */
                      if enabled% = 0% then L12330
L12230:         gosub'102(fieldnr%, 1%)    /* Display - Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L12315
L12260:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L12230
                         if fieldnr% = 1% then L12210
                         goto L12260
L12315:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L12230
L12330:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L12230
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   S C R E E N   T W O            *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then gosub delete_serial_no
                  if keyhit% <>  0% then       editpg2
L13220:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 1% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L13270:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L13270
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13270
                  lastfieldnr% = fieldnr%
            goto L13220

        REM *************************************************************~
            *       I N P U T   M O D E   S C R E E N   T H R E E       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode_b
            gosub initialize_variables

            for fieldnr% = 1% to   1%
L13210:         gosub'053(fieldnr%)        /* Default - Enables */
                      if enabled% = 0% then L13330
L13230:         gosub'103(fieldnr%, 1%)    /* Display - Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L13315
L13260:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'053(fieldnr%)
                         if enabled% = 1% then L13230
                         if fieldnr% = 1% then L13210
                         goto L13260
L13315:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L13230
L13330:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L13230
                      gosub search_raw_nos
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   S C R E E N   T H R E E        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg3
            lastfieldnr% = 0%
            gosub'103(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 10% then gosub search_raw_nos
                  if keyhit% <>  0% then       editpg3
L14220:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 1% then editpg3
            if fieldnr% = lastfieldnr% then    editpg3
            gosub'053(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg3
L14270:     gosub'103(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L14270
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L14270
                  lastfieldnr% = fieldnr%
            goto L14220

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            if aes_prt_flg$ <> "A" or aes_prt_flg$ <> "B" then return
	    if fieldnr% = 3% then enabled% = 0%
           
        return

        deffn'052(fieldnr%)
            enabled% = 1%
           
        return

        deffn'053(fieldnr%)
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
                                                     /* (AWD004)    */  
        scrn1_msg  :  data                                                       ~
         "Enter a Part Number?                                                 ",~
         "Either Change the Rack Quantity or (Hit Return) to Accept?           ",~
         "Enter a Printer?                                                     "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28310
                inpmessage$ = edtmessage$
                return

L28310
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return
                                                             
        scrn2_msg  :  data                                               ~
         "Enter a Valid Serial Number to Delete?                       "

        deffn'070(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28410
                inpmessage$ = edtmessage$
                return

L28410
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn3_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return
                                                             
        scrn3_msg  :  data                                               ~
         "Enter a Valid RAW Material number to Search?                 "

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
            init(" ") errormsg$, inpmessage$, sc_raw_no$, sc_rack_qty$,  ~
                      sc_po$, sc_item$, aes_po$, aes_item$, aes_pan_no$, ~
                      aes_serial$, aes_delivery$, aes_part$, aes_descr$, ~
                      aes_vendor$, aes_qty$, aes_ord_qty$, aes_used_qty$,~
                      aes_pan$, aes_color$, aes_cut$, aes_loc$, aes_oper$,~
                      aes_pin$, aes_ext$, aes_scan_dte$, aes_scan_tme$,  ~
                      aes_scan_qty$, aes_scan_usr$, aes_awd_dte$,        ~
                      aes_awd_tme$, aes_awd_qty$, aes_awd_usr$, aes_date_line$,~
                      aes_ln_comp$, aes_ln_qty$, aes_recv$, aes_rcv_dte$,~
                      aes_rcv_tme$, aes_rcv_usr$, aes_fil$, aes_key$, aes_key1$,~
                      aes_key2$, aes_key3$, part_cat$, part_class$, part_type$,~
                      sc_available$, sc_raw$, sc_raw_d$, aes_scan_dte1$, ~
                      aes_scan_tme1$, aes_awd_dte1$, aes_awd_tme1$,      ~
                      aes_rcv_dte1$, aes_rcv_tme1$, status$, sc_serial$, ~
                      current_avail$, spec_errormsg$,       ~
                      aes_prt_flgd$

            aes_prt_default$ = "A"                        /* (AWD005)   */ 
                                                          /* (AWD004)   */
        return

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
           init(" ") aes_key$, aes_rec$, rec$, dd$() 
           rec% = 0%
           rec_fnd = 0
REM        gosub assign_serial_no

           aes_key$ = aes_serial$   /* This should not occurr */

           read #1,hold,key 0% = aes_key$, using L30000, aes_rec$,    ~
                                                          eod goto L30010
L30000:        FMT CH(256)
           rec_fnd = 1
               delete #1

               put #1, using L30000, aes_rec$

               goto L30015               /* Finished-Preserve Exisiting*/

L30010:   
                                         /* (AESPRDLB)                 */
        aes_qty = sc_rack_qty
        aes_delivery$ = date     /* Set to Today               */
        aes_time$     = date     /* Set to Today               */
        aes_oper$     = userid$                                     

        put #1, using L35040,                                            ~
            aes_serial$,                 /* Serial Number Assigned (8) */~
            aes_delivery$,               /* Product Delivery Date      */~
            aes_part$,                   /* Part Number (Raw Materia)  */~		 
            aes_descr$,                  /* Part Number Description    */~
            aes_vendor$,                 /* Vendor Code                */~
            aes_qty,                     /* Label Quantity Assoc. Label*/~			
	    aes_ord_qty,                 /* PO Order Qty for Line Item */~
            aes_used_qty,                /* P.O.Line Item Qty Used     */~
            aes_pan,                     /* Raw Matereial Pan Size     */~
            aes_color$,                  /* Product Color              */~
            aes_cut$,                    /* Product Cut Length         */~			
            aes_loc$,                    /* Prod Loc. (Bin Location)   */~
            aes_oper$,                   /* Aes Operator               */~
            aes_pin$,                    /* AeS Pin Number             */~
            aes_ext$,                    /* XT Number                  */~
            aes_scan_dte$,               /* Date Scan by AES into Inv  */~
            aes_scan_tme$,               /* Time Scan by AES into Inv M*/~
            aes_scan_qty,                /* Actual Qty Scan into Inv   */~
            aes_scan_usr$,               /* Aes Userid than scanned Prd*/~
            aes_awd_dte$,                /* Date Scan by AWD into Inv  */~
            aes_awd_tme$,                /* Time Scan by AWD into Inv M*/~
            aes_awd_qty,                 /* Actual Qty Scan into Inv   */~
            aes_awd_usr$,                /* AWD Userid than scanned Prd*/~
            aes_date_line$,              /* Date line Item Complete    */~
            aes_ln_comp$,                /* Line Item Complete (Y or N)*/~
            aes_ln_qty,                  /* Actual Line Item Qty Comp  */~
            aes_recv$,                   /* Label Rack Qty Recv 0,1,2  */~
            aes_rcv_dte$,                /* Date Label Rack Qty Recv   */~
            aes_rcv_tme$,                /* Time Label Rack Qty Recv   */~
            aes_rcv_usr$,                /* Rack Receiver processed by */~ 
            aes_time$                    /* Rtime for label print      */ 
      
L30015:      
            write #1, eod goto L30020
            goto L30018

            if aes_serial$ = "        " then goto L30018
            str(rcvxref_key$,01,16) = aes_po$
            str(rcvxref_key$,17,09) = aes_vendor$
            str(rcvxref_key$,26,03) = aes_item$   /* ??? */
            read #13, hold, key = rcvxref_key$, using L30018,    ~
                               rcvxref_rec$, eod goto L30016       
			       /* may need a way of having duplicates? */
            delete #13
L30016:
            init(" ") rcvxref_rec$
            str(rcvxref_rec$,01,16) = aes_po$
            str(rcvxref_rec$,17,09) = aes_vendor$
            str(rcvxref_rec$,26,03) = aes_item$   /* ??? */
            str(rcvxref_rec$,29,08) = aes_serial$
            str(rcvxref_rec$,95,03) = userid$         
            write #13, using L30018, rcvxref_rec$,                   ~
                                             eod goto L30017       
L30018:        FMT CH(256)
L30017:

            rec% = 1% : rec$ = "1"
            read #1,key 0% = aes_key$, using L30000, aes_rec$,          ~
                                                        eod goto L30020
        return
L30020:     rec% = 0% : rec$ = "0"

        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

                                         /* File = (AESPRDLB)          */

L35040:     FMT CH(30),                  /* Lot Number                 */~
                CH(06),                  /* Product Delivery Date      */~
                CH(25),                  /* Part Number (Raw Materia)  */~		 
                CH(32),                  /* Part Number Description    */~
                CH(09),                  /* Vendor Code                */~
                PD(15,4),                /* Label Quantity Pan Size    */~			
	        PD(15,4),                /* PO Line Item Quantity      */~
                PD(15,4),                /* P.O. Line Item Qty Used    */~
                PD(15,4),                /* Raw Matereial Pan Size     */~
                CH(08),                  /* Product Color              */~
                CH(08),                  /* Product Cut Length         */~			
                CH(08),                  /* Prod Loc. (Bin Location)   */~
                CH(10),                  /* Aes Operator               */~
                CH(03),                  /* Aes Pin Number             */~
                CH(10),                  /* XT Number                  */~
                CH(06),                  /* Date Scan by AES into Inv  */~
                CH(04),                  /* Time Scan by AES into Inv M*/~
                PD(15,4),                /* Actual Qty Scan into Inv   */~
                CH(03),                  /* Aes Userid than scanned Prd*/~
                ch(06),                  /* Date Scan by AWD into Inv  */~
                CH(04),                  /* Time Scan by AWD into Inv M*/~
                PD(15,4),                /* Actual Qty Scan into Inv   */~
                CH(03),                  /* AWD Userid than scanned Prd*/~
                CH(06),                  /* Date line Item Complete    */~
                CH(01),                  /* Line Item Complete (Y or N)*/~
                PD(15,4),                /* Actual Line Item Qty Comp  */~
                CH(01),                  /* Label Rack Qty Recv (Y/N)  */~
                CH(06),                  /* Date Label Rack Qty Recv   */~
                CH(04),                  /* Time Label Rack Qty Recv   */~
                CH(03),                  /* Rack Receiver Processed by */~
                CH(4)                    /* Filler Area                */

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
              on fieldnr% gosub L40160,     /* Part Number             */~
                                L40160,     /* Rack Quantity           */~
                                L40160      /* Printer                 */
            if aes_prt_flg$ <> "A" and aes_prt_flg$ <> "B" then goto L40190
	    lfac$(3%) = hex(84)

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low    */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only  */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric     */
                                                        /* (AWD004)    */             
L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Lot Number:           ",                     ~
               at (03,25), fac(hex(94)), aes_serial$          , ch(30),~
                                                                         ~
               at (04,02), "Material Number:      ",                     ~
               at (04,25), fac(lfac$(1%)), aes_part$            , ch(25),~
                                                                         ~
               at (05,02), "Rack Quantity        :",                     ~
               at (05,25), fac(lfac$(2%)), sc_rack_qty$         , ch(10),~
               at (06,02), "Printer:",                                   ~
               at (06,14), fac(lfac$(3%)), aes_prt_flg$         , ch(01),~
               at (06,17), fac(hex(84)), aes_prt_flgd$          , ch(05),~
                                                                         ~  
               at (07,02), fac(hex(94)), spec_errormsg$         , ch(79),~
                                                                         ~
               at (08,02), "Ln Qty After Label   :",                     ~
               at (08,25), fac(hex(94)), sc_available$          , ch(10),~
                                                                         ~
               at (11,02), "Raw Material Number  :",                     ~
               at (11,25), fac(hex(84)), aes_part$              , ch(25),~
                                                                         ~
               at (12,02), "Raw Material Descr.  :",                     ~
               at (12,25), fac(hex(84)), aes_descr$             , ch(32),~
                                                                         ~
               at (13,02), "Raw Material Pan Size:",                     ~
               at (13,25), fac(hex(84)), aes_pan$               , ch(10),~
                                                                         ~
               at (14,02), "Raw Material color   :",                     ~
               at (14,25), fac(hex(84)), aes_color$             , ch(08),~
                                                                         ~
               at (15,02), "Raw Material Cut Len :",                     ~
               at (15,25), fac(hex(84)), aes_cut$               , ch(08),~
                                                                         ~
               at (16,02), "Raw Material Location:",                     ~
               at (16,25), fac(hex(84)), aes_loc$               , ch(08),~
                                                                         ~
               at (17,02), "Raw Mat Category Code:",                     ~
               at (17,25), fac(hex(84)), part_cat$              , ch(04),~
                                                                         ~
               at (18,02), "Raw Mat Part Class   :",                     ~
               at (18,25), fac(hex(84)), part_class$            , ch(04),~
                                                                         ~
               at (19,02), "Raw Mat Part Type    :",                     ~
               at (19,25), fac(hex(84)), part_type$             , ch(03),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 9% then goto L40400
                  gosub search_for_pos
                  gosub display_pos
                  return clear all
                  goto inputmode

              
L40400:        if keyhit% <> 8% then goto L40420
                  return clear all
                  goto inputmode_a     /* Delete a Serial Number */

L40420:        if keyhit% <> 15% then goto L40440
                  call "PRNTSCRN"
                  goto L40190

L40440:        if keyhit% <> 10% then goto L40450
                  goto inputmode_b
                  return clear all
                  goto inputmode

L40450:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &        ~
                      "                    (10)Search Raw Mat's"
            pf$(2%) = "                 (8)Delete Ser./Lookup  " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &        ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffff08ff0affffffff0f1000)

            if fieldnr% = 1% then L40570
                str(pf$(1%),60%)     = " " : str(pfkeys$,10%,1%) = hex(ff)
                str(pf$(2%),18%,26%) = " " : str(pfkeys$,8%,1%)  = hex(ff)
                str(pf$(3%),64%)     = " " : str(pfkeys$,16%,1%) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                      (15)Print Screen "
            pf$(3%) = "                                        " &        ~
                      "                      (16)PRINT LABELS "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)

            if no_label% = 0% then return
               str(pf$(3%),64%)     = " " : str(pfkeys$,16%,1%) = hex(ff)
               errormsg$ = "Cannot Print Label. Order quantity fulfilled." 

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
              on fieldnr% gosub L42160      /* Serial Number           */

              goto L42190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low    */
L42160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only  */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric     */
                                                        /* (AWD002)    */
L42190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Serial Number To Delete:",                   ~
               at (03,30), fac(lfac$(1%)), sc_serial$           , ch(08),~
                                                                         ~
               at (05,02), "P.O.-Line Item-Rack ID:",                    ~
               at (05,30), fac(hex(84)), aes_po$                , ch(16),~
               at (05,46), "-",                                          ~
               at (05,48), fac(hex(84)), aes_item$              , ch(03),~
               at (05,52), "-",                                          ~ 
               at (05,54), fac(hex(84)), aes_pan_no$            , ch(03),~
                                                                         ~
               at (07,02), "Part Number  :",                             ~
               at (07,20), fac(hex(84)), aes_part$              , ch(25),~
               at (07,47), fac(hex(84)), aes_descr$             , ch(32),~
                                                                         ~  
               at (09,02), "Rack Quantity:",                             ~
               at (09,20), fac(hex(84)), aes_qty$               , ch(10),~
               at (09,47), "Serial No.",                                 ~
               at (09,58), fac(hex(84)), aes_serial$            , ch(08),~
                                                                         ~
               at (12,14), "AES Scan Info.",                             ~
               at (12,32), "AWD Scan Info.",                             ~
               at (12,50), "RCV Posting Data",                           ~
                                                                         ~  
               at (13,02), "Scan Date :",                                ~
               at (14,02), "Scan Time :",                                ~
               at (15,02), "Scan QTY  :",                                ~
               at (16,02), "Scan User :",                                ~   
                                                                         ~
               at (13,14), fac(hex(84)), aes_scan_dte1$         , ch(10),~
               at (14,14), fac(hex(84)), aes_scan_tme1$         , ch(05),~
               at (15,14), fac(hex(84)), aes_scan_qty$          , ch(10),~
               at (16,14), fac(hex(84)), aes_scan_usr$          , ch(03),~
                                                                         ~
               at (13,32), fac(hex(84)), aes_awd_dte1$          , ch(10),~
               at (14,32), fac(hex(84)), aes_awd_tme1$          , ch(05),~
               at (15,32), fac(hex(84)), aes_awd_qty$           , ch(10),~
               at (16,32), fac(hex(84)), aes_awd_usr$           , ch(03),~
                                                                         ~
               at (13,50), fac(hex(84)), aes_rcv_dte1$          , ch(10),~
               at (14,50), fac(hex(84)), aes_rcv_tme1$          , ch(05),~
               at (15,50), fac(hex(84)), aes_recv$              , ch(01),~
               at (16,50), fac(hex(84)), aes_rcv_usr$           , ch(03),~
                                                                         ~
               at (15,58), fac(hex(84)), status$                , ch(14),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L42420
                  call "PRNTSCRN"
                  goto L42190

L42420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L42610     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &        ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L42570
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L42570:     if fieldnr% > 1% then L42590
                str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L42590:     return

L42610: if fieldnr% > 0% then L42700  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                      (15)Print Screen "
            pf$(3%) = "                                        " &        ~
                      "                   (16)Delete Ser. No. "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)


            return
L42700:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                                       "
            pf$(3%) = "                                        " &        ~
                      "                                       " 
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

          REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
              gosub'070(1%, fieldnr%)
              gosub set_pf3
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
                                                         /* (AWD002)   */
              on fieldnr% gosub L43160      /* Serial Number           */

              goto L43190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low    */
L43160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only  */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric     */
                                                        /* (AWD002)    */
L43190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Raw Material Number:",                       ~
               at (03,25), fac(lfac$(1%)), sc_raw$              , ch(14),~
               at (03,45), fac(hex(84)), sc_raw_d$              , ch(32),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L43420
                  call "PRNTSCRN"
                  goto L43190

L43420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf3
        if edit% = 2% then L43610     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &        ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L43570
                str(pf$(1%),60%) = " " : str(pfkeys$,10%,1%) = hex(ff)
L43570:     if fieldnr% > 1% then L43590
                str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L43590:     return

L43610: if fieldnr% > 0% then L43700  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                   (10)Search Raw Mat's"
            pf$(2%) = "                                        " &        ~
                      "                      (15)Print Screen "
            pf$(3%) = "                                        " &        ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffff0affffffff0f1000)


            return
L43700:                              /*  Edit Mode - Enabled    */
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

            on fieldnr% gosub L50075,        /* Part Number           */~
                              L50080,        /* Raw Material Rack Qty */~
                              L50100         /* Printer               */

            return
                                             /* (VBKMASTR)             */
L50010: Rem Purchase Order Number               sc_po$
            init(" ") aes_po$, aes_vendor$
            gosub lookup_po
            if lookup_po% = 0% then goto L50030

        return
L50030:     errormsg$ = "(Error) Invalid Purchase Order Number-" & aes_po$
            gosub error_prompt
            init (" ") sc_po$, aes_po$, aes_vendor$
        return 

L50050: Rem P.O. Lime Item Number               sc_item$
            init(" ") aes_item$, aes_prt_flgd$

            gosub lookup_po_item
            if lookup_po_item% = 0% then goto L50055

            if lookup_raw_material% = 0% then goto L50060
                                                   /* (AWD004)        */
            if aes_prt_flg$ = "A" or aes_prt_flg$ = "B" then lfac$(4) = hex(84)
            if aes_prt_flg$ = "A" then aes_prt_flgd$ = "TEST1"          ~
                                  else aes_prt_flgd$ = "TEST2"
                                                   /* (AWD004)        */
        return 
L50055:     errormsg$ = "(Error) Looking Up PO/Line Item-" & aes_po$ & aes_item$
            gosub error_prompt
            init(" ") sc_item$, aes_item$
        return

L50060:     errormsg$ = "(Error) Looking Up Raw Material-" & aes_part$
            gosub error_prompt
            init(" ") sc_item$, aes_item$, aes_part$
        return

L50070: Rem Lot Number                                           
        return

L50072:    aes_key$ = aes_serial$   /* This should not occurr */
           read #1,key = aes_serial$,using L30000,aes_rec$, eod goto L50074

L50073:     errormsg$ = "(Warning) This is a label reprint!!!"          
        get #1, using L35040,                                            ~
            aes_serial$,                 /* Serial Number Assigned (8) */~
            aes_delivery$,               /* Product Delivery Date      */~
            aes_part$,                   /* Part Number (Raw Materia)  */~		 
            aes_descr$,                  /* Part Number Description    */~
            aes_vendor$,                 /* Vendor Code                */~
            aes_qty,                     /* Label Quantity Assoc. Label*/~			
	    aes_ord_qty,                 /* PO Order Qty for Line Item */~
            aes_used_qty,                /* P.O.Line Item Qty Used     */~
            aes_pan,                     /* Raw Matereial Pan Size     */~
            aes_color$,                  /* Product Color              */~
            aes_cut$,                    /* Product Cut Length         */~			
            aes_loc$,                    /* Prod Loc. (Bin Location)   */~
            aes_oper$,                   /* Aes Operator               */~
            aes_pin$,                    /* AeS Pin Number             */~
            aes_ext$,                    /* XT Number                  */~
            aes_scan_dte$,               /* Date Scan by AES into Inv  */~
            aes_scan_tme$,               /* Time Scan by AES into Inv M*/~
            aes_scan_qty,                /* Actual Qty Scan into Inv   */~
            aes_scan_usr$,               /* Aes Userid than scanned Prd*/~
            aes_awd_dte$,                /* Date Scan by AWD into Inv  */~
            aes_awd_tme$,                /* Time Scan by AWD into Inv M*/~
            aes_awd_qty,                 /* Actual Qty Scan into Inv   */~
            aes_awd_usr$,                /* AWD Userid than scanned Prd*/~
            aes_date_line$,              /* Date line Item Complete    */~
            aes_ln_comp$,                /* Line Item Complete (Y or N)*/~
            aes_ln_qty,                  /* Actual Line Item Qty Comp  */~
            aes_recv$,                   /* Label Rack Qty Recv 0,1,2  */~
            aes_rcv_dte$,                /* Date Label Rack Qty Recv   */~
            aes_rcv_tme$,                /* Time Label Rack Qty Recv   */~
            aes_rcv_usr$,                /* Rack Receiver processed by */~ 
            aes_fil$                     /* Filler Area                */
            gosub lookup_part
            aes_key$ = aes_serial$
            hold_comp% = comp% 
          /*  gosub error_prompt */
            comp% = hold_comp%

L50074:    return

L50075: Rem Part Number                                           
        init(" ") aes_descr$
        gosub lookup_part
        if lookup_raw_material% = 0% then goto L50076

        return
L50076:     errormsg$ = "(Error) Invalid Raw Material Number?"
            gosub error_prompt
            init (" ") sc_raw$, sc_raw_d$
        return                

L50080: Rem Raw Material Rack Quantity          sc_rack_qty$
                                                        /* (AWD001)   */
            if spec_errormsg% <> 1% then spec_errormsg% = 0%
            no_label% = 0%
REM         p% = pos(sc_rack_qty$ = "." )
REM         if p% = 0% then goto L50090
            sc_available = 0.0
            aes_qty      = 0.0  
            sc_rack_qty  = 0.0
            convert sc_rack_qty$ to sc_rack_qty, data goto L50090
            sc_rack_qty% = sc_rack_qty
            sc_rack_qty  = sc_rack_qty%

            if sc_rack_qty < 10.0 then goto L50090

            if sc_rack_qty > 9999.0 then goto L50095
REM         convert sc_rack_qty to sc_rack_qty$, pic(####.####-)
            convert sc_rack_qty to sc_rack_qty$, pic(####-)

               put str(aes_rec$,103%,8%), using PO_4, sc_rack_qty 
	    if aes_serial$ = " " then                             ~
                        gosub assign_serial_no

        return

REM         convert sc_rack_qty to sc_rack_qty$, pic(####.####-)
            convert sc_rack_qty to sc_rack_qty$, pic(####-)

            aes_qty  = sc_rack_qty
            aes_qty$ = sc_rack_qty$
                                         /* Check for Line Item Closed */
            max_qty = 0.0
            max_qty = round ((sc_rack_qty + sav_used_qty), 2)
                                         /* Po Line Item Closed        */
            if max_qty > vq_ord_5 then no_label% = 1%

            if no_label% = 0% then sc_available =round((vq_ord - max_qty), 2)

            convert sc_available to sc_available$, pic(####.####-)

                                                      /* (AWD001)      */
            if (current_avail - sc_rack_qty) < 0.0 then spec_errormsg% = 2%
 
            if spec_errormsg% = 1% then goto L50090
            if spec_errormsg% = 2% then goto L50095
                                                      /* (AWD001)      */
        return

L50090:     errormsg$ = "(Error) Invalid AES Rack Quantity?"
            gosub error_prompt
            init(" ") sc_rack_qty$, aes_qty$, sc_available$
        return                                        /* (AWD001)      */
L50095:     errormsg$ = "(Error) Rack Quantity Exceed's P.O. Available Qty?"
            gosub error_prompt
            init(" ") sc_rack_qty$, aes_qty$, sc_available$
        return
                                                       /* (AWD001)     */
L50100: Rem Printer         aes_prt_flg$                      
        if aes_prt_flg$ = "A" or aes_prt_flg$ = "B" then return

        errormsg$ = "(Error) Invalid printer selected {A|B}.           "
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 2.                      *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "

            on fieldnr% gosub L52010         /* Serial Number          */

            return
                                             /* (AESPRDLB)             */
L52010: Rem Rack Serial Number                sc_serial$
           delete% = 0%
           if sc_serial$ <> " " then goto L52020
              goto L52040

L52020:            
            sc_serial% = 0%
            convert sc_serial$ to sc_serial%, data goto L52040

            convert sc_serial% to sc_serial$, pic(000000000000000000000000000000)
            
 
            read #1,key 0% = sc_serial$, using L52030, aes_rec$,           ~
                                                      eod goto L52050
L52030:        FMT CH(256)

            gosub unpack_data

                                            /* Cannot have Scanning Data */ 
                                            /* (AWD002) - Remove AES Test*/
            REM if str(aes_rec$,200%,3%) <> "???" then goto L52060
                                            /* (AWD002) AES Scann Valid  */
            if str(aes_rec$,221%,3%) <> "???" then goto L52060

            if str(aes_rec$,94%,6%) = "DELETE" then goto L52070
                                            /* (AWD002) Seal No. Received*/
            if aes_recv$ = "2" then goto L52035
 
            delete% = 1%
                                            /* Correct Inventory         */ 
            if str(aes_rec$,200%,3%) <> "???" then delete% = 2%
                                            /* (AWD002)                  */

            aes_serial$ = sc_serial$
        return
                                            /* (AWD002)                  */
L52035:     errormsg$ = "(Error) Serial Number has been Received?"
            goto L52100
                                            /* (AWD002)                  */
L52040:     errormsg$ = "(Error) Serial Number is required?"
            goto L52100
L52050:     errormsg$ = "(Error) Serial Number Not on File?"
            goto L52100
L52060:     errormsg$ = "(Error) Serial Number Active Cannot Delete?"
            goto L52100
L52070:     errormsg$ = "(Error) Serial Number Already Deleted?"
L52100:     gosub error_prompt
            init (" ") sc_serial$
            delete% = 0%
        return                

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 3.                      *~
            *************************************************************

        deffn'153(fieldnr%)
            errormsg$ = " "

            on fieldnr% gosub L52200         /* Raw Material Number    */

            return
                                             /* (HNYMASTR)             */
L52200: Rem Raw Material Number                 sc_raw$, sc_raw_d$
           return
           if sc_raw$ <> " " then goto L52210
              goto L52230

L52210:            
            read #4,key 0% = sc_raw$, using L52220, sc_raw_d$,          ~
                                                      eod goto L52230
L52220:        FMT POS(26), CH(32)
        return
L52230:     errormsg$ = "(Error) Invalid Raw Material Number?"
            gosub error_prompt
            init (" ") sc_raw$, sc_raw_d$
        return                

                                                    /* Serial Number Passed Test */                             
        delete_serial_no
                                                    /* (AWD002)                  */
            if delete% = 0% then goto L53010

               read #1,hold,key 0% = aes_serial$, using L52030, aes_rec$,           ~
                                                           eod goto L53000
                  delete #1

               str(aes_rec$,94%,6%)  = "DELETE"
                                                   /* (AWD002)                */
               str(aes_rec$,200%,3%) = "???"
               str(aes_rec$,203%,6%) = "      "
               str(aes_rec$,209%,4%) = "0000"
                                                   /* (AWD002)                */
               write #1, using L52030, aes_rec$, eod goto L53010
                                                   /* Correct Receiver        */
               if delete% = 2% then gosub inventory_delete
    
                                                   /* Put Back Rack Quantity  */
               gosub adjust_qty_used

               if err% = 5% then goto L53030
                                                   /* (AWD002)                */

               call "SHOSTAT" ("Serial Number ---> " & aes_serial$ & " DELETED")
               CALL "PAUSE" ADDR(200%)
 

        return clear all
        goto inputmode

        adjust_qty_used
            init(" ") aes_key$
            get str(aes_rec$,103%,8%), using PO_4, save_qty

            aes_key$ = str(aes_rec$,1%,22%)

        adjust_qty_used_next
            read #1,hold,key 0% > aes_key$, using L52030, aes_rec$,                ~
                                                  eod goto adjust_qty_used_done
                                                  /* Check P.O. Line Item         */
            if str(aes_key$,1%,19%) <> str(aes_rec$,1%,19%) then                   ~
                                                  goto adjust_qty_used_done
                                                  /* Re-set Primary Key           */
               init(" ") aes_key$ 
               aes_key$ = str(aes_rec$,1%,30%)

                                                  /* Skip Deleted Record          */
               if str(aes_rec$,94%,6%) = "DELETE" then goto adjust_qty_used_next

                                                  /* Skip Scanned Records         */
               if str(aes_rec$,200%,3%) <> "???" then goto adjust_qty_used_next

                                                  /* Get Quantity used            */
               get str(aes_rec$,119%,8%), using PO_4, aes_used_qty

                                                  /* Correct the Quantity Used    */
               aes_used_qty = aes_used_qty - save_qty

               if aes_used_qty < 0.0 then aes_used_qty = 0.0

                                                  /* Update the Quantity Used     */
               put str(aes_rec$,119%,8%), using PO_4, aes_used_qty
                                                  /* Delete and Update            */ 
                   delete #1
 
               write #1, using L52030, aes_rec$, eod goto L53020

            goto adjust_qty_used_next

        adjust_qty_used_done

        return

L53000:   errormsg$ = "(Error) Unable to 'READ' Serial No. " & aes_serial$
          gosub error_prompt
          return clear all
          goto inputmode
L53010:   errormsg$ = "(Error) Unable to 'DELETE' Serial No. " & aes_serial$
          gosub error_prompt
          return clear all
          goto inputmode

L53020:   errormsg$ = "(Error) Adjusting Quantity for P.O. Line " & str(aes_rec$,1%,19%)
          gosub error_prompt
          return 

L53030:   errormsg$ = "(Error) Adjusting Inventory Quantity for P.O. Line " & str(aes_rec$,1%,19%)
          gosub error_prompt
          return 

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        lookup_po                            /* (VBKMASTR)             */
            lookup_po% = 0%
            init(" ") vb_key1$, aes_po$, aes_vendor$
            vb_key1$ = sc_po$
            read #2,key 1% = vb_key1$,using PO_1, aes_vendor$,          ~
                                                   eod goto PO_2
PO_1:           FMT CH(09)
            aes_po$ = sc_po$ 
            lookup_po% = 1%
PO_2:
        return 

        lookup_po_item                       /* (VBKLINES)             */
            lookup_po_item% = 0%
            init(" ") vbline_key$, aes_item$, aes_part$, aes_descr$

            convert sc_item$ to sc_item%, data goto PO_2A
PO_2A:
            convert sc_item% to sc_item$, pic(###)

            aes_item$ = sc_item$
            str(vbline_key$,1%,9%)     = aes_vendor$
            str(vbline_key$,10%,16%)   = aes_po$
            str(vbline_key$,26%,3%)    = aes_item$ 
            read #3,key = vbline_key$, using PO_3, vbline_rec$(),   ~
                                                       eod goto PO_5
PO_3:           FMT 3*CH(200)

                                             /* Format all Data       */
            vq_ord   = 0.0                   /* Original Qty Ordered  */
            vq_rec   = 0.0                   /* Tot Qty Rec to Date   */
            vq_out   = 0.0                   /* Quantity Outstanding  */ 
            vq_ord_5 = 0.0                   /* Orig Order Plus 5%    */

            get str(vbline_rec$(),93%,8%), using PO_4, vq_ord
PO_4:           FMT  PD(15,4)            

            get str(vbline_rec$(),101%,8%), using PO_4, vq_rec 


            get str(vbline_rec$(),109%,8%), using PO_4, vq_out
 

                                             /* Add 5% to Original Qty*/
            vq_ord_5 = round(vq_ord + (vq_ord * .05), 2) 

            aes_part$  = str(vbline_rec$(),32%,25%)
            aes_descr$ = str(vbline_rec$(),57%,32%)
lookup_part:
            gosub lookup_raw_material
                                            /* Material Color      */  
            gosub lookup_color
                                            /* Material Length     */
            gosub lookup_cut_length
                                            /* Find last label     */
                                            /* created             */
            gosub search_labels             

            lookup_po_item% = 1%
                                            /* sav_used_qty        */
                                            /* (AWD001)            */
                                            /* New Available Qty   */

            return

            spec_errormsg% = 0%
            current_avail = 0.0
        REM    current_avail = round(vq_ord - vq_rec, 2) 
            current_avail = vq_out

            convert current_avail to current_avail$, pic(####.####-)
          
                 if current_avail < 1.0 then spec_errormsg% = 1%
            if spec_errormsg% = 1% then                             ~
                   spec_errormsg$ = "(Error) - P.O. is Satisfied cannot print Labels!!"
                                            /* (AWD001)            */ 
PO_5:
        return

        lookup_raw_material                  /* (HNYMASTR)         */
            lookup_raw_material% = 0% 
            init(" ") aes_pan$, aes_loc$, part_cat$, part_class$,    ~
                      part_type$, aes_color$, aes_cut$, aes_loc$ 
  
            read #4, key 0% = aes_part$, using PO_6, aes_descr$,  ~
                     part_cat$, part_class$, aes_loc$, part_type$, ~
                              aes_pan, eod goto PO_7

PO_6:        FMT POS(26), CH(32),                                ~
                 POS(90), CH(04), POS(133), CH(04), POS(155%),   ~
                 CH(08), POS(180), CH(03), POS(326), PD(15,4)
            
            convert aes_pan to aes_pan$, pic(####.####-)

                                            /* Set Rack Quantity  */
REM         sc_rack_qty$ = aes_pan$
	    /* AWD007 - don't use default qty, use zero */
REM         sc_rack_qty  = 0.0
REM         sc_rack_qty$ = "   0.00"  
	    /* AWD007 */

            lookup_raw_material% = 1%
PO_7:

        return

        lookup_color                       /* Raw Material Part Number */ 
            init(" ") readkey$, desc$, aes_color$
            str(readkey$,1%,9%)  = "COLOR    "
            str(readkey$,10%,2%) = str(aes_part$,5%,1%)  
            read #5,key = readkey$, using PO_8, desc$, eod goto PO_9
PO_8:          FMT POS(25), CH(30)
            aes_color$ = str(desc$, 6%, 8%)

PO_9:   return

        lookup_cut_length
            init(" ") aes_cut$,x$, y$
            x$ = bin(34%,1)                /* Suff a '"' Quote into X$  */
            y$ = "'"                       /* Set to ''' = Single Quote */
                                           /* 1st Check Inches          */
            p% = pos(aes_descr$ = x$ )
            if p% <> 0% then aes_cut$ = str(aes_descr$,p%-4%, 5%)

            if p% <> 0% then goto PO_10
                                           /* 2nd Check Feet            */
               p% = pos(aes_descr$ = y$ )

               if p% <> 0% then aes_cut$ = str(aes_descr$,p%-4%, 5%)
PO_10:
        return 

        search_labels
           init(" ") aes_key$, aes_rec$, aes_rec_sav$

           hit%         = 0%
           aes_pan_no%  = 0%
           sav_ord_qty  = vq_ord               /* Current Order Quantity */ 
           sav_used_qty = 0.0                  /* Curr Received   vq_rec */
                                               /* For now do not use     */
           str(aes_key$,1%,16%)  = aes_po$
           str(aes_key$,17%,3%)  = aes_item$

search_labels_next:
           read #1,key 0% > aes_key$, using L30000, aes_rec$,             ~
                                                eod goto search_labels_done
              if str(aes_key$,1%,19%) <> str(aes_rec$,1%,19%) then        ~
                                                    goto search_labels_done
                                              /* Set Lookup Key         */
                 aes_key$ = str(aes_rec$,1%,30%)
                                              /* Save Last Pan ID       */ 
                 convert str(aes_rec$,20%,3%) to aes_pan_no%,             ~
                                              data goto search_labels_error
                                              /* Check for deleted      */
                 if str(aes_rec$,94%,6%) = "DELETE" then goto search_labels_next
 
                                              /* Save Last Label Data   */
                                              /* for non-deleted record */
                 aes_rec_sav$ = aes_rec$
                 hit% = hit% + 1%

                 goto search_labels_next
search_labels_done:
                                              /* No Labels for Item Printed */
              if aes_pan_no% = 0% then return 
                                              /* At least One Valid label   */
                                              /* printed                    */
              if hit% = 0% then return
                                              /* Values for last label      */
                                              /* printed                    */
                                              /* May be more Current than   */
                                              /* Value in VBKLINES          */            
                 get str(aes_rec_sav$,119%,8%), using PO_4, sav_used_qty

        return
search_labels_error
            errormsg$ = "(ERROR) - While Searching Labels????"
            gosub error_prompt
        return

        assign_serial_no
            init(" ") readkey$, aes_serial_desc$
            str(readkey$,1%,9%)  = "AESSERIAL"
            str(readkey$,10%,15%) = "100"  
            read #5,hold,key = readkey$, using ASSIGN_1, aes_serial_desc$, eod goto ASSIGN_2
ASSIGN_1:        FMT CH(128)

            delete #5
                                      /* Table Value Plus 1           */
            aes_serial$ = str(aes_serial_desc$, 26%, 8%)
            convert aes_serial$ to aes_serial%, data goto ASSIGN_2

            aes_serial% = aes_serial% + 1%

            convert aes_serial% to aes_serial$, pic(00000000)

            str(aes_serial_desc$,25%,8%) = aes_serial$
                                      /* Save Last Serial Number Used */
            write #5, using ASSIGN_1, aes_serial_desc$, eod goto ASSIGN_2

        return
ASSIGN_2:
            errormsg$ = "(ERROR)-Unable to Assign Serial No. for " & aes_po$
            gosub error_prompt
        return clear all
        goto inputmode

                                         /* Print Raw Mat. Rack Label  */ 
        create_rack_label
            debug%     = 0%                           /* 0% = Off      */
            rec_fnd = 0
                                                      /* 1% = On       */
            been_here% = 0%

            call "SHOSTAT" ("Creating and Printing Rack Labels")
            init(" ") aes_key$

            gosub build_label

            gosub dataput         
            if rec% = 0% then goto create_rack_label_2
                                                         /* Print two (2)     */
                                                         /* Labels for each Pan */
            gosub debug_screen_display
            if debug% = 1% then goto debug_1

                lbl% = 2%

create_rack_label_1:
                                                    /* (AWD004)                 */ 
               put str(aes_rec$,103%,8%), using PO_4, sc_rack_qty 
                call "DESPLA07" (pan%, been_here%, aes_rec$, aes_prt_flg$, err%)
                if err% <> 0% then gosub print_error

                lbl% = lbl% - 1%
                if lbl% <> 0% then goto create_rack_label_1

                err% = 99%                          /* Clear Work File, Scratch */
                call "DESPLA07" (pan%, been_here%, aes_rec$, aes_prt_flg$, err%)

                if err% <> 0% then gosub print_error
                                                    /* (AWD004)                 */
debug_1:
        aes_serial$ = " "
        return clear all
        goto inputmode

create_rack_label_2
            errormsg$ = "(Error) While Updating the Database (AESPRDLB) ?"
            gosub error_prompt
        return clear all
        goto inputmode

                                         /* Raw Material Rack Label    */
        build_label
                                         /* Purchase Order Number      */
                                         /* Purchase Order Item Number */
                aes_pan_no$ = " "        /* Pan Id Assoc. with PO Item */
                aes_pan_no% = aes_pan_no% + 1%
                convert aes_pan_no% to aes_pan_no$, pic(###)

                                         /* Serial Number Assigned (8) */

                                         /* Product Delivery Date      */
                aes_delivery$ = date     /* Set to Today               */
 
                                         /* Part Number (Raw Material) */		 
                                         /* Part Number Description    */
                                         /* Vendor Code                */
                                         /* Label Rack Quantity        */			
	        aes_ord_qty = sav_ord_qty/* PO Order Quantity          */
                aes_used_qty= max_qty    /* PO Ln Qty Used / Printed   */
                                         /* Raw Matereial Pan Size     */
                                         /* Product Color              */
                                         /* Product Cut Length         */			
                                         /* Prod Loc. (Bin Location)   */
                aes_oper$ = " "          /* Aes Operator               */
                                         /* AES Pin Operator           */
                aes_pin$ = str(aes_part$,8%,3%)
                aes_ext$ = " "           /* XT Number                  */
                aes_scan_dte$ = " "      /* Date Scan by AES into Inv  */
                aes_scan_tme$ = "0000"   /* Time Scan by AES into Inv M*/
                aes_scan_qty  = 0.0      /* Actual Qty Scan into Inv   */
                aes_scan_usr$ = "???"    /* Aes Userid than scanned Prd*/
                aes_awd_dte$  = " "      /* Date Scan by AWD into Inv  */
                aes_awd_tme$  = "0000"   /* Time Scan by AWD into Inv M*/
                aes_awd_qty   = 0.0      /* Actual Qty Scan into Inv   */
                aes_awd_usr$  = "???"    /* AWD Userid than scanned Prd*/
                aes_date_line$= " "      /* Date line Item Complete    */
                aes_ln_comp$  = "N"      /* Line Item Complete (Y or N)*/
                aes_ln_qty    = 0.0      /* Actual Line Item Qty Comp  */
                                         /* 0 = Printed,1 = Scanned AES*/
                                         /* 2 = Receiver Updated       */
                aes_recv$     = "0"      /* Label Rack Qty Status      */
                aes_rcv_dte$  = " "      /* Date Label Rack Qty Received*/
                aes_rcv_tme$  = "0000"   /* Time Label Rack Qty Received*/
                aes_rcv_usr$  = "???"    /* User Id that Posted Receiver*/        
                aes_fil$      = "       " /* Filler Area                */


        return

        search_for_pos
            srch% = 0%
            init(" ") hh$, ss$, val$(), vbline_key$, vbline_rec$()
            pg_dte$ = date               /* Set Todays Date            */
                                         /* More than 30 Days Old      */ 
            call "DATE" addr("G+",pg_dte$, -30%,pg_dte1$,err%)

            srch$ = "Purchase Orders Checked = [xxxxxxxx]"

            call "SHOSTAT" ("Searching Open P.O.s")

            k%, k_max% = 0%

            str(vbline_key$,1%,9%)  = "VE32678  "
            str(vbline_key$,10%,3%) = "500"
        search_for_pos_nxt
            read #3, key > vbline_key$, using PO_3, vbline_rec$(),   ~
                                          eod goto search_for_pos_done

            if str(vbline_rec$(),1%,7%) <> "VE32678" then goto search_for_pos_done
               vbline_key$ = str(vbline_rec$(),1%,28%)
      
            srch% = srch% + 1%
            if mod(srch%,25%) <> 0 then goto search_for
               convert srch% to str(srch$,28%,8%), pic(########)
               print at(02,23);hex(84);srch$;
                                               
            search_for
               init(" ") vdt_due$, vq_ordr$, vq_out$

               str(vdt_due$,1%,6%) = str(vbline_rec$(),142%,6%)
                                              /* Special Date Test   */
               if str(vdt_due$,1%,6%) < pg_dte1$ then goto search_for_pos_nxt
                                              /* Less than 30 Days   */

               call "DATFMTC" (vdt_due$)

               get str(vbline_rec$(),93%,8%), using PO_4, vq_ordr
 
               get str(vbline_rec$(),101%,8%), using PO_4, vq_rec 

               get str(vbline_rec$(),109%,8%), using PO_4, vq_out

               rhh = vq_out
               rhh = 0.0

               vq_out = vq_ordr - (vq_rec + rhh)
                                           /* See if Qty is Avail   */
               if vq_out < 10.0 then goto search_for_pos_nxt

               k% = k% + 1%
               convert k% to k$, pic(###)

               if k% > 499% then goto search_for_pos_done 

               convert vq_ordr to vq_ordr$, pic(#####.##-)

               convert vq_out to vq_out$, pic(#####.##-)

               convert rhh to rhh$, pic(#####.##-)

                                                  /* Seq No         */
               str(val$(k%),1%,4%)    = k$ & "!"
                                                  /* Vendor Code    */
               str(val$(k%),5%,8%)    = str(vbline_rec$(),1%,7%) & "!"
                                                  /* P.O. Number    */
               str(val$(k%),13%,17%)  = str(vbline_rec$(),10%,16%) & "!"
                                                  /* Line Item No.  */
               str(val$(k%),30%,4%)   = str(vbline_rec$(),26%,3%) & "!"
                                                  /* Raw Material No*/
               str(val$(k%),34%,15%)  = str(vbline_rec$(),32%,14%) & "!"
                                                  /* Due Date       */
               str(val$(k%),49%,11%)  = vdt_due$ & "!"
                                                  /* Order Quantity */
               str(val$(k%),60%,9%)   = vq_ordr$
               str(val$(k%),69%,1%)   = "!"
                                                  /* Quantity Avali */
               str(val$(k%),70%,9%)   = vq_out$
               str(val$(k%),79%,1%)   = "!"
        REM       call "SHOSTAT" (K$ &"-" & str(val$(k%),60%,20%) & " - " &~
        REM                       rhh$ )
        REM       stop

               goto search_for_pos_nxt

        search_for_pos_done
               k_max% = k%
               cnt$ = "[ #### ]"
               convert k_max% to str(cnt$,3%,4%), pic(####)

               K% = 0% 
         return
                                         /* Search PO's for a Specific */
                                         /* Raw Material Number        */
        search_raw_nos
            srch% = 0%
            init(" ") hh$, ss$, val$(), vbline_key$, vbline_rec$()
            pg_dte$ = date               /* Set Todays Date            */
                                         /* More than 30 Days Old      */ 
            call "DATE" addr("G+",pg_dte$, -90%,pg_dte1$,err%)
/*@@@*/
REM         srch$ = "Purchase Orders Checked = [xxxxxxxx]"

REM         call "SHOSTAT" ("Searching Open P.O.s")
 
            k%, k_max% = 0%
	    hnykey$ = sc_raw$

        search_raw_nos_nxt
            read #4,key 0% > hnykey$, using HNYFMT,hnykey$, hnydesc$,   ~
                 vendor$, part_class$,   eod goto search_raw_nos_done

HNYFMT:     FMT CH(25), CH(32), POS(102), CH(09), POS(133), CH(04)      
            if vendor$ <> "VE32678" then                        ~
			     goto search_raw_nos_nxt
            if part_class$ <> "PE12" and part_class$ <> "PE14" then     ~
			     goto search_raw_nos_nxt
            srch% = srch% + 1%
            if mod(srch%,25%) <> 0 then goto search_raw
               convert srch% to str(srch$,28%,8%), pic(########)
               print at(02,23);hex(84);srch$;
                                               
            search_raw
               init(" ") vdt_due$, vq_ordr$, vq_out$

               k% = k% + 1%
               convert k% to k$, pic(###)

               if k% > 499% then goto search_raw_nos_done 

               str(val$(k%),1%,4%)    = k$ & "!"
               str(val$(k%),5%,25%)   = hnykey$ 
               str(val$(k%),30%,1%)   = "!"     
               str(val$(k%),31%,32%)  = hnydesc$ 
               str(val$(k%),63%,1%)   = "!"     

               goto search_raw_nos_nxt

        search_raw_nos_done
               k_max% = k%
               cnt$ = "[ #### ]"
               convert k_max% to str(cnt$,3%,4%), pic(####)

               K% = 0% 
               gosub display_pos
               if k_max% =  0% then goto search_raw_exit

               sc_raw$ = hnykey$

         return clear all
         goto inputmode_1
                                                  /* No P.O. Found */
         search_raw_exit
         errormsg$ = "No Raw Material ("& sc_raw$ &")"
         gosub error_prompt
         return clear all
         goto inputmode 

        REM *************************************************************~
            *  D I S P L A Y   O P E N   P U R C H A S E   O R D E R S  *~
            *-----------------------------------------------------------*~
            * Display P.O. Screen                                       *~
            *************************************************************

        display_pos
DISP_1:     gosub set_disp
            accept                                                       ~
               at (01,02), fac(hex(84)), cnt$                   , ch(08),~
               at (01,62), fac(hex(84)), pageno$                , ch(16),~
                                                                         ~
               at (02,26), fac(hex(84)), d_title$               , ch(30),~
                                                                         ~
               at (04,02), fac(hex(84)), hh$                    , ch(79),~
               at (05,02), fac(hex(84)), ss$                    , ch(79),~
                                                                         ~
               at (06,02), fac(hex(84))  , val$(k% + 1%)        , ch(79),~
               at (07,02), fac(hex(84))  , val$(k% + 2%)        , ch(79),~
               at (08,02), fac(hex(84))  , val$(k% + 3%)        , ch(79),~
               at (09,02), fac(hex(84))  , val$(k% + 4%)        , ch(79),~
               at (10,02), fac(hex(84))  , val$(k% + 5%)        , ch(79),~
               at (11,02), fac(hex(84))  , val$(k% + 6%)        , ch(79),~
               at (12,02), fac(hex(84))  , val$(k% + 7%)        , ch(79),~
               at (13,02), fac(hex(84))  , val$(k% + 8%)        , ch(79),~
               at (14,02), fac(hex(84))  , val$(k% + 9%)        , ch(79),~
               at (15,02), fac(hex(84))  , val$(k% + 10%)       , ch(79),~
               at (16,02), fac(hex(84))  , val$(k% + 11%)       , ch(79),~
               at (17,02), fac(hex(84))  , val$(k% + 12%)       , ch(79),~
               at (18,02), fac(hex(84))  , val$(k% + 13%)       , ch(79),~
               at (19,02), fac(hex(84))  , val$(k% + 14%)       , ch(79),~
               at (20,02), fac(hex(84))  , val$(k% + 15%)       , ch(79),~
               at (21,02), fac(hex(84))  , val$(k% + 16%)       , ch(79),~
               at (22,02), fac(hex(84))  , val$(k% + 17%)       , ch(79),~
                                                                         ~
               at (24,02), fac(hex(a4)), pf$(1%)                , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto DISP_3             /* First    */
DISP_2:           k% = 0%
                  goto DISP_1

DISP_3:        if keyhit% <> 3% then goto DISP_5             /* Last      */
DISP_4:           x% = int(k_max% / 17%)
                  k% = (x%*17%)
                  goto DISP_1

DISP_5:        if keyhit% <> 4% then goto DISP_6             /* Previous */
                  if k% < 18% then goto DISP_2
                  k% = k% - 17%
                  if k% <= 1% then goto DISP_2
                  goto DISP_1

DISP_6:        if keyhit% <> 5% then goto DISP_7             /* Next     */
                  k% = k% + 17%
                  if k% < k_max% then goto DISP_1
                  goto DISP_4

DISP_7:        if keyhit% <> 15 then goto DISP_8
                  call "PRNTSCRN"
                  goto DISP_1

DISP_8:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_disp
            pageno$ = "Page: XXX of XXX"
            xx% = (k_max% / 17%) + 1%
            convert xx% to str(pageno$,14%,3%), pic(###)

            pf$(1) = "(2)First     (3)Last     (4)Previous    " &        ~
                     " (5)Next (15)Print Screen <Return> Cont"
            pfkeys$ = hex(ff02030405ffffffffffffffffff0f1000)
            d_title$ = "**Raw Material Display**"
            str(hh$,1%,4%)   = "Seq!"
            str(ss$,1%,4%)   = "---!"
            str(hh$,5%,26%)   = "Part                     !"
            str(ss$,5%,26%)   = "-------------------------!"
            str(hh$,31%,33%) = "Description                      !"
            str(ss$,31%,33%) = "---------------------------------!"

            gosub check_screen

            yy% = (k%/17%) + 1%
            convert yy% to str(pageno$,7%,3%), pic(###)

            return

        check_screen
            if k_max% > 17% then goto DISP_9
               gosub no_first
               gosub no_next
               gosub no_last
               gosub no_prev
               return
DISP_9:      if k% >= 17% then goto DISP_10
                gosub no_first
                gosub no_prev
DISP_10:     if (k% + 17%) <= k_max% then goto DISP_11
                gosub no_last
DISP_11:     if k% <= (k_max% - 17%) then goto DISP_12
                gosub no_next
DISP_12: return
        no_first
            str(pf$(1%),1%, 9%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_next
            str(pf$(1%),41%, 9%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_last
            str(pf$(1%),14%,9%)  = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prev
            str(pf$(1%),26%,12%) = " " : str(pfkeys$,4%,1%) = hex(ff)
        return

        unpack_data
            init(" ") aes_po$, aes_item$, aes_pan_no$, aes_part$, aes_descr$, ~
                      aes_qty$, aes_scan_dte1$, aes_scan_tme1$, aes_scan_qty$,~
                      aes_scan_usr$, aes_awd_dte1$, aes_awd_tme1$, aes_awd_qty$, ~
                      aes_awd_usr$, aes_rcv_dte1$, aes_rcv_tme1$, aes_recv$,  ~
                      aes_rcv_usr$, aes_serial$, status$, aes_vendor$
                                                     /* (AWD002)       */  
            aes_serial$ = str(aes_rec$,23%,8%)

            aes_po$     = str(aes_rec$,1%,16%)
            aes_item$   = str(aes_rec$,17%,3%)
            aes_pan_no$ = str(aes_rec$,20%,3%)

            get str(aes_rec$,103%,8%), using PO_4, aes_qty

            convert aes_qty to aes_qty$, pic(####.####-)

            aes_part$   = str(aes_rec$,37%,25%)
            aes_descr$  = str(aes_rec$,62%,32%) 
                                                   /* AWD Scan Data     */
            str(aes_scan_dte1$,1%,6%) = str(aes_rec$,182%,6%)
            call "DATFMTC" (aes_scan_dte1$)

            aes_scan_tme1$ = str(aes_rec$,188%,2%) & ":" & str(aes_rec$,190%,2%)
                                                   /* AES Scan Quantity */
            get str(aes_rec$,192%,8%), using PO_4, aes_scan_qty
                                                   
            convert aes_scan_qty to aes_scan_qty$, pic(####.####-)
            
            aes_scan_usr$  = str(aes_rec$,200%,3%)

                                                   /* AES Scan Data     */
            str(aes_awd_dte1$,1%,6%) = str(aes_rec$,203%,6%)
            call "DATFMTC" (aes_awd_dte1$)

            aes_awd_tme1$ = str(aes_rec$,209%,2%) & ":" & str(aes_rec$,211%,2%)
                                                   /* AWD Scan Quantity */
            get str(aes_rec$,213%,8%), using PO_4, aes_awd_qty
                                                   
            convert aes_awd_qty to aes_awd_qty$, pic(####.####-)
            
            aes_awd_usr$  = str(aes_rec$,221%,3%)

                                                   /* Receiver Post     */
            str(aes_rcv_dte1$,1%,6%) = str(aes_rec$,240%,6%)
            call "DATFMTC" (aes_rcv_dte1$)

            aes_rcv_tme1$ = str(aes_rec$,246%,2%) & ":" & str(aes_rec$,248%,2%)
                                                   /* Receiver Status   */
            aes_recv$ = str(aes_rec$,239%,1%)

            if aes_recv$ = "0" then status$ = "Label Printed "
            if aes_recv$ = "1" then status$ = "Scanned By AES"
            if aes_recv$ = "2" then status$ = "Rack Received "
                                                   
            convert aes_scan_qty to aes_scan_qty$, pic(####.####-)
            
            aes_rcv_usr$  = str(aes_rec$,250%,3%)
                                                   /* (AWD002)                */  
            aes_vendor$   = str(aes_rec$,94%,9%)
                                                   /* (AWD002)                */
        return

                                                       
        debug_screen_display                       /* (RHHTEST)               */
            if debug% = 0% then return             /* Debug Turned Off        */

               init(" ") aes_po$, aes_item$, aes_part$, aes_pan$, aes_qty$,   ~
                         aes_ord_qty$, aes_used_qty$, aes_pan$, aes_descr$,   ~
                         aes_color$, aes_cut$, aes_deliv$, aes_loc$, dd$(),   ~
                         aes_serial$, aes_pan_no$, aes_pin$

               aes_po$     = str(aes_rec$,1%,16%) 
               aes_item$   = str(aes_rec$,17%,3%)
               aes_pan_no$ = str(aes_rec$,20%,3%)
               aes_part$   = str(aes_rec$,37%,25%)
               aes_serial$ = str(aes_rec$,23%,8%)

               aes_descr$ = str(aes_rec$,62%,32%)
               aes_color$ = str(aes_rec$,135%,8%)
               aes_cut$   = str(aes_rec$,143%,8%)
               aes_loc$   = str(aes_rec$,151%,8%)
               aes_pin$   = str(aes_rec$,169%,3%)
 
                                                   /* Rack Quantity on Label */
               get str(aes_rec$,103%,8%), using PO_4, aes_qty
                                                   /* PO Ord Qty for Ln Item */ 
               get str(aes_rec$,111%,8%), using PO_4, aes_ord_qty
                                                   /* PO Ord Qty Used        */
               get str(aes_rec$,119%,8%), using PO_4, aes_used_qty
                                                   /* Raw Material Pan Size  */
               convert aes_pan to aes_pan$, pic(####.####-)
   
               convert aes_qty     to aes_qty$, pic(####.####-)

               convert aes_ord_qty to aes_ord_qty$, pic(####.####-)

               convert aes_used_qty to aes_used_qty$, pic(####.####-)

               aes_delivery$ = str(aes_rec$,31%,6%)     

               aes_deliv$    = aes_delivery$

               call "DATFMTC" (aes_deliv$)
            
            dd$(1%) = aes_item$   & " - " & aes_pan_no$

            dd$(2%) = aes_qty$ & "-" & aes_ord_qty$ & "-" & aes_used_qty$

            gosub set_keys

            accept                                                       ~
               at (01,02), "AES Rack Label Display Screen",              ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (03,02), "PO Number:",                                 ~
               at (03,25), fac(hex(84)), aes_po$                , ch(16),~
                                                                         ~
               at (04,02), "PO Item and Pan No:",                        ~
               at (04,25), fac(hex(84)), dd$(1%)                , ch(20),~
                                                                         ~
               at (05,02), "Product Delivery Date",                      ~
               at (05,25), fac(hex(84)), aes_deliv$             , ch(10),~
                                                                         ~
               at (06,02), "Raw Material Part No:",                      ~
               at (06,25), fac(hex(84)), aes_part$              , ch(25),~
                                                                         ~
               at (07,02), "Part Number Descr:",                         ~
               at (07,25), fac(hex(84)), aes_descr$             , ch(32),~
                                                                         ~
               at (08,02), "Rack-Item-used Qty:",                        ~
               at (08,25), fac(hex(84)), dd$(2%)                , ch(40),~
                                                                         ~
               at (09,02), "Product Color:",                             ~
               at (09,25), fac(hex(84)), aes_color$             , ch(08),~
                                                                         ~
               at (10,02), "Product Cut Length:",                        ~
               at (10,25), fac(hex(84)), aes_cut$               , ch(08),~
                                                                         ~                                                                         
               at (11,02), "Product Location:",                          ~
               at (11,25), fac(hex(84)), aes_loc$               , ch(08),~
                                                                         ~                                                                         
               at (12,02), "Product P/N Number:",                        ~
               at (12,25), fac(hex(84)), aes_pin$               , ch(03),~
                                                                         ~                                                                         
               at (13,02), "Primary Key:",                               ~
               at (13,25), fac(hex(84)), aes_key$               , ch(22),~
                                                                         ~                                                                         
               at (14,02), "Succ Update and Read:",                      ~
               at (14,25), fac(hex(84)), rec$                   , ch(01),~
                                                                         ~
               at (17,02), "Serial Number Assign:",                      ~
               at (17,25), fac(hex(84)), aes_serial$            , ch(08),~
                                                                         ~                                                                          
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

               stop                                    /* (RHHTEST)   */
            return

        set_keys
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
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
            msg$(2%) = "Return Code (DESPLA07) = "   
            msg$(3%) = "Press <ENTER> to Continue, <PF16> to Exit"

            convert err% to str(msg$(2%),26%,2%), pic(#0)    
L64000:     k% = 2%
            call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
                if k%  =  0% then return
                if k% <> 16% then goto L64000
            return clear all
            goto inputmode
                                                   /* (AWD002)           */
        inventory_delete
            err% = 0%
            del_qty = 0.0
            del_qty = aes_qty * -1.0               /* Reverse Quantity   */

            call "AESRCVSN" (aes_vendor$,          /* Vendor Code        */ ~
                             aes_po$,              /* P.O. Number        */ ~
                             aes_item$,            /* Purchase Order Line*/ ~
                             del_qty,              /* Aes Scan Rack Qty  */ ~
                             aes_serial$,          /* Serial Number      */ ~
                             #7,                   /* (RCVSCN)           */ ~
                             #8,                   /* (RCVSCN2)          */ ~
                             err% )                /* Error Code         */
   

           if err% <> 0% then err% = 5%            /* Inventory Add Error*/
        return
                                                   /* (AWD002)           */

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program

        end

