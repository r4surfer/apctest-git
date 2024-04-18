        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN57                             *~
            *  Creation Date     - 09/10/07                             *~
            *  Written By        - David Speight                        *~
            *                                                           *~
            *  Description       - New AES Barcode Labels               *~
            *                                                           *~
            *  Subroutine Used   - AWDPLA57 (Print Barcode Labels)      *~
            *                                                           *~
            *                                             (AWD004)      *~
            *  aes_prt_flg$   - 'A' =              'B' =                *~
            *                                                           *~
            *                                                           *~  
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *09/10/2007! (New) Label Program For AES Inventory    ! DES *~
            *          !       Labels.                            !     *~
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
            * #4  ! HNYMASTR ! Part Master File                         *~
            * #5  ! GENCODES ! Master Table File                        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

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

            call "SHOSTAT" ("Opening Files, One Moment Please")

            filename$ = "HNYMASTR" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "GENCODES" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error

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
            aes_prt_default$ = "A"                        /* (AWD005)   */ 
                                                          /* (AWD004)   */
            pour$ = "BMDH"
            sub_inv$ = "100"
            locator$ = "WHSE--"
	    qty_um$  = "EA"

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

        inputmode_1 
	    fieldcnt% = 7%     
            if aes_prt_flg$ = "A" or aes_prt_flg$ = "B" then fieldcnt% = 6%
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
            if fieldnr% > 4% then fieldnr% = fieldnr% + 1%
            if fieldnr% = 4% and cursor%(2%) > 40% then fieldnr% = fieldnr% + 1%
            if fieldnr% < 1% or fieldnr% > fieldcnt% then editpg1
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
            if aes_prt_flg$ <> "A" or aes_prt_flg$ <> "B" then return
	    if fieldnr% = 7% then enabled% = 0%
           
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
           rec% = 1% : rec$ = "1"
        return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
REM           gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40160,     /* Part Number             */~
                                L40165,     /* Max page number         */~
                                L40160,     /* Sub Inv                 */~
                                L40165,     /* quantity amount         */~
                                L40160,     /* quantity unit of measure*/~
                                L40160,     /* POUR                    */~
                                L40160      /* Printer                 */
            if aes_prt_flg$ <> "A" and aes_prt_flg$ <> "B" then goto L40190
	    lfac$(7%) = hex(84)

              goto L40190

L40155:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low    */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only  */
L40165:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric     */
                                                        /* (AWD004)    */             
L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Material Number:      ",                     ~
               at (03,20), fac(lfac$(1%)), aes_part$            , ch(15),~
               at (03,47), fac(hex(84)), aes_descr$             , ch(32),~
                                                                         ~
               at (04,02), "Pages:                ",                     ~
               at (04,25), fac(lfac$(2%)), max_page$            , ch(02),~
                                                                         ~
               at (05,02), "Sub Inv:              ",                     ~
               at (05,25), fac(lfac$(3%)), sub_inv$             , ch(25),~
                                                                         ~
               at (06,02), "Quantity:             ",                     ~
               at (06,25), fac(lfac$(4%)), qty_amt$             , ch(07),~
               at (06,40), "Unit of Measure:      ",                     ~
               at (06,65), fac(lfac$(5%)), qty_um$              , ch(03),~
                                                                         ~
               at (07,02), "POUR:                 ",                     ~
               at (07,25), fac(lfac$(6%)), pour$                , ch(06),~
                                                                         ~
               at (08,02), "Printer:",                                   ~
               at (08,25), fac(lfac$(7%)), aes_prt_flg$         , ch(01),~
               at (08,27), fac(hex(84)), aes_prt_flgd$          , ch(05),~
                                                                         ~  
               at (12,02), fac(hex(94)), spec_errormsg$         , ch(79),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 9% then goto L40400
                  return clear all
                  goto inputmode

              
L40400:  

L40420:        if keyhit% <> 15% then goto L40440
                  call "PRNTSCRN"
                  goto L40190

L40440:        

L40450:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &        ~
                      "                                        "
            pf$(2%) = "                                        " &        ~
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
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "

            on fieldnr% gosub L50075,     /* Part Number             */~
                              L50080,     /* Max page number         */~
                              L50090,     /* Sub Inv                 */~
                              L50110,     /* quantity amount         */~
                              L50120,     /* quantity unit of measure*/~
                              L50130,     /* POUR                    */~
                              L50140      /* Printer                 */

            return
                                             /* (VBKMASTR)             */
L50075: Rem Part Number                                           
        init(" ") aes_descr$
        gosub lookup_raw_material  
        if lookup_raw_material% = 0% then goto L50076

        return
L50076:     errormsg$ = "(Error) Invalid Raw Material Number?"
            gosub error_prompt
            init (" ") sc_raw$, sc_raw_d$
        return                

L50080: Rem Pages                       
        convert max_page$ to max_page%, data goto L50085
	convert max_page% to max_page$, pic(##)
	if max_page% > 0% then return

L50085:     errormsg$ = "(Error) Invalid Number of Pages?"
            gosub error_prompt
            init (" ") max_page$             
        return                

L50090: Rem Sub Inv                     
	return

L50095:     errormsg$ = "(Error) Invalid Sub Inv?"
            gosub error_prompt
            init (" ") sub_inv$             
        return                

L50100: Rem Locator                     
        return                

L50110: Rem Qty                         
        convert qty_amt$ to qty_amt%, data goto L50115 
	convert qty_amt% to qty_amt$, pic(######0)
	if qty_amt% > 0% then return

L50115:     errormsg$ = "(Error) Invalid Quantity?"
            gosub error_prompt
            init (" ") qty_amt$             
        return                

L50120: Rem Unit of Measure             
        return                

L50130: Rem POUR                        
        return                

L50140: Rem Printer         aes_prt_flg$                      
        if aes_prt_flg$ = "A" or aes_prt_flg$ = "B" then return

        errormsg$ = "(Error) Invalid printer selected {A|B}.           "
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
            lookup_raw_material% = 1%
PO_7:

        return

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
                lbl% = 2%
                convert max_page$ to lbl%, data goto page_error

create_rack_label_1:
                convert lbl% to pg_nbr$, pic(##)
                call "AWDPLA57" (pan%,                                     ~    
                        been_here%,      /* Zero (Only 1st Time)       */~
                        aes_part$,       /* part number                */~
                        aes_descr$,      /* material description       */~
                        pg_nbr$,         /* Page number    of          */~
                        max_page$,       /* total pages                */~
                        sub_inv$,        /* sub inventory number       */~
                        locator$,        /* warehouse (locator)        */~
                        qty_amt$,        /* quantity                   */~
                        qty_um$,         /* unit of measure            */~
                        pour$,           /* POUR                       */~
                        aes_prt_flg$,    /* Printer Flag '1' or '2'    */~
                        error%)          /* Return Code                */
                if error% <> 0% then gosub print_error
                lbl% = lbl% - 1%
                if lbl% > 0% then goto create_rack_label_1

                error% = 99%
                call "AWDPLA57" (pan%,                                     ~    
                        been_here%,      /* Zero (Only 1st Time)       */~
                        aes_part$,       /* part number                */~
                        aes_descr$,      /* material description       */~
                        pg_nbr$,         /* Page number    of          */~
                        max_page$,       /* total pages                */~
                        sub_inv$,        /* sub inventory number       */~
                        locator$,        /* warehouse (locator)        */~
                        qty_amt$,        /* quantity                   */~
                        qty_um$,         /* unit of measure            */~
                        pour$,           /* POUR                       */~
                        aes_prt_flg$,    /* Printer Flag '1' or '2'    */~
                        error%)          /* Return Code                */
                if error% <> 0% then gosub print_error

REM             lbl% = lbl% - 1%
REM             if lbl% > 0% then goto create_rack_label_1

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

            aes_qty$ = "   0.0000 "
            get str(aes_rec$,103%,8%), using PO_4, aes_qty

            convert aes_qty to aes_qty$, pic(####.####-)

PO_4:       FMT PD(15,4)
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

        page_error                                  
            errormsg$ = "Number of pages is invalid.  "      
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

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program

        end

