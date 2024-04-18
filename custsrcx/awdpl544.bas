        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPL544                             *~
            *  Creation Date     - 06/27/2008                           *~
            *  Written By        - David Speight                        *~
            *                                                           *~
            *  Description       - kanban labels                        *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *02/09/2009! (New) Program For editing kanoldms       ! DES *~
            *************************************************************

        dim                                                              ~
            queue$(72)6, queue_msg$79,   /* print queue                */~
            queue_remark$79, seq_rec$255, /* print queue               */~
            filename$8, aes_key$31,      /* Used by EWDOPEN            */~ 
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
            aes_part$25,                 /* Part Number (Raw Materia)  */~		 
            part_desc$64,                /* Part Number Description    */~
            aes_desc$64,                /* Part Number Description    */~
            aes_qty$10,                  /* Label Quantity Pan Size    */~			
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
            apc$   = "(AWD) Edit Kanban File           "
            pname$ = "AWDPL544 - 02/09/2009"              /* (AWD004)  */

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
            * #1  ! KANOLDMS ! Kanban Master File                       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,   "KANOLDMS",                                     ~
                        varc,     indexed,  recsize = 144,              ~
                        keypos =   26, keylen =   6,                    ~
                        alt key  1, keypos =   1, keylen =  31

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 500%, rslt$(1%))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            time$ = time
	    init(" ") queue$()
            queue_cnt% = 0%
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            queue_remark$ = "Color determined by first card in queue."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            for fieldnr% = 1% to 7%
L10110:         gosub'051(fieldnr%)        /* Default - Enables */
                      if enabled% = 0% then L10240
L10130:         gosub'101(fieldnr%, 1%)    /* Display - Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% = 16% then exit_program
                  if keyhit% =  6% then save_data        
                  if keyhit% =  8% then delete_rec       
                      if keyhit% <>  4% then       L10215
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10215:               if keyhit% = 16% then exit_program
                      if keyhit% <> 0% then       L10130
L10240:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                  if errormsg$ <> " " then gosub startover
                      if keyhit% = 16% then exit_program
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
                  if keyhit% = 16% then exit_program
                  if keyhit% =  6% then save_data        
                  if keyhit% =  8% then delete_rec       
                  if keyhit% <>  0% then       editpg1
            goto L11180
L11120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 7% then editpg1
            if fieldnr% = lastfieldnr% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit% = 16% then exit_program
                  if keyhit% =  6% then save_data        
                  if keyhit% =  8% then delete_rec       
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170                
L11180:
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11170
                  lastfieldnr% = fieldnr%
            if keyhit% = 16% then exit_program
            goto L11120
                                                            
delete_rec:  
	str(aes_key$,26,6) = aes_seq$       
   	read #1, key 0% = aes_seq$, hold, USING KANOLDFMT,              ~  
	           part_nbr$,seq_nbr$, part_desc$, pour$, qty$,         ~ 
	           u_m$,sub_inv$, filler$,    ~ 
		   eod goto L20050
        delete #1
        gosub startover
L20050:	return

save_data:   
        init(" ") aes_key$
	aes_key$ = aes_part$        
	str(aes_key$,26,6) = aes_seq$       
   	read #1, key 0% = aes_seq$, hold, USING KANOLDFMT,              ~  
	           part_nbr$,seq_nbr$, part_desc$, pour$, qty$,         ~ 
	           u_m$,sub_inv$, filler$,    ~ 
		   eod goto L21175
        delete #1
L21175:
        part_nbr$  = aes_part$        
        seq_nbr$   = aes_seq$         
        pour$      = aes_pour$            
        part_desc$ = aes_desc$       
        qty$       = aes_qty$             
        u_m$       = aes_u_m$             
        sub_inv$   = aes_sub_inv$        
	filler$   = "    "
   	write #1, USING KANOLDFMT,              ~  
	           part_nbr$,seq_nbr$, part_desc$, pour$, qty$,         ~ 
	           u_m$,sub_inv$, filler$ 
        gosub startover
        return


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
                                                     /* (AWD004)    */  
        scrn1_msg  :  data                                               ~
         "Enter a Sequence Number?                                             ",~
         "Enter a Part Number?                                                 ",~
         "Enter a Part Description?                                            ",~
         "Enter a Quantity?                                                    ",~
         "Enter a Unit of Measure?                                             ",~
         "Enter a POUR?                                                        ",~
         "Enter a Sub Inventory?                                               "

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *************************************************************

        startover
            u3% = 2%
REM         call "STARTOVR" (u3%)
REM         if u3% = 1% then return
            return clear all
            goto inputmode

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, aes_part$, aes_desc$,      ~
                      aes_seq$, aes_pour$, aes_qty$, aes_u_m$,           ~
		      aes_sub_inv$
            init(" ") queue_remark$ 

        return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        REM dataload
 
        REM return
 

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
              on fieldnr% gosub L40165,     /* Sequence Number         */~
                                L40160,     /* Part Number             */~
                                L40160,     /* Part Description        */~
                                L40165,     /* Quantity                */~
                                L40160,     /* Unit of Measure         */~
                                L40160,     /* POUR                    */~
                                L40160      /* Sub Inv                 */ 
              goto L40190

L40155:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low    */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only  */
L40165:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric     */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(8c)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Sequence Number:      ",                     ~
               at (03,20), fac(lfac$(1%)), aes_seq$             , ch(06),~
                                                                         ~
               at (04,02), "Part Number:          ",                     ~
               at (04,20), fac(lfac$(2%)), aes_part$            , ch(25),~
                                                                         ~
               at (05,02), "Part Description:     ",                     ~
               at (05,20), fac(lfac$(3%)), aes_desc$             , ch(55),~
                                                                         ~
               at (06,02), "Quantity:             ",                     ~
               at (06,20), fac(lfac$(4%)), aes_qty$             , ch(07),~
                                                                         ~
               at (07,02), "Unit of Measure:      ",                     ~
               at (07,20), fac(lfac$(5%)), aes_u_m$             , ch(03),~
                                                                         ~
               at (08,02), "POUR:                 ",                     ~
               at (08,25), fac(lfac$(6%)), aes_pour$            , ch(06),~
                                                                         ~
               at (09,02), "Sub. Inv.:            ",                     ~
               at (09,25), fac(lfac$(7%)), aes_sub_inv$         , ch(11),~
                                                                         ~
               at (12,02), fac(hex(94)), spec_errormsg$         , ch(79),~
                                                                         ~
               at (19,02), fac(hex(a4)),   queue_msg$           , ch(79),~
               at (20,02), fac(hex(a4)),   queue_remark$        , ch(79),~
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

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over         (6) Save          " &        ~
                      "                                       "
            pf$(2%) = "                      (8) Delete        " &        ~
                      "                (15) Print Screen      "
            pf$(3%) = "                                        " &        ~
                      "                (16) EXIT              "
            pfkeys$ = hex(01ffff04ff06ff08ffffffffff0e0f1000)
            return
L40700:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                      (8) Delete        " &        ~
                      "                                       "
            pf$(3%) = "                                        " &        ~
                      "                (16) EXIT              "
            pfkeys$ = hex(01ffff04ffffff08ffffffffffff0f1000)
            return
  
        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50080,     /* Sequence Number         */~
                              L50070,     /* Part Number             */~
                              L50090,     /* Part Description        */~
                              L50100,     /* Quantity                */~
                              L50110,     /* Unit of Measure         */~
                              L50120,     /* Pour                    */~
                              L50130      /* Sub Inv                 */
            return

L50070: Rem Part Number                                           
        return
L50075:     errormsg$ = "(Error) Invalid Sequence Number?"
            gosub error_prompt
        return                


        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

L50080: Rem Sequence Number                                       
        init(" ") aes_descr$
        convert aes_seq$ to aes_seq%, data goto L50085
        if aes_seq% < 5000% then goto L50085

   	read #1,key 0% = aes_seq$, USING KANOLDFMT,              ~  
	           part_nbr$,seq_nbr$, part_desc$, pour$, qty$,             ~ 
	           u_m$,sub_inv$, filler$,    ~ 
		   eod goto KANOLDFMT 
        aes_part$ = part_nbr$       
        aes_seq$  = seq_nbr$       
        aes_desc$ = part_desc$      
        aes_pour$ = pour$           
        aes_qty$  = qty$            
        aes_u_m$  = u_m$            
        aes_sub_inv$ = sub_inv$        
KANOLDFMT: FMT CH(25), CH(06), CH(64), CH(16), CH(06), CH(3), CH(11), CH(13)   
        return

L50085:     errormsg$ = "(Error) Invalid Seq No?"
            gosub error_prompt
        return                

L50090: Rem Part Description                                      
        return

L50100: Rem Quantity                                              
        convert aes_qty$ to aes_qty, data goto L50105
REM     if aes_qty < 1.00 then goto L50105
        return
L50105:     errormsg$ = "(Error) Invalid Quantity!"
            gosub error_prompt
        return                

L50110: Rem Unit of Measure                                       
        if aes_u_m$ = "FT" then return 
        if aes_u_m$ = "EA" then return 
        if aes_u_m$ = "GA" then return 
        if aes_u_m$ = "RL" then return 
        if aes_u_m$ = "LB" then return 
        if aes_u_m$ = "ST" then return 
        if aes_u_m$ = "BOX" then return 
REM     return
L50115:     errormsg$ = "(Error) Invalid Unit of Measure!"
            gosub error_prompt
        return                

L50120: Rem Pour                                                  
        if aes_pour$ <= "    " then goto L50125
        return
L50125:     errormsg$ = "(Error) Invalid POUR!"
            gosub error_prompt
        return                

L50130: Rem Sub Inv                                               
        if aes_sub_inv$ <= "    " then goto L50125
        return
L50135:     errormsg$ = "(Error) Invalid Sub Inventory!"
            gosub error_prompt
        return                

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program

        end

