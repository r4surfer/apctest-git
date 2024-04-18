        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPL542                             *~
            *  Creation Date     - 06/27/2008                           *~
            *  Written By        - David Speight                        *~
            *                                                           *~
            *  Description       - kanban labels (zebra)                *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *08/28/2008! (New) Program For Kanban cards           ! DES *~
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
            apc$   = "(AWD) Generate Kanban Labels     "
            pname$ = "AWDPL542 - 07/02/2008"              /* (AWD004)  */

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
            * #2  ! LANDRU   ! Kanban label file (to print script)      *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,   "KANOLDMS",                                     ~
                        varc,     indexed,  recsize = 144,              ~
                        keypos =   26, keylen =   6,                    ~
                        alt key  1, keypos =   1, keylen =  31

            select #2,   "LANDRU", varc, consec,recsize=160 

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 500%, rslt$(1%))
            if err% <> 0% then gosub open_error

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

        inputmode_1 
            for fieldnr% = 1% to 1%
L10110:         gosub'051(fieldnr%)        /* Default - Enables */
                      if enabled% = 0% then L10240
L10130:         gosub'101(fieldnr%, 1%)    /* Display - Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% = 16% then exit_program
                      if keyhit% = 6% then gosub push_on_stack
                      if keyhit% = 6% and err% = 0% then gosub startover
                      if keyhit% = 7% then gosub clear_stack  
                      if keyhit% = 8% then gosub print_stack  
                      if keyhit% = 12% then gosub inputmode_2 
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
            fieldnr% = 1%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% = 6% then gosub push_on_stack
                  if keyhit% = 6% and err% = 0% then gosub startover
                  if keyhit% = 7% then gosub clear_stack  
                  if keyhit% = 8% then gosub print_stack  
                  if keyhit% = 16% then exit_program
                      if keyhit% = 12% then gosub inputmode_2 
                  if keyhit% <>  0% then       editpg1
            goto L11180
L11120:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 2% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                      if keyhit% = 6% then gosub push_on_stack
                      if keyhit% = 6% and err% = 0% then gosub startover
                      if keyhit% = 7% then gosub clear_stack  
                      if keyhit% = 8% then gosub print_stack  
                      if keyhit% = 12% then gosub inputmode_2 
                  if keyhit% = 6% then gosub push_on_stack
                  if keyhit% = 6% and err% = 0% then gosub startover
                  if keyhit% = 7% then gosub clear_stack  
                  if keyhit% = 8% then gosub print_stack  
                  if keyhit% = 16% then exit_program
                      if keyhit% = 12% then gosub inputmode_2 
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
L11180:
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
REM               if errormsg$ <> " " then L11170
                  if errormsg$ <> " " then gosub startover
                  lastfieldnr% = fieldnr%
            if keyhit% = 16% then exit_program
            goto L11170
                                                            
        inputmode_2 
	    init(" ") aes_part$, aes_seq$, aes_desc$, aes_pour$, aes_qty$
            for fieldnr% = 1% to 2%
L20110:         gosub'052(fieldnr%)        /* Default - Enables */
                      if enabled% = 0% then L20240
L20130:         gosub'102(fieldnr%, 1%)    /* Display - Accept  */
                      if keyhit%  =  1% then goto inputmode_2
                      if keyhit% <> 16% then goto L20140 
                      keyhit% = 0%
		      return
L20140:
                      if keyhit% <>  4% then       L20215
L20160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L20130
                         if fieldnr% = 1% then L20110
                         goto L20160
L20215:               if keyhit% <> 16% then goto L20220 
                      keyhit% = 0%
		      return
L20220:               if keyhit% <> 0% then       L20130
L20240:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if keyhit% = 16% then return          
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then goto inputmode_2
                  if keyhit% <> 16% then goto L21110 
                  keyhit% = 0%
		  return
L21110:           if keyhit% <>  0% then       editpg2
L21120:     fieldnr% = cursor%(1%) - 1%
            if fieldnr% < 1% or fieldnr% > 2% then editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L21170:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then goto inputmode_2
                  if keyhit% <>  0% then L21170
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L21170
                  lastfieldnr% = fieldnr%
            if keyhit% <> 16% then goto L21120 
            keyhit% = 0%
            return

browse_next:
        init(" ") aes_key$
	aes_key$ = aes_part$        
	str(aes_key$,26,6) = aes_seq$       
   	read #1, key 1% > aes_key$, USING KANOLDFMT,              ~  
	           part_nbr$,seq_nbr$, part_desc$, pour$, qty$,             ~ 
	           u_m$,sub_inv$, filler$,    ~ 
		   eod goto L21175
        aes_part$ = part_nbr$       
        aes_seq$  = seq_nbr$       
        aes_pour$ = pour$           
        aes_desc$ = part_desc$      
        aes_qty$  = qty$            
        aes_u_m$  = u_m$            
        aes_sub_inv$ = sub_inv$        

L21175:
	    return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
	    if fieldnr% <> 1% then enabled% = 0%
        return

        deffn'052(fieldnr%)
            enabled% = 1%
	    if fieldnr% < 1% or fieldnr% > 2% then enabled% = 0%
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
            if scrnr% = 2% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return
                                                     /* (AWD004)    */  
        scrn1_msg  :  data                                               ~
         "Enter a Sequence Number?                                             "

        scrn2_msg  :  data                                               ~
         "Enter a Part Number?                                                 ",~
         "Enter a Sequence Number?                                             "

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
 
	push_on_stack
	    err% = 0%
	    if queue_cnt% >= 72% then stack_overflow
            queue_cnt% = queue_cnt% + 1%
	    queue$(queue_cnt%) = aes_seq$
            return

stack_overflow:
            queue_remark$ = "Queue size exceeded, no more than 72 allowed."
	    err% = 1%
            return

stack_mismatch:
            queue_remark$ = "Queue must be all one color.                 "
	    err% = 1%
            return

	clear_stack
	    init(" ") queue$()
            gosub initialize_variables
            queue_cnt% = 0%
            queue_remark$ = "Color determined by first card in queue."
            return

	print_stack
            gosub create_rack_label
	    gosub clear_stack
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
              gosub set_pf1
	      gosub L40165
	      queue_msg$ = "000 labels to be printed."
	      convert queue_cnt% to str(queue_msg$,1,3), pic (##0)     
L40150:
REM           on fieldnr% gosub L40170,     /* Part Number             */~
                                L40165      /* Sequence Number         */ 
              goto L40190

L40155:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low    */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only  */
L40165:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric     */
L40170:           lfac$(fieldnr%) = hex(84)  :  return  /* Numeric     */
                                                        /* (AWD004)    */             
L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Material Number:      ",                     ~
               at (03,20), fac(hex(84)), aes_part$            , ch(25),~
                                                                         ~
               at (04,02), "Sequence Number:      ",                     ~
               at (04,20), fac(lfac$(1%)), aes_seq$             , ch(06),~
                                                                         ~
               at (05,02), "Material Description: ",                     ~
               at (05,20), fac(hex(84)), aes_desc$             , ch(55),~
                                                                         ~
               at (06,02), "Quantity:             ",                     ~
               at (06,20), fac(hex(84)), aes_qty$             , ch(07),~
                                                                         ~
               at (07,02), "Unit of Measure:      ",                     ~
               at (07,20), fac(hex(84)), aes_u_m$             , ch(03),~
                                                                         ~
               at (08,02), "POUR:                 ",                     ~
               at (08,25), fac(hex(84)), aes_pour$            , ch(06),~
                                                                         ~
               at (09,02), "Sub. Inv.:            ",                     ~
               at (09,25), fac(hex(84)), aes_sub_inv$         , ch(11),~
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
            pf$(1%) = "(1)Start Over         (6) Select to prin" &        ~
                      "t               (12) Search part number"
            pf$(2%) = "                      (7) Clear selected" &        ~
                      "                (15) Print Screen      "
            pf$(3%) = "                      (8) Print selected" &        ~
                      "                (16) EXIT              "
            pfkeys$ = hex(01ffffffff060708ffffff0cff0e0f1000)
            return
L40700:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                (12) Search part number"
            pf$(2%) = "                                        " &        ~
                      "                                       "
            pf$(3%) = "                      (8) Print selected" &        ~
                      "                (16) EXIT              "
            pfkeys$ = hex(01ffffffffffff08ffffff0cffff0f1000)
            return
  
        REM *************************************************************~
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'050(2%, fieldnr%)
              gosub set_pf2 : lfac$ = hex(84) : mod% = 0%
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
L42150:
              on fieldnr% gosub L40160,     /* Part Number             */~
                                L40165      /* Sequence Number         */ 
              goto L42190

L42190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Material Number:      ",                     ~
               at (03,20), fac(lfac$(1%)), aes_part$            , ch(25),~
                                                                         ~
               at (04,02), "Sequence Number:      ",                     ~
               at (04,20), fac(lfac$(2%)), aes_seq$             , ch(06),~
                                                                         ~
               at (05,02), "Material Description: ",                     ~
               at (05,20), fac(hex(84)), aes_desc$             , ch(55),~
                                                                         ~
               at (06,02), "Quantity:             ",                     ~
               at (06,20), fac(hex(84)), aes_qty$             , ch(07),~
                                                                         ~
               at (07,02), "POUR:                 ",                     ~
               at (07,25), fac(hex(84)), aes_pour$            , ch(06),~
                                                                         ~
               at (12,02), fac(hex(94)), spec_errormsg$         , ch(79),~
                                                                         ~
               at (19,02), "BROWSE KANBAN CARD MASTER FILE",             ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 9% then goto L42400
                  return clear all
                  goto inputmode
L42400:  

L42420:        if keyhit% <> 15% then goto L42440
                  call "PRNTSCRN"
                  goto L42190

L42440:         
               if keyhit% = 8% then gosub browse_next 
L42450:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2

L42610: 
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                (8) Next Record        "
            pf$(2%) = "                                        " &        ~
                      "                (15) Print Screen      "
            pf$(3%) = "                                        " &        ~
                      "                (16) RETURN            "
            pfkeys$ = hex(01ffffffffffff08ffffffffff0e0f1000)
            return
  
        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            gosub L50090
            return
                                             /* (VBKMASTR)             */
        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50080,     /* Part Number             */~
                              L50090      /* Sequence number         */ 
            return

L50080: Rem Part Number                                           
        init(" ") aes_descr$
	aes_key$ = aes_part$ & "            " 
   	     read #1,key 1% >= aes_key$, USING KANOLDFMT,              ~  
	           part_nbr$,seq_nbr$, part_desc$, pour$, qty$,             ~ 
	           u_m$,sub_inv$, filler$,    ~ 
		   eod goto L50085
        aes_part$ = part_nbr$       
        aes_seq$  = seq_nbr$       
        aes_desc$ = part_desc$      
        aes_pour$ = pour$           
        aes_qty$  = qty$            
        aes_u_m$  = u_m$            
        aes_sub_inv$ = sub_inv$        
KANOLDFMT: FMT CH(25), CH(06), CH(64), CH(16), CH(06), CH(3), CH(11), CH(13)   
        if str(aes_key$,1,25) <> aes_part$ then goto L50085
        return

L50085:     errormsg$ = "(Error) Invalid Part Number?"
            gosub error_prompt
        return                

L50090: Rem Part Number                                           
        init(" ") aes_descr$
   	     read #1,key 0% = aes_seq$, USING KANOLDFMT,              ~  
	           part_nbr$,seq_nbr$, part_desc$, pour$, qty$,             ~ 
	           u_m$,sub_inv$, filler$,    ~ 
		   eod goto L50095
        aes_part$ = part_nbr$       
        aes_seq$  = seq_nbr$       
        aes_desc$ = part_desc$      
        aes_pour$ = pour$           
        aes_qty$  = qty$            
        aes_u_m$  = u_m$            
        aes_sub_inv$ = sub_inv$        
        return
L50095:     errormsg$ = "(Error) Invalid Sequence Number?"
            gosub error_prompt
        return                


        create_rack_label
            debug%     = 0%                           /* 0% = Off      */
            rec_fnd = 0
                                                      /* 1% = On       */
            been_here% = 0%

            call "SHOSTAT" ("Creating and Printing Rack Labels")
            init(" ") aes_key$

            init(" ") file$, script$
            file$   = "MFGKANOL"      
             script$ = "MFGKANOL"      

            library$        = "APCDATA "
            volume$         = "CARLOS"
            call "OPENFILE" (#2, "IO   ", f2%(2%), rslt$(2%), axd$ )
            if f2%(2%) <> 0% then goto L01100
               gosub file_exists         
               if comp% <> 16% then return  
                  call "FILEBGON" (#2)

L01100:    open nodisplay #2, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
           for l% = 1% to queue_cnt%
             aes_seq$ = queue$(l%)
   	     read #1,key 0% = aes_seq$, USING KANOLDFMT,              ~  
	           part_nbr$,seq_nbr$, part_desc$, pour$, qty$,             ~ 
	           u_m$,sub_inv$, filler$,    ~ 
		   eod goto skip_write
		   if aes_seq$ <= "      " then goto skip_write
             init(" ") seq_rec$
             seq_rec$ = part_nbr$ & "|" & part_desc$ & "|" & u_m$ &  ~
			"|" & qty$ & "|" & pour$ &                  ~
			"|" & sub_inv$ & "|" & seq_nbr$ & "||"
             write #2, using SEQFMT, str(seq_rec$,1,128)
SEQFMT: FMT CH(128)
	   next l%
skip_write:
/* print */
            close #2
            script$ = "MFGKANOL"      
            call "LINK" addr(script$, lb1%, lb2%)
            if lb1%    > 0% then error% = 4%

            call "FILEBGON" (#2)          /* Scratch 'MFGDES'         */
        return

        return clear all
        goto inputmode

        file_exists
          comp% = 2%     
          hdr$ = "***  New Kanban Barcode Label ***"
          msg$(1%) = "       The File (KANBAN) Already Exists.         "
          msg$(2%) = "     New  AES B A R C O D E   L a b e l s        "
          msg$(3%) = "Press <RETURN> To Exit Prog, or PF(16) to Delete."
          call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        open_error                                  
            errormsg$ = "(Open Error) - File = " & filename$
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

