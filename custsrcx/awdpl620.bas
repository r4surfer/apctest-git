        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPL620                             *~
            *  Creation Date     - 12/13/2010                           *~
            *  Written By        - David Speight                        *~
            *                                                           *~
            *  Description       - rack labels (zebra)                  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *12/13/2010! (New) Program For Rack labels            ! DES *~
            *************************************************************

        dim                                                              ~
            queue$(72)6, queue_msg$79,   /* print queue                */~
            queue_remark$79, seq_rec$25, /* print queue               */~
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
        rack_tbl$(110)8,                                             ~
        arrowtbl$(110)1,                                             ~
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
            apc$   = "(AWD) Generate Rack Labels       "
            pname$ = "AWDPL620 - 07/02/2008"              /* (AWD004)  */

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
            * #2  ! MFGRACKL ! Kanban label file (to print script)      *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #2, "LANDRU", varc, consec, recsize=16                  

            call "SHOSTAT" ("Opening Files, One Moment Please")


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
            queue_remark$ = "                                        "

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

        inputmode_1 
            for fieldnr% = 1% to 4%
L10110:         gosub'051(fieldnr%)        /* Default - Enables */
                      if enabled% = 0% then L10240
L10130:         gosub'101(fieldnr%, 1%)    /* Display - Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% = 16% then exit_program
                      if keyhit% = 6% then gosub push_on_stack
                      if keyhit% = 6% and err% = 0% then gosub startover
                      if keyhit% = 7% then gosub clear_stack  
                      if keyhit% = 8% then gosub print_stack  
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
                  if keyhit% <>  0% then       editpg1
            goto L11180
L11120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 4% then editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11170:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                      if keyhit% = 6% then gosub push_on_stack
                      if keyhit% = 6% and err% = 0% then gosub startover
                      if keyhit% = 7% then gosub clear_stack  
                      if keyhit% = 8% then gosub print_stack  
                  if keyhit% = 6% then gosub push_on_stack
                  if keyhit% = 6% and err% = 0% then gosub startover
                  if keyhit% = 7% then gosub clear_stack  
                  if keyhit% = 8% then gosub print_stack  
                  if keyhit% = 16% then exit_program
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11170
L11180:
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
REM               if errormsg$ <> " " then L11170
                  if errormsg$ <> " " then gosub startover
                  lastfieldnr% = fieldnr%
            if keyhit% = 16% then exit_program
            goto L11170
                                                            
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
            restore line = scrn1_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return
                                                     /* (AWD004)    */  
        scrn1_msg  :  data                                               ~
         "Enter a Row Number?                                            ", ~
         "Enter a Rack Number?                                           ", ~
         "Enter a Bin Number? (or '*' for all bins)                      ", ~
         "Enter a Arror Direction? (L-Left, R-Right, ' '-No Arrow)       "

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
              aes_sub_inv$, bin$, arrow$
REM                   aes_sub_inv$, row$, rack$, bin$, arrow$
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
        if queue_cnt% >= 99% then stack_overflow
            queue_cnt% = queue_cnt% + 1%
        temp$ = str(row$,1,1)
        if (str(row$,2,1) >= "0") then temp$ = temp$ & str(row$,2,1)
        if (str(row$,3,1) > " ") then temp$ = temp$ & str(row$,3,1)
        temp$ = temp$ & "-"           
        if (str(rack$,1,1) > "0") then temp$ = temp$ & str(rack$,1,1)
        if (str(rack$,2,1) > " ") then temp$ = temp$ & str(rack$,2,1)
        temp$ = temp$ & "-"           
        if (bin$ = "*") then goto multiple_bins  
        if (bin$ > "0") then temp$ = temp$ & bin$
        rack_tbl$(queue_cnt%) = temp$       
        arrowtbl$(queue_cnt%) = arrow$
            return

multiple_bins: 
        rack_tbl$(queue_cnt%) = temp$ & "1"      
        arrowtbl$(queue_cnt%) = arrow$
            queue_cnt% = queue_cnt% + 1% 
        rack_tbl$(queue_cnt%) = temp$ & "2"      
        arrowtbl$(queue_cnt%) = arrow$
            queue_cnt% = queue_cnt% + 1% 
        rack_tbl$(queue_cnt%) = temp$ & "3"      
        arrowtbl$(queue_cnt%) = arrow$
            queue_cnt% = queue_cnt% + 1% 
        rack_tbl$(queue_cnt%) = temp$ & "4"      
        arrowtbl$(queue_cnt%) = arrow$
            queue_cnt% = queue_cnt% + 1% 
        rack_tbl$(queue_cnt%) = temp$ & "5"      
        arrowtbl$(queue_cnt%) = arrow$
            queue_cnt% = queue_cnt% + 1% 
        rack_tbl$(queue_cnt%) = temp$ & "6"      
        arrowtbl$(queue_cnt%) = arrow$
            queue_cnt% = queue_cnt% + 1% 
        rack_tbl$(queue_cnt%) = temp$ & "7"      
        arrowtbl$(queue_cnt%) = arrow$
            queue_cnt% = queue_cnt% + 1% 
        rack_tbl$(queue_cnt%) = temp$ & "8"      
        arrowtbl$(queue_cnt%) = arrow$
            queue_cnt% = queue_cnt% + 1% 
        rack_tbl$(queue_cnt%) = temp$ & "9"      
        arrowtbl$(queue_cnt%) = arrow$
            return

stack_overflow:
            queue_remark$ = "Queue size exceeded, no more than 72 allowed."
        err% = 1%
            return

row_error:
            queue_remark$ = "Invalid Row Entered.                         "
        err% = 1%
            return

rack_error:
            queue_remark$ = "Invalid Rack Entered.                         "
        err% = 1%
            return

bin_error:
            queue_remark$ = "Invalid Bin Entered.                         "
        err% = 1%
            return

    clear_stack
        init(" ") queue$()
            gosub initialize_variables
            queue_cnt% = 0%
            queue_remark$ = "                                        "
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
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
          queue_msg$ = "000 labels to be printed."
          convert queue_cnt% to str(queue_msg$,1,3), pic (##0)     
L40150:
              on fieldnr% gosub L40160,     /* Row                     */~
                                L40165,     /* Rack                    */~
                                L40160,     /* Bin                     */~
                                L40160      /* Arrow                   */ 
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
               at (03,02), "Row Number:           ",                     ~
               at (03,20), fac(lfac$(1%)), row$             , ch(03),~
               at (03,24), "(A12-Zone + number)",                        ~
                                                                         ~
               at (04,02), "Rack Number:          ",                     ~
               at (04,20), fac(lfac$(2%)), rack$            , ch(02),~
                                                                         ~
               at (05,02), "Bin Number:           ",                     ~
               at (05,20), fac(lfac$(3%)), bin$             , ch(01),~
               at (05,24), "1-9 for bin, * for all bins in rack.",       ~
                                                                         ~
               at (06,02), "Arror Direction:      ",                     ~
               at (06,20), fac(lfac$(4%)), arrow$           , ch(01),~
               at (06,24), "(' '-None, 'L'-Left, 'R'-Right)",            ~
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
                      "t                                      "
            pf$(2%) = "                      (7) Clear selected" &        ~
                      "                (15) Print Screen      "
            pf$(3%) = "                      (8) Print selected" &        ~
                      "                (16) EXIT              "
            pfkeys$ = hex(01ffffffff060708ffffffffff0e0f1000)
            return
L40700:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                      (4) Prev Field    " &        ~
                      "                                       "
            pf$(3%) = "                      (8) Print selected" &        ~
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
            on fieldnr% gosub L50080,     /* Row Number              */~
                              L50090,     /* Rack Number             */~
                              L50100,     /* Bin Number              */~
                              L50110      /* Arrow Direction         */ 
            return

L50080: Rem Row Number                                           
REM      A12-34-5
        if str(row$,1,1) < "A" or str(row$,1,1) > "Z" then goto L50085   
        if str(row$,2,1) < "0" or str(row$,2,1) > "9" then goto L50085   
        if str(row$,3,1) = " " then return   
        if str(row$,3,1) < "0" or str(row$,3,1) > "9" then goto L50085   

        return

L50085:     errormsg$ = "(Error) Invalid Row Number?"
            gosub error_prompt
        return                

L50090: Rem Rack Number                                           
REM      A12-34-5
        if rack$ = " " then return   
        if str(rack$,1,1) < "0" or str(rack$,1,1) > "9" then goto L50095   
        if str(rack$,2,1) = " " then return   
        if str(rack$,2,1) < "0" or str(rack$,2,1) > "9" then goto L50095   
        return

L50095:     errormsg$ = "(Error) Invalid Rack Number?"
            gosub error_prompt
        return                

L50100: Rem Bin Number                                           
REM      A12-34-5
        if str(bin$,1,1) = " " then return   
        if str(bin$,1,1) = "*" and rack$ > " " then return   
        if str(bin$,1,1) < "1" or str(bin$,1,1) > "9" then goto L50105   
        return

L50105:     errormsg$ = "(Error) Invalid Bin Number?"
            gosub error_prompt
        return                

L50110: Rem Arrow Direction                                       
        if arrow$ = " " then return
        if arrow$ = "R" then return
        if arrow$ = "L" then return

L50115:     errormsg$ = "(Error) Invalid Arrow Direction?"
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
            file$   = "MFGRACKL"      
             script$ = "MFGRACK"      
            volume$  = "CARLOS"
            library$ = "APCDATA "
            call "OPENFILE" (#2, "IO   ", f2%(2%), rslt$(2%), axd$ )
            if f2%(2%) <> 0% then goto L01100
               gosub file_exists         
               if comp% <> 16% then return  
                  call "FILEBGON" (#2)

L01100:    open nodisplay #2, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
           for l% = 1% to queue_cnt%
             init(" ") seq_rec$
             seq_rec$ = rack_tbl$(l%) & "," & arrowtbl$(l%) & ",            "  
             write #2, using SEQFMT, str(seq_rec$,1,16)
SEQFMT: FMT CH(16)
       next l%
skip_write:
/* print */
            close #2
            script$ = "MFGRACK"      
        lb1% = 0%
        lb2% = 0%
            call "LINK" addr(script$, lb1%, lb2%)
            if lb1%    > 0% then error% = 4%

            call "FILEBGON" (#2)          /* Scratch 'MFGDES'         */
            queue_cnt% = 0%
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

