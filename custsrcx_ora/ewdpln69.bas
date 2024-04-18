        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDPLN69                             *~
            *  Creation Date     - 03/03/99                             *~
            *  Last Mod Date     - 06/09/99                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - (GDI) Special Glass Re-Make System   *~
            *                      for Trauma Center, display and labels*~
            *                                                           *~
            *  Code Tables Used  - (PLAN REMK)                          *~
            *                                                           *~
            *  Subroutine Used   - (EWDPLA69) Print Re-make Label       *~
            *                                                           *~
            *  Special Comments  -                                      *~ 
            *                                                           *~
            *                    - Only Area (1) will print the label   *~
            *                      and create the data for display in   *~
            *                      the other area's.                    *~
            *                                                           *~  
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/11/99 ! (New) Program                            ! RHH *~
            * 03/22/99 ! (EWD001) - Mod to print a single Barcode ! RHH *~
            *          !    label                                 !     *~
            * 06/08/99 ! (EWD002) - Mod to correct glass re-make  ! RHH *~
            *          !    problem. Problem with update to       !     *~
            *          !    (APCPLNGR).                           !     *~
            * 06/09/99 ! (EWD003) - Mod to create a Purge Data    ! RHH *~
            *          !    Utility. Hidden PF(22) Key            !     *~      
            *************************************************************

        dim                                                              ~
            sc_code$2, sc_code_d$40,     /* Search Selection Code Area */~
            sc_shft$2, sc_shft_d$30,     /* Shift Selection            */~
            sc_user$3, sc_user_d$30,     /* User Who Completed         */~
            tt_in$8, tt_out$8,           /* Start and Complete Times   */~
            tt_hr$3, tt_mn$2,            /* Calculated Hours & Minutes */~  
            ss$(10%)45,                  /* Selection Locations        */~
            gt_stat$1,                   /* 0 = Open, 1 = Closed       */~
            gt_area$2, area$20,          /* Glass area Location 0=Orig */~
            gt_dte$6,                    /* Date Scanned (In) as Rmk   */~
            gt_time$8, sc_time$8,        /* Time Scanned (In) as Rmk   */~
            gt_barcode$9, barcode$9,     /* Glass Barcode              */~
            gt_model$3,                  /* Model Code                 */~
            gt_dept$3,                   /* Department Code            */~
            gt_view$3,                   /* View Top/Bot               */~
            gt_color$6,                  /* Product Color              */~
            gt_grid$7,                   /* Grid Value                 */~
            gt_cut_w$9,                  /* Glass Cut Width            */~
            gt_cut_h$9,                  /* Glass Cut Height           */~
            gt_ty$4,                     /* Glass Type Desc            */~
            gt_seq$5,                    /* Department Production Seq  */~
            gt_so$8,                     /* Customer Sales Order No    */~
            gt_cust$9,                   /* Customer Code              */~
            gt_prod$10,                  /* Production Date Formatted  */~
            gt_num$3,                    /* Glass Remake No            */~
            gt_label$1,                  /* Label Printed (Y)es or (N)o*/~
            gt_win_w$7,                  /* Actual Window Width        */~
            gt_win_h$6,                  /* Actual Window Height       */~
            gt_shft$2,                   /* Production Shift Code (In) */~
            gt_userid$3,                 /* Production User Scanned(In)*/~
            gt_contour$1,                /* Flag 'C'                   */~
            gt_text$40, text$79,         /* Glass Text String          */~
            gt_dte_o$6,                  /* Date Out Completed by Area */~
            gt_time_o$8,                 /* Time Out Completed by Area */~
            gt_spacer$4,                 /* Spacer Size                */~  
            gt_shft_o$3,                 /* Shift Code Out Comp by Area*/~
            gt_userid_o$3,               /* User Id Out Comp by Area   */~
            gt_complete$6,               /* Time to Complete hhh-mm    */~
            gt_filler$10,                /* Filler Area                */~
            gt_key$26, gt_rec$200,       /* Primary Key                */~
            sav_key$26,                  /* Use to Check Orig          */~
                                         /* (End) (EWDPLNGT)           */~ 
            h1$3, h2$3, h3$1,  h4$2,     /* Display Headers            */~
            h5$7, h6$9, h7$9, h8$4,      /*                            */~
            h9$5, h10$8, h11$8, h12$2,   /*                            */~
            h13$1, h14$2,                /*                            */~
            brass$1,                     /* Grid - Contour/Brass       */~ 
            title$37,                    /*                            */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            rm_key$12, rm_rec$256,       /* Remake Data                */~
            rm_st_time$8,                /* Time of Update             */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$10,                     /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            dsp_msg$79,                  /* Screen Display Message     */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim dt$(140%)75, cc$(140%)1,     /* Analysis Display           */~
            dt%(5%), gt_k$(140%)26       /* totals for Each Area       */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$35, pname$21
            apc$   = "Special Trauma Center Glass Utility"
            pname$ = "EWDPLN69 - Rev: R7.00"

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
            * #1  ! EWDPLNGT ! Special Re-Make Glass file for Trauma Ctr*~
            * #2  ! GENCODES ! System Master Code Table Files           *~
            * #3  ! USERCLMS ! Caelus Master User Def. (USERLCMS)       *~
            * #4  ! APCPLNGR ! Master Glass Remake File                 *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "EWDPLNGT",                                      ~
                        varc,     indexed,  recsize =   200,             ~
                        keypos =    1, keylen = 26

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #3,  "USERLCMS",                                     ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   3,                     ~
                        alt key  1, keypos =   4, keylen =  30, dup

            select #4,  "APCPLNGR",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 22,   keylen =  12,                     ~
                        alt key  1, keypos  =     7, keylen = 27,        ~
                            key  2, keypos  =     1, keylen = 33,        ~
                            key  3, keypos  =    13, keylen = 21


            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 100%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENOLIB" (#3, "SHARE", f2%(3%), rslt$(3%), axd$)
            call "OPENCHCK" (#4, fs%(1%), f2%(4%),   0%, rslt$(4%))

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATFMTC" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   3%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 14% then gosub print_label
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
                  if keyhit%  = 10% then goto process_data
                  if keyhit%  = 14% then goto print_label
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 3% then editpg1
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
            *             P R O C E S S   D A T A                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************
             
        process_data                             /* Entry Point and    */
            init(" ") dt$(), gt_k$(), cc$()      /* return Point after */
            mat dt% = zer                        /* Screen Edit        */
            dt% = 0% : dt_max% = 0%
            gt_key$ = all(hex(00))
            str(gt_key$,1%,1%) = "0"             /* Only check Open    */ 
            str(gt_key$,2%,2%) = sc_code$        /* Selected Area Only */
                                                 /* Check for New Rmks */
            if sc_code% = 1% then gosub check_orig
        process_data_next
            read #1,key > gt_key$, using L19000, gt_key$,                 ~
                                               eod goto process_data_done  
L19000:        FMT CH(26)
            if str(gt_key$,1%,1%) <> "0" then goto process_data_done
                                               /* Only Check Open Rec's */
            if str(gt_key$,2%,2%) <> sc_code$ then                        ~
                                              goto process_data_done   
               gosub dataload                  /* Load GT Record (In)   */
               gosub stuff_dt                  /* Put GT Data into DT$()*/
               goto process_data_next
        process_data_done
            dt%(sc_code%) = dt%
            dt_max%       = dt%
            gosub display_data
            gosub check_for_input              /* Check for Edit Screen */
                                               /* and Sleep             */ 
            goto process_data                  /* Continue Processing   */
                                               /* Wake-Up and Check     */
        stuff_dt
           dt% = dt% + 1%                    /* Update Specified Bucket */
           if dt% > 140% then dt% = 140%
                                             /* (140) Max in Buffer     */ 
           str(dt$(dt%),1%,3%)   = gt_model$
           str(dt$(dt%),5%,3%)   = gt_dept$  
           str(dt$(dt%),9%,1%)   = str(gt_view$,1%,1%)
           str(dt$(dt%),11%,2%)  = str(gt_color$,1%,2%)
           str(dt$(dt%),14%,7%)  = gt_grid$
           str(dt$(dt%),22%,9%)  = gt_cut_w$
           str(dt$(dt%),32%,9%)  = gt_cut_h$
           str(dt$(dt%),42%,4%)  = gt_ty$
           str(dt$(dt%),47%,5%)  = gt_seq$
           str(dt$(dt%),53%,8%)  = gt_so$
           str(dt$(dt%),62%,3%)  = str(gt_time$,1%,2%) & ":"   /* Hour */
           str(dt$(dt%),65%,2%)  = str(gt_time$,3%,2%)         /* Min  */
           str(dt$(dt%),68%,2%)  = str(gt_spacer$,3%,2%)       /*Spacer*/
           str(dt$(dt%),71%,2%)  = gt_shft$
           str(dt$(dt%),74%,1%)  = gt_contour$
           brass$ = " "
           if str(gt_color$,1%,3%) = "BRS" then brass$ = "B"
           if str(gt_color$,4%,3%) = "BRS" then brass$ = "B"
           str(dt$(dt%),75%,1%) = brass$

           gt_k$(dt%) = gt_key$            /* Save Key for Edit/Delete */
        return

        check_orig                             /* Look for New Remakes */
            sav_key$ = all(hex(00))
            str(sav_key$,1%,1%) = "0"          /* Open's Only          */
            str(sav_key$,2%,2%) = "00"         /* '00' = Originator    */
        check_orig_nxt
            read #1,key > sav_key$, using L20000, sav_key$,            ~
                                                        eod goto L20100
L20000:        FMT CH(26)
            if str(sav_key$,1%,3%) <> "000" then goto L20100
               gosub dataload_gt
               str(gt_rec$,4%,6%)   = date     /* Date Put into Area   */
               str(gt_rec$,10%,8%)  = time     /* Time Put into Area   */
               str(gt_rec$,106%,1%) = "Y"      /* Label Printed        */

               for ii% = 1% to area_max%      /* Max Area's Set at Init*/
                   convert ii% to str(gt_rec$,2%,2%), pic(00)
                   gosub dataput_gt            /* Write Display Record */
               next ii%

               er% = 0%
               call "EWDPLA69" (gt_rec$, #2, er%)
               str(gt_rec$,106%,1%) = "N" 
               if er% = 0% then str(gt_rec$,106%,1%) = "Y"
               if er% = 0% then goto L20050
                  errormsg$ = "(Error) Printing Label ( xx )"
                  convert er% to str(errormsg$,26%,2%), pic(00)
                  gosub error_prompt

L20050:                                         /* Delete Orig '00' rec*/
               gosub delete_gt_rec
               str(gt_rec$,1%,1%)   = "1"       /* Closed              */  
               str(gt_rec$,2%,2%)   = "00"      /* Originator          */
               str(gt_rec$,166%,6%) = date      /* Finished            */
               str(gt_rec$,172%,8%) = time      /* Finished Time       */
               str(gt_rec$,180%,2%) = sc_shft$  /* Finished during Shft*/
               str(gt_rec$,182%,3%) = sc_user$  /* Area (1) User       */
               gosub calc_time                  
               gosub dataput_gt

               goto check_orig_nxt

L20100: return                                 /* No more Originators  */

        delete_gt_rec
            read #1,hold,key = sav_key$, eod goto L20110
               delete #1
        return
L20110:     errormsg$ = "(Error) Unable to Delete --> " & sav_key$
            gosub error_prompt
            er% = 2%
        return 

        check_for_input
            close ws
            call "SCRNADDR" ("O",22%,4%," ",0%,0%,return%)
            call "SCRNADDR" ("E",22%,0%," ",0%,0%,return%)
            text$ = "Press PF(10) to Edit Screen?"
            call "SCRNADDR" ("W",23%,4%,text$,30%,1%,return%)
               if return% > 109% then goto L21000

            call "EWDSLEEP" (sleep_sec%,er%)

            call "SCRNADDR" ("W",23%,4%,text$,30%,1%,return%)
               if return% > 109% then goto L21000

            call "SCRNADDR" ("C",22%,0%," ",0%,0%,return%) 
        return

L21000: call "SCRNADDR" ("C",22%,0%," ",0%,0%,return%) 
        return clear all
        gosub edit_display
        if keyhit% = 16% then goto L21010
        goto process_data  

L21010: return clear all
        goto inputmode

        clean_up_screen
            for ii% = 1% to dt_max%
                if cc$(ii%) <> "X" then goto L21900
                   sav_key$ = gt_k$(ii%)
                   gosub dataload_gt
                   if load% = 0% then goto L21900  /* Could Not Load */
                      gosub delete_gt_rec
                      str(gt_rec$,1%,1%)   = "1"   /* Closed         */  
                      str(gt_rec$,2%,2%)   = sc_code$
                      str(gt_rec$,166%,6%) = date  /* Finished       */
                      str(gt_rec$,172%,8%) = time  /* Finished Time  */
                      str(gt_rec$,180%,2%) = sc_shft$  /* Finish Shft*/
                      str(gt_rec$,182%,3%) = sc_user$  /* Area-User  */
                      gosub calc_time                  
                      gosub dataput_gt
                      if sc_code% = 1% then gosub update_remake
L21900:     next ii%
        return clear all
        goto process_data
        
        calc_time
            init(" ") tt_in$, tt_out$
            tt_in$  = str(gt_rec$,10%,8%)          /* Start Time  */
            tt_out$ = str(gt_rec$,172%,8%)         /* Finish Time */

            convert str(tt_in$,1%,2%) to tt_hr1%, data goto L22010
L22010:           
            convert str(tt_in$,3%,2%) to tt_mn1%, data goto L22020
L22020:
            convert str(tt_in$,5%,4%) to tt_sc1%, data goto L22025
L22025: 

            convert str(tt_out$,1%,2%) to tt_hr2%, data goto L22030
L22030:           
            convert str(tt_out$,3%,2%) to tt_mn2%, data goto L22040
L22040:
            convert str(tt_out$,5%,4%) to tt_sc2%, data goto L22045
L22045:

            if tt_sc1% > 3000% then tt_mn1% = tt_mn1% + 1%
            if tt_sc2% > 3000% then tt_mn2% = tt_mn2% + 1%
 
                                               /* Convert start and   */ 
            tt1% = (tt_hr1% * 60%) + tt_mn1%   /* Finish time to      */
            tt2% = (tt_hr2% * 60%) + tt_mn2%   /* Minutes             */

            if tt1% > tt2% then tt2% = tt2% + 1440% /* Correct with 
                                               /* Minutes for (1) day  */
            tt3% = tt2% - tt1%                 /* Total Minutes to     */
                                               /* Complete Conversion  */
            tt_hr3% = int(tt3%/60%)            /* Total Minutes        */
            tt_mn3% = mod(tt3%,60%) 
  
            convert tt_hr3% to tt_hr$, pic(000)/* Total Hours          */

            convert tt_mn3% to tt_mn$, pic(00) /* Total Minutes        */

            str(gt_rec$,185%,6%)  = tt_hr$ & ":" & tt_mn$

        return

        purge_data                              /* (EWD003) - 06/09/99 */
            if userid$ <> "RHH" then goto L23100

               call "SHOSTAT" ("Purging Data in (EWDPLNGT)?")

            gt_key$ = all(hex(00))
        purge_data_nxt
            read #1,hold,key > gt_key$, using L23000, gt_key$,           ~
                                                 eod goto purge_data_done
L23000:        FMT CH(26)
               delete #1
               goto purge_data_nxt
        purge_data_done
        return
L23100:     errormsg$ = "(Error) - Not Authorized to Purge Data?"
            gosub error_prompt
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
         "Enter a Valid Area Selection Code?                           ",~
         "Enter a Valid Shift Code? 01, 02, or 03                      ",~
         "Enter a Valid User Id?                                       " 

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, sc_code$,  sc_code_d$,     ~
                      sc_shft$, sc_shft_d$, ss$(), dt$(), cc$(), gt_k$(),~
                      sc_user$, sc_user_d$

            ss$( 1%) = "*********************************************"
            ss$( 2%) = "*         T r a u m a   A r e a ' s         *"
            ss$( 3%) = "*(1) - Oven Person (Printer Area)           *"
            ss$( 4%) = "*(2) - Trauma Point Person (2,for Intercept)*"
            ss$( 5%) = "*(3) - Grid Person                          *"
            ss$( 6%) = "*(4) - Cutter Person                        *"
            ss$( 7%) = "*(5) - Setter Person                        *" 
            ss$( 8%) = "*(6) - Available (Future)                   *" 
            ss$( 9%) = "*                                           *" 
            ss$(10%) = "*********************************************"

            area_max% = 6%
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
            
        dataload
    
            get #1, using L35000,                                        ~
                gt_stat$,                /* Status 0 = Open, 1 = Closed*/~
                gt_area$,                /* 00 = Orig, (PLAN AREA)     */~
                gt_dte$,                 /* Date Scanned In as Rmk     */~
                gt_time$,                /* Time Scanned In            */~
                gt_barcode$,             /* Glass Barcode              */~
                gt_model$,               /* Model Code                 */~
                gt_dept$,                /* Department Code            */~
                gt_view$,                /* View Top/Bot               */~
                gt_color$,               /* Product Color              */~
                gt_grid$,                /* Grid Value                 */~
                gt_cut_w$,               /* Glass Cut Width            */~
                gt_cut_h$,               /* Glass Cut Height           */~
                gt_ty$,                  /* Glass Type Desc            */~
                gt_seq$,                 /* Department Production Seq  */~
                gt_so$,                  /* Customer Sales Order No    */~
                gt_cust$,                /* Customer Code              */~
                gt_prod$,                /* Production Date Formatted  */~
                gt_num$,                 /* Glass Remake No            */~
                gt_label$,               /* Printed (Y)es or (N)o      */~
                gt_win_w$,               /* Actual Window Width        */~
                gt_win_h$,               /* Actual Window Height       */~
                gt_shft$,                /* Production Shift Code (In) */~
                gt_userid$,              /* Production Userid (In)     */~   
                gt_contour$,             /* Flag 'C'                   */~
                gt_text$,                /* Glass Text String          */~
                gt_dte_o$,               /* Date Out of Comp Area      */~
                gt_time_o$,              /* Time Out Comp area         */~
                gt_shft_o$,              /* Shift Code Out Comp Area   */~
                gt_userid_o$,            /* User Id Comp Area          */~
                gt_complete$,            /* Time to Complete hhh-mm    */~
                gt_spacer$,              /* Glass Spacer Size          */~
                gt_filler$               /* Filler Area                */

        return

        dataload_gt
            read #1,key = sav_key$, using L30000,gt_rec$, eod goto L30010
L30000:       FMT CH(200)
            load% = 1% 
        return
L30010:     load% = 0%
            errormsg$ = "(Error) - Unable to Load --> " & sav_key$
            gosub error_prompt 
        return            

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        dataput_gt
            write #1, using L31000, gt_rec$, eod goto L31010
L31000:         FMT CH(200)
        return
L31010:     errormsg$ = "(Error) Unable to Write --> " & str(gt_rec$,1%,26%)
            gosub error_prompt
        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L35000:     FMT                                                          ~
                CH(1),                   /* Status 0 = Open, 1 = Closed*/~
                CH(2),                   /* Area 00 = Orig,(PLAN AREA) */~
                CH(6),                   /* Date Scanned In as Rmk     */~
                CH(8),                   /* Time Scanned In as Rmk     */~
                CH(9),                   /* Glass Barcode              */~
                CH(3),                   /* Model Code                 */~
                CH(3),                   /* Department Code            */~
                CH(3),                   /* View Top/Bot               */~
                CH(6),                   /* Product Color              */~
                CH(7),                   /* Grid Value                 */~
                CH(9),                   /* Glass Cut Width            */~
                CH(9),                   /* Glass Cut Height           */~
                CH(4),                   /* Glass Type Desc            */~
                CH(5),                   /* Department Production Seq  */~
                CH(8),                   /* Customer Sales Order No    */~
                CH(9),                   /* Customer Code              */~
                CH(10),                  /* Production Date Formatted  */~
                CH(3),                   /* Glass Remake No            */~
                CH(1),                   /* Label Printed (Y)es or (N)o*/~
                CH(7),                   /* Actual Window Width        */~
                CH(6),                   /* Actual Window Height       */~
                CH(2),                   /* Production Shift Code (In) */~
                CH(3),                   /* Production User Id (In)    */~
                CH(1),                   /* Flag 'C'                   */~
                CH(40),                  /* Glass Text String          */~
                CH(6),                   /* Date Out Completed         */~
                CH(8),                   /* Timr Out Completed         */~
                CH(2),                   /* Shift Out Completed        */~
                CH(3),                   /* User Id Out Completed      */~      
                CH(6),                   /* Time to Complete HHH-MM    */~
                CH(4),                   /* Spacer Size                */~  
                CH(6)                    /* Filler Area                */


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
              on fieldnr% gosub L40120,          /* Area Selection     */~
                                L40120,          /* Shift Selection    */~
                                L40120           /* User Id            */

              goto L40130

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40120:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40130:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,23), fac(hex(a4)), apc$                   , ch(35),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Area Selection :",                           ~
               at (03,20), fac(lfac$(1%)), sc_code$             , ch(02),~
               at (03,30), fac(hex(84)),   sc_code_d$           , ch(40),~
                                                                         ~   
               at (04,02), "Shift Selection:",                           ~
               at (04,20), fac(lfac$(2%)), sc_shft$             , ch(02),~
               at (04,30), fac(hex(84)),   sc_shft_d$           , ch(30),~
                                                                         ~   
               at (05,02), "User Id        :",                           ~
               at (05,20), fac(lfac$(3%)), sc_user$             , ch(03),~
               at (05,30), fac(hex(84)),   sc_user_d$           , ch(30),~
                                                                         ~   
               at (07,18), fac(hex(84)),   ss$( 1%)             , ch(45),~
               at (08,18), fac(hex(84)),   ss$( 2%)             , ch(45),~
               at (09,18), fac(hex(84)),   ss$( 3%)             , ch(45),~
               at (10,18), fac(hex(84)),   ss$( 4%)             , ch(45),~
               at (11,18), fac(hex(84)),   ss$( 5%)             , ch(45),~
               at (12,18), fac(hex(84)),   ss$( 6%)             , ch(45),~
               at (13,18), fac(hex(84)),   ss$( 7%)             , ch(45),~
               at (14,18), fac(hex(84)),   ss$( 8%)             , ch(45),~
               at (15,18), fac(hex(84)),   ss$( 9%)             , ch(45),~
               at (16,18), fac(hex(84)),   ss$(10%)             , ch(45),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
                                                   /* (EWD003) - Purge */  
               if keyhit% <> 22% then goto L40200
                  gosub purge_data
                  goto L40130
 
L40200:        if keyhit% <> 15 then goto L40450
                  call "PRNTSCRN"
                  goto L40130

L40450: close ws
        call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf1

        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Label "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffff0e0f101600)
        return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Label "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (10)Display Screen     " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffff0affffff0e0f1000)
        return
L40700:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       " 
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
        return


        REM *************************************************************~
            *           E D I T   D I S P L A Y   S C R E E N           *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        edit_display
            k% = 0%
L41000:     gosub set_pf2
   
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,23), fac(hex(a4)), apc$                   , ch(35),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
                                                                         ~
               at (02,02), fac(hex(84)), area$                  , ch(20),~
               at (02,66), fac(hex(84)), pageno$                , ch(14),~
                                                                         ~
               at (03,23), fac(hex(a4)), title$                 , ch(37),~
                                                                         ~
               at (05,04), fac(hex(a4))  , h1$                  , ch(03),~
               at (05,08), fac(hex(a4))  , h2$                  , ch(03),~
               at (05,12), fac(hex(a4))  , h3$                  , ch(01),~
               at (05,14), fac(hex(a4))  , h4$                  , ch(02),~
               at (05,17), fac(hex(a4))  , h5$                  , ch(07),~
               at (05,25), fac(hex(a4))  , h6$                  , ch(09),~
               at (05,35), fac(hex(a4))  , h7$                  , ch(09),~
               at (05,45), fac(hex(a4))  , h8$                  , ch(04),~
               at (05,50), fac(hex(a4))  , h9$                  , ch(05),~
               at (05,56), fac(hex(a4))  , h10$                 , ch(08),~
               at (05,65), fac(hex(a4))  , h11$                 , ch(05),~
               at (05,71), fac(hex(a4))  , h14$                 , ch(02),~
               at (05,74), fac(hex(a4))  , h12$                 , ch(02),~
               at (05,77), fac(hex(a4))  , h13$                 , ch(01),~                 
                                                                         ~
               at (06,02), fac(hex(81))  , cc$(k% + 1%)         , ch(01),~
               at (06,04), fac(hex(84))  , dt$(k% + 1%)         , ch(75),~
                                                                         ~
               at (07,02), fac(hex(81))  , cc$(k% + 2%)         , ch(01),~
               at (07,04), fac(hex(84))  , dt$(k% + 2%)         , ch(75),~
                                                                         ~
               at (08,02), fac(hex(81))  , cc$(k% + 3%)         , ch(01),~
               at (08,04), fac(hex(84))  , dt$(k% + 3%)         , ch(75),~
                                                                         ~
               at (09,02), fac(hex(81))  , cc$(k% + 4%)         , ch(01),~
               at (09,04), fac(hex(84))  , dt$(k% + 4%)         , ch(75),~
                                                                         ~
               at (10,02), fac(hex(81))  , cc$(k% + 5%)         , ch(01),~
               at (10,04), fac(hex(84))  , dt$(k% + 5%)         , ch(75),~
                                                                         ~
               at (11,02), fac(hex(81))  , cc$(k% + 6%)         , ch(01),~
               at (11,04), fac(hex(84))  , dt$(k% + 6%)         , ch(75),~
                                                                         ~
               at (12,02), fac(hex(81))  , cc$(k% + 7%)         , ch(01),~
               at (12,04), fac(hex(84))  , dt$(k% + 7%)         , ch(75),~
                                                                         ~
               at (13,02), fac(hex(81))  , cc$(k% + 8%)         , ch(01),~
               at (13,04), fac(hex(84))  , dt$(k% + 8%)         , ch(75),~
                                                                         ~
               at (14,02), fac(hex(81))  , cc$(k% + 9%)         , ch(01),~
               at (14,04), fac(hex(84))  , dt$(k% + 9%)         , ch(75),~
                                                                         ~
               at (15,02), fac(hex(81))  , cc$(k% + 10%)        , ch(01),~
               at (15,04), fac(hex(84))  , dt$(k% + 10%)        , ch(75),~
                                                                         ~
               at (16,02), fac(hex(81))  , cc$(k% + 11%)        , ch(01),~
               at (16,04), fac(hex(84))  , dt$(k% + 11%)        , ch(75),~
                                                                         ~
               at (17,02), fac(hex(81))  , cc$(k% + 12%)        , ch(01),~
               at (17,04), fac(hex(84))  , dt$(k% + 12%)        , ch(75),~
                                                                         ~
               at (18,02), fac(hex(81))  , cc$(k% + 13%)        , ch(01),~
               at (18,04), fac(hex(84))  , dt$(k% + 13%)        , ch(75),~
                                                                         ~
               at (19,02), fac(hex(81))  , cc$(k% + 14%)        , ch(01),~
               at (19,04), fac(hex(84))  , dt$(k% + 14%)        , ch(75),~
                                                                         ~
               at (21,02), fac(hex(a4)),   dsp_msg$             , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~  
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L41040             /* First    */
L41020:           k% = 0%
                  goto L41000

L41040:        if keyhit% <> 3% then goto L41080             /* Last      */
L41060:           x% = int(val_max% / 14%)
                  k% = (x%*14%)
                  if (k% + 1%) > val_max% then k% = k% - 14% 
                  goto L41000

L41080:        if keyhit% <> 4% then goto L41100             /* Previous */
                  if k% < 15% then goto L41020
                  k% = k% - 14%
                  if k% <= 1% then goto L41020
                  goto L41000

L41100:        if keyhit% <> 5% then goto L41150             /* Next     */
                  k% = k% + 14%
                  if k% < val_max% then goto L41000
                  goto L41060

L41150:        if keyhit% <> 15 then goto L41155
                  call "PRNTSCRN"
                  goto L41000

L41155:        if keyhit% <> 0% then goto L41160
                  gosub clean_up_screen
                  goto L41200
 
L41160:        if keyhit% <> 16% then goto L41000

L41200:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return 

        set_pf2
            init(" ") h1$, h2$, h3$, h4$, h5$, h6$, h7$, h8$,h9$, h10$,   ~
                      h11$, h12$, h13$, h14$, title$, sc_time$,  dsp_msg$
            dsp_msg$=                                                     ~
             "Use 'X' to Delete Line(s) from Screen, followed by <Return>?"
            str(dsp_msg$,65%,15%) = "Total [ xxxxx ]"
            convert dt%(sc_code%) to str(dsp_msg$,73%,5%), pic(#####)

            call "TIME" (sc_time$)
            area$ = "Area [ x ] HH:MM PM "
            convert sc_code% to str(area$,8%,1%), pic(#)

            str(area$,12%,8%) = sc_time$
            title$ = str(ss$(sc_code% + 2%),8%,37%)

            pageno$ = "Page: XX of XX"             /* k% = Array Values */
            h1$ = "Mod"                            /* (3) Model         */
            h2$ = "Dep"                            /* (3) Department    */
            h3$ = "V"                              /* (1) T or B        */
            h4$ = "Cl"                             /* (2) Color         */
            h5$ = "<Grid >"                        /* (7) Grid Descript */
            h6$ = "Cut Width"                      /* (9) Cut width     */
            h7$ = "Cut Hght "                      /* (9) Cut Height    */
            h8$ = "Type"                           /* (4) Glass Type    */
            h9$ = "Seq. "                          /* (5) Sequence No.  */
            h10$= "Sls Ord "                       /* (8) Sales Order   */
            h11$= "Time "                          /* (5) Time In       */
            h14$= "Sp"                             /* (2) Spacer Thickne*/ 
            h12$= "Sh"                             /* (2) Shft Scan By  */
            h13$= "G"                              /* (1) Contour/Brass */ 

            val_max% = dt%(sc_code%)
            if val_max% > (154% - 14%) then val_max% = 154% - 14%
                                                        /* Display Max */
            x = val_max%
            yy% = ( val_max% / 14% )
            if mod(x,14) <> 0 then yy% = yy% + 1%

            xx% = (k% / 14%) +1%
            if xx% > yy% then xx% = yy%

            convert xx% to str(pageno$,7%,2%), pic(##) /* Current Page*/

            convert yy% to str(pageno$,13%,2%), pic(##)/* Total Pages */
  

            pf$(1) = "(2)First           (5)Next              " &        ~
                     "                                       "
            pf$(2) = "(3)Last                                 " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(4)Previous                             " &        ~
                     "                       (16)Exit Program" 
            pfkeys$ = hex(ff02030405ffffffffffffffff0f1000)
            gosub check_screen
            return

        check_screen
            if val_max% > 14% then goto L41860
               gosub no_first
               gosub no_next
               gosub no_last
               gosub no_prev
               return
L41860:      if k% >= 14% then goto L41870
                gosub no_first
                gosub no_prev
L41870:      if (k% + 14%) <= val_max% then goto L41880
                gosub no_last
L41880:      if k% <= (val_max% - 14%) then goto L41900
                gosub no_next
L41900: return
        no_first
            str(pf$(1%),1%, 9%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_next
            str(pf$(1%),20%, 9%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_last
            str(pf$(2%),1%,9%)   = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prev
            str(pf$(3%),1%,12%)  = " " : str(pfkeys$,4%,1%) = hex(ff)
        return


        REM *************************************************************~
            *           D I S P L A Y   D A T A   S C R E E N           *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_data
            gosub set_pf3   
            display                                                      ~
               at (01,02), hex(8c), pname$,                              ~
               at (01,23), hex(a4), apc$,                                ~
               at (01,64), "Today:",                                     ~
               at (01,71), hex(8c), date$,                               ~
                                                                         ~
               at (02,02), hex(84), area$,                               ~
               at (02,64), hex(84), pageno$,                             ~
                                                                         ~
               at (03,23), hex(a4), title$,                              ~
                                                                         ~
               at (05,04), hex(a4)  , h1$,                               ~
               at (05,08), hex(a4)  , h2$,                               ~
               at (05,12), hex(a4)  , h3$,                               ~
               at (05,14), hex(a4)  , h4$,                               ~
               at (05,17), hex(a4)  , h5$,                               ~
               at (05,25), hex(a4)  , h6$,                               ~
               at (05,35), hex(a4)  , h7$,                               ~
               at (05,45), hex(a4)  , h8$,                               ~
               at (05,50), hex(a4)  , h9$,                               ~
               at (05,56), hex(a4)  , h10$,                              ~
               at (05,65), hex(a4)  , h11$,                              ~
               at (05,71), hex(a4)  , h14$,                              ~
               at (05,74), hex(a4)  , h12$,                              ~
               at (05,77), hex(a4)  , h13$,                              ~
                                                                         ~
               at (06,04), hex(84)  , dt$(k% + 1%),                      ~
               at (08,04), hex(8c)  , dt$(k% + 2%),                      ~
               at (10,04), hex(8c)  , dt$(k% + 3%),                      ~
               at (12,04), hex(8c)  , dt$(k% + 4%),                      ~
               at (14,04), hex(8c)  , dt$(k% + 5%),                      ~
               at (16,04), hex(8c)  , dt$(k% + 6%),                      ~
               at (18,04), hex(8c)  , dt$(k% + 7%),                      ~
               at (21,02), hex(a4),   dsp_msg$

        return 

        set_pf3
            init(" ") h1$, h2$, h3$, h4$, h5$, h6$, h7$, h8$,h9$, h10$,   ~
                      h11$, h12$, h13$, h14$, sc_time$, title$, date$

            date$ = date
            call "DATFMTC" (date$)

            call "TIME" (sc_time$)
            area$ = "Area [ x ] HH:MM PM "
            convert sc_code% to str(area$,8%,1%), pic(#)

            str(area$,12%,8%) = sc_time$

            dsp_msg$ = "Current Display Total [ xxxxx ] ???"
            pageno$  = "Total =[xxxxx]"        /* Current Active    */

            convert dt%(sc_code%) to str(dsp_msg$,25%,5%), pic(#####)

            convert dt%(sc_code%) to str(pageno$,9%,5%), pic(#####)

            title$ = str(ss$(sc_code% + 2%),8%,37%)

            h1$ = "Mod"                            /* (3) Model         */
            h2$ = "Dep"                            /* (3) Department    */
            h3$ = "V"                              /* (1) T or B        */
            h4$ = "Cl"                             /* (2) Color         */
            h5$ = "<Grid >"                        /* (7) Grid Descript */
            h6$ = "Cut Width"                      /* (9) Cut width     */
            h7$ = "Cut Hght "                      /* (9) Cut Height    */
            h8$ = "Type"                           /* (4) Glass Type    */
            h9$ = "Seq. "                          /* (5) Sequence No.  */
            h10$= "Sls Ord "                       /* (8) Sales Order   */
            h11$= "Time "                          /* (5) Time In       */
            h14$= "Sp"                             /* (2) Spacer size   */
            h12$= "Sh"                             /* (2) Shift In      */
            h13$= "G"                              /* (1) Contour/Brass */

        return
       
        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************
                                                   /* (EWD001) - Begin */ 
        print_label
            gosub set_pf4
            lfac$(1%) = hex(81)

L42005:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,23), fac(hex(a4)), apc$                   , ch(35),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Glass Barcode :",                            ~
               at (05,20), fac(lfac$(1%)), barcode$             , ch(09),~
                                                                         ~   
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 0% then goto L42010
                  gosub L50300
                  goto L42005

L42010:        if keyhit% <> 14% then goto L42020
                  gosub print_label_detail
                  goto L42005
 
L42020:        if keyhit% <> 15 then goto L42030
                  call "PRNTSCRN"
                  goto L42005

L42030:        if keyhit% <> 16% then goto L42005
                  
        close ws
        call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return clear all
        goto inputmode

        set_pf4
            inpmessage$ = "Enter a Valid Glass Barcode to Print Label and Press <RETURN>?"

            if len(barcode$) = 9 then                                    ~
               inpmessage$ = "Press PF(14) to Print Glass Barcode Label?"    

            pf$(1) = "                                        " &        ~
                     "                       (14)Print Label "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(ffffffffffffffffffffffffff0eff1000)
        return
                                                   /* (EWD001) - End   */  
        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            init(" ") errormsg$
            on fieldnr% gosub L50000,                 /* sc_code$     */~
                              L50100,                 /* sc_shft$     */~
                              L50200                  /* sc_user$     */      
 
            return

L50000: Rem Area Selection Code                       sc_code$
            init(" ") sc_code_d$
            if sc_code$ <> " " then goto L50005
               sc_code$ = "01"
  
L50005:     sc_code% = 1%
            convert sc_code$ to sc_code%, data goto L50008

L50008:     convert sc_code% to sc_code$, pic(00)

            if sc_code% < 1% or sc_code% > area_max% then goto L50010
               sc_code_d$ = str(ss$(sc_code% +2%),8%,37%)

            if sc_code% = 1% then sleep_sec% = 5% else sleep_sec% = 10%
        return
L50010:     init(" ") sc_code$, sc_code_d$
            errormsg$ = "(Error) Invalid Trauma Area Selection?"
            gosub error_prompt
        return

L50100: Rem Glass shift Code                     sc_shft$
            init(" ") sc_shft_d$
            if sc_shft$ <> " " then goto L50105
               sc_shft$ = "01"
  
L50105:     sc_shft% = 1%
            convert sc_shft$ to sc_shft%, data goto L50108

L50108:     convert sc_shft% to sc_shft$, pic(00)

            if sc_shft% < 1% or sc_shft% > 3% then goto L50110

               if sc_shft% = 1% then sc_shft_d$ = "1st Shift"
               if sc_shft% = 2% then sc_shft_d$ = "2nd Shift"
               if sc_shft% = 3% then sc_shft_d$ = "3rd Shift"

        return
L50110:     init(" ") sc_shft$, sc_shft_d$
            errormsg$ = "(Error) Invalid Shift Selection?"
            gosub error_prompt
        return

L50200: REM Check User Id                        sc_user$
           init(" ") sc_user_d$
           read #3,key = sc_user$, using L50210, sc_user_d$,           ~
                                                 eod goto L50220
L50210:       FMT POS(4), CH(30)
        return
L50220:    errormsg$ = "(Error) - Invalid User ID, 'ID' is Required?"
           gosub error_prompt
           init(" ") sc_user$, sc_user_d$
        return

L50300: REM Check Glass Barcode                barcode$
           init(" ") rm_key$, rm_rec$, errormsg$
           str(rm_key$,1%,9%)  = barcode$        /* Glass Barcode      */
           read #4,key > rm_key$, using L60000 , rm_rec$,                ~
                                                        eod goto L50310
        REM - check for Valid Barcode for Glass
              if str(rm_rec$,22%,9%) <> barcode$ then goto L50310

        return
L50310:    errormsg$ = "(Error) - Invalid Glass Barcode???"
           gosub error_prompt
           init(" ") barcode$
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************
                                   /* (EWD002) - Check on 06/08/99       */
        update_remake              /* All Glass Has at Least One         */
                                   /* entry in (APCPLNGR)                */ 
                                   /* Status 0=Remake,1=Sched,2=Complete */
           init(" ") rm_key$, rm_st_time$, rm_rec$
           call "TIME" (rm_st_time$)             /* Time Glass Scheduled */
                                                 /* Set to Scheduled Glass*/
           str(rm_key$,1%,9%)  = str(gt_rec$,18%,9%)   /* Glass Barcode  */
           str(rm_key$,10%,3%) = str(gt_rec$,103%,3%)  /* Re-Make No.  */
           read #4,hold,key = rm_key$, using L60000 , rm_rec$,            ~
                                                      eod goto L60020
L60000:       FMT CH(256)
                                                 /* (EWD002) - Fix     */
           if str(rm_rec$,13%,1%) <> "0" then goto L60030 
                                                 /*Only touch Scheduled*/
                                                 /* re-make Glass      */
              delete #4
        REM - Re-Schedule Glass for Re-Make      /* Found in Gls Batch */
              str(rm_rec$,1%,6%) = str(gt_rec$,4%,6%) /* Prod. Dte     */
              str(rm_rec$,7%,6%) = str(gt_rec$,4%,6%) /* Scan. Dte     */
              str(rm_rec$,13%,1%) = "1"          /* Chg Status 0 to a 1*/
              str(rm_rec$,14%,8%) = rm_st_time$  /* Time of Stat Change*/
                                                 /* (EWD002) Leave     */
                                                 /* Glass Barcode alone*/
                                                 /* Re-Make No Alone   */
                                                 /* and Reason Code alone*/ 
              str(rm_rec$,36%,6%) = date         /* Date of Stat Change*/
                                                 /* (EWD002) 42 - 64   */
                                                 /* Contains remake    */
                                                 /* Scan Date/Time     */ 
              str(rm_rec$,242%,5%) = str(gt_rec$,71%,5%)
                                                 /* Update seq No.     */ 
           write #4, using L60000, rm_rec$, eod goto L60010
        return
L60010:    errormsg$="(Error)- Updating (APCPLNGR) Primary Database"
           gosub error_prompt
        return
L60020:    errormsg$="(Error)- Could Not Find ---> " & rm_key$
           gosub error_prompt
        return
                                                 /* (EWD002) - Fix */
L60030:    read #4,key = rm_key$, eod goto L60020/* Release Record */
           errormsg$="(Error)- Glass already Sched or Complete?"
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

        print_label_detail                        /* (EWD001) - Mod    */
            init (" ") gt_rec$
            Call "SHOSTAT" ("Printing Glass Barcode 'Single Label'")

            gt_rec$ = "00000000000000000000000000000000000000000000000000"~
                    & "00000000000000000000000000000000000000000000000000"~
                    & "00000000000000000000000000000000000000000000000000"~
                    & "00000000000000000000000000000000000000000000000000"

            call "EWDPLA69" (gt_rec$, #2, er%)    /* Dummy Label       */
            init(" ") gt_rec$
            str(gt_rec$,1%,3%)   = "000"
            str(gt_rec$,4%,6%%)  = str(rm_rec$,1%,6%)
            str(gt_rec$,10%,8%)  = time
            str(gt_rec$,18%,9%)  = str(rm_rec$,22%,9%)
            str(gt_rec$,27%,3%)  = str(rm_rec$,72%,3%)
            str(gt_rec$,30%,3%)  = str(rm_rec$,249%,3%)
            str(gt_rec$,33%,3%)  = "TOP"
            if str(rm_rec$,111%,1%) = "B" then str(gt_rec$,33%,3%) ="BOT"
            gosub lookup_color
            str(gt_rec$,42%,7%)  = str(rm_rec$,234%,7%)
            str(gt_rec$,49%,18%) = str(rm_rec$,85%,17%)
            str(gt_rec$,67%,4%)  = str(rm_rec$,77%,2%)
            str(gt_rec$,71%,5%)  = str(rm_rec$,242%,5%)
            str(gt_rec$,76%,8%)  = str(rm_rec$,163%,8%)
            str(gt_rec$,84%,9%)  = "999999999"
            str(gt_rec$,93%,10%) = str(rm_rec$,1%,6%)
            call "DATFMTC" (str(gt_rec$,93%,10%))
            str(gt_rec$,103%,3%) = str(rm_rec$,31%,3%)
            str(gt_rec$,106%,1%) = "Y"
            str(gt_rec$,107%,7%) = str(rm_rec$,150%,7%)
            str(gt_rec$,114%,6%) = str(rm_rec$,157%,6%)
            str(gt_rec$,120%,2%) = str(rm_rec$,247%,2%)
            str(gt_rec$,122%,3%) = userid$
            if len(sc_user$) > 2 then str(gt_rec$,122%,3%) = sc_user$ 
            str(gt_rec$,125%,1%) = " "
            
            call "EWDPLA69" (gt_rec$, #2, er%)    /* Real Label       */
             
     
            init (" ") gt_rec$
            gt_rec$ = "00000000000000000000000000000000000000000000000000"~
                    & "00000000000000000000000000000000000000000000000000"~
                    & "00000000000000000000000000000000000000000000000000"~
                    & "00000000000000000000000000000000000000000000000000"

            call "EWDPLA69" (gt_rec$, #2, er%)    /* Dummy Label       */
       return clear all
       goto inputmode

       lookup_color
           init(" ") readkey$
           str(readkey$,1%,9%)   = "COLOR    "
           str(readkey$,10%,15%) = str(rm_rec$,128%,1%)
           read #2,key = readkey$, using L_CLR, str(gt_rec$,36%,6%),     ~
                                                eod goto L_CLR_DONE
L_CLR:        FMT POS(30), CH(6)
L_CLR_DONE
       return  
     
       REM **************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
          
        end

