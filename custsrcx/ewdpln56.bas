        REM *************************************************************~
            *                                                           *~
            *  max_dept% = 50%     max_load% = 300%                     *~
            *                                                           *~ 
            *  Program Name      - EWDPLN56                             *~
            *  Creation Date     - 07/31/98                             *~
            *  Last Modified Date- 01/09/08                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Analysis of Production Scanning Data *~
            *                      for completed product on time.       *~
            *                      Product Scanned complete for then    *~
            *                      Date, Load, and time Specified.      *~
            *                                                           *~
            *  Code Tables Used  - (PLAN PROD) Production Loads for Wk  *~
            *                      (PLAN DEPT) Valid Departments        *~
            *                                                           *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/31/98 ! (New) Program                            ! RHH *~
            * 08/18/98 ! Mod to Display Not Scanned Data          ! RHH *~
            * 09/15/98 ! (EWD001) Skip Howship (22) Sample Repair ! RHH *~
            * 11/18/98 ! (EWD002) Mod to Correct Problem with     ! RHH *~
            *          !   glass Warranty - mmmcWARRANTY Part No. !     *~
            * 01/09/08 ! (AWD003) mods for dept 054 and 074       ! CMG *~
            *************************************************************

        dim                                                              ~
            cnt$30,                      /* For Analysis               */~
            title$40,                    /* Analysis Title and Time    */~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
            sav_key$11,                  /* Use for Loading Table      */~
            sc_dept$3, sc_dept_d$30,     /* Department Code an Descr   */~
            sc_load$5, sc_load_d$30,     /* Load Number an Descr       */~
            ed_load$5, ed_load_d$30,     /* load Number and Descr (End)*/~
            sc_wk$2, ck_load$5,          /* Production Week            */~
            sc_date$10, sc_dte$6,        /* Completion Date for Load   */~
            sc_time$5,                   /* Completion Time for Load   */~
            dt_key$23, dt_dept$3,        /* DT Primary, SC Alt key (1) */~
            dt_cust$9, dt_part$25,       /*                            */~
            dt_seq$5, dt_date$6,         /*                            */~
            dt_st$2, dt_rec$256,         /*                            */~  
            sav_bar$18, sav_dt$2,        /* Production Barcode         */~
            sav_load$5,                  /* Production Load            */~
            ad_key1$33,                  /* Audit Key alt (1)          */~
            sav_dept$3,                  /* Audit Department           */~
            ad_dte$6, ad_time$8,         /* Audit Scan Date            */~
            or_key$8, or_hows$2,         /* APCPLNOR Key and Howship   */~
            sav_or_key$8,                /* Save for check_samples     */~ 
            h1$12, h2$12, h3$12, h4$12,  /* Summary Screen Display     */~
            h5$12, h6$12, option$24,     /*                            */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            dsp_msg$79,                  /* Screen Display Message     */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim supp$(10%)3,                 /* Save Support Departments   */~
            dept_max$3,                  /* Max Departments Selected   */~
            load_max$3,                  /* Max Loads Selected         */~   
            load$(300%)5,                /* Store Load-Load SubScript  */~
            dept$(50%)3,                 /* Store Dept - Dept SubScript*/~
            cc$(300%)1,                  /* Dept Selection Swithch     */~
            dt$(300%)75,                 /* Not Scanned Display        */~ 
            l_date$(300%)6,              /* Store Completion Date/Load */~
            l_time$(300%)5,              /* Store Completion Time/Load */~
            l_c1%(50%,300%),             /* Tot Product-Dept Ea Load   */~
            l_c2%(50%,300%),             /* Tot Comp On-Time Dept Ea Load*/~
            l_c3%(50%,300%),             /* Tot Comp Late Dept Ea Load */~
            l_c4%(50%,300%),             /* Tot Not Comp  Dept Ea Load */~
            l_c5(50%,300%),              /* Tot Complete % Dept Ea Load*/~
                                         /*                            */~
            l_c1$(50%,300%)8,            /* Display                    */~
            l_c2$(50%,300%)8,            /* Display/Print              */~
            l_c3$(50%,300%)8,            /* Display/Print              */~
            l_c4$(50%,300%)8,            /* Display/Print              */~
            l_c5$(50%,300%)8,            /* Display/Print              */~
                                         /*                            */~
            l_d(50%,5%),                 /* Totals All loads by Dept   */~
            l_d$(50%,5%)8                /* Display Total c1$ - c5$    */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%),                    /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Production Analysis of Loads "
            pname$ = "EWDPLN56 - Rev: R7.00"

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
            * #1  ! APCPLNAD ! Production Scanning Audit File           *~
            * #2  ! APCPLNSC ! Master Schedule File                     *~
            * #3  ! APCPLNDT ! Production Master Detail file            *~
            * #4  ! GENCODES ! System Master Code Table Files           *~
            * #5  ! APCPLNLD ! Planning/Scheduling Master Load File     *~
            * #6  ! APCPLNOR ! Planning S.O. Header File                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "APCPLNAD",                                      ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos = 19,   keylen = 33,                      ~
                        alt key 1, keypos =  1, keylen = 33

            select #2,  "APCPLNSC"                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 24,   keylen = 10,                      ~
                        alt key 1, keypos =  7, keylen = 27,             ~
                            key 2, keypos =  1, keylen = 33    

            select #3,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   24, keylen =   23,                    ~
                        alt key  1, keypos =   47, keylen =  57,         ~
                            key  2, keypos  =  53, keylen =  51,         ~
                            key  3, keypos  =   1, keylen =  23, dup,    ~
                            key  4, keypos  =  96, keylen =   8, dup

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #5,  "APCPLNLD"                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 11,   keylen = 5,                       ~
                        alt key 1, keypos =  3, keylen = 13,             ~
                            key 2, keypos =  1, keylen = 15    

            select #6,  "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos = 1,    keylen = 51,                      ~
                        alt key 1, keypos = 27, keylen = 25,             ~
                            key 2, keypos = 70, keylen =  8, dup,        ~
                            key 3, keypos = 78, keylen =  8, dup,        ~
                            key 4, keypos = 52, keylen =  8,             ~
                            key 5, keypos = 36, keylen = 16, dup


            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4, fs%(4%), f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5, fs%(5%), f2%(5%),  0%, rslt$(5%)) 
            call "OPENCHCK" (#6, fs%(6%), f2%(6%),  0%, rslt$(6%)) 

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

            supp$(1%) = "001" : supp$(2%) = "011" /* Support Departments */
            supp$(3%) = "021" : supp$(4%) = "044"
            supp$(5%) = "054" : supp$(6%) = "074" /*(AWD003)*/
            supp_max% = 6%

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
                                                 /* Analyize Scanning  */ 
        process_data                             /* Production Data    */
            gosub load_data
            call "SHOSTAT" ("Analyize Scanning Data")
            init(" ") dt_key$, sav_load$, sav_bar$, sav_dt$, sav_dept$, ~
                      sav_or_key$
                               
            total_ontime = 0.0  : total_plant = 0.0
            cnt$ = "Audit Records   [ xxxxxxxx ]"/* (EWD002) dt_part$  */ 
            cnt% = 0% : total% = 0%              /* Production Records */
            str(dt_key$,1%,5%) = sc_load$        /* Set Starting Load  */
                                                 /* No. For 1st Load   */
            read #3,key 3% > dt_key$, using L19000, dt_key$, sav_bar$,    ~
                           dt_dept$, dt_part$, eod goto process_data_done
L19000:        FMT CH(23), CH(18), CH(3), POS(189), CH(25)

            goto L19005 
        process_data_dt 
            read #3, using L19000, dt_key$, sav_bar$, dt_dept$, dt_part$,~
                                               eod goto process_data_done

L19005:     if str(dt_key$,1%,5%) > ed_load$ then goto Process_data_done
            if dt_dept$ > "094" then goto process_data_dt
                                               /* (EWD002) Skip Warranty*/
            if str(dt_part$,5%,4%) = "WARR" then goto process_data_dt 
                                               /* Skip all Support      */
            gosub check_samples                /* (EWD001) 09/15/98     */
            if hit% = 1% then goto process_data_dt /* Skip Repairs      */

            if sav_load$ = str(dt_key$,1%,5%) then goto L19010 
               sav_load$ = str(dt_key$,1%,5%)    
               for ld% = 1% to load_max%        /* Find Load Sub-Script */  
                   if sav_load$ = load$(ld%) then goto L19010
               next ld%
               init(" ") sav_load$
               goto process_data_dt             /* Set For Error Exit   */

L19010:     if sav_dept$ = dt_dept$ then goto L19025
               sav_dept$ = dt_dept$  
               for dp% = 1% to dept_max%       /* Find Dept Sub_script */
                   if sav_dept$ = dept$(dp%) then goto L19025
               next dp%
               init(" ") sav_dept$
               goto process_data_dt
                                              /* Department Unit Found*/ 
L19025:        l_c1%(dp%,ld%) = l_c1%(dp%,ld%) + 1%  /* Tot Dept/Load */
                                              /* Dept All Loads       */
               l_d(dp%,1%) = l_d(dp%,1%) + 1%
               for sp% = 1% to supp_max%
                   if supp$(sp%) = sav_dept$ then goto L19028
               next sp%  
               total% = total% + 1%           /* Shipped Units        */
                                              /* Analyize Audit Rec's */
L19028:        gosub check_audit_data         /* for Barcode only in  */
                                              /* Sav_dept$            */
               goto process_data_dt
            
        process_data_done
            call "SHOSTAT" ("Formatting Analysis Data")
            for i% = 1% to dept_max%           /* Calculate and Format */
                for k% = 1% to load_max%
                    if l_c1%(i%,k%) < 1% then goto L19030
                       x = l_c2%(i%,k%) : y = l_c1%(i%,k%)
                       l_c5(i%,K%) = round((x / y) * 100, 4)
L19030:         next k%
                if l_d(i%,1%) < 1% then goto L19035
                   l_d(i%,5%) = round((l_d(i%,2%) / l_d(i%,1%)) * 100, 4)

L19035:         if dept$(i%) = "044" then goto L19040
                if dept$(i%) = "054" then goto L19040    /* (AWD003) */
                if dept$(i%) = "074" then goto L19040    /* (AWD003) */
                for sp% = 1% to supp_max%
                    if supp$(sp%) = dept$(i%) then goto L19050
                next sp%                        /* Skip support Dept's */
                                                /* Except Wood Surround*/
L19040:            total_ontime = total_ontime + l_d(i%,2%) /*all Units*/
                   total_plant  = total_plant + l_d(i%,1%)  /* OnTime  */
L19050:     next i%

            for i% = 1% to dept_max%
                for k% = 1% to load_max%
                    convert l_c1%(i%,k%) to l_c1$(i%,k%), pic(########)
                    convert l_c2%(i%,k%) to l_c2$(i%,k%), pic(########)
                    convert l_c3%(i%,k%) to l_c3$(i%,k%), pic(########)
                    convert l_c4%(i%,k%) to l_c4$(i%,k%), pic(########)
                    convert l_c5(i%,k%)  to str(l_c5$(i%,k%),1%,7%), pic(###.###)
                    str(l_c5$(i%,k%),8%,1%) = "%"
                next k%
                for j% = 1% to 5%
                    if j% < 5% then                                     ~
                       convert l_d(i%,j%) to l_d$(i%,j%), pic(########)~
                               else                                     ~
                       convert l_d(i%,j%) to str(l_d$(i%,j%),1%,7%), pic(###.###)
                    if j% = 5% then str(l_d$(i%,j%),8%,1%) = "%"
                next j%
            next i%
            k% = 0%
            gosub display_summary
        return

        check_audit_data                          /* Analyize Scanning */
            init(" ") ad_key1$, ad_time$          /* Audit Data        */

            sav_unit% = 0%
            ad_key1$ = all(hex(00))               /* For a Barcode and */
            str(ad_key1$,1%,18%) = sav_bar$       /* and Specified Dept*/
            read #1,key 1% > ad_key1$, using L20000, ad_key1$, ad_time$,~
                                       eod goto check_audit_data_done
L20000:        FMT CH(33), XX(18), CH(8)
            goto L20005
        check_audit_data_nxt
            read #1, using L20000, ad_key1$, ad_time$,                  ~
                                          eod goto check_audit_data_done
L20005:     cnt% = cnt% + 1%
            if mod(cnt%,500%) <> 0 then goto L20010
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,28%);hex(84);cnt$;
                                            /* Only for Barcode Found */
L20010:     if str(ad_key1$,1%,18%) <> sav_bar$ then                    ~
                                              goto check_audit_data_done
                                            /* Only for Dept. Found   */
            if str(ad_key1$,25%,3%) <> sav_dept$ then                   ~
                                              goto check_audit_data_nxt
                                            /* Now Search for a '12'  */
                                            /*  at Least one (1)      */
               if str(ad_key1$,32%,2%) <> "12" then                     ~
                                              goto check_audit_data_nxt
                     sav_unit% = 1%         /* A '12' Found           */
                     ad_dte$ = str(ad_key1$,19%,6%)  /* Set Scan Date */ 
                                                /* Only Production    */
                                                /* With Complete Stat */
                     if str(ad_time$,7%,2%) <> "AM" then goto L20015
                        if str(ad_time$,1%,2%) <> "12" then goto L20030
                           h_fact% = - 12%
                           goto L20018
                                                 /* Must be 'PM'       */  
L20015:              if str(ad_time$,1%,2%) = "12" then goto L20030
                        h_fact% = 12% 
L20018:                 hh% = 12%
                        convert str(ad_time$,1%,2%) to hh%, data goto L20020

L20020:                 convert (hh% + h_fact%) to str(ad_time$,1%,2%), pic(00)
 
L20030:        gosub analyize_data
               goto L20050
        check_audit_data_done
                                               /* By Department, By   */ 
            l_c4%(dp%,ld%) = l_c4%(dp%,ld%) + 1%        /* By Load    */
                                               /* Tot Not Complete    */
            l_d(dp%,4%) = l_d(dp%,4%) + 1%     /* By Department,   */
                                               /* All Loads. Total    */
                                               /* Not Complete        */ 
L20050: return

        analyize_data
                                                /* Date is Late       */ 
               if ad_dte$ > l_date$(ld%) then goto L21030
                                                /* Date is Early      */
               if ad_dte$ < l_date$(ld%) then goto L21000
                                                /* Date Must be Equal */
                                                /* Time is Late/Date= */ 
               if str(ad_time$,1%,5%) > l_time$(ld%) then goto L21030
                                                 /* Complete Ontime   */
L21000:              l_c2%(dp%,ld%) = l_c2%(dp%,ld%) + 1%
                                                 /* OnTime all Loads  */
                     l_d(dp%,2%) = l_d(dp%,2%) + 1%
                     goto L21040
                                                 /* Complete Late     */
L21030:     l_c3%(dp%,ld%) = l_c3%(dp%,ld%) + 1% /* Complate Late     */
            l_d(dp%,3%) = l_d(dp%,3%) + 1%       /* Complete Late All */
                                                 /* Loads             */
L21040: return

        load_data
            call "SHOSTAT" ("Load (PLAN PROD) Criteria") /* (EWD002)*/
            load_max% = 0% : dept_max% = 0%      /* Clear Values    */
            init(" ") readkey$, desc$            /* Load Table Data */
            str(readkey$,1%,9%)  = "PLAN PROD"   /* Production Loads*/ 
            str(readkey$,10%,2%) = sc_wk$        /* to analyize     */ 
            sav_key$ = str(readkey$,1%,11%)      /* Save Table & Wk */
            read #4,key > readkey$, using L23010, readkey$, desc$,      ~
                                                        eod goto L23100
L23010:        FMT CH(24), CH(30)
            goto L23015
        load_data_next
            read #4, using L23010, readkey$, desc$, eod goto L23100  
L23015:        if str(readkey$,1%,11%) <> sav_key$ then goto L23100
               if str(sc_load$,1%,1%) = "A" then goto L23020 /* (ALL)  */
                  if str(readkey$,12%,5%) < sc_load$ or                  ~
                   str(readkey$,12%,5%) > ed_load$ then goto load_data_next
           
L23020:        load_max% = load_max% + 1%
               if load_max% > 300% then load_max% = 300%
               load$(load_max%) = str(readkey$,12%,5%) /* Load No      */
               init(" ") x$
               date% = 0%
               x$ = str(desc$,1%,6%)
               call "DATEOKC" (x$, date%, errormsg$)
               if date% = 0% then x$ = date
               call "DATUFMTC" (x$)
               l_date$(load_max%) = str(x$,1%,6%)      /* Close Date   */

               l_time$(load_max%) = str(desc$,10%,5%)  /* Close Time   */
               goto load_data_next
   
L23100: sc_load$ = load$(1%)                       /* Set Starting Load*/
        ed_load$ = load$(load_max%)                /* Set Ending Load  */
        
        REM Load GENCODES Table Data from (PLAN DEPT)  
        if sc_dept$ = "ALL" then goto L23110
           dept_max% = 1%                          /* Single Department*/
           dept$(dept_max%) = sc_dept$
           return

L23110: call "SHOSTAT" ("Load (Department) Data")
        init(" ") readkey$, desc$
        str(readkey$,1%,9%) = "PLAN DEPT"
        read #4,key > readkey$, using L23010, readkey$, desc$,          ~
                                                       eod goto L23200
        goto L23130
L23120: read #4, using L23010, readkey$, desc$, eod goto L23200

L23130: if str(readkey$,1%,9%) <> "PLAN DEPT" then goto L23200
           if str(readkey$,10%,3%) > "094" then goto L23200
              dept_max% = dept_max% + 1%
              if dept_max% > 50% then dept_max% = 50%
              dept$(dept_max%) = str(readkey$,10%,3%)
              goto L23120 
L23200: convert dept_max% to dept_max$, pic(###)
        convert load_max% to load_max$, pic(###) 
        return
                                                 /* (EWD001)        */
        check_samples                            /* Skip Sample     */
            init(" ") or_key$, or_hows$          /* Repairs         */
            if sav_or_key$ = str(sav_bar$,1%,8%) then goto L23220
               hit% = 0%
               sav_or_key$ = str(sav_bar$,1%,8%) 
               or_key$ = sav_or_key$
               read #6,key 4% = or_key$, using L23210, or_hows$,     ~
                                                    eod goto L23220
L23210:        FMT POS(92), CH(2)
            if or_hows$ = "22" then hit% = 1%
L23220: return
                                                 /* (EWD001)        */
 
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
         "Enter a Valid Department Code or (All) = Table Values        ",~
         "Enter a Valid Beg Load Number or (All) with Production week? ",~
         "Enter a Valid Ending Load Number or (All)?                   " 

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, sc_dept$, sc_dept_d$,      ~
                      sc_load$, sc_load_d$, sc_wk$, sc_dte$, sc_date$,   ~
                      sc_time$, load$(), l_date$(), l_time$(), dept$(),  ~
                      l_d$(), l_c1$(), l_c2$(), l_c3$(), l_c4$(),        ~
                      l_c5$(), sav_dept$, sav_load$, sav_bar$,           ~
                      ed_load$, ed_load_d$

            mat l_c1% = zer
            mat l_c2% = zer
            mat l_c3% = zer
            mat l_c4% = zer
            mat l_c5  = zer
            mat l_d   = zer

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
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        REM DATAPUT

        REM RETURN CLEAR ALL
        REM GOTO INPUTMODE


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

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
              on fieldnr% gosub L40160,          /* sc_dept$           */~
                                L40160,          /* sc_load$, sc_wk$   */~
                                L40160,          /* ed_load$           */

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Dept Code or (ALL)   :",                     ~
               at (03,25), fac(lfac$(1%)), sc_dept$             , ch(03),~
               at (03,40), fac(hex(84)),   sc_dept_d$           , ch(30),~
                                                                         ~
               at (04,02), "Load No/Prod Wk,(ALL):",                     ~
               at (04,25), fac(lfac$(2%)), sc_load$             , ch(05),~
               at (04,32), fac(lfac$(2%)), sc_wk$               , ch(02),~
               at (04,40), fac(hex(84)),   sc_load_d$           , ch(30),~ 
                                                                         ~
               at (05,02), "Ending Load Number   :",                     ~
               at (05,25), fac(lfac$(3%)), ed_load$             , ch(05),~
               at (05,40), fac(hex(84)),   ed_load_d$           , ch(30),~
                                                                         ~   
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 10% then goto L40400
                  gosub process_data
                  goto L40190

L40400:        if keyhit% <> 15 then goto L40420
                  call "PRNTSCRN"
                  goto L40190

L40420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(2),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (10)Process Data       " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffff0affffffff0f1000)
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
            *           D I S P L A Y   S U M M A R Y   S C R E E N     *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_summary
L41000:     gosub set_pf2   
            accept                                                       ~
               at (01,02), fac(hex(84)), option$                , ch(16),~
               at (01,65), fac(hex(84)), pageno$                , ch(14),~
                                                                         ~
               at (01,21), fac(hex(a4)), title$                 , ch(40),~
                                                                         ~
               at (03,02), fac(hex(a4))  , h1$                  , ch(12),~
               at (03,15), fac(hex(a4))  , h2$                  , ch(12),~
               at (03,28), fac(hex(a4))  , h3$                  , ch(12),~
               at (03,41), fac(hex(a4))  , h4$                  , ch(12),~
               at (03,54), fac(hex(a4))  , h5$                  , ch(12),~
               at (03,67), fac(hex(a4))  , h6$                  , ch(12),~     
                                                                         ~
               at (04,02), fac(hex(81))  , cc$(k% + 1%)         , ch(01),~
               at (04,05), fac(hex(84))  , dept$(k% + 1%)       , ch(03),~
               at (04,17), fac(hex(84))  , l_d$(k% + 1%, 1%)    , ch(08),~
               at (04,30), fac(hex(84))  , l_d$(k% + 1%, 2%)    , ch(08),~
               at (04,43), fac(hex(84))  , l_d$(k% + 1%, 3%)    , ch(08),~
               at (04,56), fac(hex(84))  , l_d$(k% + 1%, 4%)    , ch(08),~
               at (04,69), fac(hex(84))  , l_d$(k% + 1%, 5%)    , ch(08),~ 
                                                                         ~
               at (05,02), fac(hex(81))  , cc$(k% + 2%)         , ch(01),~
               at (05,05), fac(hex(84))  , dept$(k% + 2%)       , ch(03),~
               at (05,17), fac(hex(84))  , l_d$(k% + 2%, 1%)    , ch(08),~
               at (05,30), fac(hex(84))  , l_d$(k% + 2%, 2%)    , ch(08),~
               at (05,43), fac(hex(84))  , l_d$(k% + 2%, 3%)    , ch(08),~
               at (05,56), fac(hex(84))  , l_d$(k% + 2%, 4%)    , ch(08),~
               at (05,69), fac(hex(84))  , l_d$(k% + 2%, 5%)    , ch(08),~ 
                                                                         ~
               at (06,02), fac(hex(81))  , cc$(k% + 3%)         , ch(01),~
               at (06,05), fac(hex(84))  , dept$(k% + 3%)       , ch(03),~
               at (06,17), fac(hex(84))  , l_d$(k% + 3%, 1%)    , ch(08),~
               at (06,30), fac(hex(84))  , l_d$(k% + 3%, 2%)    , ch(08),~
               at (06,43), fac(hex(84))  , l_d$(k% + 3%, 3%)    , ch(08),~
               at (06,56), fac(hex(84))  , l_d$(k% + 3%, 4%)    , ch(08),~
               at (06,69), fac(hex(84))  , l_d$(k% + 3%, 5%)    , ch(08),~ 
                                                                         ~
               at (07,02), fac(hex(81))  , cc$(k% + 4%)         , ch(01),~
               at (07,05), fac(hex(84))  , dept$(k% + 4%)       , ch(03),~
               at (07,17), fac(hex(84))  , l_d$(k% + 4%, 1%)    , ch(08),~
               at (07,30), fac(hex(84))  , l_d$(k% + 4%, 2%)    , ch(08),~
               at (07,43), fac(hex(84))  , l_d$(k% + 4%, 3%)    , ch(08),~
               at (07,56), fac(hex(84))  , l_d$(k% + 4%, 4%)    , ch(08),~
               at (07,69), fac(hex(84))  , l_d$(k% + 4%, 5%)    , ch(08),~ 
                                                                         ~
               at (08,02), fac(hex(81))  , cc$(k% + 5%)         , ch(01),~
               at (08,05), fac(hex(84))  , dept$(k% + 5%)       , ch(03),~
               at (08,17), fac(hex(84))  , l_d$(k% + 5%, 1%)    , ch(08),~
               at (08,30), fac(hex(84))  , l_d$(k% + 5%, 2%)    , ch(08),~
               at (08,43), fac(hex(84))  , l_d$(k% + 5%, 3%)    , ch(08),~
               at (08,56), fac(hex(84))  , l_d$(k% + 5%, 4%)    , ch(08),~
               at (08,69), fac(hex(84))  , l_d$(k% + 5%, 5%)    , ch(08),~ 
                                                                         ~
               at (09,02), fac(hex(81))  , cc$(k% + 6%)         , ch(01),~
               at (09,05), fac(hex(84))  , dept$(k% + 6%)       , ch(03),~
               at (09,17), fac(hex(84))  , l_d$(k% + 6%, 1%)    , ch(08),~
               at (09,30), fac(hex(84))  , l_d$(k% + 6%, 2%)    , ch(08),~
               at (09,43), fac(hex(84))  , l_d$(k% + 6%, 3%)    , ch(08),~
               at (09,56), fac(hex(84))  , l_d$(k% + 6%, 4%)    , ch(08),~
               at (09,69), fac(hex(84))  , l_d$(k% + 6%, 5%)    , ch(08),~ 
                                                                         ~
               at (10,02), fac(hex(81))  , cc$(k% + 7%)         , ch(01),~
               at (10,05), fac(hex(84))  , dept$(k% + 7%)       , ch(03),~
               at (10,17), fac(hex(84))  , l_d$(k% + 7%, 1%)    , ch(08),~
               at (10,30), fac(hex(84))  , l_d$(k% + 7%, 2%)    , ch(08),~
               at (10,43), fac(hex(84))  , l_d$(k% + 7%, 3%)    , ch(08),~
               at (10,56), fac(hex(84))  , l_d$(k% + 7%, 4%)    , ch(08),~
               at (10,69), fac(hex(84))  , l_d$(k% + 7%, 5%)    , ch(08),~ 
                                                                         ~
               at (11,02), fac(hex(81))  , cc$(k% + 8%)         , ch(01),~
               at (11,05), fac(hex(84))  , dept$(k% + 8%)       , ch(03),~
               at (11,17), fac(hex(84))  , l_d$(k% + 8%, 1%)    , ch(08),~
               at (11,30), fac(hex(84))  , l_d$(k% + 8%, 2%)    , ch(08),~
               at (11,43), fac(hex(84))  , l_d$(k% + 8%, 3%)    , ch(08),~
               at (11,56), fac(hex(84))  , l_d$(k% + 8%, 4%)    , ch(08),~
               at (11,69), fac(hex(84))  , l_d$(k% + 8%, 5%)    , ch(08),~ 
                                                                         ~
               at (12,02), fac(hex(81))  , cc$(k% + 9%)         , ch(01),~
               at (12,05), fac(hex(84))  , dept$(k% + 9%)       , ch(03),~
               at (12,17), fac(hex(84))  , l_d$(k% + 9%, 1%)    , ch(08),~
               at (12,30), fac(hex(84))  , l_d$(k% + 9%, 2%)    , ch(08),~
               at (12,43), fac(hex(84))  , l_d$(k% + 9%, 3%)    , ch(08),~
               at (12,56), fac(hex(84))  , l_d$(k% + 9%, 4%)    , ch(08),~
               at (12,69), fac(hex(84))  , l_d$(k% + 9%, 5%)    , ch(08),~ 
                                                                         ~
               at (13,02), fac(hex(81))  , cc$(k% + 10%)        , ch(01),~
               at (13,05), fac(hex(84))  , dept$(k% + 10%)      , ch(03),~
               at (13,17), fac(hex(84))  , l_d$(k% + 10%, 1%)   , ch(08),~
               at (13,30), fac(hex(84))  , l_d$(k% + 10%, 2%)   , ch(08),~
               at (13,43), fac(hex(84))  , l_d$(k% + 10%, 3%)   , ch(08),~
               at (13,56), fac(hex(84))  , l_d$(k% + 10%, 4%)   , ch(08),~
               at (13,69), fac(hex(84))  , l_d$(k% + 10%, 5%)   , ch(08),~ 
                                                                         ~
               at (14,02), fac(hex(81))  , cc$(k% + 11%)        , ch(01),~
               at (14,05), fac(hex(84))  , dept$(k% + 11%)      , ch(03),~
               at (14,17), fac(hex(84))  , l_d$(k% + 11%, 1%)   , ch(08),~
               at (14,30), fac(hex(84))  , l_d$(k% + 11%, 2%)   , ch(08),~
               at (14,43), fac(hex(84))  , l_d$(k% + 11%, 3%)   , ch(08),~
               at (14,56), fac(hex(84))  , l_d$(k% + 11%, 4%)   , ch(08),~
               at (14,69), fac(hex(84))  , l_d$(k% + 11%, 5%)   , ch(08),~ 
                                                                         ~
               at (15,02), fac(hex(81))  , cc$(k% + 12%)        , ch(01),~
               at (15,05), fac(hex(84))  , dept$(k% + 12%)      , ch(03),~
               at (15,17), fac(hex(84))  , l_d$(k% + 12%, 1%)   , ch(08),~
               at (15,30), fac(hex(84))  , l_d$(k% + 12%, 2%)   , ch(08),~
               at (15,43), fac(hex(84))  , l_d$(k% + 12%, 3%)   , ch(08),~
               at (15,56), fac(hex(84))  , l_d$(k% + 12%, 4%)   , ch(08),~
               at (15,69), fac(hex(84))  , l_d$(k% + 12%, 5%)   , ch(08),~ 
                                                                         ~
               at (16,02), fac(hex(81))  , cc$(k% + 13%)        , ch(01),~
               at (16,05), fac(hex(84))  , dept$(k% + 13%)      , ch(03),~
               at (16,17), fac(hex(84))  , l_d$(k% + 13%, 1%)   , ch(08),~
               at (16,30), fac(hex(84))  , l_d$(k% + 13%, 2%)   , ch(08),~
               at (16,43), fac(hex(84))  , l_d$(k% + 13%, 3%)   , ch(08),~
               at (16,56), fac(hex(84))  , l_d$(k% + 13%, 4%)   , ch(08),~
               at (16,69), fac(hex(84))  , l_d$(k% + 13%, 5%)   , ch(08),~ 
                                                                         ~
               at (17,02), fac(hex(81))  , cc$(k% + 14%)        , ch(01),~
               at (17,05), fac(hex(84))  , dept$(k% + 14%)      , ch(03),~
               at (17,17), fac(hex(84))  , l_d$(k% + 14%, 1%)   , ch(08),~
               at (17,30), fac(hex(84))  , l_d$(k% + 14%, 2%)   , ch(08),~
               at (17,43), fac(hex(84))  , l_d$(k% + 14%, 3%)   , ch(08),~
               at (17,56), fac(hex(84))  , l_d$(k% + 14%, 4%)   , ch(08),~
               at (17,69), fac(hex(84))  , l_d$(k% + 14%, 5%)   , ch(08),~ 
                                                                         ~
               at (18,02), fac(hex(81))  , cc$(k% + 15%)        , ch(01),~
               at (18,05), fac(hex(84))  , dept$(k% + 15%)      , ch(03),~
               at (18,17), fac(hex(84))  , l_d$(k% + 15%, 1%)   , ch(08),~
               at (18,30), fac(hex(84))  , l_d$(k% + 15%, 2%)   , ch(08),~
               at (18,43), fac(hex(84))  , l_d$(k% + 15%, 3%)   , ch(08),~
               at (18,56), fac(hex(84))  , l_d$(k% + 15%, 4%)   , ch(08),~
               at (18,69), fac(hex(84))  , l_d$(k% + 15%, 5%)   , ch(08),~ 
                                                                         ~
               at (19,02), fac(hex(81))  , cc$(k% + 16%)        , ch(01),~
               at (19,05), fac(hex(84))  , dept$(k% + 16%)      , ch(03),~
               at (19,17), fac(hex(84))  , l_d$(k% + 16%, 1%)   , ch(08),~
               at (19,30), fac(hex(84))  , l_d$(k% + 16%, 2%)   , ch(08),~
               at (19,43), fac(hex(84))  , l_d$(k% + 16%, 3%)   , ch(08),~
               at (19,56), fac(hex(84))  , l_d$(k% + 16%, 4%)   , ch(08),~
               at (19,69), fac(hex(84))  , l_d$(k% + 16%, 5%)   , ch(08),~ 
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
L41060:           x% = int(val_max% / 16%)
                  k% = (x%*16%)
                  goto L41000

L41080:        if keyhit% <> 4% then goto L41100             /* Previous */
                  if k% < 17% then goto L41020
                  k% = k% - 16%
                  if k% <= 1% then goto L41020
                  goto L41000

L41100:        if keyhit% <> 5% then goto L41110             /* Next     */
                  k% = k% + 16%
                  if k% < val_max% then goto L41000
                  goto L41060

L41110:        if keyhit% <> 9% then goto L41115
                  tab% = 1%                                  /* Dept's   */ 
                  gosub display_codes
                  goto L41000

L41115:        if keyhit% <> 10% then goto L41120
                  gosub display_detail
                  goto L41000

L41120:        if keyhit% <> 15 then goto L41140
                  call "PRNTSCRN"
                  goto L41000

L41140:        if keyhit% <> 16% then goto L41000

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return clear all
        goto inputmode

        set_pf2
            rhh = 0%
            dsp_msg$=                                                                 ~
              "Total Plant On-Time Percent [xxxxxxx%] Checked [xxxxxx] Units [xxxxxx]"
            if total_plant > 0 then                                                   ~
               rhh = round((total_ontime / total_plant) * 100, 4)
            convert rhh to str(dsp_msg$,30%,8%), pic(###.###)
            convert cnt% to str(dsp_msg$,49%,6%), pic(######)
            convert total% to str(dsp_msg$,64%,6%), pic(######)

            option$ = "Production Wk=xx"
            str(option$,15%,2%) = sc_wk$
            title$ = "(Summary) Display by Dept. for All Loads"
            pageno$ = "Page: XX of XX"             /* k% = Array Values */
            h1$ = "Department  "
            h2$ = "Total Units "
            h3$ = "OnTime Units"
            h4$ = "Late Units  "
            h5$ = "Not Scanned "
            h6$ = " % On-Time  "

            val_max% = dept_max%
            if val_max% > (50% - 16%) then val_max% = 50% - 16%
                                                        /* Display Max */
            yy% = ( val_max% / 16% ) + 1%
            xx% = (k% / 16%) + 1%
            convert xx% to str(pageno$,7%,2%), pic(##) /* Current Page*/

            convert yy% to str(pageno$,13%,2%), pic(##)/* Total Pages */

            pf$(1) = "(2)First           (5)Next              " &        ~
                     "                                       "
            pf$(2) = "(3)Last            (9)Display Dept's    " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(4)Previous       (10)Display Detail-Use" &        ~
                     " 'X' for Selection?    (16)Exit Display" 
            pfkeys$ = hex(ff02030405ffffff090affffffff0f1000)
            gosub check_screen
            return

        check_screen
            if val_max% > 16% then goto L41160
               gosub no_first
               gosub no_next
               gosub no_last
               gosub no_prev
               return
L41160:      if k% >= 16% then goto L41180
                gosub no_first
                gosub no_prev
L41180:      if (k% + 16%) <= val_max% then goto L41200
                gosub no_last
L41200:      if k% <= (val_max% - 16%) then goto L41220
                gosub no_next
L41220: return
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
            *           D I S P L A Y   D E T A I L    S C R E E N      *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_detail
            k% = 0%
            for dp% = 1% to dept_max%
                if cc$(dp%) = "X" then goto L42000
            next dp%
            dp% = 1%                             /* Set Default to (1) */
  
L42000:     gosub set_pf3   
            accept                                                       ~
               at (01,02), fac(hex(84)), option$                , ch(16),~
               at (01,65), fac(hex(84)), pageno$                , ch(14),~
                                                                         ~
               at (01,21), fac(hex(a4)), title$                 , ch(40),~
                                                                         ~
               at (03,02), fac(hex(a4))  , h1$                  , ch(12),~
               at (03,15), fac(hex(a4))  , h2$                  , ch(12),~
               at (03,28), fac(hex(a4))  , h3$                  , ch(12),~
               at (03,41), fac(hex(a4))  , h4$                  , ch(12),~
               at (03,54), fac(hex(a4))  , h5$                  , ch(12),~
               at (03,67), fac(hex(a4))  , h6$                  , ch(12),~     
                                                                         ~
               at (04,02), fac(hex(81))  , cc$(k% + 1%)         , ch(01),~
               at (04,05), fac(hex(84))  , load$(k% + 1%)       , ch(05),~
               at (04,17), fac(hex(84))  , l_c1$(dp%, k% + 1%)  , ch(08),~
               at (04,30), fac(hex(84))  , l_c2$(dp%, k% + 1%)  , ch(08),~
               at (04,43), fac(hex(84))  , l_c3$(dp%, k% + 1%)  , ch(08),~
               at (04,56), fac(hex(84))  , l_c4$(dp%, k% + 1%)  , ch(08),~
               at (04,69), fac(hex(84))  , l_c5$(dp%, k% + 1%)  , ch(08),~ 
                                                                         ~
               at (05,02), fac(hex(81))  , cc$(k% + 2%)         , ch(01),~
               at (05,05), fac(hex(84))  , load$(k% + 2%)       , ch(05),~
               at (05,17), fac(hex(84))  , l_c1$(dp%, k% + 2%)  , ch(08),~
               at (05,30), fac(hex(84))  , l_c2$(dp%, k% + 2%)  , ch(08),~
               at (05,43), fac(hex(84))  , l_c3$(dp%, k% + 2%)  , ch(08),~
               at (05,56), fac(hex(84))  , l_c4$(dp%, k% + 2%)  , ch(08),~
               at (05,69), fac(hex(84))  , l_c5$(dp%, k% + 2%)  , ch(08),~ 
                                                                         ~
               at (06,02), fac(hex(81))  , cc$(k% + 3%)         , ch(01),~
               at (06,05), fac(hex(84))  , load$(k% + 3%)       , ch(05),~
               at (06,17), fac(hex(84))  , l_c1$(dp%, k% + 3%)  , ch(08),~
               at (06,30), fac(hex(84))  , l_c2$(dp%, k% + 3%)  , ch(08),~
               at (06,43), fac(hex(84))  , l_c3$(dp%, k% + 3%)  , ch(08),~
               at (06,56), fac(hex(84))  , l_c4$(dp%, k% + 3%)  , ch(08),~
               at (06,69), fac(hex(84))  , l_c5$(dp%, k% + 3%)  , ch(08),~ 
                                                                         ~
               at (07,02), fac(hex(81))  , cc$(k% + 4%)         , ch(01),~
               at (07,05), fac(hex(84))  , load$(k% + 4%)       , ch(05),~
               at (07,17), fac(hex(84))  , l_c1$(dp%, k% + 4%)  , ch(08),~
               at (07,30), fac(hex(84))  , l_c2$(dp%, k% + 4%)  , ch(08),~
               at (07,43), fac(hex(84))  , l_c3$(dp%, k% + 4%)  , ch(08),~
               at (07,56), fac(hex(84))  , l_c4$(dp%, k% + 4%)  , ch(08),~
               at (07,69), fac(hex(84))  , l_c5$(dp%, k% + 4%)  , ch(08),~ 
                                                                         ~
               at (08,02), fac(hex(81))  , cc$(k% + 5%)         , ch(01),~
               at (08,05), fac(hex(84))  , load$(k% + 5%)       , ch(05),~
               at (08,17), fac(hex(84))  , l_c1$(dp%, k% + 5%)  , ch(08),~
               at (08,30), fac(hex(84))  , l_c2$(dp%, k% + 5%)  , ch(08),~
               at (08,43), fac(hex(84))  , l_c3$(dp%, k% + 5%)  , ch(08),~
               at (08,56), fac(hex(84))  , l_c4$(dp%, k% + 5%)  , ch(08),~
               at (08,69), fac(hex(84))  , l_c5$(dp%, k% + 5%)  , ch(08),~ 
                                                                         ~
               at (09,02), fac(hex(81))  , cc$(k% + 6%)         , ch(01),~
               at (09,05), fac(hex(84))  , load$(k% + 6%)       , ch(05),~
               at (09,17), fac(hex(84))  , l_c1$(dp%, k% + 6%)  , ch(08),~
               at (09,30), fac(hex(84))  , l_c2$(dp%, k% + 6%)  , ch(08),~
               at (09,43), fac(hex(84))  , l_c3$(dp%, k% + 6%)  , ch(08),~
               at (09,56), fac(hex(84))  , l_c4$(dp%, k% + 6%)  , ch(08),~
               at (09,69), fac(hex(84))  , l_c5$(dp%, k% + 6%)  , ch(08),~ 
                                                                         ~
               at (10,02), fac(hex(81))  , cc$(k% + 7%)         , ch(01),~
               at (10,05), fac(hex(84))  , load$(k% + 7%)       , ch(05),~
               at (10,17), fac(hex(84))  , l_c1$(dp%, k% + 7%)  , ch(08),~
               at (10,30), fac(hex(84))  , l_c2$(dp%, k% + 7%)  , ch(08),~
               at (10,43), fac(hex(84))  , l_c3$(dp%, k% + 7%)  , ch(08),~
               at (10,56), fac(hex(84))  , l_c4$(dp%, k% + 7%)  , ch(08),~
               at (10,69), fac(hex(84))  , l_c5$(dp%, k% + 7%)  , ch(08),~ 
                                                                         ~
               at (11,02), fac(hex(81))  , cc$(k% + 8%)         , ch(01),~
               at (11,05), fac(hex(84))  , load$(k% + 8%)       , ch(05),~
               at (11,17), fac(hex(84))  , l_c1$(dp%, k% + 8%)  , ch(08),~
               at (11,30), fac(hex(84))  , l_c2$(dp%, k% + 8%)  , ch(08),~
               at (11,43), fac(hex(84))  , l_c3$(dp%, k% + 8%)  , ch(08),~
               at (11,56), fac(hex(84))  , l_c4$(dp%, k% + 8%)  , ch(08),~
               at (11,69), fac(hex(84))  , l_c5$(dp%, k% + 8%)  , ch(08),~ 
                                                                         ~
               at (12,02), fac(hex(81))  , cc$(k% + 9%)         , ch(01),~
               at (12,05), fac(hex(84))  , load$(k% + 9%)       , ch(05),~
               at (12,17), fac(hex(84))  , l_c1$(dp%, k% + 9%)  , ch(08),~
               at (12,30), fac(hex(84))  , l_c2$(dp%, k% + 9%)  , ch(08),~
               at (12,43), fac(hex(84))  , l_c3$(dp%, k% + 9%)  , ch(08),~
               at (12,56), fac(hex(84))  , l_c4$(dp%, k% + 9%)  , ch(08),~
               at (12,69), fac(hex(84))  , l_c5$(dp%, k% + 9%)  , ch(08),~ 
                                                                         ~
               at (13,02), fac(hex(81))  , cc$(k% + 10%)        , ch(01),~
               at (13,05), fac(hex(84))  , load$(k% + 10%)      , ch(05),~
               at (13,17), fac(hex(84))  , l_c1$(dp%, k% + 10%) , ch(08),~
               at (13,30), fac(hex(84))  , l_c2$(dp%, k% + 10%) , ch(08),~
               at (13,43), fac(hex(84))  , l_c3$(dp%, k% + 10%) , ch(08),~
               at (13,56), fac(hex(84))  , l_c4$(dp%, k% + 10%) , ch(08),~
               at (13,69), fac(hex(84))  , l_c5$(dp%, k% + 10%) , ch(08),~ 
                                                                         ~
               at (14,02), fac(hex(81))  , cc$(k% + 11%)        , ch(01),~
               at (14,05), fac(hex(84))  , load$(k% + 11%)      , ch(05),~
               at (14,17), fac(hex(84))  , l_c1$(dp%, k% + 11%) , ch(08),~
               at (14,30), fac(hex(84))  , l_c2$(dp%, k% + 11%) , ch(08),~
               at (14,43), fac(hex(84))  , l_c3$(dp%, k% + 11%) , ch(08),~
               at (14,56), fac(hex(84))  , l_c4$(dp%, k% + 11%) , ch(08),~
               at (14,69), fac(hex(84))  , l_c5$(dp%, k% + 11%) , ch(08),~ 
                                                                         ~
               at (15,02), fac(hex(81))  , cc$(k% + 12%)        , ch(01),~
               at (15,05), fac(hex(84))  , load$(k% + 12%)      , ch(05),~
               at (15,17), fac(hex(84))  , l_c1$(dp%, k% + 12%) , ch(08),~
               at (15,30), fac(hex(84))  , l_c2$(dp%, k% + 12%) , ch(08),~
               at (15,43), fac(hex(84))  , l_c3$(dp%, k% + 12%) , ch(08),~
               at (15,56), fac(hex(84))  , l_c4$(dp%, k% + 12%) , ch(08),~
               at (15,69), fac(hex(84))  , l_c5$(dp%, k% + 12%) , ch(08),~ 
                                                                         ~
               at (16,02), fac(hex(81))  , cc$(k% + 13%)        , ch(01),~
               at (16,05), fac(hex(84))  , load$(k% + 13%)      , ch(05),~
               at (16,17), fac(hex(84))  , l_c1$(dp%, k% + 13%) , ch(08),~
               at (16,30), fac(hex(84))  , l_c2$(dp%, k% + 13%) , ch(08),~
               at (16,43), fac(hex(84))  , l_c3$(dp%, k% + 13%) , ch(08),~
               at (16,56), fac(hex(84))  , l_c4$(dp%, k% + 13%) , ch(08),~
               at (16,69), fac(hex(84))  , l_c5$(dp%, k% + 13%) , ch(08),~ 
                                                                         ~
               at (17,02), fac(hex(81))  , cc$(k% + 14%)        , ch(01),~
               at (17,05), fac(hex(84))  , load$(k% + 14%)      , ch(05),~
               at (17,17), fac(hex(84))  , l_c1$(dp%, k% + 14%) , ch(08),~
               at (17,30), fac(hex(84))  , l_c2$(dp%, k% + 14%) , ch(08),~
               at (17,43), fac(hex(84))  , l_c3$(dp%, k% + 14%) , ch(08),~
               at (17,56), fac(hex(84))  , l_c4$(dp%, k% + 14%) , ch(08),~
               at (17,69), fac(hex(84))  , l_c5$(dp%, k% + 14%) , ch(08),~ 
                                                                         ~
               at (18,02), fac(hex(81))  , cc$(k% + 15%)        , ch(01),~
               at (18,05), fac(hex(84))  , load$(k% + 15%)      , ch(05),~
               at (18,17), fac(hex(84))  , l_c1$(dp%, k% + 15%) , ch(08),~
               at (18,30), fac(hex(84))  , l_c2$(dp%, k% + 15%) , ch(08),~
               at (18,43), fac(hex(84))  , l_c3$(dp%, k% + 15%) , ch(08),~
               at (18,56), fac(hex(84))  , l_c4$(dp%, k% + 15%) , ch(08),~
               at (18,69), fac(hex(84))  , l_c5$(dp%, k% + 15%) , ch(08),~ 
                                                                         ~
               at (19,02), fac(hex(81))  , cc$(k% + 16%)        , ch(01),~
               at (19,05), fac(hex(84))  , load$(k% + 16%)      , ch(05),~
               at (19,17), fac(hex(84))  , l_c1$(dp%, k% + 16%) , ch(08),~
               at (19,30), fac(hex(84))  , l_c2$(dp%, k% + 16%) , ch(08),~
               at (19,43), fac(hex(84))  , l_c3$(dp%, k% + 16%) , ch(08),~
               at (19,56), fac(hex(84))  , l_c4$(dp%, k% + 16%) , ch(08),~
               at (19,69), fac(hex(84))  , l_c5$(dp%, k% + 16%) , ch(08),~ 
                                                                         ~
               at (21,02), fac(hex(a4)),   dsp_msg$             , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~  
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L42040             /* First    */
L42020:           k% = 0%
                  goto L42000

L42040:        if keyhit% <> 3% then goto L42080             /* Last      */
L42060:           x% = int(val_max% / 16%)
                  k% = (x%*16%)
                  goto L42000

L42080:        if keyhit% <> 4% then goto L42100             /* Previous */
                  if k% < 17% then goto L42020
                  k% = k% - 16%
                  if k% <= 1% then goto L42020
                  goto L42000

L42100:        if keyhit% <> 5% then goto L42120             /* Next     */
                  k% = k% + 16%
                  if k% < val_max% then goto L42000
                  goto L42060

L42120:        if keyhit% <> 15 then goto L42140
                  call "PRNTSCRN"
                  goto L42000

L42140:        if keyhit% <> 10 then goto L42150
                  gosub display_not_scanned
                  k% = 0%
                  goto L42000

L42150:        if keyhit% <> 16% then goto L42000

               init(" ") cc$()
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               k% = 0%
        return 

        set_pf3
            init(" ") cc$()
            rhh = 0%
            dsp_msg$=                                                                 ~
              "Total Plant On-Time Percent [xxxxxxx%] Checked [xxxxxx] Units [xxxxxx]"
            if total_plant > 0 then                                                   ~
               rhh = round((total_ontime / total_plant) * 100, 4)
            convert rhh to str(dsp_msg$,30%,8%), pic(###.###)
            convert cnt% to str(dsp_msg$,49%,6%), pic(######)
            convert total% to str(dsp_msg$,64%,6%), pic(######)

            option$ = "[Loads for(xxx)]"
            str(option$,12%,3%) = dept$(dp%)
            sc_dept$ = dept$(dp%)
            gosub check_dept
            title$ = "(Detail) For "& str(desc$,1%,27%)
            pageno$ = "Page: XX of XX"             /* k% = Array Values */
            h1$ = "Load Number "
            h2$ = "Total Units "
            h3$ = "OnTime Units"
            h4$ = "Late Units  "
            h5$ = "Not Scanned "
            h6$ = " % On-Time  "

            val_max% = load_max%
            if val_max% > (125% - 16%) then val_max% = 125% - 16%
                                                        /* Display Max */
            yy% = ( val_max% / 16% ) + 1%
            xx% = (k% / 16%) + 1%
            convert xx% to str(pageno$,7%,2%), pic(##) /* Current Page*/

            convert yy% to str(pageno$,13%,2%), pic(##)/* Total Pages */

            pf$(1) = "(2)First           (5)Next              " &        ~
                     "                                       "
            pf$(2) = "(3)Last                                 " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(4)Previous       (10)Disp Not Scanned-U" &        ~
                     "se 'X' for Selection?  (16)Exit Display" 
            pfkeys$ = hex(ff02030405ffffffff0affffffff0f1000)
            gosub check_screen
            return

        REM *************************************************************~
            *    D I S P L A Y   N O T   S C A N N E D   S C R E E N    *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_not_scanned
            k% = 0%
            for ld% = 1% to load_max%
                if cc$(ld%) = "X" then goto L42900
            next ld%
            ld% = 1%                             /* Set Default to (1) */
L42900: gosub find_not_scanned
  
L43000:     gosub set_pf4   
            accept                                                       ~
               at (01,02), fac(hex(84)), option$                , ch(24),~
               at (01,65), fac(hex(84)), pageno$                , ch(14),~
                                                                         ~
               at (01,29), fac(hex(a4)), title$                 , ch(25),~
                                                                         ~
               at (03,04), fac(hex(a4))  , h1$                  , ch(12),~
               at (03,27), fac(hex(a4))  , h2$                  , ch(09),~
               at (03,37), fac(hex(a4))  , h3$                  , ch(08),~
               at (03,46), fac(hex(a4))  , h4$                  , ch(05),~
               at (03,52), fac(hex(a4))  , h5$                  , ch(12),~
               at (03,78), fac(hex(a4))  , h6$                  , ch(03),~            
                                                                         ~
               at (04,04), fac(hex(84))  , dt$(k% + 1%)         , ch(75),~
                                                                         ~
               at (05,04), fac(hex(84))  , dt$(k% + 2%)         , ch(75),~
                                                                         ~
               at (06,04), fac(hex(84))  , dt$(k% + 3%)         , ch(75),~
                                                                         ~
               at (07,04), fac(hex(84))  , dt$(k% + 4%)         , ch(75),~
                                                                         ~
               at (08,04), fac(hex(84))  , dt$(k% + 5%)         , ch(75),~
                                                                         ~
               at (09,04), fac(hex(84))  , dt$(k% + 6%)         , ch(75),~
                                                                         ~
               at (10,04), fac(hex(84))  , dt$(k% + 7%)         , ch(75),~
                                                                         ~
               at (11,04), fac(hex(84))  , dt$(k% + 8%)         , ch(75),~
                                                                         ~
               at (12,04), fac(hex(84))  , dt$(k% + 9%)         , ch(75),~
                                                                         ~
               at (13,04), fac(hex(84))  , dt$(k% + 10%)        , ch(75),~
                                                                         ~
               at (14,04), fac(hex(84))  , dt$(k% + 11%)        , ch(75),~
                                                                         ~
               at (15,04), fac(hex(84))  , dt$(k% + 12%)        , ch(75),~
                                                                         ~
               at (16,04), fac(hex(84))  , dt$(k% + 13%)        , ch(75),~
                                                                         ~
               at (17,04), fac(hex(84))  , dt$(k% + 14%)        , ch(75),~
                                                                         ~
               at (18,04), fac(hex(84))  , dt$(k% + 15%)        , ch(75),~
                                                                         ~
               at (19,04), fac(hex(84))  , dt$(k% + 16%)        , ch(75),~
                                                                         ~
               at (21,02), fac(hex(a4)),   dsp_msg$             , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~  
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L43040             /* First    */
L43020:           k% = 0%
                  goto L43000

L43040:        if keyhit% <> 3% then goto L43080             /* Last      */
L43060:           x% = int(val_max% / 16%)
                  k% = (x%*16%)
                  goto L43000

L43080:        if keyhit% <> 4% then goto L43100             /* Previous */
                  if k% < 17% then goto L43020
                  k% = k% - 16%
                  if k% <= 1% then goto L43020
                  goto L43000

L43100:        if keyhit% <> 5% then goto L43120             /* Next     */
                  k% = k% + 16%
                  if k% < val_max% then goto L43000
                  goto L43060

L43120:        if keyhit% <> 15 then goto L43140
                  call "PRNTSCRN"
                  goto L43000

L43140:        if keyhit% <> 16% then goto L43000

               init(" ") cc$()
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               k% = 0%
        return 

        set_pf4
            init(" ") dsp_msg$, h1$, h2$, h3$, h4$, h5$, h6$, title$
            init(" ") cc$()

            rhh = 0%
            dsp_msg$=                                                                 ~
              "Total Number of Windows Found [xxxxxxxx] "
            convert dt_max% to str(dsp_msg$,32%,8%), pic(########)

            str(dsp_msg$,42%,31%) = "'*' = Not Scanned,'@' = Loaded?"
            option$ = "Not Scanned Load [xxxxx]"
            str(option$,19%,5%) = ck_load$
            title$ = "(Not Scanned) For ["& sav_dept$ & "]"
            pageno$ = "Page: XX of XX"             /* k% = Array Values */
            h1$ = "<-Barcode ->"
            h2$ = "Customer "
            h3$ = "Prod.dte"
            h4$ = "Seq. "
            h5$ = "Part Number "
            h6$ = "St."

            val_max% = dt_max%
            if val_max% > (125% - 16%) then val_max% = 125% - 16%
                                                        /* Display Max */
            yy% = ( val_max% / 16% ) + 1%
            xx% = (k% / 16%) + 1%
            convert xx% to str(pageno$,7%,2%), pic(##) /* Current Page*/

            convert yy% to str(pageno$,13%,2%), pic(##)/* Total Pages */

            pf$(1) = "(2)First           (5)Next              " &        ~
                     "                                       "
            pf$(2) = "(3)Last                                 " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(4)Previous                             " &        ~
                     "                       (16)Exit Display" 
            pfkeys$ = hex(ff02030405ffffffffffffffffff0f1000)
            gosub check_screen
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50010,        /* Department Code / ALL  */~
                              L50050,        /* Load Number / All      */~
                              L50092,        /* Ending Load Number     */

            return

L50010: Rem Enter a Valid Department Code          sc_dept$, sc_dept_d$
            init(" ") sc_dept_d$
            if len(sc_dept$) > 2 then goto L50030
L50020:        sc_dept$   = "ALL"            /* Means all Loads Table  */   
               sc_dept_D$ = "(All) - Departments?"
               return
L50030:     if str(sc_dept$,1%,1%) = "A" then goto L50020    
            gosub check_dept
            if dept% = 0% then goto L50040
               sc_dept_d$ = desc$
            dept_max% = 1%
        return
L50040:     errormsg$ = "(Error) Invalid Department Code?"
            gosub error_prompt
            init(" ") sc_dept$, sc_dept_d$
        return

L50050: REM Enter Valid Load Number                sc_load$, wc_load_d$
            init(" ") sc_load_d$
            if len(sc_load$) > 2 then goto L50070
L50060:        sc_load$ = "ALL  "
               sc_load_d$ = "(All) - Production Loads?"
               sc_wk% = 0%
               convert sc_wk$ to sc_wk%, data goto L50090

               if sc_wk% < 1% or sc_wk% > 53% then goto L50090 
               return

L50070:     if str(sc_load$,1%,1%) = "A" then goto L50060
               ck_load$ = sc_load$
               gosub check_load
               if load% = 0% then goto L50080 
                  sc_load_d$ = desc$
        return
L50080:     errormsg$ = "(Error) Invalid Load Number?"
            gosub error_prompt
            init(" ") sc_load$, sc_load_d$, ed_load$, ed_load_d$
        return
L50090:     errormsg$ = "(Error) Invalid Production Week (Required)?"
            init(" ") sc_load$, sc_wk$
        return

L50092: REM Ending Load Number                     ed_load$, ed_load_d$
            init(" ") ed_load_d$
            if str(sc_load$,1%,1%) = "A" then goto L50094
            if len(ed_load$) > 2 then goto L50096
L50094:        ed_load$ = sc_load$
               ed_load_d$ = sc_load_d$
               return 
L50096:     if str(ed_load$,1%,1%) = "A" then goto L50094
            if sc_load$ = ed_load$ then goto L50094
               ck_load$ = ed_load$
               gosub check_load
               if load% = 0% then goto L50080
               ed_load_d$ = desc$
        return

        check_dept
            dept% = 0%
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "PLAN DEPT"
            str(readkey$,10%,15%) = sc_dept$
            read #4,key = readkey$, using L51000, desc$, eod goto L51010
L51000:        FMT POS(25), CH(30)
            dept% = 1%
L51010: return

        check_load
            load% = 0%
            init(" ") readkey$, desc$
            str(readkey$,1%,5%) = ck_load$
            read #5,key = readkey$, using L51020, desc$, sc_wk$,       ~
                                                        eod goto L51030
L51020:        FMT POS(16), CH(30), POS(106), CH(2)
            load% = 1%
L51030: return 

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        find_not_scanned
            call "SHOSTAT" ("Analyizing Not Scanned")
            dt% = 0% : dt_max% = 0% 
            init(" ") dt_key$, x$, dt$(), ck_load$, sav_dept$, sav_or_key$
            ck_load$ = load$(ld%)
            str(dt_key$,1%,5%) = ck_load$
            sav_dept$ = dept$(dp%)
            read #3,key 3% > dt_key$, using L60000, dt_rec$,             ~
                                           eod goto find_not_scanned_done
L60000:        FMT CH(256)
            goto L60010 
        find_not_scanned_nxt
            read #3, using L60000, dt_rec$,eod goto find_not_scanned_done

L60010:     dt_key$ = str(dt_rec$,1%,23%)
            if str(dt_key$,1%,5%) <> ck_load$ then                       ~
                                           goto find_not_scanned_done
            dt_dept$ = str(dt_rec$,42%,3%)
            if dt_dept$ <> sav_dept$ then                               ~
                                           goto find_not_scanned_nxt
            sav_bar$ = str(dt_rec$,24%,18%)
            dt_part$ = str(dt_rec$,189%,25%)
                                                 /* (EWD002) Skip Glass*/
            if str(dt_part$,5%,4%) = "WARR" then /* Glass Warranty     */~
                                           goto find_not_scanned_nxt
               gosub check_samples               /* Skip Repairs       */
               if hit% = 1% then goto find_not_scanned_nxt
               dt_date$ = str(dt_rec$,47%,6%)    /* (EWD002)           */
               dt_st$   = str(dt_rec$,64%,2%)
               dt_seq$  = str(dt_rec$,111%,5%)
               dt_cust$ = str(dt_rec$,124%,9%)
                                                 /* 1st 'Not' Scanned  */
               gosub find_audit_data
               if sav_unit% = 1% then goto find_not_scanned_nxt

            dt% = dt% + 1%
            if dt% > 300% then dt% = 300%        /* MFG Barcode        */   
            str(dt$(dt%),1%,18%)  = sav_bar$
                                                 /* Customer Code      */
            str(dt$(dt%),24%,9%)  = dt_cust$
                                                 /* Production Date    */
            str(dt$(dt%),34%,8%)  = dt_date$
            call "DATEFMT" (str(dt$(dt%),34%,8%))
                                                 /* Seq. Number        */
            str(dt$(dt%),43%,5%)  = dt_seq$
                                                 /* MFG Part Number    */
            str(dt$(dt%),49%,25%) = dt_part$
            str(dt$(dt%),75%,1%) = "*"
            if dt_st$ > "12" then str(dt$(dt%),75%,1%)="@"
                                             /* Must be Loaded         */
            goto find_not_scanned_nxt
        find_not_scanned_done
               dt_max% = dt%
        return
      
        find_audit_data
            init(" ") ad_key1$
            sav_unit% = 0%                       /* Not Scanned        */
            ad_key1$ = all(hex(00))              /* For a Barcode and  */
            str(ad_key1$,1%,18%) = sav_bar$      /* and Specified Dept */
            read #1,key 1% > ad_key1$, using L63000, ad_key1$,           ~
                                       eod goto find_audit_data_done
L63000:        FMT CH(33)
            goto L63010
        find_audit_data_nxt
            read #1, using L63000, ad_key1$,                             ~
                                          eod goto find_audit_data_done
                                            /* Only for Barcode Found */
L63010:     if str(ad_key1$,1%,18%) <> sav_bar$ then                    ~
                                              goto find_audit_data_done
                                            /* Only for Dept. Found   */
            if str(ad_key1$,25%,3%) <> sav_dept$ then                   ~
                                              goto find_audit_data_nxt
                                            /* Now Search for a '12'  */
                                            /*  at Least one (1)      */

               if str(ad_key1$,32%,2%) <> "12" then                     ~
                                              goto find_audit_data_nxt
                  sav_unit% = 1%            /* A '12' Found           */
        find_audit_data_done
        return

        display_codes
            call "APCPLN1B" (tab%, #4)
        return

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return


        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            end

