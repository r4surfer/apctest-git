        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLA60                             *~
            *  Creation Date     - 12/28/04                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie Gregory                     *~
            *                                                           *~
            *  Description       - Winding/Coil Data Report. Called from*~
            *                      AWDPLN60 to generate report.         *~
            *                      Prod. Date & Dept. Range Specified.  *~
            *                                                           *~
            *  Code Tables Used  - PLAN TDLU, PLAN DEPT, PLAN DBLE,     *~
            *                      PLAN BALC, PLAN LAMN, PLAN TLDF      *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/28/04 ! (New) Program - copied & mod AWDPLN60.   ! CMG *~
            *************************************************************

            sub "AWDPLA60" (#2, #3, #6, #4, #5, #9, #10, #1,             ~
                             #11, minth%, maxth%)

        dim                                                              ~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
            gen_key$50,                  /* Second Gencodes Key        */~
            wdtkey$5, hgtkey$5,          /* Width & Height for WC Key  */~
            sc_dept$3, sc_dept_d$30,     /* Department Code & Descr    */~
            sc_prddate_fr$10,            /* Production Date            */~
            sc_prddate_to$10,            /*       Range                */~
            sc_data_flg$1,               /* Flag to print addtl. data  */~
            rpt_date_rnge$24,            /* Date Range for Printing    */~
            prddate$10, sav_prddate$10,  /* Production Date from DT    */~
            sc_key$33, dept$3, seq$5,    /* SC Alt key (2)             */~
            dt_key$57,                   /* DT Alt key (1)             */~
            pl_key$15,                   /* Alt (3) (APCPLNDP)         */~
            wk_key$51, wk_rec$256,       /* Work file key, record area */~
            part$25,                     /* Mfg. Part No.              */~
            hinge$2,                     /* Hinge Code from Part No.   */~
            sngl$1,                      /* Single-Hung Window Flag    */~
            tddiff$1,                    /* Tube Diameter Diff Flag    */~
            windcl$(6)6,                 /* Winding/Coil Data(APCPLNWC)*/~
            tubedi$(6)2,                 /* Tube Diam Codes (APCPLNWC) */~
            co_flag$2,                   /* Flag for Cottage/Oriel     */~
            hd_flag$1,                   /* Flag for Heavy Duty Tube   */~
            sav_tht$9,                   /* Tube Height(s) for Compare */~
            hist_flag$9,                 /* Flag for Historical Report */~
            billto$9,                    /* Bill-To Customer Code      */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            time$8,                      /* System time                */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$10,                     /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3,                    /* Current User Id            */~
            prddate_fr$6,                /* Production Date            */~
            prddate_to$6                 /*       Range                */

        dim seqn$5,                      /* Production Sequence No.    */~
            mdl$3,                       /* Model No.                  */~
            sav_mdl$3,                   /* Save Model No.             */~
            wdt$7,                       /* Width                      */~
            hgt$7,                       /* Height                     */~
            gls$2,                       /* Glass No.                  */~
            scr$1,                       /* Screen No.                 */~
            bal_top$8,                   /* Top Balance                */~
            bal_bot$8,                   /* Bot Balance                */~
            tht$9,                       /* Tube Heights               */~
            tdi_top$2,                   /* Tube Diameter Code         */~
            tdi_bot$2,                   /* Tube Diameter Code         */~
            cus$9,                       /* Customer Code              */~
            inf$19                       /* Model/Customer Info        */~

        dim ctt$(10%,3%)9,               /* 1-5=Top, 6-10=Bot          */~
            gdd$(12%)10,                 /* 1-6=Top, 7-12=Bot          */~
            ajj$(10%,2%)6,               /* 1-5=Top, 6-10=Bot          */~
            dcc$(10%,2%)8,               /* 1-5=Top, 6-10=Bot          */~
            wd$7,                        /* Actual Width               */~
            ht$6                         /* Actual Height              */
            

        dim prod$4,                      /* Prodcut Key                */~
            vendor$1,                    /* Vendor for balance         */~
            bal_type$1,                  /* Type of balance for product*/~
            bal_key$27,                  /* AWDPLNWC Key               */~
            bal_key2$35,                 /* AWDPLNWC Key 2             */~
            bal_rec$256,                 /* BALANCE Record             */~
            strength$1,                  /* Glass Strength             */~
            td$2,                        /* Tube Diameter              */~
            v$1                          /* View Top or Bottom         */

        dim readbc$5,                    /* AWDPLNBC Readkey           */~
            unit_type$1,                 /* Unit Type S, Cottage, Oriel*/~
            static$1,                    /* Is it static               */~
            loc_top$14,                  /* Top Balance                */~
            loc_bot$14                   /* Bottom Balance             */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(AWD) Tube Windings/Coil Report         "
            pname$ = "AWDPLA60 - Rev: R7.00"      

        REM *************************************************************


                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! AMTBOMCD ! Master Equation File                     *~
            * #2  ! CUSTOMER ! Customer Master File                     *~
            * #3  ! APCPLNDT ! Production Master Detail file            *~
            * #4  ! GENCODES ! System Master Code Table Files           *~
            * #5  ! AWDPLNWC ! Production Windings & Coil File          *~
            * #6  ! APCPLNSC ! Planning Mawster Schedule file           *~
            * #7  ! APCPLNWK ! Work File                                *~
            * #8  ! APCPLWK2 ! Work File #2                             *~
            * #9  ! APCPLNDP ! Planning Master Dept File                *~
            * #10 ! AWDPLNBL ! Product and Balance Cross-Reference      *~
            * #11 ! AWDPLNBC ! Balance Location File                    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #7, "APCPLNWK",                                       ~
                        varc,     indexed,  recsize =   95,              ~
                        keypos =  1,   keylen = 51

            select #8, "APCPLWK2",                                       ~
                        varc,     indexed,  recsize =   32,              ~
                        keypos =  1,   keylen =  11


        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEOKC" (date$, today%, errormsg$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            time$ = " "  :  call "TIME" (time$)
            pg% = 0%  :  ln% = 99%
            select printer (134)

            work%, work2% = 0%      /* Work File has been opened flags */

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
                  if keyhit%  =  9% then goto  print_report /* Summary */
                  if keyhit%  = 10% then goto  print_report /* Detail */
                  if keyhit%  = 16% then goto  exit_program
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
                                                 /* Print Winding/Coil */ 
        print_report                             /* Production Report  */
            rpt_date_rnge$ = sc_prddate_fr$ & " TO " & sc_prddate_to$
            call "DATUFMTC" (sc_prddate_fr$)
            call "DATUFMTC" (sc_prddate_to$)
            sc_key$, dt_key$, wk_key$ = all(hex(00)) 
            str(sc_key$,,6) = sc_prddate_fr$
            str(dt_key$,,6) = sc_prddate_fr$
            call "SHOSTAT" ("Printing Winding/Coil Report...")
            cnt% = 0%
            seq% = 0%                                                     
            mode% = 1%  :  f% = 7%  :  gosub open_work
            if keyhit% <> 9% then L20100
                call "SHOSTAT" ("Printing Summary Report...")
                mode2% = 1%  :  f% = 8%  :  gosub open_work
L20100:     days%, result% = 0%                           
            prddate_fr$ = str(sc_prddate_fr$,1%,6%)       
            prddate_to$ = str(sc_prddate_to$,1%,6%)       
            call "DATE" addr("G-", prddate_fr$, prddate_to$, days%, result%)
            days% = abs(days%)
                                                          
            if days% <= 21% then gosub load_data_detail                   ~
                            else gosub load_data          
            goto exit_program


        load_data                                      
            read #6, key 2% > sc_key$, using L20210, wk_rec$,             ~
                    eod goto read_work_file
L20210:         fmt ch(128)
            if str(wk_rec$,1%,33%) <= sc_key$ then bad_data_key
            sc_key$ = str(wk_rec$,1%,33%)
            mdl$  = str(wk_rec$,34%,3%)                 /* Model        */
            part$ = str(wk_rec$,34%,25%)
            if len(part$) < 19 then goto load_data
            if str(sc_key$,,6%) > sc_prddate_to$ then goto read_work_file

                                                        /* Find Dept     */
            gosub find_dept 
            if dept% = 0% then goto load_data                
            if dept$ <> sc_dept$ and sc_dept$ <> "ALL" then               ~
                                                           goto load_data
                                                     /*Hist% = 0% = Std */
                                                     /*      = 1% = Hist*/
            if str(wk_rec$,110%,2%) > "11" and hist% = 0%                 ~
                then goto load_data /* Cmpltd  */

            if str(wk_rec$,110%,2%) < "03" then goto load_data
                                                     /* Not Planned    */ 
            if keyhit% <> 9% or sc_dept$ <> "ALL" then L20250  
                init(" ") readkey$                             
                str(readkey$,1%,9%)   = "PLAN TWAD"
                str(readkey$,10%,15%) = dept$         
                read #4, key = readkey$, eod goto load_data    
L20250:     get str(wk_rec$,70%,2%), using L20255, sc_mqty%
L20255:         FMT BI(2)

            if sc_mqty% < 1% then goto load_data

            cnt% = cnt% + 1%                    
            convert sc_mqty% to temp$, pic(####0)
            gosub write_work_file
            goto load_data


        load_data_detail
            read #3, key 1 > dt_key$, using L20215, wk_rec$,             ~
                    eod goto read_work_file
L20215:         fmt ch(256)
            if str(wk_rec$,47,57) <= dt_key$ then bad_data_key
            dt_key$ = str(wk_rec$,47,57)
            mdl$ = str(wk_rec$,189,3)
            if str(dt_key$,,6) > sc_prddate_to$ then goto read_work_file
               gosub find_dept 
            if dept% = 0% then goto load_data_detail
            if str(dt_key$,13,3) <> sc_dept$ and sc_dept$ <> "ALL"       ~
                then goto load_data_detail
            if str(dt_key$,18,2) > "11" and hist% = 0%                   ~
                then goto load_data_detail /* Cmpltd  */
            if keyhit% <> 9% or sc_dept$ <> "ALL" then L20260 
                init(" ") readkey$                            
                str(readkey$,1%,9%)   = "PLAN TWAD"
                str(readkey$,10%,15%) = str(dt_key$,13%,3%)   
                read #4, key = readkey$, eod goto load_data_detail
L20260:     cnt% = cnt% + 1%

            convert cnt% to temp$, pic(####0)
            if mod(cnt%,100%) <> 0% then goto L20265
               convert cnt% to temp$, pic(####0)
               print at (14,30), "Records Read:      " & temp$
L20265:


            gosub write_work_file_detail
            goto load_data_detail


        write_work_file_detail
REM            call "SHOSTAT" ("RECORD      " & str(wk_rec$,24%,18%))  stop
REM            call "SHOSTAT" ("WRITE DETAIL")  stop

            write #7, using L20300,                                      ~
                str(wk_rec$,47,6),          /* Production Date      */   ~
                str(wk_rec$,59,3),          /* Department Code      */   ~
                str(wk_rec$,111,5),         /* Prod. Sequence No.   */   ~
                str(wk_rec$,66,30),         /* User Defined Index   */   ~
                str(wk_rec$,104,2),         /* Shift Code           */   ~
                str(temp$,,5),              /* Count 'em up 1,2,3...*/   ~
                str(wk_rec$,24,10),         /* S.O. & Line Item     */   ~
                str(wk_rec$,124,9),         /* Customer Code        */   ~
                str(wk_rec$,189,25),        /* Part No.             */   ~
            eod goto L20310, data goto L20315

L20315:     return

        write_work_file
            seq% = seq% + 1%
            convert seq% to seq$, pic(00000)

            write #7, using L20300,                                      ~
                str(wk_rec$,1%,6%),         /* Production Date      */   ~
                dept$,                      /* Department Code      */   ~
                seq$,                       /* Prod. Sequence No.   */   ~
                " ",                        /* User Defined Index-30*/   ~
                "01",                       /* Shift Code           */   ~
                str(temp$,,5%),             /* Line Item Qty        */   ~
                str(wk_rec$,24%,10%),       /* S.O. & Line Item     */   ~
                str(wk_rec$,59%,9%),        /* Customer Code        */   ~
                part$,                      /* Part No.             */   ~
            eod goto L20310, data goto L20310

L20300:     fmt ch(6), ch(3), ch(5), ch(30), ch(2), ch(5), ch(10), ch(9),~
                ch(25)
L20310:     return




        bad_data_key
           errormsg$ = "Key Sequence Error: " & str(wk_rec$,1%,33%)
            call "SHOSTAT" (errormsg$)
            stop
            goto exit_program

        read_work_file
            if mode% = 2% then goto L20400
                mode% = 2%  :  f% = 7%  :  gosub open_work
L20400:     gosub clear_line
            read #7, key > wk_key$, using L20410, wk_key$,               ~
                cus$, part$, eod goto L20998
L20410:             fmt ch(51), xx(10), ch(9), ch(25)



            part_len% = 0% 
            part_len% = len(part$)

            init(" ") cus$
            sc_mqty% = 0% 
            convert str(wk_key$,47%,5%) to sc_mqty%, data goto L20415
L20415: 
            cnt% = cnt% + 1%
            if mod(cnt%,100%) <> 0% then goto L20420
               convert cnt% to temp$, pic(####0)
               print at (14,30), "Records Processed: " & temp$
L20420:
               convert cnt% to temp$, pic(####0)
REM            call "SHOSTAT" (" Read Work File -->" & temp$)  stop
            if part_len% < 19 then goto read_work_file

            if part_len% >= 22 then gosub set_sclmr

REM            call "SHOSTAT" ("FINDING Balance " & str(wk_key$,10%,5%))  stop

            seqn$ = str(wk_key$,10%,5%)
REM            if seqn$ = "00122" then call "SHOSTAT" (" SEQ --> " & seqn$)
REM            if seqn$ = "00122" then stop
            prddate$ = str(wk_key$,,6)
            call "DATFMTC" (prddate$)
            mdl$   = str(part$,,3)
            gls$   = str(part$,5,2)
            hinge$ = str(part$,9,2)
            scr$   = str(part$,11,1)
            wdt$   = str(part$,13,4)
            hgt$   = str(part$,17,3)
            sav_mdl$ = mdl$         
            convert wdt$ to wdt, data goto read_work_file
            
            convert wdt$ to sav_wdt, data goto read_work_file
            gosub lookup_model                               
            wdt = wdt / 10
            convert wdt to wdtkey$, pic(000.0)
            fctnl = wdt - int(wdt)          /* Convert to decimal      */
            fctnl = (fctnl / 8) * 10
            wdt = int(wdt) + fctnl
            convert wdt to wdt$, pic(##0.000)
            convert hgt$ to hgt, data goto read_work_file
            hgt = hgt / 10              
            convert hgt to hgtkey$, pic(000.0)
            fctnl = hgt - int(hgt)          /* Convert to decimal      */
            fctnl = (fctnl / 8) * 10
            hgt = int(hgt) + fctnl         /* Reused in calc_tube_hgts */ 
            convert hgt to hgt$, pic(##0.000)


            gosub check_ctg_orl
            gosub calc_glass_size
            gosub find_balance


goto new_bal



new_bal:    
REM            tdi$ = tubedi$(j%)
REM            if wcl$ = " " then wcl$ = "."
            if unit_type$ = "C" or unit_type$ = "O"  ~
                        then  str(inf$,15%,1%) = "/"

            call "STRING" addr("RJ", wcl$, 6%)      /* Both steps reqd.*/
            call "STRING" addr("CT", wcl$, 6%)      /* to center data. */
            call "STRING" addr("RJ", bal_top$, 8%)  /* Both steps reqd.*/
            call "STRING" addr("CT", bal_top$, 8%)  /* to center data. */ 
            call "STRING" addr("RJ", bal_bot$, 8%)  /* Both steps reqd.*/
            call "STRING" addr("CT", bal_bot$, 8%)  /* to center data. */  

            if tdi$ <> "99" then gosub calc_tube_hgts
            if tht$ = "." and co_flag$ <> " " then tht$ = co_flag$
            if str(inf$,1%,14%) <= "." then gosub load_color


            if str(part$,11%,1%) = "4" or str(part$,11%,1%) = "5"  ~
                  then gosub add_tso_desc
            if str(part$,11%,1%) = "6" then gosub add_tso_desc 


            mdl$ = sav_mdl$
            wdt = sav_wdt
            wdt = wdt / 10
            convert wdt to wdtkey$, pic(000.0)
            fctnl = wdt - int(wdt)          /* Convert to decimal      */
            fctnl = (fctnl / 8) * 10
            wdt = int(wdt) + fctnl
            convert wdt to wdt$, pic(##0.000)  

            on keyhit% - 8% gosub build_summary, generate_report
            sav_prddate$ = prddate$
            goto read_work_file
           
L20998:     if cnt% = 0% then goto L20999
            if keyhit% = 9% then gosub generate_summary_report
            return

L20999:     errormsg$ = "No data to print for this date range/department."
            gosub error_prompt
            return clear all
            goto inputmode










            read_gencds2
               found% = 0%
               read #4, key = readkey$, eod goto L22290                    
               found% = 1%
L22290:        return



        calc_tube_hgts
            ht1%, ht2% = 999%  :  factor = 0
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "PLAN BALC"
            str(readkey$,10%,3%) = mdl$                      
            for x% = 1% to 2%                                
              if x% = 2% then str(readkey$,13%,2%) = tdi$                ~
                 else str(readkey$,13%,2%) = " "                        
              read #4,key=readkey$, using L22500, desc$, eod goto L22510
L22500:          FMT POS(25), CH(30)
L22510:     next x%                                          
            if desc$ = " " then goto L22580                 
            convert str(desc$,,7) to factor, data goto L22580
            sngl$ = str(desc$,10,1)
            ht2% = int((hgt/2) - factor)
            factor% = 2%                   
            init(" ") readkey$, desc$                      
            str(readkey$,1%,9%)   = "PLAN TLDF"
            str(readkey$,10%,15%) = mdl$ & tdi$
            read #4,key = readkey$, using L22500, desc$, eod goto L22540
            convert str(desc$,,3) to factor%, data goto L22540
L22540:     ht1% = ht2% - factor%          
*EWD            --- Previous location of code in check_ctg_orl ---
            if co_flag$ = "CO" then ht1% = ht1% * 0.8/*Cottage*/
*EWD        if co_flag$ = "OR" then ht2% = ht2% * 1.2/* Oriel */
            if ht1% < minth% then ht1% = minth%
            if ht1% > maxth% then ht1% = maxth%
            if ht2% < minth% then ht2% = minth%
            if ht2% > maxth% then ht2% = maxth%
            tht$ = "    -xxx "
            if co_flag$ <> " "                                           ~
                then str(tht$,5,1) = "/"       /* Denotes diff in calc */
L22580:     if sngl$ <> "S" or co_flag$ <> " "                           ~
                then convert ht1% to str(tht$,,3), pic(##0)
            convert ht2% to str(tht$,6,3), pic(##0)
            return


        load_color
            readkey$ = "COLOR    " & str(part$,4,1)
            read #4, key = readkey$, using L22600, str(inf$,1%,14%),  ~
                     eod goto L22610
L22600:          fmt pos(30), ch(15)
L22610:     return



        add_tso_desc
            p% = 0%
            p% = len(inf$) + 1%
            q% = (14% - p%)
                        
            if str(part$,11%,1%) = "4" then        ~
                 str(inf$,p%,q%) = " / TSO"
            if str(part$,11%,1%) = "5" then        ~
                 str(inf$,p%,q%) = " / BSO"
            if str(part$,11%,1%) = "6" then        ~
                 str(inf$,p%,q%) = " / FGO"

        return


        set_sclmr
               a1 = 0.0 : a2 = 0.0
               convert str(part$,20%,2%) to a1, data goto L24550

               convert str(part$,22%,1%) to a2, data goto L24560
L24560:
               s_clmr = a1 + (a2/8.0)
L24550:     if s_clmr <= 8.0 then s_clmr = 0.0
        return


        clear_line
            seqn$, mdl$, wdt$, hgt$, gls$, scr$, tdi$, bal_top$, bal_bot$,  ~
                tddiff$, co_flag$, tdi_top$, tdi_bot$ = " " 
            bal_top$, bal_bot$, tdi_top$, tdi_bot$, tht$, cus$, inf$ = "."
            sav_mdl$ = " "                               

            cvr_height, s_clmr = 0.00
            return



        build_summary                         
REM          call "SHOSTAT"( "BUILD SUMMARY " )  stop
          coil% = 0%                          

         for i% = 1% to 2%
          for x% = 1% to 6% step 5%
            readkey$ = " "  :  tot%, hit% = 0%     
            str(readkey$,1%,8%) = bal_top$
            if i% = 2% then str(readkey$,1%,8%) = bal_bot$

            if readkey$ = " " then L23300
            str(readkey$,9%,2%) = tdi_top$       
            if i% = 2% then str(readkey$,9%,2%) = tdi_bot$      

            if pos(bal_top$ = "H") <> 0% then str(readkey$,11%,1%) = "H"
            if pos(bal_bot$ = "H") <> 0% then str(readkey$,11%,1%) = "H"

             

               gosub lookup_block_tackle   
REM               call "SHOSTAT" ("READKEY --> " & readkey$ ) 

 
           goto writewrkfile

REM           if str(wk_key$,7%,3%) <> "050" then goto writewrkfile 
REM           goto writecoil
          
      writewrkfile

            read #8, hold, key = readkey$, using L23100, readkey$, tot%, ~
                eod goto L23200, data goto L23400
L23100:             fmt CH(11), BI(4)        
            hit% = 1%                        
L23200:                                      

            
                        

   
            if days% <= 21% then tot% = tot% + 1%                       ~
                            else tot% = tot% + sc_mqty%   /* Line Item Qty    */


            if tdi$ = "99" then tot% = tot% + sc_mqty% /* Line item Qty */





            put #8, using L23100, readkey$, tot%
            if hit% = 0% then write #8, eod goto L23400, data goto L23400 ~
                else rewrite #8, data goto L23400 
            if tdi$ = "99" then L23400            
            if str(wk_key$,7%,3%) = "50" then goto writecoil
L23300:   next x%            
L23400:   
          next i%
          return


          writecoil
          init (" ")  readkey$           
          if coil% > 2% then return  
          coil% = coil% + 1%
             if coil% = 1% then str(readkey$,1%,6%) = str(wcl$,1%,3%)     ~
                else str(readkey$,1%,6%) = str(wcl$,4%,6%) 
             str(readkey$,7%,2%) = tdi$
             goto writewrkfile
         
                                                 

        lookup_model                             
            init(" ") readkey$, desc$          
            div_fact% = 0%
            fact = 0.00
            str(readkey$,1%,9%)   = "PLAN TMDL"
            str(readkey$,10%,15%) = mdl$
            read #4,key = readkey$, using L22700, desc$, eod goto no_model
L22700:        FMT POS(25), CH(30) 
            mdl$ = str(desc$,1%,3%)     
            convert str(desc$,12%,1%) to div_fact%, data goto no_model

            convert str(desc$,22%,5%) to fact, data goto no_model

            wdt = wdt / div_fact%
            wdt = wdt - fact

        no_model
        return                                       

        lookup_block_tackle
        return                        
            init(" ") gen_key$      
            str(gen_key$,1%,9%)   = "PLAN TBLK"
            str(gen_key$,10%,15%) = str(wk_key$,7%,3%)
            read #4,key = gen_key$, eod goto not_block_tackle

REM                  convert x% to cmg$, pic(0)
REM                  call "SHOSTAT" (" X --> " & cmg$)  stop

                  init(" ") wcl$
                  if x% = 1 then str(wcl$,4%,3%) = str(windcl$(j%),1%,3%) ~ 
                     else str(wcl$,4%,3%) = str(windcl$(j%),4%,3%)

                  str(readkey$,,6%) = wcl$ 
REM                  call "SHOSTAT" (" READKEY --> " & readkey$)  stop
        not_block_tackle
        return                                    


        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
            if fieldnr% = 3% and sc_data_flg$ = " " then sc_data_flg$="Y"
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

        scrn1_msg  :  data                                               ~
         "Enter a Production Date Range                                ",~
         "Enter a Valid Department Code or 'ALL' (Summary Report Only) ",~
         "Enter 'Y' for Glass, Screen & Model/Cust Info else Enter 'N'."

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, sc_dept$, sc_dept_d$,      ~
                      prddate$, part$, windcl$(), billto$, sc_data_flg$, ~
                      seqn$, mdl$, wdt$, hgt$, gls$, scr$, hist_flag$,   ~
                      tdi$, sc_prddate_fr$, sc_prddate_to$, tubedi$(),   ~
                      prddate_fr$, prddate_to$

            init(" ") wcl$, tht$, cus$, inf$ 

            f% = 7%                            
            if work%  <> 0% then gosub delete_work
            f% = 8%                              
            if work2% <> 0% then gosub delete_work

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
              on fieldnr% gosub L40170,          /* sc_prddate$        */~
                                L40160,          /* sc_dept$           */~
                                L40160           /* sc_data_flg$       */

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40170:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,24), fac(hex(a4)), apc$                   , ch(39),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Prod. Date Range:",                          ~
               at (03,25), fac(lfac$(1%)), sc_prddate_fr$       , ch(10),~
               at (03,40), fac(lfac$(1%)), sc_prddate_to$       , ch(10),~
               at (03,55), fac(hex(84)),   hist_flag$           , ch(09),~
                                                                         ~
               at (04,02), "Dept Code:",                                 ~
               at (04,25), fac(lfac$(2%)), sc_dept$             , ch(03),~
               at (04,40), fac(hex(84)),   sc_dept_d$           , ch(30),~
                                                                         ~
               at (05,02), "Print Addtl. Data?:",                        ~
               at (05,25), fac(lfac$(3%)), sc_data_flg$         , ch(01),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <> 15 then goto L40420
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
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                  (9)Print Summary Repor" &        ~
                     "t                      (15)Print Screen"
            pf$(3) = "                 (10)Print Detail Report" &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffff090affffffff0f1000)
            if sc_dept$ <> "ALL" then L40690
                str(pf$(3),18,23) = " "  :  str(pfkeys$,10,1) = hex(ff)
L40690:     return

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
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50010,        /* Production Date Range  */~
                              L50050,        /* Department Code        */~
                              L50090         /* Print Addtl. Data?     */

            return

L50010: Rem Enter a Production Date Range          sc_prddate_..$
            hist% = 0%  :  hist_flag$ = " "
            call "DATEOKC" (sc_prddate_fr$, date_fr%, errormsg$)
                if errormsg$ <> " " then goto L50015
            if sc_prddate_to$ = " " then sc_prddate_to$ = sc_prddate_fr$
            call "DATEOKC" (sc_prddate_to$, date_to%, errormsg$)
                if errormsg$ <> " " then goto L50015
            if date_fr% > date_to% then                                 ~
                errormsg$ = "'TO' date must be < or = 'FROM' date."
            if errormsg$ <> " " then goto L50015               
            if date_fr% > today% and date_to% > today% then goto L50015

ask_again:  k% = 2%
            call "ASKUSER" (k%,"**Select Format**","One/both of the dat" ~
              & "es you have entered are today or before.","Press <PF1>" ~
              & " to Create a Historical Report -OR-","Press <ENTER> to" ~
              & " Create a Standard Report.")
            if k% = 1% then hist% = 1%
            if k% <> 0% and hist% = 0% then goto ask_again
            if hist% <> 0% then hist_flag$ = "(HISTORY)"

L50015:     return

L50050: Rem Enter a Valid Department Code          sc_dept$, sc_dept_d$
            init(" ") sc_dept_d$
            if sc_dept$ <> "ALL" then goto L50060     
                sc_dept_d$ = "*** All Departments"    
                return                                
L50060:     gosub check_dept
            if dept% = 0% then goto L50080
               sc_dept_d$ = desc$
        return
L50080:     errormsg$ = "(Error) Invalid Department Code"
            gosub error_prompt
            init(" ") sc_dept$, sc_dept_d$
        return

L50090: Rem Enter 'Y' for Additional Data or 'N'    sc_data_flg$
            if sc_data_flg$ = "Y" or sc_data_flg$ = "N" then goto L50095
                errormsg$ = "Please Enter 'Y' for Yes or 'N' for No."
L50095:     return


        check_dept
            dept% = 0%
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "PLAN DEPT"
            str(readkey$,10%,15%) = sc_dept$
            read #4,key = readkey$, using L51000, desc$, eod goto L51010
L51000:        FMT POS(25), CH(30)
            dept% = 1%
L51010: return

        find_dept
            dept% = 0%
            init(" ") pl_key$, dept$
            str(pl_key$,1%,3%) = mdl$
        find_dept_nxt
            read #9,key 3% > pl_key$, using L51015,pl_key$, eod goto L51020
L51015:        FMT CH(15)
            if str(pl_key$,1%,3%) <> mdl$ then goto L51020
               gosub check_support
               if supp% = 1% then goto find_dept_nxt
               dept$ = str(pl_key$,11%,3%)               /* Dept Found     */
               dept% = 1%
L51020: return
 
        check_support
           supp% = 0%
           init(" ") readkey$
           str(readkey$,1%,9%)   = "PLAN SUPP"
           str(readkey$,10%,15%) = str(pl_key$,11%,3%)
           read #4,key = readkey$, eod goto check_support_done
           supp% = 1%
        check_support_done
        return

        REM *************************************************************~
            *          R E P O R T   G E N E R A T I O N                *~
            *************************************************************

        generate_report
            if ln% > 58% then gosub print_headings
            if prddate$ <> sav_prddate$ and ln% > 5%                     ~
                then gosub print_headings
            if sc_data_flg$ = "N" then gls$, scr$, cus$, inf$ = " "
            readkey$ = "PLAN TDHL" & "D" & sc_dept$ & tdi$
                gosub read_gencds2
                if found% <> 0% then tddiff$ = "*"
            readkey$ = "PLAN TDHL" &"M"& mdl$ & tdi$
                gosub read_gencds2
                if found% <> 0% then tddiff$ = "*"

REM            call "SHOSTAT" ("SEQ     --> " & seqn$)  stop
REM            call "SHOSTAT" ("BAL TOP --> " & bal_top$)  stop
REM            call "SHOSTAT" ("BAL BOT --> " & bal_bot$)  stop

            print using L64040,seqn$, mdl$, wdt$, hgt$, gls$, scr$, ~
                bal_top$, tdi_top$, bal_bot$, tdi_bot$,             ~
                tddiff$, cus$, inf$, loc_top$, loc_bot$
            ln% = ln% + 1%
            return

        print_headings
            if pg% = 0% then apc$ = apc$ & hist_flag$
            pg% = pg% + 1%
            print page
            print using L64000, date$, time$, apc$, userid$, pg%
            print using L64010, prddate$, sc_dept$, sc_dept_d$,          ~
                    rpt_date_rnge$
            print
            if keyhit% <> 9% then goto L63300  
                print using L64070
                print using L64080
                goto L63350                    
L63300:     if sc_data_flg$ = "Y"                                        ~
                then print using L64020                                  ~
                else print using L64050
            if sc_data_flg$ = "Y"                                        ~
                then print using L64030                                  ~
                else print using L64060
L63350:     ln% = 5%
            return
    

        generate_summary_report                
            mode2% = 2%  :  f% = 8%  :  gosub open_work
            apc$ = "(AWD) Tube/Coil Reqrmts Summary         " 
            apc$ = apc$ & hist_flag$
            prddate$ = "N/A"
            sav_tht$ = " "
            readkey$ = all(hex(00))
          summary_rpt_loop
            tht$, tdi$, hd_flag$ = " "
            read #8, key > readkey$, using L63400, readkey$, tot%,       ~
                eod goto summary_rpt_done, data goto summary_rpt_loop
L63400:             fmt CH(11), BI(4)         
            tht$ = str(readkey$,,8%)         
            tdi$ = str(readkey$,9%,2%)       
            hd_flag$ = str(readkey$,11%,1%)   
            if tht$ = sav_tht$ or ln% <= 5% then goto L63450             
                print  :  ln% = ln% + 1%
L63450:     if ln% > 58% then gosub print_headings
            print using L64090, tht$, tdi$, hd_flag$, tot%
            ln% = ln% + 1%
            sav_tht$ = tht$
            goto summary_rpt_loop

          summary_rpt_done
            return

            
        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L64000: %  Date: ##########  Time: ########       #######################~
        ~################# (AWDPLA60)              User: ###  Page: ####

L64010: %  Production Date: ##########     Dept. ###  ###################~
        ~###########     Production Date Range: ######################## 

L64020: % Seq.  Mdl Width   Height  Gl S  Bal/WndTop TD  Bal/WndBot TD ~
        ~ TDiff  Bal Covr   Color              Top Loc   Bot Loc

L64030: % ----- --- ------- ------- -- -  ---------- --  ---------- -- ~
        ~ -----  --------- ------------------- --------  --------
                                                             /* 62 character */
L64040: % ##### ### ####### ####### ## #  ########  ##   ########   ## ~
        ~   #    ######### ################### ###.####  ###.####

L64050: % Seq.  Mdl Width   Height       Wnd/Cl Tube Hgts TD             ~
        ~                 

L64060: % ----- --- ------- -------      ------ --------- --             ~
        ~

L64070: % Tube/Cl   TD  *   Pairs Reqd.                                  ~
        ~

L64080: % --------  --  -   -----------                                  ~
        ~

L64090: % ########  ##  #     #######-                                   ~
        ~


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

*       display_codes
*           call "APCPLN1B" (tab%, #4)
*       return

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        open_work                              
            if (mode% = 1% and f% = 7%) or (mode2% = 1% and f% = 8%)     ~
                then mode$ = "OUTPT" 
            if (mode% = 2% and f% = 7%) or (mode2% = 2% and f% = 8%)     ~
                then mode$ = "INPUT"
            if (mode% = 3% and f% = 7%) or (mode2% = 3% and f% = 8%)     ~
                then mode$ = "SHARE"           

            call "WORKOPN2" (#f%, mode$, 3000%, f2%)   
            if f2% <> 0% then goto L64580
            if f% = 7% then work%  = 1%        
            if f% = 8% then work2% = 1%        
        return
L64580:     errormsg$ = "ERROR - CANNOT OPEN (APCPLNWK) for " & mode$
            if f% = 8% then str(errormsg$,27%,3%) = "WK2"  
            call "SHOSTAT" (errormsg$) : stop
        return clear all
        goto inputmode

        delete_work
            call "FILEBGON" addr(#f%)         
            if f% = 7% then work%  = 0%       
            if f% = 8% then work2% = 0%       
        return



        calc_glass_size
            opts%, er% = 0% 
            dept$ = str(wk_key$,7%,3%)
            g_cnt%, ct% = 0% 
            if str(part$,1%,1%) = "8" then dept$ = "008"      
            call "EWDGLSSB" (opts%,      /* Options 0% = No Calc       */~
                             part$,      /* MFG Part Number            */~
                             dept$,      /* Department Code            */~
                             ct%,        /* Glass Piece Count          */~
                             ctt$(),     /* 1-5 = Top, 6-10 = Bot      */~
                             gdd$(),     /* 1-6 = Top, 7-12 = Bot      */~
                             ajj$(),     /* Window Adjustment (GED) Top*/~
                             dcc$(),     /* Decimal 1-5=Top, 6-10=Bot  */~
                             wd$,        /* Window width Eights        */~
                             ht$,        /* window Height Eights       */~
                             g_cnt%,     /* Glass Piece Cut Count      */~
                             #4,         /* (GENCODES) Master Tables   */~
                             #1,         /* (AMTBOMCD) Equations       */~
                             er% )       /* 0%=Ok, Non-Zero = Error    */


REM          for c% = 1% to 12%

REM             call "SHOSTAT" ("ctt$(c%,1%) --> " & ctt$(c%,1%)) 
REM             call "SHOSTAT" ("dcc$(c%,1%) --> " & dcc$(c%,1%)) 
REM             call "SHOSTAT" ("ctt$(c%,2%) --> " & ctt$(c%,2%)) 
REM             call "SHOSTAT" ("dcc$(c%,2%) --> " & dcc$(c%,2%)) 
REM             call "SHOSTAT" ("gdd$(c%)    --> " & gdd$(c%))    

REM          next c%
        return

        find_balance
             for gl% = 1% to 10% step 5%
                 init(" ") v$
                 if gl% <= 5% then v$ = "T"
                 if gl% >  5% then v$ = "B"

                 convert dcc$(gl%, 1%) to dec_width, data goto next_gls

                 convert dcc$(gl%, 2%) to dec_height, data goto next_gls

                 if v$ = "T" then cvr_height = dec_height

                 if unit_type$ <> "C" and unit_type$ <> "O" then           ~
                                                   goto not_cott_oriel
                         if v$ <> "B" then goto not_cott_oriel
                            cvr_height = dec_height

not_cott_oriel:

                 gosub check_sclmr
                 if s_clmr% = 1% then gosub set_sclmr_height
                 gosub lookup_awdplnbl
                 if bal% = 0% then goto next_gls
                    gosub calc_balance

next_gls:

                 gosub lookup_awdplnbc
                    if loc% = 0% then goto next_loc
                    gosub calc_location
next_loc:

                    gosub calc_balance_cover_new
              next gl%
        return

        set_sclmr_height
REM        call "SHOSTAT" ( "I AM HERE AT SETTING HEIGHT")  stop
           if v$ = "T" then dec_height = top_height
           if v$ = "B" then dec_height = bot_height
        return



        check_sclmr                            /* Both - Cottage/Oriel */
          s_clmr% = 0%
          if s_clmr < 1.0 then return



            a1 = 0.0 : a2 = 0.0
            convert str(part$,17%,2%) to a1, data goto L50150
L50150:
            convert str(part$,19%,1%) to a2, data goto L50160
L50160:
            s_height = a1 + (a2/8.0)               /* Decimal Height   */

            init(" ") readkey$, desc$
            readkey$   = "GLASS10  " & str(part$,1%,3%)
            gosub read_gencds2
            
            if found% = 0% then return
            get #4, using L22500, desc$

            t_clmr = 0.0 : b_clmr = 0.0 : tb_clmr = 0.0
            convert str(desc$,1%,8%) to t_clmr, data goto L50200

            convert str(desc$,12%,8%) to b_clmr, data goto L50200

            convert str(desc$,22%,8%) to tb_clmr, data goto L50190
L50190

               top_height =((s_height/2.0) - t_clmr)                     ~
                                  - (((s_height/2.0) + tb_clmr) - s_clmr)

               bot_height =((s_height/2.0) - b_clmr)                      ~
                                  + (((s_height/2.0) + tb_clmr) - s_clmr)
REM            call "SHOSTAT" ( "I AM HERE AT CALCULATING HEIGHT")  stop
               s_clmr% = 1%
L50200: return

        lookup_awdplnbl
             bal% = 0%
             init(" ") prod$, vendor$, bal_type$
             prod$ = str(part$,1%,3%) & "T"
             if v$ = "B" then str(prod$,4%,1%) = "B"

             read #10, key = prod$, eod goto no_bal
                    get #10, using L52000, vendor$, bal_type$,          ~ 
                                                      friction, weight
L52000:                     FMT POS(5), CH(1), CH(1), PD(14,4), PD(14,4)

                     bal% = 1%
        no_bal
        return

        calc_balance
             gosub calc_sash_weight             

             if vendor$ = "0" then gosub calc_amesbury
             if vendor$ = "1" then gosub calc_amesbury
REM             if bal_type$ = "0" then gosub calc_sprial_unique
               gosub calc_sprial_unique


             gosub read_awdplnwc

        return

        calc_sash_weight
             gosub lookup_gls_type

             strength = 2.42
             if found% <> 1% then return

                if strength$ = "4" then strength = 3.24

        return


        lookup_gls_type
            readkey$ = "TEMP GED " & str(part$,5,2)
            gosub read_gencds2
            if found% = 1% then goto get_desc2

            readkey$ = "OBS GED  " & str(part$,5,2)
            gosub read_gencds2
            if found% = 1% then get_desc2

            readkey$ = "GED 001  " & str(part$,5,2)
            gosub read_gencds2
            if found% = 1% then get_desc
 
        return
        get_desc
            get #4, using L22500, desc$

            strength$ = str(desc$,3%,1%)
        return
        get_desc2
            get #4, using L22500, desc$

            if gl% <= 5% then strength$ = str(desc$, 3%,1%)
            if gl% >  5% then strength$ = str(desc$,20%,1%)
        return


        calc_amesbury
             vinyl        = (((dec_width * weight) + (dec_height * weight)) * 2)

             gls_surface  = ((dec_width * dec_height) / 144)
             gls_weight   = (gls_surface * strength)
             gls_friction = (dec_height * friction)


             total_weight = ((vinyl + gls_weight) - gls_friction)
        return             

        calc_sprial_unique
              gls_height_sclmr = 0.00
              gosub lookup_awdplnbc
                     gls_height_sclmr = (dec_height * cott_oriel)

        return            


        read_awdplnwc
             init(" ") bal_key$, bal_key2$, td$, bal_rec$
             bal_key2$ = vendor$ & bal_type$
        awdplnwc_next
             read #5, key 2% > bal_key2$, using L53000, bal_rec$,   ~
                                    eod goto awdplnwc_done
L53000:                 FMT CH(256)


                   str(bal_key2$,1%,35) = str(bal_rec$,28%,35%)
                   str(td$,1%,2%)       = str(bal_rec$,63%,2%)

                   get str(bal_key2$) using L53010, value1, value2,  ~
                               value3, value4

L53010:                     FMT XX(03), PD(14,4), PD(14,4), PD(14,4), ~
                                PD(14,4)


                   if str(bal_key2$,1%,1%) <> vendor$ then goto awdplnwc_done
                   if str(bal_key2$,2%,1%) <> bal_type$ then goto awdplnwc_done
                   if str(bal_key2$,3%,1%) <> v$        then goto awdplnwc_next

                   if value3 <> 0.00 and value4 <> 0.00 then gosub check_4

                   if value3 = 0.00  and value4 =  0.00 then gosub check_2

                   if awdplnwc_rec% = 0% then goto awdplnwc_next
REM                call "SHOSTAT"("BAL KEY --> " & bal_key$)  stop
REM                call "SHOSTAT"("TD      --> " & td$     )  stop
REM                   tdi$ = td$

                   if bal_type$ = "0" then gosub set_sprial
                   if bal_type$ <> "0" then gosub set_bal

                      if v$ = "T" and td$ <> "99" then tdi_top$ = td$
                      if v$ = "B" and td$ <> "99" then tdi_bot$ = td$

        awdplnwc_done
        return


        set_sprial
           if v$ = "B" then goto set_sprial_bot
              str(bal_top$,1%,3%) = str(bal_rec$,12%,3%)
              str(bal_top$,5%,3%) = str(bal_rec$,4%,3%)
              str(bal_top$,4%,1%) = "-"
             return
           set_sprial_bot
              str(bal_bot$,1%,3%) = str(bal_rec$,12%,3%)
              str(bal_bot$,5%,3%) = str(bal_rec$,4%,3%)
              str(bal_bot$,4%,1%) = "-"
        return

        set_bal
         if v$ = "T" then bal_top$ = str(bal_rec$,4%,8%)
         if v$ = "B" then bal_bot$ = str(bal_rec$,4%,8%)
        return


        check_4
             if bal_type$ = "0" then goto check_4_sclmr
             awdplnwc_rec% = 0%
                 if dec_height < value3 or dec_height > value4 then return

                 if total_weight < value1 or total_weight > value2 then return

             awdplnwc_rec% = 1%
        return
        check_4_sclmr
             awdplnwc_rec% = 0%
                 if gls_height_sclmr < value3 or gls_height_sclmr > value4 then return

                 if total_weight < value1 or total_weight > value2 then return

             awdplnwc_rec% = 1%
        return


        check_2
             awdplnwc_rec% = 0%

                 if total_weight < value1 or total_weight > value2 then return
             awdplnwc_rec% = 1%
        return


        check_ctg_orl 
            init(" ") readkey$, descr$, co_flag$, unit_type$
            unit_type$ = "S"
            str(readkey$,1%,9%)   = "HINGE    "
            str(readkey$,10%,15%) = hinge$
            read #4,key = readkey$, using L22700, descr$, eod goto no_hinge
            co_flag$ = str(descr$,1%,2%)
            if co_flag$ <> "CO" and co_flag$ <> "OR" then co_flag$ = " "
            if co_flag$ = "CO" then unit_type$ = "C"
            if co_flag$ = "OR" then unit_type$ = "O"
        no_hinge
        return



        lookup_awdplnbc
           loc% = 0%
           init(" ") readbc$, static$
           limit, over_limit, under_limit, static_value, location = 0.00
           readbc$ = str(part$,1%,3%) & str(v$) & str(unit_type$)
REM           call "SHOSTAT" (" BC READKEY " & readbc$)  stop 


           read #11, key = readbc$, eod goto no_bc

                get #11, using L53050, limit, over_limit, under_limit, ~
                          static$, static_value, cott_oriel
L53050:             FMT POS(6), PD(14,4), PD(14,4), PD(14,4), CH(1),    ~
                                PD(14,4), PD(14,4)
                loc% = 1%

        no_bc
        return

        calc_location
              show% = 0%
              goto not_static
              if static$ = "N" then goto not_static
                      location = static_value
                      goto set_top_bot
not_static:
REM                      call "SHOSTAT" (" CALC Locations " ) stop
              if unit_type$ = "S" and v$ = "T" then gosub set_standard_top
              if unit_type$ = "S" and v$ = "B" then gosub set_standard_bot
              if unit_type$ = "C" and v$ = "T" then gosub set_cottage_top
              if unit_type$ = "C" and v$ = "B" then gosub set_cottage_bot
              if unit_type$ = "O" and v$ = "T" then gosub set_oriel_top
              if unit_type$ = "O" and v$ = "B" then gosub set_oriel_bot

set_top_bot:
              if show% = 1% and bal_type$ = "2" then             ~
                                   str(inf$,15%,1%) = "*"

              if v$ = "T"  and bal_type$ = "2" then            ~
                 convert location to loc_top$, pic(##0.00##)
              if v$ = "B"  and bal_type$ = "2" then            ~
                 convert location to loc_bot$, pic(##0.00##)
           
        return

        set_standard_top
            show% = 0%
REM            if dec_height >= limit then 
            location = ((hgt / 2) + over_limit)
        return

        set_standard_bot
            if dec_height >= limit then location = static_value
            if dec_height <  limit then                  ~
                 location = ((hgt / 2) + over_limit)
            if dec_height > limit then show% = 1%
        return

        set_cottage_top
REM            location = ((hgt + .25) - (dec_height + over_limit))
            location = ((hgt) - (dec_height + over_limit))
            show% = 1%
       return
 
        set_cottage_bot
            if dec_height >= limit then location = static_value
            if dec_height <  limit then                   ~
                   location = (dec_height  + over_limit)
            show% = 1%
        return

        set_oriel_top
REM            location = ((hgt + .25) - (dec_height + over_limit))
            location = ((hgt) - (dec_height + over_limit))
            show% = 1%
        return

        set_oriel_bot
            if dec_height >= limit then location = static_value
            if dec_height <  limit then                   ~
                   location = (dec_height  + over_limit)
            show% = 1%
        return


        calc_balance_cover_new
           if str(part$,11%,1%) = "4" then return
           if str(part$,11%,1%) = "5" then return
           if str(part$,11%,1%) = "6" then return

           init(" ") readbc$
           bal_cover = 0.00
           bal_cover% = 0%
           readbc$ = str(part$,1%,3%) & str(v$) & "A"
REM           call "SHOSTAT" (" BC READKEY " & readbc$)  stop 


           read #11, key = readbc$, eod goto no_bc_cvr

                get #11, using L53060, value4
L53060:             FMT POS(47), PD(14,4)

                    bal_cover = cvr_height + value4
                    bal_cover% = int(bal_cover)
                    if v$ = "T" then convert bal_cover% to str(cus$,1%,4%), pic(####)
                    if v$ = "B" then convert bal_cover% to str(cus$,6%,4%), pic(####)
        no_bc_cvr
        return



        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            close printer
            f% = 7%
            if work%  <> 0% then gosub delete_work
            f% = 8%
            if work2% <> 0% then gosub delete_work
            end

