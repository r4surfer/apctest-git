        REM *************************************************************~  
            *                                                           *~
            *  Program Name      - AWDPLB60                             *~
            *  Creation Date     - 01/20/05                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  -                                      *~ 
            *                                                           *~
            *  Description       - Production floor user inquiry program*~
            *                      for determing no. of winds or coil,  *~
            *                      tube height(s) and tube diameter.    *~
            *                      Production Date & Dept. Specified.   *~
            *                                                           *~
            *  Code Tables Used  - PLAN TDLU, PLAN DEPT, PLAN DBLE,     *~
            *                      PLAN BALC, PLAN LAMN, PLAN TLDF      *~
            *                                                           *~
            *  Programs Used By  - APCPLN48 (MFG part explosion)        *~
            *                                                           *~
            *  Special Comments  - Periods are used as placeholders in  *~
            *                      some display arrays so that LINSMASH *~
            *                      does not get them out of sequence.   *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/20/05 ! (New) Program                            ! CMG *~
            * 04/17/14 ! (AWD001) mod for triple/lamn glass weight! MES *~
            *08/07/2014! (AWD002) mod for operable shapes         ! CMG *~
            *03/28/2016! SR73395  mod to update for lite lift bal-! PWW *~
            *          !          ances by adding AWD009,IM7691,  !     *~
            *          !          SR66916 & SR69147.              !     *~
            *11/16/2017! CR1205   fix strength calc on triple check! RDB*~
            *11/20/2107! CR1173   new strength for sound windows   ! RDB*~
            *10/01/2019! CR2247   adjust balance display           ! RDB*~
            *11/17/2021! CR2957 Chg dept 006 slide no SHIMS        ! RDB*~            
            ************************************************************* 
            
        sub "AWDPLB60" (sa_part$,        /* Part Number                */~
                        dim1es,          /* (AWD002)                   */~
                        dim2es,          /* (AWD002)                   */~
                        dim3es,          /* (AWD002)                   */~
                        #1,              /*  File              */~
                        #4,              /* GENCODES File              */~
                        #9 )             /* AMTBOMIF File              */
                        
        dim                                                              ~
            title$40,                    /* Analysis Title             */~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
            wdtkey$5, hgtkey$5,          /* Width & Height for WC Key  */~
            sc_dept$3, sc_dept_d$30,     /* Department Code and Descr  */~
            h1$3, h2$7, h3$7,            /* Summary Screen Display     */~
            h4$2, h5$1, h9$15,           /* Headings                   */~
            sa_part$45,                  /* Complete Part (AWD002)     */~
            part$25,                     /* Mfg. Part No.              */~
            subpart$20,                  /* Subpart (AWD002)           */~
            hinge$2,                     /* Hinge Code from Part No.   */~
            sngl$1,                      /* Single-Hung Window Flag    */~
            windcl$(6)6,                 /* Winding/Coil Data(APCPLNWC)*/~
            co_flag$2,                   /* Flag for Cottage/Oriel     */~
            billto$9,                    /* Bill-To Customer Code      */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            dsp_msg$79,                  /* Screen Display Message     */~
            dsp_text$11,                 /* Screen Display Text        */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            pageno$16,                   /* Page Nos. for Display      */~
            userid$3                     /* Current User Id            */
        
        dim seqn$5,                      /* Sequnce Number             */~
            mdl$3,                       /* Model No.                  */~
            sav_mdl$3,                   /* Save Model No.             */~
            wdt$7,                       /* Width                      */~
            hgt$7,                       /* Height                     */~
            gls$2,                       /* Glass No.                  */~
            scr$1,                       /* Screen No.                 */~
            wcl$6,                       /* Winds/Coil                 */~
            bal_top$24,                  /* Top Balance                */~
            bal_bot$24,                   /* Bot Balance                */~
            tdi_top$2,                   /* Tube Diameter Code         */~
            tdi_bot$2,                   /* Tube Diameter Code         */~
            tdi$2,                       /* Tube Diameter Code         */~
            cus$9,                       /* Customer Code              */~
            inf$15,                      /* Model/Customer Info        */~
            dsp$10,                      /* SO Data to Pass to Dtl Sub */~
            smbal$24,                    /* Smash balance fields       */~
            s1$8,s2$8,s3$8, ~
            height$10                    /* Calculated Height          */

        dim ctt$(10%,3%)9,               /* 1-5=Top, 6-10=Bot          */~
            gdd$(12%)10,                 /* 1-6=Top, 7-12=Bot          */~
            ajj$(10%,2%)6,               /* 1-5=Top, 6-10=Bot          */~
            dcc$(10%,2%)8,               /* 1-5=Top, 6-10=Bot          */~
            wd$7,                        /* Actual Width               */~
            ht$6                         /* Actual Height              */

        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(15%)20                 /* Text from file opening     */

        dim prod$4,                      /* Prodcut Key                */~
            vendor$1,                    /* Vendor for balance         */~
            vendor2$1,                   /* Vendor for balance (AWD009)*/~
            bal_type$1,                  /* Type of balance for product*/~
            bal_type2$1,                 /* Type of bal for prd(AWD009)*/~
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
            loc_bot$14,                  /* Bottom Balance             */~
            gls_top$14,                  /* Top Balance                */~
            gls_bot$14,                  /* Bottom Balance             */~
            wgt_top$14,                  /* Top Balance                */~
            wgt_bot$14                   /* Bottom Balance             */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(AWD) Tube Windings/Coil Inquiry   "
            pname$ = "AWDPLB60 - Rev: R7.00"

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
            * #1  ! AMTBOMCD ! Master Equation File                     *~
            * #2  ! CUSTOMER ! Customer Master File                     *~
            * #4  ! GENCODES ! System Master Code Table Files           *~
            * #5  ! AWDPLNWC ! Production Windings & Coil File          *~
            * #6  ! APCPLNSC ! Planning Master Schedule File            *~
            * #7  ! APCPLNWK ! Work File                                *~
            * #8  ! TXTFILE  ! Sales Master Text File                   *~
            * #9  ! AMTBOMIF ! Master VALIDITY FILE                     *~
            * #10 ! APCPLNOR ! Planning Header Histroy                  *~
            * #11 ! BCKLINES ! Back Log Line Item File                  *~
            * #12 ! EWDPLNRK ! Glass Master Rack File                   *~
            * #15 ! APCPLNDP ! Planning Master Dept File                *~
            * #18 ! AWDPLNBL ! Product and Balance Cross-Reference      *~
            * #19 ! AWDPLNBC ! Balance Location File                    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #2,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #5,  "AWDPLNWC",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 1,   keylen = 27,                       ~
                        alt key  1, keypos =    1, keylen =  62,         ~
                            key  2, keypos =   28, keylen =  35

            select #6,  "APCPLNSC",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 24,   keylen = 10,                      ~
                        alt key 1, keypos =  7, keylen = 27,             ~
                            key 2, keypos =  1, keylen = 33

            select #7, "APCPLNWK",                                       ~
                        varc,     indexed,  recsize =   95,              ~
                        keypos =  1,   keylen = 51
 
            select #8,  "TXTFILE",                                       ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    1, keylen =  11

            select #10, "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos = 1,    keylen = 51,                      ~
                        alt key 1, keypos = 27, keylen = 25,             ~
                            key 2, keypos = 70, keylen =  8, dup,        ~
                            key 3, keypos = 78, keylen =  8, dup,        ~
                            key 4, keypos = 52, keylen =  8,             ~
                            key 5, keypos = 36, keylen = 16, dup

            select #11, "BCKLINES",                                      ~
                        varc,     indexed,  recsize = 300,               ~
                        keypos =  10,  keylen = 19

            select #12, "EWDPLNRK",                                      ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    1, keylen =    9,                    ~
                        alt key  1, keypos =   10, keylen =  14          ~
           
            select #15, "APCPLNDP",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 11,    keylen = 12,                     ~
                        alt key 1, keypos =  9, keylen = 14,             ~
                            key 2, keypos =  4, keylen = 12,             ~
                            key 3, keypos =  1, keylen = 15

            select #18, "AWDPLNBL"                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =  1,   keylen = 4     


            select #19, "AWDPLNBC",                                     ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 1,   keylen =  5 
                 


            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#2,  fs%(2%),  f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#5,  fs%(5%),  f2%(5%),  0%, rslt$(5%)) 
            call "OPENCHCK" (#6,  fs%(6%),  f2%(6%),  0%, rslt$(6%))
            call "OPENCHCK" (#8,  fs%(8%),  f2%(8%),  0%, rslt$(8%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%), 0%, rslt$(11%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%), 0%, rslt$(12%))
            call "OPENCHCK" (#15, fs%(15%), f2%(15%), 0%, rslt$(15%))
            call "OPENCHCK" (#18, fs%(18%), f2%(18%), 0%, rslt$(18%))
            call "OPENCHCK" (#19, fs%(19%), f2%(19%), 0%, rslt$(19%))


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
            minth% = 6%
            maxth% = 46%    /* Values also passed to report subrtn. */
/* (AWD002) */            
            init(" ") part$, subpart$
            part$    = str(sa_part$,1%,20%)
            subpart$ = str(sa_part$,21%,20%)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   1%  
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
L10215:               if keyhit% = 16% and fieldnr% = 1% then exit_sub    
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
                  if keyhit%  = 16% then goto exit_sub    
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%             /*Change to 3 for*/
            if fieldnr% < 1% or fieldnr% > 1% then editpg1 
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
                                                 /* Process Wind/Coil  */ 
        process_data                             /* Production Data    */
            call "SHOSTAT" ("Loading Winding/Coil Data.....")

            gosub calc_tubes
            goto display_summary

        calc_tubes
            call "SHOSTAT" ("Loading Selected Records.....")
            gosub clear_line
            part_len% = 0%
            part_len% = len(part$)

            if len(part$) < 19 then return

            if len(part$) >= 22 then gosub set_sclmr
            

            mdl$ = str(part$,1,3)
            gls$ = str(part$,5,2)
            hinge$   = str(part$,9,2)
            scr$ = str(part$,11,1)
            wdt$ = str(part$,13,4)
            hgt$ = str(part$,17,3)
            sav_mdl$ = mdl$                                
            convert wdt$ to wdt, data goto L02590

            convert wdt$ to sav_wdt, data goto calc_tubes  
            gosub lookup_model                             
            wdt = wdt / 10              
            convert wdt to wdtkey$, pic(000.0)
            fctnl = wdt - int(wdt)          /* Convert to decimal      */
            fctnl = (fctnl / 8) * 10
            wdt = int(wdt) + fctnl
            convert wdt to wdt$, pic(##0.000)
            convert hgt$ to hgt, data goto L02590
            hgt = hgt / 10              
            convert hgt to hgtkey$, pic(000.0)
            fctnl = hgt - int(hgt)          /* Convert to decimal      */
            fctnl = (fctnl / 8) * 10
            hgt = int(hgt) + fctnl         /* Reused in calc_tube_hgts */ 
            convert hgt to hgt$, pic(##0.000)

            gosub check_ctg_orl
            gosub calc_glass_size
            gosub find_balance


            if unit_type$ = "C" or unit_type$ = "O"  ~
                        then  str(inf$,15%,1%) = "/"

            call "STRING" addr("RJ", wcl$, 6%)      /* Both steps reqd.*/
            call "STRING" addr("CT", wcl$, 6%)      /* to center data. */
            if tdi$ <> "99" then gosub calc_tube_hgts
            if tht$ = "." and co_flag$ <> " " then tht$ = co_flag$
            if str(inf$,1%,14%) <= "." then gosub load_color
            mdl$ = sav_mdl$            
            wdt = sav_wdt
            wdt = wdt / 10
            convert wdt to wdtkey$, pic(000.0)
            fctnl = wdt - int(wdt)          /* Convert to decimal      */
            fctnl = (fctnl / 8) * 10
            wdt = int(wdt) + fctnl
            convert wdt to wdt$, pic(##0.000)    

            gosub load_screen
L02590:     return



            read_gencds2
               found% = 0%
               read #4, key = readkey$, eod goto L22290                    
               found% = 1%
L22290:        return



        calc_tube_hgts
            ht1%, ht2% = 999%  :  factor = 0
            init(" ") readkey$, descr$                      
            str(readkey$,1%,9%)   = "PLAN BALC"
            str(readkey$,10%,3%) = mdl$
            for x% = 1% to 2%          
              if x% = 2% then str(readkey$,13%,2%) = tdi$                ~
                 else str(readkey$,13%,2%) = " "                        
              read #4,key=readkey$, using L22500, descr$, eod goto L22510
L22500:          FMT POS(25), CH(30)
L22510:     next x%                                          
            if descr$ = " " then goto L22580                 
            convert str(descr$,,7) to factor, data goto L22580
            sngl$ = str(descr$,10,1)
            ht2% = int((hgt/2) - factor)
            factor% = 2%                
            init(" ") readkey$, descr$                      
            str(readkey$,1%,9%)   = "PLAN TLDF"
            str(readkey$,10%,15%) = mdl$ & tdi$
            read #4,key = readkey$, using L22500, descr$, eod goto L22540
            convert str(descr$,,3) to factor%, data goto L22540
L22540:     ht1% = ht2% - factor%          
*               --- Previous location of code in check_ctg_orl ---
            if co_flag$ = "CO" then ht1% = ht1% * 0.8/*Cottage*/ 
                                                        
            if co_flag$ = "OR" then ht2% = ht2% * 1.2/* Oriel */
            if ht1% < minth% then ht1% = minth%
            if ht1% > maxth% then ht1% = maxth%
            if ht2% < minth% then ht2% = minth%
            if ht2% > maxth% then ht2% = maxth%
            tht$ = "    -xxx "
            if co_flag$ <> " "                                           ~
                then str(tht$,5,1) = "/"   /* Denotes diff in calc */
L22580:     if sngl$ <> "S" or co_flag$ <> " "                           ~
                then convert ht1% to str(tht$,,3), pic(##0)
            convert ht2% to str(tht$,6,3), pic(##0)
            return


        load_color                              
            readkey$ = "COLOR    " & str(part$,4,1)   /* Only Need */
                                                      /* 1st 8 Char*/   
            read #4, key = readkey$, using L22600, str(inf$,1%,14%),       ~
                     eod goto L22610
L22600:          fmt pos(30), ch(15)
L22610:     return

        load_screen      
            p% = 0%
            p% = len(inf$) + 1%
            q% = (14% - p%)
            
            if str(sc_dept$,1%,3%) = "002" or str(sc_dept$,1%,3%) = "007"     ~
                                                       then gosub check_tso
            if str(sc_dept$,1%,3%) = "049" or str(sc_dept$,1%,3%) = "052"     ~
                                                      then gosub check_tso

            if str(sc_dept$,1%,3%) = "002" or str(sc_dept$,1%,3%) = "007"     ~
               or str(sc_dept$,1%,3%) = "036"         then gosub check_bso
            if str(sc_dept$,1%,3%) = "049" or str(sc_dept$,1%,3%) = "052"     ~
                                                      then gosub check_bso

            if str(sc_dept$,1%,3%) = "025" or str(sc_dept$,1%,3%) = "026"     ~
                                                      then gosub check_bso


            if str(sc_dept$,1%,3%) = "025" or str(sc_dept$,1%,3%) = "026"     ~
                                                      then gosub check_fgo


            if str(sc_dept$,1%,3%) = "027" or str(sc_dept$,1%,3%) = "028"     ~
                                                      then gosub check_bso


            if str(sc_dept$,1%,3%) = "027" or str(sc_dept$,1%,3%) = "028"     ~
                                                      then gosub check_fgo

            if str(sc_dept$,1%,3%) = "036"             then gosub check_fgo


REM CR2957   if str(sc_dept$,1%,3%) = "006"             then gosub check_shims
        return

        check_tso    /* test for tso, bso, fgo */
           if str(sc_dept$,1%,3%) = "052" then goto L22620
              gosub check_total_diameter

L22620:    if str(part$,11%,1%) = "4" then        ~
                 str(inf$, p%,q%) = " / TSO"
        return

        check_bso
           if str(sc_dept$,1%,3%) = "036" or str(sc_dept$,1%,3%) = "052"   ~
                                                         then goto L22630        

           if str(sc_dept$,1%,3%) = "025" or str(sc_dept$,1%,3%) = "026"   ~
                                                         then goto L22630  

           if str(sc_dept$,1%,3%) = "027" or str(sc_dept$,1%,3%) = "028"   ~
                                                         then goto L22630              
              gosub check_total_diameter
        
L22630:    if str(part$,11%,1%) = "5" then        ~
                 str(inf$, p%,q%) = " / BSO"
        return

        check_fgo
           if str(part$,11%,1%) = "6" then        ~
                 str(inf$, p%,q%) = " / FGO"
        return

        check_total_diameter   /* If tube diameter is 99 and height is >= 67 */
           height = 0.0  :  init(" ") height$  
           if tdi$ <> "99" or hgt < 67 then return

           height = round((hgt - 67) / 2, 4)

           convert height to height$, pic(####.####-)

           call "SPCSMASH" (height$)
           
           str(inf$, p%,q%) = " / " & height$
        return                     

        check_shims
           init(" ") height$
           if hgt < 60 then return

           str(inf$, p%,q%) = " / SHIMS"               
        return

        

        clear_line
            seqn$, mdl$, wdt$, hgt$, gls$, scr$, tdi$, dsp$,      ~
                                  windcl$(), tubedi$, co_flag$ = " "
            wcl$, tht$, cus$, inf$ = "."
            sav_mdl$ = " "              
            cvr_height, s_clmr = 0.00
        return


        set_sclmr
               a1 = 0.0 : a2 = 0.0
               convert str(part$,20%,2%) to a1, data goto L24550

               convert str(part$,22%,1%) to a2, data goto L24560
L24560:
               s_clmr = a1 + (a2/8.0)
L24550:     if s_clmr <= 8.0 then s_clmr = 0.0
        return


        lookup_model                           
            init(" ") readkey$, descr$          
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
         "Enter a Production Date                                      ",~
         "Enter a Valid Department Code                                ",~
         "Enter a User ID for Production                               " ~

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, sc_dept$, sc_dept_d$,      ~
                      windcl$(), billto$, seqn$, mdl$, wdt$, hgt$, gls$, ~
                      scr$, tdi$, dsp$, tubedi$, loc_top$, loc_bot$,     ~
                      tdi_top$, tdi_bot$, bal_top$, bal_bot$, gls_top$,  ~
                      gls_bot$, wgt_top$, wgt_bot$

            init(".") wcl$, tht$, cus$, inf$ /* Reqd.(LINSMASH)*/


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
              on fieldnr% gosub L40170           /* sc_prddate$        */


              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
                  lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40170:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,02), "Dept Code:",                                 ~
               at (04,25), fac(lfac$(1%)), sc_dept$             , ch(03),~
               at (04,40), fac(hex(84)),   sc_dept_d$           , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 0% then goto L40370
                  gosub L50050
                  goto process_data

L40370:        if keyhit% <> 8% then goto L40380
                  tab% = 1%
                  gosub display_codes
                  goto L40190


L40380:        if keyhit% <> 14% then goto L40390
                  gosub process_data
                  goto L40190

L40390:        if keyhit% <> 13% then goto L40400
                  call "PROCLINK" ("ILPMAN  ","        ","      ",0%,0%)
                  goto L40190

L40400:        if keyhit% <> 15 then goto L40420
                  call "PRNTSCRN"
                  goto L40190

L40420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (8)Display Departments " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "(13)Print Queue        (16)Exit Program"
            pfkeys$ = hex(01ffffffffffff08ffffffff0dff0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(3),41,15) = " "  :  str(pfkeys$,13,1) = hex(ff)
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40570:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Process Data"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
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
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~            
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
                                                                         ~
               at (01,21), fac(hex(a4)), title$                 , ch(40),~
                                                                         ~
               at (04,10), "Dept Code:",                                 ~
               at (04,33), fac(hex(84)), sc_dept$               , ch(03),~
               at (04,48), fac(hex(84)),   sc_dept_d$           , ch(30),~
                                                                         ~
               at (05,10), fac(hex(a4))  , h1$                  , ch(03),~
               at (05,14), fac(hex(a4))  , h2$                  , ch(07),~
               at (05,22), fac(hex(a4))  , h3$                  , ch(07),~
               at (05,30), fac(hex(a4))  , h4$                  , ch(02),~
               at (05,33), fac(hex(a4))  , h5$                  , ch(01),~     
               at (05,35), fac(hex(a4))  , h9$                  , ch(15),~
                                                                         ~
               at (06,10), fac(hex(84))  , mdl$                 , ch(03),~
               at (06,14), fac(hex(84))  , wdt$                 , ch(07),~
               at (06,22), fac(hex(84))  , hgt$                 , ch(07),~
               at (06,30), fac(hex(84))  , gls$                 , ch(02),~
               at (06,33), fac(hex(84))  , scr$                 , ch(01),~ 
               at (06,35), fac(hex(84))  , inf$                 , ch(15),~ 
                                                                         ~
               at (08,10), fac(hex(84))  , "TOP TDI",                    ~
               at (08,24), fac(hex(84))  , tdi_top$             , ch(02),~
               at (09,10), fac(hex(84))  , "TOP BAL",                    ~
               at (09,25), fac(hex(84))  , bal_top$             , ch(24),~
               at (10,10), fac(hex(84))  , "TOP GLS",                    ~
               at (10,25), fac(hex(84))  , gls_top$             , ch(14),~
               at (11,10), fac(hex(84))  , "TOP WGT",                    ~
               at (11,25), fac(hex(84))  , wgt_top$             , ch(14),~
                                                                         ~
               at (12,10), fac(hex(84))  , "BOT TDI",                    ~
               at (12,24), fac(hex(84))  , tdi_bot$             , ch(02),~
               at (13,10), fac(hex(84))  , "BOT BAL",                    ~
               at (13,25), fac(hex(84))  , bal_bot$             , ch(24),~
               at (14,10), fac(hex(84))  , "Bot GLS",                    ~
               at (14,25), fac(hex(84))  , gls_bot$             , ch(14),~
               at (15,10), fac(hex(84))  , "Bot WGT",                    ~
               at (15,25), fac(hex(84))  , wgt_bot$             , ch(14),~
               at (16,10), fac(hex(84))  , "Top Loc"            ,        ~
               at (16,19), fac(hex(84))  , loc_top$             , ch(14),~
               at (17,10), fac(hex(84))  , "Bot Loc"            ,        ~
               at (17,19), fac(hex(84))  , loc_bot$             , ch(14),~
                                                                         ~
               at (21,02), fac(hex(a4)),   dsp_msg$             , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L41140
                  call "PRNTSCRN"
                  goto L41000

L41140:        if keyhit% <> 16% and keyhit% <> 0% then goto L41000

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return clear all
        goto exit_sub

        set_pf2
            title$ = "(Display) Wind/Coil Data for Dept. xxx "
            str(title$,36,3) = sc_dept$
            dsp_msg$ = "Press <ENTER> or PF<16> to Return"
            pageno$ = "Page: XXX of XXX"        /* k% = Array Values */
            h1$ = "Mdl"
            h2$ = "Width  "
            h3$ = "Height "
            h4$ = "Gl"
            h5$ = "S"
            h9$ = "Model/Cust.Info" 

            pf$(1) = "                                        " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Display" 
            pfkeys$ = hex(ffffffffffffffffffffffffffff0f1000)

            dsp_text$ = " Upr - Lwr "      

                lfac$(1%) = hex(84)
                readkey$ = "PLAN TDHL" & "D" & sc_dept$ & tdi$
                    gosub read_gencds2
                    if found% <> 0% then lfac$(x%) = hex(94)

                    if found% <> 0% then str(inf$,9%,6%) = "<-----"
                readkey$ = "PLAN TDHL" &"M"& mdl$ & tdi$
                    gosub read_gencds2
                                                                
                    if found% <> 0% then lfac$(x%) = hex(94)
                    if found% <> 0% then str(inf$,9%,6%) = "<-----"
                if str(tht$,5,1)="/" then dsp_text$="/ = Ctg/Orl"
                                           
        return
              


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50050         /* Department Code        */

            return


L50050: Rem Enter a Valid Department Code          sc_dept$, sc_dept_d$
            init(" ") sc_dept_d$
            gosub check_dept
            if dept% = 0% then goto L50080
               sc_dept_d$ = desc$
        return
L50080:     errormsg$ = "(Error) Invalid Department Code"
            gosub error_prompt
            init(" ") sc_dept$, sc_dept_d$
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



        calc_glass_size
            opts%, er% = 0% 
            dept$ = str(sc_dept$,1%,3%)
            g_cnt%, ct% = 0% 
/* (AWD002) */            
REM            IF STR(PART$,1%,1%) = "8" THEN DEPT$ = "008"      
            call "EWDGLSSB" (opts%,      /* Options 0% = No Calc       */~
                             part$,      /* MFG Part Number            */~
                             subpart$,   /* Subpart (AWD002)           */~
                             dim1es,     /* (AWD002)                   */~
                             dim2es,     /* (AWD002)                   */~
                             dim3es,     /* (AWD002)                   */~
                             dept$,      /* Department Code            */~
                             ct%,        /* Glass Piece Count          */~
                             ctt$(),     /* 1-5 = Top, 6-10 = Bot      */~
                             gdd$(),     /* 1-6 = Top, 7-12 = Bot      */~
                             ajj$(),     /* Window Adjustment (GED) Top*/~
                             dcc$(),     /* Decimal 1-5=Top, 6-10=Bot  */~
                             wd$,        /* Window width Eights        */~
                             ht$,        /* window Height Eights       */~
                             g_cnt%,     /* Glass Piece Cut Count      */~
                             0%,         /* type                       */~
                             #4,         /* (GENCODES) Master Tables   */~
                             #1,         /* (AMTBOMCD) Equations       */~
                             er% )       /* 0%=Ok, Non-Zero = Error    */

        return


        find_balance
             for gl% = 1% to 10%
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
             max_weight = 0.00
             prod$ = str(part$,1%,3%) & "T"
             if v$ = "B" then str(prod$,4%,1%) = "B"

             read #18, key = prod$, eod goto no_bal
                    get #18, using L52000, vendor$, bal_type$,          ~ 
                                    friction, weight, perimeter,        ~
                                    vendor2$, bal_type2$, max_weight,   ~
/*IM7691 + */                   range_max1, range_max2, range_max3,    ~
                                range_max4, range_max1_wt, range_max2_wt, ~
/*IM7691 - */                   range_max3_wt, range_max4_wt
/*(AWD002)*/
L52000:                     FMT POS(5), CH(1), CH(1), PD(14,4), PD(14,4),~
                                 PD(14,4), CH(01), CH(01), PD(14,4),     ~
/*IM7691 + */                  PD(14,4), PD(14,4), PD(14,4), PD(14,4),   ~
/*IM7691 - */                  PD(14,4), PD(14,4), PD(14,4), PD(14,4)

                     bal% = 1%
        no_bal
        return

        calc_balance
             gosub calc_sash_weight             

balanceReset:
/* (AWD001) */
REM             IF VENDOR$ = "0" THEN GOSUB CALC_AMESBURY
REM             IF VENDOR$ = "1" THEN GOSUB CALC_AMESBURY
REM             IF BAL_TYPE$ = "0" THEN GOSUB CALC_SPRIAL_UNIQUE

            if bal_type$ = "2" or bal_type$ = "6" then gosub cal_coil
             if bal_type$ <> "2" and bal_type$ <> "6" then gosub calc_amesbury
/*(AWD009) */
             if max_weight = 0.00 then goto noWeightCheck
             if vendor2$ = " " or bal_type2$ = " " then goto noWeightCheck
                if total_weight >=  max_weight then Lite_Lift
/*IM7691 + */   within_range% = 0%
                gosub window_height_range
/*IM7691 - */   if within_range% = 0% then noWeightCheck
                
Lite_Lift:
/* RESET VENDOR AND BAL_TYPE TO SECOND TYPE */
                vendor$   = vendor2$
                bal_type$ = bal_type2$
/* SET TO BLANK SO WILL NOT CHECK MORE THAN ONCE */
                init(" ") vendor2$, bal_type2$
                goto balanceReset
noWeightCheck:
/*(\AWD009) */
             if bal_type$ = "0" then gosub calc_sprial_unique

             gosub read_awdplnwc

        return
/*IM7691 + Check additional range for weight limit  */

         window_height_range
         
REM if str(wk_rec$,111,5) <> "00002" and str(wk_rec$,189,19) <> ~
REM "E412QD0000950250371" then goto pww_cont~
REM v$ = "B"
REM stop 
             
         pww_cont
         
/*SR69147...                                                               ~
     *** Can't quite figer out whats going on here.  Should I use          ~
     *** hgt or dec_height? They both have produced wrong balances.        ~
     *** I'm going to switch back to hgt for now. Then not IF but WHEN     ~
     *** we see another example where it is wrong I will research the      ~
     *** hgt variable to see how it's wrong as compared to dec_height.    */
     
/*SR69147*/  if hgt >  range_max1 then next_height1    
/*SR66916    IF DEC_HEIGHT >  RANGE_MAX1 THEN NEXT_HEIGHT1                */
/*SR66916    IF TOTAL_WEIGHT <  RANGE_MAX1_WT THEN WITHIN_RANGE% = 1%     */
                if total_weight >=  range_max1_wt then within_range% = 1%                	
                goto weight_height_range_end
         next_height1
/*SR69147*/  if hgt >  range_max2 then next_height2    
	           if total_weight >=  range_max2_wt then within_range% = 1%
	           	
/*SR66916    IF DEC_HEIGHT >  RANGE_MAX2 THEN NEXT_HEIGHT2                */
/*SR66916    if total_weight <  range_max2_wt then within_range% = 1%     */
                goto weight_height_range_end
         next_height2
/*SR69147*/  if hgt >  range_max3 then next_height3     
             if total_weight >=  range_max3_wt then within_range% = 1%	
             	
/*SR66916    IF DEC_HEIGHT >  RANGE_MAX3 THEN NEXT_HEIGHT3                */
/*SR66916    IF TOTAL_WEIGHT >=  RANGE_MAX3_WT THEN WITHIN_RANGE% = 1%  */
                goto weight_height_range_end
         next_height3
/*SR69147*/  if hgt >  range_max4 then weight_height_range_end    
             if total_weight >=  range_max4_wt then within_range% = 1%                	
             		
/*SR66916    IF DEC_HEIGHT >  RANGE_MAX4 THEN WEIGHT_HEIGHT_RANGE_END     */
/*SR66916    IF TOTAL_WEIGHT <  RANGE_MAX4_WT THEN WITHIN_RANGE% = 1%     */
         weight_height_range_end
         return
/*IM7691 - */

        calc_sash_weight
             gosub lookup_gls_type
/*(AWD001)*/
             gosub lookup_triple
             gosub lookup_laminate
             gosub lookup_sound_gls_strength   /* CR1173 */
             
/*(\AWD001)*/                          
REM          call "SHOSTAT" ( "calculating SASH WEIGHT") stop

             strength = 2.42
           
/* CR1173 */ if sound_gls% = 1% then strength = 4.095  
 
REM             if found% <> 1% then return
             if strength$ = "3" then strength = 2.42      /* CR1205 return  */
             if strength$ = " " then return
             if strength$ = "4" then strength = 3.24
/*(AWD001)*/
             if strength$ = "4" and triple% = 1% then strength = 4.875  
             if strength$ = "4" and laminate% = 1% then strength = 5.55
             
             if strength$ = "3" and triple% = 1% then strength = 3.63

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
     
/*(AWD001)*/
          lookup_triple
            triple% = 0%
            readkey$ = "PLANTRIPL" & str(part$,5,2)
            gosub read_gencds2
            if found% = 1% then triple% = 1%
        return

        lookup_laminate
            laminate% = 0%
            readkey$ = "PLAN LAMN" & str(part$,5,2)
            gosub read_gencds2
            if found% = 1% then laminate% = 1%
        return         
/*(\AWD001)*/
        
        get_desc
            get #4, using L22500, desc$

            strength$ = str(desc$,3%,1%)
        return
        get_desc2
            get #4, using L22500, desc$

            if gl% <= 5% then strength$ = str(desc$, 3%,1%)
            if gl% >  5% then strength$ = str(desc$,20%,1%)
        return

/* CR1173 Sound windows */
        lookup_sound_gls_strength
            sound_gls% = 0%
            readkey$ = "PLAN STC " & str(part$,5,2) 
            gosub read_gencds2
            if found% = 1% then sound_gls% = 1%
            if found% = 1% then strength$ = "6"   /* set only due to new gencode */
        return

        calc_amesbury
             vinyl        = (((dec_width * weight) + (dec_height * weight)) * 2)

/*(AWD001)*/
             vinyl        = vinyl + perimeter

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

/* (AWD001) */
        cal_coil

             vinyl        = (((dec_width * weight) + (dec_height * weight)) * 2)
             vinyl        = vinyl + perimeter

             gls_surface  = ((dec_width * dec_height) / 144)
             gls_weight   = (gls_surface * strength)
             gls_friction = ((2 * dec_height) * friction)


             total_weight = ((vinyl + gls_weight) - gls_friction)
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

                   if v$ = "T" then                        ~
                        convert dec_height to gls_top$, pic(-#####0.00##)
                   if v$ = "B" then                        ~
                        convert dec_height to gls_bot$, pic(-#####0.00##)

                   if v$ = "T" then                       ~
                         convert total_weight to wgt_top$, pic(-#####0.00##)
                   if v$ = "B" then                       ~
                         convert total_weight to wgt_bot$, pic(-#####0.00##)

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
/* CR2247 use all 3 balance fields */
         s1$ = str(bal_rec$,4%,8%) 
         s2$ = str(bal_rec$,12%,8%) 
         s3$ = str(bal_rec$,20%,8%)
         smbal$ = str(bal_rec$,4%,8%) & str(bal_rec$,12%,8%) & ~
                  str(bal_rec$,20%,8%)
         call "SPCSMASH" (smbal$)
         if v$ = "T" then bal_top$ = smbal$
         if v$ = "B" then bal_bot$ = smbal$
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
           limit, over_limit, under_limit, static_value,           ~ 
                 location, cott_oriel = 0.00
           readbc$ = str(part$,1%,3%) & str(v$) & str(unit_type$)
REM           call "SHOSTAT" (" BC READKEY " & readbc$)  stop 

/* (AWD001) */
           if sc_dept$ = "049" and str(part$,1,3) = "267"    ~
                      then str(readbc$,1,3) = "215"


           read #19, key = readbc$, eod goto no_bc

                get #19, using L53050, limit, over_limit, under_limit, ~
                          static$, static_value, cott_oriel
L53050:             FMT POS(6), PD(14,4), PD(14,4), PD(14,4), CH(1),    ~
                                PD(14,4), PD(14,4)
                loc% = 1%

        no_bc
        return

        calc_location
REM              call "SHOSTAT" (" SET LOCATION  " ) stop
              show% = 0%
              goto not_static
              if static$ = "N" then goto not_static
                      location = static_value
                      goto set_top_bot
not_static:
REM                       call "SHOSTAT" (" CALC Locations " )   stop
              if unit_type$ = "S" and v$ = "T" then gosub set_standard_top
              if unit_type$ = "S" and v$ = "B" then gosub set_standard_bot
              if unit_type$ = "C" and v$ = "T" then gosub set_cottage_top
              if unit_type$ = "C" and v$ = "B" then gosub set_cottage_bot
              if unit_type$ = "O" and v$ = "T" then gosub set_oriel_top
              if unit_type$ = "O" and v$ = "B" then gosub set_oriel_bot

set_top_bot:
              if show% = 1% and bal_type$ = "2" then          ~
                           str(inf$,15%,1%) = "*"

              if v$ = "T" and bal_type$ = "2" then            ~
                 convert location to loc_top$, pic(-#####0.00##)
              if v$ = "B"  and bal_type$ = "2" then            ~
                 convert location to loc_bot$, pic(-#####0.00##)
           
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


        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

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

        exit_sub
            end


