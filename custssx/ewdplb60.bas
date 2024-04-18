        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDPLB60                             *~
            *  Creation Date     - 02/06/01                             *~
            *  Last Modified Date- 09/02/04                             *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  - Christie M. Gregory                  *~ 
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
            * 02/06/01 ! (New) Program                            ! CMG *~
            * 01/06/03 ! (EWD001) Mods for the new 411 balance    ! CMG *~
            *          !     tubes with Cottage/Oriel             !     *~
            * 01/06/03 ! (EWD002) Mods for the 421 & 431 to lookup! CMG *~
            *          !     411 data.                            !     *~
            * 03/12/03 ! (EWD003) Mods for new depts 25 and 26    ! CMG *~
            *          !          to be treated same as dept 36.  !     *~
            * 10/24/03 ! (EWD004) Mods for new depts 27 and 28    ! CMG *~
            *          !          to be treated same as dept 36.  !     *~
            * 09/02/04 ! (AWD005) Mod to take off screen balances ! CMG *~
            *          !   for sclmr.                             !     *~
            *11/17/2021! CR2957 Chg dept 006 slide no SHIMS       ! RDB *~            
            *************************************************************
            
        sub "EWDPLB60" (part$,           /* Part Number                */~
                        #1,              /* AMTBOMCD File              */~
                        #4,              /* GENCODES File              */~
                        #9 )             /* AMTBOMIF File              */
                        
        dim                                                              ~
            title$40,                    /* Analysis Title             */~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
            sav_wc_key$15,               /* Use for Loading Table      */~
            wdtkey$5, hgtkey$5,          /* Width & Height for WC Key  */~
            sc_dept$3, sc_dept_d$30,     /* Department Code and Descr  */~
            wc_key$15,                   /* WC key                     */~
            h1$3, h2$7, h3$7,            /* Summary Screen Display     */~
            h4$2, h5$1, h6$6, h7$9,      /* Headings                   */~
            h8$2, h9$15,                 /*                            */~
            part$25,                     /* Mfg. Part No.              */~
            hinge$2,                     /* Hinge Code from Part No.   */~
            sngl$1,                      /* Single-Hung Window Flag    */~
            windcl$(6)6,                 /* Winding/Coil Data(APCPLNWC)*/~
            tubedi$(6)2,                 /* Tube Diam. Codes (APCPLNWC)*/~
            co_flag$2,                   /* Flag for Cottage/Oriel     */~
/*EWD001*/  co_flg1$2,                   /* Second Flag for CO/OR      */~
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
            userid$3,                    /* Current User Id            */~
            clmr$                        /* SCLMR              (AWD005)*/

        dim mdl$3,                       /* Model No.                  */~
            sav_mdl$3,                   /* Save Model No.     (EWD002)*/~
            wdt$7,                       /* Width                      */~
            hgt$7,                       /* Height                     */~
            gls$2,                       /* Glass No.                  */~
            scr$1,                       /* Screen No.                 */~
            wcl$6,                       /* Winds/Coil                 */~
            tht$9,                       /* Tube Heights               */~
            tdi$2,                       /* Tube Diameter Code         */~
            cus$9,                       /* Customer Code              */~
            inf$15,                      /* Model/Customer Info        */~
            dsp$10,                      /* SO Data to Pass to Dtl Sub */~
            height$10                    /* Calculated Height          */

        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(15%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Tube Windings/Coil Inquiry   "
            pname$ = "EWDPLB60 - Rev: R7.00"

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
            * #5  ! APCPLNWC ! Production Windings & Coil File          *~
            * #6  ! APCPLNSC ! Planning Master Schedule File            *~
            * #7  ! APCPLNWK ! Work File                                *~
            * #8  ! TXTFILE  ! Sales Master Text File                   *~
            * #9  ! AMTBOMIF ! Master VALIDITY FILE                     *~
            * #10 ! APCPLNOR ! Planning Header Histroy                  *~
            * #11 ! BCKLINES ! Back Log Line Item File                  *~
            * #12 ! EWDPLNRK ! Glass Master Rack File                   *~
            * #15 ! APCPLNDP ! Planning Master Dept File                *~
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

            select #5,  "APCPLNWC"                                       ~
                        varc,     indexed,  recsize =   96,              ~
                        keypos =  1,   keylen = 15                      

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

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#2,  fs%(2%),  f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#5,  fs%(5%),  f2%(5%),  0%, rslt$(5%)) 
            call "OPENCHCK" (#6,  fs%(6%),  f2%(6%),  0%, rslt$(6%))
            call "OPENCHCK" (#8,  fs%(8%),  f2%(8%),  0%, rslt$(8%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%), 0%, rslt$(11%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%), 0%, rslt$(12%))
            call "OPENCHCK" (#15, fs%(15%), f2%(15%), 0%, rslt$(15%))

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
            
            mdl$ = str(part$,1,3)
            gls$ = str(part$,5,2)
            hinge$   = str(part$,9,2)
            scr$ = str(part$,11,1)
            wdt$ = str(part$,13,4)
            hgt$ = str(part$,17,3)
            sav_mdl$ = mdl$                                   /* (EWD002) */
            convert wdt$ to wdt, data goto L02590

            convert wdt$ to sav_wdt, data goto calc_tubes     /* (EWD002) */
            gosub lookup_model                                /* (EWD002) */
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
            gosub find_lkup_diam

            co_flg1$ = " "                                    /*EWD001*/
            if co_flag$ = "CO" and mdl$ = "411" then co_flg1$ = "C1"
            if co_flag$ = "OR" and mdl$ = "411" then co_flg1$ = "O1"
            for x% = 1% to 2%                                 /*EWD001*/
             sav_wc_key$ = all(hex(00))                               
             if  x% = 1% then                                               ~
                  str(sav_wc_key$,,10)=str(mdl$) & str(tdi$) & wdtkey$      ~
              else                                                          ~
                  str(sav_wc_key$,,10)=str(mdl$) & co_flg1$ & wdtkey$         
                                                             /*EWD001*/  

REM         sav_wc_key$ = all(hex(00))        
REM         str(sav_wc_key$,,10)=str(mdl$) & str(tdi$) & wdtkey$
            read #5, key > sav_wc_key$, using L20500, wc_key$,           ~
                    eod goto L20570
L20500:         fmt ch(15)                  /* 1st Pass Finds Width... */
            if str(wc_key$,,5) <> str(sav_wc_key$,,5) then goto L20570
            sav_wc_key$, wc_key$ = str(wc_key$,,10) & hgtkey$ 
next_hgt:   read #5, key >= wc_key$, using L20540, wc_key$,              ~
                    windcl$(), tubedi$(), eod goto L02590
L20540:         fmt ch(15), 6*ch(6), 6*ch(2)/* 2nd Pass Finds Height.. */
            if str(wc_key$,,10) <> str(sav_wc_key$,,10)                  ~
                 then return                /* Height Undefined...     */
L20570:     gosub find_gls_type
            if windcl$(j%) <> " " then L20580             
                wc_key$ = str(wc_key$,,14) & bin(val(str(wc_key$,15,1))+1)
                goto next_hgt                              
L20580:     if x% <> 2% then wcl$ = windcl$(j%)                /*EWD001*/~
            else str(wcl$,4%,6%) = windcl$(j%)
            if co_flg1$ = " " then x% = 2%
            next x%                                          /*EWD001*/
            tdi$ = tubedi$(j%)

            wcl$ = windcl$(j%)
            tdi$ = tubedi$(j%)
            if wcl$ = " " then wcl$ = "."
            if tdi$ = " " then tdi$ = "99"      /* 99 = Coil       */
            call "STRING" addr("RJ", wcl$, 6%)      /* Both steps reqd.*/
            call "STRING" addr("CT", wcl$, 6%)      /* to center data. */
            if tdi$ <> "99" then gosub calc_tube_hgts
            if tht$ = "." and co_flag$ <> " " then tht$ = co_flag$
            if inf$ <= "." then gosub load_color
            mdl$ = sav_mdl$                       /* (EWD002) - BEG */
            wdt = sav_wdt
            wdt = wdt / 10
            convert wdt to wdtkey$, pic(000.0)
            fctnl = wdt - int(wdt)          /* Convert to decimal      */
            fctnl = (fctnl / 8) * 10
            wdt = int(wdt) + fctnl
            convert wdt to wdt$, pic(##0.000)     /* (EWD002) - END */

            gosub load_screen
L02590:     return


        find_lkup_diam
            gosub check_ctg_orl
                                               /*  (AWD005)  - BEG */
            if co_flag$ = " " then goto L21010
            init(" ") clmr$ 
            clmr$ = str(part$,20%,3%)
            if str(clmr$,1%,1%) > "9" then goto L21015
            if clmr$ <> "   " and clmr$ <> "000" then co_flag$ = "SP" 
                                               /*  (AWD005)  - END */
L21015:
            if co_flag$ = " " then goto L21010
                tdi$ = co_flag$
                return

L21010:     read #2, key=cus$, using L21020, billto$, eod goto L21160
L21020:         fmt pos(780), ch(9)
            readkey$ = "PLAN TDLU" & str(mdl$) & " " & billto$
            gosub read_gencds1
            if found% = 1% then return

L21160:     readkey$ = "PLAN TDLU" & mdl$
            gosub read_gencds1
            if found% = 1% then return

            tdi$ = "00"         /* Default to Standard */
            return


            read_gencds1
               found% = 0%
               read #4, key = readkey$, using L21270, tdi$, inf$,~
                        eod goto L21290
L21270:             fmt pos(25), ch(2), xx(3), ch(15)
               if inf$ = " " then inf$ = "."
               found% = 1%
L21290:        return



        find_gls_type
            j% =1%
            
            readkey$ = "PLAN DBLE" & gls$
            gosub read_gencds2
            if found% = 0% then goto L22070
                j% = 2%
                return

L22070:     readkey$ = "PLAN LAMN" & gls$
            gosub read_gencds2
            if found% = 0% then goto L22110
                j% = 3%
                return                

L22110: return            


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
            read #4, key = readkey$, using L22600, inf$,             ~
                     eod goto L22610
L22600:          fmt pos(30), ch(15)
L22610:     return

        load_screen      
            p% = 0%
            p% = len(inf$) + 1%
            q% = (15% - p%)
            
            if str(sc_dept$,1%,3%) = "002" or str(sc_dept$,1%,3%) = "007"     ~
                                                       then gosub check_tso
            if str(sc_dept$,1%,3%) = "049" or str(sc_dept$,1%,3%) = "052"     ~
                                                      then gosub check_tso

            if str(sc_dept$,1%,3%) = "002" or str(sc_dept$,1%,3%) = "007"     ~
               or str(sc_dept$,1%,3%) = "036"         then gosub check_bso
            if str(sc_dept$,1%,3%) = "049" or str(sc_dept$,1%,3%) = "052"     ~
                                                      then gosub check_bso
/*(EWD003)*/
            if str(sc_dept$,1%,3%) = "025" or str(sc_dept$,1%,3%) = "026"     ~
                                                      then gosub check_bso

/*(EWD003)*/
            if str(sc_dept$,1%,3%) = "025" or str(sc_dept$,1%,3%) = "026"     ~
                                                      then gosub check_fgo

/*(EWD004)*/
            if str(sc_dept$,1%,3%) = "027" or str(sc_dept$,1%,3%) = "028"     ~
                                                      then gosub check_bso

/*(EWD004)*/
            if str(sc_dept$,1%,3%) = "027" or str(sc_dept$,1%,3%) = "028"     ~
                                                      then gosub check_fgo

            if str(sc_dept$,1%,3%) = "036"             then gosub check_fgo


REM CR2957  if str(sc_dept$,1%,3%) = "006"             then gosub check_shims
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
/*EWD003*/
           if str(sc_dept$,1%,3%) = "025" or str(sc_dept$,1%,3%) = "026"   ~
                                                         then goto L22630  
/*EWD004*/
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

        
        check_ctg_orl   /*          Was part of calc_tube_hgts;    */
                        /*          Now called from find_lkup_diam */
            init(" ") readkey$, descr$, co_flag$
            str(readkey$,1%,9%)   = "HINGE    "
            str(readkey$,10%,15%) = hinge$
            read #4,key = readkey$, using L22700, descr$, eod goto L22710
L22700:        FMT POS(25), CH(30) 
            co_flag$ = str(descr$,1%,2%)   
            if co_flag$ <> "CO" and co_flag$ <> "OR" then co_flag$ = " "
L22710:     return


        clear_line
            seqn$, mdl$, wdt$, hgt$, gls$, scr$, tdi$, dsp$,      ~
                                  windcl$(), tubedi$, co_flag$ = " "
            wcl$, tht$, cus$, inf$ = "."
            sav_mdl$ = " "                                /*EWD002*/
        return

        lookup_model                                      /* (EWD002) */
            init(" ") readkey$, descr$          
            div_fact% = 0%
            fact = 0.00
            str(readkey$,1%,9%)   = "PLAN TMDL"
            str(readkey$,10%,15%) = mdl$
            read #4,key = readkey$, using L22700, desc$, eod goto no_model

            mdl$ = str(desc$,1%,3%)     
            convert str(desc$,12%,1%) to div_fact%, data goto no_model

            convert str(desc$,22%,5%) to fact, data goto no_model

            wdt = wdt / div_fact%
            wdt = wdt - fact

        no_model
        return                                            /* (EWD002) */


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
                      scr$, tdi$, dsp$, tubedi$

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
               at (05,35), fac(hex(a4))  , h6$                  , ch(06),~
               at (05,42), fac(hex(a4))  , h7$                  , ch(09),~
               at (05,52), fac(hex(a4))  , h8$                  , ch(02),~
               at (05,55), fac(hex(a4))  , h9$                  , ch(15),~
                                                                         ~
               at (06,10), fac(hex(84))  , mdl$                 , ch(03),~
               at (06,14), fac(hex(84))  , wdt$                 , ch(07),~
               at (06,22), fac(hex(84))  , hgt$                 , ch(07),~
               at (06,30), fac(hex(84))  , gls$                 , ch(02),~
               at (06,33), fac(hex(84))  , scr$                 , ch(01),~ 
               at (06,35), fac(hex(84))  , wcl$                 , ch(06),~ 
               at (06,42), fac(hex(84))  , tht$                 , ch(09),~ 
               at (06,52), fac(lfac$(1)) , tdi$                 , ch(02),~ 
               at (06,55), fac(hex(84))  , inf$                 , ch(15),~ 
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
            h6$ = "Wnd/Cl"
            h7$ = "Tube Hgts"
            h8$ = "TD"
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

                    if found% <> 0% then str(inf$,9%,7%) = "<------"
                readkey$ = "PLAN TDHL" &"M"& mdl$ & tdi$
                    gosub read_gencds2
                                                                
                    if found% <> 0% then lfac$(x%) = hex(94)
                    if found% <> 0% then str(inf$,9%,7%) = "<------"
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
