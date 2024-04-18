        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLC60                             *~
            *  Creation Date     - 11/12/04                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie M Gregory                   *~
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
            *  Subroutines Used  - EWDPLA58 (Display SO Detail Info)    *~
            *                      AWDPLA60 (Tube Winds/Coil Report)    *~
            *                      EWDUSRVL (User ID Validation)        *~
            *                                                           *~
            *  Special Comments  - Periods are used as placeholders in  *~
            *                      some display arrays so that LINSMASH *~
            *                      does not get them out of sequence.   *~
            *                      Any logic changes should also be     *~
            *                      applied to sub AWDPLA60 (Report).    *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/12/04 ! (New) Program                            ! CMG *~
            * 10/28/08 ! (AWD001) mods for rounding and sash stop ! CMG *~
            *07/09/2009! (AWD002) mod for perimeter weight        ! CMG *~ 
            *05/19/2014! (CUT001) mod to add dim fields to CUTCC  ! CMG *~
            *11/17/2021! CR2957  mod on dept 006 chg to slider    ! RDB *~
            *************************************************************

        dim                                                              ~
            title$40, option$16,         /* Analysis Title and Time    */~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
            descr$30,                    /* GENCODES Description       */~
            wdtkey$5, hgtkey$5,          /* Width & Height for WC Key  */~
            sc_dept$3, sc_dept_d$30,     /* Department Code and Descr  */~
            sc_user$3,                   /* User ID for Production     */~
            sc_prddate$10,               /* Production Date            */~
            dt_key$57,                   /* DT Alt key (1)             */~
            wk_key$51, wk_rec$256,       /* Work file key, record area */~
            h1$5, h2$3, h3$7, h4$7,      /* Summary Screen Display     */~
            h5$8, h6$2, h7$8, h8$2,      /* Headings                   */~
            h9$9, h10$15, h11$8,         /*                            */~
            part$25,                     /* Mfg. Part No.              */~
            sav_part$25,                 /* Mfg. Part No.              */~
            hinge$2,                     /* Hinge Code from Part No.   */~
            sngl$1,                      /* Single-Hung Window Flag    */~
            windcl$(6)6,                 /* Winding/Coil Data(APCPLNWC)*/~
            tubedi$(6)2,                 /* Tube Diam. Codes (APCPLNWC)*/~
            co_flag$2,                   /* Flag for Cottage/Oriel     */~
            find_seq$5,                  /* Seq. No. to Find via PF8   */~
            billto$9,                    /* Bill-To Customer Code      */~
            temp$5,                      /* Temporary Variable         */~
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

        dim cc$(3000)1,                  /* Tab Stop/Mark for Dsp/Rmvl */~
            seqn$(3000)5,                /* Production Sequence No.    */~
            mdl$(3000)3,                 /* Model No.                  */~
            sav_mdl$3,                   /* Save Model No.             */~
            wdt$(3000)7,                 /* Width                      */~
            hgt$(3000)7,                 /* Height                     */~
            wcl$(3000)6,                 /* Winds/Coil                 */~
            tht$(3000)9,                 /* Tube Heights               */~
            tdi_top$(3000)2,             /* Tube Diameter Code         */~
            tdi_bot$(3000)2,             /* Tube Diameter Code         */~
            wgt_top$(3000)14,            /* Weight Diameter Code       */~
            wgt_bot$(3000)14,            /* Weight Diameter Code       */~
            hgt_top$(3000)14,            /* Sash Height                */~
            hgt_bot$(3000)14,            /* Sash Height Bottom         */~
            sash_stop_bot$(3000)14,      /* Sash Stop Bottom           */~            
            cus$(3000)9,                 /* Customer Code              */~
            inf$(3000)15,                /* Model/Customer Info        */~
            dsp$(3000)10,                /* SO Data to Pass to Dtl Sub */~
            height$10                    /* Calculated Height          */

        dim                              /*                            */~
            tw$1,                        /* WIDTH PARTS TYPE CODE      */~
            th$1,                        /* HEIGHT PARTS TYPE CODE     */~
            ct(100%),                    /* Cut Size - DECIMAL         */~
            ct$(100%)9,                  /* Cut Size - WITH FRACTION   */~
            cr$(100%)10,                 /* Cut Raw Material           */~
            cp$(100%)2,                  /* Cut Number of Pieces       */~
            cc_ans$(100%)1,              /* Linealmate Cut (Y) or (N)  */~
            co$(100%)25,                 /* CUT DESCRIPTIONS           */~
            eq$(100%)8,                  /* Equation Ref Key(s)        */~
            sh$(100%)1                   /* SASH TYPE ( W, H, N )      */

        dim ctt$(10%,3%)9, dept$3,       /* 1-5=Top, 6-10=Bot          */~
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
            bal_top$(3000)8,             /* Top Balance                */~
            bal_bot$(3000)8,             /* Bottom Balance             */~
            bal2_top$(3000)8,            /* Top Balance                */~
            bal2_bot$(3000)8,            /* Bottom Balance             */~
            blank$80,                    /* Blank Field                */~
            td$2,                        /* Tube Diameter              */~
            v$1                          /* View Top or Bottom         */

        dim readbc$5,                    /* AWDPLNBC Readkey           */~
            unit_type$1,                 /* Unit Type S, Cottage, Oriel*/~
            static$1,                    /* Is it static               */~
            loc_top$(3000)14,            /* Top Balance                */~
            loc_bot$(3000)14             /* Bottom Balance             */
            
 
            

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
            apc$   = "(AWD) Tube Windings/Coil Inquiry TEMP"
            pname$ = "AWDPLC60 - Rev: R7.00"

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
            * #3  ! APCPLNDT ! Production Master Detail file            *~
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
            * #17 ! APCCUTEQ ! Saw Optimization Cross-Reference File    *~
            * #18 ! AWDPLNBL ! Product and Balance Cross-Reference      *~
            * #19 ! AWDPLNBC ! Balance Location File                    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "AMTBOMCD",                                      ~
                        varc,     indexed,  recsize = 250,               ~
                        keypos = 1,    keylen = 42

            select #2,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

             select #3,  "APCPLNDT",                                     ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =   24, keylen =   23,                    ~
                        alt key  1, keypos =   47, keylen =  57,         ~
                            key  2, keypos  =  53, keylen =  51,         ~
                            key  3, keypos  =   1, keylen =  23, dup,    ~
                            key  4, keypos  =  96, keylen =   8, dup

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

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

            select #9, "AMTBOMIF",                                       ~
                        varc,     indexed,  recsize =  120,              ~
                        keypos =    1, keylen =  32

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

            select #17,  "APCCUTEQ",                                     ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    2, keylen =   7,                     ~
                        alt key  1, keypos  =     1, keylen =  8

            select #18, "AWDPLBL2"                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =  1,   keylen = 4       

            select #19, "AWDPLNBC",                                     ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos = 1,   keylen =  5                

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1,  fs%(1%),  f2%(1%),  0%, rslt$(1%))
            call "OPENCHCK" (#2,  fs%(2%),  f2%(2%),  0%, rslt$(2%))
            call "OPENCHCK" (#3,  fs%(3%),  f2%(3%),  0%, rslt$(3%))
            call "OPENCHCK" (#4,  fs%(4%),  f2%(4%),  0%, rslt$(4%))
            call "OPENCHCK" (#5,  fs%(5%),  f2%(5%),  0%, rslt$(5%)) 
            call "OPENCHCK" (#6,  fs%(6%),  f2%(6%),  0%, rslt$(6%))
            call "OPENCHCK" (#8,  fs%(8%),  f2%(8%),  0%, rslt$(8%))
            call "OPENCHCK" (#9,  fs%(9%),  f2%(9%),  0%, rslt$(9%))
            call "OPENCHCK" (#10, fs%(10%), f2%(10%), 0%, rslt$(10%))
            call "OPENCHCK" (#11, fs%(11%), f2%(11%), 0%, rslt$(11%))
            call "OPENCHCK" (#12, fs%(12%), f2%(12%), 0%, rslt$(12%))
            call "OPENCHCK" (#15, fs%(15%), f2%(15%), 0%, rslt$(15%))
            call "OPENCHCK" (#17, fs%(17%), f2%(17%), 0%, rslt$(17%))
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

            work% = 0%      /* Work File has been opened flag       */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   2%      
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
                  if keyhit%  = 10% then gosub process_data
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%            
            if fieldnr% < 1% or fieldnr% > 2% then editpg1 
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
            call "DATUFMTC" (sc_prddate$)
            dt_key$, wk_key$ = all(hex(00)) 
            str(dt_key$,,6) = sc_prddate$
            i% = 1%  :  cnt%, z% = 0%
            call "SHOSTAT" ("Loading Winding/Coil Data.....") 
            mode% = 1%  :  gosub open_work
            gosub load_data
            if work% <> 0% then gosub delete_work          
            k% = 0%
            call "DATFMTC" (sc_prddate$)
            gosub display_summary
            return

        load_data
            read #3, key 1 > dt_key$, using L20210, wk_rec$,             ~
                    eod goto read_work_file
L20210:         fmt ch(256)
            dt_key$ = str(wk_rec$,47,57)             /* Readkey 1  */
            if str(dt_key$,,6) > sc_prddate$ then goto read_work_file
            z% = z% + 1%
            if mod(z%,100%) <> 0 then goto L20230
                convert z% to temp$, pic(####0)
                print at (13,30), "Records Scanned:  " & temp$
L20230:     if str(dt_key$,13,3) <> sc_dept$ then goto load_data
            if str(dt_key$,18,2) > "11" then goto load_data /* Cmpltd  */
            cnt% = cnt% + 1%
            convert cnt% to temp$, pic(####0)
            if mod(cnt%,100%) <> 0 then goto L20240
                print at (14,30), "Records Selected: " & temp$
L20240:     gosub write_work_file
            goto load_data


        write_work_file
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
            eod goto L20310, data goto L20310

L20300:     fmt ch(6), ch(3), ch(5), ch(30), ch(2), ch(5), ch(10), ch(9),~
                ch(25)
L20310:     return


        read_work_file

            if i% <> 1% or mode% = 2% then goto L20400

                mode% = 2%  :  gosub open_work
                call "SHOSTAT" ("Loading Selected Records.....")
                cnt% = 0%

L20400:     gosub clear_line
            read #7, key > wk_key$, using L20410, wk_key$, dsp$(i%),     ~
                cus$(i%), part$, eod goto L20998
L20410:             fmt ch(51), ch(10), ch(9), ch(25)

            part_len% = 0% 
            part_len% = len(part$)


REM            if str(wk_key$,10%,5%) = "00138" then                        ~
                  call "SHOSTAT" ("I AM HERE " & str(wk_key$,10%,5%))
REM            if str(wk_key$,10%,5%) = "00138" then stop

REM            if str(wk_key$,10%,5%) = "00139" then                        ~
                  call "SHOSTAT" ("I AM HERE " & str(wk_key$,10%,5%))
REM            if str(wk_key$,10%,5%) = "00139" then stop




            cnt% = cnt% + 1%
            if mod(cnt%,50%) <> 0 then goto L20440
                convert cnt% to temp$, pic(####0)
                print at (14,30), "Record Processed: " & temp$
L20440:
            if part_len%  < 19 then goto read_work_file
REM            if str(sav_part$,1%,25%) = str(part$,1%,25%) then goto same_part
REM            str(sav_part$,1%,25%) = str(part$,1%,25%)

            if part_len%  >= 22 then gosub set_sclmr
            seqn$(i%) = str(wk_key$,10%,5%)
            mdl$(i%) = str(part$,,3)
            hinge$   = str(part$,9,2)
            wdt$(i%) = str(part$,13,4)
            hgt$(i%) = str(part$,17,3)
            sav_mdl$ = mdl$(i%)                           
            cus$(i%) = " "
            convert wdt$(i%) to wdt, data goto read_work_file

            convert wdt$(i%) to sav_wdt, data goto read_work_file 
            gosub lookup_model                                    
            wdt = wdt / 10              
            convert wdt to wdtkey$, pic(000.0)
            fctnl = wdt - int(wdt)          /* Convert to decimal      */
            fctnl = (fctnl / 8) * 10
            wdt = int(wdt) + fctnl
            convert wdt to wdt$(i%), pic(##0.000)
            convert hgt$(i%) to hgt, data goto read_work_file
            hgt = hgt / 10              
            convert hgt to hgtkey$, pic(000.0)
            fctnl = hgt - int(hgt)          /* Convert to decimal      */
            fctnl = (fctnl / 8) * 10
            hgt = int(hgt) + fctnl         /* Reused in calc_tube_hgts */ 
            convert hgt to hgt$(i%), pic(##0.000)




            gosub check_ctg_orl
            gosub calc_glass_size
            gosub find_balance
goto new_bal

new_bal:

            if unit_type$ = "C" ~
                        then  str(inf$(i%),13%,3%) = "COT"
            if unit_type$ = "O" ~
                        then  str(inf$(i%),13%,3%) = "ORL"

            call "STRING" addr("RJ", wcl$(i%), 6%)  /* Both steps reqd.*/
            call "STRING" addr("CT", wcl$(i%), 6%)  /* to center data. */

            call "STRING" addr("RJ", bal_top$(i%), 8%)  /* Both steps reqd.*/
            call "STRING" addr("CT", bal_top$(i%), 8%)  /* to center data. */ 
            call "STRING" addr("RJ", bal_bot$(i%), 8%)  /* Both steps reqd.*/
            call "STRING" addr("CT", bal_bot$(i%), 8%)  /* to center data. */

            call "STRING" addr("RJ", bal2_top$(i%), 8%)  /* Both steps reqd.*/
            call "STRING" addr("CT", bal2_top$(i%), 8%)  /* to center data. */ 
            call "STRING" addr("RJ", bal2_bot$(i%), 8%)  /* Both steps reqd.*/
            call "STRING" addr("CT", bal2_bot$(i%), 8%)  /* to center data. */  

            if str(bal_top$(i%)) <> " " or str(bal_bot$(i%)) <> " "         ~
               then gosub calc_tube_hgts  


            if tht$(i%) = "." and co_flag$ <> " " then tht$(i%) = co_flag$
            if str(inf$(i%),1%,12%) <= "." then gosub load_color
            mdl$(i%) = sav_mdl$                  
            wdt = sav_wdt
            wdt = wdt / 10
            convert wdt to wdtkey$, pic(000.0)
            fctnl = wdt - int(wdt)          /* Convert to decimal      */
            fctnl = (fctnl / 8) * 10
            wdt = int(wdt) + fctnl
            convert wdt to wdt$(i%), pic(##0.000)    
REM            gosub calc_balance_cover                 
REM            gosub calc_balance_cover_new
            gosub load_screen                    
            dept_max% = i%
            i% = i% + 1%
            if i% < 3001% then goto read_work_file
            gosub warn_missing_data
           
L20998:     if i% <= 3000% then gosub clear_line
            if dept_max% = 0% then goto L20999
            for x% = dept_max% + 1% to 3000%
                bal_top$(x%), tht$(x%), cus$(x%), inf$(x%),  ~ 
                bal_bot$(x%), tdi_top$(x%), tdi_bot$(x%),    ~
                wgt_top$(x%), wgt_bot$(x%), hgt_top$(x%),    ~
                hgt_bot$(x%), loc_top$(x%), loc_bot$(x%),    ~
                bal2_top$(x%), bal2_bot$(x%) = " "
            next x%
            return

L20999:     errormsg$ = "No data to display for this date/department."
            gosub error_prompt
            return clear all
            goto inputmode
REM same_part
REM            str(part$,1%,25%) = " "
               cc$(i%)      = " "
               seqn$(i%)    = str(wk_key$,10%,5%)
               mdl$(i%)     = mdl$(i% - 1%)
               wdt$(i%)     = wdt$(i% - 1%)
               hgt$(i%)     = hgt$(i% - 1%)
               wcl$(i%)     = wcl$(i% - 1%) 
               tht$(i%)     = tht$(i% - 1%)
               cus$(i%)     = cus$(i% - 1%)
               inf$(i%)     = inf$(i% - 1%)
               bal_top$(i%) = bal_top$(i% - 1%)
               bal_bot$(i%) = bal_bot$(i% - 1%)

               bal2_top$(i%) = bal2_top$(i% - 1%)
               bal2_bot$(i%) = bal2_bot$(i% - 1%)

               tdi_top$(i%) = tdi_top$(i% - 1%)       
               tdi_bot$(i%) = tdi_bot$(i% - 1%)       
               wgt_top$(i%) = wgt_top$(i% - 1%)
               wgt_bot$(i%) = wgt_bot$(i% - 1%)
               hgt_top$(i%) = hgt_top$(i% - 1%)
               hgt_bot$(i%) = hgt_bot$(i% - 1%)
            i% = i% + 1%
            goto read_work_file


            read_gencds2
               found% = 0%
               read #4, key = readkey$, eod goto L22290                    
               found% = 1%
L22290:        return



        calc_tube_hgts
            ht1%, ht2% = 999%  :  factor = 0
            init(" ") readkey$, descr$                      
            str(readkey$,1%,9%)   = "PLAN BALC"
            str(readkey$,10%,3%) = mdl$(i%)                  
            for x% = 1% to 2%                                
              if x% = 2% then str(readkey$,13%,2%) = tdi_bot$(i%)         ~
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
            str(readkey$,10%,15%) = mdl$(i%) & tdi_bot$(i%)
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
            tht$(i%) = "    -xxx "
            if co_flag$ <> " "                                           ~
                then str(tht$(i%),5,1) = "/"   /* Denotes diff in calc */
L22580:     if sngl$ <> "S" or co_flag$ <> " "                           ~
                then convert ht1% to str(tht$(i%),,3), pic(##0)
            convert ht2% to str(tht$(i%),6,3), pic(##0)
            return


        load_color                                   
            readkey$ = "COLOR    " & str(part$,4,1)   /* Only Need */
                                                      /* 1st 8 Char*/   
            read #4, key = readkey$, using L22600, str(inf$(i%),1%,12%),   ~
                     eod goto L22610
L22600:          fmt pos(30), ch(15)
L22610:     return

        load_screen                      
            p% = 0%
            p% = len(inf$(i%)) + 1%
            q% = (14% - p%)
            
            if str(wk_key$,7%,3%) = "002" or str(wk_key$,7%,3%) = "007"     ~
                                                       then gosub check_tso
            if str(wk_key$,7%,3%) = "049" or str(wk_key$,7%,3%) = "052"     ~
                                                      then gosub check_tso

            if str(wk_key$,7%,3%) = "002" or str(wk_key$,7%,3%) = "007"     ~
               or str(wk_key$,7%,3%) = "036"         then gosub check_bso


            if str(wk_key$,7%,3%) = "025" or str(wk_key$,7%,3%) = "026"     ~
                                                      then gosub check_bso

            if str(wk_key$,7%,3%) = "027" or str(wk_key$,7%,3%) = "028"     ~
                                                      then gosub check_bso

            if str(wk_key$,7%,3%) = "049" or str(wk_key$,7%,3%) = "052"     ~
                                                      then gosub check_bso

            if str(wk_key$,7%,3%) = "036"             then gosub check_fgo


            if str(wk_key$,7%,3%) = "025" or str(wk_key$,7%,3%) = "026"     ~
                                                      then gosub check_fgo

            if str(wk_key$,7%,3%) = "027" or str(wk_key$,7%,3%) = "028"     ~
                                                      then gosub check_fgo

REM CR2957   if str(wk_key$,7%,3%) = "006"             then gosub check_shims
        return

        check_tso    /* test for tso, bso, fgo */
           if str(wk_key$,7%,3%) = "052" then goto L22620
              gosub check_total_diameter

L22620:    if str(part$,11%,1%) = "4" then        ~
                 str(inf$(i%), p%,q%) = " / TSO"
        return

        check_bso
           if str(wk_key$,7%,3%) = "036" or str(wk_key$,7%,3%) = "052"   ~
                                                         then goto L22630   


           if str(wk_key$,7%,3%) = "025" or str(wk_key$,7%,3%) = "026"   ~
                                                         then goto L22630 


           if str(wk_key$,7%,3%) = "027" or str(wk_key$,7%,3%) = "028"   ~
                                                         then goto L22630   
     
              gosub check_total_diameter
        
L22630:    if str(part$,11%,1%) = "5" then        ~
                 str(inf$(i%), p%,q%) = " / BSO"
        return

        check_fgo
           if str(part$,11%,1%) = "6" then        ~
                 str(inf$(i%), p%,q%) = " / FGO"
        return

        check_total_diameter   /* If tube diameter is 99 and height is >= 67 */
           height = 0.0  :  init(" ") height$  
           if (tdi_bot$(i%) <> "99" and tdi_bot$(i%) <> ". " )    ~ 
                           or hgt < 67 then return

           height = round((hgt - 67) / 2, 4)

           convert height to height$, pic(####.####-)

           call "SPCSMASH" (height$)
           
           str(inf$(i%), p%,q%) = " / " & height$
        return                                         

        check_shims
           init(" ") height$
           if hgt < 60 then return

           str(inf$(i%), p%,q%) = " / SHIMS"               
        return


        clear_line
            seqn$(i%), mdl$(i%), wdt$(i%), hgt$(i%),  dsp$(i%), tubedi$(),~
                 co_flag$, unit_type$ = " "
            bal_top$(i%), bal_bot$(i%), tdi_top$(i%), tdi_bot$(i%),       ~
                 tht$(i%), cus$(i%), inf$(i%), wgt_top$(i%), wgt_bot$(i%),~
                 hgt_top$(i%), hgt_bot$(i%), loc_top$(i%), loc_bot$(i%),  ~
                 bal2_top$(i%), bal2_bot$(i%) = "."  
            sav_mdl$, sav_part$ = " "   
            cvr_height, s_clmr = 0.00
            return
        
        lookup_model                                      
            init(" ") readkey$, descr$          
            div_fact% = 0%
            fact = 0.00
            str(readkey$,1%,9%)   = "PLAN TMDL"
            str(readkey$,10%,15%) = mdl$(i%)
            read #4,key = readkey$, using L22700, desc$, eod goto no_model
L22700:        FMT POS(25), CH(30)   
            mdl$(i%) = str(desc$,1%,3%)                    
            convert str(desc$,12%,1%) to div_fact%, data goto no_model

            convert str(desc$,22%,5%) to fact, data goto no_model

            wdt = wdt / div_fact%
            wdt = wdt - fact

        no_model
        return                                           


REM        calc_balance_cover  
           gosub get_equation
           if eq% = 0% then return


           pd$ = str(mdl$(i%),1%,1%)
           tw$ = "1" : th$ = "2"
           call "APCCUTLD" (pd$, cw%, ch%, tw$, th$, #4, err% )
           if err% = 0% then goto L04510
              errormsg$ =                                                ~
                  "(Error) - Saw Cuts not Defined in (EQUATIONS) Table."
              return
L04510:    sa_mfg% = 0%                            /* MFG WINDOWS ONLY */
REM        call "SHOSTAT" ("Calculating Saw Cuts for Part")
/* (CUT001) */
           call "APCCUTCC" ( part$, 0%, 0%, 0%, sa_mfg%, cw%, ch%, eq$(),~
               ct$(), cr$(), cp$(), cc_ans$(), co$(), ct(), sh$(), tw$,  ~
               th$, #17, #1, #4, err%)

REM        gosub get_equation
           ct% = 0%
           ct% = int(ct(equ% + cw%))
REM           cus$(i%) = ct$(equ% + cw%)
           convert ct% to cus$(i%), pic(-########)

           if err% <> 0% then return
              errormsg$ =                                                ~
                  "(Error) - No Cut References Defined for Model/Color."
        return


        get_equation
            eq% = 0%
            str(readkey$,1%,9%)   = "PLAN TEQU"
            str(readkey$,10%,15%) = mdl$(i%)
            read #4,key = readkey$, using L22500, descr$, eod goto no_equ


            convert str(descr$,,2) to equ%, data goto no_equ
            eq% = 1%
            
        no_equ
              errormsg$ =                                                ~
                  "(Error) - No Equation Numbers Defined."
        return


        set_sclmr
               a1 = 0.0 : a2 = 0.0
               convert str(part$,20%,2%) to a1, data goto L04550

               convert str(part$,22%,1%) to a2, data goto L04560
L04560:
               s_clmr = a1 + (a2/8.0)
L04550:     if s_clmr <= 8.0 then s_clmr = 0.0
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
                      sc_prddate$, sc_user$, part$, windcl$(), billto$,  ~
                      cc$(), seqn$(), mdl$(), wdt$(), find_seq$,         ~
                      hgt$(), dsp$(), tubedi$(), v$, loc_bot$(), loc_top$()

            init(".") wcl$(), tht$(), cus$(), inf$() /* Reqd.(LINSMASH)*/

            dept_max% = 0%
            
            if work% <> 0% then gosub delete_work

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
                                L40160           /* sc_user$           */

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40170:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
                                                                         ~
               at (03,02), "Production Date:",                           ~
               at (03,25), fac(lfac$(1%)), sc_prddate$          , ch(10),~
                                                                         ~
               at (04,02), "Dept Code:",                                 ~
               at (04,25), fac(lfac$(2%)), sc_dept$             , ch(03),~
               at (04,40), fac(hex(84)),   sc_dept_d$           , ch(30),~
                                                                         ~
               at (05,02), "Production User ID:",                        ~
               at (05,25), fac(lfac$(3%)), sc_user$             , ch(03),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 8% then goto L40380
                  tab% = 1%
                  gosub display_codes
                  goto L40190

L40380:        if keyhit% <> 9% then goto L40390           /* Report   */
                                                          
                  call "AWDPLD60" (#2, #3, #6, #4, #5, #15, #18, #1, #19,    ~
                                      minth%, maxth%)
                  goto L40190                 

L40390:        if keyhit% <> 10% then goto L40395
                  gosub process_data
                  goto L40190

L40395:        if keyhit% <> 13% then goto L40400
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
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                                       "
            pf$(2) = "                 (8)Display Departments " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (9)Generate Report     " &        ~
                     "(13)Print Queue        (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffff0809ffffff0dff0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(3),18,23) = " "  :  str(pfkeys$, 9,1) = hex(ff)
                str(pf$(3),41,15) = " "  :  str(pfkeys$,13,1) = hex(ff)
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40570:     if fieldnr% > 1% then L40580                   
                str(pf$(1),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40580:     if fieldnr% = 2% then L40590                   
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 8,1) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
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
               at (01,65), fac(hex(84)), pageno$                , ch(16),~
                                                                         ~
               at (01,21), fac(hex(a4)), title$                 , ch(40),~
                                                                         ~
               at (03,04), fac(hex(a4))  , h1$                  , ch(05),~
               at (03,10), fac(hex(a4))  , h2$                  , ch(03),~
               at (03,14), fac(hex(a4))  , h3$                  , ch(07),~
               at (03,22), fac(hex(a4))  , h4$                  , ch(07),~
               at (03,30), fac(hex(a4))  , h5$                  , ch(08),~
               at (03,39), fac(hex(a4))  , h6$                  , ch(02),~     
               at (03,42), fac(hex(a4))  , h7$                  , ch(08),~
               at (03,51), fac(hex(a4))  , h8$                  , ch(02),~
               at (03,54), fac(hex(a4))  , h11$                 , ch(09),~
               at (03,65), fac(hex(a4))  , h10$                 , ch(15),~
                                                                         ~
               at (04,02), fac(hex(81))  , cc$(k% + 1%)         , ch(01),~
               at (04,04), fac(hex(84))  , seqn$(k% + 1%)       , ch(05),~
               at (04,10), fac(hex(84))  , mdl$(k% + 1%)        , ch(03),~
               at (04,14), fac(hex(84))  , wdt$(k% + 1%)        , ch(07),~
               at (04,22), fac(hex(84))  , hgt$(k% + 1%)        , ch(07),~
               at (04,30), fac(hex(84))  , bal_top$(k% + 1%)    , ch(08),~ 
               at (04,39), fac(lfac$(1)) , tdi_top$(k% + 1%)    , ch(02),~ 
               at (04,42), fac(hex(84))  , bal_bot$(k% + 1%)    , ch(08),~
               at (04,51), fac(lfac$(1)) , tdi_bot$(k% + 1%)    , ch(02),~
               at (04,54), fac(hex(84))  , cus$(k% + 1%)        , ch(09),~ 
               at (04,65), fac(hex(84))  , inf$(k% + 1%)        , ch(15),~ 
                                                                         ~
               at (05,30), fac(hex(84))  , bal2_top$(k% + 1%)   , ch(08),~
               at (05,42), fac(hex(84))  , bal2_bot$(k% + 1%)   , ch(08),~
                                                                         ~
               at (06,02), fac(hex(81))  , cc$(k% + 2%)         , ch(01),~
               at (06,04), fac(hex(84))  , seqn$(k% + 2%)       , ch(05),~
               at (06,10), fac(hex(84))  , mdl$(k% + 2%)        , ch(03),~
               at (06,14), fac(hex(84))  , wdt$(k% + 2%)        , ch(07),~
               at (06,22), fac(hex(84))  , hgt$(k% + 2%)        , ch(07),~
               at (06,30), fac(hex(84))  , bal_top$(k% + 2%)    , ch(08),~ 
               at (06,39), fac(lfac$(2)) , tdi_top$(k% + 2%)    , ch(02),~ 
               at (06,42), fac(hex(84))  , bal_bot$(k% + 2%)    , ch(08),~
               at (06,51), fac(lfac$(2)) , tdi_bot$(k% + 2%)    , ch(02),~
               at (06,54), fac(hex(84))  , cus$(k% + 2%)        , ch(09),~ 
               at (06,65), fac(hex(84))  , inf$(k% + 2%)        , ch(15),~ 
                                                                         ~
               at (07,30), fac(hex(84))  , bal2_top$(k% + 2%)   , ch(08),~
               at (07,42), fac(hex(84))  , bal2_bot$(k% + 2%)   , ch(08),~
                                                                         ~
               at (08,02), fac(hex(81))  , cc$(k% + 3%)         , ch(01),~
               at (08,04), fac(hex(84))  , seqn$(k% + 3%)       , ch(05),~
               at (08,10), fac(hex(84))  , mdl$(k% + 3%)        , ch(03),~
               at (08,14), fac(hex(84))  , wdt$(k% + 3%)        , ch(07),~
               at (08,22), fac(hex(84))  , hgt$(k% + 3%)        , ch(07),~
               at (08,30), fac(hex(84))  , bal_top$(k% + 3%)    , ch(08),~ 
               at (08,39), fac(lfac$(3)) , tdi_top$(k% + 3%)    , ch(02),~ 
               at (08,42), fac(hex(84))  , bal_bot$(k% + 3%)    , ch(08),~
               at (08,51), fac(lfac$(3)) , tdi_bot$(k% + 3%)    , ch(02),~
               at (08,54), fac(hex(84))  , cus$(k% + 3%)        , ch(09),~  
               at (08,65), fac(hex(84))  , inf$(k% + 3%)        , ch(15),~ 
                                                                         ~
               at (09,30), fac(hex(84))  , bal2_top$(k% + 3%)   , ch(08),~
               at (09,42), fac(hex(84))  , bal2_bot$(k% + 3%)   , ch(08),~
                                                                         ~
               at (10,02), fac(hex(81))  , cc$(k% + 4%)         , ch(01),~
               at (10,04), fac(hex(84))  , seqn$(k% + 4%)       , ch(05),~
               at (10,10), fac(hex(84))  , mdl$(k% + 4%)        , ch(03),~
               at (10,14), fac(hex(84))  , wdt$(k% + 4%)        , ch(07),~
               at (10,22), fac(hex(84))  , hgt$(k% + 4%)        , ch(07),~
               at (10,30), fac(hex(84))  , bal_top$(k% + 4%)    , ch(08),~ 
               at (10,39), fac(lfac$(4)) , tdi_top$(k% + 4%)    , ch(02),~ 
               at (10,42), fac(hex(84))  , bal_bot$(k% + 4%)    , ch(08),~
               at (10,51), fac(lfac$(4)) , tdi_bot$(k% + 4%)    , ch(02),~
               at (10,54), fac(hex(84))  , cus$(k% + 4%)        , ch(09),~ 
               at (10,65), fac(hex(84))  , inf$(k% + 4%)        , ch(15),~ 
                                                                         ~
               at (11,30), fac(hex(84))  , bal2_top$(k% + 4%)   , ch(08),~
               at (11,42), fac(hex(84))  , bal2_bot$(k% + 4%)   , ch(08),~
                                                                         ~
               at (12,02), fac(hex(81))  , cc$(k% + 5%)         , ch(01),~
               at (12,04), fac(hex(84))  , seqn$(k% + 5%)       , ch(05),~
               at (12,10), fac(hex(84))  , mdl$(k% + 5%)        , ch(03),~
               at (12,14), fac(hex(84))  , wdt$(k% + 5%)        , ch(07),~
               at (12,22), fac(hex(84))  , hgt$(k% + 5%)        , ch(07),~
               at (12,30), fac(hex(84))  , bal_top$(k% + 5%)    , ch(08),~ 
               at (12,39), fac(lfac$(5)) , tdi_top$(k% + 5%)    , ch(02),~ 
               at (12,42), fac(hex(84))  , bal_bot$(k% + 5%)    , ch(08),~
               at (12,51), fac(lfac$(5)) , tdi_bot$(k% + 5%)    , ch(02),~
               at (12,54), fac(hex(84))  , cus$(k% + 5%)        , ch(09),~ 
               at (12,65), fac(hex(84))  , inf$(k% + 5%)        , ch(15),~
                                                                         ~
               at (13,30), fac(hex(84))  , bal2_top$(k% + 5%)   , ch(08),~
               at (13,42), fac(hex(84))  , bal2_bot$(k% + 5%)   , ch(08),~
                                                                         ~
               at (14,02), fac(hex(81))  , cc$(k% + 6%)         , ch(01),~
               at (14,04), fac(hex(84))  , seqn$(k% + 6%)       , ch(05),~
               at (14,10), fac(hex(84))  , mdl$(k% + 6%)        , ch(03),~
               at (14,14), fac(hex(84))  , wdt$(k% + 6%)        , ch(07),~
               at (14,22), fac(hex(84))  , hgt$(k% + 6%)        , ch(07),~
               at (14,30), fac(hex(84))  , bal_top$(k% + 6%)    , ch(08),~ 
               at (14,39), fac(lfac$(6)) , tdi_top$(k% + 6%)    , ch(02),~ 
               at (14,42), fac(hex(84))  , bal_bot$(k% + 6%)    , ch(08),~
               at (14,51), fac(lfac$(6)) , tdi_bot$(k% + 6%)    , ch(02),~
               at (14,54), fac(hex(84))  , cus$(k% + 6%)        , ch(09),~ 
               at (14,65), fac(hex(84))  , inf$(k% + 6%)        , ch(15),~
                                                                         ~
               at (15,30), fac(hex(84))  , bal2_top$(k% + 6%)   , ch(08),~
               at (15,42), fac(hex(84))  , bal2_bot$(k% + 6%)   , ch(08),~
                                                                         ~
               at (16,02), fac(hex(81))  , cc$(k% + 7%)         , ch(01),~
               at (16,04), fac(hex(84))  , seqn$(k% + 7%)       , ch(05),~
               at (16,10), fac(hex(84))  , mdl$(k% + 7%)        , ch(03),~
               at (16,14), fac(hex(84))  , wdt$(k% + 7%)        , ch(07),~
               at (16,22), fac(hex(84))  , hgt$(k% + 7%)        , ch(07),~
               at (16,30), fac(hex(84))  , bal_top$(k% + 7%)    , ch(08),~ 
               at (16,39), fac(lfac$(7)) , tdi_top$(k% + 7%)    , ch(02),~ 
               at (16,42), fac(hex(84))  , bal_bot$(k% + 7%)    , ch(08),~
               at (16,51), fac(lfac$(7)) , tdi_bot$(k% + 7%)    , ch(02),~
               at (16,54), fac(hex(84))  , cus$(k% + 7%)        , ch(09),~ 
               at (16,65), fac(hex(84))  , inf$(k% + 7%)        , ch(15),~
                                                                         ~
               at (17,30), fac(hex(84))  , bal2_top$(k% + 7%)   , ch(08),~
               at (17,42), fac(hex(84))  , bal2_bot$(k% + 7%)   , ch(08),~
                                                                         ~
               at (18,02), fac(hex(81))  , cc$(k% + 8%)         , ch(01),~
               at (18,04), fac(hex(84))  , seqn$(k% + 8%)       , ch(05),~
               at (18,10), fac(hex(84))  , mdl$(k% + 8%)        , ch(03),~
               at (18,14), fac(hex(84))  , wdt$(k% + 8%)        , ch(07),~
               at (18,22), fac(hex(84))  , hgt$(k% + 8%)        , ch(07),~
               at (18,30), fac(hex(84))  , bal_top$(k% + 8%)    , ch(08),~
               at (18,39), fac(lfac$(8)) , tdi_top$(k% + 8%)    , ch(02),~ 
               at (18,42), fac(hex(84))  , bal_bot$(k% + 8%)    , ch(08),~
               at (18,51), fac(lfac$(8)) , tdi_bot$(k% + 8%)    , ch(02),~
               at (18,54), fac(hex(84))  , cus$(k% + 8%)        , ch(09),~ 
               at (18,65), fac(hex(84))  , inf$(k% + 8%)        , ch(15),~
               at (19,02), fac(hex(a4))  , blank$               , ch(78),~
               at (19,30), fac(hex(84))  , bal2_top$(k% + 8%)   , ch(08),~
               at (19,42), fac(hex(84))  , bal2_bot$(k% + 8%)   , ch(08),~
                                                                         ~
                                                                         ~
               at (20,10), "Production User ID = ",                      ~
               at (20,31), fac(hex(8c))  , sc_user$             , ch(03),~
               at (20,54), fac(hex(8c))  , dsp_text$            , ch(11),~
                                                                         ~
               at (21,02), fac(hex(a4)),   dsp_msg$             , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   str(pf$(3%),,52%)    , ch(52),~
               at (24,55), fac(hex(a2)),   find_seq$            , ch(05),~
               at (24,61), fac(hex(8c)),   str(pf$(3%),60%,20%) , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

                if keyhit% <> 0% then goto L41010
                    flg% = 0%
                    for x% = 1% to dept_max%
                        if cc$(x%) = " " then goto L41005
                        if cc$(x%) <> "X" then goto L41003
                          seqn$(x%), mdl$(x%), wdt$(x%), hgt$(x%),       ~
                              tht$(x%),                                  ~
                              tdi_top$(x%), tdi_bot$(x%), bal_top$(x%),  ~
                              bal_bot$(x%), cus$(x%), inf$(x%), dsp$(x%),~
                              wgt_top$(x%), wgt_bot$(x%), hgt_top$(x%),  ~
                              hgt_bot$(x%), loc_top$(x%), loc_bot$(x%),  ~
                              bal2_top$(x%), bal2_bot$(x%) = " "
                          dept_max% = dept_max% - 1%
                          flg% = 1%
L41003:                 if cc$(x%) <> "D" then goto L41005

                          call "EWDPLA58" (2%, str(dsp$(x%),1,8),        ~
                            str(dsp$(x%),9,2), " ", " ",                 ~
                            wgt_top$(x%),wgt_bot$(x%),hgt_top$(x%),      ~
                            hgt_bot$(x%),loc_top$(x%),loc_bot$(x%),      ~
                            sash_stop_bot$(x%),                          ~
                            #10, #6, #2, #4, #9, #8, #1, #11, #12, 0%)
L41005:             next x%
                    if flg% = 0% then goto L41007   /* Nothing marked */
                    call "LINSMASH" (seqn$())       /* for removal.   */
                    call "LINSMASH" (mdl$())
                    call "LINSMASH" (wdt$())                  
                    call "LINSMASH" (hgt$())
                    call "LINSMASH" (bal_top$())
                    call "LINSMASH" (bal_bot$())

                    call "LINSMASH" (bal2_top$())
                    call "LINSMASH" (bal2_bot$())

                    call "LINSMASH" (tht$())
                    call "LINSMASH" (tdi_top$())            
                    call "LINSMASH" (tdi_bot$())            
                    call "LINSMASH" (cus$())
                    call "LINSMASH" (inf$())
                    call "LINSMASH" (dsp$())
                    call "LINSMASH" (loc_top$()) 
                    call "LINSMASH" (loc_bot$())

L41007:             init(" ") cc$()
                    if flg% <> 0% then goto L41020 else goto L41000

L41010:         if keyhit% <> 2% then goto L41040          /* First    */
L41020:           k% = 0%
                  goto L41000

L41040:        if keyhit% <> 3% then goto L41080          /* Last      */
L41060:           x% = int(val_max% / 8%)
                  k% = (x%*8%)
                  goto L41000

L41080:        if keyhit% <> 4% then goto L41100           /* Previous */
                  if k% < 9% then goto L41020
                  k% = k% - 8%
                  if k% <= 1% then goto L41020
                  goto L41000

L41100:        if keyhit% <> 5% then goto L41105           /* Next     */
                  k% = k% + 8%
                  if k% < val_max% then goto L41000
                  goto L41060

L41105:        if keyhit% <> 6% then goto L41110           /* Down 1 Ln*/
                  k% = k% - 1%
                  if k% <= 1% then goto L41020
                  goto L41000

L41110:        if keyhit% <> 7% then goto L41115           /* Up 1 Line*/
                  k% = k% + 1%
                  if k% < val_max% then goto L41000
                  goto L41060

L41115:        if keyhit% <> 8% then goto L41120           /* Find Seq.*/
                  if find_seq$ = " " then goto L41000     
                  convert find_seq$ to temp, data goto L41000
                  convert temp to find_seq$, pic(00000)
                  for x% = 1% to val_max%              
                    if seqn$(x%) >= find_seq$ then k% = x% - 1% else    ~
                  next x%
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
            if sc_user$ = " " then sc_user$ = userid$
            option$ = "PrdDt=xxxxxxxxxx"
            str(option$,7%,10%) = sc_prddate$
            title$ = "(Display) Wind/Coil Data for Dept. xxx "
            str(title$,36,3) = sc_dept$
            dsp_msg$ = "Key 'D' & press <ENTER> for Detail; Key"        ~
                     & " 'X' & press <ENTER> to Remove Line(s). " 
            pageno$ = "Page: XXX of XXX"        /* k% = Array Values */
            h1$ = "Seq.#"
            h2$ = "Mdl"
            h3$ = "Width  "
            h4$ = "Height "
            h5$ = "Bal/WndT"
            h6$ = "TD"
            h7$ = "Bal/WndB"
            h8$ = "TD"       
            h9$ = "Tube Hgts"
            h10$ = "Model/Cust.Info" 
            h11$ = "Bal Covr"

            val_max% = dept_max%
            if val_max% > (3000% - 8%) then val_max% = 3000% - 8%
                                                        /* Display Max */
            yy% = ( val_max% / 8% ) + 1%
            xx% = (k% / 8%) + 1%
            if k% + 8% > val_max% then xx% = yy%
            convert xx% to str(pageno$,7%,3%), pic(###)/* Current Page */

            convert yy% to str(pageno$,14%,3%), pic(###)/* Total Pages */

            pf$(1) = "(2)First           (5)Next              " &        ~
                     "                                       "
            pf$(2) = "(3)Last            (6)Down 1 Line       " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(4)Previous        (7)Up 1 Line         " &        ~
                     "(8)Find Seq: xxxxx     (16)Exit Display" 
            pfkeys$ = hex(ff02030405060708ffffffffffff0f1000)
            gosub check_screen
            dsp_text$ = " Upr - Lwr "                         
                                                
            for x% = 1% to 8%
                lfac$(x%) = hex(84)
                readkey$ = "PLAN TDHL" & "D" & sc_dept$ & tdi_bot$(k% + x%)
                    gosub read_gencds2
                    if found% <> 0% then lfac$(x%) = hex(94)

                    if found% <> 0% then str(inf$(k% + x%),9%,6%) = "<-----"
                readkey$ = "PLAN TDHL" &"M"& mdl$(k% + x%) & tdi_bot$(k% + x%)
                    gosub read_gencds2
                                                                
                    if found% <> 0% then lfac$(x%) = hex(94)
                    if found% <> 0% then str(inf$(k% + x%),9%,6%) = "<-----"
                if str(tht$(k%+x%),5,1)="/" then dsp_text$="/ = Ctg/Orl"
            next x%
                   
        return
              

        check_screen
            if val_max% > 8% then goto L41160
               gosub no_first
               gosub no_next
               gosub no_last
               gosub no_prev
               return
L41160:      if k% >= 1% then goto L41180
                gosub no_first
                gosub no_prev
L41180:      if (k% + 8%) <= val_max% then goto L41200
                gosub no_last
L41200:      if k% <= (val_max% - 8%) then goto L41220
                gosub no_next
L41220: return
        no_first
            str(pf$(1%),1%, 9%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_next
            str(pf$(1%),20%, 9%) = " " : str(pfkeys$,5%,1%) = hex(ff)
            str(pf$(3%),20%,12%) = " " : str(pfkeys$,7%,1%) = hex(ff)
        return
        no_last
            str(pf$(2%),1%,9%)   = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prev
            str(pf$(3%),1%,12%)  = " " : str(pfkeys$,4%,1%) = hex(ff)
            str(pf$(2%),20%,14%) = " " : str(pfkeys$,6%,1%) = hex(ff)
        return



        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50010,        /* Production Date        */~
                              L50050,        /* Department Code        */~
                              L50100         /* Production User ID     */


            return

L50010: Rem Enter a Production Date                sc_prddate$
            call "DATEOKC" (sc_prddate$, 0%, errormsg$)
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

L50100: Rem Enter a User ID for Production          sc_user$ 
            call "EWDUSRVL" (sc_user$, flag$)                
            if flag$ = "Y" then goto L50110
                errormsg$ = "(Error) Invalid User ID"
                gosub error_prompt
                sc_user$ = " "
L50110:     return


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
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        calc_glass_size
            opts% = 0% 
            dept$ = str(wk_key$,7%,3%)
            g_cnt%, ct%, er% = 0% 
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

        check_sclmr                            /* Both - Cottage/Oriel */
          s_clmr% = 0%
          if s_clmr < 1.0 then return



            a1 = 0.0 : a2 = 0.0
            convert str(part$,17%,2%) to a1, data goto L50150
L50150:
            convert str(part$,19%,1%) to a2, data goto L50160
L50160:
            s_height = a1 + (a2/8.0)               /* Decimal Height   */

            init(" ") readkey$, descr$
            readkey$   = "GLASS10  " & str(part$,1%,3%)
            gosub read_gencds2
            
            if found% = 0% then return
            get #4, using L22500, descr$

            t_clmr = 0.0 : b_clmr = 0.0 : tb_clmr = 0.0
            convert str(descr$,1%,8%) to t_clmr, data goto L50200

            convert str(descr$,12%,8%) to b_clmr, data goto L50200

            convert str(descr$,22%,8%) to tb_clmr, data goto L50190
L50190

               top_height =((s_height/2.0) - t_clmr)                     ~
                                  - (((s_height/2.0) + tb_clmr) - s_clmr)

               bot_height =((s_height/2.0) - b_clmr)                      ~
                                  + (((s_height/2.0) + tb_clmr) - s_clmr)
REM            call "SHOSTAT" ( "I AM HERE AT CALCULATING HEIGHT")  stop
               s_clmr% = 1%
        return
L50200:      err% = 3%
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

        lookup_awdplnbl
             bal% = 0%
             init(" ") prod$, vendor$, bal_type$
             prod$ = str(part$,1%,3%) & "T"
             if v$ = "B" then str(prod$,4%,1%) = "B"

             read #18, key = prod$, eod goto no_bal
                    get #18, using L52000, vendor$, bal_type$,          ~ 
                                          friction, weight, perimeter
/*(AWD002)*/
L52000:                     FMT POS(5), CH(1), CH(1), PD(14,4), PD(14,4),~
                                 PD(14,4)

                     bal% = 1%
        no_bal
        return

        calc_balance
             gosub calc_sash_weight             

             if vendor$ = "0" then gosub calc_amesbury
             if vendor$ = "1" then gosub calc_amesbury
             if bal_type$ = "0" then gosub calc_sprial_unique
REM             gosub calc_sprial_unique


             gosub read_awdplnwc


             if v$ = "T" then                                   ~
                  convert total_weight to wgt_top$(i%), pic(-######.####)
             if v$ = "T" then                                   ~
                  convert dec_height to hgt_top$(i%), pic(-######.####)

             if v$ = "B" then                                   ~
                  convert total_weight to wgt_bot$(i%), pic(-######.####)
             if v$ = "B" then                                   ~
                  convert dec_height to hgt_bot$(i%), pic(-######.####)


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
            get #4, using L22500, descr$

            strength$ = str(descr$,3%,1%)
        return
        get_desc2
            get #4, using L22500, descr$

            if v$ = "T" then strength$ = str(descr$, 3%,1%)
            if v$ = "B" then strength$ = str(descr$,20%,1%)
        return


        calc_amesbury
             vinyl        = (((dec_width * weight) + (dec_height * weight)) * 2)
/*(AWD002)*/
             vinyl        = vinyl + perimeter

             gls_surface  = ((dec_width * dec_height) / 144)
             gls_weight   = (gls_surface * strength)
             gls_friction = ((2 * dec_height) * friction)


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

REM                   if str(wk_key$,10%,5%) = "01012" then               ~ 
                      call "SHOSTAT" (" I AM HERE at BOTTOM " ) 
REM                   if str(wk_key$,10%,5%) = "01012" then stop
REM                call "SHOSTAT"("BAL KEY --> " & str(bal_rec$,4%,8%))  stop
REM                call "SHOSTAT"("TD      --> " & td$     )  stop

                   if bal_type$ = "0" then gosub set_sprial
                   if bal_type$ <> "0" then gosub set_bal
                      if v$ = "T" and td$ <> "99" then tdi_top$(i%) = td$
                      if v$ = "B" and td$ <> "99" then tdi_bot$(i%) = td$

        awdplnwc_done
        return
  
        set_sprial
           if v$ = "B" then goto set_sprial_bot
              str(bal_top$(i%),1%,3%) = str(bal_rec$,12%,3%)
              str(bal_top$(i%),5%,3%) = str(bal_rec$,4%,3%)
              str(bal_top$(i%),4%,1%) = "-"
             return
           set_sprial_bot
              str(bal_bot$(i%),1%,3%) = str(bal_rec$,12%,3%)
              str(bal_bot$(i%),5%,3%) = str(bal_rec$,4%,3%)
              str(bal_bot$(i%),4%,1%) = "-"
        return

        set_bal
         if v$ = "T" then bal_top$(i%) = str(bal_rec$,4%,8%)
         if v$ = "T" then bal2_top$(i%) = str(bal_rec$,20%,8%)

         if v$ = "B" then bal_bot$(i%) = str(bal_rec$,4%,8%)
         if v$ = "B" then bal2_bot$(i%) = str(bal_rec$,20%,8%)
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

           if sc_dept$ = "049" and str(part$,1,3) = "267"    ~
                      then str(readbc$,1,3) = "215"

REM           call "SHOSTAT" (" BC READKEY " & readbc$)  stop 


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
              sash_stop_bot, location = 0.00
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
                           str(inf$(i%),15%,1%) = "*"

              if v$ = "T" and bal_type$ = "2" then            ~
                 convert location to loc_top$(i%), pic(-#####0.00##)
              if v$ = "B"  and bal_type$ = "2" then            ~
                 convert location to loc_bot$(i%), pic(-#####0.00##)

              if v$ = "B"  and bal_type$ = "2" then            ~
                 convert sash_stop_bot to sash_stop_bot$(i%), pic(-#####0.00##)

        return

        set_standard_top
            show% = 0%

            location = ((hgt / 2) + over_limit)
            location = int(location) + 1      /* (AWD001) - Round up top*/
        return

        set_standard_bot
            if dec_height < limit then goto bot_calc  /* (AWD001) */

            location = static_value  /* equal to or greater */
            show% = 1%
                return     
bot_calc
                                       /* (AWD001) - round down */
            location = int(((hgt / 2) + over_limit))
        return

        set_cottage_top
            sav_dec_height = 0.00
            sav_dec_height = dec_height

            location = ((hgt) - (dec_height + over_limit))
            location = int(location) + 1    /* (AWD001) Round Up */
            show% = 1%
       return
 
        set_cottage_bot
            if dec_height >= limit then location = static_value
                                          /* (AWD001) - round down */
            if dec_height <  limit then                   ~
               location = int(((hgt) - (dec_height + over_limit))) 


            sash_stop_bot = 0.00
            sash_stop_bot = ((dec_height - sav_dec_height) + 7.4375)

REM use set sizes of 0-5 use 5; 5.1-10 use 10; 10.1-15 use 15;
REM else exact measurement rounded up

            if sash_stop_bot <= 5 then sash_stop_bot = 5
            if sash_stop_bot > 5 and sash_stop_bot <= 10 then sash_stop_bot = 10
            if sash_stop_bot > 10 and sash_stop_bot <= 15 then sash_stop_bot = 15
            if sash_stop_bot > 15 then sash_stop_bot = int(sash_stop_bot + 1)
 
            show% = 1%
        return

        set_oriel_top
            location = ((hgt) - (dec_height + over_limit))
            location = int(location) + 1   /* (AWD001) round up */
            show% = 1%
        return

        set_oriel_bot
            if dec_height >= limit then location = static_value
                                   /* (AWD001) -- round down */
            if dec_height <  limit then                   ~
                   location = int((dec_height  + over_limit))
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

           if sc_dept$ = "049" and str(part$,1,3) = "267"    ~
                      then str(readbc$,1,3) = "215"


           read #19, key = readbc$, eod goto no_bc_cvr

                get #19, using L53060, value4
L53060:             FMT POS(47), PD(14,4)

                    bal_cover = cvr_height + value4
                    bal_cover% = int(bal_cover)
                    if v$ = "T" then convert bal_cover% to str(cus$(i%),1%,4%), pic(####)
                    if v$ = "B" then convert bal_cover% to str(cus$(i%),6%,4%), pic(####)
        no_bc_cvr
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

        warn_missing_data
           comp% = 2%
           hdr$     = "******* WARNING WARNING WARNING  *******"
           msg$(1%) = "There are >= 3000 records for this Dept/Date."
           msg$(2%) = "Some records may NOT be displayed."
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#7,mode$, 3000%, f2%)
            if f2% <> 0% then goto L64580
            work% = 1%
        return
L64580:     errormsg$ = "ERROR - CANNOT OPEN (APCPLNWK) for " & mode$
            call "SHOSTAT" (errormsg$) : stop
        return clear all
        goto inputmode

        delete_work
            call "FILEBGON" addr(#7)
            work% = 0%
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            if work% <> 0% then gosub delete_work
            end

