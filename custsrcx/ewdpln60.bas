        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDPLN60                             *~
            *  Creation Date     - 08/14/98                             *~
            *  Last Modified Date- 09/02/04                             *~
            *  Written By        - Brian W. Sanders                     *~
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
            *  Subroutines Used  - EWDPLA58 (Display SO Detail Info)    *~
            *                      EWDPLA60 (Tube Winds/Coil Report)    *~
            *                      EWDUSRVL (User ID Validation)        *~
            *                                                           *~
            *  Special Comments  - Periods are used as placeholders in  *~
            *                      some display arrays so that LINSMASH *~
            *                      does not get them out of sequence.   *~
            *                      Any logic changes should also be     *~
            *                      applied to sub EWDPLA60 (Report).    *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 08/14/98 ! (New) Program                            ! BWS *~
            * 09/14/98 ! Add workfile logic to control sort order.! BWS *~
            * 09/29/98 ! Add User ID; new logic for Ctg/Orl-EWD001! BWS *~
            * 10/05/98 ! Logic to scratch wkfile after use.-EWD002! BWS *~
            *          ! Also added PF8 option for Find.          !     *~
            * 11/04/98 ! Mods to logic for Cottage/Oriel.  -EWD003! BWS *~
            *          ! Also mods for changes to EWDPLA58.       !     *~
            * 01/22/99 ! Mods for changes to EWDPLA58.     -EWD004! BWS *~
            * 01/29/99 ! Mods for Tube Length Diff calc.   -EWD005! BWS *~
            * 02/04/99 ! Mods to lookup of table PLAN BALC.-EWD006! BWS *~
            *          ! Also mod to skip " " APCPLNWC values.    !     *~
            * 04/20/99 ! Mods for Laminate Glass.          -EWD007! BWS *~
            * 07/28/99 ! Mods to fix Blink Problem         -EWDRHH! RHH *~
            *          !   tdi$()                                 !     *~
            * 04/24/00 ! (EWD008) Mods to Fix History Report      ! RHH *~
            * 05/16/00 ! (EWD009) Mod to also pass Detail file to ! CMG *~
            *          !     subroutine; so can use Primary       !     *~      
            *          !     History or Detail file.              !     *~
            * 10/12/00 ! (EWD010) Mod to add additional info in   ! CMG *~
            *          !     the 'inf$()' based by dept and height!     *~
            * 02/22/02 ! (EWD011) Mods for the new 411 balance    ! CMG *~
            *          !     tubes with Cottage/Oriel             !     *~
            * 02/22/02 ! (EWD012) Mods for the 421 & 431 to lookup! CMG *~
            *          !     411 data.                            !     *~
            * 03/12/03 ! (EWD013) Mods for new depts 25 and 26    ! CMG *~
            *          !          to be treated same as dept 36.  !     *~
            * 10/03/03 ! (EWD014) Mods for different glass types  ! CMG *~
            *          !          on top and bottom.  Also mod    !     *~
            *          !          to add balance cover length     !     *~
            *          !          in place of customer            !     *~
            * 10/24/03 ! (EWD015) Mods for new depts 27 and 28    ! CMG *~
            *          !          to be treated same as dept 36.  !     *~
            * 03/29/04 ! (EWD016) Mods to calculate all tube height CMG *~
            *          !          before did not calc '99'.       !     *~
            * 09/02/04 ! (AWD017) Mod to take off screen balances ! CMG *~
            *          !   for sclmr.                             !     *~
            *05/19/2014! (CUT001) mod to add dim fields to CUTCC  ! CMG *~
            *************************************************************

        dim                                                              ~
            title$40, option$16,         /* Analysis Title and Time    */~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
            descr$30,                    /* GENCODES Description       */~
            sav_wc_key$15,               /* Use for Loading Table      */~
            wdtkey$5, hgtkey$5,          /* Width & Height for WC Key  */~
            sc_dept$3, sc_dept_d$30,     /* Department Code and Descr  */~
/*EWD001*/  sc_user$3,                   /* User ID for Production     */~
            sc_prddate$10,               /* Production Date            */~
            dt_key$57,                   /* DT Alt key (1)             */~
            wc_key$15,                   /* WC key                     */~
            wk_key$51, wk_rec$256,       /* Work file key, record area */~
            h1$5, h2$3, h3$7, h4$7,      /* Summary Screen Display     */~
            h5$2, h6$1, h7$6, h8$9,      /* Headings                   */~
            h9$2, h10$9, h11$15,         /*                            */~
            part$25,                     /* Mfg. Part No.              */~
            hinge$2,                     /* Hinge Code from Part No.   */~
            sngl$1,                      /* Single-Hung Window Flag    */~
            windcl$(6)6,                 /* Winding/Coil Data(APCPLNWC)*/~
            tubedi$(6)2,                 /* Tube Diam. Codes (APCPLNWC)*/~
/*EWD001*/  co_flag$2,                   /* Flag for Cottage/Oriel     */~
/*EWD011*/  co_flg1$2,                   /* Second Flag for CO/OR      */~
/*EWD002*/  find_seq$5,                  /* Seq. No. to Find via PF8   */~
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
            userid$3,                    /* Current User Id            */~
            clmr$                        /* SCLMR              (AWD017)*/

        dim cc$(3000)1,                  /* Tab Stop/Mark for Dsp/Rmvl */~
            seqn$(3000)5,                /* Production Sequence No.    */~
            mdl$(3000)3,                 /* Model No.                  */~
            sav_mdl$3,                   /* Save Model No.     (EWD012)*/~
            wdt$(3000)7,                 /* Width                      */~
            hgt$(3000)7,                 /* Height                     */~
            gls$(3000)2,                 /* Glass No.                  */~
            scr$(3000)1,                 /* Screen No.                 */~
            wcl$(3000)6,                 /* Winds/Coil                 */~
            tht$(3000)9,                 /* Tube Heights               */~
            tdi$(3000)2,                 /* Tube Diameter Code         */~
            cus$(3000)9,                 /* Customer Code              */~
            inf$(3000)15,                /* Model/Customer Info        */~
            dsp$(3000)10,                /* SO Data to Pass to Dtl Sub */~
/*EWD010*/  height$10                    /* Calculated Height          */

        dim                              /* (EWD014)                   */~
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
            pname$ = "EWDPLN60 - Rev: R7.00"

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
            * #5  ! APCPLNWC ! Production Windings & Coil File          *~
            * #6  ! APCPLNSC ! Planning Master Schedule File            *~
            * #7  ! APCPLNWK ! Work File                                *~
            * #8  ! TXTFILE  ! Sales Master Text File                   *~
            * #9  ! AMTBOMIF ! Master VALIDITY FILE                     *~
            * #10 ! APCPLNOR ! Planning Header Histroy                  *~
/*EWD003*/  * #11 ! BCKLINES ! Back Log Line Item File                  *~
/*EWD004*/  * #12 ! EWDPLNRK ! Glass Master Rack File                   *~
            * #15 ! APCPLNDP ! Planning Master Dept File       (EWD008) *~
/*EWD014*/  * #16 ! AMTBOMCD ! Master Equation File                     *~
/*EWD014*/  * #17 ! APCCUTEQ ! Saw Optimization Cross-Reference File    *~
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

/*EWD003*/  select #11, "BCKLINES",                                      ~
/*EWD003*/              varc,     indexed,  recsize = 300,               ~
/*EWD003*/              keypos =  10,  keylen = 19

/*EWD004*/  select #12, "EWDPLNRK",                                      ~
/*EWD004*/              varc,     indexed,  recsize =   64,              ~
/*EWD004*/              keypos =    1, keylen =    9,                    ~
                        alt key  1, keypos =   10, keylen =  14          ~
                                                        /* (EWD008)    */
            select #15, "APCPLNDP",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 11,    keylen = 12,                     ~
                        alt key 1, keypos =  9, keylen = 14,             ~
                            key 2, keypos =  4, keylen = 12,             ~
                            key 3, keypos =  1, keylen = 15

/*EWD014*/  select #16,  "AMTBOMCD",                                     ~
                        varc,     indexed,  recsize = 250,               ~
                        keypos = 1,    keylen = 42

/*EWD014*/  select #17,  "APCCUTEQ",                                     ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =    2, keylen =   7,                     ~
                        alt key  1, keypos  =     1, keylen =  8

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
/*EWD003*/  call "OPENCHCK" (#11, fs%(11%), f2%(11%), 0%, rslt$(11%))
/*EWD004*/  call "OPENCHCK" (#12, fs%(12%), f2%(12%), 0%, rslt$(12%))
            call "OPENCHCK" (#15, fs%(15%), f2%(15%), 0%, rslt$(15%))
/*EWD014*/  call "OPENCHCK" (#16, fs%(16%), f2%(16%), 0%, rslt$(16%))
/*EWD014*/  call "OPENCHCK" (#17, fs%(17%), f2%(17%), 0%, rslt$(17%))

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

            for fieldnr% = 1% to   2%       /* Change to 3 for EWD001 */
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
L11120:     fieldnr% = cursor%(1%) - 2%             /*Change to 3 for*/
            if fieldnr% < 1% or fieldnr% > 2% then editpg1 /*<-EWD001*/
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
            if work% <> 0% then gosub delete_work           /*EWD002*/
            k% = 0%
            call "DATFMTC" (sc_prddate$)
            gosub display_summary
            return

        load_data
            read #3, key 1 > dt_key$, using L20210, wk_rec$,             ~
                    eod goto read_work_file
L20210:         fmt ch(256)
            dt_key$ = str(wk_rec$,47,57)
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
L20400:     gosub clear_line
            read #7, key > wk_key$, using L20410, wk_key$, dsp$(i%),     ~
                cus$(i%), part$, eod goto L20998
L20410:             fmt ch(51), ch(10), ch(9), ch(25)
            seqn$(i%) = str(wk_key$,10%,5%)
            mdl$(i%) = str(part$,,3)
            gls$(i%) = str(part$,5,2)
            hinge$   = str(part$,9,2)
            scr$(i%) = str(part$,11,1)
            wdt$(i%) = str(part$,13,4)
            hgt$(i%) = str(part$,17,3)
            sav_mdl$ = mdl$(i%)                               /* (EWD012) */
            cus$(i%) = " "
            convert wdt$(i%) to wdt, data goto read_work_file

            convert wdt$(i%) to sav_wdt, data goto read_work_file /* (EWD012) */
            gosub lookup_model                                    /* (EWD012) */
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
            gosub find_lkup_diam
            co_flg1$ = " "                                   /* EWD011 */
REM            if co_flag$ = "CO" and mdl$(i%) = "411" then co_flg1$ = "C1"
REM            if co_flag$ = "OR" and mdl$(i%) = "411" then co_flg1$ = "O1"
            for x% = 1% to 2%                                /* EWD011 */
            sav_wc_key$ = all(hex(00)) 
             if x% = 1% then                                                  ~
                 str(sav_wc_key$,,10)=str(mdl$(i%)) & str(tdi$(i%)) & wdtkey$ ~
              else                                                            ~
                 str(sav_wc_key$,,10)=str(mdl$(i%)) & co_flg1$ & wdtkey$
                                                             /* EWD011 */

            read #5, key > sav_wc_key$, using L20500, wc_key$,           ~
                    eod goto L20570
L20500:         fmt ch(15)                  /* 1st Pass Finds Width... */
            if str(wc_key$,,5) <> str(sav_wc_key$,,5) then goto L20570
            sav_wc_key$, wc_key$ = str(wc_key$,,10) & hgtkey$ /*EWD006*/
next_hgt:   read #5, key >= wc_key$, using L20540, wc_key$,   /*EWD006*/ ~
                    windcl$(), tubedi$(), eod goto read_work_file       
L20540:         fmt ch(15), 6*ch(6), 6*ch(2)/* 2nd Pass Finds Height.. */
            if str(wc_key$,,10) <> str(sav_wc_key$,,10)                  ~
                 then goto read_work_file   /* Height Undefined...     */
L20570:     gosub find_gls_type
            if windcl$(j%) <> " " then L20580                /* EWD006 */
                wc_key$ = str(wc_key$,,14) & bin(val(str(wc_key$,15,1))+1)
                goto next_hgt                                /* EWD006 */
L20580:     if x% <> 2% then wcl$(i%) = windcl$(j%)          /* EWD011 */ ~
            else str(wcl$(i%),4%,6%) = windcl$(j%) 

            gosub check_top_bottom                           /* EWD014 */
            if co_flg1$ = " " then x% = 2%                   /* EWD011 */
            next x%                                          /* EWD011 */
            tdi$(i%) = tubedi$(j%)
            if wcl$(i%) = " " then wcl$(i%) = "."
            if tdi$(i%) = " " then tdi$(i%) = "99"  /* 99 = Coil       */
            call "STRING" addr("RJ", wcl$(i%), 6%)  /* Both steps reqd.*/
            call "STRING" addr("CT", wcl$(i%), 6%)  /* to center data. */ 
                                                    /*  (EWD016)       */
REM            if tdi$(i%) <> "99" then gosub calc_tube_hgts
            gosub calc_tube_hgts                    /*  (EWD016)       */
/*EWD003*/  if tht$(i%) = "." and co_flag$ <> " " then tht$(i%) = co_flag$
            if inf$(i%) <= "." then gosub load_color
            mdl$(i%) = sav_mdl$                   /* (EWD012) - BEG */
            wdt = sav_wdt
            wdt = wdt / 10
            convert wdt to wdtkey$, pic(000.0)
            fctnl = wdt - int(wdt)          /* Convert to decimal      */
            fctnl = (fctnl / 8) * 10
            wdt = int(wdt) + fctnl
            convert wdt to wdt$(i%), pic(##0.000)     /* (EWD012) - END */
            gosub calc_balance_cover                  /* (EWD014)       */
            gosub load_screen                     /* (EWD010) */
            dept_max% = i%
            i% = i% + 1%
            if i% < 3001% then goto read_work_file
            gosub warn_missing_data
           
L20998:     if i% <= 3000% then gosub clear_line
            if dept_max% = 0% then goto L20999
            for x% = dept_max% + 1% to 3000%
                wcl$(x%), tht$(x%), cus$(x%), inf$(x%) = " "
            next x%
            return

L20999:     errormsg$ = "No data to display for this date/department."
            gosub error_prompt
            return clear all
            goto inputmode



        find_lkup_diam
/*EWD003*/  gosub check_ctg_orl
                                               /*  (AWD017)  - BEG */
            if co_flag$ = " " then goto L21010
            init(" ") clmr$ 
            clmr$ = str(part$,20%,3%)
            if str(clmr$,1%,1%) > "9" then goto L21015   /* Cannot be mulled */
            if clmr$ <> "   " and clmr$ <> "000" then co_flag$ = "SP" 
                                               /*  (AWD017)  - END */
L21015:
/*  |   */  if co_flag$ = " " then goto L21010
/*  |   */      tdi$(i%) = co_flag$
/*EWD003*/      return

L21010:     read #2, key=cus$(i%), using L21020, billto$, eod goto L21160
L21020:         fmt pos(780), ch(9)
            readkey$ = "PLAN TDLU" & str(mdl$(i%)) & " " & billto$
            gosub read_gencds1
            if found% = 1% then return

L21160:     readkey$ = "PLAN TDLU" & mdl$(i%)
            gosub read_gencds1
            if found% = 1% then return

            tdi$(i%) = "00"         /* Default to Standard */
            return


            read_gencds1
               found% = 0%
               read #4, key = readkey$, using L21270, tdi$(i%), inf$(i%),~
                        eod goto L21290
L21270:             fmt pos(25), ch(2), xx(3), ch(15)
               if inf$(i%) = " " then inf$(i%) = "."
               found% = 1%
L21290:        return



        find_gls_type
            j% =1%
            
            readkey$ = "PLAN DBLE" & gls$(i%)
            gosub read_gencds2
            if found% = 0% then goto L22070
                j% = 2%
                return

L22070:     readkey$ = "PLAN LAMN" & gls$(i%)   /*EWD007*/
            gosub read_gencds2
            if found% = 0% then goto L22110
                j% = 3%
                return                

L22110:     readkey$ = "PLAN xxxx" & gls$(i%)
*           gosub read_gencds2
*           if found% = 0% then goto L22150
*               j% = 4%
*               return

*L22150:    readkey$ = "PLAN xxxx" & gls$(i%)
*           gosub read_gencds2
*           if found% = 0% then goto L22190
*               j% = 5%
*               return

/*L22190:*/ return                              /*EWD007*/


            read_gencds2
               found% = 0%
               read #4, key = readkey$, eod goto L22290                    
               found% = 1%
L22290:        return



        calc_tube_hgts
            ht1%, ht2% = 999%  :  factor = 0
            init(" ") readkey$, descr$                      
            str(readkey$,1%,9%)   = "PLAN BALC"
            str(readkey$,10%,3%) = mdl$(i%)                   /*EWD006*/
            for x% = 1% to 2%                                 /* Begin*/
              if x% = 2% then str(readkey$,13%,2%) = tdi$(i%)            ~
                 else str(readkey$,13%,2%) = " "                        
              read #4,key=readkey$, using L22500, descr$, eod goto L22510
L22500:          FMT POS(25), CH(30)
L22510:     next x%                                           /*EWD006*/
            if descr$ = " " then goto L22580                  /* End  */
            convert str(descr$,,7) to factor, data goto L22580
            sngl$ = str(descr$,10,1)
            ht2% = int((hgt/2) - factor)
            factor% = 2%                    /*  EWD005 - Begin  */
            init(" ") readkey$, descr$                      
            str(readkey$,1%,9%)   = "PLAN TLDF"
            str(readkey$,10%,15%) = mdl$(i%) & tdi$(i%)
            read #4,key = readkey$, using L22500, descr$, eod goto L22540
            convert str(descr$,,3) to factor%, data goto L22540
L22540:     ht1% = ht2% - factor%           /*   EWD005 - End   */  
*EWD003         --- Previous location of code in check_ctg_orl ---
            if co_flag$ = "CO" then ht1% = ht1% * 0.8/*Cottage*/ /*EWD*/
                                                            /* EWDRHH */
            if co_flag$ = "OR" then ht2% = ht2% * 1.2/* Oriel */ /*001*/
            if ht1% < minth% then ht1% = minth%
            if ht1% > maxth% then ht1% = maxth%
            if ht2% < minth% then ht2% = minth%
            if ht2% > maxth% then ht2% = maxth%
            tht$(i%) = "    -xxx "
            if co_flag$ <> " "                                /*EWD001*/ ~
                then str(tht$(i%),5,1) = "/"   /* Denotes diff in calc */
L22580:     if sngl$ <> "S" or co_flag$ <> " "                /*EWD001*/ ~
                then convert ht1% to str(tht$(i%),,3), pic(##0)
            convert ht2% to str(tht$(i%),6,3), pic(##0)
            return


        load_color                                    /* (EWDRHH)  */
            readkey$ = "COLOR    " & str(part$,4,1)   /* Only Need */
                                                      /* 1st 8 Char*/   
            read #4, key = readkey$, using L22600, inf$(i%),             ~
                     eod goto L22610
L22600:          fmt pos(30), ch(15)
L22610:     return

        load_screen                       /* (EWD010) Begin */
            p% = 0%
            p% = len(inf$(i%)) + 1%
            q% = (15% - p%)
            
            if str(wk_key$,7%,3%) = "002" or str(wk_key$,7%,3%) = "007"     ~
                                                       then gosub check_tso
            if str(wk_key$,7%,3%) = "049" or str(wk_key$,7%,3%) = "052"     ~
                                                      then gosub check_tso

            if str(wk_key$,7%,3%) = "002" or str(wk_key$,7%,3%) = "007"     ~
               or str(wk_key$,7%,3%) = "036"         then gosub check_bso

/*(EWD013)*/
            if str(wk_key$,7%,3%) = "025" or str(wk_key$,7%,3%) = "026"     ~
                                                      then gosub check_bso

/*(EWD013)*/

/*(EWD015)*/
            if str(wk_key$,7%,3%) = "027" or str(wk_key$,7%,3%) = "028"     ~
                                                      then gosub check_bso

/*(EWD015)*/
            if str(wk_key$,7%,3%) = "049" or str(wk_key$,7%,3%) = "052"     ~
                                                      then gosub check_bso

            if str(wk_key$,7%,3%) = "036"             then gosub check_fgo

/*(EWD013)*/
            if str(wk_key$,7%,3%) = "025" or str(wk_key$,7%,3%) = "026"     ~
                                                      then gosub check_fgo

/*(EWD013)*/

/*(EWD015)*/
            if str(wk_key$,7%,3%) = "027" or str(wk_key$,7%,3%) = "028"     ~
                                                      then gosub check_fgo

/*(EWD015)*/


            if str(wk_key$,7%,3%) = "006"             then gosub check_shims
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

/*EWD013*/
           if str(wk_key$,7%,3%) = "025" or str(wk_key$,7%,3%) = "026"   ~
                                                         then goto L22630 

/*EWD015*/
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
           if tdi$(i%) <> "99" or hgt < 67 then return

           height = round((hgt - 67) / 2, 4)

           convert height to height$, pic(####.####-)

           call "SPCSMASH" (height$)
           
           str(inf$(i%), p%,q%) = " / " & height$
        return                                          /* (EWD010) End  */

        check_shims
           init(" ") height$
           if hgt < 60 then return

           str(inf$(i%), p%,q%) = " / SHIMS"               
        return

        
        check_ctg_orl   /* EWD003 - Was part of calc_tube_hgts;    */
                        /*          Now called from find_lkup_diam */
            init(" ") readkey$, descr$, co_flag$
            str(readkey$,1%,9%)   = "HINGE    "
            str(readkey$,10%,15%) = hinge$
            read #4,key = readkey$, using L22700, descr$, eod goto L22710
L22700:        FMT POS(25), CH(30)                          /* EWD003^*/
            co_flag$ = str(descr$,1%,2%)                    /* EWD001 */
            if co_flag$ <> "CO" and co_flag$ <> "OR" then co_flag$ = " "
L22710:     return    /* <- EWD003 */       /* ^ EWD001 ^ */


        clear_line
            seqn$(i%), mdl$(i%), wdt$(i%), hgt$(i%), gls$(i%), scr$(i%), ~
                tdi$(i%), dsp$(i%), windcl$(), tubedi$(), co_flag$ = " "
            wcl$(i%), tht$(i%), cus$(i%), inf$(i%) = "."  /*^EWD001*/
            sav_mdl$ = " "                                /*EWD012*/
            return
        
        lookup_model                                      /* (EWD012) */
            init(" ") readkey$, descr$          
            div_fact% = 0%
            fact = 0.00
            str(readkey$,1%,9%)   = "PLAN TMDL"
            str(readkey$,10%,15%) = mdl$(i%)
            read #4,key = readkey$, using L22700, desc$, eod goto no_model

            mdl$(i%) = str(desc$,1%,3%)                    
            convert str(desc$,12%,1%) to div_fact%, data goto no_model

            convert str(desc$,22%,5%) to fact, data goto no_model

            wdt = wdt / div_fact%
            wdt = wdt - fact

        no_model
        return                                            /* (EWD012) */

        check_top_bottom                                  /* (EWD014) */
             readkey$ = "TEMP GED " & gls$(i%)
             gosub read_gencds2
             if found% = 0% then return
             wcl$(i%) = "TOP/BOT"
        return                                            /* (EWD014) */

        calc_balance_cover                                /* (EWD014) */
           pd$ = str(mdl$(i%),1%,1%)
           tw$ = "1" : th$ = "2"
           call "APCCUTLD" (pd$, cw%, ch%, tw$, th$, #4, err% )
           if err% = 0% then goto L04510
              errormsg$ =                                                ~
                  "(Error) - Saw Cuts not Defined in (EQUATIONS) Table."
              return
L04510:    sa_mfg% = 0%                            /* MFG WINDOWS ONLY */
           call "SHOSTAT" ("Calculating Saw Cuts for Part")
/* (CUT001) */
           
           call "APCCUTCC" ( part$, 0%, 0%, 0%, sa_mfg%, cw%, ch%, eq$(), ~
               ct$(), cr$(), cp$(), cc_ans$(), co$(), ct(), sh$(), tw$,   ~
               th$, #17, #16, #4, err%)

           gosub get_equation
           if err% <> 0% then return
              errormsg$ =                                                ~
                  "(Error) - No Cut References Defined for Model/Color."
        return


        get_equation
            str(readkey$,1%,9%)   = "PLAN TEQU"
            str(readkey$,10%,15%) = mdl$(i%)
            read #4,key = readkey$, using L22500, descr$, eod goto no_equ
            convert str(descr$,,2) to equ%, data goto no_equ
            cus$(i%) = ct$(equ% + cw%)
        no_equ
              errormsg$ =                                                ~
                  "(Error) - No Equation Numbers Defined."
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
                      hgt$(), gls$(), scr$(), tdi$(), dsp$(), tubedi$()

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
/*EWD001*/                      L40160           /* sc_user$           */

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
/*EWD001*/     at (05,02), "Production User ID:",                        ~
/*EWD001*/     at (05,25), fac(lfac$(3%)), sc_user$             , ch(03),~
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
                                                           /* (EWD008) */
                  call "EWDPLA60" (#2, #3, #6, #4, #5, #15, minth%, maxth%)
                  goto L40190                  /* (EWD009) - Added #3  */

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
L40570:     if fieldnr% > 1% then L40580                    /*EWD001*/
                str(pf$(1),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40580:     if fieldnr% = 2% then L40590                    /*EWD001*/
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
               at (03,30), fac(hex(a4))  , h5$                  , ch(02),~
               at (03,33), fac(hex(a4))  , h6$                  , ch(01),~     
               at (03,35), fac(hex(a4))  , h7$                  , ch(06),~
               at (03,42), fac(hex(a4))  , h8$                  , ch(09),~
               at (03,52), fac(hex(a4))  , h9$                  , ch(02),~
               at (03,55), fac(hex(a4))  , h10$                 , ch(09),~
               at (03,65), fac(hex(a4))  , h11$                 , ch(15),~
                                                                         ~
               at (04,02), fac(hex(81))  , cc$(k% + 1%)         , ch(01),~
               at (04,04), fac(hex(84))  , seqn$(k% + 1%)       , ch(05),~
               at (04,10), fac(hex(84))  , mdl$(k% + 1%)        , ch(03),~
               at (04,14), fac(hex(84))  , wdt$(k% + 1%)        , ch(07),~
               at (04,22), fac(hex(84))  , hgt$(k% + 1%)        , ch(07),~
               at (04,30), fac(hex(84))  , gls$(k% + 1%)        , ch(02),~
               at (04,33), fac(hex(84))  , scr$(k% + 1%)        , ch(01),~ 
               at (04,35), fac(hex(84))  , wcl$(k% + 1%)        , ch(06),~ 
               at (04,42), fac(hex(84))  , tht$(k% + 1%)        , ch(09),~ 
               at (04,52), fac(lfac$(1)) , tdi$(k% + 1%)        , ch(02),~ 
               at (04,55), fac(hex(84))  , cus$(k% + 1%)        , ch(09),~ 
               at (04,65), fac(hex(84))  , inf$(k% + 1%)        , ch(15),~ 
                                                                         ~
               at (06,02), fac(hex(81))  , cc$(k% + 2%)         , ch(01),~
               at (06,04), fac(hex(84))  , seqn$(k% + 2%)       , ch(05),~
               at (06,10), fac(hex(84))  , mdl$(k% + 2%)        , ch(03),~
               at (06,14), fac(hex(84))  , wdt$(k% + 2%)        , ch(07),~
               at (06,22), fac(hex(84))  , hgt$(k% + 2%)        , ch(07),~
               at (06,30), fac(hex(84))  , gls$(k% + 2%)        , ch(02),~
               at (06,33), fac(hex(84))  , scr$(k% + 2%)        , ch(01),~ 
               at (06,35), fac(hex(84))  , wcl$(k% + 2%)        , ch(06),~ 
               at (06,42), fac(hex(84))  , tht$(k% + 2%)        , ch(09),~ 
               at (06,52), fac(lfac$(2)) , tdi$(k% + 2%)        , ch(02),~ 
               at (06,55), fac(hex(84))  , cus$(k% + 2%)        , ch(09),~ 
               at (06,65), fac(hex(84))  , inf$(k% + 2%)        , ch(15),~ 
                                                                         ~
               at (08,02), fac(hex(81))  , cc$(k% + 3%)         , ch(01),~
               at (08,04), fac(hex(84))  , seqn$(k% + 3%)       , ch(05),~
               at (08,10), fac(hex(84))  , mdl$(k% + 3%)        , ch(03),~
               at (08,14), fac(hex(84))  , wdt$(k% + 3%)        , ch(07),~
               at (08,22), fac(hex(84))  , hgt$(k% + 3%)        , ch(07),~
               at (08,30), fac(hex(84))  , gls$(k% + 3%)        , ch(02),~
               at (08,33), fac(hex(84))  , scr$(k% + 3%)        , ch(01),~ 
               at (08,35), fac(hex(84))  , wcl$(k% + 3%)        , ch(06),~ 
               at (08,42), fac(hex(84))  , tht$(k% + 3%)        , ch(09),~ 
               at (08,52), fac(lfac$(3)) , tdi$(k% + 3%)        , ch(02),~ 
               at (08,55), fac(hex(84))  , cus$(k% + 3%)        , ch(09),~ 
               at (08,65), fac(hex(84))  , inf$(k% + 3%)        , ch(15),~ 
                                                                         ~
               at (10,02), fac(hex(81))  , cc$(k% + 4%)         , ch(01),~
               at (10,04), fac(hex(84))  , seqn$(k% + 4%)       , ch(05),~
               at (10,10), fac(hex(84))  , mdl$(k% + 4%)        , ch(03),~
               at (10,14), fac(hex(84))  , wdt$(k% + 4%)        , ch(07),~
               at (10,22), fac(hex(84))  , hgt$(k% + 4%)        , ch(07),~
               at (10,30), fac(hex(84))  , gls$(k% + 4%)        , ch(02),~
               at (10,33), fac(hex(84))  , scr$(k% + 4%)        , ch(01),~ 
               at (10,35), fac(hex(84))  , wcl$(k% + 4%)        , ch(06),~ 
               at (10,42), fac(hex(84))  , tht$(k% + 4%)        , ch(09),~ 
               at (10,52), fac(lfac$(4)) , tdi$(k% + 4%)        , ch(02),~ 
               at (10,55), fac(hex(84))  , cus$(k% + 4%)        , ch(09),~ 
               at (10,65), fac(hex(84))  , inf$(k% + 4%)        , ch(15),~ 
                                                                         ~
               at (12,02), fac(hex(81))  , cc$(k% + 5%)         , ch(01),~
               at (12,04), fac(hex(84))  , seqn$(k% + 5%)       , ch(05),~
               at (12,10), fac(hex(84))  , mdl$(k% + 5%)        , ch(03),~
               at (12,14), fac(hex(84))  , wdt$(k% + 5%)        , ch(07),~
               at (12,22), fac(hex(84))  , hgt$(k% + 5%)        , ch(07),~
               at (12,30), fac(hex(84))  , gls$(k% + 5%)        , ch(02),~
               at (12,33), fac(hex(84))  , scr$(k% + 5%)        , ch(01),~ 
               at (12,35), fac(hex(84))  , wcl$(k% + 5%)        , ch(06),~ 
               at (12,42), fac(hex(84))  , tht$(k% + 5%)        , ch(09),~ 
               at (12,52), fac(lfac$(5)) , tdi$(k% + 5%)        , ch(02),~ 
               at (12,55), fac(hex(84))  , cus$(k% + 5%)        , ch(09),~ 
               at (12,65), fac(hex(84))  , inf$(k% + 5%)        , ch(15),~ 
                                                                         ~
               at (14,02), fac(hex(81))  , cc$(k% + 6%)         , ch(01),~
               at (14,04), fac(hex(84))  , seqn$(k% + 6%)       , ch(05),~
               at (14,10), fac(hex(84))  , mdl$(k% + 6%)        , ch(03),~
               at (14,14), fac(hex(84))  , wdt$(k% + 6%)        , ch(07),~
               at (14,22), fac(hex(84))  , hgt$(k% + 6%)        , ch(07),~
               at (14,30), fac(hex(84))  , gls$(k% + 6%)        , ch(02),~
               at (14,33), fac(hex(84))  , scr$(k% + 6%)        , ch(01),~ 
               at (14,35), fac(hex(84))  , wcl$(k% + 6%)        , ch(06),~ 
               at (14,42), fac(hex(84))  , tht$(k% + 6%)        , ch(09),~ 
               at (14,52), fac(lfac$(6)) , tdi$(k% + 6%)        , ch(02),~ 
               at (14,55), fac(hex(84))  , cus$(k% + 6%)        , ch(09),~ 
               at (14,65), fac(hex(84))  , inf$(k% + 6%)        , ch(15),~ 
                                                                         ~
               at (16,02), fac(hex(81))  , cc$(k% + 7%)         , ch(01),~
               at (16,04), fac(hex(84))  , seqn$(k% + 7%)       , ch(05),~
               at (16,10), fac(hex(84))  , mdl$(k% + 7%)        , ch(03),~
               at (16,14), fac(hex(84))  , wdt$(k% + 7%)        , ch(07),~
               at (16,22), fac(hex(84))  , hgt$(k% + 7%)        , ch(07),~
               at (16,30), fac(hex(84))  , gls$(k% + 7%)        , ch(02),~
               at (16,33), fac(hex(84))  , scr$(k% + 7%)        , ch(01),~ 
               at (16,35), fac(hex(84))  , wcl$(k% + 7%)        , ch(06),~ 
               at (16,42), fac(hex(84))  , tht$(k% + 7%)        , ch(09),~ 
               at (16,52), fac(lfac$(7)) , tdi$(k% + 7%)        , ch(02),~ 
               at (16,55), fac(hex(84))  , cus$(k% + 7%)        , ch(09),~ 
               at (16,65), fac(hex(84))  , inf$(k% + 7%)        , ch(15),~ 
                                                                         ~
               at (18,02), fac(hex(81))  , cc$(k% + 8%)         , ch(01),~
/*EWD002*/     at (18,04), fac(hex(a4))  , seqn$(k% + 8%)       , ch(05),~
/* Begin*/     at (18,10), fac(hex(a4))  , mdl$(k% + 8%)        , ch(03),~
               at (18,14), fac(hex(a4))  , wdt$(k% + 8%)        , ch(07),~
               at (18,22), fac(hex(a4))  , hgt$(k% + 8%)        , ch(07),~
               at (18,30), fac(hex(a4))  , gls$(k% + 8%)        , ch(02),~
               at (18,33), fac(hex(a4))  , scr$(k% + 8%)        , ch(01),~ 
               at (18,35), fac(hex(a4))  , wcl$(k% + 8%)        , ch(06),~ 
               at (18,42), fac(hex(a4))  , tht$(k% + 8%)        , ch(09),~ 
/*No Mod*/     at (18,52), fac(lfac$(8)) , tdi$(k% + 8%)        , ch(02),~ 
               at (18,55), fac(hex(a4))  , cus$(k% + 8%)        , ch(09),~ 
               at (18,65), fac(hex(a4))  , inf$(k% + 8%)        , ch(15),~ 
                                                                         ~
/*EWD002*/     at (19,10), "Production User ID = ",                      ~
/* End  */     at (19,31), fac(hex(8c))  , sc_user$             , ch(03),~
/*EWD001*/     at (19,41), fac(hex(8c))  , dsp_text$            , ch(11),~
                                                                         ~
               at (21,02), fac(hex(a4)),   dsp_msg$             , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
/*EWD002*/     at (24,02), fac(hex(8c)),   str(pf$(3%),,52%)    , ch(52),~
/*EWD002*/     at (24,55), fac(hex(a2)),   find_seq$            , ch(05),~
/*EWD002*/     at (24,61), fac(hex(8c)),   str(pf$(3%),60%,20%) , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

                if keyhit% <> 0% then goto L41010
                    flg% = 0%
                    for x% = 1% to dept_max%
                        if cc$(x%) = " " then goto L41005
                        if cc$(x%) <> "X" then goto L41003
                          seqn$(x%), mdl$(x%), wdt$(x%), hgt$(x%),       ~
                              gls$(x%), scr$(x%), wcl$(x%), tht$(x%),    ~
                              tdi$(x%), cus$(x%), inf$(x%), dsp$(x%) =" "
                          dept_max% = dept_max% - 1%
                          flg% = 1%
L41003:                 if cc$(x%) <> "D" then goto L41005
                          call "EWDPLA58" (0%, str(dsp$(x%),1,8),        ~
/*EWD004*/                  str(dsp$(x%),9,2), " ", " ",                 ~
/*EWD003*/ /*EWD004*/       #10, #6, #2, #4, #9, #8, #1, #11, #12, 0%)
L41005:             next x%
                    if flg% = 0% then goto L41007   /* Nothing marked */
                    call "LINSMASH" (seqn$())       /* for removal.   */
                    call "LINSMASH" (mdl$())
                    call "LINSMASH" (wdt$())                  
                    call "LINSMASH" (hgt$())
                    call "LINSMASH" (gls$())
                    call "LINSMASH" (scr$())
                    call "LINSMASH" (wcl$())
                    call "LINSMASH" (tht$())
                    call "LINSMASH" (tdi$())            
                    call "LINSMASH" (cus$())
                    call "LINSMASH" (inf$())
                    call "LINSMASH" (dsp$())
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
                  if find_seq$ = " " then goto L41000      /*New-EWD002*/
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
/*EWD002*/  if sc_user$ = " " then sc_user$ = userid$
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
            h5$ = "Gl"
            h6$ = "S"
            h7$ = "Wnd/Cl"
            h8$ = "Tube Hgts"
            h9$ = "TD"
            h10$ = "Bal Covr"
            h11$ = "Model/Cust.Info" 

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
/*EWD002*/           "(8)Find Seq: xxxxx     (16)Exit Display" 
/*EWD002*/  pfkeys$ = hex(ff02030405060708ffffffffffff0f1000)
            gosub check_screen
            dsp_text$ = " Upr - Lwr "                          /*EWD001*/
                                                  /* (EWDRHH) Fix Prob */
            for x% = 1% to 8%
                lfac$(x%) = hex(84)
/*EWD003*/      readkey$ = "PLAN TDHL" & "D" & sc_dept$ & tdi$(k% + x%)
/* Begin*/          gosub read_gencds2
                    if found% <> 0% then lfac$(x%) = hex(94)

                    if found% <> 0% then str(inf$(k% + x%),9%,7%) = "<------"
                readkey$ = "PLAN TDHL" &"M"& mdl$(k% + x%) & tdi$(k% + x%)
/*EWD003*/          gosub read_gencds2
                                                                
/* End  */          if found% <> 0% then lfac$(x%) = hex(94)
                    if found% <> 0% then str(inf$(k% + x%),9%,7%) = "<------"
/*EWD001*/      if str(tht$(k%+x%),5,1)="/" then dsp_text$="/ = Ctg/Orl"
            next x%
                                                  /* (EWDRHH) Fix Prob */
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
/*EWD001*/                    L50100         /* Production User ID     */


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

L50100: Rem Enter a User ID for Production          sc_user$ /*EWD001*/
            call "EWDUSRVL" (sc_user$, flag$)                /* -New-*/
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
