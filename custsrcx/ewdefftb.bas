        REM *************************************************************~
            *                                                           *~
            *  EEEEE  W   W  DDDD   EEEEE  FFFFF  FFFFF  TTTTT  BBBB    *~
            *  E      W   W  D   D  E      F      F        T    B   B   *~
            *  EEEE   W W W  D   D  EEEE   FFFF   FFFF     T    BBBB    *~
            *  E      WW WW  D   D  E      F      F        T    B   B   *~
            *  EEEEE  WW WW  DDDD   EEEEE  F      F        T    BBBB    *~
            *                                                           *~ 
            *-----------------------------------------------------------*~
            * EWDEFFTB - Production Efficiency Reporting and Entry.     *~
            *                                                           *~
            *   NOTE - (1) - The Values for Indirect are in the Array   *~
            *                INDT(). Values Set at (9420)               *~
            *          (2) - Must ALWAYS complete entire week process   *~
            *                before recalc of wages.                    *~
            *                REM UNIT VALUES                            *~
            *          (3) - Indirect Labor Standard Unit Per Manhour   *~
            *                Goals. (Wired in INDT() ) line 9500        *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/17/00 ! New Program for (APC) - Last Mod Date    ! CMG *~
            * 05/30/00 ! Change Total Scrap % to use whole units, ! CMG *~
            *          !    not effective units (EWD0001)         !     *~
            * 01/01/02 ! Mod to add pull open frame and build open! CMG *~
            *          !    frame stock for dept 043 (EWD0002)    !     *~
            * 08/23/02 ! Mod to double and triple scanned units   ! CMG *~
            *          !    for 400 series cont. head and sill.   !     *~
            *          !    Models 421, 431, 441, 451(EWD0003)    !     *~
            * 10/30/02 ! Mod to double and triple scanned units   ! CMG *~
            *          !    for 500 series cont. head and sill.   !     *~
            *          !    Models 450, 460, 470, 480(EWD0004)    !     *~
            * 01/21/03 ! Mod to double and triple scanned units   ! CMG *~
            *          !    for 200 series cont. head and sill.   !     *~
            *          !    Models 772, 773, 774, 775(EWD0005)    !     *~
            * 09/19/03 ! Mod to add new indirect dept (EWD0006)   ! CMG *~
            * 07/21/04 ! (EWD0007) Mod to make dept '054' calc unt! CMG *~
            *          !            just like '044'               !     *~
            * 01/04/05 ! (AWD0008) Changes to column 5 and scrap  ! CMG *~
            *          !          column 10 per Libby Shadix      !     *~
            * 01/26/05 ! (AWD0009) Product Unit Value             ! CMG *~
            * 02/03/05 ! (AWD0010) Mod not to lose scrap values   ! CMG *~ 
            *          !  in lbs() scrap goal.                    !     *~
            * 01/01/06 ! (PAR000) CR347 Mod for sub part          ! CMG *~
            * 01/09/08 ! (AWD011) mods for new dept 074           ! CMG *~
            * 09/11/08 ! (AWD012) mod for new glass dept 041      ! CMG *~
            * 12/09/08 ! (AWD013) Mods to make 038 dept like 037  ! CMG *~
            *          !  & 041 departments                       !     *~
            * 02/20/09 ! (AWD014) mods for ASM Screen dept 029    ! CMG *~
            *02/19/2010! (AWD015) remove samples from dept 021    ! DES *~
            *11/12/2012! (AWD016) mod for weeksemp Sunday to Satur! CMG *~
            *06/19/2013! (AWD017) mod for NTX and various other   ! CMG *~
            *          !   changes                                !     *~
            *06/12/2017! (CR 986) new screen mesh added to gencode! RDB *~
            *10/03/2017! (CR1133) new wire mesh buckets added     ! RDB *~
            *12/21/2017! (CR1240) Input from new scrap scanning   ! RDB *~
            *03/13/2018! (CR1361) New lookup for eff_matrl for scrap!RDB*~
            *08/03/2018! (CR1489) Mod to use prod WEEKS for Hours ! CMN *~
            *01/29/2019! CR-1821 ADP Increase Emp ID field length ! DES *~
            *03/01/2019! CR-1894 Increase EMP DEPT to 3 bytes     ! DES *~
			*11/11/2022! CR3200 Using auto current with dept 022  ! RDB *~
			*04/07/2023! CR3292 Prompt screen in apcplc40         ! RDB *~
            *************************************************************

        dim                              /* EWDEFFCY                   */~
            eff_key$13, eff_sav$13,      /* Primary KEY                */~
            eff_key1$19,                 /* Alt Key                    */~
            eff_proc$1,                  /* EFF Process Flag           */~
            eff_wk$2,                    /* Production Week            */~
            eff_day$1,                   /* Production Day             */~
            eff_dte$6,                   /* Production Date            */~
            eff_model$(100%)3, model$3,  /* Product Models             */~
            cont_mod$3,                  /* Cont. Head/Sill Model      */~
            effect_unt(100%),            /* Effective Scanned Units    */~
            effect_rmk(100%),            /* Effective Remake Units    */~
            pr_dte$6,                    /* Production Date            */~
            eff_unt(100%),               /* Product Units              */~
            eff_unts(100%),              /* Product Units              */~
            eff_untss(100%),             /* Product Units              */~
            eff_untpp(100%),             /* Product Units              */~
            eff_unta(100%),              /* Product Units              */~
            eff_untb(100%),              /* Product Units              */~
            eff_untc(100%),              /* Product Units              */~
            eff_value$14                 /* Product Value  (AWD0009)   */

        dim                               /* EWDEFFEX                   */~
            ex_key$12, ex_sav$12,         /* EWDEFFEX read key          */~
            ex_unt$(3%)3, ex_year$4,      /* Extra UPMH need for Effic %*/~
            eff_price(12%),               /* Average Selling Price      */~
            eff_upmh(12%), ex_upmh(12%),  /* Planning UPMH              */~
            eff_scrapa(12%),              /* Average Scrap Weight       */~
            eff_scrapb(12%),              /* Mistake Scrap              */~
            eff_labor(12%),               /* Labor $ Goal Per Unit      */~
            eff_lbs(12%),                 /* Lbs per Unit Goal          */~
            eff_matrl(12%),               /* Material Cost Per Lbs      */~
            extra_matrl(12%)              /* Find Material Cost for Scrap */

        dim                              /* Screen Variables           */~
            sc_load$5, ed_load$,         /* Production Load or (ALL)   */~
            sc_wk$2, sc_wk_dte$8,        /* SCREEN WEEK                */~
            sc_day$1, sv_yr$,            /* SCREEN PRODUCTION DAY      */~
            sv_wk$2, sv_day$1,           /* CURRENT WEEK AND DAY       */~
            send_dept$3,                 /* Send Department now 3      */~
            sc_dept$3, sc_dept_d$30,     /* Department Code/Descript   */~
            sc_yr$4, prv_yr$4,           /* PRODUCTION YEAR            */~
            d_txt$(300%)10,              /* Screen Text Prompts        */~
            d_val$(300%,7)6,             /* Screen Text Values         */~
            d_val(300%,7%),              /* Screen Value               */~
            tot_unit$10,                 /* Screen Total Units         */~
            tot_hrsr$10,                 /* Screen Total Reg Hours     */~
            tot_hrso$10,                 /* Screen Total Overtime Hours*/~
            tot_wages$10,                /* Screen Total Wages         */~
            days$(7%)9,                  /* DAYS OF THE WEEK           */~
            days$9,                      /* DAY OF THE WEEK            */~
            date$8,                      /* SCREEN DATE                */~
            sc_desc$79                   /* Screen Description         */
       
        dim                                                              ~
            readkey$50,                  /* Generic Key                */~
            savekey$12,                  /* Generic Key                */~
            descr$32,                    /* Generic Description        */~
            ef(7%),                      /* Efficiency Hours by Day    */~
            ef_ov(7%),                   /* Efficiency Overtime Hours  */~
            tot_ef(7%),                  /* Total Hours each Day       */~
            wg(7%),                      /* Total Wages Each Day       */~
            ind_dept$(40%),              /* Indirect Department Values */~
            e_shift$2,                   /* Employee Shift             */~
            auto_tab$(50%)2,             /* AUTO LOAD DEPT'S   CR3200  */~
            sav_key$9,                   /* Copy Key                   */~
            dt_key$24,                   /* (APCPLNDT) - Primary Key   */~
            hh$40                        /* Open Error Header Msg      */
        
        dim                              /* Subroutine - Variables     */~
            cur_yr$4, cur_yr_bi$2,       /* Current Year               */~
            cur_wk$2,  cur_dy$1,         /* Current Prod. Week an Day  */~
            cur_dte$6, cur_date$8,       /* Prod Week Date Form/Unform */~
            ent_yr$4, ent_yr_bi$2,       /* Julian Year and Day YYDDD  */~
            ent_wk$2,  ent_dy$1,         /* Entry Prod. Week an Day    */~
            ent_dte$6, ent_date$8,       /* Prod Week Date Form/Unform */~
            prv_yr_bi$2, month$2,        /* binary fmt                 */~
            temp1$10, temp2$8            /* work variables             */


        dim dbg_msg$45,                  /* Header Message             */~
            dbg$(10%)10, type$7,         /* Convert and Store Debug Val*/~
            d_inp$79,                    /* Set Display Screen Prompt  */~
            dbkey$32                     /* Set Function Key Masks     */

        dim pd_dept$3, p_mod$(330%)3,    /* Planning Dept. Scan Time   */~
            jdate1$7, p_unt%(330%,3%),   /* Julian Dates Yes't, Today  */~
            p_unts%(330%,3%),            /* SAMPLES ONLY               */~
            p_untss%(330%,3%),           /* CHARGE SASHS ONLY          */~
            p_untpp%(330%,3%),           /* CHARGE PARTS ONLY          */~
            bg_dte$6, p_val(330%,3%),    /* BG_DTE$ Yesterday, Today   */~
            p_mrp(6%,3%),                /* Scanned Costing Values     */~
            p_shft$2                     /* Shift Code or 'AL'         */

       dim  uc_key$11,                   /* Units Capacity Key         */~
            pl_unta%(7%)                 /* Actual Planning Units      */

       dim                               /* Glass Cnt  - Variables     */~
            rm%(6%,3%),                  /* Process Counter            */~
            eff_glass(6%),              /* Process Counter            */~
            rm_anal$1,                   /* Analysis Flag 0=Yes 1=No   */~
            sc_dte$8                     /* Scan Date                  */

        dim                              /* Extra Variables            */~
            f2%(8%),                     /* = 0 if the file is open    */~
            f1%(8%),                     /* = 1 if READ was successful */~
            fs%(8%), axd$4,              /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(11%)20,                /* Text from file opening     */~
            cursor%(2%),                 /* Cursor location for edit   */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(297%)1,                /* Field Attribute Characters */~
            lfac1$1,                     /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            hdr$40,                      /* ASKUSER HEADER             */~
            msg$(3%)79,                  /* ASKUSER TEXT               */~
            userid$3                     /* Current User Id            */
            
        dim emp_yr$4,                    /* Employee Year       (AWD016)*/~ 
            emp_wk$2,                    /* Employee Week       (AWD016)*/~
            emp_day$1,                   /* Employee Time Clock (AWD016)*/~
            emp_ed_day$1                 /* Emp End Time Clock  (AWD016)*/    
            
        dim message$256

        dim schema$8                     /* Schema            (AWD017) */
/* CR1240 */        
        dim                              /* File = (AWDSRPDP)               */~
            sp_prod_dte$6,               /* Production Date                 */~
            sp_yr$4,                     /* Production Year                 */~
            sp_wk$2,                     /* Production Week                 */~
            sp_day$1,                    /* Production Day                  */~
            sp_dept$3,                   /* Production Department           */~
            sp_scan_dte$6,               /* Scan Date                       */~
            sp_scan_time$4,              /* Scan Time                       */~
            sp_filler$16,                /* Filler space                    */~
            awd_scrp_key$16              /* Key field for File              */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21, apc1$40
            apc$   = "(New) EWD Daily Production Efficiency   "
            apc1$  = "(New) EWD Daily Scrap Efficiency   "
            pname$ = "EWDEFFTB - Rev: R6.04"

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
            * #1  ! APCEFFCY ! New Efficiency Master Table File         *~
            * #2  ! GENCODES ! Master Code Table File                   *~
            * #3  ! APCEMPMT ! Employee Master Detail File              *~
            * #4  ! APCEMPLY ! Employee Master File                     *~
            * #5  ! APCPLNDT ! Production Tracking File                 *~
            * #6  ! APCPLNAD ! Production Tracking Audit File           *~
            * #7  ! APCPLNDP ! Master Department File                   *~
            * #8  ! APCPLNGR ! Glass Sched/Remake File                  *~
            * #9  ! APCCSTLR ! Department Average Hourly Rate           *~
            * #10 ! APCPLNGA ! Glass Remake File/All Completed Remakes  *~
            * #11 ! EWDEFFPY ! New Efficiency Pay File                  *~
            * #12 ! APCPLNUC ! Planning Master Units Capacity File      *~
            * #13 ! EWDEFFEX ! New Efficiency Table/Average Storage     *~
            * #14 ! AWDSRPDP ! Scanned department scrap totals          *~
            * #15 ! ADPEMPMT ! ADP Employee Master Hours File           *~
            * #16 ! DFEMPMT  ! DF Employee Master Hours File            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #1,   "EWDEFFCY",                                     ~
                        varc,     indexed,  recsize =  2024,             ~
                        keypos =    7, keylen =    13,                   ~
                        alt key  1, keypos =    1, keylen =  19      

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #3,   "APCEMPMT",                                     ~
                        varc,     indexed,  recsize =   128,             ~
                        keypos =    1, keylen =   13,                    ~
                        alt key  1, keypos =    4, keylen =  10, dup

            select #4,   "APCEMPLY",                                     ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    7, keylen =    5,                    ~
                        alt key  1, keypos =    1, keylen =  11, dup,    ~
                            key  2, keypos =   12, keylen =  26, dup

            select #5,   "APCPLNDT",                                     ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   24, keylen =   23,                    ~
                        alt key  1, keypos =   47, keylen =  57,         ~
                            key  2, keypos =   53, keylen =  51,         ~
                            key  3, keypos =    1, keylen =  23, dup,    ~
                            key  4, keypos =   96, keylen =   8, dup

            select #6,   "APCPLNAD",                                     ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =   19, keylen =   33,                    ~
                        alt key  1, keypos =    1, keylen =  33

            select #7,   "APCPLNDP",                                     ~
                        varc,     indexed,  recsize =   32,              ~
                        keypos =   11, keylen =   12,                    ~
                        alt key  1, keypos =    9, keylen =  14,         ~
                            key  2, keypos =    4, keylen =  12,         ~
                            key  3, keypos =    1, keylen =  15

            select #8,  "APCPLNGR",                                      ~
/*PAR000*/              varc,     indexed,  recsize =  384,              ~
                        keypos = 22,   keylen =  12,                     ~
                        alt key  1, keypos  =     7, keylen = 27,        ~
                            key  2, keypos  =     1, keylen = 33,        ~
                            key  3, keypos  =    13, keylen = 21
                                                        
            select #9,   "APCCSTLR",                                     ~
                        varc,     indexed,  recsize =  102,              ~
                        keypos =    1, keylen =   3                                                     

            select #10,  "APCPLNGA",                                     ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =  1,   keylen =  18,                     ~
                        alt key  1, keypos  =     7, keylen = 17           

            select #11,  "EWDEFFPY",                                     ~
                        varc,     indexed,  recsize =   64,              ~
                        keypos =  7,   keylen =  15,                     ~
                        alt key  1, keypos  =     1, keylen = 21           

            select #12,  "APCPLNUC",                                     ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =  1,   keylen =  11,                     ~
                        alt key  1, keypos  =     7, keylen = 11           

            select #13,  "EWDEFFEX",                                     ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =  1,   keylen =   12 

            select #14,  "AWDSRPDP",                                     ~
                        varc,     indexed,  recsize =  50,               ~
                        keypos = 01,   keylen = 16       

            select #15, "ADPEMPMT",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 7,    keylen =  23,                     ~
                        alt key 1, keypos = 10, keylen = 23,             ~
                            key 2, keypos =  1, keylen = 29,             ~
                            key 2, keypos = 90, keylen =  6, dup      

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 500%, rslt$(1%))

            filename$ = "GENCODES" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCEMPMT" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCEMPLY" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDT" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNAD" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDP" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNGR" : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCCSTLR" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNGA" : call "EWDOPEN" (#10, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNUC" : call "EWDOPEN" (#12, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "EWDEFFEX" : call "EWDOPEN" (#13, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "ADPEMPMT" : call "EWDOPEN" (#15, filename$, err%)
            if err% <> 0% then gosub open_error        /*(CR2588) */    

            call "OPENCHCK" (#14, fs%(14%), f2%(14%), 500%, rslt$(14%))

            call "OPENCHCK" (#11, fs%(11%), f2%(11%), 500%, rslt$(11%))

            call "OPENOLIB" (#6, "SHARE", f2%(6%), rslt$(6%), axd$)

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            call "EXTRACT" addr("ID", userid$)
            gosub check_access
            days$(1%) = "MONDAY   " : days$(2%) = "TUESDAY  "
            days$(3%) = "WEDNESDAY" : days$(4%) = "THURSDAY "
            days$(5%) = "FRIDAY   " : days$(6%) = "SATURDAY "
            days$(7%) = "SUNDAY   "
/* (AWD019) */            
            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #2, schema_err%)            
/* (\AWD019) */
            date$ = date
            call "DATEFMT" (date$)

REM - DEPARTMENT '22 RUNS THE AUTOCURRENT'
REM - AUTOCURRENT IS FOR ALL OF THE INDIRECT DEPARTMENTS THAT HAVE NO 
REM - SCANNING UNITS  CR3200 update list
            auto_tab$( 1%) = "15"  : auto_tab$(25%) = "30"
            auto_tab$( 2%) = "24"  : auto_tab$(26%) = "35"
            auto_tab$( 3%) = "13"  : auto_tab$(27%) = "01"
            auto_tab$( 4%) = "39"  : auto_tab$(28%) = "71"
            auto_tab$( 5%) = "31"  : auto_tab$(29%) = "40"
            auto_tab$( 6%) = "61"  : auto_tab$(30%) = "00"
            auto_tab$( 7%) = "45"  : auto_tab$(31%) = "02"
            auto_tab$( 8%) = "05"  : auto_tab$(32%) = "08"      
            auto_tab$( 9%) = "09"  : auto_tab$(33%) = "38"
            auto_tab$(10%) = "12"  : auto_tab$(34%) = "41"
            auto_tab$(11%) = "14"  : auto_tab$(35%) = "42"
            auto_tab$(12%) = "17"  : auto_tab$(36%) = "43"
            auto_tab$(13%) = "18"  : auto_tab$(37%) = "44"
            auto_tab$(14%) = "20"  : auto_tab$(38%) = "46"
            auto_tab$(15%) = "21"  : auto_tab$(39%) = "47"
            auto_tab$(16%) = "82"  : auto_tab$(40%) = "48"
            auto_tab$(17%) = "23"  : auto_tab$(41%) = "49"
            auto_tab$(18%) = "27"  : auto_tab$(42%) = "53"
            auto_tab$(19%) = "28"  : auto_tab$(43%) = "54"
            auto_tab$(20%) = "29"  : auto_tab$(44%) = "55"
            auto_tab$(21%) = "32"  : auto_tab$(45%) = "63"
            auto_tab$(22%) = "34"  : auto_tab$(46%) = "74"
            auto_tab$(23%) = "36"  : auto_tab$(47%) = "06"
            auto_tab$(24%) = "37"  : auto_tab$(48%) = "19"
            auto_tab$(49%) = "07"  : auto_tab$(50%) = "  "
            auto_current% = 0%
            auto_wages% = 0%
REM - MAX MODEL/PRODUCTS
            max_product% = 296%
REM - FOR IG AND REMAKES, SO ONLY SCANS UNITS ONCE                      
            beenherebefore% = 0%             /* Flag for Glass Remk Anal*/
REM - INDIRECT & SUPPORT DEPARTMENTS, WHETHER OR NOT TO LOAD PRODUCT
            init(" ") ind_dept$()
            ind_dept$(1%)  = "012" : ind_dept$(2%)  = "013"
            ind_dept$(3%)  = "015" : ind_dept$(4%)  = "022"
            ind_dept$(5%)  = "024" : ind_dept$(6%)  = "030"
            ind_dept$(7%)  = "031" : ind_dept$(8%)  = "032"
            ind_dept$(9%)  = "034" : ind_dept$(10%) = "035"
            ind_dept$(11%) = "037" : ind_dept$(12%) = "038"
            ind_dept$(13%) = "039" : ind_dept$(14%) = "045"
            ind_dept$(15%) = "061" : ind_dept$(16%) = "010"
            ind_dept$(17%) = "071" : ind_dept$(18%) = "001"
            ind_dept$(19%) = "041" : ind_dept$(20%) = "060"
            ind_dept$(21%) = "057" : ind_dept$(22%) = "003"  /* (AWD014) */
            ind_dept$(23%) = "041" : ind_dept$(24%) = "029"  /* (AWD012) */
            ind_dept$(25%) = "076" : ind_dept$(26%) = "   "
            ind_dept$(27%) = "   " : ind_dept$(28%) = "   "
            ind_dept$(29%) = "   " : ind_dept$(30%) = "   "
            ind_dept$(31%) = "   " : ind_dept$(32%) = "   "
            ind_dept$(33%) = "   " : ind_dept$(34%) = "   "
            ind_dept$(35%) = "   " : ind_dept$(36%) = "   "
            ind_dept$(37%) = "   " : ind_dept$(38%) = "   "
            ind_dept$(39%) = "   " : ind_dept$(40%) = "   "
/* (EWD0006)  */
/* (AWD019) */
            if schema% <> 2% then goto not_indirect_TX
            init(" ") ind_dept$()
            ind_dept$(1%)  = "082" : ind_dept$(2%)  = "078"
            ind_dept$(3%)  = "079" : ind_dept$(4%)  = "084"
            ind_dept$(5%)  = "086" : ind_dept$(6%)  = "007"
            ind_dept$(7%)  = "067" : ind_dept$(8%)  = "068"
            ind_dept$(9%)  = "072" : ind_dept$(10%) = "075"
            ind_dept$(11%) = "074" : ind_dept$(12%) = "096"
            ind_dept$(13%) = "092" : ind_dept$(14%) = "001"
            ind_dept$(15%) = "   " : ind_dept$(16%) = "   "
            ind_dept$(17%) = "   " : ind_dept$(18%) = "   "
            ind_dept$(19%) = "   " : ind_dept$(20%) = "   "
            ind_dept$(21%) = "   " : ind_dept$(22%) = "   " 
            ind_dept$(23%) = "   " : ind_dept$(24%) = "   " 
            ind_dept$(25%) = "   " : ind_dept$(26%) = "   "
            ind_dept$(27%) = "   " : ind_dept$(28%) = "   "
            ind_dept$(29%) = "   " : ind_dept$(30%) = "   "
            ind_dept$(31%) = "   " : ind_dept$(32%) = "   "
            ind_dept$(33%) = "   " : ind_dept$(34%) = "   "
            ind_dept$(35%) = "   " : ind_dept$(36%) = "   "
            ind_dept$(37%) = "   " : ind_dept$(38%) = "   "
            ind_dept$(39%) = "   " : ind_dept$(40%) = "   "
            
not_indirect_TX            
        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to 4%
L10100:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10280
L10120:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit%  = 17% then gosub move_units  /* CR3200 */
                      if keyhit%  = 10% then gosub complete_calc
                      if keyhit%  = 14% then goto inputmode_a
                      if keyhit% <>  4% then       L10260
L10210:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10120
                         if fieldnr% = 1% then L10100
                         goto L10210
L10260:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L10120
L10280:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editpg1
            eff_proc$ = "P"
            lastfieldnr% = 0%
            gosub'101(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then goto inputmode_a
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%

            if (cursor%(1%) - 2%) >  4% and                                 ~
               (cursor%(1%) - 2%) <  16% then fieldnr% = 4%

            if fieldnr% < 1% or fieldnr% > 4% then editpg1
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
            *       I N P U T   M O D E   F O R   R E P O R T           *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        inputmode_a
            gosub initialize_variables
            for fieldnr% = 1% to 2%
L12100:         gosub'052(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L12220
L12120:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L12200
L12150:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L12120
                         if fieldnr% = 1% then L12100
                         goto L12150
L12200:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L12120
L12220:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L12120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   F O R   R E P O R T            *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then gosub exit_program
                  if keyhit% <>  0% then       editpg2
L12390:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 2% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L12440:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L12440
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L12440
                  lastfieldnr% = fieldnr%
            goto L12390


        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************
        inputmode_scrap
            gosub initialize_variables

            for fieldnr% = 1% to 3%
L13100:         gosub'053(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L13280
L13120:         gosub'103(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L13260
L13210:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'053(fieldnr%)
                         if enabled% = 1% then L13120
                         if fieldnr% = 1% then L13100
                         goto L13210
L13260:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L13120
L13280:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L13120
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   M A I N   P R O G R A M        *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************
        editpg3
            eff_proc$ = "S"
            lastfieldnr% = 0%
            gosub'103(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then goto inputmode
                  if keyhit% <>  0% then       editpg3
L13220:     fieldnr% = cursor%(1%) - 2%
            if (cursor%(1%) - 2%) >  3% and                                 ~
               (cursor%(1%) - 2%) <  15% then fieldnr% = 3%
            if fieldnr% < 1% or fieldnr% > 3% then editpg3
            if fieldnr% = lastfieldnr% then    editpg3
            gosub'053(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg3
L13270:     gosub'103(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L13270
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13270
                  lastfieldnr% = fieldnr%
            goto L13220

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        print_report
            call "EWDEFF1B" ( sc_yr$,        /* Production Year       */~
                              sc_wk$,        /* Production Week       */~
                              sc_day$,       /* Production Day        */~
                              ent_dte$,      /* Screen Date           */~
                              pr_dte$,       /* Production Date       */~
                              schema%,       /* Schema code           */~
                              #1,            /* EWDEFFCY              */~
                              #2,            /* GENCODES              */~
                              #13 )          /* EWDEFFEX              */
        return clear all
        goto inputmode

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************
        deffn'051(fieldnr%)
            enabled% = 1%
        return

        deffn'052(fieldnr%)
            enabled% = 1%
        return

        deffn'053(fieldnr%)
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
         "Enter a Valid Production Week (1-52), Blank = Current Week?  ",~
         "Enter a Valid Production Day (1-7), Blank = Current Day?     ",~
         "Enter a Valid Department Code?                               ",~
         "Enter the Applicable Units for the Associated Products       "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28320
                inpmessage$ = edtmessage$
                return

L28320
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Valid Production Week (1-52), Blank = Current Week?  ",~
         "Enter a Valid Production Day (1-7), Blank = Current Day?     "

        deffn'070(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28330
                inpmessage$ = edtmessage$
                return

L28330
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn3_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn3_msg  :  data                                               ~
         "Enter a Valid Production Week (1-52), Blank = Current Week?  ",~
         "Enter a Valid Production Day (1-7), Blank = Current Day?     ",~
         "Enter the Applicable Department Scrap for Day?               "

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, scrap$, eff_dte$, sc_dept$,~
                      sc_dept_d$, days$, sc_wk_dte$, d_txt$(), d_val$(), ~
                      send_dept$, sc_yr$, eff_key$, eff_key1$, tot_unit$,~
                      tot_hrsr$, tot_hrso$, tot_wages$, sc_day$, sc_yr$, ~
                      sc_wk$, eff_model$(), sc_desc$, emp_yr$, emp_wk$,  ~
                      emp_day$, emp_ed_day$

             rm_anal$ = "0"                 /* Flag for Glass Analysis */
             p_no% = 0%                     /* Number of Models for Dept */
             mat d_val = zer
REM - DO NOT DO IN CLEAR REC
             eff_col2, eff_col3, eff_col4, eff_col8 = 0.0
             gosub clear_rec
        return

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
            call "SHOSTAT" ("Loading Department Data.")
            load% = 0%
            gosub clear_rec

            str(eff_key$,1%,1%) = eff_proc$
            str(eff_key$,2%,4%) = sc_yr$
            str(eff_key$,6%,2%) = sc_wk$
            str(eff_key$,8%,1%) = sc_day$
            str(eff_key$,9%,3%) = sc_dept$
            str(eff_key$,12%,2%) = "00"

            read #1,key = eff_key$, eod goto L30140
                gosub data_get

            if eff_proc$ = "P" then gosub convert_screen
L30140: return

        data_get
            get #1, using L35040, pr_dte$, eff_proc$, eff_year$,       ~
                    eff_wk$, eff_day$, sc_dept$, eff_shift$,           ~
                    eff_model$(), eff_unt(), eff_unts(), eff_untss(),  ~
                    eff_untpp(), eff_unta(), eff_untb(), eff_untc(),   ~
                    eff_col1, eff_col2, eff_col3, eff_col4, eff_col5,  ~
                    eff_col6, eff_col7, eff_col8, eff_col9, eff_col10, ~
                    eff_col11, eff_col12, eff_col13, eff_hrsm,         ~
                    eff_paym,  eff_glass(), eff_scan, mistake_weight, ~
                    eff_hrse, eff_avg, eff_value
            load% = 1%                          /*  (AWD0009)  */
        return
                   
        read_scan_scrap                /* CR1240 look up scanned dept scrap */
            str(awd_scrp_key$,1%,6%)  = pr_dte$
            str(awd_scrp_key$,7%,4%)  = eff_year$       
            str(awd_scrp_key$,11%,2%) = eff_wk$          
            str(awd_scrp_key$,13%,1%) = eff_day$         
            str(awd_scrp_key$,14%,3%) = sc_dept$    
            
            read #14, key = awd_scrp_key$, using L30144,                    ~
                      sp_prod_dte$,sp_yr$,sp_wk$,sp_day$,sp_dept$,sp_wgt,   ~
                      sp_scan_dte$, sp_scan_time$,sp_filler$,               ~
                      eod goto L30142
        return
L30142:     sp_wgt = 0 
        return
        
L30144: FMT                       /* File = (AWDSRPDP)               */~
            CH(06),               /* Production Date                 */~
            CH(04),               /* Production Year                 */~
            CH(02),               /* Production Week                 */~
            CH(01),               /* Production Day                  */~
            CH(03),               /* Production Department           */~
            PD(14,4),             /* Weight                          */~
            CH(06),               /* System Date of scan             */~
            CH(04),               /* System Time of scan             */~
            CH(16)                /* Filler space                    */   
            
        convert_screen  /* Convert Model & Units for Display on Screen     */
            m% = 4%     /* If m% = 4% then show units, sam/dsp, sas, & par */ 
            if schema% <> 1% then goto notNCScreen
            if sc_dept$ = "037" then m% = 1%  /* else only show units      */
            if sc_dept$ = "034" then m% = 1%
REM         if sc_dept$ = "021" then m% = 1%  /* (AWD015) */
            if sc_dept$ = "041" then m% = 1%  /* (AWD012) */
            if sc_dept$ = "038" then m% = 1%  /* (AWD013) */
            if sc_dept$ = "043" then m% = 7%  /* EWD0002 */
            if sc_dept$ = "000" then m% = 5%
            if sc_dept$ = "029" then m% = 1%  /* (AWD014) */ 
              goto setScreen
notNCScreen
            if sc_dept$ = "067" then m% = 1%  /* else only show units      */
            if sc_dept$ = "068" then m% = 1%
            if sc_dept$ = "074" then m% = 1%
            if sc_dept$ = "042" then m% = 1%                        
REM         if sc_dept$ = "021" then m% = 1%  
            if sc_dept$ = "000" then m% = 5%
            


setScreen                            
            for i% = 1% to max_product%            /* load screen until model = " " */
             if eff_model$(i%) = " " then goto L30150
             d_txt$(i%) = eff_model$(i%)
             convert eff_unt(i%) to d_val$(i%,1%), pic(#####0)
REM -  Only if Sample/Display, Sash, and Part Convert for Screen
             if m% = 4% or m% = 5% or m% = 7%  then                 ~
                   convert eff_unts(i%) to d_val$(i%,2%), pic(#####0)
             if m% = 4% or m% = 5% or m% = 7%  then                 ~
                   convert eff_untss(i%) to d_val$(i%,3%), pic(#####0)
             if m% = 4% or m% = 5% or m% = 7%  then                 ~
                   convert eff_untpp(i%) to d_val$(i%,4%), pic(#####0)
             if m% = 5% or m% = 7% then                             ~
                    convert eff_unta(i%) to d_val$(i%,5%), pic(#####0)
             if m% = 7% then                                        ~
                    convert eff_untb(i%) to d_val$(i%,6%), pic(#####0)
             if m% = 7% then                                        ~
                    convert eff_untc(i%) to d_val$(i%,7%), pic(#####0)

           next i%
L30150: p_no% = i% - 1%   /* Set product number to one less b/c loop all */
        return            /* ready incremented                           */

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************
        dataput
          call "SHOSTAT" ("Updating Department Data")
          init(" ") eff_key$
          str(eff_key$,1%,1%)  = eff_proc$
          str(eff_key$,2%,4%)  = eff_year$
          str(eff_key$,6%,2%)  = sc_wk$
          str(eff_key$,8%,1%)  = sc_day$
          str(eff_key$,9%,3%)  = sc_dept$
          str(eff_key$,12%,2%) = eff_shift$     /*  Eff Shift - Set to 00 */

          read #1,hold,key = eff_key$, eod goto L31160
              delete #1
L31160:   gosub data_put

          write #1, eod goto L31260

          if comp_calc% = 1% then return        /* if comp_calc then loop back */
          if eff_proc$ = "S" then return        /* If scrap do not test  */
          if sc_dept$ <> "022" then goto L31230 /* CHECK FOR WOOL        */
          if auto_wages% = 1% then return       /* Don't do auto_current */
             gosub auto_current                 /* if in wages routine.  */ 
			 
L31230:     if auto_current% = 1% then return
            if auto_wages% = 1% then return
        return clear all
        goto inputmode
L31260:     call "SHOSTAT" ("(Error) Updating ----> " & eff_key$)
            stop
        return clear all
        goto inputmode

        data_put
            put #1, using L35040, pr_dte$, eff_proc$, eff_year$,       ~
                    eff_wk$, eff_day$, sc_dept$, eff_shift$,           ~
                    eff_model$(), eff_unt(), eff_unts(), eff_untss(),  ~
                    eff_untpp(), eff_unta(), eff_untb(), eff_untc(),   ~
                    eff_col1, eff_col2, eff_col3, eff_col4, eff_col5,  ~
                    eff_col6, eff_col7, eff_col8, eff_col9, eff_col10, ~
                    eff_col11, eff_col12, eff_col13, eff_hrsm,         ~
                    eff_paym,  eff_glass(), eff_scan, mistake_weight, ~
                    eff_hrse, eff_avg, eff_value

        return                                        /*   (AWD0009)  */

        dataputrewrite
          call "SHOSTAT" ("Updating Department Data")
          init(" ") eff_key$
          str(eff_key$,1%,1%)  = eff_proc$
          str(eff_key$,2%,4%)  = eff_year$
          str(eff_key$,6%,2%)  = sc_wk$
          str(eff_key$,8%,1%)  = sc_day$
          str(eff_key$,9%,3%)  = sc_dept$
          str(eff_key$,12%,2%) = eff_shift$     /*  Eff Shift - Set to 00 */

          read #1,hold,key = eff_key$, eod goto L31165

          gosub dataput_rewrite

          rewrite #1, eod goto L31165

          return
L31165:     call "SHOSTAT" ("(Error) Completing Calc --> " & eff_key$)
            stop
        return clear all
        goto inputmode

        dataput_rewrite
            put #1, using L35045, eff_unt(), eff_unts(), eff_untss(),  ~
                    eff_untpp(), eff_unta(), eff_untb(), eff_untc(),   ~
                    eff_col1, eff_col2, eff_col3, eff_col4, eff_col5,  ~
                    eff_col6, eff_col7, eff_col8, eff_col9, eff_col10, ~
                    eff_col11, eff_col12, eff_col13, eff_hrsm,         ~
                    eff_paym,  eff_glass(), eff_scan, mistake_weight, ~
                    eff_hrse, eff_avg, eff_value
                                           /*  (AWD0009)  */
        return

L35045: FMT POS(320), 100*BI(2), 100*BI(2), 100*BI(2), 100*BI(2),      ~
            100*BI(2), 100*BI(2), 100*BI(2), PD(14,4), PD(14,4),       ~
            PD(14,4), PD(14,4), PD(14,4), PD(14,4), PD(14,4), PD(14,4),~
            PD(14,4), PD(14,4), PD(14,4), PD(14,4), PD(14,4), PD(14,4),~
            PD(14,4), 6*BI(2), PD(14,4), PD(14,4), PD(14,4), PD(14,4), ~
            PD(14,4) 
        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L35040: FMT                       /* File = (EWDEFFCY)               */~
            CH(06),               /* Production Date                 */~
            CH(01),               /* Process Flag P=Prod S=Scrap     */~
            CH(04),               /* Production Year                 */~
            CH(02),               /* Production Week                 */~
            CH(01),               /* Production Day                  */~
            CH(03),               /* Production Department           */~
            CH(02),               /* Production Shift 00-Default     */~
            100*CH(3),            /* Production Models by Dept       */~
            100*BI(2),            /* Production Scanning Units       */~
            100*BI(2),            /* Production Sample/Displays      */~
            100*BI(2),            /* Production Sash Units           */~
            100*BI(2),            /* Production Part Units           */~
            100*BI(2),            /* Production Extra Units          */~
            100*BI(2),            /* Production Extra Units          */~
            100*BI(2),            /* Production Extra Units          */~
            PD(14,4),             /* Total Effective Unt or Scrap Lbs*/~
            PD(14,4),             /* Reg Hrs or Act. Scrap Lbs       */~
            PD(14,4),             /* OT Hrs or Scrap Lbs/Unit        */~
            PD(14,4),             /* Tot Hrs or Lbs/Unit GOAL        */~
            PD(14,4),             /* Act. Units/Hrs or Tot Mat. Used */~
            PD(14,4),             /* Plan Units or Mat Into Product  */~
            PD(14,4),             /* Var Unit/Hrs or In Process Scrap*/~
            PD(14,4),             /* Labor Dollars or Mistake Scrap  */~
            PD(14,4),             /* Lab Dol/Unit or Mstke Scrap %   */~
            PD(14,4),             /* Lab Dol Goal/Unit or Tot Scrap% */~
            PD(14,4),             /* Var Lab Dol or Tot Scrap% Goal  */~
            PD(14,4),             /* Prod Dol or Tot Scrap Var       */~
            PD(14,4),             /* Labor % of Prod $ or Tot Scrap$ */~
            PD(14,4),             /* Manager/TeamLeaders Hours       */~
            PD(14,4),             /* Manager/TeamLeaders Pay         */~ 
            6*BI(2),              /* Total Scanned For IG 'In House' */~
            PD(14,4),             /* Actual Scanned Units            */~
            PD(14,4),             /* Mstke + In Proc * Unts Weight   */~
            PD(14,4),             /* Eff. Earned Hours or UPMH Goal  */~
            PD(14,4),             /* Avg Weight for All mods in dept */~
            PD(14,4)              /* Eff Value         (AWD0009)     */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************
        deffn'101(fieldnr%, edit%)
        REM   GOSUB SET_PF1

              gosub'050(1%, fieldnr%)
                  if userid$ = "RRH" or userid$ = "CMG" or             ~
                     userid$ = "TM2" or userid$ = "CG1"                ~
                                     then lfac1$ = hex(82)             ~
                                     else lfac1$ = hex(84)

              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40105,         /* Production Week      */~
                                L40105,         /* Production Day       */~
                                L40105,         /* Department Code      */~
                                L40120          /* Table Values (Day)   */

              goto L40140

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40105:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40120:           for k% = 1% to p_no%   /* Loop to turn on screen values */
                    for ll% = 1% to m%   /* & Left align screen values    */
                      lfac$(3%+k%) = hex(82)
                      call "STRING" addr("LJ", d_val$(k%,ll%), 6%)
                    next ll%
                  next k%
                  return
L40140:     gosub set_pf1
            accept                                                       ~
             at (01,02), fac(hex(8c)), pname$                    ,ch(21),~
             at (01,66), "Today:",                                     ~
             at (01,73), fac(hex(8c)), date$                     ,ch(08),~
             at (01,24), fac(hex(a4)), apc$                      ,ch(40),~
             at (02,02), fac(hex(94)), errormsg$                 ,ch(79),~
                                                                         ~
             at (03,02), "Production Week, 1-52:",                       ~
             at (03,25), fac(lfac$(1%)), sc_wk$                  ,ch(02),~
             at (03,30), fac(hex(84)), sc_wk_dte$                ,ch(08),~
             at (03,50), fac(lfac$(1%)), sc_yr$                  ,ch(04),~
                                                                         ~
             at (04,02), "Production Day(1 - 7):",                       ~
             at (04,25), fac(lfac$(2%)), sc_day$                 ,ch(01),~
             at (04,30), fac(hex(84)), days$                     ,ch(09),~
                                                                         ~
             at (05,02), "Department Code      :",                       ~
             at (05,25), fac(lfac$(3%)), sc_dept$                ,ch(03),~
             at (05,30), fac(hex(84)), sc_dept_d$                ,ch(30),~
                                                                         ~
             at (06,02), fac(hex(84)), sc_desc$                  ,ch(79),~
             at (07,02), fac(hex(84)), d_txt$(1%+kk%)            ,ch(10),~
             at (07,15), fac(lfac$(4%)), d_val$(1%+kk%,1%)       ,ch(06),~
             at (07,24), fac(lfac$(4%)), d_val$(1%+kk%,2%)       ,ch(06),~
             at (07,33), fac(lfac$(4%)), d_val$(1%+kk%,3%)       ,ch(06),~
             at (07,42), fac(lfac$(4%)), d_val$(1%+kk%,4%)       ,ch(06),~
             at (07,51), fac(lfac$(4%)), d_val$(1%+kk%,5%)       ,ch(06),~
             at (07,60), fac(lfac$(4%)), d_val$(1%+kk%,6%)       ,ch(06),~
             at (07,69), fac(lfac$(4%)), d_val$(1%+kk%,7%)       ,ch(06),~
                                                                         ~
             at (08,02), fac(hex(84)), d_txt$(2%+kk%)            ,ch(10),~
             at (08,15), fac(lfac$(5%)), d_val$(2%+kk%,1%)       ,ch(06),~
             at (08,24), fac(lfac$(5%)), d_val$(2%+kk%,2%)       ,ch(06),~
             at (08,33), fac(lfac$(5%)), d_val$(2%+kk%,3%)       ,ch(06),~
             at (08,42), fac(lfac$(5%)), d_val$(2%+kk%,4%)       ,ch(06),~
             at (08,51), fac(lfac$(5%)), d_val$(2%+kk%,5%)       ,ch(06),~
             at (08,60), fac(lfac$(5%)), d_val$(2%+kk%,6%)       ,ch(06),~
             at (08,69), fac(lfac$(5%)), d_val$(2%+kk%,7%)       ,ch(06),~
                                                                         ~
             at (09,02), fac(hex(84)), d_txt$(3%+kk%)            ,ch(10),~
             at (09,15), fac(lfac$(6%)), d_val$(3%+kk%,1%)       ,ch(06),~
             at (09,24), fac(lfac$(6%)), d_val$(3%+kk%,2%)       ,ch(06),~
             at (09,33), fac(lfac$(6%)), d_val$(3%+kk%,3%)       ,ch(06),~
             at (09,42), fac(lfac$(6%)), d_val$(3%+kk%,4%)       ,ch(06),~
             at (09,51), fac(lfac$(6%)), d_val$(3%+kk%,5%)       ,ch(06),~
             at (09,60), fac(lfac$(6%)), d_val$(3%+kk%,6%)       ,ch(06),~
             at (09,69), fac(lfac$(6%)), d_val$(3%+kk%,7%)       ,ch(06),~
                                                                         ~
             at (10,02), fac(hex(84)), d_txt$(4%+kk%)            ,ch(10),~
             at (10,15), fac(lfac$(7%)), d_val$(4%+kk%,1%)       ,ch(06),~
             at (10,24), fac(lfac$(7%)), d_val$(4%+kk%,2%)       ,ch(06),~
             at (10,33), fac(lfac$(7%)), d_val$(4%+kk%,3%)       ,ch(06),~
             at (10,42), fac(lfac$(7%)), d_val$(4%+kk%,4%)       ,ch(06),~
             at (10,51), fac(lfac$(7%)), d_val$(4%+kk%,5%)       ,ch(06),~
             at (10,60), fac(lfac$(7%)), d_val$(4%+kk%,6%)       ,ch(06),~
             at (10,69), fac(lfac$(7%)), d_val$(4%+kk%,7%)       ,ch(06),~
                                                                         ~
             at (11,02), fac(hex(84)), d_txt$(5%+kk%)            ,ch(10),~
             at (11,15), fac(lfac$(8%)), d_val$(5%+kk%,1%)       ,ch(06),~
             at (11,24), fac(lfac$(8%)), d_val$(5%+kk%,2%)       ,ch(06),~
             at (11,33), fac(lfac$(8%)), d_val$(5%+kk%,3%)       ,ch(06),~
             at (11,42), fac(lfac$(8%)), d_val$(5%+kk%,4%)       ,ch(06),~
             at (11,51), fac(lfac$(8%)), d_val$(5%+kk%,5%)       ,ch(06),~
             at (11,60), fac(lfac$(8%)), d_val$(5%+kk%,6%)       ,ch(06),~
             at (11,69), fac(lfac$(8%)), d_val$(5%+kk%,7%)       ,ch(06),~
                                                                         ~
             at (12,02), fac(hex(84)), d_txt$(6%+kk%)            ,ch(10),~
             at (12,15), fac(lfac$(9%)), d_val$(6%+kk%,1%)       ,ch(06),~
             at (12,24), fac(lfac$(9%)), d_val$(6%+kk%,2%)       ,ch(06),~
             at (12,33), fac(lfac$(9%)), d_val$(6%+kk%,3%)       ,ch(06),~
             at (12,42), fac(lfac$(9%)), d_val$(6%+kk%,4%)       ,ch(06),~
             at (12,51), fac(lfac$(9%)), d_val$(6%+kk%,5%)       ,ch(06),~
             at (12,60), fac(lfac$(9%)), d_val$(6%+kk%,6%)       ,ch(06),~
             at (12,69), fac(lfac$(9%)), d_val$(6%+kk%,7%)       ,ch(06),~
                                                                         ~
             at (13,02), fac(hex(84)), d_txt$(7%+kk%)            ,ch(10),~
             at (13,15), fac(lfac$(10%)), d_val$(7%+kk%,1%)      ,ch(06),~
             at (13,24), fac(lfac$(10%)), d_val$(7%+kk%,2%)      ,ch(06),~
             at (13,33), fac(lfac$(10%)), d_val$(7%+kk%,3%)      ,ch(06),~
             at (13,42), fac(lfac$(10%)), d_val$(7%+kk%,4%)      ,ch(06),~
             at (13,51), fac(lfac$(10%)), d_val$(7%+kk%,5%)      ,ch(06),~
             at (13,60), fac(lfac$(10%)), d_val$(7%+kk%,6%)      ,ch(06),~
             at (13,69), fac(lfac$(10%)), d_val$(7%+kk%,7%)      ,ch(06),~
                                                                         ~
             at (14,02), fac(hex(84)), d_txt$(8%+kk%)            ,ch(10),~
             at (14,15), fac(lfac$(11%)), d_val$(8%+kk%,1%)      ,ch(06),~
             at (14,24), fac(lfac$(11%)), d_val$(8%+kk%,2%)      ,ch(06),~
             at (14,33), fac(lfac$(11%)), d_val$(8%+kk%,3%)      ,ch(06),~
             at (14,42), fac(lfac$(11%)), d_val$(8%+kk%,4%)      ,ch(06),~
             at (14,51), fac(lfac$(11%)), d_val$(8%+kk%,5%)      ,ch(06),~
             at (14,60), fac(lfac$(11%)), d_val$(8%+kk%,6%)      ,ch(06),~
             at (14,69), fac(lfac$(11%)), d_val$(8%+kk%,7%)      ,ch(06),~
                                                                         ~
             at (15,02), fac(hex(84)), d_txt$(9%+kk%)            ,ch(10),~
             at (15,15), fac(lfac$(12%)), d_val$(9%+kk%,1%)      ,ch(06),~
             at (15,24), fac(lfac$(12%)), d_val$(9%+kk%,2%)      ,ch(06),~
             at (15,33), fac(lfac$(12%)), d_val$(9%+kk%,3%)      ,ch(06),~
             at (15,42), fac(lfac$(12%)), d_val$(9%+kk%,4%)      ,ch(06),~
             at (15,51), fac(lfac$(12%)), d_val$(9%+kk%,5%)      ,ch(06),~
             at (15,60), fac(lfac$(12%)), d_val$(9%+kk%,6%)      ,ch(06),~
             at (15,69), fac(lfac$(12%)), d_val$(9%+kk%,7%)      ,ch(06),~
                                                                         ~
             at (16,02), fac(hex(84)), d_txt$(10%+kk%)           ,ch(10),~
             at (16,15), fac(lfac$(13%)), d_val$(10%+kk%,1%)     ,ch(06),~
             at (16,24), fac(lfac$(13%)), d_val$(10%+kk%,2%)     ,ch(06),~
             at (16,33), fac(lfac$(13%)), d_val$(10%+kk%,3%)     ,ch(06),~
             at (16,42), fac(lfac$(13%)), d_val$(10%+kk%,4%)     ,ch(06),~
             at (16,51), fac(lfac$(13%)), d_val$(10%+kk%,5%)     ,ch(06),~
             at (16,60), fac(lfac$(13%)), d_val$(10%+kk%,6%)     ,ch(06),~
             at (16,69), fac(lfac$(13%)), d_val$(10%+kk%,7%)     ,ch(06),~
                                                                         ~
             at (17,02), fac(hex(84)), d_txt$(11%+kk%)           ,ch(10),~
             at (17,15), fac(lfac$(14%)), d_val$(11%+kk%,1%)     ,ch(06),~
             at (17,24), fac(lfac$(14%)), d_val$(11%+kk%,2%)     ,ch(06),~
             at (17,33), fac(lfac$(14%)), d_val$(11%+kk%,3%)     ,ch(06),~
             at (17,42), fac(lfac$(14%)), d_val$(11%+kk%,4%)     ,ch(06),~
             at (17,51), fac(lfac$(14%)), d_val$(11%+kk%,5%)     ,ch(06),~
             at (17,60), fac(lfac$(14%)), d_val$(11%+kk%,6%)     ,ch(06),~
             at (17,69), fac(lfac$(14%)), d_val$(11%+kk%,7%)     ,ch(06),~
                                                                         ~
             at (18,02), "Total Units = ",                               ~
             at (18,20), fac(lfac1$), tot_unit$                  ,ch(10),~
                                                                         ~
             at (19,02), "Reg. Hrs = ",                                  ~
             at (19,15), fac(lfac1$), tot_hrsr$                  ,ch(10),~
                                                                         ~
             at (19,30), "Ovr. Hrs = ",                                  ~
             at (19,43), fac(lfac1$), tot_hrso$                  ,ch(10),~
             at (19,55), "Wages = ",                                     ~
             at (19,63), fac(lfac1$), tot_wages$                 ,ch(10),~
                                                                         ~
             at (20,02), "Total Value = ",                               ~ 
             at (20,20), fac(lfac1$), eff_value$                 ,ch(14),~
             at (21,02), fac(hex(a4)),   inpmessage$             ,ch(79),~
             at (22,02), fac(hex(8c)),   pf$(1%)                 ,ch(79),~
             at (23,02), fac(hex(8c)),   pf$(2%)                 ,ch(79),~
             at (24,02), fac(hex(8c)),   pf$(3%)                 ,ch(79),~
                                                                         ~
             keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L40865            /* FIRST    */
L40850:           kk% = 0%
                  goto L40140

L40865:        if keyhit% <> 3% then goto L40890            /* LAST     */
L40870:           x% = int(p_no% / 11%)
                  kk% = (x% * 11%)
                  goto L40140

L40890:        if keyhit% <> 6% then goto L40925            /* PREVIOUS */
                  if fieldnr% = 1% then goto L40925       /* NOT APPLIC */
                  if kk% < 12% then goto L40850
                  kk% = kk% - 11%
                  if kk% <= 1% then goto L40850
                  goto L40140

L40925:        if keyhit% <> 5% then goto L40935            /* NEXT     */
                  kk% = kk% + 11%
                  if kk% < p_no% then goto L40140
                  goto L40870

L40935:        if keyhit% <> 8% then goto L40940          /* Scrap Input */
                  gosub inputmode_scrap                   /* Screen      */   

L40940:        if keyhit% <> 9% then goto L40945        /* Recalc Wages    */
                  eff_col2, eff_col3, eff_col4, eff_col8 = 0.0
/* (AWD016) */                  
REM                  IF EDIT% = 1% THEN SC_DAY$ = "1"      /* IF EDIT = 1     */
REM                  IF EDIT% = 1% THEN ED_DAY$ = "7"      /* THEN RECALC ALL */
REM                  IF EDIT% = 2% THEN ED_DAY$ = SC_DAY$  /* DEPTS, ELSE ONLY*/
                  if edit% = 1% then emp_day$ = "1"         /* If edit = 1    */
                  if edit% = 1% then emp_ed_day$ = "7"      /* then recalc all*/
                  if edit% = 2% then emp_ed_day$ = emp_day$ /* depts,else only*/

                    gosub recalc_wages                  /* screen dept     */
/* (AWD016) */                    
REM                  IF SC_DAY% = ED_DAY% THEN GOTO L40945
                  if emp_day% = emp_ed_day% then goto L40945                  
                  return clear all
                  goto inputmode

L40945:        if keyhit% <> 11% then goto L40946
                      gosub recalc_wages2
                      return clear all
                      goto inputmode

L40946:        if keyhit% <> 7% then goto L40950    /* Recalc Scanned Unt */
                  mat d_val = zer
                  gosub clear_rec
                  gosub load_prod_indirect
                  gosub calc_scanned_units
                  if (sc_dept$ = "037" or sc_dept$ = "034") and schema% = 1% ~
                     then gosub glass_analysis
                  if (sc_dept$ = "074" or sc_dept$ = "075") and schema% = 2% ~
                     then gosub glass_analysis                     
REM                  gosub L50300       /* go to unit val test routine to validate */
                                     /* and convert units                       */
L40950:        if keyhit% <> 12% then goto L40955
                  gosub delete_record
L40955:        if keyhit% <> 15% then goto L40970
                  call "PRNTSCRN"
                  goto L40140

L40970:        if edit% = 2% then gosub convert_data2  /* Reconvert Wages */
               close ws                                /* if changed      */
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
         init(" ") pf$()
         if load% = 0% then goto L41130
         gosub convert_data1          
L41130:  if edit% = 2% then L41185     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (14)Print report"
            pf$(2%) = "                 (8)Enter Scrap Amounts " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(4)Previous Fld                         " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ff0304ffffff08ffffff0c0d0e0f10000000140000)

            if fieldnr% = 1% then L41135
            pf$(1%) = "(1)Start Over        (4)Previous Fld    " &       ~
                      "                                       "
            pf$(2%) = "(2)First Page        (5)Next Page       " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(3)Last Page         (6)Previous Page   " &       ~
                      "                                       "

            pfkeys$ = hex(010203040506ffffffffff0c0dff0f10000000140000)
L41135:     if fieldnr% > 1% then L41150
                str(pf$(3%),1%,16%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
        return
L41150:     if fieldnr% <> 2% then L41155
                str(pf$(3%),41%,21%) = "(9)Recalc Emp Wages  " 
                str(pfkeys$,9%,1%) = hex(09)     
L41155:     if fieldnr% <> 3% then L41160
                str(pf$(2%),61%,21%) = "(10)Complete Calc    " 
                str(pfkeys$,10%,1%) = hex(0A)     

                str(pf$(3%),64%,15%) = "(17)Move Units "
                str(pfkeys$,17%,1%) = hex(11)
                
                str(pf$(3%),41%,21%) = "(11)Recalc Emp Wages2  "
                str(pfkeys$,11%,1%) = hex(0B)     
L41160:     if fieldnr% < 4% then L41165
                str(pf$(3%),41%,19%) = "(7)Recalc Scan Unt " 
                str(pfkeys$,7%,1%) = hex(07)     
            if userid$ <> "CMG" and userid$ <> "PWW" then goto L41165
                str(pf$(3%),61%,18%) = "(12)Delete Record " 
                str(pfkeys$,12%,1%) = hex(0c)     
L41165:     if fieldnr% < 5% then goto L41170
               str(pfkeys$,20%,1%) = hex(ff)
               str(pfkeys$,12%,1%) = hex(ff)
               str(pfkeys$,13%,1%) = hex(ff)
L41170:     gosub check_screen
        return
L41185: if fieldnr% > 0% then L41235  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "(9)Recalc Emp Wages    (16)Update Data "
            pfkeys$ = hex(01ff03ffffffffff09ffffffffff0f1000)
            return

L41235:
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        check_screen
            if p_no% > 11% then goto L41320
               gosub no_first
               gosub no_next
               gosub no_last
               gosub no_prev
               return
L41320:      if kk% >= 11% then goto L41335
                gosub no_first
                gosub no_prev
L41335:      if (kk% + 11%) <= p_no% then goto L41345
                gosub no_last
L41345:      if kk% <= (p_no% - 11%) then goto L41355
                gosub no_next
L41355: return
        no_first
            str(pf$(2%),1%,14%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_next
            str(pf$(2%),22%,14%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_last
            str(pf$(3%),1%,14%)  = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prev
            str(pf$(3%),22%,18%) = " " : str(pfkeys$,6%,1%) = hex(ff)
        return
        convert_data1 /* convert calc wage values for screen */
           convert eff_col1 to tot_unit$, pic(########.#)
           convert eff_col2 to tot_hrsr$, pic(######.##-)
           convert eff_col3 to tot_hrso$, pic(######.##-)
           convert eff_col8 to tot_wages$, pic(######.##-)
           convert eff_value to eff_value$, pic(######.##-)
        return
        convert_data2 /* convert again incase values change */
           convert tot_unit$ to eff_col1, data goto L41360
L41360:    convert tot_hrsr$ to eff_col2, data goto L41365
L41365:    convert tot_hrso$ to eff_col3, data goto L41370
L41370:    convert tot_wages$ to eff_col8, data goto L41375
L41375:    convert eff_value$ to eff_value, data goto L41380
           eff_col4 = 0.0
           eff_col4 = eff_col2 + eff_col3
L41380: return

        REM *************************************************************~
            *               R E P O R T   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************
        deffn'102(fieldnr%, edit%)
              gosub set_pf2

              gosub'060(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42190,         /* Production Week      */~
                                L42190          /* Production Day       */
              goto L42220

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42190:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42220:     accept                                                       ~
               at (01,02),                                               ~
                  "Production Efficiency/Scrap Reports",                 ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Production Week, 1-52:",                     ~
               at (03,25), fac(lfac$(1%)), sc_wk$               , ch(02),~
               at (03,30), fac(hex(84)), sc_wk_dte$             , ch(08),~
                                                                         ~
               at (03,40), fac(lfac$(1%)), sc_yr$               , ch(04),~
                                                                         ~
               at (04,02), "Production Day(1 - 7):",                     ~
               at (04,25), fac(lfac$(2%)), sc_day$              , ch(01),~
               at (04,30), fac(hex(84)), days$                  , ch(09),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L42700
                  call "PRNTSCRN"
                  goto L42220

L42700:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L42890     /*  Input Mode             */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                 (4)Previous Field      " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L42850
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L42850:     if fieldnr% > 1% then L42870
                str(pf$(2),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L42870:     return

L42890: if fieldnr% > 0% then L42990  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Print Report"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return

L42990:
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen for Scrap by Dept          *~
            *************************************************************
        deffn'103(fieldnr%, edit%)

              gosub'070(1%, fieldnr%)
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L43000,         /* Production Week      */~
                                L43000,         /* Production Day       */~
                                L43010          /* Table Values (Day)   */

              goto L43020

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
                  lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L43000:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L43010:           for k% = 1% to p_no%
                      lfac$(2%+k%) = hex(82)
                  next k%
                  return
L43020:     gosub set_pf3
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc1$                  , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Production Week, 1-52:",                     ~
               at (03,25), fac(lfac$(1%)), sc_wk$               , ch(02),~
               at (03,30), fac(hex(84)), sc_wk_dte$             , ch(08),~
               at (03,50), fac(lfac$(1%)), sc_yr$               , ch(04),~
                                                                         ~
               at (04,02), "Production Day(1 - 7):",                     ~
               at (04,25), fac(lfac$(2%)), sc_day$              , ch(01),~
               at (04,30), fac(hex(84)), days$                  , ch(09),~
                                                                         ~
               at (06,02), fac(hex(84)), d_txt$(1%+kk%)         , ch(10),~
               at (06,15), fac(lfac$(3%)), d_val$(1%+kk%,1%)    , ch(06),~
                                                                         ~
               at (06,27), fac(hex(84)), d_txt$(12%+kk%)        , ch(10),~
               at (06,40), fac(lfac$(14%)), d_val$(12%+kk%,1%)  , ch(06),~
                                                                         ~
               at (06,52), fac(hex(84)), d_txt$(23%+kk%)        , ch(10),~
               at (06,65), fac(lfac$(25%)), d_val$(23%+kk%,1%)  , ch(06),~
                                                                         ~
               at (07,02), fac(hex(84)), d_txt$(2%+kk%)         , ch(10),~
               at (07,15), fac(lfac$( 4%)), d_val$(2%+kk%,1%)   , ch(06),~
                                                                         ~
               at (07,27), fac(hex(84)), d_txt$(13%+kk%)        , ch(10),~
               at (07,40), fac(lfac$(15%)), d_val$(13%+kk%,1%)  , ch(06),~
                                                                         ~
               at (07,52), fac(hex(84)), d_txt$(24%+kk%)        , ch(10),~
               at (07,65), fac(lfac$(26%)), d_val$(24%+kk%,1%)  , ch(06),~
                                                                         ~
               at (08,02), fac(hex(84)), d_txt$(3%+kk%)         , ch(10),~
               at (08,15), fac(lfac$( 5%)), d_val$(3%+kk%,1%)   , ch(06),~
                                                                         ~
               at (08,27), fac(hex(84)), d_txt$(14%+kk%)        , ch(10),~
               at (08,40), fac(lfac$(16%)), d_val$(14%+kk%,1%)  , ch(06),~
                                                                         ~
               at (08,52), fac(hex(84)), d_txt$(25%+kk%)        , ch(10),~
               at (08,65), fac(lfac$(27%)), d_val$(25%+kk%,1%)  , ch(06),~
                                                                         ~
               at (09,02), fac(hex(84)), d_txt$(4%+kk%)         , ch(10),~
               at (09,15), fac(lfac$( 6%)), d_val$(4%+kk%,1%)   , ch(06),~
                                                                         ~
               at (09,27), fac(hex(84)), d_txt$(15%+kk%)        , ch(10),~
               at (09,40), fac(lfac$(17%)), d_val$(15%+kk%,1%)  , ch(06),~
                                                                         ~
               at (09,52), fac(hex(84)), d_txt$(26%+kk%)        , ch(10),~
               at (09,65), fac(lfac$(28%)), d_val$(26%+kk%,1%)  , ch(06),~
                                                                         ~
               at (10,02), fac(hex(84)), d_txt$(5%+kk%)         , ch(10),~
               at (10,15), fac(lfac$( 7%)), d_val$(5%+kk%,1%)   , ch(06),~
                                                                         ~
               at (10,27), fac(hex(84)), d_txt$(16%+kk%)        , ch(10),~
               at (10,40), fac(lfac$(18%)), d_val$(16%+kk%,1%)  , ch(06),~
                                                                         ~
               at (10,52), fac(hex(84)), d_txt$(27%+kk%)        , ch(10),~
               at (10,65), fac(lfac$(29%)), d_val$(27%+kk%,1%)  , ch(06),~
                                                                         ~
               at (11,02), fac(hex(84)), d_txt$(6%+kk%)         , ch(10),~
               at (11,15), fac(lfac$( 8%)), d_val$(6%+kk%,1%)   , ch(06),~
                                                                         ~
               at (11,27), fac(hex(84)), d_txt$(17%+kk%)        , ch(10),~
               at (11,40), fac(lfac$(19%)), d_val$(17%+kk%,1%)  , ch(06),~
                                                                         ~
               at (11,52), fac(hex(84)), d_txt$(28%+kk%)        , ch(10),~
               at (11,65), fac(lfac$(30%)), d_val$(28%+kk%,1%)  , ch(06),~
                                                                         ~
               at (12,02), fac(hex(84)), d_txt$(7%+kk%)         , ch(10),~
               at (12,15), fac(lfac$( 9%)), d_val$(7%+kk%,1%)   , ch(06),~
                                                                         ~
               at (12,27), fac(hex(84)), d_txt$(18%+kk%)        , ch(10),~
               at (12,40), fac(lfac$(20%)), d_val$(18%+kk%,1%)  , ch(06),~
                                                                         ~
               at (12,52), fac(hex(84)), d_txt$(29%+kk%)        , ch(10),~
               at (12,65), fac(lfac$(31%)), d_val$(29%+kk%,1%)  , ch(06),~
                                                                         ~
               at (13,02), fac(hex(84)), d_txt$(8%+kk%)         , ch(10),~
               at (13,15), fac(lfac$(10%)), d_val$(8%+kk%,1%)   , ch(06),~
                                                                         ~
               at (13,27), fac(hex(84)), d_txt$(19%+kk%)        , ch(10),~
               at (13,40), fac(lfac$(21%)), d_val$(19%+kk%,1%)  , ch(06),~
                                                                         ~
               at (13,52), fac(hex(84)), d_txt$(30%+kk%)        , ch(10),~
               at (13,65), fac(lfac$(32%)), d_val$(30%+kk%,1%)  , ch(06),~
                                                                         ~
               at (14,02), fac(hex(84)), d_txt$(9%+kk%)         , ch(10),~
               at (14,15), fac(lfac$(11%)), d_val$(9%+kk%,1%)   , ch(06),~
                                                                         ~
               at (14,27), fac(hex(84)), d_txt$(20%+kk%)        , ch(10),~
               at (14,40), fac(lfac$(22%)), d_val$(20%+kk%,1%)  , ch(06),~
                                                                         ~
               at (14,52), fac(hex(84)), d_txt$(31%+kk%)        , ch(10),~
               at (14,65), fac(lfac$(33%)), d_val$(31%+kk%,1%)  , ch(06),~
                                                                         ~
               at (15,02), fac(hex(84)), d_txt$(10%+kk%)        , ch(10),~
               at (15,15), fac(lfac$(12%)), d_val$(10%+kk%,1%)  , ch(06),~
                                                                         ~
               at (15,27), fac(hex(84)), d_txt$(21%+kk%)        , ch(10),~
               at (15,40), fac(lfac$(23%)), d_val$(21%+kk%,1%)  , ch(06),~
                                                                         ~
               at (15,52), fac(hex(84)), d_txt$(32%+kk%)        , ch(10),~
               at (15,65), fac(lfac$(34%)), d_val$(32%+kk%,1%)  , ch(06),~
                                                                         ~
               at (16,02), fac(hex(84)), d_txt$(11%+kk%)        , ch(10),~
               at (16,15), fac(lfac$(13%)), d_val$(11%+kk%,1%)  , ch(06),~
                                                                         ~
               at (16,27), fac(hex(84)), d_txt$(22%+kk%)        , ch(10),~
               at (16,40), fac(lfac$(24%)), d_val$(22%+kk%,1%)  , ch(06),~
                                                                         ~
               at (16,52), fac(hex(84)), d_txt$(33%+kk%)        , ch(10),~
               at (16,65), fac(lfac$(35%)), d_val$(33%+kk%,1%)  , ch(06),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then goto L43030            /* FIRST    */
L43060:           kk% = 0%
                  goto L43020

L43030:        if keyhit% <> 3% then goto L43040            /* LAST     */
L43090:           x% = int(p_no% / 33%)
                  kk% = (x% * 33%)
                  goto L43020 

L43040:        if keyhit% <> 6% then goto L43050            /* PREVIOUS */
                  if fieldnr% = 1% then goto L43050       /* NOT APPLIC */
                  if kk% < 34% then goto L43060
                  kk% = kk% - 33%
                  if kk% <= 1% then goto L43060
                  goto L43020

L43050:        if keyhit% <> 5% then goto L43070            /* NEXT     */
                  kk% = kk% + 33%
                  if kk% < p_no% then goto L43020
                  goto L43090

L43070:        if keyhit% <> 15% then goto L43080
                  call "PRNTSCRN"
                  goto L43020

L43080:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf3
        if edit% = 2% then L43100     /*  Input Mode             */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (14)Print report"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(4)Previous Fld                         " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ff0304ffffffffffffff0c0d0e0f10000000140000)

            if fieldnr% = 1% then L43140
            pf$(1%) = "(1)Start Over        (4)Previous Fld    " &       ~
                      "                                       "
            pf$(2%) = "(2)First Page        (5)Next Page       " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(3)Last Page         (6)Previous Page   " &       ~
                      "                                       "

            pfkeys$ = hex(010203040506ffffffffff0c0dff0f10000000140000)

L43140:     if fieldnr% > 1% then L43110
                str(pf$(3%),1%,16%) = " " : str(pfkeys$, 4%,1%) = hex(ff)
        return
L43110:     if fieldnr% < 5% then goto L43120
               str(pfkeys$,20%,1%) = hex(ff)
               str(pfkeys$,12%,1%) = hex(ff)
               str(pfkeys$,13%,1%) = hex(ff)
L43120:     gosub check_screen1
        return

L43100: if fieldnr% > 0% then L43130  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Update Data "
            pfkeys$ = hex(01ff03ffffffffffffffffffffff0f1000)
            return

L43130:
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                                       "
            pf$(2%) = "                                        " &       ~
                      "                                       "
            pf$(3%) = "                                        " &       ~
                      "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        check_screen1
            if p_no% > 33% then goto L43150
               gosub no_first1
               gosub no_next1
               gosub no_last1
               gosub no_prev1
               return
L43150:      if kk% >= 33% then goto L43160
                gosub no_first
                gosub no_prev
L43160:      if (kk% + 33%) < p_no% then goto L43170
                gosub no_last
L43170:      if kk% < (p_no% - 33%) then goto L43180
                gosub no_next
L43180: return
        no_first1
            str(pf$(2%),1%,14%)  = " " : str(pfkeys$,2%,1%) = hex(ff)
        return
        no_next1
            str(pf$(2%),22%,14%) = " " : str(pfkeys$,5%,1%) = hex(ff)
        return
        no_last1
            str(pf$(3%),1%,14%)  = " " : str(pfkeys$,3%,1%) = hex(ff)
        return
        no_prev1
            str(pf$(3%),22%,18%) = " " : str(pfkeys$,6%,1%) = hex(ff)
        return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************
        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50000,         /* Production Week       */ ~
                              L50100,         /* Production Day        */ ~
                              L50200,         /* Department Code       */ ~
                              L50300          /* Daily Unit Values     */
            return
L50000: REM Production Week                       MT_WK$
        init(" ") cur_yr$, cur_wk$, cur_dy$, cur_dte$, cur_date$,     ~
                  ent_yr$, ent_wk$, ent_dy$, ent_dte$, ent_date$,     ~
                  cur_yr_bi$, ent_yr_bi$, prv_yr_bi$

        if sc_yr$ = " " then goto L50010
           ent_yr$ = sc_yr$
           convert ent_yr$ to temp%, data goto L50560

           ent_yr_bi$ = bin(temp%, 2)
L50010:    if sc_wk$  <> " " then ent_wk$ = sc_wk$
           if sc_day$ <> " " then ent_dy$ = sc_day$

           call "AWDPLN0B" ( cur_yr_bi$, /* Current Production Year    */~
                             cur_wk$,    /* Current Production Week    */~
                             cur_dy$,    /* Current Production Day     */~
                             cur_dte$,   /* Current Production Date(6) */~
                             cur_date$,  /* Current Production Date(8) */~
                             ent_yr_bi$, /* Entry Production Year (IN) */~
                             ent_wk$,    /* Entry Prod Week       (IN) */~
                             ent_dy$,    /* Entry Production Day (OPT) */~
                             ent_dte$,   /* Entry Production Date (6)  */~
                             ent_date$,  /* Entry Production Date *8)  */~
                             prv_yr_bi$, /* Previous Year              */~
                             #2,         /* GENCODES                   */~
                             pl_e%    )  /* 0% = No, 1% = Found        */

            if pl_e% <> 0% then goto L50560

            temp% = val(cur_yr_bi$,2)
            convert temp% to cur_yr$, pic (####)
/* (AWD016) */
            cur_yr% = 0%
            cur_yr% = temp%            
            temp% = val(ent_yr_bi$,2)
            convert temp% to ent_yr$, pic (####)            
            temp% = val(prv_yr_bi$,2)
            convert temp% to prv_yr$, pic (####)

            sc_wk_dte$ = ent_dte$   
            sc_yr$     = ent_yr$    /* Save all screen dates */
            sv_yr$     = ent_yr$
            sv_wk$     = ent_wk$
            sv_day$    = ent_dy$
            wk% = 0%
            convert sv_wk$ to wk%, data goto L50560

            convert sv_day$ to i%, data goto L50560

           eff_dte$ = sc_wk_dte$
           sc_wk$   = sv_wk$
           sc_day$  = sv_day$
           call "DATEFMT" (sc_wk_dte$)

           ex_year$ = sc_yr$
           month$ = str(sc_wk_dte$,1%,2%)   /* Get current Screen Month */
           convert month$ to month%, data goto L50560

           convert ex_year$ to ex_year%, data goto L50550

           month% = month% - 1%
           if month% = 0% then ex_year% = ex_year% - 1%
           if month% = 0% then month% = 12%
           if month% = 11% and wk% = 1% then ex_year% = ex_year% - 1%
           convert ex_year% to ex_year$, pic(0000)
/* (AWD016) */
           gosub get_emp_day
/* (\AWD016) */           
           
           if sc_yr$ > cur_yr$ then goto L50580
           if sc_yr$ < cur_yr$ then goto L50550
           if sc_wk$ > cur_wk$ then goto L50570
L50550: return
L50560:    errormsg$ = "(Error) - Invalid Production Week (1 thru 52)?"
           init(" ") sc_wk_dte$, sc_wk$, sc_day$, days$, sc_yr$
        return
L50570:    errormsg$ = "(Error) - Future Production Week,Less than Equal ~
        ~to ("& cur_wk$ & ")"
           init(" ") sc_wk_dte$, sc_wk$, sc_day$, days$, sc_yr$
        return
L50580:    errormsg$ = "(Error) - Future Production Year,Less than Equal ~
        ~to ("& cur_yr$ & ")"
           init(" ") sc_wk_dte$, sc_wk$, sc_day$, days$, sc_yr$
        return
L50100: REM Production Day
           if sc_day$ <> " " then goto L50110
              sc_day$ = sv_day$

L50110:    sv_day$ = sc_day$
           convert sc_day$ to x%, data goto L50120

           convert cur_dy$ to y%, data goto L50120

           if x% < 1% or x% > 7% then goto L50120

           if sc_yr$ < cur_yr$ then goto L50130
           if sc_wk$ <> cur_wk$ then goto L50130
           if x% > y% then goto L50140

L50130:    days$ = days$(x%)
           sc_day% = x%
           gosub get_production_date
           i% = x%
           gosub get_emp_day 
          
        return
L50120:    errormsg$ = "(Error) - Invalid Production Day (1 thru 7)?"
           init(" ") sc_day$, days$
        return
L50140:    errormsg$ = "(Error) - Future Production Day,Less than Equal t~
        ~o (" & sv_day$ & ")"
           init(" ") sc_day$, days$
        return
L50200: REM Department Code                       SC_DEPT$
            if sc_dept$ <> " " then goto L50210
               goto L50230
L50210:     convert sc_dept$ to x%, data goto L50230

            convert x% to sc_dept$, pic(000)
           
            send_dept$ = sc_dept$           /* RDB sc_dept is 3 chars now */

            init(" ") readkey$, sc_dept_d$
            str(readkey$,1%,9%)   = "EMP DEPT "
            str(readkey$,10%,15%) = send_dept$
            read #2,key = readkey$, using L50240, sc_dept_d$,             ~
                                                           eod goto L50230
L50240:        FMT POS(25), CH(30)
REM - Set screen description based on Department
            sc_desc$ = "Models       Units    Sm/Dp    Sash     Parts" & ~
                       "                                  "
            m% = 4%
            if schema% <> 1% then goto notNCDesc
            if sc_dept$ = "037" then gosub change_desc
            if sc_dept$ = "034" then gosub change_desc
            if sc_dept$ = "041" then gosub change_desc
            if sc_dept$ = "038" then gosub change_desc /* (AWD013) */
            if sc_dept$ = "043" then gosub change_desc_stk
            if sc_dept$ = "000" then gosub change_desc_scr
            if sc_dept$ = "029" then gosub change_desc_scr_asm  /* (AWD014) */
                goto DescChanged   
notNCDesc:          
            if sc_dept$ = "067" then gosub change_desc
            if sc_dept$ = "068" then gosub change_desc
            if sc_dept$ = "074" then gosub change_desc
            if sc_dept$ = "075" then gosub change_desc /* (AWD013) */
            if sc_dept$ = "000" then gosub change_desc_scr
DescChanged:
            eff_proc$   = "P"
            eff_year$   = sc_yr$
            eff_wk$     = sc_wk$
            eff_day$    = sc_day$
            eff_shift$  = "00"
            gosub dataload             /* Try to Load Data */
            gosub load_prod_indirect   /* Load Models by Dept */
            if load% = 1% then return  /* If load do not recalc unts */
            gosub calc_scanned_units
           
            if (sc_dept$ = "037" or sc_dept$ = "034")             ~
               and schema% = 1% then gosub glass_analysis
               
            if (sc_dept$ = "074" or sc_dept$ = "075")             ~
               and schema% = 1% then gosub glass_analysis               
            if sc_dept$ = "022" then auto_current% = 1% 
        return
L50230:     errormsg$ = "(Error) - Invalid Department Code?"
            sc_dept$, sc_dept_d$ = " "
        return
        change_desc
            m% = 1%
            sc_desc$ = "Models       Units                         " & ~
                       "                                    "
        return
        change_desc_stk
            m% = 7%                                      /*   (EWD0002)  */
            sc_desc$ = "Models       Units    Sm/Dp    Sash     Parts" & ~
                       "    FG/STK   BLD OF   Pull OF     "
        return
        change_desc_scr
            m% = 5%
            sc_desc$ = "Models       Scanned  Full/Sc  Half/Sc  Csmts" & ~
                       "    Parts                         "
        return
        change_desc_scr_asm
            m% = 1%                                      /*   (AWD014)  */
            sc_desc$ = "Models       Screens"
        return
L50300: REM Unit Values                                D_VAL$()
         mat effect_unt = zer
         eff_col1  = 0.0
         for i% = 1% to p_no%
         eff_scan = 0.0      
          for ii% = 1% to m%
            d_val(i%,ii%) = 0
            convert d_val$(i%,ii%) to d_val(i%,ii%), data goto L50310
L50310:     
REM - Actual Scanned Units
            convert d_val(i%,ii%) to d_val$(i%,ii%), pic(######)
            eff_scan = eff_scan + d_val(i%,ii%)
          next ii%
          gosub convert_effective_unt
          if indirect% <> 1% and eff_scan = 0 then d_txt$(i%) = "   "
         next i%
         if comp_calc% = 1% then return
         gosub convert_product
         if load% = 1% then goto L50320
         if auto_wages% = 1% then return
         
            err% = 0%
            eff_col2, eff_col3, eff_col4, eff_col8 = 0.0
            mat ef     = zer : mat ef_ov = zer         /* indt()     */
            mat tot_ef = zer : mat wg    = zer

            e_shift$ = "00"
            eff% = 0%
/* (AWD016) change to emp_ variables */            
            call "APCEFFWG" (send_dept$, emp_yr$, emp_wk$, emp_day$, ef(), ~
                             ef_ov(), tot_ef(), wg(), e_shift$, #4, #3, ~
                             #11, #15, eff%, err% )
            if err% = 0% then goto L50330
               errormsg$ = "(Error) - Department Hours Must be Fixed ?"
               return                    /* DAILY VALUES GIVEN         */
                                         /* Daily Given and Items      */
L50330:      
/* (AWD016) change from sc_day% to emp_day% */
             eff_col2  = ef(emp_day%)       /* EFF REG HOURS  */
             eff_col3  = ef_ov(emp_day%)    /* EFF OVER HOURS */
             eff_col4  = tot_ef(emp_day%)   /* TOTAL EFF HOURS*/
             eff_col8  = wg(emp_day%)       /* TOTAL WAGES    */
REM - ADD MANAGERS PAY, AND INCENTITIVES
             gosub get_pay
             eff_col2  = eff_col2 + mgr_hrs
             eff_col4  = eff_col4 + mgr_hrs
             eff_col8  = eff_col8 + mgr_pay + mgr_inct

             load% = 1%                   /* Set load% = 1% so will     */
L50320:                                   /* convert screen values.     */

                                  /* CALC TOTALS  For Departments       */
          eff_col5, eff_col6, eff_col7, eff_col9, eff_col13 = 0.0
          call "SHOSTAT" ("Calculating Dept. " & sc_dept$)

          init(" ") uc_key$             /* Read planning file for actual    */
          mat pl_unta% = zer            /* planned unit for a specified day */
          str(uc_key$,1%,2%) =  ent_yr_bi$
          str(uc_key$,3%,2%) =  ent_wk$
          str(uc_key$,5%,2%) =  "01"
          str(uc_key$,7%,3%) =  sc_dept$
          str(uc_key$,10%,2%) = "01"
          read #12, key = uc_key$, eod goto pl_done

           get #12, using L50340, pl_unta%()             
L50340:        FMT POS(87), 7*BI(2)

          eff_col6 = pl_unta%(sc_day%)
          if sc_dept$ = "001" then eff_col6 = 0.0
        pl_done
        return
/* (AWD016) */
        get_emp_day
           emp_day%, emp_wk%, emp_yr% = 0%
           convert sc_day$ to emp_day%, data goto badSCRDay
           
badSCRDAY: convert sc_wk$ to emp_wk%, data goto badSCRWk

badSCRWk:  convert sc_yr$ to emp_yr%, data goto badSCRYr

badSCRYr:
/* (CR1489) */
           convert emp_day% to emp_day$, pic(0)
           
           convert emp_wk% to emp_wk$, pic(#0)
           
           convert emp_yr% to emp_yr$, pic(0000)
           
        return
/* (CR1489) END */
REM           emp_day% = emp_day% - 1%
REM           if emp_day% <> 0% then goto NOT_END_WK           
REM              emp_wk% = emp_wk% - 1%
REM              if emp_wk% <> 0% then goto NOT_END_YR
REM                 emp_wk% = 52%  /* Week was 0% */
REM                 emp_yr% = emp_yr% - 1%
REM NOT_END_YR              
REM              emp_day% = 7%       /* Day was 0% */
REM NOT_END_WK


           emp_day% = emp_day% + 1%
           if emp_day% <> 8% then goto NOT_END_WK
              emp_wk% = emp_wk% + 1%
              if emp_wk% <> 53% then goto NOT_END_YR
                 emp_wk% = 52%  /* Week was 53% */
                 emp_yr% = emp_yr% + 1%
NOT_END_YR:
              emp_day% = 1%       /* Day was 8% */
NOT_END_WK:
           convert emp_day% to emp_day$, pic(0)
           
           convert emp_wk% to emp_wk$, pic(#0)
           
           convert emp_yr% to emp_yr$, pic(0000)
        return                    
        REM *************************************************************~
            *                 R E P O R T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on REPORT 1.                      *~
            *************************************************************
        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50000,         /* Production Week       */ ~
                              L50100          /* Production Day        */ 
            return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 3.                      *~
            *************************************************************
        deffn'153(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50000,         /* Production Week       */ ~
                              L50500,         /* Production Day        */ ~
                              L50600          /* Department Codes      */ 
            return

L50500: REM Production Day
        gosub L50100
        eff_proc$ = "S"
        eff_year$ = sc_yr$
        eff_wk$   = sc_wk$
        eff_day$  = sc_day$ 
        eff_shift$  = "00"
        gosub load_departments
        for i% = 1% to p_no%   
           sc_dept$ = d_txt$(i%)  
           gosub dataload     
           gosub read_scan_scrap       /* CR1240  + */
           if eff_col2 = 0 or load% = 0% then                      ~
              convert sp_wgt to d_val$(i%,1%), pic(#####0)         ~   
           else                                                    ~
              convert eff_col2 to d_val$(i%,1%), pic(#####0)  
                                       /* CR1240  - */              
           call "STRING" addr("LJ", d_val$(i%,1%), 6%)
        next i%
        if d_txt$(1) > " " then ~
          gosub L50600          /* CR1240 Auto save scan scrap to eff file */
        return
L50600: REM All Department Codes to put Scrap in all at once
         for z% = 1% to p_no%
             d_val(z%,1%) = 0.0
             convert d_val$(z%,1%) to d_val(z%,1%), data goto L50610
         
             convert d_val(z%,1%) to d_val$(z%,1%), pic(####0-)
             call "STRING" addr("LJ", d_val$(z%,1%), 6%)
             
             sc_dept$ = str(d_txt$(z%),1%,3%)     /* Eff. Depts   */
             eff_col2 = d_val(z%,1%)
             gosub dataput
         next z%
        return
L50610:   scrap$ = " " 
          errormsg$ = "(Error) - Invalid Scrap Value? "
        return

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************
        clear_rec
             eff_col1, eff_col5, eff_col6, eff_col7, eff_col9,       ~
             eff_col10, eff_col11, eff_col12, eff_col13, eff_hrsm,   ~
             eff_paym, eff_scan, eff_hrse, eff_value    = 0.0
                                                 /* (AWD0009)  */
            mat eff_unt = zer    :  mat eff_unts = zer
            mat eff_untss = zer  :  mat eff_untpp = zer
            mat eff_unta = zer   :  mat eff_untb = zer
            mat eff_untc = zer
        return    
        delete_record
          call "SHOSTAT" ("Deleting Record ")
          init(" ") eff_key$
          str(eff_key$,1%,1%)  = eff_proc$
          str(eff_key$,2%,4%)  = eff_year$
          str(eff_key$,6%,2%)  = sc_wk$
          str(eff_key$,8%,1%)  = sc_day$
          str(eff_key$,9%,3%)  = sc_dept$
          str(eff_key$,12%,2%) = eff_shift$     /*  Eff Shift - Set to 00 */

          read #1,hold,key = eff_key$, eod goto L01335

              delete #1
L01335: return clear all
        goto inputmode

        load_prod
           if sc_dept$ = "044" then goto load_prod_wood
           if sc_dept$ = "054" then goto load_prod_wood   /* (EWD0007) */
           if sc_dept$ = "074" then goto load_prod_wood   /* (AWD011) */
           call "SHOSTAT" ("Loading Products for Dept ("&sc_dept$&")" )
           init(" ")  dt_key$                      /* Department Spec. */
           str(dt_key$,1%,3%) = sc_dept$           /* by the Call Prog.*/
           str(dt_key$,4%,2%) = "01"               /* Set Process Code */
           read #7,key > dt_key$, using L01330 , dt_key$,       ~
                                     eod goto L01450
           goto L01340
	        
        load_prod_nxt
           read #7, using L01330 , dt_key$, eod goto L01450
L01330:        FMT POS(11), CH(12)
L01340:    if str(dt_key$,1%,3%) <> sc_dept$ then goto L01450
           if str(dt_key$,8%,3%) = "000" then goto load_prod_nxt
           if p_no% = 0% then goto L01400
              for j% = 1% to p_no%
                  if d_txt$(j%) = str(dt_key$,8%,3%) then                ~
                                                     goto load_prod_nxt
              next j%
L01400:       p_no% = p_no% + 1%
              j% = p_no%
              if p_no% >= (max_product% -1%) then p_no% = max_product%
              d_txt$(j%) = str(dt_key$,8%,3%)
             goto load_prod_nxt
L01450:  for j% = 1% to p_no%
             if d_txt$(j%) = "999" then goto L01410
         next j%
             p_no% = p_no% + 1%             /* Undefined Product Bucket     */
             d_txt$(p_no%) = "999"          /* for units for no found model */
L01410: 

          convert p_no% to p_no$, pic(####)

           call "SHOSTAT"("P_NO -->   " & p_no$) 
REM stop
/* (AWD012) */
           goto load_prod_sc

       return


        load_prod_sc
           call "SHOSTAT" ("Loading Products for Dept ("&sc_dept$&")" )
           init(" ") readkey$, descr$
           str(readkey$,1%,9%)  = "APC EFFSC"
           str(readkey$,10%,3%) = sc_dept$
           str(savekey$,1%,12%) = readkey$           
        load_prod_sc_next
           read #2, key > readkey$, using L01480, readkey$,descr$,~
                                         eod goto load_sc_done

                   if str(readkey$,1,12) <> str(savekey$,1,12) ~
                                     then goto load_sc_done

                   if p_no% = 0% then goto load_first
                   for j% = 1% to p_no%
                       if d_txt$(j%) = str(readkey$,13%,3%) then     ~
                                           goto load_prod_sc_next
                   next j%

load_first:
                   p_no% = p_no% + 1%
                   if p_no% >= (max_product% -1%) then p_no% = max_product%
                   d_txt$(p_no%) = str(readkey$,13,3)
                         goto load_prod_sc_next
         load_sc_done
             for j% = 1% to p_no%
                 if d_txt$(j%) = "999" then goto L01415
             next j%
             p_no% = p_no% + 1%             /* Undefined Product Bucket     */
             d_txt$(p_no%) = "999"          /* for units for no found model */
L01415: 

             convert p_no% to p_no$, pic(####)
 
             call "SHOSTAT"("P_NO -->   " & p_no$) 
       return
                   

        load_prod_indirect
          kk% = 0%
          for j% = 1% to 40%            /* set p_no back to zero so will put  */
              if sc_dept$ = ind_dept$(j%) then p_no% = 0%  /* right models on */
          next j%                       /* the screen starting back at 1      */
          indirect% = 0%
          init(" ") readkey$, descr$
          str(readkey$,1%,9%)  = "APC EFFID"
          str(readkey$,10%,3%) = sc_dept$
          str(savekey$,1%,12%) = readkey$
        read_prod_table_nxt
          read #2, key > readkey$, using L01480, readkey$, descr$,  ~
                                   eod goto L01490
L01480:            FMT CH(24), CH(30)
          if str(readkey$,1%,12%) <> savekey$ then goto L01490
          if str(readkey$,10%,3%) = "043" then goto L01490
          indirect% = 1%
          p_no% = p_no% + 1%
          d_txt$(p_no%) = descr$

          eff_model$(p_no%) = str(readkey$,13%,3%)  
           goto read_prod_table_nxt
L01490: if p_no% = 0% then goto load_prod /* if none found go load from planning */
        if indirect% = 0% and load% = 1% then goto load_prod
        return

        load_departments                 /* Load all depts for scrap screen */
           call "SHOSTAT" ("Loading All Departments" )  /* that HAVE been process */
           init(" ") readkey$, sc_dept_d$, d_txt$(), eff_sav$ /* for production  */
           p_no%, j% = 0%
            str(readkey$,1%,1%) = "P"
            str(readkey$,2%,4%) = sc_yr$
            str(readkey$,6%,2%) = sc_wk$
            str(readkey$,8%,1%) = sc_day$
            str(eff_sav$,1%,8%) = readkey$
           read #1,key > readkey$, using L01500, readkey$, eod goto L01540
L01500:        FMT XX(6), CH(13)

           goto L01510
        load_dept_nxt
           read #1, using L01500, readkey$, eod goto L01540

L01510:    if str(readkey$,1%,8%) <> str(eff_sav$,1%,8%) then goto L01540

              p_no% = p_no% + 1%
              j% = p_no%
REM              if p_no% > max_product% then p_no% = max_product%
              d_txt$(j%) = str(readkey$,9%,3%)
              goto load_dept_nxt
L01540: return

        load_prod_wood   /* Only for Dept '044' load models from table */
          p_no% = 0%
          init(" ") readkey$
          str(readkey$,1%,9%)  = "APC WOOD"
          str(readkey$,10%,3%) = "A  "
          str(savekey$,1%,9%) = readkey$
        read_prod_wood_nxt
          read #2, key > readkey$, using L01485, readkey$,          ~
                                   eod goto L01560
L01485:            FMT CH(24)
          if str(readkey$,1%,9%) <> str(savekey$,1%,9%) then goto L01560
          if str(readkey$,11%,2%) = "00" then goto read_prod_wood_nxt
          if p_no% = 0% then goto L01550
          for j% = 1% to p_no%
              if d_txt$(j%) = str(readkey$,10%,3%) then                ~
                                               goto read_prod_wood_nxt
          next j%
L01550:   p_no% = p_no% + 1%
          d_txt$(p_no%) = str(readkey$,10%,3%)
          eff_model$(p_no%) = str(readkey$,10%,3%)  
           goto read_prod_wood_nxt
L01560: return

       convert_product
         init(" ") eff_model$()
         cnt% = 1%
         for p% = 1% to p_no%
              if d_txt$(p%) = "   " then goto L51130
              if cnt% > 100% then cnt% = 100%
              convert cnt% to cnt$, pic(000)
                 eff_model$(cnt%) = d_txt$(p%) 
                 if indirect% = 1% then eff_model$(cnt%) = cnt$
                     eff_unt(cnt%)   = d_val(p%,1%)
                     eff_unts(cnt%)  = d_val(p%,2%)
                     eff_untss(cnt%) = d_val(p%,3%)
                     eff_untpp(cnt%) = d_val(p%,4%)
                     eff_unta(cnt%) = d_val(p%,5%)
                     eff_untb(cnt%) = d_val(p%,6%)
                     eff_untc(cnt%) = d_val(p%,7%)
               cnt% = cnt% + 1%
L51130:  next p%
       return

       convert_effective_unt       /*  Calculate Effective Units  */
          if sc_dept$ = "000" then goto convert_effective_screen
                                              /* (AWD014) */
          if sc_dept$ = "029" and schema% = 1% then goto convert_effective_asm 
          sash, part, stock, bld_of, pull_of = 0.0
          if d_val(i%,3%) < .01 then goto no_sas
             sash = round(d_val(i%,3%) / 3, 1)
no_sas:   if d_val(i%,4%) < .01 then goto no_par
             part = round(d_val(i%,4%) / 5, 1)
no_par:   if m% <> 7% then goto no_stk
REM          if d_val(i%,5%) < .01 then goto no_stk
             stock  = round(d_val(i%,5%) / 9.09, 1)
             bld_of = round(d_val(i%,6%) * .4,1)       /* (EWD0002) */
             pull_of = round(d_val(i%,7%) * .6,1)      /* (EWD0002) */
no_stk:   effect_unt(i%) = d_val(i%,1%) + d_val(i%,2%) + sash + part
          effect_unt(i%) = effect_unt(i%) + stock + bld_of + pull_of
          eff_col1 = eff_col1 + d_val(i%,1%) + d_val(i%,2%)
          eff_col1 = eff_col1 + sash + part 
          eff_col1 = eff_col1 + stock + bld_of + pull_of /* (EWD0002) */
        return
        convert_effective_screen
          part, wscr, half = 0.0
          if d_val(i%,3%) < .01 then goto half_sc
             half = round(d_val(i%,3%) / 0.75, 1)
half_sc:  if d_val(i%,4%) < .01 then goto with_sc
             wscr = round(d_val(i%,4%) / 0.75, 1)
with_sc:  if d_val(i%,5%) < .01 then goto part_sc
             part = round(d_val(i%,5%) / 5, 1)
part_sc:   effect_unt(i%) = d_val(i%,1%) + d_val(i%,2%) + half + wscr + part
           eff_col1 = eff_col1 + d_val(i%,1%) + d_val(i%,2%) + half
           eff_col1 = eff_col1 + wscr + part
        return

/* (AWD014) */
        convert_effective_asm
          scr, value = 0.00
          value = 1.00
          if i% = 2% or i% = 4% then value = 1.40
          if i% = 5% then value = 1.25
          if i% = 6% then value = 2.3   /* (AWD019) */
          if i% = 7% then value = 0.5
          if i% = 8% then value = 3.4   /* (AWD019) */
          if i% = 9% then value = 2.49   /* CR 986 PET STOCK  */
          if i% = 10% then value = 3.60  /* CR 986 PET HD */
          if i% = 11% then value = 1.36  /* CR1133 new half wire mesh */
          if i% = 12% then value = 2.16  /* CR1133 new full wire mesh */          
          if i% = 13% then value = 2.07  /* CR1133 new casement wire mesh */
          
          if d_val(i%,1%) < .01 then goto no_screens
             scr = round(d_val(i%,1%) * value, 1)
no_screens:  

          effect_unt(i%) = d_val(i%,1%) + scr

           eff_col1 = eff_col1 + scr
        return
/* (AWD014) */
          
        complete_calc
            init(" ") message$
            str(message$,1,6) = "YEAR"
            str(message$,7,4) = "WK"
            str(message$,11,3) = "DY"
            str(message$,14,5) = "DPT"
            str(message$,19,5) = "MDL"
            str(message$,24,4) = "MN"
            str(message$,28,14) = "AVG PRICE"
            str(message$,44,10)  = "UNIT"
            str(message$,56,15) = "COL 12"
REM            call "LOGFILE" (message$)
            
            init(" ") readkey1$
            comp_calc% = 1% : tot_wg = 0.0
            str(readkey1$,1%,9%)   = "APC EFFDP"       /* Get Dept */
            str(readkey1$,10%,13%) = "   "
        complete_calc_nxt
            mat d_val = zer : init(" ") d_val$()
            gosub clear_rec
            gosub get_dept
            if sc_dept$ = "  " then goto L50700
            cnt% = 0% 
            eff_proc$ = "P"
            gosub dataload
REM            eff_col5, eff_col10, eff_col11, eff_col12, eff_hrse = 0.0
            eff_col5, eff_col11, eff_col12, eff_hrse = 0.0
            eff_col9, eff_col13, in_process, matrl_prod = 0.0
            mistake_weight, sav_weight, avg_weight, eff_col7 = 0.0
            eff_lbs, eff_matrl  = 0.00                         /*  (AWD0010)  */
            mat eff_lbs = zer
            if load% = 0% then goto complete_calc_nxt
            tot_wg = tot_wg + eff_col8                /* tot_wg for eff. %            */
            gosub L50300                              /* add up units & calc effect_unt */
            if sc_dept$ = "037" then gosub get_glass  /* add remakes to eff_col1      */
            if sc_dept$ = "034" then goto L50815      /* do not calc remakes dept     */
REM         for q% = 1% to 20%
REM             if ind_dept$(q%) = sc_dept$ then goto L50760
REM         next q%
REM - Finish Production Calculations
            for q% = 1% to p_no%
              eff_unit$ = "01"
              model$ = eff_model$(q%)
              if sc_dept$ = "000" then eff_unit$ = "17"   /*  !!!! MAKE SURE UNIT = 19 !!! */
                                                          /*  !!!! MAKE SURE UNIT = 19 !!! */
              if sc_dept$ = "029" and schema% = 1% then eff_unit$ = "17"   
              gosub read_extra                             /* effect unt */
              gosub calc_eff
              avg_weight = avg_weight + eff_scrapa(month%) /* scrap - col4       */
              cnt% = cnt% + 1%                             /* production - col12 */  
              gosub avg_price

/* Scrap -- matrl_prod, in_process, mistake_weight  */
              matrl_prod = matrl_prod + (eff_scrapa(month%) *    ~
                                          effect_unt(q%))
              in_process = in_process + (eff_scrapb(month%) *    ~
                                          effect_unt(q%))
/* (EWD0001)  */
REM           mistake_weight = (mistake_weight + (effect_unt(q%) *     ~
REM                           (eff_scrapa(month%) + eff_scrapb(month%))))
              gosub calc_mistake   /* (EWD0001)  */

              REM eff_col10 = eff_labor(month%)
              if eff_labor(month%) > 0.00  then eff_col10 = eff_labor(month%)
              if eff_lbs(month%)   > 0.00  then eff_lbs   = eff_lbs(month%)
              if eff_matrl(month%) > 0.00  then eff_matrl = eff_matrl(month%)
                                              /* (AWD0010) - Add eff_lbs  */
            next q%



              if eff_col4 < .01 then goto L50750
/* Production -- Eff %, act unt/hrs, lab$ goal/unt, lab$/unt, var lab$, & lab% of Prod$ */
              eff_col7 = round((eff_hrse / eff_col4) * 100, 4)
                                                      /*  (AWD0008) BEG */
REM              eff_col5  = round((eff_col3 / eff_col1) * 100, 1) 
              eff_col5  = round((eff_col3 / eff_col4) * 100, 1) 

                                                      /*  (AWD0008) END */
L50750:       
              eff_col9, eff_col11, eff_col13 = 0.0
              if eff_col1 < .01 then goto L50800
              eff_col9  = round(eff_col8 / eff_col1, 4)
L50800:       eff_col11 = round((eff_col10 - eff_col9) * eff_col1, 4)
              if eff_col12 < .01 then goto L50810
              eff_col13 = round((eff_col8 / eff_col12) * 100, 4)
L50810:       if sc_dept$ = "037" and schema% = 1%              ~
                           then eff_col1 = eff_col1 - ex_col1
              if sc_dept$ = "037" and schema% = 1%              ~
                           then gosub take_glass_away
L50815:       gosub dataputrewrite
              eff_col2, eff_col3, eff_col4, eff_col5, eff_col6, eff_col7,~
              eff_col8, eff_col9, eff_col10, eff_col11, eff_col12,       ~
              eff_col13, eff_avg, sav_col1 = 0.0 
              sav_col1 = eff_col1
              sav_weight = mistake_weight
              eff_proc$ = "S"
              gosub dataload
REM              if load% = 0% then goto complete_calc_nxt
              eff_col1 = sav_col1
              mistake_weight = sav_weight
REM              if sc_dept$ = "034" then mistake_weight = 0.0
REM              if sc_dept$ <> "000" then goto not_000
REM                   call "SHOSTAT" ( " I AM HERE " )  stop
REM                   convert eff_lbs(month%) to cmg$, pic(-#######.00##)
REM                   call "SHOSTAT" ( " I AM HERE " & cmg$ & ex_key$)  stop
REM                   convert month% to cmg$, pic(#)
REM                   call "SHOSTAT" ( " I AM HERE " & cmg$ & ex_key$)  stop
REM not_000
REM              eff_col11 = eff_lbs(month%)
              eff_col11 = eff_lbs                       /* (AWD0010) */

REM              eff_col13 = round(eff_col2 * eff_matrl(month%), 4)
              eff_col13 = round(eff_col2 * eff_matrl, 4)
              eff_col6 = matrl_prod
              eff_col7 = in_process
              eff_col8 = eff_col2 - eff_col7
              eff_col5 = eff_col6 + eff_col7 + eff_col8
              eff_avg = round(avg_weight / cnt%, 4)
              eff_col3, eff_col4, eff_col9, eff_col10, eff_col12 = 0.0
              if sc_dept$ = "037" and schema% = 1%             ~
                                  then gosub get_glass
              if sc_dept$ = "037" and schema% = 1%             ~
                                  then eff_col1 = eff_col1 + total_glass
              if eff_col1 < .01 then goto L50820
              eff_col3  = round(eff_col2 / eff_col1, 4)
              if sc_dept$ = "037" and schema% = 1%             ~
                                  then eff_col1 = eff_col1 - total_glass
              if sc_dept$ = "037" and schema% = 1%             ~
                                  then eff_col1 = eff_col1 - ex_col1
              if sc_dept$ = "037" and schema% = 1%             ~
                                  then gosub take_glass_away
L50820:       if mistake_weight < .01 then goto L50830
              eff_col9  = round((eff_col8 / mistake_weight) * 100, 4)
                                                    /*   (AWD0008)   -   BEG */
REM              eff_col10 = round((eff_col2 / mistake_weight) * 100, 4)                 
              eff_col10 = round((eff_col13 / eff_col1) * 100, 4)          
                                                    /*   (AWD0008)   -   END */
       
L50830:       eff_col12 = round(eff_col11 - eff_col10, 4)
              eff_col4  = round((eff_col11 / 100) * eff_avg, 4) 
              gosub dataputrewrite
REM L50760 :     goto complete_calc_nxt
L50700: gosub calc_indirect
        comp_calc% = 0%
        return clear all
        goto inputmode
		
/* CR3200 Program to move units per Gecnodes file and run shipping report */		
		move_units
		    call "EWDEFFMV" ("P",           /* Process Flag               */~
			                 sc_yr$,        /* Screen year                */~
							 sc_wk$,        /* Screen week                */~
							 sc_day$,       /* Screen day                 */~
							 pr_dte$,       /* Entry Production Date (6)  */~
                             #2,            /* (GENCDSIN) Master Code Tab */~
							 #1,            /* EWDEFFCY                   */~
							 #6)            /* Production Tracking Audit  */
							 
           gosub complete_calc							 
		return clear all
		goto inputmode

        calc_mistake                         /* (EWD0001)  */
         tot_mistake = 0.0
         tot_mistake = eff_scrapa(month%) + eff_scrapb(month%)
         mistake_weight = (mistake_weight + (eff_unt(q%) * tot_mistake))
         mistake_weight = (mistake_weight + (eff_unts(q%) * tot_mistake))
         mistake_weight = (mistake_weight + (eff_untss(q%) * tot_mistake))
         mistake_weight = (mistake_weight + (eff_untpp(q%) * tot_mistake))
         mistake_weight = (mistake_weight + (eff_unta(q%) * tot_mistake))
         mistake_weight = (mistake_weight + (eff_untb(q%) * tot_mistake))
         mistake_weight = (mistake_weight + (eff_untc(q%) * tot_mistake))
        return                                /* (EWD0001) */

        calc_indirect
         eff_proc$ = "P"
         for k% = 1% to 50%                /* CR3200 update */
          if auto_tab$(k%) = "  " then goto L50780
          sc_dept$ = "0"& auto_tab$(k%)
          gosub dataload
          if load% = 0% then goto L50780
          init(" ") ex_key$ :  eff_col7, lab_hrs = 0.0
          str(ex_key$,1%,4%) = ex_year$
          str(ex_key$,5%,3%) = sc_dept$
          str(ex_sav$,1%,7%) = str(ex_key$,1%,7%)
          read #13, key > ex_key$, using L50770, ex_key$, ex_upmh(), ~
                                   eod goto L50780
L50770:      FMT CH(12), POS(109), 12*PD(14,4)
            if str(ex_key$,1%,7%) <> str(ex_sav$,1%,7%) then goto L50780

            lab_hrs  = ((eff_col8 / tot_wg) * 100)   
            eff_col7 = ((ex_upmh(month%) / lab_hrs) * 100)
            gosub dataput
L50780:  next k%
        return

       get_glass
            ex_col1 = 0.0
            mat effect_rmk = zer  
            str(eff_key$,1%,1%) = "P"
            str(eff_key$,2%,4%) = sc_yr$
            str(eff_key$,6%,2%) = sc_wk$
            str(eff_key$,8%,1%) = sc_day$
            str(eff_key$,9%,3%) = "034"
            str(eff_key$,12%,2%) = "00"
            read #1, key = eff_key$, using L50840, effect_rmk(), ex_col1, ~
                                     eod goto L50850
L50840:     FMT POS(320), 100*BI(2), POS(1720), PD(14,4)
              eff_col1 = eff_col1 + ex_col1
              for x% = 1% to 6%
                 effect_unt(x%) = effect_unt(x%) + effect_rmk(x%)
                 eff_unt(x%)    = eff_unt(x%) + effect_rmk(x%)
             next x%
L50850:return
       take_glass_away
              for x% = 1% to 6%
                 effect_unt(x%) = effect_unt(x%) - effect_rmk(x%)
                 eff_unt(x%)    = eff_unt(x%) - effect_rmk(x%)
             next x%
       return

        read_extra          /*  !!!! MAKE SURE don't have to do anything for '044' !!! */
          mat eff_price = zer               /* Avg Selling Price */
          mat eff_upmh  = zer               /* Planning UPMH     */
          mat eff_labor = zer               /* Labor $ Goal/Unit */
          mat eff_scrapa = zer              /* Average Weight    */
          mat eff_scrapb = zer              /* Mistake Scrap Wght*/
          mat eff_lbs = zer                 /* Lbs per unit goal */
          mat eff_matrl = zer               /* matrl cost / lbs  */
          init(" ") ex_key$
          str(ex_key$,1%,4%)  = ex_year$
          str(ex_key$,5%,3%)  = sc_dept$
          str(ex_key$,8%,3%)  = model$
          str(ex_key$,11%,2%) = eff_unit$ 
          if sc_dept$ = "044" then str(ex_key$,9%,2%) = "01"
          if sc_dept$ = "054" then str(ex_key$,9%,2%) = "01"  /*  (EWD0007) */
          if sc_dept$ = "074" then str(ex_key$,9%,2%) = "01"  /*  (AWD011) */

REM          if sc_dept$ <> "000" then goto not_extra_000
REM              call "SHOSTAT" (" READ EXTRA " & ex_key$) stop
REM  not_extra_000
          if sc_dept$ = "021" then str(ex_key$,8,3) = "001"


          read #13, key = ex_key$, using ex_fmt, eff_price(), eff_upmh(),  ~
                                   eff_scrapa(), eff_scrapb(),eff_labor(), ~
                                   eff_lbs(), eff_matrl(), eod goto no_extra
ex_fmt:     FMT XX(12), 84*PD(14,4)
        return
        no_extra
/* if not found, lookup eff_matrl for scrap weight for any department CR1361 */
          gosub read_no_dept_extra
        return
        
        read_no_dept_extra
          init(" ") ex_key$
          str(ex_key$,1%,4%)  = ex_year$
          str(ex_key$,5%,3%)  = sc_dept$
          str(ex_key$,8%,3%)  = "   "
          str(ex_key$,11%,2%) = "  "
        read_no_dept_next
          read #13, key > ex_key$, using efex_fmt, ex_key$, extra_matrl(), ~
                            eod goto read_done
efex_fmt:   FMT CH(12), POS(589), 12*PD(14,4)

          if str(ex_key$,1%,4%)  <> ex_year$ then goto read_done
          if str(ex_key$,5%,3%)  <> sc_dept$ then goto read_done
          if str(ex_key$,11%,2%) <> "01" then goto read_no_dept_next
          if str(ex_key$,11%,2%) = "01" then gosub load_material
          
        read_done
        return
        
        load_material
           for k% = 1% to 12%
              eff_matrl(k%) = extra_matrl(k%)
           next k%
        return
/* CR1361 - */

        calc_eff
           if sc_dept$ = "000" then goto calc_eff_screen   /* (AWD014) */
           if sc_dept$ = "029" and schema% = 1 then goto calc_eff_screen  
           unt_hrse, unts_hrse, untss_hrse, untpp_hrse, stk_hrse = 0.0
           bld_of_hrse, pull_of_hrse = 0.0            /* (EWD0002) */
           sam_upmh, sas_upmh, par_upmh, stk_upmh, bld_of_upmh, pull_of_upmh = 0.0
           mhpu1, mhpu2 = 0.00
           ex_unt$(1%) = "03"
           ex_unt$(2%) = "11"
           ex_unt$(3%) = "15"
          for x% = 1% to 3%
           init(" ") ex_key$
           str(ex_key$,1%,4%)  = ex_year$
           str(ex_key$,5%,3%)  = sc_dept$
           str(ex_key$,8%,3%)  = model$
           str(ex_key$,11%,2%) = ex_unt$(x%) 
           if sc_dept$ = "044" then str(ex_key$,9%,2%) = "01"
           if sc_dept$ = "054" then str(ex_key$,9%,2%) = "01"   /* (EWD0007) */
           if sc_dept$ = "074" then str(ex_key$,9%,2%) = "01"   /* (AWD011) */

           read #13, key = ex_key$, using L50345, ex_upmh(),          ~
                                                  eod goto L50350
L50345:     FMT POS(109), 12*PD(14,4)

           if x% = 1% then sam_upmh = ex_upmh(month%)
           if x% = 2% then sas_upmh = ex_upmh(month%)
           if x% = 3% then par_upmh = ex_upmh(month%)
          next x%

/* calc eff% using ACTUAL scanning units  */
L50350:       if eff_upmh(month%) < .01 then goto L50710
                 unt_hrse = (eff_unt(q%) * (1.0 / eff_upmh(month%)))
L50710:       if sam_upmh < .01 then goto L50720
                 unts_hrse = (eff_unts(q%) * (1.0 / sam_upmh))
L50720:       if sas_upmh < .01 then goto L50730
                 untss_hrse = (eff_untss(q%) * (1.0 / sas_upmh))
L50730:       if par_upmh < .01 then goto L50740
                 untpp_hrse = (eff_untpp(q%) * (1.0 / par_upmh))
L50740:       if sc_dept$ <> "043" then goto L50790
                 str(ex_key$,1%,4%)  = ex_year$
                 str(ex_key$,5%,3%)  = sc_dept$
                 str(ex_key$,8%,3%)  = "SSS"
                 str(ex_key$,11%,2%) = "01"
                 read #13, key = ex_key$, using L50345, ex_upmh(),     ~
                                                  eod goto L50350
                 stk_upmh = ex_upmh(month%)
                 bld_of_upmh = (eff_upmh(month%) / .4)
                 pull_of_upmh = (eff_upmh(month%) / .6) 

                 mhpu1 = (1.0 / stk_upmh)
                 mhpu2 = (1.0 / pull_of_upmh)

                 if stk_upmh < .01 then goto L50790
                 stk_hrse = (eff_unta(q%) * (1.0 / stk_upmh))
                 bld_of_hrse  = (eff_untb(q%) * (1.0 / bld_of_upmh))
                 pull_of_hrse = (eff_untc(q%) * (mhpu1 + mhpu2))

L50790:          eff_hrse = eff_hrse + unt_hrse + unts_hrse +    ~
                         untss_hrse + untpp_hrse + stk_hrse +    ~
                         bld_of_hrse + pull_of_hrse
 
           return

       calc_eff_screen
           unt_hrse, unts_hrse, untss_hrse, untpp_hrse = 0.0
           half_upmh, with_upmh, par_upmh = 0.0
           ex_unt$(1%) = "19"
           ex_unt$(2%) = "21"
           ex_unt$(3%) = "15"
          for x% = 1% to 3%
           init(" ") ex_key$
           str(ex_key$,1%,4%)  = ex_year$
           str(ex_key$,5%,3%)  = sc_dept$
           str(ex_key$,8%,3%)  = model$
           str(ex_key$,11%,2%) = ex_unt$(x%) 

           read #13, key = ex_key$, using L50345, ex_upmh(),          ~
                                                  eod goto L50755

           if x% = 1% then half_upmh = ex_upmh(month%)
           if x% = 2% then with_upmh = ex_upmh(month%)
           if x% = 3% then par_upmh = ex_upmh(month%)
          next x%

/* calc eff% using ACTUAL scanning units  */
L50755:       if eff_upmh(month%) < .01 then goto L50715
                 unt_hrse = (eff_unt(q%) * (1.0 / eff_upmh(month%)))
                 unt_hrse = (unt_hrse + (eff_unts(q%) * (1.0 / eff_upmh(month%))))
L50715:       if half_upmh < .01 then goto L50725
                 unts_hrse = (eff_untss(q%) * (1.0 / half_upmh))
L50725:       if with_upmh < .01 then goto L50735
                 untss_hrse = (eff_untpp(q%) * (1.0 / with_upmh))
L50735:       if par_upmh < .01 then goto L50745
                 untpp_hrse = (eff_unta(q%) * (1.0 / par_upmh))
L50745:          eff_hrse = eff_hrse + unt_hrse + unts_hrse +    ~
                            untss_hrse + untpp_hrse
       return

       avg_price
         eff_unt_avg = 0.0
REM         eff_unt_avg = eff_unt(q%)
         eff_unt_avg = effect_unt(q%)

         init(" ") cont_mod$ 
         cont_mod$ = eff_model$(q%)
         gosub check_cont_head
         if cont% = 1% then eff_unt_avg = round(eff_unt_avg / hp%,4)

         eff_col12 = eff_col12 + (eff_price(month%) * eff_unt_avg)
REM         if sc_dept$ = "009" then gosub logIt
         
       return
       
       logIt
         str(message$,1,6) = sc_yr$
         str(message$,7,4) = sc_wk$
         str(message$,11,3) = sc_day$
         str(message$,14,5) = sc_dept$
         
         str(message$,19,5) = eff_model$(q%)
         convert month% to str(message$,24,4), pic(00)

         convert eff_price(month%) to str(message$,28,14), pic(-00000000.0000)
         
         convert eff_unt_avg to str(message$,44,10), pic(#########0)

         convert eff_col12 to str(message$,56,14), pic(-00000000.0000)
         
        call "LOGFILE" (message$)
       return
       

       get_production_date         /* Calc 'Actual' Screen */
            sc_dte$    = ent_dte$  /* Production Date      */
            testdate2$ = ent_dte$

            call "DATFMTC" (testdate2$,yr%,testdate$)     
            convert str(testdate$,1%,4%) to yr%, data goto L02010 
L02010:
            leap_yr% = 365%
            if mod(yr%,4%) = 0% then leap_yr% = 366%
            init(" ") jdate1$, jdate2$
                                         /* Julian date Curr Prod Week */
            call "DATE" addr("GJ", str(sc_dte$,,6%), str(jdate1$,,5%), x%)  
                                                         /* CONVERT TO */
                                                         /* JULIAN DATE*/
            call "DATJULCV" (jdate1$)                         
            convert str(jdate1$,5%,3%) to j1%, data goto L02020
L02020:                             
            j2% = j1% + (sc_day% - 1%)
            jdate2$ = jdate1$                    /* Julian Date        */
            if j2% <= leap_yr% then goto L02040
REM               j2% = 001%
                  j2% = j2% - leap_yr%

            convert str(jdate2$,1%,4%) to rhh%, data goto L02030
L02030:
            convert (rhh% + 1%) to str(jdate2$,1%,4%), pic(0000)    

L02040:     convert j2% to str(jdate2$,5%,3%), pic(000)

            call "DATJULCV" (jdate2$)                  
            call "DATE" addr("JG", str(jdate2$,,5%), pr_dte$, x%)
        return

        check_access
            init(" ") readkey$
            str(readkey$,1%,9%)   = "APC EFF00"
            str(readkey$,10%,15%) = userid$
            read #2,key = readkey$, eod goto L62085
               FMT POS(25), CH(30)
        return
L62085:    gosub access_denied
           goto exit_program

        access_denied
            comp% = 2%
            hdr$ = "* A c c e s s   D e n i e d *"
            msg$(1) = "You Do Not Have Access to the Efficiency System ?"
            msg$(2) = "                 S e c u r i t y                 "
            msg$(3) = "   Press <RETURN> or Any (PF) Key To Continue.   "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return


        auto_current
           call "SHOSTAT" ("AUTO LOADING INDIRECT DATA")          
           auto_current% = 1%
           eff_proc$ = "P"
           for r% = 1% to 50%
             gosub initialize_variables        /* CLEAR VALUES         */
			 if auto_tab$(r%) = "  " then goto L62100
             sc_yr$ = sv_yr$
             sc_wk$ = sv_wk$
             sc_day$ = sv_day$
             gosub L50000                       /* PRODUCTION DATE      */
             gosub L50100                       /* PRODUCTION DAY       */
             sc_dept$ = auto_tab$(r%)           /* DEPARTMENT VALUE     */
             if sc_dept$ = "  " then goto L62345
             gosub L50200                       /* DEPARTMENT           */
             gosub L50300                       /* CHECK UNITS          */		 
			 gosub dataput
			 
REM CR3200 add the scrap auto load    

             eff_proc$ = "S"              
             gosub L50500                       /* scrap                */   
             gosub L50300                       /* CHECK UNITS          */

L62100:
           next r%
        
L62345: 
        auto_current% = 0%
        return clear all
        goto inputmode

        get_pay
           mgr_hrs, mgr_pay, mgr_inct, eff_paym, eff_hrsm = 0.0
REM           IF SC_DAY$ = "6" THEN RETURN
REM           IF SC_DAY$ = "7" THEN RETURN
           if emp_day$ = "1" then return /* Sunday  */
           if emp_day$ = "7" then return /* Saturday*/
           readkey$ = " "
           str(readkey$,1%,9%)   = "APC EFF03"
           str(readkey$,10%,15%) = str(sc_dept$,2%,2%)
           read #2,key = readkey$, using L62910, descr$, eod goto L62950
L62910:       FMT POS(25), CH(30)

           convert str(descr$,1%,6%) to mgr_hrs, data goto L62920
L62920:

           convert str(descr$,13%,6%) to mgr_pay, data goto L62935
L62935:
           convert str(descr$,20%,6%) to mgr_inct, data goto L62945
L62945:    
           eff_paym = eff_paym + mgr_inct + mgr_pay
           eff_hrsm = mgr_hrs
L62950: return

        display_debug
                                         /* IN% = 0% - Set Before Calc */
                                         /* IN% = 1% - Set After Calc  */
            if userid$ <> "RRH" or userid$ <> "CMG" then return
            gosub set_debug
L63165:     accept                                                       ~
               at (01,02), fac(hex(94)), dbg_msg$               , ch(45),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,31),                                               ~
                  "Debug Display Screen for ",                           ~
               at (02,56), fac(hex(84)), type$                  , ch(07),~
                                                                         ~
                                                                         ~
               at (06,10), "Dept. Code    :",                            ~
               at (06,30), fac(hex(84)), sc_dept$               , ch(10),~
                                                                         ~
               at (08,10), "Dept. Wages   :",                            ~
               at (08,30), fac(hex(84)), dbg$( 3%)              , ch(10),~
                                                                         ~
               at (10,10), "Plant Wages   :",                            ~
               at (10,30), fac(hex(84)), dbg$( 4%)              , ch(10),~
                                                                         ~
               at (12,10), "Labor Dollars :",                            ~
               at (12,30), fac(hex(84)), dbg$( 5%)              , ch(10),~
                                                                         ~
               at (14,10), "UPMH Goal     :",                            ~
               at (14,30), fac(hex(84)), dbg$( 6%)              , ch(10),~
                                                                         ~
               at (16,10), "UPMH Actual   :",                            ~
               at (16,30), fac(hex(84)), dbg$( 7%)              , ch(10),~
                                                                         ~
               at (18,10), "Efficiency Pct:",                            ~
               at (18,30), fac(hex(84)), dbg$( 8%)              , ch(10),~
                                                                         ~
               at (20,10), "Total Wages I%:",                            ~
               at (20,30), fac(hex(84)), dbg$( 9%)              , ch(10),~
                                                                         ~
               at (21,10), "Total Hours   :",                            ~
               at (21,30), fac(hex(84)), dbg$(10%)              , ch(10),~
                                                                         ~
               at (24,02), fac(hex(a4)), d_inp$                 , ch(79),~
                                                                         ~
               keys(dbkey$), key(keyhit%)

               if keyhit% <> 15% then goto L63380
                  call "PRNTSCRN"
                  goto L63165

L63380:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_debug
            if in% = 0% then                                             ~
               dbg_msg$ = "** Before Indirect Labor Calc - Efficiency **"~
                        else                                             ~
               dbg_msg$ = "** After Indirect Labor Calc  - Efficiency **"

          d_inp$ = "Press <Return> To Continue, PF(15) to Print the Scree~
        ~n?"
            dbkey$ = hex(01ffffffffffffffffffffffffff0f1000)
            init(" ") dbg$(), type$
            type$ = "Debug Screen"

            convert eff_col1  to dbg$( 3%),pic(####.####-)

            convert eff_col2 to dbg$( 4%),pic(####.####-)

            convert eff_col3 to dbg$( 5%),pic(####.####-)

            convert eff_col4 to dbg$( 6%),pic(####.####-)

            convert eff_col5 to dbg$( 7%),pic(####.####-)

            convert eff_col6 to dbg$( 8%),pic(####.####-)

            convert eff_col8 to dbg$( 9%),pic(####.####-)

            convert eff_col9 to dbg$(10%),pic(####.####-)

        return

REM - CALCULATE SCANNED UNITS FOR CURRENT PRODUCTION DAY
        calc_scanned_units
            yr% = 0% : leap_yr% = 365%
            temp1$ = eff_dte$
            call "DATFMTC" (temp1$, temp%, temp2$)
            convert str(temp2$,1%,4%) to yr%, data goto L63590
L63590:
            if mod(yr%,4%) = 0% then leap_yr% = 366%
                             /* Windows Scanned from 7 AM of Specified */
                             /* Production Day, Until 6 59 AM of the   */
                             /* Next Production Day. 1st,2nd,3rd Shifts*/
            pd_dept$ = sc_dept$                /* Applicable Department*/

            call "SHOSTAT" ("Calc. New Scanning Units")
            call "DATE" addr("GJ", eff_dte$, jdate1$, x%)
                                               /* Convert to Julian DTE*/
            get jdate1$ using L63600, temp%
L63600:         FMT PD(9,0)
            convert temp% to jdate1$, pic(#######)
            convert str(jdate1$,5%,3%) to j1%, data goto L63650
L63650:
            convert sc_day$ to jj%, data goto L63660
L63660:                                        /* Current Production */
            j1% = j1% + (jj% - 1%)             /* Day Julian Date    */
            if j1% <= leap_yr% then goto L63690
               convert (yr%+1%) to str(jdate1$,1%,4%), pic(####)
               j1% = j1% - leap_yr%

L63690:     convert j1% to str(jdate1$,5%,3%), pic(000)
                                               /* Begin with Production*/
            call "DATJULCV" (jdate1$)
            call "DATE" addr("JG", jdate1$, bg_dte$, x%) /* Date and   */
REM CR3200           if auto_current% = 1% then return
REM CR3200           if auto_wages%   = 1% then return
           gosub check_dept
           if check% = 0% then return         /* Not a Scanning Dept. */

            p_max% = p_no%                     /* No. of Models Defined*/
            p_flg% = 1%                        /* Do Not Load Planning */
            p_scan% = 1%                       /* Only Scanned Product */
            p_shft$ = "AL"                     /* All Shifts           */
            p_screen% = 0%                     /* Display Screen       */
            if auto_current% = 1% then  p_screen% = 1%  /* CR3292      */
            if auto_wages% = 1%   then  p_screen% = 1%  /* CR3292      */			
            init(" ") p_mod$()
            mat p_unt% = zer
            mat p_unts%  = zer
            mat p_untss% = zer
            mat p_untpp% = zer
            for i% = 1% to p_no%               /* Pre Load Models      */
              p_mod$(i%) = str(d_txt$(i%),1%,3%)
            next i%

           sc_load$ = "ALL  "            /* Default equal All Loads    */
           ed_load$ = "ALL  "            /* Default equal All Loads    */

           call "APCPLC40" ( pd_dept$,   /* Specified Department Code  */~
                             bg_dte$,    /* Specified Production Date  */~
                                         /* for a Production Day       */~
                             p_shft$,    /* Dept Shift Code or (AL)    */~
                             sc_load$,   /* Production Load or (ALL)   */~
                             ed_load$,   /* Ending Prod Load           */~
                             p_mod$(),   /* Department Products        */~
                             p_unt%(),   /* Product Units              */~
                             p_unts%(),  /* SAMPLES ONLY               */~
                             p_untss%(), /* CHARGE SASHS ONLY          */~
                             p_untpp%(), /* CHARGE PARTS ONLY          */~
                             p_val(),    /* Product Dollar Value       */~
                             p_mrp(),    /* (5) Costing Buckets        */~
                             p_max%,     /* Max Number of Products     */~
                             p_flg%,     /* 0% = Load, 1% = No Load    */~
                             p_scan%,    /* 0%=Not Scanned, 1%=Scanned */~
                             p_screen%,  /* 0% = Yes, 1% = No - Display*/~
                             #7,         /* (APCPLNDP) NOT USED        */~
                             #5,         /* (APCPLNDT) Prod. Tracking  */~
                             #6,         /* (APCPLNAD) Audit File      */~
                             #2 )        /* (GENCDSIN) Master Code Tab */
            yy = 0.0                     /* Set Units Scanned Values   */
            for i% = 1% to p_no%         /* from Scanning Data.        */
                ww%, xx%, yy%, zz% = 0
                for k% = 1% to 3%
                    xx% = xx% + p_unt%(i%,k%)   /* Whole Units    */
                    yy% = yy% + p_unts%(i%,k%)  /* Sample/Display */
                    zz% = zz% + p_untss%(i%,k%) /* Sashes         */
                    ww% = ww% + p_untpp%(i%,k%) /* Parts          */
                    yy  = yy + p_val(i%,k%)     /* Model Values   */
                next k%
               xx% = xx% - ww% - yy% - zz%

               init(" ") eff_value$                    /* (AWD0009)  */
               convert yy to eff_value$, pic(-########.####)
               eff_value = yy                          /*  (AWD009)  */
                                                /*  (EWD0003)     */
               init(" ") cont_mod$ 
               cont_mod$ = p_mod$(i%)
               gosub check_cont_head
               if cont% <> 1% then goto L63695
                  xx% = (xx% * hp%)

L63695: 
               convert xx% to d_val$(i%,1), pic(#####0)
               if sc_dept$ <> "000" then goto L63700
                  convert ww% to d_val$(i%,5), pic(#####0)
                  goto L63710
L63700:        
               if m% <> 4% and m% <> 5% and m% <> 7% then goto L63710
                  convert yy% to d_val$(i%,2), pic(#####0)

                  convert zz% to d_val$(i%,3), pic(#####0)

                  convert ww% to d_val$(i%,4), pic(#####0)
L63710:     next i%
        return


        check_cont_head
            cont%, hp% = 0%
            init(" ") readkey$
            str(readkey$,1%,9%) = "PLAN CONT"
            str(readkey$,10%,15%) = cont_mod$
            read #2,key = readkey$, using L62910, descr$, eod goto not_cont

            convert str(descr$,30%,1%) to hp%, data goto not_cont

            cont% = 1%
        not_cont
        return

        check_dept
            check% = 0%
            init(" ") readkey$
            str(readkey$,1%,9%)   = "PLAN DEPT"
            str(readkey$,10%,15%) = pd_dept$
            read #2,key = readkey$, eod goto L63980
               check% = 1%
L63980: return

/* (EWD0001)      */
/* ADD GLASS ANALYSIS TO COUNT THE COMPLETED GLASS FOR BG_DTE$ AND ED_DTE$ */


                                          /* ONLY DO ONCE                */
        glass_analysis
            if rm_anal$ <> "0" then return           /* Flag - To perform    */
                                                     /* Glass Ana. or Not    */ 
            if beenherebefore% = 1% then goto analysis_done2 
                                                    /* So only calc once    */
                  
            sc_dte$ = bg_dte$                                                  
            testdate2$ = sc_dte$
            call "DATFMTC" (testdate2$,yr%,testdate$)                        
            convert str(testdate$,1%,4%) to yr%, data goto L64100            
L64100:
            leap_yr% = 365%
            if mod(yr%,4%) = 0% then leap_yr% = 366%
                             /* Windows Scanned from 7 AM of Specified */
                             /* Production Day, Until 6 59 AM of the   */
                             /* Next Production Day. 1st,2nd,3rd Shifts*/
            init(" ") jdate1$, jdate2$, bg_dte$, ed_dte$   

                                         /* Julian date Curr Prod Week */
            call "DATE" addr("GJ", str(sc_dte$,,6%), str(jdate1$,,5%), x%)  
                                                         /* CONVERT TO */
                                                         /* JULIAN DATE*/
            call "DATJULCV" (jdate1$)                                       
            convert str(jdate1$,5%,3%) to j1%, data goto L64110             
L64110:
                                                 /* Current Production */
                                                 /* Day Julian Date    */
            j2% = j1% + 1%                       /* Tommorow's/Next Day*/
            jdate2$ = jdate1$                    /* Julian Date        */
            if j2% < leap_yr% then goto L64130
               j2% = 001%
            convert str(jdate2$,1%,4%) to rhh%, data goto L64120             
L64120:
            convert (rhh% + 1%) to str(jdate2$,1%,4%), pic(0000)             

L64130:     convert j1% to str(jdate1$,5%,3%), pic(000)                      

            convert j2% to str(jdate2$,5%,3%), pic(000)                      
                                               /* Begin with Production*/
            call "DATJULCV" (jdate1$)                                        
            call "DATJULCV" (jdate2$)                                        
                        
            call "DATE" addr("JG", str(jdate1$,,5%), bg_dte$, x%)            
                                                         /* Date and   */
            call "DATE" addr("JG", str(jdate2$,,5%), ed_dte$, x%)             
                                                         /*Next Days   */
                                                         /*Prod. Date  */

            call "EWDEFFGA" ( bg_dte$,    /* Specified Production Date  */~
                                          /* for a Production Day - Beg */~
                              ed_dte$,    /* Specified Production Date  */~
                                          /* for a Production Day - End */~
                              rm%(),      /* Scanned Units              */~
                              #8,         /* (APCPLNGR) Remake File     */~
                              #10)        /* (APCPLNGA) Glass Scan File */

        analysis_done2
            l% = 1%
            if sc_dept$ = "034" then l% = 2%
            for j% = 1% to 5%
                                  /* rm%(j%,2%) is the remake values    */
                                  /* rm%(j%,1%) is the original values  */
                 convert rm%(j%,l%) to d_val$(j%,1), pic(######)

            next j%
            beenherebefore% = 1%
        return

        recalc_wages
           auto_wages% = 1%
           init(" ") readkey1$
           str(readkey1$,1%,9%)   = "APC EFFDP"
           str(readkey1$,10%,13%) = "   "      
           
/* (AWD016) mods for emp_yr, emp_wk, emp_day, emp_ed_day, sc_day, & ed_day */
REM           CONVERT SC_DAY$ TO SC_DAY%, DATA GOTO RE1

REM RE1       CONVERT ED_DAY$ TO ED_DAY%, DATA GOTO RE2

           convert emp_day$ to emp_day%, data goto re1
           
re1:       convert emp_ed_day$ to emp_ed_day%, data goto re2

re2:       REM IF SC_DAY% = ED_DAY% THEN GOTO RE4
           if emp_day% = emp_ed_day% then goto re4
           
              gosub get_dept
              if sc_dept$ = " " then goto re3
re4:       call "EWDWGUPD" (sc_dept$, emp_yr$, emp_wk$, emp_day%,    ~
                            emp_ed_day%, ef(), ef_ov(), tot_ef(),    ~
                            wg(), #11, err% )

          for wg% = emp_day% to emp_ed_day%
             load% = 0%
             if emp_day% = emp_ed_day% then goto re7
             if sc_dept$ = " " then goto re3
                eff_proc$ = "P"
                eff_year$ = sc_yr$
                convert wg% to sc_day$, pic(#)
                eff_day$ = emp_day$
                eff_wk$  = emp_wk$
                eff_shift$ = "00"            /*  Eff Shift  */
                gosub dataload
                if load% = 0% then goto re6
REM                gosub convert_product
re7:         eff_col2  = ef(wg%)      /* EFF REG HOURS  */
             eff_col3  = ef_ov(wg%)   /* EFF OVER HOURS */
             eff_col4  = tot_ef(wg%)  /* TOTAL EFF HOURS*/
             eff_col8  = wg(wg%)      /* TOTAL WAGES    */
REM - MANAGERS PAY, AND INCENTITIVES
             gosub get_pay
             eff_col2  = eff_col2 + mgr_hrs
             eff_col4  = eff_col4 + mgr_hrs
             eff_col8  = eff_col8 + mgr_pay + mgr_inct
REM          gosub L50300
REM             IF SC_DAY% = ED_DAY% THEN GOTO RE3
             if emp_day% = emp_ed_day% then goto re3
             gosub dataputrewrite
re6:      next wg%

                goto re2
re3:         load% = 1%
             auto_wages% = 0%
             if emp_day% = emp_ed_day% then gosub convert_data1
        return


        recalc_wages2
           call "SHOSTAT" ("RECAL WAGES 2")
REM CR3200          stop
           
           auto_wages% = 1%
           init(" ") readkey1$
           str(readkey1$,1%,9%)   = "APC EFFDP"
           str(readkey1$,10%,13%) = "   "      

           convert sc_day$ to sc_day%, data goto re1

wg2
           gosub get_dept
           if sc_dept$ = " " then goto recalc_wages2_done

           e_shift$ = "00"
           eff% = 0%
           init(" ") send_dept$
           send_dept$ = sc_dept$         /* RDB change like other set */
           emp_day% = 0%
           convert emp_day$ to emp_day%, data goto empbad1
empbad1:           
 
           call "APCEFFWG" (send_dept$, emp_yr$, emp_wk$, emp_day$, ef(), ~
                             ef_ov(), tot_ef(), wg(), e_shift$, #4, #3, ~
                             #11, #15, eff%, err% )
   
             load% = 0%
             eff_proc$ = "P"
             eff_year$ = sc_yr$
             convert sc_day% to sc_day$, pic(#)
             eff_day$ = sc_day$
             eff_wk$  = sc_wk$
             eff_shift$ = "00"            /*  Eff Shift  */
                gosub dataload
                if load% = 0% then goto wg6
             eff_col2  = ef(emp_day%)      /* EFF REG HOURS  */
             eff_col3  = ef_ov(emp_day%)   /* EFF OVER HOURS */
             eff_col4  = tot_ef(emp_day%)  /* TOTAL EFF HOURS*/
             eff_col8  = wg(emp_day%)      /* TOTAL WAGES    */
REM - MANAGERS PAY, AND INCENTITIVES
             gosub get_pay
             eff_col2  = eff_col2 + mgr_hrs
             eff_col4  = eff_col4 + mgr_hrs
             eff_col8  = eff_col8 + mgr_pay + mgr_inct

             gosub dataputrewrite
wg6:                         

                goto wg2

        recalc_wages2_done
        return
    
        get_dept
           init(" ") sc_dept$
           str(sav_key$,1%,9%)  = readkey1$
        get_dept_next
           read #2,key > readkey1$, using re5, readkey1$,         ~ 
                                    eod goto get_dept_done
re5:            FMT CH(24)
           if str(readkey1$,1%,9%) <> sav_key$ then goto get_dept_done
              sc_dept$ = str(readkey1$,13%,3%)
              if sc_dept$ = "LLL" or sc_dept$ = "SSS" then goto get_dept_next
              if sc_dept$ = "DDD" then sc_dept$ = "   "
              call "SHOSTAT" ("P r o c e s s i n g  D e p t   " & sc_dept$)
        get_dept_done
        return

        error_prompt
           comp% = 2%
           hh$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hh$, msg$(1%), msg$(2%), msg$(3%))
        return
                                                                            
        open_error                                                            
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return


        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")
            close #1 : close #2 : close #3 : close #4 : close #5
            close #6 : close #7 : close #8 : close #9 : close #10
            close #11 : close #12 : close #13 
            end
                

