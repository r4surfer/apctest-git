        REM **************************************************************~
            *                                                            *~
            *  Program Name       - AWDSCRAP                             *~
            *  Creation Date      - 12/23/2017                           *~
            *  Last Modified Date - 12/23/2017                           *~
            *  Written By         - Ricky Beane                          *~
            *  Last Mod By        -                                      *~
            *                                                            *~
            *  Description        - Scanning Utility used to automate    *~
            *                       department scrap box weight tracking *~
            *                       for the current date only. A user    *~
            *                       screen will be requested for an      *~
            *                       audit reporting.                     *~
            *                                                            *~
            *  Special Notes      - (SRP) Userid is for starting the     *~
            *                             Scrap Scanning program         *~
            *                                                            *~
            *------------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                 *~
            *---WHEN----+-----------------WHAT---------------------+-WHO-*~
            * 12/23/2017! New Program                              ! RDB *~
            * 02/08/2018! SR1234 Use system date not subrte date   ! RDB *~
            * 02/22/2018! CR1335 Reset variables before using      ! RDB *~
            * 02/27/2018! CR1353 Dept 019 set to invalid for scrap ! RDB *~
            * 11/11/2018! SR86857 Set limit on scanned weight amt  ! RDB *~
            **************************************************************

        dim filename$8,                  /* Use with EWDOPEN - EWD016  */~
            scr_dept$3,                  /* Product Line               */~
            scr_wgt$9, scr_totwgt$12,    /* Screen Weight / Day Weight */~
            scr_arith$1,                 /* Add or Subtract weight     */~
            scr_userid$10,               /* Oracle ID                  */~
            readkey$24, desc$30,         /* GENERIC KEY                */~
            savekey$12,                  /* Generic Key                */~
            pfkeys$40,                   /* PF KEYS                    */~
            rf_ee$(7%)20,                /* Stop for RF     (AWD045)   */~
            rf_err$(100%)20,             /* RF Err Msg         (AWD045)*/~
            date$8,                      /* Date for screen display    */~
            dateout$8,                   /* Time Extract For Screen    */~
            rf_errormsg$20,              /* RF Error Message   (AWD045)*/~
            i$(24%)80,                   /* Screen Image               */~
            rf_inpmessage$20,            /* Information Mess   (AWD045)*/~
            lfac$(4%)1,                  /* Field Attribute Characters */~
            rf_pf$(3%)20,                /* RF PF Screen Literal       */~
            calc_time$8,                 /* Use for Re-make Calc(EWD001*/~
            curr_dte$6,                  /* Current date               */~
            userid$3                     /* Current User Id            */

        dim schema$8                     /* NC or TX location schema   */
        
        dim                              /* Subroutine AWDPLN0B - Variables */~
            cur_yr$4, cur_yr_bi$2,       /* Current Year               */~
            cur_wk$2,  cur_dy$1,         /* Current Prod. Week an Day  */~
            cur_dte$6, cur_date$8,       /* Prod Week Date Form/Unform */~
            ent_yr$4, ent_yr_bi$2,       /* Julian Year and Day YYDDD  */~
            ent_wk$2,  ent_dy$1,         /* Entry Prod. Week an Day    */~
            ent_dte$6, ent_date$8,       /* Prod Week Date Form/Unform */~
            prv_yr_bi$2                  /* binary fmt                 */

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
            
        dim                              /* File = (AWDSRPAD)               */~
            ad_prod_date$6,              /* Production Date                 */~
            ad_user$10,                  /* Oracle user ID                  */~
            ad_dept$3,                   /* Department                      */~
            ad_sign$1,                   /* Addition or Subtraction         */~
            ad_scan_dte$6,               /* Scan date                       */~                 
            ad_scan_time$4               /* Scan time                       */

        dim f2%(45%),                    /* = 0 if the file is open      */~
            fs%(45%),                    /* = 1 if file open, -1 if it   */~
                                         /*   doesn't exist, or 0 if     */~
                                         /*   not yet checked (OPENCHCK) */~
            rslt$(45%)20,                /* Text from file opening       */~
            cursor%(2%)                  /* Cursor location for edit     */

            mat f2% = con
            mat fs% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! AWDSRPDP ! (NEW) Scrap Weight by Dept               *~
            * #2  ! GENCODES ! Master Code Table File                   *~
            * #3  ! AWDSRPAD ! Scrap Scanning Audit                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "AWDSRPDP",                                      ~
                        varc,     indexed,  recsize =  50,               ~
                        keypos = 01,   keylen = 16                      

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 1,    keylen = 24
 
            select #3,  "AWDSRPAD",                                      ~
                        varc,     indexed,  recsize =  70,               ~
                        keypos = 1,    keylen = 36      /* CR1335 increase */
                        
            call "SHOSTAT" ("Opening Files, One moment Please?")
                                                         
            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 100%, rslt$(1%))
            
            filename$ = "GENCODES" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            
            call "OPENCHCK" (#3, fs%(3%), f2%(3%), 100%, rslt$(3%))
            
        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

        dim rf_apc$20, rf_pname$8
            rf_apc$   = "Scrap Weight Utility"
            rf_pname$ = "AWDSCRAP"       

        init(" ") schema$    : schema% = 0%
        call "SCHEMA" (schema$,             /* What switch 1-NC 2-NE    */~
                       schema%,             /* Schema                   */~
                       #2,                  /* GENCODES                 */~
                       err% )               /* error                    */
                                                          
        call "EXTRACT" addr("ID", userid$)
        date$ = date
        call "DATEFMT" (date$)
        curr_dte$ = date
        call "DATUFMTC" (curr_dte$)
 
        rf_ee$(1%) = "  SSS TTT OOO PPP   "              
        rf_ee$(2%) = "  S    T  O O P P   "
        rf_ee$(3%) = "  SSS  T  O O PPP   "
        rf_ee$(4%) = "     S T  O O P     "
        rf_ee$(5%) = "     S T  O O P     "
        rf_ee$(6%) = "  SSSS T  OOO P     "

REM------------------------------------------------------------------------
REM        Informational area of errors from program                      -
REM------------------------------------------------------------------------
        rf_err$(01%)="Open Error " & filename$
        rf_err$(02%)="Bad Prod Wk        "
        rf_err$(03%)="Insert must be +   "
        rf_err$(04%)="Bad Weight         "
        rf_err$(05%)="Invalid Dept       "
        rf_err$(06%)="Not + or -         "
                                 
      initialize                                 
       
L00000:
        found_user% = 1%
        gosub rf_user
        init(" ") rf_errormsg$, rf_inpmessage$
      
        if keyhit% = 0% then gosub check_userid     /* Enter key */
        if keyhit% = 1% then gosub check_userid     /* F1        */            
        if keyhit% = 2% then goto exit_program      /* F2        */
        
        if found_user% = 0% then goto rf_start
        
        goto L00000
          
REM------------------------------------------------------------------------
REM             R F   U S E R                                             -
REM------------------------------------------------------------------------
        
        rf_user                          
          gosub rf_user_screen_1
          accept                                                         ~
               at (01,02), fac(hex(8c)), rf_pname$              , ch(08),~
               at (01,12), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), rf_errormsg$           , ch(20),~
                                                                         ~
               at (03,02), "User IDD:",                                  ~
               at (03,10), fac(lfac$(1%)), scr_userid$          , ch(10),~
                                                                         ~
               at (07,02), fac(hex(a4)),   rf_inpmessage$       ,        ~
               at (08,02), fac(hex(8c)),   rf_pf$(1%)           , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
               init(" ") rf_errormsg$

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return
        
        rf_user_screen_1
          lfac$(1%) = hex(81)
          init(" ") dateout$
          call "TIME" (dateout$)

          rf_pf$(1%) = "1) Scan  2) Exit    "
          pfkeys$ = hex(0102ffffffffffffffffffffffffffffff00)

        return                                   

rf_start:
        
        init(" ") scr_dept$, scr_wgt$
        scr_arith$ = "+"
        rf_inpmessage$ = "Enter Dept & Weight?"

REM------------------------------------------------------------------------
REM             R F   M A I N                                             -
REM------------------------------------------------------------------------
        rf_main
                                                          
          gosub rf_mainmenu
          init(" ") rf_errormsg$, rf_inpmessage$
      
          if keyhit% = 1% then goto startover                  
          if keyhit% = 2% then exit_program
       
          if cursor%(1%) = 3% or cursor%(1%) = 4%    ~
               then gosub'400(1%)                    /* Department */
          if cursor%(1%) = 5% or scr_wgt$ > " "  then gosub'450(3%) /* Weight */
              
          if keyhit% <> 3% then goto rf_main
          gosub check_dept
          if found_dept% = 1% then goto rf_start
          
          p% = 0%                                 
          p% = pos("+-" = scr_arith$)            /* Checking sign */
          if p% > 0%  then goto L02000
          
L01500:     rf_errormsg$ = "Not + or -         "
            gosub'500(fieldnr%)
            goto rf_main
    
L02000:   gosub prod_scan
           
          goto rf_main


REM------------------------------------------------------------------------
REM       B E G I N N I N G    O F    S C R E E N S                       -
REM------------------------------------------------------------------------

        rf_mainmenu                            
          gosub rf_set_screen_1
          accept                                                         ~
               at (01,02), fac(hex(8c)), rf_pname$              , ch(08),~
               at (01,12), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), rf_errormsg$           , ch(20),~
                                                                         ~
               at (03,02), "Dept:",                                      ~
               at (03,10), fac(lfac$(1%)), scr_dept$            , ch(03),~
                                                                         ~
               at (04,02), "+ or -:",                                    ~
               at (04,10), fac(lfac$(2%)), scr_arith$           , ch(01),~
                                                                         ~
               at (05,02), "Weight:",                                    ~
               at (05,10), fac(lfac$(3%)), scr_wgt$             , ch(09),~
                                                                         ~
               at (06,02), "TotDay:",                                    ~
               at (06,10), fac(hex(8c)), scr_totwgt$            , ch(12),~
                                                                         ~
               at (07,02), fac(hex(a4)),   rf_inpmessage$       ,        ~
               at (08,02), fac(hex(8c)),   rf_pf$(1%)           , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
               init(" ") rf_errormsg$

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return
        
        rf_set_screen_1
          lfac$(1%) = hex(81)
          init(" ") dateout$
          call "TIME" (dateout$)


          rf_pf$(1%) = "1)StOvr 2)Ext 3)Save"
          pfkeys$ = hex(010203ffffffffffffffffffffffffffff00)

        return                                   
        
REM------------------------------------------------------------------------
REM       Lookup Department and existing weight entry                     -
REM------------------------------------------------------------------------
 
        deffn'400(fieldnr%)
        
            gosub check_dept
            if found_dept% = 1% then goto rf_start
            
            init(" ") cur_yr$, cur_wk$, cur_dy$, cur_dte$, cur_date$,  ~
                  ent_yr$, ent_wk$, ent_dy$, ent_dte$, ent_date$,      ~
                  cur_yr_bi$, ent_yr_bi$, prv_yr_bi$, sp_prod_dte$,    ~
                  sp_wk$, sp_day$, sp_yr$, sp_dept$
            sp_wgt = 000000.00             /* CR1335 */

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
                              
            if pl_e% <> 0% then goto L05000

            temp% = val(cur_yr_bi$,2)
            convert temp% to sp_yr$, pic (####)
            
            sp_prod_dte$ = date 
            sp_wk$       = cur_wk$
            sp_day$      = cur_dy$

            str(awd_scrp_key$,1%,6%)  = sp_prod_dte$
            str(awd_scrp_key$,7%,4%)  = sp_yr$
            str(awd_scrp_key$,11%,2%) = sp_wk$
            str(awd_scrp_key$,13%,1%) = sp_day$
            str(awd_scrp_key$,14%,3%) = scr_dept$ 
            
            read #1, key = awd_scrp_key$, using L10000,                     ~
                      sp_prod_dte$,sp_yr$,sp_wk$,sp_day$,sp_dept$,sp_wgt,   ~
                      sp_scan_dte$, sp_scan_time$,sp_filler$,               ~
                      eod goto L01000
                      
            convert sp_wgt to scr_totwgt$, pic(######.####-)
      
L01000: return
        
        
REM------------------------------------------------------------------------
REM       Confirm Weight is number and sign because it is only 1 character-
REM------------------------------------------------------------------------
  
        deffn'450(fieldnr%)

             p% = 0%                                 
             p% = pos("+-" = scr_arith$)            /* Checking sign */
             if p% = 0%  then goto L01500
          
             convert scr_wgt$ to scr_wgt, data goto L07000
             if scr_wgt > 5000 then goto L08000       
        
        return

REM------------------------------------------------------------------------
REM      Production scan and weight entry processing                      -
REM------------------------------------------------------------------------

        prod_scan
            
            init(" ") cur_yr$, cur_wk$, cur_dy$, cur_dte$, cur_date$,  ~
                  ent_yr$, ent_wk$, ent_dy$, ent_dte$, ent_date$,      ~
                  cur_yr_bi$, ent_yr_bi$, prv_yr_bi$, sp_prod_dte$,    ~
                  sp_wk$, sp_day$, sp_yr$, sp_dept$
            sp_wgt = 000000.00              /* CR1335 */
                      
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
                              
            if pl_e% <> 0% then goto L05000

            temp% = val(cur_yr_bi$,2)
            convert temp% to sp_yr$, pic (####)

            sp_prod_dte$ = date 
            sp_wk$       = cur_wk$
            sp_day$      = cur_dy$

            str(awd_scrp_key$,1%,6%)  = sp_prod_dte$
            str(awd_scrp_key$,7%,4%)  = sp_yr$
            str(awd_scrp_key$,11%,2%) = sp_wk$
            str(awd_scrp_key$,13%,1%) = sp_day$
            str(awd_scrp_key$,14%,3%) = scr_dept$ 
            
            read #1, hold, key = awd_scrp_key$, using L10000,               ~
                      sp_prod_dte$,sp_yr$,sp_wk$,sp_day$,sp_dept$,sp_wgt,   ~
                      sp_scan_dte$, sp_scan_time$,                          ~
                      eod goto L04000
   
                 delete #1
                 
         /* Process Entry Found record - check weight entry the goto insert */
                 convert scr_wgt$ to scr_wgt, data goto L07000
                 if scr_arith$ = "-" then         ~
                   sp_wgt = sp_wgt - scr_wgt      ~
                 else                             ~
                   sp_wgt = sp_wgt + scr_wgt
                 
                 goto L04250
                 
L04000:   /* Process Entry Add new record   */  
                 if scr_arith$ = "-" then goto L06000  /* negative now allowed*/
                 convert scr_wgt$ to scr_wgt, data goto L07000
                 sp_wgt = scr_wgt
             
L04250:          sp_dept$ = scr_dept$
                 sp_scan_dte$ = date
                 calc_time$ = time                      /* Military - HHMMSSXX */
                 sp_scan_time$ = str(calc_time$,1%,4%)
                            
                 put #1 using L10000,    ~
                   sp_prod_dte$, sp_yr$, sp_wk$, sp_day$, sp_dept$, sp_wgt, ~
                   sp_scan_dte$, sp_scan_time$, sp_filler$    
                 
                 write #1, eod goto L05000  
                 
                 gosub scan_audit
                 
                 convert sp_wgt to scr_totwgt$, pic(######.####-)
                 rf_inpmessage$  = "Dept " &scr_dept$ & " Updated"
                 init(" ") scr_dept$, scr_wgt$
                 scr_arith$ = "+"
                 call "ALLFREE"
            return
            
L05000:     rf_errormsg$ = "Bad Prod Wk        "
            gosub'500(fieldnr%)
            return

L06000:     rf_errormsg$ = "Insert must be +   "
            gosub'500(fieldnr%)
            return

L07000:     rf_errormsg$ = "Bad Weight         "
            gosub'500(fieldnr%)
            
L08000:     rf_errormsg$ = "Weight Limit 5,000 "
            gosub'500(fieldnr%)
                        
        return

REM------------------------------------------------------------------------
REM                F O R M A T  S T A T E M E N T S                       -
REM------------------------------------------------------------------------

L10000: FMT                       /* File = (AWDSRPDP)               */~
            CH(06),               /* Production Date                 */~
            CH(04),               /* Production Year                 */~
            CH(02),               /* Production Week                 */~
            CH(01),               /* Production Day                  */~
            CH(03),               /* Production Department           */~
            PD(14,4),             /* Weight                          */~
            CH(06),               /* System Date of scan             */~
            CH(04),               /* System Time of scan             */~
            CH(16)                /* Filler space                    */
            
L10005: FMT                       /* File = AWDSRPAD                 */~
            CH(06),               /* Production Date                 */~
            CH(10),               /* User ID                         */~
            CH(03),               /* Department                      */~
            CH(01),               /* Sign                            */~
            PD(14,4),             /* Weight                          */~
            PD(14,4),             /* Total Weight                    */~
            CH(06),               /* System Date of scan             */~
            CH(04),               /* System Time of scan             */~
            CH(24)                /* Filler space                    */
            
/* Check user ID  */
       check_userid
          found_user% = 1%
          init(" ") readkey$, desc$
          str(readkey$,1%,9%)   = "SCRAP USR"
          str(readkey$,10%,15%) = scr_userid$
          read #2,key = readkey$, using L12000, desc$, eod goto L12025
L12000:       FMT POS(25), CH(30)   
            found_user% = 0%    
        return  
L12025:       
          rf_errormsg$ = "Invalid User ID    "
          gosub'500(fieldnr%)
       return
       
/* Restart the program  */
       startover
            init(" ") scr_dept$, scr_wgt$
            scr_arith$ = "+"
            init("0000.0000 ") scr_totwgt$
            rf_inpmessage$ = "Enter Dept & Weight?"
            goto rf_main

/*  Check department  */        
        check_dept
          found_dept% = 1%
/* CR1353 */
          if scr_dept$ = "019" then goto L12090
          
          init(" ") readkey$, desc$
          str(readkey$,1%,9%)   = "PLAN DEPT"
          str(readkey$,10%,15%) = scr_dept$
          read #2,key = readkey$, using L12050, desc$, eod goto L12075
L12050:       FMT POS(25), CH(30)       
            found_dept% = 0%
            scr_totwgt$ = " "               /* CR1335 */
        return
L12075:   
          init(" ") readkey$, desc$
          str(readkey$,1%,9%)   = "APC EFFSC"
          str(readkey$,10%,15%) = scr_dept$
          str(savekey$,1%,12%) = readkey$       
          read #2,key > readkey$, using L12080, readkey$, desc$, eod goto L12090
L12080:            FMT CH(24), CH(30)
            if str(readkey$,1,12) <> str(savekey$,1,12) then goto L12090          
            found_dept% = 0%
            scr_totwgt$ = " "              /* CR1335 */ 
        return
L12090:       
          rf_errormsg$ = "Invalid Dept       "
          gosub'500(fieldnr%)
        return
        
REM------------------------------------------------------------------------
REM       E R R O R    R O U T I N E S                                    -
REM------------------------------------------------------------------------
 
        deffn'500(fieldnr%)
          gosub set_error
          accept                                                       ~
               at (01,02), fac(hex(94)), rf_errormsg$           , ch(20),~
                                                                         ~
               at (02,02), fac(hex(84)), rf_ee$(1%)             , ch(20),~
               at (03,02), fac(hex(84)), rf_ee$(2%)             , ch(20),~
               at (04,02), fac(hex(84)), rf_ee$(3%)             , ch(20),~
               at (05,02), fac(hex(84)), rf_ee$(4%)             , ch(20),~
               at (06,02), fac(hex(84)), rf_ee$(5%)             , ch(20),~
                                                                         ~
               at (07,02), fac(hex(a4)),   rf_inpmessage$       ,        ~
               at (08,02), fac(hex(8c)),   rf_pf$(1%)           , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_error
          rf_inpmessage$ = "PF<04> to Continue"
          rf_pf$(1%)     = "(04) Continue     "
          pfkeys$        = hex(ffffff04ffffffffffffffffffffffffff)
        return
        
        open_error
            rf_errormsg$ = "Open Error " & filename$
            gosub'500(fieldnr%)
            goto exit_program  
        return

REM------------------------------------------------------------------------
REM       Write audit records for every scanned weight                    -
REM------------------------------------------------------------------------  
        scan_audit
 /* CR1335 */
            init(" ") ad_prod_date$, ad_user$, ad_dept$, ad_sign$,   ~
               ad_scan_dte$, ad_scan_time$
            ad_wgt = 000000.0000          
            ad_tot = 000000.000
 /* CR1335 */
            
            ad_prod_date$ = sp_prod_dte$
            ad_user$      = scr_userid$
            ad_dept$      = sp_dept$
            ad_sign$      = scr_arith$
            ad_wgt        = scr_wgt
            ad_tot        = sp_wgt
            ad_scan_dte$  = sp_scan_dte$
            ad_scan_time$ = sp_scan_time$
                            
            put #3 using L10005,    ~
                   ad_prod_date$, ad_user$, ad_dept$, ad_sign$, ad_wgt, ~
                   ad_tot, ad_scan_dte$, ad_scan_time$
                 
            write #3, eod goto L20000  
               
L20000:        
        return
        
REM------------------------------------------------------------------------
REM       E N D    O F    E R R O R    R O U T I N E S                    -
REM------------------------------------------------------------------------

REM------------------------------------------------------------------------
REM       E X I T    P R O G R A M                                        -
REM------------------------------------------------------------------------

        exit_program
            call "SHOSTAT" ("One Moment Please")
            close #1 : close #2 : close #3
        end
