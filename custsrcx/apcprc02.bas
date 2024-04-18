        REM *************************************************************~
            * Special Note - PF(10) - Build ECS Flat File(Report Screen)*~
            *  Program Name      - APCPRC02                             *~
            *  Creation Date     - 12/07/93                             *~
            *  Last Modified Date- 06/06/2017                           *~
            *  Description       - This Utility Program Creates the     *~
            *                      Data Associated with Standard and    *~
            *                      Special Ref Calc's                   *~
            *                                                           *~
            *  Special Subs - (COPY_DATA) Copy's (A)ctive Pricing Data  *~
            *                 for (Catalog, Catalog Method, Model) to   *~
            *                 (New) In-Active (Catalog, Catalog Method, *~
            *                 Model) Insuring Only One (1) In-Active    *~
            *                 Record For Key.                           *~
            *                                                           *~
            *                 (ACTIVATE_DATA) Changes In-Active Status  *~
            *                 for (Catalog, Catalog Method, Model) to   *~
            *                 an Active Status Insuring Only (1) Active *~
            *                 Active Record for Key.                    *~
            *                                                           *~
            *                 (ASSIGN_RECORD) Obtains the (Next) Record *~
            *                 Number for the 'New Record', from the     *~
            *                 (PRICE 000) Code Table. The APC (0000)    *~
            *                 Master Catalog Code is Used. At Position  *~
            *                 (21) in the Description the (Next) Number *~
            *                 is Obtained and Incremented.              *~
            *                                                           *~
            *                 (APCPR1SB) - Code Table Lookup's          *~
            *                                                           *~
            *                 (APCPR3SB) - FORMAT_BUFFER Decode Price   *~
            *                              Information for Screen and   *~
            *                              Reports. (EWD004)            *~
            *                                                           *~
            *          Note - For Primary Data Key There can be both an *~
            *                 (A)ctive and (I)nactive Record for the    *~
            *                 Same Pricing Data. But 'Only' (A)ctive    *~
            *                 Data is used to Calculate the Price of    *~
            *                 the Product.                              *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/24/94 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 04/08/97 ! Mods to Create a New Pricing Flat file   ! RHH *~
            *          !   for (ECS). New routine to LOOKUP_SERIES!     *~
            *          !   but wired for Norandex.                !     *~
            * 04/18/97 ! Mod to Support Alfa Format in Tables     ! RHH *~
            * 05/27/97 ! Mod to use the table (PRICE 017) to build! RHH *~
            *          !   the ECSPRICE Flat File. Table controls !     *~
            *          !   the models put into file.              !     *~
            * 10/31/97 ! Check for R6.04.03 Upgrade               !     *~
            * 11/19/98 ! (EWD001) - Mod Copy/Activate function to ! BWS *~
            *          !   allow Catalog/Method-level processing. !     *~
            *          !   Also mods to xref by Mdl,Ref. or Value.!     *~
            * 02/23/99 ! (EWD002) - Add Field Tbl Vals to screen. ! BWS *~
            * 04/01/99 ! (EWD003) - Lookup of recs w/no key data. ! BWS *~
            * 11/01/99 ! (EWD004) - Mods to support new wood Jamb ! RHH *~
            *          !   tables.                                !     *~
            *          !                                          !     *~
            * 09/05/00 ! (EWD005) - Mods to allow alphas on Bay/  ! CMG *~
            *          !   Bows in model code on Report Selection.!     *~
            *          !                                          !     *~     
            *05/10/2007! (EWD006) - Mods to allow alphas on dept #! DES *~
            *03/18/2014! (AWD007) - Mods to allow ALL Catagory    ! PWW *~
            *          !   option. Added Price Ref Calc Method.   !     *~
            *06/25/2014! (AWD008) - Mods to allow Purging         ! PWW *~
            *05/01/2015! (IM8052) - Mod to fix purging where      ! PWW *~
            *          !            counting was ignoring 2nd file!     *~
            *12/28/2017! (SR67154) mods for NFRC 2016             ! CMG *~
            *06/06/2017! (SR80785) limit model to 3 digits        ! CMN *~
            *10/04/2019! (CR2267) add authorized user check       ! RDB *~
            *10/15/2019! (CR2289) add user ID, date, time on chang! RDB *~
            *03/04/2020! (CR2451) 3900 PSE Price                  ! CMN *~  
            *01/24/2023! CR3237 correct issue not writing user ID ! RDB *~			
            *************************************************************

        dim                              /* (APCPCMST) Price Calc Def. */~
            pc_st$1, pc_st_d$30,         /* Price Record Status        */~
            pc_ref$8,                    /* Price Record Number        */~
            pc_c$4, pc_c_d$30,           /* Price Catalog Code/Desc    */~
            pc_cm$2, pc_cm_d$30,         /* Price Catalog Method Code  */~
            pc_m$16, pc_m_d$30,          /* APC Model Code  (CR2451)   */~
            pc_vm$13,                    /* Virtual Model (CR2451)     */~
            pc_r$2, pc_r_d$32,           /* Std/Spc Reference Code     */~
            pc_rc$3, pc_rc_d$32,         /* Price Ref. Calc Method     */~
/* CR2289 */                                                             ~
            pc_user$3, pc_date$6,        /* User ID and System Date    */~
            pc_time$6,                   /* System time                */~
                                                                         ~
/*<AWD007> */                                                            ~
            rp_rcm$3, rp_rcm_d$30,      /* Price Ref. Calc Method(Copy)*/~
            copy_read$10, copy_count$10, /* Copy Record Counts          */~
/*<AWD007> */                                                            ~
            pc_kdesc$(3%)3,              /* Field Description Codes    */~
            pc_kfld$(3%)3,pc_kfld_d$(3%)10, /* Field Description Codes */~
            pc_kfld%(3%),                 /* Field Definition Codes    */~
            pc_kbeg%(3%),                 /* Start Position in Part No.*/~
            pc_klen%(3%),                 /* Field Length in Part No.  */~
            pc_vtbl$2, pc_vtbl_d$10,     /* Field Definition Code-Table*/~
            pc_vtblv$(6%)3, vd$(6%)30,   /* Field Table Values         */~
/*EWD002*/  pc_vt_sc$(6%)3,              /* Field Table Vals for Screen*/~
            pc_vdesc$3, pc_vdesc_d$20,   /* Value Description Codes    */~
            pc_vfmt$2, pc_vfmt_d$10,     /* Value Field Format Code    */~
            pc_vcalc$2,pc_vcalc_d$12,    /* Value Calculation Code     */~
            pc_kcalc$2,pc_kcalc_d$14,    /* Key   Calculation Code     */~
            pc_kunt$2, pc_kunt_d$6,      /* Key Unit Conversion Code   */~
            pc_key$5,                    /* Primary Key (APCPCMSK)     */~
            p_key$53,                    /* Primary Key (APCPCMST)     */~
            pc_k$25,                     /* Data Value Key - APCPCMST  */~
            pc_fil$1,                    /* Definition Filler Area CR2289 */~
            kd$(3%)20,                   /* Key Input Descriptions     */~
            ik$(3%)25,                   /* Key Input Data             */~
            ik_d$(3%)20,                 /* Key Input Data Descript    */~
            pc_vl$(6%)10, pc_vl(6%),     /* Data Values                */~
            pc_spc$3, pc_spc_d$30,       /* Special Calc Code-PRICE 010*/~
            tab_val$5, tab_desc$32,      /* Table Key and Description  */~
            tab_hdr$30, sav_key$9,       /* Calc Sequence Codes        */~
            descr$32, descr_save$32,     /* GENERIC DESCRIPT FOR FIELDS*/~
            readkey$50, r_rec$30,        /* GENCODES PRIMARY KEY       */~
            company$60,                  /* For Report Company Name    */~
            print_title$43,              /* For Report Title           */~
            rpt_time$8,                  /* For Report Time            */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(21%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3, rr$5               /* Current User Id            */

        dim rp_st$1, rp_st_d$30,         /* Report (A), (I), (B)=Both  */~
            rp_c_b$4, rp_c_e$4,          /* Report Beg/End Catalog Code*/~
            rp_cm_b$2,rp_cm_e$2,         /* Report Beg/End Cat Method  */~
                                         /* (CR2451)                   */~            
            rp_m_b$16, rp_m_e$16,        /* Report Beg/End Model Code  */~
            rp_vm_b$13, rp_vm_e$13,      /* Report Beg/End VMdl (CR2451)*/~
            rp_c_bd$30, rp_c_ed$30,      /* Report Beg/End Catalog Code*/~
            rp_cm_bd$30,rp_cm_ed$30,     /* Report Beg/End Cat Method  */~
            rp_m_bd$30, rp_m_ed$30,      /* Report Beg/End Model Code  */~
            rp_rc$2, rp_rc_d$30,         /* Copy   Beg/End Ref Code Des*/~ 
            rp_desc$18,                  /* Report Ref Calc Description*/~
            cc$4, sav_c$4,               /* Report - Save Catalog      */~
            cm$2, sav_cm$2, sav_ik$30,   /* Report - Save Catalog Meth */~
            mm$16, sav_mm$16, sav_kd$20, /* Report - Save Model Code   */~
            sav_mod$16,                  /* SAVE FOR FLAT FILE         */~
            sav_r$2, sav_st$1, sav_rc$3, /* Report - save Ref Type     */~
/*<AWD007>  from_key$53, from_sav$12,       Copy - Catalog to Product  */~   
            from_key$53, from_sav$28,    /* Copy - Catalog to Product  */~
            check_key$53, pc_rec$128,    /* (APCPCMST) - Record        */~
/*EWD001*/  build_key$53, temp_key$53,   /* Model/Ref. to Catalog Keys */~
/* Begin*/  tab_xref$9, xrf_vl(6),       /* GENCODES Table, Xref Values*/~
            cg_st$1, cg_st_d$30,         /* Change Price Record Status */~
            cg_c$4, cg_c_d$30,           /* Chng Price Catlg Code/Desc */~
            cg_m$16, cg_m_d$30,          /* Change APC Model Code/Desc */~
            cg_vm$13,                    /* Change Model (CR2451)      */~
/*EWD001*/  cg_vl$10,                    /* Change Value (New Value)   */~
/* End  */  del_fr_seq$8, del_to_seq$8,  /* Delete From/To Sequence #s */~
            mdllfac$1,                   /* Virtual Mdl Screen (CR2451)*/~
            mdllfac1$1,                  /* Virtual Mdl Screen (CR2451)*/~
            usr_desc$32                  /* User Security (CR2451)     */

            
/*<AWD008>+ */
        dim apcpcmsd_key$22,               /* Readkey                    */~
            pc_std%(20%),                  /* Standard Pricing Sequence  */~
            pc_stdc$(20%)1,                /* Cross-Ref Codes            */~
            pc_stdr$(20%)9,                /* Initial Ref Calc Code 3,3,3*/~
            pc_spc%(20%),                  /* Special Pricing Sequence   */~
            pc_spcc$(20%)1,                /* Cross-Ref Codes            */~
            pc_spcr$(20%)9,                /* Initial Ref Calc Code 3,3,3*/~
            pc_sub%(20%),                  /* Sub Part Seq    (AWD006)   */~
            pc_subc$(20%)1,                /* Sub Part Cross-Ref (AWD006)*/~
            pc_subr$(20%)9,                /* Sub Part Ref Calc  (AWD006)*/~
            pc_dfil$86                     /* Filler Area                */
/*<AWD008>- */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        dim ecs_rec$128,cnt$28,          /* (ECSPRICE) - FILE          */~
            s_verify$5, pc_series$8,     /* USE FOR CONVERSION         */~
            ecs_k$(3%)25                 /* Key Values                 */

        dim                              /* FILE - LABEL PRINT FILES   */~
            file$8,                      /* Flat File Name             */~
            library$8,                   /* Library Name = 'APCDATA'   */~
            volume$6                     /* Volume Name = 'CARLOS'     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "Pricing Data Entry for Catalog Def's    "
            pname$ = "APCPRC02 - Rev: R6.04"

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
            * #1  ! APCPCMSK ! APC Pricing (Key) Definition File        *~
            * #2  ! GENCODES ! Master System Tables File                *~
            * #3  ! APCPCMST ! APC Master Price Definition File         *~
            * #4  ! APCPCMSD ! PRICING MASTER CALC DEFINITION           *~
            * #5  ! ECSPRICE ! ECS FLAT FILE LAYOUT                     *~         
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,   "APCPCMSK",                                     ~
                        varc,     indexed,  recsize =  64,               ~
                        keypos =    1, keylen =    5

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #3,   "APCPCMST",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    9, keylen =   53,                    ~
                        alt key  1, keypos =    1, keylen =  8

            select #4,   "APCPCMSD",                                     ~
                        varc,     indexed,  recsize = 768,               ~
                        keypos =    1, keylen =    22

            select #5,   "ECSPRICE", consec, recsize = 128
            
            select #6,   "AWDPCMST",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    9, keylen =   53,                    ~
                        alt key  1, keypos =    1, keylen =  8            

            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1%), f2%(1%),1000%, rslt$(1%))
            call "OPENCHCK" (#2,  fs%(2%), f2%(2%),   0%, rslt$(2%))
            call "OPENCHCK" (#3,  fs%(3%), f2%(3%),1000%, rslt$(3%))
            call "OPENCHCK" (#4,  fs%(4%), f2%(3%),1000%, rslt$(4%))
            call "OPENCHCK" (#6,  fs%(6%), f2%(6%),1000%, rslt$(6%))      

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

            ecs_flag% = 0%  : authr% = 0%
            gosub authorized_user  /* CR2267 */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            f_no% = 1%
        inputmode_a
            for fieldnr% = f_no% to  17%
L10110:         gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L10215
/*<AWD008>*/          if fieldnr% = 1 then goto inputmode_purge
L10160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'051(fieldnr%)
                         if enabled% = 1% then L10130
                         if fieldnr% = 1% then L10110
                         goto L10160
L10215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% = 14% then goto inputmode_report
                      if keyhit% =  9% then goto inputmode_copy
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
/*EWD001*/        if keyhit%  >= 9% and keyhit% <= 11% then gosub show_xref
                  if keyhit%  = 12% then gosub delete_record
                  if keyhit%  = 16% then gosub dataput
/*EWD001*/        if keyhit%  >=19% and keyhit% <= 21% then gosub print_xref
/*EWD001*/        if keyhit%  = 31% then gosub change_xref
/*EWD001*/        if chg%    <>  0% then       inputmode
                  if keyhit% <>  0% then       editpg1
L11130:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% > 8% then  gosub check_field
            if fieldnr% < 1% or fieldnr% > 17% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11200:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11200
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11200
                  lastfieldnr% = fieldnr%
            goto L11130

        check_field
            ff% = fieldnr% - 8%
            if ff% > 3% then goto L11330
               if k_no% >= ff% then return         /* CHECK KEY VALUES */
                  goto L11340
L11330:     if v_no% >= (ff% - 3%) then return     /* CHECK DATA VALUES*/
L11340: fieldnr% = 0%
        return

        REM *************************************************************~
            *       I N P U T   M O D E   R E P O R T   S C R E E N     *~
            *************************************************************

        inputmode_report
            gosub initialize_variables

            for fieldnr% = 1% to  4%
L12080:         gosub'061(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L12200
L12100:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L12180
L12130:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'061(fieldnr%)
                         if enabled% = 1% then L12100
                         if fieldnr% = 1% then L12080
                         goto L12130
L12180:               if keyhit% = 16% and fieldnr% = 1% then inputmode
                      if keyhit% <> 0% then       L12100
L12200:         gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L12100
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   R E P O R T   S C R E E N      *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 10% then gosub print_report /*Flat File*/
                  if keyhit%  = 14% then gosub print_report
                  if keyhit%  = 16% then goto  inputmode
                  if keyhit% <>  0% then       editpg2
L13110:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 4% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'061(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L13160:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L13160
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13160
                  lastfieldnr% = fieldnr%
            goto L13110

        REM *************************************************************~
            *         I N P U T   M O D E   C O P Y   S C R E E N       *~
            *************************************************************

        inputmode_copy
            gosub initialize_variables
            for fieldnr% = 1% to  8%      /*<AWD007> */
                if fieldnr% <> 6 then goto L14050
                if rp_c_b$ <> "ALL" then goto L14050
                rp_c_e$ = "ALL"
                rp_c_ed$ = "A = All Catalog Codes                "
                goto next_field
L14050:         if fieldnr% <> 8 then goto L14080
                if rp_m_b$ <> "ALL" then goto L14080
                rp_m_e$ = "ALL"
                rp_m_ed$ = "*****  All Models                    "
                goto next_field
/*<AWD007> */
L14080:         gosub'071(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L14200
L14100:         gosub'103(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L14180
/*<AWD007>*/          if fieldnr% = 7 and rp_c_b$ = "ALL" then           ~
                         fieldnr% = 6

L14130:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'071(fieldnr%)
                         if enabled% = 1% then L14100
                         if fieldnr% = 1% then L14080
                         goto L14130
L14180:               if keyhit% = 16% and fieldnr% = 1% then inputmode
                      if keyhit% <> 0% then       L14100
L14200:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L14100
next_field:
            next fieldnr%

        REM *************************************************************~
            *          E D I T   M O D E   C O P Y   S C R E E N        *~
            *************************************************************

        editpg3
            lastfieldnr% = 0%
            gosub'103(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub copy_data
                  if keyhit%  = 10% then gosub copy_from_to
                  if keyhit%  = 16% then goto inputmode
                  if keyhit% <>  0% then       editpg3
L15110:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 8% then editpg3 /*<AWD007> */
            if fieldnr% = lastfieldnr% then    editpg3
/*<AWD007>*/if fieldnr% = 6 and rp_c_b$ = "ALL" then editpg3
/*<AWD007>*/if fieldnr% = 8 and rp_m_b$ = "ALL" then editpg3
            gosub'071(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg3
L15160:     gosub'103(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L15160
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L15160
                  lastfieldnr% = fieldnr%
            goto L15110

        REM *************************************************************~
            *     I N P U T   M O D E   C H A N G E   S C R E E N       *~
            *************************************************************

        inputmode_change                   /* (EWD001) - New */
                /* "Start Over" is the only way to Exit */

            cg_vl$, cg_c$, cg_c_d$, cg_m$, cg_m_d$, cg_st$, cg_st_d$,    ~
            del_fr_seq$, del_to_seq$, cg_vm$ = " "

            for fieldnr% = 1% to  4%
L16080:         gosub'081(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L16200
L16100:         gosub'104(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L16180
L16130:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'071(fieldnr%)
                         if enabled% = 1% then L16100
                         if fieldnr% = 1% then L16080
                         goto L16130  
L16180:               if keyhit% <> 0% then       L16100
L16200:         gosub'154(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L16100
            next fieldnr%

        REM *************************************************************~
            *      E D I T   M O D E   C H A N G E   S C R E E N        *~
            *************************************************************

        editpg4                            /* (EWD001) - New */
                /* "Start Over" is the only way to Exit */

            lastfieldnr% = 0%
            gosub'104(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then       return  /* Finished... */
                  if keyhit% <>  0% then       editpg4
L17110:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 4% then editpg4
            if fieldnr% = lastfieldnr% then    editpg4
            gosub'081(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg4
L17160:     gosub'104(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L17160
            gosub'154(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L17160
                  lastfieldnr% = fieldnr%
            goto L17110
            
            
        REM *************************************************************~
            *         I N P U T   M O D E   P U R G E   S C R E E N     *~
            *************************************************************
/*<AWD008>+*/
        inputmode_purge
            gosub initialize_variables
            for fieldnr% = 1% to  5%                           
P14080:         gosub'091(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then P14200
P14100:         gosub'105(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       P14180

P14130:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'091(fieldnr%)
                         if enabled% = 1% then P14100
                         if fieldnr% = 1% then P14080
                         goto P14130
P14180:               if keyhit% = 16% and fieldnr% = 1% then inputmode
                      if keyhit% <> 0% then       P14100
P14200:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then P14100
 
            next fieldnr%

        REM *************************************************************~
            *          E D I T   M O D E   P U R G E   S C R E E N      *~
            *************************************************************

        editpg5
            lastfieldnr% = 0%
            gosub'105(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub purge_data
                  if keyhit%  = 16% then goto inputmode
                  if keyhit% <>  0% then       editpg5
P15110:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 5% then editpg5
            if fieldnr% = lastfieldnr% then    editpg5
            gosub'091(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg5
P15160:     gosub'105(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then P15160
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then P15160
                  lastfieldnr% = fieldnr%
            goto P15110
/*<AWD008>-*/

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        print_report
           gosub generate_report
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

        deffn'061(fieldnr%)
            enabled% = 1%
            return

        deffn'071(fieldnr%)
            enabled% = 1%
            return

        deffn'081(fieldnr%)                 /* (EWD001) - New */
            enabled% = 1%
            return

        deffn'091(fieldnr%)                 /* (AWD008) - New */
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
         "Enter a Valid Pricing Record Number or Leave Blank,          ",~
         "Enter a Valid Price Record Status Code, Default = Inactive?  ",~
         "Enter a Valid Customer Catalog Code. 0000 = Default?         ",~
         "Enter a Valid Catalog Method Code?                           ",~
         "Enter a Valid APC Model Code, 000 = All Models?              ",~
         "Enter a Valid Reference Code for Standard or Special Ref's?  ",~
         "Enter a Valid Reference Calculation Method Code?             ",~
         "Enter a Valid Special Calc Code, or 000 = N/A?               ",~
         "Enter the Applicable Key Data for Key (1)?                   ",~
         "Enter the Applicable Key Data for Key (2)?                   ",~
         "Enter the Applicable Key Data for Key (3)?                   ",~
         "Enter the Applicable Data (1) Value?                         ",~
         "Enter the Applicable Data (2) Value?                         ",~
         "Enter the Applicable Data (3) Value?                         ",~
         "Enter the Applicable Data (4) Value?                         ",~
         "Enter the Applicable Data (5) Value?                         ",~
         "Enter the Applicable Data (6) Value?                         "

        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28400
                inpmessage$ = edtmessage$
                return

L28400
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter Status Code (A)=Active Only,(I)In-Active Only,(B)=Both?",~
         "Enter a Beg/End Catalog Code or (A)ll Catalogs?              ",~
         "Enter a Beg/End Catalog Method Code or (A)ll Methods?        ",~
         "Enter a Beg/End Model Code or (A)ll Models?                  "

        deffn'070(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28560
                inpmessage$ = edtmessage$
                return

L28560
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn3_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn3_msg  :  data                                               ~
/*<AWD007> */                                                            ~
/*       "Enter a Activate/Copy (From) Catalog Code?                   ",*/~
         "Enter a Activate/Copy (From) Catalog Code? ALL = All         ",~
/*<AWD007> */                                                            ~
         "Enter a Activate/Copy (From) Catalog Method Code?            ",~
         "Enter a Activate/Copy (From) Model Code? ALL = All          ",~
         "Enter a Activate/Copy (From) Ref Code.                       ",~
         "Enter a Activate/Copy (From) Price Ref Calc Code? ALL = All  ",~
         "Enter a Copy To Catalog Code?                                ",~
         "Enter a Copy To Catalog Method Code?                         ",~
         "Enter a Copy To Model Code?                                  "

        deffn'080(scrnr%, fieldnr%)           /* (EWD001) New - Begin */
            if fieldnr% <> 0% then L28700
                inpmessage$ = edtmessage$
                return

L28700
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn4_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn4_msg  :  data                                               ~
         "Enter New Value for Selected Xref/Value?                     ",~
         "Enter Specific Catalog Code or ALL?                          ",~
         "Enter Specific Model Code or ALL?                            ",~
         "Enter Status Code (A)=Active Only,(I)In-Active Only,(B)=Both?"

                                              /* (EWD001) New - End     */

        deffn'090(scrnr%, fieldnr%)           /* (AWD008) New - Begin */
            if fieldnr% <> 0% then P28700
                inpmessage$ = edtmessage$
                return

P28700
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn5_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn5_msg  :  data                                               ~
         "Enter a Purge Catalog Code? ALL = All         ",               ~
         "Enter a Purge Catalog Method Code?            ",               ~
         "Enter a Purge Model Code? ALL = All          ",                ~
         "Enter a Purge Ref Code.                       ",               ~
         "Enter a Purge Price Ref Calc Code? ALL = All  "
         
                                              /* (AWD008) New - End     */
         
                                              
        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, pc_ref$, pc_st$, pc_st_d$, ~
                      pc_c$, pc_c_d$, pc_cm$, pc_cm_d$, pc_m$, pc_m_d$,  ~
/* CR2289 */          pc_user$, pc_date$, pc_time$,                      ~                      
                      pc_r$, pc_r_d$, pc_rc$, pc_rc_d$,                  ~
/*<AWD007> */         rp_rcm$, rp_rcm_d$,                                ~
                      pc_kfld$(), pc_kfld_d$(), pc_vtbl$, pc_vtbl_d$,    ~
                      pc_vdesc$, pc_vdesc_d$, pc_vtblv$(),               ~
                      pc_vfmt$, pc_vfmt_d$, pc_vcalc$, pc_vcalc_d$,      ~
                      pc_kcalc$, pc_kcalc_d$, pc_kunt$, pc_kunt_d$,      ~
                      pc_kdesc$(), pc_k$, p_key$, pc_key$, pc_fil$,      ~
                      pc_spc$, pc_spc_d$, rp_st$, rp_c_b$, rp_c_e$,      ~
                      rp_cm_b$, rp_cm_e$, rp_m_b$, rp_m_e$, rp_c_bd$,    ~
                      rp_c_ed$, rp_cm_bd$, rp_cm_ed$, rp_m_bd$,rp_m_ed$, ~
                      from_key$, from_sav$, pc_rec$, st$, rp_rc$,        ~
                      rp_rc_d$, pc_vm$, rp_vm_b$, rp_vm_e$, usr_desc$,   ~
                      mdllfac$, mdllfac1$

            init(" ") kd$(), ik$(), ik_d$(), vd$(), pc_vl$()

            mat pc_kfld% = zer
            mat pc_kbeg% = zer
            mat pc_klen% = zer
            mat pc_vl    = zer
            lookup%, rpt%, copy%, copy_from%, pass%, chg% = 0%    /*EWD001*/

            file$     = "ECSPRICE"
            volume$   = "CARLOS"
            library$  = "APCDATA "
            ecs_flag% = 0%
            cnt%      = 0%
            purge_flag% = 0                         /*<AWD008>*/
            cnt$      = "[ Records Built = XXXXXXXX ]"
            
            
/* (CR2451) beg */
            mdllfac$, mdllfac1$ = hex(8c)
            virMdlSec% = 0%
            readkey$ = "PRICE 015" & userid$
            call "DESCRIBE" (#2, readkey$, usr_desc$, 0%, f1%(2))            
           
             convert str(usr_desc$,29%,2%) to virMdlSec%, data goto noPrice015
           

noPrice015: 
/* (CR2451) end */            
            
        return

        REM *************************************************************~
            *************************************************************

        REM *************************************************************~
            * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
            *-----------------------------------------------------------*~
            * Gives the User the ability to START OVER when he wants to *~
            * or will return User back to where they were.  Must push   *~
            * two buttons to start over for safety.                     *~
            *************************************************************

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            return clear all
/*EWD001*/  if pass% <> 0% then call "DELETE" (#2, tab_xref$, 9%) 
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
              lookup% = 1%
/* CR2289 add user id, date, time */
              get #f%, using L35040, pc_ref$, pc_st$, pc_c$, pc_cm$,     ~
                                   pc_m$, pc_r$, pc_rc$, pc_k$,          ~
                                   pc_vl(), pc_spc$, pc_user$, pc_date$, ~ 
                                   pc_time$, pc_fil$
              if copy% <> 0% then return        /* Skip for Copy       */

              pc_vm$ = str(pc_m$,4%,13%)                  /* (CR2451)  */
              pc_m$ = str(pc_m$,1%,3%)                    /* (CR2451)  */
              gosub L50500                       /* Status Code         */
              gosub L50620                       /* Catalog Code        */
              gosub L50760                       /* Catalog Method Code */
              gosub L50900                       /* Product Model Code  */
              gosub L51070                       /* Type Ref Code       */
              gosub L51210                       /* Type Ref Calc       */
              gosub L51470                       /* Spec Calc Code      */
              if err% <> 0% then return         /* Format Buffer Error */
              if k_no% = 0% then goto L30280
              j% = 1%
              for i% = 1% to k_no%
                  str(ik$(i%),1%,pc_klen%(i%)) =                         ~
                                             str(pc_k$, j%, pc_klen%(i%))
                  j% = j% + pc_klen%(i%)
              next i%

L30280:       if v_no% = 0% then goto L30320
              for i% = 1% to v_no%
                  convert pc_vl(i%) to pc_vl$(i%), pic(####.####-)
              next i%
L30320:       gosub L51710                         /* Key (1) Data      */
              gosub L51720                         /* Key (2) Data      */
              gosub L51730                         /* Key (3) Data      */
              gosub L52320                         /* Data Value (1)    */
              gosub L52330                         /* Data Value (2)    */
              gosub L52340                         /* Data Value (3)    */
              gosub L52350                         /* Data Value (4)    */
              gosub L52360                         /* Data Value (5)    */
              gosub L52370                         /* Data Value (6)    */

        assign% = 0%
        lookup% = 0%
        return

        REM *************************************************************~
            *   Check user ID if authorized to edit    (CR2267)         *~
            *************************************************************
         authorized_user
            authr% = 0%
            readkey$ = all(hex(00))
            str(readkey$,1%,9%) = "APCPRC02"
            str(readkey$,10%,15%) = userid$

            read #2, key = readkey$, using L30400, r_rec$,            ~
                                                         eod goto L30499
L30400:        FMT POS(25), CH(30)
              authr% = 1%
L30499:         
         return
         
        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        delete_record
/*EWD001*/    k% = 2%
/*   |  */    call "ASKUSER" (k%,"*** Confirm Delete ***","Press <ENTER>"~
/*   |  */       & " to Delete Record","- OR -","<PF1> to Abort Delete")
/*   |  */    if k% =  1% then return
/*EWD001*/    if k% <> 0% then delete_record
        dataput
              if assign% <> 0% then goto L31130
                 read #f%,hold,key 1% = pc_ref$, eod goto L31390
                 gosub data_key
                 goto L31160

L31130:       gosub lookup_key
copy_to_put:
              p_rec% = 0%
              read #f%,hold,key = p_key$, eod goto L31230
                                                  /* Set-Up for Update */
L31160:          get #f%, using L31170, pc_ref$
L31170:             FMT CH(08)
                 p_rec% = 1%
                 delete #f%
                
                 if keyhit% <> 12% then goto L31250
                    return clear all
                    goto inputmode
                                         /* Do Not Assign a New Number */
L31230:       if assign% <> 0% then gosub assign_record
              if copy_from% <> 0% and p_rec% = 0% then return
              
/* CR2289 add user id, date, time */              
L31250:          pc_user$ = userid$
                 pc_date$ = date
                 pc_time$ = time
                 put #f%, using L35040, pc_ref$, pc_st$, pc_c$, pc_cm$,    ~
                                        pc_m$, pc_r$, pc_rc$, pc_k$,       ~
                                        pc_vl(), pc_spc$, pc_user$,        ~
                                        pc_date$, pc_time$, pc_fil$                                        

              write #f%, eod goto writeError
                             
              if copy% <> 0% then return
              if copy_from% <> 0% then return
        return clear all
              if assign% = 0% then goto inputmode
              if k_no% = 0% then goto inputmode
                 init(" ") ik$(), pc_vl$(), ik_d$()
                 mat pc_vl = zer
                 f_no% = 9%
                 goto inputmode_a

L31390: return clear all
             stop "(Error) - Unable to Update Record --->  " & pc_ref$
             goto inputmode
             
writeError:
             stop "(Error) Writing Record??"
            close ws
        return clear all
        goto inputmode           

        REM *************************************************************~
            *       A S S I G N   N E X T   P R I C E   R E C   N O.    *~
            *************************************************************

        assign_record                                /* MOD - 12/07/93 */
            readkey$ = all(hex(00))
            str(readkey$,1%,9%) = "PRICE 000"
            str(readkey$,10%,15%) = "0000"

            read #2,hold,key = readkey$, using L31540, r_rec$,            ~
                                                           eod goto L31630
L31540:        FMT POS(25), CH(30)
            convert str(r_rec$,21%,8%) to pc_ref%, data goto L31630

            convert pc_ref% to pc_ref$, pic(00000000)
            pc_ref% = pc_ref% + 1%
            convert pc_ref% to str(r_rec$,21%,8%), pic(00000000)
            put #2, using L31540, r_rec$
            rewrite #2
        return
L31630:     stop "(Error) Allocating Next Price Record Number??"
            close ws
        return clear all
        goto inputmode

        lookup_key                        /* Lookup Key for (APCPCMST) */
            p_key$ = all(hex(00))
            str(p_key$,1%,1%)   = pc_st$     /* Status Code            */
            str(p_key$,2%,4%)   = pc_c$      /* Catlog Code Number     */
            str(p_key$,6%,2%)   = pc_cm$     /* Catalog Method Code    */
            str(p_key$,8%,16%)  = pc_m$      /* Product Model Code     */
            str(p_key$,24%,2%)  = pc_r$      /* Pricing Reference Code */
            str(p_key$,26%,3%)  = pc_rc$     /* Ref Code Calc Method   */
            gosub data_key
            str(p_key$,29%,25%) = pc_k$      /* Generic Key Calc Def.  */
            
            f% = 3%
            if pc_r$ = "01" or pc_r$ = "03" then f% = 6%
        return

        data_key                           /* Lookup Key for Reference */
            if copy% <> 0% then return     /* Skip for Copy            */
            j% = 1% : pc_k$ = " "          /* Price. IK$()=Screen Data */
                                           /* K_NO% = Number of Keys   */
                                           /* Defined (FORMAT_BUFFER)  */
            if k_no% = 0% then return
            for i% = 1% to k_no%
               str(pc_k$,j%,pc_klen%(i%)) = str(ik$(i%),1%,pc_klen%(i%))
               j% = j% + pc_klen%(i%)
            next i%
        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L35040: FMT                     /* (APCPCMST) Master Definition File   */~
            CH(08),             /* Pricing Logical Sequence Number     */~
            CH(01),             /* Price Record Status Code            */~
            CH(04),             /* Pricing Catalog Code - (PRICE 000)  */~
            CH(02),             /* Pricing Catalog Meth - (PRICE 001)  */~
            CH(16),             /* Pricing Model Code   - (MODEL    )  */~
/*          XX(13),                Filler From Model      (CR2451)     */~
            CH(02),             /* Std/Spec Ref Code    - (PRICE 002)  */~
            CH(03),             /* Ref. Calc Method Code- (PRICE 003)  */~
            CH(25),             /* Generic Key (Max Five (3) Fields)   */~
            6*PD(14,4),         /* Values Associated With Calc         */~
            CH(03),             /* Spcecial Calc. Code    (PRICE 010)  */~
            CH(03),             /* User ID                CR2289       */~
            CH(06),             /* System date of change               */~
            CH(06),             /* System time of change               */~ 
            CH(01)              /* Filler Area                         */

        FMT           /*(ECSPRICE) = DEFINITION   - VARIABLE           */~
            CH(04),   /*( 1)PRC CATALOG           - PC_C$              */~
            CH(02),   /*( 2)PRC CATALOG METHOD    - PC_CM$             */~
            CH(16),   /*( 3)PRC MODEL CODE        - PC_M$              */~
            CH(08),   /*( 4)NEW SERIES CODE       -                    */~
            CH(02),   /*( 5)PRC REF CODE (1)      - PC_R$              */~
            CH(03),   /*( 6)PRC REF CALC METHOD   - PC_RC$             */~
            CH(02),   /*( 7)PRC CALC PRIORITY     - PC_STD$(PC_R$)     */~
            CH(01),   /*( 8)PRC CROSS/REF OVERRIDE- PC_STDC$(PC_R$)    */~
            CH(03),   /*( 9)FIRST REF CALC        - PC_STDR$(PC_R$)    */~
            CH(03),   /*(10)SECOND REF CALC       - PC_STDR$(PC_R$)    */~
            CH(03),   /*(11)THIRD REF CALC        - PC_STDR$(PC_R$)    */~
            CH(03),   /*(12)SPECIAL CALC FOR REF  - PC_SPC$            */~
            CH(03),   /*(13)KEY 1 DESCRIPT        - PC_KDESC$(1%)      */~
            CH(02),   /*(14)KEY 1 FIELD NO.       - PC_KFLD%(1%)       */~
            CH(02),   /*(15)KEY 1 FIELD START     - PC_KBEG%(1%)       */~
            CH(02),   /*(16)KEY 1 FIELD LENGTH    - PC_KLEN%(1%)       */~
            CH(25),   /*(17)KEY 1 FIELD VALUE     - IK$(1%)            */~
            CH(03),   /*(18)KEY 2 DESCRIPT        - PC_KDESC$(2%)      */~
            CH(02),   /*(19)KEY 2 FIELD NO.       - PC_KFLD%(2%)       */~
            CH(02),   /*(20)KEY 2 FIELD START     - PC_KBEG%(2%)       */~
            CH(02),   /*(21)KEY 2 FIELD LENGTH    - PC_KLEN%(2%)       */~
            CH(25),   /*(22)KEY 2 FIELD VALUE     - IK$(2%)            */~
            CH(03),   /*(23)KEY 3 DESCRIPT        - PC_KDESC$(3%)      */~
            CH(02),   /*(24)KEY 3 FIELD NO.       - PC_KFLD%(3%)       */~
            CH(02),   /*(25)KEY 3 FIELD START     - PC_KBEG%(3%)       */~
            CH(02),   /*(26)KEY 3 FIELD LENGTH    - PC_KLEN%(3%)       */~
            CH(25),   /*(27)KEY 3 FIELD VALUE     - IK$(3%)            */~
            CH(02),   /*(28)KEY CALC METHOD       - PC_KCALC$          */~
            CH(02),   /*(29)KEY UNIT CONVERT      - PC_KUNT$           */~
            CH(02),   /*(30)VALUE FIELD NO.       - PC_VTBL$           */~
            CH(02),   /*(31)VALUE FIELD LENGTH    - PC_VTLEN%          */~
            CH(03),   /*(32)VALUE FIELD DESC      - PC_VDESC$          */~
            CH(02),   /*(33)VALUE FIELD FORMAT    - PC_VFMT$           */~
            CH(02),   /*(34)VALUE FIELD CALC METHOD - PC_VCALC$        */~
            CH(03),   /*(35)VALUE FIELD TABLE     - PC_VTBLV$(1%)      */~
            CH(10),   /*(36)PRICE VALUE (1)       - PC_VL(1%)          */~
            CH(03),   /*(37)VALUE FIELD TABLE     - PC_VTBLV$(2%)      */~
            CH(10),   /*(38)PRICE VALUE (2)       - PC_VL(2%)          */~
            CH(03),   /*(39)VALUE FIELD TABLE     - PC_VTBLV$(3%)      */~
            CH(10),   /*(40)PRICE VALUE (3)       - PC_VL(3%)          */~
            CH(03),   /*(41)VALUE FIELD TABLE     - PC_VTBLV$(4%)      */~
            CH(10),   /*(42)PRICE VALUE (4)       - PC_VL(4%)          */~
            CH(03),   /*(43)VALUE FIELD TABLE     - PC_VTBLV$(5%)      */~
            CH(10),   /*(44)PRICE VALUE (5)       - PC_VL(5%)          */~
            CH(03),   /*(45)VALUE FIELD TABLE     - PC_VTBLV$(6%)      */~
            CH(10),   /*(46)PRICE VALUE (6)       - PC_VL(6%)          */~
            CH(24)    /*(47)FREE AREA                                  */
            
P35040: FMT                     /* (APCPCMSD)-Master Calc. Definition  */~
            CH(04),             /* Pricing Catalog Code - (PRICE 000)  */~
            CH(02),             /* Pricing Calc. Method - (PRICE 001)  */~
            CH(16),             /* Model/Product Code   - (MODEL    )  */~
/*          XX(13),             /* Filler from Model                   */~
            20*BI(1),           /* Standard Priority Codes             */~
            20*CH(1),           /* Standard Price Cross-Ref Codes      */~
            20*CH(9),           /* Init Ref. Calc Codes (3),(3),(3)    */~
            20*BI(1),           /* Special Priority Codes              */~
            20*CH(1),           /* Special Price Cross-Ref Codes       */~
            20*CH(9),           /* Init Ref. Calc Codes (3),(3),(3)    */~
            20*BI(1),           /* Sub part sequence          (AWD006) */~
            20*CH(1),           /* Sub priority codes         (AWD006) */~
            20*CH(9),           /* Sub Ref calc codes         (AWD006) */~
            CH(63)              /* Filler Area                         */


        REM *************************************************************~
            *            L O A D   D I S P L A Y   S C R E E N S        *~
            *************************************************************

        lookup_tab_1                   /* Load Data for Display Screen */
            on tab_1% gosub p_1, p_2, p_3, p_4, p_5, p_6
               return
        p_1                                /* Lookup Pricing Catalogs  */
            tab_hdr$ = " Master Pricing Catalog Table "
            sav_key$ = "PRICE 000"
            goto L36310
        p_2                                /* Lookup Catalog Method Cod*/
            tab_hdr$ = " Pricing Methods for Catalogs "
            sav_key$ = "PRICE 001"
            goto L36310
        p_3                                /* Lookup Type Reference Cod*/
            tab_hdr$ = " Pricing Type Reference Codes "
            sav_key$ = "PRICE 002"
            goto L36310
        p_4                                /* Lookup Ref Calc Codes    */
            tab_hdr$ = "Standard/Special Ref Calc Code"
            sav_key$ = "PRICE 003"
            goto L36310
        p_5                                /* Lookup Valid Models      */
            tab_hdr$ = "     APC Valid Model Codes    "
            sav_key$ = "MODEL    "
            goto L36310
        p_6                                /* Lookup Special Calc Code */
            tab_hdr$ = " Special Calc Code for Product"
            sav_key$ = "PRICE 010"

L36310:     init(" ") readkey$
            str(readkey$,1%,9%) = sav_key$
            if tab_1% <> 4% then goto L36360
               str(readkey$,10%,2%) = pc_r$
               goto L36400
L36360:     if tab_1% <> 5% then goto L36400
               if len(pc_m$) = 0 then goto L36400
                  str(readkey$,10%,1%) = pc_m$

L36400:    descr$ = hex(06) & tab_hdr$
           call "PLOWCODE" (#2, readkey$, descr$, 9%, .30, f1%(2))
        return

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
L40070:       gosub'050(1%, fieldnr%)
              gosub set_pf1
              mat pc_vt_sc$ = pc_vtblv$             /* (EWD002) Begin */
              for z% = 1% to 6%
                if vd$(z%) = " " then pc_vt_sc$(z%) = " "
              next z%                               /* (EWD002) End   */
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40320,         /* Price Record No.  */   ~
                                L40310,         /* Price Record Stat */   ~
                                L40310,         /* Price Catalog     */   ~
                                L40310,         /* Price Catalog Meth*/   ~
                                L40310,         /* Price Model Code  */   ~
                                L40310,         /* Ref Std/Spec Code */   ~
                                L40310,         /* Ref Calc Code     */   ~
                                L40310,         /* Special Calc/Process*/ ~
                                L40310,         /* Key (1) Value     */   ~
                                L40310,         /* Key (2) Value     */   ~
                                L40310,         /* Key (3) Value     */   ~
                                L40310,         /* Price Value (1)   */   ~
                                L40310,         /* Price Value (2)   */   ~
                                L40310,         /* Price Value (3)   */   ~
                                L40310,         /* Price Value (4)   */   ~
                                L40310,         /* Price Value (5)   */   ~
                                L40310          /* Price Value (6)   */
/* (CR2451) */
              if virMdlSec% = 99% and fieldnr% = 5% then mdllfac$ = hex(81)
              if virMdlSec% = 99% and fieldnr% <> 5% then mdllfac$ = hex(8c)              
              goto L40340

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40310:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40320:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40340:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Price Calc. Record No.:",                    ~
               at (03,30), fac(lfac$( 1)), pc_ref$              , ch(08),~
                                                                         ~
               at (04,02), "Price Record Status  .:",                    ~
               at (04,30), fac(lfac$( 2)), pc_st$               , ch(01),~
               at (04,48), fac(hex(84)), pc_st_d$               , ch(30),~
                                                                         ~
               at (05,02), "Price Catalog Code    :",                    ~
               at (05,30), fac(lfac$( 3)), pc_c$                , ch(04),~
               at (05,48), fac(hex(84)), pc_c_d$                , ch(30),~
                                                                         ~
               at (06,02), "Price Catalog Method  :",                    ~
               at (06,30), fac(lfac$( 4)), pc_cm$               , ch(02),~
               at (06,48), fac(hex(84)), pc_cm_d$               , ch(30),~
                                                                         ~
               at (07,02), "APC Valid Model Code  :",                    ~
/*(SR80785)*/  at (07,30), fac(lfac$( 5)), pc_m$                , ch(03),~
/*(CR2451)*/   at (07,34), fac(mdllfac$), pc_vm$                , ch(13),~
               at (07,48), fac(hex(84)), pc_m_d$                , ch(30),~
                                                                         ~
               at (08,02), "Pricing Ref. Code     :",                    ~
               at (08,30), fac(lfac$( 6)), pc_r$                , ch(02),~
               at (08,48), fac(hex(84)), pc_r_d$                , ch(30),~
                                                                         ~
               at (09,02), "Pricing Ref. Calc Code:",                    ~
               at (09,30), fac(lfac$( 7)), pc_rc$               , ch(03),~
               at (09,48), fac(hex(84)), pc_rc_d$               , ch(30),~
                                                                         ~
               at (10,02), "Special Calc. Product :",                    ~
               at (10,30), fac(lfac$( 8)), pc_spc$              , ch(03),~
               at (10,48), fac(hex(84)), pc_spc_d$              , ch(30),~
                                                                         ~
               at (11,02), fac(hex(84)), kd$(1%)                , ch(20),~
               at (11,25), fac(lfac$( 9)),                               ~
                           str(ik$(1%), 1%, pc_klen%(1%))       , ch(25),~
               at (11,55), fac(hex(84)), ik_d$(1%)              , ch(20),~
                                                                         ~
               at (12,02), fac(hex(84)), kd$(2%)                , ch(20),~
               at (12,25), fac(lfac$(10)),                               ~
                           str(ik$(2%), 1%, pc_klen%(2%))       , ch(25),~
               at (12,55), fac(hex(84)), ik_d$(2%)              , ch(20),~
                                                                         ~
               at (13,02), fac(hex(84)), kd$(3%)                , ch(20),~
               at (13,25), fac(lfac$(11)),                               ~
                           str(ik$(3%), 1%, pc_klen%(3%))       , ch(25),~
               at (13,55), fac(hex(84)), ik_d$(3%)              , ch(20),~
                                                                         ~
               at (14,02), fac(hex(84)), vd$(1%)                , ch(30),~
               at (14,44), fac(lfac$(12)), pc_vl$(1%)           , ch(10),~
/*EWD002*/     at (14,55), fac(hex(8c)), pc_vt_sc$(1%)          , ch(03),~
                                                                         ~
               at (15,02), fac(hex(84)), vd$(2%)                , ch(30),~
               at (15,44), fac(lfac$(13)), pc_vl$(2%)           , ch(10),~
/*EWD002*/     at (15,55), fac(hex(8c)), pc_vt_sc$(2%)          , ch(03),~
                                                                         ~
               at (16,02), fac(hex(84)), vd$(3%)                , ch(30),~
               at (16,44), fac(lfac$(14)), pc_vl$(3%)           , ch(10),~
/*EWD002*/     at (16,55), fac(hex(8c)), pc_vt_sc$(3%)          , ch(03),~
                                                                         ~
               at (17,02), fac(hex(84)), vd$(4%)                , ch(30),~
               at (17,44), fac(lfac$(15)), pc_vl$(4%)           , ch(10),~
/*EWD002*/     at (17,55), fac(hex(8c)), pc_vt_sc$(4%)          , ch(03),~
                                                                         ~
               at (18,02), fac(hex(84)), vd$(5%)                , ch(30),~
               at (18,44), fac(lfac$(16)), pc_vl$(5%)           , ch(10),~
/*EWD002*/     at (18,55), fac(hex(8c)), pc_vt_sc$(5%)          , ch(03),~
                                                                         ~
               at (19,02), fac(hex(84)), vd$(6%)                , ch(30),~
               at (19,44), fac(lfac$(17)), pc_vl$(6%)           , ch(10),~
/*EWD002*/     at (19,55), fac(hex(8c)), pc_vt_sc$(6%)          , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L41160
                  call "PRNTSCRN"
                  goto L40070

L41160:        if keyhit% < 2% or keyhit% > 8% then goto L41230
                  if keyhit% = 4% then goto L41230
                  tab_1% = keyhit% - 1%
                  if keyhit% > 4% then tab_1% = keyhit% - 2%
                  gosub lookup_tab_1
                  goto L40070

L41230:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L41440     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "(7)Valid Models        (14)Print Report"
            pf$(2) = "(2)Catalogs      (5)Reference Types     " &        ~
                     "(8)Spec. Calc Codes    (15)Print Screen"
            pf$(3) = "(3)Cat. Methods  (6)Std/Spc Ref Calc's  " &        ~
                     "(9)Activate/Copy Data  (16)Exit Program"
            pfkeys$ = hex(010203040506070809ffffffff0e0f1000)
            if authr% = 1% then goto L41410    /* CR2267 */
/* CR2267 */            
            pf$(3) = "(3)Cat. Methods  (6)Std/Spc Ref Calc's  " &        ~
                     "                       (16)Exit Program"
            str(pf$(1),18,18) = "                  "
            pfkeys$ = hex(010203ff05060708ffffffffff0e0f1000)

L41410:     if fieldnr% = 1% then L41400
                str(pf$(1),64)    = " "  :  str(pfkeys$,14,1) = hex(ff)
                str(pf$(3),40,22) = " "  :  str(pfkeys$, 9,1) = hex(ff)
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L41400:     if fieldnr% > 1% then L41420
/*<AWD008>      str(pf$(1),18,18) = " "  :  str(pfkeys$, 4,1) = hex(ff)*/
/*<AWD008> CR2267*/if authr% = 1% then str(pf$(1),18,18) = "(4)Purge Data     "

            
L41420:     return

L41440: if fieldnr% > 0% then L41550  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over    (12)Delete Record      " &        ~
                     "(7)Valid Models        (11)Value Xref  "
            pf$(2) = "(2)Catalogs       (5)Reference Types    " &        ~
                     "(8)Spec. Calc Codes    (15)Print Screen"
            pf$(3) = "(3)Cat. Methods   (6)Std/Spc Ref Calc's " &        ~
/*EWD001*/           "(9/10)Model/Ref. Xref  (16)Save Data   "
            pfkeys$ = hex(010203ff05060708090a0b0cffff0f101314151f00)

            if authr% = 1% then goto L41535  /* CR2267 */
/* CR2267 */            
            pf$(1) = "(1)Start Over                           " &        ~
                     "(7)Valid Models        (11)Value Xref  "
            pf$(3) = "(3)Cat. Methods   (6)Std/Spc Ref Calc's " &        ~
                     "(9/10)Model/Ref. Xref                  "
            pfkeys$ = hex(010203ff05060708090a0bffffff0fff1314151f00)
            
L41535:     if assign% = 0% then goto L41540
                str(pf$(1),18,18) = " "  :  str(pfkeys$,12,1) = hex(ff)

L41540:     return
L41550:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "(7)Valid Models                        "
            pf$(2) = "(2)Catalogs       (5)Reference Types    " &        ~
                     "(8)Spec. Calc Codes                    "
            pf$(3) = "(3)Cat. Methods   (6)Std/Spc Ref Calc's " &        ~
                     "                                       "
            pfkeys$ = hex(010203ff05060708ffffffffffffffff00)
            return

        REM *************************************************************~
            *               R E P O R T   S C R E E N                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'060(1%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42180,         /* Status Codes-A/I/B  */ ~
                                L42180,         /* Beg/End Catalog Code*/ ~
                                L42180,         /* Beg/End Cat. Method */ ~
                                L42180          /* Beg/End Model Code  */
/* (CR2451) */
              if virMdlSec% = 99% and fieldnr% = 4% then mdllfac$ = hex(81)
              if virMdlSec% = 99% and fieldnr% <> 4% then mdllfac$ = hex(8c)                                   
              goto L42210

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42210:     accept                                                       ~
               at (01,02),                                               ~
          "Input Report Selection Criteria for Pricing Data Definitions",~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Price Status Codes:",                        ~
               at (03,25), fac(lfac$( 1)), rp_st$               , ch(01),~
               at (03,44), fac(hex(84)), rp_st_d$               , ch(30),~
                                                                         ~
               at (04,02), "Beg Catalog Code  :",                        ~
               at (04,25), fac(lfac$( 2)), rp_c_b$              , ch(04),~
               at (04,44), "End Catalog Code  :",                        ~
               at (04,63), fac(lfac$( 2)), rp_c_e$              , ch(04),~
                                                                         ~
               at (05,02), "Beg Catalog Method:",                        ~
               at (05,25), fac(lfac$( 3)), rp_cm_b$             , ch(02),~
               at (05,44), "End Catalog Method:",                        ~
               at (05,63), fac(lfac$( 3)), rp_cm_e$             , ch(02),~
                                                                         ~
               at (06,02), "Beg Model Code    :",                        ~
               at (06,25), fac(lfac$( 4)), rp_m_b$              , ch(03),~
/*(CR2451)*/   at (06,29), fac(mdllfac$), rp_vm_b$              , ch(13),~
               at (06,44), "End Model Code    :",                        ~
               at (06,63), fac(lfac$( 4)), rp_m_e$              , ch(03),~
/*(CR2451)*/   at (06,67), fac(mdllfac$), rp_vm_e$              , ch(13),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 10 then goto L42540
                  ecs_flag% = 1%

L42540:        if keyhit% <> 15 then goto L42580
                  call "PRNTSCRN"
                  goto L42210

L42580:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L42770     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L42730
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L42730:     if fieldnr% > 1% then L42750
                str(pf$(1),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L42750:     return

L42770: if fieldnr% > 0% then L42860  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "(10)Build Flat File    (14)Generate Rpt"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffff0affffff0e0f1000)
            return
L42860:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *           C O P Y   C A T A L O G   S C R E E N           *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
L45070:     gosub'070(1%, fieldnr%)
            gosub set_pf4
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            on fieldnr% gosub L45200,         /* Copy From Catalog Code*/ ~
                              L45200,         /* Copy From Cat. Method */ ~
                              L45200,         /* Copy From Model Code  */ ~
                              L45200,         /* Copy From Ref Code    */ ~
                              L45200,         /* Copy From PriceCalc M */ ~                              
                              L45200,         /* Copy To Catalog Code  */ ~
                              L45200,         /* Copy To Cat. Method   */ ~
                              L45200          /* Copy To Model Code    */
/* (CR2451) */
              if virMdlSec% = 99% and fieldnr% = 3% then mdllfac$ = hex(81)
              if virMdlSec% = 99% and fieldnr% = 8% then mdllfac1$ = hex(81)
              if virMdlSec% = 99% and fieldnr% <> 3% then mdllfac$ = hex(8c)                 
              if virMdlSec% = 99% and fieldnr% <> 8% then mdllfac1$ = hex(8c)                 
              goto L45230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L45200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L45230:     accept                                                       ~
               at (01,02),                                               ~
        "Activate/Copy All Pricing Data for Catalog/Cat. Method/Product",~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "(From) Catalog Code  :",                     ~
               at (03,25), fac(lfac$( 1)), rp_c_b$              , ch(04),~
               at (03,44), fac(hex(84)),   rp_c_bd$             , ch(30),~
                                                                         ~
               at (04,02), "       Cat. Method   :",                     ~
               at (04,25), fac(lfac$( 2)), rp_cm_b$             , ch(02),~
               at (04,44), fac(hex(84)),   rp_cm_bd$            , ch(30),~
                                                                         ~
               at (05,02), "       Model Code    :",                     ~
               at (05,25), fac(lfac$( 3)), rp_m_b$              , ch(03),~
/*(CR2451)*/   at (05,29), fac(mdllfac$), rp_vm_b$              , ch(13),~
               at (05,44), fac(hex(84)),   rp_m_bd$             , ch(30),~
                                                                         ~
               at (06,02), "       Ref Code      :",                     ~
               at (06,25), fac(lfac$( 4)), rp_rc$               , ch(02),~
               at (06,44), fac(hex(84)),   rp_rc_d$             , ch(30),~
/*<AWD007> */                                                            ~
               at (07,02), "       Ref Calc Code :",                     ~
               at (07,25), fac(lfac$( 5)), rp_rcm$              , ch(03),~
               at (07,44), fac(hex(84)),   rp_rcm_d$            , ch(30),~
/*<AWD007> */                                                            ~                              
                                                                         ~
               at (08,02), "( To ) Catalog Code  :",                     ~
               at (08,25), fac(lfac$( 6)), rp_c_e$              , ch(04),~
               at (08,40), fac(hex(84)),   rp_c_ed$             , ch(30),~
                                                                         ~
               at (09,02), "       Catalog Method:",                     ~
               at (09,25), fac(lfac$( 7)), rp_cm_e$             , ch(02),~
               at (09,40), fac(hex(84)),   rp_cm_ed$            , ch(30),~
                                                                         ~
               at (10,02), "       Model Code    :",                     ~
               at (10,25), fac(lfac$( 8)), rp_m_e$              , ch(03),~
/*(CR2451)*/   at (10,29), fac(mdllfac1$), rp_vm_e$             , ch(13),~
               at (10,40), fac(hex(84)),   rp_m_ed$             , ch(30),~
                                                                         ~
                                                                         ~
               at (12,11),                                               ~
          "Activate - Activates pricing data that has a Status Flag",    ~
               at (13,11),                                               ~
          "           of (I)nactive and matches the (From) data.",       ~
               at (14,11),                                               ~
          "                                                  ",          ~
               at (15,11),                                               ~
          "Copy     - Copies (A)ctive pricing data for the (FROM)",      ~
               at (16,11),                                               ~
          "           specified product data. The product data is ",     ~
               at (17,11),                                               ~
          "           copied to the (TO) specified product data with",   ~
               at (18,11),                                               ~
          "           the Status Flag of Data Copied is always",         ~
               at (19,11),                                               ~
          "           (A)ctive.                              ",          ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto L45810
                  call "PRNTSCRN"
                  goto L45070

L45810:        if keyhit% <> 2% and keyhit% <> 3% then goto L45860
                  tab_1% = keyhit% - 1%
                  gosub lookup_tab_1
                  goto L45070

L45860:        if keyhit% <> 7 then goto L45910
                  tab_1% = 5%
                  gosub lookup_tab_1
                  goto L45070

L45910:        if keyhit% <> 13 then goto L45940
                  gosub activate_data

L45940:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf4
        if edit% = 2% then L46150     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "(7)Valid Models        (13)Activate    "
            pf$(2) = "(2)Catalogs                             " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(3)Catalog Methods                      " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01020304ffff07ffffffffff0d0e0f1000)
            if fieldnr% = 1% then L46090
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L46090:     if fieldnr% > 1% then L46110
                str(pf$(1),18,18) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L46110:     if fieldnr% = 4% then goto L46130              
                str(pf$(1),64)    = " "  :  str(pfkeys$,13,1) = hex(ff)
L46130:     return

L46150: if fieldnr% > 0% then L46240  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "       (10)Copy Frm To (14)Copy Data   "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffff0affffff0e0f1000)
            return
L46240:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return
            
/*<AWD008>+*/
        REM *************************************************************~
            *           P U R G E   C A T A L O G   S C R E E N         *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'105(fieldnr%, edit%)
P45070:     gosub'090(1%, fieldnr%)
            gosub set_pf4P                          
            if fieldnr% > 0% then init(hex(8c)) lfac$()                  ~
                             else init(hex(86)) lfac$()
            on fieldnr% gosub P45200,         /* Copy From Catalog Code*/ ~
                              P45200,         /* Copy From Cat. Method */ ~
                              P45200,         /* Copy From Model Code  */ ~
                              P45200,         /* Copy From Ref Code    */ ~
                              P45200          /* Copy From PriceCalc M */                                
/* (CR2451) */
              if virMdlSec% = 99% and fieldnr% = 3% then mdllfac$ = hex(81)
              if virMdlSec% = 99% and fieldnr% <> 3% then mdllfac$ = hex(8c)  
                  goto P45230

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
P45200:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

P45230:     accept                                                       ~
               at (01,02),                                               ~
        "Purge All Pricing Data for Catalog/Cat. Method/Product",~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), " Purge Catalog Code  :",                     ~
               at (03,25), fac(lfac$( 1)), rp_c_b$              , ch(04),~
               at (03,44), fac(hex(84)),   rp_c_bd$             , ch(30),~
                                                                         ~
               at (04,02), "       Cat. Method   :",                     ~
               at (04,25), fac(lfac$( 2)), rp_cm_b$             , ch(02),~
               at (04,44), fac(hex(84)),   rp_cm_bd$            , ch(30),~
                                                                         ~
               at (05,02), "       Model Code    :",                     ~
               at (05,25), fac(lfac$( 3)), rp_m_b$              , ch(03),~
/*(CR2451)*/   at (05,29), fac(mdllfac$), rp_vm_b$              , ch(13),~
               at (05,44), fac(hex(84)),   rp_m_bd$             , ch(30),~
                                                                         ~
               at (06,02), "       Ref Code      :",                     ~
               at (06,25), fac(lfac$( 4)), rp_rc$               , ch(02),~
               at (06,44), fac(hex(84)),   rp_rc_d$             , ch(30),~
               at (07,02), "       Ref Calc Code :",                     ~
               at (07,25), fac(lfac$( 5)), rp_rcm$              , ch(03),~
               at (07,44), fac(hex(84)),   rp_rcm_d$            , ch(30),~
                                                                         ~
               at (12,11),                                               ~
          "Purging  - Purges pricing data for the specified Product",    ~
               at (13,11),                                               ~
          "           data above. Both Active and Inactive records",     ~
               at (14,11),                                               ~
          "           will be purged!                        ",          ~
               at (15,11),                                               ~
          "                                                      ",      ~
               at (16,11),                                               ~
          "                                                       ",     ~
               at (17,11),                                               ~
          "                                                         ",   ~
               at (18,11),                                               ~
          "                                                   ",         ~
               at (19,11),                                               ~
          "                                                  ",          ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15 then goto P45810
                  call "PRNTSCRN"
                  goto P45070

P45810:        if keyhit% <> 2% and keyhit% <> 3% then goto P45860
                  tab_1% = keyhit% - 1%
                  gosub lookup_tab_1
                  goto P45070

P45860:        if keyhit% <> 7 then goto P45910
                  tab_1% = 5%
                  gosub lookup_tab_1
                  goto P45070

P45910:        if keyhit% <> 13 then goto P45940
                  gosub activate_data

P45940:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf4P
        if edit% = 2% then P46150     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "(7)Valid Models                        "
            pf$(2) = "(2)Catalogs                             " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(3)Catalog Methods                      " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01020304ffff07ffffffffff0d0e0f1000)
            if fieldnr% = 1% then P46090
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
P46090:     if fieldnr% > 1% then P46110
                str(pf$(1),18,18) = " "  :  str(pfkeys$, 4,1) = hex(ff)
P46110:     if fieldnr% = 4% then goto P46130              
                str(pf$(1),64)    = " "  :  str(pfkeys$,13,1) = hex(ff)
P46130:     return

P46150: if fieldnr% > 0% then P46240  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Purge Data   "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
            return
P46240:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return
/*<awd008>-*/

        REM *************************************************************~
            *            C H A N G E   V A L U E   S C R E E N          *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'104(fieldnr%, edit%)          /* (EWD001) New - Begin */
              gosub'080(1%, fieldnr%)
              gosub set_pf5
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              if edit%=1% then lfac$(21)=hex(82) else lfac$(21)=hex(9c)
              on fieldnr% gosub L47190,         /* New Value for Xref  */ ~
                                L47180,         /* Specific Catalog/ALL*/ ~
                                L47180,         /* Specific Model/ALL  */ ~
                                L47180          /* Status Code - A/I/B */
/* (CR2451) */
              if virMdlSec% = 99% and fieldnr% = 3% then mdllfac$ = hex(81)
              if virMdlSec% = 99% and fieldnr% <> 3% then mdllfac$ = hex(8c)                                  
              goto L47210

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L47180:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L47190:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L47210:     accept                                                       ~
               at (01,02),                                               ~
          "Input Change Selection Criteria for Pricing Data Definitions",~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "New Value     :",                            ~
               at (03,25), fac(lfac$( 1)), cg_vl$               , ch(10),~
               at (03,36), "for",                                        ~
               at (03,44), fac(hex(84)), descr_save$            , ch(30),~
                                                                         ~
               at (04,02), "Catalog Code  :",                            ~
               at (04,25), fac(lfac$( 2)), cg_c$                , ch(04),~
               at (04,44), fac(hex(84)), cg_c_d$                , ch(30),~
                                                                         ~
               at (05,02), "Model Code    :",                            ~
               at (05,25), fac(lfac$( 3)), cg_m$                , ch(03),~
/*(CR2451)*/   at (05,29), fac(mdllfac$), cg_vm$                , ch(13),~
               at (05,44), fac(hex(84)), cg_m_d$                , ch(30),~
                                                                         ~
               at (06,02), "Price Status Codes:",                        ~
               at (06,25), fac(lfac$( 4)), cg_st$               , ch(01),~
               at (06,44), fac(hex(84)), cg_st_d$               , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1)               , ch(79),~
               at (22,53), fac(lfac$(21)), del_fr_seq$          , ch(08),~
               at (22,65), fac(lfac$(21)), del_to_seq$          , ch(08),~
               at (23,02), fac(hex(8c)),   pf$(2)               , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3)               , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 12% then goto L47650
                  if del_fr_seq$ > del_to_seq$ then goto L47210
                  if del_fr_seq$ = " " or del_to_seq$ = " " then L47210
                  convert del_fr_seq$ to temp%, data goto L47210
                  convert temp% to del_fr_seq$, pic(00000000)
                  convert del_to_seq$ to temp%, data goto L47210
                  convert temp% to del_to_seq$, pic(00000000)
                  gosub rec_range_delete
                  goto  L47210

L47650:        if keyhit% <> 15 then goto L47680
                  call "PRNTSCRN"
                  goto L47210

L47680:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf5
        if edit% = 2% then L47770     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "(12)Delete xxxxxxxx to xxxxxxxx        "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffff04ffffffffffffff0cffff0fff00)
            if fieldnr% > 1% then L47750
                str(pf$(1),18,17) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L47750:     return

L47770: if fieldnr% > 0% then L47860  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)CHANGE VALUE"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return
L47860:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

                                            /* (EWD001) New - End */

        REM *************************************************************~
            *       D a t a   I n p u t   S c r e e n   E d i t s       *~
            *-----------------------------------------------------------*~
            * Edits for Items on Screen 1                               *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
              on fieldnr% gosub L50280,         /* Price Calc Rec No */   ~
                                L50500,         /* Price Status Code */   ~
                                L50620,         /* Price Catalog Code*/   ~
                                L50760,         /* Price Catalog Method*/ ~
                                L50900,         /* Model Code        */   ~
                                L51070,         /* Price Type Ref Code*/  ~
                                L51210,         /* Type Ref Calc Code  */ ~
                                L51470,         /* Spec. Calc Codes  */   ~
                                L51710,         /* Key (1) Data      */   ~
                                L51720,         /* Key (2) Data      */   ~
                                L51730,         /* Key (3) Data      */   ~
                                L52320,         /* Data Value (1)    */   ~
                                L52330,         /* Data Value (2)    */   ~
                                L52340,         /* Data Value (3)    */   ~
                                L52350,         /* Data Value (4)    */   ~
                                L52360,         /* Data Value (5)    */   ~
                                L52370          /* Data Value (6)    */

            return

L50280: REM Price Record Number                   PC_REF$
           assign% = 0%
           if pc_ref$ <> " " then goto L50340
              assign% = 1%
              return

L50340:    convert pc_ref$ to pc_ref%, data goto L50450

           convert pc_ref% to pc_ref$, pic(00000000)

           gosub findFile
           if rec% = 0% then goto L50450
           
           f% = 3%
           if rec% = 2% then f% = 6%
           gosub dataload
           if err% <> 0% then goto L50430
              fieldnr% = 17%
        return
L50430:    errormsg$ = "(Error) Unable to Decode Record?"
           goto L50460
L50450:    errormsg$ = "(Error) Pricing Record Not on File?"
L50460:    pc_ref$ = " "
           assign% = 1%
        return
        findFile
        rec% = 0%
          read #3,key 1% = pc_ref$, eod goto findFile2
            rec% = 1%
        return
        findFile2
          read #6,key 1% = pc_ref$, eod goto NoFindFile
            rec% = 2%
        NoFindFile
        return

L50500: REM Price Record Status Code              PC_ST$
            if pc_st$ <> " " then goto L50530
               pc_st$ = "I"
L50530:     pc_st_d$ = " "
            if pc_st$ = "A" then pc_st_d$ = "Active Record   "
            if pc_st$ = "I" then pc_st_d$ = "In-Active Record"
            if pc_st$ <> "A" and pc_st$ <> "I" then goto L50580
        return
L50580:     errormsg$ = "(Error) - Invalid Record Status Code?"
            pc_st$, pc_st_d$ = " "
        return

L50620: REM Price Catalog                         PC_C$
            if pc_c$ <> " " then goto L50650
               pc_c$ = "0000"
L50650:     tab_1% = 10% : tab_2% = 0%
            tab_val$ = pc_c$
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% then goto L50720
               pc_c_d$ = tab_desc$
        return
L50720:     errormsg$ = "Invalid Price Catalog Code?"
            pc_c$, pc_c_d$ = " "
        return

L50760: REM Price Catalog Method Code             PC_CM$
            if pc_cm$ <> " " then goto L50790
               pc_cm$ = "00"
L50790:     tab_1% =  1% : tab_2% = 0%
            tab_val$ = pc_cm$
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% then goto L50860
               pc_cm_d$ = tab_desc$
        return
L50860:     errormsg$ = "Invalid Price Catalog Method Code?"
            pc_cm$, pc_cm_d$ = " "
        return

L50900: REM Model Code                            PC_M$
            if pc_m$ <> " " then goto L50950
L50920:        pc_m$ = "000"
               pc_m_d$ = "(000) = All Models"
               return
L50950:     if pc_m$ = "000" then goto L50920
            tab_1% = 0% : tab_2% = 1%
            tab_val$ = str(pc_m$,1,3)
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% then goto L51030
               pc_m_d$ = tab_desc$
/* (CR2451) begin */               
            if pc_vm$ = " " then return
              pc_m$ = pc_m$ & pc_vm$
              init(" ") readkey$, descr$
              readkey$ = "VIRTUALMD" & pc_m$ & " "
              call "DESCRIBE" (#2, readkey$, str(descr$,,19%) ,0%, f1%(2)) 
                if descr$ = " " then goto L51030
/* (CR2451) end   */                
        return
L51030:     errormsg$ = "Invalid Product Line Model Code?"
            pc_m$, pc_m_d$ = " "
        return

L51070: REM Pricing Type Reference Code           PC_R$
            if pc_r$ <> " " then goto L51100
               pc_r$ = "01"
L51100:     tab_1% = 2% : tab_2% = 0%
            tab_val$ = pc_r$
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% then goto L51030
               pc_r_d$ = tab_desc$
        return
            errormsg$ = "Invalid Type Reference Code ?"
            pc_r$, pc_r_d$ = " "
        return

L51210: REM Pricing Type Reference Calc Code      PC_RC$
            if pc_rc$ <> " " then goto L51240
               pc_rc$ = "000"
L51240: REM - Verify for All Model Selection - '000' = PC_M$
            if pc_m$ <> "000" then goto L51280
               if pc_r$ = "01" and pc_rc$ = "000" then goto L51420

L51280:     convert pc_rc$ to pc_rc%, data goto L51290
            convert pc_rc% to pc_rc$, pic(000)
L51290:     readkey$ = " "
            str(readkey$,1%,9%)   = "PRICE 003"
            str(readkey$,10%,2%)  = pc_r$
            str(readkey$,12%,13%) = pc_rc$
            read #2,key = readkey$, using L51360, tab_desc$,eod goto L51390
L51360:         FMT POS(25), CH(30)
            pc_rc_d$ = tab_desc$
        return
L51390:     errormsg$ = "Invalid Type Reference Calc Code?"
            pc_rc$, pc_rc_d$ = " "
        return
L51420:    errormsg$ = "(Error) - Invalid Model Selection For Calc Code?"
           init(" ") pc_rc$, pc_rc_d$, pc_r$, pc_r_d$, pc_m$, pc_m_d$
           fieldnr% = 5%
        return

L51470: REM Special Calc. Code                    PC_SPC$
            if pc_spc$ <> " " then goto L51530
               pc_spc$ = "000"
            convert pc_spc$ to pc_spc%, data goto L51660

            convert pc_spc% to pc_spc$, pic(000)
L51530:     tab_2% = 0% : tab_1% = 11%
            tab_val$ = pc_spc$
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% then goto L51630
               pc_spc_d$ = tab_desc$
            gosub format_buffer                      /* (EWD004)        */
            if err% <> 0% then goto L51660
            if k_no% = 0% and edit% = 1% then fieldnr% = fieldnr% + 3%
            if k_no% = 0% and edit% = 1% then gosub L52120      /*EWD003*/
        return
L51630:     errormsg$ = "Invalid Special Calc. ?"
            pc_spc$, pc_spc_d$ = " "
        return
L51660:     errormsg$ = "(Error)-Unable to Decode Lookup Specifications?"
            pc_spc$, pc_spc_d$ = " "
        return

        REM Key (1) thru (3) Edit
L51710:    x% = 1% : goto L51750
L51720:    x% = 2% : goto L51750
L51730:    x% = 3%

L51750:    if k_no% = 0% then L52120 /*EWD003*/ /* NO KEY DATA DEFINED */

           if k_no% >= x% then  goto L51820
              fieldnr% = 11%
              if v_no% = 0% then fieldnr% = 17%
              return

L51820:    if pc_klen%(x%) > 5% then goto L52110
              xx% = 0%
              rr$ = str(ik$(x%),1%,pc_klen%(x%))
              rr  = pos(rr$ = "-")
              if rr <> 0 then rr$ = str(rr$,1%,rr-1)
/* (SR67154) added str(,pc_klen%(x%) */              	
              convert str(rr$,1%,pc_klen%(x%)) to xx%, data goto L52017

              if rr <> 0 then xx% = (-1.0) * xx%

              zz% = pc_klen%(x%)
              on zz% goto L51890, L51910, L51930, L51970, L52010

L51890:       convert xx% to str(ik$(x%),1%,pc_klen%(x%)), pic(0)
           goto L52030
L51910:       convert xx% to str(ik$(x%),1%,pc_klen%(x%)), pic(00)
           goto L52030
L51930:       convert xx% to str(ik$(x%),1%,pc_klen%(x%)), pic(000)
              if pc_r$ = "01" and pc_rc$ = "000" then                    ~
                   convert xx% to str(ik$(x%),1%,pc_klen%(x%)), pic(00-)
           goto L52030
L51970:       convert xx% to str(ik$(x%),1%,pc_klen%(x%)), pic(0000)
              if pc_r$ = "01" and pc_rc$ = "000" then                    ~
                  convert xx% to str(ik$(x%),1%,pc_klen%(x%)), pic(000-)
           goto L52030
L52010:       convert xx% to str(ik$(x%),1%,pc_klen%(x%)), pic(00000)
           goto L52030
        REM - ENTRY NOT NUMERIC
L52017:       str(ik$(x%),1%,pc_klen%(x%)) = rr$
                                                  
L52030:    if pc_kfld%(x%) < 1% or pc_kfld%(x%) > 7% then goto L52110
              tab_val$ = str(ik$(x%),1%,pc_klen%(x%) )
              tab_1% = 0% : tab_2% = pc_kfld%(x%)

              call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,  ~
                                                           #2, tab_rec% )
        REM   IF TAB_REC% = 0% THEN GOTO 52110       /* MOD - 04/17/97 */
                                                  
              if tab_rec% = 0% then goto L52240
                 zz% = len(str(tab_desc$,p%+1%))
                 ik_d$(x%) = str(tab_desc$,p%+1%,zz%)

L52110:    if k_no% <> x% then  goto L52230
L52120:       if lookup% = 1% then return
              if edit% <> 1% then return
              fieldnr% = 11%
              if v_no% = 0% then fieldnr% = 17%
              gosub lookup_key
              read #f%,key = p_key$, eod goto L52230
                 gosub dataload
                 if err% <> 0% then goto L52270
                 fieldnr% = 17%
L52230: return
L52240:       errormsg$ = "(Error) - Invalid Key Data for "&kd$(x%)
              ik$(x%), ik_d$(x%) = " "
        return
L52270:       errormsg$ = "(Error) - Unable to Decode ("&pc_ref$&")"
              ik$(x%), ik_d$(x%) = " "
        return
                                
        REM Value (1) thru (6) Edit


L52320:    x% = 1% : goto L52390
L52330:    x% = 2% : goto L52390
L52340:    x% = 3% : goto L52390
L52350:    x% = 4% : goto L52390
L52360:    x% = 5% : goto L52390
L52370:    x% = 6%

L52390: REM    STOP " CHECK FORMAT"
        REM    CLOSE WS
           if v_no% = 0% then return
           if v_no% >= x% then  goto L52450
              pc_vl(x%) = 0.0
              pc_vl$(x%) = " "
              fieldnr% = 17%
              return
L52450:    pc_vl(x%) = 0.0
           convert pc_vl$(x%) to pc_vl(x%), data goto L52470
L52470:
           convert pc_vfmt$ to zz%, data goto L52640

           on zz% goto L52520, L52540, L52560, L52580

L52520:       convert pc_vl(x%) to pc_vl$(x%), pic(##,###.##-)
           goto L52600
L52540:       convert pc_vl(x%) to pc_vl$(x%), pic(####.####-)
           goto L52600
L52560:       convert pc_vl(x%) to pc_vl$(x%), pic(#########-)
           goto L52600
L52580:       convert pc_vl(x%) to pc_vl$(x%), pic($#,###.##-)

L52600:    if v_no% <> x% then  goto L52630
              if edit% <> 1% then return
              fieldnr% = 17%
L52630: return
L52640:    errormsg$ = "(Error) - Invalid Value for " & vd$(x%)
           pc_vl$(x%) = " " : pc_vl(x%) = 0.0
        return

        REM *************************************************************~
            *           R e p o r t   S c r e e n   E d i t s           *~
            *************************************************************

        deffn'152(fieldnr%)
            errormsg$ = " "
              on fieldnr% gosub L52800,         /* Price Stat Code     */ ~
                                L52960,         /* Beg/End Cat Code    */ ~
                                L53260,         /* Beg/End Cat Method  */ ~
                                L53560          /* Beg/End Model Code  */
        return

L52800: REM Pricing Status Code                RP_ST$, RP_ST_D$
            if rp_st$ <> " " then goto L52830
               rp_st$ = "B"
L52830:     if rp_st$ <> "A" and rp_st$ <> "I" and rp_st$ <> "B" then    ~
                                               goto L52920
            if rp_st$ = "A" then                                         ~
                            rp_st_d$ = "(A)-Active Price Definitions  "
            if rp_st$ = "I" then                                         ~
                            rp_st_d$ = "(I)-Inactive Price Definitions"
            if rp_st$ = "B" then                                         ~
                            rp_st_d$ = "(B)-Both Active/Inactive      "
        return
L52920:     errormsg$ = "(Error) - Invalid Selection? A, I, or B "
            rp_st$, rp_st_d$ = " "
        return

L52960: REM Beginning/Ending Catalog Code         RP_C_B$, RP_C_E$
            if rp_c_b$ <> " " then goto L53010
L52980:        rp_c_b$ = "ALL "
               rp_c_e$ = "    "
               return
L53010:     if str(rp_c_b$,1%,1%) = "A" then goto L52980
            convert rp_c_b$ to y%, data goto L53220

            convert y% to rp_c_b$, pic(0000)
            tab_2% = 0% : tab_1% = 10%
            tab_val$ = rp_c_b$
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% then goto L53220
            if rp_c_e$ <> " " then goto L53120
               rp_c_e$ = rp_c_b$
L53120:     convert rp_c_e$ to y%, data goto L53220

            convert y% to rp_c_e$, pic(0000)

            tab_2% = 0% : tab_1% =10%
            tab_val$ = rp_c_e$
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% then goto L53220
        return
L53220:     errormsg$ = "(Error) Invalid Beg/End Catalog Code?"
            rp_c_b$, rp_c_e$ = " "
        return

L53260: REM Beginning/Ending Catalog Method       RP_CM_B$, RP_CM_E$
            if rp_cm_b$ <> " " then goto L53310
L53280:        rp_cm_b$ = "A "
               rp_cm_e$ = "  "
               return
L53310:     if str(rp_cm_b$,1%,1%) = "A" then goto L53280
            convert rp_cm_b$ to y%, data goto L53520

            convert y% to rp_cm_b$, pic(00)
            tab_2% = 0% : tab_1% = 1%
            tab_val$ = rp_cm_b$
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% then goto L53520
            if rp_cm_e$ <> " " then goto L53420
               rp_cm_e$ = rp_cm_b$
L53420:     convert rp_cm_e$ to y%, data goto L53520

            convert y% to rp_cm_e$, pic(00)

            tab_2% = 0% : tab_1% = 1%
            tab_val$ = rp_cm_e$
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% then goto L53520
        return
L53520:     errormsg$ = "(Error) Invalid Beg/End Catalog Method Code?"
            rp_cm_b$, rp_cm_e$ = " "
        return

L53560: REM Beginning/Ending Model Code           RP_M_B$, RP_M_E$
            if rp_m_b$ <> " " then goto L53610
L53580:        rp_m_b$ = "ALL"
               rp_m_e$ = " "
               return
L53610:     if str(rp_m_b$,1%,2%) = "AL" then goto L53580
            goto L53585        /* <EWD006> */
            if str(rp_m_b$,1%,1%) = "9" then goto L53585   /* (EWD005) */  
            convert rp_m_b$ to y%, data goto L53820
            convert y% to rp_m_b$, pic(000)

L53585:     tab_2% = 1% : tab_1% = 0%
            tab_val$ = str(rp_m_b$,1%,3%)
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% and y% <> 0% then goto L53820
/* (CR2451) begin */               
            if rp_vm_b$ = " " then goto L53590
              rp_m_b$ = rp_m_b$ & rp_vm_b$
              init(" ") readkey$, descr$
              readkey$ = "VIRTUALMD" & rp_m_b$ & " "
              call "DESCRIBE" (#2, readkey$, str(descr$,,19%) ,0%, f1%(2)) 
                if descr$ = " " then goto L53820
L53590:                
/* (CR2451) end   */              
            if rp_m_e$ <> " " then goto L53720
               rp_m_e$ = rp_m_b$
L53720:     if str(rp_m_e$,1%,1%) = "9" then goto L53725   /* (EWD005) */

            goto L53725        /* <EWD006> */
            convert rp_m_e$ to y%, data goto L53520
            convert y% to rp_m_e$, pic(000)

L53725:     tab_2% = 1% : tab_1% = 0%
            tab_val$ = str(rp_m_e$,1%,3%)
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% and y% <> 0% then goto L53820
/* (CR2451) begin */               
            if rp_vm_e$ = " " then return
              rp_m_e$ = rp_m_e$ & rp_vm_e$
              init(" ") readkey$, descr$
              readkey$ = "VIRTUALMD" & rp_m_e$ & " "
              call "DESCRIBE" (#2, readkey$, str(descr$,,19%) ,0%, f1%(2)) 
                if descr$ = " " then goto L53820
/* (CR2451) end   */                          
        return
L53820:     errormsg$ = "(Error) Invalid Beg/End Model Code ?"
            rp_m_b$, rp_m_e$ = " "
        return

        REM *************************************************************~
            *       D a t a   I n p u t   S c r e e n   E d i t s       *~
            *-----------------------------------------------------------*~
            * Edits for Copy Screen                                     *~
            *************************************************************

        deffn'153(fieldnr%)
            errormsg$ = " "
              on fieldnr% gosub L54030,         /* Copy From Catalog   */ ~
                                L54170,         /* Copy From Cat Method*/ ~
                                L54310,         /* Copy From Model     */ ~
                                L54450,         /* Copy From Ref Code  */ ~
                                L54470,         /* Copy From Price Code*/ ~
                                L54480,         /* Copy To Catalog     */ ~
                                L54620,         /* Copy To Cat Method  */ ~
                                L54760          /* Copy To Model Code  */

            return

L54030: REM Price Catalog (From)                  RP_C_B$
/*<AWD007> */
            if rp_c_b$ <> "ALL" then goto L54035
            tab_desc$, rp_c_bd$ = "A = All Catalog Codes                "
        return
L54035:
/*<AWD007> */

            if rp_c_b$ <> " " then goto L54060
               rp_c_b$ = "0000"
L54060:     tab_1% = 10% : tab_2% = 0%
            tab_val$ = rp_c_b$
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% then goto L54130
               rp_c_bd$ = tab_desc$
        return
L54130:     errormsg$ = "Invalid Price Catalog Code?"
            rp_c_b$, rp_c_bd$ = " "
        return

L54170: REM Price Catalog Method Code (From)      RP_CM_B$
            if rp_cm_b$ <> " " then goto L54200
               rp_cm_b$ = "00"
L54200:     tab_1% =  1% : tab_2% = 0%
            tab_val$ = rp_cm_b$
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% then goto L54270
               rp_cm_bd$ = tab_desc$
        return
L54270:     errormsg$ = "Invalid Price Catalog Method Code?"
            rp_cm_b$, rp_cm_bd$ = " "
        return

L54310: REM Model Code (From)                     RP_M_B$
            if rp_m_b$ <> "ALL" then goto L54320        /*EWD001*/
               rp_m_bd$ = "*****  All Models"           /*EWD001*/
               return                                   /*EWD001*/
L54320:     if rp_m_b$ <> " " then goto L54360
L54330:        rp_m_b$ = "000"
               rp_m_bd$ = "(000) = All Models Catalog"  /*EWD001*/
               return
L54360:     if rp_m_b$ = "000" then goto L54330
            tab_1% = 0% : tab_2% = 1%
            tab_val$ = str(rp_m_b$,1%,3%)
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% then goto L54440
               rp_m_bd$ = tab_desc$
/* (CR2451) begin */               
            if rp_vm_b$ = " " then return
              rp_m_b$ = rp_m_b$ & rp_vm_b$
              init(" ") readkey$, descr$
              readkey$ = "VIRTUALMD" & rp_m_b$ & " "
              call "DESCRIBE" (#2, readkey$, str(descr$,,19%) ,0%, f1%(2)) 
                if descr$ = " " then goto L54440               
        return
L54440:     errormsg$ = "Invalid Product Line Model Code?"
            rp_m_b$, rp_m_bd$ = " "
        return

L54450: REM Ref Code (From)                       RP_RC$
/*          if rp_m_b$ = "ALL" then rp_rc$ = " "          EWD001*/
            if rp_rc$ <> " " then L54455
                rp_rc_d$ = "*****  All Ref Codes"
                return
L54455:     tab_1% = 2% : tab_2% = 0%
            tab_val$ = rp_rc$
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec%)
            if tab_rec% = 0% then goto L54460
                rp_rc_d$ = tab_desc$
                return
L54460:     errormsg$ = "Invalid Reference Code"
            rp_rc$ = " " : rp_rc_d$ = " "
            return
            
L54470: REM Price Ref Calc Code (From)            RP_RCM$
/*<AWD007> */
            if RP_RCM$ <> "ALL" and RP_RCM$ <> "   " then        ~
                 goto not_all_price_calcs
            RP_RCM$ = "ALL"
            rp_rcm_d$ = "All Price Ref Calc Methods  "
            return
not_all_price_calcs:
            if RP_RCM$ <> " " then goto L54471
               RP_RCM$ = "000"
L54471: REM - Verify for All Model Selection - '000' = PC_M$
            if rp_m_b$ <> "000" then goto L54472
               if rp_rc$ = "01" and rp_rcm$ = "000" then goto L54476

L54472:     convert pc_rc$ to pc_rc%, data goto L54473
            convert pc_rc% to pc_rc$, pic(000)
L54473:     readkey$ = " "
            str(readkey$,1%,9%)   = "PRICE 003"
            str(readkey$,10%,2%)  = rp_rc$
            str(readkey$,12%,13%) = rp_rcm$
            read #2,key = readkey$, using L54474, tab_desc$,eod goto L54475
L54474:         FMT POS(25), CH(30)
            rp_rcm_d$ = tab_desc$
        return
L54475:     errormsg$ = "Invalid Type Reference Calc Code?"
            rp_rcm$, rp_rcm_d$ = " "
        return
L54476:    errormsg$ = "(Error) - Invalid Model Selection For Calc Code?"
              init(" ") rp_rcm$, rp_rcm_d$
           fieldnr% = 5%
        return
/*<AWD007> */

L54480: REM Price Catalog  (To)                   RP_C_E$
/*<AWD007> */
            if rp_c_b$ = "ALL" then rp_c_e$ = "ALL"
            if rp_c_e$ <> "ALL" then goto L54490
               rp_c_ed$ = "All Catalogs               "
               return
/*<AWD007> */
L54490:     if rp_c_e$ <> " " then goto L54510
               rp_c_e$ = "0000"
L54510:     tab_1% = 10% : tab_2% = 0%
            tab_val$ = rp_c_e$
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% then goto L54580
               rp_c_ed$ = tab_desc$
        return
L54580:     errormsg$ = "Invalid Price Catalog Code?"
            rp_c_e$, rp_c_ed$ = " "
        return

L54620: REM Price Catalog Method Code (To)        RP_CM_E$
            if rp_cm_e$ <> " " then goto L54650
               rp_cm_e$ = "00"
L54650:     tab_1% =  1% : tab_2% = 0%
            tab_val$ = rp_cm_e$
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% then goto L54720
               rp_cm_ed$ = tab_desc$
        return
L54720:     errormsg$ = "Invalid Price Catalog Method Code?"
            rp_cm_e$, rp_cm_ed$ = " "
        return

L54760: REM Model Code (To)                       RP_M_E$
            if rp_m_b$ = "ALL" then rp_m_e$ = "ALL"     /*EWD001*/
            if rp_m_e$ <> "ALL" then goto L54770        /*   |  */
               rp_m_ed$ = "*****  All Models"           /*   |  */
               return                                   /*EWD001*/
L54770:     if rp_m_e$ <> " " then goto L54810
L54780:        rp_m_e$ = "000"
               rp_m_ed$ = "(000) = All Models Catalog"  /*EWD001*/
         rem   goto L54890
               return
L54810:     if rp_m_e$ = "000" then goto L54780
            tab_1% = 0% : tab_2% = 1%
            tab_val$ = str(rp_m_e$,1%,3%)
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% then goto L54890
               rp_m_ed$ = tab_desc$
/* (CR2451) begin */               
            if rp_vm_e$ = " " then return
              rp_m_e$ = rp_m_e$ & rp_vm_e$
              init(" ") readkey$, descr$
              readkey$ = "VIRTUALMD" & rp_m_e$ & " "
              call "DESCRIBE" (#2, readkey$, str(descr$,,19%) ,0%, f1%(2)) 
                if descr$ = " " then goto L54890
/* (CR2451) end   */                  
        return
L54890:     errormsg$ = "Invalid Product Line Model Code?"
            rp_m_e$, rp_m_ed$ = " "
        return

        REM *************************************************************~
            *       D a t a   I n p u t   S c r e e n   E d i t s       *~
            *-----------------------------------------------------------*~
            * Edits for Change Screen                                   *~
            *************************************************************

        deffn'154(fieldnr%)                 /* (EWD001) New - Start */
            errormsg$ = " "
            on fieldnr% gosub   L54900,         /* New Data Value    */   ~
                                L54960,         /* Price Catalog Code*/   ~
                                L54970,         /* Model Code        */   ~
                                L54980          /* Price Status Code */   
            return

        REM Value (1) thru (6) Edit

L54900:    cg_vl = 0.0
           convert cg_vl$ to cg_vl, data goto L54950
           convert pc_vfmt$ to zz%, data goto L54950

           on zz% goto L54920, L54925, L54930, L54935

L54920:       convert cg_vl to cg_vl$, pic(##,###.##-)
           goto L54940
L54925:       convert cg_vl to cg_vl$, pic(####.####-)
           goto L54940
L54930:       convert cg_vl to cg_vl$, pic(#########-)
           goto L54940
L54935:       convert cg_vl to cg_vl$, pic($#,###.##-)

L54940: return

L54950:    errormsg$ = "(Error) - Invalid Value for " & vd$(vl%)
           cg_vl$ = " " : cg_vl = 0.0
        return


L54960: REM Price Catalog                         CG_C$
            if cg_c$ <> "ALL" then goto L54964
               cg_c_d$ = "*****  All Price Catalogs"
               return
L54964:     tab_1% = 10% : tab_2% = 0%
            tab_val$ = cg_c$
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% then goto L54966
               cg_c_d$ = tab_desc$
        return
L54966:     errormsg$ = "Invalid Price Catalog Code?"
            cg_c$, cg_c_d$ = " "
        return

L54970: REM Model Code                            CG_M$
            if cg_m$ <> "ALL" then goto L54974
               cg_m_d$ = "*****  All Models"
               return
L54972:     cg_m_d$ = "(000) = All Models Catalog"
            return
L54974:     if cg_m$ = "000" then goto L54972
            tab_1% = 0% : tab_2% = 1%
            tab_val$ = cg_m$
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% then goto L54978
               cg_m_d$ = tab_desc$
/* (CR2451) begin */               
            if cg_vm$ = " " then return
              cg_m$ = cg_m$ & cg_vm$
              init(" ") readkey$, descr$
              readkey$ = "VIRTUALMD" & cg_m$ & " "
              call "DESCRIBE" (#2, readkey$, str(descr$,,19%) ,0%, f1%(2)) 
                if descr$ = " " then goto L54978
/* (CR2451) end   */                  
        return
L54978:     errormsg$ = "Invalid Product Line Model Code?"
            cg_m$, cg_m_d$ = " "
        return

L54980: REM Pricing Status Code                CG_ST$, CG_ST_D$
            if cg_st$ <> " " then goto L54982
               cg_st$ = "B"
L54982:     if cg_st$ <> "A" and cg_st$ <> "I" and cg_st$ <> "B" then    ~
                                               goto L54986
            if cg_st$ = "A" then                                         ~
                            cg_st_d$ = "(A)-Active Price Definitions  "
            if cg_st$ = "I" then                                         ~
                            cg_st_d$ = "(I)-Inactive Price Definitions"
            if cg_st$ = "B" then                                         ~
                            cg_st_d$ = "(B)-Both Active/Inactive      "
        return
L54986:     errormsg$ = "(Error) - Invalid Selection? A, I, or B "
            cg_st$, cg_st_d$ = " "
        return
                                            /* (EWD001) New - End */
            
        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************
                                                 /* Header (1)         */
L55040: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+
L55060: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!
L55080: %! ######## @ ########                        ###################~
        ~#####################                                Page: #### !
L55100: %!Cat. Code:(####)  ####################                         ~
        ~                        Cat. Method: (##)  #################### !

L55130: %!Model    :(################)   ##############################     ~
        ~                      Status    : ( #)  #################### !
                                                      /* Column Header */
L55160: %!Rf Calc<---- Desc. ----->!<Record>!<---- Key Desc ---->!<------~
        ~ Key Value ------>!<---- Value Description ----->!<- Value->!Spc!
                                                      /* Column Format */
L55190: %!## ### ##################!########!####################!#######~
        ~##################!##############################!##########!###!

L55220: %!-------------------------!--------!--------------------!-------~
        ~------------------!------------------------------!----------!---!

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        print_header
          if lcnt% <> 99% then print using L55040
          page_no% = page_no% + 1%
          rpt_time$ = " "
          call "TIME" (rpt_time$)
          print page
          print using L55040
          print using L55080, date$, rpt_time$, print_title$, page_no%
          print using L55100, pc_c$, pc_c_d$, pc_cm$, str(pc_cm_d$,1%,20%)
          print using L55130, pc_m$, pc_m_d$, pc_st$, pc_st_d$
          print using L55060
          print using L55160
          lcnt% = 6%
          sav_st$ = pc_st$ : sav_c$ = pc_c$
          sav_cm$ = pc_cm$ : sav_mm$ = pc_m$
          sav_r$, sav_rc$ = " "
        return

        print_detail
          if ecs_flag% = 1% then goto update_flat_file
          if pc_st$ <> sav_st$ or pc_c$ <> sav_c$ or pc_cm$ <> sav_cm$   ~
                              or pc_m$ <> sav_mm$ then gosub print_header
          if lcnt% > 56% then gosub print_header
          print using L55220
          lcnt% = lcnt% + 1%
          for i% = 1% to 6%
            sav_kd$, sav_ik$ = " "
            if i% > 3% then goto L60420                         /* SKIP */
               if pc_kdesc$(i%) = "000" then                             ~
                  kd$(i%), ik$(i%) = " (Not Applicable) "
               if i% = 1% then goto L60370
                  if pc_kdesc$(i%) = "000" then kd$(i%), ik$(i%) = " "

L60370:        if pc_klen%(i%) <> 0% then                                ~
                                   sav_ik$ = str(ik$(i%),1%,pc_klen%(i%))
               if len(ik_d$(i%)) > 1 then                                ~
                  str(sav_ik$,6%) = ik_d$(i%)
               sav_kd$ = kd$(i%)
L60420:     if i% > 1% then goto L60540
               if pc_r$ = sav_r$ and pc_rc$ = sav_rc$ then goto L60510
                  sav_r$  = pc_r$                   /* Start of Record */
                  sav_rc$ = pc_rc$
                  rp_desc$ = str(pc_rc_d$,1%,18%)
               print using L55190, pc_r$, pc_rc$, rp_desc$, pc_ref$,      ~
                        sav_kd$, sav_ik$ , vd$(i%), pc_vl$(i%), pc_spc$
               goto L60590
                                                    /* Same Ref type   */
L60510:        print using L55190, " "  , " "   , " "     , pc_ref$,      ~
                        sav_kd$, sav_ik$ , vd$(i%), pc_vl$(i%), pc_spc$
               goto L60590
L60540:     if pc_vl(i%) = 0 then vd$(i%), pc_vl$(i%) = " "
            if len(sav_kd$) < 10 and len(vd$(i%)) < 10 and pc_vl(i%) = 0 ~
                                                          then goto L60600
              print using L55190, " ", " ", " ", " ", sav_kd$, sav_ik$,   ~
                                 vd$(i%), pc_vl$(i%), " "
L60590:     lcnt% = lcnt% + 1%
L60600:   next i%
        return

        select_printer
            if ecs_flag% = 1% then goto open_flat_file
            page_no% = 0%
            lcnt%    = 99%
            print_title$ = "*** A P C   Standard/Special Pricing ***"
            date$ = date  :  call "DATEFMT" (date$)
            rpt_time$ = " "
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(5))
            call "SETPRNT" ("APCPRC", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            if ecs_flag% = 1% then return
            print using L55040
            call "SETPRNT" ("APCPRC", " ", 0%, 1%)
        return

        generate_report
        for r% = 1% to 2%
          f% = 3%
          if r% = 2% then f% = 6%
            init(" ") cc$, cm$, mm$, sav_st$, sav_c$, sav_cm$, sav_mm$,  ~
                      sav_r$, sav_rc$, st$, sav_mod$
            if ecs_flag% = 0% then                                       ~
          call "SHOSTAT" ("Printing Report for Catalog ( "&rp_c_b$&" )") ~
             else                                                        ~
          call "SHOSTAT" ("Creating (ECS) Pricing Flat File")

            gosub select_printer
            rpt% = 1%
            p_key$ = " "
            if rp_st$ = "B" then goto L60940 else goto L60950
L60940:        str(p_key$,1%,1%) = "A" : goto L60970
L60950:        str(p_key$,1%,1%) = rp_st$

L60970:     if rp_c_b$ = "ALL " then goto L60980 else goto L60990
L60980:        str(p_key$,2%,4%) = "0000" : goto L61010
L60990:        str(p_key$,2%,4%) = rp_c_b$

L61010:     if rp_cm_b$ = "A " then goto L61020 else goto L61030
L61020:        str(p_key$,6%,2%) = "00"   : goto L61050
L61030:        str(p_key$,6%,2%) = rp_cm_b$

L61050:     if rp_m_b$ = "ALL" then goto L61060 else goto L61070
L61060:        str(p_key$,8%,16%) = "000" : goto generate_next
L61070:        str(p_key$,8%,16%) = rp_m_b$
               if st$ = "I" then str(p_key$,8%,16%) = rp_m_e$

        generate_next
          read #f%,key > p_key$, using L61130, p_key$,                    ~
                                                   eod goto generate_done
L61130:         FMT XX(8), CH(53)
                                            /* TEST KEY FOR SELECTIONS */
             st$ = str(p_key$,1%,1%)
             cc$ = str(p_key$,2%,4%)
             cm$ = str(p_key$,6%,2%)
             mm$ = str(p_key$,8%,16%)

          if rp_st$ = "A" and st$ <> "A" then goto generate_done
          if rp_st$ = "I" and st$ <> "I" then goto generate_done

          if rp_c_b$ = "ALL " then goto L61280
             if cc$ < rp_c_b$ then goto generate_next   /* OUT OF WIND */
             if cc$ <= rp_c_e$ then goto L61280          /* IN WINDOW   */
                goto L61380

L61280:   if rp_cm_b$ = "A " then goto L61330
             if cm$ < rp_cm_b$ then goto generate_next  /* OUT OF WIND */
             if cm$ <= rp_cm_e$ then goto L61330         /* IN WINDOW   */
                goto L61380

L61330:   if ecs_flag% = 1% then goto L61420
             if rp_m_b$ = "ALL" then goto L61450
                if mm$ < rp_m_b$ then goto generate_next
                if mm$ <= rp_m_e$ then goto L61450

L61380:            if st$ = "I" then goto generate_done
                      p_key$ = " "
                      st$, str(p_key$,1%,1%) = "I"
                      goto L60970
L61420:   gosub check_for_modle
          if chk_flat% = 0% then goto generate_next

L61450:   gosub dataload
          gosub print_detail
             goto generate_next
        generate_done
        next r%
            gosub close_printer
        return


        check_for_modle
            if sav_mod$ = mm$ then return
               chk_flat% = 0%
               sav_mod$  = mm$
               init(" ") readkey$
               str(readkey$,1%,9%)   = "PRICE 017"
               str(readkey$,10%,15%) = mm$
               read #2,key = readkey$, eod goto L61620
                  chk_flat% = 1%
L61620: return


        
        print_xref  /* Builds & displays or prints xrefs - New (EWD001) */
            opt% = 1%  :  chg% = 0%
            keyhit% = keyhit% - 10%
            goto L61650
        change_xref     /* Allow change of value after display. */
            if authr% = 0% then return   /* CR2267 */
            opt% = 0%  :  chg% = 1%
            keyhit% = keyhit% - 20%
            goto L61650
        show_xref
            opt%, chg% = 0%
L61650:     pass%, vl% = 0%
            on keyhit% - 8% gosub model_xref, ref_xref, val_xref
            if pass% = 0% then goto L61700
                call "EWDPLA63" (0%, #2, tab_xref$, opt%, " ")
            if chg% = 0% then goto L61690
                gosub inputmode_change      /* Get the change & */
                gosub change_xref_value     /* now go do it...  */
L61690:     call "DELETE" (#2, tab_xref$, 9%)
L61700:     return

        model_xref
            call "SHOSTAT" ("Building Model to Catalog Xref")
            tab_xref$ = "ZPRICEXF1"
            write #2, using L61820, tab_xref$, "...."," Model = "& pc_m$,~
                " ", eod goto model_xref_done
            readkey$ = all(hex(00))
            str(readkey$,,9%) = "PRICE 000"
            build_key$ = "A" & "xxxx" & pc_cm$ & pc_m$
            pass% = 1%
          next_mdl_ctlg
            read #2, key > readkey$, using L61750, readkey$, descr$,     ~
                eod goto model_xref_done
L61750:             fmt ch(24), ch(30)
            str(build_key$,2%,4%) = str(readkey$,10%,4%)
            read #3, key > build_key$, using L61780, temp_key$,          ~
                eod goto next_mdl_ctlg
L61780:             fmt pos(9), ch(40)
            if str(temp_key$,,10%) <> str(build_key$,,10%)               ~  
                then next_mdl_ctlg
            write #2, using L61820, tab_xref$, str(temp_key$,2%,4%), descr$, ~
                " ", data goto next_mdl_ctlg, eod goto next_mdl_ctlg
L61820:             fmt ch(9), ch(15), ch(30), ch(74)
            goto next_mdl_ctlg

          model_xref_done
            return
            

        ref_xref
            call "SHOSTAT" ("Building Reference to Ctlg/Model Xref")
            tab_xref$ = "ZPRICEXF2"
            write #2, using L61920, tab_xref$, "....", pc_r$ &" "& pc_rc$ ~
                &" "& pc_k$, " ", eod goto ref_xref_done
          val_xref_start
            build_key$ = all(hex(00))
            pass% = 1%
          next_price_rec
            descr$ = " "
            read #3, key > build_key$, using L61880, str(descr$,21%,8%),  ~
                build_key$, xrf_vl(), eod goto ref_xref_done
L61880:             fmt ch(8), ch(40), 6*PD(14,4)
            pass% = pass% + 1%
            if mod(pass%,100%) <> 0 then goto L61900
                convert pass% to temp$, pic(###########0)
                print at (13,30), "Records Scanned:  " & temp$
L61900:     if str(build_key$,11%,30%) <> pc_r$ & pc_rc$ & pc_k$          ~
                then next_price_rec
            if vl% = 0% then goto L61910
                if xrf_vl(vl%) <> pc_vl(vl%) then next_price_rec
L61910:     readkey$ = "PRICE 000" & str(build_key$,2%,4%) & " "
            call "DESCRIBE" (#2, readkey$, str(descr$,,19%) ,0%, f1%(2))
            str(descr$,30%,1%) = str(build_key$,1%)
            temp_key$ = str(build_key$,2%,4%) &" "& str(build_key$,8%,3%) ~
                & "       "
            if str(build_key$,,1%) = "I" then str(temp_key$,5%,1%) = "_"
            write #2, using L61920, tab_xref$, temp_key$, descr$, " ",    ~
                data goto next_price_rec, eod goto next_price_rec
L61920:             fmt ch(9), ch(15), ch(30), ch(74)
            goto next_price_rec

          ref_xref_done
            return                          
            

        val_xref
            vl% = 2%
            call "ASKUSER" (vl%, "*** Choose Value for Xref ***",        ~
                "Press <PF1> to <PF6> for the Value to Cross-Reference", ~
                "- OR -","Press <ENTER> to Abort")
            if vl% > 6% or pc_vl$(vl%) = " " then val_xref
            if vl% = 0% then val_xref_done
            call "SHOSTAT" ("Building Value to Ctlg/Model Xref")
            tab_xref$ = "ZPRICEXF3"
            descr$ = pc_r$ &" "& pc_rc$ &" "& pc_k$ &" "& str(vd$(vl%),22%,8%)
            call "SPCESMSH" (descr$,1%)             /* 2 passes required */
            descr$ = descr$ & ": " & pc_vl$(vl%)    /* to see all of data*/
            call "SPCESMSH" (descr$,1%)
            read #2, hold, key = tab_xref$ & "....", eod goto val_xref_notfound
                 delete #2
          val_xref_notfound
            put #2, using L61920, tab_xref$, "....", descr$, " ",        ~
                eod goto val_xref_done
            write #2
            descr_save$ = descr$
            gosub val_xref_start
          val_xref_done
            if vl% = 0% then chg% = 0%
            return                     
            

        change_xref_value
            call "SHOSTAT" ("Updating Xrefs With New Value")
            chg% = 0%
            readkey$ = all(hex(ff))
            str(readkey$,,13%) = str(tab_xref$) & "...."
          next_xref_change
            read #2, key > readkey$, using L61950, readkey$, r_rec$,     ~
                    eod goto change_xref_done
L61950:         fmt ch(24), ch(30)
            if cg_c$ <> "ALL" and str(readkey$,10%,4%) <> cg_c$          ~
                then next_xref_change
            if cg_m$ <> "ALL" and str(readkey$,15%,3%) <> cg_m$          ~
                then next_xref_change
            if cg_st$ <> "B" and str(r_rec$,30%,1%) <> cg_st$            ~
                then next_xref_change
            temp_key$ = str(r_rec$,21%,8%)
/* CR2289 */            
            read #3, hold, key 1 = temp_key$, using L61960, xrf_vl(),    ~
                  pc_user$, pc_date$, pc_time$,                          ~
                    eod goto next_xref_change
L61960:         fmt pos(49), 6*PD(14,4), POS(113), CH(03), CH(06), CH(06)
            xrf_vl(vl%) = cg_vl
/* CR2289 */
            pc_user$ = userid$
            pc_date$ = date
            pc_time$ = time
            
            put #3, using L61960, xrf_vl(), pc_user$, pc_date$, pc_time$                         
            rewrite #3, data goto next_xref_change
            chg% = chg% + 1%
        
            goto next_xref_change

          change_xref_done
            convert chg% to temp$, pic(######0)
            call "ASKUSER" (2%, "*** Change Results ***","No. of Records"~
                & " Updated = "& temp$,"---","Press <ENTER> to Continue")
            return


        rec_range_delete
        for d% = 1% to 2%
         f% = 3%
         if d% = 2% then f% = 6%
            call "SHOSTAT" ("Counting Records for Deletion...")
            del% = 0%
            temp_key$ = all(hex(00))
            str(temp_key$,,8%) = str(del_fr_seq$)
            read #3, key 1 = temp_key$, using L61990, temp_key$,         ~
                    eod goto del_verify_done
L61990:         fmt ch(8)
            del% = del% + 1%
          next_del_verify
            read #f%, key 1 > temp_key$, using L61990, temp_key$,        ~
                    eod goto del_verify_done
            if str(temp_key$,,8%) > del_to_seq$ then goto del_verify_done
            del% = del% + 1%
            goto next_del_verify
          del_verify_done
            convert del% to temp$, pic(######0)
            descr$ = temp$ &" Record(s) Will Be DELETED"
            if del% = 0% then descr$ = "-- No Records Found In Range --"
            k% = 2%
            call "ASKUSER" (k%,"*** Confirm Delete ***","Please Confirm:",~
                descr$,"Press <PF32> to Continue, <ENTER> to Abort")
            if k% = 0% or del% = 0% then return
            if k% <> 32% then goto del_verify_done

            call "SHOSTAT" ("Deleting Range of Records...")
            del% = 0%
            temp_key$ = all(hex(00))
            str(temp_key$,,8%) = str(del_fr_seq$)
            read #f%, hold, key 1 = temp_key$, using L61990, temp_key$, ~
                    eod goto range_del_done
            delete #f%
       
            del% = del% + 1%
          next_range_del
            read #f%, hold, key 1 > temp_key$, using L61990, temp_key$, ~
                    eod goto range_del_done
            if str(temp_key$,,8%) > del_to_seq$ then goto range_del_done
            delete #f%
                             
            del% = del% + 1%
            goto next_range_del
          range_del_done
          next d%
            call "STRTRLSE" addr(#3)
            call "STRTRLSE" addr(#6)
            convert del% to temp$, pic(######0)
            call "ASKUSER" (2%, "*** Delete Results ***","No. of Records"~
                & " Deleted = "& temp$, "---","Press <ENTER> to Continue")
            return

                                        /* End - New (EWD001) */
            

        REM *************************************************************~
            *           F O R M A T   S C R E E N   B U F F E R         *~
            *************************************************************

        format_buffer
                                                    /* (EWD004) - Mods */
                                                    /*  for Wood Jamb  */
                                                    /*  tables         */
         pc_vtln% = 0%
         call "APCPR3SB" (pc_r$,        /* Std/Spc Ref Code  -PRICE 002*/~
                          pc_rc$,       /* Ref. Calc Code    -PRICE 003*/~
                          pc_kdesc$(),  /* Field Description Code      */~
                          kd$(),        /* Key Text Display (3) - 20   */~
                          pc_kfld$(),   /* Key Field Def (3) - 3   - 3 */~
                          pc_kfld_d$(), /* Key Field Def Descript  -10 */~
                          pc_kfld%(),   /* Key Field Def (3)       - 1 */~
                          pc_kbeg%(),   /* Key Beginning Point     - 1 */~
                          pc_klen%(),   /* Key Length for Field (3)- 1 */~
                          pc_vtbl$,     /* Value Field Code        - 2 */~
                          pc_vtbl_d$,   /* Valuse Field Code Desc  -10 */~
                          pc_vtln%,     /* Value Table Code Length -   */~
                          pc_vdesc$,    /* Value Field Desc Code   - 3 */~
                          pc_vdesc_d$,  /* Value Field Descript -   20 */~
                          pc_vtblv$(),  /* Value Field Table Values- 3 */~
                          vd$(),        /* Value Descriptions      -30 */~
                          pc_vfmt$,     /* Value Format Tab Code   - 3 */~
                          pc_vcalc$,    /* Value Calc Code         - 2 */~
                          pc_vcalc_d$,  /* Value Calc Desc         -12 */~
                          pc_kcalc$,    /* Key Calc Code           - 2 */~
                          pc_kcalc_d$,  /* Key Calc Desc           -14 */~
                          pc_kunt$,     /* Key Unit Convert Code   - 2 */~
                          pc_kunt_d$,   /* Key Unit Convert Desc   - 6 */~
                          #1,           /* (APCPCMSK) - File           */~
                          #2,           /* (GENCODES) - File           */~
                          err% )        /* Error Non Zero Error        */
           if err% <> 0% then return
                                                    /* FORMAT KEY DATA */
           ik_d$(1%), ik_d$(2%), ik_d$(3%) = "                    "
           k_no%, v_no% = 0%
           for i% = 1% to 3%
               if pc_kfld%(i%) <> 0% then goto L62050
                  kd$(i%), ik$(i%) = " "
                  pc_klen%(i%) = 0%
                  goto L62060
L62050:        k_no% = k_no% + 1%
L62060:    next i%

           for i% = 1% to 6%
               rhh% = len(vd$(i%))
               if rhh% > 15% then goto L62120
                  vd$(i%), pc_vl$(i%) = " "
                  goto L62130
L62120:        v_no% = v_no% + 1%
L62130:    next i%

        return

        copy_data
        for c% = 1% to 2%
         f% = 3%
         if c% = 2% then f% = 6%
           call "SHOSTAT" ("Copy Data From Catalog ( "&rp_c_b$&" )")
           copy% = 1%
           from_key$ = " "
           str(from_key$, 1%,1%) = "A"
           if rp_c_b$ <> "ALL" then str(from_key$, 2%,4%) = rp_c_b$    /*<AWD007> */
           str(from_key$, 6%,2%) = rp_cm_b$
           if rp_m_b$ <> "ALL" then str(from_key$, 8%,16%) = rp_m_b$    /*EWD001*/
           str(from_key$,24%,2%) = rp_rc$
/*<AWD007> Price Ref Calc Method */
           if rp_rcm$ <> "ALL" then str(from_key$,26%,3%) = rp_rcm$
           from_sav$             = str(from_key$,1%,28%) /*<AWD007> */
           copy_read = 0
           copy_count = 0
        copy_next
           copy_read = copy_read + 1
           if MOD(copy_read,10000) <> 0 then goto read_rec
           convert copy_read to copy_read$, pic (########)
           convert copy_count to copy_count$, pic (#######)
           call "SHOSTAT" ("Processing ..."&copy_read$&hex(0D)&          ~
                                        " Records Copied..."&copy_count$)
read_rec:
           read #f%,key > from_key$, using L62280, from_key$,             ~
                                                       eod goto copy_done
L62280:        FMT POS(9), CH(53)
/*<AWD007>*/
/*<AWD007>*/if rp_c_b$ <> "ALL" and rp_cm_b$ <> "  " and rp_m_b$ <> "ALL" and    ~
               str(from_sav$,8%,16%) <> str(from_key$,8%,16%) then copy_done
/*<AWD007>*/if rp_c_b$ <> "ALL" and rp_cm_b$ <> "  " and rp_m_b$ <> "ALL" and    ~
               rp_rc$ <> " " and str(from_sav$,24%,2%) <> str(from_key$,24%,2%)  ~
               then copy_done
/*<AWD007>*/if rp_c_b$ <> "ALL" and rp_cm_b$ <> "  " and rp_m_b$ <> "ALL" and    ~
               rp_rc$ <> " " and rp_rcm$ <> "ALL" and                            ~
               str(from_sav$,26%,3%) <> str(from_key$,26%,3%)then copy_done
               
            if rp_rcm$ <> "ALL" and str(from_sav$,26%,3%) <> str(from_key$,26%,3%) ~
               then copy_next
/*<AWD007>  IF RP_RC$ <> " " AND FROM_SAV$ <> STR(FROM_KEY$,1%,12%) THEN COPY_DONE*/

            if rp_rc$ <> " " and str(from_sav$,24%,2%) <> str(from_key$,24%,2%) ~
               then copy_next
               
/*EWD001    IF RP_M_B$ <> "ALL" AND STR(FROM_SAV$,1%,10%) <> STR(FROM_KEY$,1%,10%)*/
/*EWD001       THEN COPY_DONE                                                     */
/*EWD001    IF STR(FROM_SAV$,1%,7%) <> STR(FROM_KEY$,1%,7%) THEN COPY_DONE        */

            if rp_m_b$ <> "ALL" and str(from_sav$,8%,16%) <> str(from_key$,8%,16%) ~
               then copy_next
            if rp_cm_b$ <> "  " and str(from_sav$,6%,2%) <> str(from_key$,6%,2%) ~
               then copy_next
            if rp_c_b$ <> "ALL" and str(from_sav$,2%,4%) <> str(from_key$,2%,4%) ~
               then copy_done
               
            if str(from_sav$,1%,1%) <> str(from_key$,1%,1%) then goto copy_done
            copy_count = copy_count + 1
/*<AWD007>*/
               gosub dataload
               assign% = 1%
               pc_ref$ = " "
/*<AWD007>     pc_st$  = "I"  */
               pc_st$  = "A"                                 /*<AWD007>*/
               if rp_c_e$ <> "ALL" then pc_c$   = rp_c_e$                         
               if rp_cm_e$ <> "  " then pc_cm$  = rp_cm_e$
/*<AWD007> */
               if rp_m_e$ <> "ALL" then pc_m$ = rp_m_e$     /*EWD001*/
               gosub dataput
            goto copy_next
        copy_done
        next c%
        return clear all
        goto inputmode
        
        copy_from_to
        call "SHOSTAT" ("Copy Data From To Data ( "&rp_c_b$&" )")
        for c% = 1% to 2%
         f% = 3%
         if c% = 2% then f% = 6%           
           copy_from% = 1%
           if rp_c_b$  = "ALL" then return
           if rp_cm_b$ = "ALL" then return
           if rp_m_b$  = "ALL" then return
           if rp_rc$   = "ALL" then return
           if rp_rcm$  = "ALL" then return
           
           init(" ") readkey$
           str(readkey$,1%,9%)   = "PRICE 000"
           from_key$ = " "
           str(from_key$, 1%,1%) = "A"
           str(from_key$, 2%,4%) = rp_c_b$ 
           str(from_key$, 6%,2%) = rp_cm_b$
           str(from_key$, 8%,16%)= rp_m_b$ 
           str(from_key$,24%,2%) = rp_rc$
           str(from_key$,26%,3%) = rp_rcm$
           from_sav$             = str(from_key$,1%,28%) /*<AWD007> */
           copy_read = 0
           copy_count = 0        
           
           read #f%,key = from_key$, eod goto copy_to_done
           
           
             gosub dataload
             p_key$ = from_key$
             
             if rp_c_e$ = "ALL" then goto lookupCatalogs
             str(p_key$,2%,4%) = rp_c_e$
             str(p_key$,6%,2%) = rp_cm_e$
             str(p_key$,8%,2%) = rp_m_e$             
             gosub copy_to_put
        copy_to_done
        next c%
        return clear all
        goto inputmode
        
        lookupCatalogs
           copy_read = copy_read + 1
           if MOD(copy_read,100) <> 0 then goto noCopytoShow
              call "SHOSTAT"( "Catalog --> " & pc_c$) 
              
noCopytoShow:
           gosub readCatalog        
           if catalog% = 0% then goto copy_to_done
           str(p_key$,2%,4%) = pc_c$
           str(p_key$,6%,2%) = rp_cm_e$
           str(p_key$,8%,2%) = rp_m_e$
           gosub copy_to_put
           goto lookupCatalogs   
        
        
        readCatalog
          catalog% = 0%
          read #2,key > readkey$, using L61750, readkey$, descr$,   ~
                               eod goto CatDone
          if str(readkey$,1%,9%) <> "PRICE 000" then goto catDone                    
          pc_c$ = str(readkey$,10%,4%)
          catalog% = 1%
        catDone
        return

          
           
        REM *************************************************************~
            *           P U R G E  D A T A                              *~
            *              (AWD008)                                     *~
            *************************************************************

        purge_data
           vp_desc$ = "Counting "
           if purge_flag% <> 1% then goto pv_cont
           vp_desc$ = "Purging "
        pv_cont
           call "SHOSTAT" (vp_desc$&" Data From Catalog ( "&rp_c_b$&" )")
        for d% = 1% to 2%
         f% = 3%
         if d% = 2% then f% = 6%           
           copy% = 1%
           from_key$ = " "
           str(from_key$, 1%,1%) = "A"
           if rp_c_b$ <> "ALL" then str(from_key$, 2%,4%) = rp_c_b$ /*<Catalog */
           str(from_key$, 6%,2%) = rp_cm_b$  /*Catalog Method "00" */
           if rp_m_b$ <> "ALL" then str(from_key$, 8%,16%) = rp_m_b$ /*Model */
           str(from_key$,24%,2%) = rp_rc$  /*Ref Code*/
/*<AWD007> Price Ref Calc Method */
           if rp_rcm$ <> "ALL" then str(from_key$,26%,3%) = rp_rcm$
           from_sav$             = str(from_key$,1%,28%) /*<AWD007> */
           optnr% = 0%
           if rp_c_b$ <> "ALL" then optnr% = 1%
           if rp_c_b$ <> "ALL" and  rp_m_b$ <> "ALL" then optnr% = 2%
           if rp_c_b$ <> "ALL" and  rp_m_b$ <> "ALL" and rp_rc$ <> " "         ~
                         then optnr% = 3%
REM i'm assuming rp_c_b$ <> "ALL"
REM RP_RC = " " means ALL
/*                         " Purge Catalog Code  :" rp_c_b$*/
/*                         "       Cat. Method   :" rp_cm_b$*/
/*                         "       Model Code    :" rp_m_b$*/
/*                         "       Ref Code      :" rp_rc$*/
/*                         "       Ref Calc Code :" rp_rcm$*/
           if rp_rc$ <> " " and rp_m_b$ <> "ALL" then optnr% = 3%
           if rp_rc$ <> " " and rp_m_b$ = "ALL" then optnr% = 3%

           if rp_rc$ = " " and rp_m_b$ <> "ALL" then optnr% = 2%
           if rp_c_b$ <> "ALL" and rp_m_b$ = "ALL" and rp_rc$ = " " ~
                          then optnr% = 1%
/*IM8052*/ if d% <> 1% then purge_next
           copy_read = 0
           copy_count = 0
        purge_next
           copy_read = copy_read + 1
           if MOD(copy_read,10000) <> 0 then goto read_recp
           convert copy_read to copy_read$, pic (########)
           convert copy_count to copy_count$, pic (#######)
           call "SHOSTAT" ("Processing ..."&copy_read$&hex(0D)&" Records ..."& ~
                            vp_desc$&copy_count$)
read_recp:
           read #f%,key > from_key$, using P62280, from_key$,              ~
                                                       eod goto purge_done
P62280:        FMT POS(9), CH(53)
/*IM8052*/  if rp_c_b$ <> "ALL" and str(from_sav$,2%,4%) <> str(from_key$,2%,4%) ~
               then purge_done
/*<AWD007>*/
/*<AWD007>*/if rp_c_b$ <> "ALL" and rp_cm_b$ <> "  " and rp_m_b$ <> "ALL" and    ~
               str(from_sav$,8%,16%) <> str(from_key$,8%,16%) then purge_done
/*<AWD007>*/if rp_c_b$ <> "ALL" and rp_cm_b$ <> "  " and rp_m_b$ <> "ALL" and    ~
               rp_rc$ <> " " and str(from_sav$,24%,2%) <> str(from_key$,24%,2%)  ~
               then purge_done
/*<AWD007>*/if rp_c_b$ <> "ALL" and rp_cm_b$ <> "  " and rp_m_b$ <> "ALL" and    ~
               rp_rc$ <> " " and rp_rcm$ <> "ALL" and                            ~
               str(from_sav$,26%,3%) <> str(from_key$,26%,3%)then purge_done
               
            if rp_rcm$ <> "ALL" and str(from_sav$,26%,3%) <> str(from_key$,26%,3%) ~
               then purge_next
/*<AWD007>  IF RP_RC$ <> " " AND FROM_SAV$ <> STR(FROM_KEY$,1%,12%) THEN PURGE_DONE*/

            if rp_rc$ <> " " and str(from_sav$,24%,2%) <> str(from_key$,24%,2%) ~
               then purge_next
               
/*EWD001    IF RP_M_B$ <> "ALL" AND STR(FROM_SAV$,1%,10%) <> STR(FROM_KEY$,1%,10%)*/
/*EWD001       THEN PURGE_DONE                                                     */
/*EWD001    IF STR(FROM_SAV$,1%,7%) <> STR(FROM_KEY$,1%,7%) THEN PURGE_DONE        */

            if rp_m_b$ <> "ALL" and str(from_sav$,8%,16%) <> str(from_key$,8%,16%) ~
               then purge_next
            if rp_cm_b$ <> "  " and str(from_sav$,6%,2%) <> str(from_key$,6%,2%) ~
               then purge_next
/*IM8052    if rp_c_b$ <> "ALL" and str(from_sav$,2%,4%) <> str(from_key$,2%,4%) ~
               then purge_done   */
               
/*          IF STR(FROM_SAV$,1%,1%) <> STR(FROM_KEY$,1%,1%) THEN GOTO PURGE_DONE */

            copy_count = copy_count + 1
/*<AWD007>*/
/*                         " Purge Catalog Code  :" rp_c_b$*/
/*                         "       Cat. Method   :" rp_cm_b$*/
/*                         "       Model Code    :" rp_m_b$*/
/*                         "       Ref Code      :" rp_rc$*/
/*                         "       Ref Calc Code :" rp_rcm$*/

            if purge_flag% <> 1 then goto purge_next
            on optnr% goto option_1, option_2, option_3
               stop
               
   option_1:   read #f%,hold,key = from_key$, using P62280, from_key$, eod    ~
                                  goto purge_next
               delete #f%

               apcpcmsd_key$ = str(from_key$,2%,22%)
               read #4,hold,key = apcpcmsd_key$, eod goto purge_next
               delete #4
               goto purge_next
                  
   option_2:   read #f%,hold,key = from_key$, using P62280, from_key$, eod    ~
                                  goto purge_next
               delete #f%

               apcpcmsd_key$ = str(from_key$,2%,22%)
               read #4,hold,key = apcpcmsd_key$, eod goto purge_next
               delete #4
               goto purge_next
                  
   option_3:   read #f%,hold,key = from_key$, using P62280, from_key$, eod    ~
                                  goto purge_next
               delete #f%
                        
               apcpcmsd_key$ = str(from_key$,2%,22%)
/* CR2289 add user id, date, time to get and put */               
               read #4,hold,key = apcpcmsd_key$, eod goto purge_next
                  get #4, using P35040, pc_c$, pc_cm$, pc_m$, pc_std%(),     ~
                                       pc_stdc$(), pc_stdr$(), pc_spc%(),    ~
                                       pc_spcc$(), pc_spcr$(), pc_sub%(),    ~
                                       pc_subc$(), pc_subr$(), pc_userid$,   ~
                                       pc_date$, pc_time$, pc_dfil$

               pc_spc%(7%) = 0%
               pc_spc%(8%) = 0%
/* CR2289 */               
               pc_user$ = userid$
               pc_date$ = date               
               pc_time$ = time
               
               put #4, using P35040,    pc_c$, pc_cm$, pc_m$, pc_std%(),     ~
                                       pc_stdc$(), pc_stdr$(), pc_spc%(),    ~
                                       pc_spcc$(), pc_spcr$(), pc_sub%(),    ~
                                       pc_subc$(), pc_subr$(), pc_userid$,   ~
                                       pc_date$, pc_time$, pc_dfil$

               rewrite #4, eod goto purge_next
            goto purge_next
   purge_done
        next d%   /*IM8052*/
        if purge_flag% = 1% then goto purge_exit
        convert copy_count to temp$, pic(######0)
        descr$ = temp$ &" Record(s) Will Be DELETED"
        if copy_count = 0 then descr$ = "-- No Records Found In Range --"
        k% = 2%
        call "ASKUSER" (k%,"*** Confirm Delete ***","Please Confirm:",~
            descr$,"Press <PF32> to Continue, <ENTER> to Abort")
        if k% <> 32% then goto purge_exit
        purge_flag% = 1
        goto purge_data
purge_exit
/*      next d%     IM8052  */
        return clear all
        goto inputmode

        activate_data
        
           call "SHOSTAT" ("Activating Data For Catalog ("& rp_c_b$ &    ~
                            rp_cm_b$ & ")" )
        for a% = 1% to 2%
         f% = 3%
         if a% = 2% then f% = 6%                            
           from_key$ = " "
           str(from_key$,1%,1%) = "I"
           str(from_key$,2%,4%) = rp_c_b$
           str(from_key$,6%,2%) = rp_cm_b$
/*EWD001*/ if rp_m_b$ <> "ALL" then str(from_key$,8%,3%) = rp_m_b$
           from_sav$ = str(from_key$,1%,10%)
        activate_next
           read #f%,hold,key > from_key$, using L62550, from_key$,         ~
                                                   eod goto activate_done
L62550:        FMT POS(9), CH(40)
/*EWD001*/ if rp_m_b$ <> "ALL" and from_sav$ <> str(from_key$,1%,10%)     ~
/*EWD001*/     then goto activate_done
/*EWD001*/ if str(from_sav$,,7%) <> str(from_key$,,7%) then activate_done
               get #f%, using L62580, pc_rec$
L62580:           FMT CH(128)              /* CR3237 */
               delete #f%
               str(pc_rec$,9%,1%) = "A"
/* CR2289 CR3237 */
               str(pc_rec$,113%,03%) = userid$
               str(pc_rec$,116%,06%) = date
               str(pc_rec$,122%,06%) = time
               
               check_key$ = str(pc_rec$,9%,40%)
                                  /* Insure Only One (1) Active Record */
                                  /* for Specified Key                 */
           read #f%,hold,key = check_key$, eod goto L62660
               delete #f%
L62660:     put #f%, using L62580, pc_rec$
            write #f%
                           
            goto activate_next
        activate_done
        next a%
        return clear all
        goto inputmode

        update_flat_file
            init(" ") ecs_k$()
            gosub lookup_series
            for i% = 1% to 3%
              if pc_klen%(i%) = 0% then goto L62800

                 ecs_k$(i%) = str(ik$(i%),1%,pc_klen%(i%))
L62800:     next i%
            for i% = 1% to 6%
                if i% = 1% then goto L62840        /* Always Have (1)   */
                   if len(vd$(i%)) < 10 and pc_vl(i%) = 0 then goto L62940
L62840:               gosub format_ecs
                      cnt% = cnt% + 1%
                      if mod(cnt%,25%) <> 0 then goto L62910
                         convert cnt% to str(cnt$,19%,8%), pic(########)

                         print at(02,27);hex(84);cnt$;

L62910:               put #5, using L62920, ecs_rec$
L62920:                  FMT CH(128)
                      write #5, eod goto L62960
L62940:     next i%
        return
L62960:     call "SHOSTAT" ("(Error)-Updating (ECS) Flat File?")
            stop
        return

        format_ecs
            if ecs_flag% = 0% then  return
            init(" ") ecs_rec$
            str(ecs_rec$,1%,4%) = pc_c$      /*( 1)Price Catalog       */
            str(ecs_rec$,5%,2%) = pc_cm$     /*( 2)Pricing Calc Method */
            str(ecs_rec$,7%,16%)= pc_m$     /*( 3)Pricing Model Code  */
            str(ecs_rec$,23%,2%)= pc_r$      /*( 4)Prc Ref Code Std/Spc*/
            str(ecs_rec$,25%,3%)= pc_rc$     /*( 5)Prime Ref Calc Methd*/
            str(ecs_rec$,28%,2%)= pc_kcalc$ /*( 6)Key Calc Method     */
            str(ecs_rec$,30%,2%)= pc_kunt$  /*( 7)Key Unit Conv. Methd*/
            if len(pc_kunt$) < 2 then str(ecs_rec$,30%,2%) = "00"

            str(ecs_rec$,32%,3%) = pc_spc$   /*( 8)Spec Calc Prod      */
            str(ecs_rec$,35%,25%)= ecs_k$(1%)/*( 9) Key (1) Value     */
            str(ecs_rec$,60%, 4%)= ecs_k$(2%)/*(10) Key (2) Value     */
            str(ecs_rec$,64%, 4%)= ecs_k$(3%)/*(11) Key (3) Value     */
            str(ecs_rec$,68%, 2%)= pc_vtbl$    /*(12) Value Field No. */
            if len(pc_vtbl$) < 2 then str(ecs_rec$,68%,2%) = "00"
            str(ecs_rec$,70%, 3%)= pc_vtblv$(i%) /*(13) Value Tab Fld */
            str(ecs_rec$,73%,10%)= pc_vl$(i%)  /*(14) Value Price     */
            str(ecs_rec$,83%, 2%)= pc_vcalc$   /* Val Fld Calc Method */
            str(ecs_rec$,85%, 8%)= pc_series$  /* New Ellison Ser Code*/
            str(ecs_rec$,88%,40%)= " "         /* (40) Free Area      */
        return

        lookup_series
            init(" ") readkey$, s_verify$, pc_series$
            str(ecs_rec$,10%,8%) = "        "
            str(s_verify$,1%,3%) = pc_m$
            str(s_verify$,4%,2%) = "02"          /* WIRED FOR NORANDEX */
            str(readkey$,1%,9%)   = "ELLISON05"
            str(readkey$,10%,15%) = s_verify$
            read #2,key = readkey$, using L63340, pc_series$,             ~
                                                           eod goto L63350
L63340:        FMT POS(25), CH(8)
L63350: return

        open_flat_file
            open nodisplay #5, output, space = 100%,                     ~
                 dpack = 100%, ipack = 100%, file = file$,               ~
                 library = library$, volume = volume$, blocks = 5%
        return

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end

