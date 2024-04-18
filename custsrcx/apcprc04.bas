        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPRC04                             *~
            *  Creation Date     - 09/01/02                             *~
            *  Last Modified Date-                                      *~
            *  Description       - This Utility Copies Data based on    *~
            *                      specific Catalog, Catalog Method,    *~ 
            *                      Model, Grid Codes.                   *~
            *                                                           *~
            *                                                           *~
            *  Special Subs - (COPY_DATA) Copy's (A)ctive Pricing Data  *~
            *                 for (Catalog, Catalog Method, Model, Grid)*~
            *                 to (New) In-Active (Catalog, Catalog      *~
            *                 Method, Model, Grid) Insuring Only One    *~
            *                 (1) In-Active Record For Key.             *~
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
            *                                                           *~
            *          Note - For Primary Data Key There can be both an *~
            *                 (A)ctive and (I)nactive Record for the    *~
            *                 Same Pricing Data. But 'Only' (A)ctive    *~
            *                 Data is used to Calculate the Price of    *~
            *                 the Product.                              *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 09/01/02 ! New Program for (APC) - Last Mod Date    ! CMG *~
            *************************************************************

        dim                              /* (APCPCMST) Price Calc Def. */~
            pc_st$1, pc_st_d$30,         /* Price Record Status        */~
            pc_ref$8,                    /* Price Record Number        */~
            pc_c$4, pc_c_d$30,           /* Price Catalog Code/Desc    */~
            pc_cm$2, pc_cm_d$30,         /* Price Catalog Method Code  */~
            pc_m$3, pc_m_d$30,           /* APC Model Code             */~
            pc_r$2, pc_r_d$32,           /* Std/Spc Reference Code     */~
            pc_rc$3, pc_rc_d$32,         /* Price Ref. Calc Method     */~
            pc_kdesc$(3%)3,              /* Field Description Codes    */~
            pc_kfld$(3%)3,pc_kfld_d$(3%)10, /* Field Description Codes */~
            pc_kfld%(3%),                 /* Field Definition Codes    */~
            pc_kbeg%(3%),                 /* Start Position in Part No.*/~
            pc_klen%(3%),                 /* Field Length in Part No.  */~
            pc_vtbl$2, pc_vtbl_d$10,     /* Field Definition Code-Table*/~
            pc_vtblv$(6%)3, vd$(6%)30,   /* Field Table Values         */~
            pc_vdesc$3, pc_vdesc_d$20,   /* Value Description Codes    */~
            pc_vfmt$2, pc_vfmt_d$10,     /* Value Field Format Code    */~
            pc_vcalc$2,pc_vcalc_d$12,    /* Value Calculation Code     */~
            pc_kcalc$2,pc_kcalc_d$14,    /* Key   Calculation Code     */~
            pc_kunt$2, pc_kunt_d$6,      /* Key Unit Conversion Code   */~
            pc_key$5,                    /* Primary Key (APCPCMSK)     */~
            p_key$40,                    /* Primary Key (APCPCMST)     */~
            pc_k$25,                     /* Data Value Key - APCPCMST  */~
            pc_fil$3,                    /* Definition Filler Area     */~
            kd$(3%)20,                   /* Key Input Descriptions     */~
            ik$(3%)25,                   /* Key Input Data             */~
            ik_d$(3%)20,                 /* Key Input Data Descript    */~
            pc_vl$(6%)10, pc_vl(6%),     /* Data Values                */~
            pc_spc$3, pc_spc_d$30,       /* Special Calc Code-PRICE 010*/~
            tab_val$5, tab_desc$32,      /* Table Key and Description  */~
            tab_hdr$30, sav_key$9,       /* Calc Sequence Codes        */~
            descr$32,                    /* GENERIC DESCRIPT FOR FIELDS*/~
            readkey$50, r_rec$30,        /* GENCODES PRIMARY KEY       */~
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

        dim rp_st$1,                     /* Report (A), (I), (B)=Both  */~
            rp_c_b$4, rp_c_e$4,          /* Report Beg/End Catalog Code*/~
            rp_cm_b$2,rp_cm_e$2,         /* Report Beg/End Cat Method  */~
            rp_m_b$3, rp_m_e$3,          /* Report Beg/End Model Code  */~
            rp_c_bd$30, rp_c_ed$30,      /* Report Beg/End Catalog Code*/~
            rp_cm_bd$30,rp_cm_ed$30,     /* Report Beg/End Cat Method  */~
            rp_m_bd$30, rp_m_ed$30,      /* Report Beg/End Model Code  */~
            rp_rc$2, rp_rc_d$30,         /* Copy   Beg/End Ref Code Des*/~ 
            from_key$40, from_sav$12,    /* Copy - Catalog to Product  */~
            pc_rec$102,                  /* (APCPCMST) - Record        */~
            tab_xref$9                   /* GENCODES Table, Xref Values*/
            

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "Pricing Data Entry for Catalog Def's    "
            pname$ = "APCPRC04 - Rev: R6.04"

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
                        varc,     indexed,  recsize = 102,               ~
                        keypos =    9, keylen =   40,                    ~
                        alt key  1, keypos =    1, keylen =  8

            select #4,   "APCPCMSD",                                     ~
                        varc,     indexed,  recsize = 768,               ~
                        keypos =    1, keylen =    9


            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1%), f2%(1%),1000%, rslt$(1%))
            call "OPENCHCK" (#2,  fs%(2%), f2%(2%),   0%, rslt$(2%))
            call "OPENCHCK" (#3,  fs%(3%), f2%(3%),1000%, rslt$(3%))
            call "OPENCHCK" (#4,  fs%(4%), f2%(3%),1000%, rslt$(4%))

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


        REM *************************************************************~
            *         I N P U T   M O D E   C O P Y   S C R E E N       *~
            *************************************************************

        inputmode_copy
            gosub initialize_variables

            for fieldnr% = 1% to  9%
L14080:         gosub'071(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L14200
L14100:         gosub'103(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L14180
L14130:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'071(fieldnr%)
                         if enabled% = 1% then L14100
                         if fieldnr% = 1% then L14080
                         goto L14130
L14180:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L14100
L14200:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L14100
            next fieldnr%

        REM *************************************************************~
            *          E D I T   M O D E   C O P Y   S C R E E N        *~
            *************************************************************

        editpg3
            lastfieldnr% = 0%
            gosub'103(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 14% then gosub copy_data
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg3
L15110:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 9% then editpg3
            if fieldnr% = lastfieldnr% then    editpg3
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
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'071(fieldnr%)
            enabled% = 1%
            return

        REM *************************************************************~
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

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
         "Enter a Activate/Copy (From) Catalog Code?                   ",~
         "Enter a Activate/Copy (From) Catalog Method Code?            ",~
         "Enter a Activate/Copy (From) Model Code?                     ",~
         "Enter a Activate/Copy (From) Ref Code.                       ",~
         "Enter a Activate/Copy (From) Grid Code.                      ",~
         "Enter a Copy To Catalog Code?                                ",~
         "Enter a Copy To Catalog Method Code?                         ",~
         "Enter a Copy To Model Code?                                  ",~
         "Enter a Copy To Grid Code?                                   "


        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, pc_ref$, pc_st$, pc_st_d$, ~
                      pc_c$, pc_c_d$, pc_cm$, pc_cm_d$, pc_m$, pc_m_d$,  ~
                      pc_r$, pc_r_d$, pc_rc$, pc_rc_d$,                  ~
                      pc_kfld$(), pc_kfld_d$(), pc_vtbl$, pc_vtbl_d$,    ~
                      pc_vdesc$, pc_vdesc_d$, pc_vtblv$(),               ~
                      pc_vfmt$, pc_vfmt_d$, pc_vcalc$, pc_vcalc_d$,      ~
                      pc_kcalc$, pc_kcalc_d$, pc_kunt$, pc_kunt_d$,      ~
                      pc_kdesc$(), pc_k$, p_key$, pc_key$, pc_fil$,      ~
                      pc_spc$, pc_spc_d$, rp_st$, rp_c_b$, rp_c_e$,      ~
                      rp_cm_b$, rp_cm_e$, rp_m_b$, rp_m_e$, rp_c_bd$,    ~
                      rp_c_ed$, rp_cm_bd$, rp_cm_ed$, rp_m_bd$,rp_m_ed$, ~
                      from_key$, from_sav$, pc_rec$, rp_rc$, rp_rc_d$,   ~
                      rp_gr_b$, rp_gr_bd$, rp_gr_e$, rp_gr_ed$

            init(" ") kd$(), ik$(), ik_d$(), vd$(), pc_vl$()

            mat pc_kfld% = zer
            mat pc_kbeg% = zer
            mat pc_klen% = zer
            mat pc_vl    = zer
            lookup%, copy%, pass% = 0%   

        return


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
            if pass% <> 0% then call "DELETE" (#2, tab_xref$, 9%) 
            goto inputmode_copy


        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

            dataload
              lookup% = 1%
              get #3, using L35040, pc_ref$, pc_st$, pc_c$, pc_cm$,       ~
                                   pc_m$, pc_r$, pc_rc$, pc_k$,          ~
                                   pc_vl(), pc_spc$, pc_fil$
              if copy% <> 0% then return        /* Skip for Copy       */

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
            tab_val$ = pc_m$
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% then goto L51030
               pc_m_d$ = tab_desc$
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
            gosub format_buffer                     
            if err% <> 0% then goto L51660
            if k_no% = 0% and edit% = 1% then fieldnr% = fieldnr% + 3%
            if k_no% = 0% and edit% = 1% then gosub L52120    
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

L51750:    if k_no% = 0% then L52120            /* NO KEY DATA DEFINED */

           if k_no% >= x% then  goto L51820
              fieldnr% = 11%
              if v_no% = 0% then fieldnr% = 17%
              return

L51820:    if pc_klen%(x%) > 5% then goto L52110
              xx% = 0%
              rr$ = str(ik$(x%),1%,pc_klen%(x%))
              rr  = pos(rr$ = "-")
              if rr <> 0 then rr$ = str(rr$,1%,rr-1)
              convert rr$ to xx%, data goto L52017

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
              read #3,key = p_key$, eod goto L52230
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

        lookup_key                        /* Lookup Key for (APCPCMST) */
            p_key$ = all(hex(00))
            str(p_key$,1%,1%)   = pc_st$     /* Status Code            */
            str(p_key$,2%,4%)   = pc_c$      /* Catlog Code Number     */
            str(p_key$,6%,2%)   = pc_cm$     /* Catalog Method Code    */
            str(p_key$,8%,3%)   = pc_m$      /* Product Model Code     */
            str(p_key$,11%,2%)  = pc_r$      /* Pricing Reference Code */
            str(p_key$,13%,3%)  = pc_rc$     /* Ref Code Calc Method   */
            gosub data_key
            str(p_key$,16%,25%) = pc_k$      /* Generic Key Calc Def.  */
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
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        dataput
              if assign% <> 0% then goto L31130
                 read #3,hold,key 1% = pc_ref$, eod goto L31390
                 gosub data_key
                 goto L31160

L31130:       gosub lookup_key
              read #3,hold,key = p_key$, eod goto L31230
                                                  /* Set-Up for Update */
L31160:          get #3, using L31170, pc_ref$
L31170:             FMT CH(08)
                 delete #3
                 if keyhit% <> 12% then goto L31250
                    return clear all
                    goto inputmode_copy
                                         /* Do Not Assign a New Number */
L31230:       if assign% <> 0% then gosub assign_record

L31250:          put #3, using L35040, pc_ref$, pc_st$, pc_c$, pc_cm$,    ~
                                      pc_m$, pc_r$, pc_rc$, pc_k$,       ~
                                      pc_vl(), pc_spc$, pc_fil$

              write #3
              if copy% <> 0% then return
        return clear all
              if assign% = 0% then goto inputmode_copy
              if k_no% = 0% then goto inputmode_copy
                 init(" ") ik$(), pc_vl$(), ik_d$()
                 mat pc_vl = zer
REM              f_no% = 9%
                 goto inputmode_copy

L31390: return clear all
             stop "(Error) - Unable to Update Record --->  " & pc_ref$
             goto inputmode_copy

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
        goto inputmode_copy

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L35040: FMT                     /* (APCPCMST) Master Definition File   */~
            CH(08),             /* Pricing Logical Sequence Number     */~
            CH(01),             /* Price Record Status Code            */~
            CH(04),             /* Pricing Catalog Code - (PRICE 000)  */~
            CH(02),             /* Pricing Catalog Meth - (PRICE 001)  */~
            CH(03),             /* Pricing Model Code   - (MODEL    )  */~
            CH(02),             /* Std/Spec Ref Code    - (PRICE 002)  */~
            CH(03),             /* Ref. Calc Method Code- (PRICE 003)  */~
            CH(25),             /* Generic Key (Max Five (3) Fields)   */~
            6*PD(14,4),         /* Values Associated With Calc         */~
            CH(03),             /* Spcecial Calc. Code    (PRICE 010)  */~
            CH(3)               /* Filler Area                         */


        REM *************************************************************~
            *            L O A D   D I S P L A Y   S C R E E N S        *~
            *************************************************************

        lookup_tab_1                   /* Load Data for Display Screen */
            on tab_1% gosub p_1, p_2, p_3, p_4, p_5, p_6, p_7
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
        p_7                                /* Lookup Grid/Liting Code  */
            tab_hdr$ = " Grid/Liting Code Table       "
            sav_key$ = "LITING   "

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
                              L45200,         /* Copy From Grid Code   */ ~
                              L45200,         /* Copy To Catalog Code  */ ~
                              L45200,         /* Copy To Cat. Method   */ ~
                              L45200,         /* Copy To Model Code    */ ~
                              L45200          /* Copy To Grid Code     */ 
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
               at (03,40), fac(hex(84)),   rp_c_bd$             , ch(30),~
                                                                         ~
               at (04,02), "       Cat. Method   :",                     ~
               at (04,25), fac(lfac$( 2)), rp_cm_b$             , ch(02),~
               at (04,40), fac(hex(84)),   rp_cm_bd$            , ch(30),~
                                                                         ~
               at (05,02), "       Model Code    :",                     ~
               at (05,25), fac(lfac$( 3)), rp_m_b$              , ch(03),~
               at (05,40), fac(hex(84)),   rp_m_bd$             , ch(30),~
                                                                         ~
               at (06,02), "       Ref Code      :",                     ~
               at (06,25), fac(lfac$( 4)), rp_rc$               , ch(02),~
               at (06,40), fac(hex(84)),   rp_rc_d$             , ch(30),~
                                                                         ~
               at (07,02), "       Grid Code     :",                     ~
               at (07,25), fac(lfac$( 5)), rp_gr_b$             , ch(02),~
               at (07,40), fac(hex(84)),   rp_gr_bd$            , ch(30),~
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
               at (10,40), fac(hex(84)),   rp_m_ed$             , ch(30),~
                                                                         ~
               at (11,02), "       Grid Code     :",                     ~
               at (11,25), fac(lfac$( 9)), rp_gr_e$             , ch(02),~
               at (11,40), fac(hex(84)),   rp_gr_ed$            , ch(30),~
                                                                         ~
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

L45860:        if keyhit% <> 7 then goto L45870
                  tab_1% = 5%
                  gosub lookup_tab_1
                  goto L45070

L45870:        if keyhit% <> 8 then goto L45910
                  tab_1% = 7%
                  gosub lookup_tab_1
                  goto L45070

L45910:        if keyhit% <> 13 then goto L45940
REM                  gosub activate_data

L45940:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf4
        if edit% = 2% then L46150     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "(7)Valid Models                        "
            pf$(2) = "(2)Catalogs                             " &        ~
                     "(8)Valid Grids         (15)Print Screen"
            pf$(3) = "(3)Catalog Methods                      " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01020304ffff0708ffffffffff0e0f1000)
            if fieldnr% = 1% then L46090
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L46090:     if fieldnr% > 1% then L46110
                str(pf$(1),18,18) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L46110:     if fieldnr% = 4% then goto L46130              
                str(pf$(1),64)    = " "  :  str(pfkeys$,13,1) = hex(ff)
L46130:     return

L46150: if fieldnr% > 0% then L46240  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                       (14)Copy Data   "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
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
                                L55100,         /* Copy From Grid Code */ ~
                                L54480,         /* Copy To Catalog     */ ~
                                L54620,         /* Copy To Cat Method  */ ~
                                L54760,         /* Copy To Model Code  */ ~
                                L55150          /* Copy To Grid Code   */

            return

L54030: REM Price Catalog (From)                  RP_C_B$
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
            if rp_m_b$ <> "ALL" then goto L54320                  
               rp_m_bd$ = "*****  All Models"                     
               return                                             
L54320:     if rp_m_b$ <> " " then goto L54360
L54330:        rp_m_b$ = "000"
               rp_m_bd$ = "(000) = All Models Catalog"            
               return
L54360:     if rp_m_b$ = "000" then goto L54330
            tab_1% = 0% : tab_2% = 1%
            tab_val$ = rp_m_b$
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% then goto L54440
               rp_m_bd$ = tab_desc$
        return
L54440:     errormsg$ = "Invalid Product Line Model Code?"
            rp_m_b$, rp_m_bd$ = " "
        return

L54450: REM Ref Code (From)                       RP_RC$
            if rp_m_b$ = "ALL" then rp_rc$ = " "                   
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

L55100: REM Grid Code (From)                      RP_GR_B$, RP_GR_BD$
            if rp_gr_b$ <> " " then goto L55110
               rp_gr_b$ = "AL"
               return
L55110:     tab_1% =  0% : tab_2% = 4%
            tab_val$ = rp_gr_b$
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% then goto L55120
               rp_gr_bd$ = tab_desc$
        return
L55120:     errormsg$ = "Invalid Grid/Liting Code?"
            rp_gr_b$, rp_gr_bd$ = " "
        return

L54480: REM Price Catalog  (To)                   RP_C_E$
            if rp_c_e$ <> " " then goto L54510
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
            if rp_m_b$ = "ALL" then rp_m_e$ = "ALL"               
            if rp_m_e$ <> "ALL" then goto L54770                  
               rp_m_ed$ = "*****  All Models"                     
               return                                             
L54770:     if rp_m_e$ <> " " then goto L54810
L54780:        rp_m_e$ = "000"
               rp_m_ed$ = "(000) = All Models Catalog"            
         rem   goto L54890
               return
L54810:     if rp_m_e$ = "000" then goto L54780
            tab_1% = 0% : tab_2% = 1%
            tab_val$ = rp_m_e$
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% then goto L54890
               rp_m_ed$ = tab_desc$
        return
L54890:     errormsg$ = "Invalid Product Line Model Code?"
            rp_m_e$, rp_m_ed$ = " "
        return

L55150: REM Grid Code (From)                      RP_GR_E$, RP_GR_ED$
            if rp_gr_e$ <> " " then goto L55160
               rp_gr_e$ = "AL"
               return
L55160:     tab_1% =  0% : tab_2% = 4%
            tab_val$ = rp_gr_e$
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                           #2, tab_rec% )
            if tab_rec% = 0% then goto L55170
               rp_gr_ed$ = tab_desc$
        return
L55170:     errormsg$ = "Invalid Grid/Liting Code?"
            rp_gr_e$, rp_gr_ed$ = " "
        return

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

        REM *************************************************************~
            *      C O P Y   D A T A  T O   P R I C E   R E C   N O.    *~
            *************************************************************

        copy_data
           call "SHOSTAT" ("Copy Data From Catalog ( "&rp_c_b$&" )")
           copy% = 1%
           from_key$ = " "
           str(from_key$, 1%,1%) = "A"
           str(from_key$, 2%,4%) = rp_c_b$
           str(from_key$, 6%,2%) = rp_cm_b$
           if rp_m_b$ <> "ALL" then str(from_key$, 8%,3%) = rp_m_b$               
           str(from_key$,11%,2%) = rp_rc$
           from_sav$             = str(from_key$,1%,12%)
        copy_next
           read #3,key > from_key$, using L62280, from_key$,              ~
                                                       eod goto copy_done
L62280:        FMT POS(9), CH(40)
            if rp_rc$ <> " " and from_sav$ <> str(from_key$,1%,12%) then copy_done
            if rp_m_b$ <> "ALL" and str(from_sav$,1%,10%) <> str(from_key$,1%,10%)  ~
               then copy_done
            if str(from_sav$,1%,7%) <> str(from_key$,1%,7%) then copy_done
            if rp_gr_b$ <> "AL" and str(from_key$,23%,2%) <> rp_gr_b$  ~
                                then goto copy_next
               gosub dataload
               assign% = 1%
               pc_ref$ = " "
               pc_st$  = "A"
               pc_c$   = rp_c_e$
               pc_cm$  = rp_cm_e$

               if rp_m_b$ <> "AL" then str(pc_k$,8%,2%) = rp_gr_e$

               if rp_m_e$ <> "ALL" then pc_m$ = rp_m_e$                 
               gosub dataput
            goto copy_next
        copy_done
        return clear all
        goto inputmode_copy

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end



