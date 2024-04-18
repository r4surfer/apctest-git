        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPRC03                             *~
            *  Creation Date     - 12/17/93                             *~
            *  Last Modified Date- 10/31/05                             *~
            *  Description       - This Program Defines Calc Sequence   *~
            *                      for a Catalog associated with a      *~
            *                      Product                              *~
            *  Code Tables Used  - (PRICE 000) - Master Price Catalogs  *~
            *                      (PRICE 001) - Pricing Cat. Methods   *~
            *                      (Model    ) - Model/Product Code     *~
            *                      (PRICE 002) - Reference Type Codes   *~
            *                      (PRICE 011) - Cross-Reference Codes  *~
            *                      (PRICE 017) - Models to create Flat  *~
            *                                    file for.              *~
            *                                                           *~
            *  Special Comments  - Uses Routines LOOKUP_TAB_1, and      *~
            *                      LOOKUP_TAB_2. The Table Lookup       *~
            *                      Subroutine 'APCPR1SB' is Used.       *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/04/94 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 04/28/97 ! Mod to Create a Flat file for ECS        ! RHH *~
            * 05/27/97 ! Mod to drive Flat file creation by using ! RHH *~
            *          !   the table file (PRICE 017)             !     *~
            * 10/31/97 ! Check for Upgrade to R6.04.03            ! RHH *~
            * 09/30/98 ! Mod to use EWDPLA63 to view tables.EWD001! BWS *~
            * 11/23/98 ! Mod to allow copy of All Models.   EWD002! BWS *~
            * 02/17/99 ! Add 'Global Update' function.      EWD003! BWS *~
            * 11/01/99 ! (EWD004) No Mods for Wood Jamb           ! RHH *~
            * 11/03/00 ! (EWD005) Mod to allow Alpha in Bay Bow   ! CMG *~
            *          !          models.                         !     *~             
            * 10/26/05 ! (AWD006) CR347 mods for new sub part     ! CMG *~
            * 07/31/06 ! (AWD008) Mods for copy_all function      ! CMG *~
            *05/10/2007! (AWD009) Mods for alpha part #           ! DES *~
            *10/04/2019! (CR2267) add authorized user check       ! RDB *~
            *10/15/2019! (CR2289) add user ID, date, time on chang! RDB *~ 
            *03/04/2020! (CR2451) 3900 PSE Price                  ! CMN *~             
            *************************************************************

        dim                              /* (APCPCMST) Price Calc Def. */~
            pc_c$4, pc_c_d$30,           /* Pricing Catalog Code       */~
            pc_cm$2,pc_cm_d$30,          /* Pricing Calc Method Code   */~
/*CR2451*/  pc_m$16, pc_m_d$30,          /* Pricing Model Code         */~
/*CR2451*/  pc_vm$13,                    /* Virtual Model              */~
            pc_cc$4, pc_c_dc$30,         /* Pricing Catalog Code       */~
/*AWD007*/  pc_cc_beg$4,                 /* Pricing Cat Code Copy Beg  */~
/*AWD007*/  pc_cc_end$4,                 /* Pricing Cat Code Copy End  */~
            pc_cmc$2,pc_cm_dc$30,        /* Pricing Calc Method Code   */~
/*(CR2451)*/ pc_mc$16, pc_m_dc$30,        /* Pricing Model Code         */~
            pc_std%(20%),                /* Standard Pricing Sequence  */~
            pc_stdc$(20%)1,              /* Cross-Ref Codes            */~
            pc_stdr$(20%)9,              /* Initial Ref Calc Code 3,3,3*/~
            pc_spc%(20%),                /* Special Pricing Sequence   */~
            pc_spcc$(20%)1,              /* Cross-Ref Codes            */~
            pc_spcr$(20%)9,              /* Initial Ref Calc Code 3,3,3*/~
            pc_sub%(20%),                /* Sub Part Seq    (AWD006)   */~
            pc_subc$(20%)1,              /* Sub Part Cross-Ref (AWD006)*/~
            pc_subr$(20%)9,              /* Sub Part Ref Calc  (AWD006)*/~
            pc_dfil$71, pc_key$22,       /* Filer Area    CR2289       */~
/* CR2289 */                                                             ~
            pc_user$3, pc_date$6,        /* User ID and System Date    */~
            pc_time$6,                   /* System time                */~
                                                                         ~            
            hdr1$30, hdr2$4, hdr3$3,     /* Screen Headers             */~
            hdr4$20, hdr5$3, hdr6$3,     /* Screen Headers             */~
            hdr7$3,                      /* Screen Headers             */~
            scr$(30%)30, scr_d$(30%)2,   /* Std/Spc Descriptions       */~
            scrc$(30%)1, scrc_d$(30%)20, /* Cross-Ref Codes/Descript   */~
            scrc_1$(30%)3, scrc_2$(30%)3,/* Applicable Ref Codes       */~
            scrc_3$(30%)3,               /* Applicable Ref Codes       */~
            scr_hdr$12,                  /* Screen Header Title        */~
            tab_hdr$30,                  /* Screen Header For Tables   */~
            tab_val$5, tab_desc$32,      /* Table Key and Description  */~
            sav_key$22,                  /* Used for Loading Screen    */~
            std$20, pc_std$(20%)2,       /* '*' = ACTIVE STANDARD REF  */~
            spc$20, pc_spc$(20%)2,       /* '*' = ACTIVE SPECIAL REF   */~
            sub$20, pc_sub$(20%)2,       /* '*' = ACTIVE SPECIAL REF   */~            
/*AWD006*/  chk_pri$60,                  /* CHECK PRIORITY CODE        */~
            code$(60%)4,                 /* Calc Sequence Codes        */~
            desc$(60%)32, descr$32,      /* GENERIC DESCRIPT FOR FIELDS*/~
            readkey$50,                  /* GENCODES PRIMARY KEY       */~
            company$60,                  /* For Report Company Name    */~
            print_title$46,              /* For Report Title           */~
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
            userid$3,                    /* Current User Id            */~
            mdllfac$1,                   /* Virtual Mdl Screen (CR2451)*/~
            mdllfac1$1,                  /* Virtual Mdl Screen (CR2451)*/~            
            usr_desc$32                  /* User Security (CR2451)     */
            

                                         /* Report Variables           */
        dim beg_c$4, beg_c_d$30,         /* Beginning Catalog Code     */~
            end_c$4, end_c_d$30,         /* Ending Catalog Code        */~
            sav_c$4, sav_cm$2,           /* Save Catalog an Method     */~
/*(CR2451)*/ beg_mod$16, beg_mod_d$30,    /* Beginning Model Code/Desc  */~
/*(CR2451)*/ end_mod$16, end_mod_d$30,    /* Ending Model Code/Desc     */~
            file$8,                      /* Flat File Name             */~
            library$8,                   /* Library Name = 'APCDATA'   */~
            volume$6,                    /* Volume Name = 'CARLOS'     */~
            cc$4, mm$3, sav_mod$3,       /* Save Catalog Code/Model    */~
/*EWD002*/  copy_all$5                   /* Text Version of Counter    */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            f1%(5%),                     /* = 1 if READ was successful */~
            fs%(5%),                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */

        dim ecs_rec$32,cnt$25,ecs_key$13,/* (ECSCONTR) - Record        */~
            pc_pri$2,                    /* Price Priority Seq No.     */~
            pc_rf$2,                     /* Std/Spc Ref Code -PRICE 002*/~
            pc_cr$1,                     /* Cross-Ref        -PRICE 011*/~
            pc_rf1$3,                    /* Calc Ref (1)     -PRICE 003*/~
            pc_rf2$3,                    /* Calc Ref (2)     -PRICE 003*/~
            pc_rf3$3,                    /* Calc Ref (3)     -Price 003*/~
            pc_fil$11                    /* Filler area                */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "Pricing Catalog Calc/Priority Definition"
            pname$ = "APCPRC03 - Rev: R6.04"

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
            * #1  ! APCPCMSD ! Master Calc. Definition File             *~
            * #2  ! GENCODES ! Master System Tables File                *~
            * #3  ! ECSWORK  ! Temporary Work File to Sort Data         *~
            * #4  ! ECSCONTR ! ECS Control Flat File Priority Seq.      *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,   "APCPCMSD",                                     ~
                        varc,     indexed,  recsize =  768,              ~
                        keypos =    1, keylen =    22          /*(AWD006)*/

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #3,  "ECSWORK",                                       ~
                        varc,     indexed,  recsize =   48,              ~
                        keypos =    1, keylen =  13


            select #4, "ECSCONTR", consec, recsize = 32
                        
            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1,  fs%(1%), f2%(1%),1000%, rslt$(1%))
            call "OPENCHCK" (#2,  fs%(2%), f2%(2%),   0%, rslt$(2%))
            
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

            file$    = "ECSCONTR"        /* Flat File Name             */
            library$ = "APCDATA "        /* Library Name = 'APCDATA'   */
            volume$  = "CARLOS"          /* Volume Name = 'CARLOS'     */

            ecs_flat% = 0%
            k% = 0% : authr% = 0%
            gosub authorized_user  /* CR2267 */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to  4%
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
                      if keyhit% = 14% then goto inputmode_report
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
                  if keyhit%  =  8% then gosub global_update    /*EWD003*/
                  if keyhit%  =  9% then goto inputmode_copy
                  if keyhit%  = 12% then gosub delete_it
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11130:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% > 3% then fieldnr% = 4%
            if fieldnr% < 1% or fieldnr% > 4% then editpg1
            if fieldnr% = lastfieldnr% then    editpg1
        REM EDITPG1_CONT
            gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg1
L11260:     gosub'101(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L11260
            gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L11260
                  lastfieldnr% = fieldnr%
            goto L11130

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
L13100:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 4% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'061(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L13150:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L13150
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L13150
                  lastfieldnr% = fieldnr%
            goto L13100

        REM *************************************************************~
            *         I N P U T   M O D E   C O P Y   S C R E E N       *~
            *************************************************************

        inputmode_copy
            for fieldnr% = 1% to  3%
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
L14180:               if keyhit% = 16% and fieldnr% = 1% then inputmode
                      if keyhit% <> 0% then       L14100
L14200:         gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L14100
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   R E P O R T   S C R E E N      *~
            *************************************************************

        editpg3
            lastfieldnr% = 0%
            gosub'103(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  =  7% then gosub copy_data
                  if keyhit%  =  9% then gosub copy_data
                  if keyhit%  = 16% then gosub copy_data
                  if keyhit% <>  0% then       editpg3
L15110:     fieldnr% = cursor%(1%) - 3%                     /*EWD002*/
            if fieldnr% < 1% or fieldnr% > 3% then editpg3
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
/*EWD002*/  if fieldnr% = 3% and pc_m$ = "ALL" then enabled% = 0%
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
         "Enter a Valid Catalog Code?                        (Required)",~
         "Enter a Valid Catalog Calculation Method Code?     (Required)",~
         "Enter a Valid Model/Product Code?                  (Required)",~
         "Enter Valid Priority Sequence for Std/Spec Reference Types?  "


        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28280
                inpmessage$ = edtmessage$
                return

L28280
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn2_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn2_msg  :  data                                               ~
         "Enter a Valid Beginning Catalog Code?                        ",~
         "Enter a Valid Ending Catalog Code?                           ",~
         "Enter a Valid Beginning Model Code or (A)ll?                 ",~
         "Enter a Valid Ending Model Code?                             "

        deffn'070(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28440
                inpmessage$ = edtmessage$
                return

L28440
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn3_msg, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn3_msg  :  data                                               ~
         "Enter a Valid Catalog Code to Copy To?             (Required)",~
         "Enter a Valid Catalog Calc. Method Code to Copy To?(Required)",~
         "Enter a Valid Model/Product Code to Copy To?       (Required)"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, pc_c$, pc_cm$, pc_m$,      ~
                      pc_c_d$, pc_cm_d$, pc_m_d$, scr$(), scr_d$(),      ~
                      pc_key$, pc_dfil$, std$, spc$, chk_pri$,           ~
                      scr_hdr$, pc_std$(), pc_spc$(), beg_c$, end_c$,    ~
                      beg_c_d$, end_c_d$, sav_c$, sav_cm$, cc$,          ~
                      pc_stdc$(), pc_stdr$(), pc_spcc$(), pc_spcr$(),    ~
                      scrc$(), scrc_d$(), scrc_1$(), scrc_2$(),          ~
                      scrc_3$(), pc_cc$, pc_c_dc$, pc_cmc$, pc_cm_dc$,   ~
                      pc_mc$, pc_m_dc$, beg_mod$, beg_mod_d$, end_mod$,  ~
                      end_mod_d$, sub$, pc_sub$(), pc_user$, pc_date$,   ~
                      pc_time$, pc_vm$, pc_vmc$, beg_vmod$, end_vmod$

/* (AWD006) */
           init(" ") pc_subc$(), pc_subr$(), std$, pc_sub$()

/* (AWD007) */
           init(" ") pc_cc_beg$, pc_cc_end$


           mat pc_sub% = zer


           std$ = "*******             "  /* ACTIVE STANDARD REF TYPES */
                                          /* TYPE REF'S 01 THRU 20     */
           spc$ = "******              "  /* ACTIVE SPECIAL  REF TYPES */
                                          /* TYPE REF'S 21 THRU 40     */
           sub$ = "******              "  /* ACTIVE SPECIAL  REF TYPES */
                                          /* TYPE REF'S 21 THRU 40     */
           std_spc% = 1%
           rpt% = 0%
           mat pc_std% = zer
           mat pc_spc% = zer
           for i% = 1% to 20%
               if str(std$,i%,1%) = "*" then pc_std%(i%) = i%

               if str(spc$,i%,1%) = "*" then pc_spc%(i%) = i% + 20%
/*(AWD006)*/
               if str(sub$,i%,1%) = "*" then pc_sub%(i%) = i% + 40%
               pc_stdc$(i%), pc_spcc$(i%) = "0"         /* Not Applic. */
               pc_stdr$(i%), pc_spcr$(i%) = "000000000"

/*(AWD006)*/
               pc_subc$(i%) = "0"
               pc_subr$(i%) = "000000000"

           next i%
           cnt% = 0%
           k% = 0%
           cnt$ = "Records Written [ XXXXX ]"
           
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
            goto inputmode

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        dataload
            get #1, using L35040, pc_c$, pc_cm$, pc_m$, pc_std%(),       ~
                                 pc_stdc$(), pc_stdr$(), pc_spc%(),      ~
                                 pc_spcc$(), pc_spcr$(), pc_sub%(),      ~
/*(AWD006)*/                     pc_subc$(), pc_subr$(), pc_user$,       ~
/* CR2289 */                     pc_date$, pc_time$, pc_dfil$

            if ecs_flat% = 1% then return
            if all_model% = 1% then return            /* (AWD008)  */

               pc_vm$ = str(pc_m$,4%,13%)                  /* (CR2451)  */
               pc_m$ = str(pc_m$,1%,3%)                    /* (CR2451)  */
               gosub L50150                    /* CATALOG CODES         */
               gosub L50330                    /* CATALOG METHOD CODES  */
               if edit% <> 1% then gosub L50510/* CATALOG MODEL/PRODUCT */
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

        copy_data
/*AWD008*/  all_model% = 0%
/*EWD002*/  if pc_mc$ = "ALL" then copy_all /* Copy Data from a Catalog */
/*AWD007*/  pc_c$  = pc_cc_beg$             /* and Model to Another     */
            if pc_cc_beg$ <> pc_cc_end$ then copy_all   /*(AWD007) */
/*EWD002*/  pc_cm$ = pc_cmc$
            pc_m$  = pc_mc$
            call "SHOSTAT" ( "Copying Pricing Priorities" )

        delete_it
        dataput
            pc_key$ = all(hex(00))
            str(pc_key$,1%,4%) = pc_c$
            str(pc_key$,5%,2%) = pc_cm$
            str(pc_key$,7%,16%)= pc_m$
            read #1,hold,key = pc_key$, eod goto L31200
               delete #1
            if keyhit% = 12% then call "SHOSTAT" ("Definition Deleted")
L31200:     if keyhit% = 12% then goto L31260
/* CR2289 */
            pc_user$ = userid$
            pc_date$ = date
            pc_time$ = time 
            
            put #1, using L35040,    pc_c$, pc_cm$, pc_m$, pc_std%(),     ~
                                    pc_stdc$(), pc_stdr$(), pc_spc%(),    ~
                                    pc_spcc$(), pc_spcr$(), pc_sub%(),    ~
/*(AWD006)*/                        pc_subc$(), pc_subr$(), pc_user$,     ~
/* CR2289 */                        pc_date$, pc_time$, pc_dfil$

            write #1, eod goto L31280
                         
            if pc_mc$ = "ALL" then return           /*EWD002*/
            if pc_cc_beg$ <> pc_cc_end$ then return /*AWD007*/
            if keyhit% = 8%   then return           /*EWD003*/
L31260: return clear all
        goto inputmode
L31280:     errormsg$ = "(Error) - Calc. Definition File???"
        return


        copy_all                    /* New - (EWD002) */
            all_model% = 1%              /* (AWD008) */
            call "SHOSTAT" ("Copying Pricing Priorities-ALL Models")
            init(" ") sav_mod$                  /*AWD007*/
            sav_mod$ = pc_m$                    /*AWD007*/
            if keyhit% = 7% then sav_mod$ = pc_mc$
            sav_key$ = pc_c$ & pc_cm$ 
            readkey$ = pc_c$ & pc_cm$ & hex(000000)
            pc_c$  = pc_cc_beg$             
            pc_cm$ = pc_cmc$
            copy_all% = 0%
          copy_all_next
            read #1, key > readkey$, using L31500, readkey$,           ~
                                                eod goto copy_all_end
/* (AWD007) */

L31500:     fmt CH(22)

REM   copy mulitple Model
            if str(pc_mc$,,3) = "ALL" then copy_all_model

REM   copy multiple Catalogs
            if str(readkey$,,4) > pc_cc_end$ then copy_all_end
         
            if str(readkey$,5,2%) <> str(sav_key$,5,2%) then copy_all_next


               if str(sav_mod$,1,3) <> str(readkey$,7,3) then copy_all_next


               print at(02,09);hex(84);readkey$;
               pc_c$  = str(readkey$,1,4)
               pc_cm$ = str(readkey$,5,2)

               if keyhit%  = 9% then gosub dataload
               if keyhit% <> 7% then pc_m$  = pc_mc$

               goto write_data
copy_all_model

            if str(readkey$,,6%) <> str(sav_key$,,6%) then copy_all_end

            gosub dataload

            pc_c$  = pc_cc_beg$
            pc_cm$ = pc_cmc$
REM  PC_M$  = STR(READKEY$,7%,16%)
            pc_m$  = str(readkey$,7%,3%)

write_data
            gosub dataput
                if errormsg$ <> " " then return
            copy_all% = copy_all% + 1%
            goto copy_all_next

          copy_all_end
            convert copy_all% to copy_all$, pic(####0)
            call "ASKUSER" (2%, "*** DONE ***","No. of Models Copied = " ~
                & copy_all$, "---", "Press <ENTER> to Continue")
            all_model% = 0%              /* (AWD008) */
            return clear all
            goto inputmode          /* End - (EWD002) */


        global_update               /* New - (EWD003) */
            k% = 2%
            call "ASKUSER" (k%, "*** CONFIRM UPDATE ***","Press <PF16> " ~
                & "to Start Global Update","-- OR --","<ENTER> to Abort")
            if k%  =  0% then return
            if k% <> 16% then global_update
            call "SHOSTAT" ("Global Update Processing Occurring...")
            readkey$ = "PRICE 000" & "0000" & hex(ff) /* Start at Next */
            update% = 0%
          global_update_next
            read #2, key > readkey$, using L31700, readkey$,             ~
                eod goto global_update_end
L31700:             fmt ch(24)
            if str(readkey$,,9%) <> "PRICE 000" then global_update_end
            pc_c$ = str(readkey$,10%,4%)
            sav_key$ = pc_c$ & pc_cm$ & pc_m$
            read #1, key = sav_key$, eod goto global_update_next
            gosub dataput           /* ^ Update Existing Only ^ */
                if errormsg$ <> " " then return
            update% = update% + 1%
            goto global_update_next

          global_update_end
            convert update% to copy_all$, pic(####0)
            call "ASKUSER" (2%,"*** DONE ***","No. of Records Updated = "~
                & copy_all$, "---", "Press <ENTER> to Continue")
            return clear all
            goto inputmode          /* End - (EWD003) */


        REM *************************************************************~
            *       C O N V E R T   D A T A   F O R   S C R E E N       *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        toggle_data
            k% = 0%
            gosub unload_screen
            if std_spc% <> 1% or keyhit% = 10% then goto L32100
               gosub load_special
               return

L32100:     if keyhit% = 10% and std_spc% <> 3% then goto L32150
            gosub load_standard
        return

L32150:     std_spc% = 3%
            gosub load_subpart

        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L35040: FMT                     /* (APCPCMSD)-Master Calc. Definition  */~
            CH(04),             /* Pricing Catalog Code - (PRICE 000)  */~
            CH(02),             /* Pricing Calc. Method - (PRICE 001)  */~
/*(CR2451)*/ CH(16),             /* Model/Product Code   - (MODEL    )  */~
/*            XX(13),              Filler for Model                    */~
            20*BI(1),           /* Standard Priority Codes             */~
            20*CH(1),           /* Standard Price Cross-Ref Codes      */~
            20*CH(9),           /* Init Ref. Calc Codes (3),(3),(3)    */~
            20*BI(1),           /* Special Priority Codes              */~
            20*CH(1),           /* Special Price Cross-Ref Codes       */~
            20*CH(9),           /* Init Ref. Calc Codes (3),(3),(3)    */~
            20*BI(1),           /* Sub part sequence          (AWD006) */~
            20*CH(1),           /* Sub priority codes         (AWD006) */~
            20*CH(9),           /* Sub Ref calc codes         (AWD006) */~
            CH(03),             /* User ID making change       CR2289  */~
            CH(06),             /* System date change made     CR2289  */~
            CH(06),             /* System time change made     CR2289  */~
            CH(71)              /* Filler Area                         */

        FMT                     /* (ECSCONTR)-Master Control File- Flat*/~
            CH(04),             /* Pricing Catalog Code - (PRICE 000)  */~
            CH(02),             /* Pricing Calc. Method - (PRICE 001)  */~
            CH(16),             /* Model/Product Code   - (MODEL    )  */~
            CH(02),             /* Price Pri Calc Seq - Std/Spc        */~
            CH(02),             /* Ref Codes (01-20)=Std,(21-40)=Spc   */~
            CH(01),             /* Price Cross-Ref Code (PRICE 011)    */~
            CH(03),             /* Initial Calc Ref (1) (PRICE 003)    */~
            CH(03),             /* Initial Calc Ref (2) (PRICE 003)    */~
            CH(03),             /* Initial Calc Ref (3) (PRICE 003)    */~
            CH(09)              /* Filler Area                         */

        REM *************************************************************~
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
L40035:       gosub'050(1%, fieldnr%)
              gosub set_pf1
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L40095,         /* Price Catalog       */ ~
                                L40095,         /* Price Catalog Method*/ ~
                                L40095,         /* Product/Model Code  */ ~
                                L40095          /* Standard/Special    */
                                
/* (CR2451) */
              if virMdlSec% = 99% and fieldnr% = 3% then mdllfac$ = hex(81)
              if virMdlSec% = 99% and fieldnr% <> 3% then mdllfac$ = hex(8c)                                 
              goto L40110

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40095:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40110:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Price Catalog Code      :",                  ~
               at (03,30), fac(lfac$( 1)), pc_c$                , ch(04),~
               at (03,40), fac(hex(84)), pc_c_d$                , ch(30),~
                                                                         ~
               at (04,02), "Price Calc. Method Code :",                  ~
               at (04,30), fac(lfac$( 2)), pc_cm$               , ch(02),~
               at (04,40), fac(hex(84)), pc_cm_d$               , ch(30),~
                                                                         ~
               at (05,02), "Model/Product Code      :",                  ~
               at (05,30), fac(lfac$( 3)), pc_m$                , ch(03),~
/*(CR2451)*/   at (05,34), fac(mdllfac$), pc_vm$                , ch(13),~               
               at (05,40), fac(hex(84)), pc_m_d$                , ch(30),~
                                                                         ~
               at (07,02), fac(hex(a4)), hdr1$                  , ch(30),~
               at (07,34), fac(hex(a4)), hdr2$                  , ch(04),~
               at (07,40), fac(hex(a4)), hdr3$                  , ch(03),~
               at (07,44), fac(hex(a4)), hdr4$                  , ch(20),~
               at (07,66), fac(hex(a4)), hdr5$                  , ch(03),~
               at (07,71), fac(hex(a4)), hdr6$                  , ch(03),~
               at (07,76), fac(hex(a4)), hdr7$                  , ch(03),~
                                                                         ~
               at (08,02), fac(hex(84)), scr$(1%+k%)               , ch(30),~
               at (08,35), fac(lfac$(4%)), scr_d$(1%+k%)           , ch(02),~
               at (08,41), fac(lfac$(4%)), scrc$(1%+k%)            , ch(01),~
               at (08,44), fac(hex(84)), scrc_d$(1%+k%)            , ch(20),~
               at (08,66), fac(lfac$(4%)), scrc_1$(1%+k%)          , ch(03),~
               at (08,71), fac(lfac$(4%)), scrc_2$(1%+k%)          , ch(03),~
               at (08,76), fac(lfac$(4%)), scrc_3$(1%+k%)          , ch(03),~
                                                                         ~
               at (09,02), fac(hex(84)), scr$(2%+k%)               , ch(30),~
               at (09,35), fac(lfac$(4%)), scr_d$(2%+k%)           , ch(02),~
               at (09,41), fac(lfac$(4%)), scrc$(2%+k%)            , ch(01),~
               at (09,44), fac(hex(84)), scrc_d$(2%+k%)            , ch(20),~
               at (09,66), fac(lfac$(4%)), scrc_1$(2%+k%)          , ch(03),~
               at (09,71), fac(lfac$(4%)), scrc_2$(2%+k%)          , ch(03),~
               at (09,76), fac(lfac$(4%)), scrc_3$(2%+k%)          , ch(03),~
                                                                         ~
               at (10,02), fac(hex(84)), scr$(3%+k%)               , ch(30),~
               at (10,35), fac(lfac$(4%)), scr_d$(3%+k%)           , ch(02),~
               at (10,41), fac(lfac$(4%)), scrc$(3%+k%)            , ch(01),~
               at (10,44), fac(hex(84)), scrc_d$(3%+k%)            , ch(20),~
               at (10,66), fac(lfac$(4%)), scrc_1$(3%+k%)          , ch(03),~
               at (10,71), fac(lfac$(4%)), scrc_2$(3%+k%)          , ch(03),~
               at (10,76), fac(lfac$(4%)), scrc_3$(3%+k%)          , ch(03),~
                                                                         ~
               at (11,02), fac(hex(84)), scr$(4%+k%)               , ch(30),~
               at (11,35), fac(lfac$(4%)), scr_d$(4%+k%)           , ch(02),~
               at (11,41), fac(lfac$(4%)), scrc$(4%+k%)            , ch(01),~
               at (11,44), fac(hex(84)), scrc_d$(4%+k%)            , ch(20),~
               at (11,66), fac(lfac$(4%)), scrc_1$(4%+k%)          , ch(03),~
               at (11,71), fac(lfac$(4%)), scrc_2$(4%+k%)          , ch(03),~
               at (11,76), fac(lfac$(4%)), scrc_3$(4%+k%)          , ch(03),~
                                                                         ~
               at (12,02), fac(hex(84)), scr$(5%+k%)               , ch(30),~
               at (12,35), fac(lfac$(4%)), scr_d$(5%+k%)           , ch(02),~
               at (12,41), fac(lfac$(4%)), scrc$(5%+k%)            , ch(01),~
               at (12,44), fac(hex(84)), scrc_d$(5%+k%)            , ch(20),~
               at (12,66), fac(lfac$(4%)), scrc_1$(5%+k%)          , ch(03),~
               at (12,71), fac(lfac$(4%)), scrc_2$(5%+k%)          , ch(03),~
               at (12,76), fac(lfac$(4%)), scrc_3$(5%+k%)          , ch(03),~
                                                                         ~
               at (13,02), fac(hex(84)), scr$(6%+k%)               , ch(30),~
               at (13,35), fac(lfac$(4%)), scr_d$(6%+k%)           , ch(02),~
               at (13,41), fac(lfac$(4%)), scrc$(6%+k%)            , ch(01),~
               at (13,44), fac(hex(84)), scrc_d$(6%+k%)            , ch(20),~
               at (13,66), fac(lfac$(4%)), scrc_1$(6%+k%)          , ch(03),~
               at (13,71), fac(lfac$(4%)), scrc_2$(6%+k%)          , ch(03),~
               at (13,76), fac(lfac$(4%)), scrc_3$(6%+k%)          , ch(03),~
                                                                         ~
               at (14,02), fac(hex(84)), scr$(7%+k%)               , ch(30),~
               at (14,35), fac(lfac$(4%)), scr_d$(7%+k%)           , ch(02),~
               at (14,41), fac(lfac$(4%)), scrc$(7%+k%)            , ch(01),~
               at (14,44), fac(hex(84)), scrc_d$(7%+k%)            , ch(20),~
               at (14,66), fac(lfac$(4%)), scrc_1$(7%+k%)          , ch(03),~
               at (14,71), fac(lfac$(4%)), scrc_2$(7%+k%)          , ch(03),~
               at (14,76), fac(lfac$(4%)), scrc_3$(7%+k%)          , ch(03),~
                                                                         ~
               at (15,02), fac(hex(84)), scr$(8%+k%)               , ch(30),~
               at (15,35), fac(lfac$(4%)), scr_d$(8%+k%)           , ch(02),~
               at (15,41), fac(lfac$(4%)), scrc$(8%+k%)            , ch(01),~
               at (15,44), fac(hex(84)), scrc_d$(8%+k%)            , ch(20),~
               at (15,66), fac(lfac$(4%)), scrc_1$(8%+k%)          , ch(03),~
               at (15,71), fac(lfac$(4%)), scrc_2$(8%+k%)          , ch(03),~
               at (15,76), fac(lfac$(4%)), scrc_3$(8%+k%)          , ch(03),~
                                                                         ~
               at (16,02), fac(hex(84)), scr$(9%+k%)               , ch(30),~
               at (16,35), fac(lfac$(4%)), scr_d$(9%+k%)           , ch(02),~
               at (16,41), fac(lfac$(4%)), scrc$(9%+k%)            , ch(01),~
               at (16,44), fac(hex(84)), scrc_d$(9%+k%)            , ch(20),~
               at (16,66), fac(lfac$(4%)), scrc_1$(9%+k%)          , ch(03),~
               at (16,71), fac(lfac$(4%)), scrc_2$(9%+k%)          , ch(03),~
               at (16,76), fac(lfac$(4%)), scrc_3$(9%+k%)          , ch(03),~
                                                                         ~
               at (17,02), fac(hex(84)), scr$(10%+k%)              , ch(30),~
               at (17,35), fac(lfac$(4%)), scr_d$(10%+k%)          , ch(02),~
               at (17,41), fac(lfac$(4%)), scrc$(10%+k%)           , ch(01),~
               at (17,44), fac(hex(84)), scrc_d$(10%+k%)           , ch(20),~
               at (17,66), fac(lfac$(4%)), scrc_1$(10%+k%)         , ch(03),~
               at (17,71), fac(lfac$(4%)), scrc_2$(10%+k%)         , ch(03),~
               at (17,76), fac(lfac$(4%)), scrc_3$(10%+k%)         , ch(03),~
                                                                         ~
               at (18,02), fac(hex(84)), scr$(11%+k%)              , ch(30),~
               at (18,35), fac(lfac$(4%)), scr_d$(11%+k%)          , ch(02),~
               at (18,41), fac(lfac$(4%)), scrc$(11%+k%)           , ch(01),~
               at (18,44), fac(hex(84)), scrc_d$(11%+k%)           , ch(20),~
               at (18,66), fac(lfac$(4%)), scrc_1$(11%+k%)         , ch(03),~
               at (18,71), fac(lfac$(4%)), scrc_2$(11%+k%)         , ch(03),~
               at (18,76), fac(lfac$(4%)), scrc_3$(11%+k%)         , ch(03),~
                                                                         ~
               at (19,02), fac(hex(84)), scr$(12%+k%)              , ch(30),~
               at (19,35), fac(lfac$(4%)), scr_d$(12%+k%)          , ch(02),~
               at (19,41), fac(lfac$(4%)), scrc$(12%+k%)           , ch(01),~
               at (19,44), fac(hex(84)), scrc_d$(12%+k%)           , ch(20),~
               at (19,66), fac(lfac$(4%)), scrc_1$(12%+k%)         , ch(03),~
               at (19,71), fac(lfac$(4%)), scrc_2$(12%+k%)         , ch(03),~
               at (19,76), fac(lfac$(4%)), scrc_3$(12%+k%)         , ch(03),~
                                                                         ~
               at (20,02), fac(hex(84)), scr$(13%+k%)              , ch(30),~
               at (20,35), fac(lfac$(4%)), scr_d$(13%+k%)          , ch(02),~
               at (20,41), fac(lfac$(4%)), scrc$(13%+k%)           , ch(01),~
               at (20,44), fac(hex(84)), scrc_d$(13%+k%)           , ch(20),~
               at (20,66), fac(lfac$(4%)), scrc_1$(13%+k%)         , ch(03),~
               at (20,71), fac(lfac$(4%)), scrc_2$(13%+k%)         , ch(03),~
               at (20,76), fac(lfac$(4%)), scrc_3$(13%+k%)         , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L40820
                  call "PRNTSCRN"
                  goto L40110

L40820:        if keyhit% <> 2% then goto L40850
REM                  sav_key$ = "PRICE 000"                    /*EWD001*/
REM                  gosub display_codes
                  if k% = 0% then goto k0 
                  
                  k% = 0%
                  goto L40035
k0:
                  k% = 10%                  
                  goto L40035
                  
L40850:        if keyhit% <> 3% then goto L40855

                  gosub toggle_data
                  goto L40035
/*(AWD006) */
L40855:        if keyhit% <> 10% then goto L40870
REM                  if std_spc% = 3% then std_spc% = 2% ~
                  else std_spc% = 3%
                  
                  gosub toggle_data
                  goto L40035

L40870:        if keyhit% <> 5% then goto L40900
                  sav_key$ = "PRICE 001"                    /*EWD001*/
                  gosub display_codes
                  goto L40035

L40900:        if keyhit% <> 6% then goto L40930
                  sav_key$ = "MODEL    "                    /*EWD001*/
                  gosub display_codes
                  goto L40035

L40930:        if keyhit% <> 7% then goto L40960
                  sav_key$ = "PRICE 011"                    /*EWD001*/
                  gosub display_codes
                  goto L40035

L40960:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
           hdr1$ = "<--- Std/Ref  Description --->"
           hdr2$ = "Seq."
           hdr3$ = "C/R"
           hdr4$ = "Cross Reference Desc"
           hdr5$ = "RF1"
           hdr6$ = "RF2"
           hdr7$ = "RF3"


        if edit% = 2% then L41115     /*  Input Mode             */
            pf$(1) = "(1)Start Over      (4)Previous Fields   " &        ~
                     "(7)Cross-Ref Codes     (14)Print Report"
            pf$(2) = "(2)Prev/Next       (5)Catalog Methods   " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(3)Toggle Std/Spc  (6)Model/Product     " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01020304050607ffffffffffff0e0f1000)
            if fieldnr% = 1% then L41085
                str(pf$(1),64)    = " "  :  str(pfkeys$,14,1) = hex(ff)
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L41085:     if fieldnr% > 1% then L41095
                str(pf$(1),20,18) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L41095:     if fieldnr% = 4% then goto L41105
                str(pf$(3%),1%,18%) = " " : str(pfkeys$, 3,1) = hex(ff)
L41105:     return

L41115: if fieldnr% > 0% then L41170  /*  Edit Mode - Select Fld */
/*(AWD006)*/
            pf$(1) = "(1)Start Over      (10)Toggle Std/Sub   " &        ~
                     "(7)Cross-Ref Codes     (12)Delete      "
            pf$(2) = "(2)Prev/Next       (5)Catalog Methods   " &        ~
/*EWD003*/           "(8)Global Update       (15)Print Screen"
            pf$(3) = "(3)Toggle Std/Spc  (6)Model/Product     " &        ~
                     "(9)Copy Data           (16)Save Data   "
/*EWD003*/  pfkeys$ = hex(010203ff05060708090aff0cffff0f1000)
/* CR2267 */
            if authr% = 1% then goto L41155
                       
            pf$(1) = "(1)Start Over      (10)Toggle Std/Sub   " &        ~
                     "(7)Cross-Ref Codes                     "
            pf$(2) = "(2)Prev/Next       (5)Catalog Methods   " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "(3)Toggle Std/Spc  (6)Model/Product     " &        ~
                     "                                       "
            pfkeys$ = hex(010203ff050607ffff0affffffff0fff00)

L41155:     if rec% = 1% then goto L41160
                str(pf$(1%),64)     = " " : str(pfkeys$,12,1) = hex(ff)
L41160:     if pc_c$ = "0000" then L41165   /*EWD003*/
                str(pf$(2),41%,16%) = " " : str(pfkeys$, 8,1) = hex(ff)
L41165:     return
L41170:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "(7)Cross-Ref Codes                     "
            pf$(2) = "(2)Prev/Next       (5)Catalog Methods   " &        ~
                     "                                       "
            pf$(3) = "                   (6)Model/Product     " &        ~
                     "                                       "
            pfkeys$ = hex(0102ffff050607ffffffffffffffffff00)
            return

        REM *************************************************************~
            *    C a l c u l a t i o n   S e q u e n c e   C o d e s    *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_codes                                     
            call "EWDPLA63" (0%, #2, sav_key$, 0%, " ")     /*EWD001*/
            return          

        REM *************************************************************~
            *               R e p o r t   S c r e e n                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
L44035:       gosub'060(1%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L44095,         /* Beg Catalog Code  */   ~
                                L44095,         /* End Catalog Code  */   ~
                                L44095,         /* Beg Model Code    */   ~
                                L44095          /* End Model Code    */
/* (CR2451) */
              if virMdlSec% = 99% and fieldnr% = 3% then mdllfac$ = hex(81)
              if virMdlSec% = 99% and fieldnr% = 4% then mdllfac1$ = hex(81)
              if virMdlSec% = 99% and fieldnr% <> 3% then mdllfac$ = hex(8c)                 
              if virMdlSec% = 99% and fieldnr% <> 4% then mdllfac1$ = hex(8c)    
              goto L44110

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L44095:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L44110:     accept                                                       ~
               at (01,02),                                               ~
          "Input Report Criteria for Catalog Calc. Priority Definitions",~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,02), "Beginning Catalog Code:",                    ~
               at (03,30), fac(lfac$(1%)), beg_c$               , ch(04),~
               at (03,40), fac(hex(84)), beg_c_d$               , ch(30),~
                                                                         ~
               at (04,02), "Ending Catalog Code   :",                    ~
               at (04,30), fac(lfac$(2%)), end_c$               , ch(04),~
               at (04,40), fac(hex(84)), end_c_d$               , ch(30),~
                                                                         ~
               at (05,02), "Beginning Model Code  :",                    ~
               at (05,30), fac(lfac$(3%)), beg_mod$             , ch(03),~
/*(CR2451)*/   at (05,34), fac(mdllfac$), beg_vmod$             , ch(13),~               
               at (05,40), fac(hex(84)), beg_mod_d$             , ch(30),~
                                                                         ~
               at (06,02), "Ending Model Code     :",                    ~
               at (06,30), fac(lfac$(4%)), end_mod$             , ch(03),~
/*(CR2451)*/   at (06,34), fac(mdllfac1$), end_vmod$            , ch(13),~               
               at (06,40), fac(hex(84)), end_mod_d$             , ch(30),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <> 10% then goto L44285
                  ecs_flat% = 1%


L44285:        if keyhit% <> 2% then goto L44315
                  sav_key$ = "PRICE 000"                    /*EWD001*/
                  gosub display_codes
                  goto L44035

L44315:        if keyhit% <> 15% then goto L44335
                  call "PRNTSCRN"
                  goto L44035

L44335:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
        if edit% = 2% then L44430     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                                       "
            pf$(2) = "(2)Catalog Codes                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(0102ff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L44410
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L44410:     if fieldnr% > 1% then L44420
                str(pf$(1),18,18) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L44420:     return

L44430: if fieldnr% > 0% then L44475  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "(10)ECS Flat File      (14)Print Report"
            pf$(2) = "(2)Catalog Codes                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(0102ffffffffffffff0affffff0e0f1000)
            return
L44475:                              /*  Edit Mode - Enabled    */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "(2)Catalog Codes                        " &        ~
                     "                                       "
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(0102ffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *               C O P Y   S C R E E N                       *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
              gosub'070(1%, fieldnr%)
              gosub set_pf4
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L45150,         /* TO Price Catalog    */ ~
                                L45150,         /* TO Price Catalog MT.*/ ~
                                L45150          /* TO Product/Model Cod*/
                                
/* (CR2451) */
              if virMdlSec% = 99% and fieldnr% = 3% then mdllfac$ = hex(81)
              if virMdlSec% = 99% and fieldnr% <> 3% then mdllfac$ = hex(8c)                                  
              goto L45180

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L45150:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L45180:     accept                                                       ~
               at (01,02),                                               ~
                  "Pricing Copy Calc. Definition Priority Sequence",     ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
/*EWD002*/     at (03,70), "Copy From",                                  ~
                                                                         ~
               at (04,02), "To Price Catalog Code   :",                  ~
/*AWD007*/     at (04,30), fac(lfac$(1%)), pc_cc_beg$           , ch(04),~
/*AWD007*/     at (04,40), fac(lfac$(1%)), pc_cc_end$           , ch(04),~
/*AWD007*/     at (04,50), fac(hex(84)), pc_c_dc$               , ch(20),~
/*EWD002*/     at (04,72), fac(hex(84)), pc_c$                  , ch(04),~
                                                                         ~
               at (05,02), "To Price Calc. Meth Code:",                  ~
               at (05,30), fac(lfac$(2%)), pc_cmc$              , ch(02),~
               at (05,40), fac(hex(84)), pc_cm_dc$              , ch(30),~
/*EWD002*/     at (05,72), fac(hex(84)), pc_cm$                 , ch(02),~
                                                                         ~
               at (06,02), "To Model/Product Code   :",                  ~
               at (06,30), fac(lfac$(3%)), pc_mc$               , ch(03),~
               at (06,34), fac(mdllfac$), pc_vmc$               , ch(16),~
               at (06,40), fac(hex(84)), pc_m_dc$               , ch(30),~
/*EWD002*/     at (06,72), fac(hex(84)), pc_m$                  , ch(16),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
               
               
               if keyhit% <> 7 then goto L45480
                  goto L45490
                  
L45480:                 
               if keyhit% <> 15 then goto L45490
                  call "PRNTSCRN"
                  goto L45180

L45490:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_pf4
        if edit% = 2% then L45690     /*  Input Mode             */
            pf$(1) = "(1)Start Over      (4)Previous Fields   " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L45650
                str(pf$(1),64)    = " "  :  str(pfkeys$,14,1) = hex(ff)
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L45650:     if fieldnr% > 1% then L45670
                str(pf$(1),20,18) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L45670: return

L45690: if fieldnr% > 0% then L45780  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "           (07)Copy New Model           " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "           (09)Copy Cat to Cat          " &        ~
                     "                       (16)Copy Data   "
            pfkeys$ = hex(01ffffffffff07ff09ffffffffff0f1000)
        return
L45780:                              /*  Edit Mode - Enabled    */
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
            * Edits for Items on Screen 1                               *~
            *************************************************************

        deffn'151(fieldnr%)
            tab_val$, errormsg$ = " "
              on fieldnr% gosub L50150,         /* Price Catalog Codes  */~
                                L50330,         /* Catalog Calc. Method */~
                                L50510,         /* Model/Product Codes  */~
                                L50840          /* Std/Spc Priorities   */

            return

L50150: REM Price Catalog Codes                   PC_C$
           if pc_c$ <> " " then goto L50180
              pc_c$ = "0000"
L50180:    convert pc_c$ to zz%, data goto L50190
L50190:
           convert zz% to pc_c$, pic(0000)

           tab_2% = 0%
           tab_1% = 10%                       /* (PRICE 000) Catalogs  */
           tab_val$ = pc_c$
           gosub lookup_tab_2
           if tab_rec% = 0% then goto L50290
               pc_c_d$ = tab_desc$
        return
L50290:    errormsg$ = "(Error) - Invalid Catalog Code Specified?"
           pc_c$, pc_c_d$ = " "
        return

L50330: REM Price Catalog Method Codes            PC_CM$
           if pc_cm$ <> " " then goto L50360
              pc_cm$ = "00"
L50360:    convert pc_cm$ to zz%, data goto L50370
L50370:
           convert zz% to pc_cm$, pic(00)

           tab_2% = 0%
           tab_1% = 1%                   /* (PRICE 001) Calc. Methods  */
           tab_val$ = pc_cm$
           gosub lookup_tab_2
           if tab_rec% = 0% then goto L50470
              pc_cm_d$ = tab_desc$
        return
L50470:    errormsg$ = "(Error) - Invalid Catalog Calc. Method Code?"
           pc_cm$, pc_cm_d$ = " "
        return

L50510: REM Model/Product Codes                   PC_M$
           if pc_m$ <> " " then goto L50540
              goto L50800
L50540:    if str(pc_m$,1%,1%) = "9" then goto L50560 /* Do not do for Bay Bows */
           goto L50560         /* AWD009  skip numeric checking */
           convert pc_m$ to zz%, data goto L50550     /* (EWD005) */
L50550:
           convert zz% to pc_m$, pic(000)

           if zz% = 0% then goto L50800        /* Must be a Valid Model */
L50560:    tab_2% = 1%
           tab_1% = 0%                   /* (MODEL    ) Model/Product  */
           tab_val$ = str(pc_m$,1,3)
           gosub lookup_tab_2
           if tab_rec% = 0% then goto L50800
              pc_m_d$ = tab_desc$
              
/* (CR2451) begin */               
            if pc_vm$ = " " then dataLookup
              pc_m$ = pc_m$ & pc_vm$
              init(" ") readkey$, descr$
              readkey$ = "VIRTUALMD" & pc_m$ & " "
              call "DESCRIBE" (#2, readkey$, str(descr$,,19%) ,0%, f1%(2)) 
                if descr$ = " " then goto L50800
                
dataLookup:  
/* (CR2451) end   */                
           if edit% <> 1% then goto L50780
           if rpt% = 1% then return
              pc_key$ = all(hex(00))
              str(pc_key$,1%,4%) = pc_c$
              str(pc_key$,5%,2%) = pc_cm$
              str(pc_key$,7%,16%) = pc_m$
              rec% = 0%
              read #1,hold,key = pc_key$, eod goto L50780
                  rec% = 1%
                  gosub dataload
                  fieldnr% = 4%

L50780:     gosub load_standard
        return
L50800:    errormsg$ = "(Error) - Invalid Model/Product Code?"
           pc_m$, pc_m_d$ = " "
        return

L50840: REM Standard/Special Priorities           PC_STD%(), PC_SPC%()
           gosub unload_screen
           str(chk_pri$,1%,60%) = " "
           for i% = 1% to 60%                 /* (AWD006) */
               if i% > 20% then goto L50950
                  if pc_std%(i%) = 0% then goto L51000
                     xx% = pc_std%(i%)
                     if xx% = 0% then goto L51000
                     if str(chk_pri$,xx%,1%) <> " " then goto L51020
                        str(chk_pri$,xx%,1%) = "*"
                        goto L51000
L50950:        if i% > 40% then goto L50110     /*(AWD006)*/

                  if pc_spc%(i%-20%) = 0% then goto L51000
                  xx% = pc_spc%(i%-20%)
                  if xx% = 0% then goto L51000
                  if str(chk_pri$,xx%,1%) <> " " then goto L51020
                     str(chk_pri$,xx%,1%) = "*"
                     goto L51000


L50110:           if pc_sub%(i%-40%) = 0% then goto L51000
                  xx% = pc_sub%(i%-40%)
                  if str(chk_pri$,xx%,1%) <> " " then goto L51020
                     str(chk_pri$,xx%,1%) = "*"
                    

L51000:    next i%
        return
L51020:    convert xx% to xx$, pic(00)

           errormsg$="(Error) - Priority Code ( "&xx$&" ) Has Been Used?"
REM           if i% < 21% then scr_d$(i%) = "00"                            ~
                       else scr_d$(i%-20%) = "00"

              if i% < 21% then scr_d$(i%) = "00"
              if i% > 20% and i% < 41% then scr_d$(i%-20%) = "00" 
              if i% > 40% then scr_d$(i%-40%) = "00"

        return

        load_standard
           std_spc% = 1%
           scr_hdr$ = "( Standard )"
           goto L51180
        load_special
           std_spc% = 2%
           scr_hdr$ = "( Special  )"
           goto L51180               /* (AWD006) */
        load_subpart
           std_spc% = 3%
           scr_hdr$ = "( Sub Part )"


L51180:    tab_1% = 4%
           gosub lookup_tab_1
           init(" ") scr$(), scr_d$(), scrc$(), scrc_d$(), scrc_1$(),  ~
                     scrc_2$(), scrc_3$()
           j% = 0%
           for i% = 1% to max_code%
               xx% = 0%
               convert code$(i%) to xx%, data goto L51250
L51250:
               if std_spc% = 1% and xx% > 20% then goto L51310
               if std_spc% = 2% and (xx% < 21% or xx% > 40%)  ~
                                             then goto L51310
/*(AWD006)*/
               if std_spc% = 3% and (xx% < 41% or xx% > 60%) ~
                                             then goto L51310
                  j% = j% + 1%
                  str(scr$(j%),1%,3%)  = code$(i%)
                  str(scr$(j%),4%,27%) = str(desc$(i%),1%,27%)
L51310:    next i%
           j_max% = j%
           for i% = 1% to j_max%
               if pc_stdc$(i%) <> " " then goto L51340
                  pc_stdc$(i%) = "0" : pc_stdr$(i%) = "000000000"
                  pc_spcc$(i%) = "0" : pc_spcr$(i%) = "000000000"
                  pc_subc$(i%) = "0" : pc_subr$(i%) = "000000000"

L51340:        if std_spc% <> 1% then goto L51410
                  convert pc_std%(i%) to scr_d$(i%), pic(00)
                  scrc$(i%)   = pc_stdc$(i%)
                  scrc_1$(i%) = str(pc_stdr$(i%),1%,3%)
                  scrc_2$(i%) = str(pc_stdr$(i%),4%,3%)
                  scrc_3$(i%) = str(pc_stdr$(i%),7%,3%)
                  goto L51470
L51410:                                             /*(AWD006)*/
               if std_spc% <> 2% then goto L51420
                 convert pc_spc%(i%) to scr_d$(i%), pic(00)
                 scrc$(i%)   = pc_spcc$(i%)
                 scrc_1$(i%) = str(pc_spcr$(i%),1%,3%)
                 scrc_2$(i%) = str(pc_spcr$(i%),4%,3%)
                 scrc_3$(i%) = str(pc_spcr$(i%),7%,3%)
                 goto L51470
L51420:       if std_spc% <> 3% then goto L51470
                 convert pc_sub%(i%) to scr_d$(i%), pic(00)
                 scrc$(i%)   = pc_subc$(i%)
                 scrc_1$(i%) = str(pc_subr$(i%),1%,3%)
                 scrc_2$(i%) = str(pc_subr$(i%),4%,3%)
                 scrc_3$(i%) = str(pc_subr$(i%),7%,3%)


L51470:    tab_2% = 0% : tab_1% = 12%
           tab_val$ = scrc$(i%)
           gosub lookup_tab_2
           scrc_d$(i%) = "(Error)-Invalid Code"
           if tab_rec% = 0% then goto L51540
              scrc_d$(i%) = str(tab_desc$,1%, p% - 1%)

L51540:    next i%
        return

        unload_screen
           for i% = 1% to 20%
               if std_spc% = 2% then goto L51670
               if std_spc% = 3% then goto L51685         /*(AWD006) */
                                     /* Must be Special on Screen  */
                  convert scr_d$(i%) to pc_std%(i%), data goto L51620
L51620:
                  pc_stdc$(i%) = scrc$(i%)
                  pc_stdr$(i%) = scrc_1$(i%) &scrc_2$(i%) &scrc_3$(i%)
                  goto L51720
                                     /* Must be Standard on Screen */
L51670:        convert scr_d$(i%) to pc_spc%(i%), data goto L51680
L51680:
               pc_spcc$(i%) = scrc$(i%)
               pc_spcr$(i%) = scrc_1$(i%) &scrc_2$(i%) &scrc_3$(i%)
               goto L51720                /* (AWD006) */


L51685:        convert scr_d$(i%) to pc_sub%(i%), data goto L51690
L51690:
               pc_subc$(i%) = scrc$(i%)
               pc_subr$(i%) = scrc_1$(i%) &scrc_2$(i%) &scrc_3$(i%)



L51720:    next i%
        return

        REM *************************************************************~
            *      R e p o r t   I n p u t   S c r e e n   E d i t s    *~
            *-----------------------------------------------------------*~
            * Edits for Items on Report Screen                          *~
            *************************************************************

        deffn'152(fieldnr%)
            tab_val$, errormsg$ = " "
              on fieldnr% gosub L51890,         /* Beg Ref Type Code    */~
                                L52130,         /* End Ref Type Code    */~
                                L52330,         /* Beg Mod Code         */~
                                L52520          /* End Model Code       */
              return

L51890: REM Price Catalog Code                    BEG_C$
           if beg_c$ <> " " then goto L51970
L51910:       beg_c$   = "ALL "
              beg_c_d$ = "(A)ll Catalog Codes "
              end_c$   = beg_c$
              end_c_d$ = beg_c_d$
              fieldnr% = 2%
              return
L51970:    if str(beg_c$,1%,1%) = "A" then goto L51910
           convert beg_c$ to zz%, data goto L51990
L51990:
           convert zz% to beg_c$, pic(0000)

           tab_2% = 0%
           tab_1% = 10%                        /* (PRICE 000) Catalogs  */
           tab_val$ = beg_c$
           gosub lookup_tab_2
           if tab_rec% = 0% then goto L52090
               beg_c_d$ = tab_desc$
        return
L52090:    errormsg$ = "(Error) - Invalid Beginning Catalog Code? "
           beg_c$, beg_c_d$ = " "
        return

L52130: REM Price Catalog Code                    END_C$
           if beg_c$ = "ALL " then return

           if end_c$ <> " " then goto L52180
              end_c$ = beg_c$
L52180:    convert end_c$ to zz%, data goto L52190
L52190:
           convert zz% to end_c$, pic(0000)

           tab_2% = 0%
           tab_1% = 10%                       /* (PRICE 000) Catalogs  */
           tab_val$ = end_c$
           gosub lookup_tab_2
           if tab_rec% = 0% then goto L52290
               end_c_d$ = tab_desc$
        return
L52290:    errormsg$ = "(Error) - Invalid Ending Catalog Code?"
           end_c$, end_c_d$ = " "
        return

L52330: REM Beginning Model Code                  BEG_MOD$
           if beg_mod$ <> " " then goto L52410
L52350:       beg_mod$ = "ALL"
              beg_mod_d$ = "(A)ll Model Codes"
              end_mod$ = beg_mod$
              end_mod_d$ = beg_mod_d$
              return
           if str(beg_mod$,1%,1%) = "A" then goto L52350
L52410:    tab_2% = 1%
           tab_1% = 0%                   /* (MODEL    ) Model/Product  */
           tab_val$ = str(beg_mod$,1,3)
           gosub lookup_tab_2
           if tab_rec% = 0% then goto L52480
              beg_mod_d$ = tab_desc$

/* (CR2451) begin */               
            if beg_vmod$ = " " then return
              beg_mod$ = beg_mod$ & beg_vmod$
              init(" ") readkey$, descr$
              readkey$ = "VIRTUALMD" & beg_mod$ & " "
              call "DESCRIBE" (#2, readkey$, str(descr$,,19%) ,0%, f1%(2)) 
                if descr$ = " " then goto L52480
/* (CR2451) end   */                            
        return
L52480:    errormsg$ = "(Error) - Invalid Beginning Model/Product Code?"
           init(" ") beg_mod$, beg_mod_d$
        return

L52520: REM Ending Model Code                     END_MOD$
           if end_mod$ <> " " then goto L52570
L52540:       end_mod$   = beg_mod$
              end_mod_d$ = beg_mod_d$
              return
L52570:    if str(end_mod$,1%,1%) = "A" then goto L52540
           tab_2% = 1%
           tab_1% = 0%                   /* (MODEL    ) Model/Product  */
           tab_val$ = str(end_mod$,1,3)
           gosub lookup_tab_2
           if tab_rec% = 0% then goto L52650
              end_mod_d$ = tab_desc$
              
/* (CR2451) begin */               
            if end_vmod$ = " " then validateModel
              end_mod$ = end_mod$ & end_vmod$
              init(" ") readkey$, descr$
              readkey$ = "VIRTUALMD" & end_mod$ & " "
              call "DESCRIBE" (#2, readkey$, str(descr$,,19%) ,0%, f1%(2)) 
                if descr$ = " " then goto L52650
                
validateModel:                
/* (CR2451) end   */                            
              
           if beg_mod$ > end_mod$ then goto L52650
        return
L52650:    errormsg$ = "(Error) - Invalid Ending Model/Product Code?"
           init(" ") end_mod$, end_mod_d$
        return

        REM *************************************************************~
            *       D a t a   I n p u t   S c r e e n   E d i t s       *~
            *-----------------------------------------------------------*~
            * Edits for Items on Copy Screen                            *~
            *************************************************************

        deffn'153(fieldnr%)
            tab_val$, errormsg$ = " "
              on fieldnr% gosub L52820,         /* Price Catalog Codes  */~
                                L53000,         /* Catalog Calc. Method */~
                                L53180          /* Model/Product Codes  */
            return

L52820: REM Price Catalog Codes                   PC_CC_BEG$, PC_CC_END$
           if pc_cc_beg$ <> " " then goto L52850    /* (AWD007) */
              goto L52960
L52850:    convert pc_cc_beg$ to zz%, data goto L52860
L52860:
           convert zz% to pc_cc_beg$, pic(0000)

           tab_2% = 0%
           tab_1% = 10%                       /* (PRICE 000) Catalogs  */
           tab_val$ = pc_cc_beg$              /* (AWD007) */
           gosub lookup_tab_2
           if tab_rec% = 0% then goto L52960
               pc_c_dc$ = tab_desc$

                                               /*(AWD007) - beg */
           if pc_cc_end$ <> " " then goto L52870
                pc_cc_end$ = pc_cc_beg$
L52870:
           convert pc_cc_end$ to zz%, data goto L52880

L52880:
           convert zz% to pc_cc_end$, pic(0000)

           tab_val$ = pc_cc_end$
           gosub lookup_tab_2
           if tab_rec% = 0% then goto L52965  /* (AWD007) - end */
        return
L52960:    errormsg$ = "(Error) - Invalid Beg Catalog Code Specified?"
           pc_cc_beg$, pc_c_dc$ = " "    /* (AWD007) */
        return
L52965:    errormsg$ = "(Error) - Invalid End Catalog Code Specified?"
           pc_cc_end$, pc_c_dc$ = " "    /* (AWD007) */
        return

L53000: REM Price Catalog Method Codes            PC_CMC$
           if pc_cmc$ <> " " then goto L53030
              goto L53140
L53030:    convert pc_cmc$ to zz%, data goto L53040
L53040:
           convert zz% to pc_cmc$, pic(00)

           tab_2% = 0%
           tab_1% = 1%                   /* (PRICE 001) Calc. Methods  */
           tab_val$ = pc_cmc$
           gosub lookup_tab_2
           if tab_rec% = 0% then goto L53140
              pc_cm_dc$ = tab_desc$
        return
L53140:    errormsg$ = "(Error) - Invalid Catalog Calc. Method Code?"
           pc_cmc$, pc_cm_dc$ = " "
        return

L53180: REM Model/Product Codes                   PC_MC$
           if pc_mc$ <> " " then goto L53210
              goto L53330
L53210:    if pc_mc$ <> "ALL" then goto L53215    /*EWD002*/
              pc_m_dc$ = "*** ALL Models ***"     /*   |  */
              pc_m$ = "ALL"                       /*   |  */
              return                              /*EWD002*/
L53215:    if str(pc_m$,1%,1%) = "9" then goto L53230 /* Do not do for Bay Bows */
           goto L53230       /* AWD009 skip numeric edit */
           convert pc_mc$ to zz%, data goto L53220    /* (EWD005) */
L53220:
           convert zz% to pc_mc$, pic(000)

           if zz% = 0% then goto L53330        /* Must be a Valid Model */
L53230:    tab_2% = 1%
           tab_1% = 0%                   /* (MODEL    ) Model/Product  */
/*EWD002*/ tab_val$ = str(pc_mc$,1,3)
           gosub lookup_tab_2
           if tab_rec% = 0% then goto L53330
              pc_m_dc$ = tab_desc$
              
/* (CR2451) begin */               
            if pc_vmc$ = " " then return
              pc_mc$ = pc_mc$ & pc_vmc$
              init(" ") readkey$, descr$
              readkey$ = "VIRTUALMD" & pc_mc$ & " "
              call "DESCRIBE" (#2, readkey$, str(descr$,,19%) ,0%, f1%(2)) 
                if descr$ = " " then goto L53330
/* (CR2451) end   */                   
        return
L53330:    errormsg$ = "(Error) - Invalid Model/Product Code?"
           pc_mc$, pc_m_dc$ = " "
        return

        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

                                                            /* HEADER  */
L55050: %+---------------------------------------------------------------~
        ~----------------------------------------------------------------+

L55080: %!---------------------------------------------------------------~
        ~----------------------------------------------------------------!

L55110: %!                                          #####################~
        ~#########################                             PAGE: ### !

L55140: %! Catalog Code  : ####  ##############################          ~
        ~                                      Date: ######## @ ######## !
L55160: %! Catalog Method:   ##  ##############################          ~
        ~                                                                !

L55190: %!Model!<-- Model Description -->!<---- Std/Spc Description --->!~
        ~ Priority!!!Cross-Ref!<--Descr->!Ref.  (1)!Ref.  (2)!Ref.  (3)  !

L55220: %! ### !#########################!##############################!~
        ~!   ##   !!!    #    !##########!   ###   !   ###   !   ###     ~
        ~!
L55250: %!-----!-------------------------!------------------------------!~
        ~---------!!!---------!----------!---------!---------!-----------!

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        print_header
          if lcnt% <> 99% then print using L55050
          page_no% = page_no% + 1%
          rpt_time$ = " "
          call "TIME" (rpt_time$)
          print page
          print using L55050
          print using L55110, print_title$, page_no%
          print using L55140, pc_c$, pc_c_d$, date$, rpt_time$
          print using L55160, pc_cm$, pc_cm_d$
          print using L55080
          print using L55190
          lcnt% = 6%
          sav_c$ = pc_c$ : sav_cm$ = pc_cm$
        return

        print_detail
          if ecs_flat% = 0% then goto L60250
             gosub update_flat_file
             return

L60250:   if pc_c$ <> sav_c$ or pc_cm$ <> sav_cm$ then                   ~
                                                  gosub print_header
          gosub load_standard
L60280:   for i% = 1% to 20%
              if lcnt% > 57% then gosub print_header
                 if i% = 1% then goto L60320
                    pc_m$, pc_m_d$ = " "
L60320:          x% = 0%
                 convert scr_d$(i%) to x%, data goto L60340
L60340:
                 if x% = 0% then goto L60420
                 print using L55250
                 print using L55220, pc_m$, str(pc_m_d$,1%,25%),          ~
                                    scr$(i%), scr_d$(i%), scrc$(i%),     ~
                                   scrc_d$(i%), scrc_1$(i%), scrc_2$(i%),~
                                   scrc_3$(1%)
                 lcnt% = lcnt% + 2%
L60420:       next i%
              if std_spc% = 2% then goto L60470
/*AWD006*/       if std_spc% = 3% then goto L60480
                 gosub load_special
                 goto L60280

L60470:          gosub load_subpart           /* (AWD006) */
                 goto L60280                  /* (AWD006) */


L60480: return

        select_printer
            if ecs_flat% = 0% then goto L60540
               gosub open_flat_file
               return

L60540:        page_no% = 0%
               lcnt%    = 99%
            print_title$="Master Calculation Priority Definitions Report"
               date$ = date  :  call "DATEFMT" (date$)
               rpt_time$ = " "
               call "TIME" (rpt_time$)
               call "COMPNAME" (12%, company$, f1%(5))
               call "SETPRNT" ("APCPRC", " ", 0%, 0%)
               select printer (134)
        return

        close_printer
            if ecs_flat% = 0% then goto L60700
               gosub create_ecscontr
               return

L60700:        print using L55050
               call "SETPRNT" ("APCPRC", " ", 0%, 1%)
        return

        generate_report
            init(" ") sav_c$, sav_cm$, pc_key$, cc$, sav_mod$
            rpt% = 1%
            if ecs_flat% = 0% then                                       ~
                 call "SHOSTAT" ("Printing Master Calc. Priority Report")~
             else call "SHOSTAT" ("Creating (ECSCONTR) Sort File")

            gosub select_printer
            if beg_c$ = "ALL " then goto generate_next
               str(pc_key$,1%,4%) = beg_c$
        generate_next
          read #1,key > pc_key$, using L60870, pc_key$,                   ~
                                                   eod goto generate_done
L60870:         FMT CH(22)
                                            /* TEST KEY FOR SELECTIONS */
             cc$ = str(pc_key$,1%,4%)
REM MM$ = STR(PC_KEY$,7%,16%)
             mm$ = str(pc_key$,7%,3%)

          if beg_c$ = "ALL " then goto L60960
             if cc$ < beg_c$ then goto generate_next
             if cc$ > end_c$ then goto generate_done

L60960:   if ecs_flat% = 1% then goto L61010
             if beg_mod$ = "ALL" then goto L61040  /* Standard Report   */
                if mm$ < beg_mod$ or mm$ > end_mod$ then                 ~
                                                       goto generate_next
                goto L61040
L61010:      gosub check_for_modle            /* ECS Flat Control File */
             if chk_flat% = 0% then goto generate_next

L61040:   gosub dataload
          gosub print_detail
             goto generate_next
        generate_done
            gosub close_printer
        return

        check_for_modle
            if sav_mod$ = mm$ then return
               chk_flat% = 0%
               sav_mod$  = mm$
               init(" ") readkey$
               str(readkey$,1%,9%)   = "PRICE 017"
               str(readkey$,10%,15%) = str(mm$,1,3)
               read #2,key = readkey$, eod goto L61200
                  chk_flat% = 1%
L61200: return

        REM *************************************************************~
            *        S P E C I A L   T A B L E   R O U T I N E S        *~
            *************************************************************

        lookup_tab_1                   /* Load Data for Display Screen */
            on tab_1% gosub p_1, p_2, p_3, p_4, p_5
               return
        p_1                                /* Lookup Pricing Catalogs  */
            tab_hdr$ = "  Master Price Catalog Codes  "
            sav_key$ = "PRICE 000"
            goto L61490
        p_2                                /* Lookup Catalog Methods   */
            tab_hdr$ = "Pricing Calc. Meth for Catalog"
            sav_key$ = "PRICE 001"
            goto L61490
        p_3                                /* Lookup Model/Product     */
            tab_hdr$ = "   APC Model/Product Codes    "
            sav_key$ = "MODEL    "
            goto L61490
        p_4
            tab_hdr$ = " Pricing Type Reference Codes "
            sav_key$ = "PRICE 002"
            goto L61490
        p_5
            tab_hdr$ = "Pricing Cross-Reference Codes "
            sav_key$ = "PRICE 011"

L61490:     i% = 0% : max_code% = 0%
            init(" ") code$(), desc$(), readkey$
            str(readkey$,1%,9%) = sav_key$
            if tab_1% <> 3% then goto L61550
                  str(readkey$,10%,1%) = str(pc_m$,1%,1%)

L61550:     read #2,key > readkey$, using L61570, readkey$, descr$,       ~
                                                         eod goto L61640
L61570:       FMT CH(24), CH(32)
            if sav_key$ <> str(readkey$,1%,9%) then goto L61640
               i% = i% + 1%
               code$(i%) = str(readkey$,10%,15%)
               desc$(i%) = descr$
               if i% = 60% then goto L61640
               goto L61550
L61640:     max_code% = i%
        return

        lookup_tab_2                       /* Get Value from Code Desc */
            p% = 0%
            call "APCPR1SB" (tab_1%, tab_2%, tab_val$, tab_desc$, p%,    ~
                                                            #2, tab_rec%)
        return

        open_flat_file
            call "OPENCHCK" (#3,  fs%(2%), f2%(2%), 100%, rslt$(2%))
            open nodisplay #4, output, space = 100%,                     ~
                 dpack = 100%, ipack = 100%, file = file$,               ~
                 library = library$, volume = volume$, blocks = 5%
        return

        update_flat_file
            for i% = 1% to 20%
                if pc_std%(i%) = 0% then goto L61960
                   init(" ") pc_rf$, pc_pri$, pc_cr$, pc_rf1$, pc_rf2$,  ~
                             pc_rf3$, ecs_rec$, pc_fil$

                   convert i% to pc_rf$, pic(00)

                   convert pc_std%(i%) to pc_pri$,pic(00)

                   pc_cr$  = pc_stdc$(i%)
                   pc_rf1$ = str(pc_stdr$(i%),1%,3%)
                   pc_rf2$ = str(pc_stdr$(i%),4%,3%)
                   pc_rf3$ = str(pc_stdr$(i%),7%,3%)
                   gosub format_ecs
                   gosub write_work_file
L61960:     next i%
            for i% = 1% to 20%
                if pc_spc%(i%) = 0% then goto L62120
                   init(" ") pc_rf$, pc_pri$, pc_cr$, pc_rf1$, pc_rf2$,  ~
                             pc_rf3$, ecs_rec$, pc_fil$

                   convert (i% + 20%) to pc_rf$, pic(00)

                   convert pc_spc%(i%) to pc_pri$,pic(00)

                   pc_cr$  = pc_spcc$(i%)
                   pc_rf1$ = str(pc_spcr$(i%),1%,3%)
                   pc_rf2$ = str(pc_spcr$(i%),4%,3%)
                   pc_rf3$ = str(pc_spcr$(i%),7%,3%)
                   gosub format_ecs
                   gosub write_work_file
L62120:     next i%
/* (AWD006) - Begin */
            for i% = 1% to 20%
                if pc_sub%(i%) = 0% then goto L62130
                   init(" ") pc_rf$, pc_pri$, pc_cr$, pc_rf1$, pc_rf2$,  ~
                             pc_rf3$, ecs_rec$, pc_fil$

                   convert (i% + 40%) to pc_rf$, pic(00)

                   convert pc_sub%(i%) to pc_pri$,pic(00)

                   pc_cr$  = pc_subc$(i%)
                   pc_rf1$ = str(pc_subr$(i%),1%,3%)
                   pc_rf2$ = str(pc_subr$(i%),4%,3%)
                   pc_rf3$ = str(pc_subr$(i%),7%,3%)
                   gosub format_ecs
                   gosub write_work_file
L62130:     next i%
/* (AWD006) - END */
        return

        format_ecs
            init(" ") ecs_rec$, pc_fil$

            str(ecs_rec$,1%,4%)  = pc_c$     /*( 1)PRC CATALOG         */
            str(ecs_rec$,5%,2%)  = pc_cm$    /*( 2)PRC CATALOG METHOD  */
            str(ecs_rec$,7%,16%) = pc_m$     /*( 3)PRC MODEL CODE      */
            str(ecs_rec$,23%,2%) = pc_pri$   /*( 5)PRICE PRI. CALC SEQ */
            str(ecs_rec$,25%,2%) = pc_rf$    /*( 4)STD/SPC REF CODE    */
            str(ecs_rec$,27%,1%) = pc_cr$    /*( 6)PRICE CROSS-REF     */
            str(ecs_rec$,28%,3%) = pc_rf1$   /*( 7)INIT CALC REF (1)   */
            str(ecs_rec$,31%,3%) = pc_rf2$   /*( 8)INIT CALC REF (2)   */
            str(ecs_rec$,34%,3%) = pc_rf3$   /*( 9)INIT CALC REF (3)   */
            str(ecs_rec$,37%,11%)= pc_fil$   /*(10)Filler Area         */
        return

        write_work_file
            cnt% = cnt% + 1%
            if mod(cnt%,25%) <> 0 then goto L62370
               convert cnt% to str(cnt$,19%,5%), pic(#####)

               print at(02,27);hex(84);cnt$;

L62370:     put #3, using L62380, ecs_rec$
L62380:       FMT CH(48)
            write #3, eod goto L62410
        return
L62410:     call "SHOSTAT" ("(Error)-Updating (ECS) Work File?")
            stop "ERR --> " & ecs_rec$
        return

        create_ecscontr
            call "SHOSTAT" ("Creating (ECSCONTR) Control File")

            init(" ") ecs_key$, ecs_rec$
            read #3,key > ecs_key$, using L62540, ecs_rec$,               ~
                                                 eod goto create_done
            goto L62550
        create_next
            read #3, using L62540, ecs_rec$, eod goto create_done
L62540:        FMT CH(48)
L62550:     put #4, using L62540, ecs_rec$
            write #4, eod goto L62610
            goto create_next
        create_done
            call "FILEBGON" (#3)
        return
L62610:     call "SHOSTAT" ("Error-Creating (ECSCONTR) File?")
            call "SHOSTAT" ("ERR--> " & ecs_rec$ )
            stop
            goto create_next

        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
            call "SHOSTAT" ("One Moment Please")

            end

