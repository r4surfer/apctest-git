        REM *************************************************************~
            *                                                           *~
            *                                                           *~
            *  Program Name      - EWDPLN66                             *~
            *  Creation Date     - 02/15/99                             *~
            *  Last Mod Date     - 02/07/06                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Special Glass Creation Utility       *~
            *                      for Re-Makes.                        *~
            *                                                           *~
            *  Code Tables Used  - (PLAN REMK)                          *~
            *                                                           *~
            *                       The reason code record "NN" is used *~
            *                       to assign the next Special Barcode  *~
            *                                                           *~
            *  Data Files - @GEDSPC@ - (GED) Bridge File for Planning   *~
            *             - @GEDSP1@ - (GED) Bridge File for Other      *~
            *             - @BILSPC@ - (BILCO) Bridge File for Planning *~
            *             - @BILSP1@ - (BILCO) Bridge File for Other    *~
            *                                                           *~
            *  Subroutine Used   - (EWDGLSSB) Calculate Glass Cuts      *~
            *                      (EWDPLA66) Create Bridge File (GED)  *~
            *                      (EWDPLB66) Create Bridge File (BILCO)*~
            *                                                           *~
            *  Special Routines  - data_build - Create Glass data using *~
            *                                   APCPLNGR for EWDPLNGR   *~
            *                      print_labels - Read EWDPLNGR         *~
            *                      build_label                          *~
            *                      print_lab    - Write Label data to   *~
            *                                     EWDGLSXX              *~
            *                      lookup_color                         *~
            *                      lookup_grid                          *~
            *                      change_muttin                        *~
            *                                                           *~
            *    Note - sp_qty% > 1% indicates 'Stock Glass' and then   *~
            *           sp_stock$ is set 'S'. Labels will be printed    *~
            *           but no glass will be put into the bridge file.  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/15/99 ! (New) Program                            ! RHH *~
            * 09/15/99 ! (EWD001) Mods to switch labels from the  ! RHH *~
            *          !    UBI printer to the New Zebra printer. !     *~
            * 05/23/05 ! (AWD002) Mod for 3/4" grid               ! CMG *~
            * 01/01/06 ! (PAR000) CR347 Mon for New Sub Part No.  ! RHH *~
            * 02/07/06 ! (PAR001) Mod for Finished Goods Part No. ! RHH *~
            *05/30/2007! (PAR002) Mod for 18mm                    ! DES *~
            *10/29/2019! CR2304  Increase file size ewdglsxx      ! RDB *~
            *          !                                          !     *~
            *************************************************************

        dim                                                              ~
            filename$8,                  /* Used by EWDOPEN            */~
            gl_rec$256,                  /* (EWDGLSXX) Record          */~
            gl_reb$128,                  /* CR2304 size increase       */~
            rm_rec$(2%)192, rm_key$12,   /* Re-Make Record     (PAR000)*/~
            sp_key$31, sp_rec$250,       /* Primary Key and Record     */~
            sp_barcode$9, scr_barcode$9, /* Special Barcode            */~
            sp_num$3, bat$3,             /* Re-Make Number (999)       */~
            sp_process$1,                /* Bridge File Created Y/N    */~
            sp_created$1, sp_created_d$30,/* Created By 0=Plan,1=Other */~
            sp_prod$6, sp_prod_fm$10,    /* Production Date            */~
            sp_sort$2,                   /* Sort Code Not Used         */~
            sp_dept$3, sp_dept_d$30,     /* Special Department Code    */~
            sp_spacer$10,                /* Spacer Size                */~
            sp_model$3,                  /* Product Model Code         */~
            sp_time$8,                   /* Creation Time              */~
            sp_userid$3,                 /* created By                 */~
            sp_cl$1, cl_l$6, cl_s$2,     /* Product Color              */~
            sp_ssq$5,                    /* Special Sequence number    */~
            sp_view$1, view$3,           /* Glass Top/Bot              */~
            sp_t_k$6,                    /* Glass Overall Thickness    */~
            sp_muttin$8,                 /* Glass Muttin Code          */~
            sp_space_d$10,               /* Spacer Description         */~
            sp_sandwich$10,              /* Sandwich                   */~
            sp_width_d$8,                /* Calc Width Decimal         */~
            sp_height_d$8,               /* Calc Height Decimal        */~
            sp_w_adj$6,                  /* Width Adjustment           */~
            sp_h_adj$6,                  /* Height Adjustment          */~
            sp_so$8,                     /* Special Sales order        */~
            sp_part$25, sp_part_d$32,    /* Special MFG Part Number    */~
            sp_wd1$9,                    /* Window Cut Width in Eights */~
            sp_ht1$9,                    /* Window Cut Height in Eights*/~
            sp_wd2$9,                    /* CLMR in Eights             */~
            sp_load$5,                   /* Special Load Number        */~
            sp_cust$9,                   /* Special Customer Code      */~
            sp_vert$55, sp_vert$(10%)7,  /* Custom Vertical Info.      */~
            sp_horz$55, sp_horz$(10%)7,  /* Custom Horizontal Info.    */~
            sp_wd$7,                     /* Actual Window Width in 8's */~
            sp_ht$6,                     /* Actual Window Height 8's   */~
            sp_stock$1,                  /* Stock Flag                 */~
            sp_sub_part$20,              /* New Sub Part Number(PAR000)*/~
            sp_filler$38,                /* Filler Area                */~
                                         /* End of (EWDPLNGR)          */~
            new_part_key$45,             /* New Part Key       (PAR001)*/~
            grid_type$5,                 /* Grid Type          (PAR001)*/~
            grid_size$3,                 /* Grid Size Text     (PAR001)*/~
            grid_color$11,               /* Grid Color         (PAR001)*/~
            sp_qty$3,                    /* Quantity Scheduled         */~
            team$(25%)3,                 /* Team leader Id's           */~
            ged_bilco$1, ged_bilco_d$30, /* Process Selection          */~
            sp_desc$20,                  /* Batch Description          */~
            pl_key$15,                   /* Plan Department (APCPLNDP) */~
            cut$(10%,3%)9,               /* Top/Bot Cuts for Wid/Hgt   */~
            ged$(12%)10,                 /* Intercept Data 1-6=top,7-12=bot*/~
            adj$(10%,2%)6,               /* Window Adj (GED) Top/Bot   */~
            dec$(10%,2%)8,               /* Decimal Cuts Top/Bot       */~
            wd$7,                        /* Actual Window Width Eights */~
            ht$6,                        /* Actual Window Height Eights*/~
            process_cnt$30,              /* Data Not Processed         */~
            contour$1,                   /* Contour Grid               */~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            option$12,                   /* Display                    */~
            cc$(10%)1, c1$(10%)1,        /* Glass Selection, Glass FAC */~
            title$63,                    /* Display                    */~
            dsp_msg$79, sp_msg$79,       /* Display                    */~
            hh$(10%)9,                   /* Display                    */~
            txt$(4%)50, note$(10%)50,    /* Screen '102 Header Text    */~
            err$(15%)20,                 /* Error Messages             */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32                    /* PF Key Hex Values          */

        dim glass_dte$8,                 /* For Bridge File            */~
            sp_lk$1,                     /* For Label Printing         */~
            txt1$40,                     /* Vertical Notches           */~
            txt2$40,                     /* Horizontal Notches         */~
            l_lt$6, r_lt$6,              /*                            */~
            sp_lt$2, mut$8,              /*                            */~
            sp_dte$8, sp_ty$2,           /*                            */~
            sp_s$2, sp_l$30              /* Short and Long Glass Desc  */


        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%)                     /* = 1 if READ was successful */~
                                         /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
                                         /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = " Special Glass Creation Utility "
            pname$ = "EWDPLN66 - PAR: 01.00"

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
            * #2  ! GENCODES ! System Master Code Table Files           *~
            * #3  ! INVMASTR ! Master Inventory File            (PAR001)*~
            * #4  ! APCPLNDP ! Master Planning Dept File                *~
            * #5  ! APCPLNGR ! Master Glass Re_make File                *~
            * #6  ! EWDGLSXX ! Database for all Glass Labels            *~
            * #9  ! EWDPLNGR ! Special Glass Remake File                *~
            * #10 ! @GEDSPC@ ! (GED) Bridge File for Planning           *~
            * #11 ! @GEDSP1@ ! (GED) Bridge File for Other              *~
            * #12 ! @BILSPC@ ! (BILCO) Bridge File for Planning         *~
            * #13 ! @BILSP1@ ! (BILCO) Bridge File for Other            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "AMTBOMCD",                                      ~
                        varc,     indexed,  recsize = 250,               ~
                        keypos = 1,    keylen = 42

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24
                                                            /* (PAR001) */
            select #3,  "INVMASTR",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =  1,   keylen =  45,                     ~
                        alt key  1, keypos  =   142, keylen =  9, dup,   ~
                            key  2, keypos  =   110, keylen =  4, dup,   ~
                            key  3, keypos  =    46, keylen = 32, dup
                                                            /* (PAR001) */
            select #4,  "APCPLNDP",                                      ~
                        varc,     indexed,  recsize =  32,               ~
                        keypos = 11,   keylen = 12,                      ~
                        alt key 1, keypos =  9, keylen = 14,             ~
                            key 2, keypos =  4, keylen = 12,             ~
                            key 3, keypos =  1, keylen = 15
                                                           /* (PAR000)  */
            select #5,  "APCPLNGR",                                      ~
                        varc,     indexed,  recsize =  384,              ~
                        keypos = 22,   keylen =  12,                     ~
                        alt key  1, keypos  =     7, keylen = 27,        ~
                            key  2, keypos  =     1, keylen = 33,        ~
                            key  3, keypos  =    13, keylen = 21
                                                           /* (PAR000)  */
            select #6,  "EWDGLSXX",                                      ~
                        varc,     indexed,  recsize =  384,              ~
                        keypos =  1,   keylen =  32,                     ~
                        alt key  1, keypos  =    33, keylen = 12

            select #9,  "EWDPLNGR",                                      ~
                        varc,     indexed,  recsize = 400,               ~
                        keypos = 1,   keylen = 31,                       ~
                        alt key 1, keypos = 23, keylen = 12

            select #10, "@GEDSPC@", consec, recsize = 220

            select #11, "@GEDSP1@", consec, recsize = 220

            select #12, "@BILSPC@", consec, recsize = 165

            select #13, "@BILSP1@", consec, recsize = 165

            call "SHOSTAT" ("Opening Files, One Moment Please")

                                                      /* (EWD001)     */
            filename$ = "AMTBOMCD" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "INVMASTR" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDP" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNGR" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "EWDGLSXX" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "EWDPLNGR" : call "EWDOPEN" (#9, filename$, err%)
            if err% <> 0% then gosub open_error

        REM    call "OPENCHCK" (#6, fs%(6%), f2%(6%),100%, rslt$(6%))
        REM    call "OPENCHCK" (#9, fs%(9%), f2%(9%),100%, rslt$(9%))
                                                      /* (EWD001)      */
            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", sp_userid$)
            date$ = date
            call "DATEFMT" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."

            u3% = 0%
            err$(1%) = "(1)-Thickness Error"          /* length (20)   */
            err$(2%) = "(2)-Equation Error "
            err$(3%) = "(3)-CLMR Error     "
            err$(4%) = "(4)-lits Error     "
            err$(5%) = "(5)-hinge error    "
            err$(6%) = "(6)-Muttin Error   "
            err$(7%) = "(7)-Sandwich Error "
            err$(8%) = "(8)-Spacer Descript"
            err$(10%)= "No Glass Calculated"
            err$(11%)= "Cant Assign Barcode"
            err$(12%)= "No Up_date-EWDPLNGR"
            err$(15%)= "Major Error - FIX  "

            team$( 1%) = "RHH"
            team$( 2%) = "MET"
            team$( 3%) = "CEP"
            team$( 4%) = "MAY"
            team$( 5%) = "CEB"
            team$( 6%) = "LGM"
            team$( 7%) = "TAC"
            team$( 8%) = "JDF"
            team$( 9%) = "HMB"
            team$(10%) = "SMM"
            team$(11%) = "DA1"
            team$(12%) = "JMM"
            team$(13%) = "MV1"
            team$(14%) = "MLC"
            team$(15%) = "RRS"
            team$(16%) = "JB1"
            team$(17%) = "SB1"
            team$(18%) = "LS2"
            team$(19%) = "MF1"
            team$(20%) = "   "
            team$(21%) = "   "
            team$(22%) = "   "

            t_max% = 19%

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables
            fld_start% = 1%                /* Set Field Start No.      */
        input_loop
            for fieldnr% = fld_start% to   8%
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
                      if keyhit% = 16% and fieldnr% > 3% then exit_program
                                           /* Special Process Data */
                      if keyhit%  = 12% then gosub process_data
                      if keyhit%  = 14% then gosub process_data

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
                  if keyhit%  = 10% then goto create_data
                  if keyhit%  = 12% then gosub process_data
                  if keyhit%  = 14% then gosub process_data
                  if keyhit%  = 16% then goto exit_program
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 3%
            if fieldnr% < 1% or fieldnr% > 8% then editpg1
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

        create_data
            er% = 0%
            gosub calc_glass_size
            if er% <> 0% then goto L19005      /* Calculation Error    */
            if ct% = 0% then goto L19005       /* No Glass             */

            for qq% = 1% to sp_qty%
                for ii% = 1% to 10%
                    if cc$(ii%) <> " " then gosub dataput
                                               /* Only Update Selected */
                    if er% <> 0% then goto L19010  /* Update Error     */
                next ii%
            next qq%

L19005:     init(" ") sp_part$, scr_barcode$, sp_part_d$, sp_vert$,      ~
                      sp_horz$, sp_vert$(), sp_horz$(), sp_sub_part$,    ~
                      sp_dept$, sp_dept_d$, sp_qty$

            fld_start% = 4%
            goto input_loop

L19010:     errormsg$ = err$(15%)
            gosub error_prompt
        return clear all
        goto inputmode

        process_data
                                         /* Check for No Data         */
            if sp_created$ = "0" and process_cnt0% = 0% then goto L19030
            if sp_created$ = "1" and process_cnt1% = 0% then goto L19030

            gosub batch_header
            if keyhit% =  1% then goto L19030
            if keyhit% = 16% then goto L19030
            if keyhit% = 14% then goto L19015
               goto process_data

L19015:        gosub print_labels        /* 1st Create Labels         */

               bat$ = "000"              /* 2nd Create Bridge File    */
               init(" ") glass_dte$
               glass_dte$ = date
               sp_size%   = 999%

                                         /* Check for Bilco Only      */
            if ged_bilco$ = "2" then goto L19020
               call "EWDPLA66" (sp_size%,/* Specified Batch Size      */ ~
                             glass_dte$, /* Glass Production Date     */ ~
                             sp_desc$,   /* Name of Optimized File 0  */ ~
                             bat$,       /* Number of Batches Created */ ~
                             sp_created$,/* Created By 0=Plan, 1=Other*/ ~
                             #2,         /* (GENCODES) Master Tables  */ ~
                             #9,         /* (EWDPLNGR) Special Glass  */ ~
                             #10,        /* (@GEDSPC@) Planning File  */ ~
                             #11,        /* (@GEDSP1@) Other File     */ ~
                             #5 )        /* (APCPLNGR) Master Glass   */

L19020:     if ged_bilco$ = "1" then return   /* (GED) Only           */

               call "EWDPLB66" (sp_size%,/* Specified Batch Size      */ ~
                             glass_dte$, /* Glass Production Date     */ ~
                             sp_desc$,   /* Name of Optimized File 0  */ ~
                             bat$,       /* Number of Batches Created */ ~
                             sp_created$,/* Created By 0=Plan,1=Other */ ~
                             #2,         /* (GENCODES) Master Tables  */ ~
                             #9,         /* (EWDPLNGR) Special Glass  */ ~
                             #12,        /* (@BILSPC@) Planning File  */ ~
                             #13,        /* (@BILSP1@) Other File     */ ~
                             #5 )        /* (APCPLNGR) Master Glass   */

L19030: return clear all
        goto inputmode

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
         "Enter a Process Selection, 0 = Both, 1 = GED, 2 = Bilco?     ",~
         "Entries Entered By? 0 = Planning, 1 = Other?                 ",~
         "Enter Valid Glass Barcode, If applicable?                    ",~
         "Enter a Valid Manfactured Product?                           ",~
         "Enter a Different Department Code or <Return>?               ",~
         "Standard default Quantity is (1), Type new Quantity to Override?",~
         "Enter Custom Vertical Notches? ie. xx.xxxx@xx.xxxx@Etc or 'N/A'?",~
         "Enter Custom Horizontal Notches? ie. xx.xxxx@xx.xxxx@Etc or 'N/A'?"

        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, sp_part$, sp_part_d$,      ~
                      sp_dept$, cut$(), ged$(), adj$(), dec$(), wd$, ht$,~
                      sp_model$, ged_bilco$, ged_bilco_d$,               ~
                      sp_desc$, sp_created$, sp_load$, sp_so$, sp_barcode$,~
                      sp_key$, sp_vert$, sp_horz$, scr_barcode$,         ~
                      sp_vert$(), sp_horz$(), sp_cust$, sp_created_d$,   ~
                      process_cnt$, sp_qty$, sp_dept_d$, sp_sub_part$,   ~
                      new_part_key$
                                                            /* (PAR001) */

            txt$(1%) =                                                   ~
                "**************************************************"
            txt$(2%) =                                                   ~
                "*   (New) Glass Processing and Batch Creation    *"
            txt$(3%) =                                                   ~
                "*       ( E W D ) G l a s s   S y s t e m        *"
            txt$(4%) =                                                   ~
                "**************************************************"


            note$( 1%) =                                                 ~
                "Note: Please note that the 'Batch Name' will be   "
            note$( 2%) =                                                 ~
                "      used and necessary when you are ready to    "
            note$( 3%) =                                                 ~
                "      print the applicable glass labels. This     "
            note$( 4%) =                                                 ~
                "      'Batch Name' needs to be unique and should  "
            note$( 5%) =                                                 ~
                "      be descriptive for the applicable Glass     "
            note$( 6%) =                                                 ~
                "      production run. When print 'Glass Labels'   "
            note$( 7%) =                                                 ~
                "      the 'Production Date' and 'Batch Name' wiil "
            note$( 8%) =                                                 ~
                "      be required.                                "
            note$( 9%) =                                                 ~
                "                                                  "
            note$(10%) =                                                 ~
                "                                                  "

            process_cnt0% = 0%
            process_cnt1% = 0%
            sp_key$ = all(hex(00))
            str(sp_key$,1%,1%) = "N"        /* Check for Not Processed */
L29000:     read #9,key > sp_key$, using L29010, sp_key$,                ~
                                                         eod goto L29020
L29010:        FMT CH(31)
            if str(sp_key$,1%,1%) = "Y" then goto L29020
               if str(sp_key$,2%,1%) = "0" then                          ~
                                       process_cnt0% = process_cnt0% + 1%
               if str(sp_key$,2%,1%) = "1" then                          ~
                                       process_cnt1% = process_cnt1% + 1%
               goto L29000

L29020:  return

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

        data_build
            gosub assign_barcode
            if er% <> 0% then return     /* Error Occurred             */
                                         /* Special Barcode            */
            if str(scr_barcode$,1%,3%) <> "N/A" then goto L30000
                                         /* Use Info from Glass Barcode*/
               sp_ssq$  = "SSSSS"        /* Special Sequence number    */
               sp_so$   = str(sp_barcode$,2%,8%) /* Special Sales order*/
               sp_load$ = "SSSSS"        /* Special Glass Load No.     */
               sp_cust$ = "SSSSSSSSS"    /* Special Customer Code      */

L30000:     sp_num$     = "999"          /* Re-Make Number (999)       */
            sp_process$ = "N"            /* Bridge File Created Y/N    */
            sp_prod$    = date           /* Production Date            */
            sp_sort$    = "00"           /* Sort Code Not Used         */
                                         /* Special Department Code    */
            sp_spacer$  = ged$(3%)       /* Spacer Size                */
                                         /* Product Model Code         */
            sp_time$    = time           /* Creation Time              */
                                         /* created By                 */
            sp_cl$      = str(sp_part$,4%,1%) /* Product Color         */
            sp_view$    = "T"            /* Glass Top/Bot              */
            sp_t_k$     = ged$(1%)       /* Glass Overall Thickness    */
            sp_muttin$  = ged$(5%)       /* Glass Muttin Code          */
            sp_space_d$ = ged$(6%)       /* Spacer Description         */
            sp_sandwich$= ged$(2%)       /* Sandwich                   */
            sp_width_d$ = dec$(ii%,1%)   /* Calc Width Decimal         */
            sp_height_d$= dec$(ii%,2%)   /* Calc Height Decimal        */
            sp_w_adj$   = adj$(ii%,1%)   /* Width Adjustment           */
            sp_h_adj$   = adj$(ii%,2%)   /* Height Adjustment          */
                                         /* Special MFG Part Number    */
            sp_wd1$     = cut$(ii%,1%)   /* Window Cut Width in 16's   */
            sp_ht1$     = cut$(ii%,2%)   /* Window Cut Height in 16's  */
            sp_wd2$     = cut$(ii%,3%)   /* CLMR in 16's               */
                                         /* Special Grid/Liting        */
                                         /* Actual Window Width in 8's */
                                         /* Actual Window Height 8's   */
            sp_filler$  = " "            /* Filler Area                */
            if ii% < 6% then goto L30005
               sp_spacer$   = ged$(9%)   /* Set for Bottom Glass       */
               sp_view$     = "B"
               sp_t_k$      = ged$(7%)
               sp_muttin$   = ged$(11%)
               sp_space_d$  = ged$(12%)
               sp_sandwich$ = ged$(8%)
                                         /* End of (EWDPLNGR)          */
L30005:     if str(sp_vert$,1%,3%) <> "N/A" then sp_muttin$ = "CUSTOM  "
            if str(sp_horz$,1%,3%) <> "N/A" then sp_muttin$ = "CUSTOM  "
        return

        assign_barcode                       /* Assign Special Barcode */
            init(" ") readkey$, desc$
            str(readkey$,1%,10%) = "PLAN REMK"
            str(readkey$,10%,15%)= "NN"
            read #2,hold,key = readkey$, using L30020, desc$,            ~
                                                      eod goto L30030
L30020:        FMT POS(25), CH(30)
            barcode% = 0%
            convert str(desc$,1%,7%) to barcode%, data goto L30030

            convert (barcode% + 1%) to str(desc$,1%,7%), pic(0000000)
            str(sp_barcode$,1%,8%) = "Z" & str(desc$,1%,7%)
            put #2, using L30020, desc$
            rewrite #2
                                             /* (0 - 4)=Top,(5 - 9)=Bot*/
            convert (ii% - 1%) to str(sp_barcode$,9%,1%), pic(0)

        return
L30030:     errormsg$ = "(Error) Unable to Assign Special Barcodes??"
            gosub error_prompt
            er% = 11%
        return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        dataput
            gosub data_build
            if er% <> 0% then return     /*                            */
            sp_stock$ = " "
            if sp_qty% > 1% then sp_stock$ = "S"

            put #9, using L35000,        /* (EWDPLNGR)                 */~
                    sp_process$,         /* Bridge File Created Y/N    */~
                    sp_created$,         /* Created By 0=Plan, 1=Other */~
                    sp_prod$,            /* Production Date            */~
                    sp_sort$,            /* Sort Code Not Used         */~
                    sp_spacer$,          /* Spacer Size                */~
                    sp_dept$,            /* Special Department Code    */~
                    sp_model$,           /* Product Model Code         */~
                    sp_barcode$,         /* Special Barcode            */~
                    sp_num$,             /* Re-Make Number (999)       */~
                    sp_time$,            /* Creation Time              */~
                    sp_userid$,          /* created By                 */~
                    sp_cl$,              /* Product Color              */~
                    sp_ssq$,             /* Special Sequence number    */~
                    sp_view$,            /* Glass Top/Bot              */~
                    sp_t_k$,             /* Glass Overall Thickness    */~
                    sp_muttin$,          /* Glass Muttin Code          */~
                    sp_space_d$,         /* Spacer Description         */~
                    sp_sandwich$,        /* Sandwich                   */~
                    sp_width_d$,         /* Calc Width Decimal         */~
                    sp_height_d$,        /* Calc Height Decimal        */~
                    sp_w_adj$,           /* Width Adjustment           */~
                    sp_h_adj$,           /* Height Adjustment          */~
                    sp_so$,              /* Special Sales order        */~
                    sp_part$,            /* Special MFG Part Number    */~
                    sp_wd1$,             /* Window Cut Width in 16's   */~
                    sp_ht1$,             /* Window Cut Height in 16's  */~
                    sp_wd2$,             /* CLMR in 16's               */~
                    sp_wd$,              /* Actual Window Width in 8's */~
                    sp_ht$,              /* Actual Window Height 8's   */~
                    sp_load$,            /* Production Load No.        */~
                    sp_cust$,            /* Production Customer        */~
                    sp_vert$(),          /* Custom Vertical            */~
                    sp_horz$(),          /* Custom Horizontal          */~
                    sp_stock$,           /* Stock not in Bridge File   */~
                    sp_sub_part$,        /* New Sub_part No.   (PAR000)*/~
                    sp_filler$           /* Filler Area                */
                                         /* End of (EWDPLNGR)          */

            write #9, eod goto L31000
            if sp_created$ = "0" then process_cnt0% = process_cnt0% + 1%
            if sp_created$ = "1" then process_cnt1% = process_cnt1% + 1%
        return
L31000:     errormsg$ = "(Error) - Unable to Update (EWDPLNGR) ???"
            gosub error_prompt
            er% = 12%
        return

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L35000:     FMT     CH(1),               /* Bridge File Created Y/N    */~
                    CH(1),               /* Created By 0=Plan, 1=Other */~
                    CH(6),               /* Production Date            */~
                    CH(2),               /* Sort Code Not Used         */~
                    CH(6),               /* Spacer Size                */~
                    CH(3),               /* Special Department Code    */~
                    CH(3),               /* Product Model Code         */~
                    CH(9),               /* Special Barcode            */~
                    CH(3),               /* Re-Make Number (000)       */~
                    CH(8),               /* Creation Time              */~
                    CH(3),               /* created By                 */~
                    CH(1),               /* Product Color              */~
                    CH(5),               /* Special Sequence number    */~
                    CH(1),               /* Glass Top/Bot              */~
                    CH(6),               /* Glass Overall Thickness    */~
                    CH(8),               /* Glass Muttin Code          */~
                    CH(10),              /* Spacer Description         */~
                    CH(10),              /* Sandwich                   */~
                    CH(8),               /* Calc Width Decimal         */~
                    CH(8),               /* Calc Height Decimal        */~
                    CH(6),               /* Width Adjustment           */~
                    CH(6),               /* Height Adjustment          */~
                    CH(8),               /* Special Sales order        */~
                    CH(25),              /* Special MFG Part Number    */~
                    CH(9),               /* Window Cut Width in 16's   */~
                    CH(9),               /* Window Cut Height in 16's  */~
                    CH(9),               /* CLMR in 16's               */~
                    CH(7),               /* Actual Window Width in 8's */~
                    CH(6),               /* Actual Window Height 8's   */~
                    CH(5),               /* Production Load            */~
                    CH(9),               /* Production Customer        */~
                    10*CH(7),            /* Custom Vertical            */~
                    10*CH(7),            /* Custom Horizontal          */~
                    CH(1),               /* Stock Flag                 */~
                    CH(20),              /* New Sub Part       (PAR000)*/~
                    CH(38)               /* Filler Area                */
                                         /* End of (EWDPLNGR)          */

L35040:    FMT CH(256), CH(128)          /* (EWDGLSXX)                 */


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
              on fieldnr% gosub L40165,          /* GED/BILCO          */~
                                L40165,          /* Created By         */~
                                L40160,          /* Glass Barcode      */~
                                L40160,          /* MFG Part Number    */~
                                L40160,          /* Production Dept    */~
                                L40160,          /* Quantity Scheduled */~
                                L40155,          /* Custom Vertical    */~
                                L40155           /* Custom Horizontal  */
                 goto L40190

L40155:           lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
L40165:           lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */
                                                        /* (PAR001)   */
L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (01,24), fac(hex(a4)), apc$                   , ch(40),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (03,48), fac(hex(84)),   process_cnt$         , ch(30),~
                                                                         ~
               at (04,02), "GED/Bilco,Both   :",                         ~
               at (04,21), fac(lfac$(1%)), ged_bilco$           , ch(01),~
               at (04,48), fac(hex(84)),   ged_bilco_d$         , ch(30),~
                                                                         ~
               at (05,02), "Glass Entered By :",                         ~
               at (05,21), fac(lfac$(2%)), sp_created$          , ch(01),~
               at (05,48), fac(hex(84)),   sp_created_d$        , ch(30),~
                                                                         ~
               at (06,02), "Glass Barcode    :",                         ~
               at (06,21), fac(lfac$(3%)), scr_barcode$         , ch(09),~
                                                                         ~
               at (07,02), "Spec MFG Part No :",                         ~
               at (07,21), fac(lfac$(4%)), sp_part$             , ch(25),~
               at (07,47), fac(lfac$(4%)), sp_sub_part$         , ch(20),~
                                                                         ~
               at (07,69), fac(hex(84)), str(sp_part_d$,1%,11%) , ch(11),~
                                                                         ~
               at (08,02), "Production Dept  :",                         ~
               at (08,21), fac(lfac$(5%)), sp_dept$             , ch(03),~
               at (08,48), fac(hex(84)),   sp_dept_d$           , ch(30),~
                                                                         ~
               at (09,02), "Qty Scheduled    :",                         ~
               at (09,21), fac(lfac$(6%)), sp_qty$              , ch(03),~
                                                                         ~
               at (10,02), "Vertical Notch   :",                         ~
               at (10,21), fac(lfac$(7%)), sp_vert$             , ch(55),~
                                                                         ~
               at (11,02), "Horizontal Notch :",                         ~
               at (11,21), fac(lfac$(8%)), sp_horz$             , ch(55),~
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
            process_cnt$ = "Not Processed: 0 = 000,1 = 000"

            convert process_cnt0% to str(process_cnt$,20%,3%), pic(###)

            convert process_cnt1% to str(process_cnt$,28%,3%), pic(###)

        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1) = "(1)Start Over     (4)Previous Field     " &        ~
                     "(12)Process Plan      (14)Process Other"
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffff0cff0e0f1000)
            if fieldnr% = 1% or fieldnr% > 2% then L40570
                str(pf$(2),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40570:     if fieldnr% > 1% then L40580
                str(pf$(1),19,19) = " "  :  str(pfkeys$, 4,1) = hex(ff)
L40580:     if sp_created$ <> " " then goto L40585
               str(pf$(1%),41%,18%) = " " : str(pfkeys$,12%,1%)=hex(ff)
               str(pf$(1%),62%)     = " " : str(pfkeys$,14%,1%)=hex(ff)
               goto L40600
L40585:     if process_cnt0% <> 0% and sp_created$ = "0" then goto L40590
                str(pf$(1%),41%,18%) = " " : str(pfkeys$,12%,1%)=hex(ff)
L40590:     if process_cnt1% <> 0% and sp_created$ = "1" then goto L40600
                str(pf$(1%),62%)     = " " : str(pfkeys$,14%,1%)=hex(ff)
L40600:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                 (10)Create Data        " &        ~
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
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Optimizer Batch Name(s)                                   *~
            *************************************************************

        batch_header
            gosub set_pf2
L41080:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(32),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
                                                                         ~
               at (03,16), fac(hex(84)), txt$(1%)               , ch(50),~
               at (04,16), fac(hex(84)), txt$(2%)               , ch(50),~
               at (05,16), fac(hex(84)), txt$(3%)               , ch(50),~
               at (06,16), fac(hex(84)), txt$(4%)               , ch(50),~
               at (08,02), "Batch Description :",                        ~
               at (08,25), fac(hex(81)), sp_desc$               , ch(20),~
                                                                         ~
               at (10,16), fac(hex(84)), note$( 1%)             , ch(50),~
               at (11,16), fac(hex(84)), note$( 2%)             , ch(50),~
               at (12,16), fac(hex(84)), note$( 3%)             , ch(50),~
               at (13,16), fac(hex(84)), note$( 4%)             , ch(50),~
               at (14,16), fac(hex(84)), note$( 5%)             , ch(50),~
               at (15,16), fac(hex(84)), note$( 6%)             , ch(50),~
               at (16,16), fac(hex(84)), note$( 7%)             , ch(50),~
               at (17,16), fac(hex(84)), note$( 8%)             , ch(50),~
               at (18,16), fac(hex(84)), note$( 9%)             , ch(50),~
               at (19,16), fac(hex(84)), note$(10%)             , ch(50),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L41310
                  call "PRNTSCRN"
                  goto L41080

L41310:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf2
               txt$(2%) =                                                ~
                    "*  (Optimization) Batch Processing Description   *"
            inpmessage$ = "Enter Description for Optimization Batches"
            pf$(1%) = "(1)Start Over                           " &       ~
                      "                       (14)Process Data"
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)

            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            init(" ") errormsg$
            on fieldnr% gosub L50000,                 /* GED/BILCO    */  ~
                              L50200,                 /* Created By   */  ~
                              L50600,                 /* Glass Barcode*/  ~
                              L50300,                 /* sp_part$     */  ~
                              L50700,                 /* Special Dept */  ~
                              L50800,                 /* Quantity     */  ~
                              L50400,                 /* sp_vert$     */  ~
                              L50500                  /* sp_horz$     */
            return

L50000: REM GED/Bilco                             ged_bilco$
           init(" ") ged_bilco_d$
           if ged_bilco$ <> " " then goto L50010
              ged_bilco$ = "0"                    /* Default to 'GED' */
L50010:    if ged_bilco$ = "0" then ged_bilco_d$ = "Both GED & Bilco  "
           if ged_bilco$ = "1" then ged_bilco_d$ = "GED Glass Machine "
           if ged_bilco$ = "2" then ged_bilco_d$ = "Bilco Glass Machine"
           if len(ged_bilco_d$) < 5 then goto L50020
        return
L50020:    errormsg$= "(Error)-Invalid Glass Machine Selection (0,1,2)?"
           gosub error_prompt
           init(" ") ged_bilco_d$, ged_bilco$
        return

L50200: REM Created By                            sp_created$
        init(" ") sp_created_d$
        if sp_created$ <> " " then goto L50210
           sp_created$ = "1"

L50210: if sp_created$ = "0" then sp_created_d$ = "Planning Department"
        if sp_created$ = "1" then                                      ~
                         sp_created_d$ = "Production/Other Department?"
        if len(sp_created_d$) < 5 then goto L50220
        return
L50220:    errormsg$= "(Error)-Invalid Creation Code? '0' or '1'?"
           gosub error_prompt
           init(" ") sp_created$, sp_created_d$
        return

L50300: REM Manufactured Part                     sp_part$
            init(" ") sp_part_d$, sp_model$, sp_dept$
            new_part_key$ = str(sp_part$,1%,25%)  & sp_sub_part$
            if sp_part$ <> " " then goto L50310
               sp_part_d$  = hex(06) & "Select a Valid Stock Part"
                                                            /* (PAR001)  */
               new_part_key$ = str(sp_part$,1%,25%)  & sp_sub_part$
               call "GETCODE" (#3,new_part_key$, sp_part_d$, 0%, 1.32, f1%(3%))

L50310:     sp_part_d$ = "( Non-Stock Part )"
            read #3,key = new_part_key$, using L50320,sp_part_d$, eod goto L50330
L50320:        FMT POS(46), CH(32)

            sp_part$     = str(new_part_key$,1%,25%)
            sp_sub_part$ = str(new_part_key$,26%,20%)
                                                            /* (PAR001)  */
L50330:     if len(sp_part$) < 19 then goto L50360
               sp_model$ = str(sp_part$,1%,3%)
               if str(sp_model$,1%,1%) = "8" then sp_dept$ = "008"
               if str(sp_model$,1%,1%) = "9" then goto L50360

               init(" ") pl_key$
               str(pl_key$,1%,3%) = sp_model$
L50340:        read #4,key 3% > pl_key$, using L50350, pl_key$,           ~
                                                       eod goto L50360
L50350:           FMT CH(15)
               sp_dept$ = str(pl_key$,11%,3%)
               gosub check_support
               if supp% = 1% then goto L50340
                  gosub L50700                  /* Lookup Department */
        return
L50360:     errormsg$ = "(Error)-Invalid MFG Part Number for Calculation?"
            gosub error_prompt
            init(" ") sp_part$, sp_part_d$, sp_sub_part$
        return

L50400: Rem Special Vertical Data           sp_vert$
            init(" ") sp_vert$()        /* Note always end with ','*/
            p% = len(sp_vert$)
            if p% < 5% then goto L50420 /* Insure Entry Ends with '@'*/
               if str(sp_vert$,p%,1%) <> "@" then                    ~
                                         str(sp_vert$,p%+1%,1%) = "@"
               inc% = 0%                /* Initialize Notch Counter*/
               pp%  = 1%                /* Start at the beginning  */
L50410:        p%   = pos(str(sp_vert$,pp%,) = "@")
               if p% = 0% then return
                  inc% = inc% + 1%      /* Set for Next Value      */
                  sp_vert$(inc%) = str(sp_vert$,pp%,p% - 1%)
                  pp%  = p% + pp%       /* New starting Position   */
                  goto L50410
        return
L50420:     init(" ") sp_vert$          /* No Vertical Notches     */
            sp_vert$ = "N/A"
        return

L50500: Rem Special Horizontal Data         sp_horz$
            init(" ") sp_horz$()        /* Note Always end with ','*/
            p% = len(sp_horz$)
            if p% < 5% then goto L50520 /* Insure Entry Ends with '@'*/
               if str(sp_horz$,p%,1%) <> "@" then                    ~
                                         str(sp_horz$,p%+1%,1%) = "@"
               inc%  = 0%               /* Initialize Notch Counter*/
               pp%   = 1%               /* Start at the beginning  */
L50510:        p%    = pos(str(sp_horz$,pp%) = "@")
               if p% = 0% then return
                  inc% = inc% + 1%      /* Set for Next Value      */
                  sp_horz$(inc%) = str(sp_horz$,pp%,p% - 1%)
                  pp% = p% + pp%        /* New starting Position   */
                  goto L50510
        return
L50520:     init(" ") sp_horz$          /* No Horizontal Values    */
            sp_horz$ = "N/A"
        return

L50600: Rem Glass Barcode                   scr_barcode$   /* (PAR000) */
            init(" ") rm_key$, rm_rec$()
            str(rm_key$,1%,9%) = scr_barcode$
            read #5,key > rm_key$, using L50610, rm_rec$(),eod goto L50620
L50610:        FMT 2*CH(192)
            if str(rm_rec$(),22%,9%) <> scr_barcode$ then goto L50620
               sp_ssq$  = str(rm_rec$(),242%,5%)
               sp_so$   = str(rm_rec$(),163%,8%)
               sp_part$ = str(rm_rec$(),125%,25%)
               sp_load$ = str(rm_rec$(),67%,5%)
               sp_cust$ = "         "
                                                           /* (PAR000) */
               sp_sub_part$ = str(rm_rec$(),255%,20%)
               gosub L50300             /* Lookup MFG Information      */
               fieldnr% = 4%
        return
L50620:     scr_barcode$ = "N/A      "
        return
                                                           /* (PAR000) */
L50700: Rem Special Department                sp_dept$
            init(" ") sp_dept_d$, readkey$
            str(readkey$,1%,9%)   = "PLAN DEPT"
            str(readkey$,10%,15%) = sp_dept$
            read #2,key = readkey$, using L50710, sp_dept_d$,            ~
                                               eod goto L50720
L50710:        FMT POS(25), CH(30)
        return
L50720:    errormsg$ = "(Error) Invalid Department Code?"
           gosub error_prompt
           init(" ") sp_dept$, sp_dept_d$
        return

L50800: Rem Special Quantity                     sp_qty$
            sp_qty% = 1%
            convert sp_qty$ to sp_qty%, data goto L50810

L50810:     convert sp_qty% to sp_qty$, pic(000)

            if sp_qty% = 1% then goto L50820
            for i% = 1% to t_max%
                if sp_userid$ = team$(i%) then goto L50820
            next i%
            goto L50830
L50820: return
L50830:    errormsg$ = "(Error) Invalid Quantity Number?"
           gosub error_prompt
           init(" ") sp_qty$
        return

        check_support
           supp% = 0%
           init(" ") readkey$, desc$
           str(readkey$,1%,9%)   = "PLAN SUPP"
           str(readkey$,10%,15%) = sp_dept$
           read #2,key = readkey$, eod goto check_support_done
           supp% = 1%
        check_support_done
        return


        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        calc_glass_size
            ct% = 0%
            opts% = 0%
            g_cnt% = 0%
            call "EWDGLSSB" (opts%,      /* Options 0% = No Calc       */~
                             sp_part$,   /* MFG Part Number            */~
                             sp_sub_part$, /* Sub part                 */~
                             0,0,0,      /* Dimes1, Dimes2, Dimes3     */~
                             sp_dept$,   /* Department Code            */~
                             ct%,        /* Glass Piece Count          */~
                             cut$(),     /* 1-5 = Top, 6-10 = Bot      */~
                             ged$(),     /* 1-6 = Top, 7-12 = Bot      */~
                             adj$(),     /* Window Adjustment (GED) Top*/~
                             dec$(),     /* Decimal 1-5=Top, 6-10=Bot  */~
                             sp_wd$,     /* Window width Eights        */~
                             sp_ht$,     /* window Height Eights       */~
                             g_cnt%,     /* Glass Piece Cut Count      */~
                             sptype%,    /* SP Type                    */~
                             #2,         /* (GENCODES) Master Tables   */~
                             #1,         /* (AMTBOMCD) Equations       */~
                             er% )       /* 0%=Ok, Non-Zero = Error    */

        gosub display_data

        return


        open_error                                    /* (EWD001)        */
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return
                                                      /* (EWD001)        */

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        display_data
            init(" ") cc$(), c1$()
L60000:     gosub set_display
            accept                                                       ~
               at (01,02), fac(hex(84)), option$                , ch(12),~
                                                                         ~
               at (01,08), fac(hex(a4)), title$                 , ch(63),~
                                                                         ~
               at (02,08), "Grid Type:",                                 ~
               at (02,19), fac(hex(84)), grid_type$             , ch(05),~
                                                                         ~
               at (02,30), "Grid Size:",                                 ~
               at (02,41), fac(hex(84)), grid_size$             , ch(03),~
                                                                         ~
               at (02,53), "Grid Color:",                                ~
               at (02,65), fac(hex(84)), grid_color$            , ch(11),~
                                                                         ~
               at (04,04), fac(hex(a4))  , hh$(1%)              , ch(09),~
               at (04,14), fac(hex(a4))  , hh$(2%)              , ch(09),~
               at (04,24), fac(hex(a4))  , hh$(3%)              , ch(09),~
               at (04,34), fac(hex(a4))  , hh$(4%)              , ch(09),~
               at (04,44), fac(hex(a4))  , hh$(5%)              , ch(06),~
               at (04,51), fac(hex(a4))  , hh$(6%)              , ch(06),~
               at (04,58), fac(hex(a4))  , hh$(7%)              , ch(08),~
               at (04,67), fac(hex(a4))  , hh$(8%)              , ch(08),~
                                                                         ~
               at (05,02), fac(c1$(1%))  , cc$(1%)              , ch(01),~
               at (05,04), fac(hex(84))  , cut$(1%,1%)          , ch(09),~
               at (05,14), fac(hex(84))  , cut$(1%,2%)          , ch(09),~
               at (05,24), fac(hex(84))  , cut$(1%,3%)          , ch(09),~
               at (05,34), fac(hex(84))  , ged$(1%)             , ch(09),~
               at (05,44), fac(hex(84))  , adj$(1%,1%)          , ch(06),~
               at (05,51), fac(hex(84))  , adj$(1%,2%)          , ch(06),~
               at (05,58), fac(hex(84))  , dec$(1%,1%)          , ch(08),~
               at (05,67), fac(hex(84))  , dec$(1%,2%)          , ch(08),~
                                                                         ~
               at (06,02), fac(c1$(2%))  , cc$(2%)              , ch(01),~
               at (06,04), fac(hex(84))  , cut$(2%,1%)          , ch(09),~
               at (06,14), fac(hex(84))  , cut$(2%,2%)          , ch(09),~
               at (06,24), fac(hex(84))  , cut$(2%,3%)          , ch(09),~
               at (06,34), fac(hex(84))  , ged$(2%)             , ch(09),~
               at (06,44), fac(hex(84))  , adj$(2%,1%)          , ch(06),~
               at (06,51), fac(hex(84))  , adj$(2%,2%)          , ch(06),~
               at (06,58), fac(hex(84))  , dec$(2%,1%)          , ch(08),~
               at (06,67), fac(hex(84))  , dec$(2%,2%)          , ch(08),~
                                                                         ~
               at (07,02), fac(c1$(3%))  , cc$(3%)              , ch(01),~
               at (07,04), fac(hex(84))  , cut$(3%,1%)          , ch(09),~
               at (07,14), fac(hex(84))  , cut$(3%,2%)          , ch(09),~
               at (07,24), fac(hex(84))  , cut$(3%,3%)          , ch(09),~
               at (07,34), fac(hex(84))  , ged$(3%)             , ch(09),~
               at (07,44), fac(hex(84))  , adj$(3%,1%)          , ch(06),~
               at (07,51), fac(hex(84))  , adj$(3%,2%)          , ch(06),~
               at (07,58), fac(hex(84))  , dec$(3%,1%)          , ch(08),~
               at (07,67), fac(hex(84))  , dec$(3%,2%)          , ch(08),~
                                                                         ~
               at (08,02), fac(c1$(4%))  , cc$(4%)              , ch(01),~
               at (08,04), fac(hex(84))  , cut$(4%,1%)          , ch(09),~
               at (08,14), fac(hex(84))  , cut$(4%,2%)          , ch(09),~
               at (08,24), fac(hex(84))  , cut$(4%,3%)          , ch(09),~
               at (08,34), fac(hex(84))  , ged$(4%)             , ch(09),~
               at (08,44), fac(hex(84))  , adj$(4%,1%)          , ch(06),~
               at (08,51), fac(hex(84))  , adj$(4%,2%)          , ch(06),~
               at (08,58), fac(hex(84))  , dec$(4%,1%)          , ch(08),~
               at (08,67), fac(hex(84))  , dec$(4%,2%)          , ch(08),~
                                                                         ~
               at (09,02), fac(c1$(5%))  , cc$(5%)              , ch(01),~
               at (09,04), fac(hex(84))  , cut$(5%,1%)          , ch(09),~
               at (09,14), fac(hex(84))  , cut$(5%,2%)          , ch(09),~
               at (09,24), fac(hex(84))  , cut$(5%,3%)          , ch(09),~
               at (09,34), fac(hex(84))  , ged$(5%)             , ch(09),~
               at (09,44), fac(hex(84))  , adj$(5%,1%)          , ch(06),~
               at (09,51), fac(hex(84))  , adj$(5%,2%)          , ch(06),~
               at (09,58), fac(hex(84))  , dec$(5%,1%)          , ch(08),~
               at (09,67), fac(hex(84))  , dec$(5%,2%)          , ch(08),~
                                                                         ~
               at (10,02), fac(c1$(6%))  , cc$(6%)              , ch(01),~
               at (10,04), fac(hex(84))  , cut$(6%,1%)          , ch(09),~
               at (10,14), fac(hex(84))  , cut$(6%,2%)          , ch(09),~
               at (10,24), fac(hex(84))  , cut$(6%,3%)          , ch(09),~
               at (10,34), fac(hex(84))  , ged$(7%)             , ch(09),~
               at (10,44), fac(hex(84))  , adj$(6%,1%)          , ch(06),~
               at (10,51), fac(hex(84))  , adj$(6%,2%)          , ch(06),~
               at (10,58), fac(hex(84))  , dec$(6%,1%)          , ch(08),~
               at (10,67), fac(hex(84))  , dec$(6%,2%)          , ch(08),~
                                                                         ~
               at (11,02), fac(c1$(7%))  , cc$(7%)              , ch(01),~
               at (11,04), fac(hex(84))  , cut$(7%,1%)          , ch(09),~
               at (11,14), fac(hex(84))  , cut$(7%,2%)          , ch(09),~
               at (11,24), fac(hex(84))  , cut$(7%,3%)          , ch(09),~
               at (11,34), fac(hex(84))  , ged$(8%)             , ch(09),~
               at (11,44), fac(hex(84))  , adj$(7%,1%)          , ch(06),~
               at (11,51), fac(hex(84))  , adj$(7%,2%)          , ch(06),~
               at (11,58), fac(hex(84))  , dec$(7%,1%)          , ch(08),~
               at (11,67), fac(hex(84))  , dec$(7%,2%)          , ch(08),~
                                                                         ~
               at (12,02), fac(c1$(8%))  , cc$(8%)              , ch(01),~
               at (12,04), fac(hex(84))  , cut$(8%,1%)          , ch(09),~
               at (12,14), fac(hex(84))  , cut$(8%,2%)          , ch(09),~
               at (12,24), fac(hex(84))  , cut$(8%,3%)          , ch(09),~
               at (12,34), fac(hex(84))  , ged$(9%)             , ch(09),~
               at (12,44), fac(hex(84))  , adj$(8%,1%)          , ch(06),~
               at (12,51), fac(hex(84))  , adj$(8%,2%)          , ch(06),~
               at (12,58), fac(hex(84))  , dec$(8%,1%)          , ch(08),~
               at (12,67), fac(hex(84))  , dec$(8%,2%)          , ch(08),~
                                                                         ~
               at (13,02), fac(c1$(9%))  , cc$(9%)              , ch(01),~
               at (13,04), fac(hex(84))  , cut$(9%,1%)          , ch(09),~
               at (13,14), fac(hex(84))  , cut$(9%,2%)          , ch(09),~
               at (13,24), fac(hex(84))  , cut$(9%,3%)          , ch(09),~
               at (13,34), fac(hex(84))  , ged$(10%)            , ch(09),~
               at (13,44), fac(hex(84))  , adj$(9%,1%)          , ch(06),~
               at (13,51), fac(hex(84))  , adj$(9%,2%)          , ch(06),~
               at (13,58), fac(hex(84))  , dec$(9%,1%)          , ch(08),~
               at (13,67), fac(hex(84))  , dec$(9%,2%)          , ch(08),~
                                                                         ~
               at (14,02), fac(c1$(10%)) , cc$(10%)             , ch(01),~
               at (14,04), fac(hex(84))  , cut$(10%,1%)         , ch(09),~
               at (14,14), fac(hex(84))  , cut$(10%,2%)         , ch(09),~
               at (14,24), fac(hex(84))  , cut$(10%,3%)         , ch(09),~
               at (14,34), fac(hex(84))  , ged$(11%)            , ch(09),~
               at (14,44), fac(hex(84))  , adj$(10%,1%)         , ch(06),~
               at (14,51), fac(hex(84))  , adj$(10%,2%)         , ch(06),~
               at (14,58), fac(hex(84))  , dec$(10%,1%)         , ch(08),~
               at (14,67), fac(hex(84))  , dec$(10%,2%)         , ch(08),~
                                                                         ~
               at (18,02), fac(hex(84))  , sp_msg$              , ch(79),~
                                                                         ~
               at (21,02), fac(hex(a4)),   dsp_msg$             , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <> 15 then goto L60010
                  call "PRNTSCRN"
                  goto L60000

L60010:        if keyhit% <> 0% then goto L60000

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_display
            dsp_msg$ = "Place an 'X' next to Each Pannel to be made?"
            str(dsp_msg$,46%,28%) = "Then Press <Return> to Cont."

            sp_msg$ = "Normal Glass Calc   -Glass Count [ xx ] ???"
            convert g_cnt% to str(sp_msg$,36%,2%), pic(##)

            if g_cnt% = 0% then er% = 10%

            if er% <> 0% then sp_msg$ = "Error (No Glass) " & err$(er%) & ~
                                                     "            "
                                                       /* (PAR001)    */
            option$ = "Dept.  [xxx]"
            str(option$,9%,3%) = sp_dept$
            title$  = "(Glass) for < "
            str(title$,15%,25%) = sp_part$
            str(title$,41%,20%) = sp_sub_part$
            str(title$,62%,1%) = ">"

            grid_type$ = "     "
            if str(sp_sub_part$,1%,1%) = "0" then grid_type$ = " N/A "
            if str(sp_sub_part$,1%,1%) = "1" then grid_type$ = "Flat "
            if str(sp_sub_part$,1%,1%) = "2" then grid_type$ = "Cont "
            if str(sp_sub_part$,1%,1%) = "3" then grid_type$ = "Brass"

            grid_size$ = "   "
            if str(sp_sub_part$,2%,1%) = "4" then grid_size$ = "3/8"
            if str(sp_sub_part$,2%,1%) = "1" then grid_size$ = "5/8"
            if str(sp_sub_part$,2%,1%) = "2" then grid_size$ = "3/4"
            if str(sp_sub_part$,2%,1%) = "3" then grid_size$ = " 1 "
            if str(sp_sub_part$,2%,1%) = "0" then grid_size$ = "N/A"

            grid_color$ = "           "
            if str(sp_sub_part$,3%,1%) = "0" then grid_color$ = " N/A       "
            if str(sp_sub_part$,3%,1%) = "1" then grid_color$ = "White      "
            if str(sp_sub_part$,3%,1%) = "2" then grid_color$ = "Nat Oak Wh "
            if str(sp_sub_part$,3%,1%) = "3" then grid_color$ = "Hill Oak Wh"
            if str(sp_sub_part$,3%,1%) = "4" then grid_color$ = "Brass      "

                                                       /* (PAR001)    */
            hh$(1%) = "Width    "
            hh$(2%) = "Height   "
            hh$(3%) = "CLMR     "
            hh$(4%) = " G E D   "
            hh$(5%) = "AdjWid"
            hh$(6%) = "AdjHgt"
            hh$(7%) = "Dec  Wid"
            hh$(8%) = "Dec  Hgt"

            pf$(1) = "                                        " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                                       "
            pfkeys$ = hex(ffffffffffffffffffffffffffff0f1000)

            for i% = 1% to 10%
                c1$(i%) = hex(84)
                if len(cut$(i%,1%)) > 2 then c1$(i%) = hex(81)
            next i%

            return

        print_labels
            init(" ") sp_prod_fm$, sp_key$
            sp_prod_fm$ = date
            call "DATFMTC" (sp_prod_fm$)

            sp_dte$ = "xx/xx/xx"
            str(sp_dte$,1%,2%) = str(sp_prod_fm$,1%,2%)
            str(sp_dte$,4%,2%) = str(sp_prod_fm$,4%,2%)
            str(sp_dte$,7%,2%) = str(sp_prod_fm$,9%,2%)

            call "SHOSTAT" ("Printing Special Glass Labels")
            sp_key$ = all(hex(00))
            gl_sort% = 0%

            str(sp_key$,1%,1%) = "N"            /* Only Not Processed */
            str(sp_key$,2%,1%) = sp_created$    /* Created By         */
        print_labels_nxt
            init(" ") sp_rec$, sp_vert$(), sp_horz$(), sp_filler$,     ~
                      txt1$, txt2$
                                                /* (PAR000)           */
            read #9,key > sp_key$, using L61000, sp_rec$, sp_vert$(),  ~
                             sp_horz$(), sp_stock$, sp_sub_part$,      ~
                                        sp_filler$, eod goto print_done
L61000:        FMT CH(201), 10*CH(7), 10*CH(7), CH(1), CH(20), CH(38)
                                                /* (PAR000)           */
            sp_key$ = str(sp_rec$,1%,31%)
            if str(sp_key$,1%,1%) <> "N" then print_done
            if str(sp_key$,2%,1%) <> sp_created$ then goto print_done

               sp_cl$ = str(sp_rec$,46%,1%)
               gosub lookup_color
               view$ = "TOP"
               if str(sp_rec$,52%,1%) = "B" then view$ = "BOT"
               sp_so$   = str(sp_rec$,115%,8%)
               sp_wd1$  = str(sp_rec$,148%,9%)
               sp_ht1$  = str(sp_rec$,157%,9%)
               sp_dept$ = str(sp_rec$,17%,3%)
               sp_ssq$  = str(sp_rec$,47%,5%)

               sp_model$= str(sp_rec$,20%,3%)
               sp_wd$   = str(sp_rec$,175%,7%)
               sp_ht$   = str(sp_rec$,182%,6%)
               sp_part$ = str(sp_rec$,123%,25%)
               sp_lk$   = str(sp_part$,12%,1%)
               sp_lt$   = str(sp_part$,7%,2%)
                                                      /* (PAR000)   */
               contour$ = " "
               if str(sp_sub_part$,1%,1%) = "2" then contour$ = "C"
                                                      /* (AWD002)   */
                                                      /* (PAR000)   */
               if str(sp_sub_part$,2%,1%) = "2" then contour$ = "W"

               /* <PAR002>   */
               if str(sp_sub_part$,1%,2%) = "21" then contour$ = "E"
               /* </PAR002>   */

               for kk% = 1% to 5%
                   if len(sp_vert$(kk%)) < 4 then goto L61010
                      k1% = len(sp_vert$(kk%))
                      txt1$ = txt1$ & str(sp_vert$(kk%),1%,k1%) & "-"

L61010:            if len(sp_horz$(kk%)) < 4 then goto L61020
                      k1% = len(sp_horz$(kk%))
                      txt2$ = txt2$ & str(sp_horz$(kk%),1%,k1%) & "-"

L61020:        next kk%

               sp_barcode$ = str(sp_rec$,23%,9%)
               sp_ty$   = str(sp_part$,5%,2%)
               gosub lookup_glass
               gosub lookup_grid
               gosub change_muttin
               gosub build_label
               gosub print_lab
               goto print_labels_nxt
        print_done
        return

        print_lab
           put #6,using L35040, gl_rec$, gl_reb$

           write #6, eod goto L62000
        return
L62000:    errormsg$ = "(Error) Printing Label - " & sp_barcode$
           gosub error_prompt
        return

        build_label
            gl_sort% = gl_sort% + 1%
            init(" ") gl_rec$
            str(gl_rec$,1%,1%) = "4"       /* Special Glass            */
            str(gl_rec$,2%,6%) = date      /* Production Date (Today)  */
            str(gl_rec$,8%,20%)= sp_desc$  /* Batch Description        */
            convert gl_sort% to str(gl_rec$,28%,5%), pic(00000)
                                           /* Glass Sort Code          */
            str(gl_rec$,33%,9%)= sp_barcode$
                                           /* Glass Barcode            */
            str(gl_rec$,42%,3%)= "999"     /* Re-Make Number           */
            str(gl_rec$,45%,2%)= sp_s$     /* Glass Type Code          */
            str(gl_rec$,47%,7%)= mut$      /* Grid/Liting Description  */
            str(gl_rec$,54%,8%)= sp_so$    /* S. O. Number             */

            str(gl_rec$,62%,10%)=sp_wd1$ & " "
                                           /* Calculated width         */
            str(gl_rec$,72%,3%)= sp_model$ /* Model Code               */
            str(gl_rec$,75%,10%)=sp_ht1$ & "  "
                                           /* Calculated Height        */
            str(gl_rec$,85%,2%)   = "01"   /* Set to 1st Shift         */
            str(gl_rec$,87%,5%)   = sp_ssq$/* Dept Seq. Number         */
            str(gl_rec$,92%,7%)   = sp_wd$ /* Actual window Width      */
                                           /* Color Code & Lock Code   */
            str(gl_rec$,99%,2%)   = sp_cl$ & sp_lk$        /* (PAR000) */

            str(gl_rec$,101%,6%)  = sp_ht$ /* Actual Window Height     */
                                           /* Actual window Width/Height*/
            str(gl_rec$,107%,3%) = sp_dept$/* Department Code          */
            str(gl_rec$,110%,40%) = txt1$  /* Vertical Text            */
            str(gl_rec$,150%,40%) = txt2$  /* Horizontal Text          */
            str(gl_rec$,190%,3%)  = view$  /* View Top/Bot             */
            str(gl_rec$,193%,6%)  = cl_l$  /* Window Color             */
            str(gl_rec$,199%,1%)  =contour$/* Contour Grid             */
            str(gl_rec$,200%,3%)  = "999"  /* Print re-Make Number     */
            STR(gl_rec$,203%,1%)  = sp_stock$ /* Stock Glass Flag      */
            str(gl_rec$,204%,53%) = " "    /* Filler Area              */

                                           /* Record Length = 256 ??   */
        return

        change_muttin
            mut$ = " "                         /* SWITCH VERT - HORZ */
            muttin$ = str(sp_rec$,59%,8%)      /*            TO      */
            if str(muttin$,1%,6%) = "CUSTOM" then goto L63010
            if len(muttin$) < 5 then goto L63000
               xx% = pos(muttin$ = "x")
               cc% = pos(muttin$ = "C")
               ii% = (cc% - xx%)
            mut$ = str(muttin$,xx%+1%,ii%) & "x" & str(muttin$,1%,xx%-1%)
L63000:     if lt% > 82% then mut$ = l_lt$
        return
L63010:     mut$ = "CUSTOM"
        return

        lookup_color                                  /* Look Up Color */
            init(" ") desc$, readkey$, cl_l$, cl_s$
            str(readkey$,1%,9%)   = "COLOR    "
            str(readkey$,10%,15%) = sp_cl$
            read #2,key = readkey$,using L64000, desc$,eod goto L64010
L64000:        FMT POS(25), CH(30)
            cl_l$ = str(desc$,6%,6%)                 /* Long Descript */
            cl_s$ = str(desc$,1%,2%)                 /* Short Descript*/
L64010: return

        lookup_grid                                   /* Look Up Grid  */
            lt% = 0%
            init(" ") desc$, readkey$, l_lt$, r_lt$
            str(readkey$,1%,9%)   = "LITING   "
            str(readkey$,10%,15%) = sp_lt$
            read #2,key = readkey$,using L64000, desc$,eod goto L64020
            p% = pos(desc$ = "-")
            if p% = 0% then p% = 4%
            l_lt$ = str(desc$,1%,p%-2%) & "   "  /* LEFT MUTTIN - TOP */
            r_lt$ = str(desc$,p%+2%,6%) & "   "  /* RIGHT MUTTIN - BOT*/
            convert sp_lt$ to lt%,data goto L64020

L64020: return

        lookup_glass                                 /* Look Up GLASS  */
            init(" ") readkey$, desc$, sp_s$, sp_l$
            str(readkey$,1%,9%)   = "GLASS    "
            str(readkey$,10%,15%) = sp_ty$
            read #2,key = readkey$, using L64000, desc$, eod goto L64030
            p% = pos(desc$ = "-")
            if p% = 0% then p% = 4%
            sp_l$ = desc$                             /* Long Descript */
            sp_s$ = str(desc$,1%,p%-2%)               /* Short Descript*/
L64030: return

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program

            end

