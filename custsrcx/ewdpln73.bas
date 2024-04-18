        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDPLN73                             *~
            *  Creation Date     - 11/08/99                             *~
            *  Last Modified Date- 08/12/02                             *~
            *  Written By        - Roy H. Hoffman                       *~
            *  Modifications By  - Roy H. Hoffman                       *~ 
            *                                                           *~
            *  Description       - New Production Lowe's labels.        *~
            *                                                           *~
            *  Code Tables Used  - PLAN DEPT, PLANLABEL                 *~
            *                                                           *~
            *  Subroutine Used   - EWDPLA73 (Lowe's Labels Format (A) ) *~
            *                    - EWDPLB73 (Lowe's Labels Format (B) ) *~
            *                    - EWDPLC73 (Lowe's Labels Format (C) ) *~
            *                                                           *~
            *  Special Comments  -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/08/99 ! (New) Program - Copied & EWDPLN71        ! RHH *~
            * 04/12/00 ! (EWD001) - Mod to put all labels in a    ! RHH *~
            *          !    Single File.                          !     *~
            * 05/12/00 ! (EWD001) - Sku number changed to six     ! RHH *~
            *          !    Digits                                !     *~
            * 08/12/02 ! (EWD002) - Put UPC Code on labels = SKU  ! RHH *~   
            *************************************************************

        dim                                                              ~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
            count$22,                    /* Display                    */~ 
            sc_dept$3, sc_dept_d$30,     /* Department Code & Descr    */~
            sc_prddate$10,               /* Production Date            */~
            sc_prddte$10,                /* Prod Date - Unformatted    */~
            sc_load$5,                   /* Load Number                */~
            sc_color$2, sc_color_d$30,   /* Color Code                 */~
            sc_format$1,                 /* Label Format Code          */~ 
            sc_seq_fr$5,                 /* Beginning Sequence No.     */~
            sc_seq_to$5,                 /* Ending Sequence No.        */~
            sc_sku$6,                    /* Sku Number                 */~
            sc_qty$3,                    /* Label Quantity             */~       
            sp_key$41,                   /* Special Labels Key         */~
            sp_rec$102,                  /* (EWDLABSP) Record          */~
            sp_dept$3, save_dept$3,      /* Department Code            */~
            sp_seq$5,                    /* Prod Seq. Number           */~
            sp_sku$6,                    /* Product SKU No.            */~
            sp_load$5,                   /* Load Number                */~
            sp_part$25,                  /* EWD Part Number - Exact    */~
            sp_upc$11,                   /* (EWD002) UPC Code          */~
            o_part$25,                   /* EWD Part Number - Opening  */~
            sp_mon$2,                    /* Production Month           */~
            sp_day$2,                    /* Production Day             */~
            nominal$7,                   /* Nominal Size               */~
            p_wd$3, p_ht$3,              /* Nominal width and Height   */~
            p_wd1$1, p_wd2$2,            /* Width Feet and Inches      */~
            p_ht1$1, p_ht2$2,            /* Height Feet and Inches     */~
            wd$2, ht$2,                  /* Width and Height Tot Inches*/~
            cuscode$9,                   /* Customer Code              */~
            filename$8,                  /* Used by EWDOPEN            */~ 
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            file$8,                      /* Lbl Print File             */~
            library$8,                   /* Library Name = APCDATA     */~
            script$8,                    /* Lbl SHELL SCRIPT           */~
            volume$6,                    /* DISK VOLUME = CARLOS       */~
            time$8,                      /* System time                */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(5%),                     /* = 0 if the file is open    */~
            fs%(8%),                     /* = 1 if file open, -1 if it */~
            f1%(5%),                     /* = 1 if READ was successful */~ 
            axd$4,                       /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(5%)20                  /* Text from file opening     */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21 
            apc$   = "(EWD) Generate Lowe's Special Labels  "
            pname$ = "EWDPLN73 - Rev: R7.00"

            mat f1% = zer
            mat f2% = zer

        REM *************************************************************


                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! GENCODES ! System Master Code Table Files           *~
            * #2  ! EWDLABSP ! Special Lowes Labels                     *~
            * #3  ! APCPCMST ! Pricing Master File                      *~
            * #5  ! MFGLOWES ! Lowes Label File                         *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

                                                      
            select #2, "EWDLABSP",                                       ~
                        varc,     indexed,  recsize =  102,              ~
                        keypos =    1, keylen =   41,                    ~
                        alt key  1, keypos = 42, keylen =  41 
                                                   

            select #3,   "APCPCMST",                                     ~
                        varc,     indexed,  recsize = 102,               ~
                        keypos =    9, keylen =   40,                    ~
                        alt key  1, keypos =    1, keylen =  8

            select #5, "MFGLOWES", varc, consec, recsize =   72


            call "SHOSTAT" ("Opening Files, One Moment Please")
                                                    /* (EWD004)         */

            call "OPENCHCK" (#2, fs%(2%), f2%(2%), 500%, rslt$(2%))
            filename$ = "GENCODES" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
REM            filename$ = "EWDLABSP"  call "EWDOPEN" (#2, filename$, err%)
REM            if err% <> 0% then gosub open_error
            filename$ = "APCPCMST" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error

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
            call "TIME" (time$)

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   7%
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
                  if keyhit%  = 16% then       dataload
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 2%
            if fieldnr% < 1% or fieldnr% > 7% then editpg1
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
            *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
            *-----------------------------------------------------------*~
            * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
            *************************************************************

        deffn'051(fieldnr%)
            enabled% = 1%
        return

        REM *************************************************************~
            *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
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
         "Enter a Production Date?                                     ",~
         "Enter a Specific Color Code (Required)?                      ",~
         "Enter a Specific Department Code or 'ALL'?                   ",~
         "Enter a Load Number or 'ALL'?                                ",~
         "Enter a Production Sequence Range or 'ALL'?                  ",~
         "Enter a Single SKU Number or 'ALL'?                          ",~
         "Enter a Quantity Value when SKU Number Not = 'ALL'?          "  

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
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************
        initialize_variables
            init(" ") errormsg$, inpmessage$, sc_dept$, sc_dept_d$,      ~
                      sc_prddate$, sc_prddte$, sc_load$, sc_seq_to$,     ~
                      sc_seq_fr$, sc_color$, sc_color_d$, sc_format$,    ~
                      sc_sku$, sc_qty$, save_dept$, sp_dept$, sp_upc$
                                                 /* (EWD002)            */         
           lbl% = 0%

        return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload                                         /* (EWD001)   */
            call "SHOSTAT" ("Printing Lowe's Special Labels...")
            count$ = "Labels Printed (xxxxx)"
            gosub set_file_name                /* Create and Open File */
            
            been_here_a% = 0%
            been_here_b% = 0%
            been_here_c% = 0%

            sp_key$ = all(hex(00))
            str(sp_key$,1%,6%) = str(sc_prddte$,1%,6%)
            str(sp_key$,7%,2%) = sc_color$
          load_next_rec
            read #2, key > sp_key$, using L35040, sp_rec$,               ~
                                                       eod goto load_done
            sp_key$ = str(sp_rec$,1%,41%)
                                                         /* Check Date */
            if str(sp_key$,1%,6%) <> str(sc_prddte$,1%,6%) then          ~
                                                       goto load_done
                                                         /* Check Color*/  
            if str(sp_key$,7%,2%) <> sc_color$ then goto load_done
                                                       /* Check Dept   */
            if sc_dept$ = "ALL" then goto L30005
               if str(sp_key$,9%,3%) <> sc_dept$ then goto load_next_rec
                                                       /* Check Load   */
L30005:     if str(sc_load$,1%,3%) = "ALL" then goto L30010
               if sc_load$ <> str(sp_rec$,83%,5%) then goto load_next_rec
                                                      /* check Seq No. */
L30010:     if str(sc_seq_fr$,1%,3%) = "ALL" then goto L30020
               if str(sp_rec$,12%,5%) < sc_seq_fr$ or                    ~
                  str(sp_rec$,12%,5%) > sc_seq_to$ then goto load_next_rec 
                                                      /* Check SKU No  */
L30020:     
            if str(sc_sku$,1%,3%) = "ALL" then goto L30030
               if str(sp_rec$,17%,6%) <> sc_sku$ then goto load_next_rec

                                                      /* Print Labels  */
L30030:     /* Format Label to Print */

            sp_dept$ = str(sp_rec$,9%,3%)
            sp_seq$  = str(sp_rec$,12%,5%)
            sp_sku$  = str(sp_rec$,17%,6%)            /* (EWD002)     */
            sp_load$ = str(sp_rec$,83%,5%)
            sp_part$ = str(sp_rec$,58%,25%)
            sp_upc$  = str(sp_rec$,88%,11%)           /* (EWD002)     */
                                                      /* UPC Code     */ 
            sp_mon$  = str(sc_prddate$,1%,2%)
            sp_day$  = str(sc_prddate$,4%,2%)
            gosub check_nominal

            if save_dept$ <> sp_dept$ then gosub print_header
               save_dept$ = sp_dept$
 
 
            for i% = 1% to sc_qty%
                type% = 0%
                if sc_format$ = "A" then                                 ~
                 call "EWDPLA73" (type%, /* 0%=Label, 1%=Header        */~
                        sp_dept$,        /* Department Code            */~
                        sp_seq$,         /* Production Sequence Number */~
                        sp_sku$,         /* SKU Number        (EWD001) */~
                        sp_load$,        /* Load Number                */~
                        sp_part$,        /* MFG Part Number            */~
                        sp_mon$,         /* Production Month           */~
                        sp_day$,         /* Production Day             */~
                        p_wd1$,          /* Nom Width in Feet          */~
                        p_wd2$,          /* Nom Width in Inches        */~
                        p_ht1$,          /* Nom Height in Feet         */~
                        p_ht2$,          /* Nom Height in Inches       */~ 
                        wd$,             /* Opening Width  - Inches    */~
                        ht$,             /* Opening Height - Inches    */~
                        sp_upc$,         /* (EWD002) UPC Lowe's        */~      
                        been_here_a%,    /* Zero (Only 1st Time)       */~
                        #5,              /* (MFGLOWES)                 */~
                        err%)            /* Error Code 0% = Ok         */

                if sc_format$ = "B" then                                 ~
                 call "EWDPLB73" (type%, /* 0%=Label, 1%=Header        */~
                        sp_dept$,        /* Department Code            */~
                        sp_seq$,         /* Production Sequence Number */~
                        sp_sku$,         /* SKU Number        (EWD001) */~
                        sp_load$,        /* Load Number                */~
                        sp_part$,        /* MFG Part Number            */~
                        sp_mon$,         /* Production Month           */~
                        sp_day$,         /* Production Day             */~
                        p_wd1$,          /* Nom Width in Feet          */~
                        p_wd2$,          /* Nom Width in Inches        */~
                        p_ht1$,          /* Nom Height in Feet         */~
                        p_ht2$,          /* Nom Height in Inches       */~ 
                        wd$,             /* Opening Width  - Inches    */~
                        ht$,             /* Opening Height - Inches    */~
                        sp_upc$,         /* (EWD002) UPC Lowe's        */~      
                        been_here_b%,    /* Zero (Only 1st Time)       */~
                        #5,              /* (MFGLOWES)                 */~
                        err%)            /* Error Code 0% = Ok         */

                if sc_format$ = "C" then                                 ~
                 call "EWDPLC73" (type%, /* 0%=Label, 1%=Header        */~
                        sp_dept$,        /* Department Code            */~
                        sp_seq$,         /* Production Sequence Number */~
                        sp_sku$,         /* SKU Number        (EWD001) */~
                        sp_load$,        /* Load Number                */~
                        sp_part$,        /* MFG Part Number            */~
                        sp_mon$,         /* Production Month           */~
                        sp_day$,         /* Production Day             */~
                        p_wd1$,          /* Nom Width in Feet          */~
                        p_wd2$,          /* Nom Width in Inches        */~
                        p_ht1$,          /* Nom Height in Feet         */~
                        p_ht2$,          /* Nom Height in Inches       */~ 
                        wd$,             /* Opening Width  - Inches    */~
                        ht$,             /* Opening Height - Inches    */~
                        sp_upc$,         /* (EWD002) UPC Lowe's        */~      
                        been_here_c%,    /* Zero (Only 1st Time)       */~
                        #5,              /* (MFGLOWES)                 */~
                        err%)            /* Error Code 0% = Ok         */

            next i%

            if err% <> 0% then gosub print_error
            lbl% = lbl% + sc_qty%
            if mod(lbl%,25%) <> 0 then goto L30040
               convert lbl% to str(count$,17%,5%), pic(#####)

               call "SHOSTAT" (count$)
L30040:     if sc_qty% > 1% then goto load_done  /* Single Sku Print */

            goto load_next_rec 

        load_done
            gosub close_mfglowes

            gosub load_results
            goto inputmode

        check_nominal
            init(" ") nominal$, p_wd$, p_ht$
            p_err% = 0% 
            cuscode$ = "LO0013"
            call "EWDNOMSZ" ("E", sp_part$, o_part$, nominal$, cuscode$,  ~
                                                      #3, #1, p_err%)

            p_wd$ = str(nominal$,1%,3%)
            p_ht$ = str(nominal$,5%,3%)
            call "SPCESMSH" (p_wd$, 0%)
            call "SPCESMSH" (p_ht$, 0%)

            p_wd1$ = str(p_wd$,1%,1%)               /* Width Feet      */
            p_wd2$ = str(p_wd$,2%,2%)               /* Width Inches    */

            p_ht1$ = str(p_ht$,1%,1%)               /* Height Feet     */
            p_ht2$ = str(p_ht$,2%,2%)               /* Height Inches   */
            p_wd2% = 0% : p_ht2% = 0%

            convert p_wd2$ to p_wd2%, data goto L30050
L30050:
            convert p_wd2% to p_wd2$, pic(##)

            convert p_ht2$ to p_ht2%, data goto L30060
L30060:
            convert p_ht2% to p_ht2$, pic(##)

            p_wd1%, p_ht1% = 0%
            convert p_wd1$ to p_wd1%, data goto L30100
L30100:
            convert p_ht1$ to p_ht1%, data goto L30120
L30120:
            convert ((p_wd1%*12%) + p_wd2%) to wd$, pic(##)

            convert ((p_ht1%*12%) + p_ht2%) to ht$, pic(##)

        return
 
        print_header
            if sc_qty% > 1% then return  /* No Header for Single's     */

                type% = 1%
                if sc_format$ = "A" then                                 ~
                 call "EWDPLA73" (type%, /* 0%=Label, 1%=Header        */~
                        sp_dept$,        /* Department Code            */~
                        sp_seq$,         /* Production Sequence Number */~
                        sp_sku$,         /* SKU Number        (EWD001) */~
                        sp_load$,        /* Load Number                */~
                        sp_part$,        /* MFG Part Number            */~
                        sp_mon$,         /* Production Month           */~
                        sp_day$,         /* Production Day             */~
                        p_wd1$,          /* Nom Width in Feet          */~
                        p_wd2$,          /* Nom Width in Inches        */~
                        p_ht1$,          /* Nom Height in Feet         */~
                        p_ht2$,          /* Nom Height in Inches       */~ 
                        wd$,             /* Opening Width  - Inches    */~
                        ht$,             /* Opening Height - Inches    */~
                        sp_upc$,         /* (EWD002) UPC Lowe's        */~      
                        been_here_a%,    /* Zero (Only 1st time)       */~
                        #5,              /* (MFGLOWES)                 */~ 
                        err%)            /* Error Code 0% = Ok         */

                if sc_format$ = "B" then                                 ~
                 call "EWDPLB73" (type%, /* 0%=Label, 1%=Header        */~
                        sp_dept$,        /* Department Code            */~
                        sp_seq$,         /* Production Sequence Number */~
                        sp_sku$,         /* SKU Number        (EWD001) */~
                        sp_load$,        /* Load Number                */~
                        sp_part$,        /* MFG Part Number            */~
                        sp_mon$,         /* Production Month           */~
                        sp_day$,         /* Production Day             */~
                        p_wd1$,          /* Nom Width in Feet          */~
                        p_wd2$,          /* Nom Width in Inches        */~
                        p_ht1$,          /* Nom Height in Feet         */~
                        p_ht2$,          /* Nom Height in Inches       */~ 
                        wd$,             /* Opening Width  - Inches    */~
                        ht$,             /* Opening Height - Inches    */~
                        sp_upc$,         /* (EWD002) UPC Lowe's        */~      
                        been_here_b%,    /* Zero (Only 1st time)       */~
                        #5,              /* (MFGLOWES)                 */~ 
                        err%)            /* Error Code 0% = Ok         */

                if sc_format$ = "C" then                                 ~
                 call "EWDPLC73" (type%, /* 0%=Label, 1%=Header        */~
                        sp_dept$,        /* Department Code            */~
                        sp_seq$,         /* Production Sequence Number */~
                        sp_sku$,         /* SKU Number        (EWD001) */~
                        sp_load$,        /* Load Number                */~
                        sp_part$,        /* MFG Part Number            */~
                        sp_mon$,         /* Production Month           */~
                        sp_day$,         /* Production Day             */~
                        p_wd1$,          /* Nom Width in Feet          */~
                        p_wd2$,          /* Nom Width in Inches        */~
                        p_ht1$,          /* Nom Height in Feet         */~
                        p_ht2$,          /* Nom Height in Inches       */~ 
                        wd$,             /* Opening Width  - Inches    */~
                        ht$,             /* Opening Height - Inches    */~
                        sp_upc$,         /* (EWD002) UPC Lowe's        */~      
                        been_here_c%,    /* Zero (Only 1st time)       */~
                        #5,              /* (MFGLOWES)                 */~ 
                        err%)            /* Error Code 0% = Ok         */


        return

        set_file_name
            init(" ") file$, script$
            file$   = "MFGLOWES"
            script$ = "MFGLOWES"
            gosub open_file
        return

        open_file
            library$        = "APCDATA "
            volume$         = "CARLOS"
            call "OPENFILE" (#5, "IO   ", f2%(5%), rslt$(5%), axd$ )
            if f2%(5%) <> 0% then goto L30200
               gosub file_exists         
               if comp% <> 16% then goto exit_program
                  call "FILEBGON" (#5)

L30200:    open nodisplay #5, output, space = 100%,                    ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return

        close_mfglowes
            lb1% = 0% : lb2% = 0%

            close #5

            call "LINK" addr(script$, lb1%, lb2%)
                if lb1%    > 0% then error% = 4%

            call "FILEBGON" (#5)          /* Scratch 'MFGLOWES'   */

        return
 
        file_exists
          comp% = 2%                                   
          hdr$ = "*** Energy Star File Exists **"
          msg$(1%) = "        The File (MFENERGY) Already Exists.      "
          msg$(2%) = "       E n e r g y   S t a r   L a b e l s       "
          msg$(3%) = "Press <RETURN> To Exit Prog, or PF(16) to Delete."
          call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L35040:     FMT CH(102)              /* File Key               EWDLABSP */


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
                                L40160,          /* sc_color$          */~
                                L40160,          /* sc_dept$           */~
                                L40160,          /* sc_load$           */~
                                L40160,          /* sc_seq_fr$ & _to$  */~
                                L40160,          /* sc_sku$            */~
                                L40170           /* sc_qty$            */

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
               at (03,02), "Production Date  :",                         ~
               at (03,25), fac(lfac$(1%)), sc_prddate$          , ch(10),~
                                                                         ~
               at (04,02), "Color Selection  :",                         ~
               at (04,25), fac(lfac$(2%)), sc_color$            , ch(02),~
               at (04,40), fac(hex(84)),   sc_color_d$          , ch(30),~
                                                                         ~
               at (05,02), "Dept. Code       :",                         ~
               at (05,25), fac(lfac$(3%)), sc_dept$             , ch(03),~
               at (05,40), fac(hex(84)),   sc_dept_d$           , ch(30),~
                                                                         ~
               at (06,02), "Load No. or ALL  :",                         ~
               at (06,25), fac(lfac$(4%)), sc_load$             , ch(05),~
                                                                         ~
               at (07,02), "Prod. Seq. Range :",                         ~
               at (07,25), fac(lfac$(5%)), sc_seq_fr$           , ch(05),~
               at (07,45), fac(lfac$(5%)), sc_seq_to$           , ch(05),~
                                                                         ~
               at (08,02), "Single SKU or ALL:",                         ~
               at (08,25), fac(lfac$(6%)), sc_sku$              , ch(06),~
                                                                         ~
               at (09,02), "Quantity Def (1) :",                         ~
               at (09,25), fac(lfac$(7%)), sc_qty$              , ch(03),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 15% then goto L40420
                  call "PRNTSCRN"
                  goto L40190

L40420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &        ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                      (15)Print Screen "
            pf$(3%) = "                                        " &        ~
                      "                      (16)PRINT LABELS "
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
            return
L40700:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                                       "
            pf$(3%) = "                                        " &        ~
                      "                                       " 
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50000,        /* Production Date        */~
                              L50100,        /* Color Code             */~
                              L50200,        /* Department Code        */~
                              L50300,        /* Load No.               */~
                              L50400,        /* Prod Seq. Range        */~
                              L50500,        /* Single SKU No or ALL   */~
                              L50600         /* Quantity Default (1)   */ 

            return

L50000: Rem Enter a Production Date           sc_prddate$, sc_prddte$
            call "DATEOKC" (sc_prddate$, date%, errormsg$)
            if date% = 0% then goto L50040

            sc_prddte$ = sc_prddate$
            call "DATUFMTC" (sc_prddte$)
            sp_key$ = all(hex(00))
            str(sp_key$,1%,6%) = str(sc_prddte$,1%,6%)
            read #2,key > sp_key$, using L50020, sp_key$, eod goto L50030
L50020:        FMT CH(41)
            if str(sp_key$,1%,6%) <> str(sc_prddte$,1%,6%) then goto L50030

            return
L50030:       errormsg$ = "(Error)-No Data for Production Date Specified?"
L50040:       gosub error_prompt
              init(" ") sc_prddate$, sc_prddte$
            return

L50100: Rem Enter a Color Code                  sc_color$, sc_format$
            if sc_color$ = "  " then goto L50110
            gosub check_color
            if color% = 0% then goto L50110
               sc_color_d$ = desc$
               p% = pos(desc$ = "-")
               sc_format$ = str(desc$,p% + 2%,1%) 
        return
L50110:     errormsg$ = "(Error) - Invalid Color Selection?"
            gosub error_prompt
            init(" ") sc_color$, sc_format$, sc_color_d$
        return

L50200: Rem Enter a Valid Department Code          sc_dept$, sc_dept_d$
            init(" ") sc_dept_d$
            if sc_dept$ <> " " then goto L50210
               sc_dept$ = "ALL"

L50210:     if sc_dept$ <> "ALL" then goto L50220
                sc_dept_d$ = "*** All Departments"
                return
L50220:     gosub check_dept
            if dept% = 0% then goto L50230
               sc_dept_d$ = desc$
        return
L50230:     errormsg$ = "(Error) Invalid Department Code"
            gosub error_prompt
            init(" ") sc_dept$, sc_dept_d$
        return

L50300: Rem Enter a Load No.               sc_load$
            if str(sc_load$,1%,1%) <> " " then goto L50310
               sc_load$ = "ALL  "

L50310:     if str(sc_load$,1%,3%) <> "ALL" then goto L50320
               sc_load$ = "ALL  "
               return
L50320:     if len(sc_load$) <> 5 then goto L50330
        return
L50330:     errormsg$ = "(Error) - Invalid Load Number"
            gosub error_prompt
            init(" ") sc_load$
        return

L50400: Rem Enter a Prod Seq. Range          sc_seq_fr$, sc_seq_to$
            if str(sc_seq_fr$,1%,1%) <> " " then goto L50410
               sc_seq_fr$ = "ALL  "
               sc_seq_to$ = sc_seq_fr$

L50410:     if str(sc_seq_fr$,1%,3%) <> "ALL" then goto L50420
               sc_seq_to$ = sc_seq_fr$
               return

L50420:     convert sc_seq_fr$ to sc_seq_fr%, data goto L50430

            convert sc_seq_fr% to sc_seq_fr$, pic(00000)

            if str(sc_seq_to$,1%,1%) = " " then sc_seq_to$ = sc_seq_fr$

            convert sc_seq_to$ to sc_seq_to%, data goto L50440

            convert sc_seq_to% to sc_seq_to$, pic(00000)

            if sc_seq_fr$ > sc_seq_to$ then goto L50450 
        return
L50430:     errormsg$ = "(Error) - Invalid Beginning Sequence Number?"
            gosub error_prompt
            init(" ") sc_seq_fr$, sc_seq_to$
        return
L50440:     errormsg$ = "(Error) - Invalid Ending Sequence Number?"
            gosub error_prompt
            init(" ") sc_seq_fr$, sc_seq_to$
        return
L50450:     errormsg$ = "(Error) - Invalid Sequence Number Range?"
            gosub error_prompt
            init(" ") sc_seq_fr$, sc_seq_to$
        return

L50500: Rem Enter Single SKU No.               sc_sku$
            if str(sc_sku$,1%,1%) <> " " then goto L50510
               sc_sku$ = "ALL  "
L50510:     if str(sc_sku$,1%,3%) <> "ALL" then goto L50520
               sc_sku$ = "ALL  "
               return
L50520:     convert sc_sku$ to sc_sku%, data goto L50530

        REM    convert sc_sku% to sc_sku$, pic(000000)
        return
L50530:     errormsg$ = "(Error) - Invalid Sku Number?"
            gosub error_prompt
            init(" ") sc_sku$
        return  

L50600: Rem Enter Quantity         .               sc_qty$
            if str(sc_sku$,1%,3%) = "ALL" then sc_qty$ = "001"
            if str(sc_qty$,1%,1%) = " " then sc_qty$ = "001"

            convert sc_qty$ to sc_qty%, data goto L50610

            convert sc_qty% to sc_qty$, pic(000)
        return
L50610:     errormsg$ = "(Error) - Invalid Label Quantity?"
            gosub error_prompt
            init(" ") sc_qty$
        return  

        check_dept
            dept% = 0%
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "PLAN DEPT"
            str(readkey$,10%,15%) = sc_dept$
            read #1,key = readkey$, using L51000, desc$, eod goto L51010
L51000:        FMT POS(25), CH(30)
            dept% = 1%
L51010: return
              
        check_color
            color% = 0%
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "PLANLABEL"
            str(readkey$,10%,15%) = sc_color$
            read #1,key = readkey$, using L51000, desc$, eod goto L51020
            color% = 1%
L51020: return

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        open_error                                    /* (EWD004)        */
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return
                                                      /* (EWD004)        */
        load_results
           k% = 2%
           hdr$     = "***** Label Generation Results *****"
           msg$(1%) = "This run generated xxxxx label(s)."
           msg$(2%) = "---"
           msg$(3%) = "Press <ENTER> to Acknowledge & Continue"
           convert lbl% to str(msg$(1%),20%,5%), pic(####0)
           if lbl% <> 0% then L64100
               msg$(1%) = "NO LABELS GENERATED!!!"
               str(msg$(3%),,13%) = " Press <PF16>"
L64100:    call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
           if lbl% <> 0% and k% <>  0% then load_results
           if lbl%  = 0% and k% <> 16% then load_results
           lbl% = 0%
        return

        print_error  
            hdr$     = "***** Label Printing Error *****"
            msg$(1%) = "ERROR OCCURRED WHILE PRINTING LABELS!!!"
            msg$(2%) = "Return Code (EWDPLA71) = "   
            msg$(3%) = "Press <ENTER> to Continue, <PF16> to Exit"

            convert err% to str(msg$(2%),26%,2%), pic(#0)    
L64550:     k% = 2%
            call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
                if k%  =  0% then return
                if k% <> 16% then goto L64550
            return clear all
            goto inputmode

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
        end

