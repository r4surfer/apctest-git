        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDPLN75                             *~
            *  Creation Date     - 02/28/00                             *~
            *  Last Modified Date- 10/14/2013                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Modifications By  - Christie Gregory                     *~ 
            *                                                           *~
            *  Description       - Input and Edit Energy Star Data      *~
            *                                                           *~
            *  Code Tables Used  -                                      *~
            *                                                           *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *  Special Comments      - Error Codes                      *~
            *                                                           *~
            *       GROUP              DESCRIPTION                      *~
            *       -----              -----------                      *~
            *        000               DESIGN PRESSURE RATING           *~
            *        0??               U-FACTOR RATING                  *~
            *        1??               SOLAR HEAT GAIN RATING           *~
            *        2??               VISABLE TRANSMITTANCE RATING     *~
            *        3??               AIR LEAKAGE RATING               *~
            *        4??               SOLAR HEAT WITH 5/8 GRID         *~
            *        5??               SOLAR HEAT WITH 1   GRID         *~
            *        6??               VISABLE TRANSMITTANCE 5/8 GRID   *~
            *        7??               VISABLE TRANSMITTANCE 1   GRID   *~
            *        8??               VISABLE TRANSMITTANCE SDL GRID   *~
            *        9??               SOLAR HEAT WITH SDL GRID         *~
            *        A??               U-Factor 5/8 GRID                *~
            *        B??               U-Factor 1   GRID                *~
            *        C??               U-Factor SDL GRID                *~
            *        D??               U-Factor Florida Contour         *~
            *        E??               SOLAR HEAT Florida Contour       *~
            *        F??               VISABLE TRANSMITTANCE Florida Con*~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/14/00 ! (New) Program -                          ! RHH *~
            * 08/23/00 ! (EWD001) Mod for the new Visable Transmit! CMG *~
            *          !   -tance. Allow for Res and Non-Res data !     *~
            *          !   to be entered and edited into database.!     *~
            *          !   New Code group (20?) created.          !     *~
            * 09/27/02 ! (EWD002) Mod for the new Air Leakage     ! CMG *~
            *          !   Group Codes (30?) created.             !     *~
            * 01/16/03 ! (EWD003) Mod for the new with grid and   ! CMG *~
            *          !   1 inch grid options.  Entered by new   !     *~
            *          !   group codes. Add (4??,5??,6??,&7??).   !     *~
            * 04/27/04 ! (EWD004) Mod to add a deletion option    ! CMG *~
            * 04/02/08 ! (AWD005) mods for SDL Grid               ! CMG *~
            *08/07/2008! (AWD006) mods for SDL Grid               ! DES *~
            *06/10/2009! (AWD007) mods for Florida Contour Rating ! CMG *~
            *03/12/2013! (AWD008) mods to use 001,002,& 003 as    ! CMG *~
            *          !   NTX DP Ratings.  Also use 001 for TDI N!     *~
            *10/15/2013! (AWD009) mod to allow additional NC DP   ! CMG *~
            *          !    ratings similar to TX products        !     *~
            *04/17/2020! (CR2520) Allow edit of DP rating on all  ! RDB *~
            *05/08/2020! CR1066  Allow TDI for group 022          ! RDB *~
            *************************************************************

        dim                                                              ~
            field$(5%)22,                /* Screen Fields              */~
            readkey$24, desc$30,         /* GENCODES - Lookup          */~
            eg_key$6, eg_rec$80,         /* Primary Key                */~        
            eg_model$3, eg_model_d$30,   /* Model Code                 */~
            bg_model$3, bg_model_d$30,   /* Model Code                 */~
            ed_model$3, ed_model_d$30,   /* Model Code                 */~
            eg_group$3, eg_group_d$30,   /* <000> - Design Pressure    */~
                                         /* <001> - U-Factor           */~
                                         /* <101> - Solar Heat Co-eff  */~
            eg_res$10,                   /* Residential Value          */~
            eg_nonres$10,                /* Non-Residential Value      */~
            eg_dp$2,                     /* Design Pressure            */~
            eg_fl$12,                    /* Design Pressure            */~
            eg_fill$44,                  /* Filler Area                */~
            sku_key$16,                  /* AWDSKUXR key               */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            filename$8,                  /* Used by EWDOPEN            */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$10,                     /* Date for screen display    */~
            prt_title$40,                /* Report Title               */~
            company$40,                  /* Company Name               */~
            rpt_time$8,                  /* Report Time                */~            
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim f2%(2%),                     /* = 0 if the file is open    */~
            f1%(2%),                     /* = 1 if READ was successful */~
            fs%(2%),                     /* = 1 Open, -1 doesn't exist */~
                                         /*   0 if not checked         */~
            rslt$(2%)20                  /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Input and Edit Energy Star Data"
            pname$ = "EWDPLN75 - Rev: R7.00"

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
            * #1  ! EWDPLNEX ! Energy Star Master File OLD - EWDPLNES   *~
            * #2  ! GENCODES ! System Master Code Table Files           *~
            * #3  ! AWDSKUXR ! System Master Code Table Files           *~
             ************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "EWDPLNEX",                                      ~
                        varc,     indexed,  recsize =   80,              ~
                        keypos =  1,   keylen =   6

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24


            select #3,   "AWDSKUXR",                                   ~
                        varc,     indexed,  recsize = 256,              ~
                        keypos =    1, keylen =  16,                    ~
                        alt key  1, keypos =  17, keylen =  20,         ~
                            key  2, keypos =  37, keylen =  45, dup

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 100%, rslt$(1%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%), 100%, rslt$(3%))

        REM    filename$ = "EWDPLNES" 
        REM    call "EWDOPEN" (#1, filename$, err%)
        REM    if err% <> 0% then gosub open_error

            filename$ = "GENCODES" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATFMTC" (date$)
            edtmessage$  = "To Modify Displayed Values, Position Cursor"&~
                           " to Desired Value & Press (RETURN)."
            delete_data% = 0%                                  /*  (EWD004) */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            apc$   = "(EWD) Input and Edit Energy Star Data"
            gosub initialize_variables

            for fieldnr% = 1% to   6%
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
                      if keyhit% =  7% and fieldnr% = 1% then goto inputmode_c
                                                                 /* (EWD004) */
                      if keyhit% = 12% and fieldnr% = 1% then goto inputmode_d
                      if keyhit% = 14% and fieldnr% = 1% then goto inputmode_r
                      if keyhit% <> 0% then       L10130
L10230:               gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
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
                  if keyhit%  =  8% then gosub delete_it
                  if keyhit%  = 16% then gosub dataput
                  if keyhit% <>  0% then       editpg1
L11120:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% > 6% then editpg1
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
            *       I N P U T   M O D E   C O P Y   S C R E E N         *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode_d
           delete_data% = 1%                           /*  (EWD004)   */
        inputmode_c 
            apc$   = "(EWD) Energy Star Copy Data Utility  "
            if delete_data% = 1% then str(apc$,20,20) = "Delete Data Utility"
            gosub initialize_variables

            for fieldnr% = 1% to   2%
L11110:         gosub'052(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L11230
L11130:         gosub'102(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L11215
L11160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'052(fieldnr%)
                         if enabled% = 1% then L11130
                         if fieldnr% = 1% then L11110
                         goto L11160
L11215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L11130
L11230:               gosub'152(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L11130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   C O P Y   S C R E E N          *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg2
            lastfieldnr% = 0%
            gosub'102(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then gosub copy_data
                  if keyhit% <>  0% then       editpg2
L12120:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% > 2% then editpg2
            if fieldnr% = lastfieldnr% then    editpg2
            gosub'052(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg2
L12170:     gosub'102(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L12170
            gosub'152(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L12170
                  lastfieldnr% = fieldnr%
            goto L12120

        REM *************************************************************~
            *       I N P U T   M O D E   R E P O R T   S C R E E N     *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode_r 
            apc$   = "(EWD) Energy Star Report Utility     "
            gosub initialize_variables

            for fieldnr% = 1% to   2%
L13110:         gosub'053(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L13230
L13130:         gosub'103(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% <>  4% then       L13215
L13160:                  fieldnr% = max(1%, fieldnr% - 1%)
                         gosub'053(fieldnr%)
                         if enabled% = 1% then L13130
                         if fieldnr% = 1% then L13110
                         goto L13160
L13215:               if keyhit% = 16% and fieldnr% = 1% then exit_program
                      if keyhit% <> 0% then       L13130
L13230:               gosub'153(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L13130
            next fieldnr%

        REM *************************************************************~
            *        E D I T   M O D E   R E P O R T   S C R E E N      *~
            *-----------------------------------------------------------*~
            * Handles operation of EDIT MODE for data entry screens.    *~
            *************************************************************

        editpg3
            lastfieldnr% = 0%
            gosub'103(0%, 2%)           /* Display Screen - No Entry   */
                  if keyhit%  =  1% then gosub startover
                  if keyhit%  = 16% then gosub generate_report
                  if keyhit% <>  0% then       editpg3
L14120:     fieldnr% = cursor%(1%) - 4%
            if fieldnr% < 1% or fieldnr% > 2% then editpg3
            if fieldnr% = lastfieldnr% then    editpg3
            gosub'053(fieldnr%)         /* Check Enables, Set Defaults */
                  if enabled% =  0% then       editpg3
L14170:     gosub'103(fieldnr%, 2%)     /* Display & Accept Screen     */
                  if keyhit%  =  1% then gosub startover
                  if keyhit% <>  0% then L14170
            gosub'153(fieldnr%)         /* Edit Field for Valid Entry  */
                  if errormsg$ <> " " then L14170
                  lastfieldnr% = fieldnr%
            goto L14120

        copy_data                                  /*  (EWD004) - BEG */
            if delete_data% <> 1% then goto not_delete
                call "APCPASSW" ("EWDPLN75", userid$, pass%)
                if pass% <> 0% then goto copy_data_done
not_delete:
            if delete_data% <> 1% then call "SHOSTAT" ("Copying Data From ("~
                             &bg_model$&") Too ("&ed_model$&")" )           ~
               else call "SHOSTAT" ("Deleting Data From ("                  ~
                             &bg_model$&") Too ("& ed_model$&")" )

                                                    /*  (EWD004)  - END  */
            init(" ") eg_key$, eg_rec$
            str(eg_key$,1%,3%) = bg_model$ 
        copy_data_next  
            read #1,hold, key > eg_key$, using L19010, eg_rec$,         ~
                                                 eod goto copy_data_done
L19010:        FMT CH(80)
                                                          /*  (EWD004) */
            if delete_data% <> 1% and bg_model$ <> str(eg_rec$,1%,3%)   ~
                                then goto copy_data_done
                                                          /*  (EWD004) */
            if delete_data% = 1% and str(eg_rec$,1%,3%) > ed_model$     ~
                                then goto copy_data_done
                                                          /*  (EWD004) */
            if delete_data% = 1% and str(eg_rec$,4%,3%) <> "000" then   ~
                                goto delete_it_range

               eg_key$ = str(eg_rec$,1%,6%)
               if str(eg_rec$,4%,3%) = "000" then goto copy_data_next
               str(eg_rec$,1%,3%) = ed_model$
               read #1,hold,key = str(eg_rec$,1%,6%), eod goto L19020

delete_it_range:                                             /*  (EWD004) */
                  delete #1

               if delete_data% = 1% then goto copy_data_next /* (EWD004) */

L19020:        write #1, using L19010, eg_rec$, eod goto copy_data_done
               goto copy_data_next

        copy_data_done 
                delete_data% = 0%                         /*  (EWD004)  */
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

        scrn1_msg  :  data                          /*  (EWD002)  */      ~
         "Enter a Valid Manufacturing Model Code?                       ",~   
/*AWD008*/"Enter a Valid Group (000)=DP,(001),(002),(003)=NTX DP & TDI Num",~
         "Enter a Valid Window Width?     )                             ",~
         "Enter a Valid Window Height?                                  ",~
         "Enter a Valid Design Pressure (DP) Value?                     ",~
         "Enter a Valid Florida Approval Code                           "
                                                                      
        deffn'060(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28210
                inpmessage$ = edtmessage$
                return

L28210
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg1, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg1 :  data                                                ~
         "Enter a Valid Model Code to Copy From?                        ",~
         "Enter a Valid Model Code to Copy Too?                         " 

        deffn'070(scrnr%, fieldnr%)
            if fieldnr% <> 0% then L28310
                inpmessage$ = edtmessage$
                return

L28310
*        Define the Input Message for the Screen/Field Indicated
            if scrnr% = 1% then restore line = scrn1_msg2, fieldnr%
            read inpmessage$      /* Read Input Message */
            return

        scrn1_msg2 :  data                                                ~
         "Enter a Valid Beginning Model Code?                           ",~
         "Enter a Valid Ending Model Code?                              " 

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
            init(" ") errormsg$, inpmessage$, eg_model$, eg_group$,       ~
                      eg_res$, eg_nonres$, eg_dp$, eg_fill$, eg_model_d$, ~
                      eg_group_d$, eg_key$, field$(), bg_model$, bg_model_d$,~
                      ed_model$, ed_model_d$, prt_title$, company$,       ~
                      rpt_time$, eg_fl$

REM         type% = 1%
REM         field$(1%) = "Residential U-Factor "
REM         field$(2%) = "Non-Residential U-Fac"
REM         field$(3%) = "                      "
REM         field$(4%) = "                      "

            type% = 3%
            field$(1%) = "Window Width (Dec.)  :" 
            field$(2%) = "Window Height (Dec.) :"
            field$(3%) = "Design Pressure      :"
            field$(4%) = "Florida Approval Code:"

REM     lfac$(5%) = hex(84)
REM     lfac$(6%) = hex(84)
        return

        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload
            init(" ") eg_key$
            errorkey% = 1%                   /* CR2520 */
            str(eg_key$,1%,3%) = eg_model$
            str(eg_key$,4%,3%) = eg_group$
            rec% = 0%
            eg_dp$    = "00"
            eg_res    = 0.0
            eg_nonres = 0.0
            read #1,hold,key = eg_key$, eod goto L30010
            
/*@@@*/     get #1, using L35000, eg_model$,         /* Model Code     */~
                                  eg_group$,         /* Group Code     */~
                                  eg_res,            /* Residential Val*/~
                                  eg_nonres,         /* Non-Residential*/~
                                  eg_dp$,            /* Design Pressure*/~
                                  eg_fl$,            /* Design Pressure*/~
                                  eg_fill$           /* Filler Area    */

            convert eg_res to eg_res$, pic(####.####-)

            convert eg_nonres to eg_nonres$, pic(####.####-)
 
            rec% = 1%
            
            errorkey% = 0%                   /* CR2520 */
L30010:  return

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        delete_it
        dataput

            convert eg_res$ to eg_res, data goto L31010
L31010:
            convert eg_nonres$ to eg_nonres, data goto L31020
L31020: 
          init(" ") eg_key$
          str(eg_key$,1%,3%) = eg_model$
          str(eg_key$,4%,3%) = eg_group$
 
          read #1,hold,key = eg_key$, eod goto L31030
            
            delete #1

            if keyhit% = 8% then goto L31040
/*@@@*/
L31030:     put #1, using L35000, eg_model$,         /* Model Code     */~
                                  eg_group$,         /* Group Code     */~
                                  eg_res,            /* Residential Val*/~
                                  eg_nonres,         /* Non-Residential*/~
                                  eg_dp$,            /* Design Pressure*/~
                                  eg_fl$,            /* Design Pressure*/~
                                  eg_fill$           /* Filler Area    */

            write #1, eod goto L31040

L31040: return clear all
        goto inputmode

        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L35000:     fmt  ch(03),                             /* Model Code     */~
                 ch(03),                             /* Group Code     */~
                 pd(14,4),                           /* Residential Val*/~
                 pd(14,4),                           /* Non-Residential*/~
                 ch(02),                             /* Design Pressure*/~
                 ch(12),                             /* FL/TDI Area    */~
                 ch(44)                              /* Filler Area    */

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
              on fieldnr% gosub L40170,       /* Model Selection       */~
                                L40170,       /* Group Selection       */~
                                L40170,       /* Residential Value     */~
                                L40170,       /* Non-Residential Value */~ 
                                L40170,       /* Design Pressure (DP)  */~     
                                L40170        /* Florida Approval Code */      

              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     gosub set_pf1
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,23), fac(hex(a4)), apc$                   , ch(38),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "MFG Model Code       :",                     ~
               at (05,25), fac(lfac$(1%)), eg_model$            , ch(03),~
               at (05,40), fac(hex(84)), eg_model_d$            , ch(30),~
                                                                         ~
               at (06,02), "Group Code           :",                     ~
               at (06,25), fac(lfac$(2%)), eg_group$            , ch(03),~
               at (06,40), fac(hex(84)), eg_group_d$            , ch(30),~
                                                                         ~
               at (07,02), fac(hex(84)), field$(1%)             , ch(22),~   
               at (07,25), fac(lfac$(3%)), eg_res$              , ch(10),~
                                                                         ~   
               at (08,02), fac(hex(84)), field$(2%)             , ch(22),~
               at (08,25), fac(lfac$(4%)), eg_nonres$           , ch(10),~
                                                                         ~
               at (09,02), fac(hex(84)), field$(3%)             , ch(22),~
               at (09,25), fac(lfac$(5%)), eg_dp$               ,  ch(2),~
                                                                         ~
               at (10,02), fac(hex(84)), field$(4%)             , ch(22),~
               at (10,25), fac(lfac$(6%)), eg_fl$               , ch(12),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

 
               if keyhit% <> 15 then goto L40200
                  call "PRNTSCRN"
                  goto L40190

L40200: close ws
        call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

set_pf1:
        if type% <> 1% then goto L40300
           field$(1%) = "Residential U-Factor :"       /* U-Factors     */ 
           field$(2%) = "Non-Residential U-Fac:"
           field$(3%) = "                       "
           field$(4%) = "                       "
           lfac$(5%) = hex(84)
           lfac$(6%) = hex(84)
           if fieldnr% = 3% then str(inpmessage$,33%,15%) = "for U-Factor?"
           if fieldnr% = 4% then str(inpmessage$,37%,15%) = "for U-Factor?"
           goto L40500

L40300: if type% <> 2% then goto L40400
           field$(1%) = "Residential Solar H. :"       /* Solar Heat    */ 
           field$(2%) = "Non-Residential Solar:"
           field$(3%) = "                       "
           field$(4%) = "                       "
           lfac$(5%) = hex(84)
           lfac$(6%) = hex(84)
           if fieldnr% = 3% then str(inpmessage$,33%,15%) = "for Solar Heat?"
           if fieldnr% = 4% then str(inpmessage$,37%,15%) = "for Solar Heat?"
           goto L40500
/* Design Pressure*/
L40400: if type% <> 3% then goto L40450                  /* (EWD001) */
           field$(1%) = "Window Width (Dec.)  :" 
           field$(2%) = "Window Height (Dec.) :"
           field$(3%) = "Design Pressure      :"
           field$(4%) = "Florida Approval Code:"
           if eg_group$ = "001" then field$(4%) = "TDI Approval Code    :"
/* 1066 */            
           if eg_group$ = "022" then field$(4%) = "TDI Approval Code    :"
           if eg_group$ = "004" then field$(4%) = "TDI Apprl Code Impact:"
           if eg_group$ = "002" or eg_group$ = "003" then field$(4%) = " "
           if eg_group$ = "002" or eg_group$ = "003" then lfac$(6%) = hex(84)
           /* (AWD009) */
           if eg_group$ >= "020" and eg_group$ <> "022" then field$(4%) = " "
           if eg_group$ >= "020" and eg_group$ <> "022" then lfac$(6%) = hex(84)           
           /* (\AWD009) */           
           
           if fieldnr% = 3% then str(inpmessage$,15%,24%) = "Window Width?"
           if fieldnr% = 4% then str(inpmessage$,15%,24%) = "Window Height?"
           goto L40500
           
L40450:    if type% <> 4% then goto L40460             /* Visable Trans  */ 
           field$(1%) = "Residential Vis Trans:"       /* Visable Trans */  
           field$(2%) = "Non-Residential Vis  :"                            
           field$(3%) = "                       "                           
           lfac$(6%) = hex(84)
           lfac$(5%) = hex(84)                                              
           if fieldnr% = 3% then str(inpmessage$,33%,15%) = "for Vis Trans?"
           if fieldnr% = 4% then str(inpmessage$,37%,15%) = "for Vis Trans?"
           goto L40500                                                      /* (EWD001) */

L40460:    if type% <> 5% then goto L40470
           field$(1%) = "Residential Air Leaka:"       /* Air Leakage   */
           field$(2%) = "Non-Residential Air L:"                          
           field$(3%) = "                       "                           
            lfac$(6%) = hex(84)
           lfac$(5%) = hex(84)                                              
           if fieldnr% = 3% then str(inpmessage$,33%,15%) = "for Air Leaka?"
           if fieldnr% = 4% then str(inpmessage$,37%,15%) = "for Air Leaka?"
                                                                            /* (EWD002) */

L40470:    if type% <> 6% then goto L40480
           field$(1%) = "Residential Solar 5/8:"       /* Solar  5/8    */
           field$(2%) = "Non-Residential Solar:"                          
           field$(3%) = "                       "                           
           lfac$(6%) = hex(84)
           lfac$(5%) = hex(84)                                              
           if fieldnr% = 3% then str(inpmessage$,33%,15%) = "for Solar 5/8?"
           if fieldnr% = 4% then str(inpmessage$,37%,15%) = "for Solar 5/8?"
                                                                            /* (EWD003) */

L40480:    if type% <> 7% then goto L40490
           field$(1%) = "Residential Solar  1 :"       /* Solar  1      */
           field$(2%) = "Non-Residential Solar:"                          
           field$(3%) = "                       "                           
           lfac$(6%) = hex(84)
           lfac$(5%) = hex(84)                                              
           if fieldnr% = 3% then str(inpmessage$,33%,15%) = "for Solar  1 ?"
           if fieldnr% = 4% then str(inpmessage$,37%,15%) = "for Solar  1 ?"
                                                                            /* (EWD003) */

L40490:    if type% <> 8% then goto L40495
           field$(1%) = "Residential Visab 5/8:"       /* Visable 5/8   */
           field$(2%) = "Non-Residential Visab:"                          
           field$(3%) = "                       "                           
           lfac$(6%) = hex(84)
           lfac$(5%) = hex(84)                                              
           if fieldnr% = 3% then str(inpmessage$,33%,15%) = "for Visab 5/8?"
           if fieldnr% = 4% then str(inpmessage$,37%,15%) = "for Visab 5/8?"
                                                                            /* (EWD003) */

L40495:    if type% <> 9% then goto L40500
           field$(1%) = "Residential Visable 1:"       /* Visable Tr 1  */ 
           field$(2%) = "Non-Residential Visab:"                           
           field$(3%) = "                       "                           
           lfac$(6%) = hex(84)
           lfac$(5%) = hex(84)                                              
           if fieldnr% = 3% then str(inpmessage$,33%,15%) = "for Visable 1?"
           if fieldnr% = 4% then str(inpmessage$,37%,15%) = "for Visable 1?"
                                                                            /* (EWD003) */


L40500:    if type% <> 10% then goto L40510
           field$(1%) = "Residential Visab SDL:"       /* Visable Tr 1  */ 
           field$(2%) = "Non-Resid Visab SDL  :"                           
           field$(3%) = "                       "                           
           lfac$(6%) = hex(84)
           lfac$(5%) = hex(84)                                              
           if fieldnr% = 3% then str(inpmessage$,33%,15%) = "for Visab SDL?"
           if fieldnr% = 4% then str(inpmessage$,37%,15%) = "for Visab SDL?"
                                                                            /* (AWD005) */

L40510:    if type% <> 11% then goto L40520
           field$(1%) = "Residential Solar SDL:"       /* Visable Tr 1  */ 
           field$(2%) = "Non-Resid Solar SDL  :"                           
           field$(3%) = "                       "                           
           lfac$(6%) = hex(84)
           lfac$(5%) = hex(84)                                              
           if fieldnr% = 3% then str(inpmessage$,33%,15%) = "for Solar SDL?"
           if fieldnr% = 4% then str(inpmessage$,37%,15%) = "for Solar SDL?"
                                                                            /* (AWD005) */
                                                                     
/* <AWD006> */
L40511:    if type% <> 12% then goto L40512
           field$(1%) = "Residential U-Fac 5/8:"         /* U-Factor 5/8   */ 
           field$(2%) = "Non-Residential U-Fac:"     
           field$(3%) = "                       "   
           field$(4%) = "                       "   
           lfac$(5%) = hex(84)                               
           lfac$(6%) = hex(84)                               
           if fieldnr% = 3% then str(inpmessage$,33%,15%) = "for U-Fac 5/8?"
           if fieldnr% = 4% then str(inpmessage$,37%,15%) = "for U-Fac 5/8?"

L40512:    if type% <> 13% then goto L40513
           field$(1%) = "Residential U-Fac   1:"       /* U-Factor   1  */
           field$(2%) = "Non-Residential U-Fac:"                         
           field$(3%) = "                       "                       
           field$(4%) = "                       "                       
           lfac$(5%) = hex(84)                                         
           lfac$(6%) = hex(84)                                         
           if fieldnr% = 3% then str(inpmessage$,33%,15%) = "for U-Fac   1?"
           if fieldnr% = 4% then str(inpmessage$,37%,15%) = "for U-Fac   1?"

L40513:    if type% <> 14% then goto L40514
           field$(1%) = "Residential U-Fac SDL:"       /* U-Factor SDL  */
           field$(2%) = "Non-Resid U-Fac SDL  :"                        
           field$(3%) = "                       "                      
           field$(4%) = "                       "                      
           lfac$(5%) = hex(84)                                      
           lfac$(6%) = hex(84)                                      
           if fieldnr% = 3% then str(inpmessage$,33%,15%) = "for U-Fac SDL?"
           if fieldnr% = 4% then str(inpmessage$,37%,15%) = "for U-Fac SDL?"
/* </AWD006> */


/* <AWD007> */
L40514:    if type% <> 15% then goto L40515
           field$(1%) = "Residential U-Fac Fla:"         /* U-Factor 5/8   */ 
           field$(2%) = "Non-Residential U-Fac:"     
           field$(3%) = "                       "   
           field$(4%) = "                       "   
           lfac$(5%) = hex(84)                               
           lfac$(6%) = hex(84)                               
           if fieldnr% = 3% then str(inpmessage$,33%,15%) = "for U-Fac Fla?"
           if fieldnr% = 4% then str(inpmessage$,37%,15%) = "for U-Fac Fla?"

L40515:    if type% <> 16% then goto L40516
           field$(1%) = "Residential Solar Fla:"       /* Solar Head Fla*/
           field$(2%) = "Non-Residential Solar:"                         
           field$(3%) = "                       "                       
           field$(4%) = "                       "                       
           lfac$(5%) = hex(84)                                         
           lfac$(6%) = hex(84)                                         
           if fieldnr% = 3% then str(inpmessage$,33%,15%) = "for Solar Fla?"
           if fieldnr% = 4% then str(inpmessage$,37%,15%) = "for Solar Fla?"

L40516:    if type% <> 17% then goto L40520
           field$(1%) = "Residential Vis Flori:"       /* Visable T Fla */
           field$(2%) = "Non-Resid Vis Florida:"                        
           field$(3%) = "                       "                      
           field$(4%) = "                       "                      
           lfac$(5%) = hex(84)                                      
           lfac$(6%) = hex(84)                                      
           if fieldnr% = 3% then str(inpmessage$,33%,15%) = "for Visable F?"
           if fieldnr% = 4% then str(inpmessage$,37%,15%) = "for Visable F?"
/* </AWD007> */
                                                                
L40520:
         if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &        ~
                      "                       (14)Report      "
            pf$(2%) = "                                        " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                 (7)Copy                " &        ~
                      "(12)Delete             (16)Exit Program" 
                                                               /*  (EWD004)  */
            pfkeys$ = hex(01ffff04ffff07ffffffff0cff0e0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(1%),64%) = " " : str(pfkeys$,14%,1%) = hex(ff)
                str(pf$(3%),18%,23%) = " " : str(pfkeys$,7%,1%) = hex(ff)
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                 (8)Delete              " &        ~
                      "                      (15)Print Screen "
            pf$(3%) = "                                        " &        ~
                      "                      (16)Data Update  "
            pfkeys$ = hex(01ffffffffffff08ffffffffff0e0f1000)

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
            *               S C R E E N   P A G E   2                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'102(fieldnr%, edit%)
              gosub'060(1%, fieldnr%)
              gosub set_pf2
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L41170,       /* From Model Code       */~
                                L41170        /* Too Model Code        */

              goto L41190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L41170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L41190:     gosub set_pf2
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,23), fac(hex(a4)), apc$                   , ch(38),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Beginning Model Code :",                     ~
               at (05,25), fac(lfac$(1%)), bg_model$            , ch(03),~
               at (05,40), fac(hex(84)), bg_model_d$            , ch(30),~
                                                                         ~
               at (06,02), "Ending   Model Code  :",                     ~
               at (06,25), fac(lfac$(2%)), ed_model$            , ch(03),~
               at (06,40), fac(hex(84)), ed_model_d$            , ch(30),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

 
               if keyhit% <> 15 then goto L41200
                  call "PRNTSCRN"
                  goto L41190

L41200: close ws
        call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

set_pf2:
         if edit% = 2% then L41610     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &        ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L41570
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L41570:     if fieldnr% > 1% then L41590
                str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L41590:     return

L41610: if fieldnr% > 0% then L41700  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                      (15)Print Screen "
            pf$(3%) = "                                        " &        ~
                      "                      (16)Copy Data    "
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)
                                                              /*  (EWD004) */
            if delete_data% = 1% then str(pf$(3%),67%,11% ) = "Delete Data"
            return
L41700:                              /*  Edit Mode - Enabled    */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                                       "
            pf$(3%) = "                                        " &        ~
                      "                                       " 
            pfkeys$ = hex(01ffffffffffffffffffffffffffffff00)
            return

        REM *************************************************************~
            *               S C R E E N   P A G E   3                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'103(fieldnr%, edit%)
              gosub'070(1%, fieldnr%)
              gosub set_pf3
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
              on fieldnr% gosub L42170,       /* From Model Code       */~
                                L42170        /* Too Model Code        */

              goto L42190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L42170:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L42190:     gosub set_pf3
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,64), "Today:",                                     ~
               at (01,71), fac(hex(8c)), date$                  , ch(10),~
               at (01,23), fac(hex(a4)), apc$                   , ch(38),~
               at (02,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), "Beginning Model Code :",                     ~
               at (05,25), fac(lfac$(1%)), bg_model$            , ch(03),~
               at (05,40), fac(hex(84)), bg_model_d$            , ch(30),~
                                                                         ~
               at (06,02), "Ending Model Code    :",                     ~
               at (06,25), fac(lfac$(2%)), ed_model$            , ch(03),~
               at (06,40), fac(hex(84)), ed_model_d$            , ch(30),~
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

 
               if keyhit% <> 15 then goto L42200
                  call "PRNTSCRN"
                  goto L42190

L42200: close ws
        call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

set_pf3:
         if edit% = 2% then L42610     /*  Input Mode             */
            pf$(1%) = "(1)Start Over    (4)Previous Field      " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                       (15)Print Screen"
            pf$(3%) = "                                        " &        ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L42570
                str(pf$(3%),64%) = " " : str(pfkeys$,16%,1%) = hex(ff)
L42570:     if fieldnr% > 1% then L42590
                str(pf$(1%),18%,26%) = " " : str(pfkeys$,4%,1%) = hex(ff)
L42590:     return

L42610: if fieldnr% > 0% then L42700  /*  Edit Mode - Select Fld */
            pf$(1%) = "(1)Start Over                           " &        ~
                      "                                       "
            pf$(2%) = "                                        " &        ~
                      "                      (15)Print Screen "
            pf$(3%) = "                                        " &        ~
                      "                      (16)Create Report"
            pfkeys$ = hex(01ffffffffffffffffffffffff0e0f1000)

            return
L42700:                              /*  Edit Mode - Enabled    */
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
            on fieldnr% gosub L50010,       /* Model Code              */~
                              L50100,       /* Energy Star Group Code  */~
                              L50200,       /* Residential Value       */~
                              L50300,       /* Non-Residential Value   */~
                              L50400,       /* Design Pressure         */~
                              L50500        /* Design Pressure         */

            return

L50010: REM Check Model Code                      eg_model$, eg_model_d$
            gosub lookup_model
            if model% = 0% then goto L50020
            eg_model_d$ = desc$

        return
L50020:     errormsg$ = "(Error) Invalid Model Code?"
            gosub error_prompt
            init(" ") eg_model$, eg_model_d$
        return
   
L50100: REM Check Group Code                      eg_group$, eg_group_d$
            gosub lookup_group
            if group% = 0% then goto L50130
                                         /* Check for 'U' Factor       */
               if str(eg_group$,1%,1%) = "0" then type% = 1%
                                         /* Check for Heat Coefficient */
               if str(eg_group$,1%,1%) = "1" then type% = 2%
                                         /* Check for Design Pressure  */
               if str(eg_group$,1%,3%) = "000" then type% = 3%
               if str(eg_group$,1%,3%) = "001" then type% = 3%
               if str(eg_group$,1%,3%) = "002" then type% = 3%
               if str(eg_group$,1%,3%) = "003" then type% = 3%
               if str(eg_group$,1%,3%) = "004" then type% = 3%
                                         /* Check for Visable Transmittance (EWD001) */
               if str(eg_group$,1%,1%) = "2" then type% = 4%
                                         /* Check for Air Leakage           (EWD002) */
               if str(eg_group$,1%,1%) = "3" then type% = 5%

                                         /* Check for Solar Heat with 5/8   (EWD003) */
               if str(eg_group$,1%,1%) = "4" then type% = 6%
                                         /* Check for Solar Heat with 1 in  (EWD003) */
               if str(eg_group$,1%,1%) = "5" then type% = 7%
                                         /* Check for Visable Tr with 5/8   (EWD003) */
               if str(eg_group$,1%,1%) = "6" then type% = 8%
                                         /* Check for Visable Tr with 1 in  (EWD003) */
               if str(eg_group$,1%,1%) = "7" then type% = 9%
                                         /* Check for Visable Tr with SDL (AWD005) */
               if str(eg_group$,1,1) = "8" then type% = 10%
                                         /* Check for Solar Heat with SDL (AWD005) */
               if str(eg_group$,1,1) = "9" then type% = 11%

           /* <AWD006> */
               if str(eg_group$,1,1) = "A" then type% = 12%
               if str(eg_group$,1,1) = "B" then type% = 13%
               if str(eg_group$,1,1) = "C" then type% = 14%
           /* </AWD006> */

           /* <AWD007> */
               if str(eg_group$,1,1) = "D" then type% = 15%
               if str(eg_group$,1,1) = "E" then type% = 16%
               if str(eg_group$,1,1) = "F" then type% = 17%
           /* </AWD007> */
           
           
REM (AWD009) always make group 3%
               type% = 3%           

               if type% = 1% then eg_group_d$ = "U - Factor Group         "
               if type% = 2% then eg_group_d$ = "Solar Heat Gain Co-eff.  "
               if type% = 3% then eg_group_d$ = "Design Pressure Group    "
               if type% = 3% and eg_group$ <> "000" then                  ~
                                  eg_group_d$ = "NTX Design Pressure Group "
/* (AWD009) */                                  
               if type% = 3% and eg_group$ >= "020" then                  ~
                                  eg_group_d$ = "Design Pressure Group    "
                                  
                                  
               if type% = 4% then eg_group_d$ = "Visable Transmittance    " 
 /* (EWD001) */
               if type% = 5% then eg_group_d$ = "Air Leakage Group        " 
 /* (EWD002) */
               if type% = 6% then eg_group_d$ = "Solar He with 5/8        "  
/* (EWD003) */
               if type% = 7% then eg_group_d$ = "Solar He with 1 in       " 
 /* (EWD003) */
               if type% = 8% then eg_group_d$ = "Visable Tr with 5/8      "  
/* (EWD003) */
               if type% = 9% then eg_group_d$ = "Visable Tr with 1 in     "  
/* (EWD003) */
               if type% = 10% then eg_group_d$ = "Visable Tr with SDL      " 
 /* (AWD005) */
               if type% = 11% then eg_group_d$ = "Solar He with SDL        "  
/* (AWD005) */
               
           /* <AWD006> */
               if type% = 12% then eg_group_d$ = "U-Factor with 5/8        " 
               if type% = 13% then eg_group_d$ = "U-Factor with 1 in       " 
               if type% = 14% then eg_group_d$ = "U-Factor with SDL        " 
           /* </AWD006> */

           /* <AWD007> */
               if type% = 15% then eg_group_d$ = "U-Factor Florida Contour " 
               if type% = 16% then eg_group_d$ = "Solar Heat Florida Contou" 
               if type% = 17% then eg_group_d$ = "Visable Tr Florida Contou" 
           /* </AWD007> */

               gosub dataload
               if rec% = 0% then return
                  fieldnr% = 6%
        return
L50130:     errormsg$ = "(Error) - Invalid Energy Star Group code?"
            gosub error_prompt
            init(" ") eg_group$, eg_group_d$
        return

L50200: REM Residential Data                      eg_res$
            if eg_res$ <> " " then goto L50220                /* (EWD002) */
               eg_res$ =  "0.00"
L50220:     eg_res = 0.0 
            convert eg_res$ to eg_res, data goto L50210 

            convert eg_res to eg_res$, pic(####.####-)

        return
L50210:     errormsg$ = "(Error) - Invalid residential Value?"
            gosub error_prompt
            init(" ") eg_res$
            eg_res = 0.0
        return

L50300: REM Non-Residential Data                   eg_nonres$
            if eg_nonres$ <> " " then goto L50320                /* (EWD002) */
               eg_nonres$ =  "0.00"
L50320:     eg_nonres = 0.0 
            convert eg_nonres$ to eg_nonres, data goto L50310 

            convert eg_nonres to eg_nonres$, pic(####.####-)

            if type% = 3% then return
               if edit% = 2% then return

               fieldnr% = 5%
        return
L50310:     errormsg$ = "(Error) - Invalid Non-Residential Value?"
            gosub error_prompt
            init(" ") eg_nonres$
            eg_nonres = 0.0
        return

L50400: REM Design Pressure                        eg_dp$
            eg_dp% = 0%
            convert eg_dp$ to eg_dp%, data goto L50410

            convert eg_dp% to eg_dp$, pic(##)
            
/* CR2520 */
            if type% = 3% and (eg_group$ = "002" or eg_group$ = "003")~
               and errorkey% <> 0% then fieldnr% = 6%
/*(AWD009) CR2520 */                                            
            if type% = 3% and eg_group$ >= "020" and eg_group$ <> "022" ~
               and errorkey% <> 0% then fieldnr% = 6%                                            
        return
L50410:     errormsg$ = "(Error) - Invalid Design Pressure Value?"
            gosub error_prompt
            init(" ") eg_dp$
            eg_dp% = 0%  
        return

L50500: REM Florida Product Approval Code          eg_fl$
            init(" ") sku_key$
            sku_key$ = "X_LO" & eg_fl$
REM         read #3, key = sku_key$, using AWDSKUXR, sku_key$, eod goto notfnd
AWDSKUXR:   FMT CH(16)
            return

notfnd:     errormsg$ = "(Error) - Invalid Florida Approval Code?"
            gosub error_prompt
            init(" ") eg_fl$
        return

        deffn'152(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L51010,       /* From Model Code         */~
                              L52010        /* Too Model Code          */
        return

L51010: REM From Model Code                      bg_model$, bg_model_d$
            eg_model$ = bg_model$
            gosub lookup_model
            if model% = 0% then goto L51020
            bg_model_d$ = desc$
            if bg_model$ = ed_model$ then goto L51020

        return
L51020:     errormsg$ = "(Error) Invalid Copy From Model Code?"
            gosub error_prompt
            init(" ") bg_model$, bg_model_d$
        return
  
L52010: REM Too Model Code                        ed_model$, ed_model_d$
            eg_model$ = ed_model$
            gosub lookup_model
            if model% = 0% then goto L52020
            ed_model_d$ = desc$
REM            if bg_model$ >= ed_model$ then goto L52020    /* (EWD004) */
                                                             /* (EWD004) */
            if bg_model$ >= ed_model$ and delete_data% <> 1% then goto L52020
            if bg_model$ > ed_model$ and delete_data% = 1% then goto L52020
        return
L52020:     errormsg$ = "(Error) Invalid Copy Too Model Code?"
            gosub error_prompt
            init(" ") ed_model$, ed_model_d$
        return
  
        deffn'153(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L53010,       /* Beginning Model Code    */~
                              L54010        /* Ending Model Code       */
        return

L53010: REM Beginning Model Code                  bg_model$, bg_model_d$
            if len(bg_model$) > 2 then goto L53030
L53020:        bg_model$ = "ALL"
               bg_model_d$ = "All Models              "
               return
 
L53030:     if bg_model$ = "ALL" then goto L53020
            eg_model$ = bg_model$
            gosub lookup_model
            if model% = 0% then goto L53040
            bg_model_d$ = desc$

        return
L53040:     errormsg$ = "(Error) Invalid Beginning Model Code?"
            gosub error_prompt
            init(" ") bg_model$, bg_model_d$
        return
  
L54010: REM Too Model Code                        ed_model$, ed_model_d$
            if len(ed_model$) > 2 then goto L54030
L54020:        ed_model$ = bg_model$
               ed_model_d$ = bg_model_d$
               return

L54030:     if ed_model$ = "ALL" then goto L54020
            eg_model$ = ed_model$
            gosub lookup_model
            if model% = 0% then goto L54040
            ed_model_d$ = desc$
            if bg_model$ > ed_model$ then goto L54040
            if bg_model$ = "ALL" and ed_model$ <> "ALL" then goto L54040
            if ed_model$ = "ALL" and bg_model$ <> "ALL" then goto L54040

        return
L54040:     errormsg$ = "(Error) Invalid Ending Model Code?"
            gosub error_prompt
            init(" ") ed_model$, ed_model_d$
        return
  
        REM *************************************************************~
            *           I M A G E   S T A T E M E N T S                 *~
            *************************************************************

L55040: %+-----------------------------------------------------------~
        ~-----------------------------------------------------------+
                                                 /* COLUMN 1 HEADER */
L55080: %!########## ########                    ####################~
        ~####################                             Page : ###!

L55100: %!                                       ####################~
        ~####################                                       !

L55120: %!-----------------------------------------------------------~
        ~-----------------------------------------------------------!

L55160: %!Model!<-------- Description ------->!Group!<  Descript   >!~
        ~Residential/Width!Non-Residential/Height!Design Pressure   !

L55200: %!-----!------------------------------!-----!---------------!~
        ~-----------------!----------------------!------------------!

L55240: %! ### !##############################! ### !###############!~
        ~    ##########   !    ##########        !       ###        !

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************
 
        select_printer
            page_no% = 0%
            lcnt%    = 99%
            prt_title$ = "**Energy Star Master Database Report**"
            call "FMTTITLE" (prt_title$, " ", 12%)
            date$ = date  :  call "DATFMTC" (date$)
            call "TIME" (rpt_time$)
            call "COMPNAME" (12%, company$, f1%(2%))
            call "SETPRNT" ("EWDENG", " ", 0%, 0%)
            select printer (134)
        return

        close_printer
            call "SETPRNT" ("EWDENG", " ", 0%, 1%)
        return

        generate_report
            call "SHOSTAT" ("Creating Energy Star Data Report")
            gosub select_printer
            eg_key$ = all(hex(00))
            if bg_model$ = "ALL" then goto generate_next 
               str(eg_key$,1%,3%) = bg_model$
        generate_next
            read #1,key > eg_key$, eod goto generate_done
/*@@@*/     get #1, using L35000, eg_model$,         /* Model Code     */~
                                  eg_group$,         /* Group Code     */~
                                  eg_res,            /* Residential Val*/~
                                  eg_nonres,         /* Non-Residential*/~
                                  eg_dp$,            /* Design Pressure*/~
                                  eg_fl$,            /* Design Pressure*/~
                                  eg_fill$           /* Filler Area    */

            str(eg_key$,1%,3%) = eg_model$
            str(eg_key$,4%,3%) = eg_group$
            if ed_model$ = "ALL" then goto L60010
               if str(eg_key$,1%,3%) > ed_model$ then goto generate_done

L60010:     convert eg_res to eg_res$, pic(####.####-)

            convert eg_nonres to eg_nonres$, pic(####.####-)
 
            gosub print_detail
            goto generate_next

        generate_done
            print using L55040
            gosub close_printer
        return clear all
        goto inputmode

        print_header
          page_no% = page_no% + 1%
          print page
          print using L55040
          print using L55080,date$, rpt_time$,prt_title$,page_no%
          print using L55100, company$
          print using L55120
          print using L55160
          lcnt% = 5%
        return

        print_detail
          if lcnt% < 60% then goto L60600
             if lcnt% <> 99% then print using L55040
             gosub print_header
L60600:   
                                         /* Check for 'U' Factor       */
          if str(eg_group$,1%,1%) = "0" then type% = 1%
                                         /* Check for Heat Coefficient */
          if str(eg_group$,1%,1%) = "1" then type% = 2%
                                         /* Check for Design Pressure  */
          if str(eg_group$,1%,3%) = "000" then type% = 3%
                                         /* Check for Visable Transmittance (EWD001) */
          if str(eg_group$,1%,1%) = "2" then type% = 4%

                                         /* Check for Air Leakage           (EWD002) */
          if str(eg_group$,1%,1%) = "3" then type% = 5%

                                         /* Check for Solar Heat with 5/8   (EWD003) */
          if str(eg_group$,1%,1%) = "4" then type% = 6%
                                         /* Check for Solar Hear with 1 in  (EWD003) */
          if str(eg_group$,1%,1%) = "5" then type% = 7%
                                         /* Check for Visable Tr with 5/8   (EWD003) */
          if str(eg_group$,1%,1%) = "6" then type% = 8%
                                         /* Check for Visable Tr with 1 in  (EWD003) */
          if str(eg_group$,1%,1%) = "7" then type% = 9%

          if type% = 1% then eg_group_d$ = "U-Factor Group "
          if type% = 2% then eg_group_d$ = "Solar Heat Gain"
          if type% = 3% then eg_group_d$ = "Design Pressure"
          if type% = 3% and eg_group$ <> "000" then                   ~
                                      eg_group_d$ = "NTX Design Pressure"
          if type% = 4% then eg_group_d$ = "Visable Transm"  /* (EWD001) */  
          if type% = 5% then eg_group_d$ = "Air Leakage Group        "  /* (EWD002) */
          if type% = 6% then eg_group_d$ = "Solar Heat with 5/8      "  /* (EWD003) */
          if type% = 7% then eg_group_d$ = "Solar Heat with 1 in     "  /* (EWD003) */
          if type% = 8% then eg_group_d$ = "Visable Trans with 5/8   "  /* (EWD003) */
          if type% = 9% then eg_group_d$ = "Visable Trans with 1 in  "  /* (EWD003) */                  

          gosub lookup_model
          eg_model_d$ = desc$

          print using L55200
          print using L55240, eg_model$, eg_model_d$, eg_group$, eg_group_d$,~
                              eg_res$, eg_nonres$, eg_dp$

          lcnt% = lcnt% + 2%
        return

        error_prompt
           comp% = 2%
           hdr$     = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        open_error            
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return
                              
        lookup_model
            init(" ") readkey$, desc$
            model% = 0%
            str(readkey$,1%,9%)   = "MODEL    "
            str(readkey$,10%,15%) = eg_model$  
            read #2,key = readkey$, using L60100, desc$, eod goto L60110
L60100:        FMT POS(24), CH(30)
            model% = 1%
L60110: return

        lookup_group
            init(" ") readkey$, desc$
            group% = 0%
            str(readkey$,1%,9%)   = "PLNENERGY"
            str(readkey$,10%,15%) = eg_group$
            read #2,key >= readkey$,using L60200, readkey$,desc$,         ~
                                                 eod goto L60210
L60200:        FMT CH(24), CH(30)
            if str(readkey$,1%,9%)  <> "PLNENERGY" then goto L60210
            if str(readkey$,10%,3%) <> eg_group$   then goto L60210
            group% = 1%
L60210: return
 
        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            end
