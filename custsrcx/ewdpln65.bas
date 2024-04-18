REM         *************************************************************~
            *                                                           *~  
            *  Program Name      - EWDPLN65                             *~
            *  Creation Date     - 12/14/98                             *~
            *  Last Modified Date- 11/22/06                             *~
            *  Written By        - Brian W. Sanders                     *~
            *                                                           *~
            *  Description       - Glass Rack Labels Data Gen & Print.  *~
            *                      Prod. Date, Rack No. & Department    *~
            *                      Range Specified.                     *~
            *                                                           *~
            *  Code Tables Used  - PLAN DEPT                            *~
            *                                                           *~
            *  Subroutine Used   - EWDPLA65 (Print Glass Rack Labels)   *~
            *                                                           *~
            *  Special Comments  - Outputs to a file with one record per*~
            *                      label. <PF9> used to print labels.   *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/14/98 ! (New) Program                            ! BWS *~
            * 10/17/02 ! (EWD001) - Mod to put Sunclean 'S' code  ! CMG *~
            *          !            on Rack Labels.               !     *~
            * 11/22/06 ! (AWD002) - mod to allow to print All,    ! CMG *~  
            *          !       DS or Temp                         !     *~
            *10/01/2007! (AWD003) - mod to allow to print DS.     ! DES *~
            *10/31/2007! (AWD004) - mod to optimise labels        ! DES *~
            *08/06/2008! (AWD005) - mod for SDL "B"               ! DES *~
            *11/23/2009! (AWD006) - mod for Ultra                 ! CMG *~
            *11/13/2010! (AWD007) - mod for VPD                   ! DES *~
            *03/24/2011! (AWD008) - mod for intercepts            ! CMG *~
            *05/07/2012! (AWD009) - mod for 8900, file #20        ! DES *~
            *          ! replaces 5,7-19                          !     *~
            *11/19/2014! (AWD010) mod for TX to allow all glass   ! PWW *~
            *          !    to be on a top-rack only.             !     *~
            *09/16/2016! CR00606  mod to add "L" for laminate.    ! PWW *~
            *10/03/2016! CR00604 mod to seperate Tempered from DS.! PWW *~
            *06/20/2017! CR00950 intercepts setting incorrectly   ! RDB *~
            *06/20/2017! CR00950 sk_loc setting for TX dept 059   ! RDB *~
            *08/19/2017! CR00605 New triple pain rack label       ! RDB *~
            *05/03/2019! CRPROJ  Add 029, 070, 071 to TT BB       ! RDB *~
            *05/16/2019! CR2019  Smaller print for 1100 series    ! RDB *~
            *07/25/2019! CR2140  Add 072 to TT BB & edits         ! RDB *~
            *09/11/2019! CR2218  Change 072 to 027                ! RDB *~
            *10/26/2019! CR2296  New NC STC rack label            ! RDB *~
            *02/07/2020! CR2412  Add dept 058 to TT BB            ! RDB *~            
            *************************************************************
                    
        dim                                                              ~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
            sc_dept$3, sc_dept_d$30,     /* Department Code & Descr    */~
            sc_prddate_fr$10,            /* Production Date            */~
            sc_prddate_to$10,            /*       Range                */~
            sc_rack_fr$4, sc_rack_to$4,  /* Rack No. Range             */~
            sc_glass$2,                  /* (AWD002) - ALL, DS, OR TEMP*/~
            sc_glass_d$30,               /* Selection Description      */~
            sc_lbl$1,                    /* A or P for 1100 series lbl */~
            filler$64,                   /* padding at end of record   */~
            rk_rack_code$3,              /* rack code                  */~
            rk_prddate$10,               /* Production Date from RK    */~
            rk_key$14,                   /* Record Key (Alt 1) from RK */~
            rk_bc$9,                     /* Glass Barcode              */~
            rk_seq$5,                    /* Production Sequence No.    */~
            rk_dept$3,                   /* Department Number          */~
/*CR00606*/ rk_glass$2,                  /* Glass Code                 */~
            rk_model$3,                  /* Model Number               */~
            rk_flg$(10)1,                /* Special Glass Rack Flags   */~
            rk_rack$4,                   /* Glass Rack No.             */~
            rk_loc$1,                    /* Rack Location (0=Top,1=Bot)*/~
            rk_slot$2,                   /* Rack Slot Location         */~
            rk_type$1,                   /* Rack Type (0=T/B,1=25,2=20)*/~
            lb_bc$(50)9,                 /* Glass Barcode - Label Data */~
            lb_seq$(50)5,                /* Prod. Seq. No. - Label Data*/~
            lb_flg1$(50)1,               /* Spcl GR Flag #1 - Lbl Data */~
            lb_flg2$(50)1,               /* Spcl GR Flag #2 - Lbl Data */~
            lb_flg3$(50)1,               /* Spcl GR Flag #3 - Lbl Data */~
            lb_flg4$(50)1,               /* Spcl GR Flag #4 - Lbl Data */~
            lb_flgx$(50)6,               /* Spcl GR Flags 5 thru 10    */~
            last_rack$4,                 /* Previous Glass Rack No.    */~
            first_rack$4,                /* First Rack AWD010          */~
            last_prddate$10,             /* Previous Production Date   */~
            last_dept$3,                 /* Previous Department No.    */~
            last_intercept$2, ~
            prt_flag$3,                  /* Labels Printed Flag        */~
            rec_key$14,                  /* Record Key from APCGLRKL   */~
            file$8,                      /* Glass Rack Data File Name  */~
            lib$8,                       /* Glass Rack Data Library    */~
            vol$6,                       /* Glass Rack Data Volume     */~
            hdr$40, msg$(3%)79,          /* Askuser Messages           */~
            time$8,                      /* System time                */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            edtmessage$79,               /* Edit screen message        */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            message$256,                 /* Informational Message      */~
            lfac$(20%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            pfkeys$32,                   /* PF Key Hex Values          */~
            userid$3                     /* Current User Id            */

        dim rk_intercept$2, lb_intercept$2

        dim schema$8                     /* Schema                     */

        dim logmsg$256

        dim scr$(10%)60

        dim f2%(64%),                    /* = 0 if the file is open    */~
            f1%(64%),                    /* = 1 if READ was successful */~
            fs%(64%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(64%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$40, pname$21
            apc$   = "(EWD) Generate/Print Glass Rack Labels"
            pname$ = "EWDPLN65 - Rev: R7.00"

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
            * #4  ! GENCODES ! System Master Code Table Files           *~
            * #6  ! APCGLRKL ! Glass Rack Label Data File               *~
            * #10 ! AWDPLPTO ! VPD Rack Labels                  (AWD007)*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #4,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24
 
            select #6,  "APCGLRKL",                                      ~
                        varc,     indexed,  recsize = 1280,              ~
                        keypos =    1, keylen =   14

            select #20, "AWDPLNRK",                                      ~
                        varc,     indexed,  recsize =   128,             ~
                        keypos =    1, keylen =  12,                     ~
                        alt key  1, keypos = 13, keylen =  14

            call "SHOSTAT" ("Opening Files, One Moment Please")

            call "OPENCHCK" (#4,  fs%(4%),  f2%(4%),  0%,  rslt$(4%))
            call "OPENFILE" (#6,  "VALID",  f2%(6%),  rslt$(6%), " ")
                if f2%(6%) = 0% then gosub file_option
            call "OPENCHCK" (#6,  fs%(6%),  f2%(6%), 300%, rslt$(6%))
            call "GETNAMES" addr(#6, file$, lib$, vol$)
            call "OPENCHCK" (#20, fs%(20),  f2%(20),  0%,  rslt$(20))


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
            prt_flag$ = " "

            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #4, schema_err%)
/* screen change for NC STC CR2296  */
            scr$(1%) = "****************************************"
            scr$(2%) = "* (1) - Tin        (7) DS-All          *"
            scr$(3%) = "* (2) - DuraSeal   (8) Temp-All        *"
            scr$(4%) = "* (3) - DuraLite   (19) 8900 triple    *"
            if schema% = 1% then ~
              scr$(5%) = "* (4) - Super      (22) NC STC         *" ~
            else ~
              scr$(5%) = "* (4) - Super                          *"             
            scr$(6%) = "* (5) - Thin                           *"
            scr$(7%) = "* (6) - Ultra                          *"
            scr$(8%) = "*                                      *"
            scr$(9%) = "* (20)- VPD Glass Rack Labels Stock    *"
            scr$(10%) = "* (21)- VPD Glass Rack Labels Custom   *"
            scr$(12%)= "****************************************"
/*CR00604 + */
/*  if schema% = 2% then goto skip_menu_nc     */

REM  scr$(1%) = "************************************************************"
REM  scr$(2%) = "* (1) - Tin        (7) DS- Tin         (13) Temp- Tin      *"
REM  scr$(3%) = "* (2) - DuraSeal   (8) DS- DuraSeal    (14) Temp- DuraSeal *"
REM  scr$(4%) = "* (3) - DuraLite   (9) DS- DuraLite    (15) Temp- DuraLite *"
REM  scr$(5%) = "* (4) - Super     (10) DS- Super       (16) Temp- Super    *"
REM  scr$(6%) = "* (5) - Thin      (11) DS- Thin        (17) Temp- Thin     *"
REM  scr$(7%) = "* (6) - Ultra     (12) DS- Ultra       (18) Temp- Ultra    *"
REM  scr$(8%) = "*                                      (19) 8900 Triple    *"
/* <AWD007> */
REM  scr$(9%) = "* (20)- VPD Glass Rack Labels Stock                        *"
REM  scr$(10%)= "* (21)- VPD Glass Rack Labels Custom                       *"
REM  scr$(12%)= "************************************************************"

  skip_menu_nc
/*CR00604 - */

        REM *************************************************************~
            *       I N P U T   M O D E   M A I N   P R O G R A M       *~
            *-----------------------------------------------------------*~
            * Handles normal input for data entry screens.              *~
            *************************************************************

        inputmode
            gosub initialize_variables

            for fieldnr% = 1% to   5%
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
            if fieldnr% < 1% or fieldnr% > 5% then editpg1
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
         "Enter a Production Date Range.                               ",~
         "Enter a Specific Department Code or 'ALL'.                   ",~
         "Enter a Glass Rack No. Range or 'ALL'.                       ",~
         "Enter What to Print 'A'll, 'T'emp, 'D'S.                     ",~
         "Enter Atrium 'A' or PlyGem 'P'                               "

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
                      sc_prddate_fr$, sc_prddate_to$, sc_rack_fr$,       ~
                      sc_rack_to$,                                       ~
                      last_rack$, last_prddate$, last_dept$, sc_glass$,  ~
                      sc_glass_d$, first_rack$, sc_lbl$     /*AWD010*/
            sc_lbl$ = "A"
            lbl% = 0%

          clear_label_data
            init (" ") lb_flgx$(), lb_flg1$(), lb_flg2$(),               ~
                       lb_flg3$(), lb_flg4$()
            init ("0") lb_bc$()
            init ("X") lb_seq$()
            s% = 1%
        cnt% = 0%

        return


        REM *************************************************************~
            *           L O A D   D A T A   F R O M   F I L E           *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************

        dataload
REM         +---------------------------------------------------------------+
REM         | AWD009 convert to use file 20 for all input                   |
REM         +---------------------------------------------------------------+
            ff$ = "   "
            if sc_glass$ = "01" then ff$ = "001"
            if sc_glass$ = "02" then ff$ = "002"
            if sc_glass$ = "03" then ff$ = "003"
            if sc_glass$ = "04" then ff$ = "004"
            if sc_glass$ = "05" then ff$ = "005"
            if sc_glass$ = "06" then ff$ = "006"
            if sc_glass$ = "07" then ff$ = "007"
            if sc_glass$ = "08" then ff$ = "008"
            if sc_glass$ = "09" then ff$ = "009"
            if sc_glass$ = "10" then ff$ = "010"
            if sc_glass$ = "11" then ff$ = "011"
            if sc_glass$ = "12" then ff$ = "012"
            if sc_glass$ = "13" then ff$ = "013"
            if sc_glass$ = "14" then ff$ = "014"
            if sc_glass$ = "15" then ff$ = "015"
            if sc_glass$ = "16" then ff$ = "016"
            if sc_glass$ = "17" then ff$ = "017"
            if sc_glass$ = "18" then ff$ = "018"
            if sc_glass$ = "19" then ff$ = "019"
            if sc_glass$ = "20" then ff$ = "020"
            if sc_glass$ = "21" then ff$ = "021"
            if sc_glass$ = "22" then ff$ = "022"       /* CR2296 */
            
            call "SHOSTAT" ("Generating Label Data...")
            call "DATUFMTC" (sc_prddate_fr$)
            call "DATUFMTC" (sc_prddate_to$)
            rk_key$ = all(hex(00))
            str(rk_key$,1,6%) = sc_prddate_fr$
            init(" ") last_rack$, last_prddate$, last_dept$
            init(" ") first_rack$                        /*AWD010*/
            s% = 1%
            p_s% = 0%: rk_begin% = 0%                   /*AWD010*/
        cnt% = 0%
          load_next_rec 
REM         +---------------------------------------------------------------+
REM         | rk_rack_code$ is new to AWD009 for merged file                |
REM         +---------------------------------------------------------------+
            read #20%, key 1 > rk_key$, using L53050, rk_rack_code$, rk_bc$, ~
                rk_key$, rk_seq$, rk_dept$, rk_flg$(), rk_model$, rk_glass$, ~
                rk_stock$, rk_intercept$, filler$, eod goto load_done
            
            if cnt% = 0% then last_intercept$ = rk_intercept$ 
/*CR00604 + */
            if sc_glass$ <> "07" then goto nds
              if rk_rack_code$ >= "007" and rk_rack_code$ <= "012"         ~
                                           then goto conts
              goto load_next_rec
          nds
            if sc_glass$ <> "08" then goto ntemp
              if rk_rack_code$ >= "013" and rk_rack_code$ <= "018"         ~
                                           then goto conts
              goto load_next_rec

          ntemp
            if ff$ <> rk_rack_code$ then load_next_rec
          conts
/*CR00604 - */
            rk_prddate$ = str(rk_key$,1,6%)
            rk_rack$ = str(rk_key$,7%,4%)
REM ??
            if ff$ = "007" then str(rk_rack$,1,1) = sc_glass$
            rk_loc$  = str(rk_key$,11%,1%)
            rk_slot$ = str(rk_key$,12%,2%)
            rk_type$ = str(rk_key$,14%,1%)
            if rk_prddate$ > sc_prddate_to$ then goto load_done
            if (rk_rack$ < sc_rack_fr$ or rk_rack$ > sc_rack_to$)        ~
                and sc_rack_fr$ <> "ALL" then goto load_next_rec
            if rk_dept$ <> sc_dept$ and sc_dept$ <> "ALL"                ~
                then goto load_next_rec
/* CR2019 - added 072 dept on july 25 CR2218 */                
            if sc_lbl$ = "P" and (rk_dept$ <> "029" and rk_dept$ <> "070" and ~
               rk_dept$ <> "071" and rk_dept$ <> "027") then goto load_next_rec
                 
            if sc_lbl$ = "A" and (rk_dept$ = "029" or rk_dept$ = "070" ~
             or rk_dept$ = "071")   then goto load_next_rec
             
            if sc_lbl$ = "A" and (schema% = 2% and rk_dept$ = "027") then ~
                goto load_next_rec
                
            lb_intercept$ = rk_intercept$ 

/* (AWD002) */
REM!             IF SC_GLASS$ = "2" AND RK_FLG$(4%) <> "D" AND RK_FLG$(3%) <> "T" ~
                                                    THEN GOTO LOAD_NEXT_REC
/* (AWD002) */
/* <AWD003> */
REM!            IF SC_GLASS$ = "3" AND RK_FLG$(4%) <> "D"                        ~
                                                    THEN GOTO LOAD_NEXT_REC
REM!            IF SC_GLASS$ = "4" AND RK_FLG$(3%) <> "T"                        ~
                                                    THEN GOTO LOAD_NEXT_REC
/* </AWD003> */
/* (AWD006) */
REM!            IF SC_GLASS$ = "5" AND RK_FLG$(7%) <> "Y"                        ~
                                                    THEN GOTO LOAD_NEXT_REC
REM         IF SC_GLASS$ = "5" AND RK_FLG$(4%) = "D"                        ~
                                                    THEN GOTO LOAD_NEXT_REC
REM         IF SC_GLASS$ = "5" AND RK_FLG$(5%) = "T"                        ~
                                                    THEN GOTO LOAD_NEXT_REC

REM!            IF SC_GLASS$ = "6" AND RK_FLG$(4%) <> "D" AND RK_FLG$(3%) <> "T" ~
                                                    THEN GOTO LOAD_NEXT_REC
REM!            IF SC_GLASS$ = "6" AND RK_FLG$(7%) <> "Y"                        ~
                                                    THEN GOTO LOAD_NEXT_REC
/* (/AWD006) */
REM  !            IF SC_GLASS$ = "7" AND RK_STOCK$ <> "2"                        ~
                                                    THEN GOTO LOAD_NEXT_REC
REM  !            IF SC_GLASS$ = "8" AND RK_STOCK$ <> "1"                        ~
                                                    THEN GOTO LOAD_NEXT_REC

/*CR00604   I don't believe any of this logic(below) is needed as it is already */
/*            done in APCPL41C.  PWW                                            */
/*          if sc_glass$="01" and rk_intercept$ <> "01" then goto load_next_rec*/
/*          if sc_glass$="07" and rk_intercept$ <> "01" then goto load_next_rec*/
/*CR00604   if sc_glass$="13" and rk_intercept$ <> "01" then goto load_next_rec*/

/*          if sc_glass$="02" and rk_intercept$ <> "02" then goto load_next_rec*/
/*          if sc_glass$="08" and rk_intercept$ <> "02" then goto load_next_rec*/
/*    if sc_glass$="14" and rk_intercept$ <> "02" then goto load_next_rec*/

/*          if sc_glass$="03" and rk_intercept$ <> "03" then goto load_next_rec*/
/*          if sc_glass$="09" and rk_intercept$ <> "03" then goto load_next_rec*/
/*CR00604   if sc_glass$="15" and rk_intercept$ <> "03" then goto load_next_rec*/

/*          if sc_glass$="04" and rk_intercept$ <> "04" then goto load_next_rec*/
/*          if sc_glass$="10" and rk_intercept$ <> "04" then goto load_next_rec*/
/*CR00604   if sc_glass$="16" and rk_intercept$ <> "04" then goto load_next_rec*/

/*          if sc_glass$="05" and rk_intercept$ <> "05" then goto load_next_rec*/
/*          if sc_glass$="11" and rk_intercept$ <> "05" then goto load_next_rec*/
/*CR00604   if sc_glass$="17" and rk_intercept$ <> "05" then goto load_next_rec*/

/*          if sc_glass$="06" and rk_intercept$ <> "06" then goto load_next_rec*/
/*          if sc_glass$="12" and rk_intercept$ <> "06" then goto load_next_rec*/
/*CR00604   if sc_glass$="18" and rk_intercept$ <> "06" then goto load_next_rec*/

/*CR00604 + */
        goto NC_racks   /* TX wants same changes as NC    */
        if schema% <> 2 then goto NC_racks

          TX_racks
            if sc_glass$ = "07" and rk_flg$(4%) <> "D" and rk_flg$(3%) <> "T" ~
                                                    then goto load_next_rec
            if sc_glass$ = "08" and rk_flg$(4%) <> "D" and rk_flg$(3%) <> "T" ~
                                                    then goto load_next_rec
            if sc_glass$ = "09" and rk_flg$(4%) <> "D" and rk_flg$(3%) <> "T" ~
                                                    then goto load_next_rec
            goto skip_NC_racks

          NC_racks

REM         if sc_glass$ = "07" and rk_flg$(4%) <> "D" then goto load_next_rec
REM         if sc_glass$ = "08" and rk_flg$(3%) <> "T" then goto load_next_rec
REM         if sc_glass$ = "09" and rk_flg$(4%) <> "D" then goto load_next_rec
REM         if sc_glass$ = "10" and rk_flg$(4%) <> "D" then goto load_next_rec
REM         if sc_glass$ = "11" and rk_flg$(4%) <> "D" then goto load_next_rec
REM         if sc_glass$ = "12" and rk_flg$(4%) <> "D" then goto load_next_rec

REM         if sc_glass$ = "13" and rk_flg$(3%) <> "T" then goto load_next_rec
REM         if sc_glass$ = "14" and rk_flg$(3%) <> "T" then goto load_next_rec
REM         if sc_glass$ = "15" and rk_flg$(3%) <> "T" then goto load_next_rec
REM         if sc_glass$ = "16" and rk_flg$(3%) <> "T" then goto load_next_rec
REM         if sc_glass$ = "17" and rk_flg$(3%) <> "T" then goto load_next_rec
REM         if sc_glass$ = "18" and rk_flg$(3%) <> "T" then goto load_next_rec
/*CR00604 - */      

REM         IF SC_GLASS$ = "15" AND RK_INTERCEPT$ <> "01"                   ~
                                                    THEN GOTO LOAD_NEXT_REC
REM         IF SC_GLASS$ = "16" AND RK_INTERCEPT$ <> "02"                   ~
                                                    THEN GOTO LOAD_NEXT_REC

          skip_NC_racks
          /* <AWD004> */
/*AWD010 +  CR2218 CR2412 */
            if schema% <> 2 or (schema% = 2% and rk_dept$ = "059") or ~
              (schema% = 2% and rk_dept$ = "058") or ~
              (schema% = 2% and rk_dept$ = "029") or ~
              (schema% = 2% and rk_dept$ = "070") or ~
              (schema% = 2% and rk_dept$ = "071") or ~
              (schema% = 2% and rk_dept$ = "027")    ~              
                  then goto not_tx
            convert rk_slot$ to s%, data goto load_next_rec
            if (rk_prddate$ <> last_prddate$ and last_prddate$ <> " ") ~
              or (rk_dept$ <> last_dept$ and last_dept$ <> " ")          ~
              or (rk_begin% = 1% and s% = 1%) then gosub dataput
            if s% = 1% and p_s% = 25% then gosub tx_rk_change
            p_s% = p_s% + 1%
            if rk_begin% = 1% then s% = s% + 25%
            if s% = 1% then first_rack$ = rk_rack$
            goto skip_nc

         not_tx
            first_rack$ = last_rack$
/*AWD010 - */
            if (rk_rack$ <> last_rack$ and last_rack$ <> " ")            ~
              or (rk_prddate$ <> last_prddate$ and last_prddate$ <> " ") ~
              or (rk_dept$ <> last_dept$ and last_dept$ <> " ")          ~
              then gosub dataput
 
            convert rk_slot$ to s%, data goto load_next_rec
             if rk_loc$ = "1" and rk_type$ = "0" and sc_glass$ <> "19"  ~ 
                            then s% = s% + 25%                   /* CR605 */
         skip_nc
            if s% > 50% then load_next_rec

 /* top barcode ends with "0", bottom with "5" */
 /* dept 047 has 20 not 25 slots per row */
REM     TB$ = "0"
REM     IF RK_LOC$ = "1" AND RK_TYPE$ = "0" THEN TB$ = "5"
REM     TB$ = STR(RK_BC$,9,1)
REM     IF TB$ > "4" AND S% < 26% THEN S% = 26%
REM     IF RK_DEPT$ = "047" AND TB$ < "5" AND S% < 21% THEN GOTO L20499
REM     IF RK_DEPT$ = "047" AND TB$ > "4" AND S% < 41% THEN GOTO L20499
REM     IF TB$ < "5" AND S% < 26% THEN GOTO L20499
REM     IF TB$ > "4" AND S% < 51% THEN GOTO L20499

REM         GOSUB DATAPUT
L20499:
            rk_bc% = 0%
            convert rk_bc$ to rk_bc%, data goto L20500
L20500:     convert rk_bc% to rk_bc$, pic(000000000)

            lb_bc$(s%)    = rk_bc$
            lb_seq$(s%)   = rk_seq$
            lb_flg1$(s%)  = rk_flg$(1%)
REM         LB_FLG2$(S%)  = RK_FLG$(2%)                     /*  (EWD001)  */
            lb_flg2$(s%)  = rk_flg$(5%)                /* ????? */
            lb_flg3$(s%)  = rk_flg$(3%)
            lb_flg4$(s%)  = rk_flg$(4%)
            str(lb_flgx$(s%),5%,1%)  = rk_flg$(2%)    /* CR2296 reuse field */
            str(lb_flgx$(s%),2,1) = rk_flg$(6%)       /* AWD005 */
            str(lb_flgx$(s%),3,1) = rk_flg$(7%)       /* (AWD006) */
 
            if rk_flg$(7%) = "Y" then lb_flg2$(s%)  = rk_flg$(7%)
/*CR00606 + */
            gosub check_laminate  
            if glass% = 1% then str(lb_flgx$(s%),5%,1%) = "L"
/*CR00606 - */

            if (last_rack$ = "     ") then      ~
                last_rack$    = rk_rack$
            last_rack$    = rk_rack$
            last_prddate$ = rk_prddate$
            last_dept$    = rk_dept$
/*CR2019*/  last_intercept$ = rk_intercept$
REM         S% = S% + 1%
            cnt% = cnt% + 1%

            goto load_next_rec


        load_done
            gosub dataput
            gosub load_results
            goto inputmode

        REM *************************************************************~
            *          S T U F F   D A T A   I N T O   F I L E          *~
            *-----------------------------------------------------------*~
            * Stuffs data from Program Variables into File Record Area. *~
            *************************************************************

        dataput
REM         if s% = 0% then return          /* Nothing to write... */
            if cnt% = 0% then return          /* Nothing to write... */

            call "DATFMTC" (last_prddate$)
            write #6, using L53200, last_prddate$,last_rack$,last_dept$, ~
                " ", lb_bc$(), lb_seq$(), lb_flg1$(), lb_flg2$(),        ~
                lb_flg3$(), lb_flg4$(), lb_flgx$(), last_intercept$, ~
                first_rack$, sc_lbl$, eod goto L21200, data goto L21200
        /* <AWD004> + */
            last_rack$ = rk_rack$
            first_rack$ = "    "
            p_s% = 0%
            gosub tx_rk_change
        /* </AWD004> - */
            lbl% = lbl% + 1%
L21200:     gosub clear_label_data
        return

/* AWD010 + */
        tx_rk_change
            if rk_begin% = 0% then rk_begin% = 1%                       ~
               else rk_begin% = 0%
            return
/* AWD010 - */


        REM *************************************************************~
            *               F O R M A T  S T A T E M E N T S            *~
            *************************************************************

L53050:     FMT ch(3),              /* Rack Type                       */~
                ch(9),              /* Glass Barcode          EWDPLNRK */~
                ch(14),             /* Alt Key 1 -- Prod. Date  ch(6)  */~
                                    /*              Rack No.    ch(4)  */~
                                    /*              Rack Loctn  ch(1)  */~
                                    /*              Slot No.    ch(2)  */~
                                    /*              Rack Type   ch(1)  */~
                ch(5),              /* Production Seq. No.             */~
                ch(3),              /* Department No.                  */~
                10*ch(1),           /* Special Flags                   */~
                ch(3),              /* Model No.                       */~
/*CR00606       xx(2),                 Glass Code                      */~
/*CR00606*/     ch(2),              /* Glass Code                      */~
                xx(8),              /* System Time                     */~
                xx(3),              /* User ID                         */~
                xx(1),              /* Filler                          */~
                ch(1),              /* Filler                          */~
                CH(2),              /* Intercept                       */~
                ch(65)              /* Filler                          */


L53200:     FMT ch(10),             /* Prod. Date (Formatted) APCGLRKL */~
                ch(4),              /* Rack No.                        */~
                ch(3),              /* Department No.                  */~
                ch(3),              /* Model No. (Filler)              */~
                50*ch(9),           /* Glass Barcode                   */~
                50*ch(5),           /* Production Seq. No.             */~
                50*ch(1),           /* Special Flag  1                 */~
                50*ch(1),           /* Special Flag  2                 */~
                50*ch(1),           /* Special Flag  3                 */~
                50*ch(1),           /* Special Flag  4                 */~
                50*ch(6),           /* Special Flags 5 - 10 (Reserved) */~
                ch(02),             /* Intercept                       */~
                ch(4),              /* Rack No 2.                      */~
                CH(1),              /* 1100 series label    CR2019     */~
                xx(53)              /* Filler                          */~

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
                                L40160,          /* sc_rack$           */~
              /* (AWD002)  */   L40170,          /* sc_glass$          */~
                                L40160           /* sc_lbl$            */

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
               at (03,02), "Prod. Date Range:",                          ~
               at (03,25), fac(lfac$(1%)), sc_prddate_fr$       , ch(10),~
               at (03,40), fac(lfac$(1%)), sc_prddate_to$       , ch(10),~
                                                                         ~
               at (04,02), "Dept. Code:",                                ~
               at (04,25), fac(lfac$(2%)), sc_dept$             , ch(03),~
               at (04,40), fac(hex(84)),   sc_dept_d$           , ch(30),~
                                                                         ~
               at (05,02), "Glass Rack No. Range:",                      ~
               at (05,25), fac(lfac$(3%)), sc_rack_fr$          , ch(04),~
               at (05,40), fac(lfac$(3%)), sc_rack_to$          , ch(04),~
                                                                         ~
/*<AWD003>*/   at (06,02), "Glass Type:      ",                          ~
               at (06,25), fac(lfac$(4%)), sc_glass$            , ch(02),~
/*</AWD003>*/  at (06,30), fac(hex(84)),   sc_glass_d$          , ch(30),~
                                                                         ~
/* CR2019 */   at (07,02), "Label Type:      ",                          ~
               at (07,25), fac(lfac$(5%)), sc_lbl$              , ch(01),~ 
                                                                         ~
               at (10,10), fac(hex(84)), scr$(1%)               , ch(60),~
               at (11,10), fac(hex(84)), scr$(2%)               , ch(60),~
               at (12,10), fac(hex(84)), scr$(3%)               , ch(60),~
               at (13,10), fac(hex(84)), scr$(4%)               , ch(60),~
               at (14,10), fac(hex(84)), scr$(5%)               , ch(60),~
               at (15,10), fac(hex(84)), scr$(6%)               , ch(60),~
               at (16,10), fac(hex(84)), scr$(7%)               , ch(60),~
               at (17,10), fac(hex(84)), scr$(8%)               , ch(60),~
               at (18,10), fac(hex(84)), scr$(9%)               , ch(60),~
               at (19,10), fac(hex(84)), scr$(10%)              , ch(60),~
               at (20,10), fac(hex(84)), scr$(11%)              , ch(60),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               if keyhit% <>  9 then goto L40410
                  gosub print_labels
                  gosub set_pf1     /* For print flag... */
                  goto L40190

L40410:        if keyhit% <> 15 then goto L40420
                  call "PRNTSCRN"
                  goto L40190

L40420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
        if edit% = 2% then L40610     /*  Input Mode             */
            pf$(1) = "(1)Start Over    (4)Previous Field      " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                       (15)Print Screen"
            pf$(3) = "                                        " &        ~
                     "                       (16)Exit Program"
            pfkeys$ = hex(01ffff04ffffffffffffffffffff0f1000)
            if fieldnr% = 1% then L40570
                str(pf$(3),64)    = " "  :  str(pfkeys$,16,1) = hex(ff)
L40570:     if fieldnr% > 1% then L40590
                str(pf$(1),18,26) = " "  :  str(pfkeys$, 4,1) = hex(ff)
                call "READFDR" addr(file$,lib$,vol$,0%,"RC",rec%,ret%)
                if ret% <> 0% or rec% = 0% then L40590
                    str(pf$(2),41,19) = "(9)PRINT LABELS " & prt_flag$
                    str(pfkeys$, 9,1) = hex(09)
L40590:     return

L40610: if fieldnr% > 0% then L40700  /*  Edit Mode - Select Fld */
            pf$(1) = "(1)Start Over                           " &        ~
                     "                                       "
            pf$(2) = "                                        " &        ~
                     "                      (15)Print Screen "
            pf$(3) = "                                        " &        ~
                     "                      (16)GENERATE DATA"
            pfkeys$ = hex(01ffffffffffffffffffffffffff0f1000)
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
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50010,        /* Production Date Range  */~
                              L50050,        /* Department Code        */~
                              L50090,        /* Glass Rack No. Range   */~
               /*(AWD002) */  L50200,        /* What to Print          */~
                              L50300         /* Label types            */

            return

L50010: Rem Enter a Production Date Range          sc_prddate_..$
            call "DATEOKC" (sc_prddate_fr$, sc_dt_fr%, errormsg$)
                if errormsg$ <> " " then goto L50015
            if sc_prddate_to$ = " " then sc_prddate_to$ = sc_prddate_fr$
            call "DATEOKC" (sc_prddate_to$, sc_dt_to%, errormsg$)
                if errormsg$ <> " " then goto L50015
            if sc_dt_fr% <= sc_dt_to% then goto L50015
                errormsg$ = "'TO' Date must be > or = 'FROM' Date."
L50015:     return

L50050: Rem Enter a Valid Department Code          sc_dept$, sc_dept_d$
            init(" ") sc_dept_d$
            if sc_dept$ = " " then sc_dept$ = "ALL"
            if sc_dept$ <> "ALL" then goto L50060
                sc_dept_d$ = "*** All Departments"
                return
L50060:     gosub check_dept
            if dept% = 0% then goto L50080
               sc_dept_d$ = desc$
        return
L50080:     errormsg$ = "(Error) Invalid Department Code"
            gosub error_prompt
            init(" ") sc_dept$, sc_dept_d$
        return

L50090: Rem Enter a Glass Rack No. Range         sc_rack_fr$, sc_rack_to$
            if sc_dept$ = "ALL" then sc_rack_fr$ = "ALL"
            if sc_rack_fr$ <> "ALL" then goto L50095
               sc_rack_to$ = " "
               return
L50095:     if sc_rack_fr$ = " " then goto L50100
            if sc_rack_to$ = " " then sc_rack_to$ = sc_rack_fr$
            convert sc_rack_fr$ to temp%, data goto L50100
            convert temp% to sc_rack_fr$, pic(0000)
            convert sc_rack_to$ to temp%, data goto L50100
            convert temp% to sc_rack_to$, pic(0000)
            if sc_rack_fr$ > sc_rack_to$ then                            ~
                errormsg$ = "'TO' Rack No. must be > or = 'FROM' Rack No."
        return
L50100:     errormsg$ = "(Error) Invalid Rack No."
            return

/* (AWD002) */
L50200: Rem Enter 'A'LL, or 'S'pecial       sc_glass$
            if sc_glass$ = "  " then sc_glass$ = "01"
            convert sc_glass$ to sc_glass%, data goto L50210

            convert sc_glass% to sc_glass$, pic(00)
/*CR00604 + */
            if schema% = 1% and (sc_glass% < 1% or sc_glass% > 22%) then ~
               goto L50210                   /* CR2296 */
            if schema% = 2% and (sc_glass% < 1% or sc_glass% > 21%) then ~
               goto L50210                   /* CR2296 */
            if sc_glass% >=  9% and sc_glass% <= 18% then goto L50210

            if sc_glass% <= 6% then             ~
                 sc_glass_d$ = str(scr$(sc_glass% + 1%),9%,10%)

            if sc_glass% = 20% or sc_glass% = 21% then      ~
                 sc_glass_d$ = str(scr$(sc_glass% - 11%),9%,10%)
REM              sc_glass_d$ = str(scr$(sc_glass% - 7%),9%,10%)

            if sc_glass% >= 7% and sc_glass% <= 8% then      ~
                 sc_glass_d$ = str(scr$((sc_glass% + 1%)-6%),24%,14%)
            if sc_glass% = 19% then             ~
                 sc_glass_d$ = str(scr$(sc_glass% - 9%),9%,10%)

/*          if sc_glass% > 12% and sc_glass% < 20% then      ~
                 sc_glass_d$ = str(scr$((sc_glass% + 1%)-12%),45%,14%)   */
/*CR00604 - */

REM         if sc_glass% < 1% or sc_glass% > 16% ~
                         then goto L50210

REM         if sc_glass% <= 4% then             ~
                 sc_glass_d$ = str(scr$(sc_glass% + 1%),9%,10%)

REM         if sc_glass% = 15% or sc_glass% = 16% then      ~
                 sc_glass_d$ = str(scr$(sc_glass% - 6%),9%,10%)
REM              sc_glass_d$ = str(scr$(sc_glass% - 7%),9%,10%)

REM         if sc_glass% > 6% and sc_glass% < 15% then      ~
                 sc_glass_d$ = str(scr$((sc_glass% + 1%)-6%),24%,16%)

        return
L50210:
        errormsg$ = "(Error) Invalid Glass Type Print Selection? "
        return
/* (AWD002) */
/* CR2019 */
L50300: if sc_lbl$ = " " then sc_lbl$ = "A"
        if sc_lbl$ <> "A" and sc_lbl$ <> "P" then goto L50390
        if sc_dept$ = "ALL" then goto L50380   /* valid dept later */
/* only allow P with department 029, 070, 071 CR2218 */
        if sc_lbl$ = "A" and (sc_dept$ = "029" or sc_dept$ = "070" or ~
           sc_dept$ = "071")  then goto L50391 
        if sc_lbl$ = "A" and schema% = 2% and sc_dept$ = "027" then goto L50391       
        if sc_lbl$ = "P" and (sc_dept$ <> "029" and sc_dept$ <> "070" and ~
           sc_dept$ <> "071" and sc_dept$ <> "027") then goto L50392 
           
L50380: return

L50390: errormsg$ = "(Error) Invalid Label Type Selection? "
        return
        
L50391: errormsg$ = "(Error) Label Type Must be P due to dept"
        return
        
L50392: errormsg$ = "(Error) Label Type Must be A due to dept"
        return

        check_dept
            dept% = 0%
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "PLAN DEPT"
            str(readkey$,10%,15%) = sc_dept$
            read #4,key = readkey$, using L51000, desc$, eod goto L51010
L51000:        FMT POS(25), CH(30)
            dept% = 1%
L51010: return
/*CR00606 + */
        check_laminate
            glass% = 0%
            init(" ") readkey$, desc$
            str(readkey$,1%,9%)   = "PLAN LAMN"
            str(readkey$,10%,15%) = rk_glass$
            read #4,key = readkey$, using L51000, desc$, eod goto N51010
            glass% = 1%
N51010: return
/*CR00606 - */


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

        file_option
           k% = 2%
           hdr$     = "******* Please Choose File Option ******"
           msg$(1%) = "Press <ENTER> to REPLACE Previous Data File"
           msg$(2%) = "<PF1> to ADD to Data File or <PF16> to Exit"
           msg$(3%) = "--- ALL RECORDS IN FILE WILL BE PRINTED ---"
           call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
           if k% = 16% then exit_program
           if k% <> 0% and k% <> 1% then file_option
           if k% =  1% then return
               call "OPENCHCK" (#6,  fs%(6%),  f2%(6%), 0%, rslt$(6%))
               call "FILEBGON" addr(#6)     /* Must open to scratch */
        return
 
        load_results
           k% = 2%
           hdr$     = "***** Label Data Generation Results *****"
           msg$(1%) = "This run generated data for xxxxx label(s)."
           msg$(2%) = "---"
           msg$(3%) = "Press <ENTER> to Acknowledge & Continue"
           convert lbl% to str(msg$(1%),29%,5%), pic(####0)
           if lbl% <> 0% then L64100
               msg$(1%) = "NO LABEL DATA GENERATED!!!"
               str(msg$(3%),,13%) = " Press <PF16>"
L64100:    call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
           if lbl% <> 0% and k% <>  0% then load_results
           if lbl%  = 0% and k% <> 16% then load_results
           lbl% = 0%
        return
    
        print_labels
            call "SHOSTAT" ("Printing Labels...")
            if prt_flag$ <> " " then gosub confirm_reprint
            hdr$     = "***** Label Printing Error *****"
            msg$(1%) = "ERROR OCCURRED WHILE PRINTING LABELS!!!"
            msg$(2%) = " "
            msg$(3%) = "Press <ENTER> to Continue, <PF16> to Exit"

            rec_key$ = all(hex(00))
          print_loop 
            read #6, key > rec_key$, using L64500, rec_key$,             ~
                eod goto L64600
L64500:             fmt ch(14)
            convert sc_glass$ to sc_glass%, data goto bad_glass
  
bad_glass:
            call "EWDPLA65" (#4, #6, rec_key$, sc_glass%, schema%, err%)     
            if err% = 0% then goto print_loop 

            msg$(2%) = "Return Code (EWDPLA65) = "
            convert err% to str(msg$(2%),26%,2%), pic(#0)
L64550:     k% = 2%
            call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
                if k%  =  0% then goto print_loop
                if k% <> 16% then goto L64550
L64600:     if err% = 0% then prt_flag$ = "***"
            return

          confirm_reprint
            k% = 2%
            hdr$     = "*** Please Confirm Print/Reprint ***"
            msg$(1%) = "Press <PF1> to Print &/or Reprint Labels"
            msg$(2%) = "--- OR ---"
            msg$(3%) = "Press <ENTER> to Abort Printing"
            call "ASKUSER" (k%, hdr$, msg$(1%), msg$(2%), msg$(3%))
            if k%  = 1% then return
            if k% <> 0% then confirm_reprint
            return clear all
            goto inputmode


        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            if f2%(5%) = 0% then L65500
                call "SHOSTAT" ("Can't Open File EWDPLNRK--Press <ENTER>")
                stop
L65500:     end

