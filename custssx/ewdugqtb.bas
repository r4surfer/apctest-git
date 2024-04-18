rem *************************************************************~
    *                                                           *~
    *  EEEEE  W   W  DDDD   U   U   GGG    QQQ   TTTTT  BBBB    *~
    *  E      W   W  D   D  U   U  G      Q   Q    T    B   B   *~
    *  EEEE   W   W  D   D  U   U  G GGG  Q   Q    T    BBBB    *~
    *  E      W W W  D   D  U   U  G   G  Q  QQ    T    B   B   *~
    *  EEEEE   W W   DDDD    UUU    GGG    QQQQ    T    BBBB    *~
    *                                          Q                *~
    *-----------------------------------------------------------*~
    * EWDUGQTB - Allows management of the Undelivered Goods     *~
    *            Inventory file (EWDUGQTY).  The aged inventory *~
    *            file (EWDUGAGE) is adjusted behind the scenes. *~
    *-----------------------------------------------------------*~
    *                  M O D I F I C A T I O N S                *~
    *---WHEN---+----------------WHAT----------------------+-WHO-*~
    * 07/01/98 ! Original                                 ! ERN *~
    *************************************************************

    sub "EWDUGQTB" (caller$, userid$)

rem  caller$    = 'shop', 'sales', or 'admin'

rem  user_id$   = current user.  So that can be called by APCSCANN.
rem               Extracted from system if passed as blanks.



    dim                                                              ~
        age_key$50,                  /* Key to EWDUGAGE            */~
        blankdate$8,                 /* Millennium comparisons     */~
        caller$6,                    /* Who's using: shop or sales */~
        cursor%(2%),                 /* Cursor location for edit   */~
        date$10,                     /* Date for screen display    */~
        edtmessage$79,               /* Edit screen message        */~
        errormsg$79,                 /* Error message              */~
        i$(24%)80,                   /* Screen Image               */~
        inpmessage$79,               /* Informational Message      */~
        lfac$(20%)1,                 /* Field Attribute Characters */~
        line2$79,                    /* Screen Line #2             */~
        new_qty$30,                  /* Announcement of new qty    */~
        pf$(3%)79,                   /* PF Screen Literals         */~
        pfkeyS$32,                   /* PF Key Hex Values          */~
        qty_key$50,                  /* Key for EWDUGQTY           */~
        qty_msg$30,                  /* Current on-hand or new rec */~
        scr_area_code$3,             /* Holding Area Code          */~
        scr_area_codedescr$32,       /* Holding Area Code          */~
        scr_bar_code$30,             /* Manufacturing Bar Code     */~
        scr_part_nbr$25,             /* Manufacturing Part Number  */~
        scr_qty$4,                   /* Adjustment quantity        */~
        temp$50, temp2$50,           /* Temporary working storage  */~
        userid$3,                    /* Current User Id            */~
        wandchar$1                   /*                            */ 

    dim f2%(64%),                    /* = 0 if the file is open    */~
        fs%(64%),                    /* = 1 if file open, -1 if it */~
                                     /*   doesn't exist, or 0 if   */~
                                     /*   not yet checked (OPENCHCK*/~
        rslt$(64%)20                 /* Text from file opening     */

rem *************************************************************~
    *                  Release Version ID Section               *~
    *************************************************************
    dim cms2v$50
    cms2v$ = "07.00.00 07/01/98 Holding Area Undelivered Goods"
rem *************************************************************

    mat f2% = con

rem *************************************************************~
    *                  S E L E C T   F I L E S                  *~
    *-----+----------+------------------------------------------*~
    *File#!  PRName  ! File Description                         *~
    *-----+----------+------------------------------------------*~
    * #02 ! SYSFILE2 ! Caelus Management System Information     *~
    * #03 ! GENCODES ! System General Codes file.               *~
    * #06 ! BCKLINES ! BACK LOG LINE ITEM FILE                  *~
    * #07 ! EWDUGQTY ! Undelivered Goods Held Qty               *~
    * #08 ! EWDUGAGE ! Undelivered Goods Aged Inventory         *~
    *************************************************************~
    * File Selection and Open Calls                             *~
    *************************************************************

    
    select #02, "SYSFILE2",                                      ~
           varc,   indexed,       recsize =  500,                ~
                   keypos =    1, keylen  =   20

    select #03, "GENCODES",                                      ~
           varc,   indexed,       recsize =  128,                ~
                   keypos =    1, keylen  =   24


    select #06, "BCKLINES",                                      ~
           varc,   indexed,       recsize =  300,                ~
                   keypos =   10, keylen  =   19

    select #07, "EWDUGQTY",                                      ~
           varc,   indexed,       recsize =   64,                ~
                   keypos =    1, keylen  =   28

    select #08, "EWDUGAGE",                                      ~
           varc,   indexed,       recsize =   64,                ~
                   keypos =    1, keylen  =   34

   call "SHOSTAT" ("Opening UGS Files, One Moment Please")


   call "OPENCHCK" (#02, fs%(02%), f2%(02%),    0%, rslt$(02%))
   call "OPENCHCK" (#03, fs%(03%), f2%(03%),    0%, rslt$(03%))
   call "OPENCHCK" (#05, fs%(05%), f2%(05%),    0%, rslt$(05%))
   call "OPENCHCK" (#06, fs%(06%), f2%(06%),    0%, rslt$(06%))
   call "OPENCHCK" (#07, fs%(07%), f2%(07%),  100%, rslt$(07%))
   call "OPENCHCK" (#08, fs%(08%), f2%(08%),  100%, rslt$(08%))


rem *************************************************************~
    *                I N I T I A L I Z A T I O N                *~
    *-----------------------------------------------------------*~
    * Initializes information necessary for program.            *~
    *************************************************************

    if userid$ <= " " then call "EXTRACT" addr("ID", userid$)

    date$ = date      :  call "DATFMTC"  (date$)
    blankdate$ = " "  :  call "DATUFMTC" (blankdate$)

    /* True and False for clarity */
    true%   = 1%
    false%  = 0%

    edtmessage$  = "To Modify Displayed Values, Position Cursor" & ~
                   " to Desired Value & Press (RETURN)."

    str(line2$,62%) = "EWDUGQTB: " & STR(CMS2V$,,8)

    if caller$ <> "sales" and ~ 
       caller$ <> "admin" then caller$ = "shop"
  
rem *************************************************************~
    *       I N P U T   M O D E   M A I N   P R O G R A M       *~
    *-----------------------------------------------------------*~
    * Handles normal input for data entry screens.              *~
    *************************************************************

inputmode
       gosub initialize_variables

       input_mode% = true%

       for fieldnr% = 1% TO 4%
inp1a:     gosub'051(fieldnr%)              /* Default - Enables */
           if enabled% = false%  then inp1e
inp1b:     gosub'101(fieldnr%, false%)      /* Display - Accept  */
               if keyhit%  =  1% then gosub startover
               if keyhit% <>  4% then       inp1d
inp1c:             fieldnr% = max(1%, fieldnr% - 1%)
                   gosub'051(fieldnr%)
                   if enabled% = true% then inp1b
                   if fieldnr% = 1%    then inp1a
                   goto inp1c
inp1d:         if keyhit% = 16% and fieldnr% = 1% then exit_program
               if keyhit% <> 0%                   then inp1b
inp1e:     gosub'151(fieldnr%)      /* Edit Field for Valid Entry */
               if errormsg$ <> " " then inp1b
       next fieldnr%


rem *************************************************************~
    *        E D I T   M O D E   M A I N   P R O G R A M        *~
    *-----------------------------------------------------------*~
    * Handles operation of EDIT MODE for data entry screens.    *~
    *************************************************************

editpg1
       input_mode% = false%
       lastfieldnr% = 0%
       gosub'101(0%, true%)        /* Display Screen - No Entry */
           if keyhit%  =  1% then gosub startover
           if keyhit%  = 16% then       datasave
           if keyhit% <>  0% then       editpg1
edt1a: fieldnr% = cursor%(1%) - 5%
       if fieldnr% < 4% OR fieldnr% > 4% then editpg1
       if fieldnr% = lastfieldnr%         then editpg1
edt1x: gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
           if enabled% =  false%   then editpg1
edt1b: gosub'101(fieldnr%, true%)  /* Display & Accept Screen     */
           if keyhit%  =  1%   then gosub startover
           if keyhit% <>  0%   then       edt1b
       gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
           if errormsg$ <> " " then edt1b
           lastfieldnr% = fieldnr%
           goto edt1a


rem *************************************************************~
    *             S A V E   D A T A   O N   F I L E             *~
    *-----------------------------------------------------------*~
    * Saves data on file after INPUT/EDITING.                   *~
    *************************************************************

datasave
    gosub dataput
    goto inputmode


rem *************************************************************~
    *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
    *-----------------------------------------------------------*~
    * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
    *************************************************************

deffn'051(fieldnr%)
    enabled% = true%
    on fieldnr% gosub df101,         /* Area                   */~
                      df102,         /* Bar Code               */~
                      df103,         /* Mfg Part Number        */~
                      df104          /* Quantity               */
    return

df101:   /* Def/Enable Area Code                  scr_area_code$      */
        if input_mode% = false% then enabled% = false%
        return

df102:  /* Def/Enable Shipping Bar Code           scr_bar_code$       */
        if input_mode% = false% then enabled% = false%
        return

df103:  /* Def/Enable Manufacturing Part Number   scr_part_nbr$       */
        if input_mode% = false% then enabled% = false%
        if scr_bar_code$ > " "  then enabled% = false%
        return

df104:  /* Def/Enable Quantity                    scr_qty$           */
        if caller$ = "sales" then enabled% = false%
        return


rem *************************************************************~
    *      I N I T I A L I Z E   I N P U T   M E S S A G E S    *~
    *-----------------------------------------------------------*~
    * Initializes Input Messages.                               *~
    *************************************************************

deffn'050(scrnr%, fieldnr%)
       if fieldnr% <> 0% then im000
           inpmessage$ = edtmessage$
return


im000: rem Define the Input Message for the Screen/Field Indicated
       if scrnr% = 1% then restore line = scrn1_msg, fieldnr%
       read inpmessage$      /* Read Input Message */
return


scrn1_msg  :  data                                        ~
       "Enter HOLDING AREA Code.                               ",~
       "Scan/Enter BAR CODE.  Leave blank if unavailable.      ",~
       "Enter Manufacturing PART NUMBER.                       ",~
       "Enter ADJUSTMENT QUANTITY. (Negative to decrease)      "


rem *************************************************************~
    * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
    *-----------------------------------------------------------*~
    * Initializes all defined screen variables to blank         *~
    *************************************************************
initialize_variables
    init(" ") errormsg$, inpmessage$,                            ~
              scr_area_code$, scr_area_codedescr$, temp$,        ~
              scr_bar_code$, scr_part_nbr$, scr_qty$, new_qty$,  ~
              qty_msg$, wandchar$         

    qty     = 0
    scr_qty = 0

    return


rem *************************************************************~
    * S T A R T   O V E R   L A S T   C H A N C E   S C R E E N *~
    *-----------------------------------------------------------*~
    * Gives User the ability to START OVER when s/he wants to   *~
    * or will return User back to where they were.  Must push   *~
    * two buttons to start over for safety.                     *~
    *************************************************************

startover
    u3% = 2%
    call "STARTOVR" (U3%)
    if u3% = 1% then return
        return clear all
        goto inputmode


rem *************************************************************~
    *           L O A D   D A T A   F R O M   F I L E           *~
    *-----------------------------------------------------------*~
    * Loads data from File Record Area into Program Variables.  *~
    *************************************************************
dataload

        new_record% = true%

        qty = 0

        qty_key$ = str(scr_area_code$,1%,3%) & scr_part_nbr$

        read #7, key = qty_key$, using dldfmt, qty,                ~
                 eod goto dld_missing
dldfmt:     FMT POS(29), PD(15,4)

        new_record% = false%

dld_missing:

        convert qty to qty_msg$, pic(-##,###)
        qty_msg$ = "Current Quantity: " & qty_msg$

        if new_record% = true% then qty_msg$ = "** NEW RECORD **"

        return clear all
        input_mode% = false%
        fieldnr% = 4%
        goto edt1x



rem *************************************************************~
    *          S T U F F   D A T A   I N T O   F I L E          *~
    *-----------------------------------------------------------*~
    * Stuffs data from Program Variables into File Record Area. *~
    *************************************************************

    dataput

        if new_record% = false% then dp2
            
dp2:    qty_key$ = str(scr_area_code$,1%,3%) & scr_part_nbr$

        if new_record% = false% then                              ~
            read #7, hold, key = qty_key$

        new_qty = qty + scr_qty

        put #7, using EWDUGQTY_fmt,                               ~
             scr_area_code$, scr_part_nbr$, new_qty, " "
        
        if new_qty > 0 then dp2a
            if new_record% = false% then delete #7
            goto dp4
dp2a:   if new_record% = true% then write #7 else rewrite #7


dp4 /* Now adjust the aged inventory                  */
        if scr_qty > 0 then dp4_pos

        /* remove from file                 */

            qty_left = 0 - scr_qty

dp4_loop:   age_key$          = scr_area_code$
            str(age_key$,4% ) = scr_part_nbr$
            str(age_key$,29%) = all(hex(00))

            read #8, hold, key > age_key$, using dp4_age_fmt,   ~
                        age_qty, eod goto dp4_done
dp4_age_fmt:    FMT POS(35), PD(15,4)
            
            new_age_qty = age_qty - qty_left
            
            put #8 using dp4_age_fmt, new_age_qty

            if new_age_qty <= 0 then delete #8 else rewrite #8

            if age_qty >= qty_left then qty_left = 0            ~
                                   else qty_left = qty_left - age_qty

            if qty_left > 0 then goto dp4_loop else goto dp4_done

        /* Add to file      */
dp4_pos:
            new%              = true%
            age_key$          = scr_area_code$
            str(age_key$,4% ) = scr_part_nbr$
            str(age_key$,29%) = date
            age_qty           = 0
        read #8, hold, key = age_key$, using dp4_age_fmt, age_qty, ~
                       eod goto dp4c
        new%  = false%
dp4c:   age_qty = age_qty + scr_qty
        put #8, using EWDUGAGE_fmt,                              ~
                scr_area_code$, scr_part_nbr$, date, age_qty, " "
        if new% = true% then write #8 else rewrite #8

dp4_done:
        return


rem *************************************************************~
    *        F O R M A T    S T A T E M E N T S                 *~
    *-----------------------------------------------------------*~
    * FORMAT Statements for Data Files.                         *~
    *************************************************************

EWDUGQTY_fmt:                  /* FILE: EWDUGQTY                          */~
fmt pos(   1), ch(3),          /* Holding Area Code                       */~
    pos(   4), ch(25),         /* Manufacturing Part Number               */~
    pos(  29), pd(15,4),       /* Quantity                                */~
    pos(  37), ch(28)          /* Unused Space                            */


EWDUGAGE_fmt:                  /* FILE: EWDUGAGE                          */~
fmt pos(   1), ch(3),          /* Holding Area Code                       */~
    pos(   4), ch(25),         /* Manufacturing Part Number               */~
    pos(  29), ch(6),          /* APCDUGLOG Date Entered                  */~
    pos(  35), pd(15,4),       /* Quantity                                */~
    pos(  43), ch(22)          /* Unused Space                            */



rem *************************************************************~
    *   G E N E R I C    S C R E E N    S U B R O U T I N E S   *~
    *-----------------------------------------------------------*~
    * uprlowr       - FAC Upper or Lower Case                   *~
    * upronly       - FAC Upper Case Only                       *~
    * numeric       - FAC Numeric                               *~
    * man_or_prtscr - Manual or Print Screen                    *~
    * get_screen    - Get screen and cursor image               *~
    *************************************************************

uprlowr
    lfac$(fieldnr%) = hex(80)
return


upronly
    lfac$(fieldnr%) = hex(81)
return


numeric
    lfac$(fieldnr%) = hex(82)
return


man_or_prtscr
    if keyhit% = 13% then call "MANUAL" ("EWDUGLGB") ~
                     else call "PRNTSCRN"
return


get_screen
    close WS
    call "SCREEN" addr ("C", U3%, "I", i$(), cursor%())
return


rem *************************************************************~
    *               S C R E E N   P A G E   1                   *~
    *-----------------------------------------------------------*~
    * Document Input and Edit Screen.                           *~
    *************************************************************

deffn'101(fieldnr%, edit%)
    gosub'050(1%, fieldnr%)
    if caller$ = "sales" and fieldnr% = 1% then  ~
        inpmessage$ = "Enter RECORD ID or leave blank to see list."
    if edit% = false% then gosub setpf_pg1_inp ~
                      else gosub setpf_pg1_edt

    if fieldnr% > 0% then init(hex(8C)) lfac$()                ~
                     else init(hex(86)) lfac$()

    on fieldnr% gosub upronly,         /* Area Code         */ ~
                      numeric,         /* Bar Code          */ ~
                      upronly,         /* Part Number       */ ~
                      numeric,         /* Qty               */

    if fieldnr% = 2% then lfac$(4%) = hex(99) /* (EWD001)   */

  scr1_dsply
    accept                                                       ~
       at (01,02),                                               ~
          "Maintain Undelivered Goods Log",                      ~
       at (01,64), "Today:",                                     ~
       at (01,71), fac(hex(8C)), date$                  , CH(10),~
       at (02,02), fac(hex(AC)), line2$                 , CH(79),~
       at (04,02), fac(hex(94)), errormsg$              , CH(79),~
                                                                 ~
       at (06,02), "Holding Area Code",                          ~
       at (06,30), fac(lfac$(1%)), scr_area_code$       , CH(03),~
       at (06,49), fac(hex(8C)),   scr_area_codedescr$  , CH(32),~
                                                                 ~
       at (07,02), "Shipping Bar Code",                          ~
       at (07,30), fac(lfac$(2%)), scr_bar_code$        , CH(18),~
       at (07,50), fac(lfac$(4%)), wandchar$            , ch(01),~
                                                                 ~
       at (08,02), "Manufacturing Part Number",                  ~
       at (08,30), fac(lfac$(3%)), scr_part_nbr$        , CH(25),~
                                                                 ~
       at (09,02), "Adjustment Quantity     ",                   ~
       at (09,30), fac(lfac$( 4)), scr_qty$             , CH(04),~
       at (09,49), fac(hex(8c)),   qty_msg$             , CH(30),~
       at (10,49), fac(hex(8c)),   new_qty$             , CH(30),~
                                                                 ~
       at (21,02), fac(hex(A4)),   inpmessage$          , CH(79),~
       at (22,02), fac(hex(8C)),   pf$(1%)              , CH(79),~
       at (23,02), fac(hex(8C)),   pf$(2%)              , CH(79),~
       at (24,02), fac(hex(8C)),   pf$(3%)              , CH(79),~
                                                                 ~
       keys(pfkeys$), key(keyhit%)

    if keyhit% = 13% or keyhit% = 15% then gosub man_or_prtscr
    if keyhit% = 13% or keyhit% = 15% then goto  scr1_dsply
    if keyhit% = 20%                  then caller$ = "admin"
    gosub get_screen
return


setpf_pg1_inp
    /* Input Mode */
    pf$(1%) = "(1)Start Over                                                  (13)Instructions"
    pf$(2%) = "                 (4)Previous Field                             (15)Print Screen"
    pf$(3%) = "                                                               (16)Exit Program"
    pfkeys$ = hex(01FFFF04FFFFFFFFFFFFFFFF0D140F1000)

    /* Field one, turn off previous field, otherwise turn off exit */
    if fieldnr% = 1% then str(pf$(2%), 18%, 26%) = " " ~
                     else str(pf$(3%), 64%, 16%) = " "
    if fieldnr% = 1% then str(pfkeys$,  4%,  1%) = hex(FF) ~
                     else str(pfkeys$, 16%,  1%) = hex(FF)
return


setpf_pg1_edt
    /* Edit Mode */
    if fieldnr% > 0% then setpf_pg1_edtfld

    /* Edit Mode - no field selected */
    pf$(1%) = "(1)Start Over                                                  (13)Instructions"
    pf$(2%) = "                                                               (15)Print Screen"
    pf$(3%) = "                                                               (16)Save Data   "
    pfkeys$ = HEX(01FFFFFFFFFFFFFFFFFFFFFF0DFF0F1000)
    goto setpf_pg1_edtexit

  setpf_pg1_edtfld
    pf$(1%) = "(1)Start Over                                                  (13)Instructions"
    pf$(2%) = "                                                               (15)Print Screen"
    pf$(3%) = "                                                                               "
    pfkeys$ = HEX(01FFFFFFFFFFFFFFFFFFFFFF0DFF0FFF00)

  setpf_pg1_edtexit
return




rem *************************************************************~
    *                     T E S T   D A T A                     *~
    *-----------------------------------------------------------*~
    * Test data for the items on Screen 1.                      *~
    *************************************************************

deffn'151(fieldnr%)
    errormsg$ = " "
    on fieldnr% gosub td101,         /* Area Code              */~
                      td102,         /* Bar Code               */~
                      td103,         /* Part Number            */~
                      td104          /* Quantity               */
return

td101 /* Test for Holding Area Code            scr_area_code$      */
        temp$ = "UG AREAS " & scr_area_code$
        call "PLOWCODE" (#3, temp$, scr_area_codedescr$, ~
                            9%, 0.30, f1%)
            if f1% = 1% then scr_area_code$ = str(temp$,10%)
            if f1% = 1% then return
                scr_area_code$ = " "
                errormsg$ = hex(00)
                return

td102   /* Test for SHIPPING BAR CODE       scr_bar_code$            */
        /* fmt = 1,8 = S.O., 9,2 = line, 11,4 = piece, 14,4 = of     */

        if scr_bar_code$ <= " " then return

        if len(scr_bar_code$) = 10% or len(scr_bar_code$) = 18% then td102a
            goto td102x

td102a: /* Check out the Sales Order portion                        */
        convert str(scr_bar_code$,9%,2%) to temp%, data goto td102x
        temp$ = " "
        convert temp% to str(temp$,17%,3%), pic(##0)
        str(temp$,1%,16%) = str(scr_bar_code$,1%,8%)
        read #6, key  = temp$, eod goto td102x
        get  #6, using td102e,  scr_part_nbr$
td102e:     FMT POS(32), CH(25)
        return

td102x: errormsg$ = "Bar Code entered (" & scr_bar_code$ & ") is not valid."
        return


td103   /* Test for Manufacturing Part Number    scr_part_nbr$       */
        if scr_part_nbr$ > " " then td103a
            errormsg$ = "Part Number cannot be blank."
            return
td103a: temp$ = "invalid length"
        if len(scr_part_nbr$) <> 19% and          ~ 
           len(scr_part_nbr$) <> 22% and          ~
           len(scr_part_nbr$) <> 25% then return
        temp$ = "no such model" : temp2$ = "MODEL    " & str(scr_part_nbr$, 1%,3%)
        read #3, key = temp2$, eod goto td103x
        temp$ = "no such color" : temp2$ = "COLOR    " & str(scr_part_nbr$, 4%,1%)
        read #3, key = temp2$, eod goto td103x
        temp$ = "no such glass" : temp2$ = "GLASS    " & str(scr_part_nbr$, 5%,2%)
        read #3, key = temp2$, eod goto td103x
        temp$ = "no such liting": temp2$ = "LITING   " & str(scr_part_nbr$, 7%,2%)
        read #3, key = temp2$, eod goto td103x
        temp$ = "no such hinge" : temp2$ = "HINGE    " & str(scr_part_nbr$, 9%,2%)
        read #3, key = temp2$, eod goto td103x
        temp$ = "no such screen": temp2$ = "SCREEN   " & str(scr_part_nbr$,11%,1%)
        read #3, key = temp2$, eod goto td103x
        temp$ = "no such lock " : temp2$ = "LOCKS    " & str(scr_part_nbr$,12%,1%)
        read #3, key = temp2$, eod goto td103x

        gosub dataload

        return

td103x: errormsg$ = "Part number: " & temp$ & ".  Please re-enter."
        return


td104   /* Test for Quantity                     scr_qty$            */
        call "NUMTEST" (scr_qty$, -999, 999, errormsg$, 0.0, scr_qty)
        if errormsg$ <> " " then return
        
        new_qty = qty + scr_qty
        convert new_qty to new_qty$, pic(-##,##0)
        new_qty$ = "New On-hand Qty : " & new_qty$

        if new_qty < 0 then errormsg$ =  ~
            "Adjustment cannot cause on-hand inventory to become negative."

        return





rem THISPROGRAMWASGENERATEDBYGENPGMAPROPRIETRYPRODUCTOFCAEUS,INC~
    *                          E X I T                          *~
    *-----------------------------------------------------------*~
    * Terminates execution (files closed automatically).        *~
    *-----------------------------------------------------------*~
    *  This program contains valuable trade secrets and         *~
    *  proprietary assets of CAELUS, INCORPORATED, Spokane, WA  *~
    *  embodying substantial creative efforts and confidential  *~
    *  information.  Unauthorized use, copying, decompiling,    *~
    *  translating, disclosure, or transfer of it is prohibited.*~
    *  Copyright (c) 1998, an unpublished work by CAELUS,       *~
    *  INCORPORATED, Spokane, WA.  All rights reserved.         *~
    CAELUS,INCORPORATEDSPOKANEWASHINGTONALLRIGHTSRESERVEDCAELUSIN

exit_program

   end
