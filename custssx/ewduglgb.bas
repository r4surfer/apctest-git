rem *************************************************************~
    *                                                           *~
    *  EEEEE  W   W  DDDD   U   U   GGG   L       GGG   BBBB    *~
    *  E      W   W  D   D  U   U  G      L      G      B   B   *~
    *  EEEE   W   W  D   D  U   U  G GGG  L      G GGG  BBBB    *~
    *  E      W W W  D   D  U   U  G   G  L      G   G  B   B   *~
    *  EEEEE   W W   DDDD    UUU    GGG   LLLLL   GGG   BBBB    *~
    *                                                           *~
    *-----------------------------------------------------------*~
    * EWDUGLGB - Manages the Undelivered Goods Log file         *~
    *            EWDUGLOG for receipts into the system.         *~
    *                                                           *~
    *            When a log record is added, inventory          *~
    *            (EWDUGQTY) is incremented by one for the       *~
    *            holding area and part spcified.  The aged      *~
    *            inventory file is incremented also on the      *~
    *            in date specified by the user.  Modifications  *~
    *            to existing log records DO NOT affect the      *~
    *            inventory files -- changes must be made using  *~
    *            the EWDUGQTB subroutine.                       *~
    *                                                           *~
    *            The caller$ passed in (shop, sales, admin)     *~
    *            controls which fields the user can modify.     *~
    *            Shop can modify the receipt-time fields, sales *~
    *            the resolved fields.  Admin can see all and do *~
    *            all.  The caller$ may be switched to admin     *~
    *            by pressing PF-2o whilst in input mode.        *~
    *                                                           *~
    *            The system uses two GENCODES tables --         *~
    *               UG AREAS  - Defines Holding Areas           *~
    *               UG REASON - Defines reason codes            *~
    *-----------------------------------------------------------*~
    *                  M O D I F I C A T I O N S                *~
    *---WHEN---+----------------WHAT----------------------+-WHO-*~
    * 07/01/98 ! Original                                 ! ERN *~
    *************************************************************

    sub "EWDUGLGB" (caller$, userid$, record_id$)

rem    caller$    = "shop" or "sales" or "admin"
rem    user_id$   = Current user.  So that can be called by APCSCANN.
rem    record_id$ = A specific record to maintain.  This record will
rem                 be automatically loaded and the program will end
rem                 then user does a save.

    dim                                                              ~
        blankdate$8,                 /* Millennium comparisons     */~
        caller$6,                    /* Who's using: shop or sales */~
        cursor%(2%),                 /* Cursor location for edit   */~
        date$10,                     /* Date for screen display    */~
        edtmessage$79,               /* Edit screen message        */~
        errormsg$79,                 /* Error message              */~
        i$(24%)80,                   /* Screen Image               */~
        inpmessage$79,               /* Informational Message      */~
        last_saved$30,               /* Last record if message     */~
        lfac$(20%)1,                 /* Field Attribute Characters */~
        line2$79,                    /* Screen Line #2             */~
        pf$(3%)79,                   /* PF Screen Literals         */~
        pfkeyS$32,                   /* PF Key Hex Values          */~
        record_id$8,                 /* Specific record to maintain*/~
        save_resolved_flag$1,        /* Original state             */~
        scr_area_code$3,             /* Holding Area Code          */~
        scr_area_codedescr$32,       /* Holding Area Code          */~
        scr_bar_code$30,             /* Manufacturing Bar Code     */~
        scr_cust_nbr$9,              /* Customer Number            */~
        scr_cust_name$30,            /*          Name              */~
        scr_date_in$10,              /* Date In                    */~
        scr_entered_by$3,            /* Entered By User ID         */~
        scr_entered_date$10,         /* Entered Date (system)      */~
        scr_new_so_nbr$16,           /* New Sales Order Number     */~
        scr_part_nbr$25,             /* Manufacturing Part Number  */~
        scr_po_nbr$16,               /* Customer P.O. Number       */~
        scr_rcv_comment$30,          /* Receiving Comments         */~
        scr_record_id$08,            /* Record ID                  */~
        scr_resolved_flag$1,         /* Resolved?                  */~
        scr_resolved_by$3,           /*          By                */~
        scr_resolved_date$10,        /*          Date              */~
        scr_resolved_info$30,        /*          By and Date       */~
        scr_rsn_code$3,              /* Reason Code                */~
        scr_rsn_codedescr$32,        /* Reason Code                */~
        scr_sales_comment1$30,       /* Sales Comments             */~
        scr_sales_comment2$30,       /* Sales Comments             */~
        scr_sales_hdr$40,            /* Sales Info                 */~
        scr_so_nbr$16,               /* Sales Order, Line Number   */~
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
    cms2v$ = "07.00.00 07/01/98 Log Maint. - Undelivered Goods"
rem *************************************************************

    mat f2% = con

rem *************************************************************~
    *                  S E L E C T   F I L E S                  *~
    *-----+----------+------------------------------------------*~
    *File#!  PRName  ! File Description                         *~
    *-----+----------+------------------------------------------*~
    * #01 ! EWDUGLOG ! Undelivered Goods Log File               *~
    * #02 ! SYSFILE2 ! Caelus Management System Information     *~
    * #03 ! GENCODES ! System General Codes file.               *~
    * #04 ! CUSTOMER ! Customer Master                          *~
    * #05 ! BCKMASTR ! S.O. Header Master File                  *~
    * #06 ! BCKLINES ! BACK LOG LINE ITEM FILE                  *~
    * #07 ! EWDUGQTY ! Undelivered Goods Held Qty               *~
    * #08 ! EWDUGAGE ! Undelivered Goods Aged Inventory         *~
    *************************************************************~
    * File Selection and Open Calls                             *~
    *************************************************************

    select #01, "EWDUGLOG",                                      ~
           varc,   indexed,       recsize =  384,                ~
                   keypos =    1, keylen  =     8,               ~
        alt key 1, keypos =    9, keylen  =    34, dup,          ~
            key 2, keypos =   24, keylen  =    19, dup,          ~
            key 3, keypos =   43, keylen  =    25, dup,          ~
            key 4, keypos =   68, keylen  =    16, dup,          ~
            key 5, keypos =   84, keylen  =    30, dup

    select #02, "SYSFILE2",                                      ~
           varc,   indexed,       recsize =  500,                ~
                   keypos =    1, keylen  =   20

    select #03, "GENCODES",                                      ~
           varc,   indexed,       recsize =  128,                ~
                   keypos =    1, keylen  =   24

    select #04  "CUSTOMER",                                      ~
           varc,   indexed,       recsize = 1200,                ~
                   keypos =    1, keylen  =    9

    select #05, "BCKMASTR",                                      ~
           varc,   indexed,       recsize = 1000,                ~
                   keypos =    1, keylen  =   25,                ~
       alt key  1, keypos =   26, keylen  =   16,  dup

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

   call "OPENCHCK" (#01, fs%(01%), f2%(01%), 1000%, rslt$(01%))
   call "OPENCHCK" (#02, fs%(02%), f2%(02%),    0%, rslt$(02%))
   call "OPENCHCK" (#03, fs%(03%), f2%(03%),    0%, rslt$(03%))
   call "OPENCHCK" (#04, fs%(04%), f2%(04%),    0%, rslt$(04%))
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

    str(line2$,62%) = "EWDUGLGB: " & STR(CMS2V$,,8)

    if caller$ <> "sales" and ~ 
       caller$ <> "admin" then caller$ = "shop"
  
rem *************************************************************~
    *       I N P U T   M O D E   M A I N   P R O G R A M       *~
    *-----------------------------------------------------------*~
    * Handles normal input for data entry screens.              *~
    *************************************************************

inputmode
       gosub initialize_variables

       if record_id$ = " " then inp1
            scr_record_id$ = record_id$
            gosub'151(1%)
            if errormsg$ <> " " then exit_program
                goto editpg1

inp1:  for fieldnr% = 1% TO 14%
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
       lastfieldnr% = 0%
       gosub'101(0%, true%)        /* Display Screen - No Entry */
           if keyhit%  =  1% then gosub startover
           if keyhit%  = 11% then       check_inventory
           if keyhit%  = 12% then       delete_record
           if keyhit%  = 16% then       datasave
           if keyhit% <>  0% then       editpg1
edt1a: fieldnr% = cursor%(1%) - 5%
       if fieldnr% < 2% OR fieldnr% > 15% then editpg1
       if fieldnr% = 15% then fieldnr% = 14%     /* Sales Comments*/ 
       if fieldnr% = lastfieldnr%         then editpg1
       gosub'051(fieldnr%)         /* Check Enables, Set Defaults */
           if enabled% =  false%   then editpg1
edt1b: gosub'101(fieldnr%, true%)  /* Display & Accept Screen     */
           if keyhit%  =  1%   then gosub startover
           if keyhit% <>  0%   then       edt1b
       gosub'151(fieldnr%)         /* Edit Field for Valid Entry  */
           if errormsg$ <> " " then edt1b
           lastfieldnr% = fieldnr%
           goto edt1a


check_inventory
/* get the inventory for this guy and display for user to see */
        temp$ = scr_area_code$ : str(temp$,4%) = scr_part_nbr$
        qty   = 0
        read #7, hold, key = temp$, using edfmt, qty, eod goto edfmt
edfmt:     FMT POS(29), PD(15,4)
        errormsg$ = " " 
        convert qty to errormsg$, pic(#####0)
        errormsg$ = errormsg$ & " pieces are in holding area "    & scr_area_code$
        if qty = 1 then errormsg$ = "1 piece is in holding area " & scr_area_code$
        goto editpg1

rem *************************************************************~
    *             S A V E   D A T A   O N   F I L E             *~
    *-----------------------------------------------------------*~
    * Saves data on file after INPUT/EDITING.                   *~
    *************************************************************

datasave
    gosub dataput
    if record_id$ > " " then exit_program
        goto inputmode

delete_record
    call "ASKUSER" (keyhit%, "CONFIRM DELETE",                   ~
                    "Press PF- 1 to CANCEL",                     ~
                    "      PF-16 to DELETE",                     ~
                    "Note: Does NOT modify Undelivered Goods Inventory.")
    if keyhit% =   1% then editpg1
    if keyhit% <> 16% then delete_record

        read   #1, hold, key = scr_record_id$, eod goto del1
        delete #1

del1:   if record_id$ > " " then exit_program
            goto inputmode

rem *************************************************************~
    *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
    *-----------------------------------------------------------*~
    * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
    *************************************************************

deffn'051(fieldnr%)
    enabled% = true%
    on fieldnr% gosub df101,         /* Record ID              */~
                      df102,         /* Bar Code               */~
                      df104,         /* S.O. Number            */~
                      df103,         /* Mfg Part Number        */~
                      df105,         /* Customer Number        */~
                      df106,         /* Customer P.O. Nbr      */~
                      df107,         /* Date In                */~
                      df108,         /* Holding Area Code      */~
                      df109,         /* Reason Code            */~
                      df110,         /* Receiving Comment      */~
                      df111,         /* Sales Info             */~
                      df112,         /* Resolved?              */~
                      df113,         /* New S.O. Nbr           */~
                      df114,         /* Sales Comments         */~
                      df115          /* Sales Comments         */
return

df101:   /* Def/Enable Record ID                   scr_record_id$      */
        return

df102   /* Def/Enable Shipping Bar Code           scr_bar_code$       */
        if caller$     = "sales" then enabled% = false%
        if new_record% = false%  then enabled% = false%
        return

df103   /* Def/Enable Manufacturing Part Number   scr_part_nbr$       */
        if caller$       = "sales" then enabled% = false%
        if new_record%   = false%  then enabled% = false%
        if scr_bar_code$ > " "     then enabled% = false%
        if scr_so_nbr$   > " "     then enabled% = false%
        return

df104   /* Def/Enable Sales Order, Line Number    scr_so_nbr$         */
        if caller$       = "sales" then enabled% = false%
        if new_record%   = false%  then enabled% = false%
        if scr_bar_code$ > " "     then enabled% = false%
        return

df105   /* Def/Enable Customer Number             scr_cust_nbr$       */
        if caller$       = "sales" then enabled% = false%
        if scr_bar_code$ > " "     then enabled% = false%
        if scr_so_nbr$   > " "     then enabled% = false%
        return

df106   /* Def/Enable Customer P.O. Number        scr_po_nbr$         */
        if caller$       = "sales" then enabled% = false%
        if new_record%   = false%  then enabled% = false%
        if scr_bar_code$ > " "     then enabled% = false%
        if scr_so_nbr$   > " "     then enabled% = false%
        return

df107   /* Def/Enable Date In                     scr_date_in$        */
        if scr_date_in$ = " "     then scr_date_in$ = date$
        if edit%        = false%  then enabled% = false%
        if caller$      = "sales" then enabled% = false%
        if new_record%  = false%  then enabled% = false%
        return

df108   /* Def/Enable Holding Area Code           scr_area_code$      */
        if caller$     = "sales" then enabled% = false%
        if new_record% = false%  then return
        return

df109   /* Def/Enable Reason Code                 scr_rsn_code$       */
        if caller$ = "sales" then enabled% = false%
        return

df110   /* Def/Enable Receiving Comments          scr_rcv_comment$    */
        if caller$ = "sales" then enabled% = false%
        return

df111   /* Def/Enable Sales Info                  scr_sales_hdr$      */
        enabled% = false%
        return

df112   /* Def/Enable Resolved?                   scr_resolved_flag$  */
        if caller$ = "shop" then enabled% = false%
        return

df113   /* Def/Enable New Sales Order Number      scr_new_so_nbr$     */
        if caller$ = "shop" then enabled% = false%
        return

df114   /* Def/Enable Sales Comments              scr_sales_comment1$ */
        if caller$ = "shop" then enabled% = false%
        return

df115   /* Def/Enable Sales Comments              scr_sales_comment2$ */
        if caller$ = "shop" then enabled% = false%
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
       "Scan/Enter BAR CODE.  Leave blank if unavailable.      ",~
       "Enter RECORD ID, '?' for list, or leave blank for a new entry.",~
       "Enter SALES ORDER and Sales Order LINE NUMBER.         ",~
       "Enter Manufacturing Part Number.                       ",~
       "Enter CUSTOMER NUMBER.                                 ",~
       "Enter Customer P.O. NUMBER.                            ",~
       "Enter DATE when goods moved into a holding area.       ",~
       "Enter HOLDING AREA Code.                               ",~
       "Enter REASON Code.                                     ",~
       "Enter Receiving COMMENTS.                              ",~
       "Enter Sales Info                                       ",~
       "Enter RESOLVED? ('Y' or blank).                        ",~
       "Enter NEW SALES ORDER Number.                          ",~
       "Enter Sales COMMENTS.                                  ",~
       "Enter Sales Comments.                                  "


rem *************************************************************~
    * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
    *-----------------------------------------------------------*~
    * Initializes all defined screen variables to blank         *~
    *************************************************************
initialize_variables
    init(" ") errormsg$, inpmessage$, scr_record_id$,            ~
              scr_area_code$, scr_area_codedescr$, temp$,        ~
              scr_bar_code$, scr_cust_nbr$, scr_date_in$,        ~
              scr_new_so_nbr$, scr_part_nbr$, scr_po_nbr$,       ~
              scr_po_nbr$, scr_rcv_comment$, scr_so_line$,       ~
              scr_resolved_flag$, scr_rsn_code$,                 ~
              scr_rsn_codedescr$, scr_sales_comment1$,           ~
              scr_sales_comment2$, scr_sales_hdr$, scr_so_nbr$,  ~
              scr_cust_name$, filler$, save_resolved_flag$,      ~
              scr_resolved_info$, scr_resolved_by$,              ~
              scr_resolved_date$, wandchar$

    scr_entered_by$   = userid$
    scr_entered_date$ = date
    call "DATFMTC" (scr_entered_date$)

    new_record% = true%

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
        if record_id$ > " " then exit_program
        goto inputmode

rem *************************************************************~
    *           L O A D   D A T A   F R O M   F I L E           *~
    *-----------------------------------------------------------*~
    * Loads data from File Record Area into Program Variables.  *~
    *************************************************************
dataload

        new_record% = true%

        read #1, key = scr_record_id$, using EWDUGLOG_fmt,        ~
             scr_record_id$, scr_cust_nbr$, scr_date_in$,         ~
             scr_so_nbr$, scr_so_line$, scr_part_nbr$,            ~
             scr_po_nbr$, scr_bar_code$, scr_rcv_comment$,        ~
             scr_area_code$, scr_rsn_code$, scr_entered_by$,      ~
             scr_entered_date$, scr_resolved_flag$,               ~
             scr_new_so_nbr$, scr_sales_comment1$,                ~
             scr_sales_comment2$, scr_resolved_by$,               ~
             scr_resolved_date$, filler$,                         ~
             eod goto dld_missing

        new_record% = false%

        save_resolved_flag$ = scr_resolved_flag$

        call "DATFMTC" (scr_date_in$)
        call "DATFMTC" (scr_entered_date$)
        call "DATFMTC" (scr_resolved_date$)

        if scr_resolved_by$ > " " then                              ~
            scr_resolved_info$ = "Changed by " & scr_resolved_by$ & ~
                                " on " & scr_resolved_date$         ~
        else                                                        ~
            scr_resolved_info$ = " "

        gosub dld_get_customer

        gosub dld_get_area

        gosub dld_get_rsn

        return


dld_missing:
        errormsg$ = "Record " & scr_record_id$ & " not on file."
        return


dld_get_area:
        rc% = true%
        scr_area_codedescr$ = " "
        gc_key$ = "UG AREAS " & scr_area_code$
        read #3, key = gc_key$, using dlda1, scr_area_codedescr$, ~
                 eod goto dlda2
dlda1:      FMT POS(25), CH(30)
        return
dlda2:  rc% = false%
        return


dld_get_customer:
        rc% = true%
        scr_cust_name$ = " "
        read #4, key = scr_cust_nbr$, using dldc1, scr_cust_name$, ~
                 eod goto dldc2
dldc1:      FMT POS(10), CH(30)
        return
dldc2:  rc% = false%
        return


dld_get_rsn:
        rc% = true%
        scr_rsn_codedescr$  = " "
        gc_key$ = "UG REASON" & scr_rsn_code$
        read #3, key = gc_key$, using dldr1, scr_rsn_codedescr$, ~
                 eod goto dldr2
dldr1:      FMT POS(25), CH(30)
        return
dldr2:  rc% = false%
        return


rem *************************************************************~
    *          S T U F F   D A T A   I N T O   F I L E          *~
    *-----------------------------------------------------------*~
    * Stuffs data from Program Variables into File Record Area. *~
    *************************************************************

    dataput

        if new_record% = false% then dp2
            next% = 1%
            temp$ = "UG NEXT REC ID"
            new%  = true%
            read #2, hold, key = temp$, using dp1, next%, eod goto dp1a
dp1:            FMT POS(21), BI(4)
            new% = false%
dp1a:       convert next% to scr_record_id$, pic(00000000)
            call "SHOSTAT" ("Saving record: " & scr_record_id$)
            next% = next% + 1%
            put #2 using dp1c, "UG NEXT REC ID", next%, " ", " "
dp1c:           FMT CH(20), BI(4), CH(250), CH(226)
            if new% = true% then write #2 else rewrite #2

dp2:    call "DATUFMTC" (scr_date_in$)
        call "DATUFMTC" (scr_entered_date$)
        call "DATUFMTC" (scr_resolved_date$)

        if new_record% = false% then                              ~
            read #1, hold, key = scr_record_id$

        put #1, using EWDUGLOG_fmt,                               ~
             scr_record_id$, scr_cust_nbr$, scr_date_in$,         ~
             scr_so_nbr$, scr_so_line$, scr_part_nbr$,            ~
             scr_po_nbr$, scr_bar_code$, scr_rcv_comment$,        ~
             scr_area_code$, scr_rsn_code$, scr_entered_by$,      ~
             scr_entered_date$, scr_resolved_flag$,               ~
             scr_new_so_nbr$, scr_sales_comment1$,                ~
             scr_sales_comment2$, scr_resolved_by$,               ~
             scr_resolved_date$, filler$
        
        if new_record% = true% then write #1 else rewrite #1

        last_saved$ = "Last Record ID: " & scr_record_id$

        if new_record% <> true% then return

    /* Now add to inventory and aged inventory                  */
        temp$ = scr_area_code$ : str(temp$,4%) = scr_part_nbr$
        new%  = true%
        qty   = 0
        read #7, hold, key = temp$, using dp3a, qty, eod goto dp3c
dp3a:       FMT POS(29), PD(15,4)
        new%  = false%
dp3c:   qty = qty + 1
        put #7, using EWDUGQTY_fmt,                              ~
                scr_area_code$, scr_part_nbr$, qty, " "
        if new% = true% then write #7 else rewrite #7


        temp$          = scr_area_code$
        str(temp$,4% ) = scr_part_nbr$
        str(temp$,29%) = scr_date_in$
        new%  = true%
        qty   = 0
        read #8, hold, key = temp$, using dp4a, qty, eod goto dp4c
dp4a:       FMT POS(35), PD(15,4)
        new%  = false%
dp4c:   qty = qty + 1
        put #8, using EWDUGAGE_fmt,                              ~
                scr_area_code$, scr_part_nbr$, scr_date_in$, qty, " "
        if new% = true% then write #8 else rewrite #8
        return


rem *************************************************************~
    *        F O R M A T    S T A T E M E N T S                 *~
    *-----------------------------------------------------------*~
    * FORMAT Statements for Data Files.                         *~
    *************************************************************

EWDUGLOG_fmt:                  /* FILE: EWDUGLOG                          */~
fmt pos(   1), ch(08),         /* Record Sequence Number                  */~
    pos(   9), ch(9),          /* Customer Number                         */~
    pos(  18), ch(06),         /* APCDUGLOG Date Entered                  */~
    pos(  24), ch(16),         /* Sales Order Number                      */~
    pos(  40), ch(3),          /* Sales Order Line Number                 */~
    pos(  43), ch(25),         /* Manufacturing Part Number               */~
    pos(  68), ch(16),         /* Customer Purchase Order Number          */~
    pos(  84), ch(30),         /* Scanned bar code                        */~
    pos( 114), ch(30),         /* Receiving Comment                       */~
    pos( 144), ch(3),          /* Holding Area Code                       */~
    pos( 147), ch(3),          /* Reason Code                             */~
    pos( 150), ch(3),          /* Entered By                              */~
    pos( 153), ch(06),         /* Date entered into the system            */~
    pos( 159), ch(1),          /* Resolved by sales flag (b/Y)            */~
    pos( 160), ch(16),         /* New Sales Order Number                  */~
    pos( 176), ch(30),         /* Sales Comment                           */~
    pos( 206), ch(30),         /* Sales Comment                           */~
    pos( 236), ch(3),          /* Resolved By User ID                     */~
    pos( 239), ch(06),         /* Resolved Date                           */~
    pos( 245), ch(140)         /* Unused Space                            */


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

    on fieldnr% gosub numeric,         /* Barcode           */ ~
                      upronly,         /* Record id         */ ~
                      upronly,         /* S.O. Number       */ ~
                      upronly,         /* Part Number       */ ~
                      upronly,         /* Customer Number   */ ~
                      upronly,         /* Customer P.O. Nbr */ ~
                      upronly,         /* Date In           */ ~
                      upronly,         /* Holding Area Code */ ~
                      upronly,         /* Reason Code       */ ~
                      uprlowr,         /* Receiving Comment */ ~
                      uprlowr,         /* Sales Info        */ ~
                      upronly,         /* Resolved?         */ ~
                      upronly,         /* New S.O. Nbr      */ ~
                      uprlowr,         /* Sales Comments    */ ~
                      uprlowr          /* Sales Comments    */

    if fieldnr% = 1% then lfac$(15%) = hex(99) 

  scr1_dsply
    accept                                                       ~
       at (01,02),                                               ~
          "Maintain Undelivered Goods Log",                      ~
       at (01,64), "Today:",                                     ~
       at (01,71), fac(hex(8C)), date$                  , CH(10),~
       at (02,02), fac(hex(AC)), line2$                 , CH(79),~
       at (04,02), fac(hex(94)), errormsg$              , CH(79),~
                                                                 ~
       at (05,49), fac(hex(8c)), last_saved$            , CH(30),~
                                                                 ~
       at (06,02), "Shipping Bar Code",                          ~
       at (06,30), fac(lfac$( 1)), scr_bar_code$        , CH(18),~
       at (06,50), fac(lfac$(15%)), wandchar$           , ch(01),~
                                                                 ~
       at (07,02), "Record ID     ",                             ~
       at (07,30), fac(lfac$( 2)), scr_record_id$       , CH(08),~
       at (07,49), "By ",                                        ~
       at (07,52), fac(hex(8C)), scr_entered_by$        , CH(03),~
       at (07,56), "on ",                                        ~
       at (07,59), fac(hex(8c)), scr_entered_date$      , CH(10),~
                                                                 ~
       at (08,02), "Sales Order, Line Number",                   ~
       at (08,30), fac(lfac$( 3)), scr_so_nbr$          , CH(08),~
       at (08,39), fac(lfac$( 3)), scr_so_line$         , CH(03),~
                                                                 ~
       at (09,02), "Manufacturing Part Number",                  ~
       at (09,30), fac(lfac$( 4)), scr_part_nbr$        , CH(25),~
                                                                 ~
                                                                 ~
       at (10,02), "Customer Number",                            ~
       at (10,30), fac(lfac$( 5)), scr_cust_nbr$        , CH(09),~
       at (10,49), fac(hex(8c)),   scr_cust_name$       , CH(30),~
                                                                 ~
       at (11,02), "Customer P.O. Number",                       ~
       at (11,30), fac(lfac$( 6)), scr_po_nbr$          , CH(16),~
                                                                 ~
       at (12,02), "Date In",                                    ~
       at (12,30), fac(lfac$( 7)), scr_date_in$         , CH(10),~
                                                                 ~
       at (13,02), "Holding Area Code",                          ~
       at (13,30), fac(lfac$( 8)), scr_area_code$       , CH(03),~
       at (13,49), fac(hex(8C)),   scr_area_codedescr$  , CH(32),~
                                                                 ~
       at (14,02), "Reason Code",                                ~
       at (14,30), fac(lfac$( 9)), scr_rsn_code$        , CH(03),~
       at (14,49), fac(hex(8C)),   scr_rsn_codedescr$   , CH(32),~
                                                                 ~
       at (15,02), "Receiving Comments",                         ~
       at (15,30), fac(lfac$(10)), scr_rcv_comment$     , CH(30),~
                                                                 ~
       at (16,02), "--- SALES INFO ---",                         ~
       at (16,30), fac(lfac$(11)), scr_sales_hdr$       , CH(40),~
                                                                 ~
       at (17,02), "Resolved?",                                  ~
       at (17,30), fac(lfac$(12)), scr_resolved_flag$   , CH(01),~
       at (17,49), fac(hex(8c)),   scr_resolved_info$   , CH(30),~
                                                                 ~
       at (18,02), "New Sales Order Number",                     ~
       at (18,30), fac(lfac$(13)), scr_new_so_nbr$      , CH(08),~
                                                                 ~
       at (19,02), "Sales Comments",                             ~
       at (19,30), fac(lfac$(14)), scr_sales_comment1$  , CH(30),~
       at (20,30), fac(lfac$(14)), scr_sales_comment2$  , CH(30),~
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
    pf$(2%) = "                   (11)Check Inventory                         (15)Print Screen"
    pf$(3%) = "                                                               (16)Save Data   "
    pfkeys$ = HEX(01FFFFFFFFFFFFFFFFFF0BFF0DFF0F1000)
    if caller$ = "sales" or new_record% = true% then goto spf1a
        str(pf$(3),20%,20%) = "(12)Delete Record"
        str(pfkeys$,12%,1%) = hex(0c)
spf1a: 
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
    on fieldnr% gosub td102,         /* Barcode                */~
                      td101,         /* Record Id              */~
                      td104,         /* S.O. Number            */~
                      td103,         /* Part Number            */~
                      td105,         /* Customer Number        */~
                      td106,         /* Customer P.O. Nbr      */~
                      td107,         /* Date In                */~
                      td108,         /* Holding Area Code      */~
                      td109,         /* Reason Code            */~
                      td110,         /* Receiving Comment      */~
                      td111,         /* Sales Info             */~
                      td112,         /* Resolved?              */~
                      td113,         /* New S.O. Nbr           */~
                      td114,         /* Sales Comments         */~
                      td115          /* Sales Comments         */
return

td101   /* Test for Record ID                    scr_record_id$      */
        /* Blank -> New, "?" = Getcode, else Load Data               */

        if scr_record_id$ <= " " and caller$ = "sales" then   ~
                                          scr_record_id$ = "?"
        if scr_record_id$ <= " " then return

        if str(scr_record_id$,1%,1%) <> "?" then td101a
            call "GETCODE" (#1, scr_record_id$, scr_cust_nbr$, ~
                            0%, 0.09, f1%)
            if f1% = 1% then goto td101b
                scr_record_id$ = " "
                errormsg$ = hex(00)
                return

td101a: convert scr_record_id$ to temp%, data goto td101e
        convert temp% to scr_record_id$, pic(00000000)

td101b: gosub dataload
        if errormsg$ > " " then return
            last_saved$ = " " 
            return clear all
            goto editpg1

td101e: errormsg$ = "RECORD ID must be numeric.  Please re-enter."
        return

td102   /* Test for SHIPPING BAR CODE       scr_bar_code$            */
        /* fmt = 1,8 = S.O., 9,2 = line, 11,4 = piece, 14,4 = of     */
        if scr_bar_code$ <= " " then return
        if len(scr_bar_code$) = 10% or len(scr_bar_code$) = 18% then td102a
            goto td102x
td102a: /* If full code entered, check to see that if it's on file   */
        if len(scr_bar_code$) <> 18% then td102c
            read #1, key 5 = scr_bar_code$, eod goto td102c
            scr_record_id$ = key(#1)
            gosub dataload
            return
td102c: /* Check out the Sales Order portion                        */
        convert str(scr_bar_code$,9%,2%) to temp%, data goto td102x
        temp$ = " "
        convert temp% to str(temp$,17%,3%), pic(##0)
        str(temp$,1%,16%) = str(scr_bar_code$,1%,8%)
        read #6, key = temp$, eod goto td102x
        get  #6, using td102e, scr_cust_nbr$, scr_so_nbr$,         ~
                 scr_so_line$, scr_part_nbr$
td102e:     FMT CH(9), CH(16), CH(3), POS(32), CH(25)
        temp$ = str(scr_cust_nbr$,1%,9%) & str(scr_bar_code$,1%,8%)
        read #5, key = temp$, using td102g, scr_po_nbr$, eod goto td102x
td102g:     FMT POS(26), CH(16)
        gosub dld_get_customer 
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
        return

td103x: errormsg$ = "Part number: " & temp$ & ".  Please re-enter."
        return

td104   /* Test for Sales Order, Line Number     scr_so_nbr$         */
        if scr_so_nbr$ > " " then goto td104b
            scr_so_line$ = " "
            return
td104b: temp$ = " "
        convert scr_so_line$ to temp%, data goto td104x
        convert temp% to scr_so_line$, pic(##0)
        str(temp$,17%,3%) = scr_so_line$
        convert scr_so_nbr$ to temp%, data goto td104x
        convert temp% to scr_so_nbr$, pic(00000000)
        str(temp$,1%,16%) = scr_so_nbr$
        read #6, key = temp$, eod goto td104x
        get  #6, using td104e, scr_cust_nbr$, scr_so_nbr$,         ~
                 scr_so_line$, scr_part_nbr$
td104e:     FMT CH(9), CH(16), CH(3), POS(32), CH(25)
        temp$ = str(scr_cust_nbr$,1%,9%) & scr_so_nbr$
        read #5, key = temp$, using td104g, scr_po_nbr$, eod goto td104x
td104g:     FMT POS(26), CH(16) 
        gosub dld_get_customer
        return

td104x: errormsg$ = "Sales Order entered is not valid."
        return


td105   /* Test for Customer Number              scr_cust_nbr$       */
        if scr_cust_nbr$ <= " " then return
        call "GETCODE" (#4, scr_cust_nbr$, scr_cust_name$, 0%, 0.0, f1%)
        if f1% = 1% then return
            errormsg$ = hex(00)
            return

td106   /* Test for Customer P.O. Number         scr_po_nbr$         */
        return

td107   /* Test for Date In                      scr_date_in$        */
        call "DATEOKC" (scr_date_in$, temp1%, errormsg$)
        if errormsg$ > " " then return
            temp$ = date$
            call "DATEOKC" (temp$, temp2%, errormsg$)
            if temp1% > temp2% then ~
                    errormsg$ = "Date cannot be after today."
            if temp1% < 19980101% then ~
                    errormsg$ = "Date cannot be before 01/01/1998."
        return

td108   /* Test for Holding Area Code            scr_area_code$      */
        scr_area_codedescr$ = " "
        temp$ = "UG AREAS " & scr_area_code$
        call "PLOWCODE" (#3, temp$, scr_area_codedescr$, ~
                            9%, 0.30, f1%)
            if f1% = 1% then scr_area_code$ = str(temp$,10%)
            if f1% = 1% then return
                scr_area_code$ = " "
                errormsg$ = hex(00)
                return

td109   /* Test for Reason Code                  scr_rsn_code$       */
        scr_rsn_codedescr$ = " "
        temp$ = "UG REASON" & scr_rsn_code$
        call "PLOWCODE" (#3, temp$, scr_rsn_codedescr$, ~
                            9%, 0.30, f1%)
            if f1% = 1% then scr_rsn_code$ = str(temp$,10%)
            if f1% = 1% then return
                scr_rsn_code$ = " "
                errormsg$ = hex(00)
                return

td110   /* Test for Receiving Comments           scr_rcv_comment$    */
        return

td111   /* Test for Sales Info                   scr_sales_hdr$      */
        return

td112   /* Test for Resolved?                    scr_resolved_flag$  */
        if scr_resolved_flag$ = save_resolved_flag$ then return

        if scr_resolved_flag$ <> " " then scr_resolved_flag$ = "Y"
        scr_resolved_by$   = userid$
        scr_resolved_date$ = date$
        scr_resolved_info$ = "Changed by " & userid$ & " on " & date$
        return

td113   /* Test for New Sales Order Number       scr_new_so_nbr$     */
        if scr_new_so_nbr$ <= " " then return

        convert scr_new_so_nbr$ to temp%, data goto td113x
        convert temp% to scr_new_so_nbr$, pic(00000000)
        temp$ = scr_new_so_nbr$ & hex(00)
        read #6, key > temp$, eod goto td113x
        get  #6, using td113b, temp$
td113b:     FMT POS(10), CH(16)
        if temp$ <> scr_new_so_nbr$ then goto td113x
        return

td113x: errormsg$ = "Invalid New Sales Order"
        return

td114   /* Test for Sales Comments               scr_sales_comment1$ */
td115   /* Test for Sales Comments               scr_sales_comment2$ */
        return

exit_program

   end
