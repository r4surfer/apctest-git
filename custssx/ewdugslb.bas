rem *************************************************************~
    *                                                           *~
    *  EEEEE  W   W  DDDD   U   U   GGG    ssss  L      BBBB    *~
    *  E      W   W  D   D  U   U  G      s      L      B   B   *~
    *  EEEE   W   W  D   D  U   U  G GGG  ssss   L      BBBB    *~
    *  E      W W W  D   D  U   U  G   G      s  L      B   B   *~
    *  EEEEE   W W   DDDD    UUU    GGG   ssss   LLLLL  BBBB    *~
    *                                                           *~
    *-----------------------------------------------------------*~
    * EWDUGSlB - Query/Management of then U.G. Log file         *~
    *            EWDUGLOG. Mainly intended for use by Sales,    *~
    *            but OK for the general public, too.            *~
    *                                                           *~
    *            Calls EWDUGLGB to do the actual maintenance.   *~
    *                                                           *~
    *-----------------------------------------------------------*~
    *                  M O D I F I C A T I O N S                *~
    *---WHEN---+----------------WHAT----------------------+-WHO-*~
    *          !                                          !     *~      
    * 06/22/98 ! Original                                 ! ERN *~
    *          !                                          !     *~
    *************************************************************

    sub "EWDUGSLB" (caller$, userid$)

rem  caller$    = "shop" or "sales" or "admin"
rem  user_id$   = Current user.  Extracted if passed empty.

rem     Basic Variable Naming Conventions
rem     -----------------------------------------------------------
rem         scr- prefixed variables are search parameters entered
rem              on the screen.
rem         lst- prefixed variables are arrays that contain the
rem              results of a query.
rem         Data read from disk (single record) is not prefixed.
rem         The names between the 3 groups coincide.  Other variables
rem         are standard Caelus.

    dim                                                              ~
        arg_record_id$8,             /* For passing to LGB         */~
        blankdate$8,                 /* Millennium comparisons     */~
        caller$6,                    /* Who's using: shop or sales */~
        cursor%(2),                  /* Cursor location for edit   */~
        date$10,                     /* Date for screen display    */~
        edtmessage$79,               /* Edit screen message        */~
        errormsg$79,                 /* Error message              */~
        i$(24)80,                    /* Screen Image               */~
        inpmessage$79,               /* Informational Message      */~
        kwi$50,                      /* Key variable               */~
        lfac$(20)1,                  /* Field Attribute Characters */~
        line2$79,                    /* Screen Line #2             */~
        pf$(3)79,                    /* PF Screen Literals         */~
        pfkeyS$32,                   /* PF Key Hex Values          */~
        save_resolved_flag$1,        /* Original state             */~
        scr_area_code$3,             /* Holding Area Code          */~
        scr_area_codedescr$32,       /* Holding Area Code          */~
        scr_bar_code$30,             /* Manufacturing Bar Code     */~
        scr_cust_nbr$9,              /* Customer Number            */~
        scr_cust_name$30,            /*          Name              */~
        scr_date_in$10,              /* Date In                    */~
        scr_dte_in$10,               /*         - packed           */~
        scr_new_so_nbr$16,           /* New Sales Order Number     */~
        scr_part_nbr$25,             /* Manufacturing Part Number  */~
        scr_po_nbr$16,               /* Customer P.O. Number       */~
        scr_record_id$08,            /* Record ID                  */~
        scr_resolved_flag$1,         /* Resolved?                  */~
        scr_resolved_by$3,           /*          By                */~
        scr_resolved_date$10,        /*          Date              */~
        scr_resolved_info$30,        /*          By and Date       */~
        scr_rsn_code$3,              /* Reason Code                */~
        scr_rsn_codedescr$32,        /* Reason Code                */~
        scr_sales_hdr$40,            /* Sales Info                 */~
        scr_so_nbr$16, scr_so_line$3,/* Sales Order, Line Number   */~
        temp$50, temp2$50,           /* Temporary working storage  */~
        userid$3                     /* Current User Id            */

    dim                              /* DISK VARIABLES             */~
        area_code$3,                 /* Holding Area Code          */~
        bar_code$30,                 /* Manufacturing Bar Code     */~
        cust_nbr$9,                  /* Customer Number            */~
        date_in$10,                  /* Date In                    */~
        entered_by$3,                /* Entered By User ID         */~
        entered_date$10,             /* Entered Date (system)      */~
        new_so_nbr$16,               /* New Sales Order Number     */~
        part_nbr$25,                 /* Manufacturing Part Number  */~
        po_nbr$16,                   /* Customer P.O. Number       */~
        rcv_comment$30,              /* Receiving Comments         */~
        record_id$08,                /* Record ID                  */~
        resolved_flag$1,             /* Resolved?                  */~
        resolved_by$3,               /*          By                */~
        resolved_date$10,            /*          Date              */~
        rsn_code$3,                  /* Reason Code                */~
        sales_comment1$30,           /* Sales Comments             */~
        sales_comment2$30,           /* Sales Comments             */~
        so_nbr$16, so_line$3         /* Sales Order, Line Number   */


    dim                              /* LIST SCREEN VARIABLES      */~
        lst$(5,3)79,                 /* Screen Display             */~
        lst_area_code$    (999)3,    /* Holding Area Code          */~
        lst_bar_code$     (999)30,   /* Manufacturing Bar Code     */~
        lst_cust_nbr$     (999)9,    /* Customer Number            */~
        lst_date_in$      (999)10,   /* Date In                    */~
        lst_new_so_nbr$   (999)16,   /* New Sales Order Number     */~
        lst_part_nbr$     (999)25,   /* Manufacturing Part Number  */~
        lst_po_nbr$       (999)16,   /* Customer P.O. Number       */~
        lst_record_id$    (999)08,   /* Record ID                  */~
        lst_resolved_flag$(999)1,    /* Resolved?                  */~
        lst_resolved_by$  (999)3,    /*          By                */~
        lst_rsn_code$     (999)3,    /* Reason Code                */~
        lst_so_nbr$       (999)16,   /* Sales Order, Line Number   */~
        lst_so_line$      (999)3

    dim f2%(64),                     /* = 0 if the file is open    */~
        fs%(64),                     /* = 1 if file open, -1 if it */~
                                     /*   doesn't exist, or 0 if   */~
                                     /*   not yet checked (OPENCHCK*/~
        rslt$(64)20                  /* Text from file opening     */

rem *************************************************************~
    *                  Release Version ID Section               *~
    *************************************************************
    dim cms2v$50
    cms2v$ = "07.00.00 09/22/98  Pre-Release Version            "
rem *************************************************************

    mat f2% = con

rem *************************************************************~
    *                  S E L E C T   F I L E S                  *~
    *-----+----------+------------------------------------------*~
    *File#!  PRName  ! File Description                         *~
    *-----+----------+------------------------------------------*~
    * #01 ! EWDUGLOG ! Undelivered Goods Log File               *~
    * #03 ! GENCODES ! System General Codes file.               *~
    * #04 ! CUSTOMER ! Customer Master                          *~
    * #05 ! BCKMASTR ! S.O. Header Master File                  *~
    * #06 ! BCKLINES ! BACK LOG LINE ITEM FILE                  *~
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


   call "SHOSTAT" ("Opening UGS Files, One Moment Please")

   call "OPENCHCK" (#01, fs%(01%), f2%(01%), 1000%, rslt$(01%))
   call "OPENCHCK" (#03, fs%(03%), f2%(03%),    0%, rslt$(03%))
   call "OPENCHCK" (#04, fs%(04%), f2%(04%),    0%, rslt$(04%))
   call "OPENCHCK" (#05, fs%(05%), f2%(05%),    0%, rslt$(05%))
   call "OPENCHCK" (#06, fs%(06%), f2%(06%),    0%, rslt$(06%))


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

    str(line2$,60%) = "EWDUGSLB: " & STR(CMS2V$,,8)

    if caller$ <> "sales" and ~ 
       caller$ <> "admin" then caller$ = "shop"
  
    lst_max% = 990%   /* Cheap way to insure no overflow */

rem *************************************************************~
    *       I N P U T   M O D E   M A I N   P R O G R A M       *~
    *-----------------------------------------------------------*~
    * Handles normal input for data entry screens.              *~
    *************************************************************

rem ************************************
rem Note that we start in EDIT MODE...
rem ************************************


inputmode
       gosub initialize_variables

rem *************************************
       goto editpg1
rem *************************************

       for fieldnr% = 1% TO 11%
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
           if keyhit%  = 14% then       do_query
           if keyhit%  = 16% then       exit_program
           if keyhit% <>  0% then       editpg1
edt1a: fieldnr% = cursor%(1%) - 5%
       if fieldnr% < 1% OR fieldnr% > 11% then editpg1
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


rem *************************************************************~
    *                 D O    Q U E R Y                          *~
    *-----------------------------------------------------------*~
    * Execute the query.  Results go into arrays.               *~
    *************************************************************~
    * From the entries made, try to figure out which records    *~
    * apply.  Entries that indicate a specific record, such as  *~
    * Record ID, do not come down here as they are taken care of*~
    * already.                                                  *~
    * We're going to do this in one pass.  First, we see if     *~
    * theres a key to access the file by.  If not, we pass the  *~
    * entire file.  Viva la performanca!                        *~
    *************************************************************

do_query:

    call "SHOSTAT" ("Performing Query.  One (or two) Moments, please.")

    kwi%     = 0%
    kwi$     = all(hex(00))
    break%   = 0%

    gosub init_lst

rem ***********************************************************************
rem  I have been unable to get the 'optimized' search -- that is, one that
rem  takes advantage of indexes -- to work.  The following code starts the
rem  reads at the beginning of the file.  It is then read sequentially all
rem  the way through...  Hopefully, this will be fixed before the file's
rem  too large.  To see what the problem is, just remark these two guys.
rem ***********************************************************************
    read #1, key > kwi$, eod goto q2z
    goto q2b
rem **************

    if scr_bar_code$ = " " then q1b
        kwi% = 5%
        break% = len(scr_bar_code$)
        str(kwi$,1%,break%) = scr_bar_code$
        goto q2a
q1b: if scr_part_nbr$ = " " then q1d
        kwi% = 3%
        break% = len(scr_part_nbr$)
        str(kwi$,1%,break%) = scr_part_nbr$
        goto q2a
q1d: if scr_so_nbr$ = " " then q1f
        kwi% = 2%
        break% = len(scr_so_nbr$)
        str(kwi$,1%,break%) = scr_so_nbr$
        goto q2a
q1f: if scr_po_nbr$ = " " then q1h
        kwi% = 4%
        break% = len(scr_po_nbr$)
        str(kwi$,1%,break%) = scr_po_nbr$
        goto q2a
q1h: if scr_cust_nbr$ = " " then q2a
        kwi% = 1%
        break% = len(scr_cust_nbr$)
        str(kwi$,1%,break%) = scr_cust_nbr$


q2a:
    call "PLOWALTS" (#1, kwi$, kwi%, break%, f1%)
    if f1% = 0% then q2z
    goto q2b

q2_loop:
    read #1, eod goto q2z, data goto q2_loop
q2b:
    gosub dataload

  /* "If we are accessing by a key, then get out ASAP... */
    if kwi% = 1% and cust_nbr$ <> scr_cust_nbr$ then q2z
    if kwi% = 2% and so_nbr$   <> scr_so_nbr$   then q2z
    if kwi% = 3% and part_nbr$ <> scr_part_nbr$ then q2z
    if kwi% = 4% and po_nbr$   <> scr_po_nbr$   then q2z
    if kwi% = 5% and bar_code$ <> scr_bar_code$ then q2z

   /* Check query criteria...  wild carding is not supported.             */
   /*   I was going to do this as one large if statement, however I doubt */
   /*   the compiler could handle it.  At any rate, this code section     */
   /*   is obviously the work of an anal-retentive mind - so orderly!     */
    if scr_cust_nbr$       > " " and cust_nbr$      <> scr_cust_nbr$   then q2_loop
    if scr_so_nbr$         > " " and so_nbr$        <> scr_so_nbr$     then q2_loop
    if scr_part_nbr$       > " " and part_nbr$      <> scr_part_nbr$   then q2_loop
    if scr_po_nbr$         > " " and po_nbr$        <> scr_po_nbr$     then q2_loop
    if scr_bar_code$       > " " and bar_code$      <> scr_bar_code$   then q2_loop
    if scr_so_line$        > " " and so_line$       <> scr_so_line$    then q2_loop 
    if scr_date_in$        > " " and date_in$       <> scr_dte_in$     then q2_loop
    if scr_area_code$      > " " and area_code$     <> scr_area_code$  then q2_loop
    if scr_rsn_code$       > " " and rsn_code$      <> scr_rsn_code$   then q2_loop
    if scr_resolved_flag$  = "N" and resolved_flag$ <> " "             then q2_loop
    if scr_resolved_flag$  = "Y" and resolved_flag$ <> "Y"             then q2_loop
    if scr_new_so_nbr$     > " " and new_so_nbr$    <> scr_new_so_nbr$ then q2_loop

  /* Ooops, found one.  Aber, auf Deutsch-- */
  /* Ach du meine Gutte! Wir haben eins!    */
    lst_cur% = lst_cur% + 1%
    c%       = lst_cur%
    
    call "DATEFMT" (date_in$)       /* Fmtd w/o CC to save space  */
    call "DATEFMT" (entered_date$)
    call "DATEFMT" (resolved_date$)

    lst_record_id$      (c%) = record_id$
    lst_bar_code$       (c%) = bar_code$
    lst_part_nbr$       (c%) = part_nbr$
    lst_so_nbr$         (c%) = so_nbr$
    lst_so_line$        (c%) = so_line$
    lst_po_nbr$         (c%) = po_nbr$
    lst_cust_nbr$       (c%) = cust_nbr$
    lst_date_in$        (c%) = date_in$
    lst_area_code$      (c%) = area_code$
    lst_rsn_code$       (c%) = rsn_code$
    lst_resolved_flag$  (c%) = resolved_flag$
    lst_new_so_nbr$     (c%) = new_so_nbr$

    if c% = lst_max% then q2x

    goto q2_loop

q2x:
    call "ASKUSER" (c%, "QUERY RESULT SET TOO LARGE",            ~
                    "The query resulted in too many records.",   ~
                    "Therefore, the listing is incomplete.  ",   ~
                    "To remedy, make the query more specific.")   
    goto show_query

q2z:
    if lst_cur% > 0% then show_query

        call "ASKUSER" (c%, "No Records Meet Criteria",           ~
                        "No records were found that matched the", ~
                        "Query criteria.  Press RETURN...      ")
        goto editpg1
    
rem *************************************************************~
    *                 S H O W   Q U E R Y                       *~
    *-----------------------------------------------------------*~
    * Display the query and allow selction to show record.      *~
    *************************************************************

show_query
       lst_top%     = 1%   /* Index of first line one screen */
       lst_per_scr% = 5%   /* Number of lines per screen     */

show_query_loop
       gosub'102          /* Display Screen - No Entry */
           if keyhit%  =  1% then gosub startover
           if keyhit%  =  2% then lst_top% = 1%
           if keyhit%  =  3% then lst_top% = max(1%, lst_cur% - lst_per_scr% + 1%)
           if keyhit%  =  4% then lst_top% = max(1%, lst_top% - lst_per_scr%)
           if keyhit%  =  5% then lst_top% = min(lst_cur%, lst_top% + lst_per_scr%)
           if keyhit%  =  6% then lst_top% = max(1%, lst_top% - 1%)        
           if keyhit%  =  7% then lst_top% = min(lst_cur%, lst_top% + 1%)
       rem if keyhit%  = 14% then       do_report
           if keyhit%  = 16% then       editpg1
           if keyhit% <>  0% then       show_query_loop
  rem Translate cursor position to index in array...
       fieldnr% = int((cursor%(1%) - 2%) / 3%)
       if fieldnr% < 1% OR fieldnr% > 5% then show_query_loop
       c% = lst_top% + fieldnr% - 1%
       arg_record_id$ = lst_record_id$(c%)
       
  rem Show with a call to lgb
        gosub call_lgb

        goto show_query_loop



rem *************************************************************~
    *     D E F A U L T / E N A B L E   F O R   P A G E   1     *~
    *-----------------------------------------------------------*~
    * Sets DEFAULTS and ENABLES fields for Screen  1  of Input. *~
    *************************************************************

deffn'051(fieldnr%)
    enabled% = true%
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


scrn1_msg  :  data                                               ~
       "Enter RECORD ID -or- '?' for listing.                  ",~
       "Enter BAR CODE.                                        ",~
       "Enter Manufacturing PART NUMBER.                       ",~
       "Enter SALES ORDER and Sales Order LINE NUMBER.         ",~
       "Enter CUSTOMER NUMBER.                                 ",~
       "Enter Customer P.O. NUMBER.                            ",~
       "Enter DATE when goods moved into a holding area.       ",~
       "Enter HOLDING AREA Code to restrict query to.          ",~
       "Enter REASON Code.                                     ",~
       "Enter RESOLVED? ( 'Y', 'N',  or (A)ll )                ",~
       "Enter NEW SALES ORDER Number.                          "


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
              scr_po_nbr$, scr_so_line$,                         ~
              scr_resolved_flag$, scr_rsn_code$,                 ~
              scr_rsn_codedescr$,                                ~
              scr_sales_hdr$, scr_so_nbr$,                       ~
              scr_cust_name$,   save_resolved_flag$,             ~
              scr_resolved_info$, scr_resolved_by$,              ~
              scr_resolved_date$

    init(" ") area_code$, bar_code$, cust_nbr$, date_in$,         ~
              entered_by$, entered_date$, new_so_nbr$, part_nbr$, ~
              po_nbr$, rcv_comment$, record_id$, resolved_flag$,  ~
              resolved_by$, resolved_date$, rsn_code$,            ~
              sales_comment1$, sales_comment2$, so_nbr$, so_line$

    gosub init_lst

    scr_dte_in$ = blankdate$

    scr_resolved_flag$ = "A"

return


init_lst
    init(" ") lst_area_code$(), lst_bar_code$(), lst_cust_nbr$(),   ~
              lst_date_in$(), lst_new_so_nbr$(), lst_part_nbr$(),   ~
              lst_po_nbr$(), lst_record_id$(), lst_resolved_flag$(),~
              lst_resolved_by$(), lst_rsn_code$(),                  ~
              lst_so_nbr$(), lst_so_line$()
 
    lst_cur% = 0%
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


        get #1, using EWDUGLOG_fmt,                               ~
             record_id$, cust_nbr$, date_in$,                     ~
             so_nbr$, so_line$, part_nbr$,  po_nbr$, bar_code$,   ~ 
             rcv_comment$, area_code$, rsn_code$, entered_by$,    ~
             entered_date$, resolved_flag$, new_so_nbr$,          ~
             sales_comment1$, sales_comment2$,                    ~
             resolved_by$, resolved_date$

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

    on fieldnr% gosub upronly,         /* Reccord ID        */ ~
                      numeric,         /* Bar Code          */ ~
                      upronly,         /* Part Number       */ ~
                      upronly,         /* S.O. Number       */ ~
                      upronly,         /* Customer Number   */ ~
                      upronly,         /* Customer P.O. Nbr */ ~
                      upronly,         /* Date In           */ ~
                      upronly,         /* Holding Area Code */ ~
                      upronly,         /* Reason Code       */ ~
                      upronly,         /* Resolved?         */ ~
                      upronly          /* New S.O. Nbr      */ 

  scr1_dsply
    accept                                                       ~
       at (01,02),                                               ~
          "Query/Manage Undelivered Goods Log",                  ~
       at (01,64), "Today:",                                     ~
       at (01,71), fac(hex(8C)), date$                  , CH(10),~
       at (02,02), fac(hex(AC)), line2$                 , CH(79),~
       at (04,02), fac(hex(94)), errormsg$              , CH(79),~
                                                                 ~
       at (06,02), "Record ID     ",                             ~
       at (06,30), fac(lfac$( 1)), scr_record_id$       , CH(08),~
                                                                 ~   
       at (07,02), "Shipping Bar Code",                          ~
       at (07,30), fac(lfac$( 2)), scr_bar_code$        , CH(18),~
                                                                 ~
       at (08,02), "Manufacturing Part Number",                  ~
       at (08,30), fac(lfac$( 3)), scr_part_nbr$        , CH(25),~
                                                                 ~
       at (09,02), "Sales Order, Line Number",                   ~
       at (09,30), fac(lfac$( 4)), scr_so_nbr$          , CH(08),~
       at (09,39), fac(lfac$( 4)), scr_so_line$         , CH(03),~
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
       at (15,02), "Resolved?",                                  ~
       at (15,30), fac(lfac$(10)), scr_resolved_flag$   , CH(01),~
       at (15,49), fac(hex(8c)),   scr_resolved_info$   , CH(30),~
                                                                 ~
       at (16,02), "New Sales Order Number",                     ~
       at (16,30), fac(lfac$(11)), scr_new_so_nbr$      , CH(08),~
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
    pf$(2%) = "                                     (14) Execute Query        (15)Print Screen"
    pf$(3%) = "                                                               (16)Exit Program"
    pfkeys$ = HEX(01FFFFFFFFFFFFFFFFFFFFFF0D0E0F1000)
    goto setpf_pg1_edtexit

  setpf_pg1_edtfld
    pf$(1%) = "(1)Start Over                                                  (13)Instructions"
    pf$(2%) = "                                                               (15)Print Screen"
    pf$(3%) = "                                                                               "
    pfkeys$ = HEX(01FFFFFFFFFFFFFFFFFFFFFF0DFF0FFF00)

  setpf_pg1_edtexit
return


rem *************************************************************~
    *               S C R E E N   L I S T   Q U E R Y           *~
    *-----------------------------------------------------------*~
    * Display results of Query                                  *~
    *************************************************************

deffn'102    

    gosub lst_setpf
    inpmessage$ = "To manage a record, position cursor and press RETURN."
    init(hex(8C)) lfac$()

rem Stuff stuff into stuff...
    init(" ") lst$()
    for r% = 1% to 5%
        c% = lst_top% + r% -1%
        if c% > lst_cur% then L01

     /* COLUMN 1 */
        str(lst$(r%,1%)) = "ID: " & lst_record_id$(c%)
        
        str(lst$(r%,2%),5%) = lst_date_in$(c%)

        if lst_resolved_flag$(c%) = "Y" then ~
            str(lst$(r%,3%),5%) = "Resolved" else ~
            str(lst$(r%,3%),5%) = "Unrsolvd"

    /* COLUMN 2 */
        str(lst$(r%,1%),14%) = "Area: " & lst_area_code$(c%)
        str(lst$(r%,2%),14%) = " Rsn: " & lst_rsn_code$(c%)
        str(lst$(r%,3%),14%) = " "

    /* COLUMN 3 */
        str(lst$(r%,1%),24%) = "Part: " & lst_part_nbr$(c%)
        str(lst$(r%,2%),24%) = "Cust: " & lst_cust_nbr$(c%)
        str(lst$(r%,3%),24%) = "S.O.: " & lst_so_nbr$  (c%) &   ~
                               " "      & lst_so_line$ (c%)


    /* COLUMN 4 */

        str(lst$(r%,2%),52%) = "P.O. Nbr: " & lst_po_nbr$(c%)
        str(lst$(r%,3%),52%) = "New SO #: " & lst_new_so_nbr$(c%)

        lfac$(r%) = hex(AC)
    next r%


L01:
    accept                                                       ~
       at (01,02),                                               ~
          "Query/Manage Undelivered Goods Log",                  ~
       at (01,64), "Today:",                                     ~
       at (01,71), fac(hex(8C)), date$                  , CH(10),~
       at (02,02), fac(hex(AC)), line2$                 , CH(79),~
       at (04,02), fac(hex(94)), errormsg$              , CH(79),~
                                                                 ~
       at (05,02), fac(hex(84)),  lst$(1%,1%)           , CH(79),~
       at (06,02), fac(hex(8c)),  lst$(1%,2%)           , CH(79),~
       at (07,02), fac(lfac$(1)), lst$(1%,3%)           , CH(79),~
                                                                 ~ 
       at (08,02), fac(hex(84)),  lst$(2%,1%)           , CH(79),~
       at (09,02), fac(hex(8c)),  lst$(2%,2%)           , CH(79),~
       at (10,02), fac(lfac$(2)), lst$(2%,3%)           , CH(79),~
                                                                 ~ 
       at (11,02), fac(hex(84)),  lst$(3%,1%)           , CH(79),~
       at (12,02), fac(hex(8c)),  lst$(3%,2%)           , CH(79),~
       at (13,02), fac(lfac$(3)), lst$(3%,3%)           , CH(79),~
                                                                 ~ 
       at (14,02), fac(hex(84)),  lst$(4%,1%)           , CH(79),~
       at (15,02), fac(hex(8c)),  lst$(4%,2%)           , CH(79),~
       at (16,02), fac(lfac$(4)), lst$(4%,3%)           , CH(79),~
                                                                 ~ 
       at (17,02), fac(hex(84)),  lst$(5%,1%)           , CH(79),~
       at (18,02), fac(hex(8c)),  lst$(5%,2%)           , CH(79),~
       at (19,02), fac(hex(8c)),  lst$(5%,3%)           , CH(79),~
                                                                 ~   
       at (21,02), fac(hex(A4)),   inpmessage$          , CH(79),~
       at (22,02), fac(hex(8C)),   pf$(1%)              , CH(79),~
       at (23,02), fac(hex(8C)),   pf$(2%)              , CH(79),~
       at (24,02), fac(hex(8C)),   pf$(3%)              , CH(79),~
                                                                 ~
       keys(pfkeys$), key(keyhit%)

        if keyhit% = 13% or keyhit% = 15% then gosub man_or_prtscr
        if keyhit% = 13% or keyhit% = 15% then goto  scr1_dsply
        gosub get_screen
return


lst_setpf
    /* Input Mode */
    pf$(1%) = "(1)Start Over                                                  (13)Instructions"
    pf$(2%) = "(2)First    (4)Previous    (6)Up                               (15)Print Screen"
    pf$(3%) = "(3)Last     (5)Next        (7)Down                             (16)Exit Queue  "
    pfkeys$ = hex(0102030405060708FFFFFFFF0D140F1000)
return



rem *************************************************************~
    *                     T E S T   D A T A                     *~
    *-----------------------------------------------------------*~
    * Test data for the items on Screen 1.                      *~
    *************************************************************

deffn'151(fieldnr%)
    errormsg$ = " "
    on fieldnr% gosub td101,         /* Record ID              */~
                      td102,         /* Bar Code               */~
                      td103,         /* Part Number            */~
                      td104,         /* S.O. Number            */~
                      td105,         /* Customer Number        */~
                      td106,         /* Customer P.O. Nbr      */~
                      td107,         /* Date In                */~
                      td108,         /* Holding Area Code      */~
                      td109,         /* Reason Code            */~
                      td112,         /* Resolved?              */~
                      td113          /* New S.O. Nbr           */ 
return

        /* ----------------------------------------------------------*/
td101   /* Test for Record ID                    scr_record_id$      */
        /* ----------------------------------------------------------*/
        /* "?" = Getcode, else check if record on file               */

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

td101b: read #1, key = scr_record_id$, eod goto td101f
        arg_record_id$ = scr_record_id$
        gosub call_lgb
        return clear all
        goto  inputmode

td101e: errormsg$ = "RECORD ID must be numeric.  Please re-enter."
        return

td101f: errormsg$ = "RECORD ID not on file.  Please re-specify."
        return

        /* ----------------------------------------------------------*/
td102   /* Test for SHIPPING BAR CODE       scr_bar_code$            */
        /* ----------------------------------------------------------*/
        /* fmt = 1,8 = S.O., 9,2 = line, 11,4 = piece, 14,4 = of     */
        if scr_bar_code$ <= " " then return

        if len(scr_bar_code$) = 10% or len(scr_bar_code$) = 18% then td102a
            goto td102x

td102a: /* If full code entered, check to see that if it's on file   */
        if len(scr_bar_code$) <> 18% then td102c
            read #1, key 5 = scr_bar_code$, eod goto td102x
            arg_record_id$ = key(#1)
            gosub call_lgb
            return clear all
            goto inputmode

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


        /* ----------------------------------------------------------*/
td103   /* Test for Manufacturing Part Number    scr_part_nbr$       */
        /* ----------------------------------------------------------*/
        if scr_part_nbr$ <= " " then return

        temp$ = "invalid length"
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

        temp$ = "no such part in log file"
        read #1, key 3 = scr_part_nbr$, eod goto td103x
        return

td103x: errormsg$ = "Part number: " & temp$ & ".  Please re-enter."
        return

        /* ----------------------------------------------------------*/
td104   /* Test for Sales Order, Line Number     scr_so_nbr$         */
        /* ----------------------------------------------------------*/
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

        /* ----------------------------------------------------------*/
td105   /* Test for Customer Number              scr_cust_nbr$       */
        /* ----------------------------------------------------------*/
        scr_cust_name$ = " "
        if scr_cust_nbr$ <= " " then return
        call "GETCODE" (#4, scr_cust_nbr$, scr_cust_name$, 0%, 0.0, f1%)
        if f1% = 1% then return
            errormsg$ = hex(00)
            return

        /* ----------------------------------------------------------*/
td106   /* Test for Customer P.O. Number         scr_po_nbr$         */
        /* ----------------------------------------------------------*/
        return

        /* ----------------------------------------------------------*/
td107   /* Test for Date In                      scr_date_in$        */
        /* ----------------------------------------------------------*/
        scr_dte_in$ = blankdate$
        if scr_date_in$ = " " then return

        call "DATEOKC" (scr_date_in$, temp1%, errormsg$)
        if errormsg$ > " " then return
            temp$ = date$
            call "DATEOKC" (temp$, temp2%, errormsg$)
            if temp1% > temp2% then ~
                    errormsg$ = "Date cannot be after today."
            if temp1% < 19980101% then ~
                    errormsg$ = "Date cannot be before 01/01/1998."
            scr_dte_in$ = scr_date_in$
            call "DATUFMTC" (scr_dte_in$)
        return

        /* ----------------------------------------------------------*/
td108   /* Test for Holding Area Code            scr_area_code$      */
        /* ----------------------------------------------------------*/
        scr_area_codedescr$ = " " 
        if scr_area_code$   = " " then return

        temp$ = "UG AREAS " & scr_area_code$
        call "PLOWCODE" (#3, temp$, scr_area_codedescr$, ~
                            9%, 0.30, f1%)
            if f1% = 1% then scr_area_code$ = str(temp$,10%)
            if f1% = 1% then return
                scr_area_code$ = " "
                errormsg$ = hex(00)
                return

        /* ----------------------------------------------------------*/
td109   /* Test for Reason Code                  scr_rsn_code$       */
        /* ----------------------------------------------------------*/
        scr_rsn_codedescr$ = " "
        if scr_rsn_code$   = " " then return

        temp$ = "UG REASON" & scr_rsn_code$
        call "PLOWCODE" (#3, temp$, scr_rsn_codedescr$, ~
                            9%, 0.30, f1%)
            if f1% = 1% then scr_rsn_code$ = str(temp$,10%)
            if f1% = 1% then return
                scr_rsn_code$ = " "
                errormsg$ = hex(00)
                return

        /* ----------------------------------------------------------*/
td112   /* Test for Resolved?                    scr_resolved_flag$  */
        /* ----------------------------------------------------------*/

        if scr_resolved_flag$ = "N" or            ~
           scr_resolved_flag$ = "Y" or            ~
           scr_resolved_flag$ = "A" then return

            errormsg$ = "Invalid RESOLVED Flag.  Please enter 'Y', 'N', or 'A'll."
            return


        /* ----------------------------------------------------------*/
td113   /* Test for New Sales Order Number       scr_new_so_nbr$     */
        /* ----------------------------------------------------------*/
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


rem *************************************************************
rem * Subroutines                                               *
rem *************************************************************

call_lgb
    call "EWDUGLGB" (caller$, userid$, arg_record_id$)
    return

exit_program

   end
