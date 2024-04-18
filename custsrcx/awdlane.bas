        REM *****************************************************************~
            *                                                               *~
            *  Program Name      - AWDLANE                                  *~
            *  Creation Date     - 10/12/2018                               *~
            *  Last Modified Date-                                          *~
            *  Written By        - Ricky Beane                              *~
            *  Last Mod By       -                                          *~
            *                                                               *~
            *  Description       - Scanning Utility used to track users     *~
            *                      scanning windows to a lane at DC.        *~
            *                                                               *~
            *                                                               *~
            *  Special Comments  -                                          *~  
            *  Program screen layout made for hand-held scanning in Mocha   *~
            *  session on a new Android OS scanner by Zebra.                *~
            *                                                               *~
            *---------------------------------------------------------------*~
            *                    M O D I F I C A T I O N S                  *~
            *----WHEN----+----------------WHAT------------------------+-WHO-*~
            * 10/12/2018 ! New Program - Last Mod Date                ! RDB *~
            * 03/07/2019 ! CR1952 Pickup limit of 500 extended        ! RDB *~
            *****************************************************************
        dim schema$8,                    /* Schema NC or TX            */~
            mln_key$20, mln_key1$5,      /* Master Lane key            */~
            mln_rec$145,                 /* Master Lane record         */~
            dln_key$22,                  /* Detail Lane key            */~
            dln_rec$100,                 /* Detail Lane record         */~
            lane$5,                      /* Screen Lane number         */~
            barcode_shp$20,              /* Scanned Shipping Barcode   */~
            prd_bc$18,                   /* Production Label Barcode   */~
            p_bc$20,                     /* Display undelivered barcode*/~
            plist$(900)20,               /* Display 10 undelivered     */~
            sav_bar$20,                  /* Save for cross dock        */~
            awd_app_key0$20,             /* Label Primary Key          */~
            awd_rec$(4%)256,             /* Appian Label Record        */~
            plan_lane$5,                 /* Appian Label Planned Lane  */~
            dt_rec$256,                  /* Production Detail          */~
            dt_key$23,                   /* (APCPLNDT)-Tracking File   */~
            dt_part$25,                  /* (APCPLNDT) Part Number     */~
            dt_bar$18,                   /* (APCPLNDT) Barcode         */~
            dt_dept$3,                   /* (APCPLNDT) Department      */~
            dt_load$5,                   /* (APCPLNDT) Load number     */~
            dt_warranty$8,               /* (APCPLNDT) Warranty Number */~
            dt_cust$9,                   /* (APCPLNDT) Customer        */~
            dt_brand$2,                  /* (APCPLNDT) Brand of window */~
            dt_sku$9,                    /* (APCPLNDT) SKU number      */~
            sub_part$20,                 /* (BCKSUBPT) Sub Part Number */~
            pick_scan$(10)20,            /* List of previous scans     */~
            delv_scan$(10)20             /* List of delivery prev scans*/
   
        dim apc$20, pname$07, scrn_title$21  
          
        dim blk_space$80, blk2_space$50

        dim                                                              ~
            filename$8,                  /* Use with EWDOPEN           */~
            pf$(3%)22,                   /* PF Screen Literals         */~
            pfkeys$20,                   /* PF KEYS                    */~
            rf_ee$(6)20,                 /* Error stop message         */~
            err$(100%)20,                /* Defined Error Messages     */~
            errormsg$20,                 /* Error message              */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            calc_time$8,                 /* Military time              */~
            prevcode1$20,                /* Shipping Barcode hold      */~
            prevcode2$20,                /* Shipping Barcode hold      */~
            prevlane$5                   /* Lane scan hold             */
   
        dim                                                              ~
            flag$1,                      /* Calling Program Flag       */~
            userid$3,                    /* User ID from login         */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            msg$(3)20,                   /* Message                    */~
            cnt$6,                       /* Picked count for user      */~
            pick_cnt$5,                  /* Scan pick counter          */~
            fill_space$1,                /* Screen field position      */~
            info_flds$(35%)4             /* Additional Info Fields     */
        
        dim f2%(45%),                    /* = 0 if the file is open    */~
            fs%(45%), axd$4,             /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(45%)20                 /* Text from file opening     */

            mat f2% = con
            mat fs% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! AWDMSTLN ! (NEW) Master Lane Scanning File          *~
            * #2  ! AWDDETLN ! (NEW) Detail Lane Scanning File          *~
            * #3  ! GENCODES ! Master Code Table File                   *~
            * #4  ! APCPLNDT ! Planning Tracking File                   *~
            * #5  ! AWDAPPLS ! New Appian Shipping Label File           *~
            * #7  ! BCKSUBPT ! New Sub Part Number File                 *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1, "AWDMSTLN",                                       ~
                        varc,     indexed,  recsize = 145,               ~
                        keypos =    1, keylen =   20,                    ~
                        alt key 1, keypos = 21, keylen = 5, dup                        
                        
            select #2, "AWDDETLN",                                       ~
                        varc,     indexed,  recsize = 100,               ~
                        keypos =    1, keylen =   25,                    ~
                        alt key  1, keypos = 26, keylen =  9, dup

            select #3,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 1,    keylen = 24
                        
            select #4,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 24,   keylen = 23,                      ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup

            select #5,  "AWDAPPLS",                                      ~
                        varc,     indexed,  recsize = 1024,              ~
                        keypos =    1, keylen =   20,                    ~
                        alt key 1,  keypos = 21,   keylen =  34, dup ,   ~
                            key 2,  keypos = 23,   keylen =  32, dup ,   ~
                            key 3,  keypos = 56,   keylen =  10, dup
                    
            select #7, "BCKSUBPT",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  11,                     ~
                        alt key  1, keypos =  12, keylen =  11, dup,     ~
                            key  2, keypos =  23, keylen =  45, dup

                                              
            call "SHOSTAT" ("Opening Files, One moment Please?")
                                               
            filename$ = "AWDMSTLN" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDDETLN" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error            
            filename$ = "GENCODES" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNDT" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDAPPLS" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            
        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
        apc$   = "** (AWD) Lane Master Scan Utility ***"
        pname$ = "AWDLANE "               

        init(" ") schema$    : schema% = 0%
        call "SCHEMA" (schema$,             /* What switch 1-NC 2-NE    */~
                       schema%,             /* Schema                   */~
                       #3,                  /* GENCODES                 */~
                       err% )               /* error                    */

REM        b_max% = 5%       /* SET NUMBER OF TIMES TO RING BELL ON SCREEN */
        call "EXTRACT" addr("ID", userid$)
        date$ = date
        call "DATEFMT" (date$)
        
        init("*") blk_space$ 
        init("*") blk2_space$    
        init(" ") pick_scan$(), delv_scan$()   
        s% = 1%        
        max% = 10%        

        rf_ee$(1%) = "  SSS TTT OOO PPP   "                
        rf_ee$(2%) = "  S    T  O O P P   "
        rf_ee$(3%) = "  SSS  T  O O PPP   "
        rf_ee$(4%) = "     S T  O O P     "
        rf_ee$(5%) = "     S T  O O P     "
        rf_ee$(6%) = "  SSSS T  OOO P     "
 
        err$(01%)="Brcd Not on File   "
        err$(02%)="App Lbl Not On File"
        err$(03%)="Error writing Mastr"
        err$(04%)="Error writing Detai"
        err$(05%)="Invalid Brcd Number"
        err$(06%)="Brcde Already Pickd"
        err$(07%)="No Mstr Status Updt"
        err$(08%)="Lane Can't Be Empty"
        err$(09%)="Brcd Not Picked Up "
        err$(10%)="Invalid Prd Barcode"
           
REM------------------------------------------------------------------------
REM     Program Start                                                     -
REM------------------------------------------------------------------------

        init(" ") errormsg$, prevcode1$, prevcode2$, prevlane$, scrn_title$, ~
                  pick_cnt$
        err% = 0%
        pick_cnt% = 0%
        fieldnr% = 1%
        
        call "STRING" addr("CT", "SCAN WINDOW TO LANE", 20%, scrn_title$)

REM------------------------------------------------------------------------
REM       B E G I N N I N G    O F    S C R E E N S                       -
REM------------------------------------------------------------------------
           
        mainmenu                                  /* Main Scanning Menu */
          deffn'050(fieldnr%)
    
          gosub set_init_screen
          accept                                                         ~
               at (01,02), fac(hex(8c)), pname$                 , ch(07),~
               at (01,14), fac(hex(8c)), date$                  , ch(08),~
               at (01,30), fac(hex(8c)), blk2_space$            , ch(50),~
                                                                         ~
               at (02,02), fac(hex(84)), scrn_title$            , ch(20),~
               at (02,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (03,05), "User ID: ",                                  ~
               at (03,15), fac(hex(8C)), userid$                , ch(03),~
               at (03,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (04,30), fac(hex(8c)), blk2_space$            , ch(50),~
                                                                         ~
               at (05,02), "F1 - Pickup",                                ~
               at (05,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (06,02), "F2 - Deliver",                               ~
               at (06,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (07,02), "F3 - No Shipping Label",                     ~
               at (07,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (08,02), "F4 - EXIT",                                  ~
               at (08,30), fac(hex(8c)), blk2_space$            , ch(50),~
                                                                         ~
               at (09,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (10,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (11,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (12,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (13,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (14,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (15,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (16,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (17,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (18,02), fac(hex(8c)), blk_space$             , ch(78),~               
               at (19,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (20,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (21,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (22,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (23,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (24,02), fac(hex(8c)), blk_space$             , ch(78),~  
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 1% then L00005
                  gosub find_pick_prev
                  gosub '100(fieldnr%)
                  goto mainmenu

L00005:        if keyhit% <> 2% then L00007
                  fieldnr% = 1%
                  gosub find_delv_prev
                  gosub '150(fieldnr%)
                  goto mainmenu

L00007:        if keyhit% <> 3% then L00010
                  fieldnr% = 1%
                  gosub '300(fieldnr%)
                  goto mainmenu  
                  
L00010:       if keyhit% = 4% then goto L00055
                  
              goto mainmenu

L00055:       close ws
              call "SCREEN" addr ("C", u3%, "I", i$(), cursor%()) 
              call "ALLFREE"
              goto exit_program
        return        
        
        
        set_init_screen
                                                         
          init(" ") calc_time$, pf$(1%), errormsg$, barcode_shp$, lane$, ~
                   pick_cnt$
          err% = 0%
          pick_cnt% = 0%
          scrn_title$ = "Scan Window to Lane"

          calc_time$ = time                      /* Military - HHMMSSXX */

          pfkeys$ = hex(01020304ffffffffffffffffffffffffff00)
 
        return       
      
REM------------------------------------------------------------------------
REM      Pickup screen                                                    -
REM------------------------------------------------------------------------        
        pick_screen
          deffn'100(fieldnr%)
   
          gosub set_screen
          accept                                                         ~
               at (01,02), fac(hex(8c)), pname$                 , ch(07),~
               at (01,14), fac(hex(8c)), date$                  , ch(08),~
               at (01,30), fac(hex(8c)), blk2_space$            , ch(50),~
                                                                         ~
               at (02,02), fac(hex(84)), scrn_title$            , ch(20),~
               at (02,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (03,02), fac(hex(94)), errormsg$              , ch(20),~
               at (03,30), fac(hex(8c)), blk2_space$            , ch(50),~
                                                                         ~
               at (04,02), "Scan Shipping Barcode:",                     ~
               at (04,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (05,02), fac(lfac$(1%)), barcode_shp$         , ch(20),~
               at (05,30), fac(hex(8c)), blk2_space$            , ch(50),~
                                                                         ~
               at (06,02), "Scan Counter: ",                             ~
               at (06,16), fac(hex(84)), pick_cnt$              , ch(05),~
               at (06,30), fac(hex(8c)), blk2_space$            , ch(50),~
                                                                         ~
               at (07,02), fac(hex(8c)),   pf$(1%)              , ch(22),~
               at (07,30), fac(hex(8c)), blk2_space$            , ch(50),~
                                                                         ~
               at (08,01), "> ",                                         ~
               at (08,03), fac(hex(8c)), pick_scan$(s%)         , ch(20),~ 
               at (08,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (09,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (10,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (11,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (12,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (13,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (14,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (15,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (16,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (17,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (18,02), fac(hex(8c)), blk_space$             , ch(78),~               
               at (19,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (20,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (21,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (22,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (23,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (24,02), fac(hex(8c)), blk_space$             , ch(78),~  
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 1% then L01003
                  init(" ") errormsg$, barcode_shp$
                  gosub find_pick_prev            /* find previous scan */
                  goto pick_screen
                  
L01003:        if keyhit% <> 3% then L01005
                 s% = s% - 1%
                 if s% > 0% then goto pick_screen  /* valid index so display */
                 
                 gosub find_pick_prev            /* find previous scan */

                 goto pick_screen

L01005:        if keyhit% <> 4% then L01010
                  goto L01055
                  
L01010:       gosub valid_bar         /* validate barcode length */
              if err% <> 0% then goto pick_screen
              
              init(" ") errormsg$
              prevcode1$ = barcode_shp$
              gosub pickup_scan
              if msg$(1%) <> " " then errormsg$ = msg$(1%)
              init(" ") barcode_shp$

              goto pick_screen

L01055:       close ws
              call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
              call "ALLFREE"
        return
        
        set_screen
          lfac$(1%) = hex(81)
                                                           
          init(" ") calc_time$
          err% = 0%
          convert pick_cnt% to pick_cnt$, pic(00000)
          calc_time$ = time                      /* Military - HHMMSSXX */

          pf$(1%) = "F1StOvr F3List F4Ext" 
          pfkeys$ = hex(01ff0304ffffffffffffffffffffffffff00)
          
          init(" ") barcode_shp$ 
          scrn_title$ = "Scan for PICKUP " & userid$
          
        return

REM------------------------------------------------------------------------
REM      Lane delivery scan screen                
REM------------------------------------------------------------------------
       lane_screen
        deffn'150(fieldnr%)
            
          gosub set_screen_d
          accept                                                         ~
               at (01,02), fac(hex(84)), scrn_title$            , ch(20),~
               at (01,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (02,02), fac(hex(94)), errormsg$              , ch(20),~
               at (02,30), fac(hex(8c)), blk2_space$            , ch(50),~
                                                                         ~
               at (03,02), "In Pickup: ",                                ~
               at (03,15), fac(hex(84)), cnt$                   , ch(06),~
               at (03,30), fac(hex(8c)), blk2_space$            , ch(50),~
                                                                         ~
               at (04,02), "Scan Lane: ",                                ~
               at (04,13), fac(lfac$(1%)),lane$                 , ch(05),~
               at (04,30), fac(hex(8c)), blk2_space$            , ch(50),~
                                                                         ~
               at (05,02), "Scan Shipping Barcode:",                     ~
               at (05,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (06,02), fac(lfac$(3%)), barcode_shp$         , ch(20),~
               at (06,30), fac(hex(8c)), blk2_space$            , ch(50),~
                                                                         ~
               at (07,02), fac(hex(8c)), pf$(1%)                , ch(22),~
               at (07,30), fac(hex(8c)), blk2_space$            , ch(50),~
                                                                         ~
               at (08,01), "> ",                                         ~
               at (08,03), fac(hex(8c)), delv_scan$(s%)         , ch(20),~                
               at (08,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (09,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (10,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (11,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (12,02), fac(hex(8c)), blk_space$             , ch(78),~             
               at (13,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (14,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (15,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (16,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (17,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (18,02), fac(hex(8c)), blk_space$             , ch(78),~               
               at (19,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (20,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (21,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (22,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (23,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (24,02), fac(hex(8c)), blk_space$             , ch(78),~  
                                                                         ~
               keys(pfkeys$), key(keyhit%)
               
               if keyhit% <> 1% then L02003
                  fieldnr% = 1%
                  init(" ") errormsg$, barcode_shp$, lane$ 
                  
                  gosub find_delv_prev           /* find previous scan */
                  goto lane_screen  

L02003:        if keyhit% <> 2% then L02004
                  p% = 1% 
                  gosub undelivered_screen
                  goto lane_screen

L02004:        if keyhit% <> 3% then L02005
                 s% = s% - 1%
                 if s% > 0% then goto lane_screen  /* valid index so display */
                 
                 gosub find_delv_prev            /* find previous scan */

                 goto lane_screen
      
L02005:        if keyhit% <> 4% then L02010
                  goto L02015  
                  
L02010:        on fieldnr% gosub valid_lane, valid_bar

               if err% <> 0% then goto lane_screen
      
               if fieldnr% = 1% then goto L02013           

               gosub valid_pick  /* validate window picked before lane assign */
               if err% <> 0% then goto lane_screen
               
               gosub deliver_scan
               if msg$(1%) <> " " then errormsg$ = msg$(1%)
               prevcode2$ = barcode_shp$
               init(" ") barcode_shp$
               fieldnr% = 3%
               
               goto lane_screen
L02013:
               if fieldnr% = 1% then fieldnr% = 3% else fieldnr% = 1%
               goto lane_screen
               
L02015:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               call "ALLFREE"
        return
        
        set_screen_d
          init(" ") calc_time$, cnt$, barcode_shp$
          scrn_title$ = "Delivery to Lane " & userid$
          
          calc_time$ = time                      /* Military - HHMMSSXX */
          if fieldnr% <> 1% then goto L02020
           
          lfac$(1%) = hex(81)
          lfac$(3%) = hex(84)
          goto L02030
                             
L02020:   lfac$(1%) = hex(84) 
          lfac$(3%) = hex(81)
          
L02030:   gosub count_pickups    
          convert cnt% to cnt$, pic(000000)          
          
          pf$(1%) = "F1StO F2Pck F3Lt F4Ext" 
          pfkeys$ = hex(01020304ffffffffffffffffffffffffff00)
          err% = 0%

        return
   
REM------------------------------------------------------------------------
REM      Display Picked Windows not Delivered screen                
REM------------------------------------------------------------------------
       undelivered_screen
        deffn'200(fieldnr%)
            
          gosub set_screen_u
          accept                                                         ~
               at (01,02), fac(hex(84)), scrn_title$            , ch(20),~
                                                                         ~            
               at (03,04), fac(hex(8c)), plist$(p%)             , ch(20),~
               at (04,04), fac(hex(8c)), plist$(p%+1%)          , ch(20),~
               at (05,04), fac(hex(8c)), plist$(p%+2%)          , ch(20),~
               at (06,04), fac(hex(8c)), plist$(p%+3%)          , ch(20),~
               at (07,04), fac(hex(8c)), plist$(p%+4%)          , ch(20),~
               at (08,04), fac(hex(8c)), plist$(p%+5%)          , ch(20),~
               at (09,04), fac(hex(8c)), plist$(p%+6%)          , ch(20),~
               at (10,04), fac(hex(8c)), plist$(p%+7%)          , ch(20),~
                                                                         ~
               at (12,02), fac(hex(8c)), pf$(1%)                , ch(22),~
                                                                         ~
               at (14,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (15,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (16,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (17,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (18,02), fac(hex(8c)), blk_space$             , ch(78),~               
               at (19,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (20,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (21,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (22,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (23,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (24,02), fac(hex(8c)), blk_space$             , ch(78),~  
                                                                         ~
               keys(pfkeys$), key(keyhit%)
               
               if keyhit% = 4% then goto L02075
               
               if p% >= 100% or plist$(p%+9%) = " " then p% = 1%      ~
               else  p% = p% + 8%                           /* scrolling */
               goto undelivered_screen
              
L02075:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               call "ALLFREE"
        return
        
        set_screen_u
          init(" ") plist$()
          scrn_title$ = "Remaining Picked Up for " & userid$
          
          gosub count_pickups       
          convert cnt% to cnt$, pic(000000)  
                                                  
          pf$(1%) = "ENTER(scroll)   F4Ext" 
          pfkeys$ = hex(ffffff04ffffffffffffffffffffffffff00)
          err% = 0%

        return
        
REM------------------------------------------------------------------------
REM      Allow production barcode scanning if not shipping label on window           
REM------------------------------------------------------------------------
       prod_lbl_scan
        deffn'300(fieldnr%)
            
          gosub set_screen_p
          accept                                                         ~
               at (01,02), fac(hex(8c)), pname$                 , ch(07),~
               at (01,14), fac(hex(8c)), date$                  , ch(08),~
               at (01,30), fac(hex(8c)), blk2_space$            , ch(50),~
                                                                         ~
               at (02,01), fac(hex(84)), scrn_title$            , ch(21),~
               at (02,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (03,02), fac(hex(94)), errormsg$              , ch(20),~
               at (03,30), fac(hex(8c)), blk2_space$            , ch(50),~
                                                                         ~
               at (04,02), "Scan Product Barcode:",                      ~
               at (04,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (05,04), fac(lfac$(1%)), prd_bc$              , ch(18),~
               at (05,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (06,30), fac(hex(8c)), blk2_space$            , ch(50),~
                                                                         ~
               at (07,02), fac(hex(8c)),   pf$(1%)              , ch(22),~
               at (07,30), fac(hex(8c)), blk2_space$            , ch(50),~
                                                                         ~
               at (08,01), "> ",                                         ~
               at (08,03), fac(hex(8c)), prevcode$              , ch(20),~ 
               at (08,30), fac(hex(8c)), blk2_space$            , ch(50),~
                                                                         ~               
               at (09,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (10,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (11,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (12,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (13,30), fac(hex(8c)), blk2_space$            , ch(50),~
               at (14,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (15,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (16,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (17,02), fac(hex(8c)), blk_space$             , ch(78),~
               at (18,02), fac(hex(8c)), blk_space$             , ch(78),~               
               at (19,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (20,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (21,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (22,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (23,02), fac(hex(8c)), blk_space$             , ch(78),~  
               at (24,02), fac(hex(8c)), blk_space$             , ch(78),~  
                                                                         ~
               keys(pfkeys$), key(keyhit%)
               
               if keyhit% <> 1% then L03005
                  init(" ") errormsg$, prd_bc$
                  goto prod_lbl_scan
                  
L03005:        if keyhit% <> 4% then L03010
                  goto L03050
                  
L03010:       gosub valid_prdbar         /* validate production barcode */
              if err% <> 0% then goto prod_lbl_scan
              
              init(" ") errormsg$
              prevcode$ = prd_bc$
              gosub noshiplbl_scan
              if msg$(1%) <> " " then errormsg$ = msg$(1%)
              init(" ") prd_bc$
              goto prod_lbl_scan
                  
L03050:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               call "ALLFREE"
        return
        
        set_screen_p
          init(" ") prd_bc$
          scrn_title$ = "No Shipping Label " & userid$
          fieldnr% = 1%
          lfac$(1%) = hex(81)
                             
          pf$(1%) = "01StOvr        F4Ext" 
          pfkeys$ = hex(01ffff04ffffffffffffffffffffffffff00)
          err% = 0%

        return
        
REM------------------------------------------------------------------------
REM       E N D    O F    S C R E E N S                                   -
REM------------------------------------------------------------------------
REM------------------------------------------------------------------------
REM      Validate screen barcode length                                   -
REM------------------------------------------------------------------------
         valid_bar
           pos% = 0%
           pos% = pos(barcode_shp$ = " ")
           if pos% = 0% then return
           err% = 5%
           gosub err_scrn
         return
 
REM------------------------------------------------------------------------
REM      Validate screen lane                                             -
REM------------------------------------------------------------------------
           valid_lane
              if lane$ = "     " then goto L04000
           return
           
L04000:    err% = 8%
           gosub err_scrn
         return  

REM------------------------------------------------------------------------
REM     Valid picked up before assigning to lane
REM------------------------------------------------------------------------   
        valid_pick
           init(" ") mln_key$, mln_rec$, msg$()
                     
           mln_key$ = barcode_shp$
           read #1, key= mln_key$, using L04010, mln_rec$, eod goto L04020
L04010:  FMT CH(145)         

           return
L04020:
         err% = 9%
         gosub err_scrn
       return

REM------------------------------------------------------------------------
REM     Valid the production barcode 
REM------------------------------------------------------------------------   
        valid_prdbar                          
            err%   = 0%

            init(" ") dt_key$, errormsg$, dt_bar$,    ~
                      dt_load$, dt_drop$, dt_st$

            str(dt_key$,1%,18%) = str(prd_bc$, 1%,18%)   /* Set Barcode */
            
      /*  check_prd_nxt */
            read #4,key > dt_key$, using L04200, dt_load$, dt_drop$, dt_key$, ~
                      dt_date$, dt_st$,          eod goto L04290                             

L04200:        FMT CH(05), POS(11), CH(02), POS(24), CH(23),     ~
                   CH(06), POS(64), CH(02) 

            if str(prd_bc$,1%,18%) <> str(dt_key$,1%,18%) then goto L04290
    
        return
    
L04290:    err% = 10%
           gosub err_scrn
        return
                               
REM------------------------------------------------------------------------
REM       B E G I N N I N G    O F    R O U T I N E S                     -
REM       B E G I N N I N G    O F    S C A N N I N G                     -
REM------------------------------------------------------------------------

        pickup_scan                                    /* Scan Pickup   */
          init(" ") sav_bar$
          
          gosub check_crossdock
                                                          
          gosub check_shipping  
          if err% <> 0% then return          
          gosub lookup_sub_part          
          gosub check_shipping_appian
          if err% <> 0% then return 
          gosub write_pickup
          if err% <> 0% then return
          gosub load_pick
          pick_cnt% = pick_cnt% + 1%

        return

REM ----------------------------------------------------------------
REM -  No shipping label, scan production label                   
REM ---------------------------------------------------------------        
        noshiplbl_scan                            
          init(" ") sav_bar$
          
          barcode_shp$ = prd_bc$ & dt_drop$
          gosub check_scanned
          if err% <> 0% then return
          
          gosub check_crossdock
                                                          
          gosub check_shipping  
          if err% <> 0% then return          
          gosub lookup_sub_part          
            
          prdlbl% = 1%
          mst_st$  = "00"
          gosub write_pickup
          prdlbl% = 0%

        return
        
REM ----------------------------------------------------------------
REM -  Pull APCPLNDT information                      
REM ----------------------------------------------------------------
        check_shipping                           
            err%   = 0%
            dt_st% = 0%         
            
            init(" ") dt_key$, errormsg$, dt_bar$, dt_rec$, dt_dept$,    ~
                      dt_load$, dt_warranty$, dt_cust$, dt_part$,        ~
                      dt_brand$, dt_sku$

            str(dt_key$,1%,18%) = str(barcode_shp$, 1%,18%) /* Set Barcode */
            
        check_shipping_nxt
            read #4,key > dt_key$, using L04300 ,dt_load$,dt_key$, dt_date$, ~
                      dt_st$, dt_warranty$, dt_cust$, dt_part$, dt_brand$,   ~
                      dt_sku$,  ~
                    eod goto L04390                            

L04300:        FMT CH(5), POS(24), CH(23), CH(06), POS(64), CH(2), POS(96), ~
                   CH(8), POS(124), CH(09), POS(189), CH(25), POS(243),     ~
                   CH(02), CH(09)

            if str(barcode_shp$,1%,18%) <> str(dt_key$,1%,18%) then goto L04390
               dt_bar$  = str(dt_key$,1%,18%)             
               dt_dept$ = str(dt_key$,19%,3%)
     
               convert dt_st$ to dt_st%, data goto L04390

               if dt_dept$ = "001" then goto check_shipping_nxt

        return

L04390:    err% = 1%  
           gosub err_scrn
        return
        
REM ----------------------------------------------------------------
REM -      Lookup the subpart number for the window                   
REM ----------------------------------------------------------------        
        lookup_sub_part
           init(" ") bcksubpt_rec$, flds$(), info_flds$(), sub_part$
           
           flag$ = "0"                       /* Sales Order Info         */
           pgm$  = "1"
           err1% = 0%
           so_inv$ = str(barcode_shp$,1%,8%)
           item_no$ = str(barcode_shp$,9%,2%)

           convert so_inv$ to so_inv%, data goto convert_alpha

           convert so_inv% to so_inv$, pic(00000000)

           goto order_converted

convert_alpha:
           convert str(so_inv$,2%,7%) to so_inv%, data goto sub_part1
sub_part1:
           convert so_inv% to str(so_inv$,2%,7%), pic(0000000)

order_converted:
           convert item_no$ to item_no%, data goto sub_part2
sub_part2:
           convert item_no% to item_no$, pic(###)

           call "AWDBKSUB" (flag$,        /* Flag 0=SalesOrder 1=Invoice*/~
                            pgm$,         /* Calling Program 0=BCKUPDTE */~
                                          /* 1=Any Other 2=Delete       */~
                                          /* 3=Invoice                  */~
                            so_inv$,      /* SO or Invoice Num to lookup*/~
                            item_no$,     /* Item Number                */~
                            bcksubpt_rec$, /* Record If BCKUPDTE then   */~
                                          /* pass in else pass out      */~
                            flds$(),      /* Part Number Fields         */~
                            info_flds$(), /* Information Fields         */~
                            #7,           /* BCKSUBPT File              */~
                            err1%)        /* Error Code                 */

            if err1% <> 0% then                             ~
                    str(bcksubpt_rec$,48%,20%) = "00000               "

            sub_part$ = str(bcksubpt_rec$,48%,20%)
 
       return
 
REM ----------------------------------------------------------------
REM -  Pull the APPIAN label information if needed in future                 
REM ----------------------------------------------------------------
        check_shipping_appian                            
           err%   = 0%

           init(" ") awd_app_key0$
           awd_app_key0$ = barcode_shp$
           
           read #5, key 0% = awd_app_key0$, using L04510, awd_rec$(),   ~
                 eod goto L04590
L04510:       FMT 4*CH(256)  
           
             plan_lane$ = str(awd_rec$(3%),65%,2%)   /* old lane was 2 chars */
             if plan_lane$ <> str(lane$,1%,2%) then msg$(1%) = "WARNING"

        return
        
L04590:     err% = 2%
            gosub err_scrn
        return

REM------------------------------------------------------------------------
REM- Check production label already scanned once 
REM------------------------------------------------------------------------    
        check_scanned   
           init(" ") mln_key$, mln_rec$, msg$()
                     
           mln_key$ = barcode_shp$
           read #1, key= mln_key$, using L04600, mln_rec$, eod goto L04690
L04600:  FMT CH(145)         
               err% = 6%
               gosub err_scrn
L04690:    return
        
REM------------------------------------------------------------------------
REM- Layouts for the 2 new files for this process
REM------------------------------------------------------------------------
REM     
REM                   FILE AWDMSTLN                    
REM         CH(18)     barcode                          
REM         CH(02)     drop number
REM         CH(03)     user ID
REM         CH(02)     status of barcode                      
REM         CH(25)     part                             
REM         CH(20)     subpart                          
REM         CH(09)     customer                         
REM         CH(06)     production date                  
REM         CH(06)     Initial Scan date                
REM         CH(08)     Initial Scan time  
REM         CH(20)     Comments              
REM         CH(26)     Filler                           
REM
REM                   FILE AWDDETLN                    
REM         CH(18)     barcode                          
REM         CH(02)     drop number                      
REM         CH(05)     lane number  (awdappls dock)     
REM         CH(03)     user ID                          
REM         CH(06)     Scan date                        
REM         CH(08)     Scan time       
REM         CH(20)     Comments                 
REM         CH(38)     Filler                           

REM ----------------------------------------------------------------
REM -  Write the Lane Master as pick up status                   
REM ----------------------------------------------------------------
        write_pickup
           init(" " ) mln_key$, mln_rec$, msg$()

           calc_time$ = time                      /* Military - HHMMSSXX */
                     
           mln_key$ = barcode_shp$
           read #1, hold, key = mln_key$, using L05000, mln_rec$,   ~
                     eod goto L05010
L05000:  FMT CH(145)
          
           if str(mln_rec$,24%,02%) = "00" then goto L05005   
                            /* was no shp label scan */
           
           err% = 6%
           gosub err_scrn
           return
L05005:
           delete #1
           str(mln_rec$,24%,02%) = "01"               /* set to picked up */
           str(mln_rec$,21%,03%) = userid$
           put #1, using L07000, mln_rec$
           write #1, eod goto L05055
           return
           
L05010:            /* add master lane record */
           str(mln_rec$,01%,18%) = str(barcode_shp$,01%,18%)
           str(mln_rec$,19%,02%) = str(barcode_shp$,19%,02%)
           str(mln_rec$,21%,03%) = userid$
           str(mln_rec$,24%,02%) = "01"                  /* set to picked up */
          if prdlbl% = 1% then  str(mln_rec$,24%,02%) = "00"  /* no shp label*/   
           str(mln_rec$,26%,25%) = dt_part$
           str(mln_rec$,51%,20%) = sub_part$
           str(mln_rec$,71%,09%) = dt_cust$
           str(mln_rec$,80%,06%) = dt_date$
           str(mln_rec$,86%,06%) = date
           str(mln_rec$,92%,08%) = str(calc_time$,1%,6%)
           str(mln_rec$,100%,46%) = " "
          if prdlbl% = 1% then str(mln_rec$,100%,20%) = "No Shipping Label Scan"
           
           put #1, using L05000, mln_rec$
           write #1, eod goto L05055
 
           msg$(1%) = "Successful Scan     "
        return

L05055:      err% = 3%
            gosub err_scrn
        return

REM ----------------------------------------------------------------
REM -  Write the Lane Master as pick up status                   
REM ----------------------------------------------------------------
        deliver_scan                               /* Scan deliver to lane */
          init(" ") sav_bar$
          
          gosub check_crossdock
              
          gosub check_shipping  
          if err% <> 0% then return          
          gosub check_shipping_appian
          if err% <> 0% then return               
          gosub write_lane_delivery
          if err% <> 0% then return
          gosub load_delivery

        return

REM ----------------------------------------------------------------
REM -  Writ the Lane Detail data                     
REM ----------------------------------------------------------------
        write_lane_delivery
           init(" ") dln_key$, dln_rec$
           
           str(dln_key$,1%,20%)  = barcode_shp$ 
           str(dln_key$,21%,05%) = lane$
          read #2, hold, key = dln_key$, using L06000, dln_rec$, eod goto L06010
L06000:   FMT CH(100)
          
           if str(dln_rec$,26%,03%) <> userid$ then ~
               str(dln_rec$,43%,20%) = "User ID was " & str(dln_rec$,26%,03%)
               
           delete #2     

           str(dln_rec$,01%,18%) = str(barcode_shp$,01%,18%)
           str(dln_rec$,19%,02%) = str(barcode_shp$,19%,02%)           
           str(dln_rec$,21%,05%) = lane$
           str(dln_rec$,26%,03%) = userid$ 
           str(dln_rec$,29%,06%) = date
           str(dln_rec$,35%,08%) = str(calc_time$,1%,6%)
           put #2, using L06000, dln_rec$
           
           write #2
           mst_st$ = "08"
           gosub update_mst_status 
           msg$(1%) = "Successful Override Scan" 
           return
           
L06010:        /* add detail lane record */
           dln_key$ = barcode_shp$ 
           str(dln_rec$,01%,18%) = str(barcode_shp$,01%,18%)
           str(dln_rec$,19%,02%) = str(barcode_shp$,19%,02%)
           str(dln_rec$,21%,05%) = lane$
           str(dln_rec$,26%,03%) = userid$ 
           str(dln_rec$,29%,06%) = date
           str(dln_rec$,35%,08%) = str(calc_time$,1%,6%)
           str(dln_rec$,43%,61%) = " "
           
           put #2, using L06000, dln_rec$

           write #2, eod goto L06055
           
           mst_st$ = "05"
           gosub update_mst_status 
           msg$(1%) = "Successful Scan     "
           
        return

L06055:     err% = 4%
            gosub err_scrn
        return
REM------------------------------------------------------------------------
REM     Update the master status to delivered to lane
REM------------------------------------------------------------------------   
        update_mst_status
           init(" ") mln_key$, mln_rec$, msg$()
                     
           mln_key$ = barcode_shp$
           read #1, hold, key= mln_key$, using L07000, mln_rec$, eod goto L07055
L07000:  FMT CH(145)

           delete #1
           
           if str(mln_rec$,24%,02%) = mst_st$ then mst_st$ = "06"
           str(mln_rec$,24%,02%) = mst_st$               /* set to picked up */
           if mst_st$ = "06" then str(mln_rec$,100%,20%) = "Moved lanes"
           put #1, using L07000, mln_rec$
           write #1, eod goto L07065
           
        return

L07055:     err% = 3%
            gosub err_scrn
        return    

L07065:     err% = 7%
            gosub err_scrn
        return   
        
REM------------------------------------------------------------------------
REM     Check for crossdock shipping label
REM------------------------------------------------------------------------                   
        check_crossdock
          cd% = 0%
          
          if schema% = 1% and str(barcode_shp$,1%,1%) = "A" then          ~
                                                   goto notUpdateCrossShip
          if schema% = 2% and str(barcode_shp$,1%,1%) = "B" then          ~
                                                   goto notUpdateCrossShip
                                                         
          if str(barcode_shp$,1%,1%) <> "A" and str(barcode_shp$,1%,1%) <> "B" ~
                                              then goto notUpdateCrossShip
             sav_bar$ = barcode_shp$
             str(barcode_shp$,1%,1%) = "0"
             cd% = 1%
             
notUpdateCrossShip:
          return

REM------------------------------------------------------------------------
REM     Count windows remaining in picked up status for this user
REM------------------------------------------------------------------------  
         count_pickups
           init(" " ) mln_key1$, mln_rec$, msg$()
           cnt% = 0%
         
           str(mln_key1$,1%,3%) = userid$
           str(mln_key1$,4%,2%) = "00"
           
           read #1, key 1% > mln_key1$, using L08005, p_bc$, mln_key1$, ~
                     eod goto L08075
L08005:  FMT CH(20), POS(21), CH(5)
              goto L08020
L08010:
           read #1, using L08005, p_bc$, mln_key1$, eod goto L08075
           
L08020:       if str(mln_key1$,1%,3%) <> userid$ then goto L08075
              if str(mln_key1$,4%,2%) >= "05" then goto L08075
              if str(mln_key1$,4%,2%) = "01" then cnt% = cnt% + 1%
/* CR1952 */  if str(mln_key1$,4%,2%) = "01" and cnt% <= 900%  ~
                  then plist$(cnt%) = p_bc$
             goto L08010
L08075:         
         return

REM------------------------------------------------------------------------
REM     Load the previous scanned pick array
REM------------------------------------------------------------------------  
         load_pick

           x% = 0%
               /* see if exists in array, only list once */
           for x% = 1% to max% 
              if pick_scan$(x%) = barcode_shp$ then goto L09090
           next x%
           
           if pick_scan$(max%) > " " then goto L09010  /* if array full */
           
           for x% = 1% to max% 
             if pick_scan$(x%) = " " then pick_scan$(x%)= barcode_shp$  ~
                                     else next x%
             goto L09090
           next x%
             
L09010:      /* reposition array then add new scan */
           for x% = 1% to max% - 1%
             pick_scan$(x%) = pick_scan$(x% + 1%)
           next x%
           
           x% = max%
           pick_scan$(x%) = barcode_shp$ 
           
L09090:    s% = x%
           return
           
REM------------------------------------------------------------------------
REM     Find the previous scan in the pick array
REM------------------------------------------------------------------------   
          find_pick_prev
             s% = 1%
             for x% = 1% to max% 
                if pick_scan$(x%) = prevcode1$ then s% = x%
             next x%
         return

         
REM------------------------------------------------------------------------
REM     Load the previous scanned delivery array
REM------------------------------------------------------------------------  
         load_delivery

           x% = 0%
               /* see if exists in array, only list once */
           for x% = 1% to max% 
              if delv_scan$(x%) = barcode_shp$ then goto L19090
           next x%
           
           if delv_scan$(max%) > " " then goto L19010  /* if array full */
           
           for x% = 1% to max% 
             if delv_scan$(x%) = " " then delv_scan$(x%)= barcode_shp$  ~
                                     else next x%
             goto L19090
           next x%
             
L19010:      /* reposition array then add new scan */
           for x% = 1% to max% - 1%
             delv_scan$(x%) = delv_scan$(x% + 1%)
           next x%
           
           x% = max%
           delv_scan$(x%) = barcode_shp$ 
           
L19090:    s% = x%
           return
           
REM------------------------------------------------------------------------
REM     Find the previous scan in the delivery  array
REM------------------------------------------------------------------------   
          find_delv_prev
             s% = 1%
             for x% = 1% to max% 
                if delv_scan$(x%) = prevcode2$ then s% = x%
             next x%
         return

REM ----------------------------------------------------------------
REM - Display This Screen If Barcode is Scanned And No Error Occur -
REM ----------------------------------------------------------------
        err_scrn                      /* Display this Message for Errors */
            errormsg$ = err$(err%)

REM            for i% = 1% to b_max%
                 print at(13,75);bell;
REM            next i%
REM            CALL "PAUSE" ADDR(30%)
            
            gosub err_display  
        return

REM------------------------------------------------------------------------
REM       E R R O R    R O U T I N E S                                    -
REM------------------------------------------------------------------------

        open_error
            errormsg$ = "Open Error " & filename$
            gosub err_display
            goto exit_program
        return
 
        err_display
           
          gosub set_error
          accept                                                         ~
               at (01,02), fac(hex(94)), errormsg$              , ch(20),~
                                                                         ~
               at (02,02), fac(hex(84)), rf_ee$(1%)             , ch(20),~
               at (03,02), fac(hex(84)), rf_ee$(2%)             , ch(20),~
               at (04,02), fac(hex(84)), rf_ee$(3%)             , ch(20),~
               at (05,02), fac(hex(84)), rf_ee$(4%)             , ch(20),~
               at (06,02), fac(hex(84)), rf_ee$(5%)             , ch(20),~
               at (07,02), fac(hex(84)), rf_ee$(6%)             , ch(20),~
                                                                         ~
               at (08,02), fac(hex(8c)),   pf$(1%)              , ch(22),~
               at (08,22), fac(lfac$(1%)), fill_space$          , ch(01),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_error
          fill_space$ = " "
          pf$(1%)        = "F4 Continue       "
          pfkeys$        = hex(ffffff04ffffffffffffffffffffffffff)
        return

REM------------------------------------------------------------------------
REM       E N D    O F    E R R O R    R O U T I N E S                    -
REM------------------------------------------------------------------------

REM------------------------------------------------------------------------
REM       E X I T    P R O G R A M                                        -
REM------------------------------------------------------------------------

        exit_program
            call "SHOSTAT" ("One Moment Please")
            close #1: close #2: close #3: close #4: close #5: close #7
        end
