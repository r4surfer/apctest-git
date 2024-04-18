        REM *************************************************************~
            *                                                           *~
            *  Program Name      - SCRSCANN                             *~
            *  Creation Date     - 11/20/2009                           *~
            *                                                           *~
            *  Description       - Scanning Utility for scanning to     *~
            *                      complete or remakes.                 *~
            *                                                           *~
            *  Code Tables Used  - (PLAN SHFT) - Shift Codes            *~
            *                      (PLAN PROC) - Planning Process Codes *~
            *                      (PLAN REMK) - Glass Remake Reason    *~
            *                                    Codes                  *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *11/20/2009!          New program                     ! DES *~
            *05/30/2014! (AWD001) Fix for sub part becomming      ! PWW *~
            *          ! corrupt & set to batch name.             !     *~
            *07/19/2017! (CR1036) Remove GSO and WEL option       ! RDB *~
            *************************************************************


        dim scr$(15%)40,                 /* ASKUSER Header             */~
            filename$8,                  /* Use with EWDOPEN - EWD016  */~
            trauma_center$1,             /* (EWD014) Sitch on/off      */~
            her$(20%)50, apc$70,         /* Error Text Display         */~
            rf_her$(40%)20,              /* RF Err Text Dis (AWD045)   */~
            scr_sel$1, scr_sel_d$30,     /* Screen Scanning Option     */~
            scr_id$3, msg$(3%)79, hh$40, /* Scanning User Id           */~
            scr_id_d$30,                 /* User Id Name               */~
            scr_dept$3, scr_dept_d$30,   /* Product Line / Dept Code   */~
            sav_scr_dept$3,              /* Save Dept          (EWD025)*/~
            scr_shft$2, scr_shft_d$30,   /* Screen Shift Entry         */~
            scr_proc$2, scr_proc_d$30,   /* Product / Dept Process Code*/~
            scr_load$5, scr_load_d$30,   /* Production Load and Descrip*/~
            readkey$24, desc$30,         /* GENERIC KEY                */~
            sc_reason_desc$30,                                           ~
            descr$30,                                                    ~
            table$9, code$3,             /* TABLE LOOKUP NAME AND CODE */~
            pfkeys$40,                   /* PF KEYS                    */~
            ld$(7%)50,                   /* 'Load' Message             */~
            ps$(7%)50,                   /* Scan 'COMP'lete Text Screen*/~
            ee$(7%)50,                   /* 'STOP' Message Error Text  */~
            dt_key2$12,                  /* Scanning Load Number - Only*/~
            err$(40%)50,                 /* Defined Error Messages     */~
            rf_err$(40%)20,              /* RF Err Msg         (AWD045)*/~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8, date2$6,             /* Date for screen display    */~
            dateout$8,                   /* Time Extract For Screen    */~
            inp_text$(5%)79,             /* Input Prompt Text          */~
            fld$(3%)30,                  /* Field Text                 */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(4%)1,                  /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            tt_unit$24,                  /* Scanned Units each Prod    */~
            scrn_title$40, title$40,     /* Screen Description         */~
            userid$3                     /* Current User Id            */

        dim rlib$8,                      /* Run library for link call  */~
            rvol$6,                      /* Run volume for link call   */~
            run$8                        /* Program to Run (EWD007)    */


        dim                              /* (AWD029)                   */~
            supp_dept$(20%)3             /* Support Departments        */
            

         dim rm_dept_n$3,                /* New Department No  (AWD043)*/~
             dept_wand$1,                /* Screen Character   (AWD043)*/~
             dept_d$30,                  /* Department Desc    (AWD043)*/~
             rm_status$1,                /* Remake Status      (AWD046)*/~
             rm_status_d$(11%)30,        /* Reamke Status Desc (AWD046)*/~
             gls_status$20               /* Glass Status Desc  (AWD046)*/
 
        dim                              /* Subroutine - Variables     */~
            sc_barcode$9,                /* barcode                    */~
            rec$(2)256,                  /* barcode                    */~
            hold_rec$(2)256,             /* barcode                    */~
            prevcode$9,                  /* barcode                    */~
            pname$21,                    /* barcode                    */~
            sc_wand$1,                   /* barcode                    */~
            sc_desc$40,                  /* barcode                    */~
            sc_desc2$40,                 /* barcode                    */~
            scan_type$8                  /* Remake/Complete            */ 
            
        dim f2%(50%),                    /* = 0 if the file is open    */~
            fs%(50%), axd$4,             /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(50%)20                 /* Text from file opening     */

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
            * #1  ! AWDPLNSR ! Screen Remake File                       *~
            * #2  ! GENCODES ! Master Code Table File                   *~
            * #6  ! USERLCMS ! Caelus Master User Def. (USERLCMS)       *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            
            select #1,  "AWDPLNSR",                                      ~
                        varc,     indexed,  recsize = 512,               ~
                        keypos =   42, keylen =   12,                    ~
                        alt key  1, keypos =    7, keylen =  47,         ~
                            key  2, keypos  = 163, keylen =  13,         ~
                            key  3, keypos =   1, keylen =  53,          ~
                            key  4, keypos = 205, keylen =  12, dup 

            select #2,  "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos = 1,    keylen = 24

            select #6,  "USERLCMS",                                     ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   3,                     ~
                        alt key  1, keypos =   4, keylen =  30, dup

            call "SHOSTAT" ("Opening Files, One moment Please?")
                                                         /* (EWD0016)   */
            filename$ = "AWDPLNSR" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error

            call "OPENOLIB" (#6, "SHARE", f2%(6%), rslt$(6%), axd$)
                                                         

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            apc$   = "* (EWD) Screen Remake Scan Utility **"
            pname$ = "SCRSCANN - Rev: R7.00"


            dim rf_apc$20, rf_pname$8                       /* (AWD045)  */
            rf_apc$   = "Appian Mst Scan Util"              /* (AWD045)  */
            rf_pname$ = "AWDRFSCN"                          /* (AWD045)  */

            trauma_center$ = "N"                           /* (EWD014) */


                                          /* (AWD035) - Changed Screen */
            scr$(1%) = "****************************************"
            scr$(2%) = "*       SCANNING SELECTION CODES       *"
            scr$(3%) = "* ------------------------------------ *"
            scr$(4%) = "* (1) - Screen Scanning-Table          *"
            scr$(5%) = "* (2) - Screen Remake Scanning         *"
            scr$(6%) = "****************************************"

            tt_unit% = 0%

            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
                                               /* Screen Default Msg   */
            ld$(1%) = "RRRRR   EEEEEE  M     M     A     K    K  EEEEE"
            ld$(2%) = "R    R  E       MM   MM    A A    K   K   E    "
            ld$(3%) = "R    R  E       MM   MM   A   A   K  K    E    "
            ld$(4%) = "RRRRR   EEEE    M M M M   A   A   K K     EEEE "
            ld$(5%) = "R R     E       M M M M  AAAAAAA  KK K    E    "
            ld$(6%) = "R  R    E       M  M  M  A     A  K   K   E    "
            ld$(7%) = "R   RR  EEEEEE  M  M  M  A     A  K    K  EEEEE"

            ps$(1%) = " CCCCC     OOOOO    M      M   PPPPPPP         "
            ps$(2%) = "C     C   O     O   MM    MM   P      P        "
            ps$(3%) = "C         O     O   M M  M M   P      P        "
            ps$(4%) = "C         O     O   M  MM  M   PPPPPPP    [][] "
            ps$(5%) = "C         O     O   M      M   P          [][] "
            ps$(6%) = "C     C   O     O   M      M   P               "
            ps$(7%) = " CCCCC     OOOOO    M      M   P               "
                                               /* Scanned Error Msg    */
            ee$(1%) = "      SSSSS    TTTTTTT    OOOOO    PPPPPP     "
            ee$(2%) = "     S     S      T      O     O   P     P    "
            ee$(3%) = "       S          T      O     O   P     P    "
            ee$(4%) = "         S        T      O     O   PPPPPP     "
            ee$(5%) = "           S      T      O     O   P          "
            ee$(6%) = "     S     S      T      O     O   P      [][]"
            ee$(7%) = "      SSSSS       T       OOOOO    P      [][]"


            inp_text$(1%)="Scan Barcode or Manually Enter Barcode Number"
            inp_text$(3%)="Scan Screen Remake <Reason> Barcode?"
            inp_text$(5%)="Scan Screen Remake <Department> Barcode?"

            err$(1%)="(Error) Barcode Not on File, or Invalid?          "
            err$(2%)="(Error) Barcode Not on File for Department?       "
            err$(3%)="(Error) Barcode on File for Different Department? "
            err$(4%)="(Error) Barcode Not Valid for Department/Process? "
            err$(5%)="(Error) Barcode Has Already Been Scanned Complete?"
            err$(6%)="(Error) Updating Production Department (XXX)?     "
            err$(7%)="(Error) While Updating Barcode Audit ---------->  "

            err$(8%)="(Error) Updating Receive, Dept (XXX) Not Complete "
            err$(9%)="(Error) Updating Received Wood Surround Prod?     "
            err$(10%)="(Error) Screen Barcode not on file, or Invalid?    "
            err$(11%)="(Error) Screen Barcode Not Complete Cannot Re-Make?"
            err$(12%)="(Error) Updating Glass, Cannot Update Glass? ? ?  "


           err$(13%)="(Error) Product already Staged? ? ?               "
           err$(14%)="(Error) Product already Loaded? ? ? ?             "


           err$(15%)="(Error) Invalid Reason Code for Glass Re-Make?    "
           err$(16%)="(Error) Invalid Load No.? Not Valid for Load?     "
           err$(17%)="(Error) Shift Code Not Valid for <Time of Day>?   "
           err$(18%)="(Error) Glass Barcode Not Scheduled?              "
           err$(19%)="(Error) Tempered/Special Liting - Not Re-Make Scan"
           err$(20%)="(Error) Updating Glass? Glass Not Found?          "
           err$(21%)="(Error) Invalid Scanning User Id?                 "
           err$(22%)="(Error) Updating Glass Audit File?                "
           err$(23%)="(Error) Glass Barcode Already Complete?           "
           err$(24%)="(Error) Update EWDSCHED?                          "
                                                            /* (AWD031) */
           err$(25%)="(Error) Updating Screen Received File?            "
                                                            /* (AWD033) */
           err$(26%)="(Error) Invalid Scanning Department ?             "
                                                            /* (AWD043) */
           
            her$(1%) = "     I n v a l i d   B a r c o d e   N o .     "
            her$(2%) = "   N o t   O n   F i l e   F o r   D e p t.    "
            her$(3%) = "      O n   F i l e   F o r   D e p t.         "
            her$(4%) = "N o t  V a l i d  F o r  D e p t  P r o c e s s"
            her$(5%) = " P r o d u c t   A l r e a d y   S c a n n e d "
            her$(6%) = "      E r r o r   U p d a t i n g              "
            her$(7%) = "E r r o r   U p d a t i n g   A u d i t        "
            her$(8%) = " P r o d u c t   N o t  S c a n n e d  By - XXX"
            her$(9%) = "     P r o d u c t   N o t   S t a g e d       "
            her$(10%) = " S c r e e n   B a r c o d e   I n v a l i d   "
            her$(11%) = " S c r e e n   N o t   C o m p l e t e d       "
            her$(12%) = "  U n a b l e   T o   U p d a t e   G l a s s  "
            her$(13%) = " P r o d u c t   A l r e a d y   S t a g e d   "
            her$(14%) = " P r o d u c t   A l r e a d y   L o a d e d   "
            her$(15%) = " S c r e e n   R e a s o n   C o d e   Invalid?"
            her$(16%) = "     I n v a l i d   L o a d   N u m b e r     "
            her$(17%) = "      I n v a l i d   S h i f t   C o d e      "
            her$(18%) = " S c r e e n   N o t   S c h e d u l e d       "
            her$(19%) = "T e m p e r e d / S p e c i a l  Invalid Re-Make?"


                                                              /* (AWD045) - BEG */
            rf_err$(1%)="Brcd Not on File   "
            rf_err$(2%)="Brcd Not Dept      "
            rf_err$(3%)="Brcd Diff Dept     "
            rf_err$(4%)="Brcd Not Valid Dept"
            rf_err$(5%)="Brcd Already Cmplte"
            rf_err$(6%)="Update Prd Dpt(XXX)"
            rf_err$(7%)="Update Brcd Audit  "
            rf_err$(8%)="Update Stg Dpt(XXX)"
            rf_err$(9%)="Update Load Not Stg"
           rf_err$(10%)="Glass Brcd not file"
           rf_err$(11%)="Glass Brcd Not Cmpl"
           rf_err$(12%)="Cannot Update Glass"
           rf_err$(13%)="Product already Stg"
           rf_err$(14%)="Prd already Loaded"
           rf_err$(15%)="Reason Cde Glass RM"
           rf_err$(16%)="Invalid Load No."
           rf_err$(17%)="Shift Code Not Vali"
           rf_err$(18%)="Glass Brcd Not Sch"
           rf_err$(19%)="Tempered/Special Lt"
           rf_err$(20%)="Glass Not Found"
           rf_err$(21%)="Invalid Scann User"
           rf_err$(22%)="Update Glass Audit"
           rf_err$(23%)="Glass Brcd Complet"
           rf_err$(24%)="Not Valid App Labe"
           rf_err$(25%)="Ship Lbl not Prod"
           rf_err$(26%)="Updt Prod Dept(XXX)"
           rf_err$(27%)="Updt Dept(XXX)App"
           rf_err$(28%)="App Lbl Not On File"
           rf_err$(29%)="Not Stg No App Lbl"
           rf_err$(30%)="InvalidDepartment?"

            rf_her$(1%) = "Invalid Barcode No   "
            rf_her$(2%) = "Not On File For Dept "
            rf_her$(3%) = "On File For Dept     "
            rf_her$(4%) = "Not Valid For Dept   "   
            rf_her$(5%) = "Prod Already Scanned "
            rf_her$(6%) = "Error  Updating      "
            rf_her$(7%) = "Error Updating Audit "
            rf_her$(8%) = "Prod Not Scan By XXX "
            rf_her$(9%) = "Product Not Staged   "
            rf_her$(10%) = "Glass Barcode Invalid"
            rf_her$(11%) = "Glass Not Completed  "
            rf_her$(12%) = "Not To Update Glass  "
            rf_her$(13%) = "Prod Already Staged  "
            rf_her$(14%) = "Prod Already Loaded  "
            rf_her$(15%) = "Invalid Reason Code? "
            rf_her$(16%) = "Invalid Load Number  "
            rf_her$(17%) = "Invalid Shift Code   "
            rf_her$(18%) = "Glass Not Scheduled  "
            rf_her$(19%) = "Temp/Spec    Re-Make?"
            rf_her$(24%) = "Invalid Appian Bacode"
            rf_her$(25%) = "Ship Does Match Prod "
            rf_her$(26%) = "Updating Product Dept"
            rf_her$(27%) = "Updating Appian Label" 
            rf_her$(28%) = "Appian Lbl Not On File"
            rf_her$(29%) = "NOT STAGED-NO LABEL  "

            gosub load_support
                                                   /* (AWD029)           */


            check_passwd = 0
 
        initialize
            edit% = 0%
            init(" ") scr_shft$, scr_shft_d$, scr_proc$, scr_proc_d$,    ~
                      scr_dept$, scr_dept_d$, prevcode$, tt_unit$,       ~
                      scr_sel$, scr_sel_d$, scr_id$,                     ~
                      scrn_title$, title$, fld$(3%),                     ~
                      scr_id_d$, scr_load$, scr_load_d$, sav_scr_dept$,  ~
                      rm_dept_n$, dept_wand$, dept_d$, rm_status$,       ~
                      rm_status_d$(), gls_status$, sc_loc$
                                         /* (AWD043) */
REM Remake Description RM_STATUS$ + 1 ; because status starts at 0
            rm_status_d$(1%) = "Scanned in for Remake "
            rm_status_d$(2%) = "Glass in Remake Batch "
            rm_status_d$(3%) = "Glass Completed       "
            rm_status_d$(4%) = " "
            rm_status_d$(5%) = "Glass Received in Line"
            rm_status_d$(6%) = " "
            rm_status_d$(7%) = " "
            rm_status_d$(8%) = " "
            rm_status_d$(9%) = " "
            rm_status_d$(10%) = "Tempered Being Ordered"
            rm_status_d$(11%) = "Glass Not On File "

   
                                                          /* (EWD026)  */
            scr_proc$ = "01"           /* Set Default to Manufacturing */
        restart
            init(" ") scr_sel$, scr_sel_d$, scr_shft$, scr_shft_d$,      ~
                      scr_id$, scr_id_d$, sc_reason$, sc_reason_desc$,   ~
                      sc_loc$, sc_reason_hdr$
            scr_sel% = 0%
        main
            gosub mainmenu
            init(" ") errormsg$
            if keyhit% = 16% then exit_program

               gosub check_selection

               if code% = 0% then goto initialize

               gosub check_shift
               if code% = 0% then goto initialize
               gosub check_process
               if code% = 0% then goto initialize
               gosub check_user
               if code% = 0% then goto initialize
            edit% = 1%

            pass% = 0%                                /* (AWD037)       */
            if keyhit% = 14% then gosub check_pass
                if pass% <> 0% then goto main           /*  (AWD037)      */
            if keyhit% = 14% then gosub scan_it  
         goto main

         mainmenu                                /* Main Scanning Menu */
            tt_unit% = 0%
            gosub set_screen_1
            date$ = date
            call "DATEFMT" (date$)
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,11), "Scanning Selection    :",                    ~
               at (04,35), fac(lfac$(1%)), scr_sel$             , ch(01),~
               at (04,42), fac(hex(84)),scr_sel_d$              , ch(30),~
                                                                         ~
               at (05,11), "Scanning Shift Code   :",                    ~
               at (05,35), fac(lfac$(1%)), scr_shft$            , ch(02),~
               at (05,42), fac(hex(84)),scr_shft_d$             , ch(30),~
                                                                         ~
               at (06,11), "Scanning User Id.     :",                    ~
               at (06,35), fac(lfac$(1%)), scr_id$              , ch(03),~
               at (06,42), fac(hex(84)),scr_id_d$               , ch(30),~
                                                                         ~
                                                                         ~
               at (09,21), fac(hex(84)), scr$(1%)               , ch(40),~
               at (10,21), fac(hex(84)), scr$(2%)               , ch(40),~
               at (11,21), fac(hex(84)), scr$(3%)               , ch(40),~
               at (12,21), fac(hex(84)), scr$(4%)               , ch(40),~
               at (13,21), fac(hex(84)), scr$(5%)               , ch(40),~
               at (14,21), fac(hex(84)), scr$(6%)               , ch(40),~
               at (15,21), fac(hex(84)), scr$(7%)               , ch(40),~
               at (16,21), fac(hex(84)), scr$(8%)               , ch(40),~
               at (17,21), fac(hex(84)), scr$(9%)               , ch(40),~
               at (18,21), fac(hex(84)), scr$(10%)              , ch(40),~
               at (19,21), fac(hex(84)), scr$(11%)              , ch(40),~
               at (20,21), fac(hex(84)), scr$(12%)              , ch(40),~ 
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
               errormsg$ = " "

               if keyhit% <> 1% then goto L03880     /* Print Queue      */
                  run$ = "ILPMAN" 
                  gosub Run_Program
                  goto mainmenu

L03880:        if keyhit% <> 14% then L03890          /* Report/Utility   */
REM               pass% = 0%
REM               gosub check_pass                           
REM               if pass% <> 0% then goto initialize
REM               gosub scan_it     
REM               goto mainmenu

L03890:        if keyhit% <> 6% then L03930          /* DEPARTMENT CODES */
                  table% = 1%
                  gosub display_codes2
                  goto mainmenu

L03930:        if keyhit% <> 7% then L03940          /* WARRANTY LOOK_UP */
                  run$ = "EWDPLN82"
                  gosub Run_Program
                  goto mainmenu

L03940:        if keyhit% <> 9% then L04015          /* SHIFT CODES      */
                  table% = 4%
                  gosub display_codes2
                  goto mainmenu

L04015:        if keyhit% <> 15% then L04020
                  call "PRNTSCRN"
                  goto mainmenu

L04020:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_screen_1
            lfac$(1%) = hex(81)
            init(" ") dateout$, scrn_title$
            title$ = " S c a n n i n g   S e l c t i o n s "
            call "TIME" (dateout$)
            inpmessage$ = "Enter a Valid Scanning, Department, Shift Sele~
                          ~ction, Userid Required?"
            pf$(1%) = "(1)Print Queue                          " &       ~
                      "                       (14)Scanning    "
            pf$(2%) = "(7)Warranty Look-Up  ( 9)Shift Codes    " &       ~
                      "                       (15)Print Screen"
/*AWD031*/  pf$(3%) = "                                        " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ffffffffff07ff09ffffffff0e0f102000)
        return                                  /* (EWD022) - Add 12 Key */

        check_selection
           code% = 0%
           convert scr_sel$ to scr_sel%, data goto L11060

           if scr_sel% < 1% or scr_sel% > 2% then goto L11060


              scr_sel_d$ = str(scr$(3% + scr_sel%),9%,30%)
              str(scr_sel_d$,26%,3%) = scr_dept$
              sc_reason_hdr$ = "            "
/* CR1036          sc_loc_hdr$    = "            "    */
/* CR1036          sc_loc_hdr2$   = "            "    */
              sc_loc$        = " "
              if scr_sel% = 2% then sc_reason_hdr$ = "Reason Code:"
/* CR1036              if scr_sel% = 2% then sc_loc_hdr$  = "Location Code:" */
/* CR1036              if scr_sel% = 2% then sc_loc_hdr2$ = "1=GSO, 2=WEL" */
              if scr_sel% = 2% then sc_loc$ = "2"               /* CR1036 */
              if scr_sel% = 2% and userid$ = "ASM" then sc_loc$ = "2"
              if scr_sel% = 2% and userid$ = "APN" then sc_loc$ = "2"
              if scr_sel% = 2% and userid$ = "SCN" then sc_loc$ = "2"
              scrn_title$ = scr_sel_d$
              code% = 1%      
          return
 
L11060:   
           errormsg$ = "(Error) - Invalid Scanning Selection?"
           goto L11070                            /* (EWD001)          */

L11070:    gosub error_prompt                     /* (EWD001)          */
           code% = 0%
        return


        check_shift
           table$ = "PLAN SHFT"
           code$  = scr_shft$
           gosub check_code
           if code% = 0% then goto L11330
              scr_shft_d$ = desc$
        return
L11330:  
           errormsg$ = "(Error) - Invalid Shift Selection??"
           gosub error_prompt
        return

        check_process
           table$ = "PLAN PROC"
           code$  = scr_proc$
           gosub check_code
           if code% = 0% then goto L11440
              scr_proc_d$ = desc$
        return
L11440:    
           errormsg$ = "(Error) - Invalid Process Selection??"
           gosub error_prompt
        return


        check_user
           code% = 0%
           if scr_id$ = "XXX" then goto L11770    /* (EWD003) - 06/04/98 */
           init(" ") scr_id_d$
           read #6,key = scr_id$, using L11750, scr_id_d$,eod goto L11770
L11750:       FMT POS(4), CH(30)
           code% = 1%
        return
L11770:    
           errormsg$ = "(Error) - Invalid User ID, 'ID' is Required?"
           gosub error_prompt
           init(" ") scr_id$, scr_id_d$
        return

        check_code
           code% = 0%
           init(" ") readkey$, desc$
           str(readkey$,1%,9%)   = table$
           str(readkey$,10%,15%) = code$
           read #2,key = readkey$, using L12070, desc$, eod goto L12090
L12070:       FMT POS(25), CH(30)
           code% = 1%
L12090: return

        display_codes2
           call "APCPLN1B" ( table%, #2)
        return


        error_prompt
           comp% = 2%
           hh$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hh$, msg$(1%), msg$(2%), msg$(3%))
        return
                                                      /* (EWD007) - Mods */ 
        open_error                                    /* (EWD016)        */
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
        return
                                                      /* (EWD016)        */
        Run_Program:
           return% = 0% : comp% = 0%
           init(" ") rlib$, rvol$
           call "PROCLINK" (run$, rlib$, rvol$, return%, comp%)
        return  

        error_display
           comp% = 2%
           hh$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press PF(10) Key, To Continue."
           call "ASKUSER" (comp%, hh$, msg$(1%), msg$(2%), msg$(3%))
           if comp% <> 10% then goto error_display
        return

  
        load_support
            init(" ") supp_dept$()
            supp_max% = 0%
            readkey$ = all(hex(00))
            str(readkey$,1%,9%) = "PLAN SUPP"
            read #2,key > readkey$, using L16000, readkey$,            ~
                                                 eod goto load_support_done
            goto L16010
        load_support_next
            read #2, using L16000, readkey$, eod goto load_support_done
L16000:         FMT CH(24)
L16010:     if str(readkey$,1%,9%) <> "PLAN SUPP" then goto load_support_done
               if str(readkey$,10%,3%) > "090" then load_support_done
                  supp_max% = supp_max% + 1%
                  supp_dept$(supp_max%) = str(readkey$,10%,3%)
                                                       /* Load Dept   */
               goto load_support_next
        load_support_done
        return
                                                 /*             (AWD029)*/

                                                 /*  (AWD033)           */


       check_pass                                /*  (AWD037)          */
REM          if check_passwd = 1 then return
             check_passwd = 1
REM          passusr$ = "SC1"         
             passusr$ = scr_id$     
             if scr_sel$ = "1" then                     ~
             call "APCPASSW" ("SCRSCANN", passusr$, pass%)
 
       return                                    /*  (AWD037)          */


scan_it:
            title$ = "Screen Remake Scan"                  
            pname$ = "SCRSCANN - Rev: R1.00"                
            sc_reason$ = "   "
            sc_reason_desc$ = "                      "
            sc_desc$   = "   "
            sc_desc2$  = "   "
            scan_type$ = "Remake  "
            if scr_sel$ = "1" then scan_type$ = "Complete"                    
            if scr_sel$ = "1" then title$ = "Screen Scan Complete"             
           gosub calc_date_time

            u3% = 0%

        inputmode
            prevcode$     = str(hold_rec$(),42%,9%)   /* CR1036  */
            init(" ") sc_barcode$, sc_wand$ 
            sc_reason$ = "   "
            maxfld% = 2%    /* CR1036  */
            if scr_sel$ = "1" then maxfld% = 1% 
            for fieldnr% = 1% to maxfld%
                gosub'051(fieldnr%)        /* Default / Enables */
                      if enabled% = 0% then L10230
L10130:         gosub'101(fieldnr%, 1%)    /* Display / Accept  */
                      if keyhit%  =  1% then gosub startover
                      if keyhit% = 16% then restart      
                      if keyhit% <> 0% then       L10130
L10230:         gosub'151(fieldnr%)     /* Edit Field for Valid Entry */
                      if errormsg$ <> " " then L10130
             next fieldnr%
             go to inputmode

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
         "Enter a Barcode.                                             ", ~  
         "Enter a Location                                             ", ~  
         "Enter a Reason Code                                          "  

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
            *               S C R E E N   P A G E   1                   *~
            *-----------------------------------------------------------*~
            * Document Input and Edit Screen.                           *~
            *************************************************************

        deffn'101(fieldnr%, edit%)
              gosub'050(1%, fieldnr%)
L40150:       tt_unit$ = "Scanned Units [ XXXXXX ]"
              convert tt_unit% to str(tt_unit$,17%,6%), pic(######)
              gosub set_pf1
              
              if fieldnr% > 0% then init(hex(8c)) lfac$()                ~
                               else init(hex(86)) lfac$()
REM              on fieldnr% gosub L40160                                     
              gosub L40160
              goto L40190

                  lfac$(fieldnr%) = hex(80)  :  return  /* Up / Low   */
L40160:           lfac$(fieldnr%) = hex(81)  :  return  /* Upper Only */
                  lfac$(fieldnr%) = hex(82)  :  return  /* Numeric    */

L40190:     accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,65), "Today:",                                     ~
               at (01,72), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), title$                 , ch(40),~
               at (02,65), "Time:",                                      ~
               at (02,71), fac(hex(8c)), timed$                 , ch(08),~   
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
               at (04,29), fac(hex(84)), tt_unit$               , ch(24),~
                                                                         ~
               at (05,02), fac(hex(84)), "Barcode Number To Scan :",     ~
               at (05,30), fac(lfac$(1%)), sc_barcode$          , ch(09),~
               at (05,41), fac(lfac$(4%)), sc_wandchar$         , ch(01),~
               at (05,57), fac(hex(84)),   prevcode$            , ch(18),~
                                                                         ~
               at (06,18), fac(hex(84)),   scan_type$           , ch(08),~
               at (07,30), fac(hex(84)), sc_reason_hdr$,        ~
               at (07,44), fac(lfac$(2%)), sc_reason$           , ch(03),~
               at (07,48), fac(hex(84)),   sc_reason_desc$      , ch(30),~  
                                                                         ~
               at (08,18), fac(hex(84)), sc_desc$             , ch(40),~
               at (09,18), fac(hex(84)), sc_desc2$            , ch(40),~
                                                                         ~
               at (21,02), fac(hex(a4)), inpmessage$            , ch(79),~
               at (22,02), fac(hex(8c)), pf$(1%)                , ch(79),~
               at (23,02), fac(hex(8c)), pf$(2%)                , ch(79),~
               at (24,02), fac(hex(8c)), pf$(3%)                , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <>  9% then goto L40405
               readkey$ = " "
               str(readkey$,1%,9%) = "PLAN SCRN"
               descr$ =hex(06) & "Dept Batch/Sort/Descr Info"
               call "PLOWCODE" (#2, readkey$, descr$, 9%, .30, f1%(2%))
           sc_reason$ = str(readkey$,10,3)
           sc_reason_desc$ = descr$
L40405:        if keyhit% <>  5% then goto L40410
                  gosub get_scan_totals
                  goto L40150

L40410:        if keyhit% <> 15% then goto L40420
                  call "PRNTSCRN"
                  goto L40190

L40420:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

        set_pf1
/* CR1036 */
        if scr_sel% = 2% then goto L41110 
         pf$(1%) = "(1)Start Over                           " &        ~
                   "                                       "
         pf$(2%) = "(5)Calc Units                           " &        ~
                   "                       (15)Print Screen"
         pf$(3%) = "                                        " &        ~
                   "                       (16)Exit Program"
         goto L41120              
                       
L41110: pf$(1%) = "(1)Start Over                           " &        ~
                  "                       (9)Reason Codes "
        pf$(2%) = "(5)Calc Units                           " &        ~
                  "                       (15)Print Screen"
        pf$(3%) = "                                        " &        ~
                  "                       (16)Exit Program"
L41120: pfkeys$ = hex(01ffffff05ffffff09ffffffffff0f1000)
        return


        REM *************************************************************~
            *                     T E S T   D A T A                     *~
            *-----------------------------------------------------------*~
            * Test data for the items on Screen 1.                      *~
            *************************************************************

        deffn'151(fieldnr%)
            errormsg$ = " "
            on fieldnr% gosub L50010,        /* Barcode                */ ~  
                              L50030         /* Reason                 */ 

            return


L50010:
        if scr_sel$ = "2" then return          
               gosub lookup_part_number  
               gosub display_codes
        return 

L50030:
           table$ = "PLAN SCRN"
           code$  = sc_reason$
           gosub check_code
           if code% = 0% then goto L50035
           sc_reason_desc$ = desc$
            gosub lookup_part_number  
            gosub display_codes
        return 

L50035:  
           errormsg$ = "(Error) - Invalid Remake Reason! "
           gosub error_prompt
        return

/* CR1036  L50040:  */
/* CR1036        if sc_loc$ <> "1" and sc_loc$ <> "2" then goto L50044    */
/* CR1036        if sc_loc$ = "1" then sc_loc_desc$ = "Greensboro      "  */
/* CR1036        if sc_loc$ = "2" then sc_loc_desc$ = "Welcome         "  */
/*        return  */

/* CR1036     L50044:  */
/* CR1036           errormsg$ = "(Error) - Invalid Remake Location!"   */
/* CR1036           gosub error_prompt                                 */
/* CR1036        return                                                */

get_scan_totals           
           call "SHOSTAT" ("Calculating Scann Totals?")
           tt_unit% = 0%
           init(hex(00)) dt_key2$, rec$()
           date2$ = date
           str(dt_key2$,1%,5%) = date2$ 
           str(dt_key2$,6,7) = hex(00) & hex(00) & hex(00) & hex(00) & ~      
                               hex(00) & hex(00) & hex(00) 
           read #1,key 4% >= dt_key2$, using L50047,rec$(),eod goto L50048
             goto scan_tot_first
L50046:    read #1 using L50047,rec$(),eod goto L50048
L50047:    FMT 2*CH(256)
scan_tot_first:
           dt_key2$ = str(rec$(),205,12)

REM        if str(rec$(),163%,1%) <> "2" then L50048
REM        if str(rec$(),205%,5%) <> str(date2$,1,5) then L50046
           if str(rec$(),205%,5%) < str(date2$,1,5) then L50046 
           if str(rec$(),205%,5%) > str(date2$,1,5) then L50048   
           if str(rec$(),221%,2%) <> scr_shft$ then L50046       /* CR1036 */
/*           if str(rec$(),214%,3%) <> scr_id$ then L50046          (CR1036) */
           tt_unit% = tt_unit% + 1%       
           goto L50046

L50048:    tt_unit$ = "Scanned Units [ XXXXXX ]"
           convert tt_unit% to str(tt_unit$,17%,6%), pic(######)
REM        print at (04,29), tt_unit$
           comp% = 2%
           hh$  = "                                        "
           msg$(1%) = " - - - - - - - - I n f o - - - - - - - - - "
           msg$(2%) = tt_unit$ 
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hh$, msg$(1%), msg$(2%), msg$(3%))
       return

lookup_part_number

            init(" ") rec$(), errormsg$, hold_rec$()
            key$ = "         "
            first$ = "Y"
            str(key$,1,9) = sc_barcode$
            str(key$,10,3) = "000"
read_next:
            read #1, key > key$, using F50021, rec$(), eod goto not_found
F50021:     FMT 2*CH(256)  
            first$ = "N"
            hold_key$ = str(rec$(),42,12)
            hold_st$ = str(rec$(),163,1) 
            if str(rec$(),42,9) <> sc_barcode$ then goto end_loop 
        /* don't allow stat 0 be scanned complete */
/*<AWD001>  hold_rec$() = rec$() & "                                       " &~
                 "                                                         "*/
            hold_rec$() = rec$()                 /*<AWD001>*/
            key$ = hold_key$
            go to read_next

end_loop:  /* hold_rec$() has last rec for barcode */
            if str(hold_rec$(),42,9) <> sc_barcode$ then goto not_found 
            hold_st$ = str(hold_rec$(),163,1)
            hold_key$ = str(hold_rec$(),42,12)

            if str(scan_type$,1,1) = "C" then goto Complete_scr

REM     Remake / Repair                      

REM     Remake only completed screens                                          
REM         if hold_st$ <> "2" and str(sc_reason$,1,1) = "0" then not_completed
REM     Remake & Repair must be completed... now (as of 4/19/2010)
            if hold_st$ <> "2" then not_completed

            num% = 0%
            convert str(hold_rec$(),51,3) to num%, data goto cnv_err
            num% = num% + 1%
            convert num% to str(hold_rec$(),51,3), pic (000)
            str(hold_rec$(),173,3) = str(hold_rec$(),51,3)
            str(hold_rec$(),163,1) = "0"

REM   on repair, set status to 1
REM         if str(sc_reason$,1,1) = "1" then str(hold_rec$(),163,1) = "1"
REM         if str(sc_reason$,1,1) = "1" then str(hold_rec$(),34,5) = "99999"

REM  completed  205-209 5pd yyyymmdd
REM  time       210-213 4c
REM  id         214-216 3c
REM  prod_time  217-220 4c
REM remake_sft  221-222 2c
REM reason      223-225 3c
REM remake usr  225-227 3c
REM org prod dt 228-233 5pd yyyymmdd
 
/*            sc_desc$ = hold_key$ & " Added."        (CR1036)  */     
            sc_desc$ = str(hold_rec$(),42,12) & " Added. "
            
            sc_desc2$ = "Part " & str(hold_rec$(),138,25) 
            sc_barcode$ = "         "
            date$ = date
            if str(hold_rec$(),229,5)  = "      " then                   ~
               str(hold_rec$(),229,5)  = str(hold_rec$(),1,5)
            str(hold_rec$(),1,6)  = date$
            str(hold_rec$(),8,6)  = date$
            call "DATEFMT" (date$)
            str(hold_rec$(),205,5)  = "     "
            str(hold_rec$(),210,4)  = "    "
            str(hold_rec$(),214,3)  = scr_id$
            time$ = time
            tt_unit% = tt_unit% + 1%    /* CR1036  */
REM         +---------------------------------------------------------------+
REM         | assign screen_date on scan remake  @@@                        |
REM         +---------------------------------------------------------------+
            if sc_loc$ = "1" then str(hold_rec$(),7,1) = "2"
            if sc_loc$ = "2" then str(hold_rec$(),7,1) = "3"
/*<AWD001   str(hold_rec$(),14,20)  = "                   "    */
            str(hold_rec$(),14,20)  = "                    "   /*<AWD001> */
            str(hold_rec$(),217,4)  = time$
            str(hold_rec$(),221,2)  = scr_shft$                /* CR1036 */
            str(hold_rec$(),223,3)  = sc_reason$
            str(hold_rec$(),226,3)  = userid$
            
            write #1, using F50021, hold_rec$()
        return

not_found:
            if first$ = "N" then goto completed
            sc_desc$ = "Not Found.         "
            errormsg$ = "(Error) - Invalid Barcode!        "
            sc_desc2$ = "                   "
        return

completed:
REM         sc_desc$ = "Already Completed. "
REM         errormsg$ = "(Error) - Already Completed!      "
            sc_desc$ = "Invalid Entry.     "
            errormsg$ = "(Error) - Invalid Entry!          "
            sc_desc2$ = "                   "
        return

cnv_err:
            errormsg$ = "(Error) - Invalid Barcode!        "
            sc_desc$ = "Conversion Error.  "
            sc_desc2$ = "                   "
        return

not_completed:
            errormsg$ = "(Error) - Non Completed Screen    "
            sc_desc$ = "Non Completed Screen Found. " & hold_key$
            sc_desc2$ = "                   "
        return

Complete_scr:
            if hold_st$ > "1" then completed
            key$ = str(hold_rec$(),42,12)
            if str(key$,1,9) <> sc_barcode$ then goto not_found
            read #1, key = key$, hold, using F50021, rec$(), eod goto not_found
              num% = 0%
              str(hold_rec$(),163,1) = "2"
              sc_desc$ = key$ & " Completed."
             sc_desc2$ = "Part " & str(hold_rec$(),138,25)                
             sc_barcode$ = "         "
             date$ = date
REM         +---------------------------------------------------------------+
REM         | Complete                                            @@@       |
REM         +---------------------------------------------------------------+
            if sc_loc$ = "1" then str(hold_rec$(),7,1) = "2"
            if sc_loc$ = "2" then str(hold_rec$(),7,1) = "3"
            tt_unit% = tt_unit% + 1%
            str(hold_rec$(),205,5)  = date$
            call "DATEFMT" (date$)
            time$ = time
            str(hold_rec$(),210,4) = time$
            str(hold_rec$(),214,3)  = scr_id$
REM     str(hold_rec$(),217,4)  = "    "
REM     str(hold_rec$(),223,3)  = "   "          /* reason */
            str(hold_rec$(),221,2)  = scr_shft$    
            rewrite #1, using F50021, hold_rec$()
        return

        calc_date_time
           date$ = date
           call "DATEFMT" (date$)

           time$ = time
           hr$ = str(time$,1%,2%) 
           mm$ = str(time$,3%,2%)
           a_m$ = "AM"
           hr% = 0%
           convert hr$ to hr%, data goto t_1
T_1:
           if hr% >= 12% then a_m$ = "PM"
           if hr% >= 12% then hr% = hr% - 12%
           convert hr% to hr$, pic(00)

           timed$ = hr$ & ":" & mm$ & " " & a_m$
        return

        REM *************************************************************~
            *           D I S P L A Y   C O D E   T A B L E             *~
            *-----------------------------------------------------------*~
            * Display Screen                                            *~
            *************************************************************

        display_codes

               if keyhit% <> 15 then goto L02910
                  call "PRNTSCRN"
                  goto L02910

L02910:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return

  exit_program            
        end   

