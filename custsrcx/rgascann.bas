        REM *************************************************************~
            *                                                           *~
            *  Program Name      - RGASCANN                             *~
            *  Creation Date     - 05/16/05                             *~
            *  Last Modified Date- 01/01/06                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Last Mod By       - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Scanning Utility used for tracking   *~
            *                      RGA's.                               *~
            *                                                           *~
            *                                                           *~
            *  Code Tables Used                                         *~ 
            *        (RGAREASON) RGA Reason Code Associated with Return *~
            *        (RGASTATUS) RGA Salvage Tracking Status Code       *~
            *        (RGATR-LOC) RGA Trailer/Location                   *~
            *        (RGASURGE ) RGA Surge Sku's for use by Planning    *~
            *        (RGASALVAG) 2005 Salvage Costs by Department       *~  
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *              Note  - 'USERLCMS' located in /CMS/SESDBASE/ *~
            *                                                           *~
            *                      Our Copy is in /CMS4/APCDATA/        *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/16/05 ! New Program for (AWD) - Last Mod Date    ! RHH *~
            * 01/01/06 ! (PAR000) CR347 Mod for new Sub Part No.  ! RHH *~
            *          !                                          !     *~
            *************************************************************

        dim hdr$47, code$10,             /* ASKUSER Header             */~
            readkey$50, descr$30,        /* GENCODES Lookup            */~
            filename$8,                  /* Use with EWDOPEN - EWD016  */~
            msg$(3%)79, hh$40,           /* Scanning User Id           */~
            pfkeys$40,                   /* PF KEYS                    */~
            xx$(7%)50,                   /* Screen Display area Text   */~
            gl$(7%)50,                   /* 'Glass' Message            */~
            ps$(7%)50,                   /* Scan 'COMP'lete Text Screen*/~
            ee$(7%)50,                   /* 'STOP' Message Error Text  */~
            err$(30%)50,                 /* Defined Error Messages     */~
            her$(30%)50,                 /* Error Text Display         */~
            barcode$8,                   /* Scanned Barcode            */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dateout$8,                   /* Time Extract For Screen    */~
            inp_text$(6%)79,             /* Input Prompt Text          */~
            fld$(10%)22,                 /* Field Text                 */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(10%)1,                 /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            scrn_title$40,               /* Screen Description         */~
            wandchar$1,                  /* Wand Character - Scanner   */~
            userid$3                     /* Current User Id            */

         dim                             /* (AWDRGAHD) an (AWDRGADT)   */~
            rga_key0$8, rga_rec$(2%)256, /* Primary Key                */~
            rga_dt_key$10, rga_dt_rec$128,/* Primary Key               */~
            sc_status$3, sc_status_d$30, /* RGA Status Code            */~
            sc_reason$3, sc_reason_d$30, /* RGA Reason Code            */~
            sc_trailer_loc$6,            /* RGA Trailer/Loc            */~
            sc_trailer_loc_d$30,         /* RGA Trailer Descript       */~
            sc_comment$32,               /* RGA Additional Info        */~
            rga_status$3,                /* RGA Status Code            */~ 
            rga_serial$8,                /* Serial Number Assigned     */~
            rga_status_seq$2,            /* Next Seq Number            */~
            rga_trailer_loc$6,           /* RGA Trailer Loc.           */~
            rga_reason_cd$3,             /* RGA Reason Code            */~
            rga_status_tme$4,            /* RGA Time assoc. with status*/~
            rga_comment$32,              /* RGA Additional Info.       */~
            calc_time$8                  /* Scan Time                  */

                                         /* (AWDRGAHD) - Header Data   */
        dim rga_status1$3,               /* RGA Status/Store(RGASTATUS)*/~
            rga_status1_d$30,            /* RGA Status Display Header  */~
            rga_warranty_id$8,           /* Warranty Id from Sales Ord */~
            rga_so_barcode$18,           /* Barcode assoc. with S. O.  */~
            rga_comp_no$8,               /* Complaint No. with S. O.   */~
            rga_number$4,                /* RGA No. Assigned to Prod.  */~
            rga_number_ln$2,             /* RGA Line Item No.          */~
            rga_sales_ord$8,             /* Sales Ord. Assoc. W/Prod.  */~		 
            rga_sales_ln$3,              /* Line Item Assoc. W/S.O.    */~
            rga_part$25,                 /* RGA Part No. / MFG Part No.*/~
            rga_sub_part$20,             /* New Sub Part No.   (PAR000)*/~
            rga_descr$30,                /* Part No. Description       */~			
	    rga_comp_code$7,             /* Compalint Code W/Complaint */~
            rga_comp_code_d$30,          /* Complaint Header Display   */~
            rga_comp_reas$3,             /* Reason Code W/ Complaint   */~
            rga_comp_reas_d$30,          /* Reason Comp Header         */~
            rga_trailer1_loc$6,          /* Trailer/Location Assigned  */~
            rga_trailer1_loc_d$30,       /* Trailer Display Header     */~  
            rga_reason1_cd$3,            /* RGA Reason Code (RGAREASON)*/~
            rga_reason1_cd_d$30,         /* Reason Display Header      */~
            rga_comment1$32,             /* Additional Info. Header    */~			
            rga_qty_chk$1,               /* Flagged for QTY Check Y/N  */~
            rga_qty_dte$10,              /* RGA Date Checked by Quality*/~
            rga_return_dte$10,           /* Date Assoc. w. RGA Return  */~
            rga_dept$3,                  /* Production Department Code */~
            rga_dept_d$30,               /* Department Header          */~
            rga_stat_dte$10,             /* Date of Last Status Change */~
            rga_return_tme$5,            /* Time of Last Sataus Change */~
            rga_init_usr$3,              /* User Id that made last Chg */~   
            serial_display$27,           /* Flashing display           */~
            rga_cust$9,                  /* Sales Order Customer No.   */~
            prompt$79,                   /* Input header Display       */~
            title$40                     /* Header Display Title       */
 

        dim rlib$8,                      /* Run library for link call  */~
            rvol$6,                      /* Run volume for link call   */~
            run$8                        /* Program to Run             */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            fs%(10%)                     /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
                                         /* Text from file opening     */

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
            * #1  ! AWDRGAHD ! RGA Tracking Header File                 *~
            * #2  ! AWDRGADT ! RGA Tracking Detail File                 *~
            * #5  ! GENCODES ! GENERAL CODES MASTER FILE                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

           select  #1, "AWDRGAHD",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 512,                                   ~
                        keypos = 10, keylen = 8,                          ~
                        alt key 1, keypos =  1, keylen =  17,            ~
                            key 2, keypos = 18, keylen =   8, dup,       ~
                            key 3, keypos = 26, keylen =  18, dup,       ~
                            key 4, keypos = 44, keylen =   5, dup,       ~
                            key 5, keypos = 49, keylen =   6, dup,       ~
                            key 6, keypos = 55, keylen =  11, dup,       ~
                            key 7, keypos = 66, keylen =  25, dup
  
            select #2,  "AWDRGADT",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 128,                                   ~
                        keypos =  7, keylen = 10,                        ~
                        alt key 1, keypos  =  1, keylen = 16


            select #5,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =  1,   keylen = 24



            call "SHOSTAT" ("Opening Files, One moment Please?")
                                  
            filename$ = "AWDRGAHD" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AWDRGADT" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
 
            dim apc$40, pname$21   
            apc$   = "**** (RGA) Tracking System Sanning ****"
            pname$ = "RGASCANN - 05/26/2005"


            b_max% = 10%                     /* SET NUMBER OF TIMES TO */
                                             /* RING BELL ON SCREEN    */
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
                                               /* Screen Default Msg   */
            gl$(1%) = "          RRRRRR     GGGGG     AAAAA "
            gl$(2%) = "          R     R   G     G   A     A"
            gl$(3%) = "          R     R   G     G   A     A"
            gl$(4%) = "          RRRRRR    G         AAAAAAA"
            gl$(5%) = "          A R       G  GGGG   A     A"
            gl$(6%) = "          A   R     G     G   A     A"
            gl$(7%) = "          A     R    GGGGG    A     A"

                                               /* Scanned Complete Msg */
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


            err$(1%) ="(Error) RGA Serial Number not on File?            "
            err$(2%) ="(Error) RGA Cannot Convert Last Sequence Number?  "
            err$(3%) ="(Error) RGA Invalid RGA Status Code?              "
            err$(4%) ="(Error) RGA Invalid RGA Reason Code?              "
            err$(5%) ="(Error) RGA Invalid RGA Trailer-Loc Number        "
            err$(6%) ="(Error) RGA Re-Writing RGA Header Record?         "
            err$(7%) ="(Error) RGA Creating Tracking Detail Record?      "
            err$(8%) ="(Error) RGA Shipped - Product Shipped (Closed)?   "
            err$(9%) ="(Error) RGA Sold - Product Sold (Closed)?         "
            err$(10%)="(Error) RGA Cannot Be Scanned (Trailer Closed)?   "
            err$(11%)="(Error) RGA Cannot Be Scanned (Product Scrap)?    " 

            her$(1%) = " I N V A L I D   R G A   S E R I A L   N O.    "
            her$(2%) = "   C A N N O T   C O N V E R T   S E Q.        "
            her$(3%) = " I N V A L I D   R G A   S T A T U S   C O D E "
            her$(4%) = " I N V A L I D   R G A   R E A S O N   C O D E "
            her$(5%) = " I N V A L I D   R G A   T R A I L E R - L O C "     
            her$(6%) = "E R R O R   R E - W R I T I N G   H E A D E R  "
            her$(7%) = "E R R O R   W R I T I N G   D E T A I L   R E C"
            her$(8%) = "E R R O R   P R O D U C T   S H I P P E D      "
            her$(9%) = "E R R O R   P R O D U C T   S O L D            "
            her$(10%)= "E R R O R   T R A I L E R   I S   C L O S E D  "
            her$(11%)= "E R R O R   P R O D U C T   I S   S C R A P    " 

        main
            gosub initialize

            gosub mainmenu
            init(" ") errormsg$
            if keyhit% = 16% then exit_program
            edit% = 1%
         goto main

        initialize
            err%  = 0%
            edit% = 0%
            init(" ") sc_status$, sc_status_d$, sc_reason$, sc_reason_d$,~
                      sc_trailer_loc$, sc_comment$, barcode$, wandchar$, ~
                      xx$(), sc_trailer_loc_d$
        return
     

        REM *************************************************************~
            *       AES and AED Inventory Scanning                      *~
            *************************************************************

        mainmenu
            fieldnr% = 1%
            gosub'100(fieldnr%)                      /* Scan Barcode  */

            if keyhit% <> 16% then goto RGA_SCAN_1
               return clear all
               end

RGA_SCAN_ERR:
               gosub err_scrn
               return clear all
               goto main
                                                     /* Process Data         */
RGA_SCAN_1:
            init(" ") errormsg$
            gosub check_rga_serial_no                /* Serial Number Lookup */
            if err% <> 0% then goto RGA_SCAN_ERR

RGA_SCAN_2:
            err% = 0%
            init(" ") errormsg$, code$
            fieldnr% = 2%
            gosub'100(fieldnr%)
            str(code$,1%,3%) = sc_status$
            gosub lookup_rga_status                  /* Verify RGA Status Code*/
            if err% <> 0% then gosub err_scrn 
            if err% <> 0% then goto RGA_SCAN_2
            rga_status$  = sc_status$
            sc_status_d$ = descr$

RGA_SCAN_3:
            err% = 0%
            init(" ") errormsg$, code$
            fieldnr% = 3%
            gosub'100(fieldnr%)
            str(code$,1%,3%) = sc_reason$
            gosub lookup_rga_reason                  /* Verify RGA Reason Code*/
            if err% <> 0% then gosub err_scrn
            if err% <> 0% then goto RGA_SCAN_3
            rga_reason_cd$ = sc_reason$
            sc_reason_d$   = descr$
            

RGA_SCAN_4:
            err% = 0%
            init(" ") errormsg$, code$ 
            fieldnr% = 4%
            gosub'100(fieldnr%)
            str(code$,1%,6%) = sc_trailer_loc$
            gosub lookup_trailer_loc                 /* Verify Trailer-Loc    */
            if err% <> 0% then gosub err_scrn
            if err% <> 0% then goto RGA_SCAN_4
            rga_trailer_loc$  = sc_trailer_loc$
            sc_trailer_loc_d$ = descr$

            fieldnr% = 5%
            gosub'100(fieldnr%)                      /* Additional Info       */
            rga_comment$ = sc_comment$
 
RGA_SCAN_5:
            fieldnr%= 6%
            gosub'100(fieldnr%)
            if keyhit% <> 16% then goto RGA_SCAN_6
               return clear all
               goto main

RGA_SCAN_6:
            if keyhit% <> 10% then goto RGA_SCAN_5

            gosub update_awdrgadt
            if err% <> 0% then goto RGA_SCAN_ERR
     
                                                     /* Completed       */
            gosub ok_scrn

        goto main

        REM *************************************************************~
            *       END of Main Scanning                                *~
            *************************************************************


        deffn'100(fieldnr%)
SCR_1:
     gosub set_screen_1
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), scrn_title$            , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(84)), fld$(1%)               , ch(22),~
               at (05,27), fac(lfac$(1%)), barcode$             , ch(08),~
               at (05,50), fac(lfac$(2%)), wandchar$            , ch(01),~
                                                                         ~
               at (06,02), fac(hex(84)), fld$(2%)               , ch(22),~
               at (06,27), fac(lfac$(3%)), sc_status$           , ch(03),~
               at (06,40), fac(hex(84)), sc_status_d$           , ch(30),~
                                                                         ~
               at (07,02), fac(hex(84)), fld$(3%)               , ch(22),~
               at (07,27), fac(lfac$(4%)), sc_reason$           , ch(03),~
               at (07,40), fac(hex(84)), sc_reason_d$           , ch(30),~
                                                                         ~
               at (08,02), fac(hex(84)), fld$(4%)               , ch(22),~
               at (08,27), fac(lfac$(5%)), sc_trailer_loc$      , ch(06),~
               at (08,40), fac(hex(84)), sc_trailer_loc_d$      , ch(30),~
                                                                         ~
               at (09,02), fac(hex(84)), fld$(5%)               , ch(22),~
               at (09,27), fac(lfac$(6%)), sc_comment$          , ch(30),~
                                                                         ~
               at (12,16), fac(hex(84)), xx$(1%)                , ch(50),~
               at (13,16), fac(hex(84)), xx$(2%)                , ch(50),~
               at (14,16), fac(hex(84)), xx$(3%)                , ch(50),~
               at (15,16), fac(hex(84)), xx$(4%)                , ch(50),~
               at (16,16), fac(hex(84)), xx$(5%)                , ch(50),~
               at (17,16), fac(hex(84)), xx$(6%)                , ch(50),~
               at (18,16), fac(hex(84)), xx$(7%)                , ch(50),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 1% then goto SCR_2
                  gosub startover

SCR_2:         if keyhit% <> 3% then goto SCR_2A1    /* Print Que        */
                  run$ = "ILPMAN" 
                  gosub Run_Program
                  goto SCR_1
                
SCR_2A1:       if keyhit% <> 8% then goto SCR_2A2
                  gosub display_header
                  goto SCR_1

SCR_2A2:       if keyhit% <> 9% then goto SCR_2A3
                  tab% = 26%
                  gosub display_codes
                  goto SCR_1

SCR_2A3:       if keyhit% <> 11% then goto SCR_2A4
                  tab% = 27%
                  gosub display_codes
                  goto SCR_1 

SCR_2A4:       if keyhit% <> 12% then goto SCR_3
                  tab% = 28%
                  gosub display_codes
                  goto SCR_1

SCR_3:         if keyhit% <> 15% then goto SCR_4
                  call "PRNTSCRN"
                  goto SCR_1

SCR_4:         close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_screen_1
            unpack% = 0%
            init(" ") scrn_title$
               scrn_title$ = "RGA Detail Tracking and Salvage"

            copy gl$() to xx$()

            inp_text$(1%)="Scan Barcode or Manually Enter Barcode Number"
            inp_text$(2%)="Enter a Valid RGA Status Code (Required)?    "
            inp_text$(3%)="Enter a Valid RGA Reason Code (Required)?    "
            inp_text$(4%)="Enter a Valid Trailer/Loc (Optional)?        "
            inp_text$(5%)="Enter Additional Information about RGA?      "

            init(" ") dateout$
            inpmessage$ = inp_text$(fieldnr%)
            call "TIME" (dateout$)
                                                                      
            fld$(1%)      = "  RGA Serial Number  :"
            fld$(2%)      = "  RGA Status Code (3):"
            fld$(3%)      = "  RGA Reason Code (3):"
            fld$(4%)      = "  RGA Trailer/Loc (6):"
            fld$(5%)      = "  RGA Addition Info. :"
                                                     
            pf$(1%) = "(1)Startover      (8)Display Header     " &       ~
                      "                       (11)RGA Reason  "
            pf$(2%) = "(3)Print Que      (9)RGA Status         " &       ~
                      "                       (12)RGA Trailer "
            pf$(3%) = "                  (10)RGA Update        " &       ~
                      "                       (16)Exit Screen "
            pfkeys$ = hex(01ff03ffffffff08090a0b0cff0e0f1000)

            if fieldnr% < 6% then str(fld$(fieldnr%),1%,2%) = "->"
                                                  /* Current Input Field */

            if fieldnr% = 1% then goto SCR_1A
                str(pf$(3%),60%)    = " " : str(pfkeys$,16%,1%) = hex(ff)
              
SCR_1A:     if fieldnr% <> 1% then goto SCR_2A   
               str(pf$(1%),19%,20%)= " " : str(pfkeys$, 8%,1%) = hex(ff)
               str(pf$(3%),18%,18%) = " " : str(pfkeys$,10%,1%) = hex(ff)
               init(" ") barcode$, wandchar$      /* Barcode Scan       */
               lfac$(1%) = hex(81) : lfac$(2%) = hex(99)
               lfac$(3%), lfac$(4%), lfac$(5%), lfac$(6%) = hex(84) 
        return

SCR_2A:     if fieldnr% <> 2% then goto SCR_3A
               str(pf$(3%),18,18%) = " " : str(pfkeys$,10%,1%) = hex(ff)
               lfac$(1%), lfac$(2%) = hex(84)     /* RGA Status Code    */ 
               lfac$(3%) = hex(81) 
               lfac$(4%), lfac$(5%), lfac$(6%) = hex(84) 
        return

SCR_3A:     if fieldnr% <> 3% then goto SCR_4A    /* RGA Reason Code   */  
               str(pf$(3%),18%,18%) = " " : str(pfkeys$,10%,1%) = hex(ff)
               lfac$(1%), lfac$(2%), lfac$(3%) = hex(84)
               lfac$(4%) = hex(81) 
               lfac$(5%), lfac$(6%) = hex(84) 
        return
SCR_4A:     if fieldnr% <> 4% then goto SCR_5A    /* RGA Trailer Loc   */ 
               str(pf$(3%),18%,18%) = " " : str(pfkeys$,10%,1%) = hex(ff)
               lfac$(1%), lfac$(2%), lfac$(3%), lfac$(4%) = hex(84)
               lfac$(5%) = hex(81) 
               lfac$(6%) = hex(84) 
        return
SCR_5A:     if fieldnr% <> 5% then goto SCR_6A 
               str(pf$(3%),18%,18%) = " " : str(pfkeys$,10%,1%) = hex(ff)
               lfac$(1%), lfac$(2%), lfac$(3%), lfac$(4%), lfac$(5%) = hex(84)
               lfac$(6%) = hex(80) 
        return
SCR_6A:
               lfac$(1%), lfac$(2%), lfac$(3%) = hex(84)
               lfac$(4%), lfac$(5%), lfac$(6%) = hex(84) 
                                               
               lfac$(1%) = hex(81)             /* Position Cursor   */ 
        return

        check_rga_serial_no
           init(" ") rga_key0$, rga_rec$(), code$

           rga_serial$ = barcode$              /* Set Serial Number */

           rga_serial% = 0%                    /* For Hand Entry    */
           convert rga_serial$ to rga_serial%, data goto RGA_ER2

           convert rga_serial% to rga_serial$, pic(00000000)

           barcode$ = rga_serial$              /* Insure Format     */

           rga_key0$   = rga_serial$  
           read #1,key 0% = rga_key0$, using RGA_1, rga_rec$(),           ~
                                                          eod goto RGA_ER1
RGA_1:         FMT 2*CH(256)
                                             /* Check RGA Status For Closed */
               if str(rga_rec$(),7%,3%) = "030" then goto RGA_ER8
               if str(rga_rec$(),7%,3%) = "032" then goto RGA_ER9
               if str(rga_rec$(),7%,3%) = "014" then goto RGA_ER11 

                                             /* Check Trailer              */
               rga_trailer_loc$ = str(rga_rec$(),131%,6%)
               sc_trailer_loc$  = rga_trailer_loc$
               str(code$,1%,6%) = sc_trailer_loc$
               gosub lookup_trailer_loc

                                             /* Get Last Tracking Seq. No. */
               rga_status_seq$ = str(rga_rec$(),249%,2%)
               convert rga_status_seq$ to rga_status_seq%, data goto RGA_ER2

               rga_status_seq% = rga_status_seq% + 1%

               convert rga_status_seq% to rga_status_seq$, pic(00)
 

               rga_comment$     = str(rga_rec$(),140%,32%)
               sc_comment$      = rga_comment$
            return
RGA_ER1:
        err% = 1%                            /* Serial Number Not on File */
        return
RGA_ER2:
        err% = 2%                            /* Cannot Convert Last Seq No*/
        return
RGA_ER3:
        err% = 3%                            /* Invalid RGA Status Code   */
        init(" ") sc_status$
        return
RGA_ER4:
        err% = 4%                            /* Invalid RGA Resaon Code   */
        init(" ") sc_reason$
        return
RGA_ER5:
        err% = 5%                            /* Invalid RGA Trailer-Loc   */
        init(" ") sc_trailer_loc$, sc_trailer_loc_d$
        return
RGA_ER6:
        err% = 6%                            /* Error Rewriting header    */
        return
RGA_ER7:
        err% = 7%                            /* Error Writing Detail Rec  */
        return
RGA_ER8:
        err% = 8%                            /* Product Shipped (Closed)  */
        return
RGA_ER9:
        err% = 9%                            /* Product Sold (Closed)     */
        return
RGA_ER10:
        err% = 10%                           /* Trailer is Closed         */
        return
RGA_ER11:
        err% = 11%                           /* Product is Scrap          */
        return

        lookup_rga_status                    /* RGA Status Code           */
            init(" ") readkey$, descr$
            str(readkey$,1%,9%)  = "RGASTATUS"
            str(readkey$,10%,3%) = str(code$,1%,3%) 
            read #5,key = readkey$, using RGA_2, descr$, eod goto RGA_ER3
RGA_2:        FMT POS(25), CH(30)
              if unpack% = 99% then return
                 if str(code$,1%,3%) = "002" then goto RGA_ER3
                 if str(code$,1%,3%) = "004" then goto RGA_ER3
                 if str(code$,1%,3%) = "006" then goto RGA_ER3
        return

        lookup_rga_reason                    /* RGA Reason Code           */
            init(" ") readkey$, descr$
            str(readkey$,1%,9%)  = "RGAREASON"
            str(readkey$,10%,3%) = str(code$,1%,35) 
            read #5,key = readkey$, using RGA_2, descr$, eod goto RGA_ER4
        return


        lookup_trailer_loc                 /* Verify Trailer-Loc    */
            init(" ") readkey$, descr$     /* Only Put into Salvage */
                                           /* Can have a trailer No.*/
            if sc_status$ <> "010" then rga_trailer_loc$,sc_trailer_loc$ = "T00000"

            str(readkey$,1%,9%)  = "RGATR-LOC"
            str(readkey$,10%,6%) = str(code$,1%,6%) 
            read #5,key = readkey$, using RGA_2, descr$, eod goto RGA_ER5
            if str(code$,1%,6%) = "T00000" then return
            if unpack% = 99% then return
               if str(descr$,30%,1%) = "C" then goto RGA_ER10
        return

        update_awdrgadt
           init(" ") rga_key0$, calc_time$, rga_rec$()
           rga_key0$   = rga_serial$  
           read #1,hold,key 0% = rga_key0$, using RGA_1, rga_rec$(),           ~
                                                          eod goto RGA_ER1
                                             /* (RHHTEST)                 */
              delete #1

              calc_time$                 = time
              str(rga_status_tme$,1%,2%) = str(calc_time$,1%,2%)  /* Set Hour   */
              str(rga_status_tme$,3%,2%) = str(calc_time$,3%,2%)  /* Set Min    */

                                             /* Set last Status Code      */
              str(rga_rec$(),7%,3%)   = rga_status$
                                             /* Set Trailer/Loc           */
              str(rga_rec$(),131%,6%) = rga_trailer_loc$
                                             /* Set RGA Reason Code       */
              str(rga_rec$(),137%,3%) = rga_reason_cd$
                                             /* Latest Comment            */
              str(rga_rec$(),140%,32%)= rga_comment$
                                             /* Quality Check             */
              if str(rga_rec$(),172%,1%) = "Y" then rga_qty_dte$ = str(rga_rec$(),173%,6%)~
                                              else init(" ") rga_qty_dte$
              if rga_status$ = "020" then rga_qty_chk$ = "Y"
              str(rga_rec$(),172%,1%) = rga_qty_chk$
                                             /* Quality Check Date        */
              if rga_status$ = "020" then init(" ") rga_qty_dte$ 
              if rga_status$ = "022" then init(" ") rga_qty_dte$              
              if rga_status$ = "022" then str(rga_qty_dte$,1%,6%) = date              

              str(rga_rec$(),173%,6%) = rga_qty_dte$ 
                                             /* Set Time of Last Update   */
              str(rga_rec$(),239%,4%) = rga_status_tme$
                                             /* Set Userid with Change    */
              str(rga_rec$(),243,3%)  = userid$
                                             /* Set last Tracking Seq No. */
              str(rga_rec$(),249%,2%) = rga_status_seq$
                                             /* Set Date of last Change   */
              str(rga_rec$(),251%,6%) = date

           write #1, using RGA_1, rga_rec$(), eod goto RGA_ER6      

           init(" ") rga_dt_key$
           str(rga_dt_key$,1%,8%) = rga_serial$
           str(rga_dt_key$,9%,2%) = rga_status_seq$
           read #2,hold,key 0% = rga_dt_key$, using RGA_3, rga_dt_rec$,    ~
                                                          eod goto RGA_4
RGA_3:        FMT CH(128)

               delete #2

               put #2, using RGA_3, rga_dt_rec$

               goto RGA_5                  /* Finished-Preserve Exisiting*/

RGA_4: 

        str(rga_dt_rec$,1%,6%)     = date
        str(rga_dt_rec$,7%,8%)     = rga_serial$
        str(rga_dt_rec$,15%,2%)    = rga_status_seq$
        str(rga_dt_rec$,17%,3%)    = rga_status$
        str(rga_dt_rec$,20%,3%)    = rga_reason_cd$
        str(rga_dt_rec$,23%,3%)    = userid$
        str(rga_dt_rec$,26%,4)     = rga_status_tme$
        str(rga_dt_rec$,30%,6%)    = rga_trailer_loc$
        str(rga_dt_rec$,36%,32%)   = rga_comment$
        str(rga_dt_rec$,68%,61%)   = "                        "
 

        put #2, using RGA_3,             /* (Detail File )             */~
            rga_dt_rec$                  /* Date Associated W Status   */
RGA_5:
            write #2, eod goto RGA_ER7

        return

        ok_scrn
        REM *************************************************************~
            * Display This Screen If Barcode is Scanned And No          *~
            * Errors Occur.                                             *~
            *************************************************************

            init(" ") hdr$, errormsg$ 
            hdr$ = "RGA Serial Number Scanned: XXXXXXXX     "

            str(hdr$,28%,8%)  = rga_serial$
            print at(03,02);hex(84);errormsg$;
            print at(03,17);hex(84);hdr$;
            print at(07,17);hex(84);ps$(1%);
            print at(08,17);hex(84);ps$(2%);
            print at(09,17);hex(84);ps$(3%);
            print at(10,17);hex(84);ps$(4%);
            print at(11,17);hex(84);ps$(5%);
            print at(12,17);hex(84);ps$(6%);
            print at(13,17);hex(84);ps$(7%);
            for i% = 1% to b_max%
                print at(13,75);bell;
            next i%
            CALL "PAUSE" ADDR(100%)
        return

        err_scrn                    /* Display this Message for Errors */
            errormsg$ = err$(err%)
            print at(03,02);hex(84);"                  ";
            print at(03,17);hex(84);her$(err%);
            print at(07,17);hex(84);ee$(1%);
            print at(08,17);hex(84);ee$(2%);
            print at(09,17);hex(84);ee$(3%);
            print at(10,17);hex(84);ee$(4%);
            print at(11,17);hex(84);ee$(5%);
            print at(12,17);hex(84);ee$(6%);
            print at(13,17);hex(84);ee$(7%);
            for i% = 1% to b_max%
                print at(13,75);bell;
            next i%
            CALL "PAUSE" ADDR(100%)
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

        startover
            u3% = 2%
            call "STARTOVR" (u3%)
            if u3% = 1% then return
            errormsg$ = " "
            edit% = 0%
        return clear all
        goto main

        Run_Program:
           return% = 0% : comp% = 0%
           init(" ") rlib$, rvol$
           call "PROCLINK" (run$, rlib$, rvol$, return%, comp%)
        return

        exit_program
            call "SHOSTAT" ("One Moment Please")
        end

        display_header
            serial_display$ = "** R G A   D I S P L A Y **"

            gosub unpack_header

            title$  = "*** Current RGA Header Data Display ****"
            prompt$ = "Press <RETURN> to Continue?             "
                                                       
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), title$                 , ch(40),~
                                                                         ~
               at (04,02), "**Serial No(Assigned):",                     ~
               at (04,25), fac(hex(84)), barcode$               , ch(08),~
               at (04,40), fac(hex(84)), rga_return_dte$        , ch(10),~
                                                                         ~
               at (04,60), "Customer:",                                  ~
               at (04,70), fac(hex(84)), rga_cust$              , ch(09),~
                                                                         ~  
                at (05,02), "**Warranty Id.       :",                    ~
               at (05,25), fac(hex(84)), rga_warranty_id$       , ch(08),~
                                                                         ~  
               at (06,02), "**Production Barcode :",                     ~
               at (06,25), fac(hex(84)), rga_so_barcode$        , ch(18),~
                                                                         ~
               at (07,02), "**Complaint Number   :",                     ~
               at (07,25), fac(hex(84)), rga_comp_no$           , ch(08),~
                                                                         ~
               at (08,02), "RGA Number/Line Item :",                     ~
               at (08,25), fac(hex(84)), rga_number$            , ch(04),~
               at (08,35), fac(hex(84)), rga_number_ln$         , ch(02),~
                                                                         ~
               at (09,02), "Sales Order/Line Item:",                     ~
               at (09,25), fac(hex(84)), rga_sales_ord$         , ch(08),~
               at (09,35), fac(hex(84)), rga_sales_ln$          , ch(03),~
               at (09,40), fac(hex(84)), rga_orig_date$         , ch(10),~ 
                                                                         ~
               at (10,02), "RGA Status Code      :",                     ~
               at (10,25), fac(hex(84)), rga_status1$           , ch(03),~
               at (10,40), fac(hex(84)), rga_status1_d$         , ch(30),~ 
                                                                         ~
               at (11,02), "RGA Reason Code      :",                     ~
               at (11,25), fac(hex(84)), rga_reason1_cd$        , ch(03),~
               at (11,40), fac(hex(84)), rga_reason1_cd_d$      , ch(30),~ 
                                                                         ~
               at (12,02), "MFG Part Number:",                           ~
               at (12,20), fac(hex(84)), rga_part$              , ch(25),~
               at (12,50), fac(hex(84)), rga_descr$             , ch(30),~ 
                                                                         ~
               at (13,02), "Trailer/Location     :",                     ~
               at (13,25), fac(hex(84)), rga_trailer1_loc$      , ch(06),~
               at (13,40), fac(hex(84)), rga_trailer1_loc_d$    , ch(30),~
                                                                         ~
               at (14,02), "Production Department:",                     ~
               at (14,25), fac(hex(84)), rga_dept$              , ch(03),~
               at (14,40), fac(hex(84)), rga_dept_d$            , ch(30),~ 
                                                                         ~
               at (15,02), "Flag for Quality Chk :",                     ~
               at (15,25), fac(hex(84)), rga_qty_chk$           , ch(01),~
               at (15,40), fac(hex(84)), rga_qty_dte$           , ch(10),~ 
                                                                         ~
               at (16,02), "Comment              :",                     ~
               at (16,25), fac(hex(84)), rga_comment1$          , ch(30),~
                                                                         ~
               at (17,02), "Complaint Code       :",                     ~
               at (17,25), fac(hex(84)), rga_comp_reas$         , ch(03),~
               at (17,35), fac(hex(84)), rga_comp_reas_d$       , ch(30),~ 
                                                                         ~
               at (18,02), "New Complaint Code   :",                     ~
               at (18,25), fac(hex(84)), rga_comp_code$         , ch(07),~
               at (18,35), fac(hex(84)), rga_comp_code_d$       , ch(30),~ 
                                                                         ~
               at (19,02), "Date of Last Change  :",                     ~
               at (19,25), fac(hex(84)), rga_stat_dte$          , ch(10),~
               at (19,40), fac(hex(84)), rga_return_tme$        , ch(05),~
               at (19,50), fac(hex(84)), rga_init_usr$          , ch(03),~  
                                                                         ~
                                                                         ~
               at (21,02), fac(hex(a4)),   prompt$              , ch(79),~
               at (23,27), fac(hex(94)),   serial_display$      , ch(27),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)
              

               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               return
        return

        unpack_header
            unpack% = 99%
            init(" ") rga_return_dte$, rga_warranty_id$, rga_so_barcode$,              ~
                      rga_comp_no$, rga_number$, rga_sales_ord$, rga_sales_ln$,        ~
                      rga_orig_date$, rga_status1$, rga_status1_d$, rga_reason1_cd$,   ~
                      rga_reason1_cd_d$, rga_part$, rga_descr$, rga_trailer1_loc$,     ~
                      rga_trailer1_loc_d$, rga_dept$, rga_dept_d$, rga_qty_chk$,       ~
                      rga_qty_dte$, rga_comment1$, rga_comp_reas$, rga_comp_reas_d$,   ~
                      rga_comp_code$, rga_comp_code_d$, rga_stat_dte$, rga_return_tme$,~
                      rga_init_usr$, rga_number_ln$, rga_cust$, rga_sub_part$
                                                                /* (PAR000)      */
  
                                                    /* original Entry Date       */
            str(rga_return_dte$,1%,6%) = str(rga_rec$(),1%,6%)
            call "DATFMTC" (rga_return_dte$)
                                                    /* Warranty Barcode          */
            rga_warranty_id$ = str(rga_rec$(),18%,8%)
                                                    /* Production Barcode        */
            rga_so_barcode$  = str(rga_rec$(),26%,18%)
                                                    /* Complaint Number          */
            str(rga_comp_no$,1%,4%) = str(rga_rec$(),44%,5%)
            gosub unpack_comp_number                
                                                    /* RGA Number Assigned       */
            rga_number$      = str(rga_rec$(),49%,4%)
                                                    /* RGA Line Item No.         */
            rga_number_ln$   = str(rga_rec$(),53%,2%)
                                                    /* Sales Order               */
            rga_sales_ord$   = str(rga_rec$(),55%,8%)
                                                    /* S.O. Line Item            */
            rga_sales_ln$    = str(rga_rec$(),63%,3)
                                                    /* Orginal Order Date        */
            str(rga_orig_date$,1%,6%) = str(rga_rec$(),233%,6%)
            call "DATFMTC" (rga_orig_date$)         
                                                    /* Last Status Code assigned */
            init(" ") code$
            rga_status1$     =  str(rga_rec$(),7%,3%)
            str(code$,1%,3%) = rga_status1$    
            gosub lookup_rga_status
            rga_status1_d$ = descr$
                                                    /* Last Reason Code Assigned */
            init(" ") code$
            rga_reason1_cd$  =  str(rga_rec$(),137%,3%)
            str(code$,1%,3%) = rga_reason1_cd$    
            gosub lookup_rga_reason
            rga_reason1_cd_d$ = descr$
                                                    /* MFG Part Number           */
                                                    /* (PAR000)                  */ 
            rga_part$        = str(rga_rec$(),66%,25%)
            rga_sub_part$    = str(rga_rec$(),266%,20%)
                                                    /* MFG Part Description      */
            rga_descr$       = str(rga_rec$(),91%,30%)
                                                    /* Trailer Number            */
            init(" ") code$
            rga_trailer1_loc$     =  str(rga_rec$(),131%,6%)
            str(code$,1%,6%) = rga_trailer1_loc$    
            gosub lookup_trailer_loc
            rga_trailer1_loc_d$ = descr$
                                                     /* Production Department    */
            init(" ") code$
            rga_dept$        = str(rga_rec$(),246%,3%)
            str(code$,1%,3%) = rga_dept$
            gosub lookup_dept
            rga_dept_d$       = descr$
                                                     /* Quality Check  Y-N       */          
            rga_qty_chk$      = str(rga_rec$(),172%,1%)
            str(rga_qty_dte$,1%,6%) = str(rga_rec$(),173%,6%)
            call "DATFMTC" (rga_qty_dte$)
                                                     /* Last Comment Assigned    */
            rga_comment1$     = str(rga_rec$(),140%,32%)
                                                     /* Complaint Code           */
            init(" ") code$
            rga_comp_reas$    = str(rga_rec$(),128%,3%)
            str(code$,1%,3%)  = rga_comp_reas$
            gosub lookup_complaint
            rga_comp_reas_d$  = descr$
                                                     /* New Complaint Code       */
            rga_comp_code$    = str(rga_rec$(),121%,7%)
            rga_comp_code_d$  = "                "

                                                     /* Date of last Change      */
            str(rga_stat_dte$,1%,6%) = str(rga_rec$(),251%,6%)
            call "DATFMTC" (rga_stat_dte$)
                                                     /* Time of Last Change      */
            rga_return_tme$   = str(rga_rec$(),239,2%) & ":" & str(rga_rec$(),241%,2%) 
                                                     /* User who made last Change*/
            rga_init_usr$     = str(rga_rec$(),243%,3%)
                                                     /* Sales Order Csutomer No. */
            rga_cust$         = str(rga_rec$(),257%,9%) 
        return

        unpack_comp_number
            rga_comp_no% = 0%
        
            get str(rga_comp_no$,1%,4%), using PACK_2, rga_comp_no%
PACK_2:             FMT BI(4)
            convert rga_comp_no% to rga_comp_no$, pic(00000000)

        return 

        lookup_dept         
            init(" ") readkey$, descr$
            str(readkey$,1%,9%)  = "PLAN DEPT"
            str(readkey$,10%,3%) = str(code$,1%,3%) 
            read #5,key = readkey$, using RGA_2, descr$, eod goto lookup_dept_err
       return
lookup_dept_err:
            descr$ = "(Error) - Header Department Lookup?? "
       return

       lookup_complaint                         /* Complaint Code For Complaint */ 
            init(" ") readkey$, descr$
            str(readkey$,1%,9%)  = "COMPLAINT"
            str(readkey$,10%,3%) = str(code$,1%,3%) 
            read #5,key = readkey$, using RGA_2, descr$,                         ~
                                    eod goto lookup_complaint_done
        return
lookup_complaint_done
        return

        display_codes
            call "APCPLN1B" (tab%, #5)
        return
 
