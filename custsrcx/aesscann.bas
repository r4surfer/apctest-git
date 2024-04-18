        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AESSCANN                             *~
            *  Creation Date     - 09/20/04                             *~
            *  Last Modified Date- 06/15/05                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *  Last Mod By       - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Scanning Utility used by AES and     *~
            *                      AWD for Inventory.                   *~
            *                                                           *~
            *  Special Notes     - (AES) Store 500  Selection (1)       *~
            *                                                           *~
            *                            From the Scanning screen AES   *~
            *                            can run "AWDPLN07" and         *~
            *                            print Rack Labels.             *~
            *                                                           *~
            *                      (AWD) Store 100  Selection (2)       *~
            *                                                           *~
            *                                                           *~
            *  Code Tables Used  - (AESAWDUSR) User Id and Selection    *~
            *                         (1) = AES Add Inventory to (500)  *~
            *                         (2) = AWD Move Inventory to (100) *~
            *                                                           *~
            *                                                           *~
            *  Special Comments  - Table controls seltion allowed.      *~
            *                      User only has acctes to one selection*~
            *                                                           *~
            *                      (RHHTEST) All Items Flaggged for     *~
            *                      testing that are 'TURNED OFF'        *~
            *                                                           *~
            *              Note  - 'USERLCMS' located in /CMS/SESDBASE/ *~
            *                                                           *~
            *                      Our Copy is in /CMS4/APCDATA/        *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/07/05 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 06/15/05 ! (AWD001) Put test for AES Qty Max        ! RHH *~
            * 07/06/06 ! (AWD002) Add void for AES                ! DES *~
            * 11/02/06 ! (AWD003) No quantity change on scann     ! DES *~
            *************************************************************

        dim hdr$47, scr$(15%)40,         /* ASKUSER Header             */~
            readkey$50, desc$30,         /* GENCODES Lookup            */~
            filename$8,                  /* Use with EWDOPEN - EWD016  */~
            her$(30%)50,                 /* Error Text Display         */~
            scr_sel$1, scr_sel_d$31,     /* Screen Scanning Option     */~
            scr_id$3, msg$(3%)79, hh$40, /* Scanning User Id           */~
            scr_id_d$30,                 /* User Id Name               */~
            scr_qty$10,                  /* Screen Rack Quantity       */~
            scr_store$3,                 /* Scanning Store Number      */~ 
            pfkeys$40,                   /* PF KEYS                    */~
            xx$(7%)50,                   /* Screen Display area Text   */~
            gl$(7%)50,                   /* 'Glass' Message            */~
            st$(7%)50,                   /* 'Stage' Message Test       */~
            ps$(7%)50,                   /* Scan 'COMP'lete Text Screen*/~
            ee$(7%)50,                   /* 'STOP' Message Error Text  */~
            err$(30%)50, er$2,           /* Defined Error Messages     */~
            barcode$8,                   /* Scanned Barcode            */~
            cursor%(2%),                 /* Cursor location for edit   */~
            date$8,                      /* Date for screen display    */~
            dateout$8,                   /* Time Extract For Screen    */~
            inp_text$(2%)79,             /* Input Prompt Text          */~
            fld$(4%)30,                  /* Field Text                 */~
            errormsg$79,                 /* Error message              */~
            i$(24%)80,                   /* Screen Image               */~
            inpmessage$79,               /* Informational Message      */~
            lfac$(4%)1,                  /* Field Attribute Characters */~
            pf$(3%)79,                   /* PF Screen Literals         */~
            scrn_title$40,               /* Screen Description         */~
            wandchar$1,                  /* Wand Character - Scanner   */~
            userid$3                     /* Current User Id            */

         dim                             /* (AESPRDLB) AES Label Data  */~
            aes_key$30,                  /* Primary Key                */~
            aes_key1$8,                  /* Alt Key 1                  */~
            aes_key2$25,                 /* Alt Key 2                  */~
            aes_key3$31,                 /* Alt Key 3                  */~
            aes_key4$14,                 /* Alt Key 4                  */~
            aes_rec$256,                 /* Label Record               */~
            aes_serial$8, aes_descr$32,  /* Label Serial Number        */~
            aes_vendor$9,                /* Vendor Code                */~
            aes_po$16,                   /* P.O. Number                */~
            aes_item$3,                  /* P.O. Line Item             */~
            aes_qty$10,                  /* Actual Rack Quantity       */~
            aes_scan_usr$3,              /* AES Scan User Id           */~
            aes_awd_usr$3,               /* AWD Scan User Id           */~
            aes_part$25,                 /* Raw Material Number        */~
            aes_delivery$6,              /* Product Delivery Date Debug*/~
            aes_deliv$10,                /* Delivery Formatted    Debug*/~
            fr_store$3,                  /* From Store   (500)         */~
            to_store$3,                  /* To Store     (100)         */~
            rcvhnyds_key$86,             /* RCVHNYDS key 1             */~
            rcvtif2_key$69,              /* RCVTIF2  key 1             */~
            rcvtif2_tmp$69,              /* RCVTIF2  key 1             */~
            rcvhnyds_alt$42,             /* RCVHNYDS alt key 1         */~
            rcvlines_key$69,             /* RCVLINES key               */~
            vbklines_key$28,             /* VBKLINES key               */~
            hnyquan_key$44,              /* HNYQUAN  key               */~
            rcvtif_key$69,               /* RCVTIF key                 */~
            rcv_onhand,                  /* RCVHNYDS on hand           */~
            rcv_quan,                    /* RCVHNYDS quantity to buy   */~
            xref_rec$256,                /* RCVXREF records            */~
            calc_time$8,                 /* Scan Time                  */~
            calc_t$4                     /* Set Time 'HHMM'            */                 
        dim gltext$100, text$40, hny_acct$9, hnydate$6,                 ~
	    lot$6, part$25, store$3, glstore$3, tran_type$5
        dim gl_post_info$(2)255, gltype$2,                               ~
            export_on$1              

        dim rlib$8,                      /* Run library for link call  */~
            rvol$6,                      /* Run volume for link call   */~
            run$8                        /* Program to Run             */

        dim f2%(50%),                    /* = 0 if the file is open    */~
            fs%(50%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(50%)20                 /* Text from file opening     */

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
            * #1  ! SYSFILE2 ! SYSTEM INFORMATION FILE (INVENTORY DATA) *~
            * #2  ! STORNAME ! STORE MASTER FILE                        *~
            * #3  ! USERINFO ! USER INFORMATION FILE                    *~
            * #4  ! HNYMASTR ! INVENTORY MASTER FILE                    *~
            * #5  ! GENCODES ! GENERAL CODES MASTER FILE                *~
            * #6  ! HNYDETAL ! INVENTORY TRANSACTION DETAIL             *~
            * #7  ! HNYQUAN  ! INVENTORY QUANTITY INFO FILE             *~
            * #8  ! SERTIF   ! ADDITIONS BUFFER FOR INVENTORY S/N'S     *~
            * #9  ! SERWORK  ! TEMPORARY SERIAL #'S WORK FILE Notpassed *~
            * #10 ! SERMASTR ! SERIAL NUMBER TRACKING MASTER            *~
            * #11 ! HNYPOOL  ! INVENTORY POOL FILES                     *~
            * #12 ! PIPMASTR ! PLANNED INV. POSITION MASTER             *~      
            * #13 ! HNYADJPF ! INVENTORY ADJUSTMENT PRINT JOURNAL       *~      
            * #14 ! HNYLOCNS ! LOCATION QUANTITY DETAIL FILE            *~
            * #15 ! LOCATION ! LOCATION MASTER  FILE                    *~
            * #16 ! GLMAIN   ! GENERAL LEDGER MAIN FILE                 *~
            * #17 ! GLDETAIL ! GENERAL LEDGER DETAIL FILE               *~
            * #18 ! SFCUM2   ! CUMULATIVE SALES FORCAST FILE            *~
            * #19 ! GLMASTR  ! GENERAL LEDGER MASTER FILE ???????       *~
            * #20 ! HNYADDTF ! ADDITIONS BUFFER FOR INVENTORY           *~
            * #21 ! VENDOR   ! VENDOR MASTER                            *~
            * #22 ! JOBMASTR ! WIP/JC JOB MASTER FILE                   *~
            * #23 ! USERLCMS ! Caelus Master User Def. (USERLCMS)       *~
            * #24 !          !                                          *~
            * #25 ! RCVSCN   ! Receiver Master TIF                      *~
            * #26 ! RCVSCN2  ! Receiver Line Item TIF                   *~ 
            * #32 ! RCVHNYDS !                                          *~ 
            * #41 ! RCVJRNTF !                                 AWD002   *~ 
            * #42 ! RCVXREF  !                                 AWD002   *~ 
            * #43 ! RCVLINES !                                 AWD002   *~ 
            * #44 ! VBKLINES !                                 AWD002   *~ 
            * #45 ! RCVTIF2  !                                 AWD002   *~ 
            * #49 ! AESPRDLB ! AES MASTER LABEL FILE                    *~      
            * #50 ! WRKFILE  ! HNYPST2 WORKFILE                AWD002   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "SYSFILE2",                                      ~
                        varc,     indexed,  recsize =  500,              ~
                        keypos =  1,   keylen = 20

            select #2,  "STORNAME",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos = 1,    keylen = 3

            select #3,  "USERINFO",                                      ~
                        varc,     indexed,  recsize = 150,               ~
                        keypos =  1,   keylen = 3

            select #4,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =  1,   keylen = 25,                      ~
                        alt key 1, keypos = 102, keylen =  9, dup,       ~
                            key 2, keypos = 90,  keylen =  4, dup

            select #5,  "GENCODES",                                      ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =  1,   keylen = 24

            select  #6, "HNYDETAL",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize =  150,                                 ~
                         keypos = 1, keylen = 42,                        ~
                         alternate key 1, keypos = 43, keylen = 6, dup,  ~
                                   key 2, keypos = 49, keylen = 2, dup

            select #7,  "HNYQUAN",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 650,                                  ~
                         keypos= 17, keylen = 44,                        ~
                         alternate key 1, keypos =  1, keylen = 44

            select #8,  "SERTIF",                                        ~
                        varc, indexed,  recsize =  100,                  ~
                        keypos = 1, keylen = 62


            select #10, "SERMASTR",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   52, keylen =  45,                     ~
                        alt key  1, keypos =   32, keylen =  45,         ~
                            key  2, keypos =    1, keylen =  76

            select #11,  "HNYPOOL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 38

            select #12,  "PIPMASTR",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 2024,                                 ~
                         keypos = 2, keylen = 25,                        ~
                         alternate key 1, keypos = 1, keylen = 26

            select #13,  "HNYADJPF",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 160,                                  ~
                         keypos = 1, keylen = 19   

            select #14,  "HNYLOCNS",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 700,                                  ~
                         keypos = 1, keylen = 42,                        ~
                         alternate key 1, keypos = 443, keylen = 42,     ~
                                   key 2, keypos = 485, keylen = 42,     ~
                                   key 3, keypos = 527, keylen = 42,     ~
                                   key 4, keypos = 590, keylen = 42 
            select #15,  "LOCATION",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 400,                                  ~
                         keypos = 1, keylen = 11,                        ~
                         alternate key 1, keypos = 4, keylen = 11

            select #16, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  300,                                  ~
                        keypos =    1, keylen =   9                      ~

            select #17, "GLDETAIL",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  160,                                  ~
                        keypos =    1, keylen =  26

            select #18, "SFCUM2",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 1985,                                  ~
                        keypos =    1, keylen =  25

            select #20, "HNYADDTF",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1, keylen = 6

            select #21, "VENDOR",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos = 1, keylen = 9,                          ~
                        alt key  1, keypos = 10, keylen = 30, dup

            select #22, "JOBMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 700,                                   ~
                        keypos = 1, keylen = 8
                                          /* Note 'USERLCMS' located in */
                                          /*       /CMS/SESDBASE/       */
                                          /* Our Copy in /CMS4/APCDATA/ */
            select #23,  "USERLCMS",                                     ~
                        varc,     indexed,  recsize =  600,              ~
                        keypos =    1, keylen =   3,                     ~
                        alt key  1, keypos =   4, keylen =  30, dup

            select #25,  "RCVSCN",                                       ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 150,                                  ~
                         keypos = 12, keylen = 16,                       ~
                         alternate key 1, keypos = 1, keylen = 11   

            select #26,  "RCVSCN2",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 800,                                  ~
                         keypos = 1, keylen = 28

            select #32, "RCVHNYDS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 200,                                   ~
                        keypos= 1, keylen = 86,                          ~
                        alt key 1, keypos = 45, keylen = 42              ~

            select #41, "RCVJRNTF",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 670,                                  ~
                         keypos = 10, keylen = 10                        ~

            select #42, "RCVXREF",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 256,                                   ~
                        keypos = 1, keylen = 28,                         ~
                        alt key 1, keypos = 29, keylen = 8,              ~
                            key 2, keypos = 37, keylen = 42, dup,        ~
                            key 3, keypos = 79, keylen = 16, dup
 
            select #43, "RCVLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 800,                                   ~
                        keypos= 26, keylen = 52,                         ~
                        alt key 1, keypos =  1, keylen = 69,             ~
                            key 2, keypos = 42, keylen = 36,             ~
                            key 3, keypos =128, keylen = 24              ~

            select #44, "VBKLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  700,                                  ~
                        keypos  =    1, keylen =  28

            select #45, "RCVTIF2",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 800,                                   ~
                        keypos= 26, keylen = 52,                         ~
                        alt key 1, keypos =  1, keylen = 69,             ~
                            key 2, keypos = 42, keylen = 36,             ~
                            key 3, keypos =128, keylen = 24              ~

            select #49, "AESPRDLB",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 256,                                   ~
                        keypos = 1, keylen = 30,                         ~
                        alt key 1, keypos = 23,  keylen =  8,            ~
                            key 2, keypos = 37,  keylen = 25,  dup,      ~
                            key 3, keypos = 31,  keylen = 31,  dup,      ~
                            key 4, keypos = 238, keylen = 14,  dup
 
            select #50,  "WORKFILE",                                     ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 160,                                  ~
                         keypos = 10, keylen = 19                        ~

            call "SHOSTAT" ("Opening Files, One moment Please?")
                                                         /* (EWD0016)   */
            filename$ = "SYSFILE2" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "STORNAME" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "USERINFO" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "HNYMASTR" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#5, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "HNYDETAL" : call "EWDOPEN" (#6, filename$, err%)
REM            call "OPENCHCK" (#8, fs%(8%), f2%(8%),500%, rslt$(8%))
            filename$ = "HNYQUAN"  : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "SERTIF"   : call "EWDOPEN" (#8, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "SERMASTR" : call "EWDOPEN" (#10, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "HNYPOOL"  : call "EWDOPEN" (#11, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "PIPMASTR" : call "EWDOPEN" (#12, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "HNYADJPF" : call "EWDOPEN" (#13, filename$, err%)
            if err% <> 0% then gosub open_error                      
            filename$ = "HNYLOCNS" : call "EWDOPEN" (#14, filename$, err%)
            if err% <> 0% then gosub open_error                      
            filename$ = "LOCATION" : call "EWDOPEN" (#15, filename$, err%)
            if err% <> 0% then gosub open_error                      
            filename$ = "GLMAIN"   : call "EWDOPEN" (#16, filename$, err%)
            if err% <> 0% then gosub open_error                      
            filename$ = "GLDETAIL" : call "EWDOPEN" (#17, filename$, err%)
            if err% <> 0% then gosub open_error                      
            filename$ = "SFCUM2"   : call "EWDOPEN" (#18, filename$, err%)
            if err% <> 0% then gosub open_error                      
            filename$ = "HNYADDTF" : call "EWDOPEN" (#20, filename$, err%)
            if err% <> 0% then gosub open_error                      
            filename$ = "VENDOR"   : call "EWDOPEN" (#21, filename$, err%)
            if err% <> 0% then gosub open_error                      
        REM    filename$ = "JOBMASTR"  call "EWDOPEN" (#22, filename$, err%)
        REM    if err% <> 0% then gosub open_error                      
            call "OPENCHCK" (#22, fs%(22%), f2%(22%),500%, rslt$(22%))

            filename$ = "USERLCMS" : call "EWDOPEN" (#23, filename$, err%)
            if err% <> 0% then gosub open_error                      

            call "OPENCHCK" (#25, fs%(25%), f2%(25%),500%, rslt$(25%))
            call "OPENCHCK" (#26, fs%(26%), f2%(26%),500%, rslt$(26%))

            call "OPENCHCK" (#32, fs%(32%), f2%(32%),500%, rslt$(32%))
            call "OPENCHCK" (#41, fs%(41%), f2%(41%),500%, rslt$(41%))
            call "OPENCHCK" (#42, fs%(42%), f2%(42%),500%, rslt$(42%))
            call "OPENCHCK" (#43, fs%(43%), f2%(43%),500%, rslt$(43%))
            call "OPENCHCK" (#44, fs%(44%), f2%(44%),500%, rslt$(44%))
            call "OPENCHCK" (#45, fs%(45%), f2%(45%),500%, rslt$(45%))

            filename$ = "AESPRDLB" : call "EWDOPEN" (#49, filename$, err%)
            if err% <> 0% then gosub open_error                      
                                                         
            call "WORKOPEN" (#50, "IO   ", 100%, f2%(50))

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
 
            debug% = 0%
            dim apc$40, pname$21
            apc$   = "*(AES/AWD) Inventory Scanning Utility**"
            pname$ = "AESSCANN - 12/03/2004"

            scr$(1%) = "****************************************"
            scr$(2%) = "*       SCANNING SELECTION CODES       *"
            scr$(3%) = "* ------------------------------------ *"
            scr$(4%) = "*     Inventory Scanning Selections    *"
            scr$(5%) = "* (1) - AES ADD To Inventory,Store -500*"
            scr$(6%) = "* (2) - AWD MOVE To Inventory,Store-100*"
            scr$(7%) = "* (3) - AES VOID From Inv.   Store -500*"
            scr$(8%) = "*                                      *"
            scr$(9%) = "*                                      *"
            scr$(10%)= "*                                      *"
            scr$(11%)= "*                                      *"
            scr$(12%)= "****************************************"

            b_max% = 10%                     /* SET NUMBER OF TIMES TO */
                                             /* RING BELL ON SCREEN    */
            call "EXTRACT" addr("ID", userid$)
            date$ = date
            call "DATEFMT" (date$)
                                               /* Screen Default Msg   */
            gl$(1%) = "           AAAAA    EEEEEEE    SSSSS "
            gl$(2%) = "          A     A   E         S     S"
            gl$(3%) = "          A     A   E           S    "
            gl$(4%) = "          AAAAAAA   EEEEE         S  "
            gl$(5%) = "          A     A   E               S"
            gl$(6%) = "          A     A   E         S     S"
            gl$(7%) = "          A     A   EEEEEEE    SSSSS "

            st$(1%) = "           AAAAA    W     W   DDDDDD "
            st$(2%) = "          A     A   W     W   D     D"
            st$(3%) = "          A     A   W     W   D     D"
            st$(4%) = "          AAAAAAA   W  W  W   D     D"
            st$(5%) = "          A     A   W WWW W   D     D"
            st$(6%) = "          A     A   WWW WWW   D     D"
            st$(7%) = "          A     A   WW   WW   DDDDDD "

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


            err$(1%) ="(Error) Rack Serial Number not on File?           "
            err$(2%) ="(Error) Rack Has already been'ADDED'to Inventory? "
            err$(3%) ="(Error) Rack has already been'MOVED'from Inventory"
            err$(4%) ="(Error) Rack has not been 'ADDED' Cannot 'MOVE'?  "
            err$(5%) ="(Error) While Adding Inventory to Store (500)?    "
            err$(6%) ="(Error) While Moving Inventory to Store (100)?    "
            err$(7%) ="(Error) While Reading Label file for Update?      "
            err$(8%) ="(Error) While Writing Label File for Update?      "
            err$(9%) ="(Error) While Converting the Rack Quantity?       "
            err$(10%)="(Error) Serial Number has Been Deleted No Update? "
            err$(11%)="(Error) Quantity Less Than '1.0' is Invalid?      "
            err$(12%)="(Error) Invalid Serial Number Format?             "
            err$(13%)="(Error) Rack Already VOIDED!                      "
            err$(14%)="(Error) Userid NOT Authorised for VOIDING         "

            her$(1%) = " I n v a l i d   R a c k   S e r i a l   N o.  "
            her$(2%) = "   R a c k   A l r e a d y   'A D D E D E D'   "
            her$(3%) = "   R a c k   A l r e a d y   'M O V E D'       "
            her$(4%) = "R a c k   N o t  A D D E D   C a n n o t   MOVE"   
            her$(5%) = "  A d d i n g   I n v e n t o r y   E r r o r  "
            her$(6%) = "  M o v i n g   I n v e n t o r y   E r r o r  "
            her$(7%) = "R e a d i n g  L a b e l   F i l e  U p d a t e"
            her$(8%) = "W r i t i n g  L a b e l   F i l e  U p d a t e"
            her$(9%) = "C o n v e r t i n g   R a c k   Q u a n t i t y"
            her$(10%)= "S e r i a l   N o.   D e l e t e d   No Update "
            her$(11%)= "       I n v a l i d   Q u a n t i t y         "
            her$(12%)= "I n v a l i d   S e r i a l   N o.  F o r m a t"
            her$(13%)= "   R a c k   A l r e a d y   'V O I D E D'     "
            her$(14%)= "U S E R I D   N o t   A u t h o r i s e d      "


        initialize
            edit% = 0%
            init(" ") scr_sel$, scr_sel_d$, scr_id$, scr_id_d$

        main
            gosub mainmenu
            init(" ") errormsg$
            if keyhit% = 16% then exit_program
               gosub check_user
               if code% = 0% then goto initialize
               gosub check_selection
               if code% = 0% then goto initialize
            edit% = 1%
            if keyhit% = 6% then gosub inventory_scan
            if keyhit% = 14% then gosub inventory_scan
         goto main

         mainmenu                                /* Main Scanning Menu */
            gosub set_screen_1
                                                 /* Staging Printer    */
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), apc$                   , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (04,11), "Scanning User Id.     :",                    ~
               at (04,35), fac(lfac$(1%)), scr_id$              , ch(03),~
               at (04,42), fac(hex(84)),scr_id_d$               , ch(30),~
                                                                         ~
               at (05,11), "Scanning Selection    :",                    ~
               at (05,35), fac(hex(84)), scr_sel$               , ch(01),~
               at (05,42), fac(hex(84)),scr_sel_d$              , ch(31),~
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

               if keyhit% <> 1% then L00105
                  gosub startover

L00105:        if keyhit% <> 3% then L00110     /* Print Queue      */
                  run$ = "ILPMAN" 
                  gosub Run_Program
                  goto mainmenu

L00110:        if keyhit% <> 15% then L00120
                  call "PRNTSCRN"
                  goto mainmenu


L00120:        close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_screen_1
            lfac$(1%) = hex(81)
            init(" ") dateout$, scrn_title$
            call "TIME" (dateout$)
            inpmessage$ = "Enter a Valid Scanning, Selection and Userid ~
        ~Required?"
            pf$(1%) = "(1)Startover                            " &       ~
                      "                       (14)Scanning    "
            pf$(2%) = "                                        " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(3)Print Queue                          " &       ~
                      "                       (16)Exit Program"
            pfkeys$ = hex(01ff03ffffffffffffffffff0d0e0f102000)

/*AWD002*/  if scr_id$ <> "VLK" and scr_id$ <> "VKN"  and                ~
/*AWD002*/     scr_id$ <> "DES" and scr_id$ <> "CMG"  then return   
		       
            pf$(1%) = "(1)Startover                    (6) VOID" &       ~
                      "                       (14)Scanning    "
            pfkeys$ = hex(01ff03ffff06ffffffffffff0d0e0f102000)

        return

        check_user
           code% = 0%
           init (" ") scr_id_d$

           read #23,key = scr_id$, using USER_1, scr_id_d$,eod goto USER_2
USER_1:       FMT POS(4), CH(30)
           code% = 1%

        return      
USER_2:
           errormsg$ = "(Error) - Invalid User ID, 'UserId' is Required?"
           gosub error_prompt
           code% = 0%
        return

        check_selection
           code% = 0% 
           init(" ") scr_store$, scr_sel_d$, scrn_title$

           scr_sel% = 0%
           gosub check_table_user
           if keyhit% = 6% then scr_sel$ = "3"
           convert scr_sel$ to scr_sel%, data goto SEL_2
             
REM        if scr_sel% < 1% or scr_sel% > 2% then goto SEL_3
/*AWD002*/ if scr_sel% < 1% or scr_sel% > 3% then goto SEL_3
/*AWD002*/ if scr_sel% = 3% and                                        ~
/*AWD002*/      scr_id$ <> "VLK" and scr_id$ <> "VKN"  and             ~
/*AWD002*/      scr_id$ <> "DES" and scr_id$ <> "CMG"  then goto SEL_2

              scr_sel_d$ = str(scr$(4% + scr_sel%),9%,31%)
              scrn_title$ = scr_sel_d$
              code% = 1%
              if scr_sel% = 1% then scr_store$ = "500"

              if scr_sel% = 2% then scr_store$ = "100"

/*AWD002*/    if scr_sel% = 3% then scr_store$ = "500"

        return      
SEL_2:
            errormsg$ = "(Error) Invalid selection for Userid"
            gosub error_prompt
            code% = 0%
        return
SEL_3:
            errormsg$ = "(Error) Invalid Selection?"
            gosub error_prompt
        return

        check_table_user
            init(" ") readkey$, desc$, scr_sel$
            str(readkey$,1%,9%)  = "AESAWDUSR"
            str(readkey$,10%,15%) = scr_id$  
            read #5,hold,key = readkey$, using CHECK_1, desc$, eod goto CHECK_2
CHECK_1:        FMT POS(25), CH(30)

            scr_sel$ = str(desc$,1%,1%)
CHECK_2:
        return

        REM *************************************************************~
            *       AES and AED Inventory Scanning                      *~
            *************************************************************

        inventory_scan
            err% = 0%
            init(" ") barcode$, wandchar$, xx$(), scr_qty$
            fieldnr% = 1%
            gosub'100(fieldnr%)                      /* Scan Barcode  */

            if keyhit% <> 16% then goto inventory_scan_next
               return clear all
               goto initialize

INV_SCAN_ERR:
               return clear all 
               gosub err_scrn
               goto inventory_scan
                                                     /* Process Data  */
        inventory_scan_next
            init(" ") errormsg$
            gosub check_label_data
            if err% <> 0% then goto INV_SCAN_ERR
            fieldnr% = 2%
            gosub'100(fieldnr%)
            
            gosub check_qty_update
            if err% <> 0% then goto INV_SCAN_ERR 
                                                     /* Scan and Update */
                                                     /* Completed       */
               gosub ok_scrn

        goto inventory_scan

        REM *************************************************************~
            *       END of Main Scanning                                *~
            *************************************************************


        deffn'100(fieldnr%)
SCR_1:
     gosub set_screen_2
            accept                                                       ~
               at (01,02), fac(hex(8c)), pname$                 , ch(21),~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (02,21), fac(hex(a4)), scrn_title$            , ch(40),~
               at (03,02), fac(hex(94)), errormsg$              , ch(79),~
                                                                         ~
               at (05,02), fac(hex(84)), fld$(1%)               , ch(23),~
               at (05,27), fac(lfac$(1%)), barcode$             , ch(08),~
               at (05,50), fac(lfac$(2%)), wandchar$            , ch(01),~
                                                                         ~
               at (06,02), fac(hex(84)), fld$(2%)               , ch(23),~
               at (06,27), fac(lfac$(3%)), scr_qty$             , ch(10),~
                                                                         ~
               at (14,16), fac(hex(84)), xx$(1%)                , ch(50),~
               at (15,16), fac(hex(84)), xx$(2%)                , ch(50),~
               at (16,16), fac(hex(84)), xx$(3%)                , ch(50),~
               at (17,16), fac(hex(84)), xx$(4%)                , ch(50),~
               at (18,16), fac(hex(84)), xx$(5%)                , ch(50),~
               at (19,16), fac(hex(84)), xx$(6%)                , ch(50),~
               at (20,16), fac(hex(84)), xx$(7%)                , ch(50),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 1% then goto SCR_1A
                  gosub startover

SCR_1A:        if keyhit% <> 3% then goto SCR_2     /* Print Queue      */
                  run$ = "ILPMAN" 
                  gosub Run_Program
                  goto SCR_1
                
SCR_2:         if keyhit% <> 10% then goto SCR_2A
                  run$ = "AWDPLN07"
                  gosub Run_Program
                  goto SCR_1

SCR_2A:        if keyhit% <> 14% and keyhit% <> 6% then goto SCR_2B
                  gosub process_receivers
                  goto SCR_1


SCR_2B:        if keyhit% <> 15% then goto SCR_3
                  call "PRNTSCRN"
                  goto SCR_1

SCR_3:         close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
        return

        set_screen_2
            init(" ") scrn_title$
            if scr_sel% = 1% then                                       ~
               scrn_title$ = "Add Material to AES Inventory Store(500)"
            if scr_sel% = 2% then                                       ~
               scrn_title$ = "Move Material from AES To AWD Store(100)" 
/*AWD002*/  if scr_sel% = 3% then                                       ~
               scrn_title$ = "VOID AES Inventory Store(500)"

            if scr_sel% = 1% then copy gl$() to xx$()
            if scr_sel% = 2% then copy st$() to xx$()


            inp_text$(1%)="Scan Barcode or Manually Enter Barcode Number"
            inp_text$(2%)="Either Change Quantity, or Hit <Return> To Continue?"

            init(" ") dateout$
            inpmessage$ = inp_text$(fieldnr%)
            call "TIME" (dateout$)
                                                                      
            fld$(1%)      = "Rack Serial Number   :"
            fld$(2%)      = "Rack Quantity        :"
                                                     
            pf$(1%) = "(1)Startover                            " &       ~
                      "                      (14)Proc Receiver"
            pf$(2%) = "(3)Print Queue                          " &       ~
                      "                       (15)Print Screen"
            pf$(3%) = "(10)Print Rack Labels                   " &       ~
                      "                       (16)Exit Screen "
            pfkeys$ = hex(01ff03ffffffffffff0affffff0e0f1000)


            if scr_sel% = 1% and fieldnr% = 1% then goto SCR_3A
                str(pf$(3%),1%,22%) = " " : str(pfkeys$,10%,1%) = hex(ff)
                str(pf$(1%),60%)    = " " : str(pfkeys$,14%,1%) = hex(ff)
              
SCR_3A:     if fieldnr% <> 1% then goto SCR_4
               init(" ") barcode$, wandchar$
               lfac$(1%) = hex(81) : lfac$(2%) = hex(99)
               lfac$(3%) = hex(84) 
        return
SCR_4:      lfac$(1%) = hex(84) : lfac$(2%) = hex(84)
            lfac$(3%) = hex(81) 
	       /* AWD003 */
            if scrrsel% = 1% then lfac$(3%) = hex(84) 
            if scrrsel% = 2% then lfac$(3%) = hex(84) 
	       /* AWD003 */
              
        return

        check_label_data
           init(" ") aes_key$, aes_key1$, aes_key2$, aes_key3$,         ~  
                     aes_rec$, aes_serial$, aes_qty$, aes_scan_usr$,    ~
                     aes_awd_usr$, aes_part$, fr_store$, to_store$,     ~
                     aes_descr$, aes_vendor$, aes_po$, aes_item$

           aes_qty = 0.0 
           aes_serial$ = barcode$              /* Set Serial Number */

           aes_serial% = 0%                    /* For Hand Entry    */
           convert aes_serial$ to aes_serial%, data goto AES_ER12

           convert aes_serial% to aes_serial$, pic(00000000)

           barcode$ = aes_serial$              /* Insure Format     */

/*AWD002*/ read #42,key 1% = aes_serial$, using L19550,            ~  
                             xref_rec$, eod goto AES_0
           if str(xref_rec$,98,1) <> "V" then goto AES_0 
           err% = 3%                    /* Already Moved From Inventory */ 
           return 

AES_0:
           aes_key1$   = aes_serial$  
           read #49,key 1% = aes_key1$, using AES_1, aes_rec$,         ~
                                                          eod goto AES_ER1
AES_1:         FMT CH(256)
                                             /* P.O. Number         */
               aes_po$ = str(aes_rec$,1%,16%)
                                             /* P.O. Line Item      */
               aes_item$ = str(aes_rec$,17%,3%)
                                             /* Vendor Code         */
               aes_vendor$ = str(aes_rec$,94%,9%)
 
                                             /* Printed Label Quantity*/ 
               get str(aes_rec$,103%,8%), using AES_2, aes_qty
AES_2:         FMT PD(15,4)

                                             /* Get Current used Qty */
               get str(aes_rec$,119%,8%), using AES_2, aes_used_qty

                                             /* Get Quantity Scanned */
                                             /* into Store (500)     */
               if scr_sel% = 2% then                       ~
                  get str(aes_rec$,192%,8%), using AES_2, aes_qty 

               convert aes_qty to aes_qty$, pic(####.####-)
 
                                             /* Display Current Label*/
                                             /* or Last Scanned Qty  */
               scr_qty$ = aes_qty$ 
             
               aes_scan_usr$ = str(aes_rec$,200%,3%)  /* AES Userid */

               aes_awd_usr$  = str(aes_rec$,221%,3%)  /* AWD Userid */

               if str(aes_rec$,94%,6%) = "DELETE" then goto AES_ER10

               if scr_sel% = 1% and aes_scan_usr$ <> "???" then        ~
                                                    goto AES_ER2

               if scr_sel% = 2% and aes_awd_usr$ <> "???"  then        ~
                                                    goto AES_ER3
               if scr_sel% = 2% and aes_scan_usr$ = "???"  then        ~
                                                     goto AES_ER4

               aes_part$ = str(aes_rec$,37%,25%)
               fr_store$ = "500"
               to_store$ = "100"

        return
AES_ER1:
        err% = 1%                    /* Serial Number Not on File    */
        return
AES_ER2:
        err% = 2%                    /* Already added to Inventory   */
        return
AES_ER3:
        err% = 3%                    /* Already Moved From Inventory */ 
        return
AES_ER4:
        err% = 4%                    /* Not Added cannot Move From   */ 
        return

        check_qty_update
                                     /* Check for Valid Qty           */
                                     /* Stop Scanning of Two Barcodes */ 
            p% = pos(scr_qty$ = ".")
            if p% = 0% then goto AES_ER9 

            convert scr_qty$ to scr_qty, data goto AES_ER9
 
            if scr_qty < 1.0 then goto AES_ER11
                                     /* (AWD001)                     */
            if scr_qty > 1500.0 then goto AES_ER11
 
                                     /* Reformat Quantity            */  
            convert scr_qty to scr_qty$, pic(####.####-)
     
/*AWD002*/  if scr_sel% = 3% then goto AES_2A

            if aes_qty = scr_qty then goto AES_2A

                                     /* Back Out Previous Quantity   */
               aes_used_qty = aes_used_qty - aes_qty

                                     /* Add in New Used Quantity     */
               aes_used_qty = aes_used_qty + scr_qty
                                     /* Set new Label Quantity Scanned */
               aes_qty = scr_qty  

               aes_qty$ = scr_qty$
AES_2A:
    
                                     /* Add Rack Quantity to (500)   */
            if scr_sel% = 1% then gosub inventory_add
                                     /* Move Rack Quantity to (100)  */
            if scr_sel% = 2% then gosub inventory_move
                                     /* Subtract Rack Quantity from (500) */
            if scr_sel% = 3% then gosub inventory_sub

            if err% = 5% then goto AES_ER5
           
            if err% = 6% then goto AES_ER6
                                     /* No Errors Update Label       */

               gosub update_aesprdlb

        return
AES_ER5:
        err% = 5%                        /* Inventory Add Error      */   
        return
AES_ER6:
        err% = 6%                        /* Inventory Move Error     */
        return
AES_ER7:
        err% = 7%                        /* Read Update Error        */                         
        return
AES_ER8:
        err% = 8%                        /* Write Update Error       */
        return
AES_ER9:
        err% = 9%                        /* Quantity Convert Error   */
        return
AES_ER10:
        err% = 10%                       /* Serial Number Deleted    */
        return
AES_ER11:
        err% = 11%                       /* Invalid Rack Quantity    */
        return
AES_ER12:
        err% = 12%                       /* Invalid Serial Number    */
        return

        update_aesprdlb
                                         /* (RHHTEST)                */
           debug% = 0%                   /* 0% = Off, 1% = On        */

           aes_key1$   = aes_serial$  
           read #49,hold, key 1% = aes_key1$, using AES_1, aes_rec$,    ~
                                                        eod goto AES_ER7
                                            /* (RHHTEST)             */ 
           delete #49

           init(" ") calc_time$, calc_t$
           calc_time$ = time
           str(calc_t$,1%,2%) = str(calc_time$,1%,2%)  /* Set Hour   */
           str(calc_t$,3%,2%) = str(calc_time$,3%,2%)  /* Set Min    */

        if scr_sel% = 2% then goto AES_3
                                            /* Into Inventory        */ 
                                            /* Date Scanned by AES   */   
           str(aes_rec$,182%,6%) = date
                                            /* Time Scanned by AES   */  
           str(aes_rec$,188%,4%) = calc_t$
                                            /* User Id of Scanner AES*/
           str(aes_rec$,200%,3%) = scr_id$
                                            /* Quantity Scanned   AES*/
           put str(aes_rec$,192%,8%), using AES_2, aes_qty

           str(aes_rec$,239%,1%) = "1"      /* Scanned by AES        */

           goto AES_4
AES_3:
                                            /* Moved From Into Inv   */ 
                                            /* Date Scanned by AWD   */   
           str(aes_rec$,203%,6%) = date
                                            /* Time Scanned by AWD   */  
           str(aes_rec$,209%,4%) = calc_t$
                                            /* User Id of Scanner AWD*/
           str(aes_rec$,221,3%) = scr_id$
                                            /* Quantity Scanned   AWD*/
           put str(aes_rec$,213%,8%), using AES_2, aes_qty
AES_4:
                                            /* Current Quantity Used */
           put str(aes_rec$,119%,8%), using AES_2, aes_used_qty

                                            /* (RHHTEST)             */
        
           write #49, using AES_1, aes_rec$, eod goto AES_ER8
                                           
           gosub debug_screen_display       /* (RHHTEST)             */
        return
     
        inventory_add
                                            /* (RHHTEST)             */
           call "SHOSTAT" ("Adding To Inventory ---> (500)" )
           CALL "PAUSE" ADDR(50%)
        REM   stop
        REM   return

           err% = 0%

            call "AESRCVSN" (aes_vendor$,          /* Vendor Code        */ ~
                             aes_po$,              /* P.O. Number        */ ~
                             aes_item$,            /* Purchase Order Line*/ ~
                             aes_qty,             /* Aes Scan Rack Qty  */ ~
                             aes_serial$,          /* Serial Number      */ ~
                             #25,                  /* (RCVSCN)           */ ~
                             #26,                  /* (RCVSCN2)          */ ~
                             err% )                /* Error Code         */
   
        REM call "AWDADDNS" (aes_part$,            /* Raw Material Part  */ ~
        REM                 fr_store$,             /* From Store (500)   */ ~
        REM                 aes_qty$,              /* Rack Quantity      */ ~
        REM                 aes_descr$,            /* Rack Serial Number */ ~
        REM                 scr_id$,               /* Scan User Id       */ ~
        REM                 err%,                  /* Error Code 0%=Ok   */ ~
        REM                 #1,                    /* SYSFILE2           */ ~
        REM                 #2,                    /* STORENAME          */ ~
        REM                 #3,                    /* USERINFO           */ ~
        REM                 #4,                    /* HNYMASTR           */ ~
        REM                 #5,                    /* GENCODES           */ ~
        REM                 #6,                    /* HNYDETAL           */ ~ 
        REM                 #7,                    /* HNYQUAN            */ ~
        REM                 #8,                    /* SERTIF             */ ~
        REM                 #10,                   /* SERMASTR           */ ~
        REM                 #11,                   /* HNYPOOL            */ ~
        REM                 #12,                   /* PIPMASTR           */ ~
        REM                 #13,                   /* HNYADJPF           */ ~
        REM                 #14,                   /* HNYLOCNS           */ ~
        REM                 #15,                   /* LOCATIONS          */ ~
        REM                 #16,                   /* GLMAIN             */ ~
        REM                 #17,                   /* GLDETAL            */ ~
        REM                 #18,                   /* SFCUM2             */ ~
        REM                 #20,                   /* HNYADDTF           */ ~
        REM                 #21,                   /* VENDOR             */ ~
        REM                 #22 )                  /* JOBMASTR           */

           if err% <> 0% then err% = 5%         /* Inventory Add Error */

        return

        inventory_sub
                                            /* (RHHTEST)             */
           call "SHOSTAT" ("VOID ---> (500)" )
           CALL "PAUSE" ADDR(50%)

           err% = 0%
           del_qty = aes_qty 

            call "AESVOID" (aes_vendor$,          /* Vendor Code        */ ~
                            aes_po$,              /* P.O. Number        */ ~
                            aes_item$,            /* Purchase Order Line*/ ~
                            aes_qty,             /* Aes Scan Rack Qty  */ ~
                            aes_serial$,          /* Serial Number      */ ~
                            #25,                  /* (RCVSCN)           */ ~
                            #26,                  /* (RCVSCN2)          */ ~
                            err% )                /* Error Code         */
   
            /* update receiver now */  

            read #42,key 1% = aes_serial$, using L19550,            ~  
                              xref_rec$, eod goto L19568   
            if str(xref_rec$,98,1) = "V" then goto L19568

            read #42,key 1% = aes_serial$, hold, using L19550,            ~  
                              xref_rec$, eod goto L19560   
            /* set to "voided" */
            str(xref_rec$,98,1) = "V"
            rewrite #42, using L19550, xref_rec$

L19550:     FMT CH(256)
            init(hex(00)) rcvhnyds_key$, rcvhnyds_alt$
	    /* read with primary key less date/time as date/time is not */
	    /* accurate and check to see if the part is == after read > */
            str(rcvhnyds_key$,01,16) = str(xref_rec$,79,16) /* Receiver */
            str(rcvhnyds_key$,17,09) = str(xref_rec$,17,09) /* Vendor   */
            str(rcvhnyds_key$,26,16) = str(xref_rec$,01,16) /* PO       */
            str(rcvhnyds_key$,42,03) = str(xref_rec$,26,03) /* Item     */
            str(rcvhnyds_key$,45,25) = str(xref_rec$,37,25) /* Part     */
            str(rcvhnyds_key$,70,03) = str(xref_rec$,62,03) /* Store    */
            str(rcvhnyds_key$,73,06) = str(xref_rec$,65,06) /* Lot      */
            read #32,key > rcvhnyds_key$,using L19552, rcvhnyds_alt$,    ~
                                         eod goto L19552
L19552:     FMT POS(45), CH(42)

            if str(rcvhnyds_key$,45,34) <> str(rcvhnyds_alt$,1,34)       ~
                                         then goto L19560
            read #32,key 1% = rcvhnyds_alt$, hold, using L19554,             ~
                     rcv_onhand, rcv_quan, eod goto L19560   
            rcv_onhand = rcv_onhand - del_qty
            rcv_quan   = rcv_quan   + del_qty
            rewrite #32, using L19554, rcv_onhand, rcv_quan,                 ~
                                            eod goto L19554   
L19554:     FMT POS(87), PD(15,4), POS(173), PD(15,4)

REM-------- subtract from received quantity in rcvlines -------

            init(" ") rcvlines_key$
            str(rcvlines_key$,1,25)  = str(xref_rec$,37,25) /* PART */
	    str(rcvlines_key$,26,16) = str(xref_rec$,79,16) /* RECEIVER */
	    str(rcvlines_key$,42,9)  = str(xref_rec$,17,09) /* VENCODE */
	    str(rcvlines_key$,51,16) = str(xref_rec$,01,16) /* PO */
	    str(rcvlines_key$,67,3)  = str(xref_rec$,26,03) /* VBKLSEQ# */
            read #43,key 1% = rcvlines_key$, hold, using L19555,             ~
                     rcv_qtyrecv, rcv_tohold,                               ~
                      rcv_rcv_hold, rcv_buy, eod goto L19560   
REM         rcv_qtyrecv  = rcv_qtyrecv - del_qty
            rcv_qtyrecv  = rcv_qtyrecv - del_qty
            rcv_tohold   = rcv_tohold  - del_qty
REM         rcv_rcv_hold = rcv_rcv_hold  - del_qty  /* new */
            rcv_buy      = rcv_buy     + del_qty    /* new */
            rewrite #43, using L19555, rcv_qtyrecv, rcv_tohold, ~
                      rcv_rcv_hold, rcv_buy, eod goto L19560   
L19555:     FMT POS(156), PD(15,4), POS(204), PD(15,4),                  ~
                POS(164), PD(15,4), POS(356), PD(15,4)

	    str(vbklines_key$,01,09) = str(xref_rec$,17,09) /* VENCODE */
	    str(vbklines_key$,10,16) = str(xref_rec$,01,16) /* PO */
	    str(vbklines_key$,26,03)  = str(xref_rec$,26,03) /* VBKLSEQ# */
            read #44,key = vbklines_key$, hold, using L19556,             ~
                     vbk_prior, vbk_open, vbk_rec, eod goto L19556   
L19556:   FMT POS(101), PD(15,4), POS(109), PD(15,4), POS(397), PD(15,4)

/*new*/     vbk_prior   = vbk_prior   - del_qty
/*new*/     vbk_prior   = vbk_prior   - del_qty
/*new2*/    vbk_rec     = vbk_rec     - del_qty
            vbk_open    = vbk_open    + del_qty
/*new*/     vbk_open    = vbk_open    + del_qty
            rewrite #44, using L19556, vbk_prior, vbk_open, vbk_rec,  ~
                                            eod goto L19557   
            /* update inventory now */  
	    init(" ") hnyquan_key$
            str(hnyquan_key$,1,34) = str(xref_rec$,37,34) 
            read #7,key = hnyquan_key$, hold, using L19557,             ~
                     hny_onhand, hny_order, eod goto L19557   
L19557:   FMT POS(69), PD(15,4), POS(85), PD(15,4)
REM         hny_onhand  = hny_onhand  - del_qty
REM         hny_onhand  = hny_onhand  - del_qty
            hny_order   = hny_order   + del_qty
            hny_order   = hny_order   + del_qty

            rewrite #7, using L19557, hny_onhand, hny_order,             ~
                                            eod goto L19560   
           aes_key1$ = aes_serial$
           read #49,hold, key 1% = aes_key1$, using AES_1, aes_rec$,    ~
                                            eod goto L19560   
           if str(aes_rec$,23,8) <> aes_serial$ then goto L19560
           /* close by setting awd & scan user ids */
           str(aes_rec$,221,3) =  userid$
	   if str(aes_rec$,200,3) = "???" then str(aes_rec$,200,3) = userid$
           rewrite #49, using AES_1, aes_rec$

L19560:     

REM L19565:   FMT POS(156), PD(15,4), POS(204), PD(15,4)
	   init(" ") rcvtif2_key$
           str(rcvtif2_key$,1,25)  = str(xref_rec$,37,25) /* PART */
	   str(rcvtif2_key$,26,16) = str(xref_rec$,79,16) /* RECEIVER */
	   str(rcvtif2_key$,42,9)  = str(xref_rec$,17,09) /* VENCODE */
	   str(rcvtif2_key$,51,16) = str(xref_rec$,01,16) /* PO */
	   str(rcvtif2_key$,67,3)  = str(xref_rec$,26,03) /* VBKLSEQ# */
	   aes_qty = 0.0
           if err% <> 0% then err% = 14%         /* Inventory Sub Error */
           read #45,key 1% = rcvtif2_key$, hold, using L19566,            ~  
                             rcvtif2_tmp$, eod goto L19567       
L19566:    FMT CH(69)
           if rcvtif2_key$ <> rcvtif2_tmp$ then return
	   delete #45

L19567: 
L19568: return

        inventory_move
                                                /* (RHHTEST)           */
           call "SHOSTAT" ("Moving To Inventory ---> (100)" )
           CALL "PAUSE" ADDR(50%)
        REM   stop
        REM   return

           init(" ") aes_descr$
           str(aes_descr$,1%,8%) = aes_serial$          
           err% = 0%
        call "AWDMOVE"  (aes_part$,             /* Raw Material Part  */ ~
                         aes_qty$,              /* Rack Quantity      */ ~
                         fr_store$,             /* From Store (500)   */ ~
                         to_store$,             /* To Store   (100)   */ ~ 
                         aes_descr$,            /* Rack Serial Number */ ~
                         scr_id$,               /* Scan User Id       */ ~
                         err%,                  /* Error Code 0%=Ok   */ ~
                         #1,                    /* SYSFILE2           */ ~
                         #2,                    /* STORENAME          */ ~
                         #3,                    /* USERINFO           */ ~
                         #4,                    /* HNYMASTR           */ ~
                         #5,                    /* GENCODES           */ ~
                         #6,                    /* HNYDETAL           */ ~ 
                         #7,                    /* HNYQUAN            */ ~
                         #8,                    /* SERTIF             */ ~
                         #10,                   /* SERMASTR           */ ~
                         #11,                   /* HNYPOOL            */ ~
                         #12,                   /* PIPMASTR           */ ~
                         #13,                   /* HNYADJPF           */ ~
                         #14,                   /* HNYLOCNS           */ ~
                         #15,                   /* LOCATION           */ ~
                         #16,                   /* GLMAIN             */ ~
                         #17,                   /* GLDETAL            */ ~
                         #18,                   /* SFCUM2             */ ~
                         #20,                   /* HNYADDTF           */ ~
                         #21,                   /* VENDOR             */ ~
                         #22 )                  /* JOBMASTR           */
           
           if err% <> 0% then err% = 6%         /* Inventory Move Error */
        return

        ok_scrn
        REM *************************************************************~
            * Display This Screen If Barcode is Scanned And No          *~
            * Errors Occur.                                             *~
            *************************************************************

            init(" ") hdr$, errormsg$
            hdr$ = "Mat: XXXXXXXXXXXXXX Rack Qty: XXXXXXXXXX"

            str(hdr$,6%,14%)  = str(aes_part$,1%,14%)
            str(hdr$,31%,10%) = aes_qty$
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
        goto initialize

        Run_Program:
           return% = 0% : comp% = 0%
           init(" ") rlib$, rvol$
           call "PROCLINK" (run$, rlib$, rvol$, return%, comp%)
        return

        process_receivers

           run$ = "AESUPDTE"
           gosub Run_Program

         return
                                         /* Receiver Flag is being */
                                         /* updated by 'AESUPDTE'  */
PROC_NXT:
           init(" ") aes_key4$, aes_rec$
           str(aes_key4$,1%,1%) = "1"

           read #49,key 4% > aes_key4$, using AES_1, aes_rec$, eod goto PROC_DONE
           if str(aes_rec$,239%,1%) <> "1" then goto PROC_DONE

           if str(aes_rec$,94%,6%) = "DELETE" then goto PROC_NXT        

           gosub update_rack         
   
           goto PROC_NXT 
PROC_DONE:
        return

        update_rack
           init(" ") calc_time$, calc_t$, aes_key1$
           calc_time$ = time
           str(calc_t$,1%,2%) = str(calc_time$,1%,2%)  /* Set Hour   */
           str(calc_t$,3%,2%) = str(calc_time$,3%,2%)  /* Set Min    */

           aes_key1$ = str(aes_rec$,23%,8%)
           read #49,hold,key 1% = aes_key1$, using AES_1, aes_rec$,   ~
                                                  eod goto update_error
              delete #49
            
           str(aes_rec$,239%,1%) = "2"
           str(aes_rec$,240%,6%) = date
           str(aes_rec$,246%,4%) = calc_t$
           str(aes_rec$,250%,3%) = scr_id$

           write #49, using AES_1, aes_rec$, eod goto update_error

        return

        update_error
           errormsg$ = "(ERROR) Updating Seq. No. " & str(aes_rec$,23%,8%)
           gosub error_prompt
        return

        exit_program
            call "SHOSTAT" ("One Moment Please")
        end

        debug_screen_display                       /* (RHHTEST)               */
            if debug% = 0% then return             /* Debug Turned Off        */

               convert err% to er$, pic(##)


               init(" ") aes_delivery$, aes_deliv$
               aes_delivery$ = date     

               aes_deliv$    = aes_delivery$

               call "DATFMTC" (aes_deliv$)

            gosub set_keys

            accept                                                       ~
               at (01,02), "AES/AWD Scanning Display Screen",            ~
               at (01,66), "Today:",                                     ~
               at (01,73), fac(hex(8c)), date$                  , ch(08),~
               at (03,02), "Scanning Selection:",                        ~
               at (03,25), fac(hex(84)), scr_sel$               , ch(03),~
               at (03,40), fac(hex(84)), scr_sel_d$             , ch(32),~   
                                                                         ~
               at (04,02), "Scanning User Id  :",                        ~
               at (04,25), fac(hex(84)), scr_id$                , ch(03),~
               at (04,40), fac(hex(84)), scr_id_d$              , ch(32),~   
                                                                         ~
               at (05,02), "Serial Number     :",                        ~
               at (05,25), fac(hex(84)), aes_serial$            , ch(08),~
                                                                         ~
               at (06,02), "Current Order Qty :",                        ~
               at (06,25), fac(hex(84)), scr_qty$               , ch(10),~
                                                                         ~
               at (07,02), "From Store        :",                        ~
               at (07,25), fac(hex(84)), fr_store$              , ch(03),~
                                                                         ~
               at (08,02), "To Store          :",                        ~
               at (08,25), fac(hex(84)), to_store$              , ch(03),~
                                                                         ~
               at (09,02), "Raw Material Part :",                        ~
               at (09,25), fac(hex(84)), aes_part$              , ch(25),~
                                                                         ~
               at (11,02), "Update Order Qty  :",                        ~
               at (11,25), fac(hex(84)), aes_qty$               , ch(10),~
                                                                         ~                                                                         
               at (12,02), "Scann Date      :",                          ~
               at (12,25), fac(hex(84)), aes_deliv$             , ch(10),~
                                                                         ~                                                                         
               at (13,02), "Scann Time      :",                          ~
               at (13,25), fac(hex(84)), calc_t$                , ch(04),~
                                                                         ~                                                                         
               at (14,02), "Scann User      :",                          ~
               at (14,25), fac(hex(84)), scr_id$                , ch(03),~
                                                                         ~
               at (15,02), "Error Return    :",                          ~
               at (15,25), fac(hex(84)), er$                    , Ch(02),~
                                                                         ~
               at (21,02), fac(hex(a4)),   inpmessage$          , ch(79),~
               at (22,02), fac(hex(8c)),   pf$(1%)              , ch(79),~
               at (23,02), fac(hex(8c)),   pf$(2%)              , ch(79),~
               at (24,02), fac(hex(8c)),   pf$(3%)              , ch(79),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)


               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())

        REM    stop                                    /* (RHHTEST)   */
            return

        set_keys
            pfkeys$ = hex(01ffff04ffffffffffffffff0dff0f1000)
        return



