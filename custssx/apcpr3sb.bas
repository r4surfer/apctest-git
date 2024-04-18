        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPR3SB                             *~
            *  Creation Date     - 12/06/93                             *~
            *  Last Modified Date- 11/01/99                             *~
            *  Description       - This Subroutine Builds.              *~
            *                      (1) Display Screen for Data Entry.   *~
            *                      (2) Display for Report Print out     *~
            *                      (3) The Output Format for Screen and *~
            *                          Report.                          *~
            *                                                           *~
            *  Special Comments  - Note Uses Subroutine (APCPR1SB) for  *~
            *                      Table Lookups.                       *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 01/04/94 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 04/18/97 ! Mod to Support Alpha Table Code Values   ! RHH *~
            *          !   used in Price Calc Definition.         !     *~
            *          !   (at Lines - 2680 thru 3030 )           !     *~
            * 10/31/97 ! Check for release Upgrade to R6.04.03    ! RHH *~
            *          !                                          !     *~
            * 05/18/97 ! Changed to allow alpha-numeric calc code ! ERN *~
            * 11/01/99 ! (EWD001) Mod to Support new Wood Jamb    ! RHH *~
            *          !    tables. PRICE 029 thru PRICE 032      !     *~
            * 09/25/14 ! (EWD002) Allow "00" code lookup for      ! PWW *~
            *          !          price value field #2.           !     *~
            *************************************************************

          sub "APCPR3SB" (pc_r$,        /* Std/Spc Ref Code  -PRICE 002*/~
                          pc_rc$,       /* Ref. Calc Code    -PRICE 003*/~
                          pc_kdesc$(),  /* Field Description Code      */~
                          kd$(),        /* Key Text Display (3) - 20   */~
                          pc_kfld$(),   /* Key Field Def (3) - 3   - 3 */~
                          pc_kfld_d$(), /* Key Field Def Descript  -10 */~
                          pc_kfld%(),   /* Key Field Def (3)       - 1 */~
                          pc_kbeg%(),   /* Key Beginning Point     - 1 */~
                          pc_klen%(),   /* Key Length for Field (3)- 1 */~
                          pc_vtbl$,     /* Value Field Code        - 2 */~
                          pc_vtbl_d$,   /* Valuse Field Code Desc  -10 */~
                          pc_vtln%,     /* Value Table Code Length -   */~
                          pc_vdesc$,    /* Value Field Desc Code   - 3 */~
                          pc_vdesc_d$,  /* Value Field Descript -   20 */~
                          pc_vtblv$(),  /* Value Field Table Values- 3 */~
                          vd$(),        /* Value Descriptions      -30 */~
                          pc_vfmt$,     /* Value Format Tab Code   - 3 */~
                          pc_vcalc$,    /* Value Calc Code         - 2 */~
                          pc_vcalc_d$,  /* Value Calc Desc         -12 */~
                          pc_kcalc$,    /* Key Calc Code           - 2 */~
                          pc_kcalc_d$,  /* Key Calc Desc           -14 */~
                          pc_kunt$,     /* Key Unit Convert Code   - 2 */~
                          pc_kunt_d$,   /* Key Unit Convert Desc   - 6 */~
                          #1,           /* (APCPCMSK) - File           */~
                          #2,           /* (GENCODES) - File           */~
                          err% )        /* Error Non Zero Value        */
        dim                              /* (APCPCMST) Price Calc Def. */~
            pc_r$2, pc_r_d$32,           /* Std/Spc Reference Code     */~
            pc_rc$3, pc_rc_d$32,         /* Price Ref. Calc Method     */~
            pc_kdesc$(3)3, kd$(3)20,     /* Field Description Codes    */~
            pc_kfld$(3)3,pc_kfld_d$(3)10,/* Field Description Codes    */~
            pc_kfld%(3),                 /* Field Definition Codes     */~
            pc_kbeg%(3),                 /* Start Position in Part No. */~
            pc_klen%(3),                 /* Field Length in Part No.   */~
            pc_vtbl$2, pc_vtbl_d$10,     /* Field Definition Code-Table*/~
            pc_vtblv$(6)3, vd$(6)30,     /* Field Table Values         */~
            pc_vdesc$3, pc_vdesc_d$20,   /* Value Description Codes    */~
            pc_vfmt$2, pc_vfmt_d$10,     /* Value Field Format Code    */~
            pc_vcalc$2,pc_vcalc_d$12,    /* Value Calculation Code     */~
            pc_kcalc$2,pc_kcalc_d$14,    /* Key   Calculation Code     */~
            pc_kunt$2, pc_kunt_d$6,      /* Key Unit Conversion Code   */~
            pc_key$5,                    /* Primary Key (APCPCMSK)     */~
            pc_kfil$16,                  /* Definition Filler Area     */~
            pc_link$3,                   /* From (PRICE 003)           */~
            tab_val$5, tab_desc$32       /* Table Key and Description  */

        REM - Main Line Routine
            pc_r_d$, pc_rc_d$, pc_link$, kd$() = " "
            pc_kfld$(), pc_kfld_d$(), pc_vtbl$, pc_vtbl_d$ = " "
            pc_vdesc$, pc_vdesc_d$, pc_vtblv$(), vd$() = " "
            pc_vfmt$, pc_vfmt_d$, pc_vcalc$ = " "
            pc_vcalc_d$, pc_kcalc$, pc_kcalc_d$, pc_kunt$  = " "
            pc_kunt_d$, pc_key$, pc_kdesc$() = " "

            mat pc_kfld% = zer
            mat pc_kbeg% = zer
            mat pc_klen% = zer
            pc_vtln% = 0%

            gosub read_format

            goto exit_program

        REM - End Of Main Line

        REM *************************************************************~
            *                   L O A D   D A T A                       *~
            *************************************************************

        read_format
            err% = 1%
            pc_key$ = all(hex(00))
            str(pc_key$,1%,2%) = pc_r$
            str(pc_key$,3%,3%) = pc_rc$
            read #1,key = pc_key$, eod goto L01300
               get #1, using  L01140  , pc_r$, pc_rc$, pc_kdesc$(1%),      ~
                                    pc_kdesc$(2%), pc_kdesc$(3%),        ~
                                    pc_kfld%(1%), pc_kfld%(2%),          ~
                                    pc_kfld%(3%), pc_kbeg%(1%),          ~
                                    pc_kbeg%(2%), pc_kbeg%(3%),          ~
                                    pc_klen%(1%), pc_klen%(2%),          ~
                                    pc_klen%(3%), pc_vtbl$, pc_vtln%,    ~
                                    pc_vdesc$,                           ~
                                    pc_vtblv$(1%), pc_vtblv$(2%),        ~
                                    pc_vtblv$(3%), pc_vtblv$(4%),        ~
                                    pc_vtblv$(5%), pc_vtblv$(6%),        ~
                                    pc_vfmt$, pc_vcalc$, pc_kcalc$,      ~
                                    pc_kunt$, pc_kfil$
        err% = 0%
        gosub data_convert

L01140: FMT                     /* (APCPCMSK) (Key) Definition File    */~
            CH(02),             /* Std/Spc Reference Code (PRICE 002)  */~
            CH(03),             /* Std/Spc Ref. Calc Cde- (PRICE 003)  */~
            3*CH(03),           /* Key Field Descript   - (PRICE 004)  */~
            3*BI(1),            /* Key Field Def. Code  - (PRICE 005)  */~
            3*BI(1),            /* Key Field Start Pos  - Pos(11,2)    */~
            3*BI(1),            /* Key Field Length     - Pos(14,2)    */~
            CH(02),             /* Value Field Table      (PRICE 005)  */~
            BI(1),              /* Length of Table Code                */~
            CH(03),             /* Value Field Descripti  (PRICE 004)  */~
            6*CH(03),           /* Table Code Values - From Table      */~
            CH(02),             /* Value Format Code      (PRICE 006)  */~
            CH(02),             /* Value Calc Method      (PRICE 007)  */~
            CH(02),             /* Key Calc Method        (PRICE 008)  */~
            CH(02),             /* Key Unit Conversion    (PRICE 009)  */~
            CH(09)              /* Filler Area                         */
L01300: return

        REM *************************************************************~
            *       C O N V E R T   D A T A   F O R   S C R E E N       *~
            *-----------------------------------------------------------*~
            * Loads data from File Record Area into Program Variables.  *~
            *************************************************************
        data_convert
            for i% = 1% to 3%
                convert pc_kfld%(i%) to pc_kfld$(i%), pic(00)

            next i%
            gosub L01650                         /* Price Ref Code    */
            gosub L01770                         /* Price Ref Calc Cde*/
            gosub L01910                         /* Key (1) Descript  */
            gosub L02110                         /* Key (1) Field Def */
            gosub L01920                         /* Key (2) Descript  */
            gosub L02120                         /* Key (2) Field Def */
            gosub L01930                         /* Key (3) Descript  */
            gosub L02130                         /* Key (3) Field Def */
            gosub L02370                         /* Value Field Table */
            gosub L02540                         /* Value Field Descr */
            gosub L02690                         /* Value Code (1)    */
            gosub L02700                         /* Value Code (2)    */
            gosub L02710                         /* Value Code (3)    */
            gosub L02720                         /* Value Code (4)    */
            gosub L02730                         /* Value Code (5)    */
            gosub L02740                         /* Value Code (6)    */
            gosub L03050                         /* Value Format Code */
            gosub L03190                         /* Value Calc. Code  */
            gosub L03330                         /* Key Calc Method   */
            gosub L03470                         /* Key Unit Convert  */

        return

L01650: REM Price Reference Code                  PC_R$
           convert pc_r$ to zz%, data goto L01670
L01670:
           convert zz% to pc_r$, pic(00)

           tab_1% = 2% : tab_2% = 0%          /* (PRICE 002) Ref Codes */
           tab_val$ = pc_r$
           call "APCPR1SB" (tab_1%, tab_2%, tab_val$,tab_desc$, p%, #2,  ~
                                                                tab_rec% )
           pc_r_d$ = tab_desc$
        return

L01770: REM Price Reference Calc Codes            PC_RC$
           convert pc_rc$ to zz%, data goto L01790
       
           convert zz% to pc_rc$, pic(000)
L01790:
           tab_1% = 3% : tab_2% = 0%     /* (PRICE 003) Ref Calc Codes */
           tab_val$ = pc_r$ & pc_rc$
           call "APCPR1SB" (tab_1%, tab_2%, tab_val$,tab_desc$, p%, #2,  ~
                                                                tab_rec% )
           pc_rc_d$ = tab_desc$
           pc_link$ = str(tab_desc$,22%,3%)
        return

        REM Key Descriptions ( 1 thru 3 )         PC_KDESC$()
L01910:    x% = 1% : goto L01940                     /* Table does not   */
L01920:    x% = 2% : goto L01940                     /* Alpha Values     */
L01930:    x% = 3%
L01940:    if pc_kdesc$(x%) <> " " then goto L01960
L01950:       pc_kdesc$(x%) = "000"
L01960:    if x% < 2% then goto L01990
              if pc_kdesc$(x%-1%) = "000" then pc_kdesc$(x%) = "000"

L01990:    convert pc_kdesc$(x%) to zz%, data goto L01950

           convert zz% to pc_kdesc$(x%), pic(000)
           if zz% = 0% then return
           tab_1% = 4% : tab_2% = 0%   /* (PRICE 004) - Field Descript */
           tab_val$ = pc_kdesc$(x%)
           call "APCPR1SB" (tab_1%, tab_2%, tab_val$,tab_desc$, p%, #2,  ~
                                                                tab_rec% )
           kd$(x%) = str(tab_desc$,1%,20%)
        return

        REM Key Field Definitions ( 1 thru 3 )    PC_KFLD$()
L02110:    x% = 1% : goto L02140                     /* Table does not   */
L02120:    x% = 2% : goto L02140                     /* have Alpha values*/
L02130:    x% = 3%
L02140:    if pc_kfld$(x%) <> " " then goto L02160
L02150:       pc_kfld$(x%) = "00"
L02160:    if pc_kdesc$(x%) = "000" then pc_kfld$(x%) = "00"
           convert pc_kfld$(x%) to zz%, data goto L02150

           convert zz% to pc_kfld$(x%), pic(00)

           tab_1% = 5% : tab_2% = 0% /* (PRICE 005) - Field Def. Codes */
           tab_val$ = pc_kfld$(x%)
           call "APCPR1SB" (tab_1%, tab_2%, tab_val$,tab_desc$, p%, #2,  ~
                                                                tab_rec% )
           pc_kfld_d$(x%) = str(tab_desc$,1%,9%) & " "
           convert tab_val$ to pc_kfld%(x%), data goto L02270
L02270:
           if pc_kfld%(x%) <> 0% then goto L02310
L02290:       pc_kbeg%(x%), pc_klen%(x%) = 0%
              return
L02310:    convert str(tab_desc$,11%,2%) to pc_kbeg%(x%), data goto L02290

           convert str(tab_desc$,14%,2%) to pc_klen%(x%), data goto L02290

        return

L02370: REM Value Field Table Code                PC_VTBL$
           if pc_vtbl$ <> " " then goto L02400       /* Table does not   */
L02390:       pc_vtbl$ = "00"                      /* have Alpha Values*/
L02400:    convert pc_vtbl$ to zz%, data goto L02390

           convert zz% to pc_vtbl$, pic(00)

           tab_1% = 5% : tab_2% = 0% /* (PRICE 005) - Field Def. Codes */
           tab_val$ = pc_vtbl$
           call "APCPR1SB" (tab_1%, tab_2%, tab_val$,tab_desc$, p%, #2,  ~
                                                                tab_rec% )
           pc_vtbl_d$ = str(tab_desc$,1%,9%) & " "
           pc_vtln% = 0%
           convert str(tab_desc$,14%,2%) to pc_vtln%,data goto L02510
L02510:
        return

L02540: REM Value of Field Description Code       PC_VDESC$
           if pc_vdesc$ <> " " then goto L02570      /* Table doe not    */
L02560:       pc_vdesc$ = "000"                    /* have Alpha Values*/
L02570:    convert pc_vdesc$ to zz%, data goto L02560

           convert zz% to pc_vdesc$, pic(000)

           tab_1% = 4% : tab_2% = 0%   /* (PRICE 004) - Field Descript. */
           tab_val$ = pc_vdesc$
           call "APCPR1SB" (tab_1%, tab_2%, tab_val$,tab_desc$, p%, #2,  ~
                                                                tab_rec% )
           pc_vdesc_d$ = str(tab_desc$,1%,20%) & " "
        return

        REM Value Field Table Value               PC_VTBLV$()
L02690:    x% = 1% : goto L02750                /* Valid Tables MODEL,    */
L02700:    x% = 2% : goto L02750                /*  COLOR, GLASS, LITING  */
L02710:    x% = 3% : goto L02750                /*  HINGE, SCREEN, LOCKS  */
L02720:    x% = 4% : goto L02750                /* (EWD001) - PRICE 029   */
L02730:    x% = 5% : goto L02750                /*    thru PRICE 032      */ 
L02740:    x% = 6%
L02750:    convert pc_vtbl$ to tab_2%, data goto L02760
L02760:                                       /* Check for Valid Table */
           if tab_2% > 0% and tab_2% < 8% then goto L02830
                                                /* (EWD001) Wood Jamb  */
           if tab_2% > 20% and tab_2% < 29% then goto L02830
                                                /* (EWD001) Wood Jamb  */           
L02780:       pc_vtblv$(x%)   = "000"         /* Not Alpplicable       */
              vd$(x%) = " "
              if x% = 1% and pc_vdesc$ <> "000" then                     ~
                                       vd$(x%) = pc_vdesc_d$
              return
L02830:    zz% = 0%
           convert pc_vtblv$(x%) to zz%, data goto L02950
                                              /* Table Containes Alpha */

               if x% <> 2% then goto N02950               /*EWD002*/      
                  if pc_vtblv$(x%) = "000" then goto L02780  /*EWD002*/
                  if pc_vtblv$(x%) <> "00" then goto N02950   /*EWD002*/
                  convert zz% to pc_vtblv$(x%), pic(00)      /*EWD002*/
                  goto L02950                                /*EWD002*/
                  
        REM IF ZZ% =  0% THEN GOTO 2750       /* Mod - 04/18/97        */
N02950:       if zz% =  0% then goto L02780
              if pc_vtln% = 3% then                                      ~
                 convert zz% to pc_vtblv$(x%), pic(000)
              if pc_vtln% = 2% then                                      ~
                 convert zz% to pc_vtblv$(x%), pic(00)
              if pc_vtln% = 1% then                                      ~
                 convert zz% to pc_vtblv$(x%), pic(0)

L02950:    tab_val$ = pc_vtblv$(x%)           /* (EWD001)              */
           call "APCPR1SB" (tab_1%, tab_2%, tab_val$,tab_desc$, p%, #2,  ~
                                                                tab_rec% )
           if tab_rec% = 0% then goto L02780     /* Could Not Find Value */
              str(vd$(x%),1%,20%)  = pc_vdesc_d$ /* in Specified Table */
              str(vd$(x%),20%,1%)  = " "
              str(vd$(x%),21%,10%) = str(tab_desc$,p%+1%,10%)
              str(vd$(x%),30%,1%)  = ":"
        return

L03050: REM Value Field Format Codes              PC_VFMT$
           if pc_vfmt$ <> " " then goto L03080       /* Table does not   */
L03070:       pc_vfmt$ = "01"                      /* contain Alpha Val*/
L03080:    convert pc_vfmt$ to zz%, data goto L03070

           convert zz% to pc_vfmt$, pic(00)

           tab_1% = 6% : tab_2% = 0%
           tab_val$ = pc_vfmt$
           call "APCPR1SB" (tab_1%, tab_2%, tab_val$,tab_desc$, p%, #2,  ~
                                                                tab_rec% )
           pc_vfmt_d$ = str(tab_desc$,1%, p%)
        return

L03190: REM Value Calculation Method              PC_VCALC$
           if pc_vcalc$ <> " " then goto L03220      /* Table doe not    */
L03210:       pc_vcalc$ = "00"                     /* contain Alpha Val*/
L03220:    convert pc_vcalc$ to zz%, data goto L03210

           convert zz% to pc_vcalc$, pic(00)

           tab_1% = 7% : tab_2% = 0%
           tab_val$ = pc_vcalc$
           call "APCPR1SB" (tab_1%, tab_2%, tab_val$,tab_desc$, p%, #2,  ~
                                                                tab_rec% )
           pc_vcalc_d$ = str(tab_desc$, 1%, p%-1%)
        return

L03330: REM Key Lookup Methods                    PC_KCALC$
           if pc_kcalc$ <> " " then goto L03360      /* Table doe not    */
L03350:       pc_kcalc$ = "00"                     /* contain alpha    */
L03360:    convert pc_kcalc$ to zz%, data goto L03350

           convert zz% to pc_kcalc$, pic(00)

           tab_1% = 8% : tab_2% = 0%  /* (PRICE 008)-Key Lookup Method */
           tab_val$ = pc_kcalc$
           call "APCPR1SB" (tab_1%, tab_2%, tab_val$,tab_desc$, p%, #2,  ~
                                                                tab_rec% )
           pc_kcalc_d$ = str(tab_desc$,1%, p%-1%)
        return

L03470: REM Key Unit Conversion Method            PC_KUNT$
           if pc_kunt$ <> " " then goto L03500       /* Table does not   */
L03490:       pc_kunt$ = "00"                      /* contain Alpha Val*/
L03500:    convert pc_kunt$ to zz%, data goto L03490

           convert zz% to pc_kunt$, pic(00)

           tab_1% = 9% : tab_2% = 0% /* (PRICE 009) - Unit Conv. Table */
           tab_val$ = pc_kunt$
           call "APCPR1SB" (tab_1%, tab_2%, tab_val$,tab_desc$, p%, #2,  ~
                                                                tab_rec% )
           pc_kunt_d$ = str(tab_desc$,1%, p%-1%)
        return

        exit_program
        end

