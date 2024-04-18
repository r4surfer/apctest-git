        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLC45 - Program (APCPLA45)        *~
            *  Creation Date     - 12/04/96 - File - (@???BIL@)         *~
            *  Last Modified Date- 09/25/2014                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Create BILCO Glass Bridge File with  *~
            *                      Batches. Unit Quantity is Consolidate*~
            *                      for line sizes that fall within the  *~
            *                      Processing Sort Sequence.            *~
            * x$ = bin(35%,1)      STUFF Pound symbol into X$           *~
            *                                                           *~
            *  MOD - Consolidate Glass the Same Size (Maintaining)      *~
            *        Sequence. ( Use 'sav_rec$()' to Save Reord )         *~
            *                                                           *~
            *  MOD - Modify Both the 'Header' and 'Detail' Records      *~
            *        Identification Field. Use 'MODEL' as the           *~
            *        Identifier instead of 'LOAD'.                      *~
            *                                                           *~
            *  MOD - Modify To Calc for Switching to Double Strength    *~
            *        Glass . ( BUILD_MATERIAL )                         *~
            *                                                           *~
            *  MOD - Modify @GLASS@ Record Length to Pass a Route Code  *~
            *        in Position (159), new Length is 165. This will    *~
            *        create Holes for Stock Glass. ( Var = BIN$ )       *~
            *                                                           *~
            * (1) Skip Models (312) Vinyl Patio Doors. Lines xxxxx,xxxxx*~
            *                                                           *~
            * (2) Modified to support the new Priority Sort Code that   *~
            *     is in Position (LAB_GED$,1%,2%). A batch break is     *~
            *     forced each time that the priority Sort Code Changes. *~
            *                                                           *~
            * (3) Mod to 'BUILD_MATERIAL' Force all Bronze Glass to be  *~
            *     Double Strength Glass.                                *~
            *                                                           *~
            *      - Glass Re-Make Status Codes (UPDATE_REMAKE)         *~
            *        0 = Selected for Glass Re-Make - Scanned Re-Make   *~
            *        1 = Scheduled Glass            - New Glass 1st Time*~
            *        2 = Completed Scheduled Glass - Scanned out of Oven*~
            *                                                           *~
            *                                                           *~
            *      - Bridge File Names                     (EWD003)     *~
            *        (@GLSBLA@) - Normal Production Glass  (EWD011)     *~
            *        (@RMKBLK@) - Remake All Glass                      *~
            *        (@RMKBL1@) - Remake Glass Bridge File for Loads    *~
            *        (@RMKBL2@) - Remake Glass Not Used                 *~
            *        (@RMKBL5@) - Remake Vinyl Line Codes Only (AWD013) *~
            *                                              (EWD010)     *~
            * (4) Mod to Sort Bilco Glass for New table Numbers. Special*~
            *     format for Departments. New table (GLASS11  ) this    *~
            *     table contains- ddd-x - x= Single or Double Strength  *~
            *     glass. The 1st character of the description is the    *~
            *     table Number (1 thru 9)                               *~
            *     New Subroutines - sort_bilco (Create new file EWDPLNWK)*~
            *                       find_table_no (From GLASS11 Table)  *~
            *                                                           *~
            *     New Sort Key - lab_key$                               *~
            *                    1%,5%  = Counter (1 to 9999)           *~
            *                    6%,1%  = Table Number                  *~
            *                    7%,1%  = 1 = Single, 2 = Double        *~
            *                    8%,3%  = Filler Area                   *~
            *                                                           *~
            *      - RM_FLAG%   - 0% Production                         *~
            *                     1% Remake                             *~
            *                     2% Tempered Production                *~
            *                     3% Tempered Remake                    *~
            *                     4% Laminate Glass                     *~
            *                     5% Laminate remakes                   *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 12/09/96 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 02/11/97 ! Mods to use GLASS_DTE$ for the Glass     ! RHH *~
            *          ! Production/Make Date.                    !     *~
            * 06/12/97 ! Mods to be current with the changes for  ! RHH *~
            *          ! New Family Products and Dept (049),(052) !     *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 01/14/98 ! Mod to Skip Special Glass that is bought ! RHH *~
            *          !   from the outside. (PLANGLOUT) New      !     *~
            *          !   Subroutine to Check Glass, Based on    !     *~
            *          !   "LITING" Codes in Table.               !     *~
            * 03/12/98 ! Mods to obtain glass sandwich from the   ! RHH *~
            *          !   "PLANBILCO". Replace MAT_SPEC$()       !     *~
            * 03/28/98 ! Y2K                                      ! LDJ *~
            * 07/27/98 ! (EWD001) - Mod for new glass remake      ! RHH *~
            *          ! reason codes (26, 28, 30) Those remakes  !     *~
            *          ! go into a Seperate Batch File @RMKBL?@   !     *~
            * 12/07/98 ! (EWD002) Clean-up mods for new remake    ! RHH *~
            *          !   selections and for (INDY)              !     *~
            * 01/11/99 ! (EWD003) Mods to Create Seperate Files   ! RHH *~
            *          !   Glass Selection                        !     *~
            * 08/25/99 ! (EWD004) Mods to Build Sandwich Remove   ! RHH *~
            *          !   Storm Window Logic                     !     *~
            * 12/20/99 ! (EWD005) Mod for Sort Change             ! RHH *~
            * 06/29/00 ! (EWD006) Mod to change stock             ! RHH *~
            * 07/12/00 ! (EWD007) Take out glass type '04'        ! CMG *~
            * 08/25/00 ! (EWD008) Add Dept mat_spec_bot$ for bottom!RHH *~
            *          !          glass                           !     *~
            * 09/29/00 ! (EWD009) Mod for Bilco to be department  ! RHH *~
            *          !          Specific.                       !     *~
            * 02/20/01 ! (EWD010) Mod for new Hega Glass Table    ! RHH *~
            *          !          Codes. Use new Table (GLASS11)  !     *~
            *          !          New Sub - sort_bilco            !     *~
            * 06/21/01 ! (EWD011) Mods for 2nd version of Bilco   ! RHH *~
            *          !          WINDOWMATE New Table (GLASS12  )!     *~
            * 06/10/04 ! (EWD012) Mods for special glass grid     ! CMG *~
            *          !          codes                           !     *~
            * 12/10/04 ! (AWD013) Mod for vinyl line remakes only ! CMG *~
            * 02/08/05 ! (AWD014) Mod for pre cut glass           ! CMG *~
            * 05/16/05 ! (AWD015) Mods to rearrange gls sort to so! CMG *~
            *          !   DS is sorted together to put in batchs !     *~
            *          !   together                               !     *~
            * 06/27/05 ! (AWD016) Mod to have a look ahead batch  ! CMG *~
            * 08/15/05 ! (AWD017) Mod to re-count batch           ! CMG *~
            * 01/01/06 ! (PAR000) CR347 Mods for new sub part     ! CMG *~
            * 06/22/06 ! (AWD018) Tempered Glass changes          ! DES *~
            * 08/02/07 ! (AWD019) Mods to turn off < 50           ! CMG *~
            * 08/02/07 ! (AWD020) mod to put pre-cut in remakes   ! CMG *~
            *03/02/2009! (AWD021) mod for larger sort, hex        ! CMG *~
            *05/21/2009! (AWD022) mod for florida remake file     ! CMG *~
            *07/07/2009! (AWD023) mod to take pre-cut out of remak! CMG *~
            *11/04/2009! (AWD024) mod for ultra lowe              ! CMG *~
            *03/11/2011! (AWD025) mod for multiple spacer system  ! CMG *~
            *12/05/2011! (AWD026) mod for new tempered sys        ! CMG *~
            *03/23/2012! (AWD027) mod for new triple pane         ! CMG *~
            *04/02/2012! (AWD028) mod for dept 014 DS             ! CMG *~
            *11/19/2012! (AWD029) mod for valance                 ! CMG *~
            *09/25/2014! (AWD030) mods for DS batches             ! CMG *~
            *05/26/2015! (IM8022) lamin batches                   ! CMG *~
            *07/07/2016! (CR437) Allow Remake Groupings to        ! CMG *~
            *          !          Increase Yield                  !     *~
            *************************************************************


        sub "APCPLC45" (size%,           /* Specified Batch Size      */ ~
                        scr_sel$,        /* Screen Selection (EWD003) */ ~
                        glass_dte$,      /* Glass Production Date     */ ~
                        scr_dte$,        /* Planned Production Date   */ ~
                        scr_dte1$,       /* Planned Prod. Unformated  */ ~
                        file$,           /* Name of Optimized File    */ ~
                        bat$,            /* Number of Batches Created */ ~
                        rm_flag%,        /* Remake Flag 0% or 1%      */ ~
                        ss$(),           /* Sort Codes      (AWD015)  */ ~
                        ds_batch$,       /* DS Batch flag    (AWD018) */ ~
                        reschedBatch$,   /* Reschedule Batch (AWD030) */ ~
                        tempScrn%,       /* (AWD034) tempered         */ ~
                        lamnScrn%,       /* laminate (AWD045)         */ ~
                        #1,              /* (APCPLNWK) Label Detail Fl*/ ~
                        #3,              /* (APCPLNGR) Label Detail Fl*/ ~
                        #5)              /* (GENCODES) Optimizer File */


                                         /* (EWD001) - Mod 07/27/98   */
                                         /* (AWD013) - Mod 12/10/04   */

        dim glass_dte$8, glass_dte1$10,  /* Glass Completion Date     */ ~
            scr_dte$8, bat$3,            /* Glass Production Date     */ ~
            scr_dte1$8,                  /* Glass Prod. Date UNFORMAT */ ~
            dt_dept$3,                   /* Department Code           */ ~
/*AWD020*/  bat_rec$165, bin$7,          /* Batch Record, ROUTE CODE  */ ~
            sav_rec$165,                 /* Batch Record              */ ~
            seq$3,                       /* Item Numbers              */ ~
/*AWD027*/  mat_spec$20,                 /* Material Spec's           */ ~
/*AWD027*/  mat_spec_bot$20,             /* Material Spec's (EWD008)  */ ~
            harp$4,                      /* Harp Rack Location '@XXT' */ ~
/*AWD025*/  lab_key$62,                  /* GED Primary Key           */ ~
            lab_ged$66,                  /* GED Primary Key           */ ~
/*PAR000*/  lab_rec$(2%)223,             /* Main Data Record          */ ~
            new_key$66,                  /* GED Primary Key           */ ~
/*AWD025*/  new_ged$62,                  /* New File Record   (AWD018)*/ ~
/*PAR000*/  new_rec$(2%)223,             /* New file record   (AWD018)*/ ~
            lab_bilc$10,                  /* Sort Key          (EWD010)*/ ~
            sandwich$10,                 /* Glass Sandwich    (EWD010)*/ ~
            table_no$1,                  /* Glass Table No.   (EWD010)*/ ~
            table$1,                     /* Glass Table No.   (EWD010)*/ ~
            sav_table$1,                 /* Save Table No.    (EWD010)*/ ~
             x$1,                         /* Store Pound sysbol(EWD010)*/ ~
            stock$6,                     /* STOCK Indicator           */ ~
            ty$2,                        /* Type of Glass             */ ~
            load$5,                      /* Load Number               */ ~
            sav_model$3,                 /* Save Model Code           */ ~
            cl$1,                        /* Color of Product          */ ~
            wd1$9,                       /* Cut Width of Glass        */ ~
            ht1$8,                       /* Cut Height of Glass       */ ~
            view$3,                      /* View ( Top/Bot )          */ ~
            model$3,                     /* Model Product Code        */ ~
            file$20,                     /* Batch File Name           */ ~
            inc$12,                      /* Batch File Identifier     */ ~
            hdr$40,                      /* ASKUSER Header Text       */ ~
            userid$3,                    /* User Id       (EWD002)    */ ~
/*AWD018*/  temper_flag$1,                                               ~
            msg$(3%)79,                  /* ASKUSER Info Text         */ ~
            errormsg$79,                 /* Error Message Text        */ ~
/*AWD025*/  sort$5, sav_sort$5,          /* Glass Sort Seq. Codes     */ ~
            readkey$24, descr$32,        /* Gencodes Key              */ ~
            short_date$8, short_date2$8, /* force to short date       */ ~
            short_date3$8,               /*  ""   ""                  */ ~
            spacer$6, sav_spacer$6,      /* Spacer Thickness Codes    */ ~
/*(CR437)*/ reason_flag$2,               /* Remake Reason Flag Int/Ext*/ ~
/*(CR437)*/ reason_type$10               /* Remake Type               */

        dim so$8,                        /* S.O.                       */~
            scr_sel$1,                   /* Screen Selection (EWD003)  */~
            rm_key$23, rm_rec$(2%)256,   /* Remake Key and Record      */~
            ff$8,                        /* Remake Production Day      */~
            rm_st_time$8, rm_st$1,       /* Remake Stat Time Change    */~
            rm_reason$2, chk_st$1,       /* Remake Reason Code         */~
            rm_st_dte$6                  /* Remake Status Date         */

        dim                              /*  (AWD015)                  */~
/*AWD025*/  ss$(999%)13,                 /* Glass Sort         (AWD015)*/~
            dept$3,                      /* Department         (AWD015)*/~
            strength$,                   /* Strength           (AWD015)*/~
            sort1$13,                    /* Sort               (AWD015)*/~
            p_sort$8,                    /* Partial Sort       (AWD015)*/~
            sav_p_sort$8,                /* Save Partial Sort  (AWD015)*/~
            sav_sp$4,                    /* Save Spacer        (AWD015)*/~
            sp$4                         /* Spacer             (AWD015)*/

/* (AWD025) mods to field lengths and array size */
        dim glscntr_key$13,              /* Gls Counter Readkey(AWD016)*/~
            glscntr_sort$13,             /* Gls Counter Sort   (AWD016)*/~
            lab_fil$9,                   /* Label Filler       (AWD016)*/~
            new_sort$(999)13,            /* New Glass sort     (AWD017)*/~
            new_sort_cnt%(999)          /* New Glass Counter  (AWD017)*/

        dim ds_batch$1, reschedBatch$1,  /* DS Batching?       (AWD017)*/~
            part$25,                     /* PArt number        (AWD017)*/~
            lt$1                         /* Liting Code        (AWD017)*/

        dim intercept$2,                 /* (AWD025) Intercept        */~
            sav_intercept$2,             /* (AWD025) Intercept        */~
            ultra$1,                     /* Ultra intercept           */~
            patio$1,                     /* Patio Flag                */~
            dtDte$6                      /* DT PRD DTE                */



        dim f2%(10%),                    /* = 0 if the file is open    */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            mode$5,                      /* File Mode (EWD010)         */~
            rslt$(10%)20                 /* Text from file opening     */

        dim logMessage$256               /* (AWD036) log message   */


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCPLNWK ! Glass Work File (Labels)                 *~
            * #3  ! APCPLNGR ! Glass Sched/Remake File                  *~
            * #2  ! @GLSBLA@ ! Glass Batch File for Bilco Glass System  *~
            * #4  ! @RMKBL1@ ! Remake Batch File for Bilco Glass Loads  *~
            * #5  ! GENCODES ! Master System Table File                 *~
            * #6  ! @RMKBL2@ ! Remake Batch Bilco Not Used    (EWD001)  *~
            * #7  ! @RMKBLK@ ! Remake Batch File for Bilco Glass All    *~
/*(AWD013)*/* #8  ! @RMKBL5@ ! Remake Batch File for Vinyl Line Codes   *~
/*(AWD018)*/* #9  ! @RMKBLT@ ! Remake Batch File for Tempered Glass     *~
            * #10 ! EWDPLNWK ! New Sort File for Bilco                  *~
/*(AWD018)*/* #11 ! @GLSBLT@ ! Remake Batch File for Tempered Glass     *~
/*(AWD017)*/* #16 ! AWDPLNWK ! Glass Work File (RESORT DATA)            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #10,   "EWDPLNWK",                                    ~
                        varc,     indexed,  recsize = 574,               ~
                        keypos =   1, keylen =   10,                     ~
                        alt key  1, keypos =    11, keylen =  128


/* (AWD025) modifications to key length and rec size to match #6 */

            select #16, "AWDPLNWK",                                      ~
/*PAR000*/              varc,     indexed,  recsize =   574,             ~
                        keypos =    1, keylen =   128

            select #02, "@GLSBLA@", consec, recsize = 165       /* (IM8022) */
            select #07, "@RMKBLK@", consec, recsize = 165       /* (IM8022) */
            select #08, "@GLSBLI@", consec, recsize = 165       /* (IM8022) */            
            select #09, "@RMKBLT@", consec, recsize = 165       /* (IM8022) */
            select #12, "@GLSPAT@", consec, recsize = 165       /* (IM8022) */
            select #11, "@RMKPAT@", consec, recsize = 165       /* (IM8022) */
            select #17, "@GLSBLV@", consec, recsize = 165       /* (IM8022) */
            select #18, "@RMKBLV@", consec, recsize = 165       /* (IM8022) */
            select #19, "@GLSBLL@", consec, recsize = 165       /* (IM8022) */
            select #20, "@RMKBLL@", consec, recsize = 165       /* (IM8022) */



            x$ = bin(35%,1)        /* Store Pound Symbol (EWD010) */

            glass_dte1$ = glass_dte$
            if len(glass_dte1$) = 10% then                          ~
               call "DATUFMTC" (glass_dte1$)                        ~
            else                                                    ~
               call "DATUNFMT" (glass_dte1$)

            short_date3$ = glass_dte$
            call "DATEFMT" (short_date3$)
            short_date$ = scr_dte1$
            call "DATEFMT" (short_date$,ff%,short_date2$)

                  /* (EWD018) Begin */
            sort_bilco% = 0%                   /* (EWD010) Sort File    */
            if tempScrn% = 1% then goto tempScrnFiles
            
            ff$ = "@GLSBLA@" : ff% = 2%  
            if scr_sel$ <> "8" then notValance
               ff$ = "@GLSVLB@" : ff% = 17%
               goto openFile                           
notValance:            
            if scr_sel$ <> "9" then goto notValRmk
               ff$ = "@RMKVLB@" : ff% = 18%
               sort_bilco% = 1%               
               goto openFile
notValRmk:  
            if scr_sel$ <> "4" then goto notAnnLam
               ff$ = "@GLSBLL@" : ff% = 19%
               goto openFile                           
notAnnLam:               
            if scr_sel$ <> "7" then goto notAnnLamRmk
               ff$ = "@RMKBLL@" : ff% = 20%
               sort_bilco% = 1%
               goto openFile                           
notAnnLamRmk:   
            if scr_sel$ <> "3" then goto notAnnRmk
               ff$ = "@RMKBLK@" : ff% = 7%  
               sort_bilco% = 1%              
               goto openFile            
notAnnRmk:               
            if scr_sel$ <> "6" then goto notAnnIndy
               ff$ = "@GLSBLI@" : ff% = 7%  
               sort_bilco% = 1%              
               goto openFile            
notAnnIndy:      

               goto openFile            /* End of Annealed FILES */         

tempScrnFiles:                                  /* Tempered Glass        */
            ff$ = "@GLSBLT@" : ff% = 11%  
            if scr_sel$ <> "3" then goto notTempRmk
               ff$ = "@RMKBLT@" : ff% = 9%  
               sort_bilco% = 1%              
               goto openFile             
notTempRmk:
            if scr_sel$ <> "5" then goto notTempPatio
               ff$ = "@GLSPAT@" : ff% = 12%  
               goto openFile    
notTempPatio:               
            if scr_sel$ <> "7" then goto notTempPatioRmk
               ff$ = "@RMKPAT@" : ff% = 11%  
               sort_bilco% = 1%              
               goto openFile    
notTempPatioRmk:
            if scr_sel$ <> "B" then goto notTempLam
               ff$ = "@GLSBLL@" : ff% = 19%
               goto openFile
notTempLam:
            if scr_sel$ <> "C" then goto notTempLamRmk
               ff$ = "@RMKBLL@" : ff% = 20%
               goto openFile
notTempLamRmk:

                                               /* Create Glass Batches  */
openFile:                                               
            call "OPENFILE" (#ff%, "IO   ", f2%(ff%), rslt$(ff%), axd$ )
/*AWD018*/ temper_flag$ = "N"
/*AWD018*/ if ff% = 11% or ff% = 9% then temper_flag$ = "Y"
            if f2%(ff%) <> 0% then goto L01300
               gosub file_exists
               if comp% <> 16% then goto L01260
                  call "FILEBGON" addr(#ff%)
                  goto L01300

L01260:        close #ff%
               call "OPENFILE" (#ff%,"EXTND",f2%(ff%),rslt$(ff%),axd$ )
               goto L01360

L01300:     str(rslt$(ff%),1%,6%)  = "OUTPTP"
            str(rslt$(ff%),7%,8%)  = "00001000"
            str(rslt$(ff%),15%,3%) = "100"
            str(rslt$(ff%),18%,3%) = "100"
            call "OPENFILE" (#ff%,"OUTPT",f2%(ff%), rslt$(ff%), axd$ )

L01360:
                                                  /* (AWD017)          */
            file% = 0%
            bat_no% = 1%          /* (AWD025) move before sort_records */
            if ff% = 17% then bat_no% = 501%
            qty%, count% = 0%

            gosub SORT_RECORDS    /* Sort Actual Records to be in File */

            sort_bilco% = 1%
            if sort_bilco% = 0% then gosub sort_bilco /* (EWD010)        */


            call "SHOSTAT" ("Creating Batch(es)-"& file$ )
                                                            /* (AWD015) */
            init(" ") p_sort$, sav_p_sort$, dept$, strength$,          ~
                      sort1$, lab_fil$, part$, lt$, temp$, logMessage$,~
                      reason_flag$

            sav_table$ = " "                        /* (EWD010)         */
            lab_ged$ = all(hex(00))
            lab_key$ = all(hex(00))                  /* (EWD010) - Begin */
            lab_bilc$ = all(hex(00))                 /* (AWD025) */
            if sort_bilco% = 1% then                                       ~
               read #16,key > lab_key$, using L01470, lab_key$, lab_ged$,  ~
                                 lab_rec$(), eod goto create_done          ~
                                else                                       ~
               read #10,key > lab_bilc$, using L01475, lab_bilc$, lab_key$, ~
                                 lab_ged$,  lab_rec$(), eod goto create_done
                                                     /* (EWD010) - End   */
            sav_sort$   = str(lab_key$,8%,5%)           /* (AWD025) */

            sav_intercept$ = str(lab_rec$(),241%,2%)
            sav_spacer$ = str(lab_ged$,3%,6%)
            sav_p_sort$ = "XXXXXXXX"                    /* (AWD015) */
            sav_sp$ = "XXXX"                            /* (AWD015) */
            glscntr_sort$ = "XXXXXXXXXXXXX"             /* (AWD016) */
            reason_flag$  = "XX"                        /* (CR437)  */
/* (AWD025) */
REM  !            if sort_bilco% = 0% then sav_table$ = str(lab_key$,6%,1%)
            if sort_bilco% = 0% then sav_table$ = str(lab_bilc$,6%,1%)
            goto L01480
        create_next                                  /* (EWD010) - Begin */
            if sort_bilco% = 1% then                                       ~
               read #16, using L01470, lab_key$, lab_ged$, lab_rec$(),     ~
                                                      eod goto create_done ~
                                else                                       ~
               read #10, using L01475, lab_bilc$, lab_key$, lab_ged$,      ~
                             lab_rec$(), eod goto create_done
                                                     /* (EWD010) - End   */
L01470:       FMT CH(62), CH(66), 2*CH(223)               /* (AWD017)      */
L01475:       FMT CH(10), CH(62), CH(66), 2*CH(223)       /* (AWD017)      */
L01480:     dt_dept$ = str(lab_rec$(),183%,3%)   /* 09/13/2000           */

            sort1$ = str(lab_rec$(),211%,11%)
REM            if sort1$ <> "999Tempered" then goto NOT_TEMPSORT3
REM                  call "SHOSTAT" ("TEMP SORT 3rd" )  REM stop
REM NOT_TEMPSORT3

/* (AWD025) */

            intercept$ = str(lab_rec$(),241%,2%)
            sort$   = str(lab_key$,8%,5%)
            sort1$  = " "

            lab_fil$ = str(lab_rec$(),50%,9%)             /* (AWD017)       */
                                                         /* The entire sort */
/* (AWD026) */
            awdschgl% = 0%
            if str(lab_key$,3%,5%) = "00009" then awdschgl% = 1%
/* (\AWD026) */

            sort1$  = str(lab_rec$(),211%,11%) & str(lab_rec$(),241%,2%)
            glscntr_key$ = str(lab_rec$(),222%,11%) & str(lab_rec$(),241%,2%)

            if glscntr_sort$ = "XXXXXXXXXXXXX" then  /* (AWD017) */   ~
                                        glscntr_sort$ = glscntr_key$
/*(CR437)*/                                        
            if reason_flag$ = "XX" then reason_flag$ = str(lab_ged$,1%,2%)

            gosub UPDATE_CNTR
                                                 /* (EWD001) - Begin     */
            gosub check_remake
            if rm_ok% = 0% then goto create_next /* Skip this Pass       */
                                                 /* (EWD001) - End       */

            part$ = str(lab_rec$(),59%,25%)     /* MFG Part Number   */
            lt$   = str(part$,7%,2%)           /* Grid/Liting Code  */

            if lt$ > "82" and lt$ < "88" then goto create_next
            if lt$ > "96" and lt$ <= "99" then goto create_next

            if lt$ > "82" and lt$ < "88" then goto create_next
            if lt$ > "96" and lt$ <= "99" then goto create_next
            if lt$ = "V0" then goto create_next


            p_sort$ = str(sort1$,4%,8%)
            dept$   = str(sort1$,1%,3%)
            strength$ = str(sort1$,4%,2%)
            sp$ = str(sort1$,6%,4%)                    /* Spacer          */
            if sav_p_sort$ = "XXXXXXXX" then sav_p_sort$ = p_sort$
            if sav_sp$ = "XXXX" then sav_sp$ = sp$
                                                 /* (AWD015) - end       */

            spacer$ = str(lab_ged$,3%,6%)
                                                 /* (EWD010)             */
/* (AWD025) */
REM  !            if sort_bilco% = 0% then table$ = str(lab_key$,6%,1%)
            if sort_bilco% = 0% then table$ = str(lab_bilc$,6%,1%)
            so$ = str(lab_rec$(),97%,8%)


            if sort_bilco% = 1% then goto L01595   /* (EWD010)          */
               if sav_table$ = table$ then goto L01595
                  goto L01600
L01595:     REM IF SAV_SPACER$ = SPACER$ THEN GOTO L01650  /* (CR437) */
            if sav_spacer$ = spacer$ and rm_flag% <> 1% then goto L01650 /* (CR437) */
            if reason_flag$ = str(lab_ged$,1%,2%) and rm_flag% = 1% /* (CR437) */ ~
                                                              then goto L01650
L01600:                                              /* (AWD015) */
              if rm_flag% = 1% then goto remake_cntr
              sav_sort$      = sort$
              if sav_intercept$ <> intercept$ then bat_no% = bat_no% + 100%
              sav_intercept$ = intercept$          /* (AWD025) */

/*(AWD017)  */
              if ds_batch$ = "0" then goto not_ds

              if strength$ = "DS" and glscntr_key$ <> glscntr_sort$   ~
                               then goto deptFound

              if sav_sp$ = sp$ and strength$ = "DS" and   /* (AWD015) */   ~
                         dept$ <> "047" then goto L01670

deptFound:
              if dept$ = "047" and sp$ = "SP11" then goto L01670 /* (AWD015) */

not_ds:

              sav_sort$ = sort$
              glscntr_sort$ = glscntr_key$
              sav_intercept$ = intercept$             /* (AWD025) */
remake_cntr:
               gosub build_end
               count% = 0%
               sav_spacer$ = spacer$
               sav_sort$   = sort$
               reason_flag$ = str(lab_ged$,1%,2%)        /* (CR437) */
               if sav_intercept$ <> intercept$ then bat_no% = bat_no% + 100%
               sav_intercept$ = intercept$         /* (AWD025) */
               sav_p_sort$ = str(sort1$,4%,8%)     /* (AWD015) */
               sav_sp$     = sp$                   /* (AWD015) */
               if sort_bilco% = 0% then sav_table$ = table$ /* (EWD010) */
               goto L01670
L01650:     if sav_sort$ <> sort$ then goto L01600              /* (AWD025) */
            if sav_intercept$ <> intercept$ then bat_no% = bat_no% + 100%
            if sav_intercept$ <> intercept$ then goto L01600    /* (AWD025) */

L01670:     stock$ = str(lab_rec$(),50%,6%)                  /* Stock  */
            bin$ = "        "                          /* Set to Blank */
                                                         /* (AWD023) */
            if scr_sel$ <> "2" and scr_sel$ <> "8" then goto L01730
            if stock$ <> "STOCK " then goto L01730
/* (AWD020) add P back */
/* (AWD020) do not add P back because this completely removes from file */
/* Glass wants the ability to cut if needed.                            */

                                            /* (EWD006) - 06/29/00     */
               bin$ = "P ++ P"              /* Leave a Hole for Stock  */
                                            /* For All Models 01/26/99 */
                                            /* (EWD006)                */

L01730:     harp$ = str(lab_rec$(),55%,4% )   /* N/A - For Aluminum    */
            ty% = 0%
            ty$ = str(lab_rec$(),257%,2%)     /* Glass Type CL,LE,Etc  */
            if ty$ <= " " then ty$ = str(lab_rec$(),63%,2%)
/* (AWD027)*/
            tripPane% = 0%
            gosub lookup_3p
/* (\AWD027) */
            convert ty$ to ty%, data goto L01780
L01780:
            temp$ = str(lab_rec$(),153%,1%)

/* (AWD025) */
            if dept$ = "999" then goto skip_temp_check

skip_temp_check:
            model$ = str(lab_rec$(),6%,3%)             /* Model Code   */
        REM - SKIP VINYL PATIO DOORS
            if model$ = "312" then goto create_next    /* Skip Patio   */

            load$  = str(lab_rec$(),1%,5%)             /* Load Number  */
            if count% = 0% then gosub build_title      /* Create Batch */
            if sav_model$ <> model$ then gosub build_header

            cl$    = str(lab_rec$(),9%,1%)             /* Color Code   */
            wd1$   = str(lab_rec$(),19%,9%)            /* Window Width */
            ht1$   = str(lab_rec$(),28%,8%)            /* Window Height*/
            view$  = str(lab_rec$(),45%,1%) & "  "     /* View T or B  */

            gosub update_remake
            if error% = 1% then goto create_next
            gosub build_detail                         /* (AWD017)  */
            if count% < size%  then goto create_next

               gosub build_end
               count% = 0%
               goto create_next

        create_done
            gosub build_end
            close #ff%
            convert (bat_no% - 1%) to bat$, pic(000)
            if sort_bilco% = 0% then gosub delete_work  /* (EWD010)    */

        goto exit_program                    /* (EWD001) - End         */

        CHECK_GLASS                      /* Test Liting Codes in Table */
                                         /* Check for outside purchase */
           init(" ") readkey$ : check% = 0%   /* to see if Skipped     */
           str(readkey$,1%,9%)   = "PLANGLOUT"
           str(readkey$,10%,15%) = str(new_rec$(),65%,2%)
           read #5,key = readkey$, eod goto L02150
           check% = 1%
L02150: return

        CHECK_GLASS12                    /* Check for Windowmate A or B*/
           init(" ") readkey$ : check1% = 0%  /* to see if Skipped     */
           if scr_sel$ <> "2" then return     /* Not related to remake */
           str(readkey$,1%,9%)   = "GLASS12  "
           str(readkey$,10%,3%)  = str(new_rec$(),183%,3%)  /* Department*/
           str(readkey$,13%,6%)  = str(new_ged$,3%,6%)      /* Spacer    */
           read #5,key = readkey$, eod goto L02160
           check1% = 1%                                   /* Skip      */
L02160: return

        build_title                            /* NEW TITLE EACH BATCH */
          init(" ") bat_rec$, sav_model$
          errormsg$="(Error)-(Title) Rec in Batch- "& file$
          inc$ = " BATCH (XXX)"
          convert bat_no% to str(inc$,9%,3%), pic(000)

        if sort_bilco% = 0% then goto build_title_table
        if rm_flag% = 1% then goto build_rmk_title        /* (CR437)   */
          str(bat_rec$,1%,2%) = "NT"                      /* New Title */
          str(bat_rec$,3%,32%)= file$ & inc$
          str(bat_rec$,35%,131%) = " "                    /* Line Feed */
          write #ff%, bat_rec$, eod goto L03020
        return

        build_rmk_title                                   /*(CR437) */
          init(" ") reason_type$
          str(bat_rec$,1%,2%) = "NT"                      /* New Title */
          str(bat_rec$,3%,32%)= file$ & inc$
          if reason_flag$ = "00" then reason_type$ = "EXTERNAL"
          if reason_flag$ = "01" then reason_type$ = "INTERNAL"
          str(bat_rec$,35%,14%) = " " & intercept$ & "-" & reason_type$
          str(bat_rec$,49%,117%) = " "                    /* Line Feed */
          write #ff%, bat_rec$, eod goto L03020
        return          
        build_title_table
          str(bat_rec$,1%,2%) = "NT"                      /* New Title */
          str(bat_rec$,3%,2%) = table$ & x$               /* (EWD010)  */

          str(bat_rec$,5%,32%)= file$ & inc$
          str(bat_rec$,37%,129%) = " "                    /* Line Feed */
          write #ff%, bat_rec$, eod goto L03020
                                                          /* (EWD010)  */
        return

        build_header                           /* New Header each Load */
          if count% = 0% then goto L02340
             bat_rec$ = sav_rec$
             gosub build_detail_last

L02340:   init(" ") bat_rec$, sav_rec$
         errormsg$="(Error)-(Header) Rec in Batch- "& file$
          qty%, seq% = 0%
          sav_model$ = model$
          str(bat_rec$,1%,2%)   = "NH"                 /* New Header   */
                                                       /* Model Code   */
          str(bat_rec$,3%,10%)  = model$ & " " & str(short_date2$,3%,6%)        /* (Y2K, LDJ) */
          str(bat_rec$,13%,18%) = " "
          str(bat_rec$,31%,8% ) = short_date$       /* Production Date */       /* (Y2K, LDJ) */
          str(bat_rec$,39%,8% ) = short_date3$      /* Glass Prod Date */       /* (Y2K, LDJ) */
          str(bat_rec$,47%,119%)= " "               /* Line Feed       */
          write #ff%, bat_rec$, eod goto L03020
        return

        build_detail
          init(" ") bat_rec$, qty$, seq$
          errormsg$ ="(Error)-(Item) Rec in Batch- "& file$
          gosub build_material
          str(bat_rec$,1%,2%)   = "NI"                  /* New Detail  */
                                                        /* Model Code  */
          str(bat_rec$,3%,10%)  = model$ & " " & str(short_date2$,3%,6%)        /* (Y2K, LDJ) */
          str(bat_rec$,13%,3%)  = seq$                  /* Item Number */
          str(bat_rec$,16%,4%)  = qty$                  /* Unit Qty    */
          str(bat_rec$,20%,9%)  = wd1$                  /* Cut Width   */
          str(bat_rec$,29%,9%)  = " " & ht1$            /* Cut Height  */
          str(bat_rec$,95%,4%)  = harp$                 /* Harp Rack   */
          str(bat_rec$,99%,9%)  = "         "           /* Sales Order */
          str(bat_rec$,108%,5%) = "     "               /* Reference No*/
          str(bat_rec$,113%,4%) = "****"                /* Reserved    */
          str(bat_rec$,117%,8%) = "        "            /* Model/Color */
/* (AWD027) */
REM       STR(BAT_REC$,125%,11%)= MAT_SPEC$             /* MATERIAL SPC*/
          str(bat_rec$,125%,20%)= mat_spec$             /* Material Spc*/
          count% = count% + 1%
REM         STR(BAT_REC$,136%,23%) = "                       "
          str(bat_rec$,145%,14%) = "                       "
/* (\AWD027) */
/* (AWD020) make bin 7 char instead of 1 */
REM            STR(bat_rec$,159%,1%)  = BIN$        /* PUT HOLE FOR STOCK */
REM            STR(bat_rec$,160%,6%)  = " "
            str(bat_rec$,159%,7%)  = bin$        /* Put Hole for Stock */

REM     IF (SCR_NBR% = 6%) THEN STR(bat_rec$,159%,34%) = "K++K"
          if tempScrn% = 1% then str(bat_rec$,159%,34%) = "K++K"

          if sav_rec$ <> " " then goto L02730
             sav_rec$ = bat_rec$

L02730:   if sav_rec$ <> bat_rec$ then goto build_detail_last
             qty% = qty% + 1%
             return

        build_detail_last
          seq% = seq% + 1%
          convert seq% to seq$, pic(###)
          convert qty% to qty$, pic(####)
          qty% = 1%
          str(sav_rec$,13%,3%)  = seq$                  /* ITEM NUMBER */
          str(sav_rec$,16%,4%)  = qty$                  /* UNIT QTY    */
          write #ff%, sav_rec$, eod goto L03020
          sav_rec$ = bat_rec$
        return

        build_end
          if count% = 0% then return
             bat_rec$ = sav_rec$
             gosub build_detail_last
             sav_sort$   = str(lab_key$,8%,5%)
             sav_intercept$ = str(lab_key$,1%,2%)
             sav_spacer$ = str(lab_ged$,3%,6%)          /* (EWD010)     */
/* (AWD025) */
REM  !             IF SORT_BILCO% = 0% THEN SAV_TABLE$ = STR(LAB_KEY$,6%,1%)
             if sort_bilco% = 0% then sav_table$ = str(lab_bilc$,6%,1%)
             qty% = 0%
          init(" ") bat_rec$, sav_rec$
          errormsg$ = "(Error)-(End) Rec in Batch- "& file$
          str(bat_rec$,1%,3%) = "END"                /* END OF BATCH    */
          str(bat_rec$,4%,162%) = " "               /* LINE FEED       */
          write #ff%, bat_rec$, eod goto L03020
          bat_no% = bat_no% + 1%
        return
L03020:   gosub error_prompt
        return

        build_material
          if ty% = 0% then ty% = 1%
          gosub load_sandwich

                                                     /* (EWD006)       */
          stock$ = str(lab_rec$(),50%,5%)
                                                     /* (EWD008)- Fix  */
                                                     /* Bottom Glass   */
          if str(view$,1%,1%) = "B" then mat_spec$ = mat_spec_bot$
                                                     /* (EWD008)       */
          if stock$ <> "STOCK" then goto L03025
/* (AWD020) */
REM IF STR(MAT_SPEC$,2,2) = "CL" THEN STR(MAT_SPEC$,2%,2%)  = "PC"
REM IF STR(MAT_SPEC$,2,2) = "PA" THEN STR(MAT_SPEC$,2%,2%)  = "PL"
REM IF STR(MAT_SPEC$,10,2) = "CL" THEN STR(MAT_SPEC$,10%,2%)  = "PC"
REM IF STR(MAT_SPEC$,10,2) = "PA" THEN STR(MAT_SPEC$,10%,2%)  = "PL"
REM IF TY% = 1% THEN STR(MAT_SPEC$,10%,2%) = "PC"
REM IF TY% = 4% THEN STR(MAT_SPEC$,2%,2%) = "PC"

                                                     /* (EWD007)       */
                                                     /* (EWD006) - End */
L03025:
          if ty% <> 2% then goto L03130
             if str(view$,1%,1%) = "T" then goto L03170
                str(mat_spec$,10%,2%) = "OB"
                mat_spec$ = mat_spec_bot$            /* (EWD008)       */
                goto L03170
L03130:   if ty% <> 14% then goto L03170
             if str(view$,1%,1%) = "T" then goto L03170
                str(mat_spec$,10%,2%) = "CL"
                mat_spec$ = mat_spec_bot$            /* (EWD008)       */
                                         /* Calc Double Strength Glass */
L03170:   if dt_dept$ = "047" or dt_dept$ = "033" then goto L03380
                                         /* 09/13/2000                 */

          x, y, z = 0.0                  /* (EWD004) Remove Storm Window*/
                                         /*    Logic                    */
                         /* ALL BRONZ GLASS IS DOUBLE STRENGTH 3/12/98 */
          if ty% = 6% or ty% = 10% or ty% = 12% then goto L03380
          if ty$ = "B9" or ty$ = "C0" or ty$ = "C0" or ty$ = "D1" then   ~
                                                     goto L03380
          if ty% = 7% or ty% = 12% or ty% = 15% then goto L03370

          convert str(wd1$,1%,3%) to x, data goto L03270
L03270:
          convert str(ht1$,1%,2%) to y, data goto L03290
L03290:
          if str(wd1$,7%,1%) = "/" then x = x + 1.0
          if str(ht1$,6%,1%) = "/" then y = y + 1.0
          z = (x * y)/ 144.0
          if z > 20.0 then goto L03380                /* Square Feet     */
          if x > 60.0 then goto L03380                /* Width Greater   */
          if y > 60.0 then goto L03380                /* Height Greater  */
          if (x + y) > 100.0 then goto L03380         /* Tot United Inch */
L03370: return
L03380:   str(mat_spec$,1%,1%) = "4"
          str(mat_spec$,9%,1%) = "4"
        return

        load_sandwich
/* (AWD027) */
            if tripPane% = 1% then goto p3_lookup_sandwich
            init(" ") mat_spec$, readkey$, descr$, mat_spec_bot$
            str(readkey$,1%,9%) = "PLANBILCO"
            str(readkey$,10%,2%) = ty$
            read #5,key = readkey$, using L03470 , descr$, eod goto L03490
L03470:        FMT POS(25), CH(30)
            mat_spec$ = str(descr$,1%,11%)
            mat_spec_bot$ = str(descr$,15%,11%)     /* (EWD008)     */
L03490: return
/* (AWD027) */
        p3_lookup_sandwich
            init(" ") mat_spec$, readkey$, descr$, mat_spec_bot$
            str(readkey$,1%,9%) = "PLANBILCO"
            str(readkey$,10%,3%) = ty$ & "T"
            read #5,key = readkey$, using L03470 , descr$,                ~
                                            eod goto p3_lookup_sandwich_bot

            mat_spec$ = str(descr$,1%,20%)
p3_lookup_sandwich_bot:
            init(" ") readkey$, descr$, mat_spec_bot$
            str(readkey$,1%,9%) = "PLANBILCO"
            str(readkey$,10%,3%) = ty$ & "B"
            read #5,key = readkey$, using L03470 , descr$,                ~
                                            eod goto p3_lookup_sandwich_done
            mat_spec_bot$ = str(descr$,1%,20%)
        p3_lookup_sandwich_done
        return
/* (\AWD027) */

        file_exists
            comp% = 2%
            hdr$ = "** Optimization File Exists **"
            msg$(1%) = "The File (XXXXXXXX) Already Exists. "
            str(msg$(1%),11%,8%) = ff$
            msg$(2%) = "             O P T I M I Z A T I O N             "
            msg$(3%) = "Press <RETURN> To Continue, or PF(16) to Delete. "
            call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        check_remake                                   /* (EWD001) Begin */
           if rm_flag% = 0% then goto L03520           /* (All) Pass     */
           rm_ok% = 0%                                 /* Skip Process   */
           init(" ") rm_key$
           str(rm_key$,1%,9%)  = str(lab_ged$,53%,9%)
           str(rm_key$,10%,3%) = str(lab_rec$(),186%,3%)


/* CMG DES */
           goto read_remake
/* (IM8022) */
REM           IF SCR_NBR% <> 6% THEN GOTO READ_REMAKE
REM           INIT(" ") RM_KEY$
REM           STR(RM_KEY$,1%,9%)  = STR(LAB_REC$(),97%,8%) & STR(LAB_REC$(),190%,1%)
REM           STR(RM_KEY$,10%,3%) = STR(LAB_REC$(),186%,3%)
REM           call "SHOSTAT" (" Remake Key " & rm_key$)  stop

        read_remake


           read #3,key = rm_key$, using L03500, rm_reason$,eod goto L03525
L03500:       FMT POS(34), CH(2)
           rm_reason% = 0%
           convert rm_reason$ to rm_reason%, data goto L03510
L03510                                           /* (EWD002) - Begin     */


           if ff% = 8% then goto L03520              /* (AWD013)  */
           if ff% = 7% or ff% = 4% then goto L03520 /* Both     (EWD003) */
           if ff% = 6% then goto L03515          /* In House (EWD003)    */
              if rm_reason% > 25% and rm_reason% < 31% then return
                                                 /* Skip Do Not Process  */
              goto L03520                        /* Only Process Production*/

L03515:       if rm_reason% > 25% and rm_reason% < 31% then goto L03520
                                                /* Process In-House      */
                 return                         /* Skip all Production   */
                                                /* (EWD002) - End        */
L03520:    rm_ok% = 1%                          /* Process Record        */
L03525: return                                  /* (EWD001) - End        */

        update_remake              /* (EWD002) All Glass Has at Least One*/
                                   /* entry in (APCPLNGR)                */
                                   /* Status 0=Remake,1=Sched,2=Complete */
           init(" ") rm_key$,rm_st_time$,rm_reason$,rm_st_dte$,          ~
                     rm_rec$(), rm_st$, errormsg$, chk_st$
           error% = 0%
           call "TIME" (rm_st_time$)             /* Time Glas Scheduled*/
           rm_st$ = "1"                          /* Scheduled Glass    */
           rm_reason$ = "00"                     /* Normal Sched Glass */
           rm_st_dte$ = date                     /* Todays Date        */
           str(rm_key$,1%,9%)  = str(lab_ged$,53%,9%)  /* Glass Barcode*/
           str(rm_key$,10%,3%) = str(lab_rec$(),186%,3%) /* Re-Make No */

           read #3,hold,key = rm_key$, using L03730 , rm_rec$(),        ~
                                                  eod goto schedule_glass
L03730:       FMT 2*CH(256)
           chk_st$ = str(rm_rec$(),13%,1%)     /* Match found update info */

           if chk_st$ = "2" and reschedBatch$ = "1" then return
           if chk_st$ = "2" then goto L04020     /* Error Condition    */
              delete #3
        REM - Re-Schedule Glass for Re-Make      /* Found in Gls Batch */
              str(rm_rec$(),1%,6%) = str(glass_dte1$,1%,6%) /* Prod. Dte */
              str(rm_rec$(),7%,6%) = str(glass_dte1$,1%,6%) /* Scan. Dte */
              str(rm_rec$(),13%,1%) = rm_st$       /* Chg Status 0 to 1  */
              str(rm_rec$(),14%,8%) = rm_st_time$  /* Time of Stat Change*/
                                                 /* (EWD002) Leave     */
                                                 /* Glass Barcode alone*/
                                                 /* Re-Make No Alone   */
                                                 /* and Reason Code alone*/
              str(rm_rec$(),36%,6%) = date         /* Date of Stat Change*/
                                                 /* (EWD002) 42 - 64   */
                                                 /* Contains remake    */
                                                 /* Scan Date/Time     */
              str(rm_rec$(),242%,5%) = str(lab_rec$(),176%,5%)
                                                 /* Update seq No.     */


              str(rm_rec$(),67%,445%) = str(lab_rec$(),1,445%)  /* (AWD025) */
              str(rm_rec$(),163%,8%)  = so$                     /* (AWD025) */
              goto L03970


REM              INIT(" ")LOGMESSAGE$
REM              LOGMESSAGE$ = "RM_KEY ->  " & RM_KEY$ & " " & STR(LAB_REC$(),191%,128%)
REM              CALL "LOGFILE" (LOGMESSAGE$)
REM              INIT(" ")LOGMESSAGE$
REM              LOGMESSAGE$ = "LAB_REC(1) ->  " & STR(LAB_REC$(),1%,159%)
REM              CALL "LOGFILE" (LOGMESSAGE$)
REM              INIT(" ")LOGMESSAGE$
REM              LOGMESSAGE$ = "LAB_REC(2) ->  " & STR(LAB_REC$(),160%,159%)
REM              CALL "LOGFILE" (LOGMESSAGE$)

              goto L03970
        schedule_glass             /* No match found; Create Glass Data  */
           str(rm_rec$(),1%,6%) = str(glass_dte1$,1%,6%) /* Glass Prod Dt*/
           str(rm_rec$(),7%,6%) = str(glass_dte1$,1%,6%) /* Glass Prod Dt*/
           str(rm_rec$(),13%,1%) = rm_st$          /* Scheduled Glass    */
           str(rm_rec$(),14%,8%) = rm_st_time$     /* Time of Stat Change*/
           str(rm_rec$(),22%,9%) = str(lab_ged$,53%,9%) /* Glass Barcode */
           str(rm_rec$(),31%,3%) = str(lab_rec$(),186%,3%) /* Remake No. */
           str(rm_rec$(),34%,2%) = rm_reason$      /* Glass Reason Code  */
           str(rm_rec$(),36%,6%) = date            /* Date of Stat Change*/
           str(rm_rec$(),42%,2%) = str(lab_rec$(),178%,2%) /* Scanned Sft*/
                                                 /* (EWD002)           */
                                                 /* Time 24 hour clock */
           str(rm_rec$(),44%,8%) = time            /* Scheduled Time     */
           str(rm_rec$(),52%,6%) = date            /* Scheduled Date Today*/
           str(rm_rec$(),58%,3%) = userid$         /* Who Scheduled Glass*/
           str(rm_rec$(),61%,4%) = "    "          /* Completion Calc    */
           str(rm_rec$(),65%,2%) = "  "            /* Growth Area        */

           str(rm_rec$(),67%,445%) = str(lab_rec$(),1,445%)     /* (AWD025) */
           str(rm_rec$(),163%,8%)  = so$                     /* (AWD025) */
                                                   /* Calculated Data    */
                                                   /* Check for INDY     */
/*(PAR000) Complete INDY data automatically */                                                   
           if str(lab_rec$(),190%,1%) = "1" then   /* set as completed   */~
                                            str(rm_rec$(),13%,1%) = "2"

L03970:    write #3, using L03730, rm_rec$(), eod goto L03990
        return
L03990:    errormsg$="(Err)- Updating Glass Primary Database"
           gosub error_prompt
        return
L04020:    errormsg$="(Err)-Re-Schedule of Completed Glass For S.O.= "   ~
                     & so$
           gosub error_prompt
        return

        error_prompt
           error% = 1%
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
                                 /* (EWD010) Sort Bilco by Table Number */
        sort_bilco
                                       /* (lab_key$,1%, 5%) = Counter   */
                                       /* (lab_key$,6%, 1%) = Table No. */
                                       /* (lab_key$,7%, 1%) = S or D    */
                                       /* (lab_key$,8%, 3%) = Blank     */
            call "SHOSTAT" ("Sorting Bilco Glass for Table")

            sort% = 0%
            file% = 10%
            mode% = 1% : gosub OPEN_WORK
            mode% = 3% : gosub OPEN_WORK

            lab_key$, lab_ged$ = all(hex(00))

            read #16,key > lab_key$, using L01470, lab_key$,  lab_ged$,  ~
                               lab_rec$(),  eod goto sort_bilco_done
            goto sb_1
        sort_bilco_next
            read #16, using L01470, lab_key$, lab_ged$, lab_rec$(),      ~
                                                eod goto sort_bilco_done
sb_1:       dt_dept$  = str(lab_rec$(),183%,3%)
            sandwich$ = str(lab_rec$(),115%,10%)   /* Find Glass Strength  */

            sort1$ = str(lab_rec$(),211%,11%)
REM            IF SORT1$ <> "999TEMPERED" THEN GOTO NOT_TEMPSORT2
REM                  CALL "SHOSTAT" ("TEMP SORT 2ND" )  STOP
REM NOT_TEMPSORT2


            dd% = 0%
            dd% = pos(sandwich$ = "4")  /* dd% > 0% then Double Strength */

            gosub find_table_no
            if table_no% = 0% then goto sort_bilco_next

            lab_bilc$ = all(hex(00))
            sort% = sort% + 1%
            convert sort% to str(lab_bilc$,1%,5%), pic(00000)

            str(lab_bilc$,6%,1%) = table_no$

            str(lab_bilc$,7%,1%) = "1"                     /* Single  */
            if dd% <> 0% then str(lab_bilc$,7%,1%) = "2"   /* Double  */

            write #10, using sb_fmt, lab_bilc$, lab_key$, lab_ged$,     ~
                                     lab_rec$(),  eod goto sort_bilco_next
sb_fmt:        FMT CH(10), CH(62), CH(66), 2*CH(223)

            goto sort_bilco_next
        sort_bilco_done

        return



        find_table_no
            table_no% = 0%
            init(" ") readkey$, descr$
            str(readkey$,1%,9%)  = "GLASS11  "
            str(readkey$,10%,4%) = dt_dept$ & "-"
            str(readkey$,14%,1%) = "S"                     /* Single   */
            if dd% <> 0% then str(readkey$,14%,1%) = "D"   /* Double   */
            read #5,key = readkey$, using L03470 , descr$,               ~
                                                 eod goto find_table_exit
            table_no$ = str(descr$,1%,1%)
            table_no% = 1%
        return
find_table_exit
        return

                                                 /* (AWD016) - BEGIN   */
        ASSIGN_GLS_CNTR_SORT
           if rm_flag% = 1% or rm_flag% = 3% then return
/*(AWD018)*/
           if temper_flag$ = "Y" then goto SKIP_DS_CHECK


           init(" ") glscntr_key$
REM           IF STR(SORT1$,4%,2%) = "DS" THEN GOTO SET_DS_KEY
/* (AWD017)  - DS Batch */
           if str(sort1$,4%,2%) = "DS" and ds_batch$ <> "0"           ~
                                                 then goto SET_DS_KEY

SKIP_DS_CHECK:
              glscntr_key$ = sort1$
        return
        SET_DS_KEY
           if str(sort1$,1%,3%) = "047" then goto separateDS
           if str(sort1$,1%,3%) = "007" then goto separateDS
           if str(sort1$,1%,3%) = "049" then goto separateDS
           if str(sort1$,1%,3%) = "005" then goto separateDS
           if str(sort1$,1%,3%) = "002" then goto separateDS
/*(AWD028)*/
/* (AWD030) */
           if str(sort1$,1%,3%) = "014" then goto separateDS
           if str(sort1$,1%,3%) = "017" then goto separateDS
           if str(sort1$,1%,3%) = "018" then goto separateDS

           if str(sort1$,1%,3%) = "028" then goto separateDS
           if str(sort1$,1%,3%) = "027" then goto separateDS
           if str(sort1$,1%,3%) = "048" then goto separateDS
           if str(sort1$,1%,3%) = "036" then goto separateDS
           if str(sort1$,1%,3%) = "019" then goto separateDS
/* (\AWD030) */
REM           IF STR(SORT1$,1%,3%) = "014" THEN GOTO SEPARATEDS

           if str(sort1$,1%,3%)  = "047" then                      ~
                   str(glscntr_key$,1%,11%) = str(sort1$,4%,6%) & "047"
           if str(sort1$,1%,3%) <> "047" then                      ~
                   str(glscntr_key$,1%,11%) = str(sort1$,4%,6%)
           if str(sort1$,6%,4%)  = "SP11" then                     ~
                   str(glscntr_key$,1%,11%) = "DSSP11"

        return
        separateDS
           if str(sort1$,1%,3%)  = "047" then                      ~
                   str(glscntr_key$,1%,11%) = str(sort1$,4%,6%) & "047"
REM Lump all 267s together
           if str(sort1$,1%,3%)  = "007" then                      ~
                   str(glscntr_key$,1%,11%) = str(sort1$,4%,6%) & "007"
           if str(sort1$,1%,3%)  = "049" then                      ~
                   str(glscntr_key$,1%,11%) = str(sort1$,4%,6%) & "007"
           if str(sort1$,1%,3%)  = "005" then                      ~
                   str(glscntr_key$,1%,11%) = str(sort1$,4%,6%) & "007"
           if str(sort1$,1%,3%)  = "002" then                      ~
                   str(glscntr_key$,1%,11%) = str(sort1$,4%,6%) & "007"


/* (AWD030) */

           if str(sort1$,1%,3%)  = "014" then                      ~
                   str(glscntr_key$,1%,11%) = str(sort1$,4%,6%) & "007"
           if str(sort1$,1%,3%)  = "017" then                      ~
                   str(glscntr_key$,1%,11%) = str(sort1$,4%,6%) & "007"
           if str(sort1$,1%,3%)  = "018" then                      ~
                   str(glscntr_key$,1%,11%) = str(sort1$,4%,6%) & "007"


           if str(sort1$,1%,3%)  = "019" then                      ~
                   str(glscntr_key$,1%,11%) = str(sort1$,4%,6%) & "027"
           if str(sort1$,1%,3%)  = "027" then                      ~
                   str(glscntr_key$,1%,11%) = str(sort1$,4%,6%) & "027"
           if str(sort1$,1%,3%)  = "028" then                      ~
                   str(glscntr_key$,1%,11%) = str(sort1$,4%,6%) & "027"
           if str(sort1$,1%,3%)  = "036" then                      ~
                   str(glscntr_key$,1%,11%) = str(sort1$,4%,6%) & "027"
           if str(sort1$,1%,3%)  = "048" then                      ~
                   str(glscntr_key$,1%,11%) = str(sort1$,4%,6%) & "027"
/* (\AWD030) */

           if str(sort1$,6%,4%)  = "SP11" then                     ~
                   str(glscntr_key$,1%,11%) = "DSSP11"
        return



        SORT_RECORDS                             /* (AWD017)        */
            call "SHOSTAT" ("Sorting Records for GLSBLA " )
            file% = 16%
            mode% = 1%    :     gosub OPEN_WORK
            mode% = 3%    :     gosub OPEN_WORK
            init(" ") new_sort$(), part$, lt$, new_ged$, new_rec$(),     ~
                      lab_ged$, lab_rec$(), new_key$, lab_key$
            mat new_sort_cnt% = zer
            max_sort% = 1%
            new_key$, new_ged$ = all(hex(00))  /* GED Glass Primary Key     */

            read #1,key > new_key$, using WORKFILE1 , new_key$, new_ged$,   ~
                           new_rec$(),   eod goto SORT_RECORDS_DONE

WORKFILE1:              FMT CH(62), CH(66), 2*CH(223)              /*PAR000*/
                     goto SORT_RECORDS_FIRST

        SORT_RECORDS_NEXT
            read #1, using WORKFILE1 , new_key$, new_ged$, new_rec$(),   ~
                                    eod goto SORT_RECORDS_DONE


SORT_RECORDS_FIRST:
REM         ===============================================================
REM         records to skip

            if str(new_rec$(),1%,1%) = "E" then goto SORT_RECORDS_NEXT
                                                  /* (EWD007)          */
/* (AWD021) */
REM         IF STR(NEW_GED$,1%,2%) = "ZZ" THEN GOTO SORT_RECORDS_NEXT
/* (AWD025) */
REM  !            IF STR(NEW_GED$,1%,2%) = HEX(FF) THEN GOTO SORT_RECORDS_NEXT
            if str(new_key$,8%,5%) = "00999" then goto SORT_RECORDS_NEXT


            so$       = str(new_rec$(),97%,8%)     /* Sales Order Number*/
            part$     = str(new_rec$(),59%,25%)      /* MFG Part Number   */
            lt$       = str(part$,7%,2%)             /* Liting Code       */
            ultra$    = str(new_rec$(),233%,1%)    /* (AWD033)          */
            patio$    = str(new_rec$(),234%,1%)    /* (AWD034) */
REM  !            call "SHOSTAT" (" DT DATE ") stop
            dtDte$    = str(new_rec$(),235%,6%)    /* (AWD034) */
REM  !            dtDate$   = str(new_rec$(),235%,6%)    /* (AWD034) */
REM  !            call "DATEFMT" (dtDate$)
REM  !            init(" ")logMessage$
REM  !            logMessage$ = "NEW GED ->  " & str(new_ged$,1,66) & " " & so$
REM  !            call "LOGFILE2" (logmessage$)
REM  !            init(" ")logMessage$
REM  !            logMessage$ = "NEW_REC(2) ->  " & str(new_rec$(),160%,159%) & " " & dtDate$
REM  !            call "LOGFILE2" (logmessage$)

REM            IF LT$ > "82" AND LT$ < "88" THEN GOTO SORT_RECORDS_NEXT
REM            IF LT$ > "96" AND LT$ <= "99" THEN GOTO SORT_RECORDS_NEXT

REM         -------------------------------------------------------------
REM         this is NOT the screen selection for GED/Bilco
                                                 /* 0 = GED Only       */
                                                 /* 1 = Bilco only     */
                                                 /* 2 = Both GED/Bilco */
            if str(new_rec$(),189%,1%) = "2" then goto OUTSIDE_PURCHASE
                                            /* (2) = Both GED and Bilco */
            if str(new_rec$(),189%,1%) <> "1" then goto SORT_RECORDS_NEXT
                                           /* Not Zero then must be    */
                                           /* BILCO Glass              */
REM         ---------------------------------------------------------------
OUTSIDE_PURCHASE:
            gosub CHECK_GLASS             /* See if Outside Purchase   */
                                          /* Skip Special Glass        */
            if check% = 1% then goto SORT_RECORDS_NEXT


            gosub CHECK_GLASS12                  /* (EWD011)             */
            if check1% = 1% then goto create_next

REM         ---------------------------------------------------------------
REM         Do not get pre-cut glass

            lab_fil$ = str(new_rec$(),50%,9%)
/* (AWD020) */
            if rm_flag% = 1% or rm_flag% = 3% then goto not_remakes
            if str(lab_fil$,1%,6%) =  "STOCK1" then goto SORT_RECORDS_NEXT

not_remakes:

REM         ================================================================
REM         ----------------------------------------------------------------
REM         get sort information
REM         ---------------------------------------------------------------
/* (AWD025) */
REM  !      SORT$   = STR(NEW_GED$,1%,2%)          /* Special Sort Code */
REM  !            intercept$ = str(new_key$,1%,2%)
            intercept$ = str(new_rec$(),241%,2%)
            sort$   = str(new_key$,8%,5%)
            sort1$  = " "
            sort% = 0%
            convert sort$ to sort%, data goto bad_sort

bad_sort:

REM  !      GOSUB UNPACK_SORT_CODE                  /* (AWD021) */

            if sort% < 100 then goto no_showstat
REM            call "SHOSTAT" ( " I AM HERE!!! " & sort$)  stop

no_showstat

REM         CONVERT SORT$ TO SORT%, DATA GOTO BAD_SORT

REM BAD_SORT

/*(AWD025) */
REM  !      SORT1$  = STR(SS$(SORT%),1%,11%)           /* The entire sort */
            sort1$  = str(new_rec$(),211%,11%)
REM            if sort1$ <> "999Tempered" then goto NOT_TEMPSORT
REM                  call "SHOSTAT" ("TEMP SORT 1st" ) stop
REM NOT_TEMPSORT

            gosub ASSIGN_GLS_CNTR_SORT             /* (AWD016) */
            gosub FIND_SORT
            lab_key$ = str(new_key$,1%,62%)        /* (AWD025) */
            lab_ged$ = str(new_ged$,1%,66%)
            lab_rec$() = str(new_rec$(),1%,318%)

            str(lab_rec$(),211%,11%) = str(sort1$,1,11)
            str(lab_rec$(),222%,11%) = str(glscntr_key$,1,11)
            str(lab_rec$(),233%,1%)  = ultra$
            str(lab_rec$(),234%,1%)  = patio$
            str(lab_rec$(),235%,6%)  = dtDte$
            str(lab_rec$(),241%,2%)  = intercept$


              write #16, using FMTNEWSORTREC, lab_key$, lab_ged$,     ~
                             lab_rec$(), eod goto SORT_RECORDS_ERROR

FMTNEWSORTREC:             FMT CH(62), CH(66), 2*CH(223)

                goto SORT_RECORDS_NEXT

        SORT_RECORDS_DONE
REM           call "SHOSTAT"( "APCPLC45 SORT_RECORDS_DONE " )  stop
        return
        SORT_RECORDS_ERROR
           init(" ") so$
           so$       = str(new_rec$(),97%,8%)
           errormsg$ ="(Err)-Resorting Data = "   ~
                     & so$
           gosub error_prompt
        return


        FIND_SORT
/* (AWD025) */
               str(glscntr_key$,12%,2%) = intercept$
               str(sort1$,12%,2%)        = intercept$
REM          IF STR(GLSCNTR_KEY$,1,13) <> " " AND        ~
                   STR(SORT1$,1,13) <> " " THEN GOTO NO_SORT_ERR
REM            call "SHOSTAT" ("HERE AT NEW SORT1 ")  stop

REM NO_SORT_ERR

               if lt$ > "82" and lt$ < "88" then return
               if lt$ > "96" and lt$ <= "99" then return
REM     --------------------------------------------------------------------
REM        find the new gls sort in arrary and add one to counter
REM     --------------------------------------------------------------------
           for sort% = 1% to max_sort%
              if glscntr_key$ = new_sort$(sort%) then goto FOUND_SORT
           next sort%

           sort%     = max_sort%
           max_sort% = max_sort% + 1%

           new_sort$(sort%) = glscntr_key$

REM        return
        FOUND_SORT
           new_sort_cnt%(sort%) = new_sort_cnt%(sort%) + 1%


        return

        UPDATE_CNTR

            for sort% = 1% to max_sort%

               if glscntr_key$ = new_sort$(sort%) then goto GET_CNTR
            next sort%

/* (AWD021)  change from 200 to 256 error */
REM  !        SORT% = 256%               /* No SORT, BETTER NOT HAPPEN */
            sort% = 999%

        GET_CNTR
        return


/* (AWD021) */
REM  !  UNPACK_SORT_CODE
REM  !      GET STR(SORT$,1,2), USING L63510, SORT%
REM  ! L63510:    REM  !         FMT BI(2)
REM  !      CONVERT SORT% TO SORT$, PIC(00)
REM  !  RETURN
/* (/AWD021) */


        OPEN_WORK

            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#file%,mode$, 500%, f2%)
            if f2% <> 0% then goto OPEN_WORK_ERR

        return
OPEN_WORK_ERR:
        errormsg$ = "Can Not Open AWDPLNWK"
        gosub error_prompt

        return
        delete_work

            call "FILEBGON" (#10)
        return

/* (AWD027)*/
        lookup_3p
            tripPane% = 0%
            init(" ") readkey$
            str(readkey$,1%,9%) = "PLANTRIPL"
            str(readkey$,10%,2%) = ty$
            read #5,key = readkey$,eod goto not_3p
              tripPane% = 1%
        not_3p
        return
/* (\AWD027) */

        exit_program
            call "FILEBGON" (#16)
        end



