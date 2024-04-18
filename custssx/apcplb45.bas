        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLB45 - Program (APCPLA45)        *~
            *  Creation Date     - 12/04/96 - File - (@???GED@)         *~
            *  Last Modified Date- 02/18/2019                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Create GED Glass Bridge File with    *~
            *                      New Sort to put Obscure, Tempered,   *~
            *                      and Special Liting Codes (83-89)     *~
            *                      and (94-99) at the end.              *~
            *                                                           *~
            *Note  - Break on all Spacer Changes and Start a New Batch. *~
            *        Also on Switch over From Top's to Bot's for        *~
            *        all Departments/Product Lines.                     *~
            *                                                           *~
            *      - New Sort Codes from 01 thru 43. Glass is broken    *~
            *        down into Department Areas.                        *~
            *                                                           *~
            *      - Glass Re-Make Status Codes (UPDATE_REMAKE)         *~
            *        0 = Selected for Glass Re-Make - Scanned Re-Make   *~
            *        1 = Scheduled Glass            - New Glass 1st Time*~
            *        2 = Completed Scheduled Glass - Scanned out of Oven*~
            *                                                           *~
            *                                                           *~
            *      - Bridge File Names                      (EWD003)    *~
            *        (@GLSGED@) - Normal Production Glass               *~
            *        (@RMKGDK@) - Remake Both Glass Bridge File         *~
            *        (@RMKGD1@) - Remake Production Glass Bridge File   *~
            *        (@RMKGD2@) - Remake In House Glass (Not Used)      *~
            *        (@RMKGD5@) - Vinyl Line Remake Only  (AWD013)      *~
            *        (@GLSTMP@) - Tempered Production                   *~
            *        (@RMKTMP@) - Tempered Remake                       *~
            *        (@GLSPRE@) - Pre-Cut Production  STOCK1 Flag       *~
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
            * 02/07/97 ! Correct Record Format - Scan Status      ! RHH *~
            * 02/11/97 ! Mods to Use the Glass Production Date for! RHH *~
            *          !   Scanning and the cutting of Glass.     !     *~
            * 04/23/97 ! Mods to allow for New 1 Inch Grid.       ! RHH *~
            * 06/12/97 ! Mods to Support New Family Departments   ! RHH *~
            *          !   (049) and (052).                       !     *~
            * 11/14/97 ! Mod for Upgrade to Release R6.04.03      ! RHH *~
            * 01/14/98 ! Mod to By-Pass special Glass that is     ! RHH *~
            *          !   Purchased Outside. (PLANGLOUT), based  !     *~
            *          !   on LITING Codes. Also new Table for    !     *~
            *          !   Argon Gas. (PLANARGON)                 !     *~
            * 03/26/98 ! Y2K                                      ! LDJ *~
            * 04/09/98 ! Mod for New Color'd grid Codes C$()      ! RHH *~
            * 07/27/98 ! (EWD001) - Mod for new glass remake      ! RHH *~
            *          ! reason codes (26, 28, 30) Those remakes  !     *~
            *          ! go into a Seperate Batch File @RMKGD?@   !     *~
            * 12/07/98 ! (EWD002) Clean-up mods for new remake    ! RHH *~
            *          !   selections and for (INDY)              !     *~
            * 01/11/99 ! (EWD003) Mods to Seperate Bridge Files   ! RHH *~
            * 05/06/99 ! (EWD004) Mod to change Batch Counter to  !     *~
            *          !   three digits.                          !     *~
            * 08/09/99 ! (EWD005) Mod to Glass for Grid Bumpers   ! RHH *~
            * 09/07/99 ! (EWD006) Mod for BW Grid for '124' and   ! RHH *~
            *          !   '134'                                  !     *~
            * 12/20/99 ! (EWD007) Mod for Sort Change             ! RHH *~
            * 10/20/00 ! (EWD008) Mod to correct color code for   ! CMG *~
            *          !           'F'.                           !     *~
            * 05/23/03 ! (EWD009) Mod for 3/4' Grid in GLSGED File! CMG *~
            * 09/22/03 ! (EWD010) Mod for tempered glass.         ! CMG *~
            * 05/18/04 ! (EWD011) Mod for Prairie Grid            ! CMG *~
            * 06/22/04 ! (EWD012) Mod to add additional comments  ! CMG *~
            *          !             to GLSGED file               !     *~
            * 12/10/04 ! (AWD013) Mod for vinyl line remakes only ! CMG *~
            * 02/07/05 ! (AWD014) Mod for precut glass to be in   ! CMG *~
            *          !            new file                      !     *~
            * 05/16/05 ! (AWD015) Mods to rearrange gls sort to so! CMG *~
            *          !   DS is sorted together to put in batchs !     *~
            *          !   together                               !     *~
            * 05/30/05 ! (AWD016) Mod to add M & O to cont grid   ! CMG *~
            * 06/27/05 ! (AWD017) Mod to have a look ahead batch  ! CMG *~
            * 08/15/05 ! (AWD018) Mod to re-count batch           ! CMG *~
            * 01/01/06 ! (PAR000) CR347 Mods for new sub part     ! CMG *~
            * 03/19/06 ! (AWD019) Mods for krypton                ! CMG *~
            * 07/11/06 ! (AWD020) mods for tempered gridmate      ! CMG *~
            * 03/29/07 ! (AWD021) Mods for Bumpers                ! CMG *~
            * 05/03/07 ! (AWD022) mods for 18                     ! CMG *~
            * 08/02/07 ! (AWD023) mod to turn off < 50            ! CMG *~
            * 08/02/07 ! (AWD024) mod to put pre-cut in remakes   ! CMG *~
            * 08/14/07 ! (AWD025) mod for 58 grid                 ! CMG *~
            * 10/03/07 ! (AWD026) mod for perimeter par           ! CMG *~
            * 03/05/08 ! (AWD027) mod for SDL                     ! CMG *~
            *03/02/2009! (AWD028) mod for larger sort, hex        ! CMG *~
            *05/21/2009! (AWD029) florida remake                  ! CMG *~
            *07/07/2009! (AWD030) mod to take pre-cut out of remak! CMG *~
            *07/12/2009! (AWD031) mod for clay color 'CY'         ! CMG *~
            *11/04/2009! (AWD032) mod to change order record      ! CMG *~
            *11/04/2009! (AWD033) mod for ulta batches            ! CMG *~
            *05/06/2010! (AWD034) mod for patio flag and DT prdDte! CMG *~
            *06/15/2010! (AWD035) mod for new laminates Cherry    ! CMG *~
            *          !       and White/Clay                     !     *~
            *02/14/2011! (AWD036) mods for intercept types        ! CMG *~
            *10/26/2011! (AWD037) mods for liting codes TD & TP   ! CMG *~
            *11/22/2011! (AWD038) mods for new tempered process   ! CMG *~
            *12/08/2011! (AWD039) mod for cutback and offset chng ! CMG *~
            *03/23/2012! (AWD040) mod for new grid color codes    ! CMG *~
            *11/19/2012! (AWD041) mod for valance                 ! CMG *~
            *09/15/2014! (AWD042) mod to use grid color instead of! CMG *~
            *          !    frame color                           !     *~
            *09/24/2014! (AWD043) mods for valance grid           ! CMG *~
            *09/25/2014! (AWD044) mods for DS batches             ! CMG *~
            *02/26/2015! (AWD045) mods for laminate               ! CMG *~
            *05/18/2015! (IM8022) mods for laminate               ! CMG *~
            *08/15/2015! (SR67936) mod for Valance Grid Inventory ! CMG *~
            *02/17/2015! (SR72107) mod to add comment to intercept! CMG *~
            *10/14/2016! (CR441) Grid Color Modification          ! CMN *~
            *03/12/2018! (CR800) (CR607) batch sorting            ! CMN *~
            *02/18/2019! (CR1931) modify to add glss warranty to  ! CMN *~
            *          !    merge file                            !     *~
            *02/18/2019! (CR1929) modify to add PlyGem Arg from   ! CMN *~
            *                subpart pos 7 GLASSACC               !     *~
            *04/16/2019! (CR1987) mod for STC glass               ! CMN *~
            *05/10/2019! (CR2018) change for DS TT / BB sorting   ! CMN *~
            *          !   dept 071                               !     *~
            *07/22/2019! (CR2133) change for 2SL & 3SL perimeter  ! CMN *~
            *          !   prairie                                !     *~
            *10/24/2019! (CR2221) add style DW for Mdl 521 Valance! CMN *~
            *10/30/2019! (CR2260) nc stc changes schema = 1%      ! CMN *~
            *02/15/2021! (CR2773) precut tempered                 ! CMN *~    
			*09/30/2022! CR3166 GED file changes for Erdman       ! RDB *~
            *          !      Phase 1 only remake produce comments! RDB *~			
            *11/28/2022! CR3166 GED file phase 2                  ! RDB *~
			*01/19/2023! CR3235 Add Glass width height to comments! RDB *~
            *************************************************************

        sub "APCPLB45" (size%,           /* Specified Batch Size      */ ~
                        scr_sel$,        /* Screen Selection (EWD003) */ ~
                        glass_dte$,      /* Glass Production Date     */ ~
                        scr_dte$,        /* Planned Production Date   */ ~
                        scr_dte1$,       /* Planned Prod Date Unformat*/ ~
                        file$,           /* Name of Optimized File 0  */ ~
                        bat$,            /* Number of Batches Created */ ~
                        rm_flag%,        /* Remake Flag 0% or 1%      */ ~
                        ss$(),           /* Sort Codes      (EWD012)  */ ~
                        ss_temp$(),      /* (AWD034) Tempered Sort    */ ~
                        pass%,           /* Pass through pgm  (AWD014)*/ ~
                        ds_batch$,       /* DS Batch flag    (AWD019) */ ~
                        tempScrn%,       /* (AWD034) tempered         */ ~
                        lamnScrn%,       /* laminate (AWD045)         */ ~
                        schema%,         /* Schema (CR2018)           */ ~
                        #2,              /* (GENCODES) Master Tables  */ ~
                        #6,              /* (APCPLNWK) Work File      */ ~
/*(EWD010)*/            #8)              /* (APCPLNGR) Glass Remake Fl*/

                                         /* (EWD001) - Mod 07/27/98   */
                                         /* (EWD003) - Mod 01/11/99   */
                                         /* (AWD013) - Mod 12/10/04   */
                                         /* (AWD014) - Mod 02/07/05   */

        dim glass_dte$8, glass_dte1$10,  /* Completion Date   (Y2K, LDJ) */ ~
            temp_date$10,temp_date2$10,  /* Working storage   (Y2K, LDJ) */ ~
            scr_dte$8,                   /* Production Date           */ ~
            scr_dte1$8,                  /* Production Date Unform    */ ~
            file$20,                     /* Name of Optimized File    */ ~
            sav_file$20,                 /* Save File Name            */ ~
            bat$3,                       /* Number of Batches (EWD004)*/ ~
/*AWD036*/  lab_key$62,                  /* GED Primary Key           */ ~
            lab_ged$66,                  /* GED Primary Key           */ ~
/*PAR000*/  lab_rec$(2%)223,             /* Main Data Record          */ ~
/*AWD036*/  new_key$62,                  /* NEW GED Key               */ ~
            new_ged$66,                  /* New File Record   (AWD018)*/ ~
/*PAR000*/  new_rec$(2%)223,             /* New file record   (AWD018)*/ ~
            x$1, y$2,                    /* X = Quote, Y = Comma Space*/ ~
            bat_no$3, sav_bat$3,         /* Batch Number (EWD004)     */ ~
            cnt_ord$3, sort$5,           /* Count Order Headers       */ ~
            schedSort$3,                 /* GED Sched Sort            */ ~
            cnt_dtl$3, sav_sort$5,       /* Count Order Details       */ ~
/*AWD038*/  sort2$11, sav_sort2$11,      /* Tempered special sort     */ ~
            intercept$2, sav_intercept$2,/* (AWD036)                  */ ~
            space_d$10,                  /* Spacer Description        */ ~
            spacer$6, sav_spacer$6,      /* Spacer with copy          */ ~
            sandwich$10,                 /* Glass Sandwich            */ ~
            width_d$8,                   /* Width Size Deciamal       */ ~
            height_d$8,                  /* Height Size Decimal       */ ~
            t_k$6,                       /* Overall Thickness         */ ~
            gs$2,                        /* Blank or "G1"             */ ~
            lk$1,                        /* Lock Codes                */ ~
            hng$2,                       /* Hinge Code (CR2133)       */ ~
            color$10, cl$2,              /* Color Code / Table        */ ~
            framecl$1,                   /* Frame Color               */ ~
            gridcl$2,                    /* Grid Color                */ ~
            muttin$8, gd$2, lt$2,        /* Grid Code/Liting          */ ~
            w_adj$6,                     /* Width Adjustment Code     */ ~
            h_adj$6,                     /* Height Adjustment Code    */ ~
            ged_txt$40,                  /* Field 16 - Text           */ ~
            bat_hdr$220,                 /* Batch Header Record       */ ~
            bat_order$220,               /* Batch Order Record        */ ~
/*AWD019*/  bat_dtl$(2%)192,             /* Batch Detail Record       */ ~
/*AWD019*/  bat_dtl1$(2%)192,            /* Addition Detail Record    */ ~
/*AWD028*/  ss$(999%)11,                 /* Glass Sort        (EWD012)*/ ~
            ss_temp$(999%)11,            /* Tempered Glass Sort (AWD034)*/~
            model$3,                     /* Model Code                */ ~
            tty$2, temp$1,               /* Glass Type Code           */ ~
            ssq$5,                       /* Product Seq No.           */ ~
            v$1,                         /* T or B  VIEW              */ ~
            part$25, load$5, so$8,       /* Part No, Load No., S.O.   */ ~
            ff$8, errormsg$79,           /* Batch File Name           */ ~
            readkey$24,                  /* GENCODES                  */ ~
            desc$30,                     /* GENCODES Desc     (EWD011)*/ ~
            userid$3,                    /* User Id       (EWD002)    */ ~
            scr_sel$1,                   /* Screen Selection (EWD003) */ ~
            hdr$40, error$(5%)50,        /* ASKUSER Header Text       */ ~
/* CR3166 */                                                             ~			
            txt_inside_outside$7,        /* INSIDE/OUTSIDE Text       */ ~	
            wnd_color$6,                 /* Window color desc         */ ~	
            txt_glass$30,                /* Glass description         */ ~	
            wnd_color_code$1,            /* Window color code         */ ~	
            glass_code$2,                /* Glass code                */ ~	
            view$3,                      /* Top/Bot                   */ ~
			interdesc$5,                 /* Intercept description     */ ~
			sh_type$1,                   /* Label Contour Flag        */ ~
			cmt_prod_date$10,            /* Formatted Prod Date       */ ~
		    gl_color$6,                  /* Product Color             */ ~
			makenum$3,                   /* Printed make number       */ ~
			gl_breatht$1,                /* Flag breathing tube       */ ~
			prt_num$3,                   /* Printed Make Number       */ ~
			cmt_f9$40,                   /* Formatted comment 9       */ ~
			cmt_f11$40,                  /* Formatted comment 11      */ ~   
			cmt_f12$40,                  /* Formatted comment 12      */ ~   
			cmt_f13$40,                  /* Formatted comment 13      */ ~
			cmt_f14$40,                  /* Formatted comment 14 3235 */ ~  			
			cmt_f15$40,                  /* Formatted comment 15      */ ~   
            file_nbr$3,                  /* CR3274 File number Char   */ ~			
            msg$(3%)79,                  /* ASKUSER Info Text         */ ~
            lab_fil$9                    /* Label Filler for Stock    */

        dim rm_key$23, rm_rec$(2%)256,   /* Remake Key and Record      */~
            rm_st$1,                     /* Remake Production Day      */~
            rm_st_time$8, chk_st$1,      /* Remake Stat Time Change    */~
            rm_reason$2,                 /* Remake Reason Code         */~
            rm_st_dte$6                  /* Remake Status Date         */

        dim f2%(20%),                    /* = 0 if the file is open    */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(20%)20                 /* Text from file opening     */

        dim dept$3,                      /* Department         (AWD015)*/~
            strength$2,                  /* Strength           (AWD015)*/~
            sort1$13,                    /* Sort               (AWD015)*/~
            p_sort$8,                    /* Partial Sort       (AWD015)*/~
            sav_p_sort$8,                /* Save Partial Sort  (AWD015)*/~
            sav_sp$4,                    /* Save Spacer        (AWD015)*/~
            sp$4                         /* Spacer             (AWD015)*/
/* (AWD036) mods to field lengths and array size */
        dim glscntr_key$13,              /* Gls Counter Readkey(AWD017)*/~
            glscntr_sort$13,             /* Gls Counter Sort   (AWD017)*/~
            new_sort$(999)13,            /* New Glass sort     (AWD018)*/~
            new_sort_cnt%(999),          /* New Glass Counter  (AWD018)*/~
            mode$5                       /* Work File Mode     (AWD018)*/

        dim ds_batch$1                   /* DS Batching?       (AWD019)*/

        dim                              /* (PAR000)                   */~
            field1$1,                    /* New Part Field 1 GRDTYPE   */~
            field2$1,                    /* New Part Field 2 GRDSIZE   */~
            field3$1,                    /* New Part Field 3 GRDCOLOR  */~
            field4$1,                    /* New Part Field 4 HARDWARE  */~
            field5$1,                    /* New Part Field 5 FOAM      */~
            field6$1,                    /* New Part Field 6 Casing    */~
            field7$1,                    /* New Part Field 7 SampColor */~
            field8$1,                    /* New Part Field 8 Grid Type */~
            field9$1,                    /* New Part Field 9 Grid Size */~
            field10$1,                   /* New Part Field10 Grid Color*/~
            field11$1,                   /* New Part Field11 IGridColor*/~
            test$150

        dim                                                             ~
/*AWD020*/  temper_flag$1

        dim ultra$1,                     /* Ultra Flag (AWD033)   */~
            patio$1,                     /* Patio Flag (AWD034)   */~
            dtDte$6,                     /* APCPLNDT Dte (AWD034) */~
            showMessage$100              /* ShoStat MEssage  (AWD034) */

        dim spacer_type$2                /* (AWD036) spacer type   */
        dim logMessage$256               /* (AWD036) log message   */
/* (AWD038) */
        dim interoffset$7,               /* Offset by intercept    */~
            interadj$7                   /* Adjustment by intercept*/
/*(AWD041) */
        dim style$10,                   /* DH or SH               */~
            wnd_height$3,               /* Window Height          */~
            script$8,                   /* Script                 */~
            glstype$20,                 /* Glass Type             */~
            painted$30,                 /* Painted Colors         */~
            glsbar$9                    /* Glass Barcode (CR2133) */


            call "EXTRACT" addr("ID", userid$)
            glass_dte1$ = glass_dte$
            if len(glass_dte1$) = 10% then               /* (Y2K, LDJ) */~
                call "DATUFMTC" (glass_dte1$)            /* (Y2K, LDJ) */~
            else                                         /* (Y2K, LDJ) */~
            call "DATUNFMT" (glass_dte1$)


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #16 ! AWDPLNWK ! Glass Work File (RESORT DATA)            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

/* (AWD036) modifications to key length and rec size to match #6 */

            select #16, "AWDPLNWK",                                      ~
                        varc,     indexed,  recsize =   574,             ~
                        keypos =    1, keylen =   128

            select #03, "@GLSINY@", consec, recsize = 384
            select #04, "@RMKGDK@", consec, recsize = 384
            select #05, "@RMKTMP@", consec, recsize = 384
            select #10, "@GLSGED@", consec, recsize = 384
            select #11, "@GLSTMP@", consec, recsize = 384
            select #17, "@GLSVLA@", consec, recsize = 384
            select #18, "@RMKVLA@", consec, recsize = 384
            select #19, "@PATIO@",  consec, recsize = 384
            select #21, "@RMKPAT@", consec, recsize = 384
            select #23, "@GLSLAM@", consec, recsize = 384
            select #24, "@RMKLAM@", consec, recsize = 384
            select #25, "@GLSTLA@", consec, recsize = 384
            select #26, "@RMKTLA@", consec, recsize = 384
            select #27, "@PRETMP@", consec, recsize = 384
            select #28, "@RMKTPR@", consec, recsize = 384


            spacer%  = 0%               /* Loop through spacers */
            valance% = 0%               /* valance */
            rmk%     = 0%               /* remake flag$   */
            init(" ") sav_file$
            sav_file$ = file$           /* Note - ff% = Primary File  */
            temper_flag$ = "N"
            if tempScrn% = 1% then goto tempScrnFiles
              ff$ = "@GLSGED@" : ff% = 10%

              if scr_sel$ <> "8" then goto notAnnVal
               ff$ = "@GLSVLA@" : ff% = 17%
               goto openFile
notAnnVal:
            if scr_sel$ <> "4" then goto notAnnLamn
               ff$ = "@GLSLAM@" : ff% = 23%
               goto openFile
notAnnLamn:
            if scr_sel$ <> "7" then goto notAnnLamnRmk
               ff$ = "@RMKLAM@" : ff% = 24%
               rmk% = 1%
               goto openFile
notAnnLamnRmk:

            if scr_sel$ <> "3" then goto notAnnRmk
               ff$ = "@RMKGDK@" : ff% = 4%
               rmk% = 1%
               goto openFile

notAnnRmk:
            if scr_sel$ <> "9" then goto notRmkVal
               ff$ = "@RMKVLA@" : ff% = 18%
               rmk% = 1%
               goto openFile

notRmkVal:  if scr_sel$ <> "6" then goto notAnnIndy
               ff$ = "@GLSINY@" : ff% = 3%
               goto openFile
notAnnIndy:

               goto openFile            /* End of Annealed FILES */
REM====================================
REM      TEMPERED FILES
REM====================================

tempScrnFiles:
            temper_flag$ = "Y"
            ff$ = "@GLSTMP@" : ff% = 11%
            if scr_sel$ <> "5" then goto notPatio
               ff$ = "@PATIO@" : ff% = 19%
               goto openFile
notPatio:
            if scr_sel$ <> "3" then goto notTempRmk
               ff$ = "@RMKTMP@" : ff% = 5%
               rmk% = 1%
               goto openFile
notTempRmk:
            if scr_sel$ <> "7" then goto notStockPatioRmk
               ff$ = "@RMKPAT@" : ff% = 21%
               rmk% = 1%
               goto openFile
notStockPatioRmk:
            if scr_sel$ <> "B" then goto notTempLam
               ff$ = "@GLSTLA@" : ff% = 25%
               goto openFile
notTempLam:
            if scr_sel$ <> "C" then goto notTempLamRmk
               rmk% = 1%
               ff$ = "@RMKTLA@" : ff% = 26%
               goto openFile

notTempLamRmk:
/*(CR2773)*/
            if scr_sel$ <> "4" then goto notTempStock
               ff$ = "@PRETMP@" : ff% = 27%
               goto openFile

notTempStock:
            if scr_sel$ <> "6" then goto notTempStockRmk
               rmk% = 1%
               ff$ = "@RMKTPR@" : ff% = 28%
               goto openFile

notTempStockRmk:
/*(\CR2773) */
            temper_flag$ = "N"    /* No Tempered File Selected */

L00055:

openFile:
            call "OPENFILE" (#ff%, "IO   ", f2%(ff%), rslt$(ff%), axd$ )

            if f2%(ff%) <> 0% then goto L01200
               gosub file_exists          /* Uses ff$ for Error Display*/
               if comp% <> 16% then goto L01160
                  call "FILEBGON" (#ff%)
                  goto L01200
L01160:        close #ff%                 /* Re-Open in Append Mode    */
               call "OPENFILE" (#ff%,"EXTND",f2%(ff%),rslt$(ff%),axd$)
               goto L01300

L01200:     str(rslt$(ff%),1%,6%)  = "OUTPTP"
            str(rslt$(ff%),7%,8%)  = "00001000"
            str(rslt$(ff%),15%,3%) = "100"
            str(rslt$(ff%),18%,3%) = "100"
            call "OPENFILE" (#ff%,"OUTPT",f2%(ff%),rslt$(ff%),axd$ )
                                          /* ff% =10% Normal Glass     */
L01300:     call "SHOSTAT" ("Creating GED Glass File for "&scr_dte$)
                                                  /* (AWD018)          */
            painted$ = "5,I,J,K,L,M,N,O,P"
            cnt_ord%,  cnt_dtl% = 0%      /* Set Channel for @GLSGED@  */
            if ff% = 17% then cnt_ord% = 500%  /* (AWD014)starting order num*/
            cnt_ord1%, cnt_dtl1% = 0%
            ds%, ts, qs%, tb%, tt%, bb%, dstt%, dsbb% = 0%   /* (CR2018) */

         NEXT_SPACER_TYPE
            gosub SORT_RECORDS    /* Sort Actual Records to be in File */

REM CALL "SHOSTAT" ("FINISHED SORTING RECORDS " )  STOP

            init(" ") sort$, sav_spacer$, lab_fil$, p_sort$, sav_p_sort$,    ~
                       dept$, strength$, sort1$, intercept$, sav_intercept$, ~
                       sort2$, sav_sort2$, logMessage$, spacer_type$, glstype$,~
                       glsbar$ /* (CR2133) (AWD015) (AWD036) */

            x$ = bin(34%,1)               /* Suff a '"' Quote into X$  */
            y$ = ", "                     /* Set to ',' = Comma        */
            error$(1%) = "(Error)-Writing Batch Header Record? (1)"
            error$(2%) = "(Error)-Writing Batch Order Record?  (2)"
            error$(3%) = "(Error)-Writing Order Detail Record? (3)"
            error$(4%) = "(Error)-Writing End of File Record? (4) "
            sav_bat$ = "XXX"
            lab_key$, lab_ged$ = all(hex(00)) /* GED Glass Primary Key     */

            read #16,key > lab_key$, using L01520 , lab_key$, lab_ged$, ~
                                  lab_rec$(), eod goto create_done
            gosub write_batch_header
            sav_intercept$ = str(lab_rec$(),241%,2%)

            sav_sort$ = str(lab_key$,8,5)
            sav_spacer$ = str(lab_ged$,3%,6%)
/* (AWD038) */
            sav_sort2$ = str(lab_rec$(),211%,11%)
            sav_p_sort$ = "XXXXXXXX"
            sav_sp$ = "XXXX"
            glscntr_sort$ = "XXXXXXXXXXXXX"
            schedSort$ = "XXX"
            goto L01560
        create_next
            read #16, using L01520 , lab_key$, lab_ged$, lab_rec$(),       ~
                                                     eod goto create_done
L01520:          FMT CH(62), CH(66), 2*CH(223)   /*(AWD036) */
L01560:

/* (AWD036) */                                          /* The entire sort */
REM CALL "SHOSTAT" ("CHECK SORT1 AND GLSCNTR_KEY" ) STOP
/* (AWD038) */
            awdschgl% = 0%
            if str(lab_key$,3%,5%) = "99999" then awdschgl% = 1%
            init(" ") sort2$
            if awdschgl% = 1% then sort2$ = str(lab_rec$(),211%,11%)
/* (\AWD038) */
            sort1$  = str(lab_rec$(),211%,11%) & str(lab_rec$(),241%,2%)
            glscntr_key$     = str(lab_rec$(),222%,11%) &      ~
                                  str(lab_rec$(),241%,2%)

            if glscntr_sort$ = "XXXXXXXXXXXXX" then  /* (AWD017) */   ~
                                        glscntr_sort$ = glscntr_key$
REM IF SCHEDSORT$ = "XXX" THEN SCHEDSORT$ = STR(LAB_REC$(),257%,3%)
            if schedSort$ = "XXX" then schedSort$ = str(lab_rec$(),290%,3%)
            gosub UPDATE_CNTR
            glsbar$ = str(lab_ged$,53%,9%)                  /* (CR2133) */
                                                   /* (AWD017)   - END */
            gosub check_remake                    /* ff% Has the Approp*/
                                                  /*  file Channel to  */
                                                  /*  Update           */
            gosub unpack_data
            gosub update_remake

            if lt$ > "82" and lt$ < "88" then goto create_next
            if lt$ > "96" and lt$ <= "99" then goto create_next
            if lt$ = "V0" then goto create_next

               if error% = 1% then goto create_next
               gosub write_order_header
               gosub write_detail
            new_sort_cnt%(sort%) = new_sort_cnt%(sort%) - 1%
            goto create_next
        create_done
/*(AWD036) mod for different spacer systems */
            gosub write_batch_trailer
            close #ff%
        goto exit_end


        write_batch_header
REM CALL "SHOSTAT" (" WRITE BATCH HEADER " ) STOP
            str(file$,16%,5%) = "(GED)"
            if ff% = 4% then str(file$,16%,5%) = "(GDK)"
            if ff% = 1% then str(file$,16%,5%) = "(GD1)"
            if ff% = 3% then str(file$,16%,5%) = "(GD2)"
            if ff% = 12% then str(file$,16%,5%) = "(GD5)"   /* (AWD013) */
            err% = 1%                                  /* Set Error Code */
            init(" ") bat_hdr$, bat_dtl1$()
            str(bat_dtl1$(),1%,480%) = " "             /* (AWD019) */
            str(bat_hdr$,1%,6%)   = "<V1.0>"
            str(bat_hdr$,7%,2%)   = y$                 /* Comma Space  */
            str(bat_hdr$,9%,1%)   = x$                 /* Set Quote    */
            str(bat_hdr$,10%,8%)  = scr_dte$
            str(bat_hdr$,18%,9%)  = " - AWD - "
            str(bat_hdr$,27%,20%) = file$      /* Entered in 'ACPLA45' */
            str(bat_hdr$,47%,1%)  = x$                 /* Set Quote    */
            str(bat_hdr$,48%,173%) = " "

* CMG changed
            write #ff%, str(bat_hdr$,1%,220%) & str(bat_dtl1$(),1%,164%), ~
                  eod goto L01930

            cnt_dtl% = 0%
        return
L01930:     errormsg$ = error$(err%)
            gosub error_prompt
        return

        write_batch_trailer
REM CALL "SHOSTAT" (" WRITE  " )  STOP
            init(" ") bat_hdr$, bat_dtl1$()      /* End of File Record */
            str(bat_dtl1$(),1%,384%) = " "
            err% = 4%
            str(bat_hdr$,1%,1%)   = "#"
            str(bat_hdr$,2%,219%) = " "          /* ff% = primary File */

* CMG Changed
            write #ff%, str(bat_hdr$,1%,220%) & str(bat_dtl1$(),1%,164%), ~
                  eod goto L01760                 /*AWD019*/


L01740:
        return
L01760:     errormsg$ = error$(err%)
            gosub error_prompt
        return


        write_order_header
            err% = 2%                             /* Set Error Code    */
                                                  /* (EWD001) Mod Begin*/
            if reason_flag% <> 0% then goto L02200   /* Glass House Rmk*/
            if cnt_dtl% = 0% then goto L02010     /* Start New Oder    */
               return                             /* Header Record     */
L02010:     cnt_ord% = cnt_ord% + 1%
            convert cnt_ord% to cnt_ord$, pic(000)/* Count Order Header*/

            bat_no$ = cnt_ord$
            goto L02500

L02200:       if cnt_dtl1% = 0% then goto L02210  /* Start New Order   */
                  return
L02210:       cnt_ord1% = cnt_ord1% + 1%
              convert cnt_ord1% to cnt_ord$, pic(000)/* Count ord head */

              bat_no$ = cnt_ord$
                                                  /* (EWD001) Mod End  */

L02500:     temp_date$ = scr_dte1$                            /* (Y2K, LDJ) */
            call "DATFMTC" (temp_date$, comp%, temp_date2$)   /* (Y2K, LDJ) */
            init(" ") bat_order$, bat_dtl1$()
            str(bat_dtl1$(),1%,384%) = " "          /* (AWD019 )       */
            str(bat_order$,1%,1%)  = "*"            /* Field (0)       */
            str(bat_order$,2%,2%)  = y$
            str(bat_order$,4%,1%)  = x$             /* Field (1)       */

            str(bat_order$,5%,12%) = str(file$,1%,12%)
/* (AWD036) */
            if rm_flag% = 1% then str(bat_order$,5%,12%) =             ~
                                          str(file$,1%,9%) & "-" & intercept$
            if rm_flag% = 3% then str(bat_order$,5%,12%) =             ~
                                          str(file$,1%,9%) & "-" & intercept$
/* (\AWD036) */
            str(bat_order$,17%,4%) = " " & bat_no$
/*(\AWD032)*/
            str(bat_order$,21%,1%) = x$             /*                 */
            str(bat_order$,22%,2%) = y$             /* Field (2)       */
            str(bat_order$,24%,1%) = x$
            str(bat_order$,25%,10%)= space_d$       /* Spacer Descript */
            str(bat_order$,35%,1%) = x$
            str(bat_order$,36%,2%) = y$             /* Field (3)       */
            str(bat_order$,38%,1%) = x$             /* Set to Blank    */

            str(bat_order$,39%,12%) = str(file$,1%,12%)
/* (AWD036) */
            if rm_flag% = 1% then str(bat_order$,39%,11%) =             ~
                                          str(file$,1%,8%) & "-" & intercept$
            if rm_flag% = 3% then str(bat_order$,39%,11%) =             ~
                                          str(file$,1%,8%) & "-" & intercept$
/* (\AWD036) */

            str(bat_order$,51%,4%) = bat_no$
/*(\AWD032)*/
            str(bat_order$,55%,1%) = x$             /*                 */
            str(bat_order$,56%,2%) = y$             /* Field (4)       */
                                                    /* MMDD - 1ST      */
                                                          /* (Y2K, LDJ) */
            str(bat_order$,58%,4%) = str(temp_date2$,5%,4%)/* Prod Date */
                                                         /* (Y2K, LDJ) */
            str(bat_order$,62%,2%) = str(temp_date2$,3%,2%)/* YEAR      */
            str(bat_order$,64%,2%) = y$             /* Field (5)       */
            str(bat_order$,66%,2%) = x$ & x$        /* Set to Blank    */
            str(bat_order$,68%,2%) = y$             /* Field (6)       */
            str(bat_order$,70%,2%) = x$ & x$        /* Set to Blank    */
            str(bat_order$,72%,149%) = " "
            if rmk% <> 0% then goto notPrd
            if tempScrn% <> 0% then goto notPrd
REM  IF TEMPER_FLAG$ = "Y" THEN GOTO NOTPRD

              if str(sort1$,10%,02%) = "TB" then gosub tbCntr
              if str(sort1$,10%,02%) = "TT" then gosub ttCntr
              if str(sort1$,10%,02%) = "BB" then gosub bbCntr

              sort1$ = str(lab_rec$(),211%,11%) & str(lab_rec$(),241%,2%)
              str(bat_order$,78%,14%) = sort1$
              str(bat_order$,92%,17%) = str(lab_key$,01%,17%)
              str(bat_order$,109%,01%) = " "
              str(bat_order$,110%,03%) = schedSort$
              str(bat_order$,113%,01%) = x$

notPrd:

              if rmk% <> 0% then str(bat_order$,70%,2%) = x$ & x$  /* Blank */
* CMG changed
            write #ff%, str(bat_order$,1%,220%) & str(bat_dtl1$(),1%,164%), ~
                  eod goto L01930            /*(AWD019)*/
        return
        dsCntr                                /* (CR2018) */
          if schema% = 2% and dept$ = "071" then goto dsCntrTTBB
          ds% = ds% + 1%
          str(bat_order$,71%,02%) = "00"
          convert ds% to str(bat_order$,73%,04%), pic(0000)
          str(bat_order$,77%,01%) = " "
        return
/* + (CR1987) */        
        tsCntr
          ts% = ts% + 1%
          str(bat_order$,71%,02%) = "00"
          convert ts% to str(bat_order$,73%,04%), pic(0000)
          str(bat_order$,77%,01%) = " "
        return
        qsCntr
          qs% = qs% + 1%
          str(bat_order$,71%,02%) = "00"
          convert qs% to str(bat_order$,73%,04%), pic(0000)
          str(bat_order$,77%,01%) = " "
        return
/* - (CR1987) */        
/* + (CR2018) */
        dsCntrTTBB     /* Logic added not to combine SS & DS TT/BB counters */  
           if str(sort1$,10%,02%) = "TT" then gosub dsTTCntr
           if str(sort1$,10%,02%) = "BB" then gosub dsBBCntr
           
        return
        dsTTCntr                                /* DS TT have odd numbers */
          if dstt% = 0% then gosub addDSTTFirst else gosub addDSTT

          str(bat_order$,71%,02%) = "02"
          convert dstt% to str(bat_order$,73%,04%), pic(0000)
          str(bat_order$,77%,01%) = " "
        return
        addDSTTFirst
          if dstt% = 0% then dstt% = dstt%  + 1%
        return
        addDSTT
                    /* This is in case extra bottom or top batch */
          if dstt% < dsbb% then dstt% = dsbb% + 1% else dstt% = dstt% + 2%
        return
        dsBBCntr
          gosub addDSBB

          str(bat_order$,71%,02%) = "02"
          convert dsbb% to str(bat_order$,73%,04%), pic(0000)
          str(bat_order$,77%,01%) = " "
        return
        addDSBB
          dsbb% = dsbb% + 2%                      /* BB have even numbers */
        return
/* - (CR2018) */
        tbCntr
REM IF STR(LAB_KEY$,03%,02%) = "DS" THEN GOTO DSCNTR           /* (CR1987) */
          if str(lab_key$,03%,02%) = "DD" then goto dsCntr     /* (CR1987) */
          if str(lab_key$,03%,02%) = "DT" then goto dsCntr     /* (CR2260) */
          if str(lab_key$,03%,02%) = "TT" then goto tsCntr     /* (CR1987) */
          if str(lab_key$,03%,02%) = "QQ" then goto qsCntr     /* (CR1987) */          
          tb% = tb% + 1%
          str(bat_order$,71%,02%) = "01"
          convert tb% to str(bat_order$,73%,04%), pic(0000)
          str(bat_order$,77%,01%) = " "
        return
        ttCntr                                 /* TT have odd numbers */
REM IF STR(LAB_KEY$,03%,02%) = "DS" THEN GOTO DSCNTR           /* (CR1987) */
          if str(lab_key$,03%,02%) = "DD" then goto dsCntr     /* (CR1987) */
          if str(lab_key$,03%,02%) = "DT" then goto dsCntr     /* (CR2260) */
          if str(lab_key$,03%,02%) = "TT" then goto tsCntr     /* (CR1987) */
          if str(lab_key$,03%,02%) = "QQ" then goto qsCntr     /* (CR1987) */          
          if tt% = 0% then gosub addTTFirst else gosub addTT

          str(bat_order$,71%,02%) = "02"
          convert tt% to str(bat_order$,73%,04%), pic(0000)
          str(bat_order$,77%,01%) = " "
        return
        addTTFirst
          if tt% = 0% then tt% = tt%  + 1%
        return
        addTT
                    /* This is in case extra bottom or top batch */
          if tt% < bb% then tt% = bb%  + 1% else tt% = tt% + 2%
        return

        bbCntr
REM IF STR(LAB_KEY$,03%,02%) = "DS" THEN GOTO DSCNTR  /* (CR1987) */
          if str(lab_key$,03%,02%) = "DD" then goto dsCntr
          if str(lab_key$,03%,02%) = "DT" then goto dsCntr  /* (CR2260) */
          gosub addBB

          str(bat_order$,71%,02%) = "02"
          convert bb% to str(bat_order$,73%,04%), pic(0000)
          str(bat_order$,77%,01%) = " "
        return
        addBB
          bb% = bb% + 2%                      /* BB have even numbers */
        return



        write_detail
            err% = 3%                               /* Set Error Code  */
            init(" ") bat_dtl$(), bat_dtl1$()       /* (AWD019) */
            str(bat_dtl1$(),1%,384%) = " "          /* (AWD019) */
            if reason_flag% <> 0% then goto L02600
               cnt_dtl% = cnt_dtl% + 1%
               convert cnt_dtl% to cnt_dtl$, pic(###)

               goto L03000
L02600:           cnt_dtl1% = cnt_dtl1%+ 1%
                  convert cnt_dtl1% to cnt_dtl$, pic(###)

L03000:     str(bat_dtl$(),1%,3%)   = cnt_dtl$        /* Field (0)       */
            str(bat_dtl$(),4%,2%)   = y$              /* Field (1)       */
            str(bat_dtl$(),6%,1%)   = "1"             /* Set Qty to '1'  */
            str(bat_dtl$(),7%,2%)   = y$              /* Field (2)       */
            str(bat_dtl$(),9%,1%)   = "0"             /* Set Qty to '0'  */
            str(bat_dtl$(),10%,2%)  = y$              /* Field (3)       */
            str(bat_dtl$(),12%,1%)  = x$
            str(bat_dtl$(),13%,10%) = sandwich$       /* Glass Sandwich  */
            str(bat_dtl$(),23%,1%)  = x$
            str(bat_dtl$(),24%,2%)  = y$              /* Field (4)       */
            str(bat_dtl$(),26%,8%)  = width_d$        /* Decimal Width   */
            str(bat_dtl$(),34%,2%)  = y$              /* Field (5)       */
            str(bat_dtl$(),36%,8%)  = height_d$       /* Decimal Height  */
            str(bat_dtl$(),44%,2%)  = y$              /* Field (6)       */
            str(bat_dtl$(),46%,6%)  = t_k$            /* Decimal Thicknes*/
            str(bat_dtl$(),52%,2%)  = y$              /* Field (7)       */
            str(bat_dtl$(),54%,1%)  = x$              /* Set First 4 Char*/
            str(bat_dtl$(),55%,4%)  = str(space_d$,1%,4%)
            str(bat_dtl$(),59%,1%)  = x$
            str(bat_dtl$(),60%,2%)  = y$              /* Field (8)       */
            str(bat_dtl$(),62%,1%)  = x$
            str(bat_dtl$(),63%,2%)  = gs$             /* Gas Yes or No   */
            str(bat_dtl$(),65%,1%)  = x$
            str(bat_dtl$(),66%,2%)  = y$              /* Field (9)       */
            str(bat_dtl$(),68%,1%)  = x$
            str(bat_dtl$(),69%,10%) = color$         /* Blank No Grid or*/
            str(bat_dtl$(),79%,1%)  = x$              /* Color of Grid   */
            str(bat_dtl$(),80%,2%)  = y$              /* Field (10)      */
            str(bat_dtl$(),82%,1%)  = x$
            str(bat_dtl$(),83%,8%)  = muttin$         /* New Mutin Code  */
            str(bat_dtl$(),91%,1%)  = x$              /* for Grid Config */
            str(bat_dtl$(),92%,2%)  = y$              /* Field (11)      */
            if str(muttin$,1%,6%) = "CUSTOM" then w_adj$ = "  0.00"
            str(bat_dtl$(),94%,6%)  = w_adj$          /* Model Width Adj */
            str(bat_dtl$(),100%,2%) = y$              /* Field (12)      */
            if str(muttin$,1%,6%) = "CUSTOM" then h_adj$ = "  0.00"
            str(bat_dtl$(),102%,6%) = h_adj$          /* Model Height Adj*/
            str(bat_dtl$(),108%,4%) = y$ & x$ & x$    /* Field (13)      */
            str(bat_dtl$(),112%,4%) = y$ & x$ & x$    /* Field (14)      */
            str(bat_dtl$(),116%,4%) = y$ & x$ & x$    /* Field (15)      */
            str(bat_dtl$(),120%,2%) = y$              /* Field (16)      */
            str(bat_dtl$(),122%,1%) = x$
            str(bat_dtl$(),123%,40%) = ged_txt$       /* Label Text Area */
            str(bat_dtl$(),163%,1%) = x$              /* 1st (20),2nd(20)*/
            str(bat_dtl$(),164%,2%) = y$              /* Field (17)      */
            str(bat_dtl$(),166%,1%) = x$
            str(bat_dtl$(),167%,5%) = "1-STD"         /* Default Standard*/
            str(bat_dtl$(),172%,1%) = x$
            str(bat_dtl$(),173%,3%) = y$ & "F"            /* Field (18)  */
            str(bat_dtl$(),176%,3%) = y$ & "F"            /* Field (19)  */
            str(bat_dtl$(),179%,6%) = y$ & "0.00"         /* Field (20)  */
            str(bat_dtl$(),185%,6%) = y$ & "0.00"         /* Field (21)  */
            str(bat_dtl$(),191%,6%) = y$ & "0.00"         /* Field (22)  */
            str(bat_dtl$(),197%,6%) = y$ & "0.00"         /* Field (23)  */
            str(bat_dtl$(),203%,6%) = y$ & "0.00"         /* Field (24)  */
            str(bat_dtl$(),209%,6%) = y$ & "0.00"         /* Field (25)  */
            str(bat_dtl$(),215%,6%) = y$ & "0.00"         /* Field (26)  */
            str(bat_dtl$(),221%,6%) = y$ & "0.00"         /* Field (27)  */
            str(bat_dtl$(),227%,6%) = y$ & "0.00"         /* Field (28)  */
            str(bat_dtl$(),233%,6%) = y$ & "0.00"         /* Field (29)  */
            str(bat_dtl$(),239%,6%) = y$ & "0.00"         /* Field (30)  */
            str(bat_dtl$(),245%,6%) = y$ & "0.00"         /* Field (31)  */
            str(bat_dtl$(),251%,6%) = y$ & "0.00"         /* Field (32)  */
            str(bat_dtl$(),257%,6%) = y$ & "0.00"         /* Field (33)  */
            str(bat_dtl$(),263%,6%) = y$ & "0.00"         /* Field (34)  */
/* (AWD038) */
            str(bat_dtl$(),269%,7%) = y$ & str(interoffset$,1%,5%)
/* (\AWD038) */

            str(bat_dtl$(),276%,2%) = y$ & "1"            /* Field (36)  */
            str(bat_dtl$(),278%,106%) = "    "            /* Fill Area   */
            if reason_flag% <> 0% then goto L03050
REM IF GLS_CNTR% <= 50% THEN GOTO L03100
REM  IF NEW_SORT_CNT%(SORT%) <= 51% THEN GOTO L03100
             if mod(cnt_dtl%,size%) = 0% then cnt_dtl% = 0%
             goto L03100

L03050:       if mod(cnt_dtl1%,size%) = 0% then cnt_dtl1% = 0%
L03100:    if ff% = 10% or ff% = 14% then goto write_additional_dtl
           if ff% = 17% then goto write_additional_dtl
           if ff% = 11% then goto write_additional_dtl
           if ff% = 23% then goto write_additional_dtl  /* (AWD045) */
		   if schema% <> 1% then goto L03102            /* CR3166 */
/* CR3166 */		   
REM	           if ff% = 11% or ff% = 18% or ff% = 23% or ff% = 26%  or ~
			     ff% = 27% or ff% = 28% then goto write_additional_dtl
REM     	   if ff% = 4% then goto write_additional_dtl     /* remake CR3166 */
      /* intercept */
		   if str(lab_rec$(),241%,2%) = "03" and     ~
              schema% = 1% then goto write_additional_dtl /* CR3166 P2     */
			  
L03102:
                                              /* WRITE DETAIL RECORD*/
           write #ff%, str(bat_dtl$(),1,384), eod goto L01930
                    goto finish_dtl
REM This area is to write out comment data
write_additional_dtl:

          str(bat_dtl1$(),1%,4%)    = x$ & "COM"
          str(bat_dtl1$(),5%,10%)   = "MENTS" & x$ & str(y$,1%,1%) & "2" & ~
                                     str(y$,1%,1%) & x$
          str(bat_dtl1$(),15%,3%)   = str(lab_rec$(),183%,3%)
          str(bat_dtl1$(),18%,14%)  = sort1$ & x$

/* (SR72107) */
                                                               /* (CR1931) */
REM IF SURFACE3% = 1% THEN STR(BAT_DTL1$(),32%,20%) = STR(Y$,1%,1%) & "10" ~
REM & STR(Y$,1%,1%) & X$ & "SURFACE 3 272" & X$
REM IF CNT_DTL% <> 1% AND CNT_DTL1% <> 1% THEN GOTO NOT_FIRST
REM STR(BAT_DTL1$(),32%,8%) = STR(Y$,1%,1%) & "60007" &  STR(Y$,1%,1%) & X$
REM BATCH% = SIZE%
/* (AWD023) */
REM BATCH% = BATCH% * 2%             /* BY LITES */
/* (AWD036) CHANGE FROM 38%,4% TO 40%,4% */
REM CONVERT BATCH% TO STR(BAT_DTL1$(),40%,4%), PIC(####)
/* (AWD036) CHANGE FROM 42%,1% TO 44%,14% */
REM STR(BAT_DTL1$(),44%,1%) = X$

/* (CR1931) UPDATE TO USE CMT_POS%*/
          cmt_pos% = 32%
          if cnt_dtl% <> 1% and cnt_dtl1% <> 1% then goto NOT_FIRST
             str(bat_dtl1$(),cmt_pos%,8%) = str(y$,1%,1%) & "60007" &    ~
                                                   str(y$,1%,1%) & x$
             cmt_pos% = cmt_pos% + 8%

             batch% = size%
             batch% = batch% * 2%             /* By lites */
             convert batch% to str(bat_dtl1$(),cmt_pos%,4%), pic(####)
             cmt_pos% = cmt_pos% + 4%

             str(bat_dtl1$(),cmt_pos%,1%) = x$
             cmt_pos% = cmt_pos% + 1%

NOT_FIRST:
/* CR3166 Comments 9 and 10 set for TX...different layout for NC */
REM             if schema% = 1% and ff% = 4%  then goto L03175   /* remake only */
      /* intercept */
		     if str(lab_rec$(),241%,2%) = "03" and     ~
                schema% = 1% then goto L03175           /* CR3166 P2     */

			   str(bat_dtl1$(),cmt_pos%,15%) = str(y$,1%,1%) & "9" &         ~
               str(y$,1%,1%) & x$ & str(lab_ged$,53%,9%) & x$
             cmt_pos% = cmt_pos% + 15%

             if surface3% = 1% then                                          ~
                str(bat_dtl1$(),cmt_pos%,20%) = str(y$,1%,1%) & "10" &       ~
                                str(y$,1%,1%) & x$ & "SURFACE 3 272" & x$
             cmt_pos% = cmt_pos% + 20%
			 goto L03200
L03175:			 
             if schema% = 2% then goto L03200
/* Add Label Info for Erdman CR3166 gl barcode 12, wnd size 15, le barcode 4 */
/* seq nbr 5, model 3 */
             init(" ") cmt_f9$, cmt_f11$, cmt_f12$, cmt_f13$, cmt_f14$,  ~
                       cmt_f15$
           str(cmt_f9$,1%,12%) = str(lab_ged$,53%,9%) & str(lab_rec$(),186%,3%)
			 str(cmt_f9$,13%,15%) = str(lab_rec$(),84%,7%) & "X " &  ~
			                        str(lab_rec$(),91%,6%)
			 str(cmt_f9$,28%,4%) = str(lab_rec$(),297%,4%)
			 str(cmt_f9$,32%,5%) = str(lab_rec$(),176%,5%)  
			 str(cmt_f9$,37%,3%) = str(lab_rec$(),6%,3%)
             str(bat_dtl1$(),cmt_pos%,44%) = str(y$,1%,1%) & "9"  &         ~
                        str(y$,1%,1%) & x$ & cmt_f9$ & x$
             cmt_pos% = cmt_pos% + 44%

             if surface3% = 1% then                                          ~
                str(bat_dtl1$(),cmt_pos%,20%) = str(y$,1%,1%) & "10" &       ~
                                str(y$,1%,1%) & x$ & "SURFACE 3 272" & x$
             cmt_pos% = cmt_pos% + 20%
			 
/* CR3166 */	
			 sub_part$ = str(lab_rec$(),191%,20%)	
             txt_inside_outside$ = "N/A"
			 glass_code$ = str(lab_rec$(),63%,2%)
             gosub lookup_inside_outside
			 wnd_color_code$ = str(lab_rec$(),62%,1%)  
             gosub lookup_wnd_color
			 gosub lookup_glass_text
			 view$ = " "
			 if str(lab_rec$(),45%,1%) = "T" then view$ = "TOP" 
			 if str(lab_rec$(),45%,1%) = "B" then view$ = "BOT"
			 gosub load_interdesc

             init(" ") sh_type$
                                           /*  1 inch  Grid Contour  */ 
             if str(sub_part$,1%,2%) = "23" then sh_type$ = "C"
                                           /* 3 4th     Grid          */
             if str(sub_part$,2%,1%) = "2" then sh_type$ = "W"
                                           /* 5 8th     Grid          */
             if str(sub_part$,2%,1%) = "1" then sh_type$ = " "

             if str(sub_part$,1%,2%) = "21" then sh_type$ = "E"
             if str(sub_part$,2%,1%) = "5" then sh_type$ = "E"
                                           /* 18mm Grid          */
             if str(sub_part$,1,1) = "4" then sh_type$ = "S"  			 

/* lowe out in 7, wnd color 6, dept 3, top/bot 3, SO 8, intercept description 5 */	
/* Contour 1, Glass code (in case Description different) 4                   */	
             str(cmt_f11$,1%,7%) =  txt_inside_outside$ 
			 str(cmt_f11$,8%,6%) =  wnd_color$ 
			 str(cmt_f11$,14%,3%) = str(sort1$,1%,3%) 
			 str(cmt_f11$,17%,3%) = view$ 
			 str(cmt_f11$,20%,8%) = str(lab_rec$(),97%,8%) 
			 str(cmt_f11$,28%,5%) = interdesc$  
			 str(cmt_f11$,33%,1%) = sh_type$
 
			 str(bat_dtl1$(),cmt_pos%,39%) = str(y$,1%,1%) & "11"  &       ~
                        str(y$,1%,1%) & x$ & cmt_f11$ &	x$
             cmt_pos% = cmt_pos% + 39%

/*  production date 10, text 30                    */	
             cmt_prod_date$ = str(temp_date2$,5%,2%) & "/" & 		 ~
			                  str(temp_date2$,7%,2%) & "/" & 	     ~
							  str(temp_date2$,1%,4%) & "/" 	
             str(cmt_f12$,1%,10%) = cmt_prod_date$
             str(cmt_f12$,11%,30%) = txt_glass$			
			 
			 str(bat_dtl1$(),cmt_pos%,46%) = str(y$,1%,1%) & "12"    &      ~
                        str(y$,1%,1%) & x$ &  cmt_f12$  &  x$
             cmt_pos% = cmt_pos% + 46%			 
		 
/* Grid Color  6, printed make number 3, breathing tube 1, Glass Code 4      */
             gl_color$ = "      "
			 gosub lookup_grid_color
             prt_num$ = str(lab_rec$(),186%,3%) 
             gosub set_prt_num
			 str(cmt_f13$,1%,3%) = prt_num$
			 str(cmt_f13$,4%,1%) = gl_breatht$
			 str(cmt_f13$,5%,4%) = "(" & glass_code$ & ")"
			 str(cmt_f13$,9%,6%) = gl_color$
			 
			 str(bat_dtl1$(),cmt_pos%,20) = str(y$,1%,1%) & "13"    &       ~
                        str(y$,1%,1%) & x$ & cmt_f13$ &  x$   
             cmt_pos% = cmt_pos% + 20%				 
			 
/* CR3235 Glass Size Added */
             str(cmt_f14$, 1%,9%) = str(lab_rec$(),19%,9%) 
             str(cmt_f14$,10%,8%) = str(lab_rec$(),28%,8%)   
			 
             str(bat_dtl1$(),cmt_pos%,23) = str(y$,1%,1%) & "14"    &       ~
                        str(y$,1%,1%) & x$ & cmt_f14$ &  x$   				 	 
			 cmt_pos% = cmt_pos% + 23%	
						  
/* DT Text 40         */	
/* CR3235 Change comment 14 to 15 do to possible blank comment */
             cmt_f15$ = str(lab_rec$(),301%,40%)
			 
			 str(bat_dtl1$(),cmt_pos%,46%) = str(y$,1%,1%) & "15"    &      ~
                        str(y$,1%,1%) & x$ & cmt_f15$ & x$    
             cmt_pos% = cmt_pos% + 46%		
			 
L03200:
REM Detail Line
          write #ff%, str(bat_dtl$(),1%,384%),    ~
                                     eod goto L01930

REM COMMENT LINE RECORD
          init(" ") bat_dtl$()

* (AWD019)
          write #ff%, str(bat_dtl1$(),1%,384%),    ~
                                      eod goto L01930

          if awdschgl% <> 1% then goto finish_dtl
           init(" ") bat_dtl1$()
           str(bat_dtl1$(),1%,4%)    = x$ & "COM"
           str(bat_dtl1$(),5%,10%)   = "MENTS" & x$ & str(y$,1%,1%) & "3" & ~
                                      str(y$,1%,1%) & x$
           str(bat_dtl1$(),15%,5%)   = str(lab_key$,8%,5%)
           str(bat_dtl1$(),20%,3%)   = " - "
           str(bat_dtl1$(),23%,5%)   = str(lab_key$,13%,5%)
           str(bat_dtl1$(),28%,3%)   = " - "
           str(bat_dtl1$(),31%,5%)   = sort$
           str(bat_dtl1$(),36%,3%)   = " - "
           str(bat_dtl1$(),39%,10%)  = str(lab_ged$,53%,9%) & x$

           write #ff%, str(bat_dtl1$(),1%,384%),    ~
                                      eod goto L01930

finish_dtl:

REM PRAIRIE GRID RECORD - IF PRAIRIE
          if str(part$,7%,2%) = "58" or str(part$,7%,2%) = "88"          ~
                          then gosub write_prairie
          if str(part$,7%,2%) = "57" then gosub write_prairie
          if str(part$,7%,2%) = "TP" then gosub write_prairie
          if ff% = 17% then gosub valance_grid
          if ff% = 18% then gosub valance_grid
          if ff% = 11% then gosub valance_grid
        return

        write_prairie
             if str(part$,7%,2%) = "58" and v$ = "B" then return
             if str(part$,7%,2%) = "TP" and v$ = "B" then return
             gosub lookup_offsets
             init(" ") bat_dtl$(), bat_dtl1$()      /*(AWD019)*/
             vert_off = 0.00
             hori_off = 0.00                      /* (CR2133) */
                                   /* Must be perimeter prairie and slider */
             if (style$ = "2SL" or style$ = "3SL") and                 ~
                  (str(part$,7%,2%) = "57" or str(part$,7%,2%) = "TP") ~
                                           then goto slider_perimeter_prairie 

             str(bat_dtl$(),1%,1%) = x$             /* quote */
             str(bat_dtl$(),2%,1%) = "V"
             str(bat_dtl$(),3%,1%) = x$             /* quote */
             str(bat_dtl$(),4%,2%) = y$             /* comma */

             vertical = vertical + interadj

             convert vertical to str(bat_dtl$(),6%,7%), pic(00.0000)

             vert_off = width_d - vertical

             str(bat_dtl$(),13%,2%) = y$
             convert vert_off to str(bat_dtl$(),15%,7%), pic(00.0000)

             write #ff%, str(bat_dtl$(),1%,220%) & str(bat_dtl1$(),1%,164%),   ~
                   eod goto L01930

             init(" ") bat_dtl$(), bat_dtl1$()
             vert_off = 0.00
             hori_off = 0.00
             str(bat_dtl$(),1%,1%) = x$
             str(bat_dtl$(),2%,1%) = "H"
             str(bat_dtl$(),3%,1%) = x$
             str(bat_dtl$(),4%,2%) = y$

/* (AWD026) */
             if str(part$,7%,2%) = "57" then goto perimeter
/* (AWD037) */
             if str(part$,7%,2%) = "TP" then goto perimeter
/* (AWD039) */
             horizontA = horizont
             if str(part$,1%,3%) = "200" then horizontA = 4.3125
             if str(part$,1%,3%) = "250" then horizontA = 4.8750
             if str(part$,1%,3%) = "240" then horizontA = 4.4375
             if str(part$,1%,3%) = "260" then horizontA = 4.8750
             if str(part$,1%,3%) = "270" then horizontA = 4.5000
             if str(part$,1%,3%) = "400" then horizontA = 4.4375
             if str(part$,1%,3%) = "405" then horizontA = 4.3125
             if str(part$,1%,3%) = "608" then horizontA = 5.0000
             if str(part$,1%,3%) = "609" then horizontA = 4.8750
             if str(part$,1%,3%) = "706" then horizontA = 4.6250
             horizontA = horizontA + interadj
             horizont  = horizont  + interadj

             convert horizontA to str(bat_dtl$(),6%,7%), pic(00.0000)
             hori_off = height_d - horizont

             str(bat_dtl$(),13%,2%) = y$

             convert hori_off to str(bat_dtl$(),15%,7%), pic(00.0000)
               goto write_hori

perimeter:
/*(AWD037)*/
             if v$ = "B" and str(part$,7%,2%) = "TP" then return
REM THIS IS MEASURED FROM BOTTOM UP BECAUSE GED DOES CALCULATIONS FROM BOTTOM UP
REM FOR EXAMPLE FOR 4 INCH PLACEMENT ON 25 INCH SASH THEN TOP POSITION WOULD BE 21 INCHES

             if v$ = "T" then hori_off = height_d - horizont
             if v$ = "B" then hori_off = horizont

             convert hori_off to str(bat_dtl$(),6%,7%), pic(00.0000)
write_hori:


            write #ff%, str(bat_dtl$(),1%,220%) & str(bat_dtl1$(),1%,52%),   ~
                  eod goto L01930
                                                        /*  (AWD014)  */

        return
        
/* + (CR2133) */        
        slider_perimeter_prairie
           if style$ = "3SL" then goto perimeter_3SL   

           init(" ") bat_dtl$(), bat_dtl1$()
           vert_off = 0.00
           hori_off = 0.00                       
           str(bat_dtl$(),1%,1%) = x$             /* quote */
           str(bat_dtl$(),2%,1%) = "V"
           str(bat_dtl$(),3%,1%) = x$             /* quote */
           str(bat_dtl$(),4%,2%) = y$             /* comma */
          /* default OX hng 07 */
          /*        XO hng 06  */
           vertical = vertical + interadj
           if v$ = "B" then vertical = width_d - vertical
           if hng$ = "06" then vertical = width_d - vertical
           if hng$ = "06" and v$ = "B" then vertical = vertical


           convert vertical to str(bat_dtl$(),6%,7%), pic(00.0000)

  
           write #ff%, str(bat_dtl$(),1%,220%) & str(bat_dtl1$(),1%,164%), ~
                   eod goto L01930

           init(" ") bat_dtl$(), bat_dtl1$()
           vert_off = 0.00
           hori_off = 0.00
           str(bat_dtl$(),1%,1%) = x$            /* quote */
           str(bat_dtl$(),2%,1%) = "H"
           str(bat_dtl$(),3%,1%) = x$            /* quote */
           str(bat_dtl$(),4%,2%) = y$            /* comma */
   
           if v$ = "B" and str(part$,7%,2%) = "TP" then return
          
           horizont = horizont + interadj
           /*example = 4.0 */
           hori_off = height_d - horizont
           /*example -> 55.875 - 4.00 = 51.875 */  

           convert horizont to str(bat_dtl$(),6%,7%), pic(00.0000)
           str(bat_dtl$(),13%,2%) = y$
           convert hori_off to str(bat_dtl$(),15%,7%), pic(00.0000)

           write #ff%, str(bat_dtl$(),1%,220%) & str(bat_dtl1$(),1%,52%),   ~
                   eod goto L01930
        return
        
perimeter_3SL:
           init(" ") bat_dtl$(), bat_dtl1$()
           vert_off = 0.00
           hori_off = 0.00                       
           str(bat_dtl$(),1%,1%) = x$             /* quote */
           str(bat_dtl$(),2%,1%) = "V"
           str(bat_dtl$(),3%,1%) = x$             /* quote */
           str(bat_dtl$(),4%,2%) = y$             /* comma */

           if str(glsbar$,9%,1%) = "0" then goto perimeter_3SL_noVertical
           vertical = vertical + interadj    /*panel 6 is right side of silder*/
           if str(glsbar$,9%,1%) = "6" then vertical = width_d - vertical

           /*example X-5 -> vertical = 4.00                  */
           /*example X-6 -> vertical =  33.25 - 4.00 = 29.25 */
          
           convert vertical to str(bat_dtl$(),6%,7%), pic(00.0000)
          
           write #ff%, str(bat_dtl$(),1%,220%) & str(bat_dtl1$(),1%,52%),   ~
                   eod goto L01930          
                
perimeter_3SL_noVertical:                
           /*example X-5-> "V", 4.00 */
           /*example X-6-> "V", 29.25 */

           init(" ") bat_dtl$(), bat_dtl1$()
           vert_off = 0.00
           hori_off = 0.00
           str(bat_dtl$(),1%,1%) = x$            /* quote */
           str(bat_dtl$(),2%,1%) = "H"
           str(bat_dtl$(),3%,1%) = x$            /* quote */
           str(bat_dtl$(),4%,2%) = y$            /* comma */
           
           horizont = horizont + interadj
           /*example = 4.0 */
           hori_off = height_d - horizont
           /*example -> 55.875 - 4.00 = 51.875 */
           
           convert horizont to str(bat_dtl$(),6%,7%), pic(00.0000)
           str(bat_dtl$(),13%,2%) = y$
           convert hori_off to str(bat_dtl$(),15%,7%), pic(00.0000)

           write #ff%, str(bat_dtl$(),1%,220%) & str(bat_dtl1$(),1%,52%),   ~
                   eod goto L01930
           /*example -> "H", 4.00, 51.875 */

        return
        
/* - (CR2133) */        
        
/* (AWD040) */
        valance_grid
REM CALL "SHOSTAT" ("VALANCE GRID" ) STOP
          if str(gd$,1%,1%) <> "V" then return
          if str(style$,1%,2%) = "DH" and v$ = "B" then return
          if str(style$,1%,2%) = "SH" and v$ = "B" then return
          if str(style$,1%,2%) = "HP" and v$ = "B" then return
          if str(style$,1%,2%) = "DW" and v$ = "B" then return /* (CR2221) */

          init(" ") bat_dtl$(), bat_dtl1$()

          str(bat_dtl$(),1%,11%)   = x$ & "COMMENTS" & x$ & str(y$,1%,1%)
          str(bat_dtl$(),12%,4%)   = "50" & str(y$,1%,1%) & x$
          str(bat_dtl$(),16%,3%)   = "vg" & x$

          write #ff%, str(bat_dtl$(),1%,220%) & str(bat_dtl1$(),1%,52%),   ~
                  eod goto L01930


          gosub check_height
          gosub check_width

          init(" ") bat_dtl$(), bat_dtl1$()

          str(bat_dtl$(),1%,1%) = x$
          str(bat_dtl$(),2%,1%) = "V"
          str(bat_dtl$(),3%,1%) = x$
          str(bat_dtl$(),4%,2%) = y$
          if num_of_v% = 0% then goto not_val_v
           str_pos% = 6%
           v_pos    = val_v
           for v% = 1% to num_of_v%
              convert v_pos to str(bat_dtl$(),str_pos%,7%), pic(00.0000)
              str_pos% = str_pos% + 7%
              if v% <> num_of_v% then str(bat_dtl$(),str_pos%,2%) = y$
              if v% <> num_of_v% then str_pos% = str_pos% + 2%
              if v% <> num_of_v% then v_pos = v_pos + val_v
           next v%


not_val_v:
          write #ff%, str(bat_dtl$(),1%,220%) & str(bat_dtl1$(),1%,52%),   ~
                eod goto L01930


          init(" ") bat_dtl$(), bat_dtl1$()

          str(bat_dtl$(),1%,1%) = x$
          str(bat_dtl$(),2%,1%) = "H"
          str(bat_dtl$(),3%,1%) = x$
          str(bat_dtl$(),4%,2%) = y$
          convert sash_pos to str(bat_dtl$(),6%,7%), pic(00.0000)

          write #ff%, str(bat_dtl$(),1%,220%) & str(bat_dtl1$(),1%,52%),   ~
                eod goto L01930

        return

        check_height
          sash_pos = 0.00
          val_pos  = 12.00
          wnd_height$ = str(part$,17%,19%)
          if wnd_height$ >= "554" then goto std_pos /* 55 1/2 */

          if str(style$,1%,2%) = "DH" or str(style$,1%,2%) = "SH" or ~
             str(style$,1%,2%) = "HP"                    then        ~
                         val_pos = round(height_d * 0.50,4)
          if str(style$,1%,2%) <> "DH" and str(style$,1%,2%) <> "SH" and ~
             str(style$,1%,2%) <> "HP"                           then    ~
                         val_pos = round(height_d * 0.25,4)

std_pos:
           sash_pos = height_d - val_pos
        return

        check_width
          num_of_v% = 0%
          convert str(gd$,2,1) to num_of_v%, data goto bad_val_v
          val_v = 0.00
          val_v = round(width_d / (num_of_v%+1%), 4)

        bad_val_v
        return


/* (\AWD040) */

        lookup_offsets
           vertical = 4.00
           horizont = 4.00

           init(" ") readkey$, desc$
           str(readkey$,1%,9%)   = "GEDPRAIRI"
           str(readkey$,10%,15%) = str(part$,1%,3%) & "-" & v$
REM CALL "SHOSTAT" ("GENCODES READKEY " & READKEY$)  STOP

           read #2,key = readkey$, using L03240, desc$, eod goto stand_off
L03240:             FMT POS(25), CH(30)
             convert str(desc$,1%,6%) to vertical, data goto stand_ver

stand_ver:

             convert str(desc$,8%,6%) to horizont, data goto stand_off
stand_off:

        return
                                                     /*  (EWD011) - END */

        file_exists
          comp% = 2%
          hdr$ = "**** GED Glass File Exists ***"
          msg$(1%) = "       The File (XXXXXXXX) Already Exists.      "
          str(msg$(1%),18%,8%) = ff$
          msg$(2%) = "          B u i l d   G E D   G l a s s          "
          msg$(3%) = "Press <RETURN> To Continue, or PF(16) to Delete. "
          call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        CHECK_GLASS        /* Test Liting Codes in Table to see if Skipped  */
           if str(new_rec$(),65%,2%) > "82" and str(new_rec$(),65%,2%) < "88"~
                            then return
           if str(new_rec$(),65%,2%) > "96" and str(new_rec$(),65%,2%) <= "99"~
                            then return
           init(" ") readkey$ : check% = 0%
           str(readkey$,1%,9%)   = "PLANGLOUT"
           str(readkey$,10%,15%) = str(new_rec$(),65%,2%)
           read #2,key = readkey$, eod goto L03260
           check% = 1%
L03260: return
        check_argon             /* Check Table for Glass Code and Argon Gas */
           init(" ") readkey$ : argon% = 0%
           str(readkey$,1%,9%)   = "PLANARGON"
           str(readkey$,10%,15%) = str(lab_rec$(),63%,2%)
           read #2,key = readkey$, eod goto L03330
           argon% = 1%
L03330: return

        check_krypton          /* Check Table for Glass Code and Krypton Gas*/
           init(" ") readkey$ : krypton% = 0%
           str(readkey$,1%,9%)   = "PLANKRYPT"
           str(readkey$,10%,15%) = str(lab_rec$(),63%,2%)
           read #2,key = readkey$, eod goto not_krypton
           krypton% = 1%
not_krypton:
        return

        unpack_data
REM CALL "SHOSTAT" ( " UNPACK DATA " )  STOP
            init(" ") cl$, gridcl$
            tty% = 0%
/* (SR72107) */
           surface3% = 0%
           sort$   = str(lab_key$,8,5)
/* (AWD036) to put gap in order numbers */
           model$  = str(lab_rec$(),6%,3%)        /* Mode/Product Code */
REM INTERCEPT$ = STR(LAB_KEY$,1,2)         /* (AWD036) */

           intercept$ = str(lab_rec$(),241%,2%)

           if sav_intercept$ <> intercept$ then gosub new_header

           spacer$  = str(lab_ged$,3%,6%)         /* Spacer Size       */
           model$   = str(lab_rec$(),6%,3%)       /* Mode/Product Code */
           patio$   = str(lab_rec$(),234%,1%)     /* (AWD034)          */
           framecl$ = str(lab_rec$(),9%,1%)       /* Color Code        */
           tty$     = str(lab_rec$(),63%,2%)      /* Glass Type Code   */
                                                  /* (SR72107) */


           if tty$ >= "3A" and tty$ <= "3N" then surface3% = 1%
           ssq$    = str(lab_rec$(),176%,5%)      /* Planning Seq. No. */
           field1$ = str(lab_rec$(),191%,1%)      /* (PAR000) grdtype  */
           field2$ = str(lab_rec$(),192%,1%)      /* (PAR000) grdsize  */
           field3$ = str(lab_rec$(),193%,1%)      /* (PAR000) grdcolor */
           field4$ = str(lab_rec$(),194%,1%)      /* (PAR000) hardware */
           field5$ = str(lab_rec$(),195%,1%)      /* (PAR000) foam     */
           field6$ = str(lab_rec$(),196%,1%)      /* Casing            */
           field7$ = str(lab_rec$(),197%,1%)      /* GLASSACC (CR1929) */
           field8$ = str(lab_rec$(),198%,1%)      /* Grid Type Outside */
           field9$ = str(lab_rec$(),199%,1%)      /* Grid Size  Outside*/
           field10$ = str(lab_rec$(),200%,1%)     /* Grid Color Outsid */
           field11$ = str(lab_rec$(),201%,1%)     /* Grid Color Inside */
           glstype$ = str(lab_rec$(),269%,20%)    /* (IM8022) GlassType*/


REM CALL "SHOSTAT" (" UNPACK DATA" ) STOP
           valance% = 0%
REM convert str(lab_rec$(),270%,1%) to valance%, data goto noValance
           convert str(lab_rec$(),289%,1%) to valance%, data goto noValance

noValance:

           init(" ") test$
           test$   = str(lab_rec$(), 180%, 40%)

/* (AWD034) */
           if rm_flag% = 3% and patio$ <> "2" then ssq$ = "00000"
           v$      = str(lab_rec$(),45%,1%)       /* View T or B       */
           t_k$    = str(lab_rec$(),162%,6%)      /* Gls Overall Thickn*/
           muttin$ = str(lab_rec$(),168%,8%)      /* Glass Muttin Code */
           if str(muttin$,1%,5%) = "PRAIR" then muttin$ = "CUSTOM"
/* (AWD041) */
           if ff% = 17% then muttin$ = "CUSTOM"
/* (\AWD041) */
           color$  = "          "
           convert tty$ to tty%, data goto L03490 /* Convert Gls Type  */
L03490:

badGrid:

           part$     = str(lab_rec$(),59%,25%)    /* MFG Part Number   */
           gd$       = str(part$,7%,2%)           /* Grid/Liting Code  */
           if str(gd$,1,1) = "V" then valance% = 1%
           lk$       = str(part$,12%,1%)
           hng$      = str(part$,9%,10%)          /* (CR2133) */
           lk% = 0%
           if field1$ = "2" and field2$ = "3" then lk% = 1%
           if gd$ = "00" then goto L03630  /* gd$ = "00" no grid */

             if gd$ = "58" and v$ = "B" then goto L03630  /* (AWD025) */
             cl$ = framecl$ & field3$
             readkey$ = "FRAMEGRID" & cl$
             read #2,key = readkey$, using GEN_FMT, desc$, eod goto L03630
GEN_FMT:         FMT POS(25), CH(30)
             gridcl$ = str(desc$,1%,2%)
             if field1$ = "3" then gridcl$ = "BR"
REM THIS IS BECAUSE CONTOUR IS STRAIGHT CUT AND DOES NOT HAVE FISH-MOUTH
             if str(model$,1%,1%) = "6" then goto changeGridColor
             if str(model$,1%,3%) = "124" or str(model$,1%,3%) = "134" ~
                                         then goto changeGridColor
             if str(model$,1%,3%) = "1A1" or str(model$,1%,3%) = "1A2" ~
                                         then goto changeGridColor
             if str(model$,1%,3%) = "1A3" or str(model$,1%,3%) = "1A4" ~
                                         then goto changeGridColor
             if str(model$,1%,3%) = "870" and gridcl$ = "WH"           ~
                                         then goto changeGridColor

               goto notStraightCut
changeGridColor:
               if gridcl$ = "WH" then gridcl$ = "BW"
               if gridcl$ = "AL" then gridcl$ = "SA"

notStraightCut:

              color$   = "GRID" & gridcl$
              if lk% <> 0% then color$ = "CONT" & gridcl$
              if field2$ = "2" then color$ = "WIDE" & gridcl$
/* (AWD022) */
              if field1$ = "2" and field2$ = "1"              ~
                                then color$ = "CO58" & gridcl$
REM IF CL$>="I" AND CL$ <="P" AND STR(COLOR$,1%,4%) = "CO58" THEN COLOR$ = "PT58" & G_C$(C%)
              if str(color$,1%,4%) <> "CO58" then goto notPaint
               search painted$ = framecl$ to painted()
               painted% = int(painted())
               if painted% = 0% then goto notPaint
                 color$ = "PT58" & gridcl$

notPaint:
/* (SR64645) */
             goto L03630  /* Turn OFF For Now */
             if valance% = 0% then goto L03630
              if str(color$,1%,4%) = "GRID" then str(color$,1%,4%) = "VGRD"
              if str(color$,1%,4%) = "WIDE" then str(color$,1%,4%) = "VWDE"
              if str(color$,1%,4%) = "CONT" then str(color$,1%,4%) = "VCNT"
              if str(color$,1%,4%) = "CO58" then str(color$,1%,4%) = "VL58"
              if str(color$,1%,4%) = "PT58" then str(color$,1%,4%) = "PL58"

L03630:       load$     = str(lab_rec$(),1%,5%)      /* Load Number     */
              so$       = str(lab_rec$(),97%,8%)     /* SO Number       */
                                               /* Only Production Glass */
/* (AWD038) */
           if awdschgl% = 1% then goto skip_SORest
skip_SORest:
/* (\AWD038) */
           space_d$  = str(lab_rec$(),105%,10%)   /* Spacer Description*/
           sandwich$ = str(lab_rec$(),115%,10%)   /* Sand Description  */

/* (AWD030) */
           if scr_sel$ = "2" or scr_sel$ = "8" then goto no_sandwich_change
             if sandwich$ = "IG3CLS3CLS" then sandwich$ = "IG3CL3CL"
             if sandwich$ = "IG3CLS3PAS" then sandwich$ = "IG3CL3PA"
no_sandwich_change:
/* (\AWD030) */
           width_d$  = str(lab_rec$(),125%,8%)    /* Decimal Width     */
           height_d$ = str(lab_rec$(),133%,8%)    /* Decimal Height    */
           gosub check_grid_bumpers               /* Glass Grid Bumpers*/
           w_adj$    = str(lab_rec$(),141%,6%)    /* Width Adjustment  */
           h_adj$    = str(lab_rec$(),147%,6%)    /* Height Adjustment */
           temp$     = str(lab_rec$(),153%,1%)    /* '*' = Tempered Gls*/
           lt$       = str(part$,7%,2%)           /* Liting Code       */
           gs$ = "  "                             /* Argon Gas Flag    */
           gosub check_argon                      /* Chk for Argon Gas */
           if argon% = 1% then gs$ = "G1"
           if field7$ = "1" then gs$ = "G1"       /* (CR1931)          */

           gosub check_krypton                    /* Chk for KryptonGas*/
           if krypton% = 1% then gs$ = "G2"
           if field7$ = "4" then gs$ = "BR"

           lab_fil$ = str(lab_rec$(),50%,9%)

           ged_txt$  = " "
           str(ged_txt$,1%,1%) = v$             /* Ged Text (40)=Char  */
           str(ged_txt$,2%,3%) = model$
           str(ged_txt$,5%,3%) = str(gridcl$,1%,2%) & " "
           str(ged_txt$,8%,9%) = so$ & " "
           str(ged_txt$,17%,5%)= ssq$
           str(ged_txt$,23%,18%) = str(part$,1%,18%)

/* (AWD038) */
           interoffset, interadj = 0.00
           interoffset$ = str(lab_rec$(),243%,7%)
           interadj$    = str(lab_rec$(),250%,7%)

           convert interoffset$ to interoffset, data goto badoffset

badoffset:

           convert interadj$ to interadj, data goto badadj

badadj:

           init(" ") style$
           style$ = str(lab_rec$(),259%,10%)
           if sav_bat$ = "XXX" then sav_bat$ = bat_no$
REM !! call "SHOSTAT" ("HERE AT P_SORT DEPT & STRENGTH ")
REM !! stop
           p_sort$ = str(sort1$,4%,8%)                /* Sort after dept */
           dept$   = str(sort1$,1%,3%)                /* Department      */
           strength$ = str(sort1$,4%,2%)              /* Strength        */
           sp$ = str(sort1$,6%,4%)                    /* Spacer          */
           if sav_p_sort$ = "XXXXXXXX" then sav_p_sort$ = p_sort$
           if sav_sp$ = "XXXX" then sav_sp$ = sp$

           if sav_spacer$ = spacer$ then goto L03900
L03860:                                           /* (AWD015) */
                                                  /* this means remakes */
             if ff% = 10% or ff% = 11% then goto prod_ds_check
             if ff% = 14% or ff% = 17% then goto prod_ds_check
                 goto remake_cntr


prod_ds_check:

              if ds_batch$ = "0" then goto remake_cntr
REM IF DS_BATCH$ = "0" THEN GOTO NOT_DS
              sav_sort$  = sort$
              sav_sort2$ = sort2$

REM  IF STRENGTH$ <> "DS" THEN GOTO REMAKE_CNTR                 /* (CR1987) */
REM  IF STRENGTH$ <> "DS" THEN GOTO REMAKE_CNTR                 /* (CR1987) */
REM  if strength$ <> "DD" then goto remake_cntr
              if strength$ = "SS" then goto remake_cntr
              if sav_sp$ <> sp$ then goto remake_cntr /* Has to be same Spacer*/
              if strength$ = "DD" and dept$ = "071" and schema% = 2% ~
                                                   then goto remake_cntr
                                                               /* (CR1987) */
REM IF STRENGTH$ = "DS" AND GLSCNTR_KEY$ = GLSCNTR_SORT$  THEN RETURN
                                                               /* (CR1987) */
REM if strength$ = "DD" and glscntr_key$ = glscntr_sort$    ~
                               then return
              if glscntr_key$ = glscntr_sort$ then return      /* (CR1987) */

REM IF STRENGTH$ = "DS" AND GLSCNTR_KEY$ <> GLSCNTR_SORT$ THEN GOTO DEPTFOUND
REM ??? I am not sure what sav_sp and sp are used for
REM IF SAV_SP$ = SP$ AND STRENGTH$ = "DS" AND DEPT$ <> "047" THEN RETURN
REM DEPTFOUND
REM if dept$ = "047" and str(sp$,1%,4%) = "SP11" then return
REM NOT_DS

remake_cntr:

              cnt_dtl% = 0% : cnt_dtl1% = 0%
              sav_spacer$ = spacer$
              sav_sort$   = sort$
              sav_intercept$ = intercept$         /* (AWD036) */
              sav_p_sort$ = str(sort1$,4%,8%)     /* (AWD015) */
              sav_sp$     = sp$                   /* (AWD015) */
              glscntr_sort$ = glscntr_key$        /* (AWD017) */
REM SCHEDSORT$ = STR(LAB_REC$(),257%,3%)
              schedSort$ = str(lab_rec$(),290%,3%)
              if awdschgl% = 1% then sav_sort2$ = sort2$
              return
L03900:    if sav_sort$ <> sort$ then goto L03860
/* (AWD038) */
           if awdschgl% = 0% then return
            if sav_sort2$ = sort2$ then return
               sav_sort2$ = sort2$
               goto L03860
/* (AWD038) */
        return


        new_header
/* (AWD036) to put gap in order numbers */
          sav_intercept$ = intercept$
REM may need return here for remakes or tempered, etc
          cnt_ord% = cnt_ord% + 100%

          if rm_flag% = 1% then goto order_header  /* Not Remakes */
          if rm_flag% = 3% then goto order_header  /* Not Remakes */
/* (AWD028) */
REM this is so will not write trailer and header for tempered batches
          if tempScrn% = 1% then goto order_header
          if lamnScrn% = 1% then goto order_header  /* (AWD045) */
          if scr_sel$ = "8" then goto order_header

          gosub write_batch_trailer
          gosub write_batch_header
        return
        order_header
          cnt_dtl% = 0%
        return

        check_remake
           reason_flag% = 0%                           /* #4,#1,#3  (RMK)*/
           if ff% = 10% then return                    /* Not Remake Gls */
           if ff% = 11% then return
           if ff% = 14% then return
                                                /* Production or         */
                                                /* In-House Glass        */
           init(" ") rm_key$                    /* Creating Re-Make Glass*/
           str(rm_key$,1%,9%)  = str(lab_ged$,53%,9%)
           str(rm_key$,10%,3%) = str(lab_rec$(),186%,3%)
                                                /* Only Production Glass */

           read #8,key = rm_key$, using L03950, rm_reason$, eod goto L04010
L03950:       FMT POS(34), CH(2)

           rm_reason% = 0%
           convert rm_reason$ to rm_reason%, data goto L04000
L04000                                          /* Codes 26, 28, 30      */
           if rm_reason% > 25% and rm_reason% < 31% then reason_flag% = 1%


L04010: return

        update_remake     /* All Glass Has at Least One Entry in APCPLNGR*/
                          /* Status 0=Remake,1=Sched,2=Complete,9=Order  */

           init(" ") rm_key$,rm_st_time$,rm_reason$,rm_st_dte$,           ~
                     rm_rec$(), rm_st$, errormsg$, chk_st$
           error% = 0%                           /* Formated Time AM,PM  */
           call "TIME" (rm_st_time$)             /* Time Glass Scheduled */
           rm_st$ = "1"                          /* Set to Scheduled Glass*/
           rm_reason$ = "00"                     /* Normal Sched Glass   */
           rm_st_dte$ = date                     /* Todays Date          */
           str(rm_key$,1%,9%)  = str(lab_ged$,53%,9%)  /* Glass Barcode  */
           str(rm_key$,10%,3%) = str(lab_rec$(),186%,3%) /* Re-Make No.  */

           read #8,hold,key = rm_key$, using L04050 , rm_rec$(),            ~
                                                  eod goto schedule_glass
L04050:       FMT 2*CH(256)
           chk_st$ = str(rm_rec$(),13%,1%)
           if chk_st$ = "2" then goto L04340     /* Error Condition    */

                 delete #8

        REM - Re-Schedule Glass for Re-Make      /* Found in Gls Batch */
              str(rm_rec$(),1%,6%) = str(glass_dte1$,1%,6%) /* Prod. Dte */
              str(rm_rec$(),7%,6%) = str(glass_dte1$,1%,6%) /* Scan. Dte */
              str(rm_rec$(),13%,1%) = rm_st$       /* Chg Status 0 to a 1*/
              str(rm_rec$(),14%,8%) = rm_st_time$  /* Time of Stat Change*/
                                                   /* Leave Glass Barcode*/
                                                   /* alone              */
                                                   /* Re-Make No Alone   */
                                                   /* and Rsn Code alone */
              str(rm_rec$(),36%,6%) = date         /* Date of Stat Change*/
                                                   /* 42 - 64            */
                                                   /* Contains remake    */
                                                   /* Scan Date/Time     */
              str(rm_rec$(),242%,5%) = str(lab_rec$(),176%,5%)
                                                   /* Update seq No.     */

              str(rm_rec$(),67%,445%) = str(lab_rec$(),1,445%) /*(AWD036)*/
              str(rm_rec$(),163%,8%)  = so$                    /*(AWD036)*/
              goto L04290
        schedule_glass                             /* First Time Need to */
                                                   /* Create Glass Data  */
           str(rm_rec$(),1%,6%)  = str(glass_dte1$,1%,6%) /*Glass Prod Dt*/
           str(rm_rec$(),7%,6%)  = str(glass_dte1$,1%,6%) /*Glass Prod Dt*/
           str(rm_rec$(),13%,1%) = rm_st$          /* Scheduled Glass    */
           str(rm_rec$(),14%,8%) = rm_st_time$     /* Time of Stat Change*/
           str(rm_rec$(),22%,9%) = str(lab_ged$,53%,9%) /* Glass Barcode */
           str(rm_rec$(),31%,3%) = str(lab_rec$(),186%,3%) /* Remake No. */
           str(rm_rec$(),34%,2%) = rm_reason$      /* Glass Reason Code  */
           str(rm_rec$(),36%,6%) = date            /* Date of Stat Change*/
           str(rm_rec$(),42%,2%) = str(lab_rec$(),178%,2%) /* Scn Shift  */
                                                   /* Time 24 hour clock */
           str(rm_rec$(),44%,8%) = time            /* Scheduled Time     */
           str(rm_rec$(),52%,6%) = date            /* Scheduled Date Today*/
           str(rm_rec$(),58%,3%) = userid$         /* Who Scheduled Glass*/
           str(rm_rec$(),61%,4%) = "    "          /* Completion Calc    */
           str(rm_rec$(),65%,2%) = "  "            /* Growth Area        */
           str(rm_rec$(),67%,445%) = str(lab_rec$(),1,445%)
           str(rm_rec$(),163%,8%)  = so$
                                                   /* Calculated Data    */
                                                   /* Check for INDY     */
           if str(lab_rec$(),190%,1%) = "1" then   /* set as completed   */~
                                            str(rm_rec$(),13%,1%) = "2"

/*(PAR000) only for production and remakes; not tempered */
REM this sets the status to complete

L04290:    write #8, using L04050, rm_rec$(), eod goto L04310
        return
L04310:    errormsg$="(Err)- Updating Glass Primary Database"
           gosub error_prompt
        return
L04340:    errormsg$="(Err)-Re-Schedule of Completed Glass For S.O.= " & so$
REM           gosub error_prompt
        return
                                                 /* (EWD005)           */
        check_grid_bumpers
           width_d = 0.0 : height_d = 0.0
           convert width_d$  to width_d,  data goto L04400
L04400:
           convert height_d$ to height_d, data goto L04410
L04410:
           if width_d > 74.0 then goto L04450
           if height_d > 74.0 then goto L04450
/* (AWD021) - change from 119 to 100 */
           if (width_d + height_d) > 100.0 then goto L04450

/* (AWD021) - all Doors with Bumpers */
           if model$ = "311" or model$ = "312" then goto L04450
           if model$ = "313" or model$ = "314" then goto L04450
           if model$ = "332" or model$ = "333" then goto L04450
           if model$ = "334" then goto L04450
/* (AWD021/)*/
        return
L04450:    if str(color$,1%,4%) = "CONT" then str(color$,1%,4%) = "BCNT"
           if str(color$,1%,4%) = "GRID" then str(color$,1%,4%) = "BGRD"
                                                       /*  (EWD009)    */
           if str(color$,1%,4%) = "WIDE" then str(color$,1%,4%) = "BWDE"
/*(AWD022)*/
           if str(color$,1%,4%) = "CO58" then str(color$,1%,4%) = "BC58"
/*(/AWD022)*/
/* (SR64645) */
           return   /* Turn Off for Now */
           if valance% = 0% then return
            if str(color$,1%,4%) = "GRID" then str(color$,1%,4%) = "BVGD"
            if str(color$,1%,4%) = "WIDE" then str(color$,1%,4%) = "BVWD"
            if str(color$,1%,4%) = "CONT" then str(color$,1%,4%) = "BVCT"
            if str(color$,1%,4%) = "CO58" then str(color$,1%,4%) = "BV58"
            if str(color$,1%,4%) = "PT58" then str(color$,1%,4%) = "BT58"
/* (\SR64645) */
/* (SR67936) */
REM IF STR(COLOR$,1%,8%) = "DURAGRID" THEN STR(COLOR$,1%,8%) = "DURABGRD"
REM IF STR(COLOR$,1%,8%) = "DURACONT" THEN STR(COLOR$,1%,8%) = "DURABCNT"
REM IF STR(COLOR$,1%,8%) = "DURAWIDE" THEN STR(COLOR$,1%,8%) = "DURABWDE"
REM IF STR(COLOR$,1%,8%) = "DURACO58" THEN STR(COLOR$,1%,8%) = "DURABC58"
        return
                                                 /* (EWD005)           */

                                                 /* (AWD017) - BEGIN   */
        ASSIGN_GLS_CNTR_SORT
REM DOES NOT APPLY TO PRECUT AND  DOES NOT APPLY TO REMAKE NOT SAME SORT
REM IF PASS% = 2% THEN RETURN
REM IF RM_FLAG% <> 0% THEN RETURN
           if rm_flag% = 1% or rm_flag% = 3% then return
/*(AWD020)*/
           if temper_flag$ = "Y" then goto SKIP_DS_CHECK
           if tempScrn% <> 0% then return

           init(" ") glscntr_key$
REM IF STR(SORT1$,4%,2%) = "DS" THEN GOTO SET_DS_KEY
                                                           /* (AWD019) */
REM IF STR(SORT1$,4%,2%) = "DS" AND DS_BATCH$ <> "0" THEN GOTO SET_DS_KEY
                                                             /* (CR1987) */
           if str(sort1$,4%,2%) = "DD" and ds_batch$ <> "0"            ~
                                                    then goto SET_DS_KEY
           if str(sort1$,4%,2%) = "DT" and ds_batch$ <> "0"  /* (CR2260) */~
                                                    then goto SET_DS_KEY
           if str(sort1$,4%,2%) = "TT" and ds_batch$ <> "0" /* (CR1987) */~
                                                    then goto SET_DS_KEY
           if str(sort1$,4%,2%) = "QQ" and ds_batch$ <> "0" /* (CR1987) */~
                                                    then goto SET_DS_KEY

SKIP_DS_CHECK:
              glscntr_key$ = sort1$
        return
        SET_DS_KEY
           str(glscntr_key$,1%,11%) = str(sort1$,4%,6%) & schedSort$

        return

REM IF STR(SORT1$,1%,3%) = "047" THEN GOTO SEPARATEDS
REM IF STR(SORT1$,1%,3%) = "007" THEN GOTO SEPARATEDS
REM IF STR(SORT1$,1%,3%) = "049" THEN GOTO SEPARATEDS
REM IF STR(SORT1$,1%,3%) = "005" THEN GOTO SEPARATEDS
REM IF STR(SORT1$,1%,3%) = "002" THEN GOTO SEPARATEDS
/* (AWD044) */
REM IF STR(SORT1$,1%,3%) = "014" THEN GOTO SEPARATEDS
REM IF STR(SORT1$,1%,3%) = "017" THEN GOTO SEPARATEDS
REM IF STR(SORT1$,1%,3%) = "018" THEN GOTO SEPARATEDS

REM IF STR(SORT1$,1%,3%) = "028" THEN GOTO SEPARATEDS
REM IF STR(SORT1$,1%,3%) = "027" THEN GOTO SEPARATEDS
REM IF STR(SORT1$,1%,3%) = "048" THEN GOTO SEPARATEDS
REM IF STR(SORT1$,1%,3%) = "036" THEN GOTO SEPARATEDS
REM IF STR(SORT1$,1%,3%) = "019" THEN GOTO SEPARATEDS

/* (\AWD044) */
REM IF STR(SORT1$,1%,3%)  = "047" THEN                      ~
REM STR(GLSCNTR_KEY$,1%,11%) = STR(SORT1$,4%,6%) & "047"
REM IF STR(SORT1$,1%,3%) <> "047" THEN                      ~
REM STR(GLSCNTR_KEY$,1%,11%) = STR(SORT1$,4%,6%)
REM IF STR(SORT1$,6%,4%)  = "SP11" THEN                     ~
REM STR(GLSCNTR_KEY$,1%,11%) = "DSSP11"
REM RETURN
REM SEPARATEDS
REM IF STR(SORT1$,1%,3%)  = "047" THEN                      ~
REM STR(GLSCNTR_KEY$,1%,11%) = STR(SORT1$,4%,6%) & "047"
REM LUMP ALL THREE 267 DEPTS TOGETHER
REM IF STR(SORT1$,1%,3%)  = "007" THEN                      ~
REM STR(GLSCNTR_KEY$,1%,11%) = STR(SORT1$,4%,6%) & "007"
REM IF STR(SORT1$,1%,3%)  = "049" THEN                      ~
REM STR(GLSCNTR_KEY$,1%,11%) = STR(SORT1$,4%,6%) & "007"
REM IF STR(SORT1$,1%,3%)  = "005" THEN                      ~
REM STR(GLSCNTR_KEY$,1%,11%) = STR(SORT1$,4%,6%) & "007"
REM IF STR(SORT1$,1%,3%)  = "002" THEN                      ~
REM STR(GLSCNTR_KEY$,1%,11%) = STR(SORT1$,4%,6%) & "007"
/* (AWD044) */

REM IF STR(SORT1$,1%,3%)  = "014" THEN                      ~
REM STR(GLSCNTR_KEY$,1%,11%) = STR(SORT1$,4%,6%) & "007"
REM IF STR(SORT1$,1%,3%)  = "017" THEN                      ~
REM STR(GLSCNTR_KEY$,1%,11%) = STR(SORT1$,4%,6%) & "007"
REM IF STR(SORT1$,1%,3%)  = "018" THEN                      ~
REM STR(GLSCNTR_KEY$,1%,11%) = STR(SORT1$,4%,6%) & "007"
REM IF STR(SORT1$,1%,3%)  = "019" THEN                      ~
REM STR(GLSCNTR_KEY$,1%,11%) = STR(SORT1$,4%,6%) & "027"
REM IF STR(SORT1$,1%,3%)  = "027" THEN                      ~
REM STR(GLSCNTR_KEY$,1%,11%) = STR(SORT1$,4%,6%) & "027"
REM IF STR(SORT1$,1%,3%)  = "028" THEN                      ~
REM STR(GLSCNTR_KEY$,1%,11%) = STR(SORT1$,4%,6%) & "027"
REM IF STR(SORT1$,1%,3%)  = "036" THEN                      ~
REM STR(GLSCNTR_KEY$,1%,11%) = STR(SORT1$,4%,6%) & "027"
REM IF STR(SORT1$,1%,3%)  = "048" THEN                      ~
REM STR(GLSCNTR_KEY$,1%,11%) = STR(SORT1$,4%,6%) & "027"
/* (\AWD044) */

REM IF STR(SORT1$,6%,4%)  = "SP11" THEN                     ~
REM STR(GLSCNTR_KEY$,1%,11%) = "DSSP11"
REM RETURN


                                                 /* (AWD017) - END     */

        SORT_RECORDS                             /* (AWD018)        */
REM CALL "SHOSTAT" ("SORTING RECORDS FOR GLSGED " )  STOP
            mode% = 1%    :     gosub OPEN_WORK
            mode% = 3%    :     gosub OPEN_WORK
            init(" ") new_sort$(), part$, lt$, new_ged$, new_rec$(),     ~
                      lab_ged$, lab_rec$(), ultra$, patio$, dtDte$,      ~
                      new_key$, lab_key$
            mat new_sort_cnt% = zer
            max_sort% = 1%
            new_key$, new_ged$ = all(hex(00))    /* GED Glass Primary Key     */

            read #6,key > new_key$, using WORKFILE1 , new_key$, new_ged$,     ~
                                         new_rec$(), eod goto SORT_RECORDS_DONE

WORKFILE1:              FMT CH(62), CH(66), 2*CH(223)        /* (AWD036) */
                     goto SORT_RECORDS_FIRST

        SORT_RECORDS_NEXT
            read #6, using WORKFILE1 , new_key$, new_ged$, new_rec$(),     ~
                                    eod goto SORT_RECORDS_DONE


SORT_RECORDS_FIRST:
REM         ===============================================================
REM         RECORDS TO SKIP

            if str(new_rec$(),1%,1%) = "E" then goto SORT_RECORDS_NEXT
                                                  /* (EWD007)          */
            if str(new_key$,3%,5%) <> "99999" and str(new_key$,8%,5%) = "00999" ~
                                                   then goto SORT_RECORDS_NEXT
            so$       = str(new_rec$(),97%,8%)     /* Sales Order Number*/
            part$     = str(new_rec$(),59%,25%)    /* MFG Part Number   */
            lt$       = str(part$,7%,2%)           /* Liting Code       */
            ultra$    = str(new_rec$(),233%,1%)    /* (AWD033)          */
            patio$    = str(new_rec$(),234%,1%)    /* (AWD034) */
            dtDte$    = str(new_rec$(),235%,6%)    /* (AWD034) */

REM Remakes putting Alpha character in patio$
            patio% = 0%
            convert patio$ to patio%, data goto badPatio

badPatio:

            convert patio% to patio$, pic(0)

            init(" ") showMessage$
            showMessage$ = str(new_rec$(),211%,10%)
REM CALL "SHOSTAT" ("SHOW MESSAGE --> " & SHOWMESSAGE$)  STOP

REM IF LT$ > "82" AND LT$ < "88" THEN GOTO SORT_RECORDS_NEXT
REM IF LT$ > "96" AND LT$ <= "99" THEN GOTO SORT_RECORDS_NEXT

REM -------------------------------------------------------------
REM this is NOT the screen selection for GED/Bilco
                                                 /* 0 = GED Only       */
                                                 /* 1 = Bilco only     */
                                                 /* 2 = Both GED/Bilco */
            if str(new_rec$(),189%,1%) = "2" then goto OUTSIDE_PURCHASE
                                          /* (2) = Both GED and Bilco */
            if str(new_rec$(),189%,1%) <> "0" then goto SORT_RECORDS_NEXT
                                         /* Not Zero then must be    */
                                         /* BILCO Glass              */
REM ---------------------------------------------------------------
OUTSIDE_PURCHASE:
            gosub CHECK_GLASS             /* See if Outside Purchase   */
                                          /* Skip Special Glass        */
            if check% = 1% then goto SORT_RECORDS_NEXT

REM ---------------------------------------------------------------
REM Pass 0% get pre-cut; Pass 2% do not get pre-cut

            if rm_flag% <> 0% then goto GET_SORT    /* ONLY PRODUCTION DATA */

            lab_fil$ = str(new_rec$(),50%,9%)
/* (AWD024) */
            if rm_flag% = 1% or rm_flag% = 3% then goto not_remakes
            if pass% = 0% and str(lab_fil$,1%,6%) =  "STOCK1"        ~
                                                    then goto SORT_RECORDS_NEXT
            if pass% = 2% and str(lab_fil$,1%,6%) <> "STOCK1"        ~
                                                    then goto SORT_RECORDS_NEXT

not_remakes:

REM         ================================================================
REM         ----------------------------------------------------------------
REM         get sort information
REM         ---------------------------------------------------------------
GET_SORT:
/* (AWD036) */
REM !SORT$   = STR(NEW_GED$,1%,2%)          /* SPECIAL SORT CODE */
REM INTERCEPT$ = STR(NEW_KEY$,1,2)
            intercept$ = str(new_rec$(),241%,2%)
            sort$   = str(new_key$,8,5)
            sort1$  = " "
            sort% = 0%
            convert sort$ to sort%, data goto bad_sort

bad_sort:

REM !GOSUB UNPACK_SORT_CODE                  /* (AWD028) */
REM IF SORT% < 100 THEN GOTO NO_SHOWSTAT
REM CALL "SHOSTAT" ( " I AM HERE!!! " & SORT$)  STOP
REM NO_SHOWSTAT
REM CONVERT SORT$ TO SORT%, DATA GOTO BAD_SORT

REM BAD_SORT
                                                        /* The entire sort */
REM !IF TEMPERED% <> 1% THEN SORT1$  = STR(SS$(SORT%),1%,11%)
REM !CALL "SHOSTAT" ("SORT1 -->  " & SORT1$)  STOP
REM !IF TEMPERED% <> 1% THEN SORT1$ = STR(NEW_REC$(),211%,11%)
REM !IF TEMPERED% =  1% THEN SORT1$  = STR(SS_TEMP$(SORT%),1%,11%)
REM CALL "SHOSTAT" ("SORT REC ") STOP

/* Set in case no production IE tempered */
            lab_key$   = str(new_key$,1%,62%)
            sort1$ = str(new_rec$(),211%,11%)

            gosub FIND_SORT
            gosub ASSIGN_GLS_CNTR_SORT             /* (AWD017) */

REM !LAB_KEY$   = STR(NEW_KEY$,1%,62%)      /* (AWD036) */
REM !LAB_GED$   = STR(NEW_GED$,1%,66%)
REM !LAB_REC$() = STR(NEW_REC$(),1%,446%)

            lab_ged$   = str(new_ged$,1%,66%)
            lab_rec$() = str(new_rec$(),1%,446%)

            str(lab_rec$(),211%,11%) = str(sort1$,1,11)  /* First Filler Area */
            str(lab_rec$(),222%,11%) = str(glscntr_key$,1,11)
            str(lab_rec$(),233%,1%)  = ultra$          /* (AWD033) */
            str(lab_rec$(),234%,1%)  = patio$          /* (AWD034) */
            str(lab_rec$(),235%,6%)  = dtDte$          /* (AWD034) */
            str(lab_rec$(),241%,2%)  = intercept$      /* (AWD036) */
REM STR(LAB_REC$(),243%,3%)  = SCHEDSORT$
REM STR(LAB_REC$(),257%,7%)  = SCHEDSORT$              /* (CR1931) */
REM -------------------------------------------------  /* (CR1931) */
REM lab_rec$(),259%,10%) = style$                      /* (CR1931) */
REM lab_rec$(),269%,20%) = glstype$                    /* (CR1931) */
REM lab_rec$(),289%,01%) = "0" or "1" valance          /* (CR1931) */
/* (CR1931) MOVED SCHEDSORT$ NOT TO OVERWRITE STYLE$ */
            init(" ") style$
            style$ = str(lab_rec$(),259%,10%)

REM CALL "SHOSTAT" ("STYLE -> " & STYLE$) STOP

            str(lab_rec$(),290%,7%)  = schedSort$
/* CR3166 */			
			str(lab_rec$(),297%,4%)  = str(new_rec$(),290%,4%)   
			str(lab_rec$(),301%,40%) = str(new_rec$(),294%,40%)
            str(lab_rec$(),341%,2%)  = str(new_rec$(),334%,2%)			

REM !INIT(" ") CMG$
REM !CMG$ = STR(LAB_REC$(),243%,7%)
REM !INIT(" ") CMG$
REM !CMG$ = STR(LAB_REC$(),250%,7%)


REM !INIT(" ") LOGMESSAGE$
REM !LOGMESSAGE$ = LAB_KEY$ & LAB_GED$
REM !CALL "LOGFILE45" (LOGMESSAGE$)

            write #16, using FMTNEWSORTREC, lab_key$, lab_ged$, lab_rec$(),~
                                              eod goto SORT_RECORDS_ERROR
FMTNEWSORTREC:             FMT CH(62), CH(66), 2*CH(223)

                goto SORT_RECORDS_NEXT

        SORT_RECORDS_DONE
REM CALL "SHOSTAT" ("SORT DONE !!!!")
REM STOP
        return
        SORT_RECORDS_ERROR
           init(" ") so$
           so$       = str(new_rec$(),97%,8%)
           errormsg$ ="(Err)-Resorting Data = " & so$
           gosub error_prompt
        return


        FIND_SORT
          if temper_flag$ = "Y" then return /* Not Tempered or Remakes */
          if rmk% <> 0% then return
          if tempScrn% <> 0% then return

          gosub readGEDSCHED
          str(sort$,1%,2%) = intercept$
          convert sort% to str(sort$,3%,3%), pic(000)
          str(glscntr_key$,12%,2%) = intercept$
          str(sort1$,12%,2%)       = intercept$


          init(" ") lab_key$
          lab_key$   = str(new_key$,1%,62%)       /*Begin with Saved Sort  */
          str(lab_key$,3%,2%) = str(sort1$,4%,2%) /*Add Strength DS SS     */
REM STR(LAB_KEY$,8%,5%) = SORT$             /*ADD INTERCEPT AND FILE*/
REM STR(LAB_KEY$,05%,3%) = STR(SORT$,3%,3%) /*ADD INTERCEPT AND FILE*/
                                                  /*Number from GED SCHED  */
          str(lab_key$,13%,1%) = "0"                      /* Default 0     */
REM IF STR(SORT1$,4%,2%)  = "DS" THEN RETURN
          if str(sort1$,4%,2%)  = "DD" then return        /* (CR1987) */
          if str(sort1$,4%,2%)  = "DD" then return        /* (CR2260) */
          if str(sort1$,4%,2%)  = "TT" then return        /* (CR1987) */
          if str(sort1$,4%,2%)  = "QQ" then return        /* (CR1987) */
          str(lab_key$,13%,1%) = "1"                      /* Means TB 1    */
          if str(sort1$,10%,2%) = "TB" then return
                                                          /* Means TT/BB 2 */
          if str(sort1$,10%,2%) = "TT" then str(lab_key$,13%,1%) = "2"
          if str(sort1$,10%,2%) = "TT" then return
          if str(sort1$,10%,2%) = "BB" then str(lab_key$,13%,1%) = "2"
          if str(sort1$,10%,2%) = "BB" then return

          str(lab_key$,13%,1%) = "9"
return

/* (AWD036) */
REM          str(glscntr_key$,12%,2%) = intercept$
REM          str(sort1$,12%,2%)        = intercept$
REM          if str(glscntr_key$,1,13) <> " " and        ~
REM                   str(sort1$,1,13) <> " " then goto NO_SORT_ERR
REM CALL "SHOSTAT" ("HERE AT NEW SORT1 ")   STOP

REM NO_SORT_ERR
REM IF LT$ > "82" AND LT$ < "88" THEN RETURN
REM IF LT$ > "96" AND LT$ <= "99" THEN RETURN
REM --------------------------------------------------------------------
REM FIND THE NEW GLS SORT IN ARRAY AND ADD ONE TO COUNTER
REM --------------------------------------------------------------------
REM FOR SORT% = 1% TO MAX_SORT%
REM IF GLSCNTR_KEY$ = NEW_SORT$(SORT%) THEN GOTO FOUND_SORT
REM NEXT SORT%
REM SORT%     = MAX_SORT%
REM MAX_SORT% = MAX_SORT% + 1%
REM NEW_SORT$(SORT%) = GLSCNTR_KEY$
REM RETURN
REM FOUND_SORT
REM NEW_SORT_CNT%(SORT%) = NEW_SORT_CNT%(SORT%) + 1%
REM RETURN

        UPDATE_CNTR

            for sort% = 1% to max_sort%

               if glscntr_key$ = new_sort$(sort%) then goto GET_CNTR
            next sort%

/* (AWD028)  change from 200 to 256 error */
        sort% = 999%               /* No SORT, BETTER NOT HAPPEN */
        GET_CNTR
        return


        OPEN_WORK
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#16,mode$, 500%, f2%)
            if f2% <> 0% then goto OPEN_WORK_ERR

        return
OPEN_WORK_ERR:
        errormsg$ = "Can Not Open AWDPLNWK"
        gosub error_prompt

        return
		
       lookup_inside_outside
           init(" ") readkey$
           str(readkey$,1%,9%)   = "PLANBILCO"
           str(readkey$,10%,15%) = glass_code$ & " "
           read #2,key = readkey$, using L03240, txt_planbilco$,         ~
                                                eod goto lookup_again
           goto lookup_low_e
         lookup_again
           str(readkey$,1%,9%)   = "PLANBILCO"
           str(readkey$,10%,15%) = glass_code$ & "B"
           read #2,key = readkey$, using L03240, txt_planbilco$,         ~
                                                eod goto lookup_inside_outside_done
         lookup_low_e
           if str(txt_planbilco$,12%,1%) = "+" then three_codes
         four_codes
              low_e_key$ = str(txt_planbilco$,2%,2%)
              gosub lookup_low_e_g
              low_e_key$ = str(txt_planbilco$,10%,2%)
              gosub lookup_low_e_g
              low_e_key$ = str(txt_planbilco$,16%,2%)
              gosub lookup_low_e_g
              low_e_key$ = str(txt_planbilco$,24%,2%)
              gosub lookup_low_e_g
              goto lookup_inside_outside_done
         three_codes
              low_e_key$ = str(txt_planbilco$,2%,2%)
              gosub lookup_low_e_g
              low_e_key$ = str(txt_planbilco$,10%,2%)
              gosub lookup_low_e_g
              low_e_key$ = str(txt_planbilco$,18%,2%)
              gosub lookup_low_e_g

       lookup_inside_outside_done
           return  
           
       lookup_low_e_g
           init(" ") readkey$
           str(readkey$,1%,9%)   = "LOWE     "
           str(readkey$,10%,15%) = low_e_key$
           read #2,key = readkey$, using L03240, txt_inside_outside$,         ~
                                                eod goto lookup_low_e_g_done
                                        
       lookup_low_e_g_done
           return  
  
       lookup_wnd_color
           init(" ") readkey$, wnd_color$
           str(readkey$,1%,9%)   = "COLOR    "
           str(readkey$,10%,15%) = wnd_color_code$
           read #2,key = readkey$, using L_CLR, wnd_color$,         ~
                                                eod goto lookup_wnd_color_done
L_CLR:        FMT POS(30), CH(6)
                                        
       lookup_wnd_color_done
           return  
		   
       lookup_grid_color

	    readkey$ = "GRDCOLOR                "
            str(readkey$,10,1) = str(sub_part$,3,1) 
	    read #2,key = readkey$,using GRDCOLOR, gc_desc$, eod goto L64005
GRDCOLOR:   FMT POS(25), CH(32)
	    p% = 0%
            for l% = 1% to 16%
                if str(gc_desc$,l%,1%) <> "-" then goto L64003
		p% = l% + 2%
		l% = 17%
L64003:
	    next l%
           if p% = 0% then goto L64005                        
	       gl_color$ = str(gc_desc$,p%,6%)
		return 
		
L64005:    init(" ") readkey$
           str(readkey$,1%,9%)   = "COLOR    "
           str(readkey$,10%,15%) = str(sub_part$,3,1) 
           read #1,key = readkey$, using L_CLR, gl_color$,         ~
                                                eod goto L_CLR_DONE
L_CLR_DONE		   
	   return

       lookup_glass_text
           init(" ") readkey$, txt_glass$
           str(readkey$,1%,9%)   = "GLASS    "
           str(readkey$,10%,15%) = glass_code$
           read #2,key = readkey$, using L03240, txt_glass$,         ~
                                                eod goto lookup_glass_text_done
                                        
       lookup_glass_text_done
           return  
		   
       load_interdesc
          interdesc$ = "__"
          str(readkey$,1,9) = "INTERDESC"
		  str(readkey$,10%,15%) = str(sort1$,12%,2%)
        interdesc_next
           read #2,key = readkey$, using L03240, interdesc$,         ~
                                               eod goto interdesc_done
INTERDESC_FMT:         FMT  CH(24), CH(32)

         interdesc_done
        return	

        set_prt_num
		    gosub lookup_ups
                                                 /* Set for Backorders */
            if str(lab_rec$(),341%,2%) = "21" then str(prt_num$,1%,1%) = "9"
                                                 /* Set for UPS S.O.'s */
            if str(lab_rec$(),341%,2%) = "04" or str(lab_rec$(),341%,2%) = "06" ~
              then prt_num$ = "SAM"
            if str(lab_rec$(),341%,2%) = "05" then prt_num$ = "DIS"

            if str(lab_rec$(),69%,1%) = "4" then prt_num$ = "TSO"
            if str(lab_rec$(),69%,1%) = "5" then prt_num$ = "BSO"
            if str(lab_rec$(),69%,1%) = "6" then prt_num$ = "FGO"
            if str(lab_rec$(),69%,1%) = "7" then prt_num$ = "OGO"

            if str(lab_rec$(),341%,2%) = "20" then prt_num$ = "IND"
                                           
        return

		
		lookup_ups                                         
             init(" ") readkey$
             str(readkey$,1%,9%)  = "PLAN UPS "
             str(readkey$,10%,2%) = str(lab_rec$(),341%,2%)
             read #2, key = readkey$, eod goto no_ups
                   prt_num$ = "UPS"
        no_ups
        return
		
/* (AWD028) */
REM  !  UNPACK_SORT_CODE
REM  !      GET STR(SORT$,1,2), USING L63510, SORT%
REM  ! L63510:    REM  !         FMT BI(2)
REM  !      CONVERT SORT% TO SORT$, PIC(00)
REM  !  RETURN
/* (/AWD028) */

        error_prompt
           error% = 1%
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        remove_trail
         script$ = "RMTRAIL"
         lb1%, lb2% = 0%
         call "LINK" addr(script$, lb1%, lb2%)
        return

        remove_trail_rmk
         script$ = "RMTLRMK"
         lb1%, lb2% = 0%
         call "LINK" addr(script$, lb1%, lb2%)
        return

        remove_trail_temp
         if valance% <> 1% then return
         script$ = "RMTLTMP"
         lb1%, lb2% = 0%
         call "LINK" addr(script$, lb1%, lb2%)
        return

        copy_file_remakes

		 lbl%, lb2% = 0%
		 call "LINK" addr(script$,lbl%,lb2%)
		return 
		
        readGEDSCHED
REM IF STR(SORT1$,4%,2%) = "SS" THEN RETURN
          sort% = 999%
          init(" ") readkey$, desc$, schedSort$
          str(readkey$,1%,9%)   = "GED SCHED"
          str(readkey$,10%,15%) = sort1$
          if str(sort1$,4%,2%) = "SS" then str(readkey$,10%,11%) = sort1$
          read #2,key = readkey$, using L03240, desc$, eod goto noGEDSCHED

             convert str(desc$,1%,3%) to sort%, data goto noGEDSCHED

             convert sort% to schedSort$, pic(000)

REM NEW_SORT$(SORT%) = SORT1$

noGEDSCHED
        return
        
/* CR3274 Read Gencodes for the script name associated to the file number */
        readSCRIPTNAME
		  init(" ") readkey$, desc$, script$
          str(readkey$,1%,9%)   = "PGMSCRIPT"
          str(readkey$,10%,11%) = "APCPLB45" & file_nbr$
          read #2,key = readkey$, using L03240, desc$, eod goto noSCRIPTNAME

             script$ = str(desc$,1%,8%)
noSCRIPTNAME:
		return 
		
        exit_end
            lb1% = 0% : lb2% = 0%

           if ff% = 17% then gosub remove_trail
           if ff% = 18% then gosub remove_trail_rmk
           if ff% = 11% then gosub remove_trail_temp
           call "FILEBGON" (#16)
		   
/* CR3274 moving remake files for NC */	 
          if schema% <> 1% then goto not_nc_gls
		  convert ff% to file_nbr$, pic(000)
		  gosub readSCRIPTNAME
          if script$ = " " then goto not_nc_gls	
          lbl%, lb2% = 0%
		  call "LINK" addr(script$,lbl%,lb2%)
		  
not_nc_gls:		   
           file$ = sav_file$
           bat$ = cnt_ord$

        end








