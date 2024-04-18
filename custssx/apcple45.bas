        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLE45 - Program (APCPLA45)        *~
            *  Creation Date     - 12/04/96 - File - (@???BIL@)         *~
            *  Last Modified Date- 01/01/06                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Create BILCO Glass Bridge File with  *~
            *                      Batches. Unit Quantity is Consolidate*~
            *                      for line sizes that fall within the  *~
            *                      Processing Sort Sequence.            *~
            * x$ = bin(35%,1)      STUFF Pound symbol into X$           *~
            *                                                           *~
            *  MOD - Consolidate Glass the Same Size (Maintaining)      *~
            *        Sequence. ( Use 'SAV_REC$' to Save Reord )         *~
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
            *        (@GLSBLB@) - Normal Production Glass  (EWD011)     *~
            *        (@RMKBLK@) - Remake both                           *~
            *        (@RMKBL1@) - Remake Glass Bridge File for loads    *~
            *        (@RMKBL2@) - Remake Glass Not Used                 *~
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
            *          !          (Only Production - Not Remakes )!     *~ 
            * 01/01/06 ! (PAR000) CR347 Mod for new sub part      ! CMG *~
            *************************************************************


        sub "APCPLE45" (size%,           /* Specified Batch Size      */ ~
                        scr_sel$,        /* Screen Selection (EWD003) */ ~
                        glass_dte$,      /* Glass Production Date     */ ~
                        scr_dte$,        /* Planned Production Date   */ ~
                        scr_dte1$,       /* Planned Prod. Unformated  */ ~
                        file$,           /* Name of Optimized File    */ ~
                        bat$,            /* Number of Batches Created */ ~
                        rm_flag%,        /* Remake Flag 0% or 1%      */ ~
                        #1,              /* (APCPLNWK) Label Detail Fl*/ ~
                        #3,              /* (APCPLNGR) Label Detail Fl*/ ~
                        #2,              /* (@GLSBLB@) Optimizer File */ ~
                        #7,              /* (@RMKBLK@) Remake Both    */ ~
                        #4,              /* (@RMKBL1@) Remake Product */ ~
                        #5,              /* (GENCODES) Optimizer File */ ~
                        #6 )             /* (@RMKBL2@) Remake In House*/
                                         /* (EWD001) - Mod 07/27/98   */

        dim glass_dte$8, glass_dte1$10,  /* Glass Completion Date     */ ~
            scr_dte$8, bat$3,            /* Glass Production Date     */ ~
            scr_dte1$8,                  /* Glass Prod. Date UNFORMAT */ ~
            dt_dept$3,                   /* Department Code           */ ~
            bat_rec$165, bin$1,          /* Batch Record, ROUTE CODE  */ ~
            sav_rec$165,                 /* Batch Record              */ ~
            seq$3,                       /* Item Numbers              */ ~
            mat_spec$11,                 /* Material Spec's           */ ~
            mat_spec_bot$11,             /* Material Spec's (EWD008)  */ ~
            harp$4,                      /* Harp Rack Location '@XXT' */ ~
/*PAR000*/  lab_rec$(2%)159, lab_ged$66, /* Glass Label Record        */ ~
            lab_key$10,                  /* Sort Key          (EWD010)*/ ~
            sandwich$10,                 /* Glass Sandwich    (EWD010)*/ ~
            table_no$1,                  /* Glass Table No.   (EWD010)*/ ~
            table$1,                     /* Glass Table No.   (EWD010)*/ ~
            sav_table$1,                 /* Save Table No.    (EWD010)*/ ~
            x$1,                         /* Store Pound sysbol(EWD010)*/ ~    
            stock$5,                     /* STOCK Indicator           */ ~
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
            msg$(3%)79,                  /* ASKUSER Info Text         */ ~
            errormsg$79,                 /* Error Message Text        */ ~
            sort$2, sav_sort$2,          /* Glass Sort Seq. Codes     */ ~
            readkey$24, descr$32,        /* Gencodes Key              */ ~
            short_date$8, short_date2$8, /* force to short date       */ ~
            short_date3$8,               /*  ""   ""                  */ ~
            spacer$6, sav_spacer$6       /* Spacer Thickness Codes    */

        dim so$8,                        /* S.O.                       */~
            scr_sel$1,                   /* Screen Selection (EWD003)  */~
/*PAR000*/  rm_key$23, rm_rec$(2%)192,   /* Remake Key and Record      */~
            ff$8,                        /* Remake Production Day      */~
            rm_st_time$8, rm_st$1,       /* Remake Stat Time Change    */~
            rm_reason$2, chk_st$1,       /* Remake Reason Code         */~
            rm_st_dte$6                  /* Remake Status Date         */

        dim f2%(10%),                    /* = 0 if the file is open    */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            mode$5,                      /* File Mode (EWD010)         */~
            rslt$(10%)20                 /* Text from file opening     */


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCPLNWK ! Glass Work File (Labels)                 *~
            * #3  ! APCPLNGR ! Glass Sched/Remake File                  *~
            * #2  ! @GLSBLB@ ! Glass Batch File for Bilco Glass System  *~
            * #4  ! @RMKBL1@ ! Remake Batch File for Bilco Glass Production*~
            * #5  ! GENCODES ! Master System Table File                 *~
            * #6  ! @RMKBL2@ ! Remake Batch Bilco Glass Not Used (EWD001)*~
            * #7  ! @RMKBLK@ ! Remake Batch File for Bilco Glass All    *~
            * #10 ! EWDPLNWK ! New Sort File for Bilco                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
                                               /* (EWD010)              */
            select #10,   "EWDPLNWK",                                    ~
/*(PAR000)*/            varc,     indexed,  recsize = 394,               ~
                        keypos =   1, keylen =   10,                     ~
                        alt key  1, keypos =    11, keylen =  66


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
            
            ff$ = "@GLSBLB@" : ff% = 2%        /* Only (one) Pass       */
            sort_bilco% = 0%                   /* (EWD010) Sort File    */
                                               /* Create Glass Batches  */
            call "OPENFILE" (#ff%, "IO   ", f2%(ff%), rslt$(ff%), axd$ )
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

L01360:     if sort_bilco% = 0% then gosub sort_bilco /* (EWD010)        */
            
            bat_no% = 1%
            qty%, count% = 0%
            call "SHOSTAT" ("Creating Batch(es)-"& file$ )
            sav_table$ = " "                        /* (EWD010)         */  
            lab_ged$ = all(hex(00))
            lab_key$ = all(hex(00))                  /* (EWD010) - Begin */
            if sort_bilco% = 1% then                                       ~
               read #1,key > lab_ged$, using L01470, lab_ged$, lab_rec$(), ~
                                                   eod goto create_done    ~
                                else                                       ~
               read #10,key > lab_key$, using L01475, lab_key$, lab_ged$,  ~
                                          lab_rec$(), eod goto create_done
                                                     /* (EWD010) - End   */ 
            sav_sort$   = str(lab_ged$,1%,2%)
            sav_spacer$ = str(lab_ged$,3%,6%)
            if sort_bilco% = 0% then sav_table$ = str(lab_key$,6%,1%)
            goto L01480
        create_next                                  /* (EWD010) - Begin */
            if sort_bilco% = 1% then                                       ~
               read #1, using L01470, lab_ged$, lab_rec$(),                ~
                                                      eod goto create_done ~
                                else                                       ~
               read #10, using L01475, lab_key$, lab_ged$, lab_rec$(),     ~
                                                      eod goto create_done
                                                     /* (EWD010) - End   */
L01470:       FMT CH(66), 2*CH(159)
L01475:       FMT CH(10), CH(66), 2*CH(159) 
L01480:     dt_dept$ = str(lab_rec$(),183%,3%)   /* 09/13/2000           */
       
            gosub check_glass
            if check% = 1% then goto create_next

            gosub check_glass12                  /* (EWD011)             */
            if check1% = 0% then goto create_next

                                                 /* (EWD001) - Begin     */
            gosub check_remake
            if rm_ok% = 0% then goto create_next /* Skip this Pass       */
                                                 /* (EWD001) - End       */
            sort$   = str(lab_ged$,1%,2%)
            spacer$ = str(lab_ged$,3%,6%)
                                                 /* (EWD010)             */
            if sort_bilco% = 0% then table$ = str(lab_key$,6%,1%)

            so$ = str(lab_rec$(),97%,8%)
            if str(lab_rec$(),1%,1%) = "E" then goto create_next
            if str(lab_rec$(),189%,1%) = "2" then goto L01590 /* Remake Gls*/
                                                   /* Both GED/BILCO    */
            if str(lab_rec$(),189%,1%) <> "1" then goto create_next
                                                   /* Check Bilco Only  */
L01590:     if sort_bilco% = 1% then goto L01595   /* (EWD010)          */
               if sav_table$ = table$ then goto L01595
                  goto L01600 
L01595:     if sav_spacer$ = spacer$ then goto L01650
L01600:        gosub build_end
               count% = 0%
               sav_spacer$ = spacer$
               sav_sort$   = sort$
               if sort_bilco% = 0% then sav_table$ = table$ /* (EWD010) */
               goto L01670
L01650:     if sav_sort$ <> sort$ then goto L01600

L01670:     stock$ = str(lab_rec$(),50%,5%)                  /* Stock  */
            bin$ = " "                                 /* Set to Blank */
            if stock$ <> "STOCK" then goto L01730
                                            /* (EWD006) - 06/29/00     */
        REM    bin$ = "P"                   /* Leave a Hole for Stock  */
                                            /* For All Models 01/26/99 */
                                            /* (EWD006)                */

L01730:     harp$ = str(lab_rec$(),55%,4% )   /* N/A - For Aluminum    */
            ty% = 0%
        REM TY$    = STR(LAB_REC$(),11%,2%)   /* Glass Type CL,LE,Etc  */
            ty$   = str(lab_rec$(),63%,2%)    /* Glass Type Code       */
            convert ty$ to ty%, data goto L01780
L01780:
            temp$ = str(lab_rec$(),153%,1%)
            if temp$ = "*" then goto create_next

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
            gosub build_detail
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

        check_glass                      /* Test Liting Codes in Table */
           init(" ") readkey$ : check% = 0%   /* to see if Skipped     */
           str(readkey$,1%,9%)   = "PLANGLOUT"
           str(readkey$,10%,15%) = str(lab_rec$(),65%,2%)
           read #5,key = readkey$, eod goto L02150
           check% = 1%
L02150: return

        check_glass12                    /* Test Liting Codes in Table */
           init(" ") readkey$ : check1% = 0%  /* to see if Skipped     */
           if scr_sel$ <> "2" then return     /* Not Applicable        */

           str(readkey$,1%,9%)   = "GLASS12  "
           str(readkey$,10%,3%)  = str(lab_rec$(),183%,3%)/* Department*/
           str(readkey$,13%,6%)  = str(lab_ged$,3%,6%)    /* Spacer    */
           read #5,key = readkey$, eod goto L02160
           check1% = 1%                                   /* Skip      */
L02160: return

        build_title                            /* NEW TITLE EACH BATCH */
          init(" ") bat_rec$, sav_model$
          errormsg$="(Error)-(Title) Rec in Batch- "& file$
          inc$ = " BATCH (XXX)"
          convert bat_no% to str(inc$,9%,3%), pic(000)

        if sort_bilco% = 0% then goto build_title_table   
          str(bat_rec$,1%,2%) = "NT"                      /* New Title */
          str(bat_rec$,3%,32%)= file$ & inc$
          str(bat_rec$,35%,131%) = " "                    /* Line Feed */
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
          str(bat_rec$,125%,11%)= mat_spec$             /* Material Spc*/
          count% = count% + 1%
            str(bat_rec$,136%,23%) = "                       "
            str(bat_rec$,159%,1%)  = bin$        /* Put Hole for Stock */
            str(bat_rec$,160%,6%)  = " "

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
             sav_sort$   = str(lab_ged$,1%,2%)
             sav_spacer$ = str(lab_ged$,3%,6%)          /* (EWD010)     */
             if sort_bilco% = 0% then sav_table$ = str(lab_key$,6%,1%)
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
             if ty% = 1% then str(mat_spec$,2%,2%)  = "PC" 
             if ty% = 1% then str(mat_spec$,10%,2%) = "PC"

REM             if ty% = 4% then str(mat_spec$,2%,2%) = "PC"
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
            init(" ") mat_spec$, readkey$, descr$, mat_spec_bot$
            str(readkey$,1%,9%) = "PLANBILCO"
            str(readkey$,10%,2%) = ty$
            read #5,key = readkey$, using L03470 , descr$, eod goto L03490
L03470:        FMT POS(25), CH(30)
            mat_spec$ = str(descr$,1%,11%)
            mat_spec_bot$ = str(descr$,15%,11%)     /* (EWD008)     */
L03490: return

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
           read #3,key = rm_key$, using L03500, rm_reason$,eod goto L03525
L03500:       FMT POS(34), CH(2)
           rm_reason% = 0%
           convert rm_reason$ to rm_reason%, data goto L03510
L03510                                           /* (EWD002) - Begin     */
           if ff% = 7% then goto L03520          /* Both     (EWD003)    */
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
           read #3,hold,key = rm_key$, using L03730 , rm_rec$(),           ~
                                                  eod goto schedule_glass
L03730:       FMT 2*CH(192)
           chk_st$ = str(rm_rec$(),13%,1%)
           if chk_st$ = "2" then goto L04020     /* Error Condition    */       
              delete #3
        REM - Re-Schedule Glass for Re-Make      /* Found in Gls Batch */
              str(rm_rec$(),1%,6%) = str(glass_dte1$,1%,6%) /* Prod Dte*/
              str(rm_rec$(),7%,6%) = str(glass_dte1$,1%,6%) /* Scan Dte*/
              str(rm_rec$(),13%,1%) = rm_st$       /*Chg Status 0 to 1 */
              str(rm_rec$(),14%,8%) = rm_st_time$  /*Time of Stat Chang*/
                                                 /* (EWD002) Leave     */
                                                 /* Glass Barcode alone*/
                                                 /* Re-Make No Alone   */
                                                 /* and Reason Code alone*/ 
              str(rm_rec$(),36%,6%) = date       /* Date of Stat Change*/
                                                 /* (EWD002) 42 - 64   */
                                                 /* Contains remake    */
                                                 /* Scan Date/Time     */ 
              str(rm_rec$(),242%,5%) = str(lab_rec$(),176%,5%)
                                                 /* Update seq No.     */ 
              goto L03970
        schedule_glass                           /* Create Glass Data  */
           str(rm_rec$(),1%,6%) = str(glass_dte1$,1%,6%) /*GlassProd Dt*/
           str(rm_rec$(),7%,6%) = str(glass_dte1$,1%,6%) /*GlassProd Dt*/
           str(rm_rec$(),13%,1%) = rm_st$        /* Scheduled Glass    */
           str(rm_rec$(),14%,8%) = rm_st_time$    /*Time of Stat Change*/
           str(rm_rec$(),22%,9%) = str(lab_ged$,53%,9%) /*Glass Barcode*/
           str(rm_rec$(),31%,3%) = str(lab_rec$(),186%,3%) /* Remake No*/
           str(rm_rec$(),34%,2%) = rm_reason$      /* Glass Reason Code*/
           str(rm_rec$(),36%,6%) = date            /* Date of Stat Chan*/
           str(rm_rec$(),42%,2%) = str(lab_rec$(),178%,2%) /*ScnedShift*/
                                                 /* (EWD002)           */
                                                 /* Time 24 hour clock */
           str(rm_rec$(),44%,8%) = time          /* Scheduled Time     */
           str(rm_rec$(),52%,6%) = date          /* Scheduled Date Today*/
           str(rm_rec$(),58%,3%) = userid$       /* Who Scheduled Glass*/
           str(rm_rec$(),61%,4%) = "    "        /* Completion Calc    */
           str(rm_rec$(),65%,2%) = "  "          /* Growth Area        */
           str(rm_rec$(),67%,318%) = lab_rec$()  /* Calculated Data    */
                                                 /* Check for INDY     */
           if str(lab_rec$(),190%,1%) = "1" then   /*Set as Completed  */~
                                          str(rm_rec$(),13%,1%) = "2"   
                                                 /* (EWD002) - End     */
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
            mode% = 1% : gosub open_work
            mode% = 3% : gosub open_work

            lab_ged$ = all(hex(00))
            read #1,key > lab_ged$, using L01470   , lab_ged$, lab_rec$(), ~
                                                 eod goto sort_bilco_done
            goto sb_1
        sort_bilco_next
            read #1, using L01470, lab_ged$, lab_rec$(),eod goto sort_bilco_done
sb_1:       dt_dept$  = str(lab_rec$(),183%,3%)
            sandwich$ = str(lab_rec$(),115%,10%)   /* Find Glass Strength  */ 
            dd% = 0%
            dd% = pos(sandwich$ = "4")  /* dd% > 0% then Double Strength */

            gosub find_table_no
            if table_no% = 0% then goto sort_bilco_next

            lab_key$ = all(hex(00))
            sort% = sort% + 1%
            convert sort% to str(lab_key$,1%,5%), pic(00000)

            str(lab_key$,6%,1%) = table_no$

            str(lab_key$,7%,1%) = "1"                     /* Single  */
            if dd% <> 0% then str(lab_key$,7%,1%) = "2"   /* Double  */
            write #10, using sb_fmt, lab_key$, lab_ged$, lab_rec$(),        ~
                                               eod goto sort_bilco_next
sb_fmt:        FMT CH(10), CH(66), 2*CH(159)

            goto sort_bilco_next
        sort_bilco_done

        return   

        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#10,mode$, 500%, f2%)
            if f2% <> 0% then goto open_work_error
        return
open_work_error
            errormsg$ = "(Error) Cannot Open (EWDPLNWK)"
            gosub error_prompt
        return
        delete_work
            call "FILEBGON" (#10)
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


        exit_program

        end

                              
