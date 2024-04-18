        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLH45 - Program (APCPLA45)        *~
            *  Creation Date     - 02/02/2017                           *~
            *  Last Modified Date-                                     *~
            *  Written By        - Christie Norman                      *~
            *                                                           *~
            *  Description       - Create  GED  Glass Bridge File with  *~
            *                      Batches for processing Special       *~
            *                      Shapes                               *~
            *                                                           *~
            * x$ = bin(35%,1)      STUFF Pound symbol into X$           *~
            *                                                           *~
            *  MOD - One (1) Title and (1) Header Record per batch      *~
            *        Identification Field. Use 'MODEL' as the           *~
            *        Identifier instead of 'LOAD'.                      *~
            *                                                           *~
            *      - RM_FLAG%   - 0% Production                         *~
            *                     1% Remake                             *~
            *                     2% Tempered Production                *~
            *                     3% Tempered Remake                    *~
            *                     4% Laminate Glass                     *~
            *                     5% Laminate remakes                   *~
            *                                                           *~
            *                                                           *~
            *      - Bridge File Names                                  *~
            *        (@SHAPE2@) - Batch Created Manually        (AWD001)*~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *02/02/2017! New Program for (AWD) - (CR503)          ! CMN *~
            *************************************************************

                                          /* ( Bilco Glass Cutter )    */
        sub "APCPLH45" (size%,            /* Specified Batch Size      */~
                        scr_sel$,         /* Screen Selection          */~
                        glass_dte$,       /* Glass Production Date     */~
                        scr_dte$,         /* Planned Production Date   */~
                        scr_dte1$,        /* Planned Prod Date Unformat*/~
                        file$,            /* Name of Optimized File    */~
                        bat$,             /* Number of Batches Created */~
                        #1,               /* (GENCODES) TABLES         */~
                        #27)              /* Work File                 */


        dim scr_sel$1,                   /* Screen Selection          */ ~
            glass_dte$8, glass_dte1$10,  /* Completion Date           */ ~
            temp_date$10,temp_date2$10,  /* Working storage           */ ~
            scr_dte$8,                   /* Production Date           */ ~
            scr_dte1$8,                  /* Production Date Unform    */ ~
            bat_no$3,                    /* Batch Number              */ ~
            bat_hdr$220,                 /* Batch Header Record       */ ~
            bat_order$220,               /* Batch Order Record        */ ~
            bat_dtl$(2%)192,             /* Batch Detail Record       */ ~
            bat_dtl1$(2%)192,            /* Addition Detail Record    */ ~
            cnt_dtl$3,                   /* Count Order Details       */ ~
            x$1,                         /* Store Quote sysbol        */ ~
            y$2,                         /* Store Comma               */ ~
            model$3,                     /* Model Product Code        */ ~
            file$20,                     /* Batch File Name           */ ~
            sh_key$50,                   /* SH work file key          */ ~
            sh_rec$256,                  /* SH Record                 */ ~
            bil$(15%)20,                 /* Batch File Data           */ ~
            hdr$40, errmsg$(10%)50,      /* ASKUSER Header Text       */ ~
            msg$(3%)79,                  /* ASKUSER Info Text         */ ~
            errormsg$79                  /* Error Message Text        */


        dim space_d$10,                  /* Spacer Description        */ ~
            sandwich$10,                 /* Glass Sandwich            */ ~
            width_d$8,                   /* Width Size Deciamal       */ ~
            height_d$8,                  /* Height Size Decimal       */ ~
            mirror$1,                    /* Mirror Image              */ ~
            conrnerRadius$8,             /* Field 21 Corner Radius    */ ~
            dimA_d$8,                    /* Shape Dimension A         */ ~
            dimB_d$8,                    /* Shape Dimension B         */ ~
            dimC_d$8,                    /* Shape Dimension C         */ ~
            dimD_d$8,                    /* Shape Dimension D         */ ~
            dimE_d$8,                    /* Shape Dimension E         */ ~
            dimF_d$8,                    /* Shape Dimension F         */ ~
            t_k$6,                       /* Overall Thickness         */ ~
            gs$2,                        /* Blank or "G1"             */ ~
            color$10,                    /* Color Code / Table        */ ~
            muttin$8,                    /* Grid Code/Liting          */ ~
            w_adj$6,                     /* Width Adjustment Code     */ ~
            h_adj$6,                     /* Height Adjustment Code    */ ~
            ged_txt$40,                  /* Field 16 - Text           */ ~
            interoffset$7,               /* Offset by intercept       */ ~
            ged_shape$10,                /* Ged Shape Code            */ ~
            ged_fields$10,               /* Data Entry Values         */ ~
            number$8,                    /* Generic Nmbr-format field */ ~
            part$25                      /* Part Number               */


        dim f2%(10%),                    /* = 0 if the file is open    */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! GENCODES ! Master System Table File                 *~
            * #2  ! @GEDSHP@ ! GED Special Shape Bridge File            *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #02, "@GLSSHP@", consec, recsize = 384

            x$ = bin(34%,1)               /* Suff a '"' Quote into X$  */
            y$ = ", "                     /* Set to ',' = Comma        */

            size% = 0%

            glass_dte1$ = glass_dte$
            if len(glass_dte1$) = 10% then                               ~
                call "DATUFMTC" (glass_dte1$)                            ~
            else                                                         ~
                call "DATUNFMT" (glass_dte1$)

            ff$ = "@GEDSHP@" : ff% = 2%        /* Create Glass Batches  */

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

L01360:


            errmsg$(1%) = "(Error)-(Header) Rec in Batch- "& file$
            errmsg$(2%) = "(Error)-(Header) Rec in Batch- "& file$
            errmsg$(3%) = "(Error)-(Item) Rec in Batch- "& file$
            errmsg$(4%) = " "

            init(" ") sh_key$, sh_rec$
            bat_no% = 1%
            cnt_ord1%, cnt_dtl1% = 0%


            call "SHOSTAT" ("Creating Batch(es)-"& file$ )

REM            GOSUB BUILD_TITLE
            gosub write_batch_header

REM            GOSUB BUILD_HEADER
REM

REM            GOSUB BUILD_DETAIL
detailNxt:
            gosub readRecord
             if rec% = 0% then goto detailDone
             if cnt_ord1% <> 0% then goto noHeader
               space_d$ = bil$(13%)                       /* Spacer Desc */
               gosub write_order_header
noHeader:
              gosub write_detail
            goto detailNxt

detailDone:
            gosub write_batch_trailer
            close #ff%
            convert (bat_no% - 1%) to bat$, pic(000)

        goto exit_program


        write_batch_header
          init(" ") bat_hdr$, bat_dtl1$()
          err% = 1%                                /* Set Error Code */
          str(file$,16%,5%) = "(GDS)"
          str(bat_dtl1$(),1%,480%) = " "
          str(bat_hdr$,1%,6%)   = "<V1.0>"
          str(bat_hdr$,7%,2%)   = y$                 /* Comma Space  */
          str(bat_hdr$,9%,1%)   = x$                 /* Set Quote    */
          str(bat_hdr$,10%,8%)  = scr_dte$
          str(bat_hdr$,18%,9%)  = " - AWD - "
          str(bat_hdr$,27%,20%) = file$      /* Entered in 'ACPLA45' */
          str(bat_hdr$,47%,1%)  = x$                 /* Set Quote    */
          str(bat_hdr$,48%,173%) = " "

          write #ff%, str(bat_hdr$,1%,220%) & str(bat_dtl1$(),1%,164%), ~
                  eod goto header_error

        return
header_error:
          errormsg$ = errmsg$(err%)
          gosub error_prompt
        return

        write_order_header
          err% = 2%                             /* Set Error Code    */
          init(" ") bat_order$, bat_dtl1$()

          cnt_ord1% = cnt_ord1% + 1%
          convert cnt_ord1% to cnt_ord$, pic(000)/* Count ord head */

          bat_no$ = cnt_ord$
          temp_date$ = scr_dte1$
          call "DATFMTC" (temp_date$, comp%, temp_date2$)
          str(bat_dtl1$(),1%,384%) = " "

          str(bat_order$,1%,1%)  = "*"            /* Field (0)       */
          str(bat_order$,2%,2%)  = y$
          str(bat_order$,4%,1%)  = x$             /* Field (1)       */

          str(bat_order$,5%,12%) = str(file$,1%,12%)
          str(bat_order$,17%,4%) = " " & bat_no$
          str(bat_order$,21%,1%) = x$             /*                 */
          str(bat_order$,22%,2%) = y$             /* Field (2)       */
          str(bat_order$,24%,1%) = x$
          str(bat_order$,25%,10%)= space_d$       /* Spacer Descript */
          str(bat_order$,35%,1%) = x$
          str(bat_order$,36%,2%) = y$             /* Field (3)       */
          str(bat_order$,38%,1%) = x$             /* Set to Blank    */
          str(bat_order$,39%,12%) = str(file$,1%,12%)
          str(bat_order$,51%,4%) = bat_no$
          str(bat_order$,55%,1%) = x$             /*                 */
          str(bat_order$,56%,2%) = y$             /* Field (4)       */
                                                    /* MMDD - 1ST      */
          str(bat_order$,58%,4%) = str(temp_date2$,5%,4%)/* Prod Date */
          str(bat_order$,62%,2%) = str(temp_date2$,3%,2%)/* YEAR      */
          str(bat_order$,64%,2%) = y$             /* Field (5)       */
          str(bat_order$,66%,2%) = x$ & x$        /* Set to Blank    */
          str(bat_order$,68%,2%) = y$             /* Field (6)       */
          str(bat_order$,70%,2%) = x$ & x$        /* Set to Blank    */
          str(bat_order$,72%,149%) = " "

          write #ff%, str(bat_order$,1%,220%) & str(bat_dtl1$(),1%,164%), ~
                  eod goto order_error
        return
order_error:
          errormsg$ = errmsg$(err%)
          gosub error_prompt
        return

        readRecord
          rec% = 0%
          read #27, key > sh_key$, using SH_FMT, sh_rec$, sh_key$,  ~
                                            bil$(), part$, eod goto recDone

SH_FMT:       FMT CH(256), CH(50), XX(03), 15*CH(20), CH(25)

            rec% = 1%
        recDone
        return

        write_detail
          err% = 3%                                   /* Set Error Code  */
          init(" ") bat_dtl$(), bat_dtl1$()
          str(bat_dtl1$(),1%,384%) = " "

REM          CONVERT KK% TO SEQ$, PIC(000)
          cnt_dtl1% = cnt_dtl1%+ 1%
          convert cnt_dtl1% to cnt_dtl$, pic(###)
          gosub convert_fields


          str(bat_dtl$(),1%,3%)    = cnt_dtl$        /* Field (1)       */
          str(bat_dtl$(),4%,2%)    = y$              /* Field (2)       */
          str(bat_dtl$(),6%,1%)    = "1"             /* Set Qty to '1'  */
          str(bat_dtl$(),7%,2%)    = y$              /* Field (3)       */
          str(bat_dtl$(),9%,1%)    = "0"             /* Set Qty to '0'  */
          str(bat_dtl$(),10%,2%)   = y$              /* Field (4)       */
          str(bat_dtl$(),12%,1%)   = x$
          str(bat_dtl$(),13%,10%)  = sandwich$       /* Glass Sandwich  */
          str(bat_dtl$(),23%,1%)   = x$
          str(bat_dtl$(),24%,2%)   = y$              /* Field (5)       */
          str(bat_dtl$(),26%,8%)   = width_d$        /* Decimal Width   */
          str(bat_dtl$(),34%,2%)   = y$              /* Field (6)       */
          str(bat_dtl$(),36%,8%)   = height_d$       /* Decimal Height  */
          str(bat_dtl$(),44%,2%)   = y$              /* Field (7)       */
          str(bat_dtl$(),46%,6%)   = t_k$            /* Decimal Thicknes*/
          str(bat_dtl$(),52%,2%)   = y$              /* Field (8)       */
          str(bat_dtl$(),54%,1%)   = x$              /* Set First 4 Char*/
          str(bat_dtl$(),55%,4%)   = str(space_d$,1%,4%)
          str(bat_dtl$(),59%,1%)   = x$
          str(bat_dtl$(),60%,2%)   = y$              /* Field (9)       */
          str(bat_dtl$(),62%,1%)   = x$
          str(bat_dtl$(),63%,2%)   = gs$             /* Gas Yes or No   */
          str(bat_dtl$(),65%,1%)   = x$
          str(bat_dtl$(),66%,2%)   = y$              /* Field (10)      */
          str(bat_dtl$(),68%,1%)   = x$
          str(bat_dtl$(),69%,10%)  = color$         /* Blank No Grid or*/
          str(bat_dtl$(),79%,1%)   = x$              /* Color of Grid   */
          str(bat_dtl$(),80%,2%)   = y$              /* Field (11)      */
          str(bat_dtl$(),82%,1%)   = x$
          str(bat_dtl$(),83%,8%)   = muttin$         /* New Mutin Code  */
          str(bat_dtl$(),91%,1%)   = x$              /* for Grid Config */
          str(bat_dtl$(),92%,2%)   = y$              /* Field (12)      */
          if str(muttin$,1%,8%) = "CUSTOM" then w_adj$ = "   0.000"
          str(bat_dtl$(),94%,8%)   = w_adj$          /* Model Width Adj */
          str(bat_dtl$(),102%,2%)  = y$              /* Field (13)      */
          if str(muttin$,1%,6%) = "CUSTOM" then h_adj$ = "   0.000"
          str(bat_dtl$(),104%,8%)  = h_adj$          /* Model Height Adj*/
          str(bat_dtl$(),112%,4%)  = y$ & x$ & x$    /* Field (14)      */
          str(bat_dtl$(),116%,4%)  = y$ & x$ & x$    /* Field (15)      */
          str(bat_dtl$(),120%,4%)  = y$ & x$ & x$    /* Field (16)      */
          str(bat_dtl$(),124%,2%)  = y$              /* Field (17)      */
          str(bat_dtl$(),126%,1%)  = x$
          str(bat_dtl$(),127%,40%) = ged_txt$       /* Lable Text Area */
          str(bat_dtl$(),167%,1%)  = x$              /* 1st (21),2nd(21)*/
          str(bat_dtl$(),168%,2%)  = y$              /* Field (18)      */
          str(bat_dtl$(),170%,1%)  = x$
          str(bat_dtl$(),171%,6%)  = ged_shape$       /* Shape Code      */
          str(bat_dtl$(),177%,1%)  = x$
          str(bat_dtl$(),178%,3%)  = y$ & mirror$        /* Mirror(19)  */
          str(bat_dtl$(),181%,3%)  = y$ & "F"            /* Field (20)  */
          str(bat_dtl$(),184%,10%) = y$ & conrnerRadius$ /* Field (21)  */
          str(bat_dtl$(),194%,10%) = y$ & dimA_d$        /* Shape DimA  */
          str(bat_dtl$(),204%,10%) = y$ & dimB_d$        /* Shape DimB  */
          str(bat_dtl$(),214%,10%) = y$ & dimC_d$        /* Shape DimC  */
          str(bat_dtl$(),224%,10%) = y$ & dimD_d$        /* Shape DimD  */
          str(bat_dtl$(),234%,10%) = y$ & dimE_d$        /* Shape DimE  */
          str(bat_dtl$(),244%,10%) = y$ & dimF_d$        /* Shape DimF  */
          str(bat_dtl$(),254%,10%) = y$ & "   0.000"     /* Field (28)  */
          str(bat_dtl$(),264%,10%) = y$ & "   0.000"     /* Field (29)  */
          str(bat_dtl$(),274%,10%) = y$ & "   0.000"     /* Field (30)  */
          str(bat_dtl$(),284%,10%) = y$ & "   0.000"     /* Field (31)  */
          str(bat_dtl$(),294%,10%) = y$ & "   0.000"     /* Field (32)  */
          str(bat_dtl$(),304%,10%) = y$ & "   0.000"     /* Field (33)  */
          str(bat_dtl$(),314%,10%) = y$ & "   0.000"     /* Field (34)  */
          str(bat_dtl$(),324%,10%) = y$ & "   0.000"     /* Field (35)  */
          str(bat_dtl$(),334%,7%)  = y$ & str(interoffset$,1%,5%)
          str(bat_dtl$(),341%,3%)  = y$ & "4"            /* Field (37)  */
          str(bat_dtl$(),344%,40%) = "    "             /* Fill Area   */
                                             /* WRITE DETAIL RECORD*/
           write #ff%, str(bat_dtl$(),1,384), eod goto detail_error

        return
detail_error:
          errormsg$ = errmsg$(err%)
          gosub error_prompt
        return


        convert_fields
          init(" ") model$, sandwich$, width_d$, height_d$, t_k$, space_d$,~
                    gs$, color$, muttin$, w_adj$, h_adj$, ged_txt$,       ~
                    interoffset$, conrnerRadius$

          model$ = str(sh_rec$,72%,3%)                    /* Model Code  */
          ged_shape$ = bil$(1%)                           /* GED Shp Cde */
          ged_fields$ = bil$(2%)                          /* GED Fields  */
          mirror$ = "F"
          if str(ged_fields$,10%,1%) <> " " then ~
                                 mirror$ = str(ged_fields$,10%,1%)

          number$ = bil$(3%)
          gosub formatNumber
          width_d$ = number$

          number$ = bil$(4%)
          gosub formatNumber
          height_d$ = number$

          number$ = bil$(5%)
          gosub formatNumber
          dimA_d$ = number$

          number$ = bil$(6%)
          gosub formatNumber
          dimB_d$ = number$

          number$ = bil$(7%)
          gosub formatNumber
          dimC_d$ = number$

          number$ = bil$(8%)
          gosub formatNumber
          dimD_d$ = number$

          number$ = bil$(9%)
          gosub formatNumber
          dimE_d$ = number$

          number$ = bil$(10%)
          gosub formatNumber
          dimF_d$ = number$

          conrnerRadius$ = "  0.0000"
          w_adj$ = "  0.0000"
          h_adj$ = "  0.0000"
          t_k$ = bil$(11%)                                /* Thickness   */
          sandwich$ = bil$(12%)                           /* IG Sandwich */
          space_d$ = bil$(13%)                            /* Spacer Desc */
          gs$ = bil$(14%)                                 /* Gas         */
          interoffset$ = bil$(15%)                        /* Interoffset */

          str(ged_txt$,1%,4%) = "S" & model$              /* Model */
          str(ged_txt$,5%,6%) = "-" & str(sh_rec$,87%,5%) /* Seq   */
          str(ged_txt$,11%,26%) = "-" & part$             /* Part  */

        return

        formatNumber
          number = 0.00
          convert number$ to number, data goto badNumber

badNumber:

          convert number to number$, pic(##0.0000)

        return

        write_batch_trailer
            init(" ") bat_hdr$, bat_dtl1$()      /* End of File Record */
            str(bat_dtl1$(),1%,384%) = " "
            err% = 4%
            str(bat_hdr$,1%,1%)   = "#"
            str(bat_hdr$,2%,219%) = " "          /* ff% = primary File */
                                                 /* (AWD014)           */



            write #ff%, str(bat_hdr$,1%,220%) & str(bat_dtl1$(),1%,164%), ~
                  eod goto trailer_error


        return
trailer_error:
          errormsg$ = errmsg$(err%)
          gosub error_prompt
        return

        file_exists
            comp% = 2%
            hdr$ = "** Optimization File Exists **"
            msg$(1%) = "The File (XXXXXXXX) Already Exists. "
            str(msg$(1%),11%,8%) = ff$
            msg$(2%) = "             O P T I M I Z A T I O N             "
            msg$(3%) = "Press <RETURN> To Continue, or PF(16) to Delete. "
            call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        exit_program

        end


