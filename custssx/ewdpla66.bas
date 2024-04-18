        REM *************************************************************~
            *                                                           *~
            *                                                           *~
            *  Program Name      - EWDPLA66 - Program (EWDPLN66)        *~
            *  Creation Date     - 02/15/99 - File - (@GEDSPC@)         *~
            *  Last Modified Date- 03/19/06                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Create GED Glass Bridge File for     *~
            *                      Backorders, Special Shapes, and      *~
            *                      Stock MFG                            *~
            *                                                           *~
            *Note  - Break on all Spacer Changes and Start a New Batch. *~
            *                                                           *~
            *      - Called from (EWDPLN66)                             *~
            *      - Bridge File Names                                  *~
            *        (@GEDSPC@) - Special Glass Created by Planning     *~
            *        (@GEDSP1@) - Special Glass Created by Other        *~
            *                                                           *~  
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/25/99 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 08/09/99 ! (EWD005) Mod to Glass for Grid Bumpers   ! RHH *~
            * 09/07/99 ! (EWD006) Mod for BW Grid for '124' and   ! RHH *~
            *          !   '134'                                  !     *~
            * 09/15/99 ! (EWD001) Mods for switch to Zebra Prt    !     *~       
            * 05/23/05 ! (AWD007) Mods for 3/4" grid              ! CMG *~
            * 01/01/06 ! (PAR000) CR347 Mod for new Sub Part No.  ! RHH *~ 
            * 03/19/06 ! (AWD008) mods for krypton                ! CMG *~
            *************************************************************

        sub "EWDPLA66" (size%,           /* Specified Batch Size      */ ~
                        glass_dte$,      /* Glass Production Date     */ ~
                        file$,           /* Name of Optimized File 0  */ ~
                        bat$,            /* Number of Batches Created */ ~
                        sp_created$,     /* Created By 0=Plan,1=Other */ ~
                        #1,              /* (GENCODES) Master Tables  */ ~
                        #2,              /* (EWDPLNGR) Special Glass  */ ~
                        #3,              /* (@GEDSPC@) Optimizer Plann*/ ~
                        #4,              /* (@GEDSP1@) Optimizer Other*/ ~
                        #5 )             /* (APCPLNGR) Master Glass   */

        dim glass_dte$8, glass_dte_fm$10,/* Production Date           */ ~
            temp_dte$6,                  /* MMDDYY                    */ ~
            file$20,                     /* Name of Optimized File    */ ~
            bat$3,                       /* Number of Batches         */ ~
            ged_rec$250, ged_key$31,     /* GED Glass Data            */ ~
            x$1, y$2,                    /* X = Quote, Y = Comma Space*/ ~
            bat_no$3, sav_bat$3,         /* Batch Number              */ ~
            cnt_ord$3, sort$2,           /* Count Order Headers       */ ~
            cnt_dtl$3, sav_sort$2,       /* Count Order Details       */ ~
            space_d$10,                  /* Spacer Description        */ ~
            spacer$6, sav_spacer$6,      /* Spacer with copy          */ ~
            sandwich$10,                 /* Glass Sandwich            */ ~
            width_d$8, wd1$9, ht1$9,     /* Width Size Deciamal       */ ~
            height_d$8,wd2$9,            /* Height Size Decimal       */ ~
            t_k$6,                       /* Overall Thickness         */ ~
            gs$2,                        /* Blank or "G1"             */ ~
            lk$1,                        /* Lock Codes                */ ~
            color$6, c$(15%)2, cl$1,     /* Color Code / Table        */ ~
            muttin$8, gd$2,              /* Grid Code                 */ ~
            sp_created$1,                /* Created By 0=Plan,1=Other */ ~
            sp_vert$(10%)7,              /* Custom Vertical           */ ~
            sp_horz$(10%)7,              /* Custom Horizontal         */ ~
            sp_stock$1,                  /* Stock File                */ ~
            sp_sub_part$20,              /* New Sub Part No.  (PAR000)*/ ~  
            sp_filler$38,                /* Filler Area       (PAR000)*/ ~  
            w_adj$6,                     /* Width Adjustment Code     */ ~
            h_adj$6,                     /* Height Adjustment Code    */ ~
            ged_txt$40,                  /* Field 16 - Text           */ ~
            bat_hdr$220,                 /* Batch Header Record       */ ~
            bat_order$220,               /* Batch Order Record        */ ~
            bat_dtl$220,                 /* Batch Detail Record       */ ~
            model$3,                     /* Model Code                */ ~
            ssq$5,                       /* Product Seq No.           */ ~
            v$1,                         /* T or B  VIEW              */ ~
            part$25, so$8,               /* Part No, S.O.             */ ~
            rm_key$12,                   /* Primary Key (APCPLNGR)    */ ~
            rm_rec$(2%)192,              /* (APCPLNGR) Rec    (PAR000)*/ ~
            rm_st_time$8,                /* Time of Last Change       */ ~ 
            ff$8, errormsg$79,           /* Batch File Name           */ ~
            readkey$24,                  /* GENCODES                  */ ~
            userid$3,                    /* User Id       (EWD002)    */ ~
            hdr$40, error$(5%)50,        /* ASKUSER Header Text       */ ~
            msg$(3%)79                   /* ASKUSER Info Text         */

/* (AWD009) */
        dim                              /*                            */~
            field1$1,                    /* New Part Field 1 GRDTYPE   */~
            field2$1,                    /* New Part Field 2 GRDSIZE   */~
            field3$1,                    /* New Part Field 3 GRDCOLOR  */~
            field4$1,                    /* New Part Field 4 HARDWARE  */~
            field5$1                     /* New Part Field 5 FOAM      */


        dim f2%(10%),                    /* = 0 if the file is open    */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */

            call "EXTRACT" addr("ID", userid$)
            glass_dte_fm$ = glass_dte$
            call "DATFMTC" (glass_dte_fm$)
            init(" ") glass_dte$
            str(glass_dte$,1%,2%) = str(glass_dte_fm$,1%,2%)
            str(glass_dte$,3%,2%) = str(glass_dte_fm$,4%,2%)
            str(glass_dte$,5%,4%) = str(glass_dte_fm$,7%,4%)
                                          /* Note - ff% = Primary File */  
            ff$ = "@GEDSPC@" : ff% = 3%   /* Created By Planning       */
            if sp_created$ = "1" then ff$ = "@GEDSP1@" /* By Other     */
            if sp_created$ = "1" then ff% = 4%         /* By Other     */
  
                                          /*Create GED Glass Bridge Fil*/
            call "OPENFILE" (#ff%, "IO   ", f2%(ff%), rslt$(ff%), axd$ )
            if f2%(ff%) <> 0% then goto L00120
               gosub file_exists          /* Uses ff$ for Error Display*/
               if comp% <> 16% then goto L00100
                  call "FILEBGON" (#ff%)
                  goto L00120
L00100:        close #ff%                 /* Re-Open in Append Mode    */
               call "OPENFILE" (#ff%,"EXTND",f2%(ff%),rslt$(ff%),axd$)
               goto L00130

L00120:     str(rslt$(ff%),1%,6%)  = "OUTPTP"
            str(rslt$(ff%),7%,8%)  = "00001000"
            str(rslt$(ff%),15%,3%) = "100"
            str(rslt$(ff%),18%,3%) = "100"
            call "OPENFILE" (#ff%,"OUTPT",f2%(ff%),rslt$(ff%),axd$ )
                                          /* ff% = 3% Special Glass    */
L00130:     call "SHOSTAT" ("GED Glass Bridge File for "&glass_dte_fm$)
            init(" ") sort$, sav_spacer$

            x$ = bin(34%,1)               /* Suff a '"' Quote into X$  */
            y$ = ", "                     /* Set to ',' = Comma+Space  */
            error$(1%) = "(Error)-Writing Batch Header Record? (1)"
            error$(2%) = "(Error)-Writing Batch Order Record?  (2)"
            error$(3%) = "(Error)-Writing Order Detail Record? (3)"
            error$(4%) = "(Error)-Writing End of File Record?  (4)"
            error$(5%) = "(Error)-Writing Glass Record (APCPLNGR) (5)"
                                                  /* (PAR000)          */ 
            c$(1%) =  "XX" : c$(2%) = "WH" : c$(7%) = "BW"
            c$(3%) =  "BZ" : c$(4%) = "XX"
            c$(5%) =  "CO" : c$(6%) = "AL"
            c$(8%) =  "NO"                        /* Color Code 'A'    */
            c$(9%) =  "HO"                        /* Color Code 'B'    */
                                                  /* (PAR000)          */
            sav_bat$ = "XXX"
*       RHH
            size% = 9999%                 /* Set Size to Max           */
            error% = 0%
            cnt_ord%,  cnt_dtl% = 0%      /* Set Channel for @GLSGED@  */
                                          /* (PAR000)                  */
            ged_key$ = all(hex(00))       /* GED Glass Primary Key     */
            str(ged_key$,1%,1%) = "N"     /* Only Not Processed        */
            str(ged_key$,2%,1%) = sp_created$  /* Created By           */ 
            read #2,hold,key > ged_key$, using L00200 , ged_rec$,        ~
                          sp_vert$(), sp_horz$(), sp_stock$, sp_sub_part$, ~
                                      sp_filler$, eod goto create_done

            ged_key$ = str(ged_rec$,1%,31%) /* Set Alt Key            */
            if str(ged_key$,1%,1%) <> "N" then goto create_done
            if str(ged_key$,2%,1%) <> sp_created$ then goto create_done  
            gosub write_batch_header
            sav_sort$   = str(ged_rec$,9%,2%)
            sav_spacer$ = str(ged_rec$,11%,6%)
            goto L00210
        create_next                                 /* (PAR000)         */
            read #2,hold,key > ged_key$, using L00200 , ged_rec$,        ~
                          sp_vert$(), sp_horz$(), sp_stock$, sp_sub_part$,~
                                      sp_filler$, eod goto create_done
L00200:          FMT CH(201), 10*CH(7), 10*CH(7), CH(1), CH(20), CH(38)

L00210:     ged_key$ = str(ged_rec$,1%,31%)
            if str(ged_key$,1%,1%) <> "N" then goto create_done
            if str(ged_key$,2%,1%) <> sp_created$ then goto create_done
            str(ged_rec$,1%,1%) = "Y"
            delete #2
            write #2, using L00200, ged_rec$, sp_vert$(), sp_horz$(),    ~
                              sp_stock$, sp_sub_part$, sp_filler$,       ~
                                                     eod goto create_done
                                                    /* (PAR000)         */
            if sp_stock$ = "S" then goto create_next
            gosub unpack_data
            gosub write_order_header
            if error% = 1% then goto create_next
            gosub write_detail
            if error% = 1% then goto create_next

            gosub update_glass                   /* Update (APCPLNGR)  */

            goto create_next
        create_done
            init(" ") bat_hdr$                   /* End of File Record */
            str(bat_hdr$,1%,1%)   = "#"
            str(bat_hdr$,2%,219%) = " "          /* ff% = primary File */
            write #ff%, bat_hdr$, eod goto L01760
L01740:     close #ff%
        goto exit_end
L01760:     err% = 4%
            errormsg$ = error$(err%)
            gosub error_prompt
            goto L01740

        write_batch_header           
            str(file$,16%,5%) = "(SPC)"
            if sp_created$ = "1" then str(file$,16%,5%) = "(SP1)"

            init(" ") bat_hdr$
            str(bat_hdr$,1%,6%)   = "<V1.0>"
            str(bat_hdr$,7%,2%)   = y$                 /* Comma Space  */
            str(bat_hdr$,9%,1%)   = x$                 /* Set Quote    */
            str(bat_hdr$,10%,8%)  = glass_dte$
            str(bat_hdr$,18%,9%)  = " - EWD - "
            str(bat_hdr$,27%,20%) = file$      /* Entered in 'EWDPLN66' */
            str(bat_hdr$,47%,1%)  = x$                 /* Set Quote    */
            str(bat_hdr$,48%,173%) = " "
            write #ff%, bat_hdr$, eod goto L01930
        return
L01930:     err% = 1%
            errormsg$ = error$(err%)
            gosub error_prompt
        return

        write_order_header
            if cnt_dtl% = 0% then goto L02010     /* Start New Oder    */
               return                             /* Header Record     */
L02010:     cnt_ord% = cnt_ord% + 1%
            convert cnt_ord% to cnt_ord$, pic(000) /* Count Order Header*/

            bat_no$ = cnt_ord$
            str(temp_dte$,1%,4%) = str(glass_dte$,1%,4%)
            str(temp_dte$,5%,2%) = str(glass_dte$,7%,2%)                                             /* (Y2K, LDJ) */        
            init(" ") bat_order$
            str(bat_order$,1%,1%)  = "*"            /* Field (0)       */
            str(bat_order$,2%,2%)  = y$
            str(bat_order$,4%,1%)  = x$             /* Field (1)       */
            str(bat_order$,5%,7%)  = temp_dte$ & " "/* mmddyy Date     */
            str(bat_order$,12%,3%) = bat_no$        /* Batch Number    */
            str(bat_order$,15%,2%) = "  "           /* Blank           */
            str(bat_order$,17%,1%) = x$             /* a Batch         */
            str(bat_order$,18%,2%) = y$             /* Field (2)       */
            str(bat_order$,20%,1%) = x$
            str(bat_order$,21%,10%)= space_d$       /* Spacer Descript */
            str(bat_order$,31%,1%) = x$
            str(bat_order$,32%,2%) = y$             /* Field (3)       */
            str(bat_order$,34%,1%) = x$             /* Set to Blank    */
            str(bat_order$,35%,7%) = temp_dte$ & " "/* mmddyy Date     */
            str(bat_order$,42%,3%) = bat_no$        /* Batch Number    */
            str(bat_order$,45%,2%) = "  "           /* Blank           */
            str(bat_order$,47%,1%) = x$             /* a Batch         */
            str(bat_order$,48%,2%) = y$             /* Field (4)       */
                                                    /* MMDD - 1ST      */
            str(bat_order$,50%,4%) = str(temp_dte$,1%,4%)/* Prod Date  */
            str(bat_order$,54%,2%) = str(temp_dte$,5%,2%)/* Year       */
            str(bat_order$,56%,2%) = y$             /* Field (5)       */
            str(bat_order$,58%,2%) = x$ & x$        /* Set to Blank    */
            str(bat_order$,60%,2%) = y$             /* Field (6)       */
            str(bat_order$,62%,2%) = x$ & x$        /* Set to Blank    */
            str(bat_order$,64%,157%) = " "
            write #ff%, bat_order$, eod goto L02510
        return
L02510:     err% = 2%
            errormsg$ = error$(err%)
            gosub error_prompt
        return

        write_detail
            init(" ") bat_dtl$
            cnt_dtl% = cnt_dtl% + 1%
            convert cnt_dtl% to cnt_dtl$, pic(###)

            str(bat_dtl$,1%,3%)   = cnt_dtl$        /* Field (0)       */
            str(bat_dtl$,4%,2%)   = y$              /* Field (1)       */
*       RHH - Possible Change
            str(bat_dtl$,6%,1%)   = "1"             /* Set Qty to '1'  */
            str(bat_dtl$,7%,2%)   = y$              /* Field (2)       */
*       RHH - Possible Change
            str(bat_dtl$,9%,1%)   = "0"             /* Set Qty to '0'  */
            str(bat_dtl$,10%,2%)  = y$              /* Field (3)       */
            str(bat_dtl$,12%,1%)  = x$
*       RHH - Possible Change
            str(bat_dtl$,13%,10%) = sandwich$       /* Glass Sandwich  */
            str(bat_dtl$,23%,1%)  = x$
            str(bat_dtl$,24%,2%)  = y$              /* Field (4)       */
            str(bat_dtl$,26%,8%)  = width_d$        /* Decimal Width   */
            str(bat_dtl$,34%,2%)  = y$              /* Field (5)       */
            str(bat_dtl$,36%,8%)  = height_d$       /* Decimal Height  */
            str(bat_dtl$,44%,2%)  = y$              /* Field (6)       */
            str(bat_dtl$,46%,6%)  = t_k$            /* Decimal Thicknes*/
            str(bat_dtl$,52%,2%)  = y$              /* Field (7)       */
            str(bat_dtl$,54%,1%)  = x$              /* Set First 4 Char*/
            str(bat_dtl$,55%,4%)  = str(space_d$,1%,4%)
            str(bat_dtl$,59%,1%)  = x$
            str(bat_dtl$,60%,2%)  = y$              /* Field (8)       */
            str(bat_dtl$,62%,1%)  = x$
            str(bat_dtl$,63%,2%)  = gs$             /* Gas Yes or No   */
            str(bat_dtl$,65%,1%)  = x$
            str(bat_dtl$,66%,2%)  = y$              /* Field (9)       */
            str(bat_dtl$,68%,1%)  = x$
*       RHH - Possible Change
            str(bat_dtl$,69%,6%)  = color$          /* Blank No Grid or*/
            str(bat_dtl$,75%,1%)  = x$              /* Color of Grid   */
            str(bat_dtl$,76%,2%)  = y$              /* Field (10)      */
            str(bat_dtl$,78%,1%)  = x$
*       RHH - Possible Change
            str(bat_dtl$,79%,8%)  = muttin$         /* New Mutin Code  */
            str(bat_dtl$,87%,1%)  = x$              /* for Grid Config */
            str(bat_dtl$,88%,2%)  = y$              /* Field (11)      */
            str(bat_dtl$,90%,6%)  = w_adj$          /* Model Width Adj */
            str(bat_dtl$,96%,2%)  = y$              /* Field (12)      */
            str(bat_dtl$,98%,6%)  = h_adj$          /* Model Height Adj*/
            str(bat_dtl$,104%,4%) = y$ & x$ & x$    /* Field (13)      */
            str(bat_dtl$,108%,4%) = y$ & x$ & x$    /* Field (14)      */
            str(bat_dtl$,112%,4%) = y$ & x$ & x$    /* Field (15)      */
            str(bat_dtl$,116%,2%) = y$              /* Field (16)      */
            str(bat_dtl$,118%,1%) = x$
*       RHH - Possible Change
            str(bat_dtl$,119%,40%) = ged_txt$       /* Lable Text Area */
            str(bat_dtl$,159%,1%) = x$              /* 1st (20),2nd(20)*/
            str(bat_dtl$,160%,2%) = y$              /* Field (17)      */
            str(bat_dtl$,162%,1%) = x$
*       RHH - Possible Change
            str(bat_dtl$,163%,5%) = "1-STD"         /* Default Standard*/
            str(bat_dtl$,168%,1%) = x$
            str(bat_dtl$,169%,3%) = y$ & "F"            /* Field (18)  */
            str(bat_dtl$,172%,3%) = y$ & "F"            /* Field (19)  */
            str(bat_dtl$,175%,6%) = y$ & "0.00"         /* Field (20)  */
            str(bat_dtl$,181%,6%) = y$ & "0.00"         /* Field (21)  */
            str(bat_dtl$,187%,6%) = y$ & "0.00"         /* Field (22)  */
            str(bat_dtl$,193%,6%) = y$ & "0.00"         /* Field (23)  */
            str(bat_dtl$,199%,6%) = y$ & "0.00"         /* Field (24)  */
            str(bat_dtl$,205%,6%) = y$ & "0.00"         /* Field (25)  */
            str(bat_dtl$,211%,6%) = y$ & "0.00"         /* Field (26)  */
            str(bat_dtl$,217%,4%) = "    "              /* Fill Area   */
            if mod(cnt_dtl%,size%) = 0% then cnt_dtl% = 0%
            write #ff%, bat_dtl$, eod goto L02520    /* Write Detail Rec*/
     
            gosub write_detail_vert                  /* Custom Grid    */
            gosub write_detail_horz                  /* Custom Grid    */
        return
L02520:     err% = 3%
            errormsg$ = error$(err%)
            gosub error_prompt
        return

        write_detail_vert
            if str(muttin$,1%,6%) <> "CUSTOM" then return
               init(" ") bat_dtl$
               str(bat_dtl$,1%,1%)   = x$
               str(bat_dtl$,2%,1%)   = "V"
               str(bat_dtl$,3%,1%)   = x$

               fld% = 1%                                 /* Field No.  */    
               pp%  = 4%                                 /* Pos Field 1*/ 
L02600:        if len(sp_vert$(fld%)) < 5 then goto L02610
                  str(bat_dtl$,pp%,9%)  = y$ & sp_vert$(fld%) /*1 - 10*/
                  fld% = fld% + 1%
                  pp%  = pp% + 9%
                  if fld% <= 10% then goto L02600 else goto L02610

L02610:     bat_dtl$ = bat_dtl$ & y$ & "0"
            call "SPCESMSH" (bat_dtl$,0%)
            yy% = len(bat_dtl$)
            p1% = (220% - yy%) 
            str(bat_dtl$,yy% + 1%,p1%) = " "            /* Fill Area   */
            write #ff%, bat_dtl$, eod goto L02520  /*Vertical Notch Rec*/
        return

        write_detail_horz
            if str(muttin$,1%,6%) <> "CUSTOM" then return
               init(" ") bat_dtl$
               str(bat_dtl$,1%,1%)   = x$
               str(bat_dtl$,2%,1%)   = "H"
               str(bat_dtl$,3%,1%)   = x$

               fld% = 1%                                 /* Field No.  */    
               pp%  = 4%                                 /* Pos Field 1*/ 
L02700:        if len(sp_horz$(fld%)) < 5 then goto L02710
                  str(bat_dtl$,pp%,9%)  = y$ & sp_horz$(fld%) /*1 - 10*/
                  fld% = fld% + 1%
                  pp%  = pp% + 9%
                  if fld% <= 10% then goto L02700 else goto L02710

L02710:     bat_dtl$ = bat_dtl$ & y$ & "0"
            call "SPCESMSH" (bat_dtl$,0%)
            yy% = len(bat_dtl$)
            p1% = (220% - yy%)
            str(bat_dtl$,yy% + 1%,p1%) = " "             /* Fill Area   */
            write #ff%, bat_dtl$, eod goto L02520 /*Horizontal Notch Rec*/
        return

        file_exists
          comp% = 2%
          hdr$ = "**** GED Glass File Exists ***"
          msg$(1%) = "       The File (XXXXXXXX) Already Exists.      "
          str(msg$(1%),18%,8%) = ff$
          msg$(2%) = "          B u i l d   G E D   G l a s s          "
          msg$(3%) = "Press <RETURN> To Continue, or PF(16) to Delete. "
          call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        check_argon                      /* Check Table for Glass Code */
           init(" ") readkey$ : argon% = 0%   /* and Argon Gas         */
           str(readkey$,1%,9%)   = "PLANARGON"
           str(readkey$,10%,15%) = str(part$,5%,2%)
           read #1,key = readkey$, eod goto L03330
           argon% = 1%
L03330: return

                                         /* (AWD008)        */
        check_krypton                    /* Check Table for Glass Code */
           init(" ") readkey$ : krypton% = 0%  /* and Krypton Gas      */
           str(readkey$,1%,9%)   = "PLANKRYPT"
           str(readkey$,10%,15%) = str(part$,5%,2%)
           read #2,key = readkey$, eod goto not_krypton
           krypton% = 1%
not_krypton: 
        return
                                         /* (AWD008)        */

        unpack_data
           c% = 0%
           sort$   = str(ged_rec$,9%,2%)          /* Special Sort Code */
           spacer$ = str(ged_rec$,11%,6%)         /* Spacer Size       */
           model$  = str(ged_rec$,20%,3%)         /* Mode/Product Code */
           cl$     = str(ged_rec$,46%,1%)         /* Color Code        */
           ssq$    = str(ged_rec$,47%,5%)         /* Planning Seq. No. */
           v$      = str(ged_rec$,52%,1%)         /* View T or B       */
           t_k$    = str(ged_rec$,53%,6%)         /* Gls Overall Thickn*/
           muttin$ = str(ged_rec$,59%,8%)         /* Glass Muttin Code */
           space_d$= str(ged_rec$,67%,10%)        /* Spacer Description*/
           sandwich$=str(ged_rec$,77%,10%)        /* Sandwich          */
           width_d$ =str(ged_rec$,87%,8%)         /* Width Decimal     */
           height_d$=str(ged_rec$,95%,8%)         /* height Decimal    */
           w_adj$   =str(ged_rec$,103%,6%)        /* Width Adjustment  */
           h_adj$   =str(ged_rec$,109%,6%)        /* Height Adjustment */
           so$      =str(ged_rec$,115%,8%)        /* Special S.O.      */
           part$    =str(ged_rec$,123%,25%)       /* MFG Part Number   */
           wd1$     =str(ged_rec$,148%,9%)        /* Window Width Eight*/
           ht1$     =str(ged_rec$,157%,9%)        /* Window Height Eigh*/
           wd2$     =str(ged_rec$,166%,9%)        /* CLMR in Eights    */
/*(AWD009) */
           field1$ = str(sp_sub_part$,1%,1%)      /* (AWD009) grdtype  */
           field2$ = str(sp_sub_part$,2%,1%)      /* (AWD009) grdsize  */
           field3$ = str(sp_sub_part$,3%,1%)      /* (AWD009) grdcolor */
           field4$ = str(sp_sub_part$,4%,1%)      /* (AWD009) hardware */
           field5$ = str(sp_sub_part$,5%,1%)      /* (AWD009) foam     */
                                                  /* (PAR000)          */
           color$  = "      "
           convert cl$ to c%, data goto L03510    /* Convert Gls Color */

L03510:    if cl$ = "A" then c% = 8%              /* Code 'A' = 'NO'   */
           if cl$ = "B" then c% = 9%              /* Code 'B' = 'HO'   */
/* (AWD009) */
           if cl$ = "G" then c% =  2%             /* Code G = 'WH'     */ 
           if cl$ = "H" then c% =  6%             /* Code H = 'AL'     */
           if cl$ = "I" then c% =  8%             /* Code I = '3/4 NO' */
           if cl$ = "J" then c% =  9%             /* Code J = '3/4 HO' */ 

           if field1$ = "3" then c% = 10%         /* (PAR000) Brass    */
/* (/AWD009) */

           gd$       = str(part$,7%,2%)           /* Grid/Liting Code  */
           lk$       = str(part$,12%,1%)          /* Lock Codes with   */
                                                  /* (PAR000)          */
                                                  /* 1 inch Grid Contour*/
/*(AWD009)*/
REM        if str(sp_sub_part$,1%,1%) = "2" then lk% = 1%


           if field1$ = "2" and field2$ = "3" then lk% = 1% 

           if gd$ = "00" then goto L03520
              if c% = 0% then goto L03520           /* Mod for New Grid  */
                 if c% = 2% and str(model$,1%,1%) = "6" then c% = 7%
                                                  /* (EWD006)          */
                 if c% = 2% and str(model$,1%,3%) = "124" then c% = 7%
                 if c% = 2% and str(model$,1%,3%) = "134" then c% = 7%
/*(AWD009)*/
                 if c% = 2% and str(model$,1%,3%) = "870" then c% = 7%
                                                  /* (EWD006)          */
                 color$   = "GRID" & c$(c%)
                                                  /* (PAR000) Contour  */
                 if lk% <> 0% then color$ = "CONT" & c$(c%)
                                                 
                                                  /* (PAR000) 3/4"     */
REM              if str(sp_sub_part$,2%,1%) = "2" then                  ~
                                                 color$ = "WIDE" & c$(c%)

/* (AWD009) */
                 if field2$ = "2" then color$ = "WIDE" & c$(c%)
                 if field1$ = "2" and field2$ = "1"              ~
                                then color$ = "CO58" & c$(c%)


                                                  /* (PAR000)          */
L03520:  
                                                  /* (EWD005)          */
           gosub check_grid_bumpers               /* Glass Grid Bumpers*/
                                                  /* (EWD005)          */      


           gs$ = "  "                             /* Argon Gas Flag    */
           gosub check_argon                    /* Check for Argon Gas */
           if argon% = 1% then gs$ = "G1"


           gosub check_krypton                   /* (AWD008)   */
           if krypton% = 1% then gs$ = "G2"     /* Check for Krypton Gas*/

           init(" ") ged_txt$
           str(ged_txt$,1%,1%) = v$             /* Ged Text (40)=Char  */
           str(ged_txt$,2%,3%) = model$
           str(ged_txt$,5%,3%) = str(c$(c%),1%,2%) & " "
           str(ged_txt$,8%,9%) = so$ & " "
           str(ged_txt$,17%,5%)= ssq$
           str(ged_txt$,23%,18%) = str(part$,1%,18%)
           if sav_bat$ = "XXX" then sav_bat$ = bat_no$
           if sav_spacer$ = spacer$ then goto L03900
L03860:       cnt_dtl% = 0% 
              sav_spacer$ = spacer$
              sav_sort$   = sort$
              return
L03900:    if sav_sort$ <> sort$ then goto L03860
        return

        update_glass               /* (APCPLNGR) all Glass Has Entry     */
                                   /* Status 0=Remake,1=Sched,2=Complete */
                                   /* (PAR000)                           */
           init(" ") rm_key$,rm_st_time$, rm_rec$(), errormsg$

                                                 /* Formated Time AM,PM  */
           call "TIME" (rm_st_time$)             /* Time Glass Scheduled */
           rm_key$ = str(ged_rec$,23%,12%)       /* Primary Key          */
           read #5,key = rm_key$, using L04000 , rm_rec$(),               ~
                                                  eod goto schedule_glass
L04000:       FMT 2*CH(192)
        return
                                                 /* (PAR000)             */

        schedule_glass                           /* First Time Need to */
                                                 /* Create Glass Data  */
           str(rm_rec$(),1%,6%) = str(ged_rec$,3%,6%)    /* Glass Prod Dt*/
           str(rm_rec$(),7%,6%) = str(ged_rec$,3%,6%)    /* Glass Prod Dt*/
           str(rm_rec$(),13%,1%) = "1"             /* Scheduled Glass    */
           str(rm_rec$(),14%,8%) = rm_st_time$     /* Time of Stat Change*/
           str(rm_rec$(),22%,9%) = str(ged_rec$,23%,9%)  /* Glass Barcode */
           str(rm_rec$(),31%,3%) = str(ged_rec$,32%,3%) /* Remake No.    */
           str(rm_rec$(),34%,2%) = "24"            /* Special Glass      */
           str(rm_rec$(),36%,6%) = date            /* Date of Stat Change*/
           str(rm_rec$(),42%,2%) = "01"            /* Scanned Shift*/
                                                 /* Time 24 hour clock */
           str(rm_rec$(),44%,8%) = time            /* Scheduled Time     */
           str(rm_rec$(),52%,6%) = date            /* Scheduled Date Today*/
           str(rm_rec$(),58%,3%) = str(ged_rec$,43%,3%) /* Who Scheduled */
           str(rm_rec$(),61%,4%) = "    "          /* Completion Calc    */
           str(rm_rec$(),65%,2%) = "  "            /* Growth Area        */
           str(rm_rec$(),67%,190%) = "            "/* Not Applic         */
                                                   /* (PAR000)           */
           str(rm_rec$(),255%,20%) = sp_sub_part$

           write #5, using L04000, rm_rec$(), eod goto L04010
        return
L04010:    err% = 5%
           errormsg$= error$(err%)
           gosub error_prompt
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
           if (width_d + height_d) > 119.0 then goto L04450
/* (AWD009) */
           if model$ = "311" or model$ = "312" then goto L04450
           if model$ = "313" or model$ = "314" then goto L04450
           if model$ = "332" or model$ = "333" then goto L04450
           if model$ = "334" then goto L04450

        return
L04450:    if str(color$,1%,4%) = "CONT" then str(color$,1%,4%) = "BCNT"
           if str(color$,1%,4%) = "GRID" then str(color$,1%,4%) = "BGRD"
                                                     /* (AWD007)  */
           if str(color$,1%,4%) = "WIDE" then str(color$,1%,4%) = "BWDE"
/*(AWD009)*/
           if str(color$,1%,4%) = "CO58" then str(color$,1%,4%) = "BC58"
        return
                                                 /* (EWD005)           */ 
 
        error_prompt
           error% = 1%
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return


        exit_end
           bat$ = cnt_ord$

        end

