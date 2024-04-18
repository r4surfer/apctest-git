        REM *************************************************************~
            *                                                           *~
            *  Note 'create_glass' is Enabled                (RHHTEST)  *~
            *                                                           *~
            *  Program Name      - APCPLG45 - Copy of(APCPLF45)         *~
            *  Creation Date     - 02/02/04 -                           *~
            *  Last Modified Date- 06/30/2019                           *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Create BILCO Glass Bridge File with  *~
            *                      Batches for processing Special       *~
            *                      glass processed for the day.         *~
            *                                                           *~
            *                    - Also Create Flat file for Custom     *~
            *                      Glass @SHAPE3@. Ordered Glass.       *~
            *                                                           *~
            *                    - Also create a Flat file for Custom   *~
            *                      Glass @SHAPE4@. Odered Remake Glass  *~
            *                                                           *~
            *     (AWD007)       - (AWDGLSCT) Ne Custom Database File   *~
            *                      for Special Shape Glass pricing      *~
            *                      information and transmitted data.    *~
            *                                                           *~
            * x$ = bin(35%,1)      STUFF Pound symbol into X$           *~
            *                                                           *~
            *  MOD - One (1) Title and (1) Header Record per batch      *~
            *        Identification Field. Use 'MODEL' as the           *~
            *        Identifier instead of 'LOAD'.                      *~
            *                                                           *~
            *        Key subroutines     build_title                    *~
            *                            build_header                   *~
            *                            build_detail                   *~
            *                            build_end                      *~
            *                            check_custom_glass             *~
            *                            update_custom                  *~
            *                            print_header                   *~
            *                            create_glass (Special Shapes)  *~
            *                            update_glass (Special Shapes)  *~
            *                            format_custom_record (AWD007)  *~
            *                                                           *~
            *       Calc Routines        find_base                      *~
            *                            find_height                    *~
            *                            find_radius                    *~
            *                            find_leg           (leg height)*~
            *                            find_leg_1      (left side leg)*~
            *                            find_leg_2     (right side leg)*~
            *                            find_leg_3            (top leg)*~
            *                            find_leg_4           (side leg)*~
            *                            find_leg_5           ( N/A    )*~
            *                                                           *~
            *       Data Routines        lookup_sandwich                *~
            *                            check_for_temp                 *~
            *                            lookup_thickness               *~
            *                            check_grid_size                *~
            *                            get_facing                     *~
            *                                                           *~
            *        Note If any of the dimension positions change in   *~
            *             (ewdglsxx)then the position array must be     *~
            *             changed. sh_pos%(?).                          *~
            *                                                           *~
            *                                                           *~
            *      - Bridge File Names                                  *~
            *        (@SHAPE2@) - Batch Created Automatically           *~
            *        (@SHAPE3@) - Batch File for Custom Glass Ordered   *~
            *        (@SHAPE4@) - Batch File for Custom Glass Remakes   *~
            *                                                           *~
            *      - Order of Cut dimentions for Custom                 *~
            *        Base (Width)                                       *~
            *        Height                                             *~
            *        Radius                                             *~
            *        leg                                                *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 03/10/04 ! New Program for (AWD)                    ! RHH *~
            * 06/02/04 ! (AWD001) Put Barcode in Custom file      !     *~
            * 06/11/04 ! (AWD002) Do not put Tempered in PMC      ! RHH *~
            *          !    Bridge File. Can Send to Custom.      !     *~
            * 07/13/04 ! (AWD003) New Production date format      ! RHH *~
            *          !    Requested by Custom. YYYMMDD          !     *~
            * 07/23/04 ! (AWD004) For the new Triple Eyebrow codes! RHH *~
            *          !    Mod to insure that the glass is not   !     *~
            *          !    put in the Custom Glass file.         !     *~
            *          !    Grid codes beginning with 'T,U'       !     *~
            * 08/10/04 ! (AWD005) Mod to Add Three new Fields and ! RHH *~
            *          !    change the record length              !     *~
            * 09/14/04 ! (AWD006) Mod to Track Custom Glass Sent  ! RHH *~
            *          !    'PLANGLASS' - Code = '7' for Custom   !     *~
            *          !    only update Glass sent to Custom.     !     *~
            *          !    Code = '8' update Remake Glass sent to!     *~
            *          !    Custom.                               !     *~
            * 10/27/04 ! (AWD007) Mods for Custom Pricing.        ! RHH *~
            * 11/12/04 ! (AWD008) Update to Data Passed on to     ! RHH *~
            *          !    Custom Glass.                         !     *~
            * 12/01/04 ! (AWD009) Mod to turn back on the grid    ! RHH *~
            *          !    codes "T" and "U" for Custom          !     *~
            * 12/15/04 ! (AWD010) Mot to By pass for Custom Glass ! RHH *~
            *          !    new Grid Code '59'                    !     *~
            * 01/01/06 ! (PAR000) CR347 New Sub Part No.          ! RHH *~
            * 01/31/06 ! (PAR001) Add File 'BCKSUBPT' for Grid    !     *~
            *          !    also add SO Line Item to the Special  ! RHH *~
            *          !    Shapes label File EWDGLSXX has no     !     *~
            *          !    free space.                           !     *~
            * 03/06/06 ! (PAR002) Add Error trap for Calculations ! RHH *~
            * 05/04/07 ! (AWD011) mod for 18 mm grid              ! CMG *~
            * 03/19/08 ! (AWD012) mod for sdl                     ! CMG *~
            * 05/27/08 ! (AWD013) mod for glass of 25 sqft to be  ! CMG *~
            *          !        grid only like tempered           ! CMG *~
            * 11/09/06 ! (PAR003) Add HUB to file (see apcpla45)  ! DES *~
            * 07/02/08 ! (AWD014) mod for infini-lite             ! CMG *~
            * 07/30/08 ! (AWD015) mods for Contour                ! CMG *~
            *04/01/2011! (AWD016) mods for new lab key            ! CMG *~
            *04/29/2013! (AWD017) mods for NC and TX Schema       ! CMG *~
            *03/31/2014! (AWD018) mod for PMC date location change! MES *~
            *06/30/2019! (CR2109) mod for operable shapes         ! CMN *~
            *************************************************************

                                          /* ( Bilco Glass Cutter )    */
        sub "APCPLG45" (size%,            /* Specified Batch Size      */~
                        scr_sel$,         /* Screen Selection          */~
                        sh_date$,         /* Production Date Unformatt */~
                        file$,            /* Name of Optimized File    */~
                        bat$,             /* Number of Batches Created */~
                        sh_qty%,          /* Number of Windows         */~
                        sh_grid_flag$,    /* Grid only Flag    (AWD006)*/~
                        schema%,          /* NC or TX Schema   (AWD017)*/~
                        #1,               /* (GENCODES) TABLES         */~
                        #2,               /* (@SHAPE2@) Shape Bridge   */~
                        #3,               /* (EWDGLSXX) Sorted Label   */~
                        #4,               /* (APCPLNGR) glass file     */~
                        #5,               /* (@SHAPE3@) Custom File    */~
                        #7,               /* (@SHAPE4@) Custom Remakes */~
                        #8,               /* (AWDGLSCT) Custom (AWD007)*/~
                        #9,               /* (APCPLNDT) Plan Mstr File */~
                        #6,               /* (APCPLNWK) Label Detail Fl*/~
                        #10,              /* (BCKSUBPT)Sub Part(PAR001)*/ ~
                        #13,              /* EWDHUBCL                  */~
                        #14 )             /* (AWD014) Infini-Lite Order File */


        dim scr_sel$1,                   /* Screen Selection          */ ~
            t_err$(10%)25,               /* Error Text Message(PAR002)*/ ~
            sh_date$10, bat$3,           /* Production Date Unformatt */ ~
            bat_rec$165,                 /* Batch Record, ROUTE CODE  */ ~
            seq$3,                       /* Item Numbers              */ ~
            x$1,                         /* Store Pound sysbol        */ ~
            model$3,                     /* Model Product Code        */ ~
            file$20,                     /* Batch File Name Unique    */ ~
            sh_rec$256, sh_key$32,       /* Batch File Data           */ ~
            inc$12,                      /* Batch File Identifier     */ ~
            readkey$25, desc$30,         /* Gencodes lookup           */ ~
            hdr$40,                      /* ASKUSER Header Text       */ ~
            msg$(3%)79,                  /* ASKUSER Info Text         */ ~
            errormsg$79,                 /* Error Message Text        */ ~
            hubcl_key$12,                /* key for EWDHUBCL          */ ~
            short_date$10, short_date1$10 /* Force to short date       */

        dim sh_txt2$40,                  /* Text 2                    */~
            sh_hub$10,                   /* Hub Adjustment            */~
            sh_hub1$10,                  /* Hub Adjustment 1          */~
            sh_hub2$10,                  /* Hub Adjustment 2          */~
            sh_hub3$10,                  /* Hub Adjustment 3          */~
            sh_face$4,                   /* Glass Facing code         */~
            sh_fields$7, ed_field$7,     /* Label fields              */~
            sh_bridge$7,                 /* Bridge Fields             */~
            sh_position$7,               /* Bridge Positions for lab  */~
            sh_entry$5,                  /* Data Entry                */~
            sh_glass$2,                  /* Glass Code                */~
            sh_grid$2,                   /* Glass Grid Code   (AWD004)*/~
            sh_grid_flag$1,              /* Grid only Flg(Y/N)(AWD006)*/~
            sh_dte$10,                   /* Production Date           */~
            sh_cl_lk$2,                  /* Cl or Lk code (Grid Size) */~
            s$1,                         /* Screen Code               */~
            contour$1,                   /* Contour Grid (Y) or (N)   */~
            sh_type$1,                   /* Grid size Code 1,2,3      */~
            sh_sandwich$20,              /* Glass Sandwich            */~
            shape_code$3,                /* Shape Code                */~
            sh_thickness$6,              /* Glass Thickness           */~
            sh_single$6,                 /* single Strength           */~
            sh_double$6,                 /* double Strength           */~
            sh_num$3,                    /* Glass Re-Make Number      */~
            sh_pos%(10%),                /* Calc Field Position       */~
            calc_size$9,                 /* Save Cut Size             */~
            field_flag$1,                /* Calc Field Name           */~
            face_flag$1,                 /* Glass Facing Left or Right*/~
            trk_bar$(400%)12,            /* Store Gls Barcode (AWD006)*/~
            yy$4,                        /* Year Format       (AWD003)*/~
            mm$2,                        /* Month Format      (AWD003)*/~
            dd$2,                        /* Day Format        (AWD003)*/~
            cust_rec$200,                /* Flat File         (AWD005)*/~
            ct_rec$256, ct_price$10,     /* Custom Record     (AWD007)*/~
            ct_price1$7,                 /* Bridge File Price         */~
            a$7, b$7, c$7, d$7,          /* (AWD013) fraction to dec  */~
            dept$3                       /* (CR2109) shape dept       */


        dim f2%(20%),                    /* = 0 if the file is open    */~
            ff$8, cc$8,                  /* Save File names            */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(20%)20                 /* Text from file opening     */
                                         /* (PAR000)                   */
        dim rm_key$23, rm_rec$(2%)192,   /* Remake Key and Record      */~
            sub_part$20,                 /* New Sub Part No.   (PAR000)*/~
            lab_rec$(2%)159, lab_ged$66, /* Glass Label Record (PAR001)*/~
            lab_key$62,                  /* (AWD016)                   */~
            rm_st_time$8, rm_st$1,       /* Remake Stat Time Change    */~
            rm_reason$2, chk_st$1,       /* Remake Reason Code         */~
            userid$3,                    /* Current User Id            */~
            rm_st_dte$6                  /* Remake Status Date         */

                                         /* (PAR001)                   */
        dim                                                              ~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4             /* Additional Info Fields     */

        dim logmsg$256

                                         /* (PAR001)                   */
        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! GENCODES ! Master System Table File                 *~
            * #2  ! @SHAPE2@ ! Special Shape Bridge File                *~
            * #3  ! EWDGLSXX ! Use the sorted Label File                *~
            * #4  ! APCPLNGR ! Glass Sched/Remake File          (PAR000)*~
            * #5  ! @SHAPE3@ ! Flat File for Custom Glass               *~
            * #6  ! APCPLNWK ! Label Detail Work File                   *~
            * #7  ! @SHAPE4@ ! Flat File for Custom Glass Remakes       *~
            * #8  ! AWDGLSCT ! Custom Glass Data File           (AWD007)*~
            * #9  ! APCPLNDT ! Planning Master Data File        (AWD007)*~
            * #10 ! BCKSUBPT ! New Sub Part Number DataBase     (PAR001)*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)

            x$ = bin(35%,1)                      /* Store Pound Symbol */
                                                 /* (PAR002)           */
            t_err$(1%) = "Err-1 No Equation Found  "
            t_err$(2%) = "Err-2 C.M.Rail Calc Error"
            t_err$(3%) = "Err-3 C.M.Rail Entered   "
            t_err$(4%) = "Err-4 GED Overall Thick  "
            t_err$(5%) = "Err-5 GED Sandwich Find  "
            t_err$(6%) = "Err-6 GED Spacer Descr.  "
            t_err$(7%) = "Err-7 GED No. Lits Error "
            t_err$(8%) = "Err-8 GED Hinge Code Err "
            t_err$(9%) = "Err-9 GED Muttin Error   "
            t_err$(10%) = " "
                                                 /* (PAR002)           */
            size% = 0%
            header% = 0%
            been_here% = 0%
                                                 /* (PAR000)           */

            short_date$ = sh_date$
            if len(short_date$) = 10% then                          ~
               call "DATUFMTC" (short_date$)                        ~
            else                                                    ~
               call "DATUNFMT" (short_date$)


            call "DATEFMT" (short_date$)

            short_date1$ = short_date$


            ff$ = "@SHAPE2@" : ff% = 2%        /* (AWD001)              */

            cc$ = "@SHAPE3@" : cc% = 5%        /* Custom File           */
                                               /* (AWD006)              */

            ii$ = "@SHAPE5@" : ii% = 14%       /* (AWD014) infini-lite  */

                                               /* Custom Remake File    */
            if scr_sel$ <> "5" then goto L01200
               cc$ = "@SHAPE4@" : cc% = 7%

L01200:
                                               /* (AWD006)              */
            flag% = 1%                         /* Glass File 1st        */

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

L01360:

            flag% = 2%                         /* Custom Glass 2nd      */
                                               /* Flat File Custom Glass*/
            call "OPENFILE" (#cc%, "IO   ", f2%(cc%), rslt$(cc%), axd$ )
            if f2%(cc%) <> 0% then goto L01400
               gosub file_exists
               if comp% <> 16% then goto L01380
                  call "FILEBGON" addr(#cc%)
                  goto L01400

L01380:        close #cc%
               call "OPENFILE" (#cc%,"EXTND",f2%(cc%),rslt$(cc%),axd$ )
               goto L01500

L01400:     str(rslt$(cc%),1%,6%)  = "OUTPTP"
            str(rslt$(cc%),7%,8%)  = "00001000"
            str(rslt$(cc%),15%,3%) = "100"
            str(rslt$(cc%),18%,3%) = "100"
            call "OPENFILE" (#cc%,"OUTPT",f2%(cc%), rslt$(cc%), axd$ )

L01500:

/* (AWD014) - begin */

            flag% = 3%                         /* Infini-lite  3nd      */

            call "OPENFILE" (#ii%, "IO   ", f2%(ii%), rslt$(ii%), axd$ )
            if f2%(ii%) <> 0% then goto L01450
               gosub file_exists
               if comp% <> 16% then goto L01410
                  call "FILEBGON" addr(#ii%)
                  goto L01450

L01410:        close #ii%
               call "OPENFILE" (#ii%,"EXTND",f2%(ii%),rslt$(ii%),axd$ )
               goto L01550

L01450:     str(rslt$(ii%),1%,6%)  = "OUTPTP"
            str(rslt$(ii%),7%,8%)  = "00001000"
            str(rslt$(ii%),15%,3%) = "100"
            str(rslt$(ii%),18%,3%) = "100"
            call "OPENFILE" (#ii%,"OUTPT",f2%(ii%), rslt$(ii%), axd$ )

L01550:

/* (AWD014) - end */
                                                    /* Cut Locations       */
            sh_pos%(1%) = 204%                      /* Base Calc Size      */
            sh_pos%(2%) = 213%                      /* Left Calc Size      */
            sh_pos%(3%) = 222%                      /* Right Calc Size     */
            sh_pos%(4%) = 231%                      /* Top size Calc       */
            sh_pos%(5%) = 240%                      /* S1 Cals Size        */

            calc% = 0%                              /* Position Subscript  */
            bat_no% = 1%

            call "SHOSTAT" ("Creating Batch(es)-"& file$ )

            gosub build_title

            gosub build_header
                                                    /* (AWD006)             */
            init(" ") sh_rec$, sh_key$, trk_bar$(), dept$

            cc_qty% = 0%
            trk_max% = 0%
                                                    /* (AWD006)             */
            sh_qty% = 0%
                                                    /* (EWDGLSXX) Label File*/
            str(sh_key$,1%,1%)  = "5"
            str(sh_key$,2%,6%)  = str(sh_date$,1%,6%)
            str(sh_key$,8%,20%) = file$
L02000:
            read #3,key > sh_key$, using L02010, sh_rec$, eod goto L02020
L02010:        FMT CH(256)

            if str(sh_rec$,1%,27%) <> str(sh_key$,1%,27%) then goto L02020
               sh_key$ = str(sh_rec$,1%,32%)

               gosub build_detail

               goto L02000
L02020:
            gosub build_end
            close #ff%
            convert (bat_no% - 1%) to bat$, pic(000)

        goto exit_program


        build_title                            /* New Title each Batch */
          init(" ") bat_rec$
          errormsg$="(Error)-(Title) Rec in Batch- "& file$
          inc$ = " BATCH (XXX)"
          convert bat_no% to str(inc$,9%,3%), pic(000)

          str(bat_rec$,1%,2%) = "NT"                      /* New Title */
          str(bat_rec$,3%,32%)= file$ & inc$
          str(bat_rec$,35%,131%) = " "                    /* Line Feed */
          write #ff%, bat_rec$, eod goto L03020
        return

        build_header                           /* New Header each Load */
          init(" ") bat_rec$
          errormsg$="(Error)-(Header) Rec in Batch- "& file$
          str(bat_rec$,1%,2%)   = "NH"                 /* New Header   */
                                                       /* Model Code   */
          str(bat_rec$,3%,10%)  = "Spec Shape"
                                                       /* (AWD006)     */
          if scr_sel$ = "5" then str(bat_rec$,3%,10%) = "Remk Shape"
          str(bat_rec$,13%,18%) = " "
          str(bat_rec$,31%,8%) = short_date1$         /* Prod Date    */
/* (AWD018) */
          str(bat_rec$,39%,8%) = Short_date1$         /* Gls Prod Date
          str(bat_rec$,47%,119%)= " "                  /* Line Feed    */
          write #ff%, bat_rec$, eod goto L03020
        return

        build_detail
          init(" ") bat_rec$, seq$
          errormsg$ ="(Error)-(Item) Rec in Batch- "& file$

          sh_dte$   = str(sh_rec$,2%,6%)              /* Production date */

          sh_num$   = str(sh_rec$,42%,3%)             /* Glass Re-Make No.*/

          sh_cl_lk$ = str(sh_rec$, 99%,2%)

          sh_txt2$     = str(sh_rec$,150%,40%)

          sh_hub$      = str(sh_txt2$,1%,10%)
          sh_face$     = str(sh_txt2$,11%,4%)
          sh_fields$   = str(sh_txt2$,15%,7%)
          sh_bridge$   = str(sh_txt2$,22%,7%)
          sh_position$ = str(sh_txt2$,29%,7%)
          sh_entry$    = str(sh_txt2$,36%,5%)
          ed_field$    = sh_fields$                       /* Save for Edit */
        REM  sh_glass$    = str(sh_txt2$,45%,2%)

          sh_glass$ = str(sh_rec$,45%,2%)                 /* Glass Type Code*/

                                                          /* (AWD004)      */
          sh_grid$  = str(sh_rec$,47%,2%)                 /* Grid Code     */


                                                          /* Screen code*/
          s$ = str(sh_rec$,254%,1%)                       /* Look for FGO */

          str(bat_rec$,1%,2%)   = "NI"                    /* New Detail*/
          model$ = str(sh_rec$,72%,3%)                    /* Model Code*/

          str(bat_rec$,3%,10%)  = "Spec Shape"            /* Same as Hdr*/
          str(bat_rec$,13%,3%)  = str(sh_rec$,30%,3%)     /* Seq Number*/
          str(bat_rec$,16%,4%)  = "0001"                  /* Unit Qty  */
          str(bat_rec$,20%,9%)  = str(sh_rec$,204%,9%)    /* Base      */

          if str(sh_bridge$,1%,1%) = "N" then                           ~
             str(bat_rec$,20%,9%) = "         "

          str(bat_rec$,29%,9%)  = str(sh_rec$,213%,9%)    /* Left      */

          if str(sh_bridge$,2%,1%) = "N" then                           ~
             str(bat_rec$,29%,9%) = "         "

          str(bat_rec$,38%,3%)  = str(sh_rec$,249%,3%)     /* Shape Code*/
          shape_code$ = str(sh_rec$,249%,3%)

          str(bat_rec$,41%,9%)  = str(sh_rec$,222%,9%)     /* Right     */

          if str(sh_bridge$,3%,1%) = "N" then                           ~
             str(bat_rec$,41%,9%) = "         "

          str(bat_rec$,50%,9%)  = str(sh_rec$,231%,9%)     /* Top       */

          if str(sh_bridge$,4%,1%) = "N" then                            ~
             str(bat_rec$,50%,9%) = "         "

          str(bat_rec$,59%,9%)  = str(sh_rec$,240%,9%)     /* S1        */

          if str(sh_bridge$,5%,1%) = "N" then                            ~
             str(bat_rec$,59%,9%) = "         "
                                                           /* S2 N/a    */
          str(bat_rec$,68%,9%)   =  "         "

          str(bat_rec$,77%,9%)  = "         "
          str(bat_rec$,86%,9%)  = "         "
                                                          /* Seq. No.  */

          str(bat_rec$,95%,18%) = "@" & str(sh_rec$,89%,3%) & "              "
                                                          /* Label Info*/
          str(bat_rec$,113%,4%) = "****"                  /* Reserved  */
          str(bat_rec$,117%,8%) = "        "              /* Edgework  */
                                                          /* Blank     */
          gosub lookup_sandwich
                                                          /* (AWD002)     */
          gosub check_for_temp
          if temp% = 1% then goto SKIP_TEMP

          str(bat_rec$,125%,20%)= str(sh_sandwich$,1%,20%)    /* Sandwich */
          str(bat_rec$,145%,21%) = "  "

          write #ff%, bat_rec$, eod goto L03020

          sh_qty% = sh_qty% + 1%

SKIP_TEMP:
                                                         /* (AWD002)      */
          gosub check_custom_glass                       /* Now do Custom */

                                                         /*               */
        return

        build_end
          init(" ") bat_rec$
          errormsg$ = "(Error)-(End) Rec in Batch- "& file$
          str(bat_rec$,1%,3%) = "END"                /* END OF BATCH    */
          str(bat_rec$,4%,162%) = " "                /* LINE FEED       */
          write #ff%, bat_rec$, eod goto L03020
          bat_no% = bat_no% + 1%
        return
L03020:   gosub error_prompt
        return


        lookup_sandwich
           init(" ") readkey$, desc$, sh_sandwich$
           str(readkey$, 1%,9%)  = "PLN GLASS"
           str(readkey$,10%,15%) = sh_glass$
           read #1,key = readkey$, using L03050, desc$,               ~
                                       eod goto lookup_sandwich_done
L03050:       FMT POS(25), CH(30)

REM           sh_sandwich$= str(desc$,1%,12%)
           sh_sandwich$= str(desc$,1%,20%)
        return

        lookup_sandwich_done
           sh_sandwich$ = "Error - " &sh_glass$
        return
                                                        /* (AWD002)  */
        check_for_temp
           temp% = 0%
           init(" ") readkey$, desc$
           str(readkey$, 1%,9%)  = "PLAN TEMP"
           str(readkey$,10%,15%) = sh_glass$
           read #1,key = readkey$, using L03050, desc$,               ~
                                       eod goto check_for_temp_done
           temp% = 1%
        check_for_temp_done

        return
                                                        /* (AWD002)  */
        lookup_thickness
           init(" ") readkey$, desc$
           str(readkey$, 1%,9%)  = "GED 002  "
           str(readkey$,10%,15%) = model$
           read #1,key = readkey$, using L03050, desc$,               ~
                                       eod goto lookup_thickness_done

           sh_thickness$= str(desc$,1%,6%)
           sh_single$   = str(desc$,9%,6%)
           sh_double$   = str(desc$,17%,6%)

        lookup_thickness_done

        return

        check_custom_glass
           init(" ") readkey$, desc$
           custom% = 0%
                                                   /* (AWD004)      */
                                                   /* Check for     */
                                                   /* Triple Eyebrow*/
                                                   /* (AWD009)      */
        REM   if str(sh_grid$,1%,1%) = "T" then goto check_custom_done
        REM   if str(sh_grid$,1%,1%) = "U" then goto check_custom_done
                                                   /* (AWD004)      */
                                                   /* (AWD010)      */
           if sh_grid$ = "59" then goto check_custom_done
                                                   /* By-Pass       */

           str(readkey$, 1%,9%)  = "SHAP CUST"
           str(readkey$,10%,15%) = shape_code$
           read #1,key = readkey$, using L03050, desc$,               ~
                                       eod goto check_custom_done

           custom% = 1%
           gosub update_custom

        check_custom_done

        return
                                       /* (PAR000)        */
        check_grid_size
           sh_type$ = "0"              /* No Grid         */
           gosub lookup_sub_part
                                       /* 3/8 Inch Grid   */
           if str(sub_part$,2%,1%) = "4" then sh_type$ = "4"
                                       /* 5/8 Inch Grid   */
           if str(sub_part$,2%,1%) = "1" then sh_type$ = "3"

                                       /* 3/4 Inch Grid   */
           if str(sub_part$,2%,1%) = "2" then sh_type$ = "2"
/* (AWD011) */
                                       /* 1 Inch Grid     */
REM           if str(sub_part$,1%,1%) = "2" then sh_type$ = "1"
           if str(sub_part$,1%,1%) = "2" and          ~
                  str(sub_part$,2%,1%) = "3" then sh_type$ = "1"
                                       /* 18 mm           */
           if str(sub_part$,1%,1%) = "2" and          ~
                  str(sub_part$,2%,1%) = "1" then sh_type$ = "5"
/* (/AWD011) */

/* (AWD012) SDL GRID TYPE 6  */
           if str(sub_part$,8,1) = "1" then sh_type$ = "6"

                                       /* Contour Grid Y/N*/
           contour$ = "N"
           if sh_type$ = "1" then contour$ = "Y"
/* (AWD015) */
           if str(sub_part$,1%,1%) = "2" then contour$ = "Y"

        return                         /* (PAR000)        */

        update_custom
            init(" ") cust_rec$

            if custom% = 0% then return

            if header% = 0% then gosub print_header

            call "DATEFMT" (sh_dte$)
                                                         /* (AWD008) Special Grid Check   */
                                                         /* Skip Certain Grid Codes       */
        REM    if str(sh_rec$,47%,2%) = "E0" then return
                                                         /* Skip Grid Only Circles        */
        REM    if str(sh_rec$,47%,2%) = "P0" then return
                                                         /* Skip Grid Only Picture Window */
        REM    if str(sh_rec$,47%,2%) = "I0" then return
                                                         /* Skip Grid Only Octagons       */
                                                         /* Skip Grid Only Tempered       */
        REM    if temp% = 1% and str(sh_rec$,48%,1%) = "0" then return

            sh_glass$ =  str(sh_rec$,45%,2%)
                                                         /* Special test to skip all      */
                                                         /* Glass codes 30% to 99%        */
            sh_glass% = 0%
            convert sh_glass$ to sh_glass%, data goto L03100
L03100:
        REM    if sh_glass% > 29% then return            /* All Glass Per Chad 11-23-04   */

            init(" ") logmsg$
                                                         /* (AWD008)             */
            gosub check_grid_size

            gosub lookup_thickness

            str(cust_rec$,1%,5%)  = str(sh_rec$,87%,5%)  /* Prod. Seq No. (5) */
            str(cust_rec$,6%,1%)  = ","
                                                         /* (AWD003)          */
                                                         /* New Date Format   */
            yy$ = "20" & str(short_date1$,7%,2%)

            mm$ = str(short_date1$,1%,2%)

            dd$ = str(short_date1$,4%,2%)

        REM    str(cust_rec$,7%,10%) = short_date1$      /* Prod_date     (10)*/

            str(cust_rec$,7%,10%) = yy$ & mm$ & dd$ & "  "
                                                         /* Format yyyymmdd   */
                                                         /* (AWD003) - End    */

            str(cust_rec$,17%,1%) = ","

            str(cust_rec$,18%,8%) = str(sh_rec$,54%,8%)  /* Sales order   (08)*/
            str(cust_rec$,26%,1%) = ","

            str(cust_rec$,27%,3%) = str(sh_rec$,72%,3%)  /* Model Code    (03)*/
            str(cust_rec$,30%,1%) = ","

            str(cust_rec$,31%,6%) = str(sh_rec$,193%,6%) /* Color Code    (06)*/
            str(cust_rec$,37%,1%) = ","
                                                         /* Grid Type Code(06)*/
            if sh_type$ = "1" then str(cust_rec$,38%,6%) = " 1    "
            if sh_type$ = "2" then str(cust_rec$,38%,6%) = " 3/4  "
            if sh_type$ = "3" then str(cust_rec$,38%,6%) = " 5/8  "
/* (AWD011) */
            if sh_type$ = "5" then str(cust_rec$,38%,6%) = " 18   "
/* (AWD012) */
            if sh_type$ = "6" then str(cust_rec$,38%,6%) = " SDL  "


                                                         /* Check for No Grid */
            if str(sh_rec$,48%,1%) = "0" then str(cust_rec$,38%,6%) = "      "
/* (AWD012) */
REM do not order grid from custom
            if sh_type$ = "6" then str(cust_rec$,38%,6%) = "      "

            str(cust_rec$,44%,1%) = ","
                                                         /* Grid Code     (04)*/
            str(cust_rec$,45%,4%) = " " & str(sh_rec$,47%,2%) & " "
/* (AWD012) */
REM make grid code end with 0 no grid
            if sh_type$ = "6" then str(cust_rec$,47%,1%) = "0"

            str(cust_rec$,49%,1%) = ","

                                                         /* Glass Type    (05)*/
            str(cust_rec$,50%,5%) = " " & str(sh_rec$,45%,2%) & "  "
            str(cust_rec$,55%,1%) = ","
                                                         /* Shape Base (Width)*/
            gosub find_base
            str(cust_rec$,56%,9%) = calc_size$           /* Shape Base Sz (09)*/
            str(cust_rec$,65%,1%) = ","
            save_width = 0.00                            /* (AWD013)          */
            gosub save_base                              /* (AWD013)          */
            save_width = a                               /* (AWD013           */
                                                         /* Shape Height      */


         logmsg$ = "$$ " & str(sh_rec$,54,8) & str(sh_rec$,255,2) &        ~
                   ", " & calc_size$ & ", " & str(sh_rec$,72,3) & "," & ~
           str(sh_rec$,47,2)
REM     call "LOGFILE" (logmsg$)


            gosub find_height
            str(cust_rec$,66%,9%) = calc_size$           /* Shape Hght Sz (09)*/
            str(cust_rec$,75%,1%) = ","
            save_height = 0.00                           /* (AWD013)          */
            gosub save_base                              /* (AWD013)          */
            save_height = a                              /* (AWD013)          */
            sq_ft = 0.00                                 /* (AWD013)          */
            sq_ft = round((save_width * save_height) / 144,4)



                                                         /* Shape Radius      */
            gosub find_radius                            /* Shape Field       */
                                                         /* Name of Field (04)*/
            str(cust_rec$,76%,4%) = " "& field_flag$ &"  "
            str(cust_rec$,80%,1%) = ","

            str(cust_rec$,81%,9%) = calc_size$           /* Shape Rad Sz  (09)*/
            str(cust_rec$,90%,1%) = ","

            gosub find_leg                               /* Shape Leg         */
                                                         /* Name of Field (04)*/
            str(cust_rec$,91%,4%) = " "& field_flag$ &"  "
            str(cust_rec$,95%,1%) = ","

            str(cust_rec$,96%,9%) = calc_size$           /*Shape Leg Size (09)*/
            str(cust_rec$,105%,1%) = ","

            gosub get_facing                             /* Lookup Facing     */
                                                         /* Facing L or R (04)*/
            str(cust_rec$,106%,4%) = " "& face_flag$ &"  "
            str(cust_rec$,110%,1%) = ","
                                                         /*Glass Thickness(09)*/
            str(cust_rec$,111%,9%) = " "& sh_thickness$ &"  "
            str(cust_rec$,120%,1%)= ","

            str(cust_rec$,121%,4%)= " N  "               /*See Drawing Y/N(04)*/
            str(cust_rec$,125%,1%)= ","

            str(cust_rec$,126%,3%)= "   "                /* F = FGO           */
            if s$ = "6" then str(cust_rec$,127%,1%) = "F"
            str(cust_rec$,129%,1%)= ","

REM         str(cust_rec$,130%,10%)= "          "        /* Hub 1 Adj         */
            sh_hub$ = str(sh_rec$,150%,10%)
        /*@@@*/
            sh_hub1$ = "          "
            sh_hub2$ = "          "
            sh_hub3$ = "          "
REM         hubcl_key$ = str(sh_rec$,33,12)
REM         hubcl_key$ = str(sh_rec$,33,9)
            hubcl_key$ = str(sh_rec$,54,8) & str(sh_rec$,255,2)
        read #13,key = hubcl_key$,using hubcalfl,hubcl_key$,sh_hub1$,   ~
               sh_hub2$,sh_hub3$, eod goto L02011
hubcalfl:   FMT CH(12),3*CH(10)
            goto L02012

L02011:     sh_hub1$ = "          "
            sh_hub2$ = "          "
            sh_hub3$ = "          "
L02012:
        /*@@@*/
            str(cust_rec$,130%,10%)= sh_hub1$            /* Hub 1 Adj         */
            str(cust_rec$,140%,1%) = ","

REM            str(cust_rec$,141%,10%)= "          "        /* hub 2 Adj         */
            str(cust_rec$,141%,10%)= sh_hub2$            /* hub 2 Adj         */
            str(cust_rec$,151%,1%) = ","

REM            str(cust_rec$,152%,10%)= "          "        /* Hub 3 Adj         */
            str(cust_rec$,152%,10%)= sh_hub3$            /* Hub 3 Adj         */
            str(cust_rec$,162%,1%) = ","

                                                         /* (AWD001)          */
                                                         /* (AWD005)          */
            str(cust_rec$,163%,12%)= str(sh_rec$,33%,12%)/* Glass Barcode     */
                                                         /* and Glass Remake  */
                                                         /* number            */
                                                         /* (AWD006)          */
            trk_max% = trk_max% +1%
            trk_bar$(trk_max%) = str(sh_rec$,33%,12%)
                                                         /* (AWD006)          */
            str(cust_rec$,175%,1%) = ","
                                                         /* (AWD005)          */
                                                         /* Remake Number     */
            str(cust_rec$,176%,3%) = str(sh_rec$,200%,3%)/* Printable Version */
            str(cust_rec$,179%,1%) = ","
                                                         /* Custom Price      */
            str(cust_rec$,180%,7%) = "000.00 "
            str(cust_rec$,187%,1%) = ","
                                                         /* Grid Only Flag    */
                                                         /* (AWD005) Set for  */
                                                         /* Tempered          */
            str(cust_rec$,188%,1%) = "N"
            if temp% = 1% then str(cust_rec$,188%,1%) = "Y"
            if temp% <> 1% and sq_ft > 25 ~
                    then str(cust_rec$,188%,1%) = "Z"  /* (AWD013)      */

            str(cust_rec$,189%,1%) = ","
                                                         /* Growth   (11)     */
            str(cust_rec$,190%,11%) = " "
                                                         /* (AWD007)          */
            gosub format_custom_record
            str(cust_rec$,180%,7%) = ct_price1$
                                                         /* (AWD007)          */
                                                         /* Record = 200      */
                                                         /* (AWD005)          */
                                                         /* (AWD001)          */
            write #cc%, cust_rec$, eod goto L04000

          cc_qty% = cc_qty% + 1%

/* (AWD012) */

             if sh_type$ = "6"  then goto write_sdl_grid_only

/* (AWD013) */
             if str(cust_rec$,188%,1%) = "Z" then str(cust_rec$,188%,1%) = "Y"

        return
L04000:
        gosub error_prompt_custom

        return
/* (AWD012) */
        write_sdl_grid_only
            sdl_price% = 1%
            str(cust_rec$,1%,1%)  = "S"                  /* Prod. Seq No. (5) */
                                                         /* Grid Type Code(06)*/
            if sh_type$ = "6" then str(cust_rec$,38%,6%) = " SDL  "
                                                         /* Grid Code     (04)*/
            str(cust_rec$,45%,4%) = " " & str(sh_rec$,47%,2%) & " "
                                                         /* Code for Grid Only*/
            str(cust_rec$,188%,1%) = "Y"
            gosub format_custom_record
            str(cust_rec$,180%,7%) = ct_price1$

                                                         /* Record = 200      */
            write #ii%, cust_rec$, eod goto L04200

            cc_qty% = cc_qty% + 1%

            sdl_price% = 0%
        return
L04200:
        gosub error_prompt_infinilite

        return
/* (AWD013) */
        save_base
REM           call "SHOSTAT" (" CONVERTING SIZES " )  stop
           init(" ") a$, b$, c$, d$
           a, b, c, d = 0.00

           a$ = str(calc_size$,1,3)
           b$ = str(calc_size$,4,7)

           convert a$ to a, data goto bad_size

           if b$ = " " then return   /* No Fraction */

REM           p% = pos(b$ = "/")
REM           if p% = 1% then b$ = str(b$,1,1)
REM           if p% = 1% then c$ = str(b$,3,1)
REM           if p% = 2% then b$ = str(b$,1,2)
REM           if p% = 2% then c$ = str(b$,4,2)
           c$ = str(b$,2,2)
           d$ = str(b$,5,2)

           convert c$ to c, data goto bad_size

           convert d$ to d, data goto bad_size

           b = round(c/d,4)

           a = a + b

bad_size:
           return


        find_base
           field_flag$ = "W"
           calc_size$ = "         "
           p% = pos(ed_field$ = field_flag$)
           if p% = 0% then goto find_base_done
              convert str(sh_position$,p%,1%) to calc%, data goto find_base_done

           calc_size$ = str(sh_rec$,sh_pos%(calc%),9%)

           str(ed_field$,p%,1%) = "N"                 /* Used Field           */
        find_base_done
        return

        find_height
           field_flag$ = "H"
           calc_size$ = "         "
           p% = pos(ed_field$ = field_flag$)
           if p% = 0% then goto find_height_done
              convert str(sh_position$,p%,1%) to calc%, data goto find_height_done

           calc_size$ = str(sh_rec$,sh_pos%(calc%),9%)

           str(ed_field$,p%,1%) = "N"                /* used Field            */
        find_height_done
        return

        find_radius                                 /* Shape Codes 00, 05, 06 */
                                                    /* 07, 63 No third Value  */

           if shape_code$ = "001" then goto find_leg   /* Has a third Value      */
           if shape_code$ = "002" then goto find_leg   /* Has a third Value      */
           if shape_code$ = "003" then goto find_leg   /* Has a third Value      */
           if shape_code$ = "004" then goto find_leg   /* Has a third Value      */
           if shape_code$ = "015" then goto find_leg   /* Has a third Value      */
           if shape_code$ = "025" then goto find_leg   /* Has a third Value      */

           field_flag$ = "R"
           calc_size$ = "         "
           p% = pos(ed_field$ = field_flag$)       /* Radius      */
           if p% = 0% then goto find_radius_done
              convert str(sh_position$,p%,1%) to calc%, data goto find_radius_done

           calc_size$ = str(sh_rec$,sh_pos%(calc%),9%)

           field_flag$= "R"
           str(ed_field$,p%,1%) = "N"              /* used Field  */

        return

        find_radius_done
           field_flag$ ="N"
        return

        find_leg
           field_flag$ = "L"
           calc_size$ = "         "
           p% = pos(ed_field$ = field_flag$)       /* Leg Height */
           if p% = 0% then goto find_leg_1
              convert str(sh_position$,p%,1%) to calc%, data goto find_leg_done

           calc_size$ = str(sh_rec$,sh_pos%(calc%),9%)
           str(ed_field$,p%,1%) = "N"              /* used Field */

           return

        find_leg_1
           field_flag$ = "S"
           calc_size$ = "         "
           p% = pos(ed_field$ = field_flag$)       /* Left Side Leg */
           if p% = 0% then goto find_leg_2
              convert str(sh_position$,p%,1%) to calc%, data goto find_leg_done

           calc_size$ = str(sh_rec$,sh_pos%(calc%),9%)
           str(ed_field$,p%,1%) = "N"              /* used Field    */
           return

        find_leg_2
           field_flag$ = "Z"
           calc_size$ = "         "
           p% = pos(ed_field$ = field_flag$)       /* Right Side Leg */
           if p% = 0% then goto find_leg_3
              convert str(sh_position$,p%,1%) to calc%, data goto find_leg_done

           calc_size$ = str(sh_rec$,sh_pos%(calc%),9%)
           str(ed_field$,p%,1%) = "N"              /* used Field     */
           return

        find_leg_3
           field_flag$ = "T"
           calc_size$ = "         "
           p% = pos(ed_field$ = field_flag$)       /* Top Leg        */
           if p% = 0% then goto find_leg_4
              convert str(sh_position$,p%,1%) to calc%, data goto find_leg_done

           calc_size$ = str(sh_rec$,sh_pos%(calc%),9%)
           str(ed_field$,p%,1%) = "N"              /* used Field     */
           return

        find_leg_4
           field_flag$ = "X"
           calc_size$ = "         "
           p% = pos(ed_field$ = field_flag$)       /* Side Leg Height */
           if p% = 0% then goto find_leg_5
              convert str(sh_position$,p%,1%) to calc%, data goto find_leg_done

           calc_size$ = str(sh_rec$,sh_pos%(calc%),9%)
           str(ed_field$,p%,1%) = "N"              /* used Field      */

        find_leg_5
           field_flag$ = "N"                       /* Not Applicable  */
        return

        find_leg_done
           field_flag$ = "E"                       /* Error No Flag   */
        return

        get_facing
           init(" ") readkey$, desc$, face_flag$
           str(readkey$,1%,9%)   = "PLN FACE "     /* Shape Facing        */
           str(readkey$,10%,15%) = sh_face$
           read #1,key = readkey$, using L03050, desc$,                ~
                                         eod goto get_facing_done

           face_flag$ = str(desc$,1%,1%)
        return

        get_facing_done
           face_flag$ = "N"
        return

        return

        print_header

            str(cust_rec$,1%,5%)  = "Seq. "              /* Prod. Seq No. */
            str(cust_rec$,6%,1%)  = ","

            str(cust_rec$,7%,10%) = "Prod Date "         /* Prod_date     */
            str(cust_rec$,17%,1%) = ","

            str(cust_rec$,18%,8%) = "Sales Ord."         /* Sales order   */
            str(cust_rec$,26%,1%) = ","

            str(cust_rec$,27%,3%) = "Mod"                /* Model Code     */
            str(cust_rec$,30%,1%) = ","

            str(cust_rec$,31%,6%) = "Color "             /* Color Code     */
            str(cust_rec$,37%,1%) = ","

            str(cust_rec$,38%,6%) = "Grd Sz"             /* Grid Size      */
            str(cust_rec$,44%,1%) = ","

            str(cust_rec$,45%,4%) = "Grid"               /* Grid Code      */
            str(cust_rec$,49%,1%) = ","

            str(cust_rec$,50%,5%) = "Glass"              /* Glass Type     */
            str(cust_rec$,55%,1%) = ","
                                                         /* Shape Base (Width) */
            str(cust_rec$,56%,9%) = "Width    "          /* Shape Base Size*/
            str(cust_rec$,65%,1%) = ","
                                                         /* Shape Height   */
            str(cust_rec$,66%,9%) = "Height   "          /* Shape Height Size*/
            str(cust_rec$,75%,1%) = ","
                                                         /* Shape Radius   */
            str(cust_rec$,76%,4%) = "Flag"               /* Name of Field Flag*/
            str(cust_rec$,80%,1%) = ","

            str(cust_rec$,81%,9%) = "GlassSize"          /* Shape Radius Size*/
            str(cust_rec$,90%,1%) = ","

            str(cust_rec$,91%,4%) = "Flag"               /* Name of Field  */
            str(cust_rec$,95%,1%) = ","

            str(cust_rec$,96%,9%) = "GlassSize"          /* Shape Leg Size */
            str(cust_rec$,105%,1%)= ","

            str(cust_rec$,106%,4%)= "Face"               /* Facing L-R     */
            str(cust_rec$,110%,1%)= ","

            str(cust_rec$,111%,9%)= "Gls Thick"          /* Glass Thickness*/
            str(cust_rec$,120%,1%)= ","

            str(cust_rec$,121%,4%)= "Draw"               /* See Drawing Y/N*/
            str(cust_rec$,125%,1%)= ","
                                                         /* FGO Blank or 'F'*/
            str(cust_rec$,126%,3%)="FGO"
            str(cust_rec$,129%,1%)=","

            str(cust_rec$,130%,10%)= "Hub Adj 1 "        /* Hub 1 Adj       */
            str(cust_rec$,140%,1%) = ","

            str(cust_rec$,141%,10%)= "Hub Adj 2 "        /* hub 2 Adj       */
            str(cust_rec$,151%,1%) = ","

            str(cust_rec$,152%,10%)= "Hub Adj 3 "        /* Hub 3 Adj       */
            str(cust_rec$,162%,1%) = ","
                                                         /* (AWD001)        */
                                                         /* (AWD005)        */
            str(cust_rec$,163%,12%)= "  Barcode   "      /* Glass Barcode   */
            str(cust_rec$,175%,1%) = ","
                                                         /* (AWD005)        */
                                                         /* Remake Number   */
            str(cust_rec$,176%,3%) = "RMK"
            str(cust_rec$,179%,1%) = ","
                                                         /* Custom Price    */
            str(cust_rec$,180%,7%) = " Price "
            str(cust_rec$,187%,1%) = ","
                                                         /* Grid Only Flag  */
            str(cust_rec$,188%,1%) = "G"
            str(cust_rec$,189%,1%) = ","
                                                         /* Growth   (11)   */
            str(cust_rec$,190%,11%) = " "
                                                         /* Record = 200    */
                                                         /* (AWD005)        */
                                                         /* (AWD001)        */
            write #cc%, cust_rec$, eod goto L04100

/* (AWD014) */
            write #ii%, cust_rec$, eod goto L04100

            header% = 1%
        return
L04100:
        return

        file_exists
            comp% = 2%
            hdr$ = "** Optimization File Exists **"
            msg$(1%) = "The File (XXXXXXXX) Already Exists. "
            str(msg$(1%),11%,8%) = ff$
            if flag% = 2% then str(msg$(1%),11%,8%) = cc$
/* (AWD014) */
            if flag% = 3% then str(msg$(1%),11%,8%) = ii$

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

        error_prompt_custom
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - E r r o r  C u s t o m  - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
/* (AWD014) - begin */
        error_prompt_infinilite
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - E r r o r  I N F I N I - L I T E - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return
/* (AWD014) - end */
        exit_program

           gosub create_glass

        end

        create_glass
                                               /* (AWD006)             */
            call "SHOSTAT" ("Updating Special Shape Glass")
                                               /* (AWD006)             */
                                               /* (APCPLNWK) Work File */
        /* RHHTEST */
        REM return
/* (AWD016) */
            lab_key$, lab_ged$ = all(hex(00))
            read #6,key > lab_key$, using L05100, lab_key$, lab_ged$, lab_rec$(),~
                                                   eod goto create_done
            goto L05110
        create_next
            read #6, using L05100, lab_key$, lab_ged$, lab_rec$(),              ~
                                                   eod goto create_done
L05100:       FMT CH(62), CH(66), 2*CH(159)

L05110:                                         /* (PAR002)              */
/* (AWD017) */
REM            IF STR(LAB_REC$(),183%,3%) <> "043" THEN GOTO L05150
/* + (CR2109) */
REM IF STR(LAB_REC$(),183%,3%) <> "043" AND SCHEMA% = 1%       ~
REM THEN GOTO L05150
REM IF STR(LAB_REC$(),183%,3%) <> "002" AND                    ~
REM STR(LAB_REC$(),183%,3%) <> "003" AND SCHEMA% = 2%       ~
REM THEN GOTO L05150

               check% = 0%
               dept$ = str(lab_rec$(),183%,3%)
               gosub checkDept
               if check% = 0% then goto L05150
/* - (CR2109) */               
               gosub update_glass

               goto create_next

        create_done

        return

L05150:
                                   /* (PAR002)                           */
        er% = 10%
        convert str(lab_rec$(),32%,1%) to er%, data goto L05170
L05170:

             init(" ") errormsg$
             errormsg$ = "Mod= "& str(lab_ged$,10%,3%) & " SO= " &       ~
                          str(lab_ged$,27%,8%) & "--" & t_err$(er%)

             gosub error_prompt

        goto create_next
                                   /* (PAR002)                           */

        update_glass               /* All Glass Has at Least One         */
                                   /* entry in (APCPLNGR)                */
                                   /* Status 0=Remake,1=Sched,2=Complete */
           init(" ") rm_key$,rm_st_time$,rm_reason$,rm_st_dte$,          ~
                     rm_rec$(), rm_st$, errormsg$, chk_st$
                                                 /* (PAR000)           */
           call "TIME" (rm_st_time$)             /* Time Glas Scheduled*/
           rm_st$ = "1"                          /* Scheduled Glass    */
           rm_reason$ = "00"                     /* Normal Sched Glass */
           rm_st_dte$ = date                     /* Todays Date        */
           str(rm_key$,1%,9%)  = str(lab_ged$,53%,9%)  /* Glass Barcode*/
           str(rm_key$,10%,3%) = str(lab_rec$(),186%,3%) /* Re-Make No.  */

           read #4,hold,key = rm_key$, using L05200, rm_rec$(),         ~
                                                  eod goto schedule_glass
L05200:       FMT 2*CH(192)
           chk_st$ = str(rm_rec$(),13%,1%)
        REM   if chk_st$ = "1" then goto L05215     /* No Update       */
              delete #4
                                                 /* Should not happen  */
                                                 /* at this time       */

                                                 /* Found re-Mkae Turned on*/
              str(rm_rec$(),1%,6%) = str(sh_date$,1%,6%)  /* Prod. Dte */
              str(rm_rec$(),7%,6%) = str(sh_date$,1%,6%)  /* Scan. Dte */
              str(rm_rec$(),13%,1%) = rm_st$     /* Chg Status to Complete*/
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
              goto L05210
        schedule_glass                           /* Create Glass Data  */
           str(rm_rec$(),1%,6%) = str(sh_date$,1%,6%)  /* Glass Prod Dt*/
           str(rm_rec$(),7%,6%) = str(sh_date$,1%,6%)  /* Glass Prod Dt*/
           str(rm_rec$(),13%,1%) = rm_st$        /* Scheduled Glass    */
           str(rm_rec$(),14%,8%) = rm_st_time$   /* Time of Stat Change*/
           str(rm_rec$(),22%,9%) = str(lab_ged$,53%,9%) /* Glass Barcode */
           str(rm_rec$(),31%,3%) = str(lab_rec$(),186%,3%) /* Remake No.   */

           str(rm_rec$(),34%,2%) = rm_reason$      /* Glass Reason Code  */
           str(rm_rec$(),36%,6%) = date            /* Date of Stat Change*/
           str(rm_rec$(),42%,2%) = str(lab_rec$(),178%,2%) /* Scanned Shift*/

                                                 /* Time 24 hour clock */
           str(rm_rec$(),44%,8%) = time          /* Scheduled Time     */
           str(rm_rec$(),52%,6%) = date          /* Scheduled Date Today*/
           str(rm_rec$(),58%,3%) = userid$       /* Who Scheduled Glass*/
           str(rm_rec$(),61%,4%) = "    "        /* Completion Calc    */
           str(rm_rec$(),65%,1%) = " "           /* Glass Tracking Code*/
           str(rm_rec$(),66%,1%) = " "           /* Growth Area        */
           str(rm_rec$(),67%,188%) = str(lab_rec$(),1%,188%)  /* (PAR001) */
                                                 /* Calculated Data    */
                                                 /* (PAR000)           */
L05210:
                                                 /* (AWD006)           */
                                                 /* Set Flag for Custom*/
           for jj% = 1% to trk_max%
               if trk_bar$(jj%) <> str(rm_rec$(),22%,12%) then goto TRK_1
                  str(rm_rec$(),65%,1%) = "7"    /* Custom Glass Order */

                  goto TRK_2
TRK_1:
           next jj%
TRK_2:                                           /* (AWD006)           */
                                                 /* (PAR001)           */
           str(rm_rec$(),255%,20%) = str(lab_rec$(),191%,20%)
                                                 /* (PAR001)           */

           write #4, using L05200, rm_rec$(), eod goto L05220
        return
L05220:    errormsg$="(Err)- Updating Special Shape Glass Database"
           gosub error_prompt
        return
                                                 /* (PAR000)           */
        return
                                                 /* (AWD007) - Begin   */
        format_custom_record

           init(" ") ct_rec$

           str(ct_rec$,1%,6%) = str(sh_date$,1%,6%)  /* Production Date*/

           str(ct_rec$,7%,5%) = str(sh_rec$,87%,5%)  /* Production Seq. No */

           str(ct_rec$,12%,12%)= str(sh_rec$,33%,12%) /* Barcode and Remake*/

           str(ct_rec$,24%,8%)= str(sh_rec$,54%,8%)  /* Sales Order Number */

           str(ct_rec$,32%,3%)= str(sh_rec$,72%,3%)  /* Model Code         */

           str(ct_rec$,35%,6%)= str(sh_rec$,193%,6%) /* Product Color      */

           str(ct_rec$,41%,3%)= str(sh_rec$,249%,3%) /* Special Shape Code */

           str(ct_rec$,44%,3%)= str(cust_rec$,39%,3%)/* Grid Size          */

           str(ct_rec$,47%,2%)= str(sh_rec$,47%,2%)  /* Grid Code          */
/* (AWD014) */
           if sh_type$ = "6" and sdl_price% = 0% then str(ct_rec$,48%,1%) = "0"

           str(ct_rec$,49%,2%)= str(sh_rec$,45%,2%)  /* Glass Code         */

           str(ct_rec$,51%,9%)= str(cust_rec$,56%,9%) /* Glass Base Width  */

           str(ct_rec$,60%,9%)= str(cust_rec$,66%,9%) /* Glass Height      */

           str(ct_rec$,69%,1%)= str(cust_rec$,77%,1%) /* Radius Flag       */

           str(ct_rec$,70%,9%)= str(cust_rec$,81%,9%) /* Radius size       */

           str(ct_rec$,79%,1%)= str(cust_rec$,92%,1%) /* Short Leg Flag    */

           str(ct_rec$,80%,9%)= str(cust_rec$,96%,9%) /* Short Leg Size    */

           str(ct_rec$,89%,1%)= str(cust_rec$,107%,1%)/* Glass Facing      */

           str(ct_rec$,90%,6%)= str(cust_rec$,112%,6%)/* Glass Thickness   */

           str(ct_rec$,96%,1%)= str(cust_rec$,122%,1%)/* Drawing Y or N    */

           str(ct_rec$,97%,1%)= str(cust_rec$,127%,1%)/* F = FGO           */

           str(ct_rec$,98%,27%)= " "                  /* Hub 1, 2, 3       */

           str(ct_rec$,125%,1%)= "N"                  /* Tempered Y or N   */
           if temp% = 1% then str(ct_rec$,125%,1%) = "Y"

           str(ct_rec$,126%,1%)= str(cust_rec$,188%,1%) /* Grid Only       */

           str(ct_rec$,127%,8%)= "        "          /* Glass Price        */

           str(ct_rec$,135%,1%) = "Y"                /* Price Code Y,N,W   */

           str(ct_rec$,136%,2%) = "00"               /* Remake Reason Code */

           str(ct_rec$,138%,8%) = "        "         /* window Price       */

           str(ct_rec$,146%,8%) = "        "         /* Glass Pct of Price */

           str(ct_rec$,154%,6%) = " "                /* Glass Receive Date */

           str(ct_rec$,160%,8%) = " "                /* Glass Receive Time */

           str(ct_rec$,168%,3%) = "???"              /* Receive User Id    */

           str(ct_rec$,171%,12%)= str(ct_rec$,12%,12%) /* Barcode Remake No*/
                                                     /* (PAR000)           */
           str(ct_rec$,183%,1%) = contour$           /* Contour Grid Y/N   */

           str(ct_rec$,184%,1%) = "N"                /* Wood Surround (Y/N)*/

           str(ct_rec$,185%,10%)= "           "      /* Pricing Calculation*/

           str(ct_rec$,195%,25%)= "           "      /* MFG Part Number    */

           str(ct_rec$,220%,37%)= " "                /* Filler Area        */

           ct_price = 0.0
           err% = 0%
           call "AWDCALPR"   (been_here%,    /* Initialize Arrays          */~
                              scr_sel$,      /* Screen Selection           */~
                              ct_rec$,       /* Custom Data Record         */~
                              ct_price$,     /* Custom Glass Price         */~
                              #1,            /* GENCODES File              */~
                              #8,            /* AWDGLSCT Custom Database   */~
                              #9,            /* APCPLNDT Planning Database */~
                              err% )         /* Error Code 0 = Ok, 0 <> err*/

           convert ct_price$ to ct_price, data goto L05300

L05300:
           convert ct_price to ct_price1$, pic(###.##-)

        return
                                                     /* (AWD007) - End */

                                                     /* (PAR001)       */
        lookup_sub_part
            init(" ") bcksubpt_rec$, flds$(), info_flds$(), so_inv$, item_no$
            flag$ = "0"                  /* Sales Order Info           */
            pgm$  = "1"
            err1% = 0%
            so_inv$  = str(sh_rec$,54%,8%)
            item_no$ = str(sh_rec$,255%,2%)

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

        call "AWDBKSUB"   (flag$,        /* Flag 0=SalesOrder 1=Invoice*/~
                          pgm$,          /* Calling Program 0=BCKUPDTE */~
                                         /* 1=Any Other 2=Delete       */~
                                         /* 3=Invoice                  */~
                          so_inv$,       /* SO or Invoice Num to lookup*/~
                          item_no$,      /* Item Number                */~
                          bcksubpt_rec$, /* Record If BCKUPDTE then    */~
                                         /* pass in else pass out      */~
                          flds$(),       /* Part Number Fields         */~
                          info_flds$(),  /* Information Fields         */~
                          #10,           /* BCKSUBPT File              */~
                          err1%)         /* Error Code                 */

            if err1% <> 0% then str(bcksubpt_rec$,48%,20%) = "00000000000000000000"

            sub_part$ = str(bcksubpt_rec$,48%,20%)

            if err1% = 0% then return

            errormsg$ = "Read Error-S.O.= "&so_inv$&" Line= "& item_no$
            gosub error_prompt
            err1% = 0%
        return
                                                         /* (PAR001)    */
/* + (CR2109)  */
        checkDept
           if dept$ = "043" and schema% = 1% then check% = 1%
           
           if dept$ = "002" and schema% = 2% then check% = 1%
           if dept$ = "003" and schema% = 2% then check% = 1%
           if dept$ = "064" and schema% = 2% then check% = 1%
        return
/* - (CR2109)  */


                                                        

