*       ****************************************************************~
*                       NEW -( R e p o r t s )                         *~
*                                                                      *~
*                  ( As of 05/05/08 -  )                               *~
*                                                                      *~
*        AWDPLQ42 - Slow Moving Lineals Report                         *~
*            Note - Primary Subroutine 'CALC_CUTS' at Line ( 3040 )    *~
*                   SPEC% = 11%- Slow Moving Lineals                   *~
*----------------------------------------------------------------------*~
*Mod Date  ! Description                                           !By *~
*----------+-------------------------------------------------------+---*~
*05/05/08  ! New Report                                            !CMG*~
*05/19/2014! (CUT001) mod to add dim fields to CUTCC               !CMG*~
************************************************************************

        sub "AWDPLQ42" (scr_dte$,        /* Production Date           */ ~
                        prod_dte$,       /* Completion Date           */ ~
                        spec%,           /* 0%-4%,9% Reg,5%-8%,10%Spec*/ ~
                        scr_dept$,       /* Department Code           */ ~ 
                        #1,              /* (APCCUTWK) Cut Sheet Work */ ~
                        #3,              /* (GENCODES) Table File     */ ~
                        #4,              /* (AMTBOMIF) Validity File  */ ~
                        #5,              /* (HNYMASTR) Part Master    */ ~
                        #7,              /* (APCCUTEQ) Saw Cross Ref  */ ~
                        #8 )             /* (AMTBOMCD) Equation File  */

        dim scr_dte$8, userid$3,         /* Production Date            */~
            scr_dept$3,                  /* Department Code            */~
            scr_dte1$8,                  /* Screen Date Unformatted    */~
            dt_dept$3,                   /* Department                 */~
            prod_dte$8,                  /* Completion Date            */~
            wrk_key1$51,                 /* Cut Sheet Work Key         */~
            wrk_rec$200,                 /* Cut Sheet Work Record      */~
            slow_key$34,                 /* Slow moving key            */~
            slow_rec$256,                /* Slow Moving Item Record    */~
            readkey$50,                  /* Code Table Key             */~
            slow$1,                      /* Slow Moving Part 0 or 1    */~
            size$20,                     /* Length Extruded Lineal     */~
            descr$32,                    /* Code Table Description     */~
            scr_msg$40, title$40,        /* Header Text                */~
            sav_mod$3,                   /* Save Model Code            */~
            sav_cl$1,                    /* Save Color Code            */~
            sav_part$25,                 /* Save Part Number           */~
            raw_part$25,                 /* Raw Material Part          *~
            sav_part_new$45,             /* Save New Part No.          */~
            sav_load$5,                  /* Save Load Number           */~
            model$3,                     /* Model Code                 */~
            cl$1,                        /* Color Code                 */~
            color$6,                     /* Color Description          */~
            dt_ref$8,                    /* Product Warranty Number    */~
            dt_seq$5,                    /* Production Sequence Number */~
            c_o$2,                       /* Cottage/Oriel              */~
            t_t$4,                       /* Twin/Triple                */~
            locks$3,                     /* Number of Locks            */~
            fin$3,                       /* With or Without Fin        */~
            dtl_load$5,                  /* Load Number                */~
            dtl_txt$4,                   /* Product Text Id            */~
            dtl_part$25,                 /* Part Number                */~
            dtl_sub_part$20,             /* New sub Part No            */~
            dtl_sub_info$20,             /* New Sub Info Fields        */~
            dtl_new_part$45,             /* New Part Number            */~ 
            prod$1,                      /* Product Code               */~
            xw$7,                        /* Converted Window Width     */~
            xh$7,                        /* Converted Window Height    */~
            tw$1,                        /* WIDTH CUT PARTS            */~
            th$1,                        /* HEIGHT CUT PARTS           */~
            col$(100%)25,                /* Cut Descriptions           */~
            eq$(100%)8,                  /* PRIMARY CROSS REF KEYS     */~
            ct$(100%)9,                  /* WIDTH AND HEIGHT CUTS      */~
            ct(100%),                    /* WIDTH AND HEIGHT CUTS-DECIM*/~
            sh$(100%)1,                  /* SASH TYPE ( W, H, N )      */~
            cr$(100%)10,                 /* RAW MATERIAL PARTS         */~
            cp$(100%)2,                  /* NUMBER OF PIECES TO CUT    */~
            cc$(100%)1,                  /* CUT PIECE (Y) OR (N)       */~
            c1$(100%)19,                 /* Cut Descriptions for Report*/~
            c2$(100%)9,                  /* Cuts Associated            */~
            item_descr$32                /* HNYINPUT Item Descriptio   */


        dim clmr$3,                      /*          CLMR              */~
            hg$2,                        /*          HINGE             */~
            sz$100,                      /*          Size              */~
            calc$9                       /*          fraction size     */


        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCCUTWK ! Work File                                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #2,  "AWDSLOW",                                      ~
                        varc,     indexed,  recsize =   256,             ~
                        keypos =    1, keylen =  34  

           sz$ = " 1/16 1/8  3/16 1/4  5/16 3/8  7/16 1/2  9/16 5/8 11/16~
        ~ 3/4 13/16 7/8 15/16     "              

            call "EXTRACT" addr("ID", userid$)
            scr_dte1$ = scr_dte$
            call "DATUNFMT" (scr_dte1$)  

            if spec% = 11% then                                           ~
                   scr_msg$ = "Low Volume Vinyl Pick Sheet " & scr_dept$        
            if spec% = 11% then                                           ~
                   call "SHOSTAT" ("Slow Moving Items " & scr_dept$  )

            mode% = 1%   : gosub open_work
            mode% = 3%   : gosub open_work

            pageno% = 0% : lcnt% = 99%
            call "SETPRNT" ("SLOW", " ", 0%, 0%)
            select printer (134)
            init(" ") sav_cl$, wrk_rec$, sav_mod$, sav_part$, sav_load$
            wrk_key1$ = all(hex(00))

            read #1,key 1% > wrk_key1$, using L01630, wrk_key1$, wrk_rec$, ~
                                                    eod goto print_a_done
            goto L01640
        print_a_next
            read #1, using L01630 , wrk_key1$, wrk_rec$,                   ~
                                                    eod goto print_a_done
L01630:       FMT POS(6), CH(51), CH(200)
L01640:
            model$   = str(wrk_key1$,6%,3%)         /* Model       */
            cl$      = str(wrk_key1$,9%,1%)         /* Color       */

            dt_ref$= str(wrk_rec$,1%,8%)            /* REF NO      */
            dt_seq$= str(wrk_rec$,9%,5%)            /* SEQUENCE NO */
            c_o$   = str(wrk_rec$,14%,2%)           /* COTTAGE/ORIE*/
            t_t$   = str(wrk_rec$,16%,4%)           /* TWIN/TRIPLE */
            t_b$   = str(wrk_rec$,20%,3%)           /* TSO, BSO,FSO*/
            locks$ = str(wrk_rec$,23%,3%)           /* LOCKS       */
            fin$   = str(wrk_rec$,26%,3%)           /* WITH FIN    */
            dtl_load$ = str(wrk_rec$,29%,5%)        /* LOAD NUMBER */
            dtl_txt$  = str(wrk_rec$,34%,4%)        /* TEXT ID     */
            dtl_part$ = str(wrk_rec$,38%,25%)       /* PART NO.    */
            dt_dept$ = str(wrk_rec$,78%,3%)         /* Department  */
            clmr$ = str(dtl_part$,20,3)             /* CLMR        */
            hg$   = str(dtl_part$,9,2)              /* Hinge       */

            xw$ = str(wrk_rec$,63%,7%)              /* WIDHT       */
            xh$ = str(wrk_rec$,70%,7%)              /* HEIGHT      */

            dtl_sub_part$ = str(wrk_rec$,81%,20%)
            dtl_sub_info$ = str(wrk_rec$,101%,20%)
            str(dtl_new_part$,1%,25%)  = dtl_part$
            str(dtl_new_part$,26%,20%) = dtl_sub_part$

            if cl$ <> sav_cl$ then gosub lookup_color
            if sav_mod$ <> model$ then gosub lookup_model
            if len(dtl_part$) > 18 then goto L01890
               xw$, xh$ = "PART    "
               init(" ") c_o$, t_t$, t_b$, locks$, fin$

L01890:     total% = total% + 1%
            gosub detail_a
            goto print_a_next
        print_a_done
            slow_key$ = all(hex(00))
            init(" ") slow_rec$ 
            gosub print_slow
        goto exit_sub                              /* Finished         */


        detail_a                                       /* C1$(), C2$() */
             if xw$ = "PART    " then return
                gosub calc_cuts
                sav_part_new$ = dtl_new_part$
                
        return

        lookup_part                           /* Check HNYMASTR        */
            sav_part_new$ = dtl_new_part$
            init(" ") slow$, size$, item_descr$
            read #5,key = raw_part$,using L03150 , item_descr$, size$, slow$,~
                             eod goto L03170
L03150:        FMT POS(26), CH(32), POS(686), CH(20), POS(874), CH(01)
               p% = 0%
               p% = pos(size$ = "/")
               size$ = str(size$,1,p%-1%)

L03170: return

        lookup_model                                  /* Look Up Color */
            sav_mod$ = model$
            readkey$ = all(hex(00))
            readkey$ = "MODEL    " & sav_mod$
            call "DESCRIBE" (#3, readkey$, descr$, 0%, x% )
        return

        lookup_color                                  /* Look Up Color */
            sav_cl$ = cl$
            readkey$ = all(hex(00))
            readkey$ = "COLOR    " & cl$
            call "DESCRIBE" (#3, readkey$, descr$, 0%, x% )
            color$ = str(descr$,6%,6%)
            if x% = 0% then color$ = "N/A"
        return


        calc_cuts
          cw% = 0%  : ch% = 0%
          tw$ = "1" : th$ = "2"
          init(" ") c1$(), c2$(), prod$, sav_part$
          prod$ = str(dtl_part$,1%,1%)
          call "APCCUTLD" (prod$, cw%, ch%, tw$, th$, #3, e%)

          j% = 0% : x% = 1% : eq% = cw%

          mfg% = 0%
          if str(dtl_sub_part$,1,1) = "4" then mfg% = 3%
          call "APCCUTCC" (dtl_part$, 0%, 0%, 0%, /* (CUT001) */         ~
                      mfg%, cw%, ch%, eq$(), ct$(), cr$(),               ~
                      cp$(), cc$(), col$(), ct(), sh$(), tw$, th$,       ~
                                                         #7, #8, #3, e% )
          eq% = cw% + ch% : x% = 1%        /* Load Cuts for Width & Height*/
          for i% = x% to eq%
                 if cr$(i%) = " " then goto L04930
                 raw_part$ = cr$(i%)
                 if sav_part$ <> raw_part$ then gosub lookup_part
                 sav_part$ = raw_part$
                 if slow$ <> "1" then goto L04930
                 j% = j% + 1%                     /*  Defined for Part   */
                 gosub update_work
L04930:   next i%
        return

        update_work
REM           call "SHOSTAT" (" UPDATING WORK " )
           length = 0.00
           init(" ") slow_rec$
           slow_key$ = all(hex(00))

           str(slow_key$,1,6) = scr_dte1$
           str(slow_key$,7,3) = dt_dept$
           str(slow_key$,10,25) = raw_part$
REM           str(slow_key$,26,5) = dt_seq$
                read #2, hold, key = slow_key$, using slow_fmt3, length, ~
                                    eod goto no_slow
slow_fmt3:              FMT POS(35), PD(14,4)

                       delete #2

                length = length + ct(i%)

no_slow:
                put #2, using slow_fmt, scr_dte1$,      /* Prod Date    */~
                                        dt_dept$,       /* Prod Dept    */~
                                        raw_part$,      /* Raw Material */~
                                     /*   dt_seq$,        Prod Seq     */~ 
                                        length,         /* Cut Length   */~
                                        size$,          /* SIZE         */~
                                        item_descr$     /* Description  */

slow_fmt:            fmt ch(06), ch(03), ch(25), pd(14,4), ch(20), CH(32)

               write #2

        return

        print_slow
            call "SHOSTAT" ("PRINTING SLOW ITEMS.... " ) 
            gosub header
            length = 0.00
            init(" ") raw_part$, dt_seq$, item_descr$
             
            read #2, key > slow_key$, using slow_fmt1, slow_rec$,~
                                        eod goto slow_done

slow_fmt1:        fmt ch(256)
             goto first_slow
        print_slow_next
            read #2, using slow_fmt1, slow_rec$,~
                                        eod goto slow_done

first_slow
            scr_dte1$ = str(slow_rec$,1,6)
            dt_dept$  = str(slow_rec$,7,3)
            raw_part$ = str(slow_rec$,10,25)
REM            dt_seq$   = str(slow_rec$,26,5)
            item_descr$ = str(slow_rec$,63,32)
            call "DATEFMT" (scr_dte1$)

            get slow_rec$ using slow_fmt2, length
slow_fmt2:             FMT  POS(35), PD(14,4)

            if length > 0.00 then             ~
            length = length * 1.20   /* Add 20% for scrap, remakes */

            size$ = str(slow_rec$,43,20)
            convert size$ to size%, data goto bad_size

            size% = size% * 12%       /* convert ft to inches*/

bad_size

            size% = size%   

REM            length = length / size% + 2        /* Add two sticks */
            length = length / size% + 1        /* Only add one stick */


            print using L03790, scr_dte1$, dt_dept$, raw_part$, ~
                                 length, item_descr$
            print using L04435

              goto print_slow_next

        slow_done
        return


        con_fract                            /* Convert to Sixteenth's */
              calc = round( calc, 4 ) : calc$ = " "
              a% = int(calc) : b% = int((calc - a%) * 10000)
              if b% = 0% then goto L02270                /****************/
                 d% = int(b%/625)                      /* Conversion of*/
                 if mod(b%,625) <> 0 then d% = d% + 1% /* Decimals to  */
                 b% = d%                               /*  Sixteenth's */
                 if b% <> 16% then goto L02270           /****************/
                    a% = a% + 1% : b% = 0%          /* A% = Whole Part */
L02270:       convert a% to str(calc$,1%,3%), pic(###)
              if b% <> 0% then                                           ~
                              str(calc$,5%,5%) = str(sz$,(b%*5%) - 4%,5%)
        return


        open_work
            if mode% = 1% then mode$ = "OUTPT"
            if mode% = 2% then mode$ = "INPUT"
            if mode% = 3% then mode$ = "SHARE"

            call "WORKOPN2" (#2,mode$, 500%, f2%)
            if f2% <> 0% then goto L64580
        return
L64580:     call "SHOSTAT" ("ERROR - CANNOT OPEN (APCCUTWK)") : stop
        return
        delete_work
            call "FILEBGON" (#1)
        return

                                      
L03760: %! Prod Date  ! Dept  ! Part Number      ! Needed ! Description     ~
        ~                ! Issued !  UOM  ! Notes                        !


L03790: %! ########## !  ###  !  ############### !  ####  ! ################~
        ~############### !        !       !                              !




L03910: %!                                                                  ~
        ~                                                                !


L04370: %!                 Production Date: ##########                      ~
        ~                        Completion Date: ##########             !
                                                                                
L04400: %!                                          ########################~
        ~################                                    Page: ####  !

L04430: %! Material Handler : _____________________________________________ ~
        ~                                                                !

L04435: %! -----------------------------------------------------------------~
        ~--------------------------------------------------------------- !

        header
REM          if lcnt% <> 99% then print using L04340

            title$ = scr_msg$
            call "FMTTITLE" (title$, " ", 12%)

          pageno% = pageno% + 1%
          print page
REM          print using L04340
          print using L03910
          print using L04370, scr_dte$, prod_dte$
          print using L04400, scr_msg$, pageno%
          print using L04430
          print using L03910
          print using L03760
          lcnt% = 6%

        return


        exit_sub
            gosub delete_work
            call "SETPRNT" ("SLOW", " ", 0%, 1%)
            close printer

        end



