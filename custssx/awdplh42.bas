************************************************************************~
*                       NEW -( Special Shapes Bending Report )         *~
*                                                                      *~
*        AWDPLH42 - Special Bending Report for Special Shapes          *~
*                                                                      *~
*            Note - Old Program 'APCPLE42'                             *~
*                                                                      *~
* 05/18/04 ! (AWD001) New Special Shapes Bending Report       ! RHH    *~
*          !          Special code to check for 'TRPEYE" in   !        *~
*          !          the Line Item text      'bd_prt$'       !        *~
*          !          also check for Operable Shapes and Flag !        *~
* 06/28/04 ! (AWD002) Mod to Blank out Dimensions for         ! RHH    *~
*          !          Triple-Eyes.                            !        *~
* 07/22/04 ! (AWD003) Mod for new Grid Codes to Flag Triple   ! RHH    *~
*          !          Eyebrows.                               !        *~
* 04/02/08 ! (AWD004) mods for Trim Kit Casing                ! CMG    *~   
* 03/20/16 ! SR73216  Mod to flag Flange/Fin/fla.             ! PWW    *~
*04/13/2018! (CR1453) decrease line counts for current printers!RDB    *~
*          !                                                  !        *~
************************************************************************

        sub "AWDPLH42" (scr_dte$,        /* Production Date           */ ~
                        prod_dte$,       /* Completion Date           */ ~
                        scr_shift$,      /* Shift Selection           */ ~
                        scr_load$,       /* Screen Load Selection     */ ~
                        scr_dept$,       /* Screen Dept               */ ~
                        #63,             /* bcksubpt <SR73216>        */ ~
                        #1,              /* (AWDSPECB) Shapes Bending */ ~
                        #3,              /* (GENCODES) Table file     */ ~
                        #6 )             /* (TXTFILE ) Text File      */

        dim scr_dte$8, userid$3,         /* Production Date            */~
            scr_dte1$8,                  /* Production Unformatted     */~    
            prod_dte$8,                  /* Completion Date            */~
            scr_shift$2, sav_shift$2,    /* Shift Selection            */~
            scr_load$5, sav_load$5,      /* Load Selection             */~
            scr_dept$3,                  /* Screen dept                */~
            bd_rec$256,                  /* Bending Record             */~
            bd_key$36,                   /* Primary Key                */~
            bd_key1$23,                  /* Alt Key Link to (APCPLNDT) */~
            bd_seq$5,                    /* Production Seq Number      */~
            bd_tool$1, bd_prt$3,         /* Tool Set and Tool Set Print*/~
            bd_model$3,                  /* Model Code                 */~
            bd_shape$3,                  /* Shape Code                 */~
            bd_so$8,                     /* Sales Order                */~
            bd_cust$9,                   /* Customer Code              */~
            bd_base$10,                  /* Base Cut length            */~
            bd_head$10,                  /* Head Arch Radius           */~
            bd_lleg$10,                  /* Left Leg Cut               */~
            bd_rleg$10,                  /* Right Leg Cut              */~
            bd_h1$10,                    /* Overall Height             */~
            bd_w1$10,                    /* Overall Not Used           */~
            bd_a$6,                      /* Angle 'A'                  */~
            bd_b$6,                      /* Angle 'B'                  */~
            bd_c$6,                      /* Angle 'C'                  */~
            bd_d$6,                      /* Angle 'D'                  */~
            bd_text$4,                   /* Text Id                    */~
            bd_width1$10,                /* Sort width Value           */~
            bd_width$8,                  /* Actual Width Value         */~
            bd_height$8,                 /* Actual window Height       */~
            bd_color$6, bd_cl$1,         /* Color Code and Description */~
            bd_cross$2,                  /* Shape Cross Ref Code       */~
            bd_code$3,                   /* Shape Description          */~
            no_bend$1,                   /* Shape No Bend Flag         */~
            bd_spec$7,                   /* Special Triple, Operable   */~
            bd_part$25,                  /* MFG Part Number            */~                   
            readkey$50,                  /* Code Table Key             */~
            descr$32,                    /* Code Table Description     */~
            errormsg$78,                 /* Error Message              */~
            scr_msg$40                   /* Header Text                */

        dim sav_key1$11,                 /* Save Text Key              */~
            text_key$11,                 /* Text Key                   */~
            text$(2%)70,                 /* Text Description           */~
            text_desc$60, text_d$(2%)60, /* Text Description           */~
            textid$4,                    /* Text Id                    */~
            text_flag$1                  /* Text Flag                  */
            
        dim                                                              ~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4,            /* Additional Info Fields     */~
            dt_sub_part$20,              /* New Sub Part No.           */~
            dt_sub_info$20               /* New Sub Info Fields (9)+11 */~
                                         /* SR73216                    */


            call "EXTRACT" addr("ID", userid$)

            scr_msg$ = " Special Shapes Bending Report Dept(" & scr_dept$ & ") "

            call "SHOSTAT" ("Special Sahpes Bending Report")

            init(" ") readkey$, descr$, errormsg$

            pageno% = 0% : lcnt% = 99%
            call "SETPRNT" ("APCC", " ", 0%, 0%)
            select printer (134)
            init(" ") bd_rec$, bd_key$, bd_key1$

            date% = 0%
            call "DATEOK" (scr_dte$, date%, errormsg$ )
            scr_dte1$ = scr_dte$
            call "DATUNFMT" (scr_dte1$)

            bd_key$   = all(hex(00))
            bd_key1$  = all(hex(00))
            str(bd_key$,1%,6%) = str(scr_dte1$,1%,6%)

        print_a_next 
            read #1,key > bd_key$, using L01060 , bd_rec$,             ~
                                                  eod goto print_a_done
L01060:       FMT CH(256)

            bd_key$    = str(bd_rec$,1%,36%)        /* Load Primary Key */ 
            sav_shift$ = str(bd_rec$,54%,2%)        /* Load Shift Code  */
            sav_load$  = str(bd_rec$,195%,5%)       /* Load Load        */

                                                    /* Production Date  */
            if str(bd_rec$,1%,6%) <> str(scr_dte1$,1%,6%) then            ~
                                                        goto print_a_done
                                                    /* Check shift      */          
            if scr_shift$ = "AA" then goto L01070
               if scr_shift$ <> sav_shift$ then goto print_a_next

L01070:
            if str(scr_load$,1%,1%) = "N" then goto L01080
               if scr_load$ <> sav_load$ then goto print_a_next

L01080:
            bd_seq$  = str(bd_rec$,190%,5%)         /* Prod Seq       */
            bd_tool$ = str(bd_rec$,10%,1%)          /* Tool Set       */
                                                    /* Convert Tool Set */
            if str(bd_rec$,10%,1%) = "1" then bd_tool$ = "4"
            if str(bd_rec$,10%,1%) = "2" then bd_tool$ = "3"
            if str(bd_rec$,10%,1%) = "3" then bd_tool$ = "2"
            if str(bd_rec$,10%,1%) = "4" then bd_tool$ = "1"



            bd_model$= str(bd_rec$,11%,3%)          /* Model Code     */
            bd_shape$= str(bd_rec$,14%,3%)          /* Shape Code     */

            shape% = 0%
            convert bd_shape$ to shape%, data goto L01085
L01085:

            bd_cl$   = str(bd_rec$,168%,1%)         /* Color Code     */
            
            bd_part$ = str(bd_rec$,165%,25%)

            bd_cross$ = str(bd_rec$,35%,2%)         /* 'SHAPCROSS' Code */
 
            gosub lookup_cross_desc                 /* Shape Descript */

            bd_so$   = str(bd_rec$,46%,8%)          /* S.O.           */
            bd_cust$ = str(bd_rec$,37%,9%)          /* Customer       */
                                       
                                                    /* Cut Info.      */
            bd_base$ = str(bd_rec$,56%,10%)         /* Base Cut Length*/
                                                    /* Remove cut Length */
                                                    /* Adjustment     */

            bd_head$ = str(bd_rec$,66%,10%)         /* Head Arc Radius*/
            bd_lleg$ = str(bd_rec$,76%,10%)         /* Left Leg       */
            bd_rleg$ = str(bd_rec$,86%,10%)         /* Right Leg      */
            bd_h1$   = str(bd_rec$,96%,10%)         /* Overall Height */
            bd_w1$   = str(bd_rec$,106%,10%)        /* Overall Width  */

                                                    /* Angle Info.    */
            bd_a$    = str(bd_rec$,116%,6%)         /* Angle 'A'      */
            bd_b$    = str(bd_rec$,122%,6%)         /* Angle 'B'      */
            bd_c$    = str(bd_rec$,128%,6%)         /* Angle 'C'      */
            bd_d$    = str(bd_rec$,134%,6%)         /* Angle 'D'      */

            bd_text$ = str(bd_rec$,200%,4%)         /* Text Id        */

            bd_spec$ = str(bd_rec$,207,7)

                                                    /* Covert Width   */
                                                    /* Back to Actual */
            if bd_model$ = "B70" or bd_model$ = "B71" then ~
                 gosub calc_casing_width

            if bd_model$ <> "B70" and bd_model$ <> "B71" then ~
                 gosub calc_width

            gosub lookup_color

            a1, a2 = 0.0
            height = 0.0
            convert str(bd_rec$,181%,2%) to a1, data goto L01200
L01200:
            convert str(bd_rec$,183%,1%) to a2, data goto L01210
L01210:
            height = a1 + (a2/8.0)                /* Height Decimal Value */

            convert height to bd_height$, pic(###.####)

            total% = total% + 1%
 
            bd_prt$ = " " & bd_tool$ & " "   /* Tool Set Print            */

            gosub lookup_text                /* Check for Triple eyebrow */
                                             /* Flag Triple Eyebrow      */
            gosub triple_eye                 /* (AWD002)                 */
 
           
            if bd_model$ = "765" or bd_model$ = "766" or bd_model$ = "767" ~
                                               then bd_spec$ = "Operabl"

            if bd_model$ = "515" or bd_model$ = "516" or bd_model$ = "517" ~
                                               then bd_spec$ = "Operabl"


            if bd_model$ = "B50" or bd_model$ = "B51" or bd_model$ = "B52" ~
                                               then bd_spec$ = "Operabl"

            if bd_model$ = "B53" or bd_model$ = "B54" or bd_model$ = "B55" ~
                                               then bd_spec$ = "Operabl"
/*SR73216 + */
            so_inv$  = str(bd_rec$,140%,8%)           /* Sales Order    */
            item_no$ = str(bd_rec$,148%,2%)           /* Line Item No.  */
            gosub lookup_sub_part                    /* (PAR001)       */

            if str(dt_sub_part$,18%,1%) = "4" then str(bd_spec$,7%,1%) = "4"
            if str(dt_sub_part$,18%,1%) = "5" then str(bd_spec$,7%,1%) = "5"
            if str(dt_sub_part$,18%,1%) = "6" then str(bd_spec$,7%,1%) = "6"
            if str(dt_sub_part$,18%,1%) = "7" then str(bd_spec$,7%,1%) = "7"

            gosub detail_a
            goto print_a_next
        print_a_done
            print using L02590
            call "SETPRNT" ("APCC", " ", 0%, 1%)
              scr_msg$ = "Printing S. S. Shapes Bending Report"


              call "SHOSTAT" ("Printing S. S. Shapes Bending Report")
        return clear all
        goto exit_end


        calc_casing_width
            if str(bd_part$,7,2) = "00" then goto calc_width
            bd_width1 = 0.0
            bd_width1$ = str(bd_rec$,106%,10%)

            convert bd_width1$ to bd_width1, data goto L01100

L01100:     bd_width  = bd_width1

            convert bd_width to bd_width$, pic(###.####)


        return

        calc_width
            bd_width1 = 0.0
            bd_width1$ = str(bd_rec$,20%,10%)

            convert bd_width1$ to bd_width1, data goto L01110

L01110:     bd_width  = 1000.0000 - bd_width1

            convert bd_width to bd_width$, pic(###.####)


        return

        lookup_color                                  /* Look Up Color */
            readkey$ = all(hex(00))
            readkey$ = "COLOR    " & bd_cl$
            call "DESCRIBE" (#3, readkey$, descr$, 0%, x% )
            bd_color$ = str(descr$,6%,6%)
            if x% = 0% then bd_color$ = "N/A"
        return

        lookup_cross_desc                             /* Shape Descr  */
            no_bend$ = " "
            readkey$ = all(hex(00))
            readkey$ = "SHAPCODES" & bd_cross$
            call "DESCRIBE" (#3, readkey$, descr$, 0%, x% )
            bd_code$ = str(descr$,1%,3%)
            no_bend$ = str(descr$,5%,1%)              /* No Bend Flag */
            if x% = 0% then bd_code$ = "N/A"
        return


        deffn'099(textid$)
            txt% = 0%
            if textid$ = hex(00000000) or textid$ = hex(ffffffff)        ~
                                           or textid$ = " " then return
            txt% = 1%
        return

        lookup_text                           /* Look Up Text Id       */
            p%, q% = 0%

            init(" ") text_desc$, textid$, text_key$, sav_key1$, text$(),~
                      text_d$()
            textid$ = bd_text$
            text_flag$ = "N"
            gosub'099(textid$)
            if txt% = 0% then return
            text_key$ = all(hex(00))
            str(text_key$,1%,1%) = "M"
            str(text_key$,2%,3%) = "   "
            str(text_key$,5%,4%) = textid$
            str(text_key$,9%,1%) = "1"
            sav_key1$ = text_key$
            read #6,key > text_key$, eod goto L01990
               get #6, using L01920 , text_key$,text$()
L01920:          FMT CH(11), POS(64), 2*CH(70)
            if str(sav_key1$,1%,9%) <> str(text_key$,1%,9%) then return
            if text$(1%) <> " " then text_desc$ = str(text$(1%),1%,60%)  ~
                                else text_desc$ = str(text$(2%),1%,60%)
            if text_desc$ <> " " then text_flag$ = "Y"
            text_d$(1%) = str(text$(1%),1%,60%)
            text_d$(2%) = str(text$(2%),1%,60%)

            yy% = 1%                   /* Check Production Text 1st */
            gosub check_text
            p% = x%
          
            yy% = 2%                   /* Check Glass Text 2nd      */
            gosub check_text
            q% = x%

L01990: return

L02000: %!Seq.!T-S!Mod!Shape!Color !Cut Length!Wd Width!Wd Hight!Arc Radius!~
        ~Left Leg  !Right Leg !Ang  A!Ang  B!Ang  C!Ang  D!SO Order!Special!

L02010: %!####!###!###! ### !######!##########!########!########!##########!~
        ~##########!##########!######!######!######!######!########!#######!

L02020: %!----!---!---!-----!------!----------!--------!--------!----------!~
        ~----------!----------!------!------!------!------!--------!-------!

L02130: %!TXT:############################################################  ~
        ~                                                 !        !       !

        detail_a                                     
            if lcnt% > 55% then gosub header           /* CR1453 */
 
             print using L02020
             print using L02010, str(bd_seq$,2%,4%), bd_prt$, bd_model$,   ~
                                 bd_code$, bd_color$, bd_base$, bd_width$, ~
                                 bd_height$, bd_head$, bd_lleg$, bd_rleg$, ~
                                 bd_a$, bd_b$, bd_c$, bd_d$, bd_so$, bd_spec$

             lcnt% = lcnt% + 2%
             gosub find_text

        return   

   
        find_text

        return
             gosub lookup_text
             if text_flag$ <> "Y" then return
                if text_d$(1%) <> " " then                               ~
                   print using L02130, text_d$(1%)
                if text_d$(2%) <> " " then                               ~
                   print using L02130, text_d$(2%)
                lcnt% = lcnt% + 1%
                if text_d$(1%) <> " " and text_d$(2%) <> " " then        ~
                   lcnt% = lcnt% + 1%           /* Add Additional Line */
        return

L02590: %+------------------------------------------------------------------~
        ~------------------------------------------------------------------+

L02620: %! Production Date: ##########                                      ~
        ~                                      Completion Date: ########## !

L02650: %!                                                    ##############~
        ~##########################                             Page: #### !

        header
          if lcnt% <> 99% then print using L02590
          pageno% = pageno% + 1%
          print page
          print using L02590
          print using L02620, scr_dte$, prod_dte$
          print using L02650, scr_msg$, pageno%
          print using L02590
                                              /* Print Column Heading */
          print using L02000

          lcnt% = 5%
        return

        exit_end
        end

        check_text
          x% = 0%
          jj% = 1%
          t1% = len(text_d$(yy%))
          for kk% = 1% to t1%
             if str(text_d$(yy%),jj%,6%) = "TRPEYE" then goto check_text_fnd
            
             jj% = jj% + 1%
             if jj% > (t1% - 5%) then return

          next kk%
        return
  
        check_text_fnd
          x% = 1%
        return

                                                  /* (AWD002)        */
        triple_eye                                /* (AWD003)        */ 
            if str(bd_cross$,1%,1%) = "T" then goto triple_eye_1
            if str(bd_cross$,1%,1%) = "U" then goto triple_eye_1

            if p% = 0% and q% = 0% then return

        triple_eye_1

                                                 /* (AWD003)         */
               bd_prt$ = "@" & bd_tool$ & "@"

               bd_spec$ = "Triple"
                                                  /* Blank Dimensions */

            bd_base$ = "          "               /* Cut Length     */
            bd_head$ = "          "               /* Head Arc Radius*/
            bd_lleg$ = "          "               /* Left Leg       */
            bd_rleg$ = "          "               /* Right Leg      */
            bd_h1$   = "          "               /* Overall Height */
            bd_w1$   = "          "               /* Overall Width  */

                                                  /* Angle Info.    */
            bd_a$    = "      "                   /* Angle 'A'      */
            bd_b$    = "      "                   /* Angle 'B'      */
            bd_c$    = "      "                   /* Angle 'C'      */
            bd_d$    = "      "                   /* Angle 'D'      */

        return
                                                  /* (AWD002)       */

/*SR73216 + */
        lookup_sub_part
            init(" ") bcksubpt_rec$, flds$(), info_flds$(),           ~
                      dt_sub_part$, dt_sub_info$
            dim1es, dim2es, dim3es = 0.00       /* (AWD048)          */
            flag$ = "0"                  /* Sales Order Info         */
            pgm$  = "1"
            err1% = 0%

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
                          #63,           /* BCKSUBPT File              */~
                          err1%)         /* Error Code                 */

/* (AWD048) */          
            get bcksubpt_rec$ using dimFMT, dim1es, dim2es
dimFMT:               FMT POS(153), PD(14,4), PD(14,4)
             
            if err1% = 0% then goto no_subpart_error             
               str(bcksubpt_rec$,48%,20%) = "00000000000000000000"
               str(bcksubpt_rec$,132%,20%) = "00000000000000000000"
               
no_subpart_error:
               dt_sub_part$ = str(bcksubpt_rec$,48%,20%)

               dt_sub_info$ = str(bcksubpt_rec$,132%,20%)

               if err1% = 0% then return

            return

/*SR73216 - */


