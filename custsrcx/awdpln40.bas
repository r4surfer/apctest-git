        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN38                             *~
            *  Creation Date     - 06/27/08                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New program to create daily history  *~
            *                      data to feed oracle warehouse.       *~
            *                      Data collected from BCKMASTR         *~
            *                                          BCKLINES         *~
            *                                          ARIMASTR         *~
            *                                          ARILINES         *~
            *                                          APCPLNAD         *~
            *                                          ORAINV           *~
            *                      Populates files     DAYHIST          *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/30/08 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            *          !                                          !     *~
            *************************************************************


        dim                              /* (Program) - Variables      */~
            filename$8,                  /* Used By EWDOPEN            */~
            cnt$28,                      /* Screen Display             */~
            hdr$45, msg$(3%)79,          /* Askuser - Var's            */~
            date$8,                      /* REPORT TITLE               */~
            userid$3                     /* Current User Id            */

        dim                                                              ~  
            readkey$100,                 /* Readkey                    */~
            readkey1$100,                /* Generic Readkey 1          */~
            arimastr_key$17,             /* ARIMASTR READKEY           */~
            fields$(500%)256,            /* Generic Fields             */~
            num_fields(500%)             /* Generic Fields             */ 
            

        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(15%)20                 /* Text from file opening     */


        dim                                                              ~
            cuscode$9,                   /* Customer Code              */~
            invoice$8,                   /* Invoice Number             */~
            po$16,                       /* Purchase Order             */~
            so_number$8,                 /* Sales Order Number         */~
            salesman$4,                  /* Saleman Number             */~
            region$4,                    /* Region Code                */~
            ship_date$10,                /* Ship Date                  */~
            order_date$10,               /* Order Date                 */~
            due_date$10,                 /* Due Date                   */~
            post_date$,                  /* Post Date                  */~
            comp_date$,                  /* Compare Date               */~
            quote$10,                    /* Quote Number               */~ 
            job$16,                      /* Job Number                 */~
            hdisc$14,                    /* Header Disc Percent        */~
            ldisc$14,                    /* Line Disc Percent          */~
            load$5,                      /* Load Number                */~
            status$2,                    /* Status Code                */~
            check$8,                     /* Check Number               */~
            part_number$25,              /* Part Number                */~
            catcode$4,                   /* Category Code              */~
            sub_part$20,                 /* Sub Part                   */~
            info_part$20,                /* info part                  */~
            oqty$14,                     /* order qty                  */~
            sqty$14,                     /* ship/invoice qty           */~
            uprice$14,                   /* order unit price           */~
            eprice$14,                   /* Extended Price             */~
            orabol_key$32,               /* ORABOL readkey             */~
            orainv_key$18,               /* ORAINV readkey             */~
            flag$1,                      /* Calling Program Flag       */~
            pgm$1,                       /* Calling Program BCKUPDTE?? */~
            so_inv$8,                    /* Sales Order or Invoice     */~
            item_no$3,                   /* Item Number                */~
            bcksubpt_rec$256,            /* BCKSUBPT Record            */~
            flds$(35%)4,                 /* Part Number Fields         */~
            info_flds$(35%)4             /* Additional Info Fields     */






        REM *************************************************************~
            *                     S E L E C T                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #2,  "BCKMASTR",                                      ~
                        varc,     indexed,  recsize = 1000,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =   26, keylen =  16, dup

            select #3,  "BCKLINES",                                      ~
                        varc,     indexed,  recsize =  300,              ~
                        keypos =   10, keylen =  19


            select #6,  "ARIMASTR",                                      ~
                        varc,     indexed,  recsize = 2000,              ~
                        keypos =    1, keylen =  16,                     ~
                        alt key  1, keypos =   18, keylen =  12, dup,    ~
                            key  2, keypos =   31, keylen =  25, dup,    ~
                            key  3, keypos =   17, keylen =  39, dup

    
            select #12, "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos = 1,    keylen = 51,                      ~
                        alt key 1, keypos = 27, keylen = 25,             ~
                            key 2, keypos = 70, keylen =  8, dup,        ~
                            key 3, keypos = 78, keylen =  8, dup,        ~
                            key 4, keypos = 52, keylen =  8,             ~
                            key 5, keypos = 36, keylen = 16, dup


            select #13, "ORABOL",                                        ~
                        varc,     indexed,  recsize = 512,               ~
                        keypos =    1, keylen =  32,                     ~
                        alt key  1, keypos =   13, keylen =  20, dup,    ~
                            key  2, keypos =  186, keylen =   8, dup,    ~
                            key  3, keypos =   22, keylen =  11   


            select #14, "ORAINV",                                        ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    7, keylen =  18,                     ~
                       alt key   1, keypos =    1, keylen =  24     



            select #17, "BCKSUBPT",                                      ~
                       varc,      indexed,  recsize = 256,               ~
                       keypos  =    1, keylen =  11,                     ~ 
                       alt key   1, keypos =   12, keylen =  11, dup,    ~
                           key   2, keypos =   23, keylen =  45, dup  


            filename$ = "BCKMASTR" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BCKLINES" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "ARIMASTR" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNOR" : call "EWDOPEN" (#12, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "ORABOL" : call "EWDOPEN" (#13, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "ORAINV" : call "EWDOPEN" (#14, filename$, err%)
            if err% <> 0% then gosub open_error


            filename$ = "BCKSUBPT" : call "EWDOPEN" (#17, filename$, err%)
            if err% <> 0% then gosub open_error


            
            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************


            call "EXTRACT" addr("ID", userid$)
            date$ = date

            call "DATEFMT" (date$)
            mat num_fields     = zer

REM 0% = extract entire file and 1% = only new daily data
            extract% = 0%

            gosub initialize_variables
            if extract% = 0% then gosub files_analysis
            if extract% = 1% then gosub files_analysis_inv

              goto exit_program



        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") readkey$, fields$(), filename$, cnt$, hdr$, msg$(),~
                      date$, readkey1$

            init(" ") cuscode$, invoice$, po$, so_number$, salesman$,    ~
              region$, ship_date$, order_date$, due_date$,               ~
              quote$, job$, hdisc$, load$, check$,                       ~
              part_number$, catcode$, sub_part$, info_part$, oqty$,      ~
              sqty$, uprice$, eprice$,  ~
              ldisc$, orabol_key$


        return

        REM *************************************************************~
            *************************************************************

        
        files_analysis

            init(" ") readkey$, fields$()
            readkey$ = all(hex(00))
REM            str(readkey$,1,9)  = "LO0537"
REM            str(readkey$,10,8) = "02298688"

            gosub create_mast

        return

        create_mast
             cnt% = 0%


             gosub read_mast
                   goto L34970
        create_mast_nxt
             gosub read_mast_nxt
             if done% = 1% then goto mast_done
L34970:
              goto create_mast_nxt
        return
        mast_done
        return


        read_mast
            done% = 0%
            mat num_fields = zer
        read_mast_nxt
            hdisc = 0.00
            quote% = 0% 

            read #2, key > readkey$, using mastr_fmt, readkey$,  ~
                                     eod goto read_mast_done 
        
mastr_fmt:          FMT CH(25)

                cnt% = cnt% + 1%



            if mod(cnt%,50%) <> 0% then goto L34990
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;



L34990:

            gosub get_apcplnor
             if status$ < "18" or status$ >= "90" then goto read_mast_nxt 


                get #2, using L35000, cuscode$, so_number$, po$, salesman$,~
                                      region$, quote$, job$, order_date$,  ~
                                      due_date$, hdisc


L35000:           FMT CH(09), CH(08), POS(26), CH(16), POS(580), CH(04),    ~
                      POS(595), CH(04), CH(10), POS(619), CH(16), POS(806), ~
                      CH(06), POS(818), CH(06), POS(859), PD(14,4)


REM            call "DATFMTC" (order_date$, date%, order_date$)            
REM            call "DATFMTC" (due_date$, date%, due_date$)            
REM            call "DATFMTC" (ship_date$, date%, ship_date$)            


            convert str(quote$,2,9) to quote%, data goto bad_quote

bad_quote:
            convert quote% to quote$,pic(#########0)


            convert hdisc to hdisc$, pic(-00000000.0000)



            gosub get_bcklines

              goto read_mast_nxt
        read_mast_done
             done% = 1%
        return



        get_apcplnor
           init(" ") readkey1$
           readkey1$ = str(readkey$,10,8)

           read #12, key 4% = readkey1$, eod goto apcplnor_done

                get #12, using L35150, status$, load$

L35150:          FMT POS(60), CH(02), POS(94), CH(05)

        apcplnor_done
        return


        get_bcklines
           init(" ") readkey1$ 
           str(readkey1$, 1, 16) = so_number$

        read_lines_nxt
           read #3, key > readkey1$, using lines_fmt, readkey1$, ~
                                       eod goto bcklines_done

lines_fmt:         FMT POS(10), CH(19)

           if str(readkey1$,1,8) <> so_number$ then goto bcklines_done

           get #3, using L35400, part_number$, catcode$, oqty, sqty, ~
                       uprice, ldisc, invoice$

L35400:       FMT POS(32), CH(25), POS(89), CH(04), POS(93), PD(14,4), ~
                  PD(14,4), POS(165), PD(14,4), PD(14,4), POS(247), CH(08)

              convert str(readkey1$,17,3) to item_no%, data goto bad_item

bad_item:

              if item_no% = 1% then gosub get_ship_date

              convert oqty to oqty$, pic(-000000000000) 

              convert sqty to sqty$, pic(-000000000000) 

              convert uprice to uprice$, pic(-00000000.0000)

              convert ldisc to ldisc$, pic(-00000000.0000)

              eprice = round(oqty * uprice, 2)

              dsc_amt   = 0 - round(eprice * ldisc * .01, 2)
              eprice = round(eprice + dsc_amt, 2)

              dsc_amt   = 0 - round(eprice * hdisc * .01, 2)
              eprice = round(eprice + dsc_amt, 2)

              convert eprice to eprice$, pic(-00000000.0000)

              so_inv$  = so_number$
              item_no$ = str(readkey1$,17,3)
              gosub lookup_subpart

              gosub write_orabol
                goto read_lines_nxt

        bcklines_done
        return


        get_ship_date
            init(" ") arimastr_key$
            arimastr_key$ = str(cuscode$,1,9) & invoice$

            read #6, key = arimastr_key$, using ship_fmt, ship_date$,  ~
                     eod goto ship_date_done

ship_fmt:         FMT POS(413), CH(06)

         ship_date_done
        return


        lookup_subpart
            flag$ = "0"                  /* Sales Order Info         */
            pgm$  = "1" 


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
                          #17,           /* BCKSUBPT File              */~
                          err1%)         /* Error Code                 */

           
           if err1% <> 0% then str(bcksubpt_rec$,48%,20%) = "00000000000000000000"
           if err1% <> 0% then str(bcksubpt_rec$,132%,20%) = "00000000000000000000"

           sub_part$ = str(bcksubpt_rec$,48%,20%)

           info_part$ = str(bcksubpt_rec$,132%,20%)


        return


        write_orabol
          init(" ") orabol_key$
          str(orabol_key$, 1, 8) = so_number$
          str(orabol_key$,9,3)   = item_no$


          read #13, hold, key 3% = orabol_key$, eod goto put_orabol

                 return
put_orabol:

            put #13, using L35800,  "S", date, load$, cuscode$, so_number$, ~
                         item_no$, order_date$,  ship_date$, quote$,        ~
                         part_number$, sub_part$, info_part$, oqty, sqty,   ~
                         hdisc, ldisc, uprice, eprice, catcode$, region$,   ~
                         salesman$, due_date$, invoice$, job$, po$ 



                write #13

        return
L35800: FMT                 /* FILE: ORABOL                            */~ 
            CH(1),          /* Transaction Flag 'S'end                 */~
            CH(6),          /* System Date                             */~
            CH(5),          /* Load Number                             */~
            CH(9),          /* Customer Code                           */~
            CH(8),          /* Sales Order                             */~
            CH(3),          /* Sequence Number                         */~
            CH(6),          /* Order Date                              */~
            CH(6),          /* Ship Date                               */~
            CH(10),         /* Quote Number                            */~
            CH(25),         /* Part Number                             */~
            CH(20),         /* Subpart Number                          */~
            CH(20),         /* Infopart Number                         */~
            PD(14,4),       /* Order Qty                               */~
            PD(14,4),       /* Ship Qty                                */~
            PD(14,4),       /* Head Disc Percent                       */~
            PD(14,4),       /* Line Disc Percent                       */~
            PD(14,4),       /* Unit Price                              */~
            PD(14,4),       /* Extended Price                          */~
            CH(4),          /* Category Code                           */~
            CH(4),          /* Region Code                             */~
            CH(4),          /* Salesman Code                           */~
            CH(6),          /* Due Date                                */~
            CH(8),          /* Invoice Number                          */~
            CH(16),         /* Job                                     */~ 
            CH(16)          /* PO                                      */


        
        files_analysis_inv

            init(" ") readkey$, fields$()
            readkey$ = all(hex(00))
REM            str(readkey$,1,9)  = "LO0537"
REM            str(readkey$,10,8) = "02298688"

            gosub create_mast_inv

        return

        create_mast_inv
             cnt% = 0%


             gosub read_mast_inv

        return


        read_mast_inv
            done% = 0%
            mat num_fields = zer
            net_amt = 0.00
            init(" ") comp_date$
            comp_date$ = "01/01/2008"
            call "DATUFMTC" (comp_date$)
        read_mast_nxt_inv
            hdisc = 0.00
            quote% = 0% 

            read #6, key > readkey$, using mastr_fmt1, readkey$,  ~
                                     eod goto read_mast_done_inv
        
mastr_fmt1:          FMT CH(17)

                cnt% = cnt% + 1%

 

            if mod(cnt%,50%) <> 0% then goto L34995
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;



L34995:
             get #6, using arimastr_fmt1, post_date$

arimastr_fmt1:   FMT POS(533), CH(06)

                  if str(post_date$,1,6) >= str(comp_date$,1,6) then ~
                                           goto read_mast_nxt_inv

                  gosub write_orainv
                  goto read_mast_nxt_inv
        read_mast_done_inv
        return


        write_orainv
           init(" ") orainv_key$
           orainv_key$ = "T" & str(readkey$,1,17)

           read #14, key = orainv_key$, eod goto put_orainv

             return
put_orainv:

            put #14, using orainv_fmt, date, "T", str(readkey$,1,9), ~
                    str(readkey$,10,8), post_date$, net_amt, date

            write #14
        return

orainv_fmt:   FMT CH(06), CH(01), CH(09), CH(08), CH(06), PD(14,4), CH(06)


        open_error
           comp% = 2%
           hdr$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = "(Open Error) - File = " & filename$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return





        exit_program
            end




