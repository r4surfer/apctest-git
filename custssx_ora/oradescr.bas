        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - ORADESCR                             *~
            *  Creation Date     - 04/15/05                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie Gregory                     *~
            *                                                           *~
            *  Description       - Subroutine Used to Create WW Descr   *~
            *                      file for documents to sent to cust   *~
            *                                                           *~
            *        Note        - Called from (BCKFASTR)               *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            * Subroutine - Called by BCKFASTR                           *~
            *-----------------------------------------------------------*~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/15/05 ! Original                                 ! CMG *~
            * 10/20/14 ! AWD001 Treat screen flag "7" like 4,5 & 6! PWW *~
            *************************************************************


        sub "ORADESCR" (so$,             /* S.O. No.                   */~
                        userid$,         /* User Who Entered Order     */~
                        error%,          /* Error Flag from File Open  */~
                        #1,              /* File ORADESC               */~
                        #2,              /* BCKLINES                   */~
                        #3  )            /* GENCODES                   */

        dim                              /*                            */~
            error$256,                   /* Error String               */~
            field$256,                   /* Query Return Field         */~
            stmt1$250,                   /* First Query String         */~
            stmt2$250,                   /* Second Query String        */~
            fields$(4%)100               /* String of Oracle Info      */
 
        dim bcklnes_key$19,              /* BCKLINES Readkey           */~
            bcklnes_rec$(3%)100,         /* BCKLINES REC               */~
            ora_key$14,                  /* ORAKEY                     */~
            ora_rec$(4%)256,             /* ORADESC Rec 1024           */~
            item_number$3,               /* Item Number within Line Num*/~
            item_num_ww$3,               /* Window Wizard Item Number  */~
            display$3,                   /* WW display line number     */~
            window$1,                    /* Window Flag                */~
            part$1,                      /* Part Flag                  */~
            screen$1,                    /* Screen Flag                */~
            so$8,                        /* Sales Order BCKLINES       */~
            lne$3,                       /* Sales Order Lne BCKLINES   */~
            part_num$25,                 /* Part Number                */~
            config_lne$3,                /* Config Line BCKLINES       */~
            readkey$24                   /* GENCODES Readkey           */





            error% = 0%
            gosub check_so_number
            gosub oracle_flush
            gosub call_stored_proc
REM            if oci_err% <> 0% then goto ora_desc_done
            item_num_ww% = 0%

            gosub select_data
fetch_next:           
                gosub fetch_data
                   if oci_err% > 0% and oci_err% < 100% then ora_desc_done
                   if oci_err% = 100% then ora_desc_done
                   goto fetch_next
            








        call_stored_proc
            gosub oracle_flush
            init(" ") stmt1$, stmt2$
            str(stmt1$,1%,32%)    = "CALL MSSQL.RPT_DISPLAY('" & so$ 
            str(stmt1$,33%,10%)   = "', '" & userid$ & "')"
            gosub oracle_exec 

            return
            if oci_err% = 0% then return
               call "ERROR" (error$)
               call "SHOSTAT" ("ERROR ERROR RETURN VALUE --> " & error$)
               stop      
        return
 
        select_data
            init(" ") stmt1$
            str(stmt1$,1%,40%)   = "SELECT * FROM MSSQL.DISPLAY WHERE SALESO" 
            str(stmt1$,41%,32%)  = "RDER = '"&str(so$,1%,8%)&"' AND USERID = '" 
            str(stmt1$,73%,34%)  =  userid$ & "' ORDER BY DISPLAYORDER ASC, LI"
            str(stmt1$,107%,40%) = "NENUMBER ASC, CONFIG DESC, UNITID ASC"

            gosub oracle_flush
            gosub oracle_query                             
        return

        fetch_data
            gosub oracle_fetch
            if oci_err% > 0% and oci_err% < 100% then return
            if oci_err% = 100% then return
               gosub get_rec
               gosub build_rec
               gosub write_rec
        return


        get_rec
                                              /* Total 18 columns */
            init(" ") fields$()
            pos% = 1%
            field_num% = 1%
            gosub oracle_getfield
REM         ora_seqnr$ = field$
            for field_num% = 1%  to 7%  
                gosub oracle_getfield
                if field_num% = 4% then field_len% = 10%   /* force length */
                if field_num% = 5% then field_len% = 3% /* on number fields*/
                if field_num% = 7% then field_len% = 1%
                str(fields$(),pos%, field_len%) = field$
                pos% = pos% + field_len% 
            next field_num%

            for field_num% = 8% to 9%
                gosub oracle_getfield
                gosub convert_number
                pos% = pos% + 8%
            next field_num%

            for field_num% = 10%  to 11% 
                gosub oracle_getfield
                if field_num% = 11% then field_len% = 25% /* only use 25 of 50 */
                str(fields$(),pos%, field_len%) = field$
                pos% = pos% + field_len%
            next field_num%


            for field_num% = 12% to 15%
                gosub oracle_getfield
                gosub convert_number
                pos% = pos% + 8%
            next field_num%


            field_num% = 16%
            gosub oracle_getfield
            gosub convert_binary 
            pos% = pos% + 4%

            for field_num% = 17% to 18%
                gosub oracle_getfield
                if field_num% = 17% then field_len% = 10%  /* force length */
                if field_num% = 18% then field_len% = 1% /* on number fields*/
                str(fields$(),pos%, field_len%) = field$
                pos% = pos% + field_len%
            next field_num%



            /* Field 1 - Sales Order CH(08)   POS  1  - 8  */
            /* Field 2 - User ID     CH(03)   POS  9  - 11 */
            /* Field 3 - OrderID     CH(10)   POS  12 - 21 */
            /* Field 4 - Item ID     CH(10)   POS  22 - 31 */
            /* Field 5 - Display Ord CH(03)   POS  32 - 34 */
            /* Field 6 - Line Number CH(03)   POS  35 - 37 */
            /* Field 7 - Config?     CH(01)   POS  38 - 38 */
            /* Field 8 - Qty         PD(14,4) POS  39 - 46 */
            /* Field 9 - Sub Qty     PD(14,4) POS  47 - 54 */
            /* Field 10- Description CH(250)  POS  55 - 306*/
            /* Field 11- Size        CH(25)   POS 305 - 329*/
            /* Field 12- Cat Unit    PD(14,4) POS 330 - 337*/
            /* Field 13- Cat Ext     PD(14,4) POS 338 - 347*/
            /* Field 14- Dist Unit   PD(14,4) POS 346 - 353*/
            /* Field 15- Dist Ext    PD(14,4) POS 354 - 361*/
            /* Field 16- TextID      BI(04)   POS 362 - 365*/
            /* Field 17- UnitID      CH(10)   POS 366 - 375*/
            /* Field 18- ShipSep     CH(01)   POS 376 - 376*/ 
        return

        convert_number
            amount = 0.00
            amount1 = 0.00
            convert str(field$,1%,len(field$)) to amount, data goto bad_amount
 
        bad_amount
            put str(fields$(),pos%,8%) using L20000, amount
L20000:         FMT PD(15,4)

            convert str(fields$(),pos%,8%) to amount1, data goto L20001
L20001:

            get str(fields$()) using L20002, amount1
L20002:             FMT POS(39), PD(14,4)


REM         cmg$ = str(fields$(),39%,8%)

        return


        convert_binary
            amount% = 0%
            convert str(field$,1%,len(field$)) to amount%, data goto bad_binary
 
        bad_binary
           str(fields$(),pos%,4%) = BIN(amount%,4)

        return


        build_rec
           init(" ") ora_rec$(), item_number$, item_num_ww$, window$,     ~
                     part_num$, screen$, bcklnes_key$, bcklnes_rec$(),    ~
                     part$, window$
           display% = 0%
           item_number% = 0%                       /* Caelus Item Number */
           qty, sub_qty, unit, ext = 0.00

REM  format display number
           convert str(fields$(),35%,3%) to display%, data goto build_done

           convert display% to str(fields$(),35%,3%), pic(###)

           convert str(fields$(),32%,3%) to display%, data goto build_done

           convert display% to str(fields$(),32%,3%), pic(###)

REM  lookup bcklines key if not first part of configuration
           if str(fields$(),38%,1%) =  "0" then                         ~
                                        gosub lookup_bcklines

REM  item number and item number ww counters
REM  reset if new Window Wizard Line number
           if display$ <> str(fields$(),32%,3%) then item_num_ww% = 0%
           if str(fields$(),38%,1%) = "0" then                ~
              item_num_ww% = item_num_ww% + 1%
REM  if not config then item_number% = 1% else 0%
           if str(fields$(),38%,1%) = "0" then                ~
                         item_number% = item_number% + 1%   /* Not Config */
           convert item_number% to item_number$, pic(###)

           convert item_num_ww% to item_num_ww$, pic(###)

REM  Set Window, Screen, Part Flag          
           if str(fields$(),376%,1%) <> "0" then screen$ = "1"
           if len(part_num$) < 19 and str(fields$(),38%,1%)  = "0" ~
                                                   then part$ = "1"
REM  Set size equal to blank if less than 19 digit part
           if len(part_num$) < 19 and str(fields$(),38%,1%)  = "0" ~
                                        then str(fields$(),305%,25%) = " " 

           if str(part_num$,11%,1%) = "4" then part$ = "1"
           if str(part_num$,11%,1%) = "5" then part$ = "1"
           if str(part_num$,11%,1%) = "6" then part$ = "1"
/*AWD001*/ if str(part_num$,11%,1%) = "7" then part$ = "1"
           gosub check_mull
           gosub check_screenonly
           if (screen$ <> "1" and part$ <> "1") and            ~
                    str(fields$(),38%,1%) = "0" then window$ = "1"

REM  Set Quantities

           get str(fields$()) using L24000, qty, sub_qty, unit, ext
L24000:           FMT POS(39), PD(14,4), PD(14,4),                   ~
                                      POS(330), PD(14,4), PD(14,4)


REM           convert str(fields$(),39%,8%) to qty, data goto bad_qty

REM           convert str(fields$(),47%,8%) to sub_qty, data goto bad_qty

REM bad_qty


REM           convert str(fields$(),330%,8%) to unit, data goto bad_prc

REM           convert str(fields$(),338%,8%) to ext, data goto bad_prc


REM bad_prc
REM  Build Record
           str(ora_rec$(),1%,8%)  = str(fields$(),1%,8%)  /* Sales Order */
           str(ora_rec$(),9%,3%)  = str(fields$(),35%,3%) /* Line Number */
           str(ora_rec$(),12%,3%) = item_number$          /* Caelus Count*/
           str(ora_rec$(),15%,8%) = str(fields$(),1%,8%)  /* Sales Order */
           str(ora_rec$(),23%,3%) = str(fields$(),32%,3%) /* Display Ord */
           str(ora_rec$(),26%,3%) = item_num_ww$          /* WW Lne Num  */
           str(ora_rec$(),29%,250%) = str(fields$(),55%,250%) /* Desc    */
           str(ora_rec$(),279%,1%) = window$              /* Window Flag */
           str(ora_rec$(),280%,1%) = screen$              /* Screen Flag */
           str(ora_rec$(),281%,1%) = part$                /* Part Flag   */
           str(ora_rec$(),282%,25%)= str(fields$(),305%,25%) /* Size     */

           put str(ora_rec$()) using L24050, qty, sub_qty, unit, ext
L24050:               FMT POS(307), PD(14,4), PD(14,4), PD(14,4), PD(14,4)

           str(ora_rec$(),339%,10%) = str(fields$(),12%,10%) /* Order ID */
           str(ora_rec$(),349%,10%) = str(fields$(),22%,10%) /* Item ID  */
           str(ora_rec$(),359%,3%)  = str(fields$(),32%,3%)  /* Display  */
           str(ora_rec$(),362%,1%)  = str(fields$(),38%,1%)  /* Config   */
           str(ora_rec$(),363%,25%) = part_num$              /* Part No  */

           display$ = str(fields$(),32%,3%) 

        build_done
        return

        lookup_bcklines
            str(bcklnes_key$,1%,16%) = str(fields$(),1%,8%)  /* Sales Order */
            str(bcklnes_key$,17%,3%) = str(fields$(),35%,3%) /* Line Number */
            read #2, key = bcklnes_key$, eod goto lnes_done

                   get #2, using L25000, so$, lne$, part_num$, config_lne$

L25000:                FMT POS(10), CH(16), CH(03), POS(32), CH(25), ~
                           POS(284), CH(03)
        lnes_done  
        return

        write_rec
              init(" ") ora_key$
              str(ora_key$,1%,14%) = str(ora_rec$(),1%,14%)
              read #1, hold, key = ora_key$, eod goto no_ora_rec

                   delete #1
        no_ora_rec
              str(ora_key$,1%,14%) = str(ora_rec$(),15%,14%)
              read #1, hold, key 1% = ora_key$, eod goto no_ora_rec1

                   delete #1
        no_ora_rec1

              write #1, using L30000, ora_rec$()
L30000:          FMT 4*CH(256)
        return

        check_mull
           init(" ") readkey$
           str(readkey$,1%,9%)  = "PLANMONLY"
           str(readkey$,10%,3%) = str(part_num$,1%,3%)
           read #3, key = readkey$, eod goto not_mull
                      part$ = "1"
        not_mull
        return

        check_screenonly
           init(" ") readkey$
           str(readkey$,1%,9%)  = "SCREENONL"
           str(readkey$,10%,3%) = str(part_num$,1%,3%)
           read #3, key = readkey$, eod goto not_scrnonly
                      screen$ = "1"
        not_scrnonly
        return


        oracle_query
            oci_err% = 0%
            call "QUERY" (stmt1$, stmt2$, oci_err%)
        return

        oracle_exec
            oci_err% = 0%
            call "EXEC" (stmt1$, stmt2$, oci_err%)
        return

        oracle_flush
            oci_err% = 0%
            call "FLUSH" (oci_err%)
        return

        oracle_fetch
            oci_err% = 0%
            no_fields% = 0%
            call "FETCH" (no_fields%, oci_err%)
            return

            if oci_err% = 0% then return
               call "ERROR" (error$)
               call "SHOSTAT" ("ERROR ERROR RETURN VALUE --> " & error$)
               stop               
        return

        oracle_getfield
            oci_err% = 0%
            field_len% = 0%
            init(" ") field$, name$
            call "FIELDINF" (field_num%, field$, name$, field_len%, oci_err%)

            if oci_err% = 0% then return
               call "ERROR" (error$)
               call "SHOSTAT" ("ERROR ERROR RETURN VALUE --> " & error$)
               stop 
        return
 

        oracle_delete
             init(" ") stmt1$
             str(stmt1$,1%,40%)   = "DELETE FROM MSSQL.DISPLAY WHERE SALESORD" 
             str(stmt1$,41%,30%)  = "ER = '"&str(so$,1%,8%)&"' AND USERID = '"
             str(stmt1$,71%,10%)  = userid$ & "'"
             gosub oracle_flush
             gosub oracle_exec
        return

        check_so_number
            init(" ") ora_key$
            str(ora_key$,1%,14%) = so$
        so_num_next
            read #1, hold, key > ora_key$, using L40000, ora_key$, eod goto so_num_done
L40000:                FMT CH(14)
                  if str(ora_key$,1%,8%) <> so$ then goto so_num_done

                  delete #1
        
                  goto so_num_next

        so_num_done
        return

        ora_desc_done
                   gosub oracle_delete
          end


