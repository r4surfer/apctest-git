*       ****************************************************************~
*                       NEW -( P U R G E  O P T I O N S )              *~
*                            For TX Cut Sheet Data Files               *~
*                  ( As of 08/11/2014 - PWW                            *~
*        APCCUTFP - Cut Sheets Data Purge Program                      *~
*                     North Texas                                      *~
*----------------------------------------------------------------------*~
*Mod Date  ! Description                                           !By *~
*----------+-------------------------------------------------------+---*~
*08/12/2014! New Program                                           !PWW*~
*          !                                                       !   *~
************************************************************************

        dim                                                              ~
            f1_rec$(1)256, f2_rec$128, f1_key$48, f2_key$48,             ~
            rec_type$1, f3_rec$128, f3_key$48                            
            
        dim series_style$15, sash$1, date$8,                             ~
            pg_dte$6                     /* Today's Date               */

                           
        dim scr_deptx$3, scr_dtex$8, sash_width$16, sash_height$16,      ~
            scr_dtein$8, prod_dtex$8, prod_dtein$8, f1_rail_component$4
            
        purge_days% = 14                                 /*days old */
        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #10 ! APCCUTF1 ! Header File for Cut Sheet Data           *~
            * #11 ! APCCUTF2 ! Detail File for Cut Sheet Data           *~
            * #12 ! APCCUTF3 ! Detail File for Cut Sheet Data           *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #10,  "APCCUTF1",                                     ~
                        varc,     indexed,  recsize =   256,             ~
                        keypos =    1, keylen =    40                          
                       

            select #11,  "APCCUTF2",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =   42 

            select #12,  "APCCUTF3",                                     ~
                        varc,     indexed,  recsize = 128,               ~
                        keypos =    1, keylen =   40 

            call "OPENCHCK" (#10, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#11, 0%, 0%, 0%, " ")
            call "OPENCHCK" (#12, 0%, 0%, 0%, " ")

            pg_dte$ = date 
                                            /* More than purge_days old */ 
            call "DATE" addr("G+",pg_dte$, -(purge_days%),pg_dte1$,err%)

        init (" ") f1_rec$, f2rec$, f3_rec$, f1_key$, f2_key$, f3_key$
        purged_records = 0
        f1_key$ = all(hex(00))
        purge_next
           read #10,hold,key > f1_key$, using f1_fmt, f1_key$,          ~
                               eod goto end_processing
           if str(f1_key$,1%,6%) >= pg_dte1$ then goto end_processing
           delete #10
           gosub purge_f2
           gosub purge_f3
           purged_records = purged_records + 1
           goto purge_next
        
f1_fmt:     FMT POS(1), CH(40)
f2_fmt:     FMT POS(1), CH(42)
f3_fmt:     FMT POS(1), CH(40)

        purge_f2
           f2_key$ = all(hex(00))
           f2_key$ = str(f1_key$,1%,39%)
        purge_next_f2
           read #11,hold,key > f2_key$, using f2_fmt, f2_key$,           ~
                               eod goto end_purge_f2
           if str(f2_key$,1%,39%) <> str(f1_key$,1%,39%) then goto end_purge_f2
           delete #11
           goto purge_next_f2
           
        end_purge_f2
           return
           
        purge_f3
           f3_key$ = all(hex(00))
           f3_key$ = str(f1_key$,1%,39%) & "4"
           read #12,hold,key = f3_key$, using f3_fmt, f3_key$,            ~
                               eod goto end_purge_f3
           delete #12
        end_purge_f3
           return
           
        end_processing
        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************

        exit_program
            call "SHOSTAT" ("One Moment Please")
        end
