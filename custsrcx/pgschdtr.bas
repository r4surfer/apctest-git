        REM *************************************************************~
            *                                                           *~
            *  Program Name      - PGSCHDTR                             *~
            *  Creation Date     - 10/11/2018                           *~
            *  Last Modified Date-                                      *~
            *  Written By        - Ricky Beane                          *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New Program to Build | delimited     *~
            *                      files to import into FTP.            *~
            *  Code Tables Used  -                                      *~
            *                                                           *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 10/11/18 ! New Program                              ! RDB *~
            * 03/01/19 ! ATLaS changes                            ! RDB *~
            * 05/31/19 ! CR2052 Skip seq# 00000 from sending      ! RDB *~
            * 07/24/19 ! CR20137 Swap FGO and OGO Dept 003 to 033  ! RDB *~
            *************************************************************

        dim                              /* (CATEGORY) - FILE          */~
            readkey$100,                 /* Readkey                    */~
            fields$(500%)256             /* Generic Fields             */
 
        dim                              /* (Program) - Variables      */~
            hdr$45, msg$(3%)79,          /* Askuser - Var's            */~
            userid$3,                    /* Current User Id            */~
            readdate$10                  /* Read date                  */
            
        dim                                                              ~
            dt_key$23,                   /* (APCPLNDT)-Tracking File   */~
            dt_ref$8,                    /* Warranty                   */~
            dt_seq$5,                    /* Sequence Number            */~
            dt_date$6,                   /* Production Date            */~
            dt_part$25,                  /* Part Number                */~
            dept$3,                      /* Department                 */~
            dtcnt$4                      /* Count DT SO reads          */
  
        dim tr_rec$256,                  /* Trigger record             */~
            tr_key$64,                   /* Trigger key #1             */~
            filetype$20,                 /* Trigger file type          */~
            transmit$1,                  /* 0 not sent, 1 sent         */~
            date$6,                      /* Trigger date               */~
            time$6,                      /* Trigger time               */~
            filetype2$20,                /* Trigger file type          */~
            salesorder$8,                /* Sales Order Number         */~
            linenbr$3,                   /* Sales Order Line Number    */~
            currdte$10,                  /* Current date               */~
            rlsedate$10,                 /* Production Plan Date       */~
            plndate$10,                  /* Date Planned               */~
            trgst$2,                     /* Trigger status saved       */~
            trgpgm$10,                   /* Trigger program            */~
            wcnt$6,                      /* Write counter for EOF      */~
            filler1$245,                 /* Record filler space        */~
            fillerA$164,                 /* End of record filler       */~
            spshape$1,                   /* Special Shape CR20137      */~ 
            desc$30,                     /* Gencode Description        */~
            upddte$6,                    /* Updated Trigger status date*/~
            updtime$6                    /* Updated Trigger status time*/
            
        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(15%)20                 /* Text from file opening     */

        dim pipe$1,                     /* Comma for EWDAPPSN File    */~
            file$8,                      /* File Name                  */~
            library$8,                   /* Library Name = APCDATA     */~
            volume$6                     /* DISK VOLUME = CARLOS       */

        dim schema$8                     /* Schema                     */           

        REM *************************************************************

            mat f2% = con
            mat fs% = zer
            rslt$() = " "

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                     S E L E C T                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! PGSCHDTR ! Trigger file of Orders to process        *~
            * #2  ! APCPLNDT ! Planning Tracking File                   *~
            * #3  ! GENCODES ! Master Code Tables File                  *~  
            * #10 ! PGSCHDHR ! Ply Gem data file Schedule Header        *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1, "PGSCHDTR",                                       ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    21, keylen =   44,                   ~
                        alt key 1, keypos = 1, keylen = 64,              ~
                            key 2, keypos = 54, keylen = 11, dup  
                   
            select #2,  "APCPLNDT",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 24,   keylen = 23,                      ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup
                            
            select #3, "GENCODES",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24            
                        
            select #10, "PGSCHDHR",                                      ~
                        varc,     indexed,  recsize = 256,               ~
                        keypos =    1, keylen = 91
                            
REM            call "SHOSTAT" ("Initialization")

            filename$ = "PGSCHDTR" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "APCPLNDT" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "GENCODES" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error

            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            
            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #3, schema_err%)            

             file$   = "PGSCHDHD"
             ff% = 10%   
             volume$ = "NE2"
             gosub open_file
             
        REM  begin_files_analysis
            gosub initialize_variables

            tr_key$ = all(hex(00))
 
            gosub read_trigger

            goto exit_program
              
        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") tr_key$, fields$(), filename$, hdr$, msg$(),~
                      date$, library$, volume$, readdate$
            pipe$ = "|"
        return

        REM *************************************************************~
            * Gather data for schedule header file and write record     *~
            *************************************************************
        read_trigger

            rec% = 0%
            str(tr_key$,1%,20%) = "SCHDHDR"
            str(tr_key$,21%,1%) = "0"
            
            read #1, hold, key 1% >= tr_key$, using L01000, tr_rec$,  ~
                  eod goto read_trg_done 
L01000:       FMT CH(256)

                goto L01100
                   
        read_trg_nxt
            init(" ") fields$()
  
            rec% = 0%
            read #1, hold, eod goto read_trg_done

L01100:         cnt% = cnt% + 1%

                get #1%, using L01200, filetype$, transmit$, plndate$, time$, ~
                            filetype2$, salesorder$, linenbr$, upddte$,       ~
                            updtime$, trgst$, trgpgm$, fillerA$

L01200:           FMT CH(20), CH(1), CH(6), CH(6), CH(20), CH(8), CH(3), ~
                      CH(6), CH(6), CH(02), CH(10), CH(168)
 
               if filetype$ <> "SCHDHDR" or transmit$ <> "0" then read_trg_done
                           
               gosub get_planned_data
               if crash% = 1% or dtcnt% = 0% then read_trg_nxt
               
               delete #1
               
               str(upddte$,1%,6%) = date
               updtime$ = time
               transmit$ = "1"
               put #1, using L01200, filetype$, transmit$, plndate$, time$, ~
                            filetype2$, salesorder$, linenbr$, upddte$,     ~
                            updtime$, fillerA$
               write #1, eod goto read_trg_nxt
            
               rec% = 1%            
               goto read_trg_nxt
            
        read_trg_done
               gosub write_eof
        return


        REM *************************************************************~
            * Read all of the records from DT                           *~
            *************************************************************        
        get_planned_data
            init(" ") dt_key$, desc$, dept$
            dtcnt% = 0%
            str(dt_key$,1%,8%) = salesorder$
            str(dt_key$,9%,2%) = linenbr$
            str(dt_key$,11%,8%) = "00000000"
            
        get_nxt_planned
            read #2, key > dt_key$, using L03000, dt_key$, dt_date$,   ~
                  dt_ref$, dt_seq$, dt_part$, ~
                                eod goto plan_done
                  
L03000:    FMT POS(24), CH(23), POS(47), CH(6), POS(96), CH(8), POS(111), ~
               CH(5), POS(189), CH(25)
            
            if str(dt_key$,1%,8%) <> salesorder$ or    ~
               str(dt_key$,9%,2%) <> str(linenbr$,1%,2%)  then plan_done
               
            if dt_seq$ = "00000" then goto get_nxt_planned    /* CR2052 */
            
            dtcnt% = dtcnt% + 1%
            dept$ = str(dt_key$,19%,3%)
/* CR20137 */
            spshape$ = str(dt_part$,11%,1%)
            if (dept$ = "002" or dept$ = "003") and ~
               (spshape$ = "6" or spshape$ = "7") then dept$ = "033"
               
            gosub dept_desc

            gosub set_fields
                           
            gosub write_upload_schd
            
            goto get_nxt_planned
            
        plan_done
        return

        REM *************************************************************~
            * Set fields for file output                                *~
            *************************************************************
        set_fields
            init(" ") fields$()
            
            currdte$  = date : call "DATFMTC" (currdte$)
            convert dtcnt% to dtcnt$, pic(0000)
            timefmt$ = time

            fields$(1%) = currdte$
            fields$(2%) = timefmt$
            fields$(3%) = dtcnt$
            fields$(4%) = "DALLAS"
            fields$(5%) = "SO"
            fields$(6%) = salesorder$
               linenbr% = 0%
               convert linenbr$ to linenbr%
            convert linenbr% to fields$(7%), pic(##0)
            fields$(9%) = dept$ & " - " & desc$  /* 120618 chg CR20137 */
            currdte$ = plndate$ : call "DATFMTC" (currdte$)
            
            fields$(10%) = currdte$
            gosub set_field_16

            rlsedate$ = dt_date$: call "DATFMTC" (rlsedate$)
            fields$(19%) = rlsedate$
            if trgpgm$ = "APCPLN06" and trgst$ = "04" then ~
                   fields$(23%) = "A"  ~
              else fields$(23%) = "U"
            fields$(24%) = "1"
            fields$(29%) = dt_seq$
            fields$(30%) = dt_seq$
            fields$(31%) = dt_ref$
            fields$(32%) = dt_ref$
            fields$(33%) = str(dt_key$,11%,4%)
            init(" " ) fields$(34%) 
            
        return
        
        REM *************************************************************~
            * Set field to identify line types                          *~
            *************************************************************
        set_field_16
        
            fields$(16%) = "REGULAR"    /* default value */

            if str(dt_key$,19%,3%) = "000"  then goto L03500
            
            if str(dt_part$,11%,1%) = "4" or str(dt_part$,11%,1%)= "5" then ~
                goto L03501

            if str(dt_part$,11%,1%) = "4" or str(dt_part$,11%,1%)= "5" then ~
                goto L03502
                          
            if len(dt_part$) < 19% then  fields$(16%) = "PART   " 
            goto L03900
            
L03500:     fields$(16%) = "SCRONLY"    
            return

L03501:     fields$(16%) = "SASONLY"
            return

L03502:     fields$(16%) = "GLSONLY"
            return             

L03900: return        
        
        REM *************************************************************~
            * Write the Schedule Header record                          *~
            *************************************************************
        write_upload_schd
            crash% = 0%
            write #10, using L04000, fields$(1%), fields$(2%), fields$(3),  ~
                              pipe$, fields$(4%), pipe$,       ~
                              fields$(5%), pipe$, fields$(6%),  ~
                              pipe$, fields$(7%), pipe$,       ~
                              fields$(8%), pipe$, fields$(9%),  ~
                              pipe$, fields$(10%), pipe$,      ~
                              fields$(11%), pipe$, fields$(12%),~
                              pipe$, fields$(13%), pipe$,      ~
                              fields$(14%), pipe$, fields$(15%),~
                              pipe$, fields$(16%), pipe$,      ~
                              fields$(17%), pipe$, fields$(18%),~
                              pipe$, fields$(19%), pipe$,      ~
                              fields$(20%), pipe$, fields$(21%),~
                              pipe$, fields$(22%), pipe$,      ~ 
                              fields$(23%), pipe$, fields$(24%),~
                              pipe$, fields$(25%), pipe$,      ~
                              fields$(26%), pipe$, fields$(27%),~
                              pipe$, fields$(28%), pipe$,      ~
                              fields$(29%), pipe$, fields$(31%),~
                              pipe$, fields$(33%), pipe$,      ~
                              fields$(34%),                     ~
                                  eod goto crash

                  wcnt%  = wcnt%  + 1

        return
 
crash:  
REM call "SHOSTAT" ("Write Crash")
          crash% = 1%
        return
           
L04000:          FMT CH(10), CH(8), CH(4), CH(1),                             ~
                     CH(20), CH(1), CH(10), CH(1),                            ~
                     CH(8), CH(1), CH(3), CH(1), CH(2), CH(1), CH(30), CH(1), ~
                     CH(10), CH(1), CH(2), CH(1), CH(2), CH(1), CH(2), CH(1), ~
                     CH(2), CH(1), CH(2), CH(1), CH(7), CH(1), CH(2), CH(1),  ~
                     CH(2), CH(1), CH(10), CH(1), CH(2), CH(1), CH(2), CH(1), ~
                     CH(2), CH(1), CH(1), CH(1), CH(5), CH(1), CH(2), CH(1),  ~
                     CH(2), CH(1), CH(2), CH(1), CH(2), CH(1), CH(5), CH(1),  ~
                     CH(8), CH(1), CH(4), CH(1), CH(54)
                 
             /* date                      fields$( 1%)       10/11/2018  */
             /* time                      fields$( 2%)       14153311    */
             /* read so counter           fields$( 3%)       ####        */
             /* company                   fields$( 4%)       DALLAS      */
             /* order_type                fields$( 5%)       SO          */
             /* order_number              fields$( 6%)       09004546    */
             /* line_number               fields$( 7%)       1           */
             /* schedule                  fields$( 8%)                   */
             /* department                fields$( 9%)    022 - 150/160  */
             /* release_time              fields$(10%)       10/1/12018  */
             /* panel_count               fields$(11%)                   */
             /* panel_01_type             fields$(12%)                   */
             /* panel_02_type             fields$(13%)                   */
             /* panel_03_type             fields$(14%)                   */
             /* panel_04_type             fields$(15%)                   */
             /* release_type              fields$(16%)       REGULAR     */
             /* update_time               fields$(17%)                   */
             /* work_order                fields$(18%)                   */
             /* build_date                fields$(19%)       10/18/2018  */
             /* schedule_group            fields$(20%)                   */
             /* requested_date            fields$(21%)                   */
             /* scheduled_route           fields$(22%)                   */
             /* action                    fields$(23%)       A           */
             /* quantity_scheduled        fields$(24%)       12          */
             /* parent_group              fields$(25%)                   */
             /* parent_group_sequence     fields$(26%)                   */
             /* child_group               fields$(27%)                   */
             /* child_group_sequence      fields$(28%)                   */
             /* production_batch_sequence fields$(29%)       00012       */
             /* skip ending_batch_sequence     fields$(30%)       00012       */
             /* beginning_warranty        fields$(31%)       03120112    */
             /* skip ending_warranty           fields$(32%)       03120124    */
             /* barcode_sequence          fields$(33%)                   */

        REM *************************************************************~
            *   Get department description                              *~
            *************************************************************
        dept_desc
           desc$ = "N/A"
           readkey$ = " "
           str(readkey$,1%,9%)    = "PLAN DEPT"
           str(readkey$,10%,15%)  = dept$
           read #3,key = readkey$, using L60000, desc$, eod goto L60020
L60000:        FMT POS(25), CH(30)

L60020: return
        
        REM *************************************************************~
            *                        E R R O R                          *~
            *************************************************************
        open_error
           comp% = 2%
           hdr$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = "(Open Error) - File = " & filename$
           msg$(3%) = "Press Any Key To Continue."
REM          call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        REM *************************************************************~
            * Open file in special library                              *~
            *************************************************************
        open_file
            init(" ") library$, volume$
            library$        = "ORAFILES"
            volume$ = "NE2"                                            
            if schema% = 1% then volume$ = "CARLO2"

             open nodisplay #ff%,  output, space = 100%,                 ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return

        REM *************************************************************~
            * Write the End of File counter                             *~
            *************************************************************
        write_eof
            convert wcnt%  to wcnt$, pic(000000)
            EOF$ = "@EOF#"
            init(" ") filler1$ 
            write #10, using L70000, EOF$, wcnt$, filler1$, eod goto crash
            
L70000:       FMT CH(5), CH(6), CH(245)
        return 
        
        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************~

        exit_program
REM            call "SHOSTAT" ("One Moment Please")
               close #1 : close #2 : close #3 : close #10

            end
