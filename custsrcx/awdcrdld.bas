        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDCARDLD                            *~
            *        Load parameter file for Cyberquery                 *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/01/19 ! New Program                              ! RDB *~
            * 08/09/21 ! CR2878 check no paint vendors            ! RDB *~
            * 09/24/21 ! CR2905 extend date on key start          ! RDB *~
            *************************************************************

        dim ld_app_rec$128,              /* (awdappld) New Load File   */~
            ld_app_key0$5,               /* Load No. Primary key       */~
            ld_app_key2$16,              /* Date Secondary key         */~
            ld_bol_date$6,               /* current date               */~
            ld_load_desc$6               /* Check Appian in description*/
            
        dim sn_rec$80,                   /* awdcrdsn Cardinal Sent Load*/~
            sn_load$5                    /* Load Number                */
                    
        dim pm_rec$20,                   /* Parameter record           */~
            pm_load$5                    /* Load Number                */
            
        dim parm_date$6,                 /* Last 2 days for parameter  */~
            start_date$6                 /* Start Appian read by date  */
            
        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(15%)20                 /* Text from file opening     */

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
            * #1  ! AWDAPPLD ! Appian Load File                         *~
            * #2  ! AWDCRDPM ! Cardinal Parameter of Load Numbers File  *~
            * #3  ! AWDCRDSN ! Cardinal Load Numbers sent File          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1, "AWDAPPLD",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =   12, keylen =    5,                    ~
                        alt key 1,  keypos =  1,   keylen =  16,         ~
                            key 2,  keypos =  2,   keylen =  15,         ~
                            key 3,  keypos = 17,   keylen =  15                       

            select #2, "AWDCRDPM",                                       ~
                        varc,     indexed,  recsize = 20,                ~
                        keypos =    1, keylen =   5      
                        
            select #3, "AWDCRDSN",                                       ~
                        varc,     indexed,  recsize = 80,                ~
                        keypos =    1, keylen =   5   

                            
REM            call "SHOSTAT" ("Initialization")

            filename$ = "AWDAPPLD" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub exit_program
            filename$ = "AWDCRDPM" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub exit_program
            filename$ = "AWDCRDSN" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub exit_program

            mat f1% = zer

            call "EXTRACT" addr("ID", userid$)
            call "DATE" addr("G+",date, -2%, parm_date$,err%)
            call "DATE" addr("G+",date, -21%, start_date$,err%)  /* CR2905 */
            
            gosub process_new_loads
            
            goto exit_program

       REM *************************************************************~
           * Process all new Appian Loads                              *~
           *************************************************************

       process_new_loads
        
            init(" ") ld_app_key2$, ld_app_rec$
            str(ld_app_key2$, 1%, 6%) = start_date$
            str(ld_app_key2$, 7%, 9%) = "000000000"

       read_load_nxt
        
            read #1, key 2% > ld_app_key2$, using L11000, ld_app_rec$,  ~
                                                      eod goto no_loads
L11000:      FMT CH(128)  

               ld_app_key2$ = str(ld_app_rec$, 2%, 16%)
               cnt% = cnt% + 1%
               
               ld_bol_date$ = str(ld_app_rec$, 97%, 6%) 
               
               ld_load_desc$ = str(ld_app_rec$, 1%, 1%) 
               if ld_load_desc$ <> "5" then goto read_load_nxt
                  
               if ld_bol_date$ < parm_date$ or ld_bol_date$ = " "  then ~
                     goto read_load_nxt
                   
               gosub check_load_sent
               
               if sentload% = 1%  then goto read_load_nxt
               
               gosub write_parm_file
               
               goto read_load_nxt
no_loads:
       return


        
       REM *************************************************************~
           *   Read load sent file for already sent to Cardinal        *~
           *************************************************************
           
       check_load_sent
            init(" ") sn_rec$, sn_load$
            sentload% = 0%
            sn_load$ = str(ld_app_rec$, 12%,5%) 
            
            read #3, key = sn_load$, using L21000, sn_rec$,  eod goto L22000
L21000:      FMT CH(80)

                sentload% = 1%
L22000: 
       return        

 
       REM *************************************************************~
           *  Write new completed load to parameter file               *~
           *************************************************************
           
       write_parm_file          
            pm_load$ = str(ld_app_rec$, 12%, 5%)

            read #2, hold, key = pm_load$, using L31000, pm_rec$,   ~
                   eod goto L32000               
L31000:      FMT CH(20)
               
               goto L33000      /* already in parameter file */
               
L32000:        str(pm_rec$,1%,5%) = str(ld_app_rec$, 12%,5%) 
               
               put #2, using L31000, pm_rec$
               write #2, eod goto L33000
L33000:
       return

            
       REM *************************************************************~
           *                          E X I T                          *~
           *************************************************************

       exit_program
REM            call "SHOSTAT" ("One Moment Please")
               close #1
               close #2
               close #3
            end
