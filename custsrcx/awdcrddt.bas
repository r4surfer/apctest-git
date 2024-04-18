        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDCARDDT                            *~
            *        Delete all records in the parameter file           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/11/19 ! New Program                              ! RDB *~
            *************************************************************

        dim                              /* (CATEGORY) - FILE          */~
            readkey$5                    /* Readkey                    */

        dim pm_rec$20,                   /* Parameter record           */~
            pm_load$5                    /* Parameter load number      */
        
        dim sn_rec$80,                   /* awdcrdsn Cardinal Sent Load*/~
            sn_load$5                    /* Load Number                */
            
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
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1, "AWDCRDPM",                                       ~
                        varc,     indexed,  recsize = 20,                ~
                        keypos =    1, keylen =   5         
                        
                                                
            select #2, "AWDCRDSN",                                       ~
                        varc,     indexed,  recsize = 80,                ~
                        keypos =    1, keylen =   5   
                        
REM            call "SHOSTAT" ("Initialization")

            filename$ = "AWDCRDPM" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub exit_program
            filename$ = "AWDCRDSN" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub exit_program
            
            mat f1% = zer
 
            gosub save_clear_parms
            
            goto exit_program

        REM *************************************************************~
            * Read all records in parameter file and delete             *~
            *************************************************************

        save_clear_parms
            
            str(readkey$,1%,5%) = all(hex(00))
      
        read_parm_nxt

            read #1, hold, key > readkey$,  using L01200, pm_load$, filler$ , ~
                           eod goto no_more_parms

L01200:           FMT CH(05), CH(15)

                readkey$ = pm_load$
                
                delete #1
                
                gosub write_sent_file

                goto read_parm_nxt
no_more_parms:

        return


        REM *************************************************************~
            *   Write the parameter dates to the Cardinal Loads Sent    *~
            *************************************************************
        write_sent_file
        
            init(" ") sn_rec$, sn_load$
            sn_load$ = pm_load$
            
            read #2, hold, key = sn_load$, using L21000, sn_rec$,  ~
                 eod goto L22000
L21000:      FMT CH(80)
              
              goto L23000   
       
L22000:       str(sn_rec$,  1%, 5%) = sn_load$
              str(sn_rec$,  6%, 6%) = date
              str(sn_rec$, 12%, 6%) = time
 
              put #2, using L21000, sn_rec$
              write #2, eod goto L23000

L23000:
        return


        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************
        exit_program
               close #1 
               close #2
            end
