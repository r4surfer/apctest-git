REM         *************************************************************~
            * AWDCRDRS - Resending Cardinal load file                   *~
            * --------------------------------------------------------- *~
            * Notes - The program promotes for a load number.  The load *~        
            *     is search in the Cardinal send file.  Archive the load*~
            *     and remove for reprocessing of the load.              *~
            *                                                           *~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/11/23 ! New Program                              ! RDB *~
            *************************************************************

        dim readkey$5,                   /* Readkey                    */~
            sn_rec$80,                   /* Parameter record           */~
            sn_load$5                    /* Parameter load number      */

        dim pm_rec$20,                   /* Parameter record           */~
            pm_load$5                    /* Load Number                */
            
        dim rf_pname$8,                  /* Program name               */~
            rf_pf$(3%)20,                /* Function keys              */~
            rf_inpmessage$20,            /* Display message            */~
            rf_errormsg$20,              /* Error message              */~
            fld$(4%)30,                  /* Field Text                 */~
            load$5                       /* Load number                */
  
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
            * #1  ! AWDCRDSN ! Sent Loads with date and time            *~     
            * #2  ! AWDCRDAR ! Archive previous sent load information   *~
            * #3  ! AWDCRDPM ! Parameter file of loads to send          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
                                         
            select #1, "AWDCRDSN",                                       ~
                        varc,     indexed,  recsize = 80,                ~
                        keypos =    1, keylen =   5                           
                                                
            select #2, "AWDCRDAR",                                       ~
                        varc,     indexed,  recsize = 80,                ~
                        keypos =    1, keylen =   17   
                        
            select #3, "AWDCRDPM",                                       ~
                        varc,     indexed,  recsize = 20,                ~
                        keypos =    1, keylen =   5                              

            filename$ = "AWDCRDSN" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub exit_program
            
            filename$ = "AWDCRDAR" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub exit_program
            
            filename$ = "AWDCRDPM" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub exit_program
            
            mat f1% = zer
            init(" ") rf_errormsg$, rf_inpmessage$, load$, readkey$
            rf_pname$ = "AWDCRDRS"

        REM *************************************************************~
            * Screen prompt for load number                             *~
            *************************************************************
            
         deffn'101(fieldnr%)
          gosub rf_set_screen_2
          accept                                                       ~
               at (01,02), fac(hex(8c)), rf_pname$              , ch(08),~
               at (01,12), fac(hex(8c)), date$                  , ch(08),~
               at (02,02), fac(hex(94)), rf_errormsg$           , ch(20),~
                                                                         ~
               at (04,02), fac(hex(84)), fld$(1%)               , ch(20),~
               at (05,02), fac(lfac$(1%)), load$                , ch(05),~
                                                                         ~
               at (07,02), fac(hex(a4)),   rf_inpmessage$       , ch(20),~
               at (08,02), fac(hex(8c)),   rf_pf$(1%)           , ch(20),~
                                                                         ~
               keys(pfkeys$), key(keyhit%)

               if keyhit% <> 2% then L01000
                  gosub endpgm
L01000:
               if keyhit% <> 16% then L01005
                  gosub save_clear_send
L01005:               
               close ws
               call "SCREEN" addr ("C", u3%, "I", i$(), cursor%())
               goto endpgm
               
        return
        rf_set_screen_2

         fieldnr% = 1%  
         fld$(1%)   = "Enter load #:       "
         rf_pf$(1%) = "(2)Exit  (16)Resend "
         pfkeys$ = hex(ff02ffffffffffffffffffffffffff1000)

         lfac$(1%) = hex(81)
         
        return
endpgm:            
            goto exit_program

        REM *************************************************************~
            * Read send file for given load number and delete           *~
            *************************************************************
        save_clear_send

            str(readkey$,1%,5%) = load$
      
        read_send_nxt

            read #1, hold, key = readkey$,  using L01200, sn_rec$  , ~
                           eod goto no_more_send

L01200:           FMT CH(80)
                
                delete #1
  
                str(sn_rec$, 18%, 6%) = date
                str(sn_rec$, 24%, 6%) = time
                write #2, using L01200, sn_rec$
                
                gosub write_parm_file
        return

no_more_send:
              rf_errormsg$ = "Load Not Sent Yet"
              gosub'101(1%) 
       return
        
       REM *************************************************************~
           *  Write new completed load to parameter file               *~
           *************************************************************
           
       write_parm_file          
            pm_load$ = str(sn_rec$, 1%, 5%)

            read #3, key = pm_load$, using L31000, pm_rec$,   ~
                   eod goto L32000               
L31000:      FMT CH(20)
               
               goto L33000      /* already in parameter file */
               
L32000:        str(pm_rec$,1%,5%) = str(sn_rec$, 1%,5%) 
               
               put #3, using L31000, pm_rec$
               write #3, eod goto L33000
L33000:
       return

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************
        exit_program
               call "ALLFREE"        
               close #1
               close #2
               close #3
            end
