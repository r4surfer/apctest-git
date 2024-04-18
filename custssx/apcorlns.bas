      REM ***************************************************************~
          *                                                             *~
          *  Subroutine Name   - APCORLNS                               *~
          *  Creation Date     - 04/08/2019                             *~
          *  Last Modified Date- 05/22/2019                             *~          
          *  Written By        - Ricky Beane                            *~
          *                                                             *~
          *  Description       - Load the Order Line trigger for ATLas  *~
          *                      extract later.  Only 1 record in the   *~
          *                      trigger file with 0 transmit status    *~
          *                      needed.                                *~
          *-------------------------------------------------------------*~
          *          - Paramters                                        *~
          *             Input - Sales Order, Line Number, Status,       *~ 
          *                     Program calling this subroutine         *~
          *          - INPUT Files                                      *~
          *                     PGORLNTR (PlyGem Order Line Trigger)    *~
          *          - ERROR% = 0% - Ok                                 *~
          *                    99% - Error                              *~
          *-------------------------------------------------------------*~
          *                  M O D I F I C A T I O N S                  *~
          *---WHEN----+----------------WHAT----------------------+-WHO--*~
          * 03/08/2019! New subroutine                           ! RDB  *~
          * 03/21/2019! Add wood$ to return values               ! RDB  *~
          *05/22/2019! (CR2038) add shipto to pgorlntr           ! CMN *~          
          ***************************************************************

        sub "APCORLNS" (cust$,          /* Customer (CR2038)          */~
                        so$,             /* Sales Order Number         */~
                        line$,           /* Line Number                */~
                        status$,         /* Status code                */~
                        pgmname$,        /* Program Name               */~
                        #1,              /* PGORLNTR                   */~
                        error%)          /* Return Code                */
                        
        dim                                                     ~
            cust$9,           /* Customer   (CR2038)          */~
            customer$9,       /* Customer   (CR2038)          */~
            so$8,             /* Sales Order Number           */~
            line$2,           /* Line Number                  */~
            status$2,         /* Status code                  */~
            pgmname$10,       /* Program Name                 */~
            error%            /* Return Code                  */   
        
        dim filetype$20,      /* File type triggered          */~
            transmit$1,       /* Transmit code                */~
            filetype2$20,     /* File type triggered for key  */~
            salesorder$8,     /* Sales Order Number           */~
            linenbr$2,        /* Line number                  */~
            pgdate$6,         /* Date triggered               */~
            time$6,           /* Time triggered               */~
            upddte$6,         /* Update Date on trigger send  */~
            updtime$6,        /* Update Time on trigger send  */~
            st$2,             /* Status of the order          */~
            pgm$10,           /* Program name call subroutine */~
            filler1$159       /* Filler space                 */
            

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************                       
            error%  = 0%
     
        REM *************************************************************~
            *       M A I N   P R O G R A M                             *~
            *************************************************************    
            
/************************************************************************/
/* Load the Order Line trigger for ATLas extract later.  Only 1         */
/* record in the trigger file with 0 transmit status needed.            */
/************************************************************************/

            init(" ") filetype$, transmit$, filetype2$, salesorder$, linenbr$, ~
                      pgdate$, time$, upddte$, updtime$, st$, pgm$, filler1$    

            str(tr_key$,1%,8%) = so$
            str(tr_key$,9%,2%) = line$

            read #1, key 2% >= tr_key$,   ~
                   using L01000, filetype$, transmit$, pgdate$, time$,       ~
                                 filetype2$, salesorder$, linenbr$, upddte$, ~
                                 updtime$, st$, pgm$, customer$, filler1$,   ~
                                 eod goto new_or_trigger     /* (CR2038) */

              goto trigger_or_process
               
            nxt_or_trigger   
               read #1, using L01000, filetype$, transmit$, pgdate$, time$,  ~
                                 filetype2$, salesorder$, linenbr$, upddte$, ~
                                 updtime$, st$, pgm$, customer$, filler1$,   ~
                             eod goto new_or_trigger         /* (CR2038) */
trigger_or_process:                     
                  if salesorder$ <> so$ or      ~
                     linenbr$ <> line$  then goto new_or_trigger
                     
                  if transmit$ = "0" and pgm$ = pgmname$  ~
                                          then  goto trigger_or_done
               goto nxt_or_trigger      
                               
            new_or_trigger
               filetype$   = "ORDERLINE"
               transmit$   = "0"
               str(pgdate$,1%,6%) = date
               time$       = time 
               filetype2$  = "ORDERLINE"
               salesorder$ = so$
               linenbr$    = line$
               upddte$     = " "
               updtime$    = " "
               st$         = status$
               pgm$        = pgmname$
               customer$   = cust$                      /* (CR2038) */
               
               put #1, using L01000, filetype$, transmit$, pgdate$, time$, ~
                            filetype2$, salesorder$, linenbr$, upddte$,      ~
                            updtime$, st$, pgm$, customer$, filler1$
                                                               /* (CR2038) */
L01000:           FMT CH(20), CH(1), CH(6), CH(6), CH(20), CH(8), ~
                           CH(3), CH(6), CH(6), CH(02), CH(10), CH(159)
                           
               write #1
               
        trigger_or_done  
               error% = 0%

        REM *************************************************************~
            *                          E X I T                          *~
            *************************************************************
        exit_sub

        end
        
        
        
        