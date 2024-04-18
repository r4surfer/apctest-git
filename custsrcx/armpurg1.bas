        dim purge_date$10, arm_key$21, arm_date$6, yr1$4, date$10
        
        dim f2%(64),                     /* = 0 if the file is open    */~
            f1%(64),                     /* = 1 if READ was successful */~
            axd$4,                       /* AXD Block for OPENFILE     */~
            rslt$(64)20                  /* Text from file opening     */

            select #1, "ARMTRIAL",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =    1, keylen =  21      
                        
            call "SHOSTAT" ("Opening Files, One Moment Please")
            call "OPENCHCK" (#1, fs%(1%), f2%(1%), 50%, rslt$(1%))

REM ========================
REM Set Purge Date Year
REM ========================
        date$ = date
        call "DATFMTC" (date$)
        init(" ") yr1$    :    yr1% = 0%
        yr1$ = str(date$,7%,4%)
        convert yr1$ to yr1%, data goto ARM_PURGE_DONE
        yr1% = yr1% - 2%
        convert yr1% to yr1$, pic(0000)        
        purge_date$ = yr1$ & "1231"     
        call "DATFMTC" (purge_date$)
        call "DATUFMTC" (purge_date$)

        init(" ") arm_key$    
        read_purge_next
          read #1, hold, using ARM_FMT, arm_key$, arm_date$,             ~
                       eod goto ARM_PURGE_DONE
ARM_FMT:         FMT CH(21), POS(97), CH(06)                       
             
          if arm_date$ > str(purge_date$,1%,6%) then goto read_purge_next
          
            delete #1
            
          goto read_purge_next
             
        ARM_PURGE_DONE
        
        end
        
        
        
                             