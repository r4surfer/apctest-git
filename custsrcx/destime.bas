REM         +---------------------------------------------------------------+
REM         | change "Days of Inactivity Before Diasbling Logon"            |
REM         +---------------------------------------------------------------+


        dim f2%(40%),                    /* = 0 if the file is open    */~
            f1%(40%),                    /* = 1 if READ was successful */~
            fs%(40%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(40%)20                 /* Text from file opening     */ 
            
            dim rec$(3)200,key$3            


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************

            mat f2% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! USERLCMS !                                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "USERLCMS",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 600,                                   ~
                        keypos =    1, keylen =   3,                     ~
                        alt key 1, keypos =  4, keylen = 30, dup         ~

            call "EXTRACT" addr("ID", zzid$, "IL", zzil$, "OL", zzol$,   ~
                                "IV", zziv$, "OV", zzov$, "CV", progvol$,~
                                "UE", zze$, "UR", zzr$, "UW", zzw$,      ~
                                "XL", syslib$, "XV", sysvol$)


            if pos(zze$<>hex(ff)) <> 0 then L02850
               if pos(zzr$<>hex(ff)) <> 0 then L02850
                  if pos(zzw$<>hex(ff)) <> 0 then L02850
                     secadm$ = "YES"

L02850:     call "READFDR" addr("USERLCMS", syslib$, sysvol$, 0%, f2%(1))
                if f2%(1) = 0% then L02910

            call "PUTNAMES" addr(#1, "USERLCMS", syslib$, sysvol$)
            call "WORKOPN2" (#1, "OUTSP", 100%, f2%(1))

            close #1
L02910:     call "PUTNAMES" addr(#1, "USERLCMS", syslib$, sysvol$)
            call "WORKOPN2" (#1, "SHARE", 100%, f2%(1))

            mat f1% = zer

L01000:      /* main loop */
             read #1, hold, using L50760, rec$(), eod goto END_JOB         
             get rec$() using L50765, days%                                 
	     user$ = str(rec$(),1,3)
	     if user$ = "DES" then L01000 
	     if user$ = "DSD" then L01000 
	     if user$ = "CMG" then L01000 
             days% = 60%
             put rec$(), using L50765, days%
             rewrite #1, using L50760, rec$()                        
             goto L01000

L50760:     FMT 3*CH(200)                                                    
L50765:     FMT POS(408), BI(1)                                                    
END_JOB:    end
