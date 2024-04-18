REM         +---------------------------------------------------------------+
REM         | modify amtbomif                                               |
REM         +---------------------------------------------------------------+


        dim                              /* FILE = APCPLNDT            */~
            comp_key$11,                 /* Detail Record              */~
            part_info$20                   

        dim f2%(40%),                    /* = 0 if the file is open    */~
            f1%(40%),                    /* = 1 if READ was successful */~
            fs%(40%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(40%)20                 /* Text from file opening     */ 
            
            dim rec$120, key$32, model$15, field$2, value$15           
        dim scr12$20, prt12$30                    
        dim scr19$20, prt19$30                    
        dim scr20$20, prt20$30                    
        dim scr211$20, prt211$30                    
        dim scr212$20, prt212$30                    
        dim scr221$20, prt221$30                    
        dim scr222$20, prt222$30                    


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$41, pname$32, apc1$41                   /* (EWD055) */
                                                            /* (EWD060) */
                                                            /* (EWD066) */
                                                            /* (EWD068) */
                                                            /* (EWD072) */
                                                            /* (AWD077) */
                                                            /* (AWD082) */ 
        REM *************************************************************

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
            * #1  ! AMTBOMIF !                                          *~
            * #2  ! GENCODES !                                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1,  "AMTBOMIF",                                     ~
                        varc,     indexed,  recsize = 120,              ~
                        keypos =    1, keylen =    32                   
            select #2, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,             ~
                        keypos =    1, keylen =  24

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),0%, rslt$(2%))

            mat f1% = zer

	     init(" ") key$, rec$
	     model$ = "               "
	     scr12$ = "Flat (Colonial)"
	     prt12$ = "FLAT"

	     scr19$ = "SDL"              
	     prt19$ = "SDL" 

	     scr20$ = "1 3/8"            
	     prt20$ = "1 3/8"            

	     scr211$ = "WH"               
	     prt211$ = "WHITE"            

	     scr212$ = "AL"               
	     prt212$ = "ALMOND"           

	     scr221$ = "WH"               
	     prt221$ = "WHITE"            

	     scr222$ = "AL"               
	     prt222$ = "ALMOND"           

L01000:      /* main loop */
             str(key$,01,15) = model$                  
             str(key$,16,17) = "ZZZZZZZZZZZZZZZZZ"

REM          get next model number 
             read #1, key > key$, using L50760, rec$,               ~
                                         eod goto END_JOB

L02000:      str(rec$,16,17) = "190              "
             str(rec$,34,20) = "NA                  "    
             str(rec$,54,30) = "NA                            "
             write #1, using L50760, rec$, eod goto L03000 
L03000:      str(rec$,16,17) = "200              "
             str(rec$,34,20) = "NA                  "    
             str(rec$,54,30) = "NA                            "
             write #1, using L50760, rec$, eod goto L04000 
L04000:      str(rec$,16,17) = "210              "
             str(rec$,34,20) = "NA                  "    
             str(rec$,54,30) = "NA                            "
             write #1, using L50760, rec$, eod goto L05000 
L05000:      str(rec$,16,17) = "220              "
             str(rec$,34,20) = "NA                  "    
             str(rec$,54,30) = "NA                            "
             write #1, using L50760, rec$, eod goto L06000 
REM          get field 12 and check for "4" 
L06000:      model$ = str(rec$,01,15)                  
             str(key$,01,15) = model$                  
             str(key$,16,17) = "124              "
             read #1, key = key$, using L50760, rec$,               ~
                                         eod goto L01000 

REM          get field 13 and check for "1" 
             str(key$,01,15) = model$                  
             str(key$,16,17) = "131              "
             read #1, key = key$, using L50760, rec$,               ~
                                         eod goto L01000 

REM          get field 14 and check for "8" 
             str(key$,01,15) = model$                  
             str(key$,16,17) = "148              "
             read #1, key = key$, using L50760, rec$,               ~
                                         eod goto L01000 

             init(" ") rec$
	     str(rec$,33,1)  = "l"
	     str(rec$,112,8) = "0.0000  "
             str(rec$,01,15) = model$                  
             str(rec$,16,17) = "121              "
             str(rec$,34,20) = scr12$                    
             str(rec$,54,30) = prt12$                    
             write #1, using L50760, rec$, eod goto L02500 

L02500:      str(rec$,16,17) = "191              "
             str(rec$,34,20) = scr19$                    
             str(rec$,54,30) = prt19$                    
             write #1, using L50760, rec$, eod goto L03500 

L03500:      str(rec$,16,17) = "201              "
             str(rec$,34,20) = scr20$                    
             str(rec$,54,30) = prt20$                    
             write #1, using L50760, rec$, eod goto L04500 

L04500:      str(rec$,16,17) = "211              "
             str(rec$,34,20) = scr211$                    
             str(rec$,54,30) = prt211$                    
             write #1, using L50760, rec$, eod goto L05500 
L05500:      str(rec$,16,17) = "212              "
             str(rec$,34,20) = scr212$                    
             str(rec$,54,30) = prt212$                    
             write #1, using L50760, rec$, eod goto L06500 

L06500:      str(rec$,16,17) = "221              "
             str(rec$,34,20) = scr221$                    
             str(rec$,54,30) = prt221$                    
             write #1, using L50760, rec$, eod goto L07000 
L07000:      str(rec$,16,17) = "222              "
             str(rec$,34,20) = scr222$                    
             str(rec$,54,30) = prt222$                    
             write #1, using L50760, rec$, eod goto L08000 

REM          delete field 12 and value "4" 
L08000:      model$ = str(rec$,01,15)                  
             str(key$,01,15) = model$                  
             str(key$,16,17) = "124              "
             read #1, key = key$, hold, using L50760, rec$,               ~
                                         eod goto L01000 
             delete #1
             goto L01000

L50760:     FMT CH(120)                                                  
END_JOB:    end
