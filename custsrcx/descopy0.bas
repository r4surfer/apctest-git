REM         +---------------------------------------------------------------+
REM         | copy GENCODES table                                           |
REM         +---------------------------------------------------------------+


        dim                              /* FILE = APCPLNDT            */~
            pc_rec$128,key$24,key2$24,pc_rec2$128                        

        dim f2%(40%),                    /* = 0 if the file is open    */~
            f1%(40%),                    /* = 1 if READ was successful */~
            fs%(40%),                    /* = 1 if file open, -1 if it */~
            pc_ref$8,                    /*   doesn't exist, or 0 if   */~
            r_rec$30, message$256,       /*   not yet checked (OPENCHCK*/~
            rslt$(40%)20                 /* Text from file opening     */ 
            
            

        dim                              /* (AWD083)                   */~
            Ref$2,                       /* Calling Program Flag       */~
            Model$3,                     /* Calling Program Flag       */~
            Calc$3                                                        


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
            * #1  ! GENCODES !                                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),0%, rslt$(1%))

            mat f1% = zer
            upd% = 0%
            tst% = 0%
	    cnt% = 0%           
	    init(hex(00)) key$


 	     init(" ") key$
	     key$ = "APC EFFMC               "            /* status */

L01000:      /* main loop */
            read #1, key > key$, using L50760, pc_rec$, eod goto L56890
            if str(pc_rec$,1,10) <> "APC EFFMC0" then L56890

	    cnt% = cnt% + 1
            key$ = str(pc_rec$,1,24)
            key2$ = key$                      

            for l% = 1% to 12%                    
                pc_rec2$ = pc_rec$
                str(pc_rec2$,1,9) = "APC EFFGL"
                str(pc_rec2$,13,4) = "2012"          
                convert l% to str(pc_rec2$,17,2), pic (00)       
                write #1, using L50760, pc_rec2$
            next l% 
	    goto L01000

L50760:     FMT CH(128)
L56890:     print "         records read    = ", cnt%
            print "         records test    = ", tst%
            print "         records updated = ", upd%
             end
	      
gencodes_data_err:
            print "*** ERROR *** problem updateing GENCODES"
	      stop
