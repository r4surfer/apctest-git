
        dim                              /* FILE = APCPLNDT            */~
            pc_rec$(3)256         /* Detail Record              */

        dim f2%(40%),                    /* = 0 if the file is open    */~
            f1%(40%),                    /* = 1 if READ was successful */~
            fs%(40%),                    /* = 1 if file open, -1 if it */~
            pc_key$9,                    /*   doesn't exist, or 0 if   */~
            rslt$(40%)20                 /* Text from file opening     */ 
            
            

        dim                              /* (AWD083)                   */~
            des$45,                      /* Calling Program Flag       */~
            tbl$(11)3                                                     

       dim logmsg$256

       dim new$3 
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
            * #1  ! APCPCMSD ! Pricing Master Definition File (New)     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1, "APCPCMSD",                                      ~
                        varc,     indexed,  recsize =   768,            ~
                        keypos =    1, keylen = 9                        

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),500%, rslt$(1%))

            mat f1% = zer
            upd% = 0%
            add% = 0%
            tst% = 0%
	    cnt% = 0%           
	    tbl$(01) = "311" /* list of models */
	    tbl$(02) = "312" /* list of models */
	    tbl$(03) = "378" /* list of models */
	    tbl$(04) = "313" /* list of models */
	    tbl$(05) = "373" /* list of models */
	    tbl$(06) = "314" /* list of models */
	    tbl$(07) = "332" /* list of models */
	    tbl$(08) = "388" /* list of models */
	    tbl$(09) = "333" /* list of models */
	    tbl$(10) = "383" /* list of models */
	    tbl$(11) = "334" /* list of models */
	   
REM         for c% = 0% to 9999%
L00500:     for c% = 0% to 9999%
            convert c% to c$, pic (0000)
L01000:     for l% = 1% to 11%          
	    pc_key$ = "000000"
            str(pc_key$,1,4) = c$            
            str(pc_key$,7,3) = tbl$(l%)       
            read #1,hold, key = pc_key$,hold, using L00020, pc_rec$(),     ~
                                                   eod goto L02000
            /* find i for calc_seq = 03 */
	    i = 0
            for s = 10 to 29
                get str(pc_rec$(),s,1), using L00010, x
		convert x to s$, pic (00)
                if s$ <> "03" then goto L01600
	        i = s - 9
                s = 29
L01600:     next s
            if i = 0 then L02000
REM 	    des$ = str(pc_rec$(),50,45)
            /* set calc_code(i),4,3 = "001" */
            b = 50 + ((i - 1) * 9)
            str(pc_rec$(),b+3,3) = "001"  
REM	    des$ = str(pc_rec$(),50,45)
L01800:     rewrite #1, using L00020, pc_rec$()
L02000:     next l%                                       
L03000:     next c%
L00010:      FMT BI(01)	
L00020:      FMT 3*CH(256)	
FINI:       end
	      
