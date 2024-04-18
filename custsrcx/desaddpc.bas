REM         +---------------------------------------------------------------+
REM         | copy model B50 catelog 000 to all others.                     |
REM         +---------------------------------------------------------------+


        dim                              /* FILE = APCPLNDT            */~
            pc_rec$(3)256         /* Detail Record              */

        dim f2%(40%),                    /* = 0 if the file is open    */~
            f1%(40%),                    /* = 1 if READ was successful */~
            fs%(40%),                    /* = 1 if file open, -1 if it */~
            pc_key$9,                    /*   doesn't exist, or 0 if   */~
            sv_key$9,                    /*   doesn't exist, or 0 if   */~
            rslt$(40%)20                 /* Text from file opening     */ 
            
            

        dim                              /* (AWD083)                   */~
            Model$3,                     /* Calling Program Flag       */~
            tbl$(85)3                                                     

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
            * #2  ! CATELOGS ! List of different catelogs               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1, "APCPCMSD",                                      ~
                        varc,     indexed,  recsize =   768,            ~
                        keypos =    1, keylen = 9                        

            select #2, "CATELOGS",                                      ~
                        varc,     indexed,  recsize =  4,              ~
                        keypos =    1, keylen =  4

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),500%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),0%, rslt$(2%))

            mat f1% = zer
            upd% = 0%
            add% = 0%
            tst% = 0%
	    cnt% = 0%           

        tbl$(01) = "103"
        tbl$(02) = "113"
        tbl$(03) = "123"
        tbl$(04) = "133"

L01000:      /* main loop */
            read #2, using L00010, cat$, eod goto FINI
	    if cat$ = "0000" then goto L01000
	     init(" ") pc_key$
	    cnt% = cnt% + 1

            l = 1                  
L01100:
	        pc_key$ = "000000"
                str(pc_key$,7,3) = tbl$(l)          
                read #1, key = pc_key$,hold, using L00020, pc_rec$(),          ~
                                                   eod goto L01200
                sv_key$ = pc_key$
                str(sv_key$,7,3) = tbl$(l)        
	        str(sv_key$,1,4) = cat$
	        str(sv_key$,5,2) = "00"  
                str(pc_rec$(),7,3) = tbl$(l)        
	        str(pc_rec$(),1,4) = cat$
	        str(pc_rec$(),5,2) = "00"
		gosub update_rec
		l = l + 1
		if l < 5 then goto L01100
L01200:
            goto L01000

update_rec:
                read #1,hold, key = sv_key$,hold, using L00030, tmp_key$,      ~
                                                   eod goto L01500
                goto L01800

L01500:
            write #1, using L00020, pc_rec$()
            add% = add% + 1%
            goto L02000
L01800:     rewrite #1, using L00020, pc_rec$()
	    upd% = upd% + 1%
L02000:     return

L00010:      FMT CH(04)
L00020:      FMT 3*CH(256)	
L00030:      FMT CH(09)  
FINI:       print "         records read    = ", cnt%
            print "         records added   = ", add%
            print "         records updated = ", upd%
             end
	      
