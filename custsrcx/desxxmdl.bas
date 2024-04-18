REM         +---------------------------------------------------------------+
REM         | copy model 559 with ref 27/28 to B50-B55                      |
REM         +---------------------------------------------------------------+


        dim                              /* FILE = APCPLNDT            */~
            pc_rec$102,apckey$40         /* Detail Record              */
        dim tstkey$40         /* Detail Record              */

        dim f2%(40%),                    /* = 0 if the file is open    */~
            f1%(40%),                    /* = 1 if READ was successful */~
            fs%(40%),                    /* = 1 if file open, -1 if it */~
            pc_ref$8,                    /*   doesn't exist, or 0 if   */~
            r_rec$30, readkey$50,        /*   not yet checked (OPENCHCK*/~
            rslt$(40%)20                 /* Text from file opening     */ 
            
            

        dim                              /* (AWD083)                   */~
            Ref$2,                       /* Calling Program Flag       */~
            Model$3,                     /* Calling Program Flag       */~
            Calc$3                                                        

       dim old$(55)3,new$(55)3 
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
            * #1  ! APCPCMST ! Pricing Master Definition File (New)     *~
            * #2  ! GENCODES !                                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1, "APCPCMST",                                      ~
                        varc,     indexed,  recsize =   102,            ~
                        keypos =    9, keylen = 40,                     ~
                        alt key  1, keypos =   1, keylen =  8            

            select #2, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),500%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),0%, rslt$(2%))

            mat f1% = zer
            upd% = 0%
            tst% = 0%
	    cnt% = 0%           
	    old$(01)= "312" 
	    old$(02)= "332"  
	    old$(03)= "353" 
	    old$(04)= "363" 
	    old$(05)= "999" 
	    old$(06)= "999" 
	    old$(07)= "999" 
	    new$(01)= "378" 
	    new$(02)= "388" 
	    new$(03)= "358" 
	    new$(04)= "368" 
	    new$(05)= "999" 
	    new$(06)= "999" 
	    new$(07)= "999"
	    init(hex(00)) apckey$

L01000:      /* main loop */
	     init(" ") pc_key$
            read #1, key > apckey$, hold, using L50760, pc_rec$,          ~
                                                      eod goto L56890
	    cnt% = cnt% + 1
            apckey$ = str(pc_rec$,9,40)
            Model$ = str(pc_rec$,16,3)
            Ref$ = str(pc_rec$,19,2)
            if (Ref$ <> "27" and Ref$ <> "28") then goto L01000
            if Model$ = old$(1) then goto L02000
	    goto L01000

L02000:
            l = 1
LOOP:                  
	    gosub NEW_REF
            str(pc_rec$,16,3) = new$(l)
            str(pc_rec$,01,8) = pc_ref$
	    tstkey$ = str(pc_rec$,9,40)
            read #1, key = tstkey$, hold, using L50760, pc_rec$,          ~
			 eod goto L50400:        
            goto L50500
L50400:
            tst% = tst% + 1%
            write #1, using L50760, pc_rec$       
	    upd% = upd% + 1
L50500:	    l = l + 1                         
       	    if l <=  4 then  goto LOOP
            goto L01000

L50760:     FMT CH(102)
L56890:     print "         records read    = ", cnt%
            print "         records test    = ", tst%
            print "         records updated = ", upd%
             end
	      
NEW_REF:
/* get new ref # */
            init(" ") readkey$
	    str(readkey$,1,9) = "PRICE 000"
	    str(readkey$,10,15) = "0000"
	    read #2,hold,key = readkey$, using GENCODES, r_rec$,    ~
			 eod goto gencodes_data_err
GENCODES: FMT POS(25), CH(30)
            convert str(r_rec$,21,8) to pc_ref%, data goto gencodes_data_err
	    convert pc_ref% to pc_ref$, pic(00000000)
	    pc_ref% = pc_ref% + 1%
	    convert pc_ref% to str(r_rec$,21,8), pic(00000000)
	    put #2, using GENCODES, r_rec$
	    rewrite #2
	    return

gencodes_data_err:
            print "*** ERROR *** problem updateing GENCODES"
	      stop
