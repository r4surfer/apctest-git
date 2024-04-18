REM         +---------------------------------------------------------------+
REM         | copy model 312 with ref 27/28 to 305                          |
REM         +---------------------------------------------------------------+


        dim                              /* FILE = APCPLNDT            */~
            pc_rec$102,apckey$40         /* Detail Record              */

        dim f2%(40%),                    /* = 0 if the file is open    */~
            f1%(40%),                    /* = 1 if READ was successful */~
            fs%(40%),                    /* = 1 if file open, -1 if it */~
            new_key$40,                  /*   doesn't exist, or 0 if   */~
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
	    old$(01)= "413" 
	    old$(02)= "999" 
	    new$(01)= "E21" 
	    new$(02)= "C12" 
	    new$(03)= "C18" 
	    new$(04)= "999"
	    init(hex(00)) apckey$

L01000:      /* main loop */
	     init(" ") pc_key$
            read #1, key > apckey$, hold, using L50760, pc_rec$,          ~
                                                      eod goto L56890
	    cnt% = cnt% + 1
            apckey$ = str(pc_rec$,9,40)
            Model$ = str(pc_rec$,16,3)
            Ref$ = str(pc_rec$,19,2)
            Meth$ = str(pc_rec$,21,3)
            cat$  = str(pc_rec$,10,4)
REM         if (Ref$ <> "27" and Ref$ <> "28") then goto L01000
REM         if (Meth$ <> "000") then goto L01000
            if cat$ <= "0000" then L01000
            if Model$ <> "413" then goto L01000
            if Ref$ = "27" and Meth$ = "000" then L02000
            if Ref$ = "01" and Meth$ = "005" then L02000
            if Ref$ = "01" and Meth$ = "006" then L02000
	    goto L01000
L01050:                                           
REM	    print at (10,20), hex(84), Model$
REM	    for l = 1 to 1
REM             if Model$ = old$(l) then goto L02000
REM	    next l
	    goto L01000

L02000:
         for l = 1 to 3
            str(pc_rec$,16,3) = new$(l)
	    new_key$ = str(pc_rec$,9,40)
REM	    print at (10,10), hex(84), new_key$
            gosub L02200                               
         next l
         goto L01000

L02200:
            read #1, key 1 = new_key$, eod goto L02500
	    return
L02500:
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

REM            str(pc_rec$,16,3) = new$(1)
            str(pc_rec$,01,8) = pc_ref$
	    str(pc_rec$,9,40) = new_key$
            tst% = tst% + 1%
	    print at (10,10), hex(84), new_key$
	    print at (12,10), hex(84), pc_ref$ 
            write #1, using L50760, pc_rec$, ~
		     eod goto L02700
	    upd% = upd% + 1
L02700:
            return           

L50760:     FMT CH(102)
L56890:     print "         records read    = ", cnt%
            print "         records test    = ", tst%
            print "         records updated = ", upd%
             end
	      
gencodes_data_err:
            print "*** ERROR *** problem updateing GENCODES"
	      stop
