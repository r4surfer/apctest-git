REM         +---------------------------------------------------------------+
REM         | copy model 353 with ref 27/28 to 354                          |
REM         +---------------------------------------------------------------+


        dim                              /* FILE = APCPLNDT            */~
            pc_rec$102,apckey$40         /* Detail Record              */

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
	    old$(01)= "353" 
	    old$(02)= "363" 
	    old$(03)= "999" 
	    new$(01)= "351" 
	    new$(02)= "390"
	    new$(03)= "392"
	    new$(04)= "361"
	    new$(05)= "394"
	    new$(06)= "396"
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
REM         if Model$ = old$(1) then goto L02000
   	    for l = 1 to 2
                if Model$ = old$(l) then goto L02000
   	    next l
	    goto L01000

L02000:

REM         str(pc_rec$,16,3) = new$(l)
            x = ((l - 1) * 3) + 1
            str(pc_rec$,16,3) = new$(x)
	    gosub get_ref
            str(pc_rec$,01,8) = pc_ref$
            tst% = tst% + 1%
            write #1, using L50760, pc_rec$, ~
		     eod goto L01000
	    upd% = upd% + 1
	    x = x + 1
            str(pc_rec$,16,3) = new$(x)
	    gosub get_ref
            str(pc_rec$,01,8) = pc_ref$
            tst% = tst% + 1%
            write #1, using L50760, pc_rec$, ~
		     eod goto L01000
	    upd% = upd% + 1
	    x = x + 1
            str(pc_rec$,16,3) = new$(x)
	    gosub get_ref
            str(pc_rec$,01,8) = pc_ref$
            tst% = tst% + 1%
            write #1, using L50760, pc_rec$, ~
		     eod goto L01000
	    upd% = upd% + 1
            goto L01000

L50760:     FMT CH(102)
L56890:     print "         records read    = ", cnt%
            print "         records test    = ", tst%
            print "         records updated = ", upd%
             end
	      
get_ref:
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
