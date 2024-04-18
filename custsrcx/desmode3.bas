REM         +---------------------------------------------------------------+
REM         | copy model 210, 213 & 214 to 211                              |
REM         +---------------------------------------------------------------+


        dim                              /* FILE = APCPLNDT            */~
            pc_rec$102,apckey$40, key$40 /* Detail Record              */
        dim tmp_rec$102
        dim f2%(40%),                    /* = 0 if the file is open    */~
            f1%(40%),                    /* = 1 if READ was successful */~
            fs%(40%),                    /* = 1 if file open, -1 if it */~
            pc_ref$8,                    /*   doesn't exist, or 0 if   */~
            r_rec$30, readkey$50,        /*   not yet checked (OPENCHCK*/~
            rslt$(40%)20                 /* Text from file opening     */ 
            
        dim tbl$(10,5)3



            

        dim                              /* (AWD083)                   */~
            Ref$2,                       /* Calling Program Flag       */~
            Model$3,                     /* Calling Program Flag       */~
            Calc$3                                                        

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

            tbl$(01,1) = "9A2"
            tbl$(01,2) = "9A1"
            tbl$(01,3) = "9A1"
            tbl$(01,4) = "9A3"
            tbl$(01,5) = "9A3"
            tbl$(02,1) = "9A6"
            tbl$(02,2) = "9A5"
            tbl$(02,3) = "9A5"
            tbl$(02,4) = "9A7"
            tbl$(02,5) = "9A7"
            tbl$(03,1) = "9B0"
            tbl$(03,2) = "9A9"
            tbl$(03,3) = "9A9"
            tbl$(03,4) = "9B1"
            tbl$(03,5) = "9B1"
            tbl$(04,1) = "9B4"
            tbl$(04,2) = "9B3"
            tbl$(04,3) = "9B3"
            tbl$(04,4) = "9B5"
            tbl$(05,5) = "9B5"
            tbl$(05,1) = "9B8"
            tbl$(05,2) = "9B7"
            tbl$(05,3) = "9B7"
            tbl$(05,4) = "9B9"
            tbl$(05,5) = "9B9"
            tbl$(06,1) = "928"
            tbl$(06,2) = "928"
            tbl$(06,3) = "927"
            tbl$(06,4) = "9C0"
            tbl$(06,5) = "9C0"
            tbl$(07,1) = "929"
            tbl$(07,2) = "932"
            tbl$(07,3) = "931"
            tbl$(07,4) = "9C2"
            tbl$(07,5) = "9C2"
            tbl$(08,1) = "933"
            tbl$(08,2) = "936"
            tbl$(08,3) = "935"
            tbl$(08,4) = "9C4"
            tbl$(08,5) = "9C4"
            tbl$(09,1) = "937"
            tbl$(09,2) = "940"
            tbl$(09,3) = "939"
            tbl$(09,4) = "9C6"
            tbl$(09,5) = "9C6"
            tbl$(10,1) = "941"
            tbl$(10,2) = "944"
            tbl$(10,3) = "943"
            tbl$(10,4) = "9C8"
            tbl$(10,5) = "9C8"


            mat f1% = zer
            sw%  = 1%
            upd% = 0%
            tst% = 0%
	    cnt% = 0%           
	    init(hex(00)) apckey$
	    str(apckey$,1,1) = "A"

L01000:      /* main loop */
            read #1, key > apckey$,hold, using L50760, pc_rec$,          ~
                                                   eod goto L56890
	    cnt% = cnt% + 1
	    tmp_rec$ = pc_rec$
            apckey$ = str(pc_rec$,9,40)
            if (str(apckey$,1,1) <> "A") then goto L50760	    
REM	    if (str(apckey$,2,4) = "0000") then goto L01000 
            Model$ = str(apckey$,8,3)
REM         if tbl$(1-10,2-5) = model$, roll to tbl$(1-10,1)
            for x% = 1% to 10%
	    y% = x%
	    if (Model$ = tbl$(x%,2)) then gosub process 
	    if (Model$ = tbl$(x%,3)) then gosub process 
	    if (Model$ = tbl$(x%,4)) then gosub process 
	    if (Model$ = tbl$(x%,5)) then gosub process 
	    next x%
	    goto L01000

process:    key$ = apckey$
/* get new ref # */
            if sw% = 0% then goto skip_ref
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
            sw%  = 0%
skip_ref:  
            str(pc_rec$,16,3) = tbl$(y%,1%)
            str(pc_rec$,01,8) = pc_ref$
	    tmp_rec$ = pc_rec$
            key$ = str(pc_rec$,9,40)
            tst% = tst% + 1%
            read #1, key = key$,hold, using L50760, pc_rec$,          ~
			 eod goto write_new              
            return             

write_new:                                           
            write #1, using L50760, tmp_rec$, ~
		     eod goto L03000    
            sw%  = 1%
	    upd% = upd% + 1
L03000:     return         

L50760:     FMT CH(102)
L56890:     print "         records read    = ", cnt%
            print "         records test    = ", tst%
            print "         records updated = ", upd%
             end
	      
gencodes_data_err:
            print "*** ERROR *** problem updateing GENCODES"
	      stop
