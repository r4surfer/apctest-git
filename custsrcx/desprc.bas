REM         +---------------------------------------------------------------+
REM         | create new model records                                      |
REM         +---------------------------------------------------------------+


        dim                              /* FILE = APCPLNDT            */~
            pc_rec$102,apckey$40         /* Detail Record              */

        dim f2%(40%),                    /* = 0 if the file is open    */~
            f1%(40%),                    /* = 1 if READ was successful */~
            fs%(40%),                    /* = 1 if file open, -1 if it */~
            pc_ref$8,                    /*   doesn't exist, or 0 if   */~
            r_rec$30, readkey$50,        /*   not yet checked (OPENCHCK*/~
            cat_key$4,des$40,            /*   not yet checked (OPENCHCK*/~
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
            * #3  ! CATEGODS !                                          *~
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

            select #3, "CATELOGS",                                      ~
                        varc,     indexed,  recsize =  4,              ~
                        keypos =    1, keylen =  4

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),500%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),0%, rslt$(2%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),0%, rslt$(3%))

            mat f1% = zer
            upd% = 0%
            tst% = 0%
	    cnt% = 0%           
	    new$(01)= "103" 
	    new$(02)= "113" 
	    new$(03)= "123" 
	    new$(04)= "133" 
	    new$(05)= "999"
	    init(hex(00)) apckey$

L01000:      /* main loop */
	     init(" ") pc_key$
             str(pc_rec$,01,08) = "00000000" /* ref # from PRICE 000 */
             str(pc_rec$,09,01) = "A"        /* status code          */
             str(pc_rec$,10,04) = "0000"     /* catelog code         */
             str(pc_rec$,14,02) = "00"       /* pricing method       */
             str(pc_rec$,16,03) = "103"      /* pricing model        */
             str(pc_rec$,19,02) = "27"       /* pricing ref code     */
             str(pc_rec$,21,03) = "000"      /* pricing ref calc     */
             str(pc_rec$,24,25) = " "        /* pricing gen key      */
             str(pc_rec$,49,08) = hex(000000000005200F)                
             str(pc_rec$,57,08) = hex(000000000000000F)                
             str(pc_rec$,65,08) = hex(000000000000000F)                
             str(pc_rec$,73,08) = hex(000000000000000F)                
             str(pc_rec$,81,08) = hex(000000000000000F)                
             str(pc_rec$,89,08) = hex(000000000000000F)                
             str(pc_rec$,97,03) = "000"      /* special calc prod    */
             str(pc_rec$,100,3) = "   "      /* filler               */

            cat_key$ = "    "
next_cat:         
            read #3,key > cat_key$,using cat_fmt,cat_key$, eod goto L56890
cat_fmt:    FMT CH(4)
REM         if cat_key$ < "0741" then next_cat 
REM         if cat_key$ > "0749" then L56890   
   	    for l = 1 to 4
                gosub L02000
	    next l

	    goto next_cat 

L02000:
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
skip_xref:
            str(pc_rec$,10,04) = cat_key$                                     
            str(pc_rec$,16,03) = new$(l)
	    apckey$ = str(pc_rec$,9,40)
	    des$    = str(pc_rec$,9,40)
            read #1,hold,key >= apckey$,using key_fmt, apckey$, eod goto notfnd
key_fmt:    FMT POS(9),CH(40)
	    if str(apckey$,1,15) <> str(pc_rec$,9,15) then notfnd
            delete #1

notfnd:
	    apckey$ = str(pc_rec$,9,40)
            str(pc_rec$,01,08) = pc_ref$
            tst% = tst% + 1%
            write #1, using L50760, pc_rec$, ~
		     eod goto L56890
	    upd% = upd% + 1
            return            

L50760:     FMT CH(102)
L56890:     print "         records read    = ", cnt%
            print "         records test    = ", tst%
            print "         records updated = ", upd%
             end
	      
gencodes_data_err:
            print "*** ERROR *** problem updateing GENCODES"
	      stop
