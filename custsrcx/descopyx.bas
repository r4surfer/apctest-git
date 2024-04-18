REM         +---- This is a test2 ------------------------------------------+
REM         +---------------------------------------------------------------+
REM         | copy date from old to new models with ref 27/28               |
REM         +---------------------------------------------------------------+


        dim                              /* FILE = APCPLNDT            */~
            pc_rec$102,key$40,key2$40,pc_rec2$102                        

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


       dim old$(49)3,new$(49)3                    

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
	    init(hex(00)) key$

        old$(1) = "D27"
        old$(2) = "D29"
        old$(3) = "D37"
        new$(1) = "D28"
        new$(2) = "D30"
        new$(3) = "D38"

        cat% = 3%


 	     init(" ") key$
	     str(key$,01,01) = "A"            /* status */
	     str(key$,02,04) = "0000"         /* catalog code */
	     str(key$,06,02) = "00"           /* catalog methode */
	     str(key$,08,03) = "000"          /* model code */
	     str(key$,11,02) = "27"           /* ref code */
	     str(key$,13,03) = "000"          /* calc method */
	     str(key$,16,25) = "                         " /* gen key */

            message$ = "read   " & key$
REM         call "LOGFILE" (message$)

L01000:      /* main loop */
            read #1, key > key$, using L50760, pc_rec$, eod goto L56890
	    cnt% = cnt% + 1
            key$ = str(pc_rec$,9,40)
            key2$ = str(pc_rec$,9,40)
	    if str(key$,13,03) <> "000"  then goto L01000             
	    if str(key$,11,02) <> "27" and     ~  
	       str(key$,11,02) <> "28" then goto L01000               


            for l% = 1% to cat%
	        str(key2$,08,03) = new$(l%)                       
	        if str(key$,08,03) = old$(l%) then goto L01500    
            next l%
            goto L01000

L01500:
            read #1, key = key2$, using L50760, pc_rec2$, eod goto L01600
	    goto L01000
REM         read #1, key = key2$, hold, using L50760, pc_rec2$, eod goto L01600
REM         str(pc_rec$,1,48) = str(pc_rec2$,1,48)
REM         rewrite #1, using L50760, pc_rec$
REM         message$ = "change " & key2$ 
REM         call "LOGFILE" (message$)
REM	    goto L01000

L01600:
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
            message$ = "inc " & pc_ref$   
            call "LOGFILE" (message$)
	    rewrite #2
            str(pc_rec$,1,8) = pc_ref$
            str(pc_rec$,9,40) = key2$
            write #1, using L50760, pc_rec$
            message$ = "add " & pc_ref$ & "-" & key2$  
            call "LOGFILE" (message$)
	    goto L01000

L50760:     FMT CH(102)
L56890:     print "         records read    = ", cnt%
            print "         records test    = ", tst%
            print "         records updated = ", upd%
             end
	      
gencodes_data_err:
            print "*** ERROR *** problem updateing GENCODES"
	      stop
