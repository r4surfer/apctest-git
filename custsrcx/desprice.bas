REM         +--------------------------------------------------------------+
REM         | increase price for ref 27 except selected catelogs.          |
REM         +--------------------------------------------------------------+


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
            skip$(45)4,                  /* Calling Program Flag       */~
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

            select #3, "DESMODEL",                                      ~
                        varc,  consec,  recsize =  40 
            call "OPENCHCK" (#1, fs%(1%), f2%(1%),500%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),0%, rslt$(2%))
REM            call "OPENFILE" (#3, "OUTPT", f2%(3%), rslt$(3%), axd$(3))

            mat f1% = zer
            upd% = 0%
            tst% = 0%
	    cnt% = 0%           
	    init(hex(00)) apckey$
	    skip$(01) = "0002"
	    skip$(02) = "0010"
	    skip$(03) = "0022"
	    skip$(04) = "0066"
	    skip$(05) = "0093"
	    skip$(06) = "0180"
	    skip$(07) = "0259"
	    skip$(08) = "0277"
	    skip$(09) = "0290"
	    skip$(10) = "0291"
	    skip$(11) = "0305"
	    skip$(12) = "0312"
	    skip$(13) = "0327"
	    skip$(14) = "0339"
	    skip$(15) = "0348"
	    skip$(16) = "0353"
	    skip$(17) = "0363"
	    skip$(18) = "0407"
	    skip$(19) = "0418"
	    skip$(20) = "0426"
	    skip$(21) = "0430"
	    skip$(22) = "0431"
	    skip$(23) = "0432"
	    skip$(24) = "0433"
	    skip$(25) = "0434"
	    skip$(26) = "0435"
	    skip$(27) = "0437"
	    skip$(28) = "0438"
	    skip$(29) = "1039"
	    skip$(30) = "1064"
	    skip$(31) = "1082"
	    skip$(32) = "1125"
	    skip$(33) = "1194"
	    skip$(34) = "1219"
	    skip$(35) = "1227"
	    skip$(36) = "1255"
	    skip$(37) = "1380"
	    skip$(38) = "1402"
	    skip$(39) = "1432"
	    skip$(40) = "1453"
	    skip$(41) = "1506"
	    skip$(42) = "1532"
	    skip$(43) = "1598"
	    skip$(44) = "1678"


L01000:      /* main loop */
	     init(" ") pc_key$
            read #1, key > apckey$,hold, using L50760, pc_rec$,          ~
                                                   eod goto L56890
	    cnt% = cnt% + 1
            apckey$ = str(pc_rec$,9,40)
            Model$ = str(pc_rec$,16,3)
            Ref$ = str(pc_rec$,19,2)
            get str(pc_rec$,49,8), using GETPD, discount 
GETPD:   FMT PD(14,4)
	    /* is not active skip */
            if (str(pc_rec$,9,1) > "A") then goto L50760	    
            if (str(pc_rec$,9,1) < "A") then goto L01000	    

	    /* is not ref 27 skip */
            if Ref$ <> "27" then goto L01000

	    /* skip catelogs in spread sheet */
checkit:    for l% = 01% to 44%  
	    if str(pc_rec$,10,4) = skip$(l%) then goto L01000
	    next l%

            /* incrament price */
            get str(pc_rec$,49,8), using GETPD, discount 
            discount = discount + .02
	    upd% = upd% + 1%
            put str(pc_rec$,49,8), using GETPD, discount 
           rewrite #1, using L50760, pc_rec$          
            goto L01000


L50760:     FMT CH(102)
L56890:     print "         records read    = ", cnt%
            print "         records test    = ", tst%
            print "         records updated = ", upd%
             end
	      
gencodes_data_err:
            print "*** ERROR *** problem updateing GENCODES"
	      stop
