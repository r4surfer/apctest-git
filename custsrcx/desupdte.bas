REM         +---------------------------------------------------------------+
REM         | copy models from catalog 0000 to list of catalogs             |
REM         +---------------------------------------------------------------+


        dim                              /* FILE = APCPLNDT            */~
            pc_rec$102,apckey$40,key$9,  /* Detail Record              */~
            rec$(3)256, new_key$40                                       

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


       dim cat$(1008)4, dis(1008), mdl$(10)3

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
            * #3  ! APCPCMSD !                                          *~
            * #2  ! GENCODES !                                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1, "APCPCMST",                                      ~
                        varc,     indexed,  recsize =   102,            ~
                        keypos =    9, keylen = 40,                     ~
                        alt key  1, keypos =   1, keylen =  8            

            select #3, "APCPCMSD",                                      ~
                        varc,     indexed,  recsize =   768,            ~
                        keypos =    1, keylen = 9                        

            select #2, "GENCODES",                                      ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),500%, rslt$(1%))
            call "OPENCHCK" (#3, fs%(3%), f2%(3%),500%, rslt$(3%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),0%, rslt$(2%))

            mat f1% = zer
            upd% = 0%
            tst% = 0%
	    cnt% = 0%           
	    init(hex(00)) apckey$

        mdl$(01) = "417"
        mdl$(02) = "B21"

        cat$(1) = "0009"
        cat$(2) = "0066"
        cat$(3) = "0074"
        cat$(4) = "0082"
        cat$(5) = "0096"
        cat$(6) = "0101"
        cat$(7) = "0139"
        cat$(8) = "0148"
        cat$(9) = "0149"
        cat$(10) = "0152"
        cat$(11) = "0159"
        cat$(12) = "0173"
        cat$(13) = "0175"
        cat$(14) = "0177"
        cat$(15) = "0179"
        cat$(16) = "0186"
        cat$(17) = "0192"
        cat$(18) = "0201"
        cat$(19) = "0204"
        cat$(20) = "0211"
        cat$(21) = "0216"
        cat$(22) = "0223"
        cat$(23) = "0228"
        cat$(24) = "0232"
        cat$(25) = "0233"
        cat$(26) = "0234"
        cat$(27) = "0237"
        cat$(28) = "0238"
        cat$(29) = "0239"
        cat$(30) = "0240"
        cat$(31) = "0244"
        cat$(32) = "0257"
        cat$(33) = "0258"
        cat$(34) = "0259"
        cat$(35) = "0270"
        cat$(36) = "0277"
        cat$(37) = "0278"
        cat$(38) = "0279"
        cat$(39) = "0281"
        cat$(40) = "0284"
        cat$(41) = "0285"
        cat$(42) = "0290"
        cat$(43) = "0312"
        cat$(44) = "0315"
        cat$(45) = "0328"
        cat$(46) = "0339"
        cat$(47) = "0340"
        cat$(48) = "0355"
        cat$(49) = "0358"
        cat$(50) = "0371"
        cat$(51) = "0381"
        cat$(52) = "0384"
        cat$(53) = "0393"
        cat$(54) = "0395"
        cat$(55) = "0404"
        cat$(56) = "0411"
        cat$(57) = "0424"
        cat$(58) = "0434"
        cat$(59) = "0451"
        cat$(60) = "0472"
        cat$(61) = "0473"
        cat$(62) = "0474"
        cat$(63) = "0478"
        cat$(64) = "0479"
        cat$(65) = "0487"
        cat$(66) = "0489"
        cat$(67) = "0491"
        cat$(68) = "0493"
        cat$(69) = "0498"
        cat$(70) = "0501"
        cat$(71) = "0503"
        cat$(72) = "0521"
        cat$(73) = "0529"
        cat$(74) = "0535"
        cat$(75) = "0537"
        cat$(76) = "0545"
        cat$(77) = "0546"
        cat$(78) = "0550"
        cat$(79) = "0551"
        cat$(80) = "0552"
        cat$(81) = "0555"
        cat$(82) = "0559"
        cat$(83) = "0562"
        cat$(84) = "0565"
        cat$(85) = "0580"
        cat$(86) = "0587"
        cat$(87) = "0588"
        cat$(88) = "0601"
        cat$(89) = "0604"
        cat$(90) = "0606"
        cat$(91) = "0607"
        cat$(92) = "0613"
        cat$(93) = "0622"
        cat$(94) = "0628"
        cat$(95) = "0637"
        cat$(96) = "0653"
        cat$(97) = "0674"
        cat$(98) = "0681"
        cat$(99) = "0696"
        cat$(100) = "0711"
        cat$(101) = "0712"
        cat$(102) = "0713"
        cat$(103) = "0717"
        cat$(104) = "0723"
        cat$(105) = "0729"
        cat$(106) = "0739"
        cat$(107) = "0749"
        cat$(108) = "0750"
        cat$(109) = "0753"
        cat$(110) = "0762"
        cat$(111) = "1017"
        cat$(112) = "1024"
        cat$(113) = "1052"
        cat$(114) = "1082"
        cat$(115) = "1113"
        cat$(116) = "1122"
        cat$(117) = "1199"
        cat$(118) = "1208"
        cat$(119) = "1226"
        cat$(120) = "1236"
        cat$(121) = "1237"
        cat$(122) = "1254"
        cat$(123) = "1269"
        cat$(124) = "1293"
        cat$(125) = "1294"
        cat$(126) = "1307"
        cat$(127) = "1311"
        cat$(128) = "1322"
        cat$(129) = "1331"
        cat$(130) = "1336"
        cat$(131) = "1359"
        cat$(132) = "1362"
        cat$(133) = "1375"
        cat$(134) = "1377"
        cat$(135) = "1402"
        cat$(136) = "1407"
        cat$(137) = "1418"
        cat$(138) = "1420"
        cat$(139) = "1421"
        cat$(140) = "1424"
        cat$(141) = "1435"
        cat$(142) = "1455"
        cat$(143) = "1460"
        cat$(144) = "1469"
        cat$(145) = "1475"
        cat$(146) = "1476"
        cat$(147) = "1477"
        cat$(148) = "1493"
        cat$(149) = "1511"
        cat$(150) = "1520"
        cat$(151) = "1531"
        cat$(152) = "1536"
        cat$(153) = "1546"
        cat$(154) = "1547"
        cat$(155) = "1550"
        cat$(156) = "1553"
        cat$(157) = "1563"
        cat$(158) = "1566"
        cat$(159) = "1571"
        cat$(160) = "1577"
        cat$(161) = "1578"
        cat$(162) = "1589"
        cat$(163) = "1608"
        cat$(164) = "1624"
        cat$(165) = "1637"
        cat$(166) = "1638"
        cat$(167) = "1649"
        cat$(168) = "1652"

             cat% = 168%
             mdl% = 02%


 	     init(" ") apckey$
             for m% = 1% to mdl%
	     str(apckey$,01,01) = "A"            /* status */
	     str(apckey$,02,04) = "0000"         /* catalog code */
	     str(apckey$,06,02) = "00"           /* catalog methode */
	     str(apckey$,08,03) = mdl$(m%)       /* model code */
	     str(apckey$,11,02) = "00"           /* ref code */
	     str(apckey$,13,03) = "000"          /* calc method */
	     str(apckey$,16,25) = "                         " /* gen key */

L01000:      /* main loop */
            read #1, key > apckey$, hold, using L50760, pc_rec$,          ~
                                                      eod goto L02000
            apckey$ = str(pc_rec$,9,40)
	    if str(apckey$,08,03) <> mdl$(m%) then L02000               
	    cnt% = cnt% + 1
            for c% = 1% to cat%
REM	        gosub L03000
	        str(pc_rec$,10,04) = cat$(c%)     /* catalog code */
                str(pc_rec$,1,8) = pc_ref$
		read #1, hold, using L50755, new_key$, ~
	  	     eod goto L01500
                str(pc_rec$,9,40) = new_key$
                rewrite #1, using L50760, pc_rec$, ~
	  	     eod goto L01500
L01500:
	    next c%
            goto L01000

L02000:     next m%
            goto part2

L03000: /* get new ref # */
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

part2:
           for m% = 1% to mdl%
           str(key$,1,4) = "0000"
           str(key$,5,2) = "00"
           str(key$,7,3) = mdl$(m%)
L03200:    read #3, key = key$, hold, using L50770, rec$(),          ~
                                                      eod goto L04000
	   cnt% = cnt% + 1
           for c% = 1% to cat%
		key2$ = key$
	        str(key2$,1,4) = cat$(c%)     /* catalog code */
                read #3, hold,  key = key2$, hold, using L50775,key2$,    ~
                                                      eod goto L03500
L50775:     FMT CH(9)
	        str(rec$(),1,4) = cat$(c%)     /* catalog code */
                rewrite #3, using L50770, rec$()
L03500:    next c%
L04000:    next m%

L50755:     FMT POS(9), CH(40)
L50760:     FMT CH(102)
L50770:     FMT 3*CH(256)
FINI:       print "         records read    = ", cnt%
            print "         records test    = ", tst%
            print "         records updated = ", upd%
             end
	      
gencodes_data_err:
            print "*** ERROR *** problem updateing GENCODES"
	      stop
