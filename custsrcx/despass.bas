REM         +---------------------------------------------------------------+
REM         | add clay to apcpcmsk                                          |
REM         +---------------------------------------------------------------+


        dim                              /* FILE = APCPLNDT            */~
            key$5, key2$40,         /* Detail Record              */~
            rec$64, rec2$102,                                       ~
	    key_tbl$(15)5, message$256

        dim f2%(40%),                    /* = 0 if the file is open    */~
            f1%(40%),                    /* = 1 if READ was successful */~
            fs%(40%),                    /* = 1 if file open, -1 if it */~
            sw_tbl(40),                  /*   doesn't exist, or 0 if   */~
            s6_tbl(40),                  /*   not yet checked (OPENCHCK*/~
            mdl_tbl$(140),               /*   not yet checked (OPENCHCK*/~
            rslt$(40%)20                 /* Text from file opening     */ 
            
            


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$41, pname$32, apc1$41                   /* (EWD055) */
                                                            /* (EWD060) */
            dim fv1$3, fv2$3, fv3$3, fv4$3, fv5$3, fv6$3                  

        REM *************************************************************

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
            * #1  ! APCPCMSK !                                          *~
            * #2  ! APCPCMST !                                          *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************
            select #1, "APCPCMSK",                                       ~
                        varc,     indexed,  recsize = 64,                ~
                        keypos =    1, keylen =   25                     
            select #2, "APCPCMST",                                       ~
                        varc,     indexed,  recsize = 102,               ~
                        keypos =    9, keylen =   40,                    ~
			alt key 1, keypos =  1, keylen = 8

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),0%, rslt$(1%))
            call "OPENCHCK" (#2, fs%(2%), f2%(2%),0%, rslt$(2%))

            mat f1% = zer

	     init(" ") key$
	     key_tbl$(01) = "01027"
	     key_tbl$(02) = "01002"
	     key_tbl$(03) = "01003"
	     key_tbl$(04) = "01004"
	     key_tbl$(05) = "02000"
	     key_tbl$(06) = "01025"
	     key_tbl$(07) = "01026"
	     key_tbl$(08) = "01001"
	     key_tbl$(09) = "01028"
	     key_tbl$(10) = "01030"
	     key_tbl$(11) = "01032"
	     key_tbl$(12) = "01033"
	     key_tbl$(13) = "01008"
	     key_tbl$(14) = "01029"


            mdl_tbl$(1) = "837"
            mdl_tbl$(2) = "829"
            mdl_tbl$(3) = "164"
            mdl_tbl$(4) = "166"
            mdl_tbl$(5) = "167"
            mdl_tbl$(6) = "145"
            mdl_tbl$(7) = "146"
            mdl_tbl$(8) = "831"
            mdl_tbl$(9) = "832"
            mdl_tbl$(10) = "833"
            mdl_tbl$(11) = "834"
            mdl_tbl$(12) = "835"
            mdl_tbl$(13) = "811"
            mdl_tbl$(14) = "812"
            mdl_tbl$(15) = "807"
            mdl_tbl$(16) = "813"
            mdl_tbl$(17) = "808"
            mdl_tbl$(18) = "809"
            mdl_tbl$(19) = "051"
            mdl_tbl$(20) = "052"
            mdl_tbl$(21) = "053"
            mdl_tbl$(22) = "054"
            mdl_tbl$(23) = "055"
            mdl_tbl$(24) = "056"
            mdl_tbl$(25) = "057"
            mdl_tbl$(26) = "058"
            mdl_tbl$(27) = "059"
            mdl_tbl$(28) = "060"
            mdl_tbl$(29) = "050"
            mdl_tbl$(30) = "511"
            mdl_tbl$(31) = "512"
            mdl_tbl$(32) = "521"
            mdl_tbl$(33) = "531"
            mdl_tbl$(34) = "522"
            mdl_tbl$(35) = "532"
            mdl_tbl$(36) = "525"
            mdl_tbl$(37) = "535"
            mdl_tbl$(38) = "526"
            mdl_tbl$(39) = "536"
            mdl_tbl$(40) = "126"
            mdl_tbl$(41) = "136"
            mdl_tbl$(42) = "551"
            mdl_tbl$(43) = "501"
            mdl_tbl$(44) = "553"
            mdl_tbl$(45) = "555"
            mdl_tbl$(46) = "557"
            mdl_tbl$(47) = "559"
            mdl_tbl$(48) = "561"
            mdl_tbl$(49) = "562"
            mdl_tbl$(50) = "114"
            mdl_tbl$(51) = "519"
            mdl_tbl$(52) = "529"
            mdl_tbl$(53) = "539"
            mdl_tbl$(54) = "115"
            mdl_tbl$(55) = "116"
            mdl_tbl$(56) = "513"
            mdl_tbl$(57) = "514"
            mdl_tbl$(58) = "523"
            mdl_tbl$(59) = "533"
            mdl_tbl$(60) = "524"
            mdl_tbl$(61) = "534"
            mdl_tbl$(62) = "527"
            mdl_tbl$(63) = "537"
            mdl_tbl$(64) = "528"
            mdl_tbl$(65) = "538"
            mdl_tbl$(66) = "127"
            mdl_tbl$(67) = "137"
            mdl_tbl$(68) = "552"
            mdl_tbl$(69) = "502"
            mdl_tbl$(70) = "554"
            mdl_tbl$(71) = "556"
            mdl_tbl$(72) = "558"
            mdl_tbl$(73) = "560"
            mdl_tbl$(74) = "567"
            mdl_tbl$(75) = "568"
            mdl_tbl$(76) = "118"
            mdl_tbl$(77) = "028"
            mdl_tbl$(78) = "029"
            mdl_tbl$(79) = "312"
            mdl_tbl$(80) = "313"
            mdl_tbl$(81) = "314"
            mdl_tbl$(82) = "315"
            mdl_tbl$(83) = "316"
            mdl_tbl$(84) = "353"
            mdl_tbl$(85) = "319"
            mdl_tbl$(86) = "340"
            mdl_tbl$(87) = "332"
            mdl_tbl$(88) = "333"
            mdl_tbl$(89) = "334"
            mdl_tbl$(90) = "335"
            mdl_tbl$(91) = "336"
            mdl_tbl$(92) = "363"
            mdl_tbl$(93) = "337"
            mdl_tbl$(94) = "338"
            mdl_tbl$(95) = "373"
            mdl_tbl$(96) = "383"
            mdl_tbl$(97) = "449"
            mdl_tbl$(98) = "467"
            mdl_tbl$(99) = "459"
            mdl_tbl$(100) = "468"
            mdl_tbl$(101) = "469"
            mdl_tbl$(102) = "498"
            mdl_tbl$(103) = "128"
            mdl_tbl$(104) = "427"
            mdl_tbl$(105) = "138"
            mdl_tbl$(106) = "437"
            mdl_tbl$(107) = "484"
            mdl_tbl$(108) = "584"
            mdl_tbl$(109) = "485"
            mdl_tbl$(110) = "486"
            mdl_tbl$(111) = "487"
            mdl_tbl$(112) = "488"
            mdl_tbl$(113) = "461"
            mdl_tbl$(114) = "471"
            mdl_tbl$(115) = "481"
            mdl_tbl$(116) = "417"
            mdl_tbl$(117) = "479"
            mdl_tbl$(118) = "489"
            mdl_tbl$(119) = "B11"
            mdl_tbl$(120) = "B12"
            mdl_tbl$(121) = "B13"
            mdl_tbl$(122) = "B32"
            mdl_tbl$(123) = "B33"
            mdl_tbl$(124) = "B40"
            mdl_tbl$(125) = "B41"
            mdl_tbl$(126) = "B42"
            mdl_tbl$(127) = "B43"
            mdl_tbl$(128) = "B44"
            mdl_tbl$(129) = "B45"
            mdl_tbl$(130) = "B01"
            mdl_tbl$(131) = "B02"
            mdl_tbl$(132) = "B03"
            mdl_tbl$(133) = "B21"
            mdl_tbl$(134) = "B22"
            mdl_tbl$(135) = "B23"
            mdl_tbl$(136) = "B60"
            mdl_tbl$(137) = "B61"

	     x = 1

L01000:      /* main loop */
             key$ = key_tbl$(x)
             read #1, key = key$, using L50760, rec$,             ~
                                        eod goto END_JOB

             fv1$ = str(rec$,30,1) 
             fv2$ = str(rec$,33,1) 
             fv3$ = str(rec$,36,1) 
             fv4$ = str(rec$,39,1) 
             fv5$ = str(rec$,42,1) 
             fv6$ = str(rec$,45,1) 

	     fv = 0
             if fv1$ = "6" then fv = 1                     
	     if fv > 0 then goto L03000
             if fv2$ = "6" then fv = 2                     
	     if fv > 0 then goto L03000
             if fv3$ = "6" then fv = 3                     
	     if fv > 0 then goto L03000
             if fv4$ = "6" then fv = 4                     
	     if fv > 0 then goto L03000
             if fv5$ = "6" then fv = 5                     
	     if fv > 0 then goto L03000
             if fv6$ = "6" then fv = 6                     

L03000:
	     if fv < 1 then goto L05000

	     sw = 0
             if fv1$ = "7" then sw = 1                     
	     if sw > 0 then goto L04000
             if fv2$ = "7" then sw = 2                     
	     if sw > 0 then goto L04000
             if fv3$ = "7" then sw = 3                     
	     if sw > 0 then goto L04000
             if fv4$ = "7" then sw = 4                     
	     if sw > 0 then goto L04000
             if fv5$ = "7" then sw = 5                     
	     if sw > 0 then goto L04000
             if fv6$ = "7" then sw = 6                     

L04000:
	     if sw < 1 then goto L05000
	 sw_tbl(x) = sw 
	 s6_tbl(x) = fv 
L05000:
             x = x + 1
             if x < 15 then goto L01000

L50760:     FMT CH(64)                                          
L50770:     FMT CH(102)                                          
REM  --------- now cycle through apcpcmst and check sw_tbl & s6_tbl
             model$  = "   "
             refcal$ = "     "
L60000:   
             y = 0
L60250:   
	     z = 0
L60500:   
             read #2, hold, using L50770, rec2$,             ~
                                        eod goto END_JOB
             
             model$  = str(rec2$,16,3)
             refcal$ = str(rec2$,19,5)
             if last_model$ <> model$ then z = 0 
             last_model$  = model$ 
             if last_refcal$ <> refcal$ then y = 0
             last_refcal$ = refcal$
             key2$ = str(rec2$,9,40)
             if y > 0 then goto L70000
             for l = 1 to 14
		 y = l
	         if refcal$ = key_tbl$(l) then goto L70000
             next l
             goto L60000

L70000:
             if z > 0 then goto L80000
             for l = 1 to 137
		 z = l
                 if model$ = mdl_tbl$(l) then goto L80000
             next l
             goto L60250

L80000:
	     sw = sw_tbl(y)  
	     fv = s6_tbl(y)  
             a = 49 + ((fv - 1) * 8) 
             b = 49 + ((sw - 1) * 8) 

             message$ = "# ##, # ## " & model$ & ":" & refcal$
             convert fv to str(message$,1,1), pic (0)
             convert a  to str(message$,3,2), pic (00)
             convert sw to str(message$,7,1), pic (0)
             convert b  to str(message$,9,2), pic (00)
REM             call "LOGFILE" (message$)

             str(rec2$,b,8) = str(rec2$,a,8)
             rewrite #2, using L50770, rec2$
             goto L60500

END_JOB:    end
