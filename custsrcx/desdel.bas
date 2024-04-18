REM         +---------------------------------------------------------------+
REM         | copy model 849 with ref 27/28 to 819                          |
REM         +---------------------------------------------------------------+


        dim                              /* FILE = APCPLNDT            */~
            pc_rec$102,apckey$8          /* Detail Record              */

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
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1, "APCPCMST",                                      ~
                        varc,     indexed,  recsize =   102,            ~
                        keypos =    9, keylen = 40,                     ~
                        alt key  1, keypos =   1, keylen =  8            

            call "OPENCHCK" (#1, fs%(1%), f2%(1%),500%, rslt$(1%))

            mat f1% = zer
            upd% = 0%
            tst% = 0%
	    cnt% = 0%           
	    init(hex(00)) apckey$
	    apckey$ = "00287535"

L01000:      /* main loop */
            read #1, key 1% > apckey$,hold, using L50760, pc_rec$,          ~
                                                   eod goto FINISH 
L50760: FMT CH(102)
            apckey$ = str(pc_rec$,1,8)
            r_rec$  = str(pc_rec$,9,16)
            if str(pc_rec$,19,2) <> "28" then L01000
            if str(pc_rec$,16,3) = "103" then L01500
            if str(pc_rec$,16,3) = "113" then L01500
            if str(pc_rec$,16,3) = "123" then L01500
            if str(pc_rec$,16,3) = "133" then L01500

	    goto L01000

L01500:
            delete #1	
	    cnt% = cnt% + 1
            goto L01000

	      
FINISH:            
	    end  
