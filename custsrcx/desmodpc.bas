REM         +---------------------------------------------------------------+
REM         | copy model 849 with ref 27/28 to 819                          |
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
	    cnt% = 0%           
	    del% = 0%           

L01000:      /* main loop */
            read #1, hold, using L50760, pc_rec$,          ~
                                                   eod goto ENDJOB
	    cnt% = cnt% + 1
            get str(pc_rec$,49,8), using GETPD, discount 
GETPD:   FMT PD(14,4)
            if (str(pc_rec$,9,1) <> "A") then goto L01000	    
/*      skip is not ref code 27/28 and rec cal method 000  */
            if (str(pc_rec$,21,3) <> "000") then goto L01000
            if (str(pc_rec$,19,2) <> "27" and                          ~
                str(pc_rec$,19,2) <> "28") then goto L01000
/*      only mod following models                       */
            if (str(pc_rec$,13,3) = "103") then goto set_mdl    
            if (str(pc_rec$,13,3) = "113") then goto set_mdl    
            if (str(pc_rec$,13,3) = "123") then goto set_mdl    
            if (str(pc_rec$,13,3) = "133") then goto set_mdl    
            goto L01000

set_mdl: 
/* set ref code 27 discount to $0.52, clear ref code 28 */
	    discount = 0.52
            if (str(pc_rec$,19,2) <> "27") then discount = 0.00          
            put str(pc_rec$,49,8), using GETPD, discount 
            rewrite #1, using L50760, pc_rec$, ~
		     eod goto L01000    
	    upd% = upd% + 1
            goto L01000


L50760: FMT CH(102)
ENDJOB:     print "         records read    = ", cnt%
            print "         records updated = ", upd%
	    stop
