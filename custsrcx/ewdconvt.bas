        REM              As of 04/30/98
        REM - (EWDCONVT) - Convert the Sort Index in the file (APCPLNDT)
        REM                at position (66 for 30) to a Compressed format
        REM                to allow for the size of the field to increase
        REM                from 30 to 40 Characters.
        REM
        dim dt_key$57, dt_rec$256, rslt$20, file1$8, cnt$10
        dim dt_in$40, file2$8, pd$40, dt_out$30, test$10
 
        init(" ") dt_key$, dt_rec$, rslt$, cnt$, file1$, file2$, test$

        REM - Applicable Select Statement for File.
            file1$ = "APCPLNDT" : file2$ = "APCPLNXX"
            select #1,  "APCPLNDX",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =  24,  keylen =  23,                     ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup

            select #2,  "APCPLNXX",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =  24,  keylen =  23,                     ~
                        alt key 1, keypos = 47, keylen = 57,             ~
                            key 2, keypos = 53, keylen = 51,             ~
                            key 3, keypos =  1, keylen = 23, dup,        ~
                            key 4, keypos = 96, keylen =  8, dup

            call "OPENCHCK" (#1, x%, y%,  0%, rslt$)
            init(" ") rslt$ : x%, y% = 0%
            call "OPENCHCK" (#2, x%, y%, 500%, rslt$)
            x%, y% = 0%
            pd$ = "0000000000000000000000000000000000000000"

         display                                                         ~
            at(01,21), "(EWD) Data File Conversion Utility (EWDCONVT)",  ~
            at(03,21), "          For (APCPLNDT)",                       ~
            at(05,32), hex(84), file2$

        REM - Initialize Scan Key with Production date for "Test Only" 
        REM      test$ = "04/20/98"
        REM      call "DATUNFMT" (test$)        /* For Y2K */
        REM      str(dt_key$,1%,6%) = str(test$,1%,6%)
              cnt% = 0%
            dt_key$ = all(hex(00))
        next_rec
            read #1,key > dt_key$, using L00340, dt_rec$,           ~
                                                  eod goto scan_done
L00340:         FMT CH(256)
            dt_key$   = str(dt_rec$,24%,23%)

            init(" ") dt_in$
            dt_in% = 40%                     /* Max Characters (40)    */
            dt_in$ = str(dt_rec$,66%,30%)
            ln% = len(dt_in$)                /* Find out Actual length */
            ln1% = 40% - ln%                 /* Calc the No. of Pad    */
                                             /* Characters needed      */
            str(dt_in$,ln%+1%,ln1%) = str(pd$,1%,ln1%) 
            cnt% = cnt% + 1%
            if mod(cnt%,500) = 0 then                                    ~
               print at(10,22);hex(84);"Converted Records [";cnt%;"]"

            init(" ") dt_out$
            dt_out% = 0% 
            call "COMPRESS" (dt_in$,dt_in%,dt_out$,dt_out%, ret%)

            if ret% <> 0% then goto L00400    /* Error has Occurred */
            str(dt_rec$,66%,30%) = dt_out$
            write #2, using L00340, dt_rec$, eod goto L00450
            goto next_rec
        scan_done
            convert cnt%  to cnt$,  pic(#,###,###-)

            call "SHOSTAT" ("Records Converted = " & cnt$)
            stop
L00350: end

L00400: call "SHOSTAT" ("Input = " & dt_in$)     : stop
        call "SHOSTAT" ("Sales Order = " & str(dt_rec$,24%,18%)) : stop
        call "SHOSTAT" ("Department = " & str(dt_rec$,42%,3%))  : stop

        goto next_rec 

L00450: call "SHOSTAT" ("(WE)Input = " & dt_in$)     : stop
        call "SHOSTAT" ("(WE)Output = " & dt_out$) : stop

        goto L00350 
