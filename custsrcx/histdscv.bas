        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *-----------------------------------------------------------*~
            * HISTCONV - NEW Program to update the sub part             *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 05/15/06 ! Original                                 ! CMG *~
            PRODUCTOFCAELUSASSOCIATESSPOKANEWASHINGTONALLRIGHTSRESERVED**


        dim                                                              ~
            part$25,                     /* ALL Files Part Number      */~
            part_desc$64,                /* All Files Part Description */~
            new_part$25,                 /* Corrected Part Number      */~
            new_subp$20,                 /* New Sub Part Number        */~
            new_infp$9,                  /* Information Part           */~
            beg_time$8,                  /* Beginning Time             */~
            end_time$8                   /* Ending Time                */



        dim ewdhist_rec$256,             /* EWDHIST  Record            */~
            ewdhist_key$32,              /* EWDHIST  key               */~
            ewdhist_key1$11,             /* EWDHIST  key               */~
            ewdhist_msg$80               /* EWDHIST  Message           */

        dim                              /* (Program) - Variables      */~
            filename$8,                  /* Used By EWDOPEN            */~
            hdr$45, msg$(3%)79           /* Askuser - Var's            */

        dim gen_key$11      
        dim bcksubpt_rec$256
        dim subp$20, infp$9

        dim f2%(10%),                    /* = 0 if the file is open    */~
            f1%(10%),                    /* = 1 if READ was successful */~
            fs%(10%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(10%)20                 /* Text from file opening     */


        REM *************************************************************~
            *                  Release Version ID Section               *~
            *************************************************************
            dim apc$35, pname$21            
            apc$   = "Convert part number in existing orders"
            pname$ = "HISTCONVT - 01/06/2006"

        REM *************************************************************

            mat f2% = con

            mat f1% = con

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! EWDHIST  ! (New) Master Sales Order History File    *~
            * #2  ! BCKSUBPT ! (New) Sub Part Number File               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #1,  "EWDHIST",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos =   17, keylen =  32,                     ~
                        alt key  1, keypos =   54, keylen =  15,         ~
                            key  2, keypos =   49, keylen =  20,         ~
                            key  3, keypos =   77, keylen =  26,         ~
                            key  4, keypos =    1, keylen =  25, dup,    ~
                            key  5, keypos =  193, keylen =   8, dup 
                     
            select #2, "BCKSUBPT",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,    keylen = 11,                      ~
                        alt key  1, keypos =   12, keylen =  11, dup,    ~
                            key  2, keypos =   23, keylen =  45, dup 


            filename$ = "EWDHIST" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
	    
            filename$ = "BCKSUBPT" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error

            gosub init_variables

            gosub read_ewdhist

            goto exit_program

            init_variables

            init(" ") part$, new_part$, new_subp$, new_infp$, part_desc$, ~
                      ewdhist_rec$, ewdhist_key$, ewdhist_msg$,           ~
                      nc_models$(), part_models$()
            return



REM         ----------------------------------------------------------------
REM                      EWDHIST Read         
REM         ----------------------------------------------------------------

            read_ewdhist
                init(" ") ewdhist_key$, ewdhist_rec$, part$, ewdhist_msg$
                str(ewdhist_msg$,1%,9%) = "EWDHIST "
                ewdhist_key1$ = "01748648       " 
REM             read #1, hold, key > ewdhist_key$,  using L00100,         ~
                read #1, hold, key 1 > ewdhist_key1$,  using L00100,      ~
                                       ewdhist_rec$,                      ~
                                       eod goto ewdhist_read_done

                      beg_time$ = time
                      convert beg_time$ to beg_time%
             
                      goto ewdhist_first

            read_ewdhist_next

                read #1, hold, using L00100, ewdhist_rec$,                 ~
                                       eod goto ewdhist_read_done

                


ewdhist_first:

                         ewdhist_key$  = str(ewdhist_rec$,17%,32%)
                         part$         = str(ewdhist_rec$,119%,25%)

                         if str(ewdhist_rec$,65%,4%) <> "2004" then goto read_ewdhist_next
                         gosub load_sub_part
                         str(ewdhist_msg$,15%,32%) = ewdhist_key$
                         convert beg_time% to str(ewdhist_msg$,50%,8%), pic(########)
                         print at(01,02);hex(84);ewdhist_msg$;
                           part$ = str(ewdhist_rec$,119%,25%)
                           subp$ = str(ewdhist_rec$,215%,20%)
                           infp$ = str(ewdhist_rec$,235%,9%) 
			   print at (04,02);hex(84);part$
			   print at (04,40);hex(84);new_part$
			   print at(06,02);hex(84);subp$
			   print at(06,40);hex(84);new_subp$
			   print at(08,02);hex(84);infp$
			   print at(08,40);hex(84);new_infp$
                           str(ewdhist_rec$,119%,25%)  = new_part$
                           str(ewdhist_rec$,215%,20%)  = new_subp$
                           str(ewdhist_rec$,235%,9%)   = new_infp$
                           delete #1
                           write #1, using L00100, ewdhist_rec$,       ~
                                         eod goto ewdhist_write_error

                         goto read_ewdhist_next

            ewdhist_read_done
                end_time$ = time
                convert end_time$ to end_time%

                total_time% = end_time% - beg_time%

                convert total_time% to str(ewdhist_msg$,45%,8%), pic(########)
                print at(03,02);hex(84);ewdhist_msg$;
            return
ewdhist_write_error:
                comp% = 2%
                hdr$  = "******* (Error) (Error) (Error)  *******"
                msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
                msg$(2%) = "Update EWDHIST " & ewdhist_key$
                msg$(3%) = "Press Any Key To Continue."
                call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))

                goto read_ewdhist_next             /* Continue Processing */

        load_sub_part    
             init(" ") gen_key$, new_part$, new_subp$, new_infp$
             cnt% = 1%
	     new_part$ = part$
	     new_subp$ = subp$
	     new_infp$ = infp$
             str(gen_key$,1%,11%) = str(ewdhist_rec$,54%,11%)                  
             read #2, key >= gen_key$, using L00100, bcksubpt_rec$,     ~
                                          eod goto load_sub_part_exit
              new_part$ = str(bcksubpt_rec$,23%,25%)      
              new_subp$ = str(bcksubpt_rec$,48%,20%)
              new_infp$ = str(bcksubpt_rec$,132%,9%)
              cnt% = cnt% + 1%
	load_sub_part_exit       
        return


        open_error
           comp% = 2%
           hdr$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = "(Open Error) - File = " & filename$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return





REM     -----------------------------------------------------------
REM                   FORMAT STATEMENTS
REM     ----------------------------------------------------------

L00100:           FMT CH(256)

          exit_program

                print at(18,02);hex(84);"updated ";cnt%;
                print at(24,20);hex(84);"FINISHED UPDATING FILES !!!!!!!!";
                stop


             end








