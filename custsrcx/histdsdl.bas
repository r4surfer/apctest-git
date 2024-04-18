        REM THISPROGRAMWASGENERATEDUSINGTHEGENPGMPROGRAMWHICHISAPROPRIETR~
            *-----------------------------------------------------------*~
            * HISTDSDL - NEW Program to delete 2004 records from BCKSUBPT~
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

        dim cnt2%, cnt3%, cnt4%            
        dim gen_key$11      
        dim bcksubpt_rec$256
        dim subp$20, infp$9, year$4

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
            print at(01,02);"starting";
            gosub read_ewdhist

            goto exit_program

            init_variables
            cnt%  = 0%
            cnt2% = 0%
	    cnt3% = 0%
	    cnt4% = 0%
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
                read #1, key 1 > ewdhist_key1$,  using L00100,      ~
                                       ewdhist_rec$,                      ~
                                       eod goto ewdhist_read_done

                      beg_time$ = time
                      convert beg_time$ to beg_time%
             
                      goto ewdhist_first

            read_ewdhist_next

                read #1, using L00100, ewdhist_rec$,                 ~
                                       eod goto ewdhist_read_done

                


ewdhist_first:
			 if cnt2% <> 999 then goto skip_cnt2
			 cnt2% = 0%
			 if cnt3% <> 999 then goto skip_cnt3
                         cnt3% = 0%
			 cnt4% = cnt4% + 1%
skip_cnt3:
                         cnt3% = cnt3% + 1%
skip_cnt2:                    
                         cnt2% = cnt2% + 1%

                         ewdhist_key$  = str(ewdhist_rec$,17%,32%)
                         part$         = str(ewdhist_rec$,119%,25%)
                         year$         = str(ewdhist_rec$,65%,4%)
			 print at(02,02);year$;":";cnt3%;"k ";cnt4%;"m";
                         if str(ewdhist_rec$,65%,4%) <> "2004" then goto read_ewdhist_next
                         gosub load_sub_part

                         goto read_ewdhist_next

            ewdhist_read_done
                end_time$ = time
                convert end_time$ to end_time%

                total_time% = end_time% - beg_time%

                convert total_time% to str(ewdhist_msg$,45%,8%), pic(########)
                print at(03,02);hex(84);ewdhist_msg$;
            return

        load_sub_part    
             init(" ") gen_key$, new_part$, new_subp$, new_infp$
             str(gen_key$,1%,11%) = str(ewdhist_rec$,54%,11%)                  
             read #2, hold,  key >= gen_key$, using L00100, bcksubpt_rec$,     ~
                                          eod goto load_sub_part_exit

             cnt% = cnt% + 1%
             print at(04,02);hex(84);str(ewdhist_rec$,54%,11%);
             print at(04,41);hex(84);str(bcksubpt_rec$,1%,11%);
             print at(05,02);hex(84);cnt%;    
             delete #2
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

                print at(18,02);hex(84);"deleted ";cnt%;
                print at(24,20);hex(84);"FINISHED UPDATING FILES !!!!!!!!";
                stop


             end








