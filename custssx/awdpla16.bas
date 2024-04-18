
        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - AWDPLA16                             *~
            *  Creation Date     - 07/06/04                             *~
            *  Last Modified Date- 03/17/2011                           *~
            *  Written By        - Christie Gregory                     *~
            *  Modifications By  -                                      *~
            *                                                           *~
            *  Description       - Create Glass Batch Files             *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/06/04 ! Original -                               ! CMG *~
            * 06/09/05 ! (AWD001) - Mod for DS batches            ! CMG *~
            * 08/26/05 ! (AWD002) - Mod for Comment Record        ! CMG *~
            *11/17/2009! (AWD003) - ultra batches                 ! CMG *~
            *03/17/2011! (AWD004) mod for multiple intercepts     ! CMG *~
            *************************************************************

        sub "AWDPLA16" (#1, #2, pre%, ds_merge%, schema%) 
                                           /* Return Code                */


        dim                              /*                            */~
            hdr$40,                      /* Askuser Header             */~
            msg$(3%)79,                  /* Askuser Messages           */~
            filename$8,                  /* File Name for Open         */~
            ff$8,                        /* Label Print File Name      */~
            library$8,                   /* Library Name = 'APCDATA'   */~
            volume$6                     /* Volume Name = 'CARLOS'     */

        dim f2%(99%),                    /* = 0 if the file is open    */~
            axd$4,                       /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(99%)20                 /* Text from file opening     */

        dim readkey$24,                  /* GENCODES Readkey           */~
            savekey$24,                  /* Save GENCODES Key          */~
            desc$30                      /* GENCODES Description       */

        dim name$(99%)9,                 /* Batch File Names           */~
            sort$(99%)11,                /* Glass Sort Names           */~
            sort%(99%),                  /* Glass Sort File Numbers    */~
            sort1$(99%)9,                /* Glass Sort Names           */~
            batch1%(3%,99%)                /* Batch Sizes                */

        dim gls_read$(2%)192,            /* GLS GED Read key           */~
            sav_read$(2%)192,            /* GLS GED Read key           */~
            prv_read$(2%)192,            /* GLS GED Read key           */~
            gls_hdr$(2%)192,             /* GLS Header Record          */~
            tb_sort1$(32000%)192,        /* Top/Bot  Sort              */~
            tb_sort2$(32000%)192,        /* Top/Bot  Sort              */~
            sort$2,                      /* Sort                       */~
            sav_sort$2,                  /* Sort                       */~
            gls_sort$11,                 /* Glass Sort                 */~
            prv_gls_sort$11,             /* Glass Sort                 */~
            sav_gls_sort$11              /* Glass Sort                 */

        dim intercept$2                  /* (AWD004) intercept         */



        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! @GLSGED@ ! Glass Batch File for GED Glass System    *~
            * #2  ! GENCODES ! Master Code Tables File                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************



            select #3, "CHRISTIE", consec, recsize = 384
            select #4, "CHRISTIE", consec, recsize = 384
            select #5, "CHRISTIE", consec, recsize = 384
            select #6, "CHRISTIE", consec, recsize = 384
            select #7, "CHRISTIE", consec, recsize = 384
            select #8, "CHRISTIE", consec, recsize = 384
            select #9, "CHRISTIE", consec, recsize = 384
            select #10, "CHRISTIE", consec, recsize = 384
            select #11, "CHRISTIE", consec, recsize = 384
            select #12, "CHRISTIE", consec, recsize = 384
            select #13, "CHRISTIE", consec, recsize = 384
            select #14, "CHRISTIE", consec, recsize = 384
            select #15, "CHRISTIE", consec, recsize = 384
            select #16, "CHRISTIE", consec, recsize = 384
            select #17, "CHRISTIE", consec, recsize = 384
            select #18, "CHRISTIE", consec, recsize = 384
            select #19, "CHRISTIE", consec, recsize = 384
            select #20, "CHRISTIE", consec, recsize = 384
            select #21, "CHRISTIE", consec, recsize = 384
            select #22, "CHRISTIE", consec, recsize = 384
            select #23, "CHRISTIE", consec, recsize = 384
            select #24, "CHRISTIE", consec, recsize = 384
            select #25, "CHRISTIE", consec, recsize = 384
            select #26, "CHRISTIE", consec, recsize = 384
            select #27, "CHRISTIE", consec, recsize = 384
            select #28, "CHRISTIE", consec, recsize = 384
            select #29, "CHRISTIE", consec, recsize = 384

            select #30, "CHRISTIE", consec, recsize = 384
            select #31, "CHRISTIE", consec, recsize = 384
            select #32, "CHRISTIE", consec, recsize = 384
            select #33, "CHRISTIE", consec, recsize = 384
            select #34, "CHRISTIE", consec, recsize = 384
            select #35, "CHRISTIE", consec, recsize = 384
            select #36, "CHRISTIE", consec, recsize = 384
            select #37, "CHRISTIE", consec, recsize = 384
            select #38, "CHRISTIE", consec, recsize = 384
            select #39, "CHRISTIE", consec, recsize = 384

            select #40, "CHRISTIE", consec, recsize = 384
            select #41, "CHRISTIE", consec, recsize = 384
            select #42, "CHRISTIE", consec, recsize = 384
            select #43, "CHRISTIE", consec, recsize = 384
            select #44, "CHRISTIE", consec, recsize = 384
            select #45, "CHRISTIE", consec, recsize = 384
            select #46, "CHRISTIE", consec, recsize = 384
            select #47, "CHRISTIE", consec, recsize = 384
            select #48, "CHRISTIE", consec, recsize = 384
            select #49, "CHRISTIE", consec, recsize = 384
            select #50, "CHRISTIE", consec, recsize = 384
            select #51, "CHRISTIE", consec, recsize = 384
            select #52, "CHRISTIE", consec, recsize = 384
            select #53, "CHRISTIE", consec, recsize = 384
            select #54, "CHRISTIE", consec, recsize = 384
            select #55, "CHRISTIE", consec, recsize = 384
            select #56, "CHRISTIE", consec, recsize = 384
            select #57, "CHRISTIE", consec, recsize = 384
            select #58, "CHRISTIE", consec, recsize = 384
            select #59, "CHRISTIE", consec, recsize = 384
            select #60, "CHRISTIE", consec, recsize = 384


read_next_intercept:                       /*(AWD004) */
            gosub read_glsged
REM            call "SHOSTAT" ("READ GLSGED DONE" )  stop
            if rec% <= 0% then goto exit_sub     /* (AWD004) */
            if tb_cnt% <= 0% then goto read_next_intercept
            gosub load_sort_names
            gosub load_file_names
            ff% = 2%
            gosub open_files


            call "SHOSTAT" (" Writing Glass Data ") 

            gosub write_tb

            gosub write_ending
/* (AWD004) */
REM            call "SHOSTAT" ("CLOSING FILES" )  stop
            gosub close_files
             goto read_next_intercept
            goto exit_sub

        load_sort_names
            init(" ") readkey$, savekey$, desc$, sort$(), sort1$()
            cnt%, sort_max% = 0%

            str(readkey$,1%,9%) = "GED SCHED"
        sort_nxt
            read #2, key > readkey$, using L00100, readkey$, desc$,   ~
                                         eod goto sort_names_done

L00100:          FMT CH(24), CH(30)

                 if str(readkey$,1%,9%) <> "GED SCHED"                ~
                                             then goto sort_names_done
                 cnt% = cnt% + 1%
                 sort$(cnt%)  = str(readkey$,10%,11%)   /* Glass Sort     */
                                                        /* IE 036SSSP17TB */

                                                        /* File Number to  */
                                                        /*  use per sort   */
                 convert str(desc$,2%,2%) to file%, data goto L00150
L00150:
                 convert file% to sort1$(cnt%), pic(00)


                     goto sort_nxt
                 

         
        sort_names_done
             sort_max% = cnt%
        return


        load_file_names
            init(" ") readkey$, savekey$, desc$, name$()
            cnt%, file_max% = 0%
            mat batch1% = zer

            str(readkey$,1%,9%) = "GED SCHE1"
            if pre% = 1% then str(readkey$,1%,9%) = "GED SCHE2"
/*(AWD003)*/
            if pre% = 2% then str(readkey$,1%,9%) = "GED SCHE3"
            savekey$ = readkey$
        file_nxt
            read #2, key > readkey$, using L00100, readkey$, desc$,   ~
                                         eod goto sort_files_done


                 if str(readkey$,1%,9%) <> str(savekey$,1%,9%)        ~
                                             then goto sort_files_done

                                                      /*  Only Use 1 Files */
                 if str(readkey$,10%,1%) <> "1" then goto file_nxt

REM                 if str(desc$,1%,9%) = "FFFFFFFFF" then goto sort_files_done
                 cnt% = cnt% + 1%
REM  !                 name$(cnt%)  = str(desc$,1%,9%)   /* Actual file name to use */
                 name$(cnt%)  = intercept$ & str(desc$,1%,9%)   /* (AWD004)   */
                                                   /* Batch Counters          */
                 convert str(desc$,11%,3%) to batch1%(1%,cnt%), data goto L00200
L00200:
                 convert str(desc$,15%,3%) to batch1%(2%,cnt%), data goto L00250
L00250:
                 convert str(desc$,19%,3%) to batch1%(3%,cnt%), data goto L00300
L00300:


                     goto file_nxt
                 

         
        sort_files_done
             file_max% = cnt%             
        return


        open_files
              mat sort% = zer
              for i% = 1% to cnt%



REM                   library$ = "TESTDATA"
REM                   volume$  = "EWD2" 
 
                 library$ = "SCHEDUL"
                 volume$  = "CARLOS"
                 if schema% = 2% then volume$ = "NE"

                   sort%(i%) = sort%(i%) + 1%       /* Used for File Number */


                   ff% = ff% + 1%
                   p% = 0%
                   p% = pos(name$(i%) = "#" )
                   if p% <> 0% then gosub find_file_number
                   call "SHOSTAT" ("Creating Glass Batch File - "& name$(i%) )

                   call "EWDOPEN" (#ff%, name$(i%), cmg%)
                        if cmg% = 1% then goto L00350   /* Doesn't Exist        */ 
                   str(msg$(1%),19%,8%) = name$(i%)
                   gosub file_exists
                   if comp% <> 16% then goto L00400
                   call "FILEBGON" addr(#ff%)
                      goto L00350
                                                /* Append Mode      */ 
L00400:            close #ff%

                   call "OPENFILE" (#ff%,"EXTND",f2%(fy%),rslt$(fy%), axd$ )
                      goto L00360

L00350:          

                 open nodisplay, #ff%, output, space = 300%,               ~
                 dpack   = 100%, ipack = 100%, file = name$(i%),    ~
                 library = library$, volume = volume$, blocks = 5%

                 str(gls_hdr$(1%),21%,40%) = name$(i%) & bin(34%,1)
                 write #ff%, using L10200, gls_hdr$(1%), gls_hdr$(2%)

L00360:       next i%
        return

        find_file_number
REM             sort%(i%) = sort%(i%) + 1%
             convert sort%(i%) to str(name$(i%),p%,1%),pic(#)

        return

        open_new_file
            gosub check_comments                           /* (AWD002)   */
            gosub check_h_v

            call "SHOSTAT" (" OPEN NEW FILE " ) 
            gosub write_new_ending
                          /* Find File Number In Sorted Array in GED SCHED */
            fx1% = ff% - 2%                     
                            /* this is actual #fx% number stays the same ! */
            fx%  = ff%                          

            sort%(fx1%) = sort%(fx1%) + 1%
            convert str(name$(fx1%),7%,1%) to cmg%, ~
                               data goto new_file_done
            close #fx%                       /* First Close it             */
            

REM            fx1% = ff% + file_max%
            convert sort%(fx1%) to str(name$(fx1%),7%,1%),pic(#)







            call "EWDOPEN" (#fx%, name$(fx1%), cmg%)
               if cmg% = 1% then goto L12350   /* Doesn't Exist        */ 
                   str(msg$(1),19%,8%) = name$(fx1%)
                   gosub file_exists
                   if comp% <> 16% then goto L12400
                   call "FILEBGON" addr(#fx%)
                      goto L12350
                                                /* Append Mode      */ 
L12400:            close #fx%
                   call "OPENFILE" (#fx%,"EXTND",f2%(fy%),rslt$(fy%), axd$ )
                      goto new_file_done

L12350:          
                 open nodisplay, #fx%, output, space = 300%,                ~
                 dpack   = 100%, ipack = 100%, file = name$(fx1%),           ~
                 library = library$, volume = volume$, blocks = 5%

REM               gosub get_tt_max

                 str(gls_hdr$(1%),21%,40%) = name$(fx1%) & bin(34%,1)
                 write #fx%, using L10200, gls_hdr$(1%), gls_hdr$(2%)

        new_file_done
        return


        read_glsged
              call "SHOSTAT" (" Loading Glass Data ")
              init(" ") gls_read$(), sav_read$(), prv_read$(), tb_sort1$(),  ~
                        tb_sort2$(), sort$, sav_sort$, intercept$, gls_hdr$()
              dept%, sav_dept%, tb_cnt%, prv_rec%, rec% = 0%

        gls_nxt
              read #1, using L00500, gls_read$(),   ~
                                 eod goto gls_done
L00500:                  FMT 2*CH(192)

                      rec% = 1%                              /* (AWD004) */
                      seq%  = 0%                             /* (AWD002)  */
                      convert str(gls_read$(),1%,3%) to seq%, data goto NOT_ITEM
                      init(" ") prv_read$()
                      prv_read$() = gls_read$()
                      prv_rec% = 1%
                      goto gls_nxt

NOT_ITEM:                                           /* NOT ITEM RECORD */
/*(AWD004)*/
                      if intercept$ = " " and str(gls_read$(),2%,8%) = "COMMENTS" ~ 
                                then intercept$ = str(gls_read$(),29%,2%)

                      dept% = 0%
REM                   if str(gls_read$(),250%,3%) = "SUN" then goto gls_nxt
                                                         /* (AWD001) */
REM                   if str(gls_read$(),242%,2%) = "DS" then goto gls_nxt
                                                              /* (AWD001)  */
                      if str(gls_read$(),21%,2%) = "DS" and              ~
                                          ds_merge% <> 1% then goto gls_nxt

                      init(" ") sort$
                      if str(gls_read$(),1%,1%) =  "#" then goto gls_done
                      if str(gls_read$(),1%,1%) =  "<" then              ~
                             gls_hdr$() = gls_read$()
                      if str(gls_read$(),1%,1%) =  "<" then goto gls_nxt
                      if str(gls_read$(),1%,1%) <> "*" then goto L00510
                               str(sav_read$(),1%,384%) = str(gls_read$(),1%,384%)
                               init(" ") sav_sort$               /* (AWD002) */
                               goto gls_nxt

L00510:               if str(gls_read$(),2%,1%) <> "H" and                        ~
                         str(gls_read$(),2%,1%) <> "V" then goto L00520
                              dept% = sav_dept%
                              sort$ = sav_sort$
                                                          /* (AWD002)  */
L00520:               if str(gls_read$(),2%,3%) <> "COM" then goto L00540
                              dept% = sav_dept%
                              sort$ = sav_sort$


L00540:
REM                      if dept% =  0% then                                         ~
                      convert str(gls_read$(),236%,3%) to dept%, data goto L00530
                                                                   /* (AWD002)  */
                      if dept% =  0% then                                         ~
                      convert str(gls_read$(),18%,3%) to dept%, data goto L00530

                      sav_dept% = dept%
L00530:
REM                      if sort$ = " " then sort$ = str(gls_read$(),248%,2%)
                                                              /* (AWD002)  */
                      if sort$ = " " then sort$ = str(gls_read$(),27%,2%)
                      sav_sort$ = sort$

                      if sort$ = "TB" then gosub tb_sort


                      goto gls_nxt
        gls_done
        return



        tb_sort
            if str(sav_read$(),1%,384%) = " " then goto tb_cont
               tb_cnt% = tb_cnt% + 1%
               tb_sort1$(tb_cnt%) = str(sav_read$(),1%,192%)
               tb_sort2$(tb_cnt%) = str(sav_read$(),193%,192%)
               init(" ") sav_read$()
tb_cont:

            tb_cnt% = tb_cnt% + 1%   
            if tb_cnt% >= 32000% then call "SHOSTAT" (" Max Counter SUB A !! ")
            if tb_cnt% >= 32000% then stop

                                                   /* (AWD002)  */

            if prv_rec% = 0% then goto LOAD_ITEM
               tb_sort1$(tb_cnt%) = str(prv_read$(),1%,192%)
               tb_sort2$(tb_cnt%) = str(prv_read$(),193%,192%)
               tb_cnt% = tb_cnt% + 1%
               prv_rec% = 0%

LOAD_ITEM:

            tb_sort1$(tb_cnt%) = str(gls_read$(),1%,192%)
            tb_sort2$(tb_cnt%) = str(gls_read$(),193%,192%)


        return




        write_tb
             if tb_cnt% = 0% then return
                for tb% = 1% to tb_cnt%
                    if tb_sort1$(tb%) = " " then goto L10000
                    if str(tb_sort1$(tb%),1%,1%) = "*" then gosub find_tb_file
                    
                    write #fx%, using L10200, tb_sort1$(tb%),       ~
                                             tb_sort2$(tb%)

                    if tb_counter% = 999% then goto L10210
                       convert str(tb_sort1$(tb%),2%,2%)  ~ 
                                to cmg%, data goto L10210

                       tb_counter% = tb_counter% - 1%
REM                         if tb_counter% = 0% then gosub open_new_file
                       num_to_add% = 2%

                                                               /* (AWD002)  */
                       if str(tb_sort1$(tb%),2%,3%) = "COM"               ~
                                                         then num_to_add% = 1%

                       if str(tb_sort1$(tb%+2%),2%,1%) = "H" or              ~
                             str(tb_sort1$(tb%+2%),2%,1%) = "V" then         ~
                             num_to_add% = 4%



                           if tb_counter% <= 0% and                          ~
                                 str(tb_sort1$(tb%+num_to_add%),1%,1%) = "*" ~
                                 then gosub open_new_file

L10210:

L10200:                   FMT CH(192), CH(192)
L10000:         next tb%


        return

                                                     /* (AWD002) - BEG */
        check_comments
              if str(tb_sort1$(tb%+1%),2%,3%) <> "COM" then return

              write #fx%, using L10200, tb_sort1$(tb%+1%),       ~
                                             tb_sort2$(tb%+1%)

              tb% = tb% + 1%

        return
                                                     /* (AWD002) - END */


        check_h_v
            if str(tb_sort1$(tb%+1%),2%,1%) <> "V" and               ~
               str(tb_sort1$(tb%+1%),2%,1%) <> "H" then return

                    write #fx%, using L10200, tb_sort1$(tb%+1%),       ~
                                             tb_sort2$(tb%+1%)

                    write #fx%, using L10200, tb_sort1$(tb%+2%),       ~
                                             tb_sort2$(tb%+2%) 

                    tb% = tb% + 2%              

        return

        find_tb_file
REM            call "SHOSTAT" (" FIND TB FILE " )  stop
            init(" ") gls_sort$
REM         str(gls_sort$,1%,11%) = str(tb_sort2$(tb%+1%),103%,11%)
                                                /* (AWD002)  */
            str(gls_sort$,1%,11%) = str(tb_sort1$(tb%+2%),18%,11%)
            if str(gls_sort$,1%,11%) <> str(prv_gls_sort$,1%,11%)    ~
                                      then tb_counter% = 0%
            for j% = 1% to sort_max%
                if str(sort$(j%),1%,11%) = gls_sort$ then goto tb_file_done

            next j%

            prv_gls_sort$ = gls_sort$
            ff% = file_max% + 2%              /* (AWD002) Max Num Of files */
            fx% = ff%                          /* (AWD002)                 */
         return

        tb_file_done
            prv_gls_sort$ = gls_sort$
            convert sort1$(j%) to ff%, data goto L10100
L10100:     ff% = ff% + 2%             /* must be 2; to be at start of files */
            fx% = ff%                
            if tb_counter% = 0% or tb_counter% = 999%  then  gosub get_tb_max
        return


REM        get_tb_max
REM             tb_counter% = batch1%(sort%(ff%-2%), ff%-2%)
REM        return  

        get_tb_max
             
             sort% = sort%(ff%-2%)         /* sort%(filenumber) */
             if sort% > 3 then sort% = sort% - 3%

             init(" ") cmg$
             cmg% = 99%
             convert batch1%(sort%,ff%-2%) to cmg$, pic(###)

             convert cmg$ to cmg%, data goto L12120
L12120:
             if cmg$ = " " or cmg% = 0% then sort% = 1%
             if sort% = 0% then                                  ~
             sort% = sort% + 1%            /* If it is 3 or blank*/
                                           /* then start counter */
                                           /* over; continuous   */
                                           /* counter loop       */

             tb_counter% = batch1%(sort%, ff%-2%)
        return

        write_ending
REM              call "SHOSTAT" (" Writing ending ") stop
              file_max% = file_max% + 2%
              for x% = 3% to file_max%
                  write #x%, using L10200, "#", " "
              next x%
        return

        write_new_ending
              
                  write #fx%, using L10200, "#", " "
              
        return


        file_exists
            comp% = 2%
            hdr$ = "**** Glass Batch File " & name$(ff%-2%) & "***"
                                                 /* msg$(1%) Pre-Set   */
            msg$(2) = "       G L A S S   B A T C H   F I L E S         "
            msg$(3) = "Press <RETURN> To Continue, or PF(16) to Delete. "
            call "ASKUSER" (comp%, hdr$, msg$(1), msg$(2), msg$(3))
        return

        open_error                                   
            errormsg$ = "(Open Error) - File = " & filename$
            gosub error_prompt
            err% = 0%
        return

        error_prompt
           comp% = 2%
           hdr$ = "***** (Error) (Error) (Error)  *****"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = errormsg$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        exit_sub

             end

/*(AWD004) */ 
        close_files
           fc% = 3%
           file_max% = file_max% - 2%
           for i% = 1% to file_max%
             close #fc%
             fc% = fc% + 1%
           next i%
        return

/*(AWD004/) */


