        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN28                             *~
            *  Creation Date     - 07/11/05                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New Program to Build | delimited     *~
            *                      files to import into Oracle.         *~
            *                                                           *~
            *                      This program only creates files      *~
            *                      need for WW import                   *~
            *                      AWDCOMP  - COMPLAINT (GENCODES)      *~
            *                      AWDCOMP1 - COMPCOL1                  *~
            *                      AWDCOMP2 - COMPCOL2                  *~
            *                      AWDCOMP3 - COMPCOL3                  *~
            *                      AWDCOMP4 - COMPCOL4                  *~
            *                      AWDCOMP5 - COMPCOL5                  *~
            *                      AWDCOMP6 - COMPCOL6                  *~
            *                      AWDCOMP7 - COMPCOL7                  *~
            *                                                           *~
            *  Code Tables Used  -                                      *~
            *                                                           *~
            *  Subroutine Used   -                                      *~
            *                                                           *~
            *  Spec. Comm (Screen 1) -                                  *~
            *                         PF(10) Create Delimited File.     *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 07/11/05 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            *************************************************************

        dim                              /* (GENCODES) - FILE          */~
            readkey$100,                 /* Readkey                    */~
            fields$(500%)256,            /* Generic Fields             */~
            num_fields(500%)            /* Generic Fields             */ 
            
            
        dim                              /* (Program) - Variables      */~
            filename$8,                  /* Used By EWDOPEN            */~
            cnt$28,                      /* Screen Display             */~
            hdr$45, msg$(3%)79,          /* Askuser - Var's            */~
            date$8,                      /* REPORT TITLE               */~
            userid$3,                    /* Current User Id            */~
            readdate$10                  /* Read date                  */

        dim f2%(15%),                    /* = 0 if the file is open    */~
            f1%(15%),                    /* = 1 if READ was successful */~
            fs%(15%),                    /* = 1 if file open, -1 if it */~
                                         /*   doesn't exist, or 0 if   */~
                                         /*   not yet checked (OPENCHCK*/~
            rslt$(15%)20                 /* Text from file opening     */



        dim comma$1,                     /* Comma for EWDAPPSN File    */~
            file$8,                      /* File Name                  */~
            library$8,                   /* Library Name = APCDATA     */~
            volume$6                     /* DISK VOLUME = CARLOS       */


        REM *************************************************************

            mat f2% = con
            mat fs% = zer
            rslt$() = " "

                     /* The variable F2%() should not be modified.     */
                     /* FS%() also should not be modified (see         */
                     /* OPENCHCK).                                     */

        REM *************************************************************~
            *                     S E L E C T                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! GENCODES ! Control System Codes File                *~
            * #2  ! COMPCOL1 ! Complaint Column 1 Codes                 *~
            * #3  ! COMPCOL2 ! Complaint Column 2 Codes                 *~
            * #4  ! COMPCOL3 ! Complaint Column 3 Codes                 *~
            * #5  ! COMPCOL4 ! Complaint Column 4 Codes                 *~
            * #6  ! COMPCOL5 ! Complaint Column 5 Codes                 *~
            * #7  ! COMPCOL6 ! Complaint Column 6 Codes                 *~
            * #8  ! COMPCOL7 ! Complaint Column 7 Codes                 *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************


            select #1,  "GENCODES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 128,                                   ~
                        keypos =    1,  keylen = 24


            select #2,  "COMPCOL1",                                      ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,    keylen =  4 

            select #3, "COMPCOL2",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,    keylen =  5 

            select #4, "COMPCOL3",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,    keylen =  6

            select #5, "COMPCOL4",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,    keylen =  6

            select #6, "COMPCOL5",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,    keylen =  6

            select #7, "COMPCOL6",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,    keylen =  6

            select #8, "COMPCOL7",                                       ~
                        varc,     indexed,  recsize =  256,              ~
                        keypos = 1,    keylen =  6




            select #20, "AWDCOMP",                                       ~
                        varc,     indexed, recsize =  500,               ~
                        keypos = 1,    keylen = 26


            select #21, "AWDCOL1",                                       ~
                        varc,     indexed, recsize =  268,               ~
                        keypos = 1,    keylen =  6

            select #22, "AWDCOL2",                                       ~
                        varc,     indexed, recsize =  268,               ~
                        keypos = 1,    keylen =  8


            select #23, "AWDCOL3",                                       ~
                        varc,     indexed, recsize =  268,               ~
                        keypos = 1,    keylen = 10

            select #24, "AWDCOL4",                                       ~
                        varc,     indexed, recsize =  268,               ~
                        keypos = 1,    keylen = 10

            select #25, "AWDCOL5",                                       ~
                        varc,     indexed, recsize =  268,               ~
                        keypos = 1,    keylen = 10

            select #26, "AWDCOL6",                                       ~
                        varc,     indexed, recsize =  268,               ~
                        keypos = 1,    keylen = 10

            select #27, "AWDCOL7",                                       ~
                        varc,     indexed, recsize =  268,               ~
                        keypos = 1,    keylen = 10
                        

REM            call "SHOSTAT" ("Initialization")

           mat fs% = zer
           rslt$() = " "

           filename$ = "GENCODES" : call "EWDOPEN" (#1, filename$, err%)
           if err% <> 0% then gosub open_error

           filename$ = "COMPCOL1" : call "EWDOPEN" (#2,  filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "COMPCOL2" : call "EWDOPEN" (#3,  filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "COMPCOL3" : call "EWDOPEN" (#4,  filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "COMPCOL4" : call "EWDOPEN" (#5,  filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "COMPCOL5" : call "EWDOPEN" (#6,  filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "COMPCOL6" : call "EWDOPEN" (#7,  filename$, err%)
           if err% <> 0% then gosub open_error
           filename$ = "COMPCOL7" : call "EWDOPEN" (#8,  filename$, err%)
           if err% <> 0% then gosub open_error
            
            
            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date

            call "DATEFMT" (date$)
            mat num_fields     = zer

        gosub initialize_variables
        gosub files_analysis
        goto exit_program

REM   END OF AUTOMATION

        REM *************************************************************~
            *             P R I N T   R E P O R T                       *~
            *-----------------------------------------------------------*~
            * Display Various Options                                   *~
            *************************************************************

        files_analysis
            comma$ = "|"

            gosub create_comp
            gosub create_comp1
            gosub create_comp2
            gosub create_comp3
            gosub create_comp4
            gosub create_comp5
            gosub create_comp6
            gosub create_comp7
 

        return



        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") readkey$, fields$(), filename$, cnt$, hdr$, msg$(),~
                      date$, file$, library$, volume$, readdate$

        return

        REM *************************************************************~
            *************************************************************

        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

REM     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
REM     Create AWDCOMP file from Gencodes Table COMPLAINT 
REM     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


        create_comp    
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             number_of_fields%   = 5%
             file$   = "AWDCOMP"
             ff% = 20%  
             gosub open_file
      
             gosub read_comp
                   goto L60000
        create_comp_nxt
             gosub read_comp_nxt
             if rec% <> 1% then goto comp_done
L60000:

               gosub write_upload_comp
               goto create_comp_nxt
        return
        comp_done
        return


        read_comp
            init(" ") readkey$, fields$()
            readkey$ = "COMPLAINT"
            mat num_fields = zer
            rec% = 0%
            read #1, key > readkey$, using L60010, readkey$,       ~
                            eod goto read_comp_done
L60010:          FMT CH(09)
                if str(readkey$,1%,9%) <> "COMPLAINT" then         ~
                                            goto read_comp_done
                goto L60020
        read_comp_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #1, using L60010, readkey$, eod goto read_comp_done


                if str(readkey$,1%,9%) <> "COMPLAINT" then         ~
                                            goto read_comp_done

L60020:         cnt% = cnt% + 1%
            goto L60030
            if mod(cnt%,50%) <> 0% then goto L60030
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L60030:
                get #1, using L60040, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%)



L60040:           FMT CH(09), CH(15), CH(30), CH(02), CH(72)


            rec% = 1%            
        read_comp_done
        return

        write_upload_comp
            write #20, using L60100, fields$(1%), comma$,        ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$,       ~
                              fields$(5%), comma$

L60100:               FMT CH(09), CH(01), CH(15), CH(01), CH(30), ~
                          CH(01), CH(02), CH(01), CH(72), CH(01)
        return



REM     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
REM     Create AWDCOMP1 file from COMPCOL1 File              
REM     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


        create_comp1
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             number_of_fields%   = 4%
             file$   = "AWDCOMP1"
             ff% = 21%  
             gosub open_file
      
             gosub read_comp1
                   goto L61000
        create_comp1_nxt
             gosub read_comp1_nxt
             if rec% <> 1% then goto comp1_done
L61000:

               gosub write_upload_comp1
               goto create_comp1_nxt
        return
        comp1_done
        return


        read_comp1
            init(" ") readkey$, fields$()
            mat num_fields = zer
            rec% = 0%
            read #2, key > readkey$, using L61010, readkey$,       ~
                            eod goto read_comp1_done
L61010:          FMT CH(04)

                goto L61020
        read_comp1_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #2, using L61010, readkey$, eod goto read_comp1_done



L61020:         cnt% = cnt% + 1%
            goto L61030
            if mod(cnt%,50%) <> 0% then goto L61030
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L61030:
                get #2, using L61040, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%)

                     if str(fields$(4%),1%,256%)  < " " then     ~
                           str(fields$(4%),1%,256%) = " "

L61040:           FMT CH(03), CH(01), CH(60), CH(192)


            rec% = 1%            
        read_comp1_done
        return

        write_upload_comp1
            write #21, using L61100, fields$(1%), comma$,        ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$

L61100:               FMT CH(03), CH(01), CH(01), CH(01), CH(60), ~
                          CH(01), CH(192), CH(01)
        return




REM     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
REM     Create AWDCOMP2 file from COMPCOL2 File              
REM     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


        create_comp2
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             number_of_fields%   = 5%
             file$   = "AWDCOMP2"
             ff% = 22%  
             gosub open_file
      
             gosub read_comp2
                   goto L62000
        create_comp2_nxt
             gosub read_comp2_nxt
             if rec% <> 1% then goto comp2_done
L62000:

               gosub write_upload_comp2
               goto create_comp2_nxt
        return
        comp2_done
        return


        read_comp2
            init(" ") readkey$, fields$()
            mat num_fields = zer
            rec% = 0%
            read #3, key > readkey$, using L62010, readkey$,       ~
                            eod goto read_comp2_done
L62010:          FMT CH(05)

                goto L62020
        read_comp2_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #3, using L62010, readkey$, eod goto read_comp2_done



L62020:         cnt% = cnt% + 1%
            goto L62030
            if mod(cnt%,50%) <> 0% then goto L62030
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L62030:
                get #3, using L62040, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%)

                     if str(fields$(5%),1%,256%)  < " " then     ~
                           str(fields$(5%),1%,256%) = " "

L62040:           FMT CH(03), CH(01), CH(01), CH(60), CH(192)


            rec% = 1%            
        read_comp2_done
        return

        write_upload_comp2
            write #22, using L62100, fields$(1%), comma$,        ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$,       ~
                              fields$(5%), comma$

L62100:               FMT CH(03), CH(01), CH(01), CH(01), CH(01), ~
                          CH(01), CH(60), CH(01), CH(191), CH(01)
        return

 
REM     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
REM     Create AWDCOMP3 file from COMPCOL3 File              
REM     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


        create_comp3
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             number_of_fields%   = 6%
             file$   = "AWDCOMP3"
             ff% = 23%  
             gosub open_file
      
             gosub read_comp3
                   goto L63000
        create_comp3_nxt
             gosub read_comp3_nxt
             if rec% <> 1% then goto comp3_done
L63000:

               gosub write_upload_comp3
               goto create_comp3_nxt
        return
        comp3_done
        return


        read_comp3
            init(" ") readkey$, fields$()
            mat num_fields = zer
            rec% = 0%
            read #4, key > readkey$, using L63010, readkey$,       ~
                            eod goto read_comp3_done
L63010:          FMT CH(06)

                goto L63020
        read_comp3_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #4, using L63010, readkey$, eod goto read_comp3_done



L63020:         cnt% = cnt% + 1%
            goto L63030
            if mod(cnt%,50%) <> 0% then goto L63030
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L63030:
                get #4, using L63040, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%), fields$(6%)

                     if str(fields$(6%),1%,256%)  < " " then     ~
                           str(fields$(6%),1%,256%) = " "

L63040:           FMT CH(03), CH(01), CH(01), CH(01), CH(60), CH(192)


            rec% = 1%            
        read_comp3_done
        return

        write_upload_comp3
            write #23, using L63100, fields$(1%), comma$,        ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$,       ~
                              fields$(5%), comma$, fields$(6%),  ~
                              comma$

L63100:               FMT CH(03), CH(01), CH(01), CH(01), CH(01), ~
                          CH(01), CH(01), CH(01), CH(60), CH(01), ~
                          CH(190), CH(01)
        return
        


 
REM     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
REM     Create AWDCOMP4 file from COMPCOL4 File              
REM     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


        create_comp4
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             number_of_fields%   = 6%
             file$   = "AWDCOMP4"
             ff% = 24%  
             gosub open_file
      
             gosub read_comp4
                   goto L64000
        create_comp4_nxt
             gosub read_comp4_nxt
             if rec% <> 1% then goto comp4_done
L64000:

               gosub write_upload_comp4
               goto create_comp4_nxt
        return
        comp4_done
        return


        read_comp4 
            init(" ") readkey$, fields$()
            mat num_fields = zer
            rec% = 0%
            read #5, key > readkey$, using L64010, readkey$,       ~
                            eod goto read_comp4_done
L64010:          FMT CH(06)

                goto L64020
        read_comp4_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #5, using L64010, readkey$, eod goto read_comp4_done



L64020:         cnt% = cnt% + 1%
            goto L64030
            if mod(cnt%,50%) <> 0% then goto L64030
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L64030:
                get #5, using L64040, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%), fields$(6%)

                     if str(fields$(6%),1%,256%)  < " " then     ~
                           str(fields$(6%),1%,256%) = " "

L64040:           FMT CH(03), CH(01), CH(01), CH(01), CH(60), CH(192)


            rec% = 1%            
        read_comp4_done
        return

        write_upload_comp4
            write #24, using L64100, fields$(1%), comma$,        ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$,       ~
                              fields$(5%), comma$, fields$(6%),  ~
                              comma$

L64100:               FMT CH(03), CH(01), CH(01), CH(01), CH(01), ~
                          CH(01), CH(01), CH(01), CH(60), CH(01), ~
                          CH(190), CH(01)
        return


REM     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
REM     Create AWDCOMP5 file from COMPCOL5 File              
REM     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


        create_comp5
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             number_of_fields%   = 6%
             file$   = "AWDCOMP5"
             ff% = 25%  
             gosub open_file
      
             gosub read_comp5
                   goto L65000
        create_comp5_nxt
             gosub read_comp5_nxt
             if rec% <> 1% then goto comp5_done
L65000:

               gosub write_upload_comp5
               goto create_comp5_nxt
        return
        comp5_done
        return


        read_comp5 
            init(" ") readkey$, fields$()
            mat num_fields = zer
            rec% = 0%
            read #6, key > readkey$, using L65010, readkey$,       ~
                            eod goto read_comp5_done
L65010:          FMT CH(06)

                goto L65020
        read_comp5_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #6, using L65010, readkey$, eod goto read_comp5_done



L65020:         cnt% = cnt% + 1%
            goto L65030
            if mod(cnt%,50%) <> 0% then goto L65030
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L65030:
                get #6, using L65040, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%), fields$(6%)

                     if str(fields$(6%),1%,256%)  < " " then     ~
                           str(fields$(6%),1%,256%) = " "

L65040:           FMT CH(03), CH(01), CH(01), CH(01), CH(60), CH(192)


            rec% = 1%            
        read_comp5_done
        return

        write_upload_comp5
            write #25, using L65100, fields$(1%), comma$,        ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$,       ~
                              fields$(5%), comma$, fields$(6%),  ~
                              comma$

L65100:               FMT CH(03), CH(01), CH(01), CH(01), CH(01), ~
                          CH(01), CH(01), CH(01), CH(60), CH(01), ~
                          CH(190), CH(01)
        return



REM     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
REM     Create AWDCOMP6 file from COMPCOL6 File              
REM     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


        create_comp6
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             number_of_fields%   = 6%
             file$   = "AWDCOMP6"
             ff% = 26%  
             gosub open_file
      
             gosub read_comp6
                   goto L66000
        create_comp6_nxt
             gosub read_comp6_nxt
             if rec% <> 1% then goto comp6_done
L66000:

               gosub write_upload_comp6
               goto create_comp6_nxt
        return
        comp6_done
        return


        read_comp6 
            init(" ") readkey$, fields$()
            mat num_fields = zer
            rec% = 0%
            read #7, key > readkey$, using L66010, readkey$,       ~
                            eod goto read_comp6_done
L66010:          FMT CH(06)

                goto L66020
        read_comp6_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #7, using L66010, readkey$, eod goto read_comp6_done



L66020:         cnt% = cnt% + 1%
            goto L66030
            if mod(cnt%,50%) <> 0% then goto L66030
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L66030:
                get #7, using L66040, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%), fields$(6%)

                     if str(fields$(6%),1%,256%)  < " " then     ~
                           str(fields$(6%),1%,256%) = " "

L66040:           FMT CH(03), CH(01), CH(01), CH(01), CH(60), CH(192)


            rec% = 1%            
        read_comp6_done
        return

        write_upload_comp6
            write #26, using L66100, fields$(1%), comma$,        ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$,       ~
                              fields$(5%), comma$, fields$(6%),  ~
                              comma$

L66100:               FMT CH(03), CH(01), CH(01), CH(01), CH(01), ~
                          CH(01), CH(01), CH(01), CH(60), CH(01), ~
                          CH(190), CH(01)
        return



REM     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
REM     Create AWDCOMP7 file from COMPCOL7 File              
REM     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


        create_comp7
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             number_of_fields%   = 6%
             file$   = "AWDCOMP7"
             ff% = 27%  
             gosub open_file
      
             gosub read_comp7
                   goto L67000
        create_comp7_nxt
             gosub read_comp7_nxt
             if rec% <> 1% then goto comp7_done
L67000:

               gosub write_upload_comp7
               goto create_comp7_nxt
        return
        comp7_done
        return


        read_comp7 
            init(" ") readkey$, fields$()
            mat num_fields = zer
            rec% = 0%
            read #8, key > readkey$, using L67010, readkey$,       ~
                            eod goto read_comp7_done
L67010:          FMT CH(06)

                goto L67020
        read_comp7_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #8, using L67010, readkey$, eod goto read_comp7_done



L67020:         cnt% = cnt% + 1%
            goto L67030
            if mod(cnt%,50%) <> 0% then goto L67030
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L67030:
                get #8, using L67040, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%), fields$(6%)

                     if str(fields$(6%),1%,256%)  < " " then     ~
                           str(fields$(6%),1%,256%) = " "

L67040:           FMT CH(03), CH(01), CH(01), CH(01), CH(60), CH(192)


            rec% = 1%            
        read_comp7_done
        return

        write_upload_comp7
            write #27, using L67100, fields$(1%), comma$,        ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$,       ~
                              fields$(5%), comma$, fields$(6%),  ~
                              comma$

L67100:               FMT CH(03), CH(01), CH(01), CH(01), CH(01), ~
                          CH(01), CH(01), CH(01), CH(60), CH(01), ~
                          CH(190), CH(01)
        return





        open_error
           comp% = 2%
           hdr$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = "(Open Error) - File = " & filename$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

        

        open_file
            init(" ") library$, volume$
            library$        = "ORAFILES"
            volume$         = "CARLO2"


             open nodisplay #ff%, output, space = 100%,                  ~
                dpack   = 100%, ipack = 100%, file = file$,              ~
                library = library$, volume = volume$, blocks = 5%
        return
       



        REM *************************************************************~
            *                          E X I T                          *~
            *-----------------------------------------------------------*~
            * Terminates execution (files closed automatically).        *~
            *-----------------------------------------------------------*

        exit_program
REM            call "SHOSTAT" ("One Moment Please")

            end
