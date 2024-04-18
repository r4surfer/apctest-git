        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN35                             *~
            *  Creation Date     - 06/15/06                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New Program to Build | delimited     *~
            *                      files to import into Oracle.         *~
            *                                                           *~
            *                      This program only creates the plan   *~
            *                      header or PAYMASTR & PAYLINES file to*~
            *                      import!!                             *~
            *                                                           *~
            *                      This program only creates files      *~
            *                      need for WW import                   *~
            *                      AWDPYMR  - PAYMASTR                  *~
            *                      AWDPYLN  - PAYLINES                  *~
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
            * 06/15/06 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            *************************************************************

        dim                              /* (paymastr) - FILE          */~
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
            * # 1 ! PAYMASTR ! PAYABLES MAIN HEADER FILE.               *~
            * # 2 ! PAYLINES ! PAYABLES LINE ITEMS FILE                 *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select  #1, "PAYMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 350,                                   ~
                        keypos = 1, keylen = 25

            select  #2, "PAYLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 541,                                   ~
                        keypos = 36, keylen = 28,                        ~
                        alternate key 1, keypos = 1, keylen = 63,        ~
                                  key 2, keypos = 17, keylen = 47

            select #4, "GENCODES",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #21, "AWDPYMN",                                       ~
                        varc,     indexed, recsize = 258,                ~
                        keypos = 1,    keylen = 27


            select #22, "AWDPYLN",                                       ~
                        varc,     indexed, recsize = 464,                ~
                        keypos = 39,    keylen = 31

REM            call "SHOSTAT" ("Initialization")

            filename$ = "PAYMASTR" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "PAYLINES" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error

            filename$ = "GENCODES" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error



            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************

            call "EXTRACT" addr("ID", userid$)
            date$ = date

            schema_err%, schema% = 0%
            init(" ") schema$
            call "SCHEMA" (schema$, schema%, #4, schema_err%)


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

            gosub create_paymastr
            gosub create_paylines 

        return





        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") readkey$, fields$(), filename$, cnt$, hdr$, msg$(),~
                      date$,  library$, volume$, readdate$

        return

        REM *************************************************************~
            *************************************************************


        create_paymastr
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDPYMN" 
             ff% = 21%  
             gosub open_file
      
             gosub read_paymastr
                   goto L61030
        create_paymastr_nxt 
             gosub read_paymastr_nxt
             if rec% <> 1% then goto paymastr_done
L61030:

               gosub write_upload_paymastr
               goto create_paymastr_nxt
        return
        paymastr_done
        return

        read_paymastr
            init(" ") fields$(), readkey$

            mat num_fields = zer
            rec% = 0%
            read #1, key > readkey$, eod goto read_paymastr_done
                goto L61300
        read_paymastr_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #1, eod goto read_paymastr_done

L61300:         cnt% = cnt% + 1%
            goto L63155
            if mod(cnt%,50%) <> 0% then goto L63155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L63155:
                get #1, using L61140, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%), fields$(6%),  ~
                                      fields$(7%), fields$(8%),  ~
                                      fields$(9%), num_fields(10%),  ~
                                      fields$(11%), fields$(12%),  ~
                                      fields$(13%), fields$(14%),  ~
                                      fields$(15%), num_fields(16%),  ~
                                      num_fields(17%), fields$(18%),  ~
                                      num_fields(19%), num_fields(20%),  ~
                                      fields$(21%), fields$(22%),  ~
                                      fields$(23%), num_fields(24%)

L61140:           FMT CH(09), CH(16), CH(16), CH(06), CH(09), CH(09), CH(01),~
                      CH(06), CH(06), PD(15,4), CH(06), CH(06), CH(03),      ~
                      CH(06), CH(03), PD(15,4), PD(15,4), CH(20), PD(15,4),  ~
                      PD(15,4), CH(04), CH(01), CH(04), BI(04), CH(178)       

            call "DATFMTC" (fields$(04%), date%, fields$(04%))
            call "DATFMTC" (fields$(08%), date%, fields$(08%))
            call "DATFMTC" (fields$(09%), date%, fields$(09%))
            call "DATFMTC" (fields$(11%), date%, fields$(11%))
            call "DATFMTC" (fields$(12%), date%, fields$(12%))
            call "DATFMTC" (fields$(14%), date%, fields$(14%))

            convert num_fields(10%) to fields$(10%), pic(-#######0.0000)
            convert num_fields(16%) to fields$(16%), pic(-#######0.0000)
            convert num_fields(17%) to fields$(17%), pic(-#######0.0000)
            convert num_fields(19%) to fields$(19%), pic(-#######0.0000)
            convert num_fields(20%) to fields$(20%), pic(-#######0.0000)

            convert num_fields(24%) to fields$(24%), pic(##########)

            rec% = 1%            
        read_paymastr_done
        return


        write_upload_paymastr
            write #21, using L65400, fields$(1%), comma$,        ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$,       ~
                              fields$(5%), comma$, fields$(6%),  ~
                              comma$, fields$(7%), comma$,       ~
                              fields$(8%), comma$, fields$(9%),  ~
                              comma$, fields$(10%), comma$,      ~
                              fields$(11%), comma$, fields$(12%),~
                              comma$, fields$(13%), comma$,      ~
                              fields$(14%), comma$, fields$(15%),~
                              comma$, fields$(16%), comma$,      ~
                              fields$(17%), comma$, fields$(18%),~
                              comma$, fields$(19%), comma$,      ~
                              fields$(20%), comma$, fields$(21%),~
                              comma$, fields$(22%), comma$,      ~
                              fields$(23%), comma$, fields$(24%),~
                              comma$


L65400:               FMT CH(09), CH(01), CH(16), CH(01), CH(16), CH(01), ~
                          CH(10), CH(01), CH(09), CH(01), CH(09), CH(01), ~
                          CH(01), CH(01), CH(10), CH(01), CH(10), CH(01), ~
                          CH(14), CH(01), CH(10), CH(01), CH(10), CH(01), ~
                          CH(03), CH(01), CH(10), CH(01), CH(03), CH(01), ~
                          CH(14), CH(01), CH(14), CH(01), CH(20), CH(01), ~
                          CH(14), CH(01), CH(14), CH(01), CH(04), CH(01), ~
                          CH(01), CH(01), CH(04), CH(01), CH(09), CH(01)



        return



        create_paylines
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDPYLN" 
             ff% = 22%  
             gosub open_file
      
             gosub read_paylines
                   goto L62030
        create_paylines_nxt 
             gosub read_paylines_nxt
             if rec% <> 1% then goto paylines_done
L62030:

               gosub write_upload_paylines
               goto create_paylines_nxt
        return
        paylines_done
        return

        read_paylines
            init(" ") fields$(), readkey$
            
            mat num_fields = zer
            rec% = 0%
            read #2, key > readkey$, eod goto read_paylines_done
                goto L61400
        read_paylines_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #2, eod goto read_paylines_done

L61400:         cnt% = cnt% + 1%
            goto L64155
            if mod(cnt%,50%) <> 0% then goto L64155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L64155:
                get #2, using L61440, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%), fields$(6%),  ~
                                      fields$(7%), fields$(8%),  ~
                                      num_fields(9%), num_fields(10%),  ~
                                      fields$(11%), fields$(12%),  ~
                                      fields$(13%), num_fields(14%),  ~
                                      fields$(15%), num_fields(16%),  ~
                                      fields$(17%), fields$(18%),  ~
                                      num_fields(19%), num_fields(20%),  ~
                                      num_fields(21%), num_fields(22%),  ~
                                      num_fields(23%), num_fields(24%),  ~
                                      num_fields(25%), num_fields(26%),  ~
                                      num_fields(27%), num_fields(28%),  ~
                                      num_fields(29%), num_fields(30%),  ~
                                      num_fields(31%), num_fields(32%),  ~
                                      fields$(33%), fields$(34%),  ~
                                      fields$(35%), fields$(36%)
                                      


L61440:           FMT CH(16), CH(16), CH(03), CH(09), CH(16), CH(03), CH(09),~
                      CH(25), PD(15,4), PD(15,4), CH(08), CH(03), CH(06),    ~
                      PD(15,7), CH(03), PD(15,4), CH(04), CH(25), BI(04),    ~
	              PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),      ~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),      ~
                      PD(15,4), PD(15,4), PD(15,4), CH(09), CH(16), CH(04),  ~
                      CH(226)        


            convert num_fields(9%) to fields$(9%), pic(-#######0.0000)
            convert num_fields(10%) to fields$(10%), pic(-#######0.0000)
            convert num_fields(14%) to fields$(14%), pic(-####0.0000000)
            convert num_fields(16%) to fields$(16%), pic(-#######0.0000)

            convert num_fields(19%) to fields$(19%), pic(##########)

            for i% = 20% to 32%
              convert num_fields(i%) to fields$(i%), pic(-#######0.0000)
            next i%


            rec% = 1%            
        read_paylines_done
        return


        write_upload_paylines
            write #22, using L65500, fields$(1%), comma$,        ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$,       ~
                              fields$(5%), comma$, fields$(6%),  ~
                              comma$, fields$(7%), comma$,       ~
                              fields$(8%), comma$, fields$(9%),  ~
                              comma$, fields$(10%), comma$,      ~
                              fields$(11%), comma$, fields$(12%),~
                              comma$, fields$(13%), comma$,      ~
                              fields$(14%), comma$, fields$(15%),~
                              comma$, fields$(16%), comma$,      ~
                              fields$(17%), comma$, fields$(18%),~
                              comma$, fields$(19%), comma$,      ~
                              fields$(20%), comma$, fields$(21%),~
                              comma$, fields$(22%), comma$,      ~
                              fields$(23%), comma$, fields$(24%),~
                              comma$, fields$(25%), comma$,      ~
                              fields$(26%), comma$, fields$(27%),~
                              comma$, fields$(28%), comma$,      ~
                              fields$(29%), comma$, fields$(30%),~
                              comma$, fields$(31%), comma$,      ~
                              fields$(32%), comma$, fields$(33%),~
                              comma$, fields$(34%), comma$,      ~
                              fields$(35%), comma$

L65500:               FMT CH(16), CH(01), CH(16), CH(01), CH(03), CH(01), ~
                          CH(09), CH(01), CH(16), CH(01), CH(03), CH(01), ~
                          CH(09), CH(01), CH(25), CH(01), CH(14), CH(01), ~
                          CH(14), CH(01), CH(08), CH(01), CH(03), CH(01), ~
                          CH(06), CH(01), CH(14), CH(01), CH(03), CH(01), ~
                          CH(14), CH(01), CH(04), CH(01), CH(25), CH(01), ~
                          CH(09), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                          CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                          CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                          CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                          CH(14), CH(01), CH(14), CH(01), CH(09), CH(01), ~
                          CH(16), CH(01), CH(04), CH(01)

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
            if schema% = 1% then volume$ = "CARLO2"
            if schema% = 2% then volume$ = "NE2"

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
