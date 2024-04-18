        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN32                             *~
            *  Creation Date     - 06/02/06                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New Program to Build | delimited     *~
            *                      files to import into Oracle.         *~
            *                                                           *~
            *                      This program only creates the plan   *~
            *                      header or APCPLNOR file to import!!  *~
            *                                                           *~
            *                      This program only creates files      *~
            *                      need for WW import                   *~
            *                      AWDGLMN  - GLMAIN                    *~
            *                      AWDGLDT  - GLDETAL                   *~
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
            * 06/02/06 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            *************************************************************

        dim                              /* (glmain  ) - FILE          */~
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
            * #01 ! GLMAIN   ! General ledger main file                 *~
            * #02 ! GLDETAIL ! General ledger detail file               *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************



            select #01, "GLMAIN",                                        ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 300,                                   ~
                        keypos = 1,                                      ~
                        keylen = 9

            select  #02, "GLDETAIL",     /* GENERAL LEDGER DETAILS     */~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 160,                                   ~
                        keypos = 1, keylen = 26

            select #4, "GENCODES",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #21, "AWDGLMN",                                       ~
                        varc,     indexed, recsize = 600,                ~
                        keypos = 1,    keylen = 10


            select #22, "AWDGLDT",                                       ~
                        varc,     indexed, recsize = 250,                ~
                        keypos = 1,    keylen = 39

REM            call "SHOSTAT" ("Initialization")

            filename$ = "GLMAIN"   : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GLDETAIL" : call "EWDOPEN" (#2, filename$, err%)
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

                                                          /* (AWD004)  */

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

            gosub create_glmain   
            gosub create_gldetail 

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


        create_glmain   
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDGLMN" 
             ff% = 21%  
             gosub open_file
      
             gosub read_glmain  
                   goto L61030
        create_glmain_nxt 
             gosub read_glmain_nxt
             if rec% <> 1% then goto glmain_done
L61030:

               gosub write_upload_glmain
               goto create_glmain_nxt
        return
        glmain_done
        return

        read_glmain
            init(" ") fields$(), readkey$

            mat num_fields = zer
            rec% = 0%
            read #1, key > readkey$, eod goto read_glmain_done
                goto L61300
        read_glmain_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #1, eod goto read_glmain_done

L61300:         cnt% = cnt% + 1%
            goto L63155
            if mod(cnt%,50%) <> 0% then goto L63155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L63155:
                get #1, using L61140, fields$(1%), fields$(2%),  ~
                                      fields$(3%), num_fields(4%),  ~
                                      num_fields(5%), num_fields(6%),  ~
                                      num_fields(7%), num_fields(8%),   ~
                                      num_fields(9%), num_fields(10%),  ~
                                      num_fields(11%), num_fields(12%), ~
                                      num_fields(13%), num_fields(14%), ~
                                      num_fields(15%), num_fields(16%), ~
                                      num_fields(17%), num_fields(18%), ~
                                      num_fields(19%), num_fields(20%), ~
                                      num_fields(21%), num_fields(22%), ~
                                      num_fields(23%), num_fields(24%), ~
                                      num_fields(23%), num_fields(24%), ~
                                      num_fields(25%), num_fields(26%), ~
                                      num_fields(27%), num_fields(28%), ~
                                      num_fields(29%), num_fields(30%), ~
                                      num_fields(31%), num_fields(32%), ~
                                      num_fields(33%), num_fields(34%), ~
                                      num_fields(35%), num_fields(35%), ~
                                      num_fields(36%), num_fields(37%)   


L61140:           FMT CH(09), CH(30), CH(01), BI(01), BI(03),          ~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),~ 
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),~ 
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),~ 
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),~ 
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),~ 
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),~ 
                      PD(15,4), PD(15,4)



            convert num_fields(4%) to fields$(4%), pic(##########)

            convert num_fields(5%) to fields$(5%), pic(##########)


            convert num_fields(6%) to fields$(6%), pic(-#######0.0000)
            convert num_fields(7%) to fields$(7%), pic(-#######0.0000)
            convert num_fields(8%) to fields$(8%), pic(-#######0.0000)
            convert num_fields(9%) to fields$(9%), pic(-#######0.0000)

            convert num_fields(10%) to fields$(10%), pic(-#######0.0000)
            convert num_fields(11%) to fields$(11%), pic(-#######0.0000)
            convert num_fields(12%) to fields$(12%), pic(-#######0.0000)
            convert num_fields(13%) to fields$(13%), pic(-#######0.0000)
            convert num_fields(14%) to fields$(14%), pic(-#######0.0000)
            convert num_fields(15%) to fields$(15%), pic(-#######0.0000)
            convert num_fields(16%) to fields$(16%), pic(-#######0.0000)
            convert num_fields(17%) to fields$(17%), pic(-#######0.0000)
            convert num_fields(18%) to fields$(18%), pic(-#######0.0000)
            convert num_fields(19%) to fields$(19%), pic(-#######0.0000)
            convert num_fields(20%) to fields$(20%), pic(-#######0.0000)


            convert num_fields(21%) to fields$(21%), pic(-#######0.0000)
            convert num_fields(22%) to fields$(22%), pic(-#######0.0000)
            convert num_fields(23%) to fields$(23%), pic(-#######0.0000)
            convert num_fields(24%) to fields$(24%), pic(-#######0.0000)
            convert num_fields(25%) to fields$(25%), pic(-#######0.0000)
            convert num_fields(26%) to fields$(26%), pic(-#######0.0000)
            convert num_fields(27%) to fields$(27%), pic(-#######0.0000)
            convert num_fields(28%) to fields$(28%), pic(-#######0.0000)
            convert num_fields(29%) to fields$(29%), pic(-#######0.0000)
            convert num_fields(30%) to fields$(30%), pic(-#######0.0000)

            convert num_fields(31%) to fields$(31%), pic(-#######0.0000)
            convert num_fields(32%) to fields$(32%), pic(-#######0.0000)
            convert num_fields(33%) to fields$(33%), pic(-#######0.0000)
            convert num_fields(34%) to fields$(34%), pic(-#######0.0000)
            convert num_fields(35%) to fields$(35%), pic(-#######0.0000)
            convert num_fields(36%) to fields$(36%), pic(-#######0.0000)
            convert num_fields(37%) to fields$(37%), pic(-#######0.0000)

            rec% = 1%            
        read_glmain_done
        return


        write_upload_glmain
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
                              comma$, fields$(25%), comma$,      ~
                              fields$(26%), comma$, fields$(27%),~
                              comma$, fields$(28%), comma$,      ~
                              fields$(29%), comma$, fields$(30%),~
                              comma$, fields$(31%), comma$,      ~
                              fields$(32%), comma$, fields$(33%),~
                              comma$, fields$(34%), comma$,      ~
                              fields$(35%), comma$, fields$(36%),~
                              comma$, fields$(37%), comma$

L65400:               FMT CH(09), CH(01), CH(30), CH(01), CH(01),     ~
                      CH(01), CH(10), CH(01), CH(10), CH(01),         ~
                      CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                      CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                      CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                      CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                      CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                      CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                      CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                      CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                      CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                      CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                      CH(14), CH(01), CH(14), CH(01)

        return



        create_gldetail 
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDGLDT" 
             ff% = 22%  
             gosub open_file
      
             gosub read_gldetail
                   goto L62030
        create_gldetail_nxt 
             gosub read_gldetail_nxt
             if rec% <> 1% then goto gldetail_done
L62030:

               gosub write_upload_gldetail
               goto create_gldetail_nxt
        return
        gldetail_done
        return

        read_gldetail
            init(" ") fields$(), readkey$
            readdate$ = "20050101"
            call "DATFMTC" (readdate$)
            call "DATUFMTC" (readdate$)
            
            mat num_fields = zer
            rec% = 0%
            read #2, key > readkey$, eod goto read_gldetail_done
                goto L61400
        read_gldetail_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #2, eod goto read_gldetail_done

L61400:         cnt% = cnt% + 1%
            goto L64155
            if mod(cnt%,50%) <> 0% then goto L64155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L64155:
                get #2, using L61440, fields$(1%), fields$(2%),  ~
                                      num_fields(3%), fields$(4%),  ~
                                      num_fields(5%), num_fields(6%),~
                                      fields$(7%), fields$(8%),  ~
                                      fields$(9%), fields$(10%), ~
                                      fields$(11%), num_fields(12%),~
                                      fields$(13%), fields$(14%)
                                      


L61440:           FMT CH(16), CH(06), BI(04), CH(02), PD(15,4),       ~
                      PD(15,4), CH(30), CH(34), CH(04), CH(32),       ~
                      CH(03), BI(04), CH(03), CH(06)                                


REM  Only '05 and '06 transactions
            if fields$(2%) < str(readdate$,1%,6%) then goto read_gldetail_nxt

            call "DATFMTC" (fields$(2%), date%, fields$(2%))
            call "DATFMTC" (fields$(14%), date%, fields$(14%))


            convert num_fields(3%) to fields$(3%), pic(##########)

REM            convert num_fields(4%) to fields$(4%), pic(##########)


            convert num_fields(5%) to fields$(5%), pic(-#######0.0000)
            convert num_fields(6%) to fields$(6%), pic(-#######0.0000)


            convert num_fields(12%) to fields$(12%), pic(##########)


            rec% = 1%            
        read_gldetail_done
        return


        write_upload_gldetail
            write #22, using L65500, fields$(1%), comma$,        ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$,       ~
                              fields$(5%), comma$, fields$(6%),  ~
                              comma$, fields$(7%), comma$,       ~
                              fields$(8%), comma$, fields$(9%),  ~
                              comma$, fields$(10%), comma$,      ~
                              fields$(11%), comma$, fields$(12%),~
                              comma$, fields$(13%), comma$,      ~
                              fields$(14%), comma$

L65500:               FMT CH(16), CH(01), CH(10), CH(01), CH(10),     ~
                      CH(01), CH(02), CH(01), CH(14), CH(01), CH(14), ~
                      CH(01), CH(30), CH(01), CH(34), CH(01), CH(04), ~
                      CH(01), CH(32), CH(01), CH(03), CH(01), CH(10), ~
                      CH(01), CH(03), CH(01), CH(10), CH(01)

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
