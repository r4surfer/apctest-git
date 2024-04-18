        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN34                             *~
            *  Creation Date     - 06/12/06                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New Program to Build | delimited     *~
            *                      files to import into Oracle.         *~
            *                                                           *~
            *                      This program only creates the plan   *~
            *                      header or VBKMASTR & VBKLINES file to*~
            *                      import!!                             *~
            *                                                           *~
            *                      This program only creates files      *~
            *                      need for WW import                   *~
            *                      AWDVBMR  - VBKMASTR                  *~
            *                      AWDVBLN  - VBKLINES                  *~
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
            * 06/12/06 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            *************************************************************

        dim                              /* (vbkmastr) - FILE          */~
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
            * # 1 ! VBKMASTR ! Backlog main header file                 *~
            * # 2 ! VBKLINES ! Backlog line item file                   *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************



            select # 1, "VBKMASTR",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  1030,                                 ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  10,  keylen =  16

            select # 2, "VBKLINES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =  700,                                  ~
                        keypos =    1, keylen =  28,                     ~
                        alt key 1, keypos = 333, keylen = 20, dup

            select #4, "GENCODES",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #21, "AWDVBMN",                                       ~
                        varc,     indexed, recsize = 1264,               ~
                        keypos = 1,    keylen = 27


            select #22, "AWDVBLN",                                       ~
                        varc,     indexed, recsize = 1024,               ~
                        keypos = 1,    keylen = 31

REM            call "SHOSTAT" ("Initialization")

            filename$ = "VBKMASTR" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "VBKLINES" : call "EWDOPEN" (#2, filename$, err%)
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

            gosub create_vbkmastr
            gosub create_vbklines 

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


        create_vbkmastr
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDVBMN" 
             ff% = 21%  
             gosub open_file
      
             gosub read_vbkmastr
                   goto L61030
        create_vbkmastr_nxt 
             gosub read_vbkmastr_nxt
             if rec% <> 1% then goto vbkmastr_done
L61030:

               gosub write_upload_vbkmastr
               goto create_vbkmastr_nxt
        return
        vbkmastr_done
        return

        read_vbkmastr
            init(" ") fields$(), readkey$

            mat num_fields = zer
            rec% = 0%
            read #1, key > readkey$, eod goto read_vbkmastr_done
                goto L61300
        read_vbkmastr_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #1, eod goto read_vbkmastr_done

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
                                      fields$(9%), fields$(10%),  ~
                                      fields$(11%), fields$(12%),  ~
                                      fields$(13%), fields$(14%),  ~
                                      fields$(15%), fields$(16%),  ~
                                      fields$(17%), fields$(18%),  ~
                                      fields$(19%), fields$(20%),  ~
                                      fields$(21%), fields$(22%),  ~
                                      fields$(23%), fields$(24%),  ~
                                      fields$(25%), fields$(26%),  ~
                                      num_fields(27%), fields$(28%),  ~
                                      fields$(29%), fields$(30%),  ~
                                      fields$(31%), fields$(32%),  ~
                                      fields$(33%), fields$(34%),  ~
                                      fields$(35%), fields$(36%),  ~
                                      num_fields(37%), num_fields(38%),  ~
                                      fields$(39%), fields$(40%),  ~
                                      fields$(41%), fields$(42%), ~
                                      fields$(43%), fields$(44%),  ~
                                      fields$(45%), fields$(46%),  ~
                                      fields$(47%), fields$(48%),  ~
                                      fields$(49%), fields$(50%) 

L61140:           FMT CH(09), CH(16), CH(30), CH(30), CH(30), CH(30), ~
                      CH(30), CH(30), CH(20), CH(09), CH(16), CH(200),~
                      CH(06), CH(06), CH(08), CH(06), CH(03), CH(06), ~
                      CH(03), CH(01), CH(01), CH(30), CH(01), CH(30), ~
                      CH(04), CH(06),                                 ~
                      /* Num 27 */  BI(04), CH(02), CH(06), CH(10), ~ 
                      CH(06), CH(03), CH(06), CH(03), CH(06), CH(03), ~ 
                      PD(15,4), PD(15,4), CH(10),     ~
                      CH(30), CH(30), CH(30), CH(30), CH(30), CH(30), ~
                      CH(01), CH(09), CH(10), CH(10), CH(184)


            call "DATFMTC" (fields$(13%), date%, fields$(13%))
            call "DATFMTC" (fields$(14%), date%, fields$(14%))
            call "DATFMTC" (fields$(16%), date%, fields$(16%))
            call "DATFMTC" (fields$(18%), date%, fields$(18%))

            convert num_fields(27%) to fields$(27%), pic(##########)


            call "DATFMTC" (fields$(29%), date%, fields$(29%))
            call "DATFMTC" (fields$(31%), date%, fields$(31%))
            call "DATFMTC" (fields$(33%), date%, fields$(33%))
            call "DATFMTC" (fields$(35%), date%, fields$(35%))

            convert num_fields(37%) to fields$(37%), pic(-#######0.0000)
            convert num_fields(38%) to fields$(38%), pic(-#######0.0000)


            rec% = 1%            
        read_vbkmastr_done
        return


        write_upload_vbkmastr
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
                              comma$, fields$(37%), comma$,      ~
                              fields$(38%), comma$, fields$(39%),~
                              comma$, fields$(40%), comma$,      ~
                              fields$(41%), comma$, fields$(42%),~
                              comma$, fields$(43%), comma$,      ~
                              fields$(44%), comma$, fields$(45%),~
                              comma$, fields$(46%), comma$,      ~
                              fields$(47%), comma$, fields$(48%),~
                              comma$, fields$(49%), comma$


L65400:               FMT CH(09), CH(01), CH(16), CH(01), CH(30), CH(01), ~
                          CH(30), CH(01), CH(30), CH(01), CH(30), CH(01), ~
                          CH(30), CH(01), CH(30), CH(01), CH(20), CH(01), ~
                          CH(09), CH(01), CH(16), CH(01), CH(200), CH(01), ~
                          CH(10), CH(01), CH(10), CH(01), CH(08), CH(01), ~
                          CH(10), CH(01), CH(03), CH(01), CH(10), CH(01), ~
                          CH(03), CH(01), CH(01), CH(01), CH(01), CH(01), ~
                          CH(30), CH(01), CH(01), CH(01), CH(30), CH(01), ~
                          CH(04), CH(01), CH(06), CH(01),                 ~
                          /* num 27 */ CH(09), CH(01), ~ 
                          CH(02), CH(01), CH(10), CH(01), CH(10), CH(01), ~ 
                          CH(10), CH(01), CH(03), CH(01), CH(10), CH(01), ~
                          CH(03), CH(01), CH(10), CH(01), CH(03), CH(01), ~ 
                          CH(14), CH(01), ~
                          CH(14), CH(01), CH(10), CH(01), CH(30), CH(01), ~
                          CH(30), CH(01), CH(30), CH(01), CH(30), CH(01), ~
                          CH(30), CH(01), CH(30), CH(01), CH(01), CH(01), ~
                          CH(09), CH(01), CH(10), CH(01), CH(10), CH(01)



        return



        create_vbklines
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDVBLN" 
             ff% = 22%  
             gosub open_file
      
             gosub read_vbklines
                   goto L62030
        create_vbklines_nxt 
             gosub read_vbklines_nxt
             if rec% <> 1% then goto vbklines_done
L62030:

               gosub write_upload_vbklines
               goto create_vbklines_nxt
        return
        vbklines_done
        return

        read_vbklines
            init(" ") fields$(), readkey$
            
            mat num_fields = zer
            rec% = 0%
            read #2, key > readkey$, eod goto read_vbklines_done
                goto L61400
        read_vbklines_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #2, eod goto read_vbklines_done

L61400:         cnt% = cnt% + 1%
            goto L64155
            if mod(cnt%,50%) <> 0% then goto L64155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L64155:
                get #2, using L61440, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%), fields$(6%),  ~
                                      fields$(7%), num_fields(8%),  ~
                                      num_fields(9%), num_fields(10%),  ~
                                      num_fields(11%), num_fields(12%),  ~
                                      fields$(13%), fields$(14%),  ~
                                      fields$(15%), fields$(16%),  ~
                                      fields$(17%), fields$(18%),  ~
                                      fields$(19%), fields$(20%),  ~
                                      fields$(21%), fields$(22%),  ~
                                      fields$(23%), fields$(24%),  ~
                                      fields$(25%), num_fields(26%),  ~
                                      fields$(27%), num_fields(28%),  ~
                                      fields$(29%), fields$(30%),  ~
                                      fields$(31%), fields$(32%),  ~
                                      fields$(33%), fields$(34%),  ~
                                      fields$(35%), fields$(36%),  ~
                                      num_fields(37%), num_fields(38%),  ~
                                      num_fields(39%), num_fields(40%),  ~
                                      num_fields(41%), num_fields(42%), ~
                                      num_fields(43%), num_fields(44%),  ~
                                      num_fields(45%), num_fields(46%),  ~
                                      num_fields(47%), num_fields(48%),  ~
                                      num_fields(49%), num_fields(50%),  ~
                                      num_fields(51%), num_fields(52%),  ~
                                      num_fields(53%), num_fields(54%),  ~
                                      num_fields(55%), fields$(56%),  ~
                                      fields$(57%), fields$(58%),  ~
                                      fields$(59%), fields$(60%)
                                      


L61440:           FMT CH(09), CH(16), CH(03), CH(03), CH(25), CH(32), CH(04),~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,7), PD(15,4),      ~
                      CH(09), CH(06), CH(06), CH(06), CH(06), CH(08), CH(03),~
                      CH(01), CH(03), CH(16), CH(25), CH(66), CH(04),        ~
                      PD(15,4), CH(06), BI(04), CH(01), CH(02), CH(20),      ~
                      CH(20), CH(05), CH(06), CH(06), CH(03),                ~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),      ~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),      ~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),      ~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4),                ~
                      CH(09), CH(27), CH(16), CH(04), CH(120)




            convert num_fields(8%) to fields$(8%), pic(-#######0.0000)
            convert num_fields(9%) to fields$(9%), pic(-#######0.0000)
            convert num_fields(10%) to fields$(10%), pic(-#######0.0000)
            convert num_fields(11%) to fields$(11%), pic(-####0.0000000)


            call "DATFMTC" (fields$(14%), date%, fields$(14%))
            call "DATFMTC" (fields$(15%), date%, fields$(15%))
            call "DATFMTC" (fields$(16%), date%, fields$(16%))

            convert num_fields(26%) to fields$(26%), pic(-####0.0000000)

            call "DATFMTC" (fields$(27%), date%, fields$(27%))

            convert num_fields(28%) to fields$(28%), pic(##########)

            call "DATFMTC" (fields$(34%), date%, fields$(34%))
            call "DATFMTC" (fields$(35%), date%, fields$(35%))



            for i% = 37% to 55%
              convert num_fields(i%) to fields$(i%), pic(-#######0.0000)
            next i%


            rec% = 1%            
        read_vbklines_done
        return


        write_upload_vbklines
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
                              fields$(35%), comma$, fields$(36%),~
                              comma$, fields$(37%), comma$,      ~
                              fields$(38%), comma$, fields$(39%),~
                              comma$, fields$(40%), comma$,      ~
                              fields$(41%), comma$, fields$(42%),~
                              comma$, fields$(43%), comma$,      ~
                              fields$(44%), comma$, fields$(45%),~
                              comma$, fields$(46%), comma$,      ~
                              fields$(47%), comma$, fields$(48%),~
                              comma$, fields$(49%), comma$,      ~
                              fields$(50%), comma$, fields$(51%),~
                              comma$, fields$(52%), comma$,      ~
                              fields$(53%), comma$, fields$(54%),~
                              comma$, fields$(55%), comma$,      ~
                              fields$(56%), comma$, fields$(57%),~
                              comma$, fields$(58%), comma$,      ~
                              fields$(59%), comma$

L65500:               FMT CH(09), CH(01), CH(16), CH(01), CH(03), CH(01),    ~
                          CH(03), CH(01), CH(25), CH(01), CH(32), CH(01),    ~
                          CH(04), CH(01), CH(14), CH(01), CH(14), CH(01),    ~
                          CH(14), CH(01), CH(14), CH(01), CH(14), CH(01),    ~
                          CH(09), CH(01), CH(10), CH(01), CH(10), CH(01),    ~
                          CH(10), CH(01), CH(06), CH(01), CH(08), CH(01),    ~
                          CH(03), CH(01), CH(01), CH(01), CH(03), CH(01),    ~
                          CH(16), CH(01), CH(25), CH(01), CH(66), CH(01),    ~
                          CH(04), CH(01), CH(14), CH(01), CH(10), CH(01),    ~
                          CH(09), CH(01), CH(01), CH(01), CH(02), CH(01),    ~
                          CH(20), CH(01), CH(20), CH(01), CH(05), CH(01),    ~
                          CH(10), CH(01), CH(10), CH(01), CH(03), CH(01),    ~
                          CH(14), CH(01), CH(14), CH(01), CH(14), CH(01),    ~
                          CH(14), CH(01), CH(14), CH(01), CH(14), CH(01),    ~
                          CH(14), CH(01), CH(14), CH(01), CH(14), CH(01),    ~
                          CH(14), CH(01), CH(14), CH(01), CH(14), CH(01),    ~
                          CH(14), CH(01), CH(14), CH(01), CH(14), CH(01),    ~
                          CH(14), CH(01), CH(14), CH(01), CH(14), CH(01),    ~
                          CH(14), CH(01), CH(09), CH(01), CH(27), CH(01),    ~
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
