        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN33                             *~
            *  Creation Date     - 06/02/06                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New Program to Build | delimited     *~
            *                      files to import into Oracle.         *~
            *                                                           *~
            *                      This program only creates the plan   *~
            *                      header or ARIMASTR & ARILINES file to*~
            *                      import!!                             *~
            *                                                           *~
            *                      This program only creates files      *~
            *                      need for WW import                   *~
            *                      AWDARMR  - ARIMASTR                  *~
            *                      AWDARLN  - ARILINES                  *~
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
            * 06/08/06 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            *************************************************************

        dim                              /* (arimastr) - FILE          */~
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
            * #01 ! ARIMASTR ! Invoice Master File                      *~
            * #02 ! ARILINES ! Invoice Line Items File                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************



            select #01, "ARIMASTR",                                      ~
                        varc, indexed, recsize = 2000,                   ~
                        keypos = 1, keylen =  17,                        ~
                        alt key 1, keypos = 10, keylen =  8, dup,        ~
                            key 2, keypos = 18, keylen = 16, dup,        ~
                            key 3, keypos = 34, keylen = 16, dup,        ~
                            key 4, keypos = 1783, keylen = 26

            select #02, "ARILINES",                                      ~
                        varc, indexed, recsize = 750,                    ~
                        keypos = 1, keylen = 20

            select #4, "GENCODES",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #21, "AWDARMN",                                       ~
                        varc,     indexed, recsize = 1264,               ~
                        keypos = 1,    keylen = 19


            select #22, "AWDARLN",                                       ~
                        varc,     indexed, recsize = 1024,               ~
                        keypos = 1,    keylen = 23

REM            call "SHOSTAT" ("Initialization")

            filename$ = "ARIMASTR" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "ARILINES" : call "EWDOPEN" (#2, filename$, err%)
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

            gosub create_arimastr
            gosub create_arilines 

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


        create_arimastr
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDARMN" 
             ff% = 21%  
             gosub open_file
      
             gosub read_arimastr
                   goto L61030
        create_arimastr_nxt 
             gosub read_arimastr_nxt
             if rec% <> 1% then goto arimastr_done
L61030:

               gosub write_upload_arimastr
               goto create_arimastr_nxt
        return
        arimastr_done
        return

        read_arimastr
            init(" ") fields$(), readkey$

            mat num_fields = zer
            rec% = 0%
            read #1, key > readkey$, eod goto read_arimastr_done
                goto L61300
        read_arimastr_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #1, eod goto read_arimastr_done

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
                                      fields$(21%), num_fields(22%),  ~
                                      num_fields(23%), fields$(24%),  ~
                                      fields$(25%), fields$(26%),  ~
                                      fields$(27%), num_fields(28%),  ~
                                      num_fields(29%), num_fields(30%),  ~
                                      fields$(31%), fields$(32%),  ~
                                      fields$(33%), fields$(34%),  ~
                                      fields$(35%), fields$(36%),  ~
                                      fields$(37%), fields$(38%),  ~
                                      fields$(39%), fields$(40%),  ~
                                      fields$(41%), fields$(42%), ~
                                      fields$(43%), fields$(44%),  ~
                                      fields$(45%), fields$(46%),  ~
                                      fields$(47%), fields$(48%),  ~
                                      fields$(49%), fields$(50%),  ~
                                      fields$(51%), fields$(52%),  ~
                                      num_fields(53%), num_fields(54%),  ~
                                      num_fields(55%), num_fields(56%),  ~
                                      num_fields(57%), num_fields(58%),  ~
                                      num_fields(59%), fields$(60%),  ~
                                      fields$(61%), fields$(62%),  ~
                                      fields$(63%), num_fields(64%),  ~
                                      fields$(65%), fields$(66%), ~
                                      num_fields(67%), fields$(68%), ~
                                      fields$(69%), fields$(70%), ~
                                      fields$(71%), num_fields(72%), ~
                                      num_fields(73%), fields$(74%),  ~
                                      fields$(75%), fields$(76%),  ~
                                      fields$(77%), fields$(78%),  ~
                                      fields$(79%), fields$(80%),  ~
                                      fields$(81%), fields$(82%),  ~
                                      fields$(83%), num_fields(84%),  ~
                                      num_fields(85%), fields$(86%)

L61140:           FMT CH(09), CH(08), CH(16), CH(16), CH(03),          ~
                                                                       ~
                      CH(30), CH(30), CH(30), CH(30), CH(30), CH(30),  ~
                      CH(30), CH(30), CH(30), CH(30), CH(30), CH(30),  ~
                                                                       ~
                      CH(06), CH(20), CH(20), CH(06), PD(15,4), PD(15,4),~
                      CH(20),                                            ~
                      CH(04), CH(04), CH(04),                            ~
                      BI(01), BI(01), BI(01),                            ~
                      CH(04), CH(01), CH(06), CH(06), CH(06), CH(06),    ~
                      CH(03),                                            ~
                      CH(20), CH(20), CH(20), CH(20), CH(20),            ~
                      CH(20), CH(20), CH(20), CH(20), CH(20),            ~
                      CH(09), CH(09), CH(09), CH(09), CH(09),            ~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),  ~
                      PD(15,4), PD(15,4),                                ~
                      CH(09), CH(12), CH(03), CH(10), PD(15,4), CH(01),  ~
                      CH(09), BI(04), CH(01), CH(01), CH(01), CH(20),    ~
                      PD(15,4),    /* ONLY 1 B/C 2-20 ARE ALWAY BLANK */ ~
                      POS(1168), PD(15,4),                               ~
                      POS(1408), CH(06),                                 ~
                      POS(1588), CH(06),                                 ~
                      POS(1768), CH(02), CH(09), CH(04), CH(09), CH(09), ~
                      CH(08), CH(01), CH(01), PD(14,4), PD(14,4), CH(174)


            call "DATFMTC" (fields$(18%), date%, fields$(18%))


            convert num_fields(22%) to fields$(22%), pic(-#######0.0000)
            convert num_fields(23%) to fields$(23%), pic(-#######0.0000)

            convert num_fields(28%) to fields$(28%), pic(##########)
            convert num_fields(29%) to fields$(29%), pic(##########)
            convert num_fields(30%) to fields$(30%), pic(##########)

            call "DATFMTC" (fields$(33%), date%, fields$(33%))
            call "DATFMTC" (fields$(34%), date%, fields$(34%))
            call "DATFMTC" (fields$(35%), date%, fields$(35%))
            call "DATFMTC" (fields$(36%), date%, fields$(36%))

            convert num_fields(53%) to fields$(53%), pic(-#######0.0000)
            convert num_fields(54%) to fields$(54%), pic(-#######0.0000)
            convert num_fields(55%) to fields$(55%), pic(-#######0.0000)
            convert num_fields(56%) to fields$(56%), pic(-#######0.0000)
            convert num_fields(57%) to fields$(57%), pic(-#######0.0000)
            convert num_fields(58%) to fields$(58%), pic(-#######0.0000)
            convert num_fields(59%) to fields$(59%), pic(-#######0.0000)

            convert num_fields(64%) to fields$(64%), pic(-#######0.0000)


            convert num_fields(67%) to fields$(67%), pic(##########)

            convert num_fields(72%) to fields$(72%), pic(-#######0.0000)
            convert num_fields(73%) to fields$(73%), pic(-#######0.0000)

            call "DATFMTC" (fields$(74%), date%, fields$(74%))
            call "DATFMTC" (fields$(75%), date%, fields$(75%))

            convert num_fields(84%) to fields$(84%), pic(-#######0.0000)
            convert num_fields(85%) to fields$(85%), pic(-#######0.0000)


            rec% = 1%            
        read_arimastr_done
        return


        write_upload_arimastr
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
                              comma$, fields$(49%), comma$,      ~
                              fields$(50%), comma$, fields$(51%),~
                              comma$, fields$(52%), comma$,      ~
                              fields$(53%), comma$, fields$(54%),~
                              comma$, fields$(55%), comma$,      ~
                              fields$(56%), comma$, fields$(57%),~
                              comma$, fields$(58%), comma$,      ~
                              fields$(59%), comma$, fields$(60%),~
                              comma$, fields$(61%), comma$,      ~
                              fields$(62%), comma$, fields$(63%),~
                              comma$, fields$(64%), comma$,      ~
                              fields$(65%), comma$, fields$(66%),~
                              comma$, fields$(67%), comma$,      ~
                              fields$(68%), comma$, fields$(69%),~
                              comma$, fields$(70%), comma$,      ~
                              fields$(71%), comma$, fields$(72%),~
                              comma$, fields$(73%), comma$,      ~
                              fields$(74%), comma$, fields$(75%),~
                              comma$, fields$(76%), comma$,      ~
                              fields$(77%), comma$, fields$(78%),~
                              comma$, fields$(79%), comma$,      ~
                              fields$(80%), comma$, fields$(81%),~
                              comma$, fields$(82%), comma$,      ~
                              fields$(83%), comma$, fields$(84%),~
                              comma$, fields$(85%), comma$


L65400:               FMT CH(09), CH(01), CH(08), CH(01), CH(16), CH(01), ~
                          CH(16), CH(01), CH(03), CH(01), CH(30), CH(01), ~
                          CH(30), CH(01), CH(30), CH(01), CH(30), CH(01), ~
                          CH(30), CH(01), CH(30), CH(01), CH(30), CH(01), ~
                          CH(30), CH(01), CH(30), CH(01), CH(30), CH(01), ~
                          CH(30), CH(01), CH(30), CH(01), CH(10), CH(01), ~
                          CH(20), CH(01), CH(20), CH(01), CH(06), CH(01), ~
                          CH(14), CH(01), CH(14), CH(01), CH(20), CH(01), ~
                          CH(04), CH(01), CH(04), CH(01), CH(04), CH(01), ~
                          CH(09), CH(01), CH(09), CH(01), CH(09), CH(01), ~
                          CH(04), CH(01), CH(01), CH(01), CH(10), CH(01), ~
                          CH(10), CH(01), CH(10), CH(01), CH(10), CH(01), ~
                          CH(03), CH(01), CH(20), CH(01), CH(20), CH(01), ~
                          CH(20), CH(01), CH(20), CH(01), CH(20), CH(01), ~
                          CH(20), CH(01), CH(20), CH(01), CH(20), CH(01), ~
                          CH(20), CH(01), CH(20), CH(01), CH(09), CH(01), ~
                          CH(09), CH(01), CH(09), CH(01), CH(09), CH(01), ~
                          CH(09), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                          CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                          CH(14), CH(01), CH(14), CH(01), CH(09), CH(01), ~
                          CH(12), ch(01), CH(03), CH(01), CH(10), CH(01), ~
                          CH(14), CH(01), CH(01), CH(01), CH(09), CH(01), ~
                          CH(09), CH(01), CH(01), CH(01), CH(01), CH(01), ~
                          CH(01), CH(01), CH(20), CH(01), CH(14), CH(01), ~
                                    /* ONLY 1 B/C 2-20 ARE ALWAY BLANK */ ~
                          CH(14), CH(01), CH(10), CH(01), CH(10), CH(01), ~
                          CH(02), CH(01), CH(09), CH(01), CH(04), CH(01), ~
                          CH(09), CH(01), CH(09), CH(01), CH(08), CH(01), ~
             CH(01), CH(01), CH(01), CH(01), CH(14), CH(01), CH(14), CH(01)




        return



        create_arilines
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDARLN" 
             ff% = 22%  
             gosub open_file
      
             gosub read_arilines
                   goto L62030
        create_arilines_nxt 
             gosub read_arilines_nxt
             if rec% <> 1% then goto arilines_done
L62030:

               gosub write_upload_arilines
               goto create_arilines_nxt
        return
        arilines_done
        return

        read_arilines
            init(" ") fields$(), readkey$
            readdate$ = "20050101"
            call "DATFMTC" (readdate$)
            call "DATUFMTC" (readdate$)
            
            mat num_fields = zer
            rec% = 0%
            read #2, key > readkey$, eod goto read_arilines_done
                goto L61400
        read_arilines_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #2, eod goto read_arilines_done

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
                                      num_fields(11%), fields$(12%),  ~
                                      fields$(13%), num_fields(14%),  ~
                                      num_fields(15%), num_fields(16%),  ~
                                      num_fields(17%), num_fields(18%),  ~
                                      fields$(19%), fields$(20%),  ~
                                      fields$(21%), fields$(22%),  ~
                                      num_fields(23%), fields$(24%),  ~
                                      fields$(25%), fields$(26%),  ~
                                      fields$(27%), fields$(28%),  ~
                                      fields$(29%), fields$(30%),  ~
                                      fields$(31%), fields$(32%),  ~
                                      fields$(33%), fields$(34%),  ~
                                      fields$(35%), fields$(36%),  ~
                                      fields$(37%), fields$(38%),  ~
                                      fields$(39%), fields$(40%),  ~
                                      fields$(41%), fields$(42%), ~
                                      fields$(43%), fields$(44%),  ~
                                      fields$(45%), fields$(46%),  ~
                                      fields$(47%), fields$(48%),  ~
                                      fields$(49%), fields$(50%),  ~
                                      fields$(51%), fields$(52%),  ~
                                      fields$(53%), fields$(54%),  ~
                                      num_fields(55%), num_fields(56%),  ~
                                      num_fields(57%), num_fields(58%),  ~
                                      num_fields(59%), num_fields(60%),  ~
                                      num_fields(61%), num_fields(62%),  ~
                                      num_fields(63%), num_fields(64%),  ~
                                      num_fields(65%), num_fields(66%), ~
                                      num_fields(67%), num_fields(68%), ~
                                      num_fields(69%), num_fields(70%), ~
                                      num_fields(71%), num_fields(72%), ~
                                      num_fields(73%), num_fields(74%),  ~
                                      num_fields(75%), num_fields(76%),  ~
                                      num_fields(77%), num_fields(78%),  ~
                                      num_fields(79%), num_fields(80%),  ~
                                      num_fields(81%), num_fields(82%),  ~
                                      num_fields(83%), num_fields(84%),  ~
                                      fields$(85%), fields$(86%),  ~
                                      fields$(87%), num_fields(88%),  ~
                                      num_fields(89%), fields$(90%),  ~
                                      num_fields(91%), num_fields(92%)
                                      


L61440:           FMT CH(09), CH(08), CH(03), CH(03), CH(25), CH(32), CH(04), ~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4),       ~
                      CH(04), CH(04), PD(15,7), PD(15,4), PD(15,4), PD(14,4), ~
                      PD(14,4), CH(01), CH(09), CH(09), CH(06), BI(04),       ~
                      CH(03),                                                 ~
                      CH(06), CH(06), CH(06), CH(06), CH(06), CH(06), CH(06), ~
                      CH(06), CH(06), CH(06), CH(06), CH(06), CH(06), CH(06), ~
                      CH(06), CH(06), CH(06), CH(06), CH(06), CH(06), CH(06), ~
                      CH(06), CH(06), CH(06), CH(06), CH(06), CH(06), CH(06), ~
                      CH(06), CH(06),                                         ~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),       ~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),       ~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),       ~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),       ~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),       ~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),       ~
                      CH(01), CH(08), CH(04), PD(15,4), PD(15,4), CH(01),     ~
                      PD(15,4), BI(04)             




            convert num_fields(8%) to fields$(8%), pic(-#######0.0000)
            convert num_fields(9%) to fields$(9%), pic(-#######0.0000)
            convert num_fields(10%) to fields$(10%), pic(-#######0.0000)
            convert num_fields(11%) to fields$(11%), pic(-#######0.0000)

            convert num_fields(14%) to fields$(14%), pic(-####0.0000000)
            convert num_fields(15%) to fields$(15%), pic(-#######0.0000)
            convert num_fields(16%) to fields$(16%), pic(-#######0.0000)
            convert num_fields(17%) to fields$(17%), pic(-#######0.0000)
            convert num_fields(18%) to fields$(18%), pic(-#######0.0000)


            convert num_fields(23%) to fields$(23%), pic(##########)


            for i% = 55% to 84%
              convert num_fields(i%) to fields$(i%), pic(-#######0.0000)
            next i%

            convert num_fields(88%) to fields$(88%), pic(-#######0.0000)
            convert num_fields(89%) to fields$(89%), pic(-#######0.0000)

            convert num_fields(91%) to fields$(91%), pic(-#######0.0000)

            convert num_fields(92%) to fields$(92%), pic(##########)
            rec% = 1%            
        read_arilines_done
        return


        write_upload_arilines
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
                              fields$(59%), comma$, fields$(60%),~
                              comma$, fields$(61%), comma$,      ~
                              fields$(62%), comma$, fields$(63%),~
                              comma$, fields$(64%), comma$,      ~
                              fields$(65%), comma$, fields$(66%),~
                              comma$, fields$(67%), comma$,      ~
                              fields$(68%), comma$, fields$(69%),~
                              comma$, fields$(70%), comma$,      ~
                              fields$(71%), comma$, fields$(72%),~
                              comma$, fields$(73%), comma$,      ~
                              fields$(74%), comma$, fields$(75%),~
                              comma$, fields$(76%), comma$,      ~
                              fields$(77%), comma$, fields$(78%),~
                              comma$, fields$(79%), comma$,      ~
                              fields$(80%), comma$, fields$(81%),~
                              comma$, fields$(82%), comma$,      ~
                              fields$(83%), comma$, fields$(84%),~
                              comma$, fields$(85%), comma$,      ~
                              fields$(86%), comma$, fields$(87%),~
                              comma$, fields$(88%), comma$,      ~
                              fields$(89%), comma$, fields$(90%),~
                              comma$, fields$(91%), comma$,      ~
                              fields$(92%), comma$

L65500:               FMT CH(09), CH(01), CH(08), CH(01), CH(03), CH(01), ~
                          CH(03), CH(01), CH(25), CH(01), CH(32), CH(01), ~
                          CH(04), CH(01), ~
                          CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                          CH(14), CH(01),     ~
                          CH(04), CH(01), CH(04), CH(01), CH(14), CH(01), ~
                          CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                          CH(14), CH(01), CH(01), CH(01), CH(09), CH(01), ~
                          CH(09), CH(01), ~
                          CH(06), CH(01), CH(09), CH(01), CH(03), CH(01), ~
                          CH(06), CH(01), CH(06), CH(01), CH(06), CH(01), ~
                          CH(06), CH(01), CH(06), CH(01), CH(06), CH(01), ~
                          CH(06), CH(01), CH(06), CH(01), CH(06), CH(01), ~
                          CH(06), CH(01), CH(06), CH(01), CH(06), CH(01), ~
                          CH(06), CH(01), CH(06), CH(01), CH(06), CH(01), ~
                          CH(06), CH(01), CH(06), CH(01), CH(06), CH(01), ~
                          CH(06), CH(01), CH(06), CH(01), CH(06), CH(01), ~
                          CH(06), CH(01), CH(06), CH(01), CH(06), CH(01), ~
                          CH(06), CH(01), CH(06), CH(01), CH(06), CH(01), ~
                          CH(06), CH(01), CH(06), CH(01), CH(06), CH(01), ~
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
                          CH(01), CH(01), CH(08), CH(01), CH(04), CH(01), ~
                          CH(14), CH(01), CH(14), CH(01), CH(01), CH(01), ~
                          CH(14), CH(01), CH(09), CH(01)

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
