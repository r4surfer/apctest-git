        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN06                             *~
            *  Creation Date     - 08/25/03                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New Program to Build | delimited     *~
            *                      files to import into Oracle.         *~
            *                                                           *~
            *                      This program only creates files      *~
            *                      need for WW import                   *~
            *                      AWDCATEG - CATEGORY                  *~
            *                      AWDAMTBO - AMTBOMIF                  *~
            *                      AWDPLNOR - APCPLNOR                  *~
            *                      AWDCUSIN - CUSTOMER                  *~
            *                      AWDCUSTY - CUS TYPES (GENCODES)      *~
            *                      AWDNEWC  - PLAN NEWC (GENCODES)      *~
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
            * 08/25/03 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            * 04/20/04 ! (EWD001) - Mod to add Gencodes Table     ! CMG *~
            *          !              'PLAN NEWC'                 !     *~
            *************************************************************

        dim                              /* (CATEGORY) - FILE          */~
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
            * #1  ! CATEGORY ! Inventory Category Codes File            *~
            * #2  ! AMTBOMIF ! Field Definitions for Item Number        *~
            * #3  ! APCPLNOR ! (NEW) S.O. Header Histroy                *~
            * #4  ! CUSTOMER ! Customer Master File                     *~
            * #5  ! GENCODES ! Control System Codes File                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "CATEGORY",                                      ~
                        varc,     indexed,  recsize =  200,              ~
                        keypos = 1,    keylen =  4                       

            select #2,  "AMTBOMIF",                                      ~
                        varc,     indexed,  recsize = 120,               ~
                        keypos = 1,    keylen = 32                       

            select #3,  "APCPLNOR",                                      ~
                        varc,     indexed,  recsize =  170,              ~
                        keypos = 1,    keylen = 51,                      ~
                        alt key 1, keypos = 27, keylen = 25,             ~
                            key 2, keypos = 70, keylen =  8, dup,        ~
                            key 3, keypos = 78, keylen =  8, dup,        ~
                            key 4, keypos = 52, keylen =  8,             ~
                            key 5, keypos = 36, keylen = 16, dup

            select #4,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #5,  "GENCODES",                                      ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 128,                                   ~
                        keypos =    1,  keylen = 24


            select #20, "AWDCATEG",                                      ~
                        varc,     indexed, recsize = 500,                ~
                        keypos = 1,    keylen = 5

            select #21, "AWDAMTBO",                                      ~
                        varc,     indexed, recsize = 500,                ~
                        keypos = 1,    keylen = 35

            select #22, "AWDPLNOR",                                      ~
                        varc,     indexed, recsize = 500,                ~
                        keypos = 1,    keylen = 70

            select #23, "AWDCUSIN",                                      ~
                        varc,     indexed, recsize = 1312,               ~
                        keypos = 1,    keylen = 10

            select #24, "AWDCUSTY",                                      ~
                        varc,     indexed, recsize =  500,               ~
                        keypos = 1,    keylen = 26
                        

                                                             /*  (EWD001)  */
            select #28, "AWDNEWC ",                                      ~
                        varc,     indexed, recsize =  500,               ~
                        keypos = 1,    keylen = 26


REM            call "SHOSTAT" ("Initialization")

            filename$ = "CATEGORY" : call "EWDOPEN" (#1, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "AMTBOMIF" : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "APCPLNOR" : call "EWDOPEN" (#3, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "CUSTOMER" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "GENCODES" : call "EWDOPEN" (#5, filename$, err%)
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

            gosub create_category
            gosub create_val
REM            gosub create_planning
            gosub create_cus
            gosub create_cus_type

        return





        REM *************************************************************~
            * V A R I A B L E   F I E L D   I N I T I A L I Z A T I O N *~
            *-----------------------------------------------------------*~
            * Initializes all defined screen variables to blank         *~
            *************************************************************

        initialize_variables
            init(" ") readkey$, fields$(), filename$, cnt$, hdr$, msg$(),~
                      date$, library$, volume$, readdate$

        return

        REM *************************************************************~
            *************************************************************


        REM *************************************************************~
            *           S P E C I A L   S U B R O U T I N E S           *~
            *************************************************************

        create_category
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDCATEG"
             ff% = 20%  
             gosub open_file
      
             gosub read_category
                   goto L61010
        create_category_nxt
             gosub read_category_nxt
             if rec% <> 1% then goto category_done
L61010:
REM             gosub build_file
               gosub write_upload_category
               goto create_category_nxt
        return
        category_done
        return

        read_category
            init(" ") readkey$, fields$()
            mat num_fields = zer
            rec% = 0%
            read #1, key > readkey$, eod goto read_category_done
                goto L61100
        read_category_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #1, eod goto read_category_done

L61100:         cnt% = cnt% + 1%

            goto L61155
            if mod(cnt%,50%) <> 0% then goto L61155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L61155:
                get #1, using L61110, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      num_fields(5%), fields$(6%)


REM      I do not know if field 5 will work b/c it is number
L61110:           FMT CH(4), CH(30), CH(9), CH(9), PD(14,4), CH(140)

            convert num_fields(5%) to fields$(5%), pic(-#######0.000#)

            rec% = 1%            
        read_category_done
        return


        write_upload_category
            write #20, using L63400, fields$(1%), comma$,        ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$,       ~
                              fields$(5%), comma$, fields$(6%),  ~
                              comma$

L63400:               FMT CH(4), CH(1), CH(30), CH(1), CH(9),    ~
                          CH(1), CH(9), CH(1), CH(14), CH(1),    ~
                          CH(140), CH(1)
        return


        create_val
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDAMTBO"
             ff% = 21%  
             gosub open_file
      
             gosub read_val     
                   goto L61020
        create_val_nxt
             gosub read_val_nxt
             if rec% <> 1% then goto val_done
L61020:

               gosub write_upload_val
               goto create_val_nxt
        return
        val_done
        return

        read_val
            init(" ") readkey$, fields$()
            mat num_fields = zer
            rec% = 0%
            read #2, key > readkey$, eod goto read_val_done
                goto L61200
        read_val_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #2, eod goto read_val_done

L61200:         cnt% = cnt% + 1%
            goto L62155
            if mod(cnt%,50%) <> 0% then goto L62155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L62155:
                get #2, using L61130, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%), fields$(6%),  ~
                                      fields$(7%), fields$(8%),  ~
                                      num_fields(9%), fields$(10%)


REM      I do not know if field 5 will work b/c it is number
L61130:           FMT CH(15), CH(02), CH(15), CH(01), CH(20), CH(30), ~
                      CH(25), CH(03), PD(14,4), CH(01)


            num_fields(9%) = 0.00

            convert num_fields(9%) to fields$(9%), pic(-#######0.000#)

            rec% = 1%            
        read_val_done
        return


        write_upload_val
            write #21, using L64400, fields$(1%), comma$,        ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$,       ~
                              fields$(5%), comma$, fields$(6%),  ~
                              comma$, fields$(7%), comma$,       ~
                              fields$(8%), comma$, fields$(9%),  ~
                              comma$, fields$(10%), comma$  

L64400:               FMT CH(15), CH(1), CH(02), CH(1), CH(15),  ~
                          CH(1), CH(01), CH(1), CH(20), CH(1),   ~
                          CH(30), CH(1), CH(25), CH(1), CH(03),  ~ 
                          CH(1), CH(14), CH(1), CH(01), CH(1)  
        return

        create_planning
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDPLNOR"
             ff% = 22%  
             gosub open_file
      
             gosub read_plan    
                   goto L61030
        create_plan_nxt
             gosub read_plan_nxt
             if rec% <> 1% then goto plan_done
L61030:

               gosub write_upload_plan
               goto create_plan_nxt
        return
        plan_done
        return

        read_plan
            init(" ") fields$()
            readdate$ = "20030101"
            call "DATFMTC" (readdate$)
            call "DATUFMTC" (readdate$)
            readkey$ = all(hex(00))
            str(readkey$,1%,6%) = readdate$
            
            mat num_fields = zer
            rec% = 0%
            read #3, key > readkey$, eod goto read_plan_done
                goto L61300
        read_plan_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #3, eod goto read_plan_done

L61300:         cnt% = cnt% + 1%
            goto L63155
            if mod(cnt%,50%) <> 0% then goto L63155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L63155:
                get #3, using L61140, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%), fields$(6%),  ~
                                      fields$(7%), fields$(8%),  ~
                                      fields$(9%), fields$(10%), ~
                                      fields$(11%), fields$(12%),~
                                      fields$(13%), fields$(14%),~
                                      fields$(15%), fields$(16%),~
                                      num_fields(17%), num_fields(18%),~
                                      num_fields(19%), num_fields(20%),~
                                      num_fields(21%), fields$(22%),~
                                      fields$(23%), fields$(24%),~
                                      fields$(25%), num_fields(26%),~
                                      num_fields(27%), fields$(28%),~
                                      fields$(29%)

REM      I do not know if field 5 will work b/c it is number
L61140:           FMT CH(08), CH(02), CH(05), CH(09), CH(02), CH(09), ~
                      CH(16), CH(08), CH(02), CH(08), CH(08), CH(08), ~
                      CH(04), CH(02), CH(02), CH(05), PD(14,4),       ~
                      PD(14,4), PD(14,4), BI(2), BI(2), CH(08),       ~
                      CH(08), CH(03), CH(08), BI(04), BI(2), CH(10),  ~ 
                      CH(01)


            call "DATFMTC" (fields$(1%), date%, fields$(1%))
            call "DATFMTC" (fields$(10%), date%, fields$(10%))
            call "DATFMTC" (fields$(22%), date%, fields$(22%))
            call "DATFMTC" (fields$(23%), date%, fields$(23%))
            call "DATFMTC" (fields$(25%), date%, fields$(25%))

            convert num_fields(17%) to fields$(17%), pic(-#######0.000#)
            convert num_fields(18%) to fields$(18%), pic(-#######0.000#)
            convert num_fields(19%) to fields$(19%), pic(-#######0.000#)

            convert num_fields(20%) to fields$(20%), pic(########00000)
            convert num_fields(21%) to fields$(21%), pic(########00000)
            convert num_fields(26%) to fields$(26%), pic(########00000)
            convert num_fields(27%) to fields$(27%), pic(########00000)

            rec% = 1%            
        read_plan_done
        return


        write_upload_plan
            write #22, using L65400, str(fields$(1%),1%,4%), "-", ~
                              str(fields$(1%),5%,2%), "-",        ~
                              str(fields$(1%),7%,2%), comma$,     ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$,       ~
                              fields$(5%), comma$, fields$(6%),  ~
                              comma$, fields$(7%), comma$,       ~
                              fields$(8%), comma$, fields$(9%),  ~
                              comma$, str(fields$(10%),1%,4%),   ~
                              "-", str(fields$(10%),5%,2%), "-", ~
                              str(fields$(10%),7%,2%),  comma$,  ~
                              fields$(11%), comma$, fields$(12%),~
                              comma$, fields$(13%), comma$,      ~
                              fields$(14%), comma$, fields$(15%),~
                              comma$, fields$(16%), comma$,      ~
                              fields$(17%), comma$, fields$(18%),~
                              comma$, fields$(19%), comma$,      ~
                              fields$(20%), comma$, fields$(21%),~
                              comma$, str(fields$(22%),1%,4%),   ~
                              "-", str(fields$(22%),5%,2%), "-", ~
                              str(fields$(22%),7%,2%),  comma$,  ~ 
                              str(fields$(23%),1%,4%), "-",      ~
                              str(fields$(23%),5%,2%), "-",      ~
                              str(fields$(23%),7%,2%),  comma$,  ~
                              fields$(24%), comma$,              ~
                              str(fields$(25%),1%,4%), "-",      ~
                              str(fields$(25%),5%,2%), "-",      ~
                              str(fields$(25%),7%,2%), comma$,   ~
                              fields$(26%), comma$, fields$(27%),~
                              comma$, fields$(28%), comma$,      ~
                              fields$(29%), comma$

L65400:               FMT CH(04), CH(01), CH(02), CH(01), CH(02),     ~
                      CH(01), CH(02), CH(01), CH(05), CH(01),         ~
                      CH(09), CH(01), CH(02), CH(01), CH(06), CH(01), ~
                      CH(16), CH(01), CH(08), CH(01), CH(02), CH(01), ~
                      CH(04), CH(01), CH(02), CH(01), CH(02), CH(01), ~
                      CH(08), CH(01), CH(08), CH(01), CH(04), CH(01), ~
                      CH(02), CH(01), CH(02), CH(01), CH(05), CH(01), ~
                      CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                      CH(14), CH(01), CH(14), CH(01), CH(04), CH(01), ~
                      CH(02), CH(01), CH(02), CH(01), CH(04), CH(01), ~
                      CH(02), CH(01), CH(02), CH(01), CH(03), CH(01), ~
                      CH(04), CH(01), CH(02), CH(01), CH(02), CH(01), ~
                      CH(14), CH(01), CH(14), CH(01), CH(10), CH(01), ~
                      CH(01), CH(01)
        return


        create_cus
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDCUSIN"
             ff% = 23%  
             gosub open_file
      
             gosub read_cus     
                   goto L61040
        create_cus_nxt
             gosub read_cus_nxt
             if rec% <> 1% then goto cus_done
L61040:

               gosub write_upload_cus
               goto create_cus_nxt
        return
        cus_done
        return

        read_cus
            init(" ") readkey$, fields$()
            mat num_fields = zer
            rec% = 0%
            read #4, key > readkey$, eod goto read_cus_done
                goto L61400
        read_cus_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #4, eod goto read_cus_done

L61400:         cnt% = cnt% + 1%
            goto L64155
            if mod(cnt%,50%) <> 0% then goto L64155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L64155:
                get #4, using L61150, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%), fields$(6%),  ~
                                      fields$(7%), fields$(8%),  ~
                                      fields$(9%), fields$(10%), ~
                                      fields$(11%), fields$(12%),~
                                      fields$(13%), fields$(14%),~
                                      fields$(15%), fields$(16%),~
                                      fields$(17%), fields$(18%),~
                                      fields$(19%), fields$(20%),~
                                      fields$(21%), fields$(22%),~
                                      fields$(23%), fields$(24%),~
                                      fields$(25%), fields$(26%),~
                                      fields$(27%), fields$(28%),~
                                      fields$(29%), fields$(30%),~
                                      fields$(31%), fields$(32%),~
                                      fields$(33%), fields$(34%),~
                                      fields$(35%), num_fields(36%),~
                                      fields$(37%), num_fields(38%),~
                                      fields$(39%), fields$(40%),~
                                      fields$(41%), fields$(42%),~
                                      fields$(43%), fields$(44%),~
                                      fields$(45%), fields$(46%),~
                                      fields$(47%), fields$(48%),~
                                      fields$(49%), fields$(50%),~
                                      fields$(50%), num_fields(52%),~
                                      num_fields(53%), num_fields(54%),~
                                      fields$(55%), fields$(56%),~
                                      fields$(57%), fields$(58%),~
                                      fields$(59%), fields$(60%),~
                                      fields$(61%), num_fields(62%),~
                                      fields$(63%), fields$(64%),~
                                      fields$(65%), fields$(66%),~
                                      fields$(67%), fields$(68%),~
                                      fields$(69%), fields$(70%),~
                                      fields$(71%), fields$(72%),~
                                      fields$(73%), fields$(74%),~
                                      fields$(75%), fields$(76%),~
                                      fields$(77%), fields$(78%),~
                                      fields$(79%), fields$(80%),~
                                      fields$(81%), fields$(82%),~
                                      fields$(83%), fields$(84%),~
                                      num_fields(85%), fields$(86%),~
                                      fields$(87%), fields$(88%),~
                                      fields$(89%), fields$(90%),~
                                      fields$(91%), fields$(92%),~
                                      fields$(93%), fields$(94%),~
                                      fields$(95%)



REM      I do not know if field 5 will work b/c it is number
L61150:           FMT CH(09), CH(30), CH(30), CH(30), CH(30), CH(30), CH(30), ~
                      CH(18), CH(02), CH(01), CH(09), CH(06), CH(01), CH(01), ~
                      CH(10), CH(01), CH(11), CH(03), CH(30), CH(30), CH(30), ~
                      CH(30), CH(30), CH(18), CH(02), CH(01), CH(09), CH(20), ~
                      CH(10), CH(09), CH(09), CH(09), CH(09), CH(09), CH(09), ~
                      PD(15,4), CH(01), PD(15,4), CH(06), CH(03), CH(20),     ~
                      CH(20), CH(20), CH(09), CH(01), CH(01), CH(50), CH(50), ~ 
                      CH(04), CH(04), CH(04), BI(01), BI(01), BI(01), CH(04), ~
                      CH(01), CH(01), CH(30), CH(06), CH(09), CH(09), BI(04), ~ 
                      CH(01), CH(01), CH(25), CH(20), CH(20), CH(20), CH(20), ~
                      CH(20), CH(20), CH(20), CH(20), CH(20), CH(20), CH(01), ~
                      CH(01), CH(01), CH(02), CH(10), CH(09), CH(01), CH(04), ~
                      CH(09), BI(02), CH(06), CH(03), CH(04), CH(03), CH(06), ~
                      CH(09), CH(01), CH(01), CH(01), CH(107)

            convert fields$(15%) to num_fields(15%), data goto L61180

L61180:     convert num_fields(15%) to fields$(15%), pic(##########)

            convert num_fields(36%) to fields$(36%), pic(-#######0.000#)
            convert num_fields(38%) to fields$(38%), pic(-#######0.000#)


            convert num_fields(52%) to fields$(52%), pic(########00000)
            convert num_fields(53%) to fields$(53%), pic(########00000)
            convert num_fields(54%) to fields$(54%), pic(########00000)
            convert num_fields(62%) to fields$(62%), pic(########00000)
            convert num_fields(85%) to fields$(85%), pic(########00000)

            call "DATFMTC" (fields$(12%), date%, fields$(12%))
            call "DATFMTC" (fields$(39%), date%, fields$(39%))
            call "DATFMTC" (fields$(86%), date%, fields$(86%))

            rec% = 1%            
        read_cus_done
        return


        write_upload_cus
            write #23, using L65500, fields$(1%), comma$,        ~
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
                              fields$(59%), comma$, " ", comma$, fields$(60%),~
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
                              fields$(89%), comma$, fields$(94%),~
                              comma$



L65500:               FMT CH(09), CH(01), CH(30), CH(01), CH(30), CH(01), CH(30), ~
                      CH(01), CH(30), CH(01), CH(30), CH(01), CH(30), CH(01), ~
                      CH(18), CH(01), CH(02), CH(01), CH(01), CH(01), CH(09), ~
                      CH(01), CH(08), CH(01), CH(01), CH(01), CH(01), CH(01), ~
                      CH(10), CH(01), CH(01), CH(01), CH(11), CH(01), CH(03), ~
                      CH(01), CH(30), CH(01), CH(30), CH(01), CH(30), CH(01), ~
                      CH(30), CH(01), CH(30), CH(01), CH(18), CH(01), CH(02), ~
                      CH(01), CH(01), CH(01), CH(09), CH(01), CH(20), CH(01), ~
                      CH(10), CH(01), CH(09), CH(01), CH(09), CH(01), CH(09), ~
                      CH(01), CH(09), CH(01), CH(09), CH(01), CH(09), CH(01), ~
                      CH(14), CH(01), CH(01), CH(01), CH(14), CH(01), CH(08), ~
                      CH(01), CH(03), CH(01), CH(20), CH(01), CH(20), CH(01), ~
                      CH(20), CH(01), CH(09), CH(01), CH(01), CH(01), CH(01), ~
                      CH(01), CH(50), CH(01), CH(50), CH(01), CH(04), CH(01), ~
                      CH(04), CH(01), CH(04), CH(01), CH(14), CH(01), CH(14), ~
                      CH(01), CH(14), CH(01), CH(04), CH(01), CH(01), CH(01), ~
                      CH(01), CH(01), CH(30), CH(01), CH(06), CH(01), CH(01), ~
                      CH(01), CH(09), CH(01), CH(09), CH(01), CH(14), CH(01), ~ 
                      CH(01), CH(01), CH(01), CH(01), CH(25), CH(01), CH(20), ~
                      CH(01), CH(20), CH(01), CH(20), CH(01), CH(20), CH(01), ~
                      CH(20), CH(01), CH(20), CH(01), CH(20), CH(01), CH(20), ~
                      CH(01), CH(20), CH(01), CH(20), CH(01), CH(01), CH(01), ~
                      CH(01), CH(01), CH(01), CH(01), CH(02), CH(01), CH(10), ~
                      CH(01), CH(09), CH(01), CH(01), CH(01), CH(04), CH(01), ~
                      CH(09), CH(01), CH(14), CH(01), CH(08), CH(01), CH(03), ~
                      CH(01), CH(04), CH(01), CH(03), CH(01), CH(03), CH(01), ~
                      CH(125), CH(01)
        return


        create_cus_type
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDCUSTY"
             ff% = 24%  
             gosub open_file
      
             gosub read_cus_type
                   goto L65800
        create_cus_type_nxt
             gosub read_cus_type_nxt
             if rec% <> 1% then goto cus_type_done
L65800:

               gosub write_upload_cus_type
               goto create_cus_type_nxt
        return
        cus_type_done
                                                       /*  (EWD001) - BEG */
               init(" ") readkey$, file$
               rec%, cnt% = 0%
               file$   = "AWDNEWC "
               ff% = 28%  
               gosub open_file

               gosub read_plan_newc

                 goto L65810
        create_plan_newc_nxt
                 gosub read_plan_newc_nxt
                 if rec% <> 1% then goto plan_newc_done
L65810:
                 gosub write_upload_plan_newc
                 goto create_plan_newc_nxt
        plan_newc_done
        return
                                                       /*  (EWD001) - END */

        read_cus_type
            init(" ") readkey$, fields$()
            readkey$ = "CUS TYPES"
            mat num_fields = zer
            rec% = 0%
            read #5, key > readkey$, using L65890, readkey$,       ~
                            eod goto read_cus_type_done
L65890:          FMT CH(09)
                if str(readkey$,1%,9%) <> "CUS TYPES" then         ~
                                            goto read_cus_type_done
                goto L65850
        read_cus_type_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #5, using L65890, readkey$, eod goto read_cus_type_done


                if str(readkey$,1%,9%) <> "CUS TYPES" then         ~
                                            goto read_cus_type_done

L65850:         cnt% = cnt% + 1%
            goto L65900
            if mod(cnt%,50%) <> 0% then goto L65900
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L65900:
                get #5, using L65950, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%)



L65950:           FMT CH(09), CH(15), CH(30), CH(02), CH(72)


            rec% = 1%            
        read_cus_type_done
        return

        write_upload_cus_type
            write #24, using L66000, fields$(1%), comma$,        ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$,       ~
                              fields$(5%), comma$

L66000:               FMT CH(09), CH(01), CH(15), CH(01), CH(30), ~
                          CH(01), CH(02), CH(01), CH(72), CH(01)
        return

                                                       /*  (EWD001) - BEG */
        read_plan_newc
            init(" ") readkey$, fields$()
            readkey$ = "PLAN NEWC"
            mat num_fields = zer
            rec% = 0%
            read #5, key > readkey$, using L65890, readkey$,       ~
                            eod goto read_plan_newc_done

                if str(readkey$,1%,9%) <> "PLAN NEWC" then         ~
                                            goto read_plan_newc_done
                goto L65855
        read_plan_newc_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #5, using L65890, readkey$, eod goto read_plan_newc_done


                if str(readkey$,1%,9%) <> "PLAN NEWC" then         ~
                                            goto read_plan_newc_done

L65855:         cnt% = cnt% + 1%
            goto L65905
            if mod(cnt%,50%) <> 0% then goto L65905
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L65905:
                get #5, using L65950, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%)

            rec% = 1%            
        read_plan_newc_done
        return


        write_upload_plan_newc
            write #28, using L66000, fields$(1%), comma$,        ~
                              fields$(2%), comma$, fields$(3%),  ~
                              comma$, fields$(4%), comma$,       ~
                              fields$(5%), comma$

        return
                                                       /*  (EWD001) - END */

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
