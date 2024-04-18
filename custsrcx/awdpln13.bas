        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN13                             *~
            *  Creation Date     - 08/25/03                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New Program to Build | delimited     *~
            *                      files to import into Oracle.         *~
            *                                                           *~
            *                      This program only creates customer   *~
            *                      information or CUSTOMER file to      *~
            *                      import!!!!                           *~
            *                                                           *~
            *                      This program only creates files      *~
            *                      need for WW import                   *~
            *                      AWDCUSIN - CUSTOMER                  *~
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
            *************************************************************

        dim                              /* (CATEGORY) - FILE          */~
            readkey$100,                 /* Readkey                    */~
            fields$(500%)256,            /* Generic Fields             */~
            num_fields%(500%)            /* Generic Fields             */ 
            
            
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
            * #4  ! CUSTOMER ! Customer Master File                     *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #4,  "CUSTOMER",                                      ~
                        varc,     indexed,  recsize = 1200,              ~
                        keypos =    1, keylen =   9,                     ~
                        alt key  1, keypos =   10, keylen =  30, dup,    ~
                            key  2, keypos =  424, keylen =   9, dup,    ~
                            key  3, keypos =  771, keylen =   9, dup,    ~
                            key  4, keypos =  780, keylen =   9, dup,    ~
                            key  5, keypos = 1049, keylen =   9, dup

            select #23, "AWDCUSIN",                                      ~
                        varc,     indexed, recsize = 1312,               ~
                        keypos = 1,    keylen = 10

REM            call "SHOSTAT" ("Initialization")

            filename$ = "CUSTOMER" : call "EWDOPEN" (#4, filename$, err%)
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
            mat num_fields%     = zer

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

            gosub create_cus

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
            mat num_fields% = zer
            rec% = 0%
            read #4, key > readkey$, eod goto read_cus_done
                goto L61400
        read_cus_nxt
            init(" ") fields$()
            mat num_fields% = zer      
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
                                      fields$(35%), num_fields%(36%),~
                                      fields$(37%), num_fields%(38%),~
                                      fields$(39%), fields$(40%),~
                                      fields$(41%), fields$(42%),~
                                      fields$(43%), fields$(44%),~
                                      fields$(45%), fields$(46%),~
                                      fields$(47%), fields$(48%),~
                                      fields$(49%), fields$(50%),~
                                      fields$(50%), num_fields%(52%),~
                                      num_fields%(53%), num_fields%(54%),~
                                      fields$(55%), fields$(56%),~
                                      fields$(57%), fields$(58%),~
                                      fields$(59%), fields$(60%),~
                                      fields$(61%), num_fields%(62%),~
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
                                      num_fields%(85%), fields$(86%),~
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

            convert fields$(15%) to num_fields%(15%), data goto L61180

L61180:     convert num_fields%(15%) to fields$(15%), pic(##########)

            convert num_fields%(36%) to fields$(36%), pic(-#######0.000#)
            convert num_fields%(38%) to fields$(38%), pic(-#######0.000#)


            convert num_fields%(52%) to fields$(52%), pic(########00000)
            convert num_fields%(53%) to fields$(53%), pic(########00000)
            convert num_fields%(54%) to fields$(54%), pic(########00000)
            convert num_fields%(62%) to fields$(62%), pic(########00000)
            convert num_fields%(85%) to fields$(85%), pic(########00000)

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
