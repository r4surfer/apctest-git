        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN15                             *~
            *  Creation Date     - 08/25/03                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie M. Gregory                  *~
            *  Last Modified By  -                                      *~
            *                                                           *~
            *  Description       - New Program to Build | delimited     *~
            *                      files to import into Oracle.         *~
            *                                                           *~
            *                                                           *~
            *                      This program only creates the booking*~
            *                      master and lines data to import!!    *~
            *                                                           *~
            *                      This program only creates files      *~
            *                      need for Shanes Reports              *~
            *                      AWDBNKMS - BNKMASTR                  *~
            *                      AWDBNKLS - BNKLINES                  *~
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
            * #4  ! GENCODES ! Gencodes Data                  (AWD001)  *~   
            * #6  ! BCKMASTR ! Backlog master file                      *~
            * #7  ! BCKLINES ! Back Log Line Item File                  *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

/*(AWD001)*/

            select #4, "GENCODES",                                       ~
                        varc,     indexed,  recsize =  128,              ~
                        keypos =    1, keylen =  24

            select #6,  "BNKMASTR",                                      ~
                        varc,     indexed,  recsize = 1030,              ~
                        keypos =    1, keylen =  16,                     ~
                        alt key  1, keypos =   18, keylen =  12, dup,    ~
                            key  2, keypos =   31, keylen =  25, dup,    ~
                            key  3, keypos =   17, keylen =  39, dup

            select #7,  "BNKLINES",                                      ~
                        varc,     indexed,  recsize =  336,              ~
                        keypos =    1, keylen =  19,                     ~
                        alt key  1, keypos =   46, keylen =  19, dup      
                        

            select #25, "AWDBNKMS",                                      ~
                        varc,     indexed, recsize = 1168,               ~
                        keypos = 1,    keylen = 17
                        
            select #26, "AWDBNKLS",                                      ~
                        varc,     indexed, recsize = 500,                ~
                        keypos = 1,    keylen = 21                                                                
                        
REM            call "SHOSTAT" ("Initialization")


            filename$ = "GENCODES" : call "EWDOPEN" (#4, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BNKMASTR" : call "EWDOPEN" (#6, filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "BNKLINES" : call "EWDOPEN" (#7, filename$, err%)
            if err% <> 0% then gosub open_error            
            
            mat f1% = zer

        REM *************************************************************~
            *                I N I T I A L I Z A T I O N                *~
            *-----------------------------------------------------------*~
            * Initializes information necessary for program.            *~
            *************************************************************
            call "EXTRACT" addr("ID", userid$)
            date$ = date



                                                          /* (AWD001)  */

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

            gosub create_mast
            gosub create_lines


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


        
        create_mast
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDBNKMS"
             ff% = 25%  
             gosub open_file
      
             gosub read_mast
                   goto L67010
        create_mast_nxt
             gosub read_mast_nxt
             if rec% <> 1% then goto mast_done
L67010:
REM             gosub build_file
               gosub write_upload_mast
               goto create_mast_nxt
        return
        mast_done
        return                
        
        read_mast
            init(" ") readkey$, fields$()
            mat num_fields = zer
            rec% = 0%
            read #6, key > readkey$, eod goto read_mast_done
                goto L67100
        read_mast_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #6, eod goto read_mast_done

L67100:         cnt% = cnt% + 1%
            goto L67155
            if mod(cnt%,50%) <> 0% then goto L67155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L67155:
                get #6, using L67110, fields$(1%), fields$(2%),  ~
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
                                      fields$(35%), fields$(36%),~
                                      fields$(37%), fields$(38%),~
                                      fields$(39%), fields$(40%),~
                                      fields$(41%), fields$(42%),~
                                      fields$(43%), fields$(44%),~
                                      fields$(45%), fields$(46%),~
                                      fields$(47%), fields$(48%),~
                                      fields$(49%), fields$(50%),~
                                      fields$(51%), fields$(52%),~
                                      fields$(53%), fields$(54%),~
                                      fields$(55%), fields$(56%),~
                                      fields$(57%), num_fields(58%), ~
                                      num_fields(59%), fields$(60%), ~
                                      fields$(61%), fields$(62%), ~
                                      fields$(63%), fields$(64%), ~
                                      fields$(65%), fields$(66%), ~
                                      fields$(67%), fields$(68%), ~
                                      fields$(69%) 


REM      I do not know if field 5 will work b/c it is number
L67110:           FMT CH(16), CH(01), CH(06), CH(06), CH(01),CH(09), CH(16),  ~
                      CH(16), CH(30), CH(30), CH(30), ~
                      CH(30), CH(30), CH(30), CH(30), CH(30), CH(30), CH(30), ~
                      CH(30), CH(30), CH(20), CH(20), CH(20), CH(50), CH(50), ~
                      CH(09), CH(09), CH(04), CH(04), CH(04), CH(01), CH(01), ~
                      CH(01), CH(04), CH(20), CH(20), CH(20), CH(20), CH(20), ~
                      CH(20), CH(20), CH(20), CH(20), CH(20), CH(04), CH(03), ~
                      CH(06), CH(06), CH(06), CH(06), CH(06), CH(03), CH(06), ~
                      CH(03), CH(09), CH(01), CH(01), PD(14,4), PD(14,4),     ~
                      CH(01), CH(02), CH(04), CH(02), CH(09), CH(04), CH(01), ~
                      CH(01), CH(01), CH(101)

            
            if fields$(31%) <> " " then num_fields(31%) = VAL(fields$(31%),4)                                
            if fields$(32%) <> " " then num_fields(32%) = VAL(fields$(32%),4)
            if fields$(33%) <> " " then num_fields(33%) = VAL(fields$(33%),4)
            if fields$(45%) <> " " then num_fields(45%) = VAL(fields$(45%),4)
            if fields$(61%) <> " " then num_fields(61%) = VAL(fields$(61%),2)
            if fields$(62%) <> " " then num_fields(62%) = VAL(fields$(62%),4)
            
            convert num_fields(31%) to fields$(31%), pic(########0)
            convert num_fields(32%) to fields$(32%), pic(########0)
            convert num_fields(33%) to fields$(33%), pic(########0)
            convert num_fields(45%) to fields$(45%), pic(########0)
            convert num_fields(61%) to fields$(61%), pic(########0)
            convert num_fields(62%) to fields$(62%), pic(########0)
   
            convert num_fields(58%) to fields$(58%), pic(-#######0.00##)
            convert num_fields(59%) to fields$(59%), pic(-#######0.00##)   

            call "DATFMTC" (fields$(3%), date%, fields$(3%))            
            call "DATFMTC" (fields$(47%), date%, fields$(47%))
            call "DATFMTC" (fields$(48%), date%, fields$(48%))
            call "DATFMTC" (fields$(49%), date%, fields$(49%))
            call "DATFMTC" (fields$(50%), date%, fields$(50%))
            call "DATFMTC" (fields$(51%), date%, fields$(51%))
            call "DATFMTC" (fields$(53%), date%, fields$(53%))                     

            rec% = 1%            
        read_mast_done
        return


        write_upload_mast
            write #25, using L67400, fields$(1%), comma$,        ~
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
                              comma$
 
L67400:           FMT CH(16), CH(01), CH(01), CH(01), CH(08), CH(01), ~
                      CH(06), CH(01), CH(01), CH(01), CH(09), CH(01), ~
                      CH(16), CH(01), CH(16), CH(01), CH(30), CH(01), ~
                      CH(30), CH(01), CH(30), CH(01), ~
                      CH(30), CH(01), CH(30), CH(01), CH(30), CH(01), ~
                      CH(30), CH(01), CH(30), CH(01), CH(30), CH(01), ~
                      CH(30), CH(01), CH(30), CH(01), CH(30), CH(01), ~
                      CH(20), CH(01), CH(20), CH(01), CH(20), CH(01), ~
                      CH(50), CH(01), CH(50), CH(01), CH(09), CH(01), ~
                      CH(09), CH(01), CH(04), CH(01), CH(04), CH(01), ~
                      CH(04), CH(01),~
                      CH(09), CH(01), CH(09), CH(01), CH(09), CH(01), ~
                      CH(04), CH(01), ~
                      CH(20), CH(01), CH(20), CH(01), CH(20), CH(01), ~
                      CH(20), CH(01), CH(20), CH(01), CH(20), CH(01), ~
                      CH(20), CH(01), CH(20), CH(01), CH(20), CH(01), ~
                      CH(20), CH(01), CH(09), CH(01), CH(03), CH(01), ~
                      CH(08), CH(01), CH(08), CH(01), CH(08), CH(01), ~
                      CH(08), CH(01), CH(08), CH(01), CH(03), CH(01), ~
                      CH(08), CH(01), CH(03), CH(01), CH(09), CH(01), ~
                      CH(01), CH(01), CH(01), CH(01), CH(14), CH(01), ~
                      CH(14), CH(01), CH(01), CH(01), CH(09), CH(01), ~
                      CH(09), CH(01), CH(02), CH(01), CH(09), CH(01), ~
                      CH(04), CH(01), CH(01), CH(01), CH(01), CH(01), ~
                      CH(01), CH(01), CH(101), CH(01)
        return

        
        
        create_lines
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDBNKLS"
             ff% = 26%  
             gosub open_file
      
             gosub read_lines
                   goto L68010
        create_lines_nxt
             gosub read_lines_nxt
             if rec% <> 1% then goto lines_done
L68010:
REM             gosub build_file
               gosub write_upload_lines
               goto create_lines_nxt
        return
        lines_done
        return

        read_lines
            init(" ") readkey$, fields$()
            mat num_fields = zer
            rec% = 0%
            read #7, key > readkey$, eod goto read_lines_done
                goto L68100
        read_lines_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #7, eod goto read_lines_done

L68100:         cnt% = cnt% + 1%
            goto L68155
            if mod(cnt%,50%) <> 0% then goto L68155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L68155:
                get #7, using L68110, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%), fields$(6%),  ~
                                      fields$(7%), fields$(8%),  ~
                                      fields$(9%), fields$(10%), ~
                                      fields$(11%), fields$(12%),~
                                      num_fields(13%), num_fields(14%),~
                                      num_fields(15%), num_fields(16%),~
                                      num_fields(17%), num_fields(18%),~
                                      num_fields(19%), fields$(20%), ~
                                      fields$(21%), num_fields(22%), ~
                                      num_fields(23%), num_fields(24%),~
                                      fields$(25%), fields$(26%),~
                                      fields$(27%), fields$(28%),~
                                      fields$(29%), fields$(30%),~
                                      fields$(31%), fields$(32%),~
                                      fields$(33%), fields$(34%),~
                                      fields$(35%), fields$(36%),~
                                      fields$(37%), fields$(38%),~
                                      fields$(39%), fields$(40%),~
                                      fields$(41%), fields$(42%),~
                                      fields$(43%), fields$(44%),~
                                      fields$(45%), fields$(46%)


REM      I do not know if field 5 will work b/c it is number
L68110:           FMT CH(16), CH(03), CH(06), CH(06), CH(05), CH(09), CH(16), ~
                      CH(03), CH(03), CH(25), CH(32), CH(04), ~
                      PD(14,4), PD(14,4), PD(14,4), PD(14,4), PD(14,4),       ~
                      PD(14,4), PD(14,4), CH(04), CH(04), PD(14,7), PD(14,4), ~
                      PD(14,4), CH(01), CH(09), CH(09), CH(06), CH(06),       ~
                      CH(06), CH(06), CH(08), CH(08), CH(01), CH(01), CH(04), ~
                      CH(01), CH(08), CH(08), CH(03), CH(08), CH(03), CH(01), ~
                      CH(01), CH(01), CH(21)
                      
                      
            if fields$(36%) <> " " then num_fields(36%) = VAL(fields$(36%),4)                                
            if fields$(44%) <> " " then num_fields(44%) = VAL(fields$(44%),4)
            
            convert num_fields(36%) to fields$(36%), pic(########0)
            convert num_fields(44%) to fields$(44%), pic(########0)            
            
            convert num_fields(13%) to fields$(13%), pic(-#######0.00##)
            convert num_fields(14%) to fields$(14%), pic(-#######0.00##)
            convert num_fields(15%) to fields$(15%), pic(-#######0.00##)
            convert num_fields(16%) to fields$(16%), pic(-#######0.00##)
            convert num_fields(17%) to fields$(17%), pic(-#######0.00##)
            convert num_fields(18%) to fields$(18%), pic(-#######0.00##)
            convert num_fields(19%) to fields$(19%), pic(-#######0.00##)
            convert num_fields(22%) to fields$(22%), pic(-####0.00#####)
            convert num_fields(23%) to fields$(23%), pic(-#######0.00##)
            convert num_fields(24%) to fields$(24%), pic(-#######0.00##)
            

            call "DATFMTC" (fields$(3%), date%, fields$(3%))                                    
            call "DATFMTC" (fields$(28%), date%, fields$(28%))
            call "DATFMTC" (fields$(29%), date%, fields$(29%))
            call "DATFMTC" (fields$(30%), date%, fields$(30%))            
            

            rec% = 1%            
        read_lines_done
        return


        write_upload_lines
            write #26, using L68400, fields$(1%), comma$,        ~
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
                              comma$, fields$(46%), comma$

L68400:               FMT CH(16), CH(01), CH(03), CH(01), CH(08), CH(01), ~
                          CH(06), CH(01), CH(05), CH(01), CH(09), CH(01), ~
                          CH(16), CH(01), CH(03), CH(01), CH(03), CH(01), ~
                          CH(25), CH(01), CH(32), CH(01), CH(04), CH(01), ~
                          CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                          CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                          CH(14), CH(01), CH(04), CH(01), CH(04), CH(01), ~
                          CH(14), CH(01), CH(14), CH(01), CH(14), CH(01), ~
                          CH(01), CH(01), CH(09), CH(01), CH(09), CH(01), ~
                          CH(08), CH(01), CH(08), CH(01), CH(08), CH(01), ~
                          CH(06), CH(01), CH(08), CH(01), CH(08), CH(01), ~
                          CH(01), CH(01), CH(01), CH(01), CH(09), CH(01), ~
                          CH(01), CH(01), CH(08), CH(01), CH(08), CH(01), ~
                          CH(03), CH(01), CH(08), CH(01), CH(03), CH(01), ~
                          CH(01), CH(01), CH(09), CH(01), CH(01), CH(01), ~
                          CH(21), CH(01)
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
