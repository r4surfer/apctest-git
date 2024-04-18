        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN23                             *~
            *  Creation Date     - 09/10/04                             *~
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
            *                      AWDMASTR - HNYMASTR                  *~
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
            * 09/10/04 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            *************************************************************

        dim                              /* (        ) - FILE          */~
            readkey$100,                 /* Readkey                    */~
            fields$(500%)256,            /* Generic Fields             */~
            num_fields(500%)             /* Generic Fields             */
            
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
            * #5  ! HNYMASTR ! Inventory Master File                    *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #5,  "HNYMASTR",                                      ~
                        varc,     indexed,  recsize =  900,              ~
                        keypos =    1, keylen =  25,                     ~
                        alt key  1, keypos =  102, keylen =   9, dup,    ~
                            key  2, keypos =   90, keylen =   4, dup     


            select #24, "AWDMASTR",                                      ~
                        varc,     indexed, recsize = 2020,               ~
                        keypos = 1,    keylen = 26

                        

REM            call "SHOSTAT" ("Initialization")

            filename$ = "HNYMASTR" : call "EWDOPEN" (#5, filename$, err%)
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
            mat num_fields      = zer

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
REM             call "SHOSTAT" (" CREATE MASTER " )
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDMASTR"
             ff% = 24%  
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
            read #5, key > readkey$, eod goto read_mast_done

                goto L67100
        read_mast_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #5, eod goto read_mast_done

L67100:         cnt% = cnt% + 1%
            goto L67155
            if mod(cnt%,50%) <> 0% then goto L67155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L67155:
                get #5, using L67110, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%), num_fields(6%),  ~
                                      fields$(7%), fields$(8%),  ~
                                      fields$(9%), fields$(10%), ~
                                      fields$(11%), fields$(12%),~
                                      fields$(13%), num_fields(14%),~
                                      fields$(15%), num_fields(16%),~
                                      fields$(17%), fields$(18%),~
                                      fields$(19%), fields$(20%),~
                                      fields$(21%), fields$(22%),~
                                      fields$(23%), fields$(24%),~
                                      fields$(25%), fields$(26%),~
                                      fields$(27%), fields$(28%),~
                                      fields$(29%), fields$(30%),~
                                      num_fields(31%), num_fields(32%),~
                                      num_fields(33%), fields$(34%),~
                                      fields$(35%), fields$(36%),~
                                      fields$(37%), fields$(38%),~
                                      fields$(39%), fields$(40%),~
                                      fields$(41%), fields$(42%),~
                                      fields$(43%), num_fields(44%),~
                                      num_fields(45%), fields$(46%),~
                                      fields$(47%), fields$(48%),~
                                      fields$(49%), fields$(50%),~
                                      fields$(51%), fields$(52%),~
                                      fields$(53%), fields$(54%),~
                                      fields$(55%), fields$(56%),~
                                      fields$(57%), fields$(58%), ~
                                      fields$(59%), fields$(60%), ~
                                      fields$(61%), fields$(62%), ~
                                      fields$(63%), fields$(64%), ~
                                      fields$(65%), fields$(66%), ~
                                      fields$(67%), fields$(68%), ~
                                      fields$(69%), fields$(70%), ~
                                      fields$(71%), fields$(72%), ~
                                      fields$(73%), fields$(74%), ~
                                      fields$(75%), num_fields(76%), ~
                                      num_fields(77%), num_fields(78%), ~
                                      num_fields(79%), fields$(80%), ~
                                      fields$(81%), fields$(82%), ~
                                      fields$(83%), fields$(84%), ~
                                      fields$(85%), fields$(86%), ~
                                      fields$(87%), fields$(88%), ~
                                      fields$(89%), fields$(90%), ~
                                      fields$(91%) 


REM      I do not know if field 5 will work b/c it is number
L67110:           FMT CH(25), CH(32), CH(16), CH(04), CH(04), PD(15,7),        ~
                      CH(04), CH(04), CH(04), CH(09), CH(01), CH(06), CH(01),  ~
                      PD(15,4), CH(01), BI(02), CH(01), CH(01), CH(01), CH(04),~
                      CH(02), CH(16), CH(08), CH(03), CH(04), CH(10), CH(03),  ~
                      CH(07), CH(10), CH(03), PD(15,4), PD(15,4), PD(15,4), CH(06), ~
                      CH(06), CH(03), CH(01), CH(01), CH(63), CH(01), CH(01),  ~
                      CH(03), CH(06), PD(15,4), PD(15,4), CH(01), CH(09),      ~
                      CH(09), CH(09), CH(09), CH(09), CH(09), CH(09), CH(09),  ~
                      CH(09), CH(09), CH(09), CH(09), CH(09), CH(09), CH(09),  ~
                      CH(09), CH(09), CH(09), CH(09), CH(20), CH(20), CH(20),  ~
                      CH(20), CH(20), CH(20), CH(20), CH(20), CH(20), CH(20),  ~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), CH(12), CH(12),  ~
                      CH(12), CH(12), CH(12), CH(12), CH(12), CH(12), CH(12),  ~
                      CH(12), CH(16), CH(27)

            
            
            convert num_fields(16%) to fields$(16%), pic(########0)
   
            convert num_fields(6%) to fields$(6%), pic(-####0.00#####)
            convert num_fields(14%) to fields$(14%), pic(-#######0.00##)   
            convert num_fields(31%) to fields$(31%), pic(-#######0.00##)   
            convert num_fields(32%) to fields$(32%), pic(-#######0.00##)   
            convert num_fields(33%) to fields$(33%), pic(-#######0.00##)   
            convert num_fields(44%) to fields$(44%), pic(-#######0.00##)   
            convert num_fields(45%) to fields$(45%), pic(-#######0.00##)   

            convert num_fields(76%) to fields$(76%), pic(-#######0.00##)   
            convert num_fields(77%) to fields$(77%), pic(-#######0.00##)   
            convert num_fields(78%) to fields$(78%), pic(-#######0.00##)   
            convert num_fields(79%) to fields$(79%), pic(-#######0.00##)   

            call "DATFMTC" (fields$(12%), date%, fields$(12%))            
            call "DATFMTC" (fields$(34%), date%, fields$(34%))
            call "DATFMTC" (fields$(35%), date%, fields$(35%))

            rec% = 1%            
        read_mast_done
        return


        write_upload_mast

            write #24, using L67400, fields$(1%), comma$,        ~
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
                              comma$, fields$(91%), comma$

L67400:           FMT CH(25), CH(1), CH(32), CH(1), CH(16), CH(1), CH(04),    ~
                      CH(1), CH(04), CH(1),                                   ~
                      CH(14), CH(1),                  /*  Conversion-Factor*/ ~
                      CH(04), CH(1), CH(04), CH(1), CH(04), CH(1), CH(09),    ~
                      CH(1), CH(01), CH(1),                                   ~
                      CH(08), CH(1),                  /* Cycle-Date        */ ~
                      CH(01), CH(1),                                          ~
                      CH(14), CH(1),                  /* Count Rate        */ ~
                      CH(01), CH(1),                                          ~
                      CH(09), CH(1),                  /* ATC Horizon       */ ~
                      CH(01), CH(1), CH(01), CH(1), CH(01), CH(1), CH(04),    ~
                      CH(1), CH(02), CH(1), CH(16), CH(1), CH(08), CH(1),     ~
                      CH(03), CH(1), CH(04), CH(1), CH(10), CH(1), CH(03),    ~
                      CH(1), CH(07), CH(1),CH(10), CH(1), CH(03), CH(1),      ~
                      CH(14), CH(1),                  /* Potency Factor    */ ~
                      CH(14), CH(1),                  /* Over Receipt %    */ ~
                      CH(14), CH(1),                  /* Over Receipt Units*/ ~
                      CH(08), CH(1),                  /* Create Date       */ ~
                      CH(08), CH(1),                  /* Modifed Date      */ ~
                      CH(03), CH(1), CH(01), CH(1), CH(01), CH(1), CH(63),    ~
                      CH(1), CH(01), CH(1), CH(01), CH(1), CH(03), CH(1),     ~
                      CH(06), CH(1),                                          ~
                      CH(14), CH(1),                  /* Safety Stock Level*/ ~
                      CH(14), CH(1),                  /* PanSize           */ ~
                      CH(01), CH(1), CH(09), CH(1), CH(09), CH(1), CH(09),    ~
                      CH(1), CH(09), CH(1), CH(09), CH(1), CH(09), CH(1),     ~
                      CH(09), CH(1), CH(09), CH(1), CH(09), CH(1), CH(09),    ~ 
                      CH(1), CH(09), CH(1), CH(09), CH(1), CH(09), CH(1),     ~
                      CH(09), CH(1), CH(09), CH(1), CH(09), CH(1), CH(09),    ~ 
                      CH(1), CH(09), CH(1), CH(09), CH(1), CH(20), CH(1),     ~
                      CH(20), CH(1), CH(20), CH(1), CH(20), CH(1), CH(20),    ~
                      CH(1), CH(20), CH(1), CH(20), CH(1), CH(20), CH(1),     ~
                      CH(20), CH(1), CH(20), CH(1),                           ~
                      CH(14), CH(1),                  /* Quantity to Buy   */ ~
                      CH(14), CH(1),                  /* Quantity to Buy   */ ~
                      CH(14), CH(1),                  /* % Under/Over Ship */ ~
                      CH(14), CH(1),                  /* Quantity to Buy   */ ~
                      CH(12), CH(1), CH(12), CH(1), CH(12), CH(1), CH(12),    ~
                      CH(1), CH(12), CH(1), CH(12), CH(1), CH(12), CH(1),     ~ 
                      CH(12), CH(1), CH(12), CH(1), CH(12), CH(1), CH(16),    ~
                      CH(1), CH(27), CH(1)

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
