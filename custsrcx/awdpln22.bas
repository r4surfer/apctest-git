        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDPLN22                             *~
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
            *                      AWDQUAN  - HNYQUAN                   *~
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
            * #4  ! HNYQUAN  ! Inventory Quantities File                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #4,  "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize = 650,                                   ~
                        keypos=17, keylen = 44,                          ~
                        alternate key 1, keypos =  1, keylen = 44




            select #23, "AWDQUAN",                                       ~
                        varc,     indexed, recsize = 1024,               ~
                        keypos = 1,    keylen = 47


REM            call "SHOSTAT" ("Initialization")

            filename$ = "HNYQUAN"  : call "EWDOPEN" (#4, filename$, err%)
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
            gosub create_quan

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

        create_quan
REM             call "SHOSTAT" (" CREATE QUAN " )
             init(" ") readkey$, file$
             rec%, cnt% = 0%
             file$   = "AWDQUAN"
             ff% = 23%  
             gosub open_file
      
             gosub read_quan
                   goto L61040
        create_quan_nxt
             gosub read_quan_nxt
             if rec% <> 1% then goto quan_done
L61040:

               gosub write_upload_quan
               goto create_quan_nxt
        return
        quan_done
        return

        read_quan
            init(" ") readkey$, fields$()
            mat num_fields = zer
            rec% = 0%
            read #4, hold, key > readkey$, eod goto read_quan_done
                goto L61400
        read_quan_nxt
            init(" ") fields$()
            mat num_fields = zer      
            rec% = 0%
            read #4, hold, eod goto read_quan_done

L61400:         cnt% = cnt% + 1%
            goto L64155
            if mod(cnt%,50%) <> 0% then goto L64155
               convert cnt% to str(cnt$,19%,8%), pic(########)
               print at(02,02);hex(84);cnt$;

L64155:
                get #4, using L61150, fields$(1%), fields$(2%),  ~
                                      fields$(3%), fields$(4%),  ~
                                      fields$(5%), num_fields(6%),  ~
                                      num_fields(7%), num_fields(8%),  ~
                                      num_fields(9%), num_fields(10%), ~
                                      num_fields(11%), num_fields(12%),~
                                      num_fields(13%), num_fields(14%),~
                                      num_fields(15%), num_fields(16%),~
                                      num_fields(17%), num_fields(18%),~
                                      num_fields(19%), num_fields(20%),~
                                      num_fields(21%), num_fields(22%),~
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
                                      fields$(45%), fields$(46%),~
                                      num_fields(47%), fields$(48%),~
                                      fields$(49%), fields$(50%),~
                                      fields$(50%), fields$(52%),~
                                      fields$(53%), fields$(54%),~
                                      fields$(55%), fields$(56%),~
                                      fields$(57%), fields$(58%),~
                                      fields$(59%), fields$(60%),~
                                      fields$(61%), fields$(62%),~
                                      fields$(63%)



REM      I do not know if field 5 will work b/c it is number
L61150:           FMT CH(16), CH(25), CH(03), CH(16), CH(08), PD(15,4),          ~ 
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),~
                      PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4), PD(15,4),~
                      CH(10), CH(10), CH(09), CH(09), CH(09), CH(09), CH(09), ~
                      CH(09), CH(09), CH(09), CH(09), CH(09), CH(09), CH(09), ~
                      CH(09), CH(09), CH(09), CH(09), CH(09), CH(09), CH(01), ~
                      CH(06), PD(15,4), CH(04), CH(20), CH(20), CH(20), CH(20),~
                      CH(20), CH(20), CH(20), CH(20), CH(20),   ~
                      CH(20), CH(03), CH(06), CH(03), CH(06), CH(11)

                

REM            convert fields$(3%) to num_fields(3%), data goto bad_store

REM bad_store


REM            convert num_fields(3%) to fields$(3%), pic(000)
            convert num_fields(6%) to fields$(6%), pic(-#######0.000#)
            convert num_fields(7%) to fields$(7%), pic(-#######0.000#)
            convert num_fields(8%) to fields$(8%), pic(-#######0.000#)
            convert num_fields(9%) to fields$(9%), pic(-#######0.000#)
            convert num_fields(10%) to fields$(10%), pic(-#######0.000#)
            convert num_fields(11%) to fields$(11%), pic(-#######0.000#)
            convert num_fields(12%) to fields$(12%), pic(-#######0.000#)
            convert num_fields(13%) to fields$(13%), pic(-#######0.000#)
            convert num_fields(14%) to fields$(14%), pic(-#######0.000#)
            convert num_fields(15%) to fields$(15%), pic(-#######0.000#)
            convert num_fields(16%) to fields$(16%), pic(-#######0.000#)
            convert num_fields(17%) to fields$(17%), pic(-#######0.000#)
            convert num_fields(18%) to fields$(18%), pic(-#######0.000#)
            convert num_fields(19%) to fields$(19%), pic(-#######0.000#)
            convert num_fields(20%) to fields$(20%), pic(-#######0.000#)
            convert num_fields(21%) to fields$(21%), pic(-#######0.000#)
            convert num_fields(22%) to fields$(22%), pic(-#######0.000#)
            convert num_fields(23%) to fields$(23%), pic(-#######0.000#)
            convert num_fields(24%) to fields$(24%), pic(-#######0.000#)



            call "DATFMTC" (fields$(46%), date%, fields$(46%))
            convert num_fields(47%) to fields$(47%), pic(-#######0.000#)

            call "DATFMTC" (fields$(60%), date%, fields$(60%))

            rec% = 1%            
        read_quan_done
        return


        write_upload_quan
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
                              fields$(59%), comma$, " ", comma$, ~
                              fields$(60%),~
                              comma$, fields$(61%), comma$,      ~
                              fields$(62%), comma$, fields$(63%)



L65500:               FMT CH(16), CH(1), CH(25), CH(1), CH(03), CH(1), CH(16), ~
                      CH(1), CH(08), CH(1),                                    ~
                      CH(14), CH(1),                     /*  On Hand */        ~
                      CH(14), CH(1),                     /* BCKORDRD */        ~
                      CH(14), CH(1),                     /* On-Order */        ~ 
                      CH(14), CH(1),                     /* Commited */        ~
                      CH(14), CH(1),                     /* In-Procs */        ~
                      CH(14), CH(1),                     /* Qty-Pend */        ~
                      CH(14), CH(1),                     /* TotalCost*/        ~
                      CH(14), CH(1),                     /* Cost(01) */        ~
                      CH(14), CH(1),                     /* Cost(02) */        ~
                      CH(14), CH(1),                     /* Cost(03) */        ~
                      CH(14), CH(1),                     /* Cost(04) */        ~
                      CH(14), CH(1),                     /* Cost(05) */        ~
                      CH(14), CH(1),                     /* Cost(06) */        ~
                      CH(14), CH(1),                     /* Cost(07) */        ~
                      CH(14), CH(1),                     /* Cost(08) */        ~
                      CH(14), CH(1),                     /* Cost(09) */        ~
                      CH(14), CH(1),                     /* Cost(10) */        ~
                      CH(14), CH(1),                     /* Cost(11) */        ~
                      CH(14), CH(1),                     /* Cost(12) */        ~
                      CH(10), CH(1), CH(10), CH(1), CH(09), CH(1), CH(09), ~
                      CH(1), CH(09), CH(1), CH(09), CH(1), CH(09), CH(1), ~
                      CH(09), CH(1), CH(09), CH(1), CH(09), CH(1), CH(09), ~
                      CH(1), CH(09), CH(1), CH(09), CH(1), CH(09), CH(1), ~
                      CH(09), CH(1), CH(09), CH(1), CH(09), CH(1), CH(09), ~
                      CH(1), CH(09), CH(1), CH(09), CH(1), CH(01), CH(1), ~
                      CH(08), CH(1),                     /* Dte Expired */     ~
                      CH(14), CH(1),                     /* Lot-Potency */     ~
                      CH(04), CH(1), CH(20), CH(1), CH(20), CH(1), CH(20),    ~
                      CH(1), CH(20), CH(1), CH(20), CH(1), CH(20), CH(1),     ~
                      CH(20), CH(1), CH(20), CH(1), CH(20), CH(1), CH(20),    ~
                      CH(1), CH(03), CH(1),                                   ~
                      CH(08), CH(1),                      /* Dte Changed*/    ~
                      CH(03), CH(1),                                          ~
                      CH(08), CH(1),                      /* Lst Modify DTE*/ ~
                      CH(11), CH(1)
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
