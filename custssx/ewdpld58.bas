        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - EWDPLD58                             *~
            *  Creation Date     - 08/15/2007                           *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie Gregory                     *~
            *                                                           *~
            *  Description       - Generate Impact Order File           *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            * EWDPLD58 - Generates the Order File to transmit to        *~
            *            Thermal and Dallas                             *~
            *                                                           *~
            *          - ERROR% = 0% - File OK                          *~
            *                     1% - Could Not Create File            *~
            *                     2% - Could Not Write Data             *~
            *                                                           *~
            * Subroutine - Called by EWDPLN58                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *08/15/2007! Original - New Program                   ! CMG *~
            *************************************************************

        sub "EWDPLD58" ( sp_cust$,                 /* Customer         */~
                         sp_so$,                   /* Sales Order      */~
                         sp_ln$,                   /* SO Line          */~
                         sp_ln_item$,              /* SO Line Item     */~
                         sp_due$,                  /* Due Date         */~
                         sp_part$,                 /* Part number      */~
                         sp_qty$,                  /* Qty              */~
                         subpart$,                 /* Subpart          */~
                         infopart$,                /* Infopart         */~
                         sc_type$,                 /* Screen Type      */~
                         #1,                       /* (GENCODES)       */~   
                         #22,                      /* AWDTHERMA        */~
                         #23,                      /* AWDDALLAS        */~
                         been_here%,               /* Been Here Before */~
                         error%)                   /* Return Code      */



        dim                                                              ~
            sp_cust$9,                           /* Customer Code      */~
            sp_so$8,                             /* Sales Order        */~
            sp_ln$2,                             /* Line Number        */~
            sp_ln_item$4,                        /* Line Item          */~
            sp_due$6,                            /* Due Date           */~
            sp_due_fmt$10,                       /* Due Date Formatted */~
            sp_part$25,                          /* Part Number        */~
            sp_qty$4,                            /* Qty Ordered        */~      
            subpart$20,                          /* Subpart Number     */~
            infopart$20,                         /* Info Part Number   */~
            sc_type$1,                           /* Screen Type        */~
            size$10,                             /* Size Field to dec  */~
            write_rec$(4)256                     /* Record to Write    */

        dim readkey$50,                          /* Readkey            */~
            desc$30,                             /* Desc               */~
            table$9,                             /* Table              */~
            field$10                             /* Part Number Field  */~

        dim library$8,                           /* Library            */~
            volume$6,                            /* Volume             */~
            file$9                               /* File               */~ 



            if been_here% <> 99% then goto already_open
               been_here% = 0%

               init(" ") library$, volume$, file$
               ff% = 22%
               if sc_type$ = "7" then ff% = 23%
               library$        = "FTPIMPAC"
               volume$         = "CARLO2"

               if sc_type$ = "6" then file$   = "AWDTHERMA"
               if sc_type$ = "7" then file$   = "AWDDALLAS"

                open nodisplay #ff%, output, space = 100%,            ~
                  dpack   = 100%, ipack = 100%, file = file$,         ~
                  library = library$, volume = volume$, blocks = 5%

               init(" ") write_rec$()
               write_rec$() = "CUSTOMER|SO|LINE|DUEDATE|QTY|MODEL|COLOR|"
               write_rec$() = write_rec$() & "GLASS|LITING|HINGE|SCREEN|"
               write_rec$() = write_rec$() & "LOCKS|WIDTH|HEIGHT|GRDTYPE|"
               write_rec$() = write_rec$() & "GRDSIZE|GRDCOLOR|HARDWARE|"
               write_rec$() = write_rec$() & "FOAM|ACCESSOR|INTCOLOR|EXTCOLOR|"
               write_rec$() = write_rec$() & "FIN|"
               

               put #ff%, using WRITE_FMT, write_rec$()


               write #ff%

already_open


               init(" ")write_rec$()
               sp_due_fmt$ = sp_due$
               call "DATEFMT" (sp_due_fmt$)

               start% = 1%
               call "SHOSTAT" (" SP_CUST -- > " & sp_cust$) : stop
               s% = 0%


               p% = len( str(sp_cust$,1,pos(sp_cust$ = HEX(20) ) ) )
               str(write_rec$(),start%,p%)   = sp_cust$ & "|"
               start% = start% + p%

               p% = 9%
               str(write_rec$(),start%,p%)  = sp_so$ & "|"
               start% = start% + p%

               p% = 3%
               str(write_rec$(),start%,p%)  = sp_ln$ & "|"
               start% = start% + p%

               p% = 10%
               str(write_rec$(),start%,p%) = sp_due_fmt$ & "|"
               start% = start% + p%

               p% = 5%
               str(write_rec$(),start%,p%) = sp_qty$ & "|"
               start% = start% + p%
             
               init(" ")  table$, field$
               table$ = "MODEL"
               field$ = str(sp_part$,1,3)
               gosub lookup_desc

               call "SPCSMASH" (desc$)
               p% = len( str(desc$,1,pos(desc$ = HEX(20) ) ) )
               str(write_rec$(),start%, p%) = desc$ & "|"
               start% = start% + p%


               init(" ")  table$, field$
               table$ = "COLOR"
               field$ = str(sp_part$,4,1)
               gosub lookup_desc

               call "SPCSMASH" (desc$)
               p% = len( str(desc$,1,pos(desc$ = HEX(20) ) ) )
               str(write_rec$(),start%, p%) = desc$ & "|"
               start% = start% + p%

               init(" ")  table$, field$
               table$ = "GLASS"
               field$ = str(sp_part$,5,2)
               gosub lookup_desc

               call "SPCSMASH" (desc$)
               p% = len( str(desc$,1,pos(desc$ = HEX(20) ) ) )
               str(write_rec$(),start%, p%) = desc$ & "|"
               start% = start% + p%

               init(" ")  table$, field$
               table$ = "LITING"
               field$ = str(sp_part$,7,2)
               gosub lookup_desc

               call "SPCSMASH" (desc$)
               p% = len( str(desc$,1,pos(desc$ = HEX(20) ) ) )
               str(write_rec$(),start%, p%) = desc$ & "|"
               start% = start% + p%


               init(" ")  table$, field$
               table$ = "HINGE"
               field$ = str(sp_part$,9,2)
               gosub lookup_desc

               call "SPCSMASH" (desc$)
               p% = len( str(desc$,1,pos(desc$ = HEX(20) ) ) )
               str(write_rec$(),start%, p%) = desc$ & "|"
               start% = start% + p%


               init(" ")  table$, field$
               table$ = "SCREEN"
               field$ = str(sp_part$,11,1)
               gosub lookup_desc

               call "SPCSMASH" (desc$)
               p% = len( str(desc$,1,pos(desc$ = HEX(20) ) ) )
               str(write_rec$(),start%, p%) = desc$ & "|"
               start% = start% + p%


               init(" ")  table$, field$
               table$ = "LOCKS"
               field$ = str(sp_part$,12,1)
               gosub lookup_desc

               call "SPCSMASH" (desc$)
               p% = len( str(desc$,1,pos(desc$ = HEX(20) ) ) )
               str(write_rec$(),start%, p%) = desc$ & "|"
               start% = start% + p%

               init(" ") size$
               whole% = 0%
               fract  = 0.00
               sze   = 0.00
              
               convert str(sp_part$,13,3) to whole%, data goto bad_size
           
               convert str(sp_part$,16,1) to fract, data goto bad_size

               fract = fract / 8

               sze = whole% + fract

bad_size
               convert sze to size$, pic(###.####)
               p% = 9%
               str(write_rec$(),start%,p%) = size$ & "|"
               start% = start% + p%


               init(" ") size$
               whole% = 0%
               fract  = 0.00
               sze   = 0.00
              
               convert str(sp_part$,17,2) to whole%, data goto bad_size1
           
               convert str(sp_part$,19,1) to fract, data goto bad_size1

               fract = fract / 8

               sze = whole% + fract

bad_size1
               convert sze to size$, pic(###.####)
               p% = 9%
               str(write_rec$(),start%,p%) = size$ & "|"
               start% = start% + p%

               init(" ")  table$, field$
               table$ = "GRDTYPE"
               field$ = str(subpart$,1,1)
               gosub lookup_desc

               call "SPCSMASH" (desc$)
               p% = len( str(desc$,1,pos(desc$ = HEX(20) ) ) )
               str(write_rec$(),start%, p%) = desc$ & "|"
               start% = start% + p%

               init(" ")  table$, field$
               table$ = "GRDSIZE"
               field$ = str(subpart$,2,1)
               gosub lookup_desc

               call "SPCSMASH" (desc$)
               p% = len( str(desc$,1,pos(desc$ = HEX(20) ) ) )
               str(write_rec$(),start%, p%) = desc$ & "|"
               start% = start% + p%

               init(" ")  table$, field$
               table$ = "GRDCOLOR"
               field$ = str(subpart$,3,1)
               gosub lookup_desc

               call "SPCSMASH" (desc$)
               p% = len( str(desc$,1,pos(desc$ = HEX(20) ) ) )
               str(write_rec$(),start%, p%) = desc$ & "|"
               start% = start% + p%


               init(" ")  table$, field$
               table$ = "HARDWARE"
               field$ = str(subpart$,4,1)
               gosub lookup_desc

               call "SPCSMASH" (desc$)
               p% = len( str(desc$,1,pos(desc$ = HEX(20) ) ) )
               str(write_rec$(),start%, p%) = desc$ & "|"
               start% = start% + p%

               init(" ")  table$, field$
               table$ = "FOAM"
               field$ = str(subpart$,5,1)
               gosub lookup_desc

               call "SPCSMASH" (desc$)
               p% = len( str(desc$,1,pos(desc$ = HEX(20) ) ) )
               str(write_rec$(),start%, p%) = desc$ & "|"
               start% = start% + p%

               init(" ")  table$, field$
               table$ = "ACCESSOR"
               field$ = str(subpart$,6,1)
               gosub lookup_desc

               call "SPCSMASH" (desc$)
               p% = len( str(desc$,1,pos(desc$ = HEX(20) ) ) )
               str(write_rec$(),start%, p%) = desc$ & "|"
               start% = start% + p%

               init(" ")  table$, field$
               table$ = "INTCOLOR"
               field$ = str(infopart$,1,1)
               gosub lookup_desc

               call "SPCSMASH" (desc$)
               p% = len( str(desc$,1,pos(desc$ = HEX(20) ) ) )
               str(write_rec$(),start%, p%) = desc$ & "|"
               start% = start% + p%


               init(" ")  table$, field$
               table$ = "EXTCOLOR"
               field$ = str(infopart$,2,1)
               gosub lookup_desc

               call "SPCSMASH" (desc$)
               p% = len( str(desc$,1,pos(desc$ = HEX(20) ) ) )
               str(write_rec$(),start%, p%) = desc$ & "|"
               start% = start% + p%

               init(" ")  table$, field$
               table$ = "FIN"
               field$ = str(infopart$,4,1)
               gosub lookup_desc

               call "SPCSMASH" (desc$)
               p% = len( str(desc$,1,pos(desc$ = HEX(20) ) ) )
               str(write_rec$(),start%, p%) = desc$ & "|"
               start% = start% + p%


               put #ff%, using WRITE_FMT, write_rec$()
WRITE_FMT:           FMT 4*CH(256)


               write #ff%

               goto exit_program



               exit_program
                 end


               lookup_desc
                  init(" ") readkey$, desc$
                  str(readkey$,1,9)   = table$ 
                  str(readkey$,10,15) = field$

                  read #1, key = readkey$, using gen_fmt, desc$, ~
                                             eod goto no_gen_entry
gen_fmt:              FMT POS(25), CH(30)

               return
               no_gen_entry
                   desc$ = table$ & "  " & field$ & "  NO ENTRY "
               return


