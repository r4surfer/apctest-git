        REM *************************************************************~
            *                                                           *~
            *  Program Name      - APCPLNEF                             *~
            *  Creation Date     - 11/20/02                             *~
            *  Last Modified Date- 07/01/2013                           *~
            *  Description       - Routine to calcuate value of         *~
            *                      effective units based on table       *~
            *                      'PLAN EFF '                          *~
            *  Special Comments                                         *~
            *     ERR% -  Description                                   *~
            *     ----    --------------                                *~
            *      0%     Everything is okay calc unit.                 *~
            *     99%     Error Do on add or substract unit             *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/20/02 ! New Program for (EWD) - Last Mod Date    ! CMG *~
            *07/06/2012! (AWD001) mod for parts effective untis   ! CMG *~
            *07/01/2013! (AWD002) OGO change ecn 2013-032         ! CMG *~
            *************************************************************

            sub "APCPLNEF" ( part$,      /* Part Number                */~
                             pl_unit%,   /* Number of planning units IN*/~
                             ef_unit,    /* Number of effective unitOUT*/~
                             err%,       /* Error Code              OUT*/~
                             #1 )        /* FILE = (GENCODES)          */

        dim                              /* Subroutine - Variables     */~
            part$25,                     /* Part Number                */~
            model$3,                     /* Model Number               */~
            sc$2,                        /* Screen Value               */~
            readkey$30,                  /* Gencodes Read Key          */~
            desc$30,                     /* Gencodes Description       */~
            mull$3                       /* Mull Code (AWD001)         */


            init(" ") readkey$, desc$, model$, sc$
            ef_unit, factor = 0.00
            err% = 99%
            if str(part$,1%,25%) = " " then goto end_sub
            model$ = str(part$,1%,3%)
            sc$    = str(part$,11%,1%)
            gosub lookup_value
            gosub convert_value
            gosub check_parts                                 /* (AWD001) */
            if str(part$,5%,4%) = "WARR" then ef_unit = 0.00


            end_sub
            end

            lookup_value
               factor = 1
               str(readkey$,1%,9%)    = "PLAN EFF "
               str(readkey$,10%,15%)  = model$
               read #1,key = readkey$, using L10000, desc$, eod goto no_value
L10000:           FMT POS(25), CH(30)

               convert str(desc$,1%,5) to factor, data goto no_value
        
            no_value
            return

            convert_value
               gosub check_parts
               
               ef_unit = round(pl_unit% * factor,2)
               err% = 0%
            return
/* (AWD001) */      
            check_parts
              partLen% = len(part$)
              if partLen% < 19% then factor = .20              
              if partLen% < 19% then return

              if sc$ = "4" then factor = .33
              if sc$ = "5" then factor = .33
              if sc$ = "6" then factor = .33              
/*(AWD002)*/  if sc$ = "7" then factor = .33    

              if sc$ = "4" or sc$ = "5" or sc$ = "6" then return
              if sc$ = "7" then return
/*(\AWD002)*/
              
              init(" ") mull$
              if partLen% = 22% then mull$ = str(part$,20%,3%)
              if partLen% = 25% then mull$ = str(part$,23%,3%)
              if mull$ <> " " then goto checkMull
            return
            checkMull
              if mull$ >= "013" and mull$ <= "028" then goto setPart
              if mull$ = "036" then goto setPart
            return
            setPart
              factor = .20
            return
/* (\AWD001) */

