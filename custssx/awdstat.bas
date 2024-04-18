        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDSTAT                              *~
            *  Creation Date     - 06/02/04                             *~
            *  Last Modified Date- 07/19/2013                           *~
            *  Written By        - Christie Gregory                     *~
            *                                                           *~
            *  Description       - Subroutine to be called by the Window*~
            *                      Wizard by Sales Order Number to      *~
            *                      return APCPLNOR status.              *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 06/02/04 ! New Program for (AWD) - Last Mod Date    ! CMG *~
            * 04/20/06 ! (AWD001) mod to open planning files on   ! CMG *~
            *          !   both NC and NE                         !     *~
            *07/19/2013! (AWD002) mod to check for D painted SO   ! CMG *~
			*11/10/2022! CR3196   reset sales order beginning at 1! RDB *~
            *************************************************************


        sub "AWDSTAT"    (so$,           /* Sales Order Number - IN    */~
                          st$,           /* SO Status Code     - OUT   */~
                          err% )         /* 0 = OK, NOT 0 = ERROR      */

        dim or_so$8,                     /* APCPLNOR Sales Order Number*/~
            or_st$2,                     /* APCPLNOR Status Code       */~
			or_prefix_so$1               /* CR3196 Sales order prefix  */

        dim                              /* (Program) - Variables      */~
            filename$8                   /* Used By EWDOPEN            */

        dim library$8,                   /* (AWD001)                   */~
            volume$6,                    /* (AWD001)                   */~
            vtoc$22,                     /* (AWD001)                   */~
            so$8,                        /* (AWD001)                   */~
            st$2                         /* (AWD001)                   */


            err% = 0%
        REM *************************************************************~
            *                  S E L E C T   F I L E S                  *~
            *                                                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! APCPLNOR ! Planning S.O. Header History             *~
            *************************************************************~

            select #1,   "APCPLNOR",                                     ~
                        varc,     indexed,  recsize = 170,               ~
                        keypos =    1, keylen =   51,                    ~
                        alt key  1, keypos =   27, keylen =  25,         ~
                            key  2, keypos =   70, keylen =   8, dup,    ~
                            key  3, keypos =   78, keylen =   8, dup,    ~
                            key  4, keypos =   52, keylen =   8,         ~
                            key  5, keypos =   36, keylen =  16, dup

            select #2,   "APCPLNOR",                                     ~
                        varc,     indexed,  recsize = 170,               ~
                        keypos =    1, keylen =   51,                    ~
                        alt key  1, keypos =   27, keylen =  25,         ~
                            key  2, keypos =   70, keylen =   8, dup,    ~
                            key  3, keypos =   78, keylen =   8, dup,    ~
                            key  4, keypos =   52, keylen =   8,         ~
                            key  5, keypos =   36, keylen =  16, dup


               cnt% = 0%
               x% = 1%
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "APCPLNOR" 
               library$  = "APCDATA " 
               volume$   = "?" 

               call "FIND" addr(filename$, library$, volume$,1%,x%,vtoc$)
               if vtoc$ = " " then goto file_error
                  volume$ = vtoc$
                open nogetparm #1,                                       ~
                     shared,                                             ~
                     file    = filename$,                                ~
                     library = library$,                                 ~
                     volume  = volume$

               x% = 1%
               init(" ") filename$, library$, volume$, vtoc$
               filename$ = "APCPLNOR" 
               library$  = "NEDATA" 
               volume$   = "?" 
               call "FIND" addr(filename$, library$, volume$,1%,x%,vtoc$)
               if vtoc$ = " " then goto file_error
                  volume$ = vtoc$
                open nogetparm #2,                                       ~
                     shared,                                             ~
                     file    = filename$,                                ~
                     library = library$,                                 ~
                     volume  = volume$

            or_prefix_so$ = "1"           
            ff% = 1%
            or_so$ = so$
            gosub lookup_order
REM            call "SHOSTAT" ("OR ST 1 --> " & or_st$)  stop
            st$ = or_st$

            str(or_so$,1,1) = "A"
            gosub lookup_order
REM            call "SHOSTAT" ("OR ST 2 --> " & or_st$)  stop
            if or_st$ > st$ then st$ = or_st$
            
            str(or_so$,1,1) = "C"
            gosub lookup_order
REM            call "SHOSTAT" ("OR ST 2 --> " & or_st$)  stop
            if or_st$ > st$ then st$ = or_st$
                        
            str(or_so$,1,1) = "D"
            gosub lookup_order
REM            call "SHOSTAT" ("OR ST 2 --> " & or_st$)  stop
            if or_st$ > st$ then st$ = or_st$

            

            ff% = 2%
REM            str(or_so$,1,1) = "0"
            or_so$ = so$                  /* CR3196 */
            gosub lookup_order
REM            call "SHOSTAT" ("OR ST 3 --> " & or_st$)  stop
            if or_st$ > st$ then st$ = or_st$

            str(or_so$,1,1) = "B"
            gosub lookup_order
REM            call "SHOSTAT" ("OR ST 3 --> " & or_st$)  stop
            if or_st$ > st$ then st$ = or_st$
            
            str(or_so$,1,1) = "C"
            gosub lookup_order
REM            call "SHOSTAT" ("OR ST 3 --> " & or_st$)  stop
            if or_st$ > st$ then st$ = or_st$
                        
            str(or_so$,1,1) = "D"
            gosub lookup_order
REM            call "SHOSTAT" ("OR ST 3 --> " & or_st$)  stop
            if or_st$ > st$ then st$ = or_st$
                        

            if cnt% <= 0% then goto no_or

            end

      

         lookup_order
            init(" ") or_st$

            read #ff%,hold,key 4% = or_so$, using L00010  , or_st$, ~
			     or_prefix_so$, eod goto no_so  

L00010:                 FMT POS(60), CH(2), POS(170), CH(1)
            cnt% = cnt% + 1%

         return
         no_so

         return




            no_or
               err% = 1%

            end


            file_error
               err% = 2%
 
            end


