        REM *************************************************************~
            *                                                           *~
            *  Program Name      - AWDSURGE - Subroutine                *~
            *  Creation Date     - 02/24/04                             *~
            *  Last Modified Date-                                      *~
            *  Written By        - Chrisite M. Gregory                  *~
            *                                                           *~
            *  Description       - This routine checks to see if the    *~
            *                      product is a pull from stock surge   *~
            *                      item and if so deletes the allocated *~
            *                      qty.                                 *~
            *                                                           *~
            *                      (Input) sp_part$ = Production Part No*~
            *                                                           *~
            *  Where Used        - (EWDPLA44) Sub of APCPLA44           *~
            *                    - (APCPLN03) Planning Program          *~
            *                    - (EWDPLN79) backorder program         *~
            *                                                           *~
            *                                                           *~
            *                                                           *~ 
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 02/24/04 ! New Program for (EWD) - Last Mod Date    ! CMG *~
            *************************************************************



        sub "AWDSURGE" (mfg_part$,          /* Production Part Number  */~
                        inv_qty%,           /* Addjusted Inv Qty       */~
                        #3 )                /* APCPLNSC                */


        dim                                                              ~
            filename$8,                     /* Used By EWDOPEN         */~
            hdr$45, msg$(3%)79,             /* Askuser - Var's         */~
            mfg_part$25,                    /* Production Part No.     */~
            master_key$25,                  /* HNYMASTR Readkey        */~
            quan_key$44,                    /* HNYQUAN Readkey         */~
            sc_key$10,                      /* APCPLNSC Readkey        */~
            or_key$8,                       /* APCPLNOR Readkey        */~
            pull_flag$1                     /* Inventory Pull Flag     */



        REM *************************************************************~
            *                     S E L E C T                           *~
            *-----+----------+------------------------------------------*~
            *FILE#!  PRNAME  ! D E S C R I P T I O N                    *~
            *-----+----------+------------------------------------------*~
            * #1  ! HNYMASTR ! Inventory Master File                    *~
            * #2  ! HNYQUAN  ! Inventory Quantities File                *~
            *************************************************************~
            *       File Selection and Open Calls                       *~
            *************************************************************

            select #1,  "HNYMASTR",                                      ~
                         varc,                                           ~
                         indexed,                                        ~
                         recsize = 900,                                  ~
                         keypos = 1, keylen = 25,                        ~
                         alternate key 1, keypos = 102, keylen = 9, dup, ~
                                   key 2, keypos = 90, keylen = 4, dup,  ~
                                   key 3, keypos = 26, keylen = 32, dup

            select #2,  "HNYQUAN",                                       ~
                        varc,                                            ~
                        indexed,                                         ~
                        recsize =   650,                                 ~
                        keypos =   17, keylen =  44,                     ~
                        alt key  1, keypos =    1, keylen =  44  


            filename$ = "HNYMASTR" : call "EWDOPEN" (#1,  filename$, err%)
            if err% <> 0% then gosub open_error
            filename$ = "HNYQUAN " : call "EWDOPEN" (#2, filename$, err%)
            if err% <> 0% then gosub open_error


        REM *************************************************************~
            *V a l i d a t i o n  C h e c k  R e m o v e  O r  B O      *~
            *************************************************************

            gosub check_hnymastr
            if master% = 0% then goto end_sub
            gosub update_hnyquan


            end_sub
               close #1
               close #2

            end


            check_hnymastr
               master% = 0%
               init(" ") master_key$
               str(master_key$,1%,25%) = mfg_part$
               
               read #1, key = master_key$, eod goto mastr_done

                    get #1, using L00100, pull_flag$

L00100:                  FMT POS(586), CH(1)

                    if pull_flag$ <> "N" then goto mastr_done
 
               master% = 1%
            mastr_done
            return


            update_hnyquan
                init(" ") quan_key$
                alloc = 0.00
                str(quan_key$,1%,25%) = mfg_part$
                str(quan_key$,26%,3%) = "300"

                read #2, hold, key = quan_key$, eod goto no_quan

                     get #2, using L00200, alloc

L00200:                  FMT POS(109), PD(15,4)

                     alloc = alloc - inv_qty%

                     rewrite #2, using L00200, alloc
         
            no_quan
            return


        open_error
           init(" ") hdr$, msg$()
           comp% = 2%
           hdr$  = "******* (Error) (Error) (Error)  *******"
           msg$(1%) = " - - - - - - - - E r r o r - - - - - - - - "
           msg$(2%) = "(Open Error) - File = " & filename$
           msg$(3%) = "Press Any Key To Continue."
           call "ASKUSER" (comp%, hdr$, msg$(1%), msg$(2%), msg$(3%))
        return

