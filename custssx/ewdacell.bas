        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDACELL - Subroutine                *~
            *  Creation Date     - 11/24/03                             *~
            *  Last Modified Date- 01/25/06                             *~
            *  Written By        - Chrisite M. Gregory                  *~
            *                                                           *~
            *  Description       - Validate is Product is 'A' Cell      *~
            *                      Product                              *~
            *                                                           *~
            *                      (Input) sp_part$ = Production Part No*~
            *                      (Output) spec_part$ - Lookup Stock   *~
            *                                            Part No.       *~
            *                      a_cell%=1%- Lookup for A Cell Product*~
            *                                                           *~
            *  Where Used        - (APCPLN6B) Sub of BCKUPDTE           *~
            *                    - (APCPLN06) Planning Program          *~
            *                    - (APCPL41A) Production Reports/Labels *~
            *                                                           *~
            *                                                           *~
            *                                                           *~ 
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/24/03 ! New Program for (EWD) - Last Mod Date    ! CMG *~
            * 01/25/06 ! PAR000 CR347 part mods                   ! CMG *~
            *************************************************************

        sub "EWDACELL" (a_cell%,            /* Special 'A' Cell Window */~
                        sp_part$,           /* Production Part Number  */~
                        sp_subp$,           /* SubPart Number          */~
/*PAR000*/              spec_part$,         /* Stock Lookup Part No.   */~
/*PAR000*/              spec_subp$,         /* Stock sub lookup partno */~
                        #1,                 /* GENCODES                */~
                        #11 )               /* INVMASTER               */

        dim                                                              ~
            sp_part$25,                     /* Production Part No.     */~
            spec_part$25,                   /* Lookup Part No. Modified*/~
/*PAR000*/  sp_subp$20,                     /* Subpart                 */~
/*PAR000*/  spec_subp$20,                   /* Lookup subpart modified */~
/*PAR000*/  invmastr_key$45,                /* INVMASTR readkey        */~
            cl$1,                           /* Color Type Code         */~
            ty$2,                           /* Glass Type Code         */~
            gd$2,                           /* Grid Type Code          */~
            hg$2,                           /* Hinge Type Code         */~
            lk$1,                           /* Locks Type Code         */~
            sc$1,                           /* Screen Type Code        */~
            readkey$24, desc$32             /* Use for Table Lookup    */


        REM *************************************************************~
            *V a l i d a t i o n  C h e c k  S p e c i a l  S h a p e s *~
            *************************************************************

            gosub L10000
            goto exit_sub
 
L10000: 
            a_cell% = 0% 
            init(" ") readkey$, desc$, spec_part$, cl$, ty$, gd$, lk$, ~
                      sc$
            
            cl$ = str(sp_part$,4%,1%)            /* Product Color Code */
            ty$ = str(sp_part$,5%,2%)            /* Product Glass code */
            gd$ = str(sp_part$,7%,2%)            /* Product Grid  code */
            hg$ = str(sp_part$,9%,2%)            /* Product Hinge code */
            lk$ = str(sp_part$,11%,1%)           /* Product Lock  code */
            sc$ = str(sp_part$,12%,1%)           /* Product Screen code*/
 
                                                 /* Check Valid Stock  */
                                                 /* Models 1st         */
            gosub check_model
            if a_cell% = 0% then goto L10010
                                                 /* Check Color (White)*/
            gosub check_color
            if a_cell% = 0% then goto L10010
                                                 /* Check Glass        */
            gosub check_glass
            if a_cell% = 0% then goto L10010
                                                 /* Check Grid        */
            gosub check_grid
            if a_cell% = 0% then goto L10010
                                                 /* Check Hinge       */
            gosub check_hinge
            if a_cell% = 0% then goto L10010
                                                 /* Check Screen      */
            gosub check_screen
            if a_cell% = 0% then goto L10010
                                                 /* Check Lock        */
            gosub check_lock
            if a_cell% = 0% then goto L10010

   
                                                 /* Throw away Mulling */
                                                 /* Only use 19 digit  */
                                                 /* Part Number-lookup */
            str(spec_part$,1%,19%) = str(sp_part$,1%,19%)
            str(spec_part$,4%,9%)  = "XXXXXXXXX"
            str(spec_subp$,1%,20%) = sp_subp$        /* PAR000 */

             gosub check_item_master
L10010: return

        check_model
            init(" ") readkey$, desc$
            str(readkey$,1%,9%) = "PLANAMODL"
            str(readkey$,10%,3%) = str(sp_part$,1%,3%)
            read #1,key = readkey$, eod goto not_model
                 a_cell% = 1%
        not_model
        return
  

        check_color
            init(" ") readkey$, desc$
            a_cell% = 0%
            str(readkey$,1%,9%) = "PLANACOLO"
            str(readkey$,10%,1%) = str(cl$,1%,1%)
            read #1,key = readkey$, eod goto not_color
                 a_cell% = 1%
        not_color
        return

        check_glass
            init(" ") readkey$, desc$
            a_cell% = 0%
            str(readkey$,1%,9%) = "PLANAGLAS"
            str(readkey$,10%,2%) = str(ty$,1%,2%)
            read #1,key = readkey$, eod goto not_glass
                 a_cell% = 1%
        not_glass
        return


        check_grid
            init(" ") readkey$, desc$
            a_cell% = 1%
            str(readkey$,1%,9%) = "PLANAGRID"
            str(readkey$,10%,2%) = str(gd$,1%,2%)
            read #1,key = readkey$, eod goto not_grid
                 a_cell% = 0%
        not_grid
        return

        check_hinge
            init(" ") readkey$, desc$
            a_cell% = 1%
            str(readkey$,1%,9%) = "PLANAHING"
            str(readkey$,10%,2%) = str(hg$,1%,2%)
            read #1,key = readkey$, eod goto not_hinge
                 a_cell% = 0%
        not_hinge
        return


        check_screen
            init(" ") readkey$, desc$
            a_cell% = 0%
            str(readkey$,1%,9%) = "PLANASCRE"
            str(readkey$,10%,1%) = str(sc$,1%,1%)
            read #1,key = readkey$, eod goto not_screen
                 a_cell% = 1%
        not_screen
        return

        check_lock
            init(" ") readkey$, desc$
            a_cell% = 0%
            str(readkey$,1%,9%) = "PLANALOCK"
            str(readkey$,10%,1%) = str(lk$,1%,1%)
            read #1,key = readkey$, eod goto not_lock
                 a_cell% = 1%
        not_lock
        return

        check_item_master
           a_cell% = 0%
* PAR000
           init(" ") invmastr_key$
           str(invmastr_key$,1% ,25%) = spec_part$
           str(invmastr_key$,26%,20%) = spec_subp$
           read #11,key = invmastr_key$, eod goto not_a_cell
               a_cell% = 1%
        not_a_cell
        return                                                            


        return



        exit_sub
        end


