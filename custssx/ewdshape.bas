        REM *************************************************************~
            *                                                           *~
            *  Program Name      - EWDSHAPE - Subroutine                *~
            *  Creation Date     - 07/11/01                             *~
            *  Last Modified Date- 05/22/02                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Verify Stock Special Shape Product   *~
            *                      and Frames in Stock                  *~
            *                                                           *~
            *                      (Input) sp_part$ = Production Part No*~
            *                      (Output) spec_part$ - Lookup Stock   *~
            *                                            Part No.       *~
            *                      shapes%=1%- Lookup for Shape Product *~
            *                      fram%  =1%- Lookup for frame         *~
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
            * 07/11/01 ! New Program for (EWD) - Last Mod Date    ! RHH *~
            * 05/22/02 ! (EWD001) - Mod to also check for double  ! CMG *~
            *          !    strength clear glass 'A0'.            !     *~
            *************************************************************

        sub "EWDSHAPE" (shapes%,            /* Special Shape Window    */~
                        fram%,              /* Special Shape Frame     */~
                        sp_part$,           /* Production Part Number  */~
                        spec_part$,         /* Stock Lookup Part No.   */~
                        #1 )                /* GENCODES                */

        dim                                                              ~
            sp_part$25,                     /* Production Part No.     */~
            spec_part$25,                   /* Lookup Part No. Modified*/~
            cl$1,                           /* Color Type Code         */~
            ty$2,                           /* Glass Type Code         */~
            readkey$24, desc$32             /* Use for Table Lookup    */


        REM *************************************************************~
            *V a l i d a t i o n  C h e c k  S p e c i a l  S h a p e s *~
            *************************************************************

            gosub L10000
            goto exit_sub
 
L10000: 
            shapes% = 0% : fram% = 0%
            init(" ") readkey$, desc$, spec_part$, cl$, ty$
            
            cl$ = str(sp_part$,4%,1%)            /* Product Color Code */
            ty$ = str(sp_part$,5%,2%)            /* Product Glass code */
 
                                                 /* Check Valid Stock  */
                                                 /* Models 1st         */
            str(readkey$,1%,9%) = "PLANSSHAP"
            str(readkey$,10%,3%) = str(sp_part$,1%,3%)
            read #1,key = readkey$, eod goto L10010
                                                 /* Check Color (White)*/
            if str(sp_part$,4%,1%) <> "2" then goto check_special_fram
                                                 /* Check Glass        */
                                                 /* Clear Only         */
                                                 /* (EWD001) - CHECK DS*/
                                                 /* Clear Glass        */
            if str(sp_part$,5%,2%) <> "01" and str(sp_part$,5%,2%) <> "A0"~
                                  then goto check_special_fram
                                                 /* Check liting       */
                                                 /* Only 90 & 92       */
            if str(sp_part$,7%,2%) <> "90" and                           ~
               str(sp_part$,7%,2%) <> "92" then goto check_special_fram
                                                 /* Check Fields       */
                                                 /* Hinge and Locks 000*/
                                                 /* Same applies to    */
                                                 /* Frame Criteria     */
            if str(sp_part$,9%,3%) <> "000" then return
   
                                                 /* Throw away Mulling */
                                                 /* Only use 19 digit  */
                                                 /* Part Number-lookup */
            shapes% = 1%
            str(spec_part$,1%,19%) = str(sp_part$,1%,19%)
            str(spec_part$,9%,3%) = "000"                  /* Filler   */
L10010: return  

        check_special_fram
            if cl$ <> "2" and cl$ <> "6" then return

                                              /* Check for Tempered    */
                                              /* Glass. All other glass*/
                                              /* is acceptable         */
            gosub check_temp 
            if sp_temp% = 1% then return
                                              /* Check Hinge and Locks */ 
            if str(sp_part$,9%,3%) <> "000" then return
                                              /* Throw away Mulling    */
                                              /* Only use 19 digit     */
                                              /* Part Number-lookup    */
            fram% = 1%
            str(spec_part$,1%,19%) = str(sp_part$,1%,19%)
            str(spec_part$,5%,4%) = "XXXX"               /* Glass Grid */
        return
  
        check_temp        
            sp_temp% = 0%
            init(" ") readkey$
            str(readkey$,1%,9%)   = "PLAN TEMP"
            str(readkey$,10%,15%) = ty$
            read #1,key = readkey$, eod goto check_temp_done
               sp_temp% = 1%
        check_temp_done
        return     

        exit_sub
        end
