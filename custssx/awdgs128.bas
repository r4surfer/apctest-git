        REM *************************************************************~
            *  Subroutine Name   - AWDGS128                             *~
            *  Creation Date     - 04/26/2021                           *~
            *  Written By        - Ricky Beane                          *~
            *                                                           *~
            *  Description       - Generate the check digit for the     *~
            *                      GS1-128 barcode.                     *~
            *                                                           *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 04/26/21 ! Original -                               ! RDB *~
            *************************************************************
                           
            sub "AWDGS128" (gs_unique$,        /* Characters unique for code */~
                            full_gs128$,       /* Full 30 character GS1-128  */~
                            error%)            /* Return Code                */
                        
        dim                                                                    ~
            chkgsarry$(17%)1,                   /* GS1-128 Check digit       */~
            working_gs$19%,                     /* Working area for GS1-128  */~
            gs_set_flds$8,                      /* Set 0 with Atrium code    */~
            gs_unique$9,                        /* Unique data for code      */~
            chkgsdigit$1,                       /* Calculated check digit    */~
            full_gs128$20                       /* GS1-128 code              */


          init(" ") chkgsarry$(), chkgsdigit$
          fmla% = 0% : rmdr% = 0% : chkgsdigit% = 0%
          
          gs_set_flds$ = "00719801" 
          
          working_gs$ = "00" & gs_set_flds$ & gs_unique$

          for g% = 1% to 19%
             chkgsarry$(g%) = str(working_gs$,g%,1%)
          next g%

          for a% = 3% to 19% step 2%
              convert chkgsarry$(a%) to nbr%
              fmla% = fmla% + nbr%
          next a%

          fmla% = fmla% * 3%
  
          for b% = 4% to 18% step 2%
              convert chkgsarry$(b%) to nbr%
              fmla% = fmla% + nbr%
          next b%        
         
                  
          rmdr% = MOD(fmla%,10)
          
          if rmdr% = 0% then ~
              chkgsdigit% = 0% ~
                        else ~
              chkgsdigit% = 10% - rmdr%
              
          convert chkgsdigit% to chkgsdigit$, pic(#)    
          
          full_gs128$ = str(working_gs$,1%,19%) & chkgsdigit$
          
          error% = 0%

        end


