        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - ORASLMM                              *~
            *  Creation Date     - 01/25/2018                           *~
            *  Last Modified Date-                                      *~
            *  Written By        - Christie Norman                      *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                                                           *~
            *-----------------------------------------------------------*~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            *01/25/2018!(CR1289) Orginal                          ! CMN *~
            *************************************************************


        sub "ORASLMM" (#1,              /* SLMMASTR                   */~
                       trans$,                                         ~
                       action$,                                        ~
                       file$,                                     ~
                       fields$(),                                       ~
                       no_fields%,                                     ~
                       error%)          /* Error Flag from File Open  */

        dim key$4,                       /* Read Key                   */~
            error$256,                   /* Error String               */~
            field$256,                   /* Query Return Field         */~
            stmt1$250,                   /* First Query String         */~
            stmt2$250,                   /* Second Query String        */~
            fields$(100)64,                                              ~
            rec$(3)200                   /* Customer file record       */

        dim message$256


            found = 1
            if action$   = "A" then goto add_record
            if action$   = "C" then goto change_record
            if action$   = "D" then goto delete_record
            goto FINI

change_record:
add_record:
            key$ = fields$(1%)
            read #1, key = key$, hold, eod goto add_rec
             get #1, using L25000, rec$()
             goto set_field

L25000:         FMT 3*CH(200)
add_rec:    init(" ") rec$()
            str(rec$(),1,9) = key$ & "         "
            found = 0
set_field:
        
            str(rec$(),001%,04%) = fields$(1%)   /* Salesman Number  */
            str(rec$(),005%,30%) = fields$(2%)   /* Salesman Name    */
            str(rec$(),035%,30%) = fields$(3%)   /* Salesman Address1*/
            str(rec$(),065%,30%) = fields$(4%)   /* Salesman Address2*/
            str(rec$(),095%,30%) = fields$(5%)   /* Salesman Address3*/
            str(rec$(),125%,10%) = fields$(6%)   /* Salesman PhoneNum*/
            str(rec$(),135%,20%) = fields$(7%)   /* Variable Field 1 */
            str(rec$(),155%,20%) = fields$(8%)   /* Variable Field 2 */
            str(rec$(),175%,20%) = " "           /* Variable Field 3 */
            str(rec$(),195%,20%) = " "           /* Variable Field 4 */
            str(rec$(),215%,20%) = " "           /* Variable Field 5 */
            str(rec$(),235%,20%) = " "           /* Variable Field 6 */
            str(rec$(),255%,20%) = " "           /* Variable Field 7 */
            str(rec$(),275%,20%) = " "           /* Variable Field 8 */
            str(rec$(),295%,20%) = " "           /* Variable Field 9 */
            str(rec$(),315%,20%) = " "           /* Variable Field 10*/
            str(rec$(),335%,50%) = fields$(9%)   /* Salesman Email   */
            str(rec$(),385%,216%) = " "

            put #1, using L25000, rec$()
            if found = 1 then rewrite #1               ~
              else write #1
         goto FINI

delete_record:
            key$ = fields$(1%)  & "        "
            read #1, key = key$, hold, eod goto FINI
            delete #1
         goto FINI



FINI:
        end

        