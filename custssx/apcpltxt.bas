        REM *************************************************************~
            *                                                           *~
            *  Subroutine Name   - APCPLTXT                             *~
            *  Creation Date     - 11/06/96                             *~
            *  Last Modified Date- 03/16/98                             *~
            *  Written By        - Royal H. Hoffman                     *~
            *                                                           *~
            *  Description       - Find the 1st two Lines of either     *~
            *                      Header or Line Item Text             *~
            *                                                           *~
            *-----------------------------------------------------------*~
            *                  M O D I F I C A T I O N S                *~
            *---WHEN---+----------------WHAT----------------------+-WHO-*~
            * 11/06/96 ! New Program for (APC) - Last Mod Date    ! RHH *~
            * 03/16/98 ! Check Text Routine                       ! RHH *~
            *************************************************************

         sub "APCPLTXT" (#1, textid$, text$(), nbr_line%)

        dim                                                              ~
            textid$4,                    /* Text Id.                   */~
            text$(2%)70,                 /* 1st Two Lines of Text.     */~
            text_key$11,                 /* Primary Key for Text       */~
            s_key$9                      /* Save 1 thru 9 of Key       */

            nbr_line% = 0%
            init(" ") text_key$, s_key$, text$()
            gosub'099(textid$)
            if txt% = 0% then goto exit_sub
               text_key$ = all(hex(00))
               str(text_key$,1%,1%) = "M"
               str(text_key$,2%,3%) = "   "
               str(text_key$,5%,4%) = textid$
               str(text_key$,9%,1%) = "1"
               s_key$               = str(text_key$,1%,9%)
               read #1,key > text_key$, eod goto exit_sub
                  get #1, using L00380, text_key$, text$()
L00380:              FMT CH(11), POS(64), 2*CH(70)
               if s_key$ <> str(text_key$,1%,9%) then goto exit_sub
                  if text$(1%) <> " " then nbr_line% = nbr_line% + 1%
                  if text$(2%) <> " " then nbr_line% = nbr_line% + 1%
        exit_sub
        end

        deffn'099(textid$)
            txt% = 0%
            if textid$ = hex(00000000) or textid$ = hex(ffffffff)        ~
                                           or textid$ = " " then return
            txt% = 1%
        return

