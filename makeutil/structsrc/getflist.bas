        sub "GETFLIST" (inpath$, tgt$(), tgt%, ext%, ext$())

        dim inpath$255, tgt$(800)8, ext$(5)8
	dim slash$1
        dim work$(800)22, path$256

	slash$ = "/"
        path$ = inpath$
        str(path$,len(path$)+1%, 1%) = hex(00)

        wk%, wkr% = 800%
        call "READVTOC" addr("f", path$, 1%, wk%, 22%, work$(1%),    ~
                                  ret%, wkr%)
            tgt% = 0%
            if ret% <> 0% then end
            if wk%  <= 0% then end

            for i% = 1% to wk%
                s% = pos(-str(work$(i%)) = slash$)
                if s% = 0% then L01460
                   s% = s% + 1%
                d% = pos(str(work$(i%),s%) = ".")
                if d% = 0% then                                          ~
                   l% = len(str(work$(i%))) - s%                         ~
                           else                                          ~
                   l% = d% - 2%

                if d% > 0% then L01240
                   if ext% > 0% then L01460
                   goto L01400

L01240:         if ext% = 0% then L01460
                d% = s% + d% -1%
                for j% = 1% to ext%
                   if str(work$(i%),d%) = ext$(j%) then L01400
                next j%
                goto L01460

L01400:         tgt% = tgt% + 1%
                for j% = 0% to l%
                    str(tgt$(tgt%),j%+1%,1%) = str(work$(i%),s%+j%,1%)
                    if str(tgt$(tgt%),j%+1%,1%) > hex(60) then           ~
                       str(tgt$(tgt%),j%+1%,1%) = and(hex(df))
                next j%
L01460:     next i%

            call "SORT" addr(tgt$(), tgt%, 8%, tgt$(), 1%, 8%, "A")

            end
