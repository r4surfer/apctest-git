            select # 1, "PROTO",~
                        varc, indexed,~
                        recsize = 25,~
                        keypos  =   5, keylen = 21, ~
			alt key 1, keypos = 2, keylen = 24, ~
			key 2, keypos = 1, keylen = 25

            call "OPENCHCK" (#1, 0%, 0%, 100%, " ")
