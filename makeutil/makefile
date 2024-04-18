# Master makefile for creation of individual makefiles for application
# directories.
#
# When in this directory, 'make' or 'make all' will create makefiles or
# makefile sets (subsrcxx_o) for application source directories.
# [In this context '_o' directories are both sources and targets]
#
# makefiles can be made for individual directories by entering 'make dir-name'
#
# makefiles are dynamic in the sense that the list of elements to make is
# generated from the current contents of the source library.  This means that
# a makefile cannot be generated properly unless the source library is complete.
#   [Translation: a makefile for utilsrce_o cannot be made until after 
#    utilsrce is compiled from source to .o's...etc.]
#
#	EXCEPTION:  Partitioned directories such as subsrce2_o are controlled 
#		    by lists in a corresponding library below makeutil. Each 
#		    list corresponds to the contents of one load module.  Care 
#		    must be taken to be sure each element of subsrce2_o is 
#		    present on one and only one list. (This is of particular 
#		    importance for custom subroutines which do not overlay 
#		    generic subroutines, but are intended for inclusion 
#		    in .sl or .a files.  The lists WILL have to be updated.)
#
# Special functions:
#  1) 'make genlist': makes the genlist utility used to manipulate lists
#                     so that makefiles can be dynamic in the context of current
#		      library contents. For more information enter 'genlist -h'
#		      or see examples of usage in this makefile.
#
#  2) 'make devel': makes a prototype makefile for developers - makedev.sam.
#
#  3) 'make srcdiff': makes an interactive source comparison utility. More
#                     usable than diff under some conditions.
#
#  4) 'force_it': there should NEVER be a file or directory in makeutil named
#		  'force_it'.  By its absence it is 'out of date' and thus
#		  forces the making of makefiles for libraries which DO have
#		  counterparts in makeutil such as subsrce2.
#
# General Comments - This makes use of the fact that most makefiles have 
#  common elements. A makefile for source2 is exactly the same as a makefile
#  for sourcecm with the exception of the list of source units processed.
#  contained in makeutil are the common fragments of these makefiles.  The
#  technique is to make use of the genlist utility to create the dynamic 
#  fragments, then to use 'cat' & echo to piece then together with the
#  common fragments.  This accompishes one major objective:  removal of 
#  manual maintenance for most of the makefile process.  As a side benefit
#  it provides a central location for customization of flag settings etc.
#
#
all: \
        subsrcr3        subsrcr2        cutilsrce       utilsrce   \
        subsrce3        subsrce2        custsr3         custsr2    \
        custss3         custss2         custssx         custssx_ora\
        presr3                                                     \
        presr2          press3          press2          pressx     \
        presubs         custsubs                                   \
        subobj_o                                                   \
        subsrcr2_o      subsrcr3_o      subsrce2_o      subsrce3_o \
        pressx_o        custssx_o                                  \
        utilsrce_o      cutilsrce_o     fixes                      \
        sourceac        sourcecd        sourcecm        sourceec   \
        sourceeq        sourcefa        sourcefi        sourcemc   \
        sourcepc        sourcepp        sourcewa                   \
        source2                                                    \
        presrcx         custsrcx        custsrcx_ora    presrce    \
        custsrce

subsrcr2: force_it
	if ( test -d ../subsrcr2 ) then					\
		(ls ../subsrcr2 > ./flist1.txt;				\
		 ./genlist;						\
		 touch ./tmpmake0;					\
		 cat ./ssx_omake0 >> ./tmpmake0;			\
		 echo "\t\$$(OR2)\n" >> ./tmpmake0;			\
		 cat ./tmpmake0 ./ssx_omake1 > ./tmpmake1;		\
		 cat ./tmpmake1 ./flist2.txt > ./tmpmake2;		\
		 cat ./tmpmake2 ./trgtmake > ./tmpmake3;		\
		 cat ./tmpmake3 ./endmake > ../subsrcr2/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

subsrcr3: force_it
	if ( test -d ../subsrcr3 ) then					\
		(ls ../subsrcr3 > ./flist1.txt;				\
		 ./genlist;						\
		 touch ./tmpmake0;					\
		 cat ./ssx_omake0 >> ./tmpmake0;			\
		 echo "\t\$$(OR3)\n" >> ./tmpmake0;			\
		 cat ./tmpmake0 ./ssx_omake1 > ./tmpmake1;		\
		 cat ./tmpmake1 ./flist2.txt > ./tmpmake2;		\
		 cat ./tmpmake2 ./trgtmake > ./tmpmake3;		\
		 cat ./tmpmake3 ./endmake > ../subsrcr3/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

utilsrce: force_it
	if ( test -d ../utilsrce ) then					\
		(ls ../utilsrce > ./flist1.txt;				\
		 ./genlist;						\
		 cat ./uts_omake ./flist2.txt > ./tmpmake1;		\
		 cat ./tmpmake1 ./trgtmake > ./tmpmake2;		\
		 cat ./tmpmake2 ./endmake > ../utilsrce/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

cutilsrce: force_it
	if ( test -d ../cutilsrce ) then				\
		(ls ../cutilsrce > ./flist1.txt;			\
		 ./genlist;						\
		 cat ./ucs_omake ./flist2.txt > ./tmpmake0;		\
		 cat ./tmpmake0 ./trgtmake > ../cutilsrce/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

subsrce3: force_it
	ls ../subsrce3 > ./flist1.txt
	./genlist
	touch ./tmpmake0
	cat ./ssx_omake0 >> ./tmpmake0
	echo "\t\$$(OS3)\n" >> ./tmpmake0
	cat ./tmpmake0 ./ssx_omake1 > ./tmpmake1
	cat ./tmpmake1 ./flist2.txt > ./tmpmake2
	cat ./tmpmake2 ./trgtmake > ./tmpmake3
	cat ./tmpmake3 ./endmake > ../subsrce3/makefile
	rm ./tmpmake? ./flist*

subsrce2: force_it
	ls ../subsrce2 > ./flist1.txt
	./genlist
	touch ./tmpmake0
	cat ./ssx_omake0 >> ./tmpmake0
	echo "\t\$$(OS2)\n" >> ./tmpmake0
	cat ./tmpmake0 ./ssx_omake1 > ./tmpmake1
	cat ./tmpmake1 ./flist2.txt > ./tmpmake2
	cat ./tmpmake2 ./trgtmake > ./tmpmake3
	cat ./tmpmake3 ./endmake > ../subsrce2/makefile
	rm ./tmpmake? ./flist*

presr2: force_it
	if ( test -d ../presr2 ) then					\
		(ls ../presr2 > ./flist1.txt;				\
		 ./genlist;						\
		 touch ./tmpmake0;					\
		 cat ./ssx_omake0 >> ./tmpmake0;			\
		 echo "\t\$$(OR2)\n" >> ./tmpmake0;			\
		 cat ./tmpmake0 ./ssx_omake1 > ./tmpmake1;		\
		 cat ./tmpmake1 ./flist2.txt > ./tmpmake2;		\
		 cat ./tmpmake2 ./trgtmake > ./tmpmake3;		\
		 cat ./tmpmake3 ./endmake > ../presr2/makefile;		\
		 rm ./tmpmake? ./flist*)				\
	fi

presr3: force_it
	if ( test -d ../presr3 ) then					\
		(ls ../presr3 > ./flist1.txt;				\
		 ./genlist;						\
		 touch ./tmpmake0;					\
		 cat ./ssx_omake0 >> ./tmpmake0;			\
		 echo "\t\$$(OR3)\n" >> ./tmpmake0;			\
		 cat ./tmpmake0 ./ssx_omake1 > ./tmpmake1;		\
		 cat ./tmpmake1 ./flist2.txt > ./tmpmake2;		\
		 cat ./tmpmake2 ./trgtmake > ./tmpmake3;		\
		 cat ./tmpmake3 ./endmake > ../presr3/makefile;		\
		 rm ./tmpmake? ./flist*)				\
	fi

press2: force_it
	if ( test -d ../press2 ) then					\
		(ls ../press2 > ./flist1.txt;				\
		 ./genlist;						\
		 touch ./tmpmake0;					\
		 cat ./ssx_omake0 >> ./tmpmake0;			\
		 echo "\t\$$(OS2)\n" >> ./tmpmake0;			\
		 cat ./tmpmake0 ./ssx_omake1 > ./tmpmake1;		\
		 cat ./tmpmake1 ./flist2.txt > ./tmpmake2;		\
		 cat ./tmpmake2 ./trgtmake > ./tmpmake3;		\
		 cat ./tmpmake3 ./endmake > ../press2/makefile;		\
		 rm ./tmpmake? ./flist*)				\
	fi

press3: force_it
	if ( test -d ../press3 ) then					\
		(ls ../press3 > ./flist1.txt;				\
		 ./genlist;						\
		 touch ./tmpmake0;					\
		 cat ./ssx_omake0 >> ./tmpmake0;			\
		 echo "\t\$$(OS3)\n" >> ./tmpmake0;			\
		 cat ./tmpmake0 ./ssx_omake1 > ./tmpmake1;		\
		 cat ./tmpmake1 ./flist2.txt > ./tmpmake2;		\
		 cat ./tmpmake2 ./trgtmake > ./tmpmake3;		\
		 cat ./tmpmake3 ./endmake > ../press3/makefile;		\
		 rm ./tmpmake? ./flist*)				\
	fi

pressx: force_it
	if ( test -d ../pressx ) then					\
		(ls ../pressx > ./flist1.txt;				\
		 ./genlist;						\
		 touch ./tmpmake0;					\
		 cat ./ssx_omake0 >> ./tmpmake0;			\
		 echo "\t../pressx_o\n" >> ./tmpmake0;			\
		 cat ./tmpmake0 ./ssx_omake1 > ./tmpmake1;		\
		 cat ./tmpmake1 ./flist2.txt > ./tmpmake2;		\
		 cat ./tmpmake2 ./trgtmake > ./tmpmake3;		\
		 cat ./tmpmake3 ./endmake > ../pressx/makefile;		\
		 rm ./tmpmake? ./flist*)				\
	fi

custsr2: force_it
	if ( test -d ../custsr2 ) then					\
		(ls ../custsr2 > ./flist1.txt;				\
		 ./genlist;						\
		 touch ./tmpmake0;					\
		 cat ./ssx_omake0 >> ./tmpmake0;			\
		 echo "\t\$$(OR2)\n" >> ./tmpmake0;			\
		 cat ./tmpmake0 ./ssx_omake1 > ./tmpmake1;		\
		 cat ./tmpmake1 ./flist2.txt > ./tmpmake2;		\
		 cat ./tmpmake2 ./trgtmake > ./tmpmake3;		\
		 cat ./tmpmake3 ./endmake > ../custsr2/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

custsr3: force_it
	if ( test -d ../custsr3 ) then					\
		(ls ../custsr3 > ./flist1.txt;				\
		 ./genlist;						\
		 touch ./tmpmake0;					\
		 cat ./ssx_omake0 >> ./tmpmake0;			\
		 echo "\t\$$(OR3)\n" >> ./tmpmake0;			\
		 cat ./tmpmake0 ./ssx_omake1 > ./tmpmake1;		\
		 cat ./tmpmake1 ./flist2.txt > ./tmpmake2;		\
		 cat ./tmpmake2 ./trgtmake > ./tmpmake3;		\
		 cat ./tmpmake3 ./endmake > ../custsr3/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

custss2: force_it
	if ( test -d ../custss2 ) then					\
		(ls ../custss2 > ./flist1.txt;				\
		 ./genlist;						\
		 touch ./tmpmake0;					\
		 cat ./ssx_omake0 >> ./tmpmake0;			\
		 echo "\t\$$(OS2)\n" >> ./tmpmake0;			\
		 cat ./tmpmake0 ./ssx_omake1 > ./tmpmake1;		\
		 cat ./tmpmake1 ./flist2.txt > ./tmpmake2;		\
		 cat ./tmpmake2 ./trgtmake > ./tmpmake3;		\
		 cat ./tmpmake3 ./endmake > ../custss2/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

custss3: force_it
	if ( test -d ../custss3 ) then					\
		(ls ../custss3 > ./flist1.txt;				\
		 ./genlist;						\
		 touch ./tmpmake0;					\
		 cat ./ssx_omake0 >> ./tmpmake0;			\
		 echo "\t\$$(OS3)\n" >> ./tmpmake0;			\
		 cat ./tmpmake0 ./ssx_omake1 > ./tmpmake1;		\
		 cat ./tmpmake1 ./flist2.txt > ./tmpmake2;		\
		 cat ./tmpmake2 ./trgtmake > ./tmpmake3;		\
		 cat ./tmpmake3 ./endmake > ../custss3/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

custssx: force_it
	if ( test -d ../custssx ) then					\
		(ls ../custssx > ./flist1.txt;				\
		 ./genlist;						\
		 touch ./tmpmake0;					\
		 cat ./ssx_omake0 >> ./tmpmake0;			\
		 echo "\t../custssx_o\n" >> ./tmpmake0;			\
		 cat ./tmpmake0 ./ssx_omake1 > ./tmpmake1;		\
		 cat ./tmpmake1 ./flist2.txt > ./tmpmake2;		\
		 cat ./tmpmake2 ./trgtmake > ./tmpmake3;		\
		 cat ./tmpmake3 ./endmake > ../custssx/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

custssx_ora: force_it
	if ( test -d ../custssx_ora ) then				\
		(ls ../custssx_ora > ./flist1.txt;			\
		 ./genlist;						\
		 touch ./tmpmake0;					\
		 cat ./ssx_ora_omake0 >> ./tmpmake0;			\
		 echo "\t../custssx_o\n" >> ./tmpmake0;			\
		 cat ./tmpmake0 ./ssx_ora_omake1 > ./tmpmake1;		\
		 cat ./tmpmake1 ./flist2.txt > ./tmpmake2;		\
		 cat ./tmpmake2 ./trgtmake > ./tmpmake3;		\
		 cat ./tmpmake3 ./endmake > ../custssx_ora/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

subsrce2_o: force_it
	(cd ./subsrce2;				\
	touch ./makegen;			\
	cat ../makegen >> ./makegen;		\
	make -f ./makegen;			\
	touch ./allmake;			\
	cat ../allhdr >> ./allmake;		\
	make;					\
	rm ./makefile;				\
	rm ./makegen;				\
	mv ./allmake ../allmake;		\
	cd ..;					\
	mv ./*ss2 ../subsrce2_o/;		\
	mv ./allmake ../subsrce2_o/makefile)

subsrce3_o: force_it
	(cd ./subsrce3;				\
	touch ./makegen;			\
	cat ../makegen > ./makegen;		\
	make -f ./makegen;			\
	touch ./allmake;			\
	cat ../allhdr >> ./allmake;		\
	make;					\
	rm ./makefile;				\
	rm ./makegen;				\
	mv ./allmake ../allmake;		\
	cd ..;					\
	mv ./*ss3 ../subsrce3_o/;		\
	mv ./allmake ../subsrce3_o/makefile)

subsrcr2_o: force_it
	(cd ./subsrcr2;				\
	touch ./makegen;			\
	cat ../makegen >> ./makegen;		\
	make -f ./makegen;			\
	touch ./allmake;			\
	cat ../allhdr >> ./allmake;		\
	make;					\
	rm ./makefile;				\
	rm ./makegen;				\
	mv ./allmake ../allmake;		\
	cd ..;					\
	mv ./*sr2 ../subsrcr2_o/;		\
	mv ./allmake ../subsrcr2_o/makefile)

subsrcr3_o: force_it
	(cd ./subsrcr3;				\
	touch ./makegen;			\
	cat ../makegen >> ./makegen;		\
	make -f ./makegen;			\
	touch ./allmake;			\
	cat ../allhdr >> ./allmake;		\
	make;					\
	rm ./makefile;				\
	rm ./makegen;				\
	mv ./allmake ../allmake;		\
	cd ..;					\
	mv ./*sr3 ../subsrcr3_o/;		\
	mv ./allmake ../subsrcr3_o/makefile)

pressx_o:
	if ( test -d ../pressx_o ) then					\
		(touch ./tmpmake1;					\
		 cat ./s1o_smake >> ./tmpmake1;				\
		 echo "\nLIBRARY =  \\" >> ./tmpmake1;			\
		 echo liblpacss >> ./tmpmake1;				\
		 echo "\nOBJS= \\" >> ./tmpmake1;			\
		 ls ../pressx_o > ./flist1.txt;				\
		 ./genlist -so -x.o;					\
		 cat ./tmpmake1 ./flist2.txt > ./tmpmake2;		\
		 cat ./tmpmake2 ./s2o_smake > ./tmpmake3;		\
		 mv ./tmpmake3 ../pressx_o/makefile;			\
		 rm ./tmpmake? ./flist*)				\
	fi

custssx_o:
	if ( test -d ../custssx_o ) then				\
	if ( test -f ../custssx_o/*.o) then				\
		(touch ./tmpmake1;					\
		 cat ./s1o_smake >> ./tmpmake1;				\
		 echo "\nLIBRARY =  \\" >> ./tmpmake1;			\
		 echo liblxacss >> ./tmpmake1;				\
		 echo "\nOBJS= \\" >> ./tmpmake1;			\
		 ls ../custssx_o > ./flist1.txt;			\
		 ./genlist -so -x.o;					\
		 cat ./tmpmake1 ./flist2.txt > ./tmpmake2;		\
		 cat ./tmpmake2 ./s2o_smake > ./tmpmake3;		\
		 mv ./tmpmake3 ../custssx_o/makefile;			\
		 rm ./tmpmake? ./flist*)				\
	fi; fi

utilsrce_o: force_it
	ls ../utilsrce_o > ./flist1.txt
	./genlist -so
	ls ../load1 > ./flist3.txt
	./genlist -sa\* a -c3 -p-l -i./flist3.txt -o./flist4.txt
	cat ./uto_emake0 ./flist4.txt > ./tmpmake0
	if (test -d ../custssx_o) then				\
		cat ./lnkmake2 >> ./tmpmake0;			\
	else							\
	 	cat ./lnkmake1 >> ./tmpmake0;			\
	fi
	cat ./tmpmake0 ./path.txt > ./tmpmake2
	cat ./tmpmake2 ./uto_emake1 > ./tmpmake3
	cat ./tmpmake3 ./flist2.txt > ./tmpmake4
	cat ./tmpmake4 ./trgtmake > ../utilsrce_o/makefile
	rm ./flist* ./tmpmake?

cutilsrce_o: force_it
	ls ../cutilsrce_o > ./flist1.txt
	./genlist -so
	cat ./uco_emake0 ./path.txt > ./tmpmake1
	cat ./tmpmake1 ./uco_emake1 > ./tmpmake2
	cat ./tmpmake2 ./flist2.txt > ./tmpmake3
	cat ./tmpmake3 ./trgtmake > ../cutilsrce_o/makefile
	rm ./flist* ./tmpmake?

sourceac: force_it
	if ( test -d ../sourceac ) then					\
		(ls ../sourceac > ./flist1.txt;				\
		 ls ../load1 > ./flist3.txt;				\
		 ./genlist -sa\* a -c3 -p-l -i./flist3.txt -o./flist4.txt;\
		 ./genlist;						\
		 cat ./srx_emake0 ./flist4.txt > ./tmpmake0;		\
		 if (test -d ../custssx_o) then				\
	 		 cat ./lnkmake2 >> ./tmpmake0;			\
		 else							\
	 		 cat ./lnkmake1 >> ./tmpmake0;			\
		 fi;							\
		 cat ./tmpmake0 ./path.txt > ./tmpmake1;		\
		 cat ./tmpmake1 ./srx_emake1 > ./tmpmake2;		\
		 cat ./tmpmake2 ./flist2.txt > ./tmpmake3;		\
		 cat ./tmpmake3 ./trgtmake > ./tmpmake4;		\
		 cat ./tmpmake4 ./endmake > ../sourceac/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

sourcecd: force_it
	if ( test -d ../sourcecd ) then					\
		(ls ../sourcecd > ./flist1.txt;				\
		 ls ../load1 > ./flist3.txt;				\
		 ./genlist -sa\* a -c3 -p-l -i./flist3.txt -o./flist4.txt;\
		 ./genlist;						\
		 cat ./srx_emake0 ./flist4.txt > ./tmpmake0;		\
		 if (test -d ../custssx_o) then				\
	 		 cat ./lnkmake2 >> ./tmpmake0;			\
		 else							\
	 		 cat ./lnkmake1 >> ./tmpmake0;			\
		 fi;							\
		 cat ./tmpmake0 ./path.txt > ./tmpmake1;		\
		 cat ./tmpmake1 ./srx_emake1 > ./tmpmake2;		\
		 cat ./tmpmake2 ./flist2.txt > ./tmpmake3;		\
		 cat ./tmpmake3 ./trgtmake > ./tmpmake4;		\
		 cat ./tmpmake4 ./endmake > ../sourcecd/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

sourcecm: force_it
	if ( test -d ../sourcecm ) then					\
		(ls ../sourcecm > ./flist1.txt;				\
		 ls ../load1 > ./flist3.txt;				\
		 ./genlist -sa\* a -c3 -p-l -i./flist3.txt -o./flist4.txt;\
		 ./genlist;						\
		 cat ./srx_emake0 ./flist4.txt > ./tmpmake0;		\
		 if (test -d ../custssx_o) then				\
	 		 cat ./lnkmake2 >> ./tmpmake0;			\
		 else							\
	 		 cat ./lnkmake1 >> ./tmpmake0;			\
		 fi;							\
		 cat ./tmpmake0 ./path.txt > ./tmpmake1;		\
		 cat ./tmpmake1 ./srx_emake1 > ./tmpmake2;		\
		 cat ./tmpmake2 ./flist2.txt > ./tmpmake3;		\
		 cat ./tmpmake3 ./trgtmake > ./tmpmake4;		\
		 cat ./tmpmake4 ./endmake > ../sourcecm/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

sourceec: force_it
	if ( test -d ../sourceec ) then					\
		(ls ../sourceec > ./flist1.txt;				\
		 ls ../load1 > ./flist3.txt;				\
		 ./genlist -sa\* a -c3 -p-l -i./flist3.txt -o./flist4.txt;\
		 ./genlist;						\
		 cat ./srx_emake0 ./flist4.txt > ./tmpmake0;		\
		 if (test -d ../custssx_o) then				\
	 		 cat ./lnkmake2 >> ./tmpmake0;			\
		 else							\
	 		 cat ./lnkmake1 >> ./tmpmake0;			\
		 fi;							\
		 cat ./tmpmake0 ./path.txt > ./tmpmake1;		\
		 cat ./tmpmake1 ./srx_emake1 > ./tmpmake2;		\
		 cat ./tmpmake2 ./flist2.txt > ./tmpmake3;		\
		 cat ./tmpmake3 ./trgtmake > ./tmpmake4;		\
		 cat ./tmpmake4 ./endmake > ../sourceec/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

sourceeq: force_it
	if ( test -d ../sourceeq ) then					\
		(ls ../sourceeq > ./flist1.txt;				\
		 ls ../load1 > ./flist3.txt;				\
		 ./genlist -sa\* a -c3 -p-l -i./flist3.txt -o./flist4.txt;\
		 ./genlist;						\
		 cat ./srx_emake0 ./flist4.txt > ./tmpmake0;		\
		 if (test -d ../custssx_o) then				\
	 		 cat ./lnkmake2 >> ./tmpmake0;			\
		 else							\
	 		 cat ./lnkmake1 >> ./tmpmake0;			\
		 fi;							\
		 cat ./tmpmake0 ./path.txt > ./tmpmake1;		\
		 cat ./tmpmake1 ./srx_emake1 > ./tmpmake2;		\
		 cat ./tmpmake2 ./flist2.txt > ./tmpmake3;		\
		 cat ./tmpmake3 ./trgtmake > ./tmpmake4;		\
		 cat ./tmpmake4 ./endmake > ../sourceeq/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

sourcefa: force_it
	if ( test -d ../sourcefa ) then					\
		(ls ../sourcefa > ./flist1.txt;				\
		 ls ../load1 > ./flist3.txt;				\
		 ./genlist -sa\* a -c3 -p-l -i./flist3.txt -o./flist4.txt;\
		 ./genlist;						\
		 cat ./srx_emake0 ./flist4.txt > ./tmpmake0;		\
		 if (test -d ../custssx_o) then				\
	 		 cat ./lnkmake2 >> ./tmpmake0;			\
		 else							\
	 		 cat ./lnkmake1 >> ./tmpmake0;			\
		 fi;							\
		 cat ./tmpmake0 ./path.txt > ./tmpmake1;		\
		 cat ./tmpmake1 ./srx_emake1 > ./tmpmake2;		\
		 cat ./tmpmake2 ./flist2.txt > ./tmpmake3;		\
		 cat ./tmpmake3 ./trgtmake > ./tmpmake4;		\
		 cat ./tmpmake4 ./endmake > ../sourcefa/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

sourcefi: force_it
	if ( test -d ../sourcefi ) then					\
		(ls ../sourcefi > ./flist1.txt;				\
		 ls ../load1 > ./flist3.txt;				\
		 ./genlist -sa\* a -c3 -p-l -i./flist3.txt -o./flist4.txt;\
		 ./genlist;						\
		 cat ./srx_emake0 ./flist4.txt > ./tmpmake0;		\
		 if (test -d ../custssx_o) then				\
	 		 cat ./lnkmake2 >> ./tmpmake0;			\
		 else							\
	 		 cat ./lnkmake1 >> ./tmpmake0;			\
		 fi;							\
		 cat ./tmpmake0 ./path.txt > ./tmpmake1;		\
		 cat ./tmpmake1 ./srx_emake1 > ./tmpmake2;		\
		 cat ./tmpmake2 ./flist2.txt > ./tmpmake3;		\
		 cat ./tmpmake3 ./trgtmake > ./tmpmake4;		\
		 cat ./tmpmake4 ./endmake > ../sourcefi/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

sourcemc: force_it
	if ( test -d ../sourcemc ) then					\
		(ls ../sourcemc > ./flist1.txt;				\
		 ls ../load1 > ./flist3.txt;				\
		 ./genlist -sa\* a -c3 -p-l -i./flist3.txt -o./flist4.txt;\
		 ./genlist;						\
		 cat ./srx_emake0 ./flist4.txt > ./tmpmake0;		\
		 if (test -d ../custssx_o) then				\
	 		 cat ./lnkmake2 >> ./tmpmake0;			\
		 else							\
	 		 cat ./lnkmake1 >> ./tmpmake0;			\
		 fi;							\
		 cat ./tmpmake0 ./path.txt > ./tmpmake1;		\
		 cat ./tmpmake1 ./srx_emake1 > ./tmpmake2;		\
		 cat ./tmpmake2 ./flist2.txt > ./tmpmake3;		\
		 cat ./tmpmake3 ./trgtmake > ./tmpmake4;		\
		 cat ./tmpmake4 ./endmake > ../sourcemc/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

sourcepc: force_it
	if ( test -d ../sourcepc ) then					\
		(ls ../sourcepc > ./flist1.txt;				\
		 ls ../load1 > ./flist3.txt;				\
		 ./genlist -sa\* a -c3 -p-l -i./flist3.txt -o./flist4.txt;\
		 ./genlist;						\
		 cat ./srx_emake0 ./flist4.txt > ./tmpmake0;		\
		 if (test -d ../custssx_o) then				\
	 		 cat ./lnkmake2 >> ./tmpmake0;			\
		 else							\
	 		 cat ./lnkmake1 >> ./tmpmake0;			\
		 fi;							\
		 cat ./tmpmake0 ./path.txt > ./tmpmake1;		\
		 cat ./tmpmake1 ./srx_emake1 > ./tmpmake2;		\
		 cat ./tmpmake2 ./flist2.txt > ./tmpmake3;		\
		 cat ./tmpmake3 ./trgtmake > ./tmpmake4;		\
		 cat ./tmpmake4 ./endmake > ../sourcepc/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

sourcepp: force_it
	if ( test -d ../sourcepp ) then					\
		(ls ../sourcepp > ./flist1.txt;				\
		 ls ../load1 > ./flist3.txt;				\
		 ./genlist -sa\* a -c3 -p-l -i./flist3.txt -o./flist4.txt;\
		 ./genlist;						\
		 cat ./srx_emake0 ./flist4.txt > ./tmpmake0;		\
		 if (test -d ../custssx_o) then				\
	 		 cat ./lnkmake2 >> ./tmpmake0;			\
		 else							\
	 		 cat ./lnkmake1 >> ./tmpmake0;			\
		 fi;							\
		 cat ./tmpmake0 ./path.txt > ./tmpmake1;		\
		 cat ./tmpmake1 ./srx_emake1 > ./tmpmake2;		\
		 cat ./tmpmake2 ./flist2.txt > ./tmpmake3;		\
		 cat ./tmpmake3 ./trgtmake > ./tmpmake4;		\
		 cat ./tmpmake4 ./endmake > ../sourcepp/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

sourcewa: force_it
	if ( test -d ../sourcewa ) then					\
		(ls ../sourcewa > ./flist1.txt;				\
		 ls ../load1 > ./flist3.txt;				\
		 ./genlist -sa\* a -c3 -p-l -i./flist3.txt -o./flist4.txt;\
		 ./genlist;						\
		 cat ./srx_emake0 ./flist4.txt > ./tmpmake0;		\
		 if (test -d ../custssx_o) then				\
	 		 cat ./lnkmake2 >> ./tmpmake0;			\
		 else							\
	 		 cat ./lnkmake1 >> ./tmpmake0;			\
		 fi;							\
		 cat ./tmpmake0 ./path.txt > ./tmpmake1;		\
		 cat ./tmpmake1 ./srx_emake1 > ./tmpmake2;		\
		 cat ./tmpmake2 ./flist2.txt > ./tmpmake3;		\
		 cat ./tmpmake3 ./trgtmake > ./tmpmake4;		\
		 cat ./tmpmake4 ./endmake > ../sourcewa/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

source2: force_it
		 ls ../source2 > ./flist1.txt;
		 ls ../load1 > ./flist3.txt;
		 ./genlist -sa\* a -c3 -p-l -i./flist3.txt -o./flist4.txt;
		 ./genlist;
		 cat ./srx_emake0 ./flist4.txt > ./tmpmake0;
		 if (test -d ../custssx_o) then				\
	 		 cat ./lnkmake2 >> ./tmpmake0;			\
		 else							\
	 		 cat ./lnkmake1 >> ./tmpmake0;			\
		 fi;
		 cat ./tmpmake0 ./path.txt > ./tmpmake1;
		 cat ./tmpmake1 ./srx_emake1 > ./tmpmake2;
		 cat ./tmpmake2 ./flist2.txt > ./tmpmake3;
		 cat ./tmpmake3 ./trgtmake > ./tmpmake4;
		 cat ./tmpmake4 ./endmake > ../source2/makefile;
		 rm ./tmpmake? ./flist*

custsubs: force_it
	if ( test -d ../custsubs ) then					\
	if ( test -f ../custsubs/makefile) then				\
		echo "\nCustom Subroutine makefile exists";		\
	else								\
		(./testenv noload;					\
		 ls ../custsubs > ./flist1.txt;				\
		 ./genlist;						\
		 rm -f ./tmpmake0; touch ./tmpmake0;			\
		 cat ./csubsmake0 >> ./tmpmake0;			\
		 echo "OBJ=\$$(OCUSTSUBS)\n" >> ./tmpmake0;		\
		 echo "INCPATH= \\" >> ./tmpmake0;			\
		 if (test ! -d ../include1) then			\
		 	cat ./pathinc.txt >> ./tmpmake0;		\
		 else 							\
			echo "..\n" >> ./tmpmake0;			\
		 fi;							\
		 cat ./tmpmake0 ./csubsmake1 > ./tmpmake1;		\
		 cat ./tmpmake1 ./flist2.txt > ./tmpmake2;		\
		 cat ./csubsmakex ./flist2.txt > ./tmpmakex;		\
		 make -f ./tmpmakex;					\
		 cat ./tmpmake2 ./trgtmake > ./tmpmake3;		\
		 mv ./tmpmake3 ../custsubs/makefile;			\
		 rm ./tmpmake? ./flist*)				\
	fi;								\
	fi

presubs: force_it
	if ( test -d ../presubs ) then					\
	if ( test -f ../presubs/makefile) then				\
		echo "\nPrerel Subroutine makefile exists";		\
	else								\
		(./testenv noload;					\
		 ls ../presubs > ./flist1.txt;				\
		 ./genlist;						\
		 rm -f ./tmpmake0; touch ./tmpmake0;			\
		 cat ./csubsmake0 >> ./tmpmake0;			\
		 echo "OBJ=\$$(OPRESUBS)\n" >> ./tmpmake0;		\
		 echo "INCPATH= \\" >> ./tmpmake0;			\
		 if (test ! -d ../include1) then			\
		 	cat ./pathinc.txt >> ./tmpmake0;		\
		 else 							\
			echo "..\n" >> ./tmpmake0;			\
		 fi;							\
		 cat ./tmpmake0 ./csubsmake1 > ./tmpmake1;		\
		 cat ./tmpmake1 ./flist2.txt > ./tmpmake2;		\
		 cat ./csubsmakex ./flist2.txt > ./tmpmakex;		\
		 make -f ./tmpmakex;					\
		 cat ./tmpmake2 ./trgtmake > ./tmpmake3;		\
		 mv ./tmpmake3 ../presubs/makefile;			\
		 rm ./tmpmake? ./flist*)				\
	fi;								\
	fi

custsrce: force_it
	if ( test -d ../custsrce ) then					\
	if ( test -f ../custsrce/makefile) then				\
		echo "\nCustom Source makefile exists";			\
	else								\
		(./testenv;						\
		 ls ../custsrce > ./flist1.txt;				\
		 if (test -d ../load1) then				\
		 	ls ../load1 > ./flist3.txt;			\
		 else							\
			ls `cat ./pathld.txt`/load1 > ./flist3.txt;	\
		 fi;							\
		 ./genlist -sa\* a -c3 -p-l -i./flist3.txt -o./flist4.txt;\
		 ./genlist;						\
		 cat ./csrcmake0 ./flist4.txt > ./tmpmake0;		\
		 if (test -d ../custssx_o) then				\
		 	cat ./lnkmake2 >> ./tmpmake0;			\
		 else							\
		 	cat ./lnkmake1 >> ./tmpmake0;			\
		 fi;							\
		 if (test -d ../include1) then				\
		 	cat ./tmpmake0 ./path.txt > ./tmpmake1;		\
		 else							\
			cat ./tmpmake0 ./pathinc.txt > ./tmpmake1;	\
		 fi;							\
		 echo "\nLDPATH= \\" >> ./tmpmake1;			\
		 if (test -d ../load1) then				\
		 	cat ./path.txt >> ./tmpmake1;			\
		 else							\
			cat ./pathld.txt >> ./tmpmake1;			\
		 fi;							\
		 echo "\nDEVPATH= \\" >> ./tmpmake1;			\
		 cat ./path.txt >> ./tmpmake1;				\
		 cat ./tmpmake1 ./csrcmake1 > ./tmpmake2;		\
		 if ( test -d ../custsubs_o ) then			\
			(echo "\nCUST_EXT_OBJ= \\" >> ./tmpmake2;	\
			 ls ../custsubs_o > ./flist5.txt;		\
		 ./genlist -i./flist5.txt -o./flist6.txt -so -p\$$\(C\) -x.o;\
			 cat ./flist6.txt >> ./tmpmake2);		\
		 fi;							\
		 if ( test -d ../presubs_o ) then			\
			(echo "\nPRE_EXT_OBJ= \\" >> ./tmpmake2;	\
			 ls ../presubs_o > ./flist5.txt;		\
		 ./genlist -i./flist5.txt -o./flist6.txt -so -p\$$\(P\) -x.o;\
			 cat ./flist6.txt >> ./tmpmake2);		\
		 fi;							\
		 cat ./tmpmake2 ./csrcmake2 > ./tmpmake3;		\
		 cat ./tmpmake3 ./flist2.txt > ./tmpmake4;		\
		 cat ./csrcmakex ./flist2.txt > ./tmpmakex;		\
		 make -f ./tmpmakex;					\
		 cat ./tmpmake4 ./trgtmake > ./tmpmake5;		\
		 mv ./tmpmake5 ../custsrce/makefile;			\
		 rm ./tmpmake? ./flist*)				\
	fi;								\
	fi

presrce: force_it
	if ( test -d ../presrce ) then					\
	if ( test -f ../presrce/makefile) then				\
		echo "\nPrerel Source makefile exists";			\
	else								\
		(./testenv;						\
		 ls ../presrce > ./flist1.txt;				\
		 if (test -d ../load1) then				\
		 	ls ../load1 > ./flist3.txt;			\
		 else							\
			ls `cat ./pathld.txt`/load1 > ./flist3.txt;	\
		 fi;							\
		 ./genlist -sa\* a -c3 -p-l -i./flist3.txt -o./flist4.txt;\
		 ./genlist;						\
		 cat ./csrcmake0 ./flist4.txt > ./tmpmake0;		\
		 if (test -d ../custssx_o) then				\
		 	cat ./lnkmake2 >> ./tmpmake0;			\
		 else							\
		 	cat ./lnkmake1 >> ./tmpmake0;			\
		 fi;							\
		 if (test -d ../include1) then				\
		 	cat ./tmpmake0 ./path.txt > ./tmpmake1;		\
		 else							\
			cat ./tmpmake0 ./pathinc.txt > ./tmpmake1;	\
		 fi;							\
		 echo "\nLDPATH= \\" >> ./tmpmake1;			\
		 if (test -d ../load1) then				\
		 	cat ./path.txt >> ./tmpmake1;			\
		 else							\
			cat ./pathld.txt >> ./tmpmake1;			\
		 fi;							\
		 echo "\nDEVPATH= \\" >> ./tmpmake1;			\
		 cat ./path.txt >> ./tmpmake1;				\
		 cat ./tmpmake1 ./csrcmake1 > ./tmpmake2;		\
		 if ( test -d ../custsubs_o ) then			\
			(echo "\nCUST_EXT_OBJ= \\" >> ./tmpmake2;	\
			 ls ../custsubs_o > ./flist5.txt;		\
		 ./genlist -i./flist5.txt -o./flist6.txt -so -p\$$\(C\) -x.o;\
			 cat ./flist6.txt >> ./tmpmake2);		\
		 fi;							\
		 if ( test -d ../presubs_o ) then			\
			(echo "\nPRE_EXT_OBJ= \\" >> ./tmpmake2;	\
			 ls ../presubs_o > ./flist5.txt;		\
		 ./genlist -i./flist5.txt -o./flist6.txt -so -p\$$\(P\) -x.o;\
			 cat ./flist6.txt >> ./tmpmake2);		\
		 fi;							\
		 cat ./tmpmake2 ./csrcmake2 > ./tmpmake3;		\
		 cat ./tmpmake3 ./flist2.txt > ./tmpmake4;		\
		 cat ./csrcmakex ./flist2.txt > ./tmpmakex;		\
		 make -f ./tmpmakex;					\
		 cat ./tmpmake4 ./trgtmake > ./tmpmake5;		\
		 mv ./tmpmake5 ../presrce/makefile;			\
		 rm ./tmpmake? ./flist*)				\
	fi;								\
	fi

presrcx: force_it
	if ( test -d ../presrcx ) then					\
		(ls ../presrcx > ./flist1.txt;				\
		 ls ../load1 > ./flist3.txt;				\
	 ./genlist -sa\* a -c3 -p-l -i./flist3.txt -o./flist4.txt;	\
		 ./genlist;						\
		 cat ./srx_emake0 ./flist4.txt > ./tmpmake0;		\
		 if (test -d ../custssx_o) then				\
	 		 cat ./lnkmake2 >> ./tmpmake0;			\
		 else							\
	 		 cat ./lnkmake1 >> ./tmpmake0;			\
		 fi;							\
		 cat ./tmpmake0 ./path.txt > ./tmpmake1;		\
		 cat ./tmpmake1 ./srx_emake1 > ./tmpmake2;		\
		 cat ./tmpmake2 ./flist2.txt > ./tmpmake3;		\
		 cat ./tmpmake3 ./trgtmake > ./tmpmake4;		\
		 cat ./tmpmake4 ./endmake > ../presrcx/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

custsrcx: force_it
	if ( test -d ../custsrcx ) then					\
		(ls ../custsrcx > ./flist1.txt;				\
		 ls ../load1 > ./flist3.txt;				\
	 ./genlist -sa\* a -c3 -p-l -i./flist3.txt -o./flist4.txt;	\
		 ./genlist;						\
		 cat ./srx_emake0 ./flist4.txt > ./tmpmake0;		\
		 if (test -d ../custssx_o) then				\
	 		 cat ./lnkmake2 >> ./tmpmake0;			\
		 else							\
	 		 cat ./lnkmake1 >> ./tmpmake0;			\
		 fi;							\
		 cat ./tmpmake0 ./path.txt > ./tmpmake1;		\
		 cat ./tmpmake1 ./srx_emake1 > ./tmpmake2;		\
		 cat ./tmpmake2 ./flist2.txt > ./tmpmake3;		\
		 cat ./tmpmake3 ./trgtmake > ./tmpmake4;		\
		 cat ./tmpmake4 ./endmake > ../custsrcx/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

custsrcx_ora: force_it
	if ( test -d ../custsrcx_ora ) then				\
		(ls ../custsrcx_ora > ./flist1.txt;			\
		 ls ../load1 > ./flist3.txt;				\
	 ./genlist -sa\* a -c3 -p-l -i./flist3.txt -o./flist4.txt;	\
		 ./genlist;						\
		 cat ./srx_ora_emake0 ./flist4.txt > ./tmpmake0;	\
		 if (test -d ../custssx_o) then				\
	 		 cat ./lnkmake2 >> ./tmpmake0;			\
		 else							\
	 		 cat ./lnkmake1 >> ./tmpmake0;			\
		 fi;							\
		 cat ./tmpmake0 ./path.txt > ./tmpmake1;		\
		 cat ./tmpmake1 ./srx_ora_emake1 > ./tmpmake2;		\
		 cat ./tmpmake2 ./flist2.txt > ./tmpmake3;		\
		 cat ./tmpmake3 ./trgtmake > ./tmpmake4;		\
		 cat ./tmpmake4 ./endmake > ../custsrcx_ora/makefile;	\
		 rm ./tmpmake? ./flist*)				\
	fi

subobj_o: force_it
	if ( test -d ../subobj_o ) then					\
		(ls ../subsrcr2_o > ./flist1.txt;			\
		 ls ../subsrcr3_o > ./flist2.txt;			\
		 ls ../subsrce2_o > ./flist3.txt;			\
		 ls ../subsrce3_o > ./flist4.txt;			\
		 ./genlist -i./flist1.txt -o./flist5.txt -so -p\$$\(A\) -x.o;\
		 ./genlist -i./flist2.txt -o./flist6.txt -so -p\$$\(B\) -x.o;\
		 ./genlist -i./flist3.txt -o./flist7.txt -so -p\$$\(C\) -x.o;\
		 ./genlist -i./flist4.txt -o./flist8.txt -so -p\$$\(D\) -x.o;\
		 touch ./tmpmake0;					\
		 cat ./asubsmake0 >> ./tmpmake0;			\
		 echo "\nOBJA= \\" >> ./tmpmake0;			\
		 cat ./flist5.txt >> ./tmpmake0;			\
		 echo "\nOBJB= \\" >> ./tmpmake0;			\
		 cat ./flist6.txt >> ./tmpmake0;			\
		 echo "\nOBJC= \\" >> ./tmpmake0;			\
		 cat ./flist7.txt >> ./tmpmake0;			\
		 echo "\nOBJD= \\" >> ./tmpmake0;			\
		 cat flist8.txt >> ./tmpmake0;				\
		 if (test -d ../pressx_o) then				\
		 	ls ../pressx_o > ./flist9.txt;			\
		 ./genlist -i./flist9.txt -o./flista.txt -so -p\$$\(P\) -x.o;\
		 	if (test -s ./flista.txt) then			\
		 		echo "\nOBJP= \\" >> ./tmpmake0;	\
		 		cat flista.txt >> ./tmpmake0;		\
		 	else						\
		 		echo "\nOBJP= " >> ./tmpmake0;		\
		 	fi;						\
		 else							\
		 	echo "\nOBJP= " >> ./tmpmake0;			\
		 fi;							\
		 if (test -d ../custssx_o) then				\
		 	ls ../custssx_o > ./flistb.txt;			\
		 ./genlist -i./flistb.txt -o./flistc.txt -so -p\$$\(X\) -x.o;\
		 	if (test -s ./flistc.txt) then			\
		 		echo "\nOBJX= \\" >> ./tmpmake0;	\
		 		cat flistc.txt >> ./tmpmake0;		\
		 	else						\
		 		echo "\nOBJX= " >> ./tmpmake0;		\
		 	fi;						\
		 else							\
		 	echo "\nOBJX= " >> ./tmpmake0;			\
		 fi;							\
		 cat ./tmpmake0 ./asubsmake1 > ../subobj_o/makefile;	\
		 rm ./flist* ./tmpmake?)				\
	fi

fixes: force_it
	if ( test -d ../fixes ) then					\
		(ls ../fixes > ./flist1.txt;				\
		 ls ../load1 > ./flist3.txt;				\
		 ./genlist -sa\* a -c3 -p-l -i./flist3.txt -o./flist4.txt;\
		 ./genlist;						\
		 cat ./fxs_emake0 ./flist4.txt > ./tmpmake0;		\
		 if (test -d ../custssx_o) then				\
	 		cat ./lnkmake2 >> ./tmpmake0;			\
		 else							\
	 		cat ./lnkmake1 >> ./tmpmake0;			\
		 fi;							\
		 cat ./tmpmake0 ./path.txt > ./tmpmake1;		\
		 cat ./tmpmake1 ./fxs_emake1 > ./tmpmake2;		\
		 cat ./tmpmake2 ./flist2.txt > ./tmpmake3;		\
		 cat ./tmpmake3 ./endmake > ../fixes/makefile;		\
		 rm ./tmpmake? ./flist*)				\
	fi

genlist: force_it
	cc ./genlist.c -o ./genlist

srcdiff: force_it
	cc -Aa ./srcdiff.c -lcurses -o ./srcdiff

devel: force_it
	cd ..; pwd > ./makeutil/path.txt; cd ./makeutil
	ls ../load1 > ./flist3.txt
	./genlist -sa\* a -c3 -p-l -i./flist3.txt -o./flist4.txt
	cat ./makedev0 ./path.txt > ./tmpmake0
	cat ./tmpmake0 ./makedev1 > ./tmpmake1
	cat ./tmpmake1 ./flist4.txt > ./tmpmake2
	if (test -d ../custssx_o) then			\
	 	cat ./lnkmake2 >> ./tmpmake2;		\
	else						\
	 	cat ./lnkmake1 >> ./tmpmake2;		\
	fi
	cat ./tmpmake2 ./makedev2 > ./makedev.sam
	rm ./tmpmake? ./flist*
	
force_it:

