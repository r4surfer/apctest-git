#!/usr/bin/csh
# Create structures file in ./struct
#

set structdir = $1
set templs = ls

if ($structdir == '') then
	echo 'directory not set'
	exit
endif

if ( ! -d ./struct ) then
	mkdir ./struct
endif

cd ../$structdir
$templs *.bas > ../makeutil/flist0.txt
cut -f1 -d. ../makeutil/flist0.txt > ../makeutil/flist1.txt

foreach file (`cat ../makeutil/flist1.txt`)
	../b_cgen -x ./$file
	sort -o ../makeutil/struct/$file.stx -u ./$file.str
	rm ./$file.c
	rm ./$file.h
	rm ./$file.str
end

cd ../makeutil
rm ./flist*
