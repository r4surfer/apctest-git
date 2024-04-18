#!/usr/bin/csh
# Create structures file in ./struct
#

set relid = $1
set templs = ls

if ($relid == '') then
	echo 'release id not set'
	exit
endif

cd ./struct

$templs ./*.stx > ../flist0.txt

foreach file (`cat ../flist0.txt`)
	cat $file >> ./$relid.str
	rm $file
end

rm ../flist0.txt

cd ../
