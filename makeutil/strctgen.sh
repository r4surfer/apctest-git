#!/usr/bin/csh
# Create structures file in ./struct
#

set relid = ''
set templs = ls

set resp = 99
while ($resp == 99) 
	echo ' '
	echo 'Enter structure identifier'
	echo ' '
	set opresp = $<
	if ( ! ($opresp == '' )) then
		set relid = $opresp
		set resp = 0
	endif
end

if ( ! -d ./struct ) then
	mkdir ./struct
endif

cd ./struct
if ( -f ./$relid.idx ) then
	rm ./$relid.*
endif

if ( -f ./$relid.dat ) then
	rm ./$relid.*
endif

touch ./$relid.stx
rm ./*.stx
touch ./$relid.str
rm ./*.str
touch ./$relid.str

cd ../

if ( -d ../subsrcr3 ) then
	./strctdir.sh subsrcr3
	./strctmrg.sh $relid
endif

if ( -d ../subsrce3 ) then
	./strctdir.sh subsrce3
	./strctmrg.sh $relid
endif

if ( -d ../subsrcr2 ) then
	./strctdir.sh subsrcr2
	./strctmrg.sh $relid
endif

if ( -d ../subsrce2 ) then
	./strctdir.sh subsrce2
	./strctmrg.sh $relid
endif

if ( -d ../presr3 ) then
	./strctdir.sh presr3
	./strctmrg.sh $relid
endif

if ( -d ../press3 ) then
	./strctdir.sh press3
	./strctmrg.sh $relid
endif

if ( -d ../presr2 ) then
	./strctdir.sh presr2
	./strctmrg.sh $relid
endif

if ( -d ../press2 ) then
	./strctdir.sh press2
	./strctmrg.sh $relid
endif

if ( -d ../custsr3 ) then
	./strctdir.sh custsr3
	./strctmrg.sh $relid
endif

if ( -d ../custss3 ) then
	./strctdir.sh custss3
	./strctmrg.sh $relid
endif

if ( -d ../custsr2 ) then
	./strctdir.sh custsr2
	./strctmrg.sh $relid
endif

if ( -d ../custss2 ) then
	./strctdir.sh custss2
	./strctmrg.sh $relid
endif

if ( -d ../utilsrce ) then
	./strctdir.sh utilsrce
	./strctmrg.sh $relid
endif

if ( -d ../sourceac ) then
	./strctdir.sh sourceac
	./strctmrg.sh $relid
endif

if ( -d ../sourcecd ) then
	./strctdir.sh sourcecd
	./strctmrg.sh $relid
endif

if ( -d ../sourcecm ) then
	./strctdir.sh sourcecm
	./strctmrg.sh $relid
endif

if ( -d ../sourcemc ) then
	./strctdir.sh sourcemc
	./strctmrg.sh $relid
endif

if ( -d ../sourcewa ) then
	./strctdir.sh sourcewa
	./strctmrg.sh $relid
endif

if ( -d ../source2 ) then
	./strctdir.sh source2
	./strctmrg.sh $relid
endif

cd ./struct

sort -o./$relid.dat -u ./$relid.str
rm ./$relid.str

if ( -f ./proto.idx ) then
	cp ./proto.idx ./$relid.idx
	dcheck -b ./$relid
endif

cd ../
