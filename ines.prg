#define XHB_BITOP // Habilita das operações | & ^^

#include "xhb.ch"
#include "common.ch"
#include "hbclass.ch"

CREATE CLASS iNESFileHeader
	var Magic    INIT "" //uint32  // iNES magic number
	var NumPRG   INIT 0  // byte    // number of PRG-ROM banks (16KB each)
	var NumCHR   INIT 0  //byte    // number of CHR-ROM banks (8KB each)
	var Control1 INIT 0  //byte    // control bits
	var Control2 INIT 0  //byte    // control bits
	var NumRAM   INIT 0  // byte    // PRG-RAM size (x 8KB)
ENDCLASS

METHOD New() CLASS iNESFileHeader
RETURN Self

function LoadNESFile(path)
// open file
file=fopen(path)

? "Abrindo: ",path

if ferror()#0
   ? "Erro abrindo arquivo: ",ferror()
   return 'Erro abrindo arquivos'
end if

? "Arquivo aberto, lendo header..."

header := iNESFileHeader():new()

xHeader=space(16)

fread(file,@xHeader,16)

header:Magic   :=left(xHeader,4)
header:NumPrg  :=asc(xHeader[5])
header:NumCHR  :=asc(xHeader[6])
header:Control1:=asc(xHeader[7])
header:Control2:=asc(xHeader[8])
header:NumRAM  :=asc(xHeader[9])

// verify header magic number
if header:Magic#'NES'+chr(0x1a)
   ? "Arquivo invalido."
	return 'Invalida format'
end if

// mapper type
mapper1 := (header:Control1 >> 4)
mapper2 := (header:Control2 >> 4)
mapper  := (mapper1 | (mapper2 << 4))

? "Mapper: ",mapper

// mirroring type
mirror1 := (header:Control1 & 1)
mirror2 := ((header:Control1 >> 3) & 1)
mirror  := (mirror1 | (mirror2 << 1))

? "Mirror: ",mirror

// battery-backed RAM
battery := ((header:Control1 >> 1) & 1)

// read trainer if present (unused)
if (header:Control1 & 4) == 4
   ? "Carregando trainer"
	vtrainer := repl(chr(0),512)
	trainer := aAloc(0,512)
	
	fread(file,@vtrainer,512)
	
	for i=1 to len(vtrainer)
	   trainer[i]=asc(substr(vtrainer,i,1))
	next
end if

? "Numero de PRG: ",header:NumPrg

// read prg-rom bank(s)
//make([]byte, int(header.NumPRG)*16384)

? " -> Alocando banco: ",header:NumPrg*16384

vprg=repl(chr(0),header:NumPrg*16384)

prg=aAloc(0,header:NumPrg*16384)

fread(file,@vprg,len(vprg))

for i=1 to len(vprg)
   prg[i]=asc(substr(vprg,i,1))
next

? "Numero de CHR/Bank: ",header:NumCHR
// read chr-rom bank(s)
//make([]byte, int(header.NumCHR)*8192)

? " -> Alocando banco: ",header:NumCHR*8192

vchr=repl(chr(0),header:NumCHR*8192)

chr=aAloc(0,header:NumCHR*8192)

fread(file,@vchr,len(vchr))

for i=1 to len(vchr)
   chr[i]=asc(substr(vchr,i,1))
next

// provide chr-rom/ram if not in file
if header:NumCHR == 0 
	//chr = repl(chr(0),8192)
	chr = aAloc(0,8192)
end if

// success
return Cartridge():New(prg, chr, mapper, mirror, battery)

