#include "common.ch"
#include "hbclass.ch"

CREATE CLASS Cartridge
	VAR PRG     init "" // PRG-ROM banks
	VAR CHR     init "" // CHR-ROM banks
	VAR SRAM    init "" // Save RAM
	VAR Mapper  init 0   // mapper type
	VAR Mirror  init 0   // mirroring mode
	VAR Battery init 0   // battery present
	METHOD New(prg, chr , mapper, mirror, battery)
END CLASS

METHOD New(prg, chr , mapper, mirror, battery) CLASS Cartridge
	::SRAM   :=aAloc(0,0x2000) //repl(chr(0),0x2000)
	::PRG    :=prg
	::CHR    :=chr
	::Mapper :=mapper
	::Mirror :=mirror
	::Battery:=battery
Return Self

