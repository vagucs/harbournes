#define XHB_BITOP // Habilita das operações | & ^^

#include "xhb.ch"
#include "common.ch"
#include "hbclass.ch"

//#define SHOWLOG
   
CREATE CLASS Mapper7
	VAR Cartridge
	VAR prgBank
	VAR prgBank1
	VAR prgBank2
	
	METHOD New(cartridge)
	METHOD Step()
	METHOD Read(address)
	METHOD Write(address, value)
END CLASS

METHOD New(cartridge) CLASS Mapper7
   ::Cartridge := cartridge
	::prgBank   := 0 //(len(cartridge:PRG) / 0x4000)
	::prgBank1  := 0
	::prgBank2  := ::prgBank - 1
	return Self

RETURN Self

METHOD  Step() CLASS Mapper7

RETURN

METHOD Read(address) CLASS Mapper7
	do case
	   case address < 0x2000
	   	return ::Cartridge:CHR[address+1]
	   case address >= 0x8000
		   index := ::prgBank * 0x8000 + int(address-0x8000)
		   return ::Cartridge:PRG[index+1]
	   case address >= 0x6000
   		index := (address) - 0x6000
		   return (::Cartridge:SRAM[index+1])
	   default
	   	? "unhandled mapper7 read at address: ", address
	end case
	return 0

METHOD Write(address, value) CLASS Mapper7
	do case
   	case address < 0x2000
	   	::Cartridge:CHR[address+1] = value
   	case address >= 0x8000
	   	::prgBank = int((value & 7))
	   	do case
	   	   case (value & 0x10)=0x00
   	   		::Cartridge:Mirror = 2 //MirrorSingle0
	   	   case (value & 0x10)=0x00
   	   		::Cartridge:Mirror = 3 //MirrorSingle1
	   	end case
	   	//::prgBank1 = int(value) % ::prgBanks
   	case address >= 0x6000
   	   //?? " - Put SRAM"
	   	index := int(address) - 0x6000
		   ::Cartridge:SRAM[index+1] = value

	   default
	   	? "unhandled mapper7 write at address: ",address
	end case
