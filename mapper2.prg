#define XHB_BITOP // Habilita das operações | & ^^

#include "xhb.ch"
#include "common.ch"
#include "hbclass.ch"

//#define SHOWLOG
   
CREATE CLASS Mapper2
	VAR Cartridge
	VAR prgBank
	VAR prgBank1
	VAR prgBank2
	
	METHOD New(cartridge)
	METHOD Step()
	METHOD Read(address)
	METHOD Write(address, value)
END CLASS

METHOD New(cartridge) CLASS Mapper2 // By ref
   ::Cartridge := cartridge
	::prgBank   := (len(cartridge:PRG) / 0x4000)
	::prgBank1  := 0
	::prgBank2  := ::prgBank - 1
	return Self

METHOD  Step() CLASS Mapper2

RETURN

METHOD Read(address) CLASS Mapper2
   //? "Mapper:Read: ",hb_numtohex(address,4)
   #ifdef SHOWLOG
   clog("LENDO DENTRO DO MAPPER: ",address)
   #endif
	do case
	   case address < 0x2000
	      #ifdef SHOWLOG
	      clog("<2000")
	      #endif
	      //?? " - > ",hb_numtohex(asc(::Cartridge:CHR[address+1]),2)
		   return ::Cartridge:CHR[address+1]
	   case address >= 0xC000
	      #ifdef SHOWLOG
	      clog(">=C000")
	      #endif
		   index := ::prgBank2 * 0x4000 + (address-0xC000)
		   //?? " - Index: ",hb_numtohex(index)+" - V: ",hb_numtohex((::Cartridge:PRG[index+1]),2)
		   return ::Cartridge:PRG[index+1]
	   case address >= 0x8000
	      #ifdef SHOWLOG
	      clog(">8000")
	      #endif
		   index := ::prgBank1 * 0x4000 + (address-0x8000)
		   #ifdef SHOWLOG
		   clog(index)
		   #endif
		   //?? " - Index: ",hb_numtohex(index)+" - V: ",hb_numtohex((::Cartridge:PRG[index+1]),2)
		   #ifdef SHOWLOG
		   clog((::Cartridge:PRG[index+1]))
		   #endif
		   return ::Cartridge:PRG[index+1]
	   case address >= 0x6000
	      #ifdef SHOWLOG
	      clog(">=6000")
	      #endif
   		index := (address) - 0x6000
		   //?? " - Index: ",hb_numtohex(index)+" - V: ",hb_numtohex(asc(::Cartridge:SRAM[index+1]),2)
		   #ifdef SHOWLOG
		   clog("SRAM: ",index," = ",(::Cartridge:SRAM[index+1]))
		   #endif
		   return (::Cartridge:SRAM[index+1])
	   default
		   ? "unhandled mapper2 read at address: ", address
	end case
	return 0

METHOD Write(address, value) CLASS Mapper2
   //? "Mapper:Write: ",Adress," = ",value
	do case
   	case address < 0x2000
   	   //?? " - Put IN CHR"
	   	::Cartridge:CHR[address+1] = value
   	case address >= 0x8000
   	   //?? " - Put IN PRGBANCK1"
	   	::prgBank1 = int(value) % ::prgBank
   	case address >= 0x6000
   	   //?? " - Put SRAM"
	   	index := int(address) - 0x6000
		   ::Cartridge:SRAM[index+1] = value
	   default
		   ? "unhandled mapper2 write at address: ", address
	end case

