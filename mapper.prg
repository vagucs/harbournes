#define XHB_BITOP // Habilita das operações | & ^^

#include "xhb.ch"
#include "common.ch"
#include "hbclass.ch"

CREATE CLASS Mapper
	VAR Read(address uint16) byte
	VAR Write(address uint16, value byte)
	VAR Step()
	VAR Save(encoder *gob.Encoder) error
	VAR Load(decoder *gob.Decoder) error
END CLASS

METHO New(console)
	cartridge := console:Cartridge
	do case
	   case cartridge:Mapper=0
		   return NewMapper2(cartridge)
	   /*
	   case cartridge:Mapper=1
		   return NewMapper1(cartridge)
	   case cartridge:Mapper=2
		   return NewMapper2(cartridge)
	   case cartridge:Mapper=3
		   return NewMapper3(cartridge)
	   case cartridge:Mapper=4
   		return NewMapper4(console, cartridge)
	   case cartridge:Mapper=7
   		return NewMapper7(cartridge)
	   case cartridge:Mapper=225
   		return NewMapper225(cartridge)
      */
	end if

METHOD Read(address) CLASS Mapper

METHOD Write(address,value) CLASS Mapper

METHOD Step() CLASS Mapper

METHOD Save() CLASS Mapper

METHOD Load() CLASS Mapper