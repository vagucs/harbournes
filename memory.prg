#define XHB_BITOP // Habilita das operações | & ^^

#include "xhb.ch"
#include "common.ch"
#include "hbclass.ch"

//#define SHOWLOG

CREATE CLASS Memory
   VAR console
   
   METHOD New(console)
   METHOD Read(address)
   METHOD Write(address, value)
END CLASS
// CPU Memory Map

METHOD New(console) CLASS Memory
   ::console=console
return Self

METHOD Read(address) CLASS Memory
   //address=(address & 0xFFFF)
   #ifdef SHOWLOG
   /*
   ppi := 2

   while ( !Empty(ProcName(ppi)) )
      clog(ProcName(ppi)+"("+str(ProcLine(ppi))+") - "+procfile(ppi))
      ppi++
   end do
   */
   //clog("IN.MEM: ",address)
   #endif
	do case
	   case address < 0x2000
         #ifdef SHOWLOG
	   	X=::console:RAM[(address % 0x0800)+1]
         clog("MEM.Read: ",address," = ",x)
		   return x
         #endif
		   return ::console:RAM[(address % 0x0800)+1]
	   case address < 0x4000
	      //alert("READ MEM:"+hb_numtohex(0x2000 + address % 8))
         #ifdef SHOWLOG
	      X=::console:PPU:readRegister(0x2000 + address % 8)
         clog("MEM.Read: ",address," - Value: ",x)
		   return x
         #endif
		   return ::console:PPU:readRegister(0x2000 + address % 8)
	   case address == 0x4014
         #ifdef SHOWLOG
	   	X=::console:PPU:readRegister(address)
         clog("MEM.Read: ",address," - Value: ",x)
		   return x
         #endif
		   return ::console:PPU:readRegister(address)
	   case address == 0x4015
         #ifdef SHOWLOG
	   	X=::console:APU:readRegister(address)
         clog("MEM.Read: ",address," - Value: ",x)
		   return x
         #endif
		   return ::console:APU:readRegister(address)
	   case address == 0x4016
         #ifdef SHOWLOG
	   	X=::console:Controller1:Read()
         clog("MEM.Read: ",address," - Value: ",x)
		   return x
         #endif
		   return ::console:Controller1:Read()
	   case address == 0x4017
         #ifdef SHOWLOG
	   	X=::console:Controller2:Read()
         clog("MEM.Read: ",address," - Value: ",x)
		   return x
         #endif
		   return ::console:Controller2:Read()
	   case address < 0x6000
	   	// TODO: I/O registers
	   case address >= 0x6000
	      #ifdef SHOWLOG
	      v=address
	      x=::console:Mapper:Read(address)
	      clog("Lendo do MAPPER: ",v," = ",x)
	   	return x
	      #endif
	   	return ::console:Mapper:Read(address)
	   default
	   	? "unhandled cpu memory read at address: ", address
	end case
	return 0

METHOD Write(address, value) CLASS Memory
   //? " -> Memory:Write: ",hb_numtohex(address,4),' -> ',hb_numtohex(value,2),'            '
   #ifdef SHOWLOG
   clog("MEM.Write: ",address," = ",value)
   #endif
	do case
	   case address < 0x2000
	   	//::console:RAM[(address % 0x0800)+1] = value
	   	::console:RAM[address+1] = value
	   case address < 0x4000
	   	::console:PPU:writeRegister(0x2000 + address % 8, value)
	   case address < 0x4014
	   	::console:APU:writeRegister(address, value)
	   case address == 0x4014
	   	::console:PPU:writeRegister(address, value)
	   case address == 0x4015
	   	::console:APU:writeRegister(address, value)
	   case address == 0x4016
	   	::console:Controller1:Write(value)
	   	::console:Controller2:Write(value)
	   case address == 0x4017
	   	::console:APU:writeRegister(address, value)
	   case address < 0x6000
	   	// TODO: I/O registers
	   case address >= 0x6000
	   	::console:Mapper:Write(address, value)
	   default
	   	? "unhandled cpu memory write at address: ", address
	end case


// PPU Memory Map

CREATE CLASS ppuMemory 
	VAR console 
	METHOD New(console)
	METHOD Read(address)
	METHOD Write(address, value)
END CLASS

METHOD New(console) CLASS ppuMemory
   ::console=console
	return Self

METHOD Read(address) CLASS ppuMemory
	//address = address % 0x4000
	do case
	   case address < 0x2000
         #ifdef SHOWLOG
         x=::console:Mapper:Read(address)
         clog("PPU.Read1: ",address," - Value: ",x)
		   return x
         #endif
		   return ::console:Mapper:Read(address)
	   case address < 0x3F00
         #ifdef SHOWLOG
		   mode := ::console:Cartridge:Mirror
		   clog("Mode: ",mode)
		   clog("Adress: ",MirrorAddress(mode, address) % 2048)
         x=(::console:PPU:nameTableData[(MirrorAddress(mode, address) % 2048)+1])
         clog("PPU.Read2: ",(MirrorAddress(mode, address) % 2048)," = ",x)
		   return x
		   #endif
		   mode := ::console:Cartridge:Mirror
		   return (::console:PPU:nameTableData[(MirrorAddress(mode, address) % 2048)+1])
	   case address < 0x4000
         #ifdef SHOWLOG
	      x=::console:PPU:readPalette(address % 32)
	      clog("PPU.Read3: ",address % 32," = ",x)
		   return x
		   #endif
		   return ::console:PPU:readPalette(address % 32)
	   default
		   ? "unhandled ppu memory read at address: ", address
	end case
	return 0

METHOD Write(address, value) CLASS ppuMemory
   #ifdef SHOWLOG
   clog("PPU.write: ",address," = ",value)
   #endif
	address = address % 0x4000
	do case
	   case address < 0x2000
		   ::console:Mapper:Write(address, value)
   	case address < 0x3F00
	   	mode := ::console:Cartridge:Mirror
		   ::console:PPU:nameTableData[(MirrorAddress(mode, address) % 2048)+1] = value
   	case address < 0x4000
	   	::console:PPU:writePalette(address % 32, value)
	   default
		   ? "unhandled ppu memory write at address: ", address
	end case

// Mirroring Modes

/*const (
	MirrorHorizontal = 0
	MirrorVertical   = 1
	MirrorSingle0    = 2
	MirrorSingle1    = 3
	MirrorFour       = 4
)*/


function MirrorAddress(mode, address)
   Local MirrorLookup := {{0, 0, 1, 1},;
	                       {0, 1, 0, 1},;
	                       {0, 0, 0, 0},;
	                       {1, 1, 1, 1},;
	                       {0, 1, 2, 3}}

	address = (address - 0x2000) % 0x1000
	table := address / 0x0400
	offset := address % 0x0400
	return 0x2000 + MirrorLookup[mode+1][table+1] * 0x0400 + offset

