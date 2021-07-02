#define XHB_BITOP // Habilita das operações | & ^^

#include "xhb.ch"
#include "common.ch"
#include "hbclass.ch"

//#define SHOWLOG
   
CREATE CLASS Mapper1
   VAR Cartridge
   VAR shiftRegister INIT 0
   VAR control       INIT 0
   VAR prgMode       INIT 0
   VAR chrMode       INIT 0
   VAR prgBank       INIT 0
   VAR chrBank0      INIT 0
   VAR chrBank1      INIT 0
   VAR prgOffsets    INIT {0,0}
   VAR chrOffsets    INIT {0,0}

   VAR prgBank1
   VAR prgBank2
   
   METHOD New(cartridge)
   METHOD Step()
   METHOD Read(address)
   METHOD Write(address, value)
   METHOD loadRegister(address,value)
   METHOD writeRegister(address,value)
   METHOD writeControl(value)
   METHOD writeCHRBank0(value)
   METHOD writeCHRBank1(value)
   METHOD writePRGBank(value)
   METHOD prgBankOffset(index)
   METHOD chrBankOffset(index)
   METHOD updateOffsets()
END CLASS

METHOD New(cartridge) // By ref
   ::Cartridge := cartridge
   ::shiftRegister = 0x10
   ::prgOffsets[2] = ::prgBankOffset(-1)
   return Self

METHOD  Step() CLASS Mapper1

RETURN

METHOD Read(address) CLASS Mapper1
   do case
      case address < 0x2000
         bank := address / 0x1000
         offset := address % 0x1000
         return ::Cartridge:CHR[::chrOffsets[bank+1]+int(offset)+1]
      case address >= 0x8000
         address = address - 0x8000
         bank := address / 0x4000
         offset := address % 0x4000
         return ::Cartridge:PRG[::prgOffsets[bank+1]+int(offset)+1]
      case address >= 0x6000
         return ::Cartridge:SRAM[int(address)-0x6000+1]
      default
         ? "unhandled mapper1 read at address: ", address
   end case
   return 0

METHOD Write(address, value) CLASS Mapper1
   do case
      case address < 0x2000
         bank := address / 0x1000
         offset := address % 0x1000
         ::Cartridge:CHR[::chrOffsets[bank+1]+int(offset)+1] = value
      case address >= 0x8000
         ::loadRegister(address, value)
      case address >= 0x6000
         ::Cartridge:SRAM[int(address)-0x6000+1] = value
      default
         ? "unhandled mapper1 write at address: ", address
   end case

METHOD loadRegister(address,value) CLASS Mapper1
   if (value & 0x80) == 0x80
      ::shiftRegister = 0x10
      ::writeControl((::control | 0x0C))
   else
      complete := (::shiftRegister & 1) == 1
      ::shiftRegister = (::shiftRegister >> 1)
      ::shiftRegister = (::shiftRegister | ((value & 1) << 4))
      if complete
         ::writeRegister(address, ::shiftRegister)
         ::shiftRegister = 0x10
      end if
   end if

METHOD writeRegister(address,value) CLASS Mapper1
   do case
      case address <= 0x9FFF
         ::writeControl(value)
      case address <= 0xBFFF
         ::writeCHRBank0(value)
      case address <= 0xDFFF
         ::writeCHRBank1(value)
      case address <= 0xFFFF
         ::writePRGBank(value)
   end case

// Control (internal, $8000-$9FFF)
METHOD writeControl(value) CLASS Mapper1
   ::control = value
   ::chrMode = ((value >> 4) & 1)
   ::prgMode = ((value >> 2) & 3)
   mirror := (value & 3)
   do case
      case mirror=0
         ::Cartridge:Mirror = 2 // MirrorSingle0
      case mirror=1
         ::Cartridge:Mirror = 3 // MirrorSingle1
      case mirror=2
         ::Cartridge:Mirror = 1 // MirrorVertical
      case mirror=3
         ::Cartridge:Mirror = 0 // MirrorHorizontal
   end case
   ::updateOffsets()

// CHR bank 0 (internal, $A000-$BFFF)
METHOD writeCHRBank0(value) CLASS Mapper1
   ::chrBank0 = value
   ::updateOffsets()

// CHR bank 1 (internal, $C000-$DFFF)
METHOD writeCHRBank1(value) CLASS Mapper1
   ::chrBank1 = value
   ::updateOffsets()

// PRG bank (internal, $E000-$FFFF)
METHOD writePRGBank(value) CLASS Mapper1
   ::prgBank = (value & 0x0F)
   ::updateOffsets()

METHOD prgBankOffset(index) CLASS Mapper1
   if index >= 0x80
      index -= 0x100
   end if
   index %= len(::Cartridge:PRG) / 0x4000
   offset := index * 0x4000
   if offset < 0
      offset += len(::Cartridge:PRG)
   end if
   return offset

METHOD chrBankOffset(index) CLASS Mapper1
   if index >= 0x80
      index -= 0x100
   end if
   index %= len(::Cartridge:CHR) / 0x1000
   offset := index * 0x1000
   if offset < 0
      offset += len(::Cartridge:CHR)
   end if
   return offset

// PRG ROM bank mode (0, 1: switch 32 KB at $8000, ignoring low bit of bank number;
//                    2: fix first bank at $8000 and switch 16 KB bank at $C000;
//                    3: fix last bank at $C000 and switch 16 KB bank at $8000)
// CHR ROM bank mode (0: switch 8 KB at a time; 1: switch two separate 4 KB banks)
METHOD updateOffsets() CLASS Mapper1
   do case
      case ::prgMode=0 .or. ::prgMode=1
         ::prgOffsets[1] = ::prgBankOffset(int((::prgBank & 0xFE)))
         ::prgOffsets[2] = ::prgBankOffset(int((::prgBank | 0x01)))
      case ::prgMode=2
         ::prgOffsets[1] = 0
         ::prgOffsets[2] = ::prgBankOffset(int(::prgBank))
      case ::prgMode=3
         ::prgOffsets[1] = ::prgBankOffset(int(::prgBank))
         ::prgOffsets[2] = ::prgBankOffset(-1)
   end case

   do case
      case ::chrMode=0
         ::chrOffsets[1] = ::chrBankOffset(int((::chrBank0 & 0xFE)))
         ::chrOffsets[2] = ::chrBankOffset(int((::chrBank0 | 0x01)))
      case ::chrMode=1
         ::chrOffsets[1] = ::chrBankOffset(int(::chrBank0))
         ::chrOffsets[2] = ::chrBankOffset(int(::chrBank1))
   end case
