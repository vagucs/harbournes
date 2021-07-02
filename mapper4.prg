#define XHB_BITOP // Habilita das operações | & ^^

#include "xhb.ch"
#include "common.ch"
#include "hbclass.ch"

//#define SHOWLOG
   
CREATE CLASS Mapper4
   VAR Cartridge
   VAR Console
   VAR register INIT 0 // byte
   VAR registers INIT {0,0,0,0,0,0,0,0}
   VAR prgMode INIT 0 // byte
   VAR chrMode INIT 0 // byte
   VAR prgOffsets INIT {0,0,0,0}
   VAR chrOffsets INIT {0,0,0,0,0,0,0,0}
   VAR Reload INIT 0
   VAR counter INIT 0
   VAR irqEnable INIT .f.
   
   METHOD New(cartridge,console)
   METHOD Step()
   METHOD Read(address)
   METHOD Write(address, value)
   METHOD updateOffsets()
   METHOD chrBankOffset(index)
   METHOD prgBankOffset(index)
   METHOD writeIRQEnable(value)
   METHOD writeIRQDisable(value)
   METHOD writeIRQReload(value)
   METHOD writeIRQLatch(value)
   METHOD writeProtect(value)
   METHOD writeMirror(value)
   METHOD writeBankData(value)
   METHOD writeBankSelect(value)
   METHOD writeRegister(address,value)
   METHOD HandleScanLine()
END CLASS

METHOD New(cartridge,console) CLASS Mapper4
   ::Cartridge := cartridge
   ::Console   := console
   ::prgOffsets[1]=::prgBankOffset(0)
   ::prgOffsets[2]=::prgBankOffset(1)
   ::prgOffsets[3]=::prgBankOffset(-2)
   ::prgOffsets[4]=::prgBankOffset(-1)
RETURN Self

METHOD  Step() CLASS Mapper4
   if ::Console:PPU:Cycle != 280  // TODO: this *should* be 260
      return
   end if
   if ::Console:PPU:ScanLine > 239 .and. ::Console:PPU:ScanLine < 261
      return
   end if
   if ::Console:PPU:flagShowBackground == 0 .and. ::Console:PPU:flagShowSprites == 0
      return
   end if
   ::HandleScanLine()

RETURN

METHOD HandleScanLine() CLASS Mapper4
   if ::counter == 0
      ::counter = ::reload
   else
      ::counter--
      if ::counter == 0 .and. ::irqEnable
         ::console:CPU:triggerIRQ()
      end if
   end if

METHOD Read(address) CLASS Mapper4
   do case
      case address < 0x2000
         bank := address / 0x0400
         offset := address % 0x0400
         return ::cartridge:CHR[::chrOffsets[bank+1]+int(offset)+1]
      case address >= 0x8000
         address = address - 0x8000
         bank := address / 0x2000
         offset := address % 0x2000
         return ::cartridge:PRG[::prgOffsets[bank+1]+int(offset)+1]
      case address >= 0x6000
         return ::cartridge:SRAM[((address)-0x6000)+1]
      default
         ? "unhandled mapper4 read at address: ",address
   end case
   return 0

METHOD Write(address, value) CLASS Mapper4
   do case
      case address < 0x2000
         bank := address / 0x0400
         offset := address % 0x0400
         ::cartridge:CHR[::chrOffsets[bank+1]+int(offset)+1] = value
      case address >= 0x8000
         ::writeRegister(address, value)
      case address >= 0x6000
         ::cartridge:SRAM[(int(address)-0x6000)+1] = value
      default
         ? "unhandled mapper4 write at address: ",address
   end case


METHOD writeRegister(address,value) CLASS Mapper4
   do case
      case address <= 0x9FFF .and. address % 2 == 0
         ::writeBankSelect(value)
      case address <= 0x9FFF .and. address % 2 == 1
         ::writeBankData(value)
      case address <= 0xBFFF .and. address % 2 == 0
         ::writeMirror(value)
      case address <= 0xBFFF .and. address % 2 == 1
         ::writeProtect(value)
      case address <= 0xDFFF .and. address % 2 == 0
         ::writeIRQLatch(value)
      case address <= 0xDFFF .and. address % 2 == 1
         ::writeIRQReload(value)
      case address <= 0xFFFF .and. address % 2 == 0
         ::writeIRQDisable(value)
      case address <= 0xFFFF .and. address % 2 == 1
         ::writeIRQEnable(value)
   end case

METHOD writeBankSelect(value) CLASS Mapper4
   ::prgMode = ((value >> 6) & 1)
   ::chrMode = ((value >> 7) & 1)
   ::register = (value & 7)
   ::updateOffsets()

METHOD writeBankData(value) CLASS Mapper4
   ::registers[::register+1] = value
   ::updateOffsets()

METHOD writeMirror(value) CLASS Mapper4
   do case
      case (value & 1)=0
         ::Cartridge:Mirror = 1 // MirrorVertical
      case (value & 1)=1
         ::Cartridge:Mirror = 0 // MirrorHorizontal
   end case

METHOD writeProtect(value) CLASS Mapper4

METHOD writeIRQLatch(value) CLASS Mapper4
   ::reload = value

METHOD writeIRQReload(value) CLASS Mapper4
   ::counter = 0

METHOD writeIRQDisable(value) CLASS Mapper4
   ::irqEnable = .f.


METHOD writeIRQEnable(value) CLASS Mapper4
   ::irqEnable = .t.


METHOD prgBankOffset(index) CLASS Mapper4
   if index >= 0x80
      index -= 0x100
   end if
   index %= (len(::cartridge:PRG) / 0x2000)
   offset := index * 0x2000
   if offset < 0
      offset += len(::cartridge:PRG)
   end if
   return offset

METHOD chrBankOffset(index) CLASS Mapper4
   if index >= 0x80
      index -= 0x100
   end if
   index %= len(::cartridge:CHR) / 0x0400
   offset := index * 0x0400
   if offset < 0
      offset += len(::cartridge:CHR)
   end if
   return offset

METHOD updateOffsets() CLASS Mapper4
   do case
      case ::prgMode=0
         ::prgOffsets[1] = ::prgBankOffset(int(::registers[7]))
         ::prgOffsets[2] = ::prgBankOffset(int(::registers[8]))
         ::prgOffsets[3] = ::prgBankOffset(-2)
         ::prgOffsets[4] = ::prgBankOffset(-1)
      case ::prgMode=1
         ::prgOffsets[1] = ::prgBankOffset(-2)
         ::prgOffsets[2] = ::prgBankOffset(int(::registers[8]))
         ::prgOffsets[3] = ::prgBankOffset(int(::registers[7]))
         ::prgOffsets[4] = ::prgBankOffset(-1)
   end case
   do case
      case ::chrMode=0
         ::chrOffsets[1] = ::chrBankOffset(int((::registers[1] & 0xFE)))
         ::chrOffsets[2] = ::chrBankOffset(int((::registers[1] | 0x01)))
         ::chrOffsets[3] = ::chrBankOffset(int((::registers[2] & 0xFE)))
         ::chrOffsets[4] = ::chrBankOffset(int((::registers[2] | 0x01)))
         ::chrOffsets[5] = ::chrBankOffset(int(::registers[3]))
         ::chrOffsets[6] = ::chrBankOffset(int(::registers[4]))
         ::chrOffsets[7] = ::chrBankOffset(int(::registers[5]))
         ::chrOffsets[8] = ::chrBankOffset(int(::registers[6]))
      case ::chrMode=1
         ::chrOffsets[1] = ::chrBankOffset(int(::registers[3]))
         ::chrOffsets[2] = ::chrBankOffset(int(::registers[4]))
         ::chrOffsets[3] = ::chrBankOffset(int(::registers[5]))
         ::chrOffsets[4] = ::chrBankOffset(int(::registers[6]))
         ::chrOffsets[5] = ::chrBankOffset(int((::registers[1] & 0xFE)))
         ::chrOffsets[6] = ::chrBankOffset(int((::registers[1] | 0x01)))
         ::chrOffsets[7] = ::chrBankOffset(int((::registers[2] & 0xFE)))
         ::chrOffsets[8] = ::chrBankOffset(int((::registers[2] | 0x01)))
   end case
