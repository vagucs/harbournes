//#define XHB_BITOP // Habilita das operações | & ^^

#include "xhb.ch"
#include "common.ch"
#include "hbclass.ch"

#translate ( <exp1> | <exp2> )      => ( hb_qbitOr( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> & <exp2> )      => ( hb_qbitAnd( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> ^^ <exp2> )     => ( hb_qbitXor( ( <exp1> ), ( <exp2> ) ) )

#xtranslate VER <v> <y>  => alert(<v>+" : "+str(<y>))
   
CREATE CLASS PPU
   VAR Memory           // memory interface
   VAR console          // reference to parent object

   VAR Cycle    INIT 0    // 0-340
   VAR ScanLine INIT 0    // 0-261, 0-239=visible, 240=post, 241-260=vblank, 261=pre
   VAR Frame    INIT 0 // frame counter

   // storage variables
   VAR paletteData   INIT aAloc(0,32) //repl(chr(0),32)
   VAR nameTableData INIT aAloc(0,2048) //repl(chr(0),2048)
   VAR oamData       INIT aAloc(0,260) //repl(chr(0),260)
   VAR front         //*image.RGBA
   VAR back          //*image.RGBA

   // PPU registers
   VAR v INIT 0 // current vram address (15 bit)
   VAR t INIT 0 // temporary vram address (15 bit)
   VAR x INIT 0 // fine x scroll (3 bit)
   VAR w INIT 0 // write toggle (1 bit)
   VAR f INIT 0 // even/odd frame flag (1 bit)

   VAR register INIT 0

   // NMI flags
   VAR nmiOccurred INIT .f.
   VAR nmiOutput   INIT .f.
   VAR nmiPrevious INIT .f.
   VAR nmiDelay    INIT 0

   // background temporary variables
   VAR nameTableByte      INIT 0
   VAR attributeTableByte INIT 0
   VAR lowTileByte        INIT 0
   VAR highTileByte       INIT 0
   VAR tileData           INIT 0

   // sprite temporary variables
   VAR spriteCount      INIT 0
   VAR spritePatterns   INIT {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
   VAR spritePositions  INIT {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
   VAR spritePriorities INIT {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
   VAR spriteIndexes    INIT {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}

   // $2000 PPUCTRL
   VAR flagNameTable       INIT 0 // 0: $2000; 1: $2400; 2: $2800; 3: $2C00
   VAR flagIncrement       INIT 0 // 0: add 1; 1: add 32
   VAR flagSpriteTable     INIT 0 // 0: $0000; 1: $1000; ignored in 8x16 mode
   VAR flagBackgroundTable INIT 0 // 0: $0000; 1: $1000
   VAR flagSpriteSize      INIT 0 // 0: 8x8; 1: 8x16
   VAR flagMasterSlave     INIT 0 // 0: read EXT; 1: write EXT

   // $2001 PPUMASK
   VAR flagGrayscale          INIT 0 // 0: color; 1: grayscale
   VAR flagShowLeftBackground INIT 0 // 0: hide; 1: show
   VAR flagShowLeftSprites    INIT 0 // 0: hide; 1: show
   VAR flagShowBackground     INIT 0 // 0: hide; 1: show
   VAR flagShowSprites        INIT 0 // 0: hide; 1: show
   VAR flagRedTint            INIT 0 // 0: normal; 1: emphasized
   VAR flagGreenTint          INIT 0 // 0: normal; 1: emphasized
   VAR flagBlueTint           INIT 0 // 0: normal; 1: emphasized

   // $2002 PPUSTATUS
   VAR flagSpriteZeroHit  INIT 0
   VAR flagSpriteOverflow INIT 0

   // $2003 OAMADDR
   VAR oamAddress INIT 0

   // $2007 PPUDATA
   VAR bufferedData INIT 0 // for buffered reads
   
   METHOD New(console)
   METHOD Reset()
   METHOD readPalette(address)
   METHOD writePalette(address, value)
   METHOD readRegister(address)
   METHOD writeRegister(address, value)
   METHOD writeControl(value)
   METHOD writeMask(value)
   METHOD readStatus()
   METHOD writeOAMAddress(value)
   METHOD readOAMData()
   METHOD writeOAMData(value)
   METHOD writeScroll(value)
   METHOD writeAddress(value)
   METHOD readData()
   METHOD writeData(value)
   METHOD writeDMA(value)
   METHOD incrementX()
   METHOD incrementY()
   METHOD copyX()
   METHOD copyY()
   METHOD nmiChange()
   METHOD setVerticalBlank()
   METHOD clearVerticalBlank()
   METHOD fetchNameTableByte()
   METHOD fetchAttributeTableByte()
   METHOD fetchLowTileByte()
   METHOD fetchHighTileByte()
   METHOD storeTileData()
   METHOD fetchTileData()
   METHOD backgroundPixel()
   METHOD spritePixel()
   METHOD renderPixel()
   METHOD fetchSpritePattern(i, row)
   METHOD evaluateSprites()
   METHOD tick()
   METHOD Step()
   METHOD PPUStatus()
   METHOD CHRView()
END CLASS

METHOD New(console) CLASS PPU
   ::Memory = ppuMemory():new(@console)
   ::Console = console
   ::front = image():new(0,0,256,240) //image.NewRGBA(image.Rect(0, 0, 256, 240)) // Suspeito
   ::back = image():new(0,0,256,240) //image.NewRGBA(image.Rect(0, 0, 256, 240))

   ::Reset()
   return Self
   
METHOD PPUStatus() CLASS PPU
   @ 14,70 say '[PPU]'
   @ 15,70 say "Frame              : "+str(::Frame)
   @ 16,70 say "V                  : "+hb_numtohex(::v)+"    "
   @ 17,70 say "T                  : "+hb_numtohex(::t)+"    "
   @ 18,70 say "X                  : "+hb_numtohex(::x)+"    "
   @ 19,70 say "W                  : "+hb_numtohex(::w)+"    "
   @ 20,70 say "F                  : "+hb_numtohex(::f)+"    "
   @ 21,70 say "Register           : "+str(::register)
   @ 22,70 say "nmiDelay           : "+str(::nmiDelay)
   @ 23,70 say "nameTableByte      : "+str(::nameTableByte)
   @ 24,70 say "attributeTableByte : "+str(::attributeTableByte)
   @ 25,70 say "lowTileByte        : "+str(::lowTileByte)
   @ 26,70 say "highTileByte       : "+str(::highTileByte)
   @ 27,70 say "tileData           : "+str(::tileData)+space(16)
   @ 28,70 say "spriteCount        : "+str(::spriteCount)
   @ 29,70 say "flagNameTable      : "+str(::flagNameTable)
   @ 30,70 say "flagIncrement      : "+str(::flagIncrement)
   @ 31,70 say "flagSpriteTable    : "+str(::flagSpriteTable)
   @ 32,70 say "flagBackgroundTable: "+str(::flagBackgroundTable)
   @ 33,70 say "flagSpriteSize     : "+str(::flagSpriteSize)
   @ 34,70 say "flagMasterSlave    : "+str(::flagMasterSlave)
   @ 35,70 say "renderingEnabled   : "+iif(::flagShowBackground != 0 .or. ::flagShowSprites != 0,"RENDER","      ")
   @ 36,70 say "scanline           : "+str(::scanline)


METHOD Reset() CLASS PPU
   ::Cycle = 340
   ::ScanLine = 240
   ::Frame = 0
   ::writeControl(0)
   ::writeMask(0)
   ::writeOAMAddress(0)

METHOD readPalette(address) CLASS PPU
   //clog("READPALETTE")
   if address >= 16 .and. address % 4 == 0
      address -= 16
   end if
   //clog("RET: ",(::paletteData[address+1]))
   return (::paletteData[address+1])

METHOD writePalette(address, value) CLASS PPU
   //clog("writePalette")
   if address >= 16 .and. address % 4 == 0
      address -= 16
   end if
   ::paletteData[address+1] = (value)

METHOD readRegister(address) CLASS PPU
   //clog("Read PPU register: ",address)
   do case
      case address=0x2002
         return ::readStatus()
      case address=0x2004
         return ::readOAMData()
      case address=0x2007
         return ::readData()
   end case
   return 0

METHOD writeRegister(address, value) CLASS PPU
   /*
   ppi := 2

   while ( !Empty(ProcName(ppi)) )
      plog(ProcName(ppi)+"("+str(ProcLine(ppi))+") - "+procfile(ppi))
      ppi++
   end do
   */

   //clog("Write register: ",address," = ",value)
   ::register = value
   do case
      case address=0x2000
         ::writeControl(value)
      case address=0x2001
         ::writeMask(value)
      case address=0x2003
         ::writeOAMAddress(value)
      case address=0x2004
         ::writeOAMData(value)
      case address=0x2005
         ::writeScroll(value)
      case address=0x2006
         ::writeAddress(value)
      case address=0x2007
         ::writeData(value)
      case address=0x4014
         ::writeDMA(value)
   end case

// $2000: PPUCTRL
METHOD writeControl(value) CLASS PPU
   //clog("writeControl")
   ::flagNameTable       = ((value >> 0) & 3)
   ::flagIncrement       = ((value >> 2) & 1)
   ::flagSpriteTable     = ((value >> 3) & 1)
   ::flagBackgroundTable = ((value >> 4) & 1)
   ::flagSpriteSize      = ((value >> 5) & 1)
   ::flagMasterSlave     = ((value >> 6) & 1)
   ::nmiOutput           = ((value>>7) & 1) == 1
   ::nmiChange()

   //clog("flagNameTable: ",::flagNameTable)
   //clog("flagIncrement: ",::flagIncrement)
   //clog("flagSpriteTable: ",::flagSpriteTable)
   //clog("flagBackgroundTable: ",::flagBackgroundTable)
   //clog("flagSpriteSize: ",::flagSpriteSize)
   //clog("flagMasterSlave: ",::flagMasterSlave)

   // t: ....BA.. ........ = d: ......BA
   ::t = ((::t & 0xF3FF) | ((value) & 0x03) << 10)
   //clog("t: ",::t)

// $2001: PPUMASK
METHOD writeMask(value) CLASS PPU
   //clog("writeMask: ",value)
   ::flagGrayscale          = ((value >> 0) & 1)
   ::flagShowLeftBackground = ((value >> 1) & 1)
   ::flagShowLeftSprites    = ((value >> 2) & 1)
   ::flagShowBackground     = ((value >> 3) & 1)
   ::flagShowSprites        = ((value >> 4) & 1)
   ::flagRedTint            = ((value >> 5) & 1)
   ::flagGreenTint          = ((value >> 6) & 1)
   ::flagBlueTint           = ((value >> 7) & 1)
   
   //clog("flagGrayscale: ",::flagGrayscale)
   //clog("flagShowLeftBackground: ",::flagShowLeftBackground)
   //clog("flagShowLeftSprites: ",::flagShowLeftSprites)
   //clog("flagShowBackground: ",::flagShowBackground)
   //clog("flagShowSprites: ",::flagShowSprites)
   //clog("flagRedTint: ",::flagRedTint)
   //clog("flagGreenTint: ",::flagGreenTint)
   //clog("flagGreenTint: ",::flagGreenTint)

// $2002: PPUSTATUS
METHOD readStatus() CLASS PPU
   result := (::register & 0x1F)
   result  = (result | (::flagSpriteOverflow << 5))
   result  = (result | (::flagSpriteZeroHit << 6))
   //clog("PPU READSTATUS: ",result)
   if ::nmiOccurred
      result = (result | (1 << 7))
   end if
   ::nmiOccurred = .f.
   ::nmiChange()
   // w:                   = 0
   ::w = 0
   return result

// $2003: OAMADDR
METHOD writeOAMAddress(value) CLASS PPU
   //clog("WRITE OAM ADDR: ",value)
   ::oamAddress = value

// $2004: OAMDATA (read)
METHOD readOAMData() CLASS PPU
   //clog("READ OAM DATA")
   return (::oamData[::oamAddress+1])

// $2004: OAMDATA (write)
METHOD writeOAMData(value) CLASS PPU
   //clog("WRITE OAM DATA: ",::oamAddress," = ",value)
   ::oamData[::oamAddress+1] = (value)
   ::oamAddress++

// $2005: PPUSCROLL
METHOD writeScroll(value) CLASS PPU
   //clog("writeScroll")
   if ::w == 0
      // t: ........ ...HGFED = d: HGFED...
      // x:               CBA = d: .....CBA
      // w:                   = 1
      ::t = (((::t & 0xFFE0) | ((value) >> 3)) & 0xFFFF)
      ::x = ((value & 0x07) & 0xFFFF)
      ::w = 1
   else
      // t: .CBA..HG FED..... = d: HGFEDCBA
      // w:                   = 0
      ::t = (((::t & 0x8FFF) | (((value) & 0x07) << 12)) & 0xFFFF)
      ::t = (((::t & 0xFC1F) | (((value) & 0xF8) << 2)) & 0xFFFF)
      ::w = 0
   end if

// $2006: PPUADDR
METHOD writeAddress(value) CLASS PPU
   //clog("writeAddress")
   if ::w == 0
      // t: ..FEDCBA ........ = d: ..FEDCBA
      // t: .X...... ........ = 0
      // w:                   = 1
      ::t = (((::t & 0x80FF) | (((value) & 0x3F) << 8)) & 0xFFFF)
      //alert('wrppu:'+hb_numtohex(::t))
      ::w = 1
   else
      // t: ........ HGFEDCBA = d: HGFEDCBA
      // v                    = t
      // w:                   = 0
      ::t = (((::t & 0xFF00) | (value)) & 0xFFFF)
      ::v = ::t
      //alert("wrppu2:"+hb_numtohex(::t))
      ::w = 0
   end if

// $2007: PPUDATA (read)
METHOD readData() CLASS PPU
   local value,buffered
   value := ::Memory:Read(::v)
   // emulate buffered reads
   if (::v % 0x4000) < 0x3F00
      buffered := ::bufferedData
      ::bufferedData = value
      value = buffered
   else
      ::bufferedData = ::Memory:Read(::v - 0x1000)
      //alert("read ppu2:"+hb_numtohex(::v - 0x1000))
   end if
   // increment address
   if ::flagIncrement == 0
      ::v += 1
   else
      ::v += 32
   end if
   //clog("READDATA PPU: ",value)
   return value

// $2007: PPUDATA (write)
METHOD writeData(value) CLASS PPU
   //clog("PPU WRITEDATA: ",::v," = ",value)
   ::Memory:Write(::v, value)
   if ::flagIncrement == 0
      ::v += 1
   else
      ::v += 32
   end if
   ::v=(::v & 0xFFFF)

// $4014: OAMDMA
METHOD writeDMA(value) CLASS PPU
   local address
   //clog("WRITE DMA: ",value)
   //cpu := ::console:CPU
   address := (((value) << 8) & 0xFFFF) 
   for i := 0 to 255
      ::oamAddress++
      ::oamData[::oamAddress] = (::console:cpu:memory:Read(address))
      address++
   end if
   ::Memory:console:CPU:stall += 513
   if ::Memory:console:CPU:Cycles % 2 == 1
      ::Memory:console:CPU:stall++
   end if

// NTSC Timing Helper Functions

METHOD incrementX() CLASS PPU
   // increment hori(v)
   // if coarse X == 31
   //clog("PPU INC X")
   if (::v & 0x001F) == 31
      // coarse X = 0
      ::v = ((::v & 0xFFE0) & 0xFFFF)
      // switch horizontal nametable
      ::v = ((::v ^^ 0x0400) & 0xFFFF)
   else
      // increment coarse X
      ::v++
   end if

METHOD incrementY() CLASS PPU
   // increment vert(v)
   // if fine Y < 7
   //clog("PPU INC Y")
   if (::v & 0x7000) != 0x7000
      // increment fine Y
      ::v = (::v + 0x1000 & 0xFFFF)
   else
      // fine Y = 0
      ::v = (::v & 0x8FFF)
      // let y = coarse Y
      y := ((::v & 0x03E0) >> 5)
      if y == 29
         // coarse Y = 0
         y = 0
         // switch vertical nametable
         ::v = ((::v ^^ 0x0800) & 0xFFFF)
      elseif y == 31
         // coarse Y = 0, nametable not switched
         y = 0
      else
         // increment coarse Y
         y++
      end if
      // put coarse Y back into v
      ::v = (((::v & 0xFC1F) | (y << 5)) & 0xFFFF)
   end if

METHOD copyX() CLASS PPU
   // hori(v) = hori(t)
   // v: .....F.. ...EDCBA = t: .....F.. ...EDCBA
   ::v = (((::v & 0xFBE0) | (::t & 0x041F)) & 0xFFFF)

METHOD copyY() CLASS PPU
   // vert(v) = vert(t)
   // v: .IHGF.ED CBA..... = t: .IHGF.ED CBA.....
   ::v = (((::v & 0x841F) | (::t & 0x7BE0)) & 0xFFFF)

METHOD nmiChange() CLASS PPU
   local nmi
   nmi := ::nmiOutput .and. ::nmiOccurred
   if nmi .and. !::nmiPrevious
      // TODO: this fixes some games but the delay shouldn't have to be so
      // long, so the timings are off somewhere
      ::nmiDelay = 15
   end if
   ::nmiPrevious = nmi

METHOD setVerticalBlank() CLASS PPU
   //_draw_sprite(::front:Buffer,::Back:Buffer,0,0)
   _stretch_sprite(_get_buffer(),hNes:ppu:back:Buffer(),0,0,256*3,240*3)

   //::front       = ::back 
   //::back        = ::front  // Suspeito
   ::nmiOccurred = .t.
   ::nmiChange()

METHOD clearVerticalBlank() CLASS PPU
   ::nmiOccurred = .f.
   ::nmiChange()

METHOD fetchNameTableByte() CLASS PPU
   //clog("Name table: ",(0x2000 | (::v & 0x0FFF)))
   ::nameTableByte = ::Memory:Read((0x2000 | (::v & 0x0FFF)))
   //clog("nameTableByte value: ",::nameTableByte)

METHOD fetchAttributeTableByte() CLASS PPU // Revisado endereços batem com o MESEN
   local v,address,shift
   v := ::v
   address := ((((0x23C0 | (v & 0x0C00)) | ((v >> 4) & 0x38)) | ((v >> 2) & 0x07)))
   shift   := (((((v >> 4) & 0xFF)) & 4) | (v & 2))
   // AQUI COM ERRO
   ::attributeTableByte = (((((::Memory:Read(address) >> shift) & 0xFF) & 3) << 2))
   //clog("fetchAttributeTableByte: v: ",v," - Address: ",address," - Shift: ",shift," - Attr: ",::attributeTableByte)

METHOD fetchLowTileByte() CLASS PPU // Revisada OK
   local fineY,table,tile,address
   fineY         := ((::v >> 12) & 7)
   table         := ::flagBackgroundTable
   tile          := ::nameTableByte
   address       := 0x1000 * table + tile * 16 + fineY
   ::lowTileByte := ::Memory:Read(address)
   //clog("fetchLowTileByte: ",::lowTileByte)

METHOD fetchHighTileByte() CLASS PPU
   local fineY,table,tile,address
   //clog("fetchHighTileByte")
   fineY          := ((::v >> 12) & 7)
   //clog("fineY: ",fineY)
   table          := ::flagBackgroundTable
   //clog("table: ",table)
   tile           := ::nameTableByte
   //clog("tile: ",tile)
   address        := 0x1000 * table + tile * 16 + fineY
   //clog("address: ",address)
   ::highTileByte := ::Memory:Read(address + 8)
   //clog("fetchHighTileByte: ",::highTileByte)

METHOD storeTileData() CLASS PPU
   local data:=0,i,a,p1,p2
   for i := 0 to 7
      a              := ::attributeTableByte
      p1             := ((::lowTileByte & 0x80) >> 7)
      p2             := ((::highTileByte & 0x80) >> 6)
      ::lowTileByte  := ((::lowTileByte << 1))
      ::highTileByte := ((::highTileByte << 1))

      //if i=0
      //   data=0
      //else
         data           := ((data << 4))
      //end if

      //clog("storeTileData - i: ",i," a: ",a," p1: ",p1," p2: ",p2," low: ",::lowTileByte," high: ",::highTileByte)
      data           := (data | ((a | p1) | p2)) // Suspeito
   next
   //clog("64:",data)
   ::tileData := (::tileData | data)
   //clog("Tile Data: ", ::tileData)

METHOD fetchTileData() CLASS PPU
   //clog("fetchTileData: ",::tileData)
   //clog("fetchTileData: ",(::tileData >> 32))
   return ((::tileData >> 32))

METHOD backgroundPixel() CLASS PPU
   local data
   if ::flagShowBackground == 0
      //clog("backgroundPixel: OFF")
      return 0
   end if
   //clog("backgroundPixel: ON")
   data := (::fetchTileData() >> ((7 - ::X) * 4))
   //clog("backgroundPixel: ",(data & 0x0F))
   return (data & 0x0F)

METHOD spritePixel() CLASS PPU
   local offset,color,i
   //clog("spritepixel")
   if ::flagShowSprites == 0
      return {0,0}
   end if
   for i := 0 to ::spriteCount-1
      offset := (::Cycle - 1) - ::spritePositions[i+1]
      if offset < 0 .or. offset > 7
         loop
      end if
      offset = 7 - offset
      color := ((::spritePatterns[i+1] >> (offset * 4)) & 0x0F)
      if color % 4 == 0
         loop
      end if
      return {i, color}
      //return {i,hb_randomint(0,255)}
   next
   return {0, 0}

METHOD renderPixel() CLASS PPU
   local x,y,background,sprite,i,b,s,color,c
   x          := ::Cycle - 1
   y          := ::ScanLine
   background := ::backgroundPixel()
   
   //clog("Background: ",background)
   
   sprite     := ::spritePixel()
   i          := sprite[1]
   sprite     := sprite[2]

   //clog("i: ",i)
   //clog("sprite: ",sprite)

   if x < 8 .and. ::flagShowLeftBackground == 0
      background := 0
   end if

   if x < 8 .and. ::flagShowLeftSprites == 0
      sprite := 0
   end if

   b := background % 4 != 0
   s := sprite % 4 != 0
   color := 0

   if !b .and. !s
      color = 0
   elseif !b .and. s
      color = (sprite | 0x10)
   elseif b .and. !s
      color = background
   else
      if ::spriteIndexes[i+1] == 0 .and. x < 255
         ::flagSpriteZeroHit = 1
      end if
      if ::spritePriorities[i+1] == 0
         color = (sprite | 0x10)
      else
         color = background
      end if
   end if

   c := Palette[(::readPalette(color) % 64)+1]
   ::back:SetRGBA(x, y, c) // Suspeito

METHOD fetchSpritePattern(i, row) CLASS PPU
   local tile,attributes,address,table,a,lowTileByte,highTileByte,data,p1,p2
   tile := (::oamData[i*4+2])
   attributes := (::oamData[i*4+3])
   address=0
   //clog("FETCHSPRITEPATTERN")
   if ::flagSpriteSize == 0
      if (attributes & 0x80) == 0x80
         row = 7 - row
      end if
      table := ::flagSpriteTable
      address = ((0x1000 * table + tile * 16 + row) & 0xFFFF)
   else
      if (attributes & 0x80) == 0x80
         row = 15 - row
      end if
      table := (tile & 1)
      tile  := (tile & 0xFE)
      if row > 7
         tile++
         row -= 8
      end if
      address = ((0x1000 * table + tile * 16 + row) & 0xFFFF)
   end if
   a := ((attributes & 3) << 2)
   lowTileByte  := ::Memory:Read(address)
   highTileByte := ::Memory:Read(address + 8)
   data=0
   for i := 0 to 7
      p1=0
      p2=0
      if (attributes & 0x40) == 0x40
         p1           := ((lowTileByte & 1) << 0)
         p2           := ((highTileByte & 1) << 1)
         lowTileByte  := (lowTileByte >> 1)
         highTileByte := (highTileByte >> 1)
      else
         p1           := ((lowTileByte & 0x80) >> 7)
         p2           := ((highTileByte & 0x80) >> 6)
         lowTileByte  := (lowTileByte << 1)
         highTileByte := (highTileByte << 1)
      end if

      data = (data << 4)

      data = (data | (a | p1 | p2))
   next
   return data

METHOD evaluateSprites() CLASS PPU
   local h := 0,i,y,a,x,row,count
   if ::flagSpriteSize == 0
      h = 8
   else
      h = 16
   end if
   count := 1
   for i := 0 to 63
      y   := (::oamData[i*4+1])
      a   := (::oamData[i*4+3])
      x   := (::oamData[i*4+4])
      row := ::ScanLine - y
      if row < 0 .or. row >= h
         loop
      end if
      if count < 9
         ::spritePatterns[count]   = ::fetchSpritePattern(i, row)
         ::spritePositions[count]  = x
         ::spritePriorities[count] = ((a >> 5) & 1)
         ::spriteIndexes[count]    = i
      end if
      count++
   next
   if count > 9
      count = 9
      ::flagSpriteOverflow = 1
   end if
   ::spriteCount = count-1

// tick updates Cycle, ScanLine and Frame counters
METHOD tick() CLASS PPU
   ////clog("Tick")
   if ::nmiDelay > 0
      ::nmiDelay--
      if ::nmiDelay == 0 .and. ::nmiOutput .and. ::nmiOccurred
         ////clog("NMI")
         ::console:CPU:triggerNMI()
      end if
   end if

   if ::flagShowBackground != 0 .or. ::flagShowSprites != 0
      if ::f == 1 .and. ::ScanLine == 261 .and. ::Cycle == 339
         ::Cycle = 0
         ::ScanLine = 0
         ::Frame++
         ::f = (::f ^^ 1)
         //clog("f: ",::f)
         return
      end if
   end if

   ::Cycle++

   if ::Cycle > 340
      ::Cycle = 0
      ::ScanLine++
      if ::ScanLine > 261
         ::ScanLine = 0
         ::Frame++
         ::f = (::f ^^ 1)
         //clog("f2: ",::f)
      end if
   end if

// Step executes a single PPU cycle
METHOD Step() CLASS PPU
   local renderingEnabled,preLine,visibleLine,renderLine,preFetchCycle,visibleCycle,fetchCycle
   ::tick()
   renderingEnabled := ::flagShowBackground != 0 .or. ::flagShowSprites != 0
   preLine          := ::ScanLine == 261
   visibleLine      := ::ScanLine < 240
   // postLine        := ::ScanLine == 240
   renderLine       := preLine .or. visibleLine
   preFetchCycle    := ::Cycle >= 321 .and. ::Cycle <= 336
   visibleCycle     := ::Cycle >= 1 .and. ::Cycle <= 256
   fetchCycle       := preFetchCycle .or. visibleCycle

   //if ::Frame % 2 = 0
   //   renderingEnabled=.f.
   //end if


   // background logic
   if renderingEnabled
      if visibleLine .and. visibleCycle
         //if ::Frame % 2 = 0
            ::renderPixel()
         //end if
      end if
      if renderLine .and. fetchCycle

         ::tileData = (::tileData << 4)

         xCase=::Cycle % 8
         //clog("case cycle")
         do case 
            case xCase=1
               //clog("fetchNameTableByte")
               ::fetchNameTableByte()
            case xCase=3
               //clog("fetchAttributeTableByte")
               ::fetchAttributeTableByte()
            case xCase=5
               //clog("fetchLowTileByte")
               ::fetchLowTileByte()
            case xCase=7
               //clog("fetchHighTileByte")
               ::fetchHighTileByte()
            case xCase=0
               //clog("storeTileData")
               ::storeTileData()
         end case
      end if

      if preLine .and. ::Cycle >= 280 .and. ::Cycle <= 304
         //clog("COPYY")
         ::copyY()
      end if

      if renderLine
         if fetchCycle .and. ::Cycle % 8 == 0
            //clog("incrementX")
            ::incrementX()
         end if
         if ::Cycle == 256
            //clog("incrementY")
            ::incrementY()
         end if
         if ::Cycle == 257
            //clog("copyX")
            ::copyX()
         end if
      end if
   end if

   // sprite logic
   if renderingEnabled
      if ::Cycle == 257
         if visibleLine
            ::evaluateSprites()
         else
            ::spriteCount = 0
         end if
      end if
   end if

   // vblank logic
   if ::ScanLine == 241 .and. ::Cycle == 1
      ::setVerticalBlank()
   end if
   if preLine .and. ::Cycle == 1
      ::clearVerticalBlank()
      ::flagSpriteZeroHit = 0
      ::flagSpriteOverflow = 0
   end if

METHOD CHRView() CLASS PPU

   clear
   xC=::Console:Cartridge:CHR
   
    _colors = {{0x10,0x10,0x10},;
               {0x77,0x77,0x77},;
               {0x00,0xFF,0x00},;
               {0xFF,0xFF,0xFF}}

   aRelativCol=0
   
   col=aRelativCol
   lin=0

   scale=2

   tile={}
   
   for i=1 to len(xC) step 16
      pattern0 = substr(xC,i,8)
      pattern1 = substr(xC,i+8,8)

      aadd(tile,_create_bitmap(8,8))
      
      gr_clear_to_color(tile[len(tile)],makecol(0,0,0))

      //char=pattern0+pattern1

      //str="$"+hb_numtohex(p,4)+" - "+char+" - "
      //for i=1 to len(char)
      //   str+=hb_numtohex(asc(substr(char,i,1)),2)+" "
      //next
   
      //@ 20,0 say str
      if len(pattern0)>=8 .and. len(pattern1)>=8
         for x = 0 to 7
            for y = 0 to 7

               bit0 = ((asc(pattern0[y+1]) >> (7 - x)) & 1)
               bit1 = ((asc(pattern1[y+1]) >> (7 - x)) & 1)
               color = bit0 + (bit1 << 1)

               cor = _colors[color+1];
            
               PutPixel(tile[len(tile)],x,y,makecol(cor[1],cor[2],cor[3]))
          
            next
         next
      end if
      if col>aRelativCol + ( 15 * 8 )
         col=aRelativCol
         lin+=16
         if lin*scale>video_WIDTH-8
            lin=0
            aRelativCol+= ( 15 * 8 )                
            col=aRelativCol-8
         end if
      end if
      _stretch_sprite(_get_buffer(),tile[len(tile)],col*scale,lin*scale,8*scale,16*scale)
      col+=8
   next

   /*
   tile={}
   
   scale=2
   
   col=0
   lin=0
   
   for p=1 to len(xC) step 16
   
      char=substr(xC,p,16)

      //str="$"+hb_numtohex(p,4)+" - "+char+" - "
      //for i=1 to len(char)
      //   str+=hb_numtohex(asc(substr(char,i,1)),2)+" "
      //next
   
      //@ 20,0 say str
      aadd(tile,_create_bitmap(8,8))
      
      gr_clear_to_color(tile[len(tile)],makecol(0,0,0))

      x=0
      byte=0
      for i=1 to 8
         byte++
         y=7
         for bit=0 to 7
            if (asc(char[byte]) & (1 << bit))>0
               PutPixel(tile[len(tile)],y,x,makecol(0xAA,0xAA,0xAA))
            end if
            y--
         next
         x++
      next

      x=0
      byte=0
      for i=1 to 8
         byte++
         y=7
         for bit=0 to 7
            if (asc(char[byte+8]) & (1 << bit))>0
               PutPixel(tile[len(tile)],y,x,makecol(0xFF,0xFF,0xFF))
            end if
            y--
         next
         x++
      next
      
      if col>(20*16)
         col=0
         lin+=16
      end if
      //_draw_sprite(_get_buffer(),tile[len(tile)],col,lin)
      _stretch_sprite(_get_buffer(),tile[len(tile)],col*scale,lin*scale,8*scale,16*scale)
      col+=16
      
   next
   */
   inkey(0)

procedure plog(...)
local nH
if file("ppu.txt")
   nH=fopen("ppu.txt",2)
else
   nH=fcreate("ppu.txt")
end if
fseek(nH,0,2)
aeval( hb_aParams(),{|x|fwrite(nH,st(x))})
fwrite(nH,chr(10))
fclose(nH)
   
procedure st(x)
if valtype(x)='N'
   return str(x)
else
   return x
end if
   
#pragma BEGINDUMP
#include <hbapi.h>

HB_FUNC( HB_QBITAND )
{
   hb_retnint(hb_parnl(1) & hb_parnl(2));
   /*
   HB_MAXINT lValue;

   if( hb_numParam( 1, &lValue ) )
   {
      int iPCount = hb_pcount() - 1, i = 1;
      do
      {
         HB_MAXINT lNext;
         if( ! hb_numParam( ++i, &lNext ) )
            return;
         lValue &= lNext;
      }
      while( --iPCount > 0 );
      hb_retnint( lValue );
   }*/
}

HB_FUNC( HB_QBITOR )
{
   hb_retnint(hb_parnl(1) | hb_parnl(2));
   /*HB_MAXINT lValue;

   if( hb_numParam( 1, &lValue ) )
   {
      int iPCount = hb_pcount() - 1, i = 1;
      do
      {
         HB_MAXINT lNext;
         if( ! hb_numParam( ++i, &lNext ) )
            return;
         lValue |= lNext;
      }
      while( --iPCount > 0 );
      hb_retnint( lValue );
   }*/
}

HB_FUNC( HB_QBITXOR )
{
   hb_retnint(hb_parnl(1) ^ hb_parnl(2));
   /*
   HB_MAXINT lValue;

   if( hb_numParam( 1, &lValue ) )
   {
      int iPCount = hb_pcount() - 1, i = 1;
      do
      {
         HB_MAXINT lNext;
         if( ! hb_numParam( ++i, &lNext ) )
            return;
         lValue ^= lNext;
      }
      while( --iPCount > 0 );
      hb_retnint( lValue );
   }*/
}

HB_FUNC( HB_QBITNOT )
{
   hb_retnint(~hb_parnl(1));
   //HB_MAXINT lValue;

   //if( hb_numParam( 1, &lValue ) )
   //   hb_retnint( ~lValue );
}

/*HB_FUNC( HB_QBITTEST )
{
   HB_MAXINT lValue, lBit;

   if( hb_numParam( 1, &lValue ) && hb_numParam( 2, &lBit ) )
      hb_retl( ( lValue & ( ( HB_MAXINT ) 1 << lBit ) ) != 0 );
}

HB_FUNC( HB_QBITSET )
{
   HB_MAXINT lValue, lBit;

   if( hb_numParam( 1, &lValue ) && hb_numParam( 2, &lBit ) )
      hb_retnint( lValue | ( ( HB_MAXINT ) 1 << lBit ) );
}

HB_FUNC( HB_QBITRESET )
{
   HB_MAXINT lValue, lBit;

   if( hb_numParam( 1, &lValue ) && hb_numParam( 2, &lBit ) )
      hb_retnint( lValue & ( ~( ( HB_MAXINT ) 1 << lBit ) ) );
}
*/
HB_FUNC( HB_QLBITSHIFT )
{

   hb_retnint( hb_parnl(1) << hb_parni(2) );
/*   HB_MAXINT lValue, lBits;

   if( hb_numParam( 1, &lValue ) && hb_numParam( 2, &lBits ) )
   {
      if( lBits < 0 )
         hb_retnint( lValue >> -lBits );
      else
   }*/
}

HB_FUNC( HB_QRBITSHIFT )
{
   hb_retnint( hb_parnl(1) >> -hb_parni(2) );
/*   HB_MAXINT lValue, lBits;

   if( hb_numParam( 1, &lValue ) && hb_numParam( 2, &lBits ) )
   {
      if( lBits < 0 )
         hb_retnint( lValue >> -lBits );
      else
         hb_retnint( lValue << lBits );
   }*/
}

#pragma ENDDUMP