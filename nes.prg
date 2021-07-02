/*


*/
#define XHB_BITOP // Habilita das operações | & ^^

#include "xhb.ch"

procedure main(file)

set date to french
set century on
set decimal to 0

request HB_GT_ALLEG //_DEFAULT
request HB_GT_ALLEG_DEFAULT

//gr_inicia()

video_Mode='WINDOW'

video_HEIGHT=1024
video_WIDTH=768
video_VHEIGHT=video_HEIGHT
video_VWIDTH=video_WIDTH
video_BITS=16

if "WINDOW"$video_Mode
   #ifdef __PLATFORM__WINDOWS
   video_Mode='GDIB'
   #else
   video_Mode='XWIN'
   #endif
end if
if "FULLSCREEN"$video_Mode
   #ifdef __PLATFORM__WINDOWS
   video_Mode='DXAC'
   #else
   video_Mode='XWFS'
   #endif
end if

config_driver(video_Mode)
config_lib(video_BITS,video_HEIGHT,video_WIDTH,video_VHEIGHT,video_VWIDTH)

gr_setmode(40,120)

set color to n/w,gr+/b

do while .t.
   xW=gr_openwin(5,5,11,25,'HBNES',,.f.,,)
   nOp={"Start ROM","View memory","About","Quit"}
   nResp=achoice(7,7,10,23,nOp)
   do case
      case nResp=1
         load_rom(file)
      case nResp=2
         /* TODO */
      case nResp=3
         alert("NES Emulation test;in Harbour.;;By vagucs;;www.vagucs.com.br;;vagucs@bol.com.br")
      case nResp=4
         quit
   end case
   gr_closewin(xW)
enddo

/*    START_PUB_VAR -> Start public variables    */
procedure start_pub_var
local i

Public CPUFrequency := 1789773

Public frameCounterRate  :=   CPUFrequency / 240.0

Public lengthTable := {10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14,;
                       12, 16, 24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30}

Public dutyTable := {{0, 1, 0, 0, 0, 0, 0, 0},;
                     {0, 1, 1, 0, 0, 0, 0, 0},;
                     {0, 1, 1, 1, 1, 0, 0, 0},;
                     {1, 0, 0, 1, 1, 1, 1, 1}}

Public triangleTable := {15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0,;
                         0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15}

Public noiseTable := {4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068}

Public dmcTable := {214, 190, 170, 160, 143, 127, 113, 107, 95, 80, 71, 64, 53, 42, 36, 27}

Public pulseTable:={}
Public tndTable:={}

for i := 1 to 32
   aadd(pulseTable, 95.52 / (8128.0/i + 100))
next
for i := 1 to 204
   aadd(tndTable, 163.67 / (24329.0/i + 100))
next

Public interruptNone       := 1
Public interruptNMI        := 2
Public interruptIRQ        := 3

Public modeAbsolute        := 1
Public modeAbsoluteX       := 2
Public modeAbsoluteY       := 3
Public modeAccumulator     := 4
Public modeImmediate       := 5
Public modeImplied         := 6
Public modeIndexedIndirect := 7
Public modeIndirect        := 8
Public modeIndirectIndexed := 9
Public modeRelative        := 10
Public modeZeroPage        := 11
Public modeZeroPageX       := 12
Public modeZeroPageY       := 13


Public pLog:=.f.
//                          1              2                  3             4                 5                 6               7                8
Public Palette := {0x666666,0x002A88,0x1412A7,0x3B00A4,0x5C007E,0x6E00040,0x6C0600,0x561D00,; // 8
                   0x333500,0x0B4800,0x005200,0x004F08,0x00404D,0x0000000,0x000000,0x000000,; // 16
                   0xADADAD,0x155FD9,0x4240FF,0x7527FE,0xA01ACC,0xB71E07B,0xB53120,0x994E00,; // 24
                   0x6B6D00,0x388700,0x0C9300,0x008F32,0x007C8D,0x0000000,0x000000,0x000000,; // 32
                   0xFFFEFF,0x64B0FF,0x9290FF,0xC676FF,0xF36AFF,0xFE6E0CC,0xFE8170,0xEA9E22,; // 40
                   0xBCBE00,0x88D800,0x5CE430,0x45E082,0x48CDDE,0x4F4F04F,0x000000,0x000000,; // 48
                   0xFFFEFF,0xC0DFFF,0xD3D2FF,0xE8C8FF,0xFBC2FF,0xFEC40EA,0xFECCC5,0xF7D8A5,; // 56
                   0xE4E594,0xCFEF96,0xBDF4AB,0xB3F3CC,0xB5EBF2,0xB8B80B8,0x000000,0x000000,} // 64


procedure load_rom(cFilename)
start_pub_var()

//cFilename="nestest.nes"
if !file(cFilename)
   alert("File not found!")
   return
end if

set color to w/n
clear
? "Criando console virtual"
hNes=Console():New(cFilename)

? "----- Mapper"
/*
? "PRG Banks(1): ",hNes:Mapper:prgBank
? "PRG Banks(2): ",hNes:Mapper:prgBank1
? "PRG Banks(3): ",hNes:Mapper:prgBank2*/
? "----- CHR"
//xStr=left(hNes:Mapper:Cartridge:CHR,16)

xV=""
for i=1 to 16
   xV+=hb_numtohex((hNes:Mapper:Cartridge:CHR[i]),2)+" "
next
//xV+=" - "+xStr
? xV
? "----- PRG"
//xStr=left(hNes:Mapper:Cartridge:PRG,16)

xV=""
for i=1 to 16
   xV+=hb_numtohex((hNes:Mapper:Cartridge:PRG[i]),2)+" "
next
//xV+=" - "+xStr
? xV

//xStr=left(hNes:Mapper:Cartridge:PRG,16)

//hNes:Reset()
? '-------------------------------'
? "Press a key to start emulation."
INKEY(0)
clear

//hNes:PPU:Reset()

lRun=.t.
set cursor off
uf=0
xCPU=0
do while .t.
   //hNes:CPU:PrintInstruction()
   //hNes:PPU:PPUStatus()
   //hNes:CPU:PrintREG()
   xCPu++
   if hNes:PPU:Frame#uf .and. hNes:PPU:ScanLine=0
   //if //hNes:PPU:ScanLine=261
      //_draw_sprite(_get_buffer(),hNes:ppu:front:Buffer(),0,0)
      //_draw_sprite(_get_buffer(),hNes:ppu:front:Buffer(),0,0)
      //_stretch_sprite(_get_buffer(),hNes:ppu:back:Buffer(),0,0,256*3,240*3)
      
      uf=hNes:PPU:Frame
      //if uf>3
      //   pLog:=.t.
      //end if
      @ 2,97 say "Frame: "+str(uf)
      @ 3,97 say "CPU  : "+str(xCpu)
      xCpu  =0
      
      //if uf=5
      //   inkey(0)
      //   quit
      //end if
      
   end if

   hNes:Step()
   if !lRun
      tk=inkey(0)
      if chr(tk)$'Rr'
         lRun=.t.
      end if
      if chr(tk)$'Dd'
         nH=fcreate("ram.bin")
         fwrite(nH,hNes:Ram,len(hNes:Ram))
         fclose(nH)                                 
         nH=fcreate("prg.bin")
         fwrite(nH,hNes:Cartridge:PRG,len(hNes:Cartridge:PRG))
         fclose(nH)                                 
         nH=fcreate("chr.bin")
         fwrite(nH,hNes:Cartridge:CHR,len(hNes:Cartridge:CHR))
         fclose(nH)                                 
      end if
      if chr(tk)$'Pp'
         clear
         y=0
         x=0
         clear
         for i=1 to len(pallete)-1
            sp=_create_bitmap(16,16)
            gr_clear_to_color(sp,makecol(pallete[i,1],pallete[i,2],pallete[i,3]))
            _draw_sprite(_get_buffer(),sp,y,x)
            y+=16
            if y>240
               y=0
               x+=16
            end if
         next
         inkey(0)
      end if
      if chr(tk)$'Aa'
         hNes:PPU:CHRView()
      end if
   end if
enddo
