/*
Emulador NES

Convertido do projeto https://github.com/fogleman/nes

Wagner Nunes da Silva
www.vagucs.com.br

vagucs@gmail.com
vagucs@vagucs.com.br
vagucs@bol.com.br

*/
#define XHB_BITOP // Habilita das operações | & ^^

#include "nesopt.ch"
#include "xhb.ch"
#include "common.ch"
#include "hbclass.ch"

CREATE CLASS Console
	VAR CPU         INIT Nil
	VAR APU         INIT Nil
	VAR PPU         INIT Nil
	VAR Cartridge   INIT Nil
	VAR Controller1 INIT Nil
	VAR Controller2 INIT Nil
	VAR Mapper      INIT Nil
	VAR RAM         INIT ''
	
	METHOD NEW(path)
	METHOD Reset()
	METHOD Step()
	METHOD StepFast()
	METHOD StepFrame()
	METHOD StepFrameFast()
	METHOD StepSeconds(seconds)
	METHOD Buffer()
	METHOD BackgroundColor()
	METHOD SetButtons1(buttons)
	METHOD SetButtons2(buttons)
	METHOD SetAudioChannel(channel)
	METHOD SetAudioSampleRate(sampleRate)
END CLASS

METHOD New(path) CLASS Console

	::cartridge := LoadNESFile(path)
	
	::RAM := aAloc(0,2048)
	
	//::RAM := repl(chr(0),2048)

	::controller1 := Controller():New()
	::controller2 := Controller():New()
	
	//console := Console{
	//	nil, nil, nil, cartridge, controller1, controller2, nil, ram}
	
	if ::cartridge:Mapper=0
	   ::Mapper := Mapper2():New(@::cartridge)
	elseif ::cartridge:Mapper=1
	   ::Mapper := Mapper1():New(@::cartridge)
	elseif ::cartridge:Mapper=2
	   ::Mapper := Mapper2():New(@::cartridge)
	elseif ::cartridge:Mapper=4
	   ::Mapper := Mapper4():New(@::cartridge,@self)
	elseif ::cartridge:Mapper=7
	   ::Mapper := Mapper7():New(@::cartridge)
   end if

	//::Mapper = ::cartridge:Mapper
	::CPU = CPU():New(@self)
	::APU = APU():New(@self)
	::PPU = PPU():New(@self)
	return Self

METHOD Reset() CLASS Console
	::CPU:Reset()

METHOD Step() CLASS Console
   local i, ppuCycles, cpuCycles

	cpuCycles := ::CPU:Step()
	ppuCycles := cpuCycles * 3

#ifdef OTIMIZADO
   if ::PPU:flagShowBackground == 0 .and. ::PPU:flagShowSprites == 0
      ::PPU:AdvanceCycles( ppuCycles )
   else
      for i := 1 to ppuCycles
         ::PPU:Step()
         ::Mapper:Step()
      next
   end if
#else
   for i := 1 to ppuCycles
      ::PPU:Step()
      ::Mapper:Step()
   next
#endif

	//for i := 0 to  cpuCycles-1
		//::APU:Step()
	//next

	return cpuCycles

METHOD StepFast() CLASS Console
   local i, ppuCycles, cpuCycles

   cpuCycles := ::CPU:Step()
   ppuCycles := cpuCycles * 3

#ifdef OTIMIZADO
   for i := 1 to ppuCycles
      ::PPU:FastStep()
      ::Mapper:Step()
   next
#else
   for i := 1 to ppuCycles
      ::PPU:Step()
      ::Mapper:Step()
   next
#endif

   return cpuCycles

METHOD StepFrame() CLASS Console
   local cpuCycles := 0, frame

   frame := ::PPU:Frame
   do while frame == ::PPU:Frame
      cpuCycles += ::Step()
   enddo
   return cpuCycles

METHOD StepFrameFast() CLASS Console
   local cpuCycles := 0, frame

   frame := ::PPU:Frame
   do while frame == ::PPU:Frame
      cpuCycles += ::StepFast()
   enddo
   return cpuCycles

METHOD StepSeconds(seconds) CLASS Console
	cycles := CPUFrequency * seconds
	do while cycles > 0
		cycles -= ::Step()
	enddo

METHOD Buffer() CLASS Console
	return ::PPU:front

METHOD BackgroundColor() CLASS Console
   x=Palette[::PPU:readPalette(0) % 64]
   //clog("LER BK COLOR: ",x)
	return x

METHOD SetButtons1(buttons) CLASS Console
	::Controller1:SetButtons(buttons)

METHOD SetButtons2(buttons) CLASS Console
	::Controller2:SetButtons(buttons)

METHOD SetAudioChannel(channel) CLASS Console
	::APU:channel = channel

METHOD SetAudioSampleRate(sampleRate) CLASS Console
	if sampleRate != 0
		// Convert samples per second to cpu steps per sample
		::APU:sampleRate = CPUFrequency / sampleRate
		// Initialize filters
		/*
		   ::APU:filterChain = FilterChain{
			HighPassFilter(float32(sampleRate), 90),
			HighPassFilter(float32(sampleRate), 440),
			LowPassFilter(float32(sampleRate), 14000),
		*/
	else
		::APU:filterChain = nil
	end if

procedure aAloc(xC,xS)
local aRet
#ifdef OTIMIZADO
   aRet := array( xS )
   aFill( aRet, xC )
#else
   local i
   aRet := {=>}
   for i := 1 to xS
      aRet[i] := xC
   next
#endif
return aRet