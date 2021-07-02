//#define XHB_BITOP // Habilita das operações | & ^^

#include "xhb.ch"
#include "common.ch"
#include "hbclass.ch"

#translate ( <exp1> | <exp2> )      => ( hb_qbitOr( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> & <exp2> )      => ( hb_qbitAnd( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> ^^ <exp2> )     => ( hb_qbitXor( ( <exp1> ), ( <exp2> ) ) )

// APU

CREATE CLASS APU
	VAR console     
	VAR channel     INIT 0
	VAR sampleRate  INIT 0
	VAR pulse1                  //Pulse
	VAR pulse2                  //Pulse
	VAR triangle                //Triangle
	VAR noise                   //Noise
	VAR dmc                     //DMC
	VAR cycle       INIT 0      //uint64
	VAR framePeriod INIT 0      //byte
	VAR frameValue  INIT 0      //byte
	VAR frameIRQ    INIT .f.    //INIT .f.
	VAR filterChain             //FilterChain
   METHOD New(console)
   METHOD Step()
   METHOD sendSample()
   METHOD output()
   METHOD stepFrameCounter()
   METHOD stepTimer()
   METHOD stepEnvelope()
   METHOD stepSweep()
   METHOD stepLength()
   METHOD fireIRQ()
   METHOD readRegister(address)
   METHOD writeRegister(address, value)
   METHOD readStatus()
   METHOD writeControl(value)
   METHOD writeFrameCounter(value)

END CLASS

METHOD New(console) CLASS APU
	::console = console
	::noise = apuNoise():new()
	::noise:shiftRegister = 1
	::pulse1=apuPulse():new()
	::pulse2=apuPulse():new()
	::triangle=apuTriangle():new()
	::pulse1:channel = 1
	::pulse2:channel = 2
	::dmc = dmc():new(@console)
//	::dmc:cpu = console:CPU
	return Self


METHOD Step() CLASS Apu
   local cycle1,cycle2,f1,f2,s1,s2
	cycle1 := ::cycle
	::cycle++
	cycle2 := ::cycle
	::stepTimer()
	f1 := int((cycle1) / frameCounterRate)
	f2 := int((cycle2) / frameCounterRate)
	if f1 != f2
		::stepFrameCounter()
	end if
	s1 := int((cycle1) / ::sampleRate)
	s2 := int((cycle2) / ::sampleRate)
	if s1 != s2
		::sendSample()
	end if

METHOD sendSample() CLASS Apu
   local output
	output := ::filterChain:Step(::output())
	//select {
	//case ::channel <- output:
	//default:
	//} // Suspeito

METHOD output() CLASS Apu
   local p1,p2,t,n,d
	p1       := ::pulse1:output()
	p2       := ::pulse2:output()
	t        := ::triangle:output()
	n        := ::noise:output()
	d        := ::dmc:output()
	pulseOut := pulseTable[p1+p2+1]
	tndOut   := tndTable[3*t+2*n+d+1]
	return pulseOut + tndOut

// mode 0:    mode 1:       function
// ---------  -----------  -----------------------------
//  - - - f    - - - - -    IRQ (if bit 6 is clear)
//  - l - l    l - l - -    Length counter and sweep
//  e e e e    e e e e -    Envelope and linear counter
METHOD stepFrameCounter() CLASS Apu
	do case
	   case ::framePeriod=4
	   	::frameValue = (::frameValue + 1) % 4
	   	do case
	   	   case ::frameValue=0 .or. ::frameValue=2
	   		   ::stepEnvelope()
	   	   case ::frameValue=1
	   		   ::stepEnvelope()
	   		   ::stepSweep()
	   		   ::stepLength()
	   	   case ::frameValue=3
	   		   ::stepEnvelope()
	   		   ::stepSweep()
	   		   ::stepLength()
	   		   ::fireIRQ()
	   	end case
	   case ::framePeriod=5
	   	::frameValue = (::frameValue + 1) % 5
	   	do case
	   	   case ::frameValue=0 .or. ::frameValue=2
	      		::stepEnvelope()
      		case ::frameValue=1 .or. ::frameValue=3
	   	   	::stepEnvelope()
	      		::stepSweep()
      			::stepLength()
	   	end case
	end case

METHOD stepTimer() CLASS Apu
	if ::cycle % 2 == 0
		::pulse1:stepTimer()
		::pulse2:stepTimer()
		::noise:stepTimer()
		::dmc:stepTimer()
	end if
	::triangle:stepTimer()

METHOD stepEnvelope() CLASS Apu
	::pulse1:stepEnvelope()
	::pulse2:stepEnvelope()
	::triangle:stepCounter()
	::noise:stepEnvelope()

METHOD stepSweep() CLASS Apu
	::pulse1:stepSweep()
	::pulse2:stepSweep()

METHOD stepLength() CLASS Apu
	::pulse1:stepLength()
	::pulse2:stepLength()
	::triangle:stepLength()
	::noise:stepLength()

METHOD fireIRQ() CLASS Apu
	if ::frameIRQ
		::console:CPU:triggerIRQ()
	end if

METHOD readRegister(address) CLASS Apu
	do case
   	case address=0x4015
	   	return ::readStatus()
		   // default:
   		// 	log.Fatalf("unhandled apu register read at address: 0x%04X", address)
	end case
	return 0

METHOD writeRegister(address, value) CLASS Apu
	do case
   	case address=0x4000
   		::pulse1:writeControl(value)
   	case address=0x4001
   		::pulse1:writeSweep(value)
   	case address=0x4002
   		::pulse1:writeTimerLow(value)
   	case address=0x4003
   		::pulse1:writeTimerHigh(value)
   	case address=0x4004
   		::pulse2:writeControl(value)
   	case address=0x4005
   		::pulse2:writeSweep(value)
   	case address=0x4006
   		::pulse2:writeTimerLow(value)
   	case address=0x4007
   		::pulse2:writeTimerHigh(value)
   	case address=0x4008
   		::triangle:writeControl(value)
   	case address=0x4009
   	case address=0x4010
   		::dmc:writeControl(value)
   	case address=0x4011
   		::dmc:writeValue(value)
   	case address=0x4012
   		::dmc:writeAddress(value)
   	case address=0x4013
   		::dmc:writeLength(value)
   	case address=0x400A
   		::triangle:writeTimerLow(value)
   	case address=0x400B
   		::triangle:writeTimerHigh(value)
   	case address=0x400C
   		::noise:writeControl(value)
   	case address=0x400D
   	case address=0x400E
   		::noise:writePeriod(value)
   	case address=0x400F
   		::noise:writeLength(value)
   	case address=0x4015
   		::writeControl(value)
   	case address=0x4017
   		::writeFrameCounter(value)
   		// default:
   		// 	log.Fatalf("unhandled apu register write at address: 0x%04X", address)
	end case

METHOD readStatus() CLASS Apu
	local result
	if ::pulse1:lengthValue > 0
		result = (result | 1)
	end if
	if ::pulse2:lengthValue > 0
		result = (result | 2)
	end if
	if ::triangle:lengthValue > 0
		result = (result | 4)
	end if
	if ::noise:lengthValue > 0
		result = (result | 8)
	end if
	if ::dmc:currentLength > 0
		result = (result | 16)
	end if
	return result

METHOD writeControl(value) CLASS Apu
	::pulse1:enabled = (value & 1) == 1
	::pulse2:enabled = (value & 2) == 2
	::triangle:enabled = (value & 4) == 4
	::noise:enabled = (value & 8) == 8
	::dmc:enabled = (value & 16) == 16
	if !::pulse1:enabled
		::pulse1:lengthValue = 0
	end if
	if !::pulse2:enabled
		::pulse2:lengthValue = 0
	end if
	if !::triangle:enabled
		::triangle:lengthValue = 0
	end if
	if !::noise:enabled
		::noise:lengthValue = 0
	end if
	if !::dmc:enabled
		::dmc:currentLength = 0
	else
		if ::dmc:currentLength == 0
			::dmc:restart()
		end if
	end if

METHOD writeFrameCounter(value) CLASS Apu
	::framePeriod = ((4 + (value>>7)) & 1)
	::frameIRQ = ((value>>6) & 1) == 0
	// ::frameValue = 0
	if ::framePeriod == 5
		::stepEnvelope()
		::stepSweep()
		::stepLength()
	end if

// Pulse

CREATE CLASS apuPulse
	VAR enabled         INIT .f.
	VAR channel         INIT 0
	VAR lengthEnabled   INIT .f.
	VAR lengthValue     INIT 0
	VAR timerPeriod     INIT 0
	VAR timerValue      INIT 0
	VAR dutyMode        INIT 0
	VAR dutyValue       INIT 0
	VAR sweepReload     INIT .f.
	VAR sweepEnabled    INIT .f.
	VAR sweepNegate     INIT .f.
	VAR sweepShift      INIT 0
	VAR sweepPeriod     INIT 0
	VAR sweepValue      INIT 0
	VAR envelopeEnabled INIT .f.
	VAR envelopeLoop    INIT .f.
	VAR envelopeStart   INIT .f.
	VAR envelopePeriod  INIT 0
	VAR envelopeValue   INIT 0
	VAR envelopeVolume  INIT 0
	VAR constantVolume  INIT 0

   METHOD New()
   METHOD writeControl(value)
   METHOD writeSweep(value)
   METHOD writeTimerLow(value)
   METHOD writeTimerHigh(value)
   METHOD stepTimer()
   METHOD stepEnvelope()
   METHOD stepSweep()
   METHOD stepLength()
   METHOD sweep()
   METHOD output()
   
END CLASS

METHOD New() CLASS apuPulse
RETURN Self

METHOD writeControl(value) CLASS apuPulse
	::dutyMode = ((value >> 6) & 3)
	::lengthEnabled = ((value >> 5) & 1) == 0
	::envelopeLoop = ((value >> 5) & 1) == 1
	::envelopeEnabled = ((value >> 4) & 1) == 0
	::envelopePeriod = (value & 15)
	::constantVolume = (value & 15)
	::envelopeStart = .t.

METHOD writeSweep(value) CLASS apuPulse
	::sweepEnabled = ((value>>7) & 1) == 1
	::sweepPeriod = ((value>>4) & 7) + 1
	::sweepNegate = ((value>>3) & 1) == 1
	::sweepShift = (value & 7)
	::sweepReload = .t.

METHOD writeTimerLow(value) CLASS apuPulse
	::timerPeriod = ((::timerPeriod & 0xFF00) | (value & 0xFFFF))

METHOD writeTimerHigh(value) CLASS apuPulse
	::lengthValue = lengthTable[(value >> 3)+1]
	::timerPeriod = ((::timerPeriod & 0x00FF) | (((value & 7) & 0xFFFF) << 8))
	::envelopeStart = .t.
	::dutyValue = 0

METHOD stepTimer() CLASS apuPulse
	if ::timerValue == 0
		::timerValue = ::timerPeriod
		::dutyValue = (::dutyValue + 1) % 8
	else
		::timerValue--
	end if

METHOD stepEnvelope() CLASS apuPulse
	if ::envelopeStart
		::envelopeVolume = 15
		::envelopeValue = ::envelopePeriod
		::envelopeStart = .f.
	elseif ::envelopeValue > 0
		::envelopeValue--
	else
		if ::envelopeVolume > 0
			::envelopeVolume--
		elseif ::envelopeLoop
			::envelopeVolume = 15
		end if
		::envelopeValue = ::envelopePeriod
	end if

METHOD stepSweep() CLASS apuPulse
	if ::sweepReload
		if ::sweepEnabled .and. ::sweepValue == 0
			::sweep()
		end if
		::sweepValue = ::sweepPeriod
		::sweepReload = .f.
	elseif ::sweepValue > 0
		::sweepValue--
	else
		if ::sweepEnabled
			::sweep()
		end if
		::sweepValue = ::sweepPeriod
	end if

METHOD stepLength() CLASS apuPulse
	if ::lengthEnabled .and. ::lengthValue > 0
		::lengthValue--
	end if

METHOD sweep() CLASS apuPulse
	delta := (::timerPeriod >> ::sweepShift)
	if ::sweepNegate
		::timerPeriod -= delta
		if ::channel == 1
			::timerPeriod--
		end if
	else
		::timerPeriod += delta
	end if

METHOD output() CLASS apuPulse
	if !::enabled
		return 0
	end if
	if ::lengthValue == 0
		return 0
	end if
	if dutyTable[::dutyMode+1][::dutyValue+1] == 0
		return 0
	end if
	if ::timerPeriod < 8 .or. ::timerPeriod > 0x7FF
		return 0
	end if
	// if !::sweepNegate .and. ::timerPeriod+(::timerPeriod>>::sweepShift) > 0x7FF {
	// 	return 0
	// }
	if ::envelopeEnabled
		return ::envelopeVolume
	else
		return ::constantVolume
	end if

// Triangle

CREATE CLASS apuTriangle
	VAR enabled       INIT .f.
	VAR lengthEnabled INIT .f.
	VAR lengthValue   INIT 0
	VAR timerPeriod   INIT 0
	VAR timerValue    INIT 0
	VAR dutyValue     INIT 0
	VAR counterPeriod INIT 0
	VAR counterValue  INIT 0
	VAR counterReload INIT .f.

   METHOD New()
   METHOD writeControl(value)
   METHOD writeTimerLow(value)
   METHOD writeTimerHigh(value)
   METHOD stepTimer()
   METHOD stepLength()
   METHOD stepCounter()
   METHOD output()
END CLASS

METHOD New() CLASS apuTriangle
RETURN Self

METHOD writeControl(value) CLASS apuTriangle
	::lengthEnabled = ((value>>7) & 1) == 0
	::counterPeriod = (value & 0x7F)

METHOD writeTimerLow(value) CLASS apuTriangle
	::timerPeriod = ((::timerPeriod & 0xFF00) | (value & 0xFFFF))

METHOD writeTimerHigh(value) CLASS apuTriangle
	::lengthValue = lengthTable[((value >> 3) & 0xFF)+1]
	::timerPeriod = ((::timerPeriod & 0x00FF) | (((value & 0xFFFF) & 7) << 8))
	::timerValue = ::timerPeriod
	::counterReload = .t.

METHOD stepTimer() CLASS apuTriangle
	if ::timerValue == 0
		::timerValue = ::timerPeriod
		if ::lengthValue > 0 .and. ::counterValue > 0
			::dutyValue = (::dutyValue + 1) % 32
		end if
	else
		::timerValue--
	end if

METHOD stepLength() CLASS apuTriangle
	if ::lengthEnabled .and. ::lengthValue > 0
		::lengthValue--
	end if

METHOD stepCounter() CLASS apuTriangle
	if ::counterReload
		::counterValue = ::counterPeriod
	elseif ::counterValue > 0
		::counterValue--
	end if
	if ::lengthEnabled
		::counterReload = .f.
	end if

METHOD output() CLASS apuTriangle
	if !::enabled
		return 0
	end if
	if ::lengthValue == 0
		return 0
	end if
	if ::counterValue == 0
		return 0
	end if
	return triangleTable[::dutyValue+1]

// Noise

CREATE CLASS apuNoise 
	VAR enabled         INIT .f.
	VAR mode            INIT .f.
	VAR shiftRegister   INIT 0
	VAR lengthEnabled   INIT .f.
	VAR lengthValue     INIT 0
	VAR timerPeriod     INIT 0
	VAR timerValue      INIT 0
	VAR envelopeEnabled INIT .f.
	VAR envelopeLoop    INIT .f.
	VAR envelopeStart   INIT .f.
	VAR envelopePeriod  INIT 0
	VAR envelopeValue   INIT 0
	VAR envelopeVolume  INIT 0
	VAR constantVolume  INIT 0
	
	METHOD New()
   METHOD writeControl(value)
   METHOD writePeriod(value)
   METHOD writeLength(value)
   METHOD stepTimer()
   METHOD stepEnvelope()
   METHOD stepLength()
   METHOD output()

END CLASS

METHOD New() CLASS apuNoise
RETURN Self

METHOD writeControl(value) CLASS apuNoise
	::lengthEnabled = ((value >> 5) & 1) == 0
	::envelopeLoop = ((value >> 5) & 1) == 1
	::envelopeEnabled = ((value >> 4) & 1) == 0
	::envelopePeriod = (value & 15)
	::constantVolume = (value & 15)
	::envelopeStart = .t.

METHOD writePeriod(value) CLASS apuNoise
	::mode = (value & 0x80) == 0x80
	::timerPeriod = noiseTable[(value & 0x0F)+1]

METHOD writeLength(value) CLASS apuNoise
	::lengthValue = lengthTable[(value >> 3)+1]
	::envelopeStart = .t.

METHOD stepTimer() CLASS apuNoise
	if ::timerValue == 0
		::timerValue = ::timerPeriod

		if ::mode
			shift = 6
		else
			shift = 1
		end if
		b1 := (::shiftRegister & 1)
		b2 := ((::shiftRegister >> shift) & 1)
		::shiftRegister =  (::shiftRegister >> 1)
		::shiftRegister = (::shiftRegister | ((b1 ^^ b2) << 14))
	else
		::timerValue--
	end if

METHOD stepEnvelope() CLASS apuNoise
	if ::envelopeStart
		::envelopeVolume = 15
		::envelopeValue = ::envelopePeriod
		::envelopeStart = .f.
	elseif ::envelopeValue > 0
		::envelopeValue--
	else
		if ::envelopeVolume > 0
			::envelopeVolume--
		elseif ::envelopeLoop
			::envelopeVolume = 15
		end if
		::envelopeValue = ::envelopePeriod
	end if

METHOD stepLength() CLASS apuNoise
	if ::lengthEnabled .and. ::lengthValue > 0
		::lengthValue--
	end if

METHOD output() CLASS apuNoise
	if !::enabled
		return 0
	end if
	if ::lengthValue == 0
		return 0
	end if
	if (::shiftRegister & 1) == 1
		return 0
	end if
	if ::envelopeEnabled
		return ::envelopeVolume
	else
		return ::constantVolume
	end if

// DMC

CREATE CLASS DMC
	VAR cpu            
	VAR enabled        INIT .f.
	VAR value          INIT 0
	VAR sampleAddress  INIT 0
	VAR sampleLength   INIT 0
	VAR currentAddress INIT 0
	VAR currentLength  INIT 0
	VAR shiftRegister  INIT 0
	VAR bitCount       INIT 0
	VAR tickPeriod     INIT 0
	VAR tickValue      INIT 0
	VAR loop           INIT .f.
	VAR irq            INIT .f.

   METHOD New(console)
   METHOD writeControl(value)
   METHOD writeValue(value)
   METHOD writeAddress(value)
   METHOD writeLength(value)
   METHOD restart()
   METHOD stepTimer()
   METHOD stepReader()
   METHOD stepShifter()
   METHOD output()

END CLASS

METHOD New(console) CLASS Dmc
   ::CPU=console:cpu
Return Self

METHOD writeControl(value) CLASS Dmc
	::irq        = (value & 0x80) == 0x80
	::loop       = (value & 0x40) == 0x40
	::tickPeriod = dmcTable[(value & 0x0F)+1]

METHOD writeValue(value) CLASS Dmc
	::value = (value & 0x7F)

METHOD writeAddress(value) CLASS Dmc
	// Sample address = %11AAAAAA.AA000000
	::sampleAddress = (0xC000 | ((value & 0xFFFF) << 6))

METHOD writeLength(value) CLASS Dmc
	// Sample length = %0000LLLL.LLLL0001
	::sampleLength = (((value & 0xFFFF) << 4) | 1)

METHOD restart() CLASS Dmc
	::currentAddress = ::sampleAddress
	::currentLength = ::sampleLength

METHOD stepTimer() CLASS Dmc
	if !::enabled
		return
	end if
	::stepReader()
	if ::tickValue == 0
		::tickValue = ::tickPeriod
		::stepShifter()
	else
		::tickValue--
	end if

METHOD stepReader() CLASS Dmc
	if ::currentLength > 0 .and. ::bitCount == 0
		::cpu:stall += 4
		::shiftRegister = ::cpu:Read(::currentAddress)
		::bitCount = 8
		::currentAddress++
		if ::currentAddress == 0
			::currentAddress = 0x8000
		end if
		::currentLength--
		if ::currentLength == 0 .and. ::loop
			::restart()
		end if
	end if

METHOD stepShifter() CLASS Dmc
	if ::bitCount == 0
		return
	end if
	if (::shiftRegister & 1) == 1
		if ::value <= 125
			::value += 2
		end if
	else
		if ::value >= 2
			::value -= 2
		end if
	end if
	::shiftRegister = (::shiftRegister >> 1)
	::bitCount--

METHOD output() CLASS Dmc
	return ::value

