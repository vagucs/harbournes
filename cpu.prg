//#define XHB_BITOP // Habilita das operações | & ^^

#include "xhb.ch"
#include "common.ch"
#include "hbclass.ch"

#translate ( <exp1> | <exp2> )      => ( hb_qbitOr( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> & <exp2> )      => ( hb_qbitAnd( ( <exp1> ), ( <exp2> ) ) )
#translate ( <exp1> ^^ <exp2> )     => ( hb_qbitXor( ( <exp1> ), ( <exp2> ) ) )


Static nH:=0

Static instructionModes := {;
                            6 , 7, 6, 7, 11, 11, 11, 11, 6, 5, 4, 5, 1, 1, 1, 1,;
                            10, 9, 6, 9, 12, 12, 12, 12, 6, 3, 6, 3, 2, 2, 2, 2,;
                            1 , 7, 6, 7, 11, 11, 11, 11, 6, 5, 4, 5, 1, 1, 1, 1,;
                            10, 9, 6, 9, 12, 12, 12, 12, 6, 3, 6, 3, 2, 2, 2, 2,;
                            6 , 7, 6, 7, 11, 11, 11, 11, 6, 5, 4, 5, 1, 1, 1, 1,;
                            10, 9, 6, 9, 12, 12, 12, 12, 6, 3, 6, 3, 2, 2, 2, 2,;
                            6 , 7, 6, 7, 11, 11, 11, 11, 6, 5, 4, 5, 8, 1, 1, 1,;
                            10, 9, 6, 9, 12, 12, 12, 12, 6, 3, 6, 3, 2, 2, 2, 2,;
                            5 , 7, 5, 7, 11, 11, 11, 11, 6, 5, 6, 5, 1, 1, 1, 1,;
                            10, 9, 6, 9, 12, 12, 13, 13, 6, 3, 6, 3, 2, 2, 3, 3,;
                            5 , 7, 5, 7, 11, 11, 11, 11, 6, 5, 6, 5, 1, 1, 1, 1,;
                            10, 9, 6, 9, 12, 12, 13, 13, 6, 3, 6, 3, 2, 2, 3, 3,;
                            5 , 7, 5, 7, 11, 11, 11, 11, 6, 5, 6, 5, 1, 1, 1, 1,;
                            10, 9, 6, 9, 12, 12, 12, 12, 6, 3, 6, 3, 2, 2, 2, 2,;
                            5 , 7, 5, 7, 11, 11, 11, 11, 6, 5, 6, 5, 1, 1, 1, 1,;
                            10, 9, 6, 9, 12, 12, 12, 12, 6, 3, 6, 3, 2, 2, 2, 2;
                           }
 

// instructionSizes indicates the size of each instruction in bytes
Static instructionSizes := {;
                            2, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0,;
                            2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 3, 3, 3, 0,;
                            3, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0,;
                            2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 3, 3, 3, 0,;
                            1, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0,;
                            2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 3, 3, 3, 0,;
                            1, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0,;
                            2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 3, 3, 3, 0,;
                            2, 2, 0, 0, 2, 2, 2, 0, 1, 0, 1, 0, 3, 3, 3, 0,;
                            2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 0, 3, 0, 0,;
                            2, 2, 2, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0,;
                            2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 3, 3, 3, 0,;
                            2, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0,;
                            2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 3, 3, 3, 0,;
                            2, 2, 0, 0, 2, 2, 2, 0, 1, 2, 1, 0, 3, 3, 3, 0,;
                            2, 2, 0, 0, 2, 2, 2, 0, 1, 3, 1, 0, 3, 3, 3, 0;
                           }

// instructionCycles indicates the number of cycles used by each instruction,
// not including conditional cycles
Static instructionCycles := {;
                             7, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 4, 4, 6, 6,;
                             2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,;
                             6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 4, 4, 6, 6,;
                             2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,;
                             6, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 3, 4, 6, 6,;
                             2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,;
                             6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 5, 4, 6, 6,;
                             2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,;
                             2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4,;
                             2, 6, 2, 6, 4, 4, 4, 4, 2, 5, 2, 5, 5, 5, 5, 5,;
                             2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4,;
                             2, 5, 2, 5, 4, 4, 4, 4, 2, 4, 2, 4, 4, 4, 4, 4,;
                             2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6,;
                             2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,;
                             2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6,;
                             2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7;
                            }

// instructionPageCycles indicates the number of cycles used by each
// instruction when a page is crossed
Static instructionPageCycles := {;
                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,;
                                 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0,;
                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,;
                                 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0,;
                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,;
                                 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0,;
                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,;
                                 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0,;
                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,;
                                 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,;
                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,;
                                 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1,;
                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,;
                                 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0,;
                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,;
                                 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0;
                                }

// instructionNames indicates the name of each instruction
Static instructionNames := {;
                            "BRK", "ORA", "KIL", "SLO", "NOP", "ORA", "ASL", "SLO",;
                            "PHP", "ORA", "ASL", "ANC", "NOP", "ORA", "ASL", "SLO",;
                            "BPL", "ORA", "KIL", "SLO", "NOP", "ORA", "ASL", "SLO",;
                            "CLC", "ORA", "NOP", "SLO", "NOP", "ORA", "ASL", "SLO",;
                            "JSR", "AND", "KIL", "RLA", "BIT", "AND", "ROL", "RLA",;
                            "PLP", "AND", "ROL", "ANC", "BIT", "AND", "ROL", "RLA",;
                            "BMI", "AND", "KIL", "RLA", "NOP", "AND", "ROL", "RLA",;
                            "SEC", "AND", "NOP", "RLA", "NOP", "AND", "ROL", "RLA",;
                            "RTI", "EOR", "KIL", "SRE", "NOP", "EOR", "LSR", "SRE",;
                            "PHA", "EOR", "LSR", "ALR", "JMP", "EOR", "LSR", "SRE",;
                            "BVC", "EOR", "KIL", "SRE", "NOP", "EOR", "LSR", "SRE",;
                            "CLI", "EOR", "NOP", "SRE", "NOP", "EOR", "LSR", "SRE",;
                            "RTS", "ADC", "KIL", "RRA", "NOP", "ADC", "ROR", "RRA",;
                            "PLA", "ADC", "ROR", "ARR", "JMP", "ADC", "ROR", "RRA",;
                            "BVS", "ADC", "KIL", "RRA", "NOP", "ADC", "ROR", "RRA",;
                            "SEI", "ADC", "NOP", "RRA", "NOP", "ADC", "ROR", "RRA",;
                            "NOP", "STA", "NOP", "SAX", "STY", "STA", "STX", "SAX",;
                            "DEY", "NOP", "TXA", "XAA", "STY", "STA", "STX", "SAX",;
                            "BCC", "STA", "KIL", "AHX", "STY", "STA", "STX", "SAX",;
                            "TYA", "STA", "TXS", "TAS", "SHY", "STA", "SHX", "AHX",;
                            "LDY", "LDA", "LDX", "LAX", "LDY", "LDA", "LDX", "LAX",;
                            "TAY", "LDA", "TAX", "LAX", "LDY", "LDA", "LDX", "LAX",;
                            "BCS", "LDA", "KIL", "LAX", "LDY", "LDA", "LDX", "LAX",;
                            "CLV", "LDA", "TSX", "LAS", "LDY", "LDA", "LDX", "LAX",;
                            "CPY", "CMP", "NOP", "DCP", "CPY", "CMP", "DEC", "DCP",;
                            "INY", "CMP", "DEX", "AXS", "CPY", "CMP", "DEC", "DCP",;
                            "BNE", "CMP", "KIL", "DCP", "NOP", "CMP", "DEC", "DCP",;
                            "CLD", "CMP", "NOP", "DCP", "NOP", "CMP", "DEC", "DCP",;
                            "CPX", "SBC", "NOP", "ISC", "CPX", "SBC", "INC", "ISC",;
                            "INX", "SBC", "NOP", "SBC", "CPX", "SBC", "INC", "ISC",;
                            "BEQ", "SBC", "KIL", "ISC", "NOP", "SBC", "INC", "ISC",;
                            "SED", "SBC", "NOP", "ISC", "NOP", "SBC", "INC", "ISC";
                           }

CREATE CLASS CPU
   VAR Memory
   VAR Cycles    INIT 0 // number of cycles
   VAR PC        INIT 0 // program counter
   VAR SP        INIT 0 // stack pointer
   VAR A         INIT 0 // accumulator
   VAR X         INIT 0 // x register
   VAR Y         INIT 0 // y register
   VAR C         INIT 0 // carry flag
   VAR Z         INIT 0 // zero flag
   VAR I         INIT 0 // interrupt disable flag
   VAR D         INIT 0 // decimal mode flag
   VAR B         INIT 0 // break command flag
   VAR U         INIT 0 // unused flag
   VAR V         INIT 0 // overflow flag
   VAR N         INIT 0 // negative flag
   VAR interrupt INIT 0 // interrupt type to perform
   VAR stall     INIT 0 // number of cycles to stall
   VAR table     INIT {}
   VAR cOphash     INIT {=>}
   
   METHOD New(console)
   METHOD Reset()
   METHOD PrintInstruction()
   METHOD PrintReg()
   METHOD addBranchCycles(info)
   METHOD compare(a, b)
   METHOD Read16(address)
   METHOD read16bug(address)
   METHOD push(value)
   METHOD pull()
   METHOD push16(value)
   METHOD pull16()
   METHOD Flags()
   METHOD SetFlags(flags)
   METHOD setZ(value)
   METHOD setN(value)
   METHOD setZN(value)
   METHOD triggerNMI()
   METHOD triggerIRQ()
   METHOD Step()
   METHOD nmi()
   METHOD irq()
   METHOD adc(info)
   METHOD and(info)
   METHOD asl(info)
   METHOD bcc(info)
   METHOD bcs(info)
   METHOD beq(info)
   METHOD bit(info)
   METHOD bmi(info)
   METHOD bne(info)
   METHOD bpl(info)
   METHOD brk(info)
   METHOD bvc(info)
   METHOD bvs(info)
   METHOD clc(info)
   METHOD cld(info)
   METHOD cli(info)
   METHOD clv(info)
   METHOD cmp(info)
   METHOD cpx(info)
   METHOD cpy(info)
   METHOD dec(info)
   METHOD dex(info)
   METHOD dey(info)
   METHOD eor(info)
   METHOD inc(info)
   METHOD inx(info)
   METHOD iny(info)
   METHOD jmp(info)
   METHOD jsr(info)
   METHOD lda(info)
   METHOD ldx(info)
   METHOD ldy(info)
   METHOD lsr(info)
   METHOD nop(info)
   METHOD ora(info)
   METHOD pha(info)
   METHOD php(info)
   METHOD pla(info)
   METHOD plp(info)
   METHOD rol(info)
   METHOD ror(info)
   METHOD rti(info)
   METHOD rts(info)
   METHOD sbc(info)
   METHOD sec(info)
   METHOD sed(info)
   METHOD sei(info)
   METHOD sta(info)
   METHOD stx(info)
   METHOD sty(info)
   METHOD tax(info)
   METHOD tay(info)
   METHOD tsx(info)
   METHOD txa(info)
   METHOD txs(info)
   METHOD tya(info)
   METHOD ahx(info)
   METHOD alr(info)
   METHOD anc(info)
   METHOD arr(info)
   METHOD axs(info)
   METHOD dcp(info)
   METHOD isc(info)
   METHOD kil(info)
   METHOD las(info)
   METHOD lax(info)
   METHOD rla(info)
   METHOD rra(info)
   METHOD sax(info)
   METHOD shx(info)
   METHOD shy(info)
   METHOD slo(info)
   METHOD sre(info)
   METHOD tas(info)
   METHOD xaa(info)
END CLASS

METHOD New(console) CLASS CPU
   ::Memory= Memory():New(@console)
   ::Reset()
   ::table = {;
              {|x|::brk(x)}, {|x|::ora(x)}, {|x|::kil(x)}, {|x|::slo(x)}, {|x|::nop(x)}, {|x|::ora(x)}, {|x|::asl(x)}, {|x|::slo(x)},;
              {|x|::php(x)}, {|x|::ora(x)}, {|x|::asl(x)}, {|x|::anc(x)}, {|x|::nop(x)}, {|x|::ora(x)}, {|x|::asl(x)}, {|x|::slo(x)},;
              {|x|::bpl(x)}, {|x|::ora(x)}, {|x|::kil(x)}, {|x|::slo(x)}, {|x|::nop(x)}, {|x|::ora(x)}, {|x|::asl(x)}, {|x|::slo(x)},;
              {|x|::clc(x)}, {|x|::ora(x)}, {|x|::nop(x)}, {|x|::slo(x)}, {|x|::nop(x)}, {|x|::ora(x)}, {|x|::asl(x)}, {|x|::slo(x)},;
              {|x|::jsr(x)}, {|x|::and(x)}, {|x|::kil(x)}, {|x|::rla(x)}, {|x|::bit(x)}, {|x|::and(x)}, {|x|::rol(x)}, {|x|::rla(x)},;
              {|x|::plp(x)}, {|x|::and(x)}, {|x|::rol(x)}, {|x|::anc(x)}, {|x|::bit(x)}, {|x|::and(x)}, {|x|::rol(x)}, {|x|::rla(x)},;
              {|x|::bmi(x)}, {|x|::and(x)}, {|x|::kil(x)}, {|x|::rla(x)}, {|x|::nop(x)}, {|x|::and(x)}, {|x|::rol(x)}, {|x|::rla(x)},;
              {|x|::sec(x)}, {|x|::and(x)}, {|x|::nop(x)}, {|x|::rla(x)}, {|x|::nop(x)}, {|x|::and(x)}, {|x|::rol(x)}, {|x|::rla(x)},;
              {|x|::rti(x)}, {|x|::eor(x)}, {|x|::kil(x)}, {|x|::sre(x)}, {|x|::nop(x)}, {|x|::eor(x)}, {|x|::lsr(x)}, {|x|::sre(x)},;
              {|x|::pha(x)}, {|x|::eor(x)}, {|x|::lsr(x)}, {|x|::alr(x)}, {|x|::jmp(x)}, {|x|::eor(x)}, {|x|::lsr(x)}, {|x|::sre(x)},;
              {|x|::bvc(x)}, {|x|::eor(x)}, {|x|::kil(x)}, {|x|::sre(x)}, {|x|::nop(x)}, {|x|::eor(x)}, {|x|::lsr(x)}, {|x|::sre(x)},;
              {|x|::cli(x)}, {|x|::eor(x)}, {|x|::nop(x)}, {|x|::sre(x)}, {|x|::nop(x)}, {|x|::eor(x)}, {|x|::lsr(x)}, {|x|::sre(x)},;
              {|x|::rts(x)}, {|x|::adc(x)}, {|x|::kil(x)}, {|x|::rra(x)}, {|x|::nop(x)}, {|x|::adc(x)}, {|x|::ror(x)}, {|x|::rra(x)},;
              {|x|::pla(x)}, {|x|::adc(x)}, {|x|::ror(x)}, {|x|::arr(x)}, {|x|::jmp(x)}, {|x|::adc(x)}, {|x|::ror(x)}, {|x|::rra(x)},;
              {|x|::bvs(x)}, {|x|::adc(x)}, {|x|::kil(x)}, {|x|::rra(x)}, {|x|::nop(x)}, {|x|::adc(x)}, {|x|::ror(x)}, {|x|::rra(x)},;
              {|x|::sei(x)}, {|x|::adc(x)}, {|x|::nop(x)}, {|x|::rra(x)}, {|x|::nop(x)}, {|x|::adc(x)}, {|x|::ror(x)}, {|x|::rra(x)},;
              {|x|::nop(x)}, {|x|::sta(x)}, {|x|::nop(x)}, {|x|::sax(x)}, {|x|::sty(x)}, {|x|::sta(x)}, {|x|::stx(x)}, {|x|::sax(x)},;
              {|x|::dey(x)}, {|x|::nop(x)}, {|x|::txa(x)}, {|x|::xaa(x)}, {|x|::sty(x)}, {|x|::sta(x)}, {|x|::stx(x)}, {|x|::sax(x)},;
              {|x|::bcc(x)}, {|x|::sta(x)}, {|x|::kil(x)}, {|x|::ahx(x)}, {|x|::sty(x)}, {|x|::sta(x)}, {|x|::stx(x)}, {|x|::sax(x)},;
              {|x|::tya(x)}, {|x|::sta(x)}, {|x|::txs(x)}, {|x|::tas(x)}, {|x|::shy(x)}, {|x|::sta(x)}, {|x|::shx(x)}, {|x|::ahx(x)},;
              {|x|::ldy(x)}, {|x|::lda(x)}, {|x|::ldx(x)}, {|x|::lax(x)}, {|x|::ldy(x)}, {|x|::lda(x)}, {|x|::ldx(x)}, {|x|::lax(x)},;
              {|x|::tay(x)}, {|x|::lda(x)}, {|x|::tax(x)}, {|x|::lax(x)}, {|x|::ldy(x)}, {|x|::lda(x)}, {|x|::ldx(x)}, {|x|::lax(x)},;
              {|x|::bcs(x)}, {|x|::lda(x)}, {|x|::kil(x)}, {|x|::lax(x)}, {|x|::ldy(x)}, {|x|::lda(x)}, {|x|::ldx(x)}, {|x|::lax(x)},;
              {|x|::clv(x)}, {|x|::lda(x)}, {|x|::tsx(x)}, {|x|::las(x)}, {|x|::ldy(x)}, {|x|::lda(x)}, {|x|::ldx(x)}, {|x|::lax(x)},;
              {|x|::cpy(x)}, {|x|::cmp(x)}, {|x|::nop(x)}, {|x|::dcp(x)}, {|x|::cpy(x)}, {|x|::cmp(x)}, {|x|::dec(x)}, {|x|::dcp(x)},;
              {|x|::iny(x)}, {|x|::cmp(x)}, {|x|::dex(x)}, {|x|::axs(x)}, {|x|::cpy(x)}, {|x|::cmp(x)}, {|x|::dec(x)}, {|x|::dcp(x)},;
              {|x|::bne(x)}, {|x|::cmp(x)}, {|x|::kil(x)}, {|x|::dcp(x)}, {|x|::nop(x)}, {|x|::cmp(x)}, {|x|::dec(x)}, {|x|::dcp(x)},;
              {|x|::cld(x)}, {|x|::cmp(x)}, {|x|::nop(x)}, {|x|::dcp(x)}, {|x|::nop(x)}, {|x|::cmp(x)}, {|x|::dec(x)}, {|x|::dcp(x)},;
              {|x|::cpx(x)}, {|x|::sbc(x)}, {|x|::nop(x)}, {|x|::isc(x)}, {|x|::cpx(x)}, {|x|::sbc(x)}, {|x|::inc(x)}, {|x|::isc(x)},;
              {|x|::inx(x)}, {|x|::sbc(x)}, {|x|::nop(x)}, {|x|::sbc(x)}, {|x|::cpx(x)}, {|x|::sbc(x)}, {|x|::inc(x)}, {|x|::isc(x)},;
              {|x|::beq(x)}, {|x|::sbc(x)}, {|x|::kil(x)}, {|x|::isc(x)}, {|x|::nop(x)}, {|x|::sbc(x)}, {|x|::inc(x)}, {|x|::isc(x)},;
              {|x|::sed(x)}, {|x|::sbc(x)}, {|x|::nop(x)}, {|x|::isc(x)}, {|x|::nop(x)}, {|x|::sbc(x)}, {|x|::inc(x)}, {|x|::isc(x)};
             }

   ::cOphash={0   => {|x|::brk(x)},1   => {|x|::ora(x)},2   => {|x|::kil(x)},3   => {|x|::slo(x)},4   => {|x|::nop(x)},5   => {|x|::ora(x)},6   => {|x|::asl(x)},7   => {|x|::slo(x)},8   => {|x|::php(x)},;
              9   => {|x|::ora(x)},10  => {|x|::asl(x)},11  => {|x|::anc(x)},12  => {|x|::nop(x)},13  => {|x|::ora(x)},14  => {|x|::asl(x)},15  => {|x|::slo(x)},16  => {|x|::bpl(x)},17  => {|x|::ora(x)},;
              18  => {|x|::kil(x)},19  => {|x|::slo(x)},20  => {|x|::nop(x)},21  => {|x|::ora(x)},22  => {|x|::asl(x)},23  => {|x|::slo(x)},24  => {|x|::clc(x)},25  => {|x|::ora(x)},26  => {|x|::nop(x)},;
              27  => {|x|::slo(x)},28  => {|x|::nop(x)},29  => {|x|::ora(x)},30  => {|x|::asl(x)},31  => {|x|::slo(x)},32  => {|x|::jsr(x)},33  => {|x|::and(x)},34  => {|x|::kil(x)},35  => {|x|::rla(x)},;
              36  => {|x|::bit(x)},37  => {|x|::and(x)},38  => {|x|::rol(x)},39  => {|x|::rla(x)},40  => {|x|::plp(x)},41  => {|x|::and(x)},42  => {|x|::rol(x)},43  => {|x|::anc(x)},44  => {|x|::bit(x)},;
              45  => {|x|::and(x)},46  => {|x|::rol(x)},47  => {|x|::rla(x)},48  => {|x|::bmi(x)},49  => {|x|::and(x)},50  => {|x|::kil(x)},51  => {|x|::rla(x)},52  => {|x|::nop(x)},53  => {|x|::and(x)},;
              54  => {|x|::rol(x)},55  => {|x|::rla(x)},56  => {|x|::sec(x)},57  => {|x|::and(x)},58  => {|x|::nop(x)},59  => {|x|::rla(x)},60  => {|x|::nop(x)},61  => {|x|::and(x)},62  => {|x|::rol(x)},;
              63  => {|x|::rla(x)},64  => {|x|::rti(x)},65  => {|x|::eor(x)},66  => {|x|::kil(x)},67  => {|x|::sre(x)},68  => {|x|::nop(x)},69  => {|x|::eor(x)},70  => {|x|::lsr(x)},71  => {|x|::sre(x)},;
              72  => {|x|::pha(x)},73  => {|x|::eor(x)},74  => {|x|::lsr(x)},75  => {|x|::alr(x)},76  => {|x|::jmp(x)},77  => {|x|::eor(x)},78  => {|x|::lsr(x)},79  => {|x|::sre(x)},80  => {|x|::bvc(x)},;
              81  => {|x|::eor(x)},82  => {|x|::kil(x)},83  => {|x|::sre(x)},84  => {|x|::nop(x)},85  => {|x|::eor(x)},86  => {|x|::lsr(x)},87  => {|x|::sre(x)},88  => {|x|::cli(x)},89  => {|x|::eor(x)},;
              90  => {|x|::nop(x)},91  => {|x|::sre(x)},92  => {|x|::nop(x)},93  => {|x|::eor(x)},94  => {|x|::lsr(x)},95  => {|x|::sre(x)},96  => {|x|::rts(x)},97  => {|x|::adc(x)},98  => {|x|::kil(x)},;
              99  => {|x|::rra(x)},100 => {|x|::nop(x)},101 => {|x|::adc(x)},102 => {|x|::ror(x)},103 => {|x|::rra(x)},104 => {|x|::pla(x)},105 => {|x|::adc(x)},106 => {|x|::ror(x)},107 => {|x|::arr(x)},;
              108 => {|x|::jmp(x)},109 => {|x|::adc(x)},110 => {|x|::ror(x)},111 => {|x|::rra(x)},112 => {|x|::bvs(x)},113 => {|x|::adc(x)},114 => {|x|::kil(x)},115 => {|x|::rra(x)},116 => {|x|::nop(x)},;
              117 => {|x|::adc(x)},118 => {|x|::ror(x)},119 => {|x|::rra(x)},120 => {|x|::sei(x)},121 => {|x|::adc(x)},122 => {|x|::nop(x)},123 => {|x|::rra(x)},124 => {|x|::nop(x)},125 => {|x|::adc(x)},;
              126 => {|x|::ror(x)},127 => {|x|::rra(x)},128 => {|x|::nop(x)},129 => {|x|::sta(x)},130 => {|x|::nop(x)},131 => {|x|::sax(x)},132 => {|x|::sty(x)},133 => {|x|::sta(x)},134 => {|x|::stx(x)},;
              135 => {|x|::sax(x)},136 => {|x|::dey(x)},137 => {|x|::nop(x)},138 => {|x|::txa(x)},139 => {|x|::xaa(x)},140 => {|x|::sty(x)},141 => {|x|::sta(x)},142 => {|x|::stx(x)},143 => {|x|::sax(x)},;
              144 => {|x|::bcc(x)},145 => {|x|::sta(x)},146 => {|x|::kil(x)},147 => {|x|::ahx(x)},148 => {|x|::sty(x)},149 => {|x|::sta(x)},150 => {|x|::stx(x)},151 => {|x|::sax(x)},152 => {|x|::tya(x)},;
              153 => {|x|::sta(x)},154 => {|x|::txs(x)},155 => {|x|::tas(x)},156 => {|x|::shy(x)},157 => {|x|::sta(x)},158 => {|x|::shx(x)},159 => {|x|::ahx(x)},160 => {|x|::ldy(x)},161 => {|x|::lda(x)},;
              162 => {|x|::ldx(x)},163 => {|x|::lax(x)},164 => {|x|::ldy(x)},165 => {|x|::lda(x)},166 => {|x|::ldx(x)},167 => {|x|::lax(x)},168 => {|x|::tay(x)},169 => {|x|::lda(x)},170 => {|x|::tax(x)},;
              171 => {|x|::lax(x)},172 => {|x|::ldy(x)},173 => {|x|::lda(x)},174 => {|x|::ldx(x)},175 => {|x|::lax(x)},176 => {|x|::bcs(x)},177 => {|x|::lda(x)},178 => {|x|::kil(x)},179 => {|x|::lax(x)},;
              180 => {|x|::ldy(x)},181 => {|x|::lda(x)},182 => {|x|::ldx(x)},183 => {|x|::lax(x)},184 => {|x|::clv(x)},185 => {|x|::lda(x)},186 => {|x|::tsx(x)},187 => {|x|::las(x)},188 => {|x|::ldy(x)},;
              189 => {|x|::lda(x)},190 => {|x|::ldx(x)},191 => {|x|::lax(x)},192 => {|x|::cpy(x)},193 => {|x|::cmp(x)},194 => {|x|::nop(x)},195 => {|x|::dcp(x)},196 => {|x|::cpy(x)},197 => {|x|::cmp(x)},;
              198 => {|x|::dec(x)},199 => {|x|::dcp(x)},200 => {|x|::iny(x)},201 => {|x|::cmp(x)},202 => {|x|::dex(x)},203 => {|x|::axs(x)},204 => {|x|::cpy(x)},205 => {|x|::cmp(x)},206 => {|x|::dec(x)},;
              207 => {|x|::dcp(x)},208 => {|x|::bne(x)},209 => {|x|::cmp(x)},210 => {|x|::kil(x)},211 => {|x|::dcp(x)},212 => {|x|::nop(x)},213 => {|x|::cmp(x)},214 => {|x|::dec(x)},215 => {|x|::dcp(x)},;
              216 => {|x|::cld(x)},217 => {|x|::cmp(x)},218 => {|x|::nop(x)},219 => {|x|::dcp(x)},220 => {|x|::nop(x)},221 => {|x|::cmp(x)},222 => {|x|::dec(x)},223 => {|x|::dcp(x)},224 => {|x|::cpx(x)},;
              225 => {|x|::sbc(x)},226 => {|x|::nop(x)},227 => {|x|::isc(x)},228 => {|x|::cpx(x)},229 => {|x|::sbc(x)},230 => {|x|::inc(x)},231 => {|x|::isc(x)},232 => {|x|::inx(x)},233 => {|x|::sbc(x)},;
              234 => {|x|::nop(x)},235 => {|x|::sbc(x)},236 => {|x|::cpx(x)},237 => {|x|::sbc(x)},238 => {|x|::inc(x)},239 => {|x|::isc(x)},240 => {|x|::beq(x)},241 => {|x|::sbc(x)},242 => {|x|::kil(x)},;
              243 => {|x|::isc(x)},244 => {|x|::nop(x)},245 => {|x|::sbc(x)},246 => {|x|::inc(x)},247 => {|x|::isc(x)},248 => {|x|::sed(x)},249 => {|x|::sbc(x)},250 => {|x|::nop(x)},251 => {|x|::isc(x)},;
              252 => {|x|::nop(x)},253 => {|x|::sbc(x)},254 => {|x|::inc(x)},255 => {|x|::isc(x)}}

   RETURN Self

// Reset resets the CPU to its initial powerup state
METHOD Reset() CLASS CPU
   ::PC = ::Read16(0xFFFC)
   ::SP = 0xFD
   ::SetFlags(0x24)

// PrintInstruction prints the current CPU state
METHOD PrintInstruction() CLASS CPU
   local opcode,bytes,name,w0,w1,w2,xInstruction
   opcode := ::Memory:Read(::PC)
   bytes := instructionSizes[opcode+1]
   name := instructionNames[opcode+1]
   w0 := hb_numtohex(::Memory:Read(::PC+0),2)
   w1 := hb_numtohex(::Memory:Read(::PC+1),2)
   w2 := hb_numtohex(::Memory:Read(::PC+2),2)
   if bytes < 2
      w1 = "  "
   end if
   if bytes < 3
      w2 = "  "
   end if
      
   xInstruction=space(5)
   if !empty(w2) .or. !empty(w1)
      xInstruction=padr("$"+alltrim(w2)+alltrim(w1),5," ")
   end if
      
   //scroll(16,0,maxrow(),59,+1)
   //@ maxrow(),0 say hb_numtohex(::PC,4)+" : "+w0+" : "+alltrim(name)+" "+xInstruction
   
   //nRow=row()
   //nCol=col()

   /*@  0,60 say "Cycles  : " + alltrim(str(::Cycles))
   @  1,60 say "PC      : " + hb_numtohex(::PC,4)+" ("+alltrim(str(::PC))+")   "
   @  2,60 say "SP      : " + hb_numtohex(::SP,4)+" ("+alltrim(str(::SP))+")   "
   @  3,60 say "A       : " + hb_numtohex(::A,2)+" ("+alltrim(str(::A))+")   "
   @  4,60 say "X       : " + hb_numtohex(::X,2)+" ("+alltrim(str(::X))+")   "
   @  5,60 say "Y       : " + hb_numtohex(::Y,2)+" ("+alltrim(str(::Y))+")   "
   @  7,60 say "C       : " + hb_numtohex(::C,2)+" ("+alltrim(str(::C))+")   "
   @  8,60 say "Z       : " + hb_numtohex(::Z,2)+" ("+alltrim(str(::Z))+")   "
   @  9,60 say "I       : " + hb_numtohex(::I,2)+" ("+alltrim(str(::I))+")   "
   @ 10,60 say "D       : " + hb_numtohex(::D,2)+" ("+alltrim(str(::D))+")   "
   @ 11,60 say "B       : " + hb_numtohex(::B,2)+" ("+alltrim(str(::B))+")   "
//   @ 11,60 say "U       : " + hb_numtohex(::U,2)+" ("+alltrim(str(::U))+")"
   @ 12,60 say "V       : " + hb_numtohex(::V,2)+" ("+alltrim(str(::V))+")   "
//   @ 13,60 say "N       : " + hb_numtohex(::N,2)+" ("+alltrim(str(::N))+")   "
   @ 13,60 say "Interupt: " + hb_numtohex(::interrupt,2)+" ("+alltrim(str(::interrupt))+")   "
//   @ 15,60 say "Stall   : " + alltrim(str(::stall))+" ("+alltrim(str(::PC))+")"
   
//   setpos(0,0)*/
   
   //," CPU - A:",hb_numtohex(::A,2),"X: ",hb_numtohex(::X,2),"Y: ",hb_numtohex(::Y,2)," F: ",hb_numtohex(::Flags(),1)," Sp:",hb_numtohex(::SP,4)," Cy:",(::Cycles*3) % 341
   
   clog(::PC," ",w0," ", w1," ", w2," ", name, " A:",::A," X: ", ::X," Y:", ::Y, " P:",::Flags(), " SP:",::SP, " CYC:",(::Cycles*3) % 341)

METHOD PrintReg() CLASS CPU

   @  0,70 say "Cycles  : " + alltrim(str(::Cycles))
   @  1,70 say "PC      : " + hb_numtohex(::PC,4)+" ("+alltrim(str(::PC))+")"
   @  2,70 say "SP      : " + hb_numtohex(::SP,4)+" ("+alltrim(str(::SP))+")"
   @  3,70 say "A       : " + hb_numtohex(::A,2)+" ("+alltrim(str(::A))+")"
   @  4,70 say "X       : " + hb_numtohex(::X,2)+" ("+alltrim(str(::X))+")"
   @  5,70 say "Y       : " + hb_numtohex(::Y,2)+" ("+alltrim(str(::Y))+")"
   @  6,70 say "C       : " + hb_numtohex(::C,2)+" ("+alltrim(str(::C))+")"
   @  7,70 say "Z       : " + hb_numtohex(::Z,2)+" ("+alltrim(str(::Z))+")"
   @  8,70 say "I       : " + hb_numtohex(::I,2)+" ("+alltrim(str(::I))+")"
   @  9,70 say "D       : " + hb_numtohex(::D,2)+" ("+alltrim(str(::D))+")"
   @ 10,70 say "B       : " + hb_numtohex(::B,2)+" ("+alltrim(str(::B))+")"
//   @ 11,70 say "U       : " + hb_numtohex(::U,2)+" ("+alltrim(str(::U))+")"
   @ 11,70 say "V       : " + hb_numtohex(::V,2)+" ("+alltrim(str(::V))+")"
   @ 12,70 say "N       : " + hb_numtohex(::N,2)+" ("+alltrim(str(::N))+")"
   @ 13,70 say "Interupt: " + hb_numtohex(::interrupt,2)+" ("+alltrim(str(::interrupt))+")"
//   @ 15,60 say "Stall   : " + alltrim(str(::stall))+" ("+alltrim(str(::PC))+")"
   
//   setpos(0,0)

// pagesDiffer returns .t. if the two addresses reference different pages
FUNCTION pagesDiffer(a, b) 
   return (a & 0xFF00) != (b & 0xFF00)

// addBranchCycles adds a cycle for taking a branch and adds another cycle
// if the branch jumps to a new page
METHOD addBranchCycles(info) CLASS CPU
   ::Cycles++
   if pagesDiffer(info[2], info[1])
      ::Cycles++
   end if

METHOD compare(a, b) CLASS CPU
   ::setZN(a - b)
   if a >= b
      ::C = 1
   else
      ::C = 0
   end if

// Read16 reads two bytes using Read to return a double-word value
METHOD Read16(address) CLASS CPU
   local lo,hi
   lo := ::Memory:Read(address)
   hi := ::Memory:Read(address + 1)
   return ((hi << 8) | lo)

// read16bug emulates a 6502 bug that caused the low byte to wrap without
// incrementing the high byte
METHOD read16bug(address) CLASS CPU
   local a,b,lo,hi
   a := address
   b := ((a & 0xFF00) | (a+1 & 0xFFFF))
   lo := ::Memory:Read(a)
   hi := ::Memory:Read(b)
   return ((hi << 8) | lo)

// push pushes a byte onto the stack
METHOD push(value) CLASS CPU
   ::Memory:Write( (0x100 | ::SP), value)
   ::SP--

// pull pops a byte from the stack
METHOD pull() CLASS CPU
   ::SP++
   return ::Memory:Read( (0x100 | (::SP)))

// push16 pushes two bytes onto the stack
METHOD push16(value) CLASS CPU
   local hi,lo
   hi := ((value >> 8) & 0xFF)
   lo := (value & 0xFF)
   ::push(hi)
   ::push(lo)

// pull16 pops two bytes from the stack
METHOD pull16() CLASS CPU
   local lo,hi
   lo := ::pull()
   hi := ::pull()
   return ((hi << 8) | lo)


// Flags returns the processor status flags
METHOD Flags() CLASS CPU
   local flags:=0
   flags = (flags | (::C << 0))
   flags = (flags | (::Z << 1))
   flags = (flags | (::I << 2))
   flags = (flags | (::D << 3))
   flags = (flags | (::B << 4))
   flags = (flags | (::U << 5))
   flags = (flags | (::V << 6))
   flags = (flags | (::N << 7))
   return flags

// SetFlags sets the processor status flags
METHOD SetFlags(flags) CLASS CPU
   ::C = ((flags >> 0) & 1)
   ::Z = ((flags >> 1) & 1)
   ::I = ((flags >> 2) & 1)
   ::D = ((flags >> 3) & 1)
   ::B = ((flags >> 4) & 1)
   ::U = ((flags >> 5) & 1)
   ::V = ((flags >> 6) & 1)
   ::N = ((flags >> 7) & 1)

// setZ sets the zero flag if the argument is zero
METHOD setZ(value) CLASS CPU
   if value = 0 .or. value>255
      ::Z = 1
   else
      ::Z = 0
   end if

// setN sets the negative flag if the argument is negative (high bit is set)
METHOD setN(value) CLASS CPU
   if(value & 0x80) != 0 //(value & 0x80) != 0  value<0 // 
      ::N = 1
   else
      ::N = 0
   end if

// setZN sets the zero flag and the negative flag
METHOD setZN(value) CLASS CPU
   ::setZ(value)
   ::setN(value)

// triggerNMI causes a non-maskable interrupt to occur on the next cycle
METHOD triggerNMI() CLASS CPU
   ::interrupt = interruptNMI

// triggerIRQ causes an IRQ interrupt to occur on the next cycle
METHOD triggerIRQ() CLASS CPU
   if ::I == 0
      ::interrupt = interruptIRQ
   end if

// stepInfo contains information that the instruction functions use
/*
CREATE CLASS stepInfo
   VAR address INIT 0
   VAR pc      INIT 0
   VAR mode    INIT 0
END CLASS
*/

// Step executes a single CPU instruction
METHOD Step(pPPU) CLASS CPU
   local cycles,opcode,mode,address,pageCrossed,info
   if ::stall > 0
      ::stall--
      return 1
   end if
   cycles := ::Cycles
   
   do case
      case ::interrupt=interruptNMI
         //clog("NMI INTERRUPT")
         ::nmi()
      case ::interrupt=interruptIRQ
         //clog("IRQ INTERRUPT")
         ::irq()
   end case

   ::interrupt = interruptNone

   opcode := ::Memory:Read(::PC) + 1
   mode := instructionModes[opcode]

   address     :=0
   pageCrossed := .f.
   //clog("case")
   do case
      case mode=modeAbsolute
         //clog("modeAbsolute")
         address = ::Read16(::PC + 1)
      case mode=modeAbsoluteX
         //clog("modeAbsoluteX")
         address = ::Read16(::PC + 1) + ::X 
         pageCrossed = pagesDiffer(address-::X, address)
      case mode=modeAbsoluteY
         //clog("modeAbsoluteY")
         address = ::Read16(::PC+1) + ::Y 
         pageCrossed = pagesDiffer(address - ::Y, address)
      case mode=modeAccumulator
         //clog("modeAccumulator")
         address = 0
      case mode=modeImmediate
         //clog("modeImmediate")
         address = ::PC + 1
      case mode=modeImplied
         //clog("modeImplied")
         address = 0
      case mode=modeIndexedIndirect
         //clog("modeIndexedIndirect")
         address = ::read16bug(::Memory:Read(::PC + 1) + ::X )
      case mode=modeIndirect
         //clog("modeIndirect")
         address = ::read16bug(::Read16(::PC + 1))
      case mode=modeIndirectIndexed
         //clog("modeIndirectIndexed")
         //clog("PC: ",::PC+1)
         //clog("Y: ",(::Y & 0xFFFF))
         address = ::read16bug(::Memory:Read(::PC + 1) ) + ::Y
         //clog("ADD: ",(address & 0xFFFF))
         pageCrossed = pagesDiffer(address - ::Y, address)
      case mode=modeRelative
         //clog("modeRelative")
         offset := ::Memory:Read(::PC + 1)
         if offset < 0x80
            address = ::PC + 2 + offset
         else
            address = ::PC + 2 + offset - 0x100
         end if
      case mode=modeZeroPage
         //clog("modeZeroPage")
         address = ::Memory:Read(::PC + 1)
      case mode=modeZeroPageX
         //clog("modeZeroPageX")
         address = ::Memory:Read(::PC + 1) + ::X
      case mode=modeZeroPageY
         //clog("modeZeroPageY")
         address = ::Memory:Read(::PC + 1) + ::Y
   end case

   ::PC += instructionSizes[opcode]
   ::Cycles += instructionCycles[opcode]

   if pageCrossed
      ::Cycles += instructionPageCycles[opcode]
   end if

//   clog("Address:",address)

   //#define ARRAY_OPCODE
   //#define HASH_OPCODE
   #define CASE_OPCODE

   #ifdef ARRAY_OPCODE
   eval(::table[opcode],{address, ::PC, mode})
   #endif
   #ifdef HASH_OPCODE
   opcode--
   eval(::cOphash[opcode],{address, ::PC, mode})
   #endif
   
   #ifdef CASE_OPCODE
   opcode--

   do case
      case opcode=0
         ::brk({address, ::PC, mode})
      case opcode=1
         ::ora({address, ::PC, mode})
      case opcode=2
         ::kil({address, ::PC, mode})
      case opcode=3
         ::slo({address, ::PC, mode})
      case opcode=4
         ::nop({address, ::PC, mode})
      case opcode=5
         ::ora({address, ::PC, mode})
      case opcode=6
         ::asl({address, ::PC, mode})
      case opcode=7
         ::slo({address, ::PC, mode})
      case opcode=8
         ::php({address, ::PC, mode})
      case opcode=9
         ::ora({address, ::PC, mode})
      case opcode=10
         ::asl({address, ::PC, mode})
      case opcode=11
         ::anc({address, ::PC, mode})
      case opcode=12
         ::nop({address, ::PC, mode})
      case opcode=13
         ::ora({address, ::PC, mode})
      case opcode=14
         ::asl({address, ::PC, mode})
      case opcode=15
         ::slo({address, ::PC, mode})
      case opcode=16
         ::bpl({address, ::PC, mode})
      case opcode=17
         ::ora({address, ::PC, mode})
      case opcode=18
         ::kil({address, ::PC, mode})
      case opcode=19
         ::slo({address, ::PC, mode})
      case opcode=20
         ::nop({address, ::PC, mode})
      case opcode=21
         ::ora({address, ::PC, mode})
      case opcode=22
         ::asl({address, ::PC, mode})
      case opcode=23
         ::slo({address, ::PC, mode})
      case opcode=24
         ::clc({address, ::PC, mode})
      case opcode=25
         ::ora({address, ::PC, mode})
      case opcode=26
         ::nop({address, ::PC, mode})
      case opcode=27
         ::slo({address, ::PC, mode})
      case opcode=28
         ::nop({address, ::PC, mode})
      case opcode=29
         ::ora({address, ::PC, mode})
      case opcode=30
         ::asl({address, ::PC, mode})
      case opcode=31
         ::slo({address, ::PC, mode})
      case opcode=32
         ::jsr({address, ::PC, mode})
      case opcode=33
         ::and({address, ::PC, mode})
      case opcode=34
         ::kil({address, ::PC, mode})
      case opcode=35
         ::rla({address, ::PC, mode})
      case opcode=36
         ::bit({address, ::PC, mode})
      case opcode=37
         ::and({address, ::PC, mode})
      case opcode=38
         ::rol({address, ::PC, mode})
      case opcode=39
         ::rla({address, ::PC, mode})
      case opcode=40
         ::plp({address, ::PC, mode})
      case opcode=41
         ::and({address, ::PC, mode})
      case opcode=42
         ::rol({address, ::PC, mode})
      case opcode=43
         ::anc({address, ::PC, mode})
      case opcode=44
         ::bit({address, ::PC, mode})
      case opcode=45
         ::and({address, ::PC, mode})
      case opcode=46
         ::rol({address, ::PC, mode})
      case opcode=47
         ::rla({address, ::PC, mode})
      case opcode=48
         ::bmi({address, ::PC, mode})
      case opcode=49
         ::and({address, ::PC, mode})
      case opcode=50
         ::kil({address, ::PC, mode})
      case opcode=51
         ::rla({address, ::PC, mode})
      case opcode=52
         ::nop({address, ::PC, mode})
      case opcode=53
         ::and({address, ::PC, mode})
      case opcode=54
         ::rol({address, ::PC, mode})
      case opcode=55
         ::rla({address, ::PC, mode})
      case opcode=56
         ::sec({address, ::PC, mode})
      case opcode=57
         ::and({address, ::PC, mode})
      case opcode=58
         ::nop({address, ::PC, mode})
      case opcode=59
         ::rla({address, ::PC, mode})
      case opcode=60
         ::nop({address, ::PC, mode})
      case opcode=61
         ::and({address, ::PC, mode})
      case opcode=62
         ::rol({address, ::PC, mode})
      case opcode=63
         ::rla({address, ::PC, mode})
      case opcode=64
         ::rti({address, ::PC, mode})
      case opcode=65
         ::eor({address, ::PC, mode})
      case opcode=66
         ::kil({address, ::PC, mode})
      case opcode=67
         ::sre({address, ::PC, mode})
      case opcode=68
         ::nop({address, ::PC, mode})
      case opcode=69
         ::eor({address, ::PC, mode})
      case opcode=70
         ::lsr({address, ::PC, mode})
      case opcode=71
         ::sre({address, ::PC, mode})
      case opcode=72
         ::pha({address, ::PC, mode})
      case opcode=73
         ::eor({address, ::PC, mode})
      case opcode=74
         ::lsr({address, ::PC, mode})
      case opcode=75
         ::alr({address, ::PC, mode})
      case opcode=76
         ::jmp({address, ::PC, mode})
      case opcode=77
         ::eor({address, ::PC, mode})
      case opcode=78
         ::lsr({address, ::PC, mode})
      case opcode=79
         ::sre({address, ::PC, mode})
      case opcode=80
         ::bvc({address, ::PC, mode})
      case opcode=81
         ::eor({address, ::PC, mode})
      case opcode=82
         ::kil({address, ::PC, mode})
      case opcode=83
         ::sre({address, ::PC, mode})
      case opcode=84
         ::nop({address, ::PC, mode})
      case opcode=85
         ::eor({address, ::PC, mode})
      case opcode=86
         ::lsr({address, ::PC, mode})
      case opcode=87
         ::sre({address, ::PC, mode})
      case opcode=88
         ::cli({address, ::PC, mode})
      case opcode=89
         ::eor({address, ::PC, mode})
      case opcode=90
         ::nop({address, ::PC, mode})
      case opcode=91
         ::sre({address, ::PC, mode})
      case opcode=92
         ::nop({address, ::PC, mode})
      case opcode=93
         ::eor({address, ::PC, mode})
      case opcode=94
         ::lsr({address, ::PC, mode})
      case opcode=95
         ::sre({address, ::PC, mode})
      case opcode=96
         ::rts({address, ::PC, mode})
      case opcode=97
         ::adc({address, ::PC, mode})
      case opcode=98
         ::kil({address, ::PC, mode})
      case opcode=99
         ::rra({address, ::PC, mode})
      case opcode=100
         ::nop({address, ::PC, mode})
      case opcode=101
         ::adc({address, ::PC, mode})
      case opcode=102
         ::ror({address, ::PC, mode})
      case opcode=103
         ::rra({address, ::PC, mode})
      case opcode=104
         ::pla({address, ::PC, mode})
      case opcode=105
         ::adc({address, ::PC, mode})
      case opcode=106
         ::ror({address, ::PC, mode})
      case opcode=107
         ::arr({address, ::PC, mode})
      case opcode=108
         ::jmp({address, ::PC, mode})
      case opcode=109
         ::adc({address, ::PC, mode})
      case opcode=110
         ::ror({address, ::PC, mode})
      case opcode=111
         ::rra({address, ::PC, mode})
      case opcode=112
         ::bvs({address, ::PC, mode})
      case opcode=113
         ::adc({address, ::PC, mode})
      case opcode=114
         ::kil({address, ::PC, mode})
      case opcode=115
         ::rra({address, ::PC, mode})
      case opcode=116
         ::nop({address, ::PC, mode})
      case opcode=117
         ::adc({address, ::PC, mode})
      case opcode=118
         ::ror({address, ::PC, mode})
      case opcode=119
         ::rra({address, ::PC, mode})
      case opcode=120
         ::sei({address, ::PC, mode})
      case opcode=121
         ::adc({address, ::PC, mode})
      case opcode=122
         ::nop({address, ::PC, mode})
      case opcode=123
         ::rra({address, ::PC, mode})
      case opcode=124
         ::nop({address, ::PC, mode})
      case opcode=125
         ::adc({address, ::PC, mode})
      case opcode=126
         ::ror({address, ::PC, mode})
      case opcode=127
         ::rra({address, ::PC, mode})
      case opcode=128
         ::nop({address, ::PC, mode})
      case opcode=129
         ::sta({address, ::PC, mode})
      case opcode=130
         ::nop({address, ::PC, mode})
      case opcode=131
         ::sax({address, ::PC, mode})
      case opcode=132
         ::sty({address, ::PC, mode})
      case opcode=133
         ::sta({address, ::PC, mode})
      case opcode=134
         ::stx({address, ::PC, mode})
      case opcode=135
         ::sax({address, ::PC, mode})
      case opcode=136
         ::dey({address, ::PC, mode})
      case opcode=137
         ::nop({address, ::PC, mode})
      case opcode=138
         ::txa({address, ::PC, mode})
      case opcode=139
         ::xaa({address, ::PC, mode})
      case opcode=140
         ::sty({address, ::PC, mode})
      case opcode=141
         ::sta({address, ::PC, mode})
      case opcode=142
         ::stx({address, ::PC, mode})
      case opcode=143
         ::sax({address, ::PC, mode})
      case opcode=144
         ::bcc({address, ::PC, mode})
      case opcode=145
         ::sta({address, ::PC, mode})
      case opcode=146
         ::kil({address, ::PC, mode})
      case opcode=147
         ::ahx({address, ::PC, mode})
      case opcode=148
         ::sty({address, ::PC, mode})
      case opcode=149
         ::sta({address, ::PC, mode})
      case opcode=150
         ::stx({address, ::PC, mode})
      case opcode=151
         ::sax({address, ::PC, mode})
      case opcode=152
         ::tya({address, ::PC, mode})
      case opcode=153
         ::sta({address, ::PC, mode})
      case opcode=154
         ::txs({address, ::PC, mode})
      case opcode=155
         ::tas({address, ::PC, mode})
      case opcode=156
         ::shy({address, ::PC, mode})
      case opcode=157
         ::sta({address, ::PC, mode})
      case opcode=158
         ::shx({address, ::PC, mode})
      case opcode=159
         ::ahx({address, ::PC, mode})
      case opcode=160
         ::ldy({address, ::PC, mode})
      case opcode=161
         ::lda({address, ::PC, mode})
      case opcode=162
         ::ldx({address, ::PC, mode})
      case opcode=163
         ::lax({address, ::PC, mode})
      case opcode=164
         ::ldy({address, ::PC, mode})
      case opcode=165
         ::lda({address, ::PC, mode})
      case opcode=166
         ::ldx({address, ::PC, mode})
      case opcode=167
         ::lax({address, ::PC, mode})
      case opcode=168
         ::tay({address, ::PC, mode})
      case opcode=169
         ::lda({address, ::PC, mode})
      case opcode=170
         ::tax({address, ::PC, mode})
      case opcode=171
         ::lax({address, ::PC, mode})
      case opcode=172
         ::ldy({address, ::PC, mode})
      case opcode=173
         ::lda({address, ::PC, mode})
      case opcode=174
         ::ldx({address, ::PC, mode})
      case opcode=175
         ::lax({address, ::PC, mode})
      case opcode=176
         ::bcs({address, ::PC, mode})
      case opcode=177
         ::lda({address, ::PC, mode})
      case opcode=178
         ::kil({address, ::PC, mode})
      case opcode=179
         ::lax({address, ::PC, mode})
      case opcode=180
         ::ldy({address, ::PC, mode})
      case opcode=181
         ::lda({address, ::PC, mode})
      case opcode=182
         ::ldx({address, ::PC, mode})
      case opcode=183
         ::lax({address, ::PC, mode})
      case opcode=184
         ::clv({address, ::PC, mode})
      case opcode=185
         ::lda({address, ::PC, mode})
      case opcode=186
         ::tsx({address, ::PC, mode})
      case opcode=187
         ::las({address, ::PC, mode})
      case opcode=188
         ::ldy({address, ::PC, mode})
      case opcode=189
         ::lda({address, ::PC, mode})
      case opcode=190
         ::ldx({address, ::PC, mode})
      case opcode=191
         ::lax({address, ::PC, mode})
      case opcode=192
         ::cpy({address, ::PC, mode})
      case opcode=193
         ::cmp({address, ::PC, mode})
      case opcode=194
         ::nop({address, ::PC, mode})
      case opcode=195
         ::dcp({address, ::PC, mode})
      case opcode=196
         ::cpy({address, ::PC, mode})
      case opcode=197
         ::cmp({address, ::PC, mode})
      case opcode=198
         ::dec({address, ::PC, mode})
      case opcode=199
         ::dcp({address, ::PC, mode})
      case opcode=200
         ::iny({address, ::PC, mode})
      case opcode=201
         ::cmp({address, ::PC, mode})
      case opcode=202
         ::dex({address, ::PC, mode})
      case opcode=203
         ::axs({address, ::PC, mode})
      case opcode=204
         ::cpy({address, ::PC, mode})
      case opcode=205
         ::cmp({address, ::PC, mode})
      case opcode=206
         ::dec({address, ::PC, mode})
      case opcode=207
         ::dcp({address, ::PC, mode})
      case opcode=208
         ::bne({address, ::PC, mode})
      case opcode=209
         ::cmp({address, ::PC, mode})
      case opcode=210
         ::kil({address, ::PC, mode})
      case opcode=211
         ::dcp({address, ::PC, mode})
      case opcode=212
         ::nop({address, ::PC, mode})
      case opcode=213
         ::cmp({address, ::PC, mode})
      case opcode=214
         ::dec({address, ::PC, mode})
      case opcode=215
         ::dcp({address, ::PC, mode})
      case opcode=216
         ::cld({address, ::PC, mode})
      case opcode=217
         ::cmp({address, ::PC, mode})
      case opcode=218
         ::nop({address, ::PC, mode})
      case opcode=219
         ::dcp({address, ::PC, mode})
      case opcode=220
         ::nop({address, ::PC, mode})
      case opcode=221
         ::cmp({address, ::PC, mode})
      case opcode=222
         ::dec({address, ::PC, mode})
      case opcode=223
         ::dcp({address, ::PC, mode})
      case opcode=224
         ::cpx({address, ::PC, mode})
      case opcode=225
         ::sbc({address, ::PC, mode})
      case opcode=226
         ::nop({address, ::PC, mode})
      case opcode=227
         ::isc({address, ::PC, mode})
      case opcode=228
         ::cpx({address, ::PC, mode})
      case opcode=229
         ::sbc({address, ::PC, mode})
      case opcode=230
         ::inc({address, ::PC, mode})
      case opcode=231
         ::isc({address, ::PC, mode})
      case opcode=232
         ::inx({address, ::PC, mode})
      case opcode=233
         ::sbc({address, ::PC, mode})
      case opcode=234
         ::nop({address, ::PC, mode})
      case opcode=235
         ::sbc({address, ::PC, mode})
      case opcode=236
         ::cpx({address, ::PC, mode})
      case opcode=237
         ::sbc({address, ::PC, mode})
      case opcode=238
         ::inc({address, ::PC, mode})
      case opcode=239
         ::isc({address, ::PC, mode})
      case opcode=240
         ::beq({address, ::PC, mode})
      case opcode=241
         ::sbc({address, ::PC, mode})
      case opcode=242
         ::kil({address, ::PC, mode})
      case opcode=243
         ::isc({address, ::PC, mode})
      case opcode=244
         ::nop({address, ::PC, mode})
      case opcode=245
         ::sbc({address, ::PC, mode})
      case opcode=246
         ::inc({address, ::PC, mode})
      case opcode=247
         ::isc({address, ::PC, mode})
      case opcode=248
         ::sed({address, ::PC, mode})
      case opcode=249
         ::sbc({address, ::PC, mode})
      case opcode=250
         ::nop({address, ::PC, mode})
      case opcode=251
         ::isc({address, ::PC, mode})
      case opcode=252
         ::nop({address, ::PC, mode})
      case opcode=253
         ::sbc({address, ::PC, mode})
      case opcode=254
         ::inc({address, ::PC, mode})
      case opcode=255
         ::isc({address, ::PC, mode})
   end case
   #endif

   return ::Cycles - cycles

// NMI - Non-Maskable Interrupt
METHOD nmi() CLASS CPU
   ::push16(::PC)
   ::php(nil)
   ::PC = ::Read16(0xFFFA)
   ::I = 1
   ::Cycles += 7

// IRQ - IRQ Interrupt
METHOD irq() CLASS CPU
   ::push16(::PC)
   ::php(nil)
   ::PC = ::Read16(0xFFFE)
   ::I = 1
   ::Cycles += 7

// ADC - Add with Carry
METHOD adc(info) CLASS CPU
   local a,b,c
   a := ::A
   b := ::Memory:Read(info[1])
   c := ::C
   ::A = a + b + c

   ::setZN(::A)

   ::A = (::A & 0xFF)

   if a + b + c > 0xFF
      ::C = 1
   else
      ::C = 0
   end if
   if ((a ^^ b) & 0x80) == 0 .and. ((a ^^ ::A) & 0x80) != 0
      ::V = 1
   else
      ::V = 0
   end if

// AND - Logical AND
METHOD and(info) CLASS CPU
   ::A = ((::A & ::Memory:Read(info[1])) & 0xFF)
   ::setZN(::A)

// ASL - Arithmetic Shift Left
METHOD asl(info) CLASS CPU
   if info[3] == modeAccumulator
      ::C = (((::A >> 7) & 0xFF) & 1)
      ::A = ((::A << 1) & 0xFF)
      ::setZN(::A)
   else
      value := ::Memory:Read(info[1])
      ::C = (((value >> 7) & 0xFF) & 1)
      value = ((value <<  1) & 0xFF)
      ::Memory:Write(info[1], value)
      ::setZN(value)
   end if

// BCC - Branch if Carry Clear
METHOD bcc(info) CLASS CPU
   if ::C == 0
      ::PC = info[1]
      ::addBranchCycles(info)
   end if

// BCS - Branch if Carry Set
METHOD bcs(info) CLASS CPU
   if ::C != 0
      ::PC = info[1]
      ::addBranchCycles(info)
   end if

// BEQ - Branch if Equal
METHOD beq(info) CLASS CPU 
   if ::Z != 0
      ::PC = info[1]
      ::addBranchCycles(info)
   end if

// BIT - Bit Test
METHOD bit(info) CLASS CPU
   value := ::Memory:Read(info[1])
   ::V = (((value >> 6) & 0xFF) & 1)
   ::setZ(value & ::A)
   ::setN(value)

// BMI - Branch if Minus
METHOD bmi(info) CLASS CPU
   if ::N != 0
      ::PC = info[1]
      ::addBranchCycles(info)
   end if

// BNE - Branch if Not Equal
METHOD bne(info) CLASS CPU
   if ::Z == 0
      ::PC = info[1]
      ::addBranchCycles(info)
   end if

// BPL - Branch if Positive
METHOD bpl(info) CLASS CPU
   if ::N == 0
      ::PC = info[1]
      ::addBranchCycles(info)
   end if

// BRK - Force Interrupt
METHOD brk(info) CLASS CPU
   ::push16(::PC)
   ::php(info)
   ::sei(info)
   ::PC = ::Read16(0xFFFE)

// BVC - Branch if Overflow Clear
METHOD bvc(info) CLASS CPU
   if ::V == 0
      ::PC = info[1]
      ::addBranchCycles(info)
   end if

// BVS - Branch if Overflow Set
METHOD bvs(info) CLASS CPU
   if ::V != 0
      ::PC = info[1]
      ::addBranchCycles(info)
   end if

// CLC - Clear Carry Flag
METHOD clc(info) CLASS CPU
   ::C = 0

// CLD - Clear Decimal Mode
METHOD cld(info) CLASS CPU
   ::D = 0

// CLI - Clear Interrupt Disable
METHOD cli(info) CLASS CPU
   ::I = 0

// CLV - Clear Overflow Flag
METHOD clv(info) CLASS CPU
   ::V = 0

// CMP - Compare
METHOD cmp(info) CLASS CPU
   value := ::Memory:Read(info[1])
   ::compare(::A, value)

// CPX - Compare X Register
METHOD cpx(info) CLASS CPU
   local value
   value := ::Memory:Read(info[1])
   ::compare(::X, value)

// CPY - Compare Y Register
METHOD cpy(info) CLASS CPU
   local value
   value := ::Memory:Read(info[1])
   ::compare(::Y, value)

// DEC - Decrement Memory
METHOD dec(info) CLASS CPU
   value := ::Memory:Read(info[1]) - 1
   ::Memory:Write(info[1], value)
   ::setZN(value)

// DEX - Decrement X Register
METHOD dex(info) CLASS CPU
   ::X--
   
   ::setZN(::X)
   
   ::X=(::X & 0xFF)

// DEY - Decrement Y Register
METHOD dey(info) CLASS CPU
   ::Y--
   
   ::setZN(::Y)

   ::Y=(::Y & 0xFF)
   
   
// EOR - Exclusive OR
METHOD eor(info) CLASS CPU
//   clog("A: ",::A)
   x=::Memory:Read(info[1])
//   clog("MEM: ",x)
   
   ::A = (::A ^^ x)
//   clog("A ^^ MEM: ",::A)
   ::setZN(::A)
   ::A=(::A & 0xFF)
//   clog("A ON EXIT: ",::A)

// INC - Increment Memory
METHOD inc(info) CLASS CPU
   value := ((::Memory:Read(info[1]) + 1) & 0xFF)
   ::Memory:Write(info[1], value)
   ::setZN(value)

// INX - Increment X Register
METHOD inx(info) CLASS CPU
   ::X++
   ::setZN(::X)
   ::X=(::X & 0xFF)

// INY - Increment Y Register
METHOD iny(info) CLASS CPU
   ::Y++
   ::setZN(::Y)
   ::Y=(::Y & 0xFF)

// JMP - Jump
METHOD jmp(info) CLASS CPU
   ::PC = info[1]

// JSR - Jump to Subroutine
METHOD jsr(info) CLASS CPU
   ::push16(::PC - 1)
   ::PC = info[1]

// LDA - Load Accumulator
METHOD lda(info) CLASS CPU
   ::A = ::Memory:Read(info[1])
   ::setZN(::A)

   ::A=(::A & 0xFF)

// LDX - Load X Register
METHOD ldx(info) CLASS CPU
   ::X = ::Memory:Read(info[1])
   ::setZN(::X)

// LDY - Load Y Register
METHOD ldy(info) CLASS CPU
   x=::Memory:Read(info[1])
   ::Y = x
   ::setZN(::Y)
   ::Y = (::Y & 0xFF)

// LSR - Logical Shift Right
METHOD lsr(info) CLASS CPU
   if info[3] == modeAccumulator
      ::C = (::A & 1)
      ::A = ((::A >> 1) & 0xFF)
      ::setZN(::A)
      ::A=(::A & 0xFF)
   else
      value := ::Memory:Read(info[1])
      ::C = (value & 1)
      value = ((value >> 1) & 0xFF)
      ::Memory:Write(info[1], value)
      ::setZN(value)
   end if

// NOP - No Operation
METHOD nop(info) CLASS CPU

// ORA - Logical Inclusive OR
METHOD ora(info) CLASS CPU
   ::A = (::A | ::Memory:Read(info[1]))
   ::setZN(::A)
   ::A=(::A & 0xFF)

// PHA - Push Accumulator
METHOD pha(info) CLASS CPU
   ::push(::A)

// PHP - Push Processor Status
METHOD php(info) CLASS CPU
   ::push((::Flags() | 0x10))

// PLA - Pull Accumulator
METHOD pla(info) CLASS CPU
   ::A = ::pull()
   ::setZN(::A)
   ::A=(::A & 0xFF)

// PLP - Pull Processor Status
METHOD plp(info) CLASS CPU
   ::SetFlags((::pull() & 0xEF) | 0x20)

// ROL - Rotate Left
METHOD rol(info) CLASS CPU
   local c,value
   if info[3] == modeAccumulator
      c   := ::C
      ::C := (((::A >> 7) & 0xFF) & 1)
      ::A := (((::A << 1) & 0xFF) | c)
      ::setZN(::A)
   else
      c     := ::C
      value := ::Memory:Read(info[1])
      ::C   := (((value >> 7) & 0xFF) & 1)
      value := (((value << 1) & 0xFF) | c)
      ::Memory:Write(info[1], value)
      ::setZN(value)
   end if

// ROR - Rotate Right
METHOD ror(info) CLASS CPU
   local c,value
   if info[3] == modeAccumulator
      c   := ::C
      ::C := ((::A & 1) & 0xFF)
      ::A := (((::A >> 1) & 0xFF) | ((c << 7) & 0xFF))
      ::setZN(::A)
   else
      c     := ::C
      value := ::Memory:Read(info[1])
      ::C   := ((value & 1) & 0xFF)
      value := (((value >> 1) & 0xFF) | ((c << 7) & 0xFF))
      ::Memory:Write(info[1], value)
      ::setZN(value)
   end if

// RTI - Return from Interrupt
METHOD rti(info) CLASS CPU
   ::SetFlags(((::pull() & 0xEF) | 0x20))
   ::PC = ::pull16()

// RTS - Return from Subroutine
METHOD rts(info) CLASS CPU
   ::PC = ::pull16() + 1

// SBC - Subtract with Carry
METHOD sbc(info) CLASS CPU
   local a,b,c
   a := ::A
   b := ::Memory:Read(info[1])
   c := ::C
   ::A = a - b - (1 - c)
   ::setZN(::A)
   ::A=(::A & 0xFF)
   if a-b-(1-c) >= 0
      ::C := 1
   else
      ::C := 0
   end if
   if ((a ^^ b) & 0x80) != 0 .and. ((a ^^ ::A) & 0x80) != 0
      ::V = 1
   else
      ::V = 0
   end if

// SEC - Set Carry Flag
METHOD sec(info) CLASS CPU
   ::C = 1

// SED - Set Decimal Flag
METHOD sed(info) CLASS CPU
   ::D = 1

// SEI - Set Interrupt Disable
METHOD sei(info) CLASS CPU
   ::I = 1

// STA - Store Accumulator
METHOD sta(info) CLASS CPU
//   clog("STA: ",info[1]," = A = ",::A)
   ::Memory:Write(info[1], ::A)

// STX - Store X Register
METHOD stx(info) CLASS CPU
   ::Memory:Write(info[1], ::X)

// STY - Store Y Register
METHOD sty(info) CLASS CPU
   ::Memory:Write(info[1], ::Y)

// TAX - Transfer Accumulator to X
METHOD tax(info) CLASS CPU
   ::X = ::A
   ::setZN(::X)

// TAY - Transfer Accumulator to Y
METHOD tay(info) CLASS CPU
   ::Y = ::A
   ::setZN(::Y)
   ::Y = (::Y & 0xFF)

// TSX - Transfer Stack Pointer to X
METHOD tsx(info) CLASS CPU
   ::X = ::SP
   ::setZN(::X)

// TXA - Transfer X to Accumulator
METHOD txa(info) CLASS CPU
   ::A = ::X
   ::setZN(::A)
   ::A=(::A & 0xFF)

// TXS - Transfer X to Stack Pointer
METHOD txs(info) CLASS CPU
   ::SP = ::X

// TYA - Transfer Y to Accumulator
METHOD tya(info) CLASS CPU
   ::A = ::Y
   ::setZN(::A)
   ::A=(::A & 0xFF)

// illegal opcodes below

METHOD ahx(info) CLASS CPU


METHOD alr(info) CLASS CPU


METHOD anc(info) CLASS CPU


METHOD arr(info) CLASS CPU


METHOD axs(info) CLASS CPU


METHOD dcp(info) CLASS CPU


METHOD isc(info) CLASS CPU


METHOD kil(info) CLASS CPU


METHOD las(info) CLASS CPU


METHOD lax(info) CLASS CPU


METHOD rla(info) CLASS CPU


METHOD rra(info) CLASS CPU


METHOD sax(info) CLASS CPU


METHOD shx(info) CLASS CPU


METHOD shy(info) CLASS CPU


METHOD slo(info) CLASS CPU


METHOD sre(info) CLASS CPU


METHOD tas(info) CLASS CPU


METHOD xaa(info) CLASS CPU

/*
procedure sym_byte(x)
return (x & 0xFF)
if x < 0 .or. x > 0xFF
   return asc(chr(x)) //+256
else
   return x
end if

procedure sym_uint16(x)
return (x & 0xFFFF)
if x < 0
   return x + 0xFFFF + 1
elseif x > 0xFFFF
   r=int(x / 0xFFFF)
   return (x - (r * 0xFFFF)) - r
else
   return x
end if

procedure sym_uint32(x)
return (x & 0xFFFFFFFF)
if x < 0
   return x + 0xFFFFFFFF + 1
else
   return x
end if
*/

//procedure uint64(x)
//return (x & 0xFFFFFFFFFFFFFFFF)
/*
if x < 0
   return x + 18446744073709551614 + 1
elseif x > 18446744073709551614
   r=int(x / 18446744073709551614)
   return (x - (r * 18446744073709551614)) - r
else
   return x
end if
*/
   
procedure clog(...)
return
if nH=0
//   if file("cpu.txt")
//      nH=fopen("cpu.txt",2)
//   else
      nH=fcreate("cpu.txt")
//   end if
      fseek(nH,0,2)
end if
   
   aeval(hb_aParams(),{|x|fwrite(nH,hst(x))})
   
   fwrite(nH,chr(10))
//   fclose(nH)

procedure hst(x)
if valtype(x)='N'
   if x>=0 .and. x<=255
      return hb_numtohex(x,2)
   elseif x<=0xFFFF
      return hb_numtohex(x,4)
   else
      return "str:"+transform(x,"99999999999999999999")
   end if
else
   return x
end if