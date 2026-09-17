taskkill -im nes.exe -f
del cpu.txt
rem OTIMIZADO ativo via nesopt.ch (incluido nos .prg). Para desativar: -uNOTOTIMIZADO
hbmk2 -prgflag=gc0 -cflag=-mwindows nes.prg ines.prg grlib.prg console.prg ^
cartridge.prg controller.prg mapper1.prg mapper2.prg mapper4.prg mapper7.prg ^
ppu.prg memory.prg apu.prg cpu.prg image.prg -lxhb -lhbxpp -lllibg -lalleg -lgtalleg ^
-lpng -lddraw -ldxguid -ldinput -lgdi32 -ldsound -lhbct -lalttf -cflag+=-Wall -cflag+=-O3 -cflag+=-flto ^
-cflag+=-DALLEGRO_STATICLINK -nulrdd 