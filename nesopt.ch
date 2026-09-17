#ifndef NESOPT_CH
#define NESOPT_CH

// Otimizacoes de desempenho do emulador NES (Harbour)
// Ativo por padrao. Para desativar no compile.bat: hbmk2 -uNOTOTIMIZADO ...
// (nao use -uOTIMIZADO: o Harbour tenta incluir OTIMIZADO.ch automaticamente)
#ifndef NOTOTIMIZADO
   #ifndef OTIMIZADO
      #define OTIMIZADO
   #endif
#endif

#endif
