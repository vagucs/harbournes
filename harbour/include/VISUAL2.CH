
////////// Sistema de Botäes

#Define Read(X) ReadModal(getlist,1,X)
#define LB_SHOW		.F.	//Fun‡Æo LinButton() - Desenha botäes
#define LB_PROC		.T.	//Fun‡Æo LinButton() - Processa botäes

#define NB_GRAY		1	//Fundo Cinza
#define NB_WHITE	2	//Fundo Branco
#define NB_USER		3	//Cores definidas pelo usu rio

/// ButtonColors() - cores definidas pelo usu rio (NB_USER)

#xtranslate :cButMold => \[1\]		//[1] Moldura
#xtranslate :cBut3dUp => \[2\]		//[2] Efeito 3D UP
#xtranslate :cBut3dDn => \[3\]		//[3] Efeito 3D DOWN
#xtranslate :cButEnbl => \[4\]		//[4] Habilitado
#xtranslate :cButDsbl => \[5\]		//[5] Desabilitado
#xtranslate :cButYFoc => \[6\]		//[6] Com foco
#xtranslate :cButNFoc => \[7\]		//[7] Sem foco
#xtranslate :cButHotk => \[8\]		//[8] Hot-Key
#xtranslate :cButDMsg => \[9\]		//[9] Mensagem do rodap‚

// MenuColors() - cores definidas pelo usu rio

#xtranslate :cMenuEnbl  =>  \[1\]	//[1] Item Habilitado
#xtranslate :cMenuDsbl  =>  \[2\]	//[2] Item Desabilitado
#xtranslate :cMenuYFoc  =>  \[3\]	//[3] Item Habilitado com Foco
#xtranslate :cMenuNFoc  =>  \[4\]	//[4] Item Desabilitado com Foco
#xtranslate :cMenuHKYF  =>  \[5\]	//[5] HotKey com Foco (habilitado)
#xtranslate :cMenuHKNF  =>  \[6\]	//[6] HotKey sem Foco (habilitado)
#xtranslate :cMenuHDYF  =>  \[7\]	//[7] HotKey com Foco (desabilitado)
#xtranslate :cMenuHDNF  =>  \[8\]	//[8] HotKey com Foco (desabilitado)
#xtranslate :cMenuMold  =>  \[9\]	//[9] Cor da Moldura
#xtranslate :cMenuDMsg  =>  \[10\]	//[10] Mensagem do rodap‚

////////// Frame()

#define FT_LEFT		1	//Titulo alinhado a esquerda
#define FT_CNTR		2	//Titulo centralizado
#define FT_RIGH		3	//Titulo alinhado a direita

#define FE_DOWN		1	//Efeito: Baixo relevo
#define FE_UP		2	//Alto relevo
#define FE_NORMAL	3	//Frame() normal, sem relevo

////////// DrawFn()

#define FK_1	CHR( 01 )	//Tecla F1
#define FK_2	CHR( 02 )	//Tecla F2
#define FK_3	CHR( 03 )	//Tecla F3
#define FK_4	CHR( 04 )	//Tecla F4
#define FK_5	CHR( 05 )	//Tecla F5
#define FK_6	CHR( 06 )	//Tecla F6
#define FK_7	CHR( 07 )	//Tecla F7
#define FK_8	CHR( 08 )	//Tecla F8
#define FK_9	CHR( 09 )	//Tecla F9
#define FK_10   CHR( 10 )	//Tecla F10
#define FK_11   CHR( 11 )	//Tecla F11
#define FK_12   CHR( 12 )	//Tecla F12

////////// Fun‡Æo Win() - cores da janela
////////// Obs: "WT_" = Win T¡tulo e "WD_" = Win Desktop

#define WT_PRETOBRANCO	"W+/N"	//T¡tulo branco / fundo preto
#define WD_PRETOBRANCO	"N*/W"	//Frente preto / fundo branco

#define WT_AZUL1BRANCO	"W+/B"	//T¡tulo branco / fundo azul escuro
#define WD_AZUL1BRANCO	"B*/W"	//Frente azul escuro / fundo branco

#define WT_AZUL2BRANCO	"W+*/B"	//T¡tulo branco / fundo azul claro
#define WD_AZUL2BRANCO	"B+*/W"	//Frente azul claro / fundo branco

#define WT_AZUL1  "W+*/B"     //T¡tulo branco / fundo azul claro
#define WD_AZUL1  "B*/WW*"     //Frente azul claro / fundo branco

#define WT_VERDEBRANCO	"W+/G"	//T¡tulo branco / fundo verde
#define WD_VERDEBRANCO	"G*/W"	//Frente verde / fundo branco

#define WT_VERM1BRANCO	"W+/R"	//T¡tulo branco / fundo vermelho escuro
#define WD_VERM1BRANCO	"R*/W"	//Frente vermelho escuro / fundo branco

#define WT_VERM2BRANCO    "W+*/R"     //T¡tulo branco / fundo vermelho claro
#define WD_VERM2BRANCO	"R+*/W"	//Frente vermelho claro / fundo branco

#define WT_MARROMBRANCO	"W+/GR"	//T¡tulo branco / fundo marrom
#define WD_MARROMBRANCO	"GR*/W"	//Frente marrom / fundo branco

#define WT_PRETOCINZA	"W+/N"	//T¡tulo branco / fundo preto
#define WD_PRETOCINZA	"N/W"	//Frente preto / fundo cinza

#define WT_AZUL1CINZA	"W+/B"	//T¡tulo branco / fundo azul escuro
#define WD_AZUL1CINZA	"B/W"	//Frente azul escuro / fundo cinza

#define WT_VERDECINZA	"W+/G"	//T¡tulo branco / fundo verde
#define WD_VERDECINZA	"G/W"	//Frente verde / fundo cinza

#define WT_VERM1CINZA	"W+/R"	//T¡tulo branco / fundo vermelho escuro
#define WD_VERM1CINZA	"R/W"	//Frente vermelho escuro / fundo cinza

#define WT_VERM2CINZA	"W+*/R"	//T¡tulo branco / fundo vermelho claro
#define WD_VERM2CINZA	"R+/W"	//Frente vermelho claro / fundo cinza

#define WT_MARROMCINZA	"W+/GR"	//T¡tulo branco / fundo marrom
#define WD_MARROMCINZA	"GR/W"	//Frente marrom / fundo cinza

#define WT_BRANCOCINZA	"N*/W"	//T¡tulo preto / fundo branco
#define WD_BRANCOCINZA	"W+/W"	//Frente branco / fundo cinza

////////// VLMouseInf() - Controle do Mouse

#xtranslate :mouseLeft   => \[1\]	//[1] BotÆo esquerdo pressionado
#xtranslate :mouseRight  => \[2\]	//[2] BotÆo direito pressionado
#xtranslate :mouseRow    => \[3\]	//[3] Linha do cursor do mouse
#xtranslate :mouseCol    => \[4\]	//[4] Coluna do cursor do mouse

// Macro para verificar botÆo pressionado - Retorna .T. ou .F.
#define MOUSEPRESS( aMouse ) ( aMouse:mouseLeft .OR. aMouse:mouseRight )

// VLREAD permite o uso do mouse no sistema de GET do Clipper
#command VLREAD =>;AddObj(lastwindow,{|nWindow|__temp__:=nWindow,aEval(GetList,{|oGet|WVW_DrawBoxGet(__temp__,oGet:Row,oGet:Col,Len(Transform(oGet:VarGet(),oGet:Picture)))})});
                  ; ReadModal( GetList ) ; GetList := {}

// Redirecionamento de sa¡das na tela para uso com o mouse

#xtranslate Scroll( [ <parms,...> ])      => VLScroll( <parms> )
#xtranslate SetMode( [ <parms,...> ] )    => VLSetMode( <parms> )
#xtranslate SaveScreen( [ <parms,...> ] ) => VLSaveScreen( <parms> )
#xtranslate RestScreen( [ <parms,...> ] ) => VLRestScreen( <parms> )
#xtranslate DevOut( [ <parms,...> ] )     => VLDevOut( <parms> )
#xtranslate QOut( [ <parms,...> ] )       => VLQout( { <parms> } )
#xtranslate QQOut( [ <parms,...> ] )      => VLQQout( { <parms> } )
#xtranslate DevOutPict( [ <parms,...> ] ) => VLDevOutPi( <parms> )
#xtranslate DispBox( [ <parms,...> ] )    => VLDispBox( <parms> )

////////// Fun‡Æo WaitState() - c¢digos de retorno

#define WAIT_NONE      0	//Uso interno
#define WAIT_KEYPRESS  1	//Tecla pressionada
#define WAIT_MOUSEDOWN 2	//BotÆo do mouse pressionado
#define WAIT_MOUSEUP   3	//BotÆo do mouse liberado
#define WAIT_MOUSEMOVE 4	//Mouse em movimento com botÆo pressionado

// Öndices do array retornado por WaitState()

#xtranslate :wsType => \[1\]	//[1] Tipo de ocorrˆncia
#xtranslate :wsKey  => \[2\]	//[2] C¢digo da tecla
#xtranslate :wsRow  => \[2\]	//[2] Linha do cursor do mouse
#xtranslate :wsCol  => \[3\]	//[3] Coluna do cursor do mouse

// Fim de VISUAL2.CH
