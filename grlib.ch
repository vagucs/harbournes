#define COR_CORPO_JANELA        1
#define COR_TITULO_JANELA       2
#define COR_DESKTOP             3
#define COR_MENU_ATIVO          4
#define COR_MENU_INATIVO        5
#define COR_MENU_BARRA          6
#define COR_SUBMENU             7
#define COR_GET                 8
#define COR_TELA_GET            9
#define COR_MENU_BROWSE         10
#define COR_ALERT               11
#define COR_PROGRESS_BAR        12
#define COR_TEXTO_PROGRESS_BAR  13
#define COR_DESTAQUE            14

#define IS_GRAPH          [ALLEG]$upper(hb_gtversion())

#define SYSTEM_NAME       "SISLOJA"
#define SYSTEM_VERSION    "1.5"

#define WINDOW_VISTA

#ifdef WINDOW_VISTA
   #define WINDOW_MEANS        "./imagens/janela_vista/vista_means.bmp"
   #define WINDOW_TOP_LEFT     "./imagens/janela_vista/vista_top_left.bmp"
   #define WINDOW_BOTTOM_LEFT  "./imagens/janela_vista/vista_bottom_left.bmp"
   #define WINDOW_TOP_MEANS    "./imagens/janela_vista/vista_top_means.bmp"
   #define WINDOW_BOTTOM_MEANS "./imagens/janela_vista/vista_bottom_means.bmp"
   #define WINDOW_TOP_RIGHT    "./imagens/janela_vista/vista_top_right.bmp"
   #define WINDOW_BOTTOM_RIGHT "./imagens/janela_vista/vista_bottom_right.bmp"
   #define WINDOW_LEFT_LINE_1  "./imagens/janela_vista/vista_left_linha_1.bmp"  // Possui 3 varivais de linha, pois talvez necessite para efeitos como o que foi usado no visual do Vista. Se nao houver necessidade, repetir a mesma imagem nas 3 variaveis
   #define WINDOW_RIGHT_LINE_1 "./imagens/janela_vista/vista_right_linha_1.bmp"
   #define WINDOW_LEFT_LINE_2  "./imagens/janela_vista/vista_left_linha_2.bmp"
   #define WINDOW_RIGHT_LINE_2 "./imagens/janela_vista/vista_right_linha_2.bmp"
   #define WINDOW_LEFT_LINE_3  "./imagens/janela_vista/vista_left_linha_3.bmp"
   #define WINDOW_RIGHT_LINE_3 "./imagens/janela_vista/vista_right_linha_3.bmp"
   #define WINDOW_BUTTON_ON    "./imagens/janela_vista/vista_top_buttons.bmp"
   #define WINDOW_BUTTON_OFF   "./imagens/janela_vista/vista_top_buttons_off.bmp"
#endif
#ifdef WINDOW_XP
   #define WINDOW_MEANS        "./imagens/janela_xp/vista_means.bmp"
   #define WINDOW_TOP_LEFT     "./imagens/janela_xp/vista_top_left.bmp"
   #define WINDOW_BOTTOM_LEFT  "./imagens/janela_xp/vista_bottom_left.bmp"
   #define WINDOW_TOP_MEANS    "./imagens/janela_xp/vista_top_means.bmp"
   #define WINDOW_BOTTOM_MEANS "./imagens/janela_xp/vista_bottom_means.bmp"
   #define WINDOW_TOP_RIGHT    "./imagens/janela_xp/vista_top_right.bmp"
   #define WINDOW_BOTTOM_RIGHT "./imagens/janela_xp/vista_bottom_right.bmp"
   #define WINDOW_LEFT_LINE_1  "./imagens/janela_xp/vista_left_linha_1.bmp"  // Possui 3 varivais de linha, pois talvez necessite para efeitos como o que foi usado no visual do Vista. Se nao houver necessidade, repetir a mesma imagem nas 3 variaveis
   #define WINDOW_RIGHT_LINE_1 "./imagens/janela_xp/vista_right_linha_1.bmp"
   #define WINDOW_LEFT_LINE_2  "./imagens/janela_xp/vista_left_linha_2.bmp"
   #define WINDOW_RIGHT_LINE_2 "./imagens/janela_xp/vista_right_linha_2.bmp"
   #define WINDOW_LEFT_LINE_3  "./imagens/janela_xp/vista_left_linha_3.bmp"
   #define WINDOW_RIGHT_LINE_3 "./imagens/janela_xp/vista_right_linha_3.bmp"
   #define WINDOW_BUTTON_ON    "./imagens/janela_xp/vista_top_buttons.bmp"
   #define WINDOW_BUTTON_OFF   "./imagens/janela_xp/vista_top_buttons_off.bmp"
#endif   
#define BUTTON_ON_LEFT      "./imagens/botao_vista/botao_on_left.bmp"
#define BUTTON_ON_RIGHT     "./imagens/botao_vista/botao_on_right.bmp"
#define BUTTON_ON_MEANS     "./imagens/botao_vista/botao_on_means.bmp"
#define BUTTON_OFF_LEFT     "./imagens/botao_vista/botao_off_left.bmp"
#define BUTTON_OFF_RIGHT    "./imagens/botao_vista/botao_off_right.bmp"
#define BUTTON_OFF_MEANS    "./imagens/botao_vista/botao_off_means.bmp"

#define TOOLBAR_MEANS       "./imagens/barra_tarefas/barra_means.bmp"

#define SHADOW_MEANS        "./imagens/sombra/sombra_means.bmp"
#define SHADOW_RIGHT        "./imagens/sombra/sombra_right.bmp"
#define SHADOW_BOTTOM_RIGHT "./imagens/sombra/sombra_bottom_right.bmp"
#define SHADOW              "./imagens/sombra/sombra.bmp"

#define ACHOICE_LEFT        "./imagens/barra_tarefas/barra_achoice_left.bmp"
#define ACHOICE_MEANS       "./imagens/barra_tarefas/barra_achoice_means.bmp"
#define ACHOICE_RIGHT       "./imagens/barra_tarefas/barra_achoice_right.bmp"
#define ACHOICE_LEFT_CLEAR  "./imagens/barra_tarefas/barra_achoice_left_clear.bmp"

#define BAR_BUTTON_MEANS    "./imagens/barra_tarefas/barra_button_means.bmp"
#define BAR_BUTTON_LEFT     "./imagens/barra_tarefas/barra_button_left.bmp"
#define BAR_BUTTON_RIGHT    "./imagens/barra_tarefas/barra_button_right.bmp"

#define ABA_ATIVA_DIREITA    "./imagens/aba_xp/ww_aba_ativa_direita.bmp"
#define ABA_ATIVA_ESQUERDA   "./imagens/aba_xp/ww_aba_ativa_esquerda.bmp"
#define ABA_ATIVA_MEIO       "./imagens/aba_xp/ww_aba_ativa_meio.bmp"
#define ABA_INATIVA_DIREITA  "./imagens/aba_xp/ww_aba_inativa_direita.bmp"
#define ABA_INATIVA_ESQUERDA "./imagens/aba_xp/ww_aba_inativa_esquerda.bmp"
#define ABA_INATIVA_MEIO     "./imagens/aba_xp/ww_aba_inativa_meio.bmp"

#define N_SHADOW_MEANS        1
#define N_SHADOW_RIGHT        2
#define N_SHADOW              3
#define N_SHADOW_BOTTOM_RIGHT 4
#define N_WINDOW_MEANS        5
#define N_WINDOW_TOP_LEFT     6
#define N_WINDOW_BOTTOM_LEFT  7
#define N_WINDOW_TOP_MEANS    8
#define N_WINDOW_BOTTOM_MEANS 9
#define N_WINDOW_TOP_RIGHT    10
#define N_WINDOW_BOTTOM_RIGHT 11
#define N_WINDOW_LEFT_LINE_1  12
#define N_WINDOW_RIGHT_LINE_1 13
#define N_WINDOW_LEFT_LINE_2  14
#define N_WINDOW_RIGHT_LINE_2 15
#define N_WINDOW_LEFT_LINE_3  16
#define N_WINDOW_RIGHT_LINE_3 17
#define N_WINDOW_BUTTON_ON    18
#define N_WINDOW_BUTTON_OFF   19
#define N_ACHOICE_LEFT        20
#define N_ACHOICE_MEANS       21
#define N_ACHOICE_RIGHT       22
#define N_ACHOICE_LEFT_CLEAR  23
#define N_BAR_BUTTON_MEANS    24
#define N_BAR_BUTTON_LEFT     25
#define N_BAR_BUTTON_RIGHT    26
#define N_BUTTON_ON_LEFT      27
#define N_BUTTON_ON_RIGHT     28
#define N_BUTTON_ON_MEANS     29
#define N_BUTTON_OFF_LEFT     30
#define N_BUTTON_OFF_RIGHT    31
#define N_BUTTON_OFF_MEANS    32
#define N_TOOLBAR_MEANS       33

#define N_ABA_ATIVA_DIREITA    34
#define N_ABA_ATIVA_ESQUERDA   35
#define N_ABA_ATIVA_MEIO       36
#define N_ABA_INATIVA_DIREITA  37
#define N_ABA_INATIVA_ESQUERDA 38
#define N_ABA_INATIVA_MEIO     39

#command DRAW FRAME   => aEval(GetList,{|oGet|gr_getframe(oGet:Row,oGet:Col,oGet:Row,oGet:Col+Len(Transform(oGet:VarGet(),oGet:Picture)))})
