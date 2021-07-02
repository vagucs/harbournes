

#include "box.ch" 
#include "grlib.ch"

procedure smsg(cMsg) // Apenas exibe um texto formatado na tela.
gr_window(16,22,20,80,"Aguarde...")
@ 18,(100-len(cMsg))/2 say cMsg color "n/w*"
//logdia(cMsg)

function gr_Msg(cMsg,nLin1,nCol1,nLin2,nCol2,cTitulo)
local aRet
nLin1=iif(nLin1=nil,int(maxrow()/2)-2,nLin1)
nLin2=iif(nLin2=nil,int(maxrow()/2)+2,nLin2)
nCol1=iif(nCol1=nil,int(maxcol()/2)-35,nCol1)
nCol2=iif(nCol2=nil,int(maxcol()/2)+35,nCol2)
cTitulo="Aguarde..."
aRet=gr_openwin(nLin1,nCol1,nLin2,nCol2,cTitulo)
@ int(maxrow()/2),int(maxcol()-len(cMsg))/2 say cMsg
//logdia(cMsg)
return aRet

function gr_centerinfo(cMsg)
@ int(maxrow()/2)+1,int(maxcol()-len(cMsg))/2 say cMsg

procedure gr_savescreen(nLinha,nColuna,nLinha2,nColuna2)
local bmp,cTexto
if IS_GRAPH
   nLinha=iif(nLinha=0,nLinha,nLinha-1)
   bmp=_create_bitmap(((nColuna2-nColuna)+1)*8,((nLinha2-nLinha)+1)*16)
   _blit(_get_buffer(),bmp,nColuna*8,nLinha*16,0,0,((nColuna2-nColuna)+1)*8,((nLinha2-nLinha)+1)*16)
   cTexto=savescreen(nLinha,nColuna,nLinha2,nColuna2)   
   return {bmp,cTexto}
else
   return savescreen(nLinha,nColuna,nLinha2,nColuna2)
end if

procedure gr_restscreen(nLinha,nColuna,nLinha2,nColuna2,cTela)
if IS_GRAPH
   nLinha=iif(nLinha=0,nLinha,nLinha-1)

   _draw_sprite(_get_buffer(),cTela[1],nColuna*8,nLinha*16)

   _destroy_bitmap(cTela[1])
   @ maxrow(),maxcol() say " "
   return Nil
else
   return restscreen(nLinha,nColuna,nLinha2,nColuna2,cTela)
end if

procedure gr_cutscreen(nLinha,nColuna,nLinha2,nColuna2)
local bmp,cTexto
if IS_GRAPH
   nLinha=iif(nLinha=0,nLinha,nLinha-1)
   bmp=_create_bitmap(((nColuna2-nColuna)+1)*8,((nLinha2-nLinha)+1)*16)
   _blit(_get_buffer(),bmp,nColuna*8,nLinha*16,0,0,((nColuna2-nColuna)+1)*8,((nLinha2-nLinha)+1)*16)
   return {bmp}
else
   return savescreen(nLinha,nColuna,nLinha2,nColuna2)
end if

procedure gr_pastescreen(nLinha,nColuna,nLinha2,nColuna2,cTela)
if IS_GRAPH
   nLinha=iif(nLinha=0,nLinha,nLinha-1)
   _draw_sprite(_get_buffer(),cTela[1],nColuna*8,nLinha*16)
   _destroy_bitmap(cTela[1])
   @ maxrow(),maxcol() say " "
   return Nil
else
   return restscreen(nLinha,nColuna,nLinha2,nColuna2,cTela)
end if

procedure gr_clear(nLinha,nColuna,nLinha2,nColuna2)
if pcount()=0
   clear
else
   @ nLinha,nColuna clear to nLinha2,nColuna2
end if

procedure gr_setmode(nLin,nCol)
return setmode(nLin,nCol)

procedure gr_openwin(nLinha,nColuna,nLinha2,nColuna2,cTexto,nCor,lTransp,cButton,cCor)
local cTela[5]
cTela[1]=nLinha-1
cTela[2]=nColuna-2
cTela[3]=nLinha2+1
cTela[4]=nColuna2+2
cTela[5]=gr_savescreen(nLinha-1,nColuna-2,nLinha2+1,nColuna2+2)
gr_window(nLinha,nColuna,nLinha2,nColuna2,cTexto,nCor,lTransp,cButton,cCor)
return cTela

procedure gr_closewin(cTela)
gr_restscreen(cTela[1],cTela[2],cTela[3],cTela[4],cTela[5])

function gr_window(nlinha,ncoluna,nlinha2,ncoluna2,ctexto,ncor,ltransp,cbutton,ccor)
local nlin,ncol,nlin2,ncol2,i,y,imagem,ncont
ccor=iif(ccor=nil,"w",ccor)
@ nlinha,ncoluna say repl(" ",ncoluna2-ncoluna+1) color("w+/b")
if cTexto=NIl
	cTexto=""
end if
@ nlinha,nColuna+1 say "[ "+ctexto+" ]" color ("w+/b")
gr_caixa3d(nlinha+1,ncoluna,nlinha2,ncoluna2,ccor)
return nil

function gr_caixa3d(nlinha,ncoluna,nlinha2,ncoluna2,ccor,cmodoexibe)
local t_bordacaixa1,t_bordacaixa2,t_bordacaixa3,t_bordacaixa4,nlin,ccor1,ccor2
if ccor=nil
   ccor1:="w+/w"
   ccor2:="n/w"
else
   ccor1:="w+/"+ccor
   ccor2:="n/"+ccor
end if
if(cmodoexibe=nil,cmodoexibe:="E",cmodoexibe)
t_bordacaixa1="зд    юЁ "
t_bordacaixa2="д"
t_bordacaixa3="©"
t_bordacaixa4="ы"
@ nlinha,ncoluna,nlinha2,ncoluna2 box t_bordacaixa1 color(iif(upper(cmodoexibe)="E",ccor1,ccor2))
@ nlinha,ncoluna2 say t_bordacaixa3 color(iif(upper(cmodoexibe)="E",ccor2,ccor1))
@ nlinha2,ncoluna2 say t_bordacaixa4 color(iif(upper(cmodoexibe)="E",ccor2,ccor1))
@ nlinha2,ncoluna+1 say repl(t_bordacaixa2,(ncoluna2-ncoluna)-1) color(iif(upper(cmodoexibe)="E",ccor2,ccor1))
for nlin=nlinha+1 to nlinha2-1
   @ nlin,ncoluna2 say "Ё" color(iif(upper(cmodoexibe)="E",ccor2,ccor1))
next

function open_image(cImagem,nColuna,nLinha)
local cImg
cImg=_load_bitmap(cImagem)
if file("logbmp")
   //logdia(cImagem)
end if
if cImg>0
   _draw_sprite(_get_buffer(),cImg,nColuna,nLinha)
end if
_destroy_bitmap(cImg)

function abrir_imagem(cImagem,nColuna,nLinha)
if cImagem>0
   if file("logbmp")
      //logdia(cImagem)
   end if
   if cImagem>0
      _draw_sprite(_get_buffer(),cImagem,nColuna,nLinha)
   end if
end if


#pragma BEGINDUMP
#include <math.h>
#include <allegro.h>
#include <hbapi.h>
#include "loadpng.h"


HB_FUNC(GR_CLEAR_BITMAP){
   clear_bitmap((BITMAP *)hb_parnl(1));
}

HB_FUNC(GR_CLEAR_TO_COLOR){
   clear_to_color((BITMAP *)hb_parnl(1),hb_parnl(2));
}

HB_FUNC(RECTFILL){
   rectfill((BITMAP *)hb_parnl(1),hb_parnl(2),hb_parnl(3),hb_parnl(4),hb_parnl(5),hb_parnl(6));
}
HB_FUNC(VLINE){
   vline((BITMAP *)hb_parnl(1),hb_parnl(2),hb_parnl(3),hb_parnl(4),hb_parnl(5));
}
HB_FUNC(HLINE){
   hline((BITMAP *)hb_parnl(1),hb_parnl(2),hb_parnl(3),hb_parnl(4),hb_parnl(5));
}
HB_FUNC(LINE){
   line((BITMAP *)hb_parnl(1),hb_parnl(2),hb_parnl(3),hb_parnl(4),hb_parnl(5),hb_parnl(6));
}
HB_FUNC(RECT){
   rect((BITMAP *)hb_parnl(1),hb_parnl(2),hb_parnl(3),hb_parnl(4),hb_parnl(5),hb_parnl(6));
}
HB_FUNC(_get_buffer){
   hb_retnl((HB_ULONG) screen);
}
HB_FUNC(MAKECOL){
   hb_retnl((HB_ULONG)makecol(hb_parnl(1),hb_parnl(2),hb_parnl(3)));
}
HB_FUNC(PUTPIXEL){
   putpixel((BITMAP *)hb_parnl(1),hb_parnl(2),hb_parnl(3),hb_parnl(4));
}

HB_FUNC(GETPIXEL){
   hb_retnl((HB_ULONG)getpixel((BITMAP *)hb_parnl(1),hb_parnl(2),hb_parnl(3)));
}
/*
HB_FUNC(_LOAD_BITMAP){
   PALETTE pal;
   hb_retnl((HB_ULONG)load_bitmap(hb_parc(1),pal));
}*/

HB_FUNC(_GET_W){
   hb_parnl(SCREEN_W);
}

HB_FUNC(_GET_H){
   hb_parnl(SCREEN_H);
}

/*
HB_FUNC(_GET_H_BITMAP)
{
    BITMAP *temp;
    temp=(BITMAP *)hb_parnl(1);
    hb_retnl((HB_ULONG)temp->h);
}

HB_FUNC(_GET_W_BITMAP)
{
    BITMAP *temp;
    temp=(BITMAP *)hb_parnl(1);
    hb_retnl((HB_ULONG)temp->w);
}
*/
HB_FUNC(_STRETCH_SPRITE){
   BITMAP *dest;
   BITMAP *font;
   int x,y,w,h;
   dest=(BITMAP *)hb_parnl(1);
   font=(BITMAP *)hb_parnl(2);
   if ((dest==0) || (font==0)){return;}
   y=hb_parnl(3);
   x=hb_parnl(4);
   w=hb_parnl(5);
   h=hb_parnl(6);
   stretch_sprite(dest,font,y,x,w,h);
}

/*
HB_FUNC(LOAD_SAMPLE)
{
 char *arq=hb_parc(1);
 SAMPLE *temp;
 temp=load_sample(arq);
 hb_retnl((int)temp);
}
*/

HB_FUNC(_LOAD_BMP){
   PALETTE pal;
   hb_retnl((HB_ULONG)load_bmp(hb_parc(1),pal));
}

HB_FUNC(STOP_SAMPLE){
   SAMPLE *temp=(SAMPLE *)hb_parnl(1);
   stop_sample(temp);
}
HB_FUNC(PLAY_SAMPLE){
   SAMPLE *temp=(SAMPLE *)hb_parnl(1);
   int vol=hb_parni(2);
   int pan=hb_parni(3);
   int freq=hb_parni(4);
   int loop=hb_parni(5);
   if (temp){
      hb_retni(play_sample(temp,vol,pan,freq,loop));
   }else{hb_retni(-1);}
}

HB_FUNC(DESTROY_SAMPLE){
   SAMPLE *temp=(SAMPLE *)hb_parnl(1);
   destroy_sample(temp);
}

/*
HB_FUNC(_DESTROY_BITMAP){
   destroy_bitmap((BITMAP *)hb_parnl(1));
}
*/
/*
HB_FUNC(_BLIT){
    BITMAP *dest=(BITMAP *)hb_parnl(1);
    BITMAP *source=(BITMAP *)hb_parnl(2);
    if ((dest==0) || (source==0)){return;}
    int x=hb_parni(3);
    int y=hb_parni(4);
    int xi=hb_parni(5);
    int yi=hb_parni(6);
    int xie=hb_parni(7);
    int yie=hb_parni(8);
    blit(dest,source,x,y,xi,yi,xie,yie);
}
*/
/*
HB_FUNC(_DRAW_SPRITE_){
   BITMAP *dest;
   BITMAP *font;
   int x,y;
   dest=(BITMAP *)hb_parnl(1);
   font=(BITMAP *)hb_parnl(2);
   if ((dest==0) || (font==0)){return;}
   y=hb_parnl(3);
   x=hb_parnl(4);
   draw_sprite(dest,font,y,x);
}
*/

HB_FUNC(_DRAW_SPRITE_H_FLIP){
   BITMAP *dest;
   BITMAP *font;
   int x,y;
   dest=(BITMAP *)hb_parnl(1);
   font=(BITMAP *)hb_parnl(2);
   if ((dest==0) || (font==0)){return;}
   y=hb_parnl(3);
   x=hb_parnl(4);
   draw_sprite_h_flip(dest,font,y,x);
}

HB_FUNC(_DRAW_TRANS_SPRITE){
   BITMAP *dest;
   BITMAP *font;
   int x,y;
   dest=(BITMAP *)hb_parnl(1);
   font=(BITMAP *)hb_parnl(2);
   if ((dest==0) || (font==0)){return;}
   y=hb_parnl(3);
   x=hb_parnl(4);
   draw_trans_sprite(dest,font,y,x);
}
HB_FUNC(_GET_BMP_Y)
{
    BITMAP *temp;
    temp=(BITMAP *)hb_parnl(1);
    hb_retnl((HB_ULONG)temp->h);
}

HB_FUNC(_GET_BMP_X)
{
    BITMAP *temp;
    temp=(BITMAP *)hb_parnl(1);
    hb_retnl((HB_ULONG)temp->w);
}

/*
HB_FUNC(_CREATE_BITMAP){
   BITMAP *temp;
   temp=create_bitmap(hb_parnl(1),hb_parnl(2));
   hb_retnl((HB_ULONG)temp);
}
*/

/*
HB_FUNC(_CREATE_BITMAP_EX){
   BITMAP *temp;
   temp=create_bitmap_ex(hb_parnl(1),hb_parnl(2),hb_parnl(3));
   hb_retnl((HB_ULONG)temp);
}*/

HB_FUNC(_SCARE_MOUSE){
    scare_mouse();  
}

HB_FUNC(_UNSCARE_MOUSE){
    unscare_mouse();  
}

HB_FUNC(_OPEN_FLI){
   hb_retnl((HB_ULONG)open_fli(hb_parc(1)));
}
HB_FUNC(_CLOSE_FLI){
   close_fli();
}

HB_FUNC(_FLI_BITMAP){
   hb_retnl((HB_ULONG)fli_bitmap);
}
HB_FUNC(_NEXT_FLI_FRAME){
   hb_retnl((HB_ULONG)next_fli_frame((int)hb_parni(1)));
}

HB_FUNC(START_ALLEGRO_OPTIONS)
{
   register_png_file_type();
}


HB_FUNC(INSTALL_MOUSE)
{
	install_mouse();
}

HB_FUNC(REMOVE_MOUSE)
{
	remove_mouse();
}

HB_FUNC(INSTALL_TIMER)
{
	install_timer();
}

HB_FUNC(REMOVE_TIMER)
{
	remove_timer();
}

//HB_FUNC(SHOW_MOUSE){
//   show_mouse((BITMAP *)hb_parnl(1));
//}

HB_FUNC(SHOW_MOUSE_NULL){
      enable_hardware_cursor();

}

#pragma ENDDUMP


