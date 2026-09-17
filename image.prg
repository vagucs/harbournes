/*
Emulador NES

Convertido do projeto https://github.com/fogleman/nes

Wagner Nunes da Silva
www.vagucs.com.br

vagucs@gmail.com
vagucs@vagucs.com.br
vagucs@bol.com.br

*/
#include "nesopt.ch"
#include "common.ch"
#include "hbclass.ch"

CREATE CLASS Image
   VAR Buffer
   VAR X
   VAR Y
   VAR X2
   VAR Y2
   METHOD SETRGBA(x,y,z)
   METHOD NEW(x,y,x2,y2)
ENDCLASS

METHOD NEW(y,x,y2,x2) CLASS Image
   ::Buffer=_create_bitmap(y2,x2)
   ::Y=y
   ::X=x

   gr_clear_to_color(::Buffer,makecol(0,0,0))

   Return Self

METHOD SETRGBA(x,y,cor) CLASS Image
   //cor=makecol(z[1],z[2],z[3])
   //if z>1
   //   x=x+100
   //   alert("100")
   //end if
   
   // TV Aspect
   //if x % 2 = 1
   //   if y % 2 = 1
   //      PutPixel(::Buffer,x,y,cor)
   //   end if
   //end if

   // Vertical scanline
   //if x % 2 = 1
   //   PutPixel(::Buffer,x,y,cor)
   //end if

   //Horizontal scanline
   //if y % 2 = 1
   //   PutPixel(::Buffer,x,y,cor)
   //end if
   
   //if GetPixel(::Buffer,x,y)#cor
   //   PutPixel(::Buffer,x,y,cor)
   //end if
   
   //_draw_sprite(_get_buffer(),::Buffer,::Y,::X)
   
#ifdef OTIMIZADO
   PutPixelFast( ::Buffer, x, y, cor )
#else
   PutPixel(::Buffer,x,y,cor)
#endif
   