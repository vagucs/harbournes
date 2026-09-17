#define XHB_BITOP

#include "nesopt.ch"
#include "xhb.ch"
#include "common.ch"
#include "hbclass.ch"

// Indices dos botoes (0-based no Go, 1-based no Harbour)
#define BTN_A       1
#define BTN_B       2
#define BTN_SELECT  3
#define BTN_START   4
#define BTN_UP      5
#define BTN_DOWN    6
#define BTN_LEFT    7
#define BTN_RIGHT   8

static sF12Down  := .f.
static sF9Down   := .f.
static sF10Down  := .f.
static lPadDebug := .F.
static nFrameSkip := 0

#define FRAMESKIP_MAX  50

CREATE CLASS Controller
   VAR buttons  INIT { .f., .f., .f., .f., .f., .f., .f., .f. }
   VAR index    INIT 0
   VAR strobe   INIT 0
   VAR lastRead INIT 0
   VAR readHits INIT 0

   METHOD New()
   METHOD SetButtons( buttons )
   METHOD Read()
   METHOD Write( value )
   METHOD StateString()
END CLASS

METHOD New() CLASS Controller
   RETURN Self

METHOD SetButtons( buttons ) CLASS Controller
   local i, v
   if buttons != NIL
      for i := 1 to 8
         v := buttons[i]
         if valtype( v ) == "L"
            ::buttons[i] := v
         else
            ::buttons[i] := ( v != 0 )
         end if
      next
   end if
   RETURN

METHOD Read() CLASS Controller
   local value := 0
   if ::index < 8 .and. ::buttons[::index + 1]
      value := 1
   end if
   ::lastRead := value
   ::readHits++
   ::index := ((::index + 1) & 0xFF)
   if (::strobe & 1) == 1
      ::index := 0
   end if
   RETURN value

METHOD Write( value ) CLASS Controller
   ::strobe := (value & 0xFF)
   if (::strobe & 1) == 1
      ::index := 0
   end if
   RETURN

METHOD StateString() CLASS Controller
   local s := ""
   s += iif( ::buttons[BTN_A],      "A", "-" )
   s += iif( ::buttons[BTN_B],      "B", "-" )
   s += iif( ::buttons[BTN_SELECT], "S", "-" )
   s += iif( ::buttons[BTN_START],  "T", "-" )
   s += iif( ::buttons[BTN_UP],     "^", "-" )
   s += iif( ::buttons[BTN_DOWN],   "v", "-" )
   s += iif( ::buttons[BTN_LEFT],   "<", "-" )
   s += iif( ::buttons[BTN_RIGHT],  ">", "-" )
   RETURN s

function ReadNesKeyboard()
   RETURN Read_Nes_Pad1()

function ReadNesKeyboard2()
   RETURN Read_Nes_Pad2()

function ReadNesInput()
   RETURN Read_Nes_Input()

function GetFrameSkip()
   RETURN nFrameSkip

function FrameSkipCount()
   RETURN nFrameSkip + 1

function NES_AdjustFrameSkip( aInput )
   local lF9, lF10
   if aInput != NIL
      lF9  := aInput[4]
      lF10 := aInput[5]
   else
      lF9  := AllegKeyDown( 67 )
      lF10 := AllegKeyDown( 68 )
   end if
   if lF9 .and. ! sF9Down .and. nFrameSkip > 0
      nFrameSkip--
   end if
   if lF10 .and. ! sF10Down .and. nFrameSkip < FRAMESKIP_MAX
      nFrameSkip++
   end if
   sF9Down  := lF9
   sF10Down := lF10
   RETURN NIL

function NES_EscapePressed()
   RETURN AllegEscapePressed()

function PadDebugLine( cLabel, oPad )
   RETURN pad( cLabel + " [" + oPad:StateString() + "] idx=" + str( oPad:index ) + ;
               " st=" + str( oPad:strobe ) + " rd=" + str( oPad:lastRead ) + ;
               " hits=" + str( oPad:readHits ) )

procedure ShowNesControls()
   @ 32, 2 say "Controles P1: Z=A  X=B  Enter=Start  Shift=Select  Setas=Dir"
   @ 33, 2 say "Controles P2: Num1=A Num3=B Num0=Start Num.=Select Num8/4/2/6=Dir"
   @ 34, 2 say "F9/F10=fast-forward skip (0=off, pula render)   ESC=sair"

procedure ShowEmuStatsPanel()
   @  2, 97 say "Frame:   0"
   @  3, 97 say "CPU  :   0"
   @  4, 97 say "Skip :   0"

function NES_ToggleDebugKey()
   local lNow := AllegKeyDown( 88 )  // KEY_F12 (Allegro)
   if lNow .and. ! sF12Down
      TogglePadDebug()
   end if
   sF12Down := lNow
   RETURN NIL

procedure ShowPadDebug( hNes )
   local cRaw
   if ! lPadDebug
      RETURN
   end if
   cRaw := NES_Key_Debug()
   @  5, 97 say PadDebugLine( "P1", hNes:Controller1 )
   @  6, 97 say PadDebugLine( "P2", hNes:Controller2 )
   @  7, 97 say cRaw + space( 20 )
   @  8, 97 say "Legenda: A B Select Start ^ v < >  (letra=pressionado, -=solto)    "

procedure TogglePadDebug()
   lPadDebug := ! lPadDebug
   if ! lPadDebug
      @ 4, 2 clear to 7, 78
   end if
