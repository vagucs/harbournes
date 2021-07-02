#define XHB_BITOP // Habilita das operações | & ^^

#include "xhb.ch"
#include "common.ch"
#include "hbclass.ch"
/*
const (
	ButtonA = iota
	ButtonB
	ButtonSelect
	ButtonStart
	ButtonUp
	ButtonDown
	ButtonLeft
	ButtonRight
)
*/

CREATE CLASS Controller
	VAR buttons INIT  {.f.,.f.,.f.,.f.,.f.,.f.,.f.,.f.} //[8]bool
	VAR index   INIT 0
	VAR strobe  INIT 0

	METHOD New()
	METHOD SetButtons(buttons)
	METHOD Read()
	METHOD Write(value)
END CLASS

METHOD New() CLASS Controller
Return Self

METHOD SetButtons(buttons) CLASS Controller
	::buttons = buttons
   return

METHOD Read() CLASS Controller
	value := 0
	if ::index < 8 .and. ::buttons[::index+1] 
		value = 1
	end if
	::index++
	if (::strobe & 1) == 1
		::index = 0
	end if
	return value


METHOD Write(value) CLASS Controller
	::strobe = value
	if (::strobe & 1) == 1
		::index = 0
	end if
