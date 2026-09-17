
#ifndef HBRAYGUI_CH_
#define HBRAYGUI_CH_

// Gui control state
#define GUI_STATE_NORMAL     0
#define GUI_STATE_FOCUSED    1
#define GUI_STATE_PRESSED    2
#define GUI_STATE_DISABLED   3

// Gui control text alignment
#define GUI_TEXT_ALIGN_LEFT    0
#define GUI_TEXT_ALIGN_CENTER  1
#define GUI_TEXT_ALIGN_RIGHT   2

// Gui controls
#define DEFAULT       0
#define LABEL         1    // LABELBUTTON
#define BUTTON        2    // IMAGEBUTTON
#define TOGGLE        3    // TOGGLEGROUP
#define SLIDER        4    // SLIDERBAR
#define PROGRESSBAR   5
#define CHECKBOX      6
#define COMBOBOX      7
#define DROPDOWNBOX   8
#define TEXTBOX       9    // TEXTBOXMULTI
#define VALUEBOX      10
#define SPINNER       11
#define LISTVIEW      12
#define COLORPICKER   13
#define SCROLLBAR     14
#define STATUSBAR     15

// Gui base properties for every control
#define BORDER_COLOR_NORMAL    0
#define BASE_COLOR_NORMAL      1
#define TEXT_COLOR_NORMAL      2
#define BORDER_COLOR_FOCUSED   3
#define BASE_COLOR_FOCUSED     4
#define TEXT_COLOR_FOCUSED     5
#define BORDER_COLOR_PRESSED   6
#define BASE_COLOR_PRESSED     7
#define TEXT_COLOR_PRESSED     8
#define BORDER_COLOR_DISABLED  9
#define BASE_COLOR_DISABLED    10
#define TEXT_COLOR_DISABLED    11
#define BORDER_WIDTH           12
#define TEXT_PADDING           13
#define TEXT_ALIGNMENT         14
#define RESERVED               15

// Gui extended properties depend on control
// NOTE: We reserve a fixed size of additional properties per control

// DEFAULT properties
#define TEXT_SIZE          16
#define TEXT_SPACING       17
#define LINE_COLOR         18
#define BACKGROUND_COLOR   19

// Label
//typedef enum { } GuiLabelProperty;

// Button
//typedef enum { } GuiButtonProperty;

// Toggle / ToggleGroup
#define GROUP_PADDING   16

// Slider / SliderBar
#define SLIDER_WIDTH     16
#define SLIDER_PADDING   17

// ProgressBar
#define PROGRESS_PADDING   16

// CheckBox
#define CHECK_PADDING   16

// ComboBox
#define COMBO_BUTTON_WIDTH     16
#define COMBO_BUTTON_PADDING   17

// DropdownBox
#define ARROW_PADDING            16
#define DROPDOWN_ITEMS_PADDING   17

// TextBox / TextBoxMulti / ValueBox / Spinner
#define TEXT_INNER_PADDING   16
#define TEXT_LINES_PADDING   17
#define COLOR_SELECTED_FG    18
#define COLOR_SELECTED_BG    19

// Spinner
#define SPIN_BUTTON_WIDTH     16
#define SPIN_BUTTON_PADDING   17

// ScrollBar
#define ARROWS_SIZE             16
#define ARROWS_VISIBLE          17
#define SCROLL_SLIDER_PADDING   18
#define SCROLL_SLIDER_SIZE      19
#define SCROLL_PADDING          20
#define SCROLL_SPEED            21

// ScrollBar side
#define SCROLLBAR_LEFT_SIDE    0
#define SCROLLBAR_RIGHT_SIDE   1

#define LIST_ITEMS_HEIGHT    16
#define LIST_ITEMS_PADDING   17
#define SCROLLBAR_WIDTH      18
#define SCROLLBAR_SIDE       19

// ColorPicker
#define COLOR_SELECTOR_SIZE        16
#define HUEBAR_WIDTH               17    // Right hue bar width
#define HUEBAR_PADDING             18    // Right hue bar separation from panel
#define HUEBAR_SELECTOR_HEIGHT     19    // Right hue bar selector height
#define HUEBAR_SELECTOR_OVERFLOW   20    // Right hue bar selector overflow

#endif /* HBRAYGUI_CH_ */