
#ifndef HBRAYLIB_CH_
#define HBRAYLIB_CH_

// Some Basic Colors
// NOTE: Custom raylib color palette for amazing visuals on WHITE background
#define LIGHTGRAY  { 200, 200, 200, 255 }   // Light Gray
#define GRAY       { 130, 130, 130, 255 }   // Gray
#define DARKGRAY   { 80, 80, 80, 255 }      // Dark Gray
#define YELLOW     { 253, 249, 0, 255 }     // Yellow
#define GOLD       { 255, 203, 0, 255 }     // Gold
#define ORANGE     { 255, 161, 0, 255 }     // Orange
#define PINK       { 255, 109, 194, 255 }   // Pink
#define RED        { 230, 41, 55, 255 }     // Red
#define MAROON     { 190, 33, 55, 255 }     // Maroon
#define GREEN      { 0, 228, 48, 255 }      // Green
#define LIME       { 0, 158, 47, 255 }      // Lime
#define DARKGREEN  { 0, 117, 44, 255 }      // Dark Green
#define SKYBLUE    { 102, 191, 255, 255 }   // Sky Blue
#define BLUE       { 0, 121, 241, 255 }     // Blue
#define DARKBLUE   { 0, 82, 172, 255 }      // Dark Blue
#define PURPLE     { 200, 122, 255, 255 }   // Purple
#define VIOLET     { 135, 60, 190, 255 }    // Violet
#define DARKPURPLE { 112, 31, 126, 255 }    // Dark Purple
#define BEIGE      { 211, 176, 131, 255 }   // Beige
#define BROWN      { 127, 106, 79, 255 }    // Brown
#define DARKBROWN  { 76, 63, 47, 255 }      // Dark Brown

#define WHITE      { 255, 255, 255, 255 }   // White
#define BLACK      { 0, 0, 0, 255 }         // Black
#define BLANK      { 0, 0, 0, 0 }           // Blank (Transparent)
#define MAGENTA    { 255, 0, 255, 255 }     // Magenta
#define RAYWHITE   { 245, 245, 245, 255 }   // My own White (raylib logo)

//----------------------------------------------------------------------------------
// Enumerators Definition
//----------------------------------------------------------------------------------
// System/Window config flags
// NOTE: Every bit registers one state (use it with bit masks)
// By default all flags are set to 0
#define FLAG_VSYNC_HINT           0x00000040   // Set to try enabling V-Sync on GPU
#define FLAG_FULLSCREEN_MODE      0x00000002   // Set to run program in fullscreen
#define FLAG_WINDOW_RESIZABLE     0x00000004   // Set to allow resizable window
#define FLAG_WINDOW_UNDECORATED   0x00000008   // Set to disable window decoration (frame and buttons)
#define FLAG_WINDOW_HIDDEN        0x00000080   // Set to hide window
#define FLAG_WINDOW_MINIMIZED     0x00000200   // Set to minimize window (iconify)
#define FLAG_WINDOW_MAXIMIZED     0x00000400   // Set to maximize window (expanded to monitor)
#define FLAG_WINDOW_UNFOCUSED     0x00000800   // Set to window non focused
#define FLAG_WINDOW_TOPMOST       0x00001000   // Set to window always on top
#define FLAG_WINDOW_ALWAYS_RUN    0x00000100   // Set to allow windows running while minimized
#define FLAG_WINDOW_TRANSPARENT   0x00000010   // Set to allow transparent framebuffer
#define FLAG_WINDOW_HIGHDPI       0x00002000   // Set to support HighDPI
#define FLAG_MSAA_4X_HINT         0x00000020   // Set to try enabling MSAA 4X
#define FLAG_INTERLACED_HINT      0x00010000   // Set to try enabling interlaced video format (for V3D)

// Trace log type
#define LOG_ALL              0        // Display all logs
#define LOG_TRACE            1
#define LOG_DEBUG            2
#define LOG_INFO             3
#define LOG_WARNING          4
#define LOG_ERROR            5
#define LOG_FATAL            6
#define LOG_NONE             7            // Disable logging#define

// Keyboard keys (US keyboard layout)
// NOTE: Use GetKeyPressed() to allow redefining
// required keys for alternative layouts

// Alphanumeric keys
#define KEY_NULL             0
#define KEY_APOSTROPHE       39
#define KEY_COMMA            44
#define KEY_MINUS            45
#define KEY_PERIOD           46
#define KEY_SLASH            47
#define KEY_ZERO             48
#define KEY_ONE              49
#define KEY_TWO              50
#define KEY_THREE            51
#define KEY_FOUR             52
#define KEY_FIVE             53
#define KEY_SIX              54
#define KEY_SEVEN            55
#define KEY_EIGHT            56
#define KEY_NINE             57
#define KEY_SEMICOLON        59
#define KEY_EQUAL            61
#define KEY_A                65
#define KEY_B                66
#define KEY_C                67
#define KEY_D                68
#define KEY_E                69
#define KEY_F                70
#define KEY_G                71
#define KEY_H                72
#define KEY_I                73
#define KEY_J                74
#define KEY_K                75
#define KEY_L                76
#define KEY_M                77
#define KEY_N                78
#define KEY_O                79
#define KEY_P                80
#define KEY_Q                81
#define KEY_R                82
#define KEY_S                83
#define KEY_T                84
#define KEY_U                85
#define KEY_V                86
#define KEY_W                87
#define KEY_X                88
#define KEY_Y                89
#define KEY_Z                90

// Function keys
#define KEY_SPACE            32
#define KEY_ESCAPE           256
#define KEY_ENTER            257
#define KEY_TAB              258
#define KEY_BACKSPACE        259
#define KEY_INSERT           260
#define KEY_DELETE           261
#define KEY_RIGHT            262
#define KEY_LEFT             263
#define KEY_DOWN             264
#define KEY_UP               265
#define KEY_PAGE_UP          266
#define KEY_PAGE_DOWN        267
#define KEY_HOME             268
#define KEY_END              269
#define KEY_CAPS_LOCK        280
#define KEY_SCROLL_LOCK      281
#define KEY_NUM_LOCK         282
#define KEY_PRINT_SCREEN     283
#define KEY_PAUSE            284
#define KEY_F1               290
#define KEY_F2               291
#define KEY_F3               292
#define KEY_F4               293
#define KEY_F5               294
#define KEY_F6               295
#define KEY_F7               296
#define KEY_F8               297
#define KEY_F9               298
#define KEY_F10              299
#define KEY_F11              300
#define KEY_F12              301
#define KEY_LEFT_SHIFT       340
#define KEY_LEFT_CONTROL     341
#define KEY_LEFT_ALT         342
#define KEY_LEFT_SUPER       343
#define KEY_RIGHT_SHIFT      344
#define KEY_RIGHT_CONTROL    345
#define KEY_RIGHT_ALT        346
#define KEY_RIGHT_SUPER      347
#define KEY_KB_MENU          348
#define KEY_LEFT_BRACKET     91
#define KEY_BACKSLASH        92
#define KEY_RIGHT_BRACKET    93
#define KEY_GRAVE            96

// Keypad keys
#define KEY_KP_0             320
#define KEY_KP_1             321
#define KEY_KP_2             322
#define KEY_KP_3             323
#define KEY_KP_4             324
#define KEY_KP_5             325
#define KEY_KP_6             326
#define KEY_KP_7             327
#define KEY_KP_8             328
#define KEY_KP_9             329
#define KEY_KP_DECIMAL       330
#define KEY_KP_DIVIDE        331
#define KEY_KP_MULTIPLY      332
#define KEY_KP_SUBTRACT      333
#define KEY_KP_ADD           334
#define KEY_KP_ENTER         335
#define KEY_KP_EQUAL         336

// Android buttons
#define KEY_BACK             4
#define KEY_MENU             82
#define KEY_VOLUME_UP        24
#define KEY_VOLUME_DOWN      25

// Mouse buttons
#define MOUSE_LEFT_BUTTON    0
#define MOUSE_RIGHT_BUTTON   1
#define MOUSE_MIDDLE_BUTTON  2

// Mouse cursor types
#define MOUSE_CURSOR_DEFAULT         0
#define MOUSE_CURSOR_ARROW           1
#define MOUSE_CURSOR_IBEAM           2
#define MOUSE_CURSOR_CROSSHAIR       3
#define MOUSE_CURSOR_POINTING_HAND   4
#define MOUSE_CURSOR_RESIZE_EW       5     // The horizontal resize/move arrow shape
#define MOUSE_CURSOR_RESIZE_NS       6     // The vertical resize/move arrow shape
#define MOUSE_CURSOR_RESIZE_NWSE     7     // The top-left to bottom-right diagonal resize/move arrow shape
#define MOUSE_CURSOR_RESIZE_NESW     8     // The top-right to bottom-left diagonal resize/move arrow shape
#define MOUSE_CURSOR_RESIZE_ALL      9     // The omni-directional resize/move cursor shape
#define MOUSE_CURSOR_NOT_ALLOWED     10     // The operation-not-allowed shape#define

// Mouse buttons
#define MOUSE_BUTTON_LEFT    0
#define MOUSE_BUTTON_RIGHT   1
#define MOUSE_BUTTON_MIDDLE  2
#define MOUSE_BUTTON_SIDE    3
#define MOUSE_BUTTON_EXTRA   4
#define MOUSE_BUTTON_FORWARD 5
#define MOUSE_BUTTON_BACK    6

// Gamepad buttons
// This is here just for error checking
#define GAMEPAD_BUTTON_UNKNOWN          0

// This is normally a DPAD
#define GAMEPAD_BUTTON_LEFT_FACE_UP     1
#define GAMEPAD_BUTTON_LEFT_FACE_RIGHT  2
#define GAMEPAD_BUTTON_LEFT_FACE_DOWN   3
#define GAMEPAD_BUTTON_LEFT_FACE_LEFT   4

// This normally corresponds with PlayStation and Xbox controllers
// XBOX: [YXAB]
// PS3: [TriangleSquareCrossCircle]
// No support for 6 button controllers though..
#define GAMEPAD_BUTTON_RIGHT_FACE_UP     5
#define GAMEPAD_BUTTON_RIGHT_FACE_RIGHT  6
#define GAMEPAD_BUTTON_RIGHT_FACE_DOWN   7
#define GAMEPAD_BUTTON_RIGHT_FACE_LEFT   8

// Triggers
#define GAMEPAD_BUTTON_LEFT_TRIGGER_1    9
#define GAMEPAD_BUTTON_LEFT_TRIGGER_2    10
#define GAMEPAD_BUTTON_RIGHT_TRIGGER_1   11
#define GAMEPAD_BUTTON_RIGHT_TRIGGER_2   12

// These are buttons in the center of the gamepad
#define GAMEPAD_BUTTON_MIDDLE_LEFT       13 //PS3 Select
#define GAMEPAD_BUTTON_MIDDLE            14 //PS Button/XBOX Button
#define GAMEPAD_BUTTON_MIDDLE_RIGHT      15 //PS3 Start

// These are the joystick press in buttons
#define GAMEPAD_BUTTON_LEFT_THUMB        16
#define GAMEPAD_BUTTON_RIGHT_THUMB       17

// Gamepad axis
// Left stick
#define GAMEPAD_AXIS_LEFT_X   0
#define GAMEPAD_AXIS_LEFT_Y   1

// Right stick
#define GAMEPAD_AXIS_RIGHT_X  2
#define GAMEPAD_AXIS_RIGHT_Y  3

// Pressure levels for the back triggers
#define GAMEPAD_AXIS_LEFT_TRIGGER     4      // [1..-1] (pressure-level)
#define GAMEPAD_AXIS_RIGHT_TRIGGER    5      // [1..-1] (pressure-level)

// Material map index
#define MATERIAL_MAP_ALBEDO           0     // MATERIAL_MAP_DIFFUSE
#define MATERIAL_MAP_METALNESS        1     // MATERIAL_MAP_SPECULAR
#define MATERIAL_MAP_NORMAL           2 
#define MATERIAL_MAP_ROUGHNESS        3
#define MATERIAL_MAP_OCCLUSION        4
#define MATERIAL_MAP_EMISSION         5
#define MATERIAL_MAP_HEIGHT           6
#define MATERIAL_MAP_BRDG             7
#define MATERIAL_MAP_CUBEMAP          8     // NOTE: Uses GL_TEXTURE_CUBE_MAP
#define MATERIAL_MAP_IRRADIANCE       9     // NOTE: Uses GL_TEXTURE_CUBE_MAP
#define MATERIAL_MAP_PREFILTER       10     // NOTE: Uses GL_TEXTURE_CUBE_MAP

#define MATERIAL_MAP_DIFFUSE         MATERIAL_MAP_ALBEDO
#define MATERIAL_MAP_SPECULAR        MATERIAL_MAP_METALNESS

// Shader location index
#define SHADER_LOC_VERTEX_POSITION    0
#define SHADER_LOC_VERTEX_TEXCOORD01  1
#define SHADER_LOC_VERTEX_TEXCOORD02  2
#define SHADER_LOC_VERTEX_NORMAL      3
#define SHADER_LOC_VERTEX_TANGENT     4
#define SHADER_LOC_VERTEX_COLOR       5
#define SHADER_LOC_MATRIX_MVP         6
#define SHADER_LOC_MATRIX_VIEW        7
#define SHADER_LOC_MATRIX_PROJECTION  8
#define SHADER_LOC_MATRIX_MODEL       9
#define SHADER_LOC_MATRIX_NORMAL     10 
#define SHADER_LOC_VECTOR_VIEW       11
#define SHADER_LOC_COLOR_DIFFUSE     12
#define SHADER_LOC_COLOR_SPECULAR    13
#define SHADER_LOC_COLOR_AMBIENT     14 
#define SHADER_LOC_MAP_ALBEDO        15        // SHADER_LOC_MAP_DIFFUSE
#define SHADER_LOC_MAP_METALNESS     16       // SHADER_LOC_MAP_SPECULAR
#define SHADER_LOC_MAP_NORMAL        17
#define SHADER_LOC_MAP_ROUGHNESS     18
#define SHADER_LOC_MAP_OCCLUSION     19
#define SHADER_LOC_MAP_EMISSION      20
#define SHADER_LOC_MAP_HEIGHT        21
#define SHADER_LOC_MAP_CUBEMAP       22
#define SHADER_LOC_MAP_IRRADIANCE    23
#define SHADER_LOC_MAP_PREFILTER     24
#define SHADER_LOC_MAP_BRDF          25

#define SHADER_LOC_MAP_DIFFUSE       SHADER_LOC_MAP_ALBEDO
#define SHADER_LOC_MAP_SPECULAR      SHADER_LOC_MAP_METALNESS

// Shader uniform data types
#define UNIFORM_FLOAT       0
#define UNIFORM_VEC2        1
#define UNIFORM_VEC3        2
#define UNIFORM_VEC4        3
#define UNIFORM_INT         4
#define UNIFORM_IVEC2       5
#define UNIFORM_IVEC3       6
#define UNIFORM_IVEC4       7
#define UNIFORM_SAMPLER2D   8

// Pixel formats
// NOTE: Support depends on OpenGL version and platform
#define PIXELFORMAT_UNCOMPRESSED_GRAYSCALE      1    // 8 bit per pixel (no alpha)
#define PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA     2    // 8*2 bpp (2 channels)
#define PIXELFORMAT_UNCOMPRESSED_R5G6B5         3    // 16 bpp
#define PIXELFORMAT_UNCOMPRESSED_R8G8B8         4    // 24 bpp
#define PIXELFORMAT_UNCOMPRESSED_R5G5B5A1       5    // 16 bpp (1 bit alpha)
#define PIXELFORMAT_UNCOMPRESSED_R4G4B4A4       6    // 16 bpp (4 bit alpha)
#define PIXELFORMAT_UNCOMPRESSED_R8G8B8A8       7    // 32 bpp
#define PIXELFORMAT_UNCOMPRESSED_R32            8    // 32 bpp (1 channel - float)
#define PIXELFORMAT_UNCOMPRESSED_R32G32B32      9    // 32*3 bpp (3 channels - float)
#define PIXELFORMAT_UNCOMPRESSED_R32G32B32A32  10    // 32*4 bpp (4 channels - float)
#define PIXELFORMAT_COMPRESSED_DXT1_RGB        11     // 4 bpp (no alpha)
#define PIXELFORMAT_COMPRESSED_DXT1_RGBA       12     // 4 bpp (1 bit alpha)
#define PIXELFORMAT_COMPRESSED_DXT3_RGBA       13     // 8 bpp
#define PIXELFORMAT_COMPRESSED_DXT5_RGBA       14     // 8 bpp
#define PIXELFORMAT_COMPRESSED_ETC1_RGB        15     // 4 bpp
#define PIXELFORMAT_COMPRESSED_ETC2_RGB        16     // 4 bpp
#define PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA   17     // 8 bpp
#define PIXELFORMAT_COMPRESSED_PVRT_RGB        18     // 4 bpp
#define PIXELFORMAT_COMPRESSED_PVRT_RGBA       19     // 4 bpp
#define PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA   20     // 8 bpp
#define PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA   21     // 2 bpp

// Texture parameters: filter mode
// NOTE 1: Filtering considers mipmaps if available in the texture
// NOTE 2: Filter is accordingly set for minification and magnification
#define TEXTURE_FILTER_POINT             0      // No filter, just pixel aproximation
#define TEXTURE_FILTER_BILINEAR          1      // Linear filtering
#define TEXTURE_FILTER_TRILINEAR         2      // Trilinear filtering (linear with mipmaps)
#define TEXTURE_FILTER_ANISOTROPIC_4X    3      // Anisotropic filtering 4x
#define TEXTURE_FILTER_ANISOTROPIC_8X    4      // Anisotropic filtering 8x
#define TEXTURE_FILTER_ANISOTROPIC_16X   5      // Anisotropic filtering 16x

// Texture parameters: wrap mode
#define TEXTURE_WRAP_REPEAT              0        // Repeats texture in tiled mode
#define TEXTURE_WRAP_CLAMP               1        // Clamps texture to edge pixel in tiled mode
#define TEXTURE_WRAP_MIRROR_REPEAT       2        // Mirrors and repeats the texture in tiled mode
#define TEXTURE_WRAP_MIRROR_CLAMP        3        // Mirrors and clamps to border the texture in tiled mode

// Cubemap layouts
#define CUBEMAP_LAYOUT_AUTO_DETECT           0   // Automatically detect layout type
#define CUBEMAP_LAYOUT_LINE_VERTICAL         1   // Layout is defined by a vertical line with faces
#define CUBEMAP_LAYOUT_LINE_HORIZONTAL       2   // Layout is defined by an horizontal line with faces
#define CUBEMAP_LAYOUT_CROSS_THREE_BY_FOUR   3   // Layout is defined by a 3x4 cross with cubemap faces
#define CUBEMAP_LAYOUT_CROSS_FOUR_BY_THREE   4   // Layout is defined by a 4x3 cross with cubemap faces
#define CUBEMAP_LAYOUT_PANORAMA              5   // Layout is defined by a panorama image (equirectangular map)

// Font type defines generation method
#define FONT_DEFAULT   0       // Default font generation anti-aliased
#define FONT_BITMAP    1       // Bitmap font generation no anti-aliasing
#define FONT_SDF       2       // SDF font generation requires external shader

// Color blending modes (pre-defined)
#define BLEND_ALPHA             0  // Blend textures considering alpha (default)
#define BLEND_ADDITIVE          1  // Blend textures adding colors
#define BLEND_MULTIPLIED        2  // Blend textures multiplying colors
#define BLEND_ADD_COLORS        3  // Blend textures adding colors (alternative)
#define BLEND_SUBTRACT_COLORS   4  // Blend textures subtracting colors (alternative)
#define BLEND_CUSTOM            5  // Belnd textures using custom src/dst factors (use SetBlendModeCustom())

// Gestures type
// NOTE: It could be used as flags to enable only some gestures
#define GESTURE_NONE            0
#define GESTURE_TAP             1
#define GESTURE_DOUBLETAP       2
#define GESTURE_HOLD            4
#define GESTURE_DRAG            8
#define GESTURE_SWIPE_RIGHT     16
#define GESTURE_SWIPE_LEFT      32
#define GESTURE_SWIPE_UP        64
#define GESTURE_SWIPE_DOWN      128
#define GESTURE_PINCH_IN        256
#define GESTURE_PINCH_OUT       512

// Camera system modes
#define CAMERA_CUSTOM           0
#define CAMERA_FREE             1
#define CAMERA_ORBITAL          2
#define CAMERA_FIRST_PERSON     3
#define CAMERA_THIRD_PERSON     4

// Camera projection modes
#define CAMERA_PERSPECTIVE      0
#define CAMERA_ORTHOGRAPHIC     1

// N-patch layout
#define NPATCH_NINE_PATCH             0   // Npatch layout: 3x3 tiles
#define NPATCH_THREE_PATCH_VERTICAL   1   // Npatch layout: 1x3 tiles
#define NPATCH_THREE_PATCH_HORIZONTAL 2   // Npatch layout: 3x1 tiles

#endif /* HBRAYLIB_CH_ */
