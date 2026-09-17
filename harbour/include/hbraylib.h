
#ifndef HB_RAYLIB_H_
#define HB_RAYLIB_H_

#include "hbapi.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbstack.h"

#define RAYGUI_IMPLEMENTATION
#define RAYGUI_SUPPORT_ICONS

#include <raylib.h>

HB_EXTERN_BEGIN

extern HB_EXPORT Image* hb_image_param( int iParam );

HB_EXTERN_END

#endif /* HB_RAYLIB_H_ */
