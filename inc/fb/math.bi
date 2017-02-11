#ifndef __FB_MATH_BI__
#define __FB_MATH_BI__

namespace FB.math
extern "C"

declare function intlog10 overload alias "fb_IntLog10_32" _
	( byval a as ulong ) as long

declare function intlog10 overload alias "fb_IntLog10_64" _
	( byval a as ulongint ) as long

end extern
end namespace

#endif
