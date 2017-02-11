/* integer log base 2 function */

#include "fb.h"


/*:::::*/
FBCALL int fb_IntLog2_32 ( unsigned int x )
{
	int ret = 0;
	if( x & 0xffff0000 ) { ret += 16; x >>= 16; }
	if( x & 0x0000ff00 ) { ret +=  8; x >>=  8; }
	if( x & 0x000000f0 ) { ret +=  4; x >>=  4; }
	if( x & 0x0000000c ) { ret +=  2; x >>=  2; }
	if( x & 0x00000002 ) { ret +=  1; x >>=  1; }
	return ret;
}

/*:::::*/
FBCALL int fb_IntLog2_64 ( unsigned long long x )
{
	if( x & 0xffffffff00000000ull )
	{
		return 32 + fb_IntLog2_32( (unsigned int)(x >> 32) );
	}
	else
	{
		return fb_IntLog2_32( (unsigned int)x );
	}
}
