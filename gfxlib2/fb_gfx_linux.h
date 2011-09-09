/* common linux internal definitions */

#ifndef __FB_GFX_LINUX_H__
#define __FB_GFX_LINUX_H__

#include <pthread.h>
#include <sys/ipc.h>
#include <sys/shm.h>

#define DOUBLE_CLICK_TIME		250

extern GFXDRIVER fb_gfxDriverFBDev;

extern int fb_hFBDevInfo(int *width, int *height, int *depth, int *refresh);

#endif
