/*
 *  libgfx2 - FreeBASIC's alternative gfx library
 *	Copyright (C) 2005 Angelo Mottola (a.mottola@libero.it)
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/*
 * vga.c -- VGA gfx driver
 *
 * chng: apr/2005 written [DrV]
 *
 */

#include "fb_gfx_dos.h"

static int driver_init(char *title, int w, int h, int depth, int refresh_rate, int flags);
static void driver_update(void);
static void end_of_driver_update(void);

GFXDRIVER fb_gfxDriverVGA =
{
	"VGA",			/* char *name; */
	driver_init,		/* int (*init)(char *title, int w, int h, int depth, int refresh_rate, int flags); */
	fb_dos_exit,		/* void (*exit)(void); */
	fb_dos_lock,		/* void (*lock)(void); */
	fb_dos_unlock,		/* void (*unlock)(void); */
	fb_dos_set_palette,	/* void (*set_palette)(int index, int r, int g, int b); */
	fb_dos_vga_wait_vsync,	/* void (*wait_vsync)(void); */
	fb_dos_get_mouse,	/* int (*get_mouse)(int *x, int *y, int *z, int *buttons); */
	fb_dos_set_mouse,	/* void (*set_mouse)(int x, int y, int cursor); */
	fb_dos_set_window_title,/* void (*set_window_title)(char *title); */
	NULL			/* void (*flip)(void); */
};

/*:::::*/
static int driver_init(char *title, int w, int h, int depth, int refresh_rate, int flags)
{
	fb_dos_detect();
	
	if (flags & DRIVER_OPENGL)
		return -1;
	
	if ((w != 320) || (h != 200) || (depth != 8)) {
		return -1;
	}
	
	/* set video mode */
	fb_dos.regs.x.ax = 0x13;
	__dpmi_int(0x10, &fb_dos.regs);
	
	fb_dos.update = driver_update;
	fb_dos.update_len = (unsigned int)end_of_driver_update - (unsigned int)driver_update;
	fb_dos.set_palette = fb_dos_vga_set_palette;
	
	fb_dos_init(title, w, h, depth, 70, flags);
	
	return 0;

}



/*:::::*/
static void driver_update(void)
{
	int y;
	unsigned int buffer = (unsigned int)fb_mode->framebuffer;
	unsigned int screen = 0xA0000;
	
	for (y = 0; y < fb_dos.h; y++, buffer += fb_dos.w, screen += fb_dos.w) {
		if (fb_mode->dirty[y]) {
			movedata(_my_ds(), buffer, _dos_ds, screen, fb_dos.w);
		}
	}
}

static void end_of_driver_update(void) { /* do not remove */ }

