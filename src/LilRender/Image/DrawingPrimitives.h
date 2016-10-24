#ifndef __LILRENDER_DRAWINGPRIMITIVES_H
#define __LILRENDER_DRAWINGPRIMITIVES_H

#include <stdint.h>

typedef uint32_t *(*ColorGetter)(double *, uint32_t *);

void drawTri(uint32_t *image, int *z, int width, ColorGetter getter, double *t_vtx1, double *t_vtx2, double *t_vtx3);
#endif
