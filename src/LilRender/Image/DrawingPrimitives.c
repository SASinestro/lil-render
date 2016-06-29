#include "DrawingPrimitives.h"

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

#define min(a, b) ((a < b) ? a : b)
#define max(a, b) ((a > b) ? a : b)

double *toBarycentric(double *t_vtx1, double *t_vtx2, double *t_vtx3, double *point)
{
    double *out = malloc(3 * sizeof(double));

    double v1x = t_vtx3[0] - t_vtx1[0];
    double v1y = t_vtx2[0] - t_vtx1[0];
    double v1z = t_vtx1[0] - point[0];

    double v2x = t_vtx3[1] - t_vtx1[1];
    double v2y = t_vtx2[1] - t_vtx1[1];
    double v2z = t_vtx1[1] - point[1];

    double a = (v1y * v2z - v1z * v2y);
    double b = (v1z * v2x - v1x * v2z);
    double c = (v1x * v2y - v1y * v2x);

    if (fabs(c) < 1)
    {
        out[0] = -1;
        out[1] = -1;
        out[2] = -1;
    }
    else
    {
        out[0] = (1 - (a + b)/c);
        out[1] = (b/c);
        out[2] = (a/c);
    }

    return out;
}

void drawTri(char *image, int *z, int width, ColorGetter getter, double *t_vtx1, double *t_vtx2, double *t_vtx3)
{
    int min_x = min( t_vtx1[0], min( t_vtx2[0], t_vtx3[0] ));
    int min_y = min( t_vtx1[1], min( t_vtx2[1], t_vtx3[1] ));

    int max_x = max( t_vtx1[0], max( t_vtx2[0], t_vtx3[0] ));
    int max_y = max( t_vtx1[1], max( t_vtx2[1], t_vtx3[1] ));

    for (int y = min_y; y <= max_y; y++)
    {
        for (int x = min_x; x <= max_x; x++)
        {
            double point[] = {x, y};
            double *bary = toBarycentric(t_vtx1, t_vtx2, t_vtx3, point);

            if (bary[0] >= 0 && bary[1] >= 0 && bary[2] >= 0)
            {
                int newZ = t_vtx1[2] * bary[0] + t_vtx2[2] * bary[1] + t_vtx3[2] * bary[2];
                int idx = (x + y * width)*3;

                if (idx > 1710000)
                    printf("(%i, %i) -> %i\n", x, y, idx);

                if (newZ > z[idx])
                {
                    char *color = getter(bary);

                    if (color[0])
                    {
                        memcpy(&image[idx], &color[1], 3);
                        z[idx] = newZ;
                    }
                }
            }
        }
    }

    printf("Done with C!\n");
}
