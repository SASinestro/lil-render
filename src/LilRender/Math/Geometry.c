#include "Geometry.h"

#include <stdlib.h>

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

    if (c < 1)
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
