#include "matrix.h"


void matmult4x4(double *a, double *b, double *out)
{
    for (int x = 0; x < 4; x++)
    {
        for (int y = 0; y < 4; y++)
        {
            out[4*x+y] = 0;

            for (int z = 0; z < 4; z++)
            {
                out[4*x+y] += a[4*z+y] * b[4*x+z];
            }
        }
    }
}
