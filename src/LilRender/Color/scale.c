#include "scale.h"

#include <math.h>
#include <stdlib.h>

#define min(a, b) ((a < b) ? a : b)
#define max(a, b) ((a > b) ? a : b)

void scale_color(char *color, double scale, char *out)
{
    scale = max(0.0, min(1.0, scale));

    for (int i = 0; i < 3; i++)
        out[i] = color[i] * scale;
}
