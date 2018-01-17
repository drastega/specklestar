#include "image_helper.h"

bool IsUnderThresholdFrame(unsigned short *piData, unsigned short threshold)
{
  for(int i = 0; i < IMAGE_SIZE; i++)
    if (threshold < piData[i])
      return true;

    return false;
}
