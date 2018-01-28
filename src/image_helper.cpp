#include "image_helper.h"

bool IsOverThresholdFrame(unsigned short *piData, unsigned short threshold)
{
  for(int i = 0; i < IMAGE_SIZE; i++)
    if (piData[i] > threshold)
      return true;

    return false;
}

bool IsZeroFrame(unsigned short *piData) {
  long sumData;

  for(int i = 0; i < IMAGE_SIZE; i++) {
//  if (piData[0] != 0) return false; ???
    sumData += piData[i];
  }
    if (sumData == 0)
      return true;

    return false;
}
