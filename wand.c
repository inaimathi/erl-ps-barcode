#include <limits.h>
#include <libgen.h>
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <wand/MagickWand.h>

/* A collection of functions useful for image transformation */
/* Only generically useful procedures go here                */

#define ThrowWandException(wand, ret) \
{ \
  char \
    *description; \
 \
  ExceptionType \
    severity; \
 \
  description=MagickGetException(wand,&severity); \
  (void) fprintf(stderr,"%s %s %lu %s\n",GetMagickModule(),description); \
  description=(char *) MagickRelinquishMemory(description); \
  wand=DestroyMagickWand(wand); \
  MagickWandTerminus(); \
  return ret; \
}

char *chop_path(char *orig, char *addition) {
  char buf[PATH_MAX + 1];
  char *res, *new_path;

  res = realpath(orig, buf);
  if (res) {
    new_path = strcat(res, addition);
    return new_path;
  }
  return 0;
}

int determine_padding(int width, int height){
  int w_pad, h_pad;

  w_pad = width / 10;
  h_pad = height / 10;

  if(w_pad > h_pad){
    return round(w_pad);
  } else {
    return round(h_pad);
  }

}

int barcode_to_png (char *image_name) {
  MagickWand *magick_wand;
  MagickBooleanType status;

  int width, height, pad, half_pad;

  /* read a barcode image */
  MagickWandGenesis();
  magick_wand = NewMagickWand();
  status = MagickReadImage(magick_wand, image_name);
  if (status == MagickFalse) ThrowWandException(magick_wand, 1);

  /* trim the image, resample it, and pad it by [10% of the long side] per side */
  MagickTrimImage(magick_wand, 10);
  MagickResampleImage(magick_wand, 300, 300, CatromFilter, 0.5);
  width = MagickGetImageWidth(magick_wand);
  height = MagickGetImageHeight(magick_wand);
  pad = determine_padding(width, height);
  half_pad = round(pad/2);
  MagickExtentImage(magick_wand, width+pad, height+pad, -half_pad, -half_pad);
  
  /* write image (a PNG version and a formatted PS version) */
  status=MagickWriteImage(magick_wand, chop_path(image_name, ".png"));
  if (status == MagickFalse) ThrowWandException(magick_wand, 2);
  status=MagickWriteImage(magick_wand, chop_path(image_name, ".ps"));
  if (status == MagickFalse) ThrowWandException(magick_wand, 2);

  /* clean up */
  magick_wand=DestroyMagickWand(magick_wand);
  MagickWandTerminus();

  return 0;
}

int thumbnail (char *image_name, char *thumbnail_name, int thumb_width, int thumb_height){

  MagickWand *magick_wand;
  MagickBooleanType status;

  /* Read an image. */
  MagickWandGenesis();
  magick_wand=NewMagickWand();
  status=MagickReadImage(magick_wand, image_name);
  if (status == MagickFalse) ThrowWandException(magick_wand, 1);
  
  /* Turn the images into a thumbnail sequence. */
  MagickResetIterator(magick_wand);
  while (MagickNextImage(magick_wand) != MagickFalse)
    MagickResizeImage(magick_wand,thumb_width,thumb_height,LanczosFilter,1.0);

  /* Write the image then destroy it. */
  status=MagickWriteImages(magick_wand, thumbnail_name, MagickTrue);
  if (status == MagickFalse) ThrowWandException(magick_wand, 2);
  magick_wand=DestroyMagickWand(magick_wand);
  MagickWandTerminus();
  
  return 0;
}
