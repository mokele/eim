#include <stdio.h>
#include <stdlib.h>
#include <cmath>
#include <wand/MagickWand.h>
#include "erl_nif_compat.h"

#define ThrowWandException(wand) \
{ \
  magick_wand=DestroyMagickWand(magick_wand); \
  MagickWandTerminus(); \
  throw("An error occured"); \
}
    
class eim_image
{
public:
    eim_image(const void *data, const size_t size)
        : data_(data),
          size_(size)
    {
        MagickWandGenesis();
        magick_wand=NewMagickWand();
        //status=MagickReadImage(magick_wand,"../priv/fibula.jpg");
        status=MagickReadImageBlob(magick_wand, data_, size_);
        if (status == MagickFalse) {
            throw("An error occured");
        }
        long int x,y;
        status=MagickGetImagePage(magick_wand,&width_,&height_,&x,&y);
        if (status == MagickFalse) {
            throw("An error occured");
        }
    }
    
    void scale_width(size_t width)
    {
        MagickResetIterator(magick_wand);
        while (MagickNextImage(magick_wand) != MagickFalse) {
            size_t height = (size_t)(height_ * ((double)width/width_));
            MagickResizeImage(magick_wand,width,height,LanczosFilter,1.0);
        }
    }
    void scale_height(size_t height)
    {
        MagickResetIterator(magick_wand);
        while (MagickNextImage(magick_wand) != MagickFalse) {
            size_t width = (size_t)(width_ * ((double)height/height_));
            MagickResizeImage(magick_wand,width,height,LanczosFilter,1.0);
        }
    }
    
    unsigned char* process(size_t *new_length)
    {
        status=MagickSetImageFormat(magick_wand, "jpg");
        if (status == MagickFalse) {
            throw("An error occured");
        }
        unsigned char *new_blob;
        new_blob = MagickGetImageBlob(magick_wand,new_length);

        magick_wand=DestroyMagickWand(magick_wand);
        MagickWandTerminus();
        return new_blob;
    }
    virtual ~eim_image()
    {
        magick_wand=DestroyMagickWand(magick_wand);
        MagickWandTerminus();
    }
    
protected:
    const void *data_;
    const size_t size_;
    MagickWand *magick_wand;
    MagickBooleanType status;
    long unsigned int width_,height_;
};

