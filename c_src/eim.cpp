#include <stdio.h>
#include <stdlib.h>
#include <wand/MagickWand.h>
#include "erl_nif_compat.h"

class eim
{
public:
    static unsigned char* resize(const void *blob, const size_t length, int width, int height, 
        size_t *new_length)
    {
        #define ThrowWandException(wand) \
        { \
          magick_wand=DestroyMagickWand(magick_wand); \
          MagickWandTerminus(); \
          throw("An error occured"); \
        }

        MagickBooleanType status;
        MagickWand *magick_wand;
        
        MagickWandGenesis();
        magick_wand=NewMagickWand();
        //status=MagickReadImage(magick_wand,"../priv/fibula.jpg");
        status=MagickReadImageBlob(magick_wand, blob, length);
        if (status == MagickFalse) {
            ThrowWandException(magick_wand);
        }

        MagickResetIterator(magick_wand);
        while (MagickNextImage(magick_wand) != MagickFalse) {
            MagickResizeImage(magick_wand,width,height,LanczosFilter,1.0);
        }
        status=MagickSetImageFormat(magick_wand, "jpg");
        if (status == MagickFalse) {
            ThrowWandException(magick_wand);
        }
        unsigned char *new_blob;
        new_blob = MagickGetImageBlob(magick_wand,new_length);

        magick_wand=DestroyMagickWand(magick_wand);
        MagickWandTerminus();
        return new_blob;
    }
};

