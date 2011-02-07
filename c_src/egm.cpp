#include <stdio.h>
#include <stdlib.h>
#include <wand/MagickWand.h>

class egm
{
public:
    static void resize()
    {
        #define ThrowWandException(wand) \
        { \
        char \
            *description; \
        ExceptionType \
            severity; \
         \
          description=MagickGetException(wand,&severity); \
          (void) fprintf(stderr,"%s %s %u %s\n",GetMagickModule(),description); \
          description=(char *) MagickRelinquishMemory(description); \
          exit(-1); \
        }

          MagickBooleanType
            status;

          MagickWand
            *magick_wand;

          /*
            Read an image.
          */
          MagickWandGenesis();
          magick_wand=NewMagickWand();
          status=MagickReadImage(magick_wand,"../priv/fibula.jpg");
          if (status == MagickFalse)
            ThrowWandException(magick_wand);
          /*
            Turn the images into a thumbnail sequence.
          */
          MagickResetIterator(magick_wand);
          while (MagickNextImage(magick_wand) != MagickFalse)
            MagickResizeImage(magick_wand,106,80,LanczosFilter,1.0);
          /*
            Write the image then destroy it.
          */
          status=MagickWriteImages(magick_wand,"../priv/out2.png",MagickTrue);
          if (status == MagickFalse)
            ThrowWandException(magick_wand);
          magick_wand=DestroyMagickWand(magick_wand);
          MagickWandTerminus();
    }
};

