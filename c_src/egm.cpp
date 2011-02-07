#include <stdio.h>
#include <string.h>
#include <time.h>
#include <sys/types.h>
#include <magick/api.h>

class egm
{
public:
    static void resize()
    {
        ExceptionInfo
        exception;

        Image
          *image,
          *images,
          *resize_image,
          *thumbnails;

        ImageInfo
          *image_info;

        /*
          Initialize the image info structure and read the list of files
          provided by the user as a image sequence
        */
        GetExceptionInfo(&exception);
        image_info=CloneImageInfo((ImageInfo *) NULL);
        images=NewImageList();
        //for (i=1; i< argc-1; i++)
        //  {
            (void) strcpy(image_info->filename,"../priv/flyer-3.gif");
            //printf("Reading %s ...", image_info->filename);
            image=ReadImage(image_info,&exception);
            //printf(" %lu frames\n", GetImageListLength(image));
            if (exception.severity != UndefinedException)
              CatchException(&exception);
            if (image)
              (void) AppendImageToList(&images,image);
        //  }

        //if (!images)
        //  {
        //    printf("Failed to read any images!\n");
        //    exit(1);
        //  }
        /*
          Create a thumbnail image sequence
        */
        thumbnails=NewImageList();
        while ((image=RemoveFirstImageFromList(&images)) != (Image *) NULL)
          {
            resize_image=ResizeImage(image,106,80,LanczosFilter,1.0,&exception);
            DestroyImage(image);
            if (resize_image == (Image *) NULL)
              {
                CatchException(&exception);
                continue;
              }
            (void) AppendImageToList(&thumbnails,resize_image);
          }
        /*
          Write the thumbnail image sequence to file
        */
        if (thumbnails)
          {
            (void) strcpy(thumbnails->filename,"../priv/out.png");
            //printf("Writing %s ... %lu frames\n", thumbnails->filename,
            //       GetImageListLength(thumbnails));
            WriteImage(image_info,thumbnails);
          }

        /*
          Release resources
        */
        DestroyImageList(thumbnails);
        DestroyImageInfo(image_info);
        DestroyExceptionInfo(&exception);
    }
};

