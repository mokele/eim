#include <stdio.h>
#include <stdlib.h>
#include <cmath>
#include <iostream>
#include <wand/MagickWand.h>
#include "erl_nif_compat.h"

#define EIM_FLOAT_LEFT   1
#define EIM_FLOAT_TOP    2
#define EIM_FLOAT_CENTER 4
#define EIM_FLOAT_BOTTOM 8
#define EIM_FLOAT_RIGHT  16

typedef enum
{
    EIM_FORMAT_JPG,
    EIM_FORMAT_GIF,
    EIM_FORMAT_PNG
} EIM_FORMAT;

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
        size_t height = (size_t)round(height_ * ((double)width/width_));
        
        MagickResetIterator(magick_wand);
        while (MagickNextImage(magick_wand) != MagickFalse) {
            MagickResizeImage(magick_wand,width,height,LanczosFilter,1.0);
        }
        width_ = width;
        height_ = height;
    }
    void scale_height(size_t height)
    {
        size_t width = (size_t)round(width_ * ((double)height/height_));
        
        MagickResetIterator(magick_wand);
        while (MagickNextImage(magick_wand) != MagickFalse) {
            MagickResizeImage(magick_wand,width,height,LanczosFilter,1.0);
        }
        width_ = width;
        height_ = height;
    }
    void max_height(size_t height)
    {
        if(height_ > height)
        {
            crop(width_, height, 0, 0);
        }
    }
    void max_width(size_t width)
    {
        if(width_ > width)
        {
            crop(width, height_, 0, 0);
        }
    }
    
    void crop(size_t width, size_t height, size_t x, size_t y)
    {
        MagickResetIterator(magick_wand);
        while (MagickNextImage(magick_wand) != MagickFalse) {
            if(MagickFalse == MagickCropImage(magick_wand,width,height,x,y))
            {
                throw("An error occured");
            }
        }
        width_ = width;
        height_ = height;
    }
    
    void fit(size_t width, size_t height)
    {
        double width_ratio = (double)width/width_;
        double height_ratio = (double)height/height_;
        size_t new_width, new_height;
        if(width_ratio < height_ratio)
        {
            new_width = width;
            new_height = (size_t)(height_ * width_ratio);
        }
        else
        {
            new_width = (size_t)(width_ * height_ratio);
            new_height = height;
        }
        width_ = new_width;
        height_ = new_height;
        
        MagickResetIterator(magick_wand);
        while (MagickNextImage(magick_wand) != MagickFalse) {
            MagickResizeImage(magick_wand,new_width,new_height,LanczosFilter,1.0);
        }
    }
    void box(size_t width, size_t height, char floated)
    {
        double width_ratio = (double)width/width_;
        double height_ratio = (double)height/height_;
        size_t new_width, new_height, crop_x, crop_y;
        if(width_ratio > height_ratio)
        {
            new_width = width;
            new_height = (size_t)(height_ * width_ratio);
            crop_x = 0;
            crop_y = 0;
            
            if((floated & EIM_FLOAT_BOTTOM) == EIM_FLOAT_BOTTOM)
            {
                crop_y = new_height - height;
            }
            else if((floated & EIM_FLOAT_TOP) == EIM_FLOAT_TOP)
            {
                crop_x = 0;
            }
            else//if((floated & EIM_FLOAT_CENTER) == EIM_FLOAT_CENTER)
            {
                crop_y = (size_t)(new_height / 2.0 - height / 2.0);
            }
        }
        else
        {
            new_width = (size_t)(width_ * height_ratio);
            new_height = height;
            crop_y = 0;
            if((floated & EIM_FLOAT_RIGHT) == EIM_FLOAT_RIGHT)
            {
                crop_x = new_width - width;
            }
            else if((floated & EIM_FLOAT_LEFT) == EIM_FLOAT_LEFT)
            {
                crop_x = 0;
            }
            else//if((floated & EIM_FLOAT_CENTER) == EIM_FLOAT_CENTER)
            {
                crop_x = (size_t)(new_width / 2.0 - width / 2.0);
            } 
        }
        MagickResetIterator(magick_wand);
        while (MagickNextImage(magick_wand) != MagickFalse) {
            MagickResizeImage(magick_wand,new_width,new_height,LanczosFilter,1.0);
            MagickCropImage(magick_wand,width,height,crop_x,crop_y);
        }
        
        width_ = width;
        height_ = height;
    }
    
    unsigned char* process(EIM_FORMAT fmt, size_t *new_length)
    {
        //long unsigned int number_properties;
        //char **properties = MagickGetImageProperties(magick_wand, "*", &number_properties);
        ////void *MagickRelinquishMemory(void *resource)
        //std::cout << "Profiles: " << std::endl;
        //if (properties != (char **) NULL)
        //{
        //    for (ssize_t i=0; i < (ssize_t)number_properties; i++)
        //    {
        //        char *property = MagickGetImageProperty(magick_wand,properties[i]);
        //        std::cout << properties[i] << std::endl;
        //        std::cout << property << std::endl;
        //        properties[i]=(char *) MagickRelinquishMemory(properties[i]);
        //    }
        //    properties=(char **) MagickRelinquishMemory(properties);
        //}
        
        //orientation is stored in the data we're about to strip, so correct the image first
        reorientate();
        MagickStripImage(magick_wand);
        
        switch(fmt)
        {
            case EIM_FORMAT_JPG:
                status=MagickSetImageFormat(magick_wand, "jpg");
                break;
            case EIM_FORMAT_GIF:
                status=MagickSetImageFormat(magick_wand, "gif");
                break;
            case EIM_FORMAT_PNG:
            default:
                status=MagickSetImageFormat(magick_wand, "png");
                break;
        }
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
        MagickClearException(magick_wand);
        magick_wand=DestroyMagickWand(magick_wand);
        MagickWandTerminus();
    }
    
protected:
    void reorientate()
    {
        PixelWand *background;
        switch(MagickGetImageOrientation(magick_wand))
        {
            //todo: separate out rotate logic
            case TopRightOrientation:
                MagickFlopImage(magick_wand);
            break;
            case BottomLeftOrientation:
                MagickFlopImage(magick_wand);
                //continue to 6
            case BottomRightOrientation:
                background = NewPixelWand();
                MagickRotateImage(magick_wand, background, 180);
                DestroyPixelWand(background);
            break;
            case RightBottomOrientation:
                MagickFlopImage(magick_wand);
                //continue to 6
            case RightTopOrientation:
                background = NewPixelWand();
                MagickRotateImage(magick_wand, background, 90);
                DestroyPixelWand(background);
            break;
            case LeftTopOrientation:
                MagickFlopImage(magick_wand);
            case LeftBottomOrientation:
                background = NewPixelWand();
                MagickRotateImage(magick_wand, background, 270);
                DestroyPixelWand(background);
            break;
            default: break;
        }
    }
    
    const void *data_;
    const size_t size_;
    MagickWand *magick_wand;
    MagickBooleanType status;
    long unsigned int width_,height_;
};

