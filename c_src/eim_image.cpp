/*
 * Copyright (C) 2011 by Steven Gravell
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 */

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
typedef enum
{
    EIM_ROTATE_90,
    EIM_ROTATE_180,
    EIM_ROTATE_270
} EIM_ROTATE;

#define ThrowWandException(wand) \
{ \
  magick_wand=DestroyMagickWand(wand); \
  MagickWandTerminus(); \
  throw("An error occured"); \
}
    
class eim_image
{
public:
    eim_image(const void *data, const size_t size)
        : data_(data),
          size_(size),
          background_(NULL)
    {
        MagickWandGenesis();
        MagickWand *magick_wand2=NewMagickWand();
        status=MagickReadImageBlob(magick_wand2, data_, size_);
        if (status == MagickFalse) {
            ThrowWandException(magick_wand2)
        }
        status=MagickGetImagePage(magick_wand2,&width_,&height_,&x_,&y_);
        if (status == MagickFalse) {
            ThrowWandException(magick_wand2)
        }
        //magick_wand=magick_wand2;
        magick_wand=MagickCoalesceImages(magick_wand2);
        magick_wand2=DestroyMagickWand(magick_wand2);
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
                ThrowWandException(magick_wand)
            }
            MagickSetImagePage(magick_wand,width,height,x,y);
        }
        width_ = width;
        height_ = height;
        x_ = x;
        y_ = y;
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
            MagickSetImagePage(magick_wand,width,height,crop_x*-2,crop_y*-2);
        }
        
        x_ = crop_x*-2;
        y_ = crop_y*-2;
        width_ = width;
        height_ = height;
    }
    
    void rotate(EIM_ROTATE rotate)
    {
        init_background();
        switch(rotate)
        {
            case EIM_ROTATE_90:
                MagickRotateImage(magick_wand, background_, 90);
            break;
            case EIM_ROTATE_180:
                MagickRotateImage(magick_wand, background_, 180);
            break;
            case EIM_ROTATE_270:
                MagickRotateImage(magick_wand, background_, 270);
            break;
        }
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
        MagickSetPage(magick_wand,width_,height_,x_,y_);
        
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
            ThrowWandException(magick_wand)
        }
        unsigned char *new_blob;
        MagickResetIterator(magick_wand);
        new_blob = MagickGetImagesBlob(magick_wand,new_length);

        magick_wand=DestroyMagickWand(magick_wand);
        MagickWandTerminus();
        return new_blob;
    }
    virtual ~eim_image()
    {
        if(background_ != NULL)
        {
            DestroyPixelWand(background_);
        }
        MagickClearException(magick_wand);
        magick_wand=DestroyMagickWand(magick_wand);
        MagickWandTerminus();
    }
    
protected:
    void init_background()
    {
        if(background_ == NULL)
        {
            background_ = NewPixelWand();
        }
    }
    void reorientate()
    {
        switch(MagickGetImageOrientation(magick_wand))
        {
            //todo: separate out rotate logic
            case TopRightOrientation:
                MagickFlopImage(magick_wand);
            break;
            case BottomLeftOrientation:
                MagickFlopImage(magick_wand);
                //continue to BottomRightOrientation
            case BottomRightOrientation:
                rotate(EIM_ROTATE_180);
            break;
            case RightBottomOrientation:
                MagickFlopImage(magick_wand);
                //continue to RightTopOrientation
            case RightTopOrientation:
                rotate(EIM_ROTATE_90);
            break;
            case LeftTopOrientation:
                MagickFlopImage(magick_wand);
            case LeftBottomOrientation:
                rotate(EIM_ROTATE_270);
            break;
            default: break;
        }
    }
    
    const void *data_;
    const size_t size_;
    MagickWand *magick_wand;
    PixelWand *background_;
    MagickBooleanType status;
    long unsigned int width_,height_;
    long int x_, y_;
};

