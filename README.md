# eim - Erlang ImageMagick Interface (for the web) #
Provides nif helpers around MagickWand to resize, crop, and rotate images. 
This is the initial goal of the eim project due to these being the primary 
functions needed for web based services that require image uploading and 
basic manipulation functionality.

# Examples #
A photo hosting service which primarily deals with photographs of people 
may create these derivations of updates images:

    1> {ok, Image} = eim:read(Binary).
    2> %% make derivations here
       ...
    N> eim:close(Image).
  
Create a thumbnail of the top portion of the image and scale it to a 
box of 50x50 pixels - similar to default Facebook newsfeed profile 
pictures

    eim:derive(Image, {box, 50, 50, center, top})
    
Resize the image to width 180 and maintain aspect ratio on the height 
and then make sure the height doesn't exceed 350, otherwise crop the 
bottom out so that it is exactly 350 height. Will result in an image 
with width 180 and a height of =< 350 - similar to Facebook style 
pictures on the left of profile pages

    eim:derive(Image, [{scale, width, 180}, {crop_max, height, 350, top}])
   
User defined cropping the image to a specific region - similar to 
the result of Facebook profile image thumbnail editing

    eim:derive(Image, [{crop, 45, 130, 300, 300}, {scale, height, 50}])

# Derivation Reference #

crop a region

    {crop, X, Y, Width, Height}
    
scale by width and maintain aspect ratio on height

    {scale, width, Width}

scale by height and maintain aspect ratio on width

    {scale, height, Height}
    
crop the image if it exceeds these limits float crop center

    {crop_max, width, Width} -> {crop_max, width, Width, center}
    {crop_max, height, Height} -> {crop_max, height, Height, center}

crop the image if it exceeds these limits and float the crop

    {crop_max, width, Width, FloatX}
    {crop_max, height, Height, FloatY}

crop to 350 if the original Height > 350 and float the crop in the top
which is normally where someone's head is in a photograph

      {crop_max, height, 350, top}
    
fit inside this box by cropping out some of the image so it results in  
am image exactly Width, Height in dimensions

    {box, Width, Height} -> {crop_box, Width, Height, center, center}

same as above but with variable FloatX and FloatY

    {box, Width, Height, FloatX, FloatY}
    
fit inside a box of this size but maintain the aspect ratio
results in either
     NewWidth =:= Width and NewHeight =< Height
  or NewHeight =:= Height and NewWidth =< Width

    {fit, Width, Height} -> {fit, Width, Height, center, center}
    {fit, Width, Height, FloatX, FloatY}

fit inside a 200x200 box and float the image in the center
    
    {box, 200, 200, center, center}
    
?? maybe this ??
define this Derivation as a distinct image result from the original
and make sure it's resource is returned along with any other defined 
derivations. Allow creation of multiple images from one source.

    {image, Derivation}

rotate then box, then make this one image

    {image, [{rotate, 90}, {box, 50, 50, center, top}]}




