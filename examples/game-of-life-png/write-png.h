#ifndef WRITE_PNG_H
#define WRITE_PNG_H

#include <png.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

/* Pixels in this bitmap structure are stored as BGR. */
typedef struct _RGBPixel {
    uint8_t blue;
    uint8_t green;
    uint8_t red;
} RGBPixel;

/* Structure for containing decompressed bitmaps. */
typedef struct _RGBBitmap {
    RGBPixel *pixels;
    size_t width;
    size_t height;
    uint8_t bytes_per_pixel;
} RGBBitmap;

/* Returns pixel of bitmap at given point. */
#define RGBPixelAtPoint(image, x, y) \
    *(((image)->pixels) + (((image)->width * (y)) \
                        + ((x))))

#define RGBBufferAtPoint(image, x, y) \
    (((image)->pixels) + (((image)->width * (y)) \
                        + ((x))))
int bitmap_init(RGBBitmap *img, int width, int height);
int bitmap_set(RGBBitmap *img, int x, int y, int r, int g, int b);
int bitmap_save_to_png(RGBBitmap *bitmap, const char *path);

#endif /* WRITE_PNG_H */
