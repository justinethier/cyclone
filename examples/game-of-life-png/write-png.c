// Based on example program from
// http://stackoverflow.com/q/1821806/101258
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
    size_t bytewidth;
    uint8_t bytes_per_pixel;
} RGBBitmap;

/* Returns pixel of bitmap at given point. */
//#define RGBPixelAtPoint(image, x, y) \
//    *(((image)->pixels) + (((image)->bytewidth * (y)) \
//                        + ((x) * (image)->bytes_per_pixel)))
//
//#define RGBBufferAtPoint(image, x, y) \
//    (((image)->pixels) + (((image)->bytewidth * (y)) \
//                        + ((x) * (image)->bytes_per_pixel)))
#define RGBPixelAtPoint(image, x, y) \
    *(((image)->pixels) + (((image)->width * (y)) \
                        + ((x))))

#define RGBBufferAtPoint(image, x, y) \
    (((image)->pixels) + (((image)->width * (y)) \
                        + ((x))))

/* Attempts to save PNG to file; returns 0 on success, non-zero on error. */
int save_png_to_file(RGBBitmap *bitmap, const char *path)
{
    FILE *fp = fopen(path, "wb");
    png_structp png_ptr = NULL;
    png_infop info_ptr = NULL;
    size_t x, y;
    png_uint_32 bytes_per_row;
    png_byte **row_pointers = NULL;

    if (fp == NULL) return -1;

    /* Initialize the write struct. */
    png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
    if (png_ptr == NULL) {
      fclose(fp);
      return -1;
    }

    /* Initialize the info struct. */
    info_ptr = png_create_info_struct(png_ptr);
    if (info_ptr == NULL) {
      png_destroy_write_struct(&png_ptr, NULL);
      fclose(fp);
      return -1;
    }

    /* Set up error handling. */
    if (setjmp(png_jmpbuf(png_ptr))) {
      png_destroy_write_struct(&png_ptr, &info_ptr);
      fclose(fp);
      return -1;
    }

    /* Set image attributes. */
    png_set_IHDR(png_ptr,
                 info_ptr,
                 bitmap->width,
                 bitmap->height,
                 8,
                 PNG_COLOR_TYPE_RGB,
                 PNG_INTERLACE_NONE,
                 PNG_COMPRESSION_TYPE_DEFAULT,
                 PNG_FILTER_TYPE_DEFAULT);

    /* Initialize rows of PNG. */
    bytes_per_row = bitmap->width * bitmap->bytes_per_pixel;
    row_pointers = png_malloc(png_ptr, bitmap->height * sizeof(png_byte *));
    for (y = 0; y < bitmap->height; ++y) {
      uint8_t *row = png_malloc(png_ptr, sizeof(uint8_t) * bytes_per_row); //bitmap->bytes_per_pixel * bitmap->width);
      row_pointers[y] = (png_byte *)row;
      for (x = 0; x < bitmap->width; ++x) {
        RGBPixel color = RGBPixelAtPoint(bitmap, x, y);
        *row++ = color.red;
        *row++ = color.green;
        *row++ = color.blue;
      }
    }

    /* Actually write the image data. */
    png_init_io(png_ptr, fp);
    png_set_rows(png_ptr, info_ptr, row_pointers);
    png_write_png(png_ptr, info_ptr, PNG_TRANSFORM_IDENTITY, NULL);

    /* Cleanup. */
    for (y = 0; y < bitmap->height; y++) {
      png_free(png_ptr, row_pointers[y]);
    }
    png_free(png_ptr, row_pointers);

    /* Finish writing. */
    png_destroy_write_struct(&png_ptr, &info_ptr);
    fclose(fp);
    return 0;
}

int main()
{
  const char path[] = "test.png";
  int status = 0, x, y;
  RGBBitmap img;
  RGBPixel *pixel;

  img.width = 100;
  img.height = 100;
  img.bytewidth = img.width; // ????
  img.bytes_per_pixel = 3;
  img.pixels = calloc(1, sizeof(RGBPixel) * img.width * img.height);

  for (y = 0; y < img.height; y++) {
    for (x = 0; x < img.height; x++) {
      pixel = RGBBufferAtPoint(&img, x, y);
      pixel->red = 255;
      pixel->green = 255;
      pixel->blue = 255;
    }
  }
  pixel = RGBBufferAtPoint(&img, 50, 50);
  pixel->red = 0;
  pixel->green = 0;
  pixel->blue = 255;
  pixel = RGBBufferAtPoint(&img, 0, 0);
  pixel->red = 0;
  pixel->green = 0;
  pixel->blue = 255;
  pixel = RGBBufferAtPoint(&img, 99, 0);
  pixel->red = 0;
  pixel->green = 0;
  pixel->blue = 255;
  pixel = RGBBufferAtPoint(&img, 0, 99);
  pixel->red = 0;
  pixel->green = 0;
  pixel->blue = 255;
  pixel = RGBBufferAtPoint(&img, 99, 99);
  pixel->red = 0;
  pixel->green = 0;
  pixel->blue = 255;

  status = save_png_to_file(&img, path);
  if (!status){
    printf("Successfully saved %s\n", path);
  } else {
    printf("Unable to save %s\n", path);
  }
  return status;
}
