// Based on example program from
// http://stackoverflow.com/q/1821806/101258
#include "write-png.h"

/* Attempts to save PNG to file; returns 0 on success, non-zero on error. */
int bitmap_save_to_png(RGBBitmap *bitmap, const char *path)
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
      uint8_t *row = png_malloc(png_ptr, sizeof(uint8_t) * bytes_per_row);
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

int bitmap_init(RGBBitmap *img, int width, int height)
{
  img->width = width;
  img->height = height;
  img->bytes_per_pixel = 3;
  img->pixels = calloc(1, sizeof(RGBPixel) * img->width * img->height);
  return 0;
}

int bitmap_set(RGBBitmap *img, int x, int y, int r, int g, int b)
{
  RGBPixel *pixel = RGBBufferAtPoint(img, x, y);
  pixel->red = r;
  pixel->green = g;
  pixel->blue = b;
  return 0;
}

//int main()
//{
//  const char path[] = "test.png";
//  int status = 0, x, y;
//  RGBBitmap img;
//
//  bitmap_init(&img, 100, 100);
//  for (y = 0; y < img.height; y++) {
//    for (x = 0; x < img.height; x++) {
//      bitmap_set(&img, x, y, 255, 255, 255);
//    }
//  }
//  bitmap_set(&img, 50, 50, 0, 0, 255);
//  bitmap_set(&img, 0,  0, 0, 0, 255);
//  bitmap_set(&img, 99, 0, 0, 0, 255);
//  bitmap_set(&img, 0,  99, 0, 0, 255);
//  bitmap_set(&img, 99, 99, 0, 0, 255);
//
//  status = bitmap_save_to_png(&img, path);
//  if (!status){
//    printf("Successfully saved %s\n", path);
//  } else {
//    printf("Unable to save %s\n", path);
//  }
//  return status;
//}
