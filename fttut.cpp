#include "stdlib.h"
#include "stdio.h"
#include "math.h"
#include "locale.h"

#include "wchar.h"

#include "png.h"

#include <ft2build.h>
#include <freetype/freetype.h> /* flymake 용으로 기입한 문구. */

#include FT_FREETYPE_H

#define ERROR 0

typedef struct _DrawBuffer_ {
  int width;
  int height;
  unsigned char* data;
} DrawBuffer;

FT_Library ftlib;

extern "C" DrawBuffer* make_new_draw_buffer( int width, int height )
{
  DrawBuffer* db = (DrawBuffer*)malloc(sizeof(DrawBuffer));
  db->width = width;
  db->height = height;
  db->data = (unsigned char*) calloc( 3*width*height, 1 );
  return db;
}

extern "C" int delete_draw_buffer( DrawBuffer* db )
{
  if( db->data ) {
    free(db->data);
    db->data = NULL;
    free(db);
  }
  return 1;
}

extern "C" DrawBuffer* read_png( const char* fn )
{
  png_structp png_ptr;
  png_infop info_ptr;
  int number_of_passes;
  png_bytep* row_pointers;
  png_byte color_type = PNG_COLOR_TYPE_RGBA;
  png_byte bit_depth = 8;
  int x, y, i, j;
  FILE* fp;
  DrawBuffer* drawBuffer;
  drawBuffer = (DrawBuffer*)malloc(sizeof(DrawBuffer));

  png_byte header[8];

  fp = fopen(fn, "r");
  if( !fp ) {
    printf( "file open error... \n" );
    return 0;
  }

  fread(header, 1, 8, fp);
  if(png_sig_cmp(header, 0, 8)) {
    printf( "file is not recognized as a png file\n");
    return (ERROR);
  }

  // initialize stuff
  png_ptr = png_create_read_struct( PNG_LIBPNG_VER_STRING,
				    NULL, NULL, NULL );
  if(!png_ptr) {
    printf( "png_create_read_struct failed\n");
    return (ERROR);
  }

  info_ptr = png_create_info_struct(png_ptr);
  if(!info_ptr) {
    printf( "png_create_info_struct is failed.\n");
    return (ERROR);
  }

  if(setjmp(png_jmpbuf(png_ptr))) {
    printf( "error during init_io\n");
    return (ERROR);
  }
  png_init_io(png_ptr, fp);
  png_set_sig_bytes(png_ptr, 8);

  png_read_info(png_ptr, info_ptr);

  // info_ptr 에 width, height 등의 정보가 있으나, deprecated 되었으며,
  // 이제는 png_get_image_width, png_get_image_height 와 같은 accessor
  // 함수를 사용하여 값을 얻어와야 한다. 
  drawBuffer->width = png_get_image_width(png_ptr, info_ptr);
  drawBuffer->height = png_get_image_height(png_ptr, info_ptr);
  color_type = png_get_color_type(png_ptr, info_ptr);
  bit_depth = png_get_bit_depth(png_ptr, info_ptr);
  number_of_passes = png_set_interlace_handling(png_ptr);
  png_read_update_info(png_ptr, info_ptr);

  // allocate memory for store image
  int rowbytes = png_get_rowbytes(png_ptr, info_ptr);
  drawBuffer->data = (unsigned char*) 
    malloc(sizeof(unsigned char*) * (drawBuffer->height) * (drawBuffer->width) * 3);

  printf( "drawBuffer->width : %d, drawBuffer->height : %d \n", 
	  drawBuffer->width, drawBuffer->height );


  row_pointers = (png_bytep *) malloc(sizeof(unsigned char *) * (drawBuffer->height));
  for( y = 0; y < drawBuffer->height; y++ )
    row_pointers[y] = (png_byte*) 
      ((drawBuffer->data) + y * 3 * (drawBuffer->width));

  // read file
  if(setjmp(png_jmpbuf(png_ptr))) {
    printf( "error during read_image\n");
    return (ERROR);
  }

  png_read_image(png_ptr, row_pointers);
  fclose(fp);

  return drawBuffer;
  
}



// int write_png( FILE* fp, int width, int height, png_byte* data )
extern "C" int write_png( const char* fn, DrawBuffer* db ) 
{
  png_structp png_ptr;
  png_infop info_ptr;
  int number_of_passes;
  png_bytep* row_pointers;
  png_byte color_type = PNG_COLOR_TYPE_RGB;
  png_byte bit_depth = 8;
  int x, y, i, j;
  FILE *fp;

  fp = fopen( fn, "w" );
  if( !fp ) {
    printf( "File Open Error.. \n" );
    return 0;
  }


  // make image data (임시로)
  row_pointers = (png_bytep *) malloc(sizeof(png_bytep *) * db->height);
  for( i = 0; i < db->height; i++ ) {
    *(row_pointers+i) = (png_bytep)(db->data+(i * 3 * db->width));
  }

  png_ptr = png_create_write_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
  if( !png_ptr ) return (ERROR);

  info_ptr = png_create_info_struct(png_ptr);
  if( !info_ptr ) return (ERROR);

  if( setjmp( png_jmpbuf( png_ptr ) ) ) {
    printf( "error during setjmp point\n" );
    return (ERROR);
  }
  png_init_io(png_ptr, fp);

  /* write header */
  if(setjmp(png_jmpbuf(png_ptr))) {
    printf( "error during png_jmpbuf point for write header\n" );
    return (ERROR);
  }

  png_set_IHDR( png_ptr, 
		info_ptr, 
		db->width, db->height,
		bit_depth,
		color_type,
		PNG_INTERLACE_NONE,
		PNG_COMPRESSION_TYPE_BASE,
		PNG_FILTER_TYPE_BASE );
  png_write_info( png_ptr, info_ptr );

  // write bytes
  if( setjmp(png_jmpbuf(png_ptr)) ) {
    printf( "write_png_file : Error during writing bytes\n" );
    return (ERROR);
  }
  png_write_image( png_ptr, row_pointers );

  // end write
  if( setjmp(png_jmpbuf(png_ptr)) ) {
    printf( "Error during end of write\n" );
    return (ERROR);
  }
  png_write_end(png_ptr, NULL);

  // cleanup heap allocation
  // for( y = 0; y < height; y++ ) {
  //   free( row_pointers[y] );
  // }

  free( row_pointers );
  fclose( fp );

  return 1;
}

extern "C" int draw_bitmap( int x, int y, 
		 DrawBuffer* buffer, 
		 int source_width, 
		 int source_height,
		 unsigned char* glyph_data )
{
  int i, j, t_i, t_j;
  unsigned char *src = glyph_data;

  t_i = y + source_height;
  t_j = x + source_width;

  for( i = y; i < t_i; i++ ) {
    for( j = x; j < t_j; j++ ) {
      if( (i <= buffer->width) && (i >= 0) &&
	  (j <= buffer->height) && (j >= 0) ) {
	*(buffer->data + (i * buffer->width * 3) + (j * 3) + 0) = *src;
	*(buffer->data + (i * buffer->width * 3) + (j * 3) + 1) = *src;
	*(buffer->data + (i * buffer->width * 3) + (j * 3) + 2) = *src;
      }
      src++;
    }
  }
  return 1;
}

extern "C" int draw_string_line( FT_Face face, 
				 DrawBuffer* db, 
				 int x, 
				 int y, 
				 const wchar_t* str )
{
  int error;
  int i, j, k;
  int len = wcslen(str);
  FT_GlyphSlot slot = face->glyph;

  printf( "BX   BY   Adv  W    H\n");
  printf( "---- ---- ---- ---- ----\n");
  for( i = 0; i < len; i++ ) {
    error = FT_Load_Glyph( face, 
			   FT_Get_Char_Index( face, *(str+i) ),
			   FT_LOAD_RENDER );

    printf( "%4d %4d %4d %4d %4d\n", 
	    (int) slot->metrics.horiBearingX / 64,
	    (int) slot->metrics.horiBearingY / 64,
	    (int) slot->metrics.horiAdvance / 64,
	    (int) slot->metrics.width / 64,
	    (int) slot->metrics.height / 64 );

    draw_bitmap( x + slot->metrics.horiBearingX/64, 
		 y - slot->metrics.horiBearingY/64,
		 db, 
		 slot->bitmap.width,
		 slot->bitmap.rows,
		 slot->bitmap.buffer );
    x += ((slot->metrics.horiAdvance - slot->metrics.horiBearingX) / 64);
  }
}



int main(int argc, char* argv[])
{
  int error;
  FT_Face face;
  FILE *fp;
  const wchar_t* str1 = L"위대한 younwook 연욱대왕 만만세~";

  DrawBuffer *drawBuffer;

  int x, y, width, height;
  width = 300; 
  height = 300;

  setlocale(LC_ALL, "ko_KR.UTF-8");

  printf("test\n\n");
  wprintf( L"유니코드 입력 가능 여부 확인\n" );

  error = FT_Init_FreeType( &ftlib );
  if( error ) {
    printf( "FT_Init_FreeType error occurred!\n" );
  }

  /* Load a font face */
  error = FT_New_Face( ftlib, 
		       // "/usr/share/fonts/truetype/unfonts/UnTaza.ttf",
		       "/System/Library/Fonts/AppleGothic.ttf",
		       0,
		       &face );
  if( error == FT_Err_Unknown_File_Format ) {
    printf( "font format is not supported!! \n" );
  } else if( error ) {
    printf( "Font file load failure!!! \n");
  }

  /* 픽셀 크기를 세팅하는 부분입니다. */
  error = FT_Set_Char_Size(
			   face,   /* handle to face object */
			   0,      /* char_width in 1/64th of points */
			   8*64,  /* char_height in 1/64th of points */
			   width,    /* horizontal device resolution */
			   height );  /* vertical device resolution */
  /*
    캐릭터의 높이와 너비는 1/64 포인트 단위로 정해집니다. 1 포인트는
    장치상에서 1/72 인치를 의미합니다. 만약 너비에 0을 세팅한다면 이는
    "캐릭터 높이와 같음" 을 의미하는 것입니다. 
  */
  if( error ) {
    printf( "Face size setting error occurred! \n" );
  }

  
  /*
    libpng 부분을 먼저 구현해 봅니다...
  */

  png_byte *data;

  // fp = fopen("in.png", "r");
  // if( !fp ) {
  //   printf( "file open error... \n" );
  //   return 0;
  // }
  drawBuffer = read_png("in.png");

  printf("tttt\n");

  // drawBuffer 변수를 생성한다. 
  // (사실 이 부분은 팩토리든 뭐든 사용해서 추상화시켜야 한다.)
  // drawBuffer.width = width;
  // drawBuffer.height = height;
  // drawBuffer.data = data;
  //
  //

  // 다시 freetype 부분으로 돌아옵니다. 
  FT_GlyphSlot slot = face->glyph;
  int pen_x, pen_y, n;
  char str[] = "test";
  unsigned char* test_buffer;
  FT_Bitmap_Size bitmap_size;
  int i, j, k;
  unsigned char p;

  pen_x = 300;
  pen_y = 200;
  error = FT_Select_Charmap( face, FT_ENCODING_UNICODE );

  wprintf( L"the char is %c, %d, %d \n", *str1,  *str1, wcslen(str1) );
  error = FT_Load_Glyph( face, 
			 FT_Get_Char_Index( face, *str1 ), // 0x00a9 ), 
			 FT_LOAD_RENDER );

  error = FT_Render_Glyph( face->glyph, FT_RENDER_MODE_NORMAL );
  if (error) {
    printf( "FT_Load_Glyph, FT_Render_Glyph error!!\n" );
    return 0;
  }
  
  printf("The bitmap size is %d, %d\n", slot->bitmap.width, slot->bitmap.rows);
  test_buffer = slot->bitmap.buffer;

  draw_string_line( face, drawBuffer, 50, 50, str1 ); 

  write_png("tt.png", drawBuffer);

  return 1;

}
