#include <stdio.h>
#include <stdlib.h>

typedef unsigned char byte;

int read_cmd(byte *buff);
int write_cmd(byte *buff, int len);

int main(){
  int result, i, len;
  byte buff[255];

  while (read_cmd(buff) > 0) {
    result = barcode_to_png(buff);
    
    buff[0] = result;
    write_cmd(buff, 1);
  }
}
