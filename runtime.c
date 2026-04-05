#include <stdio.h>
#include <stdint.h>

typedef struct {
    size_t len;
    char data[]; 
} WillowString;

void print(WillowString* str) {
    if (str == NULL) {
        printf("null\n");
        return;
    }
    
    fwrite(str->data, 1, str->len, stdout);
}
