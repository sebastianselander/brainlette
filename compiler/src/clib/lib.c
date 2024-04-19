#include <stdio.h>

char* readString() {
    char* str = NULL;
    size_t str_len = 0;
    size_t bytes = getline(&str, &str_len, stdin);
    return str;
}
