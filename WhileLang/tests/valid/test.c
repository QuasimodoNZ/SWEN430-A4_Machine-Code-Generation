#include<stdio.h>
#include "runtime.c"

int main() {
	slot_t x = 20;
	slot_t t = 1;
	internal_print(&x, &t);
}
