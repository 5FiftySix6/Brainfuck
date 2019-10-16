#ifndef __BRAINFUCK_H__
#define __BRAINFUCK_H__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BRAINFUCK_START int main(){\
	char* mem = malloc(sizeof(char) * 30000);\
	memset(mem, 0, sizeof(char) * 30000);\
	mem += 15000;\

#define R ++mem;\

#define L --mem;\

#define A ++*mem;\

#define S --*mem;\

#define P printf("%c",*mem);\

#define G *mem = getchar();\

#define W while(*mem) {\

#define E }\

#define BRAINFUCK_END }

#endif
