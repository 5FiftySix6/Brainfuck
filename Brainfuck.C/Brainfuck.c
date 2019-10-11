#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int main(int argc, char* argv[])
{
	if (argc > 2)
	{
		fprintf(stderr, "Too many arguments supplied. Usage: [%s] FILE\n", argv[0]);
		return -1;
	}
	else if (argc == 1)
	{
		fprintf(stderr, "Not enough arguments supplied. Usage: [%s] FILE\n", argv[0]);
		return -1;
	}

	FILE *f = fopen(argv[1], "rb+");

	if (!f)
	{
		fprintf(stderr, "File [%s] does not exist!\n", argv[1]);
	}

	char* mem = malloc(sizeof(char) * 30000);
	memset(mem, 0, sizeof(char) * 30000);
	
	int c;
	while ( (c = fgetc(f)) != EOF)
	{
		int brackets = 1;

		switch(c)
		{
			case '>':
				mem++;
				break;
			case '<':
				mem--;
				break;
			case '+':
				(*mem)++;
				break;
			case '-':
				(*mem)--;
				break;
			case ',':
				*mem = getchar();
				break;
			case '.':
				printf("%c", *mem);
				break;
			case '[':
				if (*mem) continue;

				while (brackets)
				{
					c = fgetc(f);

					if (c == EOF)
					{
						fprintf(stderr, "Expected character, got EOF\n");
						return -1;
					}


					if (c == '[')
					{
						brackets++;
					}
					else if (c == ']')
					{
						brackets--;
					}
				}
				break;
			case ']':
				if (!*mem) continue;

				int brackets = 1;

				while (brackets)
				{
					fseek(f, -2, SEEK_CUR);
					c = fgetc(f);

					if (c == '[')
						brackets--;
					else if (c == ']')
						brackets++;

				}

				break;
		}
	}

	fclose(f);

	return 0;
}
