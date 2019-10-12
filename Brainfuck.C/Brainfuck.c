#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int run(char* buffer, char** mem_ptr);


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
		return -1;
	}

	char* mem = malloc(sizeof(char) * 30000);
	memset(mem, 0, sizeof(char) * 30000);

	char* buffer = NULL;
	size_t len;
	ssize_t bytes_read = getdelim(&buffer, &len, '\0', f);

	fclose(f);

	if (bytes_read == -1)
	{
		fprintf(stderr, "Unable to read bytes.\n");
	}
	
	return run(buffer, &mem);
}

int run(char* buffer, char** mem_ptr)
{
	char* mem = *mem_ptr;

	for (int c = *buffer; c != '\0'; c = *(++buffer))
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
					c = *(++buffer);

					if (c == '\0')
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

				while (brackets)
				{
					c = *(--buffer);

					if (c == '[')
						brackets--;
					else if (c == ']')
						brackets++;

				}

				break;
		}
	}
	
	return 0;

}
