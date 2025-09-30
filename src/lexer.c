#include "lexer.h"
#include <stdio.h>
#include <stdlib.h>

void addToken(Lexer* lexer, TokenData tok) {
	if (lexer->used == lexer->size) {
		lexer->size *=2;
		lexer->tokens = realloc(lexer->tokens,lexer->size * sizeof(TokenData));
	}
}

//void freeLexer(Lexer* lexer) {}

int main() {
	printf("Hello, World!\n");
}
