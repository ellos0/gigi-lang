#ifndef GIGI_COMPILER_H_
#define GIGI_COMPILER_H_
#include <stddef.h>

typedef enum token {
	eof,
	lparen,
	rparen,
	period,
	comma,
	plus,
	minus,
	star,
	slash,
	equal,
	greaterthan,
	lessthan,
	and,
	or,
	not,
	func,
	ret,
	string

} Token;

typedef struct token_data {
	Token type;
	char *val;
} TokenData;

typedef struct lexer {
	char *curTok;
	size_t used;
	size_t size;
	TokenData *tokens;
} Lexer;

int lex(Lexer *lexer);
void addToken(Lexer* lexer, TokenData tok);
void freeLexer(Lexer* lexer);
char *tokenToString(Token token);

#endif
