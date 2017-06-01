
#include <stdio.h>
#include <stdlib.h>

#pragma warning("-Wno-old-function-decl")

/* NOTE: Optimally, this function should never be called.
 *       Therefor we mark is as 'unused', as it should be optimized away! */
__attribute__((unused,noreturn))
static void ass_fail2(f,l,t1,tt1,t2,tt2)
	char const *f,*t1,*tt1,*t2,*tt2;
	int l;
{
	printf("%s(%d) : Assertion failed ('%s':'%s' != '%s':'%s')\n",
	       f,l,t1,tt1,t2,tt2);
	exit(1);
}

/* AssertSameType */
#define AST(T1,T2) \
	(__builtin_types_compatible_p(__typeof__(T1),__typeof__(T2)) || \
	(ass_fail2(__FILE__,__LINE__,\
	           #T1,__builtin_typestr(__typeof__(T1)),\
	           #T2,__builtin_typestr(__typeof__(T2))),0))
#define NAST(T1,T2) \
	(!__builtin_types_compatible_p(__typeof__(T1),__typeof__(T2)) || \
	(ass_fail2(__FILE__,__LINE__,\
	           #T1,__builtin_typestr(__typeof__(T1)),\
	           #T2,__builtin_typestr(__typeof__(T2))),0))

int main(int argc, char *argv[]) {

	char               c   [[unused]];
	int                i   [[unused]];
	long               l   [[unused]];
	long long          ll  [[unused]];
	unsigned int       u   [[unused]];
	unsigned long      ul  [[unused]];
	unsigned long long ull [[unused]];

	/* Generic same-type assertions. */
#ifdef __CHAR_UNSIGNED__
	AST(char,unsigned char);
#else
	AST(char,signed char);
#endif

	AST(short,short int);
	AST(short,signed short);
	AST(short,signed short int);
	AST(unsigned short,unsigned short int);

	AST(signed,int);
	AST(signed,signed int);
	AST(unsigned,unsigned int);

	AST(long,long int);
	AST(long,signed long);
	AST(long,signed long int);
	AST(unsigned long,unsigned long int);

	AST(long long,long long int);
	AST(long long,signed long long);
	AST(long long,signed long long int);
	AST(unsigned long long,unsigned long long int);

	NAST(int,short);
	AST (int,signed);
	NAST(int,unsigned);
	NAST(int,long);
	NAST(int,long long);
	NAST(unsigned int,short);
	NAST(unsigned int,signed);
	AST (unsigned int,unsigned);
	NAST(unsigned int,long);
	NAST(unsigned int,long long);

	/* Integer promotions. */
	AST(char,        c);
	AST(int,         +c);
#ifdef __CHAR_UNSIGNED__
	AST(int,         -(signed char)c);
#else
	AST(int,         -c);
#endif
	AST(int,         ~c);
	AST(int,         i);

	AST(int,i+i);
	AST(int,i+c);
	AST(int,c+i);
	AST(int,c+c);
	AST(long,l+l);
	AST(long,l+i);
	AST(long,l+c);
	AST(long,i+l);
	AST(long,c+l);
	AST(long long,ll+c);
	AST(long long,ll+i);
	AST(long long,ll+l);
	AST(long long,c+ll);
	AST(long long,i+ll);
	AST(long long,l+ll);
	AST(unsigned int,u+c);
	AST(unsigned int,u+i);
	AST(unsigned int,c+u);
	AST(unsigned int,i+u);
	AST(unsigned long,ul+c);
	AST(unsigned long,ul+i);
	AST(unsigned long,ul+l);
	AST(unsigned long,c+ul);
	AST(unsigned long,i+ul);
	AST(unsigned long,l+ul);
	AST(unsigned long long,ull+c);
	AST(unsigned long long,ull+i);
	AST(unsigned long long,ull+l);
	AST(unsigned long long,ull+ll);
	AST(unsigned long long,c+ull);
	AST(unsigned long long,i+ull);
	AST(unsigned long long,l+ull);
	AST(unsigned long long,ll+ull);

	return 0;
}


