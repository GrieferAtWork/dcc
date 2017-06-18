/* Copyright (c) 2017 Griefer@Work                                            *
 *                                                                            *
 * This software is provided 'as-is', without any express or implied          *
 * warranty. In no event will the authors be held liable for any damages      *
 * arising from the use of this software.                                     *
 *                                                                            *
 * Permission is granted to anyone to use this software for any purpose,      *
 * including commercial applications, and to alter it and redistribute it     *
 * freely, subject to the following restrictions:                             *
 *                                                                            *
 * 1. The origin of this software must not be misrepresented; you must not    *
 *    claim that you wrote the original software. If you use this software    *
 *    in a product, an acknowledgement in the product documentation would be  *
 *    appreciated but is not required.                                        *
 * 2. Altered source versions must be plainly marked as such, and must not be *
 *    misrepresented as being the original software.                          *
 * 3. This notice may not be removed or altered from any source distribution. *
 */
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>


/* Never implemented to cause linker errors when referenced by reachable code.
 * > Calling this function from unreachable code will not create a reference. */
extern int nnn(void);

/* Call a function that _is_ reachable. */
extern int yyy(void);

/* Call a function that may be reachable (Currently the same as 'yyy()'). */
extern int mmm(void);

int yyy(void) { return 0; }
int mmm(void) { return 0; }
_Noreturn int _nrt(int line) {
 printf("%s(%d) : Unreachable\n",__FILE__,line);
 exit(1);
}
#define nrt_()  _nrt(__LINE__)
#define nrt()  (_nrt(__LINE__),nnn())


volatile int zero = 0;
#define B  if (zero)
#define Q  zero

int main(int argc, char *argv[]) {
 int x,y,z;

 /* Check linear reachability. */
 B { yyy(); nrt_(); nnn(); }
 B { yyy(), nrt_(), nnn(); }

 if (1) yyy();
 if (0) nnn();
 if (Q) mmm();

 /* Check conditional reachability using logical operators. */
 B { yyy();      1 || (nnn(),   nrt(),   nnn()); yyy(); }
 B { yyy();      0 || (yyy(),   nrt(),   nnn()); nnn(); }
 B { yyy();      0 ||  yyy() || nrt() || nnn();  nnn(); }
 B { yyy();      Q ||  mmm() || nrt() || nnn();  mmm(); }
 B { yyy(); 0 || Q ||  mmm() || nrt() || nnn();  mmm(); }
 B { yyy(); Q || 0 ||  mmm() || nrt() || nnn();  mmm(); }

 B { yyy();      1 && (yyy(),   nrt(),   nnn()); nnn(); }
 B { yyy();      0 && (nnn(),   nrt(),   nnn()); yyy(); }
 B { yyy();      1 &&  yyy() && nrt() && nnn();  nnn(); }
 B { yyy();      Q &&  mmm() && nrt() && nnn();  mmm(); }
 B { yyy(); 1 && Q &&  mmm() && nrt() && nnn();  mmm(); }
 B { yyy(); Q && 1 &&  mmm() && nrt() && nnn();  mmm(); }

 x = 10,y = 20,z = 1;
 /* Check simple runtime-evaluation */
 ( (x != y)) || nrt();
 (!(x == y)) || nrt();
 ( (x == y)) && nrt();
 (!(x != y)) && nrt();

 /* Check advanced runtime-evaluation */
 ( (z == (x != y))) || nrt();
 (!(z != (x != y))) || nrt();
 ( (z != (x != y))) && nrt();
 (!(z == (x != y))) && nrt();
 ( (z != (x == y))) || nrt();
 (!(z == (x == y))) || nrt();
 ( (z == (x == y))) && nrt();
 (!(z != (x == y))) && nrt();

 /* Check conditional reachability using the ?: operator. */
 B { 1 ? yyy() : nnn(); yyy(); }
 B { 0 ? nnn() : yyy(); yyy(); }
 B { 1 ? nrt() : nnn(); nnn(); }
 B { 0 ? nrt() : yyy(); yyy(); }
 B { 1 ? yyy() : nrt(); yyy(); }
 B { 0 ? nnn() : nrt(); nnn(); }
 B { Q ? mmm() : mmm(); yyy(); }
 B { Q ? nrt() : mmm(); mmm(); }
 B { Q ? mmm() : nrt(); mmm(); }
 B { Q ? nrt() : nrt(); nnn(); }

 return 0;
}


