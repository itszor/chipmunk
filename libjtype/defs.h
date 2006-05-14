#ifndef JT_DEFS_H
#define JT_DEFS_H 1

/* the compiler should optimise this to a real ROR, if it's right... */
#define ROR(X,R) (((X)>>(R)) | ((X)<<(32-(R))))

#define ROL(X,R) (((X)<<(R)) | ((X)>>(32-(R))))

#ifndef IGNORE
#  define IGNORE(X) ((X)=(X))
#endif

#ifndef TRUE
#  define TRUE 1
#endif

#ifndef FALSE
#  define FALSE 0
#endif

/* these may need changing for different compilers/platforms */
typedef unsigned long long uint6;
typedef signed long long sint6;
typedef unsigned int uint5;
typedef signed int sint5;
typedef unsigned short uint4;
typedef signed short sint4;
typedef unsigned char uint3;
typedef signed char sint3;

#endif
