#ifndef JT_ENDIANFIX_H
#define JT_ENDIANFIX_H 1

#ifdef BIGENDIAN

#define FIXHALF(N) ((((uint4)(N)&0x00ffU)<<8) | (((uint4)(N)&0xff00U)>>8))

#define FIXWORD(N) ((((uint5)(N)&0xff000000U)>>24) | \
                    (((uint5)(N)&0x00ff0000U)>>8)  | \
                    (((uint5)(N)&0x0000ff00U)<<8)  | \
                    (((uint5)(N)&0x000000ffU)<<24))

#else

#define FIXHALF(N) (N)
#define FIXWORD(N) (N)

#endif

#endif
