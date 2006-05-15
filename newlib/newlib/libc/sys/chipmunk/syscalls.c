/* thanks to Bill Gatliff's site */

#include <_ansi.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/fcntl.h>
#include <stdio.h>
#include <time.h>
#include <sys/time.h>
#include <sys/times.h>
#include <errno.h>
#include <reent.h>
#include <stdarg.h>

/* was 1024*1024, FAR too small. Doh! */
/* A heap of 512mb should suffice... */
#define HEAPSIZE 1024*1024*512

typedef enum {
  DOP_SYS_READ,
  DOP_SYS_WRITE,
  DOP_SYS_OPEN,
  DOP_SYS_CLOSE,
  DOP_SYS_FSTAT,
  DOP_SYS_TIME
} dop_fake_syscall;

typedef struct {
  const char* name;
  int (*open_r) (struct _reent* r, const char* path, int flags, int mode);
  int (*close_r) (struct _reent* r, int fd);
  int (*write_r) (struct _reent* r, int fd, const void* ptr, int len);
  int (*read_r) (struct _reent* r, int fd, void* ptr, int len);
} devoptab_t;

int com1_open_r(struct _reent*, const char*, int, int);
int com1_close_r(struct _reent*, int fd);
int com1_write_r(struct _reent* r, int fd, const void*, int);
int com1_read_r(struct _reent* r, int fd, void*, int);

const devoptab_t devoptab_com1 = {
  "com1",
  com1_open_r,
  com1_close_r,
  com1_write_r,
  com1_read_r
};

const devoptab_t *devoptab_list[] = {
  &devoptab_com1, /* standard input */
  &devoptab_com1, /* standard output */
  &devoptab_com1, /* standard error */
  0
};

_ssize_t write(int fd, const void* buf, size_t cnt)
{
  return _write_r(0, fd, buf, cnt);
}

_ssize_t _write_r(struct _reent* ptr, int fd, const void* buf, size_t cnt)
{
  if (fd<4)
    return devoptab_list[fd]->write_r(ptr, fd, buf, cnt);
  else
    return file_write_r(ptr, fd, buf, cnt);
}

_ssize_t read(int fd, void* buf, size_t cnt)
{
  return _read_r(0, fd, buf, cnt);
}

_ssize_t _read_r(struct _reent* ptr, int fd, void* buf, size_t cnt)
{
  if (fd<4)
    return devoptab_list[fd]->read_r(ptr, fd, buf, cnt);
  else
    return file_read_r(ptr, fd, buf, cnt);
}

int open(const char* file, int flags, ...)
{
  va_list ap;
  int ret;
  va_start(ap, flags);
  ret = _open_r(0, file, flags, va_arg(ap, int));
  va_end(ap);
  return ret;
}

int _open_r(struct _reent* ptr, const char* file, int flags, int mode)
{
  int which_devoptab = 0;
  int fd = -1;
  
  /* search for "file" in dotab_list[].name */
  do {
    if (strcmp(devoptab_list[which_devoptab]->name, file) == 0)
    {
      fd = which_devoptab;
      break;
    }
  } while (devoptab_list[which_devoptab++]);
  
  /* if found, invoke open_r method */
  if (fd != -1)
  {
    devoptab_list[fd]->open_r(ptr, file, flags, mode);
  }
  else
  {
    return file_open_r(ptr, file, flags, mode);
/*    ptr->_errno = ENODEV;*/
  }
  
  return fd;
}

int close(int fd)
{
  return _close_r(0, fd);
}

int _close_r(struct _reent* ptr, int fd)
{
  if (fd<4)
    return devoptab_list[fd]->close_r(ptr, fd);
  else
    return file_close_r(ptr, fd);
}

int com1_open_r(struct _reent* r, const char* path, int flags, int mode)
{
  /* we can no-op for now */
}

int com1_close_r(struct _reent* r, int fd)
{
  /* also no-op */
}

int com1_write_r(struct _reent* r, int fd, const void* ptr, int len)
{
  int i;
  
  /* this should probably be a syscall of some sort */
  for (i=0; i<len; i++)
  {
    *((volatile int*)0xf0000000) = ((const char*)ptr)[i];
  }
  
  return len;
}

int com1_read_r(struct _reent* r, int fd, void* ptr, int len)
{
  /* erm read something */
}

/*unsigned char heap[HEAPSIZE];*/
extern unsigned char ___dop_heapbase___;

void* _sbrk_r(struct _reent* r, ptrdiff_t incr)
{
  static unsigned char* heap_end = 0;
  unsigned char* prev_heap_end, *heap;
  
  heap = &___dop_heapbase___;
  
  /* initialize */
  if (heap_end == 0) heap_end = heap;
  
  prev_heap_end = heap_end;
  
  if (heap_end + incr - heap > HEAPSIZE)
  {
/*    write(2, "Heap overflow!\n", 15);*/
    abort();
  }
  
  heap_end += incr;
  
  return (void*) prev_heap_end;
}

int _stat_r(struct _reent* r, const char* file, struct stat* pstat)
{
  pstat->st_mode = S_IFCHR;
  return 0;
}

int _fstat_r(struct _reent* r, int x, struct stat* s)
{
  return 0;
}

_off_t _lseek_r(struct _reent* r, int a, _off_t t, int b)
{
  return 0;
}

int _getpid_r(struct _reent* r)
{
  return 1;
}

int _kill_r(struct _reent* r, int pid, int sig)
{
  return 1;
}

void isatty(void) {}

void sleep(int c)
{
  /* I'm not sleepy though */
}

int file_open_r(struct _reent* r, const char* file, int flags, int mode)
{
  register int path asm ("r0") = (int)file;
  register int flgs asm ("r1") = flags;
  register int md asm ("r2") = mode;
  register int res asm ("r0");
  asm volatile ("swi 3" : "=r" (res)
                        : "r" (path), "r" (flgs), "r" (md)
                        : "r3");
  return res;
}

int file_close_r(struct _reent* r, int fd)
{
  register int descr asm ("r0") = fd;
  register int succ asm ("r0");
  asm volatile ("swi 4" : "=r" (succ) : "r" (descr) : "r1", "r2", "r3");
  return succ;
}

int file_read_r(struct _reent* r, int fd, void* buf, size_t count)
{
  register int descr asm ("r0") = fd;
  register int buffer asm ("r1") = (int) buf;
  register int cnt asm ("r2") = (int) count;
  register int res asm ("r0");
  asm volatile ("swi 1" : "=r" (res)
                        : "r" (descr), "r" (buffer), "r" (cnt)
                        : "r3");
  return res;
}

int file_write_r(struct _reent* r, int fd, void* buf, size_t count)
{
  register int descr asm ("r0") = fd;
  register int buffer asm ("r1") = (int) buf;
  register int cnt asm ("r2") = (int) count;
  register int res asm ("r0");
  asm volatile ("swi 2" : "=r" (res)
                        : "r" (descr), "r" (buffer), "r" (cnt)
                        : "r3");
  return res;
}

int fstat(int fd, struct stat* sbuf)
{
  register int descr asm ("r0") = fd;
  register int buf asm ("r1") = (int)sbuf;
  register int succ asm ("r0");
  asm volatile ("swi 5" : "=r" (succ)
                        : "r" (descr), "r" (buf)
                        : "r2", "r3");
  return succ;
}

int getwd(void)
{
  return ENOSYS;
}

int pathconf(void)
{
  return ENOSYS;
}

int dup(int fd)
{
  register int oldfd asm ("r0") = fd;
  register int res asm ("r0");
  asm volatile ("swi 7" : "=r" (res)
                        : "r" (oldfd)
                        : "r1", "r2", "r3");
  return res;
}

int dup2(int ofd, int nfd)
{
  register int oldfd asm ("r0") = ofd;
  register int newfd asm ("r1") = nfd;
  register int res asm ("r0");
  asm volatile ("swi 8" : "=r" (res)
                        : "r" (oldfd), "r" (newfd)
                        : "r2", "r3");
  return res;
}

int _time_r(struct _reent* r, time_t* t)
{
  return time(t);
}

time_t time(time_t* timer)
{
  register int tptr asm ("r0") = (int)timer;
  register int res asm ("r0");
  asm volatile ("swi 6" : "=r" (res)
                        : "r" (tptr)
                        : "r1", "r2", "r3");
  return res;
}

int chown(const char* path, uid_t owner, gid_t group)
{
  register int pathname asm ("r0") = (int)path;
  register int own asm ("r1") = (int)owner;
  register int grp asm ("r2") = (int)group;
  register int succ asm ("r0");
  asm volatile ("swi 9" : "=r" (succ)
                        : "r" (pathname), "r" (own), "r" (grp)
                        : "r3");
  return succ;
}

int chmod(const char* path, mode_t mode)
{
  register int pathname asm ("r0") = (int)path;
  register int themode asm ("r1") = (int)mode;
  register int succ asm ("r0");
  asm volatile ("swi 10" : "=r" (succ)
                         : "r" (pathname), "r" (themode)
                         : "r2", "r3");
  return succ;
}

off_t lseek(int fildes, off_t offset, int whence)
{
  register int descr asm ("r0") = fildes;
  register int offs asm ("r1") = offset;
  register int typ asm ("r2") = whence;
  register int res asm ("r0");
  asm volatile ("swi 11" : "=r" (res)
                         : "r" (descr), "r" (offs), "r" (typ)
                         : "r3");
  return res;
}

int lstat(const char* filename, struct stat* buf)
{
  register int file asm ("r0") = (int)filename;
  register int buffer asm ("r1") = (int)buf;
  register int res asm ("r0");
  asm volatile ("swi 12" : "=r" (res)
                         : "r" (file), "r" (buffer)
                         : "r2", "r3");
  return res;
}

int stat(const char* filename, struct stat* buf)
{
  register int file asm ("r0") = (int)filename;
  register int buffer asm ("r1") = (int)buf;
  register int res asm ("r0");
  asm volatile ("swi 13" : "=r" (res)
                         : "r" (file), "r" (buffer)
                         : "r2", "r3");
  return res;
}

int _unlink_r(struct _reent* r, const char* pathname)
{
  unlink(pathname);
}

int unlink(const char* pathname)
{
  register int path asm ("r0") = (int)pathname;
  register int res asm ("r0");
  asm volatile ("swi 14" : "=r" (res)
                         : "r" (path)
                         : "r1", "r2", "r3");
  return res;
}

int utime(const char* filename, struct utimbuf* buf)
{
  register int file asm ("r0") = (int)filename;
  register int buffer asm ("r1") = (int)buf;
  register int res asm ("r0");
  asm volatile ("swi 15" : "=r" (res)
                         : "r" (file), "r" (buffer)
                         : "r2", "r3");
  return res;
}

clock_t _times_r(struct _reent *r, struct tms *buf)
{
  return 0;
}
