/*
   Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
   All rights reserved.

   Copyright (C) 2007-2008, Gabriel Dos Reis.
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are
   met:

       - Redistributions of source code must retain the above copyright
         notice, this list of conditions and the following disclaimer.

       - Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in
         the documentation and/or other materials provided with the
         distribution.

       - Neither the name of The Numerical Algorithms Group Ltd. nor the
         names of its contributors may be used to endorse or promote products
         derived from this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
   IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
   TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
   PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
   OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "openaxiom-c-macros.h"

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <math.h>


#ifdef __WIN32__
#  include <windows.h>
#else
#  include <dirent.h>
#endif

#include "cfuns.h"

/* Most versions of Windows don't have the POSIX functions getuid(),
   geteuid(), getgid(), and getegid().  The following definitions are
   approximations, to patch for the deficiencies of Windows
   POSIX interface.  */

#if !HAVE_DECL_GETUID
#  define getuid() 0
#endif

#if !HAVE_DECL_GETGID
#  define getgid() 0
#endif

#if !HAVE_DECL_GETEUID
#  define geteuid() getuid()
#endif

#if !HAVE_DECL_GETEGID
#  define getegid() getgid()
#endif

OPENAXIOM_EXPORT int
addtopath(char *dir)
{
    char *path, *newpath;

    path = getenv("PATH");
    if (path == NULL)
        return -1;

    newpath = (char *) malloc(1 + strlen(path) + strlen(dir) + strlen("PATH=:"));
    if (newpath == NULL)
        return -1;

    sprintf(newpath, "PATH=%s:%s", path, dir);

    return putenv(newpath);
}

/*
 * Test whether the path is the name of a directory.  Returns 1 if so, 0 if
 * not, -1 if it doesn't exist.
 */


OPENAXIOM_EXPORT int
directoryp(char *path)
{
    struct stat buf;
    int code = stat(path, &buf);

    return code == -1 ? -1 : S_ISDIR(buf.st_mode);
}

OPENAXIOM_EXPORT int 
make_path_from_file(char *s, char *t)
{
    char *pos = "";
    char *c;

    /** simply copies the path name from t into s **/
    for (c = t + strlen(t); c != s; c--)
        if (*c == '/') {
            pos = c;
            break;
        }
    /** Check to see if the path was actually present **/
    if (c == t) {               /** No Path, so return the pwd **/
        return (-1);
    }
    /** now just do the copying **/
    strncpy(s, t, pos - t);
    return 1;
}

/* The functions writeablep() and readablep() determine write and
   read access of a file designated by its name.  The function
   axiom_has_write_access is a sub-routine of writeablep.

   The access is determined based on the POSIX semantics; see
   "Advanced Programming in the UNIX Environement", section 4.5.

   1. If the effective user ID of the process is 0 (the superuser),
      access is allowed.  This gives the superuser free rein throughout
      the entire file system.

   2. If the effective user ID of the process equals the owner ID of
      the file (i.e., the process owns the file), access is allowed
      if the appropriate user access permission bit is set. [...]

   3. If the effective group ID of the process or one of the
      supplementary group IDs of the process equals the group ID
      of the file, access is allowed if the appropriate
      group access permission bit is set.  Otherwise, permission
      is denied.

   4. If the appropriate other access permission bit is set, access is
      allowed.  Otherwise, permission is defined.   */


/* Return 1 if the process has write access of file as explained above.
   Otherwise, return 0.  */

static inline int
axiom_has_write_access(const struct stat* file_info)
{
   int effetive_uid = geteuid();

   if (effetive_uid == 0)
      return 1;

   if (effetive_uid == file_info->st_uid)
      return file_info->st_mode & S_IWUSR;

#ifdef S_IWGRP
   if (getegid() == file_info->st_gid)
      return file_info->st_mode & S_IWGRP;
#endif

#ifdef S_IWOTH
   return file_info->st_mode & S_IWOTH;
#else
   return 0;
#endif   
}


/* Return
     -1 if the file designated by PATH is inexistent.
      0 if the file exists but wirte access is denied.
      1 if the file exists and process has write access.
      2 if the file does not exists but process has write
        has write access to the dirname of path.  */

OPENAXIOM_EXPORT int
writeablep(char *path)
{
    struct stat buf;
    char newpath[100];
    int code;

    code = stat(path, &buf);
    if (code == -1) {
        /** The file does not exist, so check to see
                 if the directory is writable                  *****/
        if (make_path_from_file(newpath, path) == -1
            || stat(newpath, &buf) == -1)
           return -1;

        return 2 * axiom_has_write_access(&buf);
    }

    return axiom_has_write_access(&buf);
}


/* Return
     -1 if the file designated by PATH is inexistent.
      0 if the file exists but process has no read access.
      1 if the file exists and read access is granted.  */

OPENAXIOM_EXPORT int
readablep(char *path)
{
    struct stat buf;
    int code;

    code = stat(path, &buf);
    if (code == -1)
        return -1;

    if (geteuid() == buf.st_uid) 
        return ((buf.st_mode & S_IREAD) != 0);

#ifdef S_IRGRP    
     if (getegid() == buf.st_gid) 
        return ((buf.st_mode & S_IRGRP) != 0);
#endif

#ifdef S_IROTH
     return ((buf.st_mode & S_IROTH) != 0);
#else
     return 0;
#endif
}



OPENAXIOM_EXPORT long
findString(char *file, char *string)
{
    int nstring, charpos;
    FILE *fn;
    char buffer[1024];

    if ((fn = fopen(file, "r")) == NULL)
        return -1;

    for (charpos = 0, nstring = strlen(string);
         fgets(buffer, sizeof buffer, fn) != NULL;
         charpos += strlen(buffer)
        )
        if (!strncmp(buffer, string, nstring))
            return charpos;
    return -1;

}

OPENAXIOM_EXPORT int
copyEnvValue(char *varName, char *buffer)
{
    char *s;

    s = getenv(varName);
    if (s == NULL)
        return 0;
    strcpy(buffer, s);
    return strlen(s);
}

/* Return 1 if the file descriptor FD, as viewed by the Core Executable,
   is attached to a terminal.  */
OPENAXIOM_EXPORT int
std_stream_is_terminal(int fd)
{
   assert(fd > -1 && fd < 3);
#ifdef __WIN32__
   DWORD handle;
   switch (fd) {
   case 0: handle = STD_INPUT_HANDLE; break;
   case 1: handle = STD_OUTPUT_HANDLE; break;
   case 2: handle = STD_ERROR_HANDLE; break;
      
   }
   /* The MS documentation suggests `GetFileType' for determining
      the nature of the file handle.  The return value, in our case,
      is an over approximation of what we are interested int:  Are we
      dealing with a stream connected to a terminal?  The constant
      FILE_TYPE_CHAR characterises character files; in particular
      a console terminal, or a printer.  There is an undocumented
      function `VerifyConsoleIoHandle' to deal precisely with the case
      we are interested in.  However, while availale in Wine, it is
      not available in the MinGW headers.  Consequently, we cannot
      rely on it for the moment.  
      So, we may still get garbage out of this function on MS platforms.  */
   return GetFileType(GetStdHandle(handle)) == FILE_TYPE_CHAR;
#else
   return isatty(fd);
#endif
}

/* Change the process' curretnt directory.  Return zero on success,
   and -1 on failure.  */
OPENAXIOM_EXPORT int
oa_chdir(const char* path)
{
#ifdef __MINGW32__
   return SetCurrentDirectory(path) ? 0 : -1;
#else
   return chdir(path);
#endif /* __MINGW32__ */
}


/* return true if path is `.' or `..'.  */
static inline int
is_dot_or_dotdot(const char* path)
{
   return strcmp(path, ".") == 0 || strcmp(path, "..") == 0;
}

/* Remove a directory entry.  Files are removed, directories are
   recursively walked and there removed.
   Return 0 on success, and -1 on falure.
   In practice, OpenAxiom does not remove directories with
   non-trivial recursive structues.  */
OPENAXIOM_EXPORT int
oa_unlink(const char* path)
{
#ifdef __MINGW32__
   WIN32_FIND_DATA findData;
   HANDLE walkHandle;
   DWORD pathAttributes;

   if (is_dot_or_dotdot(path))
      return -1;

   if ((pathAttributes = GetFileAttributes(path)) == 0xFFFFFFFF)
      return -1;

   if (pathAttributes & ~FILE_ATTRIBUTE_DIRECTORY)
      return DeleteFile(path) ? 0 : -1;
   
   if ((walkHandle = FindFirstFile(path, &findData)) == INVALID_HANDLE_VALUE
       || oa_chdir(path) < 0)
      return -1;
   do {
      if (is_dot_or_dotdot(findData.cFileName))
         continue;
      if (findData.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
         if (oa_chdir(findData.cFileName) < 0)
            return -1;
      }
      else if (!DeleteFile(findData.cFileName))
         return -1;
   } while (FindNextFile(walkHandle, &findData));
   if (!FindClose(walkHandle))
      return -1;
   if (oa_chdir("..") < 0)
      return -1;
   return RemoveDirectory(path) ? 0 : -1;
#else
   struct stat pathstat;
   DIR* dir;
   struct dirent* entry;

   /* Don't ever try to remove `.' or `..'.  */
   if (is_dot_or_dotdot(path))
      return -1;

   if (stat(path, &pathstat) < 0)
      return -1;

   /* handle non dictories first.  */
   if (!S_ISDIR(pathstat.st_mode))
      return unlink(path);

   /* change into the path so that we don't have to form full
      pathnames. */
   if ((dir = opendir(path)) == NULL || oa_chdir(path) < 0)
      return -1;

   while (errno = 0, (entry = readdir(dir)) != NULL) {
      struct stat s;
      if (is_dot_or_dotdot(entry->d_name))
         continue;
      if (stat(entry->d_name, &s) < 0)
         return -1;
      if (S_ISDIR(s.st_mode) && oa_unlink(entry->d_name) < 0)
         return -1;
      else if (unlink(entry->d_name) < 0)
         return -1;
   }
   if (errno != 0)
      return -1;

   /* Now, get one level up, and remove the empty directory.  */
   if (oa_chdir("..") < 0 || closedir(dir) < 0 || rmdir(path) < 0)
      return -1;

   return 0;
#endif /* __MINGW32__ */
}

/* Rename a file or directory.  */
OPENAXIOM_EXPORT int
oa_rename(const char* old_path, const char* new_path)
{
#ifdef __MINGW32__
   return MoveFile(old_path, new_path) ? 0 : -1;
#else
   return rename(old_path, new_path);
#endif
}

/* Create a new directory named `path'.  Return 0 on success,
   and -1 on failure.  */
OPENAXIOM_EXPORT int
oa_mkdir(const char* path)
{
#ifdef __MINGW32__
   return CreateDirectory(path, NULL) ? 0 : -1;
#else
#  define DIRECTORY_PERM ((S_IRWXU|S_IRWXG|S_IRWXO) & ~(S_IWGRP|S_IWOTH))
   return mkdir (path, DIRECTORY_PERM);
#  undef DIRECTORY_PERM   
#endif   
}

/* Run a shell command.  Effectively forward to C's system().  */
OPENAXIOM_EXPORT int
oa_system(const char* cmd)
{
   return system(cmd);
}



/* Return the value of an environment variable.  */
OPENAXIOM_EXPORT char*
oa_getenv(const char* var)
{
#ifdef __MINGW32__   
#define BUFSIZE 128
   char* buf = (char*) malloc(BUFSIZE);
   int len = GetEnvironmentVariable(var, buf, BUFSIZE);
   if (len == 0) {
      free(buf);
      return NULL;
   }
   else if (len > BUFSIZE) {
     buf = (char*) realloc(buf,len);
      len = GetEnvironmentVariable(var, buf, len);
      if (len == 0) {
         free(buf);
         return NULL;
      }
   }
   return buf;
#else
   return getenv(var);
#endif   
}


OPENAXIOM_EXPORT char*
oa_getcwd(void)
{
   int bufsz = 256;
   char* buf = (char*) malloc(bufsz);
#ifdef __MINGW32__
   int n = GetCurrentDirectory(bufsz, buf);
   if (n == 0) {
      perror("oa_getcwd");
      exit(-1);
   }
   else if (n > bufsz) {
      buf = realloc(buf,n);
      if (GetCurrentDirectory(n, buf) != n) {
         perror("oa_getcwd");
         exit(-1);
      }
   }
   return buf;
#else /* __MINGW32__ */
   errno = 0;
   while (getcwd(buf,bufsz) == 0) {
      if (errno == ERANGE) {
         errno = 0;
         bufsz *= 2;
         buf = realloc(buf, bufsz);
      }
      else {
         perror("oa_getcwd");
         exit(-1);
      }
   }
   return buf;
#endif
}

OPENAXIOM_EXPORT int
oa_access_file_for_read(const char* path)
{
#ifdef __MINGW32__
  return GetFileAttributes(path) == INVALID_FILE_ATTRIBUTES ? -1 : 1;
#else
   return access(path, R_OK);
#endif   
}


OPENAXIOM_EXPORT const char*
oa_get_tmpdir(void)
{
#ifdef __MINGW32__
   char* buf;
   /* First, probe.  */
   int bufsz = GetTempPath(0, NULL);
   if (bufsz == 0) {
      perror("oa_get_tmpdir");
      exit(1);
   }
   else {
      int new_size;
      buf = (char*) malloc(bufsz + 1);
      new_size = GetTempPath(bufsz, buf);
      if(new_size = 0 || new_size >= bufsz) {
         perror("oa_get_tmpdir");
         free(buf);
         exit(1);
      }
      buf[new_size] = '\0';
   }
   return buf;
#else
   return "/tmp";
#endif   
}



OPENAXIOM_EXPORT double 
plus_infinity(void )
{
#ifdef INFINITY   
   return INFINITY;
#else
   /* This must be a curious platform.  */
   volatile double zero = 0.0;
   return 1.0 / zero;           /* If it traps, well, it traps.  */
#endif   
}

OPENAXIOM_EXPORT double 
minus_infinity(void)
{
   return -plus_infinity();
}

OPENAXIOM_EXPORT double 
NANQ(void)
{
#ifdef NAN
   return NAN;
#else
   return sqrt(-1.0);            /* Juts pick one.  */
#endif
}
