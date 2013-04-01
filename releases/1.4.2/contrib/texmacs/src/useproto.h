
/* released under the Modified BSD License */

#ifndef _USEPROTO_H_
#define _USEPROTO_H_ 1

#if defined(SGIplatform)||defined(LINUXplatform)||defined(HPplatform) ||defined(RIOSplatform) ||defined(RIOS4platform) || defined(SUN4OS5platform)
#ifdef _NO_PROTO
#undef _NO_PROTO
#endif
#ifndef NeedFunctionPrototypes
#define NeedFunctionPrototypes 1
#endif
#endif /*SGIplatform ... */


#if defined(ALPHAplatform)
#ifdef __STDC__

#ifdef _NO_PROTO
#undef _NO_PROTO
#endif
#ifndef NeedFunctionPrototypes
#define NeedFunctionPrototypes 1
#endif

#else 

#define _NO_PROTO
#undef NeedFunctionPrototypes

#endif
#endif /* ALPHA */



#ifdef SUNplatform
#define _NO_PROTO
#define const   
#endif

#endif /* _USEPROTO_H_ */
 
