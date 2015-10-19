#ifndef _ATOMIC_COMPAT_H_
#define _ATOMIC_COMPAT_H_

#ifdef ARM_CM4_FP

	#include <ARMCM4_FP.h>

	// arch dependent macros
	#define ATOMIC_begin(new_value, expression, dest) \
		__DMB(); \
	    do { \
	        new_value = expression __LDREXW((uint32_t *) dest);

	#define ATOMIC_end(new_value, dest) \
	       } while ( __STREXW(new_value, (uint32_t *) dest) );

#else

	// atomic guarantees are not ensured
	#warning "Atomic guarantees are not ensured!"

	#define ATOMIC_begin()
	#define ATOMIC_end()

#endif

#endif //_ATOMIC_COMPAT_H_