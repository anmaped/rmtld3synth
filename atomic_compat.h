#ifndef _ATOMIC_COMPAT_H_
#define _ATOMIC_COMPAT_H_

// arch dependent macros

#ifdef ARM_CM4_FP

	#include <ARMCM4_FP.h>

	#define ATOMIC_begin(new_value, expression, dest) \
		__DMB(); \
	    do { \
	        new_value = expression __LDREXW((uint32_t *) dest);

	#define ATOMIC_end(new_value, dest) \
	       } while ( __STREXW(new_value, (uint32_t *) dest) );

#elif defined __x86__

	#include <atomic>

	// code for x86
	#define ATOMIC_begin(old_value, expression, dest) \
        int old_value = *atomic_load(dest); \
	int __new_value = expression old_value; \
        do { 

	#define ATOMIC_end(old_value, dest) \
	} while(!std::atomic_compare_exchange_strong(dest, *old_value, __new_value));

#else

	// atomic guarantees are not ensured
	#warning "Atomic guarantees are not ensured!"

	#define ATOMIC_begin()
	#define ATOMIC_end()

#endif

#endif //_ATOMIC_COMPAT_H_
