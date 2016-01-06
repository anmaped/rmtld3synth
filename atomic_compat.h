#ifndef _ATOMIC_COMPAT_H_
#define _ATOMIC_COMPAT_H_

// arch dependent macros

/* there is a big difference between __x86__ implementation and __ARM_CM4__
 * arm implements the atomic operations using Load-link/store-conditional
 * instructions and __x86__ implements it using compare-and-swap (CAS)
 *
 * ARM implementation is too restrictive. it ensures that any inter-living
 *   is detected from any source (e.g., interrupts and shared memory among cores).
 * X86 implementation is too relaxed. ABA problem can occur since a thread B can change
 *   the value to the expected one by the thread A that started the atomic operation.
 *
 *  Contention may be an issue... [TO CONFIRM]
 */


#ifdef ARM_CM4_FP

	#include <ARMCM4_FP.h>

 	#include <atomic>

	/*#define ATOMIC_begin(new_value, expression, dest) \
		__DMB(); \
	    do { \
	        new_value = expression __LDREXW((uint32_t *) dest);

	#define ATOMIC_end(new_value, dest) \
	       } while ( __STREXW(new_value, (uint32_t *) dest) );*/

	#define ATOMIC_begin_VALUE64(dest) \
		bool fail = false; \
        uint64_t old_value64 = (uint64_t) std::atomic_load(&dest); \
        do { \
        	if(fail) { pthread_yield(); }


	#define ATOMIC_end_VALUE64(new_value, dest) \
	} while((fail = !std::atomic_compare_exchange_strong(&dest, &old_value64, (uint64_t)new_value)));

	#define ATOMIC_begin_VALUE64_NOEXCHANGE(dest) \
        uint64_t old_value64 = (uint64_t) std::atomic_load(&dest); \
        do { 

	#define ATOMIC_end_VALUE64_NOEXCHANGE(dest) \
	} while( !( std::atomic_load(&dest) == old_value64 ) );

#elif defined __x86__

	#include <atomic>

	// code for x86

	/*#define ATOMIC_begin(dest) \
        uint32_t * old_value = (uint32_t *) std::atomic_load(&dest); \
        do { 


	#define ATOMIC_end(new_value, dest) \
	} while(!std::atomic_compare_exchange_strong(&dest, &old_value, (uint32_t*)new_value));*/


	#define ATOMIC_begin_VALUE64(dest) \
		bool fail = false; \
        uint64_t old_value64 = (uint64_t) std::atomic_load(&dest); \
        do { \
        	if(fail) { pthread_yield(); }


	#define ATOMIC_end_VALUE64(new_value, dest) \
	} while((fail = !std::atomic_compare_exchange_strong(&dest, &old_value64, (uint64_t)new_value)));

	#define ATOMIC_begin_VALUE64_NOEXCHANGE(dest) \
        uint64_t old_value64 = (uint64_t) std::atomic_load(&dest); \
        do { 

	#define ATOMIC_end_VALUE64_NOEXCHANGE(dest) \
	} while( !( std::atomic_load(&dest) == old_value64 ) );

#else

	#warning "Atomic guarantees are not supported!"

	#define ATOMIC_begin()
	#define ATOMIC_end()

#endif

#endif //_ATOMIC_COMPAT_H_
