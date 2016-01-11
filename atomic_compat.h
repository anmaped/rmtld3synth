#ifndef _ATOMIC_COMPAT_H_
#define _ATOMIC_COMPAT_H_

// arch dependent macros

/* there is a big difference between __x86__ implementation and __ARM_CM4__
 * arm implements the atomic operations using Load-link/store-conditional
 * instructions and __x86__ implements it using compare-exchange.
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

	#define ATOMIC_begin(expression, dest) \
 		uint32_t __lst_; \
		__DMB(); \
	    do { \
	        __lst_ = expression __LDREXW((uint32_t *) dest);

	#define ATOMIC_end(dest) \
	       } while ( __STREXW(__lst_, (uint32_t *) dest) );

	#define DMB __DMB();
	#define DSB __DSB();
	#define ISB __ISB();

	#define FRAME_ADDRESS \
	    frame_address

	#define OLD_FRAME_ADDRESS \
	    __old_value32

	#define FRAME_ADDRESS_type \
	    std::atomic<uint32_t>

	#define FRAME_ADDRESS_subtype uint32_t
	

	#define ATOMIC_begin_VALUE64(dest) \
		bool fail = false; \
        uint32_t OLD_FRAME_ADDRESS = (uint32_t) std::atomic_load(&dest); \
        do { \
        	if(fail) { pthread_yield(); }

	#define ATOMIC_end_VALUE64(new_value, dest) \
	} while((fail = !std::atomic_compare_exchange_strong(&dest, &OLD_FRAME_ADDRESS, (uint32_t)new_value)));


	#define ATOMIC_begin_VALUE64_NOEXCHANGE(dest) \
        uint32_t OLD_FRAME_ADDRESS = (uint32_t) std::atomic_load(&dest); \
        do { 

	#define ATOMIC_end_VALUE64_NOEXCHANGE(dest) \
	} while( !( std::atomic_load(&dest) == OLD_FRAME_ADDRESS ) );


#elif defined __x86__


	#include <atomic>

	#define DMB 

	#define FRAME_ADDRESS \
	    __counter

	#define OLD_FRAME_ADDRESS \
	    __old_value64

	#define FRAME_ADDRESS_type \
	    std::atomic<uint64_t>

	#define FRAME_ADDRESS_subtype uint64_t


	#define ATOMIC_begin_VALUE64(dest) \
		bool fail = false; \
        uint64_t OLD_FRAME_ADDRESS = (uint64_t) std::atomic_load(&dest); \
        do { \
        	if(fail) { pthread_yield(); }


	#define ATOMIC_end_VALUE64(new_value, dest) \
	} while((fail = !std::atomic_compare_exchange_strong(&dest, &OLD_FRAME_ADDRESS, (uint64_t)new_value)));

	#define ATOMIC_begin_VALUE64_NOEXCHANGE(dest) \
        uint64_t OLD_FRAME_ADDRESS = (uint64_t) std::atomic_load(&dest); \
        do { 

	#define ATOMIC_end_VALUE64_NOEXCHANGE(dest) \
	} while( !( std::atomic_load(&dest) == OLD_FRAME_ADDRESS ) );

#else

	#warning "Atomic guarantees are not supported!"

	#define ATOMIC_begin()
	#define ATOMIC_end()

#endif

#endif //_ATOMIC_COMPAT_H_
