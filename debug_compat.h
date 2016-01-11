

#define DEBUG 1

#if defined(DEBUG) && DEBUG > 0 && defined(__x86__)



#define DEBUGV_ERROR(fmt, args...) fprintf(stderr, "ERROR: %s:%d:%s(): " fmt, \
    __FILE__, __LINE__, __func__, ##args)

#define DEBUGV(fmt, args...) fprintf(stdout, "DEBUG: %s:%d:%s(): " fmt, \
    __FILE__, __LINE__, __func__, ##args)


#if defined(DEBUG) && DEBUG > 2 && defined(__x86__)

    #define DEBUGV3(fmt, args...) fprintf(stdout, "DEBUG: %s:%d:%s(): " fmt, \
        __FILE__, __LINE__, __func__, ##args)

    #define DEBUGV3_APPEND(fmt, args...) fprintf(stdout, fmt, \
    ##args)

#else
    #define DEBUGV3(...)

    #define DEBUGV3_APPEND(...)

#endif



#elif defined(DEBUG) && DEBUG > 0 && defined(ARM_CM4_FP)


#define DEBUGV_ERROR(fmt, args...) ::printf("ERROR: %s:%d:%s(): " fmt, \
    __FILE__, __LINE__, __func__, ##args)

#define DEBUGV(fmt, args...) ::printf("DEBUG: %s:%d:%s(): " fmt, \
    __FILE__, __LINE__, __func__, ##args)


#if defined(DEBUG) && DEBUG > 2 && defined(ARM_CM4_FP)

    #define DEBUGV3(fmt, args...) ::printf("DEBUG3: %s:%d:%s(): " fmt, \
        __FILE__, __LINE__, __func__, ##args)

    #define DEBUGV3_APPEND(fmt, args...) ::printf(fmt, ##args)

#else

    #define DEBUGV3(...)

    #define DEBUGV3_APPEND(...)

#endif

#else



#define DEBUGV(...)

#define DEBUGV3(...)

#define DEBUGV_ERROR(...)

#define DEBUGV3_APPEND(...)



#endif


// measures

#define START_MEASURE() \
	uint64_t start, stop; \
    volatile int cycle_count=0; \
    start = clockgettime(); \
    DEBUGV3("START_TIME: %lld\n", start);

#define COUNT_CYCLE() \
    cycle_count++;

#define STOP_MEASURE() \
    stop = clockgettime(); \
    DEBUGV("DURATION_TIME:%llu:%u\n", stop-start, cycle_count);
