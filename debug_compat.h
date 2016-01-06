

#define DEBUG 1

#if defined(DEBUG) && DEBUG > 0 && defined(__x86__)

#define DEBUGV_ERROR(fmt, args...) fprintf(stderr, "ERROR: %s:%d:%s(): " fmt, \
    __FILE__, __LINE__, __func__, ##args)

#define DEBUGV(fmt, args...) fprintf(stdout, "DEBUG: %s:%d:%s(): " fmt, \
    __FILE__, __LINE__, __func__, ##args)

#define DEBUGV3(...)

#elif defined(DEBUG) && DEBUG > 2 && defined(__x86__)

#define DEBUGV3(...) (fmt, args...) fprintf(stdout, "DEBUG: %s:%d:%s(): " fmt, \
    __FILE__, __LINE__, __func__, ##args)

#else

#define DEBUGV(...)

#define DEBUGV3(...)

#define DEBUGV_ERROR

#endif


// measures

#define START_MEASURE() \
	uint64_t start, stop; \
    volatile int cycle_count=0; \
    start = clockgettime(); \
    DEBUGV("START_TIME: %lld\n", start);

#define COUNT_CYCLE() \
    cycle_count++;

#define STOP_MEASURE() \
    stop = clockgettime(); \
    DEBUGV("DURATION_TIME:%lld:%d\n", stop-start, cycle_count);

