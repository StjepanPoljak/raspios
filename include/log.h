#ifndef LOG_H
#define LOG_H

#define LOG_ERROR "!"
#define LOG_WARNING "*"
#define LOG_INFO "i"
#define LOG_TEST "T"
#define LOG_FAIL "F"

#define _log(suffix, value) print ## suffix(value)

#define log(suffix, value, level) \
do { \
	print("(" level ") [" __FILE__ "] "); \
	_log(suffix, value); \
} while(0)

#define log_fail(suffix, value) log(suffix, value, LOG_FAIL)
#define log_test(suffix, value) log(suffix, value, LOG_TEST)
#define log_info(suffix, value) log(suffix, value, LOG_INFO)
#define log_warning(suffix, value) log(suffix, value, LOG_WARNING)
#define log_error(suffix, value) log(suffix, value, LOG_ERROR)

#endif
