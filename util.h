#ifndef UTIL_H
#define UTIL_H
#include <stdarg.h> // va_list

namespace glsl {

// An implementation of std::find
template <typename I, typename T>
static inline I find(I first, I last, const T &value) {
    for (; first != last; ++first)
        if (*first == value)
            return first;
    return last;
}

// An implementation of vasprintf
int allocvfmt(char **str, const char *fmt, va_list vp);

// An implementation of vsprintf
int allocfmt(char **str, const char *fmt, ...);

}

#endif
