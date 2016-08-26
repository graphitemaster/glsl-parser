#ifndef UTIL_H
#define UTIL_H
#include <stdarg.h> // va_list
#include <vector>

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

// a tiny wrapper around std::vector so you can provide your own
template <typename T>
struct vector {
    size_t size() const { return m_data.size(); }
    bool empty() const { return m_data.empty(); }
    const T& operator[](size_t index) const { return m_data[index]; }
    T& operator[](size_t index) { return m_data[index]; }
    T* begin() { return &m_data[0]; }
    T* end() { return &m_data[size()]; }
    const T* begin() const { return &m_data[0]; }
    const T* end() const { return &m_data[size()]; }
    void insert(T *at, const T& value = T()) { m_data.insert(m_data.begin() + size_t(at - begin()), value); }
    void insert(T *at, const T *beg, const T *end) { m_data.insert(m_data.begin() + size_t(at - begin()), beg, end); }
    void push_back(const T &value) { m_data.push_back(value); }
    void reserve(size_t size) { m_data.reserve(size); }
    T* erase(T *position) { return &*m_data.erase(m_data.begin() + size_t(position - begin())); }
    T* erase(T *first, T *last) { return &*m_data.erase(m_data.begin() + size_t(first - begin()), m_data.begin() + size_t(last - begin())); }
    void pop_back() { m_data.pop_back(); }
    T &front() { return *begin(); }
    const T &front() const { return *begin(); }
    T &back() { return *(end() - 1); }
    const T& back() const { return *(end() - 1); }
    void resize(size_t size) { m_data.resize(size); }
private:
    std::vector<T> m_data;
};

}

#endif
