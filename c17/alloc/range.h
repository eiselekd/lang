#ifndef RANGE_IT_H_
#define RANGE_IT_H_

template <class T>
class range_ {
private:
    class iter {
    private:
        T at;
    public:
        iter(T at) : at(at) {}
        bool operator!=(iter const& other) const { return at != other.at; }
        T const& operator*() const { return at; }
        iter& operator++() { ++at; return *this; }
    };

    T begin_val;
    T end_val;
public:
    range_(T begin_val, T end_val) :
        begin_val(begin_val), end_val(end_val) { }
    range_(T end_val) :
        begin_val(0), end_val(end_val) { }
    iter begin() { return iter(begin_val); }
    iter end() { return iter(end_val); }
};

template <typename c> range_<c> range(c i) { return range_<c>(i); };

#endif
