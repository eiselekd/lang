class a {
public:
    a(int v) : v(v) {};
    int v;
};
class b : public a {
public:
    b(int v) : a(v) {};
};
class c : public a {
public:
    c(int v) : a(v) {};
};
a nullelem0(1);
b nullelem1(1);

template <typename _Tp, _Tp n>
class h
{
public:
    bool isnull(_Tp a) { return a == n; }
};

int main(int argc, char **argv) {

    h<a *, &nullelem0> n0;
    c p1(2);
    n0.isnull(&nullelem0);
    n0.isnull(&p1);

    h<a *, (a*)&nullelem1> n1;

    return 0;
}
