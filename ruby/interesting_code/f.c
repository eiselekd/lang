

int func1(int c) {
    int a;
    a = 1+0x1233;
    c = a << a;
    return c;
}

int main() {
    return func1(2);
}
