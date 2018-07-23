struct Expr { virtual int eval () = 0; };
struct Value : Expr {
    Value() : Expr() {}; int eval () { }; int value; };
struct Plus : Expr {
    Plus() : Expr() {}; int eval () { };  Expr* e1; Expr* e2; };

#include <mach7/type_switchN-patterns-xtl.hpp>  // Match()
#include <mach7/patterns/constructor.hpp>       // mch::C
#include <iostream>

using mch::C;
using namespace std;

int eval (const Expr& e)
{
    Match(e)
	Case(C<Value>()) { std::cout << match0.value; printf ("Value"); };
        Case(C<Plus>())  { printf ("Plus"); };
    EndMatch
}

int
main(int argc, char **argv) {
    Plus a;
    eval(a);
}
