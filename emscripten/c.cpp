
// https://github.com/Anil8753/WASMDemo/blob/master/CallJsFromCpp/src/main.cpp

#include <iostream>
#include <string>

#include <emscripten.h>
#include <emscripten/bind.h>
#include <emscripten/val.h>

EM_JS(void, call_js_agrs, (const char *title, int lentitle, const char *msg, int lenmsg), {
    console.log(UTF8ToString(title, lentitle), UTF8ToString(msg, lenmsg));
});

bool callJsBackWithAgrs()
{
    const std::string title = "Hello from C++";
    const std::string msg = "This string is passed as a paramter from C++ code!";
    call_js_agrs(title.c_str(), title.length(), msg.c_str(), msg.length());
    return true;
}

EMSCRIPTEN_BINDINGS(module)
{
    emscripten::function("callJsBackWithAgrs", &callJsBackWithAgrs);
}


int main() {
  printf("try call:\n");
  callJsBackWithAgrs();
  return 0;
}
