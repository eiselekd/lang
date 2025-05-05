EM_JS(void, call_js_agrs, (const char *title, int lentitle, const char *msg, int lenmsg), {
    jsMethodAgrs(UTF8ToString(title, lentitle), UTF8ToString(msg, lenmsg));
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
