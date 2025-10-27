/*
 * MiniCargo - mrustc's minimal clone of cargo
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * debug.cpp
 * - Debugging helpers
 */
#if defined(__MINGW32__)
# define DISABLE_MULTITHREAD    // Mingw32 doesn't have c++11 threads
#endif
#include <set>
#include <iostream>
#include "debug.h"
#include <mutex>

static int giIndentLevel = 0;
static const char* gsDebugPhase = "";
static ::std::set<::std::string> gmDisabledDebug;
#ifndef DISABLE_MULTITHREAD
static ::std::mutex gDebugLock;
#endif

void Debug_SetPhase(const char* phase_name)
{
    gsDebugPhase = phase_name;
}
bool Debug_IsEnabled()
{
    if( gmDisabledDebug.find(gsDebugPhase) != gmDisabledDebug.end() )
        return false;
    return true;
}
void Debug_DisablePhase(const char* phase_name)
{
    gmDisabledDebug.insert( ::std::string(phase_name) );
}
void Debug_EnablePhase(const char* phase_name)
{
    auto it = gmDisabledDebug.find(phase_name);
    if( it != gmDisabledDebug.end() )
    {
        gmDisabledDebug.erase(it);
    }
}
void Debug_Print(::std::function<void(::std::ostream& os)> cb)
{
    if( !Debug_IsEnabled() )
        return ;
#ifndef DISABLE_MULTITHREAD
    ::std::unique_lock<::std::mutex>    _lh { gDebugLock };
#endif

    ::std::cout << gsDebugPhase << "- ";
    for(auto i = giIndentLevel; i --; )
        ::std::cout << " ";
    cb(::std::cout);
    ::std::cout << ::std::endl;
}
void Debug_EnterScope(const char* name, dbg_cb_t cb)
{
    if( !Debug_IsEnabled() )
        return ;
#ifndef DISABLE_MULTITHREAD
    ::std::unique_lock<::std::mutex>    _lh { gDebugLock };
#endif

    ::std::cout << gsDebugPhase << "- ";
    for(auto i = giIndentLevel; i --; )
        ::std::cout << " ";
    ::std::cout << ">>> " << name << "(";
    cb(::std::cout);
    ::std::cout << ")" << ::std::endl;
    giIndentLevel ++;
}
void Debug_LeaveScope(const char* name, dbg_cb_t cb)
{
    if( !Debug_IsEnabled() )
        return ;
#ifndef DISABLE_MULTITHREAD
    ::std::unique_lock<::std::mutex>    _lh { gDebugLock };
#endif

    ::std::cout << gsDebugPhase << "- ";
    giIndentLevel --;
    for(auto i = giIndentLevel; i --; )
        ::std::cout << " ";
    ::std::cout << "<<< " << name << ::std::endl;
}
