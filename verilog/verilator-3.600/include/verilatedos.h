// $Id$ -*- C++ -*-
//*************************************************************************
//
// Copyright 2003-2006 by Wilson Snyder. This program is free software; you can
// redistribute it and/or modify it under the terms of either the GNU
// General Public License or the Perl Artistic License.
//
// Verilator is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
//*************************************************************************
///
/// \file
/// \brief Verilator: Common include for OS portability (verilated & verilator)
///
///	This header is used by both verilated code, and the verilator
///	program itself Code needed by only one goes into verilated.h or
///	config.h.in respectively.
///
/// Code available from: http://www.veripool.com/verilator
///
//*************************************************************************


#ifndef _VERILATEDOS_H_
#define _VERILATEDOS_H_ 1 ///< Header Guard

//=========================================================================
// Compiler pragma abstraction

#ifdef __GNUC__
# define VL_ATTR_PRINTF(fmtArgNum) __attribute__ ((format (printf, fmtArgNum, fmtArgNum+1)))
# define VL_ATTR_ALIGNED(alignment) __attribute__ ((aligned (alignment)))
# define VL_ATTR_NORETURN __attribute__ ((noreturn))
# define VL_ATTR_UNUSED __attribute__ ((unused))
# define VL_LIKELY(x)	__builtin_expect(!!(x), 1)
# define VL_UNLIKELY(x)	__builtin_expect(!!(x), 0)
#else
# define VL_ATTR_PRINTF(fmtArgNum)	///< Function with printf format checking
# define VL_ATTR_ALIGNED(alignment)	///< Align structure to specified byte alignment
# define VL_ATTR_NORETURN		///< Function does not ever return
# define VL_ATTR_UNUSED			///< Function that may be never used
# define VL_LIKELY(x)	(!!(x))		///< Boolean expression more often true then false
# define VL_UNLIKELY(x)	(!!(x))		///< Boolean expression more often false then true
#endif

#ifdef VL_THREADED
# ifdef __GNUC__
#  define VL_THREAD	__thread	///< Storage class for thread-local storage
# else
#  error "Unsupported compiler for VL_THREADED: No thread-local declarator"
# endif
#else
# define VL_THREAD			///< Storage class for thread-local storage
#endif

#ifdef _MSC_VER
# define VL_ULL(c) (c##ui64)	///< Add appropriate suffix to 64-bit constant
#else
# define VL_ULL(c) (c##ULL)	///< Add appropriate suffix to 64-bit constant
#endif

//=========================================================================
// Basic integer types

#ifdef VL_UINTS_DEFINED
#elif defined(__CYGWIN__)
# include <stdint.h>
typedef unsigned char           uint8_t;	///< 8-bit basic type
typedef unsigned short int      uint16_t;	///< 16-bit basic type
typedef unsigned long           uint32_t;	///< 32-bit basic type
typedef unsigned long long      vluint64_t;	///< 64-bit basic type
typedef long  			vlsint32_t;	///< 32-bit signed type
typedef long long      		vlsint64_t;	///< 64-bit signed type
#elif defined(_WIN32)
typedef unsigned char           uint8_t;	///< 8-bit basic type
typedef unsigned short int      uint16_t;	///< 16-bit basic type
typedef unsigned int            uint32_t;	///< 32-bit basic type
typedef unsigned __int64        vluint64_t;	///< 64-bit basic type
typedef int  			vlsint32_t;	///< 32-bit signed type
typedef __int64      		vlsint64_t;	///< 64-bit signed type
#else // Linux or compliant Unix flavors
# include <stdint.h>
typedef unsigned long long      vluint64_t;	///< 64-bit basic type
typedef long  			vlsint32_t;	///< 32-bit signed type
typedef long long      		vlsint64_t;	///< 64-bit signed type
#endif

//=========================================================================
// Integer size macros

#define VL_WORDSIZE 32			///< Bits in a word
#define VL_QUADSIZE 64			///< Bits in a quadword
#define VL_WORDSIZE_LOG2 5		///< log2(VL_WORDSIZE)

/// Words this number of bits needs (1 bit=1 word)
#define VL_WORDS_I(nbits) (((nbits)+(VL_WORDSIZE-1))/VL_WORDSIZE)

//=========================================================================
// Verilated function size macros

#define VL_MULS_MAX_WORDS 16		///< Max size in words of MULS operation
#define VL_TO_STRING_MAX_WORDS 64	///< Max size in words of String conversion operation

//=========================================================================
// Base macros

#define VL_SIZEBITS_I (VL_WORDSIZE-1)	///< Bit mask for bits in a word
#define VL_SIZEBITS_Q (VL_QUADSIZE-1)	///< Bit mask for bits in a quad

/// Mask for words with 1's where relevant bits are (0=all bits)
#define VL_MASK_I(nbits)  (((nbits) & VL_SIZEBITS_I) \
			 ? ((1U << ((nbits) & VL_SIZEBITS_I) )-1) : ~0)
/// Mask for quads with 1's where relevant bits are (0=all bits)
#define VL_MASK_Q(nbits)  (((nbits) & VL_SIZEBITS_Q) \
			 ? ((VL_ULL(1) << ((nbits) & VL_SIZEBITS_Q) )-VL_ULL(1)) : VL_ULL(~0))
#define VL_BITWORD_I(bit)	((bit)/VL_WORDSIZE)	///< Word number for a wide quantity
#define VL_BITBIT_I(bit)	((bit)&VL_SIZEBITS_I)	///< Bit number for a bit in a long
#define VL_BITBIT_Q(bit)	((bit)&VL_SIZEBITS_Q)	///< Bit number for a bit in a quad

//=========================================================================

#endif /*guard*/
