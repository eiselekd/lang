/*
 * Copyright (c) 2016 Andrew Kelley
 *
 * This file is part of zig, which is MIT licensed.
 * See http://opensource.org/licenses/MIT
 */

#include "error.hpp"

const char *err_str(int err) {
    switch ((enum Error)err) {
        case ErrorNone: return "(no error)";
        case ErrorNoMem: return "out of memory";
        case ErrorInvalidFormat: return "invalid format";
        case ErrorSemanticAnalyzeFail: return "semantic analyze failed";
        case ErrorAccess: return "access denied";
        case ErrorInterrupted: return "interrupted";
        case ErrorSystemResources: return "lack of system resources";
        case ErrorFileNotFound: return "file not found";
        case ErrorFileSystem: return "file system error";
        case ErrorFileTooBig: return "file too big";
        case ErrorDivByZero: return "division by zero";
        case ErrorOverflow: return "overflow";
        case ErrorPathAlreadyExists: return "path already exists";
        case ErrorUnexpected: return "unexpected error";
        case ErrorExactDivRemainder: return "exact division had a remainder";
        case ErrorNegativeDenominator: return "negative denominator";
        case ErrorShiftedOutOneBits: return "exact shift shifted out one bits";
        case ErrorCCompileErrors: return "C compile errors";
    }
    return "(invalid error)";
}
